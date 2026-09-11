//! `yield` lowering (jasisz/aver#1329, phase one).
//!
//! A function whose effect list names `yield` never runs as written. Every
//! call to an operation of a capability in its effect list is a stop, and
//! every self tail call is a stop of kind `Yield`: the function hands back
//! what it needs next as a value and a coordinator answers it. The
//! lowering cuts the body at every stop and generates, in the reserved
//! `__` namespace of the same module:
//!
//! - one sum type per request kind, `__F<Kind>State`, with one variant per
//!   stop of that kind holding the variables the rest of that path reads;
//! - `__FRequest`, one constructor per kind carrying the operation's
//!   arguments and the state of that kind, plus `Yield(__FYieldState)`;
//! - `__FOutcome = Done(<result>) | Waiting(__FRequest)`;
//! - `__fStart(<params>) -> __FOutcome`, which runs to the first stop;
//! - `__fAnswer<Kind>(__state, __answer) -> __FOutcome` per kind, which
//!   matches the state variant and runs to the next stop or to `Done`;
//! - `__fAnswerYield(__state)`, which re-enters `__fStart`.
//!
//! The generated items are ordinary types and pure functions of the
//! module: the checker, every backend and both proof exporters see them
//! as if the user had written them. The original function is removed;
//! calling it is a type error with a recipe.
//!
//! The pass runs inside the front door of the pipeline
//! ([`crate::ir::pipeline::front`]) between TCO and the type checker,
//! in two phases: the program is checked once as written so every
//! expression of the `yield` function carries its type stamp, the
//! function is lowered from that stamped copy, and the whole module —
//! generated items and the hand-written coordinator that refers to them —
//! is checked again.

use std::collections::HashSet;

use crate::ast::*;
use crate::codegen::expr_walk;
use crate::types::checker::TypeError;

/// The signature map the first type check produced: parameters, result and
/// declared effects per function name. The lowering reads only the effects,
/// and only to give a generated function the in-place effects its own
/// segment performs (decision 4).
pub(crate) type FnSigs =
    std::collections::HashMap<String, (Vec<crate::ast::Type>, crate::ast::Type, Vec<String>)>;

mod build;
mod coordinator;
mod lower;

#[derive(Debug, Clone, Default)]
pub struct YieldLoweringReport {
    /// Names of the functions that were lowered, in source order.
    pub lowered: Vec<String>,
    /// The generated items, in the order they were spliced into the module.
    pub generated: Vec<TopLevel>,
    /// One entry per lowered function: the protocol the loop generator
    /// dispatches over.
    pub protocols: Vec<ProcessProtocol>,
    /// The generated loop, as source, when the manifest asked for one.
    pub loop_source: Option<String>,
}

/// What the loop generator has to know about one lowered process.
///
/// The lowering already knows all of it — it named the kinds, it wrote the
/// state types and it remembers which operation each kind is a request for —
/// and reconstructing any of that from the generated items would be reading
/// names back out of strings. So it is handed over as data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessProtocol {
    /// The function as the program wrote it, e.g. `peer`.
    pub fn_name: String,
    /// Its parameters, in order. A seated process takes none.
    pub params: Vec<(String, String)>,
    /// What it answers when it is done.
    pub return_type: String,
    /// `__peerStart`.
    pub start: String,
    /// `__PeerRequest`.
    pub request: String,
    /// `__PeerOutcome`.
    pub outcome: String,
    /// One per request kind, in the order the request sum declares them.
    pub kinds: Vec<ProtocolKind>,
}

/// One request kind of one process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProtocolKind {
    /// `Claim`, or `Yield` for the self tail call.
    pub name: String,
    /// The dotted operation this kind is a request for; `None` for `Yield`,
    /// which is answered by the process itself.
    pub operation: Option<String>,
    /// The operation's declared argument types, in order.
    pub arg_types: Vec<String>,
    /// The operation's result type; `None` for `Yield`.
    pub answer_type: Option<String>,
    /// `__PeerClaimState`.
    pub state: String,
    /// `__peerAnswerClaim`.
    pub answer_fn: String,
    /// The state type's variants: one per stop of this kind, with how many
    /// live variables it carries.
    pub variants: Vec<(String, usize)>,
}

impl YieldLoweringReport {
    /// The generated items rendered as Aver source.
    pub fn generated_source(&self) -> String {
        let protocol = crate::ast::unparse::unparse(&self.generated).unwrap_or_default();
        match &self.loop_source {
            Some(loop_source) => format!("{protocol}\n{loop_source}"),
            None => protocol,
        }
    }
}

/// The language's own effect: bare and lowercase, never a capability.
pub const YIELD_EFFECT: &str = "yield";

pub fn is_yield_fn(fd: &FnDef) -> bool {
    fd.effects.iter().any(|e| e.node == YIELD_EFFECT)
}

pub fn has_yield_fns(items: &[TopLevel]) -> bool {
    items
        .iter()
        .any(|item| matches!(item, TopLevel::FnDef(fd) if is_yield_fn(fd)))
}

/// The start function a coordinator calls instead of `fn_name`.
pub fn start_name(fn_name: &str) -> String {
    lower::Names::new(fn_name).start()
}

/// The start function under the spelling the call site used: `loop` from
/// the same module is `__loopStart`, `Looper.loop` from a dependency is
/// `Looper.__loopStart` (the exporter rewrites its `exposes` to match).
pub fn qualified_start_name(callee: &str) -> String {
    match callee.rsplit_once('.') {
        Some((module, name)) => format!("{module}.{}", start_name(name)),
        None => start_name(callee),
    }
}

/// Decision 4: a yielding function is called only through its generated
/// entry points, so a plain call to one — in this module or in a
/// dependency — is an error carrying the recipe, never a missing-effect
/// complaint about `yield`.
pub fn direct_call_recipe(caller: &str, callee: &str) -> String {
    format!(
        "Function '{caller}' calls '{callee}' directly, but '{callee}' yields; call '{}(...)' and answer its requests",
        qualified_start_name(callee)
    )
}

/// The same recipe at a call site that survives the lowering: the name is
/// gone from the module's surface, and the protocol standing in its place
/// is the evidence of why.
pub fn removed_call_recipe(callee: &str) -> String {
    format!(
        "'{callee}' yields; call '{}(...)' and answer its requests",
        qualified_start_name(callee)
    )
}

fn error_at(line: usize, message: String) -> TypeError {
    TypeError {
        message,
        line,
        col: 1,
        origin: None,
        secondary: None,
    }
}

fn item_line(item: &TopLevel) -> Option<usize> {
    match item {
        TopLevel::Module(m) => Some(m.line),
        TopLevel::FnDef(fd) => Some(fd.line),
        TopLevel::Verify(vb) => Some(vb.line),
        TopLevel::Decision(db) => Some(db.line),
        TopLevel::Stmt(Stmt::Binding(_, _, e)) | TopLevel::Stmt(Stmt::Expr(e)) => Some(e.line),
        TopLevel::TypeDef(TypeDef::Sum { line, .. } | TypeDef::Product { line, .. }) => Some(*line),
        TopLevel::Capability(item) => Some(item.line()),
    }
}

/// Lower every `yield` function of `items`.
///
/// `stamped` is the same program after a type check: same items in the
/// same order, with every expression of the `yield` functions carrying its
/// inferred type. `stamped_errors` are that check's diagnostics; the ones
/// inside a `yield` function stop the lowering, the others are left to the
/// second check of the lowered module (they may only concern names the
/// lowering is about to generate).
pub fn lower(
    items: &mut Vec<TopLevel>,
    stamped: &[TopLevel],
    stamped_errors: &[TypeError],
    marked: &crate::config::MarkedCapabilities,
    fn_sigs: &FnSigs,
) -> Result<YieldLoweringReport, Vec<TypeError>> {
    debug_assert_eq!(items.len(), stamped.len());
    let yield_fns: HashSet<String> = stamped
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) if is_yield_fn(fd) => Some(fd.name.clone()),
            _ => None,
        })
        .collect();
    if yield_fns.is_empty() {
        return Ok(YieldLoweringReport::default());
    }

    let mut errors: Vec<TypeError> = Vec::new();
    for (index, item) in stamped.iter().enumerate() {
        let TopLevel::FnDef(fd) = item else { continue };
        if !is_yield_fn(fd) {
            continue;
        }
        let end = stamped[index + 1..]
            .iter()
            .filter_map(item_line)
            .filter(|line| *line > fd.line)
            .min()
            .unwrap_or(usize::MAX);
        errors.extend(
            stamped_errors
                .iter()
                .filter(|e| e.origin.is_none() && e.line >= fd.line && e.line < end)
                .cloned(),
        );
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    for item in stamped {
        match item {
            TopLevel::FnDef(fd) => scan_fn(fd, &yield_fns, &mut errors),
            TopLevel::Verify(vb) => scan_verify(vb, &yield_fns, &mut errors),
            _ => {}
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    let mut report = YieldLoweringReport::default();
    let mut out: Vec<TopLevel> = Vec::with_capacity(items.len());
    let mut exposes_rewrite: Vec<(String, Vec<String>)> = Vec::new();
    for (item, typed) in items.drain(..).zip(stamped) {
        let TopLevel::FnDef(fd) = typed else {
            out.push(item);
            continue;
        };
        if !is_yield_fn(fd) {
            out.push(item);
            continue;
        }
        match lower::lower_fn(fd, marked, fn_sigs) {
            Ok(generated) => {
                report.lowered.push(fd.name.clone());
                report.protocols.push(generated.protocol.clone());
                report.generated.extend(generated.items.iter().cloned());
                exposes_rewrite.push((fd.name.clone(), generated.public_names));
                out.extend(generated.items);
            }
            // The function stays in the module exactly as written. It
            // will not run — the errors below stop this door — but every
            // later diagnostic is then about the user's own code instead
            // of about a function that silently vanished.
            Err(mut fn_errors) => {
                errors.append(&mut fn_errors);
                out.push(item);
            }
        }
    }
    *items = out;
    if !errors.is_empty() {
        return Err(errors);
    }

    // The loop the manifest asked for, generated into the module the `[run]`
    // table names — the entry module, and no other, because that is where the
    // policies and the view are and where a program is entered.
    let module_name = items.iter().find_map(|item| match item {
        TopLevel::Module(module) => Some(module.name.clone()),
        _ => None,
    });
    let plan = marked.run();
    if coordinator::is_run_module(module_name.as_deref(), plan) {
        let plan = plan.expect("checked by is_run_module");
        let generated =
            coordinator::generate(items, &report.generated, &report.protocols, plan, fn_sigs)?;
        report.loop_source = Some(generated.source);
        report.generated.extend(generated.items.iter().cloned());
        items.extend(generated.items);
        // The program declared the effects its processes perform; the turn
        // performs the wait, the stop observation and both ends of every job
        // kind besides, and the module's own boundary has to admit what is
        // generated into it.
        for item in items.iter_mut() {
            let TopLevel::Module(module) = item else {
                continue;
            };
            let Some(declared) = module.effects.as_mut() else {
                continue;
            };
            for effect in &generated.module_effects {
                if !declared.iter().any(|entry| entry == effect) {
                    declared.push(effect.clone());
                }
            }
        }
    }

    // An exposed `yield` function exposes its protocol instead.
    for item in items.iter_mut() {
        let TopLevel::Module(module) = item else {
            continue;
        };
        for (fn_name, public_names) in &exposes_rewrite {
            if let Some(pos) = module.exposes.iter().position(|e| e == fn_name) {
                module.exposes.remove(pos);
                for (offset, name) in public_names.iter().enumerate() {
                    module.exposes.insert(pos + offset, name.clone());
                }
            }
        }
    }
    Ok(report)
}

/// The tail positions of a body: the last statement's expression and,
/// through `match`, every arm's leaf.
fn scan_fn(fd: &FnDef, yield_fns: &HashSet<String>, errors: &mut Vec<TypeError>) {
    let stmts = fd.body.stmts();
    for (index, stmt) in stmts.iter().enumerate() {
        let (expr, tail) = match stmt {
            Stmt::Expr(expr) => (expr, index + 1 == stmts.len()),
            Stmt::Binding(_, _, expr) => (expr, false),
        };
        scan_expr(fd, expr, tail, yield_fns, errors);
    }
}

fn scan_expr(
    fd: &FnDef,
    expr: &Spanned<Expr>,
    tail: bool,
    yield_fns: &HashSet<String>,
    errors: &mut Vec<TypeError>,
) {
    match &expr.node {
        Expr::Match { subject, arms } => {
            scan_expr(fd, subject, false, yield_fns, errors);
            for arm in arms {
                scan_expr(fd, &arm.body, tail, yield_fns, errors);
            }
        }
        Expr::FnCall(callee, args) => {
            if let Expr::Ident(name) = &callee.node
                && yield_fns.contains(name)
            {
                report_call(fd, name, tail, expr.line, errors);
            }
            for arg in args {
                scan_expr(fd, arg, false, yield_fns, errors);
            }
        }
        Expr::TailCall(tc) => {
            if yield_fns.contains(&tc.target) {
                report_call(fd, &tc.target, true, expr.line, errors);
            }
            for arg in &tc.args {
                scan_expr(fd, arg, false, yield_fns, errors);
            }
        }
        _ => expr_walk::for_each_child(expr, &mut |child| {
            scan_expr(fd, child, false, yield_fns, errors)
        }),
    }
}

fn report_call(fd: &FnDef, callee: &str, tail: bool, line: usize, errors: &mut Vec<TypeError>) {
    let message = if !is_yield_fn(fd) {
        direct_call_recipe(&fd.name, callee)
    } else if !tail {
        format!(
            "Function '{}' calls yield function '{callee}' outside tail position; pass what comes next as data, or make it a tail call",
            fd.name
        )
    } else if callee != fd.name {
        format!(
            "Function '{}' tail-calls yield function '{callee}', which has its own request and outcome types; fold '{callee}' into '{}', or have the coordinator drive '{}(...)' after '{}' is Done",
            fd.name,
            fd.name,
            start_name(callee),
            fd.name
        )
    } else {
        return;
    };
    errors.push(error_at(line, message));
}

fn scan_verify(vb: &VerifyBlock, yield_fns: &HashSet<String>, errors: &mut Vec<TypeError>) {
    let mut seen: HashSet<String> = HashSet::new();
    let mut visit = |expr: &Spanned<Expr>| {
        expr_walk::walk(expr, &mut |e| {
            if let Expr::FnCall(callee, _) = &e.node
                && let Expr::Ident(name) = &callee.node
                && yield_fns.contains(name)
                && seen.insert(name.clone())
            {
                errors.push(error_at(
                    e.line,
                    format!(
                        "verify block for '{}' calls '{name}' directly, but '{name}' yields; call '{}(...)' and answer its requests, or verify the generated answer functions",
                        vb.fn_name,
                        start_name(name)
                    ),
                ));
            }
        });
    };
    for (lhs, rhs) in &vb.cases {
        visit(lhs);
        visit(rhs);
    }
}
