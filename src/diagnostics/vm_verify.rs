//! Oracle-aware VM verify runner. Shared between `aver verify` (bin) and
//! `aver audit` (lib `diagnostics::analyze`) so every consumer sees the
//! same case-pass/fail outcomes for a given source.
//!
//! Covers the Oracle v1 extras that plain VM eval doesn't: LHS trace-
//! projection stripping, per-case `given` stub installation into the VM,
//! trace-event collection + projection back into the assertion value,
//! and `apply_trace_projection` for `.trace.{length,event,contains}` /
//! `.trace.group(N).*` shapes.

use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::ast::{
    Expr, FnBody, FnDef, SourceSpan, Spanned, TopLevel, VerifyBlock, VerifyGiven, VerifyKind,
};
use crate::checker::{
    VerifyCaseOutcome, VerifyCaseResult, VerifyLawContext, VerifyResult, expr_to_str,
    merge_verify_blocks,
};
use crate::config::ProjectConfig;
use crate::nan_value::{Arena, NanValueConvert};
use crate::types::checker::effect_classification::{
    EffectDimension, classify, classify_with_registry, is_classified,
};
use crate::types::checker::hostile_effects::hostile_profiles_for;
use crate::value::{Value, aver_repr, list_from_vec};
use crate::verify_law::expand::{ExpandedCase, ExpansionMode, expand_law_cases};
use crate::vm;

/// Mirror of the parser's declared-side cap (`Parser::VERIFY_LAW_MAX_CASES`).
/// Hostile expansion is unbounded *in principle* — value-side boundary
/// sets multiply with the per-method profile cartesian — so a fn with
/// 3 classified effects (5 × 4 × 3 = 60 worlds) and a `given x: Int = 0..100`
/// would balloon to 6_000 cases per declared case. We cap at the same
/// 10_000 the parser uses for declared expansion; over-budget blocks
/// fail with a runtime error pointing at the law's source line so the
/// user can either tighten the `given` domain or drop a `when` guard.
pub const VERIFY_HOSTILE_MAX_CASES: usize = 10_000;

/// Replace `block.cases` with the hostile expansion of its law template,
/// then layer effect-side hostile on top: each case is multiplied by the
/// cartesian product of `(method, profile)` adversarial responses for
/// every classified effect the fn declares. Both expansions run together
/// when `--hostile` is on; cases the user wrote stay as-is (origin=false,
/// profiles=[]), value-hostile cases get origin=true / profiles=[],
/// effect-hostile cases get origin=true / profiles=[(method, profile),...].
///
/// No-op for non-law blocks on the value side (hostile only expands
/// `given` *domains*, which exist in law form), but effect-side hostile
/// applies to both forms since classified effects are independent of
/// `given` shape.
///
/// Returns `Err` when the cartesian would exceed
/// `VERIFY_HOSTILE_MAX_CASES`; mirrors the parser's declared-side cap so
/// `--hostile` can't accidentally produce a multi-hour run from a small
/// source.
type HostileCaseTuple = (
    Spanned<Expr>,
    Spanned<Expr>,
    bool,
    Vec<(String, Spanned<Expr>)>,
    Option<Spanned<Expr>>,
);

#[allow(dead_code)]
pub(crate) fn apply_hostile_expansion(
    block: &mut VerifyBlock,
    items: &[TopLevel],
) -> Result<(), String> {
    apply_hostile_expansion_with_registry(
        block,
        items,
        &crate::capability::CapabilityRegistry::default(),
    )
}

pub(crate) fn apply_hostile_expansion_with_registry(
    block: &mut VerifyBlock,
    items: &[TopLevel],
    capabilities: &crate::capability::CapabilityRegistry,
) -> Result<(), String> {
    let value_expanded: Vec<HostileCaseTuple> = match &block.kind {
        VerifyKind::Law(law) => expand_law_cases(law, ExpansionMode::Hostile)
            .into_iter()
            .map(|c: ExpandedCase| (c.lhs, c.rhs, c.from_hostile, c.bindings, c.guard))
            .collect(),
        VerifyKind::Cases => block
            .cases
            .iter()
            .enumerate()
            .map(|(idx, (lhs, rhs))| {
                (
                    lhs.clone(),
                    rhs.clone(),
                    block
                        .case_hostile_origins
                        .get(idx)
                        .copied()
                        .unwrap_or(false),
                    block.case_givens.get(idx).cloned().unwrap_or_default(),
                    None,
                )
            })
            .collect(),
    };

    let effect_combos =
        collect_effect_profile_combinations_with_registry(block, items, capabilities);
    let fn_uses_independent_product = fn_body_uses_independent_product(block, items);

    // Pre-flight cap check. The post-expansion length is
    // value_expanded × (1 + |effect_combos|) × (order-axis multiplier),
    // where the order-axis adds a reverse-execution twin for every
    // forward case but only for laws whose fn body actually contains
    // an `(a, b)!` independent-product (otherwise the twin would be
    // a pure copy with no observable difference). checked_mul keeps
    // 32-bit-overflowing `given` domains from panicking on wrap.
    let per_value = effect_combos.len().saturating_add(1);
    let order_factor: usize = if fn_uses_independent_product { 2 } else { 1 };
    let projected = value_expanded
        .len()
        .checked_mul(per_value)
        .and_then(|n| n.checked_mul(order_factor));
    if projected
        .map(|n| n > VERIFY_HOSTILE_MAX_CASES)
        .unwrap_or(true)
    {
        return Err(format!(
            "verify '{}' under --hostile expands to more than {} cases ({} declared/boundary cases × {} adversarial worlds × {} eval orders). Tighten the `given` domain, add a `when` precondition, or drop hostile mode for this law.",
            block.fn_name,
            VERIFY_HOSTILE_MAX_CASES,
            value_expanded.len(),
            per_value,
            order_factor,
        ));
    }

    // Hostile-injected cases inherit the law block's source line so
    // diagnostics point to the `verify ... law` declaration instead of
    // 0:0 — IDEs / playground / LSP can highlight the right span and the
    // user understands which law the boundary value broke. Declared cases
    // already had spans pointing at their own source positions, but the
    // post-parse expansion replaces the whole list so we synthesise spans
    // for every case here; the block-line fallback is conservative and
    // good enough until per-case spans for declared values get re-mapped
    // through the new pipeline (separate refactor).
    let block_span = SourceSpan {
        line: block.line,
        col: 1,
        end_line: block.line,
        end_col: 1,
    };

    let mut new_cases: Vec<(Spanned<Expr>, Spanned<Expr>)> = Vec::new();
    let mut new_origins: Vec<bool> = Vec::new();
    let mut new_givens: Vec<Vec<(String, Spanned<Expr>)>> = Vec::new();
    let mut new_spans: Vec<SourceSpan> = Vec::new();
    let mut new_profiles: Vec<Vec<(String, String)>> = Vec::new();
    let mut new_reverse: Vec<bool> = Vec::new();
    let mut new_guards: Vec<Spanned<Expr>> = Vec::new();
    let has_when = matches!(&block.kind, VerifyKind::Law(law) if law.when.is_some());

    // Order-axis: each forward case gets a reverse-execution twin
    // when the fn under test uses `(a, b)!` (CALL_PAR). The twin
    // shares LHS/RHS/given/profile — only `case_reverse_order[idx]`
    // is true. A pure law's branches are independent, so the twin
    // must agree with the forward run; mismatch surfaces as
    // `verify-hostile-order-mismatch`. We skip the twin for fns
    // without CALL_PAR because reverse_eval is a no-op there — pure
    // 2× cost for zero new signal.
    let orders: &[bool] = if fn_uses_independent_product {
        &[false, true]
    } else {
        &[false]
    };

    for (lhs, rhs, from_hostile, bindings, guard) in value_expanded {
        // Always keep the un-effected case (declared or value-hostile),
        // each in forward then reverse order.
        for &reverse in orders {
            new_cases.push((lhs.clone(), rhs.clone()));
            new_origins.push(from_hostile || reverse);
            new_givens.push(bindings.clone());
            new_spans.push(block_span.clone());
            new_profiles.push(Vec::new());
            new_reverse.push(reverse);
            if has_when && let Some(g) = &guard {
                new_guards.push(g.clone());
            }
        }

        // Then add one case per effect-profile cartesian combination,
        // each in forward then reverse order.
        for combo in &effect_combos {
            for &reverse in orders {
                new_cases.push((lhs.clone(), rhs.clone()));
                new_origins.push(true);
                new_givens.push(bindings.clone());
                new_spans.push(block_span.clone());
                new_profiles.push(combo.clone());
                new_reverse.push(reverse);
                if has_when && let Some(g) = &guard {
                    new_guards.push(g.clone());
                }
            }
        }
    }

    block.cases = new_cases;
    block.case_spans = new_spans;
    block.case_givens = new_givens;
    block.case_hostile_origins = new_origins;
    block.case_hostile_profiles = new_profiles;
    block.case_reverse_order = new_reverse;
    if let VerifyKind::Law(law_box) = &mut block.kind {
        law_box.sample_guards = new_guards;
    }
    Ok(())
}

/// Walk the fn's body and every transitively-reachable callee
/// looking for any `Expr::IndependentProduct`, which lowers to the
/// VM `CALL_PAR` opcode. Only laws whose call-graph closure contains
/// one of these have anything to gain from the order-axis hostile
/// twin — for every other shape, reverse-eval is a no-op
/// (sequential evaluation) and the twin would just double cost.
///
/// Transitive coverage matters: a wrapper that calls a helper which
/// itself contains `(a, b)!` is order-sensitive, but the root fn's
/// own body doesn't show that. Pre-fix this gate ran only against
/// `block.fn_name`'s body and silently skipped the reverse-eval
/// twin for those shapes, so hostile mode missed real order bugs in
/// transitively-called helpers.
fn fn_body_uses_independent_product(block: &VerifyBlock, items: &[TopLevel]) -> bool {
    let mut visited: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut stack: Vec<String> = vec![block.fn_name.clone()];
    while let Some(name) = stack.pop() {
        if !visited.insert(name.clone()) {
            continue;
        }
        let Some(fn_def) = items.iter().find_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name == name => Some(fd),
            _ => None,
        }) else {
            continue;
        };
        if fn_body_contains_independent_product(&fn_def.body) {
            return true;
        }
        let mut callees = std::collections::HashSet::new();
        for stmt in fn_def.body.stmts() {
            match stmt {
                crate::ast::Stmt::Expr(e) | crate::ast::Stmt::Binding(_, _, e) => {
                    collect_fn_call_names(&e.node, &mut callees);
                }
            }
        }
        for callee in callees {
            if !visited.contains(&callee) {
                stack.push(callee);
            }
        }
    }
    false
}

fn collect_fn_call_names(expr: &Expr, out: &mut std::collections::HashSet<String>) {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&callee.node) {
                // Skip namespace builtins (Console.print, List.reverse, etc.)
                // — they don't have user-side `(a, b)!` shapes that hostile
                // would reorder. Same heuristic the dafny collect_called_fns
                // helper uses.
                if !name.contains('.') {
                    out.insert(name);
                }
            }
            collect_fn_call_names(&callee.node, out);
            for arg in args {
                collect_fn_call_names(&arg.node, out);
            }
        }
        Expr::TailCall(boxed) => {
            if !boxed.target.contains('.') {
                out.insert(boxed.target.clone());
            }
            for arg in &boxed.args {
                collect_fn_call_names(&arg.node, out);
            }
        }
        Expr::Attr(o, _) | Expr::Neg(o) | Expr::ErrorProp(o) => {
            collect_fn_call_names(&o.node, out);
        }
        Expr::BinOp(_, l, r) => {
            collect_fn_call_names(&l.node, out);
            collect_fn_call_names(&r.node, out);
        }
        Expr::Match { subject, arms } => {
            collect_fn_call_names(&subject.node, out);
            for arm in arms {
                collect_fn_call_names(&arm.body.node, out);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_fn_call_names(&p.node, out);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_fn_call_names(&item.node, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_fn_call_names(&k.node, out);
                collect_fn_call_names(&v.node, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_fn_call_names(&v.node, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_fn_call_names(&base.node, out);
            for (_, v) in updates {
                collect_fn_call_names(&v.node, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    collect_fn_call_names(&inner.node, out);
                }
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

fn fn_body_contains_independent_product(body: &crate::ast::FnBody) -> bool {
    let crate::ast::FnBody::Block(stmts) = body;
    stmts.iter().any(stmt_contains_independent_product)
}

fn stmt_contains_independent_product(stmt: &crate::ast::Stmt) -> bool {
    match stmt {
        crate::ast::Stmt::Expr(e) | crate::ast::Stmt::Binding(_, _, e) => {
            expr_contains_independent_product(&e.node)
        }
    }
}

fn expr_contains_independent_product(expr: &Expr) -> bool {
    match expr {
        Expr::IndependentProduct(_, _) => true,
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => false,
        Expr::Attr(obj, _) | Expr::Neg(obj) | Expr::ErrorProp(obj) => {
            expr_contains_independent_product(&obj.node)
        }
        Expr::BinOp(_, l, r) => {
            expr_contains_independent_product(&l.node) || expr_contains_independent_product(&r.node)
        }
        Expr::FnCall(callee, args) => {
            expr_contains_independent_product(&callee.node)
                || args
                    .iter()
                    .any(|a| expr_contains_independent_product(&a.node))
        }
        Expr::TailCall(boxed) => boxed
            .args
            .iter()
            .any(|a| expr_contains_independent_product(&a.node)),
        Expr::Match { subject, arms } => {
            expr_contains_independent_product(&subject.node)
                || arms
                    .iter()
                    .any(|arm| expr_contains_independent_product(&arm.body.node))
        }
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .is_some_and(|p| expr_contains_independent_product(&p.node)),
        Expr::List(items) | Expr::Tuple(items) => items
            .iter()
            .any(|e| expr_contains_independent_product(&e.node)),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_contains_independent_product(&k.node) || expr_contains_independent_product(&v.node)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, v)| expr_contains_independent_product(&v.node)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_contains_independent_product(&base.node)
                || updates
                    .iter()
                    .any(|(_, v)| expr_contains_independent_product(&v.node))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            crate::ast::StrPart::Parsed(inner) => expr_contains_independent_product(&inner.node),
            _ => false,
        }),
    }
}

/// For a verify block, find the function under test and return the
/// cartesian product of `(method, profile)` pairs across every classified
/// non-`Output` effect the fn declares. Each inner Vec is one adversarial
/// world.
///
/// **Effect-side hostile applies to `verify <fn> law <name>` blocks**
/// (with or without `trace`) — universal claims with runtime oracle
/// stubs. The user's `given <Effect> = [stub]` pins one specific world
/// inside what is otherwise a universal claim; hostile profiles ask
/// "does this law hold under every adversarial world, not just the one
/// stub you picked?".
///
/// Cases-form opts out — both plain `verify <fn>` and `verify <fn>
/// trace` (no `law`) are *fixtures*: the user wrote one specific
/// scenario with one chosen stub. Multiplying a fixture by the profile
/// cartesian would change "this scenario" into "every scenario", which
/// is not what the user wrote.
#[allow(dead_code)]
fn collect_effect_profile_combinations(
    block: &VerifyBlock,
    items: &[TopLevel],
) -> Vec<Vec<(String, String)>> {
    collect_effect_profile_combinations_with_registry(
        block,
        items,
        &crate::capability::CapabilityRegistry::default(),
    )
}

fn collect_effect_profile_combinations_with_registry(
    block: &VerifyBlock,
    items: &[TopLevel],
    capabilities: &crate::capability::CapabilityRegistry,
) -> Vec<Vec<(String, String)>> {
    let is_law = matches!(block.kind, VerifyKind::Law(_));
    if !is_law {
        return Vec::new();
    }
    let fn_def = items.iter().find_map(|i| match i {
        TopLevel::FnDef(fd) if fd.name == block.fn_name => Some(fd),
        _ => None,
    });
    let Some(fn_def) = fn_def else {
        return Vec::new();
    };

    let mut per_method: Vec<(String, Vec<String>)> = Vec::new();
    for eff in &fn_def.effects {
        let method = eff.node.as_str();
        let Some(c) = classify_with_registry(capabilities, method) else {
            continue;
        };
        if matches!(c.dimension, EffectDimension::Output) {
            continue;
        }
        let profiles: Vec<String> = if let Some(operation) = capabilities.operation(method) {
            operation
                .hostile
                .iter()
                .map(|profile| format!("{}.{}", operation.module, profile))
                .collect()
        } else {
            hostile_profiles_for(method)
                .into_iter()
                .map(|profile| profile.name.to_string())
                .collect()
        };
        if profiles.is_empty() {
            continue;
        }
        per_method.push((method.to_string(), profiles));
    }

    if per_method.is_empty() {
        return Vec::new();
    }

    let mut out: Vec<Vec<(String, String)>> = vec![Vec::new()];
    for (method, profiles) in &per_method {
        let mut next: Vec<Vec<(String, String)>> =
            Vec::with_capacity(out.len() * profiles.len().max(1));
        for partial in &out {
            for profile in profiles {
                let mut extended = partial.clone();
                extended.push((method.clone(), profile.clone()));
                next.push(extended);
            }
        }
        out = next;
    }
    out
}

/// For each `verify <fn> law <name>` block (with or without `trace`),
/// find the function under test and the classified non-Output effects
/// it declares, then parse + inject the corresponding hostile profile
/// bodies as `TopLevel::FnDef`. Effect-side hostile only applies to
/// law form — see `collect_effect_profile_combinations` for the
/// rationale. Runs before type-check so synthetic stubs go through the
/// regular checker and compile to ordinary user-space fns the runner
/// can install as oracle stubs.
pub(crate) fn inject_hostile_effect_stubs_for_blocks(
    items: &mut Vec<TopLevel>,
    blocks: &[VerifyBlock],
) {
    let existing: std::collections::HashSet<String> = items
        .iter()
        .filter_map(|i| match i {
            TopLevel::FnDef(fd) => Some(fd.name.clone()),
            _ => None,
        })
        .collect();

    // Owned method names, deduplicated in declaration order. Owning the
    // strings (instead of borrowing from `fn_def.effects`) is what lets
    // the injection loop below take the mutable borrow of `items` — and
    // it keys `hostile_profiles_for` on the classified method name
    // verbatim, so a newly classified effect can never be silently
    // dropped by a stale name mapping (the previous hand-maintained
    // `&'static str` table mapped unknown methods to `""`, which made
    // the Tcp byte methods' profiles vanish from this path).
    let mut to_inject: Vec<String> = Vec::new();
    let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

    for block in blocks {
        if !matches!(block.kind, VerifyKind::Law(_)) {
            continue;
        }
        let Some(fn_def) = items.iter().find_map(|i| match i {
            TopLevel::FnDef(fd) if fd.name == block.fn_name => Some(fd),
            _ => None,
        }) else {
            continue;
        };
        for eff in &fn_def.effects {
            let method = eff.node.as_str();
            let Some(c) = classify(method) else { continue };
            if matches!(c.dimension, EffectDimension::Output) {
                continue;
            }
            if !seen.insert(method.to_string()) {
                continue;
            }
            to_inject.push(method.to_string());
        }
    }

    let mut new_items: Vec<TopLevel> = Vec::new();
    for method in &to_inject {
        for profile in hostile_profiles_for(method) {
            if existing.contains(&profile.stub_fn_name) {
                continue;
            }
            match parse_stub_body(&profile.stub_body) {
                Ok(stub_items) => new_items.extend(stub_items),
                Err(e) => {
                    eprintln!(
                        "warning: hostile profile {} failed to parse: {}",
                        profile.stub_fn_name, e
                    );
                }
            }
        }
    }
    items.extend(new_items);
}

fn parse_stub_body(source: &str) -> Result<Vec<TopLevel>, String> {
    let mut lexer = crate::lexer::Lexer::new(source);
    let tokens = lexer.tokenize().map_err(|e| e.to_string())?;
    let mut parser = crate::parser::Parser::new(tokens);
    parser.parse().map_err(|e| e.to_string())
}

/// Render a per-case hostile-effect-profile assignment into a short human
/// label. `[("Time.now", "frozen"), ("Random.int", "min")]` →
/// `Some("Time.now/frozen + Random.int/min")`. `None` for empty / missing.
fn render_hostile_profile_label(combo: Option<&[(String, String)]>) -> Option<String> {
    let combo = combo?;
    if combo.is_empty() {
        return None;
    }
    Some(
        combo
            .iter()
            .map(|(method, profile)| format!("{}/{}", method, profile))
            .collect::<Vec<_>>()
            .join(" + "),
    )
}

/// Run the complete verify pipeline for a set of items: typecheck,
/// synthesize per-case helpers, compile + run in VM with Oracle stub
/// installation and trace collection wired through.
///
/// `config` is an optional pre-loaded aver.toml (CLI loads it; audit /
/// LSP can pass `None` or load their own). `base_dir` is the module
/// root for cross-module resolution. `source_file` labels the VM's
/// compilation unit for diagnostics.
pub fn run_verify_for_items_vm(
    items: Vec<TopLevel>,
    config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    source_file: &str,
) -> Result<Vec<VerifyResult>, String> {
    run_verify_for_items_vm_with_mode(
        items,
        config,
        base_dir,
        source_file,
        ExpansionMode::Declared,
    )
}

/// Same as [`run_verify_for_items_vm`] but lets the caller request hostile
/// boundary-value expansion of `verify ... law` cases. Under
/// `ExpansionMode::Hostile`, each typed `given` clause is augmented with
/// the per-type boundary set (`Int` min/max/0/±1, `Str` empty/long/edge,
/// ...) and the `when` precondition is preserved across the expansion.
/// Cases injected by the hostile pass carry `from_hostile = true` in
/// `VerifyBlock::case_hostile_origins` so the renderer can label them as
/// "outside declared given" when they fail.
pub fn run_verify_for_items_vm_with_mode(
    mut items: Vec<TopLevel>,
    config: Option<ProjectConfig>,
    base_dir: Option<&str>,
    source_file: &str,
    mode: ExpansionMode,
) -> Result<Vec<VerifyResult>, String> {
    crate::ir::pipeline::tco(&mut items);

    // Everything appended below this line is compiler-fabricated, so
    // this is where the program the user wrote ends — the scope the
    // shadowing ban in `pipeline::typecheck_gate` is entitled to read.
    let user_program_len = items.len();

    if mode == ExpansionMode::Hostile {
        // Inject hostile effect-profile stubs as `TopLevel::FnDef` before
        // type-check so they're visible to the verify pipeline as ordinary
        // user-defined functions. We can't compute case-level
        // `case_hostile_profiles` until after type-check (because the
        // value-side hostile expansion must run on a checked AST), so we
        // pre-scan: find every classified non-Output effect each verify
        // block's fn declares but the user did not pin via `given`, and
        // inject the corresponding hostile profile bodies. The cases are
        // wired up to those stubs in `apply_hostile_expansion` after
        // type-check.
        let preview_blocks = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &preview_blocks);
    }

    // Load dep modules so `SymbolTable::build` below sees cross-module
    // fns. Without this the resolver classifies `Module.fn(...)`
    // callsites as `Passthrough`, the VM compiler later errors with
    // "missing VM symbol for exposed function", and any multi-module
    // verify fails — exactly the bug #213 surfaced on the 0.22 release.
    // Mirrors what `aver run` / `aver compile` / `bench/runner` /
    // `wasm_gc_verify` all do.
    let dep_modules = if let Some(root) = base_dir {
        crate::source::load_compile_deps(&items, root)?
    } else {
        Vec::new()
    };

    // `pipeline::typecheck_gate`, not the bare `pipeline::typecheck`:
    // verify cannot use `pipeline::run` (it has to merge verify blocks,
    // expand hostile cases and build its VM plans BETWEEN typecheck and
    // resolve, and the orchestrator has no seam there), but the gate it
    // owes its users is the same one every other front door passes
    // through — type errors AND the shadowing ban (#954). Without this
    // the #951 program, whose three executors disagreed about a call
    // through a binder spelling a module fn's name, was still EXECUTED
    // by `aver verify` while `run` and `compile` refused it.
    let tc_result = crate::ir::pipeline::typecheck_gate(
        &items,
        &crate::ir::TypecheckMode::Full { base_dir },
        &items[..user_program_len],
    );
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    let mut verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Ok(vec![]);
    }

    if mode == ExpansionMode::Hostile {
        for block in &mut verify_blocks {
            apply_hostile_expansion_with_registry(block, &items, &tc_result.capabilities)?;
        }
    }

    let plans = build_verify_vm_plans(&mut items, &verify_blocks);
    crate::ir::pipeline::resolve(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    // verify path runs `pipeline::tco` / the typecheck gate / `resolve`
    // directly without the canonical `pipeline::run` orchestrator (it
    // has to interleave verify-block work between them), so no
    // AnalysisResult is available
    // here — the VM compiler falls through to the conservative
    // "assume allocates" branch (no per-fn `no_alloc` promotion).
    // Build the resolved-identity table + lift the items to resolved
    // HIR ourselves so the VM compiler stays on the Phase E contract.
    let symbol_table = crate::ir::SymbolTable::build(&items, &dep_modules);
    let resolved_items = crate::ir::hir::resolve_program(&symbol_table, &items);
    let (code, globals) = vm::compile_program_with_modules(
        &resolved_items,
        &symbol_table,
        &mut arena,
        base_dir,
        source_file,
        None,
    )
    .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_step_limit(Some(VERIFY_VM_STEP_LIMIT));
    if let Some(cfg) = config {
        machine.set_runtime_policy(cfg);
    }

    let mut results = Vec::new();
    for plan in &plans {
        results.push(run_verify_vm(plan, &mut machine, &tc_result.capabilities));
    }
    Ok(results)
}

/// Per-`run_named_function` opcode cap installed before evaluating
/// verify cases. AFL nightly found tail-recursive shapes
/// (`fn id(x) -> Int = id(-7)` and friends — byte-havoc dropping the
/// terminating arm) where Aver's TCO turns infinite recursion into a
/// goto-loop with no stack growth, so without this cap the VM
/// dispatch loop never terminates. 1M opcodes is 100× above what
/// real verify cases need (per-case eval is normally <10k steps on
/// the bench suite) and short enough that an unconverging case bails
/// in ~5-50ms even on a slow shared CI runner — well under AFL's
/// default `AFL_HANG_TMOUT=1000ms`, so fuzz-discovered infinite loops
/// surface as proper `VmError::StepLimit` instead of AFL hangs. The
/// cap resets per `run_named_function`, so cases don't share budget.
const VERIFY_VM_STEP_LIMIT: u64 = 1_000_000;

/// Variant for audit / LSP: accept pre-loaded dependency modules
/// (virtual filesystems) instead of resolving from disk.
pub fn run_verify_for_items_vm_with_loaded(
    items: Vec<TopLevel>,
    loaded: Vec<crate::source::LoadedModule>,
    config: Option<ProjectConfig>,
    source_file: &str,
) -> Result<Vec<VerifyResult>, String> {
    run_verify_for_items_vm_with_loaded_and_mode(
        items,
        loaded,
        config,
        source_file,
        ExpansionMode::Declared,
    )
}

pub fn run_verify_for_items_vm_with_loaded_and_mode(
    mut items: Vec<TopLevel>,
    loaded: Vec<crate::source::LoadedModule>,
    config: Option<ProjectConfig>,
    source_file: &str,
    mode: ExpansionMode,
) -> Result<Vec<VerifyResult>, String> {
    crate::ir::pipeline::tco(&mut items);

    // End of the program the user wrote — see the disk-loader path.
    let user_program_len = items.len();

    if mode == ExpansionMode::Hostile {
        let preview_blocks = merge_verify_blocks(&items);
        inject_hostile_effect_stubs_for_blocks(&mut items, &preview_blocks);
    }

    // Same gate as the disk-loader path above, so the playground and
    // the LSP refuse a shadowing program exactly as the CLI does.
    let tc_result = crate::ir::pipeline::typecheck_gate(
        &items,
        &crate::ir::TypecheckMode::WithLoaded(&loaded),
        &items[..user_program_len],
    );
    if !tc_result.errors.is_empty() {
        return Err(format_type_errors(&tc_result.errors));
    }

    let mut verify_blocks = merge_verify_blocks(&items);
    if verify_blocks.is_empty() {
        return Ok(vec![]);
    }

    if mode == ExpansionMode::Hostile {
        for block in &mut verify_blocks {
            apply_hostile_expansion_with_registry(block, &items, &tc_result.capabilities)?;
        }
    }

    let plans = build_verify_vm_plans(&mut items, &verify_blocks);
    crate::ir::pipeline::resolve(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    // Mirror of the disk-loader path's dep_modules wiring (#213 fix):
    // turn the pre-loaded virtual-fs modules into `ModuleInfo` records
    // so `SymbolTable::build` sees every cross-module fn. Without this
    // the playground's multi-file programs would hit the same
    // "missing VM symbol for exposed function" path that the disk path
    // hit before the fix.
    let dep_modules = crate::source::loaded_to_module_info(&loaded);
    let symbol_table = crate::ir::SymbolTable::build(&items, &dep_modules);
    let resolved_items = crate::ir::hir::resolve_program(&symbol_table, &items);
    let (code, globals) = vm::compile_program_with_loaded_modules(
        &resolved_items,
        &symbol_table,
        &mut arena,
        loaded,
        source_file,
        None,
    )
    .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.set_step_limit(Some(VERIFY_VM_STEP_LIMIT));
    if let Some(cfg) = config {
        machine.set_runtime_policy(cfg);
    }

    let mut results = Vec::new();
    for plan in &plans {
        results.push(run_verify_vm(plan, &mut machine, &tc_result.capabilities));
    }
    Ok(results)
}

pub(crate) fn format_type_errors(errors: &[crate::types::checker::TypeError]) -> String {
    let mut out = Vec::new();
    for te in errors {
        out.push(format!("error[{}:{}]: {}", te.line, te.col, te.message));
    }
    out.join("\n")
}

// ─── Plan synthesis ───────────────────────────────────────────────────────────

struct VmVerifyCaseFns {
    left: String,
    right: String,
    guard: Option<String>,
}

struct VmVerifyPlan {
    block: VerifyBlock,
    cases: Vec<VmVerifyCaseFns>,
}

enum VmVerifyEval {
    Value(Value),
    ErrProp(Value),
}

/// Per-case guard regeneration for hostile-profile cases.
///
/// Returns `Some(rewritten_guard)` when the case is in a `law` block,
/// has a `when` clause, AND has at least one effect-given name that
/// the per-case profile assignment overrides. Returns `None` otherwise
/// (caller falls back to parser's pre-substituted `sample_guards`).
///
/// The rewrite swaps each effect-given binding's name (e.g. `clock`)
/// from the user's stub fn name (`realStub` after the parser's
/// substitution pass) to the synthetic hostile profile fn name
/// (`__hostile_Time_unixMs_frozen_zero`). After the rewrite, the
/// guard helper invokes the profile stub directly — install /
/// uninstall machinery is bypassed for guard purposes, but the case
/// body itself still sees the profile via the standard
/// `install_oracle_stubs` path.
pub(crate) fn guard_for_case(block: &VerifyBlock, case_idx: usize) -> Option<Spanned<Expr>> {
    use crate::ast_rewrite::rewrite_idents_scoped;
    use std::collections::HashMap;

    let VerifyKind::Law(law) = &block.kind else {
        return None;
    };
    let when = law.when.as_ref()?;
    let combo = block.case_hostile_profiles.get(case_idx)?;
    if combo.is_empty() {
        return None;
    }
    let case_bindings = block.case_givens.get(case_idx)?;

    let mut bindings: HashMap<String, Spanned<Expr>> = HashMap::new();
    // Start from the case's actual given bindings — value-given names
    // already resolve here, and effect-given names point at the user's
    // stub fn name (via the parser's per-case substitution).
    for (name, value) in case_bindings {
        bindings.insert(name.clone(), value.clone());
    }
    // Override every effect-given that has a profile assigned for this
    // case. The hostile profile fn (`__hostile_<method>_<profile>`) was
    // injected as a TopLevel::FnDef in `inject_hostile_effect_stubs_for_blocks`,
    // so by name it resolves to a real fn at VM compile time.
    for given in &law.givens {
        if let Some((_, profile)) = combo.iter().find(|(m, _)| m == &given.type_name) {
            let stub_fn_name = hostile_stub_name(&given.type_name, profile);
            bindings.insert(given.name.clone(), Spanned::bare(Expr::Ident(stub_fn_name)));
        }
    }
    Some(rewrite_idents_scoped(when, |name| {
        bindings.get(name).cloned()
    }))
}

pub(crate) fn make_verify_helper(
    name: String,
    line: usize,
    expr: Spanned<Expr>,
    wrap_result: bool,
) -> TopLevel {
    let body_expr = if wrap_result {
        Spanned::new(
            Expr::Constructor("Result.Ok".to_string(), Some(Box::new(expr))),
            line,
        )
    } else {
        expr
    };

    TopLevel::FnDef(FnDef {
        name,
        line,
        params: vec![],
        return_type: "Unit".to_string(),
        effects: vec![],
        desc: None,
        body: Rc::new(FnBody::from_expr(body_expr)),
        resolution: None,
    })
}

fn build_verify_vm_plans(
    items: &mut Vec<TopLevel>,
    verify_blocks: &[VerifyBlock],
) -> Vec<VmVerifyPlan> {
    let mut plans = Vec::with_capacity(verify_blocks.len());

    for (block_idx, block) in verify_blocks.iter().enumerate() {
        let mut case_plans = Vec::with_capacity(block.cases.len());
        let sample_guards = match &block.kind {
            VerifyKind::Law(law) => Some(&law.sample_guards),
            VerifyKind::Cases => None,
        };

        for (case_idx, (left_expr, right_expr)) in block.cases.iter().cloned().enumerate() {
            let prefix = format!("__verify_{}_{}_{}", block.fn_name, block_idx, case_idx);
            let left_name = format!("{}_left", prefix);
            let right_name = format!("{}_right", prefix);
            let left_expr = if block.trace {
                strip_trace_projection(left_expr)
            } else {
                left_expr
            };
            items.push(make_verify_helper(
                left_name.clone(),
                block.line,
                left_expr,
                true,
            ));
            items.push(make_verify_helper(
                right_name.clone(),
                block.line,
                right_expr,
                true,
            ));

            // Guard generation. For hostile-profile cases, regenerate
            // the guard from `law.when` with effect-given names rebound
            // to the synthetic `__hostile_<method>_<profile>` fn names
            // so the predicate fires under the same world the case
            // body sees. For declared / value-only-hostile cases, use
            // the parser's pre-substituted `sample_guards` as before.
            let guard_expr = guard_for_case(block, case_idx)
                .or_else(|| sample_guards.and_then(|gs| gs.get(case_idx)).cloned());
            let guard_name = guard_expr.map(|guard_expr| {
                let name = format!("{}_guard", prefix);
                items.push(make_verify_helper(
                    name.clone(),
                    block.line,
                    guard_expr,
                    false,
                ));
                name
            });

            case_plans.push(VmVerifyCaseFns {
                left: left_name,
                right: right_name,
                guard: guard_name,
            });
        }

        plans.push(VmVerifyPlan {
            block: block.clone(),
            cases: case_plans,
        });
    }

    plans
}

// ─── Trace projection ─────────────────────────────────────────────────────────

fn strip_trace_projection(expr: Spanned<Expr>) -> Spanned<Expr> {
    if let Some(fc) = find_trace_underlying_fn_call(&expr.node) {
        return fc;
    }
    expr
}

fn find_trace_underlying_fn_call(node: &Expr) -> Option<Spanned<Expr>> {
    match node {
        Expr::Attr(inner, field) if field == "result" => {
            if matches!(&inner.node, Expr::FnCall(_, _)) {
                Some((**inner).clone())
            } else {
                None
            }
        }
        Expr::Attr(inner, field) if field == "trace" => {
            if matches!(&inner.node, Expr::FnCall(_, _)) {
                Some((**inner).clone())
            } else {
                None
            }
        }
        Expr::FnCall(callee, _) => {
            let Expr::Attr(inner_attr, _) = &callee.node else {
                return None;
            };
            let (scope, _, _) = peel_tree_nav_stages(inner_attr);
            let Expr::Attr(fn_call_box, trace_field) = &scope.node else {
                return None;
            };
            if trace_field == "trace" && matches!(&fn_call_box.node, Expr::FnCall(_, _)) {
                return Some((**fn_call_box).clone());
            }
            None
        }
        _ => None,
    }
}

fn apply_trace_projection(
    helper_result: Result<VmVerifyEval, String>,
    original_lhs: &Spanned<Expr>,
    collected: &[Value],
    coords: &[crate::vm::runtime::TraceCoord],
    trace_mode: bool,
) -> Result<VmVerifyEval, String> {
    if !trace_mode {
        return helper_result;
    }
    let helper_result = helper_result?;
    if matches!(&original_lhs.node, Expr::Attr(_, f) if f == "result") {
        return Ok(helper_result);
    }
    match &original_lhs.node {
        Expr::Attr(inner, field)
            if field == "trace" && matches!(&inner.node, Expr::FnCall(_, _)) =>
        {
            Ok(VmVerifyEval::Value(build_trace_record(collected)))
        }
        Expr::FnCall(callee, args) => {
            let Expr::Attr(inner_attr, method) = &callee.node else {
                return Ok(helper_result);
            };
            let (scope, group_id, branch_idx) = peel_tree_nav_stages(inner_attr);
            let Expr::Attr(fn_call_box, trace_field) = &scope.node else {
                return Ok(helper_result);
            };
            if trace_field != "trace" || !matches!(&fn_call_box.node, Expr::FnCall(_, _)) {
                return Ok(helper_result);
            }
            let filtered: Vec<&Value> = collected
                .iter()
                .zip(coords.iter())
                .filter(|(_, c)| match group_id {
                    Some(n) => c.group_id == Some(n + 1),
                    None => true,
                })
                .filter(|(_, c)| match branch_idx {
                    Some(b) => c.branch_idx == Some(b),
                    None => true,
                })
                .map(|(ev, _)| ev)
                .collect();
            match method.as_str() {
                "length" => Ok(VmVerifyEval::Value(Value::int(filtered.len() as i64))),
                "event" => {
                    if args.len() != 1 {
                        return Err(format!("Trace.event(k) expects 1 arg, got {}", args.len()));
                    }
                    let k = match &args[0].node {
                        Expr::Literal(crate::ast::Literal::Int(i)) if *i >= 0 => *i as usize,
                        _ => {
                            return Err(
                                "Trace.event(k) requires a literal non-negative Int for v0"
                                    .to_string(),
                            );
                        }
                    };
                    let v = match filtered.get(k) {
                        Some(ev) => Value::Some(Box::new((**ev).clone())),
                        None => Value::None,
                    };
                    Ok(VmVerifyEval::Value(v))
                }
                "contains" => {
                    if args.len() != 1 {
                        return Err(format!(
                            "Trace.contains(x) expects 1 arg, got {}",
                            args.len()
                        ));
                    }
                    let arg = &args[0];
                    let method_only_name = effect_method_ref_name(arg);
                    let hit = if let Some(method_name) = method_only_name {
                        filtered
                            .iter()
                            .any(|ev| effect_event_has_method(ev, &method_name))
                    } else {
                        let expected = match expr_to_effect_event(arg) {
                            Some(ev) => ev,
                            None => {
                                return Err(
                                    "Trace.contains(x): argument must be either an effect-method \
                                     reference (e.g. `Console.print`) or an effect-method call \
                                     (e.g. `Console.print(\"...\")`) that elaborates to an event literal"
                                        .to_string(),
                                );
                            }
                        };
                        filtered
                            .iter()
                            .any(|ev| effect_event_method_args_eq(ev, &expected))
                    };
                    Ok(VmVerifyEval::Value(Value::Bool(hit)))
                }
                "count" => {
                    // 0.13 Limit nail #3: count laws. `.trace.count(M)` returns
                    // the number of trace events whose method matches `M` (an
                    // effect-method reference like `Time.now` or a call literal
                    // `Console.print("hi")`). Lets users write quantitative
                    // assertions — "this fn calls the API exactly once" —
                    // instead of just "contains" / "length over the whole
                    // trace". The hostile-effect profiles and trace count
                    // laws compose: `count(Time.now) == n` checks how many
                    // times the law's fn reaches for the clock under each
                    // adversarial profile.
                    if args.len() != 1 {
                        return Err(format!("Trace.count(x) expects 1 arg, got {}", args.len()));
                    }
                    let arg = &args[0];
                    let method_only_name = effect_method_ref_name(arg);
                    let n = if let Some(method_name) = method_only_name {
                        filtered
                            .iter()
                            .filter(|ev| effect_event_has_method(ev, &method_name))
                            .count()
                    } else {
                        let expected = match expr_to_effect_event(arg) {
                            Some(ev) => ev,
                            None => {
                                return Err(
                                    "Trace.count(x): argument must be either an effect-method \
                                     reference (e.g. `Console.print`) or an effect-method call \
                                     (e.g. `Console.print(\"...\")`) that elaborates to an event literal"
                                        .to_string(),
                                );
                            }
                        };
                        filtered
                            .iter()
                            .filter(|ev| effect_event_method_args_eq(ev, &expected))
                            .count()
                    };
                    Ok(VmVerifyEval::Value(Value::int(n as i64)))
                }
                _ => Ok(helper_result),
            }
        }
        _ => Ok(helper_result),
    }
}

fn peel_named_int_stage<'a>(
    expr: &'a Spanned<Expr>,
    expected: &str,
) -> Option<(&'a Spanned<Expr>, u32)> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let Expr::Attr(base, method) = &callee.node else {
        return None;
    };
    if method != expected || args.len() != 1 {
        return None;
    }
    match &args[0].node {
        Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 0 => Some((base.as_ref(), *n as u32)),
        _ => None,
    }
}

fn peel_tree_nav_stages(expr: &Spanned<Expr>) -> (&Spanned<Expr>, Option<u32>, Option<u32>) {
    if let Some((after_branch, idx)) = peel_named_int_stage(expr, "branch")
        && let Some((after_group, gid)) = peel_named_int_stage(after_branch, "group")
    {
        return (after_group, Some(gid), Some(idx));
    }
    if let Some((after_group, gid)) = peel_named_int_stage(expr, "group") {
        return (after_group, Some(gid), None);
    }
    (expr, None, None)
}

fn is_trace_projection_shape(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Attr(inner, field) if field == "trace" => {
            matches!(&inner.node, Expr::FnCall(_, _))
        }
        Expr::FnCall(callee, _) => {
            let Expr::Attr(inner_attr, method) = &callee.node else {
                return false;
            };
            if !matches!(method.as_str(), "length" | "event" | "contains" | "count") {
                return false;
            }
            let (scope, _, _) = peel_tree_nav_stages(inner_attr);
            let Expr::Attr(fn_call_box, trace_field) = &scope.node else {
                return false;
            };
            trace_field == "trace" && matches!(&fn_call_box.node, Expr::FnCall(_, _))
        }
        _ => false,
    }
}

fn format_trace_events_tail(events: &[Value]) -> String {
    if events.is_empty() {
        return "  (trace: [] — no effects recorded)".to_string();
    }
    let rendered: Vec<String> = events.iter().map(aver_repr).collect();
    format!("  (trace: [{}])", rendered.join(", "))
}

fn build_trace_record(collected: &[Value]) -> Value {
    Value::Record {
        type_name: crate::types::trace::TYPE_NAME.to_string(),
        fields: vec![(
            crate::types::trace::FIELD_EVENTS.to_string(),
            list_from_vec(collected.to_vec()),
        )]
        .into(),
    }
}

fn effect_event_method_args_eq(recorded: &Value, expected: &Value) -> bool {
    let Value::Record {
        type_name: rt,
        fields: rf,
    } = recorded
    else {
        return false;
    };
    let Value::Record {
        type_name: et,
        fields: ef,
    } = expected
    else {
        return false;
    };
    if rt != et {
        return false;
    }
    let pick = |fields: &[(String, Value)], name: &str| -> Option<Value> {
        fields
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, v)| v.clone())
    };
    let r_method = pick(rf, crate::types::effect_event::FIELD_METHOD);
    let e_method = pick(ef, crate::types::effect_event::FIELD_METHOD);
    let r_args = pick(rf, crate::types::effect_event::FIELD_ARGS);
    let e_args = pick(ef, crate::types::effect_event::FIELD_ARGS);
    r_method == e_method && r_args == e_args
}

fn effect_method_ref_name(expr: &Spanned<Expr>) -> Option<String> {
    let (ns, method) = match &expr.node {
        Expr::Attr(ns, method) => {
            let Expr::Ident(ns_name) = &ns.node else {
                return None;
            };
            (ns_name.clone(), method.clone())
        }
        Expr::Resolved { name, .. } => {
            let (ns, method) = name.rsplit_once('.')?;
            (ns.to_string(), method.to_string())
        }
        _ => return None,
    };
    let full = format!("{}.{}", ns, method);
    if is_classified(&full) {
        Some(full)
    } else {
        None
    }
}

fn effect_event_has_method(recorded: &Value, expected_name: &str) -> bool {
    let Value::Record { type_name, fields } = recorded else {
        return false;
    };
    if type_name != crate::types::effect_event::TYPE_NAME {
        return false;
    }
    fields.iter().any(|(name, val)| {
        name == crate::types::effect_event::FIELD_METHOD
            && matches!(val, Value::Str(s) if s == expected_name)
    })
}

fn expr_to_effect_event(expr: &Spanned<Expr>) -> Option<Value> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let Expr::Attr(ns, method) = &callee.node else {
        return None;
    };
    let Expr::Ident(ns_name) = &ns.node else {
        return None;
    };
    let full_name = format!("{}.{}", ns_name, method);
    let arg_vals: Vec<Value> = args.iter().filter_map(literal_expr_to_value).collect();
    if arg_vals.len() != args.len() {
        return None;
    }
    Some(Value::Record {
        type_name: crate::types::effect_event::TYPE_NAME.to_string(),
        fields: vec![
            (
                crate::types::effect_event::FIELD_METHOD.to_string(),
                Value::Str(full_name),
            ),
            (
                crate::types::effect_event::FIELD_ARGS.to_string(),
                list_from_vec(arg_vals),
            ),
        ]
        .into(),
    })
}

fn literal_expr_to_value(expr: &Spanned<Expr>) -> Option<Value> {
    match &expr.node {
        Expr::Literal(crate::ast::Literal::Int(i)) => Some(Value::int(*i)),
        Expr::Literal(crate::ast::Literal::BigInt(s)) => {
            Some(Value::Int(s.parse::<aver_rt::AverInt>().ok()?))
        }
        Expr::Literal(crate::ast::Literal::Float(f)) => Some(Value::Float(*f)),
        Expr::Literal(crate::ast::Literal::Str(s)) => Some(Value::Str(s.clone())),
        Expr::Literal(crate::ast::Literal::Bool(b)) => Some(Value::Bool(*b)),
        Expr::Literal(crate::ast::Literal::Unit) => Some(Value::Unit),
        Expr::InterpolatedStr(parts) => {
            let mut s = String::new();
            for part in parts {
                match part {
                    crate::ast::StrPart::Literal(lit) => s.push_str(lit),
                    crate::ast::StrPart::Parsed(inner) => {
                        let v = literal_expr_to_value(inner)?;
                        s.push_str(&interp_value_to_str(&v)?);
                    }
                }
            }
            Some(Value::Str(s))
        }
        _ => None,
    }
}

fn interp_value_to_str(value: &Value) -> Option<String> {
    match value {
        Value::Str(s) => Some(s.clone()),
        Value::Int(i) => Some(i.to_string()),
        Value::Float(f) => Some(f.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        Value::Unit => Some(String::new()),
        _ => None,
    }
}

// ─── VM helpers + oracle stubs ────────────────────────────────────────────────

fn vm_call_verify_helper(machine: &mut vm::VM, fn_name: &str) -> Result<VmVerifyEval, String> {
    let value = machine
        .run_named_function(fn_name, &[])
        .map_err(|e| e.to_string())?
        .to_value(&machine.arena);

    match value {
        Value::Ok(inner) => Ok(VmVerifyEval::Value(*inner)),
        Value::Err(inner) => Ok(VmVerifyEval::ErrProp(*inner)),
        other => Err(format!(
            "verify helper '{}' returned unexpected shape: {}",
            fn_name,
            aver_repr(&other)
        )),
    }
}

fn vm_call_guard_helper(machine: &mut vm::VM, fn_name: &str) -> Result<Value, String> {
    machine
        .run_named_function(fn_name, &[])
        .map_err(|e| e.to_string())
        .map(|value| value.to_value(&machine.arena))
}

fn dotted_path_from_expr(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(s) => Some(s.clone()),
        Expr::Resolved { name, .. } => Some(name.clone()),
        Expr::Attr(obj, field) => {
            let head = dotted_path_from_expr(&obj.node)?;
            Some(format!("{}.{}", head, field))
        }
        _ => None,
    }
}

fn hostile_stub_name(method: &str, profile: &str) -> String {
    if profile.contains('.') {
        profile.to_string()
    } else {
        format!("__hostile_{}_{}", method.replace('.', "_"), profile)
    }
}

fn build_case_oracle_stubs(
    machine: &vm::VM,
    block: &VerifyBlock,
    case_idx: usize,
    capabilities: &crate::capability::CapabilityRegistry,
) -> HashMap<String, u32> {
    let mut out = HashMap::new();
    let givens_source: &[VerifyGiven] = match &block.kind {
        VerifyKind::Law(law) => law.givens.as_slice(),
        VerifyKind::Cases => block.cases_givens.as_slice(),
    };

    if !givens_source.is_empty()
        && let Some(case_bindings) = block.case_givens.get(case_idx)
    {
        for given in givens_source {
            let Some(classification) = classify_with_registry(capabilities, &given.type_name)
            else {
                continue;
            };
            if matches!(classification.dimension, EffectDimension::Output) {
                continue;
            }
            let Some((_, value_expr)) = case_bindings.iter().find(|(n, _)| n == &given.name) else {
                continue;
            };
            let stub_name = match &value_expr.node {
                Expr::Ident(s) => s.clone(),
                Expr::Resolved { name, .. } => name.clone(),
                _ => {
                    let Some(dotted) = dotted_path_from_expr(&value_expr.node) else {
                        continue;
                    };
                    dotted
                }
            };
            let Some(fn_id) = machine.find_fn_id(&stub_name) else {
                continue;
            };
            out.insert(given.type_name.clone(), fn_id);
        }
    }

    // Layer hostile effect-profile stubs on top of (or instead of) the
    // user's `given` stubs. Effect-profile assignments are computed by
    // `apply_hostile_expansion` and only contain effects the user did
    // not pin via `given`, so user-given always wins for the same method
    // — a hostile-expanded case for an un-given effect installs the
    // profile's synthetic stub fn. This keeps the runner agnostic to
    // which kind of stub it is — the VM only cares about (method →
    // fn_id), and `__hostile_<method>_<profile>` lives as an ordinary
    // FnDef thanks to `inject_hostile_effect_stubs_for_blocks`.
    if let Some(combo) = block.case_hostile_profiles.get(case_idx) {
        for (method, profile) in combo {
            let stub_fn_name = hostile_stub_name(method, profile);
            // `apply_hostile_expansion` only assigns profiles for the
            // classified non-Output effects of the fn under test, and
            // `inject_hostile_effect_stubs_for_blocks` injects a stub
            // FnDef for exactly that same set before type-check — so a
            // profile that reaches this point without its stub in the
            // VM is a pipeline bug, not a user error. Silently skipping
            // it would run the "hostile" case against the real builtin
            // and report a vacuous pass.
            let Some(fn_id) = machine.find_fn_id(&stub_fn_name) else {
                panic!(
                    "hostile profile stub `{stub_fn_name}` for effect `{method}` is missing \
                     from the VM — `inject_hostile_effect_stubs_for_blocks` guarantees the \
                     stub exists for every profile the expansion assigns (compiler bug)"
                );
            };
            out.insert(method.clone(), fn_id);
        }
    }
    out
}

// ─── Case runner ──────────────────────────────────────────────────────────────

fn run_verify_vm(
    plan: &VmVerifyPlan,
    machine: &mut vm::VM,
    capabilities: &crate::capability::CapabilityRegistry,
) -> VerifyResult {
    let block = &plan.block;
    let mut passed = 0;
    let mut failed = 0;
    let mut skipped = 0;
    let mut failures = Vec::new();
    let mut case_results = Vec::new();
    let is_law = matches!(block.kind, VerifyKind::Law(_));
    let case_total = block.cases.len();

    let law_context_template = if let VerifyKind::Law(law) = &block.kind {
        Some(format!(
            "{} == {}",
            expr_to_str(&law.lhs),
            expr_to_str(&law.rhs)
        ))
    } else {
        None
    };

    // Track which case_exprs already failed in their un-effected base
    // (declared) world. Once a base case fails, the per-profile
    // follow-ups for the same `case_expr` would just re-confirm the
    // same counter-example under harder worlds — Aver skips them
    // instead of running the VM. Saves time on every law where a
    // boundary value or declared value already breaks the claim.
    use std::collections::HashSet;
    let mut base_failed: HashSet<String> = HashSet::new();

    for (idx, ((left_expr, right_expr), case_fns)) in
        block.cases.iter().zip(&plan.cases).enumerate()
    {
        let case_str = format!("{} == {}", expr_to_str(left_expr), expr_to_str(right_expr));
        let span = block.case_spans.get(idx).cloned();
        let from_hostile = block
            .case_hostile_origins
            .get(idx)
            .copied()
            .unwrap_or(false);
        let base_hostile_profile =
            render_hostile_profile_label(block.case_hostile_profiles.get(idx).map(Vec::as_slice));
        let case_reverse = block.case_reverse_order.get(idx).copied().unwrap_or(false);
        // Layer the order-axis tag on top of any effect-profile label so
        // a failure renderer surfaces "+reverse-eval" alongside the
        // profile that was active. Lets the user tell apart a baseline
        // mismatch (effect profile failing as expected) from an
        // order-axis catch (Aver claims independent products but a
        // reversed eval gives a different tuple under the same world).
        let hostile_profile = if case_reverse {
            Some(match base_hostile_profile {
                Some(s) => format!("{}+reverse-eval", s),
                None => "reverse-eval".to_string(),
            })
        } else {
            base_hostile_profile
        };

        // Profile-multiplied case for a base that already failed?
        // Skip the VM run; record an outcome variant the renderer
        // suppresses by default but tools can detect via
        // `SkippedAfterBaseFail` in JSON.
        let is_profile_case = block
            .case_hostile_profiles
            .get(idx)
            .map(|c| !c.is_empty())
            .unwrap_or(false);
        if is_profile_case && base_failed.contains(&case_str) {
            skipped += 1;
            let law_context = if let VerifyKind::Law(_) = &block.kind {
                let givens: Vec<(String, String)> = block
                    .case_givens
                    .get(idx)
                    .map(|gs| {
                        gs.iter()
                            .map(|(name, expr)| (name.clone(), expr_to_str(expr)))
                            .collect()
                    })
                    .unwrap_or_default();
                Some(VerifyLawContext {
                    givens,
                    law_expr: law_context_template.clone().unwrap_or_default(),
                })
            } else {
                None
            };
            case_results.push(VerifyCaseResult {
                outcome: VerifyCaseOutcome::SkippedAfterBaseFail,
                span,
                case_expr: case_str,
                case_index: idx,
                case_total,
                law_context,
                from_hostile,
                hostile_profile,
                expected_value: None,
            });
            continue;
        }
        let failure_case = if is_law {
            format!("case {}/{} [{}]", idx + 1, case_total, case_str)
        } else {
            case_str.clone()
        };

        let law_context = if let VerifyKind::Law(_) = &block.kind {
            let givens: Vec<(String, String)> = block
                .case_givens
                .get(idx)
                .map(|gs| {
                    gs.iter()
                        .map(|(name, expr)| (name.clone(), expr_to_str(expr)))
                        .collect()
                })
                .unwrap_or_default();
            Some(VerifyLawContext {
                givens,
                law_expr: law_context_template.clone().unwrap_or_default(),
            })
        } else {
            None
        };

        // Install oracle stubs and start trace collection BEFORE the
        // guard evaluates, so `when` predicates that reference effect
        // stubs (e.g. `when clock(root, 1) > clock(root, 0)` for
        // monotonicity) see the same stub world the case body will
        // see. Without this, a guard like that would fire real-time
        // and either flap or fail the effect gate.
        let oracle_stubs = build_case_oracle_stubs(machine, block, idx, capabilities);
        let has_stubs = !oracle_stubs.is_empty();
        if has_stubs {
            machine.install_oracle_stubs(oracle_stubs);
        }
        let reverse_order = block.case_reverse_order.get(idx).copied().unwrap_or(false);
        machine.set_reverse_independent_eval(reverse_order);
        machine.start_trace_collection();
        let root_fn_id = machine.find_fn_id(&block.fn_name);
        machine.set_trace_root_fn_id(root_fn_id);

        if let Some(guard_name) = &case_fns.guard {
            let guard_result = vm_call_guard_helper(machine, guard_name);
            // Drain any trace events emitted while the guard ran so
            // they don't bleed into the case's trace assertions. The
            // public wrapper also stops trace collection as a side
            // effect, so we restart it on the happy path before the
            // case body runs — otherwise the body would lose effect-
            // gate suppression and `.trace.*` assertions would see
            // an empty buffer.
            let _ = machine.take_trace_events_with_coords();
            let cleanup = |m: &mut vm::VM| {
                if has_stubs {
                    m.clear_oracle_stubs();
                }
                m.set_reverse_independent_eval(false);
            };
            match guard_result {
                Ok(Value::Bool(true)) => {
                    machine.start_trace_collection();
                    machine.set_trace_root_fn_id(root_fn_id);
                }
                Ok(Value::Bool(false)) => {
                    cleanup(machine);
                    skipped += 1;
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Skipped,
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                        expected_value: None,
                    });
                    continue;
                }
                Ok(Value::Err(err_val)) => {
                    cleanup(machine);
                    failed += 1;
                    let err_repr = format!("Result.Err({})", aver_repr(&err_val));
                    failures.push((failure_case, String::new(), err_repr.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::UnexpectedErr { err_repr },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                        expected_value: None,
                    });
                    continue;
                }
                Ok(other) => {
                    cleanup(machine);
                    failed += 1;
                    let error = format!("when produced {}, expected Bool", aver_repr(&other));
                    failures.push((failure_case, "Bool".to_string(), error.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::RuntimeError { error },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                        expected_value: None,
                    });
                    continue;
                }
                Err(e) => {
                    cleanup(machine);
                    failed += 1;
                    let error = format!("guard error: {}", e);
                    failures.push((failure_case, String::new(), error.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::RuntimeError { error },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                        expected_value: None,
                    });
                    continue;
                }
            }
        }

        // Trace collection started above (before the guard). Continue
        // through the case body.
        // The generated verify helper (`__verify_foo_left`) declares
        // no effects, so calling an effectful user fn from it would
        // trip the VM's effect gate for any classified effect that
        // isn't stubbed — e.g. a result-only law on a fn with
        // `! [Random.int, Console.print]` has a stub for Random.int
        // but Console.print is Output (no oracle), and the helper
        // would fail with a runtime effect violation on what the
        // typechecker / proof export both accept.
        // `trace_collecting` is exactly the suppression the runtime
        // uses for classified effects (src/vm/runtime.rs:563), so
        // enabling it here keeps non-trace law evaluation consistent
        // with typecheck + proof. We discard the buffered events for
        // non-trace blocks — they're only consumed by the trace
        // projection path below.
        let left_result = vm_call_verify_helper(machine, &case_fns.left);
        let (lhs_trace_events, lhs_trace_coords): (
            Vec<Value>,
            Vec<crate::vm::runtime::TraceCoord>,
        ) = if block.trace {
            machine.take_trace_events_with_coords()
        } else {
            // Drain and discard so the buffer stays clean between
            // cases and nothing leaks into the next plan.
            let _ = machine.take_trace_events_with_coords();
            (Vec::new(), Vec::new())
        };

        let left_result = apply_trace_projection(
            left_result,
            &block.cases[idx].0,
            &lhs_trace_events,
            &lhs_trace_coords,
            block.trace,
        );

        let right_result = vm_call_verify_helper(machine, &case_fns.right);

        if has_stubs {
            machine.clear_oracle_stubs();
        }
        machine.set_reverse_independent_eval(false);

        match (left_result, right_result) {
            (Ok(VmVerifyEval::Value(left_val)), Ok(VmVerifyEval::Value(right_val))) => {
                if left_val == right_val {
                    passed += 1;
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Pass,
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                        // Ground truth for proof export: on Pass, the
                        // right side's VM value IS the case's expected
                        // value (and equals the impl side). Lean emission
                        // literalizes the expected side from it so bounded
                        // sample checks compare model vs program result.
                        expected_value: Some(right_val),
                    });
                } else {
                    failed += 1;
                    let expected = aver_repr(&right_val);
                    let mut actual = aver_repr(&left_val);
                    if block.trace && is_trace_projection_shape(&block.cases[idx].0) {
                        actual.push_str(&format_trace_events_tail(&lhs_trace_events));
                    }
                    failures.push((failure_case, expected.clone(), actual.clone()));
                    case_results.push(VerifyCaseResult {
                        outcome: VerifyCaseOutcome::Mismatch { expected, actual },
                        span,
                        case_expr: case_str,
                        case_index: idx,
                        case_total,
                        law_context,
                        from_hostile,
                        hostile_profile: hostile_profile.clone(),
                        expected_value: None,
                    });
                }
            }
            (Ok(VmVerifyEval::ErrProp(err_val)), _) | (_, Ok(VmVerifyEval::ErrProp(err_val))) => {
                failed += 1;
                let err_repr = format!("Result.Err({})", aver_repr(&err_val));
                failures.push((failure_case, String::new(), err_repr.clone()));
                case_results.push(VerifyCaseResult {
                    outcome: VerifyCaseOutcome::UnexpectedErr { err_repr },
                    span,
                    case_expr: case_str,
                    case_index: idx,
                    case_total,
                    law_context,
                    from_hostile,
                    hostile_profile: hostile_profile.clone(),
                    expected_value: None,
                });
            }
            (Err(e), _) | (_, Err(e)) => {
                failed += 1;
                let error = e.to_string();
                failures.push((failure_case, String::new(), error.clone()));
                case_results.push(VerifyCaseResult {
                    outcome: VerifyCaseOutcome::RuntimeError { error },
                    span,
                    case_expr: case_str,
                    case_index: idx,
                    case_total,
                    law_context,
                    from_hostile,
                    hostile_profile: hostile_profile.clone(),
                    expected_value: None,
                });
            }
        }

        // After this case is fully resolved, if it was a base
        // (un-effected) case and ended in `Mismatch`, mark its
        // `case_expr` so subsequent profile-multiplied cases for the
        // same case_expr skip the VM run entirely.
        if !is_profile_case
            && let Some(last) = case_results.last()
            && matches!(last.outcome, VerifyCaseOutcome::Mismatch { .. })
        {
            base_failed.insert(last.case_expr.clone());
        }
    }

    let block_label = match &block.kind {
        VerifyKind::Law(law) => format!("{} law {}", block.fn_name, law.name),
        VerifyKind::Cases => block.fn_name.clone(),
    };
    VerifyResult {
        fn_name: block.fn_name.clone(),
        is_law: matches!(&block.kind, VerifyKind::Law(_)),
        block_label,
        passed,
        failed,
        skipped,
        case_results,
        failures,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, Spanned, VerifyLaw};

    fn parse_source(src: &str) -> Vec<TopLevel> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse().expect("parse")
    }

    #[test]
    fn collect_effect_profile_returns_cartesian_for_unfixed_effects() {
        let src = "module M\n    effects [Time.now, Random.int]\n\nfn f() -> Int\n    ? \"toy\"\n    ! [Time.now, Random.int]\n    1\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            case_reverse_order: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: true,
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        // Profile counts grow per effect; what matters here is shape:
        // every combo references one Time.now profile + one Random.int
        // profile, and the cartesian equals the product of the per-method
        // profile counts.
        let time_now_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        let random_int_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Random.int").len();
        assert!(time_now_profiles >= 2);
        assert!(random_int_profiles >= 2);
        assert_eq!(combos.len(), time_now_profiles * random_int_profiles);
        for combo in &combos {
            assert_eq!(combo.len(), 2);
        }
    }

    /// Regression: `Tcp.sendBytes` was classified (GenerativeOutput)
    /// without shipping hostile profiles, so the collector produced no
    /// combination for it and hostile law verification silently ran with
    /// the effect unmodelled.
    #[test]
    fn collect_effect_profile_covers_tcp_send_bytes() {
        let src = "module M\n    effects [Tcp.sendBytes]\n\nfn f() -> Int\n    ? \"toy\"\n    ! [Tcp.sendBytes]\n    1\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            case_reverse_order: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: true,
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        let profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Tcp.sendBytes");
        assert!(profiles.len() >= 2);
        assert_eq!(combos.len(), profiles.len());
        assert!(
            combos.iter().any(|combo| combo
                .iter()
                .any(|(m, p)| m == "Tcp.sendBytes" && p == "always_err")),
            "hostile profile must appear among the combinations"
        );
    }

    #[test]
    fn collect_effect_profile_applies_to_law_form_with_or_without_trace() {
        // Law form is a universal claim. Even without `trace`, the user's
        // `given <Effect>` is a runtime stub (one chosen world); hostile
        // profiles ask "does this law hold under every world?". So the
        // cartesian fires.
        let src = "module M\n    effects [Time.now]\n\nfn f() -> Int\n    ? \"toy\"\n    ! [Time.now]\n    1\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            case_reverse_order: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: false, // no trace, still law → still gets effect-side
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        let time_now = crate::types::checker::hostile_effects::hostile_profiles_for("Time.now");
        assert_eq!(combos.len(), time_now.len());
    }

    #[test]
    fn collect_effect_profile_excludes_output_effects() {
        // Console.print is Output — no oracle response to vary, so no
        // profiles. Use trace+law shape since that's the only form
        // where effect-side hostile applies in the first place.
        let src = "module M\n    effects [Console.print]\n\nfn greet() -> Unit\n    ? \"toy\"\n    ! [Console.print]\n    Console.print(\"hi\")\n";
        let items = parse_source(src);
        let block = VerifyBlock {
            fn_name: "greet".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            case_reverse_order: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: true,
            cases_givens: vec![],
        };
        let combos = collect_effect_profile_combinations(&block, &items);
        assert!(combos.is_empty());
    }

    #[test]
    fn inject_hostile_stubs_appends_fn_defs_for_unfixed_classified_effects() {
        let src = "module M\n    effects [Time.now]\n\nfn f() -> String\n    ? \"toy\"\n    ! [Time.now]\n    Time.now()\n";
        let mut items = parse_source(src);
        let blocks = vec![VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![],
            case_reverse_order: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: true,
            cases_givens: vec![],
        }];
        let before = items.len();
        inject_hostile_effect_stubs_for_blocks(&mut items, &blocks);
        let after = items.len();
        let expected =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        assert_eq!(after - before, expected);
        let fn_names: Vec<String> = items
            .iter()
            .filter_map(|i| match i {
                TopLevel::FnDef(fd) => Some(fd.name.clone()),
                _ => None,
            })
            .collect();
        assert!(fn_names.contains(&"__hostile_Time_now_frozen".to_string()));
        assert!(fn_names.contains(&"__hostile_Time_now_epoch".to_string()));
    }

    #[test]
    fn apply_hostile_expansion_multiplies_cases_by_effect_cartesian() {
        // Trace+law form on a fn that uses Time.now. Effect-side hostile
        // only applies to `verify <fn> trace law <name>` (a universal
        // trace claim); cases-form trace is a fixture and opts out.
        // Hostile expansion produces: 1 declared case × (1 un-effected
        // base + K Time.now profiles).
        let src = r#"module M
    effects [Time.now]

fn frozenStub(path: BranchPath, n: Int) -> String
    "2026-01-01T00:00:00Z"

fn currentYear() -> String
    ? "current year"
    ! [Time.now]
    Time.now()

verify currentYear trace law alwaysReturns
    given clock: Time.now = [frozenStub]
    currentYear().result => "2026-01-01T00:00:00Z"
"#;
        let items = parse_source(src);
        let mut blocks = crate::checker::merge_verify_blocks(&items);
        assert_eq!(blocks.len(), 1);
        let block = &mut blocks[0];
        let declared_count = block.cases.len();
        assert_eq!(declared_count, 1);

        apply_hostile_expansion(block, &items).expect("expansion within budget");

        let time_now_profiles =
            crate::types::checker::hostile_effects::hostile_profiles_for("Time.now").len();
        let expected_total = declared_count * (1 + time_now_profiles);
        assert_eq!(block.cases.len(), expected_total);
        assert_eq!(block.case_hostile_profiles.len(), expected_total);
        let base_count = block
            .case_hostile_profiles
            .iter()
            .filter(|p| p.is_empty())
            .count();
        assert_eq!(base_count, declared_count);
        let profile_count = block
            .case_hostile_profiles
            .iter()
            .filter(|p| !p.is_empty())
            .count();
        assert_eq!(profile_count, declared_count * time_now_profiles);
    }

    #[test]
    fn cases_form_trace_opts_out_of_effect_side_hostile() {
        // `verify <fn> trace` (no `law`) is a fixture — explicit scenario
        // with one chosen stub. Effect-side hostile would change its
        // semantics from "this scenario" to "every scenario", which the
        // user did not write. The block stays untouched on the effect
        // side; the cases-form `trace` semantics are preserved.
        let src = r#"module M
    effects [Time.now]

fn frozenStub(path: BranchPath, n: Int) -> String
    "2026-01-01T00:00:00Z"

fn currentYear() -> String
    ? "current year"
    ! [Time.now]
    Time.now()

verify currentYear trace
    given clock: Time.now = [frozenStub]
    currentYear().result => "2026-01-01T00:00:00Z"
"#;
        let items = parse_source(src);
        let combos = collect_effect_profile_combinations(
            &crate::checker::merge_verify_blocks(&items)[0],
            &items,
        );
        assert!(
            combos.is_empty(),
            "cases-form trace is a fixture; effect-side hostile must not multiply it"
        );
    }

    #[test]
    fn apply_hostile_expansion_rejects_over_budget_cartesian() {
        // Stress: pick a `given x: Int` range big enough to clear the
        // hostile cap once value-boundary cases are added but small
        // enough that the parser still accepts it. Effect-side hostile
        // doesn't apply to law form, so this test exercises just the
        // value-side cap path.
        let big_range = VERIFY_HOSTILE_MAX_CASES + 50;
        let src = format!(
            "module M\n    effects []\n\nfn f(x: Int) -> Int\n    ? \"toy\"\n    x\n\nverify f law big\n    given x: Int = 1..{}\n    f(x) => x\n",
            big_range
        );
        // The parser may reject the range first (its own cap is also
        // 10_000); either path keeps us safe. Test passes if at least
        // one of the two layers stops the run.
        let parse_result = crate::source::parse_source(&src);
        match parse_result {
            Err(_) => {
                // Parser-side cap fired — fine, that's the first line of
                // defence and the user gets a clear error already.
            }
            Ok(items) => {
                let mut blocks = crate::checker::merge_verify_blocks(&items);
                if let Some(block) = blocks.first_mut() {
                    let res = apply_hostile_expansion(block, &items);
                    assert!(
                        res.is_err(),
                        "expected --hostile cap to reject over-budget expansion"
                    );
                    assert!(
                        res.unwrap_err().contains("more than"),
                        "error should mention the cap"
                    );
                }
            }
        }
    }

    /// Structural guard for the injection path: for EVERY classified
    /// non-Output effect method, a law block over a fn declaring that
    /// effect must get every one of the method's hostile profile stubs
    /// injected as a `TopLevel::FnDef`. Before this guard, the injector
    /// routed method names through a hand-maintained `&'static str`
    /// mapping whose catch-all returned `""` — `Tcp.sendBytes` /
    /// `Tcp.readBytes` / `Tcp.writeBytes` fell through it and their
    /// hostile profiles were silently never injected. Paired with
    /// `hostile_effects::tests::every_classified_non_output_effect_ships_hostile_profiles`
    /// (which forbids empty profile lists), this makes it impossible for
    /// the next new classified effect to vanish from hostile verification
    /// without a test failure.
    #[test]
    fn inject_hostile_stubs_covers_every_classified_non_output_effect() {
        use crate::types::checker::effect_classification::classifications_for_proof_subset;

        for classification in classifications_for_proof_subset() {
            if matches!(classification.dimension, EffectDimension::Output) {
                continue;
            }
            let method = classification.method;
            let src = format!(
                "module M\n    effects [{method}]\n\nfn f() -> Int\n    ? \"toy\"\n    ! [{method}]\n    1\n"
            );
            let mut items = parse_source(&src);
            let blocks = vec![VerifyBlock {
                fn_name: "f".to_string(),
                line: 1,
                cases: vec![],
                case_spans: vec![],
                case_givens: vec![],
                case_hostile_origins: vec![],
                case_hostile_profiles: vec![],
                case_reverse_order: vec![],
                kind: VerifyKind::Law(Box::new(VerifyLaw {
                    name: "test".to_string(),
                    givens: vec![],
                    when: None,
                    lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                    rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                    sample_guards: vec![],
                })),
                trace: true,
                cases_givens: vec![],
            }];
            inject_hostile_effect_stubs_for_blocks(&mut items, &blocks);
            let fn_names: std::collections::HashSet<String> = items
                .iter()
                .filter_map(|i| match i {
                    TopLevel::FnDef(fd) => Some(fd.name.clone()),
                    _ => None,
                })
                .collect();
            for profile in crate::types::checker::hostile_effects::hostile_profiles_for(method) {
                assert!(
                    fn_names.contains(&profile.stub_fn_name),
                    "hostile profile stub {} for {} was not injected — the injection path \
                     silently dropped a classified effect",
                    profile.stub_fn_name,
                    method
                );
            }
        }
    }

    #[test]
    fn render_hostile_profile_label_joins_pairs() {
        let combo = vec![
            ("Time.now".to_string(), "frozen".to_string()),
            ("Random.int".to_string(), "min".to_string()),
        ];
        assert_eq!(
            render_hostile_profile_label(Some(&combo)),
            Some("Time.now/frozen + Random.int/min".to_string())
        );
        assert_eq!(render_hostile_profile_label(Some(&[])), None);
        assert_eq!(render_hostile_profile_label(None), None);
    }

    /// Since the injection fix (`inject_hostile_effect_stubs_for_blocks`
    /// keys `hostile_profiles_for` on the classified method verbatim),
    /// every profile the expansion assigns has its stub FnDef in the VM.
    /// A miss is a pipeline bug — the runner must fail loudly instead of
    /// silently running the "hostile" case against the real builtin.
    #[test]
    #[should_panic(
        expected = "hostile profile stub `__hostile_Time_now_frozen` for effect `Time.now` is missing"
    )]
    fn missing_hostile_stub_fn_is_a_hard_error() {
        let src = "module M\n    effects []\n\nfn f() -> Int\n    ? \"toy\"\n    1\n";
        let mut items = parse_source(src);
        crate::ir::pipeline::resolve(&mut items);
        let mut arena = Arena::new();
        vm::register_service_types(&mut arena);
        let symbol_table = crate::ir::SymbolTable::build(&items, &[]);
        let resolved_items = crate::ir::hir::resolve_program(&symbol_table, &items);
        let (code, globals) = vm::compile_program_with_modules(
            &resolved_items,
            &symbol_table,
            &mut arena,
            None,
            "missing_stub_test.av",
            None,
        )
        .expect("compile");
        let machine = vm::VM::new(code, globals, arena);

        // A hostile-expanded case referencing a profile whose stub was
        // never injected (nothing named `__hostile_Time_now_frozen`
        // exists in the program above).
        let block = VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![],
            case_spans: vec![],
            case_givens: vec![],
            case_hostile_origins: vec![],
            case_hostile_profiles: vec![vec![("Time.now".to_string(), "frozen".to_string())]],
            case_reverse_order: vec![],
            kind: VerifyKind::Law(Box::new(VerifyLaw {
                name: "test".to_string(),
                givens: vec![],
                when: None,
                lhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                rhs: Spanned::bare(Expr::Literal(Literal::Unit)),
                sample_guards: vec![],
            })),
            trace: false,
            cases_givens: vec![],
        };
        let _ = build_case_oracle_stubs(
            &machine,
            &block,
            0,
            &crate::capability::CapabilityRegistry::default(),
        );
    }
}
