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

/// How generated source spells the types whose source spelling would not
/// name them in the module it is generated into, keyed by identity; see
/// `SymbolTable::generated_type_spellings`.
pub(crate) type TypeSpellings = std::collections::HashMap<crate::ir::TypeId, String>;

/// A stamped type as generated source must write it: the source spelling,
/// except where that spelling names another type (or none) in this module,
/// where it is the declaring module's qualified name.
pub(crate) fn spell_type(ty: &crate::ast::Type, spellings: &TypeSpellings) -> String {
    ty.display_with(&|id| spellings.get(&id).cloned())
}

mod build;
mod carried_waits;
mod coordinator;
mod lower;
mod trace;
mod verify;

/// The stop observation a target can supply to a generated coordinator.
/// This changes only the generated turn; explicit capability calls still
/// have to be supported by the target.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum CoordinatorStop {
    #[default]
    HostSignal,
    /// WASI 0.2 has no signal subscription. The loop ends through its policy
    /// or the normal exhaustion rule, with `View.stopping` remaining false.
    PolicyOnly,
}

#[derive(Debug, Clone, Default)]
pub struct YieldLoweringReport {
    /// Names of the functions that were lowered, in source order.
    pub lowered: Vec<String>,
    /// The generated items, in the order they were spliced into the module.
    pub generated: Vec<TopLevel>,
    /// One entry per lowered function: the protocol the loop generator
    /// dispatches over.
    pub protocols: Vec<ProcessProtocol>,
    /// Original stamped definitions, including private helpers, before any
    /// body is replaced by protocol code. The source observer reads these.
    pub sources: Vec<FnDef>,
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
    /// Nested-call routers, retained as data for compositional trace obligations.
    pub nests: Vec<ProcessNest>,
    /// Source observer exported by the owning module, when its effects are modeled.
    pub trace: Option<ProcessTrace>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessNest {
    pub callee: String,
    pub router: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessTrace {
    /// Ordinary helper and splice laws required by the root correspondence law.
    pub dependencies: Vec<String>,
    /// The retained source cone contains recursion, including through helpers.
    /// Imported subtraces still need a composition theorem; Yield alone is not recursion.
    pub recursive: bool,
    pub operations: Vec<ProtocolKind>,
    pub input: String,
    pub query: String,
    pub event: String,
    pub result: String,
    pub source: String,
    /// Owning-module observers and law dependencies used by import adapters.
    pub drive: String,
    pub protocol_from: String,
    /// Pure observations of actual effectful Start/Answer bodies in their owner.
    pub segments: Vec<ProcessTraceSegment>,
    pub cursor: Option<String>,
    pub correspondence: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessTraceSegment {
    pub function: String,
    pub observer: String,
    pub result: String,
    pub params: Vec<(String, String)>,
    /// The owning module's cursor wrapper for this observation, when it checked
    /// one, so an importer's lift adapter can cite that contract instead of
    /// reopening the observer's body. Empty when the owner checked none.
    pub cursor: String,
    /// One entry per parameter: the owning module's public sample function for
    /// that parameter's type, or empty when the type needs none. A protocol
    /// state is built from constructors that stay private to its owner, so an
    /// importer that must quantify over one calls the owner's sample instead of
    /// spelling a constructor it cannot name. A type nobody publishes keeps an
    /// empty entry and stays unsampled, so the law is declined rather than
    /// written against a name that does not resolve.
    pub samples: Vec<String>,
}

/// Public source signature and its lowered protocol, retained across module loading.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessExport {
    pub protocol: ProcessProtocol,
    pub effects: Vec<String>,
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
    /// The state type's variants: one per stop of this kind, with the declared
    /// type of each live variable it carries, in order. A caller reads the
    /// layout to write a sample of an imported stop it can never construct by
    /// name; the arity is the length of the list.
    pub variants: Vec<(String, Vec<String>)>,
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

/// A `main` that reads `Run.lastTurn` in a program that runs no generated
/// loop: there is no turn to report, so the program is refused at check time
/// instead of reading `0/0` forever. `has_loop` is whether the lowering of
/// this module generated one. A module with no `main` is a library, and the
/// program that depends on it decides; a `main` that calls `Run.all()` has a
/// loop whenever its module writes a process, and is refused for that on its
/// own when it does not.
pub fn last_turn_without_loop(items: &[TopLevel], has_loop: bool) -> Option<TypeError> {
    if has_loop {
        return None;
    }
    items.iter().find_map(|item| match item {
        TopLevel::FnDef(fd)
            if fd.name == "main"
                && fd
                    .effects
                    .iter()
                    .any(|effect| effect.node == coordinator::LAST_TURN) =>
        {
            Some(error_at(
                fd.line,
                format!(
                    "'main' reads {} (its effect list names it), but this program runs no generated loop, so there is no turn to report. {} answers only inside a program run by Run.all(): write a process and let the loop run it, or drop the effect",
                    coordinator::LAST_TURN,
                    coordinator::LAST_TURN
                ),
            ))
        }
        _ => None,
    })
}

/// Whether a module calls `Wait.poll` anywhere, read off its source.
pub fn calls_wait_poll(items: &[TopLevel]) -> bool {
    carried_waits::calls_wait_poll(items)
}

/// Carry the waits of a module with no process through an `Int`-keyed wait;
/// see `carried_waits`. `stamped` is the module after a type check. Answers
/// the generated source when anything was carried.
pub fn carry_waits(
    items: &mut Vec<TopLevel>,
    stamped: &[TopLevel],
    spellings: &TypeSpellings,
) -> Result<Option<String>, Vec<TypeError>> {
    carried_waits::carry(items, stamped, spellings, &|_| false)
        .map(|carried| carried.map(|carried| carried.source))
        .map_err(|parse| {
            vec![error_at(
                1,
                format!(
                    "internal error carrying this module's waits through an Int-keyed wait: {parse}; please report this program"
                ),
            )]
        })
}

pub fn calls_run_all(fd: &FnDef) -> bool {
    coordinator::calls_run_all(fd)
}

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
#[allow(clippy::too_many_arguments)]
pub fn lower(
    items: &mut Vec<TopLevel>,
    stamped: &[TopLevel],
    stamped_errors: &[TypeError],
    marked: &crate::config::MarkedCapabilities,
    fn_sigs: &FnSigs,
    imported: &std::collections::HashMap<String, ProcessProtocol>,
    type_spellings: &TypeSpellings,
    coordinator_stop: CoordinatorStop,
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

    let callable_processes: HashSet<String> =
        yield_fns.iter().chain(imported.keys()).cloned().collect();
    for item in stamped {
        match item {
            TopLevel::FnDef(fd) => scan_fn(fd, &callable_processes, &mut errors),
            TopLevel::Verify(vb) => scan_verify(vb, &yield_fns, &mut errors),
            _ => {}
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    // A helper is lowered before the process that enters it, because a
    // nested state is built from the helper's own protocol. A cycle in that
    // graph would be an infinite state, so it is refused instead.
    let calls = nested_calls(stamped, &yield_fns);
    let order = match lowering_order(&calls) {
        Ok(order) => order,
        Err(cycle) => {
            errors.push(mutual_nesting_error(&cycle, &calls, stamped));
            return Err(errors);
        }
    };
    let mut nesting = lower::Nesting::new(yield_fns.clone(), imported);
    let mut lowered: std::collections::HashMap<String, lower::Generated> =
        std::collections::HashMap::new();
    let mut failed: HashSet<String> = HashSet::new();
    for name in &order {
        let Some(fd) = stamped.iter().find_map(|item| match item {
            TopLevel::FnDef(fd) if &fd.name == name && is_yield_fn(fd) => Some(fd),
            _ => None,
        }) else {
            continue;
        };
        // A helper that could not be lowered has no protocol, so the process
        // that enters it is left as written too: one report of the real
        // reason, not a second one about a name that was never generated.
        if calls
            .get(name)
            .is_some_and(|called| called.iter().any(|edge| failed.contains(&edge.callee)))
        {
            failed.insert(name.clone());
            continue;
        }
        match lower::lower_fn(fd, marked, fn_sigs, type_spellings, &nesting) {
            Ok(generated) => {
                nesting.record(&generated);
                lowered.insert(name.clone(), generated);
            }
            Err(mut fn_errors) => {
                errors.append(&mut fn_errors);
                failed.insert(name.clone());
            }
        }
    }

    let mut report = YieldLoweringReport {
        sources: stamped
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd.clone()),
                _ => None,
            })
            .collect(),
        ..Default::default()
    };
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
        // The function stays in the module exactly as written when its
        // lowering failed. It will not run — the errors below stop this
        // door — but every later diagnostic is then about the user's own
        // code instead of about a function that silently vanished.
        let Some(generated) = lowered.remove(&fd.name) else {
            out.push(item);
            continue;
        };
        report.lowered.push(fd.name.clone());
        report.protocols.push(generated.protocol.clone());
        report.generated.extend(generated.items.iter().cloned());
        exposes_rewrite.push((fd.name.clone(), generated.public_names));
        out.extend(generated.items);
    }
    *items = out;
    if !errors.is_empty() {
        return Err(errors);
    }

    let module_name = items.iter().find_map(|item| match item {
        TopLevel::Module(module) => Some(module.name.clone()),
        _ => None,
    });
    let plan = marked.run();
    let entry = coordinator::is_run_module(module_name.as_deref(), plan);
    let traces = trace::generate(
        items,
        &report.sources,
        &mut report.protocols,
        fn_sigs,
        type_spellings,
        imported,
        entry,
    )?;
    report.generated.extend(traces.iter().cloned());
    items.extend(traces);
    report
        .generated
        .extend(trace::strengthen_laws(items, &mut report.protocols));
    let verification = verify::generate(items, &report.protocols, fn_sigs)?;
    report.generated.extend(verification.iter().cloned());
    items.extend(verification);

    // The loop, generated into the program's entry module when that module
    // writes a process the loop can seat and either has no `main` or has a
    // `main` that calls `Run.all()`. An entry whose own `main` drives the
    // protocol by hand gets no loop.
    if entry {
        let plan = plan.expect("checked by is_run_module");
        let seatings: Vec<ProcessSeating> = stamped
            .iter()
            .find_map(|item| match item {
                TopLevel::Module(module) => Some(module.seatings.clone()),
                _ => None,
            })
            .unwrap_or_default();
        let main = stamped.iter().find_map(|item| match item {
            TopLevel::FnDef(fd) if fd.name == "main" => Some(fd),
            _ => None,
        });
        let runs_all = main.is_some_and(coordinator::calls_run_all);
        // The loop seats the processes, and a helper is not one: it is
        // entered through the protocol of the process that calls it, whose
        // own request sum already carries the helper's requests.
        let entered: HashSet<&str> = calls
            .values()
            .flat_map(|called| called.iter().map(|edge| edge.callee.as_str()))
            .collect();
        for seating in &seatings {
            if !report.lowered.contains(&seating.process) {
                errors.push(error_at(seating.line, format!(
                    "`process {} seated by {}` names no yielding function of this module; a process is a function whose effect list names `yield`",
                    seating.process, seating.by
                )));
            } else if entered.contains(seating.process.as_str()) {
                errors.push(error_at(seating.line, format!(
                    "`process {} seated by {}` names a yielding helper another process of this module enters; the loop seats processes, and a helper runs inside the process that calls it",
                    seating.process, seating.by
                )));
            }
        }
        if !errors.is_empty() {
            return Err(errors);
        }
        // What the loop seats: a yielding function that answers Unit, or one
        // the module declares a seating for. One that answers something else
        // and has no seating is a protocol the program drives or verifies
        // itself. A Unit process with parameters and no seating is handed to
        // the generator all the same, which says what it is missing.
        let seated: Vec<ProcessProtocol> = report
            .protocols
            .iter()
            .filter(|protocol| !entered.contains(protocol.fn_name.as_str()))
            .filter(|protocol| {
                seatings
                    .iter()
                    .any(|seating| seating.process == protocol.fn_name)
                    || protocol.return_type == "Unit"
            })
            .cloned()
            .collect();
        let generate = !seated.is_empty() && (main.is_none() || runs_all);
        if runs_all && seated.is_empty() {
            let line = main.map(|fd| fd.line).unwrap_or(1);
            return Err(vec![error_at(line, "'main' calls Run.all(), which runs the generated loop, but this module writes no process the loop can seat: a yielding function that answers Unit, or one a `process ... seated by ...` line names".to_string())]);
        }
        if !generate && !seatings.is_empty() {
            let line = seatings[0].line;
            return Err(vec![error_at(line, "this module declares a seated process, and a seated process runs under the generated loop; its own 'main' does not call Run.all(). Call Run.all() from 'main', or remove 'main' and let the loop's own be generated".to_string())]);
        }
        if generate {
            let mut generated = coordinator::generate(
                items,
                &report.generated,
                &seated,
                &seatings,
                plan,
                fn_sigs,
                coordinator_stop,
            )?;
            report.loop_source = Some(generated.source);
            report.generated.extend(generated.items.iter().cloned());
            coordinator::rewrite_run_names(items);
            items.extend(generated.items);
            if main.is_none() {
                // Every other door reports what `main` answered. A WASI 0.2
                // component has no host that does, so when a turn can fail
                // the run, its generated `main` writes the reason to stderr
                // itself before answering it.
                let reports = coordinator_stop == CoordinatorStop::PolicyOnly
                    && generated
                        .module_effects
                        .iter()
                        .any(|effect| effect == "Run.failure");
                if reports
                    && !generated
                        .module_effects
                        .iter()
                        .any(|effect| effect == "Console.error")
                {
                    generated.module_effects.push("Console.error".to_string());
                    generated.module_effects.sort();
                }
                let main_source = if reports {
                    format!(
                        "fn main() -> Result<Unit, String>\n    ? \"Runs the generated loop until it is over, and writes the reason a failed run gave to stderr.\"\n    ! [{}]\n    __reported(__all())\n\nfn __reported(ran: Result<Unit, String>) -> Result<Unit, String>\n    ? \"What the run answered, with the reason of a failed one written to stderr first.\"\n    ! [Console.error]\n    match ran\n        Result.Ok(_) -> ran\n        Result.Err(reason) -> __failedWith(reason)\n\nfn __failedWith(reason: String) -> Result<Unit, String>\n    ? \"A failed run's reason, written to stderr and answered.\"\n    ! [Console.error]\n    Console.error(reason)\n    Result.Err(reason)\n",
                        generated.module_effects.join(", ")
                    )
                } else {
                    format!(
                        "fn main() -> Result<Unit, String>\n    ? \"Runs the generated loop until it is over.\"\n    ! [{}]\n    __all()\n",
                        generated.module_effects.join(", ")
                    )
                };
                let tokens = crate::lexer::Lexer::new(&main_source)
                    .tokenize()
                    .expect("generated main lexes");
                let parsed = crate::parser::Parser::new_compiler_generated(tokens)
                    .parse()
                    .expect("generated main parses");
                if let Some(source) = report.loop_source.as_mut() {
                    source.push('\n');
                    source.push_str(&main_source);
                }
                report.generated.extend(parsed.iter().cloned());
                items.extend(parsed);
            }
            // The program declared the effects its processes perform; the
            // turn performs the wait, the stop observation and the clock
            // reading besides, and the module's own boundary and the `main`
            // that runs the loop have to admit what is generated into them.
            for item in items.iter_mut() {
                match item {
                    TopLevel::Module(module) => {
                        let Some(declared) = module.effects.as_mut() else {
                            continue;
                        };
                        for effect in &generated.module_effects {
                            if !declared.iter().any(|entry| entry == effect) {
                                declared.push(effect.clone());
                            }
                        }
                    }
                    TopLevel::FnDef(fd) if fd.name == "main" && runs_all => {
                        for effect in &generated.module_effects {
                            if !fd.effects.iter().any(|entry| &entry.node == effect) {
                                fd.effects.push(Spanned::new(effect.clone(), fd.line));
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // The generated loop keys its wait by `Int`. A program that answers a
    // capability of its own carries every other wait it writes through an
    // `Int`-keyed one, so the loop's wait and the program's own can meet in
    // one program.
    if !marked.is_empty() {
        let carried = carried_waits::carry(items, stamped, type_spellings, &|name| {
            yield_fns.contains(name)
        })
        .map_err(|parse| {
            vec![error_at(
                1,
                format!(
                    "internal error carrying this module's waits through an Int-keyed wait: {parse}; please report this program"
                ),
            )]
        })?;
        if let Some(carried) = carried {
            report.generated.extend(carried.items);
            match report.loop_source.as_mut() {
                Some(source) => source.push_str(&carried.source),
                None => report.loop_source = Some(carried.source),
            }
        }
    }

    // Materialize the default export surface before replacing source process
    // names with reserved protocol names (which the underscore rule hides).
    let exports = crate::visibility::collect_module_exports(stamped);
    let default_exposes: Vec<String> = exports
        .functions
        .iter()
        .map(|fd| fd.name.clone())
        .chain(
            exports
                .types
                .iter()
                .filter(|ty| !ty.is_opaque)
                .map(|ty| match ty.def {
                    crate::ast::TypeDef::Sum { name, .. }
                    | crate::ast::TypeDef::Product { name, .. } => name.clone(),
                }),
        )
        .collect();
    // An exposed `yield` function exposes its protocol instead.
    for item in items.iter_mut() {
        let TopLevel::Module(module) = item else {
            continue;
        };
        module.yield_sources = report.sources.clone();
        if module.exposes.is_empty() {
            module.exposes = default_exposes.clone();
        }
        for protocol in &report.protocols {
            if module.exposes.contains(&protocol.fn_name) {
                let effects = stamped
                    .iter()
                    .find_map(|item| match item {
                        TopLevel::FnDef(fd) if fd.name == protocol.fn_name => {
                            Some(fd.effects.iter().map(|e| e.node.clone()).collect())
                        }
                        _ => None,
                    })
                    .unwrap_or_default();
                module.yield_protocols.push(ProcessExport {
                    protocol: protocol.clone(),
                    effects,
                });
            }
        }
        for (fn_name, public_names) in &exposes_rewrite {
            if let Some(pos) = module.exposes.iter().position(|e| e == fn_name) {
                module.exposes.remove(pos);
                let mut public_names = public_names.clone();
                if let Some(trace) = report
                    .protocols
                    .iter()
                    .find(|p| p.fn_name == *fn_name)
                    .and_then(|p| p.trace.as_ref())
                {
                    public_names.extend([
                        trace.input.clone(),
                        trace.query.clone(),
                        trace.event.clone(),
                        trace.result.clone(),
                        trace.source.clone(),
                        trace.drive.clone(),
                        trace.protocol_from.clone(),
                    ]);
                    public_names.extend(trace.cursor.iter().cloned());
                    for segment in &trace.segments {
                        public_names.extend([segment.observer.clone(), segment.result.clone()]);
                        // An importer quantifying over this segment's state
                        // calls the sample instead of naming a constructor the
                        // owner keeps private, so the sample travels with the
                        // observer it belongs to.
                        public_names
                            .extend(segment.samples.iter().filter(|s| !s.is_empty()).cloned());
                        // An importer's adapter cites this observation's cursor
                        // and event-prefix contracts instead of reopening its
                        // body, so the wrappers the contracts are stated about
                        // travel too.
                        if !segment.cursor.is_empty() {
                            public_names.push(segment.cursor.clone());
                            public_names.push(format!("{}Prefixed", segment.observer));
                        }
                    }
                    if trace.correspondence.is_some() {
                        public_names.push(format!("__{fn_name}SourceTraceFrom"));
                    }
                }
                for (offset, name) in public_names.iter().enumerate() {
                    module.exposes.insert(pos + offset, name.clone());
                }
            }
        }
    }
    Ok(report)
}

/// One `yield` function of this module calling another: which one, the line
/// of the first such call, and whether every call site is a tail call.
struct NestedCall {
    callee: String,
    line: usize,
    only_tail: bool,
}

/// Which `yield` functions of this module each one calls. A self call is not
/// an edge: it is the process's own loop, which stays inside its own machine.
type NestedCalls = std::collections::BTreeMap<String, Vec<NestedCall>>;

fn nested_calls(stamped: &[TopLevel], yield_fns: &HashSet<String>) -> NestedCalls {
    let mut calls = NestedCalls::new();
    for item in stamped {
        let TopLevel::FnDef(fd) = item else { continue };
        if !is_yield_fn(fd) {
            continue;
        }
        let mut called: Vec<NestedCall> = Vec::new();
        for stmt in fd.body.stmts() {
            let expr = match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr,
            };
            expr_walk::walk(expr, &mut |e| {
                let (name, tail) = match &e.node {
                    Expr::FnCall(callee, _) => match &callee.node {
                        Expr::Ident(name) => (Some(name.clone()), false),
                        _ => (None, false),
                    },
                    Expr::TailCall(tc) => (Some(tc.target.clone()), true),
                    _ => (None, false),
                };
                let Some(name) = name else { return };
                if name == fd.name || !yield_fns.contains(&name) {
                    return;
                }
                match called.iter_mut().find(|other| other.callee == name) {
                    Some(edge) => edge.only_tail = edge.only_tail && tail,
                    None => called.push(NestedCall {
                        callee: name,
                        line: e.line,
                        only_tail: tail,
                    }),
                }
            });
        }
        calls.insert(fd.name.clone(), called);
    }
    calls
}

/// Helpers before the processes that enter them. `Err` carries the names on
/// a cycle, in call order, with the first name repeated at the end.
fn lowering_order(calls: &NestedCalls) -> Result<Vec<String>, Vec<String>> {
    let mut order: Vec<String> = Vec::with_capacity(calls.len());
    let mut done: HashSet<String> = HashSet::new();
    for root in calls.keys() {
        let mut path: Vec<String> = Vec::new();
        visit(root, calls, &mut done, &mut path, &mut order)?;
    }
    Ok(order)
}

fn visit(
    name: &str,
    calls: &NestedCalls,
    done: &mut HashSet<String>,
    path: &mut Vec<String>,
    order: &mut Vec<String>,
) -> Result<(), Vec<String>> {
    if done.contains(name) {
        return Ok(());
    }
    if let Some(start) = path.iter().position(|entry| entry == name) {
        let mut cycle: Vec<String> = path[start..].to_vec();
        cycle.push(name.to_string());
        return Err(cycle);
    }
    path.push(name.to_string());
    if let Some(called) = calls.get(name) {
        for edge in called {
            visit(&edge.callee, calls, done, path, order)?;
        }
    }
    path.pop();
    done.insert(name.to_string());
    order.push(name.to_string());
    Ok(())
}

/// A cycle among the `yield` functions of one module. Nesting would put each
/// one's state inside the other's, which has no bottom; a cycle written with
/// tail calls only keeps none of those states, but each function's request
/// sum still carries the one it hands over to, so it has no bottom either —
/// and the message says which of the two shapes it found.
fn mutual_nesting_error(cycle: &[String], calls: &NestedCalls, stamped: &[TopLevel]) -> TypeError {
    let edge = |from: &String, to: &String| {
        calls
            .get(from)
            .and_then(|called| called.iter().find(|edge| &edge.callee == to))
    };
    let line = cycle
        .first()
        .zip(cycle.get(1))
        .and_then(|(from, to)| edge(from, to).map(|edge| edge.line))
        .or_else(|| {
            stamped.iter().find_map(|item| match item {
                TopLevel::FnDef(fd) if Some(&fd.name) == cycle.first() => Some(fd.line),
                _ => None,
            })
        })
        .unwrap_or(1);
    let only_tail = cycle
        .windows(2)
        .all(|pair| edge(&pair[0], &pair[1]).is_some_and(|edge| edge.only_tail));
    let message = if only_tail {
        format!(
            "A cycle of tail calls between yield functions is not supported by yield lowering: {} — a tail call hands over to the callee's protocol, so the caller's requests gain the callee's, and a cycle of them has no request sum to start from. Break the cycle: give one function a parameter saying which phase comes next and have it tail call itself",
            cycle.join(" calls ")
        )
    } else {
        format!(
            "Mutual nesting is not supported by yield lowering: {} — a nested state holds the callee's state inside the caller's, and a cycle has no innermost state to start from. Break the cycle: pass what comes next as data in one of them, or fold them into one function",
            cycle.join(" calls ")
        )
    };
    error_at(line, message)
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
            if let Some(name) = build::dotted_name(callee)
                && yield_fns.contains(&name)
            {
                report_call(fd, &name, tail, expr.line, errors);
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
        Expr::Ident(_) | Expr::Attr(_, _)
            if build::dotted_name(expr).is_some_and(|name| yield_fns.contains(&name)) =>
        {
            let name = build::dotted_name(expr).expect("matched a process name");
            errors.push(error_at(expr.line, format!(
                "Yield function '{name}' cannot be passed as a function value; call it directly inside another yield function, or drive '{}(...)' explicitly",
                qualified_start_name(&name)
            )));
        }
        _ => expr_walk::for_each_child(expr, &mut |child| {
            scan_expr(fd, child, false, yield_fns, errors)
        }),
    }
}

fn report_call(fd: &FnDef, callee: &str, tail: bool, line: usize, errors: &mut Vec<TypeError>) {
    let message = if !is_yield_fn(fd) {
        direct_call_recipe(&fd.name, callee)
    } else if callee != fd.name {
        // A process may split its work into `yield` helpers: a tail call
        // enters the helper's protocol, a non-tail call nests the helper's
        // state inside this one's. Only a cycle among them is refused, and
        // the lowering says so once, over the whole cycle.
        return;
    } else if !tail {
        format!(
            "Function '{}' calls itself outside tail position; pass what comes next as data, or make it a tail call. A process nests another yield function, not itself: its own state would have to hold a copy of itself",
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
                && !verify::supports(vb, name)
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
