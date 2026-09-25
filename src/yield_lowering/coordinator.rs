//! The generated loop (jasisz/aver#1329, process layer v2).
//!
//! An entry module that writes processes gets its loop written for it. What
//! the program writes is then only: the processes (its `yield` functions),
//! one `process ... seated by ...` line for each process that runs once per
//! key, the modules that answer the capabilities those processes wait on
//! (each says so with `answers [...]` in its header), and optionally a
//! `stop` and an `admit` policy of the entry, found by name. Everything
//! between them — the slot table, the request instance numbers, the seating
//! of keyed processes, the wait set, the poll timeout, the dispatch through
//! the answer modules, the stop observation, the turn and the loop itself —
//! is generated here, into the entry module, in the reserved `__`
//! namespace, by the same pass that generated the protocol.
//!
//! The generated items are ordinary Aver of the program: `aver verify` runs
//! them and every backend compiles them. The generated names stay callable,
//! so a program that wants to write its own loop over the protocol still
//! can, and the scenario fixtures state the loop's properties over them.

use std::collections::BTreeSet;

use crate::ast::{ProcessSeating, TopLevel, Type};
use crate::config::RunPlan;
use crate::types::checker::TypeError;

use super::{CoordinatorStop, FnSigs, ProcessProtocol};

mod host_driver;

/// The fields the generated run table writes itself. An answer module whose
/// name would take one of them is refused rather than generating a record
/// with the field declared twice.
const RUN_FIELDS: [&str; 8] = [
    "slots", "versions", "late", "dropped", "stopping", "now", "nextId", "failed",
];

/// The operation the end of a run performs on a job a parked request waits
/// on.
const CANCEL: &str = "Work.cancel";

/// The operation a process or an answer module ends the run with.
const FAIL: &str = "Run.fail";

/// The operation the loop reads the reason back with, once after it seats
/// and once after every turn.
const FAILURE: &str = "Run.failure";

/// What the generator resolved about one answer module.
struct Answer {
    /// The module as the program loads it, e.g. `Ledger`.
    module: String,
    /// The `__Run` field its state is held in, e.g. `ledger`.
    field: String,
    /// The state type, qualified to the module that declares it.
    state: String,
    /// The capabilities this module answers.
    capabilities: Vec<String>,
    /// The key its version lives under in `__Run.versions`.
    index: usize,
}

/// A process the loop seats: once, or once per key.
struct Proc<'a> {
    protocol: &'a ProcessProtocol,
    upper: String,
    keyed: Option<Keyed>,
}

/// What seats a keyed process: the pure function of an answer module that
/// lists the keys, and the key type.
struct Keyed {
    key: String,
    by: String,
    field: String,
}

/// The generated loop: its source, so `AVER_YIELD_DUMP` can show it, and the
/// items it parses to.
pub(super) struct GeneratedLoop {
    pub source: String,
    pub items: Vec<TopLevel>,
    /// What the loop performs, for the module's own `effects` boundary: the
    /// program declared the processes' effects, not the turn's.
    pub module_effects: Vec<String>,
}

/// Whether `module` is the one the loop may be generated into: the
/// program's entry. Every other module of the program is lowered without one.
pub(super) fn is_run_module(module: Option<&str>, plan: Option<&RunPlan>) -> bool {
    match (module, plan) {
        (Some(module), Some(plan)) => plan.entry == module,
        _ => false,
    }
}

fn error(line: usize, message: String) -> TypeError {
    TypeError {
        message,
        line,
        col: 1,
        origin: None,
        secondary: None,
    }
}

/// `peer` → `Peer`: the constructor that stands for one process.
fn marker_variant(fn_name: &str) -> String {
    super::build::capitalize(fn_name)
}

fn effects(list: &[String]) -> String {
    if list.is_empty() {
        String::new()
    } else {
        format!("    ! [{}]\n", list.join(", "))
    }
}

/// `Infra.Ledger` → `ledger`: the `__Run` field one answer module's state
/// lives in.
fn field_name(module: &str) -> String {
    let leaf = module.rsplit('.').next().unwrap_or(module);
    let mut chars = leaf.chars();
    match chars.next() {
        Some(head) => head.to_lowercase().collect::<String>() + chars.as_str(),
        None => leaf.to_string(),
    }
}

fn bare(name: &str) -> &str {
    name.rsplit_once('.').map(|(_, bare)| bare).unwrap_or(name)
}

fn owner(name: &str) -> &str {
    name.rsplit_once('.').map(|(owner, _)| owner).unwrap_or("")
}

/// How a wrong `fresh` reads back, so the diagnostic shows it rather than
/// describing it.
fn render_fresh(params: &[Type], state: &str, effects: &[String], module: &str) -> String {
    let rendered: Vec<String> = params
        .iter()
        .map(|param| crate::capability::canonicalize_type_names(param.clone(), module).display())
        .collect();
    let effects = if effects.is_empty() {
        String::new()
    } else {
        format!(" ! [{}]", effects.join(", "))
    };
    format!("fresh({}) -> {state}{effects}", rendered.join(", "))
}

/// The policies an entry may write, found by name.
struct Policies {
    stop: bool,
    admit: bool,
}

/// Generate the loop for `protocols` into the module `items` declares.
#[allow(clippy::too_many_arguments)]
pub(super) fn generate(
    items: &[TopLevel],
    generated: &[TopLevel],
    protocols: &[ProcessProtocol],
    seatings: &[ProcessSeating],
    plan: &RunPlan,
    fn_sigs: &FnSigs,
    coordinator_stop: CoordinatorStop,
) -> Result<GeneratedLoop, Vec<TypeError>> {
    let module = items.iter().find_map(|item| match item {
        TopLevel::Module(module) => Some(module),
        _ => None,
    });
    let line = module.map(|module| module.line).unwrap_or(1);
    let module_name = module.map(|module| module.name.as_str()).unwrap_or("");
    let mut errors = Vec::new();

    let answers = resolve_answers(plan, fn_sigs, line)?;
    let mut procs = Vec::new();
    for protocol in protocols {
        let seating = seatings
            .iter()
            .find(|seating| seating.process == protocol.fn_name);
        if protocol.return_type != "Unit" {
            errors.push(error(
                seating.map(|seating| seating.line).unwrap_or(line),
                format!(
                    "process '{}' is seated by the generated loop and its result goes nowhere; a process answers Unit, and it is '{}'",
                    protocol.fn_name, protocol.return_type
                ),
            ));
        }
        let keyed = match seating {
            Some(seating) => {
                match resolve_seating(seating, protocol, &answers, fn_sigs, module_name) {
                    Ok(keyed) => Some(keyed),
                    Err(found) => {
                        errors.push(found);
                        continue;
                    }
                }
            }
            None => {
                if !protocol.params.is_empty() {
                    errors.push(error(line, format!(
                        "the generated loop seats one '{0}' and has nothing to hand it; a process that takes a key is declared with the function that lists the keys, for example `process {0} seated by Sockets.peers`, and a process without one takes no parameters",
                        protocol.fn_name
                    )));
                    continue;
                }
                None
            }
        };
        procs.push(Proc {
            protocol,
            upper: marker_variant(&protocol.fn_name),
            keyed,
        });
    }
    let policies = check_policies(items, &mut errors);
    if !errors.is_empty() {
        return Err(errors);
    }

    let process_effects = process_effect_lists(&procs, generated, &answers, fn_sigs);
    let serve_effects = sorted(
        process_effects
            .iter()
            .flat_map(|process| process.serve.iter().cloned())
            .collect(),
    );
    let family_effects = sorted(
        procs
            .iter()
            .zip(&process_effects)
            .filter(|(proc, _)| proc.keyed.is_some())
            .flat_map(|(_, effects)| effects.seat.iter().cloned())
            .collect(),
    );
    // The loop reads a failure back only when something it runs can give
    // one: a program that never calls `Run.fail` from a process or an
    // answer module keeps the loop it had, effects and bytes alike.
    let fails = process_effects.iter().any(|process| {
        process
            .seat
            .iter()
            .chain(&process.serve)
            .any(|effect| effect == FAIL)
    });
    let turn_effects = turn_effect_list(&serve_effects, &family_effects, coordinator_stop, fails);
    let main_effects = main_effect_list(
        &turn_effects,
        &process_effects,
        !plan.job_kinds.is_empty(),
        fails,
    );
    let source = write_loop(
        &procs,
        &answers,
        &policies,
        &process_effects,
        &serve_effects,
        &family_effects,
        &turn_effects,
        &main_effects,
        coordinator_stop,
        !plan.job_kinds.is_empty(),
        fails,
    );
    match parse_generated(&source) {
        Ok(items) => Ok(GeneratedLoop {
            source,
            items,
            module_effects: main_effects,
        }),
        Err(parse) => Err(vec![error(
            line,
            format!(
                "internal error generating the loop of this program: {parse}; please report this program"
            ),
        )]),
    }
}

/// Parse what the generator wrote. The compiler-generated entry point is the
/// one that accepts the `__` namespace the loop lives in; a source file still
/// cannot write those names.
fn parse_generated(source: &str) -> Result<Vec<TopLevel>, String> {
    let tokens = crate::lexer::Lexer::new(source)
        .tokenize()
        .map_err(|error| error.to_string())?;
    crate::parser::Parser::new_compiler_generated(tokens)
        .parse()
        .map_err(|error| error.to_string())
}

/// One entry per answer module of the program: which state it threads, and
/// which field of the run holds it.
fn resolve_answers(
    plan: &RunPlan,
    fn_sigs: &FnSigs,
    line: usize,
) -> Result<Vec<Answer>, Vec<TypeError>> {
    let mut answers: Vec<Answer> = Vec::new();
    let mut errors = Vec::new();
    for (_, module) in &plan.answers {
        if answers.iter().any(|answer| &answer.module == module) {
            continue;
        }
        // The state is what `fresh` answers, not what some parameter happens
        // to be: the loop starts every answer module from `fresh`, so that
        // one function is where the state is named once and read once.
        let fresh = format!("{module}.fresh");
        let Some((params, result, effects)) = fn_sigs.get(&fresh) else {
            errors.push(error(line, format!(
                "the generated loop starts answer module '{module}' from '{fresh}'; this program has no such function. Add 'fn fresh() -> State' to '{module}', the state it holds before anything has happened"
            )));
            continue;
        };
        let state = crate::capability::canonicalize_type_names(result.clone(), module).display();
        if !params.is_empty() || !effects.is_empty() || !state.starts_with(&format!("{module}.")) {
            errors.push(error(line, format!(
                "the generated loop starts answer module '{module}' from '{fresh}', which takes nothing, is pure, and answers a type '{module}' declares; it is '{}'",
                render_fresh(params, &state, effects, module)
            )));
            continue;
        }
        let field = field_name(module);
        if RUN_FIELDS.contains(&field.as_str())
            || field.starts_with("seated")
            || field.starts_with("retired")
        {
            errors.push(error(line, format!(
                "the generated run table holds the state of answer module '{module}' in a field named '{field}', and the loop writes a field of its own by that name. Rename the module"
            )));
            continue;
        }
        if let Some(other) = answers.iter().find(|answer| answer.field == field) {
            errors.push(error(line, format!(
                "the generated run table holds the state of answer module '{module}' in a field named '{field}', and it already holds the state of module '{}' there. Rename one of them",
                other.module
            )));
            continue;
        }
        answers.push(Answer {
            module: module.clone(),
            field,
            state,
            capabilities: plan
                .answers
                .iter()
                .filter(|(_, owner)| owner == module)
                .map(|(capability, _)| capability.clone())
                .collect(),
            index: answers.len() + 1,
        });
    }
    if errors.is_empty() {
        Ok(answers)
    } else {
        Err(errors)
    }
}

/// `process peer seated by Sockets.peers`: the key the process takes, and
/// the pure function of the answer module that lists the keys.
fn resolve_seating(
    seating: &ProcessSeating,
    protocol: &ProcessProtocol,
    answers: &[Answer],
    fn_sigs: &FnSigs,
    entry: &str,
) -> Result<Keyed, TypeError> {
    let line = seating.line;
    let process = &protocol.fn_name;
    if protocol.params.len() != 1 {
        return Err(error(
            line,
            format!(
                "`process {process} seated by {}` seats one '{process}' per key, so '{process}' takes exactly one parameter, the key; it takes {}",
                seating.by,
                protocol.params.len()
            ),
        ));
    }
    let module = owner(&seating.by);
    let Some(answer) = answers.iter().find(|answer| answer.module == module) else {
        return Err(error(
            line,
            format!(
                "`process {process} seated by {}` names a function of module '{module}', which answers no capability of this program; the keys come from the state of an answer module, so name a function of a module whose header says `answers [...]`",
                seating.by
            ),
        ));
    };
    let Some((params, result, effects)) = fn_sigs.get(&seating.by) else {
        return Err(error(
            line,
            format!(
                "`process {process} seated by {}` names a function this program does not have",
                seating.by
            ),
        ));
    };
    let key = crate::capability::canonicalize_type_names(
        crate::types::parse_type_str(&protocol.params[0].1),
        entry,
    );
    let wanted = format!(
        "{}({}) -> List<{}>",
        bare(&seating.by),
        answer.state,
        key.display()
    );
    let param_ok = params.len() == 1
        && crate::capability::canonicalize_type_names(params[0].clone(), module).display()
            == answer.state;
    let listed = match crate::capability::canonicalize_type_names(result.clone(), module) {
        Type::List(inner) => Some(*inner),
        _ => None,
    };
    let key_ok = listed
        .as_ref()
        .is_some_and(|inner| inner.display() == key.display());
    if !param_ok || !key_ok {
        let rendered: Vec<String> = params
            .iter()
            .map(|param| {
                crate::capability::canonicalize_type_names(param.clone(), module).display()
            })
            .collect();
        return Err(error(
            line,
            format!(
                "`process {process} seated by {}` reads the keys to seat '{process}' with from the state of '{module}', so it must be '{wanted}'; it is '{}({}) -> {}'",
                seating.by,
                bare(&seating.by),
                rendered.join(", "),
                crate::capability::canonicalize_type_names(result.clone(), module).display()
            ),
        ));
    }
    if !effects.is_empty() {
        return Err(error(
            line,
            format!(
                "`process {process} seated by {}` names a function that declares effects [{}]; the loop reads the keys between waits, so it is pure",
                seating.by,
                effects.join(", ")
            ),
        ));
    }
    Ok(Keyed {
        key: protocol.params[0].1.clone(),
        by: seating.by.clone(),
        field: answer.field.clone(),
    })
}

/// `stop` and `admit` are the entry's own functions, found by name, and each
/// is held to the one shape the loop calls it with.
fn check_policies(items: &[TopLevel], errors: &mut Vec<TypeError>) -> Policies {
    let mut found = Policies {
        stop: false,
        admit: false,
    };
    for item in items {
        let TopLevel::FnDef(fd) = item else { continue };
        let (wanted_params, slot): (&[&str], &mut bool) = match fd.name.as_str() {
            "stop" => (&["Run.View"], &mut found.stop),
            "admit" => (&["Run.View", "Int"], &mut found.admit),
            _ => continue,
        };
        *slot = true;
        let params: Vec<&str> = fd.params.iter().map(|(_, ty)| ty.as_str()).collect();
        let wanted = match fd.name.as_str() {
            "stop" => "stop(view: Run.View) -> Bool",
            _ => "admit(view: Run.View, id: Int) -> Bool",
        };
        if params != wanted_params || fd.return_type != "Bool" {
            errors.push(error(fd.line, format!(
                "the generated loop calls '{}' of the entry module as its policy, so it must be '{wanted}'; it is '{}({}) -> {}'",
                fd.name,
                fd.name,
                fd.params
                    .iter()
                    .map(|(name, ty)| format!("{name}: {ty}"))
                    .collect::<Vec<_>>()
                    .join(", "),
                fd.return_type
            )));
        }
        if !fd.effects.is_empty() {
            errors.push(error(fd.line, format!(
                "the generated loop calls '{}' of the entry module as its policy, and it declares effects [{}]; a policy reads the view and answers, so it is pure",
                fd.name,
                fd.effects
                    .iter()
                    .map(|effect| effect.node.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            )));
        }
    }
    found
}

/// The whole generated loop, as Aver source.
///
/// Source rather than AST: every line of it is a line a reader of
/// `AVER_YIELD_DUMP=1` has to be able to read back. The parse that follows is
/// the one check that the generator wrote Aver rather than something that
/// looks like it.
#[allow(clippy::too_many_arguments)]
fn write_loop(
    procs: &[Proc<'_>],
    answers: &[Answer],
    policies: &Policies,
    process_effects: &[ProcessEffects],
    serve_effects: &[String],
    family_effects: &[String],
    turn_effects: &[String],
    main_effects: &[String],
    coordinator_stop: CoordinatorStop,
    cancels_waited: bool,
    fails: bool,
) -> String {
    let mut out = String::new();

    // ── What the policies read ─────────────────────────────────────
    out.push_str("type __Pending\n");
    for proc in procs {
        match &proc.keyed {
            Some(keyed) => out.push_str(&format!(
                "    {}({}, Int, Run.Wake)\n",
                proc.upper, keyed.key
            )),
            None => out.push_str(&format!("    {}(Int, Run.Wake)\n", proc.upper)),
        }
    }
    out.push_str("\nrecord __View\n    pending: Map<Int, __Pending>\n    ready: List<Int>\n    askable: List<Int>\n    dropped: Int\n    stopping: Bool\n");

    // ── The table ──────────────────────────────────────────────────
    out.push_str("\ntype __Process\n");
    for proc in procs {
        match &proc.keyed {
            Some(keyed) => out.push_str(&format!(
                "    {}({}, {})\n",
                proc.upper, keyed.key, proc.protocol.request
            )),
            None => out.push_str(&format!("    {}({})\n", proc.upper, proc.protocol.request)),
        }
    }
    out.push_str("\nrecord __Slot\n    seq: Int\n    pending: __Process\n    waiting: Run.Wake\n    due: Int\n    ms: Int\n    owner: Int\n    version: Int\n");
    out.push_str("\nrecord __Run\n    slots: Map<Int, __Slot>\n");
    for answer in answers {
        out.push_str(&format!("    {}: Option<{}>\n", answer.field, answer.state));
    }
    for proc in procs {
        if let Some(keyed) = &proc.keyed {
            out.push_str(&format!(
                "    seated{0}: Map<{1}, Int>\n    retired{0}: Map<{1}, Bool>\n",
                proc.upper, keyed.key
            ));
        }
    }
    out.push_str("    versions: Map<Int, Int>\n    late: Int\n    dropped: Int\n    stopping: Bool\n    now: Int\n    nextId: Int\n");
    if fails {
        out.push_str("    failed: Option<String>\n");
    }

    out.push_str("\nfn __fresh() -> __Run\n    ? \"The run before anything has happened: nothing seated, every answer module at its own empty state.\"\n    __Run(slots = {}");
    for answer in answers {
        out.push_str(&format!(
            ", {} = Option.Some({}.fresh())",
            answer.field, answer.module
        ));
    }
    for proc in procs {
        if proc.keyed.is_some() {
            out.push_str(&format!(
                ", seated{0} = {{}}, retired{0} = {{}}",
                proc.upper
            ));
        }
    }
    out.push_str(if fails {
        ", versions = {}, late = 0, dropped = 0, stopping = false, now = 0, nextId = 1, failed = Option.None)\n"
    } else {
        ", versions = {}, late = 0, dropped = 0, stopping = false, now = 0, nextId = 1)\n"
    });
    out.push_str("\nfn __asked(pending: __Process, seq: Int) -> __Slot\n    ? \"A slot whose request is to be asked in the next turn: a process just seated, or one whose last request was just answered.\"\n    __Slot(seq = seq, pending = pending, waiting = Run.Wake.Until([], Option.Some(0)), due = 0, ms = 0, owner = 0, version = 0)\n");

    // ── Handing a state out ────────────────────────────────────────
    for answer in answers {
        out.push_str(&format!(
            "\nfn __take{0}(run: __Run) -> Tuple<Option<{1}>, __Run>\n    ? \"Hands the state of '{2}' out of the run and leaves none behind, so the answer function it goes to holds the only reference to it and can update it in place. The state is read first and the run is updated at its last use, so the run it came from gives up its other fields instead of still holding them. The state comes back with the answer.\"\n    (run.{3}, __Run.update(run, {3} = Option.None))\n",
            super::build::capitalize(&answer.field),
            answer.state,
            answer.module,
            answer.field
        ));
    }

    // ── Seating ────────────────────────────────────────────────────
    for (proc, performs) in procs.iter().zip(process_effects) {
        write_seating(&mut out, proc, performs);
    }
    let unkeyed = procs
        .iter()
        .filter(|proc| proc.keyed.is_none())
        .fold("__fresh()".to_string(), |inner, proc| {
            format!("__seat{}({inner})", proc.upper)
        });
    let mut start_effects: BTreeSet<String> = procs
        .iter()
        .zip(process_effects)
        .flat_map(|(_, effects)| effects.seat.iter().cloned())
        .collect();
    let seated_at_start = if fails {
        start_effects.insert(FAILURE.to_string());
        format!("__failedAfter(__seatFamilies({unkeyed}))")
    } else {
        format!("__seatFamilies({unkeyed})")
    };
    out.push_str(&format!(
        "\nfn __start() -> __Run\n    ? \"Seats one of every process that takes no key, then every keyed process its answer module lists{}.\"\n{}    {seated_at_start}\n",
        if fails {
            ", and reads whether seating them already failed the run"
        } else {
            ""
        },
        effects(&sorted(start_effects))
    ));
    if fails {
        out.push_str(&format!(
            "\nfn __failedAfter(run: __Run) -> __Run\n    ? \"The run once a turn is over: the first reason a process or an answer module gave Run.fail, kept from the first turn that saw one.\"\n    ! [{FAILURE}]\n    match run.failed\n        Option.Some(_) -> run\n        Option.None -> __Run.update(run, failed = {FAILURE}())\n"
        ));
        out.push_str("\nfn __hasFailed(run: __Run) -> Bool\n    ? \"Whether some turn of this run called Run.fail.\"\n    match run.failed\n        Option.Some(_) -> true\n        Option.None -> false\n");
    }
    let families = procs
        .iter()
        .filter(|proc| proc.keyed.is_some())
        .fold("run".to_string(), |inner, proc| {
            format!("__seatFamily{0}({inner}, __keysOf{0}({inner}))", proc.upper)
        });
    let families = if procs.iter().filter(|proc| proc.keyed.is_some()).count() > 1 {
        // Each family reads the run the one before it left.
        let mut body = String::new();
        let mut run_of = "run".to_string();
        for (index, proc) in procs.iter().filter(|proc| proc.keyed.is_some()).enumerate() {
            let name = format!("seated{index}");
            body.push_str(&format!(
                "    {name} = __seatFamily{0}({run_of}, __keysOf{0}({run_of}))\n",
                proc.upper
            ));
            run_of = name;
        }
        body.push_str(&format!("    {run_of}\n"));
        body
    } else {
        format!("    {families}\n")
    };
    out.push_str(&format!(
        "\nfn __seatFamilies(run: __Run) -> __Run\n    ? \"The turn boundary: every keyed process is seated once per key its answer module lists, in list order, and an instance whose key has left that list is dropped.\"\n{}{families}",
        effects(family_effects)
    ));

    // ── The slot table and its two invariants ──────────────────────
    out.push_str("\nfn __current(run: __Run, id: Int) -> Int\n    ? \"The instance number of the request one process is waiting on, or -1 when nothing is seated under that id.\"\n    match Map.get(run.slots, id)\n        Option.None -> 0 - 1\n        Option.Some(slot) -> slot.seq\n");
    out.push_str("\nfn __nextInstance(seq: Int) -> Int\n    ? \"The instance number an answer for the current one leaves behind. It rises, so the instance just answered can never be current again.\"\n    seq + 1\n");
    out.push_str("\nfn __versionOf(run: __Run, owner: Int) -> Int\n    ? \"How many times this answer module has answered other than Settled. A request parked on Settled is asked again once this has moved past the number it was parked at.\"\n    match Map.get(run.versions, owner)\n        Option.None -> 0\n        Option.Some(version) -> version\n");
    out.push_str("\nfn __bump(run: __Run, owner: Int) -> __Run\n    ? \"One answer of this module that was not Settled: its state may have moved, so every request parked on Settled with it may be worth asking again. The version is read first, so the versions Map is handed to Map.set at the run's last use.\"\n    version = __versionOf(run, owner)\n    __Run.update(run, versions = Map.set(run.versions, owner, version + 1))\n");
    out.push_str("\nfn __deadlineOf(wake: Run.Wake) -> Option<Int>\n    ? \"The deadline half of a wake, if it has one.\"\n    match wake\n        Run.Wake.Until(_, deadline) -> deadline\n        Run.Wake.Settled(deadline) -> deadline\n");
    out.push_str("\nfn __dueOf(deadline: Option<Int>, now: Int) -> Int\n    ? \"The clock reading a deadline falls due at. A negative deadline is due now; no deadline carries none.\"\n    match deadline\n        Option.None -> 0\n        Option.Some(ms) -> now + Int.max(ms, 0)\n");
    out.push_str("\nfn __msOf(deadline: Option<Int>) -> Int\n    ? \"How long the request asked to be left alone for, never less than nothing.\"\n    match deadline\n        Option.None -> 0\n        Option.Some(ms) -> Int.max(ms, 0)\n");
    out.push_str("\nfn __parked(slot: __Slot, wake: Run.Wake, now: Int, owner: Int, version: Int) -> __Slot\n    ? \"The slot an Err leaves behind: the same instance and the same request, now remembering what would make asking again worth it. A deadline is turned into the clock reading it falls due at, and the ms that was asked for is kept beside it, so a clock that steps backwards cannot strand the request.\"\n    __Slot(seq = slot.seq, pending = slot.pending, waiting = wake, due = __dueOf(__deadlineOf(wake), now), ms = __msOf(__deadlineOf(wake)), owner = owner, version = version)\n");
    out.push_str("\nfn __park(run: __Run, id: Int, wake: Run.Wake, owner: Int) -> __Run\n    ? \"An Err: the request stays where it is with the same instance number. The state the answer module returned was already written back. An answer that is not Settled moves the module's version first; a Settled one does not, so a request cannot wake itself. The clock and the version are read first, so the slots Map is handed to Map.set at the last use of the run.\"\n    moved = __moved(run, wake, owner)\n    now = moved.now\n    version = __versionOf(moved, owner)\n    match Map.get(moved.slots, id)\n        Option.None -> moved\n        Option.Some(slot) -> __Run.update(moved, slots = Map.set(moved.slots, id, __parked(slot, wake, now, owner, version)))\n");
    out.push_str("\nfn __moved(run: __Run, wake: Run.Wake, owner: Int) -> __Run\n    ? \"The versions after one Err: Until moves its module's version, Settled leaves it.\"\n    match wake\n        Run.Wake.Until(_, _) -> __bump(run, owner)\n        Run.Wake.Settled(_) -> run\n");
    out.push_str("\nfn __staleInstance(run: __Run, id: Int, seq: Int) -> Bool\n    ? \"Why an answer carrying this instance number changes nothing: it is not the number the slot under this id is waiting on.\"\n    seq != __current(run, id)\n");

    for proc in procs {
        write_settle(&mut out, proc);
    }

    // ── The view ───────────────────────────────────────────────────
    out.push_str("\nfn __view(run: __Run, ready: List<Int>) -> __View\n    ? \"The resource-free summary of a run that stop and admit read. It is built again for every id the turn asks admit about, so a policy reads the seating this turn has already changed.\"\n    ids = Map.keys(run.slots)\n    __View(pending = __pendingOf(run, ids, {}), ready = ready, askable = __askableOf(run, ready, ids, []), dropped = run.dropped, stopping = run.stopping)\n");
    out.push_str("\nfn __pendingOf(run: __Run, ids: List<Int>, acc: Map<Int, __Pending>) -> Map<Int, __Pending>\n    ? \"One marker per seated process, in key order.\"\n    match ids\n        [] -> acc\n        [id, ..rest] -> __pendingOf(run, rest, __pendingAt(run, id, acc))\n");
    out.push_str("\nfn __pendingAt(run: __Run, id: Int, acc: Map<Int, __Pending>) -> Map<Int, __Pending>\n    ? \"The marker for one id, if that id is still seated.\"\n    match Map.get(run.slots, id)\n        Option.None -> acc\n        Option.Some(slot) -> Map.set(acc, id, __markerOf(slot))\n");
    out.push_str("\nfn __markerOf(slot: __Slot) -> __Pending\n    ? \"Which process one slot holds, its key when it has one, the instance it is waiting on, and what would wake it.\"\n    match slot.pending\n");
    for proc in procs {
        match &proc.keyed {
            Some(_) => out.push_str(&format!(
                "        __Process.{0}(key, _) -> __Pending.{0}(key, slot.seq, slot.waiting)\n",
                proc.upper
            )),
            None => out.push_str(&format!(
                "        __Process.{0}(_) -> __Pending.{0}(slot.seq, slot.waiting)\n",
                proc.upper
            )),
        }
    }
    out.push_str("\nfn __askableOf(run: __Run, ready: List<Int>, ids: List<Int>, acc: List<Int>) -> List<Int>\n    ? \"The ids this turn may ask, in slot order.\"\n    match ids\n        [] -> acc\n        [id, ..rest] -> __askableOf(run, ready, rest, __askableAt(run, ready, id, acc))\n");
    out.push_str("\nfn __askableAt(run: __Run, ready: List<Int>, id: Int, acc: List<Int>) -> List<Int>\n    ? \"One id, kept when its wake has fired.\"\n    match __askable(run, ready, id)\n        false -> acc\n        true -> List.concat(acc, [id])\n");

    // ── The gate: which slots this turn may ask ────────────────────
    out.push_str("\nfn __askable(run: __Run, ready: List<Int>, id: Int) -> Bool\n    ? \"Whether the turn may ask the request seated under this id. Nothing is seated there, nothing to ask.\"\n    match Map.get(run.slots, id)\n        Option.None -> false\n        Option.Some(slot) -> __askableSlot(run, slot, ready, id)\n");
    out.push_str("\nfn __deadlinePassed(deadline: Option<Int>, due: Int, ms: Int, now: Int) -> Bool\n    ? \"Whether the deadline half of a wake has fired: the turn's clock reading has reached the moment it falls due at, or has fallen further back than the deadline asked for, because a wall clock that steps backwards would otherwise leave the request waiting for a reading that never comes. No deadline never fires.\"\n    match deadline\n        Option.None -> false\n        Option.Some(_) -> Bool.or(due <= now, now < due - ms)\n");
    out.push_str("\nfn __askableSlot(run: __Run, slot: __Slot, ready: List<Int>, id: Int) -> Bool\n    ? \"What one wake gates. Until is asked once the wait reported one of its items or its deadline passed; false readiness is allowed, and the module may answer Err again. Settled is asked once its module has answered anything other than Settled since, or its deadline passed.\"\n    match slot.waiting\n        Run.Wake.Until(_, deadline) -> Bool.or(List.contains(ready, id), __deadlinePassed(deadline, slot.due, slot.ms, run.now))\n        Run.Wake.Settled(deadline) -> Bool.or(__versionOf(run, slot.owner) > slot.version, __deadlinePassed(deadline, slot.due, slot.ms, run.now))\n");

    // ── The wait and the timeout ───────────────────────────────────
    out.push_str("\nrecord __WaitPlan\n    items: Map<Int, Wait.Item>\n    owners: Map<Int, Int>\n    next: Int\n");
    out.push_str("\nfn __waitPlan(run: __Run) -> __WaitPlan\n    ? \"The one wait of a turn: every item every parked request waits on, each under a key of its own, and which slot each key belongs to.\"\n    __waitPlanOf(run, Map.keys(run.slots), __WaitPlan(items = {}, owners = {}, next = 0))\n");
    out.push_str("\nfn __waitPlanOf(run: __Run, ids: List<Int>, acc: __WaitPlan) -> __WaitPlan\n    ? \"Every seated slot's items, in slot order.\"\n    match ids\n        [] -> acc\n        [id, ..rest] -> __waitPlanOf(run, rest, __waitPlanAt(run, id, acc))\n");
    out.push_str("\nfn __waitPlanAt(run: __Run, id: Int, acc: __WaitPlan) -> __WaitPlan\n    ? \"What the request seated under one id waits on, if anything is seated there. Settled waits on no item: its module's version is the loop's own to watch.\"\n    match Map.get(run.slots, id)\n        Option.None -> acc\n        Option.Some(slot) -> match slot.waiting\n            Run.Wake.Until(items, _) -> __waitItems(acc, id, items)\n            Run.Wake.Settled(_) -> acc\n");
    out.push_str("\nfn __waitItems(acc: __WaitPlan, id: Int, items: List<Wait.Item>) -> __WaitPlan\n    ? \"One key per item, all owned by this slot.\"\n    match items\n        [] -> acc\n        [item, ..rest] -> __waitItems(__WaitPlan(items = Map.set(acc.items, acc.next, item), owners = Map.set(acc.owners, acc.next, id), next = acc.next + 1), id, rest)\n");
    out.push_str("\nfn __readySlots(owners: Map<Int, Int>, keys: List<Int>, acc: List<Int>) -> List<Int>\n    ? \"The slots the keys the wait reported belong to.\"\n    match keys\n        [] -> acc\n        [key, ..rest] -> __readySlots(owners, rest, __readySlot(owners, key, acc))\n");
    out.push_str("\nfn __readySlot(owners: Map<Int, Int>, key: Int, acc: List<Int>) -> List<Int>\n    ? \"One reported key, as the slot that owns it.\"\n    match Map.get(owners, key)\n        Option.None -> acc\n        Option.Some(id) -> List.concat(acc, [id])\n");
    out.push_str("\nfn __timeout(run: __Run) -> Int\n    ? \"How long this turn may wait: nothing at all while some request can be asked already, the soonest deadline still ahead of the turn's clock reading when one is pending, and one second when neither.\"\n    __deadline(run, Map.keys(run.slots), 0 - 1)\n");
    out.push_str("\nfn __deadline(run: __Run, ids: List<Int>, best: Int) -> Int\n    ? \"The soonest deadline across every seated slot, or -1 when none carries one.\"\n    match ids\n        [] -> __tick(best)\n        [id, ..rest] -> __deadline(run, rest, __deadlineAt(run, id, best))\n");
    out.push_str("\nfn __deadlineAt(run: __Run, id: Int, best: Int) -> Int\n    ? \"What one id contributes to the turn's wait. A request parked on Settled whose module has moved since is askable now, so the turn does not wait at all.\"\n    match Map.get(run.slots, id)\n        Option.None -> best\n        Option.Some(slot) -> match __settledMoved(run, slot)\n            true -> 0\n            false -> __soonest(best, slot, run.now)\n");
    out.push_str("\nfn __settledMoved(run: __Run, slot: __Slot) -> Bool\n    ? \"A request parked on Settled whose module has answered since.\"\n    match slot.waiting\n        Run.Wake.Settled(_) -> __versionOf(run, slot.owner) > slot.version\n        Run.Wake.Until(_, _) -> false\n");
    out.push_str("\nfn __soonest(best: Int, slot: __Slot, now: Int) -> Int\n    ? \"Two deadlines keep the nearer; a wake without a deadline carries none.\"\n    match __deadlineOf(slot.waiting)\n        Option.None -> best\n        Option.Some(_) -> __nearer(best, __remaining(slot.due, now, slot.ms))\n");
    out.push_str("\nfn __remaining(due: Int, now: Int, ms: Int) -> Int\n    ? \"How much of one deadline is left: never less than nothing, and never more than the ms that was asked for, because a clock that stepped backwards would otherwise put the turn to sleep for longer than any request in the program asked for.\"\n    Int.min(Int.max(due - now, 0), ms)\n");
    out.push_str("\nfn __nearer(best: Int, ms: Int) -> Int\n    ? \"The nearer of two deadlines, where -1 means none yet.\"\n    match best < 0\n        true -> ms\n        false -> Int.min(best, ms)\n");
    out.push_str("\nfn __tick(best: Int) -> Int\n    ? \"The wait of a turn with no deadline in it: one second.\"\n    match best < 0\n        true -> 1000\n        false -> best\n");

    // ── The serve path ─────────────────────────────────────────────
    out.push_str(&format!(
        "\nfn __serve(run: __Run, id: Int) -> __Run\n    ? \"Serves one slot: asks the module that answers its request, then either replaces the slot with what the process does next, or leaves it parked.\"\n{}    match Map.get(run.slots, id)\n        Option.None -> run\n        Option.Some(slot) -> match slot.pending\n",
        effects(serve_effects)
    ));
    for proc in procs {
        match &proc.keyed {
            Some(_) => out.push_str(&format!(
                "            __Process.{0}(key, request) -> __serve{0}(run, id, slot.seq, key, request)\n",
                proc.upper
            )),
            None => out.push_str(&format!(
                "            __Process.{0}(request) -> __serve{0}(run, id, slot.seq, request)\n",
                proc.upper
            )),
        }
    }
    for (proc, performs) in procs.iter().zip(process_effects) {
        out.push_str(&write_serve(proc, answers, performs));
    }

    let stop_observation = match coordinator_stop {
        CoordinatorStop::HostSignal => "Process.stopRequested()",
        CoordinatorStop::PolicyOnly => "false",
    };
    out.push_str(&host_driver::write_step(turn_effects, fails));
    out.push_str(&format!(
        "\nfn __turn(run: __Run) -> Result<__Run, String>\n    ? \"Observe stopping, wait once, then perform the same turn as the external host driver.\"\n{}    observed = __Run.update(run, stopping = {stop_observation})\n    plan = __waitPlan(observed)\n    keys = Wait.poll(plan.items, __timeout(observed))?\n    Result.Ok(__workHostStep(observed, keys))\n",
        effects(turn_effects),
    ));
    out.push_str(&format!(
        "\nfn __serveEach(run: __Run, ready: List<Int>, ids: List<Int>) -> __Run\n    ? \"Every seated id, in slot order, once, if its wake has fired{}.\"\n{}    match ids\n        [] -> run\n        [id, ..rest] -> __serveEach(__serveIf(run, ready, id), ready, rest)\n",
        if policies.admit { " and admit lets it" } else { "" },
        effects(serve_effects)
    ));
    let admitted = if policies.admit {
        "match admit(__view(run, ready), id)\n            false -> run\n            true -> __serve(run, id)"
    } else {
        "__serve(run, id)"
    };
    out.push_str(&format!(
        "\nfn __serveIf(run: __Run, ready: List<Int>, id: Int) -> __Run\n    ? \"One id, asked only when its wake has fired. A slot that is not askable is not asked{}.\"\n{}    match __askable(run, ready, id)\n        false -> run\n        true -> {admitted}\n",
        if policies.admit { ", and one that is is asked only if admit lets it" } else { "" },
        effects(serve_effects)
    ));
    let stopping = if policies.stop {
        "Bool.or(stop(__view(run, [])), Map.len(run.slots) == 0)"
    } else {
        "Bool.or(run.stopping, Map.len(run.slots) == 0)"
    };
    let stopping = if fails {
        format!("Bool.or(__hasFailed(run), {stopping})")
    } else {
        stopping.to_string()
    };
    out.push_str(&format!(
        "\nfn __stopped(run: __Run) -> Bool\n    ? \"The run is over once nothing is seated{}{}.\"\n    {stopping}\n",
        if policies.stop { ", or once the entry's stop says so" } else { ", or once a stop was requested" },
        if fails { ", or once a turn called Run.fail" } else { "" }
    ));
    out.push_str(&format!(
        "\nfn __runAll(run: __Run) -> Result<__Run, String>\n    ? \"Turns until the run is over.\"\n{}    match __stopped(run)\n        true -> Result.Ok(run)\n        false -> __runAll(__turn(run)?)\n",
        effects(turn_effects)
    ));
    out.push_str(&host_driver::write_exports(
        main_effects,
        cancels_waited,
        coordinator_stop,
    ));
    out.push_str(&format!(
        "\nfn __all() -> Result<Unit, String>\n    ? \"Seats every process, turns until the run is over, and ends it.\"\n{}    __over(__runAll(__start())?)\n",
        effects(main_effects)
    ));

    // ── The end of a run ───────────────────────────────────────────
    // A request parked on a job its answer module began itself holds a
    // handle the loop never put anywhere else. It is over with the run, so
    // its job is cancelled rather than left running.
    if cancels_waited {
        out.push_str(&format!(
            "\nfn __cancelWaited(run: __Run, ids: List<Int>) -> __Run\n    ? \"Every job a parked request is waiting on, cancelled, in slot order.\"\n    ! [{CANCEL}]\n    match ids\n        [] -> run\n        [id, ..rest] -> __cancelWaited(__cancelWaitedAt(run, id), rest)\n"
        ));
        out.push_str(&format!(
            "\nfn __cancelWaitedAt(run: __Run, id: Int) -> __Run\n    ? \"The jobs the request seated under one id waits on, if it waits on any.\"\n    ! [{CANCEL}]\n    match Map.get(run.slots, id)\n        Option.None -> run\n        Option.Some(slot) -> match slot.waiting\n            Run.Wake.Until(items, _) -> __cancelItems(run, items)\n            Run.Wake.Settled(_) -> run\n"
        ));
        out.push_str(&format!(
            "\nfn __cancelItems(run: __Run, items: List<Wait.Item>) -> __Run\n    ? \"A job is cancelled; a socket belongs to its answer module and is left alone.\"\n    ! [{CANCEL}]\n    match items\n        [] -> run\n        [item, ..rest] -> __cancelItems(__cancelItem(run, item), rest)\n"
        ));
        out.push_str(&format!(
            "\nfn __cancelItem(run: __Run, item: Wait.Item) -> __Run\n    ? \"One item of a parked request.\"\n    ! [{CANCEL}]\n    match item\n        Wait.Item.Job(job) -> __cancelledWaited(run, {CANCEL}(job))\n        Wait.Item.Socket(_) -> run\n"
        ));
        out.push_str("\nfn __cancelledWaited(run: __Run, cancelled: Unit) -> __Run\n    ? \"The run once one waited-on job has been cancelled; the slot itself stays as it was.\"\n    run\n");
        out.push_str(&format!(
            "\nfn __over(run: __Run) -> Result<Unit, String>\n    ? \"The run is over: whatever is still seated stays where it is, and every job a parked request waits on is cancelled rather than abandoned{}.\"\n    ! [{CANCEL}]\n    _parked = __cancelWaited(run, Map.keys(run.slots))\n    {}\n",
            if fails { ". A run some turn failed answers the reason it was given" } else { "" },
            if fails { "__outcome(run)" } else { "Result.Ok(Unit)" }
        ));
    } else {
        out.push_str(&format!(
            "\nfn __over(run: __Run) -> Result<Unit, String>\n    ? \"The run is over: whatever is still seated stays where it is{}.\"\n    {}\n",
            if fails { ". A run some turn failed answers the reason it was given" } else { "" },
            if fails { "__outcome(run)" } else { "Result.Ok(Unit)" }
        ));
    }
    if fails {
        out.push_str("\nfn __outcome(run: __Run) -> Result<Unit, String>\n    ? \"What Run.all() answers: the reason the first Run.fail gave, or Unit when nothing failed.\"\n    match run.failed\n        Option.Some(reason) -> Result.Err(reason)\n        Option.None -> Result.Ok(Unit)\n");
    }
    out
}

/// Seating one process: once for a process without a key, once per key for
/// a keyed one.
fn write_seating(out: &mut String, proc: &Proc<'_>, performs: &ProcessEffects) {
    let upper = &proc.upper;
    let protocol = proc.protocol;
    let seat = effects(&performs.seat);
    let Some(keyed) = &proc.keyed else {
        out.push_str(&format!(
            "\nfn __seat{upper}(run: __Run) -> __Run\n    ? \"Seats the '{}' process under its own slot id, at its first request.\"\n{seat}    __seated{upper}(run, {}())\n",
            protocol.fn_name, protocol.start
        ));
        out.push_str(&format!(
            "\nfn __seated{upper}(run: __Run, outcome: {0}) -> __Run\n    ? \"A process that is already done is not seated; one that is waiting takes the next free slot id.\"\n    match outcome\n        {0}.Done(_) -> run\n        {0}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, run.nextId, __asked(__Process.{upper}(request), 1)), nextId = run.nextId + 1)\n",
            protocol.outcome
        ));
        return;
    };
    let key = &keyed.key;
    out.push_str(&format!(
        "\nfn __keysOf{upper}(run: __Run) -> List<{key}>\n    ? \"The keys '{0}' lists for the '{1}' family. Its state is in the run at every turn boundary; it is out only while one answer function holds it.\"\n    match run.{2}\n        Option.Some(state) -> {0}(state)\n        Option.None -> []\n",
        keyed.by, protocol.fn_name, keyed.field
    ));
    out.push_str(&format!(
        "\nfn __seatFamily{upper}(run: __Run, keys: List<{key}>) -> __Run\n    ? \"The '{0}' family at the turn boundary: instances whose key has left the list are dropped, retired keys that have left it may come back later, and every listed key that is neither seated nor retired is seated, in list order. The retired keys are listed first, so the retired Map is handed on at the last use of the run.\"\n{seat}    present = __keySet{upper}(keys, {{}})\n    kept = __dropLeft{upper}(run, Map.keys(run.seated{upper}), present)\n    retiredKeys = Map.keys(kept.retired{upper})\n    back = __Run.update(kept, retired{upper} = __unretire{upper}(kept.retired{upper}, retiredKeys, present))\n    __seatKeys{upper}(back, keys)\n",
        protocol.fn_name
    ));
    out.push_str(&format!(
        "\nfn __keySet{upper}(keys: List<{key}>, acc: Map<{key}, Bool>) -> Map<{key}, Bool>\n    ? \"The listed keys, as a set.\"\n    match keys\n        [] -> acc\n        [key, ..rest] -> __keySet{upper}(rest, Map.set(acc, key, true))\n"
    ));
    out.push_str(&format!(
        "\nfn __dropLeft{upper}(run: __Run, seated: List<{key}>, present: Map<{key}, Bool>) -> __Run\n    ? \"Every seated instance whose key is no longer listed is dropped: its slot goes, and the run counts it.\"\n    match seated\n        [] -> run\n        [key, ..rest] -> __dropLeft{upper}(__dropIfLeft{upper}(run, key, Map.has(present, key)), rest, present)\n"
    ));
    out.push_str(&format!(
        "\nfn __dropIfLeft{upper}(run: __Run, key: {key}, listed: Bool) -> __Run\n    ? \"One seated key: kept while it is listed, dropped once it is not.\"\n    match listed\n        true -> run\n        false -> __drop{upper}(run, key)\n"
    ));
    out.push_str(&format!(
        "\nfn __drop{upper}(run: __Run, key: {key}) -> __Run\n    ? \"The instance seated for this key is gone: its slot and its seat are removed, and the run counts one more dropped instance.\"\n    match Map.get(run.seated{upper}, key)\n        Option.None -> run\n        Option.Some(id) -> __Run.update(run, slots = Map.remove(run.slots, id), seated{upper} = Map.remove(run.seated{upper}, key), dropped = run.dropped + 1)\n"
    ));
    out.push_str(&format!(
        "\nfn __unretire{upper}(retired: Map<{key}, Bool>, keys: List<{key}>, present: Map<{key}, Bool>) -> Map<{key}, Bool>\n    ? \"A retired key stays retired while it is listed, and may be seated again once it has been absent.\"\n    match keys\n        [] -> retired\n        [key, ..rest] -> __unretire{upper}(__unretireIf{upper}(retired, key, Map.has(present, key)), rest, present)\n"
    ));
    out.push_str(&format!(
        "\nfn __unretireIf{upper}(retired: Map<{key}, Bool>, key: {key}, listed: Bool) -> Map<{key}, Bool>\n    ? \"One retired key.\"\n    match listed\n        true -> retired\n        false -> Map.remove(retired, key)\n"
    ));
    out.push_str(&format!(
        "\nfn __seatKeys{upper}(run: __Run, keys: List<{key}>) -> __Run\n    ? \"Every listed key that is neither seated nor retired gets an instance, in list order.\"\n{seat}    match keys\n        [] -> run\n        [key, ..rest] -> __seatKeys{upper}(__seatKey{upper}(run, key), rest)\n"
    ));
    out.push_str(&format!(
        "\nfn __seatKey{upper}(run: __Run, key: {key}) -> __Run\n    ? \"One listed key.\"\n{seat}    match Bool.or(Map.has(run.seated{upper}, key), Map.has(run.retired{upper}, key))\n        true -> run\n        false -> __seated{upper}(run, key, {}(key))\n",
        protocol.start
    ));
    out.push_str(&format!(
        "\nfn __seated{upper}(run: __Run, key: {key}, outcome: {0}) -> __Run\n    ? \"An instance that is already done retires its key; one that is waiting takes the next free slot id.\"\n    match outcome\n        {0}.Done(_) -> __Run.update(run, retired{upper} = Map.set(run.retired{upper}, key, true))\n        {0}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, run.nextId, __asked(__Process.{upper}(key, request), 1)), seated{upper} = Map.set(run.seated{upper}, key, run.nextId), nextId = run.nextId + 1)\n",
        protocol.outcome
    ));
}

/// Settling one process's request: the instance check, and what the answer
/// leaves behind.
fn write_settle(out: &mut String, proc: &Proc<'_>) {
    let upper = &proc.upper;
    let protocol = proc.protocol;
    let (key_param, key_arg, key_in) = match &proc.keyed {
        Some(keyed) => (format!(", key: {}", keyed.key), ", key", "key, "),
        None => (String::new(), "", ""),
    };
    out.push_str(&format!(
        "\nfn __settle{upper}(run: __Run, id: Int, seq: Int{key_param}, outcome: {0}) -> __Run\n    ? \"An answer for the current instance replaces that process's one slot and raises its number; an answer for an older instance changes nothing and is counted.\"\n    match seq == __current(run, id)\n        false -> __Run.update(run, late = run.late + 1)\n        true -> __settled{upper}At(run, id, seq{key_arg}, outcome)\n",
        protocol.outcome
    ));
    let done = match &proc.keyed {
        Some(_) => format!(
            "__Run.update(run, slots = Map.remove(run.slots, id), seated{upper} = Map.remove(run.seated{upper}, key), retired{upper} = Map.set(run.retired{upper}, key, true))"
        ),
        None => "__Run.update(run, slots = Map.remove(run.slots, id))".to_string(),
    };
    out.push_str(&format!(
        "\nfn __settled{upper}At(run: __Run, id: Int, seq: Int{key_param}, outcome: {0}) -> __Run\n    ? \"What an answer for the current instance leaves behind: an empty slot when the process is done{1}, the next request under the next instance number otherwise.\"\n    match outcome\n        {0}.Done(_) -> {done}\n        {0}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, id, __asked(__Process.{upper}({key_in}request), __nextInstance(seq))))\n",
        protocol.outcome,
        if proc.keyed.is_some() { ", with its key retired" } else { "" }
    ));
}

/// The dispatch of one process: one arm per request kind, and one function
/// per kind that reads the reply the answer module gave.
fn write_serve(proc: &Proc<'_>, answers: &[Answer], performs: &ProcessEffects) -> String {
    let upper = &proc.upper;
    let protocol = proc.protocol;
    let (key_param, key_arg) = match &proc.keyed {
        Some(keyed) => (format!(", key: {}", keyed.key), ", key"),
        None => (String::new(), ""),
    };
    let mut out = format!(
        "\nfn __serve{upper}(run: __Run, id: Int, seq: Int{key_param}, request: {}) -> __Run\n    ? \"One arm per request kind of '{}'.\"\n{}    match request\n",
        protocol.request,
        protocol.fn_name,
        effects(&performs.serve)
    );
    let mut bodies = String::new();
    for (kind, resumes) in protocol.kinds.iter().zip(&performs.kinds) {
        let Some(operation) = &kind.operation else {
            out.push_str(&format!(
                "        {}.{}(state) -> __settle{upper}(run, id, seq{key_arg}, {}(state))\n",
                protocol.request, kind.name, kind.answer_fn
            ));
            continue;
        };
        let (capability, op) = operation
            .rsplit_once('.')
            .expect("a marked operation is dotted");
        let answer =
            answering_module(answers, capability).expect("a marked capability is answered");
        let binders: Vec<String> = (0..kind.arg_types.len())
            .map(|index| format!("__a{index}"))
            .collect();
        // The state is handed out of the run before the answer function sees
        // it, so the run does not share it while the answer updates it.
        let mut call_args = vec!["__taken".to_string()];
        call_args.extend(binders.iter().cloned());
        let mut pattern = binders.clone();
        pattern.push("state".to_string());
        out.push_str(&format!(
            "        {}.{}({}) -> match __take{}(run)\n            (__held, __rest) -> match __held\n                Option.Some(__taken) -> __serve{upper}{}(__rest, id, seq{key_arg}, state, {}.{op}({}))\n                Option.None -> __rest\n",
            protocol.request,
            kind.name,
            pattern.join(", "),
            super::build::capitalize(&answer.field),
            kind.name,
            answer.module,
            call_args.join(", ")
        ));
        let result = kind.answer_type.as_deref().unwrap_or("Unit");
        let (binder, resume) = match kind.answer_type.as_deref() {
            Some("Unit") | None => ("_", format!("{}(state)", kind.answer_fn)),
            Some(_) => ("__answer", format!("{}(state, __answer)", kind.answer_fn)),
        };
        bodies.push_str(&format!(
            "\nfn __serve{upper}{kind_name}(run: __Run, id: Int, seq: Int{key_param}, state: {state}, answered: Tuple<{module_state}, Result<{result}, Run.Wake>>) -> __Run\n    ? \"An Ok answers the request with the answer function of this kind; an Err keeps the state the module returned and parks the request on the wake it named.\"\n{resumes}    match answered\n        (__next, __reply) -> match __reply\n            Result.Ok({binder}) -> __settle{upper}(__bump(__Run.update(run, {field} = Option.Some(__next)), {index}), id, seq{key_arg}, {resume})\n            Result.Err(__wake) -> __park(__Run.update(run, {field} = Option.Some(__next)), id, __wake, {index})\n",
            kind_name = kind.name,
            state = kind.state,
            module_state = answer.state,
            resumes = effects(resumes),
            field = answer.field,
            index = answer.index,
        ));
    }
    out.push_str(&bodies);
    out
}

/// The module answering one capability, matched the way an effect entry is:
/// a header names the capability as the program writes it in `depends`.
fn answering_module<'a>(answers: &'a [Answer], capability: &str) -> Option<&'a Answer> {
    answers.iter().find(|answer| {
        answer
            .capabilities
            .iter()
            .any(|name| capability == name || capability.ends_with(&format!(".{name}")))
    })
}

/// What the generated functions of one process perform, each one carrying its
/// own path rather than the program's.
///
/// Seating a process performs what that process performs on its way to its
/// first request, and serving one request kind performs what the answer
/// module performs plus what the segment the answer resumes performs. A
/// process that touches nothing keeps every function the loop generates for
/// it pure, however loud its neighbour is.
struct ProcessEffects {
    /// What seating performs: the start function's own segment.
    seat: Vec<String>,
    /// What `__serve<P>` performs: every kind of this process.
    serve: Vec<String>,
    /// What `__serve<P><Kind>` performs, aligned with `protocol.kinds`.
    kinds: Vec<Vec<String>>,
}

fn sorted(found: BTreeSet<String>) -> Vec<String> {
    found.into_iter().collect()
}

/// One entry per process, in the order the processes are given.
fn process_effect_lists(
    procs: &[Proc<'_>],
    generated: &[TopLevel],
    answers: &[Answer],
    fn_sigs: &FnSigs,
) -> Vec<ProcessEffects> {
    let declared = |name: &str| -> Vec<String> {
        generated
            .iter()
            .find_map(|item| match item {
                TopLevel::FnDef(fd) if fd.name == name => {
                    Some(fd.effects.iter().map(|e| e.node.clone()).collect())
                }
                _ => None,
            })
            .unwrap_or_default()
    };
    let answered = |kind: &super::ProtocolKind| -> Vec<String> {
        let Some(operation) = &kind.operation else {
            return Vec::new();
        };
        let Some((capability, op)) = operation.rsplit_once('.') else {
            return Vec::new();
        };
        let Some(answer) = answering_module(answers, capability) else {
            return Vec::new();
        };
        fn_sigs
            .get(&format!("{}.{op}", answer.module))
            .map(|(_, _, effects)| effects.clone())
            .unwrap_or_default()
    };
    procs
        .iter()
        .map(|proc| {
            let protocol = proc.protocol;
            let kinds: Vec<Vec<String>> = protocol
                .kinds
                .iter()
                .map(|kind| declared(&kind.answer_fn))
                .collect();
            let mut serve: BTreeSet<String> = BTreeSet::new();
            for (kind, resumed) in protocol.kinds.iter().zip(&kinds) {
                serve.extend(resumed.iter().cloned());
                serve.extend(answered(kind));
            }
            ProcessEffects {
                seat: declared(&protocol.start),
                serve: sorted(serve),
                kinds,
            }
        })
        .collect()
}

/// What a whole turn performs: the stop observation, the clock reading the
/// wake gate is measured against, the one wait, the serve path, and the
/// seating of keyed processes at the turn boundary.
fn turn_effect_list(
    serve: &[String],
    families: &[String],
    coordinator_stop: CoordinatorStop,
    fails: bool,
) -> Vec<String> {
    let mut found: BTreeSet<String> = BTreeSet::new();
    if coordinator_stop == CoordinatorStop::HostSignal {
        found.insert("Process.stopRequested".to_string());
    }
    found.insert("Time.unixMs".to_string());
    found.insert("Wait.poll".to_string());
    found.extend(serve.iter().cloned());
    found.extend(families.iter().cloned());
    if fails {
        found.insert(FAILURE.to_string());
    }
    found.into_iter().collect()
}

/// What the loop's entry performs: it seats every process, turns, and
/// cancels whatever job a parked request still waits on when the run is
/// over.
fn main_effect_list(
    turn: &[String],
    effects: &[ProcessEffects],
    cancels: bool,
    fails: bool,
) -> Vec<String> {
    let mut found: BTreeSet<String> = turn.iter().cloned().collect();
    if fails {
        found.insert(FAILURE.to_string());
    }
    for process in effects {
        found.extend(process.seat.iter().cloned());
    }
    if cancels {
        found.insert(CANCEL.to_string());
    }
    found.into_iter().collect()
}

/// Rewrite what the entry module writes in the loop's vocabulary into the
/// generated names: `Run.View` and `Run.Pending` are the generated view and
/// marker sum, and `Run.all()` runs the generated loop. The names are the
/// program's way to reach generated items it cannot spell.
pub(super) fn rewrite_run_names(items: &mut [TopLevel]) {
    use crate::ast::{Expr, Pattern, Spanned, Stmt, TypeDef};
    fn ty(annotation: &mut String) {
        for (from, to) in [("Run.View", "__View"), ("Run.Pending", "__Pending")] {
            let mut out = String::new();
            let mut rest = annotation.as_str();
            while let Some(at) = rest.find(from) {
                let before_ok = out.is_empty() && at == 0
                    || rest[..at]
                        .chars()
                        .last()
                        .or_else(|| out.chars().last())
                        .is_none_or(|c| !c.is_alphanumeric() && c != '_' && c != '.');
                let after = &rest[at + from.len()..];
                let after_ok = after
                    .chars()
                    .next()
                    .is_none_or(|c| !c.is_alphanumeric() && c != '_');
                out.push_str(&rest[..at]);
                if before_ok && after_ok {
                    out.push_str(to);
                } else {
                    out.push_str(from);
                }
                rest = after;
            }
            out.push_str(rest);
            *annotation = out;
        }
    }
    fn pattern(pat: &mut Pattern) {
        match pat {
            Pattern::Constructor(name, _) => {
                if let Some(rest) = name.strip_prefix("Run.Pending.") {
                    *name = format!("__Pending.{rest}");
                }
            }
            Pattern::Tuple(items) => items.iter_mut().for_each(pattern),
            _ => {}
        }
    }
    fn expr(e: &mut Spanned<Expr>) {
        match &mut e.node {
            Expr::Attr(inner, field) => {
                if let Expr::Ident(name) = &inner.node
                    && name == "Run"
                {
                    match field.as_str() {
                        "Pending" => {
                            e.node = Expr::Ident("__Pending".to_string());
                            return;
                        }
                        "View" => {
                            e.node = Expr::Ident("__View".to_string());
                            return;
                        }
                        "all" => {
                            e.node = Expr::Ident("__all".to_string());
                            return;
                        }
                        _ => {}
                    }
                }
            }
            Expr::Constructor(name, _) => {
                if let Some(rest) = name.strip_prefix("Run.Pending.") {
                    *name = format!("__Pending.{rest}");
                }
            }
            Expr::RecordCreate { type_name, .. } | Expr::RecordUpdate { type_name, .. } => {
                if type_name == "Run.View" {
                    *type_name = "__View".to_string();
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms.iter_mut() {
                    pattern(&mut arm.pattern);
                }
            }
            _ => {}
        }
        crate::codegen::expr_walk::for_each_child_mut(e, &mut |child| expr(child));
    }
    for item in items.iter_mut() {
        match item {
            TopLevel::FnDef(fd) => {
                for (_, annotation) in fd.params.iter_mut() {
                    ty(annotation);
                }
                ty(&mut fd.return_type);
                let body = std::sync::Arc::make_mut(&mut fd.body);
                for stmt in body.stmts_mut() {
                    match stmt {
                        Stmt::Binding(_, annotation, value) => {
                            if let Some(annotation) = annotation {
                                ty(annotation);
                            }
                            expr(value);
                        }
                        Stmt::Expr(value) => expr(value),
                    }
                }
            }
            TopLevel::Verify(vb) => {
                for (left, right) in vb.cases.iter_mut() {
                    expr(left);
                    expr(right);
                }
                for givens in vb.case_givens.iter_mut() {
                    for (_, value) in givens.iter_mut() {
                        expr(value);
                    }
                }
                let mut givens: Vec<&mut crate::ast::VerifyGiven> =
                    vb.cases_givens.iter_mut().collect();
                if let crate::ast::VerifyKind::Law(law) = &mut vb.kind {
                    let law = law.as_mut();
                    for value in law
                        .when
                        .iter_mut()
                        .chain(law.because.iter_mut())
                        .chain(law.sample_guards.iter_mut())
                        .chain([&mut law.lhs, &mut law.rhs])
                    {
                        expr(value);
                    }
                    givens.extend(law.givens.iter_mut());
                }
                for given in givens {
                    ty(&mut given.type_name);
                    if let crate::ast::VerifyGivenDomain::Explicit(values) = &mut given.domain {
                        values.iter_mut().for_each(expr);
                    }
                }
            }
            TopLevel::TypeDef(TypeDef::Product { fields, .. }) => {
                for (_, annotation) in fields.iter_mut() {
                    ty(annotation);
                }
            }
            TopLevel::TypeDef(TypeDef::Sum { variants, .. }) => {
                for variant in variants.iter_mut() {
                    for annotation in variant.fields.iter_mut() {
                        ty(annotation);
                    }
                }
            }
            _ => {}
        }
    }
}

/// Whether a function body calls `Run.all()`.
pub(super) fn calls_run_all(fd: &crate::ast::FnDef) -> bool {
    use crate::ast::{Expr, Stmt};
    fd.body.stmts().iter().any(|stmt| {
        let value = match stmt {
            Stmt::Binding(_, _, value) | Stmt::Expr(value) => value,
        };
        crate::codegen::expr_walk::any(value, &mut |e| {
            matches!(&e.node, Expr::FnCall(callee, _) if super::build::dotted_name(callee).as_deref() == Some("Run.all"))
        })
    })
}
