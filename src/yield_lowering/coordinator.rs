//! The generated loop (jasisz/aver#1329, leg 2.3).
//!
//! A program whose `aver.toml` carries a `[run]` table asks for its
//! coordinator to be written for it. What the program writes is then only:
//! the processes (its `yield` functions), the modules that answer the
//! capabilities those processes wait on, and three pure policies over one
//! record it declares. Everything between them — the slot table, the request
//! instance numbers, the wait set, the poll timeout, the dispatch through the
//! answer modules, the job seam, the stop observation, the turn and the loop
//! itself — is generated here, into the entry module, in the reserved `__`
//! namespace, by the same pass that generated the protocol.
//!
//! The generated items are ordinary Aver of the program: `aver verify` runs
//! them, the VM executes them, and the invariants of the loop are `verify`
//! laws over generated functions, so the Lean wall reads them directly rather
//! than being handed a theorem about a generic loop that has to be
//! instantiated. That is what "the loop is generated, not in the stdlib"
//! buys, and it is why the generator emits the laws too: the program does not
//! write them either.
//!
//! The generated names stay referenceable, so a program that wants to write
//! its own loop over the protocol still can. That is a door, not the road.

use crate::ast::{Module, TopLevel, Type, TypeDef};
use crate::config::RunPlan;
use crate::types::checker::TypeError;

use super::{FnSigs, ProcessProtocol};

/// The `__Run` field a job table lives in.
const JOBS_FIELD: &str = "jobs";

/// The fields the view record has to declare, in order, with the type each
/// one carries. `pending`'s value type is the program's own `Pending` sum,
/// so it is checked separately.
const VIEW_FIELDS: [(&str, &str); 4] = [
    ("ready", "List<Int>"),
    ("jobs", "Int"),
    ("room", "Int"),
    ("stopping", "Bool"),
];

/// What the generator resolved about one answer module.
struct Answer {
    /// The module as the manifest names it, e.g. `Ledger`.
    module: String,
    /// The `__Run` field its state is held in, e.g. `ledger`.
    field: String,
    /// The state type, qualified to the module that declares it.
    state: String,
    /// The capabilities this module answers, as the manifest names them.
    capabilities: Vec<String>,
}

/// What the generator resolved about one job kind.
struct Job {
    /// The job-kind capability, e.g. `Validation`.
    capability: String,
    /// The module-qualified `task` function.
    task: String,
    /// The module-qualified `landed` function.
    landed: String,
    /// What `task` answers, e.g. `Option<Bytes>`.
    task_type: String,
    /// What `take` answers once unwrapped, e.g. `Option<Int>`.
    payload_type: String,
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

/// Whether `module` is the one the `[run]` table asked the loop to be
/// generated into. Every other module of the program is lowered without one.
pub(super) fn is_run_module(module: Option<&str>, plan: Option<&RunPlan>) -> bool {
    match (module, plan) {
        (Some(module), Some(plan)) => plan.policies.module() == module,
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

/// Generate the loop for `protocols` into the module `items` declares.
pub(super) fn generate(
    items: &[TopLevel],
    generated: &[TopLevel],
    protocols: &[ProcessProtocol],
    plan: &RunPlan,
    fn_sigs: &FnSigs,
) -> Result<GeneratedLoop, Vec<TypeError>> {
    let module = items.iter().find_map(|item| match item {
        TopLevel::Module(module) => Some(module),
        _ => None,
    });
    let line = module.map(|module| module.line).unwrap_or(1);
    let mut errors = Vec::new();

    if protocols.is_empty() {
        errors.push(error(line, format!(
            "aver.toml declares [run], so the loop of this program is generated into module '{}', but that module writes no process; a process is a function whose effect list names `yield`",
            plan.policies.module()
        )));
        return Err(errors);
    }
    for protocol in protocols {
        if !protocol.params.is_empty() {
            errors.push(error(line, format!(
                "aver.toml declares [run], so the generated loop seats one of every process this module writes, and it has nothing to seat '{}' with; a seated process takes no parameters and asks the module that answers its first request for what it needs",
                protocol.fn_name
            )));
        }
        if protocol.return_type != "Unit" {
            errors.push(error(line, format!(
                "aver.toml declares [run], so process '{}' is seated by the generated loop and its result goes nowhere; a seated process answers Unit, and it is '{}'",
                protocol.fn_name, protocol.return_type
            )));
        }
    }
    if let Some(module) = module {
        errors.extend(check_declared_depends(module, plan, line));
    }
    if fn_sigs.contains_key("main") {
        errors.push(error(line, format!(
            "aver.toml declares [run], so the entry point of this program is generated; module '{}' also writes its own 'main'. Remove it, or remove [run] and drive the protocol by hand",
            plan.policies.module()
        )));
    }

    let answers = match resolve_answers(plan, fn_sigs, line) {
        Ok(answers) => answers,
        Err(found) => {
            errors.extend(found);
            return Err(errors);
        }
    };
    let jobs = match resolve_jobs(plan, &answers, fn_sigs, line) {
        Ok(jobs) => jobs,
        Err(found) => {
            errors.extend(found);
            return Err(errors);
        }
    };
    errors.extend(check_policies(plan, fn_sigs, line));
    let (marker, view_errors) = check_view(items, protocols, plan, line);
    errors.extend(view_errors);
    if !errors.is_empty() {
        return Err(errors);
    }

    let serve_effects = serve_effect_list(protocols, generated, &answers, fn_sigs);
    let module_effects = turn_effect_list(&serve_effects, &jobs);
    let source = write_loop(
        protocols,
        plan,
        &answers,
        &jobs,
        &marker,
        &serve_effects,
        &module_effects,
    );
    match parse_generated(&source) {
        Ok(items) => Ok(GeneratedLoop {
            source,
            items,
            module_effects,
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

/// Every module the generated loop names has to be a module this one already
/// depends on: the loop is spliced in after the dependency walk has run, so
/// an edge it added itself would name a module nothing loaded.
fn check_declared_depends(module: &Module, plan: &RunPlan, line: usize) -> Vec<TypeError> {
    let mut wanted: Vec<String> = vec![crate::capability::work::WAIT_MODULE.to_string()];
    if !plan.jobs.is_empty() {
        wanted.push(crate::capability::work::WORK_MODULE.to_string());
    }
    for (_, answer) in &plan.answers {
        wanted.push(answer.clone());
    }
    for job in &plan.jobs {
        wanted.push(job.capability.clone());
    }
    let mut errors = Vec::new();
    for name in wanted {
        if module.depends.iter().any(|dep| dep == &name) {
            continue;
        }
        errors.push(error(line, format!(
            "aver.toml declares [run], so the loop generated into module '{}' names '{name}'; add '{name}' to its `depends`",
            module.name
        )));
    }
    errors
}

/// One entry per module the manifest answers a capability with: which state
/// it threads, and which field of the run holds it.
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
                "aver.toml declares [run], so the generated loop starts module '{module}' from '{fresh}'; this program has no such function. Add 'fn fresh() -> {module}.State' to '{module}', the state it holds before anything has happened"
            )));
            continue;
        };
        let state = crate::capability::canonicalize_type_names(result.clone(), module).display();
        if !params.is_empty() || !effects.is_empty() || !state.starts_with(&format!("{module}.")) {
            errors.push(error(line, format!(
                "aver.toml declares [run], so the generated loop starts module '{module}' from '{fresh}', which takes nothing, is pure, and answers a type '{module}' declares; it is '{}'",
                render_fresh(params, &state, effects, module)
            )));
            continue;
        }
        answers.push(Answer {
            module: module.clone(),
            field: field_name(module),
            state,
            capabilities: plan
                .answers
                .iter()
                .filter(|(_, owner)| owner == module)
                .map(|(capability, _)| capability.clone())
                .collect(),
        });
    }
    if errors.is_empty() {
        Ok(answers)
    } else {
        Err(errors)
    }
}

/// One entry per job kind whose seam the manifest declared, with the two
/// types the generated turn moves across it.
fn resolve_jobs(
    plan: &RunPlan,
    answers: &[Answer],
    fn_sigs: &FnSigs,
    line: usize,
) -> Result<Vec<Job>, Vec<TypeError>> {
    let mut jobs = Vec::new();
    let mut errors = Vec::new();
    for seam in &plan.jobs {
        fn owner(name: &str) -> &str {
            name.rsplit_once('.').map(|(owner, _)| owner).unwrap_or("")
        }
        for name in [&seam.task, &seam.landed] {
            if !answers.iter().any(|answer| answer.module == owner(name)) {
                errors.push(error(line, format!(
                    "aver.toml declares [run], so the generated turn moves job '{}' across '{name}'; that function's module answers no capability of this program, and the turn holds no state for it",
                    seam.capability
                )));
            }
        }
        let (Some((_, task_result, _)), Some((landed_params, _, _))) =
            (fn_sigs.get(&seam.task), fn_sigs.get(&seam.landed))
        else {
            // Leg 2.1's job-seam check is the one that says which half is
            // missing; this door only needs the types, so it stays quiet.
            continue;
        };
        let Some(payload) = landed_params.get(1) else {
            continue;
        };
        jobs.push(Job {
            capability: seam.capability.clone(),
            task: seam.task.clone(),
            landed: seam.landed.clone(),
            task_type: crate::capability::canonicalize_type_names(
                task_result.clone(),
                owner(&seam.task),
            )
            .display(),
            payload_type: Type::Option(Box::new(crate::capability::canonicalize_type_names(
                payload.clone(),
                owner(&seam.landed),
            )))
            .display(),
        });
    }
    if errors.is_empty() {
        Ok(jobs)
    } else {
        Err(errors)
    }
}

/// The three policies exist, are pure, and read the view the manifest names.
fn check_policies(plan: &RunPlan, fn_sigs: &FnSigs, line: usize) -> Vec<TypeError> {
    let view = plan.policies.view.clone();
    let expected: [(&str, &str, &str); 3] = [
        ("order", plan.policies.order.as_str(), "List<Int>"),
        ("admit", plan.policies.admit.as_str(), "Bool"),
        ("stop", plan.policies.stop.as_str(), "Bool"),
    ];
    let mut errors = Vec::new();
    for (key, name, result) in expected {
        let wanted = if key == "admit" {
            format!("{}(view: {view}, id: Int) -> {result}", bare(name))
        } else {
            format!("{}(view: {view}) -> {result}", bare(name))
        };
        let Some((params, actual, effects)) = fn_sigs.get(name) else {
            errors.push(error(line, format!(
                "aver.toml: [run] names {key} = \"{name}\", but this program has no function '{name}'; it must be '{wanted}'"
            )));
            continue;
        };
        let module = plan.policies.module();
        let rendered: Vec<String> = params
            .iter()
            .map(|param| {
                crate::capability::canonicalize_type_names(param.clone(), module).display()
            })
            .collect();
        let actual = crate::capability::canonicalize_type_names(actual.clone(), module).display();
        let wants: Vec<String> = if key == "admit" {
            vec![view.clone(), "Int".to_string()]
        } else {
            vec![view.clone()]
        };
        if rendered != wants || actual != result {
            errors.push(error(line, format!(
                "aver.toml: [run] names {key} = \"{name}\", so it must be '{wanted}'; it is '{}({}) -> {actual}'",
                bare(name),
                rendered.join(", ")
            )));
        }
        if !effects.is_empty() {
            errors.push(error(line, format!(
                "aver.toml: [run] names {key} = \"{name}\", and that function declares effects [{}]; a policy reads the view and answers, so it is pure",
                effects.join(", ")
            )));
        }
    }
    errors
}

/// The `view-shape` check: the record the three policies read, and the sum it
/// keys its pending table by.
///
/// The view is declared by the program rather than generated because a law
/// about a policy has to name it, and a program cannot name a generated type
/// it never wrote. So the generator fills a shape the program declares, and
/// this is where the two are held to the same shape — exactly as `WorkShape`
/// holds a job kind to `begin` and `take`.
fn check_view(
    items: &[TopLevel],
    protocols: &[ProcessProtocol],
    plan: &RunPlan,
    line: usize,
) -> (String, Vec<TypeError>) {
    let view_name = plan.policies.view_name();
    let expected = expected_view(view_name, protocols);
    let Some(fields) = items.iter().find_map(|item| match item {
        TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) if name == view_name => {
            Some(fields)
        }
        _ => None,
    }) else {
        return (
            String::new(),
            vec![error(
                line,
                format!(
                    "aver.toml: [run] names view = \"{}\", but module '{}' declares no record '{view_name}'. The generated loop fills it, so it declares exactly:\n{expected}",
                    plan.policies.view,
                    plan.policies.module()
                ),
            )],
        );
    };
    let Some((_, pending)) = fields.iter().find(|(name, _)| name == "pending") else {
        return (
            String::new(),
            vec![error(
                line,
                format!(
                    "record '{view_name}' declares no field 'pending', and the generated loop fills one marker per seated process into it. The view the loop fills is exactly:\n{expected}"
                ),
            )],
        );
    };
    let Some(marker) = pending
        .strip_prefix("Map<Int, ")
        .and_then(|rest| rest.strip_suffix('>'))
    else {
        return (
            String::new(),
            vec![error(
                line,
                format!(
                    "field 'pending' of record '{view_name}' is '{pending}', and the generated loop fills one marker per seated process keyed by slot id. The view the loop fills is exactly:\n{expected}"
                ),
            )],
        );
    };
    let marker = marker.trim().to_string();
    let mut errors = Vec::new();
    for (name, ty) in VIEW_FIELDS {
        match fields.iter().find(|(field, _)| field == name) {
            Some((_, actual)) if actual == ty => {}
            Some((_, actual)) => errors.push(error(line, format!(
                "field '{name}' of record '{view_name}' is '{actual}', and the generated loop fills a '{ty}'. The view the loop fills is exactly:\n{expected}"
            ))),
            None => errors.push(error(line, format!(
                "record '{view_name}' declares no field '{name}', and the generated loop fills one. The view the loop fills is exactly:\n{expected}"
            ))),
        }
    }
    if fields.len() != VIEW_FIELDS.len() + 1 {
        errors.push(error(line, format!(
            "record '{view_name}' declares {} fields, and the generated loop fills {}; it has nothing to put in another one. The view the loop fills is exactly:\n{expected}",
            fields.len(),
            VIEW_FIELDS.len() + 1
        )));
    }

    let Some(variants) = items.iter().find_map(|item| match item {
        TopLevel::TypeDef(TypeDef::Sum { name, variants, .. }) if name == &marker => Some(variants),
        _ => None,
    }) else {
        errors.push(error(line, format!(
            "field 'pending' of record '{view_name}' is keyed by '{marker}', but module '{}' declares no sum '{marker}'. The view the loop fills is exactly:\n{expected}",
            plan.policies.module()
        )));
        return (marker, errors);
    };
    for protocol in protocols {
        let wanted = marker_variant(&protocol.fn_name);
        match variants.iter().find(|variant| variant.name == wanted) {
            Some(variant) if variant.fields == vec!["Int".to_string(), "Wait.Wake".to_string()] => {}
            Some(variant) => errors.push(error(line, format!(
                "constructor '{marker}.{wanted}' carries ({}), and the generated loop fills it with the instance number of the request process '{}' is waiting on and what would wake it. The view the loop fills is exactly:\n{expected}",
                variant.fields.join(", "),
                protocol.fn_name
            ))),
            None => errors.push(error(line, format!(
                "sum '{marker}' has no constructor '{wanted}', and the generated loop seats process '{}'. The view the loop fills is exactly:\n{expected}",
                protocol.fn_name
            ))),
        }
    }
    for variant in variants {
        if !protocols
            .iter()
            .any(|protocol| marker_variant(&protocol.fn_name) == variant.name)
        {
            errors.push(error(line, format!(
                "sum '{marker}' declares constructor '{}', and this program writes no process of that name; the generated loop fills one constructor per process. The view the loop fills is exactly:\n{expected}",
                variant.name
            )));
        }
    }
    (marker, errors)
}

/// The declaration the `view-shape` diagnostic prints: what this program's
/// view and marker sum have to be, spelled out.
fn expected_view(view_name: &str, protocols: &[ProcessProtocol]) -> String {
    let mut out = "    type Pending\n".to_string();
    for protocol in protocols {
        out.push_str(&format!(
            "        {}(Int, Wait.Wake)\n",
            marker_variant(&protocol.fn_name)
        ));
    }
    out.push_str(&format!("\n    record {view_name}\n"));
    out.push_str("        pending: Map<Int, Pending>\n");
    for (name, ty) in VIEW_FIELDS {
        out.push_str(&format!("        {name}: {ty}\n"));
    }
    out.trim_end().to_string()
}

/// `peer` → `Peer`: the constructor of the marker sum that stands for one
/// process.
fn marker_variant(fn_name: &str) -> String {
    super::build::capitalize(fn_name)
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

fn bare(name: &str) -> &str {
    name.rsplit_once('.').map(|(_, bare)| bare).unwrap_or(name)
}

/// The whole generated loop, as Aver source.
///
/// Source rather than AST: every line of it is a line a reader of
/// `AVER_YIELD_DUMP=1` has to be able to read back, and the shapes here are
/// the proposal's own text. The parse that follows is the one check that the
/// generator wrote Aver rather than something that looks like it.
fn write_loop(
    protocols: &[ProcessProtocol],
    plan: &RunPlan,
    answers: &[Answer],
    jobs: &[Job],
    marker: &str,
    serve_effects: &[String],
    turn_effects: &[String],
) -> String {
    let mut out = String::new();
    let view = plan.policies.view_name().to_string();
    let has_jobs = !jobs.is_empty();

    // ── The table ──────────────────────────────────────────────────
    out.push_str("type __Process\n");
    for protocol in protocols {
        out.push_str(&format!(
            "    {}({})\n",
            marker_variant(&protocol.fn_name),
            protocol.request
        ));
    }
    out.push_str("\nrecord __Slot\n    seq: Int\n    pending: __Process\n    waiting: Wait.Wake\n");
    out.push_str("\nrecord __Run\n    slots: Map<Int, __Slot>\n");
    for answer in answers {
        out.push_str(&format!("    {}: {}\n", answer.field, answer.state));
    }
    if has_jobs {
        out.push_str(&format!("    {JOBS_FIELD}: Map<Int, Work.Job>\n"));
    }
    out.push_str("    dropped: Int\n    stopping: Bool\n    nextId: Int\n");

    out.push_str(&format!(
        "\nfn __maxJobs() -> Int\n    ? \"The job limit this program was built with, from [work] max-jobs in aver.toml.\"\n    {}\n",
        plan.max_jobs
    ));

    out.push_str("\nfn __fresh() -> __Run\n    ? \"The run before anything has happened: no slot seated, every answer module at its own empty state.\"\n    __Run(slots = {}");
    for answer in answers {
        out.push_str(&format!(", {} = {}.fresh()", answer.field, answer.module));
    }
    if has_jobs {
        out.push_str(", jobs = {}");
    }
    out.push_str(", dropped = 0, stopping = false, nextId = 1)\n");

    // ── Seating ────────────────────────────────────────────────────
    let effects = |list: &[String]| {
        if list.is_empty() {
            String::new()
        } else {
            format!("    ! [{}]\n", list.join(", "))
        }
    };
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "\nfn __seat{upper}(run: __Run) -> __Run\n    ? \"Seats the '{}' process under its own slot id, at its first request.\"\n{}    __seated{upper}(run, {}())\n",
            protocol.fn_name, effects(serve_effects), protocol.start
        ));
        out.push_str(&format!(
            "\nfn __seated{upper}(run: __Run, outcome: {}) -> __Run\n    ? \"A process that is already done is not seated; one that is waiting takes the next free slot id.\"\n    match outcome\n        {}.Done(_) -> run\n        {}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, run.nextId, __Slot(seq = 1, pending = __Process.{upper}(request), waiting = Wait.Wake.NextTurn)), nextId = run.nextId + 1)\n",
            protocol.outcome, protocol.outcome, protocol.outcome
        ));
    }

    // ── The slot table and its two invariants ──────────────────────
    out.push_str("\nfn __current(run: __Run, id: Int) -> Int\n    ? \"The instance number of the request one process is waiting on, or -1 when nothing is seated under that id.\"\n    match Map.get(run.slots, id)\n        Option.None -> 0 - 1\n        Option.Some(slot) -> slot.seq\n");
    out.push_str("\nfn __nextInstance(seq: Int) -> Int\n    ? \"The instance number an answer for the current one leaves behind. It rises, so the instance just answered can never be current again.\"\n    seq + 1\n");
    out.push_str("\nfn __parked(slot: __Slot, wake: Wait.Wake) -> __Slot\n    ? \"The slot a Later leaves behind: the same instance and the same request, now remembering what would make asking again worth it.\"\n    __Slot(seq = slot.seq, pending = slot.pending, waiting = wake)\n");
    out.push_str("\nfn __park(run: __Run, id: Int, wake: Wait.Wake) -> __Run\n    ? \"A Later: the request stays where it is with the same instance number, and the state the answer module returned is discarded.\"\n    match Map.get(run.slots, id)\n        Option.None -> run\n        Option.Some(slot) -> __Run.update(run, slots = Map.set(run.slots, id, __parked(slot, wake)))\n");
    out.push_str("\nfn __staleInstance(run: __Run, id: Int, seq: Int) -> Bool\n    ? \"Why an answer carrying this instance number changes nothing: it is not the number the slot under this id is waiting on.\"\n    seq != __current(run, id)\n");

    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "\nfn __settle{upper}(run: __Run, id: Int, seq: Int, outcome: {}) -> __Run\n    ? \"An answer for the current instance replaces that process's one slot and raises its number; an answer for an older instance changes nothing and is counted.\"\n    match seq == __current(run, id)\n        false -> __Run.update(run, dropped = run.dropped + 1)\n        true -> __settled{upper}(run, id, seq, outcome)\n",
            protocol.outcome
        ));
        out.push_str(&format!(
            "\nfn __settled{upper}(run: __Run, id: Int, seq: Int, outcome: {}) -> __Run\n    ? \"What an answer for the current instance leaves behind: an empty slot when the process is done, the next request under the next instance number otherwise.\"\n    match outcome\n        {}.Done(_) -> __Run.update(run, slots = Map.remove(run.slots, id))\n        {}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, id, __Slot(seq = __nextInstance(seq), pending = __Process.{upper}(request), waiting = Wait.Wake.NextTurn)))\n",
            protocol.outcome, protocol.outcome, protocol.outcome
        ));
        out.push_str(&format!(
            "\nfn __settleKeepsOrShrinks{upper}(run: __Run, id: Int) -> Bool\n    ? \"Why settling never seats a second process: this id is already seated, so every arm either counts a late answer, or removes that one slot, or replaces it.\"\n    Map.has(run.slots, id)\n"
        ));
    }

    // ── The view ───────────────────────────────────────────────────
    out.push_str(&format!(
        "\nfn __view(run: __Run, ready: List<Int>) -> {view}\n    ? \"The resource-free summary of a run that the three policies read, and the only shape a law about them can sample.\"\n    __viewOf(run, ready, {})\n",
        if has_jobs {
            format!("Map.len(run.{JOBS_FIELD})")
        } else {
            "0".to_string()
        }
    ));
    out.push_str(&format!(
        "\nfn __viewOf(run: __Run, ready: List<Int>, jobs: Int) -> {view}\n    ? \"The view, once the turn has counted the jobs it is running.\"\n    {view}(pending = __pendingOf(run, Map.keys(run.slots), {{}}), ready = ready, jobs = jobs, room = __maxJobs() - jobs, stopping = run.stopping)\n"
    ));
    out.push_str(&format!(
        "\nfn __pendingOf(run: __Run, ids: List<Int>, acc: Map<Int, {marker}>) -> Map<Int, {marker}>\n    ? \"One marker per seated process, in key order.\"\n    match ids\n        [] -> acc\n        [id, ..rest] -> __pendingOf(run, rest, __pendingAt(run, id, acc))\n"
    ));
    out.push_str(&format!(
        "\nfn __pendingAt(run: __Run, id: Int, acc: Map<Int, {marker}>) -> Map<Int, {marker}>\n    ? \"The marker for one id, if that id is still seated.\"\n    match Map.get(run.slots, id)\n        Option.None -> acc\n        Option.Some(slot) -> Map.set(acc, id, __markerOf(slot))\n"
    ));
    out.push_str(&format!(
        "\nfn __markerOf(slot: __Slot) -> {marker}\n    ? \"Which process one slot holds, the instance it is waiting on, and what would wake it.\"\n    match slot.pending\n"
    ));
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "        __Process.{upper}(_) -> {marker}.{upper}(slot.seq, slot.waiting)\n"
        ));
    }

    // ── The wait and the timeout ───────────────────────────────────
    out.push_str(&format!(
        "\nfn __waitSet(run: __Run, ids: List<Int>, acc: Map<Int, Wait.Item>) -> Map<Int, Wait.Item>\n    ? \"The one wait of a turn, keyed by slot id: what each parked request is waiting on{}.\"\n    match ids\n        [] -> {}\n        [id, ..rest] -> __waitSet(run, rest, __waitAt(run, id, acc))\n",
        if has_jobs { ", plus one key per running job" } else { "" },
        if has_jobs { format!("__jobItems(run, Map.keys(run.{JOBS_FIELD}), acc)") } else { "acc".to_string() },
    ));
    out.push_str("\nfn __waitAt(run: __Run, id: Int, acc: Map<Int, Wait.Item>) -> Map<Int, Wait.Item>\n    ? \"What the request seated under one id is waiting on, if anything is seated there.\"\n    match Map.get(run.slots, id)\n        Option.None -> acc\n        Option.Some(slot) -> __waitOn(acc, id, slot.waiting)\n");
    out.push_str("\nfn __waitOn(acc: Map<Int, Wait.Item>, id: Int, wake: Wait.Wake) -> Map<Int, Wait.Item>\n    ? \"Only a request parked on a socket or a job is waited for; a deadline is the poll's own timeout, and a NextTurn is asked again rather than woken.\"\n    match wake\n        Wait.Wake.Item(item) -> Map.set(acc, id, item)\n        Wait.Wake.After(_) -> acc\n        Wait.Wake.NextTurn -> acc\n");
    if has_jobs {
        out.push_str(
            "\nfn __jobItems(run: __Run, keys: List<Int>, acc: Map<Int, Wait.Item>) -> Map<Int, Wait.Item>\n    ? \"One wait-set key per running job.\"\n    match keys\n        [] -> acc\n        [key, ..rest] -> __jobItems(run, rest, __jobItem(run, key, acc))\n",
        );
        out.push_str(&format!(
            "\nfn __jobItem(run: __Run, key: Int, acc: Map<Int, Wait.Item>) -> Map<Int, Wait.Item>\n    ? \"One running job, as the thing the wait watches for it.\"\n    match Map.get(run.{JOBS_FIELD}, key)\n        Option.None -> acc\n        Option.Some(job) -> Map.set(acc, key, Wait.Item.Job(job))\n"
        ));
    }
    out.push_str("\nfn __timeout(run: __Run) -> Int\n    ? \"How long this turn may wait: nothing at all while some request is to be asked again, the soonest deadline when one is pending, and one second when neither.\"\n    __deadline(run, Map.keys(run.slots), 0 - 1)\n");
    out.push_str("\nfn __deadline(run: __Run, ids: List<Int>, best: Int) -> Int\n    ? \"The soonest deadline across every seated slot, or -1 when none carries one.\"\n    match ids\n        [] -> __tick(best)\n        [id, ..rest] -> __deadline(run, rest, __deadlineAt(run, id, best))\n");
    out.push_str("\nfn __deadlineAt(run: __Run, id: Int, best: Int) -> Int\n    ? \"What one id contributes to the turn's wait.\"\n    match Map.get(run.slots, id)\n        Option.None -> best\n        Option.Some(slot) -> __soonest(best, slot.waiting)\n");
    out.push_str("\nfn __soonest(best: Int, wake: Wait.Wake) -> Int\n    ? \"A request to be asked again next turn wins outright; two deadlines keep the nearer; a socket or a job carries none.\"\n    match wake\n        Wait.Wake.NextTurn -> 0\n        Wait.Wake.Item(_) -> best\n        Wait.Wake.After(ms) -> __nearer(best, ms)\n");
    out.push_str("\nfn __nearer(best: Int, ms: Int) -> Int\n    ? \"The nearer of two deadlines, where -1 means none yet.\"\n    match best < 0\n        true -> ms\n        false -> __smaller(best, ms)\n");
    out.push_str("\nfn __smaller(left: Int, right: Int) -> Int\n    ? \"The smaller of two whole numbers.\"\n    match left < right\n        true -> left\n        false -> right\n");
    out.push_str("\nfn __tick(best: Int) -> Int\n    ? \"The wait of a turn with no deadline in it: one second.\"\n    match best < 0\n        true -> 1000\n        false -> best\n");

    // ── The serve path ─────────────────────────────────────────────
    out.push_str(&format!(
        "\nfn __serve(run: __Run, id: Int) -> __Run\n    ? \"Serves one slot: asks the module that answers its request, then either replaces the slot with what the process does next, or leaves it parked.\"\n{}    match Map.get(run.slots, id)\n        Option.None -> run\n        Option.Some(slot) -> __serveSlot(run, id, slot)\n",
        effects(serve_effects)
    ));
    out.push_str(&format!(
        "\nfn __serveSlot(run: __Run, id: Int, slot: __Slot) -> __Run\n    ? \"Which process the slot holds decides which dispatch answers it.\"\n{}    match slot.pending\n",
        effects(serve_effects)
    ));
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "        __Process.{upper}(request) -> __serve{upper}(run, id, slot.seq, request)\n"
        ));
    }
    for protocol in protocols {
        out.push_str(&write_serve(protocol, answers, serve_effects));
    }

    // ── The job seam ───────────────────────────────────────────────
    for job in jobs {
        out.push_str(&write_job(job, answers));
    }

    // ── The turn and the loop ──────────────────────────────────────
    out.push_str(&format!(
        "\nfn __turn(run: __Run) -> Result<__Run, String>\n    ? \"One turn: observe the stop flag, wait once, serve the admitted slots in the policy's order{}.\"\n{}    observed = __Run.update(run, stopping = Process.stopRequested())\n    ready = Wait.poll(__waitSet(observed, Map.keys(observed.slots), {{}}), __timeout(observed))?\n    served = __serveEach(observed, ready, {}(__view(observed, ready)))\n{}",
        if has_jobs { ", take every job that finished, and start jobs while there is room" } else { "" },
        effects(turn_effects),
        bare(&plan.policies.order),
        if has_jobs {
            let job = &jobs[0];
            format!(
                "    taken = __takeEach{}(served, ready)?\n    __startJobs{}(taken)\n",
                marker_variant(&job.capability),
                marker_variant(&job.capability)
            )
        } else {
            "    Result.Ok(served)\n".to_string()
        }
    ));
    out.push_str(&format!(
        "\nfn __serveEach(run: __Run, ready: List<Int>, ids: List<Int>) -> __Run\n    ? \"Every id the policy ordered, once, if the policy admits it in this turn.\"\n{}    match ids\n        [] -> run\n        [id, ..rest] -> __serveEach(__serveIf(run, ready, id), ready, rest)\n",
        effects(serve_effects)
    ));
    out.push_str(&format!(
        "\nfn __serveIf(run: __Run, ready: List<Int>, id: Int) -> __Run\n    ? \"One id, served only if the policy admits it.\"\n{}    match {}(__view(run, ready), id)\n        false -> run\n        true -> __serve(run, id)\n",
        effects(serve_effects),
        bare(&plan.policies.admit)
    ));
    out.push_str(&format!(
        "\nfn __runAll(run: __Run) -> Result<__Run, String>\n    ? \"Turns until the policy says stop or nothing is seated.\"\n{}    match Bool.or({}(__view(run, [])), Map.len(run.slots) == 0)\n        true -> Result.Ok(run)\n        false -> __runAll(__turn(run)?)\n",
        effects(turn_effects),
        bare(&plan.policies.stop)
    ));
    let seated = protocols
        .iter()
        .fold("__fresh()".to_string(), |inner, protocol| {
            format!("__seat{}({inner})", marker_variant(&protocol.fn_name))
        });
    out.push_str(&format!(
        "\nfn main() -> Result<Unit, String>\n    ? \"Seats one of every process this program writes and turns until the policy stops the run.\"\n{}    __over(__runAll({seated})?)\n",
        effects(turn_effects)
    ));
    out.push_str("\nfn __over(run: __Run) -> Result<Unit, String>\n    ? \"The run is over: every process that finished left its slot, and whatever is still seated stays where it is.\"\n    Result.Ok(Unit)\n");

    // ── The invariants ─────────────────────────────────────────────
    out.push_str(&write_laws(protocols, answers, serve_effects));
    out
}

/// The dispatch of one process: one arm per request kind, and one function
/// per kind that reads the reply the answer module gave.
fn write_serve(protocol: &ProcessProtocol, answers: &[Answer], serve_effects: &[String]) -> String {
    let upper = marker_variant(&protocol.fn_name);
    let effects = if serve_effects.is_empty() {
        String::new()
    } else {
        format!("    ! [{}]\n", serve_effects.join(", "))
    };
    let mut out = format!(
        "\nfn __serve{upper}(run: __Run, id: Int, seq: Int, request: {}) -> __Run\n    ? \"One arm per request kind of '{}'. This is the match a program writes by hand today.\"\n{effects}    match request\n",
        protocol.request, protocol.fn_name
    );
    let mut bodies = String::new();
    for kind in &protocol.kinds {
        let Some(operation) = &kind.operation else {
            out.push_str(&format!(
                "        {}.{}(state) -> __settle{upper}(run, id, seq, {}(state))\n",
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
        let mut call_args = vec![format!("run.{}", answer.field)];
        call_args.extend(binders.iter().cloned());
        let mut pattern = binders.clone();
        pattern.push("state".to_string());
        out.push_str(&format!(
            "        {}.{}({}) -> __serve{upper}{}(run, id, seq, state, {}.{op}({}))\n",
            protocol.request,
            kind.name,
            pattern.join(", "),
            kind.name,
            answer.module,
            call_args.join(", ")
        ));
        let reply = format!(
            "{capability}.{}",
            crate::capability::answer::reply_type_name(op)
        );
        let resume = match kind.answer_type.as_deref() {
            Some("Unit") | None => format!("{}(state)", kind.answer_fn),
            Some(_) => format!("{}(state, __answer)", kind.answer_fn),
        };
        let now_binder = match kind.answer_type.as_deref() {
            Some("Unit") | None => "_",
            Some(_) => "__answer",
        };
        bodies.push_str(&format!(
            "\nfn __serve{upper}{}(run: __Run, id: Int, seq: Int, state: {}, answered: Tuple<{}, {reply}>) -> __Run\n    ? \"A Now settles the slot with the answer function of this kind; a Later parks the request and discards the state the module returned, so a Later cannot change it.\"\n    match answered\n        (__next, __reply) -> match __reply\n            {reply}.Later(__wake) -> __park(run, id, __wake)\n            {reply}.Now({now_binder}) -> __settle{upper}(__Run.update(run, {} = __next), id, seq, {resume})\n",
            kind.name, kind.state, answer.state, answer.field
        ));
    }
    out.push_str(&bodies);
    out
}

/// The two ends of one job kind's seam, as the turn crosses them.
fn write_job(job: &Job, answers: &[Answer]) -> String {
    let upper = marker_variant(&job.capability);
    let landed_field = answers
        .iter()
        .find(|answer| {
            job.landed
                .rsplit_once('.')
                .is_some_and(|(owner, _)| owner == answer.module)
        })
        .map(|answer| answer.field.clone())
        .unwrap_or_default();
    let task_field = answers
        .iter()
        .find(|answer| {
            job.task
                .rsplit_once('.')
                .is_some_and(|(owner, _)| owner == answer.module)
        })
        .map(|answer| answer.field.clone())
        .unwrap_or_default();
    let mut out = String::new();
    out.push_str(&format!(
        "\nfn __takeEach{upper}(run: __Run, ready: List<Int>) -> Result<__Run, String>\n    ? \"A job result is a coordinator event, not an answer to a request: it goes to the answer state through the manifest's `landed` function and resumes nobody.\"\n    ! [{}.take]\n    match ready\n        [] -> Result.Ok(run)\n        [key, ..rest] -> __takeEach{upper}(__taken{upper}(run, key)?, rest)\n",
        job.capability
    ));
    out.push_str(&format!(
        "\nfn __taken{upper}(run: __Run, key: Int) -> Result<__Run, String>\n    ? \"One reported key: a key that is not a running job of this kind is not this seam's business.\"\n    ! [{}.take]\n    match Map.get(run.{JOBS_FIELD}, key)\n        Option.None -> Result.Ok(run)\n        Option.Some(job) -> __landed{upper}(__Run.update(run, {JOBS_FIELD} = Map.remove(run.{JOBS_FIELD}, key)), {}.take(job)?)\n",
        job.capability, job.capability
    ));
    out.push_str(&format!(
        "\nfn __landed{upper}(run: __Run, result: {}) -> Result<__Run, String>\n    ? \"Where a finished job's result goes: the `landed` function of the answer state, which resumes nobody.\"\n    match result\n        Option.None -> Result.Ok(run)\n        Option.Some(payload) -> Result.Ok(__Run.update(run, {landed_field} = {}(run.{landed_field}, payload)))\n",
        job.payload_type, job.landed
    ));
    out.push_str(&format!(
        "\nfn __startJobs{upper}(run: __Run) -> Result<__Run, String>\n    ? \"While there is room under [work] max-jobs and the answer state has a task, start one.\"\n    ! [{}.begin]\n    match Map.len(run.{JOBS_FIELD}) >= __maxJobs()\n        true -> Result.Ok(run)\n        false -> __startOne{upper}(run, {}(run.{task_field}))\n",
        job.capability, job.task
    ));
    out.push_str(&format!(
        "\nfn __startOne{upper}(run: __Run, task: {}) -> Result<__Run, String>\n    ? \"One task, started under its own key so the wait can watch it.\"\n    ! [{}.begin]\n    match task\n        Option.None -> Result.Ok(run)\n        Option.Some(payload) -> __startJobs{upper}(__Run.update(run, {JOBS_FIELD} = Map.set(run.{JOBS_FIELD}, run.nextId, {}.begin(payload)?), nextId = run.nextId + 1))\n",
        job.task_type, job.capability, job.capability
    ));
    out
}

/// The invariants of the loop, as `verify` laws over the generated
/// functions, so the program does not write them either.
///
/// I2 (an answer applies only to the current instance, and a late one is
/// dropped and counted) and I3 (an answer for the current instance raises
/// that instance, so the same one can never be answered twice) are stated
/// here. I4 — a `Later` leaves the answer state unchanged — is by
/// construction, because every `__serve<P><Kind>` discards the state the
/// module returned on its `Later` arm; the two laws below say what a law can
/// see of it: neither the instance number nor any answer state moves. I1 is
/// the size comparison the proposal expected to stay open, written with its
/// `because` so the report can say where it stands.
fn write_laws(
    protocols: &[ProcessProtocol],
    answers: &[Answer],
    serve_effects: &[String],
) -> String {
    let mut out = String::new();
    let _ = serve_effects;
    // The sample seats each process at the request it re-enters itself with.
    // A process takes no parameters, so that request carries nothing, which
    // is what makes the sample — and every law below it — pure even when the
    // process performs something in place on its way to its first stop.
    let mut sampled: Vec<&ProcessProtocol> = Vec::new();
    for protocol in protocols {
        let Some(kind) = protocol.kinds.iter().find(|kind| kind.operation.is_none()) else {
            continue;
        };
        let Some((variant, _)) = kind.variants.iter().find(|(_, arity)| *arity == 0) else {
            continue;
        };
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "\nfn __sampleSeat{upper}(run: __Run) -> __Run\n    ? \"The '{}' process, seated at the request it re-enters itself with. Pure, so the laws below can sample it.\"\n    __seated{upper}(run, {}.Waiting({}.{}({}.{variant})))\n",
            protocol.fn_name, protocol.outcome, protocol.request, kind.name, kind.state
        ));
        sampled.push(protocol);
    }
    let seated = sampled
        .iter()
        .fold("__fresh()".to_string(), |inner, protocol| {
            format!("__sampleSeat{}({inner})", marker_variant(&protocol.fn_name))
        });
    out.push_str(&format!(
        "\nfn __sampleRun() -> __Run\n    ? \"One run with one of every process seated: the sample the laws below are stated over.\"\n    {seated}\n"
    ));
    out.push_str("\nfn __slotIsSeated(run: __Run, id: Int) -> Bool\n    ? \"Why a Later moves nothing: the slot written back is the slot that was already there, carrying the instance number it already had.\"\n    Map.has(run.slots, id)\n");

    let ids: Vec<String> = (1..=sampled.len() + 1).map(|id| id.to_string()).collect();
    let wake_domain = "    given wake: Wait.Wake = [Wait.Wake.NextTurn, Wait.Wake.After(5)]\n";
    let park_domain = format!(
        "    given run: __Run = [__sampleRun()]\n    given id: Int = [{}]\n{wake_domain}    when Map.has(run.slots, id)\n    because __slotIsSeated(run, id)\n",
        ids.join(", ")
    );
    out.push_str(&format!(
        "\nverify __park law laterKeepsTheInstance\n{park_domain}    __current(__park(run, id, wake), id) => __current(run, id)\n"
    ));
    for answer in answers {
        out.push_str(&format!(
            "\nverify __park law laterLeaves{}Alone\n{park_domain}    __park(run, id, wake).{} => run.{}\n",
            marker_variant(&answer.field),
            answer.field,
            answer.field
        ));
    }
    out.push_str("\nverify __nextInstance law nowRaisesTheInstance\n    given seq: Int = [0, 1, 7]\n    __nextInstance(seq) > seq holds\n");
    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        let domain = format!(
            "    given run: __Run = [__sampleRun()]\n    given id: Int = [{}]\n    given stale: Int = [0, {}]\n    given outcome: {} = [{}.Done(Unit)]\n",
            ids.join(", "),
            sampled.len() + 2,
            protocol.outcome,
            protocol.outcome
        );
        out.push_str(&format!(
            "\nverify __settle{upper} law lateAnswerIsDropped\n{domain}    when stale != __current(run, id)\n    because __staleInstance(run, id, stale)\n    __settle{upper}(run, id, stale, outcome).slots => run.slots\n"
        ));
        out.push_str(&format!(
            "\nverify __settle{upper} law lateAnswerIsRecorded\n{domain}    when stale != __current(run, id)\n    because __staleInstance(run, id, stale)\n    __settle{upper}(run, id, stale, outcome).dropped => run.dropped + 1\n"
        ));
        out.push_str(&format!(
            "\nverify __settle{upper} law oneSlotPerProcess\n    given run: __Run = [__sampleRun()]\n    given id: Int = [{}]\n    given outcome: {} = [{}.Done(Unit)]\n    when Map.has(run.slots, id)\n    because __settleKeepsOrShrinks{upper}(run, id)\n    Map.len(__settle{upper}(run, id, __current(run, id), outcome).slots) <= Map.len(run.slots) holds\n",
            ids.join(", "),
            protocol.outcome,
            protocol.outcome
        ));
    }
    out
}

/// The module answering one capability, matched the way an effect entry is:
/// a manifest names the capability as the program writes it in `depends`.
fn answering_module<'a>(answers: &'a [Answer], capability: &str) -> Option<&'a Answer> {
    answers.iter().find(|answer| {
        answer
            .capabilities
            .iter()
            .any(|name| capability == name || capability.ends_with(&format!(".{name}")))
    })
}

/// The effects the serve path performs: whatever the answer functions it
/// calls declare. A program whose answer modules are pure keeps the whole
/// serve path pure, and the turn then performs only the wait and the jobs.
fn serve_effect_list(
    protocols: &[ProcessProtocol],
    generated: &[TopLevel],
    answers: &[Answer],
    fn_sigs: &FnSigs,
) -> Vec<String> {
    let mut found: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    // Decision 4: a generated protocol function carries the unmarked
    // operations its own segment performs. The serve path calls them, so it
    // performs them too.
    for item in generated {
        if let TopLevel::FnDef(fd) = item {
            found.extend(fd.effects.iter().map(|effect| effect.node.clone()));
        }
    }
    for protocol in protocols {
        for kind in &protocol.kinds {
            let Some(operation) = &kind.operation else {
                continue;
            };
            let Some((capability, op)) = operation.rsplit_once('.') else {
                continue;
            };
            let Some(answer) = answering_module(answers, capability) else {
                continue;
            };
            if let Some((_, _, effects)) = fn_sigs.get(&format!("{}.{op}", answer.module)) {
                found.extend(effects.iter().cloned());
            }
        }
    }
    found.into_iter().collect()
}

/// What a whole turn performs: the stop observation, the one wait, the serve
/// path, and both ends of every job kind.
fn turn_effect_list(serve: &[String], jobs: &[Job]) -> Vec<String> {
    let mut found: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    found.insert("Process.stopRequested".to_string());
    found.insert("Wait.poll".to_string());
    found.extend(serve.iter().cloned());
    for job in jobs {
        found.insert(format!("{}.begin", job.capability));
        found.insert(format!("{}.take", job.capability));
    }
    found.into_iter().collect()
}
