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

/// The fields the generated run table writes itself. An answer module whose
/// name would take one of them is refused rather than generating a record
/// with the field declared twice.
const RUN_FIELDS: [&str; 6] = ["slots", JOBS_FIELD, "dropped", "stopping", "now", "nextId"];

/// The operation the end of a run performs on a job that is still running.
const CANCEL: &str = "Work.cancel";

/// The fields the view record has to declare, in order, with the type each
/// one carries. `pending`'s value type is the program's own `Pending` sum,
/// so it is checked separately.
const VIEW_FIELDS: [(&str, &str); 5] = [
    ("ready", "List<Int>"),
    ("askable", "List<Int>"),
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
    /// What one finished job carries, e.g. `Int`: the `take` payload.
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
    // The turn crosses one job seam: it reads `jobs[0]` where it writes the
    // take and the start, so a second job kind would be declared, admitted
    // by the checker, and then never started or taken. Refuse it here rather
    // than generate a loop that quietly serves one of them.
    if jobs.len() > 1 {
        let kinds: Vec<&str> = jobs.iter().map(|job| job.capability.as_str()).collect();
        errors.push(error(line, format!(
            "aver.toml declares [run], so the generated turn crosses the seam of one job kind, and this program declares {}: {}. One job kind per generated loop is the limit in this build. Keep one `work` binding, or remove [run] and drive the job seam by hand",
            kinds.len(),
            kinds.join(", ")
        )));
    }
    // The turn asks the answer state for the next task once per slot of room,
    // and starting a job does not change that state, so a limit above one
    // would start the same task once per slot. Refuse it rather than run it.
    if !jobs.is_empty() && plan.max_jobs != 1 {
        let job = &jobs[0];
        errors.push(error(line, format!(
            "aver.toml declares [run], so the generated turn starts the jobs of kind '{}' itself: it asks '{}' for the next task once per slot of room, and starting a job does not change the state that answered. This program's limit is {}, so one task would be started up to {} times. Declare `[work] max-jobs = 1`, or remove [run] and drive the job seam by hand",
            job.capability, job.task, plan.max_jobs, plan.max_jobs
        )));
    }
    errors.extend(check_policies(plan, fn_sigs, line));
    let (marker, view_errors) = check_view(items, protocols, plan, line);
    errors.extend(view_errors);
    if !errors.is_empty() {
        return Err(errors);
    }

    let process_effects = process_effect_lists(protocols, generated, &answers, fn_sigs);
    let serve_effects = serve_effect_list(&process_effects);
    let turn_effects = turn_effect_list(&serve_effects, &jobs);
    let main_effects = main_effect_list(&turn_effects, &process_effects, &jobs);
    let source = write_loop(
        protocols,
        plan,
        &answers,
        &jobs,
        &marker,
        &process_effects,
        &serve_effects,
        &turn_effects,
        &main_effects,
    );
    // The module's own boundary has to admit everything generated into it,
    // and the entry point is the widest of the generated functions.
    let module_effects = main_effects;
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
        let field = field_name(module);
        if RUN_FIELDS.contains(&field.as_str()) {
            errors.push(error(line, format!(
                "aver.toml declares [run], so the generated run table holds the state of module '{module}' in a field named '{field}', and the loop writes a field of its own by that name. Rename the module"
            )));
            continue;
        }
        if let Some(other) = answers.iter().find(|answer| answer.field == field) {
            errors.push(error(line, format!(
                "aver.toml declares [run], so the generated run table holds the state of module '{module}' in a field named '{field}', and it already holds the state of module '{}' there. Rename one of them",
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
        // `landed` is `(S, Result<R, String>) -> S`, so the payload one job
        // carries is the ok half of its second parameter.
        let Some(Type::Result(payload, _)) = landed_params.get(1) else {
            continue;
        };
        let payload = payload.as_ref();
        jobs.push(Job {
            capability: seam.capability.clone(),
            task: seam.task.clone(),
            landed: seam.landed.clone(),
            task_type: crate::capability::canonicalize_type_names(
                task_result.clone(),
                owner(&seam.task),
            )
            .display(),
            payload_type: crate::capability::canonicalize_type_names(
                payload.clone(),
                owner(&seam.landed),
            )
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
#[allow(clippy::too_many_arguments)]
fn write_loop(
    protocols: &[ProcessProtocol],
    plan: &RunPlan,
    answers: &[Answer],
    jobs: &[Job],
    marker: &str,
    process_effects: &[ProcessEffects],
    serve_effects: &[String],
    turn_effects: &[String],
    main_effects: &[String],
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
    out.push_str("\nrecord __Slot\n    seq: Int\n    pending: __Process\n    waiting: Wait.Wake\n    due: Int\n    ms: Int\n");
    out.push_str("\nrecord __Run\n    slots: Map<Int, __Slot>\n");
    for answer in answers {
        out.push_str(&format!("    {}: {}\n", answer.field, answer.state));
    }
    if has_jobs {
        out.push_str(&format!("    {JOBS_FIELD}: Map<Int, Work.Job>\n"));
    }
    out.push_str("    dropped: Int\n    stopping: Bool\n    now: Int\n    nextId: Int\n");

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
    out.push_str(", dropped = 0, stopping = false, now = 0, nextId = 1)\n");

    // ── Seating ────────────────────────────────────────────────────
    let effects = |list: &[String]| {
        if list.is_empty() {
            String::new()
        } else {
            format!("    ! [{}]\n", list.join(", "))
        }
    };
    for (protocol, performs) in protocols.iter().zip(process_effects) {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "\nfn __seat{upper}(run: __Run) -> __Run\n    ? \"Seats the '{}' process under its own slot id, at its first request.\"\n{}    __seated{upper}(run, {}())\n",
            protocol.fn_name, effects(&performs.seat), protocol.start
        ));
        out.push_str(&format!(
            "\nfn __seated{upper}(run: __Run, outcome: {}) -> __Run\n    ? \"A process that is already done is not seated; one that is waiting takes the next free slot id.\"\n    match outcome\n        {}.Done(_) -> run\n        {}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, run.nextId, __Slot(seq = 1, pending = __Process.{upper}(request), waiting = Wait.Wake.NextTurn, due = 0, ms = 0)), nextId = run.nextId + 1)\n",
            protocol.outcome, protocol.outcome, protocol.outcome
        ));
    }

    // ── The slot table and its two invariants ──────────────────────
    out.push_str("\nfn __current(run: __Run, id: Int) -> Int\n    ? \"The instance number of the request one process is waiting on, or -1 when nothing is seated under that id.\"\n    match Map.get(run.slots, id)\n        Option.None -> 0 - 1\n        Option.Some(slot) -> slot.seq\n");
    out.push_str("\nfn __nextInstance(seq: Int) -> Int\n    ? \"The instance number an answer for the current one leaves behind. It rises, so the instance just answered can never be current again.\"\n    seq + 1\n");
    out.push_str("\nfn __parked(slot: __Slot, wake: Wait.Wake, now: Int) -> __Slot\n    ? \"The slot a Later leaves behind: the same instance and the same request, now remembering what would make asking again worth it. A deadline is turned into the clock reading it falls due at, because ms is how long from now and the turn asks against the clock; the ms that was asked for is kept beside it, so a clock that steps backwards cannot strand the request behind a due it will never reach.\"\n    __Slot(seq = slot.seq, pending = slot.pending, waiting = wake, due = __dueOf(wake, now), ms = __msOf(wake))\n");
    out.push_str("\nfn __dueOf(wake: Wait.Wake, now: Int) -> Int\n    ? \"The clock reading a request parked on a deadline may be asked again at. A request parked on a socket, on a job, or on the next turn carries none.\"\n    match wake\n        Wait.Wake.After(ms) -> now + ms\n        Wait.Wake.Item(_) -> 0\n        Wait.Wake.NextTurn -> 0\n");
    out.push_str("\nfn __msOf(wake: Wait.Wake) -> Int\n    ? \"How long the request asked to be left alone for. A request parked on a socket, on a job, or on the next turn asked for nothing.\"\n    match wake\n        Wait.Wake.After(ms) -> ms\n        Wait.Wake.Item(_) -> 0\n        Wait.Wake.NextTurn -> 0\n");
    out.push_str("\nfn __park(run: __Run, id: Int, wake: Wait.Wake) -> __Run\n    ? \"A Later: the request stays where it is with the same instance number. The state the answer module returned was already written back, because a Later is where a module records its own progress.\"\n    match Map.get(run.slots, id)\n        Option.None -> run\n        Option.Some(slot) -> __Run.update(run, slots = Map.set(run.slots, id, __parked(slot, wake, run.now)))\n");
    out.push_str("\nfn __staleInstance(run: __Run, id: Int, seq: Int) -> Bool\n    ? \"Why an answer carrying this instance number changes nothing: it is not the number the slot under this id is waiting on.\"\n    seq != __current(run, id)\n");

    for protocol in protocols {
        let upper = marker_variant(&protocol.fn_name);
        out.push_str(&format!(
            "\nfn __settle{upper}(run: __Run, id: Int, seq: Int, outcome: {}) -> __Run\n    ? \"An answer for the current instance replaces that process's one slot and raises its number; an answer for an older instance changes nothing and is counted.\"\n    match seq == __current(run, id)\n        false -> __Run.update(run, dropped = run.dropped + 1)\n        true -> __settled{upper}(run, id, seq, outcome)\n",
            protocol.outcome
        ));
        out.push_str(&format!(
            "\nfn __settled{upper}(run: __Run, id: Int, seq: Int, outcome: {}) -> __Run\n    ? \"What an answer for the current instance leaves behind: an empty slot when the process is done, the next request under the next instance number otherwise.\"\n    match outcome\n        {}.Done(_) -> __Run.update(run, slots = Map.remove(run.slots, id))\n        {}.Waiting(request) -> __Run.update(run, slots = Map.set(run.slots, id, __settledSlot{upper}(seq, request)))\n",
            protocol.outcome, protocol.outcome, protocol.outcome
        ));
        out.push_str(&format!(
            "\nfn __settledSlot{upper}(seq: Int, request: {}) -> __Slot\n    ? \"The slot an answer for instance 'seq' writes back under that id: the next request of '{}', under the instance number after 'seq'.\"\n    __Slot(seq = __nextInstance(seq), pending = __Process.{upper}(request), waiting = Wait.Wake.NextTurn, due = 0, ms = 0)\n",
            protocol.request, protocol.fn_name
        ));
        out.push_str(&format!(
            "\nfn __settleKeepsOrShrinks{upper}(run: __Run, id: Int) -> Bool\n    ? \"Why settling never seats a second process: this id is already seated, so every arm either counts a late answer, or removes that one slot, or replaces it.\"\n    Map.has(run.slots, id)\n"
        ));
    }

    // ── The view ───────────────────────────────────────────────────
    out.push_str(&format!(
        "\nfn __view(run: __Run, ready: List<Int>) -> {view}\n    ? \"The resource-free summary of a run that the three policies read, and the only shape a law about them can sample. It is built again for every id the turn asks about, deliberately: a policy reads the seating this turn has already changed, not the seating the turn began with.\"\n    __viewOf(run, ready, {}, Map.keys(run.slots))\n",
        if has_jobs {
            format!("Map.len(run.{JOBS_FIELD})")
        } else {
            "0".to_string()
        }
    ));
    out.push_str(&format!(
        "\nfn __viewOf(run: __Run, ready: List<Int>, jobs: Int, ids: List<Int>) -> {view}\n    ? \"The view, once the turn has counted the jobs it is running and the ids it is summarising. The ids are handed in rather than read twice, so a program that asks for a loop is never warned about a repetition it did not write.\"\n    {view}(pending = __pendingOf(run, ids, {{}}), ready = ready, askable = __askableOf(run, ready, ids, []), jobs = jobs, room = __maxJobs() - jobs, stopping = run.stopping)\n"
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

    // ── The gate: which slots this turn may ask ────────────────────
    out.push_str("\nfn __askableOf(run: __Run, ready: List<Int>, ids: List<Int>, acc: List<Int>) -> List<Int>\n    ? \"The ids this turn may ask, in slot order. A wake gates the ask: the turn asks a slot when what that slot is waiting for has happened, and never otherwise.\"\n    match ids\n        [] -> acc\n        [id, ..rest] -> __askableOf(run, ready, rest, __askableAt(run, ready, id, acc))\n");
    out.push_str("\nfn __askableAt(run: __Run, ready: List<Int>, id: Int, acc: List<Int>) -> List<Int>\n    ? \"One id, kept when its wake has fired.\"\n    match __askable(run, ready, id)\n        false -> acc\n        true -> List.concat(acc, [id])\n");
    out.push_str("\nfn __askable(run: __Run, ready: List<Int>, id: Int) -> Bool\n    ? \"Whether the turn may ask the request seated under this id. Nothing is seated there, nothing to ask.\"\n    match Map.get(run.slots, id)\n        Option.None -> false\n        Option.Some(slot) -> __askableSlot(slot, ready, id, run.now)\n");
    out.push_str("\nfn __askableSlot(slot: __Slot, ready: List<Int>, id: Int, now: Int) -> Bool\n    ? \"What one wake gates. A request to be asked again next turn is askable at once, which is also where a freshly seated process and one whose request was just answered stand. A request parked on a socket or a job is asked in a turn whose wait reported its key, and false readiness is allowed: the module may answer Later again. A request parked on a deadline is asked once the turn's clock reading has reached it, or once that reading has fallen further back than the deadline asked for: a wall clock that steps backwards would otherwise leave the request waiting for a reading that never comes.\"\n    match slot.waiting\n        Wait.Wake.NextTurn -> true\n        Wait.Wake.Item(_) -> List.contains(ready, id)\n        Wait.Wake.After(_) -> Bool.or(slot.due <= now, now < slot.due - slot.ms)\n");

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
    out.push_str("\nfn __timeout(run: __Run) -> Int\n    ? \"How long this turn may wait: nothing at all while some request is to be asked again, the soonest deadline still ahead of the turn's clock reading when one is pending, and one second when neither.\"\n    __deadline(run, Map.keys(run.slots), 0 - 1)\n");
    out.push_str("\nfn __deadline(run: __Run, ids: List<Int>, best: Int) -> Int\n    ? \"The soonest deadline across every seated slot, or -1 when none carries one.\"\n    match ids\n        [] -> __tick(best)\n        [id, ..rest] -> __deadline(run, rest, __deadlineAt(run, id, best))\n");
    out.push_str("\nfn __deadlineAt(run: __Run, id: Int, best: Int) -> Int\n    ? \"What one id contributes to the turn's wait.\"\n    match Map.get(run.slots, id)\n        Option.None -> best\n        Option.Some(slot) -> __soonest(best, slot, run.now)\n");
    out.push_str("\nfn __soonest(best: Int, slot: __Slot, now: Int) -> Int\n    ? \"A request to be asked again next turn wins outright; two deadlines keep the nearer; a socket or a job carries none.\"\n    match slot.waiting\n        Wait.Wake.NextTurn -> 0\n        Wait.Wake.Item(_) -> best\n        Wait.Wake.After(_) -> __nearer(best, __remaining(slot.due, now, slot.ms))\n");
    out.push_str("\nfn __remaining(due: Int, now: Int, ms: Int) -> Int\n    ? \"How much of one deadline is left: never less than nothing, because a deadline the clock has already reached is asked again in this turn rather than waited on, and never more than the ms that was asked for, because a clock that stepped backwards would otherwise put the turn to sleep for longer than any request in the program asked for.\"\n    Int.min(Int.max(due - now, 0), ms)\n");
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
    for (protocol, performs) in protocols.iter().zip(process_effects) {
        out.push_str(&write_serve(protocol, answers, performs));
    }

    // ── The job seam ───────────────────────────────────────────────
    for job in jobs {
        out.push_str(&write_job(job, answers));
    }

    // ── The turn and the loop ──────────────────────────────────────
    out.push_str(&format!(
        "\nfn __turn(run: __Run) -> Result<__Run, String>\n    ? \"One turn: observe the stop flag, wait once, read the clock the wait came back at, serve the askable slots the policy admits, in its order{}. The clock is read after the wait and before the turn serves, so a deadline that fell due while the turn was waiting is askable in this turn rather than the next one; the wait of the turn after this one is measured against the same reading.\"\n{}    observed = __Run.update(run, stopping = Process.stopRequested())\n    ready = Wait.poll(__waitSet(observed, Map.keys(observed.slots), {{}}), __timeout(observed))?\n    timed = __Run.update(observed, now = Time.unixMs())\n    served = __serveEach(timed, ready, {}(__view(timed, ready)))\n{}",
        if has_jobs { ", take every job that finished, and start jobs while there is room" } else { "" },
        effects(turn_effects),
        bare(&plan.policies.order),
        if has_jobs {
            let job = &jobs[0];
            format!(
                "    taken = __takeEach{}(served, ready)\n    __startJobs{}(taken)\n",
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
        "\nfn __serveIf(run: __Run, ready: List<Int>, id: Int) -> __Run\n    ? \"One id, asked only when its wake has fired. The policy orders the whole view and is asked about the askable ids only; a slot that is not askable is not asked whatever the order said.\"\n{}    match __askable(run, ready, id)\n        false -> run\n        true -> __serveAdmitted(run, ready, id)\n",
        effects(serve_effects)
    ));
    out.push_str(&format!(
        "\nfn __serveAdmitted(run: __Run, ready: List<Int>, id: Int) -> __Run\n    ? \"One askable id, served only if the policy admits it. The view it is admitted against is taken again here, so a slot an earlier id of this turn removed is already gone from it.\"\n{}    match {}(__view(run, ready), id)\n        false -> run\n        true -> __serve(run, id)\n",
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
        effects(main_effects)
    ));
    if has_jobs {
        out.push_str(&format!(
            "\nfn __over(run: __Run) -> Result<Unit, String>\n    ? \"The run is over: every process that finished left its slot, whatever is still seated stays where it is, and every job still running is cancelled rather than abandoned.\"\n    ! [{CANCEL}]\n    __cancelEach(run, Map.keys(run.{JOBS_FIELD}))\n"
        ));
        out.push_str(&format!(
            "\nfn __cancelEach(run: __Run, keys: List<Int>) -> Result<Unit, String>\n    ? \"Every job the run still holds a handle for, in key order.\"\n    ! [{CANCEL}]\n    match keys\n        [] -> Result.Ok(Unit)\n        [key, ..rest] -> __cancelEach(__cancelOne(run, key), rest)\n"
        ));
        out.push_str(&format!(
            "\nfn __cancelOne(run: __Run, key: Int) -> __Run\n    ? \"One running job, cancelled and dropped from the table. A job that finished before the cancel is not this path's business; it shows up in take.\"\n    ! [{CANCEL}]\n    match Map.get(run.{JOBS_FIELD}, key)\n        Option.None -> run\n        Option.Some(job) -> __cancelled(run, key, {CANCEL}(job))\n"
        ));
        out.push_str(&format!(
            "\nfn __cancelled(run: __Run, key: Int, cancelled: Unit) -> __Run\n    ? \"The table once one job has been cancelled: the handle is gone, so nothing cancels it twice.\"\n    __Run.update(run, {JOBS_FIELD} = Map.remove(run.{JOBS_FIELD}, key))\n"
        ));
    } else {
        out.push_str("\nfn __over(run: __Run) -> Result<Unit, String>\n    ? \"The run is over: every process that finished left its slot, and whatever is still seated stays where it is.\"\n    Result.Ok(Unit)\n");
    }

    // ── The invariants ─────────────────────────────────────────────
    out.push_str(&write_laws(protocols));
    out
}

/// The dispatch of one process: one arm per request kind, and one function
/// per kind that reads the reply the answer module gave.
fn write_serve(
    protocol: &ProcessProtocol,
    answers: &[Answer],
    performs: &ProcessEffects,
) -> String {
    let upper = marker_variant(&protocol.fn_name);
    let list = |effects: &[String]| {
        if effects.is_empty() {
            String::new()
        } else {
            format!("    ! [{}]\n", effects.join(", "))
        }
    };
    let effects = list(&performs.serve);
    let mut out = format!(
        "\nfn __serve{upper}(run: __Run, id: Int, seq: Int, request: {}) -> __Run\n    ? \"One arm per request kind of '{}'. This is the match a program writes by hand today.\"\n{effects}    match request\n",
        protocol.request, protocol.fn_name
    );
    let mut bodies = String::new();
    for (kind, resumes) in protocol.kinds.iter().zip(&performs.kinds) {
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
            "\nfn __serve{upper}{}(run: __Run, id: Int, seq: Int, state: {}, answered: Tuple<{}, {reply}>) -> __Run\n    ? \"A Now settles the slot with the answer function of this kind; a Later keeps the state the module returned and parks the request, so a Later records the module's progress and leaves the request alone.\"\n{}    match answered\n        (__next, __reply) -> match __reply\n            {reply}.Later(__wake) -> __park(__Run.update(run, {} = __next), id, __wake)\n            {reply}.Now({now_binder}) -> __settle{upper}(__Run.update(run, {} = __next), id, seq, {resume})\n",
            kind.name,
            kind.state,
            answer.state,
            list(resumes),
            answer.field,
            answer.field
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
        "\nfn __takeEach{upper}(run: __Run, ready: List<Int>) -> __Run\n    ? \"A job outcome is a coordinator event, not an answer to a request: it goes to the answer state through the manifest's `landed` function and resumes nobody.\"\n    ! [{}.take]\n    match ready\n        [] -> run\n        [key, ..rest] -> __takeEach{upper}(__taken{upper}(run, key), rest)\n",
        job.capability
    ));
    out.push_str(&format!(
        "\nfn __taken{upper}(run: __Run, key: Int) -> __Run\n    ? \"One reported key: a key that is not a running job of this kind is not this seam's business. The job stays in the table until its own outcome says it is over, because a wait may report a job ready before it has finished.\"\n    ! [{}.take]\n    match Map.get(run.{JOBS_FIELD}, key)\n        Option.None -> run\n        Option.Some(job) -> __reported{upper}(run, key, {}.take(job))\n",
        job.capability, job.capability
    ));
    out.push_str(&format!(
        "\nfn __reported{upper}(run: __Run, key: Int, taken: Result<Option<{}>, String>) -> __Run\n    ? \"What one take answered. A job that was cancelled, whose body stopped, or whose id the engine has forgotten will never land a payload: it leaves the table and reaches `landed` as the error it is, and the run goes on.\"\n    match taken\n        Result.Err(reason) -> __landed{upper}(run, key, Result.Err(reason))\n        Result.Ok(payload) -> __finished{upper}(run, key, payload)\n",
        job.payload_type
    ));
    out.push_str(&format!(
        "\nfn __finished{upper}(run: __Run, key: Int, payload: Option<{}>) -> __Run\n    ? \"A job reported ready that has not finished answers nothing, and keeps both its handle and the run it was taken from, so a later turn collects it.\"\n    match payload\n        Option.None -> run\n        Option.Some(value) -> __landed{upper}(run, key, Result.Ok(value))\n",
        job.payload_type
    ));
    out.push_str(&format!(
        "\nfn __landed{upper}(run: __Run, key: Int, outcome: Result<{}, String>) -> __Run\n    ? \"Where a job that is over goes: the `landed` function of the answer state, which resumes nobody. The handle leaves the table either way, because this job has no second outcome to give.\"\n    __Run.update(run, {JOBS_FIELD} = Map.remove(run.{JOBS_FIELD}, key), {landed_field} = {}(run.{landed_field}, outcome))\n",
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
/// here. I4 is now about the request rather than the state: a `Later` keeps
/// whatever state the answer module returned, because that is where a module
/// records its own progress, and what it leaves alone is the request — the
/// same instance number and the same request value. The two laws below say
/// exactly that, one for the instance and one for the request. I1 is the size
/// comparison the proposal expected to stay open, written with its `because`
/// so the report can say where it stands.
fn write_laws(protocols: &[ProcessProtocol]) -> String {
    let mut out = String::new();
    // The sample seats each process at the request it re-enters itself with.
    // A process takes no parameters, so that request carries nothing, which
    // is what makes the sample — and every law below it — pure even when the
    // process performs something in place on its way to its first stop.
    let mut sampled: Vec<(&ProcessProtocol, String)> = Vec::new();
    for protocol in protocols {
        let Some(kind) = protocol.kinds.iter().find(|kind| kind.operation.is_none()) else {
            continue;
        };
        let Some((variant, _)) = kind.variants.iter().find(|(_, arity)| *arity == 0) else {
            continue;
        };
        let upper = marker_variant(&protocol.fn_name);
        let waiting = format!(
            "{}.Waiting({}.{}({}.{variant}))",
            protocol.outcome, protocol.request, kind.name, kind.state
        );
        out.push_str(&format!(
            "\nfn __sampleSeat{upper}(run: __Run) -> __Run\n    ? \"The '{}' process, seated at the request it re-enters itself with. Pure, so the laws below can sample it.\"\n    __seated{upper}(run, {waiting})\n",
            protocol.fn_name
        ));
        sampled.push((protocol, waiting));
    }
    let seated = sampled
        .iter()
        .fold("__fresh()".to_string(), |inner, (protocol, _)| {
            format!("__sampleSeat{}({inner})", marker_variant(&protocol.fn_name))
        });
    out.push_str(&format!(
        "\nfn __sampleRun() -> __Run\n    ? \"One run with one of every process seated: the sample the laws below are stated over.\"\n    {seated}\n"
    ));
    out.push_str("\nfn __slotIsSeated(run: __Run, id: Int) -> Bool\n    ? \"Why a Later moves the request nowhere: the slot written back is the slot that was already there, carrying the instance number and the request it already had.\"\n    Map.has(run.slots, id)\n");
    // One slot a law can write down: the first sampled process's own next
    // request, as the slot an answer for instance 1 wrote back.
    let sample_slot = sampled.first().and_then(|(protocol, _)| {
        let request = sample_request(protocol)?;
        Some(format!(
            "__settledSlot{}(1, {request})",
            marker_variant(&protocol.fn_name)
        ))
    });
    if let Some(sample) = &sample_slot {
        out.push_str(&format!(
            "\nfn __sampleSlot() -> __Slot\n    ? \"One seated slot the laws below sample: the next request of the first process this program writes.\"\n    {sample}\n"
        ));
    }

    let ids: Vec<String> = (1..=sampled.len() + 1).map(|id| id.to_string()).collect();
    let wake_domain = "    given wake: Wait.Wake = [Wait.Wake.NextTurn, Wait.Wake.After(5)]\n";
    let park_domain = format!(
        "    given run: __Run = [__sampleRun()]\n    given id: Int = [{}]\n{wake_domain}    when Map.has(run.slots, id)\n    because __slotIsSeated(run, id)\n",
        ids.join(", ")
    );
    out.push_str(&format!(
        "\nverify __park law laterKeepsTheInstance\n{park_domain}    __current(__park(run, id, wake), id) => __current(run, id)\n"
    ));
    if sample_slot.is_some() {
        out.push_str(&format!(
            "\nverify __parked law laterKeepsTheRequest\n    given slot: __Slot = [__sampleSlot()]\n{wake_domain}    given now: Int = [0, 7]\n    __parked(slot, wake, now).pending => slot.pending\n"
        ));
        // The wake gates the ask, and the half a law can carry is the
        // deadline: an `Int` comparison the wall closes over a sampled
        // domain. The socket-and-job half is `List.contains(ready, id)`,
        // which says nothing a law can check without the wait itself.
        out.push_str("\nfn __parkedAfter(slot: __Slot, now: Int) -> Bool\n    ? \"Why a parked request is not asked yet: it is waiting on a deadline this clock reading has not reached, and the reading has not fallen back behind the moment the request was parked either.\"\n    match slot.waiting\n        Wait.Wake.NextTurn -> false\n        Wait.Wake.Item(_) -> false\n        Wait.Wake.After(_) -> Bool.and(now < slot.due, now >= slot.due - slot.ms)\n");
        out.push_str(&format!(
            "\nverify __askableSlot law aDeadlineGatesTheAsk\n    given slot: __Slot = [__parked(__sampleSlot(), Wait.Wake.After(5), 0), __parked(__sampleSlot(), Wait.Wake.NextTurn, 0)]\n    given ready: List<Int> = [[], [{}]]\n    given id: Int = [{}]\n    given now: Int = [0, 1, 4, 5, 9]\n    when __parkedAfter(slot, now)\n    because __parkedAfter(slot, now)\n    __askableSlot(slot, ready, id, now) => false\n",
            ids.join(", "),
            ids.join(", ")
        ));
        // The other side of the same gate: a reading that has fallen back
        // further than the request asked for makes the slot askable again.
        // Without this the branch that keeps a backwards clock from
        // stranding a request is asserted by no law at all — the law above
        // guards it out, because its premise is exactly its negation.
        out.push_str("\nfn __steppedBack(slot: __Slot, now: Int) -> Bool\n    ? \"Why a parked request is asked again although its deadline has not been reached: the clock reading has fallen back behind the moment the request was parked, so the reading the deadline was set against will never come.\"\n    match slot.waiting\n        Wait.Wake.NextTurn -> false\n        Wait.Wake.Item(_) -> false\n        Wait.Wake.After(_) -> now < slot.due - slot.ms\n");
        out.push_str(&format!(
            "\nverify __askableSlot law aBackwardsClockNeverStrandsARequest\n    given slot: __Slot = [__parked(__sampleSlot(), Wait.Wake.After(5), 0), __parked(__sampleSlot(), Wait.Wake.NextTurn, 0)]\n    given ready: List<Int> = [[], [{}]]\n    given id: Int = [{}]\n    given now: Int = [0 - 5000, 0 - 1, 0, 4]\n    when __steppedBack(slot, now)\n    because __steppedBack(slot, now)\n    __askableSlot(slot, ready, id, now) => true\n",
            ids.join(", "),
            ids.join(", ")
        ));
    }
    out.push_str("\nverify __nextInstance law theNextInstanceIsHigher\n    given seq: Int = [0, 1, 7]\n    __nextInstance(seq) > seq holds\n");
    // The wait a deadline contributes is never longer than the deadline asked
    // for. That is the half of the backwards-clock story a law can carry: a
    // reading that jumped back makes `due - now` larger than the request, and
    // the turn would sleep past every other deadline in the program.
    out.push_str("\nverify __remaining law theWaitNeverExceedsTheRequest\n    given due: Int = [0, 5, 50]\n    given now: Int = [0 - 5000, 0, 5, 50]\n    given ms: Int = [0, 5, 50]\n    __remaining(due, now, ms) <= ms holds\n");
    // I3 per call, in the two halves the wall can carry. The first is about
    // the slot table's contents: the slot an answer for the current instance
    // writes back under that id carries a strictly higher instance number
    // than the one it answered, so the instance just answered is never
    // current again and a second answer to it is the late answer the two
    // laws above drop. The second is the read-back: the slot written under an
    // id is the slot read from it, which is the one-key `Map.set` fact.
    for (protocol, _) in &sampled {
        let upper = marker_variant(&protocol.fn_name);
        let Some(request) = sample_request(protocol) else {
            continue;
        };
        out.push_str(&format!(
            "\nverify __settledSlot{upper} law nowRaisesTheInstance\n    given seq: Int = [0, 1, 7]\n    given request: {} = [{request}]\n    __settledSlot{upper}(seq, request).seq > seq holds\n",
            protocol.request
        ));
    }
    if sample_slot.is_some() {
        out.push_str(&format!(
            "\nverify __current law theSlotWrittenIsTheSlotRead\n    given slots: Map<Int, __Slot> = [{{}}, __sampleRun().slots]\n    given id: Int = [{}]\n    given slot: __Slot = [__sampleSlot()]\n    Map.get(Map.set(slots, id, slot), id) => Option.Some(slot)\n",
            ids.join(", ")
        ));
    }
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

/// One request of a process that a law can write down: the one it re-enters
/// itself with, which carries nothing but the segment it resumes at.
fn sample_request(protocol: &ProcessProtocol) -> Option<String> {
    let kind = protocol
        .kinds
        .iter()
        .find(|kind| kind.operation.is_none())?;
    let (variant, _) = kind.variants.iter().find(|(_, arity)| *arity == 0)?;
    Some(format!(
        "{}.{}({}.{variant})",
        protocol.request, kind.name, kind.state
    ))
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

/// What the generated functions of one process perform, each one carrying its
/// own path rather than the program's.
///
/// Decision 4's rule is per segment — a generated protocol function carries
/// exactly the unmarked operations on its own segment's path — and the loop's
/// own functions are held to the same rule: seating a process performs what
/// that process performs on its way to its first request, and serving one
/// request kind performs what the answer module performs plus what the
/// segment the answer resumes performs. A process that touches nothing keeps
/// every function the loop generates for it pure, however loud its neighbour
/// is, so one generative effect anywhere does not oracle-lift the whole loop.
struct ProcessEffects {
    /// What `__seat<P>` performs: the start function's own segment.
    seat: Vec<String>,
    /// What `__serve<P>` performs: every kind of this process.
    serve: Vec<String>,
    /// What `__serve<P><Kind>` performs, aligned with `protocol.kinds`: the
    /// answer function of that kind, because its caller performed the answer
    /// module's operation already.
    kinds: Vec<Vec<String>>,
}

fn sorted(found: std::collections::BTreeSet<String>) -> Vec<String> {
    found.into_iter().collect()
}

/// One entry per process, in the order the protocols are given.
fn process_effect_lists(
    protocols: &[ProcessProtocol],
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
    protocols
        .iter()
        .map(|protocol| {
            let kinds: Vec<Vec<String>> = protocol
                .kinds
                .iter()
                .map(|kind| declared(&kind.answer_fn))
                .collect();
            let mut serve: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
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

/// What the whole serve path performs: the dispatch reaches every process, so
/// `__serve`, `__serveSlot`, `__serveEach`, `__serveIf` and `__serveAdmitted`
/// carry the union of what the processes carry — and nothing else.
fn serve_effect_list(effects: &[ProcessEffects]) -> Vec<String> {
    sorted(
        effects
            .iter()
            .flat_map(|process| process.serve.iter().cloned())
            .collect(),
    )
}

/// What a whole turn performs: the stop observation, the clock reading the
/// wake gate is measured against, the one wait, the serve path, and both ends
/// of every job kind.
fn turn_effect_list(serve: &[String], jobs: &[Job]) -> Vec<String> {
    let mut found: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    found.insert("Process.stopRequested".to_string());
    found.insert("Time.unixMs".to_string());
    found.insert("Wait.poll".to_string());
    found.extend(serve.iter().cloned());
    for job in jobs {
        found.insert(format!("{}.begin", job.capability));
        found.insert(format!("{}.take", job.capability));
    }
    found.into_iter().collect()
}

/// What the generated entry point performs: it seats every process, turns,
/// and cancels whatever job is still running when the run is over.
fn main_effect_list(turn: &[String], effects: &[ProcessEffects], jobs: &[Job]) -> Vec<String> {
    let mut found: std::collections::BTreeSet<String> = turn.iter().cloned().collect();
    for process in effects {
        found.extend(process.seat.iter().cloned());
    }
    if !jobs.is_empty() {
        found.insert(CANCEL.to_string());
    }
    found.into_iter().collect()
}
