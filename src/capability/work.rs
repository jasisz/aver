//! The Work shape: the one capability shape whose provider is a function of
//! the program rather than a host package.
//!
//! A job kind is an ordinary program capability that names the stdlib job
//! handle `Work.Job` at its boundary. Doing so fixes its whole surface: it
//! declares exactly `begin(task: T) -> Result<Work.Job, String>` and
//! `take(job: Work.Job) -> Result<Option<R>, String>`, and `aver.toml` binds
//! it with `work = "Module.function"` to a pure function `(T) -> R` of the
//! same program. Both halves are checked here so `aver run`, `aver check`,
//! `aver verify` and `aver compile` agree about what a job kind means.

use std::collections::{BTreeMap, BTreeSet};

use crate::ast::Type;
use crate::capability::CapabilityRegistry;
use crate::config::{ProviderAnswerBinding, ProviderWorkBinding};

use super::validation::canonicalize_type_names;

/// The stdlib module that owns the running-job handle.
pub const WORK_MODULE: &str = "Work";

/// The stdlib module that owns the one wait of a turn.
pub const WAIT_MODULE: &str = "Wait";

/// The canonical name of the running-job handle.
pub const WORK_JOB: &str = "Work.Job";

/// The canonical name of the one operation the job capability owns.
pub const WORK_CANCEL: &str = "Work.cancel";

/// The canonical name of the one wait of a turn.
pub const WAIT_POLL: &str = "Wait.poll";

/// The checked surface of one job-kind capability.
#[derive(Debug, Clone, PartialEq)]
pub struct WorkShape {
    /// Capability module, e.g. `Validation`.
    pub capability: String,
    /// The `begin` parameter type, canonicalised to the capability's scope.
    pub task: Type,
    /// The `take` payload type, canonicalised to the capability's scope.
    pub payload: Type,
}

/// One job kind as the wasm backends need it: the checked shape, the two
/// operation names a call site spells, and the program function the manifest
/// bound to run it.
///
/// A job kind is answered by the program on every backend, so it is never a
/// host import, a WIT interface or a host adapter entry. The wasm plans carry
/// it instead: `CapabilityWasmGcPlan` and `CapabilityWitPlan` already reach
/// `emit_module_with`, and this is what the emitter reads to lower `begin`,
/// `take` and the handle they mint inline.
///
/// `function` is `None` until the manifest is read — `build` sees only the
/// contracts — and a program that ever runs a job has it filled in by
/// `bind_work_functions`, because `check_bindings` refuses a job kind with no
/// binding at the program door.
#[derive(Debug, Clone, PartialEq)]
pub struct JobKindPlan {
    /// The checked shape: capability, task type and payload type.
    pub shape: WorkShape,
    /// The operation that starts a job, `Validation.begin`, verbatim.
    pub begin: crate::capability::CapabilityOperation,
    /// The operation that collects one, `Validation.take`, verbatim.
    pub take: crate::capability::CapabilityOperation,
    /// `work = "Node.validate"`, the pure function of the program one job runs.
    pub function: Option<String>,
}

impl JobKindPlan {
    /// The plan for one checked job-kind shape, before the manifest is read.
    ///
    /// `None` when the registry no longer holds both operations, which
    /// `check_shape` has already accepted, so this is a defensive `Option`
    /// rather than a case a program reaches.
    pub fn new(registry: &CapabilityRegistry, shape: WorkShape) -> Option<Self> {
        let begin = registry
            .operation(&format!("{}.begin", shape.capability))?
            .clone();
        let take = registry
            .operation(&format!("{}.take", shape.capability))?
            .clone();
        Some(Self {
            shape,
            begin,
            take,
            function: None,
        })
    }

    /// Type spellings the wasm type registry has to carry for this job kind.
    ///
    /// The task and payload types can be spelled nowhere else in the program
    /// — a `begin` whose result is matched in place never annotates either —
    /// so the registry would allocate no slot for the very values the inline
    /// lowering builds. These are the spellings it needs.
    pub fn boundary_type_strings(&self) -> Vec<String> {
        let payload = self.shape.payload.display();
        vec![
            self.shape.task.display(),
            payload.clone(),
            format!("Option<{payload}>"),
            format!("Result<Option<{payload}>,String>"),
            format!("Result<{WORK_JOB},String>"),
        ]
    }
}

/// Fill each job kind's `work = "Module.function"` in from the manifest.
///
/// Kept beside the plan so both wasm plans bind a job kind the same way.
pub fn bind_work_functions(
    kinds: &mut [JobKindPlan],
    bindings: &[crate::config::ProviderWorkBinding],
) {
    for kind in kinds {
        if let Some(binding) = bindings
            .iter()
            .find(|binding| binding.capability == kind.shape.capability)
        {
            kind.function = Some(binding.function.clone());
        }
    }
}

/// Whether a finding blocks the program or only tells its author something.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkSeverity {
    Error,
    Warning,
}

impl WorkSeverity {
    fn label(self) -> &'static str {
        match self {
            WorkSeverity::Error => "error",
            WorkSeverity::Warning => "warning",
        }
    }
}

/// One work diagnostic: the stable slug plus the sentence a front door prints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkDiagnostic {
    pub slug: &'static str,
    pub severity: WorkSeverity,
    pub message: String,
}

impl WorkDiagnostic {
    fn new(slug: &'static str, message: String) -> Self {
        Self {
            slug,
            severity: WorkSeverity::Error,
            message,
        }
    }

    fn warning(slug: &'static str, message: String) -> Self {
        Self {
            slug,
            severity: WorkSeverity::Warning,
            message,
        }
    }

    pub fn is_error(&self) -> bool {
        self.severity == WorkSeverity::Error
    }

    /// The one-line form every text front door prints.
    pub fn rendered(&self) -> String {
        format!("{}[{}]: {}", self.severity.label(), self.slug, self.message)
    }
}

pub const WORK_SHAPE: &str = "work-shape";
pub const WORK_BINDING: &str = "work-binding";
pub const WORK_TARGET: &str = "work-target";
pub const ANSWER_SHAPE: &str = "answer-shape";
pub const ANSWER_BINDING: &str = "answer-binding";
pub const INTERCEPT_OUTSIDE_YIELD: &str = "intercept-outside-yield";

/// Every capability of `registry` that names `Work.Job` at its boundary,
/// paired with its checked shape or the `work-shape` diagnostic it fails.
///
/// Naming the handle is what makes a capability a job kind: no separate
/// keyword, and no way to half-declare one.
pub fn job_kinds(
    registry: &CapabilityRegistry,
) -> Vec<(String, Result<WorkShape, WorkDiagnostic>)> {
    let mut resources = registry
        .resource_tainted_types()
        .cloned()
        .collect::<BTreeSet<_>>();
    resources.extend(
        crate::stdlib::embedded_capability_resources()
            .iter()
            .cloned(),
    );

    let mut modules = BTreeSet::new();
    for operation in registry.operations() {
        if operation.module == WORK_MODULE || operation.module == WAIT_MODULE {
            continue;
        }
        let mentions = operation
            .params
            .iter()
            .any(|(_, ty)| mentions_job(ty, &operation.module))
            || mentions_job(&operation.return_type, &operation.module);
        if mentions {
            modules.insert(operation.module.clone());
        }
    }

    modules
        .into_iter()
        .map(|module| {
            let outcome = check_shape(registry, &module, &resources);
            (module, outcome)
        })
        .collect()
}

/// Whether `module` is a job kind of `registry`.
///
/// Naming `Work.Job` at the boundary is the whole test, so this is the same
/// question `job_kinds` answers, asked about one capability.
pub fn is_job_kind(registry: &CapabilityRegistry, module: &str) -> bool {
    job_kinds(registry)
        .iter()
        .any(|(declared, _)| declared == module)
}

fn check_shape(
    registry: &CapabilityRegistry,
    module: &str,
    resources: &BTreeSet<String>,
) -> Result<WorkShape, WorkDiagnostic> {
    let operations = registry
        .operations()
        .filter(|operation| operation.module == module)
        .collect::<Vec<_>>();
    let names = operations
        .iter()
        .map(|operation| operation.name.clone())
        .collect::<Vec<_>>();
    if names != ["begin", "take"] {
        return Err(shape_error(format!(
            "capability '{module}' names the job handle {WORK_JOB}, so it is a job kind and must declare exactly two operations, `begin` and `take`; it declares [{}]",
            names.join(", ")
        )));
    }
    let begin = operations[0];
    let take = operations[1];

    let job = Type::named(WORK_JOB.to_string());
    let string = Type::Str;

    if begin.params.len() != 1 {
        return Err(shape_error(format!(
            "operation '{module}.begin' must take exactly one task parameter; it takes {}",
            begin.params.len()
        )));
    }
    let task = canonicalize_type_names(begin.params[0].1.clone(), module);
    let expected_begin = Type::Result(Box::new(job.clone()), Box::new(string.clone()));
    let actual_begin = canonicalize_type_names(begin.return_type.clone(), module);
    if actual_begin != expected_begin {
        return Err(shape_error(format!(
            "operation '{module}.begin' must return {}; it returns {}",
            expected_begin.display(),
            actual_begin.display()
        )));
    }
    if let Some(offender) = resource_in(&task, resources) {
        return Err(shape_error(format!(
            "operation '{module}.begin' takes a task containing capability resource '{offender}'; a job task is ordinary data the runtime can carry off the turn"
        )));
    }

    if take.params.len() != 1 || canonicalize_type_names(take.params[0].1.clone(), module) != job {
        return Err(shape_error(format!(
            "operation '{module}.take' must take exactly one {WORK_JOB} parameter"
        )));
    }
    let actual_take = canonicalize_type_names(take.return_type.clone(), module);
    let payload = match &actual_take {
        Type::Result(ok, err) if **err == string => match &**ok {
            Type::Option(payload) => (**payload).clone(),
            _ => {
                return Err(take_shape_error(module, &actual_take));
            }
        },
        _ => return Err(take_shape_error(module, &actual_take)),
    };
    if let Some(offender) = resource_in(&payload, resources) {
        return Err(shape_error(format!(
            "operation '{module}.take' yields a result containing capability resource '{offender}'; a job result is ordinary data the runtime can carry back onto the turn"
        )));
    }

    Ok(WorkShape {
        capability: module.to_string(),
        task,
        payload,
    })
}

fn shape_error(message: String) -> WorkDiagnostic {
    WorkDiagnostic::new(WORK_SHAPE, message)
}

fn take_shape_error(module: &str, actual: &Type) -> WorkDiagnostic {
    shape_error(format!(
        "operation '{module}.take' must return Result<Option<R>, String> for a result type R; it returns {}",
        actual.display()
    ))
}

/// One target the program can be prepared for.
///
/// Every target answers a capability the program answers itself: the state
/// types, the reply sums and the pure answer functions the lowering leaves
/// behind are ordinary data and ordinary functions, and the two wasm targets
/// gained the representations those reply sums reach — the job handle
/// `Work.Job`, equality on a sum that carries it, and a `Unit` variant field.
///
/// A job itself is the narrower question: `begin` and `take` are answered by
/// a function of the program, and the one wait that watches a job is
/// `Wait.poll`. Both run on the VM and the Rust backend in this build.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WorkTarget {
    Vm,
    Rust,
    WasmGc,
    Wasip2,
}

impl WorkTarget {
    fn label(self) -> &'static str {
        match self {
            WorkTarget::Vm => "vm",
            WorkTarget::Rust => "rust",
            WorkTarget::WasmGc => "wasm-gc",
            WorkTarget::Wasip2 => "wasip2",
        }
    }

    /// Whether this target runs jobs and the wait that watches them.
    ///
    /// All four do. The VM and the Rust backend run a job beside the turn on
    /// a thread; wasm-gc and wasip2 run it inline at `begin`, because a
    /// component and a wasm-gc module are single-threaded. The semantics are
    /// the same on all four — only the wall clock differs.
    fn runs_jobs(self) -> bool {
        true
    }
}

/// Resolved signature of one function of the program: parameters, result and
/// declared effects, exactly as `TypeCheckResult::fn_sigs` carries them.
pub type FnSignature = (Vec<Type>, Type, Vec<String>);

/// The one gate where a program's job kinds meet its manifest bindings.
///
/// `entry_module` is the module the command was pointed at: a capability
/// module checked on its own still declares its own shape, but it is not yet
/// a program, so it does not need a binding.
pub fn gate(
    registry: &CapabilityRegistry,
    manifest: Option<&crate::config::ProviderPackageManifest>,
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    entry_module: Option<&str>,
    target: WorkTarget,
) -> Vec<WorkDiagnostic> {
    let mut errors = Vec::new();
    let mut shapes = Vec::new();
    let mut declared = BTreeSet::new();
    for (module, outcome) in job_kinds(registry) {
        declared.insert(module);
        match outcome {
            Ok(shape) => shapes.push(shape),
            Err(error) => errors.push(error),
        }
    }
    let work_bindings = manifest
        .map(|manifest| manifest.work_bindings.as_slice())
        .unwrap_or(&[]);
    let answer_bindings = manifest
        .map(|manifest| manifest.answer_bindings.as_slice())
        .unwrap_or(&[]);
    let (answers, answer_errors) = check_answers(registry, answer_bindings, fn_sigs, entry_module);
    errors.extend(answer_errors);
    errors.extend(requests_outside_a_process(registry, &answers, fn_sigs));
    errors.extend(check_bindings(
        registry,
        &shapes,
        &declared,
        ManifestBindings {
            work: work_bindings,
            answer: answer_bindings,
        },
        &answers,
        fn_sigs,
        entry_module,
    ));
    // A capability the program answers is not refused on any target: the
    // lowering leaves behind data and pure functions, and the wasm targets
    // represent every type those reach. Only the two questions below are
    // still the VM's and the Rust backend's alone.
    //
    // The first of the two is a backstop, not the refusal a user reads. A
    // program that performs `Wait.poll` or `Work.cancel` and declares no job
    // kind is stopped before this gate by `capability_target_rejection` in
    // `src/main/commands.rs`, which reports
    // `error[capability-target-unsupported]` with
    // `reason[standard-binding-unavailable]`, because the target manifest
    // binds neither reserved contract on either wasm target. That earlier
    // refusal is the one `docs/diagnostics-slugs.md` sends such a user to.
    // This branch stands for the day a wasm target binds part of `Wait` or
    // `Work` while jobs themselves are still the VM's.
    //
    // TODO(owner): jasisz/aver#1329 — decisions 1, 3, 4 and 6 of
    // `prompts/wasm-inline-jobs-brief.md` (a job run inline at `begin`,
    // `Wait.poll` over `Socket` and `Job` items, `Work.cancel`, and the
    // recording replayed without recomputation) are not built. Both
    // refusals below stand until they are, and decision 5 — this gate
    // accepting all four targets — waits on them.
    if !target.runs_jobs()
        && shapes.is_empty()
        && let Some(reserved) = reserved_contract_performed(registry, fn_sigs)
    {
        errors.push(WorkDiagnostic::new(WORK_TARGET, format!(
            "The one wait of a turn runs on the VM and the Rust backend in this build; the wasm-gc and wasip2 backends follow in a later change. The program performs an operation of '{}', a reserved contract neither wasm target binds, and the requested target is {}.",
            reserved,
            target.label()
        )));
    }
    if !shapes.is_empty() && !target.runs_jobs() {
        errors.push(WorkDiagnostic::new(WORK_TARGET, format!(
            "A job kind runs on the VM and the Rust backend in this build; the wasm-gc and wasip2 backends follow in a later change. '{}' is a job kind and the requested target is {}",
            shapes[0].capability,
            target.label()
        )));
    }
    errors
}

/// An operation of an answered capability performed outside a process.
///
/// A marked operation is a request, and a request is only a request inside a
/// function whose effect list names `yield`: the lowering cuts such a
/// function at it and the operation disappears from every effect list it
/// leaves behind. So a marked operation still standing in a declared effect
/// list after the lowering is a call from a plain function, and it has no
/// answer — no provider is bound to the capability, and no request kind was
/// generated for it. Saying so here is what makes proposal §2.4's "an
/// unanswered request kind is impossible by construction" true.
fn requests_outside_a_process(
    registry: &CapabilityRegistry,
    answers: &[AnswerShape],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
) -> Vec<WorkDiagnostic> {
    let mut answered: BTreeMap<&str, &str> = BTreeMap::new();
    for shape in answers {
        for capability in &shape.capabilities {
            answered.insert(capability.as_str(), shape.module.as_str());
        }
    }
    let mut findings = Vec::new();
    for name in program_functions(registry, fn_sigs) {
        // A generated function is nobody's to fix. It carries what the plain
        // function it calls declares, and that function is where this is
        // reported.
        if name
            .rsplit('.')
            .next()
            .is_some_and(|bare| bare.starts_with("__"))
        {
            continue;
        }
        let (_, _, effects) = &fn_sigs[name];
        // A process is where a request belongs. One that did not lower has
        // its own error from the lowering, and this is not a second one.
        if effects
            .iter()
            .any(|effect| effect == crate::yield_lowering::YIELD_EFFECT)
        {
            continue;
        }
        for effect in effects {
            let Some((capability, operation)) = effect.rsplit_once('.') else {
                continue;
            };
            let Some(module) = answered.get(capability) else {
                continue;
            };
            findings.push(WorkDiagnostic::new(
                INTERCEPT_OUTSIDE_YIELD,
                format!(
                    "'{effect}' is answered by this program — aver.toml binds capability \"{capability}\" to module '{module}' — so it is a request, and a request is only legal in a function whose effect list names `yield`. Add `yield` to '{name}', or call '{module}.{operation}({})' directly if what you wanted was the answer itself",
                    answer_call_arguments(registry, capability, operation)
                ),
            ));
        }
    }
    findings
}

/// The functions the program itself writes, in name order: everything in the
/// signature map that is not a capability's own operation, counted once.
///
/// A capability declares `Pool.claim` and the map carries it as a function
/// whose only effect is itself; that is the declaration of a request, not a
/// call to one. The entry module's own functions are in the map twice, bare
/// and qualified, and the qualified name is the one that says where they are.
fn program_functions<'a>(
    registry: &CapabilityRegistry,
    fn_sigs: &'a std::collections::HashMap<String, FnSignature>,
) -> Vec<&'a String> {
    let mut names: Vec<&String> = fn_sigs.keys().collect();
    names.sort();
    let qualified: BTreeSet<&str> = names
        .iter()
        .filter_map(|name| name.rsplit_once('.'))
        .map(|(_, bare)| bare)
        .collect();
    names
        .into_iter()
        .filter(|name| match name.rsplit_once('.') {
            Some((module, _)) => registry.contract(module).is_none(),
            None => !qualified.contains(name.as_str()),
        })
        .collect()
}

/// How the answer function of one operation reads at a call site: the state
/// first, then the operation's own parameters by the names the capability
/// gave them.
fn answer_call_arguments(
    registry: &CapabilityRegistry,
    capability: &str,
    operation: &str,
) -> String {
    let mut names = vec!["state".to_string()];
    if let Some(declared) = operations_of(registry, capability)
        .into_iter()
        .find(|candidate| candidate.name == operation)
    {
        names.extend(declared.params.iter().map(|(name, _)| name.clone()));
    }
    names.join(", ")
}

/// The first reserved contract (`Wait`, `Work`) the program depends on, if any.
/// Cheap and wide: a front door reads it to decide whether the gate is worth
/// running at all, never to refuse a target.
pub fn reserved_contract_in_use(registry: &CapabilityRegistry) -> Option<&'static str> {
    crate::stdlib::RESERVED_CAPABILITY_MODULES
        .iter()
        .copied()
        .find(|module| registry.contract(module).is_some())
}

/// The first reserved contract the program performs an operation of, if any.
/// A program can use `Wait.poll` over sockets alone, without a job kind, and
/// no non-VM backend lowers it yet, so the target gate refuses that too.
///
/// A `depends` edge is not enough, and the difference is the whole of this
/// function: an answered capability's generated reply sums name `Wait.Wake`,
/// so every program that answers a capability depends on `Wait` while never
/// polling it. Such a program is data and pure functions, which every backend
/// compiles.
fn reserved_contract_performed(
    registry: &CapabilityRegistry,
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
) -> Option<&'static str> {
    crate::stdlib::RESERVED_CAPABILITY_MODULES
        .iter()
        .copied()
        .find(|module| {
            registry.contract(module).is_some() && performs_operation_of(registry, fn_sigs, module)
        })
}

/// Whether any function of the program declares an effect of `module`, as the
/// operation (`Wait.poll`) or as the bare namespace (`Wait`).
fn performs_operation_of(
    registry: &CapabilityRegistry,
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    module: &str,
) -> bool {
    let prefix = format!("{module}.");
    program_functions(registry, fn_sigs)
        .into_iter()
        .any(|name| {
            let (_, _, effects) = &fn_sigs[name];
            effects
                .iter()
                .any(|effect| effect == module || effect.starts_with(&prefix))
        })
}

/// The two binding kinds of the manifest that name a function or a module of
/// the program, carried together because the job seam is checked against both:
/// `work` says which function runs off the turn, `answer` which module holds
/// the state its two ends reach.
#[derive(Debug, Clone, Copy)]
pub struct ManifestBindings<'a> {
    pub work: &'a [ProviderWorkBinding],
    pub answer: &'a [ProviderAnswerBinding],
}

/// Check every job kind of the program against the manifest's `work` bindings
/// and the functions those bindings name. The returned strings are complete
/// `work-binding` diagnostics, in capability order.
pub fn check_bindings(
    registry: &CapabilityRegistry,
    shapes: &[WorkShape],
    declared: &BTreeSet<String>,
    bindings: ManifestBindings<'_>,
    answers: &[AnswerShape],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    entry_module: Option<&str>,
) -> Vec<WorkDiagnostic> {
    let mut errors = Vec::new();
    errors.extend(check_job_seam(
        shapes,
        bindings,
        answers,
        fn_sigs,
        entry_module,
    ));
    for binding in bindings.work {
        if registry.contract(&binding.capability).is_none() {
            continue;
        }
        // A capability that tried to be a job kind and failed its shape has
        // already been told so; do not also accuse it of being the wrong kind.
        if !declared.contains(&binding.capability) {
            errors.push(binding_error(format!(
                "aver.toml: [[providers.bindings]] index {} binds capability '{}' with `work`, but that capability is not a job kind; only a capability declaring `begin` and `take` over {WORK_JOB} can be answered by a function of the program",
                binding.index, binding.capability
            )));
        }
    }

    for shape in shapes {
        // A capability module checked or verified on its own is not yet a
        // program: it declares its own shape, and the binding belongs to
        // whoever runs it.
        if entry_module == Some(shape.capability.as_str()) {
            continue;
        }
        let Some(binding) = bindings
            .work
            .iter()
            .find(|binding| binding.capability == shape.capability)
        else {
            errors.push(binding_error(format!(
                "job kind '{0}' has no `work` binding; add one [[providers.bindings]] entry to aver.toml with capability = \"{0}\" and work = \"Module.function\" naming the pure function that runs the job",
                shape.capability
            )));
            continue;
        };
        // A job runs off the turn, and both runtimes reach its function
        // through the module that owns it: the VM asks the module table for
        // `Module.function`, and the generated crate calls the module's own
        // Rust path. The entry module is not in either — it is the unit the
        // command was pointed at, not a module of the program — so a binding
        // that names it is refused here, at the one door both backends read,
        // rather than failing as a runtime error on one and an uncompilable
        // crate on the other.
        if entry_module == Some(binding.module()) {
            errors.push(binding_error(format!(
                "job kind '{}' binds work = \"{}\", but '{}' is the entry module this command was pointed at; a job reaches its function through the module that owns it, so move that function into a module of its own and bind it there",
                shape.capability,
                binding.function,
                binding.module()
            )));
            continue;
        }
        let Some((params, result, effects)) = fn_sigs.get(&binding.function) else {
            errors.push(binding_error(format!(
                "job kind '{}' binds work = \"{}\", but this program has no function '{}'",
                shape.capability, binding.function, binding.function
            )));
            continue;
        };
        if !effects.is_empty() {
            errors.push(binding_error(format!(
                "job kind '{}' binds work = \"{}\", but that function declares effects [{}]; a job runs off the turn, so its function must be pure",
                shape.capability,
                binding.function,
                effects.join(", ")
            )));
            continue;
        }
        if params.len() != 1 {
            errors.push(binding_error(format!(
                "job kind '{}' binds work = \"{}\", which takes {} parameters; a bound function takes exactly the task",
                shape.capability,
                binding.function,
                params.len()
            )));
            continue;
        }
        // The bound function writes its own module's types bare; the
        // capability writes its own bare too. Canonicalise each side in the
        // scope that wrote it before the nominal comparison.
        let param = canonicalize_type_names(params[0].clone(), binding.module());
        let result = canonicalize_type_names(result.clone(), binding.module());
        if !same_type(&param, &shape.task) {
            errors.push(binding_error(format!(
                "job kind '{}' binds work = \"{}\", whose parameter is {}; '{}.begin' hands it {}",
                shape.capability,
                binding.function,
                param.display(),
                shape.capability,
                shape.task.display()
            )));
            continue;
        }
        if !same_type(&result, &shape.payload) {
            errors.push(binding_error(format!(
                "job kind '{}' binds work = \"{}\", which returns {}; '{}.take' yields {}",
                shape.capability,
                binding.function,
                result.display(),
                shape.capability,
                shape.payload.display()
            )));
        }
    }
    errors
}

/// Nominal identity for the binding comparison. Both sides are canonicalised
/// to the scope that wrote them first — the capability for the task and the
/// payload, the binding's module for the bound function — so a named type
/// carries the module that owns it and two same-named records of different
/// modules are two types, as they are everywhere else in the language.
fn same_type(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::Named { name: left, .. }, Type::Named { name: right, .. }) => left == right,
        (Type::Result(la, lb), Type::Result(ra, rb)) | (Type::Map(la, lb), Type::Map(ra, rb)) => {
            same_type(la, ra) && same_type(lb, rb)
        }
        (Type::Option(left), Type::Option(right))
        | (Type::List(left), Type::List(right))
        | (Type::Vector(left), Type::Vector(right)) => same_type(left, right),
        (Type::Tuple(left), Type::Tuple(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| same_type(left, right))
        }
        _ => left == right,
    }
}

fn mentions_job(ty: &Type, scope: &str) -> bool {
    match canonicalize_type_names(ty.clone(), scope) {
        Type::Named { name, .. } => name == WORK_JOB,
        Type::Result(left, right) | Type::Map(left, right) => {
            mentions_job(&left, scope) || mentions_job(&right, scope)
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            mentions_job(&inner, scope)
        }
        Type::Tuple(items) => items.iter().any(|item| mentions_job(item, scope)),
        Type::Fn(params, ret, _) => {
            params.iter().any(|param| mentions_job(param, scope)) || mentions_job(&ret, scope)
        }
        _ => false,
    }
}

fn resource_in(ty: &Type, resources: &BTreeSet<String>) -> Option<String> {
    match ty {
        Type::Named { name, .. } => resources.contains(name).then(|| name.clone()),
        Type::Result(left, right) | Type::Map(left, right) => {
            resource_in(left, resources).or_else(|| resource_in(right, resources))
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            resource_in(inner, resources)
        }
        Type::Tuple(items) => items.iter().find_map(|item| resource_in(item, resources)),
        Type::Fn(params, ret, _) => params
            .iter()
            .find_map(|param| resource_in(param, resources))
            .or_else(|| resource_in(ret, resources)),
        _ => None,
    }
}

fn binding_error(message: String) -> WorkDiagnostic {
    WorkDiagnostic::new(WORK_BINDING, message)
}

// ── Answered capabilities ───────────────────────────────────────────────
//
// The second shape whose provider is the program itself. A `work` binding
// says "the runtime answers this capability by running that pure function off
// the turn"; an `answer` binding says "the program answers it, inside the
// turn, from one state". Both are checked here so every front door agrees
// about what a binding means before anything is lowered against it.

/// The checked surface of one answer module.
#[derive(Debug, Clone, PartialEq)]
pub struct AnswerShape {
    /// The module named by `answer`, e.g. `Ledger`.
    pub module: String,
    /// The one state every answer function of this module threads, read off
    /// the first parameter of its first answer function exactly as
    /// [`WorkShape`] reads the task off `begin`.
    pub state: Type,
    /// The capabilities this module answers, in manifest order.
    pub capabilities: Vec<String>,
}

/// Every `answer = "Module"` binding of the manifest, checked against the
/// capability it marks and the module it names.
pub fn check_answers(
    registry: &CapabilityRegistry,
    answer_bindings: &[ProviderAnswerBinding],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    entry_module: Option<&str>,
) -> (Vec<AnswerShape>, Vec<WorkDiagnostic>) {
    let mut findings = Vec::new();
    let mut modules: Vec<(String, Vec<&ProviderAnswerBinding>)> = Vec::new();
    for binding in answer_bindings {
        // A capability module checked on its own is not yet a program: it
        // declares its own operations, and the module that answers them
        // belongs to whoever runs it.
        if entry_module == Some(binding.capability.as_str()) {
            continue;
        }
        if let Some(reason) = compiler_shipped_reason(&binding.capability) {
            findings.push(answer_binding_error(format!(
                "aver.toml: [[providers.bindings]] index {} binds capability '{}' with `answer`, but {reason}, and the generated turn calls it itself; only a capability this program declares may be answered by a module of it",
                binding.index, binding.capability
            )));
            continue;
        }
        // A manifest may carry bindings for capabilities this particular
        // program does not depend on; the `work` gate reads them the same way.
        if registry.contract(&binding.capability).is_none() {
            continue;
        }
        // An answer module is ordinary Aver that computes an answer. A
        // capability module declares operations and computes nothing, so
        // naming one — the answered capability itself, most of all — is the
        // mistake, and saying so is better than reading its operations as
        // answer functions that perform themselves.
        if registry.contract(&binding.module).is_some() {
            findings.push(answer_binding_error(format!(
                "aver.toml: [[providers.bindings]] index {} binds capability '{}' to answer = \"{}\", but '{}' is a capability module; an answer module is an ordinary module of the program that computes the answer",
                binding.index, binding.capability, binding.module, binding.module
            )));
            continue;
        }
        if !module_has_functions(fn_sigs, &binding.module) {
            findings.push(answer_binding_error(format!(
                "aver.toml: [[providers.bindings]] index {} binds capability '{}' to answer = \"{}\", but this program has no module '{}'",
                binding.index, binding.capability, binding.module, binding.module
            )));
            continue;
        }
        match modules
            .iter_mut()
            .find(|(module, _)| module == &binding.module)
        {
            Some((_, group)) => group.push(binding),
            None => modules.push((binding.module.clone(), vec![binding])),
        }
    }

    let mut shapes = Vec::new();
    for (module, group) in &modules {
        let (shape, module_findings) = check_answer_module(registry, module, group, fn_sigs);
        findings.extend(module_findings);
        if let Some(shape) = shape {
            shapes.push(shape);
        }
    }
    (shapes, findings)
}

fn check_answer_module(
    registry: &CapabilityRegistry,
    module: &str,
    group: &[&ProviderAnswerBinding],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
) -> (Option<AnswerShape>, Vec<WorkDiagnostic>) {
    let mut findings = Vec::new();
    let mut pairs: Vec<(&str, &crate::capability::CapabilityOperation)> = Vec::new();
    for binding in group {
        for operation in operations_of(registry, &binding.capability) {
            pairs.push((binding.capability.as_str(), operation));
        }
    }

    // One function per operation: two capabilities of one module cannot share
    // an operation name, because one function would have to answer both.
    let mut owners: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for (capability, operation) in &pairs {
        owners
            .entry(operation.name.as_str())
            .or_default()
            .push(capability);
    }
    let mut collided = BTreeSet::new();
    for (name, capabilities) in &owners {
        if capabilities.len() < 2 {
            continue;
        }
        collided.insert(*name);
        findings.push(answer_binding_error(format!(
            "module '{module}' answers '{}.{name}' and '{}.{name}' with one function '{module}.{name}'; rename one of the operations",
            capabilities[0], capabilities[1]
        )));
    }

    let (state, state_owner, state_finding) = module_state(module, &pairs, fn_sigs);
    findings.extend(state_finding);

    // A module answering several capabilities holds one state; a capability
    // whose own answer functions thread a different one is named as such
    // rather than told its every signature is wrong.
    let mut skipped = BTreeSet::new();
    if let (Some(state), Some(state_owner)) = (&state, state_owner) {
        for binding in group {
            if binding.capability == state_owner {
                continue;
            }
            let own = pairs
                .iter()
                .filter(|(capability, _)| *capability == binding.capability)
                .find_map(|(_, operation)| {
                    let (params, _, _) = fn_sigs.get(&answer_key(module, operation))?;
                    let first = params.first()?;
                    Some(canonicalize_type_names(first.clone(), module))
                });
            let Some(own) = own else { continue };
            if !same_type(&own, state) {
                findings.push(answer_binding_error(format!(
                    "module '{module}' answers '{state_owner}' with state {} and '{}' with state {}; one module answering several capabilities holds one state",
                    state.display(),
                    binding.capability,
                    own.display()
                )));
                skipped.insert(binding.capability.as_str());
            }
        }
    }

    for (capability, operation) in &pairs {
        if collided.contains(operation.name.as_str()) || skipped.contains(capability) {
            continue;
        }
        let key = answer_key(module, operation);
        let Some((params, result, effects)) = fn_sigs.get(&key) else {
            // A `yield` function is cut into generated pieces before any
            // signature is read, so its own name is gone by the time the gate
            // looks; the pieces it left behind are how the gate knows.
            if lowered_yield_function(fn_sigs, module, &operation.name) {
                findings.push(answer_shape_error(format!(
                    "aver.toml marks capability '{capability}' as answered by '{module}', and '{key}' declares `yield`; an answer is computed inside the turn, so it cannot itself be a process the turn has to drive"
                )));
                continue;
            }
            findings.push(answer_binding_error(format!(
                "capability '{capability}' is answered by module '{module}', so every one of its operations needs an answer function; this program has no function '{key}'"
            )));
            continue;
        };
        if effects
            .iter()
            .any(|effect| effect == crate::yield_lowering::YIELD_EFFECT)
        {
            findings.push(answer_shape_error(format!(
                "aver.toml marks capability '{capability}' as answered by '{module}', and '{key}' declares `yield`; an answer is computed inside the turn, so it cannot itself be a process the turn has to drive"
            )));
            continue;
        }
        if !effects.is_empty() {
            findings.push(answer_shape_warning(format!(
                "aver.toml marks capability '{capability}' as answered by '{module}', and '{key}' declares effects [{}]; an answer runs inside the turn, so this is allowed, but it can stall every other process",
                effects.join(", ")
            )));
        }
        let Some(state) = &state else { continue };
        let mut expected_params = vec![state.clone()];
        expected_params.extend(
            operation
                .params
                .iter()
                .map(|(_, ty)| canonicalize_type_names(ty.clone(), capability)),
        );
        let expected_result = Type::Tuple(vec![
            state.clone(),
            Type::named(reply_type_name(capability, &operation.name)),
        ]);
        let actual_params = params
            .iter()
            .map(|ty| canonicalize_type_names(ty.clone(), module))
            .collect::<Vec<_>>();
        let actual_result = canonicalize_type_names(result.clone(), module);
        let agrees = actual_params.len() == expected_params.len()
            && actual_params
                .iter()
                .zip(&expected_params)
                .all(|(left, right)| same_type(left, right))
            && same_type(&actual_result, &expected_result);
        if !agrees {
            findings.push(answer_binding_error(format!(
                "capability '{capability}' declares operation '{}', so '{key}' must be {}; it is {}",
                render_operation(capability, operation),
                render_signature(&expected_params, &expected_result),
                render_signature(&actual_params, &actual_result)
            )));
            continue;
        }
    }

    let shape = state.map(|state| AnswerShape {
        module: module.to_string(),
        state,
        capabilities: group
            .iter()
            .map(|binding| binding.capability.clone())
            .collect(),
    });
    (shape, findings)
}

/// The state a module threads, read off the first parameter of the first
/// answer function it actually declares.
///
/// The state is a type of the answering module, so a first parameter that is
/// anything else — an `Int`, or another module's record, which is what a
/// parameter order written the other way round looks like — is named as the
/// one mistake it is rather than inferred and then repeated as a wrong
/// expected signature under every operation.
fn module_state<'a>(
    module: &str,
    pairs: &[(&'a str, &crate::capability::CapabilityOperation)],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
) -> (Option<Type>, Option<&'a str>, Option<WorkDiagnostic>) {
    for (capability, operation) in pairs {
        let key = answer_key(module, operation);
        let Some((params, _, _)) = fn_sigs.get(&key) else {
            continue;
        };
        let Some(first) = params.first() else {
            return (
                None,
                None,
                Some(answer_binding_error(format!(
                    "'{key}' answers '{capability}.{}', so its first parameter is the state module '{module}' holds; it takes no parameters",
                    operation.name
                ))),
            );
        };
        let state = canonicalize_type_names(first.clone(), module);
        if !declared_by(&state, module) {
            return (
                None,
                None,
                Some(answer_binding_error(format!(
                    "'{key}' answers '{capability}.{}', so its first parameter is the state module '{module}' holds, a type that module declares; it is {}",
                    operation.name,
                    state.display()
                ))),
            );
        }
        return (Some(state), Some(capability), None);
    }
    (None, None, None)
}

/// Whether a canonicalised type is a named type of this module. A state is
/// one record of the answering module: `Ledger.State`, never `Int` and never
/// `Pool.Assignment`.
fn declared_by(ty: &Type, module: &str) -> bool {
    match ty {
        Type::Named { name, .. } => name
            .strip_prefix(module)
            .is_some_and(|rest| rest.starts_with('.')),
        _ => false,
    }
}

/// The two ends of a job kind's seam: where the turn takes the next task
/// from, and where a finished job's result lands. Both are pure functions of
/// an answer module, because the state they read and write is the state the
/// turn already holds.
fn check_job_seam(
    shapes: &[WorkShape],
    bindings: ManifestBindings<'_>,
    answers: &[AnswerShape],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    entry_module: Option<&str>,
) -> Vec<WorkDiagnostic> {
    let mut errors = Vec::new();
    for binding in bindings.work {
        // A capability module checked on its own is not yet a program, so the
        // seam between the turn and an answer state is not its business.
        if entry_module == Some(binding.capability.as_str()) {
            continue;
        }
        let Some(shape) = shapes
            .iter()
            .find(|shape| shape.capability == binding.capability)
        else {
            continue;
        };
        if let Some(task) = &binding.task {
            let expected = |state: &Type| {
                (
                    vec![state.clone()],
                    Type::Option(Box::new(shape.task.clone())),
                )
            };
            errors.extend(check_seam_function(
                binding,
                "task",
                task,
                bindings.answer,
                answers,
                fn_sigs,
                &expected,
            ));
        }
        if let Some(landed) = &binding.landed {
            // A job that was cancelled, whose body stopped, or whose id the
            // engine has forgotten has no payload to land, and the run goes
            // on: the outcome reaches the answer state as the error it is.
            let expected = |state: &Type| {
                (
                    vec![
                        state.clone(),
                        Type::Result(Box::new(shape.payload.clone()), Box::new(Type::Str)),
                    ],
                    state.clone(),
                )
            };
            errors.extend(check_seam_function(
                binding,
                "landed",
                landed,
                bindings.answer,
                answers,
                fn_sigs,
                &expected,
            ));
        }
    }
    errors
}

fn check_seam_function(
    binding: &ProviderWorkBinding,
    field: &str,
    value: &str,
    answer_bindings: &[ProviderAnswerBinding],
    answers: &[AnswerShape],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    expected: &dyn Fn(&Type) -> (Vec<Type>, Type),
) -> Vec<WorkDiagnostic> {
    let capability = &binding.capability;
    let module = value
        .rsplit_once('.')
        .map(|(module, _)| module)
        .unwrap_or("");
    let Some(answer) = answers.iter().find(|answer| answer.module == module) else {
        // The manifest may bind that module with `answer` for a capability
        // outside the closure now being analysed, and then this analysis
        // simply cannot see the state. Only a module the manifest never binds
        // is a mistake the manifest can be told about.
        if answer_bindings
            .iter()
            .any(|binding| binding.module == module)
        {
            return Vec::new();
        }
        return vec![binding_error(format!(
            "job kind '{capability}' binds {field} = \"{value}\", but no `answer` binding in aver.toml names module '{module}'; the seam reaches the state the turn holds, so it names a function of a module bound with `answer`"
        ))];
    };
    let Some((params, result, effects)) = fn_sigs.get(value) else {
        return vec![binding_error(format!(
            "job kind '{capability}' binds {field} = \"{value}\", but this program has no function '{value}'"
        ))];
    };
    if !effects.is_empty() {
        return vec![binding_error(format!(
            "job kind '{capability}' binds {field} = \"{value}\", but that function declares effects [{}]; the turn reads the seam between waits, so both its ends are pure",
            effects.join(", ")
        ))];
    }
    let (expected_params, expected_result) = expected(&answer.state);
    let actual_params = params
        .iter()
        .map(|ty| canonicalize_type_names(ty.clone(), module))
        .collect::<Vec<_>>();
    let actual_result = canonicalize_type_names(result.clone(), module);
    let agrees = actual_params.len() == expected_params.len()
        && actual_params
            .iter()
            .zip(&expected_params)
            .all(|(left, right)| same_type(left, right))
        && same_type(&actual_result, &expected_result);
    if agrees {
        return Vec::new();
    }
    vec![binding_error(format!(
        "job kind '{capability}' binds {field} = \"{value}\", so that function must be {}; it is {}",
        render_signature(&expected_params, &expected_result),
        render_signature(&actual_params, &actual_result)
    ))]
}

fn operations_of<'a>(
    registry: &'a CapabilityRegistry,
    capability: &str,
) -> Vec<&'a crate::capability::CapabilityOperation> {
    registry
        .operations()
        .filter(|operation| operation.module == capability)
        .collect()
}

fn answer_key(module: &str, operation: &crate::capability::CapabilityOperation) -> String {
    format!("{module}.{}", operation.name)
}

/// `Pool.claim` is answered with `Pool.__ClaimReply`: one generated sum per
/// operation, because Aver has no generic user types for a shared `Reply<A>`.
fn reply_type_name(capability: &str, operation: &str) -> String {
    let mut chars = operation.chars();
    let head = match chars.next() {
        Some(head) => head.to_uppercase().collect::<String>(),
        None => String::new(),
    };
    format!("{capability}.__{head}{}Reply", chars.as_str())
}

fn render_operation(
    capability: &str,
    operation: &crate::capability::CapabilityOperation,
) -> String {
    let params = operation
        .params
        .iter()
        .map(|(name, ty)| format!("{name}: {}", ty.display()))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "{}({params}) -> {}",
        operation.name,
        canonicalize_type_names(operation.return_type.clone(), capability).display()
    )
}

fn render_signature(params: &[Type], result: &Type) -> String {
    format!(
        "({}) -> {}",
        params
            .iter()
            .map(Type::display)
            .collect::<Vec<_>>()
            .join(", "),
        result.display()
    )
}

/// Why a capability cannot be marked `answer`: the runtime's own providers
/// answer it, and the coordinator the compiler generates calls it directly.
fn compiler_shipped_reason(capability: &str) -> Option<String> {
    if crate::stdlib::STANDARD_CAPABILITY_MODULES.contains(&capability) {
        return Some(format!(
            "'{capability}' is a standard capability this compiler ships and the runtime's providers answer"
        ));
    }
    if crate::stdlib::RESERVED_CAPABILITY_MODULES.contains(&capability) {
        return Some(format!(
            "'{capability}' is a capability this compiler reserves for the runtime's own adapters"
        ));
    }
    None
}

/// Whether `module.name` was a `yield` function: the lowering replaced it
/// with `__<name>Start` and one answer function per request kind.
fn lowered_yield_function(
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    module: &str,
    name: &str,
) -> bool {
    fn_sigs.contains_key(&format!("{module}.__{name}Start"))
}

fn module_has_functions(
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    module: &str,
) -> bool {
    let prefix = format!("{module}.");
    fn_sigs.keys().any(|key| key.starts_with(&prefix))
}

fn answer_binding_error(message: String) -> WorkDiagnostic {
    WorkDiagnostic::new(ANSWER_BINDING, message)
}

fn answer_shape_error(message: String) -> WorkDiagnostic {
    WorkDiagnostic::new(ANSWER_SHAPE, message)
}

fn answer_shape_warning(message: String) -> WorkDiagnostic {
    WorkDiagnostic::warning(ANSWER_SHAPE, message)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    const POOL: &str = "\
module Pool
    kind = capability
    semantics = effectful
    intent = \"Which height a peer should fetch next.\"
    exposes [Assignment, claim, gone]

type Assignment
    Height(Int)
    Idle
    Stop

operation claim(key: Int) -> Pool.Assignment
    ? \"The next height this peer should fetch.\"
    oracle = generative
    replay = recorded

operation gone(key: Int) -> Unit
    ? \"This peer is finished; free whatever it held.\"
    oracle = generativeOutput
    replay = recorded
";

    const VALIDATION: &str = "\
module Validation
    kind = capability
    semantics = effectful
    depends [Work]
    intent = \"One job kind: validating a text record off the turn.\"
    exposes [begin, take]

operation begin(task: String) -> Result<Work.Job, String>
    ? \"Starts one validation off the turn.\"
    oracle = generativeOutput
    replay = recorded

operation take(job: Work.Job) -> Result<Option<Int>, String>
    ? \"Collects one finished validation.\"
    oracle = generativeOutput
    replay = recorded
";

    fn registry_of(modules: &[(&str, &str)]) -> CapabilityRegistry {
        let mut registry = CapabilityRegistry::default();
        for (scope, source) in modules {
            let items = crate::source::parse_source(source).expect("capability parses");
            let (one, errors) = CapabilityRegistry::from_module(scope, &items);
            assert!(errors.is_empty(), "contract errors: {errors:?}");
            registry.merge(one);
        }
        registry
    }

    fn state() -> Type {
        Type::named("State".to_string())
    }

    /// The answer functions a well-shaped `Ledger` declares, as the type
    /// checker hands them over: types written in the module's own scope.
    ///
    /// No fixture can reach this path yet, because a reply sum is generated
    /// into the capability module by the next leg and a hand-written name
    /// cannot begin with `__`. The accept path is checked here instead, so
    /// the expected reply name this leg computes is pinned before the leg
    /// that emits it.
    fn ledger_sigs() -> HashMap<String, FnSignature> {
        let mut sigs = HashMap::new();
        sigs.insert(
            "Ledger.claim".to_string(),
            (
                vec![state(), Type::Int],
                Type::Tuple(vec![state(), Type::named("Pool.__ClaimReply".to_string())]),
                Vec::new(),
            ),
        );
        sigs.insert(
            "Ledger.gone".to_string(),
            (
                vec![state(), Type::Int],
                Type::Tuple(vec![state(), Type::named("Pool.__GoneReply".to_string())]),
                Vec::new(),
            ),
        );
        sigs
    }

    fn answer_binding() -> ProviderAnswerBinding {
        ProviderAnswerBinding {
            capability: "Pool".to_string(),
            module: "Ledger".to_string(),
            index: 0,
        }
    }

    #[test]
    fn a_well_shaped_answer_module_passes_and_reports_its_state() {
        let registry = registry_of(&[("Pool", POOL)]);
        let (shapes, findings) =
            check_answers(&registry, &[answer_binding()], &ledger_sigs(), None);
        assert!(findings.is_empty(), "unexpected findings: {findings:?}");
        assert_eq!(shapes.len(), 1);
        assert_eq!(shapes[0].module, "Ledger");
        assert_eq!(shapes[0].state, Type::named("Ledger.State".to_string()));
        assert_eq!(shapes[0].capabilities, vec!["Pool".to_string()]);
    }

    #[test]
    fn the_expected_reply_name_uppercases_only_the_first_letter() {
        assert_eq!(reply_type_name("Pool", "claim"), "Pool.__ClaimReply");
        assert_eq!(
            reply_type_name("Chain", "nextTarget"),
            "Chain.__NextTargetReply"
        );
    }

    #[test]
    fn a_well_shaped_job_seam_passes_against_the_answer_state() {
        let registry = registry_of(&[("Pool", POOL), ("Validation", VALIDATION)]);
        let mut sigs = ledger_sigs();
        sigs.insert(
            "Ledger.nextTask".to_string(),
            (vec![state()], Type::Option(Box::new(Type::Str)), Vec::new()),
        );
        sigs.insert(
            "Ledger.validated".to_string(),
            (
                vec![
                    state(),
                    Type::Result(Box::new(Type::Int), Box::new(Type::Str)),
                ],
                state(),
                Vec::new(),
            ),
        );
        sigs.insert(
            "Node.validate".to_string(),
            (vec![Type::Str], Type::Int, Vec::new()),
        );
        let answer_bindings = vec![answer_binding()];
        let (answers, findings) = check_answers(&registry, &answer_bindings, &sigs, None);
        assert!(findings.is_empty(), "unexpected findings: {findings:?}");

        let work_bindings = vec![ProviderWorkBinding {
            capability: "Validation".to_string(),
            function: "Node.validate".to_string(),
            index: 1,
            task: Some("Ledger.nextTask".to_string()),
            landed: Some("Ledger.validated".to_string()),
        }];
        let mut shapes = Vec::new();
        let mut declared = BTreeSet::new();
        for (module, outcome) in job_kinds(&registry) {
            declared.insert(module);
            shapes.push(outcome.expect("Validation is a job kind"));
        }
        let errors = check_bindings(
            &registry,
            &shapes,
            &declared,
            ManifestBindings {
                work: &work_bindings,
                answer: &answer_bindings,
            },
            &answers,
            &sigs,
            None,
        );
        assert!(errors.is_empty(), "unexpected seam errors: {errors:?}");
    }

    #[test]
    fn a_work_binding_naming_the_entry_module_is_refused() {
        let registry = registry_of(&[("Validation", VALIDATION)]);
        let mut sigs = std::collections::HashMap::new();
        sigs.insert(
            "Main.validate".to_string(),
            (vec![Type::Str], Type::Int, Vec::new()),
        );
        let work_bindings = vec![ProviderWorkBinding {
            capability: "Validation".to_string(),
            function: "Main.validate".to_string(),
            index: 0,
            task: None,
            landed: None,
        }];
        let mut shapes = Vec::new();
        let mut declared = BTreeSet::new();
        for (module, outcome) in job_kinds(&registry) {
            declared.insert(module);
            shapes.push(outcome.expect("Validation is a job kind"));
        }
        let errors = check_bindings(
            &registry,
            &shapes,
            &declared,
            ManifestBindings {
                work: &work_bindings,
                answer: &[],
            },
            &[],
            &sigs,
            Some("Main"),
        );
        assert_eq!(errors.len(), 1, "unexpected findings: {errors:?}");
        assert_eq!(errors[0].slug, WORK_BINDING);
        assert!(
            errors[0].message.contains("is the entry module"),
            "unexpected message: {}",
            errors[0].message
        );
    }
}
