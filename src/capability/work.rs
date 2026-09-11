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

use std::collections::BTreeSet;

use crate::ast::Type;
use crate::capability::CapabilityRegistry;
use crate::config::ProviderWorkBinding;

use super::validation::canonicalize_type_names;

/// The stdlib module that owns the running-job handle.
pub const WORK_MODULE: &str = "Work";

/// The stdlib module that owns the one wait of a turn.
pub const WAIT_MODULE: &str = "Wait";

/// The canonical name of the running-job handle.
pub const WORK_JOB: &str = "Work.Job";

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

/// One work diagnostic: the stable slug plus the sentence a front door prints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WorkDiagnostic {
    pub slug: &'static str,
    pub message: String,
}

impl WorkDiagnostic {
    fn new(slug: &'static str, message: String) -> Self {
        Self { slug, message }
    }

    /// The one-line form every text front door prints.
    pub fn rendered(&self) -> String {
        format!("error[{}]: {}", self.slug, self.message)
    }
}

pub const WORK_SHAPE: &str = "work-shape";
pub const WORK_BINDING: &str = "work-binding";
pub const WORK_TARGET: &str = "work-target";

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

/// One target the program can be prepared for. Only the VM answers a job in
/// this build; decision 7 of the epic brief keeps the other three honest.
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
    errors.extend(check_bindings(
        registry,
        &shapes,
        &declared,
        work_bindings,
        fn_sigs,
        entry_module,
    ));
    if !shapes.is_empty() && target != WorkTarget::Vm {
        errors.push(WorkDiagnostic::new(WORK_TARGET, format!(
            "Work-bound capabilities run on the VM in this build; the Rust, wasm-gc and wasip2 backends follow in a later change. '{}' is a job kind and the requested target is {}",
            shapes[0].capability,
            target.label()
        )));
    }
    errors
}

/// Check every job kind of the program against the manifest's `work` bindings
/// and the functions those bindings name. The returned strings are complete
/// `work-binding` diagnostics, in capability order.
pub fn check_bindings(
    registry: &CapabilityRegistry,
    shapes: &[WorkShape],
    declared: &BTreeSet<String>,
    work_bindings: &[ProviderWorkBinding],
    fn_sigs: &std::collections::HashMap<String, FnSignature>,
    entry_module: Option<&str>,
) -> Vec<WorkDiagnostic> {
    let mut errors = Vec::new();
    for binding in work_bindings {
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
        let Some(binding) = work_bindings
            .iter()
            .find(|binding| binding.capability == shape.capability)
        else {
            errors.push(binding_error(format!(
                "job kind '{0}' has no `work` binding; add one [[providers.bindings]] entry to aver.toml with capability = \"{0}\" and work = \"Module.function\" naming the pure function that runs the job",
                shape.capability
            )));
            continue;
        };
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
