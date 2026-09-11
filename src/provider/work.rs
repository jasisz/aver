//! The provider a job kind gets: a pure function of the program itself.
//!
//! A capability of Work shape declares `begin` and `take` and binds no host
//! package. What answers it is `work = "Module.function"` in `aver.toml`, run
//! off the turn on the job engine in `aver-rt`. This module is the seam: it
//! turns one `begin` into one job whose body is a child VM over the same
//! compiled program, and one `take` into whatever that child VM produced.

use std::sync::{Arc, OnceLock};

use aver_rt::provider::{
    CapabilityProvider, NativeProviderRegistry, ProviderContext, ProviderFault, ProviderValue,
};
use aver_rt::work::JobEngine;

use crate::ast::Type;
use crate::capability::CapabilityRegistry;

/// The compiled program a job's child VM runs.
///
/// A job's body is an ordinary Aver function, so it needs the program's code,
/// its globals and an arena to work in. The VM hands this over the first time
/// a program reaches `begin`, because only a running VM can freeze a parallel
/// base context whose globals are already initialised.
pub struct WorkBaseContext {
    code: crate::vm::CodeStore,
    globals: Vec<crate::nan_value::NanValue>,
    arena: crate::nan_value::Arena,
}

impl WorkBaseContext {
    pub fn new(
        code: crate::vm::CodeStore,
        globals: Vec<crate::nan_value::NanValue>,
        arena: crate::nan_value::Arena,
    ) -> Self {
        Self {
            code,
            globals,
            arena,
        }
    }
}

/// One job kind, bound to one pure function of the program.
pub struct WorkProvider {
    capability: String,
    function: String,
    task_type: Type,
    payload_type: Type,
    contracts: Arc<CapabilityRegistry>,
    /// The contract-only registry a job's child VM runs under. A job body is
    /// pure, so it needs the program's contracts to pass its own preflight
    /// and no binding at all: an effect inside a job is a refusal, not a
    /// second live provider.
    child_providers: Arc<super::ProviderRegistry>,
    engine: Arc<JobEngine>,
    base: OnceLock<Arc<WorkBaseContext>>,
    identity: String,
}

/// A job's task and its result are ordinary data; the Work shape check has
/// already refused a capability whose boundary carries a resource. The value
/// codec still wants a native registry, so give it one that owns nothing.
fn dataless_registry() -> NativeProviderRegistry {
    NativeProviderRegistry::new(std::iter::empty())
        .expect("an empty native provider registry is always valid")
}

pub const WORK_FINGERPRINT: &str = concat!("aver-lang/", env!("CARGO_PKG_VERSION"));

impl WorkProvider {
    pub fn new(
        capability: &str,
        function: &str,
        task_type: Type,
        payload_type: Type,
        contracts: Arc<CapabilityRegistry>,
        engine: Arc<JobEngine>,
    ) -> Self {
        let child_providers =
            Arc::new(super::ProviderRegistry::for_contracts((*contracts).clone()));
        Self {
            capability: capability.to_string(),
            function: function.to_string(),
            task_type,
            payload_type,
            contracts,
            child_providers,
            engine,
            base: OnceLock::new(),
            identity: format!("aver.work.{capability}/vm"),
        }
    }

    pub fn capability(&self) -> &str {
        &self.capability
    }

    pub fn engine(&self) -> &Arc<JobEngine> {
        &self.engine
    }

    pub fn has_base_context(&self) -> bool {
        self.base.get().is_some()
    }

    pub fn install_base_context(&self, base: Arc<WorkBaseContext>) {
        let _ = self.base.set(base);
    }

    /// The job handle an argument names, for the operations that read a job
    /// without going through the value codec.
    fn job<'a>(
        &self,
        operation: &str,
        value: &'a ProviderValue,
    ) -> Result<&'a aver_rt::work::Job, ProviderFault> {
        let ProviderValue::Resource(resource) = value else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} expects a Work.Job resource"),
            ));
        };
        resource
            .downcast_ref::<aver_rt::work::Job>()
            .ok_or_else(|| {
                ProviderFault::new(
                    "invalid_resource",
                    format!("{operation} received a resource from an incompatible provider"),
                )
            })
    }

    fn begin(&self, task: ProviderValue) -> Result<ProviderValue, ProviderFault> {
        let Some(base) = self.base.get().cloned() else {
            return Err(ProviderFault::new(
                "work_unprepared",
                format!(
                    "job kind '{}' has no compiled program to run '{}' with",
                    self.capability, self.function
                ),
            ));
        };
        let capability = self.capability.clone();
        let function = self.function.clone();
        let task_type = self.task_type.clone();
        let payload_type = self.payload_type.clone();
        let contracts = self.contracts.clone();
        let child_providers = self.child_providers.clone();
        let body = move |cancel: Arc<std::sync::atomic::AtomicBool>| {
            run_bound_function(
                &base,
                &capability,
                &function,
                &task_type,
                &payload_type,
                &contracts,
                child_providers,
                task,
                cancel,
            )
        };
        match self.engine.begin(Box::new(body)) {
            Ok(job) => Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Resource(
                job.into_resource(),
            )))),
            Err(message) => Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                message,
            )))),
        }
    }

    fn answer(outcome: Result<Option<ProviderValue>, String>) -> ProviderValue {
        match outcome {
            Ok(Some(value)) => {
                ProviderValue::ResultOk(Box::new(ProviderValue::OptionSome(Box::new(value))))
            }
            Ok(None) => ProviderValue::ResultOk(Box::new(ProviderValue::OptionNone)),
            Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
        }
    }

    /// The answer a job holds, without collecting it. Replay compares the
    /// recorded answer against this one.
    pub fn peek(&self, value: &ProviderValue) -> Result<ProviderValue, ProviderFault> {
        let operation = format!("{}.take", self.capability);
        Ok(Self::answer(self.job(&operation, value)?.peek()))
    }

    /// Drop the answer a job holds, so a replayed take is not collected twice.
    pub fn discard(&self, value: &ProviderValue) {
        let operation = format!("{}.take", self.capability);
        if let Ok(job) = self.job(&operation, value) {
            job.cancel();
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn run_bound_function(
    base: &WorkBaseContext,
    capability: &str,
    function: &str,
    task_type: &Type,
    payload_type: &Type,
    contracts: &CapabilityRegistry,
    child_providers: Arc<super::ProviderRegistry>,
    task: ProviderValue,
    cancel: Arc<std::sync::atomic::AtomicBool>,
) -> Result<ProviderValue, String> {
    use crate::nan_value::NanValueConvert;

    let native = dataless_registry();
    let task =
        super::value::from_provider_value(task, task_type, capability, contracts, None, &native)
            .map_err(|why| {
                format!("work: job kind '{capability}' was handed an unusable task: {why}")
            })?;

    let mut vm = crate::vm::VM::new(
        base.code.clone(),
        base.globals.clone(),
        base.arena.clone_static(),
    );
    vm.set_provider_registry(child_providers);
    vm.defer_missing_capability_providers_to_dispatch(true);
    vm.set_cancelled(cancel);
    vm.run_top_level()
        .map_err(|error| format!("work: {function}: {error}"))?;
    let argument = crate::nan_value::NanValue::from_value(&task, &mut vm.arena);
    let produced = vm
        .run_named_function(function, &[argument])
        .map_err(|error| format!("work: {function}: {error}"))?;
    let produced = produced.to_value(&vm.arena);
    super::value::to_provider_value(&produced, payload_type, capability, contracts, &native)
        .map_err(|why| format!("work: job kind '{capability}' produced an unusable result: {why}"))
}

impl CapabilityProvider for WorkProvider {
    fn identity(&self) -> &str {
        &self.identity
    }

    fn fingerprint(&self) -> &str {
        WORK_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let operation = context.operation.as_str();
        let [single] = args else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "{operation} expects exactly one argument, got {}",
                    args.len()
                ),
            ));
        };
        match operation.rsplit_once('.').map(|(_, name)| name) {
            Some("begin") => self.begin(single.clone()),
            Some("take") => Ok(Self::answer(self.job(operation, single)?.take())),
            _ => Err(ProviderFault::new(
                "unknown_operation",
                format!(
                    "job kind '{}' does not implement '{operation}'",
                    self.capability
                ),
            )),
        }
    }
}

/// The `begin` parameter type and the `take` payload type of one job kind, as
/// the contract declares them. The Work shape check has already proved both
/// operations exist and have these shapes.
pub fn boundary_types(contracts: &CapabilityRegistry, capability: &str) -> Option<(Type, Type)> {
    let begin = contracts.operation(&format!("{capability}.begin"))?;
    let take = contracts.operation(&format!("{capability}.take"))?;
    let task = begin.params.first()?.1.clone();
    let payload = match &take.return_type {
        Type::Result(ok, _) => match &**ok {
            Type::Option(payload) => (**payload).clone(),
            _ => return None,
        },
        _ => return None,
    };
    Some((task, payload))
}
