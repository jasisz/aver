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
    /// Jobs recomputed during replay, keyed by the trace token the recording
    /// gave the handle that started them. Bounded like the engine's own dead
    /// slots: a recording long enough to pass the bound has already replayed
    /// its oldest jobs.
    replayed: std::sync::Mutex<BoundedJobs>,
    /// The jobs this job kind started itself.
    ///
    /// Every job kind of a program shares one engine, and `Work.Job` is one
    /// stdlib type, so a handle minted by one kind type-checks as an argument
    /// to another kind's `take`. Nothing but the runtime can tell them apart,
    /// so the runtime remembers whose job is whose.
    ///
    /// Bounded the same way the engine bounds its dead slots: an id that
    /// leaves this set names a job whose slot the engine has forgotten too,
    /// so `take` answers `work: unknown job` rather than claiming another
    /// kind started it.
    minted: std::sync::Mutex<BoundedIds>,
}

/// The ids one job kind minted, oldest first, bounded by the number of dead
/// slots the engine itself keeps.
#[derive(Default)]
struct BoundedIds {
    order: std::collections::VecDeque<u64>,
    ids: std::collections::BTreeSet<u64>,
}

impl BoundedIds {
    fn insert(&mut self, id: u64) {
        if !self.ids.insert(id) {
            return;
        }
        self.order.push_back(id);
        while self.order.len() > aver_rt::work::DEAD_SLOT_LIMIT
            && let Some(oldest) = self.order.pop_front()
        {
            self.ids.remove(&oldest);
        }
    }

    fn contains(&self, id: u64) -> bool {
        self.ids.contains(&id)
    }
}

/// The jobs a replay recomputed, keyed by trace token, bounded the same way.
#[derive(Default)]
struct BoundedJobs {
    order: std::collections::VecDeque<u64>,
    jobs: std::collections::BTreeMap<u64, aver_rt::work::Job>,
}

impl BoundedJobs {
    fn insert(&mut self, token: u64, job: aver_rt::work::Job) {
        if self.jobs.insert(token, job).is_none() {
            self.order.push_back(token);
        }
        while self.order.len() > aver_rt::work::DEAD_SLOT_LIMIT
            && let Some(oldest) = self.order.pop_front()
        {
            self.jobs.remove(&oldest);
        }
    }

    fn get(&self, token: u64) -> Option<aver_rt::work::Job> {
        self.jobs.get(&token).cloned()
    }
}

/// A job's task and its result are ordinary data; the Work shape check has
/// already refused a capability whose boundary carries a resource. The value
/// codec still wants a native registry, so give it one that owns nothing.
fn dataless_registry() -> NativeProviderRegistry {
    NativeProviderRegistry::new(std::iter::empty())
        .expect("an empty native provider registry is always valid")
}

pub const WORK_FINGERPRINT: &str = concat!("aver-lang/", env!("CARGO_PKG_VERSION"));

/// How long a replayed `take` waits for the job it is comparing against.
const REPLAY_SETTLE_GRACE: std::time::Duration = std::time::Duration::from_secs(5);

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
            replayed: std::sync::Mutex::new(BoundedJobs::default()),
            minted: std::sync::Mutex::new(BoundedIds::default()),
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
            Ok(job) => {
                if let Ok(mut minted) = self.minted.lock() {
                    minted.insert(job.id());
                }
                Ok(ProviderValue::ResultOk(Box::new(ProviderValue::Resource(
                    job.into_resource(),
                ))))
            }
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

    /// Start the job a recorded `begin` started, under the trace token that
    /// recording gave its handle.
    ///
    /// Replay hands the program the recorded handle, so the recording's own
    /// arguments stay exactly what they were; the live job runs beside it and
    /// is found again by that token.
    pub fn start_replay_job(&self, token: u64, task: ProviderValue) -> Result<(), String> {
        let started = match self.begin(task) {
            Ok(ProviderValue::ResultOk(inner)) => match *inner {
                ProviderValue::Resource(resource) => {
                    resource.downcast_ref::<aver_rt::work::Job>().cloned()
                }
                _ => None,
            },
            Ok(ProviderValue::ResultErr(inner)) => {
                return Err(match *inner {
                    ProviderValue::String(message) => message,
                    other => other.shape(),
                });
            }
            Ok(_) => None,
            Err(fault) => return Err(fault.to_string()),
        };
        let Some(job) = started else {
            return Err(format!(
                "job kind '{}' could not start the job it recorded",
                self.capability
            ));
        };
        if let Ok(mut replayed) = self.replayed.lock() {
            replayed.insert(token, job);
        }
        Ok(())
    }

    /// Whether this job kind is the one that started the job with this id.
    fn started_here(&self, id: u64) -> bool {
        self.minted
            .lock()
            .map(|minted| minted.contains(id))
            .unwrap_or(false)
    }

    fn replayed_job(&self, token: u64) -> Option<aver_rt::work::Job> {
        self.replayed.lock().ok()?.get(token)
    }

    /// What the recomputed job holds, without collecting it, as an Aver value.
    /// `None` means this token never named a live job.
    ///
    /// The recording says this job had finished by the turn the caller is
    /// replaying, so waiting a bounded moment for the recomputation asks for
    /// nothing the recording did not already observe. Without the wait the
    /// comparison would only ever fire on a machine slower than the one that
    /// recorded, which is not a check at all.
    pub fn replay_peek(&self, token: u64) -> Option<Result<Option<crate::value::Value>, String>> {
        let job = self.replayed_job(token)?;
        job.settle_by(std::time::Instant::now() + REPLAY_SETTLE_GRACE);
        Some(match job.peek() {
            Ok(None) => Ok(None),
            Ok(Some(value)) => self.decode_payload(value).map(Some),
            Err(message) => Err(message),
        })
    }

    fn decode_payload(&self, value: ProviderValue) -> Result<crate::value::Value, String> {
        super::value::from_provider_value(
            value,
            &self.payload_type,
            &self.capability,
            &self.contracts,
            None,
            &dataless_registry(),
        )
    }

    /// Collect the recomputed job's answer so a second take answers the same
    /// way the recording did.
    pub fn replay_consume(&self, token: u64) {
        if let Some(job) = self.replayed_job(token) {
            let _ = job.take();
        }
    }

    /// Stop the recomputed job a replayed `cancel` cancelled.
    pub fn replay_cancel(&self, token: u64) {
        if let Some(job) = self.replayed_job(token) {
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
    // The base context was frozen from a VM that had already run the top
    // level, so its globals are initialised: running the top level again here
    // would repeat the whole program's setup once per job.
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
            Some("take") => {
                let job = self.job(operation, single)?;
                if !self.started_here(job.id()) {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!(
                            "work: this job was not started by job kind '{}'",
                            self.capability
                        ),
                    ))));
                }
                Ok(Self::answer(job.take()))
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The ids a job kind remembers are bounded: past the bound the oldest
    /// is forgotten, and the id it named is one the engine has forgotten
    /// too, so `take` answers "unknown job" rather than growing for ever.
    #[test]
    fn minted_ids_are_bounded_and_forget_the_oldest_first() {
        let bound = aver_rt::work::DEAD_SLOT_LIMIT as u64;
        let mut minted = BoundedIds::default();
        for id in 1..=bound {
            minted.insert(id);
        }
        assert!(minted.contains(1));
        assert!(minted.contains(bound));

        minted.insert(bound + 1);
        assert!(!minted.contains(1), "the oldest id outlived the bound");
        assert!(minted.contains(2));
        assert!(minted.contains(bound + 1));
    }

    /// Minting the same id twice is one entry, so a repeated id cannot push
    /// the bound's worth of live ids out.
    #[test]
    fn minting_the_same_id_twice_keeps_one_entry() {
        let mut minted = BoundedIds::default();
        minted.insert(7);
        minted.insert(7);
        assert_eq!(minted.order.len(), 1);
        assert!(minted.contains(7));
    }
}
