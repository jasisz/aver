//! Wasmtime workers for the versioned Work ABI. Compiled code is shared;
//! each worker owns its Store, GC heap and temporary boundary values.

use std::collections::BTreeMap;
use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::time::Duration;

use aver::capability::work::JobKindPlan;
use aver::provider::{ProviderRegistry, ProviderValue};
use aver_rt::work::{Job, JobEngine};
use wasmtime::{Engine, ExternType, Instance, Linker, Module, Store};

use super::RunWasmGcHost;

mod boundary;
pub(super) use boundary::dispatch;

pub(super) struct WorkerJob {
    pub kind: usize,
    pub task: Option<ProviderValue>,
    pub outcome: Option<ProviderValue>,
    pub cancel: Arc<AtomicBool>,
}

struct Worker {
    store: Store<RunWasmGcHost>,
    instance: Instance,
}

pub(super) struct HostWork {
    pub engine: Arc<JobEngine>,
    wasm_engine: Engine,
    module: Module,
    kinds: Vec<JobKindPlan>,
    providers: ProviderRegistry,
    workers: Mutex<Vec<Worker>>,
    jobs: Mutex<BTreeMap<u64, (usize, Job)>>,
    stop_clock: Arc<AtomicBool>,
}

impl HostWork {
    pub fn new(
        engine: &Engine,
        module: &Module,
        kinds: Vec<JobKindPlan>,
        providers: ProviderRegistry,
        limit: usize,
    ) -> Result<Arc<Self>, String> {
        let work = Arc::new(Self {
            engine: JobEngine::new(limit),
            wasm_engine: engine.clone(),
            module: module.clone(),
            kinds,
            providers,
            workers: Mutex::new(Vec::new()),
            jobs: Mutex::new(BTreeMap::new()),
            stop_clock: Arc::new(AtomicBool::new(false)),
        });
        let clock = work.stop_clock.clone();
        let engine = engine.clone();
        std::thread::Builder::new()
            .name("aver-wasm-work-clock".into())
            .spawn(move || {
                while !clock.load(Ordering::Relaxed) {
                    std::thread::sleep(Duration::from_millis(10));
                    engine.increment_epoch();
                }
            })
            .map_err(|error| format!("work: cannot start cancellation clock: {error}"))?;
        Ok(work)
    }

    pub fn begin(
        self: &Arc<Self>,
        kind: usize,
        task: ProviderValue,
        trace: Option<u64>,
    ) -> Result<Job, String> {
        if let Some(trace) = trace
            && self
                .jobs
                .lock()
                .map_err(|_| "work: poisoned handle table")?
                .contains_key(&trace)
        {
            return Err("work: duplicate replay handle".into());
        }
        let pool = self.clone();
        let job = self
            .engine
            .begin(Box::new(move |cancel| pool.compute(kind, task, cancel)))?;
        let mut jobs = self
            .jobs
            .lock()
            .map_err(|_| "work: poisoned handle table")?;
        jobs.retain(|_, (_, job)| self.engine.knows(job.id()));
        jobs.insert(trace.unwrap_or(job.id()), (kind, job.clone()));
        Ok(job)
    }

    pub fn job(&self, id: i64, kind: Option<usize>) -> Result<Job, String> {
        let jobs = self
            .jobs
            .lock()
            .map_err(|_| "work: poisoned handle table")?;
        let (owner, job) = jobs.get(&(id as u64)).ok_or("work: unknown job")?;
        if let Some(kind) = kind
            && *owner != kind
        {
            return Err(format!(
                "work: this job was not started by job kind '{}'",
                self.kinds[kind].shape.capability
            ));
        }
        Ok(job.clone())
    }

    fn compute(
        &self,
        kind: usize,
        task: ProviderValue,
        cancel: Arc<AtomicBool>,
    ) -> Result<ProviderValue, String> {
        if cancel.load(Ordering::Relaxed) {
            return Err("work: job cancelled".into());
        }
        let mut worker = self
            .workers
            .lock()
            .map_err(|_| "work: poisoned worker pool")?
            .pop();
        if worker.is_none() {
            worker = Some(self.new_worker()?);
        }
        let mut worker = worker.expect("worker created");
        worker.store.data_mut().worker_job = Some(WorkerJob {
            kind,
            task: Some(task),
            outcome: None,
            cancel,
        });
        worker.store.set_epoch_deadline(1);
        let entry = worker
            .instance
            .get_typed_func::<(), ()>(&mut worker.store, &format!("__work_v1_run_{kind}"))
            .map_err(|error| format!("work: missing worker export: {error:#}"))?;
        // A HandleScope releases all host roots made by the transport before
        // this Store returns to the pool; the worker never retains old tasks.
        let result = {
            let mut scope = wasmtime::RootScope::new(&mut worker.store);
            entry
                .call(&mut scope, ())
                .map_err(|error| format!("work: job trapped: {error:#}"))
        };
        let state = worker
            .store
            .data_mut()
            .worker_job
            .take()
            .expect("worker job installed");
        let answer = result.and_then(|_| {
            state
                .outcome
                .ok_or_else(|| "work: worker returned without an answer".into())
        });
        // A trapped instance is discarded so partial mutable runtime state
        // cannot leak into the next job.
        if answer.is_ok() {
            self.workers
                .lock()
                .map_err(|_| "work: poisoned worker pool")?
                .push(worker);
        }
        answer
    }

    fn new_worker(&self) -> Result<Worker, String> {
        let mut store = Store::new(
            &self.wasm_engine,
            RunWasmGcHost {
                program_args: Vec::new(),
                recorder: None,
                caller_fn_table: Vec::new(),
                tcp_settings: Default::default(),
                project_config: None,
                job_kinds: self.kinds.clone(),
                providers: Some(self.providers.clone()),
                host_work: None,
                worker_job: None,
            },
        );
        store.set_epoch_deadline(1);
        store.epoch_deadline_callback(|context| {
            if context
                .data()
                .worker_job
                .as_ref()
                .is_some_and(|job| job.cancel.load(Ordering::Relaxed))
            {
                return Err(wasmtime::Error::msg("work: job cancelled"));
            }
            Ok(wasmtime::UpdateDeadline::Continue(1))
        });
        let mut linker = Linker::new(&self.wasm_engine);
        for import in self.module.imports() {
            let ExternType::Func(ty) = import.ty() else {
                return Err("work: worker module imports a non-function".into());
            };
            let module = import.module().to_string();
            let name = import.name().to_string();
            let module_owned = module.clone();
            let name_owned = name.clone();
            linker
                .func_new(&module, &name, ty, move |mut caller, params, results| {
                    if module_owned == aver::codegen::wasm_gc::work_abi::MODULE
                        && matches!(name_owned.as_str(), "task" | "complete")
                    {
                        return dispatch(&name_owned, &mut caller, params, results);
                    }
                    Err(wasmtime::Error::msg(format!(
                        "work: pure worker called {module_owned}.{name_owned}"
                    )))
                })
                .map_err(|error| format!("work: worker import: {error:#}"))?;
        }
        let instance = linker
            .instantiate(&mut store, &self.module)
            .map_err(|error| format!("work: worker instantiate: {error:#}"))?;
        Ok(Worker { store, instance })
    }
}

impl Drop for HostWork {
    fn drop(&mut self) {
        self.stop_clock.store(true, Ordering::Relaxed);
    }
}

/// Scope-owned shutdown also runs when main traps or replay rejects a trace.
pub(super) struct Shutdown(pub Option<Arc<HostWork>>);
impl Drop for Shutdown {
    fn drop(&mut self) {
        if let Some(work) = &self.0 {
            work.engine.shutdown();
        }
    }
}
