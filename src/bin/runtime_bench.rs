use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Instant, SystemTime};

use aver::runtime_bench_cases::{CORE_BENCH_CASES, CoreBenchCase};

const DEFAULT_OUTPUT_DIR: &str = "/tmp/aver_runtime_bench";
const WORKFLOW_MODULE_ROOT: &str = "projects/workflow_engine";
const WORKFLOW_ENTRY_FILE: &str = "projects/workflow_engine/main.av";
const WORKFLOW_DATA_DIR: &str = "/tmp/aver_workflow_engine";
const WORKFLOW_GENERATED_NAME: &str = "runtime_bench_workflow_engine";
const DEFAULT_WORKFLOW_APP_SEED: usize = 250;
const PAYMENT_MODULE_ROOT: &str = "projects/payment_ops";
const PAYMENT_ENTRY_FILE: &str = "projects/payment_ops/main.av";
const PAYMENT_DATA_DIR: &str = "/tmp/aver_payment_ops";
const PAYMENT_GENERATED_NAME: &str = "runtime_bench_payment_ops";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RuntimeKind {
    Vm,
    Wasm,
    Generated,
    SelfHost,
}

impl RuntimeKind {
    fn all() -> &'static [RuntimeKind] {
        &[Self::Vm, Self::Wasm, Self::Generated, Self::SelfHost]
    }

    fn name(self) -> &'static str {
        match self {
            Self::Vm => "vm",
            Self::Wasm => "wasm",
            Self::Generated => "generated",
            Self::SelfHost => "selfhost",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SuiteKind {
    Core,
    Apps,
    All,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AppKind {
    WorkflowEngine,
    PaymentOps,
}

impl AppKind {
    fn all() -> &'static [AppKind] {
        &[Self::WorkflowEngine, Self::PaymentOps]
    }

    fn name(self) -> &'static str {
        match self {
            Self::WorkflowEngine => "workflow_engine",
            Self::PaymentOps => "payment_ops",
        }
    }

    fn module_root(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_MODULE_ROOT,
            Self::PaymentOps => PAYMENT_MODULE_ROOT,
        }
    }

    fn entry_file(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_ENTRY_FILE,
            Self::PaymentOps => PAYMENT_ENTRY_FILE,
        }
    }

    fn data_dir(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_DATA_DIR,
            Self::PaymentOps => PAYMENT_DATA_DIR,
        }
    }

    fn generated_name(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_GENERATED_NAME,
            Self::PaymentOps => PAYMENT_GENERATED_NAME,
        }
    }
}

#[derive(Debug)]
struct Config {
    suite: SuiteKind,
    runtimes: Vec<RuntimeKind>,
    core_case: Option<String>,
    app: Option<String>,
    workload: Option<String>,
    full: bool,
    seed: usize,
    seed_explicit: bool,
    iters: usize,
    warmup: usize,
    output_dir: PathBuf,
    rebuild: bool,
}

#[derive(Debug)]
struct CoreResult {
    name: &'static str,
    vm_ms: f64,
    wasm_ms: f64,
    generated_ms: f64,
    selfhost_ms: f64,
    outputs_match: bool,
}

#[derive(Debug)]
struct AppResult {
    app: &'static str,
    workload: &'static str,
    vm_ms: f64,
    wasm_ms: f64,
    generated_ms: f64,
    selfhost_ms: f64,
}

type CommandBatch = Vec<Vec<String>>;
type WorkloadPlan = (CommandBatch, CommandBatch);

struct GeneratedProjectSpec<'a> {
    file: &'a Path,
    module_root: Option<&'a Path>,
    output_dir: &'a Path,
    project_name: &'a str,
    override_main: Option<&'a str>,
}

struct RunContext<'a> {
    repo_root: &'a Path,
    aver_bin: &'a Path,
    generated_bin: &'a Path,
}

fn usage() -> &'static str {
    "Usage: cargo run --release --bin runtime_bench -- [--suite core|apps|all] [--runtime vm|wasm|generated|selfhost|all] [--core-case SLUG|all] [--app workflow_engine|payment_ops|all] [--workload NAME|all] [--full] [--seed N] [--iters N] [--warmup N] [--output DIR] [--rebuild]"
}

fn parse_suite(value: &str) -> Option<SuiteKind> {
    match value {
        "core" => Some(SuiteKind::Core),
        "apps" => Some(SuiteKind::Apps),
        "all" => Some(SuiteKind::All),
        _ => None,
    }
}

fn parse_runtimes(value: &str) -> Option<Vec<RuntimeKind>> {
    if value == "all" {
        return Some(RuntimeKind::all().to_vec());
    }

    let mut out = Vec::new();
    for part in value
        .split(',')
        .map(|raw| raw.trim())
        .filter(|raw| !raw.is_empty())
    {
        let runtime = match part {
            "vm" => RuntimeKind::Vm,
            "wasm" => RuntimeKind::Wasm,
            "generated" => RuntimeKind::Generated,
            "selfhost" => RuntimeKind::SelfHost,
            _ => return None,
        };
        if !out.contains(&runtime) {
            out.push(runtime);
        }
    }

    (!out.is_empty()).then_some(out)
}

fn parse_args() -> Result<Config, String> {
    let mut suite = SuiteKind::All;
    let mut runtimes = RuntimeKind::all().to_vec();
    let mut core_case = None;
    let mut app = None;
    let mut workload = None;
    let mut full = false;
    let mut seed = 1_000usize;
    let mut seed_explicit = false;
    let mut iters = 3usize;
    let mut warmup = 1usize;
    let mut output_dir = PathBuf::from(DEFAULT_OUTPUT_DIR);
    let mut rebuild = false;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--suite" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --suite".to_string())?;
                suite =
                    parse_suite(&value).ok_or_else(|| format!("unknown --suite '{}'", value))?;
            }
            "--runtime" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --runtime".to_string())?;
                runtimes = parse_runtimes(&value)
                    .ok_or_else(|| format!("unknown --runtime '{}'", value))?;
            }
            "--core-case" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --core-case".to_string())?;
                if value != "all" {
                    core_case = Some(value);
                }
            }
            "--app" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --app".to_string())?;
                if value != "all" {
                    app = Some(value);
                }
            }
            "--workload" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --workload".to_string())?;
                if value != "all" {
                    workload = Some(value);
                }
            }
            "--full" => full = true,
            "--seed" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --seed".to_string())?;
                seed = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --seed '{}'", value))?;
                seed_explicit = true;
            }
            "--iters" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --iters".to_string())?;
                iters = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --iters '{}'", value))?;
            }
            "--warmup" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --warmup".to_string())?;
                warmup = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --warmup '{}'", value))?;
            }
            "--output" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --output".to_string())?;
                output_dir = PathBuf::from(value);
            }
            "--rebuild" => rebuild = true,
            "--help" | "-h" => return Err(usage().to_string()),
            other => return Err(format!("unknown argument '{}'\n{}", other, usage())),
        }
    }

    Ok(Config {
        suite,
        runtimes,
        core_case,
        app,
        workload,
        full,
        seed,
        seed_explicit,
        iters,
        warmup,
        output_dir,
        rebuild,
    })
}

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn host_aver_path(repo_root: &Path) -> PathBuf {
    repo_root
        .join("target")
        .join("release")
        .join(format!("aver{}", env::consts::EXE_SUFFIX))
}

fn latest_source_mtime(path: &Path) -> Result<SystemTime, String> {
    let metadata =
        fs::metadata(path).map_err(|e| format!("cannot stat '{}': {}", path.display(), e))?;
    let mut newest = metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH);
    if metadata.is_dir() {
        for entry in
            fs::read_dir(path).map_err(|e| format!("cannot read '{}': {}", path.display(), e))?
        {
            let entry =
                entry.map_err(|e| format!("cannot read entry in '{}': {}", path.display(), e))?;
            let child_newest = latest_source_mtime(&entry.path())?;
            newest = newest.max(child_newest);
        }
    }
    Ok(newest)
}

fn render_status(status: ExitStatus) -> String {
    match status.code() {
        Some(code) => format!("code {}", code),
        None => "signal".to_string(),
    }
}

fn median(mut values: Vec<f64>) -> f64 {
    values.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let mid = values.len() / 2;
    if values.len().is_multiple_of(2) {
        (values[mid - 1] + values[mid]) / 2.0
    } else {
        values[mid]
    }
}

fn run_silent(cmd: &mut Command, context: &str) -> Result<(), String> {
    cmd.stdout(Stdio::null());
    cmd.stderr(Stdio::null());
    let status = cmd
        .status()
        .map_err(|e| format!("{}: cannot start command: {}", context, e))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "{}: command exited with {}",
            context,
            render_status(status)
        ))
    }
}

fn run_capture(cmd: &mut Command, context: &str) -> Result<String, String> {
    cmd.stderr(Stdio::null());
    let output = cmd
        .output()
        .map_err(|e| format!("{}: cannot start command: {}", context, e))?;
    if !output.status.success() {
        return Err(format!(
            "{}: command exited with {}",
            context,
            render_status(output.status)
        ));
    }
    String::from_utf8(output.stdout)
        .map(|out| out.trim().to_string())
        .map_err(|e| format!("{}: stdout was not utf8: {}", context, e))
}

fn ensure_host_aver(repo_root: &Path) -> Result<PathBuf, String> {
    let aver = host_aver_path(repo_root);
    let mut cmd = Command::new("cargo");
    cmd.current_dir(repo_root)
        .arg("build")
        .arg("--release")
        .arg("--bin")
        .arg("aver")
        .arg("--features")
        .arg("wasm");
    run_silent(&mut cmd, "build host aver")?;
    Ok(aver)
}

fn generated_binary_path(output_dir: &Path, name: &str) -> PathBuf {
    output_dir
        .join("target")
        .join("release")
        .join(format!("{}{}", name, env::consts::EXE_SUFFIX))
}

fn ensure_generated_project(
    repo_root: &Path,
    aver_bin: &Path,
    spec: GeneratedProjectSpec<'_>,
    rebuild: bool,
) -> Result<(PathBuf, f64), String> {
    let GeneratedProjectSpec {
        file,
        module_root,
        output_dir,
        project_name,
        override_main,
    } = spec;
    if rebuild && output_dir.exists() {
        fs::remove_dir_all(output_dir)
            .map_err(|e| format!("cannot remove '{}': {}", output_dir.display(), e))?;
    }

    let cargo_toml = output_dir.join("Cargo.toml");
    let latest_input = latest_source_mtime(file)?
        .max(
            module_root
                .map(latest_source_mtime)
                .transpose()?
                .unwrap_or(SystemTime::UNIX_EPOCH),
        )
        .max(latest_source_mtime(aver_bin)?)
        .max(latest_source_mtime(
            &repo_root.join("aver-rt").join("Cargo.toml"),
        )?);
    let output_mtime = fs::metadata(&cargo_toml)
        .ok()
        .and_then(|meta| meta.modified().ok())
        .unwrap_or(SystemTime::UNIX_EPOCH);

    if rebuild || !cargo_toml.exists() || latest_input > output_mtime {
        let mut cmd = Command::new(aver_bin);
        cmd.current_dir(repo_root)
            .env("AVER_RUNTIME_PATH", repo_root.join("aver-rt"))
            .arg("compile")
            .arg(file)
            .arg("--output")
            .arg(output_dir)
            .arg("--name")
            .arg(project_name);
        if let Some(module_root) = module_root {
            cmd.arg("--module-root").arg(module_root);
        }
        run_silent(
            &mut cmd,
            &format!("compile {} to generated Rust", project_name),
        )?;
    }

    if let Some(main_src) = override_main {
        fs::write(output_dir.join("src").join("main.rs"), main_src)
            .map_err(|e| format!("cannot patch generated main for '{}': {}", project_name, e))?;
    }

    let started = Instant::now();
    let mut build_cmd = Command::new("cargo");
    build_cmd
        .current_dir(output_dir)
        .arg("build")
        .arg("--release");
    run_silent(
        &mut build_cmd,
        &format!("build generated Rust project '{}'", project_name),
    )?;

    Ok((
        generated_binary_path(output_dir, project_name),
        started.elapsed().as_secs_f64() * 1000.0,
    ))
}

fn write_core_source(case: &CoreBenchCase, output_dir: &Path) -> Result<PathBuf, String> {
    let dir = output_dir.join("core_sources");
    fs::create_dir_all(&dir).map_err(|e| format!("cannot create '{}': {}", dir.display(), e))?;
    let path = dir.join(format!("{}.av", case.slug));
    let renamed = case.source.replacen("fn main()", "fn benchMain()", 1);
    let wrapped = format!(
        "module Bench\n    intent =\n        \"Runtime benchmark case.\"\n\n{}\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"{{benchMain()}}\")\n",
        renamed
    );
    fs::write(&path, wrapped).map_err(|e| format!("cannot write '{}': {}", path.display(), e))?;
    Ok(path)
}

fn run_core_once(
    runtime: RuntimeKind,
    repo_root: &Path,
    aver_bin: &Path,
    source_path: &Path,
    generated_bin: &Path,
) -> Result<String, String> {
    match runtime {
        RuntimeKind::Vm => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root).arg("run").arg(source_path);
            run_capture(&mut cmd, "core vm run")
        }
        RuntimeKind::Wasm => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(source_path)
                .arg("--wasm");
            run_capture(&mut cmd, "core wasm run")
        }
        RuntimeKind::Generated => {
            let mut cmd = Command::new(generated_bin);
            cmd.current_dir(repo_root);
            run_capture(&mut cmd, "core generated run")
        }
        RuntimeKind::SelfHost => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(source_path)
                .arg("--self-host");
            run_capture(&mut cmd, "core selfhost run")
        }
    }
}

fn bench_core_runtime(
    runtime: RuntimeKind,
    repo_root: &Path,
    aver_bin: &Path,
    source_path: &Path,
    generated_bin: &Path,
    warmup: usize,
    iters: usize,
) -> Result<(f64, String), String> {
    for _ in 0..warmup {
        let _ = run_core_once(runtime, repo_root, aver_bin, source_path, generated_bin)?;
    }

    let mut times = Vec::new();
    let mut last_output = String::new();
    for _ in 0..iters {
        let started = Instant::now();
        last_output = run_core_once(runtime, repo_root, aver_bin, source_path, generated_bin)?;
        times.push(started.elapsed().as_secs_f64() * 1000.0);
    }
    Ok((median(times), last_output))
}

fn best_vs_vm(result_vm_ms: f64, candidates: &[(&str, f64)]) -> String {
    let best = candidates
        .iter()
        .filter(|(_, ms)| *ms > 0.0)
        .min_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    match best {
        Some((name, ms)) if result_vm_ms > 0.0 => format!("{:.1}x ({})", result_vm_ms / ms, name),
        _ => "n/a".to_string(),
    }
}

fn print_core_results(results: &[CoreResult]) {
    println!();
    println!("Core benchmarks — end-to-end wall time (median)");
    println!("{:-<140}", "");
    println!(
        "{:<22} {:>12} {:>12} {:>12} {:>12} {:>18} {:>8}",
        "Benchmark", "VM", "WASM", "Generated", "Self-host", "Best vs VM", "Match"
    );
    println!("{:-<140}", "");
    for result in results {
        let best = best_vs_vm(
            result.vm_ms,
            &[
                ("wasm", result.wasm_ms),
                ("gen", result.generated_ms),
                ("selfhost", result.selfhost_ms),
            ],
        );
        let match_status = if result.outputs_match { "OK" } else { "DIFF" };
        println!(
            "{:<22} {:>10.3}ms {:>10.3}ms {:>10.3}ms {:>10.3}ms {:>18} {:>8}",
            result.name,
            result.vm_ms,
            result.wasm_ms,
            result.generated_ms,
            result.selfhost_ms,
            best,
            match_status
        );
    }
    println!("{:-<140}", "");
}

fn benchmark_core(
    cfg: &Config,
    repo_root: &Path,
    aver_bin: &Path,
) -> Result<Vec<CoreResult>, String> {
    let cases: Vec<_> = CORE_BENCH_CASES
        .iter()
        .filter(|case| {
            cfg.core_case
                .as_deref()
                .is_none_or(|slug| slug == case.slug)
        })
        .collect();
    if cases.is_empty() {
        return Err("no core benchmark cases matched the selected filter".to_string());
    }

    let mut results = Vec::new();
    for case in cases {
        let source_path = write_core_source(case, &cfg.output_dir)?;
        let generated_output = cfg
            .output_dir
            .join("generated")
            .join("core")
            .join(case.slug);
        let generated_name = format!("runtime_bench_core_{}", case.slug);
        let (generated_bin, _generated_build_ms) = ensure_generated_project(
            repo_root,
            aver_bin,
            GeneratedProjectSpec {
                file: &source_path,
                module_root: None,
                output_dir: &generated_output,
                project_name: &generated_name,
                override_main: None,
            },
            cfg.rebuild,
        )?;

        let (vm_ms, vm_out) = bench_core_runtime(
            RuntimeKind::Vm,
            repo_root,
            aver_bin,
            &source_path,
            &generated_bin,
            cfg.warmup,
            cfg.iters,
        )?;
        let (wasm_ms, wasm_out) = bench_core_runtime(
            RuntimeKind::Wasm,
            repo_root,
            aver_bin,
            &source_path,
            &generated_bin,
            cfg.warmup,
            cfg.iters,
        )?;
        let (generated_ms, generated_out) = bench_core_runtime(
            RuntimeKind::Generated,
            repo_root,
            aver_bin,
            &source_path,
            &generated_bin,
            cfg.warmup,
            cfg.iters,
        )?;
        let (selfhost_ms, selfhost_out) = bench_core_runtime(
            RuntimeKind::SelfHost,
            repo_root,
            aver_bin,
            &source_path,
            &generated_bin,
            cfg.warmup,
            cfg.iters,
        )?;

        results.push(CoreResult {
            name: case.name,
            vm_ms,
            wasm_ms,
            generated_ms,
            selfhost_ms,
            outputs_match: vm_out == wasm_out
                && wasm_out == generated_out
                && generated_out == selfhost_out,
        });
    }

    Ok(results)
}

fn workflow_seed_commands(seed: usize) -> Vec<Vec<String>> {
    let mut commands = vec![vec![
        "create_project".to_string(),
        "alpha".to_string(),
        "Alpha".to_string(),
    ]];
    for idx in 1..=seed {
        commands.push(vec![
            "create_task".to_string(),
            "alpha".to_string(),
            format!("t{}", idx),
            format!("Task {}", idx),
            "normal".to_string(),
            "-".to_string(),
            "-".to_string(),
        ]);
    }
    commands
}

fn workflow_commands(workload: &str, seed: usize) -> Option<WorkloadPlan> {
    match workload {
        "seed_tasks" => Some((vec![], workflow_seed_commands(seed))),
        "list_tasks" => Some((
            workflow_seed_commands(seed),
            vec![vec!["list_tasks".to_string()]],
        )),
        "show_task" => Some((
            workflow_seed_commands(seed),
            vec![vec!["show_task".to_string(), format!("t{}", seed)]],
        )),
        "run_rules" => Some((
            workflow_seed_commands(seed),
            vec![vec!["run_rules".to_string()]],
        )),
        _ => None,
    }
}

fn effective_app_seed(app: AppKind, cfg: &Config) -> usize {
    if cfg.seed_explicit || cfg.full {
        return cfg.seed;
    }

    match app {
        AppKind::WorkflowEngine => DEFAULT_WORKFLOW_APP_SEED,
        AppKind::PaymentOps => cfg.seed,
    }
}

fn payment_commands(workload: &str) -> Option<WorkloadPlan> {
    let ingest = vec![
        "ingest_webhooks".to_string(),
        "stripe".to_string(),
        "projects/payment_ops/fixtures/stripe_webhooks_day1.txt".to_string(),
    ];
    let import = vec![
        "import_settlement".to_string(),
        "stripe".to_string(),
        "projects/payment_ops/fixtures/stripe_settlement_day1.txt".to_string(),
    ];
    let reconcile = vec!["reconcile".to_string(), "stripe".to_string()];

    match workload {
        "ingest_webhooks" => Some((vec![], vec![ingest])),
        "import_settlement" => Some((vec![ingest], vec![import])),
        "reconcile" => Some((vec![ingest, import], vec![reconcile])),
        "show_payment" => Some((
            vec![ingest, import, reconcile],
            vec![vec!["show_payment".to_string(), "pay-2".to_string()]],
        )),
        "list_cases" => Some((
            vec![ingest, import, reconcile],
            vec![vec!["list_cases".to_string(), "open".to_string()]],
        )),
        "provider_summary" => Some((
            vec![ingest, import, reconcile],
            vec![vec!["provider_summary".to_string(), "stripe".to_string()]],
        )),
        "show_audit" => Some((
            vec![ingest, import, reconcile],
            vec![vec!["show_audit".to_string()]],
        )),
        _ => None,
    }
}

fn selected_apps(cfg: &Config) -> Vec<AppKind> {
    AppKind::all()
        .iter()
        .copied()
        .filter(|app| cfg.app.as_deref().is_none_or(|name| name == app.name()))
        .collect()
}

fn selected_workloads(app: AppKind, cfg: &Config) -> Vec<&'static str> {
    match app {
        AppKind::WorkflowEngine => vec!["seed_tasks", "list_tasks", "show_task", "run_rules"],
        AppKind::PaymentOps => vec![
            "ingest_webhooks",
            "import_settlement",
            "reconcile",
            "show_payment",
            "list_cases",
            "provider_summary",
            "show_audit",
        ],
    }
    .into_iter()
    .filter(|name| cfg.workload.as_deref().is_none_or(|filter| filter == *name))
    .collect()
}

fn reset_store(app: AppKind) -> Result<(), String> {
    let path = Path::new(app.data_dir());
    if path.exists() {
        fs::remove_dir_all(path)
            .map_err(|e| format!("cannot remove '{}': {}", path.display(), e))?;
    }
    Ok(())
}

fn run_app_command(
    runtime: RuntimeKind,
    app: AppKind,
    repo_root: &Path,
    aver_bin: &Path,
    generated_bin: &Path,
    args: &[String],
) -> Result<(), String> {
    let mut cmd = match runtime {
        RuntimeKind::Vm => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(repo_root.join(app.entry_file()))
                .arg("--module-root")
                .arg(repo_root.join(app.module_root()))
                .arg("--");
            cmd
        }
        RuntimeKind::Wasm => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(repo_root.join(app.entry_file()))
                .arg("--module-root")
                .arg(repo_root.join(app.module_root()))
                .arg("--wasm")
                .arg("--");
            cmd
        }
        RuntimeKind::Generated => {
            let mut cmd = Command::new(generated_bin);
            cmd.current_dir(repo_root);
            cmd
        }
        RuntimeKind::SelfHost => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(repo_root.join(app.entry_file()))
                .arg("--module-root")
                .arg(repo_root.join(app.module_root()))
                .arg("--self-host")
                .arg("--");
            cmd
        }
    };

    cmd.args(args);
    run_silent(
        &mut cmd,
        &format!("{} {} {}", app.name(), runtime.name(), args.join(" ")),
    )
}

fn app_iteration(
    runtime: RuntimeKind,
    app: AppKind,
    workload: &str,
    seed: usize,
    ctx: &RunContext<'_>,
) -> Result<f64, String> {
    let (prepare, measure) = match app {
        AppKind::WorkflowEngine => workflow_commands(workload, seed),
        AppKind::PaymentOps => payment_commands(workload),
    }
    .ok_or_else(|| format!("unknown workload '{}' for {}", workload, app.name()))?;

    reset_store(app)?;
    for args in &prepare {
        run_app_command(
            runtime,
            app,
            ctx.repo_root,
            ctx.aver_bin,
            ctx.generated_bin,
            args,
        )?;
    }

    let started = Instant::now();
    for args in &measure {
        run_app_command(
            runtime,
            app,
            ctx.repo_root,
            ctx.aver_bin,
            ctx.generated_bin,
            args,
        )?;
    }
    Ok(started.elapsed().as_secs_f64() * 1000.0)
}

fn bench_app_runtime(
    runtime: RuntimeKind,
    app: AppKind,
    workload: &str,
    seed: usize,
    ctx: &RunContext<'_>,
    warmup: usize,
    iters: usize,
) -> Result<f64, String> {
    for _ in 0..warmup {
        let _ = app_iteration(runtime, app, workload, seed, ctx)?;
    }

    let mut times = Vec::new();
    for _ in 0..iters {
        times.push(app_iteration(runtime, app, workload, seed, ctx)?);
    }
    Ok(median(times))
}

fn print_app_results(results: &[AppResult], full: bool) {
    println!();
    println!("App benchmarks — end-to-end wall time (median)");
    if !full {
        println!(
            "default app scale uses workflow_engine seed={}; pass --full or --seed N for larger workloads",
            DEFAULT_WORKFLOW_APP_SEED
        );
    }
    println!("{:-<140}", "");
    println!(
        "{:<18} {:<18} {:>12} {:>12} {:>12} {:>12} {:>18}",
        "App", "Workload", "VM", "WASM", "Generated", "Self-host", "Best vs VM"
    );
    println!("{:-<140}", "");
    for result in results {
        let best = best_vs_vm(
            result.vm_ms,
            &[
                ("wasm", result.wasm_ms),
                ("gen", result.generated_ms),
                ("selfhost", result.selfhost_ms),
            ],
        );
        println!(
            "{:<18} {:<18} {:>10.3}ms {:>10.3}ms {:>10.3}ms {:>10.3}ms {:>18}",
            result.app,
            result.workload,
            result.vm_ms,
            result.wasm_ms,
            result.generated_ms,
            result.selfhost_ms,
            best
        );
    }
    println!("{:-<140}", "");
}

fn benchmark_apps(
    cfg: &Config,
    repo_root: &Path,
    aver_bin: &Path,
) -> Result<Vec<AppResult>, String> {
    let mut results = Vec::new();
    for app in selected_apps(cfg) {
        let app_seed = effective_app_seed(app, cfg);
        let workloads = selected_workloads(app, cfg);
        if workloads.is_empty() {
            return Err(format!("no workloads matched for {}", app.name()));
        }
        let generated_output = cfg
            .output_dir
            .join("generated")
            .join("apps")
            .join(app.name());
        let entry_file = repo_root.join(app.entry_file());
        let module_root = repo_root.join(app.module_root());
        let (generated_bin, generated_build_ms) = ensure_generated_project(
            repo_root,
            aver_bin,
            GeneratedProjectSpec {
                file: &entry_file,
                module_root: Some(&module_root),
                output_dir: &generated_output,
                project_name: app.generated_name(),
                override_main: None,
            },
            cfg.rebuild,
        )?;
        let ctx = RunContext {
            repo_root,
            aver_bin,
            generated_bin: &generated_bin,
        };
        println!(
            "{} generated build: {:.3}ms",
            app.name(),
            generated_build_ms
        );

        for workload in workloads {
            let vm_ms = bench_app_runtime(
                RuntimeKind::Vm,
                app,
                workload,
                app_seed,
                &ctx,
                cfg.warmup,
                cfg.iters,
            )?;
            let wasm_ms = bench_app_runtime(
                RuntimeKind::Wasm,
                app,
                workload,
                app_seed,
                &ctx,
                cfg.warmup,
                cfg.iters,
            )?;
            let generated_ms = bench_app_runtime(
                RuntimeKind::Generated,
                app,
                workload,
                app_seed,
                &ctx,
                cfg.warmup,
                cfg.iters,
            )?;
            let selfhost_ms = bench_app_runtime(
                RuntimeKind::SelfHost,
                app,
                workload,
                app_seed,
                &ctx,
                cfg.warmup,
                cfg.iters,
            )?;
            results.push(AppResult {
                app: app.name(),
                workload,
                vm_ms,
                wasm_ms,
                generated_ms,
                selfhost_ms,
            });
        }
    }

    Ok(results)
}

fn main() {
    let cfg = match parse_args() {
        Ok(cfg) => cfg,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };

    if cfg.runtimes != RuntimeKind::all() {
        eprintln!(
            "runtime_bench currently measures all four runtimes together; selective --runtime filtering is not implemented yet"
        );
        std::process::exit(2);
    }

    let repo_root = repo_root();
    let aver_bin = match ensure_host_aver(&repo_root) {
        Ok(path) => path,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(1);
        }
    };

    println!(
        "runtime_bench suite={:?} runtimes={} full={} seed={} iters={} warmup={} output={}",
        cfg.suite,
        cfg.runtimes
            .iter()
            .map(|runtime| runtime.name())
            .collect::<Vec<_>>()
            .join(","),
        cfg.full,
        cfg.seed,
        cfg.iters,
        cfg.warmup,
        cfg.output_dir.display()
    );

    if matches!(cfg.suite, SuiteKind::Core | SuiteKind::All) {
        match benchmark_core(&cfg, &repo_root, &aver_bin) {
            Ok(results) => print_core_results(&results),
            Err(msg) => {
                eprintln!("{msg}");
                std::process::exit(1);
            }
        }
    }

    if matches!(cfg.suite, SuiteKind::Apps | SuiteKind::All) {
        match benchmark_apps(&cfg, &repo_root, &aver_bin) {
            Ok(results) => print_app_results(&results, cfg.full),
            Err(msg) => {
                eprintln!("{msg}");
                std::process::exit(1);
            }
        }
    }
}
