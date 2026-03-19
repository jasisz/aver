use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};
use std::time::{Duration, Instant};

const MODULE_ROOT: &str = "projects/workflow_engine";
const ENTRY_FILE: &str = "projects/workflow_engine/main.av";
const DATA_DIR: &str = "/tmp/aver_workflow_engine";
const DEFAULT_OUTPUT_DIR: &str = "/tmp/aver_workflow_codegen_bench";
const GENERATED_NAME: &str = "workflow_codegen_bench";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum RuntimeKind {
    Interpreter,
    Vm,
    Generated,
}

impl RuntimeKind {
    fn all() -> &'static [RuntimeKind] {
        &[
            RuntimeKind::Interpreter,
            RuntimeKind::Vm,
            RuntimeKind::Generated,
        ]
    }

    fn name(self) -> &'static str {
        match self {
            RuntimeKind::Interpreter => "interpreter",
            RuntimeKind::Vm => "vm",
            RuntimeKind::Generated => "generated",
        }
    }

    fn parse(input: &str) -> Option<Vec<RuntimeKind>> {
        match input {
            "interpreter" => Some(vec![RuntimeKind::Interpreter]),
            "vm" => Some(vec![RuntimeKind::Vm]),
            "generated" => Some(vec![RuntimeKind::Generated]),
            "both" => Some(vec![RuntimeKind::Interpreter, RuntimeKind::Generated]),
            "all" => Some(Self::all().to_vec()),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Workload {
    SeedTasks,
    ListTasks,
    ShowTask,
    RunRules,
}

impl Workload {
    fn all() -> &'static [Workload] {
        &[
            Workload::SeedTasks,
            Workload::ListTasks,
            Workload::ShowTask,
            Workload::RunRules,
        ]
    }

    fn name(self) -> &'static str {
        match self {
            Workload::SeedTasks => "seed_tasks",
            Workload::ListTasks => "list_tasks",
            Workload::ShowTask => "show_task",
            Workload::RunRules => "run_rules",
        }
    }

    fn parse(input: &str) -> Option<Vec<Workload>> {
        match input {
            "seed_tasks" => Some(vec![Workload::SeedTasks]),
            "list_tasks" => Some(vec![Workload::ListTasks]),
            "show_task" => Some(vec![Workload::ShowTask]),
            "run_rules" => Some(vec![Workload::RunRules]),
            "all" => Some(Self::all().to_vec()),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Config {
    seed: usize,
    iters: usize,
    runtimes: Vec<RuntimeKind>,
    workloads: Vec<Workload>,
    output_dir: PathBuf,
    rebuild: bool,
}

#[derive(Debug)]
struct BenchResult {
    runtime: RuntimeKind,
    workload: Workload,
    seed_elapsed: Duration,
    measured: Duration,
}

fn usage() -> &'static str {
    "Usage: cargo run --release --bin workflow_codegen_bench -- [--seed N] [--iters N] [--runtime interpreter|vm|generated|both|all] [--workload NAME|all] [--output DIR] [--rebuild]"
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

fn generated_binary_path(output_dir: &Path) -> PathBuf {
    output_dir.join("target").join("release").join(format!(
        "{}{}",
        GENERATED_NAME,
        env::consts::EXE_SUFFIX
    ))
}

fn parse_args() -> Result<Config, String> {
    let mut seed = 1_000usize;
    let mut iters = 3usize;
    let mut runtimes = RuntimeKind::all().to_vec();
    let mut workloads = Workload::all().to_vec();
    let mut output_dir = PathBuf::from(DEFAULT_OUTPUT_DIR);
    let mut rebuild = false;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--seed" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --seed".to_string())?;
                seed = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --seed '{}'", value))?;
            }
            "--iters" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --iters".to_string())?;
                iters = value
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --iters '{}'", value))?;
            }
            "--runtime" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --runtime".to_string())?;
                runtimes = RuntimeKind::parse(&value)
                    .ok_or_else(|| format!("unknown --runtime '{}'", value))?;
            }
            "--workload" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --workload".to_string())?;
                workloads = Workload::parse(&value)
                    .ok_or_else(|| format!("unknown --workload '{}'", value))?;
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
        seed,
        iters,
        runtimes,
        workloads,
        output_dir,
        rebuild,
    })
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

fn render_status(status: ExitStatus) -> String {
    match status.code() {
        Some(code) => format!("code {}", code),
        None => "signal".to_string(),
    }
}

fn ensure_host_aver(repo_root: &Path) -> Result<PathBuf, String> {
    let aver = host_aver_path(repo_root);
    let mut cmd = Command::new("cargo");
    cmd.current_dir(repo_root)
        .arg("build")
        .arg("--release")
        .arg("--bin")
        .arg("aver");
    run_silent(&mut cmd, "build host aver")?;
    Ok(aver)
}

fn ensure_generated_project(
    repo_root: &Path,
    aver_bin: &Path,
    output_dir: &Path,
    rebuild: bool,
) -> Result<PathBuf, String> {
    if rebuild && output_dir.exists() {
        fs::remove_dir_all(output_dir).map_err(|e| {
            format!(
                "cannot remove generated output '{}': {}",
                output_dir.display(),
                e
            )
        })?;
    }

    if rebuild || !output_dir.join("Cargo.toml").exists() {
        let mut cmd = Command::new(aver_bin);
        cmd.current_dir(repo_root)
            .env("AVER_RUNTIME_PATH", repo_root.join("aver-rt"))
            .arg("compile")
            .arg(repo_root.join(ENTRY_FILE))
            .arg("--module-root")
            .arg(repo_root.join(MODULE_ROOT))
            .arg("--output")
            .arg(output_dir)
            .arg("--name")
            .arg(GENERATED_NAME);
        run_silent(&mut cmd, "compile workflow_engine to generated Rust")?;
    }

    let mut build_cmd = Command::new("cargo");
    build_cmd
        .current_dir(output_dir)
        .arg("build")
        .arg("--release");
    run_silent(&mut build_cmd, "build generated Rust project")?;

    Ok(generated_binary_path(output_dir))
}

fn reset_store() -> Result<(), String> {
    let path = Path::new(DATA_DIR);
    if path.exists() {
        fs::remove_dir_all(path)
            .map_err(|e| format!("cannot remove '{}': {}", path.display(), e))?;
    }
    Ok(())
}

fn create_task_args(task_index: usize) -> Vec<String> {
    vec![
        "create_task".to_string(),
        "alpha".to_string(),
        format!("t{}", task_index),
        format!("Task {}", task_index),
        "normal".to_string(),
        "-".to_string(),
        "-".to_string(),
    ]
}

fn query_args(workload: Workload, seed: usize) -> Vec<String> {
    match workload {
        Workload::SeedTasks => vec![],
        Workload::ListTasks => vec!["list_tasks".to_string()],
        Workload::ShowTask => vec!["show_task".to_string(), format!("t{}", seed)],
        Workload::RunRules => vec!["run_rules".to_string()],
    }
}

fn run_workflow_command(
    runtime: RuntimeKind,
    repo_root: &Path,
    aver_bin: &Path,
    generated_bin: &Path,
    args: &[String],
) -> Result<(), String> {
    let context = format!("{} {}", runtime.name(), args.join(" "));
    let mut cmd = match runtime {
        RuntimeKind::Interpreter => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(repo_root.join(ENTRY_FILE))
                .arg("--module-root")
                .arg(repo_root.join(MODULE_ROOT))
                .arg("--");
            cmd
        }
        RuntimeKind::Vm => {
            let mut cmd = Command::new(aver_bin);
            cmd.current_dir(repo_root)
                .arg("run")
                .arg(repo_root.join(ENTRY_FILE))
                .arg("--module-root")
                .arg(repo_root.join(MODULE_ROOT))
                .arg("--vm")
                .arg("--");
            cmd
        }
        RuntimeKind::Generated => {
            let mut cmd = Command::new(generated_bin);
            cmd.current_dir(repo_root);
            cmd
        }
    };

    cmd.args(args);
    run_silent(&mut cmd, &context)
}

fn seed_tasks(
    runtime: RuntimeKind,
    repo_root: &Path,
    aver_bin: &Path,
    generated_bin: &Path,
    seed: usize,
) -> Result<Duration, String> {
    reset_store()?;

    run_workflow_command(
        runtime,
        repo_root,
        aver_bin,
        generated_bin,
        &[
            "create_project".to_string(),
            "alpha".to_string(),
            "Alpha".to_string(),
        ],
    )?;

    let started = Instant::now();
    for idx in 1..=seed {
        run_workflow_command(
            runtime,
            repo_root,
            aver_bin,
            generated_bin,
            &create_task_args(idx),
        )?;
    }
    Ok(started.elapsed())
}

fn benchmark_query(
    runtime: RuntimeKind,
    repo_root: &Path,
    aver_bin: &Path,
    generated_bin: &Path,
    args: &[String],
    iters: usize,
) -> Result<Duration, String> {
    run_workflow_command(runtime, repo_root, aver_bin, generated_bin, args)?;

    let started = Instant::now();
    for _ in 0..iters {
        run_workflow_command(runtime, repo_root, aver_bin, generated_bin, args)?;
    }
    Ok(started.elapsed())
}

fn run_workload(
    runtime: RuntimeKind,
    repo_root: &Path,
    aver_bin: &Path,
    generated_bin: &Path,
    workload: Workload,
    seed: usize,
    iters: usize,
) -> Result<BenchResult, String> {
    let seed_elapsed = seed_tasks(runtime, repo_root, aver_bin, generated_bin, seed)?;
    let measured = match workload {
        Workload::SeedTasks => seed_elapsed,
        _ => benchmark_query(
            runtime,
            repo_root,
            aver_bin,
            generated_bin,
            &query_args(workload, seed),
            iters,
        )?,
    };

    Ok(BenchResult {
        runtime,
        workload,
        seed_elapsed,
        measured,
    })
}

fn print_result(result: &BenchResult, seed: usize, iters: usize) {
    if result.workload == Workload::SeedTasks {
        let avg = if seed == 0 {
            Duration::ZERO
        } else {
            result.measured / seed as u32
        };
        println!(
            "{:<12} {:<12} total={:?} avg_per_task={:?}",
            result.runtime.name(),
            result.workload.name(),
            result.measured,
            avg
        );
    } else {
        let avg = if iters == 0 {
            Duration::ZERO
        } else {
            result.measured / iters as u32
        };
        println!(
            "{:<12} {:<12} seed={:?} total={:?} avg={:?}",
            result.runtime.name(),
            result.workload.name(),
            result.seed_elapsed,
            result.measured,
            avg
        );
    }
}

fn print_pair_speedup(results: &[BenchResult]) {
    for workload in Workload::all() {
        let interpreter = results
            .iter()
            .find(|r| r.runtime == RuntimeKind::Interpreter && r.workload == *workload);
        let vm = results
            .iter()
            .find(|r| r.runtime == RuntimeKind::Vm && r.workload == *workload);
        let generated = results
            .iter()
            .find(|r| r.runtime == RuntimeKind::Generated && r.workload == *workload);
        let Some(interpreter) = interpreter else {
            continue;
        };

        let interp_secs = interpreter.measured.as_secs_f64();
        if let Some(vm) = vm {
            let vm_secs = vm.measured.as_secs_f64();
            if vm_secs > 0.0 {
                println!(
                    "{:<12} vm_speedup={:.2}x",
                    workload.name(),
                    interp_secs / vm_secs
                );
            }
        }

        let Some(generated) = generated else {
            continue;
        };
        let gen_secs = generated.measured.as_secs_f64();
        if gen_secs > 0.0 {
            println!(
                "{:<12} generated_speedup={:.2}x",
                workload.name(),
                interp_secs / gen_secs
            );
        }
    }
}

fn main() {
    let cfg = match parse_args() {
        Ok(cfg) => cfg,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };

    let repo_root = repo_root();
    let aver_bin = match ensure_host_aver(&repo_root) {
        Ok(path) => path,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(1);
        }
    };

    let generated_bin = if cfg.runtimes.contains(&RuntimeKind::Generated) {
        match ensure_generated_project(&repo_root, &aver_bin, &cfg.output_dir, cfg.rebuild) {
            Ok(path) => path,
            Err(msg) => {
                eprintln!("{msg}");
                std::process::exit(1);
            }
        }
    } else {
        generated_binary_path(&cfg.output_dir)
    };

    println!(
        "workflow_codegen_bench seed={} iters={} runtimes={} workloads={} output={}",
        cfg.seed,
        cfg.iters,
        cfg.runtimes
            .iter()
            .map(|runtime| runtime.name())
            .collect::<Vec<_>>()
            .join(","),
        cfg.workloads
            .iter()
            .map(|workload| workload.name())
            .collect::<Vec<_>>()
            .join(","),
        cfg.output_dir.display()
    );

    let mut results = Vec::new();
    for runtime in &cfg.runtimes {
        for workload in &cfg.workloads {
            match run_workload(
                *runtime,
                &repo_root,
                &aver_bin,
                &generated_bin,
                *workload,
                cfg.seed,
                cfg.iters,
            ) {
                Ok(result) => {
                    print_result(&result, cfg.seed, cfg.iters);
                    results.push(result);
                }
                Err(msg) => {
                    eprintln!("{} {} failed: {}", runtime.name(), workload.name(), msg);
                    std::process::exit(1);
                }
            }
        }
    }

    if cfg.runtimes.len() >= 2 {
        print_pair_speedup(&results);
    }
}
