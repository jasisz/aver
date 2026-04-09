use std::env;
use std::fs;
use std::hint::black_box;
use std::path::Path;
use std::time::{Duration, Instant};

use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::resolver;
use aver::source::{parse_source, require_module_declaration};
use aver::tco;
use aver::types::checker::run_type_check_full;
use aver::value::{Value, aver_repr, list_from_vec};
use aver::vm;

const MODULE_ROOT: &str = "projects/workflow_engine";
const ENTRY_FILE: &str = "projects/workflow_engine/app/cli.av";
const DATA_DIR: &str = "/tmp/aver_workflow_engine";

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

    fn parse(name: &str) -> Option<Workload> {
        Self::all().iter().copied().find(|w| w.name() == name)
    }
}

fn usage() -> &'static str {
    "Usage: cargo run --release --bin workflow_bench -- [--seed N] [--iters N] [--workload NAME|all]"
}

fn parse_args() -> Result<(usize, usize, Option<Workload>), String> {
    let mut seed = 20_000usize;
    let mut iters = 5usize;
    let mut workload = None;

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
            "--workload" => {
                let value = args
                    .next()
                    .ok_or_else(|| "missing value for --workload".to_string())?;
                if value != "all" {
                    workload = Some(
                        Workload::parse(&value)
                            .ok_or_else(|| format!("unknown workload '{}'", value))?,
                    );
                }
            }
            "--help" | "-h" => return Err(usage().to_string()),
            other => return Err(format!("unknown argument '{}'\n{}", other, usage())),
        }
    }

    Ok((seed, iters, workload))
}

fn read_file(path: &str) -> Result<String, String> {
    fs::read_to_string(path).map_err(|e| format!("Cannot open file '{}': {}", path, e))
}

fn build_vm() -> Result<vm::VM, String> {
    let source = read_file(ENTRY_FILE)?;
    let mut items = parse_source(&source)?;
    require_module_declaration(&items, ENTRY_FILE)?;
    tco::transform_program(&mut items);

    let tc_result = run_type_check_full(&items, Some(MODULE_ROOT));
    if !tc_result.errors.is_empty() {
        let rendered = tc_result
            .errors
            .iter()
            .map(|err| format!("error[{}:{}]: {}", err.line, err.col, err.message))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(rendered);
    }

    resolver::resolve_program(&mut items);

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) =
        vm::compile_program_with_modules(&items, &mut arena, Some(MODULE_ROOT), ENTRY_FILE)
            .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run_top_level().map_err(|e| format!("{}", e))?;
    Ok(machine)
}

fn call_fn(machine: &mut vm::VM, name: &str, args: Vec<Value>) -> Result<Value, String> {
    let nv_args: Vec<NanValue> = args
        .iter()
        .map(|v| NanValue::from_value(v, &mut machine.arena))
        .collect();
    let result = machine
        .run_named_function(name, &nv_args)
        .map_err(|e| e.to_string())?;
    Ok(result.to_value(&machine.arena))
}

fn unwrap_ok(value: Value, context: &str) -> Result<Value, String> {
    match value {
        Value::Ok(inner) => Ok(*inner),
        Value::Err(inner) => Err(format!("{} failed: {}", context, aver_repr(&inner))),
        other => Err(format!(
            "{} returned non-Result value: {}",
            context,
            aver_repr(&other)
        )),
    }
}

fn string_list<I>(items: I) -> Value
where
    I: IntoIterator<Item = String>,
{
    list_from_vec(items.into_iter().map(Value::Str).collect())
}

fn execute_args(machine: &mut vm::VM, args: Vec<String>) -> Result<Value, String> {
    let value = call_fn(machine, "executeArgs", vec![string_list(args.clone())])?;
    unwrap_ok(value, &format!("executeArgs({})", args.join(" ")))
}

fn reset_store() -> Result<(), String> {
    let path = Path::new(DATA_DIR);
    if path.exists() {
        fs::remove_dir_all(path)
            .map_err(|e| format!("cannot remove '{}': {}", path.display(), e))?;
    }
    Ok(())
}

fn init_store(machine: &mut vm::VM) -> Result<(), String> {
    let _ = unwrap_ok(call_fn(machine, "initCli", vec![])?, "initCli")?;
    Ok(())
}

fn create_project(machine: &mut vm::VM) -> Result<(), String> {
    let _ = execute_args(
        machine,
        vec![
            "create_project".to_string(),
            "alpha".to_string(),
            "Alpha".to_string(),
        ],
    )?;
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

fn seed_tasks(machine: &mut vm::VM, seed: usize) -> Result<Duration, String> {
    reset_store()?;
    init_store(machine)?;
    create_project(machine)?;

    let started = Instant::now();
    for idx in 1..=seed {
        black_box(execute_args(machine, create_task_args(idx))?);
    }
    Ok(started.elapsed())
}

fn benchmark_query(
    machine: &mut vm::VM,
    args: Vec<String>,
    iters: usize,
) -> Result<Duration, String> {
    black_box(execute_args(machine, args.clone())?);
    let started = Instant::now();
    for _ in 0..iters {
        black_box(execute_args(machine, args.clone())?);
    }
    Ok(started.elapsed())
}

fn run_workload(
    workload: Workload,
    seed: usize,
    iters: usize,
) -> Result<(Duration, Duration), String> {
    let mut machine = build_vm()?;
    let seed_elapsed = seed_tasks(&mut machine, seed)?;

    let measured = match workload {
        Workload::SeedTasks => seed_elapsed,
        Workload::ListTasks => {
            benchmark_query(&mut machine, vec!["list_tasks".to_string()], iters)?
        }
        Workload::ShowTask => benchmark_query(
            &mut machine,
            vec!["show_task".to_string(), format!("t{}", seed)],
            iters,
        )?,
        Workload::RunRules => benchmark_query(&mut machine, vec!["run_rules".to_string()], iters)?,
    };

    Ok((seed_elapsed, measured))
}

fn main() {
    let (seed, iters, workload) = match parse_args() {
        Ok(cfg) => cfg,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(2);
        }
    };

    let workloads = match workload {
        Some(one) => vec![one],
        None => Workload::all().to_vec(),
    };

    println!("workflow_bench seed={seed} iters={iters}");
    for workload in workloads {
        match run_workload(workload, seed, iters) {
            Ok((seed_elapsed, measured)) => {
                if workload == Workload::SeedTasks {
                    let avg = if seed == 0 {
                        Duration::ZERO
                    } else {
                        measured / seed as u32
                    };
                    println!(
                        "{:<18} total={:?} avg_per_task={:?}",
                        workload.name(),
                        measured,
                        avg
                    );
                } else {
                    let avg = measured / iters as u32;
                    println!(
                        "{:<18} seed={:?} total={:?} avg={:?}",
                        workload.name(),
                        seed_elapsed,
                        measured,
                        avg
                    );
                }
            }
            Err(msg) => {
                eprintln!("{} failed: {}", workload.name(), msg);
                std::process::exit(1);
            }
        }
    }
}
