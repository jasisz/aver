use std::fs;
use std::path::Path;

use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
use aver::nan_value::Arena;
use aver::source::{parse_project_source, require_module_declaration};
use aver::vm::{self, VM, VmProfileReport};

const WORKFLOW_MODULE_ROOT: &str = "projects/workflow_engine";
const WORKFLOW_ENTRY_FILE: &str = "projects/workflow_engine/main.av";
const WORKFLOW_DATA_DIR: &str = "/tmp/aver_workflow_engine";
const DEFAULT_WORKFLOW_SEED: usize = 250;
const PAYMENT_MODULE_ROOT: &str = "projects/payment_ops";
const PAYMENT_ENTRY_FILE: &str = "projects/payment_ops/main.av";
const PAYMENT_DATA_DIR: &str = "/tmp/aver_payment_ops";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AppKind {
    WorkflowEngine,
    PaymentOps,
}

impl AppKind {
    fn parse(raw: &str) -> Option<Self> {
        match raw {
            "workflow_engine" => Some(Self::WorkflowEngine),
            "payment_ops" => Some(Self::PaymentOps),
            _ => None,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Self::WorkflowEngine => "workflow_engine",
            Self::PaymentOps => "payment_ops",
        }
    }

    fn entry_file(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_ENTRY_FILE,
            Self::PaymentOps => PAYMENT_ENTRY_FILE,
        }
    }

    fn module_root(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_MODULE_ROOT,
            Self::PaymentOps => PAYMENT_MODULE_ROOT,
        }
    }

    fn data_dir(self) -> &'static str {
        match self {
            Self::WorkflowEngine => WORKFLOW_DATA_DIR,
            Self::PaymentOps => PAYMENT_DATA_DIR,
        }
    }
}

#[derive(Debug)]
struct Config {
    app: AppKind,
    workload: String,
    seed: usize,
    top: usize,
    include_prepare: bool,
}

type CommandBatch = Vec<Vec<String>>;
type WorkloadPlan = (CommandBatch, CommandBatch);

fn usage() -> &'static str {
    "Usage: cargo run --release --bin vm_profile -- [--app workflow_engine|payment_ops] [--workload NAME] [--seed N] [--top N] [--include-prepare]"
}

fn parse_args() -> Result<Config, String> {
    let mut app = AppKind::WorkflowEngine;
    let mut workload = "list_tasks".to_string();
    let mut seed = DEFAULT_WORKFLOW_SEED;
    let mut top = 15usize;
    let mut include_prepare = false;

    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--app" => {
                let raw = args
                    .next()
                    .ok_or_else(|| "missing value for --app".to_string())?;
                app = AppKind::parse(&raw).ok_or_else(|| format!("unknown --app '{}'", raw))?;
                if app == AppKind::PaymentOps && workload == "list_tasks" {
                    workload = "show_payment".to_string();
                }
            }
            "--workload" => {
                workload = args
                    .next()
                    .ok_or_else(|| "missing value for --workload".to_string())?;
            }
            "--seed" => {
                let raw = args
                    .next()
                    .ok_or_else(|| "missing value for --seed".to_string())?;
                seed = raw
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --seed '{}'", raw))?;
            }
            "--top" => {
                let raw = args
                    .next()
                    .ok_or_else(|| "missing value for --top".to_string())?;
                top = raw
                    .parse::<usize>()
                    .map_err(|_| format!("invalid --top '{}'", raw))?;
            }
            "--include-prepare" => include_prepare = true,
            "--help" | "-h" => return Err(usage().to_string()),
            other => return Err(format!("unknown argument '{}'\n{}", other, usage())),
        }
    }

    Ok(Config {
        app,
        workload,
        seed,
        top,
        include_prepare,
    })
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

fn workload_plan(app: AppKind, workload: &str, seed: usize) -> Result<WorkloadPlan, String> {
    match app {
        AppKind::WorkflowEngine => workflow_commands(workload, seed),
        AppKind::PaymentOps => payment_commands(workload),
    }
    .ok_or_else(|| format!("unknown workload '{}' for {}", workload, app.name()))
}

fn reset_store(app: AppKind) -> Result<(), String> {
    let path = Path::new(app.data_dir());
    if path.exists() {
        fs::remove_dir_all(path)
            .map_err(|e| format!("cannot remove '{}': {}", path.display(), e))?;
    }
    Ok(())
}

fn run_vm_command(
    app: AppKind,
    args: &[String],
    profile: bool,
) -> Result<Option<VmProfileReport>, String> {
    let module_root = app.module_root();
    let file = app.entry_file();
    let source =
        fs::read_to_string(file).map_err(|e| format!("Cannot open file '{}': {}", file, e))?;
    let mut items = parse_project_source(&source, module_root, file).map_err(|e| e.to_string())?;
    require_module_declaration(&items, file).map_err(|e| e.to_string())?;

    // Preload dep modules so the entry's `SymbolTable` knows about
    // cross-module call targets — mirrors `cmd_run_vm` so the VM
    // compiler's per-dep resolver path drops out.
    let depends = items
        .iter()
        .find_map(|i| match i {
            aver::ast::TopLevel::Module(m) => Some(m.depends.clone()),
            _ => None,
        })
        .unwrap_or_default();
    let loaded =
        aver::source::load_module_tree(&depends, module_root).map_err(|e| e.to_string())?;
    let dep_modules: Vec<aver::codegen::ModuleInfo> = loaded
        .iter()
        .map(aver::codegen::ModuleInfo::from_loaded)
        .collect();

    // VM profiler runs the canonical full pipeline so the numbers reflect
    // what `aver run` actually executes.
    let pipeline_result = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        let errors = tc_result
            .errors
            .iter()
            .map(|err| format!("error[{}:{}]: {}", err.line, err.col, err.message))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(errors);
    }

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &pipeline_result.resolved_items,
        &pipeline_result.symbol_table,
        &mut arena,
        Some(module_root),
        file,
        pipeline_result.analysis.as_ref(),
    )
    .map_err(|e| format!("VM compile error: {}", e))?;
    let mut machine = VM::new(code, globals, arena);
    if let Some(config) = aver::config::ProjectConfig::load_from_dir(Path::new(module_root))
        .map_err(|e| format!("aver.toml: {}", e))?
    {
        machine.set_runtime_policy(config);
    }
    machine.set_cli_args(args.to_vec());
    machine.set_silent_console(true);
    if profile {
        machine.start_profiling();
    }

    let result = machine.run().map_err(|e| e.to_string())?;
    if result.is_err() {
        let inner = result.wrapper_inner(&machine.arena);
        return Err(format!(
            "Main returned error: {}",
            inner.repr(&machine.arena)
        ));
    }

    Ok(profile.then(|| {
        machine
            .profile_report()
            .expect("profile should exist after start_profiling")
    }))
}

fn print_return_summary(report: &VmProfileReport) {
    let stats = &report.returns;
    println!("Return paths");
    println!(
        "  total entries: {} | thin: {} | parent-thin: {}",
        stats.total_entries, stats.thin_entries, stats.parent_thin_entries
    );
    if stats.thin_entries > 0 {
        println!(
            "  thin fast: {} ({:.1}%) | thin slow: {}",
            stats.thin_fast_returns,
            (stats.thin_fast_returns as f64 / stats.thin_entries as f64) * 100.0,
            stats.thin_slow_returns
        );
    }
    if stats.parent_thin_entries > 0 {
        println!(
            "  parent-thin fast: {} ({:.1}%) | parent-thin slow: {}",
            stats.parent_thin_fast_returns,
            (stats.parent_thin_fast_returns as f64 / stats.parent_thin_entries as f64) * 100.0,
            stats.parent_thin_slow_returns
        );
    }
    println!(
        "  young-truncate fast: {} | regular slow: {}",
        stats.young_truncate_fast_returns, stats.regular_slow_returns
    );
}

fn print_top_opcodes(report: &VmProfileReport, top: usize) {
    println!();
    println!("Top opcodes");
    for entry in report.opcodes.iter().take(top) {
        let share = if report.total_opcodes == 0 {
            0.0
        } else {
            (entry.count as f64 / report.total_opcodes as f64) * 100.0
        };
        println!("  {:<18} {:>10} {:>6.1}%", entry.name, entry.count, share);
    }
}

fn print_top_functions(report: &VmProfileReport, top: usize) {
    println!();
    println!("Top functions");
    for entry in report.functions.iter().take(top) {
        let class = if entry.parent_thin {
            "parent-thin"
        } else if entry.thin {
            "thin"
        } else {
            "normal"
        };
        println!(
            "  {:<40} {:<12} entries={:<8} fast={:<8} ytrunc={:<8} slow={:<8}",
            entry.name,
            class,
            entry.entries,
            entry.fast_returns,
            entry.young_truncate_fast_returns,
            entry.slow_returns
        );
    }
}

fn print_top_builtin_calls(report: &VmProfileReport, top: usize) {
    if report.builtins.is_empty() {
        return;
    }
    println!();
    println!("Top builtins");
    for entry in report.builtins.iter().take(top) {
        println!("  {:<28} {}", entry.name, entry.count);
    }
}

fn print_slowest_parent_thin(report: &VmProfileReport, top: usize) {
    let mut candidates = report
        .functions
        .iter()
        .filter(|entry| entry.parent_thin && entry.slow_returns > 0)
        .collect::<Vec<_>>();
    if candidates.is_empty() {
        return;
    }
    candidates.sort_by(|a, b| {
        b.slow_returns
            .cmp(&a.slow_returns)
            .then_with(|| a.name.cmp(&b.name))
    });
    println!();
    println!("Parent-thin slow-path suspects");
    for entry in candidates.into_iter().take(top) {
        println!(
            "  {:<40} slow={} fast={} ytrunc={}",
            entry.name, entry.slow_returns, entry.fast_returns, entry.young_truncate_fast_returns
        );
    }
}

fn main() {
    let cfg = match parse_args() {
        Ok(cfg) => cfg,
        Err(msg) => {
            eprintln!("{msg}");
            std::process::exit(1);
        }
    };

    let (prepare, measure) = match workload_plan(cfg.app, &cfg.workload, cfg.seed) {
        Ok(plan) => plan,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
    };

    if let Err(err) = reset_store(cfg.app) {
        eprintln!("{err}");
        std::process::exit(1);
    }

    let mut aggregate: Option<VmProfileReport> = None;
    let mut profiled_commands = 0usize;

    for args in &prepare {
        let should_profile = cfg.include_prepare;
        match run_vm_command(cfg.app, args, should_profile) {
            Ok(Some(report)) => {
                profiled_commands += 1;
                if let Some(aggregate_report) = &mut aggregate {
                    aggregate_report.merge(&report);
                } else {
                    aggregate = Some(report);
                }
            }
            Ok(None) => {}
            Err(err) => {
                eprintln!("prepare command '{}' failed: {}", args.join(" "), err);
                std::process::exit(1);
            }
        }
    }

    for args in &measure {
        match run_vm_command(cfg.app, args, true) {
            Ok(Some(report)) => {
                profiled_commands += 1;
                if let Some(aggregate_report) = &mut aggregate {
                    aggregate_report.merge(&report);
                } else {
                    aggregate = Some(report);
                }
            }
            Ok(None) => unreachable!("measure commands are always profiled"),
            Err(err) => {
                eprintln!("measure command '{}' failed: {}", args.join(" "), err);
                std::process::exit(1);
            }
        }
    }

    let report = aggregate.expect("at least one profiled command should run");
    println!(
        "VM profile — app={} workload={} seed={} profiled_commands={}",
        cfg.app.name(),
        cfg.workload,
        cfg.seed,
        profiled_commands
    );
    print_return_summary(&report);
    print_top_opcodes(&report, cfg.top);
    print_top_functions(&report, cfg.top);
    print_top_builtin_calls(&report, cfg.top);
    print_slowest_parent_thin(&report, cfg.top);
}
