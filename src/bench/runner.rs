//! VM scenario runner — compile entry once, run N+warmup times timing
//! each iteration.
//!
//! 0.15.1 scope is wall time only. Stdout is silenced (via the runtime's
//! silent-console switch) so program output doesn't pollute the bench's
//! own JSON output, but bytes are not captured. `response_bytes` lands
//! in 0.15.2 alongside the alloc counters; the JSON shape carries the
//! field as `Option<usize>` from day one so the contract is stable.

use std::time::Instant;

use crate::ast::TopLevel;
use crate::bench::manifest::Manifest;
use crate::bench::report::{BenchReport, IterationStats, ScenarioMetadata};
use crate::ir::{PipelineConfig, PipelineStage, TypecheckMode};
use crate::nan_value::Arena;
use crate::source::parse_source;
use crate::vm;

#[derive(Debug)]
pub enum RunError {
    Read(String),
    Parse(String),
    Typecheck(String),
    Compile(String),
    Runtime(String),
}

impl std::fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Read(m)
            | Self::Parse(m)
            | Self::Typecheck(m)
            | Self::Compile(m)
            | Self::Runtime(m) => f.write_str(m),
        }
    }
}

/// Run `manifest` against the VM target and produce a `BenchReport`.
pub fn run_vm_scenario(manifest: &Manifest) -> Result<BenchReport, RunError> {
    let entry_str = manifest.entry.to_string_lossy().into_owned();
    let module_root = manifest
        .entry
        .parent()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_default();

    let source = std::fs::read_to_string(&manifest.entry)
        .map_err(|e| RunError::Read(format!("{}: {}", entry_str, e)))?;
    let mut items: Vec<TopLevel> = parse_source(&source).map_err(RunError::Parse)?;

    let passes_applied = std::cell::RefCell::new(Vec::<String>::new());
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            on_after_pass: Some(Box::new(|stage: PipelineStage, _| {
                passes_applied.borrow_mut().push(stage.name().to_string());
            })),
            ..Default::default()
        },
    );
    let tc_result = pipeline_result.typecheck.expect("typecheck was requested");
    if !tc_result.errors.is_empty() {
        let msg = tc_result
            .errors
            .iter()
            .map(|err| format!("error[{}:{}]: {}", err.line, err.col, err.message))
            .collect::<Vec<_>>()
            .join("\n");
        return Err(RunError::Typecheck(msg));
    }

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &items,
        &mut arena,
        Some(&module_root),
        &entry_str,
        pipeline_result.analysis.as_ref(),
    )
    .map_err(|e| RunError::Compile(format!("VM compile: {}", e)))?;

    let mut samples: Vec<f64> = Vec::with_capacity(manifest.iterations);

    // Warmup runs are not timed; they exist to settle JIT-like effects
    // (alloc-pool growth, branch predictor, OS page cache) before the
    // recorded iterations.
    for _ in 0..manifest.warmup {
        run_one(&code, &globals, &arena, &manifest.args)?;
    }
    for _ in 0..manifest.iterations {
        let t = Instant::now();
        run_one(&code, &globals, &arena, &manifest.args)?;
        let elapsed_ms = t.elapsed().as_secs_f64() * 1000.0;
        samples.push(elapsed_ms);
    }

    let stats = IterationStats::from_samples(&samples);

    Ok(BenchReport {
        scenario: ScenarioMetadata {
            name: manifest.name.clone(),
            entry: entry_str,
            target: "vm".to_string(),
            iterations_count: manifest.iterations,
            warmup_count: manifest.warmup,
        },
        iterations: stats,
        // 0.15.2 will populate these once stdout capture + IR-level alloc
        // counter are wired up. Field is in the JSON shape from day one
        // so consumers can rely on its presence.
        response_bytes: None,
        expected_match: None,
        passes_applied: passes_applied.into_inner(),
        compiler_visible_allocs: None,
    })
}

fn run_one(
    code: &vm::CodeStore,
    globals: &[crate::nan_value::NanValue],
    arena: &Arena,
    args: &[String],
) -> Result<(), RunError> {
    let mut machine = vm::VM::new(code.clone(), globals.to_vec(), arena.clone());
    machine.set_silent_console(true);
    machine.set_cli_args(args.to_vec());
    machine
        .run()
        .map_err(|e| RunError::Runtime(format!("{}", e)))?;
    Ok(())
}
