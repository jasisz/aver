//! `aver run --wasip2` — embedded wasmtime + wasmtime-wasi host for
//! the WASI 0.2 component target.
//!
//! Phase 1.7 of 0.18 "Span". Compiles the source to a
//! `wasi:cli/command` component (same path as
//! `aver compile --target wasip2 -o out`, but in-memory), then
//! instantiates it through `wasmtime::component::Component` with
//! a `wasmtime_wasi::WasiCtx` linker. Effect calls reach the host
//! via canonical-ABI WASI imports — `Console.print` writes to the
//! process's real stdout, `Time.unixMs` reads the wall clock,
//! `Random.*` gets cryptographically-random bytes from the OS.
//!
//! Effect surface is whatever the wasm-gc backend's wasip2 path
//! has wired today (see `EffectName::lowers_on_wasip2`):
//! `Console.{print, error, warn}` (Phase 1.2b1.5), `Time.unixMs`,
//! `Random.{int, float}` (Phase 1.4). Programs touching effects
//! that are not yet wired are rejected at compile time by
//! `wasip2::effect_check`.

use std::process;

use colored::Colorize;

pub(super) fn cmd_run_wasip2(
    file: &str,
    module_root_override: Option<&str>,
    record_dir: Option<&str>,
    program_args: Vec<String>,
) {
    #[cfg(not(feature = "wasip2"))]
    {
        let _ = (file, module_root_override, record_dir, program_args);
        eprintln!(
            "{}",
            "--wasip2 requires --features wasip2 (rebuild with: cargo build --features wasip2)"
                .red()
        );
        process::exit(1);
    }

    #[cfg(feature = "wasip2")]
    {
        if record_dir.is_some() {
            // Recording requires a separate plumbing pass against the
            // canonical-ABI WASI imports, which is a future-phase task.
            // Reject the flag rather than silently dropping it — the
            // user explicitly asked for an artifact and would otherwise
            // get a successful run with no recording on disk.
            eprintln!(
                "{}",
                "--record is not yet supported on --wasip2: the recorder needs a \
                 separate plumbing pass against the canonical-ABI WASI imports. \
                 Use `aver run --wasm-gc --record` until that lands."
                    .red()
            );
            process::exit(1);
        }
        let component_bytes = match build_component_bytes(file, module_root_override) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("{}", e.red());
                process::exit(1);
            }
        };
        match run_component(&component_bytes, &program_args) {
            Ok(()) => {}
            Err(e) => {
                eprintln!(
                    "{}",
                    format!("--wasip2: component execution failed — {e}").red()
                );
                process::exit(1);
            }
        }
    }
}

#[cfg(feature = "wasip2")]
fn build_component_bytes(
    file: &str,
    module_root_override: Option<&str>,
) -> Result<Vec<u8>, String> {
    use aver::codegen::{wasip2 as wasip2_codegen, wasm_gc};
    use aver::ir::{NeutralAllocPolicy, PipelineConfig, TypecheckMode};

    let module_root = super::shared::resolve_module_root_for_entry(file, module_root_override);
    let source = super::shared::read_file(file).map_err(|e| e.to_string())?;
    let mut items = super::shared::parse_file(&source).map_err(|e| e.to_string())?;

    let neutral_policy = NeutralAllocPolicy;
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            alloc_policy: Some(&neutral_policy),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    if let Some(tc) = &result.typecheck
        && !tc.errors.is_empty()
    {
        return Err(super::shared::format_type_errors(&tc.errors));
    }

    let dep_modules = super::commands::load_compile_deps(&items, &module_root, false, false);
    aver::codegen::wasm_gc::flatten_multimodule(&mut items, &dep_modules);
    aver::ir::pipeline::resolve(&mut items);

    if let Err(unsupported) = wasip2_codegen::check_supported_effects(&items) {
        let mut out = format!(
            "error[target-effect-unsupported]: {} effect site(s) cannot be lowered by \
             `--wasip2`\n",
            unsupported.len()
        );
        out.push_str(&wasip2_codegen::render_errors(&unsupported));
        out.push_str(
            "\n  See docs/wasip2.md (\"Why X is rejected, not stubbed\") for the \
             static-target vs dynamic-host axis.",
        );
        return Err(out);
    }

    let core_bytes = wasm_gc::compile_to_wasm_gc_for_wasip2(&items, result.analysis.as_ref())
        .map_err(|e| format!("wasm-gc lowering: {e}"))?;
    let (component_bytes, _wit) =
        wasip2_codegen::compile_to_component(&core_bytes, wasip2_codegen::Wasip2World::CliCommand)
            .map_err(|e| format!("component wrap: {e}"))?;
    Ok(component_bytes)
}

#[cfg(feature = "wasip2")]
fn run_component(component_bytes: &[u8], program_args: &[String]) -> wasmtime::Result<()> {
    use wasmtime::component::{Component, Linker, ResourceTable};
    use wasmtime::{Config, Engine, Store};
    use wasmtime_wasi::p2::bindings::sync::Command;
    use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

    // Engine config: GC + function-references + Component Model are
    // all needed for the wasm-gc-derived component to validate and
    // execute. Sync mode keeps Phase 1.7 simple — wasmtime-wasi has
    // both sync and async paths; sync is the right shape for a CLI
    // command that doesn't need to interleave I/O.
    let mut config = Config::new();
    config.wasm_component_model(true);
    config.wasm_gc(true);
    config.wasm_function_references(true);
    let engine = Engine::new(&config)?;

    let component = Component::from_binary(&engine, component_bytes)?;

    // WasiCtx wires stdout / stderr / stdin / env / clocks / random
    // to the host's actual implementations. `inherit_stdio` and
    // `inherit_stderr` are wasmtime-wasi defaults; `inherit_args`
    // forwards `program_args` through `wasi:cli/environment.
    // get-arguments` once Phase 1.3 wires that import.
    let mut ctx_builder = WasiCtxBuilder::new();
    ctx_builder.inherit_stdio();
    // First arg is the program name by convention (matches POSIX
    // argv[0]). `aver run --wasip2 file.av -- a b c` populates
    // wasi-cli args as `[file.av, a, b, c]` so guests reading
    // `Args.get` see the same shape they'd see on the VM target.
    ctx_builder.arg(std::path::Path::new("aver-wasip2").display().to_string());
    for a in program_args {
        ctx_builder.arg(a);
    }
    ctx_builder.inherit_env();
    // Preopen the current working directory as `.` so
    // `wasi:filesystem/preopens.get-directories` returns at least
    // one entry. Aver `Disk.*` calls then resolve guest paths
    // (e.g. `Disk.readText("foo.txt")`) against the host CWD,
    // matching what `aver run` (VM target) does. Full perms keep
    // parity with the VM; later we may want a `--allow-disk` flag
    // for the published component-runner story.
    //
    // Failure here means the host can't open `.` at all — Disk.*
    // would then hit a runtime trap on first use ("no preopens").
    // Surface the host error up front instead of letting it look
    // like a guest bug.
    ctx_builder
        .preopened_dir(
            ".",
            ".",
            wasmtime_wasi::DirPerms::all(),
            wasmtime_wasi::FilePerms::all(),
        )
        .map_err(|e| wasmtime::Error::msg(format!("preopen `.`: {e}")))?;
    let ctx = ctx_builder.build();

    struct Host {
        ctx: WasiCtx,
        table: ResourceTable,
    }
    impl WasiView for Host {
        fn ctx(&mut self) -> WasiCtxView<'_> {
            WasiCtxView {
                ctx: &mut self.ctx,
                table: &mut self.table,
            }
        }
    }

    let mut store = Store::new(
        &engine,
        Host {
            ctx,
            table: ResourceTable::new(),
        },
    );
    let mut linker = Linker::<Host>::new(&engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    let command = Command::instantiate(&mut store, &component, &linker)?;
    // wasi:cli/run.run() -> result<_, _>; Ok(()) on success, Err(())
    // when the guest signalled failure via `i32.const 1`. Aver's
    // entry doesn't yet wire a return-code path, so Err here means
    // the wasm-gc emitter produced a non-zero — surface as exit 1.
    match command.wasi_cli_run().call_run(&mut store)? {
        Ok(()) => Ok(()),
        Err(()) => {
            eprintln!("{}", "--wasip2: program returned non-zero exit code".red());
            std::process::exit(1);
        }
    }
}
