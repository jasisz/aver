use clap::Parser as ClapParser;

#[path = "main/capabilities_cmd.rs"]
mod capabilities_cmd;
#[path = "main/cert_delegate.rs"]
mod cert_delegate;
#[path = "main/cli.rs"]
mod cli;
#[path = "main/commands.rs"]
mod commands;
#[path = "main/context_cmd.rs"]
mod context_cmd;
#[path = "main/context_data.rs"]
mod context_data;
#[path = "main/context_format.rs"]
mod context_format;
#[path = "main/diagnostic.rs"]
#[allow(dead_code)]
mod diagnostic;
use crate::format as format_cmd;
#[path = "main/provider_host_cmd.rs"]
mod provider_host_cmd;
#[path = "main/repl.rs"]
mod repl;
#[path = "main/replay_cmd.rs"]
mod replay_cmd;
#[cfg(feature = "wasip2")]
#[path = "main/run_wasip2.rs"]
mod run_wasip2;
#[path = "main/run_wasm_gc.rs"]
mod run_wasm_gc;
#[path = "main/shape_cmd.rs"]
mod shape_cmd;
#[path = "main/shared.rs"]
mod shared;
#[cfg(feature = "wasip2")]
#[path = "main/wasip2_provider_host.rs"]
mod wasip2_provider_host;
#[path = "main/why_cmd.rs"]
mod why_cmd;

use cli::{Cli, Commands, CompilePolicyMode};
#[allow(unused_imports)]
use cli::{CompileTarget, DeployPack, DeployPreset};

pub fn main() {
    main_impl(None, None);
}

/// Run the exact stock CLI with process-local provider bindings already
/// constructed by the cached Rust provider host.
pub fn main_with_provider_bindings(bindings: Vec<aver::provider::ProviderBinding>) {
    main_impl(Some(bindings), None);
}

/// Run the stock CLI against explicit argv and process-local bindings.
/// Useful to embedders that cannot mutate process-global arguments.
pub fn main_with_args_and_provider_bindings(
    raw_args: Vec<std::ffi::OsString>,
    bindings: Vec<aver::provider::ProviderBinding>,
) {
    main_impl(Some(bindings), Some(raw_args));
}

fn main_impl(
    injected_provider_bindings: Option<Vec<aver::provider::ProviderBinding>>,
    raw_args_override: Option<Vec<std::ffi::OsString>>,
) {
    if raw_args_override.is_none()
        && let Some(result) = cert_delegate::run_if_requested()
    {
        match result {
            Ok(status) => std::process::exit(cert_delegate::exit_code(status)),
            Err(error) => {
                eprintln!("error: {error}");
                std::process::exit(1);
            }
        }
    }

    let raw_args = raw_args_override.unwrap_or_else(|| std::env::args_os().collect());
    let cli = Cli::parse_from(raw_args.clone());

    if injected_provider_bindings.is_none()
        && let Some(result) = provider_host_cmd::run_if_requested(&cli.command, &raw_args)
    {
        match result {
            Ok(status) => std::process::exit(status.code().unwrap_or(1)),
            Err(error) => {
                use colored::Colorize;
                eprintln!("{}", error.red());
                std::process::exit(1);
            }
        }
    }
    let provider_bindings = injected_provider_bindings.as_deref().unwrap_or(&[]);

    match &cli.command {
        Commands::Run {
            file,
            module_root,
            record,
            expr,
            input_file,
            self_host,
            profile,
            wasm_gc,
            wasip2,
            program_args,
        } => {
            let expressions = match shared::collect_entry_expressions(expr, input_file.as_deref()) {
                Ok(list) => list,
                Err(e) => {
                    use colored::Colorize;
                    eprintln!("{}", e.red());
                    std::process::exit(1);
                }
            };

            if !expressions.is_empty() && *self_host {
                use colored::Colorize;
                eprintln!(
                    "{}",
                    "--expr / --input-file are not supported with --self-host in this release"
                        .red()
                );
                std::process::exit(1);
            }

            if *wasip2 {
                if !expressions.is_empty() {
                    use colored::Colorize;
                    eprintln!(
                        "{}",
                        "--expr / --input-file are not supported with --wasip2 yet (Phase 1.7 \
                         minimal scope — extends in a follow-up)"
                            .red()
                    );
                    std::process::exit(1);
                }
                #[cfg(feature = "wasip2")]
                run_wasip2::cmd_run_wasip2(
                    file,
                    module_root.as_deref(),
                    record.as_deref(),
                    program_args.clone(),
                    provider_bindings,
                );
                #[cfg(not(feature = "wasip2"))]
                {
                    use colored::Colorize;
                    eprintln!(
                        "{}",
                        "--wasip2 requires --features wasip2 (rebuild with: \
                         cargo build --features wasip2)"
                            .red()
                    );
                    std::process::exit(1);
                }
            } else if *wasm_gc {
                if expressions.is_empty() {
                    run_wasm_gc::cmd_run_wasm_gc(
                        file,
                        module_root.as_deref(),
                        program_args.clone(),
                        record.as_deref(),
                        None,
                    );
                } else {
                    // Batch: each --expr gets its own fresh wasmtime
                    // instance so recording state / module globals
                    // start clean per expression.
                    for expr_src in &expressions {
                        run_wasm_gc::cmd_run_wasm_gc(
                            file,
                            module_root.as_deref(),
                            program_args.clone(),
                            record.as_deref(),
                            Some(expr_src.as_str()),
                        );
                    }
                }
            } else if *self_host {
                commands::cmd_run_self_hosted(
                    file,
                    module_root.as_deref(),
                    record.as_deref(),
                    program_args.clone(),
                );
            } else if expressions.is_empty() {
                commands::cmd_run_vm(
                    file,
                    module_root.as_deref(),
                    record.as_deref(),
                    program_args.clone(),
                    *profile,
                    None,
                    provider_bindings,
                );
            } else {
                // Batch: run each expression as its own invocation so that
                // recording state and VM globals start fresh each time.
                for expr_src in &expressions {
                    commands::cmd_run_vm(
                        file,
                        module_root.as_deref(),
                        record.as_deref(),
                        program_args.clone(),
                        *profile,
                        Some(expr_src.as_str()),
                        provider_bindings,
                    );
                }
            }
        }
        Commands::Check {
            file,
            module_root,
            verbose,
            json,
        } => {
            commands::cmd_check(file, module_root.as_deref(), *verbose, *json);
        }
        Commands::Capabilities {
            file,
            module_root,
            json,
        } => {
            capabilities_cmd::cmd_capabilities(file, module_root.as_deref(), *json);
        }
        Commands::Verify {
            file,
            module_root,
            verbose,
            json,
            hostile,
            wasm_gc,
        } => {
            commands::cmd_verify(
                file,
                module_root.as_deref(),
                *verbose,
                *json,
                *hostile,
                *wasm_gc,
                provider_bindings,
            );
        }
        Commands::Audit {
            path,
            module_root,
            json,
            hostile,
        } => {
            commands::cmd_audit(
                path,
                module_root.as_deref(),
                *json,
                *hostile,
                provider_bindings,
            );
        }
        Commands::Format { path, check, json } => {
            format_cmd::cmd_format(path, *check, *json);
        }
        Commands::Replay {
            recording,
            diff,
            test,
            check_args,
            self_host,
            wasm_gc,
            json,
        } => {
            replay_cmd::cmd_replay(
                recording,
                *diff,
                *test,
                *check_args,
                *self_host,
                *wasm_gc,
                *json,
            );
        }
        Commands::Repl => {
            repl::cmd_repl();
        }
        Commands::Context {
            file,
            module_root,
            output,
            json,
            decisions_only,
            focus,
            depth,
            budget,
        } => {
            context_cmd::cmd_context(
                file,
                module_root.as_deref(),
                output.as_deref(),
                *json,
                *decisions_only,
                focus.as_deref(),
                *depth,
                *budget,
            );
        }
        Commands::Shape {
            file,
            module_root,
            summary,
            json,
            lint,
        } => {
            shape_cmd::cmd_shape(file, module_root.as_deref(), *summary, *json, *lint);
        }
        Commands::Compile {
            file,
            output,
            name,
            module_root,
            target,
            with_replay,
            policy,
            guest_entry,
            with_self_host_support,
            pack,
            preset,
            handler,
            world,
            optimize,
            certify,
            emit_ir_after,
            explain_passes,
            explain_mir_coverage,
            json,
        } => {
            let policy_mode = (*policy).unwrap_or(if *with_replay {
                CompilePolicyMode::Runtime
            } else {
                CompilePolicyMode::Embed
            });
            // Expand --preset into the (target, pack) pair it stands for.
            // Clap's `conflicts_with_all` already rejects mixing --preset
            // with explicit axes.
            let (effective_target, effective_pack) = match preset {
                // wasm-gc + the synthesised `aver_http_handle` wrapper is
                // the production shape on Cloudflare Workers: workerd
                // ships a stable wasm-gc + tail-call V8, the runtime is
                // inlined as per-instantiation `__rt_*` helpers (no
                // `WebAssembly.instantiate(bytes, …)` from runtime-fetched
                // bytes — that's the path Workers reject). User must
                // pass `--handler <fn>` (validated below).
                Some(cli::DeployPreset::Cloudflare) => (
                    cli::CompileTarget::WasmGc,
                    Some(cli::DeployPack::Cloudflare),
                ),
                None => (*target, *pack),
            };
            // `--certify` is a wasm-gc-only artifact certificate. Clap
            // already blocks `--certify --optimize`; reject any non-wasm-gc
            // target here so the flag never silently no-ops.
            if *certify && !matches!(effective_target, cli::CompileTarget::WasmGc) {
                use colored::Colorize;
                eprintln!(
                    "{}",
                    "--certify requires --target wasm-gc (the artifact certificate is emitted \
                     for the wasm-gc module)"
                        .red()
                );
                std::process::exit(1);
            }
            if matches!(preset, Some(cli::DeployPreset::Cloudflare)) && handler.is_none() {
                use colored::Colorize;
                eprintln!(
                    "{}",
                    "--preset cloudflare requires --handler <fn> (the Aver fn with signature \
                     `Fn(HttpRequest) -> HttpResponse` to expose as the request handler)"
                        .red()
                );
                std::process::exit(1);
            }
            // `--emit-ir-after=PASS` short-circuits before codegen — print
            // the IR snapshot for the named stage and exit. Drives observability
            // in 0.15.1 without touching the compile path otherwise.
            if let Some(stage) = emit_ir_after.as_deref() {
                commands::cmd_emit_ir_after(file, module_root.as_deref(), stage);
                return;
            }
            // `--explain-passes` runs the full pipeline (no codegen) and
            // prints a per-pass diagnostic report. Same short-circuit
            // shape as `--emit-ir-after`. `--json` switches to a stable
            // machine-readable shape for CI consumption.
            if *explain_passes {
                commands::cmd_explain_passes(file, module_root.as_deref(), *json);
                return;
            }
            // `--explain-mir-coverage` lowers the program to MIR and reports
            // how much of it the MIR walker accepts vs. drops to the HIR
            // fallback (by blocking shape). Same short-circuit shape.
            // `--target wasm-gc` switches to the wasm-gc backend's reach
            // over the lowered MIR (Phase 5 #340); other targets report the
            // VM lowering-level coverage.
            if *explain_mir_coverage {
                commands::cmd_explain_mir_coverage(
                    file,
                    module_root.as_deref(),
                    *json,
                    effective_target,
                );
                return;
            }
            commands::cmd_compile(commands::CompileOptions {
                file,
                output_dir: output,
                project_name: name.as_deref(),
                module_root_override: module_root.as_deref(),
                target: effective_target,
                with_replay: *with_replay,
                policy_mode: &policy_mode,
                guest_entry: guest_entry.as_deref(),
                with_self_host_support: *with_self_host_support,
                pack: effective_pack,
                handler: handler.as_deref(),
                world: *world,
                optimize: *optimize,
                certify: *certify,
            });
        }
        Commands::Why {
            file,
            module_root,
            verbose,
            json,
        } => {
            why_cmd::cmd_why(file, module_root.as_deref(), *verbose, *json);
        }
        Commands::Bench {
            scenario,
            target,
            iterations,
            warmup,
            json,
            save_baseline,
            compare,
            baseline_dir,
            fail_on_regression,
        } => {
            commands::cmd_bench(commands::BenchOptions {
                scenario_path: scenario,
                target,
                iterations: *iterations,
                warmup: *warmup,
                json: *json,
                save_baseline: save_baseline.as_deref(),
                compare: compare.as_deref(),
                baseline_dir: baseline_dir.as_deref(),
                fail_on_regression: *fail_on_regression,
            });
        }
        Commands::Proof {
            file,
            output,
            name,
            module_root,
            backend,
            verify_mode,
            check,
            error_budget,
            sorry_budget,
            declined_budget,
            check_json,
            explain,
            minimize,
            allow_mathlib,
            gate,
            write_baseline,
        } => {
            commands::cmd_proof(
                file,
                output,
                name.as_deref(),
                module_root.as_deref(),
                backend,
                verify_mode,
                *check,
                *error_budget,
                *sorry_budget,
                *declined_budget,
                *check_json,
                *explain,
                *minimize,
                *allow_mathlib,
                gate.as_deref(),
                write_baseline.as_deref(),
            );
        }
        Commands::Cert { .. } => unreachable!("`aver cert` is delegated before CLI parsing"),
    }
}
