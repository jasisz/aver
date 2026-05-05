use clap::Parser as ClapParser;

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
#[path = "main/format_cmd.rs"]
mod format_cmd;
#[path = "main/repl.rs"]
mod repl;
#[path = "main/replay_cmd.rs"]
mod replay_cmd;
#[path = "main/run_wasm_gc.rs"]
mod run_wasm_gc;
#[path = "main/shared.rs"]
mod shared;
#[path = "main/why_cmd.rs"]
mod why_cmd;

use cli::{Cli, Commands, CompilePolicyMode};
#[allow(unused_imports)]
use cli::{CompileTarget, DeployPack, DeployPreset, WasmBridge};

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run {
            file,
            module_root,
            verify,
            record,
            expr,
            input_file,
            self_host,
            profile,
            wasm,
            wasm_gc,
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

            if !expressions.is_empty() && (*wasm || *wasm_gc || *self_host) {
                use colored::Colorize;
                eprintln!(
                    "{}",
                    "--expr / --input-file are not supported with --wasm / --wasm-gc / --self-host in this release"
                        .red()
                );
                std::process::exit(1);
            }

            if *wasm {
                commands::cmd_run_wasm(file, module_root.as_deref(), program_args.clone());
            } else if *wasm_gc {
                run_wasm_gc::cmd_run_wasm_gc(
                    file,
                    module_root.as_deref(),
                    program_args.clone(),
                    record.as_deref(),
                );
            } else if *self_host {
                commands::cmd_run_self_hosted(
                    file,
                    module_root.as_deref(),
                    *verify,
                    record.as_deref(),
                    program_args.clone(),
                );
            } else if expressions.is_empty() {
                commands::cmd_run_vm(
                    file,
                    module_root.as_deref(),
                    *verify,
                    record.as_deref(),
                    program_args.clone(),
                    *profile,
                    None,
                );
            } else {
                // Batch: run each expression as its own invocation so that
                // recording state and VM globals start fresh each time.
                for expr_src in &expressions {
                    commands::cmd_run_vm(
                        file,
                        module_root.as_deref(),
                        *verify,
                        record.as_deref(),
                        program_args.clone(),
                        *profile,
                        Some(expr_src.as_str()),
                    );
                }
            }
        }
        Commands::Check {
            file,
            module_root,
            deps,
            verbose,
            json,
        } => {
            commands::cmd_check(file, module_root.as_deref(), *deps, *verbose, *json);
        }
        Commands::Verify {
            file,
            module_root,
            deps,
            verbose,
            json,
            hostile,
        } => {
            commands::cmd_verify(
                file,
                module_root.as_deref(),
                *deps,
                *verbose,
                *json,
                *hostile,
            );
        }
        Commands::Audit {
            path,
            module_root,
            json,
            hostile,
        } => {
            commands::cmd_audit(path, module_root.as_deref(), *json, *hostile);
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
            json,
        } => {
            replay_cmd::cmd_replay(recording, *diff, *test, *check_args, *self_host, *json);
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
            bridge,
            pack,
            preset,
            handler,
            optimize,
            emit_ir_after,
            explain_passes,
            json,
        } => {
            let policy_mode = (*policy).unwrap_or(if *with_replay {
                CompilePolicyMode::Runtime
            } else {
                CompilePolicyMode::Embed
            });
            // Expand --preset into the (target, bridge, pack) triple it
            // stands for. Clap's `conflicts_with_all` already rejects
            // mixing --preset with explicit axes.
            let (effective_target, effective_bridge, effective_pack) = match preset {
                // wasm-gc + the synthesised `aver_http_handle` wrapper is
                // the production shape on Cloudflare Workers: workerd
                // ships a stable wasm-gc + tail-call V8, the runtime is
                // inlined as per-instantiation `__rt_*` helpers (no
                // `WebAssembly.instantiate(bytes, …)` from runtime-fetched
                // bytes — that's the path Workers reject), and the
                // emitted binary is ~20% smaller than the legacy
                // `--target wasm` + wasm-merge bundle. The bridge axis
                // does not apply: `--handler <fn>` synthesises a thin
                // wrapper that reads request fields via `Request.*` host
                // imports and writes the response via `Response.*`, no
                // separate fetch shim. The accompanying worker.js
                // template is the LM string transport plus a 138-line
                // request adapter — no JSPI, no NaN-box dance. User must
                // pass `--handler <fn>` (validated below).
                Some(cli::DeployPreset::Cloudflare) => (
                    cli::CompileTarget::WasmGc,
                    None,
                    Some(cli::DeployPack::Cloudflare),
                ),
                None => (*target, *bridge, *pack),
            };
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
            // `--bridge` is a legacy concept tied to `--target wasm` —
            // wasm-gc has its own shape for both axes the legacy bridges
            // covered: HTTP via `--handler <fn>` (synth `aver_http_handle`
            // wrapper, no separate fetch shim) and standalone runtime via
            // the planned `--target wasip2` Component Model output. Reject
            // up front rather than silently ignoring the flag.
            if matches!(effective_target, cli::CompileTarget::WasmGc) && effective_bridge.is_some()
            {
                use colored::Colorize;
                let hint = match effective_bridge {
                    Some(cli::WasmBridge::Fetch) => {
                        "use `--handler <fn>` (HTTP synth wrapper) or `--preset cloudflare \
                         --handler <fn>` (full Workers pack) instead"
                    }
                    Some(cli::WasmBridge::Wasip1) => {
                        "wasm-gc skips preview 1 by design — `--target wasm --bridge wasip1` \
                         is the legacy standalone-WASI path; `--target wasip2` (Component \
                         Model, `wasi:http/proxy` + `wasi:filesystem` + `wasi:sockets`) is \
                         planned as the modern wasm-gc companion"
                    }
                    Some(cli::WasmBridge::None) | None => "drop `--bridge` (wasm-gc default)",
                };
                eprintln!(
                    "{}",
                    format!("--bridge is not supported with --target wasm-gc; {hint}").red()
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
                bridge: effective_bridge,
                pack: effective_pack,
                handler: handler.as_deref(),
                optimize: *optimize,
            });
        }
        Commands::WasmRuntime {
            output,
            artifact,
            optimize,
            wat,
        } => {
            commands::cmd_wasm_runtime(output, *artifact, *optimize, *wat);
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
        } => {
            commands::cmd_proof(
                file,
                output,
                name.as_deref(),
                module_root.as_deref(),
                backend,
                verify_mode,
            );
        }
    }
}
