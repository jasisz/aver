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
#[path = "main/shared.rs"]
mod shared;
#[path = "main/why_cmd.rs"]
mod why_cmd;

use cli::{Cli, Commands, CompilePolicyMode};

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

            if !expressions.is_empty() && (*wasm || *self_host) {
                use colored::Colorize;
                eprintln!(
                    "{}",
                    "--expr / --input-file are not supported with --wasm or --self-host in this release"
                        .red()
                );
                std::process::exit(1);
            }

            if *wasm {
                commands::cmd_run_wasm(file, module_root.as_deref(), program_args.clone());
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
            wasm_opt,
        } => {
            let policy_mode = (*policy).unwrap_or(if *with_replay {
                CompilePolicyMode::Runtime
            } else {
                CompilePolicyMode::Embed
            });
            commands::cmd_compile(commands::CompileOptions {
                file,
                output_dir: output,
                project_name: name.as_deref(),
                module_root_override: module_root.as_deref(),
                target: *target,
                with_replay: *with_replay,
                policy_mode: &policy_mode,
                guest_entry: guest_entry.as_deref(),
                with_self_host_support: *with_self_host_support,
                bridge: *bridge,
                wasm_opt: *wasm_opt,
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
