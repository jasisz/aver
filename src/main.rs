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

use cli::{Cli, Commands, CompilePolicyMode};

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run {
            file,
            module_root,
            verify,
            record,
            vm,
            self_host,
            profile,
            program_args,
        } => {
            if *self_host {
                commands::cmd_run_self_hosted(
                    file,
                    module_root.as_deref(),
                    *verify,
                    record.as_deref(),
                    program_args.clone(),
                );
            } else if *vm || *profile {
                commands::cmd_run_vm(
                    file,
                    module_root.as_deref(),
                    *verify,
                    record.as_deref(),
                    program_args.clone(),
                    *profile,
                );
            } else {
                commands::cmd_run(
                    file,
                    module_root.as_deref(),
                    *verify,
                    record.as_deref(),
                    program_args.clone(),
                );
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
            vm,
        } => {
            commands::cmd_verify(file, module_root.as_deref(), *deps, *vm);
        }
        Commands::Format { path, check } => {
            format_cmd::cmd_format(path, *check);
        }
        Commands::Replay {
            recording,
            diff,
            test,
            check_args,
            vm,
            self_host,
        } => {
            replay_cmd::cmd_replay(recording, *diff, *test, *check_args, *vm, *self_host);
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
            with_replay,
            policy,
            guest_entry,
            with_self_host_support,
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
                with_replay: *with_replay,
                policy_mode: &policy_mode,
                guest_entry: guest_entry.as_deref(),
                with_self_host_support: *with_self_host_support,
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
