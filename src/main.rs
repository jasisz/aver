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
#[path = "main/format_cmd.rs"]
mod format_cmd;
#[path = "main/repl.rs"]
mod repl;
#[path = "main/replay_cmd.rs"]
mod replay_cmd;
#[path = "main/shared.rs"]
mod shared;

use cli::{Cli, Commands};

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Run {
            file,
            module_root,
            verify,
            record,
            vm,
            program_args,
        } => {
            if *vm {
                commands::cmd_run_vm(
                    file,
                    module_root.as_deref(),
                    *verify,
                    record.as_deref(),
                    program_args.clone(),
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
        } => {
            commands::cmd_check(file, module_root.as_deref(), *deps);
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
        } => {
            replay_cmd::cmd_replay(recording, *diff, *test, *check_args, *vm);
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
        } => {
            commands::cmd_compile(file, output, name.as_deref(), module_root.as_deref());
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
