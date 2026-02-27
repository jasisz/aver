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
#[path = "main/decisions_cmd.rs"]
mod decisions_cmd;
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
        } => {
            commands::cmd_run(file, module_root.as_deref(), *verify, record.as_deref());
        }
        Commands::Check {
            file,
            module_root,
            strict,
        } => {
            commands::cmd_check(file, module_root.as_deref(), *strict);
        }
        Commands::Verify { file, module_root } => {
            commands::cmd_verify(file, module_root.as_deref());
        }
        Commands::Replay {
            recording,
            diff,
            test,
            check_args,
        } => {
            replay_cmd::cmd_replay(recording, *diff, *test, *check_args);
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
        } => {
            context_cmd::cmd_context(
                file,
                module_root.as_deref(),
                output.as_deref(),
                *json,
                *decisions_only,
            );
        }
        Commands::Decisions {
            source,
            output,
            json,
            docs,
        } => {
            decisions_cmd::cmd_decisions(source.as_deref(), output.as_deref(), *json, *docs);
        }
    }
}
