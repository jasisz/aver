use clap::{Parser as ClapParser, Subcommand};

#[derive(ClapParser)]
#[command(name = "aver", about = "The Aver language interpreter")]
pub(super) struct Cli {
    #[command(subcommand)]
    pub(super) command: Commands,
}

#[derive(Subcommand)]
pub(super) enum Commands {
    /// Run an Aver file
    Run {
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Also run verify blocks after execution
        #[arg(long)]
        verify: bool,
        /// Record effect calls and persist a replay session JSON into this directory
        #[arg(long)]
        record: Option<String>,
    },
    /// Static analysis (intent presence, module size)
    Check {
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Treat all warnings as errors (exit 1 if any warning)
        #[arg(long)]
        strict: bool,
    },
    /// Run all verify blocks
    Verify {
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
    },
    /// Replay an execution from recorded effects JSON
    Replay {
        recording: String,
        /// Show expected vs got output and first JSON diff path
        #[arg(long)]
        diff: bool,
        /// Exit with non-zero when replay output differs from recording
        #[arg(long)]
        test: bool,
        /// Validate effect arguments in addition to effect sequence/type
        #[arg(long = "check-args")]
        check_args: bool,
    },
    /// Interactive REPL
    Repl,
    /// Export project context for LLM consumption
    Context {
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Write output to file instead of stdout
        #[arg(short = 'o', long)]
        output: Option<String>,
        /// Output as JSON instead of Markdown
        #[arg(long)]
        json: bool,
        /// Output only decision blocks (unified replacement for `aver decisions`)
        #[arg(long)]
        decisions_only: bool,
    },
}
