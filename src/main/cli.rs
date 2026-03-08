use clap::{Parser as ClapParser, Subcommand, ValueEnum};

#[derive(ClapParser)]
#[command(name = "aver", version, about = "The Aver language toolchain")]
pub(super) struct Cli {
    #[command(subcommand)]
    pub(super) command: Commands,
}

/// Proof verify emission mode.
#[derive(Clone, Debug, ValueEnum)]
pub(super) enum ProofVerifyMode {
    /// Auto mode: regular cases use `native_decide`; supported law universals get auto-proofs.
    #[value(name = "auto")]
    Auto,
    /// Emit `example ... := by sorry`
    #[value(name = "sorry")]
    Sorry,
    /// Emit named theorem stubs `theorem ... := by sorry`
    #[value(name = "theorem-skeleton")]
    TheoremSkeleton,
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
        /// Also run contract checks for transitive `depends [...]` modules
        #[arg(long)]
        deps: bool,
    },
    /// Run all verify blocks
    Verify {
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Also run verify blocks for transitive `depends [...]` modules
        #[arg(long)]
        deps: bool,
    },
    /// Format Aver source files
    Format {
        /// File or directory to format (default: current directory)
        #[arg(default_value = ".")]
        path: String,
        /// Check formatting only (non-zero exit if changes would be made)
        #[arg(long)]
        check: bool,
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
        /// Output only decision blocks
        #[arg(long)]
        decisions_only: bool,
    },
    /// Compile an Aver file to a Rust/Cargo project
    Compile {
        file: String,
        /// Output directory for the generated project
        #[arg(short = 'o', long, default_value = "out")]
        output: String,
        /// Project name (default: derived from file name)
        #[arg(long)]
        name: Option<String>,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
    },
    /// Export pure Aver code to a Lean 4 proof project
    Proof {
        file: String,
        /// Output directory for the generated project
        #[arg(short = 'o', long, default_value = "out")]
        output: String,
        /// Project name (default: derived from file name)
        #[arg(long)]
        name: Option<String>,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// How to emit `verify` cases and law theorems in generated Lean
        #[arg(long, default_value = "auto")]
        verify_mode: ProofVerifyMode,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_accepts_deps_flag() {
        let cli = Cli::parse_from(["aver", "verify", "examples/app.av", "--deps"]);
        match cli.command {
            Commands::Verify { file, deps, .. } => {
                assert_eq!(file, "examples/app.av");
                assert!(deps);
            }
            _ => panic!("expected verify command"),
        }
    }
}
