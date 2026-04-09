use std::str::FromStr;

use clap::{Parser as ClapParser, Subcommand, ValueEnum};

#[derive(ClapParser)]
#[command(name = "aver", version, about = "The Aver language toolchain")]
pub(super) struct Cli {
    #[command(subcommand)]
    pub(super) command: Commands,
}

/// Proof backend target.
#[derive(Clone, Debug, Default, ValueEnum)]
pub(super) enum ProofBackend {
    /// Generate Lean 4 proof project (default).
    #[default]
    #[value(name = "lean")]
    Lean,
    /// Generate Dafny verification file (Z3-powered).
    #[value(name = "dafny")]
    Dafny,
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

/// WASM import ABI adapter mode.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum WasmAdapter {
    /// Default: aver/* capability imports. Requires a host that provides capabilities.
    Aver,
    /// Compatibility: WASI imports. Works with standalone wasmtime.
    Wasi,
}

/// Optional post-pass optimization for generated WASM modules.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum WasmOptMode {
    /// Optimize for runtime speed.
    #[value(name = "o3")]
    O3,
    /// Optimize aggressively for binary size.
    #[value(name = "oz")]
    Oz,
}

/// Compile target language.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, ValueEnum)]
pub(super) enum CompileTarget {
    /// Generate a Rust/Cargo project (default).
    #[default]
    #[value(name = "rust")]
    Rust,
    /// Generate a .wasm binary (requires --features wasm).
    #[value(name = "wasm")]
    Wasm,
}

/// Runtime policy handling for generated Rust projects.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum CompilePolicyMode {
    /// Bake aver.toml policy into the generated binary at compile time.
    #[value(name = "embed")]
    Embed,
    /// Load aver.toml at runtime from the active module root / guest boundary.
    #[value(name = "runtime")]
    Runtime,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ContextDepth {
    Auto,
    Unlimited,
    Limited(usize),
}

impl FromStr for ContextDepth {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let value = input.trim().to_ascii_lowercase();
        match value.as_str() {
            "auto" => Ok(Self::Auto),
            "unlimited" => Ok(Self::Unlimited),
            _ => value
                .parse::<usize>()
                .map(Self::Limited)
                .map_err(|_| "expected auto, unlimited, or a non-negative integer".to_string()),
        }
    }
}

pub(super) fn parse_context_budget(input: &str) -> Result<usize, String> {
    let value = input.trim().to_ascii_lowercase();
    let (number, multiplier) = if let Some(raw) = value.strip_suffix("kb") {
        (raw.trim(), 1024usize)
    } else if let Some(raw) = value.strip_suffix("mb") {
        (raw.trim(), 1024usize * 1024)
    } else if let Some(raw) = value.strip_suffix('b') {
        (raw.trim(), 1usize)
    } else {
        (value.as_str(), 1usize)
    };

    let amount = number
        .parse::<usize>()
        .map_err(|_| "expected a byte size like 8192, 10kb, or 1mb".to_string())?;

    amount
        .checked_mul(multiplier)
        .ok_or_else(|| "budget is too large".to_string())
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
        /// Execute using the bytecode VM instead of the tree-walking interpreter
        #[arg(long)]
        vm: bool,
        /// Execute using the self-hosted Aver interpreter compiled to Rust
        #[arg(long, conflicts_with_all = ["vm", "profile"])]
        self_host: bool,
        /// Print VM opcode/function profile after execution (implies --vm)
        #[arg(long)]
        profile: bool,
        /// Compile to WASM and execute with built-in host (aver/* import ABI)
        #[arg(long, conflicts_with_all = ["vm", "self_host", "profile"])]
        wasm: bool,
        /// Arguments passed to the Aver program (available via Args.get()), after --
        #[arg(last = true)]
        program_args: Vec<String>,
    },
    /// Static analysis (intent presence, module size)
    Check {
        /// Aver file or directory
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Also run contract checks for transitive `depends [...]` modules
        #[arg(long)]
        deps: bool,
        /// Show full diagnostic detail (intent, source snippets for warnings, repair alternatives)
        #[arg(long)]
        verbose: bool,
        /// Output diagnostics as JSON (one object per line)
        #[arg(long)]
        json: bool,
    },
    /// Run all verify blocks
    Verify {
        /// Aver file or directory
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Also run verify blocks for transitive `depends [...]` modules
        #[arg(long)]
        deps: bool,
        /// Execute verify cases on the bytecode VM instead of the tree-walking interpreter
        #[arg(long)]
        vm: bool,
        /// Show full diagnostic detail (source snippets on failures)
        #[arg(long)]
        verbose: bool,
        /// Output diagnostics as JSON (NDJSON, one object per line)
        #[arg(long)]
        json: bool,
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
        /// Replay using the bytecode VM instead of the tree-walking interpreter
        #[arg(long)]
        vm: bool,
        /// Replay using the self-hosted Aver interpreter compiled to Rust
        #[arg(long, conflicts_with = "vm")]
        self_host: bool,
        /// Output results as JSON (NDJSON, one object per line)
        #[arg(long)]
        json: bool,
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
        /// Focus context around a specific function or qualified symbol
        #[arg(long)]
        focus: Option<String>,
        /// Dependency depth, or call depth when --focus is set: auto (default), unlimited, or a non-negative integer
        #[arg(long, default_value = "auto")]
        depth: ContextDepth,
        /// Byte budget for --depth auto / --focus auto, e.g. 10kb or 1mb (default: 10kb)
        #[arg(long, default_value = "10kb", value_parser = parse_context_budget)]
        budget: usize,
    },
    /// Compile an Aver file to a Rust/Cargo project or WASM binary
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
        /// Compile target: rust (default) or wasm
        #[arg(long, default_value = "rust")]
        target: CompileTarget,
        /// Emit optional record/replay runtime support into the generated project
        #[arg(long)]
        with_replay: bool,
        /// Runtime policy mode: embed aver.toml at compile time or load it at runtime
        #[arg(long = "policy", value_enum)]
        policy: Option<CompilePolicyMode>,
        /// Explicit guest execution boundary for scoped replay/policy (self-host style)
        #[arg(long)]
        guest_entry: Option<String>,
        /// Emit extra self-host-only runtime glue (FnStore callbacks, HttpServer bridge)
        #[arg(long)]
        with_self_host_support: bool,
        /// WASM import ABI adapter (default: aver/* capability imports)
        #[arg(long, value_enum)]
        adapter: Option<WasmAdapter>,
        /// Post-process generated WASM with wasm-opt (`o3` for speed, `oz` for size)
        #[arg(long, value_enum)]
        wasm_opt: Option<WasmOptMode>,
    },
    /// Trace justifications: decisions, verify blocks, descriptions
    Why {
        /// Aver file or directory
        file: String,
        /// Resolve `depends [...]` from this root (default: current working directory)
        #[arg(long)]
        module_root: Option<String>,
        /// Show all partial/unjustified functions (default: top 3 per file)
        #[arg(long)]
        verbose: bool,
        /// Output as NDJSON (one object per line)
        #[arg(long)]
        json: bool,
    },
    /// Export pure Aver code to a proof/verification project
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
        /// Proof backend: lean (default) or dafny
        #[arg(long, default_value = "lean")]
        backend: ProofBackend,
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
        let cli = Cli::parse_from(["aver", "verify", "examples/modules/app.av", "--deps"]);
        match cli.command {
            Commands::Verify { file, deps, vm, .. } => {
                assert_eq!(file, "examples/modules/app.av");
                assert!(deps);
                assert!(!vm);
            }
            _ => panic!("expected verify command"),
        }
    }

    #[test]
    fn verify_accepts_vm_flag() {
        let cli = Cli::parse_from(["aver", "verify", "examples/modules/app.av", "--vm"]);
        match cli.command {
            Commands::Verify { vm, .. } => assert!(vm),
            _ => panic!("expected verify command"),
        }
    }

    #[test]
    fn run_accepts_self_host_flag() {
        let cli = Cli::parse_from(["aver", "run", "examples/modules/app.av", "--self-host"]);
        match cli.command {
            Commands::Run { self_host, vm, .. } => {
                assert!(self_host);
                assert!(!vm);
            }
            _ => panic!("expected run command"),
        }
    }

    #[test]
    fn replay_accepts_vm_flag() {
        let cli = Cli::parse_from(["aver", "replay", "recordings", "--vm"]);
        match cli.command {
            Commands::Replay { vm, self_host, .. } => {
                assert!(vm);
                assert!(!self_host);
            }
            _ => panic!("expected replay command"),
        }
    }

    #[test]
    fn replay_accepts_self_host_flag() {
        let cli = Cli::parse_from(["aver", "replay", "recordings", "--self-host"]);
        match cli.command {
            Commands::Replay { self_host, vm, .. } => {
                assert!(self_host);
                assert!(!vm);
            }
            _ => panic!("expected replay command"),
        }
    }

    #[test]
    fn compile_accepts_with_replay_and_guest_entry() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/modules/app.av",
            "--with-replay",
            "--guest-entry",
            "runGuestProgram",
        ]);
        match cli.command {
            Commands::Compile {
                with_replay,
                policy,
                guest_entry,
                with_self_host_support,
                ..
            } => {
                assert!(with_replay);
                assert_eq!(policy, None);
                assert_eq!(guest_entry.as_deref(), Some("runGuestProgram"));
                assert!(!with_self_host_support);
            }
            _ => panic!("expected compile command"),
        }
    }

    #[test]
    fn compile_accepts_wasm_opt() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/core/hello.av",
            "--target",
            "wasm",
            "--wasm-opt",
            "oz",
        ]);
        match cli.command {
            Commands::Compile {
                target, wasm_opt, ..
            } => {
                assert_eq!(target, CompileTarget::Wasm);
                assert_eq!(wasm_opt, Some(WasmOptMode::Oz));
            }
            _ => panic!("expected compile command"),
        }
    }

    #[test]
    fn compile_accepts_explicit_self_host_support() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "self_hosted/main.av",
            "--with-self-host-support",
            "--guest-entry",
            "runGuestCliProgram",
            "--policy",
            "runtime",
        ]);
        match cli.command {
            Commands::Compile {
                policy,
                guest_entry,
                with_self_host_support,
                ..
            } => {
                assert_eq!(policy, Some(CompilePolicyMode::Runtime));
                assert_eq!(guest_entry.as_deref(), Some("runGuestCliProgram"));
                assert!(with_self_host_support);
            }
            _ => panic!("expected compile command"),
        }
    }

    #[test]
    fn compile_accepts_explicit_runtime_policy() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/modules/app.av",
            "--policy",
            "runtime",
        ]);
        match cli.command {
            Commands::Compile { policy, .. } => {
                assert_eq!(policy, Some(CompilePolicyMode::Runtime));
            }
            _ => panic!("expected compile command"),
        }
    }

    #[test]
    fn context_defaults_to_auto_depth_and_10kb_budget() {
        let cli = Cli::parse_from(["aver", "context", "examples/modules/app.av"]);
        match cli.command {
            Commands::Context { depth, budget, .. } => {
                assert_eq!(depth, ContextDepth::Auto);
                assert_eq!(budget, 10 * 1024);
            }
            _ => panic!("expected context command"),
        }
    }

    #[test]
    fn context_accepts_unlimited_and_labeled_budget() {
        let cli = Cli::parse_from([
            "aver",
            "context",
            "examples/modules/app.av",
            "--depth",
            "unlimited",
            "--budget",
            "12kb",
        ]);
        match cli.command {
            Commands::Context { depth, budget, .. } => {
                assert_eq!(depth, ContextDepth::Unlimited);
                assert_eq!(budget, 12 * 1024);
            }
            _ => panic!("expected context command"),
        }
    }

    #[test]
    fn context_accepts_numeric_depth() {
        let cli = Cli::parse_from(["aver", "context", "examples/modules/app.av", "--depth", "2"]);
        match cli.command {
            Commands::Context { depth, .. } => {
                assert_eq!(depth, ContextDepth::Limited(2));
            }
            _ => panic!("expected context command"),
        }
    }

    #[test]
    fn context_accepts_focus_symbol() {
        let cli = Cli::parse_from([
            "aver",
            "context",
            "examples/modules/app.av",
            "--focus",
            "Json.fromString",
        ]);
        match cli.command {
            Commands::Context { focus, .. } => {
                assert_eq!(focus.as_deref(), Some("Json.fromString"));
            }
            _ => panic!("expected context command"),
        }
    }
}
