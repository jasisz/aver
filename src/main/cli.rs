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

/// Deployment bundle pack. Independent of compiler target — the same
/// `--target wasm-gc` artifacts can ship to Cloudflare Workers, Fastly
/// Compute, Deno Deploy, etc.; `--pack` decides which extra bootstrap
/// files (worker.js, wrangler.toml, …) the compiler drops next to
/// user.wasm so the deployment is one `wrangler deploy` away.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum DeployPack {
    /// No pack: just user.wasm, the host wires it up.
    None,
    /// Emit `worker.js` + `wrangler.toml` for `wrangler deploy`.
    Cloudflare,
}

/// One-flag UX shortcut that expands to a `(target, pack)` preset.
/// `--preset cloudflare` ≡ `--target wasm-gc --pack cloudflare`.
/// Requires `--handler <fn>` (the Aver fn with signature
/// `Fn(HttpRequest) -> HttpResponse` to expose as the worker's
/// request handler). Equivalent CLI surface, fewer keystrokes.
///
/// Workerd's V8 ships stable wasm-gc + tail calls, the runtime is
/// inlined as per-instantiation `__rt_*` helpers (no
/// `WebAssembly.instantiate(bytes, …)` from runtime-fetched bytes —
/// that's the path Cloudflare Workers reject).
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum DeployPreset {
    /// wasm-gc + Cloudflare worker.js/wrangler.toml. Requires --handler.
    Cloudflare,
}

/// Optional post-pass optimization mode for generated WASM modules.
/// Triggers a multi-stage pipeline (wasm-metadce → wasm-opt with
/// converge + strip-producers + strip-target-features), so the flag
/// is `--optimize` rather than `--wasm-opt` — it does more than just
/// invoking the `wasm-opt` binary.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum WasmOptMode {
    /// Optimize for runtime speed.
    #[value(name = "speed")]
    O3,
    /// Optimize aggressively for binary size.
    #[value(name = "size")]
    Oz,
}

/// Compile target language.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, ValueEnum)]
pub(super) enum CompileTarget {
    /// Generate a Rust/Cargo project (default).
    #[default]
    #[value(name = "rust")]
    Rust,
    /// Native WebAssembly GC + tail-call output — no custom runtime,
    /// no NaN-boxing, no boundary GC framing. The recommended target
    /// from 0.16 onward. Pairs with `--handler X` to synthesise a
    /// JS-callable `aver_http_handle` wrapper for fetch-bridge
    /// deployments (see `tools/edge/`). Requires Chrome 119+ /
    /// Firefox 120+ / Safari 18.2+ / wasmtime 25+ / Node 22+ /
    /// Cloudflare Workers. See `src/codegen/wasm_gc/README.md` for
    /// the design notes and the cross-engine bench matrix.
    #[value(name = "wasm-gc")]
    WasmGc,
    /// WASI 0.2 / Component Model output (`--target wasip2`). Wraps
    /// a wasm-gc core module via `wit-component` plus a direct WIT
    /// lowering of Aver effects to canonical-ABI WASI imports — no
    /// preview-1 adapter. Emits `.component.wasm` plus a sibling
    /// `.wit`. Peer target with `--target wasm-gc`, not a successor —
    /// `wasm-gc` runs on browsers, Workers, and JS hosts via `aver/*`
    /// host imports; `wasip2` runs on wasmtime, Spin, NGINX Unit,
    /// wasmCloud, and every other Component Model host via canonical
    /// WIT imports. `Terminal.*` is rejected at compile time. See
    /// `docs/wasip2.md` for the full contract.
    #[value(name = "wasip2")]
    Wasip2,
}

/// WASI 0.2 world the component targets. Used only by
/// `--target wasip2`; ignored by every other target. Defaults to
/// `wasi:cli/command` (a long-lived process exporting `wasi:cli/run`).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, ValueEnum)]
pub(super) enum Wasip2World {
    /// Standard CLI / long-running process. Exports `wasi:cli/run`.
    /// Effects reach the host via `wasi:cli/{stdin,stdout,stderr,
    /// environment}`, `wasi:filesystem/{types,preopens}`,
    /// `wasi:clocks`, `wasi:random`, `wasi:http/outgoing-handler`,
    /// `wasi:sockets/tcp`. `Terminal.*` is rejected at compile time.
    #[default]
    #[value(name = "wasi:cli/command")]
    CliCommand,
    /// HTTP server shape (Phase 3 / 0.19). Exports
    /// `wasi:http/incoming-handler`. Compile-rejected in 0.18 unless
    /// the implementation lands cleanly inside the release window.
    #[value(name = "wasi:http/proxy")]
    HttpProxy,
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
        /// Record effect calls and persist a replay session JSON into this directory
        #[arg(long)]
        record: Option<String>,
        /// Execute an arbitrary Aver call expression as entry point instead of main.
        /// Example: --expr 'loadTaxRate("PL")'. Repeat the flag to record a batch.
        #[arg(short = 'e', long = "expr", value_name = "CALL_EXPR")]
        expr: Vec<String>,
        /// Read the call expression from a file (use '-' for stdin).
        /// Mutually exclusive with --expr.
        #[arg(long = "input-file", value_name = "PATH", conflicts_with = "expr")]
        input_file: Option<String>,
        /// Execute using the self-hosted Aver interpreter compiled to Rust
        #[arg(long, conflicts_with = "profile")]
        self_host: bool,
        /// Print VM opcode/function profile after execution
        #[arg(long)]
        profile: bool,
        /// Compile via the wasm-gc backend (engine GC + tail calls) and
        /// execute with built-in host. Effects reach the host through the
        /// `aver/*` import ABI.
        #[arg(long = "wasm-gc", conflicts_with_all = ["self_host", "profile", "wasip2"])]
        wasm_gc: bool,
        /// Compile to a WASI 0.2 component (`--target wasip2` shape) and
        /// execute it via embedded wasmtime + wasmtime-wasi. Aver effects
        /// reach the host through canonical-ABI WASI imports — no `aver/*`
        /// bridge, no preview-1 adapter. Same effect set as
        /// `aver compile --target wasip2`: `Console.{print, error, warn}`,
        /// `Time.unixMs`, `Random.{int, float}` work today; `Terminal`,
        /// `Env.set`, `Time.sleep`, `Http`, `Tcp`, `HttpServer` are
        /// rejected; `Console.readLine`, `Args.get`, `Env.get`, `Disk.*`
        /// pending later phases of 0.18 "Span".
        #[arg(long = "wasip2", conflicts_with_all = ["self_host", "profile", "wasm_gc"])]
        wasip2: bool,
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
        /// Show full diagnostic detail (source snippets on failures)
        #[arg(long)]
        verbose: bool,
        /// Output diagnostics as JSON (NDJSON, one object per line)
        #[arg(long)]
        json: bool,
        /// Re-run each `verify ... law` against an adversarial world. Boundary
        /// values for declared domains (Int min/max/0/±1, String empty/long/edge
        /// unicode, ...) and worst-case classified-effect responses (Time.now
        /// goes backward, Disk.readText returns Err, Random.int returns
        /// boundaries, ...). Compared against the declared run, divergences are
        /// reported on stderr — catches one-sided assumptions in both
        /// directions (law assumed nice-world, or law assumed only-hostile).
        /// `when` clauses stay binding; `given` ranges become exploration
        /// hints, not contracts.
        #[arg(long)]
        hostile: bool,
        /// Execute verify cases via the wasm-gc backend instead of the
        /// VM. Cross-target check: catches divergences between VM and
        /// wasm-gc codegen on equality. Cross-module `depends [...]`
        /// supported; primitive return types render actual runtime
        /// values on fail. Trace projections (`.trace.*`), classified-
        /// effect Oracle stubs (`given X: Time = stub`), and case
        /// bodies mentioning `BranchPath` are rejected with a pointer
        /// back to plain `aver verify` (VM).
        #[arg(long = "wasm-gc")]
        wasm_gc: bool,
    },
    /// Run check + verify + format-check in one pass
    Audit {
        /// File or directory to audit (default: current directory)
        #[arg(default_value = ".")]
        path: String,
        /// Project module root for dependency resolution
        #[arg(long)]
        module_root: Option<String>,
        /// Emit NDJSON AnalysisReport bundles — one per file, trailing summary
        #[arg(long)]
        json: bool,
        /// Forward `--hostile` to the verify step. See `aver verify --hostile`.
        #[arg(long)]
        hostile: bool,
    },
    /// Format Aver source files
    Format {
        /// File or directory to format (default: current directory)
        #[arg(default_value = ".")]
        path: String,
        /// Check formatting only (non-zero exit if changes would be made)
        #[arg(long)]
        check: bool,
        /// Emit NDJSON AnalysisReport bundles for files needing formatting
        /// (implies --check). One bundle per file, trailing summary.
        #[arg(long)]
        json: bool,
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
        /// Replay using the self-hosted Aver interpreter compiled to Rust
        #[arg(long, conflicts_with = "wasm_gc")]
        self_host: bool,
        /// Replay through the wasm-gc backend (engine GC + tail calls).
        /// Same trace shape as VM/self-host — every backend reads the
        /// JSON recording the VM, self-host, or `aver run --wasm-gc
        /// --record` produced.
        #[arg(long = "wasm-gc")]
        wasm_gc: bool,
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
        /// Compile target: rust (default), wasm-gc, wasip2
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
        /// Deployment bundle pack — drops extra files (worker.js,
        /// wrangler.toml, …) next to user.wasm so the build is one
        /// platform-CLI command away from running. Independent of
        /// `--target`.
        #[arg(long, value_enum)]
        pack: Option<DeployPack>,
        /// One-flag preset that expands to a `(target, pack)` pair.
        /// `cloudflare` ≡ `--target wasm-gc --pack cloudflare` (also
        /// requires `--handler <fn>`). Mutually exclusive with explicit
        /// `--target` / `--pack` — pick one shape of UX.
        #[arg(long, value_enum, conflicts_with_all = &["target", "pack"])]
        preset: Option<DeployPreset>,
        /// Top-level Aver function to expose as the HTTP handler in
        /// the deployment pack. Must have signature
        /// `Fn(HttpRequest) -> HttpResponse`. Compiler exports it
        /// as `aver_http_handle`; the pack's bootstrap (worker.js
        /// for Cloudflare, etc.) routes requests through it. No
        /// magic detection of `HttpServer.listen` in `main` — the
        /// handler is whatever you point this flag at.
        #[arg(long)]
        handler: Option<String>,
        /// WASI 0.2 world the component targets. Used only by
        /// `--target wasip2`; ignored otherwise. Default
        /// `wasi:cli/command` (long-running process exporting
        /// `wasi:cli/run`). `wasi:http/proxy` is Phase 3 and
        /// compile-rejected in 0.18 unless ready.
        #[arg(long, value_enum, default_value = "wasi:cli/command")]
        world: Wasip2World,
        /// Post-process generated WASM through a multi-stage size/speed
        /// pipeline (wasm-metadce → wasm-opt --converge --strip-*).
        /// Pass `size` for aggressive size reduction (`-Oz`) or `speed`
        /// for runtime tuning (`-O3`).
        #[arg(long, value_enum)]
        optimize: Option<WasmOptMode>,
        /// Print the IR after the named pipeline stage and exit before codegen.
        /// One of: `tco`, `typecheck`, `interp_lower`, `buffer_build`, `resolve`.
        /// Use `--emit-ir-after=resolve` to see the final IR that goes into
        /// codegen. Pass `parse` to see the AST as the parser produced it,
        /// before any pass runs.
        #[arg(long, value_name = "PASS")]
        emit_ir_after: Option<String>,
        /// Run the full pipeline and print a per-pass diagnostic report
        /// describing what fired (tail-call conversions, interpolations
        /// lowered, fusion sites rewritten, sinks synthesized, slots
        /// resolved, last-use markers annotated, alloc/recursion facts).
        /// Drives the failable-invariant CI checks ("fail if buffer_build
        /// no longer fires on the canonical shape", "fail if hot fn
        /// loses no-alloc status"). Output is human-readable; pair with
        /// `--emit-ir-after=PASS` to inspect the actual IR. Pair with
        /// `--json` for a machine-readable shape consumable by CI scripts.
        #[arg(long, default_value_t = false)]
        explain_passes: bool,
        /// JSON output for `--explain-passes`. One object per pass
        /// (`stage`, `summary`, `details`), top-level wrapper has
        /// `schema_version: 1` so consumers can pin against the shape.
        #[arg(long, default_value_t = false)]
        json: bool,
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
    /// Run benchmark scenario(s) and report wall-time stats.
    ///
    /// `scenario` can be a single TOML manifest (`bench/scenarios/fib.toml`)
    /// or a directory containing them — the directory form globs every
    /// `*.toml` file inside, runs each in alphabetical order, and emits
    /// one report per scenario. With `--json`, batch runs emit NDJSON
    /// (one report per line) so consumers can stream them.
    ///
    /// Human-readable text by default; `--json` produces the structured
    /// shape used by `--compare baseline.json` (0.15.2 CI gate).
    Bench {
        /// What to run:
        /// - `bench/scenarios/foo.toml` — named manifest (per-scenario tolerances, expected shape)
        /// - `bench/scenarios/foo.av`  — ad-hoc, defaults + `--iterations` / `--warmup` overrides
        /// - `bench/scenarios/`         — directory globs every `*.toml`, alphabetical
        scenario: String,
        /// Bench target: `vm` (in-process), `wasm-gc` (wasm-gc bytes
        /// under wasmtime — engine ceiling for alloc-heavy workloads),
        /// `wasm-gc-v8` (the same bytes under V8 via Node 22+ and
        /// `tools/wasm-gc-bench-v8.mjs` — production-relevant for
        /// browser / Cloudflare Workers / Node / Bun / Deno deploys;
        /// alloc-heavy scenarios run dramatically faster on V8),
        /// `rust` (native binary via `aver compile --target rust` +
        /// `cargo build`). All `wasm-*` targets require the `wasm`
        /// feature.
        #[arg(long, default_value = "vm")]
        target: String,
        /// Number of timed iterations (ad-hoc `.av` mode only; ignored
        /// for `.toml` and directory mode). Default 30.
        #[arg(long, value_name = "N")]
        iterations: Option<usize>,
        /// Warmup iterations not included in stats (ad-hoc `.av` mode
        /// only). Default 3.
        #[arg(long, value_name = "N")]
        warmup: Option<usize>,
        /// Emit the structured JSON report instead of the human-readable
        /// summary. Use this in CI / scripts; the JSON shape is the
        /// stable contract for `--compare baseline.json`.
        #[arg(long)]
        json: bool,
        /// Save the resulting report (JSON) at `PATH`. Use this once on
        /// a stable machine to capture a baseline; subsequent runs compare
        /// against it via `--compare`.
        #[arg(long, value_name = "PATH")]
        save_baseline: Option<String>,
        /// Compare the current run against a baseline JSON report at
        /// `PATH`. Prints a diff (per-metric delta vs. tolerance) and,
        /// when combined with `--fail-on-regression`, exits 1 if any
        /// gated metric is over its tolerance budget.
        #[arg(long, value_name = "PATH", conflicts_with = "baseline_dir")]
        compare: Option<String>,
        /// Auto-pick a baseline file from `DIR` based on the running
        /// machine: `<host.os>-<host.arch>-<backend.name>.json` (e.g.
        /// `macos-aarch64-vm.json`). Falls back silently to no-compare
        /// when no matching file exists — lets a single CI workflow
        /// gate against a per-host pinned baseline without per-runner
        /// branching. Mutually exclusive with `--compare`.
        #[arg(long, value_name = "DIR", conflicts_with = "compare")]
        baseline_dir: Option<String>,
        /// Exit non-zero when `--compare` / `--baseline-dir` finds a
        /// regression. Use this in CI to gate merges on bench numbers
        /// staying within tolerance.
        #[arg(long)]
        fail_on_regression: bool,
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
            Commands::Verify { file, deps, .. } => {
                assert_eq!(file, "examples/modules/app.av");
                assert!(deps);
            }
            _ => panic!("expected verify command"),
        }
    }

    #[test]
    fn run_accepts_self_host_flag() {
        let cli = Cli::parse_from(["aver", "run", "examples/modules/app.av", "--self-host"]);
        match cli.command {
            Commands::Run { self_host, .. } => {
                assert!(self_host);
            }
            _ => panic!("expected run command"),
        }
    }

    #[test]
    fn replay_accepts_self_host_flag() {
        let cli = Cli::parse_from(["aver", "replay", "recordings", "--self-host"]);
        match cli.command {
            Commands::Replay { self_host, .. } => {
                assert!(self_host);
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
    fn compile_accepts_target_wasip2_with_default_world() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/core/hello.av",
            "--target",
            "wasip2",
        ]);
        match cli.command {
            Commands::Compile { target, world, .. } => {
                assert_eq!(target, CompileTarget::Wasip2);
                assert_eq!(world, Wasip2World::CliCommand);
            }
            _ => panic!("expected compile command"),
        }
    }

    #[test]
    fn compile_accepts_target_wasip2_with_explicit_world() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/core/hello.av",
            "--target",
            "wasip2",
            "--world",
            "wasi:http/proxy",
        ]);
        match cli.command {
            Commands::Compile { target, world, .. } => {
                assert_eq!(target, CompileTarget::Wasip2);
                assert_eq!(world, Wasip2World::HttpProxy);
            }
            _ => panic!("expected compile command"),
        }
    }

    #[test]
    fn compile_accepts_optimize() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/core/hello.av",
            "--target",
            "wasm-gc",
            "--optimize",
            "size",
        ]);
        match cli.command {
            Commands::Compile {
                target, optimize, ..
            } => {
                assert_eq!(target, CompileTarget::WasmGc);
                assert_eq!(optimize, Some(WasmOptMode::Oz));
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
