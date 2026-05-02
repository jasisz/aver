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

/// Deployment-time bridge that satisfies user.wasm's `aver/*` host
/// imports. user.wasm bytes are identical regardless of bridge — the
/// choice only affects what gets bundled with `--target wasm`. With
/// `--target edge-wasm` the bridge is irrelevant (thin output, host
/// wires imports at instantiate time).
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum WasmBridge {
    /// No bridge: aver/* imports left unresolved. Consumer host
    /// (browser playground, `aver run --wasm`, custom edge runtime)
    /// supplies them at instantiate time.
    None,
    /// Translate aver/* → wasi_snapshot_preview1.* via a bundled
    /// shim. Lets `wasmtime program.wasm` run standalone without an
    /// external host. The "p1" matches the WASI spec community's
    /// shorthand for preview 1 (paired with `wasip2`/`wasip3`
    /// elsewhere) and pins this bridge to core-wasm preview 1
    /// specifically. WASI 0.2 / Component Model gets its own
    /// compilation target (`--target wasi-http`) rather than
    /// another bridge, since it's a different model end-to-end
    /// (component wasm output, WIT worlds, host-owned accept
    /// loop) — not a swap-out shim.
    #[value(name = "wasip1")]
    Wasip1,
    /// Translate aver/* → JS host APIs (`console.log`, `Date.now()`,
    /// `crypto.getRandomValues`, the Fetch API). The right choice
    /// for any JS-environment edge runtime — Cloudflare Workers,
    /// Fastly Compute (when bundled JS shim is acceptable), Deno
    /// Deploy, Bun, Node. Pairs with `--target edge-wasm` and a
    /// per-host deployment pack.
    Fetch,
}

/// Deployment bundle pack. Independent of compiler target and bridge —
/// the same `--target edge-wasm --bridge fetch` artifacts can be
/// shipped to Cloudflare Workers, Fastly Compute, Deno Deploy, etc.;
/// `--pack` decides which extra bootstrap files (worker.js,
/// wrangler.toml, fastly.toml, …) the compiler drops next to
/// user.wasm so the deployment is one `wrangler deploy` away.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum DeployPack {
    /// No pack: just user.wasm, the host wires it up.
    None,
    /// Emit `worker.js` + `wrangler.toml` for `wrangler deploy`.
    Cloudflare,
}

/// One-flag UX shortcut that expands to a `(target, bridge, pack)`
/// preset. `--preset cloudflare` ≡ `--target wasm --bridge fetch
/// --pack cloudflare`. Equivalent CLI surface, fewer keystrokes.
///
/// Cloudflare Workers reject `WebAssembly.instantiate(bytes, …)` from
/// runtime-fetched bytes (sandbox security). Only statically imported
/// wasm modules are accepted, so `--target edge-wasm`'s "thin
/// user.wasm + imported runtime from CDN" architecture doesn't apply
/// on this host — the preset uses `--target wasm` (wasm-merge inlines
/// the runtime into a single bundled module that worker.js imports
/// statically). Browsers / Deno / Bun keep the edge-wasm shape via
/// the runtime CDN at averlang.dev/runtime/.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum DeployPreset {
    /// wasm + fetch bridge + Cloudflare worker.js/wrangler.toml.
    Cloudflare,
}

/// Which runtime artifact `aver wasm-runtime` emits.
#[derive(Clone, Copy, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum WasmRuntimeArtifact {
    /// The shared aver_runtime module (alloc, GC, str/list/map/vec ops).
    #[value(name = "runtime")]
    Runtime,
    /// The aver→WASI translation shim that satisfies `aver/*` host
    /// imports against `wasi_snapshot_preview1.fd_write`.
    #[value(name = "wasi-bridge")]
    WasiBridge,
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
    /// Legacy fallback (`--target wasm`) — bundled .wasm with the
    /// custom Aver runtime inlined via `wasm-merge`. Recommended only
    /// for pre-2024 hosts that don't speak the WebAssembly GC + tail-
    /// call proposals (wasmtime CLI < 25, Node < 22, etc.). For
    /// modern targets prefer `--target wasm-gc` — same source, same
    /// `--handler X` shape, faster on most workloads (8.4× on `fib`,
    /// 182× on `vector_ops`, ~ tied on f64-heavy renderers like the
    /// fractal demo), and a 35 % smaller binary because there's no
    /// custom runtime to ship.
    #[value(name = "wasm")]
    Wasm,
    /// Legacy edge variant — thin .wasm that imports the runtime as a
    /// separate module instead of inlining it. Same fallback role as
    /// `--target wasm`; pick this for browser playgrounds / dev
    /// workflows where one shared runtime serves many programs.
    /// Modern targets should prefer `--target wasm-gc`.
    #[value(name = "edge-wasm")]
    EdgeWasm,
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
}

impl CompileTarget {
    pub(super) fn needs_wasm_pipeline(self) -> bool {
        matches!(
            self,
            CompileTarget::Wasm | CompileTarget::EdgeWasm | CompileTarget::WasmGc
        )
    }
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
        /// Compile to WASM and execute with built-in host (aver/* import ABI)
        #[arg(long, conflicts_with_all = ["self_host", "profile"])]
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
        #[arg(long)]
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
        /// Deployment-time bridge for WASM `aver/*` host imports.
        /// `wasip1` bundles the aver→wasi shim so `wasmtime program.wasm`
        /// runs standalone; `fetch` translates aver/* to JS host APIs
        /// (Cloudflare Workers, Deno, Bun); `none` (default) leaves
        /// aver/* unresolved for the consumer host to satisfy.
        #[arg(long, value_enum)]
        bridge: Option<WasmBridge>,
        /// Deployment bundle pack — drops extra files (worker.js,
        /// wrangler.toml, …) next to user.wasm so the build is one
        /// platform-CLI command away from running. Independent of
        /// `--target` and `--bridge`.
        #[arg(long, value_enum)]
        pack: Option<DeployPack>,
        /// One-flag preset that expands to a `(target, bridge, pack)`
        /// triple. `cloudflare` ≡ `--target edge-wasm --bridge fetch
        /// --pack cloudflare`. Mutually exclusive with explicit
        /// `--target` / `--bridge` / `--pack` — pick one shape of UX.
        #[arg(long, value_enum, conflicts_with_all = &["target", "bridge", "pack"])]
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
    /// Emit a standalone aver_runtime / aver_to_wasi artifact to disk.
    /// Internal release tooling — used by tools/release/* to publish
    /// per-version runtime modules to averlang.dev. Not part of the
    /// user-facing CLI surface.
    #[command(hide = true)]
    WasmRuntime {
        /// Output file path (e.g. dist/aver_runtime.wasm)
        #[arg(short = 'o', long)]
        output: String,
        /// Which runtime artifact to emit. `runtime` (default) is the
        /// shared aver_runtime module imported by every user.wasm;
        /// `wasi-bridge` is the aver→wasi translation shim.
        #[arg(long, value_enum, default_value = "runtime")]
        artifact: WasmRuntimeArtifact,
        /// Apply the same optimize pipeline used for user code, but
        /// in library mode (every export is a DCE root). `size` runs
        /// `-Oz --converge --strip-*`; `speed` is `-O3`. Default: raw.
        #[arg(long, value_enum)]
        optimize: Option<WasmOptMode>,
        /// Also emit a human-readable .wat companion next to the .wasm
        /// (uses `wasm-tools print`).
        #[arg(long)]
        wat: bool,
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
    /// 0.15.1 ships VM target only; `wasm-local` and `wasm-cloudflare`
    /// follow in 0.15.2.
    Bench {
        /// What to run:
        /// - `bench/scenarios/foo.toml` — named manifest (per-scenario tolerances, expected shape)
        /// - `bench/scenarios/foo.av`  — ad-hoc, defaults + `--iterations` / `--warmup` overrides
        /// - `bench/scenarios/`         — directory globs every `*.toml`, alphabetical
        scenario: String,
        /// Bench target: `vm` (in-process), `wasm-local` (wasmtime
        /// in-process, requires the `wasm` feature), `rust` (native
        /// binary via `aver compile --target rust` + `cargo build`).
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
    fn compile_accepts_optimize() {
        let cli = Cli::parse_from([
            "aver",
            "compile",
            "examples/core/hello.av",
            "--target",
            "wasm",
            "--optimize",
            "size",
        ]);
        match cli.command {
            Commands::Compile {
                target, optimize, ..
            } => {
                assert_eq!(target, CompileTarget::Wasm);
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
