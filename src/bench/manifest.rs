//! Scenario manifest: TOML on disk, struct in memory.
//!
//! ```toml
//! # bench/scenarios/factorial.toml
//! name  = "factorial"             # optional, defaults to file stem
//! entry = "/tmp/factorial.av"     # absolute or relative to manifest dir
//! iterations = 50
//! warmup     = 5
//! args       = []                 # CLI args passed to `main`, default empty
//!
//! [expected]
//! # response_bytes = 23           # exact match (optional)
//! # response_bytes_min = 100      # range match (optional)
//! # response_bytes_max = 200
//! ```
//!
//! Keep the format dumb: TOML, flat, no env vars, no string templates.
//! A scenario is meant to be the simplest reproducible thing that pins
//! a measurement, not a generic test runner. If a scenario needs more
//! than this, that's a sign to write a small Aver harness fn in the
//! `entry` file rather than to grow this struct.

use std::path::{Path, PathBuf};

#[derive(Debug)]
pub enum ManifestError {
    Read(String),
    Parse(String),
    Validate(String),
}

impl std::fmt::Display for ManifestError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Read(m) | Self::Parse(m) | Self::Validate(m) => f.write_str(m),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Manifest {
    pub name: String,
    /// Absolute path to the entry `.av` file.
    pub entry: PathBuf,
    pub iterations: usize,
    pub warmup: usize,
    pub args: Vec<String>,
    pub expected: ExpectedShape,
    pub tolerance: Tolerance,
}

/// Bench targets — picks which backend runs the scenario.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BenchTarget {
    /// In-process VM via `vm::compile_program_with_modules`.
    Vm,
    /// Native Rust binary produced by `aver compile --target rust` +
    /// `cargo build --release`. Subprocess spawn per iteration.
    Rust,
    /// In-process wasm-gc backend via wasmtime with the GC + tail-call
    /// proposals enabled. Compiles once with `aver compile --target
    /// wasm-gc`, instantiates + invokes per iteration. wasmtime's GC
    /// heap is the ceiling for alloc-heavy scenarios — the V8 column
    /// (below) is the production-relevant number for any wasm-gc
    /// deploy that lands in a browser, Cloudflare Workers, Node, Bun,
    /// or Deno. Keep both around so the gap is visible.
    WasmGc,
    /// Same wasm-gc bytes (`aver compile --target wasm-gc`), executed
    /// under V8 via Node 22+ and `tools/wasm-gc-bench-v8.mjs`. Spawns
    /// one Node process per scenario, parses raw sample timings off
    /// stdout, then stitches the full `BenchReport` Rust-side. Memory
    /// `project_v8_vs_wasmtime_gc.md` documents the engine gap that
    /// motivates the dual column (string_interp is 2300× faster on V8
    /// than on wasmtime as of 0.17.2). Requires `node` v22+ on PATH
    /// or under `~/.nvm/versions/node/v22.*` — earlier Nodes ship a
    /// V8 that rejects packed `i8` arrays.
    WasmGcV8,
}

impl BenchTarget {
    pub fn parse(s: &str) -> Result<Self, String> {
        match s {
            "vm" => Ok(Self::Vm),
            "wasm-gc" => Ok(Self::WasmGc),
            "wasm-gc-v8" => Ok(Self::WasmGcV8),
            "rust" => Ok(Self::Rust),
            other => Err(format!(
                "unknown bench target '{}'; expected one of: vm, wasm-gc, wasm-gc-v8, rust",
                other
            )),
        }
    }

    pub const fn name(self) -> &'static str {
        match self {
            Self::Vm => "vm",
            Self::WasmGc => "wasm-gc",
            Self::WasmGcV8 => "wasm-gc-v8",
            Self::Rust => "rust",
        }
    }
}

/// Per-metric regression tolerances used by `--compare baseline.json`.
/// Defaults are deliberately loose for 0.15.1 — the bench harness is new
/// and machines vary. Tighten per-scenario in TOML once a baseline is
/// stable on the target machine.
#[derive(Debug, Clone, Copy)]
pub struct Tolerance {
    /// Maximum allowed `p50_ms` increase as a percentage of baseline.
    /// Default 20.0 (i.e. +20% over baseline counts as a regression).
    pub wall_time_p50_pct: f64,
    /// Maximum allowed `p95_ms` increase, percentage. Default 30.0 —
    /// p95 is noisier than p50 by nature, so the bar is looser.
    pub wall_time_p95_pct: f64,
}

impl Default for Tolerance {
    fn default() -> Self {
        Self {
            wall_time_p50_pct: 20.0,
            wall_time_p95_pct: 30.0,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct ExpectedShape {
    pub response_bytes: Option<usize>,
    pub response_bytes_min: Option<usize>,
    pub response_bytes_max: Option<usize>,
}

impl Manifest {
    pub fn load(path: &Path) -> Result<Self, ManifestError> {
        let raw = std::fs::read_to_string(path)
            .map_err(|e| ManifestError::Read(format!("cannot read '{}': {}", path.display(), e)))?;
        let table: toml::Value = raw.parse().map_err(|e: toml::de::Error| {
            ManifestError::Parse(format!("{}: {}", path.display(), e))
        })?;
        let manifest_dir = path.parent().unwrap_or_else(|| Path::new("."));
        Self::from_toml(&table, path, manifest_dir)
    }

    fn from_toml(
        table: &toml::Value,
        manifest_path: &Path,
        manifest_dir: &Path,
    ) -> Result<Self, ManifestError> {
        let entry_raw = table.get("entry").and_then(|v| v.as_str()).ok_or_else(|| {
            ManifestError::Validate(format!(
                "{}: missing required `entry = \"...\"`",
                manifest_path.display()
            ))
        })?;
        let entry_path = Path::new(entry_raw);
        let entry = if entry_path.is_absolute() {
            entry_path.to_path_buf()
        } else {
            manifest_dir.join(entry_path)
        };
        if !entry.exists() {
            return Err(ManifestError::Validate(format!(
                "{}: entry '{}' does not exist (resolved to '{}')",
                manifest_path.display(),
                entry_raw,
                entry.display()
            )));
        }

        let name = table
            .get("name")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                manifest_path
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("scenario")
                    .to_string()
            });

        let iterations = table
            .get("iterations")
            .and_then(|v| v.as_integer())
            .unwrap_or(10) as usize;
        let warmup = table
            .get("warmup")
            .and_then(|v| v.as_integer())
            .unwrap_or(1) as usize;

        if iterations == 0 {
            return Err(ManifestError::Validate(format!(
                "{}: `iterations` must be > 0",
                manifest_path.display()
            )));
        }

        let args = table
            .get("args")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default();

        let expected = match table.get("expected") {
            Some(toml::Value::Table(t)) => ExpectedShape {
                response_bytes: t
                    .get("response_bytes")
                    .and_then(|v| v.as_integer())
                    .map(|n| n as usize),
                response_bytes_min: t
                    .get("response_bytes_min")
                    .and_then(|v| v.as_integer())
                    .map(|n| n as usize),
                response_bytes_max: t
                    .get("response_bytes_max")
                    .and_then(|v| v.as_integer())
                    .map(|n| n as usize),
            },
            _ => ExpectedShape::default(),
        };

        let tolerance = match table.get("tolerance") {
            Some(toml::Value::Table(t)) => {
                let mut tol = Tolerance::default();
                if let Some(v) = t.get("wall_time_p50_pct").and_then(|v| v.as_float()) {
                    tol.wall_time_p50_pct = v;
                }
                if let Some(v) = t.get("wall_time_p95_pct").and_then(|v| v.as_float()) {
                    tol.wall_time_p95_pct = v;
                }
                tol
            }
            _ => Tolerance::default(),
        };

        Ok(Manifest {
            name,
            entry,
            iterations,
            warmup,
            args,
            expected,
            tolerance,
        })
    }
}
