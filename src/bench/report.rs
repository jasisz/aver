//! Bench report — the structured JSON shape that `aver bench` emits.
//!
//! This is the contract that `aver bench --compare baseline.json` (0.15.2)
//! and the future CI gate read. Adding fields is fine, removing/renaming
//! is a breaking change to that contract.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchReport {
    pub scenario: ScenarioMetadata,
    /// Identifies the build that ran the bench: aver version, build
    /// profile, target backend, plus optional version strings for
    /// per-target runtimes (e.g. wasmtime for `wasm-local`).
    pub backend: BackendInfo,
    /// OS / architecture / process identity. Same JSON shape across
    /// targets; downstream tools join on `host.os + host.arch + backend.name`
    /// to compare like-for-like across runs.
    pub host: HostInfo,
    pub iterations: IterationStats,
    /// UTF-8 byte count of the last iteration's "result". Semantics
    /// vary by target:
    ///
    /// - `vm`: byte length of `main`'s return value rendered through
    ///   `aver_display` (same path `Console.print` uses). `None` when
    ///   `main` returns `Unit` — those scenarios print for side effect,
    ///   and bench mode silences the console.
    /// - `wasm-local`: total bytes the guest tried to write through
    ///   `fd_write` (sum of iovec lengths) during the last iteration.
    ///   `0` when the guest never called `fd_write` (most scenarios
    ///   that don't print).
    /// - `rust`: actual stdout byte count from the spawned binary's
    ///   subprocess output. `0` when the binary printed nothing.
    ///
    /// VM and wasm-local/rust use different shapes ("rendered return
    /// value" vs "actual stdout"). `aver bench --compare` only ever
    /// matches same-target baselines, so the divergence doesn't break
    /// gating — the field is exact-match within a target.
    pub response_bytes: Option<usize>,
    /// `true` when the run satisfied every `[expected]` constraint in
    /// the manifest. `null` when the manifest has no expectations.
    pub expected_match: Option<bool>,
    /// Pipeline stages that actually fired. Sourced from the pipeline's
    /// `on_after_pass` hook so it reflects what *ran*, not what was
    /// requested.
    pub passes_applied: Vec<String>,
    /// IR-level allocation counter. `null` in 0.15.1 — pending the
    /// `aver compile --explain-allocations` work in 0.15.2.
    pub compiler_visible_allocs: Option<usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackendInfo {
    /// Target name as parsed from `--target`
    /// (`vm` / `wasm-local` / `wasm-gc` / `rust`).
    pub name: String,
    /// Version of the `aver` binary that ran the bench (Cargo package
    /// version at compile time of this binary).
    pub aver_version: String,
    /// `"release"` or `"debug"`, derived from the calling binary's
    /// build profile (`debug_assertions` cfg).
    pub build: String,
    /// wasmtime crate version when the report came from `--target=wasm-local`,
    /// `null` otherwise.
    pub wasmtime_version: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HostInfo {
    /// `"macos"` / `"linux"` / `"windows"` (from `std::env::consts::OS`).
    pub os: String,
    /// `"aarch64"` / `"x86_64"` / `"x86"` etc. (from `std::env::consts::ARCH`).
    pub arch: String,
    /// Logical CPU count from `std::thread::available_parallelism`.
    pub cpus: usize,
}

impl BackendInfo {
    pub fn for_target(target: crate::bench::manifest::BenchTarget) -> Self {
        let build = if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        };
        let wasmtime_version = match target {
            crate::bench::manifest::BenchTarget::WasmGc => Some(WASMTIME_VERSION.to_string()),
            _ => None,
        };
        Self {
            name: target.name().to_string(),
            aver_version: env!("CARGO_PKG_VERSION").to_string(),
            build: build.to_string(),
            wasmtime_version,
        }
    }
}

impl HostInfo {
    pub fn capture() -> Self {
        let cpus = std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);
        Self {
            os: std::env::consts::OS.to_string(),
            arch: std::env::consts::ARCH.to_string(),
            cpus,
        }
    }
}

/// Wasmtime version string compiled into the bench reports. Bumped
/// alongside the `wasmtime` dependency in `Cargo.toml`; downstream
/// tools that compare bench numbers across runs use it to detect
/// runtime upgrades that might explain a delta.
const WASMTIME_VERSION: &str = "29";

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScenarioMetadata {
    pub name: String,
    pub entry: String,
    pub target: String,
    pub iterations_count: usize,
    pub warmup_count: usize,
}

/// Per-iteration wall-clock stats in milliseconds.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IterationStats {
    pub min_ms: f64,
    pub max_ms: f64,
    pub mean_ms: f64,
    pub p50_ms: f64,
    pub p95_ms: f64,
    pub p99_ms: f64,
}

/// Render `report` as a multi-line human-readable summary (default
/// `aver bench` output). The shape is deliberately compact — bench
/// engineers want one glance to read pass list + percentiles, not
/// a wall of pretty-printed JSON.
pub fn format_human(report: &BenchReport) -> String {
    use std::fmt::Write;

    fn fmt_ms(ms: f64) -> String {
        if ms >= 1.0 {
            format!("{:.2}ms", ms)
        } else {
            format!("{:.0}µs", ms * 1000.0)
        }
    }

    let mut out = String::new();
    let s = &report.scenario;
    let b = &report.backend;
    let h = &report.host;
    let it = &report.iterations;
    writeln!(out, "{} [{}]", s.name, s.target).ok();
    writeln!(out, "  entry:        {}", s.entry).ok();
    let mut backend_line = format!("aver {} ({})", b.aver_version, b.build);
    if let Some(wt) = &b.wasmtime_version {
        backend_line.push_str(&format!(", wasmtime {}", wt));
    }
    writeln!(out, "  backend:      {}", backend_line).ok();
    writeln!(out, "  host:         {}/{} ({} cpus)", h.os, h.arch, h.cpus).ok();
    writeln!(
        out,
        "  iterations:   {} (warmup {})",
        s.iterations_count, s.warmup_count
    )
    .ok();
    writeln!(
        out,
        "  passes:       {}",
        if report.passes_applied.is_empty() {
            "(none)".to_string()
        } else {
            report.passes_applied.join(", ")
        }
    )
    .ok();
    writeln!(
        out,
        "  wall_time:    min={}  p50={}  p95={}  max={}  mean={}",
        fmt_ms(it.min_ms),
        fmt_ms(it.p50_ms),
        fmt_ms(it.p95_ms),
        fmt_ms(it.max_ms),
        fmt_ms(it.mean_ms),
    )
    .ok();
    if let Some(bytes) = report.response_bytes {
        writeln!(out, "  response:     {} bytes", bytes).ok();
    }
    if let Some(matched) = report.expected_match {
        writeln!(
            out,
            "  expected:     {}",
            if matched { "ok" } else { "MISMATCH" }
        )
        .ok();
    }
    out
}

impl IterationStats {
    pub fn from_samples(samples: &[f64]) -> Self {
        assert!(!samples.is_empty(), "IterationStats requires ≥1 sample");
        let mut sorted: Vec<f64> = samples.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let n = sorted.len();
        let percentile = |p: f64| -> f64 {
            // Nearest-rank percentile — small N so the choice between
            // nearest-rank and linear-interp doesn't matter much; nearest-
            // rank is dependency-free and reproducible.
            let idx = ((p / 100.0) * (n as f64)).ceil() as usize;
            let idx = idx.saturating_sub(1).min(n - 1);
            sorted[idx]
        };
        IterationStats {
            min_ms: *sorted.first().unwrap(),
            max_ms: *sorted.last().unwrap(),
            mean_ms: sorted.iter().sum::<f64>() / (n as f64),
            p50_ms: percentile(50.0),
            p95_ms: percentile(95.0),
            p99_ms: percentile(99.0),
        }
    }
}
