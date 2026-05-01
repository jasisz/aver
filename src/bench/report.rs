//! Bench report — the structured JSON shape that `aver bench` emits.
//!
//! This is the contract that `aver bench --compare baseline.json` (0.15.2)
//! and the future CI gate read. Adding fields is fine, removing/renaming
//! is a breaking change to that contract.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchReport {
    pub scenario: ScenarioMetadata,
    pub iterations: IterationStats,
    /// Total stdout byte count of the last iteration. `null` in 0.15.1
    /// (capture infrastructure lands with the runtime allocators in
    /// 0.15.2). Used by `expected.response_bytes*` checks once populated.
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
    let it = &report.iterations;
    writeln!(out, "{} [{}]", s.name, s.target).ok();
    writeln!(out, "  entry:        {}", s.entry).ok();
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
