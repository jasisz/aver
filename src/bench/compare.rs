//! Baseline comparison — diff a current `BenchReport` against a stored
//! one and decide whether the run regressed past the configured tolerance.
//!
//! Two metrics gated in 0.15.1: `p50_ms` and `p95_ms`. `response_bytes`
//! and `passes_applied` mismatches are reported but not gated yet —
//! once `response_bytes` capture lands in 0.15.2, exact-match becomes
//! a hard regression. `passes_applied` mismatch is interesting (a pass
//! stopped firing) but happens legitimately when a pipeline-level
//! refactor changes which stages run, so it's reported at info level
//! for now.

use crate::bench::manifest::Tolerance;
use crate::bench::report::BenchReport;

#[derive(Debug)]
pub struct DiffReport {
    pub scenario: String,
    pub p50: MetricDiff,
    pub p95: MetricDiff,
    /// `true` if any gated metric exceeded its tolerance.
    pub regressed: bool,
    /// Non-gated observations worth printing — e.g. response_bytes
    /// changed, passes_applied set differs.
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
pub struct MetricDiff {
    pub baseline: f64,
    pub current: f64,
    pub delta_pct: f64,
    pub tolerance_pct: f64,
    pub regressed: bool,
}

impl MetricDiff {
    fn new(baseline: f64, current: f64, tolerance_pct: f64) -> Self {
        let delta_pct = if baseline > 0.0 {
            ((current - baseline) / baseline) * 100.0
        } else {
            0.0
        };
        // Negative delta = faster than baseline = never a regression.
        let regressed = delta_pct > tolerance_pct;
        Self {
            baseline,
            current,
            delta_pct,
            tolerance_pct,
            regressed,
        }
    }
}

pub fn diff(current: &BenchReport, baseline: &BenchReport, tolerance: Tolerance) -> DiffReport {
    let p50 = MetricDiff::new(
        baseline.iterations.p50_ms,
        current.iterations.p50_ms,
        tolerance.wall_time_p50_pct,
    );
    let p95 = MetricDiff::new(
        baseline.iterations.p95_ms,
        current.iterations.p95_ms,
        tolerance.wall_time_p95_pct,
    );

    let mut notes = Vec::new();
    if current.scenario.target != baseline.scenario.target {
        notes.push(format!(
            "target changed: baseline={} current={} — different units, do not trust the diff",
            baseline.scenario.target, current.scenario.target
        ));
    }
    if current.passes_applied != baseline.passes_applied {
        notes.push(format!(
            "passes_applied changed: baseline={:?} current={:?}",
            baseline.passes_applied, current.passes_applied
        ));
    }
    match (current.response_bytes, baseline.response_bytes) {
        (Some(c), Some(b)) if c != b => {
            notes.push(format!("response_bytes: baseline={} current={}", b, c));
        }
        _ => {}
    }

    DiffReport {
        scenario: current.scenario.name.clone(),
        p50,
        p95,
        regressed: p50.regressed || p95.regressed,
        notes,
    }
}

/// Render a diff in the same compact column shape as `format_human` for
/// `BenchReport` — single block per scenario, colour-free (callers add
/// terminal colour at the print site if desired).
pub fn format_diff(diff: &DiffReport) -> String {
    use std::fmt::Write;

    fn fmt_ms(ms: f64) -> String {
        if ms >= 1.0 {
            format!("{:.2}ms", ms)
        } else {
            format!("{:.0}µs", ms * 1000.0)
        }
    }
    fn fmt_metric(label: &str, m: &MetricDiff) -> String {
        let sign = if m.delta_pct >= 0.0 { "+" } else { "" };
        let verdict = if m.regressed {
            format!("REGRESSION (limit +{:.0}%)", m.tolerance_pct)
        } else {
            "ok".to_string()
        };
        format!(
            "  {:<6} {} (baseline {}, {}{:.1}%)  {}",
            label,
            fmt_ms(m.current),
            fmt_ms(m.baseline),
            sign,
            m.delta_pct,
            verdict,
        )
    }

    let mut out = String::new();
    writeln!(
        out,
        "{}: {}",
        diff.scenario,
        if diff.regressed { "REGRESSION" } else { "ok" }
    )
    .ok();
    writeln!(out, "{}", fmt_metric("p50", &diff.p50)).ok();
    writeln!(out, "{}", fmt_metric("p95", &diff.p95)).ok();
    for note in &diff.notes {
        writeln!(out, "  note: {}", note).ok();
    }
    out
}
