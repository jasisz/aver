//! Baseline comparison — diff a current `BenchReport` against a stored
//! one and decide whether the run regressed past the configured tolerance.
//!
//! Gated metrics (any failure flips `regressed=true`):
//! - `p50_ms` / `p95_ms` (per `[tolerance]` budget)
//! - `compiler_visible_allocs` (exact match — growth past baseline is a
//!   regression, shrinkage is reported as improvement)
//! - `response_bytes` (exact match when both sides have a number)
//!
//! `passes_applied` mismatch is reported as a note but not gated —
//! pipeline refactors legitimately change which stages run.

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
    // `response_bytes` exact-match: any mismatch is reported and gated.
    // Output bytes change ⇒ semantics changed (different result, different
    // formatting) ⇒ regression. `None` on either side is a wash (capture
    // not available), no gate.
    let mut response_bytes_regressed = false;
    if let (Some(c), Some(b)) = (current.response_bytes, baseline.response_bytes)
        && c != b
    {
        notes.push(format!(
            "response_bytes: baseline={} current={} — regression",
            b, c
        ));
        response_bytes_regressed = true;
    }

    // `compiler_visible_allocs` is exact-match — any change is reported.
    // Growing past baseline is a regression (a hot fn just learned to
    // allocate); shrinking is good but worth noting (a fusion fired
    // that didn't before, codegen got tighter, or the manifest changed).
    let mut allocs_regressed = false;
    if let (Some(c), Some(b)) = (
        current.compiler_visible_allocs,
        baseline.compiler_visible_allocs,
    ) && c != b
    {
        if c > b {
            notes.push(format!(
                "compiler_visible_allocs grew: baseline={} current={} — regression",
                b, c
            ));
            allocs_regressed = true;
        } else {
            notes.push(format!(
                "compiler_visible_allocs shrank: baseline={} current={} — improvement",
                b, c
            ));
        }
    }

    DiffReport {
        scenario: current.scenario.name.clone(),
        p50,
        p95,
        regressed: p50.regressed || p95.regressed || allocs_regressed || response_bytes_regressed,
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
