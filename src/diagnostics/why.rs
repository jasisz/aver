//! File-local "why" summary: per-function justification signals plus
//! aggregate line counts. Shared by CLI `aver why` and the playground
//! Why panel — single analysis, two renderers.
//!
//! Runtime-neutral: walks parsed items without touching the VM or the
//! filesystem.

use std::collections::{HashMap, HashSet};

use serde::Serialize;

use crate::ast::{DecisionBlock, FnDef, TopLevel};
use crate::checker::{collect_verify_coverage_warnings, merge_verify_blocks};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Justification {
    /// Description + verify (or description alone for effectful fns), no
    /// coverage gaps.
    Justified,
    /// At least one signal present but incomplete.
    Partial,
    /// No description, no verify, no decision impact.
    Unjustified,
}

impl Justification {
    pub fn priority(self) -> u8 {
        match self {
            Justification::Unjustified => 0,
            Justification::Partial => 1,
            Justification::Justified => 2,
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct FnDetail {
    pub name: String,
    pub lines: usize,
    pub has_description: bool,
    pub is_effectful: bool,
    pub verify_cases: usize,
    pub has_coverage_gaps: bool,
    pub has_decision_impact: bool,
    pub level: Justification,
    pub missing: Vec<&'static str>,
}

impl FnDetail {
    fn compute_level(
        has_description: bool,
        is_effectful: bool,
        verify_cases: usize,
        has_coverage_gaps: bool,
        has_decision_impact: bool,
    ) -> Justification {
        let has_verify = verify_cases > 0;
        if is_effectful {
            if has_description {
                return Justification::Justified;
            }
        } else if has_description && has_verify && !has_coverage_gaps {
            return Justification::Justified;
        }
        if has_description || has_verify || has_decision_impact {
            Justification::Partial
        } else {
            Justification::Unjustified
        }
    }

    fn compute_missing(
        has_description: bool,
        is_effectful: bool,
        verify_cases: usize,
        has_coverage_gaps: bool,
    ) -> Vec<&'static str> {
        let mut missing = Vec::new();
        if !has_description {
            missing.push("no description");
        }
        if is_effectful {
            if !has_description {
                missing.push("effectful — test via replay");
            }
        } else if verify_cases == 0 {
            missing.push("no verify");
        } else if has_coverage_gaps {
            missing.push("verify has gaps");
        }
        missing
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct DecisionSummary {
    pub name: String,
    pub date: String,
    pub reason_prefix: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct WhySummary {
    pub file_label: String,
    pub total_lines: usize,
    pub justified_lines: usize,
    pub partial_lines: usize,
    pub unjustified_lines: usize,
    pub has_module_intent: bool,
    pub decisions: Vec<DecisionSummary>,
    pub functions: Vec<FnDetail>,
}

/// Compute the why summary for a single parsed file. Pure function.
pub fn summarize(items: &[TopLevel], source: &str, file_label: impl Into<String>) -> WhySummary {
    let total_lines = source.lines().count();

    let decisions: Vec<&DecisionBlock> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Decision(d) => Some(d),
            _ => None,
        })
        .collect();

    let impact_symbols: HashSet<String> = decisions
        .iter()
        .flat_map(|d| d.impacts.iter().map(|i| i.node.text().to_string()))
        .collect();

    let merged_verify = merge_verify_blocks(items);
    let mut verify_cases_per_fn: HashMap<String, usize> = HashMap::new();
    for vb in &merged_verify {
        *verify_cases_per_fn.entry(vb.fn_name.clone()).or_default() += vb.cases.len();
    }

    let coverage_warnings = collect_verify_coverage_warnings(items);
    let fns_with_coverage_gaps: HashSet<String> = coverage_warnings
        .iter()
        .filter_map(|w| {
            w.message
                .strip_prefix("verify examples for ")
                .or_else(|| {
                    w.message
                        .strip_prefix("verify examples for recursive function ")
                })
                .and_then(|rest| rest.split_whitespace().next())
                .map(|s| s.to_string())
        })
        .collect();

    let has_module_intent = items
        .iter()
        .any(|item| matches!(item, TopLevel::Module(m) if !m.intent.is_empty()));

    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    let mut functions = Vec::new();
    let mut justified_lines = 0usize;
    let mut partial_lines = 0usize;
    let mut unjustified_lines = 0usize;

    for (i, fd) in fns.iter().enumerate() {
        let fn_start = fd.line;
        let fn_end = if i + 1 < fns.len() {
            fns[i + 1].line.saturating_sub(1)
        } else {
            next_toplevel_line_after(items, fd.line).unwrap_or(total_lines)
        };
        let fn_lines = fn_end.saturating_sub(fn_start).max(1);

        let has_decision_impact = impact_symbols.contains(&fd.name)
            || impact_symbols
                .iter()
                .any(|s: &String| fd.name.starts_with(s.as_str()));

        let verify_cases = verify_cases_per_fn.get(&fd.name).copied().unwrap_or(0);
        let has_description = fd.desc.is_some();
        let is_effectful = !fd.effects.is_empty();
        let has_coverage_gaps = fns_with_coverage_gaps.contains(&fd.name);

        let level = FnDetail::compute_level(
            has_description,
            is_effectful,
            verify_cases,
            has_coverage_gaps,
            has_decision_impact,
        );
        let missing = FnDetail::compute_missing(
            has_description,
            is_effectful,
            verify_cases,
            has_coverage_gaps,
        );

        match level {
            Justification::Justified => justified_lines += fn_lines,
            Justification::Partial => partial_lines += fn_lines,
            Justification::Unjustified => unjustified_lines += fn_lines,
        }

        functions.push(FnDetail {
            name: fd.name.clone(),
            lines: fn_lines,
            has_description,
            is_effectful,
            verify_cases,
            has_coverage_gaps,
            has_decision_impact,
            level,
            missing,
        });
    }

    let non_fn_lines =
        total_lines.saturating_sub(justified_lines + partial_lines + unjustified_lines);
    if has_module_intent {
        justified_lines += non_fn_lines;
    } else {
        unjustified_lines += non_fn_lines;
    }

    let decision_summaries: Vec<DecisionSummary> = decisions
        .iter()
        .map(|d| {
            let reason_prefix: String = d.reason.chars().take(60).collect();
            let reason_prefix = if d.reason.len() > 60 {
                format!("{}...", reason_prefix.trim_end())
            } else {
                reason_prefix
            };
            DecisionSummary {
                name: d.name.clone(),
                date: d.date.clone(),
                reason_prefix,
            }
        })
        .collect();

    WhySummary {
        file_label: file_label.into(),
        total_lines,
        justified_lines,
        partial_lines,
        unjustified_lines,
        has_module_intent,
        decisions: decision_summaries,
        functions,
    }
}

fn next_toplevel_line_after(items: &[TopLevel], after_line: usize) -> Option<usize> {
    let mut min_line: Option<usize> = None;
    for item in items {
        let line = match item {
            TopLevel::FnDef(fd) => fd.line,
            TopLevel::Verify(v) => v.line,
            TopLevel::Decision(d) => d.line,
            TopLevel::TypeDef(_) | TopLevel::Module(_) | TopLevel::Stmt(_) => continue,
        };
        if line > after_line {
            min_line = Some(match min_line {
                Some(cur) if line < cur => line,
                Some(cur) => cur,
                None => line,
            });
        }
    }
    min_line.map(|l| l.saturating_sub(1))
}
