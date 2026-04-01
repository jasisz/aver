use std::collections::{HashMap, HashSet};
use std::process;

use colored::Colorize;

use aver::ast::{DecisionBlock, FnDef, TopLevel};
use aver::checker::{collect_verify_coverage_warnings, merge_verify_blocks};

use super::commands::{display_check_path, resolve_av_inputs};
use super::shared::{parse_file, read_file, resolve_module_root};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

pub(super) fn cmd_why(path: &str, module_root_override: Option<&str>) {
    let module_root = resolve_module_root(module_root_override);
    let inputs = match resolve_av_inputs(path) {
        Ok(inputs) => inputs,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    let mut total_stats = FileStats::default();

    for file in &inputs {
        let shown_path = display_check_path(file, &module_root);
        match analyze_file(file) {
            Ok(stats) => {
                render_file(&shown_path, &stats);
                total_stats.total_lines += stats.total_lines;
                total_stats.justified_lines += stats.justified_lines;
                total_stats.partial_lines += stats.partial_lines;
                total_stats.unjustified_lines += stats.unjustified_lines;
                total_stats.decisions.extend(stats.decisions);
                total_stats.fn_details.extend(stats.fn_details);
            }
            Err(e) => {
                println!("{}", shown_path.red());
                println!("  error: {}", e);
                println!();
            }
        }
    }

    // Summary
    println!("{}", "─".repeat(50).dimmed());
    println!();
    println!(
        "{} {} files, {} lines",
        "Summary:".bold(),
        inputs.len(),
        total_stats.total_lines
    );
    println!(
        "  {}    {} lines ({})",
        "justified".green(),
        total_stats.justified_lines,
        fmt_pct(total_stats.justified_lines, total_stats.total_lines)
    );
    println!(
        "  {}      {} lines ({})",
        "partial".yellow(),
        total_stats.partial_lines,
        fmt_pct(total_stats.partial_lines, total_stats.total_lines)
    );
    println!(
        "  {}  {} lines ({})",
        "unjustified".red(),
        total_stats.unjustified_lines,
        fmt_pct(total_stats.unjustified_lines, total_stats.total_lines)
    );
    println!();
    println!(
        "{}",
        "Tip: add ? descriptions, verify blocks, and decision blocks to improve coverage.".dimmed()
    );
}

// ---------------------------------------------------------------------------
// Analysis
// ---------------------------------------------------------------------------

#[derive(Default, Clone)]
struct FnDetail {
    name: String,
    lines: usize,
    has_description: bool,
    verify_cases: usize,
    has_coverage_gaps: bool,
    has_decision_impact: bool,
}

#[derive(Clone, Copy, PartialEq)]
enum Justification {
    /// description + verify with no coverage gaps
    Justified,
    /// has something but incomplete (desc only, verify with gaps, decision only, etc.)
    Partial,
    /// nothing at all
    Unjustified,
}

impl FnDetail {
    fn justification(&self) -> Justification {
        let has_verify = self.verify_cases > 0;
        // Justified: description AND verify without coverage gaps
        if self.has_description && has_verify && !self.has_coverage_gaps {
            return Justification::Justified;
        }
        // Partial: has at least one signal
        if self.has_description || has_verify || self.has_decision_impact {
            return Justification::Partial;
        }
        Justification::Unjustified
    }
}

#[derive(Default)]
struct FileStats {
    total_lines: usize,
    justified_lines: usize,
    partial_lines: usize,
    unjustified_lines: usize,
    decisions: Vec<DecisionSummary>,
    fn_details: Vec<FnDetail>,
}

#[derive(Clone)]
struct DecisionSummary {
    name: String,
    date: String,
    reason_prefix: String,
}

fn analyze_file(path: &str) -> Result<FileStats, String> {
    let source = read_file(path)?;
    let items = parse_file(&source)?;

    let total_lines = source.lines().count();

    // Collect decisions
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

    // Verify: merge blocks, count cases per fn
    let merged_verify = merge_verify_blocks(&items);
    let mut verify_cases_per_fn: HashMap<String, usize> = HashMap::new();
    for vb in &merged_verify {
        *verify_cases_per_fn.entry(vb.fn_name.clone()).or_default() += vb.cases.len();
    }

    // Coverage gaps: run the existing checker, collect fn names with warnings
    let coverage_warnings = collect_verify_coverage_warnings(&items);
    let fns_with_coverage_gaps: HashSet<String> = coverage_warnings
        .iter()
        .filter_map(|w| {
            // Messages are "verify examples for {fn_name} ..."
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

    // Check module intent
    let has_module_intent = items
        .iter()
        .any(|item| matches!(item, TopLevel::Module(m) if !m.intent.is_empty()));

    // Analyze functions
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    let mut fn_details = Vec::new();
    let mut justified_lines = 0usize;
    let mut partial_lines = 0usize;
    let mut unjustified_lines = 0usize;

    for (i, fd) in fns.iter().enumerate() {
        let fn_start = fd.line;
        let fn_end = if i + 1 < fns.len() {
            fns[i + 1].line.saturating_sub(1)
        } else {
            next_toplevel_line_after(&items, fd.line).unwrap_or(total_lines)
        };
        let fn_lines = fn_end.saturating_sub(fn_start).max(1);

        let has_decision_impact = impact_symbols.contains(&fd.name)
            || impact_symbols
                .iter()
                .any(|s: &String| fd.name.starts_with(s.as_str()));

        let verify_cases = verify_cases_per_fn.get(&fd.name).copied().unwrap_or(0);

        let detail = FnDetail {
            name: fd.name.clone(),
            lines: fn_lines,
            has_description: fd.desc.is_some(),
            verify_cases,
            has_coverage_gaps: fns_with_coverage_gaps.contains(&fd.name),
            has_decision_impact,
        };

        match detail.justification() {
            Justification::Justified => justified_lines += fn_lines,
            Justification::Partial => partial_lines += fn_lines,
            Justification::Unjustified => unjustified_lines += fn_lines,
        }

        fn_details.push(detail);
    }

    // Non-function lines
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

    Ok(FileStats {
        total_lines,
        justified_lines,
        partial_lines,
        unjustified_lines,
        decisions: decision_summaries,
        fn_details,
    })
}

fn next_toplevel_line_after(items: &[TopLevel], after_line: usize) -> Option<usize> {
    let mut min_line = None;
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

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

fn render_file(shown_path: &str, stats: &FileStats) {
    let just_raw = raw_pct(stats.justified_lines, stats.total_lines);

    let color_path = if just_raw >= 60 {
        shown_path.green()
    } else if just_raw >= 30 {
        shown_path.yellow()
    } else {
        shown_path.red()
    };
    println!("{}", color_path);

    // One-line summary
    println!(
        "  {} {} | {} {} | {} {}",
        fmt_pct(stats.justified_lines, stats.total_lines).green(),
        "justified".green(),
        fmt_pct(stats.partial_lines, stats.total_lines).yellow(),
        "partial".yellow(),
        fmt_pct(stats.unjustified_lines, stats.total_lines).red(),
        "unjustified".red(),
    );

    for d in &stats.decisions {
        println!(
            "  {} {} {}: {}",
            "decision".blue(),
            d.name,
            format!("({})", d.date).dimmed(),
            d.reason_prefix
        );
    }

    // Show partial/unjustified functions (up to 3 worst)
    let mut problematic: Vec<&FnDetail> = stats
        .fn_details
        .iter()
        .filter(|f| f.justification() != Justification::Justified)
        .collect();
    problematic.sort_by(|a, b| {
        // Unjustified first, then partial; within each tier, largest first
        a.justification()
            .cmp_priority()
            .cmp(&b.justification().cmp_priority())
            .then(b.lines.cmp(&a.lines))
    });

    for f in problematic.iter().take(3) {
        let tag = match f.justification() {
            Justification::Unjustified => "unjustified:".red(),
            Justification::Partial => "partial:".yellow(),
            Justification::Justified => unreachable!(),
        };
        let mut hints = Vec::new();
        if !f.has_description {
            hints.push("no description");
        }
        if f.verify_cases == 0 {
            hints.push("no verify");
        } else if f.has_coverage_gaps {
            hints.push("verify has gaps");
        }
        let hint = if hints.is_empty() {
            String::new()
        } else {
            format!(" ({})", hints.join(", "))
        };
        println!("  {} {}{}", tag, f.name, hint.dimmed());
    }
    if problematic.len() > 3 {
        println!(
            "  {}",
            format!("...and {} more", problematic.len() - 3).dimmed()
        );
    }

    println!();
}

impl Justification {
    fn cmp_priority(self) -> u8 {
        match self {
            Justification::Unjustified => 0,
            Justification::Partial => 1,
            Justification::Justified => 2,
        }
    }
}

fn raw_pct(part: usize, total: usize) -> usize {
    if total == 0 {
        return 0;
    }
    (part * 100) / total
}

fn fmt_pct(part: usize, total: usize) -> String {
    if total == 0 || part == 0 {
        return "0%".to_string();
    }
    let pct = (part * 100) / total;
    if pct == 0 {
        "<1%".to_string()
    } else {
        format!("{}%", pct)
    }
}
