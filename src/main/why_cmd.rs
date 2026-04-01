use std::process;

use colored::Colorize;

use aver::ast::{DecisionBlock, FnDef, TopLevel};

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
    let just_pct = pct(total_stats.justified_lines, total_stats.total_lines);
    let part_pct = pct(total_stats.partial_lines, total_stats.total_lines);
    let unjust_pct = pct(total_stats.unjustified_lines, total_stats.total_lines);
    println!(
        "  {}    {} lines ({}%)",
        "justified".green(),
        total_stats.justified_lines,
        just_pct
    );
    println!(
        "  {}      {} lines ({}%)",
        "partial".yellow(),
        total_stats.partial_lines,
        part_pct
    );
    println!(
        "  {}  {} lines ({}%)",
        "unjustified".red(),
        total_stats.unjustified_lines,
        unjust_pct
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

#[derive(Default)]
struct FnDetail {
    name: String,
    line: usize,
    lines: usize,
    has_description: bool,
    has_verify: bool,
    has_decision_impact: bool,
}

impl FnDetail {
    fn justification_level(&self) -> Justification {
        let score =
            self.has_description as u8 + self.has_verify as u8 + self.has_decision_impact as u8;
        match score {
            0 => Justification::Unjustified,
            1 => Justification::Partial,
            _ => Justification::Justified,
        }
    }
}

#[derive(Clone, Copy)]
enum Justification {
    Justified,
    Partial,
    Unjustified,
}

#[derive(Default)]
struct FileStats {
    total_lines: usize,
    justified_lines: usize,
    partial_lines: usize,
    unjustified_lines: usize,
    _has_module_intent: bool,
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

    // Collect decision impact symbols for cross-referencing
    let impact_symbols: std::collections::HashSet<String> = decisions
        .iter()
        .flat_map(|d| d.impacts.iter().map(|i| i.node.text().to_string()))
        .collect();

    // Collect verify blocks by function name
    let verified_fns: std::collections::HashSet<String> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Verify(v) => Some(v.fn_name.clone()),
            _ => None,
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

    // Estimate lines per function: from fn line to next fn/toplevel line (or EOF)
    for (i, fd) in fns.iter().enumerate() {
        let fn_start = fd.line;
        let fn_end = if i + 1 < fns.len() {
            fns[i + 1].line.saturating_sub(1)
        } else {
            // Last fn — estimate: look for next non-fn toplevel or EOF
            next_toplevel_line_after(&items, fd.line).unwrap_or(total_lines)
        };
        let fn_lines = fn_end.saturating_sub(fn_start).max(1);

        let has_decision_impact = impact_symbols.contains(&fd.name)
            || impact_symbols
                .iter()
                .any(|s: &String| fd.name.starts_with(s.as_str()));

        let detail = FnDetail {
            name: fd.name.clone(),
            line: fd.line,
            lines: fn_lines,
            has_description: fd.desc.is_some(),
            has_verify: verified_fns.contains(&fd.name),
            has_decision_impact,
        };

        match detail.justification_level() {
            Justification::Justified => justified_lines += fn_lines,
            Justification::Partial => partial_lines += fn_lines,
            Justification::Unjustified => unjustified_lines += fn_lines,
        }

        fn_details.push(detail);
    }

    // Non-function lines (module, type defs, decisions, verify blocks, etc.)
    // Count them as justified if module has intent, partial otherwise
    let non_fn_lines =
        total_lines.saturating_sub(justified_lines + partial_lines + unjustified_lines);
    if has_module_intent {
        justified_lines += non_fn_lines;
    } else {
        partial_lines += non_fn_lines;
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
        _has_module_intent: has_module_intent,
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
            TopLevel::TypeDef(_) => continue,
            TopLevel::Module(_) => continue,
            TopLevel::Stmt(_) => continue,
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
    let just_pct = pct(stats.justified_lines, stats.total_lines);

    let color_path = if just_pct >= 60 {
        shown_path.green()
    } else if just_pct >= 30 {
        shown_path.yellow()
    } else {
        shown_path.red()
    };
    println!("{}", color_path);
    println!(
        "  {} {}/{} lines ({}%)",
        "justified:".bold(),
        stats.justified_lines,
        stats.total_lines,
        just_pct
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

    // Count verify/description coverage
    let verify_count = stats.fn_details.iter().filter(|f| f.has_verify).count();
    let desc_count = stats
        .fn_details
        .iter()
        .filter(|f| f.has_description)
        .count();
    let total_fns = stats.fn_details.len();
    if total_fns > 0 {
        println!(
            "  {}",
            format!(
                "{} verify block(s), {} description(s), {} function(s)",
                verify_count, desc_count, total_fns
            )
            .dimmed()
        );
    }

    // Show worst unjustified functions (up to 3)
    let mut unjustified: Vec<&FnDetail> = stats
        .fn_details
        .iter()
        .filter(|f| matches!(f.justification_level(), Justification::Unjustified))
        .collect();
    unjustified.sort_by(|a, b| b.lines.cmp(&a.lines));

    if !unjustified.is_empty() {
        let shown = unjustified.iter().take(3);
        for f in shown {
            println!(
                "  {} {} ({} lines, line {})",
                "unjustified:".red(),
                f.name,
                f.lines,
                f.line
            );
        }
        if unjustified.len() > 3 {
            println!(
                "  {}",
                format!(
                    "...and {} more unjustified function(s)",
                    unjustified.len() - 3
                )
                .dimmed()
            );
        }
    }

    println!();
}

fn pct(part: usize, total: usize) -> usize {
    if total == 0 {
        return 0;
    }
    (part * 100) / total
}
