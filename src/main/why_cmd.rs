use std::process;

use aver::diagnostics::why::{FnDetail, Justification, WhySummary, summarize};
use colored::Colorize;

use super::commands::{display_check_path, resolve_av_inputs};
use super::shared::{parse_file, read_file, resolve_module_root};

pub(super) fn cmd_why(path: &str, module_root_override: Option<&str>, verbose: bool, json: bool) {
    let module_root = resolve_module_root(module_root_override);
    let inputs = match resolve_av_inputs(path) {
        Ok(inputs) => inputs,
        Err(e) => {
            eprintln!("{}", e.red());
            process::exit(1);
        }
    };

    // Aggregate totals across all analyzed files — displayed at the end.
    let mut total_lines = 0usize;
    let mut justified = 0usize;
    let mut partial = 0usize;
    let mut unjustified = 0usize;

    for file in &inputs {
        let shown_path = display_check_path(file, &module_root);
        match analyze_file(file, &shown_path) {
            Ok(summary) => {
                if json {
                    render_file_json(&summary);
                } else {
                    render_file(&summary, verbose);
                }
                total_lines += summary.total_lines;
                justified += summary.justified_lines;
                partial += summary.partial_lines;
                unjustified += summary.unjustified_lines;
            }
            Err(e) => {
                if json {
                    println!(
                        "{{\"schema_version\":1,\"kind\":\"file-error\",\"file\":\"{}\",\"error\":\"{}\"}}",
                        json_escape(&shown_path),
                        json_escape(&e)
                    );
                } else {
                    println!("{}", shown_path.red());
                    println!("  error: {}", e);
                    println!();
                }
            }
        }
    }

    if json {
        println!(
            "{{\"schema_version\":1,\"kind\":\"summary\",\"files\":{},\"lines\":{},\"justified\":{},\"partial\":{},\"unjustified\":{}}}",
            inputs.len(),
            total_lines,
            justified,
            partial,
            unjustified
        );
    } else {
        println!("{}", "─".repeat(50).dimmed());
        println!();
        println!(
            "{} {} files, {} lines",
            "Summary:".bold(),
            inputs.len(),
            total_lines
        );
        println!(
            "  {}    {} lines ({})",
            "justified".green(),
            justified,
            fmt_pct(justified, total_lines)
        );
        println!(
            "  {}      {} lines ({})",
            "partial".yellow(),
            partial,
            fmt_pct(partial, total_lines)
        );
        println!(
            "  {}  {} lines ({})",
            "unjustified".red(),
            unjustified,
            fmt_pct(unjustified, total_lines)
        );
        println!();
        println!(
            "{}",
            "Tip: add ? descriptions, verify blocks, and decision blocks to improve coverage."
                .dimmed()
        );
    }
}

fn analyze_file(path: &str, shown_path: &str) -> Result<WhySummary, String> {
    let source = read_file(path)?;
    let items = parse_file(&source)?;
    Ok(summarize(&items, &source, shown_path))
}

fn render_file(summary: &WhySummary, verbose: bool) {
    let just_raw = raw_pct(summary.justified_lines, summary.total_lines);

    let color_path = if just_raw >= 60 {
        summary.file_label.green()
    } else if just_raw >= 30 {
        summary.file_label.yellow()
    } else {
        summary.file_label.red()
    };
    println!("{}", color_path);

    println!(
        "  {} {} | {} {} | {} {}",
        fmt_pct(summary.justified_lines, summary.total_lines).green(),
        "justified".green(),
        fmt_pct(summary.partial_lines, summary.total_lines).yellow(),
        "partial".yellow(),
        fmt_pct(summary.unjustified_lines, summary.total_lines).red(),
        "unjustified".red(),
    );

    for d in &summary.decisions {
        println!(
            "  {} {} {}: {}",
            "decision".blue(),
            d.name,
            format!("({})", d.date).dimmed(),
            d.reason_prefix
        );
    }

    let mut problematic: Vec<&FnDetail> = summary
        .functions
        .iter()
        .filter(|f| f.level != Justification::Justified)
        .collect();
    problematic.sort_by(|a, b| {
        a.level
            .priority()
            .cmp(&b.level.priority())
            .then(b.lines.cmp(&a.lines))
    });

    let max_shown = if verbose { usize::MAX } else { 3 };
    for f in problematic.iter().take(max_shown) {
        let tag = match f.level {
            Justification::Unjustified => "unjustified:".red(),
            Justification::Partial => "partial:".yellow(),
            Justification::Justified => unreachable!(),
        };
        let hint = if f.missing.is_empty() {
            String::new()
        } else {
            format!(" ({})", f.missing.join(", "))
        };
        println!("  {} {}{}", tag, f.name, hint.dimmed());
    }
    if !verbose && problematic.len() > 3 {
        println!(
            "  {}",
            format!("...and {} more", problematic.len() - 3).dimmed()
        );
    }

    println!();
}

fn render_file_json(summary: &WhySummary) {
    let fns_json: Vec<String> = summary
        .functions
        .iter()
        .map(|f| {
            let level = match f.level {
                Justification::Justified => "justified",
                Justification::Partial => "partial",
                Justification::Unjustified => "unjustified",
            };
            let missing_json = f
                .missing
                .iter()
                .map(|m| format!("\"{}\"", json_escape(m)))
                .collect::<Vec<_>>()
                .join(",");
            format!(
                "{{\"name\":\"{}\",\"lines\":{},\"level\":\"{}\",\"description\":{},\"effectful\":{},\"verify_cases\":{},\"coverage_gaps\":{},\"decision_impact\":{},\"missing\":[{}]}}",
                json_escape(&f.name),
                f.lines,
                level,
                f.has_description,
                f.is_effectful,
                f.verify_cases,
                f.has_coverage_gaps,
                f.has_decision_impact,
                missing_json
            )
        })
        .collect();

    let decisions_json: Vec<String> = summary
        .decisions
        .iter()
        .map(|d| {
            format!(
                "{{\"name\":\"{}\",\"date\":\"{}\"}}",
                json_escape(&d.name),
                json_escape(&d.date)
            )
        })
        .collect();

    println!(
        "{{\"schema_version\":1,\"kind\":\"file\",\"file\":\"{}\",\"lines\":{},\"justified\":{},\"partial\":{},\"unjustified\":{},\"decisions\":[{}],\"functions\":[{}]}}",
        json_escape(&summary.file_label),
        summary.total_lines,
        summary.justified_lines,
        summary.partial_lines,
        summary.unjustified_lines,
        decisions_json.join(","),
        fns_json.join(",")
    );
}

fn json_escape(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
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
