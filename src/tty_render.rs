//! Terminal adapter for canonical `Diagnostic`.
//!
//! All color-aware (`colored`) rendering lives here — the canonical model in
//! `aver::diagnostics` stays runtime-neutral so the playground / LSP can reuse
//! it without pulling tty dependencies.
//!
//! Single entry point: [`render_tty`] — adaptive color rendering for terminals.
//! JSON emission goes through canonical serde
//! ([`aver::diagnostics::AnalysisReport::to_json`] for bundles,
//! `serde_json::to_string(&diagnostic)` for individual records).

use aver::diagnostics::{Diagnostic, Severity};
use colored::Colorize;
use std::fmt::Write;

/// Adaptive terminal renderer. Errors get full treatment (fields + source);
/// warnings stay compact unless `verbose` is set.
pub fn render_tty(d: &Diagnostic, verbose: bool) -> String {
    let mut out = String::new();

    // --- header ---
    let tag = match d.severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Fail => "fail",
        Severity::Hint => "hint",
    };
    let header_text = format!("{}[{}]: {}", tag, d.slug, d.summary);
    let header = match d.severity {
        Severity::Error | Severity::Fail => header_text.red().bold().to_string(),
        Severity::Warning => header_text.yellow().bold().to_string(),
        Severity::Hint => header_text.cyan().bold().to_string(),
    };
    let _ = writeln!(out, "{}", header);

    // --- at ---
    let at_label = "at:".blue().to_string();
    let _ = writeln!(
        out,
        "  {} {}:{}:{}",
        at_label, d.span.file, d.span.line, d.span.col
    );

    // --- in-fn ---
    if let Some(ref fn_name) = d.fn_name {
        let key = "in-fn:".blue().to_string();
        let _ = writeln!(out, "  {} {}", key, fn_name);
    }

    // --- intent (verbose only) ---
    if verbose && let Some(ref intent) = d.intent {
        let key = "intent:".blue().to_string();
        let _ = writeln!(out, "  {} {}", key, intent.dimmed());
    }

    let is_error = d.is_error();

    // --- conflict (errors) ---
    if is_error && let Some(ref conflict) = d.conflict {
        let key = "conflict:".blue().to_string();
        let _ = writeln!(out, "  {} {}", key, conflict);
    }

    // --- fields ---
    let field_limit = if verbose {
        d.fields.len()
    } else if is_error {
        4
    } else {
        2
    };
    for (key, value) in d.fields.iter().take(field_limit) {
        let colored_key = format!("{}:", key).blue().to_string();
        let _ = writeln!(out, "  {} {}", colored_key, value);
    }

    // --- repair.primary ---
    if let Some(ref repair) = d.repair.primary {
        let key = "repair:".blue().to_string();
        let _ = writeln!(out, "  {} {}", key, repair.cyan());
    }

    // --- repair.alternatives (verbose only) ---
    if verbose {
        for alt in &d.repair.alternatives {
            let key = "repair.alt:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, alt.cyan());
        }
    }

    // --- repair.example (verbose only) ---
    if verbose && let Some(ref example) = d.repair.example {
        let key = "repair.example:".blue().to_string();
        let _ = writeln!(out, "  {} {}", key, example.cyan());
    }

    // --- source snippet (multi-region) ---
    let skip_snippet = matches!(
        d.slug,
        "missing-verify" | "verify-effectful" | "missing-description"
    );
    let show_source = (is_error || verbose) && !skip_snippet;
    let has_source = d.regions.iter().any(|r| !r.source_lines.is_empty());
    if show_source && has_source {
        let max_num = d
            .regions
            .iter()
            .flat_map(|r| r.source_lines.iter().map(|sl| sl.line_num))
            .max()
            .unwrap_or(0);
        let gutter_width = format!("{}", max_num).len();
        let gutter_pad: String = " ".repeat(gutter_width);

        let _ = writeln!(out, "  {} {}", gutter_pad, "|".blue());

        let mut last_emitted: Option<usize> = None;

        for region in &d.regions {
            if let Some(first_sl) = region.source_lines.first()
                && let Some(last) = last_emitted
                && first_sl.line_num > last + 1
            {
                let _ = writeln!(out, "  {}", "...".blue());
            }

            for sl in &region.source_lines {
                if let Some(last) = last_emitted
                    && sl.line_num <= last
                {
                    continue;
                }
                let num_str = format!("{:>width$}", sl.line_num, width = gutter_width);
                let _ = writeln!(out, "  {} {} {}", num_str.dimmed(), "|".blue(), sl.text);
                last_emitted = Some(sl.line_num);
            }

            if let Some(ref ul) = region.underline {
                let pad: String = " ".repeat(ul.col.saturating_sub(1));
                let carets: String = "^".repeat(ul.len.max(1));
                let colored_carets = match d.severity {
                    Severity::Error | Severity::Fail => carets.red().to_string(),
                    Severity::Warning => carets.yellow().to_string(),
                    Severity::Hint => carets.cyan().to_string(),
                };
                let _ = writeln!(
                    out,
                    "  {} {} {}{}  {}",
                    gutter_pad,
                    "|".blue(),
                    pad,
                    colored_carets,
                    ul.label.dimmed()
                );
            }
        }
    }

    out
}
