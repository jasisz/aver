/// Structured diagnostic model and adaptive renderer for Aver.
///
/// One canonical model (`Diagnostic`) — always rich.
/// Adaptive rendering: errors get full treatment, warnings are compact.
///
/// Field order: severity, slug, summary, at, in-fn, intent,
///   contract.*, observed.*, conflict, repair.*, verify.*, source
use aver::types::checker::TypeError;
use colored::Colorize;
use std::fmt::Write;

// ---------------------------------------------------------------------------
// Core types
// ---------------------------------------------------------------------------

/// Severity level.
pub(super) enum Severity {
    Error,
    Warning,
    Fail,
}

/// Source location.
pub(super) struct Span {
    pub file: String,
    pub line: usize,
    pub col: usize,
}

/// A single source line for display.
pub(super) struct SourceLine {
    pub line_num: usize,
    pub text: String,
}

/// Underline annotation beneath a source line.
pub(super) struct Underline {
    pub col: usize,
    pub len: usize,
    pub label: String,
}

/// A contiguous group of source lines with an optional underline annotation.
pub(super) struct AnnotatedRegion {
    pub source_lines: Vec<SourceLine>,
    pub underline: Option<Underline>,
}

impl AnnotatedRegion {
    /// Convenience: wrap a single source block + underline into a Vec of one region.
    pub(super) fn single(source_lines: Vec<SourceLine>, underline: Option<Underline>) -> Vec<Self> {
        vec![Self {
            source_lines,
            underline,
        }]
    }
}

/// The canonical diagnostic record.
pub(super) struct Diagnostic {
    pub severity: Severity,
    pub slug: &'static str,
    pub summary: String,
    pub span: Span,
    pub fn_name: Option<String>,
    pub intent: Option<String>,
    pub fields: Vec<(&'static str, String)>,
    pub conflict: Option<String>,
    pub repair_primary: Option<String>,
    pub repair_alternatives: Vec<String>,
    pub repair_example: Option<String>,
    pub regions: Vec<AnnotatedRegion>,
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

impl Diagnostic {
    pub fn is_warning(&self) -> bool {
        matches!(self.severity, Severity::Warning)
    }

    /// Render for terminal output.
    ///
    /// Errors get full treatment (fields + source).
    /// Warnings get compact treatment (fewer fields, source only if verbose).
    pub fn render(&self, verbose: bool) -> String {
        let mut out = String::new();

        // --- header ---
        let tag = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Fail => "fail",
        };
        let header_text = format!("{}[{}]: {}", tag, self.slug, self.summary);
        let header = match self.severity {
            Severity::Error | Severity::Fail => header_text.red().bold().to_string(),
            Severity::Warning => header_text.yellow().bold().to_string(),
        };
        let _ = writeln!(out, "{}", header);

        // --- at ---
        let at_label = "at:".blue().to_string();
        let _ = writeln!(
            out,
            "  {} {}:{}:{}",
            at_label, self.span.file, self.span.line, self.span.col
        );

        // --- in-fn ---
        if let Some(ref fn_name) = self.fn_name {
            let key = "in-fn:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, fn_name);
        }

        // --- intent (verbose only, or error+verbose) ---
        if verbose && let Some(ref intent) = self.intent {
            let key = "intent:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, intent.dimmed());
        }

        let is_error = matches!(self.severity, Severity::Error | Severity::Fail);

        // --- conflict (errors) ---
        if is_error && let Some(ref conflict) = self.conflict {
            let key = "conflict:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, conflict);
        }

        // --- fields ---
        let field_limit = if verbose {
            self.fields.len() // all
        } else if is_error {
            4
        } else {
            2
        };
        for (key, value) in self.fields.iter().take(field_limit) {
            let colored_key = format!("{}:", key).blue().to_string();
            let _ = writeln!(out, "  {} {}", colored_key, value);
        }

        // --- repair.primary ---
        if let Some(ref repair) = self.repair_primary {
            let key = "repair:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, repair.cyan());
        }

        // --- repair.alternatives (verbose only) ---
        if verbose {
            for alt in &self.repair_alternatives {
                let key = "repair.alt:".blue().to_string();
                let _ = writeln!(out, "  {} {}", key, alt.cyan());
            }
        }

        // --- repair.example (verbose only) ---
        if verbose && let Some(ref example) = self.repair_example {
            let key = "repair.example:".blue().to_string();
            let _ = writeln!(out, "  {} {}", key, example.cyan());
        }

        // --- source snippet (multi-region) ---
        // Errors: always show. Warnings: only if verbose.
        // Skip snippet for structural hints where the signature adds no information
        // beyond what in-fn: and repair: already say.
        let skip_snippet = matches!(
            self.slug,
            "missing-verify" | "verify-effectful" | "missing-description"
        );
        let show_source = (is_error || verbose) && !skip_snippet;
        let has_source = self.regions.iter().any(|r| !r.source_lines.is_empty());
        if show_source && has_source {
            let max_num = self
                .regions
                .iter()
                .flat_map(|r| r.source_lines.iter().map(|sl| sl.line_num))
                .max()
                .unwrap_or(0);
            let gutter_width = format!("{}", max_num).len();
            let gutter_pad: String = " ".repeat(gutter_width);

            // initial empty gutter
            let _ = writeln!(out, "  {} {}", gutter_pad, "|".blue());

            let mut last_emitted: Option<usize> = None;

            for region in &self.regions {
                // Insert `...` separator between non-contiguous regions.
                // Small gaps (<=2 lines) are pre-filled by `fill_small_region_gaps`.
                if let Some(first_sl) = region.source_lines.first()
                    && let Some(last) = last_emitted
                    && first_sl.line_num > last + 1
                {
                    let _ = writeln!(out, "  {}", "...".blue());
                }

                // Emit source lines, skipping already-emitted ones
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

                // Underline for this region
                if let Some(ref ul) = region.underline {
                    let pad: String = " ".repeat(ul.col.saturating_sub(1));
                    let carets: String = "^".repeat(ul.len.max(1));
                    let colored_carets = match self.severity {
                        Severity::Error | Severity::Fail => carets.red().to_string(),
                        Severity::Warning => carets.yellow().to_string(),
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

    /// Render as JSON for tooling.
    pub fn render_json(&self) -> String {
        let severity_str = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Fail => "fail",
        };
        let mut parts: Vec<String> = Vec::new();
        parts.push("\"schema_version\":1".to_string());
        parts.push("\"kind\":\"diagnostic\"".to_string());
        parts.push(format!("\"severity\":\"{}\"", severity_str));
        parts.push(format!("\"slug\":\"{}\"", self.slug));
        parts.push(format!("\"summary\":{}", json_escape(&self.summary)));
        parts.push(format!("\"file\":{}", json_escape(&self.span.file)));
        parts.push(format!("\"line\":{}", self.span.line));
        parts.push(format!("\"col\":{}", self.span.col));
        if let Some(ref fn_name) = self.fn_name {
            parts.push(format!("\"fn\":{}", json_escape(fn_name)));
        }
        if let Some(ref intent) = self.intent {
            parts.push(format!("\"intent\":{}", json_escape(intent)));
        }
        if !self.fields.is_empty() {
            let field_strs: Vec<String> = self
                .fields
                .iter()
                .map(|(k, v)| format!("{}:{}", json_escape(k), json_escape(v)))
                .collect();
            parts.push(format!("\"fields\":{{{}}}", field_strs.join(",")));
        }
        if let Some(ref conflict) = self.conflict {
            parts.push(format!("\"conflict\":{}", json_escape(conflict)));
        }
        if let Some(ref repair) = self.repair_primary {
            parts.push(format!("\"repair\":{}", json_escape(repair)));
        }
        format!("{{{}}}", parts.join(","))
    }
}

/// Minimal JSON string escaping (no external deps).
pub(super) fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

// ---------------------------------------------------------------------------
// Source line extraction
// ---------------------------------------------------------------------------

/// Extract source lines around `line` (1-based) with `context` lines of
/// surrounding context on each side.
fn extract_source_lines(source: &str, line: usize, context: usize) -> Vec<SourceLine> {
    let lines: Vec<&str> = source.lines().collect();
    let start = line.saturating_sub(context + 1); // 0-based
    let end = (line + context).min(lines.len()); // exclusive, 0-based
    (start..end)
        .map(|i| SourceLine {
            line_num: i + 1,
            text: lines[i].to_string(),
        })
        .collect()
}

/// Extract the declared return type from a "declared return type is X" message.
fn extract_return_type(msg: &str) -> &str {
    msg.rsplit("declared return type is ").next().unwrap_or("?")
}

/// Estimate how many characters to underline starting at `col` (1-based).
fn estimate_span_len(line: &str, col: usize) -> usize {
    let start = col.saturating_sub(1);
    if start >= line.len() {
        return 1;
    }
    let rest = &line[start..];
    let len = rest
        .chars()
        .take_while(|c| !c.is_whitespace() && !matches!(c, '(' | ')' | '[' | ']' | ',' | ':'))
        .count();
    if len == 0 { 1 } else { len }
}

/// Extract source lines for an inclusive range [from..to] (1-based).
fn extract_source_lines_range(source: &str, from: usize, to: usize) -> Vec<SourceLine> {
    let lines: Vec<&str> = source.lines().collect();
    let start = from.saturating_sub(1); // 0-based
    let end = to.min(lines.len()); // inclusive, but capped
    (start..end)
        .map(|i| SourceLine {
            line_num: i + 1,
            text: lines[i].to_string(),
        })
        .collect()
}

/// Fill small gaps (<=2 lines) between sorted regions with bridging source lines.
/// Avoids a `...` separator for tiny jumps.
fn fill_small_region_gaps(regions: &mut Vec<AnnotatedRegion>, source: &str) {
    if regions.len() < 2 {
        return;
    }
    let lines: Vec<&str> = source.lines().collect();
    let mut i = 0;
    while i + 1 < regions.len() {
        let last_of_prev = regions[i]
            .source_lines
            .last()
            .map(|sl| sl.line_num)
            .unwrap_or(0);
        let first_of_next = regions[i + 1]
            .source_lines
            .first()
            .map(|sl| sl.line_num)
            .unwrap_or(0);
        if first_of_next > last_of_prev + 1 && first_of_next <= last_of_prev + 3 {
            // Gap of 1-2 lines: insert a bridge region with no underline.
            let bridge: Vec<SourceLine> = ((last_of_prev + 1)..first_of_next)
                .filter_map(|ln| {
                    lines.get(ln.saturating_sub(1)).map(|t| SourceLine {
                        line_num: ln,
                        text: t.to_string(),
                    })
                })
                .collect();
            if !bridge.is_empty() {
                regions.insert(
                    i + 1,
                    AnnotatedRegion {
                        source_lines: bridge,
                        underline: None,
                    },
                );
                i += 1; // skip the bridge we just inserted
            }
        }
        i += 1;
    }
}

/// Find the line number of the block header (`fn`, `verify`, `decision`) for `name`,
/// searching forward from the start up to (but not including) `before_line`.
/// Tries `fn <name>`, `verify <name>`, `decision <name>`.
fn find_block_header_line(source: &str, name: &str, before_line: usize) -> Option<usize> {
    let needles = [
        format!("fn {}", name),
        format!("verify {}", name),
        format!("decision {}", name),
    ];
    let mut best: Option<usize> = None;
    for (i, line) in source.lines().enumerate() {
        let line_num = i + 1;
        if line_num >= before_line {
            break;
        }
        let trimmed = line.trim_start();
        for needle in &needles {
            if trimmed.starts_with(needle.as_str()) {
                best = Some(line_num); // keep latest match (closest to finding)
            }
        }
    }
    best
}

/// Find where the block preamble ends.
/// For fn: declaration + `?` desc + `!` effects.
/// For decision: declaration + indented key = value lines.
/// Returns the last preamble line number (1-based), capped before `before_line`.
fn find_preamble_end(source: &str, header_line: usize, before_line: usize) -> usize {
    let mut end = header_line;
    for (i, line) in source.lines().enumerate() {
        let line_num = i + 1;
        if line_num <= header_line {
            continue;
        }
        if line_num >= before_line {
            break;
        }
        let trimmed = line.trim_start();
        // Preamble lines: ? "desc", ! [effects], continuation strings,
        // or indented key=value (decision fields like date =, reason =, etc.)
        if trimmed.starts_with('?')
            || trimmed.starts_with('!')
            || trimmed.starts_with('"')
            || trimmed.starts_with('[')
            || trimmed.is_empty()
            || (line.starts_with("    ") && trimmed.contains(" = "))
        {
            end = line_num;
        } else {
            break;
        }
    }
    end
}

/// Try to find a precise (col, len) span by extracting the first quoted
/// expression from `summary` and locating it in `source_line`.
/// Tries backticks first, then single quotes. Returns 1-based col.
/// If the summary mentions "right side" or `=>`, searches after `=>` in the source line.
fn find_precise_span(source_line: &str, summary: &str) -> Option<(usize, usize)> {
    let search_after_arrow = summary.contains("right side") || summary.contains("=>");
    for quote in ['`', '\''] {
        if let Some(start_offset) = summary.find(quote) {
            let start = start_offset + 1;
            if let Some(end_offset) = summary[start..].find(quote) {
                let needle = &summary[start..start + end_offset];
                if !needle.is_empty() {
                    let search_region = if search_after_arrow {
                        // Search only after `=>` to find the right-hand side
                        source_line
                            .find("=>")
                            .map(|arrow_pos| arrow_pos + 2)
                            .unwrap_or(0)
                    } else {
                        0
                    };
                    if let Some(pos) = source_line[search_region..].find(needle) {
                        return Some((search_region + pos + 1, needle.len()));
                    }
                }
            }
        }
    }
    None
}

// ---------------------------------------------------------------------------
// Factory functions — convert existing error types to Diagnostic
// ---------------------------------------------------------------------------

/// Build a `Diagnostic` from a `TypeError` (from the typechecker).
pub(super) fn from_type_error(te: &TypeError, source: &str, file: &str) -> Diagnostic {
    let msg = &te.message;
    let line = te.line;
    let col = te.col;

    // Try to extract structured information from the message.
    let (slug, conflict, fields, repair) = classify_type_error(msg);

    let source_line_text = source
        .lines()
        .nth(line.saturating_sub(1))
        .unwrap_or_default();
    let (ul_col, ul_len) = if col > 0 {
        (col, estimate_span_len(source_line_text, col))
    } else {
        // col=0: underline the trimmed content of the target line
        let indent = source_line_text.len() - source_line_text.trim_start().len();
        (indent + 1, source_line_text.trim().len())
    };

    // When we have a secondary span (e.g. return type mismatch), narrow the
    // primary underline to just the declared return type after `->`.
    let (primary_col, primary_len, primary_label) = if te.secondary.is_some() {
        if let Some(arrow_pos) = source_line_text.find("-> ") {
            let after_arrow = &source_line_text[arrow_pos + 3..];
            let ret_type_len = after_arrow
                .chars()
                .take_while(|c| {
                    c.is_alphanumeric()
                        || *c == '<'
                        || *c == '>'
                        || *c == ','
                        || *c == ' '
                        || *c == '.'
                })
                .count();
            let ret_type_len = after_arrow[..ret_type_len].trim_end().len();
            (
                arrow_pos + 4,
                ret_type_len.max(1),
                format!("declared {}", extract_return_type(msg)),
            )
        } else {
            (ul_col, ul_len, String::new())
        }
    } else {
        (ul_col, ul_len, String::new())
    };

    let primary_underline = if primary_len > 0 {
        Some(Underline {
            col: primary_col,
            len: primary_len,
            label: primary_label,
        })
    } else {
        None
    };

    let mut regions = vec![AnnotatedRegion {
        source_lines: extract_source_lines(source, line, 0),
        underline: primary_underline,
    }];

    // Add secondary span region if present (e.g. return type mismatch body line).
    if let Some(ref sec) = te.secondary
        && sec.line != line
    {
        let sec_source_text = source
            .lines()
            .nth(sec.line.saturating_sub(1))
            .unwrap_or_default();
        let (sec_col, sec_len) = if sec.col > 0 {
            (sec.col, estimate_span_len(sec_source_text, sec.col))
        } else {
            let indent = sec_source_text.len() - sec_source_text.trim_start().len();
            (indent + 1, sec_source_text.trim().len())
        };
        regions.push(AnnotatedRegion {
            source_lines: extract_source_lines(source, sec.line, 0),
            underline: Some(Underline {
                col: sec_col,
                len: sec_len,
                label: sec.label.clone(),
            }),
        });
    }

    regions.sort_by_key(|r| r.source_lines.first().map(|sl| sl.line_num).unwrap_or(0));
    fill_small_region_gaps(&mut regions, source);

    Diagnostic {
        severity: Severity::Error,
        slug,
        summary: msg.to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col: ul_col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict,
        repair_primary: repair,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions,
    }
}

/// Build a `Diagnostic` for an unused binding warning.
pub(super) fn unused_binding_diagnostic(
    binding: &str,
    fn_name: &str,
    line: usize,
    source: &str,
    file: &str,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Warning,
        slug: "unused-binding",
        summary: format!("Unused binding '{}' in function '{}'", binding, fn_name),
        span: Span {
            file: file.to_string(),
            line,
            col: 0,
        },
        fn_name: Some(fn_name.to_string()),
        intent: None,
        fields: vec![("binding", binding.to_string())],
        conflict: None,
        repair_primary: Some(format!("Remove the binding or prefix with _: _{}", binding)),
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(extract_source_lines(source, line, 0), None),
    }
}

/// Build a `Diagnostic` for a missing verify warning.
pub(super) fn missing_verify_diagnostic(
    fn_name: &str,
    line: usize,
    _intent: Option<&str>,
    source: &str,
    file: &str,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Warning,
        slug: "missing-verify",
        summary: format!("Function '{}' has no verify block", fn_name),
        span: Span {
            file: file.to_string(),
            line,
            col: 0,
        },
        fn_name: Some(fn_name.to_string()),
        intent: _intent.map(|s| s.to_string()),
        fields: Vec::new(),
        conflict: None,
        repair_primary: Some(format!("Add a verify block: verify {}:", fn_name)),
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(extract_source_lines(source, line, 0), None),
    }
}

/// Build a `Diagnostic` for an effect violation. (Stub for V1.)
pub(super) fn effect_violation_diagnostic(
    msg: &str,
    line: usize,
    col: usize,
    source: &str,
    file: &str,
) -> Diagnostic {
    Diagnostic {
        severity: Severity::Error,
        slug: "effect-violation",
        summary: msg.to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields: Vec::new(),
        conflict: Some(msg.to_string()),
        repair_primary: Some("Declare missing effects with ! [Effect] on the function".to_string()),
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(
            extract_source_lines(source, line, 1),
            if col > 0 {
                let source_line_text = source
                    .lines()
                    .nth(line.saturating_sub(1))
                    .unwrap_or_default();
                Some(Underline {
                    col,
                    len: estimate_span_len(source_line_text, col),
                    label: String::new(),
                })
            } else {
                None
            },
        ),
    }
}

/// Build a `Diagnostic` from a `CheckFinding` (intent/verify/coverage warning or error).
pub(super) fn from_check_finding(
    severity: Severity,
    finding: &aver::checker::CheckFinding,
    source: &str,
    file: &str,
) -> Diagnostic {
    let (slug, repair) = classify_finding(&finding.message);
    // Use structured fn_name if available, otherwise try to extract from message text
    let fn_name = finding
        .fn_name
        .clone()
        .or_else(|| extract_fn_name_from_finding(&finding.message));
    // For classified findings, strip the repair part from the summary
    let summary = if repair.is_some() {
        finding
            .message
            .split_once(" — ")
            .or_else(|| finding.message.split_once(" -- "))
            .map(|(s, _)| s.to_string())
            .unwrap_or_else(|| finding.message.clone())
    } else {
        finding.message.clone()
    };
    // Compute underline from the source line at finding.line.
    // Try to find the backtick-quoted expression in the source line for precise underline.
    let source_line_text = source
        .lines()
        .nth(finding.line.saturating_sub(1))
        .unwrap_or_default();
    let (col, span_len) = find_precise_span(source_line_text, &summary).unwrap_or_else(|| {
        let indent = source_line_text.len() - source_line_text.trim_start().len();
        (indent + 1, source_line_text.trim().len())
    });

    // Build regions: primary + extra_spans + optional fn header context
    let primary_underline = if span_len > 0 {
        Some(Underline {
            col,
            len: span_len,
            label: String::new(),
        })
    } else {
        None
    };
    let mut regions = vec![AnnotatedRegion {
        source_lines: extract_source_lines(source, finding.line, 0),
        underline: primary_underline,
    }];

    // Extra spans → extra regions
    for extra in &finding.extra_spans {
        let extra_source_line = source
            .lines()
            .nth(extra.line.saturating_sub(1))
            .unwrap_or_default();
        let (extra_col, extra_len) = if extra.col > 0 && extra.len > 0 {
            (extra.col, extra.len)
        } else {
            // Auto-compute from the source line
            find_precise_span(extra_source_line, &extra.label).unwrap_or_else(|| {
                let indent = extra_source_line.len() - extra_source_line.trim_start().len();
                (indent + 1, extra_source_line.trim().len())
            })
        };
        regions.push(AnnotatedRegion {
            source_lines: extract_source_lines(source, extra.line, 0),
            underline: Some(Underline {
                col: extra_col,
                len: extra_len,
                label: extra.label.clone(),
            }),
        });
    }

    // Block header context: show preamble (declaration + ? desc + ! effects),
    // not the entire body. Cap at 4 lines to avoid noise from long intents.
    // Skip if the finding itself IS the block header (verify/decision/fn line).
    let finding_is_header = source_line_text.trim_start().starts_with("fn ")
        || source_line_text.trim_start().starts_with("verify ")
        || source_line_text.trim_start().starts_with("decision ");
    if !finding_is_header
        && let Some(ref name) = fn_name
        && let Some(header_line) = find_block_header_line(source, name, finding.line)
        && header_line < finding.line
    {
        let fn_line = header_line;
        let preamble_end = find_preamble_end(source, fn_line, finding.line);
        let capped_end = preamble_end.min(fn_line + 3); // max 4 lines
        let header_lines = extract_source_lines_range(source, fn_line, capped_end);
        if !header_lines.is_empty() {
            regions.insert(
                0,
                AnnotatedRegion {
                    source_lines: header_lines,
                    underline: None,
                },
            );
        }
    }

    // Sort regions by first line number, then fill small gaps
    regions.sort_by_key(|r| r.source_lines.first().map(|sl| sl.line_num).unwrap_or(0));
    fill_small_region_gaps(&mut regions, source);

    Diagnostic {
        severity,
        slug,
        summary,
        span: Span {
            file: file.to_string(),
            line: finding.line,
            col,
        },
        fn_name,
        intent: None,
        fields: Vec::new(),
        conflict: None,
        repair_primary: repair,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions,
    }
}

fn classify_finding(msg: &str) -> (&'static str, Option<String>) {
    if msg.contains("has effects") && msg.contains("verify blocks are for pure") {
        (
            "verify-effectful",
            Some(
                "Remove verify block; test via `aver run --record` + `aver replay --test`"
                    .to_string(),
            ),
        )
    } else if msg.contains("no verify block") {
        (
            "missing-verify",
            Some("Add a verify block with representative test cases".to_string()),
        )
    } else if msg.contains("no description") {
        (
            "missing-description",
            Some("Add a ? \"description\" line after the function signature".to_string()),
        )
    } else if msg.contains("non-tail recursion") {
        (
            "non-tail-recursion",
            Some("Convert to accumulator style for tail-call optimization".to_string()),
        )
    } else if msg.contains("unused expose") || msg.contains("not used by") {
        ("unused-expose", None)
    } else if msg.contains("verify coverage") || msg.contains("verify case") {
        ("verify-coverage", None)
    } else if msg.contains("verify law") {
        ("verify-law", None)
    } else if msg.contains("List.len") && msg.contains("traverses the entire list") {
        ("perf-list-len", split_repair(msg))
    } else if msg.contains("string concatenation") && msg.contains("recursive call") {
        ("perf-string-concat", split_repair(msg))
    } else if msg.contains("nested `match") {
        ("perf-nested-match", split_repair(msg))
    } else if msg.contains("recomputed every recursive call") {
        ("perf-loop-invariant", split_repair(msg))
    } else if msg.contains("computed in both the match condition") {
        ("cse-match", split_repair(msg))
    } else if msg.contains("computed") && msg.contains("times in this function") {
        ("cse-duplicate", split_repair(msg))
    } else if msg.contains("unused effect") {
        (
            "unused-effect",
            Some("Remove unused effects from the ! [...] declaration".to_string()),
        )
    } else if msg.contains("unknown impact symbol") {
        ("unknown-impact", split_repair(msg))
    } else if msg.contains("must not call") && msg.contains("on the right side") {
        ("verify-rhs", None)
    } else if msg.contains("consider granular") {
        ("effect-granularity", split_repair(msg))
    } else if msg.contains("verify examples") || msg.contains("verify case") {
        ("verify-coverage", None)
    } else {
        ("check", None)
    }
}

/// Split a message on ` — ` (em-dash) to extract the repair suggestion.
fn split_repair(msg: &str) -> Option<String> {
    msg.split_once(" — ")
        .or_else(|| msg.split_once(" -- "))
        .map(|(_, repair)| {
            let mut r = repair.to_string();
            // Capitalize first letter
            if let Some(first) = r.get_mut(0..1) {
                first.make_ascii_uppercase();
            }
            r
        })
}

fn extract_fn_name_from_finding(msg: &str) -> Option<String> {
    // "Function 'foo' has no verify block" → "foo"
    if let Some(start) = msg.find('\'')
        && let Some(end) = msg[start + 1..].find('\'')
    {
        return Some(msg[start + 1..start + 1 + end].to_string());
    }
    None
}

// ---------------------------------------------------------------------------
// Internal: classify a type error message into slug/conflict/fields/repair
// ---------------------------------------------------------------------------

/// (slug, conflict, fields, repair)
type Classification = (
    &'static str,
    Option<String>,
    Vec<(&'static str, String)>,
    Option<String>,
);

fn classify_type_error(msg: &str) -> Classification {
    // Type mismatch pattern: "Type mismatch: expected X, got Y"
    if let Some(rest) = msg.strip_prefix("Type mismatch:") {
        let rest = rest.trim();
        let mut fields = Vec::new();
        let mut expected = String::new();
        let mut got = String::new();
        if let Some((exp, g)) = rest.split_once(", got ") {
            expected = exp
                .strip_prefix("expected ")
                .unwrap_or(exp)
                .trim()
                .to_string();
            got = g.trim().to_string();
            fields.push(("contract.expected", expected.clone()));
            fields.push(("observed.actual", got.clone()));
        }
        let repair = if !expected.is_empty() && !got.is_empty() {
            Some(format!("Change the expression to produce {}", expected))
        } else {
            None
        };
        return ("type-mismatch", Some(msg.to_string()), fields, repair);
    }

    // Unknown identifier
    if msg.starts_with("Unknown identifier") || msg.starts_with("Unknown function") {
        return (
            "unknown-ident",
            None,
            Vec::new(),
            Some("Check the spelling or add the missing import".to_string()),
        );
    }

    // Arity mismatch
    if msg.contains("expects") && msg.contains("argument") {
        return (
            "arity-mismatch",
            Some(msg.to_string()),
            Vec::new(),
            Some("Adjust the number of arguments".to_string()),
        );
    }

    // Effect violation
    if msg.contains("effect") && (msg.contains("not declared") || msg.contains("not allowed")) {
        return (
            "effect-violation",
            Some(msg.to_string()),
            Vec::new(),
            Some("Add the missing effect to the function's ! [...] declaration".to_string()),
        );
    }

    // Fallback
    ("type-error", None, Vec::new(), None)
}

// -- Verify failure diagnostics -----------------------------------------------

#[allow(clippy::too_many_arguments)]
pub(super) fn verify_mismatch_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    expected: &str,
    actual: &str,
    line: usize,
    col: usize,
    is_law: bool,
    law_context: Option<&aver::checker::VerifyLawContext>,
) -> Diagnostic {
    let summary = if is_law {
        "law violated"
    } else {
        "assertion failed"
    };
    let mut fields: Vec<(&'static str, String)> = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("expected", expected.to_string()),
        ("actual", actual.to_string()),
    ];
    if let Some(lctx) = law_context {
        for (name, val) in &lctx.givens {
            fields.push(("given", format!("{} = {}", name, val)));
        }
        fields.push(("law", lctx.law_expr.clone()));
    }
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-mismatch",
        summary: summary.to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(
            extract_source_lines(source, line, 0),
            Some(Underline {
                col,
                len: source
                    .lines()
                    .nth(line.saturating_sub(1))
                    .map(|l| l.trim().len())
                    .unwrap_or(1)
                    .max(1),
                label: "verify-mismatch".to_string(),
            }),
        ),
    }
}

pub(super) fn verify_runtime_error_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    error: &str,
    line: usize,
    col: usize,
) -> Diagnostic {
    let fields: Vec<(&'static str, String)> = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("error", error.to_string()),
    ];
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-runtime-error",
        summary: "case aborted".to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(
            extract_source_lines(source, line, 0),
            Some(Underline {
                col,
                len: source
                    .lines()
                    .nth(line.saturating_sub(1))
                    .map(|l| l.trim().len())
                    .unwrap_or(1)
                    .max(1),
                label: "verify-runtime-error".to_string(),
            }),
        ),
    }
}

pub(super) fn verify_unexpected_err_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    err_repr: &str,
    line: usize,
    col: usize,
) -> Diagnostic {
    let fields: Vec<(&'static str, String)> = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("error", err_repr.to_string()),
    ];
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-unexpected-err",
        summary: "error propagated from ?".to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(
            extract_source_lines(source, line, 0),
            Some(Underline {
                col,
                len: source
                    .lines()
                    .nth(line.saturating_sub(1))
                    .map(|l| l.trim().len())
                    .unwrap_or(1)
                    .max(1),
                label: "verify-unexpected-err".to_string(),
            }),
        ),
    }
}

// -- Replay failure diagnostics -----------------------------------------------

#[allow(clippy::too_many_arguments)]
pub(super) fn replay_output_mismatch_diagnostic(
    program_file: &str,
    recording_path: &str,
    expected: &str,
    actual: &str,
    diff_path: Option<&str>,
    entry_fn: &str,
    entry_line: usize,
    recording_output_line: usize,
) -> Diagnostic {
    let recording_ref = if recording_output_line > 0 {
        format!("{}:{}", recording_path, recording_output_line)
    } else {
        format!("{}:$.output", recording_path)
    };
    let mut fields: Vec<(&'static str, String)> = vec![
        ("recording", recording_ref),
        ("expected", expected.to_string()),
        ("actual", actual.to_string()),
    ];
    if let Some(dp) = diff_path {
        let label = if dp == "$" {
            "$ (root)".to_string()
        } else {
            dp.to_string()
        };
        fields.push(("diff", label));
    }
    Diagnostic {
        severity: Severity::Fail,
        slug: "replay-output-mismatch",
        summary: "recorded output differs".to_string(),
        span: Span {
            file: program_file.to_string(),
            line: entry_line,
            col: 0,
        },
        fn_name: if entry_fn.is_empty() {
            None
        } else {
            Some(entry_fn.to_string())
        },
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(vec![], None),
    }
}

pub(super) fn replay_effect_error_diagnostic(
    program_file: &str,
    recording_path: &str,
    error: &str,
    entry_fn: &str,
    entry_line: usize,
) -> Diagnostic {
    let fields: Vec<(&'static str, String)> = vec![
        ("recording", recording_path.to_string()),
        ("error", error.to_string()),
    ];
    Diagnostic {
        severity: Severity::Fail,
        slug: "replay-error",
        summary: "replay failed".to_string(),
        span: Span {
            file: program_file.to_string(),
            line: entry_line,
            col: 0,
        },
        fn_name: if entry_fn.is_empty() {
            None
        } else {
            Some(entry_fn.to_string())
        },
        intent: None,
        fields,
        conflict: None,
        repair_primary: None,
        repair_alternatives: Vec::new(),
        repair_example: None,
        regions: AnnotatedRegion::single(vec![], None),
    }
}
