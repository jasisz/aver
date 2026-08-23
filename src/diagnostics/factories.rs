//! Factory functions — convert internal error types to canonical `Diagnostic`.
//!
//! All factories are pure: they take source text + structural error data and
//! return a `Diagnostic`. No IO, no globals, wasm-safe.

use super::classify::{
    SourceIndex, classify_finding, classify_type_error, estimate_span_len,
    extract_fn_name_from_finding, extract_return_type, extract_source_lines, find_precise_span,
};
use super::model::{AnnotatedRegion, Diagnostic, Repair, Severity, Span, Underline};
use crate::checker::{CheckFinding, VerifyLawContext};
use crate::types::checker::TypeError;

/// Provider composition failed before verify cases could execute. This is a
/// runtime setup error, not a source type error; keeping the distinction in
/// the canonical diagnostic prevents `audit` from silently passing or
/// sending users to `aver check`.
pub fn verify_provider_setup_diagnostic(file: &str, error: &str) -> Diagnostic {
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-provider-setup",
        summary: format!("provider composition could not be installed for verify: {error}"),
        span: Span {
            file: file.to_string(),
            line: 1,
            col: 1,
        },
        fn_name: None,
        intent: None,
        fields: vec![("provider_error", error.to_string())],
        conflict: None,
        repair: Repair::default(),
        regions: Vec::new(),
        related: Vec::new(),
        from_hostile: false,
    }
}

/// Build a `Diagnostic` from a `TypeError` (from the typechecker).
pub fn from_type_error(te: &TypeError, source: &str, file: &str) -> Diagnostic {
    let source_index = SourceIndex::new(source);
    from_type_error_with_index(te, &source_index, source, file)
}

pub(crate) fn from_type_error_with_index(
    te: &TypeError,
    source_index: &SourceIndex<'_>,
    source: &str,
    file: &str,
) -> Diagnostic {
    let (source, file) = match &te.origin {
        Some(origin) => (
            origin.source.as_deref().unwrap_or_default(),
            origin.file.as_str(),
        ),
        None => (source, file),
    };
    if te.origin.is_some() {
        let origin_index = SourceIndex::new(source);
        return from_type_error_indexed(te, &origin_index, file);
    }
    from_type_error_indexed(te, source_index, file)
}

fn from_type_error_indexed(
    te: &TypeError,
    source_index: &SourceIndex<'_>,
    file: &str,
) -> Diagnostic {
    let msg = &te.message;
    let line = te.line;
    let col = te.col;

    let (slug, conflict, fields, repair_text) = classify_type_error(msg);

    let source_line_text = source_index.line(line);
    let (ul_col, ul_len) = if col > 0 {
        (col, estimate_span_len(source_line_text, col))
    } else {
        let indent = source_line_text.len() - source_line_text.trim_start().len();
        (indent + 1, source_line_text.trim().len())
    };

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
        source_lines: source_index.extract(line, 0),
        underline: primary_underline,
    }];

    if let Some(ref sec) = te.secondary
        && sec.line != line
    {
        let sec_source_text = source_index.line(sec.line);
        let (sec_col, sec_len) = if sec.col > 0 {
            (sec.col, estimate_span_len(sec_source_text, sec.col))
        } else {
            let indent = sec_source_text.len() - sec_source_text.trim_start().len();
            (indent + 1, sec_source_text.trim().len())
        };
        regions.push(AnnotatedRegion {
            source_lines: source_index.extract(sec.line, 0),
            underline: Some(Underline {
                col: sec_col,
                len: sec_len,
                label: sec.label.clone(),
            }),
        });
    }

    regions.sort_by_key(|r| r.source_lines.first().map(|sl| sl.line_num).unwrap_or(0));
    source_index.fill_small_region_gaps(&mut regions);

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
        repair: repair_text.map(Repair::primary).unwrap_or_default(),
        regions,
        related: Vec::new(),
        from_hostile: false,
    }
}

/// Build a `Diagnostic` for an unused binding warning.
pub fn unused_binding_diagnostic(
    binding: &str,
    fn_name: &str,
    line: usize,
    source: &str,
    file: &str,
) -> Diagnostic {
    let source_index = SourceIndex::new(source);
    unused_binding_diagnostic_with_index(binding, fn_name, line, &source_index, file)
}

pub(crate) fn unused_binding_diagnostic_with_index(
    binding: &str,
    fn_name: &str,
    line: usize,
    source_index: &SourceIndex<'_>,
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
        repair: Repair::primary(format!("Remove the binding or prefix with _: _{}", binding)),
        regions: AnnotatedRegion::single(source_index.extract(line, 0), None),
        related: Vec::new(),
        from_hostile: false,
    }
}

/// Build a `Diagnostic` signaling that a file is not formatter-clean.
///
/// `violations` carries structured per-rule hits from
/// `try_format_source`. When empty (no rule reports a violation yet the
/// formatter still rewrote the file), emits a minimal warning without
/// line ranges — no speculation, no false positives.
///
/// Up to [`MAX_VIOLATION_REGIONS`] violations surface as `regions` so
/// tty output can underline them; the rest are still counted in
/// `fields.changed_ranges`. Primary span points at the first violation.
pub fn needs_format_diagnostic(
    file: &str,
    violations: &[super::model::FormatViolation],
    source: &str,
) -> Diagnostic {
    const MAX_VIOLATION_REGIONS: usize = 3;

    if violations.is_empty() {
        return Diagnostic {
            severity: Severity::Warning,
            slug: "needs-format",
            summary: "file is not formatter-clean".to_string(),
            span: Span {
                file: file.to_string(),
                line: 1,
                col: 1,
            },
            fn_name: None,
            intent: None,
            fields: Vec::new(),
            conflict: None,
            repair: Repair::primary("Run `aver format` to apply the formatter"),
            regions: Vec::new(),
            related: Vec::new(),
            from_hostile: false,
        };
    }

    // Regions must be in ascending-line order so the tty renderer
    // doesn't collapse later regions into earlier already-emitted lines.
    let mut sorted: Vec<&super::model::FormatViolation> = violations.iter().collect();
    sorted.sort_by_key(|v| (v.line, v.col));
    // `first` tracks the earliest-appearing violation so the summary
    // and primary span agree with the first rendered region.
    let first = sorted[0];

    let source_lines: Vec<&str> = source.lines().collect();
    let mut regions: Vec<AnnotatedRegion> = Vec::new();
    for v in sorted.iter().take(MAX_VIOLATION_REGIONS) {
        let line_text = source_lines
            .get(v.line.saturating_sub(1))
            .copied()
            .unwrap_or("");
        if line_text.is_empty() && v.line == 0 {
            continue;
        }
        let line_len = line_text.len().max(1);
        let label = format!("{}: {}", v.rule, v.message);
        regions.push(AnnotatedRegion {
            source_lines: vec![super::model::SourceLine {
                line_num: v.line,
                text: line_text.to_string(),
            }],
            underline: Some(Underline {
                col: v.col.max(1),
                len: (line_len - v.col.saturating_sub(1).min(line_len)).max(1),
                label,
            }),
        });
    }

    let mut fields: Vec<(&'static str, String)> = vec![
        ("violations", violations.len().to_string()),
        ("first_rule", first.rule.to_string()),
    ];
    if violations.len() > MAX_VIOLATION_REGIONS {
        fields.push((
            "shown_violations",
            format!("{} (of {})", MAX_VIOLATION_REGIONS, violations.len()),
        ));
    }

    let summary = if violations.len() == 1 {
        first.message.clone()
    } else {
        format!("{} violations, first: {}", violations.len(), first.message)
    };

    Diagnostic {
        severity: Severity::Warning,
        slug: "needs-format",
        summary,
        span: Span {
            file: file.to_string(),
            line: first.line.max(1),
            col: first.col.max(1),
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair: Repair::primary("Run `aver format` to apply the formatter"),
        regions,
        related: Vec::new(),
        from_hostile: false,
    }
}

/// Build a `Diagnostic` from a `CheckFinding` (intent/verify/coverage/etc).
pub fn from_check_finding(
    severity: Severity,
    finding: &CheckFinding,
    source: &str,
    file: &str,
) -> Diagnostic {
    let source_index = SourceIndex::new(source);
    from_check_finding_with_index(severity, finding, &source_index, file)
}

pub(crate) fn from_check_finding_with_index(
    severity: Severity,
    finding: &CheckFinding,
    source_index: &SourceIndex<'_>,
    file: &str,
) -> Diagnostic {
    let (slug, repair_text) = classify_finding(&finding.message);
    let fn_name = finding
        .fn_name
        .clone()
        .or_else(|| extract_fn_name_from_finding(&finding.message));

    let summary = if repair_text.is_some() {
        finding
            .message
            .split_once(" — ")
            .or_else(|| finding.message.split_once(" -- "))
            .map(|(s, _)| s.to_string())
            .unwrap_or_else(|| finding.message.clone())
    } else {
        finding.message.clone()
    };

    let source_line_text = source_index.line(finding.line);
    let (col, span_len) = find_precise_span(source_line_text, &summary).unwrap_or_else(|| {
        let indent = source_line_text.len() - source_line_text.trim_start().len();
        (indent + 1, source_line_text.trim().len())
    });

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
        source_lines: source_index.extract(finding.line, 0),
        underline: primary_underline,
    }];

    for extra in &finding.extra_spans {
        let extra_source_line = source_index.line(extra.line);
        let (extra_col, extra_len) = if extra.col > 0 && extra.len > 0 {
            (extra.col, extra.len)
        } else {
            find_precise_span(extra_source_line, &extra.label).unwrap_or_else(|| {
                let indent = extra_source_line.len() - extra_source_line.trim_start().len();
                (indent + 1, extra_source_line.trim().len())
            })
        };
        regions.push(AnnotatedRegion {
            source_lines: source_index.extract(extra.line, 0),
            underline: Some(Underline {
                col: extra_col,
                len: extra_len,
                label: extra.label.clone(),
            }),
        });
    }

    let finding_is_header = source_line_text.trim_start().starts_with("fn ")
        || source_line_text.trim_start().starts_with("verify ")
        || source_line_text.trim_start().starts_with("decision ");
    if !finding_is_header
        && let Some(ref name) = fn_name
        && let Some(header_line) = source_index.find_block_header_line(name, finding.line)
        && header_line < finding.line
    {
        let preamble_end = source_index.find_preamble_end(header_line, finding.line);
        let capped_end = preamble_end.min(header_line + 3);
        let header_lines = source_index.extract_range(header_line, capped_end);
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

    regions.sort_by_key(|r| r.source_lines.first().map(|sl| sl.line_num).unwrap_or(0));
    source_index.fill_small_region_gaps(&mut regions);

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
        repair: repair_text.map(Repair::primary).unwrap_or_default(),
        regions,
        related: Vec::new(),
        from_hostile: false,
    }
}

// ─── Verify failure diagnostics ──────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
pub fn verify_mismatch_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    expected: &str,
    actual: &str,
    line: usize,
    col: usize,
    is_law: bool,
    law_context: Option<&VerifyLawContext>,
    from_hostile: bool,
    hostile_profile: Option<&str>,
) -> Diagnostic {
    let summary = match (is_law, from_hostile) {
        (true, true) => "law violated under --hostile expansion",
        (true, false) => "law violated",
        (false, _) => "assertion failed",
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
    if from_hostile {
        // Origin label distinguishes the two hostile axes:
        //   "effect profile: <method>/<profile>" — the user's `given`
        //   stub for that classified effect was overridden by an
        //   adversarial profile (Time.unixMs/saturated, ...).
        //   "value boundary substitution"        — the typed `given`
        //   domain was extended with a per-type adversarial value
        //   (Int 0/1/-1/MIN/MAX, Float NaN/±Inf, Str empty/NUL/long).
        //   The substituted value already shows up in the `case`
        //   field (e.g. `willCompleteBeforeDeadline(0)`), so this
        //   label just names the axis.
        let origin_label = match hostile_profile {
            Some(profile) => format!("effect profile: {}", profile),
            None => "value boundary substitution".to_string(),
        };
        fields.push(("origin", origin_label));
    }
    let repair = if from_hostile {
        if hostile_profile
            .map(|p| p.contains("reverse-eval"))
            .unwrap_or(false)
        {
            // Order-axis hostile: the case differs between forward and
            // reverse evaluation of an `(a, b)!` independent product.
            // Pure laws claim their independent products commute, so
            // a divergence usually means the law accidentally pinned
            // execution order through a flat trace projection like
            // `.trace.event(k)` (which indexes the global emission
            // sequence) instead of a structural one like
            // `.trace.group(g).branch(b).event(k)` (which indexes by
            // source position and is order-invariant).
            Repair::primary(
                "the law's tuple disagrees between forward and reverse \
                 evaluation of the `(a, b)!` independent product — Aver \
                 makes no guarantee about which branch runs first, so any \
                 law that does is fragile. If you used a flat projection \
                 like `.trace.event(k)`, rewrite it as \
                 `.trace.group(g).branch(b).event(k)` to address events by \
                 source position instead of execution order. If the \
                 divergence is in the return value itself, you've found a \
                 stub or compiler-level ordering dependency that the \
                 \"independent\" claim doesn't actually support — drop \
                 `!` for sequential evaluation, or fix the stub to be \
                 purely a function of `(path, n, args...)`.",
            )
        } else if hostile_profile.is_some() {
            // Effect-side hostile fires inside `verify <fn> law <name>`
            // (with or without `trace`). Three honest options: adjust
            // the impl (the profile models a real production world);
            // declare the oracle assumption with `when` so hostile
            // skips profiles that violate it (`when clock(root, 1) >
            // clock(root, 0)` for monotonicity); or — if the claim
            // really only holds for the one stub you wrote — drop
            // `law` form and use `verify <fn>` cases-form (example
            // semantics).
            Repair::primary(
                "the law passes for the world your `given` stub describes \
                 but breaks under this adversarial profile. Three options: \
                 (a) adjust the impl to be robust against the profile (it \
                 models a real production world — frozen clock, empty disk, \
                 network down); (b) declare the oracle assumption with \
                 `when` (e.g. `when clock(root, 1) > clock(root, 0)` for \
                 monotonicity) so hostile skips profiles that violate it; \
                 (c) if the claim really only holds for the one stub you \
                 wrote, drop `law` form and use `verify <fn>` cases-form \
                 (example semantics) with that stub.",
            )
        } else {
            // Value-side hostile: only fires on `verify <fn> law <name>`,
            // which DOES support `when`. The `when` suggestion is honest
            // here. If the law has typed `given` clauses, name them in
            // the message so the user sees exactly which clause to
            // narrow with `when` instead of staring at a generic
            // suggestion.
            let given_refs = law_context
                .map(|lc| {
                    lc.givens
                        .iter()
                        .map(|(name, _)| format!("`{}`", name))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let scoped = match given_refs.as_slice() {
                [] => "add `when <precondition>` to scope the law".to_string(),
                [one] => format!(
                    "add `when {} <precondition>` (e.g. `when {} > 0`) to scope the law to the values {} actually holds for",
                    one, one, one
                ),
                many => format!(
                    "add `when` over {} to scope the law to the values they actually hold for",
                    many.join(" / ")
                ),
            };
            Repair::primary(format!(
                "this case isn't in your declared `given` set — the claim isn't \
                 universal. Either {}, or drop `law` form and use `verify <fn>` \
                 (cases form, example semantics) with the values you actually meant.",
                scoped
            ))
        }
    } else {
        Repair::default()
    };
    // Distinct slug for hostile-only failures so consumers (jq, CI gates,
    // playground filters) can split the dual-run into two channels:
    //   `verify-mismatch`         — declared world failure, real bug
    //   `verify-hostile-mismatch` — adversarial world failure, missing
    //                                precondition or unpinned effect
    // Both carry the same fields and `from_hostile` flag; the slug is
    // for routing.
    let slug = if from_hostile {
        "verify-hostile-mismatch"
    } else {
        "verify-mismatch"
    };
    Diagnostic {
        severity: Severity::Fail,
        slug,
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
        repair,
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
                label: slug.to_string(),
            }),
        ),
        related: Vec::new(),
        from_hostile,
    }
}

pub fn verify_runtime_error_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    error: &str,
    line: usize,
    col: usize,
) -> Diagnostic {
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
        fields: vec![
            ("block", block_name.to_string()),
            ("case", case_expr.to_string()),
            ("error", error.to_string()),
        ],
        conflict: None,
        repair: Repair::default(),
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
        related: Vec::new(),
        from_hostile: false,
    }
}

/// A verify case that ran out of its step budget.
///
/// Deliberately its own slug rather than a `verify-runtime-error`: the case
/// did not crash and did not disagree, it was not answered. The repair is a
/// project decision (`aver.toml`), not a source fix, so the two must not
/// share a diagnostic.
#[allow(clippy::too_many_arguments)]
pub fn verify_declined_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    reason: &str,
    steps: u64,
    limit: u64,
    raised_by: Option<&str>,
    line: usize,
    col: usize,
) -> Diagnostic {
    let mut fields = vec![
        ("block", block_name.to_string()),
        ("case", case_expr.to_string()),
        ("reason", reason.to_string()),
        ("steps", steps.to_string()),
        ("budget", limit.to_string()),
    ];
    if let Some(entry) = raised_by {
        fields.push((
            "budget-raised-by",
            format!("aver.toml [[verify.costly]] fn = \"{}\"", entry),
        ));
    }
    Diagnostic {
        severity: Severity::Fail,
        slug: "verify-declined",
        summary: "case not answered".to_string(),
        span: Span {
            file: file.to_string(),
            line,
            col,
        },
        fn_name: None,
        intent: None,
        fields,
        conflict: None,
        repair: Repair {
            primary: Some(format!(
                "raise the budget for this fn in aver.toml: [[verify.costly]] fn = \"{}\", step-limit = {}, reason = \"why this case is expensive\"",
                block_name.split_whitespace().next().unwrap_or(block_name),
                limit.saturating_mul(4)
            )),
            alternatives: Vec::new(),
            example: None,
        },
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
                label: "verify-declined".to_string(),
            }),
        ),
        related: Vec::new(),
        from_hostile: false,
    }
}

pub fn verify_unexpected_err_diagnostic(
    file: &str,
    source: &str,
    block_name: &str,
    case_expr: &str,
    err_repr: &str,
    line: usize,
    col: usize,
) -> Diagnostic {
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
        fields: vec![
            ("block", block_name.to_string()),
            ("case", case_expr.to_string()),
            ("error", err_repr.to_string()),
        ],
        conflict: None,
        repair: Repair::default(),
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
        related: Vec::new(),
        from_hostile: false,
    }
}

// ─── Replay failure diagnostics ──────────────────────────────────────────────

#[allow(clippy::too_many_arguments)]
pub fn replay_output_mismatch_diagnostic(
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
        repair: Repair::default(),
        regions: AnnotatedRegion::single(vec![], None),
        related: Vec::new(),
        from_hostile: false,
    }
}

pub fn replay_effect_error_diagnostic(
    program_file: &str,
    recording_path: &str,
    error: &str,
    entry_fn: &str,
    entry_line: usize,
) -> Diagnostic {
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
        fields: vec![
            ("recording", recording_path.to_string()),
            ("error", error.to_string()),
        ],
        conflict: None,
        repair: Repair::default(),
        regions: AnnotatedRegion::single(vec![], None),
        related: Vec::new(),
        from_hostile: false,
    }
}
