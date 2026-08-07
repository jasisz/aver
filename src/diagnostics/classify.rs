//! Classifiers and source-extraction helpers shared by factories.
//!
//! All functions are pure (no IO, no globals) and runtime-neutral.

use super::model::{AnnotatedRegion, SourceLine};

// ─── Source extraction ───────────────────────────────────────────────────────

/// Extract source lines around `line` (1-based) with `context` lines of
/// surrounding context on each side.
pub(crate) fn extract_source_lines(source: &str, line: usize, context: usize) -> Vec<SourceLine> {
    let lines: Vec<&str> = source.lines().collect();
    let start = line.saturating_sub(context + 1);
    let end = (line + context).min(lines.len());
    (start..end)
        .map(|i| SourceLine {
            line_num: i + 1,
            text: lines[i].to_string(),
        })
        .collect()
}

/// Extract source lines for an inclusive range [from..to] (1-based).
pub(crate) fn extract_source_lines_range(source: &str, from: usize, to: usize) -> Vec<SourceLine> {
    let lines: Vec<&str> = source.lines().collect();
    let start = from.saturating_sub(1);
    let end = to.min(lines.len());
    (start..end)
        .map(|i| SourceLine {
            line_num: i + 1,
            text: lines[i].to_string(),
        })
        .collect()
}

/// Extract the declared return type from a "declared return type is X" message.
pub(crate) fn extract_return_type(msg: &str) -> &str {
    msg.rsplit("declared return type is ").next().unwrap_or("?")
}

/// Estimate how many characters to underline starting at `col` (1-based,
/// counted in Unicode scalar values).
///
/// Byte-indexing into `line` mis-measures multi-byte characters (em-dash,
/// CJK, emoji). We iterate `chars()` so `col` lines up with what the
/// parser emits (char-based) and what frontends render (1 glyph per
/// scalar).
pub(crate) fn estimate_span_len(line: &str, col: usize) -> usize {
    let start = col.saturating_sub(1);
    let len = line
        .chars()
        .skip(start)
        .take_while(|c| !c.is_whitespace() && !matches!(c, '(' | ')' | '[' | ']' | ',' | ':'))
        .count();
    if len == 0 { 1 } else { len }
}

/// Fill small gaps (≤2 lines) between sorted regions with bridging source
/// lines. Avoids a `...` separator for tiny jumps.
pub(crate) fn fill_small_region_gaps(regions: &mut Vec<AnnotatedRegion>, source: &str) {
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
                i += 1;
            }
        }
        i += 1;
    }
}

/// Find the line number of the block header (`fn`, `verify`, `decision`) for
/// `name`, searching forward from the start up to (but not including)
/// `before_line`.
pub(crate) fn find_block_header_line(
    source: &str,
    name: &str,
    before_line: usize,
) -> Option<usize> {
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
                best = Some(line_num);
            }
        }
    }
    best
}

/// Find where the block preamble ends. Returns the last preamble line
/// number (1-based), capped before `before_line`.
pub(crate) fn find_preamble_end(source: &str, header_line: usize, before_line: usize) -> usize {
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
/// If `summary` mentions "right side" or `=>`, searches after `=>` in the line.
/// Returns 1-based col.
pub(crate) fn find_precise_span(source_line: &str, summary: &str) -> Option<(usize, usize)> {
    let search_after_arrow = summary.contains("right side") || summary.contains("=>");
    for quote in ['`', '\''] {
        if let Some(start_offset) = summary.find(quote) {
            let start = start_offset + 1;
            if let Some(end_offset) = summary[start..].find(quote) {
                let needle = &summary[start..start + end_offset];
                if !needle.is_empty() {
                    let search_region = if search_after_arrow {
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

// ─── Classification ──────────────────────────────────────────────────────────

/// Classification of a type error message: (slug, conflict, fields, repair).
pub(crate) type TypeErrorClassification = (
    &'static str,
    Option<String>,
    Vec<(&'static str, String)>,
    Option<String>,
);

pub(crate) fn classify_type_error(msg: &str) -> TypeErrorClassification {
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

    if msg.starts_with("Unknown identifier") || msg.starts_with("Unknown function") {
        return (
            "unknown-ident",
            None,
            Vec::new(),
            Some("Check the spelling or add the missing import".to_string()),
        );
    }

    if msg.contains("expects") && msg.contains("argument") {
        return (
            "arity-mismatch",
            Some(msg.to_string()),
            Vec::new(),
            Some("Adjust the number of arguments".to_string()),
        );
    }

    if msg.contains("effect") && (msg.contains("not declared") || msg.contains("not allowed")) {
        return (
            "effect-violation",
            Some(msg.to_string()),
            Vec::new(),
            Some("Add the missing effect to the function's ! [...] declaration".to_string()),
        );
    }

    if msg.contains("'/' operator is not defined for Int") {
        return (
            "int-div",
            None,
            Vec::new(),
            Some(
                "Use Int.div(a, b) : Result<Int, String>; handle the failure with `match` or `Result.withDefault(Int.div(a, b), fallback)`. With a nonzero literal divisor, Int.div(a, k) is total and returns plain Int"
                    .to_string(),
            ),
        );
    }

    ("type-error", None, Vec::new(), None)
}

pub(crate) fn classify_finding(msg: &str) -> (&'static str, Option<String>) {
    if msg.starts_with("File must declare `module") {
        return (
            "missing-module",
            Some("Add `module <Name>` as the first top-level item".to_string()),
        );
    }
    if msg.contains("has effects") && msg.contains("plain verify block") {
        (
            "verify-effectful",
            Some(
                "Use `verify <fn> trace` with `given` stubs, or test stateful flows via replay"
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
    } else if msg.contains("Independent product branches")
        && msg.contains("potentially conflicting effects")
    {
        ("independence-hazard", split_repair(msg))
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
    } else if msg.starts_with("Function '") && msg.contains("should use camelCase") {
        (
            "bad-fn-name",
            Some("Rename the function to camelCase; fix call sites manually".to_string()),
        )
    } else if msg.starts_with("Type '") && msg.contains("should use PascalCase") {
        (
            "bad-type-name",
            Some("Rename the type to PascalCase; fix constructor references manually".to_string()),
        )
    } else if msg.starts_with("Module '") && msg.contains("should use PascalCase") {
        (
            "bad-module-name",
            Some("Rename the module to PascalCase; update depends/file name to match".to_string()),
        )
    } else if msg.starts_with("Variant '") && msg.contains("PascalCase") {
        (
            "bad-variant-name",
            Some("Rename the variant to PascalCase".to_string()),
        )
    } else if msg.starts_with("Record field '") && msg.contains("camelCase") {
        (
            "bad-field-name",
            Some("Rename the field to camelCase".to_string()),
        )
    } else if msg.contains("reserved by the Aver standard library") {
        ("stdlib-shadow", split_repair(msg))
    } else if msg.contains("verify examples") || msg.contains("verify case") {
        ("verify-coverage", None)
    } else {
        ("check", None)
    }
}

/// Split a message on ` — ` (em-dash) to extract the repair suggestion.
pub(crate) fn split_repair(msg: &str) -> Option<String> {
    msg.split_once(" — ")
        .or_else(|| msg.split_once(" -- "))
        .map(|(_, repair)| {
            let mut r = repair.to_string();
            if let Some(first) = r.get_mut(0..1) {
                first.make_ascii_uppercase();
            }
            r
        })
}

pub(crate) fn extract_fn_name_from_finding(msg: &str) -> Option<String> {
    if let Some(start) = msg.find('\'')
        && let Some(end) = msg[start + 1..].find('\'')
    {
        return Some(msg[start + 1..start + 1 + end].to_string());
    }
    None
}

#[cfg(test)]
mod tests {
    use super::classify_finding;

    #[test]
    fn classifies_independence_hazard_warning() {
        let (slug, repair) = classify_finding(
            "Independent product branches 1 and 2 use potentially conflicting effects [Console.print, Console.error] (shared terminal/output hazard) — independent products may reorder or overlap these effects; keep them sequential or suppress with [[check.suppress]] reason if this independence is intentional",
        );
        assert_eq!(slug, "independence-hazard");
        assert!(repair.is_some());
    }
}
