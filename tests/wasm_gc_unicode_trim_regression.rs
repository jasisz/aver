#![cfg(feature = "wasm")]

//! Regression — `String.trim` on wasm-gc must match the VM and Rust
//! `str::trim`: trim every Unicode `White_Space` scalar value, not only
//! space/tab/LF/CR bytes. The concrete failures from #1189 included
//! NBSP (U+00A0), ideographic space (U+3000), vertical tab and form feed.

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

const WHITE_SPACE: &[char] = &[
    '\u{0009}', '\u{000A}', '\u{000B}', '\u{000C}', '\u{000D}', '\u{0020}', '\u{0085}', '\u{00A0}',
    '\u{1680}', '\u{2000}', '\u{2001}', '\u{2002}', '\u{2003}', '\u{2004}', '\u{2005}', '\u{2006}',
    '\u{2007}', '\u{2008}', '\u{2009}', '\u{200A}', '\u{2028}', '\u{2029}', '\u{202F}', '\u{205F}',
    '\u{3000}',
];

fn run_both_backends(source: &str) -> [(&'static str, Vec<VerifyResult>); 2] {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let vm = run_verify_for_items_vm(
        items.clone(),
        None,
        None,
        "wasm_gc_unicode_trim_regression.av",
    )
    .unwrap_or_else(|e| panic!("VM verify failed: {e}\n--- source ---\n{source}"));
    let gc = run_verify_for_items_wasm_gc(items, None, None, "wasm_gc_unicode_trim_regression.av")
        .unwrap_or_else(|e| panic!("wasm-gc verify failed: {e}\n--- source ---\n{source}"));
    [("vm", vm), ("wasm-gc", gc)]
}

fn assert_all_pass_on_both(source: &str, expected_passed: usize) {
    for (backend, results) in run_both_backends(source) {
        let passed: usize = results.iter().map(|r| r.passed).sum();
        let failed: usize = results.iter().map(|r| r.failed).sum();
        let skipped: usize = results.iter().map(|r| r.skipped).sum();
        let declined: usize = results.iter().map(|r| r.declined).sum();
        assert_eq!(
            (passed, failed, skipped, declined),
            (expected_passed, 0, 0, 0),
            "[{backend}] expected {expected_passed}/0/0/0 passed/failed/skipped/declined, \
             got {passed}/{failed}/{skipped}/{declined}\n--- source ---\n{source}"
        );
    }
}

/// Escape one arbitrary scalar sequence into an Aver string literal.
/// Aver has no `\u` escape, so non-ASCII whitespace enters the fixture as
/// raw UTF-8; the lexer-supported ASCII controls use named escapes where
/// possible so the source stays readable.
fn escape_aver(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 8);
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '{' => out.push_str("\\{"),
            '}' => out.push_str("\\}"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{000C}' => out.push_str("\\f"),
            _ => out.push(c),
        }
    }
    out
}

fn trim_program(inputs: &[String]) -> String {
    let mut src = String::from(
        r#"
fn trimmed(s: String) -> String
    ? "Trim whitespace from both ends."
    String.trim(s)

verify trimmed
"#,
    );
    for input in inputs {
        src.push_str("    trimmed(\"");
        src.push_str(&escape_aver(input));
        src.push_str("\") => \"");
        src.push_str(&escape_aver(input.trim()));
        src.push_str("\"\n");
    }
    src
}

#[test]
fn trim_strips_the_full_unicode_white_space_set() {
    let mut inputs: Vec<String> = WHITE_SPACE.iter().map(|&ch| format!("{ch}x{ch}")).collect();

    let all: String = WHITE_SPACE.iter().collect();
    let all_reversed: String = WHITE_SPACE.iter().rev().collect();
    inputs.push(format!("{all}x{all_reversed}"));

    // Internal whitespace is preserved, and similarly named non-White_Space
    // scalars (U+200B ZERO WIDTH SPACE) are not stripped.
    inputs.push(format!("a{}b", '\u{3000}'));
    inputs.push("\u{200B}x\u{200B}".to_string());
    // Exercise the four-byte UTF-8 decode path when trimming stops at a
    // non-whitespace scalar.
    inputs.push("\u{3000}😀\u{3000}".to_string());
    inputs.push("plain".to_string());

    let source = trim_program(&inputs);
    assert_all_pass_on_both(&source, inputs.len());
}
