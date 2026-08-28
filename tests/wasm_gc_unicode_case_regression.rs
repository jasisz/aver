#![cfg(feature = "wasm")]

//! Regression — `String.toLower` / `String.toUpper` on wasm-gc must
//! produce exactly what the VM and the generated Rust produce, i.e.
//! Rust `str::to_lowercase` / `str::to_uppercase`: full Unicode
//! mappings, the one-to-many expansions (`ß` uppercases to `SS`), and
//! the final-sigma rule (a Greek capital sigma at the end of a word
//! lowercases to `ς`, elsewhere to `σ`).
//!
//! The wasm-gc helpers used to shift only `A`-`Z` and `a`-`z`, so
//! `String.toLower("ĄĆĘ ŁÓŚ ΩΔ")` came back unchanged while the VM
//! answered `"ąćę łóś ωδ"`. `docs/services.md` has always promised
//! "Unicode-aware" case for both.
//!
//! Every block here runs through BOTH the VM verify runner and the
//! wasm-gc verify runner, and every expected value is Rust std's
//! answer — so a pass on both runners is a cross-backend parity proof
//! for these inputs.

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

fn run_both_backends(source: &str) -> [(&'static str, Vec<VerifyResult>); 2] {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}");
    });
    let vm = run_verify_for_items_vm(
        items.clone(),
        None,
        None,
        "wasm_gc_unicode_case_regression.av",
    )
    .unwrap_or_else(|e| panic!("VM verify failed: {e}"));
    let gc = run_verify_for_items_wasm_gc(items, None, None, "wasm_gc_unicode_case_regression.av")
        .unwrap_or_else(|e| panic!("wasm-gc verify failed: {e}"));
    [("vm", vm), ("wasm-gc", gc)]
}

/// Assert every case passed on both backends. Also reports the first
/// few mismatching cases, which is what makes a red run readable when
/// the corpus is generated rather than hand-written.
fn assert_all_pass_on_both(source: &str, expected_passed: usize) {
    for (backend, results) in run_both_backends(source) {
        let passed: usize = results.iter().map(|r| r.passed).sum();
        let failed: usize = results.iter().map(|r| r.failed).sum();
        let skipped: usize = results.iter().map(|r| r.skipped).sum();
        let declined: usize = results.iter().map(|r| r.declined).sum();
        if (passed, failed, skipped, declined) != (expected_passed, 0, 0, 0) {
            let mut detail = String::new();
            for r in &results {
                for (expr, expected, actual) in r.failures.iter().take(8) {
                    detail.push_str(&format!(
                        "\n  {expr}\n    expected {expected:?}\n    actual   {actual:?}"
                    ));
                }
            }
            panic!(
                "[{backend}] expected {expected_passed}/0/0/0 passed/failed/skipped/declined, \
                 got {passed}/{failed}/{skipped}/{declined}{detail}"
            );
        }
    }
}

/// Escape one arbitrary Unicode scalar sequence into an Aver string
/// literal. Aver has no `\u` escape, so scalars go in as raw UTF-8;
/// only the five characters the lexer reads specially need escaping
/// (`{` opens interpolation, a doubled `}` collapses to one, and a raw
/// newline ends the literal).
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
            _ => out.push(c),
        }
    }
    out
}

const PROGRAM_HEAD: &str = r#"
fn lowerOf(s: String) -> String
    ? "Lowercase of the string"
    String.toLower(s)

fn upperOf(s: String) -> String
    ? "Uppercase of the string"
    String.toUpper(s)
"#;

/// Build one Aver program whose expected values are Rust std's own
/// answers for each chunk. Returns the source and the case count.
fn generated_program(chunks: &[String]) -> (String, usize) {
    let mut src = String::from(PROGRAM_HEAD);
    src.push_str("\nverify lowerOf\n");
    for chunk in chunks {
        src.push_str("    lowerOf(\"");
        src.push_str(&escape_aver(chunk));
        src.push_str("\") => \"");
        src.push_str(&escape_aver(&chunk.to_lowercase()));
        src.push_str("\"\n");
    }
    src.push_str("\nverify upperOf\n");
    for chunk in chunks {
        src.push_str("    upperOf(\"");
        src.push_str(&escape_aver(chunk));
        src.push_str("\") => \"");
        src.push_str(&escape_aver(&chunk.to_uppercase()));
        src.push_str("\"\n");
    }
    let cases = chunks.len() * 2;
    (src, cases)
}

/// Group scalars into chunks. One chunk is one verify case; the
/// per-case fuel budget of the wasm-gc verify runner is what caps the
/// size (a case that runs out of fuel is *declined*, not failed).
fn chunks_of(scalars: impl Iterator<Item = char>, per_chunk: usize) -> Vec<String> {
    let mut out = Vec::new();
    let mut current = String::new();
    let mut n = 0usize;
    for c in scalars {
        current.push(c);
        n += 1;
        if n == per_chunk {
            out.push(std::mem::take(&mut current));
            n = 0;
        }
    }
    if !current.is_empty() {
        out.push(current);
    }
    out
}

fn scalars_in(range: std::ops::RangeInclusive<u32>) -> impl Iterator<Item = char> {
    range.filter_map(char::from_u32)
}

/// The contexts a byte-wise ASCII shift cannot express: the final-sigma
/// rule, the one-to-many expansions, the mappings that change a
/// character's UTF-8 length, and non-Latin scripts inside and outside
/// the BMP. Every expected value here is Rust std's answer.
#[test]
fn explicit_unicode_case_contexts() {
    assert_all_pass_on_both(
        r#"
fn lowerOf(s: String) -> String
    ? "Lowercase of the string"
    String.toLower(s)

fn upperOf(s: String) -> String
    ? "Uppercase of the string"
    String.toUpper(s)

verify lowerOf
    lowerOf("") => ""
    lowerOf("ABC") => "abc"
    lowerOf("Σ") => "σ"
    lowerOf("ΑΣ") => "ας"
    lowerOf("ΑΣΒ") => "ασβ"
    lowerOf("Α'Σ") => "α'ς"
    lowerOf("ΆΣ") => "άς"
    lowerOf("ΣΣ") => "σς"
    lowerOf("ΑΒΓ") => "αβγ"
    lowerOf("😀AΣ") => "😀aς"
    lowerOf("İ") => "i̇"
    lowerOf("K") => "k"
    lowerOf("I") => "i"
    lowerOf("ĄĆĘ ŁÓŚ ß ÀÉÎ ΩΔ") => "ąćę łóś ß àéî ωδ"
    lowerOf("𐐀") => "𐐨"
    lowerOf("𐐨") => "𐐨"
    lowerOf("𞤀") => "𞤢"
    lowerOf("Ꮈ") => "ꮈ"
    lowerOf("Ა") => "ა"

verify upperOf
    upperOf("") => ""
    upperOf("abc") => "ABC"
    upperOf("ß") => "SS"
    upperOf("ŉ") => "ʼN"
    upperOf("ﬀ") => "FF"
    upperOf("ﬆ") => "ST"
    upperOf("ǰ") => "J̌"
    upperOf("ΐ") => "Ϊ́"
    upperOf("ΰ") => "Ϋ́"
    upperOf("և") => "ԵՒ"
    upperOf("ﬓ") => "ՄՆ"
    upperOf("ĳ") => "Ĳ"
    upperOf("ı") => "I"
    upperOf("İ") => "İ"
    upperOf("σ") => "Σ"
    upperOf("ς") => "Σ"
    upperOf("zażółć gęślą jaźń") => "ZAŻÓŁĆ GĘŚLĄ JAŹŃ"
    upperOf("𐐨") => "𐐀"
    upperOf("𞤢") => "𞤀"
    upperOf("ა") => "Ა"
    upperOf("Ꮈ") => "Ꮈ"
"#,
        40,
    );
}

/// Every scalar of the Basic Multilingual Plane except the surrogate
/// block, in chunks, lowercased and uppercased. The expectation is
/// `str::to_lowercase` / `str::to_uppercase` of the same chunk, so the
/// VM lane proves the expectation and the wasm-gc lane proves parity.
#[test]
fn bmp_case_matches_vm() {
    let scalars = scalars_in(0..=0xFFFF);
    let chunks = chunks_of(scalars, 512);
    let (source, cases) = generated_program(&chunks);
    assert_all_pass_on_both(&source, cases);
}

/// Every scalar outside the BMP that has a non-identity case mapping,
/// plus a slice of unmapped astral text around them.
#[test]
fn astral_case_matches_vm() {
    let mapped: Vec<char> = scalars_in(0x10000..=0x10FFFF)
        .filter(|c| c.to_lowercase().next() != Some(*c) || c.to_uppercase().next() != Some(*c))
        .collect();
    let mut scalars = mapped;
    scalars.extend(scalars_in(0x1F600..=0x1F64F));
    let chunks = chunks_of(scalars.into_iter(), 256);
    let (source, cases) = generated_program(&chunks);
    assert_all_pass_on_both(&source, cases);
}

/// The boundaries of every compressed mapping run: a run that is one
/// scalar too wide or too narrow shows up here and nowhere else.
#[test]
fn mapping_run_boundaries_match_vm() {
    let mut interesting: Vec<char> = Vec::new();
    let mut prev_lower_delta: Option<i64> = None;
    let mut prev_upper_delta: Option<i64> = None;
    for cp in 0u32..=0x10FFFF {
        let Some(c) = char::from_u32(cp) else {
            prev_lower_delta = None;
            prev_upper_delta = None;
            continue;
        };
        let lower_delta = simple_delta(c.to_lowercase().collect::<Vec<_>>(), cp);
        let upper_delta = simple_delta(c.to_uppercase().collect::<Vec<_>>(), cp);
        if lower_delta != prev_lower_delta || upper_delta != prev_upper_delta {
            for probe in [cp.saturating_sub(1), cp, cp + 1] {
                if let Some(p) = char::from_u32(probe) {
                    interesting.push(p);
                }
            }
        }
        prev_lower_delta = lower_delta;
        prev_upper_delta = upper_delta;
    }
    interesting.sort_unstable();
    interesting.dedup();
    let chunks = chunks_of(interesting.into_iter(), 256);
    let (source, cases) = generated_program(&chunks);
    assert_all_pass_on_both(&source, cases);
}

fn simple_delta(mapped: Vec<char>, cp: u32) -> Option<i64> {
    match mapped.as_slice() {
        [one] => Some(*one as i64 - cp as i64),
        _ => None,
    }
}

/// The whole scalar space, both directions. Ignored by default — the
/// three tests above already cover every mapped scalar; this one also
/// walks the ~1.05M unmapped ones and takes minutes.
#[test]
#[ignore]
fn every_scalar_case_matches_vm() {
    let chunks = chunks_of(scalars_in(0..=0x10FFFF), 512);
    let (source, cases) = generated_program(&chunks);
    assert_all_pass_on_both(&source, cases);
}
