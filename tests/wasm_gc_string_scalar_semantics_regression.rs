#![cfg(feature = "wasm")]

//! Regression — `String.len` / `String.charAt` / `String.slice` /
//! `String.chars` on wasm-gc must count and index Unicode scalar
//! values, exactly like the VM (the canonical semantics: docs say
//! "number of characters", the VM uses `s.chars()`).
//!
//! Strings lower to UTF-8 `(array i8)` on wasm-gc, and the original
//! helpers worked directly in byte space: `String.len` was `array.len`
//! (so `len("ż") == 2`), `charAt` returned a single byte (tearing
//! multi-byte characters apart), `slice` cut at byte offsets, and
//! `chars` split into bytes. Any program mixing these with multi-byte
//! text diverged from the VM — `examples/data/json.av` under
//! `verify --wasm-gc` failed its emoji cases. `String.byteLength`
//! (explicit byte count on every backend) keeps the old `array.len`
//! body under its own helper.
//!
//! Every block here runs through BOTH the VM verify runner and the
//! wasm-gc verify runner; the expected values are the VM's answers, so
//! a pass on both runners is a cross-backend parity proof for these
//! cases.

use aver::checker::VerifyResult;
use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc;
use aver::source::parse_source;

fn run_both_backends(source: &str) -> [(&'static str, Vec<VerifyResult>); 2] {
    let items = parse_source(source).unwrap_or_else(|e| {
        panic!("parse failed: {e}\n--- source ---\n{source}");
    });
    let vm = run_verify_for_items_vm(
        items.clone(),
        None,
        None,
        "wasm_gc_string_scalar_semantics_regression.av",
    )
    .unwrap_or_else(|e| panic!("VM verify failed: {e}\n--- source ---\n{source}"));
    let gc = run_verify_for_items_wasm_gc(
        items,
        None,
        None,
        "wasm_gc_string_scalar_semantics_regression.av",
    )
    .unwrap_or_else(|e| panic!("wasm-gc verify failed: {e}\n--- source ---\n{source}"));
    [("vm", vm), ("wasm-gc", gc)]
}

fn assert_all_pass_on_both(source: &str, expected_passed: usize) {
    for (backend, results) in run_both_backends(source) {
        let passed: usize = results.iter().map(|r| r.passed).sum();
        let failed: usize = results.iter().map(|r| r.failed).sum();
        let skipped: usize = results.iter().map(|r| r.skipped).sum();
        assert_eq!(
            (passed, failed, skipped),
            (expected_passed, 0, 0),
            "[{backend}] expected {expected_passed}/0/0 passed/failed/skipped, got {passed}/{failed}/{skipped}\n--- source ---\n{source}"
        );
    }
}

/// `String.len` over 2-byte (ż), 3-byte (—) and 4-byte (😀) characters:
/// one character, one unit of length. Red on the byte-counting helper
/// (`len("ż")` was 2, `len("😀")` was 4).
#[test]
fn len_counts_scalar_values() {
    assert_all_pass_on_both(
        r#"
fn lenOf(s: String) -> Int
    ? "Length of the string in characters"
    String.len(s)

verify lenOf
    lenOf("") => 0
    lenOf("abc") => 3
    lenOf("zażółć") => 6
    lenOf("😀") => 1
    lenOf("a—b") => 3
"#,
        5,
    );
}

/// `String.byteLength` stays byte-based on every backend — the
/// explicit escape hatch must NOT inherit the scalar semantics.
#[test]
fn byte_length_still_counts_bytes() {
    assert_all_pass_on_both(
        r#"
fn byteLen(s: String) -> Int
    ? "UTF-8 byte length of the string"
    String.byteLength(s)

verify byteLen
    byteLen("abc") => 3
    byteLen("ż") => 2
    byteLen("😀") => 4
"#,
        3,
    );
}

/// `String.charAt` indexes scalars and returns the FULL character
/// (1–4 bytes). Red on the byte helper twice over: the index was a
/// byte offset and the result a single byte of a torn character.
#[test]
fn char_at_indexes_scalars_and_returns_full_character() {
    assert_all_pass_on_both(
        r#"
fn charAtOf(s: String, i: Int) -> Option<String>
    ? "Character at index i"
    String.charAt(s, i)

verify charAtOf
    charAtOf("zażółć", 2) => Option.Some("ż")
    charAtOf("😀x", 0) => Option.Some("😀")
    charAtOf("😀x", 1) => Option.Some("x")
    charAtOf("😀x", 2) => Option.None
    charAtOf("abc", 0 - 1) => Option.None
"#,
        5,
    );
}

/// Recursive indexed character dispatch takes the allocation-free hidden
/// `codeAt` route. Non-ASCII scalars are included so the wasm helper must
/// decode the UTF-8 leading sequence rather than return its first byte.
#[test]
fn indexed_dispatch_decodes_codepoints_without_materializing_characters() {
    assert_all_pass_on_both(
        r#"
fn scoreChar(c: String) -> Int
    match c
        "a" -> 1
        "z" -> 10
        _ -> 100

fn scoreAt(text: String, pos: Int, acc: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(c) -> scoreAt(text, pos + 1, acc + scoreChar(c))

fn score(text: String) -> Int
    scoreAt(text, 0, 0)

fn mutualEven(text: String, pos: Int, acc: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(c) -> mutualOdd(text, pos + 1, acc + scoreChar(c))

fn mutualOdd(text: String, pos: Int, acc: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(c) -> mutualEven(text, pos + 1, acc + scoreChar(c))

verify score
    score("") => 0
    score("az") => 11
    score("aą😀z") => 211

verify mutualEven
    mutualEven("aą😀z", 0, 0) => 211
"#,
        4,
    );
}

/// `String.slice` takes scalar indices with the VM's clamping rules
/// (negatives to 0, past-the-end to the end, `start >= end` empty).
#[test]
fn slice_uses_scalar_indices_with_vm_clamping() {
    assert_all_pass_on_both(
        r#"
fn sliceOf(s: String, a: Int, b: Int) -> String
    ? "Slice of the string from a to b"
    String.slice(s, a, b)

verify sliceOf
    sliceOf("zażółć", 1, 4) => "ażó"
    sliceOf("😀abc", 0, 1) => "😀"
    sliceOf("😀abc", 1, 3) => "ab"
    sliceOf("abc", 2, 99) => "c"
    sliceOf("abc", 0 - 5, 2) => "ab"
    sliceOf("abc", 2, 1) => ""
"#,
        6,
    );
}

/// `String.chars` splits into whole characters, not bytes.
#[test]
fn chars_splits_into_scalar_values() {
    assert_all_pass_on_both(
        r#"
fn charsOf(s: String) -> List<String>
    ? "List of characters"
    String.chars(s)

verify charsOf
    charsOf("aż😀") => ["a", "ż", "😀"]
    charsOf("ab") => ["a", "b"]
    charsOf("") => []
"#,
        3,
    );
}

/// String's code-point API decodes the first Unicode scalar value, not the
/// first UTF-8 byte, and represents empty text explicitly as `Option.None`.
#[test]
fn string_code_point_helpers_decode_scalar_values() {
    assert_all_pass_on_both(
        r#"
fn codeOf(c: String) -> Int
    ? "Unicode scalar code of c."
    Option.withDefault(String.firstCodePoint(c), 0)

fn roundTrip(c: String) -> String
    ? "Encode the decoded code point again."
    match String.fromCodePoint(Option.withDefault(String.firstCodePoint(c), 0))
        Option.Some(out) -> out
        Option.None -> ""

verify codeOf
    codeOf("a") => 97
    codeOf("ż") => 380
    codeOf("…") => 8230
    codeOf("😀") => 128512

verify roundTrip
    roundTrip("a") => "a"
    roundTrip("ż") => "ż"
    roundTrip("…") => "…"
    roundTrip("😀") => "😀"
"#,
        8,
    );
}
