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
        let failures: Vec<_> = results
            .iter()
            .flat_map(|result| result.failures.iter())
            .collect();
        let outcomes: Vec<_> = results
            .iter()
            .flat_map(|result| result.case_results.iter().map(|case| &case.outcome))
            .collect();
        assert_eq!(
            (passed, failed, skipped),
            (expected_passed, 0, 0),
            "[{backend}] expected {expected_passed}/0/0 passed/failed/skipped, got {passed}/{failed}/{skipped}; failures={failures:?}; outcomes={outcomes:?}\n--- source ---\n{source}"
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

/// `String.len` of an indexed String reads the index inline: no table for
/// an ASCII (or empty) String, one boundary per character plus one
/// otherwise. A loop that hands itself a shorter String sees its length.
#[test]
fn indexed_len_reads_the_index_and_follows_a_new_string() {
    assert_all_pass_on_both(
        r#"
fn count(text: String, pos: Int, acc: Int) -> Int
    match pos < String.len(text)
        false -> acc
        true -> match String.charAt(text, pos)
            Option.None -> 0 - 1
            Option.Some(_) -> count(text, pos + 1, acc + 1)

fn total(text: String) -> String
    before = String.len(text)
    n = count(text, 0, 0)
    "{before}/{n}/{String.len(text)}"

fn shrink(text: String, pos: Int, acc: String) -> String
    match pos < String.len(text)
        false -> acc
        true -> match String.charAt(text, pos)
            Option.None -> acc
            Option.Some(" ") -> shrink(text, pos + 1, "{acc}_{String.len(text)},")
            Option.Some(c) -> shrink(String.slice(text, 1, String.len(text)), pos, "{acc}{c}{String.len(text)},")

verify total
    total("") => "0/0/0"
    total("hello") => "5/5/5"
    total("aą😀z") => "4/4/4"

verify shrink
    shrink("a b", 0, "") => "a3,_2,b2,"
    shrink("ą 😀z", 0, "") => "ą4,_3,😀3,z2,"
"#,
        5,
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

/// Indexed reads over ASCII strings (which get no index table on wasm-gc,
/// position `i` being byte `i`) and over strings with wider characters
/// answer alike: the character itself, its codepoint in a dispatch, a slice
/// at clamped positions, and a slice compared in place with `==` and `!=`,
/// on either side.
#[test]
fn indexed_reads_agree_on_ascii_and_wider_text() {
    assert_all_pass_on_both(
        r#"
fn kept(s: String, i: Int, acc: String) -> String
    ? "Every character from position i, each but a b once more"
    match String.charAt(s, i)
        Option.None -> acc
        Option.Some(c) -> kept(s, i + 1, keep(c, acc))

fn keep(c: String, acc: String) -> String
    ? "c added to acc, twice unless it is a b"
    match c == "b"
        true -> acc + c
        false -> acc + c + c

fn countB(s: String, i: Int, acc: Int) -> Int
    ? "The b's from position i, matched as characters"
    match String.charAt(s, i)
        Option.None -> acc
        Option.Some("b") -> countB(s, i + 1, acc + 1)
        Option.Some -> countB(s, i + 1, acc)

fn pairs(s: String, i: Int, acc: String) -> String
    ? "Overlapping two-character slices, the last one cut short"
    match String.charAt(s, i)
        Option.None -> acc
        Option.Some -> pairs(s, i + 1, acc + "[" + String.slice(s, i, i + 2) + "]")

fn find(s: String, i: Int, t: String) -> Int
    ? "The first position from i where t stands, or -1"
    match String.charAt(s, i)
        Option.None -> -1
        Option.Some -> match String.slice(s, i, i + String.len(t)) == t
            true -> i
            false -> find(s, i + 1, t)

fn misses(s: String, i: Int, t: String, acc: Int) -> Int
    ? "Positions from i where t does not stand, compared the other way round"
    match String.charAt(s, i)
        Option.None -> acc
        Option.Some -> match t != String.slice(s, i, i + String.len(t))
            true -> misses(s, i + 1, t, acc + 1)
            false -> misses(s, i + 1, t, acc)

fn edges(s: String, i: Int, acc: String) -> String
    ? "Slices at negative, reversed and past-the-end positions"
    match String.charAt(s, i)
        Option.None -> acc
        Option.Some -> edges(s, i + 1, acc + String.slice(s, i - 9, i) + "|" + String.slice(s, i + 3, i) + "|" + String.slice(s, i, i + 99) + ";")

verify kept
    kept("abc", 0, "") => "aabcc"
    kept("żbł😀", 0, "") => "żżbłł😀😀"
    kept("abc", -1, "") => ""
    kept("", 0, "") => ""

verify countB
    countB("abba", 0, 0) => 2
    countB("ąbżb😀b", 0, 0) => 3
    countB("abba", 4, 0) => 0

verify pairs
    pairs("abc", 0, "") => "[ab][bc][c]"
    pairs("ż😀x", 0, "") => "[ż😀][😀x][x]"
    pairs("", 0, "") => ""

verify find
    find("abcabc", 1, "ca") => 2
    find("żółwżółw", 1, "wż") => 3
    find("abc", 0, "") => 0
    find("abc", 0, "cd") => -1
    find("ab", 0, "abc") => -1

verify misses
    misses("abab", 0, "ab", 0) => 2
    misses("αβαβ", 0, "βα", 0) => 3
    misses("abab", 0, "", 0) => 0

verify edges
    edges("abc", 0, "") => "||abc;a||bc;ab||c;"
    edges("ż😀", 0, "") => "||ż😀;ż||😀;"
"#,
        20,
    );
}

/// `String.split` on a one-byte separator (searched for from the end on
/// wasm-gc) gives the parts the search from the start gives, empty ones
/// included; a longer separator keeps the search from the start; the empty
/// separator gives an empty part, every character whole, and an empty part
/// (wasm-gc used to give bytes, with no empty parts).
#[test]
fn split_on_one_byte_keeps_every_part() {
    assert_all_pass_on_both(
        r#"
fn parts(s: String, sep: String) -> List<String>
    ? "The parts of s between separators"
    String.split(s, sep)

verify parts
    parts("", "\n") => [""]
    parts("\n", "\n") => ["", ""]
    parts("a", "\n") => ["a"]
    parts("a\n\nb\n", "\n") => ["a", "", "b", ""]
    parts("\na", "\n") => ["", "a"]
    parts("αβ,γ,", ",") => ["αβ", "γ", ""]
    parts("a—b—", "—") => ["a", "b", ""]
    parts("aaa", "aa") => ["", "a"]
    parts("abcabc", "bc") => ["a", "a", ""]
    parts("ab", "") => ["", "a", "b", ""]
    parts("", "") => ["", ""]
    parts("ż😀", "") => ["", "ż", "😀", ""]
"#,
        12,
    );
}

/// A loop over `String.chars` that reads a character's first code point
/// takes the character's code at the cursor instead, with no one-character
/// string; under `Option.withDefault` with a literal default no `Option`
/// either.
#[test]
fn first_code_point_of_each_character_reads_the_cursor() {
    assert_all_pass_on_both(
        r#"
fn ofChars(cs: List<String>, acc: Int) -> Int
    ? "A hash over code points"
    match cs
        [] -> acc
        [c, ..rest] -> ofChars(rest, acc * 31 + Option.withDefault(String.firstCodePoint(c), 0))

fn ofText(s: String) -> Int
    ? "The hash of a string"
    ofChars(String.chars(s), 17)

fn codes(cs: List<String>, acc: List<Option<Int>>) -> List<Option<Int>>
    ? "Every first code point, last first"
    match cs
        [] -> acc
        [c, ..rest] -> codes(rest, List.prepend(String.firstCodePoint(c), acc))

fn codesOf(s: String) -> List<Option<Int>>
    ? "The first code point of every character, last first"
    codes(String.chars(s), [])

verify ofText
    ofText("") => 17
    ofText("ab") => (17 * 31 + 97) * 31 + 98
    ofText("ż😀") => (17 * 31 + 380) * 31 + 128512

verify codesOf
    codesOf("") => []
    codesOf("aż😀") => [Option.Some(128512), Option.Some(380), Option.Some(97)]
"#,
        5,
    );
}
