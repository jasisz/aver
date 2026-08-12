//! Specification of the `Bits` namespace — a bit-level VIEW of `Int`.
//!
//! Everything here pins one claim: `Bits` reads an ordinary mathematical
//! `Int` as an INFINITE two's-complement bit sequence for the duration of one
//! call, and hands back an ordinary mathematical `Int`. There is no bit-vector
//! type, no machine word, no implicit width and no wraparound anywhere.
//!
//! Three layers, because three things can independently drift:
//!
//! 1. **Values** — every case the specification names, run on the VM.
//! 2. **Algebra** — the identities that MAKE it two's complement rather than
//!    some other convention (`not x == -x - 1`, `and(-1, x) == x`,
//!    `shiftLeft(x, n) == x * 2^n`, `0 <= low(x, w) < 2^w`). A value test
//!    passes for the wrong reason; these do not.
//! 3. **Typing** — a syntactic non-negative literal count discharges the
//!    error and the call types as plain `Int`; anything else keeps
//!    `Result<Int, String>`. That boundary is the whole literal-discharge
//!    rule, and it is easy to widen by accident.
//!
//! Cross-backend agreement lives with each backend's own differential suite
//! (`rust_codegen_differential.rs`, `wasm_gc_spec.rs`, `proof_spec`), so a
//! backend that drifts fails next to its own machinery rather than here.

use std::process::Command;

fn run_source(label: &str, src: &str) -> String {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, src).expect("write entry");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .output()
        .expect("run aver");
    assert!(
        output.status.success(),
        "{label} failed to run:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout)
        .trim_end()
        .to_string()
}

/// Wrap a list of `(label, expression)` pairs into a program that prints each
/// on its own line, so one process run covers a whole table.
fn print_all(label: &str, exprs: &[&str]) -> Vec<String> {
    let mut src = String::from("fn main() -> Unit\n    ! [Console.print]\n");
    for e in exprs {
        src.push_str(&format!("    Console.print(\"{{{e}}}\")\n"));
    }
    run_source(label, &src)
        .lines()
        .map(|l| l.to_string())
        .collect()
}

/// Type-check by running: `aver run` refuses a program with a type error, and
/// unlike `aver check` it does not also enforce the style gates (module
/// header, verify blocks) that would mask WHICH problem a fixture hit.
fn typecheck_source(src: &str) -> (bool, String) {
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("main.av");
    std::fs::write(&entry, src).expect("write entry");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .output()
        .expect("run aver");
    (
        output.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        ),
    )
}

// ─── Layer 1: the specified values ──────────────────────────────────────────

#[test]
fn pointwise_operations_match_the_specification() {
    let got = print_all(
        "pointwise",
        &[
            "Bits.and(6, 3)",
            "Bits.or(6, 3)",
            "Bits.xor(6, 3)",
            // All-ones is the `and` identity and the `or` annihilator —
            // this is what "infinitely many leading ones" MEANS.
            "Bits.and(-1, 42)",
            "Bits.or(-1, 42)",
            "Bits.not(0)",
            "Bits.not(-1)",
            "Bits.not(Bits.not(123))",
        ],
    );
    assert_eq!(got, ["2", "7", "5", "42", "-1", "-1", "0", "123"]);
}

#[test]
fn shifts_and_low_match_the_specification() {
    let got = print_all(
        "shifts",
        &[
            // Past the i64 cliff: exact, not truncated to 64 bits.
            "Bits.shiftLeft(1, 100)",
            // ARITHMETIC right shift: the sign tail shifts in, so this is
            // -2 and never -1.
            "Bits.shiftRight(-3, 1)",
            "Bits.low(257, 8)",
            "Bits.low(-1, 8)",
            "Bits.low(123, 0)",
        ],
    );
    assert_eq!(
        got,
        ["1267650600228229401496703205376", "-2", "1", "255", "0"]
    );
}

#[test]
fn large_operands_of_both_signs_stay_exact() {
    // Straddles the Small/Big representation seam in both directions: an
    // operand past i64, a result past i64, and a result that comes back
    // INTO i64 range from Big operands.
    let got = print_all(
        "large",
        &[
            "Bits.xor(Bits.shiftLeft(1, 100), Bits.shiftLeft(1, 100))",
            "Bits.or(Bits.shiftLeft(1, 100), 1)",
            "Bits.and(Bits.not(Bits.shiftLeft(1, 100)), Bits.shiftLeft(1, 100))",
            "Bits.not(Bits.shiftLeft(1, 100))",
            "Bits.shiftRight(Bits.not(Bits.shiftLeft(1, 100)), 100)",
            "Bits.low(Bits.not(Bits.shiftLeft(1, 100)), 8)",
        ],
    );
    assert_eq!(
        got,
        [
            "0",
            "1267650600228229401496703205377",
            "0",
            "-1267650600228229401496703205377",
            "-2",
            "255"
        ]
    );
}

// ─── Layer 2: the algebra that fixes the convention ─────────────────────────

/// Each case is `Bool` so a failure names the LAW, not a number. The values
/// deliberately include both signs, zero, and magnitudes on both sides of the
/// i64 boundary.
#[test]
fn algebraic_identities_hold() {
    let src = r#"module BitsLaws
    intent =
        "The identities that make this infinite two's complement rather than"
        "some other convention."

fn complementIsNegateMinusOne(x: Int) -> Bool
    ? "Complementing every bit is exactly -x - 1 over the integers."
    Bits.not(x) == 0 - x - 1

fn complementIsAnInvolution(x: Int) -> Bool
    ? "Complementing twice returns the original value."
    Bits.not(Bits.not(x)) == x

fn allOnesIsTheAndIdentity(x: Int) -> Bool
    ? "-1 is all ones, so conjunction with it changes nothing."
    Bits.and(-1, x) == x

fn zeroIsTheOrIdentity(x: Int) -> Bool
    ? "0 is all zeroes, so disjunction with it changes nothing."
    Bits.or(0, x) == x

fn xorWithSelfIsZero(x: Int) -> Bool
    ? "Every bit cancels against itself."
    Bits.xor(x, x) == 0

fn xorWithAllOnesIsComplement(x: Int) -> Bool
    ? "Exclusive-or against all ones flips every bit."
    Bits.xor(-1, x) == Bits.not(x)

fn shiftLeftIsMultiplication(x: Int, n: Int) -> Bool
    ? "Shifting up by n multiplies by two to the n."
    Bits.shiftLeft(x, 3) == x * 8

fn shiftRightIsFloorDivision(x: Int) -> Bool
    ? "Shifting down by n floors the division by two to the n."
    Bits.shiftRight(x, 3) == Int.div(x, 8)

fn lowIsNonNegative(x: Int) -> Bool
    ? "The low bits always name a non-negative value."
    Bits.low(x, 8) >= 0

fn lowIsBelowTheWidth(x: Int) -> Bool
    ? "The low w bits name a value below two to the w."
    Bits.low(x, 8) < 256

fn lowOfZeroWidthIsZero(x: Int) -> Bool
    ? "A zero-bit window carries no value at all."
    Bits.low(x, 0) == 0

verify complementIsNegateMinusOne
    complementIsNegateMinusOne(0) => true
    complementIsNegateMinusOne(1) => true
    complementIsNegateMinusOne(-1) => true
    complementIsNegateMinusOne(123456789) => true
    complementIsNegateMinusOne(-123456789) => true

verify complementIsAnInvolution
    complementIsAnInvolution(0) => true
    complementIsAnInvolution(42) => true
    complementIsAnInvolution(-42) => true

verify allOnesIsTheAndIdentity
    allOnesIsTheAndIdentity(0) => true
    allOnesIsTheAndIdentity(42) => true
    allOnesIsTheAndIdentity(-42) => true

verify zeroIsTheOrIdentity
    zeroIsTheOrIdentity(0) => true
    zeroIsTheOrIdentity(42) => true
    zeroIsTheOrIdentity(-42) => true

verify xorWithSelfIsZero
    xorWithSelfIsZero(0) => true
    xorWithSelfIsZero(42) => true
    xorWithSelfIsZero(-42) => true

verify xorWithAllOnesIsComplement
    xorWithAllOnesIsComplement(0) => true
    xorWithAllOnesIsComplement(42) => true
    xorWithAllOnesIsComplement(-42) => true

verify shiftLeftIsMultiplication
    shiftLeftIsMultiplication(0, 3) => true
    shiftLeftIsMultiplication(7, 3) => true
    shiftLeftIsMultiplication(-7, 3) => true

verify shiftRightIsFloorDivision
    shiftRightIsFloorDivision(0) => true
    shiftRightIsFloorDivision(7) => true
    shiftRightIsFloorDivision(-7) => true

verify lowIsNonNegative
    lowIsNonNegative(0) => true
    lowIsNonNegative(257) => true
    lowIsNonNegative(-1) => true
    lowIsNonNegative(-257) => true

verify lowIsBelowTheWidth
    lowIsBelowTheWidth(0) => true
    lowIsBelowTheWidth(257) => true
    lowIsBelowTheWidth(-1) => true

verify lowOfZeroWidthIsZero
    lowOfZeroWidthIsZero(0) => true
    lowOfZeroWidthIsZero(123) => true
    lowOfZeroWidthIsZero(-123) => true
"#;
    let dir = tempfile::tempdir().expect("tempdir");
    let entry = dir.path().join("laws.av");
    std::fs::write(&entry, src).expect("write laws.av");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("verify")
        .arg(&entry)
        .output()
        .expect("run aver verify");
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        output.status.success() && combined.contains("0 failed"),
        "Bits algebra broke:\n{combined}"
    );
}

// ─── Layer 3: the partiality boundary ───────────────────────────────────────

#[test]
fn a_dynamic_negative_count_is_a_catchable_error() {
    // A negative count must be `Result.Err` — never a panic, never a silent
    // direction flip, never a clamp to zero, and never the host language's
    // behaviour for an oversized or negative shift.
    let src = r#"fn shift(x: Int, n: Int) -> String
    ? "Reports how a caller-supplied shift amount was treated."
    match Bits.shiftLeft(x, n)
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn down(x: Int, n: Int) -> String
    ? "Reports how a caller-supplied right-shift amount was treated."
    match Bits.shiftRight(x, n)
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn width(x: Int, w: Int) -> String
    ? "Reports how a caller-supplied bit width was treated."
    match Bits.low(x, w)
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn main() -> Unit
    ! [Console.print]
    Console.print(shift(1, 4))
    Console.print(shift(1, -1))
    Console.print(down(-3, 1))
    Console.print(down(-3, -1))
    Console.print(width(-1, 8))
    Console.print(width(-1, -1))
"#;
    let got = run_source("dynamic counts", src);
    assert_eq!(
        got,
        "ok 16\n\
         err negative shift count\n\
         ok -2\n\
         err negative shift count\n\
         ok 255\n\
         err negative bit width"
    );
}

#[test]
fn a_literal_non_negative_count_discharges_to_a_plain_int() {
    // The discharged call has NO Result to unwrap: binding it to an `Int`
    // annotation must type-check. If the discharge stopped firing this is a
    // type error, which is exactly the regression worth catching.
    let src = r#"fn packed(checksum: Int, value: Int) -> Int
    ? "A Bech32-style polymod step, entirely on discharged literal widths."
    top: Int = Bits.shiftRight(checksum, 25)
    shifted: Int = Bits.low(Bits.shiftLeft(checksum, 5), 25)
    mixed: Int = Bits.xor(shifted, value)
    top + mixed

fn zeroWidthDischarges(x: Int) -> Int
    ? "A zero-bit window is well defined, so a 0 literal discharges too."
    Bits.low(x, 0)

fn main() -> Unit
    ! [Console.print]
    Console.print("{packed(1, 0)} {zeroWidthDischarges(7)}")
"#;
    let (ok, out) = typecheck_source(src);
    assert!(ok, "literal counts should discharge:\n{out}");
    assert_eq!(run_source("discharged", src), "32 0");
}

#[test]
fn a_non_literal_count_keeps_the_result_type() {
    // The discharge is SYNTACTIC and narrow by design: not constant
    // propagation, not refinement inference, not dependent typing. A named
    // constant and a constant-valued expression both keep `Result`, so
    // annotating them as `Int` must be rejected.
    for (label, count) in [
        ("identifier", "amount"),
        ("constant expression", "4 + 1"),
        ("negated literal", "-1"),
    ] {
        let src = format!(
            r#"fn shifted(value: Int, amount: Int) -> Int
    ? "Tries to treat a non-literal count as if it had discharged."
    result: Int = Bits.shiftLeft(value, {count})
    result

fn main() -> Unit
    ! [Console.print]
    Console.print("{{shifted(1, 2)}}")
"#
        );
        let (ok, out) = typecheck_source(&src);
        assert!(
            !ok,
            "a {label} count must keep Result<Int, String>, but it type-checked:\n{out}"
        );
        assert!(
            out.contains("Result<Int, String>"),
            "a {label} count must fail because the Result SURVIVED, not for some \
             unrelated reason:\n{out}"
        );
    }
}

#[test]
fn bits_is_a_namespace_and_not_a_type() {
    // The whole design rests on this: `Bits` names a way of READING an Int,
    // so it must not be usable in type position. #861 made an undeclared
    // type name an error, which is what gives this test its teeth.
    let src = r#"fn widen(x: Bits) -> Int
    ? "Tries to use the namespace as if it were a type."
    0

fn main() -> Unit
    ! [Console.print]
    Console.print("{widen(1)}")
"#;
    let (ok, out) = typecheck_source(src);
    assert!(
        !ok,
        "`Bits` must not be usable as a type, but this checked:\n{out}"
    );
}

#[test]
fn a_user_module_named_bits_still_shadows_the_namespace() {
    // Bit-twiddling code predating this namespace lives in project-local
    // `Bits` modules — that is exactly what issue #860 was written against.
    // A `depends [Bits]` module must keep winning, or adding the namespace
    // silently rewires existing programs to different arithmetic.
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(
        dir.path().join("bits.av"),
        "module Bits\n    intent = \"project-local Bits\"\n    exposes [fold]\n    effects []\n\nfn fold(a: Int, b: Int) -> Int\n    ? \"A deliberately wrong stand-in, so shadowing is observable.\"\n    a + b\n",
    )
    .expect("write bits.av");
    let entry = dir.path().join("main.av");
    std::fs::write(
        &entry,
        "module Main\n    intent = \"use the project-local Bits\"\n    depends [Bits]\n    effects [Console.print]\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"{Bits.fold(6, 3)} {Bits.xor(6, 3)}\")\n",
    )
    .expect("write main.av");

    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&entry)
        .arg("--module-root")
        .arg(dir.path())
        .output()
        .expect("run aver");
    assert!(
        output.status.success(),
        "shadowed run failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    // 9 is the module's `a + b`; 5 is the builtin's xor. The module wins for
    // its own name and the namespace still answers for the rest.
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim_end(),
        "9 5",
        "a project-local `Bits` module must still shadow the builtin namespace"
    );
}

/// A module named after a builtin namespace cannot RE-DEFINE one of that
/// namespace's own method names. This is not new and not specific to `Bits`:
/// `module Bool` has never been able to define `fn and`. It is pinned here
/// because adding `Bits` extends that rule to seven more names, which is the
/// one way this change can break existing source — and the diagnostic a user
/// hits should be the same familiar one, not something `Bits`-specific.
#[test]
fn redefining_a_namespace_method_collides_the_same_way_for_bits_and_bool() {
    let collide = |module: &str, method: &str| -> String {
        let dir = tempfile::tempdir().expect("tempdir");
        std::fs::write(
            dir.path().join(format!("{}.av", module.to_lowercase())),
            format!(
                "module {module}\n    intent = \"collides with the builtin\"\n    exposes [{method}]\n    effects []\n\nfn {method}(a: Int, b: Int) -> Int\n    ? \"Re-defines a name the namespace already owns.\"\n    a + b\n"
            ),
        )
        .expect("write module");
        let entry = dir.path().join("main.av");
        std::fs::write(
            &entry,
            format!(
                "module Main\n    intent = \"use it\"\n    depends [{module}]\n    effects [Console.print]\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"{{{module}.{method}(6, 3)}}\")\n"
            ),
        )
        .expect("write main.av");
        let output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("run")
            .arg(&entry)
            .arg("--module-root")
            .arg(dir.path())
            .output()
            .expect("run aver");
        assert!(!output.status.success(), "{module}.{method} should collide");
        String::from_utf8_lossy(&output.stderr).to_string()
    };

    let bool_error = collide("Bool", "and");
    let bits_error = collide("Bits", "xor");
    assert!(
        bool_error.contains("already defined in this module"),
        "unexpected Bool collision diagnostic:\n{bool_error}"
    );
    assert!(
        bits_error.contains("already defined in this module"),
        "`Bits` must collide with the SAME diagnostic every other builtin \
         namespace produces, not a bespoke one:\n{bits_error}"
    );
}

// ─── The rejected-operator pattern ──────────────────────────────────────────

/// Every operator Aver rejects has a named function in its place — that is the
/// pattern issue #860 was written against, and it only WORKS if the compiler
/// says so. `^` used to lex as `Unknown character: '^'`, which tells a reader
/// nothing about `Bits.xor`, so the rule read as an omission rather than a
/// redirection. Each of these must now name its replacement.
///
/// `<<` / `>>` are checked here too because they cannot be handled in the
/// lexer: `<` and `>` are real tokens and `>>` closes a nested generic. They
/// are caught in the expression parser instead, and
/// `nested_generics_still_parse` is the guard that this did not break
/// `Map<String, List<Int>>`.
#[test]
fn every_rejected_operator_names_its_replacement() {
    for (op, expected) in [
        ("a ^ b", "Bits.xor(a, b)"),
        ("a & b", "Bits.and(a, b)"),
        ("a | b", "Bits.or(a, b)"),
        ("a && b", "Bool.and(a, b)"),
        ("a || b", "Bool.or(a, b)"),
        ("0 - a", ""),
        ("a % b", "Int.mod(a, b)"),
        ("a << b", "Bits.shiftLeft(x, n)"),
        ("a >> b", "Bits.shiftRight(x, n)"),
    ] {
        if expected.is_empty() {
            continue;
        }
        let src = format!(
            "fn f(a: Int, b: Int) -> Int\n    ? \"probe\"\n    {op}\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"{{f(1, 2)}}\")\n"
        );
        let (ok, out) = typecheck_source(&src);
        assert!(!ok, "`{op}` should be rejected, but it compiled:\n{out}");
        assert!(
            out.contains("does not exist in Aver"),
            "`{op}` must be reported as a REJECTED OPERATOR, not as an unknown \
             character or a bare parse error:\n{out}"
        );
        assert!(
            out.contains(expected),
            "`{op}` must name `{expected}` as its replacement:\n{out}"
        );
    }
}

/// The unary complement, which has no binary form to probe above.
#[test]
fn the_complement_operator_names_bits_not() {
    let src = "fn f(a: Int) -> Int\n    ? \"probe\"\n    ~a\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"{f(1)}\")\n";
    let (ok, out) = typecheck_source(src);
    assert!(!ok, "`~a` should be rejected:\n{out}");
    assert!(
        out.contains("Bits.not(x)") && out.contains("does not exist in Aver"),
        "`~` must name `Bits.not`:\n{out}"
    );
}

/// The shift diagnostic keys on two ADJACENT `<` / `>` tokens, and `>>` is
/// also how a nested generic closes. This is the guard that the parser arm
/// cannot reach a type annotation — including three closing brackets in a
/// row — and that ordinary comparisons are untouched.
#[test]
fn nested_generics_and_comparisons_still_parse() {
    let src = r#"module Generics
    intent = "Nested generics and comparisons must survive the shift diagnostic"
    effects [Console.print]

fn nested(m: Map<String, List<Int>>) -> List<List<Int>>
    ? "A doubly-nested generic in both a parameter and a return type."
    Map.values(m)

fn deeper(x: Result<Option<List<Int>>, String>) -> Int
    ? "Three closing angle brackets in a row."
    match x
        Result.Ok(_) -> 1
        Result.Err(_) -> 0

fn compares(a: Int, b: Int) -> Bool
    ? "Ordinary comparisons must be untouched."
    match a > b
        true -> a < b
        false -> a >= b

fn main() -> Unit
    ! [Console.print]
    m: Map<String, List<Int>> = {"a" => [1, 2]}
    n: List<List<Int>> = nested(m)
    Console.print("{List.len(n)} {deeper(Result.Ok(Option.None))} {compares(2, 1)}")
"#;
    assert_eq!(run_source("nested generics", src), "1 1 false");
}
