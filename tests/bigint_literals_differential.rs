//! Arbitrary-precision integer LITERALS (the 0.26.0 "Zahlen" feature): an
//! integer literal in source whose magnitude exceeds `i64` is lexed as a
//! `Literal::BigInt` and lowered, on every backend, through the same
//! arbitrary-precision `Int` construction `Int.n("…")` uses.
//!
//! These are VM-vs-wasm-gc DIFFERENTIALS that ALSO pin the exact digits: a big
//! literal that wrapped or truncated on either backend would either diverge
//! (caught by the cross-backend equality) or be wrong on BOTH (caught by the
//! exact-string assertion). A literal in a `verify` expected value and a
//! large-magnitude negative are covered too.

#![cfg(feature = "wasm")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{cleanup, temp_module};

use std::path::PathBuf;
use std::process::Command;

fn run(prefix: &str, source: &str, args: &[&str]) -> (bool, String, String) {
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let path = temp_module(prefix, source);
    let mut cmd = Command::new(aver_bin);
    cmd.current_dir(&repo_root);
    for a in args {
        cmd.arg(a);
    }
    cmd.arg(&path);
    let out = cmd.output().expect("aver executes");
    cleanup(&path);
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stdout).trim().to_string(),
        String::from_utf8_lossy(&out.stderr).trim().to_string(),
    )
}

/// A program that exercises big-int literals across every shape, printing one
/// line per case. The expected digits are pinned independently of the backend.
const BIG_PROGRAM: &str = r#"module Big
    intent = "Arbitrary-precision integer literals across backends."
    exposes [pow2_100, factorial, addOne, twoPow64, negBig, idInt, classify]
    effects [Console]

fn pow2_100() -> Int
    ? "2^100 — larger than i64::MAX."
    1267650600228229401496703205376

fn factorial(n: Int) -> Int
    ? "factorial(30) overflows i64 several times over."
    match n < 1
        true  -> 1
        false -> n * factorial(n - 1)

fn addOne(n: Int) -> Int
    ? "Crosses i64::MAX into a big result."
    n + 1

fn twoPow64() -> Int
    ? "2^64 as a literal."
    18446744073709551616

fn negBig() -> Int
    ? "Large-magnitude negative literal (sign via subtraction)."
    -1267650600228229401496703205376

fn idInt(n: Int) -> Int
    ? "Feed a big literal through an Int parameter."
    n

fn classify(n: Int) -> String
    ? "Match a big value via the idiomatic comparison form."
    match n == 1267650600228229401496703205376
        true  -> "is 2^100"
        false -> "other"

fn main() -> Unit
    ! [Console.print]
    Console.print("{pow2_100()}")
    Console.print("{factorial(30)}")
    Console.print("{addOne(9223372036854775807)}")
    Console.print("{twoPow64()}")
    Console.print("{negBig()}")
    Console.print("{idInt(1267650600228229401496703205376)}")
    Console.print(classify(1267650600228229401496703205376))
    Console.print(classify(5))
"#;

const EXPECTED: &str = "1267650600228229401496703205376\n\
265252859812191058636308480000000\n\
9223372036854775808\n\
18446744073709551616\n\
-1267650600228229401496703205376\n\
1267650600228229401496703205376\n\
is 2^100\n\
other";

#[test]
fn big_literals_digit_exact_on_vm_and_wasm_gc() {
    let (vm_ok, vm_out, vm_err) = run("biglit-vm", BIG_PROGRAM, &["run"]);
    assert!(vm_ok, "VM run failed:\n{vm_err}");
    let (wg_ok, wg_out, wg_err) = run("biglit-wg", BIG_PROGRAM, &["run", "--wasm-gc"]);
    assert!(wg_ok, "wasm-gc run failed:\n{wg_err}");

    // 1) The two backends agree (no backend silently wrapped).
    assert_eq!(
        vm_out, wg_out,
        "VM-vs-wasm-gc DIVERGENCE on big-int literals.\n  VM     = {vm_out:?}\n  wasm-gc= {wg_out:?}"
    );
    // 2) The shared answer is the exact arbitrary-precision value (neither
    //    backend wrapped identically).
    assert_eq!(vm_out, EXPECTED, "big-int literal value is not digit-exact");
}

/// A `>i64` literal used as a `verify` expected value must check on the VM,
/// including the i64::MIN boundary (its magnitude `9223372036854775808`
/// overflows i64) and a computed bignum equal to a written literal.
#[test]
fn big_literals_in_verify_pass_on_vm() {
    const SRC: &str = r#"module V
    intent = "Big literals as verify expected values, incl. i64 boundaries."
    exposes [pow, addOne, i64MaxPlus1, i64MinValue]

fn pow() -> Int
    ? "2^100."
    1267650600228229401496703205376

fn addOne(n: Int) -> Int
    ? "n + 1."
    n + 1

fn i64MaxPlus1() -> Int
    ? "i64::MAX + 1, as a literal."
    9223372036854775808

fn i64MinValue() -> Int
    ? "i64::MIN, whose magnitude overflows i64 (sign via subtraction)."
    -9223372036854775808

verify pow
    pow() => 1267650600228229401496703205376

verify addOne
    addOne(9223372036854775807) => 9223372036854775808

verify i64MaxPlus1
    i64MaxPlus1() => 9223372036854775808

verify i64MinValue
    i64MinValue() => -9223372036854775808
"#;
    let (ok, out, err) = run("biglit-verify", SRC, &["verify"]);
    assert!(ok, "verify run failed:\nstdout={out}\nstderr={err}");
    assert!(
        out.contains("0 failed"),
        "expected all big-literal verify cases to pass, got:\n{out}"
    );
}

/// A big-int literal buried inside string interpolation, in a module where NO
/// function signature mentions `Int`. On wasm-gc this still drives the
/// `Int.fromString` lowering, whose `Result<Int,String>` type slot must be
/// interned by the type-discovery walk — and that walk must recurse INTO
/// interpolation (mirroring the bignum gate). A miss is a
/// `Result<Int,String> slot wasn't registered` wasm-gc validation failure while
/// the VM prints fine; caught here by the cross-backend equality. (Regression
/// for the type-discovery / gate asymmetry found by the adversarial probe.)
#[test]
fn big_literal_in_interpolation_without_int_signature_matches() {
    const SRC: &str = r#"module Nest
    intent = "The only integer anywhere is a big literal inside interpolation."
    exposes [render, label]
    effects [Console]

fn render() -> String
    ? "Big literal inside interpolation; return type is String (no Int token)."
    "v={1267650600228229401496703205376}"

fn label() -> String
    ? "A second interpolation site with a big literal mid-string."
    "lo {18446744073709551616} hi"

fn main() -> Unit
    ! [Console.print]
    Console.print(render())
    Console.print(label())
"#;
    let (vm_ok, vm_out, vm_err) = run("biglit-interp-vm", SRC, &["run"]);
    assert!(vm_ok, "VM run failed:\n{vm_err}");
    let (wg_ok, wg_out, wg_err) = run("biglit-interp-wg", SRC, &["run", "--wasm-gc"]);
    assert!(
        wg_ok,
        "wasm-gc run failed (Result<Int,String> slot likely unregistered):\n{wg_err}"
    );
    assert_eq!(
        vm_out, wg_out,
        "VM-vs-wasm-gc DIVERGENCE on interpolated big-int literals.\n  VM     = {vm_out:?}\n  wasm-gc= {wg_out:?}"
    );
    assert_eq!(
        vm_out, "v=1267650600228229401496703205376\nlo 18446744073709551616 hi",
        "interpolated big-int literal value is not digit-exact"
    );
}

/// A big-int literal in PATTERN position is unsupported and must be a clean
/// parse error (never a panic or a runtime trap) — the idiomatic form is a
/// comparison match (`match n == <value>`), which is exercised above.
#[test]
fn big_literal_pattern_is_a_clean_error() {
    const SRC: &str = r#"module P
    exposes [f]

fn f(n: Int) -> Int
    match n
        1267650600228229401496703205376 -> 1
        _ -> 0
"#;
    let (ok, out, err) = run("biglit-badpat", SRC, &["check"]);
    assert!(!ok, "expected a big-int literal pattern to be rejected");
    let combined = format!("{out}\n{err}");
    assert!(
        combined.contains("64 bits") || combined.contains("comparison"),
        "expected a helpful 'literal patterns beyond 64 bits' error, got:\n{combined}"
    );
}

#[test]
fn limb_division_matches_euclidean_bigint_arithmetic() {
    use num_bigint::BigInt;
    use num_traits::{Signed, Zero};

    let magnitudes = [
        BigInt::zero(),
        BigInt::from(1),
        BigInt::from(1) << 63,
        (BigInt::from(1) << 127) + (BigInt::from(1) << 64) + 9,
        (BigInt::from(1) << 256) - 1,
        BigInt::from(4_294_967_295u64) * (BigInt::from(1) << 32) - 1,
    ];
    let mut source = String::from(
        r#"module WordDivision
    effects [Console.print]

fn quotient(a: Int, d: Int) -> Int
    Result.withDefault(Int.div(a, d), 0)

fn remainder(a: Int, d: Int) -> Int
    Result.withDefault(Int.mod(a, d), 0)

fn report(a: Int, d: Int) -> Unit
    ! [Console.print]
    Console.print("{quotient(a, d)},{remainder(a, d)}")

fn main() -> Unit
    ! [Console.print]
"#,
    );
    let mut expected = Vec::new();
    for magnitude in magnitudes {
        for divisor in [
            1u64,
            2,
            3,
            4_294_967_295,
            4_294_967_296,
            4_294_967_297,
            9_223_372_036_854_775_811,
            u64::MAX,
        ]
        .map(BigInt::from)
        .into_iter()
        .chain([
            (BigInt::from(1) << 127) - 1,
            (BigInt::from(1) << 128) + 3,
            (BigInt::from(1) << 255) + 1,
        ]) {
            for a_sign in [-1, 1] {
                for d_sign in [-1, 1] {
                    let a: BigInt = &magnitude * a_sign;
                    let d: BigInt = &divisor * d_sign;
                    let mut q: BigInt = &a / &d;
                    let mut r: BigInt = &a % &d;
                    if r.is_negative() {
                        r += d.abs();
                        q -= d.signum();
                    }
                    source.push_str(&format!("    report({a}, {d})\n"));
                    expected.push(format!("{q},{r}"));
                }
            }
        }
    }
    let expected = expected.join("\n");
    for (prefix, args) in [
        ("word-div-vm", vec!["run"]),
        ("word-div-wasm", vec!["run", "--wasm-gc"]),
    ] {
        let (ok, output, err) = run(prefix, &source, &args);
        assert!(ok, "{prefix}: {err}");
        assert_eq!(output, expected, "{prefix}: {err}");
    }
}

#[test]
fn large_integer_division_fits_the_default_verify_budget() {
    // Construct powers directly so decimal parsing is not the measured cost.
    let source = r#"module Halving
    effects []

fn power(n: Int) -> Int
    Result.withDefault(Bits.shiftLeft(1, n), 0)

fn halveMany(value: Int, count: Int) -> Int
    match count <= 0
        true -> value
        false -> halveMany(Int.div(value, 2), count - 1)

verify halveMany
    halveMany(power(4096) - 1, 64) => power(4032) - 1
    halveMany(1 - power(4096), 64) => 0 - power(4032)

fn quotient(a: Int, b: Int) -> Int
    Result.withDefault(Int.div(a, b), 0)

verify quotient
    quotient(power(4096) - 1, power(4095) + 1) => 1
    quotient(1 - power(4096), power(4095) + 1) => -2
"#;
    for (prefix, args) in [
        ("large-halving-vm", vec!["verify"]),
        ("large-halving-wasm", vec!["verify", "--wasm-gc"]),
    ] {
        let (ok, output, err) = run(prefix, source, &args);
        assert!(ok, "{prefix}: {output}\n{err}");
    }
}
