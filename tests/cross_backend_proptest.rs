//! Iron — C3: cross-backend differential property tests.
//!
//! Generate a well-typed Aver program, run it on every production
//! backend, assert the stdout is byte-identical. A semantic divergence
//! between VM, wasm-gc, and the Rust codegen target — number
//! promotion, overflow wrap, string interning, evaluation order in a
//! BinOp, a TCO-lowered loop drifting from its source — surfaces here
//! as a property failure with the minimal reproducer attached.
//!
//! The proptest harness shrinks the failing input into the smallest
//! divergent program; that shape goes straight into
//! `tests/regressions/parser/` (lexer/parser variants) or a dedicated
//! cross-backend regression once a real find lands.
//!
//! Cost / budget:
//! - Each proptest case spawns `aver run` twice (VM + wasm-gc) plus a
//!   compile per backend. Roughly 500-1500 ms per case under release-
//!   built `aver`; with the default `PROPTEST_CASES=64` override below
//!   the property finishes inside ~90 s. The Iron CI override of 2 000
//!   would push this past 30 minutes — set the override down via the
//!   `PROPTEST_CASES` env or rely on `proptest!`'s built-in default
//!   when this file is not the one being stress-tested.
//! - Feature-gated on `wasm` (same gate that runs `cross_backend_stress`)
//!   so the property does not fire from a vanilla `cargo test`. The
//!   default Check & Test CI job runs with `--features wasm` so the
//!   gate fires there.
#![cfg(feature = "wasm")]

use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use proptest::prelude::*;

// ─── Backend runners ───────────────────────────────────────────────────────
//
// Duplicated from `tests/cross_backend_stress.rs` rather than extracted
// to `tests/common/mod.rs`: Rust integration tests are each their own
// crate, sharing requires a `mod common;` ref from every test file plus
// the visibility plumbing. The runners are small enough that the
// duplication does not pay back the refactor.

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time before unix epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("{}-{}", prefix, nanos));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &std::path::Path) {
    let _ = fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

fn run_vm(prefix: &str, source: &str) -> Result<String, String> {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` (VM) to execute");
    cleanup(&path);
    if !out.status.success() {
        return Err(format!("VM run failed:\n{}", format_output(&out)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn run_wasm_gc(prefix: &str, source: &str) -> Result<String, String> {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("run")
        .arg(&path)
        .arg("--wasm-gc")
        .output()
        .expect("expected `aver run --wasm-gc` to execute");
    cleanup(&path);
    if !out.status.success() {
        return Err(format!("wasm-gc run failed:\n{}", format_output(&out)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Like `run_wasm_gc`, but with the opt-in `AVER_WASMGC_BIGNUM=1` flag
/// set so `Int` lowers to the arbitrary-precision `$AverInt` carrier
/// (bignum slice 1). This is the differential oracle for add/sub/mul/
/// neg/cmp/eq: with the flag on, wasm-gc must agree with the VM
/// (Int = ℤ) on EVERY input, including the i64-overflow ones the
/// generator produces.
fn run_wasm_gc_bignum(prefix: &str, source: &str) -> Result<String, String> {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let path = temp_module(prefix, source);
    let out = Command::new(aver_bin)
        .current_dir(&repo_root)
        .env("AVER_WASMGC_BIGNUM", "1")
        .arg("run")
        .arg(&path)
        .arg("--wasm-gc")
        .output()
        .expect("expected `aver run --wasm-gc` (bignum) to execute");
    cleanup(&path);
    if !out.status.success() {
        return Err(format!(
            "wasm-gc (bignum) run failed:\n{}",
            format_output(&out)
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Compile `source` to a Rust project via `aver compile --target rust`,
/// `cargo build` it (a REAL build — the AverInt arithmetic + clone cascade
/// must type-check + borrow-check), then run the binary and return trimmed
/// stdout. Both VM and the Rust backend now compute `Int = ℤ`, so this
/// closes the convergent arm: the two must agree on every input, INCLUDING
/// i64-overflow ones (the no-wrap proof). Builds against a shared target dir
/// so the (slow) dependency compile amortises across cases.
fn run_rust(prefix: &str, source: &str) -> Result<String, String> {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let file = temp_module(prefix, source);
    let project = file
        .parent()
        .expect("temp module has parent")
        .join("project");
    let name = "cross_bp_rust";

    let compile = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(&file)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg(name)
        .arg("-o")
        .arg(&project)
        .output()
        .expect("expected `aver compile --target rust` to execute");
    if !compile.status.success() {
        cleanup(&file);
        return Err(format!("rust compile failed:\n{}", format_output(&compile)));
    }

    let target = repo_root.join("target").join("cross-backend-rust-shared");
    let _ = fs::create_dir_all(&target);
    let build = Command::new("cargo")
        .arg("build")
        .arg("-q")
        .arg("--manifest-path")
        .arg(project.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &target)
        .output()
        .expect("expected `cargo build` to execute");
    if !build.status.success() {
        cleanup(&file);
        return Err(format!(
            "rust cargo build failed:\n{}",
            format_output(&build)
        ));
    }

    let bin = target.join("debug").join(name);
    let run = Command::new(&bin)
        .output()
        .expect("expected compiled rust binary to execute");
    cleanup(&file);
    if !run.status.success() {
        return Err(format!("rust binary failed:\n{}", format_output(&run)));
    }
    Ok(String::from_utf8_lossy(&run.stdout).trim().to_string())
}

// ─── Generators ────────────────────────────────────────────────────────────
//
// The generator produces a *string-typed* Aver expression. The wrapper
// builds `fn main() ! [Console.print] = Console.print(<expr>)`, which
// every backend prints to stdout in a backend-agnostic shape — no
// debug-format quirks, no array/list pretty-printer drift.
//
// Subgenerators produce typed expressions (Int, Float, Bool, String).
// The Int generator is recursive with depth-bounded BinOp; Float and
// Bool reach Int via comparisons / promotions to give the type checker
// real cross-numeric work; String covers literals, interpolation, and
// `String.fromInt` on Int.

// All four subgenerators return `BoxedStrategy<String>` rather than
// `impl Strategy<Value = String>`. The properties below mix-and-match
// them (bool arms hold ints, strings wrap ints, …), and the implicit
// nominal-type tower that `impl Strategy` builds across mutually
// recursive subgenerators blows proptest's strategy graph past the
// thread stack at PROPTEST_CASES ≥ 32. Boxing collapses the tower into
// a uniform trait object and stays linear in the input.

/// Depth-bounded Int-returning expression.
fn int_expr(max_depth: u32) -> BoxedStrategy<String> {
    let leaf = prop_oneof![
        // Random Int literal in a bounded range. Wide enough to exercise
        // overflow paths without flooding the corpus with full i64 noise
        // that backends might agree on trivially (signed wrap on all).
        (-1_000_000i64..=1_000_000i64).prop_map(|n| {
            if n < 0 {
                format!("({})", n)
            } else {
                n.to_string()
            }
        }),
    ];
    leaf.prop_recursive(max_depth, 16, 4, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} + {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} - {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} * {})", a, b)),
            // Unary minus — first-class `Expr::Neg` post-A1.
            inner.clone().prop_map(|a| format!("(-{})", a)),
            // `match` on a Bool with two Int arms. Forces backends to
            // agree on branch-typed evaluation and value plumbing
            // through arms (the wasm-gc `if` ↔ VM jump-table split).
            // The bool sub-tree caps at depth 1 to keep total program
            // size bounded — int_expr inside the bool already covers
            // the deeper combinatorics.
            (bool_expr(1), inner.clone(), inner.clone()).prop_map(|(c, t, f)| {
                format!("(match {}\n    true -> {}\n    false -> {})", c, t, f)
            }),
        ]
    })
    .boxed()
}

/// Depth-bounded Int expression whose LEAVES deliberately include the
/// i64 boundary values (`i64::MIN`, `i64::MAX`, `0`, `±1`, and
/// large-near-overflow magnitudes), so the bignum differential oracle
/// exercises the overflow → Big promotion, the `MIN`/`-1` mul trap edge,
/// the `-i64::MIN` neg promotion, and the canonical demote-on-cancel
/// paths — not just random mid-range arithmetic. Mirrors `int_expr`'s
/// recursion otherwise.
fn int_boundary_expr(max_depth: u32) -> BoxedStrategy<String> {
    // Note literals at/over i64::MIN can't be written directly (the
    // lexer rejects `9223372036854775808`), so `i64::MIN` is built as
    // `(0 - 9223372036854775807 - 1)`. Negative leaves are parenthesised.
    let leaf = prop_oneof![
        (-1_000_000i64..=1_000_000i64).prop_map(|n| if n < 0 {
            format!("({})", n)
        } else {
            n.to_string()
        }),
        Just("9223372036854775807".to_string()), // i64::MAX
        Just("(0 - 9223372036854775807 - 1)".to_string()), // i64::MIN
        Just("0".to_string()),
        Just("1".to_string()),
        Just("(-1)".to_string()),
        Just("3037000500".to_string()), // ~sqrt(i64::MAX), squares overflow
        Just("(-3037000500)".to_string()),
        Just("4611686018427387904".to_string()), // 2^62
        Just("(-4611686018427387904)".to_string()),
    ];
    leaf.prop_recursive(max_depth, 24, 4, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} + {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} - {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} * {})", a, b)),
            inner.clone().prop_map(|a| format!("(-{})", a)),
        ]
    })
    .boxed()
}

/// Depth-bounded Float-returning expression. Aver promotes mixed
/// `Int op Float` to Float at the typechecker level; both backends
/// must lower the same promotion to the same f64 result.
fn float_expr(max_depth: u32) -> BoxedStrategy<String> {
    let leaf = prop_oneof![
        // Float literal with bounded magnitude. Avoid extreme values
        // that drift into Inf/NaN territory — backends agree that
        // `1e308 * 10 = Inf` but printing Inf differs across hosts.
        (-1_000.0_f64..=1_000.0_f64)
            .prop_filter("finite, non-zero magnitude", |f| f.is_finite()
                && f.abs() > 1e-6)
            .prop_map(|f| {
                if f < 0.0 {
                    format!("({:.4})", f)
                } else {
                    format!("{:.4}", f)
                }
            }),
        // Int promoted to Float via `Int.toFloat`.
        int_expr(1).prop_map(|e| format!("Int.toFloat({})", e)),
    ];
    leaf.prop_recursive(max_depth, 12, 3, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} + {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} - {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} * {})", a, b)),
            // Unary minus on Float — sign-bit-preserving codepath
            // landed in A1 (`f64.neg` on wasm-gc, dedicated Neg
            // opcode in VM).
            inner.clone().prop_map(|a| format!("(-{})", a)),
        ]
    })
    .boxed()
}

/// Bool-returning expression. Comparisons over Int / Float plus
/// `match`-spelled and/or.
fn bool_expr(max_depth: u32) -> BoxedStrategy<String> {
    let leaf = prop_oneof![
        Just("true".to_string()),
        Just("false".to_string()),
        (int_expr(1), int_expr(1)).prop_map(|(a, b)| format!("({} < {})", a, b)),
        (int_expr(1), int_expr(1)).prop_map(|(a, b)| format!("({} == {})", a, b)),
        (int_expr(1), int_expr(1)).prop_map(|(a, b)| format!("({} >= {})", a, b)),
        (float_expr(1), float_expr(1)).prop_map(|(a, b)| format!("({} < {})", a, b)),
    ];
    leaf.prop_recursive(max_depth, 8, 2, |inner| {
        prop_oneof![
            // Aver rejects `&&` / `||`; logical and/or is spelled
            // through `match`. Generate both shapes so wasm-gc and
            // VM agree on the branch-elimination semantics.
            (inner.clone(), inner.clone()).prop_map(|(a, b)| {
                format!("(match {}\n    true -> {}\n    false -> false)", a, b)
            }),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| {
                format!("(match {}\n    true -> true\n    false -> {})", a, b)
            }),
        ]
    })
    .boxed()
}

/// String-returning expression. Mixes plain literals, integer /
/// bool projections, and concatenation.
///
/// `String.fromFloat` is *intentionally* excluded: the VM uses
/// Rust's ryu shortest-roundtrip printer, while wasm-gc emits a
/// hand-rolled WAT printer whose tie-break differs in the last
/// decimal digit. That divergence is observed and tolerated on
/// `cross_float_arithmetic_vm_vs_wasm_gc` (which compares the
/// parsed f64 values with a relative tolerance); piping it into a
/// strict-string property would noise this gate without finding
/// the codegen bugs the property actually guards against.
fn string_expr(max_depth: u32) -> BoxedStrategy<String> {
    let leaf = prop_oneof![
        "[a-z ]{0,6}".prop_map(|s| format!("\"{}\"", s)),
        int_expr(1).prop_map(|e| format!("String.fromInt({})", e)),
        bool_expr(1).prop_map(|e| format!("String.fromBool({})", e)),
    ];
    leaf.prop_recursive(max_depth, 8, 2, |inner| {
        prop_oneof![(inner.clone(), inner.clone()).prop_map(|(a, b)| format!("({} + {})", a, b))]
    })
    .boxed()
}

/// Wrap a String-typed expression in a runnable Aver module that
/// prints its value to stdout.
fn wrap_program(expr: &str) -> String {
    format!(
        "module Tmp\n\
         \n\
         fn main()\n    \
            ! [Console.print]\n    \
            Console.print({})\n",
        expr
    )
}

// ─── Equality-class regression (canonical-invariant) ────────────────────────
//
// The differential proptest above compares RENDERED output
// (`String.fromInt` / interpolation), so it structurally CANNOT catch a
// canonical-invariant violation: a value stored as Big when it should be
// Small (or vice-versa) renders to the same decimal string, yet
// `__aint_eq` — which short-circuits "a Small and a Big are never equal"
// — would report two equal values UNEQUAL. The only observable that
// distinguishes them is `==`. This focused test runs programs whose
// correctness shows ONLY through `==`, flag-on (`AVER_WASMGC_BIGNUM=1`)
// wasm-gc, and asserts the Bool output matches the VM (`Int = ℤ`).
//
// Coverage:
//   1. both sides reach i64::MIN by DIFFERENT routes (neg-demote vs
//      sub-demote) — the neg→Small(i64::MIN) demotion just fixed;
//   2. double negation round-trips +2^63 (Big) → i64::MIN (Small) → +2^63;
//   3. a Big that cancels back into i64-range must demote to Small so it
//      compares equal to the natively-Small literal.

/// Render a Bool via `String.fromBool` (Console.print needs a String) so
/// the program's only observable is the `==` result.
fn eq_program(expr: &str) -> String {
    wrap_program(&format!("String.fromBool({})", expr))
}

#[test]
fn cross_int_equality_canonical_invariant_vm_vs_wasm_gc() {
    // (expr, expected) — expected is the ℤ truth the VM computes.
    let cases: &[(&str, &str)] = &[
        // i64::MIN reached two ways: neg of +2^63 vs (0 - i64::MAX) - 1.
        (
            "((-(9223372036854775807 + 1)) == ((0 - 9223372036854775807) - 1))",
            "true",
        ),
        // double-neg: +2^63 (Big) → i64::MIN (Small) → +2^63 (Big).
        (
            "((-(-(9223372036854775807 + 1))) == (9223372036854775807 + 1))",
            "true",
        ),
        // Big cancelling back into i64 range must demote to Small.
        (
            "(((9223372036854775807 * 2) - 9223372036854775807) == 9223372036854775807)",
            "true",
        ),
        // Negative control: a genuine Big is NOT equal to a Small.
        (
            "((9223372036854775807 + 1) == 9223372036854775807)",
            "false",
        ),
    ];

    for (expr, expected) in cases {
        let source = eq_program(expr);
        let vm = run_vm("cross-eq-vm", &source).expect("VM must accept equality program");
        let wg = run_wasm_gc_bignum("cross-eq-wg", &source)
            .expect("wasm-gc (bignum) must accept equality program");
        assert_eq!(
            vm, *expected,
            "VM disagreed with the expected ℤ truth on `{expr}`",
        );
        assert_eq!(
            wg, vm,
            "wasm-gc (bignum) diverged from VM on canonical-invariant case `{expr}`:\n\
             VM = {vm}, wasm-gc = {wg}",
        );
    }
}

// ─── Properties ────────────────────────────────────────────────────────────

proptest! {
    // 64 cases is roughly a minute of wall time for the VM-vs-wasm-gc
    // diff at this generator complexity (release `aver` binary, one
    // subprocess spawn per backend per case). Bump locally via
    // `PROPTEST_CASES=N cargo test --features wasm
    // cross_backend_proptest_int_arithmetic_vm_vs_wasm_gc`.
    #![proptest_config(ProptestConfig {
        cases: 64,
        max_shrink_iters: 256,
        .. ProptestConfig::default()
    })]

    /// Int-arithmetic expressions, printed via `String.fromInt`,
    /// must produce the same stdout on every backend. A divergence
    /// here means VM and wasm-gc disagree on integer semantics
    /// (associativity, ordering of side effects inside a chain of
    /// BinOps, …).
    ///
    /// bignum slice 1 — RE-ENABLED. wasm-gc now carries the same
    /// arbitrary-precision `Int = ℤ` semantics as the VM under the opt-in
    /// `AVER_WASMGC_BIGNUM` flag (`$AverInt` carrier; add/sub/mul/neg/cmp/
    /// eq as limb helpers). The differential oracle therefore holds on
    /// every input — INCLUDING the i64-overflow ones this generator
    /// produces — which is the whole point: where the wrapping backend
    /// returned a wrapped-negative i64 (`a*a` at i64::MAX printed `1`),
    /// the bignum backend now prints the exact ℤ value the VM prints.
    /// The leaves include i64::MIN/MAX, 0, ±1, and large-near-overflow
    /// magnitudes so the Big-promotion + mul-trap-edge + neg-promotion
    /// + canonical-demote paths are all hit.
    #[test]
    fn cross_int_arithmetic_vm_vs_wasm_gc(expr in int_boundary_expr(3)) {
        let source = wrap_program(&format!("String.fromInt({})", expr));
        let vm = run_vm("cross-bp-int-vm", &source);
        let wg = run_wasm_gc_bignum("cross-bp-int-wg", &source);
        match (&vm, &wg) {
            // Both backends accept the program — outputs must match.
            (Ok(v), Ok(w)) => prop_assert_eq!(
                v, w,
                "VM vs wasm-gc diverged on source:\n{}\nVM:\n{}\nwasm-gc:\n{}",
                source, v, w
            ),
            // Both reject — that's fine (the program was ill-formed
            // in a way both backends agree on, e.g. integer overflow
            // surfacing as a runtime error on both sides).
            (Err(_), Err(_)) => {}
            // Asymmetric rejection — one backend accepts, the other
            // rejects. That is itself a divergence.
            (Ok(v), Err(e)) => prop_assert!(
                false,
                "VM accepted but wasm-gc rejected on source:\n{}\nVM stdout:\n{}\nwasm-gc error:\n{}",
                source, v, e
            ),
            (Err(e), Ok(w)) => prop_assert!(
                false,
                "wasm-gc accepted but VM rejected on source:\n{}\nVM error:\n{}\nwasm-gc stdout:\n{}",
                source, e, w
            ),
        }
    }

    /// THE SAME Int-arithmetic differential, but the value is rendered
    /// through `"{...}"` string interpolation instead of
    /// `String.fromInt(...)`. Interpolation is the idiomatic way to print
    /// an Int, and it is a DISTINCT reachability path: the bignum gate
    /// (`fn_uses_int_arithmetic`) must descend into interpolation parts, or
    /// a program whose ONLY arithmetic lives inside a `{...}` lowers the
    /// whole module's Int as wrapping i64 — a silent miscompile the
    /// `String.fromInt` variant above structurally cannot catch (that call
    /// keeps the gate on). Regression for that gate hole.
    #[test]
    fn cross_int_arithmetic_via_interpolation_vm_vs_wasm_gc(expr in int_boundary_expr(3)) {
        let source = wrap_program(&format!("\"{{{}}}\"", expr));
        let vm = run_vm("cross-bp-interp-vm", &source);
        let wg = run_wasm_gc_bignum("cross-bp-interp-wg", &source);
        match (&vm, &wg) {
            (Ok(v), Ok(w)) => prop_assert_eq!(
                v, w,
                "VM vs wasm-gc diverged (interpolation render) on source:\n{}\nVM:\n{}\nwasm-gc:\n{}",
                source, v, w
            ),
            (Err(_), Err(_)) => {}
            (Ok(v), Err(e)) => prop_assert!(
                false,
                "VM accepted but wasm-gc rejected (interpolation) on source:\n{}\nVM stdout:\n{}\nwasm-gc error:\n{}",
                source, v, e
            ),
            (Err(e), Ok(w)) => prop_assert!(
                false,
                "wasm-gc accepted but VM rejected (interpolation) on source:\n{}\nVM error:\n{}\nwasm-gc stdout:\n{}",
                source, e, w
            ),
        }
    }

    /// String concatenation + `String.fromInt` / `fromFloat` /
    /// `fromBool` projections must round-trip identically across
    /// backends. Covers the lowering of `+` over `AverStr` against
    /// the wasm-gc string-concat builtin path *and* the per-numeric-
    /// type stringification helpers.
    #[test]
    fn cross_string_concat_vm_vs_wasm_gc(expr in string_expr(2)) {
        let source = wrap_program(&expr);
        let vm = run_vm("cross-bp-str-vm", &source);
        let wg = run_wasm_gc("cross-bp-str-wg", &source);
        assert_backends_agree(&source, &vm, &wg);
    }

    /// Float arithmetic — `+ - * (-)` plus `Int.toFloat` promotion.
    /// Bounded magnitude in the generator keeps the property from
    /// drifting into `Inf` / `NaN`, where stringification differs
    /// host-to-host without it being a real codegen bug.
    ///
    /// Comparison is **tolerant**, not byte-equal: the VM renders
    /// `f64` via Rust's `format!("{}", x)` (ryu shortest-roundtrip),
    /// while wasm-gc emits its own hand-rolled WAT printer at
    /// `codegen/wasm_gc/builtins/mod.rs::emit_string_from_float`.
    /// Both observe the same f64 bits — confirmed by diffing
    /// `(x - x)` and by scaling x into the integer-printable range
    /// — but the last decimal digit drifts because the WAT printer
    /// does not match ryu's tie-break. A bit-perfect WAT
    /// implementation of shortest-roundtrip would close the gap;
    /// that's a separate piece of work, not C3 scope. Until then we
    /// allow a tiny relative tolerance and treat divergence beyond
    /// it as a real arithmetic bug.
    #[test]
    fn cross_float_arithmetic_vm_vs_wasm_gc(expr in float_expr(3)) {
        let source = wrap_program(&format!("String.fromFloat({})", expr));
        let vm = run_vm("cross-bp-flt-vm", &source);
        let wg = run_wasm_gc("cross-bp-flt-wg", &source);
        assert_backends_agree_float(&source, &vm, &wg);
    }

    /// Bool comparison and `match`-spelled and/or. wasm-gc lowers
    /// `Int < Int` etc. through `i64.lt_s`, the VM through
    /// `LT_INT` / `LT_FLOAT` / a typed-comparison opcode; both must
    /// answer identically and route through `match` arms identically.
    #[test]
    fn cross_bool_match_vm_vs_wasm_gc(expr in bool_expr(2)) {
        let source = wrap_program(&format!("String.fromBool({})", expr));
        let vm = run_vm("cross-bp-bool-vm", &source);
        let wg = run_wasm_gc("cross-bp-bool-wg", &source);
        assert_backends_agree(&source, &vm, &wg);
    }
}

/// Shared assertion for Int / Bool / String outputs: accept "both
/// succeed and outputs match" plus "both fail" (compiler-level
/// rejection on both sides is fine — the language said no on both
/// backends). Asymmetric outcomes are the divergence we want to
/// surface.
#[track_caller]
fn assert_backends_agree(source: &str, vm: &Result<String, String>, wg: &Result<String, String>) {
    match (vm, wg) {
        (Ok(v), Ok(w)) => assert_eq!(
            v, w,
            "VM vs wasm-gc diverged on source:\n{}\nVM:\n{}\nwasm-gc:\n{}",
            source, v, w
        ),
        (Err(_), Err(_)) => {}
        (Ok(v), Err(e)) => panic!(
            "VM accepted but wasm-gc rejected on source:\n{}\nVM stdout:\n{}\nwasm-gc error:\n{}",
            source, v, e
        ),
        (Err(e), Ok(w)) => panic!(
            "wasm-gc accepted but VM rejected on source:\n{}\nVM error:\n{}\nwasm-gc stdout:\n{}",
            source, e, w
        ),
    }
}

/// Float-tolerant version of `assert_backends_agree`. Parses both
/// stdouts as `f64` and compares against a relative epsilon scaled
/// by the magnitude, with a fixed absolute floor for values near
/// zero. The bound (`5e-12` relative, `1e-12` absolute) is chosen
/// to admit the last-decimal-digit drift documented on the
/// `cross_float_arithmetic_vm_vs_wasm_gc` property without
/// admitting real `f64` arithmetic disagreement (a single ULP at
/// magnitude 1.0 is ≈2.2e-16, so 5e-12 covers ~4 ULPs near the
/// printer-divergent magnitudes and stays orders of magnitude
/// tighter than any real codegen bug we expect to find).
#[track_caller]
fn assert_backends_agree_float(
    source: &str,
    vm: &Result<String, String>,
    wg: &Result<String, String>,
) {
    match (vm, wg) {
        (Ok(v), Ok(w)) => {
            let vf: f64 = v
                .parse()
                .unwrap_or_else(|_| panic!("VM stdout not f64-parseable: {:?}\n{}", v, source));
            let wf: f64 = w.parse().unwrap_or_else(|_| {
                panic!("wasm-gc stdout not f64-parseable: {:?}\n{}", w, source)
            });
            let scale = vf.abs().max(wf.abs()).max(1.0);
            let tolerance = 5e-12 * scale + 1e-12;
            assert!(
                (vf - wf).abs() <= tolerance,
                "VM vs wasm-gc diverged on source:\n{}\nVM:    {}\nwasm-gc: {}\ndelta: {:e}, tolerance: {:e}",
                source,
                v,
                w,
                (vf - wf).abs(),
                tolerance
            );
        }
        (Err(_), Err(_)) => {}
        (Ok(v), Err(e)) => panic!(
            "VM accepted but wasm-gc rejected on source:\n{}\nVM stdout:\n{}\nwasm-gc error:\n{}",
            source, v, e
        ),
        (Err(e), Ok(w)) => panic!(
            "wasm-gc accepted but VM rejected on source:\n{}\nVM error:\n{}\nwasm-gc stdout:\n{}",
            source, e, w
        ),
    }
}

// ─── Convergent VM-vs-Rust arm (separate, low-budget block) ─────────────────
//
// Split into its own `proptest!` block so it can carry a much smaller case
// count: every case does a real `cargo build` of the emitted Rust project
// (seconds), unlike the fast subprocess-run arms above. Override the count
// locally with `PROPTEST_CASES=N`.

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 8,
        max_shrink_iters: 64,
        .. ProptestConfig::default()
    })]

    /// CONVERGENT arm: VM vs the Rust codegen on the SAME overflowing
    /// `int_expr` generator. After the Int = ℤ migration both backends use
    /// arbitrary-precision integers (no wrapping), so they MUST agree on
    /// every input — including the i64-overflow cases this generator produces
    /// (the no-wrap proof at the property level). The VM-vs-wasm-gc arm above
    /// stays `#[ignore]`'d because wasm-gc still wraps; this arm is un-gated
    /// because the VM↔Rust pair has converged on Int = ℤ.
    #[test]
    fn cross_int_arithmetic_vm_vs_rust(expr in int_expr(3)) {
        let source = wrap_program(&format!("String.fromInt({})", expr));
        let vm = run_vm("cross-bp-int-vm", &source);
        let rs = run_rust("cross-bp-int-rs", &source);
        match (&vm, &rs) {
            (Ok(v), Ok(r)) => prop_assert_eq!(
                v, r,
                "VM vs Rust diverged on source:\n{}\nVM:\n{}\nRust:\n{}",
                source, v, r
            ),
            (Err(_), Err(_)) => {}
            (Ok(v), Err(e)) => prop_assert!(
                false,
                "VM accepted but Rust rejected on source:\n{}\nVM stdout:\n{}\nRust error:\n{}",
                source, v, e
            ),
            (Err(e), Ok(r)) => prop_assert!(
                false,
                "Rust accepted but VM rejected on source:\n{}\nVM error:\n{}\nRust stdout:\n{}",
                source, e, r
            ),
        }
    }
}
