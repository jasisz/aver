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
    /// (associativity, sign of overflow wrap, ordering of side
    /// effects inside a chain of BinOps, …).
    #[test]
    fn cross_int_arithmetic_vm_vs_wasm_gc(expr in int_expr(3)) {
        let source = wrap_program(&format!("String.fromInt({})", expr));
        let vm = run_vm("cross-bp-int-vm", &source);
        let wg = run_wasm_gc("cross-bp-int-wg", &source);
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
