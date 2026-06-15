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

/// Run `source` on the wasm-gc backend. `Int = ℤ` is now the ONLY
/// wasm-gc Int semantics (the `AVER_WASMGC_BIGNUM` flag was removed in
/// the slice-4 flip), so this is also the differential oracle for
/// add/sub/mul/neg/cmp/eq/div-mod/conversions: wasm-gc must agree with
/// the VM on EVERY input, including the i64-overflow ones the
/// generators produce.
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
            // Euclidean `Int.div` / `Int.mod` (`Result<Int,String>`),
            // rendered through the `Result` match so the generator stays
            // Int-typed; `Int.abs` / `Int.min` / `Int.max` are Int directly.
            // These exercise the slice-2 divmod + abs/min/max paths in the
            // differential fuzz (the in-repo generator previously emitted
            // only `+ - * neg`, so CI never fuzzed them).
            (inner.clone(), inner.clone()).prop_map(|(a, b)| int_div_mod_expr("div", &a, &b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| int_div_mod_expr("mod", &a, &b)),
            inner.clone().prop_map(|a| format!("Int.abs({})", a)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("Int.min({}, {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("Int.max({}, {})", a, b)),
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

/// Render `Int.div(a, b)` / `Int.mod(a, b)` as an Int-typed expression by
/// unwrapping the `Result<Int,String>` through an inline `match`. The
/// divide-by-zero `Err` arm yields `0` so the expression is total (the
/// differential still compares the Ok values, and both backends agree on
/// the `0` fallback for `b == 0`). Wrapped in parens so it drops into any
/// expression position.
fn int_div_mod_expr(op: &str, a: &str, b: &str) -> String {
    format!("(match Int.{op}({a}, {b})\n    Result.Ok(__q) -> __q\n    Result.Err(__e) -> 0)")
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
            // Euclidean div/mod (through the `Result` match) + abs/min/max,
            // at the i64-boundary leaves — exercises the slice-2 limb long
            // division + the `MIN/-1` no-overflow edge + abs/min/max across
            // the Small/Big boundary under the bignum differential oracle.
            (inner.clone(), inner.clone()).prop_map(|(a, b)| int_div_mod_expr("div", &a, &b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| int_div_mod_expr("mod", &a, &b)),
            inner.clone().prop_map(|a| format!("Int.abs({})", a)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("Int.min({}, {})", a, b)),
            (inner.clone(), inner.clone()).prop_map(|(a, b)| format!("Int.max({}, {})", a, b)),
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
// correctness shows ONLY through `==` on the wasm-gc backend
// (`Int = ℤ` by default), and asserts the Bool output matches the VM.
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
        let wg = run_wasm_gc("cross-eq-wg", &source)
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

// ─── Int eq+hash gap: Map / Set / record / sum (slice 4) ────────────────────
//
// Slice 3 fixed List/Vector element eq+hash under bignum, but Map-with-Int-
// keys, Set-of-Int (modelled as `Map<Int, Bool>`), and record/sum `==` with
// an Int field still emitted `i64.eq` / `i32.wrap_i64` on an `$aint` REF —
// invalid wasm AND semantically wrong for Big Int (a Big key looked up by an
// equal Big reached a DIFFERENT way would miss its entry). These cases route
// every such site through `__aint_eq` / `__aint_hash`. Each builds Big values
// by ARITHMETIC (`i64::MAX + 1`, `a*2`) — literals past i64 are lexer-rejected
// — and the differential asserts the wasm-gc backend (`Int = ℤ` by default)
// matches the VM exactly. The decisive observable is a Big key/field
// reached two ways: a string render can't tell a hit from a miss, but
// `Map.get` / `Map.has` / `==` can.
//
// NOTE (out of slice-4 scope, documented honestly): `Map.len` / `List.len`
// return a raw i64 under bignum (an Int-returning builtin not lifted to
// `$aint`), and bare `List<Int>` literals don't register their instantiation
// under the flag. Both are PRE-EXISTING slice-1/3 representation gaps
// independent of eq+hash; these tests therefore avoid `.len` + `String.fromInt`
// and `List<Int>` literals. They are NOT regressions of this change.

/// Empty `Map<Int, V>` builder (the `{}` literal needs a type annotation,
/// which a binding can't carry — so a tiny typed helper fn supplies it).
fn map_int_program(v_ty: &str, body: &str) -> String {
    format!(
        "module Tmp\n\
         \n\
         fn emptyM() -> Map<Int, {v_ty}>\n    \
             {{}}\n\
         \n\
         fn main()\n    \
             ! [Console.print]\n\
         {body}"
    )
}

#[test]
fn cross_int_eqhash_map_big_keys_vm_vs_wasm_gc() {
    // Two distinct Big keys + a Small key; a Big key reached via a SECOND
    // computation (`bigAgain`, which equals `big` by ℤ arithmetic) must hit
    // the SAME entry — overwriting it, not adding one (Eq/Hash agreement
    // across the Small/Big boundary). Then `Map.remove` a Big key and read
    // ONLY the post-remove map.
    //
    // We never read a pre-`Map.remove` map after the remove: wasm-gc's
    // `Map.remove` rewrites the keys array in place (a PRE-EXISTING aliasing
    // bug, reproducible flag-OFF too, unrelated to bignum eq+hash), so doing
    // so would test that bug rather than this fix.
    let body = "    \
        big = (9223372036854775807 + 1)\n    \
        big2 = (big * 2)\n    \
        small = 42\n    \
        bigAgain = ((9223372036854775807 * 2) - 9223372036854775807 + 1)\n    \
        m1 = Map.set(emptyM(), big, 100)\n    \
        m2 = Map.set(m1, big2, 200)\n    \
        m3 = Map.set(m2, small, 300)\n    \
        m4 = Map.set(m3, bigAgain, 101)\n    \
        Console.print(String.fromInt(Option.withDefault(Map.get(m4, big), 0 - 1)))\n    \
        Console.print(String.fromInt(Option.withDefault(Map.get(m4, big2), 0 - 1)))\n    \
        Console.print(String.fromInt(Option.withDefault(Map.get(m4, small), 0 - 1)))\n    \
        Console.print(String.fromInt(Option.withDefault(Map.get(m4, bigAgain), 0 - 1)))\n    \
        Console.print(String.fromBool(Map.has(m4, big2)))\n    \
        m5 = Map.remove(m4, big2)\n    \
        Console.print(String.fromBool(Map.has(m5, big2)))\n    \
        Console.print(String.fromInt(Option.withDefault(Map.get(m5, big), 0 - 1)))\n    \
        Console.print(String.fromInt(Option.withDefault(Map.get(m5, bigAgain), 0 - 1)))\n";
    let source = map_int_program("Int", body);
    let vm = run_vm("cross-eqhash-map-vm", &source).expect("VM must accept Map<Int,Int> program");
    let wg = run_wasm_gc("cross-eqhash-map-wg", &source)
        .expect("wasm-gc (bignum) must accept Map<Int,Int> with Big keys");
    assert_eq!(
        wg, vm,
        "VM vs wasm-gc (bignum) diverged on Map<Int,Int> with Big keys \
         (get/has/overwrite/remove):\nVM:\n{vm}\nwasm-gc:\n{wg}"
    );
}

#[test]
fn cross_int_eqhash_set_big_dedup_vm_vs_wasm_gc() {
    // Set-of-Int as `Map<Int, Bool>`: an equal-Big-via-two-paths must
    // collapse to one membership entry; non-members miss.
    let body = "    \
        big = (9223372036854775807 + 1)\n    \
        bigSame = ((9223372036854775807 * 2) - 9223372036854775807 + 1)\n    \
        s1 = Map.set(emptyM(), big, true)\n    \
        s2 = Map.set(s1, bigSame, true)\n    \
        s3 = Map.set(s2, 7, true)\n    \
        Console.print(String.fromBool(Map.has(s3, big)))\n    \
        Console.print(String.fromBool(Map.has(s3, bigSame)))\n    \
        Console.print(String.fromBool(Map.has(s3, 7)))\n    \
        Console.print(String.fromBool(Map.has(s3, 8)))\n    \
        Console.print(String.fromBool(Map.has(s3, (big * 2))))\n";
    let source = map_int_program("Bool", body);
    let vm = run_vm("cross-eqhash-set-vm", &source).expect("VM must accept Map<Int,Bool> program");
    let wg = run_wasm_gc("cross-eqhash-set-wg", &source)
        .expect("wasm-gc (bignum) must accept Set-of-Int with Big members");
    assert_eq!(
        wg, vm,
        "VM vs wasm-gc (bignum) diverged on Set-of-Int (Map<Int,Bool>) Big dedup:\n\
         VM:\n{vm}\nwasm-gc:\n{wg}"
    );
}

#[test]
fn cross_int_eqhash_record_sum_big_field_vm_vs_wasm_gc() {
    // record `==` and sum `==` with a Big Int field: equal Big fields → true,
    // differing → false, Small-vs-Big → false, different variant → false.
    let source = "module Tmp\n\
         \n\
         record Point\n    \
             x: Int\n    \
             y: Int\n\
         \n\
         type Wrap\n    \
             W(Int)\n    \
             V(Int)\n\
         \n\
         fn main()\n    \
             ! [Console.print]\n    \
             big = (9223372036854775807 + 1)\n    \
             bigSame = ((9223372036854775807 * 2) - 9223372036854775807 + 1)\n    \
             p1 = Point(x = big, y = 5)\n    \
             p2 = Point(x = bigSame, y = 5)\n    \
             p3 = Point(x = big, y = 6)\n    \
             p4 = Point(x = 42, y = 5)\n    \
             Console.print(String.fromBool(p1 == p2))\n    \
             Console.print(String.fromBool(p1 == p3))\n    \
             Console.print(String.fromBool(p1 == p4))\n    \
             w1 = Wrap.W(big)\n    \
             w2 = Wrap.W(bigSame)\n    \
             w3 = Wrap.W(42)\n    \
             w4 = Wrap.V(big)\n    \
             Console.print(String.fromBool(w1 == w2))\n    \
             Console.print(String.fromBool(w1 == w3))\n    \
             Console.print(String.fromBool(w1 == w4))\n"
        .to_string();
    let vm = run_vm("cross-eqhash-rec-vm", &source).expect("VM must accept record/sum eq program");
    let wg = run_wasm_gc("cross-eqhash-rec-wg", &source)
        .expect("wasm-gc (bignum) must accept record/sum eq with a Big Int field");
    assert_eq!(
        wg, vm,
        "VM vs wasm-gc (bignum) diverged on record/sum `==` with a Big Int field:\n\
         VM:\n{vm}\nwasm-gc:\n{wg}"
    );
}

// ─── Euclidean divmod (slice 2) ─────────────────────────────────────────────
//
// `Int.div` / `Int.mod` return `Result<Int, String>` with EUCLIDEAN
// semantics (remainder always in `[0, |b|)`), matching the VM
// (`src/types/int.rs`) and `aver-rt` exactly. Under bignum there is NO
// `i64::MIN / -1` overflow — that quotient is the valid Big `+2^63`.
//
// The differential proptest compares RENDERED stdout, so it is blind to
// two slice-1 bug classes; both are covered explicitly below:
//   - render via BOTH `String.fromInt` AND `"{...}"` interpolation;
//   - assert the law `(a/b)*b + (a%b) == a` and `0 <= (a%b) < |b|`
//     through `==` (flag-on wasm-gc must equal the VM ℤ truth `true`),
//     which a string render cannot distinguish from a canonical-invariant
//     violation.

/// The full sign/boundary matrix of operands for the Euclidean law. Each
/// is a *parenthesised Int expression* (so it drops straight into a call
/// arg). Includes i64::MIN/MAX, 0, ±1, near-sqrt(MAX) (squares overflow),
/// 2^62, and Big values built by arithmetic (`a*a`, `±2^63`).
fn divmod_operands() -> Vec<&'static str> {
    // One representative per sign/boundary class. Kept to 11 so the
    // generated `main` (11×11 pairs × 3 prints) stays well under the VM's
    // single-function stack budget — the Big magnitudes are exercised by
    // `cross_int_big_operand_divmod_vm_vs_wasm_gc` and the sweep below.
    vec![
        "0",
        "1",
        "(0 - 1)",
        "7",
        "(0 - 7)",
        "9223372036854775807",             // i64::MAX
        "((0 - 9223372036854775807) - 1)", // i64::MIN
        "(9223372036854775807 + 1)",       // +2^63 (Big)
        "(0 - (9223372036854775807 + 1))", // -2^63 (Big)
        "3037000500",
        "(0 - 3037000500)",
    ]
}

/// `Int.div(a,b)` rendered as a String via a Result-unwrapping helper.
/// `b == 0` renders the `Result.Err` message, so divide-by-zero is part
/// of the differential (both backends must produce the same message).
fn divmod_harness(body_lines: &str) -> String {
    // `law` asserts the Euclidean contract `(q*b + r == a) && (0 <= r < |b|)`
    // through `==` / `<` (NOT a string render), so a value mis-stored as
    // Small-vs-Big — invisible to `String.fromInt` — fails it. The
    // conjunction is spelled with `Bool.and` + a helper (Aver's parser
    // rejects deeply nested inline `match` in this position).
    format!(
        "module Tmp\n\
         \n\
         fn dv(a: Int, b: Int) -> String\n    \
             match Int.div(a, b)\n        \
                 Result.Ok(q) -> String.fromInt(q)\n        \
                 Result.Err(e) -> e\n\
         \n\
         fn md(a: Int, b: Int) -> String\n    \
             match Int.mod(a, b)\n        \
                 Result.Ok(r) -> String.fromInt(r)\n        \
                 Result.Err(e) -> e\n\
         \n\
         fn lawq(a: Int, b: Int, q: Int) -> Bool\n    \
             match Int.mod(a, b)\n        \
                 Result.Ok(r) -> Bool.and(((q * b) + r) == a, Bool.and((0 - 1) < r, r < Int.abs(b)))\n        \
                 Result.Err(_) -> false\n\
         \n\
         fn law(a: Int, b: Int) -> Bool\n    \
             match Int.div(a, b)\n        \
                 Result.Ok(q) -> lawq(a, b, q)\n        \
                 Result.Err(_) -> true\n\
         \n\
         fn main()\n    \
             ! [Console.print]\n\
         {body_lines}\n"
    )
}

#[test]
fn cross_int_euclidean_divmod_vm_vs_wasm_gc() {
    let ops = divmod_operands();
    // Build one program that prints, for every (a, b) pair:
    //   - div rendered via String.fromInt
    //   - mod rendered via "{...}" interpolation (the slice-1 render blind
    //     spot — a distinct reachability path)
    //   - the Euclidean law `(q*b + r == a) && (0 <= r < |b|)` via `==`
    //     (the canonical-invariant blind spot — only `==` distinguishes a
    //     mis-stored Small/Big)
    let mut lines = String::new();
    let mut pair_count = 0usize;
    for a in &ops {
        for b in &ops {
            lines.push_str(&format!("    Console.print(dv({a}, {b}))\n"));
            lines.push_str(&format!("    Console.print(\"{{md({a}, {b})}}\")\n"));
            lines.push_str(&format!(
                "    Console.print(String.fromBool(law({a}, {b})))\n"
            ));
            pair_count += 1;
        }
    }
    let source = divmod_harness(lines.trim_end());

    let vm = run_vm("cross-divmod-vm", &source).expect("VM must accept the divmod harness");
    let wg = run_wasm_gc("cross-divmod-wg", &source)
        .expect("wasm-gc (bignum) must accept the divmod harness");

    // 1. The two backends agree byte-for-byte on every rendered line.
    assert_eq!(
        vm, wg,
        "VM vs wasm-gc (bignum) diverged on the Euclidean divmod matrix \
         ({pair_count} pairs, div+mod+law per pair)"
    );

    // 2. Every `law(...)` line is `true` on the VM (the ℤ oracle). The law
    //    lines are every third line; assert none is `false`, so the matrix
    //    actually exercised the law rather than trivially agreeing on a
    //    shared bug.
    let law_lines: Vec<&str> = vm.lines().skip(2).step_by(3).collect();
    assert_eq!(
        law_lines.len(),
        pair_count,
        "expected one law line per pair"
    );
    assert!(
        law_lines.iter().all(|l| *l == "true"),
        "the Euclidean law `q*b + r == a, 0 <= r < |b|` failed on the VM ℤ \
         oracle for some pair (first offending around index {:?})",
        law_lines.iter().position(|l| *l != "true")
    );
}

#[test]
fn cross_int_big_operand_divmod_vm_vs_wasm_gc() {
    // Big-operand divmod specifically exercises the limb long division
    // (the i64 fast path can't reach these). `(a*a)/a == a`, `(a*a)%a == 0`
    // for a near i64::MAX, plus a Big dividend over a Small divisor (both
    // signs). Rendered via String.fromInt; the VM truth is asserted inline.
    let cases: &[(&str, &str)] = &[
        // (program-expr, expected ℤ stdout)
        (
            "dv(9223372036854775807 * 9223372036854775807, 9223372036854775807)",
            "9223372036854775807",
        ),
        (
            "md(9223372036854775807 * 9223372036854775807, 9223372036854775807)",
            "0",
        ),
        ("dv(3037000500 * 3037000500, 3037000500)", "3037000500"),
        ("md(3037000500 * 3037000500, 3037000500)", "0"),
        // Big dividend, Small divisor, exactly divisible (i64::MAX*7 % 7 == 0).
        ("md(9223372036854775807 * 7, 7)", "0"),
        ("dv(9223372036854775807 * 7, 7)", "9223372036854775807"),
        // Negative Big dividend, Small divisor — Euclidean remainder is
        // non-negative; exact divisibility keeps it 0 (the slice-2 stale-
        // length bug produced a spurious +7 here).
        ("md((0 - 9223372036854775807) * 7, 7)", "0"),
        (
            "dv((0 - 9223372036854775807) * 7, 7)",
            "-9223372036854775807",
        ),
    ];

    let body: String = cases
        .iter()
        .map(|(expr, _)| format!("    Console.print({expr})"))
        .collect::<Vec<_>>()
        .join("\n");
    let source = divmod_harness(&body);

    let vm = run_vm("cross-bigdm-vm", &source).expect("VM must accept the big divmod program");
    let wg = run_wasm_gc("cross-bigdm-wg", &source)
        .expect("wasm-gc (bignum) must accept the big divmod program");

    assert_eq!(
        vm, wg,
        "VM vs wasm-gc (bignum) diverged on Big-operand divmod"
    );

    let vm_lines: Vec<&str> = vm.lines().collect();
    assert_eq!(vm_lines.len(), cases.len(), "line count mismatch");
    for ((expr, expected), got) in cases.iter().zip(vm_lines.iter()) {
        assert_eq!(
            got, expected,
            "VM ℤ truth wrong for `{expr}`: got {got}, expected {expected}"
        );
    }
}

// ─── Decimal parse/format + Int<->Float/index exactness (slice 3) ───────────
//
// These cover the three slice-3 conversions whose wasm-gc lowering used to be
// i64-only (`Int.fromString` wrapping past i64), saturating
// (`Int.fromFloat`/`Float.fromInt` outside i64), or wrong (an `I32WrapI64`
// of a Big `Vector` index into a wrong in-range slot). On the wasm-gc
// backend (`Int = ℤ` by default) they must match the VM exactly. As with
// the slice-1/2 focused tests, the render goes through BOTH `String.fromInt`
// AND `"{...}"` interpolation where applicable so neither blind spot hides a
// divergence.

#[test]
fn cross_int_from_string_roundtrip_vm_vs_wasm_gc() {
    // `String.fromInt(Int.fromString(s)) == s` for magnitudes well past i64
    // (both signs), plus invalid-input error-message parity. The 38-digit
    // value is `i64::MAX * i64::MAX` — the headline acceptance.
    let ok_cases: &[&str] = &[
        "0",
        "1",
        "-1",
        "9223372036854775807",                     // i64::MAX
        "9223372036854775808",                     // i64::MAX + 1 (Big)
        "-9223372036854775808",                    // i64::MIN
        "-9223372036854775809",                    // i64::MIN - 1 (Big)
        "85070591730234615847396907784232501249",  // i64::MAX^2 (38 digits)
        "-85070591730234615847396907784232501249", // negative, Big
        "170141183460469231731687303715884105728", // 2^127 (39 digits)
    ];
    // Helpers do the `Result` match (an inline `match` inside a call arg
    // doesn't parse); `main` just prints. `fs` reformats the parse, `rt`
    // observes the round-trip equality `fromInt(fromString(s)) == orig` via
    // `==` (an Int mis-stored as Small/Big still `String.fromInt`-renders the
    // same decimal, so the explicit `==` is the real canonical-invariant check).
    let mut lines = String::new();
    for s in ok_cases {
        lines.push_str(&format!("    Console.print(fs(\"{s}\"))\n"));
        lines.push_str(&format!("    Console.print(rt(\"{s}\", \"{s}\"))\n"));
    }
    // Invalid-input → `Err` message parity (must be byte-identical to the VM).
    let bad_cases: &[&str] = &[" 5", "5 ", "", "-", "+", "0x10", "1.5", "--5", "abc", "12a"];
    for s in bad_cases {
        lines.push_str(&format!("    Console.print(fs(\"{s}\"))\n"));
    }
    let source = format!(
        "module Tmp\n\n\
         fn fs(s: String) -> String\n    \
             match Int.fromString(s)\n        \
                 Result.Ok(n) -> String.fromInt(n)\n        \
                 Result.Err(e) -> e\n\
         \n\
         fn rt(s: String, orig: String) -> String\n    \
             match Int.fromString(s)\n        \
                 Result.Ok(n) -> String.fromBool(String.fromInt(n) == orig)\n        \
                 Result.Err(e) -> e\n\
         \n\
         fn main()\n    ! [Console.print]\n{}\n",
        lines.trim_end()
    );

    let vm = run_vm("cross-fs-vm", &source).expect("VM must accept the fromString harness");
    let wg = run_wasm_gc("cross-fs-wg", &source)
        .expect("wasm-gc (bignum) must accept the fromString harness");
    assert_eq!(
        vm, wg,
        "VM vs wasm-gc (bignum) diverged on Int.fromString parse/format/error parity"
    );
    // The round-trip-equality lines (every other line in the Ok block) must
    // all be `true` on the VM ℤ oracle — proof the corpus actually exercised
    // past-i64 round-trips rather than trivially agreeing.
    let rt_true = vm.lines().filter(|l| *l == "true").count();
    assert_eq!(
        rt_true,
        ok_cases.len(),
        "expected one `true` round-trip line per Ok case (got {rt_true}); \
         a past-i64 round-trip silently failed"
    );
}

#[test]
fn cross_int_float_exactness_vm_vs_wasm_gc() {
    // `Float.fromInt` (±inf saturation) and `Int.fromFloat` (exact Big /
    // non-finite → 0) must match the VM. Big values are built by arithmetic
    // (the lexer rejects > i64 literals — a separate follow-up). The
    // observables avoid `String.fromFloat` on huge magnitudes (a pre-existing
    // wasm-gc trap, unrelated to slice 3): we render Bool comparisons and
    // the exact Int round-trip via `String.fromInt`.
    let cases: &[(&str, &str)] = &[
        // Float.fromInt(Big) compared against a Float threshold (observable
        // without String.fromFloat): 2^63 > 1e9.
        (
            "String.fromBool(Float.fromInt(9223372036854775807 + 1) > 1000000000.0)",
            "true",
        ),
        // Round-trip: Int.fromFloat(Float.fromInt(2^63)) == 2^63 exactly.
        (
            "String.fromInt(Int.fromFloat(Float.fromInt(9223372036854775807 + 1)))",
            "9223372036854775808",
        ),
        // Negative Big round-trip.
        (
            "String.fromInt(Int.fromFloat(Float.fromInt(0 - (9223372036854775807 + 1))))",
            "-9223372036854775808",
        ),
        // Int.fromFloat of a huge finite float (1e27, built by Float mult) is
        // an EXACT Big — matches the VM's BigInt::from_f64 rounding.
        (
            "String.fromInt(Int.fromFloat(1000000000.0 * 1000000000.0 * 1000000000.0))",
            "1000000000000000013287555072",
        ),
        // Int.fromFloat of a Small-range float stays Small.
        ("String.fromInt(Int.fromFloat(42.9))", "42"),
        ("String.fromInt(Int.fromFloat(-42.9))", "-42"),
        // Float.fromInt of a Small round-trips.
        (
            "String.fromBool(Int.fromFloat(Float.fromInt(123456789)) == 123456789)",
            "true",
        ),
    ];
    let body: String = cases
        .iter()
        .map(|(expr, _)| format!("    Console.print({expr})"))
        .collect::<Vec<_>>()
        .join("\n");
    let source = format!("module Tmp\n\nfn main()\n    ! [Console.print]\n{body}\n");

    let vm = run_vm("cross-flt-vm", &source).expect("VM must accept the float-exactness harness");
    let wg = run_wasm_gc("cross-flt-wg", &source)
        .expect("wasm-gc (bignum) must accept the float-exactness harness");
    assert_eq!(
        vm, wg,
        "VM vs wasm-gc (bignum) diverged on Int<->Float exactness"
    );
    let vm_lines: Vec<&str> = vm.lines().collect();
    assert_eq!(vm_lines.len(), cases.len(), "line count mismatch");
    for ((expr, expected), got) in cases.iter().zip(vm_lines.iter()) {
        assert_eq!(
            got, expected,
            "VM ℤ truth wrong for `{expr}`: got {got}, expected {expected}"
        );
    }
}

#[test]
fn cross_float_from_int_rounding_vm_vs_wasm_gc() {
    // `Float.fromInt` (Big → f64) must be CORRECTLY ROUNDED (round-to-nearest-
    // even), bit-identical to the VM's `AverInt::to_f64` (which funnels through
    // `num_bigint::BigInt::to_f64`). The earlier exactness test
    // (`cross_int_float_exactness_vm_vs_wasm_gc`) only fed `Float.fromInt`
    // EXACTLY-representable Bigs (2^63, 1e27-class), so it never exercised the
    // rounding boundary. A high→low Horner accumulation (`acc = acc*2^32 + limb`
    // in f64) double-rounds, diverging by 1 ULP on ~10% of >=3-limb magnitudes;
    // this test feeds NON-representable >=3-limb Bigs (>= ~2^85) and asserts the
    // round-trip `Int.fromFloat(Float.fromInt(big)) == big` matches the VM
    // exactly. It FAILS on the pre-fix Horner helper and PASSES after the
    // sticky-jam top-64-bits conversion (confirmed empirically in wasmtime).
    //
    // Inputs: the three cross-vendor counterexamples, plus a deterministic
    // spread of >=3-limb magnitudes (both signs) including near-power-of-two
    // and bit-53 tie/round boundary shapes, so the rounding decision is
    // exercised in both directions (round up, round down, exact, tie-to-even).
    let mut cases: Vec<String> = vec![
        // Confirmed off-by-1-ULP counterexamples from two cross-vendor panels.
        "38693363567040072931711122".to_string(),
        "1842425632053114986489719067".to_string(),
        "1204155011761264803879351098619922060".to_string(),
        // Negative variants (sign applied after the correctly-rounded magnitude).
        "-38693363567040072931711122".to_string(),
        "-1204155011761264803879351098619922060".to_string(),
        // Near power-of-two +/- 1 (>= 3 limbs): 2^85, 2^96, 2^127, 2^200.
        "38685626227668133590597632".to_string(), // 2^85 (exact)
        "38685626227668133590597631".to_string(), // 2^85 - 1
        "79228162514264337593543950336".to_string(), // 2^96 (exact)
        "79228162514264337593543950337".to_string(), // 2^96 + 1
        "170141183460469231731687303715884105728".to_string(), // 2^127
        "170141183460469231731687303715884105727".to_string(), // 2^127 - 1
        // bit-53 tie/round boundary: ((2^53 + k) << s) and +/- a low bit, so
        // round-to-nearest-even is forced both ways across the dropped bits.
        "77371252455336267181195264".to_string(), // (2^53) << 33, exact
        "77371252455336267181195265".to_string(), // + 1 (sticky -> round up?)
        "77371252455336275771129856".to_string(), // (2^53) << 33 + 2^32 (half -> tie-even)
        "77371252455336284361064448".to_string(), // (2^53 + 1) << 33
        "618970019642690137449562112".to_string(), // (2^53) << 36
        "618970019642690137449562113".to_string(), // + 1
        // A long magnitude (>= 6 limbs) so the multi-limb sticky OR is hit.
        "12345678901234567890123456789012345678901234567890".to_string(),
        "-12345678901234567890123456789012345678901234567890".to_string(),
    ];
    // A deterministic LCG spread of >=3-limb magnitudes, both signs, so the
    // 1-ULP divergence (which the Horner helper hit on ~10% of inputs) is very
    // likely to be tripped by at least one case if the fix ever regresses.
    let mut state: u128 = 0x9E37_79B9_7F4A_7C15;
    for _ in 0..40 {
        state = state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        let hi = (state >> 64) as u64;
        let lo = state as u64;
        // 96..160-bit magnitude (>= 3 32-bit limbs), top bit forced set.
        let mag = ((hi as u128) << 64) | (lo as u128) | (1u128 << 95);
        let sign = if state & 1 == 0 { "" } else { "-" };
        cases.push(format!("{sign}{mag}"));
    }

    // Render each through the round-trip `Int.fromFloat(Float.fromInt(big))`
    // (the observable both panels used — avoids the unrelated huge-magnitude
    // `String.fromFloat` trap). The VM is the ℤ oracle.
    let body: String = cases
        .iter()
        .map(|c| format!("    Console.print(rt(\"{c}\"))"))
        .collect::<Vec<_>>()
        .join("\n");
    let source = format!(
        "module Tmp\n\n\
         fn rt(s: String) -> String\n    \
             match Int.fromString(s)\n        \
                 Result.Ok(n) -> String.fromInt(Int.fromFloat(Float.fromInt(n)))\n        \
                 Result.Err(e) -> e\n\
         \n\
         fn main()\n    ! [Console.print]\n{body}\n"
    );

    let vm =
        run_vm("cross-flt-round-vm", &source).expect("VM must accept the float-rounding harness");
    let wg = run_wasm_gc("cross-flt-round-wg", &source)
        .expect("wasm-gc (bignum) must accept the float-rounding harness");
    assert_eq!(
        vm, wg,
        "VM vs wasm-gc (bignum) diverged on Float.fromInt(Big) correct rounding \
         (the round-trip Int.fromFloat(Float.fromInt(big)) drifted by >= 1 ULP)"
    );
    // Sanity: the corpus actually exercised >=3-limb Bigs (none collapsed to a
    // trivially-agreeing Small) — every line is a long decimal, not "0".
    assert_eq!(
        vm.lines().count(),
        cases.len(),
        "line count mismatch (a case failed to render)"
    );
    assert!(
        vm.lines().all(|l| l.trim_start_matches('-').len() > 18),
        "expected every round-trip to render a past-i64 (>18 digit) magnitude"
    );
}

#[test]
fn cross_big_vector_index_oob_vm_vs_wasm_gc() {
    // A Big `Int` index into a `Vector` is necessarily out of bounds, so it
    // must behave as the VM's `Option.None` — NOT an `I32WrapI64`-truncated
    // in-range access. Covers Vector.get (boxed) + the negative-index lower
    // bound. The index is built by arithmetic so it overflows i64 into a Big.
    // (index-expr, expected). The helper `vg` does the bounds-checked
    // `Vector.get` + `Option` match (a 3-element vector of `7`s).
    let cases: &[(&str, &str)] = &[
        // Big positive index → None.
        ("9223372036854775807 + 1", "none"),
        // Big negative index → None.
        ("0 - (9223372036854775807 + 1)", "none"),
        // Plain negative index → None.
        ("0 - 1", "none"),
        // In-range index still works (the fix must not break valid access).
        ("1", "7"),
        // An index that would WRAP to an in-range i32 if truncated
        // (`2^32 + 1` `I32WrapI64`s to `1`) must still be None.
        ("4294967296 + 1", "none"),
    ];
    let body: String = cases
        .iter()
        .map(|(expr, _)| format!("    Console.print(vg({expr}))"))
        .collect::<Vec<_>>()
        .join("\n");
    let source = format!(
        "module Tmp\n\n\
         fn vg(i: Int) -> String\n    \
             match Vector.get(Vector.new(3, 7), i)\n        \
                 Option.Some(x) -> String.fromInt(x)\n        \
                 Option.None -> \"none\"\n\
         \n\
         fn main()\n    ! [Console.print]\n{body}\n"
    );

    let vm = run_vm("cross-bigidx-vm", &source).expect("VM must accept the big-index harness");
    let wg = run_wasm_gc("cross-bigidx-wg", &source)
        .expect("wasm-gc (bignum) must accept the big-index harness");
    assert_eq!(
        vm, wg,
        "VM vs wasm-gc (bignum) diverged on Big-Vector-index out-of-bounds"
    );
    let vm_lines: Vec<&str> = vm.lines().collect();
    assert_eq!(vm_lines.len(), cases.len(), "line count mismatch");
    for ((expr, expected), got) in cases.iter().zip(vm_lines.iter()) {
        assert_eq!(
            got, expected,
            "VM ℤ truth wrong for index `{expr}`: got {got}, expected {expected}"
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
    /// wasm-gc now carries the same arbitrary-precision `Int = ℤ`
    /// semantics as the VM BY DEFAULT (`$AverInt` carrier; add/sub/mul/
    /// neg/cmp/eq as limb helpers — no flag). The differential oracle
    /// therefore holds on every input — INCLUDING the i64-overflow ones
    /// this generator produces — which is the whole point: where the
    /// old wrapping backend returned a wrapped-negative i64 (`a*a` at
    /// i64::MAX printed `1`), the bignum backend now prints the exact ℤ
    /// value the VM prints.
    /// The leaves include i64::MIN/MAX, 0, ±1, and large-near-overflow
    /// magnitudes so the Big-promotion + mul-trap-edge + neg-promotion
    /// + canonical-demote paths are all hit.
    #[test]
    fn cross_int_arithmetic_vm_vs_wasm_gc(expr in int_boundary_expr(3)) {
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
        let wg = run_wasm_gc("cross-bp-interp-wg", &source);
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
