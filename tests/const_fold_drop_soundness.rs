//! Soundness regression net for const-fold "drop" bugs.
//!
//! A structural fold that statically collapses a consumer over a known
//! constructor (Fold B) or a const-divisor `Int.div`/`Int.mod` (Fold A)
//! must NEVER drop a strictly-evaluated sub-expression. In 0.24 four folds
//! did, silently discarding an effectful / diverging sub-expression:
//!
//!   - B1: `match Result.Ok(noisy()) { _ -> .. }` — the Wildcard arm
//!     dropped the ctor arg, so its effect vanished;
//!   - B2: `Result.withDefault(Result.Err(noisy()), d)` — the Err payload
//!     was dropped, so its effect vanished;
//!   - B3: `Int.div(noisy(), 0)` — the dividend was dropped, so its effect
//!     vanished (and a *diverging* dividend turned non-termination into
//!     termination);
//!   - panic: `match Result.Ok(x) { Result.Ok(_) -> .. }` — the `_` field's
//!     sentinel-slot `Let` made the VM's u8 `STORE_LOCAL` index out of
//!     bounds and panic.
//!
//! Each program runs through the real VM (`aver run`) — NOT `aver verify` —
//! per the audit-crash lesson (verify ≠ run; a test that passes with AND
//! without the fix proves nothing). On the pre-fix binary the asserted
//! effect is absent / the VM panics, so each test FAILs without the fix.
//! The final test pins the headline win (const-divisor fold still computes)
//! so the fix can't pass by neutering const-fold.

use std::fs;
use std::process::Command;

/// Run an Aver program via the built `aver` binary; assert it exits
/// successfully (a panic is a non-success exit) and return trimmed stdout.
fn run_aver(name: &str, source: &str) -> String {
    let dir = std::env::temp_dir().join(format!("aver_const_fold_drop_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create temp dir");
    let file = dir.join(format!("{name}.av"));
    fs::write(&file, source).expect("write source");

    let out = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&file)
        .output()
        .expect("spawn aver run");
    assert!(
        out.status.success(),
        "`aver run {}` failed (panic / non-zero exit): {}",
        file.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = fs::remove_dir_all(&dir);
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

#[test]
fn b1_wildcard_arm_keeps_ctor_arg_effect() {
    let src = r#"module B1
    intent = "Wildcard arm over Result.Ok(effectful) must not drop the effect."
    exposes [main]
    depends []
    effects [Console]

fn noisy(n: Int) -> Int
    ? "Prints, returns n."
    ! [Console.print]
    _ = Console.print("noisy ran with {n}")
    n

fn pick() -> Int
    ? "Match over Result.Ok(effectful) with a wildcard arm."
    ! [Console.print]
    match Result.Ok(noisy(7))
        _ -> 42

fn main() -> Unit
    ! [Console.print]
    r = pick()
    Console.print("result {r}")
"#;
    let out = run_aver("b1_wildcard", src);
    assert!(
        out.contains("noisy ran with 7"),
        "Wildcard-arm fold dropped the ctor arg's effect; got:\n{out}"
    );
    assert!(out.contains("result 42"), "got:\n{out}");
}

#[test]
fn b2_withdefault_err_keeps_payload_effect() {
    let src = r#"module B2
    intent = "withDefault over Result.Err(effectful) must not drop the payload effect."
    exposes [main]
    depends []
    effects [Console]

fn boom(n: Int) -> String
    ? "Prints, returns an error string."
    ! [Console.print]
    _ = Console.print("boom ran with {n}")
    "the error"

fn pick() -> String
    ? "withDefault over a statically-Err result."
    ! [Console.print]
    Result.withDefault(Result.Err(boom(3)), "fallback")

fn main() -> Unit
    ! [Console.print]
    r = pick()
    Console.print("result {r}")
"#;
    let out = run_aver("b2_withdefault", src);
    assert!(
        out.contains("boom ran with 3"),
        "withDefault Err fold dropped the payload's effect; got:\n{out}"
    );
    assert!(out.contains("result fallback"), "got:\n{out}");
}

#[test]
fn b3_div_by_zero_keeps_dividend_effect() {
    let src = r#"module B3
    intent = "Int.div by literal zero must not drop the dividend's effect."
    exposes [main]
    depends []
    effects [Console]

fn loud(n: Int) -> Int
    ? "Prints, returns n — this is the dividend."
    ! [Console.print]
    _ = Console.print("dividend evaluated")
    n

fn compute() -> Result<Int, String>
    ? "Divide an effectful dividend by literal zero."
    ! [Console.print]
    Int.div(loud(5), 0)

fn main() -> Unit
    ! [Console.print]
    match compute()
        Result.Ok(v) -> Console.print("ok {v}")
        Result.Err(e) -> Console.print("err {e}")
"#;
    let out = run_aver("b3_divzero", src);
    assert!(
        out.contains("dividend evaluated"),
        "Int.div(_, 0) fold dropped the dividend's effect; got:\n{out}"
    );
    assert!(out.contains("err division by zero"), "got:\n{out}");
}

#[test]
fn panic_ctor_discard_field_runs_without_vm_panic() {
    let src = r#"module Discard
    intent = "Ctor arm with a `_` field over a known ctor must not panic the VM."
    exposes [main]
    depends []
    effects [Console]

fn noisy(n: Int) -> Int
    ? "Prints, returns n."
    ! [Console.print]
    _ = Console.print("noisy ran with {n}")
    n

fn pick() -> Int
    ? "Ctor arm binds the payload field to `_` (discard)."
    ! [Console.print]
    match Result.Ok(noisy(7))
        Result.Ok(_) -> 42
        Result.Err(_) -> 0

fn main() -> Unit
    ! [Console.print]
    Console.print("result {pick()}")
"#;
    let out = run_aver("panic_discard", src);
    assert!(
        out.contains("noisy ran with 7"),
        "ctor `_`-field fold dropped the arg's effect; got:\n{out}"
    );
    assert!(out.contains("result 42"), "got:\n{out}");
}

#[test]
fn win_const_divisor_fold_still_computes() {
    // The headline win must survive the soundness gate: a const-divisor
    // `Int.div` still folds and computes the right Euclidean quotient.
    let src = r#"module Win
    intent = "const-divisor Int.div folds and still computes correctly."
    exposes [main]
    depends []
    effects [Console]

fn half(a: Int) -> Result<Int, String>
    ? "Divide by a literal 2 — folds to a bare Euclid at MIR."
    Int.div(a, 2)

fn main() -> Unit
    ! [Console.print]
    match half(10)
        Result.Ok(v) -> Console.print("half {v}")
        Result.Err(e) -> Console.print("err {e}")
"#;
    let out = run_aver("win_const_divisor", src);
    assert_eq!(
        out, "half 5",
        "const-divisor fold must still compute; got:\n{out}"
    );
}
