//! Rust backend behavioral parity harness (Wave 0 of the rust-on-MIR port).
//!
//! The pre-existing Rust codegen tests (`rust_codegen_regression.rs`)
//! are all `cargo check`-only: they prove the emitted source *parses
//! and type-checks*, not that it *behaves*. That gap is exactly the
//! class of bug the rust-on-MIR port can introduce — a "covered" fn
//! can emit Rust that fails rustc's borrow checker, or silently drops
//! a policy / replay wrapper while still type-checking and producing
//! identical happy-path stdout. This harness closes the gap by doing
//! a real `cargo build` (the borrow-check) + RUN + behavioral assert.
//!
//! Three behavioral modes — plain stdout parity is NOT enough on its
//! own (a dropped policy or replay wrapper type-checks and produces
//! identical happy-path stdout):
//!
//! - **plain**: `aver compile --target rust` → `cargo build` → run the
//!   binary → assert stdout equals the VM run (`aver run`).
//! - **deny-policy**: compile+run a Disk-write program under a runtime
//!   `aver.toml` that DENIES the write path; assert the built binary
//!   REJECTS the effect at runtime (catches a dropped
//!   `aver_policy::check_*` wrapper).
//! - **record/replay**: `--record` a run then replay it; assert the
//!   recording captures every effect with the right per-effect
//!   arg-json shape, and the replay roundtrips (catches a dropped
//!   `aver_replay::invoke_effect` wrapper).
//!
//! ## Scope
//!
//! Every test here runs on every `cargo test` invocation. It started as a
//! 3-example plain-parity subset plus the two critical behavioral probes
//! (deny-policy, record/replay) and has since grown a tail of single-shape
//! regressions — each one a lowering or ownership bug that only a real
//! build-and-run catches. ~one cargo dep-build, then seconds each. The suite
//! competes for a shared CI budget, so a new case earns its place here only
//! when no cheaper harness can hold the same ground, and only one shape per
//! class.
//!
//! ## Why this is the porting safety net, not theater
//!
//! `rust_codegen_revert.rs` (the sibling self-checking revert-test
//! suite) demonstrates that breaking the HIR emitter — dropping a
//! `.clone()`, dropping the policy wrapper, dropping the replay
//! wrapper — turns each mode RED. A net that passes with AND without
//! the bug proves nothing; the revert evidence is what makes this one
//! trustworthy.
//!
//! Gated on `runtime` (the default feature set) — needs the `aver`
//! binary + the local `aver-rt` runtime that `aver compile` pins.

#![cfg(feature = "runtime")]

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

// ─── Shared infrastructure ──────────────────────────────────────────────

/// Monotonic counter so concurrently-running tests never collide on a
/// temp-dir name even within the same nanosecond.
static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn temp_dir(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-rust-diff-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    dir
}

/// Single `cargo build` target dir shared across every example in one
/// process so the (slow) dependency compile amortises — the first
/// example pays it, the rest are seconds.
fn shared_target_dir() -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            repo_root()
                .join("target")
                .join("rust-codegen-differential-shared")
        })
}

fn binary_name(name: &str) -> String {
    format!("{name}{}", std::env::consts::EXE_SUFFIX)
}

/// `aver run <file>` (VM) — the parity oracle. Returns trimmed stdout.
fn run_vm(file: &Path, module_root: Option<&Path>) -> Result<String, String> {
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root()).arg("run").arg(file);
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    let out = cmd.output().expect("expected `aver run` (VM) to execute");
    if !out.status.success() {
        return Err(format!("VM run failed:\n{}", format_output(&out)));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// Compile `file` to a Rust project at `project_dir`. Extra args are
/// appended verbatim (e.g. `--policy runtime`, `--with-replay`).
fn compile_rust(
    file: &Path,
    project_dir: &Path,
    name: &str,
    module_root: Option<&Path>,
    extra: &[&str],
) -> Result<(), String> {
    compile_rust_env(file, project_dir, name, module_root, extra, &[])
}

/// As [`compile_rust`], but sets extra env vars on the `aver compile`
/// process. The rust-on-MIR HIR walker was deleted in W6/Stage-3, so MIR
/// is the unconditional codegen path and there are no MIR flags left to
/// set — callers pass `&[]`. The env hook is retained for forward use.
fn compile_rust_env(
    file: &Path,
    project_dir: &Path,
    name: &str,
    module_root: Option<&Path>,
    extra: &[&str],
    env: &[(&str, &str)],
) -> Result<(), String> {
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root())
        .arg("compile")
        .arg(file)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg(name)
        .arg("-o")
        .arg(project_dir);
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    cmd.args(extra);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd
        .output()
        .expect("expected `aver compile --target rust` to spawn");
    if !out.status.success() {
        return Err(format!(
            "aver compile --target rust failed:\n{}",
            format_output(&out)
        ));
    }
    Ok(())
}

/// `aver compile … --explain-mir-coverage --target rust --json` →
/// parse the `mir_lowered` count (how many fns the MIR walker emits).
/// Since the HIR walker was deleted (W6/Stage-3) MIR is the sole codegen
/// path, so this is the "MIR is exercised" guard the probes assert up
/// front — a zero would mean the construct under test never reached the
/// walker (so the build / parity assertions would pass for the wrong
/// reason).
///
/// BLIND SPOT — do not guard an effect probe with this. The coverage walk
/// runs `MirEmitCtx::for_test`, whose builtin table is empty, so it counts
/// EVERY fn that calls a builtin (`String.len` as much as
/// `Disk.writeText`) as a fallback. A program whose every fn calls a
/// builtin therefore reports `mir_lowered = 0` however cleanly the
/// production path emits it, and a `> 0` guard on it can only ever fail.
/// Use it on probes that contain at least one builtin-free fn; elsewhere
/// assert on the emitted Rust instead.
fn mir_lowered_count(
    file: &Path,
    module_root: Option<&Path>,
    extra: &[&str],
) -> Result<u64, String> {
    let mut cmd = Command::new(aver_bin());
    cmd.current_dir(repo_root())
        .arg("compile")
        .arg(file)
        .arg("--explain-mir-coverage")
        .arg("--target")
        .arg("rust")
        .arg("--json");
    if let Some(root) = module_root {
        cmd.arg("--module-root").arg(root);
    }
    cmd.args(extra);
    let out = cmd
        .output()
        .expect("expected `aver compile --explain-mir-coverage` to spawn");
    if !out.status.success() {
        return Err(format!(
            "explain-mir-coverage failed:\n{}",
            format_output(&out)
        ));
    }
    let json = String::from_utf8_lossy(&out.stdout);
    // Tiny field extractor — avoids pulling serde into the test.
    let needle = "\"mir_lowered\":";
    let start = json
        .find(needle)
        .ok_or_else(|| format!("no `mir_lowered` field in coverage JSON:\n{json}"))?
        + needle.len();
    let rest = &json[start..];
    let end = rest
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len());
    rest[..end]
        .parse::<u64>()
        .map_err(|e| format!("bad mir_lowered count {:?}: {e}", &rest[..end]))
}

/// `cargo build` the emitted project against the shared target dir.
/// This is a REAL build (not `cargo check`) so move / borrow / Arc
/// bugs that pass `check` but fail `build` surface here. Returns the
/// path to the produced binary.
fn cargo_build(project_dir: &Path, name: &str) -> Result<PathBuf, String> {
    cargo_build_in(project_dir, name, &shared_target_dir())
}

/// As [`cargo_build`], but builds against an explicit target dir. The
/// forced-MIR full-corpus tier passes a PER-EXAMPLE target dir here so
/// the concurrent generated-project builds never share a target tree —
/// `--offline` + a shared `CARGO_TARGET_DIR` races on the `.rmeta` /
/// proc-macro outputs (observed during the W6 audit), and the per-test
/// isolation is what keeps this standing net from being flaky.
fn cargo_build_in(project_dir: &Path, name: &str, target: &Path) -> Result<PathBuf, String> {
    fs::create_dir_all(target).expect("create cargo target dir");
    let out = Command::new("cargo")
        .arg("build")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", target)
        .output()
        .expect("expected `cargo build` to spawn");
    if !out.status.success() {
        return Err(format!(
            "cargo build failed on emitted project:\n{}",
            format_output(&out)
        ));
    }
    Ok(target.join("debug").join(binary_name(name)))
}

/// `cargo test -q --offline` on the emitted project — builds AND runs the
/// `#[cfg(test)]` module that `emit_verify_blocks` generates from verify
/// blocks, which `cargo build` alone never compiles.
fn cargo_test_in(project_dir: &Path, target: &Path) -> Result<(), String> {
    fs::create_dir_all(target).expect("create cargo target dir");
    let out = Command::new("cargo")
        .arg("test")
        .arg("-q")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(project_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", target)
        .output()
        .expect("expected `cargo test` to spawn");
    if !out.status.success() {
        return Err(format!(
            "cargo test failed on emitted project:\n{}",
            format_output(&out)
        ));
    }
    Ok(())
}

// ─── Mode (a): plain stdout parity ──────────────────────────────────────

/// Compile + build + RUN an example, asserting stdout equals the VM.
fn assert_plain_parity(relative: &str, module_root: Option<&str>) -> Result<(), String> {
    assert_plain_parity_then(relative, module_root, |_, _| Ok(()))
}

/// [`assert_plain_parity`], then `then` on the built binary and the stdout
/// both runs gave.
fn assert_plain_parity_then(
    relative: &str,
    module_root: Option<&str>,
    then: impl FnOnce(&Path, &str) -> Result<(), String>,
) -> Result<(), String> {
    let file = repo_root().join(relative);
    if !file.exists() {
        return Err(format!("{relative}: corpus file missing"));
    }
    let root = module_root.map(|r| repo_root().join(r));
    let vm_stdout = run_vm(&file, root.as_deref())?;

    let ws = temp_dir(&sanitise(relative));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("p_{}", sanitise(relative));

    let result = (|| {
        compile_rust(&file, &project, &name, root.as_deref(), &[])?;
        let bin = cargo_build(&project, &name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "{relative}: compiled binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "{relative}: stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        then(&bin, &vm_stdout)
    })();

    let _ = fs::remove_dir_all(&ws);
    result
}

fn sanitise(relative: &str) -> String {
    relative
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect()
}

// ─── Fast tier ──────────────────────────────────────────────────────────

/// 3-example plain-parity subset for the fast (every-CI) tier. Picked
/// for: single-file, deterministic (no Time / Random / Http), exercises
/// records + sum types + match + list ops + recursion.
const FAST_PLAIN: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/shapes.av",
    "examples/core/lists.av",
];

#[test]
fn fast_plain_stdout_parity_with_vm() {
    let mut failures = Vec::new();
    for relative in FAST_PLAIN {
        if let Err(e) = assert_plain_parity(relative, None) {
            failures.push(e);
        }
    }
    assert!(
        failures.is_empty(),
        "{} of {} fast plain-parity examples failed:\n  - {}",
        failures.len(),
        FAST_PLAIN.len(),
        failures.join("\n  - ")
    );
}

/// The branches of an independent product run on threads of their own. They
/// must get the stack `main` gets: on Rust's default 2 MB a recursion that
/// `main` survives aborts the whole process inside a branch.
#[test]
fn deep_recursion_in_a_product_branch_matches_main_on_rust_and_vm() {
    assert_plain_parity("tests/fixtures/deep_recursion_in_product_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A `main` returning a `Result` other than `Result<Unit, String>` runs on
/// the same big-stack thread as every other `main`, not on the process's
/// own 8 MB main thread, where this recursion overflows.
#[test]
fn deep_recursion_under_a_result_int_main_matches_vm() {
    assert_plain_parity("tests/fixtures/deep_recursion_in_result_main_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A value of a recursive sum is one pointer to a node holding its
/// constructor's tag and fields, matched through a view: alone, in a tuple,
/// with a name bound to the whole value, in a self tail call, compared with
/// `==` on shared nodes, and ordered as a `Map` key by constructor name.
#[test]
fn recursive_sum_as_one_pointer_matches_vm() {
    assert_plain_parity("tests/fixtures/recursive_sum_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// `==` on a recursive sum compares two pointers before it reads either
/// node: terms rebuilt above shared leaves, equal or apart at each level, a
/// sum holding a `Float` (which does not take that shortcut), deferred calls
/// (which do not either), two threads at once, and chains a million
/// applications deep leaning either way, compared with no stack overflow.
#[test]
fn equality_of_terms_with_shared_subterms_matches_vm() {
    assert_plain_parity_then("tests/fixtures/equal_terms_app.av", None, |bin, vm| {
        let out = Command::new(bin)
            .arg("1000000")
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        let deep = String::from_utf8_lossy(&out.stdout).trim().to_string();
        match out.status.success() && deep == vm {
            true => Ok(()),
            false => Err(format!("at a million levels:\n{}", format_output(&out))),
        }
    })
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A refinement carrier whose smart constructor proves a bound within `i64`
/// is a struct over a raw `i64` field: a word and a hash in the shape of a
/// kernel's terms (a recursive sum carrying its word first, words computed
/// from words with remainders, quotients, masks and shifts, a map keyed by
/// hash and one keyed by word), and a carrier as a variable, a record and
/// variant field, a list, option and tuple element, a map key and value, at
/// its bound edges, refused by its smart constructor, updated by a literal,
/// past `i64` and back, and built from a value outside its smart constructor
/// (which keeps the `Int` field), all read as the VM; its verify cases pass
/// as tests, and the build with every carrier boxed reads the same.
#[test]
fn refinement_carriers_held_as_i64_match_vm() {
    let relative = "tests/fixtures/refined_int_carrier/main.av";
    let root = repo_root().join("tests/fixtures/refined_int_carrier");
    let file = repo_root().join(relative);
    let vm_stdout = run_vm(&file, Some(&root)).unwrap_or_else(|e| panic!("{e}"));
    let ws = temp_dir("refined_int_carrier");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&file, &project, "refined_int_carrier", Some(&root), &[])?;
        let emitted = fs::read_to_string(project.join("src/aver_generated/lib/word/mod.rs"))
            .map_err(|e| format!("read emitted word module: {e}"))?;
        if !emitted.contains("pub value: i64,") {
            return Err(format!("the word is not held as an i64:\n{emitted}"));
        }
        let bin = cargo_build(&project, "refined_int_carrier")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if !out.status.success() || rust_stdout != vm_stdout {
            return Err(format!(
                "stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{}",
                format_output(&out)
            ));
        }
        cargo_test_in(&project, &shared_target_dir())?;
        // The same program with every carrier on its `AverInt` field reads
        // the same: the field changed its representation, not a value.
        let boxed = ws.join("boxed");
        fs::create_dir_all(&boxed).expect("create boxed project dir");
        compile_rust_env(
            &file,
            &boxed,
            "refined_int_carrier_boxed",
            Some(&root),
            &[],
            &[("AVER_NO_CARRIER_I64", "1")],
        )?;
        let emitted = fs::read_to_string(boxed.join("src/aver_generated/lib/word/mod.rs"))
            .map_err(|e| format!("read emitted word module: {e}"))?;
        if !emitted.contains("pub value: aver_rt::AverInt,") {
            return Err(format!(
                "AVER_NO_CARRIER_I64 kept the i64 field:\n{emitted}"
            ));
        }
        let bin = cargo_build(&boxed, "refined_int_carrier_boxed")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        match String::from_utf8_lossy(&out.stdout).trim() == vm_stdout {
            true => Ok(()),
            false => Err(format!("boxed carriers diverged:\n{}", format_output(&out))),
        }
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// `lazy f(…)` keeps the call in a node until the value is first looked at:
/// a value never looked into, one looked at many times and from two threads
/// at once, compared both ways with the value built at once, a spine a
/// million deep looked at one level at a time, a million calls never made,
/// each holding the one before, and a call deferred from another module, all
/// read as the VM, which makes every call at once.
#[test]
fn deferred_calls_match_vm() {
    assert_plain_parity(
        "tests/fixtures/lazy_call/main.av",
        Some("tests/fixtures/lazy_call"),
    )
    .unwrap_or_else(|e| panic!("{e}"));
}

/// `lazy f(…) knowing f.law(…)` keeps the value a proved law gives of the
/// call's result in the deferred node: its projection read before the call
/// is made, the call made later, two calls that know different values
/// compared, a call from another module, a call under a `when` that does not
/// hold (made at once), and the instantiation shape of a kernel, all read as
/// the VM, which makes every call at once.
#[test]
fn deferred_calls_that_know_a_value_match_vm() {
    assert_plain_parity(
        "tests/fixtures/lazy_knowing/main.av",
        Some("tests/fixtures/lazy_knowing"),
    )
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A law `--trust-laws` lets through unproved is checked every time a call
/// that knows a value from it is made: a false one stops the program with
/// the law's name, though the value it gave was read before.
/// Lists and integers the Rust runtime takes over where the VM copies: a
/// `List<Int>` and a `Bytes` accumulator grown by `concat` while a list it
/// started from is still read, `reverse` of a list handed over and of one
/// still held, a long chain dropped over a tail that is printed after,
/// 64-bit rotations and `Bits.low` around every word boundary on large and
/// negative values, `Vector.fromList` and `Map.values`, all read as the VM.
#[test]
fn lists_handed_over_and_large_integers_match_vm() {
    assert_plain_parity(
        "tests/fixtures/runtime_lists_ints/main.av",
        Some("tests/fixtures/runtime_lists_ints"),
    )
    .unwrap_or_else(|e| panic!("{e}"));
}

#[test]
fn a_trusted_law_that_does_not_hold_stops_the_program_when_the_call_is_made() {
    const SOURCE: &str = "module Trusted\n    intent = \"a deferred call knows a value from a law that does not hold\"\n    effects [Console.print]\n\ntype Term\n    Var(Int, Int)\n    App(Term, Term, Int)\n\nfn wordOf(t: Term) -> Int\n    ? \"The word a term carries.\"\n    match t\n        Term.Var(_, w) -> w\n        Term.App(_, _, w) -> w\n\nfn shift(t: Term, by: Int) -> Term\n    ? \"Every variable moved by `by`.\"\n    match t\n        Term.Var(n, _) -> Term.Var(n + by, wordOf(t) * 31 + by)\n        Term.App(f, a, _) -> Term.App(shift(f, by), shift(a, by), wordOf(t) * 31 + by)\n\nverify shift law wordGuess\n    given t: Term = [Term.Var(0, 5)]\n    given by: Int = [0]\n    wordOf(shift(t, by)) => wordOf(t) * 31\n\nfn main() -> Unit\n    ! [Console.print]\n    t = Term.Var(0, 1)\n    later = lazy shift(t, 2) knowing shift.wordGuess(t, 2)\n    Console.print(\"known {wordOf(later)}\")\n    Console.print(\"made {wordOf(shift(later, 0))}\")\n";
    let ws = temp_dir("trusted_law");
    let result = (|| {
        let file = ws.join("main.av");
        fs::write(&file, SOURCE).expect("write program");
        let project = ws.join("p_trusted");
        fs::create_dir_all(&project).expect("create project dir");
        compile_rust(&file, &project, "p_trusted", Some(&ws), &["--trust-laws"])?;
        let bin = cargo_build(&project, "p_trusted")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        if out.status.success()
            || stdout.trim() != "known 31"
            || !stderr.contains(
                "law `shift.wordGuess` of module Trusted does not hold: it says `wordOf` of the call is 31, and it is 33",
            )
        {
            return Err(format!("expected a stop naming the law:\n{}", format_output(&out)));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Constant folding can turn `0.0 / 0.0` or `1.0 / 0.0` into NaN or an
/// infinity, which have no Rust literal; they emit as the `f64` constants.
#[test]
fn folded_nan_and_infinity_constants_match_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/float_special_constants_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A worker that hands its String to another module's indexed helper passes
/// the index it holds instead of going through the wrapper, which would index
/// the whole string again on every call. Multibyte characters make the index
/// a real table; the call on a string of its own keeps the wrapper.
#[test]
fn a_cross_module_indexed_call_forwards_the_index_on_rust_and_vm() {
    let root = repo_root().join("tests/fixtures/string_index_cross_module");
    let file = root.join("main.av");
    assert_eq!(
        run_vm(&file, Some(&root)).expect("vm run"),
        "ab|😀ą||tail\n10"
    );
    assert_plain_parity(
        "tests/fixtures/string_index_cross_module/main.av",
        Some("tests/fixtures/string_index_cross_module"),
    )
    .unwrap_or_else(|e| panic!("{e}"));

    let ws = temp_dir("string_index_cross_module");
    compile_rust(&file, &ws, "p_string_index_cross_module", Some(&root), &[])
        .expect("compile to Rust");
    let entry = fs::read_to_string(ws.join("src/aver_generated/entry/mod.rs"))
        .expect("read the entry module");
    let _ = fs::remove_dir_all(&ws);
    assert!(
        entry.contains("scan::skipTo__indexed(") && entry.contains("scan::skipTo(aver_rt::AverInt"),
        "the worker forwards its index, the other call keeps the wrapper:\n{entry}"
    );
}

/// `Bytes.toHex` builds its string with the list-driven loop that the
/// deforestation pass now recognises, and BOTH backends now fuse it —
/// but by different routes. The Rust build reads the dependency out of
/// the `ModuleInfo` the compile driver deforested; the VM re-parses the
/// dependency off disk and re-runs the pass against the entry's symbol
/// table. Two independent producers of the same rewrite is exactly the
/// shape where a cheaper harness cannot hold the ground: only a real
/// build-and-run proves they agree on the bytes.
#[test]
fn fused_stdlib_hex_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/stdlib_bytes_hex_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// The decoding direction, which the chars-fusion pass rewrites twice
/// over: `Bytes.parseHexChars` walks a cursor instead of the list
/// `String.chars` would have built, and `Bytes.hexDigitValue` compares
/// codepoints instead of sixteen strings. Both rewrites reach the two
/// backends by the two different routes the encoding test describes, and
/// the `Result.Err` arms are in the program on purpose: their message
/// bytes are built from the character the cursor read, so a cursor that
/// read the wrong one shows up as a changed message rather than a
/// changed count.
#[test]
fn fused_stdlib_hex_decoding_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/stdlib_bytes_dehex_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A recursive sum keeps each constructor's fields behind one shared `Arc`.
/// The fixture holds the shapes that representation must build and read: a
/// record on the cycle (which had no fixed size in Rust at all), recursion
/// through `Option`, a constructor pattern inside a tuple, equality, and one
/// value used as both fields of a node, 18 levels deep — 262144 leaves, each
/// level a single allocation.
///
/// `==` on two such values built apart, a record on the cycle and two sums
/// on one cycle compares each shared pair once, not once per path to it, and
/// a map keyed by such values hashes each shared node once: at 64 levels a
/// walk of the tree would not end, so the compiled binary must answer as it
/// does at 12 within a minute.
#[test]
fn shared_recursive_sum_matches_between_rust_and_vm() {
    assert_plain_parity_then(
        "tests/fixtures/shared_recursive_sum_app.av",
        None,
        |bin, shallow| {
            let mut child = Command::new(bin)
                .arg("64")
                .stdout(std::process::Stdio::piped())
                .spawn()
                .map_err(|e| format!("failed to run compiled binary: {e}"))?;
            let started = std::time::Instant::now();
            while child.try_wait().expect("binary is waitable").is_none() {
                if started.elapsed() > std::time::Duration::from_secs(60) {
                    let _ = child.kill();
                    return Err("`==` at 64 levels did not end within a minute".into());
                }
                std::thread::sleep(std::time::Duration::from_millis(20));
            }
            let out = child.wait_with_output().expect("binary output");
            let deep = String::from_utf8_lossy(&out.stdout).trim().to_string();
            match deep == shallow {
                true => Ok(()),
                false => Err(format!("at 64 levels:\n{deep}\nat 12:\n{shallow}")),
            }
        },
    )
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A value of a recursive sum a million levels deep is let go whole: a chain,
/// a tree leaning either way, a chain through a record and an `Option`, and
/// two sums on one cycle. Each node freed inside its parent's free would take
/// more than the 256 MB of stack `main` has in a debug build; the binary
/// must end within a minute and print the depths.
#[test]
fn a_deep_recursive_sum_value_is_freed_on_rust() {
    assert_plain_parity_then(
        "tests/fixtures/deep_recursive_sum_drop_app.av",
        None,
        |bin, _| {
            let mut child = Command::new(bin)
                .arg("1000000")
                .stdout(std::process::Stdio::piped())
                .stderr(std::process::Stdio::piped())
                .spawn()
                .map_err(|e| format!("failed to run compiled binary: {e}"))?;
            let started = std::time::Instant::now();
            while child.try_wait().expect("binary is waitable").is_none() {
                if started.elapsed() > std::time::Duration::from_secs(60) {
                    let _ = child.kill();
                    return Err("a million levels did not end within a minute".into());
                }
                std::thread::sleep(std::time::Duration::from_millis(20));
            }
            let out = child.wait_with_output().expect("binary output");
            let expected = ["chain", "left", "right", "linked", "ping"]
                .map(|case| format!("{case} 1000000"))
                .join("\n");
            match out.status.success() && String::from_utf8_lossy(&out.stdout).trim() == expected {
                true => Ok(()),
                false => Err(format!("at a million levels:\n{}", format_output(&out))),
            }
        },
    )
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A constructor much larger than the others in a sum that is not recursive
/// goes behind an `Arc`, and `==` compares a node's scalar fields before its
/// children. The fixture builds, matches, compares and keys a map on the
/// large constructor, and compares nodes that declare their hash, a `Float`
/// (NaN included) or a field-less sum after a list or a subtree.
#[test]
fn large_constructors_and_equality_order_match_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/sum_layout_and_equality_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A fallback that only computes goes in a closure (`unwrap_or_else`,
/// `ok_or_else`), so a present value never builds it, and a fallback with an
/// effect still runs first: `fallback 7 ran` prints for the present value
/// too. The closure has to pass the borrow check, so the program is built and
/// run; the VM's side of the same fixture is pinned in `mir_vm_codegen.rs`.
#[test]
fn a_computed_fallback_runs_only_when_missing_on_rust_and_vm() {
    let relative = "tests/fixtures/lazy_fallback_app.av";
    let file = repo_root().join(relative);
    assert_eq!(
        run_vm(&file, None).expect("vm run"),
        "pick 3 65536\npickOk 4 65536\nfound ok 5 err 65536\nfallback 7 ran\nnoisy 1\n\
         fallback 7 ran\nnoisy 7\ntotal 210"
    );
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));

    let ws = temp_dir("lazy_fallback");
    compile_rust(&file, &ws, "p_lazy_fallback", None, &[]).expect("compile to Rust");
    let entry = fs::read_to_string(ws.join("src/aver_generated/entry/mod.rs"))
        .expect("read the entry module");
    let _ = fs::remove_dir_all(&ws);
    for lazy in [
        "unwrap_or_else(|| fresh())",
        "unwrap_or_else(|_| fresh())",
        "ok_or_else(|| ",
    ] {
        assert!(entry.contains(lazy), "missing `{lazy}`:\n{entry}");
    }
    assert!(
        entry.contains("unwrap_or(noisy("),
        "the effectful fallback runs first:\n{entry}"
    );
}

/// A fallback with an effect runs before the value is looked at, present or
/// missing: Rust prints the written output, the same `cross_backend_stress`
/// and `wasip2_stress` hold the VM, wasm-gc and wasip2 to.
#[test]
fn effectful_fallback_runs_first_on_rust() {
    let relative = "tests/fixtures/effectful_fallback_app.av";
    let expected = include_str!("fixtures/effectful_fallback_app.expected").trim();
    assert_plain_parity_then(relative, None, |_, stdout| {
        (stdout == expected)
            .then_some(())
            .ok_or_else(|| format!("unexpected output:\n{stdout}"))
    })
    .unwrap_or_else(|e| panic!("{e}"));
}

/// The error of `Result.fromOption` may unwrap another value itself: Rust
/// prints the written output `cross_backend_stress` and `wasip2_stress`
/// hold the VM, wasm-gc and wasip2 to.
#[test]
fn nested_unwrap_in_error_keeps_value_on_rust() {
    let relative = "tests/fixtures/nested_unwrap_error_app.av";
    let expected = include_str!("fixtures/nested_unwrap_error_app.expected").trim();
    assert_plain_parity_then(relative, None, |_, stdout| {
        (stdout == expected)
            .then_some(())
            .ok_or_else(|| format!("unexpected output:\n{stdout}"))
    })
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A two-arm bool `match` whose default arm NAMES the matched value
/// (`other -> …`) instead of spelling `false` or `_`.
///
/// The MIR pass that turns a bool match into `IfThenElse` used to
/// recognise only `false` and `_` in that slot, so the named form
/// reached the backends as a live `Match` — and every backend answers
/// that differently: the VM ran it, wasm-gc emitted a module that
/// trapped on `unreachable`, and the Rust walker substituted
/// `compile_error!("MIR walker could not render fn …")`. Three answers
/// for one program is what makes this a build-and-run case rather than
/// a `cargo check` one: the Rust half only proves itself against a VM
/// run of the same source.
///
/// The fixture carries both halves of the rewrite, because they fail
/// apart: `abs` never reads its binder (the reinstated `let` must be
/// dead-code-eliminated, and a wrong constant there would go unnoticed),
/// while `describe` prints its binder — pinning the value the arm binds
/// as `false`, not a second evaluation of the subject.
#[test]
fn bool_match_named_default_arm_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/bool_match_named_default_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// Two different keys the VM hashes to the same 64 bits, as Int and as
/// String. The VM addressed its map by that hash alone and merged them; the
/// generated Rust keys a real hash table on the key itself and never did.
///
/// Both are held to the written expected output, each on its own: agreeing
/// with each other would also pass if both were wrong the same way, and the
/// other test that reads this file for the VM is only built with `wasm`.
#[test]
fn colliding_map_keys_match_the_expected_output_on_rust_and_vm() {
    let relative = "tests/fixtures/map_colliding_keys_app.av";
    let expected = include_str!("fixtures/map_colliding_keys_app.expected").trim();
    let file = repo_root().join(relative);

    let vm_stdout = run_vm(&file, None).unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(vm_stdout, expected, "the VM's output");

    let ws = temp_dir(&sanitise(relative));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("p_{}", sanitise(relative));
    let rust_stdout = (|| {
        compile_rust(&file, &project, &name, None, &[])?;
        let bin = cargo_build(&project, &name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "compiled binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
    })();
    let _ = fs::remove_dir_all(&ws);
    assert_eq!(
        rust_stdout.unwrap_or_else(|e| panic!("{e}")),
        expected,
        "the compiled Rust binary's output"
    );
}

/// Generic List, Map and Vector builtins over an empty literal nothing
/// else types (`List.len([])`, `Map.len({})`, `[] == []`). rustc cannot
/// infer the element type of `AverList::empty().len()`, so the walker names
/// the type the checker settled on; the run must print what the VM prints.
/// An annotated empty literal bound to a name (`m: Map<Int, Int> = {}` read
/// only by `Map.has`) names its type the same way.
#[test]
fn empty_literal_operands_match_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/empty_literal_operands_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// An empty literal bound to a name without a type, settled by a later
/// read (`e = {}` then `Map.set(e, 1, "a")`). The checker stamps the
/// binding with the type that read settles, so the crate builds.
/// `tests/cross_backend_stress.rs` runs the same file on wasm-gc.
#[test]
fn bound_empty_literal_settled_later_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/bound_empty_settled_later_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// Map keys told apart by value and maps iterated in key order: records,
/// tuples and lists of strings that print like separators, sums, numbers,
/// options and booleans. `tests/cross_backend_stress.rs` runs the same file
/// on wasm-gc and the self-host.
#[test]
fn map_key_identity_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/map_key_identity_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// `String.fromFloat` and interpolation of NaN, the infinities, `-0.0` and
/// ordinary numbers. `tests/cross_backend_stress.rs` runs the same file on
/// wasm-gc and the self-host.
#[test]
fn float_text_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/float_text_app.av", None).unwrap_or_else(|e| panic!("{e}"));
}

/// An interpolated String in a module-level statement. The interpolation
/// pass lowered function bodies only, so a top-level `"a{1}b"`, bound or
/// bare, reached the Rust walker as a raw interpolation and the crate got
/// `compile_error!("MIR walker could not render a top-level statement")`.
/// Found by the nightly `fuzz_codegen_rust` run.
#[test]
fn top_level_interpolation_matches_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/top_level_interpolation_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A match on a tuple literal whose arms carry list, literal or Option
/// patterns in the tuple's elements — `([], 0)`, `([next, ..rest], n)`,
/// `(true, 0)`, `(Option.Some(task), 0)`, `(Option.None, _)` — and one
/// whose arms carry binds alone. The Rust walker used to refuse the first
/// three functions with `MIR walker could not render fn`: an `AverInt` is
/// not a pattern literal and an `AverList` is not a slice, so such a tuple
/// arm has no structural Rust pattern and lowers to a guard chain over the
/// cloned elements instead. Only a build proves that chain: the `pick`
/// shape is exhaustive over its `Option` tags and ends in no default arm,
/// which the chain builder used to close with an unbraced `else`. Three
/// more shapes put a `String` literal in an element (`("==", Option.Some(s))`,
/// `(0, ("zero", true))`, `(Shape.Circle(r), "big")`): the element is an
/// `AverStr`, which Rust cannot match against a `&str` literal, so the
/// literal becomes a guard — on the arm of a plain `match`, or one more
/// conjunct of the chain when an `Int` literal sits beside it. Every arm of
/// every shape must print what the VM prints.
#[test]
fn tuple_match_with_list_literal_and_option_elements_matches_between_rust_and_vm() {
    const FIXTURE: &str = "tests/fixtures/tuple_match_element_patterns_app.av";
    let vm = run_vm(&repo_root().join(FIXTURE), None).unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(vm.lines().count(), 21, "one line per arm:\n{vm}");
    assert_plain_parity(FIXTURE, None).unwrap_or_else(|e| panic!("{e}"));
}

/// Nested literal / constructor patterns and list patterns (`Option.Some(0)`,
/// `Shape.Rect(0, _)`, `[a, b, ..rest]`, `[Option.Some(x), ..rest]`). The
/// front door compiles them into nested flat matches over fresh binders
/// before any backend runs; only a build proves the Rust those matches
/// render borrows and moves the bound sub-values correctly, and only a run
/// proves the compiled decision tree takes the arm the VM takes. The fixture
/// also holds the wild list shapes (`[_, .._]`, `[_]`, `[_, _]`, `[x, .._]`,
/// `[_, ..rest]`, and wild lists inside a constructor or a tuple) over a
/// borrowed parameter: an arm with no binder lets the match borrow its
/// subject, and `[_, .._]` used to uncons that borrow and fail to build
/// (#1450).
#[test]
fn nested_literal_and_list_patterns_match_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/nested_patterns.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A field of a recursive type bound inside a tuple pattern and used as a
/// value: `match (t, k)` with the arm `(Ty.TyList(a), _) -> a`. The generated
/// type boxes a recursive field, and the arm unboxed it only when the
/// constructor was the whole pattern, so inside a tuple `a` stayed an
/// `Arc<Ty>` where a `Ty` was expected and the crate did not build. The
/// fixture puts the constructor in each tuple position, beside a string
/// literal, in a tuple inside a tuple, and under another constructor, and
/// both returns the field and stores it in a new constructor. `check`,
/// `verify`, the VM and wasm-gc accept all of it; only a build shows the
/// type error.
#[test]
fn recursive_field_bound_in_a_tuple_pattern_matches_between_rust_and_vm() {
    const FIXTURE: &str = "tests/fixtures/tuple_pattern_recursive_field_app.av";
    let vm = run_vm(&repo_root().join(FIXTURE), None).unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(
        vm, "first 1 0\nsecond 1 0\nboth 2 2 0\ndeeper 3 1 2\ninside 1 1 2\nwrapped 1 2 0 2",
        "the VM is the oracle and must print the expected lines"
    );
    assert_plain_parity(FIXTURE, None).unwrap_or_else(|e| panic!("{e}"));
}

/// A value read twice in one expression: once where it is kept, and once
/// inside a part that hands it on (`Clock.update(later(c, n + leftMs),
/// answeredMs = leftMs)`, #1454). The last-use facts follow Aver's order —
/// a record update's base before its fields, operands and arguments left to
/// right — and mark the later read as the one that moves. Generated Rust has
/// to evaluate in that order and must not hold a borrow of the earlier read
/// across the move: `..base` runs after the fields in Rust, and a method
/// receiver, a comparison operand or a borrowed argument stays borrowed
/// while later operands run. A base that moves out of the record in place
/// (`..shelf.box` after `..shelf.box.clock` in a nested update) stays last,
/// where Rust moves only what the fields left. `check`, `verify` and the VM
/// all accept every shape here; only a build proves the Rust does, and only
/// a run proves it prints what the VM prints.
#[test]
fn value_read_twice_in_one_expression_matches_between_rust_and_vm() {
    assert_plain_parity(
        "tests/fixtures/value_read_twice_in_one_expression_app.av",
        None,
    )
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A one-arm wildcard match over an effectful call: `match say(x)` with a
/// single `_ ->` arm is how a process performs something in place before it
/// goes on, and the Rust backend used to render the arm's body alone, so the
/// subject — and its effect — never ran. Only a build-and-run sees the
/// missing line; `cargo check` and the VM are both happy.
#[test]
fn wildcard_match_still_runs_its_subject_between_rust_and_vm() {
    assert_plain_parity("tests/fixtures/wildcard_match_subject_app.av", None)
        .unwrap_or_else(|e| panic!("{e}"));
}

/// A type declared in a module nobody imports must not reach the code the
/// other modules generate.
///
/// `Domain.State` declares a sum type `Step` and `Domain.User` — which
/// depends on it and on nothing else — spells that name bare. `Domain.Tally`
/// declares a record that happens to be called `Step` too, and `Domain.User`
/// never imports it. The bare-name lookup used to scan the whole program, so
/// the second declaration made the name ambiguous, the lookup answered
/// `None`, and `None` reads downstream as "not a user constructor": the Rust
/// backend substituted `compile_error!` for every function that touched it
/// while `check`, `verify` and `compile` all reported success. The failure
/// surfaced only at `cargo build`.
///
/// Nothing cheaper than a real build settles this: the emitted crate is
/// exactly where the substitution lands.
#[test]
fn a_type_in_an_unimported_module_does_not_break_the_generated_crate() {
    let root = repo_root().join("tests/fixtures/bare_type_scope");
    let file = root.join("app/entry.av");
    let vm_stdout = run_vm(&file, Some(&root)).unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(
        vm_stdout, "7 1",
        "fixture should print both Steps' payloads"
    );

    let ws = temp_dir("bare_type_scope");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "p_bare_type_scope";

    let result = (|| {
        compile_rust(&file, &project, name, Some(&root), &[])?;
        let unrenderable: Vec<String> = walk_rust_sources(&project)
            .into_iter()
            .filter(|(_, content)| content.contains("compile_error!"))
            .map(|(path, _)| path)
            .collect();
        if !unrenderable.is_empty() {
            return Err(format!(
                "emitted crate carries a deliberate compile error in:\n  - {}",
                unrenderable.join("\n  - ")
            ));
        }
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "compiled binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// What `main` returns is the exit status of the compiled binary, as on the
/// other runners (`main_exit_status_spec.rs`): an `Int` in 0..=255 is the
/// status, any other `Int` is 1 with the reason on stderr, and `Result.Err`
/// is 1. Only a real build runs the generated `fn main`.
#[test]
fn main_return_is_the_exit_status_of_the_compiled_binary() {
    const ARG_INT: &str = "module ExitArg\n    intent = \"main returns the status it reads\"\n    effects [Args.get]\n\nfn fromArg(arg: String) -> Int\n    ? \"The argument as an Int, 99 when it is not one.\"\n    match Int.fromString(arg)\n        Result.Ok(n) -> n\n        Result.Err(_) -> 99\n\nfn main() -> Int\n    ! [Args.get]\n    match Args.get()\n        [] -> 3\n        [arg, ..rest] -> fromArg(arg)\n";
    const ERR: &str = "module ExitErr\n    intent = \"main returns an error\"\n\nfn main() -> Result<Unit, String>\n    Result.Err(\"boom\")\n";
    let ws = temp_dir("main_exit_status");
    let result = (|| {
        let mut binaries = Vec::new();
        for (name, source) in [("p_exit_int", ARG_INT), ("p_exit_err", ERR)] {
            let file = ws.join(format!("{name}.av"));
            fs::write(&file, source).expect("write program");
            let project = ws.join(name);
            fs::create_dir_all(&project).expect("create project dir");
            compile_rust(&file, &project, name, Some(&ws), &[])?;
            binaries.push(cargo_build(&project, name)?);
        }
        let status = |bin: &Path, args: &[&str]| -> Result<(Option<i32>, String), String> {
            let out = Command::new(bin)
                .args(args)
                .output()
                .map_err(|e| format!("failed to run compiled binary: {e}"))?;
            Ok((
                out.status.code(),
                String::from_utf8_lossy(&out.stderr).into_owned(),
            ))
        };
        let expected: [(&[&str], i32); 5] = [
            (&[], 3),
            (&["0"], 0),
            (&["255"], 255),
            (&["256"], 1),
            (&["-1"], 1),
        ];
        for (args, code) in expected {
            let (got, stderr) = status(&binaries[0], args)?;
            if got != Some(code) {
                return Err(format!(
                    "args {args:?}: exit {got:?}, expected {code}\n{stderr}"
                ));
            }
        }
        let (_, stderr) = status(&binaries[0], &["100000000000000000000"])?;
        if !stderr.contains("main returned exit code 100000000000000000000, outside 0..255") {
            return Err(format!("a Big status does not say why:\n{stderr}"));
        }
        let (got, stderr) = status(&binaries[1], &[])?;
        if got != Some(1) {
            return Err(format!("Result.Err: exit {got:?}, expected 1\n{stderr}"));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A `main` run once ends the process at its tail without freeing what it
/// still holds (`aver_rt::exit_after_main`). What the process says does not
/// change: stdout, stderr and the exit status match the VM and the earlier
/// Rust output whether the output goes to a pipe or to a file, and the
/// benchmark loop still runs `main` as many times as it is asked to.
#[test]
fn main_that_ends_the_process_at_its_tail_loses_no_output() {
    const UNIT: &str = "module EndUnit\n    intent = \"main prints many lines while it holds a list\"\n    effects [Console.print, Console.error]\n\nfn build(n: Int, acc: List<String>) -> List<String>\n    ? \"n numbered strings.\"\n    match n == 0\n        true -> acc\n        false -> build(n - 1, List.prepend(\"line \" + String.fromInt(n), acc))\n\nfn emit(xs: List<String>) -> Unit\n    ? \"Prints every string.\"\n    ! [Console.print]\n    match xs\n        [] -> Unit\n        [x, ..rest] -> emitOne(x, rest)\n\nfn emitOne(x: String, rest: List<String>) -> Unit\n    ? \"Prints one string, then the rest.\"\n    ! [Console.print]\n    Console.print(x)\n    emit(rest)\n\nfn main()\n    ! [Console.print, Console.error]\n    xs = build(3000, [])\n    emit(xs)\n    Console.error(\"to stderr\")\n    Console.print(\"last of {List.len(xs)}\")\n";
    const INT: &str = "module EndInt\n    intent = \"main returns a status while it holds a list\"\n    effects [Console.print]\n\nfn main() -> Int\n    ! [Console.print]\n    xs = [\"a\", \"b\"]\n    Console.print(\"held {List.len(xs)}\")\n    7\n";
    const ERR: &str = "module EndErr\n    intent = \"main returns an error while it holds a list\"\n    effects [Console.print]\n\nfn main() -> Result<Unit, String>\n    ! [Console.print]\n    xs = [\"a\", \"b\"]\n    Console.print(\"held {List.len(xs)}\")\n    Result.Err(\"boom\")\n";
    let ws = temp_dir("main_tail_exit");
    let result = (|| {
        for (name, source, stderr) in [
            ("p_end_unit", UNIT, "to stderr\n"),
            ("p_end_int", INT, ""),
            ("p_end_err", ERR, "boom\n"),
        ] {
            let file = ws.join(format!("{name}.av"));
            fs::write(&file, source).expect("write program");
            let vm = Command::new(aver_bin())
                .arg("run")
                .arg(&file)
                .output()
                .expect("expected `aver run` (VM) to execute");
            let project = ws.join(name);
            fs::create_dir_all(&project).expect("create project dir");
            compile_rust(&file, &project, name, Some(&ws), &[])?;
            let entry = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
                .expect("read generated entry module");
            if !entry.contains("aver_rt::exit_after_main(") {
                return Err(format!("{name}: the tail of main does not end the process"));
            }
            let bin = cargo_build(&project, name)?;
            let piped = Command::new(&bin)
                .output()
                .map_err(|e| format!("failed to run compiled binary: {e}"))?;
            let out_file = ws.join(format!("{name}.out"));
            let err_file = ws.join(format!("{name}.err"));
            let to_files = Command::new(&bin)
                .stdout(fs::File::create(&out_file).expect("create stdout file"))
                .stderr(fs::File::create(&err_file).expect("create stderr file"))
                .status()
                .map_err(|e| format!("failed to run compiled binary: {e}"))?;
            let runs = [
                ("pipe", piped.status.code(), piped.stdout, piped.stderr),
                (
                    "file",
                    to_files.code(),
                    fs::read(&out_file).expect("read stdout file"),
                    fs::read(&err_file).expect("read stderr file"),
                ),
            ];
            for (how, code, stdout, got_stderr) in runs {
                if code != vm.status.code()
                    || stdout != vm.stdout
                    || got_stderr != stderr.as_bytes()
                {
                    return Err(format!(
                        "{name} to a {how}: exit {code:?}, stdout {} bytes, stderr {:?}; VM: exit {:?}, stdout {} bytes",
                        stdout.len(),
                        String::from_utf8_lossy(&got_stderr),
                        vm.status.code(),
                        vm.stdout.len(),
                    ));
                }
            }
            if name == "p_end_unit" {
                let bench = Command::new(&bin)
                    .env("AVER_BENCH_ITER", "2")
                    .output()
                    .map_err(|e| format!("failed to run compiled binary: {e}"))?;
                let twice = [vm.stdout.as_slice(), vm.stdout.as_slice()].concat();
                if !bench.status.success() || bench.stdout != twice {
                    return Err(format!(
                        "{name} under AVER_BENCH_ITER=2: exit {:?}, stdout {} bytes, expected {}",
                        bench.status.code(),
                        bench.stdout.len(),
                        twice.len()
                    ));
                }
            }
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Every `.rs` file under `dir`, as (display path, contents).
fn walk_rust_sources(dir: &Path) -> Vec<(String, String)> {
    let mut out = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(current) = stack.pop() {
        let Ok(entries) = fs::read_dir(&current) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|ext| ext == "rs")
                && let Ok(content) = fs::read_to_string(&path)
            {
                out.push((path.display().to_string(), content));
            }
        }
    }
    out
}

/// Chars fusion, on the shapes where the two backends could disagree.
///
/// The cursor steps by CODEPOINTS because `String.chars` yields
/// codepoints — a byte cursor answers 3 for `"éx"` and flips every
/// even-length check on multibyte input. The case-folded match keeps
/// whatever arm `String.toLower` would have chosen, `U+212A KELVIN SIGN`
/// (lowercase `"k"`) and `U+0130` (lowercase: two characters, so no
/// single-character arm) included. The VM and the Rust backend lower all
/// of this separately, so only running both settles it.
#[test]
fn rust_chars_fusion_matches_vm() {
    let src = r#"module CharsFusionDifferential
    intent = "Every chars-fusion shape in one program, for cross-backend agreement"
    effects [Console.print]

fn count(chars: List<String>, acc: Int) -> Int
    ? "The canonical linear traversal — one cursor step per character."
    match chars
        [] -> acc
        [head, ..tail] -> count(tail, acc + 1)

fn pairs(chars: List<String>, acc: List<String>) -> List<String>
    ? "Two cells per step, so an odd length has to fall out of the middle test."
    match chars
        [] -> List.reverse(acc)
        [first, ..afterFirst] -> match afterFirst
            [] -> List.reverse(List.prepend("{first}?", acc))
            [second, ..rest] -> pairs(rest, List.prepend("{first}{second}", acc))

fn value(digit: String) -> Int
    ? "Sixteen single-character arms behind a case fold."
    match String.toLower(digit)
        "0" -> 0
        "9" -> 9
        "a" -> 10
        "f" -> 15
        "k" -> 20
        _ -> -1

fn shape(text: String) -> String
    ? "Length, parity, and the pair split for one input."
    n = count(String.chars(text), 0)
    "{n}:{Int.mod(n, 2)}:{String.join(pairs(String.chars(text), []), "-")}"

fn decode(chars: List<String>, acc: Int) -> String
    ? "The codepoint-call shape: the classifier takes the character's code, and the error arm still prints the character it read."
    match chars
        [] -> "ok:{acc}"
        [head, ..tail] -> match value(head) < 0
            true -> "bad '{head}' after {acc}"
            false -> decode(tail, acc + value(head))

fn total(text: String) -> String
    ? "Decode a whole string, or point at the first bad character."
    decode(String.chars(text), 0)

fn zeros(chars: List<String>, n: Int) -> Int
    ? "The bound head is only matched, so the peel reads its code."
    match chars
        [] -> n
        [c, ..rest] -> match c
            "0" -> zeros(rest, n + 1)
            "k" -> zeros(rest, n + 100)
            _ -> zeros(rest, n)

fn codes(chars: List<String>, acc: List<Int>) -> List<Int>
    ? "Built in order, so list building specializes the cursor loop once more."
    match chars
        [] -> List.reverse(acc)
        [head, ..tail] -> codes(tail, List.prepend(value(head), acc))

fn ordered(xs: List<Int>, acc: Int) -> Int
    ? "The list as one number, so an element out of order changes it."
    match xs
        [] -> acc
        [x, ..rest] -> ordered(rest, acc * 100 + x + 1)

fn foldedZeros(chars: List<String>, n: Int) -> Int
    ? "The same, matched behind a case fold of the code."
    match chars
        [] -> n
        [c, ..rest] -> match String.toLower(c)
            "0" -> foldedZeros(rest, n + 1)
            "k" -> foldedZeros(rest, n + 10)
            _ -> foldedZeros(rest, n)

fn main() -> Unit
    ? "Print every shape so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print("{shape("")} {shape("abc")} {shape("\u{e9}x")} {shape("\u{1F980}ab")}")
    Console.print("{value("0")}{value("9")}/{value("A")}{value("f")}/{value("\u{212A}")}/{value("\u{130}")}/{value("ab")}/{value("")}")
    Console.print("{total("09af")}|{total("")}|{total("0x9")}|{total("\u{e9}0")}|{total("9\u{1F980}")}|{total("\u{212A}")}")
    mixed = "a0\u{212A}0\u{e9}\u{1F980}k\u{130}"
    Console.print("{zeros(String.chars(mixed), 0)}/{foldedZeros(String.chars(mixed), 0)}/{zeros(String.chars(""), 0)}")
    Console.print("{ordered(codes(String.chars(mixed), []), 0)}")
"#
    .replace("\\u{e9}", "\u{e9}")
    .replace("\\u{1F980}", "\u{1F980}")
    .replace("\\u{212A}", "\u{212A}")
    .replace("\\u{130}", "\u{130}");
    // "éx" is two codepoints in three bytes; "🦀ab" is three in six.
    // The Kelvin sign lowercases to "k"; U+0130 lowercases to two
    // characters and so matches nothing; "" and "ab" are not one
    // character either. The decode line exercises the codepoint-call
    // shape: the classifier consumes each character's code, and the
    // error message still prints the character the loop was on —
    // multibyte characters included — so a cursor that re-read the
    // wrong offset shows up as a changed message.
    let expected = "0:0: 3:1:ab-c? 2:0:éx 3:1:🦀a-b?\n09/1015/20/-1/-1/-1\nok:34|ok:0|bad 'x' after 0|bad 'é' after 0|bad '🦀' after 9|ok:20\n102/22/0\n1101210100002100";
    let vm = run_vm_inline("chars_fusion", &src).expect("vm run");
    let rust =
        build_run_rust_inline("chars_fusion", &src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM chars-fusion contract changed");
    assert_eq!(rust, expected, "Rust chars fusion diverged from the VM");
}

/// Recursive indexed access builds one hidden codepoint table and carries it
/// through the loop. This is deliberately Unicode-heavy: both backends must
/// agree that public positions are scalar-value indices rather than UTF-8 byte
/// offsets, for both `charAt` and `slice`.
#[test]
fn rust_indexed_string_access_matches_vm() {
    let src = r#"module StringIndexDifferential
    intent = "Exercise the hidden index used by recursive string access"
    effects [Console.print]

fn walk(text: String, pos: Int, seen: Int) -> String
    ? "Read every codepoint, then take a slice through the same hidden index."
    match String.charAt(text, pos)
        Option.None -> "{seen}:{String.slice(text, 1, 3)}"
        Option.Some(_) -> walk(text, pos + 1, seen + 1)

fn walkFar(text: String, pos: Int, seen: Int) -> String
    ? "The same walk, sliced across the edge of the index's first block of 64."
    match String.charAt(text, pos)
        Option.None -> "{seen}:{String.slice(text, 62, 67)}"
        Option.Some(_) -> walkFar(text, pos + 1, seen + 1)

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

record At
    pos: Int

fn spaceAt(text: String, pos: Int) -> Int
    ? "A raw position returned from the walk."
    match String.charAt(text, pos)
        Option.None -> pos
        Option.Some(" ") -> pos
        Option.Some(_) -> spaceAt(text, pos + 1)

fn endAt(text: String, pos: Int) -> At
    ? "A raw position stored in a record."
    match String.charAt(text, pos)
        Option.None -> At(pos = pos)
        Option.Some(_) -> endAt(text, pos + 1)

fn pairs(text: String, pos: Int, acc: String) -> String
    ? "A position bounded by a length test, sliced raw."
    match pos < String.len(text)
        true -> pairs(text, pos + 2, "{acc}[{String.slice(text, pos, pos + 2)}]")
        false -> acc

fn far(text: String, pos: Int) -> Int
    ? "Positions outside i64 and below zero stay boxed and read as past the end."
    match String.charAt(text, pos)
        Option.None -> pos
        Option.Some(_) -> far(text, pos + 1)

fn main() -> Unit
    ? "Print the indexed result for backend parity."
    ! [Console.print]
    Console.print("{walk("aą😀z", 0, 0)}|{score("aą😀z")}|{mutualEven("aą😀z", 0, 0)}")
    Console.print("{walkFar("LONG", 0, 0)}|{score("LONG")}|{mutualEven("LONG", 0, 0)}")
    Console.print("{spaceAt("ą😀 z", 0)}|{spaceAt("ą😀z", 1)}|{endAt("aą😀", 0 - 2).pos}|{pairs("aą😀zb", 0, "")}")
    Console.print("{far("ab", String.len("ab") * 1000000000000 * 1000000000000)}|{far("ab", 0 - 5)}")
"#
    .replace("LONG", &"aą😀z".repeat(40));
    let expected =
        "4:ą😀|211|211\n160:😀zaą😀|8440|8440\n2|3|-2|[aą][😀z][b]\n2000000000000000000000000|-5";
    let vm = run_vm_inline("string_index", &src).expect("VM run");
    let rust =
        build_run_rust_inline("string_index", &src).expect("Rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM indexed String contract changed");
    assert_eq!(
        rust, expected,
        "Rust indexed String access diverged from the VM"
    );
}

/// A string walk whose start is not bounded (a record field) is checked
/// once on entry: a start a read can succeed at runs the raw `i64` loop, any
/// other — negative, past `i64`, right at its edge — runs the failure path
/// on the boxed start. Covers a classifier guarding each step and handing
/// on a start, a sentinel matched by an arm, a returned and a stored
/// position, a failure arm that binds the classifier's answer, a prefix
/// before the read and a length test around it, on a Unicode text; and
/// params only read at as positions (the classifiers, the ends of a
/// slice), handed raw positions by the loops and saturated ones by the rest.
#[test]
fn rust_positions_checked_on_entry_match_vm() {
    let src = r#"module EntryChecked
    intent = "Walk positions whose start is not bounded, checked once on entry"
    effects [Console.print]

record NumAt
    value: Int
    end: Int

fn digitAt(s: String, i: Int) -> Int
    ? "The digit at i, or -1."
    match String.charAt(s, i)
        Option.Some("0") -> 0
        Option.Some("1") -> 1
        Option.Some("2") -> 2
        Option.Some("3") -> 3
        Option.Some("4") -> 4
        Option.Some("5") -> 5
        Option.Some("6") -> 6
        Option.Some("7") -> 7
        Option.Some("8") -> 8
        Option.Some("9") -> 9
        Option.Some -> -1
        Option.None -> -1

fn numberAt(s: String, i: Int, acc: Int) -> NumAt
    ? "Digits matched in the loop."
    match String.charAt(s, i)
        Option.Some("0") -> numberAt(s, i + 1, acc * 10 + 0)
        Option.Some("1") -> numberAt(s, i + 1, acc * 10 + 1)
        Option.Some("2") -> numberAt(s, i + 1, acc * 10 + 2)
        Option.Some("3") -> numberAt(s, i + 1, acc * 10 + 3)
        Option.Some("4") -> numberAt(s, i + 1, acc * 10 + 4)
        Option.Some("5") -> numberAt(s, i + 1, acc * 10 + 5)
        Option.Some("6") -> numberAt(s, i + 1, acc * 10 + 6)
        Option.Some("7") -> numberAt(s, i + 1, acc * 10 + 7)
        Option.Some("8") -> numberAt(s, i + 1, acc * 10 + 8)
        Option.Some("9") -> numberAt(s, i + 1, acc * 10 + 9)
        Option.Some -> NumAt(value = acc, end = i)
        Option.None -> NumAt(value = acc, end = i)

fn canonAt(s: String, i: Int) -> NumAt
    ? "A digit through the classifier, then the loop."
    d = digitAt(s, i)
    match d < 0
        true -> NumAt(value = 0, end = -1)
        false -> numberAt(s, i + 1, d)

fn walkAt(s: String, i: Int, acc: Int) -> NumAt
    ? "Each step guarded through the classifier."
    d = digitAt(s, i)
    match d < 0
        true -> NumAt(value = acc, end = i)
        false -> walkAt(s, i + 1, acc * 10 + d)

fn bitAt(s: String, i: Int) -> Int
    ? "1 or 0, or 9 for anything else."
    match String.charAt(s, i)
        Option.Some("1") -> 1
        Option.Some("0") -> 0
        Option.Some -> 9
        Option.None -> 9

fn bits(s: String, i: Int, n: Int) -> Int
    ? "A match on the classifier's sentinel; the result mixes in the position."
    match bitAt(s, i)
        9 -> n * 1000 + i
        b -> bits(s, i + 1, n * 2 + b)

fn spaceAt(s: String, i: Int) -> Int
    ? "Returns the raw position."
    match String.charAt(s, i)
        Option.None -> i
        Option.Some(" ") -> i
        Option.Some(_) -> spaceAt(s, i + 1)

fn describe(c: Option<String>, i: Int) -> String
    ? "What stopped the walk."
    match c
        Option.None -> "end@{i}"
        Option.Some(x) -> "{x}@{i}"

fn untilX(s: String, i: Int) -> String
    ? "A read the walk binds before matching it: the start stays boxed."
    match String.charAt(s, i)
        Option.Some("x") -> untilX(s, i + 1)
        other -> describe(other, i)

fn ones(s: String, i: Int, n: Int) -> Int
    ? "The failure takes an arm that binds the classifier's answer."
    match bitAt(s, i)
        1 -> ones(s, i + 1, n + 1)
        b -> n * 100 + b * 10 + i

fn doubled(s: String, i: Int, n: Int) -> Int
    ? "A prefix before the read uses the start."
    k = i * 2
    match String.charAt(s, i)
        Option.None -> k + n
        Option.Some(_) -> doubled(s, i + 1, n + 1)

fn bounded(s: String, i: Int) -> Int
    ? "A length test before the read: the failure path keeps both branches."
    match i < String.len(s)
        true -> match String.charAt(s, i)
            Option.Some(_) -> bounded(s, i + 1)
            Option.None -> 0 - 1
        false -> i

fn cut(s: String, a: Int, b: Int) -> String
    ? "Positions only read at, as the ends of a slice."
    String.slice(s, a, b)

fn cuts(s: String, i: Int, acc: String) -> String
    ? "Slices from the walk's position, and around it once the walk ends."
    match String.charAt(s, i)
        Option.None -> "{acc}|{cut(s, i - 2, i + 9)}"
        Option.Some(_) -> cuts(s, i + 1, "{acc}{cut(s, i, i + 1)}")

record Start
    at: Int

fn show(n: NumAt) -> String
    ? "A number and where it ends."
    "{n.value}/{n.end}"

fn tryAll(s: String, starts: List<Start>, acc: String) -> String
    ? "Every reader from every start."
    match starts
        [] -> acc
        [st, ..rest] -> tryAll(s, rest, "{acc} {show(canonAt(s, st.at))},{show(walkAt(s, st.at, 0))},{bits(s, st.at, 0)},{spaceAt(s, st.at)},{untilX(s, st.at)},{ones(s, st.at, 0)},{doubled(s, st.at, 0)},{bounded(s, st.at)},{cuts(s, st.at, "")}")

fn main() -> Unit
    ? "Print every reader for backend parity."
    ! [Console.print]
    big = String.len("ab") * 1000000000000 * 1000000000000
    top = 9223372036854775807
    starts = [Start(at = 0), Start(at = 1), Start(at = 2), Start(at = 7), Start(at = 9), Start(at = 30), Start(at = 0 - 1), Start(at = 0 - 7), Start(at = big), Start(at = 0 - big), Start(at = top), Start(at = top - 9), Start(at = top - 8), Start(at = 0 - top - 1)]
    Console.print(tryAll("x12ą😀 1011xx3", starts, ""))
    Console.print(tryAll("10x", [Start(at = 0), Start(at = 2)], ""))
"#;
    let expected = "0/-1,0/0,0,5,1@1,90,39,13,x12ą😀 1011xx3|x3 12/3,12/3,1002,5,1@1,192,38,13,12ą😀 1011xx3|x3 2/3,2/3,2,5,2@2,92,37,13,2ą😀 1011xx3|x3 11/10,11/10,3010,13,0@7,7,32,13,011xx3|x3 1/10,1/10,1010,13,1@9,200,30,13,1xx3|x3 0/-1,0/30,30,30,end@30,120,60,30,| 0/-1,0/-1,-1,-1,end@-1,89,-2,-1,|x12ą😀 10 0/-1,0/-7,-7,-7,end@-7,83,-14,-1,|x1 0/-1,0/2000000000000000000000000,2000000000000000000000000,2000000000000000000000000,end@2000000000000000000000000,2000000000000000000000090,4000000000000000000000000,2000000000000000000000000,| 0/-1,0/-2000000000000000000000000,-2000000000000000000000000,-2000000000000000000000000,end@-2000000000000000000000000,-1999999999999999999999910,-4000000000000000000000000,-1,| 0/-1,0/9223372036854775807,9223372036854775807,9223372036854775807,end@9223372036854775807,9223372036854775897,18446744073709551614,9223372036854775807,| 0/-1,0/9223372036854775798,9223372036854775798,9223372036854775798,end@9223372036854775798,9223372036854775888,18446744073709551596,9223372036854775798,| 0/-1,0/9223372036854775799,9223372036854775799,9223372036854775799,end@9223372036854775799,9223372036854775889,18446744073709551598,9223372036854775799,| 0/-1,0/-9223372036854775808,-9223372036854775808,-9223372036854775808,end@-9223372036854775808,-9223372036854775718,-18446744073709551616,-1,|\n 10/2,10/2,2002,3,1@0,101,9,3,10x|0x 0/-1,0/2,2,3,end@3,92,7,3,x|0x";
    let vm = run_vm_inline("entry_checked", src).expect("VM run");
    let rust =
        build_run_rust_inline("entry_checked", src).expect("Rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM contract changed");
    assert_eq!(
        rust, expected,
        "Rust entry-checked walk diverged from the VM"
    );
}

/// `String.slice(s, a, b) == t` compares the slice where it stands in `s`
/// instead of building it: either way round, with `!=`, a literal or a
/// variable or another slice on the other side, positions out of range,
/// negative or reversed, and through the hidden index of a loop, with boxed
/// and raw positions. A right side that takes `s` keeps the built slice.
#[test]
fn rust_slice_compared_in_place_matches_vm() {
    let src = r#"module SliceCompare
    intent = "Compare slices with strings without building the slices"
    effects [Console.print]

fn at(s: String, i: Int, t: String) -> Bool
    ? "Whether t stands in s at i: the slice on the left, a variable on the right."
    String.slice(s, i, i + String.len(t)) == t

fn differs(s: String, a: Int, b: Int, t: String) -> Bool
    ? "The variable on the left, with not-equal."
    t != String.slice(s, a, b)

fn literal(s: String, a: Int, b: Int) -> String
    ? "Literals on either side, and a match on the comparison."
    match String.slice(s, a, b) == "żó"
        true -> "L"
        false -> match "" == String.slice(s, a, b)
            true -> "E"
            false -> "-"

fn both(s: String, t: String) -> Bool
    ? "A slice on each side."
    String.slice(s, 1, 3) == String.slice(t, 0, 2)

fn keep(s: String) -> String
    ? "Takes the string and hands it on."
    "{s}"

fn moved(s: String) -> Bool
    ? "The right side takes s."
    String.slice(s, 0, 2) == keep(s)

fn hits(text: String, pos: Int, word: String, acc: Int) -> Int
    ? "Walk the text; count where word stands, through the hidden index."
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(_) -> hits(text, pos + 1, word, acc + hit(text, pos, word))

fn hit(text: String, pos: Int, word: String) -> Int
    ? "One indexed comparison per position, out of range near the ends."
    match String.slice(text, pos - 1, pos + String.len(word) - 1) == word
        true -> 1
        false -> match String.slice(text, pos + 1, pos) != ""
            true -> 100
            false -> 0

fn pairs(text: String, pos: Int, acc: Int) -> Int
    ? "A position bounded by a length test reaches the slice raw."
    match pos < String.len(text)
        true -> pairs(text, pos + 1, acc + pairAt(String.slice(text, pos, pos + 2) == "ąb"))
        false -> acc

fn pairAt(found: Bool) -> Int
    match found
        true -> 1
        false -> 0

fn main() -> Unit
    ? "Print every answer for backend parity."
    ! [Console.print]
    s = "zażółć gęślą"
    Console.print("{at(s, 2, "żó")}|{at(s, 2, "żo")}|{at(s, 0 - 1, "za")}|{at(s, 11, "ą")}|{at(s, 11, "ą!")}|{at(s, 12, "")}|{at("", 0, "")}")
    Console.print("{differs(s, 7, 99, "gęślą")}|{differs(s, 7, 12, "gęśla")}|{differs(s, 5, 2, "")}|{differs(s, 0 - 9, 2, "za")}|{differs("abc", 1, 2, "b")}")
    Console.print("{literal(s, 2, 4)}|{literal(s, 2, 5)}|{literal(s, 4, 4)}|{literal(s, 40, 50)}|{literal(s, 0 - 5, 0 - 1)}|{literal(s, 3, 1)}")
    Console.print("{both(s, "aż")}|{both(s, "ażó")}|{both("x", "")}|{both("xab", "bad")}|{moved("ab")}|{moved("abc")}")
    Console.print("{hits("ąbąbaąb", 0, "ąb", 0)}|{hits("aaaa", 0, "aa", 0)}|{hits("ąb😀ąb", 0, "😀ą", 0)}|{hits("ab", 0, "", 0)}")
    Console.print("{pairs("ąbąbaąb", 0, 0)}|{pairs("xyz", 0, 0)}")
"#;
    let expected = "true|false|false|true|false|true|true\nfalse|true|false|false|false\nL|-|E|E|E|E\ntrue|true|true|false|true|false\n3|3|1|2\n3|0";
    assert_eq!(
        run_vm_inline("slice_compare", src).expect("VM run"),
        expected,
        "VM String.slice contract changed"
    );
    let ws = temp_dir("slice_compare");
    let file = ws.join("slice_compare.av");
    fs::write(&file, src).expect("write source");
    let project = ws.join("project");
    let result = (|| {
        compile_rust(&file, &project, "slice_compare", None, &[])?;
        let entry = generated_entry(&project)?;
        let bin = cargo_build(&project, "slice_compare")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run the compiled binary: {e}"))?;
        Ok::<_, String>((
            entry,
            String::from_utf8_lossy(&out.stdout).trim().to_string(),
        ))
    })();
    let _ = fs::remove_dir_all(&ws);
    let (entry, rust) = result.unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(rust, expected, "Rust slice comparison diverged from the VM");
    assert_eq!(
        entry.matches("string_slice_part(").count(),
        1,
        "only `moved` builds its slice:\n{entry}"
    );
    assert!(
        entry.contains("aver_rt::string_index_slice_str(&text, &__str_index, pos, (pos + 2i64))"),
        "the indexed loop compares in place with raw positions:\n{entry}"
    );
}

/// Strings on the Rust runtime: a long `acc + piece` loop that grows one
/// string in place, with copies of it that must stay as they were;
/// `String.chars`; interpolated Int, Float, Bool and String; the builtins
/// whose answer is their whole text, or empty, or one character; and an
/// indexed walk over a long ASCII string with a multibyte character at a
/// block edge.
#[test]
fn rust_string_runtime_shapes_match_vm() {
    let src = r#"module StringRuntimeDifferential
    intent = "String appends, characters, interpolation and builtins that keep or empty their text"
    effects [Console.print]

fn grow(n: Int, acc: String) -> String
    ? "Append n pieces, ASCII and multibyte in turn."
    match n <= 0
        true -> acc
        false -> match Int.mod(n, 2) == 0
            true -> grow(n - 1, acc + "ab")
            false -> grow(n - 1, acc + "é")

fn spell(chars: List<String>, acc: String) -> String
    ? "Join characters with a bar, one append each."
    match chars
        [] -> acc
        [c, ..rest] -> spell(rest, acc + c + "|")

fn codes(text: String, pos: Int, acc: Int) -> Int
    ? "Read every character by position and sum the codes, weighted by position."
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(c) -> codes(text, pos + 1, acc + pos * Option.withDefault(String.firstCodePoint(c), 0))

fn main() -> Unit
    ? "Print each shape so a drifting backend shows a diff."
    ! [Console.print]
    base = grow(3000, "")
    kept = base
    left = base + "L"
    right = kept + "R"
    Console.print("{String.len(base)}:{String.len(left)}:{String.len(right)}:{String.slice(left, 4494, 4510)}:{String.slice(right, 4494, 4510)}:{String.slice(base, 0, 6)}")
    Console.print(spell(String.chars("aé😀 z"), "") + spell(String.chars(""), "") + "{List.len(String.chars(base))}")
    Console.print("{0 - 7}/{123456789012345678901234567890 * 3}/{0.1}/{Float.fromInt(3)}/{1.5 * 0.0 - 2.25}/{1 < 2}/{2 < 1}/{"s"}/{""}/{base == left}")
    Console.print("[{String.trim("")}][{String.trim("   ")}][{String.trim("x")}][{String.trim(" é ")}][{String.slice("hello", 0 - 3, 99)}][{String.slice("hello", 3, 3)}][{String.slice("zażółć", 1, 4)}]")
    Console.print("[{String.replace("hello", "zz", "y")}][{String.replace("ab", "", "-")}][{String.replace("aaa", "a", "bb")}][{String.toLower("abc")}][{String.toUpper("abc")}][{String.toLower("ŻÓŁĆ K")}][{String.toUpper("straße")}]")
    Console.print("[{String.fromInt(0 - 12)}][{String.fromFloat(2.5)}][{String.fromBool(true)}][{String.fromBool(false)}][{Option.withDefault(String.charAt("aé", 1), "?")}][{Option.withDefault(String.charAt("aé", 2), "?")}][{Option.withDefault(String.fromCodePoint(128512), "?")}][{Option.withDefault(String.fromCodePoint(65), "?")}][{String.join(["", "b", "cd"], ", ")}][{String.join([], ",")}]")
    sparse = String.join(["a", "b", "c"], "SPARSE")
    Console.print("{codes(sparse, 0, 0)}|{codes("ascii only text", 0, 0)}|{String.len(sparse)}")
"#
    .replace(
        "SPARSE",
        &("x".repeat(62) + "\u{e9}" + &"y".repeat(70) + "\u{1F600}" + "zzzzz"),
    );
    let expected = "4500:4501:4501:abéabéL:abéabéR:abéabé\na|é|😀| |z|4500\n-7/370370367037037036703703703670/0.1/3/-2.25/true/false/s//false\n[][][x][é][hello][][ażó]\n[hello][-a-b-][bbbbbb][abc][ABC][żółć k][STRASSE]\n[-12][2.5][true][false][é][?][😀][A][, b, cd][]\n57153564|10589|281";
    let vm = run_vm_inline("string_runtime", &src).expect("VM run");
    let rust =
        build_run_rust_inline("string_runtime", &src).expect("Rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM string contract changed");
    assert_eq!(rust, expected, "Rust strings diverged from the VM");
}

/// List build, on the shapes where the two backends could disagree.
///
/// ORDER is the claim: `prepend` then `reverse` yields traversal order,
/// so appending in loop order has to reproduce it — asserted here as a
/// running program rather than argued, and asserted against the SAME
/// program's unfused answer, which `pairs` supplies by collecting the
/// identical run without an accumulator the pass can recognise.
///
/// The element types are the other half. The Rust builder is the
/// `AverList` the accumulator always was, written in place; the VM keeps
/// immediates in a pool it owns and hands the builder back as the cons
/// chain the moment a heap value arrives, because a pool the collector
/// cannot see must not hold anything the collector could move. `mixed`
/// crosses that line MID-BUILD — three small integers, then integers too
/// wide to travel inline — so the elements collected before the crossing
/// and after it have to come back in one order.
#[test]
fn rust_list_build_matches_vm() {
    let src = r#"module ListBuildDifferential
    intent = "Every list-build shape in one program, for cross-backend agreement"
    effects [Console.print]

fn collect(n: Int, limit: Int, acc: List<Int>) -> List<Int>
    ? "Count up, collect, reverse on the way out — the canonical shape."
    match n > limit
        true -> List.reverse(acc)
        false -> collect(n + 1, limit, List.prepend(n, acc))

fn upTo(n: Int) -> List<Int>
    ? "The same run with no accumulator to recognise: the unfused oracle."
    match n <= 0
        true -> []
        false -> List.concat(upTo(n - 1), [n])

fn wide(n: Int) -> Int
    ? "Small below three, past the inline integer range above it."
    match n < 3
        true -> n
        false -> n * 100000000000000

fn mixed(n: Int, limit: Int, acc: List<Int>) -> List<Int>
    ? "Crosses from inline elements to heap ones halfway through."
    match n > limit
        true -> List.reverse(acc)
        false -> mixed(n + 1, limit, List.prepend(wide(n), acc))

fn tagsInto(values: List<Int>, acc: List<String>) -> List<String>
    ? "Heap elements from the first step, and the caller does the reversing."
    match values
        [] -> acc
        [head, ..tail] -> tagsInto(tail, List.prepend("t{head}", acc))

fn parse(chars: List<String>, acc: List<Int>) -> Result<List<Int>, String>
    ? "The parser shape: nested arms, error exits, the list inside a constructor."
    match chars
        [] -> Result.Ok(List.reverse(acc))
        [high, ..afterHigh] -> match afterHigh
            [] -> Result.Err("odd")
            [low, ..rest] -> match String.len(high) == 1
                true -> parse(rest, List.prepend(String.len(low), acc))
                false -> Result.Err("wide '{high}'")

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{head}/{render(tail)}"

fn decoded(chars: List<String>) -> String
    ? "Either the parsed list or the message that says why not."
    match parse(chars, [])
        Result.Ok(values) -> render(values)
        Result.Err(message) -> message

fn main() -> Unit
    ? "Print every shape so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print("{render(collect(1, 5, []))} {render(upTo(5))}")
    Console.print("{render(mixed(1, 6, []))}")
    Console.print("{String.join(List.reverse(tagsInto([1, 2, 3], [])), "-")} {String.join(tagsInto([1, 2, 3], []), "-")}")
    Console.print("{decoded(["a", "b", "c", "d"])} {decoded(["a"])} {decoded(["ab", "c"])}")
"#;
    // collect and upTo are the same run, written once with the
    // accumulator the pass recognises and once without it: the builder
    // answers what the oracle answers. mixed keeps 1, 2 inline and
    // widens from 3 on.
    // tagsInto reversed by its caller is forwards; bare is backwards.
    // parse reads a length per pair, and its error arms carry the
    // character that failed.
    let expected = "1/2/3/4/5/ 1/2/3/4/5/\n\
                    1/2/300000000000000/400000000000000/500000000000000/600000000000000/\n\
                    t1-t2-t3 t3-t2-t1\n\
                    1/1/ odd wide 'ab'";
    let vm = run_vm_inline("list_build", src).expect("vm run");
    let rust = build_run_rust_inline("list_build", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM list-build contract changed");
    assert_eq!(rust, expected, "Rust list build diverged from the VM");
}

/// The shapes list build must NOT touch, run on both backends.
///
/// They live here rather than beside the other decline witnesses
/// because the VM alone cannot see this class of mistake. A VM builder
/// that is handed something other than a fresh builder falls back to the
/// cons chain, which prepends and reverses — the very thing the rewrite
/// replaces — so a wrong fusion answers correctly there by accident.
/// Compiled Rust appends and does not reverse, and says so.
///
/// `bail` is the loop whose exits disagree about the reverse; `into` is
/// the one that leaves the reversing to its caller, read once each way.
#[test]
fn rust_list_build_declines_match_vm() {
    let src = r#"module ListBuildDeclinesDifferential
    intent = "Collecting loops the rewrite must leave alone, on both backends"
    effects [Console.print]

fn into(values: List<Int>, acc: List<Int>) -> List<Int>
    ? "Hands the accumulator back bare, so only a reversing caller may be moved."
    match values
        [] -> acc
        [head, ..tail] -> into(tail, List.prepend(head, acc))

fn bail(n: Int, acc: List<Int>) -> List<Int>
    ? "Collects forwards, except on the value that hands the raw accumulator back."
    match n <= 0
        true -> List.reverse(acc)
        false -> match n == 2
            true -> acc
            false -> bail(n - 1, List.prepend(n, acc))

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{head}/{render(tail)}"

fn main() -> Unit
    ? "Print each so a backend that fuses one of them shows a diff."
    ! [Console.print]
    Console.print("{render(bail(4, []))} {render(bail(1, []))} {render(List.reverse(into([1, 2, 3], [])))} {render(into([1, 2, 3], []))}")
"#;
    // bail(4) collects 4 and 3 and then bails with the raw accumulator,
    // which reads 3, 4; bail(1) never reaches the bail-out. `into` read
    // through its caller's reverse is forwards and read bare is
    // backwards, and only the first of those may be moved.
    let expected = "3/4/ 1/ 1/2/3/ 3/2/1/";
    let vm = run_vm_inline("list_build_declines", src).expect("vm run");
    let rust = build_run_rust_inline("list_build_declines", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM answer for an unfusable loop changed");
    assert_eq!(
        rust, expected,
        "Rust fused a loop the recogniser must decline"
    );
}

/// The driver-and-step normalization, on the shapes where the two
/// backends could disagree — every fused pair here goes through the
/// inline (renamed binders, substituted arguments, a statement folded
/// into the driver's branch structure) and then through the same
/// builder rewrite the single-fn loops take, so a hygiene slip shows up
/// as a wrong list rather than a missed optimization. One test, one
/// build: the five shapes the mechanism is specified against, in one
/// program.
///
/// `readAll`/`readNext` is the fallible reader — the step's `?` moves
/// into the driver's arm and the error exits must keep their meaning.
/// `gatherAll`/`gatherOne` terminates inside a record constructor, so
/// the finalize lands in a field while the other fields stay ordinary
/// reads. `sharedAll`/`sharedOne` must DECLINE — the step has a second
/// caller that enters the loop mid-flight, and both callers' answers
/// are pinned. `capturedAll`/`capturedOne` is the hygiene witness: the
/// step reads the top-level `scale` and the driver re-binds that name
/// around the call site, so an inline would turn the helper call into a
/// read of an integer — declined, answer pinned. `trimAll`/
/// `keepUnlessLast` is the exact two-match nesting depth of the code
/// that motivated the stage: the step carries its own exit arm beside
/// the arm that recurses back into the driver. `pairAll`/`pairOne` is
/// the argument-spelling witness: the driver's first cons binder wears
/// the step's SECOND parameter's name, so a substitution that walks
/// one parameter at a time rewrites the identifiers it just inserted —
/// both backends then agree on the wrong answer, which is why this
/// case pins the exact answer and not just backend parity.
#[test]
fn rust_driver_step_pairs_match_vm() {
    let src = r#"module DriverStepDifferential
    intent = "Every driver-and-step shape in one program, for cross-backend agreement"
    exposes [decoded, gatherAll, sharedEntry, sharedOther, entryCaptured, trimAll, pairAll]
    effects [Console.print]

fn readAll(bytes: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    ? "Driver of the fallible pair: matches and terminates with a Result."
    match bytes
        [] -> Result.Ok(List.reverse(acc))
        [h, ..t] -> readNext(h, t, acc)

fn readNext(h: Int, t: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    ? "Step of the fallible pair: one checked unit, then back into the driver."
    v = checked(h)?
    readAll(t, List.prepend(v, acc))

fn checked(h: Int) -> Result<Int, String>
    ? "Reject nines, double the rest."
    match h == 9
        true -> Result.Err("nine")
        false -> Result.Ok(h * 2)

fn decoded(bytes: List<Int>) -> String
    ? "Either the parsed list or the message that says why not."
    match readAll(bytes, [])
        Result.Ok(values) -> render(values)
        Result.Err(message) -> message

record Gathered
    items: List<Int>
    seen: Int

fn gatherAll(xs: List<Int>, seen: Int, acc: List<Int>) -> Gathered
    ? "Driver of the record pair: the exit wraps the reverse in a field."
    match xs
        [] -> Gathered(items = List.reverse(acc), seen = seen)
        [h, ..t] -> gatherOne(h, t, seen, acc)

fn gatherOne(h: Int, t: List<Int>, seen: Int, acc: List<Int>) -> Gathered
    ? "Step of the record pair: one summed element, then back into the driver."
    g = h + seen
    gatherAll(t, seen + 1, List.prepend(g, acc))

fn sharedAll(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver whose step is shared — this pair must keep both functions."
    match xs
        [] -> List.reverse(acc)
        [h, ..t] -> sharedOne(h, t, acc)

fn sharedOne(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    ? "Step with a second caller, so it is shared code rather than the idiom."
    sharedAll(t, List.prepend(h * 3, acc))

fn sharedEntry(xs: List<Int>) -> List<Int>
    ? "The driver's own caller."
    sharedAll(xs, [])

fn sharedOther(h: Int) -> List<Int>
    ? "The second caller, entering the loop through the step mid-flight."
    sharedOne(h, [2], [4])

fn scale(n: Int) -> Int
    ? "The function the step calls across the pair boundary."
    n * 10

fn capturedAll(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver handing each element to a step that calls a module helper."
    match xs
        [] -> List.reverse(acc)
        [value, ..t] -> capturedOne(value, t, acc)

fn capturedOne(h: Int, t: List<Int>, acc: List<Int>) -> List<Int>
    ? "Step that reads the top-level scale."
    capturedAll(t, List.prepend(scale(h), acc))

fn entryCaptured(xs: List<Int>) -> List<Int>
    ? "Start the captured pair with an empty accumulator."
    capturedAll(xs, [])

fn trimAll(parts: List<String>, acc: List<String>) -> List<String>
    ? "Driver of the two-match pair: everything but the final element."
    match parts
        [] -> List.reverse(acc)
        [head, ..tail] -> keepUnlessLast(head, tail, acc)

fn keepUnlessLast(head: String, tail: List<String>, acc: List<String>) -> List<String>
    ? "Step with its own exit arm beside the arm that recurses back."
    match tail
        [] -> List.reverse(acc)
        [next, ..rest] -> trimAll(tail, List.prepend(head, acc))

fn pairAll(xs: List<Int>, acc: List<Int>) -> List<Int>
    ? "Driver of the pairwise pair: peels two, the first binder wears the step's second param name."
    match xs
        [] -> List.reverse(acc)
        [b, ..t] -> match t
            [] -> List.reverse(acc)
            [c, ..t2] -> pairOne(b, c, t2, acc)

fn pairOne(a: Int, b: Int, st: List<Int>, sacc: List<Int>) -> List<Int>
    ? "Step of the pairwise pair: combine the pair as a*10 + b."
    pairAll(st, List.prepend(a * 10 + b, sacc))

fn render(values: List<Int>) -> String
    ? "Print a list with no accumulator of its own."
    match values
        [] -> ""
        [head, ..tail] -> "{head}/{render(tail)}"

fn main() -> Unit
    ? "Print every shape so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print("{decoded([1, 2, 3])} {decoded([1, 9, 3])}")
    g = gatherAll([5, 6, 7], 0, [])
    Console.print("{render(g.items)} {g.seen}")
    Console.print("{render(sharedEntry([1, 2]))} {render(sharedOther(5))}")
    Console.print("{render(entryCaptured([1, 2]))}")
    Console.print("[{String.join(trimAll(["a", "b", "c"], []), "-")}] [{String.join(trimAll(["x"], []), "-")}]")
    Console.print("{render(pairAll([1, 2, 3, 4], []))}")
"#;
    // The fallible pair doubles until it meets a nine, whose error is
    // the whole answer. The record pair adds the running count to each
    // element and reports how many it saw. The shared pair answers both
    // ways it is entered — through the driver, and mid-flight through
    // the step with a seeded accumulator. The scaling pair scales by
    // ten through a module helper the step calls across the pair
    // boundary; its driver used to bind that helper's name in the cons
    // pattern, which the shadowing ban (issue #954) refuses now — the
    // refusal is pinned in `tests/driver_step_pairs.rs`, and this cell
    // keeps the pair shape with a distinct binder. The two-match pair
    // drops the last element, and a lone element leaves nothing. The
    // pairwise pair combines each pair as tens-digit/units-digit — the
    // answer sequential substitution turned into 22/44.
    let expected = "2/4/6/ nine\n\
                    5/7/9/ 3\n\
                    3/6/ 4/15/6/\n\
                    10/20/\n\
                    [a-b] []\n\
                    12/34/";
    let vm = run_vm_inline("driver_step", src).expect("vm run");
    let rust = build_run_rust_inline("driver_step", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM driver-and-step contract changed");
    assert_eq!(
        rust, expected,
        "Rust driver-and-step fusion diverged from the VM"
    );
}

/// The byte sink, on the shapes where the two backends could disagree —
/// and against the answers the UNFUSED pair gives, since the whole
/// program carries a word-for-word copy of the standard library's
/// `fromList` family and both loops' only readers hand it their lists.
///
/// ORDER is the first claim: bytes come out in loop order, pinned as
/// the exact sequence and not a length. The ERROR PATH is the second:
/// the first out-of-range element wins, its index counts the elements
/// before it, a negative and a wider-than-`i64` value are reported in
/// the library's exact words, and a parse error beats a later range
/// error because the loop stopped first. The second round replays it
/// all after four thousand one hundred builders were abandoned
/// mid-build — past the VM's pool cap — so the cons-chain fallback
/// answers the same matrix the pooled path did. Compiled Rust has no
/// pool and no fallback, which is exactly why only running both
/// settles it.
#[test]
fn rust_byte_sink_matches_vm() {
    let src = r#"module ByteSinkDifferential
    intent = "Every byte-sink shape in one program, for cross-backend agreement"
    exposes opaque [Bytes]
    effects [Console.print]

record Bytes
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    ? "Return true when every integer in the list is an octet."
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn firstOutOfRange(xs: List<Int>) -> Int
    ? "Return the first non-octet value; -1 when every value is an octet."
    match xs
        [] -> -1
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> firstOutOfRange(tail)
            false -> head

fn firstOutOfRangeIndex(xs: List<Int>) -> Int
    ? "Return the index of the first non-octet value; the length when every value is an octet."
    match xs
        [] -> 0
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> 1 + firstOutOfRangeIndex(tail)
            false -> 0

fn fromList(xs: List<Int>) -> Result<Bytes, String>
    ? "Validate raw integers and construct a byte sequence."
    match allInRange(xs)
        true -> Result.Ok(Bytes(values = xs))
        false -> Result.Err("byte {firstOutOfRange(xs)} at index {firstOutOfRangeIndex(xs)} is outside 0..=255")

fn ramp(n: Int, limit: Int, step: Int, acc: List<Int>) -> List<Int>
    ? "Collect limit values stepping by step, in ascending loop order."
    match n <= 0
        true -> List.reverse(acc)
        false -> ramp(n - 1, limit, step, List.prepend((limit - n) * step, acc))

fn parseTriples(values: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    ? "Triple every sample from left to right; a zero is a parse error."
    match values
        [] -> Result.Ok(List.reverse(acc))
        [head, ..tail] -> match head == 0
            true -> Result.Err("zero is not a sample")
            false -> parseTriples(tail, List.prepend(head * 3, acc))

fn toBytes(values: List<Int>) -> Result<Bytes, String>
    ? "Parse the samples, then hand the octets over as bytes."
    collected = parseTriples(values, [])?
    fromList(collected)

fn renderItems(values: List<Int>) -> String
    ? "The elements, comma-separated."
    match values
        [] -> ""
        [head, ..tail] -> match tail
            [] -> "{head}"
            [next, ..rest] -> "{head}, {renderItems(tail)}"

fn describe(outcome: Result<Bytes, String>) -> String
    ? "Render either side of a fromList answer."
    match outcome
        Result.Ok(bytes) -> "ok:{renderItems(bytes.values)}"
        Result.Err(message) -> "err:{message}"

fn leak(n: Int) -> Int
    ? "Run a parse error n times, abandoning a builder mid-build each time."
    match n <= 0
        true -> 0
        false -> match toBytes([1, 0])
            Result.Ok(bytes) -> leak(n - 1)
            Result.Err(message) -> leak(n - 1)

fn round() -> Unit
    ? "Every shape once: order, first offender, sign, magnitude, parse precedence."
    ! [Console.print]
    Console.print(describe(fromList(ramp(4, 4, 1, []))))
    Console.print(describe(fromList(ramp(4, 4, 100, []))))
    Console.print(describe(fromList(ramp(4, 4, 0 - 1, []))))
    Console.print(describe(fromList(ramp(6, 6, 100, []))))
    Console.print(describe(toBytes([5, 6])))
    Console.print(describe(toBytes([5, 90])))
    Console.print(describe(toBytes([5, 0, 999])))
    Console.print(describe(toBytes([5, 100000000000000000000, 7])))
    Console.print(describe(fromList(ramp(0, 0, 1, []))))

fn main() -> Unit
    ! [Console.print]
    round()
    Console.print("leaked:{leak(4100)}")
    round()
"#;
    // Verified against the unfused answers of the same program with the
    // pass off: the retarget must be invisible in every byte of output.
    let round = "ok:0, 1, 2, 3\n\
                 err:byte 300 at index 3 is outside 0..=255\n\
                 err:byte -1 at index 1 is outside 0..=255\n\
                 err:byte 300 at index 3 is outside 0..=255\n\
                 ok:15, 18\n\
                 err:byte 270 at index 1 is outside 0..=255\n\
                 err:zero is not a sample\n\
                 err:byte 300000000000000000000 at index 1 is outside 0..=255\n\
                 ok:";
    let expected = format!("{round}\nleaked:0\n{round}");
    let vm = run_vm_inline("byte_sink", src).expect("vm run");
    let rust = build_run_rust_inline("byte_sink", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM byte-sink contract changed");
    assert_eq!(rust, expected, "Rust byte sink diverged from the VM");
}

// ─── own_param ownership: build+run rust vs VM + emitted-shape guard ─────

/// Build+run an inline Aver program through the Rust backend and return
/// trimmed stdout. Shares the fast-tier compile + cargo-build path.
fn build_run_rust_inline(name: &str, source: &str) -> Result<String, String> {
    let ws = temp_dir(name);
    let src = ws.join(format!("{name}.av"));
    fs::write(&src, source).expect("write source");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!("binary exited non-zero:\n{}", format_output(&out)));
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
    })();
    let _ = fs::remove_dir_all(&ws);
    result
}

/// Run an inline Aver program on the VM and return trimmed stdout.
fn run_vm_inline(name: &str, source: &str) -> Result<String, String> {
    let ws = temp_dir(&format!("{name}_vm"));
    let src = ws.join(format!("{name}.av"));
    fs::write(&src, source).expect("write source");
    let out = run_vm(&src, None);
    let _ = fs::remove_dir_all(&ws);
    out
}

/// Binding an integer literal to a name is about as ordinary as Aver gets,
/// and until this was fixed it broke the Rust backend outright: the emitted
/// crate did not compile.
///
/// The bare-i64 rewrite treats an Int literal as already raw and inserts no
/// representation boundary around it, so codegen is what has to render the
/// value raw into a bare binding. The nested `Let` arm did; the FLAT
/// top-level let chain — the shape a whole function body of bindings takes,
/// and the one the compiler prefers — did not, so it emitted
/// `let n = AverInt::from_i64(1);` into an `i64` binding and rustc rejected
/// the crate with E0308.
///
/// Each program here is a whole-body let chain over a bare Int binding: the
/// bare literal, one const-folded from arithmetic, the same binding inside a
/// helper whose signature is itself bare, and the committed fuzz seed's
/// `match Option.Some(1)` (which folds to the same binding). The oracle is
/// the VM.
#[test]
fn rust_bare_int_literal_binding_matches_vm() {
    let cases: &[(&str, &str)] = &[
        (
            "bare_int_binding",
            r#"module BareIntBinding
    intent = "Bind an integer literal to a name and answer it"
    effects [Console.print]

fn answer() -> Int
    ? "The whole body is one binding of a literal."
    n = 1
    n

fn main() -> Unit
    ? "Prints the answer."
    ! [Console.print]
    Console.print(String.fromInt(answer()))
"#,
        ),
        (
            "bare_int_folded",
            r#"module BareIntFolded
    intent = "Bind a constant-folded integer to a name and answer it"
    effects [Console.print]

fn answer() -> Int
    ? "The initializer folds to a literal before codegen sees it."
    n = 1 + 2
    n

fn main() -> Unit
    ? "Prints the answer."
    ! [Console.print]
    Console.print(String.fromInt(answer()))
"#,
        ),
        (
            "bare_int_from_match",
            r#"module BareIntFromMatch
    intent = "Bind an integer that reaches the name through a match"
    effects [Console.print]

fn answer() -> Int
    ? "The match folds to the same single bare binding."
    match Option.Some(1)
        Option.Some(n) -> n
        Option.None -> 0

fn main() -> Unit
    ? "Prints the answer."
    ! [Console.print]
    Console.print(String.fromInt(answer()))
"#,
        ),
    ];
    for (name, src) in cases {
        let vm = run_vm_inline(name, src).unwrap_or_else(|e| panic!("{name}: {e}"));
        let rust = build_run_rust_inline(name, src).unwrap_or_else(|e| panic!("{name}: {e}"));
        assert_eq!(vm, rust, "{name}: Rust backend diverged from the VM");
    }

    // The reported shape: no descriptions, no output, `main` itself
    // returning the bound Int. Nothing is printed, so the whole assertion is
    // that the emitted crate builds and runs at all. The Int is 0 because it
    // is the exit status.
    let reported = r#"module P

fn main() -> Int
    ! []
    n = 0
    n
"#;
    let vm = run_vm_inline("bare_int_reported", reported).unwrap_or_else(|e| panic!("{e}"));
    let rust =
        build_run_rust_inline("bare_int_reported", reported).unwrap_or_else(|e| panic!("{e}"));
    assert_eq!(vm, rust, "Rust backend diverged from the VM");
}

/// Literal smart-constructor discharge on the Rust backend. A call whose
/// argument is an all-literal list inside the interval the refinement itself
/// proves types as the refined type and lowers to a direct carrier
/// construction; a computed argument keeps the fallible constructor. Both
/// shapes sit in one program, so a backend that discharged too much or too
/// little diverges from the VM here.
#[test]
fn rust_bits_namespace_matches_vm() {
    // `Bits` is a bit-level VIEW of `Int`, so the Rust backend must answer on
    // the SAME `AverInt` carrier the VM uses — never a raw i64. The cases that
    // would catch a drift are the ones past the 64-bit boundary (a truncating
    // backend gives a different number, not an error) and the negative-count
    // arms (whose `Result.Err` bytes must match the VM verbatim). The two
    // oversized dynamic cases also pin the shared allocation-bomb boundary.
    let src = r#"module BitsDifferential
    intent = "Every Bits shape in one program, for cross-backend agreement"
    effects [Console.print]

fn pointwise() -> String
    ? "The four total operations, on operands of both signs."
    "{Bits.and(6, 3)} {Bits.or(6, 3)} {Bits.xor(6, 3)} {Bits.and(-1, 42)} {Bits.or(-1, 42)} {Bits.xor(-6, 3)} {Bits.not(0)} {Bits.not(-1)}"

fn counted() -> String
    ? "Discharged literal counts, including one past the 64-bit cliff."
    "{Bits.shiftLeft(1, 100)} {Bits.shiftRight(-3, 1)} {Bits.low(257, 8)} {Bits.low(-1, 8)} {Bits.low(123, 0)}"

fn large() -> String
    ? "Operands and results on the far side of the 64-bit boundary."
    huge = Bits.shiftLeft(1, 100)
    "{Bits.not(huge)} {Bits.or(huge, 1)} {Bits.xor(huge, huge)} {Bits.and(Bits.not(huge), huge)} {Bits.shiftRight(Bits.not(huge), 100)}"

fn dynamic(count: Int) -> String
    ? "An undischarged count, so the Result survives to runtime."
    match Bits.shiftLeft(1, count)
        Result.Ok(v) -> "ok {v}"
        Result.Err(e) -> "err {e}"

fn main() -> Unit
    ? "Print every shape so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print(pointwise())
    Console.print(counted())
    Console.print(large())
    Console.print(dynamic(4))
    Console.print(dynamic(-1))
    Console.print(dynamic(16777217))
    Console.print(dynamic(4294967296))
"#;
    let expected = "2 7 5 42 -1 -7 -1 0\n1267650600228229401496703205376 -2 1 255 0\n-1267650600228229401496703205377 1267650600228229401496703205377 0 0 -2\nok 16\nerr negative shift count\nerr shift count exceeds the 16777216 bit limit\nerr shift count exceeds the 16777216 bit limit";

    let vm = run_vm_inline("bits_namespace", src).expect("vm run");
    let rust =
        build_run_rust_inline("bits_namespace", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM Bits contract changed");
    assert_eq!(rust, expected, "Rust Bits semantics diverged from the VM");
}

#[test]
fn rust_branch_path_partial_constructors_match_vm() {
    let src = r#"module BranchPathDifferential
    intent = "Literal discharge and catchable dynamic BranchPath construction"
    effects [Console.print]

fn child(index: Int) -> String
    ? "Shows either the dynamic child path or its validation error."
    match BranchPath.child(BranchPath.Root, index)
        Result.Ok(_) -> "ok"
        Result.Err(error) -> "err {error}"

fn parsed(raw: String) -> String
    ? "Shows either the parsed path or its validation error."
    match BranchPath.parse(raw)
        Result.Ok(_) -> "ok"
        Result.Err(error) -> "err {error}"

fn main() -> Unit
    ! [Console.print]
    literalChild = BranchPath.child(BranchPath.Root, 18446744073709551616)
    literalParsed = BranchPath.parse("2.0")
    Console.print("literal child ok")
    Console.print("literal parse ok")
    Console.print(child(-1))
    Console.print(child(4294967296))
    Console.print(parsed("2..0"))
"#;
    let expected = "literal child ok\nliteral parse ok\nerr BranchPath.child: `idx` must be non-negative\nok\nerr BranchPath.parse: invalid dewey-decimal path: `2..0`";

    let vm = run_vm_inline("branch_path_partial", src).expect("vm run");
    let rust = build_run_rust_inline("branch_path_partial", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM BranchPath contract changed");
    assert_eq!(rust, expected, "Rust BranchPath semantics diverged from VM");
}

#[test]
fn rust_map_iteration_order_matches_vm() {
    // A map iterates sorted by key on every read: `Map.keys`, `Map.values`
    // and `Map.entries` must agree with each other and with the VM. The
    // compiled backend used to walk `HashMap::values()` bare while `keys`
    // and `entries` beside it sorted, so the value order changed per process
    // and `zip(keys, values)` stopped matching `entries` — inside ONE
    // backend, on one run. Six string keys make an accidental agreement a
    // 1-in-720 event, and the zip-vs-entries line catches it without any
    // cross-run comparison. The integer-keyed map pins the comparator:
    // numeric order, not the printed digits that would put 10 before 2.
    let src = r#"module MapOrderDifferential
    intent = "Every map iteration read in one program, for cross-backend agreement"
    effects [Console.print]

fn stringKeyed() -> Map<String, Int>
    ? "Six keys inserted deliberately out of key order."
    m0 = Map.set({}, "z", 1)
    m1 = Map.set(m0, "a", 2)
    m2 = Map.set(m1, "m", 3)
    m3 = Map.set(m2, "k", 4)
    m4 = Map.set(m3, "e", 5)
    Map.set(m4, "t", 6)

fn intKeyed() -> Map<Int, String>
    ? "Keys whose numeric order differs from their printed order."
    m0 = Map.set({}, 10, "ten")
    m1 = Map.set(m0, 2, "two")
    Map.set(m1, 33, "lot")

fn joinStrings(xs: List<String>) -> String
    ? "Joins strings with a comma separator."
    match xs
        [] -> ""
        [x, ..rest] -> match rest
            [] -> x
            _ -> "{x},{joinStrings(rest)}"

fn joinInts(xs: List<Int>) -> String
    ? "Joins integers with a comma separator."
    match xs
        [] -> ""
        [x, ..rest] -> match rest
            [] -> "{x}"
            _ -> "{x},{joinInts(rest)}"

fn pairText(p: Tuple<String, Int>) -> String
    ? "Renders one key-value pair."
    match p
        (k, v) -> "{k}={v}"

fn joinPairs(xs: List<Tuple<String, Int>>) -> String
    ? "Joins key-value pairs with a comma separator."
    match xs
        [] -> ""
        [p, ..rest] -> match rest
            [] -> pairText(p)
            _ -> "{pairText(p)},{joinPairs(rest)}"

fn main() -> Unit
    ? "Print every read so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print(joinStrings(Map.keys(stringKeyed())))
    Console.print(joinInts(Map.values(stringKeyed())))
    Console.print(joinPairs(Map.entries(stringKeyed())))
    Console.print(joinPairs(List.zip(Map.keys(stringKeyed()), Map.values(stringKeyed()))))
    Console.print(joinInts(Map.keys(intKeyed())))
    Console.print(joinStrings(Map.values(intKeyed())))
"#;
    // Lines 3 and 4 are the correspondence check: zip(keys, values) must BE
    // entries, or `keys[i]` no longer names the key of `values[i]`.
    let expected = "a,e,k,m,t,z\n2,5,4,3,6,1\na=2,e=5,k=4,m=3,t=6,z=1\na=2,e=5,k=4,m=3,t=6,z=1\n2,10,33\ntwo,ten,lot";

    let vm = run_vm_inline("map_iteration_order", src).expect("vm run");
    let rust = build_run_rust_inline("map_iteration_order", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM map iteration order changed");
    assert_eq!(
        rust, expected,
        "Rust map iteration diverged from the VM — a map read stopped sorting by key"
    );
}

#[test]
fn rust_try_in_argument_position_matches_vm() {
    // `?` inside an argument returns from the function whose body contains it,
    // and the Rust backend gets this from `?` on a real `Result`. The VM reaches
    // the same answer only if its error exit knows the enclosing frame may be a
    // frameless leaf — which is what `leafTry` and `leafPar` here are: bodies
    // with no user call and no local binding, so their callers' `CALL_KNOWN` is
    // upgraded to `CALL_LEAF`. A VM that unwinds one frame too far ends the
    // program on the first failure, so the divergence shows up as a missing
    // trailing line rather than a changed one.
    let src = r#"module TryArgumentDifferential
    intent = "Every `?`-in-argument shape in one program, for cross-backend agreement"
    effects [Console.print]

fn leafTry(s: String) -> Result<Int, String>
    ? "Frameless leaf: `?` in the second argument of a builtin call."
    Int.div(100, Int.fromString(s)?)

fn twoTries(a: String, b: String) -> Result<Int, String>
    ? "Both arguments propagate, so the second fires on a partial stack."
    Int.div(Int.fromString(a)?, Int.fromString(b)?)

fn twoLevels(s: String) -> Result<Int, String>
    ? "`?` in the argument of a call that is itself an argument."
    Int.div(1000, Int.max(Int.fromString(s)?, 1))

fn leafPar(a: Int, b: Int) -> Result<Tuple<Int, Int>, String>
    ? "The `?!` error exit out of a frameless leaf."
    Result.Ok((Int.div(100, a), Int.div(100, b))?!)

fn letBound(s: String) -> Result<Int, String>
    ? "Control: binding the propagated value keeps the body off the leaf path."
    n = Int.fromString(s)?
    Int.div(100, n)

fn describe(r: Result<Int, String>) -> String
    ? "Render a result without pinning the error text."
    match r
        Result.Ok(v) -> "ok {v}"
        Result.Err(_) -> "err"

fn describePair(r: Result<Tuple<Int, Int>, String>) -> String
    ? "Render a product result without pinning the error text."
    match r
        Result.Ok(_) -> "ok"
        Result.Err(_) -> "err"

fn main() -> Unit
    ? "Print every shape so a backend that unwinds differently shows a diff."
    ! [Console.print]
    Console.print("{describe(leafTry("bad"))} {describe(leafTry("4"))}")
    Console.print("{describe(twoTries("bad", "2"))} {describe(twoTries("100", "bad"))} {describe(twoTries("100", "4"))}")
    Console.print("{describe(twoLevels("bad"))} {describe(twoLevels("9"))}")
    Console.print("{describePair(leafPar(0, 5))} {describePair(leafPar(4, 5))}")
    Console.print("{describe(letBound("bad"))} {describe(letBound("4"))}")
"#;
    let expected = "err ok 25\nerr err ok 25\nerr ok 111\nerr ok\nerr ok 25";

    let vm = run_vm_inline("try_in_argument", src).expect("vm run");
    let rust =
        build_run_rust_inline("try_in_argument", src).expect("rust compile + cargo build + run");
    assert_eq!(rust, expected, "Rust `?`-in-argument semantics changed");
    assert_eq!(
        vm, expected,
        "the VM propagated a `?` out of the wrong frame"
    );
}

#[test]
fn rust_literal_refinement_discharge_matches_vm() {
    let src = r#"module LiteralRefinement
    intent = "Discharged and fallible smart-constructor calls in one program"
    depends [Bytes]
    effects [Console.print]

fn describe(bytes: Bytes) -> String
    ? "Render a validated frame as hex plus its length."
    "{Bytes.toHex(bytes)}/{List.len(Bytes.octets(bytes))}"

fn dynamic(values: List<Int>) -> String
    ? "Validate a computed list through the fallible constructor."
    match Bytes.fromList(values)
        Result.Ok(bytes) -> describe(bytes)
        Result.Err(e) -> e

fn main() -> Unit
    ? "Print the discharged and fallible results side by side."
    ! [Console.print]
    Console.print(describe(Bytes.fromList([249, 190, 180, 217])))
    Console.print(describe(Bytes.fromList([])))
    Console.print(dynamic(List.concat([249, 190], [180, 217])))
    Console.print(dynamic([65, 256]))
"#;
    let expected = "f9beb4d9/4\n/0\nf9beb4d9/4\nbyte 256 at index 1 is outside 0..=255";

    let vm = run_vm_inline("literal_refinement_discharge", src).expect("vm run");
    let rust = build_run_rust_inline("literal_refinement_discharge", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM literal-discharge contract changed");
    assert_eq!(
        rust, expected,
        "Rust literal-discharge contract diverged from VM"
    );
}

/// `Bits.popCount` on the Rust backend: the program `cross_backend_stress`
/// runs on the VM and wasm-gc.
#[test]
fn rust_bits_pop_count_matches_vm() {
    let src = include_str!("fixtures/bits_pop_count.av");
    let expected = include_str!("fixtures/bits_pop_count.expected").trim();

    let vm = run_vm_inline("bits_pop_count", src).expect("vm run");
    let rust =
        build_run_rust_inline("bits_pop_count", src).expect("rust compile + cargo build + run");
    assert_eq!(vm.trim(), expected, "VM popCount contract changed");
    assert_eq!(rust.trim(), expected, "Rust popCount diverged from the VM");
}

/// The same rule over an `Int` carrier, on the Rust backend: the program
/// `cross_backend_stress` runs on the VM and wasm-gc.
#[test]
fn rust_literal_int_refinement_discharge_matches_vm() {
    let src = include_str!("fixtures/literal_int_refinement.av");
    let expected = include_str!("fixtures/literal_int_refinement.expected").trim();

    let vm = run_vm_inline("literal_int_refinement_discharge", src).expect("vm run");
    let rust = build_run_rust_inline("literal_int_refinement_discharge", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm.trim(), expected, "VM literal-discharge contract changed");
    assert_eq!(
        rust.trim(),
        expected,
        "Rust literal-discharge contract diverged from VM"
    );
}

/// A function whose Aver name is a Rust keyword, in MUTUAL tail recursion.
/// The trampoline variant is the name capitalised, and capitalising the
/// already-escaped `r#await` gave `R#await` — not an identifier, so the
/// emitted project stopped parsing there and every later declaration
/// vanished with it. `cargo check` alone would not have caught it any
/// earlier than a build, but nothing caught it at all, so the program had
/// to be built to be told. Both halves of the trigger have controls in the
/// same file: a keyword-named SELF-recursive fn (no trampoline) and a
/// mutual pair of ordinary names (trampoline, no escape).
///
/// The second half of the file is the plainer failure the same bug was
/// hiding: functions named with a Rust keyword that the escape table simply
/// did not list. `become`, `try`, `macro` and the rest need no recursion and
/// no trampoline — they were emitted as `pub fn become(…)` and the project
/// did not parse. They are here rather than in a second test because the
/// expensive part is the `cargo build`, and one build can carry both.
///
/// The third half, for the same reason, is one keyword name in each of the
/// OTHER positions the escape helper touches — a parameter, a `let`
/// binding, a match binder, a record field and a module-level binding — so
/// the claim that every such position is escaped is backed by a build and a
/// run rather than by reading the emitter.
#[test]
fn rust_keyword_named_mutual_recursion_builds_and_matches_vm() {
    let src = r#"module KeywordMutualTco
    intent = "Rust keywords as function names, in mutual tail recursion"
    effects [Console.print]

record Holder
    move: Int

static = 41

fn await(budget: Int) -> String
    ? "Hand off to the other one."
    match budget > 0
        true -> resume(budget - 1)
        false -> "done"

fn resume(budget: Int) -> String
    ? "Hand back."
    match budget > 0
        true -> await(budget - 1)
        false -> "handed back"

fn impl(n: Int) -> Int
    ? "A second keyword pair, so one repaired name is not enough to pass."
    match n == 0
        true -> 0
        false -> move(n - 1)

fn move(n: Int) -> Int
    ? "The other half of the second keyword pair."
    match n == 0
        true -> 1
        false -> impl(n - 1)

fn unsafe(n: Int) -> Int
    ? "Control: a keyword name that only recurses into itself."
    match n == 0
        true -> 7
        false -> unsafe(n - 1)

fn ping(n: Int) -> Int
    ? "Control: mutual recursion between ordinary names."
    match n == 0
        true -> 11
        false -> pong(n - 1)

fn pong(n: Int) -> Int
    ? "The other ordinary half."
    match n == 0
        true -> 13
        false -> ping(n - 1)

fn become(n: Int) -> Int
    ? "Reserved for a future Rust, and named in the issue. No recursion."
    n + 1

fn try(n: Int) -> Int
    ? "Reserved since edition 2018."
    n + 2

fn macro(n: Int) -> Int
    ? "Reserved, and not the same word as the weak keyword macro_rules."
    n + 3

fn final(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 4

fn virtual(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 5

fn do(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 6

fn priv(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 7

fn abstract(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 8

fn override(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 9

fn typeof(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 10

fn unsized(n: Int) -> Int
    ? "Reserved for a future Rust."
    n + 11

fn gen(n: Int) -> Int
    ? "Strict since edition 2024, which is what the generated crate asks for."
    n + 12

fn takesKeyword(box: Int) -> Int
    ? "A keyword name in the parameter position."
    box + 1

fn bindsKeyword(n: Int) -> Int
    ? "A keyword name in the let-binding position."
    loop = n + 1
    loop + 1

fn matchesKeyword(xs: List<Int>) -> Int
    ? "Keyword names in both match-binder positions."
    match xs
        [] -> 0
        [ref, ..mut] -> ref + 1

fn readsKeywordField(h: Holder) -> Int
    ? "A keyword name in the record-field position."
    h.move

fn main() -> Unit
    ? "Print every shape, so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print(await(4))
    Console.print(resume(2))
    Console.print("{impl(4)} {move(4)} {unsafe(3)} {ping(4)} {pong(4)}")
    Console.print("{become(0)} {try(0)} {macro(0)} {final(0)} {virtual(0)} {do(0)}")
    Console.print("{priv(0)} {abstract(0)} {override(0)} {typeof(0)} {unsized(0)} {gen(0)}")
    Console.print("{takesKeyword(1)} {bindsKeyword(1)} {matchesKeyword([5, 6])} {readsKeywordField(Holder(move = 3))} {static}")
"#;
    let expected = "done\nhanded back\n0 1 7 11 13\n1 2 3 4 5 6\n7 8 9 10 11 12\n2 3 6 3 41";

    let vm = run_vm_inline("keyword_mutual_tco", src).expect("vm run");
    let rust =
        build_run_rust_inline("keyword_mutual_tco", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM keyword-name contract changed");
    assert_eq!(
        rust, expected,
        "Rust keyword-name contract diverged from VM"
    );
}

/// A mutual tail-call group keeps a param every member hands on unchanged
/// (an invariant) out of the trampoline's state. Two shapes used to break
/// the build: a member that names the invariant differently (`o` for `op`),
/// and a bounce that passes the invariant at a second, ordinary position
/// too (`walkB(op, op, ..)`).
#[test]
fn mutual_tail_group_invariant_renamed_or_passed_twice_matches_vm() {
    let src = r#"module MutualInvariant
    intent = "Invariants of mutual tail-call groups, renamed and passed twice"
    effects [Console.print]

record Op
    shift: Int
    label: String

fn walkA(op: Op, n: Int, acc: Int) -> Int
    ? "Hands the op on unchanged."
    match n == 0
        true -> acc
        false -> walkB(op, n - 1, acc + op.shift)

fn walkB(o: Op, n: Int, acc: Int) -> Int
    ? "The same op under another name."
    match n == 0
        true -> acc + 1
        false -> walkA(o, n - 1, acc - 1)

fn twiceA(op: Op, x: Op, n: Int) -> Int
    ? "Hands the op on unchanged, and in place of x as well."
    match n == 0
        true -> x.shift
        false -> twiceB(op, op, n - 1)

fn twiceB(op: Op, x: Op, n: Int) -> Int
    ? "Builds a fresh x."
    match n == 0
        true -> x.shift + 1
        false -> twiceA(op, Op(shift = x.shift + 1, label = x.label), n - 1)

fn main() -> Unit
    ? "Runs both groups deep."
    ! [Console.print]
    op = Op(shift = 3, label = "op")
    Console.print("{walkA(op, 1000001, 0)} {walkB(op, 10, 0)}")
    Console.print("{twiceA(op, op, 1000001)} {twiceB(op, op, 10)}")
"#;
    let expected = "1000004 11\n4 4";
    let vm = run_vm_inline("mutual_invariant", src).expect("vm run");
    assert_eq!(vm, expected, "VM contract changed");
    let rust = build_run_rust_inline("mutual_invariant", src).expect("rust build + run");
    assert_eq!(rust, expected, "Rust diverged from the VM");
}

/// The reported shape, which is a two-file one: the keyword-named mutual
/// pair lives in a DEPENDENCY module and the entry module only calls into
/// it. That is how the bug stayed hidden — the dep module had been
/// uncompilable since it was written, and nothing said so until a second
/// module depended on it. The dep module's trampoline is emitted into its
/// own file, so the entry module's output can be perfectly fine while the
/// project still does not build.
#[test]
fn rust_dep_module_keyword_mutual_recursion_builds_and_matches_vm() {
    let dir = temp_dir("dep_keyword_mutual");
    fs::write(
        dir.join("Worker.av"),
        r#"module Worker
    exposes [await]
    intent = "A keyword-named mutual pair, in a dependency module"
    effects []

fn await(n: Int) -> Int
    ? "One half of the pair."
    match n == 0
        true -> 0
        false -> resume(n - 1)

fn resume(n: Int) -> Int
    ? "The other half."
    match n == 0
        true -> 1
        false -> await(n - 1)
"#,
    )
    .expect("write dep module");

    let entry = dir.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Worker]
    intent = "Calls a keyword-named mutual pair across a module boundary"
    effects [Console.print]

fn main() -> Unit
    ? "Both parities of the bounce, so a dropped arm shows as a diff."
    ! [Console.print]
    Console.print("{Worker.await(4)} {Worker.await(5)}")
"#,
    )
    .expect("write entry module");

    let expected = "0 1";

    let vm = run_vm(&entry, Some(&dir)).expect("vm run");
    assert_eq!(vm, expected, "VM contract changed");

    let project = dir.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    compile_rust(&entry, &project, "dep_keyword_mutual", Some(&dir), &[])
        .expect("aver compile --target rust");
    let bin = cargo_build(&project, "dep_keyword_mutual").expect("cargo build");
    let out = Command::new(&bin).output().expect("run compiled binary");
    assert!(
        out.status.success(),
        "compiled binary failed:\n{}",
        format_output(&out)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        expected,
        "Rust dep-module keyword contract diverged from VM"
    );

    let _ = fs::remove_dir_all(&dir);
}

/// `crate`, `self`, `super`, `Self` and `_` have no Rust spelling at all —
/// `r#` is a parse error for exactly these five — so the backend renames
/// them, with the `_avr_` prefix, at the one place it spells a name. This
/// drives the real `aver compile` binary and then `cargo build`, because
/// the rename is only worth anything if the project it produces builds:
/// before it, each of these programs compiled "successfully" and then
/// failed `cargo build` naming a generated file the user never wrote.
///
/// Four shapes, because each reached the emitter down a different path:
///
/// - `fn crate` — a function name, the shape reported on the issue. It
///   emitted `pub fn crate(…)`.
/// - `self = 41` at module level — a `TopLevel::Stmt`, which lives in
///   `ctx.items` and in no `FnDef`. It emitted `let r#self = 41i64;` into
///   `fn main` and the build stopped at ``error: `self` cannot be a raw
///   identifier``.
/// - `fn _` — `_` is not a keyword, it is Rust's wildcard, so it is not in
///   the escape table and needs no escape; it simply has no identifier
///   form. It emitted `pub fn _(…)` and the build stopped at ``expected
///   identifier, found reserved identifier``.
/// - a record field named `super`, read back through `h.super` — the
///   declaration and the read have to be renamed together or the struct
///   and its use disagree.
///
/// Each program runs on the VM first and the Rust answer has to match it:
/// the rename is a spelling, so it must not be observable in the output.
#[test]
fn never_spellable_rust_names_build_and_match_vm() {
    // (label, source, expected output)
    let cases: &[(&str, &str, &str)] = &[
        (
            "fn_name",
            r#"module NeverRaw
    intent = "A function named with a word Rust cannot spell"
    effects [Console.print]

fn crate(n: Int) -> Int
    ? "Legal Aver, unspellable Rust."
    n + 1

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{crate(1)}")
"#,
            "2",
        ),
        (
            "module_level_binding",
            r#"module NeverRawTop
    intent = "A module-level binding named with a word Rust cannot spell"
    effects [Console.print]

self = 41

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print(String.fromInt(self))
"#,
            "41",
        ),
        (
            "underscore_fn_name",
            r#"module NeverRawUnderscore
    intent = "A function named with a single underscore"
    effects [Console.print]

fn _(n: Int) -> Int
    ? "Legal Aver, no Rust identifier form."
    n + 1

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print(String.fromInt(_(1)))
"#,
            "2",
        ),
        (
            "record_field",
            r#"module NeverRawField
    intent = "A record field named with a word Rust cannot spell"
    effects [Console.print]

record Holder
  super: Int

fn read(h: Holder) -> Int
    ? "Read the field back."
    h.super

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print(String.fromInt(read(Holder(super = 7))))
"#,
            "7",
        ),
    ];

    for (label, src, expected) in cases {
        let name = format!("never_spellable_{label}");
        let vm = run_vm_inline(&name, src).expect("vm run");
        assert_eq!(vm, *expected, "the {label} program is valid Aver");
        let rust = build_run_rust_inline(&name, src)
            .unwrap_or_else(|e| panic!("the {label} program must build and run: {e}"));
        assert_eq!(
            rust, vm,
            "the rename is a spelling and must not change what the {label} \
             program prints"
        );
    }
}

/// `_` as a PARAMETER, in every shape that decides how it is spelled.
///
/// A parameter normally lowers to a Rust pattern, where a bare `_` means
/// "discard" and builds fine. Three things take it out of pattern position,
/// and each one used to be a broken build:
///
/// - a self-TCO signature makes every parameter the loop's mutable state,
///   so it emitted `pub fn count(mut n: i64, mut _: aver_rt::AverInt)` —
///   ``error: `mut` must be followed by a named binding``;
/// - a mutual-TCO trampoline binds `__MutualTco1::IsEven(mut n, mut _)` and
///   the wrapper then passes the parameter BY NAME to build the variant,
///   `__MutualTco1::IsEven(n, _)` — ``error: in expressions, `_` can only
///   be used on the left-hand side of an assignment``;
/// - a COLLECTION parameter needs no recursion at all: `own_param` proves
///   it uniquely owned when every call site passes a fresh value, and the
///   owned spelling is `mut p: T`. The proof is by parameter position and
///   never reads the name, so it reached the wildcard too and emitted
///   `mut _: aver_rt::AverVector<…>`, which `aver check`, `aver run` and
///   `aver compile` all accepted and only `cargo build` rejected.
///
/// All four shapes (the three above plus the plain pattern) now spell the
/// parameter `_avr__` and build. Every program runs on the VM first, and
/// the Rust answer has to match it.
#[test]
fn a_wildcard_parameter_builds_in_every_shape_that_spells_it() {
    let cases: &[(&str, &str, &str)] = &[
        (
            "self_tco",
            r#"module WildcardSelfTco
    intent = "A wildcard parameter on a self tail-recursive function"
    effects [Console.print]

fn count(n: Int, _: Int) -> Int
    ? "Count down, ignoring the second argument."
    match n == 0
        true -> 0
        false -> count(n - 1, 0)

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{count(5, 7)}")
"#,
            "0",
        ),
        (
            "mutual_tco",
            r#"module WildcardMutualTco
    intent = "A wildcard parameter on a mutually tail-recursive pair"
    effects [Console.print]

fn isEven(n: Int, _: Int) -> Bool
    ? "True when n is even, ignoring the second argument."
    match n == 0
        true -> true
        false -> isOdd(n - 1, 0)

fn isOdd(n: Int, _: Int) -> Bool
    ? "True when n is odd, ignoring the second argument."
    match n == 0
        true -> false
        false -> isEven(n - 1, 0)

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{isEven(10, 7)}")
"#,
            "true",
        ),
        (
            "non_tail",
            r#"module WildcardNonTail
    intent = "A wildcard parameter on a function that recurses off tail position"
    effects [Console.print]

fn deep(n: Int, _: Int) -> Int
    ? "Recurse off tail position, ignoring the second argument."
    match n == 0
        true -> 0
        false -> deep(n - 1, 0) + 1

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{deep(5, 7)}")
"#,
            "5",
        ),
        (
            "owned_collection",
            r#"module WildcardOwnedCollection
    intent = "A wildcard collection parameter on a function that is not recursive"
    effects [Console.print]

fn firstOr(v: Vector<Int>, _: Vector<Int>) -> Int
    ? "Read the first cell of v, ignoring the second vector entirely."
    Option.withDefault(Vector.get(v, 0), 0)

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{firstOr(Vector.new(5, 7), Vector.new(3, 1))}")
"#,
            "7",
        ),
    ];

    for (label, src, expected) in cases {
        let name = format!("wildcard_param_{label}");
        let vm = run_vm_inline(&name, src).expect("vm run");
        assert_eq!(vm, *expected, "the {label} program is valid Aver");
        let rust = build_run_rust_inline(&name, src)
            .unwrap_or_else(|e| panic!("the {label} wildcard parameter must build and run: {e}"));
        assert_eq!(
            rust, vm,
            "the Rust backend must spell a wildcard parameter without \
             changing what the {label} program prints"
        );
    }
}

/// A mutually tail-recursive function named `self`, and one named `ſelf`
/// (U+017F LATIN SMALL LETTER LONG S, which upper-cases to `S`). Both
/// capitalise onto `Self` — the one Rust keyword with no raw spelling — so
/// both need the trampoline variant renamed, and `ſelf` reaches it without
/// being one of the five names itself. The function keeps its own spelling
/// where it is legal (`ſelf` is a fine Rust identifier; `self` is not), so
/// this also pins that the rename follows the position, not the program.
#[test]
fn mutual_recursion_through_a_name_that_capitalises_onto_self_builds() {
    let cases: &[(&str, &str)] = &[
        (
            "self",
            r#"module SelfMutual
    intent = "Mutual tail recursion through a function named self"
    effects [Console.print]

fn self(n: Int) -> Int
    ? "Count down through the other one."
    match n == 0
        true -> 0
        false -> other(n - 1)

fn other(n: Int) -> Int
    ? "Count down through self."
    match n == 0
        true -> 1
        false -> self(n - 1)

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{self(6)}")
"#,
        ),
        (
            "long_s",
            r#"module LongSMutual
    intent = "Mutual tail recursion through a name that capitalises onto Self"
    effects [Console.print]

fn ſelf(n: Int) -> Int
    ? "Count down through the other one."
    match n == 0
        true -> 0
        false -> other(n - 1)

fn other(n: Int) -> Int
    ? "Count down through the long-s one."
    match n == 0
        true -> 1
        false -> ſelf(n - 1)

fn main() -> Unit
    ? "Print it."
    ! [Console.print]
    Console.print("{ſelf(6)}")
"#,
        ),
    ];

    for (label, src) in cases {
        let name = format!("capitalises_onto_self_{label}");
        let vm = run_vm_inline(&name, src).expect("vm run");
        assert_eq!(vm, "0", "the {label} program is valid Aver");
        let rust = build_run_rust_inline(&name, src)
            .unwrap_or_else(|e| panic!("the {label} trampoline must build and run: {e}"));
        assert_eq!(rust, vm, "Rust answer for the {label} trampoline");
    }
}

/// A verify block on a keyword-named function, driven through the real
/// `cargo test` of the emitted project.
///
/// The generated test is named `test_<fn>_case_<n>`, so the function name
/// lands in the MIDDLE of an identifier. Escaping it first produced `fn
/// test_r#await_case_1()`, where the `#` ends the identifier: the project
/// still passed `cargo build` (the verify module is `#[cfg(test)]`) and
/// only fell over under `cargo test` with ``error: prefix `test_r` is
/// unknown``. `become` is here as well as `await` because `become` was not
/// in the escape table before this change — `verify become` used to emit a
/// perfectly good `test_become_case_1`, so completing the table turned a
/// working program into a broken one until the composition was fixed too.
#[test]
fn rust_verify_block_on_a_keyword_named_fn_passes_cargo_test() {
    let src = r#"module KeywordVerify
    intent = "Verify blocks on keyword-named functions must survive cargo test"
    effects [Console.print]

fn await(n: Int) -> Int
    ? "A keyword name that was already escaped before this change."
    n + 1

fn become(n: Int) -> Int
    ? "A keyword name the escape table only learned in this change."
    n + 2

verify await
    await(1) => 2
    await(2) => 3

verify become
    become(1) => 3

fn main() -> Unit
    ? "Print both, so the binary exercises them too."
    ! [Console.print]
    Console.print("{await(1)} {become(1)}")
"#;

    let name = "keyword_verify";
    let ws = temp_dir(name);
    let src_file = ws.join(format!("{name}.av"));
    fs::write(&src_file, src).expect("write source");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&src_file, &project, name, None, &[])?;
        cargo_test_in(&project, &shared_target_dir())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.expect("rust compile + cargo test of the generated verify module");
}

/// `aver compile` can succeed while leaving a MIR-walker `compile_error!` in
/// the emitted project, so this regression must drive `Tcp.sendBytes` through
/// a real `cargo build`. Invalid raw lists fail at `Bytes.fromList` before any
/// socket connection, pinning the refinement boundary on both backends.
#[test]
fn rust_tcp_send_bytes_builds_with_the_bytes_refinement_boundary() {
    let src = r#"module TcpSendBytesRange
    intent = "Rust codegen must render Tcp.sendBytes with nominal Bytes"
    depends [Bytes]
    effects [Console, Tcp]

fn report(payload: List<Int>) -> Unit
    ? "Print the result of validating one binary payload."
    ! [Console.print, Tcp.sendBytes]
    match Bytes.fromList(payload)
        Result.Err(e) -> Console.print(e)
        Result.Ok(bytes) -> match Tcp.sendBytes("127.0.0.1", 1, bytes)
            Result.Ok(_) -> Console.print("unexpected-ok")
            Result.Err(e) -> Console.print(e)

fn main() -> Unit
    ! [Console.print, Tcp.sendBytes]
    report([65, 256])
    report([65, 1208925819614629174706176])
"#;
    let expected = "byte 256 at index 1 is outside 0..=255\n\
                    byte 1208925819614629174706176 at index 1 is outside 0..=255";

    let vm = run_vm_inline("tcp_send_bytes_range", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_send_bytes_range", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM Bytes refinement contract changed");
    assert_eq!(
        rust, expected,
        "Rust Bytes refinement contract diverged from VM"
    );
}

#[test]
fn rust_tcp_read_bytes_builds_with_nominal_bytes() {
    let src = r#"module TcpReadBytesBuild
    intent = "Rust codegen must render Tcp.readBytes with nominal Bytes"
    depends [Bytes]
    effects [Console, Tcp]

fn readFrame(conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Read one exact binary frame."
    ! [Tcp.readBytes]
    Tcp.readBytes(conn, count)

fn main() -> Unit
    ! [Console.print]
    Console.print("compiled")
"#;

    let vm = run_vm_inline("tcp_read_bytes_build", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_read_bytes_build", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "compiled");
    assert_eq!(rust, vm, "Rust Tcp.readBytes codegen diverged from VM");
}

#[test]
fn rust_tcp_poll_and_read_some_build_through_the_standard_provider() {
    let src = r#"module TcpPollReadSomeBuild
    intent = "Rust codegen must render readiness and partial binary reads through the Tcp provider"
    depends [Bytes]
    effects [Console, Tcp]

fn ready(sockets: Map<Int, Tcp.Socket>, timeoutMs: Int) -> Result<List<Int>, String>
    ? "Return caller-owned IDs whose sockets can make progress without waiting."
    ! [Tcp.poll]
    Tcp.poll(sockets, timeoutMs)

fn readChunk(conn: Tcp.Connection, maxBytes: Int) -> Result<Bytes, String>
    ? "Read the bytes currently available, up to the caller's bound."
    ! [Tcp.readSome]
    Tcp.readSome(conn, maxBytes)

fn main() -> Unit
    ! [Console.print]
    Console.print("compiled")
"#;

    let vm = run_vm_inline("tcp_poll_read_some_build", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_poll_read_some_build", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "compiled");
    assert_eq!(rust, vm, "Rust Tcp.poll/readSome codegen diverged from VM");
}

#[test]
fn rust_disk_bytes_build_and_preserve_non_utf8_octets() {
    let data_root = temp_dir("disk_bytes_data");
    let data_file = data_root.join("payload.bin");
    let path = aver_path_literal(&data_file);
    let src = format!(
        r#"module DiskBytes
    intent = "Generated Rust must carry nominal Bytes through the Disk provider"
    depends [Bytes]
    effects [Console.print, Disk.writeBytes, Disk.appendBytes, Disk.readBytesAt, Disk.readBytes, Disk.size]

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.writeBytes, Disk.appendBytes, Disk.readBytesAt, Disk.readBytes, Disk.size]
    written = Disk.writeBytes("{path}", Bytes.fromList([0, 127, 128, 255]))?
    appended = Disk.appendBytes("{path}", Bytes.fromList([1, 2]))?
    slice = Disk.readBytesAt("{path}", 2, 99)?
    past = Disk.readBytesAt("{path}", 99, 4)?
    content = Disk.readBytes("{path}")?
    size = Disk.size("{path}")?
    shown = Console.print("{{Bytes.toHex(content)}}:{{Bytes.toHex(slice)}}:{{Bytes.toHex(past)}}:{{size}}")
    Result.Ok(Unit)
"#
    );

    let result = (|| -> Result<(), String> {
        let vm = run_vm_inline("disk_bytes", &src)?;
        let rust = build_run_rust_inline("disk_bytes", &src)?;
        if vm != "007f80ff0102:80ff0102::6" {
            return Err(format!("VM Disk byte result changed: {vm}"));
        }
        if rust != vm {
            return Err(format!("Rust Disk byte result diverged: {rust}"));
        }
        let bytes = fs::read(&data_file).map_err(|error| format!("read payload: {error}"))?;
        if bytes != [0, 127, 128, 255, 1, 2] {
            return Err(format!("Disk payload changed: {bytes:?}"));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&data_root);
    result.expect("VM/Rust Disk byte parity");
}

#[test]
fn rust_disk_sync_accepts_a_file_and_a_directory_and_leaves_content_alone() {
    let data_root = temp_dir("disk_sync_data");
    let data_file = data_root.join("payload.bin");
    fs::write(&data_file, [0, 127, 128, 255]).expect("write sync fixture");
    let file = aver_path_literal(&data_file);
    let dir = aver_path_literal(&data_root);
    let missing = aver_path_literal(&data_root.join("absent.bin"));
    let src = format!(
        r#"module DiskSync
    intent = "Generated Rust must reach Disk.sync through the provider table"
    effects [Console.print, Disk.sync]

fn shape(outcome: Result<Unit, String>) -> String
    ? "Reduce an outcome to ok/err so host error text stays out of the output."
    match outcome
        Result.Ok(_) -> "ok"
        Result.Err(_) -> "err"

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.sync]
    onFile = shape(Disk.sync("{file}"))
    onDir = shape(Disk.sync("{dir}"))
    onMissing = shape(Disk.sync("{missing}"))
    shown = Console.print("{{onFile}}:{{onDir}}:{{onMissing}}")
    Result.Ok(Unit)
"#
    );

    let result = (|| -> Result<(), String> {
        let vm = run_vm_inline("disk_sync", &src)?;
        let rust = build_run_rust_inline("disk_sync", &src)?;
        // A file and a directory both sync; an absent path does not.
        // The directory half is what makes a new file's name durable,
        // and it only works because the provider opens read-only.
        if vm != "ok:ok:err" {
            return Err(format!("VM Disk.sync result changed: {vm}"));
        }
        if rust != vm {
            return Err(format!("Rust Disk.sync result diverged: {rust}"));
        }
        let bytes = fs::read(&data_file).map_err(|error| format!("read payload: {error}"))?;
        if bytes != [0, 127, 128, 255] {
            return Err(format!("Disk.sync rewrote the file it flushed: {bytes:?}"));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&data_root);
    result.expect("VM/Rust Disk.sync parity");
}

#[test]
fn rust_tcp_write_bytes_builds_with_nominal_bytes() {
    let src = r#"module TcpWriteBytesBuild
    intent = "Rust codegen must render Tcp.writeBytes with nominal Bytes"
    depends [Bytes]
    effects [Console, Tcp]

fn writeFrame(conn: Tcp.Connection, payload: Bytes) -> Result<Unit, String>
    ? "Write one exact binary frame."
    ! [Tcp.writeBytes]
    Tcp.writeBytes(conn, payload)

fn main() -> Unit
    ! [Console.print]
    Console.print("compiled")
"#;

    let vm = run_vm_inline("tcp_write_bytes_build", src).expect("vm run");
    let rust = build_run_rust_inline("tcp_write_bytes_build", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "compiled");
    assert_eq!(rust, vm, "Rust Tcp.writeBytes codegen diverged from VM");
}

/// `Crypto.sha256` produces a `Digest32` even when the program never names
/// `Crypto.Digest32` in `depends`. The emitted Rust references
/// `crate::aver_generated::crypto::digest32::Digest32`, so the owning
/// standard module must join the generated project implicitly — this used to
/// pass `aver compile` and then fail `cargo build` with E0433. The printed
/// tag derives from the digests (structural equality of two hashes), so the
/// sha256 calls stay observable: a future optimizer that deleted an unused
/// pure call would otherwise silently stop exercising the emission path
/// while this test kept passing.
#[test]
fn rust_sha256_builds_without_digest32_in_depends() {
    let src = r#"module Sha256ImplicitDigest
    intent = "Crypto.sha256 must build even when depends omits Crypto.Digest32"
    depends [Bytes]
    effects [Console.print]

fn hashTag(xs: List<Int>, ys: List<Int>) -> Result<String, String>
    ? "Hash both octet lists and report whether the digests agree."
    first = Crypto.sha256(Bytes.fromList(xs)?)
    second = Crypto.sha256(Bytes.fromList(ys)?)
    match first == second
        true -> Result.Ok("digests-agree")
        false -> Result.Ok("digests-differ")

fn main() -> Unit
    ? "Hash byte lists and print digest agreement."
    ! [Console.print]
    match hashTag([1, 2, 3], [1, 2, 3])
        Result.Ok(tag) -> Console.print(tag)
        Result.Err(msg) -> Console.print(msg)
    match hashTag([1, 2, 3], [9, 9, 9])
        Result.Ok(tag) -> Console.print(tag)
        Result.Err(msg) -> Console.print(msg)
"#;

    let vm = run_vm_inline("sha256_implicit_digest32", src).expect("vm run");
    let rust = build_run_rust_inline("sha256_implicit_digest32", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "digests-agree\ndigests-differ");
    assert_eq!(rust, vm, "Rust Crypto.sha256 codegen diverged from VM");
}

/// The UTF-8 builtins own a source-defined `Bytes` type even when the caller
/// never imports its namespace. Both the VM's module integration and generated
/// Rust project must therefore follow the implicit source-typed builtin edge.
/// The non-ASCII text makes a code-point-list implementation observably wrong.
#[test]
fn rust_utf8_round_trip_builds_without_bytes_in_depends() {
    let src = r#"module Utf8ImplicitBytes
    intent = "UTF-8 conversion owns its Bytes dependency implicitly."
    effects [Console.print]

fn decode(bytes: Bytes) -> String
    ? "Render a decode result without propagating it."
    match String.fromUtf8(bytes)
        Result.Ok(text) -> text
        Result.Err(error) -> error

fn main() -> Unit
    ! [Console.print]
    Console.print(decode(String.toUtf8("Zażółć 🦀")))
"#;

    let vm = run_vm_inline("utf8_implicit_bytes", src).expect("vm run");
    let rust = build_run_rust_inline("utf8_implicit_bytes", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "Zażółć 🦀");
    assert_eq!(rust, vm, "Rust UTF-8 conversion diverged from VM");
}

/// This is the btc-listener path from #1195: exact-width integer encoding
/// must build a packed `Bytes` carrier directly, keep arbitrary-precision
/// values intact, and work without a handwritten `depends [Bytes]` edge.
#[test]
fn rust_int_endian_codecs_match_vm_and_build_implicit_bytes() {
    let src = r#"module EndianImplicitBytes
    intent = "Fixed-width integer encoding owns its Bytes dependency implicitly."
    effects [Console.print]

fn encodeBig(value: Int, width: Int) -> Result<Bytes, String>
    ? "Keep both arguments dynamic to exercise the fallible path."
    Int.toBigEndian(value, width)

fn encodeLittle(value: Int, width: Int) -> Result<Bytes, String>
    ? "Keep both arguments dynamic to exercise the fallible path."
    Int.toLittleEndian(value, width)

fn showBig(result: Result<Bytes, String>) -> String
    match result
        Result.Ok(bytes) -> String.fromInt(Int.fromBigEndian(bytes))
        Result.Err(error) -> error

fn showLittleAsBig(result: Result<Bytes, String>) -> String
    match result
        Result.Ok(bytes) -> String.fromInt(Int.fromBigEndian(bytes))
        Result.Err(error) -> error

fn showLittle(result: Result<Bytes, String>) -> String
    match result
        Result.Ok(bytes) -> String.fromInt(Int.fromLittleEndian(bytes))
        Result.Err(error) -> error

fn main() -> Unit
    ! [Console.print]
    Console.print("{showBig(encodeBig(258, 2))}/{showLittleAsBig(encodeLittle(258, 2))}/{showLittle(encodeLittle(258, 2))}/{Int.fromBigEndian(Int.toBigEndian(18446744073709551616, 9))}/{Int.fromLittleEndian(Int.toLittleEndian(18446744073709551616, 9))}")
"#;

    let vm = run_vm_inline("int_endian_implicit_bytes", src).expect("vm run");
    let rust = build_run_rust_inline("int_endian_implicit_bytes", src)
        .expect("rust compile + cargo build + run");
    assert_eq!(vm, "258/513/258/18446744073709551616/18446744073709551616");
    assert_eq!(rust, vm, "Rust Int endian codecs diverged from VM");
}

/// An untyped `[]` used to become `AverList` even when compared with an
/// `AverIntList`. Real compilation catches the representation mismatch;
/// VM parity also checks both operand orders, inequality and nested lists.
#[test]
fn rust_empty_list_equality_matches_vm() {
    assert_plain_parity("tests/fixtures/empty_list_equality.av", None)
        .expect("empty list equality must compile and match the VM");
}

/// `List.drop` on the compiled backend: stepping through a list must see
/// exactly what stepping by destructuring sees, and the two answers are
/// printed side by side so a skipped or duplicated element shows up in the
/// line itself.
///
/// This is the backend the issue was measured on (#913), and the only one
/// whose count clamp lives in the emitted expression rather than in a shared
/// builtin: reading `to_usize().unwrap_or(usize::MAX)` straight had a
/// negative count DROP the whole list here and drop nothing everywhere else,
/// with `List.take` inverted the same way. No cheaper harness reaches that —
/// the wasm-gc and VM spellings of the clamp are separate code — so the
/// negative-count lines below are the whole reason this case is in the
/// default tier.
#[test]
fn rust_list_drop_walk_matches_destructuring_and_the_vm() {
    let src = r#"module ListDropWalk
    intent = "Stepping a list with List.drop must agree with destructuring"
    effects [Console.print]

fn built(n: Int, acc: List<Int>) -> List<Int>
    ? "A list of n elements, grown from the front."
    match n <= 0
        true -> acc
        false -> built(n - 1, List.prepend(n, acc))

fn grown(i: Int, n: Int, acc: List<Int>) -> List<Int>
    ? "A list grown from the back, so the runtime holds it as a spine."
    match i >= n
        true -> acc
        false -> grown(i + 1, n, List.concat(acc, [i]))

fn walkByDrop(xs: List<Int>, step: Int, acc: Int) -> Int
    ? "Advance step at a time with List.drop."
    match xs
        [] -> acc
        [head, ..tail] -> walkByDrop(List.drop(xs, step), step, acc + head)

fn skip(xs: List<Int>, n: Int) -> List<Int>
    ? "Step over n elements by destructuring."
    match n <= 0
        true -> xs
        false -> skipOne(xs, n)

fn skipOne(xs: List<Int>, n: Int) -> List<Int>
    ? "One element at a time."
    match xs
        [] -> []
        [head, ..tail] -> skip(tail, n - 1)

fn walkByUncons(xs: List<Int>, step: Int, acc: Int) -> Int
    ? "The same walk, written as destructuring."
    match xs
        [] -> acc
        [head, ..tail] -> walkByUncons(skip(xs, step), step, acc + head)

fn joinInts(xs: List<Int>) -> String
    ? "Render a list of ints."
    match xs
        [] -> "."
        [x, ..rest] -> String.fromInt(x) + "," + joinInts(rest)

fn walks(xs: List<Int>) -> String
    ? "Both walks over the same list, side by side."
    String.fromInt(walkByDrop(xs, 7, 0)) + "/" + String.fromInt(walkByUncons(xs, 7, 0))

fn main() -> Unit
    ? "Walk three list shapes two ways, then the counts at the edges."
    ! [Console.print]
    prepended = built(200, [])
    spined = grown(0, 60, [])
    Console.print(walks(prepended))
    Console.print(walks(spined))
    Console.print(walks(List.concat(prepended, spined)))
    Console.print(joinInts(List.drop([1, 2, 3, 4, 5], 2)))
    Console.print(joinInts(List.drop([1, 2, 3, 4, 5], 0)))
    Console.print(joinInts(List.drop([1, 2, 3, 4, 5], 9)))
    Console.print(joinInts(List.drop([1, 2, 3, 4, 5], -3)))
    Console.print(joinInts(List.take([1, 2, 3, 4, 5], -3)))
"#;

    let expected = "2871/2871\n\
                    252/252\n\
                    3150/3150\n\
                    3,4,5,.\n\
                    1,2,3,4,5,.\n\
                    .\n\
                    1,2,3,4,5,.\n\
                    .";

    let vm = run_vm_inline("list_drop_walk", src).expect("vm run");
    let rust =
        build_run_rust_inline("list_drop_walk", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "the VM's List.drop contract changed");
    assert_eq!(rust, vm, "Rust List.drop / List.take diverged from the VM");
}

/// A program whose ONLY `Crypto.sha256` call sits in a verify case still
/// generates a project whose `#[cfg(test)]` module references
/// `crate::aver_generated::crypto::digest32::Digest32` — the implicit
/// stdlib-module scan must cover verify blocks, not just fn bodies. This
/// used to emit a project that passed `cargo build` and then failed
/// `cargo test` with E0433. Driving `cargo test` (not just the build) also
/// proves the digest-equality cases hold in the generated code.
///
/// The last verify case uses `Bytes.fromHex(…)?` directly, which doubles
/// as the regression for `?` inside a verify case: the generated test fn
/// returns `Result<(), String>` while generated stdlib fns error with
/// `AverStr`, so the emitter must convert at the `?` boundary (bare `?`
/// used to fail `cargo test` with E0277). It also pins that a discharged
/// literal `Bytes.fromList([1, 2])` and the equivalent value built
/// through the fallible `fromHex` path hash to the same digest.
#[test]
fn rust_sha256_verify_only_generates_testable_project() {
    let src = r#"module Sha256VerifyOnly
    intent = "A verify-only Crypto.sha256 call still generates a buildable test module"
    depends [Bytes]
    effects [Console.print]

fn describe(same: Bool) -> String
    ? "Describe digest agreement."
    match same
        true -> "same"
        false -> "different"

fn main() -> Unit
    ? "Print a fixed label."
    ! [Console.print]
    Console.print(describe(true))

verify describe
    describe(Crypto.sha256(Bytes.fromList([1, 2])) == Crypto.sha256(Bytes.fromList([1, 2]))) => "same"
    describe(Crypto.sha256(Bytes.fromList([1, 2])) == Crypto.sha256(Bytes.fromList([2, 1]))) => "different"
    describe(Crypto.sha256(Bytes.fromHex("0102")?) == Crypto.sha256(Bytes.fromList([1, 2]))) => "same"
"#;

    let name = "sha256_verify_only";
    let ws = temp_dir(name);
    let src_file = ws.join(format!("{name}.av"));
    fs::write(&src_file, src).expect("write source");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&src_file, &project, name, None, &[])?;
        cargo_test_in(&project, &shared_target_dir())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.expect("rust compile + cargo test of the generated verify module");
}

/// The #383 corruption class on the RUST backend: a Vector PARAM captured
/// into a record field AND own-mutated, both in the SAME fn on the SAME
/// param. `own_param`'s capture guard must keep the slot flagged so the
/// Rust emit keeps the `.clone()` at the mutation site (refcount-2
/// `Rc::make_mut` deep-copies → the record's snapshot is protected).
/// Build+run the emitted Rust and assert it equals the VM — a wrongly
/// skipped clone would diverge (1998 vs the correct 1006).
#[test]
fn rust_param_captured_and_mutated_in_same_fn_matches_vm() {
    let src = r#"module SameFnCapture
    intent = "Vector param captured into a record AND mutated in the same fn (#383 class)"
    depends []
    effects [Console.print]

record Holder
    snapshot: Vector<Int>

fn captureAndMutate(v: Vector<Int>) -> Int
    ? "store v in a record, then set position 0 to 999 on v; snapshot must read the original"
    h = Holder(snapshot = v)
    mutated = Option.withDefault(Vector.set(v, 0, 999), v)
    snap0 = Option.withDefault(Vector.get(h.snapshot, 0), 0 - 1)
    mut0 = Option.withDefault(Vector.get(mutated, 0), 0 - 1)
    snap0 + mut0

fn run() -> Int
    base = Option.withDefault(Vector.set(Vector.new(3, 0), 0, 7), Vector.new(3, 0))
    captureAndMutate(base)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    let vm = run_vm_inline("samecap", src).expect("vm run");
    let rust = build_run_rust_inline("samecap", src).expect("rust build+run");
    assert_eq!(vm, "1006", "VM must compute the immutable-model value");
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — a captured param's clone was wrongly skipped"
    );
}

/// Multi-tail-call Int-unboxing soundness hole (the C0 bug): a counter `n`
/// with TWO self-tail-call paths — one decrements (`n - 1`), one GROWS
/// (`n + 1_000_000_000_000_000_000`). The pre-fix recurrence recognizer saw
/// only the first (decrement) path, under-approximated `n`'s interval, and
/// marked it bare → the Rust backend emitted native `i64` for `n`, so at
/// runtime the growth path drove `n` past `i64::MAX` and `n + n` silently
/// WRAPPED in release (a panic in dev under `overflow-checks`). The fix
/// boxes `n` (every self-tail-call must be the SAME monotone decrement, else
/// the recurrence is unbounded). Build+run the emitted Rust and assert it
/// equals the VM's arbitrary-precision `10000000000000000016` — a wrongly
/// bare counter would diverge (wrap) or panic.
#[test]
fn rust_multi_tailcall_growing_counter_matches_vm() {
    let src = r#"module MultiTail
    intent = "two self-tail-call paths: one decrements, one grows — counter must box"
    depends []
    effects [Console.print]

fn loopit(n: Int, phase: Int) -> Int
    ? "phase 5000 decrements n; any other non-zero phase grows n past i64 range"
    match n
        0 -> n + n
        _ -> match phase
            0 -> n + n
            5000 -> loopit(n - 1, phase)
            _ -> loopit(n + 1000000000000000000, phase - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(loopit(8, 5)))
"#;
    let vm = run_vm_inline("multitail", src).expect("vm run");
    let rust = build_run_rust_inline("multitail", src).expect("rust build+run");
    assert_eq!(
        vm, "10000000000000000016",
        "VM computes the arbitrary-precision (no-wrap) value"
    );
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — the multi-tail-call counter was wrongly unboxed to i64 and wrapped/panicked"
    );
}

/// Transient intermediate-overflow Int-unboxing hole: a compound
/// `(n + i64::MAX) - i64::MAX` over a bare counter `n`. The whole-tree result
/// narrows back INTO `i64`, but the inner `n + i64::MAX` intermediate leaves
/// `i64`. The pre-fix `bare_expr_interval` checked only the FINAL result
/// interval, so the compound was marked bare and the Rust backend emitted a
/// MIXED tree — the inner `add` boxed-then-`to_i64()` then a raw `i64` outer
/// `.sub` — which does NOT compile (`E0599 no method 'sub' for i64`). This is
/// fail-closed (a compile error, never a silent wrap), but a valid Aver
/// program must still compile: the fix gates EVERY intermediate against `i64`
/// (mirroring the analysis-side `eval_interval` worst-join), so the whole
/// compound boxes. Build+run the emitted Rust and assert it builds and equals
/// the VM's exact `1`.
#[test]
fn rust_transient_intermediate_overflow_compound_matches_vm() {
    let src = r#"module IntermediateOverflow
    intent = "a compound whose intermediate leaves i64 but final narrows back must box"
    depends []
    effects [Console.print]

fn loopit(n: Int) -> Int
    ? "base case adds then subtracts i64::MAX; the intermediate overflows i64"
    match n
        1 -> (n + 9223372036854775807) - 9223372036854775807
        _ -> loopit(n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(loopit(5)))
"#;
    let vm = run_vm_inline("intermediate_overflow", src).expect("vm run");
    let rust = build_run_rust_inline("intermediate_overflow", src)
        .expect("rust build+run (pre-fix: E0599 — the mixed bare/boxed compound did not compile)");
    assert_eq!(
        vm, "1",
        "VM computes the exact value (the intermediate is ℤ, never wraps)"
    );
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — the transient-overflow compound was wrongly kept bare"
    );
}

/// The own_param perf win on the RUST backend, verified end-to-end: a
/// linearly-threaded Vector param the pass proves uniquely owned must (a)
/// build+run to the same result as the VM, and (b) emit the OWNED-by-value
/// in-place shape — `let __vec = v;` (a MOVE) NOT `let __vec = v.clone();`
/// — so the `Rc::make_mut` runs on a refcount-1 backing (native O(n)),
/// the whole point of wiring own_param into rust. A regression that drops
/// the graduation would re-introduce the `.clone()` (O(n²) COW) and trip
/// the emitted-shape assert even if the result stays correct.
#[test]
fn rust_owned_vector_param_emits_in_place_move_and_matches_vm() {
    let src = r#"module Fill
    intent = "linearly-threaded vector fill+sum — the own_param rust target"
    depends []
    effects [Console.print]

fn fillVector(v: Vector<Int>, n: Int, i: Int) -> Vector<Int>
    ? "tail-recursive fill: write i*i at position i"
    match i == n
        true -> v
        false -> fillVector(Option.withDefault(Vector.set(v, i, i * i), v), n, i + 1)

fn sumVector(v: Vector<Int>, n: Int, i: Int, acc: Int) -> Int
    ? "tail-recursive sum across positions 0..n"
    match i == n
        true -> acc
        false -> sumVector(v, n, i + 1, acc + Option.withDefault(Vector.get(v, i), 0))

fn run() -> Int
    v = fillVector(Vector.new(5, 0), 5, 0)
    sumVector(v, 5, 0, 0)

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(run()))
"#;
    // (a) build+run vs VM
    let vm = run_vm_inline("fillmove", src).expect("vm run");
    let rust = build_run_rust_inline("fillmove", src).expect("rust build+run");
    assert_eq!(vm, "30", "VM fill+sum value");
    assert_eq!(rust, vm, "Rust fill+sum diverged from VM");

    // (b) emitted-shape guard: the fusion in `fillVector` must MOVE the
    // owned param (no `.clone()`).
    let ws = temp_dir("fillmove_emit");
    let f = ws.join("fillmove.av");
    fs::write(&f, src).expect("write");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("project dir");
    compile_rust(&f, &project, "fillmove", None, &[]).expect("compile rust");
    let emitted = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    let _ = fs::remove_dir_all(&ws);
    assert!(
        emitted.contains("let __vec = v;"),
        "expected owned-by-value MOVE (`let __vec = v;`) in fillVector, got:\n{emitted}"
    );
    assert!(
        !emitted.contains("let __vec = v.clone();"),
        "fillVector still clones the owned vector param (O(n²) COW regressed):\n{emitted}"
    );
}

/// Issue #890 on the emitted backend: extracting `Map.set` into a named
/// Map-returning helper must not change a linear fold from an owned move into
/// a borrowed clone. The behavioral half checks VM/Rust parity; the emitted
/// shape is the performance regression guard, because both spellings compute
/// the same value even when the borrowed one copies the whole table.
#[test]
fn rust_map_helper_result_preserves_the_owned_move() {
    let relative = "tests/fixtures/rust_map_helper_return.av";
    assert_plain_parity(relative, None).expect("helper-return Map must match the VM");

    let file = repo_root().join(relative);
    let ws = temp_dir("map_helper_return_emit");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("project dir");
    compile_rust(&file, &project, "map_helper_return", None, &[]).expect("compile rust");
    let emitted = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    let _ = fs::remove_dir_all(&ws);

    assert!(
        emitted.contains("pub fn setOne(key @ _: AverStr, mut into @ _: aver_rt::AverMap"),
        "setOne must take its Map by value after return-alias analysis:\n{emitted}"
    );
    assert!(
        emitted.contains("into.insert_owned(key, ::aver_rt::str_lit!(\"v\"))"),
        "setOne must move the owned Map into insert_owned:\n{emitted}"
    );
    assert!(
        emitted.contains("let __tco1 = setOne(head, into);"),
        "the recursive fold must move its accumulator through setOne:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn setOneResult(key @ _: AverStr, mut into @ _: aver_rt::AverMap"),
        "the Rust-only pass must graduate a Map carried by Result:\n{emitted}"
    );
    assert!(
        emitted.contains("Ok(into.insert_owned(key, ::aver_rt::str_lit!(\"v\")))"),
        "Result.Ok must wrap the owned Map successor without cloning:\n{emitted}"
    );
    assert!(
        emitted.contains("let __tco1 = setOneResult(head, into)?;"),
        "Result `?` must preserve the owned accumulator move:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn setStoreOne(key @ _: AverStr, mut store @ _: Store)"),
        "a returned Store successor must take the record by value:\n{emitted}"
    );
    assert!(
        emitted.contains("values: store.values.insert_owned(key, ::aver_rt::str_lit!(\"v\"))"),
        "the replaced Map field must move out of Store without a forced clone:\n{emitted}"
    );
    assert!(
        !emitted.contains("store.values.clone().insert_owned"),
        "the record-wrapped Map path must not force quadratic COW:\n{emitted}"
    );
    assert!(
        emitted.contains("let __tco1 = setStoreOne(head, store)?;"),
        "the Store fold must move its accumulator through the helper:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn setStoreOneCreate(key @ _: AverStr, mut store @ _: Store)"),
        "a constructor-returned Store successor must take the record by value:\n{emitted}"
    );
    assert!(
        emitted.contains(
            "Store { values: store.values.insert_owned(key, ::aver_rt::str_lit!(\"v\")) }"
        ),
        "the constructor must move the Map field out of Store without a forced clone:\n{emitted}"
    );
    assert!(
        !emitted.contains("Store { values: store.values.clone().insert_owned"),
        "the constructor-wrapped Map path must not force quadratic COW:\n{emitted}"
    );
    assert!(
        emitted.contains("let __tco1 = setStoreOneCreate(head, store);"),
        "the constructor Store fold must move its accumulator through the helper:\n{emitted}"
    );
}

/// A bare Map or Vector param that the function updates at its last use is
/// taken by value even when one caller keeps its own copy. `keep` reads its
/// map after calling `grow`, so `own_param` cannot prove every caller hands
/// over an owned map; before this, `grow` borrowed the map and every
/// `Map.set` copied the whole table. Now the loop in `fill` moves its map
/// through `grow`, and `keep` pays the one clone at its call instead of
/// `grow` paying it inside. The same holds for the self-keep `Vector.set`,
/// and a function value keeps calling the by-value item directly.
#[test]
fn rust_bare_collection_param_updated_at_last_use_is_taken_by_value() {
    let src = r#"module OwnedBareMap
    intent = "A bare Map param set at its last use, with one caller that keeps its map"
    depends []
    effects [Console.print]

fn grow(table: Map<String, Int>, key: String) -> Map<String, Int>
    ? "Insert one key."
    Map.set(table, key, 100)

fn fill(table: Map<String, Int>, n: Int) -> Map<String, Int>
    ? "Tail loop adding n keys through grow."
    match n == 0
        true -> table
        false -> fill(grow(table, "k{n}"), n - 1)

fn keep(table: Map<String, Int>) -> String
    ? "Grow a copy, then read the original."
    grown = grow(table, "extra")
    "{Map.len(grown)}/{Map.len(table)}"

fn poke(v: Vector<Int>, i: Int) -> Vector<Int>
    ? "Self-keep vector update."
    Option.withDefault(Vector.set(v, i, 7), v)

fn apply(f: Fn(Map<String, Int>, String) -> Map<String, Int>, m: Map<String, Int>) -> Map<String, Int>
    ? "Call a function value."
    f(m, "viaValue")

fn main() -> Unit
    ! [Console.print]
    built = fill({}, 1000)
    v = poke(Vector.new(3, 0), 1)
    kept = poke(v, 2)
    viaFn = apply(grow, built)
    Console.print("{Map.len(built)} {keep(built)} {Map.len(viaFn)} {Option.withDefault(Vector.get(v, 2), 9)} {Option.withDefault(Vector.get(kept, 2), 9)}")
"#;
    let expected = "1000 1001/1000 1001 0 7";
    let vm = run_vm_inline("owned_bare_map", src).expect("vm run");
    assert_eq!(vm, expected, "VM result");
    let rust = build_run_rust_inline("owned_bare_map", src).expect("rust build+run");
    assert_eq!(rust, vm, "generated Rust diverged from the VM");

    let ws = temp_dir("owned_bare_map_emit");
    let f = ws.join("owned_bare_map.av");
    fs::write(&f, src).expect("write");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("project dir");
    compile_rust(&f, &project, "owned_bare_map", None, &[]).expect("compile rust");
    let emitted = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    let _ = fs::remove_dir_all(&ws);
    assert!(
        emitted.contains("pub fn grow(mut table @ _: aver_rt::AverMap"),
        "grow must take its Map by value:\n{emitted}"
    );
    assert!(
        emitted.contains("table.insert_owned(key, ")
            && !emitted.contains("table.clone().insert_owned"),
        "grow must update the owned Map without cloning it:\n{emitted}"
    );
    assert!(
        emitted.contains("grow(table, __arg"),
        "the fill loop must move its map into grow:\n{emitted}"
    );
    assert!(
        emitted.contains("grow(table.clone(), ::aver_rt::str_lit!(\"extra\"))"),
        "keep reads its map later, so it clones once at the call:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn poke(mut v @ _: aver_rt::AverVector")
            && emitted.contains("let __vec = v;"),
        "poke must take its Vector by value and update it in place:\n{emitted}"
    );
    assert!(
        emitted.contains("apply(grow, "),
        "a function value passes the by-value item without a borrowing adapter:\n{emitted}"
    );
}

/// The generated entry module of the inline program `src`.
fn emitted_entry_module(name: &str, src: &str) -> String {
    let ws = temp_dir(&format!("{name}_emit"));
    let f = ws.join(format!("{name}.av"));
    fs::write(&f, src).expect("write");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("project dir");
    compile_rust(&f, &project, name, None, &[]).expect("compile rust");
    let emitted = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    let _ = fs::remove_dir_all(&ws);
    emitted
}

/// A record handed to a by-value param while a later argument of the same
/// call still reads it. Aver evaluates `bumpNode(s, nodeAt(s, id))` left to
/// right, so the first `s` was not the last read and Rust cloned the store;
/// the clone kept the caller's map alive and `Map.set` in `bumpNode` copied
/// the whole map on every call. The computed argument is now bound to a
/// local first and `s` moves; reading `s` has no effect, so this keeps
/// Aver's order, also when the later argument has one (`noisy`).
#[test]
fn rust_later_computed_argument_runs_before_the_moved_one() {
    let src = r#"module ArgOrder
    intent = "Thread a store through calls whose later argument reads it"
    depends []
    effects [Console.print]

record Node
    tag: Int
    hits: Int

record Store
    nodes: Map<Int, Node>
    next: Int

record SI
    s: Store
    id: Int

fn nodeAt(s: Store, id: Int) -> Node
    ? "The node at id, or a blank one."
    Option.withDefault(Map.get(s.nodes, id), Node(tag = 0, hits = 0))

fn bumpNode(s: Store, n: Node) -> SI
    ? "Store the node one hit higher under a fresh id."
    SI(s = Store(nodes = Map.set(s.nodes, s.next, Node(tag = n.tag, hits = n.hits + 1)), next = s.next + 1), id = s.next)

fn bump(s: Store, id: Int) -> SI
    ? "Bump the node at id."
    bumpNode(s, nodeAt(s, id))

fn bumpBoth(s: Store, a: Int, b: Int) -> SI
    ? "Bump two nodes into one."
    bumpTwo(s, nodeAt(s, a), nodeAt(s, b))

fn bumpTwo(s: Store, x: Node, y: Node) -> SI
    ? "Store a node with both hit counts."
    bumpNode(s, Node(tag = x.tag + y.tag, hits = x.hits + y.hits))

fn noisy(s: Store, id: Int) -> Node
    ? "nodeAt with a print."
    ! [Console.print]
    Console.print("read {id}")
    nodeAt(s, id)

fn bumpLoud(s: Store, id: Int) -> SI
    ? "Bump through an effectful read."
    ! [Console.print]
    bumpNode(s, noisy(s, id))

fn loop(s: Store, id: Int, n: Int) -> Store
    ? "Bump n times."
    match n == 0
        true -> s
        false -> loopStep(bump(s, id), n)

fn loopStep(r: SI, n: Int) -> Store
    ? "Continue from the bumped store."
    loop(r.s, r.id, n - 1)

fn seed(s: Store, n: Int) -> Store
    ? "n blank nodes."
    match n == 0
        true -> s
        false -> seed(Store(nodes = Map.set(s.nodes, s.next, Node(tag = n, hits = 0)), next = s.next + 1), n - 1)

fn main() -> Unit
    ! [Console.print]
    s = seed(Store(nodes = {}, next = 0), 200)
    out = loop(s, 0, 300)
    both = bumpBoth(out, 3, out.next - 1)
    loud = bumpLoud(both.s, both.id)
    Console.print("{Map.len(out.nodes)} {out.next} {nodeAt(out, out.next - 1).hits} {nodeAt(both.s, both.id).tag} {nodeAt(loud.s, loud.id).hits}")
"#;
    let expected = "read 500\n500 500 300 397 302";
    let vm = run_vm_inline("arg_order", src).expect("vm run");
    assert_eq!(vm, expected, "VM result");
    let rust = build_run_rust_inline("arg_order", src).expect("rust build+run");
    assert_eq!(rust, vm, "generated Rust diverged from the VM");

    let emitted = emitted_entry_module("arg_order", src);
    assert!(
        emitted.contains("pub fn bump(mut s @ _: Store")
            && emitted.contains("= nodeAt(&s, id);")
            && emitted.contains("bumpNode(s, &__arg"),
        "bump must read the node first, then move its store:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn bumpBoth(mut s @ _: Store") && emitted.contains("bumpTwo(s, "),
        "bumpBoth must read both nodes first, then move its store:\n{emitted}"
    );
    assert!(
        emitted.contains("= noisy(&s, id);") && !emitted.contains("bumpNode(s.clone()"),
        "an effectful later argument is bound first too, and the store moves:\n{emitted}"
    );
}

/// The self-keep `Option.withDefault(Vector.set(s.f, i, x), s.f)` on a field
/// of an owned record at its last use. The two reads of `s.f` hand on one
/// value, so the field moves into the in-place set instead of being cloned
/// for the set and again for the default. When the index reads the same
/// field, the field cannot move before it and is cloned once.
#[test]
fn rust_self_keep_vector_set_on_a_record_field_moves_the_field() {
    let src = r#"module FieldKeep
    intent = "Append nodes to a Vector field of a store record"
    depends []
    effects [Console.print]

record Node
    tag: Int
    a: Int

record Store
    nodes: Vector<Node>
    index: Map<Int, Int>
    next: Int

record SI
    s: Store
    id: Int

fn added(s: Store, n: Node) -> SI
    ? "Store n at the next slot and index it."
    SI(s = Store(nodes = Option.withDefault(Vector.set(s.nodes, s.next, n), s.nodes), index = Map.set(s.index, n.tag, s.next), next = s.next + 1), id = s.next)

fn lastSet(s: Store, n: Node) -> Store
    ? "Overwrite the last slot; the index reads the field too."
    Store(nodes = Option.withDefault(Vector.set(s.nodes, Vector.len(s.nodes) - 1, n), s.nodes), index = s.index, next = s.next)

fn fill(s: Store, k: Int) -> Store
    ? "Add k nodes."
    match k == 0
        true -> s
        false -> fill(added(s, Node(tag = k, a = k * 2)).s, k - 1)

fn main() -> Unit
    ! [Console.print]
    s = lastSet(fill(Store(nodes = Vector.new(300, Node(tag = 0, a = 0)), index = {}, next = 0), 310), Node(tag = 7, a = 7))
    Console.print("{Vector.len(s.nodes)} {Map.len(s.index)} {s.next} {Option.withDefault(Vector.get(s.nodes, 5), Node(tag = 0, a = 0)).a} {Option.withDefault(Vector.get(s.nodes, 299), Node(tag = 0, a = 0)).a}")
"#;
    let expected = "300 310 310 610 7";
    let vm = run_vm_inline("field_keep", src).expect("vm run");
    assert_eq!(vm, expected, "VM result");
    let rust = build_run_rust_inline("field_keep", src).expect("rust build+run");
    assert_eq!(rust, vm, "generated Rust diverged from the VM");

    let emitted = emitted_entry_module("field_keep", src);
    let added = emitted
        .lines()
        .skip_while(|line| !line.starts_with("pub fn added("))
        .take(3)
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        added.starts_with("pub fn added(mut s @ _: Store")
            && added.contains("let __vec = s.nodes;")
            && !added.contains("s.nodes.clone()"),
        "added must move the field into the in-place set:\n{emitted}"
    );
    assert!(
        emitted.contains("let __vec = s.nodes.clone();"),
        "lastSet reads the field in the index, so it clones it once:\n{emitted}"
    );
}

/// A record param that only hands one field on to a tail-call loop which
/// merely reads it stays borrowed: the field is cloned inside, and the caller
/// does not clone the whole record. A loop that stores the field still gets
/// it moved out of an owned record.
#[test]
fn rust_record_param_handing_a_field_to_a_reading_loop_stays_borrowed() {
    let src = r#"module FieldRead
    intent = "Read and extend a trie held in a state record"
    depends []
    effects [Console.print]

type Trie
    Leaf
    Node(Int, Int, Trie)

record St
    t: Trie
    seen: Map<Int, Int>
    next: Int

fn trieGet(t: Trie, k: Int) -> Int
    ? "The value at key k, or 0."
    match t
        Trie.Leaf -> 0
        Trie.Node(key, v, rest) -> match key == k
            true -> v
            false -> trieGet(rest, k)

fn trieGrow(t: Trie, n: Int) -> Trie
    ? "Prepends keys n down to 1."
    match n == 0
        true -> t
        false -> trieGrow(Trie.Node(n, n * 10, t), n - 1)

fn stGet(st: St, k: Int) -> Int
    ? "Reads through the state's trie only."
    trieGet(st.t, k)

fn stGrow(st: St, n: Int) -> Trie
    ? "Extends the state's trie."
    trieGrow(st.t, n)

fn main() -> Unit
    ! [Console.print]
    st = St(t = Trie.Leaf, seen = {}, next = 0)
    grown = St(t = stGrow(st, 5), seen = Map.set(st.seen, 1, 1), next = 1)
    Console.print("{stGet(grown, 3)} {stGet(grown, 9)} {grown.next}")
"#;
    let expected = "30 0 1";
    let vm = run_vm_inline("field_read", src).expect("vm run");
    assert_eq!(vm, expected, "VM result");
    let rust = build_run_rust_inline("field_read", src).expect("rust build+run");
    assert_eq!(rust, vm, "generated Rust diverged from the VM");

    let emitted = emitted_entry_module("field_read", src);
    assert!(
        emitted.contains("pub fn stGet(st @ _: &St")
            && emitted.contains("trieGet(&st.t,")
            && !emitted.contains("stGet(grown.clone()"),
        "stGet must borrow the record and clone only its field:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn stGrow(mut st @ _: St") && emitted.contains("trieGrow(st.t,"),
        "stGrow hands the field to a loop that stores it, so it moves:\n{emitted}"
    );
}

/// A memo Map threaded through mutually recursive functions that return it
/// inside a record, and updated in place only on a field of such a result
/// (`remember(build(t, memo), ..)` with `Map.set(r.memo, ..)`), is moved
/// along the whole cycle: no function borrows it and clones it for the next
/// call, which made every insert copy the whole map. The compiled binary
/// must remember 2^17 nodes within a minute.
#[test]
fn a_map_threaded_through_record_results_is_moved_not_cloned() {
    let relative = "tests/fixtures/map_threaded_through_results_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("map_through_results", &src);
    assert!(
        !emitted.contains("&aver_rt::AverMap") && !emitted.contains("memo.clone()"),
        "the memo must be taken by value and moved along the cycle:\n{emitted}"
    );
    assert_plain_parity_then(relative, None, |bin, _| {
        let mut child = Command::new(bin)
            .arg("17")
            .stdout(std::process::Stdio::piped())
            .spawn()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        let started = std::time::Instant::now();
        while child.try_wait().expect("binary is waitable").is_none() {
            if started.elapsed() > std::time::Duration::from_secs(60) {
                let _ = child.kill();
                return Err("2^17 inserts did not end within a minute".into());
            }
            std::thread::sleep(std::time::Duration::from_millis(20));
        }
        let out = child.wait_with_output().expect("binary output");
        let got = String::from_utf8_lossy(&out.stdout).trim().to_string();
        match got == "remembered 131071 root 1" {
            true => Ok(()),
            false => Err(format!("at 17 levels:\n{}", format_output(&out))),
        }
    })
    .unwrap_or_else(|e| panic!("{e}"));
}

/// Runs `bin arg` and returns its stdout, or an error when it does not end
/// within `secs` seconds.
fn run_within(bin: &Path, arg: &str, secs: u64) -> Result<String, String> {
    let mut child = Command::new(bin)
        .arg(arg)
        .stdout(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| format!("failed to run compiled binary: {e}"))?;
    let started = std::time::Instant::now();
    while child.try_wait().expect("binary is waitable").is_none() {
        if started.elapsed() > std::time::Duration::from_secs(secs) {
            let _ = child.kill();
            return Err(format!("`{arg}` did not end within {secs} s"));
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    let out = child.wait_with_output().expect("binary output");
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

/// A record handed by value to a function that sets a Vector in it, while a
/// later argument of the same call still reads it behind `?`
/// (`addExpr(t, i, exprOf(t, ..)?)`): the reading argument runs first, so `t`
/// moves into `addExpr` and the function takes it by value. Cloned at the
/// call, the record kept a second owner of the Vector for the whole call and
/// every set copied it. The compiled binary must write 2^17 lines into 2^17
/// slots within a minute.
#[test]
fn a_param_moved_and_read_in_one_call_is_read_first_and_moved() {
    let relative = "tests/fixtures/moved_and_read_in_one_call_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("moved_and_read_in_one_call", &src);
    assert!(
        emitted.contains("pub fn exprLine(mut t @ _: Tables,")
            && emitted.contains("addExpr(t, i, __arg")
            && !emitted.contains("addExpr(t.clone()"),
        "exprLine must read `t` first and move it into addExpr:\n{emitted}"
    );
    let file = repo_root().join(relative);
    let ws = temp_dir("moved_and_read_in_one_call");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&file, &project, "moved_and_read_in_one_call", None, &[])?;
        let bin = cargo_build(&project, "moved_and_read_in_one_call")?;
        let small = run_within(&bin, "1000", 60)?;
        let vm = Command::new(aver_bin())
            .current_dir(repo_root())
            .args(["run", relative, "--", "1000"])
            .output()
            .map_err(|e| format!("vm run: {e}"))?;
        let vm = String::from_utf8_lossy(&vm.stdout).trim().to_string();
        if small != vm {
            return Err(format!("--- VM ---\n{vm}\n--- Rust ---\n{small}"));
        }
        let got = run_within(&bin, "131072", 60)?;
        match got.starts_with("n 131072 last ") && got.ends_with("n 131073 last -") {
            true => Ok(()),
            false => Err(format!("at 2^17 lines: {got}")),
        }
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// What a match reads through a borrow is borrowed too, and copied where an
/// arm needs it owned (Pestka #53): a bucket entry `(k, v)` is cloned only on
/// a hit, a value in a map is matched in place (`m.get(&key)`), and a borrowed
/// `Option` is matched without copying it. A list handed over at its last use
/// is prepended without a count, a list read at an index is read in place,
/// and a trampoline that calls itself binds what it computes first and moves
/// the rest (Pestka #54). The compiled program must print what the VM does.
#[test]
fn borrowed_payloads_and_handed_lists_match_vm() {
    let relative = "tests/fixtures/lent_payloads_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("lent_payloads", &src);
    for expected in [
        "let (k, v) = __pat0; if (k == e) { Some(v.clone()) }",
        "match (m).get(&key) { Some(b @ _) => { findIn(b, e) }",
        "match o { Some(e @ _) => { weight(e) }",
        "aver_rt::AverList::prepend_owned(mk(i), acc)",
        "Bucket::Entries(aver_rt::AverList::prepend(entry.clone(), es))",
        "match (xs).get(aver_rt::clamp_list_count(&(k))) { Some(x) => { x.clone() }, None =>",
        "aver_rt::AverList::prepend_owned(envOf(&env, n.clone()), stack); __MutualTco1::Ev(env, __arg",
    ] {
        assert!(
            emitted.contains(expected),
            "missing `{expected}`:\n{emitted}"
        );
    }
    assert!(!emitted.contains(".get(&key).cloned()"), "{emitted}");
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));
}

/// A list a match takes apart as it dies goes through `aver_list_match!`,
/// which moves the head and the rest out of a node it alone holds, also in a
/// mutual tail-call arm; a list the function only borrows still lends its
/// head. A `String` param a function only reads in place is borrowed, so the
/// loop calling it on every character lends the string, and `String.split`
/// makes each line once. The compiled program must print what the VM does.
#[test]
fn dying_lists_and_read_strings_match_vm() {
    let relative = "tests/fixtures/dying_lists_read_strings_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("dying_lists_read_strings", &src);
    for expected in [
        "::aver_rt::aver_list_match_reuse!(stack, [] =>",
        "[c, others, __list_slot] => { { let __arg",
        "fn digitAt__indexed(s @ _: &AverStr,",
        "digitAt__indexed(&s, ",
        "fn sameAt(s @ _: &AverStr, i @ _: aver_rt::AverInt, t @ _: &AverStr)",
        "sameAt(&s, ",
        ".split('\\n').map(aver_rt::AverStr::from)",
    ] {
        assert!(
            emitted.contains(expected),
            "missing `{expected}`:\n{emitted}"
        );
    }
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));
}

/// A separator or search text that is a literal of one character, also a
/// multi-byte one, is a `char` pattern (served by `memchr`); a longer or empty
/// literal stays a `&str` pattern. Separators at the edges and in a row give
/// the same parts as on the VM.
#[test]
fn one_character_literal_patterns_match_vm() {
    let relative = "tests/fixtures/split_on_one_char_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("split_on_one_char", &src);
    for expected in [
        ".split('\\n')",
        ".split(',')",
        ".split('é')",
        ".split('🙂')",
        ".split('\\'')",
        ".split(\"ab\")",
        ".split(\"\")",
        ".contains(':')",
        ".starts_with('é')",
        ".ends_with('🙂')",
        ".contains(\"::\")",
    ] {
        assert!(
            emitted.contains(expected),
            "missing `{expected}`:\n{emitted}"
        );
    }
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));
}

/// A `List` param every caller hands over (a list just built, or the rest of
/// one taken apart) is taken by value, so a match takes it apart in place,
/// and a cons arm that builds one list onto another builds it in the node it
/// took apart. A list read again after the call, or read by another argument
/// of the same call, stays borrowed. A stack the caller keeps has shared
/// nodes, which are copied as before. The compiled program must print what
/// the VM does.
#[test]
fn handed_lists_are_taken_apart_and_reuse_their_nodes_like_vm() {
    let relative = "tests/fixtures/handed_lists_reuse_nodes_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("handed_lists_reuse_nodes", &src);
    for expected in [
        "pub fn spill(mut stack @ _: aver_rt::AverList<Clo>, mut env @ _: aver_rt::AverList<Clo>)",
        "::aver_rt::aver_list_match_reuse!(stack, [] => { return env; }, [c, rest, __list_slot] =>",
        "let __tco1 = __list_slot.prepend(c, env);",
        "pub fn keepEven(mut stack @ _: aver_rt::AverList<Clo>,",
        "[c, others, __list_slot] => { { let __arg5 @ _ = __list_slot.prepend(c, env);",
        "pub fn ids(xs @ _: &aver_rt::AverList<Clo>,",
        "pub fn sharedStack(stack @ _: &aver_rt::AverList<Clo>)",
    ] {
        assert!(
            emitted.contains(expected),
            "missing `{expected}`:\n{emitted}"
        );
    }
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));
}

/// The same with a bounce between two mutually recursive functions
/// (`step(st, t, localValue(st.lctx, n), n)`), a field read by an earlier
/// argument (`putFirst(readOnly(w.st, w.e), w.st, w.e)`), and a field read
/// by a later argument into an `Int` (`putLast(w.st, readOnly(w.st, w.e),
/// w.e)`): the read runs first and the record or the field moves. Cloned,
/// it kept a second owner of the Maps for the whole call and every write
/// copied a whole Map. The compiled binary must make 2^16 steps of each
/// within a minute.
#[test]
fn a_record_or_field_moved_and_read_in_one_call_is_read_first_and_moved() {
    let relative = "tests/fixtures/moved_and_read_shapes_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("moved_and_read_shapes", &src);
    for expected in [
        "let __arg3 @ _ = localValue(&st.lctx, n.clone()); __MutualTco1::Step(st, t, __arg3, n) }",
        "pub fn earlier(mut w @ _: W) -> St {",
        "let __arg1 @ _ = readOnly(&w.st, w.e.clone());\n    putFirst(__arg1, w.st, w.e)",
        "pub fn later(mut w @ _: W) -> St {",
        "let __arg1 @ _ = readOnly(&w.st, w.e.clone());\n    putLast(w.st, __arg1, w.e)",
    ] {
        assert!(
            emitted.contains(expected),
            "missing `{expected}`:\n{emitted}"
        );
    }
    assert!(
        !emitted.contains("Step(st.clone()") && !emitted.contains("w.st.clone()"),
        "no state may be cloned for the call:\n{emitted}"
    );
    let file = repo_root().join(relative);
    let ws = temp_dir("moved_and_read_shapes");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&file, &project, "moved_and_read_shapes", None, &[])?;
        let bin = cargo_build(&project, "moved_and_read_shapes")?;
        let small = run_within(&bin, "1000", 60)?;
        let vm = Command::new(aver_bin())
            .current_dir(repo_root())
            .args(["run", relative, "--", "1000"])
            .output()
            .map_err(|e| format!("vm run: {e}"))?;
        let vm = String::from_utf8_lossy(&vm.stdout).trim().to_string();
        if small != vm {
            return Err(format!("--- VM ---\n{vm}\n--- Rust ---\n{small}"));
        }
        let got = run_within(&bin, "65536", 60)?;
        match got == "walk 65536 earlier 65536 later 65536 at1 2" {
            true => Ok(()),
            false => Err(format!("at 2^16 steps: {got}")),
        }
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A record param whose field holding a Map goes into a record literal at
/// the param's last use (`remembered(RB(b = i == 1, st = r.st), k)?` after
/// `i = r.i`) is taken by value and the field moves into the literal.
/// Borrowed, the function cloned the field while the caller's value still
/// held the Map, and every write to it copied the whole Map. The compiled
/// binary must make 2^17 steps within a minute.
#[test]
fn a_param_field_stored_into_a_literal_moves_and_the_param_is_taken_by_value() {
    let relative = "tests/fixtures/param_field_into_literal_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("param_field_into_literal", &src);
    assert!(
        emitted.contains("pub fn bindingKept(mut r @ _: RI,")
            && emitted.contains("st: r.st }")
            && !emitted.contains("r.st.clone()"),
        "bindingKept must take `r` by value and move `r.st` into the literal:\n{emitted}"
    );
    let file = repo_root().join(relative);
    let ws = temp_dir("param_field_into_literal");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&file, &project, "param_field_into_literal", None, &[])?;
        let bin = cargo_build(&project, "param_field_into_literal")?;
        let small = run_within(&bin, "1000", 60)?;
        let vm = Command::new(aver_bin())
            .current_dir(repo_root())
            .args(["run", relative, "--", "1000"])
            .output()
            .map_err(|e| format!("vm run: {e}"))?;
        let vm = String::from_utf8_lossy(&vm.stdout).trim().to_string();
        if small != vm {
            return Err(format!("--- VM ---\n{vm}\n--- Rust ---\n{small}"));
        }
        let got = run_within(&bin, "131072", 60)?;
        match got == "i 131072 n 131072 len 131072 last 262142" {
            true => Ok(()),
            false => Err(format!("at 2^17 steps: {got}")),
        }
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A field read by an earlier part of a record or constructor is copied
/// there, so a later read of the same field in the same literal is its last
/// and moves (`SR(e = r.e, memo = Map.set(r.memo, key, (e, r.e)))`, a memo's
/// write). The earlier part must keep the value it copied: a Map copied into
/// `before` and set in `after` keeps its old entries.
#[test]
fn a_field_copied_by_an_earlier_part_moves_at_its_later_read() {
    let relative = "tests/fixtures/record_part_copy_then_move_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("record_part_copy_then_move", &src);
    for expected in [
        "SR { e: r.e.clone(), memo: r.memo.insert_owned(key, (e, r.e)) }",
        "Two { before: t.after.clone(), after: t.after.insert_owned(k.clone(), k) }",
        "Pair::Kept(t.after.clone(), t.after.insert_owned(k.clone(), k))",
        "MX { e: x.e.clone(), ms: MS { forced: x.ms.forced.insert_owned(id, x.e), ..x.ms } }",
        "return Done { run: run.clone(), at: run.pos };",
    ] {
        assert!(
            emitted.contains(expected),
            "missing `{expected}`:\n{emitted}"
        );
    }
    let file = repo_root().join(relative);
    let ws = temp_dir("record_part_copy_then_move");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&file, &project, "record_part_copy_then_move", None, &[])?;
        let bin = cargo_build(&project, "record_part_copy_then_move")?;
        let rust = run_within(&bin, "300", 60)?;
        let vm = Command::new(aver_bin())
            .current_dir(repo_root())
            .args(["run", relative, "--", "300"])
            .output()
            .map_err(|e| format!("vm run: {e}"))?;
        let vm = String::from_utf8_lossy(&vm.stdout).trim().to_string();
        match rust == vm && vm.contains("split before 299 after 300 has false true") {
            true => Ok(()),
            false => Err(format!("--- VM ---\n{vm}\n--- Rust ---\n{rust}")),
        }
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A match subject that looks a key up in a Map handed out of the state
/// (`found(Map.get(cacheFor(st, true), k), k)`) is bound by a `let` before
/// the `match`. Matched in place, the Map's clone was a temporary of the
/// scrutinee and lived through the arms, so the write in the `None` arm
/// found the Map shared and copied all of it. The same lookup as an argument
/// before the state is handed on (`rememberMiss(found(..), st, k)`) is bound
/// first as well, and the self-tail-recursive `fill` stays a loop. The
/// compiled binary must write 2^17 keys each way within a minute.
#[test]
fn a_match_subject_drops_its_temporaries_before_the_arms() {
    let relative = "tests/fixtures/scrutinee_temporary_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("scrutinee_temporary", &src);
    assert!(
        emitted
            .contains("let __subject = { let __arg2 @ _ = cacheFor(&st, true).get(&k).cloned();")
            && emitted.contains("match __subject {")
            && emitted.contains("rememberMiss(&__arg3, st, k)")
            && emitted.contains("loop {"),
        "the subject and the argument must be bound first, fill must loop:\n{emitted}"
    );
    let file = repo_root().join(relative);
    let ws = temp_dir("scrutinee_temporary");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| {
        compile_rust(&file, &project, "scrutinee_temporary", None, &[])?;
        let bin = cargo_build(&project, "scrutinee_temporary")?;
        let small = run_within(&bin, "1000", 60)?;
        let vm = Command::new(aver_bin())
            .current_dir(repo_root())
            .args(["run", relative, "--", "1000"])
            .output()
            .map_err(|e| format!("vm run: {e}"))?;
        let vm = String::from_utf8_lossy(&vm.stdout).trim().to_string();
        if small != vm {
            return Err(format!("--- VM ---\n{vm}\n--- Rust ---\n{small}"));
        }
        let got = run_within(&bin, "131072", 60)?;
        match got == "hot 131071 cold 1 arg 131071" {
            true => Ok(()),
            false => Err(format!("at 2^17 keys: {got}")),
        }
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A local Vector named again only as the fallback of its own `Vector.set`
/// (`Option.withDefault(Vector.set(w, i, x), w)`, or `w` in the `None` arm
/// of a match on the set) moves into the set: the set fails only out of
/// range, where nothing changed, and then hands the Vector back. Cloned
/// first, the Vector had two owners and every write copied all of it. The
/// compiled binary must make 10^6 writes within a minute.
#[test]
fn a_local_named_again_as_its_own_set_fallback_moves_into_the_set() {
    let relative = "tests/fixtures/vector_set_fallback_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("vector_set_fallback", &src);
    assert!(
        !emitted.contains("w.clone()"),
        "the local must move into its set:\n{emitted}"
    );
    assert_plain_parity_then(relative, None, |bin, _| {
        let got = run_within(bin, "1000000", 60)?;
        match got == "n 1000000 last 999999 out 1000000" {
            true => Ok(()),
            false => Err(format!("at 10^6 writes: {got}")),
        }
    })
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A tail-recursive walk that passes a record on unchanged and hands it, where
/// it stops, to a function that sets a Vector in it, keeps the record as an
/// owned local of the loop and moves it out on that exit. It used to wrap the
/// record in a `Shared` and return `(*c).clone()`, so the Vector had two
/// owners and every append copied it. The compiled binary must make 2^20
/// appends to a Vector of 2^20 slots within a minute.
#[test]
fn a_loop_moves_a_param_it_passes_on_out_on_exit() {
    let relative = "tests/fixtures/loop_hands_on_its_param_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("loop_hands_on_its_param", &src);
    assert!(
        emitted.contains("return appended(c, key);")
            && !emitted.contains("(*c).clone()")
            && !emitted.contains("Shared::new(c)"),
        "the walk must move its record out on exit:\n{emitted}"
    );
    assert_plain_parity_then(relative, None, |bin, _| {
        let got = run_within(bin, "1048576", 60)?;
        match got == "n 1048576 last 1048575 again 1048576" {
            true => Ok(()),
            false => Err(format!("at 2^20 appends: {got}")),
        }
    })
    .unwrap_or_else(|e| panic!("{e}"));
}

/// A record whose fields take more than 128 bytes is one pointer to them
/// (`pub type St = Box<St__Fields>`), so handing it on copies the pointer.
/// An update of its last use writes into its own box: where it is when an
/// owned parameter holds it (also after a field was read out of it, and
/// with two fields swapped), through a binding of its own when a local
/// holds it, and into a new box only when a field moved out of that local
/// first or a value reads the local whole. A copy made beside it is a box of
/// its own. The VM and the binary, also with replay, print the same.
#[test]
fn a_large_record_is_boxed_and_updated_in_its_box() {
    let relative = "tests/fixtures/boxed_state_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("boxed_state_app", &src);
    for shape in [
        "pub type St = Box<St__Fields>;",
        "pub type Big = Box<Big__Fields>;",
        "s.a = __field0; s.seen = __field1; s }",
        "{ let __field0 = s.b; let __field1 = s.a; s.a = __field0; s.b = __field1; s }",
        "{ let mut __record0 = r.st; __record0.c = __record0.c.add(&r.v); __record0 }",
        "b.st = __field0; b.x = __field1; b }",
        "{ let mut __record0 = s.clone(); __record0.e = ",
    ] {
        assert!(emitted.contains(shape), "missing `{shape}`:\n{emitted}");
    }
    assert_eq!(emitted.matches("..*r.st })").count(), 2, "{emitted}");
    assert!(
        !emitted.contains("pub type Res"),
        "a small record stays inline"
    );
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));

    let file = repo_root().join(relative);
    let vm_stdout = run_vm(&file, None).unwrap_or_else(|e| panic!("{e}"));
    let project = temp_dir("boxed_state_replay").join("project");
    fs::create_dir_all(&project).expect("create project dir");
    compile_rust(
        &file,
        &project,
        "boxed_state_replay",
        None,
        &["--with-replay"],
    )
    .unwrap_or_else(|e| panic!("{e}"));
    let bin = cargo_build(&project, "boxed_state_replay").unwrap_or_else(|e| panic!("{e}"));
    let out = Command::new(&bin).output().expect("run replay binary");
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), vm_stdout);
}

/// A tail-recursive loop borrows the params it only reads: a tree it
/// descends, a bucket or list it walks, a key it passes on. Its callers then
/// lend their values too, so `known(st, ..)`, which only hands a field of
/// `st` to such a loop, borrows `st` instead of every caller cloning the
/// whole record. A borrowed list is matched through its borrow, and a part
/// of a constructor or the head of a list that is only read is borrowed from
/// it. A loop that returns its accumulator keeps taking it by value.
#[test]
fn a_loop_borrows_the_params_it_only_reads() {
    let relative = "tests/fixtures/read_only_loop_params_app.av";
    let src = fs::read_to_string(repo_root().join(relative)).expect("read fixture");
    let emitted = emitted_entry_module("read_only_loop_params", &src);
    for signature in [
        "fn getAt(t @ _: &Tree,",
        "fn findIn(bucket @ _: &aver_rt::AverList<",
        "fn known(st @ _: &St,",
        "fn sameAs(names @ _: &aver_rt::AverList<AverStr>, mut name @ _: AverStr)",
        "fn keep(names @ _: &aver_rt::AverList<AverStr>, mut acc @ _: aver_rt::AverList<AverStr>)",
    ] {
        assert!(
            emitted.contains(signature),
            "missing `{signature}`:\n{emitted}"
        );
    }
    assert!(
        !emitted.contains("st.clone()") && !emitted.contains("names.clone()"),
        "a record or list only read must not be cloned:\n{emitted}"
    );
    // The parts of a tree or a list a function only reads are borrowed from
    // it, not cloned out.
    assert!(
        emitted.contains("let l = &") && emitted.contains("let r = &"),
        "the branches `leaves` only reads must be borrowed:\n{emitted}"
    );
    assert!(
        emitted.contains("(Some(t), Some(rest)) = (__list_subject.first(), __list_subject.tail())"),
        "the head `allLeaves` only reads must be borrowed:\n{emitted}"
    );
    // A head one branch hands on is borrowed too, and cloned only there:
    // `findBefore` compares a field of every head and copies the one it
    // finds, once.
    assert!(
        emitted.contains("(Some(p), Some(rest)) = (__list_subject.first(), __list_subject.tail()) { if (p.name == n) { return placedBefore(p.clone(), upTo); }"),
        "the head `findBefore` hands on in one branch must be borrowed:\n{emitted}"
    );
    // A borrowed head, or a field of one, pushed into a list being built is
    // cloned into it.
    for pushed in [
        "aver_rt::list_builder_push(acc, p.clone())",
        "aver_rt::list_builder_push(acc, p.c.clone())",
    ] {
        assert!(emitted.contains(pushed), "missing `{pushed}`:\n{emitted}");
    }
    assert_plain_parity(relative, None).unwrap_or_else(|e| panic!("{e}"));
}

/// A record holding a Map keyed by a tuple of more than three items: the
/// generated record display needs the tuple key to display too.
#[test]
fn rust_map_keyed_by_a_wide_tuple_in_a_record_compiles() {
    let src = r#"module TupleKey
    intent = "A record holding Maps keyed by four- and five-item tuples"
    depends []
    effects [Console.print]

record Store
    index: Map<Tuple<Int, Int, Int, Int>, Int>
    names: Map<Tuple<Int, String, Bool, Int, Int>, String>
    next: Int

fn keyOf(a: Int, b: Int, c: Int, d: Int) -> Tuple<Int, Int, Int, Int>
    ? "The index key of one node."
    (a, b, c, d)

fn main() -> Unit
    ! [Console.print]
    s = Store(index = Map.set(Map.set({}, keyOf(1, 2, 3, 4), 0), keyOf(0, 9, 9, 9), 1), names = Map.set({}, (1, "a", true, 2, 5), "x"), next = 2)
    name = Option.withDefault(Map.get(s.names, (1, "a", true, 2, 5)), "-")
    Console.print("{Option.withDefault(Map.get(s.index, (1, 2, 3, 4)), 9)} {Option.withDefault(Map.get(s.index, keyOf(0, 9, 9, 9)), 9)} {Map.len(s.index)} {name} {s.next}")
"#;
    let expected = "0 1 2 x 2";
    let vm = run_vm_inline("tuple_key", src).expect("vm run");
    assert_eq!(vm, expected, "VM result");
    let rust = build_run_rust_inline("tuple_key", src).expect("rust build+run");
    assert_eq!(rust, vm, "generated Rust diverged from the VM");
}

/// Issue #1196 end to end: a Map owned by a mutual-TCO trampoline crosses a
/// module boundary into a normal helper, returns inside `Result<Tuple<...>>`,
/// and is destructured before the next trampoline state. The pattern-bound
/// successor is an owned Rust value; losing that fact makes `changed` borrow
/// the Map and its first `insert_owned` copy the complete table every round.
#[test]
fn rust_mutual_tco_moves_last_use_map_into_cross_module_helper() {
    let ws = temp_dir("mutual_tco_cross_module_map");
    let worker = ws.join("worker.av");
    let entry = ws.join("main.av");
    fs::write(
        &worker,
        r#"module Worker
    intent = "change an owned map and return it through a consumed aggregate"
    depends []
    exposes [changed, forked, inserted]
    effects []

fn changed(into: Map<String, Int>) -> Result<Tuple<Map<String, Int>, Int>, String>
    ? "return the map successor in the aggregate consumed by the dispatcher"
    Result.Ok((Map.set(into, "key", 1), 1))

fn forked(into: Map<String, Int>) -> Tuple<Map<String, Int>, Map<String, Int>>
    ? "return two COW handles so the Rust-only ownership fact is tested safely"
    (into, into)

fn inserted(into: Map<String, Int>, key: String) -> Map<String, Int>
    ? "mutate one extracted handle while its sibling remains observable"
    Map.set(into, key, 1)
"#,
    )
    .expect("write worker module");
    fs::write(
        &entry,
        r#"module Entry
    intent = "thread a map through mutual TCO and a cross-module helper"
    depends [Worker]
    effects [Console.print]

fn absorbing(left: Int, into: Map<String, Int>) -> Result<Map<String, Int>, String>
    ? "hand the last-use map to Worker.changed before tail-calling continued"
    match Worker.changed(into)
        Result.Err(why) -> Result.Err(why)
        Result.Ok(parts) -> match parts
            (next, _) -> continued(left, next)

fn continued(left: Int, into: Map<String, Int>) -> Result<Map<String, Int>, String>
    ? "mutually tail-call absorbing until the requested rounds are complete"
    match left
        0 -> Result.Ok(into)
        _ -> absorbing(left - 1, into)

fn renderedFork(pair: Tuple<Map<String, Int>, Map<String, Int>>) -> String
    ? "prove that moving a pattern local preserves a still-live COW sibling"
    match pair
        (moving, kept) -> "{Map.len(Worker.inserted(moving, "other"))}/{Map.len(kept)}"

fn main() -> Unit
    ! [Console.print]
    built = Result.withDefault(absorbing(3, {}), {})
    Console.print(renderedFork(Worker.forked(built)))
"#,
    )
    .expect("write entry module");

    let vm = run_vm(&entry, Some(&ws)).expect("VM run");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("project dir");
    let name = "mutual_tco_cross_module_map";
    compile_rust(&entry, &project, name, Some(&ws), &[]).expect("compile Rust project");

    let entry_rust = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
        .expect("read generated entry module");
    let worker_rust = fs::read_to_string(project.join("src/aver_generated/worker/mod.rs"))
        .expect("read generated worker module");
    assert!(
        worker_rust.contains("pub fn changed(mut into @ _: aver_rt::AverMap"),
        "the cross-module helper must take its Map by value:\n{worker_rust}"
    );
    assert!(
        entry_rust.contains("worker::changed(into)"),
        "the trampoline must move its last-use Map across the module call:\n{entry_rust}"
    );
    assert!(
        !entry_rust.contains("worker::changed(&into)"),
        "the old borrowed call recreates an Rc owner and forces full-table COW:\n{entry_rust}"
    );
    assert!(
        worker_rust.contains("into.insert_owned"),
        "the owned helper parameter must flow directly into Map.set:\n{worker_rust}"
    );
    assert!(
        entry_rust.contains("worker::inserted(moving"),
        "a last-use pattern local must move into an owned helper parameter:\n{entry_rust}"
    );

    let bin = cargo_build(&project, name).expect("build generated project");
    let output = Command::new(bin).output().expect("run generated binary");
    assert!(
        output.status.success(),
        "generated binary failed:\n{}",
        format_output(&output)
    );
    let rust = String::from_utf8_lossy(&output.stdout).trim().to_string();
    let _ = fs::remove_dir_all(&ws);
    assert_eq!(vm, "2/1", "VM result and retained COW sibling");
    assert_eq!(rust, vm, "generated Rust diverged from the VM");
}

// ─── fn-result alias summaries preserve caller aliases ─────────────────
//
// A named function result may grant ownership only through the complete
// return-alias summary: every parameter whose backing it can return must have
// received a uniquely-owned argument. A first-class fn value has no static
// summary and remains unknown. Both shapes below feed a call result STRAIGHT
// into another call's argument while the caller keeps its own handle and
// reads it afterwards. The named `keepFirst` summary is `{a, b}`, and `a` is
// not at last use; granting ownership would mutate the caller's collection.
//
// The VM-vs-`expected` assert is the only one that can go red for this
// class, and it is checked FIRST: the VM is where a wrong grant corrupts
// the caller's collection, so under the bug that assert panics and the
// parity assert below it is never reached. The compiled Rust keeps the
// collection's copy-on-write protection and still prints the right answer,
// so the parity assert carries no independent signal here — it is the
// harness's standing shape, not this class's net. What these tests add
// over the in-process pins is the end-to-end path: the same program
// actually compiled to a Rust project, built, and run.
//
// Cheap pins for the same class, no `cargo build` involved:
//   - `own_param_graduation::named_fn_call_result_argument_keeps_the_param_flagged`
//   - `own_param_graduation::fn_value_call_result_argument_keeps_the_param_flagged`
//   - `own_param_soundness::named_fn_result_argument_is_not_mutated_in_place`
//   - `own_param_soundness::fn_value_result_argument_is_not_mutated_in_place`
// Both halves of the arm therefore have a structural pin AND a VM-only
// behavioural witness; the `MirCallee::LocalSlot` half (a first-class fn
// value) has no build test on top of those, deliberately.

/// The `Map` half, and the one shape of this class that stays in the
/// default tier. `keepFirst` hands back one of its two argument maps, and
/// that result is `growth`'s argument with no binding in between; `run`
/// keeps `base` live and prints it afterwards, so an ownership grant on the
/// call result shows up as `base` losing its entries.
#[test]
fn rust_fn_result_argument_keeps_the_callers_map_intact() {
    let src = r#"module OwnedFnResultMap
    intent = "A helper's Map result flows straight into another call while the caller keeps its own map"
    depends []
    effects [Console.print]

fn keepFirst(a: Map<String, Int>, b: Map<String, Int>) -> Map<String, Int>
    ? "Returns one of its arguments, so the result shares a caller value."
    match Map.len(a) > 0
        true -> a
        false -> b

fn growth(m: Map<String, Int>, n: Int) -> Int
    ? "Threads the map linearly, then reports its size."
    match n == 0
        true -> Map.len(m)
        false -> growth(Map.set(m, "g{n}", n), n - 1)

fn render(m: Map<String, Int>) -> String
    ? "Size plus the value under a."
    "{Map.len(m)}/{Option.withDefault(Map.get(m, "a"), 0 - 1)}"

fn run() -> String
    ? "Pass a call result straight into another call, then read the original."
    base = Map.set(Map.set({}, "a", 7), "b", 8)
    grown = growth(keepFirst(base, {}), 4)
    "{grown} {render(base)}"

fn main() -> Unit
    ! [Console.print]
    Console.print(run())
"#;
    // 4 keys added to a 2-key map, and `base` still holds its own 2 entries.
    let expected = "6 2/7";
    let vm = run_vm_inline("own_fn_result_map", src).expect("vm run");
    let rust = build_run_rust_inline("own_fn_result_map", src).expect("rust build+run");
    assert_eq!(
        vm, expected,
        "the map the caller kept was mutated through a call result"
    );
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — a call result was treated as a uniquely-owned map"
    );
}

/// Nested-tuple Int-literal match on the RUST backend. After the
/// `Int -> AverInt` migration an `AverInt` can't be a Rust `match` pattern,
/// so Int-literal arms lower to an if/else-if equality-guard chain. That
/// lowering originally handled only one level of tuple nesting and bailed on
/// a NESTED tuple, emitting `compile_error!("MIR walker could not render …")`
/// — which builds fine on the VM but fails `cargo build`. This drives a match
/// with a deeply-nested tuple pattern (literal leaves at depth, plus a binding
/// and a wildcard) end-to-end: compile to Rust, `cargo build`, RUN, and assert
/// stdout equals the VM. `aver compile` exits 0 even when it emits the
/// `compile_error!` stub, so only the real build+run catches the regression.
#[test]
fn rust_nested_tuple_int_literal_match_builds_and_matches_vm() {
    let src = r#"module NestedTupleIntMatch
    intent = "Nested-tuple Int-literal match must lower to recursive equality guards"
    depends []
    effects [Console.print]

fn pick(t: Tuple<Int, Tuple<Int, Tuple<Int, Int>>, Int>) -> Int
    ? "match Int literals nested two tuples deep, plus a binding arm"
    match t
        (1, (2, (3, 4)), 5) -> 99
        (1, (x, _), _) -> x
        _ -> 0

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(pick((1, (2, (3, 4)), 5))))
    Console.print(String.fromInt(pick((1, (42, (7, 8)), 9))))
    Console.print(String.fromInt(pick((5, (2, (3, 4)), 5))))
"#;
    // First arm matches (→ 99); second arm binds the nested `x` (→ 42);
    // neither literal arm matches the third call (→ 0).
    let vm = run_vm_inline("nestedtupleint", src).expect("vm run");
    let rust = build_run_rust_inline("nestedtupleint", src).expect("rust build+run");
    assert_eq!(vm, "99\n42\n0", "VM nested-tuple Int match values");
    assert_eq!(
        rust, vm,
        "Rust nested-tuple Int-literal match diverged from VM (or emitted a compile_error! stub)"
    );
}

/// A parameter handed straight back, once per parameter type. Params of a
/// collection / sum / product / tuple type are emitted as `&T` while the
/// return type stays by value, so the fn's tail has to materialise an owned
/// value — the same `.clone()` a match arm or a call argument already gets.
/// The tail was the one owning position that never asked for it, so
/// `passthrough(x) = x` emitted `&T` where `T` was expected and the generated
/// project would not build at all (`Result`, `Option`, a sum type, a record, a
/// tuple, a `List` and a provider resource all failed; `String`, `Int`,
/// `Float`, `Bool` and the
/// `own_param`-graduated `Vector` / `Map` built because they are already
/// owned). Every shape sits in one program, so a backend that materialises too
/// little fails the build and one that materialises the wrong value diverges
/// from the VM.
#[test]
fn rust_passthrough_param_of_every_type_builds_and_matches_vm() {
    let src = r#"module Passthrough
    intent = "One passthrough fn per parameter type, so every borrow shape is exercised"
    depends []
    effects [Console.print]

type Colour
    Red
    Green(Int)

record Point
    x: Int
    y: Int

fn passResult(v: Result<Int, String>) -> Result<Int, String>
    ? "Hand back exactly what was given."
    v

fn passOption(v: Option<Int>) -> Option<Int>
    ? "Hand back exactly what was given."
    v

fn passColour(v: Colour) -> Colour
    ? "Hand back exactly what was given."
    v

fn passPoint(v: Point) -> Point
    ? "Hand back exactly what was given."
    v

fn passTuple(v: Tuple<Int, String>) -> Tuple<Int, String>
    ? "Hand back exactly what was given."
    v

fn passList(v: List<Int>) -> List<Int>
    ? "Hand back exactly what was given."
    v

fn passVector(v: Vector<Int>) -> Vector<Int>
    ? "Hand back exactly what was given."
    v

fn passMap(v: Map<String, Int>) -> Map<String, Int>
    ? "Hand back exactly what was given."
    v

fn passString(v: String) -> String
    ? "Hand back exactly what was given."
    v

fn passInt(v: Int) -> Int
    ? "Hand back exactly what was given."
    v

fn passFloat(v: Float) -> Float
    ? "Hand back exactly what was given."
    v

fn passBool(v: Bool) -> Bool
    ? "Hand back exactly what was given."
    v

fn passConnection(v: Tcp.Connection) -> Tcp.Connection
    ? "Clone the opaque provider handle when an owned resource is returned."
    v

fn keepOwnedAndAlias(items: List<Int>) -> Int
    ? "A named alias must not move an owned local that is read again."
    reversed = List.reverse(items)
    alias = reversed
    List.len(reversed) + List.len(alias)

fn showResult(v: Result<Int, String>) -> String
    ? "Render it."
    match v
        Result.Ok(n) -> String.fromInt(n)
        Result.Err(e) -> e

verify showResult
    showResult(Result.Ok(1)) => "1"
    showResult(Result.Err("e")) => "e"

fn showOption(v: Option<Int>) -> String
    ? "Render it."
    match v
        Option.Some(n) -> String.fromInt(n)
        Option.None -> "none"

verify showOption
    showOption(Option.Some(2)) => "2"
    showOption(Option.None) => "none"

fn showColour(v: Colour) -> String
    ? "Render it."
    match v
        Colour.Red -> "red"
        Colour.Green(n) -> "green {n}"

verify showColour
    showColour(Colour.Red) => "red"
    showColour(Colour.Green(3)) => "green 3"

fn showTuple(v: Tuple<Int, String>) -> String
    ? "Render it."
    match v
        (n, s) -> "{n}{s}"

verify showTuple
    showTuple((6, "t")) => "6t"

fn main() -> Unit
    ? "Print every passthrough so a divergence shows as a diff, not a pass."
    ! [Console.print]
    Console.print(showResult(passResult(Result.Ok(1))))
    Console.print(showOption(passOption(Option.Some(2))))
    Console.print(showColour(passColour(Colour.Green(3))))
    Console.print(String.fromInt(passPoint(Point(x = 4, y = 5)).y))
    Console.print(showTuple(passTuple((6, "t"))))
    Console.print(String.fromInt(List.len(passList([7, 8]))))
    Console.print(String.fromInt(Vector.len(passVector(Vector.new(3, 0)))))
    Console.print(String.fromInt(Map.len(passMap({"a" => 9}))))
    Console.print(passString("s"))
    Console.print(String.fromInt(passInt(10)))
    Console.print("{passFloat(1.5)}")
    Console.print("{passBool(true)}")
    Console.print(String.fromInt(keepOwnedAndAlias([1, 2])))
"#;
    let expected = "1\n2\ngreen 3\n5\n6t\n2\n3\n1\ns\n10\n1.5\ntrue\n4";
    let vm = run_vm_inline("passthrough_param", src).expect("vm run");
    let rust =
        build_run_rust_inline("passthrough_param", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM passthrough contract changed");
    assert_eq!(
        rust, vm,
        "Rust passthrough of a borrowed param diverged from the VM"
    );
}

/// A parameter that reaches the return slot through something other than
/// being the whole body. Three positions in one program, because the fix for
/// one of them does not cover the other two:
///
/// - the last line of a statement chain (`closeAfter`, the cleanup-wrapper
///   shape from the issue, and `yAfter`, the field read through a borrowed
///   param on that same line);
/// - a NAME the parameter was bound to (`kept = outcome` then `kept`, and
///   `y = p.y` then `y`) — giving the value a name does not change what the
///   function may do with it, but the binding used to be rendered raw, so
///   `kept` was a `&Result<…>` and `y` was a move out of a shared reference;
/// - a field read more than one level deep (`s.piece.kind`), in the tail and
///   bound to a name and as the last line after an effect — `s.piece` is
///   behind the same shared reference `s` is, so a rule that only looks one
///   level down leaves every nested read raw.
///
/// The build is the primary detector for this class — the issue is a project
/// that does not compile — and the stdout comparison against the VM is what
/// catches materialising the WRONG value once it does compile.
///
/// `tagged` is a different guard and exercises none of the tail/binding code:
/// the caller keeps using a map it passed to a callee that adds a key, and the
/// two stay separate because `Map.set` emits `m.clone().insert_owned(…)` — the
/// argument itself is passed as a plain `&original` borrow. It is kept because
/// that separation IS invisible to the build (a callee writing into the
/// caller's map compiles fine) and only shows up as a divergence from the VM,
/// so it is the one shape here that the stdout comparison, not rustc, decides.
///
/// `shifted` is a PARITY WITNESS, not a guard, and the caption says so
/// because the honest accounting matters more than the extra shape: it
/// NAMES the borrowed param and then reads fields through the name, but it
/// cannot go red either way. Pre-fix the emitter wrote `let kept = p;`,
/// which binds `&Point`, and `kept.x` / `kept.y` autoderef through the
/// shared reference — measured: that project builds and prints `10 11`
/// unchanged. And no emission can make `origin` differ, because
/// `Point(x = …, y = …)` allocates a fresh record and Aver never writes
/// through the `&T`. It is here to show the new binding path carries a
/// record read through a name end-to-end without disturbing the caller.
///
/// The shapes that actually go red are the other seven. Against the base
/// commit this program fails to build with seven rustc errors: `closeAfter`
/// and `keptOutcome` E0308 (`&Result<…>` where `Result<…>` is expected),
/// `yAfter` and `keptY` E0507 (move out of `p.y`), `nestedTail`,
/// `nestedNamed` and `nestedAfter` E0507 (move out of `s.piece.kind`).
/// Three of those seven — `keptOutcome`, `keptY`, `nestedNamed` — are the
/// binding position specifically: reverting only the binding call sites,
/// with the tail rule left in place, still fails with exactly those three.
#[test]
fn rust_param_returned_via_a_name_or_a_nested_field_builds_and_matches_vm() {
    let src = r#"module CloseAfter
    intent = "A parameter handed back through a name, a nested field or a statement chain"
    depends []
    effects [Console.print]

record Kind
    label: String

record Piece
    kind: Kind
    size: Int

record Slot
    piece: Piece
    tag: Int

record Point
    x: Int
    y: Int

fn closeAfter(label: String, outcome: Result<Int, String>) -> Result<Int, String>
    ? "Always print the label, whatever the session did."
    ! [Console.print]
    _printed = Console.print(label)
    outcome

fn yAfter(label: String, p: Point) -> Int
    ? "Print the label, then hand back a field of the point."
    ! [Console.print]
    _printed = Console.print(label)
    p.y

fn keptOutcome(outcome: Result<Int, String>) -> Result<Int, String>
    ? "Bind the parameter to a name, then hand the name back."
    kept = outcome
    kept

fn keptY(p: Point) -> Int
    ? "Bind a field read to a name, then hand the name back."
    y = p.y
    y

fn nestedTail(s: Slot) -> Kind
    ? "Hand back a field two levels down."
    s.piece.kind

fn nestedNamed(s: Slot) -> Kind
    ? "Bind a field two levels down to a name, then hand the name back."
    k = s.piece.kind
    k

fn nestedAfter(label: String, s: Slot) -> Kind
    ? "Print the label, then hand back a field two levels down."
    ! [Console.print]
    _printed = Console.print(label)
    s.piece.kind

fn shifted(p: Point) -> Point
    ? "Name the point, then build a changed copy from the name."
    kept = p
    Point(x = kept.x + 1, y = kept.y)

fn tagged(m: Map<String, Int>) -> Map<String, Int>
    ? "Add a key and hand the map back."
    Map.set(m, "b", 2)

fn shown(v: Result<Int, String>) -> String
    ? "Render it."
    match v
        Result.Ok(n) -> String.fromInt(n)
        Result.Err(e) -> e

verify shown
    shown(Result.Ok(1)) => "1"
    shown(Result.Err("e")) => "e"

fn slot() -> Slot
    ? "One nested record to read through."
    Slot(piece = Piece(kind = Kind(label = "axe"), size = 3), tag = 7)

verify slot
    slot().tag => 7

fn main() -> Unit
    ? "Print each shape so a divergence shows as a diff, not a pass."
    ! [Console.print]
    Console.print(shown(closeAfter("closing", Result.Ok(1))))
    Console.print(String.fromInt(yAfter("point", Point(x = 4, y = 5))))
    Console.print(shown(keptOutcome(Result.Ok(2))))
    Console.print(String.fromInt(keptY(Point(x = 4, y = 6))))
    Console.print(nestedTail(slot()).label)
    Console.print(nestedNamed(slot()).label)
    Console.print(nestedAfter("nested", slot()).label)
    origin = Point(x = 10, y = 20)
    moved = shifted(origin)
    Console.print("{origin.x} {moved.x}")
    original = {"a" => 1}
    grown = tagged(original)
    Console.print(String.fromInt(Map.len(original)))
    Console.print(String.fromInt(Map.len(grown)))
"#;
    // `origin` is unchanged by `shifted`, and `original` still has one key
    // after `tagged` added one to its own copy.
    let expected = "closing\n1\npoint\n5\n2\n6\naxe\naxe\nnested\naxe\n10 11\n1\n2";
    let vm = run_vm_inline("close_after", src).expect("vm run");
    let rust = build_run_rust_inline("close_after", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM contract for these shapes changed");
    assert_eq!(
        rust, vm,
        "Rust diverged from the VM on a parameter returned through a name or a nested field"
    );
}

// ─── Mode (b): deny-policy ──────────────────────────────────────────────

/// A Disk-write program. `__PATH__` is substituted with the real
/// write target at test time. Routed through a helper fn so the
/// effect rides a normal cross-fn call (the same shape the policy
/// wrapper guards).
const DISK_WRITE_PROBE: &str = r#"module DiskProbe
    intent =
        "Writes one file then prints DONE. Probes the policy wrapper:"
        "under a deny policy the write must be rejected at runtime."
    effects [Console, Disk]

fn writeIt(path: String) -> Result<Unit, String>
    ? "Writes a fixed payload to the given path."
    ! [Disk.writeText]
    Disk.writeText(path, "payload")

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.writeText]
    written = writeIt("__PATH__")?
    shown = Console.print("DONE")
    Result.Ok(Unit)
"#;

fn write_runtime_disk_policy(dir: &Path, allowed_path: &str) {
    fs::create_dir_all(dir).expect("create policy dir");
    fs::write(
        dir.join("aver.toml"),
        format!("[effects.Disk]\npaths = [{allowed_path:?}]\n"),
    )
    .expect("write aver.toml");
}

fn write_embedded_disk_policy(dir: &Path, allowed_path: &str) {
    fs::create_dir_all(dir).expect("create policy dir");
    fs::write(
        dir.join("aver.toml"),
        format!("[effects.Disk]\npaths = [{allowed_path:?}]\n"),
    )
    .expect("write aver.toml");
}

#[test]
fn deny_policy_rejects_denied_disk_write_at_runtime() {
    let ws = temp_dir("deny");
    let out_path = ws.join("out.txt");
    let src = ws.join("disk_probe.av");
    fs::write(
        &src,
        DISK_WRITE_PROBE.replace("__PATH__", &aver_path_literal(&out_path)),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "deny_disk_probe";

    let result = (|| -> Result<(), String> {
        // `--policy runtime` loads aver.toml at run time from
        // AVER_REPLAY_MODULE_ROOT, so one built binary serves both
        // the deny and the allow probe.
        compile_rust(&src, &project, name, None, &["--policy", "runtime"])?;
        let bin = cargo_build(&project, name)?;

        // (1) DENY: allow-list names a DIFFERENT path → the write to
        // out.txt is denied. Binary must exit non-zero and NOT create
        // the file.
        let deny_root = ws.join("deny-policy");
        write_runtime_disk_policy(&deny_root, "/aver/nonexistent/allowed/only");
        let denied = Command::new(&bin)
            .env("AVER_REPLAY_MODULE_ROOT", &deny_root)
            .output()
            .map_err(|e| format!("run denied binary: {e}"))?;
        if denied.status.success() {
            return Err(format!(
                "deny-policy run unexpectedly SUCCEEDED — the policy wrapper \
                 was not enforced:\n{}",
                format_output(&denied)
            ));
        }
        let denied_stderr = String::from_utf8_lossy(&denied.stderr);
        if !denied_stderr.contains("denied by aver.toml policy") {
            return Err(format!(
                "deny-policy run failed for the wrong reason (expected a \
                 policy violation):\n{}",
                format_output(&denied)
            ));
        }
        if out_path.exists() {
            return Err(format!(
                "deny-policy run wrote the file at {} despite the deny policy — \
                 the policy check ran AFTER the effect (or not at all)",
                out_path.display()
            ));
        }

        // (2) ALLOW: allow-list names the real write path → the write
        // is permitted. Proves the deny in (1) was the policy, not an
        // unconditional failure.
        let allow_root = ws.join("allow-policy");
        write_runtime_disk_policy(&allow_root, &out_path.to_string_lossy());
        let allowed = Command::new(&bin)
            .env("AVER_REPLAY_MODULE_ROOT", &allow_root)
            .output()
            .map_err(|e| format!("run allowed binary: {e}"))?;
        if !allowed.status.success() {
            return Err(format!(
                "allow-policy run failed — the probe should succeed when the \
                 write path is permitted:\n{}",
                format_output(&allowed)
            ));
        }
        if !out_path.exists() {
            return Err(format!(
                "allow-policy run did not write {} — the effect was suppressed \
                 even though the policy allowed it",
                out_path.display()
            ));
        }
        let allowed_stdout = String::from_utf8_lossy(&allowed.stdout);
        if !allowed_stdout.contains("DONE") {
            return Err(format!(
                "allow-policy run did not print DONE:\n{}",
                format_output(&allowed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

fn aver_path_literal(path: &Path) -> String {
    // Aver string literal — escape backslashes and quotes.
    path.to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
}

// ─── Mode (b'): the policy-checked argument is evaluated once ───────────
//
// The policy check and the effect call are two use sites of the SAME
// argument. Emitting the argument expression at both of them evaluated
// it twice: any non-`Copy` binding it named was moved by the check and
// then used again by the call (the generated project failed to build
// with E0382), and any effect it carried ran twice (the built binary
// diverged from the VM). Both halves ride one program here: `pathOf`
// takes two non-`Copy` parameters AND announces itself, and it feeds a
// one-argument `Disk.readText` and a two-argument `Disk.writeText`.

/// `__DIR__` is substituted with a real directory at test time.
const POLICY_ARG_PROBE: &str = r#"module PolicyArgProbe
    intent =
        "Writes then reads a file whose path is built from parameters,"
        "announcing the path once per call. Probes the policy-checked"
        "argument: it must be evaluated exactly once."
    effects [Console, Disk]

fn pathOf(dir: String, n: Int) -> String
    ? "Says which file it means, then names it."
    ! [Console.print]
    said = Console.print("naming")
    "{dir}/f{n}.txt"

fn writeAt(dir: String, n: Int, body: String) -> Result<Unit, String>
    ? "Writes to a path built from two parameters."
    ! [Console.print, Disk.writeText]
    Disk.writeText(pathOf(dir, n), body)

fn readAt(dir: String, n: Int) -> Result<String, String>
    ? "Reads a path built from two parameters."
    ! [Console.print, Disk.readText]
    Disk.readText(pathOf(dir, n))

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readText, Disk.writeText]
    written = writeAt("__DIR__", 1, "payload")?
    text = readAt("__DIR__", 1)?
    shown = Console.print(text)
    Result.Ok(Unit)
"#;

/// How many times the emitted module mentions `pathOf` — one definition
/// plus one mention per call site. A policy check that re-emits the
/// argument expression pushes this up by one per guarded call.
fn path_of_mentions(emitted: &str) -> usize {
    emitted.matches("pathOf(").count()
}

fn write_policy_arg_probe(dir: &Path, target_dir: &Path) -> PathBuf {
    fs::create_dir_all(dir).expect("create source dir");
    let src = dir.join("policy_arg_probe.av");
    fs::write(
        &src,
        POLICY_ARG_PROBE.replace("__DIR__", &aver_path_literal(target_dir)),
    )
    .expect("write probe source");
    src
}

fn generated_entry(project: &Path) -> Result<String, String> {
    fs::read_to_string(
        project
            .join("src")
            .join("aver_generated")
            .join("entry")
            .join("mod.rs"),
    )
    .map_err(|e| format!("read emitted module: {e}"))
}

#[test]
fn policy_checked_disk_argument_builds_and_matches_vm() {
    let ws = temp_dir("policy-arg");
    // The embedded aver.toml is read from the module root at compile
    // time, so the source + the policy share a dir.
    let proj_root = ws.join("src-root");
    let files = ws.join("files");
    fs::create_dir_all(&files).expect("create target dir");
    let src = write_policy_arg_probe(&proj_root, &files);
    // A real allow-list naming the write/read directory: the check has
    // to see the same path the call uses, or the run is denied.
    write_embedded_disk_policy(&proj_root, &files.to_string_lossy());

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "policy_arg_probe";

    let result = (|| -> Result<(), String> {
        // No `--policy` flag: the default (embed) is what a plain
        // `aver compile` does, and the presence of aver.toml alone is
        // what turns the check on.
        compile_rust(&src, &project, name, Some(&proj_root), &[])?;
        let emitted = generated_entry(&project)?;
        if !emitted.contains("aver_policy::check_disk") {
            return Err(format!(
                "emitted Rust is missing the `aver_policy::check_disk` wrapper — \
                 the probe would be testing nothing:\n{emitted}"
            ));
        }
        let mentions = path_of_mentions(&emitted);
        if mentions != 3 {
            return Err(format!(
                "`pathOf` is mentioned {mentions} times (expected 3: one \
                 definition + one per call site) — the policy check is \
                 re-emitting the argument expression:\n{emitted}"
            ));
        }

        // The real gate: rustc rejects the second evaluation (E0382 on
        // the moved parameters), and the run shows how often the
        // argument's own effect fired.
        let bin = cargo_build(&project, name)?;
        let run = Command::new(&bin)
            .output()
            .map_err(|e| format!("run policy-arg binary: {e}"))?;
        if !run.status.success() {
            return Err(format!("policy-arg run failed:\n{}", format_output(&run)));
        }
        let rust_stdout = String::from_utf8_lossy(&run.stdout).trim().to_string();
        let vm_stdout = run_vm(&src, Some(&proj_root))?;
        if rust_stdout != vm_stdout {
            return Err(format!(
                "Rust diverged from the VM: the argument expression's effect \
                 ran a different number of times\nvm:\n{vm_stdout}\nrust:\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
fn policy_check_rides_any_aver_toml_and_leaves_the_unguarded_shape_alone() {
    let ws = temp_dir("policy-arg-shape");
    let files = ws.join("files");
    fs::create_dir_all(&files).expect("create target dir");

    let result = (|| -> Result<(), String> {
        // (1) An aver.toml that says nothing about Disk. The check is
        // emitted anyway — the trigger is the file's existence — so the
        // argument still has two use sites and must still be rendered
        // once.
        let quiet_root = ws.join("quiet-root");
        let quiet_src = write_policy_arg_probe(&quiet_root, &files);
        fs::write(
            quiet_root.join("aver.toml"),
            "[[check.suppress]]\nslug = \"verify-coverage\"\nfiles = [\"nothing.av\"]\nreason = \"placeholder\"\n",
        )
        .expect("write aver.toml");
        let quiet_project = ws.join("quiet-project");
        compile_rust(
            &quiet_src,
            &quiet_project,
            "policy_arg_quiet",
            Some(&quiet_root),
            &[],
        )?;
        let quiet = generated_entry(&quiet_project)?;
        if !quiet.contains("aver_policy::check_disk") {
            return Err(format!(
                "an aver.toml with no Disk section emitted no check — the \
                 trigger for this emission changed:\n{quiet}"
            ));
        }
        let mentions = path_of_mentions(&quiet);
        if mentions != 3 {
            return Err(format!(
                "`pathOf` is mentioned {mentions} times under a policy-free \
                 aver.toml (expected 3) — the check is re-emitting the \
                 argument expression:\n{quiet}"
            ));
        }

        // (2) Control: no aver.toml, no check, and the call keeps the
        // shape it always had.
        let bare_root = ws.join("bare-root");
        let bare_src = write_policy_arg_probe(&bare_root, &files);
        let bare_project = ws.join("bare-project");
        compile_rust(
            &bare_src,
            &bare_project,
            "policy_arg_bare",
            Some(&bare_root),
            &[],
        )?;
        let bare = generated_entry(&bare_project)?;
        if bare.contains("aver_policy") {
            return Err(format!(
                "no aver.toml, but a policy check was emitted:\n{bare}"
            ));
        }
        let bare_mentions = path_of_mentions(&bare);
        if bare_mentions != 3 {
            return Err(format!(
                "`pathOf` is mentioned {bare_mentions} times with no policy \
                 at all (expected 3) — the unguarded emission changed:\n{bare}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (b''): the policy-checked argument keeps its ownership ────────
//
// Binding the checked argument to a local fixed the double evaluation but
// made that local an OWNING position, which the argument had never been
// asked to satisfy: every guarded raw body reads argument 0 as `&{}`, so
// before the temp existed both use sites merely borrowed. Two argument
// shapes are moved by the bind and are not moved by a borrow:
//
//   * a local that the program reads again after the guarded call — the
//     temp takes it, and the later read is `error[E0382]: borrow of moved
//     value`;
//   * a field read through a record parameter — record params are emitted
//     as `&T`, so the temp is `error[E0507]: cannot move out of *place`,
//     which needs no second read at all.
//
// `POLICY_ARG_PROBE` cannot see either one: its argument is `pathOf(dir, n)`,
// a call result that already owns itself, and the same is true of every
// other guarded call site in the corpus. Both shapes ride one program here,
// and the `cargo build` is the gate — the emission assertions above it only
// say which of the two went wrong.

/// `__DIR__` is substituted with a real directory at test time.
const POLICY_ARG_OWNERSHIP_PROBE: &str = r#"module PolicyArgOwnership
    intent =
        "Names a path once and uses it twice, and reads a path a record"
        "carries. Probes the policy-checked argument's ownership: binding"
        "it must not take the value away from the rest of the program."
    effects [Console, Disk]

record Location
    dir: String
    segment: String

fn writeThenCheck(dir: String, n: Int) -> Result<Unit, String>
    ? "Writes a file, checks it, then prints the path it used."
    ! [Console.print, Disk.exists, Disk.writeText]
    path = "{dir}/f{n}.txt"
    written = Disk.writeText(path, "payload")?
    present = Disk.exists(path)
    shown = Console.print("exists {path}: {present}")
    Result.Ok(Unit)

fn readThere(place: Location) -> Result<String, String>
    ? "Reads the file a record field names."
    ! [Disk.readText]
    Disk.readText(place.segment)

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.exists, Disk.readText, Disk.writeText]
    checked = writeThenCheck("__DIR__", 1)?
    place = Location(dir = "__DIR__", segment = "__DIR__/f1.txt")
    text = readThere(place)?
    echoed = Console.print("read {text}")
    Result.Ok(Unit)
"#;

#[test]
fn policy_checked_argument_keeps_its_ownership() {
    let ws = temp_dir("policy-arg-own");
    let proj_root = ws.join("src-root");
    let files = ws.join("files");
    fs::create_dir_all(&files).expect("create target dir");
    fs::create_dir_all(&proj_root).expect("create src root");
    let src = proj_root.join("policy_arg_ownership.av");
    fs::write(
        &src,
        POLICY_ARG_OWNERSHIP_PROBE.replace("__DIR__", &aver_path_literal(&files)),
    )
    .expect("write probe source");
    write_embedded_disk_policy(&proj_root, &files.to_string_lossy());

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "policy_arg_ownership";

    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, Some(&proj_root), &[])?;
        let emitted = generated_entry(&project)?;
        if !emitted.contains("aver_policy::check_disk") {
            return Err(format!(
                "emitted Rust is missing the `aver_policy::check_disk` wrapper — \
                 the probe would be testing nothing:\n{emitted}"
            ));
        }
        // A local the program reads again: the temp has to own a copy,
        // not take the local. Disk is provider-bound now, so the guarded
        // argument is the capability call's own `__provider_arg0` temp —
        // same clone-before-bind shape the builtin seam's `__policy_arg`
        // always had.
        if !emitted.contains("let __provider_arg0: AverStr = path.clone();") {
            return Err(format!(
                "the provider arg temp takes `path` instead of owning a copy of it — \
                 every later read of `path` is E0382:\n{emitted}"
            ));
        }
        // A field read through a `&Location` param: the temp has to own a
        // copy, not move out of the borrow.
        if !emitted.contains("let __provider_arg0: AverStr = place.segment.clone();") {
            return Err(format!(
                "the provider arg temp moves `place.segment` out of a shared \
                 reference — E0507 on the first and only use:\n{emitted}"
            ));
        }

        // The gate: rustc is the judge of both shapes.
        let bin = cargo_build(&project, name)?;
        let run = Command::new(&bin)
            .output()
            .map_err(|e| format!("run policy-arg-ownership binary: {e}"))?;
        if !run.status.success() {
            return Err(format!(
                "policy-arg-ownership run failed:\n{}",
                format_output(&run)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&run.stdout).trim().to_string();
        let vm_stdout = run_vm(&src, Some(&proj_root))?;
        if rust_stdout != vm_stdout {
            return Err(format!(
                "Rust diverged from the VM\nvm:\n{vm_stdout}\nrust:\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (c): record / replay ──────────────────────────────────────────

/// Reads a file, then echoes its contents via Console.print. The read
/// result must be woven into the print arg, so the recorded
/// `Console.print` arg-json proves the `Disk.readText` result flowed
/// through. `__PATH__` is substituted at test time.
const READ_ECHO_PROBE: &str = r#"module RwProbe
    intent =
        "Reads a file and echoes its contents. The record captures the read"
        "result; replay serves it back. Probes the replay wrapper."
    effects [Console, Disk]

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readText]
    content = Disk.readText("__PATH__")?
    shown = Console.print("READ:{content}")
    Result.Ok(Unit)
"#;

#[test]
fn record_replay_roundtrips_effects_through_invoke_wrapper() {
    let ws = temp_dir("replay");
    let data_path = ws.join("data.txt");
    fs::write(&data_path, "recorded-bytes").expect("write probe data");
    let src = ws.join("rw_probe.av");
    fs::write(
        &src,
        READ_ECHO_PROBE.replace("__PATH__", &aver_path_literal(&data_path)),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "rw_probe";

    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, None, &["--with-replay"])?;
        let bin = cargo_build(&project, name)?;

        // (1) RECORD: run live, capturing the effects into a session.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        let recorded_stdout = String::from_utf8_lossy(&recorded.stdout);
        if !recorded_stdout.contains("READ:recorded-bytes") {
            return Err(format!(
                "record run did not echo the read bytes (live read broken):\n{}",
                format_output(&recorded)
            ));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }

        // The session must capture BOTH effects through invoke_effect.
        // A dropped replay wrapper makes one (or both) vanish.
        let session_json = fs::read_to_string(&session).expect("read session");
        // Disk.readText recorded with its result.
        if !session_json.contains("\"Disk.readText\"") {
            return Err(format!(
                "session is missing the Disk.readText effect — the replay \
                 wrapper was dropped on the read:\n{session_json}"
            ));
        }
        // Console.print recorded — its arg-json proves the read result
        // flowed through into the printed string (per-effect arg shape).
        if !session_json.contains("\"Console.print\"") {
            return Err(format!(
                "session is missing the Console.print effect — the replay \
                 wrapper was dropped on the print:\n{session_json}"
            ));
        }
        if !session_json.contains("READ:recorded-bytes") {
            return Err(format!(
                "session does not carry the woven read result in the \
                 Console.print arg — per-effect arg-json shape is wrong:\n{session_json}"
            ));
        }

        // (2) REPLAY: mutate the data file so a LIVE read would differ,
        // then replay. Replay must serve the recorded bytes from the
        // session (not re-read the mutated file) and roundtrip the
        // recorded effects without a position mismatch.
        fs::write(&data_path, "MUTATED-ON-DISK").expect("mutate data file");
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded session did not roundtrip:\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
fn normal_capability_calls_skip_replay_argument_serialization() {
    let ws = temp_dir("lazy-replay-args");
    let source = ws.join("lazy_args.av");
    fs::write(
        &source,
        "module LazyArgs\n    intent = \"Replay snapshots are optional work.\"\n\nfn main() -> Unit\n    ! [Console.print]\n    Console.print(\"payload\")\n",
    )
    .unwrap();
    let project = ws.join("project");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "lazy_args", None, &["--with-replay"])?;
        // Instrument the actual generated string codec: checking output or
        // emitted syntax alone would miss an eagerly built, discarded snapshot.
        let runtime = project.join("src/replay_support.rs");
        let emitted = fs::read_to_string(&runtime).unwrap();
        let needle = "impl ReplayValue for aver_rt::AverStr {\n        fn to_replay_json(&self) -> ReplayJson {";
        assert!(emitted.contains(needle));
        fs::write(&runtime, emitted.replacen(needle, &format!(
            "{needle}\n            assert!(std::env::var_os(\"AVER_TEST_FORBID_SNAPSHOT\").is_none(), \"unnecessary argument snapshot\");"
        ), 1)).unwrap();
        let binary = cargo_build(&project, "lazy_args")?;
        let normal = Command::new(&binary)
            .env("AVER_TEST_FORBID_SNAPSHOT", "1")
            .env_remove("AVER_REPLAY_RECORD")
            .env_remove("AVER_REPLAY_REPLAY")
            .output()
            .unwrap();
        if !normal.status.success() {
            return Err(format!(
                "normal run serialized arguments: {}",
                format_output(&normal)
            ));
        }
        assert_eq!(String::from_utf8_lossy(&normal.stdout).trim(), "payload");
        let session = ws.join("session.json");
        let record = Command::new(&binary)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .unwrap();
        if !record.status.success() {
            return Err(format!("record failed: {}", format_output(&record)));
        }
        let data: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(&session).unwrap()).unwrap();
        let print = data["effects"]
            .as_array()
            .unwrap()
            .iter()
            .find(|effect| effect["type"] == "Console.print")
            .unwrap();
        assert_eq!(print["args"], serde_json::json!(["payload"]));
        let replay = Command::new(&binary)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .unwrap();
        if !replay.status.success() {
            return Err(format!("replay failed: {}", format_output(&replay)));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

// ─── Mode (e): MIR-synthesized TCO (Wave 5) ──────────────────────────────
//
// The MIR walker synthesizes the self-TCO loop and the mutual-recursion
// trampoline from `MirExpr::TailCall` (the sole codegen path since the
// HIR walker was deleted). TCO is verified BEHAVIORALLY here (there is no
// byte-parity gate): build + RUN the MIR-synthesized binary and
// assert (1) stdout parity with the VM AND (2) the deep self-recursion
// case does NOT stack-overflow — which proves the emitted code is a
// genuine loop, not a recursive call that merely happens to compute the
// right answer at shallow depth.
//
// ## Revert-proof (what makes this a real net, not theater)
//
// Break the synthesized self-loop in
// `src/codegen/rust/from_mir.rs::emit_mir_self_tco_continue` by replacing
// the `continue;` line with a recursive self-call that returns instead of
// looping, e.g.:
//
// ```rust
// // lines.push("            continue;".to_string());
// lines.push(format!(
//     "            return {}({});",
//     "countUp",
//     arg_strs.join(", ")
// ));
// ```
//
// The shallow mutual case still passes (correct answer), but the DEEP
// self-recursion case (10M) overflows the stack → the binary aborts with
// a non-zero exit → `mir_tco_deep_self_and_mutual_recursion_behaves`
// fails on the "compiled binary exited non-zero" branch. Restoring
// `continue;` turns it green again. (Verified during Wave 5 development:
// the test goes RED with the loop broken, GREEN with it intact.)

/// Deep self-TCO (sum to 10M — overflows WITHOUT a real loop) + a
/// 2-cycle and a 3-cycle of mutual recursion, all in one program so a
/// single cargo build amortizes. Console-only, deterministic.
const MIR_TCO_PROBE: &str = r#"module TcoProbe
    intent =
        "Deep self-TCO that would stack-overflow without a real loop, plus"
        "mutual recursion (a 2-cycle and a 3-cycle). Probes the Wave-5 MIR"
        "TCO synthesis: the deep case proves the loop, the cycles the trampoline."
    effects [Console]

fn countUp(n: Int, acc: Int) -> Int
    ? "Tail-recursive sum from n down to 0 — deep enough to overflow without TCO."
    match n == 0
        true -> acc
        false -> countUp(n - 1, acc + n)

fn isEven(n: Int) -> Bool
    ? "True when n is even (mutual recursion with isOdd)."
    match n == 0
        true -> true
        false -> isOdd(n - 1)

fn isOdd(n: Int) -> Bool
    ? "True when n is odd (mutual recursion with isEven)."
    match n == 0
        true -> false
        false -> isEven(n - 1)

fn cycleA(n: Int) -> Int
    ? "Three-cycle member A."
    match n == 0
        true -> 100
        false -> cycleB(n - 1)

fn cycleB(n: Int) -> Int
    ? "Three-cycle member B."
    match n == 0
        true -> 200
        false -> cycleC(n - 1)

fn cycleC(n: Int) -> Int
    ? "Three-cycle member C."
    match n == 0
        true -> 300
        false -> cycleA(n - 1)

fn main() -> Unit
    ! [Console.print]
    total = countUp(10000000, 0)
    e = isEven(5000000)
    o = isOdd(5000000)
    c = cycleA(7)
    Console.print("total={total} even={e} odd={o} cycle={c}")
"#;

#[test]
fn mir_tco_deep_self_and_mutual_recursion_behaves() {
    let ws = temp_dir("mir_tco");
    let src = ws.join("tco_probe.av");
    fs::write(&src, MIR_TCO_PROBE).expect("write TCO probe source");

    // The VM oracle: a deep tail-recursive loop the VM runs via frame
    // reuse, so it produces the answer without overflowing.
    let vm_stdout = run_vm(&src, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_tco_probe";

    let result = (|| -> Result<(), String> {
        // MIR is the sole codegen path: the self-TCO loop + mutual-rec
        // trampoline are synthesized from `MirExpr::TailCall` by the MIR
        // walker (`emit_mir_tco_fn` / `emit_mir_mutual_tco_block`).
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            // A non-zero exit on the DEEP case is the stack overflow the
            // loop is supposed to prevent — the revert-proof failure mode.
            return Err(format!(
                "MIR-TCO binary exited non-zero — the deep self-recursion likely \
                 stack-overflowed (the synthesized loop is broken):\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR-TCO stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (MIR TCO) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (f): MIR-emitted IndependentProduct (`?!` / `!`) ────────────────
//
// The MIR walker emits `MirExpr::IndependentProduct` — the Rust
// backend's truly-PARALLEL product (`std::thread::scope` + per-branch
// `spawn`, the cancel-flag machinery for `?!`, the bare tuple fold for
// `!`) — as the sole codegen path. This probe proves the MIR walker
// OWNS the parallel emission and the built binary still produces
// VM-identical output.
//
// `independent_fanout.av` exercises `?!` (flatOk, processStep — recursive
// fan-out), bare `!` (flatFail, bareProduct), and the `Err`-propagation
// path, so a dropped cancel flag, a wrong tuple fold, or a dropped
// unwrap would change stdout here.

#[test]
fn mir_forced_independent_product_builds_and_matches_vm() {
    let relative = "examples/core/independent_fanout.av";
    let file = repo_root().join(relative);
    if !file.exists() {
        panic!("{relative}: corpus file missing");
    }

    let vm_stdout = run_vm(&file, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let ws = temp_dir("mir-ip");
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_ip_fanout";

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the IndependentProduct fns
        // must lower to MIR. Without this guard an empty program would
        // let the probe pass for the wrong reason.
        let lowered = mir_lowered_count(&file, None, &[])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the IndependentProduct emit is not being \
                 exercised by the MIR walker"
                    .to_string(),
            );
        }

        // The parallel IndependentProduct shape is MIR-emitted (the sole
        // codegen path), then build + run.
        compile_rust(&file, &project, name, None, &[])?;

        // Structural tripwire: the emitted Rust must carry the MIR-
        // emitted parallel product machinery (the cancel-flag branch
        // runner for `?!` and the `thread::scope` fan-out).
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("run_cancelable_branch") || !emitted.contains("std::thread::scope") {
            return Err(format!(
                "emitted Rust is missing the parallel IndependentProduct machinery \
                 (run_cancelable_branch / thread::scope) — the MIR product emit was \
                 dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "MIR IndependentProduct binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR IndependentProduct stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1019: both closures in a recursive `?!` read the
/// same non-Copy context. The old emitter cloned call arguments in the
/// first branch but moved their last uses into the second branch; the
/// first `move` closure had already captured the originals, so rustc
/// rejected the second closure with E0382.
///
/// Rust-on-MIR W6/Stage-3 retired the HIR Rust emitter. The successor
/// parity gates used here are therefore (a) `mir_lowered > 0`, proving
/// this shape went through MIR, and (b) built-Rust output equal to the
/// VM oracle that preserves the former HIR/MIR semantic golden.
#[test]
fn recursive_independent_product_owns_each_shared_capture() {
    let relative = "tests/fixtures/rust_independent_shared_capture.av";
    let file = repo_root().join(relative);
    if !file.exists() {
        panic!("{relative}: regression fixture missing");
    }

    let vm_stdout = run_vm(&file, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));
    let ws = temp_dir("shared-ip-capture");

    let result = (|| -> Result<(), String> {
        let lowered = mir_lowered_count(&file, None, &[])?;
        if lowered == 0 {
            return Err("recursive independent product did not reach the MIR emitter".to_string());
        }

        for (mode, extra) in [("plain", &[][..]), ("replay", &["--with-replay"][..])] {
            let project = ws.join(mode);
            fs::create_dir_all(&project).expect("create project dir");
            let name = format!("shared_ip_capture_{mode}");
            compile_rust(&file, &project, &name, None, extra)?;
            let emitted = fs::read_to_string(
                project
                    .join("src")
                    .join("aver_generated")
                    .join("entry")
                    .join("mod.rs"),
            )
            .map_err(|e| format!("read emitted module: {e}"))?;
            if !emitted.contains("std::thread::scope") {
                return Err(format!(
                    "{mode}: recursive independent product lost scoped-thread emission:\n{emitted}"
                ));
            }
            // Each branch binds its own capture before spawn: the first clones,
            // the second holds the final read and moves the original.
            for capture in ["height", "blockId"] {
                let binding = format!("let {capture} = {capture}");
                if emitted.matches(&binding).count() < 2 {
                    return Err(format!(
                        "{mode}: every branch must own `{capture}` before spawn:\n{emitted}"
                    ));
                }
            }
            if mode == "replay" && !emitted.contains("capture_parallel_scope_context") {
                return Err(format!(
                    "replay: shared-capture path lost parallel replay scope:\n{emitted}"
                ));
            }

            let bin = cargo_build(&project, &name)?;
            let out = Command::new(&bin)
                .output()
                .map_err(|e| format!("failed to run {mode} binary: {e}"))?;
            if !out.status.success() {
                return Err(format!(
                    "{mode}: shared-capture binary exited non-zero:\n{}",
                    format_output(&out)
                ));
            }
            let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
            if rust_stdout != vm_stdout {
                return Err(format!(
                    "{mode}: shared-capture output mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
                ));
            }
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A branch of `(a, b)!` takes a local it reads for the last time by move,
/// the way a call at its last use does. Cloning it left the original alive
/// in the enclosing frame for the whole branch, so the first `Map.set` in the
/// branch copied the whole map. A local read after the product is still
/// cloned, and a local read by both branches is cloned for the first and
/// moved into the second.
#[test]
fn independent_product_moves_a_branch_argument_at_its_last_use() {
    let ws = temp_dir("ip_last_use_move");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module ProductLastUse
    intent = "Independent-product branches take the locals they read for the last time."
    effects [Console]

record Tables
    entries: Map<String, Int>

fn build(n: Int) -> Map<String, Int>
    ? "A map with n entries."
    match n == 0
        true -> {}
        false -> Map.set(build(n - 1), "k{n}", n)

fn grow(tables: Tables, key: String) -> Tables
    ? "The tables with one more entry."
    Tables.update(tables, entries = Map.set(tables.entries, key, 100))

fn size(tables: Tables) -> Int
    ? "Count the entries."
    Map.len(tables.entries)

fn movedAtLastUse(n: Int) -> Int
    ? "Both branch arguments are read for the last time here."
    table = Tables(entries = build(n))
    other = Tables(entries = build(2))
    match (grow(table, "z"), size(other))!
        (a, b) -> size(a) * 10 + b

fn readAfter(n: Int) -> Int
    ? "The tables are read again after the product."
    kept = Tables(entries = build(n))
    match (grow(kept, "z"), size(Tables(entries = build(1))))!
        (a, b) -> size(a) * 100 + b * 10 + size(kept)

fn readByBoth(n: Int) -> Int
    ? "Both branches read the same tables."
    shared = Tables(entries = build(n))
    match (grow(shared, "x"), grow(shared, "k1"))!
        (a, b) -> size(a) * 10 + size(b)

fn main() -> Unit
    ! [Console.print]
    Console.print("moved: {movedAtLastUse(3)}")
    Console.print("after: {readAfter(3)}")
    Console.print("both: {readByBoth(3)}")
"#,
    )
    .expect("write last-use product source");

    let vm_stdout = run_vm(&source, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");

    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "ip_last_use_move", None, &[])?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if emitted.matches("std::thread::scope").count() < 3 {
            return Err(format!(
                "probe did not emit the three parallel products:\n{emitted}"
            ));
        }
        let count = |needle: &str| emitted.matches(needle).count();
        let expectations = [
            ("let table = table.clone();", 0),
            ("let table = table;", 1),
            // The branch hands the moved tables on, so `Map.set` updates
            // them in place.
            ("grow(table, ", 1),
            ("let other = other.clone();", 0),
            ("let kept = kept.clone();", 1),
            ("let kept = kept;", 0),
            ("let shared = shared.clone();", 1),
            ("let shared = shared;", 1),
        ];
        for (needle, expected) in expectations {
            if count(needle) != expected {
                return Err(format!(
                    "expected `{needle}` {expected} time(s), found {}:\n{emitted}",
                    count(needle)
                ));
            }
        }

        let bin = cargo_build(&project, "ip_last_use_move")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run last-use product binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "last-use product binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "last-use product output mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1191: a mutual-TCO invariant is represented as `&T` in
/// the trampoline. Capturing it for a `?!` branch must copy that reference;
/// calling `.clone()` at the capture boundary clones the enum referent into
/// an owned `T`, while call emission still treats the local as the invariant
/// wrapper and produces `&*network` (E0614).
#[test]
fn mutual_tco_enum_invariant_builds_inside_independent_product() {
    let ws = temp_dir("mutual_tco_ip_enum_capture");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Repro
    intent = "A mutual-TCO invariant is captured by two independent-product branches."
    effects [Console]

type Network
    Mainnet
    Signet

fn observed(network: Network) -> Result<Int, String>
    ? "Turn the selected network into a small result."
    match network
        Network.Mainnet -> Result.Ok(1)
        Network.Signet -> Result.Ok(2)

fn first(network: Network, remaining: Int) -> Result<Int, String>
    ? "Stop at zero or hand the invariant network to the partner."
    match remaining == 0
        true -> Result.Ok(0)
        false -> second(network, remaining)

fn second(network: Network, remaining: Int) -> Result<Int, String>
    ? "Read the invariant in both branches before returning to first."
    both = (observed(network), observed(network))?!
    match both
        (left, right) -> first(network, remaining - left - right)

fn main() -> Unit
    ! [Console.print]
    match first(Network.Mainnet, 4)
        Result.Ok(value) -> Console.print("{value}")
        Result.Err(error) -> Console.print(error)
"#,
    )
    .expect("write #1191 regression source");

    let vm_stdout = run_vm(&source, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");

    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "mutual_tco_ip_enum_capture", None, &[])?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("enum __MutualTco")
            || !emitted.contains("fn __mutual_tco_trampoline_")
            || !emitted.contains("std::thread::scope")
        {
            return Err(format!(
                "probe did not exercise mutual TCO plus independent-product emission:\n{emitted}"
            ));
        }
        if emitted.matches("let network = network;").count() < 2
            || emitted.contains("let network = network.clone();")
        {
            return Err(format!(
                "mutual-TCO &Network was not copied into every branch:\n{emitted}"
            ));
        }

        // Load-bearing gate: before the fix rustc reports two E0614 errors.
        let bin = cargo_build(&project, "mutual_tco_ip_enum_capture")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run #1191 regression binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "#1191 regression binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "#1191 output mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Mode (g): MIR-emitted first-class fn values ──────────────────────────
//
// W6/Stage-0 (the final construct gap): the MIR walker now emits
// `MirExpr::FnValue` (a named fn used as a value — `applyIt(dbl, 21)`
// passes `dbl`) and `MirCallee::LocalSlot` (calling a fn-typed param —
// `f(v)` inside `applyIt`). Post-#379 a fn value can only enter through a
// `Fn(..)` param, so the emitted Rust is a plain fn-pointer
// (`fn(i64)->i64`) — no closure / `dyn Fn`, no monomorphization. Rust CAN
// execute these (unlike wasm-gc, which traps), so this is an EMIT path,
// not a trap-stub, and must produce VM-identical output.
//
// The corpus is thin on higher-order (only `pairSpec` in
// `examples/formal/oracle_independent_products.av`, verify-only), so this
// is an inline RUNTIME probe: `dbl` / `inc` are passed into `applyIt`
// (FnValue in arg position), `applyIt` calls them through its slot
// (LocalSlot). The MIR walker (the sole codegen path) emits the
// FnValue / LocalSlot fns, so the built binary's `a=42 b=42` proves the
// MIR walker OWNS the first-class-fn emission (a dropped FnValue arg or a
// mis-emitted slot call would change stdout or fail to build).
const MIR_HIGHER_ORDER_PROBE: &str = r#"module HigherOrderProbe
    intent =
        "Probes first-class fn values: a fn passed as a Fn(..) param value"
        "(MirExpr::FnValue) and called through that slot (MirCallee::LocalSlot)."
    effects [Console]

fn dbl(x: Int) -> Int
    ? "Double a number."
    x * 2

fn inc(x: Int) -> Int
    ? "Increment a number."
    x + 1

fn applyIt(f: Fn(Int) -> Int, v: Int) -> Int
    ? "Apply a first-class fn value to v (calls through the slot — LocalSlot)."
    f(v)

fn runDouble(v: Int) -> Int
    ? "Pass dbl as a fn value into applyIt (FnValue in arg position)."
    applyIt(dbl, v)

fn runInc(v: Int) -> Int
    ? "Pass inc as a fn value into applyIt (FnValue in arg position)."
    applyIt(inc, v)

fn main() -> Unit
    ! [Console.print]
    a = runDouble(21)
    b = runInc(41)
    Console.print("a={a} b={b}")
"#;

#[test]
fn mir_first_class_fn_value_builds_and_matches_vm() {
    let ws = temp_dir("mir_ho");
    let src = ws.join("higher_order_probe.av");
    fs::write(&src, MIR_HIGHER_ORDER_PROBE).expect("write higher-order probe source");

    let vm_stdout = run_vm(&src, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_ho_probe";

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: the FnValue / LocalSlot fns
        // (`applyIt`, `runDouble`, `runInc`) must lower to MIR. Without
        // this guard an empty program would let the probe pass for the
        // wrong reason (the FnValue / LocalSlot emit would never run).
        // `dbl` / `inc` / the three higher-order fns + `main` all lower —
        // so ≥ 5 must lower.
        let lowered = mir_lowered_count(&src, None, &[])?;
        if lowered < 5 {
            return Err(format!(
                "expected ≥ 5 fns to lower to MIR (dbl, inc, applyIt, runDouble, \
                 runInc) — got {lowered}. The FnValue / LocalSlot emit is not being \
                 exercised by the MIR walker."
            ));
        }

        // The FnValue arg + LocalSlot call are MIR-emitted (the sole
        // codegen path), then build + run.
        compile_rust(&src, &project, name, None, &[])?;

        // Structural tripwire: the emitted Rust must carry the fn-pointer
        // param (`f @ _: fn(aver_rt::AverInt) -> aver_rt::AverInt` — `Int` lowers
        // to `AverInt` now), the FnValue passed by bare name
        // (`applyIt(dbl, v)`), and the call-through-slot (`f(v)`). A
        // dropped FnValue or a mis-emitted slot call would erase these.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("f @ _: fn(aver_rt::AverInt) -> aver_rt::AverInt") {
            return Err(format!(
                "emitted Rust is missing the fn-pointer param \
                 `f @ _: fn(aver_rt::AverInt) -> aver_rt::AverInt` — \
                 the LocalSlot param lowering was dropped:\n{emitted}"
            ));
        }
        if !emitted.contains("applyIt(dbl, v)") || !emitted.contains("applyIt(inc, v)") {
            return Err(format!(
                "emitted Rust is missing the FnValue arg `applyIt(dbl, v)` / \
                 `applyIt(inc, v)` — the FnValue emit was dropped:\n{emitted}"
            ));
        }
        if !emitted.contains("f(v)") {
            return Err(format!(
                "emitted Rust is missing the call-through-slot `f(v)` — the \
                 LocalSlot call emit was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "MIR first-class-fn binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR first-class-fn stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1037: a direct function borrows collection parameters,
/// while another call still returns an owned collection. Equality must compare
/// matching Rust shapes (`&T` with `&T`) in either operand order; VM execution
/// alone cannot expose this because VM values have no Rust borrow distinction.
#[test]
fn call_result_equality_with_borrowed_param_builds_and_matches_vm() {
    let ws = temp_dir("borrowed_equality");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Main
    intent = "Compare an owned call result with a borrowed collection parameter."
    effects [Console]

fn made() -> List<Int>
    ? "Return a freshly built list by value."
    [1, 2]

fn sameRight(other: List<Int>) -> Bool
    ? "Compare an owned call result on the left with a borrowed parameter."
    made() == other

fn sameLeft(other: List<Int>) -> Bool
    ? "Compare a borrowed parameter on the left with an owned call result."
    other == made()

fn differs(other: List<Int>) -> Bool
    ? "Exercise the same ownership boundary through inequality."
    made() != other

fn main() -> Unit
    ! [Console.print]
    Console.print("right={sameRight([1, 2])} left={sameLeft([1, 2])} neq={differs([3])}")
"#,
    )
    .expect("write borrowed-equality probe source");

    let vm_stdout = run_vm(&source, None).expect("VM run");
    assert_eq!(vm_stdout, "right=true left=true neq=true");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "borrowed_equality", None, &[])?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("&(made()) == other")
            || !emitted.contains("other == &(made())")
            || !emitted.contains("&(made()) != other")
        {
            return Err(format!(
                "owned call results were not borrowed against collection params:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, "borrowed_equality")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "borrowed-equality generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "borrowed-equality stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1130: Rust keeps a borrow from an earlier call argument
/// alive while evaluating every later argument. If one of those later
/// expressions consumes the same owned binding, the earlier borrow must not
/// overlap the move: the later argument is bound first, and the borrow comes
/// after it.
#[test]
fn borrowed_call_argument_detaches_before_later_move() {
    let ws = temp_dir("borrowed_call_arg_later_move");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Main
    intent = "Use one match binding by reference and by value in one call."
    effects [Console]

record Sample
    atMs: Int
    done: Int

fn lastOf(samples: List<Sample>, fallback: Sample) -> Sample
    ? "Return the last sample, or the fallback."
    match samples
        [] -> fallback
        [head, ..tail] -> lastOf(tail, head)

fn slopeOf(newest: Sample, oldest: Sample) -> Int
    ? "Return the completed-work difference."
    newest.done - oldest.done

fn rateOf(samples: List<Sample>) -> Int
    ? "Borrow newest for slopeOf while lastOf consumes it as its fallback."
    match samples
        [] -> 0
        [newest, ..older] -> slopeOf(newest, lastOf(older, newest))

fn sameOf(samples: List<Sample>) -> Int
    ? "Borrow the same owned match binding twice without consuming it."
    match samples
        [] -> 0
        [newest, ..older] -> slopeOf(newest, newest)

fn firstOr(samples: List<Sample>) -> Sample
    ? "The first sample, or an empty one."
    match samples
        [] -> Sample(atMs = 0, done = 0)
        [s, .._] -> s

fn rateLet(samples: List<Sample>) -> Int
    ? "Borrow a bound sample for slopeOf while lastOf consumes it as its fallback."
    newest = firstOr(samples)
    slopeOf(newest, lastOf(samples, newest))

record Held
    first: Int
    kept: Sample

fn pairUp(a: Sample, b: Sample) -> Held
    ? "Read one sample, keep the other."
    Held(first = a.done, kept = b)

fn pairLet(samples: List<Sample>) -> Int
    ? "Borrow a bound sample and hand the same one on in one call."
    newest = firstOr(samples)
    held = pairUp(newest, newest)
    held.first + held.kept.done

fn main() -> Unit
    ! [Console.print]
    samples = [Sample(atMs = 2, done = 4), Sample(atMs = 0, done = 1)]
    Console.print("{rateOf(samples)}:{sameOf(samples)}:{rateLet(samples)}:{pairLet(samples)}")
"#,
    )
    .expect("write borrowed-call-argument probe source");

    let vm_stdout = run_vm(&source, None).expect("VM run");
    assert_eq!(vm_stdout, "3:0:3:8");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "borrowed_call_arg_later_move", None, &[])?;
        let emitted = generated_entry(&project)?;
        // The later argument is bound first, so the borrow comes after it.
        if !emitted.contains("= lastOf(samples, newest.clone());\n    slopeOf(&newest, &__arg") {
            return Err(format!(
                "the later argument was not bound before the borrow:\n{emitted}"
            ));
        }
        // The same local borrowed and then moved in one call.
        if !emitted.contains("pairUp(&newest.clone(), newest)") {
            return Err(format!(
                "the borrow was not detached before the move of the same local:\n{emitted}"
            ));
        }
        // A list head is borrowed from the list, so only the move clones it.
        if !emitted.contains("= lastOf(&older, newest.clone()); slopeOf(newest, &__arg") {
            return Err(format!(
                "the head handed on was not borrowed and cloned at the move:\n{emitted}"
            ));
        }
        // `newest` is only read here, so the arm borrows it from the list.
        if !emitted.contains("slopeOf(newest, newest)") {
            return Err(format!(
                "two borrowed arguments cloned despite having no later move:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, "borrowed_call_arg_later_move")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run borrowed-call-argument binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "borrowed-call-argument generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "borrowed-call-argument stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A self-TCO identity argument is elided from the Rust loop rather than
/// evaluated. If a later tail-call argument moves that same non-Copy param
/// into a helper, the value is still live on the hidden loop edge and must be
/// cloned at that owning use. Pure identity pass-through itself stays
/// clone-free.
#[test]
fn self_tco_clones_loop_carried_param_only_at_later_owning_use() {
    let ws = temp_dir("self_tco_loop_carried_move");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Repro
    exposes [entriesFor]
    intent =
        "Tail-recursive accumulator that also passes a non-Copy parameter to a helper."
    effects []

fn entryFor(txid: String, index: Int) -> String
    ? "Build one entry from the shared id and the position."
    "{txid}:{index}"

fn entriesFor(txid: String, items: List<Int>, index: Int, acc: List<String>) -> List<String>
    ? "Walk the items, using txid in every entry and again in the tail call."
    match items
        [] -> List.reverse(acc)
        [head, ..tail] -> entriesFor(txid, tail, index + 1, List.prepend(entryFor(txid, index), acc))

verify entriesFor
    entriesFor("ab", [7, 8], 0, []) => ["ab:0", "ab:1"]
"#,
    )
    .expect("write self-TCO moved-value probe source");

    let verify = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("verify")
        .arg(&source)
        .output()
        .expect("expected `aver verify` to execute");
    assert!(
        verify.status.success(),
        "VM verify failed:\n{}",
        format_output(&verify)
    );

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "self_tco_loop_carried_move", None, &[])?;

        // Load-bearing assertion: before the fix this build failed E0382 on
        // the second loop iteration. Build before checking source shape so
        // the test proves the emitted project passes rustc's ownership gate.
        let _bin = cargo_build(&project, "self_tco_loop_carried_move")?;

        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("entryFor(txid.clone(), index)") {
            return Err(format!(
                "the later owning helper call did not clone loop-carried txid:\n{emitted}"
            ));
        }
        if emitted.contains("let __tco0 = txid.clone()") {
            return Err(format!(
                "identity pass-through was cloned instead of the actual owning use:\n{emitted}"
            ));
        }

        cargo_test_in(&project, &shared_target_dir())?;
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A mutual-TCO invariant is removed from the state enum and passed to the
/// trampoline as `&T`. Equality must still recognize that Rust representation
/// and borrow the computed owned operand, just as it does for an ordinary
/// borrow-by-default parameter.
#[test]
fn mutual_tco_aligns_owned_equality_with_borrowed_invariant() {
    let ws = temp_dir("mutual_tco_borrowed_equality");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Repro
    exposes [without, kept]
    intent = "Mutual tail recursion compares an owned temporary with a shared invariant."
    effects []

fn without(items: List<List<Int>>, target: List<Int>, acc: List<List<Int>>) -> List<List<Int>>
    ? "Walk the outer list, handing each element to the partner."
    match items
        [] -> List.reverse(acc)
        [head, ..tail] -> kept(head, tail, target, acc)

fn kept(item: List<Int>, rest: List<List<Int>>, target: List<Int>, acc: List<List<Int>>) -> List<List<Int>>
    ? "Compare an owned reversed item with the invariant target."
    match List.reverse(item) == target
        true -> without(rest, target, acc)
        false -> without(rest, target, List.prepend(item, acc))

verify without
    without([[1], [2]], [1], []) => [[2]]

verify kept
    kept([1], [], [1], []) => []
"#,
    )
    .expect("write mutual-TCO borrowed-equality probe source");

    let verify = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("verify")
        .arg(&source)
        .output()
        .expect("expected `aver verify` to execute");
    assert!(
        verify.status.success(),
        "VM verify failed:\n{}",
        format_output(&verify)
    );

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "mutual_tco_borrowed_equality", None, &[])?;

        // Load-bearing assertion: before the fix this build failed E0308
        // because `item.reverse()` was owned while `target` was `&T`.
        let _bin = cargo_build(&project, "mutual_tco_borrowed_equality")?;

        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("enum __MutualTco") || !emitted.contains("fn __mutual_tco_trampoline_")
        {
            return Err(format!(
                "probe did not exercise the mutual-TCO trampoline:\n{emitted}"
            ));
        }
        if !emitted.contains("&(item.reverse()) == target") {
            return Err(format!(
                "owned comparison operand was not aligned with borrowed invariant target:\n{emitted}"
            ));
        }

        cargo_test_in(&project, &shared_target_dir())?;
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Second half of #1037: the same owned-versus-borrowed comparison as the
/// subject of a boolean `match`, which the Rust backend lowers to an `if`
/// through its own comparison emitter. That path skipped the borrow
/// alignment, so `match script == made()` compiled to `(script == made())`.
#[test]
fn match_subject_equality_with_borrowed_param_builds_and_matches_vm() {
    let ws = temp_dir("borrowed_match_equality");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Main
    intent = "Compare an owned call result with a borrowed parameter as a match subject."
    effects [Console]

fn made() -> List<Int>
    ? "Return a freshly built list by value."
    [1, 2]

fn paramLeft(script: List<Int>) -> String
    ? "Borrowed parameter on the left of the match subject."
    match script == made()
        true -> "same"
        false -> "other"

fn paramRight(script: List<Int>) -> String
    ? "Borrowed parameter on the right of the match subject."
    match made() == script
        true -> "same"
        false -> "other"

fn differs(script: List<Int>) -> String
    ? "Inequality as a match subject across the same boundary."
    match script != made()
        true -> "differs"
        false -> "same"

fn named(name: String) -> String
    ? "A string literal on the right of a match subject."
    match name == "anchor"
        true -> "yes"
        false -> "no"

fn main() -> Unit
    ! [Console.print]
    Console.print("left={paramLeft([1, 2])} right={paramRight([3])} neq={differs([3])} str={named("anchor")}")
"#,
    )
    .expect("write borrowed-match-equality probe source");

    let vm_stdout = run_vm(&source, None).expect("VM run");
    assert_eq!(vm_stdout, "left=same right=other neq=differs str=yes");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "borrowed_match_equality", None, &[])?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("script == &(made())")
            || !emitted.contains("&(made()) == script")
            || !emitted.contains("script == &(made())")
            || !emitted.contains("&**name == \"anchor\"")
        {
            return Err(format!(
                "match-subject comparisons were not aligned with the borrowed params:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, "borrowed_match_equality")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "borrowed-match-equality generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "borrowed-match-equality stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// #1065: the same match-subject comparison, but the call returns a list the
/// backend carries in its packed byte representation (a SHA-256 digest
/// turned back into `List<Int>`), compared with a borrowed parameter. The
/// borrow alignment must not depend on which Rust representation the list
/// takes.
#[test]
fn packed_list_match_subject_equality_with_borrowed_param_builds_and_matches_vm() {
    let ws = temp_dir("packed_match_equality");
    let source = ws.join("main.av");
    fs::write(
        &source,
        r#"module Main
    intent = "Compare a packed digest list with a borrowed parameter as a match subject."
    depends [Bytes, Crypto.Digest32]
    effects [Console]

fn sha256Of(script: List<Int>) -> List<Int>
    ? "A single SHA-256 of the script bytes, as a list."
    Bytes.octets(Crypto.Digest32.bytes(Crypto.sha256(Result.withDefault(Bytes.fromList(script), Bytes.fromList([])))))

fn matched(program: List<Int>, script: List<Int>) -> String
    ? "Does the program commit to this script?"
    match sha256Of(script) == program
        true -> "matched"
        false -> "other"

fn main() -> Unit
    ! [Console.print]
    Console.print("{matched(sha256Of([1, 2, 3]), [1, 2, 3])} {matched([0], [1, 2, 3])}")
"#,
    )
    .expect("write packed-match-equality probe source");

    let vm_stdout = run_vm(&source, None).expect("VM run");
    assert_eq!(vm_stdout, "matched other");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&source, &project, "packed_match_equality", None, &[])?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("sha256Of(script)) == program") {
            return Err(format!(
                "packed match-subject comparison was not aligned with the borrowed param:\n{emitted}"
            ));
        }
        let bin = cargo_build(&project, "packed_match_equality")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "packed-match-equality generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "packed-match-equality stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A qualified project type inside `Fn` is a module-owned Rust path, not a
/// compiler-owned dotted record alias. Before #1033 the context-free type
/// renderer flattened `Worker.Shape` to the nonexistent `Worker_Shape`, so
/// `aver compile` succeeded but the generated Cargo project failed to build.
#[test]
fn qualified_user_type_inside_fn_signature_builds_and_matches_vm() {
    let ws = temp_dir("qualified_fn_type");
    fs::write(
        ws.join("Worker.av"),
        r#"module Worker
    exposes [Shape, area]
    intent = "Expose a named type and a function over it."
    effects []

type Shape
    Circle(Int)

fn area(s: Shape) -> Int
    ? "Read a dependency-owned shape."
    match s
        Shape.Circle(n) -> n * 2
"#,
    )
    .expect("write Worker module");

    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Worker]
    intent = "Pass a dependency function through a qualified Fn signature."
    effects [Console]

fn apply(f: Fn(Worker.Shape) -> Int) -> Int
    ? "Call a function whose argument type belongs to Worker."
    f(Worker.Shape.Circle(5))

fn main() -> Unit
    ! [Console.print]
    Console.print(String.fromInt(apply(Worker.area)))
"#,
    )
    .expect("write entry module");

    let vm_stdout = run_vm(&entry, Some(&ws)).expect("VM run");
    assert_eq!(vm_stdout, "10", "VM contract changed");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(&entry, &project, "qualified_fn_type", Some(&ws), &[])?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("f @ _: fn(crate::aver_generated::worker::Shape) -> aver_rt::AverInt")
            || emitted.contains("Worker_Shape")
        {
            return Err(format!(
                "qualified user type did not retain its module-owned Rust path:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, "qualified_fn_type")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "qualified-Fn generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "qualified-Fn stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1042: colliding dependency exports must not turn an Aver
/// match binder into an ambiguous Rust name. Both dependencies expose
/// `message`, but the entry calls only `Legacy.message`;
/// `Result.Ok(message)` is purely local.
#[test]
fn colliding_dependency_function_names_do_not_conflict_with_match_binders() {
    let ws = temp_dir("dependency_function_binder_collision");
    fs::write(
        ws.join("Legacy.av"),
        r#"module Legacy
    exposes [message]
    intent = "Expose the legacy message calculation."
    effects []

fn message(value: Int) -> Result<Int, String>
    ? "Return the legacy message."
    Result.Ok(value)
"#,
    )
    .expect("write Legacy module");
    fs::write(
        ws.join("Modern.av"),
        r#"module Modern
    exposes [message]
    intent = "Expose the modern message calculation."
    effects []

fn message(value: Int) -> Result<Int, String>
    ? "Return the modern message."
    Result.Ok(value + 1)
"#,
    )
    .expect("write Modern module");

    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Legacy, Modern]
    intent = "Select one qualified message without importing either short name."
    effects [Console]

fn select() -> Option<Int>
    ? "Bind the result under the same short name exposed by both dependencies."
    match Legacy.message(7)
        Result.Err(why) -> Option.None
        Result.Ok(message) -> Option.Some(message)

fn main() -> Unit
    ! [Console.print]
    Console.print("picked={Option.withDefault(select(), 0)}")
"#,
    )
    .expect("write entry module");

    let vm_stdout = run_vm(&entry, Some(&ws)).expect("VM run");
    assert_eq!(vm_stdout, "picked=7", "VM contract changed");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(
            &entry,
            &project,
            "dependency_function_binder_collision",
            Some(&ws),
            &[],
        )?;
        let bin = cargo_build(&project, "dependency_function_binder_collision")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "dependency-function-binder generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "dependency-function-binder stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1151: the same Rust item-resolution ambiguity applies to
/// an ordinary `let` pattern. The source only calls qualified dependency
/// functions, so the local `opened` binding must remain unambiguously local.
#[test]
fn colliding_dependency_function_names_do_not_conflict_with_let_binders() {
    let ws = temp_dir("dependency_function_let_binder_collision");
    for (module, value) in [("Alpha", 1), ("Beta", 2)] {
        fs::write(
            ws.join(format!("{module}.av")),
            format!(
                r#"module {module}
    exposes [opened]
    intent = "Expose one of two colliding short function names."
    effects []

fn opened() -> Int
    ? "Return this module's value."
    {value}
"#
            ),
        )
        .expect("write dependency module");
    }

    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Alpha, Beta]
    intent = "Keep a local let binder distinct from colliding dependency functions."
    effects [Console]

fn total() -> Int
    ? "Bind a qualified call under the dependencies' shared short name."
    opened = Alpha.opened()
    higher(opened, Beta.opened())

fn higher(a: Int, b: Int) -> Int
    ? "Return the larger dependency value."
    match a >= b
        true -> a
        false -> b

fn main() -> Unit
    ! [Console.print]
    Console.print("total={total()}")
"#,
    )
    .expect("write entry module");

    let vm_stdout = run_vm(&entry, Some(&ws)).expect("VM run");
    assert_eq!(vm_stdout, "total=2", "VM contract changed");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(
            &entry,
            &project,
            "dependency_function_let_binder_collision",
            Some(&ws),
            &[],
        )?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted entry module: {e}"))?;
        if !emitted.contains("let opened @ _ =") {
            return Err(format!(
                "local let binder was not made explicit:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, "dependency_function_let_binder_collision")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "dependency-function-let-binder generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "dependency-function-let-binder stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1162: a function parameter is a Rust pattern too. Two
/// dependencies exporting the parameter's name must not make the signature
/// ambiguous, including signatures synthesized for TCO.
#[test]
fn colliding_dependency_function_names_do_not_conflict_with_parameters() {
    let ws = temp_dir("dependency_function_parameter_collision");
    for (module, suffix) in [("Alpha", ""), ("Beta", "!")] {
        fs::write(
            ws.join(format!("{module}.av")),
            format!(
                r#"module {module}
    exposes [payload]
    intent = "Expose one of two colliding short function names."
    effects []

fn payload(value: String) -> String
    ? "Return this module's payload."
    "{{value}}{suffix}"
"#
            ),
        )
        .expect("write dependency module");
    }

    fs::write(
        ws.join("Gamma.av"),
        r#"module Gamma
    exposes [carried, bounce, ping, pong]
    depends [Alpha, Beta]
    intent = "Keep parameter binders distinct from colliding dependency functions."
    effects []

fn carried(payload: String) -> String
    ? "Return an ordinary parameter with the colliding name."
    payload

fn bounce(n: Int, payload: List<Int>) -> List<Int>
    ? "Carry the colliding parameter through a self tail call."
    match n > 0
        true -> bounce(n - 1, payload)
        false -> payload

fn ping(n: Int, payload: List<Int>) -> List<Int>
    ? "Carry the colliding parameter through a mutual tail call."
    match n > 0
        true -> pong(n - 1, payload)
        false -> payload

fn pong(n: Int, payload: List<Int>) -> List<Int>
    ? "Return to ping with the same colliding parameter."
    match n > 0
        true -> ping(n - 1, payload)
        false -> payload
"#,
    )
    .expect("write Gamma module");

    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Gamma]
    intent = "Reach every parameter-emission shape."
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    Console.print("{Gamma.carried("ok")}/{List.len(Gamma.bounce(1, [1]))}/{List.len(Gamma.ping(1, [1]))}")
"#,
    )
    .expect("write entry module");

    let vm_stdout = run_vm(&entry, Some(&ws)).expect("VM run");
    assert_eq!(vm_stdout, "ok/1/1", "VM contract changed");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        compile_rust(
            &entry,
            &project,
            "dependency_function_parameter_collision",
            Some(&ws),
            &[],
        )?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("gamma")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted Gamma module: {e}"))?;
        if !emitted.contains("carried(payload @ _: AverStr)") {
            return Err(format!(
                "ordinary parameter binder was not made explicit:\n{emitted}"
            ));
        }
        if !emitted.contains("payload @ _: aver_rt::AverIntList") {
            return Err(format!(
                "TCO parameter binder was not made explicit:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, "dependency_function_parameter_collision")?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "dependency-function-parameter generated binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "dependency-function-parameter stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Class-level regression for #1174. Two dependency namespaces deliberately
/// reuse both a function name and a type name, while the consumer reuses the
/// function name in every local binding position. The generated module must
/// contain no dependency glob imports: identity-bearing calls and types are
/// qualified, and local hygiene remains a separate concern.
#[test]
fn rust_symbol_collision_matrix_compiles_without_dependency_globs() {
    let ws = temp_dir("rust_symbol_collision_matrix");
    fs::write(
        ws.join("Alpha.av"),
        r#"module Alpha
    exposes [clash, Packet, read]
    intent = "First namespace in the collision matrix."
    effects []

record Packet
    value: Int

fn clash() -> String
    "alpha"

fn read(packet: Packet) -> Int
    packet.value
"#,
    )
    .expect("write Alpha module");
    fs::write(
        ws.join("Beta.av"),
        r#"module Beta
    depends [Alpha]
    exposes [clash, Packet, Envelope, read, unpack]
    intent = "Second namespace in the collision matrix."
    effects []

record Packet
    label: String

record Envelope
    packet: Alpha.Packet

fn clash() -> String
    "beta"

fn read(packet: Packet) -> Int
    String.len(packet.label)

fn unpack(envelope: Envelope) -> Int
    Alpha.read(envelope.packet)
"#,
    )
    .expect("write Beta module");

    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Alpha, Beta]
    intent = "Exercise namespace identity and every local binder shape."
    effects []

fn fromParameter(clash: String) -> Int
    String.len(clash)

fn fromLet() -> Int
    clash = Alpha.clash()
    String.len(clash)

fn fromMatch() -> Int
    match Result.Ok(Beta.clash())
        Result.Ok(clash) -> String.len(clash)
        Result.Err(_) -> 0

fn add(left: Int, right: Int) -> Int
    left + right

fn localHygiene(_s: Int, _h0: Int, _r0: Int, _b0: Int) -> Tuple<Int, Int>
    (add(_s, _h0), add(_r0, _b0))!

fn mapHygiene(m: List<Tuple<String, Int>>) -> Map<String, Int>
    Map.fromList(m)

fn main() -> Int
    local = localHygiene(1, 10, 100, 1000)
    fromParameter("xx") + fromLet() + fromMatch() + Alpha.read(Alpha.Packet(value = 3)) + Beta.read(Beta.Packet(label = "four")) + Beta.unpack(Beta.Envelope(packet = Alpha.Packet(value = 5)))
"#,
    )
    .expect("write entry module");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let shared_target = shared_target_dir();
    let shared_target = shared_target
        .to_str()
        .expect("shared Cargo target path should be UTF-8");
    let result = (|| -> Result<(), String> {
        compile_rust_env(
            &entry,
            &project,
            "rust_symbol_collision_matrix",
            Some(&ws),
            &["--with-replay", "--check"],
            &[
                ("CARGO_TARGET_DIR", shared_target),
                ("CARGO_NET_OFFLINE", "true"),
            ],
        )?;
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|error| format!("read emitted entry module: {error}"))?;
        if emitted.contains("use crate::aver_generated::") {
            return Err(format!(
                "entry module still opens a dependency namespace:\n{emitted}"
            ));
        }
        for dependency in ["alpha", "beta"] {
            let module = fs::read_to_string(
                project
                    .join("src")
                    .join("aver_generated")
                    .join(dependency)
                    .join("mod.rs"),
            )
            .map_err(|error| format!("read emitted {dependency} module: {error}"))?;
            if !module.contains("pub fn clash") {
                return Err(format!(
                    "same-named function was not retained in {dependency}:\n{module}"
                ));
            }
            if dependency == "beta"
                && !module.contains(
                    "<crate::aver_generated::alpha::Packet as ReplayValue>::from_replay_json",
                )
            {
                return Err(format!(
                    "replay field type did not retain Alpha.Packet identity:\n{module}"
                ));
            }
        }
        for qualified in [
            "crate::aver_generated::alpha::read",
            "crate::aver_generated::beta::read",
            "crate::aver_generated::alpha::Packet",
            "crate::aver_generated::beta::Packet",
            "crate::aver_generated::beta::Envelope",
        ] {
            if !emitted.contains(qualified) {
                return Err(format!(
                    "missing identity-qualified symbol `{qualified}`:\n{emitted}"
                ));
            }
        }
        if !emitted.contains("std::thread::scope(|__s|")
            || !emitted.contains("_s @ _")
            || !emitted.contains("let mut __map = HashMap::new()")
        {
            return Err(format!(
                "compiler temporaries did not stay in the reserved `__` namespace:\n{emitted}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// Regression for #1134: derive requirements belong to a declaration, not
/// its bare type name. `Snapshot.Progress` can carry Eq + Hash, while the
/// same-named `Bodies.Progress` contains a Float and cannot. The emitter unit
/// test pins the chosen derive text; this test asks rustc about the complete
/// multi-module project through the public `compile --check` gate.
#[test]
fn same_named_records_keep_declaration_scoped_derives_under_compile_check() {
    let ws = temp_dir("same_named_record_derives");
    fs::write(
        ws.join("Snapshot.av"),
        r#"module Snapshot
    exposes [Progress, done]
    intent = "Own the equality-safe progress record."
    effects []

record Progress
    done: Int

fn done(progress: Progress) -> Int
    ? "Read the completed count."
    progress.done
"#,
    )
    .expect("write Snapshot module");
    fs::write(
        ws.join("Bodies.av"),
        r#"module Bodies
    exposes [Progress, Walk, count]
    intent = "Own a same-named progress record that cannot be Eq."
    effects []

record Progress
    ratio: Float

record Walk
    step: Progress

fn count(walk: Walk) -> Int
    ? "Ignore the floating-point walk state."
    0
"#,
    )
    .expect("write Bodies module");
    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    depends [Snapshot, Bodies]
    intent = "Reach both same-named records in one generated Rust crate."
    effects []

fn main() -> Int
    Snapshot.done(Snapshot.Progress(done = 1)) + Bodies.count(Bodies.Walk(step = Bodies.Progress(ratio = 0.5)))
"#,
    )
    .expect("write entry module");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let shared_target = shared_target_dir();
    let shared_target = shared_target
        .to_str()
        .expect("shared Cargo target path should be UTF-8");
    let result = compile_rust_env(
        &entry,
        &project,
        "same_named_record_derives",
        Some(&ws),
        &["--check"],
        &[
            ("CARGO_TARGET_DIR", shared_target),
            ("CARGO_NET_OFFLINE", "true"),
        ],
    );

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// A bare namespace reference written as a MODULE-LEVEL statement is dead
/// code, exactly as it is inside a fn body.
///
/// `Vector.set` alone on a line is legal Aver — see
/// `tests/regressions/parser/bare_namespace_method_ref.av`, where it sits
/// in a fn body and the program runs. In a body the statement chain lowers
/// to a MIR `Let` chain and the DCE pass elides `let <unread> = <pure>;`
/// before any emitter sees it. Module-level statements are lowered one
/// value at a time, outside `MirProgram.fns`, so DCE never ran on them and
/// the Rust emitter rendered the `Project` over a `FnValue` verbatim:
/// `Result.Err;`, a field access on a namespace, E0423. `aver check` said
/// ✓ and the failure surfaced only at `cargo build` of the emitted crate.
/// `fuzz_codegen_rust` found the 20-byte `module M\n\nResult.Err`.
#[test]
fn a_module_level_bare_namespace_reference_is_dead_code_not_a_field_access() {
    let ws = temp_dir("module_level_bare_namespace");
    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module M
    intent = "Bare namespace references at module level are no-ops."
    effects []

Result.Err
String.len
Option.None

fn value() -> Int
    ? "Give the crate something to emit besides the dead statements."
    7
"#,
    )
    .expect("write bare-namespace entry");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let shared_target = shared_target_dir();
    let shared_target = shared_target
        .to_str()
        .expect("shared Cargo target path should be UTF-8");

    let result = (|| -> Result<(), String> {
        // The VM already treats the three statements as no-ops; Rust must
        // agree, and `--check` is rustc's own verdict on the emitted crate.
        run_vm(&entry, Some(&ws))?;
        compile_rust_env(
            &entry,
            &project,
            "module_level_bare_namespace",
            Some(&ws),
            &["--check"],
            &[
                ("CARGO_TARGET_DIR", shared_target),
                ("CARGO_NET_OFFLINE", "true"),
            ],
        )?;
        let entry_rs = fs::read_to_string(project.join("src/aver_generated/entry/mod.rs"))
            .map_err(|e| format!("read generated entry module: {e}"))?;
        for namespace in ["Result.Err", "String.len", "Option.None"] {
            if entry_rs.contains(namespace) {
                return Err(format!(
                    "emitted `{namespace}` as a Rust field access on a namespace:\n{entry_rs}"
                ));
            }
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

/// A lowercase name in a sum-type variant field is refused with a
/// diagnostic, not an emitter panic.
///
/// `type Tree / Leaf / Node(tree, Int)` used to pass `aver check` ✓ — the
/// strict annotation parser refused `tree`, and both `TypeDef` arms
/// swallowed the refusal into the checker's internal `Type::Invalid`
/// recovery value without reporting it. Codegen then hit
/// `type_to_rust`'s deliberate panic on `Type::Invalid` ("unresolved
/// typing leaked into codegen"), which is the right assertion in the
/// wrong place: 45 of the 121 `fuzz_codegen_rust` crashes on the
/// 2026-08-31 nightly were this one shape. The compiler must refuse the
/// program where the annotation is, and must not abort.
#[test]
fn a_lowercase_variant_field_type_is_a_diagnostic_not_a_codegen_panic() {
    let ws = temp_dir("lowercase_variant_field");
    let entry = ws.join("main.av");
    fs::write(
        &entry,
        r#"module M
    intent = "A variant field annotated with a name that is not a type."
    effects []

type Tree
    Leaf
    Node(tree, Int)

fn size(t: Tree) -> Int
    ? "Never reached — the declaration above does not typecheck."
    1
"#,
    )
    .expect("write lowercase-variant entry");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");

    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(&entry)
        .arg("--target")
        .arg("rust")
        .arg("--module-root")
        .arg(&ws)
        .arg("-o")
        .arg(&project)
        .output()
        .expect("expected `aver compile --target rust` to spawn");
    let rendered = format_output(&out);
    let _ = fs::remove_dir_all(&ws);

    assert!(
        !out.status.success(),
        "compile must refuse the program:\n{rendered}"
    );
    assert!(
        rendered.contains("Type 'Tree', variant 'Node'")
            && rendered.contains("Unknown type 'tree'"),
        "expected the annotation to be named:\n{rendered}"
    );
    assert!(
        !rendered.contains("panicked"),
        "the emitter must not abort:\n{rendered}"
    );
}

// ─── Mode (h): Int.fromString / Float.fromString Err-message bytes ────────
//
// `Int.fromString` / `Float.fromString` return a `Result<_, String>`.
// On a failed parse the VM (`src/types/int.rs` / `float.rs`) formats the
// Err string as `Cannot parse '{input}' as Int` / `… as Float`. The Rust
// emit used to delegate to rustc's NATIVE `parse` error ("invalid digit
// found in string"), a real cross-backend SEMANTIC divergence: a program
// that reads the `Result.Err(String)` got different bytes on Rust vs the
// VM, and verify cases asserting the message failed under `cargo test`.
//
// This probe reads BOTH parse Results back as strings and prints them, so
// the built binary's stdout carries the exact Err bytes. A regression to
// the native rustc message would change the bytes here and fail parity.
const MIR_FROMSTRING_ERR_PROBE: &str = r#"module FromStringErrProbe
    intent =
        "Probes Int.fromString / Float.fromString Err-message bytes."
        "The Err string must match the VM byte-for-byte, not rustc's native parse error."
    effects [Console]

fn showInt(s: String) -> String
    ? "Render the Int.fromString Result as a string."
    match Int.fromString(s)
        Result.Ok(n) -> "ok:{n}"
        Result.Err(e) -> "err:{e}"

fn showFloat(s: String) -> String
    ? "Render the Float.fromString Result as a string."
    match Float.fromString(s)
        Result.Ok(f) -> "ok:{f}"
        Result.Err(e) -> "err:{e}"

fn main() -> Unit
    ! [Console.print]
    Console.print(showInt("bad"))
    Console.print(showInt("12x"))
    Console.print(showInt(""))
    Console.print(showInt("42"))
    Console.print(showFloat("nope"))
    Console.print(showFloat("3.14"))
"#;

#[test]
fn mir_fromstring_err_message_matches_vm() {
    let ws = temp_dir("mir_fromstring");
    let src = ws.join("fromstring_err_probe.av");
    fs::write(&src, MIR_FROMSTRING_ERR_PROBE).expect("write fromString probe source");

    let vm_stdout = run_vm(&src, None).unwrap_or_else(|e| panic!("VM run failed: {e}"));

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_fromstring_probe";

    let result = (|| -> Result<(), String> {
        // NOTE: no `mir_lowered_count` guard here — every fn in this probe
        // calls a builtin (`Int.fromString` / `Console.print`), and the
        // coverage walk's `--explain-mir-coverage` reports `Call(Builtin)`
        // as a fallback (its `for_test` ctx carries an empty builtin
        // table), so it would report `mir_lowered = 0` even though the
        // production path emits these fns fine. The structural tripwire
        // below (the VM-format message must appear in the emitted Rust)
        // is the real "fromString emit is exercised" guard.
        compile_rust(&src, &project, name, None, &[])?;

        // Structural tripwire: the emitted Rust must format the Aver Err
        // message, NOT delegate to rustc's native parse error.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("Cannot parse '{}' as Int")
            || !emitted.contains("Cannot parse '{}' as Float")
        {
            return Err(format!(
                "emitted Rust is missing the VM-format fromString Err message \
                 (`Cannot parse '{{}}' as Int/Float`) — it likely regressed to \
                 rustc's native parse error:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "MIR fromString binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "MIR fromString Err-bytes mismatch (Rust must match VM byte-for-byte)\n\
                 --- VM ---\n{vm_stdout}\n--- Rust (MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Representation of a type that arrives without being named ──────────

/// A record whose type a module never names, and never can.
///
/// `High.Report` depends on `Mid.Rules` alone. `Mid.Rules` returns a
/// `Rules`, whose `policy` field is a `Low.Policy.Policy` — so `Policy`
/// reaches `High.Report` by projection, from a module it does not import
/// and does not re-expose. That is legal Aver and the VM runs it.
///
/// The Rust backend asks one question about the base of a projection: is
/// this type's `List<Int>` stored as packed bytes? It used to answer from a
/// `TypeId` that had been minted in a DIFFERENT symbol table (each
/// dependency module is typechecked on its own first) and left in place
/// when re-resolution failed, so the id indexed whatever declaration
/// happened to sit at that position in the whole-program table. Here it
/// landed on `Bytes`, and the emitter wrote `.to_int_list()` — a `PackedU8`
/// method — onto a `bool` field. `aver compile` reported success, `aver
/// check` was byte-identical, and the only thing that ever said otherwise
/// was `cargo build`:
/// `error[E0599]: no method named to_int_list found for type bool`.
const PROJECTED_TYPE_MODULES: &[(&str, &str)] = &[
    (
        "low/policy.av",
        r#"module Policy
    intent =
        "The flags a run is made under. Nothing above Mid.Rules imports this."
    exposes [Policy, none]
    depends []
    effects []

record Policy
    strict: Bool
    verbose: Bool

fn none() -> Policy
    ? "Every flag off."
    Policy(strict = false, verbose = false)

verify none
    none().strict => false
"#,
    ),
    (
        "mid/rules.av",
        r#"module Rules
    intent =
        "The rules in force, one field of which is a Policy."
    exposes [Rules, at]
    depends [Low.Policy]
    effects []

record Rules
    height: Int
    policy: Policy

fn at(height: Int) -> Rules
    ? "The rules at a height, with no policy flag set."
    Rules(height = height, policy = Low.Policy.none())

verify at
    at(1).height => 1
"#,
    ),
    (
        "high/report.av",
        r#"module Report
    intent =
        "Reads a flag off a Policy it never names and cannot name."
    exposes [strictness]
    depends [Mid.Rules]
    effects []

fn strictness(rules: Rules) -> String
    ? "Says whether the rules are strict, reading the flag by projection."
    match rules.policy.strict
        true -> "strict"
        _ -> "lenient"

verify strictness
    strictness(Mid.Rules.at(3)) => "lenient"
"#,
    ),
    (
        "main.av",
        r#"module Main
    intent =
        "Prints how strict the rules at a height are, and some bytes as hex."
    depends [Bytes, High.Report, Mid.Rules]
    effects [Console.print]

fn main() -> Unit
    ? "Prints the strictness of the rules at height 9 and three bytes."
    ! [Console.print]
    Console.print("{High.Report.strictness(Mid.Rules.at(9))} {hexOf([1, 2, 3])}")

fn hexOf(values: List<Int>) -> String
    ? "The hex spelling of a list of byte values."
    match Bytes.fromList(values)
        Result.Ok(bytes) -> Bytes.toHex(bytes)
        Result.Err(why) -> why
"#,
    ),
];

#[test]
fn a_type_reached_only_by_projection_keeps_its_representation() {
    let ws = temp_dir("projected-type");
    let root = ws.join("src");
    for (relative, source) in PROJECTED_TYPE_MODULES {
        let path = root.join(relative);
        fs::create_dir_all(path.parent().expect("module parent")).expect("create module dir");
        fs::write(&path, source).expect("write module");
    }
    let entry = root.join("main.av");
    let name = "projected_type_probe";
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");

    let result = (|| -> Result<(), String> {
        let vm_stdout = run_vm(&entry, Some(&root))?;
        compile_rust(&entry, &project, name, Some(&root), &[])?;

        // The emitted projection must be the bool field itself. A
        // `.to_int_list()` here is the backend re-typing a `bool` as a
        // packed byte sequence.
        let report = project.join("src/aver_generated/high/report/mod.rs");
        let emitted =
            fs::read_to_string(&report).map_err(|e| format!("read {}: {e}", report.display()))?;
        if emitted.contains("to_int_list") {
            return Err(format!(
                "the backend chose a packed-bytes representation for a `Bool` field \
                 whose owning type it could not identify:\n{emitted}"
            ));
        }

        // And the crate builds — which is the only thing that ever caught
        // this before.
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Regression for #1085: a dependency's bare `main()` is its own ordinary
/// module function. It must never bind to the entry point merely because the
/// dependency projection used to omit functions named `main`.
#[test]
fn dependency_calling_its_own_main_builds_and_matches_vm() {
    let ws = temp_dir("dependency-own-main");
    let root = ws.join("src");
    let dep = root.join("dep/helper.av");
    fs::create_dir_all(dep.parent().expect("dependency parent")).expect("create module dir");
    fs::write(
        &dep,
        r#"module Helper
    intent = "Keep a module-local main callable as an ordinary function."
    exposes [ask]
    depends []

fn main() -> Int
    111

fn ask() -> Int
    main()

verify ask
    ask() => 111
"#,
    )
    .expect("write dependency");
    let entry = root.join("main.av");
    fs::write(
        &entry,
        r#"module Main
    intent = "Print the dependency's answer without capturing its main name."
    depends [Dep.Helper]
    effects [Console.print]

fn main() -> Unit
    ! [Console.print]
    Console.print("{Dep.Helper.ask()}")
"#,
    )
    .expect("write entry");

    let name = "dependency_own_main";
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let result = (|| -> Result<(), String> {
        let vm_stdout = run_vm(&entry, Some(&root))?;
        if vm_stdout != "111" {
            return Err(format!("VM oracle returned {vm_stdout:?}, expected 111"));
        }
        compile_rust(&entry, &project, name, Some(&root), &[])?;
        let bin = cargo_build(&project, name)?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "generated dependency-main binary failed:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}
