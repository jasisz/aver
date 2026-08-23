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
//! ## Tiers
//!
//! (No test counts are quoted here on purpose: they have gone stale
//! twice, and a grep that counts them also matches this paragraph. The
//! attribute is the tier marker — read it off the source.)
//!
//! - **default**: every test NOT marked `#[ignore]`, run by every `cargo
//!   test` invocation. It started as a 3-example plain-parity subset plus
//!   the two critical behavioral probes (deny-policy, record/replay) and
//!   has since grown a tail of single-shape regressions — each one a
//!   lowering or ownership bug that only a real build-and-run catches.
//!   ~one cargo dep-build, then seconds each. This tier competes for a
//!   shared CI budget, so a new case earns its place here only when no
//!   cheaper harness can hold the same ground, and only one shape per
//!   class.
//! - **full**: every `#[ignore]`d test, each one additionally guarded by
//!   an `AVER_RUST_DIFF_FULL` env-var check in its own body — every
//!   single-file example, the multi-module (`depends`) examples, and the
//!   second shape of a class whose first shape already runs in the
//!   default tier. The dep-build + per-example build is minutes of wall
//!   time, too heavy for PR smoke. Run it with
//!   `AVER_RUST_DIFF_FULL=1 cargo test --test rust_codegen_differential -- --ignored --nocapture`.
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

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

// ─── Shared infrastructure ──────────────────────────────────────────────

/// Monotonic counter so concurrently-running tests never collide on a
/// temp-dir name even within the same nanosecond.
static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

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

fn format_output(output: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
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
        Ok(())
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

fn main() -> Unit
    ? "Print every shape so a backend that drifts shows a diff, not a pass."
    ! [Console.print]
    Console.print("{shape("")} {shape("abc")} {shape("\u{e9}x")} {shape("\u{1F980}ab")}")
    Console.print("{value("0")}{value("9")}/{value("A")}{value("f")}/{value("\u{212A}")}/{value("\u{130}")}/{value("ab")}/{value("")}")
    Console.print("{total("09af")}|{total("")}|{total("0x9")}|{total("\u{e9}0")}|{total("9\u{1F980}")}|{total("\u{212A}")}")
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
    let expected = "0:0: 3:1:ab-c? 2:0:éx 3:1:🦀a-b?\n09/1015/20/-1/-1/-1\nok:34|ok:0|bad 'x' after 0|bad 'é' after 0|bad '🦀' after 9|ok:20";
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

fn main() -> Unit
    ? "Print the indexed result for backend parity."
    ! [Console.print]
    Console.print(walk("aą😀z", 0, 0))
"#;
    let expected = "4:ą😀";
    let vm = run_vm_inline("string_index", src).expect("VM run");
    let rust =
        build_run_rust_inline("string_index", src).expect("Rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM indexed String contract changed");
    assert_eq!(
        rust, expected,
        "Rust indexed String access diverged from the VM"
    );
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
    // arm (whose `Result.Err` bytes must match the VM verbatim).
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
"#;
    let expected = "2 7 5 42 -1 -7 -1 0\n1267650600228229401496703205376 -2 1 255 0\n-1267650600228229401496703205377 1267650600228229401496703205377 0 0 -2\nok 16\nerr negative shift count";

    let vm = run_vm_inline("bits_namespace", src).expect("vm run");
    let rust =
        build_run_rust_inline("bits_namespace", src).expect("rust compile + cargo build + run");
    assert_eq!(vm, expected, "VM Bits contract changed");
    assert_eq!(rust, expected, "Rust Bits semantics diverged from the VM");
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
    "{Bytes.toHex(bytes)}/{List.len(Bytes.toList(bytes))}"

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

fn ready(connections: Map<Int, Tcp.Connection>, timeoutMs: Int) -> Result<List<Int>, String>
    ? "Return caller-owned IDs whose connections can be read without waiting."
    ! [Tcp.poll]
    Tcp.poll(connections, timeoutMs)

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
        emitted.contains("pub fn setOne(key: AverStr, mut into: aver_rt::AverMap"),
        "setOne must take its Map by value after return-alias analysis:\n{emitted}"
    );
    assert!(
        emitted.contains("into.insert_owned(key, AverStr::from(\"v\"))"),
        "setOne must move the owned Map into insert_owned:\n{emitted}"
    );
    assert!(
        emitted.contains("let __tco1 = setOne(head, into);"),
        "the recursive fold must move its accumulator through setOne:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn setOneResult(key: AverStr, mut into: aver_rt::AverMap"),
        "the Rust-only pass must graduate a Map carried by Result:\n{emitted}"
    );
    assert!(
        emitted.contains("Ok(into.insert_owned(key, AverStr::from(\"v\")))"),
        "Result.Ok must wrap the owned Map successor without cloning:\n{emitted}"
    );
    assert!(
        emitted.contains("let __tco1 = setOneResult(head, into)?;"),
        "Result `?` must preserve the owned accumulator move:\n{emitted}"
    );
    assert!(
        emitted.contains("pub fn setStoreOne(key: AverStr, mut store: Store)"),
        "a returned Store successor must take the record by value:\n{emitted}"
    );
    assert!(
        emitted.contains("values: store.values.insert_owned(key, AverStr::from(\"v\"))"),
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

/// The `Vector` half of the same class — the other collection `own_param`
/// can graduate. `keepLonger` hands back one of its argument vectors
/// straight into `overwrite`, which writes over every position, and `run`
/// reads position 0 of its own vector afterwards.
///
/// Full tier: the `Map` sibling above already carries the class through the
/// emitted-project path, and the cheap in-process pins cover the arm, so a
/// second `cargo build` does not belong in the default tier.
#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn rust_fn_result_argument_keeps_the_callers_vector_intact() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping the Vector half of the call-result ownership class — \
             set AVER_RUST_DIFF_FULL=1 (the Map half runs in the default tier, \
             and the in-process pins in own_param_graduation cover the arm)"
        );
        return;
    }

    let src = r#"module OwnedFnResultVector
    intent = "A helper's Vector result flows straight into another call while the caller keeps its own vector"
    depends []
    effects [Console.print]

fn keepLonger(a: Vector<Int>, b: Vector<Int>) -> Vector<Int>
    ? "Returns one of its arguments, so the result shares a caller value."
    match Vector.len(a) >= Vector.len(b)
        true -> a
        false -> b

fn overwrite(v: Vector<Int>, i: Int, n: Int) -> Int
    ? "Threads the vector linearly, writing i*i at every position."
    match i == n
        true -> Vector.len(v)
        false -> overwrite(Option.withDefault(Vector.set(v, i, i * i), v), i + 1, n)

fn run() -> String
    ? "Pass a call result straight into another call, then read the original."
    base = Option.withDefault(Vector.set(Vector.new(4, 0), 0, 7), Vector.new(4, 0))
    touched = overwrite(keepLonger(base, Vector.new(2, 0)), 0, 4)
    "{touched} {Option.withDefault(Vector.get(base, 0), 0 - 1)}"

fn main() -> Unit
    ! [Console.print]
    Console.print(run())
"#;
    // 4 positions overwritten, and `base` still reads 7 at position 0.
    let expected = "4 7";
    let vm = run_vm_inline("own_fn_result_vector", src).expect("vm run");
    let rust = build_run_rust_inline("own_fn_result_vector", src).expect("rust build+run");
    assert_eq!(
        vm, expected,
        "the vector the caller kept was mutated through a call result"
    );
    assert_eq!(
        rust, vm,
        "Rust diverged from VM — a call result was treated as a uniquely-owned vector"
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
/// tuple and a `List` all failed; `String`, `Int`, `Float`, `Bool` and the
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
"#;
    let expected = "1\n2\ngreen 3\n5\n6t\n2\n3\n1\ns\n10\n1.5\ntrue";
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
        if !emitted.contains("let __provider_arg0 = path.clone();") {
            return Err(format!(
                "the provider arg temp takes `path` instead of owning a copy of it — \
                 every later read of `path` is E0382:\n{emitted}"
            ));
        }
        // A field read through a `&Location` param: the temp has to own a
        // copy, not move out of the borrow.
        if !emitted.contains("let __provider_arg0 = place.segment.clone();") {
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

// ─── Mode (d): effectful security probe ──────────────────────────────────
//
// SECURITY-SENSITIVE. The MIR walker OWNS effectful builtin emission
// (replay / policy / bare framing) — it is the sole codegen path now. A
// dropped wrapper there silently disables aver.toml DENY enforcement or
// record/replay capture — and it's invisible to rustc, to coverage, and
// to happy-path stdout. These probes build + RUN the binary and assert
// the policy is actually enforced / the effect is actually captured, so
// a dropped MIR wrapper reaches the built binary and the probe catches it.
//
// An effect probe cannot use the `mir_lowered_count(...)` guard. Every fn
// in one calls a builtin, and the coverage walk behind
// `--explain-mir-coverage` reports every `Call(Builtin)` as a fallback
// (its `for_test` ctx carries an empty builtin table), so the guard reads
// zero on a program the production path emits fine — it can only ever
// fail. What proves the wrapper was emitted is the structural tripwire on
// the emitted Rust plus the deny / capture run below.

/// Single-fn Disk-write program for the embedded-policy probe. The
/// write rides a helper fn (`writeIt`) that is a single-expr body, so the
/// `aver_policy::check_disk` wrapper is emitted by the MIR walker.
/// `__PATH__` is substituted at test time.
const MIR_DISK_WRITE_PROBE: &str = r#"module MirDiskProbe
    intent =
        "Writes one file then prints DONE. Probes the MIR-emitted policy"
        "wrapper: under a deny policy the write must be rejected at runtime."
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

fn write_embedded_disk_policy(dir: &Path, allowed_path: &str) {
    fs::create_dir_all(dir).expect("create policy dir");
    fs::write(
        dir.join("aver.toml"),
        format!("[effects.Disk]\npaths = [{allowed_path:?}]\n"),
    )
    .expect("write aver.toml");
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --include-ignored"]
fn mir_forced_embedded_policy_rejects_denied_disk_write() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced policy probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let ws = temp_dir("mir-deny");
    // The embedded aver.toml is read from the module root at compile
    // time, so the source + the deny aver.toml share a dir.
    let proj_root = ws.join("src-root");
    fs::create_dir_all(&proj_root).expect("create src root");
    let out_path = ws.join("out.txt");
    let src = proj_root.join("disk_probe.av");
    fs::write(
        &src,
        MIR_DISK_WRITE_PROBE.replace("__PATH__", &aver_path_literal(&out_path)),
    )
    .expect("write probe source");

    let result = (|| -> Result<(), String> {
        // (0) NOTE: no `mir_lowered_count` guard here. Both fns in this
        // probe call host-backed operations (provider-bound `Disk.writeText`
        // and builtin `Console.print`), and the coverage walk behind
        // `--explain-mir-coverage` lacks the production capability/builtin
        // tables. It can therefore report these calls as fallbacks even
        // though the production path emits both fns fine. A
        // guard on that number cannot pass on an effect probe; it only
        // short-circuited the deny/allow assertions below, which is the
        // whole point of the test. The structural tripwire (the emitted
        // Rust must carry the `aver_policy::check_disk` wrapper) plus the
        // deny/allow runs are the real "the MIR walker emitted this
        // effect" evidence.

        // (1) DENY: embed an aver.toml whose allow-list names a
        // DIFFERENT path → the write to out.txt is denied at compile-
        // baked policy. The MIR walker emits the `aver_policy::check_disk`
        // wrapper, then build + run.
        write_embedded_disk_policy(&proj_root, "/aver/nonexistent/allowed/only");
        let project = ws.join("project-deny");
        fs::create_dir_all(&project).expect("create project dir");
        let name = "mir_deny_disk_probe";
        compile_rust_env(
            &src,
            &project,
            name,
            Some(&proj_root),
            &["--policy", "embed"],
            &[],
        )?;
        // Sanity: the emitted source must carry the MIR-emitted policy
        // wrapper. (If a future refactor drops it, the run-time assert
        // below is the real gate; this is a fast structural tripwire.)
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_policy::check_disk") {
            return Err(format!(
                "emitted Rust is missing the `aver_policy::check_disk` wrapper — \
                 the MIR effectful policy wrap was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build(&project, name)?;
        let denied = Command::new(&bin)
            .output()
            .map_err(|e| format!("run denied binary: {e}"))?;
        if denied.status.success() {
            return Err(format!(
                "deny run unexpectedly SUCCEEDED — the MIR-emitted policy wrapper \
                 was not enforced:\n{}",
                format_output(&denied)
            ));
        }
        let denied_stderr = String::from_utf8_lossy(&denied.stderr);
        if !denied_stderr.contains("denied by aver.toml policy") {
            return Err(format!(
                "deny run failed for the wrong reason (expected a policy \
                 violation):\n{}",
                format_output(&denied)
            ));
        }
        if out_path.exists() {
            return Err(format!(
                "deny run wrote {} despite the deny policy — the MIR-emitted \
                 check ran AFTER the effect (or not at all)",
                out_path.display()
            ));
        }

        // (2) ALLOW: re-embed an aver.toml whose allow-list names the
        // real write path → the write is permitted. Proves the deny in
        // (1) was the policy, not an unconditional failure.
        write_embedded_disk_policy(&proj_root, &out_path.to_string_lossy());
        let project_allow = ws.join("project-allow");
        fs::create_dir_all(&project_allow).expect("create allow project dir");
        let name_allow = "mir_allow_disk_probe";
        compile_rust_env(
            &src,
            &project_allow,
            name_allow,
            Some(&proj_root),
            &["--policy", "embed"],
            &[],
        )?;
        let bin_allow = cargo_build(&project_allow, name_allow)?;
        let allowed = Command::new(&bin_allow)
            .output()
            .map_err(|e| format!("run allowed binary: {e}"))?;
        if !allowed.status.success() {
            return Err(format!(
                "allow run failed — the probe should succeed when the write path \
                 is permitted:\n{}",
                format_output(&allowed)
            ));
        }
        if !out_path.exists() {
            return Err(format!(
                "allow run did not write {} — the effect was suppressed even \
                 though the policy allowed it",
                out_path.display()
            ));
        }
        if !String::from_utf8_lossy(&allowed.stdout).contains("DONE") {
            return Err(format!(
                "allow run did not print DONE:\n{}",
                format_output(&allowed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// Reads a file, then echoes its contents via Console.print — same
/// shape as `READ_ECHO_PROBE`. The MIR walker (the sole codegen path)
/// emits the `aver_replay::invoke_effect` reroute for BOTH effects.
/// `__PATH__` is substituted at test time.
const MIR_READ_ECHO_PROBE: &str = r#"module MirRwProbe
    intent =
        "Reads a file and echoes its contents. The record captures the read"
        "result; replay serves it back. Probes the MIR-emitted replay wrapper."
    effects [Console, Disk]

fn readIt(path: String) -> Result<String, String>
    ? "Reads the file at the given path."
    ! [Disk.readText]
    Disk.readText(path)

fn main() -> Result<Unit, String>
    ! [Console.print, Disk.readText]
    content = readIt("__PATH__")?
    shown = Console.print("READ:{content}")
    Result.Ok(Unit)
"#;

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --include-ignored"]
fn mir_forced_record_replay_captures_effects_through_invoke_wrapper() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced replay probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let ws = temp_dir("mir-replay");
    let data_path = ws.join("data.txt");
    fs::write(&data_path, "recorded-bytes").expect("write probe data");
    let src = ws.join("rw_probe.av");
    fs::write(
        &src,
        MIR_READ_ECHO_PROBE.replace("__PATH__", &aver_path_literal(&data_path)),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_rw_probe";

    let result = (|| -> Result<(), String> {
        // No `mir_lowered_count` guard here for the same reason as the disk
        // probe above: every fn in this probe calls a builtin, so the guard
        // can only ever fail. The structural tripwire on the emitted Rust
        // plus the capture run below are what prove the reroute was emitted.
        compile_rust_env(&src, &project, name, None, &["--with-replay"], &[])?;

        // Structural tripwire: the emitted Rust must carry the MIR-
        // emitted `invoke_effect` reroute for the read.
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_replay::invoke_effect") {
            return Err(format!(
                "emitted Rust is missing the `aver_replay::invoke_effect` reroute — \
                 the MIR effectful replay wrap was dropped:\n{emitted}"
            ));
        }

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
        if !String::from_utf8_lossy(&recorded.stdout).contains("READ:recorded-bytes") {
            return Err(format!(
                "record run did not echo the read bytes (live read broken):\n{}",
                format_output(&recorded)
            ));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }

        // BOTH effects must be captured through invoke_effect — a
        // dropped MIR replay wrapper makes one (or both) vanish.
        let session_json = fs::read_to_string(&session).expect("read session");
        if !session_json.contains("\"Disk.readText\"") {
            return Err(format!(
                "session is missing the Disk.readText effect — the MIR replay \
                 wrapper was dropped on the read:\n{session_json}"
            ));
        }
        if !session_json.contains("\"Console.print\"") {
            return Err(format!(
                "session is missing the Console.print effect — the MIR replay \
                 wrapper was dropped on the print:\n{session_json}"
            ));
        }
        if !session_json.contains("READ:recorded-bytes") {
            return Err(format!(
                "session does not carry the woven read result in the Console.print \
                 arg — per-effect arg-json shape is wrong:\n{session_json}"
            ));
        }

        // (2) REPLAY: mutate the data file so a LIVE read would differ,
        // then replay. Replay must serve the recorded bytes (not re-read
        // the mutated file) and roundtrip without a position mismatch.
        fs::write(&data_path, "MUTATED-ON-DISK").expect("mutate data file");
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded session did not roundtrip (a \
                 dropped or mis-ordered MIR invoke_effect reroute trips a position \
                 mismatch here):\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

// ─── Full tier (env-gated, #[ignore]) ───────────────────────────────────

/// Every single-file example with deterministic, build-and-run-able
/// behavior (Console-only or pure — no Time / Random / Http / Tcp /
/// Terminal, no interactive loop). Plain-parity tier.
const FULL_SINGLE_FILE: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/hello.av",
    "examples/core/lambda.av",
    "examples/core/lists.av",
    "examples/core/order_total.av",
    "examples/core/result_chain.av",
    "examples/core/result_pipeline.av",
    "examples/core/shapes.av",
    "examples/core/temperature.av",
    "examples/core/user_record.av",
    "examples/data/fibonacci.av",
    "examples/data/list_length_fold.av",
    "examples/data/map.av",
    "examples/data/quicksort.av",
    "examples/data/red_black_tree.av",
    "examples/data/rle.av",
    "examples/data/sum_acc.av",
];

/// Multi-module (`depends`) examples — (entry file, module root).
/// These exercise the cross-module path-mangling the Rust backend
/// emits (the `crate::aver_generated::<dep>::*` references). The games
/// are excluded: they're interactive Terminal loops, not batch
/// programs with deterministic stdout.
const FULL_MULTI_MODULE: &[(&str, &str)] = &[
    ("examples/modules/app.av", "examples"),
    ("examples/modules/pricing_app.av", "examples"),
];

#[test]
#[ignore = "full tier: minutes of build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn full_plain_stdout_parity_with_vm() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping full tier — set AVER_RUST_DIFF_FULL=1 to run \
             (single-file + multi-module plain parity over the corpus)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut passed = 0usize;

    for relative in FULL_SINGLE_FILE {
        match assert_plain_parity(relative, None) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }
    for (relative, root) in FULL_MULTI_MODULE {
        match assert_plain_parity(relative, Some(root)) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }

    let total = FULL_SINGLE_FILE.len() + FULL_MULTI_MODULE.len();
    eprintln!("full_plain_stdout_parity_with_vm: {passed}/{total} passed");
    assert!(
        failures.is_empty(),
        "{} of {} full-tier examples failed plain parity:\n  - {}",
        failures.len(),
        total,
        failures.join("\n  - ")
    );
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
            for capture in ["height", "blockId"] {
                let binding = format!("let {capture} = {capture}.clone();");
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
        // param (`f: fn(aver_rt::AverInt) -> aver_rt::AverInt` — `Int` lowers
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
        if !emitted.contains("f: fn(aver_rt::AverInt) -> aver_rt::AverInt") {
            return Err(format!(
                "emitted Rust is missing the fn-pointer param \
                 `f: fn(aver_rt::AverInt) -> aver_rt::AverInt` — \
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
            || !emitted.contains("&*name == \"anchor\"")
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
    Bytes.toList(Crypto.Digest32.toBytes(Crypto.sha256(Result.withDefault(Bytes.fromList(script), Bytes.fromList([])))))

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
        if !emitted.contains("f: fn(crate::aver_generated::worker::Shape) -> aver_rt::AverInt")
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

/// Regression for #1042: dependency glob imports must not turn an Aver match
/// binder into an ambiguous Rust name. Both dependencies expose `message`, but
/// the entry calls only `Legacy.message`; `Result.Ok(message)` is purely local.
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

// ═══════════════════════════════════════════════════════════════════════
// The MIR behavioral net (MIR is the sole Rust codegen path)
// ═══════════════════════════════════════════════════════════════════════
//
// The HIR `ResolvedExpr` walker was deleted in W6/Stage-3, so the MIR
// walker OWNS all runtime codegen unconditionally — there are no flags
// to set anymore. This section is the STANDING, REPEATABLE gate: build +
// run the corpus and assert the built binaries match the VM. Since every
// compile already takes the MIR path, "forced-MIR" is just "the corpus".
//
// Two discipline guards make this a real net, not theater:
//
//  - **mir_lowered > 0**: every parity assert first checks
//    `mir_lowered_count(...) > 0`, so a test cannot pass while the
//    construct under test never reached the walker (which would mean the
//    build / parity assertion validated nothing relevant).
//  - **per-test isolated target dir** (the W6 audit flakiness fix): the
//    generated-project `cargo build` runs against a UNIQUE target dir per
//    example, so concurrent `--offline` builds never corrupt each other's
//    `.rmeta` / proc-macro outputs.

/// Per-example isolated `cargo build` target dir. The forced-MIR tier
/// uses one of these per example so concurrent `--offline` builds never
/// race on a shared target tree (the `.rmeta` / proc-macro corruption hit
/// during the W6 audit). Lives next to the temp project so the temp-dir
/// cleanup reclaims it.
fn isolated_target_dir(ws: &Path) -> PathBuf {
    ws.join("cargo-target")
}

/// Compile + build + RUN an example through the (sole) MIR codegen path,
/// asserting stdout parity with the VM. Applies the mir_lowered > 0 guard
/// (the program must actually reach the walker) + a per-example isolated
/// target dir.
fn assert_forced_mir_parity(relative: &str, module_root: Option<&str>) -> Result<(), String> {
    let file = repo_root().join(relative);
    if !file.exists() {
        return Err(format!("{relative}: corpus file missing"));
    }
    let root = module_root.map(|r| repo_root().join(r));
    let vm_stdout = run_vm(&file, root.as_deref())?;

    let ws = temp_dir(&format!("fmir-{}", sanitise(relative)));
    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("fm_{}", sanitise(relative));

    let result = (|| -> Result<(), String> {
        // Guard: SOMETHING must lower to MIR (so the parity assert below
        // is validating the MIR walker's output, not an empty program).
        let lowered = mir_lowered_count(&file, root.as_deref(), &[])?;
        if lowered == 0 {
            return Err(format!(
                "{relative}: no fn lowered to MIR — the parity assert here would \
                 be validating an empty program"
            ));
        }

        compile_rust_env(&file, &project, &name, root.as_deref(), &[], &[])?;
        let bin = cargo_build_in(&project, &name, &isolated_target_dir(&ws))?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("failed to run compiled binary: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "{relative}: forced-MIR binary exited non-zero:\n{}",
                format_output(&out)
            ));
        }
        let rust_stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if rust_stdout != vm_stdout {
            return Err(format!(
                "{relative}: forced-MIR stdout mismatch\n--- VM ---\n{vm_stdout}\n--- Rust (forced MIR) ---\n{rust_stdout}"
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result
}

// ─── Oracle-shape full-corpus build (no residual Rust exclusions) ────────
//
// The Oracle-only `BranchPath` type and the Terminal-only `Terminal.Size`
// type are emitted by the Rust backend (a `pub use aver_rt::BranchPath`
// / `Terminal_Size` alias, mirroring `Tcp.Connection`), and the verify
// module skips the verify-only Oracle / trace / given-universal cases, so
// the Oracle programs (`oracle_trace`, `hostile_order_axis`,
// `clock_as_data`, `randomness_paradox`, `terminal_size_snapshot`,
// `file_store_shell`, `services/redis`) `cargo build` + `cargo test`
// cleanly on Rust and run with VM parity.
//
// The last residual was `oracle_independent_products.av`: its higher-order
// spec fn `pairSpec(path, rnd: Fn(BranchPath, …) -> Int)` builds a tuple
// from calls to the `rnd` fn-pointer param over `BranchPath.child(path, n)`
// — the MIR walker bailed because the `BranchPath.child` / `.parse` builtin
// calls and the `BranchPath.Root` nullary value had no emit arm (they fell
// through to `_ => None`, yielding a `compile_error!`). Adding those arms
// (the `aver_rt::BranchPath::{child,parse,root}` constructors) closes the
// gap — the higher-order tuple-of-LocalSlot-calls shape already emitted.
// There is now NO residual Rust-build exclusion in the example corpus.

/// (relative path, optional module root) of the Oracle-shape examples that
/// must `cargo build` cleanly on the (sole) MIR codegen path with zero
/// `compile_error!` stubs. Previously these were the *exclusions*; the
/// BranchPath builtin-emit gap is now closed, so the assertion flipped to
/// a positive build proof. A regression (a dropped `BranchPath.*` arm, an
/// undefined `BranchPath` symbol, or a re-introduced higher-order None)
/// fails the `cargo build` and this test catches it.
const BRANCH_PATH_BUILDS: &[(&str, Option<&str>)] = &[(
    "examples/formal/oracle_independent_products.av",
    Some("examples"),
)];

/// `cargo build` an example through the (sole) MIR codegen path,
/// returning Ok(()) on a clean build or Err(stderr) on failure. Used by
/// the BranchPath build proof to confirm the Oracle-shape examples build
/// on Rust without `compile_error!` stubs.
fn try_build_walker(
    relative: &str,
    module_root: Option<&str>,
    ws: &Path,
    tag: &str,
) -> Result<Result<(), String>, String> {
    let file = repo_root().join(relative);
    if !file.exists() {
        return Err(format!("{relative}: corpus file missing"));
    }
    let root = module_root.map(|r| repo_root().join(r));
    let project = ws.join(format!("project-{tag}"));
    fs::create_dir_all(&project).expect("create project dir");
    let name = format!("bp_{}_{tag}", sanitise(relative));

    // The compile (Rust source emit) may itself fail; if so that's the
    // failure to inspect. Capture it as an Err.
    if let Err(e) = compile_rust(&file, &project, &name, root.as_deref(), &[]) {
        return Ok(Err(format!("compile: {e}")));
    }
    // Guard against a silent `compile_error!` stub slipping through: any
    // such stub fails `cargo build` below, but assert explicitly too so a
    // regression names the exact failure mode.
    let emitted = fs::read_to_string(
        project
            .join("src")
            .join("aver_generated")
            .join("entry")
            .join("mod.rs"),
    )
    .map_err(|e| format!("read emitted module: {e}"))?;
    if emitted.contains("compile_error!") {
        return Ok(Err(
            "emitted Rust still carries a `compile_error!` stub — a fn body \
             the MIR walker could not render"
                .to_string(),
        ));
    }
    match cargo_build_in(&project, &name, &ws.join(format!("target-{tag}"))) {
        Ok(_) => Ok(Ok(())),
        Err(e) => Ok(Err(e)),
    }
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn oracle_shape_examples_build_clean_on_rust() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping Oracle-shape build proof — set AVER_RUST_DIFF_FULL=1 \
             (proves the higher-order BranchPath Oracle example builds clean on Rust)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut confirmed = 0usize;

    for (relative, root) in BRANCH_PATH_BUILDS {
        let ws = temp_dir(&format!("bp-{}", sanitise(relative)));
        let built = try_build_walker(relative, *root, &ws, "mir");
        let _ = fs::remove_dir_all(&ws);

        match built {
            Ok(Ok(())) => confirmed += 1,
            Ok(Err(err)) => failures.push(format!(
                "{relative}: expected a clean Rust build (BranchPath builtin-emit \
                 gap is closed), but it failed:\n  err:\n{err}"
            )),
            Err(e) => failures.push(format!("{relative}: harness error: {e}")),
        }
    }

    eprintln!(
        "oracle_shape_examples_build_clean_on_rust: {confirmed}/{} built clean",
        BRANCH_PATH_BUILDS.len()
    );
    assert!(
        failures.is_empty(),
        "{} of {} Oracle-shape examples did not build clean:\n  - {}",
        failures.len(),
        BRANCH_PATH_BUILDS.len(),
        failures.join("\n  - ")
    );
}

// ─── Forced-MIR full-corpus run-parity tier (env-gated) ──────────────────

/// Single-file examples that build + run deterministically under
/// forced-MIR (Console-only or pure — no live Time / Random / Http / Tcp /
/// Terminal in the run path, no interactive loop). Empirically verified
/// to build + run with VM parity under all four MIR flags. This is the
/// honest buildable denominator: the BranchPath set is excluded above.
const FORCED_MIR_SINGLE_FILE: &[&str] = &[
    "examples/core/calculator.av",
    "examples/core/hello.av",
    "examples/core/lambda.av",
    "examples/core/lists.av",
    "examples/core/order_total.av",
    "examples/core/result_chain.av",
    "examples/core/result_pipeline.av",
    "examples/core/shapes.av",
    "examples/core/temperature.av",
    "examples/core/user_record.av",
    "examples/core/effects_explicit.av",
    // NOTE: examples/core/grok_s_language.av is deliberately EXCLUDED — it
    // is an interactive `Console.readLine` REPL loop, not a batch program;
    // with stdin closed it spins on EOF and the VM oracle itself never
    // terminates. Interactive examples are not run-parity candidates (same
    // reason the games are excluded).
    "examples/data/fibonacci.av",
    "examples/data/list_length_fold.av",
    "examples/data/map.av",
    "examples/data/quicksort.av",
    "examples/data/red_black_tree.av",
    "examples/data/rle.av",
    "examples/data/sum_acc.av",
    "examples/data/json.av",
    // Pure / proof-shaped formal examples that DO build + run in Rust
    // (they never reach the Oracle / trace runtime API, so no BranchPath
    // gap). Empirically VM-parity under forced-MIR (graduated 2/4/6/2).
    "examples/formal/file_store_pure_core.av",
    "examples/formal/spec_laws.av",
    "examples/formal/law_auto.av",
    "examples/formal/trust_check.av",
];

/// Multi-module (`depends`) examples — (entry file, module root). These
/// exercise the cross-module path-mangling the Rust backend emits, with
/// `main` + top-level-statement values routed through MIR (the sole
/// codegen path). The games are excluded: every one is an interactive
/// Terminal loop (Terminal is host territory + no deterministic batch
/// stdout), so they are neither buildable in Rust nor run-parity
/// candidates.
const FORCED_MIR_MULTI_MODULE: &[(&str, &str)] = &[
    ("examples/modules/app.av", "examples"),
    ("examples/modules/app_dot.av", "examples"),
    ("examples/modules/pricing_app.av", "examples"),
];

#[test]
#[ignore = "full tier: minutes of build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn forced_mir_full_corpus_parity_with_vm() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!(
            "skipping forced-MIR full-corpus tier — set AVER_RUST_DIFF_FULL=1 to run \
             (all four MIR flags; single-file + multi-module run-parity over the \
             buildable corpus, with the graduated>0 guard)"
        );
        return;
    }

    let mut failures = Vec::new();
    let mut passed = 0usize;

    for relative in FORCED_MIR_SINGLE_FILE {
        match assert_forced_mir_parity(relative, None) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }
    for (relative, root) in FORCED_MIR_MULTI_MODULE {
        match assert_forced_mir_parity(relative, Some(root)) {
            Ok(()) => passed += 1,
            Err(e) => failures.push(e),
        }
    }

    let total = FORCED_MIR_SINGLE_FILE.len() + FORCED_MIR_MULTI_MODULE.len();
    eprintln!(
        "forced_mir_full_corpus_parity_with_vm: {passed}/{total} passed (all four MIR flags)"
    );
    assert!(
        failures.is_empty(),
        "{} of {} forced-MIR corpus examples failed parity:\n  - {}",
        failures.len(),
        total,
        failures.join("\n  - ")
    );
}

// ─── Forced-MIR effect-mode probes (Time / Random / Tcp) ─────────────────
//
// Wave 3b lets the MIR walker emit effectful builtins (the policy /
// replay / bare-framing wrappers). Disk is covered by the deny-policy +
// record/replay probes above. These probes extend that coverage to
// Time / Random (deterministic via record->replay: the live record
// captures the nondeterministic value, replay serves it back) and to a
// Tcp shape (a real loopback listener for the record, then replay serves
// it back so the assertion is listener-independent). Each rides the
// effect on a HELPER fn so it graduates onto MIR (graduated > 0 guard) and
// asserts the MIR-emitted `aver_replay::invoke_effect` reroute captures
// the effect with the right per-effect arg-json shape.

/// Time + Random through helper fns (so they graduate). `Random.int`
/// rides `rollDie`, `Time.unixMs` rides `stamp`.
const MIR_TIME_RANDOM_PROBE: &str = r#"module MirTimeRandomProbe
    intent =
        "Draws a random int and reads a unix-ms timestamp through helper fns,"
        "echoes both. record captures the nondeterministic effects; replay"
        "serves them back. Probes the MIR-emitted Time / Random replay wrap."
    effects [Console, Random, Time]

fn rollDie(lo: Int, hi: Int) -> Int
    ? "Draws a random int in the inclusive range."
    ! [Random.int]
    Random.int(lo, hi)

fn stamp() -> Int
    ? "Reads the current unix-ms timestamp."
    ! [Time.unixMs]
    Time.unixMs()

fn main() -> Unit
    ! [Console.print, Random.int, Time.unixMs]
    r = rollDie(1, 100)
    t = stamp()
    Console.print("r={r} t={t}")
"#;

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn mir_forced_time_random_record_replay_roundtrips() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced Time/Random probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let ws = temp_dir("mir-timerand");
    let src = ws.join("time_random_probe.av");
    fs::write(&src, MIR_TIME_RANDOM_PROBE).expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_time_random_probe";

    let result = (|| -> Result<(), String> {
        compile_rust_env(&src, &project, name, None, &["--with-replay"], &[])?;

        // Structural tripwire: standard Time/Random calls must use the typed
        // capability replay door, while legacy Console keeps invoke_effect.
        // Do not use
        // `mir_lowered_count` here: its test emission context deliberately has
        // no builtin table, so every effect helper reports as a fallback (see
        // the helper's BLIND SPOT comment above).
        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_replay::invoke_capability_effect") {
            return Err(format!(
                "emitted Rust is missing the `aver_replay::invoke_capability_effect` reroute — \
                 the standard capability replay wrap was dropped:\n{emitted}"
            ));
        }
        if !emitted.contains("aver_replay::invoke_effect") {
            return Err(format!(
                "emitted Rust is missing the legacy `aver_replay::invoke_effect` reroute for Console:\n{emitted}"
            ));
        }

        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;

        // (1) RECORD: run live (nondeterministic), capturing into a session.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }
        // The recorded run prints the live, nondeterministic `r=N t=M`
        // line — capture it so we can prove replay serves THOSE exact
        // values back (not a fresh roll / clock read).
        let recorded_stdout = String::from_utf8_lossy(&recorded.stdout).trim().to_string();
        let recorded_line = recorded_stdout
            .lines()
            .find(|l| l.starts_with("r="))
            .ok_or_else(|| format!("record run had no `r=` line:\n{recorded_stdout}"))?
            .to_string();

        // BOTH nondeterministic effects must be captured through
        // their respective replay doors — a dropped MIR wrapper makes one vanish.
        let session_json = fs::read_to_string(&session).expect("read session");
        for effect in ["\"Random.int\"", "\"Time.unixMs\"", "\"Console.print\""] {
            if !session_json.contains(effect) {
                return Err(format!(
                    "session is missing the {effect} effect — the MIR replay wrapper \
                     was dropped on it:\n{session_json}"
                ));
            }
        }
        // The recorded Random / Time values flowed through into the
        // interpolated Console.print arg — the session must carry that
        // exact `r=N t=M` string (per-effect arg-json shape). This is the
        // determinism evidence: the captured value is pinned in the
        // session, so replay has the recorded draw to serve back rather
        // than re-rolling.
        if !session_json.contains(&recorded_line) {
            return Err(format!(
                "session does not carry the recorded `{recorded_line}` in the \
                 Console.print arg — the woven Random/Time values were not captured \
                 (per-effect arg-json shape is wrong):\n{session_json}"
            ));
        }

        // (2) REPLAY: roundtrip the recorded session. Replay serves the
        // recorded Random / Time draws back (NOT a fresh roll / clock
        // read); a dropped or mis-ordered MIR invoke_effect reroute trips
        // a position mismatch and the run fails here.
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded Time/Random session did not \
                 roundtrip (a dropped / mis-ordered MIR invoke_effect reroute trips a \
                 position mismatch here):\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

/// A Tcp.send shape through a helper fn (so it graduates). `__PORT__` is
/// substituted with a real loopback listener port at test time.
const MIR_TCP_PROBE: &str = r#"module MirTcpProbe
    intent =
        "Sends a line over TCP through a helper fn and echoes the reply. The"
        "helper rides Tcp.send so it graduates onto the MIR path. record"
        "captures the exchange; replay serves it back."
    effects [Console, Tcp]

fn ask(host: String, port: Int, msg: String) -> Result<String, String>
    ? "Open a TCP connection, write msg, read the reply, close."
    ! [Tcp.send]
    Tcp.send(host, port, msg)

fn main() -> Unit
    ! [Console.print, Tcp.send]
    match ask("127.0.0.1", __PORT__, "ping")
        Result.Ok(r) -> Console.print("got:{r}")
        Result.Err(e) -> Console.print("err:{e}")
"#;

/// Spawn a single-threaded loopback TCP server that replies `pong\n` to
/// every connection. Returns the bound port + a join handle that exits
/// once `stop` is set. No external process / Python — pure std, so it is
/// deterministic and dependency-free in CI.
fn spawn_pong_server(
    stop: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> (u16, std::thread::JoinHandle<()>) {
    use std::io::{Read, Write};
    use std::net::TcpListener;
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener addr").port();
    listener
        .set_nonblocking(true)
        .expect("set listener nonblocking");
    let handle = std::thread::spawn(move || {
        while !stop.load(Ordering::Relaxed) {
            match listener.accept() {
                Ok((mut conn, _)) => {
                    // The accepted stream can inherit the listener's
                    // non-blocking mode on some platforms — force it back
                    // to BLOCKING so the read below actually waits for the
                    // client's bytes instead of returning WouldBlock and
                    // racing the connection closed (the "connection reset
                    // by peer" the first cut hit).
                    let _ = conn.set_nonblocking(false);
                    let _ = conn.set_read_timeout(Some(std::time::Duration::from_secs(2)));
                    // `Tcp.send` writes one request line then half-closes
                    // its write side; read until EOF / the request line so
                    // we don't reply before the request lands.
                    let mut buf = [0u8; 256];
                    let _ = conn.read(&mut buf);
                    let _ = conn.write_all(b"pong\n");
                    let _ = conn.flush();
                    // Give the client time to read the reply before the
                    // stream drops (drop closes the socket).
                    let _ = conn.shutdown(std::net::Shutdown::Write);
                    let mut drain = [0u8; 16];
                    let _ = conn.read(&mut drain);
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
                Err(_) => break,
            }
        }
    });
    (port, handle)
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn rust_tcp_send_bytes_round_trips_non_utf8() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping Rust Tcp.sendBytes probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    use std::io::{Read, Write};
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener addr").port();
    listener
        .set_nonblocking(true)
        .expect("set listener nonblocking");

    let ws = temp_dir("tcp-send-bytes");
    let src = ws.join("tcp_send_bytes.av");
    fs::write(
        &src,
        format!(
            r#"module TcpSendBytesProbe
    intent = "Round-trip non-UTF-8 bytes through the Rust backend"
    depends [Bytes]
    effects [Console, Tcp]

fn exchange() -> Result<Bytes, String>
    ? "Send one binary payload to a loopback echo server."
    ! [Tcp.sendBytes]
    payload = Bytes.fromList([249, 190, 180, 217])
    Tcp.sendBytes("127.0.0.1", {port}, payload)

fn main() -> Unit
    ! [Console.print, Tcp.sendBytes]
    match exchange()
        Result.Ok(response) -> Console.print("{{Bytes.toList(response)}}")
        Result.Err(e) -> Console.print("err:{{e}}")
"#
        ),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "tcp_send_bytes_probe";

    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;

        let server = std::thread::spawn(move || -> Result<Vec<u8>, String> {
            let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
            loop {
                match listener.accept() {
                    Ok((mut stream, _)) => {
                        stream
                            .set_nonblocking(false)
                            .map_err(|e| format!("set stream blocking: {e}"))?;
                        stream
                            .set_read_timeout(Some(std::time::Duration::from_secs(2)))
                            .map_err(|e| format!("set read timeout: {e}"))?;
                        let mut payload = Vec::new();
                        stream
                            .read_to_end(&mut payload)
                            .map_err(|e| format!("read payload: {e}"))?;
                        stream
                            .write_all(&payload)
                            .map_err(|e| format!("echo payload: {e}"))?;
                        return Ok(payload);
                    }
                    Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        if std::time::Instant::now() >= deadline {
                            return Err(
                                "timed out waiting for Tcp.sendBytes connection".to_string()
                            );
                        }
                        std::thread::sleep(std::time::Duration::from_millis(5));
                    }
                    Err(e) => return Err(format!("accept connection: {e}")),
                }
            }
        });

        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        let received = server
            .join()
            .map_err(|_| "loopback server panicked".to_string())??;
        if !out.status.success() {
            return Err(format!("generated binary failed:\n{}", format_output(&out)));
        }
        if received != [249, 190, 180, 217] {
            return Err(format!(
                "loopback server received wrong payload: {received:?}"
            ));
        }
        let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if stdout != "[249, 190, 180, 217]" {
            return Err(format!(
                "generated binary returned wrong payload:\n{}",
                format_output(&out)
            ));
        }
        Ok(())
    })();

    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|e| panic!("{e}"));
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn rust_tcp_read_bytes_round_trips_non_utf8() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping Rust Tcp.readBytes probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    use std::io::Write;
    use std::net::TcpListener;

    let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback listener");
    let port = listener.local_addr().expect("listener addr").port();
    listener
        .set_nonblocking(true)
        .expect("set listener nonblocking");
    let server = std::thread::spawn(move || -> Result<(), String> {
        // The generated project's first cargo build happens after this thread
        // starts and can take several seconds on a cold CI runner.
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(60);
        loop {
            match listener.accept() {
                Ok((mut stream, _)) => {
                    return stream
                        .write_all(&[249, 190, 180, 217])
                        .map_err(|e| format!("write binary frame: {e}"));
                }
                Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    if std::time::Instant::now() >= deadline {
                        return Err("timed out waiting for Tcp.readBytes connection".to_string());
                    }
                    std::thread::sleep(std::time::Duration::from_millis(5));
                }
                Err(e) => return Err(format!("accept Tcp.readBytes: {e}")),
            }
        }
    });

    let ws = temp_dir("tcp-read-bytes");
    let src = ws.join("tcp_read_bytes.av");
    fs::write(
        &src,
        format!(
            r#"module TcpReadBytesProbe
    intent = "Read non-UTF-8 bytes through the Rust backend"
    depends [Bytes]
    effects [Console, Tcp]

fn readFrame(conn: Tcp.Connection) -> Unit
    ! [Tcp.readBytes, Console.print]
    match Tcp.readBytes(conn, 4)
        Result.Ok(frame) -> Console.print("{{Bytes.toList(frame)}}")
        Result.Err(e) -> Console.print("err:{{e}}")

fn main() -> Unit
    ! [Tcp.connect, Tcp.readBytes, Console.print]
    match Tcp.connect("127.0.0.1", {port})
        Result.Ok(conn) -> readFrame(conn)
        Result.Err(e) -> Console.print("connect:{{e}}")
"#
        ),
    )
    .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "tcp_read_bytes_probe";
    let result = (|| -> Result<(), String> {
        compile_rust(&src, &project, name, None, &[])?;
        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;
        let out = Command::new(&bin)
            .output()
            .map_err(|e| format!("run generated binary: {e}"))?;
        server
            .join()
            .map_err(|_| "loopback server panicked".to_string())??;
        if !out.status.success() {
            return Err(format!("generated binary failed:\n{}", format_output(&out)));
        }
        let stdout = String::from_utf8_lossy(&out.stdout).trim().to_string();
        if stdout != "[249, 190, 180, 217]" {
            return Err(format!("unexpected stdout: {stdout:?}"));
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.expect("Rust Tcp.readBytes round-trip");
}

#[test]
#[ignore = "full tier: cargo build wall-time; set AVER_RUST_DIFF_FULL=1 and run with --ignored"]
fn mir_forced_tcp_send_record_replay_roundtrips() {
    if std::env::var("AVER_RUST_DIFF_FULL").is_err() {
        eprintln!("skipping MIR-forced Tcp probe — set AVER_RUST_DIFF_FULL=1");
        return;
    }

    let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let (port, server) = spawn_pong_server(stop.clone());

    let ws = temp_dir("mir-tcp");
    let src = ws.join("tcp_probe.av");
    fs::write(&src, MIR_TCP_PROBE.replace("__PORT__", &port.to_string()))
        .expect("write probe source");

    let project = ws.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let name = "mir_tcp_probe";

    let result = (|| -> Result<(), String> {
        // (0) PROVE the MIR path is exercised: `ask` must lower to MIR.
        let lowered = mir_lowered_count(&src, None, &["--with-replay"])?;
        if lowered == 0 {
            return Err(
                "no fn lowered to MIR — the Tcp replay reroute is not being exercised".to_string(),
            );
        }

        compile_rust_env(&src, &project, name, None, &["--with-replay"], &[])?;

        let emitted = fs::read_to_string(
            project
                .join("src")
                .join("aver_generated")
                .join("entry")
                .join("mod.rs"),
        )
        .map_err(|e| format!("read emitted module: {e}"))?;
        if !emitted.contains("aver_replay::invoke_effect") {
            return Err(format!(
                "emitted Rust is missing the `aver_replay::invoke_effect` reroute — \
                 the MIR Tcp replay wrap was dropped:\n{emitted}"
            ));
        }

        let bin = cargo_build_in(&project, name, &isolated_target_dir(&ws))?;

        // (1) RECORD: live against the loopback listener.
        let session = ws.join("session.json");
        let recorded = Command::new(&bin)
            .env("AVER_REPLAY_RECORD", &session)
            .output()
            .map_err(|e| format!("run record binary: {e}"))?;
        if !recorded.status.success() {
            return Err(format!("record run failed:\n{}", format_output(&recorded)));
        }
        if !String::from_utf8_lossy(&recorded.stdout).contains("got:pong") {
            return Err(format!(
                "record run did not echo the listener reply (live Tcp.send broken):\n{}",
                format_output(&recorded)
            ));
        }
        if !session.exists() {
            return Err("record run did not write the session JSON".to_string());
        }
        let session_json = fs::read_to_string(&session).expect("read session");
        for effect in ["\"Tcp.send\"", "\"Console.print\""] {
            if !session_json.contains(effect) {
                return Err(format!(
                    "session is missing the {effect} effect — the MIR replay wrapper \
                     was dropped on it:\n{session_json}"
                ));
            }
        }
        if !session_json.contains("pong") {
            return Err(format!(
                "session does not carry the recorded `pong` reply — the per-effect \
                 Tcp arg-json shape is wrong:\n{session_json}"
            ));
        }

        // (2) REPLAY: stop the listener first so a LIVE Tcp.send would
        // FAIL with a connection error — replay must instead serve the
        // recorded reply from the session (listener-independent) and
        // roundtrip without a position mismatch. A dropped MIR Tcp replay
        // reroute would re-attempt a live connection to the now-dead
        // listener and the run would fail here.
        stop.store(true, Ordering::Relaxed);
        let replayed = Command::new(&bin)
            .env("AVER_REPLAY_REPLAY", &session)
            .output()
            .map_err(|e| format!("run replay binary: {e}"))?;
        if !replayed.status.success() {
            return Err(format!(
                "replay run failed — the recorded Tcp session did not roundtrip (the \
                 MIR Tcp replay reroute likely re-tried a live connection to the \
                 stopped listener instead of serving the recorded reply):\n{}",
                format_output(&replayed)
            ));
        }
        Ok(())
    })();

    stop.store(true, Ordering::Relaxed);
    let _ = server.join();
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
