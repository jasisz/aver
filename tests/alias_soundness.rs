//! Soundness regression net for the alias-slot annotation pass
//! (`ir::alias`), which decides when a `Vector` / `Map` local is safe for
//! the VM's owned in-place fast path (`VECTOR_SET_OR_KEEP` /
//! `compute_builtin_owned_mask`).
//!
//! Issue #410: the pass flagged the *destination* of an alias but not the
//! *source*. When a collection local's handle is retained by another live
//! binding — a rename `b = a`, a match arm tail, an aggregate member, a
//! builtin value-arg, or an arg to a user fn that returns it — the source
//! must NOT take the owned path, or the in-place mutation silently rewrites
//! the other binding. Every shape below corrupted on the VM (both with and
//! without `own_param`, since these are plain `main` locals, not params)
//! before the fix; the self-hosted interpreter (immutable by construction)
//! was always correct and is the oracle here.
//!
//! Each test fails on the pre-fix `ir::alias` (VM yields the mutated value);
//! the trailing `rebuild` test pins that the receiver / self-keep fast path
//! is preserved, so the fix can't pass by flagging everything.

use std::fs;
use std::process::Command;

#[derive(Clone, Copy)]
enum Mode {
    /// VM, `own_param` on (default).
    Vm,
    /// VM, `own_param` off — exercises `ir::alias` flags without the param
    /// refinement that incidentally masks some source-aliasing bugs.
    VmNoOwnParam,
    /// Self-hosted interpreter — immutable by construction, the oracle.
    SelfHost,
}

fn run(name: &str, source: &str, mode: Mode) -> String {
    let dir = std::env::temp_dir().join(format!("aver_alias_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).expect("create temp dir");
    let file = dir.join(format!("{name}.av"));
    fs::write(&file, source).expect("write source");

    let mut cmd = Command::new(env!("CARGO_BIN_EXE_aver"));
    cmd.arg("run");
    if matches!(mode, Mode::SelfHost) {
        cmd.arg("--self-host");
    }
    cmd.arg(&file);
    if matches!(mode, Mode::VmNoOwnParam) {
        cmd.env("AVER_NO_OWN_PARAM", "1");
    }
    let out = cmd.output().expect("spawn aver run");
    assert!(
        out.status.success(),
        "`aver run {}` failed: {}",
        file.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = fs::remove_dir_all(&dir);
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// The VM (owned fast path, `own_param` on AND off) must agree with the
/// self-hosted interpreter (immutable model) and the hand-computed value.
/// The off run matters: `own_param` masks some source-aliasing corruptions
/// (e.g. the user-fn passthrough), so the on run alone wouldn't catch them.
fn assert_immutable(name: &str, source: &str, expected: &str) {
    let host = run(name, source, Mode::SelfHost);
    assert_eq!(
        host, expected,
        "{name}: self-host (immutable oracle) must produce the model value"
    );
    for mode in [Mode::Vm, Mode::VmNoOwnParam] {
        let vm = run(name, source, mode);
        assert_eq!(
            vm, expected,
            "{name}: VM diverged from the immutable model — an aliased source \
             was own-mutated in place ({vm} != {expected})"
        );
    }
}

#[test]
fn rename_source_is_not_mutated_in_place() {
    assert_immutable(
        "rename",
        r#"fn main() -> Unit
  ! [Console.print]
  base = {1 => 7}
  snap = base
  arg = Map.set(base, 1, 500)
  Console.print("{Option.withDefault(Map.get(snap, 1), 0 - 1)}")
"#,
        "7",
    );
}

#[test]
fn user_fn_passthrough_source_is_not_mutated_in_place() {
    assert_immutable(
        "userfn",
        r#"fn idMap(m: Map<Int, Int>) -> Map<Int, Int>
  ? "passthrough returns its arg"
  m

fn main() -> Unit
  ! [Console.print]
  base = {1 => 7}
  snap = idMap(base)
  arg = Map.set(base, 1, 500)
  Console.print("{Option.withDefault(Map.get(snap, 1), 0 - 1)}")
"#,
        "7",
    );
}

#[test]
fn match_arm_passthrough_source_is_not_mutated_in_place() {
    assert_immutable(
        "matcharm",
        r#"fn main() -> Unit
  ! [Console.print]
  base = {1 => 7}
  snap = match true
    true -> base
    false -> base
  arg = Map.set(base, 1, 500)
  Console.print("{Option.withDefault(Map.get(snap, 1), 0 - 1)}")
"#,
        "7",
    );
}

#[test]
fn list_capture_source_is_not_mutated_in_place() {
    assert_immutable(
        "listcap",
        r#"fn main() -> Unit
  ! [Console.print]
  base = {1 => 7}
  held = [base]
  arg = Map.set(base, 1, 500)
  snap = match held
    [] -> {0 => 0}
    [h, ..t] -> h
  Console.print("{Option.withDefault(Map.get(snap, 1), 0 - 1)}")
"#,
        "7",
    );
}

#[test]
fn vector_rename_source_is_not_mutated_in_place() {
    assert_immutable(
        "vecrename",
        r#"fn main() -> Unit
  ! [Console.print]
  base = Vector.new(2, 7)
  snap = base
  arg = Option.withDefault(Vector.set(base, 0, 500), base)
  Console.print("{Option.withDefault(Vector.get(snap, 0), 0 - 1)}")
"#,
        "7",
    );
}

#[test]
fn map_value_capture_source_is_not_mutated_in_place() {
    assert_immutable(
        "mapvalcap",
        r#"fn main() -> Unit
  ! [Console.print]
  base = {1 => 7}
  holder = {0 => base}
  arg = Map.set(base, 1, 500)
  snap = Option.withDefault(Map.get(holder, 0), {9 => 9})
  Console.print("{Option.withDefault(Map.get(snap, 1), 0 - 1)}")
"#,
        "7",
    );
}

// --- value-OUT-of-aggregate (the mirror direction, found by the adversarial
// audit): a whole collection EXTRACTED from an aggregate into a local aliases
// the aggregate's arena entry, so own-mutating the local clobbers the
// aggregate. Closed by the fresh-producer whitelist (extraction is not fresh).

#[test]
fn record_field_extraction_is_not_mutated_in_place() {
    assert_immutable(
        "fieldextract",
        r#"record Box
  held: Map<Int, Int>

fn main() -> Unit
  ! [Console.print]
  inner = {8 => 110}
  b = Box(held = inner)
  extracted = b.held
  arg = Map.set(extracted, 8, 999)
  Console.print("{Option.withDefault(Map.get(b.held, 8), 0 - 1)}")
"#,
        "110",
    );
}

#[test]
fn map_value_extraction_is_not_mutated_in_place() {
    assert_immutable(
        "mapvalextract",
        r#"fn main() -> Unit
  ! [Console.print]
  inner = {8 => 110}
  holder = {0 => inner}
  extracted = Option.withDefault(Map.get(holder, 0), {9 => 9})
  arg = Map.set(extracted, 8, 999)
  reread = Option.withDefault(Map.get(holder, 0), {9 => 9})
  v = Option.withDefault(Map.get(reread, 8), 0 - 1)
  Console.print("{v}")
"#,
        "110",
    );
}

/// A list view must not become a window onto a later write.
///
/// This harness runs the VM variants only (see `assert_immutable`), so what it
/// pins is the ARENA side of #913: `Arena::list_drop` hands back a shared body
/// at an advanced offset, and an in-place `Vector.set` after the view is taken
/// must copy rather than show through. The aver-rt side of the same mechanism
/// (`AverList::drop_first` over the allocation `Vector::to_list` handed over)
/// is pinned by `writing_to_a_vector_does_not_reach_a_view_sharing_its_allocation`
/// in aver-rt, which this test does not reach. Nothing here is a claim about
/// the pass this file covers — it is the control that says the extra sharing
/// did not buy a program a look at a value that was never in the list it took
/// the view from.
#[test]
fn a_dropped_view_does_not_see_a_write_to_the_vector_it_came_from() {
    assert_immutable(
        "dropview",
        r#"fn total(xs: List<Int>, acc: Int) -> Int
  ? "add the elements"
  match xs
    [] -> acc
    [h, ..t] -> total(t, acc + h)

fn main() -> Unit
  ! [Console.print]
  base = Vector.new(4, 7)
  xs = List.fromVector(base)
  ys = List.drop(xs, 2)
  arg = Option.withDefault(Vector.set(base, 3, 500), base)
  Console.print("{total(ys, 0)}")
"#,
        "14",
    );
}

/// Non-vacuity: the receiver / self-keep rebuild idiom must stay correct
/// (and eligible for the owned path) — the fix must not flag everything.
#[test]
fn self_keep_rebuild_is_preserved() {
    assert_immutable(
        "rebuild",
        r#"fn main() -> Unit
  ! [Console.print]
  v = Vector.new(3, 0)
  v1 = Option.withDefault(Vector.set(v, 0, 10), v)
  v2 = Option.withDefault(Vector.set(v1, 1, 20), v1)
  v3 = Option.withDefault(Vector.set(v2, 2, 30), v2)
  Console.print("{Option.withDefault(Vector.get(v3, 1), 0 - 1)}")
"#,
        "20",
    );
}
