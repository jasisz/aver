//! Frame-boundary soundness for the VM's owned in-place vector write.
//!
//! `VECTOR_SET_OR_KEEP`'s owned branch (`src/vm/execute/dispatch.rs`) stores a
//! value into an EXISTING arena slot and hands the same handle back. When the
//! vector was allocated by an outer frame, that slot lives BELOW the current
//! frame's marks while the value just written lives ABOVE them — the frame
//! boundary must therefore rewrite the below-mark slot before it truncates its
//! own regions.
//!
//! Two return paths skip that rewrite and drop the freshly written element:
//!
//! - `Arena::evacuate_local_root` (`aver-memory/src/memory.rs`) returns an
//!   out-of-region root untouched instead of descending into it, unlike its
//!   siblings `rewrite_young_refs_in_place` (relocation) and
//!   `rewrite_promoted_young_refs_in_place` (promotion), which both do descend;
//! - `VM::can_fast_return_with_young_truncate` (`src/vm/execute/boundary.rs`)
//!   decides "nothing frame-local escapes" from the RESULT HANDLE alone and
//!   truncates young with no rewrite at all.
//!
//! The write is also not always done by the frame whose boundary drops the
//! value: a callee can store a string its caller allocated, and the callee's
//! own return may drop nothing at all. The flag therefore travels to the caller
//! on return, which
//! `an_inherited_in_place_write_survives_the_callers_boundary` pins.
//!
//! Each test below prints a value the program wrote into the vector; the VM
//! prints a recycled arena entry instead (or panics with an out-of-bounds arena
//! index when the stale index falls past the truncated region). The
//! self-hosted interpreter — immutable by construction — is correct on all of
//! them.

use std::fs;
use std::process::Command;

fn run_vm(name: &str, source: &str) -> String {
    let dir = std::env::temp_dir().join(format!("aver_frame_boundary_{name}"));
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
        "`aver run {}` failed (a stale arena index panics when it falls past \
         the truncated region): {}",
        file.display(),
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = fs::remove_dir_all(&dir);
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// The tail-call boundary evacuates the frame (`evacuate_frame_to_yard`) with
/// the caller's vector as a root. The vector's arena slot is BELOW the frame's
/// young mark, so `evacuate_local_root` returns it untouched — and the strings
/// it was just given, which live ABOVE the mark, are truncated away.
///
/// This is also the test that catches clearing `inplace_write_escaped` when the
/// tail call reuses the frame, alongside the dirty bits: the survivors that
/// boundary compacts land above `yard_base`, which the frame's own RETURN
/// truncates to, so the obligation is still outstanding and clearing it here
/// loses an element three iterations later — as an out-of-bounds arena index
/// rather than a wrong string, since the stale index falls past the region.
#[test]
fn tail_call_evacuation_keeps_the_element_written_in_place() {
    let src = r#"module FrameBoundaryTail
    intent = "in-place write into a caller-owned vector must survive the tail-call boundary"
    depends []
    effects [Console]

fn fill(v: Vector<String>, i: Int, tag: String) -> Vector<String>
    ? "tail-recursive in-place fill with freshly built heap strings"
    match i >= 3
        true -> v
        false -> fill(Option.withDefault(Vector.set(v, i, String.toUpper("element-{i}-marker")), v), i + 1, String.toUpper(tag))

fn main() -> Unit
    ! [Console.print]
    w = fill(Vector.new(3, "placeholder-string"), 0, "tag-value-long")
    j1 = String.toUpper("junk-filler-one")
    j2 = String.toUpper("junk-filler-two")
    j3 = String.toUpper("junk-filler-three")
    Console.print("{Option.withDefault(Vector.get(w, 0), "MISSING")} {Option.withDefault(Vector.get(w, 1), "MISSING")} {Option.withDefault(Vector.get(w, 2), "MISSING")}")
    Console.print("{String.len(j1)} {String.len(j2)} {String.len(j3)}")
"#;
    assert_eq!(
        run_vm("tail_evacuation", src),
        "ELEMENT-0-MARKER ELEMENT-1-MARKER ELEMENT-2-MARKER\n15 15 17",
        "the frame boundary dropped the elements written in place into the \
         caller's vector"
    );
}

/// The plain return boundary takes the young-truncate fast path: the result
/// handle (the caller's vector) is not frame-local, so the frame's whole young
/// region is truncated — including the string the frame had just stored INTO
/// that vector.
#[test]
fn young_truncate_fast_return_keeps_the_element_written_in_place() {
    let src = r#"module FrameBoundaryReturn
    intent = "in-place write into a caller-owned vector must survive an ordinary return"
    depends []
    effects [Console]

fn mkTail(n: Int) -> String
    ? "build a heap string"
    "tail-marker-{n}-endmarker"

fn touch(v: Vector<String>, n: Int) -> Vector<String>
    ? "own-mutate the caller's vector with a value this frame allocated"
    a = mkTail(n)
    b = String.toUpper(a)
    Option.withDefault(Vector.set(v, 0, b), v)

fn main() -> Unit
    ! [Console.print]
    w = touch(Vector.new(3, "placeholder-string"), 7)
    j1 = String.toUpper("junk-filler-one")
    j2 = String.toUpper("junk-filler-two")
    j3 = String.toUpper("junk-filler-three")
    Console.print("{Option.withDefault(Vector.get(w, 0), "MISSING")}")
    Console.print("{String.len(j1)} {String.len(j2)} {String.len(j3)}")
"#;
    assert_eq!(
        run_vm("young_truncate_return", src),
        "TAIL-MARKER-7-ENDMARKER\n15 15 17",
        "the young-truncate fast return dropped the element written in place \
         into the caller's vector"
    );
}

/// Not a String-only shape: a `Vector<Cell>` built by `Vector.fromList`
/// graduates the same way and loses its records just as silently.
#[test]
fn record_elements_written_in_place_survive_the_boundary() {
    let src = r#"module FrameBoundaryRecord
    intent = "record elements written in place must survive the frame boundary"
    depends []
    effects [Console]

record Cell
    tag: Int
    name: String

fn fill(v: Vector<Cell>, i: Int, tag: String) -> Vector<Cell>
    ? "tail-recursive in-place fill with freshly built records"
    match i >= 3
        true -> v
        false -> fill(Option.withDefault(Vector.set(v, i, Cell(tag = i * 100, name = "cell")), v), i + 1, String.toUpper(tag))

fn tagOf(c: Cell) -> Int
    ? "read the tag"
    c.tag

fn main() -> Unit
    ! [Console.print]
    w = fill(Vector.fromList([Cell(tag = 0 - 1, name = "a"), Cell(tag = 0 - 2, name = "b"), Cell(tag = 0 - 3, name = "c")]), 0, "tag-value-long")
    j1 = Cell(tag = 777, name = "junk-one")
    j2 = Cell(tag = 778, name = "junk-two")
    j3 = Cell(tag = 779, name = "junk-three")
    Console.print("{tagOf(Option.withDefault(Vector.get(w, 0), j1))} {tagOf(Option.withDefault(Vector.get(w, 1), j1))} {tagOf(Option.withDefault(Vector.get(w, 2), j1))}")
    Console.print("{tagOf(j1)} {tagOf(j2)} {tagOf(j3)}")
"#;
    assert_eq!(
        run_vm("record_elements", src),
        "0 100 200\n777 778 779",
        "the frame boundary dropped the records written in place into the \
         caller's vector"
    );
}

/// The frame that writes and the frame whose boundary drops the value need not
/// be the same one. `touch` stores a string `mid` allocated into a vector that
/// belongs to `main`; `touch` has its own frame (the call to `tag` keeps it off
/// the frameless-leaf path) and its own return drops nothing, so the obligation
/// is still outstanding when `mid` returns and takes the young-truncate path.
/// This is what handing the flag to the caller buys: without that one line the
/// program prints `JUNK-FILLER-ONE` — a later string that reused the slot — in
/// place of the payload.
#[test]
fn an_inherited_in_place_write_survives_the_callers_boundary() {
    let src = r#"module FrameBoundaryInherit
    intent = "an in-place write by a callee must survive the caller's boundary"
    depends []
    effects [Console]

fn tag(s: String) -> String
    ? "keep touch off the frameless-leaf path"
    s

fn touch(v: Vector<String>, s: String) -> Vector<String>
    ? "store the caller's string into a vector the caller's caller owns"
    Option.withDefault(Vector.set(v, 0, tag(s)), v)

fn mid(v: Vector<String>) -> Vector<String>
    ? "allocate the payload here and let the callee store it"
    s = String.toUpper("payload-marker-value")
    touch(v, s)

fn main() -> Unit
    ! [Console.print]
    w = mid(Vector.new(3, "placeholder-string"))
    j1 = String.toUpper("junk-filler-one")
    j2 = String.toUpper("junk-filler-two")
    j3 = String.toUpper("junk-filler-three")
    Console.print("{Option.withDefault(Vector.get(w, 0), "MISSING")}")
    Console.print("{String.len(j1)} {String.len(j2)} {String.len(j3)}")
"#;
    assert_eq!(
        run_vm("inherited_write", src),
        "PAYLOAD-MARKER-VALUE\n15 15 17",
        "the caller's boundary dropped a value its callee had written in place"
    );
}

/// Control: the same program shape over a `Map` is correct on pristine `main`
/// and stays correct here, because a map never takes the in-place route.
///
/// `Map.set` on a sole-owned map (`src/types/map.rs`, `set_nv_owned`) takes the
/// old table and pushes a NEW arena entry for the updated one
/// (`push_inheriting_source_space`, `aver-memory/src/arena.rs`), handing back a
/// fresh handle. A fresh handle is an ordinary allocation the boundary already
/// tracks through its roots, so no slot below the marks is ever mutated and
/// there is nothing for the boundary to rewrite. There is no fused
/// `MAP_SET_OR_KEEP` opcode either: `VECTOR_SET_OR_KEEP`'s owned branch is the
/// VM's only true in-place arena-slot write, which is why it is the only shape
/// at risk.
#[test]
fn map_updates_never_take_the_in_place_route() {
    let src = r#"module FrameBoundaryMapControl
    intent = "map updates allocate a fresh entry, so no boundary rewrite is owed"
    depends []
    effects [Console]

fn fill(m: Map<Int, String>, i: Int, tag: String) -> Map<Int, String>
    ? "tail-recursive fill with freshly built heap strings"
    match i >= 3
        true -> m
        false -> fill(Map.set(m, i, String.toUpper("element-{i}-marker")), i + 1, String.toUpper(tag))

fn emptyCells() -> Map<Int, String>
    ? "an empty map to fill"
    {}

fn main() -> Unit
    ! [Console.print]
    m = fill(emptyCells(), 0, "tag-value-long")
    j1 = String.toUpper("junk-filler-one")
    j2 = String.toUpper("junk-filler-two")
    j3 = String.toUpper("junk-filler-three")
    Console.print("{Option.withDefault(Map.get(m, 0), "MISSING")} {Option.withDefault(Map.get(m, 1), "MISSING")} {Option.withDefault(Map.get(m, 2), "MISSING")}")
    Console.print("{String.len(j1)} {String.len(j2)} {String.len(j3)}")
"#;
    assert_eq!(
        run_vm("map_control", src),
        "ELEMENT-0-MARKER ELEMENT-1-MARKER ELEMENT-2-MARKER\n15 15 17",
        "a map update lost an element at the frame boundary"
    );
}
