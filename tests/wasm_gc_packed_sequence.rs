//! Differential coverage for proof-packed `List<Int>` refinements.

#![cfg(feature = "wasm")]

use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static NEXT_TEMP_ID: AtomicU64 = AtomicU64::new(0);

fn run(source: &str, wasm_gc: bool, packed: bool) -> (bool, String) {
    let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let dir =
        std::env::temp_dir().join(format!("aver-packed-sequence-{}-{id}", std::process::id()));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, source).expect("source");
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.arg("run").arg(&path);
    if wasm_gc {
        command.arg("--wasm-gc");
    }
    if !packed {
        command.env("AVER_NO_PACKED_SEQUENCES", "1");
    }
    let output = command.output().expect("run aver");
    let _ = std::fs::remove_dir_all(dir);
    (
        output.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
        .trim()
        .to_string(),
    )
}

/// `aver verify` twin of `run` — the verify runner has its own wasm-gc
/// execution path (`aver verify --wasm-gc`), so a codegen gap can be red
/// there while `aver run` is green (and vice versa).
fn verify(source: &str, wasm_gc: bool, packed: bool) -> (bool, String) {
    let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!(
        "aver-packed-sequence-verify-{}-{id}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, source).expect("source");
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.arg("verify").arg(&path);
    if wasm_gc {
        command.arg("--wasm-gc");
    }
    if !packed {
        command.env("AVER_NO_PACKED_SEQUENCES", "1");
    }
    let output = command.output().expect("verify aver");
    let _ = std::fs::remove_dir_all(dir);
    (
        output.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
        .trim()
        .to_string(),
    )
}

/// Multi-module variant of `run`: writes `main.av` plus one dep module
/// file, then drives `aver run main.av --module-root <dir>` so the CLI's
/// multi-module flatten + wasm-gc path is exercised end to end.
fn run_multi(
    entry_src: &str,
    dep_file: &str,
    dep_src: &str,
    wasm_gc: bool,
    packed: bool,
) -> (bool, String) {
    let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!(
        "aver-packed-sequence-multi-{}-{id}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("main.av");
    std::fs::write(&path, entry_src).expect("entry source");
    std::fs::write(dir.join(dep_file), dep_src).expect("dep source");
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.arg("run").arg(&path).arg("--module-root").arg(&dir);
    if wasm_gc {
        command.arg("--wasm-gc");
    }
    if !packed {
        command.env("AVER_NO_PACKED_SEQUENCES", "1");
    }
    let output = command.output().expect("run aver");
    let _ = std::fs::remove_dir_all(dir);
    (
        output.status.success(),
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
        .trim()
        .to_string(),
    )
}

fn compile(source: &str, packed: bool) -> Vec<u8> {
    let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!(
        "aver-packed-sequence-compile-{}-{id}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("main.av");
    let out_dir = dir.join("out");
    std::fs::write(&path, source).expect("source");
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir);
    if !packed {
        command.env("AVER_NO_PACKED_SEQUENCES", "1");
    }
    let output = command.output().expect("compile aver");
    assert!(
        output.status.success(),
        "compile failed: {}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let wasm = std::fs::read(out_dir.join("main.wasm")).expect("compiled wasm");
    let _ = std::fs::remove_dir_all(dir);
    wasm
}

/// Multi-module variant of `compile`: emits the wasm-gc module for an
/// entry that depends on one module file, so type-section assertions can
/// be made about a refinement declared in a dependency.
fn compile_multi(entry_src: &str, dep_file: &str, dep_src: &str, packed: bool) -> Vec<u8> {
    let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!(
        "aver-packed-sequence-compile-multi-{}-{id}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("temp dir");
    let path = dir.join("main.av");
    let out_dir = dir.join("out");
    std::fs::write(&path, entry_src).expect("entry source");
    std::fs::write(dir.join(dep_file), dep_src).expect("dep source");
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command
        .arg("compile")
        .arg(&path)
        .arg("--module-root")
        .arg(&dir)
        .arg("--target")
        .arg("wasm-gc")
        .arg("-o")
        .arg(&out_dir);
    if !packed {
        command.env("AVER_NO_PACKED_SEQUENCES", "1");
    }
    let output = command.output().expect("compile aver");
    assert!(
        output.status.success(),
        "compile failed: {}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let wasm = std::fs::read(out_dir.join("main.wasm")).expect("compiled wasm");
    let _ = std::fs::remove_dir_all(dir);
    wasm
}

fn i8_array_count(wasm: &[u8]) -> usize {
    use wasmparser::{CompositeInnerType, Parser, Payload, StorageType};

    Parser::new(0)
        .parse_all(wasm)
        .filter_map(Result::ok)
        .filter_map(|payload| match payload {
            Payload::TypeSection(reader) => Some(reader),
            _ => None,
        })
        .flat_map(|reader| reader.into_iter().filter_map(Result::ok))
        .flat_map(|group| group.into_types())
        .filter(|sub| {
            matches!(
                &sub.composite_type.inner,
                CompositeInnerType::Array(array)
                    if matches!(array.0.element_type, StorageType::I8)
            )
        })
        .count()
}

const OCTETS: &str = r#"module M
    intent = "generic packed sequence"
    effects [Console]

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn toList(value: Octets) -> List<Int>
    value.values

fn same(left: Octets, right: Octets) -> Bool
    left == right

fn main() -> Unit
    ! [Console.print]
    match fromList(List.concat([0, 1, 127], [128, 255]))
        Result.Ok(value) -> match Vector.get(Vector.fromList([value]), 0)
            Option.Some(first) -> match Map.get(Map.set({}, "value", first), "value")
                Option.Some(stored) -> match Map.get(Map.set({}, stored, "present"), value)
                    Option.Some(_) -> match same(stored, value)
                        true -> Console.print("{List.len(toList(stored))}")
                        false -> Console.print("bad equality")
                    Option.None -> Console.print("missing map key")
                Option.None -> Console.print("missing map value")
            Option.None -> Console.print("missing value")
        Result.Err(error) -> Console.print(error)
"#;

const BYPASSED_OCTETS: &str = r#"module M
    intent = "an ungated constructor must demote the packed representation"
    effects [Console]

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn bypass(xs: List<Int>) -> Octets
    Octets(values = xs)

fn main() -> Unit
    ! [Console.print]
    value = bypass([256])
    match value.values
        [head, .._] -> Console.print("{head}")
        [] -> Console.print("empty")
"#;

#[test]
fn generic_u8_refinement_matches_vm_and_boxed_wasm() {
    let (vm_ok, vm) = run(OCTETS, false, true);
    let (packed_ok, packed) = run(OCTETS, true, true);
    let (boxed_ok, boxed) = run(OCTETS, true, false);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(packed_ok, "packed wasm failed: {packed}");
    assert!(boxed_ok, "boxed wasm failed: {boxed}");
    assert_eq!(packed, vm);
    assert_eq!(boxed, vm);
    assert_eq!(vm, "5");

    let packed_wasm = compile(OCTETS, true);
    let boxed_wasm = compile(OCTETS, false);
    assert_eq!(
        i8_array_count(&packed_wasm),
        i8_array_count(&boxed_wasm) + 1,
        "the proof-derived layout must add one packed i8 array"
    );
}

// Entry-side qualified annotation over a SOLE-declarer packed dep type
// (`o: Dep.Octets = ...`). The dep's gated refinement earns the packed
// layout under its bare post-flatten name while the entry annotation's
// qualified spelling survives in the type stamps; the flatten-derived
// alias must resolve both spellings to the same layout, on the packed
// path AND under the boxed differential baseline.
const QUALIFIED_ENTRY: &str = r#"module Main
    intent = "entry qualified annotation over sole-declarer packed dep type"
    depends [Dep]
    effects [Console]

fn firstValue(o: Dep.Octets) -> Int
    match o.values
        [head, .._] -> head
        [] -> 0 - 1

fn run() -> Result<Int, String>
    o: Dep.Octets = Dep.fromList([200])
    Result.Ok(firstValue(o))

fn main() -> Unit
    ! [Console.print]
    match run()
        Result.Ok(n) -> Console.print("{n}")
        Result.Err(error) -> Console.print(error)
"#;

const QUALIFIED_DEP: &str = r#"module Dep
    intent = "sole-declarer gated Octets refinement"
    exposes [Octets, fromList]
    depends []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")
"#;

#[test]
fn qualified_annotation_over_sole_declarer_dep_matches_vm_and_boxed_wasm() {
    let (vm_ok, vm) = run_multi(QUALIFIED_ENTRY, "dep.av", QUALIFIED_DEP, false, true);
    let (packed_ok, packed) = run_multi(QUALIFIED_ENTRY, "dep.av", QUALIFIED_DEP, true, true);
    let (boxed_ok, boxed) = run_multi(QUALIFIED_ENTRY, "dep.av", QUALIFIED_DEP, true, false);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(packed_ok, "packed wasm failed: {packed}");
    assert!(boxed_ok, "boxed wasm failed: {boxed}");
    assert_eq!(packed, vm);
    assert_eq!(boxed, vm);
    assert_eq!(vm, "200");
}

// SOUNDNESS counterpart of the alias availability test above: an
// entry-side QUALIFIED ungated constructor (`Dep.Octets(values = xs)`)
// resolves the packed layout through the flatten-derived alias, so the
// demotion scan must see that construct site under the SAME key — every
// spelling that can resolve a packed layout at a construct site must be
// visible to the scan. Before the alias-aware scan, the qualified
// spelling never matched the bare `Octets` candidate, the type kept its
// packed u8 storage, and the out-of-range 1000 silently truncated to
// 232 on the packed path (the exact bug class the exact-name rule
// killed for collision-renamed types).
const QUALIFIED_BYPASS_ENTRY: &str = r#"module Main
    intent = "entry-side qualified ungated constructor must demote the packed layout"
    depends [Dep]
    effects [Console]

fn bypass(xs: List<Int>) -> Dep.Octets
    Dep.Octets(values = xs)

fn main() -> Unit
    ! [Console.print]
    value = bypass([1000])
    match value.values
        [head, .._] -> Console.print("{head}")
        [] -> Console.print("empty")
"#;

#[test]
fn qualified_ungated_constructor_demotes_instead_of_truncating() {
    let (vm_ok, vm) = run_multi(QUALIFIED_BYPASS_ENTRY, "dep.av", QUALIFIED_DEP, false, true);
    let (packed_ok, packed) =
        run_multi(QUALIFIED_BYPASS_ENTRY, "dep.av", QUALIFIED_DEP, true, true);
    let (boxed_ok, boxed) = run_multi(QUALIFIED_BYPASS_ENTRY, "dep.av", QUALIFIED_DEP, true, false);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(packed_ok, "packed wasm failed: {packed}");
    assert!(boxed_ok, "boxed wasm failed: {boxed}");
    assert_eq!(packed, vm, "packed wasm must not truncate the ungated 1000");
    assert_eq!(boxed, vm);
    assert_eq!(vm, "1000");
}

// MULTI-FIELD twin of the qualified bypass above, for the carrier-i64
// path: the record-level fail-closed scan (`multi_field_record_demotions`)
// must see an entry-side QUALIFIED ungated construct site
// (`Dep.Coord(x = ..., y = ...)`) under the same canonical bare key its
// candidate set uses. If that canonicalization ever regressed, the
// qualified spelling would escape Scan 1, `Coord`'s fields would keep
// their native-i64 erasure, and the ungated beyond-i64 value below would
// trap in `__aint_to_i64_checked` at the construct bridge instead of
// demoting the record to its boxed `$AverInt` layout. The VM keeps the
// full bignum carrier, so identical output across all three legs is the
// soundness gate.
const QUALIFIED_MULTI_FIELD_BYPASS_ENTRY: &str = r#"module Main
    intent = "entry-side qualified ungated constructor must demote the multi-field carrier"
    depends [Dep]
    effects [Console]

fn bypass(a: Int, b: Int) -> Dep.Coord
    Dep.Coord(x = a, y = b)

fn main() -> Unit
    ! [Console.print]
    c = bypass(10000000000000000000, 4)
    Console.print("{c.x}")
"#;

const QUALIFIED_MULTI_FIELD_DEP: &str = r#"module Dep
    intent = "sole-declarer gated multi-field Coord record"
    exposes [Coord, coord]
    depends []

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 100), Bool.and(y >= 0, y <= 100))
        true -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("oob")
"#;

#[test]
fn qualified_ungated_multi_field_constructor_demotes_instead_of_trapping() {
    let (vm_ok, vm) = run_multi(
        QUALIFIED_MULTI_FIELD_BYPASS_ENTRY,
        "dep.av",
        QUALIFIED_MULTI_FIELD_DEP,
        false,
        true,
    );
    let (packed_ok, packed) = run_multi(
        QUALIFIED_MULTI_FIELD_BYPASS_ENTRY,
        "dep.av",
        QUALIFIED_MULTI_FIELD_DEP,
        true,
        true,
    );
    let (boxed_ok, boxed) = run_multi(
        QUALIFIED_MULTI_FIELD_BYPASS_ENTRY,
        "dep.av",
        QUALIFIED_MULTI_FIELD_DEP,
        true,
        false,
    );
    assert!(vm_ok, "VM failed: {vm}");
    assert!(
        packed_ok,
        "carrier-enabled wasm failed (beyond-i64 construct trapped instead \
         of demoting): {packed}"
    );
    assert!(boxed_ok, "AVER_NO_PACKED_SEQUENCES wasm failed: {boxed}");
    assert_eq!(
        packed, vm,
        "carrier-enabled wasm must demote the qualified ungated Coord, not \
         trap or truncate its beyond-i64 field"
    );
    assert_eq!(boxed, vm);
    assert_eq!(vm, "10000000000000000000");
}

#[test]
fn ungated_constructor_demotes_instead_of_truncating() {
    let (vm_ok, vm) = run(BYPASSED_OCTETS, false, true);
    let (wasm_ok, wasm) = run(BYPASSED_OCTETS, true, true);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(wasm_ok, "wasm failed: {wasm}");
    assert_eq!(wasm, vm);
    assert_eq!(vm, "256");
}

// ─── Literal smart-constructor discharge ────────────────────────────────
//
// `Dep.fromList([<all literals inside the proven interval>])` types as
// `Dep.Octets` and lowers to the carrier construction instead of a
// `Result`. The packed layout must survive that, and it does by
// construction: the discharge gate reads the SAME derived element
// interval the packed layout is chosen from, so an admitted value is
// always storable in the packed `i8` array. The two tests below pin both
// halves — the representation (type-section shape) and the value
// (VM / packed wasm-gc / boxed wasm-gc all agree).

const DISCHARGED_ENTRY: &str = r#"module Main
    intent = "a discharged literal smart-constructor call over a packed dep carrier"
    depends [Dep]
    effects [Console]

fn firstValue(o: Dep.Octets) -> Int
    match o.values
        [head, .._] -> head
        [] -> 0 - 1

fn describe(discharged: Dep.Octets, gated: Dep.Octets) -> String
    match discharged == gated
        true -> "{firstValue(discharged)} {List.len(Dep.toList(discharged))} same"
        false -> "different"

fn run() -> Result<String, String>
    discharged = Dep.fromList([200, 0, 255])
    gated = Dep.fromList(List.concat([200], [0, 255]))?
    Result.Ok(describe(discharged, gated))

fn main() -> Unit
    ! [Console.print]
    match run()
        Result.Ok(text) -> Console.print(text)
        Result.Err(error) -> Console.print(error)
"#;

// Same program with an out-of-interval element. The discharge declines,
// the smart constructor runs, and every backend reports its error —
// nothing reaches the packed store, whose element write is a raw
// `array.set` with no range check.
const DISCHARGE_DECLINED_ENTRY: &str = r#"module Main
    intent = "an out-of-interval literal keeps the fallible constructor"
    depends [Dep]
    effects [Console]

fn main() -> Unit
    ! [Console.print]
    match Dep.fromList([65, 256])
        Result.Ok(value) -> Console.print("unexpected {List.len(Dep.toList(value))}")
        Result.Err(error) -> Console.print(error)
"#;

const DISCHARGE_DEP: &str = r#"module Dep
    intent = "sole-declarer gated Octets refinement with a reader"
    exposes [Octets, fromList, toList]
    depends []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn toList(o: Octets) -> List<Int>
    o.values
"#;

#[test]
fn discharged_literal_construction_keeps_the_packed_layout_and_matches_vm() {
    let (vm_ok, vm) = run_multi(DISCHARGED_ENTRY, "dep.av", DISCHARGE_DEP, false, true);
    let (packed_ok, packed) = run_multi(DISCHARGED_ENTRY, "dep.av", DISCHARGE_DEP, true, true);
    let (boxed_ok, boxed) = run_multi(DISCHARGED_ENTRY, "dep.av", DISCHARGE_DEP, true, false);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(packed_ok, "packed wasm failed: {packed}");
    assert!(boxed_ok, "boxed wasm failed: {boxed}");
    // The discharged value is indistinguishable from the one the smart
    // constructor builds, on every backend.
    assert_eq!(vm, "200 3 same");
    assert_eq!(packed, vm, "packed wasm-gc diverged from the VM");
    assert_eq!(boxed, vm, "boxed wasm-gc diverged from the VM");

    // Representation: the discharged construct site is NOT an ungated
    // construction. The proof-derived packed layout is still installed, so
    // the packed module carries exactly one more `i8` array than the boxed
    // one — the same delta a gated-only program produces.
    let packed_wasm = compile_multi(DISCHARGED_ENTRY, "dep.av", DISCHARGE_DEP, true);
    let boxed_wasm = compile_multi(DISCHARGED_ENTRY, "dep.av", DISCHARGE_DEP, false);
    assert_eq!(
        i8_array_count(&packed_wasm),
        i8_array_count(&boxed_wasm) + 1,
        "a discharged literal construct site must keep the proof-derived \
         packed layout"
    );
}

#[test]
fn out_of_interval_literal_declines_the_discharge_on_every_backend() {
    let (vm_ok, vm) = run_multi(
        DISCHARGE_DECLINED_ENTRY,
        "dep.av",
        DISCHARGE_DEP,
        false,
        true,
    );
    let (packed_ok, packed) = run_multi(
        DISCHARGE_DECLINED_ENTRY,
        "dep.av",
        DISCHARGE_DEP,
        true,
        true,
    );
    let (boxed_ok, boxed) = run_multi(
        DISCHARGE_DECLINED_ENTRY,
        "dep.av",
        DISCHARGE_DEP,
        true,
        false,
    );
    assert!(vm_ok, "VM failed: {vm}");
    assert!(packed_ok, "packed wasm failed: {packed}");
    assert!(boxed_ok, "boxed wasm failed: {boxed}");
    assert_eq!(vm, "oob");
    assert_eq!(packed, vm, "packed wasm-gc must not silently truncate 256");
    assert_eq!(boxed, vm);
}

// ─── Interpolating the carrier ──────────────────────────────────────────
//
// `"{o.values}"` embeds the PROJECTED `List<Int>` carrier in a string.
// The projection itself already inserts the packed→list unpack bridge, so
// what the interpolation receives is a plain cons list on both the packed
// and the boxed path — but wasm-gc had no stringifier for a `List<Int>`
// embed at all. The MIR interpolation emitter bailed on the compound type,
// and a bail makes the WHOLE enclosing fn an `unreachable` trap stub, so
// the program compiled and then trapped at runtime with no diagnostic.
// The VM (and generated Rust) print `[0, 1, 127, 255]`.
//
// The program below pins every leg of the rendering against the VM:
// empty / single / multi carriers, a NEGATIVE and a beyond-i64 element on
// a plain (unrefined) list so the bignum formatter is exercised too, and
// two embeds in one literal so the parts array indexing is covered.
const INTERPOLATED_OCTETS: &str = r#"module M
    intent = "interpolate the projected carrier of a packed refinement"
    effects [Console]

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn show(o: Octets) -> String
    "{o.values}"

fn render(xs: List<Int>) -> String
    match fromList(xs)
        Result.Ok(o) -> show(o)
        Result.Err(error) -> error

fn main() -> Unit
    ! [Console.print]
    empty: List<Int> = []
    Console.print(render(empty))
    Console.print(render(List.concat([7], empty)))
    Console.print(render(List.concat([0, 1], [127, 255])))
    plain = List.concat([0 - 5, 0], [10000000000000000000])
    Console.print("plain={plain} twice={plain}")
"#;

#[test]
fn interpolated_packed_field_matches_vm_and_boxed_wasm() {
    let (vm_ok, vm) = run(INTERPOLATED_OCTETS, false, true);
    let (packed_ok, packed) = run(INTERPOLATED_OCTETS, true, true);
    let (boxed_ok, boxed) = run(INTERPOLATED_OCTETS, true, false);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(
        packed_ok,
        "packed wasm failed (interpolating the carrier trapped): {packed}"
    );
    assert!(boxed_ok, "boxed wasm failed: {boxed}");
    assert_eq!(
        vm,
        "[]\n[7]\n[0, 1, 127, 255]\nplain=[-5, 0, 10000000000000000000] \
         twice=[-5, 0, 10000000000000000000]"
    );
    assert_eq!(packed, vm, "packed wasm-gc diverged from the VM");
    assert_eq!(boxed, vm, "boxed wasm-gc diverged from the VM");
}

// Same gap, second surface: `aver verify --wasm-gc` runs cases through
// its own wasm-gc execution path, so the trap-stub body reached it too —
// the case bodies below returned nothing and every case failed.
const INTERPOLATED_OCTETS_VERIFY: &str = r#"module M
    intent = "verify an interpolated packed carrier on the wasm-gc runner"
    effects []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")

fn render(xs: List<Int>) -> String
    match fromList(xs)
        Result.Ok(o) -> "{o.values}"
        Result.Err(error) -> error

verify render
    render([]) => "[]"
    render([0, 1, 127, 255]) => "[0, 1, 127, 255]"
    render([256]) => "oob"
"#;

#[test]
fn interpolated_packed_field_verifies_on_the_wasm_gc_runner() {
    let (vm_ok, vm) = verify(INTERPOLATED_OCTETS_VERIFY, false, true);
    let (packed_ok, packed) = verify(INTERPOLATED_OCTETS_VERIFY, true, true);
    let (boxed_ok, boxed) = verify(INTERPOLATED_OCTETS_VERIFY, true, false);
    assert!(vm_ok, "VM verify failed: {vm}");
    assert!(packed_ok, "packed wasm-gc verify failed: {packed}");
    assert!(boxed_ok, "boxed wasm-gc verify failed: {boxed}");
    for (label, out) in [("packed", &packed), ("boxed", &boxed)] {
        assert!(
            out.contains("3/3 cases passed"),
            "{label} wasm-gc verify did not pass every case: {out}"
        );
    }
}
