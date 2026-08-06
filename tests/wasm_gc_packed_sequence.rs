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
    match fromList([0, 1, 127, 128, 255])
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

#[test]
fn ungated_constructor_demotes_instead_of_truncating() {
    let (vm_ok, vm) = run(BYPASSED_OCTETS, false, true);
    let (wasm_ok, wasm) = run(BYPASSED_OCTETS, true, true);
    assert!(vm_ok, "VM failed: {vm}");
    assert!(wasm_ok, "wasm failed: {wasm}");
    assert_eq!(wasm, vm);
    assert_eq!(vm, "256");
}
