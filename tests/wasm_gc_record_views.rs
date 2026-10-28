//! Field-only record arguments and results agree with the VM, including
//! effects, precision, recursion, function values and mixed packed callers.

#![cfg(feature = "wasm")]

use std::process::Command;

fn differential(source: &str, expected: &str) {
    let dir = tempfile::tempdir().expect("test directory");
    let path = dir.path().join("main.av");
    std::fs::write(&path, source).expect("test source");
    for wasm in [false, true] {
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_aver"));
        cmd.current_dir(env!("CARGO_MANIFEST_DIR"))
            .arg("run")
            .arg(&path);
        if wasm {
            cmd.arg("--wasm-gc");
        }
        let out = cmd.output().expect("aver runs");
        assert!(
            out.status.success(),
            "wasm={wasm}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(
            String::from_utf8_lossy(&out.stdout).trim(),
            expected,
            "wasm={wasm}"
        );
    }
}

#[test]
fn fields_cross_producer_and_forwarding_calls_without_losing_precision() {
    differential(
        r#"module Main
    intent = "A producer's record crosses two forwarding calls."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn pair(x: Int) -> Pair
    Pair(left = x, right = x + 1)

fn forward(p: Pair, n: Int) -> Int
    read(p, n)

fn read(p: Pair, n: Int) -> Int
    p.left * n + p.right

fn main() -> Unit
    ! [Console.print]
    Console.print("{forward(pair(9223372036854775808), 2)}")
"#,
        "27670116110564327425",
    );
}

#[test]
fn written_field_order_and_other_argument_effects_are_preserved() {
    differential(
        r#"module Main
    intent = "Splitting a record preserves written evaluation order."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn mark(n: Int) -> Int
    ! [Console.print]
    Console.print("{n}")
    n

fn read(p: Pair, n: Int) -> Int
    p.left * 100 + p.right * 10 + n

fn main() -> Unit
    ! [Console.print]
    Console.print("{read(Pair(right = mark(2), left = mark(1)), mark(3))}")
"#,
        "2\n1\n3\n123",
    );
}

#[test]
fn record_results_and_parameters_survive_recursive_tail_calls() {
    differential(
        r#"module Main
    intent = "A recursive producer and a recursive field-only consumer."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn make(n: Int, x: Int) -> Pair
    match n == 0
        true -> Pair(left = x, right = x + 1)
        false -> make(n - 1, x + 1)

fn read(p: Pair, n: Int) -> Int
    match n == 0
        true -> p.left + p.right
        false -> read(p, n - 1)

fn main() -> Unit
    ! [Console.print]
    Console.print("{read(make(1000, 7), 1000)}")
"#,
        "2015",
    );
}

#[test]
fn packed_arguments_and_function_values_keep_the_source_signature() {
    differential(
        r#"module Main
    intent = "Packed callers and an indirect call use the same function."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn pair(x: Int) -> Pair
    Pair(left = x, right = x + 1)

fn read(p: Pair) -> Int
    p.left + p.right

fn indirect(f: Fn(Pair) -> Int, p: Pair) -> Int
    f(p)

fn main() -> Unit
    ! [Console.print]
    p = pair(10)
    Console.print("{read(pair(1))}, {read(p)}, {indirect(read, p)}")
"#,
        "3, 21, 21",
    );
}

#[test]
fn a_record_passed_to_a_retaining_function_stays_a_value() {
    differential(
        r#"module Main
    intent = "A whole-value use stops field-only parameter treatment."
    effects [Console.print]

record Pair
    left: Int
    right: Int

record Saved
    pair: Pair
    count: Int

fn save(p: Pair) -> Saved
    Saved(pair = p, count = p.left)

fn forward(p: Pair) -> Saved
    save(p)

fn main() -> Unit
    ! [Console.print]
    s = forward(Pair(left = 7, right = 11))
    Console.print("{s.pair.right + s.count}")
"#,
        "18",
    );
}

#[test]
fn a_record_result_projected_directly_and_updated_keeps_its_fields() {
    differential(
        r#"module Main
    intent = "Project and update producers through unpacked results."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn pair(n: Int) -> Pair
    Pair(left = n, right = n + 1)

fn update(p: Pair, n: Int) -> Pair
    Pair.update(p, left = n)

fn main() -> Unit
    ! [Console.print]
    Console.print("{pair(6).right}, {update(pair(8), 20).left}, {update(pair(8), 20).right}")
"#,
        "7, 20, 9",
    );
}

#[test]
fn multiple_record_params_remap_correctly_beside_unit_and_scalar_params() {
    differential(
        r#"module Main
    intent = "Removing two record params preserves all other parameter slots."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn pair(n: Int) -> Pair
    Pair(left = n, right = n + 1)

fn read(a: Pair, u: Unit, n: Int, b: Pair, f: Float) -> String
    "{a.left}, {n + b.right}, {f}"

fn main() -> Unit
    ! [Console.print]
    u = Console.print("unit")
    Console.print(read(pair(7), u, 9223372036854775808, pair(10), 1.5))
"#,
        "unit\n7, 9223372036854775819, 1.5",
    );
}

#[test]
fn a_failed_field_exits_before_later_fields_and_arguments() {
    differential(
        r#"module Main
    intent = "An early error while splitting fields preserves evaluation order."
    effects [Console.print]

record Pair
    left: Int
    right: Int

fn mark(n: Int) -> Int
    ! [Console.print]
    Console.print("{n}")
    n

fn fail() -> Result<Int, String>
    ! [Console.print]
    Console.print("fail")
    Result.Err("stop")

fn read(p: Pair, n: Int) -> Int
    p.left + p.right + n

fn run() -> Result<Int, String>
    ! [Console.print]
    Result.Ok(read(Pair(right = fail()?, left = mark(1)), mark(3)))

fn main() -> Unit
    ! [Console.print]
    match run()
        Result.Ok(n) -> Console.print("{n}")
        Result.Err(e) -> Console.print(e)
"#,
        "fail\nstop",
    );
}

#[test]
fn record_splitting_does_not_change_a_certified_builds_bytes() {
    let dir = tempfile::tempdir().expect("test directory");
    let path = dir.path().join("main.av");
    std::fs::write(
        &path,
        r#"module Main
    intent = "Certificate candidates retain their ordinary record lowering."

record Pair
    left: Int
    right: Int

fn projected(n: Int) -> Int
    Pair(left = n, right = n + 1).right

fn pair(n: Int) -> Pair
    Pair(left = n, right = n)

fn read(p: Pair) -> Int
    p.left + p.right

fn main() -> Int
    read(pair(projected(4)))
"#,
    )
    .expect("test source");
    let compile = |certify: bool, records: bool| {
        let out_dir = tempfile::tempdir().expect("output directory");
        let mut cmd = Command::new(env!("CARGO_BIN_EXE_aver"));
        cmd.current_dir(env!("CARGO_MANIFEST_DIR"))
            .arg("compile")
            .arg(&path)
            .args(["--target", "wasm-gc", "-o"])
            .arg(out_dir.path());
        if certify {
            cmd.arg("--certify");
        }
        if !records {
            cmd.env("AVER_NO_RECORD_TWINS", "1");
        }
        let out = cmd.output().expect("aver compiles");
        assert!(
            out.status.success(),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        std::fs::read(out_dir.path().join("main.wasm")).expect("compiled module")
    };
    assert_ne!(compile(false, true), compile(false, false));
    assert_eq!(compile(true, true), compile(true, false));
}

#[test]
fn the_record_producer_has_a_multi_value_body_without_a_record_allocation() {
    use aver::ir::{PipelineConfig, TypecheckMode, pipeline};
    use wasmparser::{CompositeInnerType, Operator, Parser, Payload, ValType};

    let source = r#"module Main
    intent = "A record result is consumed by a field-only function."

record Pair
    left: Int
    right: Int

fn pair(n: Int) -> Pair
    Pair(left = n, right = n)

fn forward(p: Pair) -> Int
    read(p)

fn read(p: Pair) -> Int
    p.left + p.right

fn main() -> Int
    forward(pair(7))
"#;
    let mut items = aver::source::parse_source(source).expect("source parses");
    let prepared = pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            run_interp_lower: false,
            run_buffer_build: false,
            ..Default::default()
        },
    );
    assert!(prepared.typecheck.as_ref().unwrap().errors.is_empty());
    let bytes = aver::codegen::wasm_gc::compile_to_wasm_gc(&items, prepared.analysis.as_ref())
        .expect("source compiles");
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .expect("wasm validates");
    let mut field_results = Vec::new();
    let mut functions = Vec::new();
    let mut bodies = 0;
    let mut found = false;
    for payload in Parser::new(0).parse_all(&bytes) {
        match payload.expect("wasm parses") {
            Payload::TypeSection(section) => {
                for group in section {
                    for ty in group.unwrap().into_types() {
                        field_results.push(matches!(ty.composite_type.inner,
                            CompositeInnerType::Func(ref f) if f.results().len() == 2
                                && f.results().iter().all(|v| matches!(v, ValType::Ref(_)))));
                    }
                }
            }
            Payload::FunctionSection(section) => {
                functions.extend(section.into_iter().map(Result::unwrap))
            }
            Payload::CodeSectionEntry(body) => {
                if field_results[functions[bodies] as usize] {
                    let allocates = body
                        .get_operators_reader()
                        .unwrap()
                        .into_iter()
                        .any(|op| matches!(op.unwrap(), Operator::StructNew { .. }));
                    found |= !allocates;
                }
                bodies += 1;
            }
            _ => {}
        }
    }
    assert!(
        found,
        "no record producer returned its two fields without allocating"
    );
}
