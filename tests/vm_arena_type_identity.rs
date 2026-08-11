//! VM arena identity regressions for records that share a bare name across modules.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use aver::codegen::ModuleInfo;
use aver::ir::pipeline::{self, PipelineConfig, TypecheckMode};
use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::source::{LoadedModule, parse_source};
use aver::value::Value;
use aver::vm;

const LEFT_SRC: &str = r#"module Left
    intent = "Dependency with a private Box record."
    exposes [mk]
    effects []

record Box
    b: Int
    a: Int

fn mk() -> Int
    ? "Read the private Box."
    Box(b = 9, a = 8).a
"#;

const ENTRY_SRC: &str = r#"module Main
    intent = "Entry with its own Box record."
    depends [Left]
    exposes [main]
    effects [Console.print]

record Box
    a: Int
    b: Int

fn probe() -> Int
    ? "Read both fields from the entry Box."
    x = Box(a = 111, b = 222)
    x.a * 1000 + x.b

verify probe
    probe() => 111222

fn main() -> Unit
    ? "Print the probe result."
    ! [Console.print]
    Console.print("{probe()}")
"#;

const WIDER_ENTRY_SRC: &str = r#"module Main
    intent = "Entry with a wider Box record."
    depends [Left]
    effects []

record Box
    a: Int
    b: Int
    c: Int

fn probe() -> Int
    x = Box(a = 1, b = 2, c = 3)
    x.c
"#;

const DEP_ONLY_ENTRY_SRC: &str = r#"module Main
    intent = "Entry that uses the dependency Box through Left.mk."
    depends [Left]
    effects []

fn probe() -> Int
    Left.mk()
"#;

struct CompiledFixture {
    code: vm::CodeStore,
    globals: Vec<NanValue>,
    arena: Arena,
}

fn compile_fixture(entry_source: &str) -> CompiledFixture {
    let mut entry_items = parse_source(entry_source).expect("entry parse");
    let loaded = vec![LoadedModule {
        dep_name: "Left".to_string(),
        items: parse_source(LEFT_SRC).expect("Left parse"),
        path: PathBuf::from("left.av"),
    }];
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let result = pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_build_symbols: true,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let typecheck = result.typecheck.as_ref().expect("typecheck result");
    assert!(
        typecheck.errors.is_empty(),
        "fixture should typecheck: {:?}",
        typecheck.errors
    );

    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_loaded_modules(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        loaded,
        "<test>",
        result.analysis.as_ref(),
    )
    .expect("compile fixture");

    CompiledFixture {
        code,
        globals,
        arena,
    }
}

fn run_probe(entry_source: &str) -> (Value, Arena) {
    let CompiledFixture {
        code,
        globals,
        arena,
    } = compile_fixture(entry_source);
    let mut machine = vm::VM::new(code, globals, arena);
    machine.run_top_level().expect("run top level");
    let result = machine
        .run_named_function("probe", &[])
        .expect("run probe")
        .to_value(&machine.arena);
    (result, machine.arena)
}

fn write_cli_fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create fixture directory");
    std::fs::write(dir.path().join("left.av"), LEFT_SRC).expect("write left.av");
    std::fs::write(dir.path().join("main.av"), ENTRY_SRC).expect("write main.av");
    dir
}

fn run_cli(dir: &Path, subcommand: &str, self_host: bool) -> Output {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command.arg(subcommand);
    if self_host {
        command.arg("--self-host");
    }
    command
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(dir)
        .output()
        .unwrap_or_else(|error| panic!("spawn `aver {subcommand}`: {error}"))
}

fn assert_success(output: &Output, command: &str) {
    assert!(
        output.status.success(),
        "`{command}` failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn arena_keys_entry_and_dep_records_separately() {
    let fixture = compile_fixture(ENTRY_SRC);
    let entry_id = fixture.arena.find_type_id("Box").expect("entry Box");
    let dep_id = fixture
        .arena
        .find_type_id("Left.Box")
        .expect("dependency Left.Box");

    assert_ne!(entry_id, dep_id);
    assert_eq!(fixture.arena.get_field_names(entry_id), ["a", "b"]);
    assert_eq!(fixture.arena.get_field_names(dep_id), ["b", "a"]);
    assert_eq!(fixture.arena.get_type_name(entry_id), "Box");
    assert_eq!(fixture.arena.get_type_name(dep_id), "Box");
}

#[test]
fn vm_reads_entry_record_fields_when_a_dep_shadows_the_bare_name() {
    let (result, _) = run_probe(ENTRY_SRC);
    assert_eq!(result, Value::int(111222));
}

#[test]
fn vm_does_not_read_out_of_bounds_when_the_entry_record_is_wider() {
    let (result, _) = run_probe(WIDER_ENTRY_SRC);
    assert_eq!(result, Value::int(3));
}

#[test]
fn vm_matches_self_host_on_shadowed_record_layout() {
    let dir = write_cli_fixture();
    let vm = run_cli(dir.path(), "run", false);
    let self_host = run_cli(dir.path(), "run", true);
    assert_success(&vm, "aver run");
    assert_success(&self_host, "aver run --self-host");

    let vm_stdout = String::from_utf8_lossy(&vm.stdout).trim().to_string();
    let self_host_stdout = String::from_utf8_lossy(&self_host.stdout)
        .trim()
        .to_string();
    assert_eq!(self_host_stdout, "111222");
    assert_eq!(vm_stdout, self_host_stdout);
}

#[test]
fn verify_passes_for_shadowed_record_layout() {
    let dir = write_cli_fixture();
    let output = run_cli(dir.path(), "verify", false);
    assert_success(&output, "aver verify");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("1/1"),
        "unexpected verify output:\n{stdout}"
    );
}

#[test]
fn dep_only_record_still_resolves_by_bare_name() {
    let fixture = compile_fixture(DEP_ONLY_ENTRY_SRC);
    assert_eq!(
        fixture.arena.find_type_id("Box"),
        fixture.arena.find_type_id("Left.Box"),
        "the dependency record's bare display spelling remains a fallback alias"
    );

    let mut machine = vm::VM::new(fixture.code, fixture.globals, fixture.arena);
    machine.run_top_level().expect("run top level");
    let result = machine
        .run_named_function("probe", &[])
        .expect("run probe")
        .to_value(&machine.arena);
    assert_eq!(result, Value::int(8));
}
