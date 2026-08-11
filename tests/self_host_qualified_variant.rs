//! Cross-module sum-type variant identity on the self-hosted interpreter.
//!
//! The self-host keys a variant on its constructor name, so the same
//! variant has to carry the same name whichever module wrote it down.
//! A dependency builds `Shade.Dark`; its caller writes the same
//! constructor as `Palette.Shade.Dark`. Both spellings name one
//! constructor, and every producer/consumer pairing across that
//! boundary must behave the same way it does on the VM.
//!
//! The oracle is the VM: each fixture runs under `aver run` and under
//! `aver run --self-host`, and the two stdouts must be equal.
//!
//! Regression for #847 — matching a dependency's sum type by its
//! qualified name took no arm at all, on a program with no name
//! collision anywhere in it.

#![cfg(feature = "runtime")]

use std::path::Path;
use std::process::{Command, Output};

/// A dependency that both produces and consumes its own sum types,
/// spelling the constructors bare the way its own module sees them.
/// `Tone` carries fields, so its constructors travel as calls rather
/// than as bare names and its patterns bind.
const PALETTE_SRC: &str = r#"module Palette
    intent = "Two sum types with producers and consumers — one nullary, one carrying fields."
    exposes [Shade, Tone, pick, other, label, solid, weight]
    effects []

type Shade
    Dark
    Light

type Tone
    Solid(Int)
    Blend(Int, Int)

fn pick() -> Shade
    ? "Hand out Dark."
    Shade.Dark

fn other() -> Shade
    ? "Hand out Light."
    Shade.Light

fn label(s: Shade) -> Str
    ? "Name the shade from inside the owning module."
    match s
        Shade.Dark -> "in:dark"
        Shade.Light -> "in:light"

verify label
    label(Shade.Dark) => "in:dark"
    label(Shade.Light) => "in:light"

fn solid() -> Tone
    ? "Hand out a solid tone."
    Tone.Solid(7)

fn weight(t: Tone) -> Int
    ? "Total weight of a tone, from inside the owning module."
    match t
        Tone.Solid(v) -> v
        Tone.Blend(a, b) -> a + b

verify weight
    weight(Tone.Solid(7)) => 7
    weight(Tone.Blend(2, 3)) => 5
"#;

/// Every pairing of the two spellings: a value the dependency built
/// against a qualified pattern (#847 itself), a value the caller built
/// against the dependency's own bare pattern, and equality across the
/// boundary in both directions.
const ENTRY_SRC: &str = r#"module Main
    depends [Palette]
    intent = "Exercise both spellings of a dependency's variant in both directions."
    exposes [main]
    effects [Console.print]

fn name(s: Palette.Shade) -> Str
    ? "Name the shade from outside the owning module."
    match s
        Palette.Shade.Dark -> "out:dark"
        Palette.Shade.Light -> "out:light"

verify name
    name(Palette.Shade.Dark) => "out:dark"
    name(Palette.Shade.Light) => "out:light"

fn describe(t: Palette.Tone) -> Str
    ? "Describe a tone from outside the owning module, binding its fields."
    match t
        Palette.Tone.Solid(v) -> "out:solid:{v}"
        Palette.Tone.Blend(a, b) -> "out:blend:{a}:{b}"

verify describe
    describe(Palette.Tone.Solid(7)) => "out:solid:7"
    describe(Palette.Tone.Blend(2, 3)) => "out:blend:2:3"

fn main() -> Unit
    ? "Print every producer/consumer combination."
    ! [Console.print]
    Console.print(name(Palette.pick()))
    Console.print(name(Palette.Shade.Light))
    Console.print(Palette.label(Palette.pick()))
    Console.print(Palette.label(Palette.Shade.Light))
    Console.print(String.fromBool(Palette.pick() == Palette.Shade.Dark))
    Console.print(String.fromBool(Palette.other() == Palette.Shade.Light))
    Console.print(describe(Palette.solid()))
    Console.print(describe(Palette.Tone.Blend(2, 3)))
    Console.print("{Palette.weight(Palette.Tone.Blend(2, 3))}")
    Console.print("{Palette.weight(Palette.solid())}")
"#;

const ENTRY_EXPECTED: &str = "out:dark\nout:light\nin:dark\nin:light\ntrue\ntrue\n\
     out:solid:7\nout:blend:2:3\n5\n7";

/// A sum type declared in the entry module only — the case that has
/// always worked, kept as the control that canonicalising qualified
/// spellings did not disturb the unqualified ones.
const LOCAL_ONLY_SRC: &str = r#"module Main
    intent = "A sum type declared and used entirely inside the entry module."
    exposes [main]
    effects [Console.print]

type Shade
    Dark
    Light

fn name(s: Shade) -> Str
    ? "Name the local shade."
    match s
        Shade.Dark -> "local:dark"
        Shade.Light -> "local:light"

verify name
    name(Shade.Dark) => "local:dark"
    name(Shade.Light) => "local:light"

fn main() -> Unit
    ? "Print both local variants."
    ! [Console.print]
    Console.print(name(Shade.Dark))
    Console.print(name(Shade.Light))
"#;

const LOCAL_ONLY_EXPECTED: &str = "local:dark\nlocal:light";

/// A local sum type shadowing a dependency's same-named one. The
/// language's rule is that the local declaration wins for the bare
/// spelling and the dependency's own type stays reachable qualified,
/// so each function must see the variants of the type it declared.
const SHADOWING_SRC: &str = r#"module Main
    depends [Palette]
    intent = "A local sum type that shadows the dependency's same-named one."
    exposes [main]
    effects [Console.print]

type Shade
    Dark
    Light

fn localName(s: Shade) -> Str
    ? "Name the local shade."
    match s
        Shade.Dark -> "local:dark"
        Shade.Light -> "local:light"

verify localName
    localName(Shade.Dark) => "local:dark"
    localName(Shade.Light) => "local:light"

fn depName(s: Palette.Shade) -> Str
    ? "Name the dependency's shade."
    match s
        Palette.Shade.Dark -> "dep:dark"
        Palette.Shade.Light -> "dep:light"

fn main() -> Unit
    ? "Exercise both same-named sum types."
    ! [Console.print]
    Console.print(localName(Shade.Light))
    Console.print(depName(Palette.pick()))
    Console.print(Palette.label(Palette.other()))
"#;

const SHADOWING_EXPECTED: &str = "local:light\ndep:dark\nin:light";

fn write_fixture(entry_source: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create fixture directory");
    std::fs::write(dir.path().join("palette.av"), PALETTE_SRC).expect("write palette.av");
    std::fs::write(dir.path().join("main.av"), entry_source).expect("write main.av");
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

fn stdout_of(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

/// Run one fixture on both backends and require identical stdout.
fn assert_backends_agree(entry_source: &str, expected: &str) {
    let dir = write_fixture(entry_source);
    let vm = run_cli(dir.path(), "run", false);
    let self_host = run_cli(dir.path(), "run", true);
    assert_success(&vm, "aver run");
    assert_success(&self_host, "aver run --self-host");

    let vm_stdout = stdout_of(&vm);
    let self_host_stdout = stdout_of(&self_host);
    assert_eq!(vm_stdout, expected, "VM output moved");
    assert_eq!(
        self_host_stdout, vm_stdout,
        "self-host disagrees with the VM"
    );
}

#[test]
fn self_host_matches_vm_on_a_dependency_variant_in_both_spellings() {
    assert_backends_agree(ENTRY_SRC, ENTRY_EXPECTED);
}

#[test]
fn self_host_matches_vm_on_a_variant_of_a_local_sum_type() {
    assert_backends_agree(LOCAL_ONLY_SRC, LOCAL_ONLY_EXPECTED);
}

#[test]
fn self_host_shadows_a_dependency_sum_type_with_the_local_one() {
    // VM-vs-self-host stdout is not compared here yet: on this commit
    // the VM hits an internal assertion compiling two same-named sum
    // types (the symbol table is keyed on the bare `Type.Variant`
    // spelling), which #846 fixes by keying it on the canonical name.
    // Pinning the self-host's own output records the answer the
    // language's shadowing rule requires, so the day the VM side is
    // added it has something to agree with.
    let dir = write_fixture(SHADOWING_SRC);
    let self_host = run_cli(dir.path(), "run", true);
    assert_success(&self_host, "aver run --self-host");
    assert_eq!(stdout_of(&self_host), SHADOWING_EXPECTED);
}

#[test]
fn verify_passes_on_both_spellings_of_a_dependency_variant() {
    let dir = write_fixture(ENTRY_SRC);
    let output = run_cli(dir.path(), "verify", false);
    assert_success(&output, "aver verify");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("4/4 cases passed | 0 failed"),
        "unexpected verify output:\n{stdout}"
    );
}
