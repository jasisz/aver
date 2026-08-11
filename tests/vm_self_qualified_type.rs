//! A module that spells its own type with its own module name.
//!
//! `module Main` declaring `type Shade` may write that constructor as
//! `Shade.Dark` or as `Main.Shade.Dark`; the checker accepts both, so
//! every backend has to agree on both. `Main.Shade.Dark` in value
//! position used to reach the VM as a chain of namespace member lookups
//! and stop the program with "namespace Main has no member 'Shade'"
//! after a clean `aver check`; the wasm-gc backend panicked on the same
//! shape. Only a call — `Main.Tone.Solid(7)` — took the path that
//! recognised the qualifier, which is why a variant carrying fields
//! worked and a nullary one did not.
//!
//! Two oracles, because the disagreement had two halves. Within the VM,
//! the answer must not depend on which spelling the source used. Across
//! backends, the VM, the self-hosted interpreter and wasm-gc must print
//! the same thing for one program.
//!
//! Regression for #851.

use std::path::Path;
use std::process::{Command, Output};

/// The reproduction from the issue, widened to the constructors that
/// travel with it: a nullary variant and a variant carrying fields,
/// each written with the module's own name for its own type, in
/// expression and in pattern position.
const SELF_QUALIFIED_SRC: &str = r#"module Main
    intent = "Spell the module's own constructors with the module's own name."
    exposes [main]
    effects [Console.print]

type Shade
    Dark
    Light

type Tone
    Solid(Int)
    Blend(Int, Int)

fn name(s: Shade) -> String
    ? "Name a shade."
    match s
        Main.Shade.Dark -> "dark"
        Main.Shade.Light -> "light"

verify name
    name(Main.Shade.Dark) => "dark"
    name(Main.Shade.Light) => "light"

fn weight(t: Tone) -> Int
    ? "Weigh a tone."
    match t
        Main.Tone.Solid(v) -> v
        Main.Tone.Blend(a, b) -> a + b

verify weight
    weight(Main.Tone.Solid(7)) => 7
    weight(Main.Tone.Blend(2, 3)) => 5

fn main() -> Unit
    ? "Print one line per shape."
    ! [Console.print]
    Console.print("{name(Main.Shade.Dark)}|{name(Main.Shade.Light)}|{weight(Main.Tone.Solid(7))}|{weight(Main.Tone.Blend(2, 3))}")
"#;

/// The same program in the ordinary spelling. Its output is what every
/// other spelling has to produce.
const BARE_SRC: &str = r#"module Main
    intent = "The same program spelled the ordinary way."
    exposes [main]
    effects [Console.print]

type Shade
    Dark
    Light

type Tone
    Solid(Int)
    Blend(Int, Int)

fn name(s: Shade) -> String
    ? "Name a shade."
    match s
        Shade.Dark -> "dark"
        Shade.Light -> "light"

fn weight(t: Tone) -> Int
    ? "Weigh a tone."
    match t
        Tone.Solid(v) -> v
        Tone.Blend(a, b) -> a + b

fn main() -> Unit
    ? "Print one line per shape."
    ! [Console.print]
    Console.print("{name(Shade.Dark)}|{name(Shade.Light)}|{weight(Tone.Solid(7))}|{weight(Tone.Blend(2, 3))}")
"#;

/// Mixed: values built qualified, patterns written bare. A value and
/// the arm meant to catch it must be one constructor even when the two
/// sites disagree about how much of the name to write.
const QUALIFIED_VALUE_BARE_PATTERN_SRC: &str = r#"module Main
    intent = "Build qualified, match bare."
    exposes [main]
    effects [Console.print]

type Shade
    Dark
    Light

type Tone
    Solid(Int)
    Blend(Int, Int)

fn name(s: Shade) -> String
    ? "Name a shade."
    match s
        Shade.Dark -> "dark"
        Shade.Light -> "light"

fn weight(t: Tone) -> Int
    ? "Weigh a tone."
    match t
        Tone.Solid(v) -> v
        Tone.Blend(a, b) -> a + b

fn main() -> Unit
    ? "Print one line per shape."
    ! [Console.print]
    Console.print("{name(Main.Shade.Dark)}|{name(Main.Shade.Light)}|{weight(Main.Tone.Solid(7))}|{weight(Main.Tone.Blend(2, 3))}")
"#;

/// Mixed the other way: values built bare, patterns written qualified.
const BARE_VALUE_QUALIFIED_PATTERN_SRC: &str = r#"module Main
    intent = "Build bare, match qualified."
    exposes [main]
    effects [Console.print]

type Shade
    Dark
    Light

type Tone
    Solid(Int)
    Blend(Int, Int)

fn name(s: Shade) -> String
    ? "Name a shade."
    match s
        Main.Shade.Dark -> "dark"
        Main.Shade.Light -> "light"

fn weight(t: Tone) -> Int
    ? "Weigh a tone."
    match t
        Main.Tone.Solid(v) -> v
        Main.Tone.Blend(a, b) -> a + b

fn main() -> Unit
    ? "Print one line per shape."
    ! [Console.print]
    Console.print("{name(Shade.Dark)}|{name(Shade.Light)}|{weight(Tone.Solid(7))}|{weight(Tone.Blend(2, 3))}")
"#;

/// A record the module builds through its own module name, plus the
/// same name in the return annotation.
const SELF_QUALIFIED_RECORD_SRC: &str = r#"module Main
    intent = "Build the module's own record through the module's own name."
    exposes [main]
    effects [Console.print]

record Point
    x: Int
    y: Int

fn origin() -> Main.Point
    ? "A point at the origin."
    Main.Point(x = 0, y = 0)

fn shifted() -> Main.Point
    ? "The origin moved right and up."
    Main.Point.update(origin(), x = 3, y = 4)

verify shifted
    shifted().x => 3
    shifted().y => 4

fn main() -> Unit
    ? "Print the shifted point."
    ! [Console.print]
    p = shifted()
    Console.print("{p.x}|{p.y}")
"#;

const EXPECTED: &str = "dark|light|7|5";

fn write_fixture(source: &str) -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("create fixture directory");
    std::fs::write(dir.path().join("main.av"), source).expect("write main.av");
    dir
}

fn run_cli(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(args)
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(dir)
        .output()
        .unwrap_or_else(|error| panic!("spawn `aver {}`: {error}", args.join(" ")))
}

fn stdout_of(dir: &Path, args: &[&str]) -> String {
    let output = run_cli(dir, args);
    assert!(
        output.status.success(),
        "`aver {}` failed\nstdout:\n{}\nstderr:\n{}",
        args.join(" "),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn vm_stdout(source: &str) -> String {
    let dir = write_fixture(source);
    stdout_of(dir.path(), &["run"])
}

#[test]
fn the_vm_answers_the_same_whichever_spelling_the_module_uses() {
    let bare = vm_stdout(BARE_SRC);
    assert_eq!(bare, EXPECTED);
    assert_eq!(vm_stdout(SELF_QUALIFIED_SRC), bare);
    assert_eq!(vm_stdout(QUALIFIED_VALUE_BARE_PATTERN_SRC), bare);
    assert_eq!(vm_stdout(BARE_VALUE_QUALIFIED_PATTERN_SRC), bare);
}

#[test]
fn the_vm_matches_the_self_host_on_a_self_qualified_constructor() {
    let dir = write_fixture(SELF_QUALIFIED_SRC);
    let vm = stdout_of(dir.path(), &["run"]);
    let self_host = stdout_of(dir.path(), &["run", "--self-host"]);
    assert_eq!(vm, EXPECTED);
    assert_eq!(vm, self_host);
}

#[test]
fn the_vm_matches_the_self_host_on_a_self_qualified_record() {
    let dir = write_fixture(SELF_QUALIFIED_RECORD_SRC);
    let vm = stdout_of(dir.path(), &["run"]);
    let self_host = stdout_of(dir.path(), &["run", "--self-host"]);
    assert_eq!(vm, "3|4");
    assert_eq!(vm, self_host);
}

#[test]
fn verify_passes_for_a_self_qualified_constructor() {
    let dir = write_fixture(SELF_QUALIFIED_SRC);
    let stdout = stdout_of(dir.path(), &["verify"]);
    assert!(
        stdout.contains("4/4"),
        "unexpected verify output:\n{stdout}"
    );
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_matches_the_vm_on_a_self_qualified_constructor() {
    let dir = write_fixture(SELF_QUALIFIED_SRC);
    let vm = stdout_of(dir.path(), &["run"]);
    let wasm_gc = stdout_of(dir.path(), &["run", "--wasm-gc"]);
    assert_eq!(vm, EXPECTED);
    assert_eq!(vm, wasm_gc);
}
