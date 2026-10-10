#![cfg(feature = "wasm")]

//! Regression — a field read through a record its module does not expose.
//!
//! `Chain` exposes `Setting`, whose public field `walk` has the type `Walk`,
//! which `Chain` leaves out of `exposes`. A consumer that reads
//! `setting.walk.counted.store` holds a `Walk` value without being able to
//! name the type. The checker registered field types only for exposed
//! records, so the `.counted` read was stamped `Type::Invalid` without an
//! error. The VM does not need the stamp and ran the program; the wasm-gc
//! emitter found no layout for `Invalid` and refused the fn ("the field
//! `store` of `Invalid`, which has no registered layout"). btc-listener's
//! `Infra.Follow.withSetting` was the first program to reach it.
//!
//! Two modules declare a record `Built` with different fields, and the
//! consumer has a variant named `Built`, so the read only works when every
//! step of the chain is keyed by the declaring module, not the bare name.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output};
use std::fs;
use std::path::Path;
use std::process::Command;

const CHAIN: &str = r#"module Chain
    intent = "A walk whose record Built shares its name with another module's."
    exposes [Built, Setting, sampleSetting]

record Built
    store: Int
    blocks: Int

record Walk
    counted: Built
    target: Int

record Setting
    walk: Walk
    height: Int

fn sampleSetting(h: Int) -> Setting
    ? "A setting at this height."
    Setting(walk = Walk(counted = Built(store = h * 10, blocks = h), target = h), height = h)
"#;

const INDEX: &str = r#"module Index
    intent = "Another record named Built, with different fields."
    exposes [Built, sample]

record Built
    keys: List<String>
    written: Int

fn sample() -> Built
    ? "An empty index."
    Built(keys = [], written = 7)
"#;

const FOLLOW: &str = r#"module Follow
    intent = "Reads fields through a record type its dependency does not expose."
    exposes [storeOf, targetOf, Chunked, chunked]
    depends [Infra.Chain, Infra.Index]

type Chunked
    Built(Int, Bool)
    Refused(String)

fn storeOf(setting: Setting) -> Int
    ? "The store the walk counted."
    setting.walk.counted.store

fn targetOf(setting: Setting) -> Int
    ? "Where the walk is going."
    setting.walk.target

fn chunked(setting: Setting) -> Chunked
    ? "Built when the walk counted a store."
    match (setting.walk.counted.store > 0, setting.walk.counted.blocks)
        (true, n) -> Chunked.Built(setting.walk.counted.store + n, true)
        (false, _) -> Chunked.Refused("none")
"#;

const MAIN: &str = r#"module Main
    intent = "Reads a private record's fields across modules."
    depends [Infra.Chain, Infra.Index, Infra.Follow]

fn main() -> Unit
    ! [Console.print]
    s = Infra.Chain.sampleSetting(5)
    Console.print(String.fromInt(Infra.Follow.storeOf(s)))
    Console.print(String.fromInt(Infra.Follow.targetOf(s)))
    match Infra.Follow.chunked(s)
        Infra.Follow.Chunked.Built(n, _) -> Console.print("built {n}")
        Infra.Follow.Chunked.Refused(why) -> Console.print(why)
    Console.print(String.fromInt(Infra.Index.sample().written))
"#;

fn write_project(root: &Path) {
    fs::create_dir_all(root.join("infra")).expect("create infra dir");
    fs::write(root.join("infra/chain.av"), CHAIN).expect("write chain.av");
    fs::write(root.join("infra/index.av"), INDEX).expect("write index.av");
    fs::write(root.join("infra/follow.av"), FOLLOW).expect("write follow.av");
    fs::write(root.join("main.av"), MAIN).expect("write main.av");
}

fn run(root: &Path, extra: &[&str]) -> String {
    let output = Command::new(aver_bin())
        .arg("run")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(root)
        .args(extra)
        .output()
        .expect("spawn aver run");
    assert!(output.status.success(), "{}", format_output(&output));
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn field_of_unexposed_record_compiles_and_runs_on_wasm_gc() {
    let root = tempfile::tempdir().expect("temp project");
    write_project(root.path());

    let out_dir = root.path().join("out");
    let compiled = Command::new(aver_bin())
        .arg("compile")
        .arg(root.path().join("main.av"))
        .arg("--module-root")
        .arg(root.path())
        .args(["--target", "wasm-gc", "-o"])
        .arg(&out_dir)
        .output()
        .expect("spawn aver compile");
    assert!(compiled.status.success(), "{}", format_output(&compiled));

    let expected = "50\n5\nbuilt 55\n7\n";
    assert_eq!(run(root.path(), &[]), expected, "VM output");
    assert_eq!(run(root.path(), &["--wasm-gc"]), expected, "wasm-gc output");
}
