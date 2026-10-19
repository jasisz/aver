//! What `main` returns is the process exit status, the same on every runner:
//! `Unit` ends in 0, `Result.Err` in 1, and an `Int` is the status itself when
//! it lies in 0..=255. Any other `Int` ends in 1 rather than in its low byte,
//! so 256 never reads as success. The compiled Rust binary is held to the same
//! table in `rust_codegen_differential.rs`.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, cleanup, format_output, temp_module};

use std::process::Command;

/// No arithmetic, so wasm-gc keeps `Int` as a scalar `i64`.
const LITERAL_INT: &str = r#"module ExitLiteral
    intent = "main returns the exit status as a literal"

fn main() -> Int
    3
"#;

/// The status comes from the command line, so one program covers 0, a
/// status in range, and values outside 0..=255 (a Big among them).
const ARG_INT: &str = r#"module ExitArg
    intent = "main returns the exit status it reads from the command line"
    effects [Args.get]

fn fromArg(arg: String) -> Int
    ? "The argument as an Int, 99 when it is not one."
    match Int.fromString(arg)
        Result.Ok(n) -> n + 0
        Result.Err(_) -> 99

fn main() -> Int
    ! [Args.get]
    match Args.get()
        [] -> 98
        [arg, ..rest] -> fromArg(arg)
"#;

/// `Args.get` is only what follows `--`: no program name, no `run` or file.
const ARG_COUNT: &str = r#"module ExitArgCount
    intent = "main returns how many arguments it was given"
    effects [Args.get]

fn main() -> Int
    ! [Args.get]
    List.len(Args.get())
"#;

const UNIT: &str = r#"module ExitUnit
    intent = "main returns Unit"
    effects [Console.print]

fn main() -> Unit
    ! [Console.print]
    Console.print("ok")
"#;

const ERR: &str = r#"module ExitErr
    intent = "main returns an error"

fn main() -> Result<Unit, String>
    Result.Err("boom")
"#;

/// Runs `source` with `aver run <mode>`, returning the exit code and stderr.
fn run(mode: &[&str], source: &str, args: &[&str]) -> (Option<i32>, String) {
    let path = temp_module("aver-main-exit", source);
    let root = path.parent().expect("temp module has parent");
    let mut command = Command::new(aver_bin());
    command
        .current_dir(root)
        .arg("run")
        .arg(&path)
        .arg("--module-root")
        .arg(root)
        .args(mode);
    if !args.is_empty() {
        command.arg("--").args(args);
    }
    let out = command.output().expect("run aver");
    cleanup(&path);
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    assert!(
        out.status.code().is_some(),
        "no exit code:\n{}",
        format_output(&out)
    );
    (out.status.code(), stderr)
}

/// The whole table on one runner. `out_of_range_says_why` is false only on
/// wasip2, where the component has no channel for the message.
fn assert_exit_statuses(mode: &[&str], out_of_range_says_why: bool) {
    assert_eq!(run(mode, LITERAL_INT, &[]).0, Some(3), "{mode:?} literal");
    assert_eq!(run(mode, ARG_INT, &["0"]).0, Some(0), "{mode:?} zero");
    assert_eq!(run(mode, ARG_INT, &["255"]).0, Some(255), "{mode:?} 255");
    for outside in ["256", "-1", "100000000000000000000"] {
        let (code, stderr) = run(mode, ARG_INT, &[outside]);
        assert_eq!(code, Some(1), "{mode:?} {outside}: {stderr}");
        if out_of_range_says_why && outside != "100000000000000000000" {
            assert!(
                stderr.contains(&format!(
                    "main returned exit code {outside}, outside 0..255"
                )),
                "{mode:?} {outside}: {stderr}"
            );
        }
    }
    assert_eq!(run(mode, ARG_COUNT, &[]).0, Some(0), "{mode:?} no args");
    assert_eq!(
        run(mode, ARG_COUNT, &["a", "b"]).0,
        Some(2),
        "{mode:?} two args"
    );
    assert_eq!(run(mode, UNIT, &[]).0, Some(0), "{mode:?} Unit");
    let (code, stderr) = run(mode, ERR, &[]);
    assert_eq!(code, Some(1), "{mode:?} Err: {stderr}");
}

#[test]
fn vm_main_return_is_the_exit_status() {
    assert_exit_statuses(&[], true);
    // A Big is said on stderr too.
    let (_, stderr) = run(&[], ARG_INT, &["100000000000000000000"]);
    assert!(
        stderr.contains("main returned exit code 100000000000000000000, outside 0..255"),
        "{stderr}"
    );
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_gc_main_return_is_the_exit_status() {
    assert_exit_statuses(&["--wasm-gc"], true);
}

#[cfg(feature = "wasip2")]
#[test]
fn wasip2_main_return_is_the_exit_status() {
    assert_exit_statuses(&["--wasip2"], false);
}
