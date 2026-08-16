//! The shadowing ban at the toolchain's front doors (issue #954).
//!
//! A binder — fn parameter, statement binding, or match-pattern
//! binding — may not spell a name already visible at that point: an
//! enclosing local, a top-level fn or operation of the same module, or
//! the enclosing function's own name. The resolver-side walk and the
//! full (binder position × shadowed kind) matrix live in
//! `src/resolver.rs` unit tests; this file pins that the refusal
//! reaches the user through the real commands — `aver run`,
//! `aver compile` and `aver verify` (both its disk-loader entry and
//! the pre-loaded virtual-filesystem entry the playground and the LSP
//! use) — and that the program from issue #951, whose three executors
//! disagreed about a call through a binder spelling a module fn's
//! name, is now rejected before any executor sees it.
//!
//! Sibling match arms reusing a name stay LEGAL (they are never in
//! each other's scope; the per-arm slot machinery from #949 keeps them
//! sound) — the control below must keep running. So does a module that
//! spells a function the way a hostile effect stub spells one of its
//! parameters: those stubs are fabricated by the compiler, and the ban
//! is about the program the user wrote.
//!
//! `every_front_door_refuses_the_951_program` is the durable half:
//! doors reach the type checker by different routes, so no grep for one
//! route can prove they are all gated. That test drives every one of
//! them with the same program and then asserts that every subcommand
//! `aver --help` lists is either driven or carries a written reason for
//! not being a door — so the next door lands there for a decision
//! instead of slipping past.

#![cfg(feature = "runtime")]

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

static UNIQUE: AtomicU64 = AtomicU64::new(0);

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn temp_module(prefix: &str, source: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-shadow-ban-{prefix}-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    let path = dir.join("main.av");
    fs::write(&path, source).expect("write temp module source");
    path
}

fn cleanup(path: &Path) {
    let _ = fs::remove_dir_all(path.parent().expect("temp module has parent"));
}

fn format_output(out: &std::process::Output) -> String {
    format!(
        "status: {}\nstdout:\n{}\nstderr:\n{}",
        out.status,
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    )
}

/// The issue-#951 program, verbatim shape: the VM failed at runtime
/// while compiled Rust and the self-host answered 6. Now no executor
/// gets to have an opinion.
const DIVERGENCE_951_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn probe(x: Int) -> Int
    match Option.Some(x)
        Option.Some(dbl) -> dbl(3)
        Option.None -> 0

fn main()
    ! [Console.print]
    Console.print(String.fromInt(probe(3)))
"#;

const DIVERGENCE_951_ERROR: &str = "the pattern binding 'dbl' shadows the function 'dbl' \
     defined at line 3; every name means one thing in its scope — rename one of them";

#[test]
fn issue_951_divergence_program_is_rejected_by_run() {
    let path = temp_module("run-951", DIVERGENCE_951_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "the #951 program must be rejected, not executed:\n{}",
        format_output(&out)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(DIVERGENCE_951_ERROR),
        "the refusal must name the binder, what it shadows, and where it lives:\n{}",
        format_output(&out)
    );
    assert!(
        stderr.contains("error[8:0]"),
        "the error position must point at the BINDER's line:\n{}",
        format_output(&out)
    );
}

#[test]
fn issue_951_divergence_program_is_rejected_by_compile() {
    let path = temp_module("compile-951", DIVERGENCE_951_SRC);
    let module_root = path.parent().expect("temp module has parent").to_path_buf();
    let project = module_root.join("project");
    fs::create_dir_all(&project).expect("create project dir");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("compile")
        .arg(&path)
        .arg("--target")
        .arg("rust")
        .arg("--name")
        .arg("shadow_reject")
        .arg("-o")
        .arg(&project)
        .arg("--module-root")
        .arg(&module_root)
        .output()
        .expect("expected `aver compile` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "the #951 program must be rejected by compile too:\n{}",
        format_output(&out)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(DIVERGENCE_951_ERROR),
        "compile must report the same refusal run does:\n{}",
        format_output(&out)
    );
}

/// The same program carrying a `verify` block instead of a `main`.
/// `aver verify` compiles and RUNS the function under test, so it is a
/// front door in exactly the sense that matters: before this, it ran
/// the #951 program and reported the divergence as a runtime fault
/// ("cannot call non-function (got Unit)") while `run` and `compile`
/// refused the same file.
const DIVERGENCE_951_VERIFY_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn probe(x: Int) -> Int
    match Option.Some(x)
        Option.Some(dbl) -> dbl(3)
        Option.None -> 0

verify probe
    probe(3) => 6
"#;

#[test]
fn issue_951_divergence_program_is_rejected_by_verify() {
    let path = temp_module("verify-951", DIVERGENCE_951_VERIFY_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("verify")
        .arg(&path)
        .output()
        .expect("expected `aver verify` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "the #951 program must be rejected by verify, not executed:\n{}",
        format_output(&out)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(DIVERGENCE_951_ERROR),
        "verify must report the same refusal run and compile do:\n{}",
        format_output(&out)
    );
    assert!(
        !stderr.contains("cannot call non-function"),
        "verify must refuse the program instead of executing it into the \
         divergence:\n{}",
        format_output(&out)
    );
}

/// The other verify entry point: pre-loaded modules instead of a disk
/// loader. This is the one the playground and the LSP call, and it had
/// its own `pipeline::typecheck` call, so routing only the disk path
/// would have left the browser executing what the CLI refuses.
#[test]
fn issue_951_divergence_program_is_rejected_by_the_preloaded_verify_path() {
    let items = aver::source::parse_source(DIVERGENCE_951_VERIFY_SRC)
        .expect("the #951 program parses; it is the meaning that is banned");
    let Err(err) = aver::diagnostics::vm_verify::run_verify_for_items_vm_with_loaded(
        items,
        Vec::new(),
        None,
        "main.av",
    ) else {
        panic!("the pre-loaded verify path must reject the #951 program, not run it");
    };
    assert!(
        err.contains(DIVERGENCE_951_ERROR),
        "the pre-loaded path must carry the standard shadow error, got:\n{err}"
    );
}

/// A parameter spelling a module fn's name is the same offence — this
/// is the shape the rogue example's `renderTile(…, isVisible, …)`
/// param had before its rename.
const PARAM_OVER_FN_SRC: &str = r#"module Tmp

fn area(s: Int) -> Int
    s * s

fn apply(area: Int) -> Int
    area + 1

fn main()
    ! [Console.print]
    Console.print(String.fromInt(apply(2)))
"#;

#[test]
fn a_param_spelling_a_module_fn_is_rejected_by_run() {
    let path = temp_module("run-param", PARAM_OVER_FN_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        !out.status.success(),
        "a param over a module fn must be rejected:\n{}",
        format_output(&out)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(
            "the parameter 'area' shadows the function 'area' defined at line 3; \
             every name means one thing in its scope — rename one of them"
        ),
        "the refusal must be the standard shadow error:\n{}",
        format_output(&out)
    );
}

/// The third verify door: `aver verify --wasm-gc`, which had a
/// `pipeline::typecheck` call of its own. Off the default lane (it
/// needs wasmtime), on in the wasm job.
#[cfg(feature = "wasm")]
#[test]
fn issue_951_divergence_program_is_rejected_by_wasm_gc_verify() {
    let items = aver::source::parse_source(DIVERGENCE_951_VERIFY_SRC)
        .expect("the #951 program parses; it is the meaning that is banned");
    let Err(err) = aver::diagnostics::wasm_gc_verify::run_verify_for_items_wasm_gc(
        items, None, None, "main.av",
    ) else {
        panic!("the wasm-gc verify door must reject the #951 program, not run it");
    };
    assert!(
        err.contains(DIVERGENCE_951_ERROR),
        "the wasm-gc door must carry the standard shadow error, got:\n{err}"
    );
}

/// The hostile verify path APPENDS compiler-fabricated effect stubs to
/// the item list before the typecheck gate, because the checker has to
/// see them. Their parameters are spelled `path`, `n`, `conn`, `count`
/// (and `min` / `max` / `key` for the other effects) — ordinary names a
/// user module is free to define as functions. The ban reads the
/// program the user wrote, so a module defining `fn count` must still
/// verify: run the ban over the post-injection list instead and this
/// module is refused for a binder it never wrote.
#[test]
fn a_module_naming_a_fn_like_a_hostile_stub_param_still_verifies() {
    let src = r#"module M
    intent = "A module whose own fn is spelled like a hostile stub's parameter."
    depends [Bytes]
    effects [Tcp]

fn count(n: Int) -> Int
    ? "How many frames have been seen so far."
    n + 1

fn frameVerdict(conn: Tcp.Connection) -> String
    ? "Classify one exact-frame read."
    ! [Tcp.readBytes]
    match Tcp.readBytes(conn, 4)
        Result.Ok(_) -> String.fromInt(count(0))
        Result.Err(_) -> "err"

verify frameVerdict law neverReads
    given conn: Tcp.Connection = [Tcp.Connection(id = "fake", host = "127.0.0.1", port = 1)]
    frameVerdict(conn) => "err"
"#;
    let items = aver::source::parse_source(src).expect("the guard program must parse");
    let results = aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(&repo_root().to_string_lossy().to_string()),
        "hostile_stub_param_scope.av",
        aver::verify_law::expand::ExpansionMode::Hostile,
    )
    .expect("a user fn spelled like a fabricated stub's parameter is not shadowing");
    assert_eq!(
        results.len(),
        1,
        "the hostile run must produce the block's results, not a refusal"
    );
}

/// SIBLING arms may reuse a name — the #949 per-arm slot machinery
/// keeps them sound, and the ban must not touch them.
const SIBLING_ARMS_SRC: &str = r#"module Tmp

type Shape
    Circle(Int)
    Square(Int)

fn eval(p: Shape) -> Int
    match p
        Shape.Circle(n) -> n + 1
        Shape.Square(n) -> n * 10

fn main()
    ! [Console.print]
    Console.print(String.fromInt(eval(Shape.Circle(5))))
    Console.print(String.fromInt(eval(Shape.Square(3))))
"#;

#[test]
fn sibling_arms_reusing_a_name_still_run() {
    let path = temp_module("run-siblings", SIBLING_ARMS_SRC);
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(&path)
        .output()
        .expect("expected `aver run` to execute");
    cleanup(&path);
    assert!(
        out.status.success(),
        "sibling arms are not in each other's scope — this program is legal:\n{}",
        format_output(&out)
    );
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "6\n30",
        "the sibling-arm program must still answer correctly"
    );
}

// ── The front doors, enumerated ──────────────────────────────────────

/// The #951 program with everything a door might ask for: a `main` to
/// run, a `verify` block to check, and the shadowing binder that all of
/// them must refuse. One file, every door.
const EVERY_DOOR_SRC: &str = r#"module Tmp

fn dbl(n: Int) -> Int
    n * 2

fn probe(x: Int) -> Int
    match Option.Some(x)
        Option.Some(dbl) -> dbl(3)
        Option.None -> 0

verify probe
    probe(3) => 6

fn main()
    ! [Console.print]
    Console.print(String.fromInt(probe(3)))
"#;

/// The same program as a REPL session: one entry per definition, blank
/// lines closing the blocks, then the call. `module` and `main` are not
/// REPL shapes, and the REPL numbers lines per ENTRY rather than per
/// file — which is why the doors are pinned on the two halves of the
/// message below and not on the whole string.
const EVERY_DOOR_REPL_SESSION: &str = "fn dbl(n: Int) -> Int\n    n * 2\n\n\
     fn probe(x: Int) -> Int\n    match Option.Some(x)\n        \
     Option.Some(dbl) -> dbl(3)\n        Option.None -> 0\n\nprobe(3)\n:quit\n";

/// What the refusal must name, and the rule it must state. The line
/// number sits between them: it is the SHADOWED declaration's, and the
/// REPL counts lines per entry, so it is the one part of the message
/// that legitimately differs between doors.
const BAN_NAMES_THE_BINDER: &str =
    "the pattern binding 'dbl' shadows the function 'dbl' defined at line";
const BAN_STATES_THE_RULE: &str = "every name means one thing in its scope — rename one of them";

/// The mark of an executor having got hold of the program: this is the
/// fault the VM reported for #951 before the ban, and no door may reach
/// it.
const EXECUTED_ANYWAY: &str = "cannot call non-function";

/// What one door did with the program.
struct DoorResult {
    /// stdout and stderr merged — doors differ on which one they use,
    /// and the assertion is about the message, not the stream.
    output: String,
    /// `Some(true)` when the door reported success. `None` for a door
    /// whose exit code is not a verdict on the program: the REPL is a
    /// loop that prints the refusal and reads the next line.
    succeeded: Option<bool>,
}

type DriveDoor = Box<dyn Fn(&Path) -> DoorResult>;

/// One front door: an entry point that takes a user's source and either
/// executes it or rejects it.
struct Door {
    /// How the door is spelled in this test's failure output.
    label: &'static str,
    /// The `aver` subcommand it belongs to, for the completeness check
    /// below. Several doors can share one (`run` and `run -e`).
    subcommand: &'static str,
    drive: DriveDoor,
}

fn cli_door(label: &'static str, subcommand: &'static str, args: Vec<String>) -> Door {
    Door {
        label,
        subcommand,
        drive: Box::new(move |path: &Path| {
            let out = Command::new(aver_bin())
                .current_dir(repo_root())
                .args(
                    args.iter()
                        .map(|a| a.replace("{file}", &path.to_string_lossy())),
                )
                .output()
                .unwrap_or_else(|e| panic!("expected `aver {}` to execute: {e}", args.join(" ")));
            DoorResult {
                output: format!(
                    "{}{}",
                    String::from_utf8_lossy(&out.stdout),
                    String::from_utf8_lossy(&out.stderr)
                ),
                succeeded: Some(out.status.success()),
            }
        }),
    }
}

/// Feed a REPL session to `aver repl` on stdin and collect everything it
/// prints. Dropping the pipe ends the session even if `:quit` is missed.
fn drive_repl(session: &str) -> DoorResult {
    let mut child = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("repl")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("expected `aver repl` to start");
    child
        .stdin
        .take()
        .expect("the repl reads stdin")
        .write_all(session.as_bytes())
        .expect("expected the repl to accept the session");
    let out = child.wait_with_output().expect("expected the repl to exit");
    DoorResult {
        output: format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        ),
        succeeded: None,
    }
}

fn front_doors(out_dir: &Path) -> Vec<Door> {
    let proof_out = out_dir.join("proof").to_string_lossy().into_owned();
    let rust_out = out_dir.join("rust").to_string_lossy().into_owned();
    let module_root = out_dir.to_string_lossy().into_owned();
    let mut doors = vec![
        cli_door("run", "run", vec!["run".into(), "{file}".into()]),
        cli_door(
            "run -e",
            "run",
            vec![
                "run".into(),
                "{file}".into(),
                "-e".into(),
                "probe(3)".into(),
            ],
        ),
        cli_door("check", "check", vec!["check".into(), "{file}".into()]),
        cli_door("verify", "verify", vec!["verify".into(), "{file}".into()]),
        cli_door("audit", "audit", vec!["audit".into(), "{file}".into()]),
        cli_door("shape", "shape", vec!["shape".into(), "{file}".into()]),
        cli_door(
            "bench",
            "bench",
            vec![
                "bench".into(),
                "{file}".into(),
                "--target".into(),
                "vm".into(),
                "--iterations".into(),
                "1".into(),
                "--warmup".into(),
                "0".into(),
            ],
        ),
        cli_door(
            "compile --target rust",
            "compile",
            vec![
                "compile".into(),
                "{file}".into(),
                "--target".into(),
                "rust".into(),
                "--name".into(),
                "shadow_reject".into(),
                "-o".into(),
                rust_out,
                "--module-root".into(),
                module_root,
            ],
        ),
        cli_door(
            "proof",
            "proof",
            vec!["proof".into(), "{file}".into(), "-o".into(), proof_out],
        ),
        Door {
            label: "repl",
            subcommand: "repl",
            drive: Box::new(|_path: &Path| drive_repl(EVERY_DOOR_REPL_SESSION)),
        },
    ];
    // Off the default lane — it needs wasmtime — and on in the wasm job.
    // `cfg!` rather than `#[cfg]` so the row is compiled either way and
    // cannot rot while the default lane is the one being run.
    if cfg!(feature = "wasm") {
        doors.push(cli_door(
            "verify --wasm-gc",
            "verify",
            vec!["verify".into(), "{file}".into(), "--wasm-gc".into()],
        ));
    }
    doors
}

/// Subcommands that are NOT front doors, each with the reason it cannot
/// reject a program. A subcommand belongs here only if it never decides
/// anything about a program's meaning.
const NOT_A_DOOR: &[(&str, &str)] = &[
    (
        "format",
        "rewrites source text and never type-checks it: a file with a shadowing binder \
         still formats, and must — you format code to read it, including code you are \
         about to fix",
    ),
    (
        "why",
        "reads descriptions and verify blocks off the parsed AST to report justification \
         coverage; it type-checks nothing and rejects nothing",
    ),
    (
        "context",
        "harvests fn signatures and TCO flags for an LLM context dump and discards \
         `tc_result.errors` entirely (diagnostics/context.rs::compute_context_fn_flags), \
         so a finding there would reach no one",
    ),
    (
        "replay",
        "executes through `pipeline::run`'s gated typecheck stage \
         (main/replay_cmd/backends.rs), but it is driven by a recording rather than by a \
         source path, and a recording of this program cannot exist: the only thing that \
         writes one is `aver run --record`, which refuses it at that same gate",
    ),
    (
        "cert",
        "consumes an emitted certificate directory and the wasm module it describes, not \
         Aver source",
    ),
    ("help", "prints help"),
];

/// Read the subcommand names out of `aver --help`.
fn cli_subcommands() -> Vec<String> {
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("--help")
        .output()
        .expect("expected `aver --help` to execute");
    let text = String::from_utf8_lossy(&out.stdout).into_owned();
    let mut names = Vec::new();
    let mut in_commands = false;
    for line in text.lines() {
        if line.starts_with("Commands:") {
            in_commands = true;
            continue;
        }
        if in_commands {
            if line.trim().is_empty() {
                break;
            }
            if let Some(name) = line.split_whitespace().next() {
                names.push(name.to_string());
            }
        }
    }
    assert!(
        names.len() > 5,
        "expected to read the subcommand list out of `aver --help`, got: {names:?}\n{text}"
    );
    names
}

/// EVERY front door, driven with the same program.
///
/// A door is an entry point that takes a user's source and either
/// executes it or rejects it. The ban has to hold at all of them, and
/// they do not reach the type checker by one route: most go through
/// `pipeline::run`'s typecheck stage, the three `verify` entries call
/// `pipeline::typecheck_gate` directly, and `aver repl` called the
/// checker itself — which is how it stayed ungated long enough to
/// type-check and RUN the program the ban exists to reject while `run`,
/// `check`, `compile`, `proof` and `verify` all refused the same file.
/// No grep for one route can prove they are all gated, so this drives
/// them instead.
///
/// The second half is what makes it durable: every subcommand
/// `aver --help` lists is either driven above or carries a written
/// reason for not being a door. Adding a subcommand that reaches the
/// type checker without the gate fails this test twice — once because
/// it is unclassified, and again the moment it is added to the table.
#[test]
fn every_front_door_refuses_the_951_program() {
    let path = temp_module("every-door", EVERY_DOOR_SRC);
    let out_dir = path.parent().expect("temp module has parent").to_path_buf();
    let doors = front_doors(&out_dir);

    for door in &doors {
        let result = (door.drive)(&path);
        assert!(
            result.succeeded != Some(true),
            "the `{}` door accepted the #951 program:\n{}",
            door.label,
            result.output
        );
        assert!(
            result.output.contains(BAN_NAMES_THE_BINDER)
                && result.output.contains(BAN_STATES_THE_RULE),
            "the `{}` door must refuse the #951 program with the ban's message:\n{}",
            door.label,
            result.output
        );
        assert!(
            !result.output.contains(EXECUTED_ANYWAY),
            "the `{}` door executed the #951 program into its divergence instead of \
             refusing it:\n{}",
            door.label,
            result.output
        );
    }

    // Completeness: no subcommand may be neither driven nor explained.
    let driven: Vec<&str> = doors.iter().map(|d| d.subcommand).collect();
    let subcommands = cli_subcommands();
    for name in &subcommands {
        let is_door = driven.contains(&name.as_str());
        let excused = NOT_A_DOOR.iter().any(|(n, _)| n == name);
        assert!(
            is_door ^ excused,
            "`aver {name}` is not classified: a subcommand is either a front door — driven \
             above with the #951 program, refusing it with the ban's message — or listed in \
             NOT_A_DOOR with the reason it cannot reject a program. Deciding which is the \
             point of this test."
        );
    }
    // And no stale classification: everything named above still exists.
    for name in driven
        .iter()
        .copied()
        .chain(NOT_A_DOOR.iter().map(|(n, _)| *n))
    {
        assert!(
            subcommands.iter().any(|a| a == name),
            "`{name}` is classified here but `aver --help` no longer lists it"
        );
    }

    cleanup(&path);
}

// ── The REPL door's own scope ────────────────────────────────────────

/// A REPL session is ONE module, assembled a turn at a time: the ban
/// reads every item entered so far together with the entry being read
/// now. So a binder entered now may not spell a function defined three
/// turns ago, exactly as it may not in a file where the two lines
/// appear in that order.
#[test]
fn the_repl_refuses_a_binder_that_shadows_an_earlier_turns_fn() {
    let result = drive_repl(
        "fn area(s: Int) -> Int\n    s * s\n\nfn apply(area: Int) -> Int\n    area + 1\n\napply(2)\n:quit\n",
    );
    assert!(
        result
            .output
            .contains("the parameter 'area' shadows the function 'area' defined at line")
            && result.output.contains(BAN_STATES_THE_RULE),
        "a parameter entered after the function it spells must be refused:\n{}",
        result.output
    );
    assert!(
        !result.output.contains("\n3\n"),
        "the repl must refuse the entry rather than answer with it:\n{}",
        result.output
    );
}

/// The other direction of the same decision, and the reason it is
/// liveable: the refused entry is NOT added to the session, so the
/// session never enters a shadowing state, and everything entered
/// before the refusal still runs.
#[test]
fn a_refused_repl_entry_leaves_the_session_usable() {
    let result = drive_repl(
        "fn area(s: Int) -> Int\n    s * s\n\nfn apply(area: Int) -> Int\n    area + 1\n\narea(4)\n:quit\n",
    );
    assert!(
        result
            .output
            .contains("the parameter 'area' shadows the function 'area' defined at line"),
        "the shadowing entry must be refused:\n{}",
        result.output
    );
    assert!(
        result.output.contains("16"),
        "the session must keep working after a refused entry:\n{}",
        result.output
    );
}

/// The control: the ban must not cost the REPL a legal session. Sibling
/// arms reusing a name are legal here too, and a name re-entered at the
/// prompt is a different question — the checker already answers it
/// ('x' is already defined), and the ban never reads top-level items.
#[test]
fn the_repl_still_runs_a_session_the_ban_allows() {
    let result = drive_repl(
        "fn size(s: Int) -> Int\n    s * s\n\nfn apply(edge: Int) -> Int\n    size(edge) + 1\n\napply(4)\n:quit\n",
    );
    assert!(
        !result.output.contains("shadows"),
        "a session with distinct names must not be refused:\n{}",
        result.output
    );
    assert!(
        result.output.contains("17"),
        "the legal session must still answer:\n{}",
        result.output
    );
}

/// A shadowing binder in a DEPENDENCY module is refused when the entry
/// is compiled — dep modules go through the same front door.
#[test]
fn a_shadowing_dep_module_is_rejected_by_run() {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let n = UNIQUE.fetch_add(1, Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("aver-shadow-ban-dep-{nanos}-{n}"));
    fs::create_dir_all(&dir).expect("create temp dir");
    fs::write(
        dir.join("main.av"),
        "module Main\n    depends [Helper]\n\nfn main()\n    ! [Console.print]\n    Console.print(String.fromInt(Helper.apply(2)))\n",
    )
    .expect("write entry");
    fs::write(
        dir.join("helper.av"),
        "module Helper\n    exposes [apply]\n\nfn area(s: Int) -> Int\n    s * s\n\nfn apply(area: Int) -> Int\n    area + 1\n",
    )
    .expect("write dep");
    let out = Command::new(aver_bin())
        .current_dir(repo_root())
        .arg("run")
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(&dir)
        .output()
        .expect("expected `aver run` to execute");
    let _ = fs::remove_dir_all(&dir);
    assert!(
        !out.status.success(),
        "a shadowing dep module must be rejected:\n{}",
        format_output(&out)
    );
    assert!(
        String::from_utf8_lossy(&out.stderr).contains(
            "the parameter 'area' shadows the function 'area' defined at line 4; \
             every name means one thing in its scope — rename one of them"
        ),
        "the dep refusal must carry the standard shadow error:\n{}",
        format_output(&out)
    );
}
