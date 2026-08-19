//! Plain verify cases may call an effect-declaring function, but they may not
//! cross an unstubbed effect boundary.

use std::fs;
use std::process::{Command, Output};

fn aver_bin() -> &'static str {
    env!("CARGO_BIN_EXE_aver")
}

fn command_report(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}

#[test]
fn plain_case_on_a_non_effectful_path_has_no_function_wide_effect_warning() {
    let temp = tempfile::tempdir().expect("temporary module root");
    let source = "module VerifyPath
    intent = \"A concrete case may stay on the in-memory arm.\"
    effects [Random.int]

type Bag
    Memory(Int)
    Randomized(Unit)

fn size(bag: Bag) -> Int
    ? \"Read memory directly or ask the host for a size.\"
    ! [Random.int]
    match bag
        Bag.Memory(held) -> held
        Bag.Randomized(_) -> Random.int(1, 6)

verify size
    size(Bag.Memory(2)) => 2
";
    let path = temp.path().join("main.av");
    fs::write(&path, source).expect("write effect-path fixture");

    let check = Command::new(aver_bin())
        .arg("check")
        .arg(&path)
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver check");
    assert!(
        check.status.success(),
        "the path fixture must typecheck:\n{}",
        command_report(&check)
    );
    let check_report = command_report(&check);
    assert!(
        !check_report.contains("warning[verify-effectful]")
            && !check_report.contains("test will flap"),
        "a function-wide warning cannot claim that this concrete case reaches Random.int:\n\
         {check_report}"
    );

    let verify = Command::new(aver_bin())
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        verify.status.success(),
        "the in-memory case reaches no effect and must remain a plain verify:\n{}",
        command_report(&verify)
    );
}

#[test]
fn plain_case_that_reaches_an_unstubbed_snapshot_stops_before_the_host() {
    let temp = tempfile::tempdir().expect("temporary module root");
    let source = "module VerifyHostRead
    intent = \"A plain case must not read an unstubbed host value.\"
    effects [Env.get]

fn observed() -> Option<String>
    ? \"Read a value that only the host process knows.\"
    ! [Env.get]
    Env.get(\"AVER_VERIFY_REACHED_EFFECT_TEST\")

verify observed
    observed() => Option.Some(\"host-value\")
";
    let path = temp.path().join("main.av");
    fs::write(&path, source).expect("write host-read fixture");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(temp.path())
        .env("AVER_VERIFY_REACHED_EFFECT_TEST", "host-value")
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "plain verify must stop instead of reading the process environment:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("plain verify reached unstubbed effect 'Env.get'")
            && report.contains("verify observed trace")
            && report.contains("given"),
        "the reached-effect error must name the operation and safe repair:\n{report}"
    );
}

#[test]
fn plain_case_that_reaches_an_output_effect_requires_trace() {
    let temp = tempfile::tempdir().expect("temporary module root");
    let source = "module VerifyOutput
    intent = \"A plain case must not silently erase a reached output effect.\"
    effects [Console.print]

fn announce() -> Int
    ? \"Emit a line before returning a stable value.\"
    ! [Console.print]
    Console.print(\"VERIFY-OUTPUT-SENTINEL\")
    1

verify announce
    announce() => 1
";
    let path = temp.path().join("main.av");
    fs::write(&path, source).expect("write output fixture");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "plain verify must not silently suppress a reached output effect:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("plain verify reached unstubbed effect 'Console.print'")
            && report.contains("verify announce trace"),
        "the output-effect error must point to trace verification:\n{report}"
    );
    assert!(
        !report.contains("VERIFY-OUTPUT-SENTINEL"),
        "the effect payload must not reach stdout/stderr:\n{report}"
    );
}

#[test]
fn plain_case_stops_before_an_unstubbed_effectful_capability_provider() {
    let temp = tempfile::tempdir().expect("temporary module root");
    let capability = "module Clock
    kind = capability
    semantics = effectful
    exposes [now]
    effects []

operation now() -> Int
    oracle = generative
    replay = recorded
";
    let source = "module VerifyCapability
    intent = \"A plain case must not dispatch an unstubbed effectful provider.\"
    depends [Clock]
    effects [Clock.now]

fn observed() -> Int
    ? \"Read the provider clock.\"
    ! [Clock.now]
    Clock.now()

verify observed
    observed() => 7
";
    fs::write(temp.path().join("Clock.av"), capability).expect("write capability contract");
    let path = temp.path().join("main.av");
    fs::write(&path, source).expect("write capability consumer");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "plain verify must stop before provider dispatch:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("plain verify reached unstubbed effect 'Clock.now'")
            && report.contains("verify observed trace"),
        "the reached capability error must own the failure:\n{report}"
    );
    assert!(
        !report.contains("capability-provider-missing")
            && !report.contains("capability provider missing"),
        "the effect guard must run before provider lookup:\n{report}"
    );
}

#[test]
fn plain_case_guard_cannot_be_bypassed_by_an_independent_product_child_vm() {
    let temp = tempfile::tempdir().expect("temporary module root");
    let source = "module VerifyParallelHostRead
    intent = \"Independent branches inherit the plain verify effect guard.\"
    effects [Env.get]

fn readHost() -> Option<String>
    ? \"Read a value known only to the host process.\"
    ! [Env.get]
    Env.get(\"AVER_VERIFY_PARALLEL_EFFECT_TEST\")

fn absent() -> Option<String>
    Option.None

fn observed() -> Tuple<Option<String>, Option<String>>
    ? \"Read the host in one independent branch.\"
    ! [Env.get]
    (readHost(), absent())!

verify observed
    observed() => (Option.Some(\"host-value\"), Option.None)
";
    let path = temp.path().join("main.av");
    fs::write(&path, source).expect("write parallel host-read fixture");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(&path)
        .arg("--module-root")
        .arg(temp.path())
        .env("AVER_VERIFY_PARALLEL_EFFECT_TEST", "host-value")
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "a child VM must not escape the plain verify effect guard:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("plain verify reached unstubbed effect 'Env.get'")
            && report.contains("verify observed trace"),
        "the parent VM must retain ownership of reached-effect diagnostics:\n{report}"
    );
}
