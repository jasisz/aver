//! Missing custom providers fail only concrete verify cases that dispatch them.

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

const HASH160_SOURCE: &str = "module Hash160
    kind = capability
    semantics = pure
    exposes [digest]

operation digest(input: List<Int>) -> String
    ? \"Hash bytes through a custom provider.\"
";

const MAIN_SOURCE: &str = "module Main
    depends [Hash160]
    exposes [hashed, unrelated]

fn hashed(input: List<Int>) -> String
    ? \"Reach the unbound capability.\"
    Hash160.digest(input)

verify hashed
    hashed([]) => \"b472a266d0bd89c13706a4132ccfb16f7c3b9fcb\"

fn unrelated(n: Int) -> Int
    ? \"Touch no capability.\"
    n + 1

verify unrelated
    unrelated(1) => 2
    unrelated(0) => 1

fn main() -> Int
    unrelated(1)
";

const STUBBED_MAIN_SOURCE: &str = "module Main
    depends [Hash160]
    exposes [hashed, unrelated]

fn fixtureHash(input: List<Int>) -> String
    ? \"Return a published-vector stand-in for the provider.\"
    \"b472a266d0bd89c13706a4132ccfb16f7c3b9fcb\"

fn hashed(input: List<Int>) -> String
    ? \"Reach the capability through a verify-time provider stub.\"
    Hash160.digest(input)

verify hashed
    given hash: Hash160.digest = [fixtureHash]
    hashed([]) => \"b472a266d0bd89c13706a4132ccfb16f7c3b9fcb\"

fn unrelated(n: Int) -> Int
    ? \"Touch no capability.\"
    n + 1

verify unrelated
    unrelated(1) => 2

fn main() -> Int
    unrelated(1)
";

const NAMESPACED_PROBE_SOURCE: &str = "module Probe
    intent = \"A pure capability below a module namespace.\"
    kind = capability
    semantics = pure
    exposes [answer]

operation answer(n: Int) -> Int
    ? \"Return the provider's answer.\"
";

const NAMESPACED_STUBBED_MAIN_SOURCE: &str = "module Main
    depends [Sub.Probe]
    exposes [doubled]

fn doubled(n: Int) -> Int
    ? \"Double a namespaced capability result.\"
    Sub.Probe.answer(n) * 2

fn fixtureAnswer(n: Int) -> Int
    ? \"Stand in for the namespaced provider.\"
    n + 100

verify doubled
    given probe: Sub.Probe.answer = [fixtureAnswer]
    doubled(2) => 204
";

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

fn write_fixture(root: &Path) {
    fs::write(root.join("Hash160.av"), HASH160_SOURCE).expect("write capability module");
    fs::write(root.join("main.av"), MAIN_SOURCE).expect("write entry module");
}

fn write_stubbed_fixture(root: &Path) {
    fs::write(root.join("Hash160.av"), HASH160_SOURCE).expect("write capability module");
    fs::write(root.join("main.av"), STUBBED_MAIN_SOURCE).expect("write entry module");
}

#[test]
fn plain_verify_case_can_bind_a_pure_capability_operation_to_an_aver_stub() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_stubbed_fixture(temp.path());
    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        output.status.success(),
        "the plain case's capability stub must satisfy dispatch:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("hashed") && report.contains("1/1"),
        "the capability-backed case must pass:\n{report}"
    );
    assert!(
        !report.contains("capability provider missing"),
        "verify-time stubbing must precede provider dispatch:\n{report}"
    );
}

#[test]
fn plain_verify_case_can_bind_a_namespaced_capability_by_its_canonical_path() {
    let temp = tempfile::tempdir().expect("temporary module root");
    fs::create_dir_all(temp.path().join("sub")).expect("create module namespace");
    fs::write(temp.path().join("sub/probe.av"), NAMESPACED_PROBE_SOURCE)
        .expect("write namespaced capability module");
    fs::write(temp.path().join("main.av"), NAMESPACED_STUBBED_MAIN_SOURCE)
        .expect("write namespaced capability consumer");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        output.status.success(),
        "the canonical namespaced operation must install its stub:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("doubled") && report.contains("1/1"),
        "the namespaced capability-backed case must pass:\n{report}"
    );
}

#[test]
fn unresolved_operation_given_fails_statically_with_the_canonical_path() {
    let temp = tempfile::tempdir().expect("temporary module root");
    fs::create_dir_all(temp.path().join("sub")).expect("create module namespace");
    fs::write(temp.path().join("sub/probe.av"), NAMESPACED_PROBE_SOURCE)
        .expect("write namespaced capability module");
    let short_name = NAMESPACED_STUBBED_MAIN_SOURCE.replacen(
        "given probe: Sub.Probe.answer",
        "given probe: Probe.answer",
        1,
    );
    fs::write(temp.path().join("main.av"), short_name)
        .expect("write consumer with a near-miss given");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "an unresolved operation-shaped given must fail before runtime:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains(
            "given 'probe': unknown capability operation or classified effect 'Probe.answer'"
        ) && report.contains("Did you mean the full canonical path 'Sub.Probe.answer'"),
        "the diagnostic must resolve the short spelling to its canonical candidate:\n{report}"
    );
    assert!(
        !report.contains("capability-provider-missing")
            && !report.contains("capability provider missing"),
        "the invalid binding must never reach provider dispatch:\n{report}"
    );
}

#[test]
fn plain_capability_given_works_through_the_loaded_verify_entry_point() {
    let items = aver::source::parse_source(STUBBED_MAIN_SOURCE).expect("parse entry module");
    let loaded = vec![aver::source::LoadedModule {
        dep_name: "Hash160".to_string(),
        items: aver::source::parse_source(HASH160_SOURCE).expect("parse capability module"),
        path: "Hash160.av".into(),
    }];
    let results = aver::diagnostics::vm_verify::run_verify_for_items_vm_with_loaded(
        items, loaded, None, "main.av",
    )
    .expect("loaded verify path must accept a cases-form capability stub");

    let hashed = results
        .iter()
        .find(|result| result.fn_name == "hashed")
        .expect("hashed verify result");
    assert_eq!((hashed.passed, hashed.failed), (1, 0));
}

#[test]
fn verify_time_capability_given_does_not_install_a_normal_run_provider() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_stubbed_fixture(temp.path());
    let output = Command::new(aver_bin())
        .arg("run")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver program");
    assert!(
        !output.status.success(),
        "a verify-only stub must not satisfy normal provider preflight:\n{}",
        command_report(&output)
    );
    assert!(
        command_report(&output).contains("capability provider missing for 'Hash160.digest'"),
        "normal run must remain provider-strict:\n{}",
        command_report(&output)
    );
}

#[test]
fn plain_capability_given_rejects_a_stub_with_the_wrong_contract_shape() {
    let temp = tempfile::tempdir().expect("temporary module root");
    fs::write(temp.path().join("Hash160.av"), HASH160_SOURCE).expect("write capability module");
    let bad_main = STUBBED_MAIN_SOURCE.replacen(
        "fn fixtureHash(input: List<Int>) -> String",
        "fn fixtureHash(input: String) -> String",
        1,
    );
    fs::write(temp.path().join("main.av"), bad_main).expect("write entry module");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "a wrong provider-stub shape must fail before dispatch:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("given 'hash: Hash160.digest' expects a stub of type")
            && report.contains("Pure capability stubs use the operation's contract signature"),
        "the diagnostic must explain the exact pure-operation shape:\n{report}"
    );
}

#[test]
fn plain_verify_case_keeps_the_existing_effectful_capability_oracle_shape() {
    let temp = tempfile::tempdir().expect("temporary module root");
    fs::write(
        temp.path().join("Clock.av"),
        "module Clock
    kind = capability
    semantics = effectful
    exposes [now]
    effects []

operation now() -> Int
    oracle = generative
    replay = recorded
",
    )
    .expect("write capability module");
    fs::write(
        temp.path().join("main.av"),
        "module Main
    depends [Clock]
    effects [Clock.now]

fn seven(path: BranchPath, call: Int) -> Int
    7

fn tick() -> Int
    ! [Clock.now]
    Clock.now()

verify tick
    given clock: Clock.now = [seven]
    tick() => 7
",
    )
    .expect("write entry module");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        output.status.success(),
        "plain cases must retain BranchPath/counter Oracle dispatch for effectful capabilities:\n{}",
        command_report(&output)
    );
}

#[test]
fn separate_plain_blocks_for_one_function_keep_their_own_provider_stubs() {
    let temp = tempfile::tempdir().expect("temporary module root");
    fs::write(temp.path().join("Hash160.av"), HASH160_SOURCE).expect("write capability module");
    fs::write(
        temp.path().join("main.av"),
        "module Main
    depends [Hash160]

fn firstHash(input: List<Int>) -> String
    \"first\"

fn secondHash(input: List<Int>) -> String
    \"second\"

fn hashed(input: List<Int>) -> String
    Hash160.digest(input)

verify hashed
    given hash: Hash160.digest = [firstHash]
    hashed([1]) => \"first\"

verify hashed
    given hash: Hash160.digest = [secondHash]
    hashed([2]) => \"second\"
",
    )
    .expect("write entry module");

    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        output.status.success(),
        "each cases-form block must retain its own provider-stub world:\n{}",
        command_report(&output)
    );
}

#[test]
fn unrelated_cases_survive_an_unbound_capability_in_the_same_program() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_fixture(temp.path());
    let output = Command::new(aver_bin())
        .arg("verify")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver verify");
    assert!(
        !output.status.success(),
        "the reached capability case must still fail:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("hashed") && report.contains("capability provider missing"),
        "the reached case must fail at provider dispatch:\n{report}"
    );
    assert!(
        report.contains("unrelated") && report.contains("2/2"),
        "unrelated cases must keep running:\n{report}"
    );
    assert!(
        !report.contains("capability contract missing at runtime"),
        "verify must install the checked contract registry:\n{report}"
    );
}

#[test]
fn normal_run_keeps_strict_whole_program_provider_preflight() {
    let temp = tempfile::tempdir().expect("temporary module root");
    write_fixture(temp.path());
    let output = Command::new(aver_bin())
        .arg("run")
        .arg(temp.path().join("main.av"))
        .arg("--module-root")
        .arg(temp.path())
        .output()
        .expect("run aver program");
    assert!(
        !output.status.success(),
        "normal execution must keep strict provider preflight:\n{}",
        command_report(&output)
    );
    let report = command_report(&output);
    assert!(
        report.contains("capability provider missing for 'Hash160.digest'"),
        "normal execution must reject the unbound program before main runs:\n{report}"
    );
}

#[test]
fn loaded_verify_path_has_the_same_case_granularity() {
    let items = aver::source::parse_source(MAIN_SOURCE).expect("parse entry module");
    let loaded = vec![aver::source::LoadedModule {
        dep_name: "Hash160".to_string(),
        items: aver::source::parse_source(HASH160_SOURCE).expect("parse capability module"),
        path: "Hash160.av".into(),
    }];
    let results = aver::diagnostics::vm_verify::run_verify_for_items_vm_with_loaded(
        items, loaded, None, "main.av",
    )
    .expect("loaded verify path must compile the checked capability contract");

    let hashed = results
        .iter()
        .find(|result| result.fn_name == "hashed")
        .expect("hashed verify result");
    assert_eq!((hashed.passed, hashed.failed), (0, 1));
    assert!(matches!(
        &hashed.case_results[0].outcome,
        aver::checker::VerifyCaseOutcome::RuntimeError { error }
            if error.contains("capability provider missing for 'Hash160.digest'")
    ));

    let unrelated = results
        .iter()
        .find(|result| result.fn_name == "unrelated")
        .expect("unrelated verify result");
    assert_eq!((unrelated.passed, unrelated.failed), (2, 0));
}
