//! Proof export of functions that perform a generic capability operation.
//!
//! `Wait.poll<K>` is generic over the key its wait set is keyed by, and a
//! program keys every wait the same way. The export types the oracle of a
//! lifted function at the program's key; a program that keys its waits two
//! ways has no such type, and its claims are declined with a reason rather
//! than aborting the whole export (#1449).

use super::*;

fn summary_from(output: &std::process::Output) -> serde_json::Value {
    let line = output
        .stdout
        .split(|byte| *byte == b'\n')
        .rev()
        .find_map(|line| {
            std::str::from_utf8(line)
                .ok()
                .filter(|text| text.starts_with('{'))
        })
        .unwrap_or_else(|| panic!("no JSON summary:\n{}", format_output(output)));
    serde_json::from_str(line).unwrap_or_else(|error| {
        panic!(
            "invalid JSON summary ({error}): {line}\n{}",
            format_output(output)
        )
    })
}

/// Export `source` as module `name` with `--check`, and return the summary
/// and the generated entry file.
fn prove(name: &str, source: &str, extra: &[&str]) -> Option<(serde_json::Value, String)> {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping generic oracle proof test: `lake` not available");
        return None;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source_dir = temp_output_dir("aver-generic-oracle-src");
    std::fs::create_dir_all(&source_dir).expect("create source dir");
    let file = source_dir.join(format!("{}.av", name.to_lowercase()));
    std::fs::write(&file, source).expect("write source");
    let output_dir = temp_output_dir("aver-generic-oracle-out");
    let proof = Command::new(aver_bin)
        .arg("proof")
        .arg(&file)
        .arg("--module-root")
        .arg(&source_dir)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .args(extra)
        .output()
        .expect("run proof export");
    let summary = summary_from(&proof);
    let lean = std::fs::read_to_string(output_dir.join(format!("{name}.lean")))
        .unwrap_or_else(|error| panic!("read {name}.lean ({error}):\n{}", format_output(&proof)));
    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
    Some((summary, lean))
}

#[test]
fn lean_types_the_wait_poll_oracle_at_the_programs_key() {
    let Some((summary, lean)) = prove(
        "WaitKeyed",
        r#"module WaitKeyed
    intent = "Wait.poll keyed by a sum type, with cases through the pure branch."
    exposes [polled, pollTwice, Watch]
    depends [Wait]
    effects [Wait.poll]

type Watch
    Peer(Int)
    Listener

fn polled(items: Map<Watch, Wait.Item>, timeoutMs: Int) -> Result<List<Watch>, String>
    ? "Which watches are ready; none, when nothing is watched."
    ! [Wait.poll]
    match Map.len(items) == 0
        true -> Result.Ok([])
        false -> Wait.poll(items, timeoutMs)

verify polled
    polled({}, 10) => Result.Ok([])

fn pollTwice(items: Map<Watch, Wait.Item>) -> Result<Int, String>
    ? "Counts the ready watches over two polls, through a helper that polls."
    ! [Wait.poll]
    first = polled(items, 0)?
    second = polled(items, 5)?
    Result.Ok(List.len(first) + List.len(second))

verify pollTwice
    pollTwice({}) => Result.Ok(0)
"#,
        &[],
    ) else {
        return;
    };
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
            summary["declined"].as_u64().unwrap_or(0),
        ),
        (Some(true), Some(0), Some(0), 0),
        "both cases must build without a sorry or a refusal:\n{summary}\n{lean}"
    );
    let oracle = "rnd_Wait_poll : BranchPath → Int → (List (Watch × Wait.Item)) → Int → Except String (List Watch)";
    assert!(
        lean.contains(&format!("def polled (path : BranchPath) ({oracle})")),
        "the lifted function takes the oracle at the program's key:\n{lean}"
    );
    assert!(
        lean.contains(&format!("example ({oracle}) : pollTwice")),
        "a case through a polling helper quantifies over the same oracle:\n{lean}"
    );
}

#[test]
fn lean_declines_the_claims_of_a_program_that_keys_its_waits_two_ways() {
    let Some((summary, lean)) = prove(
        "TwoKeys",
        r#"module TwoKeys
    intent = "One function polls a wait set keyed by Int and another keyed by a sum type."
    exposes [pollBoth, Watch]
    depends [Wait]
    effects [Wait.poll]

type Watch
    Peer(Int)
    Listener

fn pollBoth(numbered: Map<Int, Wait.Item>, named: Map<Watch, Wait.Item>) -> Result<Int, String>
    ? "Counts what is ready in both sets; nothing, when both are empty."
    ! [Wait.poll]
    match Map.len(numbered) + Map.len(named) == 0
        true -> Result.Ok(0)
        false -> twoPolls(numbered, named)

fn twoPolls(numbered: Map<Int, Wait.Item>, named: Map<Watch, Wait.Item>) -> Result<Int, String>
    ? "Polls each set once."
    ! [Wait.poll]
    a = Wait.poll(numbered, 0)?
    b = Wait.poll(named, 0)?
    Result.Ok(List.len(a) + List.len(b))

verify pollBoth
    pollBoth({}, {}) => Result.Ok(0)
"#,
        &["--declined-budget", "1"],
    ) else {
        return;
    };
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["build_errors"].as_u64(),
            summary["declined"].as_u64().unwrap_or(0),
        ),
        (Some(true), Some(0), 1),
        "the one claim is declined and the rest of the export builds:\n{summary}\n{lean}"
    );
    assert!(
        lean.contains("-- verify pollBoth: `pollBoth` is not exported: `Wait.poll` is generic over its key and this program does not settle one key type"),
        "the refusal names the unsettled key:\n{lean}"
    );
}
