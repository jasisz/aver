//! Jobs running on the VM (jasisz/aver#1329, leg 1.2).
//!
//! Leg 1.1 proved a job kind is a shape the compiler agrees about. This
//! suite proves the shape runs: a coordinator starts one job per input,
//! waits for them in one wait set, takes their results, and prints them;
//! a recording of that turn replays to the same turn; a cancelled job
//! refuses to be taken; a second job at the limit refuses instead of
//! blocking the turn; and a hostile wait still leaves the law standing.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

fn fixture(name: &str) -> PathBuf {
    repo_root().join("tests/fixtures").join(name)
}

fn aver(fixture_name: &str, args: &[&str]) -> Output {
    let dir = fixture(fixture_name);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg(args[0]).arg("main.av");
    command.arg("--module-root").arg(&dir);
    command.args(&args[1..]);
    command.output().expect("aver runs")
}

fn combined(out: &Output) -> String {
    format!(
        "{}{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    )
}

fn scratch(name: &str) -> PathBuf {
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock after the epoch")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("aver-work-{name}-{stamp}"));
    std::fs::create_dir_all(&dir).expect("scratch directory");
    dir
}

fn only_recording(dir: &Path) -> PathBuf {
    std::fs::read_dir(dir)
        .expect("recording directory")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .find(|path| {
            path.extension()
                .is_some_and(|extension| extension == "json")
        })
        .expect("one recording file")
}

#[test]
fn a_coordinator_starts_one_job_per_input_and_prints_every_result() {
    let out = aver("work_jobs", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(text.contains("job 1 scored 5"), "{}", format_output(&out));
    assert!(text.contains("job 2 scored 8"), "{}", format_output(&out));
}

#[test]
fn a_recorded_turn_of_jobs_replays_to_the_same_turn() {
    let dir = scratch("replay");
    let recorded = aver(
        "work_jobs",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));

    let recording = only_recording(&dir);
    let fixture_dir = fixture("work_jobs");
    let mut command = Command::new(aver_bin());
    command.current_dir(&fixture_dir);
    command.arg("replay").arg(&recording);
    command.arg("--check-args");
    let replayed = command.output().expect("aver replays");
    let text = combined(&replayed);
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        text.contains("job 1 scored 5"),
        "{}",
        format_output(&replayed)
    );
    assert!(
        text.contains("job 2 scored 8"),
        "{}",
        format_output(&replayed)
    );
    assert!(
        text.contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_replay_stops_when_the_bound_function_produces_a_different_result() {
    let dir = scratch("divergence");
    let recorded = aver(
        "work_jobs",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));
    let recording = only_recording(&dir);

    // Replay the same recording against a program whose bound function
    // scores one higher. The recording is the only thing that changed hands;
    // what the job computes is recomputed, so the two must disagree.
    let diverged = fixture("work_jobs_divergent");
    let mut command = Command::new(aver_bin());
    command.current_dir(&diverged);
    command.arg("replay").arg(&recording);
    let out = command.output().expect("aver replays");
    let text = combined(&out);
    assert!(
        text.contains("fail[replay-error]"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Replay divergence: job kind 'Validation'"),
        "{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_recorded_turn_of_record_valued_jobs_replays_to_the_same_turn() {
    let dir = scratch("record-replay");
    let recorded = aver(
        "work_jobs_record",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));

    // The task and the answer are records the capability owns, so the ledger
    // spells them the way the provider boundary does: `Scorer.Task` and
    // `Scorer.Report`, not whichever short name the program side used.
    let recording = only_recording(&dir);
    let text = std::fs::read_to_string(&recording).expect("recording reads");
    assert!(
        text.contains("\"type\": \"Scorer.Task\""),
        "the task must record under its canonical name:\n{text}"
    );
    assert!(
        text.contains("\"type\": \"Scorer.Report\""),
        "the answer must record under its canonical name:\n{text}"
    );

    let fixture_dir = fixture("work_jobs_record");
    let mut command = Command::new(aver_bin());
    command.current_dir(&fixture_dir);
    command.arg("replay").arg(&recording);
    command.arg("--check-args");
    let replayed = command.output().expect("aver replays");
    let text = combined(&replayed);
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        text.contains("job 1 scored 10 for alpha"),
        "{}",
        format_output(&replayed)
    );
    assert!(
        text.contains("job 2 scored 24 for beta-two"),
        "{}",
        format_output(&replayed)
    );
    assert!(
        text.contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_recording_spelling_its_records_by_short_names_still_replays() {
    // Older writers let the program-side short name leak into the ledger:
    // `Task` where the boundary writes `Scorer.Task`. That spelling is the
    // type's own short name — its one legitimate alias — so the recording
    // still replays.
    let dir = scratch("record-legacy");
    let recorded = aver(
        "work_jobs_record",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));
    let recording = only_recording(&dir);
    let text = std::fs::read_to_string(&recording).expect("recording reads");
    let legacy = text
        .replace("\"type\": \"Scorer.Task\"", "\"type\": \"Task\"")
        .replace("\"type\": \"Scorer.Report\"", "\"type\": \"Report\"");
    assert_ne!(legacy, text, "the rewrite must change the recording");
    std::fs::write(&recording, legacy).expect("recording writes");

    let fixture_dir = fixture("work_jobs_record");
    let mut command = Command::new(aver_bin());
    command.current_dir(&fixture_dir);
    command.arg("replay").arg(&recording);
    command.arg("--check-args");
    let replayed = command.output().expect("aver replays");
    let text = combined(&replayed);
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        text.contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_recording_naming_a_foreign_record_type_is_a_diagnostic() {
    // `Other.Report` shares the short name but not the nominal type: the tag
    // is the identity, so the replay refuses it instead of decoding a record
    // of the wrong type — and says so, without a panic.
    let dir = scratch("record-foreign");
    let recorded = aver(
        "work_jobs_record",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));
    let recording = only_recording(&dir);
    let text = std::fs::read_to_string(&recording).expect("recording reads");
    let foreign = text.replace("\"type\": \"Scorer.Report\"", "\"type\": \"Other.Report\"");
    assert_ne!(foreign, text, "the rewrite must change the recording");
    std::fs::write(&recording, foreign).expect("recording writes");

    let fixture_dir = fixture("work_jobs_record");
    let mut command = Command::new(aver_bin());
    command.current_dir(&fixture_dir);
    command.arg("replay").arg(&recording);
    let out = command.output().expect("aver replays");
    let text = combined(&out);
    assert!(
        text.contains("fail[replay-error]"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Other.Report") && text.contains("Scorer.Report"),
        "the diagnostic must name both the foreign and the expected type:\n{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_replay_stops_when_a_record_valued_job_produces_a_different_result() {
    let dir = scratch("record-divergence");
    let recorded = aver(
        "work_jobs_record",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));
    let recording = only_recording(&dir);

    // The bound function scores one higher in the divergent copy, so the
    // recomputed answer must disagree with the recorded report — and the
    // divergence names the record under its canonical type.
    let diverged = fixture("work_jobs_record_divergent");
    let mut command = Command::new(aver_bin());
    command.current_dir(&diverged);
    command.arg("replay").arg(&recording);
    let out = command.output().expect("aver replays");
    let text = combined(&out);
    assert!(
        text.contains("fail[replay-error]"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Replay divergence: job kind 'Scorer'"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("Scorer.Report(score:"),
        "the divergence must show the record under its canonical name:\n{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_recorded_turn_of_a_unit_task_job_replays_to_the_same_turn() {
    let dir = scratch("unit-task-replay");
    let recorded = aver(
        "work_jobs_unit_task",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
    );
    assert!(recorded.status.success(), "{}", format_output(&recorded));

    let recording = only_recording(&dir);
    let fixture_dir = fixture("work_jobs_unit_task");
    let mut command = Command::new(aver_bin());
    command.current_dir(&fixture_dir);
    command.arg("replay").arg(&recording);
    command.arg("--check-args");
    let replayed = command.output().expect("aver replays");
    let text = combined(&replayed);
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        text.contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_cancelled_job_refuses_to_be_taken() {
    let out = aver("work_jobs_cancel", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("work: job cancelled"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn a_job_kind_refuses_a_job_another_job_kind_started() {
    // Every job kind of a program shares one engine and `Work.Job` is one
    // stdlib type, so the type checker cannot tell two kinds' handles apart.
    // The runtime must: answering with the other kind's result would hand the
    // program a value it never asked for.
    let out = aver("work_jobs_two_kinds", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(
        text.contains("beta: work: this job was not started by job kind 'Beta'"),
        "{}",
        format_output(&out)
    );
    assert!(text.contains("alpha: scored 5"), "{}", format_output(&out));
}

#[test]
fn a_manifest_that_does_not_load_reports_its_own_error() {
    // A manifest whose `[work]` section is wrong is not a manifest without
    // bindings: reporting it as a missing binding names a cause that is not
    // there and offers a repair that cannot help.
    let dir = scratch("manifest");
    let source = fixture("work_jobs");
    for name in ["main.av", "node.av", "validation.av", "aver.toml"] {
        std::fs::copy(source.join(name), dir.join(name)).expect("fixture file copies");
    }
    let manifest = dir.join("aver.toml");
    let text = std::fs::read_to_string(&manifest).expect("manifest reads");
    std::fs::write(&manifest, format!("{text}\n[work]\nmax-jobs = 0\n")).expect("manifest writes");

    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg("run").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    assert!(
        text.contains("[work].max-jobs must be a positive integer, got 0"),
        "{}",
        format_output(&out)
    );
    assert!(!text.contains("work-binding"), "{}", format_output(&out));
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_second_job_at_the_limit_refuses_instead_of_blocking_the_turn() {
    let out = aver("work_jobs_limit", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("work: job limit 1 reached"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn a_given_on_take_verifies_a_function_that_collects_a_job() {
    let out = aver("work_jobs", &["verify"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        combined(&out).contains("scoreOnce      1/1"),
        "{}",
        format_output(&out)
    );
}

// TODO(owner): the law this test asserts holds over an empty wait set, so
// `pollEverythingReady` has no key to invent and nothing about job readiness
// is proved. The law that would prove it — begin a job under a `given` stub,
// poll `{1 => Wait.Item.Job(job)}`, and check the reported keys never exceed
// the set — cannot be written today: verify answers it with
// `Runtime error: unknown boundary type 'Work.Job'`, and the same shape over
// `Tcp.poll` with a stubbed `Tcp.Connection` fails identically, so the limit
// is verify's value codec for capability resources under a stub, not this
// leg. Closing it is a change to `src/provider/value.rs` that touches every
// capability resource, which this brief does not cover.
#[test]
fn a_hostile_wait_that_reports_everything_ready_leaves_the_law_standing() {
    let out = aver("work_jobs", &["verify", "--hostile"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    assert!(
        text.contains("anEmptyWaitSetReportsNoKey      4/4"),
        "{}",
        format_output(&out)
    );
    assert!(text.contains("0 failed"), "{}", format_output(&out));
}
