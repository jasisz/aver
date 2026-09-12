//! The generated loop (jasisz/aver#1329, leg 2.3).
//!
//! `tests/fixtures/run_all_slice/` is the whole claim in one program: five
//! processes, three answer modules, one job kind and three policies, with
//! every line between them generated. This suite runs it the way a user
//! would — `aver run`, `aver verify`, a recording and its replay, a hostile
//! wait, and the dump that shows what was generated — and holds the two
//! refusals that say what the program has to declare for the loop to be
//! generated at all.
//!
//! The slice answers `Wire` over real sockets: it binds a loopback listener
//! on the port it is run with, accepts one peer there and talks to it with
//! `Tcp.writeNow` and `Tcp.readNow`. The other side of that conversation is
//! the loopback peer below, which lives in this test and never in the
//! fixture, and the port is taken from a free one this test found rather
//! than fixed.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

const SLICE: &str = "run_all_slice";

/// How many block bodies the slice fetches over the wire: `Ledger.fresh`
/// makes the run three blocks long.
const BODIES: usize = 3;

/// How long one of those bodies is: `Ledger.blockOf` writes `block <height>`.
const BODY_LEN: usize = 7;

/// How long the peer holds a body back before echoing it. The read that
/// follows a finished write is asked in the very next turn, microseconds
/// after the last byte left, so the peer has to be slower than that for the
/// read to find nothing and park on `Connected` — which is the claim this
/// suite pins. A quarter of a second is far longer than a turn and far
/// shorter than the test's own patience.
const ECHO_DELAY: Duration = Duration::from_millis(250);

/// A loopback port nobody is listening on. The listener is bound only to be
/// told which port the kernel picked, then dropped, so the slice can bind it
/// itself: a `Tcp.Listener` the slice owns is the whole point of its
/// `Sockets` module, and a fixed port would collide with a parallel run.
fn free_port() -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind a loopback listener");
    listener.local_addr().expect("listener address").port()
}

/// Connects to the slice's listener, which is bound inside the first turn —
/// after this process has already been spawned — so the first attempts are
/// expected to be refused.
fn connect_when_bound(port: u16) -> TcpStream {
    let deadline = Instant::now() + Duration::from_secs(20);
    loop {
        match TcpStream::connect(("127.0.0.1", port)) {
            Ok(stream) => return stream,
            Err(error) => {
                assert!(
                    Instant::now() < deadline,
                    "the slice never bound 127.0.0.1:{port}: {error}"
                );
                thread::sleep(Duration::from_millis(10));
            }
        }
    }
}

/// The other side of `Wire`, in the test: one peer that takes every body the
/// slice writes it — in as many pieces as the slice sends them — holds it
/// back long enough for the read that follows to find nothing, and then
/// sends it back as the body that peer was asked for.
fn loopback_peer(port: u16) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut stream = connect_when_bound(port);
        for _ in 0..BODIES {
            let mut body = vec![0u8; BODY_LEN];
            stream
                .read_exact(&mut body)
                .expect("the body the slice wrote");
            thread::sleep(ECHO_DELAY);
            stream.write_all(&body).expect("send the body back");
        }
    })
}

/// Runs the slice against that peer, on a port this test picked, and gives
/// back what the run printed once the peer has played its whole part.
fn slice_with_peer(extra: &[&str]) -> Output {
    let port = free_port();
    let peer = loopback_peer(port);
    let port = port.to_string();
    let mut args: Vec<&str> = vec!["run"];
    args.extend_from_slice(extra);
    args.push("--");
    args.push(&port);
    let out = aver(SLICE, &args);
    assert!(out.status.success(), "{}", format_output(&out));
    peer.join()
        .expect("the loopback peer played its whole part");
    out
}

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
    let dir = std::env::temp_dir().join(format!("aver-run-all-{name}-{stamp}"));
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
fn the_slice_runs_to_the_end_on_the_vm_with_nothing_written_between_its_processes() {
    let out = slice_with_peer(&[]);
    let text = String::from_utf8_lossy(&out.stdout);
    // Every line of output comes from an in-place `Console.print` inside a
    // process, so counting them counts how far each process got: the peer
    // asks the pool four times (three heights and the Stop), and the walk
    // looks for a target four times (three commits and the empty chain).
    assert_eq!(
        text.matches("peer: asking the pool for work").count(),
        4,
        "{}",
        format_output(&out)
    );
    assert_eq!(
        text.matches("walk: looking for the next block to connect")
            .count(),
        4,
        "{}",
        format_output(&out)
    );
}

/// The wake gates the ask, measured on the two answer modules that park —
/// one of them over a real socket.
///
/// `Sockets.write` offers the socket one chunk per ask, so a body of seven
/// bytes is taken in two pieces: `Tcp.writeNow` answers four, the module
/// records that offset in its own state and parks on `Sending(connection)`,
/// and the ask after it sends the last three and answers. `Sockets.read`
/// asks `Tcp.readNow` and finds nothing, because the peer holds the body back
/// past the turn that follows the write, so it parks on
/// `Connected(connection)` and is asked again only in the turn whose
/// `Wait.poll` reported that key, which is the ask that finds the body. Three
/// bodies are therefore exactly six write asks and exactly six read asks —
/// two each — and the third body reaches the chain, which is how the run gets
/// to its end at all: a second piece that never arrived would leave the walk
/// without its last body.
///
/// `Clocked.tick` arms a fifty-millisecond deadline on the first ask of every
/// tick and answers the tick on the ask after it. Four ticks are two asks each
/// and the closing ask is the ninth, so nine is what the deadline allows over
/// this run. Without the gate the ticker would be asked once per turn, and
/// this run has far more turns than that.
#[test]
fn a_parked_request_is_asked_again_only_when_its_wake_has_fired() {
    let out = slice_with_peer(&[]);
    assert!(
        combined(&out).contains("ticker: asked 9 times"),
        "{}",
        format_output(&out)
    );
    assert!(
        combined(&out).contains("sockets: 3 payloads took 6 asks and 6 reads"),
        "{}",
        format_output(&out)
    );
}

/// A job that will never produce a result reaches `landed` as the error it is:
/// the answer state records the rejection, the handle leaves the table, and
/// the run reaches its end instead of stopping on the take.
#[test]
fn a_cancelled_job_lands_as_an_error_and_the_run_goes_on() {
    let out = aver("run_failed_job", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        combined(&out).contains("cancelled, then landed: jobs 0 scored 0 rejected 1"),
        "{}",
        format_output(&out)
    );
}

/// The other half of the wake gate, over a real job handle: a request parked
/// on `Item(Job(...))` is asked in a turn whose wait reported its key, and in
/// no other turn. No law can sample this one — a job handle is a resource a
/// law cannot write down — so the fixture reads the gate against a live
/// handle instead.
#[test]
fn a_request_parked_on_a_job_is_asked_only_when_the_wait_reports_its_key() {
    let out = aver("run_failed_job", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        combined(&out).contains("gated on the job"),
        "{}",
        format_output(&out)
    );
}

/// The same claim without a job engine: the generated seam is pure from the
/// take's answer onwards, so `aver verify` pins both outcomes of one key and
/// a law over `take`'s three answers pins that only a failure is recorded as
/// a rejection.
#[test]
fn the_failed_job_slice_verifies_and_checks_clean() {
    for command in ["check", "verify"] {
        let out = aver("run_failed_job", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let verified = combined(&aver("run_failed_job", &["verify"]));
    assert!(
        verified.contains("probeLanded"),
        "the seam's own verify block did not run"
    );
    assert!(
        verified.contains("__reportedValidation law aFailedTakeRecordsARejection"),
        "the law over take's answers did not run:\n{verified}"
    );
}

#[test]
fn the_slice_checks_clean() {
    let out = aver(SLICE, &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

#[test]
fn the_generated_invariants_and_the_programs_priority_law_hold() {
    let out = aver(SLICE, &["verify"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    for law in [
        "__park law laterKeepsTheInstance",
        "__parked law laterKeepsTheRequest",
        "__nextInstance law theNextInstanceIsHigher",
        "__remaining law theWaitNeverExceedsTheRequest",
        "__settledSlotPeer law nowRaisesTheInstance",
        "__current law theSlotWrittenIsTheSlotRead",
        "__settlePeer law lateAnswerIsDropped",
        "__settlePeer law lateAnswerIsRecorded",
        "__settlePeer law oneSlotPerProcess",
        "admit law readyPeerBeforeNewJob",
    ] {
        assert!(
            text.contains(law),
            "{law} missing from:\n{}",
            combined(&out)
        );
    }
    assert!(text.contains("0 failed"), "{}", format_output(&out));
}

/// The recording carries every socket answer the peer gave, so the replay
/// needs no peer at all: nothing binds, nothing connects, and the run that
/// comes back out is the run that went in.
#[test]
fn a_recorded_run_of_the_slice_replays_to_the_same_run() {
    let dir = scratch("replay");
    let recorded = slice_with_peer(&["--record", dir.to_str().expect("utf-8 scratch path")]);
    assert!(
        combined(&recorded).contains("sockets: 3 payloads took 6 asks and 6 reads"),
        "{}",
        format_output(&recorded)
    );

    let recording = only_recording(&dir);
    let mut command = Command::new(aver_bin());
    command.current_dir(fixture(SLICE));
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

/// A hostile world leaves every invariant of the loop standing, and it is
/// worth saying why it must: the loop's invariants are laws over pure
/// functions of the slot table, which no wait and no provider can reach. The
/// hostile profiles that do fire here are the ones a process performs in
/// place on its way to its first request — the sample run seats every
/// process, so every such profile is exercised under every law below it.
///
/// `Wait.poll`'s own hostile profiles are not among them, and cannot be
/// today: a law that reaches the wait has to be stated over a function that
/// performs `Wait.poll`, and such a law does not reach the Lean wall —
/// measured on `tests/fixtures/work_jobs`, whose `readyCount law` makes
/// `aver proof --backend lean` fail with five build errors, because an
/// oracle-lifted function's law renders its sample theorems without the
/// oracle arguments. Generating such a law into every program that asks for
/// a loop would break `aver proof` for all of them, so this leg does not.
#[test]
fn a_hostile_world_leaves_every_invariant_of_the_loop_standing() {
    let plain = aver(SLICE, &["verify"]);
    let hostile = aver(SLICE, &["verify", "--hostile"]);
    assert!(hostile.status.success(), "{}", format_output(&hostile));
    let text = combined(&hostile);
    assert!(text.contains("0 failed"), "{}", format_output(&hostile));
    assert!(
        text.contains("__settlePeer law lateAnswerIsDropped"),
        "{}",
        format_output(&hostile)
    );
    // The hostile run expands into strictly more cases than the honest one,
    // so the profiles are actually being installed rather than skipped.
    assert!(
        cases(&combined(&hostile)) > cases(&combined(&plain)),
        "hostile ran no more cases than the honest run:\n{}",
        format_output(&hostile)
    );
}

/// The `N/M cases passed` count from a verify summary line.
fn cases(text: &str) -> usize {
    text.lines()
        .find_map(|line| {
            let (_, rest) = line.split_once("| ")?;
            let (count, _) = rest.split_once(" cases passed")?;
            let (passed, _) = count.rsplit_once('/')?;
            passed.rsplit(' ').next()?.parse::<usize>().ok()
        })
        .unwrap_or(0)
}

#[test]
fn the_dump_shows_the_loop_that_was_generated() {
    let dir = fixture(SLICE);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    for line in [
        "record __Run",
        "fn __turn(run: __Run) -> Result<__Run, String>",
        "fn __runAll(run: __Run) -> Result<__Run, String>",
        "fn main() -> Result<Unit, String>",
        "fn __servePeer(run: __Run, id: Int, seq: Int, request: __PeerRequest) -> __Run",
        "Ledger.claim(run.ledger)",
        "Validation.begin(payload)?",
        "verify __settlePeer law lateAnswerIsDropped",
        // The view is built again per id on purpose, and the generated
        // description says so rather than leaving a reader to wonder.
        "It is built again for every id the turn asks about, deliberately",
        // The run ends by cancelling what is still running rather than
        // dropping its handles.
        "fn __cancelEach(run: __Run, keys: List<Int>) -> Result<Unit, String>",
        "Option.Some(job) -> __cancelled(run, key, (Work).cancel(job))",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
}

/// Decision 4's rule is per segment, and the loop's own functions obey it too:
/// what the loop generates for one process carries what that process performs,
/// not what the program performs. `peer` and `walk` print on their way to a
/// request, and serving the peer also performs the two socket operations the
/// answer module uses to answer it; `accepting` and `dialling` touch nothing
/// on their own path, so what the loop generates to seat them is pure even
/// though serving the accept binds and accepts on a real listener — one
/// generative effect in one process cannot oracle-lift the laws of another.
#[test]
fn the_loop_carries_each_processs_own_effects_and_not_the_programs() {
    let dir = fixture(SLICE);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    for function in ["__seatPeer", "__seatWalk", "__serveWalk", "__serveTicker"] {
        assert!(
            declared_effects(&text, function) == Some("Console.print".to_string()),
            "{function} does not carry its own effects:\n{}",
            format_output(&out)
        );
    }
    for function in [
        "__seatAccepting",
        "__seatDialling",
        "__seatTicker",
        "__serveDialling",
        "__serveDiallingDialled",
    ] {
        assert!(
            declared_effects(&text, function).is_none(),
            "{function} carries effects it does not perform:\n{}",
            format_output(&out)
        );
    }
    // Serving one request carries what the module answering it performs, and
    // nothing the other processes perform: the peer's own two socket
    // operations, and the accept's own four.
    assert_eq!(
        declared_effects(&text, "__servePeer"),
        Some("Console.print, Tcp.readNow, Tcp.writeNow".to_string())
    );
    assert_eq!(
        declared_effects(&text, "__serveAccepting"),
        Some("Args.get, Tcp.accept, Tcp.closeListener, Tcp.listen".to_string())
    );
    // The dispatch reaches every process, so it carries the union — and the
    // turn adds the wait, the stop observation and both ends of the job seam.
    assert_eq!(
        declared_effects(&text, "__serve"),
        Some(
            "Args.get, Console.print, Tcp.accept, Tcp.closeListener, Tcp.listen, Tcp.readNow, Tcp.writeNow"
                .to_string()
        )
    );
    assert_eq!(
        declared_effects(&text, "main"),
        Some(
            "Args.get, Console.print, Process.stopRequested, Tcp.accept, Tcp.closeListener, Tcp.listen, Tcp.readNow, Tcp.writeNow, Time.unixMs, Validation.begin, Validation.take, Wait.poll, Work.cancel"
                .to_string()
        )
    );
}

/// The `! [...]` line one generated function declares, if it declares one.
fn declared_effects(dump: &str, function: &str) -> Option<String> {
    let mut lines = dump.lines();
    lines.find(|line| {
        line.starts_with(&format!("fn {function}("))
            || line.starts_with(&format!("fn {function}()"))
    })?;
    for line in lines {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("! [") {
            return Some(rest.trim_end_matches(']').to_string());
        }
        if !trimmed.starts_with('?') {
            return None;
        }
    }
    None
}

/// A process the generated loop cannot see is refused rather than lowered,
/// seated by nobody and silently never run.
#[test]
fn a_process_written_outside_the_module_the_loop_is_generated_into_is_refused() {
    let sentence = "module 'Walker' writes process 'walking', and nothing seats it";
    for command in ["check", "run"] {
        let out = aver("run_process_elsewhere", &[command]);
        assert!(!out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            combined(&out).contains(sentence),
            "{command}: {}",
            format_output(&out)
        );
    }
    assert!(
        combined(&aver("run_process_elsewhere", &["check"])).contains("error[run-binding]:"),
        "the refusal is slugged"
    );
}

/// The turn asks the answer state for the next task once per slot of room, and
/// starting a job does not change that state, so a limit above one would start
/// the same task once per slot. That is refused with the reason rather than
/// run.
#[test]
fn a_job_limit_above_one_is_refused_because_the_turn_would_start_one_task_twice() {
    let dir = scratch("max-jobs");
    let slice = fixture(SLICE);
    for entry in std::fs::read_dir(&slice).expect("the slice") {
        let path = entry.expect("a slice file").path();
        if path.is_file() {
            std::fs::copy(&path, dir.join(path.file_name().expect("a file name")))
                .expect("copying the slice");
        }
    }
    let manifest = dir.join("aver.toml");
    let raised = std::fs::read_to_string(&manifest)
        .expect("the manifest")
        .replace("max-jobs = 1", "max-jobs = 4");
    std::fs::write(&manifest, raised).expect("raising the limit");

    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    assert!(
        text.contains("error[run-binding]:") && text.contains("This program's limit is 4"),
        "{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// `Wait.poll`'s contract allows false-positive readiness: a job may be
/// reported ready and still be running, and then `take` answers `Ok(None)`.
/// The seam must keep the handle for a later turn rather than drop it while
/// the computation continues. The fixture takes a job it has just started —
/// which is exactly what a wait is allowed to report — and then, once the
/// job has really settled, takes it again.
#[test]
fn a_job_reported_ready_before_it_finished_keeps_its_handle_and_lands_later() {
    let out = aver("run_false_ready", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        combined(&out).contains("kept the handle, then landed: jobs 0 scored 2"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn the_false_ready_slice_checks_clean() {
    let out = aver("run_false_ready", &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

/// The same claim at the lowering: `__taken<Kind>` hands the whole run and the
/// key on, and only an outcome the job will not repeat — a payload or an error
/// — removes it from the table.
#[test]
fn the_generated_take_removes_a_job_only_when_its_outcome_is_final() {
    let dir = fixture(SLICE);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    for line in [
        "fn __takenValidation(run: __Run, key: Int) -> __Run",
        "Option.Some(job) -> __reportedValidation(run, key, (Validation).take(job))",
        "fn __reportedValidation(run: __Run, key: Int, taken: Result<Option<Int>, String>) -> __Run",
        "Result.Err(reason) -> __landedValidation(run, key, (Result).Err(reason))",
        "fn __finishedValidation(run: __Run, key: Int, payload: Option<Int>) -> __Run",
        "Option.None -> run",
        "Option.Some(value) -> __landedValidation(run, key, (Result).Ok(value))",
        "ledger = (Ledger).validated((run).ledger, outcome)",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
    // The take no longer stops the turn on an error: a job that will not land
    // reaches `landed` as the error it is, and the run goes on.
    assert!(
        !text.contains("(Validation).take(job)?"),
        "the take still propagates a job's error out of the turn"
    );
}

/// The generated turn crosses one job seam: it takes and starts the jobs of
/// `jobs[0]` and nothing else, while the checker admits any number of job
/// kinds. A second kind would be declared, accepted and then never started
/// or taken, so it is refused with the reason, at every door, exactly as the
/// `max-jobs` refusal is.
#[test]
fn two_job_kinds_under_one_generated_loop_are_refused() {
    let sentence = "the generated turn crosses the seam of one job kind, and this program declares 2: Alpha, Beta. One job kind per generated loop is the limit in this build";
    for command in ["check", "run"] {
        let out = aver("run_two_job_kinds", &[command]);
        assert!(!out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            combined(&out).contains(sentence),
            "{command}: {}",
            format_output(&out)
        );
    }
    assert!(
        combined(&aver("run_two_job_kinds", &["check"])).contains("error[run-binding]:"),
        "the refusal is slugged"
    );
}

#[test]
fn a_view_that_is_not_the_shape_the_loop_fills_is_refused_with_the_declaration_it_wants() {
    let out = aver("run_view_shape", &["check"]);
    let text = combined(&out);
    assert!(
        text.contains("error[view-shape]: record 'View' declares no field 'room'"),
        "{}",
        format_output(&out)
    );
    assert!(
        text.contains("        room: Int"),
        "the message prints the declaration the loop fills:\n{}",
        format_output(&out)
    );
}

#[test]
fn a_process_the_loop_cannot_seat_is_refused_at_every_door() {
    // The lowering refuses before anything runs, so `run` and `verify` see
    // the same sentence `check` slugs.
    let sentence = "aver.toml declares [run], so the generated loop seats one of every process this module writes, and it has nothing to seat 'looping' with";
    for command in ["check", "run", "verify"] {
        let out = aver("run_shape_parameters", &[command]);
        assert!(!out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            combined(&out).contains(sentence),
            "{command}: {}",
            format_output(&out)
        );
    }
    let checked = aver("run_shape_parameters", &["check"]);
    assert!(
        combined(&checked).contains("error[run-binding]:"),
        "{}",
        format_output(&checked)
    );
}

/// The laws decision 7 names, on the Lean wall.
///
/// All twenty-seven of the example's laws close as universals: I2 (a late
/// answer changes nothing and is counted) and I4's visible half (a `Later`
/// moves neither the instance number nor any answer state) for every process
/// and every answer module, I3's per-call half in its two halves — the slot an
/// answer for the current instance writes back carries a strictly higher
/// instance number than the one it answered, and the slot written under an id
/// is the slot read from it — I1 for every process, the wait one deadline
/// contributes never being longer than the `ms` that deadline asked for, and
/// the program's own priority law. No law is bounded and none is a `sorry`.
///
/// I1's implication was the last one open: a size comparison across one
/// `Map.set` or one `Map.remove` inside a record update. It needed two facts
/// the prelude did not carry — a set under a key the map already holds does
/// not move the size, and a removal never grows a map — the first of which was
/// not even true of the old map model, whose `set` could insert a second entry
/// for a key already present in an unsorted list. The model's `set` is now
/// key-canonical and both facts ship; the `because` supplies the membership.
///
/// The composed statement of I3 — `__current` after `__settled` is strictly
/// higher — is still generated as two halves. That split was made for the same
/// missing one-key `Map.set` fact, so it may no longer be needed; re-measuring
/// the composed sentence belongs to the coordinator generator, not here.
#[test]
fn the_generated_invariants_reach_the_lean_wall() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping the Lean wall: `lake` is not available");
        return;
    }
    let dir = fixture(SLICE);
    let out_dir = scratch("lean");
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg("proof").arg("main.av");
    command.arg("--module-root").arg(&dir);
    command.arg("--backend").arg("lean");
    command.arg("-o").arg(&out_dir);
    command.arg("--check").arg("--check-json");
    command.arg("--sorry-budget").arg("0");
    let out = command.output().expect("aver proves");
    let json = String::from_utf8_lossy(&out.stdout);
    let line = json
        .lines()
        .rev()
        .find(|line| line.starts_with('{'))
        .unwrap_or_else(|| panic!("no JSON summary:\n{}", format_output(&out)));
    let summary: serde_json::Value = serde_json::from_str(line).expect("the summary is JSON");
    assert_eq!(
        summary["build_errors"].as_u64(),
        Some(0),
        "{}",
        format_output(&out)
    );
    assert_eq!(
        summary["universal_laws"].as_u64(),
        Some(27),
        "universal-law drift:\n{}",
        format_output(&out)
    );
    assert_eq!(
        summary["bounded_laws"].as_u64(),
        Some(0),
        "a law became bounded — say so in the report:\n{}",
        format_output(&out)
    );
    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "sorry drift:\n{}",
        format_output(&out)
    );
    let obligations = &summary["obligations"];
    for closed in [
        "__park.laterKeepsTheInstance.implication",
        "__askableSlot.aDeadlineGatesTheAsk.implication",
        "__settlePeer.lateAnswerIsDropped.implication",
        "__settlePeer.lateAnswerIsRecorded.implication",
        "admit.readyPeerBeforeNewJob.implication",
    ] {
        assert_eq!(
            obligations[closed].as_str(),
            Some("universal"),
            "{closed} is no longer universal:\n{}",
            format_output(&out)
        );
    }
    assert_eq!(
        obligations["__settlePeer.oneSlotPerProcess.implication"].as_str(),
        Some("universal"),
        "I1 reopened — the one-key `Map.set` size fact stopped reaching it:\n{}",
        format_output(&out)
    );
    // The laws with no `when` carry no implication obligation of their own, so
    // the way to pin that `laterKeepsTheRequest` closed is that nothing at all
    // is open: no build error, nothing bounded, and an empty sorry list.
    // The summary leaves `sorry_laws` out when nothing is open.
    let open: Vec<&str> = summary["sorry_laws"]
        .as_array()
        .map(|laws| laws.iter().filter_map(|law| law.as_str()).collect())
        .unwrap_or_default();
    assert!(
        open.is_empty(),
        "a law is open on the wall: {open:?}\n{}",
        format_output(&out)
    );
    let _ = std::fs::remove_dir_all(&out_dir);
}
