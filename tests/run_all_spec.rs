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
//! `tests/support/loopback_peer.rs`, which lives in the test and never in
//! the fixture, and the port is a free one that module found rather than a
//! fixed one.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;
#[path = "support/loopback_peer.rs"]
mod loopback_peer;

use aver_cmd::{aver_bin, format_output, repo_root};
use loopback_peer::{free_port, loopback_peer, silent_peer};

use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

const SLICE: &str = "run_all_slice";

/// Runs the slice against a loopback peer, on a port this test picked, and gives
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
    // A peer that could not play its whole part says why beside what the
    // slice printed: the slice's own account of the conversation is the only
    // way to tell a read that timed out from a pool that gave up.
    if let Err(reason) = peer.join() {
        panic!(
            "the loopback peer did not play its whole part: {:?}\n{}",
            reason
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| reason.downcast_ref::<&str>().map(|s| s.to_string())),
            format_output(&out)
        );
    }
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

/// The same run, with a wall-clock bound on it.
///
/// The two tests that pin "this slice ends on its own" exist to catch a run
/// that turns for ever, and a child process nobody bounds would hang the
/// suite rather than fail it. The bound is generous — those runs take about
/// five seconds, so a minute is a run that is not going to end rather than a
/// slow machine — and a run that reaches it is killed before the failure is
/// reported, so nothing is left behind holding a port.
fn aver_within(fixture_name: &str, args: &[&str], seconds: u64) -> Output {
    use std::io::Read;

    let dir = fixture(fixture_name);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.arg(args[0]).arg("main.av");
    command.arg("--module-root").arg(&dir);
    command.args(&args[1..]);
    command.stdout(Stdio::piped());
    command.stderr(Stdio::piped());
    let mut child = command.spawn().expect("aver starts");
    let mut child_stdout = child.stdout.take().expect("aver's stdout is piped");
    let mut child_stderr = child.stderr.take().expect("aver's stderr is piped");
    // The pipes are drained while the run is still going, because a run that
    // filled one would block on it and look like the hang this bound is for.
    let stdout_reader = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        let _ = child_stdout.read_to_end(&mut bytes);
        bytes
    });
    let stderr_reader = std::thread::spawn(move || {
        let mut bytes = Vec::new();
        let _ = child_stderr.read_to_end(&mut bytes);
        bytes
    });
    let deadline = Instant::now() + Duration::from_secs(seconds);
    let mut overran = false;
    let status = loop {
        match child.try_wait().expect("aver is waitable") {
            Some(status) => break status,
            None if Instant::now() >= deadline => {
                overran = true;
                let _ = child.kill();
                break child.wait().expect("aver is waitable");
            }
            None => std::thread::sleep(Duration::from_millis(20)),
        }
    };
    let out = Output {
        status,
        stdout: stdout_reader.join().expect("aver's stdout was read"),
        stderr: stderr_reader.join().expect("aver's stderr was read"),
    };
    assert!(
        !overran,
        "the run did not end within {seconds} seconds:\n{}",
        format_output(&out)
    );
    out
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

/// A peer that says nothing runs out the deadline its read was given.
///
/// `Sockets.read` records the clock reading a read falls due at on that read's
/// first ask and parks on `Either(Socket(Connected(conn)), left)`, so the wait
/// reporting the socket and the deadline running out both bring it back —
/// whichever comes first. Nothing ever arrives here, so the deadline is what
/// comes first: the ask after it answers `Now(Heard.TimedOut)`, the peer
/// process says so and hands the peer back through `Pool.gone`, and the run
/// reaches its end because the pool a peer has left hands out no more work.
#[test]
fn a_read_that_hears_nothing_runs_out_its_deadline_and_the_peer_is_handed_back() {
    let port = free_port();
    let peer = silent_peer(port);
    let port = port.to_string();
    let out = aver_within(SLICE, &["run", "--", &port], 60);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    assert!(
        text.contains("peer: 1 said nothing in time"),
        "the read did not time out:\n{}",
        format_output(&out)
    );
    // The peer asked the pool once, was given a height, and ended after
    // handing that peer back rather than asking for a second one.
    assert_eq!(
        text.matches("peer: asking the pool for work").count(),
        1,
        "{}",
        format_output(&out)
    );
    // The walk looked for a target once and never again: with a silent peer
    // no body is ever fetched, so a second line here would mean the chain
    // moved. The run reached its end rather than turning for ever around a
    // chain whose bodies nobody will fetch.
    assert_eq!(
        text.matches("walk: looking for the next block to connect")
            .count(),
        1,
        "{}",
        format_output(&out)
    );
    peer.join().expect("the silent peer played its whole part");
}

/// The slice gives up on its own, so a run nobody connects to still ends.
///
/// Every process here waits for something that will never come: the accepting
/// process for a client on its listener, the peer for a height the pool has
/// nobody to give it, the walk for a body nobody will fetch. The pool spends a
/// fixed number of idle asks and then stops, the chain ends where it stands
/// once the pool is done, and the program's `stop` policy ends the run once
/// the accepting process is the only one still seated — the listener itself
/// never gives up, which is what keeps a client that connects while the pool
/// still has work from being reset by a listener that closed under it — so the
/// run prints its summary and exits instead of turning for ever.
#[test]
fn a_run_of_the_slice_with_nobody_on_the_other_end_gives_up_and_ends() {
    let port = free_port().to_string();
    let out = aver_within(SLICE, &["run", "--", &port], 60);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    assert!(
        text.contains("sockets: 0 payloads took 0 asks and 0 reads"),
        "the peer never reached the pool's Stop:\n{}",
        format_output(&out)
    );
    assert!(
        text.contains("ticker: asked 9 times"),
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
        "__askableSlot law aBackwardsClockNeverStrandsARequest",
        "__eitherAskable law anEitherIsAskableOnceItsDeadlineHasPassed",
        "__remaining law theWaitNeverExceedsTheRequest",
        "__eitherWait law theEitherWaitNeverExceedsItsDeadline",
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
        // The job seam's third end: `begin` answers a Result the turn matches
        // on, and only the Ok half records the start through `started`.
        "Option.Some(task) -> __beganValidation(run, task, (Validation).begin(task))",
        "Result.Ok(job) -> __startJobsValidation(__jobSeatedValidation(run, task, job))",
        "ledger = __consumedValidation(run.ledger, task)",
        "fn __consumedValidation(state: Ledger.State, task: Tuple<Int, Bytes>) -> Ledger.State",
        "(Ledger).taskStarted(state, task)",
        "verify __settlePeer law lateAnswerIsDropped",
        // The view is built again per id on purpose, and the generated
        // description says so rather than leaving a reader to wonder.
        "It is built again for every id the turn asks about, deliberately",
        // The run ends by cancelling what is still running rather than
        // dropping its handles.
        "fn __cancelEach(run: __Run, keys: List<Int>) -> Result<Unit, String>",
        "Option.Some(job) -> __cancelled(run, key, (Work).cancel(__jobHandle(job)))",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
}

/// A process split into a `yield` helper: the loop seats the process, not the
/// helper. `peer` calls `fetchBody` non-tail, so `fetchBody`'s requests reach
/// the turn as requests of `peer` carrying the helper's state, and the slot
/// table has one marker per seated process and none for the helper.
#[test]
fn a_yield_helper_is_nested_in_its_caller_and_never_seated() {
    let dir = fixture(SLICE);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    for line in [
        // The helper has a protocol of its own, and the caller's states hold it.
        "fn __fetchBodyStart(key: Int, height: Int) -> __FetchBodyOutcome",
        "    InFetchBodyAt1(__FetchBodyWriteState, Int)",
        "fn __peerInFetchBodyAt1(__outcome: __FetchBodyOutcome, key: Int) -> __PeerOutcome",
        // The turn dispatches the helper's kinds as kinds of the caller.
        "fn __servePeerWrite(",
        // One marker per seated process, and the helper is not one.
        "type __Process\n    Accepting(__AcceptingRequest)\n    Dialling(__DiallingRequest)\n    Ticker(__TickerRequest)\n    Peer(__PeerRequest)\n    Walk(__WalkRequest)\n",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
    assert!(
        !text.contains("fn __seatFetchBody"),
        "the helper is entered through its caller, not seated:\n{text}"
    );
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
        Some("Console.print, Tcp.readNow, Tcp.writeNow, Time.unixMs".to_string())
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
            "Args.get, Console.print, Tcp.accept, Tcp.closeListener, Tcp.listen, Tcp.readNow, Tcp.writeNow, Time.unixMs"
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

/// A limit above one runs rather than being refused: `started` consumes the
/// task the moment its job begins, so the turn's next ask is offered a
/// different one and the slots of room start different tasks. The slice is
/// raised to `max-jobs = 4` and played against the same loopback peer; the
/// three bodies are fetched, validated and connected exactly as before.
#[test]
fn a_job_limit_above_one_runs_the_same_slice() {
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

    let port = free_port();
    let peer = loopback_peer(port);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command
        .arg("run")
        .arg("main.av")
        .arg("--module-root")
        .arg(&dir)
        .arg("--")
        .arg(port.to_string());
    let out = command.output().expect("aver runs");
    assert!(out.status.success(), "{}", format_output(&out));
    peer.join()
        .expect("the loopback peer played its whole part");
    let text = String::from_utf8_lossy(&out.stdout);
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
    assert!(
        text.contains("sockets: 3 payloads took 6 asks and 6 reads"),
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
        // The table holds one `__Job` sum over every kind, so a take key is
        // read against the whole table and dispatched on the variant.
        "type __Job\n    Validation(Work.Job)",
        "jobs: Map<Int, __Job>",
        "fn __jobHandle(job: __Job) -> Work.Job",
        "__Job.Validation(handle) -> handle",
        "fn __takenValidation(run: __Run, key: Int) -> __Run",
        "Option.Some(job) -> match job",
        "__Job.Validation(handle) -> __reportedValidation(run, key, (Validation).take(handle))",
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

/// Two job kinds under one generated loop: the turn takes and starts both
/// kinds over one table and one shared `max-jobs` limit. The fixture queues
/// two tasks of each kind, prints one line per landing — the kind and what it
/// scored — and parks the process until all four have landed; each of the
/// four lines appears exactly once, which is the proof that each task was
/// started once and landed once, and the sum it reports is what 2+3+20+40
/// makes.
#[test]
fn two_job_kinds_under_one_generated_loop_each_land_once() {
    let out = aver("run_two_job_kinds", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    for landing in [
        "alpha landed: scored 2",
        "alpha landed: scored 3",
        "beta landed: scored 20",
        "beta landed: scored 40",
    ] {
        assert_eq!(
            text.matches(landing).count(),
            1,
            "{landing} did not land exactly once:\n{}",
            format_output(&out)
        );
    }
    assert!(
        text.contains("all jobs landed: scored 65"),
        "{}",
        format_output(&out)
    );
    for command in ["check", "verify"] {
        let out = aver("run_two_job_kinds", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let verified = combined(&aver("run_two_job_kinds", &["verify"]));
    for law in [
        "__consumedAlpha law aStartedTaskIsNotAskedAgain",
        "__consumedBeta law aStartedTaskIsNotAskedAgain",
        "__startableAlpha law aFullTableAsksNothing",
        "__startableBeta law aFullTableAsksNothing",
    ] {
        assert!(verified.contains(law), "{law} missing from:\n{verified}");
    }
    assert!(verified.contains("0 failed"), "{verified}");
}

/// One table and one limit for every kind: `__Job` is the generated sum that
/// lets `jobs: Map<Int, __Job>` carry both kinds, `__jobHandle` unwraps it for
/// the wait and the cancel, and a take dispatches on the variant before it
/// reaches this kind's `take`. The room the ask checks is `max-jobs` minus
/// the whole table, not minus this kind's own count.
#[test]
fn the_two_kinds_share_one_table_and_one_limit() {
    let dir = fixture("run_two_job_kinds");
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    let text = combined(&out);
    for line in [
        "type __Job\n    Alpha(Work.Job)\n    Beta(Work.Job)",
        "jobs: Map<Int, __Job>",
        "fn __jobHandle(job: __Job) -> Work.Job",
        "__Job.Alpha(handle) -> handle",
        "__Job.Beta(handle) -> handle",
        "fn __roomLeft(run: __Run) -> Int",
        "__maxJobs() - Map.len(run.jobs)",
        "fn __takeEachAlpha(run: __Run, ready: List<Int>) -> __Run",
        "fn __takeEachBeta(run: __Run, ready: List<Int>) -> __Run",
        "__Job.Alpha(handle) -> __reportedAlpha(run, key, (Alpha).take(handle))",
        "__Job.Beta(_) -> run",
        "__Job.Beta(handle) -> __reportedBeta(run, key, (Beta).take(handle))",
        "__Job.Alpha(_) -> run",
        "fn __startJobsAlpha(run: __Run) -> __Run",
        "fn __startJobsBeta(run: __Run) -> __Run",
        "Option.Some(task) -> __beganAlpha(run, task, (Alpha).begin(task))",
        "Option.Some(task) -> __beganBeta(run, task, (Beta).begin(task))",
        "pooled = __consumedAlpha(run.pooled, task)",
        "pooled = __consumedBeta(run.pooled, task)",
        "(Pooled).alphaStarted(state, task)",
        "(Pooled).betaStarted(state, task)",
        // The turn takes every kind first and then starts every kind, so a
        // job that landed this turn frees room the same turn can use.
        "taken0 = __takeEachAlpha(served, ready)",
        "taken1 = __takeEachBeta(taken0, ready)",
        "started0 = __startJobsAlpha(taken1)",
        "Result.Ok(__startJobsBeta(started0))",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
}

/// A recorded run of the two-kind fixture replays to the same run: every job
/// outcome the recording carried comes back out of the replay, kind by kind.
#[test]
fn a_recorded_run_of_two_job_kinds_replays_to_the_same_run() {
    let dir = scratch("two-kinds-replay");
    let mut recorded = Command::new(aver_bin());
    recorded
        .current_dir(fixture("run_two_job_kinds"))
        .arg("run")
        .arg("main.av")
        .arg("--module-root")
        .arg(fixture("run_two_job_kinds"))
        .arg("--record")
        .arg(&dir);
    let out = recorded.output().expect("aver runs");
    assert!(out.status.success(), "{}", format_output(&out));
    for line in ["beta landed: scored 40", "all jobs landed: scored 65"] {
        assert!(combined(&out).contains(line), "{}", format_output(&out));
    }

    let recording = only_recording(&dir);
    let mut command = Command::new(aver_bin());
    command.current_dir(fixture("run_two_job_kinds"));
    command.arg("replay").arg(&recording);
    command.arg("--check-args");
    let replayed = command.output().expect("aver replays");
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        combined(&replayed).contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
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

/// The generated `__consumed<K> law aStartedTaskIsNotAskedAgain` cites the
/// program's own law of that name on its `started` function. A program that
/// states none is refused at the door, with the block to write, before the
/// generated module is checked — never as a `using` that fails to resolve at
/// a line of generated code.
#[test]
fn a_started_function_without_its_law_is_refused_with_the_block_to_write() {
    let sentence = "aver.toml declares [run], so the generated turn records a start of job 'Validation' through 'Pooled.taskStarted' and cites the law that function states about it; 'Pooled.taskStarted' states no law named 'aStartedTaskIsNotAskedAgain'";
    for command in ["check", "run", "verify"] {
        let out = aver("run_started_law_missing", &[command]);
        assert!(!out.status.success(), "{command}: {}", format_output(&out));
        let text = combined(&out);
        assert!(
            text.contains(sentence),
            "{command}: {}",
            format_output(&out)
        );
        for line in [
            "verify taskStarted law aStartedTaskIsNotAskedAgain",
            "    given state: State = [fresh()]",
            "    given task: Int = [...]",
            "    when nextTask(state) == Option.Some(task)",
            "    nextTask(taskStarted(state, task)) != Option.Some(task) holds",
        ] {
            assert!(
                text.contains(line),
                "{command}: the refusal does not print the block:\n{}",
                format_output(&out)
            );
        }
        assert!(
            !text.contains("uses unknown or unexposed law"),
            "{command}: the door let the generated `using` fail instead:\n{}",
            format_output(&out)
        );
    }
    let checked = aver("run_started_law_missing", &["check"]);
    assert!(
        combined(&checked).contains("error[run-binding]:"),
        "{}",
        format_output(&checked)
    );
}

/// The laws decision 7 names, on the Lean wall.
///
/// All thirty-four of the example's laws close as universals: I2 (a late
/// answer changes nothing and is counted) and I4's visible half (a `Later`
/// moves neither the instance number nor any answer state) for every process
/// and every answer module, I3's per-call half in its two halves — the slot an
/// answer for the current instance writes back carries a strictly higher
/// instance number than the one it answered, and the slot written under an id
/// is the slot read from it — I1 for every process, both halves of the
/// deadline gate — a reading that has not reached the deadline does not ask,
/// and a reading that has fallen back further than the deadline asked for
/// does — the wait one deadline contributes never being longer than the `ms`
/// that deadline asked for, both halves of the same gate for a request parked
/// on an item and a deadline at once, the program's own priority law, and the
/// four the job seam's third end brought: the program's
/// `taskStarted law aStartedTaskIsNotAskedAgain` and the
/// `withoutTask law theStartedTaskIsOut` it cites, the generated
/// `__consumedValidation law aStartedTaskIsNotAskedAgain` that cites the
/// first, and `__startableValidation law aFullTableAsksNothing`. No law is
/// bounded and none is a `sorry`.
///
/// The two `Either` laws are stated over the gate's arithmetic rather than
/// over a sampled slot, because a slot parked on `Either` cannot be written
/// down: the constructor carries a `Wait.Item`, and a `Wait.Item` carries a
/// socket or a job handle a program cannot construct. That is the same reason
/// the socket half of the gate has no law at all.
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
        Some(34),
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
        "__askableSlot.aBackwardsClockNeverStrandsARequest.implication",
        "__eitherAskable.anEitherIsAskableOnceItsDeadlineHasPassed.implication",
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
