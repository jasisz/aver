//! The generated loop (jasisz/aver#1329, process layer v2).
//!
//! `tests/fixtures/run_all_slice/` is the whole claim in one program: five
//! processes, three answer modules, one job kind and two policies, with
//! every line between them generated. This suite runs it the way a user
//! would — `aver run`, `aver verify`, a recording and its replay, a hostile
//! wait, and the dump that shows what was generated — and holds the
//! refusals that say what the program has to write for the loop to be
//! generated at all. `run_families` seats one process per key, and
//! `run_all_from_main` runs the loop from a `main` of the program's own.
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
#[cfg(unix)]
#[path = "support/sigint.rs"]
mod sigint;
#[cfg(unix)]
use sigint::stopped_by_sigint;

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
/// first ask and parks on `Until([Socket(Connected(conn))], Some(left))`, so
/// the wait reporting the socket and the deadline running out both bring it
/// back — whichever comes first. Nothing ever arrives here, so the deadline is
/// what comes first: the ask after it answers `Ok(Heard.TimedOut)`, the peer
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

/// A job that will never produce a result: the answer module began it and
/// cancelled it at once, the take after the wait reported it answers why, and
/// the module answers the request with that reason rather than failing the
/// turn. The run reaches its end.
#[test]
fn a_cancelled_job_is_answered_as_an_error_and_the_run_goes_on() {
    let out = aver("run_failed_job", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        combined(&out).contains("cancelled, then answered: work: job cancelled"),
        "{}",
        format_output(&out)
    );
}

/// The other half of the wake gate, over a real job handle: a request parked
/// on `Until([Job(...)], None)` is asked in a turn whose wait reported its
/// key, and in no other turn. No law can sample this one — a job handle is a resource a
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

/// The answer module's own record of a take that failed is a pure function,
/// so `aver verify` states it as a law.
#[test]
fn the_failed_job_slice_verifies_and_checks_clean() {
    for command in ["check", "verify"] {
        let out = aver("run_failed_job", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let verified = combined(&aver("run_failed_job", &["verify"]));
    assert!(
        verified.contains("rejectedFor law aFailedTakeIsRecorded"),
        "the law over a failed take did not run:\n{verified}"
    );
}

#[test]
fn the_slice_checks_clean() {
    let out = aver(SLICE, &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

/// The loop generates no laws of its own into a program: its invariants are
/// stated once, over the generated functions, in
/// `tests/fixtures/run_schedule_cases`. What `aver verify` runs here is the
/// program's own policy law.
#[test]
fn the_programs_priority_law_holds_and_the_loop_adds_no_law() {
    let out = aver(SLICE, &["verify"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    assert!(
        text.contains("admit law aPeerIsAlwaysAdmitted"),
        "{}",
        format_output(&out)
    );
    assert!(
        !text.contains("verify __") && !text.contains("✓ __"),
        "a generated law ran in a program that states none:\n{}",
        format_output(&out)
    );
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

/// A hostile world leaves the program's own laws standing: the hostile
/// profiles that fire are the ones the answer modules and the processes
/// perform, and the laws are over pure functions no provider reaches.
#[test]
fn a_hostile_world_leaves_the_programs_laws_standing() {
    let plain = aver(SLICE, &["verify"]);
    let hostile = aver(SLICE, &["verify", "--hostile"]);
    assert!(hostile.status.success(), "{}", format_output(&hostile));
    let text = combined(&hostile);
    assert!(text.contains("0 failed"), "{}", format_output(&hostile));
    assert!(
        text.contains("admit law aPeerIsAlwaysAdmitted"),
        "{}",
        format_output(&hostile)
    );
    assert!(
        cases(&combined(&hostile)) >= cases(&combined(&plain)),
        "hostile ran fewer cases than the honest run:\n{}",
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
    let text = dump(SLICE);
    for line in [
        "record __Run",
        "fn __turn(run: __Run) -> Result<__Run, String>",
        "fn __runAll(run: __Run) -> Result<__Run, String>",
        "fn __all() -> Result<Unit, String>",
        "fn main() -> Result<Unit, String>",
        "fn __servePeer(run: __Run, id: Int, seq: Int, request: __PeerRequest) -> __Run",
        // The answer module's state is handed out of the run for the one
        // answer, so the answer function holds the only reference to it.
        "fn __takeLedger(run: __Run) -> Tuple<Option<Ledger.State>, __Run>",
        "Ledger.claim(__taken)",
        // An Ok settles the request, an Err parks it on the wake it named.
        "Result.Err(__wake) -> __park(",
        // The run's own Maps are handed to Map.set at the run's last use,
        // with everything else read first, so no answer copies them.
        "    version = __versionOf(run, owner)\n    __Run.update(run, versions = Map.set(run.versions, owner, version + 1))",
        "__Run.update(moved, slots = Map.set(moved.slots, id, __parked(slot, wake, now, owner, version)))",
        "(run.ledger, __Run.update(run, ledger = Option.None))",
        "Run.Wake.Settled(deadline) -> Bool.or(__versionOf(run, slot.owner) > slot.version",
        // The entry's own policies are called by name.
        "match admit(__view(run, ready), id)",
        "Bool.or(stop(__view(run, [])), Map.len(run.slots) == 0)",
        // The run ends by cancelling every job a parked request waits on.
        "fn __cancelWaited(run: __Run, ids: List<Int>) -> __Run",
        "Wait.Item.Job(job) -> __cancelledWaited(run, Work.cancel(job))",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
    for gone in [
        "__maxJobs",
        "__roomLeft",
        "room",
        "__consumed",
        "__startJobs",
        "type __Job",
        "__ThenAnswer",
        "__HistoryEvent",
        "\nverify __",
        "SourceTrace",
    ] {
        assert!(
            !text.contains(gone),
            "{gone} is still in the generated loop:\n{text}"
        );
    }
}

/// The source `AVER_YIELD_DUMP=1` prints for one fixture: what the lowering
/// and the loop generator wrote.
fn dump(fixture_name: &str) -> String {
    let dir = fixture(fixture_name);
    let mut command = Command::new(aver_bin());
    command.current_dir(&dir);
    command.env("AVER_YIELD_DUMP", "1");
    command.arg("check").arg("main.av");
    command.arg("--module-root").arg(&dir);
    let out = command.output().expect("aver runs");
    combined(&out)
}

/// A process split into a `yield` helper: the loop seats the process, not the
/// helper. `peer` calls `fetchBody` non-tail, so `fetchBody`'s requests reach
/// the turn as requests of `peer` carrying the helper's state, and the slot
/// table has one marker per seated process and none for the helper.
#[test]
fn a_yield_helper_is_nested_in_its_caller_and_never_seated() {
    let text = dump(SLICE);
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
    let text = dump(SLICE);
    for function in ["__seatPeer", "__seatWalk", "__serveTicker"] {
        assert!(
            declared_effects(&text, function) == Some("Console.print".to_string()),
            "{function} does not carry its own effects:\n{text}"
        );
    }
    // The walk's target waits on a validation job its answer module takes.
    assert_eq!(
        declared_effects(&text, "__serveWalk"),
        Some("Console.print, Validation.take".to_string())
    );
    for function in [
        "__seatAccepting",
        "__seatDialling",
        "__seatTicker",
        "__serveDialling",
        "__serveDiallingDialled",
    ] {
        assert!(
            declared_effects(&text, function).is_none(),
            "{function} carries effects it does not perform:\n{text}"
        );
    }
    // Serving one request carries what the module answering it performs, and
    // nothing the other processes perform: the peer's socket operations and
    // the validation its delivery begins, and the accept's own four.
    assert_eq!(
        declared_effects(&text, "__servePeer"),
        Some("Console.print, Tcp.readNow, Tcp.writeNow, Time.unixMs, Validation.begin".to_string())
    );
    assert_eq!(
        declared_effects(&text, "__serveAccepting"),
        Some("Args.get, Tcp.accept, Tcp.closeListener, Tcp.listen".to_string())
    );
    // The dispatch reaches every process, so it carries the union — and the
    // loop's entry adds the wait, the stop observation and the cancel.
    assert_eq!(
        declared_effects(&text, "__serve"),
        Some(
            "Args.get, Console.print, Tcp.accept, Tcp.closeListener, Tcp.listen, Tcp.readNow, Tcp.writeNow, Time.unixMs, Validation.begin, Validation.take"
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

/// Dependency yielding functions are library protocols. Only the entry
/// process is seated, and it enters Walker's protocol through a tail call.
/// Walker depends on the capability it asks, not on the module that answers
/// it: the program's answer modules are the ones its whole cone reaches.
#[test]
fn a_process_enters_an_imported_helper_under_the_generated_loop() {
    for command in ["check", "run"] {
        let out = aver("run_process_elsewhere", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
}

/// The loop belongs to the entry, so a dependency whose yielding function
/// takes a key is a library helper and is never seated, at any door.
#[test]
fn a_dependency_helper_is_never_seated() {
    for command in ["check", "run", "verify"] {
        let out = aver("run_default_process_elsewhere", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            !combined(&out).contains("nothing to seat"),
            "{command}: {}",
            format_output(&out)
        );
    }
    let out = aver("run_default_process_elsewhere", &["run"]);
    assert!(
        combined(&out).contains("peer 1 closed"),
        "{}",
        format_output(&out)
    );
}

/// Generated source is parsed again in the entry's scope. A type the process
/// reads from its capability (`Wire.Heard`) shares its bare name with a
/// record of another dependency (`Other.Heard`), so the protocol and trace
/// types must spell it qualified; spelled bare, `Heard` was ambiguous in
/// every generated type that carried it.
#[test]
fn a_generated_type_names_an_answer_type_whose_bare_name_is_taken() {
    for command in ["check", "run", "verify"] {
        let out = aver("run_colliding_answer_type", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            !combined(&out).contains("Ambiguous type name"),
            "{command}: {}",
            format_output(&out)
        );
    }
    let out = aver("run_colliding_answer_type", &["run"]);
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("gone done"),
        "{}",
        format_output(&out)
    );
}

/// `Random.int` with literal bounds that fit is an `Int` at the call, not a
/// `Result`: the checker narrows it. The generated trace types read what the
/// checker stamped on the call rather than the operation's declared result,
/// so the process's own `Int` flows into them unchanged.
#[test]
fn a_trace_reads_the_narrowed_type_of_an_in_place_effect() {
    for command in ["check", "run", "verify"] {
        let out = aver("run_literal_random_answer", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let out = aver("run_literal_random_answer", &["run"]);
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("gone done "),
        "{}",
        format_output(&out)
    );
}

/// One request parks on a listener that another request of the same turn
/// then closes. The next turn's wait holds a socket the runtime no longer
/// knows. That used to fail the whole wait, and with it `__runAll`; it is now
/// reported ready, like a job the engine has forgotten, so the parked request
/// is asked again, learns the socket is gone from the operation it runs, and
/// the run ends normally.
#[test]
fn a_socket_closed_while_a_request_is_parked_on_it_does_not_end_the_run() {
    for command in ["check", "run"] {
        let out = aver("run_closed_socket_wait", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let out = aver("run_closed_socket_wait", &["run"]);
    let text = String::from_utf8_lossy(&out.stdout);
    for line in ["closed the watched socket", "the watched socket is gone"] {
        assert!(text.contains(line), "{line}: {}", format_output(&out));
    }
}

/// A limit above one runs the same slice: the answer module begins one
/// validation per delivered body, and the engine runs up to four at once.
/// The slice is raised to `max-jobs = 4` and played against the same
/// loopback peer; the three bodies are fetched, validated and connected
/// exactly as before.
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
/// The answer module must keep the handle and wait on the job again rather
/// than drop it while the computation continues. The fixture takes its job
/// the moment it begins, which is exactly what a wait is allowed to report,
/// and answers once the job has really landed.
#[test]
fn a_job_reported_ready_before_it_finished_keeps_its_handle_and_lands_later() {
    let out = aver("run_false_ready", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = combined(&out);
    for line in [
        "kept the handle, then landed: scored 2",
        "a take found the job still running",
    ] {
        assert!(text.contains(line), "{line}: {}", format_output(&out));
    }
}

#[test]
fn the_false_ready_slice_checks_clean() {
    let out = aver("run_false_ready", &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
}

/// Two job kinds and one keyed family under one generated loop: one scorer is
/// seated per task the answer module lists, each asks for its score, and the
/// module begins one job of the task's kind for each. Each of the four
/// landings appears exactly once, and the summary process, parked on Settled
/// until every task has landed, reports what 2+3+20+40 makes.
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
}

/// The loop keeps no job table: the jobs live in the answer module's state,
/// and the loop only seats the scorers, one per listed task.
#[test]
fn the_loop_seats_the_scorers_and_holds_no_job() {
    let text = dump("run_two_job_kinds");
    for line in [
        "fn __seatFamilyScorer(run: __Run, keys: List<Int>) -> __Run",
        "seatedScorer: Map<Int, Int>",
        "retiredScorer: Map<Int, Bool>",
        "__seatFamilyScorer(run, __keysOfScorer(run))",
        "Option.Some(state) -> Pooled.tasks(state)",
    ] {
        assert!(text.contains(line), "{line} missing from the dump");
    }
    assert!(!text.contains("type __Job"), "the loop holds a job table");
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
fn a_process_the_loop_cannot_seat_is_refused_at_every_door() {
    // The lowering refuses before anything runs, so `run` and `verify` see
    // the same sentence `check` slugs.
    let sentence = "the generated loop seats one 'looping' and has nothing to hand it; a process that takes a key is declared with the function that lists the keys, for example `process looping seated by Sockets.peers`";
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

/// The program's own laws, the policy law among them, close universally on
/// the Lean wall. The loop adds none of its own; its invariants are checked on
/// `tests/fixtures/run_schedule_cases`.
/// The manifest must retain every law without a bounded or admitted fallback.
#[test]
fn the_programs_laws_reach_the_lean_wall() {
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
        Some(1),
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
    assert_eq!(
        obligations["admit.aPeerIsAlwaysAdmitted.implication"].as_str(),
        Some("universal"),
        "the policy law is no longer universal:\n{}",
        format_output(&out)
    );
    // Nothing at all is open: no build error, nothing bounded, and an empty
    // sorry list.
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

/// The job as a request: an answer module begins a job in place and parks
/// the request on `Until([Wait.Item.Job(job)], None)`.
/// The quick square lands and wakes the process; the slow one never does, so
/// only a stop request ends the run. The stop is observed within a moment,
/// although the wait it arrives in has no deadline, and the generated `__over`
/// cancels the job the parked request waits on rather than abandoning it.
#[cfg(unix)]
#[test]
fn a_stop_request_ends_a_run_parked_on_a_job_its_answer_module_began() {
    for command in ["check", "verify"] {
        let out = aver("run_job_request", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let dir = scratch("job-request-stop");
    let record = dir.to_str().expect("utf-8 scratch path");
    let (out, stopped_after) = stopped_by_sigint(
        &fixture("run_job_request"),
        "asking for a square that takes a long time",
        &["--record", record],
    );
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("7 squared is 49"),
        "{}",
        format_output(&out)
    );
    assert!(
        stopped_after < Duration::from_secs(5),
        "a stop request took {stopped_after:?} to end a run parked on a job"
    );
    let recording = std::fs::read_to_string(only_recording(&dir)).expect("the recording");
    assert!(
        recording.contains("\"Work.cancel\""),
        "the job the parked request waited on was not cancelled when the run ended:\n{recording}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// A keyed family: one member is seated per key the hub lists, in list
/// order, and each is woken by Settled when the feeder's post moves the hub.
/// The post that stops listing key 2 drops member 2 at the turn boundary,
/// before it reads again: it never reads 20 and is never told it is gone.
#[test]
fn a_family_is_seated_per_key_and_dropped_when_its_key_leaves() {
    for command in ["check", "verify"] {
        let out = aver("run_families", &[command]);
        assert!(out.status.success(), "{command}: {}", format_output(&out));
    }
    let out = aver("run_families", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "member 1 read 10\nmember 2 read 10\nmember 1 read 20\nmember 1 read 30\nmember 1 is gone",
        "{}",
        format_output(&out)
    );
}

/// The loop is also reachable from a `main` of the program's own: `node`
/// runs it, anything else runs nothing.
#[test]
fn a_main_runs_the_loop_through_run_all() {
    let out = aver("run_all_from_main", &["run", "--", "node"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "ticked 3 times",
        "{}",
        format_output(&out)
    );
    let out = aver("run_all_from_main", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert_eq!(
        String::from_utf8_lossy(&out.stdout).trim(),
        "usage: main node",
        "{}",
        format_output(&out)
    );
    let out = aver("run_all_from_main", &["check"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        !combined(&out).contains("warning["),
        "{}",
        format_output(&out)
    );
}

/// What a seating line has to name, and what a policy has to be, are said at
/// the door with the signature wanted.
#[test]
fn a_wrong_seating_or_policy_is_refused_with_the_shape_it_needs() {
    let dir = scratch("seating");
    let families = fixture("run_families");
    for name in ["main.av", "hub.av", "board.av"] {
        std::fs::copy(families.join(name), dir.join(name)).expect("copy fixture");
    }
    let check = |dir: &Path| {
        let mut command = Command::new(aver_bin());
        command.current_dir(dir);
        command
            .arg("check")
            .arg("main.av")
            .arg("--module-root")
            .arg(dir);
        command.output().expect("aver runs")
    };
    let main = dir.join("main.av");
    let source = std::fs::read_to_string(&main).expect("main");
    for (from, to, expected) in [
        (
            "process member seated by Hub.members",
            "process member seated by Hub.next",
            "`process member seated by Hub.next` reads the keys to seat 'member' with from the state of 'Hub', so it must be 'next(Hub.State) -> List<Int>'",
        ),
        (
            "process member seated by Hub.members",
            "process member seated by Board.next",
            "names a function of module 'Board', which answers no capability of this program",
        ),
        (
            "fn stop(view: Run.View) -> Bool",
            "fn stop(view: Run.View) -> Int",
            "the generated loop calls 'stop' of the entry module as its policy, so it must be 'stop(view: Run.View) -> Bool'",
        ),
        (
            "process member seated by Hub.members",
            "process echo seated by Hub.members",
            "`process echo seated by Hub.members` names no yielding function of this module",
        ),
    ] {
        std::fs::write(&main, source.replace(from, to)).expect("edit main");
        let out = check(&dir);
        assert!(!out.status.success(), "{to}: {}", format_output(&out));
        let text = combined(&out);
        assert!(text.contains(expected), "{to}: {}", format_output(&out));
        assert!(
            text.contains("error[run-binding]"),
            "{to}: {}",
            format_output(&out)
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}

/// A directory check walks `lib/ticks.av` as the entry of its own program,
/// where it is named by its `module Ticks` line rather than the `Lib.Ticks`
/// every importer uses. The answer modules a batch shares must carry the
/// importer's name, or the entry's loop imports a `Ticks` nobody can load and
/// its process loses `Lib` altogether.
#[test]
fn a_directory_check_names_an_answer_module_under_a_subdirectory_the_way_its_importer_does() {
    let dir = fixture("run_process_subdir_capability");
    for target in ["main.av", "."] {
        let out = Command::new(aver_bin())
            .current_dir(&dir)
            .args(["check", target, "--module-root", "."])
            .output()
            .expect("aver runs");
        assert!(out.status.success(), "{target}: {}", format_output(&out));
        assert!(
            !combined(&out).contains("unknown-ident"),
            "{target}: {}",
            format_output(&out)
        );
    }
    let out = aver("run_process_subdir_capability", &["run"]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim(), "ticked");
}

/// A program whose generated loop keys its wait by `Int` can key its own
/// waits by a sum, in the entry and in a dependency. Each such wait is
/// carried through an `Int`-keyed wait by helpers generated for its key type,
/// and answers the same keys it would have answered, in the same order. The
/// second fixture also matches the waits' answers with nested patterns, in
/// the entry and in the dependency function that waits: the dependency's
/// patterns are compiled first and its waits carried after.
#[test]
fn waits_keyed_by_a_sum_run_beside_the_generated_loop() {
    for name in ["run_wait_own_key", "run_wait_own_key_nested"] {
        let looped = aver_within(name, &["run"], 60);
        assert!(looped.status.success(), "{}", format_output(&looped));
        assert_eq!(
            String::from_utf8_lossy(&looped.stdout).trim(),
            "ticked 3 times"
        );

        let manual = aver_within(name, &["run", "--", "manual"], 60);
        assert!(manual.status.success(), "{}", format_output(&manual));
        let text = String::from_utf8_lossy(&manual.stdout);
        let mut lines: Vec<&str> = text.lines().collect();
        lines.sort_unstable();
        assert_eq!(
            lines,
            [
                "an empty wait reported 0 keys",
                "read 1 scored 5",
                "write 2 scored 8",
            ],
            "{name}: {}",
            format_output(&manual)
        );

        let dir = fixture(name);
        let dump = Command::new(aver_bin())
            .current_dir(&dir)
            .env("AVER_YIELD_DUMP", "1")
            .arg("check")
            .arg("main.av")
            .arg("--module-root")
            .arg(&dir)
            .output()
            .expect("aver runs");
        assert!(dump.status.success(), "{}", format_output(&dump));
        let dumped = combined(&dump);
        for helper in [
            "fn __waitPollByCollectingWatch(items: Map<Collecting.Watch, Wait.Item>, timeoutMs: Int) -> Result<List<Collecting.Watch>, String>",
            "fn __waitKeysAtCollectingWatch(",
            "fn __waitPollByWatch(items: Map<Watch, Wait.Item>, timeoutMs: Int) -> Result<List<Watch>, String>",
        ] {
            assert!(
                dumped.contains(helper),
                "{name}: missing `{helper}`:\n{dumped}"
            );
        }
    }
}

/// A recording of the hand-written waits replays: the recording holds the
/// `Int`-keyed wait every backend performs, and the replay performs it again.
#[test]
fn a_recording_of_carried_waits_replays() {
    let dir = scratch("own-key-replay");
    let mut recorded = Command::new(aver_bin());
    recorded
        .current_dir(fixture("run_wait_own_key"))
        .arg("run")
        .arg("main.av")
        .arg("--module-root")
        .arg(fixture("run_wait_own_key"))
        .arg("--record")
        .arg(&dir)
        .args(["--", "manual"]);
    let out = recorded.output().expect("aver runs");
    assert!(out.status.success(), "{}", format_output(&out));
    let recording = only_recording(&dir);
    let mut command = Command::new(aver_bin());
    command.current_dir(fixture("run_wait_own_key"));
    command.arg("replay").arg(&recording).arg("--check-args");
    let replayed = command.output().expect("aver replays");
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        combined(&replayed).contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// Two processes each call `Run.fail` on their third tick, in the same turn.
/// The turn is finished, the run answers the reason of the process the turn
/// served first, and the loop's own `main` exits non-zero with it on stderr.
#[test]
fn a_turn_that_calls_run_fail_ends_the_run_with_the_first_reason() {
    let out = aver_within("run_fail", &["run"], 60);
    assert!(!out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout);
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines,
        [
            "first saw tick 1 in turn 1",
            "second saw tick 2 in turn 1",
            "first saw tick 3 in turn 2",
            "second saw tick 4 in turn 2",
            "first saw tick 5 in turn 3",
            "second saw tick 6 in turn 3",
        ],
        "{}",
        format_output(&out)
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("first gave up in turn 3"),
        "{}",
        format_output(&out)
    );
    assert!(
        !stderr.contains("second gave up"),
        "a later Run.fail in the same turn changes nothing:\n{}",
        format_output(&out)
    );
}

/// An answer module fails the run in the turn a process ends and the entry's
/// `stop` would end it too: the failure is what `Run.all()` answers. When
/// `stop` ends the run a turn earlier, before anything failed, it answers Ok.
#[test]
fn run_fail_from_an_answer_module_wins_over_a_stop_in_the_same_turn() {
    let late = aver_within("run_fail_answer", &["run", "--", "late"], 60);
    assert!(late.status.success(), "{}", format_output(&late));
    let text = String::from_utf8_lossy(&late.stdout);
    assert!(
        text.trim_end()
            .ends_with("the run failed: the clock broke at tick 3"),
        "{}",
        format_output(&late)
    );
    assert!(!text.contains("in turn 3"), "{}", format_output(&late));

    let quit = aver_within("run_fail_answer", &["run", "--", "quit"], 60);
    assert!(quit.status.success(), "{}", format_output(&quit));
    let text = String::from_utf8_lossy(&quit.stdout);
    assert!(
        text.trim_end().ends_with("the run ended cleanly"),
        "{}",
        format_output(&quit)
    );
    assert!(!text.contains("broke"), "{}", format_output(&quit));
}

/// A recorded failed run replays to the same failure: the recording carries
/// `Run.fail` and every reading the loop made of it, and the replay answers
/// the same `Err` the run did.
#[test]
fn a_recorded_failed_run_replays_to_the_same_failure() {
    for (name, args) in [
        ("run_fail", &[][..]),
        ("run_fail_answer", &["--", "late"][..]),
    ] {
        let dir = scratch(&format!("{name}-replay"));
        let mut recorded = Command::new(aver_bin());
        recorded
            .current_dir(fixture(name))
            .arg("run")
            .arg("main.av")
            .arg("--module-root")
            .arg(fixture(name))
            .arg("--record")
            .arg(&dir)
            .args(args);
        let out = recorded.output().expect("aver runs");
        let recording = only_recording(&dir);
        let written = std::fs::read_to_string(&recording).expect("recording is readable");
        assert!(
            written.contains("\"Run.fail\"") && written.contains("\"Run.failure\""),
            "{name}: the recording does not carry the failure:\n{}",
            format_output(&out)
        );

        let mut command = Command::new(aver_bin());
        command.current_dir(fixture(name));
        command.arg("replay").arg(&recording).arg("--check-args");
        let replayed = command.output().expect("aver replays");
        assert!(replayed.status.success(), "{}", format_output(&replayed));
        assert!(
            combined(&replayed).contains("Output:  MATCH"),
            "{name}: {}",
            format_output(&replayed)
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}

/// A program whose processes and answer modules never call `Run.fail` gets
/// the loop it had: no failure field, no reading of one, no new effect.
#[test]
fn a_loop_that_cannot_fail_reads_no_failure() {
    let dump = |name: &str| {
        let dir = fixture(name);
        let out = Command::new(aver_bin())
            .current_dir(&dir)
            .env("AVER_YIELD_DUMP", "1")
            .arg("check")
            .arg("main.av")
            .arg("--module-root")
            .arg(&dir)
            .output()
            .expect("aver runs");
        combined(&out)
    };
    let quiet = dump("run_all_from_main");
    assert!(quiet.contains("fn __over"), "{quiet}");
    assert!(!quiet.contains("Run.failure"), "{quiet}");
    assert!(!quiet.contains("failed:"), "{quiet}");

    let failing = dump("run_fail");
    for line in [
        "failed: Option<String>",
        "fn __failedAfter(run: __Run) -> __Run",
        "__failedAfter(__seatFamilies(served))",
        "Bool.or(__hasFailed(run), ",
        "fn __outcome(run: __Run) -> Result<Unit, String>",
    ] {
        assert!(failing.contains(line), "missing `{line}`:\n{failing}");
    }
}

// ── Run.lastTurn ────────────────────────────────────────────────────────

#[path = "support/last_turn.rs"]
mod last_turn;

/// The ticker reads `Run.lastTurn()` while it is seated, after three ticks
/// that each park on a 250 ms deadline, and after one that parks until the
/// next turn. The seated reading is 0/0; a tick waited about its deadline and
/// the turn before it worked far less; the tick that waited for nothing
/// waited next to nothing.
#[test]
fn run_last_turn_reports_the_wait_and_the_work_around_a_turn() {
    let out = aver_within("run_last_turn", &["run"], 60);
    assert!(out.status.success(), "{}", format_output(&out));
    last_turn::check(&String::from_utf8_lossy(&out.stdout))
        .unwrap_or_else(|error| panic!("{error}"));
}

/// The readings are recorded like `Time.unixMs`: the recording carries every
/// `Run.lastTurn` and the loop's marks around its waits, and a replay answers
/// the recorded numbers, so it prints the run's lines again, digit for digit.
#[test]
fn a_recorded_last_turn_replays_to_the_same_numbers() {
    let dir = scratch("last-turn-replay");
    let out = aver_within(
        "run_last_turn",
        &["run", "--record", dir.to_str().expect("utf-8 scratch path")],
        60,
    );
    assert!(out.status.success(), "{}", format_output(&out));
    let recording = only_recording(&dir);
    let written = std::fs::read_to_string(&recording).expect("recording is readable");
    for effect in ["\"Run.lastTurn\"", "\"Run.waitStarts\"", "\"Run.waitEnds\""] {
        assert!(
            written.contains(effect),
            "the recording does not carry {effect}:\n{}",
            format_output(&out)
        );
    }

    let replayed = Command::new(aver_bin())
        .current_dir(fixture("run_last_turn"))
        .arg("replay")
        .arg(&recording)
        .arg("--check-args")
        .output()
        .expect("aver replays");
    assert!(replayed.status.success(), "{}", format_output(&replayed));
    assert!(
        combined(&replayed).contains("Output:  MATCH"),
        "{}",
        format_output(&replayed)
    );
    let _ = std::fs::remove_dir_all(&dir);
}

/// What `AVER_YIELD_DUMP` prints for one fixture: the protocol and the loop.
fn loop_dump(name: &str) -> String {
    let dir = fixture(name);
    let out = Command::new(aver_bin())
        .current_dir(&dir)
        .env("AVER_YIELD_DUMP", "1")
        .arg("check")
        .arg("main.av")
        .arg("--module-root")
        .arg(&dir)
        .output()
        .expect("aver runs");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// A program that never reads `Run.lastTurn` gets the loop it had before the
/// operation existed, character for character. The digests were taken from
/// the loop the compiler generated for these fixtures before `Run.lastTurn`
/// was added; every backend compiles that one loop. A change to the
/// generator moves them: read the loop with
/// `AVER_YIELD_DUMP=1 aver check main.av --module-root .` and accept the new
/// snapshot only when the change is meant.
#[test]
fn a_loop_that_reads_no_last_turn_is_the_loop_it_was() {
    use sha2::{Digest, Sha256};
    let mut digests = String::new();
    for name in [
        "run_all_from_main",
        "run_all_slice",
        "run_fail",
        "run_fail_answer",
        "run_guide_example",
    ] {
        let dump = loop_dump(name);
        assert!(dump.contains("fn __turn"), "{name}: no loop in\n{dump}");
        assert!(!dump.contains("Run.waitStarts"), "{name}:\n{dump}");
        assert!(!dump.contains("Run.waitEnds"), "{name}:\n{dump}");
        let digest = Sha256::digest(dump.as_bytes());
        let hex: String = digest.iter().map(|byte| format!("{byte:02x}")).collect();
        digests.push_str(&format!("{name} {hex}\n"));
    }
    insta::assert_snapshot!("loop_without_last_turn", digests);

    let measuring = loop_dump("run_last_turn");
    for line in [
        "    Run.waitStarts()\n    keys = Wait.poll(plan.items, __timeout(observed))?",
        "    Run.waitEnds()\n    ready = __readySlots(__waitPlan(run).owners, keys, [])",
        "    ! [Process.stopRequested, Run.waitStarts]\n    Run.waitStarts()\n    __Run.update(run, stopping = Process.stopRequested())",
    ] {
        assert!(measuring.contains(line), "missing `{line}`:\n{measuring}");
    }
}

/// A `main` that reads `Run.lastTurn` in a program that runs no generated
/// loop is refused at check time: there is no turn to report.
#[test]
fn last_turn_without_a_loop_is_refused_at_check_time() {
    let dir = scratch("last-turn-standalone");
    std::fs::write(
        dir.join("main.av"),
        "module Standalone\n    intent = \"Reads the last turn with no loop to report one.\"\n\nfn main() -> Unit\n    ? \"Prints how long the last turn waited.\"\n    ! [Console.print, Run.lastTurn]\n    turn = Run.lastTurn()\n    Console.print(\"waited {turn.waitedMs}\")\n",
    )
    .expect("write the program");
    for command in ["check", "run"] {
        let out = Command::new(aver_bin())
            .current_dir(&dir)
            .arg(command)
            .arg("main.av")
            .output()
            .expect("aver runs");
        assert!(!out.status.success(), "{command}: {}", format_output(&out));
        assert!(
            combined(&out).contains("runs no generated loop"),
            "{command}: {}",
            format_output(&out)
        );
    }
    let _ = std::fs::remove_dir_all(&dir);
}
