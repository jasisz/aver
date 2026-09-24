//! Stopping a running program the way a terminal does: SIGINT, once it has
//! said it is parked. Shared by the suites that check a stop request ends a
//! wait within a moment rather than at the wait's deadline.
#![allow(dead_code)]

use std::io::{BufRead, BufReader, Read};
use std::path::Path;
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

use super::aver_cmd::aver_bin;

/// Run `aver run` on a fixture that prints `ready_line` once it is parked,
/// send it SIGINT, and hand back what it printed and how long it took to
/// stop after the signal.
pub fn stopped_by_sigint(dir: &Path, ready_line: &str, extra: &[&str]) -> (Output, Duration) {
    let mut child = Command::new(aver_bin())
        .current_dir(dir)
        .arg("run")
        .arg("main.av")
        .arg("--module-root")
        .arg(dir)
        .args(extra)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("aver starts");
    let mut stdout = BufReader::new(child.stdout.take().expect("stdout"));
    let mut printed = String::new();
    loop {
        let mut line = String::new();
        let read = stdout.read_line(&mut line).expect("stdout reads");
        printed.push_str(&line);
        assert!(read > 0, "the run ended before it parked:\n{printed}");
        if line.contains(ready_line) {
            break;
        }
    }
    let signalled = Instant::now();
    let status = Command::new("kill")
        .arg("-INT")
        .arg(child.id().to_string())
        .status()
        .expect("kill runs");
    assert!(status.success(), "kill -INT failed");
    let deadline = signalled + Duration::from_secs(20);
    let exit = loop {
        if let Some(exit) = child.try_wait().expect("the child can be waited on") {
            break exit;
        }
        assert!(
            Instant::now() < deadline,
            "the run ignored its stop request"
        );
        std::thread::sleep(Duration::from_millis(20));
    };
    let stopped_after = signalled.elapsed();
    stdout
        .read_to_string(&mut printed)
        .expect("the rest of stdout");
    let mut stderr = String::new();
    child
        .stderr
        .take()
        .expect("stderr")
        .read_to_string(&mut stderr)
        .expect("stderr reads");
    (
        Output {
            status: exit,
            stdout: printed.into_bytes(),
            stderr: stderr.into_bytes(),
        },
        stopped_after,
    )
}
