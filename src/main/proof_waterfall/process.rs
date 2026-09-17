//! Bounded subprocesses with file-backed output, so verbose Lean diagnostics
//! cannot block a pipe. A timeout cancels Lake and its Lean children together.

use std::io::{Read, Seek};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

pub struct Run {
    pub ok: bool,
    pub timed_out: bool,
    pub output: String,
}

fn cancel(child: &mut Child) {
    #[cfg(unix)]
    let _ = Command::new("/bin/kill")
        .args(["-KILL", "--", &format!("-{}", child.id())])
        .output();
    #[cfg(windows)]
    let _ = Command::new("taskkill")
        .args(["/PID", &child.id().to_string(), "/T", "/F"])
        .output();
    let _ = child.kill();
    let _ = child.wait();
}

pub fn run(mut command: Command, seconds: u32) -> Result<Run, String> {
    let mut output = tempfile::tempfile().map_err(|e| e.to_string())?;
    command
        .stdin(Stdio::null())
        .stdout(output.try_clone().map_err(|e| e.to_string())?)
        .stderr(output.try_clone().map_err(|e| e.to_string())?);
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        command.process_group(0);
    }
    let mut child = command
        .spawn()
        .map_err(|e| format!("could not start proof subprocess: {e}"))?;
    let start = Instant::now();
    let (ok, timed_out) = loop {
        match child.try_wait() {
            Ok(Some(status)) => break (status.success(), false),
            Ok(None) if start.elapsed() >= Duration::from_secs(u64::from(seconds)) => {
                cancel(&mut child);
                break (false, true);
            }
            Ok(None) => std::thread::sleep(Duration::from_millis(50)),
            Err(error) => {
                cancel(&mut child);
                return Err(error.to_string());
            }
        }
    };
    output.rewind().map_err(|e| e.to_string())?;
    let mut text = String::new();
    output
        .read_to_string(&mut text)
        .map_err(|e| e.to_string())?;
    Ok(Run {
        ok,
        timed_out,
        output: text,
    })
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    #[test]
    fn waterfall_timeout_cancels_descendants() {
        let dir = tempfile::tempdir().unwrap();
        let escaped = dir.path().join("escaped");
        let mut command = Command::new("/bin/sh");
        command
            .args([
                "-c",
                "(sleep 2; printf escaped > \"$1\") & wait",
                "timeout-test",
            ])
            .arg(&escaped);
        let result = run(command, 1).unwrap();
        assert!(result.timed_out && !result.ok);
        std::thread::sleep(Duration::from_millis(1500));
        assert!(!escaped.exists(), "the child must not survive the timeout");
    }
}
