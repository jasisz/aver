//! `projects/song`: a piece played by processes, synthesised in Aver.
//!
//! The project is checked and its verify cases run, so a language change
//! that breaks it is caught here rather than by someone trying to listen.

use std::process::Command;

const SONG: &str = "projects/song";

fn aver(args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(args)
        .output()
        .expect("aver runs")
}

fn assert_ok(output: &std::process::Output, what: &str) {
    assert!(
        output.status.success(),
        "{what} failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn song_passes_check_and_lists_its_processes() {
    let output = aver(&["check", &format!("{SONG}/main.av"), "--module-root", SONG]);
    assert_ok(&output, "aver check projects/song");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("process pulsing: requests Speaker.play, answered by Synth"),
        "the process listing names why pulsing is a process:\n{stdout}"
    );
}

#[test]
fn song_verify_cases_pass() {
    for file in ["main.av", "stage.av", "synth.av", "wave.av"] {
        let output = aver(&["verify", &format!("{SONG}/{file}"), "--module-root", SONG]);
        assert_ok(&output, &format!("aver verify projects/song/{file}"));
    }
}
