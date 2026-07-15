//! End-to-end checks for the intentionally opaque `aver cert` subprocess
//! boundary. The child is a tiny fake verifier so the test covers delegation,
//! not certificate semantics.

#![cfg(unix)]

use std::ffi::OsString;
use std::fs;
use std::os::unix::ffi::OsStringExt;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, Stdio};

fn install_executable(path: &Path, contents: &[u8]) {
    fs::write(path, contents).expect("write executable");
    fs::set_permissions(path, fs::Permissions::from_mode(0o755)).expect("chmod executable");
}

fn copy_aver(dir: &Path) -> std::path::PathBuf {
    let destination = dir.join("aver");
    fs::copy(env!("CARGO_BIN_EXE_aver"), &destination).expect("copy aver test binary");
    destination
}

#[test]
fn sibling_receives_raw_argv_and_inherited_streams_and_controls_exit() {
    let dir = tempfile::tempdir().expect("tempdir");
    let aver = copy_aver(dir.path());
    let verifier = dir.path().join("aver-cert");
    install_executable(
        &verifier,
        br#"#!/bin/sh
IFS= read -r input
printf 'child-stdout:%s:%s\n' "$1" "$input"
printf 'child-stderr:%s\n' "$2" >&2
expected=$(printf 'raw-\377')
test "$3" = "$expected" || exit 91
exit 37
"#,
    );

    let raw_name = OsString::from_vec(b"raw-\xff".to_vec());

    let mut child = Command::new(aver)
        .current_dir(dir.path())
        .arg("cert")
        .arg("--future-verifier-flag")
        .arg("stderr-marker")
        .arg(raw_name)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn copied aver");
    use std::io::Write;
    child
        .stdin
        .take()
        .expect("piped stdin")
        .write_all(b"stdin-marker\n")
        .expect("write stdin");
    let output = child.wait_with_output().expect("wait for aver");

    assert_eq!(output.status.code(), Some(37));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "child-stdout:--future-verifier-flag:stdin-marker\n"
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        "child-stderr:stderr-marker\n"
    );
}

#[test]
fn path_verifier_is_used_when_no_sibling_exists() {
    let aver_dir = tempfile::tempdir().expect("aver tempdir");
    let path_dir = tempfile::tempdir().expect("PATH tempdir");
    let aver = copy_aver(aver_dir.path());
    install_executable(
        &path_dir.path().join("aver-cert"),
        b"#!/bin/sh\nprintf 'path-verifier\\n'\nexit 23\n",
    );

    let output = Command::new(aver)
        .args(["cert", "--future-command"])
        .env("PATH", path_dir.path())
        .output()
        .expect("run aver with PATH verifier");

    assert_eq!(output.status.code(), Some(23));
    assert_eq!(output.stdout, b"path-verifier\n");
}
