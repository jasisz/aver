//! Process boundary between the Aver toolchain and the independent verifier.
//!
//! `aver cert ...` deliberately does not parse verifier arguments and does not
//! link a verifier fallback. It forwards every argument after `cert` to an
//! `aver-cert` child with inherited standard streams, preferring the executable
//! installed next to `aver` and otherwise relying on `PATH`.

use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};

pub(super) fn run_if_requested() -> Option<Result<ExitStatus, String>> {
    let mut args = std::env::args_os();
    let _aver = args.next();
    if args.next().as_deref() != Some(OsStr::new("cert")) {
        return None;
    }

    let forwarded = args.collect::<Vec<_>>();
    let current_exe = std::env::current_exe().ok();
    let mut child = verifier_command(current_exe.as_deref(), forwarded);
    child
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit());
    Some(child.status().map_err(|error| {
        format!("could not run `aver-cert`: {error} (install it next to `aver` or put it on PATH)")
    }))
}

pub(super) fn exit_code(status: ExitStatus) -> i32 {
    if let Some(code) = status.code() {
        return code;
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(signal) = status.signal() {
            return 128 + signal;
        }
    }

    1
}

fn verifier_command(
    current_exe: Option<&Path>,
    forwarded: impl IntoIterator<Item = OsString>,
) -> Command {
    let mut command = Command::new(verifier_program(current_exe));
    command.args(forwarded);
    command
}

fn verifier_program(current_exe: Option<&Path>) -> PathBuf {
    let sibling = current_exe
        .and_then(Path::parent)
        .map(|parent| parent.join(format!("aver-cert{}", std::env::consts::EXE_SUFFIX)));
    sibling
        .filter(|candidate| is_executable_file(candidate))
        .unwrap_or_else(|| PathBuf::from(format!("aver-cert{}", std::env::consts::EXE_SUFFIX)))
}

fn is_executable_file(candidate: &Path) -> bool {
    let Ok(metadata) = candidate.metadata() else {
        return false;
    };
    if !metadata.is_file() {
        return false;
    }

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        metadata.permissions().mode() & 0o111 != 0
    }
    #[cfg(not(unix))]
    {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sibling_verifier_wins_over_path_and_arguments_stay_unparsed() {
        let dir = tempfile::tempdir().unwrap();
        let current_exe = dir
            .path()
            .join(format!("aver{}", std::env::consts::EXE_SUFFIX));
        let sibling = dir
            .path()
            .join(format!("aver-cert{}", std::env::consts::EXE_SUFFIX));
        std::fs::write(&sibling, b"test verifier placeholder").unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&sibling, std::fs::Permissions::from_mode(0o755)).unwrap();
        }

        let raw = vec![
            OsString::from("verify"),
            OsString::from("--future-verifier-flag"),
            OsString::from("artifact.wasm"),
        ];
        let command = verifier_command(Some(&current_exe), raw.clone());

        assert_eq!(command.get_program(), sibling.as_os_str());
        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            raw.iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn path_is_used_when_no_sibling_is_installed() {
        let dir = tempfile::tempdir().unwrap();
        let current_exe = dir
            .path()
            .join(format!("aver{}", std::env::consts::EXE_SUFFIX));
        let command = verifier_command(Some(&current_exe), [OsString::from("--help")]);

        assert_eq!(
            command.get_program(),
            OsStr::new(&format!("aver-cert{}", std::env::consts::EXE_SUFFIX))
        );
    }

    #[cfg(unix)]
    #[test]
    fn child_exit_code_is_preserved() {
        let status = Command::new("sh")
            .args(["-c", "exit 37"])
            .status()
            .expect("test child starts");

        assert_eq!(exit_code(status), 37);
    }
}
