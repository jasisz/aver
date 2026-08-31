use std::ffi::OsStr;
use std::fmt;
use std::io;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};

#[derive(Debug)]
pub(super) enum CargoCheckError {
    Spawn {
        manifest: PathBuf,
        source: io::Error,
    },
    Failed {
        manifest: PathBuf,
        status: ExitStatus,
    },
}

impl CargoCheckError {
    pub(super) fn exit_code(&self) -> i32 {
        match self {
            Self::Spawn { .. } => 1,
            Self::Failed { status, .. } => status.code().unwrap_or(1),
        }
    }
}

impl fmt::Display for CargoCheckError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Spawn { manifest, source } => write!(
                formatter,
                "Generated Rust project remains at '{}', but `cargo check` could not start: {}",
                manifest
                    .parent()
                    .unwrap_or_else(|| Path::new("."))
                    .display(),
                source
            ),
            Self::Failed { manifest, status } => write!(
                formatter,
                "Generated Rust project remains at '{}', but `cargo check` failed with {}",
                manifest
                    .parent()
                    .unwrap_or_else(|| Path::new("."))
                    .display(),
                status
            ),
        }
    }
}

pub(super) fn run(output_dir: &Path) -> Result<(), CargoCheckError> {
    run_with_program(OsStr::new("cargo"), output_dir)
}

fn run_with_program(cargo: &OsStr, output_dir: &Path) -> Result<(), CargoCheckError> {
    let manifest = output_dir.join("Cargo.toml");
    let status = Command::new(cargo)
        .arg("check")
        .arg("--manifest-path")
        .arg(&manifest)
        .status()
        .map_err(|source| CargoCheckError::Spawn {
            manifest: manifest.clone(),
            source,
        })?;

    if status.success() {
        Ok(())
    } else {
        Err(CargoCheckError::Failed { manifest, status })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    use std::fs;
    #[cfg(unix)]
    use std::os::unix::fs::PermissionsExt;
    #[cfg(unix)]
    use std::os::unix::process::ExitStatusExt;

    #[cfg(unix)]
    fn shell_quote(path: &Path) -> String {
        format!("'{}'", path.to_string_lossy().replace('\'', "'\"'\"'"))
    }

    #[cfg(unix)]
    fn fake_cargo(dir: &Path, exit_code: i32) -> (PathBuf, PathBuf) {
        let program = dir.join("cargo");
        let args_log = dir.join("args.log");
        let script = format!(
            "#!/bin/sh\nprintf '%s\\n' \"$@\" > {}\nexit {}\n",
            shell_quote(&args_log),
            exit_code
        );
        fs::write(&program, script).expect("write fake cargo");
        let mut permissions = fs::metadata(&program)
            .expect("stat fake cargo")
            .permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&program, permissions).expect("make fake cargo executable");
        (program, args_log)
    }

    #[cfg(unix)]
    #[test]
    fn invokes_cargo_check_with_generated_manifest() {
        let temp = tempfile::tempdir().expect("create temp dir");
        let project = temp.path().join("generated");
        fs::create_dir_all(&project).expect("create generated project");
        let (cargo, args_log) = fake_cargo(temp.path(), 0);

        run_with_program(cargo.as_os_str(), &project).expect("fake cargo check should pass");

        let args = fs::read_to_string(args_log).expect("read fake cargo args");
        assert_eq!(
            args.lines().collect::<Vec<_>>(),
            vec![
                "check",
                "--manifest-path",
                project.join("Cargo.toml").to_string_lossy().as_ref()
            ]
        );
    }

    #[cfg(unix)]
    #[test]
    fn failed_cargo_check_preserves_the_status_exit_code() {
        let status = ExitStatus::from_raw(37 << 8);
        let error = CargoCheckError::Failed {
            manifest: PathBuf::from("/tmp/generated/Cargo.toml"),
            status,
        };

        assert_eq!(error.exit_code(), 37);
    }

    #[test]
    fn spawn_failure_uses_nonzero_exit_code() {
        let temp = tempfile::tempdir().expect("create temp dir");
        let missing = temp.path().join("definitely-not-cargo");

        let error = run_with_program(missing.as_os_str(), temp.path())
            .expect_err("missing cargo program should fail");

        assert_eq!(error.exit_code(), 1);
        assert!(error.to_string().contains("could not start"));
    }
}
