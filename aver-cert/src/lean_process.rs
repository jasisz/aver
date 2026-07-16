//! Hermetic subprocess boundary for the checker-owned Lean toolchain.
//!
//! Lake's `env` command prepends project/toolchain search paths to ambient
//! `LEAN_PATH` and `LEAN_SRC_PATH`; it does not replace them. Every verifier
//! subprocess therefore starts from an empty environment. The Elan home is the
//! explicit bootstrap trust anchor: its canonical `bin/elan` is selected once,
//! invoked by absolute path, and told to run the exact embedded toolchain.

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::time::{Duration, Instant};

/// Overrides the per-step wall-clock limit, in whole seconds, for every Lean
/// toolchain subprocess on the verify/check path.
const PHASE_TIMEOUT_ENV: &str = "AVER_CERT_PHASE_TIMEOUT_SECS";
/// Generous default: real certificate verifies take minutes per step, and the
/// first run on a machine may also install the pinned toolchain.
const DEFAULT_PHASE_TIMEOUT: Duration = Duration::from_secs(15 * 60);
const WAIT_POLL: Duration = Duration::from_millis(50);

pub(crate) struct LeanRunner {
    elan: PathBuf,
    elan_home: PathBuf,
    toolchain: String,
    system_environment: Vec<(&'static str, OsString)>,
    phase_timeout: Duration,
}

impl LeanRunner {
    pub(crate) fn new(toolchain: &str) -> Result<Self, String> {
        Self::with_environment(toolchain, |name| std::env::var_os(name))
    }

    fn with_environment(
        toolchain: &str,
        environment: impl Fn(&str) -> Option<OsString>,
    ) -> Result<Self, String> {
        let toolchain = toolchain.trim();
        if toolchain.is_empty() || toolchain.chars().any(char::is_whitespace) {
            return Err("embedded Lean toolchain name is empty or malformed".to_string());
        }
        let phase_timeout = phase_timeout(&environment)?;

        #[cfg(windows)]
        let user_home =
            nonempty(environment("USERPROFILE")).or_else(|| nonempty(environment("HOME")));
        #[cfg(not(windows))]
        let user_home = nonempty(environment("HOME"));
        let elan_home = nonempty(environment("ELAN_HOME"))
            .map(PathBuf::from)
            .or_else(|| user_home.map(|home| PathBuf::from(home).join(".elan")))
            .ok_or_else(|| {
                "cannot resolve the pinned Lean toolchain: set ELAN_HOME or HOME".to_string()
            })?;
        let elan_home = std::fs::canonicalize(&elan_home).map_err(|error| {
            format!(
                "cannot resolve trusted Elan home {}: {error}",
                elan_home.display()
            )
        })?;
        let elan_name = format!("elan{}", std::env::consts::EXE_SUFFIX);
        let elan = elan_home.join("bin").join(elan_name);
        let elan = std::fs::canonicalize(&elan).map_err(|error| {
            format!(
                "cannot resolve trusted Elan executable {}: {error}; install Elan there or set ELAN_HOME",
                elan.display()
            )
        })?;
        if !elan.is_file() {
            return Err(format!(
                "trusted Elan executable is not a regular file: {}",
                elan.display()
            ));
        }

        let system_environment = Vec::new();

        #[cfg(windows)]
        let mut system_environment = system_environment;
        #[cfg(windows)]
        {
            copy_if_present(&mut system_environment, "SYSTEMROOT", &environment);
            copy_if_present(&mut system_environment, "WINDIR", &environment);
        }

        Ok(Self {
            elan,
            elan_home,
            toolchain: toolchain.to_string(),
            system_environment,
            phase_timeout,
        })
    }

    pub(crate) fn run_lake(
        &self,
        work_dir: &Path,
        phase: &str,
        arguments: &[&str],
    ) -> Result<Output, String> {
        let work_dir = std::fs::canonicalize(work_dir).map_err(|error| {
            format!(
                "cannot resolve checker-owned Lean work dir {}: {error}",
                work_dir.display()
            )
        })?;
        let temp_dir = work_dir.join(format!(
            ".aver-cert-tmp-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        let mut builder = std::fs::DirBuilder::new();
        #[cfg(unix)]
        {
            use std::os::unix::fs::DirBuilderExt;
            builder.mode(0o700);
        }
        builder.create(&temp_dir).map_err(|error| {
            format!(
                "cannot create checker-owned Lean temp dir {}: {error}",
                temp_dir.display()
            )
        })?;

        let output = run_with_timeout(
            self.lake_command(&work_dir, &temp_dir, arguments),
            self.phase_timeout,
            phase,
        );
        let _ = std::fs::remove_dir_all(&temp_dir);
        output
    }

    fn lake_command(&self, work_dir: &Path, temp_dir: &Path, arguments: &[&str]) -> Command {
        let mut command = Command::new(&self.elan);
        command.env_clear();
        command
            .env("ELAN_HOME", &self.elan_home)
            // Lake 4.32 reads its toolchain-local artifact cache by default.
            // Disable every implicit Lake cache; Aver caches are explicit,
            // content-addressed, and validated separately.
            .env("LAKE_ARTIFACT_CACHE", "0")
            .env("LAKE_RESTORE_ARTIFACTS", "0")
            .env("LAKE_NO_CACHE", "1")
            .env("LAKE_CACHE_DIR", "");
        for (name, value) in &self.system_environment {
            command.env(name, value);
        }

        // Never inherit an attacker-selected temp root. Both production callers
        // pass a newly created checker-owned project directory here.
        command
            .env("TMPDIR", temp_dir)
            .env("TMP", temp_dir)
            .env("TEMP", temp_dir)
            .current_dir(work_dir)
            .arg("run")
            .arg("--install")
            .arg(&self.toolchain)
            .arg("lake")
            .args(arguments);
        command
    }
}

fn phase_timeout(environment: &impl Fn(&str) -> Option<OsString>) -> Result<Duration, String> {
    let Some(value) = nonempty(environment(PHASE_TIMEOUT_ENV)) else {
        return Ok(DEFAULT_PHASE_TIMEOUT);
    };
    let text = value.to_string_lossy();
    text.trim()
        .parse::<u64>()
        .ok()
        .filter(|&seconds| seconds > 0)
        .map(Duration::from_secs)
        .ok_or_else(|| {
            format!(
                "{PHASE_TIMEOUT_ENV} must be a positive whole number of seconds, got `{}`",
                text.trim()
            )
        })
}

/// Run one verifier step to completion under a wall-clock limit. On expiry the
/// step's entire process tree is killed and a fail-closed error naming the
/// step is returned: a timed-out step can never contribute to a green verdict.
fn run_with_timeout(mut command: Command, limit: Duration, phase: &str) -> Result<Output, String> {
    command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        // A fresh process group lets the checker stop lake together with
        // every lean worker it spawned using one group-wide signal.
        command.process_group(0);
    }
    let mut child = command.spawn().map_err(|error| {
        format!("could not start the trusted Lean toolchain for the {phase} step: {error}")
    })?;
    let stdout = drain(child.stdout.take());
    let stderr = drain(child.stderr.take());

    // The deadline covers both process exit and output drain: a runaway
    // descendant holding the output pipes open must not stall the verifier.
    let deadline = Instant::now() + limit;
    let mut status = None;
    let finished = loop {
        if status.is_none() {
            status = match child.try_wait() {
                Ok(exited) => exited,
                Err(error) => {
                    kill_process_tree(&mut child);
                    let _ = child.wait();
                    return Err(format!("could not observe the {phase} step: {error}"));
                }
            };
        }
        match status {
            Some(status) if stdout.is_finished() && stderr.is_finished() => break Some(status),
            _ if Instant::now() >= deadline => break None,
            _ => std::thread::sleep(WAIT_POLL),
        }
    };

    let Some(status) = finished else {
        kill_process_tree(&mut child);
        let _ = child.wait();
        let _ = (join_drain(stdout), join_drain(stderr));
        return Err(format!(
            "the {phase} step exceeded its {}-second time limit and was stopped; \
             the certificate is not accepted (set {PHASE_TIMEOUT_ENV} to allow more time)",
            limit.as_secs()
        ));
    };
    Ok(Output {
        status,
        stdout: join_drain(stdout),
        stderr: join_drain(stderr),
    })
}

/// Stop the child and every process it spawned. On Unix the child leads its
/// own process group, so one group-wide SIGKILL reaches lake and its lean
/// workers; on Windows `taskkill /T` walks the process tree.
fn kill_process_tree(child: &mut Child) {
    #[cfg(unix)]
    {
        let _ = Command::new("kill")
            .args(["-KILL", "--", &format!("-{}", child.id())])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
    }
    #[cfg(windows)]
    {
        let _ = Command::new("taskkill")
            .args(["/T", "/F", "/PID", &child.id().to_string()])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
    }
    let _ = child.kill();
}

fn drain(pipe: Option<impl std::io::Read + Send + 'static>) -> std::thread::JoinHandle<Vec<u8>> {
    std::thread::spawn(move || {
        let mut buffer = Vec::new();
        if let Some(mut pipe) = pipe {
            let _ = pipe.read_to_end(&mut buffer);
        }
        buffer
    })
}

fn join_drain(reader: std::thread::JoinHandle<Vec<u8>>) -> Vec<u8> {
    reader.join().unwrap_or_default()
}

fn unique_nanos() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0)
}

#[cfg(windows)]
fn copy_if_present(
    destination: &mut Vec<(&'static str, OsString)>,
    name: &'static str,
    environment: &impl Fn(&str) -> Option<OsString>,
) {
    if let Some(value) = nonempty(environment(name)) {
        destination.push((name, value));
    }
}

fn nonempty(value: Option<OsString>) -> Option<OsString> {
    value.filter(|value| !value.is_empty())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wall;
    use std::collections::BTreeMap;
    use std::path::PathBuf;
    use std::process::Command;

    fn command_environment(command: &Command) -> BTreeMap<String, OsString> {
        command
            .get_envs()
            .filter_map(|(name, value)| {
                value.map(|value| (name.to_string_lossy().into_owned(), value.to_os_string()))
            })
            .collect()
    }

    #[test]
    fn command_whitelists_only_toolchain_bootstrap_and_checker_temp() {
        let runner = LeanRunner {
            elan: PathBuf::from("/trusted/elan/bin/elan"),
            elan_home: PathBuf::from("/trusted/elan"),
            toolchain: wall::LEAN_TOOLCHAIN.trim().to_string(),
            system_environment: Vec::new(),
            phase_timeout: DEFAULT_PHASE_TIMEOUT,
        };
        let command = runner.lake_command(
            Path::new("/checker/build"),
            Path::new("/checker/temp"),
            &["env", "lean"],
        );

        assert_eq!(command.get_program(), "/trusted/elan/bin/elan");
        assert_eq!(
            command
                .get_args()
                .map(|argument| argument.to_string_lossy().into_owned())
                .collect::<Vec<_>>(),
            [
                "run",
                "--install",
                wall::LEAN_TOOLCHAIN.trim(),
                "lake",
                "env",
                "lean"
            ]
        );

        let environment = command_environment(&command);
        for forbidden in [
            "LEAN_PATH",
            "LEAN_SRC_PATH",
            "LEAN_SYSROOT",
            "ELAN_TOOLCHAIN",
            "PATH",
            "HOME",
            "LAKE_HOME",
            "LAKE_OVERRIDE_LEAN",
        ] {
            assert!(
                !environment.contains_key(forbidden),
                "forwarded {forbidden}"
            );
        }
        assert_eq!(environment.get("ELAN_HOME").unwrap(), "/trusted/elan");
        assert_eq!(environment.get("LAKE_ARTIFACT_CACHE").unwrap(), "0");
        assert_eq!(environment.get("LAKE_RESTORE_ARTIFACTS").unwrap(), "0");
        assert_eq!(environment.get("LAKE_NO_CACHE").unwrap(), "1");
        assert_eq!(environment.get("LAKE_CACHE_DIR").unwrap(), "");
        for temp in ["TMPDIR", "TMP", "TEMP"] {
            assert_eq!(environment.get(temp).unwrap(), "/checker/temp");
        }
    }

    struct TestTree(PathBuf);

    impl TestTree {
        fn new() -> Self {
            let path = std::env::temp_dir().join(format!(
                "aver-cert-lean-env-{}-{}",
                std::process::id(),
                std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .map(|duration| duration.as_nanos())
                    .unwrap_or(0)
            ));
            std::fs::create_dir(&path).unwrap();
            Self(path)
        }
    }

    impl Drop for TestTree {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }

    fn report(output: &std::process::Output) -> String {
        format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
    }

    /// Lean 4.32's `lake env` keeps inherited LEAN_PATH entries live after its
    /// own package paths. Prove the baseline can import a valid ambient olean,
    /// then prove the production environment boundary makes it unreachable.
    #[test]
    fn hostile_lean_path_olean_is_not_visible() {
        let toolchain = wall::LEAN_TOOLCHAIN.trim();
        let available = Command::new("elan")
            .args(["run", "--install", toolchain, "lake", "--version"])
            .output();
        if !matches!(available, Ok(output) if output.status.success()) {
            assert!(
                std::env::var_os("AVER_CERT_REQUIRE_PINNED_LEAN").is_none(),
                "pinned Elan toolchain is required for this security gate"
            );
            eprintln!("skipping hostile LEAN_PATH test: pinned Elan toolchain unavailable");
            return;
        }

        let tree = TestTree::new();
        let ambient = tree.0.join("ambient");
        let consumer = tree.0.join("consumer");
        std::fs::create_dir(&ambient).unwrap();
        std::fs::create_dir(&consumer).unwrap();
        for directory in [&ambient, &consumer] {
            std::fs::write(directory.join("lean-toolchain"), wall::LEAN_TOOLCHAIN).unwrap();
            std::fs::write(
                directory.join("lakefile.lean"),
                "import Lake\nopen Lake DSL\npackage «envprobe» where\n",
            )
            .unwrap();
        }
        std::fs::write(
            ambient.join("CertPrelude.lean"),
            "def ambientMarker : Nat := 666\n",
        )
        .unwrap();

        let runner = LeanRunner::new(wall::LEAN_TOOLCHAIN).unwrap();
        let built = runner
            .run_lake(
                &ambient,
                "shadow olean build",
                &["env", "lean", "-o", "CertPrelude.olean", "CertPrelude.lean"],
            )
            .unwrap();
        assert!(
            built.status.success(),
            "shadow olean build failed:\n{}",
            report(&built)
        );
        std::fs::remove_file(ambient.join("CertPrelude.lean")).unwrap();
        assert!(ambient.join("CertPrelude.olean").is_file());

        std::fs::write(
            consumer.join("Probe.lean"),
            "import CertPrelude\n\ndef observed : Nat := ambientMarker\n",
        )
        .unwrap();

        let baseline = Command::new("elan")
            .current_dir(&consumer)
            .env("LEAN_PATH", &ambient)
            .args(["run", toolchain, "lake", "env", "lean", "Probe.lean"])
            .output()
            .unwrap();
        assert!(
            baseline.status.success(),
            "Lean 4.32 baseline did not retain ambient LEAN_PATH:\n{}",
            report(&baseline)
        );

        let hostile = |name: &str| match name {
            "LEAN_PATH" | "LEAN_SRC_PATH" => Some(ambient.as_os_str().to_os_string()),
            "ELAN_TOOLCHAIN" => Some(OsString::from("leanprover/lean4:v4.31.0")),
            _ => std::env::var_os(name),
        };
        let hardened = LeanRunner::with_environment(wall::LEAN_TOOLCHAIN, hostile)
            .unwrap()
            .run_lake(&consumer, "hostile probe", &["env", "lean", "Probe.lean"])
            .unwrap();
        let hardened_report = report(&hardened);
        assert!(
            !hardened.status.success(),
            "hostile ambient olean crossed the cleared environment:\n{hardened_report}"
        );
        assert!(
            hardened_report.contains("CertPrelude")
                && (hardened_report.contains("unknown module")
                    || hardened_report.contains("not found")),
            "hardened failure was not the missing hostile module:\n{hardened_report}"
        );
    }

    fn runner_with_timeout_setting(value: Option<&str>) -> Result<LeanRunner, String> {
        let tree = TestTree::new();
        let elan_home = tree.0.join("elan");
        std::fs::create_dir_all(elan_home.join("bin")).unwrap();
        std::fs::write(
            elan_home
                .join("bin")
                .join(format!("elan{}", std::env::consts::EXE_SUFFIX)),
            b"placeholder",
        )
        .unwrap();

        let elan_home = elan_home.into_os_string();
        let timeout = value.map(OsString::from);
        LeanRunner::with_environment(wall::LEAN_TOOLCHAIN, move |name| match name {
            "ELAN_HOME" => Some(elan_home.clone()),
            PHASE_TIMEOUT_ENV => timeout.clone(),
            _ => None,
        })
    }

    #[test]
    fn phase_timeout_defaults_generously_and_validates_the_override() {
        let default = runner_with_timeout_setting(None).unwrap();
        assert_eq!(default.phase_timeout, DEFAULT_PHASE_TIMEOUT);
        assert_eq!(default.phase_timeout, Duration::from_secs(900));

        let overridden = runner_with_timeout_setting(Some("120")).unwrap();
        assert_eq!(overridden.phase_timeout, Duration::from_secs(120));

        for invalid in ["0", "-5", "soon", "1.5"] {
            let Err(error) = runner_with_timeout_setting(Some(invalid)) else {
                panic!("malformed timeout override `{invalid}` must fail closed");
            };
            assert!(error.contains(PHASE_TIMEOUT_ENV), "{error}");
        }
    }

    /// The hang guard must kill the whole process group: lake spawns lean
    /// workers, and killing only the direct child would leave them running.
    #[cfg(unix)]
    #[test]
    fn expired_phase_kills_the_whole_process_group() {
        let tree = TestTree::new();
        let pid_file = tree.0.join("grandchild.pid");
        let mut command = Command::new("sh");
        command.arg("-c").arg(format!(
            "tail -f /dev/null & echo $! > '{}'; wait",
            pid_file.display()
        ));

        let error = run_with_timeout(command, Duration::from_secs(1), "test probe")
            .expect_err("a hung step must fail closed");
        assert!(error.contains("test probe"), "{error}");
        assert!(error.contains("time limit"), "{error}");
        assert!(error.contains(PHASE_TIMEOUT_ENV), "{error}");

        let grandchild =
            std::fs::read_to_string(&pid_file).expect("grandchild pid recorded before the timeout");
        let grandchild = grandchild.trim().to_string();
        assert!(!grandchild.is_empty(), "grandchild pid file was empty");

        let mut alive = true;
        for _ in 0..100 {
            let probe = Command::new("ps")
                .args(["-p", &grandchild, "-o", "pid="])
                .output()
                .expect("ps runs");
            if !probe.status.success() || String::from_utf8_lossy(&probe.stdout).trim().is_empty() {
                alive = false;
                break;
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        assert!(
            !alive,
            "grandchild process {grandchild} survived the group kill"
        );
    }
}
