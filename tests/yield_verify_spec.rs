//! Direct-style process cases execute the lowered protocol with exact stubs.
//! The live answer deliberately disagrees, so a passing value also proves
//! that the verifier did not route the request through the answer module.

#[path = "support/aver_cmd.rs"]
mod aver_cmd;

use aver_cmd::{aver_bin, format_output, repo_root};
use std::path::Path;
use std::process::{Command, Output};

fn invoke(dir: &Path, command: &str, args: &[&str]) -> Output {
    Command::new(aver_bin())
        .current_dir(repo_root())
        .arg(command)
        .arg(dir.join("main.av"))
        .arg("--module-root")
        .arg(dir)
        .args(args)
        .output()
        .expect("aver runs")
}

fn edited_fixture(edit: impl FnOnce(String) -> String) -> tempfile::TempDir {
    let source = repo_root().join("tests/fixtures/yield_verify_stubs");
    let dir = tempfile::tempdir().unwrap();
    for name in ["main.av", "pool.av", "pooled.av", "aver.toml"] {
        std::fs::copy(source.join(name), dir.path().join(name)).unwrap();
    }
    let main = dir.path().join("main.av");
    std::fs::write(&main, edit(std::fs::read_to_string(&main).unwrap())).unwrap();
    dir
}

#[test]
fn process_cases_use_stubs_and_reset_their_counters_for_each_case() {
    let dir = repo_root().join("tests/fixtures/yield_verify_stubs");
    let out = invoke(&dir, "verify", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    assert!(text.contains("11/11 cases passed"), "{text}");
    assert!(!text.contains("__verifyProcess"), "{text}");
}

#[test]
fn a_process_case_requires_a_stub_for_each_request() {
    let dir =
        edited_fixture(|source| source.replace("    given answer: Pool.claim = [numbered]\n", ""));
    let out = invoke(dir.path(), "verify", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    let text = format_output(&out);
    assert!(text.contains("must supply an exact given stub"), "{text}");
    assert!(text.contains("Pool.claim"), "{text}");
}

#[test]
fn adding_process_cases_does_not_allow_plain_calls_in_the_program() {
    let dir = edited_fixture(|source| format!("{source}\nfn main() -> Int\n    pair(2)\n"));
    let out = invoke(dir.path(), "check", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("calls 'pair' directly"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn a_wrong_expected_process_result_fails_under_the_source_name() {
    let dir = edited_fixture(|source| source.replace("pair(2) => 15", "pair(2) => 16"));
    let out = invoke(dir.path(), "verify", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    let text = format_output(&out);
    assert!(text.contains("pair"), "{text}");
    assert!(text.contains("1 failed"), "{text}");
    assert!(!text.contains("__verifyProcess"), "{text}");
}

#[test]
fn process_verification_helpers_cannot_be_called_by_the_program() {
    let dir = edited_fixture(|source| {
        format!("{source}\nfn main() -> Int\n    __verifyProcess_pair(2)\n")
    });
    let out = invoke(dir.path(), "check", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("verify-only process driver"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn an_invalid_request_stub_is_type_checked() {
    let dir = edited_fixture(|source| {
        source.replace(
            "given answer: Pool.claim = [numbered]",
            "given answer: Pool.claim = [committed]",
        )
    });
    let out = invoke(dir.path(), "verify", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("stub"),
        "{}",
        format_output(&out)
    );
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_process_stubs_are_refused_with_a_vm_recipe() {
    let dir = repo_root().join("tests/fixtures/yield_verify_stubs");
    let out = invoke(&dir, "verify", &["--wasm-gc"]);
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("Use `aver verify` (VM)"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn an_unstubbed_in_place_effect_is_refused_before_host_dispatch() {
    let dir =
        edited_fixture(|source| source.replace("    given clock: Time.unixMs = [timeStub]\n", ""));
    let out = invoke(dir.path(), "verify", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    let text = format_output(&out);
    assert!(text.contains("Time.unixMs"), "{text}");
    assert!(
        text.contains("plain verify reached unstubbed effect"),
        "{text}"
    );
}

#[test]
fn a_process_loop_obeys_the_budget_named_after_the_source_function() {
    let dir = edited_fixture(|source| {
        format!(
            "{source}\nfn forever() -> Int\n    ! [yield]\n    forever()\n\nverify forever\n    forever() => 0\n"
        )
    });
    let config = dir.path().join("aver.toml");
    let original = std::fs::read_to_string(&config).unwrap();
    std::fs::write(config, format!("{original}\n[verify]\nstep-limit = 1000\n\n[[verify.costly]]\nfn = \"forever\"\nstep-limit = 2000\nreason = \"Exercise the process case budget\"\n")).unwrap();
    let out = invoke(dir.path(), "verify", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    let text = format_output(&out);
    assert!(text.contains("2000 steps, raised by"), "{text}");
    assert!(text.contains("fn = \"forever\""), "{text}");
}

#[test]
fn a_process_without_requests_can_finish_and_unit_results_can_be_verified() {
    let dir = edited_fixture(|source| {
        format!(
            "{source}\nfn countdown(n: Int) -> Int\n    ! [yield]\n    match n == 0\n        true -> 5\n        false -> countdown(n - 1)\n\nverify countdown\n    countdown(2) => 5\n\nfn notifyOnly() -> Unit\n    ! [Pool.notice, yield]\n    Pool.notice(1)\n\nverify notifyOnly\n    given answer: Pool.notice = [noticeStub]\n    notifyOnly() => Unit\n"
        )
    });
    let out = invoke(dir.path(), "verify", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("13/13 cases passed"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn coordinator_cases_pin_service_order_turn_groups_and_late_job_observations() {
    let dir = repo_root().join("tests/fixtures/run_schedule_cases");
    for args in [vec![], vec!["--hostile"]] {
        let out = invoke(&dir, "verify", &args);
        assert!(out.status.success(), "{}", format_output(&out));
        let text = String::from_utf8_lossy(&out.stdout);
        for label in [
            "serveGroups",
            "observeJob",
            "observeCancelled",
            "historyObservation",
            "historyJobLimit",
        ] {
            assert!(text.contains(label), "{text}");
        }
        assert!(text.contains("0 failed"), "{text}");
    }
}

#[test]
fn coordinator_laws_are_universal_beside_the_vm_schedule_scenarios() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping coordinator Lean check: lake is unavailable");
        return;
    }
    let fixture = repo_root().join("tests/fixtures/run_schedule_cases");
    let source = tempfile::tempdir().unwrap();
    for name in [
        "main.av",
        "ledger.av",
        "inbox.av",
        "scoring.av",
        "aver.toml",
    ] {
        std::fs::copy(fixture.join(name), source.path().join(name)).unwrap();
    }
    // The cases exercise VM resource stubs. Export every function and law
    // unchanged, without asking the proof lifter to model these sampled cases.
    let main = source.path().join("main.av");
    let original = std::fs::read_to_string(&main).unwrap();
    let mut skip = false;
    let laws_only: String = original
        .split_inclusive('\n')
        .filter(|line| {
            if line.starts_with("verify ") {
                skip = !line.contains(" law ");
            } else if !line.trim().is_empty() && !line.starts_with(char::is_whitespace) {
                skip = false;
            }
            !skip
        })
        .collect();
    std::fs::write(main, laws_only).unwrap();
    let target = tempfile::tempdir().unwrap();
    let out = invoke(
        source.path(),
        "proof",
        &[
            "--backend",
            "lean",
            "-o",
            target.path().to_str().unwrap(),
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(out.status.success(), "{}", format_output(&out));
    let stdout = String::from_utf8_lossy(&out.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .expect("proof summary"),
    )
    .unwrap();
    assert_eq!(summary["universal"], true, "{summary}");
    assert_eq!(summary["universal_laws"], 25, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    assert_eq!(summary["sorries"], 0, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{summary}");
}

#[test]
fn proof_export_refuses_process_cases_before_emitting_a_different_oracle_model() {
    let dir = repo_root().join("tests/fixtures/yield_verify_stubs");
    let target = tempfile::tempdir().unwrap();
    for backend in ["lean", "dafny"] {
        let out = invoke(
            &dir,
            "proof",
            &["--backend", backend, "-o", target.path().to_str().unwrap()],
        );
        assert!(!out.status.success(), "{}", format_output(&out));
        assert!(
            format_output(&out).contains("dynamic Oracle counter"),
            "{}",
            format_output(&out)
        );
    }
}

#[test]
fn process_cases_add_no_live_provider_requirement_to_run_or_rust() {
    let dir = edited_fixture(|source| format!("{source}\nfn main() -> Int\n    0\n"));
    let out = invoke(dir.path(), "run", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    let target = tempfile::tempdir().unwrap();
    let out = invoke(
        dir.path(),
        "compile",
        &["--target", "rust", "-o", target.path().to_str().unwrap()],
    );
    assert!(out.status.success(), "{}", format_output(&out));
    let rust = std::fs::read_to_string(target.path().join("src/main.rs")).unwrap();
    assert!(!rust.contains("__verifyDrive"), "{rust}");
    assert!(!rust.contains("__verifyProcess"), "{rust}");
}

#[test]
fn imported_process_cases_keep_their_metadata_and_add_no_live_provider_requirement() {
    let dir = edited_fixture(|source| source.replace("module YieldVerifyStubs", "module Worker"));
    std::fs::rename(dir.path().join("main.av"), dir.path().join("worker.av")).unwrap();
    std::fs::write(dir.path().join("main.av"), "module Entry\n    intent = \"Import a tested process protocol.\"\n    depends [Worker, Pool, Pooled]\n\nfn main() -> Int\n    0\n").unwrap();
    for command in ["verify", "run"] {
        let out = invoke(dir.path(), command, &[]);
        assert!(out.status.success(), "{}", format_output(&out));
        if command == "verify" {
            assert!(
                format_output(&out).contains("11/11 cases passed"),
                "{}",
                format_output(&out)
            );
        }
    }
    for backend in ["rust", "wasm-gc"] {
        if backend == "wasm-gc" && !cfg!(feature = "wasm") {
            continue;
        }
        let target = tempfile::tempdir().unwrap();
        let out = invoke(
            dir.path(),
            "compile",
            &["--target", backend, "-o", target.path().to_str().unwrap()],
        );
        assert!(out.status.success(), "{}", format_output(&out));
    }
}

#[cfg(feature = "wasm")]
#[test]
fn process_cases_add_no_request_import_to_wasm_artifacts() {
    let dir = edited_fixture(|source| format!("{source}\nfn main() -> Int\n    0\n"));
    for backend in ["wasm-gc", "wasip2"] {
        if backend == "wasip2" && !cfg!(feature = "wasip2") {
            continue;
        }
        let target = tempfile::tempdir().unwrap();
        let out = invoke(
            dir.path(),
            "compile",
            &["--target", backend, "-o", target.path().to_str().unwrap()],
        );
        assert!(out.status.success(), "{}", format_output(&out));
        assert!(
            !format_output(&out).contains("capability Pool:"),
            "{}",
            format_output(&out)
        );
    }
}
