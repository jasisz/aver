//! Imported source calls compose the same protocols on every execution door.

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

fn fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    let source = repo_root().join("tests/fixtures/yield_module_helpers");
    for file in std::fs::read_dir(source).unwrap() {
        let file = file.unwrap();
        std::fs::copy(file.path(), dir.path().join(file.file_name())).unwrap();
    }
    dir
}

fn edit(dir: &Path, file: &str, change: impl FnOnce(String) -> String) {
    let path = dir.join(file);
    std::fs::write(&path, change(std::fs::read_to_string(&path).unwrap())).unwrap();
}

#[test]
fn imported_helpers_resume_twice_and_tail_enter_through_a_dependency_chain() {
    let dir = fixture();
    let out = invoke(dir.path(), "verify", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(format_output(&out).contains("7/7 cases passed"));
    let out = invoke(dir.path(), "run", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(format_output(&out).contains("total = 44"));
}

#[test]
fn a_parent_process_can_stub_requests_from_its_imported_helpers() {
    let dir = fixture();
    edit(dir.path(), "main.av", |s| {
        format!(
            "{s}\nfn numbered(path: BranchPath, index: Int, peer: Int) -> Int\n    peer * 2\n\nverify combined\n    given answer: Pool.claim = [numbered]\n    combined(2) => 44\n\nverify delegated\n    given answer: Pool.claim = [numbered]\n    delegated(2) => 16\n"
        )
    });
    let out = invoke(dir.path(), "verify", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(format_output(&out).contains("9/9 cases passed"));
}

#[test]
fn an_imported_helper_still_requires_its_effects_at_the_source_call() {
    let dir = fixture();
    edit(dir.path(), "main.av", |s| {
        s.replacen("! [Pool.claim, yield]", "! [yield]", 1)
    });
    let out = invoke(dir.path(), "check", &[]);
    assert!(!out.status.success());
    let text = format_output(&out);
    assert!(
        text.contains("Looper.loop") && text.contains("does not declare it"),
        "{text}"
    );
}

#[test]
fn hidden_helpers_and_transitive_modules_do_not_become_visible() {
    for hide_module in [false, true] {
        let dir = fixture();
        if hide_module {
            edit(dir.path(), "main.av", |s| {
                s.replace("[Looper, Middle,", "[Middle,")
            });
        } else {
            edit(dir.path(), "looper.av", |s| {
                format!(
                    "{}\nfn visible() -> Int\n    0\n",
                    s.replace("exposes [loop]", "exposes [visible]")
                )
            });
        }
        let out = invoke(dir.path(), "check", &[]);
        assert!(!out.status.success(), "{}", format_output(&out));
        let text = format_output(&out);
        assert!(
            text.contains("Looper") && !text.contains("internal"),
            "{text}"
        );
    }
}

#[test]
fn imported_source_helpers_cannot_escape_as_function_arguments() {
    let dir = fixture();
    edit(dir.path(), "main.av", |s| {
        format!(
            "{s}\nfn apply(f: Fn(Int, Int) -> Int ! [Pool.claim, yield]) -> Int\n    ! [Pool.claim, yield]\n    f(2, 10)\n\nfn escaping() -> Int\n    ! [Pool.claim, yield]\n    apply(Looper.loop)\n"
        )
    });
    let out = invoke(dir.path(), "check", &[]);
    assert!(!out.status.success(), "{}", format_output(&out));
    assert!(
        format_output(&out).contains("cannot be passed as a function value"),
        "{}",
        format_output(&out)
    );
}

#[test]
fn a_module_cycle_is_reported_before_composing_recursive_protocols() {
    let dir = fixture();
    edit(dir.path(), "looper.av", |s| {
        s.replace("[Pool, Pooled]", "[Middle, Pool, Pooled]")
    });
    let out = invoke(dir.path(), "run", &[]);
    assert!(!out.status.success());
    let text = format_output(&out);
    assert!(
        text.to_lowercase().contains("circular") || text.to_lowercase().contains("cycle"),
        "{text}"
    );
}

#[test]
fn nominal_parameters_and_results_keep_their_library_identity() {
    let dir = fixture();
    edit(dir.path(), "looper.av", |s| {
        format!(
            "{}\nrecord Ticket\n    value: Int\n\nfn request(ticket: Ticket) -> Ticket\n    ! [Pool.claim, yield]\n    answer = Pool.claim(ticket.value)\n    Ticket(value = answer)\n",
            s.replace("exposes [loop]", "exposes [loop, Ticket, request]")
        )
    });
    edit(dir.path(), "main.av", |s| {
        format!(
            "{s}\nfn relay(ticket: Looper.Ticket) -> Looper.Ticket\n    ! [Pool.claim, yield]\n    Looper.request(ticket)\n\nfn ticketStub(path: BranchPath, index: Int, id: Int) -> Int\n    id * 3\n\nverify relay\n    given answer: Pool.claim = [ticketStub]\n    relay(Looper.Ticket(value = 7)) => Looper.Ticket(value = 21)\n"
        )
    });
    let out = invoke(dir.path(), "verify", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(format_output(&out).contains("8/8 cases passed"));
}

#[cfg(feature = "wasm")]
#[test]
fn imported_helpers_run_and_verify_on_wasm_gc() {
    let dir = fixture();
    for command in ["run", "verify"] {
        let out = invoke(dir.path(), command, &["--wasm-gc"]);
        assert!(out.status.success(), "{}", format_output(&out));
        let expected = if command == "run" {
            "total = 44"
        } else {
            "7/7 cases passed"
        };
        assert!(format_output(&out).contains(expected));
    }
}

#[test]
fn imported_segments_propagate_in_place_effects_and_share_stub_coordinates() {
    let dir = fixture();
    std::fs::write(dir.path().join("looper.av"), "module Looper\n    intent = \"A request followed by an inline clock read.\"\n    depends [Pool, Pooled]\n    exposes [once]\n\nfn once(id: Int) -> Int\n    ! [Pool.claim, Time.unixMs, yield]\n    answer = Pool.claim(id)\n    answer + Time.unixMs()\n").unwrap();
    std::fs::write(dir.path().join("main.av"), "module Client\n    intent = \"Exercise effects of an imported generated continuation.\"\n    depends [Looper, Pool, Pooled]\n\nfn parent() -> Int\n    ! [Pool.claim, Time.unixMs, yield]\n    value = Looper.once(2)\n    value + 1\n\nfn requestStub(path: BranchPath, index: Int, id: Int) -> Int\n    id + index\n\nfn clockStub(path: BranchPath, index: Int) -> Int\n    100 + index\n\nverify parent\n    given request: Pool.claim = [requestStub]\n    given clock: Time.unixMs = [clockStub]\n    parent() => 104\n").unwrap();
    let out = invoke(dir.path(), "verify", &[]);
    assert!(out.status.success(), "{}", format_output(&out));
    assert!(format_output(&out).contains("1/1 cases passed"));
}

#[test]
fn the_disk_loading_frontend_composes_imports_without_command_preparation() {
    let dir = fixture();
    let base = dir.path().to_str().unwrap();
    let source = std::fs::read_to_string(dir.path().join("main.av")).unwrap();
    let mut items = aver::source::parse_source(&source).unwrap();
    let len = items.len();
    let result = aver::ir::pipeline::front_gate(
        &mut items,
        &aver::ir::TypecheckMode::Full {
            base_dir: Some(base),
        },
        len,
        &aver::config::MarkedCapabilities::for_project_dir(Some(base)),
    );
    assert!(result.errors.is_empty(), "{:?}", result.errors);
}

#[test]
fn lean_proves_the_imported_helpers_first_request_universally() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping Lean check: lake not available");
        return;
    }
    let dir = fixture();
    let output = tempfile::tempdir().unwrap();
    let out = invoke(
        dir.path(),
        "proof",
        &[
            "--backend",
            "lean",
            "-o",
            output.path().to_str().unwrap(),
            "--check",
            "--check-json",
            "--sorry-budget",
            "0",
        ],
    );
    assert!(out.status.success(), "{}", format_output(&out));
    let text = String::from_utf8_lossy(&out.stdout);
    let report: serde_json::Value = serde_json::from_str(
        text.lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .expect("proof report"),
    )
    .unwrap();
    assert_eq!(report["universal_laws"], 1, "{text}");
    assert_eq!(report["bounded_laws"], 0, "{text}");
    assert_eq!(report["sorries"], 0, "{text}");
    assert_eq!(report["build_errors"], 0, "{text}");
}
