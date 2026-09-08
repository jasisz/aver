use super::*;

const FIXTURES: &str = "tests/fixtures/dafny_structure";

fn command(fixture: &str, operation: &str) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args([operation, &format!("{FIXTURES}/{fixture}.av")]);
    if fixture.starts_with("callbacks_imported/") {
        command.args(["--module-root", &format!("{FIXTURES}/callbacks_imported")]);
    }
    command
}

fn run(fixture: &str) -> Option<(serde_json::Value, PathBuf)> {
    if Command::new("dafny").arg("--version").output().is_err() {
        return None;
    }
    let dir = temp_output_dir(&format!("aver-structure-{}", fixture.replace('/', "-")));
    let output = command(fixture, "proof")
        .args(["--backend", "dafny", "--check-json", "-o"])
        .arg(&dir)
        .output()
        .expect("run structural proof");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .unwrap_or_else(|| panic!("{fixture}: {}", format_output(&output))),
    )
    .expect("proof summary");
    assert_eq!(
        summary["passed"],
        output.status.success(),
        "{fixture}: {summary}"
    );
    Some((summary, dir))
}

fn assert_no_trust_or_refusal(summary: &serde_json::Value) {
    for field in ["declined", "axioms", "omitted", "timeouts"] {
        assert_eq!(summary[field].as_u64().unwrap_or(0), 0, "{summary}");
    }
}

#[test]
fn dafny_structure_source_laws_are_checked_without_opaque_assumptions() {
    for fixture in [
        "containers",
        "unit_maps",
        "callbacks_positive",
        "callbacks_imported/main",
        "strings/positive",
        "strings/plain",
        "strings/text_positive",
        "propagation_positive",
        "refinement",
        "refinement_update",
        "bytes",
    ] {
        let Some((summary, dir)) = run(fixture) else {
            continue;
        };
        assert_no_trust_or_refusal(&summary);
        assert_eq!(summary["passed"], true, "{fixture}: {summary}");
        assert_eq!(summary["errors"], 0, "{fixture}: {summary}");
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_structure_false_steps_and_invalid_constructors_fail_actual_verification() {
    for fixture in [
        "containers_false",
        "unit_maps_false",
        "callbacks_false",
        "strings/false_reason",
        "strings/text_false",
        "propagation_false_reason",
        "refinement_invalid",
        "refinement_update_invalid",
    ] {
        let Some((summary, dir)) = run(fixture) else {
            continue;
        };
        assert_no_trust_or_refusal(&summary);
        assert_eq!(summary["passed"], false, "{fixture}: {summary}");
        assert!(
            summary["errors"].as_u64().unwrap() > 0,
            "{fixture}: {summary}"
        );
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_structure_missing_semantics_and_hidden_callback_cycles_remain_explicit() {
    for fixture in [
        "strings/unsupported_float",
        "callbacks_unsupported",
        "refinement_opaque",
        "bytes_hex_opaque",
    ] {
        let Some((summary, dir)) = run(fixture) else {
            continue;
        };
        assert_eq!(summary["passed"], false, "{fixture}: {summary}");
        assert!(
            summary["declined"].as_u64().unwrap() > 0,
            "{fixture}: {summary}"
        );
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_structure_unsupported_callback_models_are_declined_before_the_backend_checker() {
    // Dafny also rejects these function-value shapes during resolution.
    // Inspect export refusals directly; a parse error is not a failed proof.
    for (fixture, module, reason) in [
        (
            "callbacks_recursive",
            "CallbacksRecursive",
            "closes a hidden higher-order recursion cycle",
        ),
        (
            "callbacks_container",
            "CallbackContainerIdentity",
            "callback containers have no source-equivalent equality model",
        ),
    ] {
        let dir = temp_output_dir("aver-structure-callback-refusal");
        let output = command(fixture, "proof")
            .args(["--backend", "dafny", "-o"])
            .arg(&dir)
            .output()
            .expect("export callback control");
        assert!(output.status.success(), "{}", format_output(&output));
        let source = std::fs::read_to_string(dir.join(format!("{module}.dfy"))).unwrap();
        assert!(source.contains(reason), "{source}");
        assert!(!source.contains("// aver:dafny-law "), "{source}");
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_structure_vm_preserves_source_samples_even_when_false_laws_fool_the_samples() {
    // The deliberate callback cycle is export-only and must never execute.
    for fixture in [
        "containers",
        "unit_maps",
        "callbacks_positive",
        "callbacks_imported/main",
        "strings/positive",
        "strings/plain",
        "strings/text_positive",
        "strings/text_samples",
        "propagation_positive",
        "refinement",
        "refinement_update",
        "bytes",
        "containers_false",
        "unit_maps_false",
        "callbacks_false",
        "strings/false_reason",
        "strings/text_false",
        "propagation_false_reason",
        "refinement_invalid",
        "refinement_update_invalid",
    ] {
        let output = command(fixture, "verify")
            .output()
            .expect("run source samples");
        assert!(
            output.status.success(),
            "{fixture}: {}",
            format_output(&output)
        );
    }
}
