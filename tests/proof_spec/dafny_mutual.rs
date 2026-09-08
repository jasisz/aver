use super::*;

const FIXTURES: &str = "tests/fixtures/dafny_guidance_mutual";

fn command(fixture: &str, operation: &str) -> Command {
    let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
    command
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args([operation, &format!("{FIXTURES}/{fixture}.av")]);
    if fixture.starts_with("imported/") {
        command.args(["--module-root", &format!("{FIXTURES}/imported")]);
    }
    command
}

fn run(fixture: &str, backend: &str) -> Option<(serde_json::Value, PathBuf)> {
    let checker = if backend == "lean" { "lake" } else { "dafny" };
    if Command::new(checker).arg("--version").output().is_err() {
        return None;
    }
    let dir = temp_output_dir(&format!(
        "aver-mutual-{}-{backend}",
        fixture.replace('/', "-")
    ));
    let output = command(fixture, "proof")
        .args(["--backend", backend, "--check-json", "-o"])
        .arg(&dir)
        .output()
        .expect("run mutual proof");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .unwrap_or_else(|| panic!("{fixture}/{backend}: {}", format_output(&output))),
    )
    .expect("parse proof summary");
    assert_eq!(summary["passed"], output.status.success(), "{summary}");
    Some((summary, dir))
}

fn assert_checked(summary: &serde_json::Value, backend: &str) {
    for field in if backend == "lean" {
        &["build_errors"][..]
    } else {
        &["declined", "axioms", "omitted", "timeouts"][..]
    } {
        assert_eq!(summary[field].as_u64().unwrap_or(0), 0, "{summary}");
    }
}

fn generated(dir: &std::path::Path) -> String {
    std::fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "dfy"))
        .map(|path| std::fs::read_to_string(path).unwrap())
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn dafny_mutual_source_transitions_verify_with_only_the_consumed_input_in_the_measure() {
    for fixture in ["positive", "imported/main"] {
        let Some((summary, dir)) = run(fixture, "dafny") else {
            continue;
        };
        assert_checked(&summary, "dafny");
        assert_eq!(summary["passed"], true, "{fixture}: {summary}");
        assert_eq!(summary["errors"], 0, "{summary}");
        let source = generated(&dir);
        for (name, driver) in [("collect", "items"), ("accept", "rest")] {
            assert!(!source.contains(&format!("{name}__fuel")), "{source}");
            let declaration = source.split(&format!("function {name}(")).nth(1).unwrap();
            let header = declaration.split('{').next().unwrap();
            let measure = header
                .lines()
                .find(|line| line.trim().starts_with("decreases "))
                .unwrap();
            assert!(
                measure.contains(&format!("|{driver}|")),
                "{name}: {measure}"
            );
            assert!(!measure.contains("|acc|"), "{name}: {measure}");
        }
        for (marker, count) in [("// aver:dafny-law ", 2), ("// aver:dafny-obligation ", 4)] {
            assert_eq!(
                source
                    .lines()
                    .filter(|line| line.trim().starts_with(marker))
                    .count(),
                count,
                "{source}"
            );
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_mutual_false_intermediate_is_not_excused_by_a_true_final_goal() {
    for backend in ["lean", "dafny"] {
        let Some((summary, dir)) = run("false_reason", backend) else {
            continue;
        };
        assert_checked(&summary, backend);
        assert_eq!(summary["passed"], false, "{backend}: {summary}");
        assert!(
            summary[if backend == "lean" {
                "sorries"
            } else {
                "errors"
            }]
            .as_u64()
            .unwrap()
                > 0,
            "{summary}"
        );
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn dafny_mutual_nonshrinking_input_stays_declined() {
    let Some((summary, dir)) = run("nonshrinking", "dafny") else {
        return;
    };
    assert_eq!(summary["passed"], false, "{summary}");
    assert!(summary["declined"].as_u64().unwrap() > 0, "{summary}");
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn dafny_mutual_finite_samples_preserve_accumulator_behavior() {
    // Never run the deliberately nonterminating nonshrinking fixture.
    for fixture in ["positive", "imported/main", "false_reason"] {
        let output = command(fixture, "verify")
            .output()
            .expect("run mutual samples");
        assert!(
            output.status.success(),
            "{fixture}: {}",
            format_output(&output)
        );
    }
}
