use super::*;

const ALGEBRA: &str = include_str!("../fixtures/source_recursion/reverse_algebra.av");
const SIGNED: &str = include_str!("../fixtures/source_recursion/signed_frame.av");

#[test]
fn reverse_algebra_and_acyclic_frames_pass_both_checkers() {
    for (label, source) in [
        ("algebra", ALGEBRA.to_string()),
        ("signed", SIGNED.to_string()),
        (
            "renamed",
            SIGNED
                .replace("8", "10")
                .replace("4", "5")
                .replace("encode", "place")
                .replace("read", "consume"),
        ),
    ] {
        let dir = temp_output_dir(&format!("aver-reverse-{label}"));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("main.av");
        std::fs::write(&path, source).unwrap();
        for backend in ["dafny", "lean"] {
            let Some(summary) = super::source_recursion::check(path.to_str().unwrap(), backend)
            else {
                continue;
            };
            assert_eq!(summary["passed"], true, "{label}/{backend}: {summary}");
            if backend == "lean" {
                assert_eq!(summary["universal_laws"], 3);
            }
        }
    }
}

#[test]
fn reverse_algebra_rejects_order_loss_hidden_by_symmetric_samples() {
    let source = r#"module ReverseAlgebra
fn flip(values: List<Bool>) -> List<Bool>
    List.reverse(values)
verify flip law preservesOrder
    given values: List<Bool> = [[], [true], [false, true, false]]
    flip(values) => values
"#;
    let dir = temp_output_dir("aver-reverse-false-order");
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("main.av");
    std::fs::write(&path, source).unwrap();
    let vm = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(vm.status.success(), "{}", format_output(&vm));
    for backend in ["dafny", "lean"] {
        let Some(summary) = super::source_recursion::check(path.to_str().unwrap(), backend) else {
            continue;
        };
        assert_eq!(summary["passed"], false, "{backend}: {summary}");
        if backend == "dafny" {
            assert!(summary["errors"].as_u64().unwrap() > 0);
        }
    }
}

#[test]
fn acyclic_reverse_frame_still_requires_the_original_domain_guard() {
    // Keep the false law isolated from optional quantified supplier facts.
    // Every retained VM sample satisfies the removed premise.
    let definitions = SIGNED.split_once("verify read law suffix").unwrap().0;
    let rest = SIGNED.split_once("fn signed").unwrap().1;
    let source = format!("{definitions}fn signed{rest}").replace("    when top >= 0\n", "");
    let dir = temp_output_dir("aver-reverse-missing-guard");
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("main.av");
    std::fs::write(&path, source).unwrap();
    let vm = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(vm.status.success(), "{}", format_output(&vm));
    for backend in ["dafny", "lean"] {
        let checker = if backend == "lean" { "lake" } else { "dafny" };
        if Command::new(checker).arg("--version").output().is_err() {
            continue;
        }
        let output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args([
                "proof",
                path.to_str().unwrap(),
                "--backend",
                backend,
                "--check-json",
                "-o",
            ])
            .arg(temp_output_dir("aver-reverse-guard-check"))
            .output()
            .unwrap();
        let stdout = String::from_utf8_lossy(&output.stdout);
        let summary: serde_json::Value = serde_json::from_str(
            stdout
                .lines()
                .rev()
                .find(|line| line.starts_with('{'))
                .unwrap_or_else(|| panic!("{}", format_output(&output))),
        )
        .unwrap();
        assert!(!output.status.success(), "{backend}: {summary}");
        assert_eq!(summary["passed"], false, "{backend}: {summary}");
        if backend == "dafny" {
            assert_eq!(summary["axioms"], 0);
            assert_eq!(summary["omitted"], 0);
            // The composed arithmetic counterexample may exhaust SMT search.
            // It must still refuse all universal credit. The separate order
            // control requires a concrete error with zero timeouts.
            assert!(
                summary["errors"].as_u64().unwrap() + summary["timeouts"].as_u64().unwrap() > 0
            );
        } else {
            assert_eq!(summary["build_errors"], 0);
            assert_eq!(summary["universal"], false);
            assert_eq!(summary["universal_laws"], 0);
        }
    }
}
