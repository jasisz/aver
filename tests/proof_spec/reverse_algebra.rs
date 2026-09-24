use super::*;

const SIGNED: &str = include_str!("../fixtures/source_recursion/signed_frame.av");

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
    if let Some(summary) = super::source_recursion::check(path.to_str().unwrap()) {
        assert_eq!(summary["passed"], false, "{summary}");
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
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["proof", path.to_str().unwrap(), "--check-json", "-o"])
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
    assert!(!output.status.success(), "{summary}");
    assert_eq!(summary["passed"], false, "{summary}");
    assert_eq!(summary["build_errors"], 0);
    assert_eq!(summary["universal"], false);
    assert_eq!(summary["universal_laws"], 0);
}
