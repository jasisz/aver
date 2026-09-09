use super::*;

const ALGEBRA: &str = include_str!("../fixtures/source_recursion/reverse_algebra.av");
const SIGNED: &str = include_str!("../fixtures/source_recursion/signed_frame.av");

// The acyclic-constructor strategy has been withdrawn. The Aver fixtures
// remain diagnostic inputs in tools/proof_search_matrix.py; they are not
// counted as universally proved by these independent library checks.
#[test]
fn reverse_library_checks_general_equations_and_rejects_false_order() {
    if Command::new("dafny").arg("--version").output().is_err() {
        return;
    }
    let library = include_str!("../../src/codegen/dafny/prelude/list.dfy");
    for (label, claim, body, expected) in [
        (
            "positive",
            "ListReverse(ListReverse(xs + ys)) == xs + ys",
            "ListReverseInvolution(xs + ys);",
            true,
        ),
        ("negative", "ListReverse(xs) == xs", "", false),
    ] {
        let dir = temp_output_dir("aver-reverse-library");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("library.dfy");
        std::fs::write(&path, format!(
            "datatype Option<T> = None | Some(value: T)\n{library}\nlemma {{:induction false}} Check(xs: seq<bool>, ys: seq<bool>)\n ensures {claim}\n{{ {body} }}\n"
        )).unwrap();
        let output = Command::new("dafny")
            .arg("verify")
            .arg(&path)
            .args(["--verification-time-limit", "5"])
            .output()
            .unwrap();
        let text = format_output(&output);
        assert_eq!(output.status.success(), expected, "{label}: {text}");
        assert!(text.contains("Dafny program verifier finished"), "{text}");
        assert!(
            !text.contains("time out") && !text.contains("timed out"),
            "{text}"
        );
        if expected {
            assert!(text.contains("0 errors"), "{text}");
        } else {
            assert!(text.contains("1 error"), "{text}");
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}

#[test]
fn reversal_does_not_install_a_constructor_specific_induction_policy() {
    let dir = temp_output_dir("aver-reverse-no-policy");
    std::fs::create_dir_all(&dir).unwrap();
    for (label, source) in [("algebra", ALGEBRA), ("signed", SIGNED)] {
        let path = dir.join(format!("{label}.av"));
        std::fs::write(&path, source).unwrap();
        let out = dir.join(label);
        let output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .args(["proof", path.to_str().unwrap(), "--backend", "dafny", "-o"])
            .arg(&out)
            .output()
            .unwrap();
        assert!(output.status.success(), "{}", format_output(&output));
        // The library contains definitions/calls of its own. No source theorem
        // receives the retired universal reversal pool or its explicit trigger.
        for entry in std::fs::read_dir(out).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "dfy") {
                let text = std::fs::read_to_string(path).unwrap();
                assert!(!text.contains("{:trigger ListReverse("), "{text}");
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
