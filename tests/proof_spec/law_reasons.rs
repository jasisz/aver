use super::*;

#[test]
fn reasons_are_audited_separately_and_cannot_hide_behind_an_easy_goal() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-reasons");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/law_reasons.av",
        &dir,
        0,
        &[],
        &["--explain"],
    );
    assert!(
        !run.status.success(),
        "a false explanation must fail the normal gate"
    );
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(
        summary["universal_laws"], 2,
        "obligations must not inflate the law count"
    );
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    let find = |kind: &str, id: &str| {
        manifest[kind]
            .as_array()
            .unwrap()
            .iter()
            .find(|r| r["law"] == id)
            .unwrap()
    };
    let bad = "identity.badReasonCannotHideBehindEasyGoal";
    assert_eq!(find("laws", bad)["tier"], "failed");
    assert_eq!(
        find("obligations", &format!("{bad}.because1"))["tier"],
        "failed"
    );
    assert!(
        find("obligations", &format!("{bad}.because1"))["open_goal"]
            .as_str()
            .is_some_and(|s| s.contains('⊢'))
    );
    assert_eq!(
        find("obligations", &format!("{bad}.implication"))["tier"],
        "universal"
    );
    for step in ["because1", "because2", "implication"] {
        assert_eq!(
            find("obligations", &format!("identity.positiveChain.{step}"))["tier"],
            "universal"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn imported_citations_respect_visibility_and_prove_through_a_module_boundary() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-reasons-imports");
    std::fs::create_dir_all(&dir).unwrap();
    let lib = "module Lib\n    exposes [qrev, rev]\n    intent = \"Reversal lemma.\"\n    effects []\nfn qrev(xs: List<Int>, acc: List<Int>) -> List<Int>\n    match xs\n        [] -> acc\n        [x, ..rest] -> qrev(rest, List.prepend(x, acc))\nfn rev(xs: List<Int>) -> List<Int>\n    match xs\n        [] -> []\n        [x, ..rest] -> List.concat(rev(rest), [x])\nverify qrev law specification\n    given xs: List<Int> = [[], [1, 2]]\n    given acc: List<Int> = [[], [3]]\n    qrev(xs, acc) => List.concat(rev(xs), acc)\n";
    let source = "module Consumer\n    depends [Lib]\n    intent = \"Use an exposed law in an explanation.\"\n    effects []\nfn reverse(xs: List<Int>) -> List<Int>\n    Lib.rev(xs)\nverify reverse law explained\n    given xs: List<Int> = [[], [1, 2]]\n    because Lib.qrev(xs, []) == Lib.rev(xs)\n    using [Lib.qrev.specification]\n    reverse(xs) => Lib.qrev(xs, [])\n";
    std::fs::write(dir.join("Lib.av"), lib).unwrap();
    let file = dir.join("Consumer.av");
    std::fs::write(&file, source).unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        file.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--module-root", dir.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(
        summary["obligations"]["reverse.explained.because1"],
        "universal"
    );
    // Omitting `using` must reuse the existing cross-module automatic pool.
    std::fs::write(
        &file,
        source.replace("    using [Lib.qrev.specification]\n", ""),
    )
    .unwrap();
    let (automatic, run) = run_lean_check_json_with_args(
        file.to_str().unwrap(),
        &dir.join("automatic"),
        0,
        &[],
        &["--module-root", dir.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(automatic["obligations"], summary["obligations"]);
    std::fs::write(&file, source).unwrap();
    std::fs::write(
        dir.join("Lib.av"),
        lib.replace("exposes [qrev, rev]", "exposes [rev]"),
    )
    .unwrap();
    let check = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "check",
            file.to_str().unwrap(),
            "--module-root",
            dir.to_str().unwrap(),
        ])
        .output()
        .unwrap();
    assert!(!check.status.success());
    assert!(
        format_output(&check).contains("unknown or unexposed law 'Lib.qrev.specification'"),
        "{}",
        format_output(&check)
    );
    std::fs::write(
        dir.join("Lib.av"),
        lib.replace(
            "    qrev(xs, acc) =>",
            "    using [qrev.missing]\n    qrev(xs, acc) =>",
        ),
    )
    .unwrap();
    let check = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "check",
            file.to_str().unwrap(),
            "--module-root",
            dir.to_str().unwrap(),
        ])
        .output()
        .unwrap();
    assert!(!check.status.success());
    assert!(
        format_output(&check).contains("unknown or unexposed law 'qrev.missing'"),
        "{}",
        format_output(&check)
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn formatter_keeps_ordered_explanations() {
    let source = std::fs::read_to_string("tests/fixtures/law_reasons.av").unwrap();
    let (formatted, _) = aver::format::try_format_source(&source).unwrap();
    assert!(formatted.contains("because value >= 1\n    because value + 1 > 1\n    using []"));
    assert!(aver::source::parse_source(&formatted).is_ok());
    assert_eq!(
        aver::format::try_format_source(&formatted).unwrap().0,
        formatted
    );
}

#[cfg(feature = "wasm")]
#[test]
fn wasm_checks_declared_explanations_even_when_the_claim_is_true() {
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", "tests/fixtures/law_reasons_edges.av", "--wasm-gc"])
        .output()
        .unwrap();
    assert!(!run.status.success());
    let output = format_output(&run);
    assert!(output.contains("falseReason.because1"), "{output}");
    assert!(
        output.contains("an explanation is an obligation"),
        "{output}"
    );
}

#[test]
fn dafny_declines_explanations_without_dropping_them_silently() {
    if Command::new("dafny").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-reasons-dafny");
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args([
            "proof",
            "tests/fixtures/law_reasons.av",
            "--backend",
            "dafny",
            "--check-json",
            "-o",
            dir.to_str().unwrap(),
        ])
        .output()
        .unwrap();
    assert!(!run.status.success());
    let stdout = String::from_utf8_lossy(&run.stdout);
    let json = stdout
        .lines()
        .rev()
        .find(|l| l.starts_with('{'))
        .unwrap_or_else(|| panic!("{}", format_output(&run)));
    let summary: serde_json::Value = serde_json::from_str(json).unwrap();
    assert_eq!(summary["declined"], 3, "{summary}");
    assert_eq!(summary["errors"], 0, "{summary}");
    assert!(
        summary["declined_claims"]
            .as_array()
            .unwrap()
            .iter()
            .all(|r| r["reason"]
                .as_str()
                .unwrap()
                .contains("require the Lean backend"))
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn reasons_close_computed_list_facts_with_explicit_forward_citations() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-reasons-digits");
    std::fs::create_dir_all(&dir).unwrap();
    let source = std::fs::read_to_string("tests/fixtures/law_reasons_digits.av").unwrap();
    let start = source.find("verify rIsMinimalNumber law").unwrap();
    let end = start + source[start..].find("\n\n").unwrap();
    let block = &source[start..end];
    let without = format!("{}{}", &source[..start], &source[end..]);
    let first_function = without.find("fn ").unwrap();
    let reordered = format!(
        "{}{}\n\n{}",
        &without[..first_function],
        block,
        &without[first_function..]
    );
    let file = dir.join("source.av");
    std::fs::write(&file, reordered).unwrap();
    let (summary, run) = run_lean_check_json(file.to_str().unwrap(), &dir.join("lean"), 0, &[]);
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 6);
    assert_eq!(summary["sorries"], 0);
    assert_eq!(
        summary["obligations"]["rIsMinimalNumber.acceptsWhatFromNumberWrites.because1"],
        "universal"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn hostile_checks_explanations_under_the_original_guard() {
    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .args(["verify", "tests/fixtures/law_reasons.av", "--hostile"])
        .output()
        .unwrap();
    assert!(!run.status.success());
    let output = format_output(&run);
    assert!(output.contains("badReasonCannotHideBehindEasyGoal.because1"));
    assert!(
        output.contains("an explanation is an obligation"),
        "{output}"
    );
    assert!(
        !output.contains("Either add `when"),
        "a bad explanation is not a reason to weaken the law"
    );
}

#[test]
fn true_false_restatement_and_tainted_citation_do_not_launder_credit() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-reasons-edges");
    let (summary, run) = run_lean_check_json("tests/fixtures/law_reasons_edges.av", &dir, 0, &[]);
    assert!(!run.status.success());
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 0);
    assert_eq!(
        summary["obligations"]["identity.trueReason.because1"],
        "universal"
    );
    assert_eq!(
        summary["obligations"]["identity.trueReason.implication"],
        "failed"
    );
    assert_eq!(
        summary["obligations"]["identity.falseReason.because1"],
        "failed"
    );
    assert_eq!(
        summary["obligations"]["identity.restatedGoal.because1"],
        "failed"
    );
    assert_eq!(
        summary["obligations"]["identity.restatedGoal.implication"],
        "universal"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn explanations_require_bool_purity_and_acyclic_known_dependencies() {
    let errors = |source: &str| {
        let items = aver::source::parse_source(source).unwrap();
        aver::types::checker::run_type_check(&items)
            .into_iter()
            .map(|e| e.message)
            .collect::<Vec<_>>()
            .join("\n")
    };
    let header = "fn f(x: Int) -> Int\n    x\n";
    assert!(
        errors(&format!(
            "{header}verify f law bad\n    given x: Int = [0]\n    because x\n    f(x) => x\n"
        ))
        .contains("because must have type Bool")
    );
    assert!(errors(&format!("{header}verify f law bad\n    given x: Int = [0]\n    using [f.missing]\n    f(x) => x\n")).contains("unknown or unexposed"));
    assert!(errors(&format!("{header}verify f law a\n    given x: Int = [0]\n    using [f.b]\n    f(x) => x\nverify f law b\n    given x: Int = [0]\n    using [f.a]\n    f(x) => x\n")).contains("cyclic 'using'"));
    let effectful = "fn noisy(x: Int) -> Bool\n    ! [Console.print]\n    Console.print(\"effect\")\n    true\n";
    assert!(!errors(&format!("{header}{effectful}verify f law bad\n    given x: Int = [0]\n    because noisy(x)\n    f(x) => x\n")).is_empty());
}
