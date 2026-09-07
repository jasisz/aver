use super::*;

#[test]
fn citations_remain_available_in_the_final_implication_after_a_reason() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-final-citation");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/law_reason_final_citation.av",
        &dir,
        0,
        &[],
        &[],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0);
    assert_eq!(summary["universal_laws"], 2);
    for step in ["because1", "implication"] {
        assert_eq!(
            summary["obligations"][format!("count.explainedConcatenation.{step}")],
            "universal"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

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

#[test]
fn guarded_countdown_equations_support_citations_and_accumulator_laws() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-guarded-countdown");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/guarded_countdown_digits.av",
        &dir,
        0,
        &[],
        &[],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["build_errors"], 0);
    assert_eq!(summary["universal_laws"], 4);
    assert_eq!(summary["bounded_laws"], 0);
    let manifest: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(dir.join("proof_manifest.json")).unwrap())
            .unwrap();
    for law in manifest["laws"].as_array().unwrap() {
        assert_eq!(law["tier"], "universal", "{law}");
        for axiom in law["axioms"].as_array().unwrap() {
            assert!(
                matches!(
                    axiom.as_str(),
                    Some("propext" | "Classical.choice" | "Quot.sound")
                ),
                "{law}"
            );
        }
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn imported_record_reason_retains_its_owner_when_entry_has_the_same_type_name() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-imported-record-reason");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(
        dir.join("lib.av"),
        r#"module Lib
    exposes [identity]
    intent = "An executable reason comparing local records."
    effects []
record Count
    value: Int
    rest: List<Int>
fn identity(value: Int) -> Int
    value
fn reason(value: Int) -> Bool
    Count(value = identity(value), rest = []) == Count(value = value, rest = [])
verify identity law explained
    given value: Int = [0, 1]
    because reason(value)
    identity(value) => value
"#,
    )
    .unwrap();
    let av = dir.join("entry.av");
    std::fs::write(
        &av,
        r#"module Entry
    depends [Lib]
    intent = "A same-named entry record must not capture the dependency stamp."
    effects []
record Count
    text: String
fn copy(value: Int) -> Int
    Lib.identity(value)
verify copy law copied
    given value: Int = [0, 1]
    using [Lib.identity.explained]
    copy(value) => value
"#,
    )
    .unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        av.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--module-root", dir.to_str().unwrap()],
    );
    assert!(run.status.success(), "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 2);
    assert_eq!(summary["build_errors"], 0);
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn constant_citations_preserve_arguments_and_require_every_premise() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-law-constant-citation");
    let (summary, run) = run_lean_check_json_with_args(
        "tests/fixtures/law_reason_constant_citation.av",
        &dir,
        0,
        &[],
        &[],
    );
    assert!(!run.status.success(), "the missing guard must fail");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(summary["universal_laws"], 2, "{summary}");
    assert_eq!(summary["bounded_laws"], 0, "{summary}");
    for step in ["because1", "implication"] {
        assert_eq!(
            summary["obligations"][format!("product.nonnegative.{step}")],
            "universal",
            "{summary}"
        );
    }
    assert_eq!(
        summary["obligations"]["product.missingFactorGuard.because1"], "failed",
        "{summary}"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn explain_reports_source_requirements_without_changing_proof_credit() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-source-explain-citation");
    let fixture = "tests/fixtures/law_reason_constant_citation.av";
    let (plain, _) = run_lean_check_json_with_args(fixture, &dir, 0, &[], &[]);
    assert!(plain.get("explanations").is_none());
    let counted_source = std::fs::read(dir.join("ConstantCitation.lean")).unwrap();
    let (explained, run) = run_lean_check_json_with_args(fixture, &dir, 0, &[], &["--explain"]);
    assert_eq!(
        counted_source,
        std::fs::read(dir.join("ConstantCitation.lean")).unwrap()
    );
    assert!(!run.status.success());
    for (key, value) in plain.as_object().unwrap() {
        assert_eq!(&explained[key], value, "counted field {key} changed");
    }
    let reports = explained["explanations"].as_object().unwrap();
    assert_eq!(reports.len(), 1, "{explained}");
    let report = &reports["product.missingFactorGuard.because1"];
    assert_eq!(report["file"], fixture);
    assert_eq!(report["line"], 39);
    assert_eq!(report["goal"], "orderedProducts(0, a, b)");
    assert_eq!(report["assumptions"][0]["expression"], "a >= 0");
    assert_eq!(report["assumptions"].as_array().unwrap().len(), 1);
    assert_eq!(report["citations"][0]["status"], "universal");
    assert_eq!(
        report["citations"][0]["requires"],
        serde_json::json!(["0 <= a", "b >= 0"])
    );
    let attempt = &report["citation_attempts"][0];
    assert_eq!(
        attempt["phase"], "diagnostic_direct_application",
        "{report}"
    );
    assert_eq!(attempt["law"], "orderedProducts.monotone", "{report}");
    assert_eq!(attempt["outcome"], "matched", "{report}");
    assert_eq!(attempt["law_status"], "universal", "{report}");
    assert_eq!(attempt["established_dependencies"], true, "{report}");
    assert_eq!(attempt["premises"][0]["expression"], "0 <= a", "{report}");
    assert_eq!(attempt["premises"][0]["status"], "closed", "{report}");
    assert_eq!(attempt["premises"][0]["closure_audited"], true, "{report}");
    assert!(
        attempt["premises"][0]["proof_axioms"]
            .as_array()
            .unwrap()
            .iter()
            .all(|axiom| matches!(
                axiom.as_str(),
                Some("propext" | "Classical.choice" | "Quot.sound")
            )),
        "{report}"
    );
    assert_eq!(attempt["premises"][1]["expression"], "0 <= b", "{report}");
    assert_eq!(attempt["premises"][1]["status"], "open", "{report}");
    assert!(
        std::fs::read_to_string(dir.join("proof_backend.log"))
            .unwrap()
            .contains("AVER_REASON_OPEN:")
    );

    let run = Command::new(env!("CARGO_BIN_EXE_aver"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["proof", fixture, "--check", "--explain", "-o"])
        .arg(&dir)
        .output()
        .unwrap();
    assert!(!run.status.success());
    let output = format_output(&run);
    assert!(
        output.contains("To prove: orderedProducts(0, a, b)"),
        "{output}"
    );
    assert!(output.contains("requires b >= 0"), "{output}");
    assert!(output.contains("[closed in probe] 0 <= a"), "{output}");
    assert!(output.contains("[open in probe] 0 <= b"), "{output}");
    assert!(output.contains("when a >= 0 [assumed]"), "{output}");
    for technical in ["AVER_REASON_OPEN:", "⊢", ".lean:", "simp only", "case "] {
        assert!(
            !output.contains(technical),
            "raw backend state leaked: {output}"
        );
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn explain_locates_private_imported_steps_and_marks_failed_previous_reasons() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-source-explain-private-import");
    std::fs::create_dir_all(&dir).unwrap();
    let library = dir.join("lib.av");
    std::fs::write(&library, "module Lib\n    exposes [identity]\n    intent = \"Private proof diagnostics.\"\n    effects []\nfn identity(x: Int) -> Int\n    x\nfn secret(x: Int) -> Int\n    x\nverify secret law chain\n    given x: Int = [0]\n    because x > 0\n    because x > 1\n    because x > 2\n    using []\n    secret(x) => x\n").unwrap();
    let entry = dir.join("entry.av");
    std::fs::write(&entry, "module Entry\n    depends [Lib]\n    intent = \"Import a private proof.\"\n    effects []\nfn copy(x: Int) -> Int\n    Lib.identity(x)\n").unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        entry.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--explain", "--module-root", dir.to_str().unwrap()],
    );
    assert!(!run.status.success());
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    let report = &summary["explanations"]["Lib.secret.chain.because2"];
    assert_eq!(report["file"], library.to_str().unwrap(), "{summary}");
    assert_eq!(report["line"], 12);
    assert_eq!(report["goal"], "x > 1");
    assert_eq!(report["assumptions"].as_array().unwrap().len(), 1);
    assert_eq!(report["assumptions"][0]["expression"], "x > 0");
    assert_eq!(report["assumptions"][0]["status"], "failed");
    let _ = std::fs::remove_dir_all(dir);
}
