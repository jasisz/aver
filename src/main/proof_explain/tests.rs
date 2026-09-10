use super::*;
use aver::ast::TopLevel;

#[test]
fn residual_candidates_exclude_marked_proof_steps_without_name_guessing() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(
        dir.path().join("lakefile.lean"),
        "lean_lib Entry where\n  roots := #[`Entry]\n",
    )
    .unwrap();
    std::fs::write(dir.path().join("Entry.lean"), "namespace Entry\n-- aver:law-class f_law_plain universal f.plain\ntheorem f_law_plain : True := by trivial\n-- aver:law-obligation ordinary_law_step universal f.reason.because1\ntheorem ordinary_law_step : True := by trivial\n-- aver:law-class __aver_reason_legitimate_law_name universal f.legitimate\ntheorem __aver_reason_legitimate_law_name : True := by trivial\nnamespace Helpers\n-- aver:law-obligation f_law_plain universal nested.reason.because1\ntheorem f_law_plain : True := by trivial\n-- aver:law-class __aver_reason_legitimate_law_name universal nested.name\ntheorem __aver_reason_legitimate_law_name : True := by trivial\nend Helpers\nend Entry\n").unwrap();
    let actual = super::super::emitted_main_law_theorems(dir.path().to_str().unwrap());
    assert_eq!(
        actual,
        vec![
            ("f.plain".into(), "f_law_plain".into()),
            (
                "f.legitimate".into(),
                "__aver_reason_legitimate_law_name".into()
            )
        ]
    );
}

fn catalog(source: &str, scope: Option<&str>, file: &str) -> Catalog {
    let mut catalog = Catalog::default();
    for item in aver::source::parse_source(source).unwrap() {
        if let TopLevel::Verify(block) = item {
            catalog.add(&block, scope, file);
        }
    }
    catalog
}

#[test]
fn helper_suggestions_require_a_source_law_without_explicit_reasons() {
    let source = "fn f(x: Int) -> Int\n    x\nverify f law plain\n    given x: Int = [1]\n    f(x) => x\nverify f law steps\n    given x: Int = [1]\n    because x > 0\n    f(x) => x\n";
    let catalog = catalog(source, None, "source.av");
    assert!(catalog.accepts_residual_suggestion("f.plain"));
    for identity in [
        "f.steps",
        "f.steps.because1",
        "f.steps.implication",
        "__aver_reason_f_law_steps_because1",
        "unknown_law",
    ] {
        assert!(!catalog.accepts_residual_suggestion(identity), "{identity}");
    }
}

#[test]
fn unavailable_citations_are_not_reported_as_missing_mathematical_premises() {
    let source = "fn f(x: Int) -> Int\n    x\nverify f law chain\n    given x: Int = [1]\n    because x == x\n    using []\n    f(x) => x\n";
    let catalog = catalog(source, None, "source.av");
    let reports = collect(
        &catalog,
        None,
        &["f.chain.because1".into()],
        "unused",
        "info: Entry.lean:8:2: AVER_REASON_OPEN:f.chain.because1:dependency has no available theorem\n",
    );
    let report = &reports["f.chain.because1"];
    assert_eq!(report["status"], "citation_unavailable");
    assert_eq!(report["goal"], "x == x");
    assert!(
        report["next"]
            .as_str()
            .unwrap()
            .contains("already universally checked")
    );
    assert!(
        !report["next"]
            .as_str()
            .unwrap()
            .contains("missing intermediate fact")
    );
}

#[test]
fn explanations_show_only_earlier_reasons_and_instantiated_requirements() {
    let source = include_str!("../../../tests/fixtures/law_reason_constant_citation.av");
    let catalog = catalog(source, None, "source.av");
    let reports = collect(
        &catalog,
        None,
        &["product.missingFactorGuard.because1".into()],
        "unused",
        "",
    );
    let report = &reports["product.missingFactorGuard.because1"];
    assert_eq!(report["assumptions"].as_array().unwrap().len(), 1);
    assert_eq!(report["assumptions"][0]["expression"], "a >= 0");
    assert_eq!(report["goal"], "orderedProducts(0, a, b)");
    let citation = &report["citations"][0];
    assert_eq!(citation["instantiated"], true);
    assert_eq!(citation["requires"], json!(["0 <= a", "b >= 0"]));
    assert_eq!(citation["bindings"]["factor"], "b");
    assert_eq!(citation["status"], "not_checked");
}

#[test]
fn imported_claims_keep_their_owner_and_source_location() {
    let source = "fn f(x: Int) -> Int\n    x\nverify f law chain\n    given x: Int = [1]\n    because x > 0\n    because x > 1\n    because x > 2\n    using []\n    f(x) => x\n";
    let catalog = catalog(source, Some("Domain.Lib"), "domain/lib.av");
    let reports = collect(
        &catalog,
        None,
        &["Domain.Lib.f.chain.because2".into()],
        "unused",
        "",
    );
    let report = &reports["Domain.Lib.f.chain.because2"];
    assert_eq!(report["file"], "domain/lib.av");
    assert_eq!(report["line"], 6);
    assert_eq!(report["goal"], "x > 1");
    assert_eq!(report["assumptions"].as_array().unwrap().len(), 1);
    assert_eq!(report["assumptions"][0]["expression"], "x > 0");
    assert_eq!(report["assumptions"][0]["status"], "not_checked");
}

#[test]
fn checker_limits_keep_source_context_without_a_successful_audit() {
    let source = "fn f(x: Int) -> Int\n    x\nverify f law chain\n    given x: Int = [1]\n    because x > 0\n    using []\n    f(x) => x\n";
    let catalog = catalog(source, None, "source.av");
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("Entry.lean"), "-- aver:law-obligation reason universal f.chain.because1\ntheorem reason : True := by\n  trivial\n").unwrap();
    let log = "error: ././Entry.lean:3:2: Tactic failed\n(deterministic) timeout at whnf, maximum number of heartbeats reached\nerror: Lean exited with code 1\n";
    let reports = collect(&catalog, None, &[], dir.path().to_str().unwrap(), log);
    assert_eq!(reports.len(), 1);
    assert_eq!(reports["f.chain.because1"]["status"], "checker_limit");
    assert_eq!(reports["f.chain.because1"]["goal"], "x > 0");
    assert_eq!(reports["f.chain.because1"]["file"], "source.av");
}

#[test]
fn expression_rendering_preserves_grouping() {
    let items = aver::source::parse_source("fn f(x: Int) -> Int\n    x\nverify f law grouping\n    given x: Int = [1]\n    because (x + 1) * x > 0\n    f(x) => x\n").unwrap();
    let TopLevel::Verify(block) = &items[1] else {
        panic!()
    };
    let aver::ast::VerifyKind::Law(law) = &block.kind else {
        panic!()
    };
    assert_eq!(source::expression(&law.because[0]), "((x + 1) * x) > 0");
}

#[test]
fn expression_rendering_keeps_dotted_names_and_escaped_literals_readable() {
    let source = r#"fn f(x: Int) -> Int
    x
verify f law display
    given x: Int = [1]
    because Domain.Fprep.value(x).exp > x.exp + 1
    because String.len("quote: \" and slash: \\") > 0
    f(x) => x
"#;
    let catalog = catalog(source, None, "source.av");
    let reports = collect(
        &catalog,
        None,
        &["f.display.because1".into(), "f.display.because2".into()],
        "unused",
        "",
    );
    assert_eq!(
        reports["f.display.because1"]["goal"],
        "Domain.Fprep.value(x).exp > (x.exp + 1)"
    );
    assert_eq!(
        reports["f.display.because2"]["goal"],
        r#"String.len("quote: \" and slash: \\") > 0"#
    );
}

#[test]
fn unrelated_checker_errors_are_not_hidden_by_an_open_source_step() {
    let source = "fn f(x: Int) -> Int\n    x\nverify f law chain\n    given x: Int = [1]\n    because x > 0\n    f(x) => x\n";
    let catalog = catalog(source, None, "source.av");
    let reports = collect(
        &catalog,
        None,
        &["f.chain.because1".into()],
        "unused",
        "error: required dependency unavailable\n",
    );
    assert_eq!(reports.len(), 2);
    assert_eq!(reports["<proof checker>"]["status"], "checker_error");
    assert_eq!(reports["f.chain.because1"]["status"], "unproved");
}

#[test]
fn substitution_does_not_capture_names_in_match_arms() {
    let source = "fn p(x: Int) -> Bool\n    x > 0\nverify p law positive\n    given x: Int = [1]\n    when match Option.Some(1)\n        Option.Some(y) -> x > y\n        Option.None -> false\n    p(x) holds\nverify p law useIt\n    given y: Int = [1]\n    because p(y)\n    using [p.positive]\n    p(y) holds\n";
    let catalog = catalog(source, None, "source.av");
    let reports = collect(&catalog, None, &["p.useIt.because1".into()], "unused", "");
    let citation = &reports["p.useIt.because1"]["citations"][0];
    assert_eq!(citation["instantiated"], false);
    assert_eq!(citation["bindings"], json!({}));
    assert!(citation["requires"][0].as_str().unwrap().contains("x > y"));
}

#[test]
fn dafny_imported_steps_keep_source_context_and_no_partial_credit() {
    let source = "fn f(x: Int) -> Int\n    x\nverify f law chain\n    given x: Int = [2]\n    when x >= 0\n    because x > 0\n    because x > 1\n    using []\n    f(x) => x\n";
    let catalog = catalog(source, Some("Domain.Lib"), "domain/lib.av");
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("Domain")).unwrap();
    std::fs::write(dir.path().join("Domain/Lib.dfy"), "// aver:dafny-obligation first Domain.Lib.f.chain.because1\nlemma first()\n{ assert false; }\n// aver:dafny-obligation second Domain.Lib.f.chain.because2\nlemma second()\n{ assert false; }\n// aver:dafny-law parent Domain.Lib.f.chain\nlemma parent() {}\nfunction unrelated(): int { 0 }\n").unwrap();
    let log = "Domain/Lib.dfy(3,2): Error: assertion might not hold\nDomain/Lib.dfy(6,2): Error: Verification timed out after 30 seconds\nDomain/Lib.dfy(2,2): Related location: this is the postcondition\n";
    let (reports, claims) = collect_dafny(
        &catalog,
        dir.path().to_str().unwrap(),
        "Domain/Lib.dfy",
        log,
        false,
        true,
    );
    let report = &reports["Domain.Lib.f.chain.because2"];
    assert_eq!(report["status"], "checker_limit");
    assert_eq!(report["file"], "domain/lib.av");
    assert_eq!(report["goal"], "x > 1");
    assert_eq!(report["assumptions"][0]["expression"], "x >= 0");
    assert_eq!(report["assumptions"][1]["expression"], "x > 0");
    assert_eq!(report["assumptions"][1]["status"], "unresolved");
    assert_eq!(reports["Domain.Lib.f.chain.because1"]["status"], "unproved");
    assert_eq!(claims["Domain.Lib.f.chain"]["status"], "unresolved");
    assert_eq!(
        claims["Domain.Lib.f.chain.implication"]["status"],
        "not_exported"
    );
    let (reports, _) = collect_dafny(
        &catalog,
        dir.path().to_str().unwrap(),
        "Domain/Lib.dfy",
        "Domain/Lib.dfy(9,2): Error: unknown name\n",
        false,
        true,
    );
    assert_eq!(reports.len(), 1);
    assert!(reports.contains_key("<proof checker>"));
    let (reports, _) = collect_dafny(
        &catalog,
        dir.path().to_str().unwrap(),
        "Domain/Lib.dfy",
        "checker terminated",
        false,
        true,
    );
    assert!(reports.contains_key("<proof checker>"));
}
