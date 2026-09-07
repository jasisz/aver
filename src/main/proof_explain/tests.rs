use super::*;
use aver::ast::TopLevel;

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
