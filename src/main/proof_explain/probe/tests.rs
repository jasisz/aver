use super::*;

const CLAIM: &str = "p.target.because1";

fn catalog() -> Catalog {
    let source = "fn p(by: Int) -> Bool\n    by > 0\nverify p law selected\n    given by: Int = [1]\n    p(by) holds\nverify p law sibling\n    given by: Int = [1]\n    p(by) holds\nverify p law target\n    given by: Int = [1]\n    because p(by)\n    using [p.selected, p.sibling]\n    p(by) holds\n";
    let mut catalog = Catalog::default();
    for item in aver::source::parse_source(source).unwrap() {
        if let aver::ast::TopLevel::Verify(block) = item {
            catalog.add(&block, None, "source.av");
        }
    }
    catalog
}

fn reports() -> BTreeMap<String, Value> {
    BTreeMap::from([(
        CLAIM.to_string(),
        json!({
            "claim": CLAIM, "status": "unproved", "assumptions": [],
        }),
    )])
}

fn owners() -> BTreeMap<String, Context> {
    BTreeMap::from([(
        CLAIM.to_string(),
        Context {
            module: "Entry".to_string(),
            citations: vec!["p_law_selected".to_string(), "p_law_sibling".to_string()],
        },
    )])
}

fn labels() -> BTreeMap<String, String> {
    BTreeMap::from([
        ("Entry.p_law_selected".to_string(), "p.selected".to_string()),
        ("Entry.p_law_sibling".to_string(), "p.sibling".to_string()),
    ])
}

fn manifest(selected: Option<&str>, sibling: Option<&str>) -> ProofManifest {
    let laws: Vec<_> = [("p.selected", selected), ("p.sibling", sibling)]
        .into_iter()
        .filter_map(|(law, tier)| tier.map(|tier| json!({"law": law, "tier": tier})))
        .collect();
    super::super::super::parse_proof_manifest(&json!({"backend": "lean", "laws": laws}).to_string())
        .unwrap()
}

fn goal() -> Value {
    json!({"forall": {
        "name": "by'", "ty": {"const": "Int"}, "body": {"app": {
            "fn": {"const": "LE.le"}, "args": [
                {"const": "Int"}, {"const": "Int.instLEInt"}, {"nat": "0"}, {"var": "by'"}
            ]
        }}
    }})
}

fn attempt() -> Value {
    json!({"claim": CLAIM, "citation": "p_law_selected",
        "phase": "diagnostic_direct_application", "outcome": "matched",
        "premises": [{"status": "closed", "goal": goal(), "proof_audit": "checked", "proof_axioms": ["propext"]}],
    })
}

fn trace(attempt: &Value) -> String {
    format!("{}{}\n", citation_probe::MARKER, attempt)
}

#[test]
fn unknown_claims_and_citations_cannot_attach_attempts() {
    let catalog = catalog();
    let manifest = manifest(Some("universal"), Some("universal"));
    for (key, unknown) in [
        ("claim", "unknown.target.because1"),
        ("citation", "unknown_law"),
    ] {
        let mut output = attempt();
        output[key] = json!(unknown);
        let mut reports = reports();
        let original = reports.clone();
        attach_output(
            &catalog,
            Some(&manifest),
            &mut reports,
            &owners(),
            &labels(),
            &trace(&output),
        );
        assert_eq!(reports, original, "must ignore an unknown {key}");
    }
}

#[test]
fn closed_premises_are_conditional_on_every_available_law() {
    let catalog = catalog();
    for (selected, sibling, expected) in [
        (Some("universal"), Some("universal"), true),
        (Some("failed"), Some("universal"), false),
        (Some("universal"), Some("failed"), false),
        (Some("universal"), None, false),
        (Some("bounded"), Some("universal"), false),
    ] {
        let manifest = manifest(selected, sibling);
        let mut reports = reports();
        attach_output(
            &catalog,
            Some(&manifest),
            &mut reports,
            &owners(),
            &labels(),
            &trace(&attempt()),
        );
        let actual = &reports[CLAIM]["citation_attempts"][0];
        assert_eq!(actual["established_dependencies"], expected);
        assert_eq!(actual["law_status"], selected.unwrap_or("not_checked"));
        assert_eq!(
            actual["available_laws"][1]["status"],
            sibling.unwrap_or("not_checked")
        );
        assert_eq!(actual["premises"][0]["status"], "closed");
        assert_eq!(
            reports[CLAIM]["status"], "unproved",
            "probe must not upgrade proof status"
        );
    }
}

#[test]
fn closure_requires_its_own_complete_axiom_audit_even_with_healthy_citations() {
    let catalog = catalog();
    let manifest = manifest(Some("universal"), Some("universal"));
    for (audit, axioms) in [
        (json!("checked"), json!(["sorryAx"])),
        (json!("checked"), json!(["foreignAxiom"])),
        (json!("unavailable"), json!([])),
        (Value::Null, Value::Null),
        (json!("checked"), Value::Null),
    ] {
        let mut output = attempt();
        output["premises"][0]["proof_audit"] = audit;
        output["premises"][0]["proof_axioms"] = axioms;
        let mut reports = reports();
        attach_output(
            &catalog,
            Some(&manifest),
            &mut reports,
            &owners(),
            &labels(),
            &trace(&output),
        );
        let actual = &reports[CLAIM]["citation_attempts"][0];
        assert_eq!(actual["established_dependencies"], false, "{actual}");
        assert_eq!(actual["premises"][0]["closure_audited"], false, "{actual}");
    }
}

#[test]
fn failed_prior_reason_keeps_a_closed_attempt_conditional() {
    let catalog = catalog();
    let manifest = manifest(Some("universal"), Some("universal"));
    let mut reports = reports();
    reports.get_mut(CLAIM).unwrap()["assumptions"] = json!([
        {"kind": "when", "status": "assumed", "expression": "by > 0"},
        {"kind": "because", "status": "failed", "expression": "p(by)"},
    ]);
    attach_output(
        &catalog,
        Some(&manifest),
        &mut reports,
        &owners(),
        &labels(),
        &trace(&attempt()),
    );
    assert_eq!(
        reports[CLAIM]["citation_attempts"][0]["established_dependencies"],
        false
    );
    assert_eq!(
        reports[CLAIM]["citation_attempts"][0]["law_status"],
        "universal"
    );
}

#[test]
fn unrecognized_record_premise_never_becomes_fabricated_aver() {
    let catalog = catalog();
    let mut output = attempt();
    output["premises"][0]["goal"]["forall"]["body"]["app"]["args"][3] = json!({"proj": {
        "struct": "Unknown.Record", "idx": 0, "e": {"var": "by'"},
    }});
    let mut reports = reports();
    attach_output(
        &catalog,
        None,
        &mut reports,
        &owners(),
        &labels(),
        &trace(&output),
    );
    let premise = &reports[CLAIM]["citation_attempts"][0]["premises"][0];
    assert_eq!(premise["source_form"], "unavailable");
    assert!(premise.get("expression").is_none());
    assert!(!reports[CLAIM].to_string().contains("Unknown.Record"));
}

#[test]
fn actual_premise_restores_the_source_variable_spelling() {
    let catalog = catalog();
    let mut reports = reports();
    attach_output(
        &catalog,
        None,
        &mut reports,
        &owners(),
        &labels(),
        &trace(&attempt()),
    );
    let premise = &reports[CLAIM]["citation_attempts"][0]["premises"][0];
    assert_eq!(premise["source_form"], "aver");
    assert_eq!(premise["expression"], "0 <= by");
    assert_eq!(premise["variables"], json!([["by", "Int"]]));
}

#[test]
fn unsuccessful_application_does_not_claim_partial_premise_results() {
    let catalog = catalog();
    for outcome in ["application_failed", "preparation_failed", "probe_error"] {
        let mut output = attempt();
        output["outcome"] = json!(outcome);
        let mut reports = reports();
        attach_output(
            &catalog,
            None,
            &mut reports,
            &owners(),
            &labels(),
            &trace(&output),
        );
        let actual = &reports[CLAIM]["citation_attempts"][0];
        assert_eq!(actual["outcome"], outcome);
        assert_eq!(actual["premises"], json!([]));
    }
}

#[test]
fn invalid_phase_outcome_or_premise_status_is_ignored() {
    let catalog = catalog();
    for invalid in ["phase", "outcome", "premise"] {
        let mut output = attempt();
        match invalid {
            "premise" => output["premises"][0]["status"] = json!("universal"),
            key => output[key] = json!("counted_proof"),
        }
        let mut reports = reports();
        let original = reports.clone();
        attach_output(
            &catalog,
            None,
            &mut reports,
            &owners(),
            &labels(),
            &trace(&output),
        );
        assert_eq!(reports, original);
    }
}

#[test]
fn duplicated_theorem_or_claim_markers_have_no_attribution_winner() {
    let mut catalog = catalog();
    for law in catalog.laws.values_mut() {
        law.emitted_module = "Entry".to_string();
    }
    let prefix = aver::codegen::lean::LAW_CLASS_MARKER_PREFIX;
    let source = |rows: &[(&str, &str)]| {
        rows.iter()
            .map(|(theorem, claim)| format!("{prefix}{theorem} universal {claim}\n"))
            .collect::<String>()
    };
    for conflict in [
        vec![("same", "p.selected"), ("same", "p.sibling")],
        vec![("first", "p.selected"), ("second", "p.selected")],
        vec![("same", "p.selected"), ("same", "p.selected")],
    ] {
        let mut rows = conflict;
        rows.push(("unrelated", "p.target"));
        let modules = [Module {
            name: "Entry".to_string(),
            source: source(&rows),
        }];
        let actual = unique_markers(&catalog, &modules);
        assert_eq!(
            actual,
            BTreeMap::from([("Entry.unrelated".to_string(), "p.target".to_string())])
        );
    }
}

#[test]
fn wrong_owner_markers_do_not_contaminate_unique_source_attribution() {
    let mut catalog = catalog();
    for law in catalog.laws.values_mut() {
        law.emitted_module = "Entry".to_string();
    }
    let prefix = aver::codegen::lean::LAW_CLASS_MARKER_PREFIX;
    let modules = [
        Module {
            name: "Entry".to_string(),
            source: format!("{prefix}real universal p.selected\n"),
        },
        Module {
            name: "StaleCopy".to_string(),
            source: format!("{prefix}other universal p.selected\n"),
        },
    ];
    assert_eq!(
        unique_markers(&catalog, &modules),
        BTreeMap::from([("Entry.real".to_string(), "p.selected".to_string())])
    );
}
