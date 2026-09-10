//! Explanations of failed proof steps in the source language. This report is
//! diagnostic metadata only; the existing backend audit remains authoritative.

mod attempt_report;
mod backend;
mod candidates;
mod dafny;
mod display;
mod probe;
mod source;

pub(super) use candidates::render as render_candidates;

pub(super) fn attach_citation_attempts(
    dir: &str,
    catalog: &Catalog,
    manifest: Option<&ProofManifest>,
    reports: &mut BTreeMap<String, Value>,
) {
    probe::attach(dir, catalog, manifest, reports);
}

use super::{LawTier, ProofManifest};
use serde_json::{Value, json};
pub(super) use source::Catalog;
use std::collections::BTreeMap;

pub(super) fn collect(
    catalog: &Catalog,
    manifest: Option<&ProofManifest>,
    sorry_laws: &[String],
    dir: &str,
    output: &str,
) -> BTreeMap<String, Value> {
    collect_with_failures(
        catalog,
        manifest,
        sorry_laws,
        output,
        backend::failures(dir, output),
    )
}

pub(super) fn collect_dafny(
    catalog: &Catalog,
    dir: &str,
    entry: &str,
    output: &str,
    checked: bool,
    checker_failed: bool,
) -> (BTreeMap<String, Value>, BTreeMap<String, Value>) {
    let (mut failures, exported) = dafny::scan(catalog, dir, entry, output);
    let mut claims = BTreeMap::new();
    for law in catalog.laws.values() {
        let steps = (1..=law.body.because.len()).map(|i| format!("{}.because{i}", law.id));
        let implication = (!law.body.because.is_empty()).then(|| format!("{}.implication", law.id));
        for id in std::iter::once(law.id.clone())
            .chain(steps)
            .chain(implication)
        {
            let emitted = exported.contains(&id);
            let status = if !emitted {
                "not_exported"
            } else if checked {
                "checked"
            } else {
                "unresolved"
            };
            claims.insert(id, json!({"exported": emitted, "status": status}));
        }
    }
    // A failed process without a usable location still needs a source-facing
    // explanation. Never infer success from the absence of a lemma diagnostic.
    if checker_failed && failures.claims.is_empty() {
        failures.unmapped = true;
    }
    let mut reports = collect_with_failures(catalog, None, &[], output, failures);
    for report in reports.values_mut() {
        for key in ["assumptions", "citations"] {
            if let Some(items) = report[key].as_array_mut() {
                for item in items {
                    let identity = item["claim"].as_str().or_else(|| item["law"].as_str());
                    if let Some(status) = identity
                        .and_then(|id| claims.get(id))
                        .map(|c| c["status"].clone())
                    {
                        item["status"] = status;
                    }
                }
            }
        }
    }
    (reports, claims)
}

fn collect_with_failures(
    catalog: &Catalog,
    manifest: Option<&ProofManifest>,
    sorry_laws: &[String],
    output: &str,
    failures: backend::Failures,
) -> BTreeMap<String, Value> {
    let mut issues = BTreeMap::<String, backend::Failure>::new();
    let unproved = backend::Failure {
        status: "unproved",
        message: "This step has not been proved from the current assumptions.",
    };
    if let Some(manifest) = manifest {
        for record in manifest.laws.iter().chain(&manifest.obligations) {
            if record.tier != LawTier::Universal {
                let issue = if record.tier == LawTier::Bounded {
                    backend::Failure {
                        status: "bounded",
                        message: "Only the declared sample domain is proved; the general statement is still open.",
                    }
                } else {
                    unproved.clone()
                };
                issues.insert(record.law.clone(), issue);
            }
        }
    }
    for identity in sorry_laws {
        issues
            .entry(identity.clone())
            .or_insert_with(|| unproved.clone());
    }
    // A hard error takes precedence over a residual warning at the same step.
    let unmapped =
        failures.unmapped || failures.claims.keys().any(|id| catalog.claim(id).is_none());
    issues.extend(failures.claims);
    let identities: Vec<_> = issues.keys().cloned().collect();
    let mut reports = BTreeMap::new();
    for (identity, failure) in issues {
        let Some((law, step)) = catalog.claim(&identity) else {
            continue;
        };
        if step.is_none()
            && identities.iter().any(|other| {
                other
                    .strip_prefix(&format!("{}.", law.id))
                    .is_some_and(|suffix| {
                        suffix == "implication"
                            || suffix
                                .strip_prefix("because")
                                .is_some_and(|n| n.parse::<usize>().is_ok())
                    })
            })
        {
            continue;
        }
        let index = step.unwrap_or(law.body.because.len());
        let reason = law.body.because.get(index);
        let line = reason.map_or(law.line, |expr| expr.line);
        let goal = reason
            .map(source::expression)
            .unwrap_or_else(|| source::assertion(&law.body));
        let variables: Vec<_> = law
            .body
            .givens
            .iter()
            .map(|given| json!({"name": given.name, "type": given.type_name}))
            .collect();
        let mut assumptions = Vec::new();
        for guard in law.body.when.iter().flat_map(source::conditions) {
            assumptions.push(json!({"kind": "when", "expression": source::expression(guard), "status": "assumed"}));
        }
        for (previous, expr) in law.body.because.iter().take(index).enumerate() {
            let previous_id = format!("{}.because{}", law.id, previous + 1);
            assumptions.push(json!({
                "kind": "because", "claim": previous_id,
                "expression": source::expression(expr),
                "status": tier(manifest, &previous_id),
            }));
        }
        let citations: Vec<_> = law.body.using.iter().flatten().map(|name| {
            let Some(cited) = catalog.citation(law, name) else {
                return json!({"law": name, "status": "source_unavailable"});
            };
            let bindings = source::bindings(law, cited, reason);
            let requirements: Vec<_> = cited.body.when.iter().flat_map(source::conditions).map(|expr| {
                source::expression(&bindings.as_ref().map_or_else(|| expr.clone(), |bindings| source::substitute(expr, bindings)))
            }).collect();
            let mut assertion = cited.body.clone();
            if let Some(bindings) = &bindings {
                assertion.lhs = source::substitute(&assertion.lhs, bindings);
                assertion.rhs = source::substitute(&assertion.rhs, bindings);
            }
            let bindings_json: BTreeMap<_, _> = bindings.as_ref().into_iter().flat_map(|bindings| {
                bindings.iter().map(|(name, expr)| (name.clone(), source::expression(expr)))
            }).collect();
            json!({
                "law": cited.id, "file": cited.file, "line": cited.line,
                "status": tier(manifest, &cited.id),
                "variables": cited.body.givens.iter().map(|given| json!({"name": given.name, "type": given.type_name})).collect::<Vec<_>>(),
                "instantiated": bindings.is_some(), "bindings": bindings_json,
                "requires": requirements, "conclusion": source::assertion(&assertion),
            })
        }).collect();
        let next = if failure.status == "checker_error" {
            "Report the checker error at this source step, with proof_backend.log from the output directory. The error does not identify a missing mathematical premise."
        } else if failure.status == "checker_limit" {
            "Split this step into smaller because expressions or a helper law; the checker did not finish the current proof."
        } else if failure.status == "citation_unavailable" {
            "Check that each selected law can be cited at this step. If the cited laws are already universally checked, report this diagnostic with proof_backend.log."
        } else if reason.is_none() && !law.body.because.is_empty() {
            "Connect the earlier because statements to the final claim. A proved intermediate statement does not by itself establish this implication."
        } else if citations
            .iter()
            .any(|c| c["requires"].as_array().is_some_and(|r| !r.is_empty()))
        {
            "Check that the cited law's required conditions follow from the current assumptions. If needed, establish them in earlier because steps or use another helper law."
        } else {
            "State the missing intermediate fact as a because expression or a helper law. Use aver verify to check concrete examples."
        };
        reports.insert(
            identity.clone(),
            json!({
                "claim": identity, "law": law.id, "file": law.file, "line": line,
                "status": failure.status, "message": failure.message,
                "goal": goal, "variables": variables, "assumptions": assumptions,
                "citation_mode": if law.body.using.is_some() { "explicit" } else { "automatic" },
                "citations": citations, "next": next,
            }),
        );
    }
    if unmapped
        || (reports.is_empty()
            && output
                .lines()
                .any(|line| line.trim_start().starts_with("error:")))
    {
        reports.insert("<proof checker>".to_string(), json!({
            "claim": "<proof checker>", "file": catalog.entry_file, "line": 1,
            "status": "checker_error", "goal": "check the program's proof",
            "message": "A proof checker error could not be associated with an Aver proof step.",
            "next": "The technical diagnostic is saved in proof_backend.log. Report this checker error; no mathematical conclusion can be drawn from it.",
        }));
    }
    reports
}

fn tier(manifest: Option<&ProofManifest>, identity: &str) -> &'static str {
    manifest
        .and_then(|manifest| {
            manifest
                .laws
                .iter()
                .chain(&manifest.obligations)
                .find(|record| record.law == identity)
        })
        .map_or("not_checked", |record| record.tier.as_str())
}

pub(super) fn render(reports: &BTreeMap<String, Value>) {
    if reports.is_empty() {
        return;
    }
    println!("\n--explain: proof steps in Aver");
    for report in reports.values() {
        let text = |key: &str| report[key].as_str().unwrap_or("");
        println!("\n{}:{} — {}", text("file"), report["line"], text("claim"));
        println!("  {}", text("message"));
        println!("  To prove: {}", text("goal"));
        if let Some(variables) = report["variables"].as_array() {
            let variables: Vec<_> = variables
                .iter()
                .map(|v| {
                    format!(
                        "{}: {}",
                        v["name"].as_str().unwrap_or(""),
                        v["type"].as_str().unwrap_or("")
                    )
                })
                .collect();
            if !variables.is_empty() {
                println!("  For any {}", variables.join(", "));
            }
        }
        if let Some(assumptions) = report["assumptions"].as_array() {
            for assumption in assumptions {
                println!(
                    "  {} {} [{}]",
                    assumption["kind"].as_str().unwrap_or(""),
                    assumption["expression"].as_str().unwrap_or(""),
                    assumption["status"].as_str().unwrap_or("")
                );
            }
        }
        if report["citation_mode"] == "automatic" {
            println!("  Law selection: automatic (no explicit using list)");
        }
        if let Some(citations) = report["citations"].as_array() {
            for citation in citations {
                let scope = if citation["instantiated"] == true {
                    "source arguments substituted"
                } else {
                    "schematic parameters"
                };
                println!(
                    "  using {} [{}; {scope}]",
                    citation["law"].as_str().unwrap_or(""),
                    citation["status"].as_str().unwrap_or("")
                );
                if let Some(conclusion) = citation["conclusion"].as_str() {
                    println!("    provides {conclusion}");
                }
                if let Some(requirements) = citation["requires"].as_array() {
                    for requirement in requirements {
                        println!("    requires {}", requirement.as_str().unwrap_or(""));
                    }
                }
            }
        }
        attempt_report::render(report);
        println!("  Next: {}", text("next"));
    }
}

#[cfg(test)]
mod tests;
