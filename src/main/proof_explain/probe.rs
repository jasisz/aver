//! Run isolated twins of open source obligations. Probe output never enters
//! the counted build, residual audit, or proof manifest.

use super::{Catalog, ProofManifest, source, tier};
use aver::codegen::lean::{citation_probe, untranslate};
use serde::Deserialize;
use serde_json::{Value, json};
use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

#[derive(Deserialize)]
struct Attempt {
    claim: String,
    citation: String,
    phase: String,
    outcome: String,
    premises: Vec<Premise>,
}

#[derive(Deserialize)]
struct Premise {
    status: String,
    goal: Value,
    #[serde(default)]
    proof_audit: Option<String>,
    #[serde(default)]
    proof_axioms: Option<Vec<String>>,
}

fn audited_closure(premise: &Premise) -> bool {
    premise.proof_audit.as_deref() == Some("checked")
        && premise.proof_axioms.as_ref().is_some_and(|axioms| {
            axioms.iter().all(|axiom| {
                matches!(
                    axiom.as_str(),
                    "propext" | "Classical.choice" | "Quot.sound"
                )
            })
        })
}

struct Module {
    name: String,
    source: String,
}

struct Context {
    module: String,
    citations: Vec<String>,
}

pub(super) fn attach(
    dir: &str,
    catalog: &Catalog,
    manifest: Option<&ProofManifest>,
    reports: &mut BTreeMap<String, Value>,
) {
    let wanted: BTreeSet<_> = reports
        .iter()
        .filter(|(_, report)| report["status"] == "unproved")
        .map(|(claim, _)| claim.clone())
        .collect();
    if wanted.is_empty() {
        return;
    }
    let mut modules = Vec::new();
    read_modules(Path::new(dir), Path::new(dir), &mut modules);
    modules.retain(|module| {
        catalog
            .laws
            .values()
            .any(|law| law.emitted_module == module.name)
    });
    modules.sort_by(|a, b| a.name.cmp(&b.name));
    let labels = unique_markers(catalog, &modules);
    let mut bodies = String::new();
    let mut owners = BTreeMap::new();
    let mut imports = BTreeSet::new();
    for module in &modules {
        let lines: Vec<_> = module.source.lines().collect();
        let mut found = String::new();
        for (index, line) in lines.iter().enumerate() {
            let Some((theorem, claim)) = marker(line) else {
                continue;
            };
            if !wanted.contains(claim)
                || labels
                    .get(&format!("{}.{theorem}", module.name))
                    .map(String::as_str)
                    != Some(claim)
                || !catalog
                    .claim(claim)
                    .is_some_and(|(law, _)| law.emitted_module == module.name)
            {
                continue;
            }
            let Some(start) = (index + 1..lines.len()).find(|&i| !lines[i].trim().is_empty())
            else {
                continue;
            };
            if lines[start]
                .trim()
                .strip_prefix("theorem ")
                .and_then(|declaration| declaration.split_once(" : "))
                .map(|(name, _)| name)
                != Some(theorem)
            {
                continue;
            }
            let end = (start + 1..lines.len())
                .find(|&i| {
                    marker(lines[i]).is_some()
                        || lines[i].starts_with("theorem ")
                        || lines[i].starts_with("private theorem ")
                        || lines[i].starts_with("end ")
                        || lines[i].starts_with("def ")
                        || lines[i].starts_with("namespace ")
                })
                .unwrap_or(lines.len());
            let name = format!("_aver_citation_{}", owners.len());
            let Some(body) = citation_probe::probe_body(&lines[start..end], &name, claim) else {
                continue;
            };
            let citations = body
                .lines()
                .filter_map(|line| {
                    line.trim()
                        .strip_prefix("have _fact")?
                        .split_once(" := ")
                        .map(|(_, name)| name.to_string())
                })
                .collect();
            found.push_str(&body);
            owners.insert(
                claim.to_string(),
                Context {
                    module: module.name.clone(),
                    citations,
                },
            );
        }
        if found.is_empty() {
            continue;
        }
        imports.insert(module.name.clone());
        bodies.push_str("section\n");
        for line in &lines {
            if line.starts_with("namespace ") {
                break;
            }
            if line.starts_with("open ") || line.starts_with("set_option ") {
                bodies.push_str(line);
                bodies.push('\n');
            }
        }
        bodies.push_str(&format!(
            "namespace {}\n{found}end {}\nend\n",
            module.name, module.name
        ));
    }
    if owners.is_empty() {
        return;
    }
    let mut text = String::from("import Lean\n");
    for module in imports {
        text.push_str(&format!("import {module}\n"));
    }
    text.push_str(untranslate::AVER_DUMP_GOAL_ELAB.trim_start_matches("import Lean\n"));
    text.push_str(citation_probe::ELAB);
    text.push_str(&bodies);
    let name = "_aver_citation_probe.lean";
    let file = Path::new(dir).join(name);
    if std::fs::write(&file, text).is_err() {
        return;
    }
    let output = std::process::Command::new("lake")
        .args(["env", "lean", name])
        .current_dir(dir)
        .output();
    let _ = std::fs::remove_file(&file);
    let Ok(output) = output else { return };
    let text = format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let _ = std::fs::write(Path::new(dir).join("proof_citations.log"), &text);
    if !output.status.success() {
        for claim in owners.keys() {
            if let Some(report) = reports.get_mut(claim) {
                report["citation_probe"] = json!({"status": "unavailable"});
            }
        }
        return;
    }
    attach_output(catalog, manifest, reports, &owners, &labels, &text);
    for report in reports.values_mut() {
        super::attempt_report::finish(report);
    }
}

/// A claim and its emitted theorem must identify each other uniquely. Even
/// identical repeated markers are refused: two candidate source blocks must
/// never compete to supply one reported diagnostic context.
fn unique_markers(catalog: &Catalog, modules: &[Module]) -> BTreeMap<String, String> {
    let mut candidates = Vec::new();
    for module in modules {
        for line in module.source.lines() {
            let Some((theorem, claim)) = marker(line) else {
                continue;
            };
            if catalog
                .claim(claim)
                .is_some_and(|(law, _)| law.emitted_module == module.name)
            {
                candidates.push((format!("{}.{theorem}", module.name), claim.to_string()));
            }
        }
    }
    let mut theorem_counts = BTreeMap::<&str, usize>::new();
    let mut claim_counts = BTreeMap::<&str, usize>::new();
    for (theorem, claim) in &candidates {
        *theorem_counts.entry(theorem).or_default() += 1;
        *claim_counts.entry(claim).or_default() += 1;
    }
    candidates
        .iter()
        .filter(|(theorem, claim)| {
            theorem_counts.get(theorem.as_str()) == Some(&1)
                && claim_counts.get(claim.as_str()) == Some(&1)
        })
        .cloned()
        .collect()
}

fn read_modules(root: &Path, dir: &Path, modules: &mut Vec<Module>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        if name.to_string_lossy().starts_with('.') {
            continue;
        }
        let Ok(kind) = entry.file_type() else {
            continue;
        };
        if kind.is_dir() {
            read_modules(root, &entry.path(), modules);
        } else if kind.is_file() && entry.path().extension().is_some_and(|ext| ext == "lean") {
            let path = entry.path();
            let Ok(relative) = path.strip_prefix(root) else {
                continue;
            };
            let Some(relative) = relative.to_str() else {
                continue;
            };
            let Ok(source) = std::fs::read_to_string(&path) else {
                continue;
            };
            if source.lines().any(|line| marker(line).is_some()) {
                modules.push(Module {
                    name: relative
                        .trim_end_matches(".lean")
                        .replace(std::path::MAIN_SEPARATOR, "."),
                    source,
                });
            }
        }
    }
}

fn marker(line: &str) -> Option<(&str, &str)> {
    let marker = line
        .strip_prefix(aver::codegen::lean::LAW_CLASS_MARKER_PREFIX)
        .or_else(|| line.strip_prefix(aver::codegen::lean::LAW_OBLIGATION_MARKER_PREFIX))?;
    let mut fields = marker.split_whitespace();
    let name = fields.next()?;
    fields.next()?;
    Some((name, fields.next()?))
}

fn attach_output(
    catalog: &Catalog,
    manifest: Option<&ProofManifest>,
    reports: &mut BTreeMap<String, Value>,
    owners: &BTreeMap<String, Context>,
    labels: &BTreeMap<String, String>,
    text: &str,
) {
    for line in text.lines() {
        let Some((_, text)) = line.split_once(citation_probe::MARKER) else {
            continue;
        };
        let Ok(attempt) = serde_json::from_str::<Attempt>(text) else {
            continue;
        };
        if attempt.phase != "diagnostic_direct_application"
            || !matches!(
                attempt.outcome.as_str(),
                "matched" | "application_failed" | "preparation_failed" | "probe_error"
            )
        {
            continue;
        }
        let Some(owner) = owners.get(&attempt.claim) else {
            continue;
        };
        if !owner.citations.contains(&attempt.citation) {
            continue;
        }
        let Some(report) = reports.get_mut(&attempt.claim) else {
            continue;
        };
        let Some((law, _)) = catalog.claim(&attempt.claim) else {
            continue;
        };
        let Some(citation) = labels
            .get(&format!("{}.{}", owner.module, attempt.citation))
            .or_else(|| labels.get(&attempt.citation))
        else {
            continue;
        };
        let available_laws: Vec<_> = owner.citations.iter().map(|name| {
            let label = labels.get(&format!("{}.{name}", owner.module)).or_else(|| labels.get(name));
            json!({"law": label, "status": label.map_or("not_checked", |label| tier(manifest, label))})
        }).collect();
        let established_dependencies = attempt
            .premises
            .iter()
            .all(|p| p.status != "closed" || audited_closure(p))
            && available_laws
                .iter()
                .all(|law| law["status"] == "universal")
            && report["assumptions"].as_array().is_none_or(|assumptions| {
                assumptions
                    .iter()
                    .all(|a| a["kind"] != "because" || a["status"] == "universal")
            });
        if attempt
            .premises
            .iter()
            .any(|p| !matches!(p.status.as_str(), "closed" | "open"))
        {
            continue;
        }
        let premises: Vec<_> = if attempt.outcome == "matched" {
            attempt.premises.iter().map(|premise| {
                let mut rendered = match untranslate::untranslate_premise_json(&premise.goal.to_string(), &law.untranslate) {
                    Ok(goal) => json!({"status": premise.status, "expression": source::expression(&goal.expression), "variables": goal.givens, "source_form": "aver"}),
                    Err(_) => json!({"status": premise.status, "source_form": "unavailable"}),
                };
                if premise.status == "closed" {
                    rendered["closure_audited"] = json!(audited_closure(premise));
                    rendered["proof_axioms"] = json!(premise.proof_axioms);
                }
                rendered
            }).collect()
        } else {
            Vec::new()
        };
        if report["citation_attempts"].is_null() {
            report["citation_attempts"] = json!([]);
        }
        report["citation_attempts"]
            .as_array_mut()
            .unwrap()
            .push(json!({
            "law": citation, "phase": attempt.phase, "outcome": attempt.outcome,
            "law_status": tier(manifest, citation), "available_laws": available_laws,
            "established_dependencies": established_dependencies, "premises": premises,
            }));
        report["citation_probe"] = json!({"status": "completed"});
    }
}

#[cfg(test)]
mod tests;
