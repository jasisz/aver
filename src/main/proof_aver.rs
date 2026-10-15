//! `aver proof --backend aver`: the step producers write each law's proof as
//! data and the proof kernel written in Aver (`tools/proof-kernel`, embedded
//! as `aver::proof_kernel`) checks it in process. No Lean export, no `lake`.
//! A law no producer handles, or whose script the kernel refuses, is reported
//! as not closed by this backend, like a `sorry` under Lean.

use std::collections::{BTreeMap, BTreeSet};

use aver::ir::LawTheorem;

/// How one law ended under the aver backend.
enum Verdict {
    Steps,
    /// The producer checked the script but the kernel refused it: a
    /// producer bug, which fails the run.
    Refused(String),
    /// No producer wrote steps; why, when one said.
    Open(Option<String>),
    /// The script checks, but cites a law this backend did not close.
    CitesOpen(String),
    /// The script checks, but cites a law whose citations go round in a
    /// circle, so no law of the circle is proved.
    CitesCircle(String),
}

fn law_key(symbols: &aver::ir::SymbolTable, t: &LawTheorem) -> String {
    let key = &symbols.fn_entry(t.fn_id).key;
    match key.scope_str() {
        Some(scope) => format!("{scope}.{}.{}", key.name, t.law_name),
        None => format!("{}.{}", key.name, t.law_name),
    }
}

#[allow(clippy::too_many_arguments)]
pub(super) fn run(
    file: &str,
    output_dir: &str,
    project_name: Option<&str>,
    module_root_override: Option<&str>,
    check: bool,
    sorry_budget: Option<usize>,
    check_json: bool,
) {
    let (ctx, _module_root) = super::build_codegen_context(
        file,
        project_name,
        module_root_override,
        false,
        &super::super::cli::CompilePolicyMode::Embed,
        None,
        false,
        false,
        true,
        true,
        true,
        true, // proof_command
    );
    let started = std::time::Instant::now();
    let steps_dir = std::path::Path::new(output_dir).join("proof_steps");
    let mut verdicts: BTreeMap<String, Verdict> = BTreeMap::new();
    // The obligations of `because` chains, apart from the laws: a law
    // counts only when all of its obligations close.
    let mut obligation_verdicts: BTreeMap<String, Verdict> = BTreeMap::new();
    let mut obligation_cites: BTreeMap<String, Vec<String>> = BTreeMap::new();
    // What would close a law the producer left open, by law.
    let mut hints: BTreeMap<String, Vec<String>> = BTreeMap::new();
    // The laws a project plan proved, and how a report names that proof.
    let mut by_plan: BTreeMap<String, String> = BTreeMap::new();
    // The earlier laws the automatic pass applied, by law, and the laws
    // whose `using` line names what they cite.
    let mut automatic: BTreeMap<String, Vec<String>> = BTreeMap::new();
    let mut pinned: Vec<String> = Vec::new();
    // What each checked script stands on, read off the text the kernel
    // checked; reported for the laws and obligations that close.
    let mut trust: BTreeMap<String, aver::ir::proof_steps::trust::Trust> = BTreeMap::new();
    for theorem in &ctx.proof_ir.law_theorems {
        let key = law_key(&ctx.symbol_table, theorem);
        if theorem.steps.is_some() && !theorem.steps_auto_using.is_empty() {
            automatic.insert(key.clone(), theorem.steps_auto_using.clone());
        }
        if theorem.using.as_ref().is_some_and(|u| !u.is_empty()) {
            pinned.push(key.clone());
        }
        if let Some(script) = &theorem.steps
            && let Some(plan) = &script.plan
        {
            by_plan.insert(key.clone(), plan.describe(script.proof.size()));
        }
        if !theorem.steps_hints.is_empty() {
            hints.insert(key.clone(), theorem.steps_hints.clone());
        }
        let verdict = match &theorem.steps {
            None => Verdict::Open(theorem.steps_refusal.clone()),
            Some(script) => match aver::ir::proof_steps::sexpr::script(script, &ctx.symbol_table) {
                Err(why) => Verdict::Refused(why),
                Ok(text) => {
                    if std::fs::create_dir_all(&steps_dir).is_ok() {
                        let _ = std::fs::write(steps_dir.join(format!("{key}.steps")), &text);
                    }
                    match aver::proof_kernel::verdict(&text) {
                        Ok(proved) if proved == key => {
                            if check_json
                                && let Ok(t) = aver::ir::proof_steps::trust::of_script(&text)
                            {
                                trust.insert(key.clone(), t);
                            }
                            Verdict::Steps
                        }
                        Ok(other) => Verdict::Refused(format!("the script proves {other}")),
                        Err(why) => Verdict::Refused(why),
                    }
                }
            },
        };
        verdicts.insert(key, verdict);
        for ob in &theorem.obligation_steps {
            let verdict = match &ob.script {
                None => Verdict::Open(ob.refusal.clone()),
                Some(script) => {
                    match aver::ir::proof_steps::sexpr::script(script, &ctx.symbol_table) {
                        Err(why) => Verdict::Refused(why),
                        Ok(text) => {
                            if std::fs::create_dir_all(&steps_dir).is_ok() {
                                let _ = std::fs::write(
                                    steps_dir.join(format!("{}.steps", ob.key)),
                                    &text,
                                );
                            }
                            match aver::proof_kernel::verdict(&text) {
                                Ok(proved) if proved == ob.key => {
                                    if check_json
                                        && let Ok(t) =
                                            aver::ir::proof_steps::trust::of_script(&text)
                                    {
                                        trust.insert(ob.key.clone(), t);
                                    }
                                    Verdict::Steps
                                }
                                Ok(other) => Verdict::Refused(format!("the script proves {other}")),
                                Err(why) => Verdict::Refused(why),
                            }
                        }
                    }
                }
            };
            obligation_verdicts.insert(ob.key.clone(), verdict);
            if let Some(script) = &ob.script {
                obligation_cites.insert(
                    ob.key.clone(),
                    script
                        .laws
                        .iter()
                        .filter(|l| l.fact.is_none())
                        .map(|l| l.key.clone())
                        .collect(),
                );
            }
        }
    }
    // Credit composes: a law whose script cites a law this backend did not
    // close is not closed either (Lean gets the same from its axiom audit).
    let cites: BTreeMap<String, Vec<String>> = ctx
        .proof_ir
        .law_theorems
        .iter()
        .filter_map(|t| {
            let script = t.steps.as_ref()?;
            Some((
                law_key(&ctx.symbol_table, t),
                // A builtin fact carries its proof, which the kernel just
                // checked as part of the citing script.
                script
                    .laws
                    .iter()
                    .filter(|l| l.fact.is_none())
                    .map(|l| l.key.clone())
                    .collect(),
            ))
        })
        .collect();
    loop {
        let demote: Vec<(String, String)> = verdicts
            .iter()
            .filter(|(_, v)| matches!(v, Verdict::Steps))
            .filter_map(|(k, _)| {
                cites.get(k)?.iter().find_map(|c| {
                    (!matches!(verdicts.get(c), Some(Verdict::Steps)))
                        .then(|| (k.clone(), c.clone()))
                })
            })
            .collect();
        if demote.is_empty() {
            break;
        }
        for (k, c) in demote {
            verdicts.insert(k, Verdict::CitesOpen(c));
        }
    }
    credit_bottom_up(&mut verdicts, &cites);
    // An obligation is closed only when the laws it cites are.
    for (k, cited) in &obligation_cites {
        if !matches!(obligation_verdicts.get(k), Some(Verdict::Steps)) {
            continue;
        }
        if let Some(c) = cited
            .iter()
            .find(|c| !matches!(verdicts.get(*c), Some(Verdict::Steps)))
        {
            obligation_verdicts.insert(k.clone(), Verdict::CitesOpen(c.clone()));
        }
    }
    let elapsed = started.elapsed();
    let closed = verdicts
        .values()
        .filter(|v| matches!(v, Verdict::Steps))
        .count();
    let open = verdicts.len() - closed;
    let closed_with = |laws: &mut dyn Iterator<Item = &String>| {
        laws.filter(|k| matches!(verdicts.get(*k), Some(Verdict::Steps)))
            .count()
    };
    let closed_pinned = closed_with(&mut pinned.iter());
    let closed_automatic = closed_with(&mut automatic.keys());
    let refused: Vec<(&String, &String)> = verdicts
        .iter()
        .chain(&obligation_verdicts)
        .filter_map(|(k, v)| match v {
            Verdict::Refused(why) => Some((k, why)),
            _ => None,
        })
        .collect();
    let checking = check || check_json;
    let passed = open <= sorry_budget.unwrap_or(0);
    if check_json {
        let mut obj = serde_json::Map::new();
        obj.insert("backend".into(), "aver".into());
        obj.insert(
            "closed_by".into(),
            serde_json::Value::Object(
                verdicts
                    .iter()
                    .map(|(k, v)| {
                        let by = if matches!(v, Verdict::Steps) {
                            "steps"
                        } else {
                            "open"
                        };
                        (k.clone(), by.into())
                    })
                    .collect(),
            ),
        );
        obj.insert(
            "obligations_closed_by".into(),
            serde_json::Value::Object(
                obligation_verdicts
                    .iter()
                    .map(|(k, v)| {
                        let by = if matches!(v, Verdict::Steps) {
                            "steps"
                        } else {
                            "open"
                        };
                        (k.clone(), by.into())
                    })
                    .collect(),
            ),
        );
        obj.insert(
            "steps_rejected".into(),
            serde_json::Value::Array(refused.iter().map(|(k, _)| (*k).clone().into()).collect()),
        );
        if !hints.is_empty() {
            obj.insert(
                "steps_hints".into(),
                serde_json::Value::Object(
                    hints
                        .iter()
                        .map(|(k, hs)| {
                            (
                                k.clone(),
                                serde_json::Value::Array(
                                    hs.iter().map(|h| h.clone().into()).collect(),
                                ),
                            )
                        })
                        .collect(),
                ),
            );
        }
        if !automatic.is_empty() {
            obj.insert(
                "automatic_using".into(),
                serde_json::Value::Object(
                    automatic
                        .iter()
                        .map(|(k, laws)| {
                            (
                                k.clone(),
                                serde_json::Value::Array(
                                    laws.iter().map(|l| l.clone().into()).collect(),
                                ),
                            )
                        })
                        .collect(),
                ),
            );
        }
        // What each closed law and obligation stands on: the wall rules,
        // the kernel's own procedures and the laws and facts it cites.
        let closed_trust: serde_json::Map<String, serde_json::Value> = verdicts
            .iter()
            .chain(&obligation_verdicts)
            .filter(|(_, v)| matches!(v, Verdict::Steps))
            .filter_map(|(k, _)| Some((k.clone(), trust.get(k)?.to_json())))
            .collect();
        if !closed_trust.is_empty() {
            obj.insert("trust".into(), serde_json::Value::Object(closed_trust));
        }
        obj.insert("closed_with_pinned_using".into(), closed_pinned.into());
        obj.insert(
            "closed_with_automatic_using".into(),
            closed_automatic.into(),
        );
        obj.insert("universal_laws".into(), closed.into());
        obj.insert("open_laws".into(), open.into());
        obj.insert("budget".into(), sorry_budget.unwrap_or(0).into());
        obj.insert("passed".into(), passed.into());
        obj.insert(
            "kernel_ms".into(),
            serde_json::Value::from(elapsed.as_secs_f64() * 1000.0),
        );
        println!("{}", serde_json::Value::Object(obj));
    } else {
        for (key, verdict) in &verdicts {
            let line = match verdict {
                Verdict::Steps => match by_plan.get(key) {
                    Some(plan) => format!("closed by steps ({plan})"),
                    None => "closed by steps".to_string(),
                },
                Verdict::Open(None) => "not closed by this backend".to_string(),
                Verdict::Open(Some(why)) => format!("not closed by this backend (steps: {why})"),
                Verdict::Refused(why) => format!("not closed by this backend (kernel: {why})"),
                Verdict::CitesOpen(law) => {
                    format!("not closed by this backend (it cites {law}, which is not)")
                }
                Verdict::CitesCircle(law) => format!(
                    "not closed by this backend (it cites {law}, whose citations go round in a circle)"
                ),
            };
            println!("  {key}: {line}");
            let prefix = format!("{key}.");
            for (ob, v) in obligation_verdicts
                .iter()
                .filter(|(k, _)| k.starts_with(&prefix))
            {
                let line = match v {
                    Verdict::Steps => "closed by steps".to_string(),
                    Verdict::Open(None) => "open".to_string(),
                    Verdict::Open(Some(why)) => format!("open (steps: {why})"),
                    Verdict::Refused(why) => format!("open (kernel: {why})"),
                    Verdict::CitesOpen(law) => format!("open (it cites {law}, which is not)"),
                    Verdict::CitesCircle(law) => {
                        format!("open (it cites {law}, whose citations go round in a circle)")
                    }
                };
                println!("    obligation {ob}: {line}");
            }
            // An open law's hints say what would close it; a closed law's,
            // what would make its steps faster.
            for hint in hints.get(key).into_iter().flatten() {
                println!("    hint: {hint}");
            }
        }
        println!(
            "aver proof (backend aver): {closed} of {} law(s) closed by steps checked by the Aver kernel in {:.1} ms",
            verdicts.len(),
            elapsed.as_secs_f64() * 1000.0
        );
        if closed_automatic > 0 {
            println!(
                "aver proof (backend aver): {closed_automatic} of them with earlier laws the steps chose, {closed_pinned} with a `using` line"
            );
        }
    }
    if !refused.is_empty() {
        eprintln!(
            "aver proof (backend aver): the kernel refused the steps of {} (a proof-step producer error; please report it)",
            refused
                .iter()
                .map(|(k, _)| k.as_str())
                .collect::<Vec<_>>()
                .join(", ")
        );
        std::process::exit(1);
    }
    if checking && !passed {
        std::process::exit(1);
    }
}

/// Every law left as [`Verdict::Steps`] cites only laws whose scripts check,
/// but a script takes the laws it cites as given, so credit is earned
/// bottom-up: a law is closed once every law it cites was closed before it.
/// Laws that cite each other in a circle are never reached, and neither is a
/// law that cites one of them; they become [`Verdict::CitesCircle`].
fn credit_bottom_up(
    verdicts: &mut BTreeMap<String, Verdict>,
    cites: &BTreeMap<String, Vec<String>>,
) {
    let mut credited: BTreeSet<String> = BTreeSet::new();
    loop {
        let next: Vec<String> = verdicts
            .iter()
            .filter(|(k, v)| matches!(v, Verdict::Steps) && !credited.contains(*k))
            .filter(|(k, _)| {
                cites
                    .get(*k)
                    .is_none_or(|cs| cs.iter().all(|c| credited.contains(c)))
            })
            .map(|(k, _)| k.clone())
            .collect();
        if next.is_empty() {
            break;
        }
        credited.extend(next);
    }
    for (k, v) in verdicts.iter_mut() {
        if matches!(v, Verdict::Steps) && !credited.contains(k) {
            let c = cites
                .get(k)
                .and_then(|cs| cs.iter().find(|c| !credited.contains(*c)))
                .cloned()
                .unwrap_or_default();
            *v = Verdict::CitesCircle(c);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn laws_that_cite_each_other_in_a_circle_are_not_closed() {
        let mut verdicts: BTreeMap<String, Verdict> = BTreeMap::new();
        let mut cites: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (law, cited) in [
            ("base", vec![]),
            ("onBase", vec!["base"]),
            ("a", vec!["b"]),
            ("b", vec!["a"]),
            ("p", vec!["q"]),
            ("q", vec!["r"]),
            ("r", vec!["p", "base"]),
            ("self", vec!["self"]),
            ("onCircle", vec!["base", "q"]),
        ] {
            verdicts.insert(law.into(), Verdict::Steps);
            cites.insert(law.into(), cited.into_iter().map(String::from).collect());
        }
        credit_bottom_up(&mut verdicts, &cites);
        for law in ["base", "onBase"] {
            assert!(matches!(verdicts[law], Verdict::Steps), "{law}");
        }
        for (law, cited) in [
            ("a", "b"),
            ("b", "a"),
            ("p", "q"),
            ("q", "r"),
            ("r", "p"),
            ("self", "self"),
            ("onCircle", "q"),
        ] {
            assert!(
                matches!(&verdicts[law], Verdict::CitesCircle(c) if c == cited),
                "{law}"
            );
        }
    }
}
