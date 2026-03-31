use std::collections::HashMap;

use serde_json::json;
use tower_lsp_server::ls_types::{CodeLens, Command, Position, Range};

use aver::ast::{DecisionImpact, TopLevel, TypeDef, VerifyKind};

/// Match `Logic.GameState` against fn name `GameState` (qualified suffix match).
fn impact_matches_fn(impact_symbol: &str, fn_name: &str) -> bool {
    impact_symbol == fn_name
        || impact_symbol
            .rsplit('.')
            .next()
            .is_some_and(|suffix| suffix == fn_name)
}
use aver::checker::{index_decisions, merge_verify_blocks};
use aver::types::checker::run_type_check_full;
use aver::verify_law::canonical_spec_ref;

use crate::completion;

const OPEN_LOCATION_COMMAND: &str = "aver.openLocation";

pub fn code_lenses(source: &str, base_dir: Option<&str>, uri: &str) -> Vec<CodeLens> {
    let items = completion::parse_items(source);
    if items.is_empty() {
        return Vec::new();
    }

    let tc_result = run_type_check_full(&items, base_dir);
    let verify_by_fn: HashMap<String, Vec<_>> =
        merge_verify_blocks(&items)
            .into_iter()
            .fold(HashMap::new(), |mut acc, vb| {
                acc.entry(vb.fn_name.clone()).or_default().push(vb);
                acc
            });
    let decisions = index_decisions(&items);

    let mut lenses = Vec::new();

    for item in &items {
        match item {
            TopLevel::FnDef(fd) => {
                if let Some(blocks) = verify_by_fn.get(&fd.name) {
                    let case_count: usize = blocks
                        .iter()
                        .filter(|vb| matches!(vb.kind, VerifyKind::Cases))
                        .map(|vb| vb.cases.len())
                        .sum();
                    let laws: Vec<_> = blocks
                        .iter()
                        .filter_map(|vb| match &vb.kind {
                            VerifyKind::Law(law) => Some(law.as_ref()),
                            VerifyKind::Cases => None,
                        })
                        .collect();
                    let law_names: Vec<&str> = laws.iter().map(|law| law.name.as_str()).collect();
                    let spec_names = canonical_specs(&fd.name, &laws, &tc_result.fn_sigs);

                    let mut details = Vec::new();
                    if case_count > 0 {
                        details.push(format!("{} concrete case(s)", case_count));
                    }
                    if !law_names.is_empty() {
                        details.push(format!("laws: {}", law_names.join(", ")));
                    }
                    if !spec_names.is_empty() {
                        details.push(format!("specs: {}", spec_names.join(", ")));
                    }

                    let verify_line = blocks.iter().map(|vb| vb.line).min().unwrap_or(fd.line);
                    lenses.push(make_lens(
                        uri,
                        verify_line,
                        fd.line,
                        format!("verify: {}", summarize_counts(case_count, law_names.len())),
                        format!("{}()\n{}", fd.name, details.join("\n")),
                    ));
                }

                let impacted_by: Vec<&str> = decisions
                    .iter()
                    .filter(|decision| {
                        decision.impacts.iter().any(
                            |impact| matches!(&impact.node, DecisionImpact::Symbol(symbol) if impact_matches_fn(symbol, &fd.name)),
                        )
                    })
                    .map(|decision| decision.name.as_str())
                    .collect();
                if !impacted_by.is_empty() {
                    let decision_line = decisions
                        .iter()
                        .find(|decision| {
                            decision.impacts.iter().any(|impact| {
                                matches!(&impact.node, DecisionImpact::Symbol(symbol) if impact_matches_fn(symbol, &fd.name))
                            })
                        })
                        .map(|decision| decision.line)
                        .unwrap_or(fd.line);
                    lenses.push(make_lens(
                        uri,
                        decision_line,
                        fd.line,
                        format!("decisions: {}", impacted_by.len()),
                        format!("{}() is impacted by:\n{}", fd.name, impacted_by.join("\n")),
                    ));
                }
            }
            TopLevel::Decision(decision) => {
                let impacts: Vec<&str> =
                    decision.impacts.iter().map(|s| s.node.text()).collect();
                let target_line = first_impact_line(&items, decision).unwrap_or(decision.line);
                lenses.push(make_lens(
                    uri,
                    target_line,
                    decision.line,
                    format!("impacts: {}", impacts.len()),
                    format!(
                        "decision {}\nchosen: {}\nimpacts:\n{}",
                        decision.name,
                        decision.chosen.node.text(),
                        impacts.join("\n")
                    ),
                ));
            }
            _ => {}
        }
    }

    lenses
}

fn canonical_specs(
    fn_name: &str,
    laws: &[&aver::ast::VerifyLaw],
    fn_sigs: &aver::verify_law::FnSigMap,
) -> Vec<String> {
    let mut specs = Vec::new();
    for law in laws {
        if let Some(spec_ref) = canonical_spec_ref(fn_name, law, fn_sigs)
            && !specs.contains(&spec_ref.spec_fn_name)
        {
            specs.push(spec_ref.spec_fn_name);
        }
    }
    specs
}

fn summarize_counts(case_count: usize, law_count: usize) -> String {
    match (case_count, law_count) {
        (0, 1) => "1 law".to_string(),
        (0, n) => format!("{} laws", n),
        (1, 0) => "1 case".to_string(),
        (n, 0) => format!("{} cases", n),
        (1, 1) => "1 case, 1 law".to_string(),
        (1, n) => format!("1 case, {} laws", n),
        (n, 1) => format!("{} cases, 1 law", n),
        (n, m) => format!("{} cases, {} laws", n, m),
    }
}

fn first_impact_line(items: &[TopLevel], decision: &aver::ast::DecisionBlock) -> Option<usize> {
    for impact in &decision.impacts {
        let DecisionImpact::Symbol(symbol) = &impact.node else {
            continue;
        };
        for item in items {
            match item {
                TopLevel::FnDef(fd) if fd.name == *symbol => return Some(fd.line),
                TopLevel::Decision(other) if other.name == *symbol => return Some(other.line),
                TopLevel::Module(module) if module.name == *symbol => return Some(module.line),
                TopLevel::TypeDef(TypeDef::Sum { name, line, .. }) if name == symbol => {
                    return Some(*line);
                }
                TopLevel::TypeDef(TypeDef::Product { name, line, .. }) if name == symbol => {
                    return Some(*line);
                }
                _ => {}
            }
        }
    }
    None
}

fn make_lens(
    uri: &str,
    target_line: usize,
    line: usize,
    title: String,
    message: String,
) -> CodeLens {
    let line = line.saturating_sub(1) as u32;
    CodeLens {
        range: Range {
            start: Position { line, character: 0 },
            end: Position { line, character: 0 },
        },
        command: Some(Command {
            title,
            command: OPEN_LOCATION_COMMAND.to_string(),
            arguments: Some(vec![
                json!(uri),
                json!(target_line.saturating_sub(1)),
                json!(message),
            ]),
        }),
        data: None,
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::code_lenses;

    #[test]
    fn code_lenses_cover_verify_and_decisions() {
        let source = r#"module Demo
    exposes [publicFn]

decision KeepCorePure
    date = "2026-03-10"
    reason =
        "Keep orchestration explicit."
    chosen = "PureCore"
    rejected = ["ImpureCore"]
    impacts = [publicFn]

fn publicFn(x: Int) -> Int
    x + 1

fn publicFnSpec(x: Int) -> Int
    x + 1

verify publicFn
    publicFn(1) => 2

verify publicFn law publicFnSpec
    given x: Int = 0..1
    publicFn(x) => publicFnSpec(x)
"#;

        let lenses = code_lenses(source, None, "file:///demo.av");
        let titles: Vec<&str> = lenses
            .iter()
            .filter_map(|lens| lens.command.as_ref().map(|command| command.title.as_str()))
            .collect();

        assert!(titles.contains(&"verify: 1 case, 1 law"));
        assert!(titles.contains(&"decisions: 1"));
        assert!(titles.contains(&"impacts: 1"));
        let verify = lenses
            .iter()
            .find(|lens| {
                lens.command
                    .as_ref()
                    .is_some_and(|cmd| cmd.title == "verify: 1 case, 1 law")
            })
            .and_then(|lens| lens.command.as_ref())
            .unwrap();
        let args = verify.arguments.as_ref().unwrap();
        assert_eq!(args[0], json!("file:///demo.av"));
        assert_eq!(args[1], json!(17u64));
    }
}
