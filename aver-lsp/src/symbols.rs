use tower_lsp_server::ls_types::{
    DocumentSymbol, DocumentSymbolResponse, Position, Range, SymbolKind,
};

use aver::ast::{DecisionImpact, TopLevel, TypeDef, VerifyKind};
use aver::checker::merge_verify_blocks;

use crate::completion;

pub fn document_symbols(source: &str) -> Option<DocumentSymbolResponse> {
    let items = completion::parse_items(source);
    if items.is_empty() {
        return None;
    }

    let exposed = items.iter().find_map(|item| {
        if let TopLevel::Module(module) = item {
            if module.exposes.is_empty() {
                None
            } else {
                Some(
                    module
                        .exposes
                        .iter()
                        .map(|name| name.as_str())
                        .collect::<std::collections::HashSet<_>>(),
                )
            }
        } else {
            None
        }
    });
    let verify_by_fn: std::collections::HashMap<String, Vec<_>> = merge_verify_blocks(&items)
        .into_iter()
        .fold(std::collections::HashMap::new(), |mut acc, vb| {
            acc.entry(vb.fn_name.clone()).or_default().push(vb);
            acc
        });

    let symbols = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Module(module) => Some(make_symbol(
                module.name.clone(),
                Some("module".to_string()),
                SymbolKind::MODULE,
                line_range(module.line),
                None,
            )),
            TopLevel::Decision(decision) => Some(make_symbol(
                decision.name.clone(),
                Some(format!("chosen {}", impact_text(&decision.chosen))),
                SymbolKind::OBJECT,
                line_range(decision.line),
                None,
            )),
            TopLevel::TypeDef(TypeDef::Sum {
                name,
                line,
                variants,
            }) => Some(make_symbol(
                name.clone(),
                Some(format!("type {} variants", variants.len())),
                SymbolKind::ENUM,
                line_range(*line),
                None,
            )),
            TopLevel::TypeDef(TypeDef::Product { name, line, fields }) => Some(make_symbol(
                name.clone(),
                Some(format!("record {} fields", fields.len())),
                SymbolKind::STRUCT,
                line_range(*line),
                None,
            )),
            TopLevel::FnDef(fd) => {
                let children = verify_by_fn.get(&fd.name).map(|blocks| {
                    blocks
                        .iter()
                        .map(|vb| {
                            make_symbol(
                                match &vb.kind {
                                    VerifyKind::Cases => format!("verify {}", vb.fn_name),
                                    VerifyKind::Law(law) => format!("law {}", law.name),
                                },
                                Some(match &vb.kind {
                                    VerifyKind::Cases => format!("{} cases", vb.cases.len()),
                                    VerifyKind::Law(law) => format!("{} givens", law.givens.len()),
                                }),
                                SymbolKind::PROPERTY,
                                line_range(vb.line),
                                None,
                            )
                        })
                        .collect()
                });

                let is_internal = exposed
                    .as_ref()
                    .is_some_and(|set| !set.contains(fd.name.as_str()));
                Some(make_symbol(
                    fd.name.clone(),
                    Some(if is_internal {
                        "internal fn".to_string()
                    } else {
                        "fn".to_string()
                    }),
                    SymbolKind::FUNCTION,
                    line_range(fd.line),
                    children,
                ))
            }
            _ => None,
        })
        .collect();

    Some(DocumentSymbolResponse::Nested(symbols))
}

fn line_range(line: usize) -> Range {
    let line = line.saturating_sub(1) as u32;
    Range {
        start: Position { line, character: 0 },
        end: Position { line, character: 0 },
    }
}

fn impact_text(impact: &DecisionImpact) -> &str {
    impact.text()
}

fn make_symbol(
    name: String,
    detail: Option<String>,
    kind: SymbolKind,
    range: Range,
    children: Option<Vec<DocumentSymbol>>,
) -> DocumentSymbol {
    #[allow(deprecated)]
    {
        DocumentSymbol {
            name,
            detail,
            kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children,
        }
    }
}

#[cfg(test)]
mod tests {
    use tower_lsp_server::ls_types::DocumentSymbolResponse;

    use super::document_symbols;

    #[test]
    fn document_symbols_group_verify_under_functions() {
        let source = r#"module Demo
    exposes [publicFn]

decision KeepCorePure
    date = "2026-03-10"
    reason =
        "test"
    chosen = "PureCore"
    rejected = ["ImpureCore"]
    impacts = [publicFn]

fn publicFn(x: Int) -> Int
    x + 1

verify publicFn
    publicFn(1) => 2

verify publicFn law grows
    given x: Int = 0..1
    publicFn(x) => x + 1
"#;

        let Some(DocumentSymbolResponse::Nested(symbols)) = document_symbols(source) else {
            panic!("expected document symbols");
        };

        assert!(symbols.iter().any(|s| s.name == "Demo"));
        assert!(symbols.iter().any(|s| s.name == "KeepCorePure"));
        let function = symbols.iter().find(|s| s.name == "publicFn").unwrap();
        let children = function.children.as_ref().unwrap();
        assert!(children.iter().any(|s| s.name == "verify publicFn"));
        assert!(children.iter().any(|s| s.name == "law grows"));
    }
}
