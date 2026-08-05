use crate::ast::TopLevel;
use crate::verify_law::{
    collect_contextual_helper_law_hints, collect_missing_helper_law_hints,
    contextual_helper_law_message, missing_helper_law_message,
};

use super::{CheckFinding, FnSigMap, module_name_for_items};

pub fn collect_verify_law_dependency_warnings(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
) -> Vec<CheckFinding> {
    collect_verify_law_dependency_warnings_in(items, fn_sigs, None)
}

pub fn collect_verify_law_dependency_warnings_in(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
    source_file: Option<&str>,
) -> Vec<CheckFinding> {
    let module_name = module_name_for_items(items);
    let mut findings = collect_missing_helper_law_hints(items, fn_sigs)
        .into_iter()
        .map(|hint| CheckFinding {
            line: hint.line,
            module: module_name.clone(),
            file: source_file.map(|s| s.to_string()),
            fn_name: None,
            message: missing_helper_law_message(&hint),
            extra_spans: vec![],
        })
        .collect::<Vec<_>>();
    findings.extend(
        collect_contextual_helper_law_hints(items, fn_sigs)
            .into_iter()
            .map(|hint| CheckFinding {
                line: hint.line,
                module: module_name.clone(),
                file: source_file.map(|s| s.to_string()),
                fn_name: None,
                message: contextual_helper_law_message(&hint),
                extra_spans: vec![],
            }),
    );
    findings
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_items(src: &str) -> Vec<TopLevel> {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parse failed")
    }

    #[test]
    fn law_dependency_warning_points_at_missing_helper_laws_in_json() {
        let items = parse_items(include_str!("../../examples/data/json.av"));
        let tc = crate::ir::pipeline::typecheck(
            &items,
            &crate::ir::TypecheckMode::Full {
                base_dir: Some(env!("CARGO_MANIFEST_DIR")),
            },
        );
        assert!(
            tc.errors.is_empty(),
            "expected json example to type-check, got {:?}",
            tc.errors
        );

        let warnings = collect_verify_law_dependency_warnings(&items, &tc.fn_sigs);
        assert!(
            warnings.is_empty(),
            "expected json helper-law ladder to be complete, got {:?}",
            warnings
        );
    }
}
