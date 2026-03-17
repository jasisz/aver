mod coverage;
mod intent;
mod law;
mod verify;

use crate::ast::{Expr, Literal, Pattern, TopLevel, TypeDef, VerifyBlock, VerifyKind};

pub struct VerifyResult {
    #[allow(dead_code)]
    pub fn_name: String,
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
    #[allow(dead_code)]
    pub failures: Vec<(String, String, String)>, // (expr_src, expected, actual)
}

pub struct ModuleCheckFindings {
    pub errors: Vec<CheckFinding>,
    pub warnings: Vec<CheckFinding>,
}

pub(crate) type FnSigSummary = (Vec<crate::types::Type>, crate::types::Type, Vec<String>);
pub(crate) type FnSigMap = std::collections::HashMap<String, FnSigSummary>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckFinding {
    pub line: usize,
    pub module: Option<String>,
    pub file: Option<String>,
    pub message: String,
}

fn module_name_for_items(items: &[TopLevel]) -> Option<String> {
    items.iter().find_map(|item| {
        if let TopLevel::Module(m) = item {
            Some(m.name.clone())
        } else {
            None
        }
    })
}

fn dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => {
            let mut prefix = dotted_name(base)?;
            prefix.push('.');
            prefix.push_str(field);
            Some(prefix)
        }
        _ => None,
    }
}

fn normalize_constructor_tag(path: &str) -> Option<String> {
    let mut parts = path.split('.').collect::<Vec<_>>();
    if parts.len() < 2 {
        return None;
    }
    let variant = parts.pop()?;
    let type_name = parts.pop()?;
    Some(format!("{}.{}", type_name, variant))
}

fn constructor_tag_from_pattern(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Constructor(path, _) => normalize_constructor_tag(path),
        _ => None,
    }
}

fn constructor_tag_from_expr(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Attr(_, _) => normalize_constructor_tag(&dotted_name(expr)?),
        Expr::FnCall(callee, _) => normalize_constructor_tag(&dotted_name(callee)?),
        Expr::Constructor(name, _) => normalize_constructor_tag(name),
        _ => None,
    }
}

fn expr_is_result_err_case(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Result.Err"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Result.Err")
        }
        _ => false,
    }
}

fn expr_is_result_ok_case(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Result.Ok"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Result.Ok")
        }
        _ => false,
    }
}

fn expr_is_option_none_case(expr: &Expr) -> bool {
    match expr {
        Expr::Attr(_, _) => dotted_name(expr)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Option.None"),
        Expr::Constructor(name, None) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Option.None")
        }
        _ => false,
    }
}

fn expr_is_option_some_case(expr: &Expr) -> bool {
    match expr {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Option.Some"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Option.Some")
        }
        _ => false,
    }
}

fn expr_is_bool_case(expr: &Expr, expected: bool) -> bool {
    matches!(expr, Expr::Literal(Literal::Bool(value)) if *value == expected)
}

fn expr_is_empty_list_case(expr: &Expr) -> bool {
    matches!(expr, Expr::List(items) if items.is_empty())
}

fn expr_is_non_empty_list_case(expr: &Expr) -> bool {
    matches!(expr, Expr::List(items) if !items.is_empty())
}

fn expr_is_empty_string_case(expr: &Expr) -> bool {
    matches!(expr, Expr::Literal(Literal::Str(value)) if value.is_empty())
}

fn expr_is_int_literal_case(expr: &Expr, expected: i64) -> bool {
    matches!(expr, Expr::Literal(Literal::Int(value)) if *value == expected)
}

fn verify_cases_block_is_well_formed(block: &VerifyBlock) -> bool {
    matches!(block.kind, VerifyKind::Cases)
        && !block.cases.is_empty()
        && block.cases.iter().all(|(left, right)| {
            verify_case_calls_target(left, &block.fn_name)
                && !verify_case_calls_target(right, &block.fn_name)
        })
}

fn local_sum_type_constructors(items: &[TopLevel], type_name: &str) -> Option<Vec<String>> {
    items.iter().find_map(|item| match item {
        TopLevel::TypeDef(TypeDef::Sum { name, variants, .. }) if name == type_name => Some(
            variants
                .iter()
                .map(|variant| format!("{name}.{}", variant.name))
                .collect(),
        ),
        _ => None,
    })
}

fn callee_is_target(callee: &Expr, fn_name: &str) -> bool {
    matches!(callee, Expr::Ident(name) if name == fn_name)
}

// Re-export from verify submodule
use verify::collect_target_call_args;
use verify::verify_case_calls_target;

// Public re-exports so external callers don't break
pub use coverage::{collect_verify_coverage_warnings, collect_verify_coverage_warnings_in};
pub use intent::{
    check_module_intent, check_module_intent_with_sigs, check_module_intent_with_sigs_in,
    index_decisions,
};
pub use law::{collect_verify_law_dependency_warnings, collect_verify_law_dependency_warnings_in};
pub use verify::{expr_to_str, merge_verify_blocks, run_verify};
