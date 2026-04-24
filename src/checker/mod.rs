mod coverage;
mod cse;
mod independence;
mod intent;
#[cfg(feature = "runtime")]
mod law;
mod naming;
mod perf;
mod verify;
mod verify_effects;

use crate::ast::{
    Expr, Literal, Pattern, SourceSpan, Spanned, TopLevel, TypeDef, VerifyBlock, VerifyKind,
};

// -- Structured verify results ------------------------------------------------

#[derive(Debug, Clone)]
pub enum VerifyCaseOutcome {
    Pass,
    Skipped,
    Mismatch { expected: String, actual: String },
    RuntimeError { error: String },
    UnexpectedErr { err_repr: String },
}

#[derive(Debug, Clone)]
pub struct VerifyCaseResult {
    pub outcome: VerifyCaseOutcome,
    pub span: Option<SourceSpan>,
    pub case_expr: String,
    pub case_index: usize,
    pub case_total: usize,
    pub law_context: Option<VerifyLawContext>,
}

#[derive(Debug, Clone)]
pub struct VerifyLawContext {
    pub givens: Vec<(String, String)>, // (name, value_repr)
    pub law_expr: String,
}

pub struct VerifyResult {
    pub fn_name: String,
    pub block_label: String, // "add" or "sort spec isSorted"
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
    pub case_results: Vec<VerifyCaseResult>,
    // Legacy field — kept temporarily for existing consumers
    pub failures: Vec<(String, String, String)>, // (expr_src, expected, actual)
}

pub struct ModuleCheckFindings {
    pub errors: Vec<CheckFinding>,
    pub warnings: Vec<CheckFinding>,
}

pub(crate) type FnSigSummary = (Vec<crate::types::Type>, crate::types::Type, Vec<String>);
pub(crate) type FnSigMap = std::collections::HashMap<String, FnSigSummary>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FindingSpan {
    pub line: usize,
    pub col: usize,
    pub len: usize,
    pub label: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckFinding {
    pub line: usize,
    pub module: Option<String>,
    pub file: Option<String>,
    pub fn_name: Option<String>,
    pub message: String,
    pub extra_spans: Vec<FindingSpan>,
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

fn dotted_name(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
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
    Some(crate::visibility::member_key(type_name, variant))
}

fn constructor_tag_from_pattern(pattern: &Pattern) -> Option<String> {
    match pattern {
        Pattern::Constructor(path, _) => normalize_constructor_tag(path),
        _ => None,
    }
}

fn constructor_tag_from_expr(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Attr(_, _) => normalize_constructor_tag(&dotted_name(expr)?),
        Expr::FnCall(callee, _) => normalize_constructor_tag(&dotted_name(callee)?),
        Expr::Constructor(name, _) => normalize_constructor_tag(name),
        _ => None,
    }
}

fn expr_is_result_err_case(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Result.Err"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Result.Err")
        }
        _ => false,
    }
}

fn expr_is_result_ok_case(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Result.Ok"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Result.Ok")
        }
        _ => false,
    }
}

fn expr_is_option_none_case(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Attr(_, _) => dotted_name(expr)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Option.None"),
        Expr::Constructor(name, None) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Option.None")
        }
        _ => false,
    }
}

fn expr_is_option_some_case(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::FnCall(callee, _) => dotted_name(callee)
            .and_then(|path| normalize_constructor_tag(&path))
            .is_some_and(|tag| tag == "Option.Some"),
        Expr::Constructor(name, _) => {
            normalize_constructor_tag(name).is_some_and(|tag| tag == "Option.Some")
        }
        _ => false,
    }
}

fn expr_is_bool_case(expr: &Spanned<Expr>, expected: bool) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Bool(value)) if *value == expected)
}

fn expr_is_empty_list_case(expr: &Spanned<Expr>) -> bool {
    matches!(&expr.node, Expr::List(items) if items.is_empty())
}

fn expr_is_non_empty_list_case(expr: &Spanned<Expr>) -> bool {
    matches!(&expr.node, Expr::List(items) if !items.is_empty())
}

fn expr_is_empty_string_case(expr: &Spanned<Expr>) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Str(value)) if value.is_empty())
}

fn expr_is_int_literal_case(expr: &Spanned<Expr>, expected: i64) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Int(value)) if *value == expected)
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
                .map(|variant| crate::visibility::member_key(name, &variant.name))
                .collect(),
        ),
        _ => None,
    })
}

fn callee_is_target(callee: &Spanned<Expr>, fn_name: &str) -> bool {
    matches!(&callee.node, Expr::Ident(name) if name == fn_name)
}

// Re-export from verify submodule
use verify::collect_target_call_args;
use verify::verify_case_calls_target;

// Public re-exports so external callers don't break
pub use coverage::{collect_verify_coverage_warnings, collect_verify_coverage_warnings_in};
pub use cse::{collect_cse_warnings, collect_cse_warnings_in};
pub use independence::{collect_independence_warnings, collect_independence_warnings_in};
pub use intent::{
    check_module_intent, check_module_intent_with_sigs, check_module_intent_with_sigs_in,
    index_decisions,
};
#[cfg(feature = "runtime")]
pub use law::{collect_verify_law_dependency_warnings, collect_verify_law_dependency_warnings_in};
pub use naming::{collect_naming_warnings, collect_naming_warnings_in};
pub use perf::{collect_perf_warnings, collect_perf_warnings_in};
pub use verify::{expr_to_str, merge_verify_blocks};
pub use verify_effects::{
    collect_plain_cases_effectful_warnings, collect_plain_cases_effectful_warnings_in,
};
