mod coverage;
mod coverage_flow;
mod cse;
mod independence;
mod intent;
#[cfg(feature = "runtime")]
mod law;
mod module_effects;
mod naming;
mod perf;
mod traversal;
mod verify;

use crate::ast::{
    Expr, Literal, Pattern, SourceSpan, Spanned, TopLevel, TypeDef, VerifyBlock, VerifyKind,
};

// -- Structured verify results ------------------------------------------------

#[derive(Debug, Clone)]
pub enum VerifyCaseOutcome {
    Pass,
    Skipped,
    /// Hostile-profile case for a `case_expr` whose un-effected base
    /// case already failed. Aver doesn't run the VM for these — the
    /// counter-example is the base failure itself; the per-profile
    /// follow-ups would only re-confirm the same case under harder
    /// worlds. Distinct from `Skipped` (which is `when`-driven and
    /// drives the vacuous-under-hostile warning).
    SkippedAfterBaseFail,
    /// The case was not answered.
    ///
    /// A third outcome, deliberately neither `Pass` nor a failure: the engine
    /// ran out of the budget it was given before the case produced a value,
    /// so nothing was observed — not agreement, and not a counter-example.
    /// Counted on its own, reported with its reason, and it fails the run,
    /// because "we did not check this" must never read as "this checks out".
    Declined {
        /// One sentence, user-facing: why we did not answer.
        reason: String,
        /// Work the case consumed before the budget stopped it: VM opcodes on
        /// the VM lane, wasmtime fuel on the wasm-gc lane.
        steps: u64,
        /// The budget in force for this case.
        limit: u64,
        /// `Some(fn)` when an `aver.toml` `[[verify.costly]]` entry raised the
        /// budget above the project default and it still was not enough.
        raised_by: Option<String>,
    },
    Mismatch {
        expected: String,
        actual: String,
    },
    RuntimeError {
        error: String,
    },
    UnexpectedErr {
        err_repr: String,
    },
}

#[derive(Debug, Clone)]
pub struct VerifyCaseResult {
    pub outcome: VerifyCaseOutcome,
    pub span: Option<SourceSpan>,
    pub case_expr: String,
    pub case_index: usize,
    pub case_total: usize,
    pub law_context: Option<VerifyLawContext>,
    /// `true` for cases injected by `aver verify --hostile` boundary
    /// expansion (a binding the user did not declare). Drives differential
    /// reporting: a hostile-only failure means the claim is not universal,
    /// so it isn't a law — either encode the missing precondition with
    /// `when`, or downgrade from `law` form to `verify` (cases form,
    /// example/scenario semantics) with the values you actually meant.
    pub from_hostile: bool,
    /// Display label for the effect-side hostile profile, e.g.
    /// `"Time.now/frozen + Random.int/min"`. `None` when the case wasn't
    /// effect-hostile-expanded (declared, value-hostile-only, or fns
    /// without applicable classified effects). Reporting prepends this to
    /// the diagnostic so the user sees which adversarial world broke the
    /// law: "Time.now/frozen + Random.int/min: assumed deadline > now".
    pub hostile_profile: Option<String>,
    /// VM-computed ground-truth value of the case's EXPECTED (right) side,
    /// recorded on `Pass` by the VM verify runner. Proof export consumes it
    /// to literalize the expected side of bounded Lean checks
    /// (model-vs-ground-truth instead of model-vs-model, which is vacuously
    /// true when fuel exhaustion collapses both sides to `default`). `None`
    /// for non-`Pass` outcomes and for runners that don't compute values
    /// (wasm-gc differential verify).
    pub expected_value: Option<crate::value::Value>,
    /// Work this case cost: VM opcodes dispatched on the VM lane, wasmtime
    /// fuel consumed on the wasm-gc lane, `0` where nothing ran (a skipped
    /// case) or where the runner does not measure. Reporting uses it to show
    /// what a raised `[[verify.costly]]` budget actually bought.
    pub steps: u64,
}

/// The per-case step budget a verify block's cases ran under.
///
/// Carried on the result so the report can say what a raised budget bought:
/// the cases that needed more than `default_limit` are exactly the ones that
/// would have been declined without the `[[verify.costly]]` entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyBudgetInfo {
    /// Budget in force for each case of the block.
    pub limit: u64,
    /// The project default `limit` was raised from, equal to `limit` when
    /// nothing raised it.
    pub default_limit: u64,
    /// `Some(fn)` when an `aver.toml` `[[verify.costly]]` entry raised it.
    pub raised_by: Option<String>,
}

impl Default for VerifyBudgetInfo {
    fn default() -> Self {
        VerifyBudgetInfo {
            limit: crate::config::DEFAULT_VERIFY_STEP_LIMIT,
            default_limit: crate::config::DEFAULT_VERIFY_STEP_LIMIT,
            raised_by: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VerifyLawContext {
    pub givens: Vec<(String, String)>, // (name, value_repr)
    pub law_expr: String,
}

pub struct VerifyResult {
    pub fn_name: String,
    /// True for `verify ... law ...` blocks. Carried as a field so
    /// consumers never derive law-ness from the rendered label string.
    pub is_law: bool,
    pub block_label: String, // "add" or "sort law isSorted"
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
    /// Cases that exceeded their step budget. Never folded into `failed`:
    /// a decline says the block was not checked, not that it is wrong.
    pub declined: usize,
    /// The budget these cases ran under.
    pub budget: VerifyBudgetInfo,
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

/// True when the left side of a verify case applies `?` directly to a call of
/// the function under verification, however that `?` is wrapped —
/// `readOne([7, 9])?.value => 7` or `List.len(readOne(xs)?.rest) => 1`.
///
/// Such a case establishes `Result.Ok` at least as strongly as an explicit
/// `=> Result.Ok(...)`: had the call produced an error, `?` would have
/// propagated it and the case would not pass. The `?` must sit on a call of
/// the target itself — `helper(other(x)?)` says nothing about `helper`.
fn verify_case_unwraps_target(expr: &Spanned<Expr>, fn_name: &str) -> bool {
    let mut found = false;
    crate::call_graph::walk_expr(expr, &mut |node| {
        if let Expr::ErrorProp(inner) = node
            && expr_is_target_call(inner, fn_name)
        {
            found = true;
        }
    });
    found
}

fn expr_is_target_call(expr: &Spanned<Expr>, fn_name: &str) -> bool {
    match &expr.node {
        Expr::FnCall(callee, _) => callee_is_target(callee, fn_name),
        Expr::TailCall(boxed) => boxed.target == fn_name,
        _ => false,
    }
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
pub use module_effects::{collect_module_effects_warnings, collect_module_effects_warnings_in};
pub use naming::{collect_naming_warnings, collect_naming_warnings_in};
pub use perf::{collect_perf_warnings, collect_perf_warnings_in};
pub use traversal::collect_traversal_warnings_in;
pub use verify::{expr_to_str, merge_verify_blocks};
