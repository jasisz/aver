//! Integer division keeps Aver’s Result boundary and Euclidean sign rules.
//! Recursive guidance uses the same checked quotient descent as native Dafny
//! functions; the existence of a division expression alone proves no descent.

use crate::ast::{Expr, FnDef, Spanned, Type, VerifyLaw};
use crate::codegen::CodegenContext;

mod quotient;
#[cfg(test)]
mod tests;

/// Argument types are checked by the caller. This is exactly the syntactic
/// discharge used by typechecking and HIR: a nonzero literal returns Int;
/// zero and dynamic divisors retain Result<Int, String>, including its error.
/// Dafny’s existing lowering implements Euclidean / and % for either sign.
pub(super) fn division_result_type(name: &str, args: &[Spanned<Expr>]) -> Option<Type> {
    if !matches!(name, "Int.div" | "Int.mod") || args.len() != 2 {
        return None;
    }
    Some(if crate::ast::is_literal_nonzero_int_divisor(&args[1]) {
        Type::Int
    } else {
        Type::Result(Box::new(Type::Int), Box::new(Type::Str))
    })
}

/// The contract validates every self-call: a positive source argument shrinks
/// by one fixed literal divisor >= 2. In particular, a zero-only base case
/// that recurses on negative integers does not receive this contract.
pub(super) fn quotient_parameter(fd: &FnDef, ctx: &CodegenContext) -> Option<usize> {
    let contract = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)?;
    let Some(crate::ir::RecursionContract::WellFoundedToNat {
        param,
        floor_div: Some(shrink),
    }) = &contract.recursion
    else {
        return None;
    };
    if shrink.divisor < 2 {
        return None;
    }
    fd.params
        .iter()
        .position(|(name, ty)| name == param && ty == "Int")
}

/// Instantiate the checked step at the source predicate’s recursive arguments.
/// The parent emitter supplies a well-founded Int measure and generalizes the
/// law givens, including accumulators which may grow as the quotient shrinks.
pub(super) fn emit_quotient_calls(
    expr: &Spanned<Expr>,
    law: &VerifyLaw,
    step_name: &str,
    ctx: &CodegenContext,
) -> Option<Vec<String>> {
    quotient::emit(expr, law, step_name, ctx)
}
