//! Build `ProofIR` from a `CodegenContext`.
//!
//! The lowering producer side of the Step 1 / Step 2 split: types
//! live in `src/ir/proof_ir.rs`, the function that fills them in
//! from a typechecked + analysed codegen context lives here. Output
//! lands in `CodegenContext.proof_ir`; both proof backends read
//! from the same field, so any classifier-side decision flows
//! consistently to Lean and Dafny without each backend re-running
//! shape detection.
//!
//! **Step 2 scope**: this commit only populates
//! `ProofIR.refined_types` — refinement-via-opaque records lifted
//! to subtype on Lean / subset type on Dafny. `fn_contracts` and
//! `law_theorems` are intentionally left empty; backends still go
//! through the legacy `codegen::recursion::RecursionPlan` and the
//! ad-hoc law-lowering path in `lean::toplevel`. Step 3 onwards
//! migrates backends to read from ProofIR one feature at a time.
//!
//! A diff test (`tests/proof_ir_diff.rs`) asserts the new producer
//! agrees with the legacy `refinement_info_for` + Dafny
//! `refinement_witness_for` walk on every flagship refinement
//! example — once the test is stable the legacy walkers become dead
//! code in their consumers (Step 3 / Step 4 deletes them).

use crate::ast::{Expr, Literal, Spanned, TopLevel, TypeDef};
use crate::codegen::CodegenContext;
use crate::codegen::common::{expr_to_dotted_name, refinement_info_for};
use crate::ir::proof_ir::{Predicate, ProofIR, RefinedTypeDecl};

/// Walk every type definition in `ctx` (entry items + dependent
/// modules), classify the refinement-via-opaque ones, and build a
/// `ProofIR` carrying their decided shapes. Pure function — never
/// reads side effects, never mutates `ctx`.
pub fn lower(ctx: &CodegenContext) -> ProofIR {
    let mut ir = ProofIR::default();
    populate_refined_types(ctx, &mut ir);
    ir
}

fn populate_refined_types(ctx: &CodegenContext, ir: &mut ProofIR) {
    // Walk entry items first, then dep modules. Both feed into the
    // same map keyed by bare type name — consumers (Lean / Dafny
    // emit paths) always query by bare name because that's what
    // they see in the AST nodes (`Expr::RecordCreate { type_name:
    // "Natural", ... }`, `TypeDef::Product { name: "Natural", ... }`).
    // Aver's module DAG invariant + typechecker's duplicate-type
    // rejection (PR #89) make name collisions a compile error, so
    // bare-name keying is safe.
    let entry_typedefs = ctx.items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    });
    let module_typedefs = ctx.modules.iter().flat_map(|m| m.type_defs.iter());

    for td in entry_typedefs.chain(module_typedefs) {
        let TypeDef::Product { name, fields, .. } = td else {
            continue;
        };
        if fields.len() != 1 {
            continue;
        }
        if ir.refined_types.contains_key(name) {
            // Already classified via another path (typically the
            // entry walk picked it up first); skip the dep-module
            // duplicate so we don't overwrite a verified-witness
            // entry with a predicate-eval fallback witness.
            continue;
        }
        let Some(info) = refinement_info_for(name, ctx) else {
            continue;
        };
        let invariant = Predicate {
            free_vars: vec![(
                info.param_name.to_string(),
                crate::ir::proof_ir::QuantifierType::Plain(info.carrier_type.to_string()),
            )],
            expr: info.predicate.clone(),
        };
        let dafny_witness = pick_dafny_witness(name, ctx, info.predicate, info.param_name);
        ir.refined_types.insert(
            name.clone(),
            RefinedTypeDecl {
                name: name.clone(),
                carrier_type: info.carrier_type.to_string(),
                carrier_field: info.carrier_field.to_string(),
                predicate_param: info.param_name.to_string(),
                invariant,
                dafny_witness,
            },
        );
    }
}

/// Pick a Dafny `witness` value that provably satisfies the
/// refinement predicate. First tries the smart constructor's
/// verify-block samples (entry-module only — `ModuleInfo` doesn't
/// surface verify blocks); falls back to evaluating the predicate
/// against `[0, 1, -1]` and returning the first satisfier.
/// Mirrors the existing logic in `dafny/toplevel.rs::refinement_
/// witness_for`; Step 4 will retire that copy.
fn pick_dafny_witness(
    type_name: &str,
    ctx: &CodegenContext,
    predicate: &Spanned<Expr>,
    param_name: &str,
) -> Option<String> {
    let smart_ctor_name = ctx.items.iter().find_map(|item| match item {
        TopLevel::FnDef(fd)
            if fd.return_type.starts_with("Result<")
                && fd.return_type[7..].starts_with(type_name)
                && fd.params.len() == 1 =>
        {
            Some(fd.name.clone())
        }
        _ => None,
    });
    if let Some(smart_ctor_name) = smart_ctor_name {
        for item in &ctx.items {
            let TopLevel::Verify(vb) = item else {
                continue;
            };
            if vb.fn_name != smart_ctor_name {
                continue;
            }
            for (lhs, rhs) in &vb.cases {
                if !is_result_ok(&rhs.node) {
                    continue;
                }
                let Expr::FnCall(_, args) = &lhs.node else {
                    continue;
                };
                if args.len() != 1 {
                    continue;
                }
                if let Some(lit) = literal_int_value(&args[0]) {
                    return Some(lit);
                }
            }
        }
    }
    for candidate in [0i64, 1, -1] {
        if eval_int_bool_predicate(predicate, param_name, candidate) == Some(true) {
            return Some(candidate.to_string());
        }
    }
    None
}

fn is_result_ok(expr: &Expr) -> bool {
    match expr {
        Expr::Constructor(name, _) => name == "Result.Ok",
        Expr::FnCall(callee, _) => matches!(
            &callee.node,
            Expr::Attr(obj, field)
                if field == "Ok" && matches!(&obj.node, Expr::Ident(n) if n == "Result")
        ),
        _ => false,
    }
}

fn literal_int_value(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(n.to_string()),
        Expr::Neg(inner) => {
            let inner_str = literal_int_value(inner)?;
            Some(format!("-{inner_str}"))
        }
        _ => None,
    }
}

fn eval_int_bool_predicate(expr: &Spanned<Expr>, param_name: &str, value: i64) -> Option<bool> {
    match &expr.node {
        Expr::Literal(Literal::Bool(b)) => Some(*b),
        Expr::BinOp(op, l, r) => {
            use crate::ast::BinOp::*;
            let li = eval_int_arith(l, param_name, value)?;
            let ri = eval_int_arith(r, param_name, value)?;
            Some(match op {
                Lt => li < ri,
                Gt => li > ri,
                Lte => li <= ri,
                Gte => li >= ri,
                Eq => li == ri,
                Neq => li != ri,
                _ => return None,
            })
        }
        Expr::FnCall(callee, args) if args.len() == 2 => {
            let name = expr_to_dotted_name(&callee.node)?;
            match name.as_str() {
                "Bool.and" => Some(
                    eval_int_bool_predicate(&args[0], param_name, value)?
                        && eval_int_bool_predicate(&args[1], param_name, value)?,
                ),
                "Bool.or" => Some(
                    eval_int_bool_predicate(&args[0], param_name, value)?
                        || eval_int_bool_predicate(&args[1], param_name, value)?,
                ),
                _ => None,
            }
        }
        _ => None,
    }
}

fn eval_int_arith(expr: &Spanned<Expr>, param_name: &str, value: i64) -> Option<i64> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        Expr::Ident(name) | Expr::Resolved { name, .. } if name == param_name => Some(value),
        Expr::BinOp(op, l, r) => {
            use crate::ast::BinOp::*;
            let li = eval_int_arith(l, param_name, value)?;
            let ri = eval_int_arith(r, param_name, value)?;
            match op {
                Add => Some(li.checked_add(ri)?),
                Sub => Some(li.checked_sub(ri)?),
                Mul => Some(li.checked_mul(ri)?),
                Div => Some(li.checked_div(ri)?),
                _ => None,
            }
        }
        Expr::Neg(inner) => Some(-eval_int_arith(inner, param_name, value)?),
        _ => None,
    }
}
