//! Phase 6 wave 7 — nullary literal inlining.
//!
//! For every fn whose body is exactly a `MirExpr::Literal` and
//! whose param list is empty, rewrite every `Call(Fn(id), [])`
//! to that literal value. Smallest non-trivial inliner — no
//! param substitution needed — and it unlocks further
//! const-fold / DCE cascades.
//!
//! Cycle safety: a literal-only body has no `MirExpr::Call`, so
//! a "nullary literal fn that calls itself" is structurally
//! impossible — no SCC analysis needed.

use std::collections::HashMap;

use crate::ast::{Literal, Spanned};
use crate::ir::FnId;

use super::super::expr::{MirCallee, MirExpr};
use super::super::program::MirProgram;

pub fn inline_nullary_literals(mut program: MirProgram) -> MirProgram {
    let candidates = collect_nullary_literal_fns(&program);
    if candidates.is_empty() {
        return program;
    }
    for mir_fn in program.fns.values_mut() {
        inline_in_place(&mut mir_fn.body, &candidates);
    }
    program
}

fn collect_nullary_literal_fns(program: &MirProgram) -> HashMap<FnId, Literal> {
    let mut out = HashMap::new();
    for (fn_id, mir_fn) in program.iter() {
        if !mir_fn.params.is_empty() {
            continue;
        }
        if let MirExpr::Literal(spanned_lit) = &mir_fn.body.node {
            out.insert(*fn_id, spanned_lit.node.clone());
        }
    }
    out
}

fn inline_in_place(expr: &mut Spanned<MirExpr>, candidates: &HashMap<FnId, Literal>) {
    inline_walk_children(&mut expr.node, candidates);

    let replacement = if let MirExpr::Call(spanned_call) = &expr.node {
        let call = &spanned_call.node;
        if call.args.is_empty() {
            if let MirCallee::Fn(fn_id) = &call.callee {
                candidates.get(fn_id).cloned()
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    };

    if let Some(lit) = replacement {
        let ty = std::sync::OnceLock::new();
        if let Some(t) = expr.ty() {
            let _ = ty.set(t.clone());
        }
        expr.node = MirExpr::Literal(Spanned {
            node: lit,
            line: expr.line,
            ty,
        });
    }
}

fn inline_walk_children(node: &mut MirExpr, candidates: &HashMap<FnId, Literal>) {
    match node {
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
        MirExpr::Neg(inner) => inline_in_place(inner, candidates),
        MirExpr::BinOp(spanned_bop) => {
            inline_in_place(&mut spanned_bop.node.lhs, candidates);
            inline_in_place(&mut spanned_bop.node.rhs, candidates);
        }
        MirExpr::Let(spanned_let) => {
            inline_in_place(&mut spanned_let.node.value, candidates);
            inline_in_place(&mut spanned_let.node.body, candidates);
        }
        MirExpr::Call(spanned_call) => {
            for arg in &mut spanned_call.node.args {
                inline_in_place(arg, candidates);
            }
        }
        MirExpr::TailCall(spanned_tc) => {
            for arg in &mut spanned_tc.node.args {
                inline_in_place(arg, candidates);
            }
        }
        MirExpr::Match(spanned_match) => {
            inline_in_place(&mut spanned_match.node.subject, candidates);
            for arm in &mut spanned_match.node.arms {
                inline_in_place(&mut arm.body, candidates);
            }
        }
        MirExpr::IfThenElse(spanned_ite) => {
            inline_in_place(&mut spanned_ite.node.cond, candidates);
            inline_in_place(&mut spanned_ite.node.then_branch, candidates);
            inline_in_place(&mut spanned_ite.node.else_branch, candidates);
        }
        MirExpr::Construct(spanned_ctor) => {
            for arg in &mut spanned_ctor.node.args {
                inline_in_place(arg, candidates);
            }
        }
        MirExpr::RecordCreate(spanned_rec) => {
            for f in &mut spanned_rec.node.fields {
                inline_in_place(&mut f.value, candidates);
            }
        }
        MirExpr::RecordUpdate(spanned_upd) => {
            inline_in_place(&mut spanned_upd.node.base, candidates);
            for f in &mut spanned_upd.node.updates {
                inline_in_place(&mut f.value, candidates);
            }
        }
        MirExpr::Project(spanned_proj) => inline_in_place(&mut spanned_proj.node.base, candidates),
        MirExpr::Try(inner)
        | MirExpr::Return(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => inline_in_place(inner, candidates),
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for item in items {
                inline_in_place(item, candidates);
            }
        }
        MirExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                inline_in_place(k, candidates);
                inline_in_place(v, candidates);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let super::super::expr::MirStrPart::Expr(e) = part {
                    inline_in_place(e, candidates);
                }
            }
        }
        MirExpr::IndependentProduct(spanned_ip) => {
            for item in &mut spanned_ip.node.items {
                inline_in_place(item, candidates);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::expr::{MirBinOp, MirCall, MirLet};
    use super::super::super::program::{LocalId, MirFn, MirParam};
    use super::super::const_fold::const_fold;
    use super::super::dead_code::dead_code;
    use super::super::test_helpers::span;
    use super::*;
    use crate::ast::BinOp;

    fn two_fn_program(callee_body: MirExpr, caller_body: MirExpr) -> MirProgram {
        let mut p = MirProgram::empty();
        p.fns.insert(
            FnId(0),
            MirFn {
                fn_id: FnId(0),
                name: "callee".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(callee_body),
                local_count: 0,
                aliased_slots: std::sync::Arc::new(Vec::new()),
                repr: crate::ir::mir::program::MirFnRepr::default(),
            },
        );
        p.fns.insert(
            FnId(1),
            MirFn {
                fn_id: FnId(1),
                name: "caller".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(caller_body),
                local_count: 0,
                aliased_slots: std::sync::Arc::new(Vec::new()),
                repr: crate::ir::mir::program::MirFnRepr::default(),
            },
        );
        p
    }

    fn caller_body(p: &MirProgram) -> &MirExpr {
        &p.fns.get(&FnId(1)).unwrap().body.node
    }

    #[test]
    fn inlines_nullary_literal_call() {
        let callee_body = MirExpr::Literal(span(Literal::Int(42)));
        let caller_body_expr = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let inlined = inline_nullary_literals(two_fn_program(callee_body, caller_body_expr));
        assert!(
            matches!(caller_body(&inlined), MirExpr::Literal(s) if matches!(s.node, Literal::Int(42))),
            "nullary literal call should inline to the literal"
        );
    }

    #[test]
    fn does_not_inline_non_nullary_call() {
        let mut p = MirProgram::empty();
        p.fns.insert(
            FnId(0),
            MirFn {
                fn_id: FnId(0),
                name: "callee".to_string(),
                params: vec![MirParam {
                    local: LocalId(0),
                    name: "x".to_string(),
                    ty: "Int".to_string(),
                }],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(MirExpr::Literal(span(Literal::Int(42)))),
                local_count: 1,
                aliased_slots: std::sync::Arc::new(Vec::new()),
                repr: crate::ir::mir::program::MirFnRepr::default(),
            },
        );
        let caller_body_expr = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![span(MirExpr::Literal(span(Literal::Int(1))))],
        }));
        p.fns.insert(
            FnId(1),
            MirFn {
                fn_id: FnId(1),
                name: "caller".to_string(),
                params: vec![],
                return_type: "Int".to_string(),
                effects: vec![],
                body: span(caller_body_expr),
                local_count: 0,
                aliased_slots: std::sync::Arc::new(Vec::new()),
                repr: crate::ir::mir::program::MirFnRepr::default(),
            },
        );
        let inlined = inline_nullary_literals(p);
        assert!(
            matches!(caller_body(&inlined), MirExpr::Call(_)),
            "non-nullary call must not be inlined even if body is literal"
        );
    }

    #[test]
    fn does_not_inline_nullary_non_literal_body() {
        let callee_body = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(span(MirExpr::Literal(span(Literal::Int(1))))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let caller_body_expr = MirExpr::Call(span(MirCall {
            callee: MirCallee::Fn(FnId(0)),
            args: vec![],
        }));
        let inlined = inline_nullary_literals(two_fn_program(callee_body, caller_body_expr));
        assert!(
            matches!(caller_body(&inlined), MirExpr::Call(_)),
            "nullary call with BinOp body must not be inlined directly"
        );
    }

    #[test]
    fn pipeline_inline_then_fold_then_dce() {
        let callee_body = MirExpr::Literal(span(Literal::Int(3)));
        let mul = MirExpr::BinOp(span(MirBinOp {
            op: BinOp::Mul,
            lhs: Box::new(span(MirExpr::Call(span(MirCall {
                callee: MirCallee::Fn(FnId(0)),
                args: vec![],
            })))),
            rhs: Box::new(span(MirExpr::Literal(span(Literal::Int(2))))),
        }));
        let caller_body_expr = MirExpr::Let(span(MirLet {
            binding: LocalId(0),
            binding_name: "x".to_string(),
            value: Box::new(span(mul)),
            body: Box::new(span(MirExpr::Literal(span(Literal::Int(99))))),
        }));
        let p = two_fn_program(callee_body, caller_body_expr);
        let optimized = dead_code(const_fold(inline_nullary_literals(p)));
        assert!(
            matches!(caller_body(&optimized), MirExpr::Literal(s) if matches!(s.node, Literal::Int(99))),
            "full pipeline should collapse caller body to `99`"
        );
    }
}
