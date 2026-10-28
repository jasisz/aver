//! The expressions supplying a structured MIR expression's normal result.
//!
//! `Let` supplies its body's value; each match arm or conditional branch can
//! supply a value. Initializers, subjects, conditions and operands are not
//! result positions. An explicit `Return`, `Try` or `TailCall` stays a leaf:
//! consumers decide how its control transfer constrains their own analysis.
//! Returning the original spans also lets an emitter identify these exact
//! nodes without rebuilding or annotating the MIR.

use crate::ast::Spanned;

use super::MirExpr;

pub fn collect<'a>(expr: &'a Spanned<MirExpr>, out: &mut Vec<&'a Spanned<MirExpr>>) {
    match &expr.node {
        MirExpr::Let(l) => collect(&l.node.body, out),
        MirExpr::Match(m) => {
            for arm in &m.node.arms {
                collect(&arm.body, out);
            }
        }
        MirExpr::IfThenElse(ite) => {
            collect(&ite.node.then_branch, out);
            collect(&ite.node.else_branch, out);
        }
        _ => out.push(expr),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Literal;
    use crate::ir::mir::{LocalId, MirIfThenElse, MirLet};

    fn int(n: i64) -> Spanned<MirExpr> {
        Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Int(n))))
    }

    #[test]
    fn only_branches_and_let_bodies_supply_the_result() {
        let expr = Spanned::bare(MirExpr::Let(Spanned::bare(MirLet {
            binding: LocalId(0),
            binding_name: "unused".into(),
            value: Box::new(int(99)),
            body: Box::new(Spanned::bare(MirExpr::IfThenElse(Spanned::bare(
                MirIfThenElse {
                    cond: Box::new(int(88)),
                    then_branch: Box::new(int(1)),
                    else_branch: Box::new(int(2)),
                },
            )))),
        })));
        let mut found = Vec::new();
        collect(&expr, &mut found);
        let values: Vec<_> = found
            .iter()
            .map(|e| match &e.node {
                MirExpr::Literal(l) => l.node.clone(),
                _ => panic!("unexpected result"),
            })
            .collect();
        assert_eq!(values, vec![Literal::Int(1), Literal::Int(2)]);
    }

    #[test]
    fn control_transfers_keep_their_node_identity() {
        let expr = Spanned::bare(MirExpr::Return(Box::new(int(7))));
        let mut found = Vec::new();
        collect(&expr, &mut found);
        assert_eq!(found.len(), 1);
        assert!(std::ptr::eq(found[0], &expr));
    }
}
