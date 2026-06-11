//! AST recognition helpers shared by both lane families
//! (sign-premise and bridge-premise).

use crate::ast::{Expr, Spanned};

pub(super) fn ident_of(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
        _ => None,
    }
}

pub(super) fn call_of(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    match &e.node {
        Expr::FnCall(callee, args) => Some((
            crate::codegen::common::expr_to_dotted_name(&callee.node)?,
            args.as_slice(),
        )),
        Expr::TailCall(data) => Some((data.target.clone(), data.args.as_slice())),
        _ => None,
    }
}

pub(super) fn ctor_of(e: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
    match &e.node {
        Expr::FnCall(callee, args) => {
            let name = crate::codegen::common::expr_to_dotted_name(&callee.node)?;
            let leaf = name.rsplit('.').next()?;
            if !leaf.chars().next().is_some_and(|c| c.is_uppercase()) {
                return None;
            }
            Some((name, args.iter().collect()))
        }
        Expr::Constructor(name, payload) => {
            let args: Vec<&Spanned<Expr>> = match payload.as_deref() {
                None => Vec::new(),
                Some(Spanned {
                    node: Expr::Tuple(items),
                    ..
                }) => items.iter().collect(),
                Some(single) => vec![single],
            };
            Some((name.clone(), args))
        }
        _ => None,
    }
}
