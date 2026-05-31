//! Shared per-pass test fixtures. Each pass's own `mod tests`
//! imports `span` / `one_fn_program` / `body_of` from here so
//! the boilerplate isn't duplicated six times.

use crate::ast::Spanned;
use crate::ir::FnId;

use super::super::expr::MirExpr;
use super::super::program::{MirFn, MirProgram};

pub(crate) fn span<T>(node: T) -> Spanned<T> {
    Spanned {
        node,
        line: 0,
        ty: std::sync::OnceLock::new(),
    }
}

pub(crate) fn one_fn_program(body: MirExpr) -> MirProgram {
    let mut p = MirProgram::empty();
    p.fns.insert(
        FnId(0),
        MirFn {
            fn_id: FnId(0),
            name: "test".to_string(),
            params: vec![],
            return_type: "Int".to_string(),
            effects: vec![],
            body: span(body),
        },
    );
    p
}

pub(crate) fn body_of(p: &MirProgram) -> &MirExpr {
    &p.fns.get(&FnId(0)).unwrap().body.node
}
