use super::expr_to_dotted_name;
use crate::ast::{Expr, Spanned};

/// Shorthand: wrap an Expr in Box<Spanned::bare(...)>.
fn sbb(expr: Expr) -> Box<Spanned<Expr>> {
    Box::new(Spanned::bare(expr))
}

#[test]
fn dotted_name_flattens_attr_paths() {
    let expr = Expr::Attr(
        sbb(Expr::Attr(
            sbb(Expr::Ident("Data".to_string())),
            "Fib".to_string(),
        )),
        "fib".to_string(),
    );
    assert_eq!(expr_to_dotted_name(&expr).as_deref(), Some("Data.Fib.fib"));
}
