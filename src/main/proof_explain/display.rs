//! Readable source expressions, retaining explicit arithmetic grouping. The
//! general unparser handles uncommon forms and literal escaping.
use aver::ast::{BinOp, Expr, Spanned};

pub(super) fn expression(expr: &Spanned<Expr>) -> String {
    let Ok(text) = grouped(expr) else {
        return "<source expression unavailable>".into();
    };
    let text = text.trim();
    if matches!(expr.node, Expr::BinOp(..)) {
        text.strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(text)
            .to_string()
    } else {
        text.to_string()
    }
}

fn grouped(expr: &Spanned<Expr>) -> Result<String, aver::ast::unparse::UnparseError> {
    match &expr.node {
        Expr::Attr(receiver, name)
            if matches!(
                receiver.node,
                Expr::Ident(_) | Expr::Attr(..) | Expr::FnCall(..)
            ) =>
        {
            Ok(format!("{}.{name}", grouped(receiver)?))
        }
        Expr::FnCall(callee, args) => {
            let args = args.iter().map(grouped).collect::<Result<Vec<_>, _>>()?;
            Ok(format!("{}({})", grouped(callee)?, args.join(", ")))
        }
        Expr::BinOp(op, lhs, rhs) => {
            let op = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            Ok(format!("({} {op} {})", grouped(lhs)?, grouped(rhs)?))
        }
        _ => {
            let mut text = String::new();
            aver::ast::unparse::write_expr_public(&mut text, expr, 0)?;
            Ok(text)
        }
    }
}
