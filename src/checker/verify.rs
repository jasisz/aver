use crate::ast::{Expr, Spanned, TailCallData, TopLevel, VerifyBlock, VerifyKind};

use super::callee_is_target;

pub(super) fn collect_target_call_args<'a>(
    expr: &'a Spanned<Expr>,
    fn_name: &str,
    arg_index: usize,
    out: &mut Vec<&'a Spanned<Expr>>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if callee_is_target(callee, fn_name)
                && let Some(arg) = args.get(arg_index)
            {
                out.push(arg);
            }
            collect_target_call_args(callee, fn_name, arg_index, out);
            for arg in args {
                collect_target_call_args(arg, fn_name, arg_index, out);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_target_call_args(left, fn_name, arg_index, out);
            collect_target_call_args(right, fn_name, arg_index, out);
        }
        Expr::Neg(inner) => collect_target_call_args(inner, fn_name, arg_index, out),
        Expr::Match { subject, arms, .. } => {
            collect_target_call_args(subject, fn_name, arg_index, out);
            for arm in arms {
                collect_target_call_args(&arm.body, fn_name, arg_index, out);
            }
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            collect_target_call_args(inner, fn_name, arg_index, out);
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_target_call_args(item, fn_name, arg_index, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_target_call_args(key, fn_name, arg_index, out);
                collect_target_call_args(value, fn_name, arg_index, out);
            }
        }
        Expr::Attr(obj, _) => collect_target_call_args(obj, fn_name, arg_index, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_target_call_args(value, fn_name, arg_index, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_target_call_args(base, fn_name, arg_index, out);
            for (_, value) in updates {
                collect_target_call_args(value, fn_name, arg_index, out);
            }
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            if target == fn_name
                && let Some(arg) = args.get(arg_index)
            {
                out.push(arg);
            }
            for arg in args {
                collect_target_call_args(arg, fn_name, arg_index, out);
            }
        }
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::InterpolatedStr(_)
        | Expr::Resolved { .. }
        | Expr::Constructor(_, None) => {}
    }
}

pub(super) fn verify_case_calls_target(left: &Spanned<Expr>, fn_name: &str) -> bool {
    match &left.node {
        Expr::FnCall(callee, args) => {
            callee_is_target(callee, fn_name)
                || verify_case_calls_target(callee, fn_name)
                || args
                    .iter()
                    .any(|arg| verify_case_calls_target(arg, fn_name))
        }
        Expr::BinOp(_, left_expr, right_expr) => {
            verify_case_calls_target(left_expr, fn_name)
                || verify_case_calls_target(right_expr, fn_name)
        }
        Expr::Neg(inner) => verify_case_calls_target(inner, fn_name),
        Expr::Match { subject, arms, .. } => {
            verify_case_calls_target(subject, fn_name)
                || arms
                    .iter()
                    .any(|arm| verify_case_calls_target(&arm.body, fn_name))
        }
        Expr::Constructor(_, Some(inner)) => verify_case_calls_target(inner, fn_name),
        Expr::ErrorProp(inner) => verify_case_calls_target(inner, fn_name),
        Expr::List(elems) => elems
            .iter()
            .any(|elem| verify_case_calls_target(elem, fn_name)),
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => items
            .iter()
            .any(|item| verify_case_calls_target(item, fn_name)),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            verify_case_calls_target(k, fn_name) || verify_case_calls_target(v, fn_name)
        }),
        Expr::Attr(obj, _) => verify_case_calls_target(obj, fn_name),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, expr)| verify_case_calls_target(expr, fn_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            verify_case_calls_target(base, fn_name)
                || updates
                    .iter()
                    .any(|(_, expr)| verify_case_calls_target(expr, fn_name))
        }
        Expr::TailCall(boxed) => {
            boxed.target == fn_name
                || boxed
                    .args
                    .iter()
                    .any(|arg| verify_case_calls_target(arg, fn_name))
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::InterpolatedStr(_) | Expr::Resolved { .. } => {
            false
        }
        Expr::Constructor(_, None) => false,
    }
}

pub fn merge_verify_blocks(items: &[TopLevel]) -> Vec<VerifyBlock> {
    let mut merged: Vec<VerifyBlock> = Vec::new();
    let mut by_fn_cases: std::collections::HashMap<String, usize> =
        std::collections::HashMap::new();

    for item in items {
        let TopLevel::Verify(vb) = item else {
            continue;
        };
        match &vb.kind {
            VerifyKind::Cases => {
                if let Some(&idx) = by_fn_cases.get(&vb.fn_name) {
                    merged[idx].cases.extend(vb.cases.clone());
                    merged[idx].case_spans.extend(vb.case_spans.clone());
                    merged[idx].case_givens.extend(vb.case_givens.clone());
                    debug_assert_eq!(merged[idx].cases.len(), merged[idx].case_spans.len());
                } else {
                    by_fn_cases.insert(vb.fn_name.clone(), merged.len());
                    merged.push(vb.clone());
                }
            }
            VerifyKind::Law(_) => {
                merged.push(vb.clone());
            }
        }
    }

    merged
}

pub fn expr_to_str(expr: &Spanned<Expr>) -> String {
    use crate::ast::Expr;
    use crate::ast::Literal;

    match &expr.node {
        Expr::Literal(lit) => match lit {
            Literal::Int(i) => i.to_string(),
            Literal::BigInt(s) => s.clone(),
            Literal::Float(f) => f.to_string(),
            Literal::Str(s) => format!("\"{}\"", s),
            Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Literal::Unit => "Unit".to_string(),
        },
        Expr::Ident(name) => name.clone(),
        Expr::FnCall(fn_expr, args) => {
            let fn_str = expr_to_str(fn_expr);
            let args_str = args.iter().map(expr_to_str).collect::<Vec<_>>().join(", ");
            format!("{}({})", fn_str, args_str)
        }
        Expr::Constructor(name, arg) => match arg {
            None => name.clone(),
            Some(a) => format!("{}({})", name, expr_to_str(a)),
        },
        Expr::BinOp(op, left, right) => {
            use crate::ast::BinOp;
            let op_str = match op {
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
            format!("{} {} {}", expr_to_str(left), op_str, expr_to_str(right))
        }
        Expr::Neg(inner) => format!("-{}", expr_to_str(inner)),
        Expr::InterpolatedStr(parts) => {
            use crate::ast::StrPart;
            let mut inner = String::new();
            for part in parts {
                match part {
                    StrPart::Literal(s) => inner.push_str(s),
                    StrPart::Parsed(e) => {
                        inner.push('{');
                        inner.push_str(&expr_to_str(e));
                        inner.push('}');
                    }
                }
            }
            format!("\"{}\"", inner)
        }
        Expr::List(elements) => {
            let parts: Vec<String> = elements.iter().map(expr_to_str).collect();
            format!("[{}]", parts.join(", "))
        }
        Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            let parts: Vec<String> = items.iter().map(expr_to_str).collect();
            format!("({})", parts.join(", "))
        }
        Expr::MapLiteral(entries) => {
            let parts = entries
                .iter()
                .map(|(key, value)| format!("{} => {}", expr_to_str(key), expr_to_str(value)))
                .collect::<Vec<_>>();
            format!("{{{}}}", parts.join(", "))
        }
        Expr::ErrorProp(inner) => format!("{}?", expr_to_str(inner)),
        Expr::Attr(obj, field) => format!("{}.{}", expr_to_str(obj), field),
        Expr::RecordCreate { type_name, fields } => {
            let flds: Vec<String> = fields
                .iter()
                .map(|(name, expr)| format!("{} = {}", name, expr_to_str(expr)))
                .collect();
            format!("{}({})", type_name, flds.join(", "))
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => {
            let upds: Vec<String> = updates
                .iter()
                .map(|(name, expr)| format!("{} = {}", name, expr_to_str(expr)))
                .collect();
            format!(
                "{}.update({}, {})",
                type_name,
                expr_to_str(base),
                upds.join(", ")
            )
        }
        Expr::TailCall(boxed) => {
            let TailCallData { target, args, .. } = boxed.as_ref();
            let a = args.iter().map(expr_to_str).collect::<Vec<_>>().join(", ");
            format!("<tail-call:{}>({})", target, a)
        }
        Expr::Resolved { name, last_use, .. } => {
            if last_use.0 {
                format!("<{}:last>", name)
            } else {
                format!("<{}>", name)
            }
        }
        Expr::Match { subject, arms, .. } => {
            let s = expr_to_str(subject);
            let arms_str: Vec<String> = arms
                .iter()
                .map(|arm| format!("{:?} -> {}", arm.pattern, expr_to_str(&arm.body)))
                .collect();
            format!("match {} {}", s, arms_str.join(", "))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse_items(src: &str) -> Vec<TopLevel> {
        let mut lexer = Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex failed");
        let mut parser = Parser::new(tokens);
        parser.parse().expect("parse failed")
    }

    #[test]
    fn merge_verify_blocks_coalesces_cases_by_function() {
        let items = parse_items(
            r#"
fn f(x: Int) -> Int
    x

verify f
    f(1) => 1

verify f
    f(2) => 2
"#,
        );
        let merged = merge_verify_blocks(&items);
        assert_eq!(merged.len(), 1);
        assert_eq!(merged[0].fn_name, "f");
        assert_eq!(merged[0].cases.len(), 2);
    }

    #[test]
    fn merge_verify_blocks_keeps_law_blocks_separate() {
        let items = parse_items(
            r#"
fn f(x: Int) -> Int
    x

verify f
    f(1) => 1

verify f law l1
    given x: Int = [1]
    x => x

verify f law l2
    given x: Int = [2]
    x => x

verify f
    f(2) => 2
"#,
        );
        let merged = merge_verify_blocks(&items);
        assert_eq!(merged.len(), 3);
        assert!(matches!(merged[0].kind, VerifyKind::Cases));
        assert_eq!(merged[0].cases.len(), 2);
        assert!(matches!(merged[1].kind, VerifyKind::Law(_)));
        assert!(matches!(merged[2].kind, VerifyKind::Law(_)));
    }
}
