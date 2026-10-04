//! Terms written back as Aver source, for refusals and reports: where a
//! producer stopped and what it had in scope.

use crate::ast::{BinOp, Literal};
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedPattern, ResolvedStrPart};

use super::sexpr::Names;
use super::term::{HOLE, Term};
use super::{Eqn, Proof};

fn op(o: BinOp) -> &'static str {
    match o {
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
    }
}

fn literal(l: &Literal) -> String {
    match l {
        Literal::Int(v) => v.to_string(),
        Literal::BigInt(s) => s.clone(),
        Literal::Float(f) => format!("{f}"),
        Literal::Str(s) => format!("{s:?}"),
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "Unit".to_string(),
    }
}

fn args(xs: &[Term], names: &dyn Names) -> String {
    xs.iter()
        .map(|x| term(x, names))
        .collect::<Vec<_>>()
        .join(", ")
}

/// An operand: parenthesised when it is itself an operator application.
fn operand(t: &Term, names: &dyn Names) -> String {
    match &t.node {
        ResolvedExpr::BinOp(..) | ResolvedExpr::Match { .. } => format!("({})", term(t, names)),
        _ => term(t, names),
    }
}

fn pattern(p: &ResolvedPattern, names: &dyn Names) -> String {
    match p {
        ResolvedPattern::Wildcard => "_".into(),
        ResolvedPattern::Ident(n) => n.clone(),
        ResolvedPattern::Literal(l) => literal(l),
        ResolvedPattern::EmptyList => "[]".into(),
        ResolvedPattern::Cons(h, t) => format!("[{h}, ..{t}]"),
        ResolvedPattern::Tuple(ps) => format!(
            "({})",
            ps.iter()
                .map(|p| pattern(p, names))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ResolvedPattern::Ctor(c, ns) if ns.is_empty() => names.ctor_name(c),
        ResolvedPattern::Ctor(c, ns) => format!("{}({})", names.ctor_name(c), ns.join(", ")),
    }
}

/// One term, on one line.
pub fn term(t: &Term, names: &dyn Names) -> String {
    match &t.node {
        ResolvedExpr::Literal(l) => literal(l),
        ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => {
            if n == HOLE {
                "□".into()
            } else {
                n.clone()
            }
        }
        ResolvedExpr::Attr(o, f) => format!("{}.{f}", operand(o, names)),
        ResolvedExpr::Call(callee, xs) => {
            let head = match callee {
                ResolvedCallee::Fn(id) => names.fn_name(*id),
                ResolvedCallee::Builtin(b) => b.clone(),
                ResolvedCallee::Intrinsic(i) => i.name().to_string(),
                ResolvedCallee::LocalSlot { name, .. } => name.clone(),
                ResolvedCallee::Unresolved { .. } => "?".into(),
            };
            format!("{head}({})", args(xs, names))
        }
        ResolvedExpr::TailCall { target, args: xs } => {
            format!("{}({})", names.fn_name(*target), args(xs, names))
        }
        ResolvedExpr::BinOp(o, a, b) => {
            format!("{} {} {}", operand(a, names), op(*o), operand(b, names))
        }
        ResolvedExpr::Neg(a) => format!("-{}", operand(a, names)),
        ResolvedExpr::Match { subject, arms } => {
            let mut s = format!("match {}", operand(subject, names));
            for arm in arms {
                s.push_str(&format!(
                    " {{ {} -> {} }}",
                    pattern(&arm.pattern, names),
                    term(&arm.body, names)
                ));
            }
            s
        }
        ResolvedExpr::Ctor(c, xs) if xs.is_empty() => names.ctor_name(c),
        ResolvedExpr::Ctor(c, xs) => format!("{}({})", names.ctor_name(c), args(xs, names)),
        ResolvedExpr::ErrorProp(a) => format!("{}?", operand(a, names)),
        ResolvedExpr::InterpolatedStr(parts) => {
            let mut s = String::from("\"");
            for p in parts {
                match p {
                    ResolvedStrPart::Literal(text) => s.push_str(text),
                    ResolvedStrPart::Parsed(e) => s.push_str(&format!("{{{}}}", term(e, names))),
                }
            }
            s.push('"');
            s
        }
        ResolvedExpr::List(xs) => format!("[{}]", args(xs, names)),
        ResolvedExpr::Tuple(xs) => format!("({})", args(xs, names)),
        ResolvedExpr::MapLiteral(kvs) => format!(
            "{{{}}}",
            kvs.iter()
                .map(|(k, v)| format!("{} => {}", term(k, names), term(v, names)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ResolvedExpr::RecordCreate {
            type_name, fields, ..
        } => format!(
            "{type_name}({})",
            fields
                .iter()
                .map(|(n, v)| format!("{n} = {}", term(v, names)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ResolvedExpr::RecordUpdate {
            type_name,
            base,
            updates,
            ..
        } => format!(
            "{type_name}.update({}, {})",
            term(base, names),
            updates
                .iter()
                .map(|(n, v)| format!("{n} = {}", term(v, names)))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        ResolvedExpr::IndependentProduct(xs, _) => format!("({})!", args(xs, names)),
    }
}

pub fn eqn(e: &Eqn, names: &dyn Names) -> String {
    format!("{} = {}", term(&e.lhs, names), term(&e.rhs, names))
}

/// The rule a step applies, by name, for a report.
pub fn rule_name(p: &Proof, names: &dyn Names) -> String {
    match p {
        Proof::Law { law, .. } => format!("law {law}"),
        Proof::Rule { rule, .. } => format!("rule {}", rule.id()),
        Proof::Unfold { fn_id, .. } => format!("the definition of {}", names.fn_name(*fn_id)),
        Proof::Hyp(h) => format!("hypothesis {h}"),
        _ => "a step".into(),
    }
}
