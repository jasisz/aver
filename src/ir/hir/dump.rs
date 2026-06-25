//! Textual dump for the resolved HIR. Mirrors `ir::dump` (which
//! renders pre-resolve `Vec<TopLevel>`) but emits opaque `FnId` /
//! `CtorId` / `TypeId` markers in expression positions so the
//! Phase E migration is visually verifiable from the CLI.
//!
//! The format is Aver-like surface syntax with explicit identity
//! markers wherever the resolver classified a name:
//!
//!   `<fn:N>(args)`         — `Call(ResolvedCallee::Fn(FnId(N)), …)`
//!   `<slot:N>(args)`       — `Call(ResolvedCallee::LocalSlot { slot: N, … }, …)`
//!   `<builtin:NS.method>`  — `Call(ResolvedCallee::Builtin("NS.method"), …)`
//!   `<intrinsic:__name>`   — `Call(ResolvedCallee::Intrinsic(_), …)`
//!   `<unresolved>(args)`   — `Call(ResolvedCallee::Unresolved { … }, …)`
//!   `<ctor:M@type:K>name`  — `Ctor(ResolvedCtor::User { ctor_id: M, type_id: K, name })`
//!   `Result.Ok` etc.       — `Ctor(ResolvedCtor::Builtin(BuiltinCtor::ResultOk))`
//!   `<type:K>name(...)`    — `RecordCreate { type_id: Some(K), type_name: name }`
//!   `<tail:fn:N>(args)`    — `TailCall { target: FnId(N), … }`
//!
//! Top-level scaffolding (module headers, fn sigs, stmt list) keeps
//! the same shape as the pre-resolve dump so a side-by-side `diff`
//! between `--emit-ir-after=resolve` and
//! `--emit-ir-after=name_resolve` highlights only the parts the
//! resolver touched.

use std::fmt::Write;

use super::{
    BuiltinCtor, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnBody, ResolvedFnDef,
    ResolvedMatchArm, ResolvedPattern, ResolvedStmt, ResolvedStrPart, ResolvedTopLevel,
};
use crate::ast::{BinOp, Literal, Spanned};

/// Render every top-level item in `items`, separated by blank
/// lines. Passes the original `Passthrough` items through the
/// pre-resolve dump unchanged so a mixed dump still reads
/// uniformly.
pub fn dump_resolved_program(items: &[ResolvedTopLevel]) -> String {
    let mut out = String::new();
    let mut first = true;
    for item in items {
        if !first {
            out.push('\n');
        }
        first = false;
        dump_resolved_top_level(item, &mut out);
    }
    out
}

fn dump_resolved_top_level(item: &ResolvedTopLevel, out: &mut String) {
    match item {
        ResolvedTopLevel::Module(m) => {
            writeln!(out, "module {}", m.name).ok();
            if !m.depends.is_empty() {
                writeln!(out, "    depends [{}]", m.depends.join(", ")).ok();
            }
        }
        ResolvedTopLevel::FnDef(fd) => dump_resolved_fn_def(fd, out),
        ResolvedTopLevel::Passthrough(item) => {
            // Reuse the pre-resolve dump so passthrough items
            // (verify / decision / type defs) still render in a
            // recognisable form. Cost: borrows the existing
            // single-item helper indirectly via `dump_items` on a
            // one-element slice.
            let rendered = crate::ir::dump::dump_items(std::slice::from_ref(item), None);
            out.push_str(rendered.trim_end());
            out.push('\n');
        }
    }
}

fn dump_resolved_fn_def(fd: &ResolvedFnDef, out: &mut String) {
    let params: Vec<String> = fd
        .params
        .iter()
        .map(|(name, ty)| format!("{name}: {}", ty.display()))
        .collect();
    let effects = if fd.effects.is_empty() {
        String::new()
    } else {
        let names: Vec<&str> = fd.effects.iter().map(|e| e.node.as_str()).collect();
        format!(" ! [{}]", names.join(", "))
    };
    writeln!(
        out,
        "fn <fn:{}>{}({}) -> {}{}",
        fd.fn_id.0,
        fd.name,
        params.join(", "),
        fd.return_type.display(),
        effects,
    )
    .ok();
    dump_resolved_fn_body(&fd.body, out);
}

fn dump_resolved_fn_body(body: &ResolvedFnBody, out: &mut String) {
    match body {
        ResolvedFnBody::Block(stmts) => {
            for stmt in stmts {
                dump_resolved_stmt(stmt, 1, out);
            }
        }
    }
}

fn dump_resolved_stmt(stmt: &ResolvedStmt, indent: usize, out: &mut String) {
    let pad = "    ".repeat(indent);
    match stmt {
        ResolvedStmt::Binding {
            name,
            ty_ann,
            value,
        } => {
            let ann = match ty_ann {
                Some(t) => format!(": {}", t.display()),
                None => String::new(),
            };
            writeln!(
                out,
                "{pad}{name}{ann} = {}",
                dump_resolved_expr(&value.node)
            )
            .ok();
        }
        ResolvedStmt::Expr(expr) => {
            writeln!(out, "{pad}{}", dump_resolved_expr(&expr.node)).ok();
        }
    }
}

/// Render an expression to a single line.
pub fn dump_resolved_expr(expr: &ResolvedExpr) -> String {
    match expr {
        ResolvedExpr::Literal(l) => dump_literal(l),
        ResolvedExpr::Ident(name) => name.clone(),
        ResolvedExpr::Resolved { slot, name, .. } => format!("<slot:{slot}:{name}>"),
        ResolvedExpr::Attr(obj, field) => {
            format!("{}.{field}", dump_resolved_expr(&obj.node))
        }
        ResolvedExpr::Call(callee, args) => {
            let args_str = render_args(args);
            format!("{}({args_str})", dump_resolved_callee(callee))
        }
        ResolvedExpr::BinOp(op, l, r) => format!(
            "({} {} {})",
            dump_resolved_expr(&l.node),
            dump_binop(*op),
            dump_resolved_expr(&r.node)
        ),
        ResolvedExpr::Neg(inner) => format!("-{}", dump_resolved_expr(&inner.node)),
        ResolvedExpr::Match { subject, arms } => {
            let mut s = format!("match {}", dump_resolved_expr(&subject.node));
            for arm in arms {
                s.push_str(&format!(" | {}", dump_resolved_arm(arm)));
            }
            s
        }
        ResolvedExpr::Ctor(ctor, args) => {
            let args_str = render_args(args);
            if args_str.is_empty() {
                dump_resolved_ctor(ctor)
            } else {
                format!("{}({args_str})", dump_resolved_ctor(ctor))
            }
        }
        ResolvedExpr::ErrorProp(inner) => format!("{}?", dump_resolved_expr(&inner.node)),
        ResolvedExpr::InterpolatedStr(parts) => {
            let pieces: Vec<String> = parts
                .iter()
                .map(|p| match p {
                    ResolvedStrPart::Literal(s) => s.clone(),
                    ResolvedStrPart::Parsed(e) => {
                        format!("${{{}}}", dump_resolved_expr(&e.node))
                    }
                })
                .collect();
            format!("\"{}\"", pieces.join(""))
        }
        ResolvedExpr::List(items) => format!("[{}]", render_args(items)),
        ResolvedExpr::Tuple(items) => format!("({})", render_args(items)),
        ResolvedExpr::MapLiteral(pairs) => {
            let pieces: Vec<String> = pairs
                .iter()
                .map(|(k, v)| {
                    format!(
                        "{} => {}",
                        dump_resolved_expr(&k.node),
                        dump_resolved_expr(&v.node)
                    )
                })
                .collect();
            format!("{{{}}}", pieces.join(", "))
        }
        ResolvedExpr::RecordCreate {
            type_id,
            type_name,
            fields,
        } => {
            let marker = match type_id {
                Some(id) => format!("<type:{}>", id.0),
                None => "<type:?>".to_string(),
            };
            let pieces: Vec<String> = fields
                .iter()
                .map(|(n, e)| format!("{n} = {}", dump_resolved_expr(&e.node)))
                .collect();
            format!("{marker}{type_name}({})", pieces.join(", "))
        }
        ResolvedExpr::RecordUpdate {
            type_id,
            type_name,
            base,
            updates,
        } => {
            let marker = match type_id {
                Some(id) => format!("<type:{}>", id.0),
                None => "<type:?>".to_string(),
            };
            let pieces: Vec<String> = updates
                .iter()
                .map(|(n, e)| format!("{n} = {}", dump_resolved_expr(&e.node)))
                .collect();
            format!(
                "{marker}{type_name}.update({}, {})",
                dump_resolved_expr(&base.node),
                pieces.join(", ")
            )
        }
        ResolvedExpr::TailCall { target, args } => {
            format!("<tail:fn:{}>({})", target.0, render_args(args))
        }
        ResolvedExpr::IndependentProduct(items, unwrap) => {
            let suffix = if *unwrap { "?!" } else { "!" };
            format!("({}){suffix}", render_args(items))
        }
    }
}

fn render_args(args: &[Spanned<ResolvedExpr>]) -> String {
    args.iter()
        .map(|a| dump_resolved_expr(&a.node))
        .collect::<Vec<_>>()
        .join(", ")
}

fn dump_resolved_callee(callee: &ResolvedCallee) -> String {
    match callee {
        ResolvedCallee::Fn(id) => format!("<fn:{}>", id.0),
        ResolvedCallee::Builtin(name) => format!("<builtin:{name}>"),
        ResolvedCallee::Intrinsic(kind) => format!("<intrinsic:{}>", kind.name()),
        ResolvedCallee::LocalSlot { slot, name, .. } => format!("<slot:{slot}:{name}>"),
        ResolvedCallee::Unresolved { callee } => {
            format!("<unresolved:{}>", dump_resolved_expr(&callee.node))
        }
    }
}

fn dump_resolved_ctor(ctor: &ResolvedCtor) -> String {
    match ctor {
        ResolvedCtor::User {
            ctor_id,
            type_id,
            name,
        } => format!("<ctor:{}@type:{}>{name}", ctor_id.0, type_id.0),
        ResolvedCtor::Builtin(BuiltinCtor::ResultOk) => "Result.Ok".to_string(),
        ResolvedCtor::Builtin(BuiltinCtor::ResultErr) => "Result.Err".to_string(),
        ResolvedCtor::Builtin(BuiltinCtor::OptionSome) => "Option.Some".to_string(),
        ResolvedCtor::Builtin(BuiltinCtor::OptionNone) => "Option.None".to_string(),
        ResolvedCtor::Unresolved { name } => format!("<unresolved-ctor:{name}>"),
    }
}

fn dump_resolved_arm(arm: &ResolvedMatchArm) -> String {
    format!(
        "{} -> {}",
        dump_resolved_pattern(&arm.pattern),
        dump_resolved_expr(&arm.body.node)
    )
}

fn dump_resolved_pattern(pat: &ResolvedPattern) -> String {
    match pat {
        ResolvedPattern::Wildcard => "_".to_string(),
        ResolvedPattern::Literal(l) => dump_literal(l),
        ResolvedPattern::Ident(name) => name.clone(),
        ResolvedPattern::EmptyList => "[]".to_string(),
        ResolvedPattern::Cons(head, tail) => format!("[{head}, ..{tail}]"),
        ResolvedPattern::Tuple(items) => {
            let pieces: Vec<String> = items.iter().map(dump_resolved_pattern).collect();
            format!("({})", pieces.join(", "))
        }
        ResolvedPattern::Ctor(ctor, bindings) => {
            let head = dump_resolved_ctor(ctor);
            if bindings.is_empty() {
                head
            } else {
                format!("{head}({})", bindings.join(", "))
            }
        }
    }
}

fn dump_literal(l: &Literal) -> String {
    match l {
        Literal::Int(i) => i.to_string(),
        Literal::BigInt(s) => s.clone(),
        Literal::Float(f) => format!("{f}"),
        Literal::Str(s) => format!("\"{s}\""),
        Literal::Bool(b) => b.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

fn dump_binop(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "+",
        BinOp::Sub => "-",
        BinOp::Mul => "*",
        BinOp::Div => "/",
        BinOp::Eq => "==",
        BinOp::Neq => "!=",
        BinOp::Lt => "<",
        BinOp::Lte => "<=",
        BinOp::Gt => ">",
        BinOp::Gte => ">=",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::SymbolTable;
    use crate::ir::hir::resolve_program;
    use crate::source::parse_source;
    use crate::tco;

    fn dump(src: &str) -> String {
        let mut items = parse_source(src).expect("parse");
        tco::transform_program(&mut items);
        let symbols = SymbolTable::build(&items, &[]);
        let resolved = resolve_program(&symbols, &items);
        dump_resolved_program(&resolved)
    }

    #[test]
    fn dump_includes_fn_id_marker_on_call() {
        let out = dump(
            r#"
fn helper(n: Int) -> Int
    n + 1

fn main() -> Int
    helper(7)
"#,
        );
        // helper has FnId(0), main has FnId(1) by source order.
        assert!(
            out.contains("fn <fn:0>helper"),
            "expected fn-id marker on helper signature, got:\n{out}"
        );
        assert!(
            out.contains("<fn:0>(7)"),
            "expected helper call marker in main body, got:\n{out}"
        );
    }

    #[test]
    fn dump_includes_builtin_ctor_marker() {
        let out = dump(
            r#"
fn make() -> Result<Int, String>
    Result.Ok(42)
"#,
        );
        assert!(
            out.contains("Result.Ok(42)"),
            "expected Result.Ok builtin ctor render, got:\n{out}"
        );
    }

    #[test]
    fn dump_includes_user_ctor_marker() {
        let out = dump(
            r#"
type Shape
    Circle(Float)

fn make() -> Shape
    Shape.Circle(1.0)
"#,
        );
        assert!(
            out.contains("<ctor:") && out.contains("@type:") && out.contains(">Circle"),
            "expected `<ctor:N@type:K>Circle(...)`, got:\n{out}"
        );
    }

    #[test]
    fn dump_includes_record_type_id_marker() {
        let out = dump(
            r#"
record Point
    x: Int
    y: Int

fn origin() -> Point
    Point(x = 0, y = 0)
"#,
        );
        assert!(
            out.contains("<type:") && out.contains(">Point(x = 0, y = 0)"),
            "expected `<type:K>Point(x = 0, y = 0)`, got:\n{out}"
        );
    }

    #[test]
    fn dump_includes_tail_call_marker() {
        let out = dump(
            r#"
fn count(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> count(n - 1, acc + 1)
"#,
        );
        assert!(
            out.contains("<tail:fn:"),
            "expected tail-call marker on recursive arm, got:\n{out}"
        );
    }

    #[test]
    fn dump_renders_builtin_namespace_method_call() {
        let out = dump(
            r#"
fn abs_neg() -> Int
    Int.abs(-3)
"#,
        );
        assert!(
            out.contains("<builtin:Int.abs>(-3)"),
            "expected builtin namespace method marker, got:\n{out}"
        );
    }

    #[test]
    fn dump_passes_through_non_fn_items() {
        // Verify / decision / type def items render via the
        // pre-resolve dump (no Phase-E markers).
        let out = dump(
            r#"
type Tag
    On
    Off

fn use_it() -> Int
    1
"#,
        );
        assert!(
            out.contains("type Tag"),
            "TypeDef should pass through: {out}"
        );
        assert!(out.contains("fn <fn:"), "FnDef should be promoted: {out}");
    }
}
