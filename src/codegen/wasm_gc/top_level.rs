//! Module-level bindings (`greeting = "a{1}b"` outside any fn) read from a
//! fn body.
//!
//! The VM evaluates such a binding once into a global and the Rust backend
//! renders it as a `let` at the top of `main`. wasm-gc has no place to run
//! module-level code before a fn, so it substitutes the binding's value
//! for each read of its name instead. A module-level value is checked with
//! no effects allowed, so evaluating it where it is read gives the same
//! value as evaluating it once.
//!
//! The substitution runs on the post-resolve items: inside a fn body a
//! local is already `Expr::Resolved`, and the language forbids a local
//! from shadowing a module-level name, so a bare `Expr::Ident` naming a
//! binding is always a read of that binding. A binding is substituted
//! as its typed expression, after the bindings it reads have been
//! substituted into it. A value with its own binders (a `match`) would
//! need slots in every fn it lands in, so reading one from a fn is a
//! compile error naming the binding.

use std::collections::HashMap;
use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, StrPart, TopLevel};

use super::WasmGcError;

/// `items` with every module-level binding read substituted into the fn
/// bodies that read it, or `None` when no fn reads one (the common case,
/// which leaves the items untouched).
pub(super) fn inline_module_bindings(
    items: &[TopLevel],
) -> Result<Option<Vec<TopLevel>>, WasmGcError> {
    let mut values: HashMap<String, Spanned<Expr>> = HashMap::new();
    for item in items {
        if let TopLevel::Stmt(Stmt::Binding(name, _, value)) = item {
            let mut value = value.clone();
            substitute(&mut value, &values, None)?;
            values.insert(name.clone(), value);
        }
    }
    if values.is_empty() {
        return Ok(None);
    }
    let reads_a_binding = items.iter().any(|item| match item {
        TopLevel::FnDef(fd) => fd.body.stmts().iter().any(|stmt| {
            let (Stmt::Binding(_, _, e) | Stmt::Expr(e)) = stmt;
            reads_any(e, &values)
        }),
        _ => false,
    });
    if !reads_a_binding {
        return Ok(None);
    }
    let mut out = Vec::with_capacity(items.len());
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                let mut body: FnBody = fd.body.as_ref().clone();
                for stmt in body.stmts_mut() {
                    let (Stmt::Binding(_, _, e) | Stmt::Expr(e)) = stmt;
                    substitute(e, &values, Some(&fd.name))?;
                }
                out.push(TopLevel::FnDef(FnDef {
                    body: Arc::new(body),
                    ..fd.clone()
                }));
            }
            other => out.push(other.clone()),
        }
    }
    Ok(Some(out))
}

fn reads_any(e: &Spanned<Expr>, values: &HashMap<String, Spanned<Expr>>) -> bool {
    let mut found = false;
    visit(e, &mut |node| {
        if let Expr::Ident(name) = node
            && values.contains_key(name)
        {
            found = true;
        }
    });
    found
}

fn visit(e: &Spanned<Expr>, f: &mut dyn FnMut(&Expr)) {
    f(&e.node);
    match &e.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::ErrorProp(inner) => visit(inner, f),
        Expr::FnCall(callee, args) => {
            visit(callee, f);
            args.iter().for_each(|a| visit(a, f));
        }
        Expr::BinOp(_, l, r) => {
            visit(l, f);
            visit(r, f);
        }
        Expr::Match { subject, arms } => {
            visit(subject, f);
            arms.iter().for_each(|arm| visit(&arm.body, f));
        }
        Expr::Constructor(_, arg) => {
            if let Some(arg) = arg {
                visit(arg, f);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    visit(inner, f);
                }
            }
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            xs.iter().for_each(|x| visit(x, f))
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                visit(k, f);
                visit(v, f);
            }
        }
        Expr::RecordCreate { fields, .. } => fields.iter().for_each(|(_, v)| visit(v, f)),
        Expr::RecordUpdate { base, updates, .. } => {
            visit(base, f);
            updates.iter().for_each(|(_, v)| visit(v, f));
        }
        Expr::TailCall(data) => data.args.iter().for_each(|a| visit(a, f)),
    }
}

/// Does `e` carry binders of its own — a `match` arm's pattern names?
fn has_binders(e: &Spanned<Expr>) -> bool {
    let mut found = false;
    visit(e, &mut |node| {
        if matches!(node, Expr::Match { .. }) {
            found = true;
        }
    });
    found
}

/// Replace every read of a binding in `e` with its value. `reader` names
/// the fn being rewritten, for the error a binder-carrying value raises.
fn substitute(
    e: &mut Spanned<Expr>,
    values: &HashMap<String, Spanned<Expr>>,
    reader: Option<&str>,
) -> Result<(), WasmGcError> {
    if let Expr::Ident(name) = &e.node
        && let Some(value) = values.get(name)
    {
        if let Some(reader) = reader
            && has_binders(value)
        {
            return Err(WasmGcError::Validation(format!(
                "fn `{reader}` reads the module-level binding `{name}`, whose value contains a \
                 `match`; wasm-gc substitutes a module-level value where it is read and cannot \
                 give that match's bindings a place in `{reader}`. Move the value into a fn \
                 and call it."
            )));
        }
        *e = value.clone();
        return Ok(());
    }
    let each = |x: &mut Spanned<Expr>| substitute(x, values, reader);
    match &mut e.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(inner, _) | Expr::Neg(inner) | Expr::ErrorProp(inner) => each(inner)?,
        Expr::FnCall(callee, args) => {
            each(callee)?;
            for a in args {
                each(a)?;
            }
        }
        Expr::BinOp(_, l, r) => {
            each(l)?;
            each(r)?;
        }
        Expr::Match { subject, arms } => {
            each(subject)?;
            for arm in arms {
                each(&mut arm.body)?;
            }
        }
        Expr::Constructor(_, arg) => {
            if let Some(arg) = arg {
                each(arg)?;
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    each(inner)?;
                }
            }
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            for x in xs {
                each(x)?;
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                each(k)?;
                each(v)?;
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                each(v)?;
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            each(base)?;
            for (_, v) in updates {
                each(v)?;
            }
        }
        Expr::TailCall(data) => {
            for a in &mut data.args {
                each(a)?;
            }
        }
    }
    Ok(())
}
