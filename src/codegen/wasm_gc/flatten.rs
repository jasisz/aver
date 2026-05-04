//! Single-binary multi-module flattening for the wasm-gc backend.
//!
//! The wasm-gc backend emits one standalone module today. Dependent Aver
//! modules are therefore inlined into the entry module by prefixing function
//! names with their module path and rewriting cross-module calls to those flat
//! function names. Type definitions stay bare because the wasm-gc type
//! registry is local to the final flattened module.

use std::collections::HashSet;

use crate::ast::{Expr, FnBody, Spanned, Stmt, TopLevel, TypeDef};
use crate::codegen::ModuleInfo;

/// Walks each loaded `ModuleInfo`, prefixes every fn name with
/// `{module_prefix}_`, rewrites bare same-module call sites to use the
/// prefixed name, then appends the dep's `TypeDef`s and `FnDef`s onto the
/// entry items. Cross-module callsites in the entry are rewritten from
/// `Attr(Ident("Fractal"), "render")` to `Ident("Fractal_render")`.
///
/// Component Model is a future separate mode; this single-binary path is the
/// bench-friendly and playground-friendly default.
pub fn flatten_multimodule(items: &mut Vec<TopLevel>, dep_modules: &[ModuleInfo]) {
    if dep_modules.is_empty() {
        return;
    }

    let prefixes: HashSet<String> = dep_modules.iter().map(|m| m.prefix.clone()).collect();

    let qualified_type_names: HashSet<String> = dep_modules
        .iter()
        .flat_map(|dep| {
            dep.type_defs.iter().map(|td| {
                let name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name.clone(),
                };
                format!("{}.{}", dep.prefix, name)
            })
        })
        .collect();

    let empty_set: HashSet<String> = HashSet::new();
    for item in items.iter_mut() {
        match item {
            TopLevel::FnDef(fd) => {
                rewrite_fn_signature(fd, &qualified_type_names);
                let body_arc = std::sync::Arc::make_mut(&mut fd.body);
                let FnBody::Block(stmts) = body_arc;
                rewrite_stmts(stmts, &prefixes, None, &empty_set);
            }
            TopLevel::TypeDef(td) => rewrite_type_def(td, &qualified_type_names),
            _ => {}
        }
    }

    for dep in dep_modules {
        let same_module_fns: HashSet<String> =
            dep.fn_defs.iter().map(|fd| fd.name.clone()).collect();
        for td in &dep.type_defs {
            let mut new_td = td.clone();
            rewrite_type_def(&mut new_td, &qualified_type_names);
            items.push(TopLevel::TypeDef(new_td));
        }
        for fd in &dep.fn_defs {
            let mut new_fd = fd.clone();
            rewrite_fn_signature(&mut new_fd, &qualified_type_names);

            let body_arc = std::sync::Arc::make_mut(&mut new_fd.body);
            let FnBody::Block(stmts) = body_arc;
            rewrite_stmts(stmts, &prefixes, Some(&dep.prefix), &same_module_fns);

            new_fd.name = prefixed(&dep.prefix, &fd.name);
            items.push(TopLevel::FnDef(new_fd));
        }
    }
}

fn prefixed(prefix: &str, name: &str) -> String {
    format!("{}_{}", prefix.replace('.', "_"), name)
}

fn rewrite_fn_signature(fd: &mut crate::ast::FnDef, qualified_type_names: &HashSet<String>) {
    for (_, ty) in fd.params.iter_mut() {
        *ty = strip_module_prefixes(ty, qualified_type_names);
    }
    fd.return_type = strip_module_prefixes(&fd.return_type, qualified_type_names);
}

fn rewrite_type_def(td: &mut TypeDef, qualified_type_names: &HashSet<String>) {
    match td {
        TypeDef::Sum { variants, .. } => {
            for variant in variants {
                for ty in variant.fields.iter_mut() {
                    *ty = strip_module_prefixes(ty, qualified_type_names);
                }
            }
        }
        TypeDef::Product { fields, .. } => {
            for (_, ty) in fields.iter_mut() {
                *ty = strip_module_prefixes(ty, qualified_type_names);
            }
        }
    }
}

fn strip_module_prefixes(type_str: &str, qualified_type_names: &HashSet<String>) -> String {
    if qualified_type_names.is_empty() {
        return type_str.to_string();
    }
    let mut out = type_str.to_string();
    for qualified in qualified_type_names {
        if let Some((_, bare)) = qualified.rsplit_once('.') {
            out = replace_qualified_type(&out, qualified, bare);
        }
    }
    out
}

fn replace_qualified_type(input: &str, qualified: &str, bare: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut i = 0;
    let bytes = input.as_bytes();
    while i < bytes.len() {
        let rest = &input[i..];
        if rest.starts_with(qualified) {
            let before_ok = i == 0 || is_type_boundary(bytes[i - 1]);
            let after = i + qualified.len();
            let after_ok = after >= bytes.len() || is_type_boundary(bytes[after]);
            if before_ok && after_ok {
                out.push_str(bare);
                i = after;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}

fn is_type_boundary(byte: u8) -> bool {
    matches!(
        byte,
        b'<' | b'>' | b',' | b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')'
    )
}

/// Flatten a chained `Attr` expression into its dotted form. Returns `None`
/// for any non-Attr or any Attr whose root is not an Ident.
fn attr_chain_to_dotted(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(parent, member) => {
            let head = attr_chain_to_dotted(&parent.node)?;
            Some(format!("{head}.{member}"))
        }
        _ => None,
    }
}

fn rewrite_expr(
    expr: &mut Expr,
    prefixes: &HashSet<String>,
    same_module_prefix: Option<&str>,
    same_module_fns: &HashSet<String>,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            let mut new_callee: Option<Expr> = None;
            if let Expr::Attr(parent, member) = &callee.node {
                if let Expr::Ident(p) = &parent.node
                    && prefixes.contains(p)
                {
                    new_callee = Some(Expr::Ident(prefixed(p, member)));
                } else if let Some(dotted) = attr_chain_to_dotted(&callee.node) {
                    new_callee = rewrite_dotted_module_ref(&dotted, prefixes);
                }
            }
            if new_callee.is_none()
                && let Expr::Ident(name) = &callee.node
                && let Some(prefix) = same_module_prefix
                && same_module_fns.contains(name)
            {
                new_callee = Some(Expr::Ident(prefixed(prefix, name)));
            }
            if let Some(rep) = new_callee {
                callee.node = rep;
            }
            rewrite_expr(
                &mut callee.node,
                prefixes,
                same_module_prefix,
                same_module_fns,
            );
            for arg in args.iter_mut() {
                rewrite_expr(&mut arg.node, prefixes, same_module_prefix, same_module_fns);
            }
        }
        Expr::TailCall(boxed) => {
            if let Some(prefix) = same_module_prefix
                && same_module_fns.contains(&boxed.target)
            {
                boxed.target = prefixed(prefix, &boxed.target);
            }
            for arg in boxed.args.iter_mut() {
                rewrite_expr(&mut arg.node, prefixes, same_module_prefix, same_module_fns);
            }
        }
        Expr::BinOp(_, left, right) => {
            rewrite_expr(
                &mut left.node,
                prefixes,
                same_module_prefix,
                same_module_fns,
            );
            rewrite_expr(
                &mut right.node,
                prefixes,
                same_module_prefix,
                same_module_fns,
            );
        }
        Expr::Match { subject, arms } => {
            rewrite_expr(
                &mut subject.node,
                prefixes,
                same_module_prefix,
                same_module_fns,
            );
            for arm in arms.iter_mut() {
                rewrite_expr(
                    &mut arm.body.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
        Expr::Attr(_, _) => {
            let rewrite = attr_chain_to_dotted(expr)
                .and_then(|dotted| rewrite_dotted_module_ref(&dotted, prefixes));
            if let Some(new_node) = rewrite {
                *expr = new_node;
                return;
            }
            if let Expr::Attr(obj, _) = expr {
                rewrite_expr(&mut obj.node, prefixes, same_module_prefix, same_module_fns);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(payload) = payload.as_deref_mut() {
                rewrite_expr(
                    &mut payload.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields.iter_mut() {
                rewrite_expr(
                    &mut expr.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            rewrite_expr(
                &mut base.node,
                prefixes,
                same_module_prefix,
                same_module_fns,
            );
            for (_, expr) in updates.iter_mut() {
                rewrite_expr(
                    &mut expr.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items.iter_mut() {
                rewrite_expr(
                    &mut item.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries.iter_mut() {
                rewrite_expr(&mut key.node, prefixes, same_module_prefix, same_module_fns);
                rewrite_expr(
                    &mut value.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
        Expr::ErrorProp(inner) => {
            rewrite_expr(
                &mut inner.node,
                prefixes,
                same_module_prefix,
                same_module_fns,
            );
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts.iter_mut() {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    rewrite_expr(
                        &mut inner.node,
                        prefixes,
                        same_module_prefix,
                        same_module_fns,
                    );
                }
            }
        }
        _ => {}
    }
}

fn rewrite_dotted_module_ref(dotted: &str, prefixes: &HashSet<String>) -> Option<Expr> {
    let segments: Vec<&str> = dotted.split('.').collect();
    if segments.len() < 2 {
        return None;
    }

    let mut best: Option<usize> = None;
    for split in (1..segments.len()).rev() {
        let candidate = segments[..split].join(".");
        if prefixes.contains(&candidate) {
            best = Some(split);
            break;
        }
    }
    let split = best?;
    let prefix_dotted = segments[..split].join(".");

    Some(match segments.len() - split {
        0 => return None,
        1 => Expr::Ident(prefixed(&prefix_dotted, segments[split])),
        _ => {
            let type_name = segments[split..segments.len() - 1].join("_");
            let last = segments[segments.len() - 1].to_string();
            Expr::Attr(Box::new(Spanned::bare(Expr::Ident(type_name))), last)
        }
    })
}

fn rewrite_stmts(
    stmts: &mut [Stmt],
    prefixes: &HashSet<String>,
    same_module_prefix: Option<&str>,
    same_module_fns: &HashSet<String>,
) {
    for stmt in stmts.iter_mut() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                rewrite_expr(
                    &mut expr.node,
                    prefixes,
                    same_module_prefix,
                    same_module_fns,
                );
            }
        }
    }
}
