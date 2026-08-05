//! Dependency order for Lean declarations.
//!
//! Lean elaborates declarations top-to-bottom. Most Aver types can be emitted
//! before every function, but a refinement subtype depends on the function
//! used as its smart-constructor predicate. Nested refinements interleave the
//! two kinds further (`allInRange` → `Bytes` → `hasLength32` → `Digest32`).
//! This module builds one small declaration DAG per Aver module and keeps the
//! main transpiler concerned only with rendering the resulting order.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, FnDef, Spanned, Stmt, TypeDef};
use crate::codegen::CodegenContext;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum ScopedDecl {
    Type(usize),
    FnComponent(usize),
}

pub(super) struct ScopedDeclPlan<'a> {
    pub components: Vec<Vec<&'a FnDef>>,
    pub order: Vec<ScopedDecl>,
}

pub(super) fn plan_scoped_declarations<'a>(
    ctx: &CodegenContext,
    type_defs: &'a [TypeDef],
    fn_defs: &'a [FnDef],
    scope: Option<&str>,
) -> ScopedDeclPlan<'a> {
    let pure: Vec<&FnDef> = fn_defs
        .iter()
        .filter(|fd| super::toplevel::is_pure_fn(fd))
        .collect();
    let components = crate::call_graph::ordered_fn_components(&pure, &ctx.module_prefixes);

    let type_count = type_defs.len();
    let node_count = type_count + components.len();
    let mut dependencies = vec![HashSet::<usize>::new(); node_count];

    let type_nodes: HashMap<String, usize> = type_defs
        .iter()
        .enumerate()
        .map(|(index, td)| (type_name(td).to_string(), index))
        .collect();
    let mut fn_nodes = HashMap::<String, usize>::new();
    for (component_index, component) in components.iter().enumerate() {
        for fd in component {
            fn_nodes.insert(fd.name.clone(), type_count + component_index);
        }
    }

    for (type_index, td) in type_defs.iter().enumerate() {
        let mut refs = HashSet::new();
        collect_type_def_refs(td, &mut refs);
        add_local_type_dependencies(type_index, &refs, &type_nodes, scope, &mut dependencies);

        if let Some(refined) =
            crate::codegen::common::find_refined_type_scoped(ctx, type_name(td), scope)
        {
            let mut predicate_fns = HashSet::new();
            collect_resolved_fn_refs(&refined.invariant.expr, &mut predicate_fns);
            for fn_id in predicate_fns {
                let name = &ctx.symbol_table.fn_entry(fn_id).key.name;
                if let Some(&fn_node) = fn_nodes.get(name) {
                    dependencies[type_index].insert(fn_node);
                }
            }
        }
    }

    for (component_index, component) in components.iter().enumerate() {
        let node = type_count + component_index;
        let mut called = HashSet::new();
        let mut type_refs = HashSet::new();
        for fd in component {
            collect_called_idents_in_body(&fd.body, &mut called);
            for (_, ty) in &fd.params {
                collect_annotation_type_refs(ty, &mut type_refs);
            }
            collect_annotation_type_refs(&fd.return_type, &mut type_refs);
            collect_body_type_refs(&fd.body, &mut type_refs);
        }
        for name in called {
            if let Some(&dependency) = fn_nodes.get(&name)
                && dependency != node
            {
                dependencies[node].insert(dependency);
            }
        }
        add_local_type_dependencies(node, &type_refs, &type_nodes, scope, &mut dependencies);
    }

    let mut emitted = vec![false; node_count];
    let mut node_order = Vec::with_capacity(node_count);
    while node_order.len() < node_count {
        let ready = (0..node_count).find(|&node| {
            !emitted[node]
                && dependencies[node]
                    .iter()
                    .all(|dependency| emitted[*dependency])
        });
        let Some(node) = ready else {
            // A declaration cycle such as `type T` depending on predicate `p`
            // while `p` itself accepts `T` has no ordinary Lean declaration
            // order. Preserve deterministic source order for the remaining
            // nodes; Lean then reports the cycle instead of the compiler
            // silently dropping a declaration or invariant.
            for (node, was_emitted) in emitted.iter_mut().enumerate() {
                if !*was_emitted {
                    *was_emitted = true;
                    node_order.push(node);
                }
            }
            break;
        };
        emitted[node] = true;
        node_order.push(node);
    }

    let order = node_order
        .into_iter()
        .map(|node| {
            if node < type_count {
                ScopedDecl::Type(node)
            } else {
                ScopedDecl::FnComponent(node - type_count)
            }
        })
        .collect();

    ScopedDeclPlan { components, order }
}

fn type_name(td: &TypeDef) -> &str {
    match td {
        TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
    }
}

fn collect_type_def_refs(td: &TypeDef, out: &mut HashSet<String>) {
    match td {
        TypeDef::Sum { variants, .. } => {
            for variant in variants {
                for field in &variant.fields {
                    collect_annotation_type_refs(field, out);
                }
            }
        }
        TypeDef::Product { fields, .. } => {
            for (_, field_type) in fields {
                collect_annotation_type_refs(field_type, out);
            }
        }
    }
}

fn collect_annotation_type_refs(annotation: &str, out: &mut HashSet<String>) {
    collect_type_refs(&crate::types::parse_type_str(annotation), out);
}

fn collect_type_refs(ty: &crate::types::Type, out: &mut HashSet<String>) {
    use crate::types::Type;
    match ty {
        Type::Named { name, .. } => {
            out.insert(name.clone());
        }
        Type::List(inner) | Type::Vector(inner) | Type::Option(inner) => {
            collect_type_refs(inner, out);
        }
        Type::Map(key, value) | Type::Result(key, value) => {
            collect_type_refs(key, out);
            collect_type_refs(value, out);
        }
        Type::Tuple(items) => {
            for item in items {
                collect_type_refs(item, out);
            }
        }
        Type::Fn(params, result, _) => {
            for param in params {
                collect_type_refs(param, out);
            }
            collect_type_refs(result, out);
        }
        Type::Int
        | Type::Float
        | Type::Str
        | Type::Bool
        | Type::Unit
        | Type::Var(_)
        | Type::Invalid => {}
    }
}

fn add_local_type_dependencies(
    node: usize,
    refs: &HashSet<String>,
    type_nodes: &HashMap<String, usize>,
    scope: Option<&str>,
    dependencies: &mut [HashSet<usize>],
) {
    for name in refs {
        let local_name = if let Some((prefix, bare)) = name.rsplit_once('.') {
            if scope == Some(prefix) {
                bare
            } else {
                continue;
            }
        } else {
            name.as_str()
        };
        if let Some(&dependency) = type_nodes.get(local_name)
            && dependency != node
        {
            dependencies[node].insert(dependency);
        }
    }
}

fn collect_called_idents_in_body(body: &FnBody, out: &mut HashSet<String>) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => collect_called_idents(expr, out),
        }
    }
}

fn collect_called_idents(expr: &Spanned<Expr>, out: &mut HashSet<String>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Expr::Ident(name) | Expr::Resolved { name, .. } = &callee.node {
                out.insert(name.clone());
            } else {
                collect_called_idents(callee, out);
            }
            for arg in args {
                collect_called_idents(arg, out);
            }
        }
        Expr::TailCall(call) => {
            out.insert(call.target.clone());
            for arg in &call.args {
                collect_called_idents(arg, out);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_called_idents(left, out);
            collect_called_idents(right, out);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            collect_called_idents(inner, out);
        }
        Expr::Match { subject, arms } => {
            collect_called_idents(subject, out);
            for arm in arms {
                collect_called_idents(&arm.body, out);
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_called_idents(inner, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    collect_called_idents(inner, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_called_idents(item, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_called_idents(key, out);
                collect_called_idents(value, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_called_idents(value, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_called_idents(base, out);
            for (_, value) in updates {
                collect_called_idents(value, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

fn collect_body_type_refs(body: &FnBody, out: &mut HashSet<String>) {
    for stmt in body.stmts() {
        match stmt {
            Stmt::Binding(_, annotation, expr) => {
                if let Some(annotation) = annotation {
                    collect_annotation_type_refs(annotation, out);
                }
                collect_expr_type_refs(expr, out);
            }
            Stmt::Expr(expr) => collect_expr_type_refs(expr, out),
        }
    }
}

fn collect_expr_type_refs(expr: &Spanned<Expr>, out: &mut HashSet<String>) {
    match &expr.node {
        Expr::RecordCreate {
            type_name, fields, ..
        } => {
            out.insert(type_name.clone());
            for (_, value) in fields {
                collect_expr_type_refs(value, out);
            }
        }
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
            ..
        } => {
            out.insert(type_name.clone());
            collect_expr_type_refs(base, out);
            for (_, value) in updates {
                collect_expr_type_refs(value, out);
            }
        }
        Expr::FnCall(callee, args) => {
            collect_expr_type_refs(callee, out);
            for arg in args {
                collect_expr_type_refs(arg, out);
            }
        }
        Expr::TailCall(call) => {
            for arg in &call.args {
                collect_expr_type_refs(arg, out);
            }
        }
        Expr::BinOp(_, left, right) => {
            collect_expr_type_refs(left, out);
            collect_expr_type_refs(right, out);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            collect_expr_type_refs(inner, out);
        }
        Expr::Match { subject, arms } => {
            collect_expr_type_refs(subject, out);
            for arm in arms {
                collect_expr_type_refs(&arm.body, out);
            }
        }
        Expr::Constructor(_, Some(inner)) => collect_expr_type_refs(inner, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    collect_expr_type_refs(inner, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_expr_type_refs(item, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_expr_type_refs(key, out);
                collect_expr_type_refs(value, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {}
    }
}

fn collect_resolved_fn_refs(expr: &Spanned<ResolvedExpr>, out: &mut HashSet<crate::ir::FnId>) {
    match &expr.node {
        ResolvedExpr::Call(callee, args) => {
            if let ResolvedCallee::Fn(fn_id) = callee {
                out.insert(*fn_id);
            } else if let ResolvedCallee::Unresolved { callee } = callee {
                collect_resolved_fn_refs(callee, out);
            }
            for arg in args {
                collect_resolved_fn_refs(arg, out);
            }
        }
        ResolvedExpr::TailCall { target, args } => {
            out.insert(*target);
            for arg in args {
                collect_resolved_fn_refs(arg, out);
            }
        }
        ResolvedExpr::Attr(inner, _)
        | ResolvedExpr::Neg(inner)
        | ResolvedExpr::ErrorProp(inner) => collect_resolved_fn_refs(inner, out),
        ResolvedExpr::BinOp(_, left, right) => {
            collect_resolved_fn_refs(left, out);
            collect_resolved_fn_refs(right, out);
        }
        ResolvedExpr::Match { subject, arms } => {
            collect_resolved_fn_refs(subject, out);
            for arm in arms {
                collect_resolved_fn_refs(&arm.body, out);
            }
        }
        ResolvedExpr::Ctor(_, args)
        | ResolvedExpr::List(args)
        | ResolvedExpr::Tuple(args)
        | ResolvedExpr::IndependentProduct(args, _) => {
            for arg in args {
                collect_resolved_fn_refs(arg, out);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_resolved_fn_refs(key, out);
                collect_resolved_fn_refs(value, out);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_resolved_fn_refs(value, out);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_resolved_fn_refs(base, out);
            for (_, value) in updates {
                collect_resolved_fn_refs(value, out);
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ir::hir::ResolvedStrPart::Parsed(inner) = part {
                    collect_resolved_fn_refs(inner, out);
                }
            }
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
    }
}
