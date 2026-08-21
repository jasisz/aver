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
    CapabilityOperation(usize),
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

    let entry_module = ctx.entry_module_name();
    let capability_operations: Vec<&crate::capability::CapabilityOperation> = ctx
        .capabilities
        .operations()
        .filter(|operation| !operation.is_effectful())
        .filter(|operation| match scope {
            Some(prefix) => operation.module == prefix,
            None => entry_module.as_deref() == Some(operation.module.as_str()),
        })
        .collect();

    let type_count = type_defs.len();
    let operation_count = capability_operations.len();
    let fn_offset = type_count + operation_count;
    let node_count = fn_offset + components.len();
    let mut dependencies = vec![HashSet::<usize>::new(); node_count];

    let type_nodes: HashMap<String, usize> = type_defs
        .iter()
        .enumerate()
        .map(|(index, td)| (type_name(td).to_string(), index))
        .collect();
    let mut fn_nodes = HashMap::<String, usize>::new();
    for (component_index, component) in components.iter().enumerate() {
        for fd in component {
            fn_nodes.insert(fd.name.clone(), fn_offset + component_index);
        }
    }
    let mut operation_nodes = HashMap::<String, usize>::new();
    for (operation_index, operation) in capability_operations.iter().enumerate() {
        let node = type_count + operation_index;
        operation_nodes.insert(operation.name.clone(), node);
        operation_nodes.insert(operation.canonical_name.clone(), node);

        let mut type_refs = HashSet::new();
        for (_, ty) in &operation.params {
            collect_type_refs(ty, &mut type_refs);
        }
        collect_type_refs(&operation.return_type, &mut type_refs);
        add_local_type_dependencies(node, &type_refs, &type_nodes, scope, &mut dependencies);
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
        let node = fn_offset + component_index;
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
            if let Some(&dependency) = operation_nodes.get(&name) {
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
            } else if node < fn_offset {
                ScopedDecl::CapabilityOperation(node - type_count)
            } else {
                ScopedDecl::FnComponent(node - fn_offset)
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

pub(super) fn collect_annotation_type_refs(annotation: &str, out: &mut HashSet<String>) {
    collect_type_refs(&crate::types::parse_type_str(annotation), out);
}

fn collect_type_refs(ty: &crate::types::Type, out: &mut HashSet<String>) {
    use crate::types::Type;
    match ty {
        // syntax-discovery-only: this per-scope declaration DAG compares the
        // source spelling against type declarations in the same scope; typed
        // identity has already selected that scope before this local walk.
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
            if let Some(name) = crate::ir::expr_to_dotted_name(&callee.node) {
                out.insert(name);
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

pub(super) fn collect_resolved_fn_refs(
    expr: &Spanned<ResolvedExpr>,
    out: &mut HashSet<crate::ir::FnId>,
) {
    for_each_resolved_callee(expr, &mut |callee| {
        if let ResolvedCallee::Fn(fn_id) = callee {
            out.insert(*fn_id);
        }
    });
}

/// Visit every callee `expr` can reach, tail calls included (reported as
/// `ResolvedCallee::Fn`).
pub(super) fn for_each_resolved_callee(
    expr: &Spanned<ResolvedExpr>,
    visit: &mut impl FnMut(&ResolvedCallee),
) {
    match &expr.node {
        ResolvedExpr::Call(callee, args) => {
            visit(callee);
            if let ResolvedCallee::Unresolved { callee } = callee {
                for_each_resolved_callee(callee, visit);
            }
            for arg in args {
                for_each_resolved_callee(arg, visit);
            }
        }
        ResolvedExpr::TailCall { target, args } => {
            visit(&ResolvedCallee::Fn(*target));
            for arg in args {
                for_each_resolved_callee(arg, visit);
            }
        }
        ResolvedExpr::Attr(inner, _)
        | ResolvedExpr::Neg(inner)
        | ResolvedExpr::ErrorProp(inner) => for_each_resolved_callee(inner, visit),
        ResolvedExpr::BinOp(_, left, right) => {
            for_each_resolved_callee(left, visit);
            for_each_resolved_callee(right, visit);
        }
        ResolvedExpr::Match { subject, arms } => {
            for_each_resolved_callee(subject, visit);
            for arm in arms {
                for_each_resolved_callee(&arm.body, visit);
            }
        }
        ResolvedExpr::Ctor(_, args)
        | ResolvedExpr::List(args)
        | ResolvedExpr::Tuple(args)
        | ResolvedExpr::IndependentProduct(args, _) => {
            for arg in args {
                for_each_resolved_callee(arg, visit);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (key, value) in entries {
                for_each_resolved_callee(key, visit);
                for_each_resolved_callee(value, visit);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                for_each_resolved_callee(value, visit);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            for_each_resolved_callee(base, visit);
            for (_, value) in updates {
                for_each_resolved_callee(value, visit);
            }
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ir::hir::ResolvedStrPart::Parsed(inner) = part {
                    for_each_resolved_callee(inner, visit);
                }
            }
        }
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
    }
}
