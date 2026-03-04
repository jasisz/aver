/// Lean 4 backend for the Aver transpiler.
///
/// Transpiles only pure core logic: functions without effects, type definitions,
/// verify blocks (as `example` proofs), and decision blocks (as comments).
/// Effectful functions and `main` are skipped.
mod builtins;
mod expr;
mod pattern;
mod shared;
mod toplevel;
mod types;

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Expr, FnBody, FnDef, MatchArm, Pattern, Stmt, TopLevel, VerifyKind};
use crate::call_graph;
use crate::codegen::{CodegenContext, ProjectOutput};

/// How verify blocks should be emitted in generated Lean.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum VerifyEmitMode {
    /// `example : lhs = rhs := by native_decide`
    NativeDecide,
    /// `example : lhs = rhs := by sorry`
    Sorry,
    /// Named theorem stubs:
    /// `theorem <fn>_verify_<n> : lhs = rhs := by`
    /// `  sorry`
    TheoremSkeleton,
}

/// Proof-mode recursion support class for emitted Lean definitions.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RecursionPlan {
    /// Single-function recursion where first `Int` parameter decreases by 1.
    IntCountdown,
    /// Single-function structural recursion on first `List<_>` parameter.
    ListStructural,
    /// Single-function recursion where first `String` stays the same and second `Int`
    /// parameter strictly advances (`pos + k`, `k >= 1`).
    StringPosAdvance,
    /// Mutual recursion group where first `Int` parameter decreases by 1 across SCC calls.
    MutualIntCountdown,
    /// Mutual recursion group where first `String` is preserved and second `Int`
    /// either stays the same across rank-decreasing edges, or strictly advances.
    MutualStringPosAdvance { rank: usize },
    /// Generic mutual recursion plan using `sizeOf` on a selected parameter,
    /// plus rank for same-parameter edges.
    MutualSizeOfRanked {
        rank: usize,
        metric_param_index: usize,
    },
}

fn pure_fns<'a>(ctx: &'a CodegenContext) -> Vec<&'a FnDef> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| toplevel::is_pure_fn(fd))
        .collect()
}

fn recursive_pure_fn_names(ctx: &CodegenContext) -> HashSet<String> {
    let pure_names: HashSet<String> = pure_fns(ctx)
        .into_iter()
        .map(|fd| fd.name.clone())
        .collect();
    let mut callgraph_items = ctx.items.clone();
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            callgraph_items.push(TopLevel::FnDef(fd.clone()));
        }
    }
    call_graph::find_recursive_fns(&callgraph_items)
        .into_iter()
        .filter(|name| pure_names.contains(name))
        .collect()
}

fn verify_counter_key(vb: &crate::ast::VerifyBlock) -> String {
    match &vb.kind {
        VerifyKind::Cases => format!("fn:{}", vb.fn_name),
        VerifyKind::Law(law) => format!("law:{}::{}", vb.fn_name, law.name),
    }
}

fn expr_to_dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(obj, field) => expr_to_dotted_name(obj).map(|p| format!("{}.{}", p, field)),
        _ => None,
    }
}

fn call_matches(name: &str, target: &str) -> bool {
    name == target || name.rsplit('.').next() == Some(target)
}

fn call_is_in_set(name: &str, targets: &HashSet<String>) -> bool {
    call_matches_any(name, targets)
}

fn canonical_callee_name(name: &str, targets: &HashSet<String>) -> Option<String> {
    if targets.contains(name) {
        return Some(name.to_string());
    }
    name.rsplit('.')
        .next()
        .filter(|last| targets.contains(*last))
        .map(ToString::to_string)
}

fn call_matches_any(name: &str, targets: &HashSet<String>) -> bool {
    if targets.contains(name) {
        return true;
    }
    match name.rsplit('.').next() {
        Some(last) => targets.contains(last),
        None => false,
    }
}

fn is_int_minus_one(expr: &Expr, param_name: &str) -> bool {
    match expr {
        Expr::BinOp(BinOp::Sub, left, right) => {
            matches!(left.as_ref(), Expr::Ident(id) if id == param_name)
                && matches!(right.as_ref(), Expr::Literal(crate::ast::Literal::Int(1)))
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = expr_to_dotted_name(callee) else {
                return false;
            };
            (name == "Int.sub" || name == "int.sub")
                && args.len() == 2
                && matches!(&args[0], Expr::Ident(id) if id == param_name)
                && matches!(&args[1], Expr::Literal(crate::ast::Literal::Int(1)))
        }
        _ => false,
    }
}

fn collect_calls_from_expr<'a>(expr: &'a Expr, out: &mut Vec<(String, Vec<&'a Expr>)>) {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(callee) {
                out.push((name, args.iter().collect()));
            }
            collect_calls_from_expr(callee, out);
            for arg in args {
                collect_calls_from_expr(arg, out);
            }
        }
        Expr::TailCall(boxed) => {
            let (name, args) = boxed.as_ref();
            out.push((name.clone(), args.iter().collect()));
            for arg in args {
                collect_calls_from_expr(arg, out);
            }
        }
        Expr::Attr(obj, _) => collect_calls_from_expr(obj, out),
        Expr::BinOp(_, left, right) => {
            collect_calls_from_expr(left, out);
            collect_calls_from_expr(right, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_calls_from_expr(subject, out);
            for arm in arms {
                collect_calls_from_expr(&arm.body, out);
            }
        }
        Expr::Pipe(left, right) => {
            collect_calls_from_expr(left, out);
            collect_calls_from_expr(right, out);
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_calls_from_expr(inner, out);
            }
        }
        Expr::ErrorProp(inner) => collect_calls_from_expr(inner, out),
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_calls_from_expr(e, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_calls_from_expr(item, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_calls_from_expr(k, out);
                collect_calls_from_expr(v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_calls_from_expr(v, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_calls_from_expr(base, out);
            for (_, v) in updates {
                collect_calls_from_expr(v, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

fn collect_calls_from_body<'a>(body: &'a FnBody) -> Vec<(String, Vec<&'a Expr>)> {
    let mut out = Vec::new();
    match body {
        FnBody::Expr(expr) => collect_calls_from_expr(expr, &mut out),
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                        collect_calls_from_expr(expr, &mut out)
                    }
                }
            }
        }
    }
    out
}

fn collect_list_tail_binders_from_expr(
    expr: &Expr,
    list_param_name: &str,
    tails: &mut HashSet<String>,
) {
    match expr {
        Expr::Match { subject, arms, .. } => {
            if matches!(subject.as_ref(), Expr::Ident(id) if id == list_param_name) {
                for MatchArm { pattern, .. } in arms {
                    if let Pattern::Cons(_, tail) = pattern {
                        tails.insert(tail.clone());
                    }
                }
            }
            for arm in arms {
                collect_list_tail_binders_from_expr(&arm.body, list_param_name, tails);
            }
            collect_list_tail_binders_from_expr(subject, list_param_name, tails);
        }
        Expr::FnCall(callee, args) => {
            collect_list_tail_binders_from_expr(callee, list_param_name, tails);
            for arg in args {
                collect_list_tail_binders_from_expr(arg, list_param_name, tails);
            }
        }
        Expr::TailCall(boxed) => {
            let (_, args) = boxed.as_ref();
            for arg in args {
                collect_list_tail_binders_from_expr(arg, list_param_name, tails);
            }
        }
        Expr::Attr(obj, _) => collect_list_tail_binders_from_expr(obj, list_param_name, tails),
        Expr::BinOp(_, left, right) | Expr::Pipe(left, right) => {
            collect_list_tail_binders_from_expr(left, list_param_name, tails);
            collect_list_tail_binders_from_expr(right, list_param_name, tails);
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_list_tail_binders_from_expr(inner, list_param_name, tails);
            }
        }
        Expr::ErrorProp(inner) => {
            collect_list_tail_binders_from_expr(inner, list_param_name, tails)
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(e) = p {
                    collect_list_tail_binders_from_expr(e, list_param_name, tails);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_list_tail_binders_from_expr(item, list_param_name, tails);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_list_tail_binders_from_expr(k, list_param_name, tails);
                collect_list_tail_binders_from_expr(v, list_param_name, tails);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_list_tail_binders_from_expr(v, list_param_name, tails);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_list_tail_binders_from_expr(base, list_param_name, tails);
            for (_, v) in updates {
                collect_list_tail_binders_from_expr(v, list_param_name, tails);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

fn collect_list_tail_binders(fd: &FnDef, list_param_name: &str) -> HashSet<String> {
    let mut tails = HashSet::new();
    match fd.body.as_ref() {
        FnBody::Expr(expr) => {
            collect_list_tail_binders_from_expr(expr, list_param_name, &mut tails)
        }
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                        collect_list_tail_binders_from_expr(expr, list_param_name, &mut tails)
                    }
                }
            }
        }
    }
    tails
}

fn supports_single_int_countdown(fd: &FnDef) -> bool {
    let Some((param_name, param_ty)) = fd.params.first() else {
        return false;
    };
    if param_ty != "Int" {
        return false;
    }
    let recursive_calls: Vec<Option<&Expr>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args.first().copied())
        .collect();
    if recursive_calls.is_empty() {
        return false;
    }
    recursive_calls
        .into_iter()
        .all(|arg0| arg0.is_some_and(|e| is_int_minus_one(e, param_name)))
}

fn supports_single_list_structural(fd: &FnDef) -> bool {
    let Some((param_name, param_ty)) = fd.params.first() else {
        return false;
    };
    if !(param_ty.starts_with("List<") || param_ty == "List") {
        return false;
    }

    let tails = collect_list_tail_binders(fd, param_name);
    if tails.is_empty() {
        return false;
    }

    let recursive_calls: Vec<Option<&Expr>> = collect_calls_from_body(fd.body.as_ref())
        .into_iter()
        .filter(|(name, _)| call_matches(name, &fd.name))
        .map(|(_, args)| args.first().copied())
        .collect();
    if recursive_calls.is_empty() {
        return false;
    }

    recursive_calls.into_iter().all(|arg0| {
        matches!(
            arg0,
            Some(Expr::Ident(id)) if tails.contains(id)
        )
    })
}

fn is_ident(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Ident(id) if id == name)
}

fn is_int_plus_positive(expr: &Expr, param_name: &str) -> bool {
    match expr {
        Expr::BinOp(BinOp::Add, left, right) => {
            matches!(left.as_ref(), Expr::Ident(id) if id == param_name)
                && matches!(right.as_ref(), Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 1)
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = expr_to_dotted_name(callee) else {
                return false;
            };
            (name == "Int.add" || name == "int.add")
                && args.len() == 2
                && matches!(&args[0], Expr::Ident(id) if id == param_name)
                && matches!(&args[1], Expr::Literal(crate::ast::Literal::Int(n)) if *n >= 1)
        }
        _ => false,
    }
}

fn is_skip_ws_advance(expr: &Expr, string_param: &str, pos_param: &str) -> bool {
    let Expr::FnCall(callee, args) = expr else {
        return false;
    };
    let Some(name) = expr_to_dotted_name(callee) else {
        return false;
    };
    if !call_matches(&name, "skipWs") || args.len() != 2 {
        return false;
    }
    is_ident(&args[0], string_param) && is_int_plus_positive(&args[1], pos_param)
}

fn is_skip_ws_same(expr: &Expr, string_param: &str, pos_param: &str) -> bool {
    let Expr::FnCall(callee, args) = expr else {
        return false;
    };
    let Some(name) = expr_to_dotted_name(callee) else {
        return false;
    };
    if !call_matches(&name, "skipWs") || args.len() != 2 {
        return false;
    }
    is_ident(&args[0], string_param) && is_ident(&args[1], pos_param)
}

fn is_string_pos_advance(expr: &Expr, string_param: &str, pos_param: &str) -> bool {
    is_int_plus_positive(expr, pos_param) || is_skip_ws_advance(expr, string_param, pos_param)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum StringPosEdge {
    Same,
    Advance,
}

fn classify_string_pos_edge(
    expr: &Expr,
    string_param: &str,
    pos_param: &str,
) -> Option<StringPosEdge> {
    if is_ident(expr, pos_param) || is_skip_ws_same(expr, string_param, pos_param) {
        return Some(StringPosEdge::Same);
    }
    if is_string_pos_advance(expr, string_param, pos_param) {
        return Some(StringPosEdge::Advance);
    }
    if let Expr::FnCall(callee, args) = expr {
        let name = expr_to_dotted_name(callee)?;
        if call_matches(&name, "skipWs") && args.len() == 2 && is_ident(&args[0], string_param) {
            if matches!(&args[1], Expr::Ident(id) if id != pos_param) {
                return Some(StringPosEdge::Advance);
            }
        }
    }
    if matches!(expr, Expr::Ident(id) if id != pos_param) {
        return Some(StringPosEdge::Advance);
    }
    None
}

fn ranks_from_same_edges(
    names: &HashSet<String>,
    same_edges: &HashMap<String, HashSet<String>>,
) -> Option<HashMap<String, usize>> {
    let mut indegree: HashMap<String, usize> = names.iter().map(|n| (n.clone(), 0)).collect();
    for outs in same_edges.values() {
        for to in outs {
            if let Some(entry) = indegree.get_mut(to) {
                *entry += 1;
            } else {
                return None;
            }
        }
    }

    let mut queue: Vec<String> = indegree
        .iter()
        .filter_map(|(name, &deg)| (deg == 0).then_some(name.clone()))
        .collect();
    queue.sort();
    let mut topo = Vec::new();
    while let Some(node) = queue.pop() {
        topo.push(node.clone());
        let outs = same_edges.get(&node).cloned().unwrap_or_default();
        let mut newly_zero = Vec::new();
        for to in outs {
            if let Some(entry) = indegree.get_mut(&to) {
                *entry -= 1;
                if *entry == 0 {
                    newly_zero.push(to);
                }
            } else {
                return None;
            }
        }
        newly_zero.sort();
        queue.extend(newly_zero);
    }

    if topo.len() != names.len() {
        return None;
    }

    let n = topo.len();
    let mut ranks = HashMap::new();
    for (idx, name) in topo.into_iter().enumerate() {
        ranks.insert(name, n - idx);
    }
    Some(ranks)
}

fn supports_single_string_pos_advance(fd: &FnDef) -> bool {
    let Some((string_param, string_ty)) = fd.params.first() else {
        return false;
    };
    let Some((pos_param, pos_ty)) = fd.params.get(1) else {
        return false;
    };
    if string_ty != "String" || pos_ty != "Int" {
        return false;
    }

    let recursive_calls: Vec<(Option<&Expr>, Option<&Expr>)> =
        collect_calls_from_body(fd.body.as_ref())
            .into_iter()
            .filter(|(name, _)| call_matches(name, &fd.name))
            .map(|(_, args)| (args.first().copied(), args.get(1).copied()))
            .collect();
    if recursive_calls.is_empty() {
        return false;
    }

    recursive_calls.into_iter().all(|(arg0, arg1)| {
        arg0.is_some_and(|e| is_ident(e, string_param))
            && arg1.is_some_and(|e| is_string_pos_advance(e, string_param, pos_param))
    })
}

fn supports_mutual_int_countdown(component: &[&FnDef]) -> bool {
    if component.len() < 2 {
        return false;
    }
    if component
        .iter()
        .any(|fd| !matches!(fd.params.first(), Some((_, t)) if t == "Int"))
    {
        return false;
    }
    let names: HashSet<String> = component.iter().map(|fd| fd.name.clone()).collect();
    let mut any_intra = false;
    for fd in component {
        let param_name = &fd.params[0].0;
        for (callee, args) in collect_calls_from_body(fd.body.as_ref()) {
            if !call_is_in_set(&callee, &names) {
                continue;
            }
            any_intra = true;
            let Some(arg0) = args.first().copied() else {
                return false;
            };
            if !is_int_minus_one(arg0, param_name) {
                return false;
            }
        }
    }
    any_intra
}

fn supports_mutual_string_pos_advance(component: &[&FnDef]) -> Option<HashMap<String, usize>> {
    if component.len() < 2 {
        return None;
    }
    if component.iter().any(|fd| {
        !matches!(fd.params.first(), Some((_, t)) if t == "String")
            || !matches!(fd.params.get(1), Some((_, t)) if t == "Int")
    }) {
        return None;
    }

    let names: HashSet<String> = component.iter().map(|fd| fd.name.clone()).collect();
    let mut same_edges: HashMap<String, HashSet<String>> =
        names.iter().map(|n| (n.clone(), HashSet::new())).collect();
    let mut any_intra = false;

    for fd in component {
        let string_param = &fd.params[0].0;
        let pos_param = &fd.params[1].0;
        for (callee_raw, args) in collect_calls_from_body(fd.body.as_ref()) {
            let Some(callee) = canonical_callee_name(&callee_raw, &names) else {
                continue;
            };
            any_intra = true;

            let Some(arg0) = args.first().copied() else {
                return None;
            };
            let Some(arg1) = args.get(1).copied() else {
                return None;
            };

            if !is_ident(arg0, string_param) {
                return None;
            }

            match classify_string_pos_edge(arg1, string_param, pos_param) {
                Some(StringPosEdge::Same) => {
                    if let Some(edges) = same_edges.get_mut(&fd.name) {
                        edges.insert(callee);
                    } else {
                        return None;
                    }
                }
                Some(StringPosEdge::Advance) => {}
                None => return None,
            }
        }
    }

    if !any_intra {
        return None;
    }

    ranks_from_same_edges(&names, &same_edges)
}

fn metric_param_index_for_fn(fd: &FnDef) -> usize {
    if fd.params.len() >= 2
        && fd.params[0].1 == "String"
        && (fd.params[1].1 == "Int"
            || fd.params[1].1 == "List"
            || fd.params[1].1.starts_with("List<"))
    {
        return 1;
    }
    0
}

fn supports_mutual_sizeof_ranked(component: &[&FnDef]) -> Option<HashMap<String, (usize, usize)>> {
    if component.len() < 2 {
        return None;
    }
    let names: HashSet<String> = component.iter().map(|fd| fd.name.clone()).collect();
    let metric_idx: HashMap<String, usize> = component
        .iter()
        .map(|fd| (fd.name.clone(), metric_param_index_for_fn(fd)))
        .collect();
    if component.iter().any(|fd| {
        let idx = metric_idx.get(&fd.name).copied().unwrap_or(usize::MAX);
        idx >= fd.params.len()
    }) {
        return None;
    }

    let mut same_edges: HashMap<String, HashSet<String>> =
        names.iter().map(|n| (n.clone(), HashSet::new())).collect();
    let mut any_intra = false;
    for fd in component {
        let caller_metric_idx = metric_idx.get(&fd.name).copied()?;
        let caller_metric_param = &fd.params[caller_metric_idx].0;
        for (callee_raw, args) in collect_calls_from_body(fd.body.as_ref()) {
            let Some(callee) = canonical_callee_name(&callee_raw, &names) else {
                continue;
            };
            any_intra = true;
            let callee_metric_idx = metric_idx.get(&callee).copied()?;
            let Some(metric_arg) = args.get(callee_metric_idx).copied() else {
                return None;
            };
            if is_ident(metric_arg, caller_metric_param) {
                if let Some(edges) = same_edges.get_mut(&fd.name) {
                    edges.insert(callee);
                } else {
                    return None;
                }
            }
        }
    }
    if !any_intra {
        return None;
    }

    let ranks = ranks_from_same_edges(&names, &same_edges)?;
    let mut out = HashMap::new();
    for fd in component {
        let rank = ranks.get(&fd.name).copied()?;
        let idx = metric_idx.get(&fd.name).copied()?;
        out.insert(fd.name.clone(), (rank, idx));
    }
    Some(out)
}

fn proof_mode_recursion_analysis(
    ctx: &CodegenContext,
) -> (HashMap<String, RecursionPlan>, Vec<String>) {
    let mut plans = HashMap::new();
    let mut issues = Vec::new();

    let all_pure = pure_fns(ctx);
    let recursive_names = recursive_pure_fn_names(ctx);
    let components = call_graph::ordered_fn_components(&all_pure);

    for component in components {
        if component.is_empty() {
            continue;
        }
        let is_recursive_component =
            component.len() > 1 || recursive_names.contains(&component[0].name);
        if !is_recursive_component {
            continue;
        }

        if component.len() > 1 {
            if supports_mutual_int_countdown(&component) {
                for fd in &component {
                    plans.insert(fd.name.clone(), RecursionPlan::MutualIntCountdown);
                }
            } else if let Some(ranks) = supports_mutual_string_pos_advance(&component) {
                for fd in &component {
                    if let Some(rank) = ranks.get(&fd.name).copied() {
                        plans.insert(
                            fd.name.clone(),
                            RecursionPlan::MutualStringPosAdvance { rank },
                        );
                    } else {
                        issues.push(format!(
                            "internal proof-mode error: missing mutual String+pos rank for function '{}'",
                            fd.name
                        ));
                    }
                }
            } else if let Some(rank_idx) = supports_mutual_sizeof_ranked(&component) {
                for fd in &component {
                    if let Some((rank, metric_param_index)) = rank_idx.get(&fd.name).copied() {
                        plans.insert(
                            fd.name.clone(),
                            RecursionPlan::MutualSizeOfRanked {
                                rank,
                                metric_param_index,
                            },
                        );
                    } else {
                        issues.push(format!(
                            "internal proof-mode error: missing mutual sizeOf plan for function '{}'",
                            fd.name
                        ));
                    }
                }
            } else {
                let names = component
                    .iter()
                    .map(|fd| fd.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                issues.push(format!(
                    "unsupported mutual recursion group (supported: Int countdown on first param, String+pos advance, or ranked-sizeOf mutual plan): {}",
                    names
                ));
            }
            continue;
        }

        let fd = component[0];
        if supports_single_int_countdown(fd) {
            plans.insert(fd.name.clone(), RecursionPlan::IntCountdown);
        } else if supports_single_list_structural(fd) {
            plans.insert(fd.name.clone(), RecursionPlan::ListStructural);
        } else if supports_single_string_pos_advance(fd) {
            plans.insert(fd.name.clone(), RecursionPlan::StringPosAdvance);
        } else {
            issues.push(format!(
                "recursive function '{}' is outside proof subset (supported: Int countdown, List structural, String+pos advance, or supported mutual recursion class)",
                fd.name
            ));
        }
    }

    (plans, issues)
}

/// Strict proof-mode gate for Lean transpilation.
///
/// Returns human-readable blockers that would force generated Lean to rely on
/// non-proof recursion fallback (`partial`).
pub fn proof_mode_issues(ctx: &CodegenContext) -> Vec<String> {
    let (_plans, issues) = proof_mode_recursion_analysis(ctx);
    issues
}

/// Transpile an Aver program to a Lean 4 project.
pub fn transpile(ctx: &CodegenContext) -> ProjectOutput {
    transpile_with_verify_mode(ctx, VerifyEmitMode::NativeDecide)
}

/// Proof-mode transpilation.
///
/// Uses recursion plans validated by `proof_mode_issues` and emits supported
/// recursive functions without `partial`, adding `termination_by` scaffolding.
pub fn transpile_for_proof_mode(
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
) -> ProjectOutput {
    let (plans, _issues) = proof_mode_recursion_analysis(ctx);

    let mut sections = Vec::new();
    sections.push(generate_prelude());
    sections.push(String::new());

    // Reuse the same type emission as standard mode.
    for module in &ctx.modules {
        for td in &module.type_defs {
            sections.push(toplevel::emit_type_def(td));
            if toplevel::is_recursive_type_def(td) {
                sections.push(toplevel::emit_recursive_decidable_eq(
                    toplevel::type_def_name(td),
                ));
            }
            sections.push(String::new());
        }
    }
    for td in &ctx.type_defs {
        sections.push(toplevel::emit_type_def(td));
        if toplevel::is_recursive_type_def(td) {
            sections.push(toplevel::emit_recursive_decidable_eq(
                toplevel::type_def_name(td),
            ));
        }
        sections.push(String::new());
    }

    emit_pure_functions_proof(ctx, &plans, &mut sections);

    for item in &ctx.items {
        if let TopLevel::Decision(db) = item {
            sections.push(toplevel::emit_decision(db));
            sections.push(String::new());
        }
    }

    let mut verify_case_counters: HashMap<String, usize> = HashMap::new();
    for item in &ctx.items {
        if let TopLevel::Verify(vb) = item {
            let key = verify_counter_key(vb);
            let start_idx = *verify_case_counters.get(&key).unwrap_or(&0);
            let (emitted, next_idx) = toplevel::emit_verify_block(vb, ctx, verify_mode, start_idx);
            verify_case_counters.insert(key, next_idx);
            sections.push(emitted);
            sections.push(String::new());
        }
    }

    let lean_source = sections.join("\n");
    let project_name = capitalize_first(&ctx.project_name);
    let lakefile = generate_lakefile(&project_name);
    let toolchain = generate_toolchain();
    ProjectOutput {
        files: vec![
            ("lakefile.lean".to_string(), lakefile),
            ("lean-toolchain".to_string(), toolchain),
            (format!("{}.lean", project_name), lean_source),
        ],
    }
}

/// Transpile an Aver program to a Lean 4 project with configurable verify proof mode.
///
/// - `NativeDecide` emits `example ... := by native_decide`
/// - `Sorry` emits `example ... := by sorry`
/// - `TheoremSkeleton` emits named theorem skeletons with `sorry`
pub fn transpile_with_verify_mode(
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
) -> ProjectOutput {
    let mut sections = Vec::new();

    // Prelude
    sections.push(generate_prelude());
    sections.push(String::new());

    // Detect recursive functions for `partial` annotation
    let recursive_fns = call_graph::find_recursive_fns(&ctx.items);

    // Module type definitions (from depends)
    for module in &ctx.modules {
        for td in &module.type_defs {
            sections.push(toplevel::emit_type_def(td));
            // #18: Recursive types need unsafe DecidableEq instance
            if toplevel::is_recursive_type_def(td) {
                sections.push(toplevel::emit_recursive_decidable_eq(
                    toplevel::type_def_name(td),
                ));
            }
            sections.push(String::new());
        }
    }

    // Type definitions
    for td in &ctx.type_defs {
        sections.push(toplevel::emit_type_def(td));
        // #18: Recursive types need unsafe DecidableEq instance
        if toplevel::is_recursive_type_def(td) {
            sections.push(toplevel::emit_recursive_decidable_eq(
                toplevel::type_def_name(td),
            ));
        }
        sections.push(String::new());
    }

    // Emit pure functions in SCC-topological order:
    // callees first, then callers; SCCs as `mutual` blocks.
    emit_pure_functions(ctx, &recursive_fns, &mut sections);

    // Decision blocks (as comments)
    for item in &ctx.items {
        if let TopLevel::Decision(db) = item {
            sections.push(toplevel::emit_decision(db));
            sections.push(String::new());
        }
    }

    // Verify blocks → proof items (`native_decide` by default, optional `sorry`/theorem stubs).
    let mut verify_case_counters: HashMap<String, usize> = HashMap::new();
    for item in &ctx.items {
        if let TopLevel::Verify(vb) = item {
            let key = verify_counter_key(vb);
            let start_idx = *verify_case_counters.get(&key).unwrap_or(&0);
            let (emitted, next_idx) = toplevel::emit_verify_block(vb, ctx, verify_mode, start_idx);
            verify_case_counters.insert(key, next_idx);
            sections.push(emitted);
            sections.push(String::new());
        }
    }

    let lean_source = sections.join("\n");

    // Project files
    let project_name = capitalize_first(&ctx.project_name);
    let lakefile = generate_lakefile(&project_name);
    let toolchain = generate_toolchain();

    ProjectOutput {
        files: vec![
            ("lakefile.lean".to_string(), lakefile),
            ("lean-toolchain".to_string(), toolchain),
            (format!("{}.lean", project_name), lean_source),
        ],
    }
}

fn emit_pure_functions(
    ctx: &CodegenContext,
    recursive_fns: &HashSet<String>,
    sections: &mut Vec<String>,
) {
    let all_fns: Vec<&FnDef> = ctx
        .modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| toplevel::is_pure_fn(fd))
        .collect();
    if all_fns.is_empty() {
        return;
    }

    let components = call_graph::ordered_fn_components(&all_fns);
    for fns in components {
        if fns.is_empty() {
            continue;
        }

        // Multi-node SCC => emit `mutual ... end`.
        if fns.len() > 1 {
            sections.push(toplevel::emit_mutual_group(&fns, ctx));
            sections.push(String::new());
            continue;
        }

        // Singleton SCC => regular `def` (recursive singletons still get `partial`
        // via `recursive_fns` in emit_fn_def).
        if let Some(code) = toplevel::emit_fn_def(fns[0], recursive_fns, ctx) {
            sections.push(code);
            sections.push(String::new());
        }
    }
}

fn emit_pure_functions_proof(
    ctx: &CodegenContext,
    plans: &HashMap<String, RecursionPlan>,
    sections: &mut Vec<String>,
) {
    let all_fns: Vec<&FnDef> = pure_fns(ctx);
    if all_fns.is_empty() {
        return;
    }

    let components = call_graph::ordered_fn_components(&all_fns);
    for fns in components {
        if fns.is_empty() {
            continue;
        }
        if fns.len() > 1 {
            sections.push(toplevel::emit_mutual_group_proof(&fns, ctx, plans));
            sections.push(String::new());
            continue;
        }

        if let Some(code) =
            toplevel::emit_fn_def_proof(fns[0], plans.get(&fns[0].name).copied(), ctx)
        {
            sections.push(code);
            sections.push(String::new());
        }
    }
}

fn generate_prelude() -> String {
    let mut lines = Vec::new();
    lines.push("-- Generated by the Aver → Lean 4 transpiler".to_string());
    lines.push("-- Pure core logic only (effectful functions are omitted)".to_string());
    lines.push(String::new());
    lines.push("-- Prelude: helper definitions for Aver builtins".to_string());
    lines.push(String::new());

    // #2: Int → Float coercion
    lines.push("instance : Coe Int Float := ⟨fun n => Float.ofInt n⟩".to_string());
    lines.push(String::new());

    // #6: Float DecidableEq (Float is opaque in Lean kernel)
    // unsafe + @[implemented_by] pattern: computable, native_decide works, deriving works
    lines.push(
        "private unsafe def Float.unsafeDecEq (a b : Float) : Decidable (a = b) :=".to_string(),
    );
    lines.push("  if a == b then isTrue (unsafeCast ()) else isFalse (unsafeCast ())".to_string());
    lines.push("@[implemented_by Float.unsafeDecEq]".to_string());
    lines.push("private opaque Float.compDecEq (a b : Float) : Decidable (a = b)".to_string());
    lines.push("instance : DecidableEq Float := Float.compDecEq".to_string());
    lines.push(String::new());

    // #7: Except DecidableEq
    lines.push("instance [DecidableEq ε] [DecidableEq α] : DecidableEq (Except ε α)".to_string());
    lines.push("  | .ok a, .ok b =>".to_string());
    lines.push(
        "    if h : a = b then isTrue (h ▸ rfl) else isFalse (by intro h'; cases h'; exact h rfl)"
            .to_string(),
    );
    lines.push("  | .error a, .error b =>".to_string());
    lines.push(
        "    if h : a = b then isTrue (h ▸ rfl) else isFalse (by intro h'; cases h'; exact h rfl)"
            .to_string(),
    );
    lines.push("  | .ok _, .error _ => isFalse (by intro h; cases h)".to_string());
    lines.push("  | .error _, .ok _ => isFalse (by intro h; cases h)".to_string());
    lines.push(String::new());

    // Except.withDefault
    lines.push("namespace Except".to_string());
    lines.push("def withDefault (r : Except ε α) (d : α) : α :=".to_string());
    lines.push("  match r with".to_string());
    lines.push("  | .ok v => v".to_string());
    lines.push("  | .error _ => d".to_string());
    lines.push("end Except".to_string());
    lines.push(String::new());

    // Option.toExcept
    lines.push("def Option.toExcept (o : Option α) (e : ε) : Except ε α :=".to_string());
    lines.push("  match o with".to_string());
    lines.push("  | some v => .ok v".to_string());
    lines.push("  | none => .error e".to_string());
    lines.push(String::new());

    // #13: String + → String.append (Lean uses ++ not +)
    lines.push("instance : HAdd String String String := ⟨String.append⟩".to_string());
    lines.push(String::new());

    // Map helpers (Map<K,V> = List (K × V))
    lines.push("namespace AverMap".to_string());
    lines.push("def empty : List (α × β) := []".to_string());
    lines.push("def get [BEq α] (m : List (α × β)) (k : α) : Option β :=".to_string());
    lines.push("  match m with".to_string());
    lines.push("  | [] => none".to_string());
    lines.push("  | (k', v) :: rest => if k == k' then some v else AverMap.get rest k".to_string());
    lines.push("def set [BEq α] (m : List (α × β)) (k : α) (v : β) : List (α × β) :=".to_string());
    lines.push("  let rec go : List (α × β) → List (α × β)".to_string());
    lines.push("    | [] => [(k, v)]".to_string());
    lines.push(
        "    | (k', v') :: rest => if k == k' then (k, v) :: rest else (k', v') :: go rest"
            .to_string(),
    );
    lines.push("  go m".to_string());
    lines.push("def has [BEq α] (m : List (α × β)) (k : α) : Bool :=".to_string());
    lines.push("  m.any (fun p => p.1 == k)".to_string());
    lines.push("def remove [BEq α] (m : List (α × β)) (k : α) : List (α × β) :=".to_string());
    lines.push("  m.filter (fun p => !(p.1 == k))".to_string());
    lines.push("def keys (m : List (α × β)) : List α := m.map Prod.fst".to_string());
    lines.push("def values (m : List (α × β)) : List β := m.map Prod.snd".to_string());
    lines.push("def entries (m : List (α × β)) : List (α × β) := m".to_string());
    lines.push("def len (m : List (α × β)) : Nat := m.length".to_string());
    lines.push("def fromList (entries : List (α × β)) : List (α × β) := entries".to_string());
    lines.push("end AverMap".to_string());
    lines.push(String::new());

    lines.push("namespace AverList".to_string());
    lines.push("private def insertSorted [Ord α] (x : α) : List α → List α".to_string());
    lines.push("  | [] => [x]".to_string());
    lines.push("  | y :: ys =>".to_string());
    lines.push("    if compare x y == Ordering.lt || compare x y == Ordering.eq then".to_string());
    lines.push("      x :: y :: ys".to_string());
    lines.push("    else".to_string());
    lines.push("      y :: insertSorted x ys".to_string());
    lines.push("def sort [Ord α] (xs : List α) : List α :=".to_string());
    lines.push("  xs.foldl (fun acc x => insertSorted x acc) []".to_string());
    lines.push("end AverList".to_string());
    lines.push(String::new());

    // String helpers (Aver has no Char type — single chars are strings)
    // charAt/slice are code-point based to match interpreter semantics.
    lines.push("def String.charAt (s : String) (i : Int) : Option String :=".to_string());
    lines.push("  if i < 0 then none".to_string());
    lines.push("  else (s.toList.get? i.toNat).map Char.toString".to_string());
    lines.push("def String.slice (s : String) (start stop : Int) : String :=".to_string());
    lines.push("  let startN := if start < 0 then 0 else start.toNat".to_string());
    lines.push("  let stopN := if stop < 0 then 0 else stop.toNat".to_string());
    lines.push("  let chars := s.toList".to_string());
    lines.push("  String.mk ((chars.drop startN).take (stopN - startN))".to_string());
    lines.push(
        "private def trimFloatTrailingZerosChars (chars : List Char) : List Char :=".to_string(),
    );
    lines
        .push("  let noZeros := (chars.reverse.dropWhile (fun c => c == '0')).reverse".to_string());
    lines.push("  match noZeros.reverse with".to_string());
    lines.push("  | '.' :: rest => rest.reverse".to_string());
    lines.push("  | _ => noZeros".to_string());
    lines.push("private def normalizeFloatString (s : String) : String :=".to_string());
    lines.push("  if s.toList.any (fun c => c == '.') then".to_string());
    lines.push("    String.mk (trimFloatTrailingZerosChars s.toList)".to_string());
    lines.push("  else s".to_string());
    lines.push("def String.fromInt (n : Int) : String := toString n".to_string());
    lines.push(
        "def String.fromFloat (f : Float) : String := normalizeFloatString (toString f)"
            .to_string(),
    );
    lines.push(
        "def String.chars (s : String) : List String := s.toList.map Char.toString".to_string(),
    );
    lines.push("namespace AverString".to_string());
    lines.push("def split (s delim : String) : List String :=".to_string());
    lines.push("  if delim.isEmpty then".to_string());
    lines.push("    \"\" :: (s.toList.map Char.toString) ++ [\"\"]".to_string());
    lines.push("  else".to_string());
    lines.push("    s.splitOn delim".to_string());
    lines.push("end AverString".to_string());
    lines.push(String::new());

    // Int/Float parsing (Aver returns Result<T, String>)
    lines.push("def Int.fromString (s : String) : Except String Int :=".to_string());
    lines.push("  match s.toInt? with".to_string());
    lines.push("  | some n => .ok n".to_string());
    lines.push("  | none => .error (\"Invalid integer: \" ++ s)".to_string());
    lines.push(String::new());

    // #19: Float.fromString — real parser using Float.ofScientific
    lines.push("private def charDigitsToNat (cs : List Char) : Nat :=".to_string());
    lines.push("  cs.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0".to_string());
    lines.push(String::new());
    lines.push("private def parseExpPart : List Char → (Bool × List Char)".to_string());
    lines.push("  | '-' :: rest => (true, rest.takeWhile Char.isDigit)".to_string());
    lines.push("  | '+' :: rest => (false, rest.takeWhile Char.isDigit)".to_string());
    lines.push("  | rest => (false, rest.takeWhile Char.isDigit)".to_string());
    lines.push(String::new());
    lines.push("def Float.fromString (s : String) : Except String Float :=".to_string());
    lines.push("  let chars := s.toList".to_string());
    lines.push("  let (neg, chars) := match chars with".to_string());
    lines.push("    | '-' :: rest => (true, rest)".to_string());
    lines.push("    | _ => (false, chars)".to_string());
    lines.push("  let intPart := chars.takeWhile Char.isDigit".to_string());
    lines.push("  let rest := chars.dropWhile Char.isDigit".to_string());
    lines.push("  let (fracPart, rest) := match rest with".to_string());
    lines.push(
        "    | '.' :: rest => (rest.takeWhile Char.isDigit, rest.dropWhile Char.isDigit)"
            .to_string(),
    );
    lines.push("    | _ => ([], rest)".to_string());
    lines.push("  let (expNeg, expDigits) := match rest with".to_string());
    lines.push("    | 'e' :: rest => parseExpPart rest".to_string());
    lines.push("    | 'E' :: rest => parseExpPart rest".to_string());
    lines.push("    | _ => (false, [])".to_string());
    lines.push(
        "  if intPart.isEmpty && fracPart.isEmpty then .error (\"Invalid float: \" ++ s)"
            .to_string(),
    );
    lines.push("  else".to_string());
    lines.push("    let mantissa := charDigitsToNat (intPart ++ fracPart)".to_string());
    lines.push("    let fracLen : Int := fracPart.length".to_string());
    lines.push("    let expVal : Int := charDigitsToNat expDigits".to_string());
    lines.push("    let shift : Int := (if expNeg then -expVal else expVal) - fracLen".to_string());
    lines.push(
        "    let f := if shift >= 0 then Float.ofScientific mantissa false shift.toNat".to_string(),
    );
    lines.push("             else Float.ofScientific mantissa true ((-shift).toNat)".to_string());
    lines.push("    .ok (if neg then -f else f)".to_string());
    lines.push(String::new());

    // Char helpers (Aver Char namespace operates on strings)
    lines.push("def Char.toCode (s : String) : Int :=".to_string());
    lines.push("  match s.toList.head? with".to_string());
    lines.push("  | some c => (c.toNat : Int)".to_string());
    lines.push("  | none => panic! \"Char.toCode: string is empty\"".to_string());
    lines.push("def Char.fromCode (n : Int) : Option String :=".to_string());
    lines.push("  if n < 0 || n > 1114111 then none".to_string());
    lines.push("  else if n >= 55296 && n <= 57343 then none".to_string());
    lines.push("  else some (Char.toString (Char.ofNat n.toNat))".to_string());
    lines.push(String::new());

    // #20: Hex conversion helpers (for Byte.toHex / Byte.fromHex)
    lines.push("def hexDigit (n : Int) : String :=".to_string());
    lines.push("  match n with".to_string());
    lines.push("  | 0 => \"0\" | 1 => \"1\" | 2 => \"2\" | 3 => \"3\"".to_string());
    lines.push("  | 4 => \"4\" | 5 => \"5\" | 6 => \"6\" | 7 => \"7\"".to_string());
    lines.push("  | 8 => \"8\" | 9 => \"9\" | 10 => \"a\" | 11 => \"b\"".to_string());
    lines.push("  | 12 => \"c\" | 13 => \"d\" | 14 => \"e\" | 15 => \"f\"".to_string());
    lines.push("  | _ => \"?\"".to_string());
    lines.push(String::new());
    lines.push("def byteToHex (code : Int) : String :=".to_string());
    lines.push("  hexDigit (code / 16) ++ hexDigit (code % 16)".to_string());
    lines.push(String::new());
    lines.push("namespace AverByte".to_string());
    lines.push("private def hexValue (c : Char) : Option Int :=".to_string());
    lines.push("  match c with".to_string());
    lines.push("  | '0' => some 0  | '1' => some 1  | '2' => some 2  | '3' => some 3".to_string());
    lines.push("  | '4' => some 4  | '5' => some 5  | '6' => some 6  | '7' => some 7".to_string());
    lines.push("  | '8' => some 8  | '9' => some 9  | 'a' => some 10 | 'b' => some 11".to_string());
    lines.push("  | 'c' => some 12 | 'd' => some 13 | 'e' => some 14 | 'f' => some 15".to_string());
    lines.push("  | 'A' => some 10 | 'B' => some 11 | 'C' => some 12 | 'D' => some 13".to_string());
    lines.push("  | 'E' => some 14 | 'F' => some 15".to_string());
    lines.push("  | _ => none".to_string());
    lines.push("def toHex (n : Int) : Except String String :=".to_string());
    lines.push("  if n < 0 || n > 255 then".to_string());
    lines.push(
        "    .error (\"Byte.toHex: \" ++ toString n ++ \" is out of range 0-255\")".to_string(),
    );
    lines.push("  else".to_string());
    lines.push("    .ok (byteToHex n)".to_string());
    lines.push("def fromHex (s : String) : Except String Int :=".to_string());
    lines.push("  match s.toList with".to_string());
    lines.push("  | [hi, lo] =>".to_string());
    lines.push("    match hexValue hi, hexValue lo with".to_string());
    lines.push("    | some h, some l => .ok (h * 16 + l)".to_string());
    lines.push("    | _, _ => .error (\"Byte.fromHex: invalid hex '\" ++ s ++ \"'\")".to_string());
    lines.push(
        "  | _ => .error (\"Byte.fromHex: expected exactly 2 hex chars, got '\" ++ s ++ \"'\")"
            .to_string(),
    );
    lines.push("end AverByte".to_string());

    lines.join("\n")
}

fn generate_lakefile(project_name: &str) -> String {
    format!(
        r#"import Lake
open Lake DSL

package «{}» where
  version := v!"0.1.0"

@[default_target]
lean_lib «{}» where
  srcDir := "."
"#,
        project_name.to_lowercase(),
        project_name
    )
}

fn generate_toolchain() -> String {
    "leanprover/lean4:v4.15.0\n".to_string()
}

fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().to_string() + chars.as_str(),
    }
}

#[cfg(test)]
mod tests {
    use super::{
        VerifyEmitMode, generate_prelude, proof_mode_issues, transpile, transpile_for_proof_mode,
        transpile_with_verify_mode,
    };
    use crate::ast::{
        BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, TopLevel, TypeDef, TypeVariant,
        VerifyBlock, VerifyGiven, VerifyGivenDomain, VerifyKind, VerifyLaw,
    };
    use crate::codegen::CodegenContext;
    use std::collections::{HashMap, HashSet};
    use std::rc::Rc;

    fn empty_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            fn_sigs: HashMap::new(),
            memo_fns: HashSet::new(),
            memo_safe_types: HashSet::new(),
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "verify_mode".to_string(),
            modules: vec![],
            module_prefixes: HashSet::new(),
        }
    }

    fn empty_ctx_with_verify_case() -> CodegenContext {
        let mut ctx = empty_ctx();
        ctx.items.push(TopLevel::Verify(VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![(
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(1)),
            )],
            kind: VerifyKind::Cases,
        }));
        ctx
    }

    fn empty_ctx_with_two_verify_blocks_same_fn() -> CodegenContext {
        let mut ctx = empty_ctx();
        ctx.items.push(TopLevel::Verify(VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![(
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(1)),
            )],
            kind: VerifyKind::Cases,
        }));
        ctx.items.push(TopLevel::Verify(VerifyBlock {
            fn_name: "f".to_string(),
            line: 2,
            cases: vec![(
                Expr::Literal(Literal::Int(2)),
                Expr::Literal(Literal::Int(2)),
            )],
            kind: VerifyKind::Cases,
        }));
        ctx
    }

    fn empty_ctx_with_verify_law() -> CodegenContext {
        let mut ctx = empty_ctx();
        ctx.items.push(TopLevel::Verify(VerifyBlock {
            fn_name: "add".to_string(),
            line: 1,
            cases: vec![
                (
                    Expr::FnCall(
                        Box::new(Expr::Ident("add".to_string())),
                        vec![
                            Expr::Literal(Literal::Int(1)),
                            Expr::Literal(Literal::Int(2)),
                        ],
                    ),
                    Expr::FnCall(
                        Box::new(Expr::Ident("add".to_string())),
                        vec![
                            Expr::Literal(Literal::Int(2)),
                            Expr::Literal(Literal::Int(1)),
                        ],
                    ),
                ),
                (
                    Expr::FnCall(
                        Box::new(Expr::Ident("add".to_string())),
                        vec![
                            Expr::Literal(Literal::Int(2)),
                            Expr::Literal(Literal::Int(3)),
                        ],
                    ),
                    Expr::FnCall(
                        Box::new(Expr::Ident("add".to_string())),
                        vec![
                            Expr::Literal(Literal::Int(3)),
                            Expr::Literal(Literal::Int(2)),
                        ],
                    ),
                ),
            ],
            kind: VerifyKind::Law(VerifyLaw {
                name: "commutative".to_string(),
                givens: vec![
                    VerifyGiven {
                        name: "a".to_string(),
                        type_name: "Int".to_string(),
                        domain: VerifyGivenDomain::IntRange { start: 1, end: 2 },
                    },
                    VerifyGiven {
                        name: "b".to_string(),
                        type_name: "Int".to_string(),
                        domain: VerifyGivenDomain::Explicit(vec![
                            Expr::Literal(Literal::Int(2)),
                            Expr::Literal(Literal::Int(3)),
                        ]),
                    },
                ],
                lhs: Expr::FnCall(
                    Box::new(Expr::Ident("add".to_string())),
                    vec![Expr::Ident("a".to_string()), Expr::Ident("b".to_string())],
                ),
                rhs: Expr::FnCall(
                    Box::new(Expr::Ident("add".to_string())),
                    vec![Expr::Ident("b".to_string()), Expr::Ident("a".to_string())],
                ),
            }),
        }));
        ctx
    }

    #[test]
    fn prelude_normalizes_float_string_format() {
        let prelude = generate_prelude();
        assert!(
            prelude.contains("private def normalizeFloatString (s : String) : String :="),
            "missing normalizeFloatString helper in prelude"
        );
        assert!(
            prelude.contains(
                "def String.fromFloat (f : Float) : String := normalizeFloatString (toString f)"
            ),
            "String.fromFloat should normalize Lean float formatting"
        );
    }

    #[test]
    fn prelude_validates_char_from_code_unicode_bounds() {
        let prelude = generate_prelude();
        assert!(
            prelude.contains("if n < 0 || n > 1114111 then none"),
            "Char.fromCode should reject code points above Unicode max"
        );
        assert!(
            prelude.contains("else if n >= 55296 && n <= 57343 then none"),
            "Char.fromCode should reject surrogate code points"
        );
    }

    #[test]
    fn transpile_emits_native_decide_for_verify_by_default() {
        let out = transpile(&empty_ctx_with_verify_case());
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("example : 1 = 1 := by native_decide"));
    }

    #[test]
    fn transpile_can_emit_sorry_for_verify_when_requested() {
        let out = transpile_with_verify_mode(&empty_ctx_with_verify_case(), VerifyEmitMode::Sorry);
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("example : 1 = 1 := by sorry"));
    }

    #[test]
    fn transpile_can_emit_theorem_skeletons_for_verify() {
        let out = transpile_with_verify_mode(
            &empty_ctx_with_verify_case(),
            VerifyEmitMode::TheoremSkeleton,
        );
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("theorem f_verify_1 : 1 = 1 := by"));
        assert!(lean.contains("  sorry"));
    }

    #[test]
    fn theorem_skeleton_numbering_is_global_per_function_across_verify_blocks() {
        let out = transpile_with_verify_mode(
            &empty_ctx_with_two_verify_blocks_same_fn(),
            VerifyEmitMode::TheoremSkeleton,
        );
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("theorem f_verify_1 : 1 = 1 := by"));
        assert!(lean.contains("theorem f_verify_2 : 2 = 2 := by"));
    }

    #[test]
    fn transpile_emits_named_theorems_for_verify_law() {
        let out = transpile(&empty_ctx_with_verify_law());
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("-- verify law add.commutative (2 cases)"));
        assert!(lean.contains("-- given a: Int = 1..2"));
        assert!(lean.contains("-- given b: Int = [2, 3]"));
        assert!(lean.contains(
            "theorem add_law_commutative : ∀ (a : Int) (b : Int), add a b = add b a := by"
        ));
        assert!(lean.contains(
            "theorem add_law_commutative_sample_1 : add 1 2 = add 2 1 := by native_decide"
        ));
        assert!(lean.contains(
            "theorem add_law_commutative_sample_2 : add 2 3 = add 3 2 := by native_decide"
        ));
    }

    #[test]
    fn verify_law_numbering_is_scoped_per_law_name() {
        let mut ctx = empty_ctx();
        ctx.items.push(TopLevel::Verify(VerifyBlock {
            fn_name: "f".to_string(),
            line: 1,
            cases: vec![(
                Expr::Literal(Literal::Int(1)),
                Expr::Literal(Literal::Int(1)),
            )],
            kind: VerifyKind::Cases,
        }));
        ctx.items.push(TopLevel::Verify(VerifyBlock {
            fn_name: "f".to_string(),
            line: 2,
            cases: vec![(
                Expr::Literal(Literal::Int(2)),
                Expr::Literal(Literal::Int(2)),
            )],
            kind: VerifyKind::Law(VerifyLaw {
                name: "identity".to_string(),
                givens: vec![VerifyGiven {
                    name: "x".to_string(),
                    type_name: "Int".to_string(),
                    domain: VerifyGivenDomain::Explicit(vec![Expr::Literal(Literal::Int(2))]),
                }],
                lhs: Expr::Ident("x".to_string()),
                rhs: Expr::Ident("x".to_string()),
            }),
        }));
        let out = transpile_with_verify_mode(&ctx, VerifyEmitMode::TheoremSkeleton);
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("theorem f_verify_1 : 1 = 1 := by"));
        assert!(lean.contains("theorem f_law_identity : ∀ (x : Int), x = x := by"));
        assert!(lean.contains("theorem f_law_identity_sample_1 : 2 = 2 := by"));
        assert!(!lean.contains("theorem f_law_identity_sample_2 : 2 = 2 := by"));
    }

    #[test]
    fn proof_mode_accepts_single_int_countdown_recursion() {
        let mut ctx = empty_ctx();
        let down = FnDef {
            name: "down".to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::Ident("n".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Expr::Literal(Literal::Int(0))),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::TailCall(Box::new((
                            "down".to_string(),
                            vec![Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Expr::Ident("n".to_string())),
                                Box::new(Expr::Literal(Literal::Int(1))),
                            )],
                        )))),
                    },
                ],
                line: 1,
            })),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(down.clone()));
        ctx.fn_defs.push(down);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.is_empty(),
            "expected Int countdown recursion to be accepted, got: {:?}",
            issues
        );

        let out = transpile_for_proof_mode(&ctx, VerifyEmitMode::NativeDecide);
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("def down (n : Int) : Int :="));
        assert!(lean.contains("termination_by Int.natAbs n"));
        assert!(lean.contains("decreasing_by"));
    }

    #[test]
    fn proof_mode_accepts_single_list_structural_recursion() {
        let mut ctx = empty_ctx();
        let len = FnDef {
            name: "len".to_string(),
            line: 1,
            params: vec![("xs".to_string(), "List<Int>".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::Ident("xs".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::EmptyList,
                        body: Box::new(Expr::Literal(Literal::Int(0))),
                    },
                    MatchArm {
                        pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                        body: Box::new(Expr::TailCall(Box::new((
                            "len".to_string(),
                            vec![Expr::Ident("t".to_string())],
                        )))),
                    },
                ],
                line: 1,
            })),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(len.clone()));
        ctx.fn_defs.push(len);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.is_empty(),
            "expected List structural recursion to be accepted, got: {:?}",
            issues
        );
    }

    #[test]
    fn proof_mode_accepts_single_string_pos_advance_recursion() {
        let mut ctx = empty_ctx();
        let skip_ws = FnDef {
            name: "skipWs".to_string(),
            line: 1,
            params: vec![
                ("s".to_string(), "String".to_string()),
                ("pos".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::FnCall(
                    Box::new(Expr::Attr(
                        Box::new(Expr::Ident("String".to_string())),
                        "charAt".to_string(),
                    )),
                    vec![Expr::Ident("s".to_string()), Expr::Ident("pos".to_string())],
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                        body: Box::new(Expr::Ident("pos".to_string())),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::TailCall(Box::new((
                            "skipWs".to_string(),
                            vec![
                                Expr::Ident("s".to_string()),
                                Expr::BinOp(
                                    BinOp::Add,
                                    Box::new(Expr::Ident("pos".to_string())),
                                    Box::new(Expr::Literal(Literal::Int(1))),
                                ),
                            ],
                        )))),
                    },
                ],
                line: 1,
            })),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(skip_ws.clone()));
        ctx.fn_defs.push(skip_ws);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.is_empty(),
            "expected String+pos recursion to be accepted, got: {:?}",
            issues
        );

        let out = transpile_for_proof_mode(&ctx, VerifyEmitMode::NativeDecide);
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("def skipWs (s : String) (pos : Int) : Int :="));
        assert!(lean.contains("termination_by ((s.data.length) - (pos.toNat))"));
        assert!(lean.contains("decreasing_by"));
    }

    #[test]
    fn proof_mode_accepts_mutual_int_countdown_recursion() {
        let mut ctx = empty_ctx();
        let even = FnDef {
            name: "even".to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Bool".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::Ident("n".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Expr::Literal(Literal::Bool(true))),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::TailCall(Box::new((
                            "odd".to_string(),
                            vec![Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Expr::Ident("n".to_string())),
                                Box::new(Expr::Literal(Literal::Int(1))),
                            )],
                        )))),
                    },
                ],
                line: 1,
            })),
            resolution: None,
        };
        let odd = FnDef {
            name: "odd".to_string(),
            line: 2,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Bool".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::Ident("n".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Int(0)),
                        body: Box::new(Expr::Literal(Literal::Bool(false))),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::TailCall(Box::new((
                            "even".to_string(),
                            vec![Expr::BinOp(
                                BinOp::Sub,
                                Box::new(Expr::Ident("n".to_string())),
                                Box::new(Expr::Literal(Literal::Int(1))),
                            )],
                        )))),
                    },
                ],
                line: 2,
            })),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(even.clone()));
        ctx.items.push(TopLevel::FnDef(odd.clone()));
        ctx.fn_defs.push(even);
        ctx.fn_defs.push(odd);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.is_empty(),
            "expected mutual Int countdown recursion to be accepted, got: {:?}",
            issues
        );
    }

    #[test]
    fn proof_mode_accepts_mutual_string_pos_recursion_with_ranked_same_edges() {
        let mut ctx = empty_ctx();
        let f = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![
                ("s".to_string(), "String".to_string()),
                ("pos".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::BinOp(
                    BinOp::Gte,
                    Box::new(Expr::Ident("pos".to_string())),
                    Box::new(Expr::Literal(Literal::Int(3))),
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(Expr::Ident("pos".to_string())),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::TailCall(Box::new((
                            "g".to_string(),
                            vec![Expr::Ident("s".to_string()), Expr::Ident("pos".to_string())],
                        )))),
                    },
                ],
                line: 1,
            })),
            resolution: None,
        };
        let g = FnDef {
            name: "g".to_string(),
            line: 2,
            params: vec![
                ("s".to_string(), "String".to_string()),
                ("pos".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::BinOp(
                    BinOp::Gte,
                    Box::new(Expr::Ident("pos".to_string())),
                    Box::new(Expr::Literal(Literal::Int(3))),
                )),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::Literal(Literal::Bool(true)),
                        body: Box::new(Expr::Ident("pos".to_string())),
                    },
                    MatchArm {
                        pattern: Pattern::Wildcard,
                        body: Box::new(Expr::TailCall(Box::new((
                            "f".to_string(),
                            vec![
                                Expr::Ident("s".to_string()),
                                Expr::BinOp(
                                    BinOp::Add,
                                    Box::new(Expr::Ident("pos".to_string())),
                                    Box::new(Expr::Literal(Literal::Int(1))),
                                ),
                            ],
                        )))),
                    },
                ],
                line: 2,
            })),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(f.clone()));
        ctx.items.push(TopLevel::FnDef(g.clone()));
        ctx.fn_defs.push(f);
        ctx.fn_defs.push(g);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.is_empty(),
            "expected mutual String+pos recursion to be accepted, got: {:?}",
            issues
        );

        let out = transpile_for_proof_mode(&ctx, VerifyEmitMode::NativeDecide);
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("termination_by"));
        assert!(lean.contains("termination_by ((s.data.length) - (pos.toNat),"));
        assert!(lean.contains("decreasing_by"));
    }

    #[test]
    fn proof_mode_accepts_mutual_ranked_sizeof_recursion() {
        let mut ctx = empty_ctx();
        let f = FnDef {
            name: "f".to_string(),
            line: 1,
            params: vec![("xs".to_string(), "List<Int>".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::TailCall(Box::new((
                "g".to_string(),
                vec![
                    Expr::Literal(Literal::Str("acc".to_string())),
                    Expr::Ident("xs".to_string()),
                ],
            ))))),
            resolution: None,
        };
        let g = FnDef {
            name: "g".to_string(),
            line: 2,
            params: vec![
                ("acc".to_string(), "String".to_string()),
                ("xs".to_string(), "List<Int>".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::Match {
                subject: Box::new(Expr::Ident("xs".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: Pattern::EmptyList,
                        body: Box::new(Expr::Literal(Literal::Int(0))),
                    },
                    MatchArm {
                        pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                        body: Box::new(Expr::TailCall(Box::new((
                            "f".to_string(),
                            vec![Expr::Ident("t".to_string())],
                        )))),
                    },
                ],
                line: 2,
            })),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(f.clone()));
        ctx.items.push(TopLevel::FnDef(g.clone()));
        ctx.fn_defs.push(f);
        ctx.fn_defs.push(g);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.is_empty(),
            "expected mutual ranked-sizeOf recursion to be accepted, got: {:?}",
            issues
        );

        let out = transpile_for_proof_mode(&ctx, VerifyEmitMode::NativeDecide);
        let lean = out
            .files
            .iter()
            .find_map(|(name, content)| (name == "Verify_mode.lean").then_some(content))
            .expect("expected generated Lean file");
        assert!(lean.contains("termination_by"));
        assert!(lean.contains("(sizeOf"));
        assert!(lean.contains("decreasing_by"));
    }

    #[test]
    fn proof_mode_rejects_recursive_pure_functions() {
        let mut ctx = empty_ctx();
        let recursive_fn = FnDef {
            name: "loop".to_string(),
            line: 1,
            params: vec![("n".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::Expr(Expr::FnCall(
                Box::new(Expr::Ident("loop".to_string())),
                vec![Expr::Ident("n".to_string())],
            ))),
            resolution: None,
        };
        ctx.items.push(TopLevel::FnDef(recursive_fn.clone()));
        ctx.fn_defs.push(recursive_fn);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues.iter().any(|i| i.contains("outside proof subset")),
            "expected recursive function blocker, got: {:?}",
            issues
        );
    }

    #[test]
    fn proof_mode_allows_recursive_types() {
        let mut ctx = empty_ctx();
        let recursive_type = TypeDef::Sum {
            name: "Node".to_string(),
            variants: vec![TypeVariant {
                name: "Cons".to_string(),
                fields: vec!["Node".to_string()],
            }],
            line: 1,
        };
        ctx.items.push(TopLevel::TypeDef(recursive_type.clone()));
        ctx.type_defs.push(recursive_type);

        let issues = proof_mode_issues(&ctx);
        assert!(
            issues
                .iter()
                .all(|i| !i.contains("recursive types require unsafe DecidableEq shim")),
            "did not expect recursive type blocker, got: {:?}",
            issues
        );
    }
}
