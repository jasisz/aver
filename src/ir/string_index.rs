//! Loop-scoped indexing for repeated `String.charAt` / `String.slice`.
//!
//! A source program keeps codepoint positions as ordinary `Int` values. When
//! those positions repeatedly index the same immutable String through a
//! recursive call component, this pass leaves every source function's ABI in
//! place as a wrapper and synthesizes a worker carrying one hidden
//! `String.Index` parameter. Calls inside the component forward that index;
//! indexed reads translate codepoint positions to UTF-8 byte boundaries in
//! constant time.
//!
//! The pass is deliberately below the proof line. `String.Index` and the
//! `__str_index_*` calls do not exist in Aver source and must never appear in
//! a theorem about that source.

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet, VecDeque};
use std::sync::Arc;

use crate::ast::{Expr, FnBody, FnDef, Pattern, Spanned, Stmt, StrPart, TailCallData, TopLevel};

const WORKER_SUFFIX: &str = "__indexed";
const INDEX_PARAM: &str = "__str_index";
const INDEX_TYPE: &str = "String.Index";

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct StringIndexPassReport {
    /// Source functions whose ABI-preserving wrapper now builds/forwards an
    /// index to a synthesized worker.
    pub indexed_fns: Vec<String>,
    /// Synthesized worker names.
    pub synthesized: Vec<String>,
    /// `String.charAt` and `String.slice` sites moved onto the index.
    pub indexed_accesses: usize,
    /// Connected recursive string-flow components accepted by the pass.
    pub components: usize,
    /// Components declined after opportunity discovery, keyed by a stable
    /// comma-separated function-name label.
    pub declined: BTreeMap<String, &'static str>,
}

impl StringIndexPassReport {
    pub fn fired(&self) -> bool {
        !self.synthesized.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct ParamKey {
    function: String,
    index: usize,
}

#[derive(Debug, Clone)]
struct FlowEdge {
    from: ParamKey,
    to: ParamKey,
}

#[derive(Debug, Default)]
struct FlowFacts {
    seeds: BTreeSet<ParamKey>,
    edges: Vec<FlowEdge>,
}

/// Rewrite every recursive component that repeatedly indexes one immutable
/// String parameter. A component is all-or-nothing: an ambiguous function
/// with two tracked String parameters, or any generated-name collision,
/// leaves the complete source component untouched.
pub fn run_string_index_pass(items: &mut Vec<TopLevel>) -> StringIndexPassReport {
    let signatures: HashMap<String, Vec<String>> = fn_defs(items)
        .map(|fd| {
            (
                fd.name.clone(),
                fd.params.iter().map(|(_, ty)| canonical(ty)).collect(),
            )
        })
        .collect();
    let facts = collect_flow_facts(items, &signatures);
    let components = connected_components(&facts);
    let taken = crate::ir::chars_fusion::taken_names(items);
    let mut accepted: BTreeMap<String, usize> = BTreeMap::new();
    let mut report = StringIndexPassReport::default();

    for component in components {
        if !component.iter().any(|node| facts.seeds.contains(node))
            || !component_has_cycle(&component, &facts.edges)
        {
            continue;
        }
        let label = component_label(&component);
        let mut by_fn: BTreeMap<&str, usize> = BTreeMap::new();
        let ambiguous = component
            .iter()
            .any(|node| match by_fn.get(node.function.as_str()) {
                Some(index) => *index != node.index,
                None => {
                    by_fn.insert(&node.function, node.index);
                    false
                }
            });
        if ambiguous {
            report
                .declined
                .insert(label, "one function carries two indexed String parameters");
            continue;
        }
        let wildcard_param = component.iter().any(|node| {
            fn_defs(items)
                .find(|fd| fd.name == node.function)
                .is_some_and(|fd| fd.params.iter().any(|(name, _)| name == "_"))
        });
        if wildcard_param {
            report.declined.insert(
                label,
                "an ABI-preserving indexed wrapper cannot forward a wildcard parameter",
            );
            continue;
        }
        let collision = component.iter().any(|node| {
            let worker = format!("{}{WORKER_SUFFIX}", node.function);
            taken.contains(&worker) || function_mentions_prefix(items, &node.function, INDEX_PARAM)
        });
        if collision {
            report.declined.insert(
                label,
                "the __indexed worker or __str_index namespace is already taken",
            );
            continue;
        }
        report.components += 1;
        for node in component {
            accepted.insert(node.function, node.index);
        }
    }

    if accepted.is_empty() {
        return report;
    }

    let originals: Vec<FnDef> = fn_defs(items)
        .filter(|fd| accepted.contains_key(&fd.name))
        .cloned()
        .collect();
    let mut workers = Vec::with_capacity(originals.len());
    for original in &originals {
        let string_param = accepted[&original.name];
        let worker_name = format!("{}{WORKER_SUFFIX}", original.name);
        let (worker, accesses) = build_worker(original, string_param, &worker_name, &accepted);
        report.indexed_accesses += accesses;
        report.indexed_fns.push(original.name.clone());
        report.synthesized.push(worker_name);
        workers.push(worker);
    }

    for fd in fn_defs_mut(items) {
        let Some(&string_param) = accepted.get(&fd.name) else {
            continue;
        };
        fd.body = Arc::new(wrapper_body(fd, string_param));
        fd.resolution = None;
    }
    items.extend(workers.into_iter().map(TopLevel::FnDef));
    report.indexed_fns.sort();
    report.synthesized.sort();
    report
}

/// Cheap, read-only opportunity probe for callers that must decide whether
/// cloning a dependency module for lowering is worthwhile. It deliberately
/// stops before collision/ambiguity validation: a false positive only pays
/// for one clone, while a false negative would make separately loaded module
/// copies disagree about synthesized workers.
pub fn has_string_index_shape(items: &[TopLevel]) -> bool {
    let signatures: HashMap<String, Vec<String>> = fn_defs(items)
        .map(|fd| {
            (
                fd.name.clone(),
                fd.params.iter().map(|(_, ty)| canonical(ty)).collect(),
            )
        })
        .collect();
    let facts = collect_flow_facts(items, &signatures);
    connected_components(&facts).into_iter().any(|component| {
        component.iter().any(|node| facts.seeds.contains(node))
            && component_has_cycle(&component, &facts.edges)
    })
}

fn canonical(ty: &str) -> String {
    ty.chars().filter(|c| !c.is_whitespace()).collect()
}

fn collect_flow_facts(items: &[TopLevel], signatures: &HashMap<String, Vec<String>>) -> FlowFacts {
    let mut facts = FlowFacts::default();
    for fd in fn_defs(items) {
        let mut live: HashMap<String, usize> = fd
            .params
            .iter()
            .enumerate()
            .filter(|(_, (name, ty))| name != "_" && canonical(ty) == "String")
            .map(|(index, (name, _))| (name.clone(), index))
            .collect();
        for stmt in fd.body.stmts() {
            visit_expr(
                crate::ir::chars_fusion::stmt_expr(stmt),
                &live,
                &mut |expr, scope| collect_expr_fact(expr, scope, fd, signatures, &mut facts),
            );
            if let Stmt::Binding(name, _, _) = stmt {
                live.remove(name);
            }
        }
    }
    facts
}

fn collect_expr_fact(
    expr: &Expr,
    live: &HashMap<String, usize>,
    fd: &FnDef,
    signatures: &HashMap<String, Vec<String>>,
    facts: &mut FlowFacts,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            if (is_dotted(&callee.node, "String", "charAt")
                || is_dotted(&callee.node, "String", "slice"))
                && let Some(index) = args.first().and_then(|arg| live_param(arg, live))
            {
                facts.seeds.insert(ParamKey {
                    function: fd.name.clone(),
                    index,
                });
            }
            let Some(target) = bare_ident(&callee.node) else {
                return;
            };
            collect_call_edges(target, args, live, fd, signatures, facts);
        }
        Expr::TailCall(data) => {
            collect_call_edges(&data.target, &data.args, live, fd, signatures, facts);
        }
        _ => {}
    }
}

fn collect_call_edges(
    target: &str,
    args: &[Spanned<Expr>],
    live: &HashMap<String, usize>,
    fd: &FnDef,
    signatures: &HashMap<String, Vec<String>>,
    facts: &mut FlowFacts,
) {
    let Some(target_params) = signatures.get(target) else {
        return;
    };
    for (target_index, arg) in args.iter().enumerate() {
        if target_params
            .get(target_index)
            .is_none_or(|ty| ty != "String")
        {
            continue;
        }
        let Some(source_index) = live_param(arg, live) else {
            continue;
        };
        facts.edges.push(FlowEdge {
            from: ParamKey {
                function: fd.name.clone(),
                index: source_index,
            },
            to: ParamKey {
                function: target.to_string(),
                index: target_index,
            },
        });
    }
}

fn connected_components(facts: &FlowFacts) -> Vec<BTreeSet<ParamKey>> {
    let mut adjacency: BTreeMap<ParamKey, BTreeSet<ParamKey>> = BTreeMap::new();
    for seed in &facts.seeds {
        adjacency.entry(seed.clone()).or_default();
    }
    for edge in &facts.edges {
        adjacency
            .entry(edge.from.clone())
            .or_default()
            .insert(edge.to.clone());
        adjacency
            .entry(edge.to.clone())
            .or_default()
            .insert(edge.from.clone());
    }
    let mut unseen: BTreeSet<ParamKey> = adjacency.keys().cloned().collect();
    let mut out = Vec::new();
    while let Some(start) = unseen.pop_first() {
        let mut component = BTreeSet::new();
        let mut queue = VecDeque::from([start]);
        while let Some(node) = queue.pop_front() {
            if !component.insert(node.clone()) {
                continue;
            }
            unseen.remove(&node);
            if let Some(neighbours) = adjacency.get(&node) {
                queue.extend(neighbours.iter().cloned());
            }
        }
        out.push(component);
    }
    out
}

fn component_has_cycle(component: &BTreeSet<ParamKey>, edges: &[FlowEdge]) -> bool {
    fn visit(
        node: &ParamKey,
        component: &BTreeSet<ParamKey>,
        edges: &[FlowEdge],
        visiting: &mut HashSet<ParamKey>,
        done: &mut HashSet<ParamKey>,
    ) -> bool {
        if visiting.contains(node) {
            return true;
        }
        if done.contains(node) {
            return false;
        }
        visiting.insert(node.clone());
        let cyclic = edges.iter().filter(|edge| &edge.from == node).any(|edge| {
            component.contains(&edge.to) && visit(&edge.to, component, edges, visiting, done)
        });
        visiting.remove(node);
        done.insert(node.clone());
        cyclic
    }

    let mut visiting = HashSet::new();
    let mut done = HashSet::new();
    component
        .iter()
        .any(|node| visit(node, component, edges, &mut visiting, &mut done))
}

fn component_label(component: &BTreeSet<ParamKey>) -> String {
    component
        .iter()
        .map(|node| node.function.as_str())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .join(",")
}

fn function_mentions_prefix(items: &[TopLevel], function: &str, prefix: &str) -> bool {
    let Some(fd) = fn_defs(items).find(|fd| fd.name == function) else {
        return false;
    };
    if fd.params.iter().any(|(name, _)| name.starts_with(prefix)) {
        return true;
    }
    fd.body.stmts().iter().any(|stmt| {
        if let Stmt::Binding(name, _, _) = stmt
            && name.starts_with(prefix)
        {
            return true;
        }
        expr_mentions_prefix(crate::ir::chars_fusion::stmt_expr(stmt), prefix)
    })
}

fn expr_mentions_prefix(expr: &Spanned<Expr>, prefix: &str) -> bool {
    if matches!(&expr.node, Expr::Ident(name) | Expr::Resolved { name, .. } if name.starts_with(prefix))
    {
        return true;
    }
    if let Expr::Match { subject, arms } = &expr.node {
        return expr_mentions_prefix(subject, prefix)
            || arms.iter().any(|arm| {
                pattern_bindings(&arm.pattern)
                    .iter()
                    .any(|name| name.starts_with(prefix))
                    || expr_mentions_prefix(&arm.body, prefix)
            });
    }
    let mut found = false;
    crate::ir::chars_fusion::walk_children(expr, &mut |child| {
        found = found || expr_mentions_prefix(child, prefix);
    });
    found
}

fn wrapper_body(fd: &FnDef, string_param: usize) -> FnBody {
    let line = fd.line;
    let mut args: Vec<Spanned<Expr>> = fd
        .params
        .iter()
        .map(|(name, ty)| typed_ident(name, line, crate::types::parse_type_str(ty)))
        .collect();
    let source = fd.params[string_param].0.clone();
    args.push(typed_call(
        "__str_index_build",
        vec![typed_ident(&source, line, crate::types::Type::Str)],
        line,
        crate::types::Type::named(INDEX_TYPE),
    ));
    FnBody::Block(vec![Stmt::Expr(typed_call(
        &format!("{}{WORKER_SUFFIX}", fd.name),
        args,
        line,
        crate::types::parse_type_str(&fd.return_type),
    ))])
}

fn build_worker(
    fd: &FnDef,
    string_param: usize,
    worker_name: &str,
    accepted: &BTreeMap<String, usize>,
) -> (FnDef, usize) {
    let source_name = fd.params[string_param].0.clone();
    let mut transformer = WorkerTransformer {
        source_name: &source_name,
        worker_name,
        accepted,
        accesses: 0,
    };
    let mut live = true;
    let mut stmts = Vec::with_capacity(fd.body.stmts().len());
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Expr(expr) => stmts.push(Stmt::Expr(transformer.transform(expr, live))),
            Stmt::Binding(name, ty, expr) => {
                stmts.push(Stmt::Binding(
                    name.clone(),
                    ty.clone(),
                    transformer.transform(expr, live),
                ));
                if name == &source_name {
                    live = false;
                }
            }
        }
    }
    let mut params = fd.params.clone();
    params.push((INDEX_PARAM.to_string(), INDEX_TYPE.to_string()));
    let accesses = transformer.accesses;
    (
        FnDef {
            name: worker_name.to_string(),
            line: fd.line,
            params,
            return_type: fd.return_type.clone(),
            effects: fd.effects.clone(),
            desc: Some(format!(
                "Synthesized indexed worker of `{}`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.",
                fd.name
            )),
            body: Arc::new(FnBody::Block(stmts)),
            resolution: None,
        },
        accesses,
    )
}

struct WorkerTransformer<'a> {
    source_name: &'a str,
    worker_name: &'a str,
    accepted: &'a BTreeMap<String, usize>,
    accesses: usize,
}

impl WorkerTransformer<'_> {
    fn transform(&mut self, expr: &Spanned<Expr>, source_live: bool) -> Spanned<Expr> {
        let line = expr.line;
        let out = match &expr.node {
            Expr::FnCall(callee, args) => {
                if source_live
                    && (is_dotted(&callee.node, "String", "charAt")
                        || is_dotted(&callee.node, "String", "slice"))
                    && args
                        .first()
                        .is_some_and(|arg| is_ident(&arg.node, self.source_name))
                {
                    let intrinsic = if args.len() == 2 {
                        "__str_index_char_at"
                    } else {
                        "__str_index_slice"
                    };
                    let mut lowered = vec![
                        args[0].clone(),
                        typed_ident(INDEX_PARAM, line, crate::types::Type::named(INDEX_TYPE)),
                    ];
                    lowered.extend(args[1..].iter().cloned());
                    self.accesses += 1;
                    let out = call(intrinsic, lowered, line);
                    inherit_type(&out, expr);
                    return out;
                }
                let mut lowered_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.transform(arg, source_live))
                    .collect();
                let mut lowered_callee = self.transform(callee, source_live);
                if source_live
                    && let Some(target) = bare_ident(&callee.node)
                    && let Some(&target_param) = self.accepted.get(target)
                    && args
                        .get(target_param)
                        .is_some_and(|arg| is_ident(&arg.node, self.source_name))
                {
                    lowered_callee = ident(&format!("{target}{WORKER_SUFFIX}"), line);
                    lowered_args.push(typed_ident(
                        INDEX_PARAM,
                        line,
                        crate::types::Type::named(INDEX_TYPE),
                    ));
                }
                sp(Expr::FnCall(Box::new(lowered_callee), lowered_args), line)
            }
            Expr::TailCall(data) => {
                let mut args: Vec<_> = data
                    .args
                    .iter()
                    .map(|arg| self.transform(arg, source_live))
                    .collect();
                let mut target = data.target.clone();
                if source_live
                    && let Some(&target_param) = self.accepted.get(&data.target)
                    && data
                        .args
                        .get(target_param)
                        .is_some_and(|arg| is_ident(&arg.node, self.source_name))
                {
                    target = if data.target == self.worker_name.trim_end_matches(WORKER_SUFFIX) {
                        self.worker_name.to_string()
                    } else {
                        format!("{}{WORKER_SUFFIX}", data.target)
                    };
                    args.push(typed_ident(
                        INDEX_PARAM,
                        line,
                        crate::types::Type::named(INDEX_TYPE),
                    ));
                }
                sp(
                    Expr::TailCall(Box::new(TailCallData::new(target, args))),
                    line,
                )
            }
            Expr::Match { subject, arms } => {
                let subject = self.transform(subject, source_live);
                let arms = arms
                    .iter()
                    .map(|arm| {
                        let shadowed = source_live
                            && pattern_bindings(&arm.pattern)
                                .iter()
                                .any(|name| name == self.source_name);
                        crate::ast::MatchArm {
                            pattern: arm.pattern.clone(),
                            body: Box::new(self.transform(&arm.body, !shadowed && source_live)),
                            binding_slots: std::sync::OnceLock::new(),
                        }
                    })
                    .collect();
                sp(
                    Expr::Match {
                        subject: Box::new(subject),
                        arms,
                    },
                    line,
                )
            }
            Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => expr.clone(),
            Expr::Attr(base, field) => sp(
                Expr::Attr(Box::new(self.transform(base, source_live)), field.clone()),
                line,
            ),
            Expr::BinOp(op, lhs, rhs) => sp(
                Expr::BinOp(
                    *op,
                    Box::new(self.transform(lhs, source_live)),
                    Box::new(self.transform(rhs, source_live)),
                ),
                line,
            ),
            Expr::Neg(inner) => sp(
                Expr::Neg(Box::new(self.transform(inner, source_live))),
                line,
            ),
            Expr::ErrorProp(inner) => sp(
                Expr::ErrorProp(Box::new(self.transform(inner, source_live))),
                line,
            ),
            Expr::Constructor(name, payload) => sp(
                Expr::Constructor(
                    name.clone(),
                    payload
                        .as_ref()
                        .map(|value| Box::new(self.transform(value, source_live))),
                ),
                line,
            ),
            Expr::InterpolatedStr(parts) => sp(
                Expr::InterpolatedStr(
                    parts
                        .iter()
                        .map(|part| match part {
                            StrPart::Literal(value) => StrPart::Literal(value.clone()),
                            StrPart::Parsed(value) => {
                                StrPart::Parsed(Box::new(self.transform(value, source_live)))
                            }
                        })
                        .collect(),
                ),
                line,
            ),
            Expr::List(values) => sp(
                Expr::List(
                    values
                        .iter()
                        .map(|value| self.transform(value, source_live))
                        .collect(),
                ),
                line,
            ),
            Expr::Tuple(values) => sp(
                Expr::Tuple(
                    values
                        .iter()
                        .map(|value| self.transform(value, source_live))
                        .collect(),
                ),
                line,
            ),
            Expr::IndependentProduct(values, unwrap) => sp(
                Expr::IndependentProduct(
                    values
                        .iter()
                        .map(|value| self.transform(value, source_live))
                        .collect(),
                    *unwrap,
                ),
                line,
            ),
            Expr::MapLiteral(entries) => sp(
                Expr::MapLiteral(
                    entries
                        .iter()
                        .map(|(key, value)| {
                            (
                                self.transform(key, source_live),
                                self.transform(value, source_live),
                            )
                        })
                        .collect(),
                ),
                line,
            ),
            Expr::RecordCreate { type_name, fields } => sp(
                Expr::RecordCreate {
                    type_name: type_name.clone(),
                    fields: fields
                        .iter()
                        .map(|(name, value)| (name.clone(), self.transform(value, source_live)))
                        .collect(),
                },
                line,
            ),
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => sp(
                Expr::RecordUpdate {
                    type_name: type_name.clone(),
                    base: Box::new(self.transform(base, source_live)),
                    updates: updates
                        .iter()
                        .map(|(name, value)| (name.clone(), self.transform(value, source_live)))
                        .collect(),
                },
                line,
            ),
        };
        inherit_type(&out, expr);
        out
    }
}

fn visit_expr(
    expr: &Spanned<Expr>,
    live: &HashMap<String, usize>,
    f: &mut impl FnMut(&Expr, &HashMap<String, usize>),
) {
    f(&expr.node, live);
    if let Expr::Match { subject, arms } = &expr.node {
        visit_expr(subject, live, f);
        for arm in arms {
            let mut inner = live.clone();
            for name in pattern_bindings(&arm.pattern) {
                inner.remove(&name);
            }
            visit_expr(&arm.body, &inner, f);
        }
        return;
    }
    crate::ir::chars_fusion::walk_children(expr, &mut |child| visit_expr(child, live, f));
}

fn pattern_bindings(pattern: &Pattern) -> Vec<String> {
    match pattern {
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => Vec::new(),
        Pattern::Ident(name) => vec![name.clone()],
        Pattern::Cons(head, tail) => vec![head.clone(), tail.clone()],
        Pattern::Tuple(items) => items.iter().flat_map(pattern_bindings).collect(),
        Pattern::Constructor(_, bindings) => bindings.clone(),
    }
}

fn live_param(expr: &Spanned<Expr>, live: &HashMap<String, usize>) -> Option<usize> {
    bare_ident(&expr.node).and_then(|name| live.get(name).copied())
}

fn bare_ident(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

fn is_ident(expr: &Expr, expected: &str) -> bool {
    bare_ident(expr) == Some(expected)
}

fn is_dotted(expr: &Expr, namespace: &str, method: &str) -> bool {
    matches!(expr, Expr::Attr(base, name) if name == method && is_ident(&base.node, namespace))
}

fn ident(name: &str, line: crate::ast::SourceLine) -> Spanned<Expr> {
    sp(Expr::Ident(name.to_string()), line)
}

fn call(name: &str, args: Vec<Spanned<Expr>>, line: crate::ast::SourceLine) -> Spanned<Expr> {
    sp(Expr::FnCall(Box::new(ident(name, line)), args), line)
}

fn typed_ident(name: &str, line: crate::ast::SourceLine, ty: crate::types::Type) -> Spanned<Expr> {
    let out = ident(name, line);
    out.set_ty(ty);
    out
}

fn typed_call(
    name: &str,
    args: Vec<Spanned<Expr>>,
    line: crate::ast::SourceLine,
    ty: crate::types::Type,
) -> Spanned<Expr> {
    let out = call(name, args, line);
    out.set_ty(ty);
    out
}

fn inherit_type(out: &Spanned<Expr>, source: &Spanned<Expr>) {
    if let Some(ty) = source.ty() {
        out.set_ty(ty.clone());
    }
}

fn sp(node: Expr, line: crate::ast::SourceLine) -> Spanned<Expr> {
    Spanned::new(node, line)
}

fn fn_defs(items: &[TopLevel]) -> impl Iterator<Item = &FnDef> {
    crate::ir::chars_fusion::fn_defs(items)
}

fn fn_defs_mut(items: &mut [TopLevel]) -> impl Iterator<Item = &mut FnDef> {
    crate::ir::chars_fusion::fn_defs_mut(items)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn prepared(source: &str) -> Vec<TopLevel> {
        let mut items = crate::source::parse_source(source).expect("fixture parses");
        crate::ir::pipeline::tco(&mut items);
        items
    }

    fn rendered_fn(items: &[TopLevel], name: &str) -> String {
        format!(
            "{:?}",
            fn_defs(items)
                .find(|fd| fd.name == name)
                .unwrap_or_else(|| panic!("{name} is present"))
        )
    }

    const LOOP: &str = r#"module Indexed
    intent = "Exercise indexed string access in one recursive component."
    effects []

fn walk(text: String, pos: Int, acc: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(c) -> walk(text, pos + 1, acc + String.len(c))

fn count(text: String) -> Int
    walk(text, 0, 0)
"#;

    #[test]
    fn recursive_string_flow_builds_one_wrapper_index_and_forwards_it() {
        let mut items = prepared(LOOP);
        let report = run_string_index_pass(&mut items);

        assert_eq!(report.components, 1);
        assert_eq!(report.indexed_accesses, 1);
        assert_eq!(
            report.synthesized,
            vec!["count__indexed".to_string(), "walk__indexed".to_string()]
        );
        let count = rendered_fn(&items, "count");
        assert!(
            count.contains("__str_index_build"),
            "wrapper builds once: {count}"
        );
        let count_worker = rendered_fn(&items, "count__indexed");
        assert!(
            count_worker.contains("walk__indexed") && count_worker.contains("__str_index"),
            "upstream worker forwards the index: {count_worker}"
        );
        let walk_worker = rendered_fn(&items, "walk__indexed");
        assert!(
            walk_worker.contains("__str_index_char_at") && walk_worker.contains("walk__indexed"),
            "recursive worker uses and forwards the index: {walk_worker}"
        );
    }

    #[test]
    fn one_nonrecursive_char_at_does_not_pay_for_an_index() {
        let mut items = prepared(
            r#"module One
    intent = "One indexed read is cheaper as the builtin."
    effects []

fn first(text: String) -> Option<String>
    String.charAt(text, 0)
"#,
        );
        let report = run_string_index_pass(&mut items);
        assert!(!report.fired());
        assert_eq!(fn_defs(&items).count(), 1);
    }

    #[test]
    fn declines_components_whose_wrapper_cannot_forward_every_parameter() {
        let mut items = prepared(
            r#"module Wildcard
    intent = "Keep source ABI when a parameter is intentionally ignored."
    effects []

fn walk(text: String, pos: Int, _: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> pos
        Option.Some(_) -> walk(text, pos + 1, 0)
"#,
        );
        let report = run_string_index_pass(&mut items);
        assert!(!report.fired());
        assert_eq!(report.declined.len(), 1);
    }

    #[test]
    fn declines_generated_index_shadowed_by_a_pattern_binding() {
        let mut items = prepared(
            r#"module Shadow
    intent = "Do not let a synthesized index collide with pattern scope."
    effects []

fn walk(text: String, pos: Int, value: Option<Int>) -> Int
    match value
        Option.None -> pos
        Option.Some(__str_index) -> match String.charAt(text, pos)
            Option.None -> __str_index
            Option.Some(_) -> walk(text, pos + 1, value)
"#,
        );
        let report = run_string_index_pass(&mut items);
        assert!(!report.fired());
        assert_eq!(report.declined.len(), 1);
    }
}
