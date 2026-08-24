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

use crate::ast::{
    Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart, TailCallData, TopLevel,
};

const WORKER_SUFFIX: &str = "__indexed";
/// Reserved scalar parameter on code variants synthesized by this pass.
/// The name is unspellable in source and carries the pass's exact
/// `-1 | Unicode scalar` construction invariant into MIR representation.
pub(super) const INDEX_CODE_PARAM: &str = "__str_index_code";
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
    /// Indexed `charAt` matches whose `Option<String>` and one-character
    /// String were replaced by one direct codepoint read.
    pub codepoint_accesses: usize,
    /// Codepoint classifier variants reached by indexed codepoint reads.
    pub code_variants: Vec<String>,
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
    let fn_names: BTreeSet<String> = fn_defs(items).map(|fd| fd.name.clone()).collect();
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
            taken.contains(&worker)
        });
        if collision {
            report
                .declined
                .insert(label, "the __indexed worker name is already taken");
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

    let code_variant_plans = discover_index_code_variants(items, &taken, &fn_names);
    let code_classifiers: BTreeMap<String, usize> = code_variant_plans
        .iter()
        .map(|(name, plan)| (name.clone(), plan.param_index))
        .collect();

    let originals: Vec<FnDef> = fn_defs(items)
        .filter(|fd| accepted.contains_key(&fd.name))
        .cloned()
        .collect();
    let mut workers = Vec::with_capacity(originals.len());
    let mut used_code_classifiers = BTreeSet::new();
    for original in &originals {
        let string_param = accepted[&original.name];
        let worker_name = format!("{}{WORKER_SUFFIX}", original.name);
        let (worker, accesses, codepoint_accesses) = build_worker(
            original,
            string_param,
            &worker_name,
            &accepted,
            &code_classifiers,
            &mut used_code_classifiers,
        );
        report.indexed_accesses += accesses;
        report.codepoint_accesses += codepoint_accesses;
        report.indexed_fns.push(original.name.clone());
        report.synthesized.push(worker_name);
        workers.push(worker);
    }

    expand_code_variant_dependencies(&mut used_code_classifiers, &code_variant_plans);
    let mut code_variants = Vec::new();
    for name in used_code_classifiers {
        let variant_name = format!("{name}{}", crate::ir::chars_fusion::CODE_SUFFIX);
        report.code_variants.push(variant_name.clone());
        if fn_names.contains(&variant_name) {
            continue;
        }
        code_variants.push(
            code_variant_plans
                .get(&name)
                .expect("a reached classifier has a discovered codepoint variant")
                .function
                .clone(),
        );
    }

    for fd in fn_defs_mut(items) {
        let Some(&string_param) = accepted.get(&fd.name) else {
            continue;
        };
        fd.body = Arc::new(wrapper_body(fd, string_param));
        fd.resolution = None;
    }
    items.extend(workers.into_iter().map(TopLevel::FnDef));
    items.extend(code_variants.into_iter().map(TopLevel::FnDef));
    report.indexed_fns.sort();
    report.synthesized.sort();
    report.code_variants.sort();
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
    code_classifiers: &BTreeMap<String, usize>,
    used_code_classifiers: &mut BTreeSet<String>,
) -> (FnDef, usize, usize) {
    let source_name = fd.params[string_param].0.clone();
    let mut bound_names = HashSet::new();
    crate::ir::chars_fusion::collect_bound_names(fd, &mut bound_names);
    let mut transformer = WorkerTransformer {
        source_name: &source_name,
        worker_name,
        accepted,
        code_classifiers,
        bound_names,
        used_code_classifiers,
        accesses: 0,
        codepoint_accesses: 0,
        code_fresh: 0,
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
    let codepoint_accesses = transformer.codepoint_accesses;
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
        codepoint_accesses,
    )
}

struct WorkerTransformer<'a> {
    source_name: &'a str,
    worker_name: &'a str,
    accepted: &'a BTreeMap<String, usize>,
    code_classifiers: &'a BTreeMap<String, usize>,
    bound_names: HashSet<String>,
    used_code_classifiers: &'a mut BTreeSet<String>,
    accesses: usize,
    codepoint_accesses: usize,
    code_fresh: usize,
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
                if source_live
                    && let Some(lowered) = self.lower_codepoint_match(subject, arms, expr)
                {
                    return lowered;
                }
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

    fn lower_codepoint_match(
        &mut self,
        subject: &Spanned<Expr>,
        arms: &[MatchArm],
        source_match: &Spanned<Expr>,
    ) -> Option<Spanned<Expr>> {
        let Expr::FnCall(callee, args) = &subject.node else {
            return None;
        };
        if args.len() != 2
            || !is_dotted(&callee.node, "String", "charAt")
            || !is_ident(&args[0].node, self.source_name)
        {
            return None;
        }

        let mut none_body = None;
        let mut some = None;
        for arm in arms {
            match &arm.pattern {
                Pattern::Constructor(name, bindings)
                    if name == "Option.None" && bindings.is_empty() && none_body.is_none() =>
                {
                    none_body = Some((*arm.body).clone());
                }
                Pattern::Constructor(name, bindings)
                    if name == "Option.Some" && bindings.len() == 1 && some.is_none() =>
                {
                    some = Some((bindings[0].clone(), (*arm.body).clone()));
                }
                _ => return None,
            }
        }
        let (none_body, (char_name, mut some_body)) = (none_body?, some?);

        self.code_fresh += 1;
        let code_name = format!("__str_ix_c{}", self.code_fresh);
        let mut reached = BTreeSet::new();
        let mut code_reads = 0usize;
        if char_name != "_"
            && !rewrite_indexed_code_uses(
                &mut some_body,
                &char_name,
                &code_name,
                self.code_classifiers,
                &self.bound_names,
                &mut reached,
                &mut code_reads,
            )
        {
            return None;
        }

        let line = subject.line;
        let indexed_subject = typed_call(
            "__str_index_code_at",
            vec![
                args[0].clone(),
                typed_ident(INDEX_PARAM, line, crate::types::Type::named(INDEX_TYPE)),
                self.transform(&args[1], true),
            ],
            line,
            crate::types::Type::Int,
        );
        let none_body = self.transform(&none_body, true);
        let some_body = self.transform(&some_body, char_name != self.source_name);
        let some_pattern = if code_reads == 0 {
            Pattern::Wildcard
        } else {
            Pattern::Ident(code_name)
        };
        let out = sp(
            Expr::Match {
                subject: Box::new(indexed_subject),
                arms: vec![
                    MatchArm::new(Pattern::Literal(Literal::Int(-1)), none_body),
                    MatchArm::new(some_pattern, some_body),
                ],
            },
            line,
        );
        inherit_type(&out, source_match);
        self.used_code_classifiers.extend(reached);
        self.accesses += 1;
        self.codepoint_accesses += 1;
        Some(out)
    }
}

#[derive(Clone)]
struct IndexCodeVariant {
    param_index: usize,
    function: FnDef,
    dependencies: BTreeSet<String>,
}

/// Find pure functions that can receive one character as an `Int` instead of
/// a one-character `String`. Discovery is a fixed point because a dispatcher
/// can become eligible only after the classifiers it delegates to have their
/// own codepoint variants.
fn discover_index_code_variants(
    items: &[TopLevel],
    taken: &HashSet<String>,
    fn_names: &BTreeSet<String>,
) -> BTreeMap<String, IndexCodeVariant> {
    let candidates: Vec<FnDef> = fn_defs(items).cloned().collect();
    let mut variants = BTreeMap::new();
    loop {
        let available: BTreeMap<String, usize> = variants
            .iter()
            .map(|(name, plan): (&String, &IndexCodeVariant)| (name.clone(), plan.param_index))
            .collect();
        let mut discovered = Vec::new();
        for fd in &candidates {
            if variants.contains_key(&fd.name) || !fd.effects.is_empty() {
                continue;
            }
            let variant_name = format!("{}{}", fd.name, crate::ir::chars_fusion::CODE_SUFFIX);
            if taken.contains(&variant_name) && !fn_names.contains(&variant_name) {
                continue;
            }
            let eligible: Vec<_> = fd
                .params
                .iter()
                .enumerate()
                .filter(|(_, (name, ty))| name != "_" && canonical(ty) == "String")
                .filter_map(|(param_index, _)| {
                    build_index_code_variant(fd, param_index, &available)
                        .map(|plan| (param_index, plan))
                })
                .collect();
            if let [(param_index, (function, dependencies))] = eligible.as_slice() {
                discovered.push((
                    fd.name.clone(),
                    IndexCodeVariant {
                        param_index: *param_index,
                        function: function.clone(),
                        dependencies: dependencies.clone(),
                    },
                ));
            }
        }
        if discovered.is_empty() {
            break;
        }
        variants.extend(discovered);
    }
    variants
}

fn expand_code_variant_dependencies(
    used: &mut BTreeSet<String>,
    variants: &BTreeMap<String, IndexCodeVariant>,
) {
    let mut queue: VecDeque<String> = used.iter().cloned().collect();
    while let Some(name) = queue.pop_front() {
        let Some(plan) = variants.get(&name) else {
            continue;
        };
        for dependency in &plan.dependencies {
            if used.insert(dependency.clone()) {
                queue.push_back(dependency.clone());
            }
        }
    }
}

fn build_index_code_variant(
    fd: &FnDef,
    param_index: usize,
    available: &BTreeMap<String, usize>,
) -> Option<(FnDef, BTreeSet<String>)> {
    let param = &fd.params.get(param_index)?.0;
    let code_param = INDEX_CODE_PARAM;
    let mut bound_names = HashSet::new();
    crate::ir::chars_fusion::collect_bound_names(fd, &mut bound_names);
    let mut dependencies = BTreeSet::new();
    let mut code_reads = 0usize;
    let mut live = true;
    let mut stmts = fd.body.stmts().to_vec();
    for stmt in &mut stmts {
        let expr = match stmt {
            Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => expr,
        };
        if live
            && !rewrite_variant_code_uses(
                expr,
                param,
                code_param,
                available,
                &bound_names,
                &mut dependencies,
                &mut code_reads,
            )
        {
            return None;
        }
        if matches!(stmt, Stmt::Binding(name, _, _) if name == param) {
            live = false;
        }
    }
    if code_reads == 0 {
        return None;
    }

    let mut params = fd.params.clone();
    params[param_index].0 = code_param.to_string();
    params[param_index].1 = "Int".to_string();
    Some((
        FnDef {
            name: format!("{}{}", fd.name, crate::ir::chars_fusion::CODE_SUFFIX),
            line: fd.line,
            params,
            return_type: fd.return_type.clone(),
            effects: fd.effects.clone(),
            desc: Some(format!(
                "Synthesized codepoint variant of `{}` for indexed character dispatch. Parameter `{param}` is the Unicode scalar of the one-character String accepted by the source function.",
                fd.name
            )),
            body: Arc::new(FnBody::Block(stmts)),
            resolution: None,
        },
        dependencies,
    ))
}

/// Rewrite the tracked String parameter inside a candidate codepoint variant.
/// The `Option.withDefault(String.firstCodePoint(c), <literal>)` case is exact
/// because this variant is reachable only from `Option.Some(c)` of `charAt`;
/// the default branch cannot occur and the literal has no evaluation effect.
fn rewrite_variant_code_uses(
    expr: &mut Spanned<Expr>,
    param: &str,
    code_param: &str,
    available: &BTreeMap<String, usize>,
    bound_names: &HashSet<String>,
    dependencies: &mut BTreeSet<String>,
    code_reads: &mut usize,
) -> bool {
    let line = expr.line;
    if is_code1_call(expr, param) || is_first_codepoint_with_literal_default(expr, param) {
        *expr = typed_ident(code_param, line, crate::types::Type::Int);
        *code_reads += 1;
        return true;
    }

    let delegated = match &expr.node {
        Expr::FnCall(callee, args) => bare_ident(&callee.node).and_then(|name| {
            let &target_param = available.get(name)?;
            (!bound_names.contains(name)
                && args
                    .get(target_param)
                    .is_some_and(|arg| is_ident(&arg.node, param)))
            .then(|| (name.to_string(), target_param))
        }),
        _ => None,
    };
    if let Some((name, target_param)) = delegated {
        let Expr::FnCall(callee, args) = &mut expr.node else {
            unreachable!("delegated codepoint use was matched as a call")
        };
        callee.node = Expr::Ident(format!("{name}{}", crate::ir::chars_fusion::CODE_SUFFIX));
        args[target_param] =
            typed_ident(code_param, args[target_param].line, crate::types::Type::Int);
        for (index, arg) in args.iter_mut().enumerate() {
            if index != target_param
                && !rewrite_variant_code_uses(
                    arg,
                    param,
                    code_param,
                    available,
                    bound_names,
                    dependencies,
                    code_reads,
                )
            {
                return false;
            }
        }
        dependencies.insert(name);
        *code_reads += 1;
        return true;
    }

    if is_ident(&expr.node, param) {
        return false;
    }
    if let Expr::Match { subject, arms } = &mut expr.node {
        if !rewrite_variant_code_uses(
            subject,
            param,
            code_param,
            available,
            bound_names,
            dependencies,
            code_reads,
        ) {
            return false;
        }
        for arm in arms {
            if pattern_bindings(&arm.pattern)
                .iter()
                .any(|name| name == param)
            {
                continue;
            }
            if !rewrite_variant_code_uses(
                &mut arm.body,
                param,
                code_param,
                available,
                bound_names,
                dependencies,
                code_reads,
            ) {
                return false;
            }
        }
        return true;
    }

    let mut valid = true;
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        if valid {
            valid = rewrite_variant_code_uses(
                child,
                param,
                code_param,
                available,
                bound_names,
                dependencies,
                code_reads,
            );
        }
    });
    valid
}

fn is_code1_call(expr: &Spanned<Expr>, param: &str) -> bool {
    matches!(
        &expr.node,
        Expr::FnCall(callee, args)
            if bare_ident(&callee.node) == Some("__str_code1")
                && args.len() == 1
                && is_ident(&args[0].node, param)
    )
}

fn is_first_codepoint_with_literal_default(expr: &Spanned<Expr>, param: &str) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    if !is_dotted(&callee.node, "Option", "withDefault")
        || args.len() != 2
        || !matches!(args[1].node, Expr::Literal(_))
    {
        return false;
    }
    let Expr::FnCall(first, first_args) = &args[0].node else {
        return false;
    };
    is_dotted(&first.node, "String", "firstCodePoint")
        && first_args.len() == 1
        && is_ident(&first_args[0].node, param)
}

/// Replace every use of one `Option.Some` character that can consume its
/// codepoint directly. Any other read declines the complete match, so the
/// generic indexed `Option<String>` path remains the semantic fallback.
fn rewrite_indexed_code_uses(
    expr: &mut Spanned<Expr>,
    char_name: &str,
    code_name: &str,
    classifiers: &BTreeMap<String, usize>,
    bound_names: &HashSet<String>,
    reached: &mut BTreeSet<String>,
    code_reads: &mut usize,
) -> bool {
    enum DirectUse {
        Code,
        Classifier(String, usize),
    }

    let direct = match &expr.node {
        Expr::FnCall(callee, args)
            if args.len() == 1
                && is_ident(&args[0].node, char_name)
                && bare_ident(&callee.node) == Some("__str_code1") =>
        {
            Some(DirectUse::Code)
        }
        Expr::FnCall(callee, args) => bare_ident(&callee.node).and_then(|name| {
            let &param_index = classifiers.get(name)?;
            (!bound_names.contains(name)
                && args
                    .get(param_index)
                    .is_some_and(|arg| is_ident(&arg.node, char_name)))
            .then(|| DirectUse::Classifier(name.to_string(), param_index))
        }),
        _ => None,
    };
    match direct {
        Some(DirectUse::Code) => {
            *expr = typed_ident(code_name, expr.line, crate::types::Type::Int);
            *code_reads += 1;
            return true;
        }
        Some(DirectUse::Classifier(name, param_index)) => {
            let Expr::FnCall(callee, args) = &mut expr.node else {
                unreachable!("classifier use was matched as a call")
            };
            callee.node = Expr::Ident(format!("{name}{}", crate::ir::chars_fusion::CODE_SUFFIX));
            args[param_index] =
                typed_ident(code_name, args[param_index].line, crate::types::Type::Int);
            for (index, arg) in args.iter_mut().enumerate() {
                if index != param_index
                    && !rewrite_indexed_code_uses(
                        arg,
                        char_name,
                        code_name,
                        classifiers,
                        bound_names,
                        reached,
                        code_reads,
                    )
                {
                    return false;
                }
            }
            reached.insert(name);
            *code_reads += 1;
            return true;
        }
        None => {}
    }

    if is_ident(&expr.node, char_name) {
        return false;
    }
    if let Expr::Match { subject, arms } = &mut expr.node {
        if !rewrite_indexed_code_uses(
            subject,
            char_name,
            code_name,
            classifiers,
            bound_names,
            reached,
            code_reads,
        ) {
            return false;
        }
        for arm in arms {
            if pattern_bindings(&arm.pattern)
                .iter()
                .any(|name| name == char_name)
            {
                continue;
            }
            if !rewrite_indexed_code_uses(
                &mut arm.body,
                char_name,
                code_name,
                classifiers,
                bound_names,
                reached,
                code_reads,
            ) {
                return false;
            }
        }
        return true;
    }

    let mut valid = true;
    crate::ir::chars_fusion::walk_children_mut(expr, &mut |child| {
        if valid {
            valid = rewrite_indexed_code_uses(
                child,
                char_name,
                code_name,
                classifiers,
                bound_names,
                reached,
                code_reads,
            );
        }
    });
    valid
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
    fn indexed_character_dispatch_uses_codepoint_without_option_or_string() {
        let mut items = prepared(
            r#"module IndexedCode
    intent = "Classify every indexed character without materialising it."
    effects []

fn digit(c: String) -> Int
    match c
        "0" -> 1
        _ -> 0

fn walk(text: String, pos: Int, acc: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> acc
        Option.Some(c) -> walk(text, pos + 1, acc + digit(c))

fn count(text: String) -> Int
    walk(text, 0, 0)
"#,
        );
        crate::ir::chars_fusion::run_chars_fusion_pass(&mut items);
        let report = run_string_index_pass(&mut items);

        assert_eq!(report.codepoint_accesses, 1);
        assert_eq!(report.code_variants, vec!["digit__code".to_string()]);
        let worker = rendered_fn(&items, "walk__indexed");
        assert!(
            worker.contains("__str_index_code_at")
                && worker.contains("digit__code")
                && !worker.contains("__str_index_char_at"),
            "hot dispatch must carry only the codepoint: {worker}"
        );
        let classifier = rendered_fn(&items, "digit__code");
        assert!(
            classifier.contains("Int(48)") && !classifier.contains("__str_code1"),
            "classifier variant must consume the code directly: {classifier}"
        );
    }

    #[test]
    fn indexed_codepoint_crosses_multi_argument_dispatch_and_first_codepoint_helpers() {
        let mut items = prepared(
            r#"module IndexedHelpers
    intent = "Carry one indexed character through ordinary pure helpers."
    effects []

fn isDigit(c: String) -> Bool
    code = Option.withDefault(String.firstCodePoint(c), 0)
    match code >= 48
        true -> code <= 57
        false -> false

fn dispatch(text: String, pos: Int, c: String) -> Bool
    match isDigit(c)
        true -> String.len(text) > pos
        false -> false

fn walk(text: String, pos: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> pos
        Option.Some(c) -> match dispatch(text, pos, c)
            true -> walk(text, pos + 1)
            false -> pos

fn count(text: String) -> Int
    walk(text, 0)
"#,
        );
        crate::ir::chars_fusion::run_chars_fusion_pass(&mut items);
        let report = run_string_index_pass(&mut items);

        assert_eq!(report.codepoint_accesses, 1);
        assert_eq!(
            report.code_variants,
            vec!["dispatch__code".to_string(), "isDigit__code".to_string()]
        );
        let worker = rendered_fn(&items, "walk__indexed");
        assert!(
            worker.contains("__str_index_code_at")
                && worker.contains("dispatch__code")
                && !worker.contains("__str_index_char_at"),
            "worker must hand the scalar to the dispatcher: {worker}"
        );
        let classifier_fn = fn_defs(&items)
            .find(|fd| fd.name == "isDigit__code")
            .expect("codepoint classifier exists");
        assert_eq!(
            classifier_fn.params,
            vec![("__str_index_code".to_string(), "Int".to_string())]
        );
        let classifier = format!("{classifier_fn:?}");
        assert!(
            !classifier.contains("firstCodePoint") && !classifier.contains("withDefault"),
            "firstCodePoint/default must collapse to the proven scalar: {classifier}"
        );
    }

    #[test]
    fn indexed_character_that_escapes_keeps_the_option_string_path() {
        let mut items = prepared(LOOP);
        crate::ir::chars_fusion::run_chars_fusion_pass(&mut items);
        let report = run_string_index_pass(&mut items);

        assert_eq!(report.codepoint_accesses, 0);
        let worker = rendered_fn(&items, "walk__indexed");
        assert!(
            worker.contains("__str_index_char_at") && !worker.contains("__str_index_code_at"),
            "String.len(c) still needs the one-character String: {worker}"
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
    fn declines_a_taken_infix_worker_name() {
        let mut items = prepared(
            r#"module Taken
    intent = "Do not replace a legal user function with a synthesized worker."
    effects []

fn walk(text: String, pos: Int) -> Int
    match String.charAt(text, pos)
        Option.None -> pos
        Option.Some(_) -> walk(text, pos + 1)

fn walk__indexed(text: String, pos: Int) -> Int
    pos
"#,
        );
        let report = run_string_index_pass(&mut items);
        assert!(!report.fired());
        assert_eq!(report.declined.len(), 1);
    }
}
