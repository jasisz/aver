//! A termination measure for a mutually recursive group, chosen by reading
//! every call between its members.
//!
//! Each member is measured by the sum of the sizes of some of its parameters.
//! Which parameters is decided by the call edges, argument position by
//! argument position: a callee parameter may only be measured when EVERY
//! call into the callee hands it a measured parameter of the caller, either
//! unchanged or as a strictly smaller part of one — a binder of a pattern
//! matched against it (the head or tail of a list, a field of a
//! constructor, nested as deep as the matches go), or a guarded `n - k`
//! countdown. Under that rule the measure never grows along a call, and a
//! call on which at least one measured argument shrinks strictly decreases
//! it. A call that hands every measured argument on unchanged keeps the
//! measure equal, so those calls are ordered by a per-member tie-break
//! index and the pair `(measure, index)` decreases lexicographically on
//! every call. When the unchanged calls form a cycle, no such measure
//! exists and the group is declined with the call that fails.
//!
//! The analysis is keyed on the shape of the calls, never on what the
//! functions are called or what they compute: the same rule covers a pair,
//! a three-member cycle where every member descends on a different list,
//! and a group that mixes an `Int` countdown with a list descent.

use std::collections::{HashMap, HashSet};

use super::detect::{
    canonical_callee_name, expr_to_dotted_name, guards_imply_param_ge_one, is_int_minus_positive,
    local_name_of, mutual_sizeof_measure_param_indices, pattern_bound_names, ranks_from_same_edges,
};
use super::flip_comparison_binop;
use crate::ast::{BinOp, Expr, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt};

/// How a measured parameter contributes to its member's measure.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MeasureKind {
    /// `sizeOf p` — a list, vector, map or recursive ADT, where every binder
    /// of a pattern matched against it is strictly smaller.
    Structural,
    /// `Int.toNat p` — an `Int` a guarded `p - k` counts down.
    Countdown,
}

/// A parameter a backend can measure, by position in the member's signature.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Candidate {
    pub index: usize,
    pub kind: MeasureKind,
}

/// The measure of one member: the parameters it counts, in signature order,
/// and the tie-break index for calls that leave the sum unchanged.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberMeasure {
    pub params: Vec<Candidate>,
    pub rank: usize,
}

/// Why no measure decreases on every call of the group. One sentence,
/// naming the call that fails, for the user-facing refusal.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NoMeasure {
    pub reason: String,
}

/// Where a name sits inside the parameter it is a part of: the binders
/// taken to reach it, outermost first, each as (pattern identity, binder
/// index). The parameter itself has the empty path; anything deeper is a
/// strict part. Two parts of one parameter are disjoint exactly when their
/// paths first differ at two binders of the SAME pattern — siblings of one
/// destructuring, or cousins below them — and the sizes of disjoint parts
/// add up to less than the size of the whole, so a call may count both.
/// A path that is a prefix of the other names a part the other was taken
/// from; paths that first differ at two DIFFERENT patterns come from two
/// destructurings of one term, whose binders may be the same part under
/// two names (`match xs … [a, ..r1] -> match xs … [b, ..r2]` binds the
/// tail twice), so neither is disjoint and only one may be counted.
type Path = Vec<(usize, usize)>;

/// How a call's argument relates to the caller's parameters.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Relation {
    /// The caller's parameter at `param` itself (empty path) or a strictly
    /// smaller part of it.
    Part { param: usize, path: Path },
    /// Anything else — computed, grown, or a binder the analysis does not
    /// track.
    Unknown,
}

impl Relation {
    fn strict(&self) -> bool {
        matches!(self, Relation::Part { path, .. } if !path.is_empty())
    }
}

fn overlap(a: &Path, b: &Path) -> bool {
    match a.iter().zip(b).find(|(x, y)| x != y) {
        // One path is a prefix of the other: a part and a part of it.
        None => true,
        // Two binders of one pattern are disjoint; two patterns on the same
        // subject bind parts in an unknown relation.
        Some((x, y)) => x.0 != y.0,
    }
}

/// One call between members, every argument classified against the
/// caller's parameters.
struct Edge<'a> {
    caller: usize,
    callee: usize,
    relations: Vec<Relation>,
    args: Vec<&'a Spanned<Expr>>,
}

/// Why a parameter was dropped from the measure. A parameter dropped because
/// the parameter it is derived from was dropped inherits that root cause.
#[derive(Clone, Copy)]
enum Cause {
    /// The edge hands this callee parameter something that is not a
    /// measured parameter of the caller or a smaller part of one — or a
    /// part measured by a different kind of size, which cannot be compared.
    NotAPart { edge: usize, slot: usize },
    /// The edge hands overlapping parts of one caller parameter to two
    /// callee parameters; only the first can be counted.
    Twice {
        edge: usize,
        kept: usize,
        dropped: usize,
    },
}

/// What a local name stands for at a call site: a parameter of the caller,
/// or a strictly smaller part of one.
#[derive(Clone)]
struct Origin {
    param: usize,
    path: Path,
}

/// The scope a call sits in: the tracked names and the guards in force.
#[derive(Clone)]
struct Scope {
    tracked: HashMap<String, Origin>,
    guards: Vec<Spanned<Expr>>,
}

struct CallSite<'a> {
    callee: String,
    args: Vec<&'a Spanned<Expr>>,
    scope: Scope,
}

/// Choose a measure for `fns` from `candidates` (one list per member, in the
/// same order). `heads_are_parts` says whether the head of a matched list is
/// a strictly smaller part of it: it is by size, which `sizeOf` measures,
/// and not by length, where only the tail is shorter.
pub fn measure_for_cycle(
    fns: &[&FnDef],
    candidates: &[Vec<Candidate>],
    heads_are_parts: bool,
) -> Result<Vec<MemberMeasure>, NoMeasure> {
    let names: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let edges = collect_edges(fns, &names, heads_are_parts);

    // Narrowest measure first. The structural parameters a member hands on
    // by name are the measure a pair already has, and a group they settle
    // keeps it; every structural parameter joins where a part (a tail, a
    // field) has to carry the decrease; the `Int` countdowns join last,
    // where the lists leave a cycle with no decrease.
    let structural: Vec<Vec<Candidate>> = candidates
        .iter()
        .map(|cs| {
            cs.iter()
                .copied()
                .filter(|c| c.kind == MeasureKind::Structural)
                .collect()
        })
        .collect();
    let forwarded: Vec<Vec<Candidate>> = fns
        .iter()
        .zip(&structural)
        .map(|(fd, cs)| {
            let by_name = mutual_sizeof_measure_param_indices(fd);
            cs.iter()
                .copied()
                .filter(|c| by_name.contains(&c.index))
                .collect()
        })
        .collect();
    let mut tiers: Vec<&[Vec<Candidate>]> = vec![&forwarded, &structural, candidates];
    tiers.dedup();
    let mut last = Err(NoMeasure {
        reason: String::new(),
    });
    for tier in tiers {
        last = select(fns, &edges, tier);
        if last.is_ok() {
            break;
        }
    }
    last
}

/// Walk every member and classify every call into the group.
fn collect_edges<'a>(
    fns: &[&'a FnDef],
    names: &HashSet<String>,
    heads_are_parts: bool,
) -> Vec<Edge<'a>> {
    let mut edges = Vec::new();
    for (caller, fd) in fns.iter().enumerate() {
        for site in call_sites(fd, names, heads_are_parts) {
            let Some(callee) = fns.iter().position(|peer| peer.name == site.callee) else {
                continue;
            };
            let relations = (0..fns[callee].params.len())
                .map(|slot| {
                    site.args
                        .get(slot)
                        .map_or(Relation::Unknown, |arg| relation(arg, fd, &site.scope))
                })
                .collect();
            edges.push(Edge {
                caller,
                callee,
                relations,
                args: site.args,
            });
        }
    }
    edges
}

/// Every call from `fd` into the group, each with the scope in force at the
/// call — the names that stand for a part of a parameter THERE, not
/// somewhere else in the body, and the guards the call sits under.
fn call_sites<'a>(
    fd: &'a FnDef,
    names: &HashSet<String>,
    heads_are_parts: bool,
) -> Vec<CallSite<'a>> {
    let mut scope = Scope {
        tracked: fd
            .params
            .iter()
            .enumerate()
            .map(|(param, (name, _))| {
                (
                    name.clone(),
                    Origin {
                        param,
                        path: Vec::new(),
                    },
                )
            })
            .collect(),
        guards: Vec::new(),
    };
    let mut out = Vec::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                walk(expr, &scope, names, heads_are_parts, &mut out);
                // A `let` of the same name shadows the part from here on.
                scope.tracked.remove(name);
            }
            Stmt::Expr(expr) => walk(expr, &scope, names, heads_are_parts, &mut out),
        }
    }
    out
}

fn walk<'a>(
    expr: &'a Spanned<Expr>,
    scope: &Scope,
    names: &HashSet<String>,
    heads_are_parts: bool,
    out: &mut Vec<CallSite<'a>>,
) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(callee)
                && let Some(callee) = canonical_callee_name(&name, names)
            {
                out.push(CallSite {
                    callee,
                    args: args.iter().collect(),
                    scope: scope.clone(),
                });
            }
        }
        Expr::TailCall(boxed) => {
            if let Some(callee) = canonical_callee_name(&boxed.target, names) {
                out.push(CallSite {
                    callee,
                    args: boxed.args.iter().collect(),
                    scope: scope.clone(),
                });
            }
        }
        Expr::Match { subject, arms } => {
            walk(subject, scope, names, heads_are_parts, out);
            let subject_origin =
                local_name_of(subject).and_then(|id| scope.tracked.get(id).cloned());
            for MatchArm { pattern, body, .. } in arms {
                let scoped = arm_scope(
                    scope,
                    pattern,
                    subject,
                    subject_origin.as_ref(),
                    heads_are_parts,
                );
                walk(body, &scoped, names, heads_are_parts, out);
            }
            return;
        }
        _ => {}
    }
    // A match is the only shape that changes the scope; everything else
    // descends generically, through `expr_walk` so a new `Expr` variant
    // cannot be silently skipped here.
    crate::codegen::expr_walk::for_each_child(expr, &mut |child| {
        walk(child, scope, names, heads_are_parts, out)
    });
}

/// The scope an arm's body sees: every name the pattern binds leaves (inside
/// the arm it is whatever the pattern matched); when the subject is tracked,
/// every binder of a destructuring pattern enters as a strict part of the
/// same parameter (a constructor's `sizeOf` is one more than the sum of its
/// fields', so each field — the head of a list as much as its tail — is
/// smaller than the whole; by length only the tail is, so unless
/// `heads_are_parts` a list's head stays out) and a plain binder enters as an
/// alias of the subject; a Bool-literal arm adds the subject (or its
/// negation) to the guards.
fn arm_scope(
    scope: &Scope,
    pattern: &Pattern,
    subject: &Spanned<Expr>,
    subject_origin: Option<&Origin>,
    heads_are_parts: bool,
) -> Scope {
    let mut scoped = scope.clone();
    for bound in pattern_bound_names(pattern) {
        scoped.tracked.remove(bound);
    }
    if let Some(origin) = subject_origin {
        match pattern {
            Pattern::Ident(alias) => {
                scoped.tracked.insert(alias.clone(), origin.clone());
            }
            Pattern::Cons(..) | Pattern::Constructor(..) | Pattern::Tuple(..) => {
                // The pattern's address identifies it: binders of one
                // pattern are siblings; a second pattern on the same
                // subject, at any depth, binds parts that may overlap
                // the first's (see `overlap`).
                let pattern_id = pattern as *const Pattern as usize;
                for (index, bound) in pattern_bound_names(pattern).into_iter().enumerate() {
                    if !heads_are_parts && index == 0 && matches!(pattern, Pattern::Cons(..)) {
                        continue;
                    }
                    let mut path = origin.path.clone();
                    path.push((pattern_id, index));
                    scoped.tracked.insert(
                        bound.to_string(),
                        Origin {
                            param: origin.param,
                            path,
                        },
                    );
                }
            }
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
        }
    }
    match pattern {
        Pattern::Literal(Literal::Bool(true)) => scoped.guards.push(subject.clone()),
        Pattern::Literal(Literal::Bool(false)) => {
            if let Some(flipped) = flip_comparison_binop(subject) {
                scoped.guards.push(flipped);
            }
        }
        _ => {}
    }
    scoped
}

/// Classify one argument against the caller's parameters in `scope`.
fn relation(arg: &Spanned<Expr>, caller: &FnDef, scope: &Scope) -> Relation {
    if let Some(name) = local_name_of(arg) {
        return match scope.tracked.get(name) {
            Some(origin) => Relation::Part {
                param: origin.param,
                path: origin.path.clone(),
            },
            None => Relation::Unknown,
        };
    }
    // `Map.entries(m)` lowers to `m` itself in the proof model (the map is
    // list-backed), so it carries exactly the size of its argument.
    if let Expr::FnCall(callee, args) = &arg.node
        && args.len() == 1
        && expr_to_dotted_name(callee).as_deref() == Some("Map.entries")
    {
        return relation(&args[0], caller, scope);
    }
    // `n - k` on an `Int` parameter the guards in force bound below by one.
    for (index, (name, type_name)) in caller.params.iter().enumerate() {
        if type_name == "Int"
            && is_int_minus_positive(arg, name)
            && scope
                .tracked
                .get(name)
                .is_some_and(|origin| origin.param == index && origin.path.is_empty())
        {
            return if guards_imply_param_ge_one(&scope.guards, name) {
                // One step down: a part, disjoint from nothing else the
                // call could pass of the same `Int`.
                Relation::Part {
                    param: index,
                    path: vec![(0, 0)],
                }
            } else {
                Relation::Unknown
            };
        }
    }
    Relation::Unknown
}

/// Keep every candidate the edges let stand: a callee parameter stays
/// measured only while every call into it hands it a measured parameter of
/// the caller or a part of one, each caller parameter counted at most once
/// per call. Then order the calls that leave the sum unchanged.
fn select(
    fns: &[&FnDef],
    edges: &[Edge<'_>],
    candidates: &[Vec<Candidate>],
) -> Result<Vec<MemberMeasure>, NoMeasure> {
    let mut selected: Vec<HashMap<usize, Candidate>> = candidates
        .iter()
        .map(|cs| cs.iter().map(|c| (c.index, *c)).collect())
        .collect();
    let mut causes: HashMap<(usize, usize), Cause> = HashMap::new();

    let mut changed = true;
    while changed {
        changed = false;
        for (edge_index, edge) in edges.iter().enumerate() {
            // Per caller parameter, the parts already counted on this call,
            // each with the slot that counts it.
            let mut sources: HashMap<usize, Vec<(&Path, usize)>> = HashMap::new();
            let mut slots: Vec<usize> = selected[edge.callee].keys().copied().collect();
            slots.sort_unstable();
            for slot in slots {
                let dropped = match &edge.relations[slot] {
                    Relation::Unknown => Some(Cause::NotAPart {
                        edge: edge_index,
                        slot,
                    }),
                    Relation::Part { param, path } => {
                        let param = *param;
                        let source = selected[edge.caller].get(&param);
                        if source.is_none() {
                            Some(causes.get(&(edge.caller, param)).copied().unwrap_or(
                                Cause::NotAPart {
                                    edge: edge_index,
                                    slot,
                                },
                            ))
                        } else if source
                            .is_some_and(|c| c.kind != selected[edge.callee][&slot].kind)
                        {
                            // A part of a list handed to an `Int` countdown,
                            // say: the two sizes are not comparable.
                            Some(Cause::NotAPart {
                                edge: edge_index,
                                slot,
                            })
                        } else if let Some(&(_, kept)) = sources
                            .get(&param)
                            .and_then(|used| used.iter().find(|(used, _)| overlap(used, path)))
                        {
                            Some(Cause::Twice {
                                edge: edge_index,
                                kept,
                                dropped: slot,
                            })
                        } else {
                            sources.entry(param).or_default().push((path, slot));
                            None
                        }
                    }
                };
                if let Some(cause) = dropped {
                    selected[edge.callee].remove(&slot);
                    causes.insert((edge.callee, slot), cause);
                    changed = true;
                }
            }
        }
    }

    // A call shrinks the sum when some measured argument is a strict part;
    // every other call keeps it equal and must drop the tie-break index.
    let is_strict = |edge: &Edge<'_>| {
        selected[edge.callee]
            .keys()
            .any(|slot| edge.relations[*slot].strict())
    };
    let names: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let mut same_edges: HashMap<String, HashSet<String>> =
        names.iter().map(|n| (n.clone(), HashSet::new())).collect();
    for edge in edges.iter().filter(|edge| !is_strict(edge)) {
        same_edges
            .entry(fns[edge.caller].name.clone())
            .or_default()
            .insert(fns[edge.callee].name.clone());
    }
    let Some(ranks) = ranks_from_same_edges(&names, &same_edges) else {
        return Err(no_measure(fns, edges, &same_edges, &selected, &causes));
    };

    Ok(fns
        .iter()
        .enumerate()
        .map(|(member, fd)| {
            let mut params: Vec<Candidate> = selected[member].values().copied().collect();
            params.sort_by_key(|c| c.index);
            MemberMeasure {
                params,
                rank: ranks[&fd.name],
            }
        })
        .collect())
}

/// The refusal for a group whose unchanged calls form a cycle: the first
/// call on that cycle that dropped a parameter from the measure is the one
/// to name, with why; failing that, the cycle itself.
fn no_measure(
    fns: &[&FnDef],
    edges: &[Edge<'_>],
    same_edges: &HashMap<String, HashSet<String>>,
    selected: &[HashMap<usize, Candidate>],
    causes: &HashMap<(usize, usize), Cause>,
) -> NoMeasure {
    let members = fns
        .iter()
        .map(|fd| format!("`{}`", fd.name))
        .collect::<Vec<_>>()
        .join(", ");
    let cycle = unchanged_cycle(fns, same_edges);
    let on_cycle = |edge: &Edge<'_>| {
        cycle
            .windows(2)
            .any(|pair| pair[0] == edge.caller && pair[1] == edge.callee)
    };
    let root = edges.iter().filter(|edge| on_cycle(edge)).find_map(|edge| {
        (0..fns[edge.callee].params.len())
            .filter(|slot| !selected[edge.callee].contains_key(slot))
            .find_map(|slot| causes.get(&(edge.callee, slot)).copied())
    });
    let detail = match root {
        Some(Cause::NotAPart { edge, slot }) => {
            let edge = &edges[edge];
            let caller = fns[edge.caller];
            let arg = edge.args.get(slot);
            let unguarded_countdown = arg.is_some_and(|arg| {
                caller
                    .params
                    .iter()
                    .any(|(name, type_name)| type_name == "Int" && is_int_minus_positive(arg, name))
            });
            format!(
                "the call from `{}` to `{}` passes `{}` for `{}`, which {}",
                caller.name,
                fns[edge.callee].name,
                arg.map_or_else(String::new, |arg| describe(arg)),
                fns[edge.callee].params[slot].0,
                if unguarded_countdown {
                    "no guard on the call shows to be a smaller number".to_string()
                } else {
                    format!(
                        "is not a parameter of `{}` or a smaller part of one",
                        caller.name
                    )
                },
            )
        }
        Some(Cause::Twice {
            edge,
            kept,
            dropped,
        }) => {
            let edge = &edges[edge];
            let arg = |slot: usize| {
                edge.args
                    .get(slot)
                    .map_or_else(String::new, |arg| describe(arg))
            };
            format!(
                "the call from `{}` to `{}` passes `{}` for `{}`, which overlaps the `{}` it passes for `{}`",
                fns[edge.caller].name,
                fns[edge.callee].name,
                arg(dropped),
                fns[edge.callee].params[dropped].0,
                arg(kept),
                fns[edge.callee].params[kept].0,
            )
        }
        None => format!(
            "no measured argument shrinks on the calls {}",
            cycle
                .iter()
                .map(|member| format!("`{}`", fns[*member].name))
                .collect::<Vec<_>>()
                .join(" → ")
        ),
    };
    NoMeasure {
        reason: format!(
            "the mutual recursion of {members} has no measure that decreases on every call: {detail}"
        ),
    }
}

/// A cycle of unchanged calls as member indices, first member repeated at
/// the end. Members are tried in signature order, so the answer is stable.
fn unchanged_cycle(fns: &[&FnDef], same_edges: &HashMap<String, HashSet<String>>) -> Vec<usize> {
    let index_of = |name: &str| fns.iter().position(|fd| fd.name == name);
    for start in 0..fns.len() {
        let mut path = vec![start];
        if let Some(cycle) = extend_cycle(fns, same_edges, &index_of, &mut path) {
            return cycle;
        }
    }
    Vec::new()
}

fn extend_cycle(
    fns: &[&FnDef],
    same_edges: &HashMap<String, HashSet<String>>,
    index_of: &dyn Fn(&str) -> Option<usize>,
    path: &mut Vec<usize>,
) -> Option<Vec<usize>> {
    let last = *path.last()?;
    let mut next: Vec<usize> = same_edges
        .get(&fns[last].name)
        .into_iter()
        .flatten()
        .filter_map(|name| index_of(name))
        .collect();
    next.sort_unstable();
    for member in next {
        if let Some(at) = path.iter().position(|seen| *seen == member) {
            let mut cycle = path[at..].to_vec();
            cycle.push(member);
            return Some(cycle);
        }
        path.push(member);
        if let Some(cycle) = extend_cycle(fns, same_edges, index_of, path) {
            return Some(cycle);
        }
        path.pop();
    }
    None
}

/// Source-shaped rendering of an argument for the refusal text.
pub(crate) fn describe(expr: &Spanned<Expr>) -> String {
    let list = |items: &[Spanned<Expr>]| items.iter().map(describe).collect::<Vec<_>>().join(", ");
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => name.clone(),
        Expr::Literal(Literal::Int(value)) => value.to_string(),
        Expr::Literal(Literal::Bool(value)) => value.to_string(),
        Expr::Literal(Literal::Str(value)) => format!("{value:?}"),
        Expr::Literal(_) => "a literal".to_string(),
        Expr::FnCall(callee, args) => format!(
            "{}({})",
            expr_to_dotted_name(callee).unwrap_or_else(|| "…".to_string()),
            list(args)
        ),
        Expr::TailCall(boxed) => format!("{}({})", boxed.target, list(&boxed.args)),
        Expr::BinOp(op, left, right) => {
            let symbol = match op {
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
            format!("{} {symbol} {}", describe(left), describe(right))
        }
        Expr::Attr(object, field) => format!("{}.{field}", describe(object)),
        Expr::List(items) => format!("[{}]", list(items)),
        Expr::Neg(inner) => format!("-{}", describe(inner)),
        _ => "an expression".to_string(),
    }
}
