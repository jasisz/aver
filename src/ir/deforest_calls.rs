//! Deforestation across a call: a value one function builds only for its
//! caller to take apart is never built.
//!
//! The shape is a *producer* whose leaves build a sum value and a
//! *consumer* that matches on it at once:
//!
//! ```aver
//! fn parseLine(line: String) -> Line
//!     match line == ""
//!         true -> Line.Blank
//!         false -> match appLine(line)
//!             Option.Some(ids) -> Line.App(ids.ie, ids.fnIdx, ids.argIdx)
//!             Option.None -> otherLine(line)
//!
//! fn applyLine(t: Tables, l: Line) -> Result<Tables, String>
//!     match l
//!         Line.App(ie, f, a) -> Export.Items.appAt(t, ie, f, a)
//!         …
//!         Line.Blank -> Result.Ok(t)
//!
//! applyLine(t, parseLine(l))          -- or directly:  match parseLine(l) …
//! ```
//!
//! Compiled as written, every line allocates a `Line`, hands it across one
//! call, matches it once and frees it. This pass rewrites the call site
//! instead: the producer's body is unfolded in place of the call (its
//! parameters replaced by the arguments) and the consumer's match is
//! pushed into the leaves of that body (case-of-case). A leaf that is a
//! known constructor becomes the matching arm with the constructor's
//! fields in place of the arm's binders (case-of-known-constructor); a
//! leaf whose value is not known — the `otherLine(line)` call — keeps one
//! copy of the whole match. The intermediate value is gone where it was
//! known: no allocation, no construction, no match, no free.
//!
//! A record that reaches a body only through its fields — an argument, or
//! the payload of a known constructor — is taken apart the same way, field
//! by field (the `escape` pass's scalar replacement, as one step of this
//! rule). A site fires only when a known leaf builds a *cell*: a
//! constructor of a user sum type with a field, perhaps inside `Option` or
//! `Result`. A record is a struct by value in the Rust backend and an
//! `Option` around an existing value costs nothing, so a fusion that saves
//! no cell only copies arms; measured on Pestka, fusing those shapes cost
//! 3–4% of the instructions. The record written at a call site stays with
//! `escape`, above the proof line.
//!
//! ## What is fused
//!
//! Two call-site shapes, in any function of the module being compiled:
//!
//! - `match f(args) …` where `f` is a producer of this module and the
//!   match's arms are constructor (or `_`) patterns;
//! - `g(…, v, …)` where `g` is a consumer of this module: its body is one
//!   `match` over the parameter receiving `v`, with constructor (or `_`)
//!   arms that never read that parameter whole; `v` is a producer call of
//!   this module or a constructor written at the call site.
//!
//! A producer is unfolded only when it is small (at most
//! [`MAX_PRODUCER_NODES`] nodes), pure (no declared effects), not
//! recursive (no tail call, no call to itself), and its body has no `?`,
//! no `!` product and no `lazy` call. A `?` returns from the function it
//! is written in; unfolded into the caller it would return from the
//! caller instead (the escape pass had that disease, see its
//! `contains_error_prop`), so such a body stays a call. The same holds for
//! the consumer. A producer whose body is `Bytes.get`'s — a list match over
//! `List.drop` — is left alone too: the packed-index rewrite turns a match
//! on its call into one byte read, and needs the call to find it. A call
//! at a leaf of the unfolded body is unfolded in turn,
//! at most [`MAX_UNFOLD_DEPTH`] deep, so a wrapper that only forwards to a
//! worker (the ABI-preserving wrapper of the string index) is seen
//! through.
//!
//! ## What keeps the code bounded
//!
//! The consumer may be copied at most [`MAX_CONSUMER_COPIES`] times, i.e.
//! at most one unknown leaf; the rewritten site may grow the caller by at
//! most [`MAX_GROWTH_NODES`] nodes, all its sites together at most
//! [`MAX_FN_GROWTH_NODES`]; and at least one known leaf must build a cell,
//! so there is a value to deforest at all. When the budget is
//! not met the deepest unfolding is given up first, and when no depth
//! fits the call stays as written.
//!
//! ## Evaluation order
//!
//! Aver is pure, so the order matters only where a value is not computed
//! at all or computed twice. Every argument of the fused calls and every
//! field of a known constructor is either a variable, a literal or a field
//! read — substituted in place — or is bound once, in source order, by a
//! one-arm `match` (`match value  __df1_x -> …`) before the body that
//! reads it. A field a `_` binder discards is still computed.
//!
//! ## Names
//!
//! An argument reads the caller's variables; the body it is put into — a
//! producer's, or a consumer's arm — binds names of its own, and across
//! two functions the same spelling is common (`pick(Node.Leaf(z + 1), z)`
//! against an arm that binds `Option.Some(z)`). So every binder of a
//! body is renamed to a fresh `__df…` name, which no source program can
//! spell, *before* anything is put into it, and the parameters a body
//! reads are replaced all at once (`substitute`), so what one argument
//! reads is never taken for a parameter put in after it (`p.x` for `a`
//! when `p` is a record parameter read by field).
//!
//! ## Where it sits
//!
//! Below the proof line, with the other body-rewriting passes: it runs
//! after the string index and the list builders, whose workers it sees
//! through, and never on the proof view. The proof exporters and the
//! certificate model read the program as written; what the VM, the Rust
//! backend and wasm-gc compile is the fused one. A `--certify` build keeps
//! the functions the plan printer could print unfused, as the fabricating
//! passes do (see `cert_shape`).

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::ast::{
    Expr, FnBody, FnDef, MatchArm, Pattern, Shared, Spanned, Stmt, StrPart, TopLevel, Type, TypeDef,
};

/// The largest producer body (in AST nodes) the pass unfolds.
pub const MAX_PRODUCER_NODES: usize = 64;
/// How many producers deep a leaf call is unfolded.
pub const MAX_UNFOLD_DEPTH: usize = 4;
/// How many unknown leaves — copies of the whole consumer — a site may
/// have.
pub const MAX_CONSUMER_COPIES: usize = 1;
/// How many nodes a rewritten site may add to its function.
pub const MAX_GROWTH_NODES: usize = 256;
/// How many nodes all the sites of one function may add to it together.
/// A fused site is searched for sites of its own, so a chain of
/// producers each consumed by the next would otherwise be unfolded into
/// its caller whole, one site at a time.
pub const MAX_FN_GROWTH_NODES: usize = 512;
/// Prefix of the fresh binder names the pass writes; no source program can
/// spell a `__` name.
const FRESH_PREFIX: &str = "__df";

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct DeforestCallsPassReport {
    /// Call sites rewritten.
    pub rewrites: usize,
    /// Producer bodies unfolded across all rewritten sites (a wrapper seen
    /// through counts as another).
    pub unfolded: usize,
    /// Leaves that were a known constructor or record and became the
    /// consumer's arm or body.
    pub known_leaves: usize,
    /// Nodes added across all rewritten functions.
    pub grown_nodes: usize,
    /// Functions rewritten: name → (sites fused, nodes added).
    pub sites_by_fn: BTreeMap<String, (usize, usize)>,
}

impl DeforestCallsPassReport {
    pub fn fired(&self) -> bool {
        self.rewrites > 0
    }
}

/// Run the pass on every function of `items` not in `kept`. `foreign` are
/// the lowered dependency modules, read for their sum types (a consumer may
/// match a type another module declares) and their function names (a
/// dotted call to one is never mistaken for a constructor).
pub fn run_deforest_calls_pass(
    items: &mut [TopLevel],
    foreign: &[crate::codegen::ModuleInfo],
    kept: &HashSet<String>,
) -> DeforestCallsPassReport {
    let table = Table::build(items, foreign);
    let mut report = DeforestCallsPassReport::default();
    if table.fns.is_empty() {
        return report;
    }
    for item in items.iter_mut() {
        let TopLevel::FnDef(fd) = item else {
            continue;
        };
        if kept.contains(&fd.name) {
            continue;
        }
        let mut cx = FnCx {
            table: &table,
            fresh: 0,
            unfolded: 0,
            known_leaves: 0,
            rewrites: 0,
            grown: 0,
        };
        let body = std::sync::Arc::make_mut(&mut fd.body);
        for stmt in body.stmts_mut() {
            match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => cx.rewrite(expr),
            }
        }
        if cx.rewrites > 0 {
            report.rewrites += cx.rewrites;
            report.unfolded += cx.unfolded;
            report.known_leaves += cx.known_leaves;
            report.grown_nodes += cx.grown;
            report
                .sites_by_fn
                .insert(fd.name.clone(), (cx.rewrites, cx.grown));
        }
    }
    report
}

// ── The table of functions and types the pass reads ─────────────────

/// A function of this module as the pass sees it: its parameters, its
/// statement bindings, its final expression, whether it may be unfolded
/// (`producer`) and which parameters it takes apart (`consumes`).
struct Shape {
    params: Vec<String>,
    bindings: Vec<(String, Spanned<Expr>)>,
    body: Spanned<Expr>,
    /// Small enough, pure, not recursive (alone or with others), no `?`,
    /// `!` or `lazy`.
    producer: bool,
    /// The parameter the body is one `match` over, with constructor or
    /// `_` arms that never read it whole. `None` when the body has
    /// statement bindings or is anything else.
    consumes: Option<usize>,
}

struct Table {
    fns: HashMap<String, Shape>,
    /// Bare sum type name → variant → field count.
    sums: HashMap<String, HashMap<String, usize>>,
    /// Dotted names of dependency functions (`Module.fn`).
    foreign_fns: HashSet<String>,
}

impl Table {
    fn build(items: &[TopLevel], foreign: &[crate::codegen::ModuleInfo]) -> Self {
        let mut sums: HashMap<String, HashMap<String, usize>> = HashMap::new();
        let mut add_type = |def: &TypeDef| {
            if let TypeDef::Sum { name, variants, .. } = def {
                let entry = sums.entry(bare_name(name).to_string()).or_default();
                for variant in variants {
                    entry.insert(variant.name.clone(), variant.fields.len());
                }
            }
        };
        for item in items {
            if let TopLevel::TypeDef(def) = item {
                add_type(def);
            }
        }
        for module in foreign {
            for def in &module.type_defs {
                add_type(def);
            }
        }
        let foreign_fns = foreign
            .iter()
            .flat_map(|module| {
                module
                    .fn_defs
                    .iter()
                    .map(move |fd| format!("{}.{}", module.prefix, fd.name))
            })
            .collect();
        let recursive = crate::call_graph::find_recursive_fns(items);
        let fns = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => {
                    shape_of(fd, recursive.contains(&fd.name)).map(|shape| (fd.name.clone(), shape))
                }
                _ => None,
            })
            .collect();
        Self {
            fns,
            sums,
            foreign_fns,
        }
    }
}

fn bare_name(dotted: &str) -> &str {
    dotted.rsplit('.').next().unwrap_or(dotted)
}

fn shape_of(fd: &FnDef, recursive: bool) -> Option<Shape> {
    if !fd.effects.is_empty() || fd.name == "main" {
        return None;
    }
    let FnBody::Block(stmts) = fd.body.as_ref();
    let (last, firsts) = stmts.split_last()?;
    let Stmt::Expr(body) = last else {
        return None;
    };
    let mut bindings = Vec::with_capacity(firsts.len());
    for stmt in firsts {
        match stmt {
            Stmt::Binding(name, _, value) => bindings.push((name.clone(), value.clone())),
            Stmt::Expr(_) => return None,
        }
    }
    let params: Vec<String> = fd.params.iter().map(|(name, _)| name.clone()).collect();
    let mut size = size_of(body);
    let mut plain = is_plain(body, &fd.name);
    for (_, value) in &bindings {
        size += size_of(value) + 1;
        plain &= is_plain(value, &fd.name);
    }
    if !plain {
        return None;
    }
    let producer = !recursive && size <= MAX_PRODUCER_NODES && !reads_list_by_drop(body);
    let consumes = if bindings.is_empty() {
        consumed_param(body, &params)
    } else {
        None
    };
    if !producer && consumes.is_none() {
        return None;
    }
    Some(Shape {
        params,
        bindings,
        body: body.clone(),
        producer,
        consumes,
    })
}

/// The parameter `body` is one `match` over, with constructor or `_` arms
/// that never read it whole.
fn consumed_param(body: &Spanned<Expr>, params: &[String]) -> Option<usize> {
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    let Expr::Ident(name) = &subject.node else {
        return None;
    };
    if name == "_" || arms.is_empty() {
        return None;
    }
    let k = params.iter().position(|p| p == name)?;
    let flat = arms
        .iter()
        .all(|arm| matches!(arm.pattern, Pattern::Constructor(..) | Pattern::Wildcard));
    let whole = arms.iter().any(|arm| count_ident(&arm.body, name) > 0);
    (flat && !whole).then_some(k)
}

/// A list match over `List.drop(…)`: the body of `Bytes.get`, which the
/// packed-index rewrite (`ir::mir::optimize::packed_index`) recognises in
/// MIR and turns a match on its call into one indexed byte read. Unfolded
/// here, that match would be gone before the rewrite looks for it, so a
/// reader of this shape is left as a call.
fn reads_list_by_drop(body: &Spanned<Expr>) -> bool {
    let mut found = false;
    walk(body, &mut |e| {
        if let Expr::Match { subject, arms } = &e.node
            && let Expr::FnCall(callee, _) = &subject.node
            && crate::ir::expr_to_dotted_name(&callee.node).as_deref() == Some("List.drop")
            && arms
                .iter()
                .any(|arm| matches!(arm.pattern, Pattern::EmptyList | Pattern::Cons(..)))
        {
            found = true;
        }
    });
    found
}

/// `param` occurs in `body`, and only as the base of a field read.
fn reads_only_fields(body: &Spanned<Expr>, param: &str) -> bool {
    let whole = count_ident(body, param);
    let mut field_reads = 0;
    walk(body, &mut |e| {
        if let Expr::Attr(base, _) = &e.node
            && matches!(&base.node, Expr::Ident(name) if name == param)
        {
            field_reads += 1;
        }
    });
    whole > 0 && whole == field_reads
}

/// No tail call, no `?`, no `!` product, no deferred call, no call of
/// `self_name`, and no pattern a flat match cannot carry.
fn is_plain(expr: &Spanned<Expr>, self_name: &str) -> bool {
    let mut plain = true;
    walk(expr, &mut |e| {
        plain &= match &e.node {
            Expr::TailCall(_) | Expr::ErrorProp(_) | Expr::IndependentProduct(..) => false,
            Expr::FnCall(..) => !e.lazy.is_deferred(),
            Expr::Ident(n) => n != self_name,
            Expr::Match { arms, .. } => arms.iter().all(|arm| {
                !matches!(
                    arm.pattern,
                    Pattern::ConstructorNested(..) | Pattern::List { .. }
                )
            }),
            _ => true,
        };
    });
    plain
}

// ── Walking ─────────────────────────────────────────────────────────

/// Visit `expr` and every expression below it, parents first.
fn walk<'a>(expr: &'a Spanned<Expr>, f: &mut impl FnMut(&'a Spanned<Expr>)) {
    f(expr);
    match &expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(base, _) | Expr::Neg(base) | Expr::ErrorProp(base) => walk(base, f),
        Expr::FnCall(callee, args) => {
            walk(callee, f);
            args.iter().for_each(|a| walk(a, f));
        }
        Expr::BinOp(_, l, r) => {
            walk(l, f);
            walk(r, f);
        }
        Expr::Match { subject, arms } => {
            walk(subject, f);
            arms.iter().for_each(|arm| walk(&arm.body, f));
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload {
                walk(p, f);
            }
        }
        Expr::InterpolatedStr(parts) => parts.iter().for_each(|part| {
            if let StrPart::Parsed(inner) = part {
                walk(inner, f);
            }
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().for_each(|i| walk(i, f));
        }
        Expr::MapLiteral(entries) => entries.iter().for_each(|(k, v)| {
            walk(k, f);
            walk(v, f);
        }),
        Expr::RecordCreate { fields, .. } => fields.iter().for_each(|(_, v)| walk(v, f)),
        Expr::RecordUpdate { base, updates, .. } => {
            walk(base, f);
            updates.iter().for_each(|(_, v)| walk(v, f));
        }
        Expr::TailCall(data) => data.args.iter().for_each(|a| walk(a, f)),
    }
}

/// Visit every direct child of `expr`, mutably.
fn for_each_child_mut(expr: &mut Spanned<Expr>, f: &mut impl FnMut(&mut Spanned<Expr>)) {
    match &mut expr.node {
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
        Expr::Attr(base, _) | Expr::Neg(base) | Expr::ErrorProp(base) => f(base),
        Expr::FnCall(callee, args) => {
            f(callee);
            args.iter_mut().for_each(&mut *f);
        }
        Expr::BinOp(_, l, r) => {
            f(l);
            f(r);
        }
        Expr::Match { subject, arms } => {
            f(subject);
            arms.iter_mut().for_each(|arm| f(&mut arm.body));
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload {
                f(p);
            }
        }
        Expr::InterpolatedStr(parts) => parts.iter_mut().for_each(|part| {
            if let StrPart::Parsed(inner) = part {
                f(inner);
            }
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter_mut().for_each(&mut *f);
        }
        Expr::MapLiteral(entries) => entries.iter_mut().for_each(|(k, v)| {
            f(k);
            f(v);
        }),
        Expr::RecordCreate { fields, .. } => fields.iter_mut().for_each(|(_, v)| f(v)),
        Expr::RecordUpdate { base, updates, .. } => {
            f(base);
            updates.iter_mut().for_each(|(_, v)| f(v));
        }
        Expr::TailCall(data) => data.args.iter_mut().for_each(f),
    }
}

fn size_of(expr: &Spanned<Expr>) -> usize {
    let mut n = 0;
    walk(expr, &mut |_| n += 1);
    n
}

fn count_ident(expr: &Spanned<Expr>, name: &str) -> usize {
    let mut n = 0;
    walk(expr, &mut |e| {
        if matches!(&e.node, Expr::Ident(id) if id == name) {
            n += 1;
        }
    });
    n
}

fn count_field_read(expr: &Spanned<Expr>, param: &str, field: &str) -> usize {
    let mut n = 0;
    walk(expr, &mut |e| {
        if let Expr::Attr(base, name) = &e.node
            && name == field
            && matches!(&base.node, Expr::Ident(id) if id == param)
        {
            n += 1;
        }
    });
    n
}

/// Replace every `Ident` named in `idents` and every `param.field` named
/// in `fields`, in place and all at once: what is put in is not looked
/// into again, so a variable of the caller read by an argument is never
/// taken for a parameter of the same spelling put in after it. Types,
/// lines and `lazy` marks on every other node stay as they are. Binders
/// are not consulted: callers rename them to fresh names first.
fn substitute(
    expr: &mut Spanned<Expr>,
    idents: &HashMap<&str, &Spanned<Expr>>,
    fields: &HashMap<&str, &HashMap<String, Spanned<Expr>>>,
) {
    let value = match &expr.node {
        Expr::Ident(name) => idents.get(name.as_str()).copied(),
        Expr::Attr(base, field) => match &base.node {
            Expr::Ident(name) => fields.get(name.as_str()).and_then(|map| map.get(field)),
            _ => None,
        },
        _ => None,
    };
    if let Some(value) = value {
        let ty = expr.ty.get().cloned();
        *expr = value.clone();
        if expr.ty.get().is_none()
            && let Some(ty) = ty
        {
            expr.set_ty_shared(&ty);
        }
        return;
    }
    for_each_child_mut(expr, &mut |child| substitute(child, idents, fields));
}

fn rename_ident(expr: &mut Spanned<Expr>, from: &str, to: &str) {
    if let Expr::Ident(name) = &mut expr.node {
        if name == from {
            *name = to.to_string();
        }
        return;
    }
    for_each_child_mut(expr, &mut |child| rename_ident(child, from, to));
}

// ── Classifying expressions ─────────────────────────────────────────

/// A variable or a constant (a literal, negated or not, or the empty list):
/// substituted in place however often it is read, and dropped when unread.
/// An unread `[]` bound by a `_` would leave the Rust backend a list whose
/// element type nothing names.
fn is_trivial(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Ident(_) | Expr::Literal(_) => true,
        Expr::List(items) => items.is_empty(),
        Expr::Neg(inner) => matches!(inner.node, Expr::Literal(_)),
        _ => false,
    }
}

/// A chain of field reads on a variable: pure, so it may be read later or
/// not at all, and cheap enough to read once in place.
fn is_field_read(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Ident(_) => true,
        Expr::Attr(base, _) => is_field_read(base),
        _ => false,
    }
}

/// `Some((name, args))` for `Name.Path(args)`, `Name.Path` or a
/// constructor literal.
fn dotted_call(expr: &Spanned<Expr>) -> Option<(String, Vec<Spanned<Expr>>)> {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let name = crate::ir::expr_to_dotted_name(&callee.node)?;
            Some((name, args.iter().cloned().collect()))
        }
        Expr::Attr(..) => Some((crate::ir::expr_to_dotted_name(&expr.node)?, Vec::new())),
        Expr::Constructor(name, payload) => Some((
            name.clone(),
            payload.iter().map(|p| (**p).clone()).collect(),
        )),
        _ => None,
    }
}

/// The constructor key both a pattern and an expression spell: the type's
/// bare name and the variant. `Option.None` may be written `None`.
fn ctor_key(dotted: &str) -> (String, String) {
    match dotted.rsplit_once('.') {
        Some((ty, variant)) => (bare_name(ty).to_string(), variant.to_string()),
        None => ("Option".to_string(), dotted.to_string()),
    }
}

/// The same type for the purpose of matching a leaf against the consumer:
/// user types by identity, `Option` and `Result` by kind.
fn same_type(subject: &Type, leaf: &Type) -> bool {
    match (subject, leaf) {
        (Type::Named { .. }, Type::Named { .. }) => subject == leaf,
        (Type::Option(_), Type::Option(_)) | (Type::Result(..), Type::Result(..)) => true,
        _ => false,
    }
}

/// `Some(name)` when `expr` is `Ident(name)` applied to arguments.
fn called_fn(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::FnCall(callee, _) => match &callee.node {
            Expr::Ident(name) => Some(name.as_str()),
            _ => None,
        },
        _ => None,
    }
}

// ── The consumer ────────────────────────────────────────────────────

/// The match pushed into the producer's leaves, with the consumer's other
/// parameters already replaced by the call's arguments.
struct Consumer {
    arms: Vec<MatchArm>,
    subject_ty: Shared<Type>,
}

impl Consumer {
    /// The arm a leaf constructor `key` with `arity` fields selects, with
    /// the binders of its pattern — none for a `_` arm.
    fn arm_for(
        &self,
        key: &(String, String),
        arity: usize,
    ) -> Option<(Vec<String>, Spanned<Expr>)> {
        for arm in &self.arms {
            match &arm.pattern {
                Pattern::Constructor(name, binders) => {
                    if ctor_key(name) == *key {
                        return (binders.len() == arity)
                            .then(|| (binders.clone(), (*arm.body).clone()));
                    }
                }
                Pattern::Wildcard => return Some((Vec::new(), (*arm.body).clone())),
                _ => return None,
            }
        }
        None
    }
}

// ── Rewriting one function ──────────────────────────────────────────

struct FnCx<'a> {
    table: &'a Table,
    fresh: usize,
    unfolded: usize,
    known_leaves: usize,
    rewrites: usize,
    /// Nodes the function has grown by so far.
    grown: usize,
}

/// What a fusion attempt produced, before the budget is checked.
struct Fused {
    expr: Spanned<Expr>,
    /// Unknown leaves, each holding a copy of the whole consumer.
    copies: usize,
    /// Known leaves that build a cell: a constructor of a user sum type
    /// with a field, perhaps inside `Option`/`Result`.
    built: usize,
    known: usize,
    unfolded: usize,
}

/// A binding the fused expression starts with, in evaluation order.
type Binding = (Pattern, Spanned<Expr>);

struct Unfolded {
    prelude: Vec<Binding>,
    body: Spanned<Expr>,
}

/// How a value reaches the body that reads it (see [`FnCx::argument`]).
enum Bound {
    /// Every read of the name becomes this expression.
    InPlace(Spanned<Expr>),
    /// Every read `name.field` becomes the field's expression; a field not
    /// in the map was bound in the prelude or dropped.
    Fields(HashMap<String, Spanned<Expr>>),
    /// Bound in the prelude or dropped: nothing to substitute.
    Done,
}

/// The reads of a field as a body that reads an identifier, so a field's
/// value is classified like an argument: `count_ident` of the field name
/// in it is the number of reads of `param.field` in `body`.
fn field_reads_as_ident(body: &Spanned<Expr>, param: &str, field: &str) -> Spanned<Expr> {
    let reads = count_field_read(body, param, field);
    let read = Spanned::bare(Expr::Ident(field.to_string()));
    Spanned::bare(Expr::List(Shared::new(vec![read; reads])))
}

/// Apply what [`FnCx::argument`] decided for every parameter, all at once.
/// `body` must have had its binders made fresh first (see
/// [`FnCx::fresh_body`]): the arguments read the caller's variables, and a
/// binder of `body` spelled like one would capture it.
fn apply_bound(body: &mut Spanned<Expr>, bound: &[(String, Bound)]) {
    let mut idents: HashMap<&str, &Spanned<Expr>> = HashMap::new();
    let mut fields: HashMap<&str, &HashMap<String, Spanned<Expr>>> = HashMap::new();
    for (param, how) in bound {
        match how {
            Bound::InPlace(value) => {
                idents.insert(param, value);
            }
            Bound::Fields(map) => {
                fields.insert(param, map);
            }
            Bound::Done => {}
        }
    }
    substitute(body, &idents, &fields);
}

impl FnCx<'_> {
    /// A name made fresh twice (an arm's binder made fresh at the site,
    /// then bound once as a field) keeps the name it had in the source.
    fn fresh_name(&mut self, original: &str) -> String {
        let mut base = original;
        while let Some((_, name)) = base
            .strip_prefix(FRESH_PREFIX)
            .and_then(|rest| rest.split_once('_'))
        {
            base = name;
        }
        self.fresh += 1;
        format!("{FRESH_PREFIX}{}_{base}", self.fresh)
    }

    /// Children first, so a site inside an argument is fused before the
    /// call around it is looked at.
    fn rewrite(&mut self, expr: &mut Spanned<Expr>) {
        for_each_child_mut(expr, &mut |child| self.rewrite(child));
        if let Some(fused) = self.try_site(expr) {
            *expr = fused;
            self.rewrites += 1;
            // The unfolded bodies carry sites of their own (the producer
            // matching what a further producer builds); they are fused
            // as the standalone bodies are. This ends: a producer is not
            // recursive, so what it unfolds to calls only functions
            // further down the call graph.
            self.rewrite(expr);
        }
    }

    fn try_site(&mut self, expr: &Spanned<Expr>) -> Option<Spanned<Expr>> {
        if expr.lazy.is_deferred() {
            return None;
        }
        let result_ty = expr.ty.get().cloned();
        let original_size = size_of(expr);
        match &expr.node {
            Expr::Match { subject, arms } => {
                let f = called_fn(subject)?;
                if subject.lazy.is_deferred() || !self.table.fns.get(f)?.producer {
                    return None;
                }
                if !arms
                    .iter()
                    .all(|arm| matches!(arm.pattern, Pattern::Constructor(..) | Pattern::Wildcard))
                {
                    return None;
                }
                let consumer = Consumer {
                    arms: arms.iter().cloned().collect(),
                    subject_ty: subject.ty.get()?.clone(),
                };
                let producer = (**subject).clone();
                self.fuse_site(Vec::new(), producer, consumer, result_ty, original_size)
            }
            Expr::FnCall(callee, args) => {
                let Expr::Ident(g) = &callee.node else {
                    return None;
                };
                let shape = self.table.fns.get(g)?;
                if args.len() != shape.params.len() {
                    return None;
                }
                // The argument `g` matches on: a producer call of this
                // module, or a constructor written at the call site.
                let k = shape.consumes?;
                let arg = &args[k];
                let producible = match called_fn(arg) {
                    Some(f) => self.table.fns.get(f).is_some_and(|s| s.producer),
                    None => dotted_call(arg).is_some(),
                };
                if arg.lazy.is_deferred() || !producible {
                    return None;
                }
                // Arguments after the producer's are read in place only if
                // reading them later cannot be told from reading them
                // first; the ones before it are bound in order.
                if !args[k + 1..].iter().all(is_field_read) {
                    return None;
                }
                let mut prelude = Vec::new();
                let mut bound: Vec<(String, Bound)> = Vec::new();
                for (i, (param, arg)) in shape.params.iter().zip(args.iter()).enumerate() {
                    if i == k {
                        continue;
                    }
                    let how = self.argument(param, arg, &shape.body, &mut prelude)?;
                    bound.push((param.clone(), how));
                }
                let Expr::Match { subject, arms } = &shape.body.node else {
                    return None;
                };
                let subject_ty = subject.ty.get()?.clone();
                // The arms' binders are made fresh before the arguments
                // go in: an argument may read a variable of the caller
                // spelled like a binder of the arm, its own or one nested
                // below it.
                let arms: Vec<MatchArm> = arms
                    .iter()
                    .map(|arm| {
                        let mut arm = self.fresh_arm(arm);
                        let mut body = (*arm.body).clone();
                        apply_bound(&mut body, &bound);
                        arm.body = Shared::new(body);
                        arm
                    })
                    .collect();
                let consumer = Consumer { arms, subject_ty };
                self.fuse_site(prelude, args[k].clone(), consumer, result_ty, original_size)
            }
            _ => None,
        }
    }

    /// How the value `arg` reaches `body`, which reads it as `param`:
    /// substituted in place, bound once in `prelude` (or dropped when
    /// trivial and unread), or — a record whose fields are all `body`
    /// reads of it — taken apart into its fields, each reaching `body`
    /// the same way. `None` declines the site.
    fn argument(
        &mut self,
        param: &str,
        arg: &Spanned<Expr>,
        body: &Spanned<Expr>,
        prelude: &mut Vec<Binding>,
    ) -> Option<Bound> {
        if param == "_" {
            if !is_trivial(arg) {
                prelude.push((Pattern::Wildcard, arg.clone()));
            }
            return Some(Bound::Done);
        }
        let uses = count_ident(body, param);
        if is_trivial(arg) || (is_field_read(arg) && uses <= 1) {
            return Some(Bound::InPlace(arg.clone()));
        }
        if let Expr::RecordCreate { fields, .. } = &arg.node
            && (uses == 0 || reads_only_fields(body, param))
        {
            let mut map = HashMap::new();
            for (field, value) in fields.iter() {
                let reads = count_field_read(body, param, field);
                let binder = if reads == 0 { "_" } else { field.as_str() };
                match self.argument(
                    binder,
                    value,
                    &field_reads_as_ident(body, param, field),
                    prelude,
                )? {
                    Bound::InPlace(value) => {
                        map.insert(field.clone(), value);
                    }
                    Bound::Fields(..) | Bound::Done => {}
                }
            }
            return Some(Bound::Fields(map));
        }
        // Bound once, in order: the binder takes the argument's type from
        // the subject, so an unstamped argument cannot be bound.
        arg.ty.get()?;
        let name = self.fresh_name(param);
        prelude.push((Pattern::Ident(name.clone()), arg.clone()));
        Some(Bound::InPlace(ident_like(&name, arg)))
    }

    /// Try the deepest unfolding first; the first one within budget wins.
    fn fuse_site(
        &mut self,
        prelude: Vec<Binding>,
        producer: Spanned<Expr>,
        consumer: Consumer,
        result_ty: Option<Shared<Type>>,
        original_size: usize,
    ) -> Option<Spanned<Expr>> {
        let fresh_at_start = self.fresh;
        for depth in (1..=MAX_UNFOLD_DEPTH).rev() {
            self.fresh = fresh_at_start;
            let fused = self.fuse_leaf(producer.clone(), &consumer, result_ty.as_ref(), depth);
            if fused.copies > MAX_CONSUMER_COPIES || fused.built == 0 {
                continue;
            }
            let mut expr = fused.expr;
            for (pattern, value) in prelude.iter().rev() {
                expr = let_match(pattern.clone(), value.clone(), expr, result_ty.as_ref());
            }
            let grown = size_of(&expr).saturating_sub(original_size);
            if grown > MAX_GROWTH_NODES || self.grown + grown > MAX_FN_GROWTH_NODES {
                continue;
            }
            self.grown += grown;
            self.unfolded += fused.unfolded;
            self.known_leaves += fused.known;
            return Some(expr);
        }
        self.fresh = fresh_at_start;
        None
    }

    /// The consumer pushed into `leaf`: a known constructor or record
    /// becomes the consumer's arm or body, a producer call is unfolded
    /// (`depth` permitting) and its own leaves taken in turn, anything else
    /// keeps the whole consumer.
    fn fuse_leaf(
        &mut self,
        leaf: Spanned<Expr>,
        consumer: &Consumer,
        result_ty: Option<&Shared<Type>>,
        depth: usize,
    ) -> Fused {
        if let Some(reduced) = self.reduce_known(&leaf, consumer, result_ty) {
            return reduced;
        }
        if depth > 0
            && let Some(f) = called_fn(&leaf)
            && let Some(shape) = self.table.fns.get(f)
            && shape.producer
            && let Expr::FnCall(_, args) = &leaf.node
            && args.len() == shape.params.len()
            && let Some(unfolded) = self.unfold(shape, args)
        {
            let mut fused = self.push_into(unfolded.body, consumer, result_ty, depth - 1);
            for (pattern, value) in unfolded.prelude.into_iter().rev() {
                fused.expr = let_match(pattern, value, fused.expr, result_ty);
            }
            fused.unfolded += 1;
            return fused;
        }
        let line = leaf.line;
        let arms: Vec<MatchArm> = consumer
            .arms
            .iter()
            .map(|arm| self.fresh_arm(arm))
            .collect();
        let expr = typed(
            Expr::Match {
                subject: Shared::new(leaf),
                arms: Shared::new(arms),
            },
            line,
            result_ty,
        );
        Fused {
            expr,
            copies: 1,
            built: 0,
            known: 0,
            unfolded: 0,
        }
    }

    /// Walk down through the matches of an unfolded body to its leaves.
    fn push_into(
        &mut self,
        mut body: Spanned<Expr>,
        consumer: &Consumer,
        result_ty: Option<&Shared<Type>>,
        depth: usize,
    ) -> Fused {
        if let Expr::Match { arms, .. } = &mut body.node {
            let mut total = Fused {
                expr: Spanned::bare(Expr::Literal(crate::ast::Literal::Int(0))),
                copies: 0,
                built: 0,
                known: 0,
                unfolded: 0,
            };
            for arm in arms.iter_mut() {
                let fused = self.push_into((*arm.body).clone(), consumer, result_ty, depth);
                total.copies += fused.copies;
                total.built += fused.built;
                total.known += fused.known;
                total.unfolded += fused.unfolded;
                arm.body = Shared::new(fused.expr);
            }
            if let Some(ty) = result_ty {
                body.ty = std::sync::OnceLock::from(ty.clone());
            }
            total.expr = body;
            return total;
        }
        self.fuse_leaf(body, consumer, result_ty, depth)
    }

    /// `leaf` is a constructor of the consumer's type: the arm it selects,
    /// with the leaf's fields for the arm's binders.
    fn reduce_known(
        &mut self,
        leaf: &Spanned<Expr>,
        consumer: &Consumer,
        result_ty: Option<&Shared<Type>>,
    ) -> Option<Fused> {
        let leaf_ty = leaf.ty.get()?;
        if !same_type(&consumer.subject_ty, leaf_ty) {
            return None;
        }
        self.reduce_ctor(leaf, consumer, result_ty)
    }

    fn reduce_ctor(
        &mut self,
        leaf: &Spanned<Expr>,
        consumer: &Consumer,
        result_ty: Option<&Shared<Type>>,
    ) -> Option<Fused> {
        let (dotted, fields) = dotted_call(leaf)?;
        let key = ctor_key(&dotted);
        let known = match key.0.as_str() {
            "Option" => matches!((key.1.as_str(), fields.len()), ("Some", 1) | ("None", 0)),
            "Result" => matches!((key.1.as_str(), fields.len()), ("Ok", 1) | ("Err", 1)),
            ty => {
                !self.table.foreign_fns.contains(&dotted)
                    && !self.table.fns.contains_key(&dotted)
                    && consumer
                        .subject_ty
                        .named_name()
                        .is_some_and(|name| bare_name(name) == ty)
                    && self
                        .table
                        .sums
                        .get(ty)
                        .and_then(|variants| variants.get(&key.1))
                        .is_some_and(|arity| *arity == fields.len())
            }
        };
        if !known {
            return None;
        }
        let (binders, body) = consumer.arm_for(&key, fields.len())?;
        let mut body = self.fresh_body(body);
        // Fields are bound in source order, outermost first; a field a
        // binder reads once in place is not bound.
        let mut wraps: Vec<Binding> = Vec::new();
        let mut bound: Vec<(String, Bound)> = Vec::new();
        for (i, field) in fields.iter().enumerate() {
            let binder = binders.get(i).map(String::as_str).unwrap_or("_");
            let how = self.argument(binder, field, &body, &mut wraps)?;
            bound.push((binder.to_string(), how));
        }
        apply_bound(&mut body, &bound);
        for (pattern, value) in wraps.into_iter().rev() {
            body = let_match(pattern, value, body, result_ty);
        }
        Some(Fused {
            expr: body,
            copies: 0,
            built: usize::from(self.builds_cell(leaf)),
            known: 1,
            unfolded: 0,
        })
    }

    /// Whether `expr` builds a cell every runtime backend allocates: a
    /// constructor of a user sum type with at least one field, directly or
    /// as the payload of `Option.Some` / `Result.Ok` / `Result.Err`. A
    /// nullary constructor, an `Option` or `Result` around a value that
    /// exists already, and a record (a struct by value in the Rust backend)
    /// cost no cell, and a fusion that saves no cell only copies arms.
    fn builds_cell(&self, expr: &Spanned<Expr>) -> bool {
        let Some((dotted, fields)) = dotted_call(expr) else {
            return false;
        };
        let key = ctor_key(&dotted);
        match key.0.as_str() {
            "Option" | "Result" => fields.iter().any(|f| self.builds_cell(f)),
            ty => {
                !fields.is_empty()
                    && self
                        .table
                        .sums
                        .get(ty)
                        .is_some_and(|variants| variants.contains_key(&key.1))
            }
        }
    }

    /// The producer's body with its parameters replaced by `args` and its
    /// binders renamed; arguments that are not read in place are bound in
    /// `prelude`, in order, as are the body's own statement bindings.
    fn unfold(&mut self, shape: &Shape, args: &[Spanned<Expr>]) -> Option<Unfolded> {
        let mut prelude = Vec::new();
        // The whole body, bindings first, as the reads of a parameter.
        let whole = Spanned::bare(Expr::List(Shared::new(
            shape
                .bindings
                .iter()
                .map(|(_, value)| value.clone())
                .chain(std::iter::once(shape.body.clone()))
                .collect(),
        )));
        let mut bound: Vec<(String, Bound)> = Vec::new();
        for (param, arg) in shape.params.iter().zip(args) {
            let how = self.argument(param, arg, &whole, &mut prelude)?;
            bound.push((param.clone(), how));
        }
        for (name, value) in &shape.bindings {
            let mut value = self.fresh_body(value.clone());
            apply_bound(&mut value, &bound);
            value.ty.get()?;
            let fresh = self.fresh_name(name);
            let reference = ident_like(&fresh, &value);
            prelude.push((Pattern::Ident(fresh), value));
            bound.push((name.clone(), Bound::InPlace(reference)));
        }
        let mut body = self.fresh_body(shape.body.clone());
        apply_bound(&mut body, &bound);
        Some(Unfolded { prelude, body })
    }

    /// `arm` with fresh binders, for a copy of the consumer's match.
    fn fresh_arm(&mut self, arm: &MatchArm) -> MatchArm {
        let mut body = (*arm.body).clone();
        let pattern = self.fresh_pattern(&arm.pattern, &mut body);
        let body = self.fresh_body(body);
        MatchArm::new(pattern, body)
    }

    /// Every binder below `body` renamed to a fresh name.
    fn fresh_body(&mut self, mut body: Spanned<Expr>) -> Spanned<Expr> {
        self.rename_below(&mut body);
        body
    }

    fn rename_below(&mut self, expr: &mut Spanned<Expr>) {
        if let Expr::Match { arms, .. } = &mut expr.node {
            for arm in arms.iter_mut() {
                let mut body = (*arm.body).clone();
                arm.pattern = self.fresh_pattern(&arm.pattern, &mut body);
                arm.body = Shared::new(body);
            }
        }
        for_each_child_mut(expr, &mut |child| self.rename_below(child));
    }

    /// `pattern` with every binder renamed, the renames applied to `body`.
    fn fresh_pattern(&mut self, pattern: &Pattern, body: &mut Spanned<Expr>) -> Pattern {
        let rename = |cx: &mut Self, name: &String, body: &mut Spanned<Expr>| -> String {
            if name == "_" {
                return name.clone();
            }
            let fresh = cx.fresh_name(name);
            rename_ident(body, name, &fresh);
            fresh
        };
        match pattern {
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => pattern.clone(),
            Pattern::Ident(name) => Pattern::Ident(rename(self, name, body)),
            Pattern::Cons(head, tail) => {
                let head = rename(self, head, body);
                let tail = rename(self, tail, body);
                Pattern::Cons(head, tail)
            }
            Pattern::Constructor(name, binders) => Pattern::Constructor(
                name.clone(),
                binders.iter().map(|b| rename(self, b, body)).collect(),
            ),
            Pattern::Tuple(items) => Pattern::Tuple(
                items
                    .iter()
                    .map(|item| self.fresh_pattern(item, body))
                    .collect(),
            ),
            // Not in a lowered program (see `is_plain`).
            Pattern::ConstructorNested(..) | Pattern::List { .. } => pattern.clone(),
        }
    }
}

/// A reference to the binder `name`, typed like `like`.
fn ident_like(name: &str, like: &Spanned<Expr>) -> Spanned<Expr> {
    typed(Expr::Ident(name.to_string()), like.line, like.ty.get())
}

fn typed(node: Expr, line: crate::ast::SourceLine, ty: Option<&Shared<Type>>) -> Spanned<Expr> {
    let expr = Spanned::new(node, line);
    if let Some(ty) = ty {
        expr.set_ty_shared(ty);
    }
    expr
}

/// `match value  pattern -> body`: a binding in expression position.
fn let_match(
    pattern: Pattern,
    value: Spanned<Expr>,
    body: Spanned<Expr>,
    result_ty: Option<&Shared<Type>>,
) -> Spanned<Expr> {
    let line = value.line;
    typed(
        Expr::Match {
            subject: Shared::new(value),
            arms: Shared::new(vec![MatchArm::new(pattern, body)]),
        },
        line,
        result_ty,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A program through the front door, as the pass sees it: parsed,
    /// tail calls marked, nested patterns compiled, types stamped.
    fn prepared(source: &str) -> Vec<TopLevel> {
        let mut items = crate::source::parse_source(source).expect("fixture parses");
        let user_program_len = items.len();
        let tc = crate::ir::pipeline::front_gate(
            &mut items,
            &crate::ir::TypecheckMode::Full { base_dir: None },
            user_program_len,
            &crate::config::MarkedCapabilities::none(),
        );
        assert!(tc.errors.is_empty(), "fixture typechecks: {:?}", tc.errors);
        items
    }

    fn run(items: &mut [TopLevel]) -> DeforestCallsPassReport {
        run_deforest_calls_pass(items, &[], &HashSet::new())
    }

    fn rendered(items: &[TopLevel], name: &str) -> String {
        let item = items
            .iter()
            .find(|item| matches!(item, TopLevel::FnDef(fd) if fd.name == name))
            .unwrap_or_else(|| panic!("{name} is present"));
        crate::ir::dump::dump_items(std::slice::from_ref(item), None)
    }

    const LINES: &str = r#"module Lines
    intent = "A sum built by one function and matched by the next."
    effects []

type Line
    App(Int, Int)
    Name(String)
    Blank
    Bad(String)

record Ids
    a: Int
    b: Int

fn probe(line: String) -> Option<Ids>
    match String.startsWith(line, "a")
        true -> Option.Some(Ids(a = String.len(line), b = 2))
        false -> Option.None

fn other(line: String) -> Line
    match String.startsWith(line, "n")
        true -> Line.Name(line)
        false -> match String.len(line) > 10
            true -> other(String.slice(line, 0, 10))
            false -> Line.Bad(line)

fn parse(line: String) -> Line
    match line == ""
        true -> Line.Blank
        false -> match probe(line)
            Option.Some(ids) -> Line.App(ids.a, ids.b)
            Option.None -> other(line)

fn apply(t: Int, l: Line) -> Result<Int, String>
    match l
        Line.App(a, b) -> Result.Ok(t + a * b)
        Line.Name(n) -> Result.Ok(t + String.len(n))
        Line.Blank -> Result.Ok(t)
        Line.Bad(e) -> Result.Err(e)

fn step(t: Int, line: String) -> Result<Int, String>
    apply(t, parse(line))

fn direct(line: String) -> Int
    match parse(line)
        Line.App(a, b) -> a + b
        _ -> 0
"#;

    #[test]
    fn sum_built_by_a_call_is_matched_in_place() {
        let mut items = prepared(LINES);
        let report = run(&mut items);
        // `parse` matches what `probe` builds too, but `probe` builds no
        // cell — an `Option` around a record — so that site stays.
        assert_eq!(report.rewrites, 2, "{report:?}");
        assert_eq!(report.sites_by_fn.get("step").map(|s| s.0), Some(1));
        assert_eq!(report.sites_by_fn.get("direct").map(|s| s.0), Some(1));
        assert!(!report.sites_by_fn.contains_key("parse"), "{report:?}");
        let step = rendered(&items, "step");
        assert!(!step.contains("parse("), "producer unfolded: {step}");
        assert!(!step.contains("apply("), "consumer unfolded: {step}");
        assert!(!step.contains("Line.App("), "known leaf reduced: {step}");
        assert!(
            step.contains("other("),
            "unknown leaf keeps its call: {step}"
        );
        assert_eq!(
            step.matches("Line.Bad").count(),
            1,
            "the consumer match is copied once: {step}"
        );
        assert!(step.contains("__df"), "binders are fresh: {step}");
        let direct = rendered(&items, "direct");
        assert!(!direct.contains("parse("), "{direct}");
        assert!(direct.contains("other("), "{direct}");
    }

    /// A record is no cell: the site stays for `escape`, which splices the
    /// record written at the call after slot resolution. A record inside
    /// a sum constructor is taken apart with the sum.
    #[test]
    fn a_record_alone_is_not_a_cell_but_one_inside_a_sum_is() {
        let mut items = prepared(
            r#"module Points
    intent = "Scalar replacement belongs to escape; a record in a cell goes with the cell."
    effects []

record Point
    x: Int
    y: Int

type Shape
    At(Point)
    Nowhere

fn distance(p: Point) -> Int
    p.x * p.x + p.y * p.y

fn mk(n: Int) -> Point
    match n > 0
        true -> Point(x = n, y = n + 1)
        false -> Point(x = 0, y = 0)

fn direct(n: Int) -> Int
    distance(Point(x = n, y = n * 2))

fn viaCall(n: Int) -> Int
    distance(mk(n))

fn place(n: Int) -> Shape
    match n > 0
        true -> Shape.At(Point(x = n, y = n * 2))
        false -> Shape.Nowhere

fn far(s: Shape) -> Int
    match s
        Shape.At(p) -> p.x * p.x + p.y
        Shape.Nowhere -> 0

fn viaSum(n: Int) -> Int
    far(place(n))
"#,
        );
        let report = run(&mut items);
        assert_eq!(report.rewrites, 1, "{report:?}");
        assert!(report.sites_by_fn.contains_key("viaSum"), "{report:?}");
        let via = rendered(&items, "viaSum");
        assert!(
            !via.contains("Shape.At(") && !via.contains("Point("),
            "{via}"
        );
        assert!(
            via.contains("__df"),
            "`n * 2` is read twice through the record, so it is bound once: {via}"
        );
    }

    #[test]
    fn a_producer_with_error_propagation_stays_a_call() {
        let mut items = prepared(
            r#"module Props
    intent = "A `?` returns from the function it is written in."
    effects []

type Shape
    Circle(Int)
    Empty

fn radius(s: String) -> Result<Int, String>
    match s == ""
        true -> Result.Err("empty")
        false -> Result.Ok(String.len(s))

fn parse(s: String) -> Result<Shape, String>
    Result.Ok(Shape.Circle(radius(s)?))

fn area(s: Shape) -> Int
    match s
        Shape.Circle(r) -> r * r
        Shape.Empty -> 0

fn areaOf(s: String) -> Result<Int, String>
    Result.Ok(area(parse(s)?))

fn tryArea(r: Result<Shape, String>) -> Result<Int, String>
    match r
        Result.Ok(s) -> Result.Ok(area(s))
        Result.Err(e) -> Result.Err(e)

fn both(s: String) -> Result<Int, String>
    tryArea(parse(s))
"#,
        );
        let report = run(&mut items);
        assert_eq!(report.rewrites, 0, "{report:?}");
    }

    #[test]
    fn a_consumer_arm_with_error_propagation_stays_a_call() {
        let mut items = prepared(
            r#"module Arms
    intent = "A consumer whose arm leaves early is not spliced."
    effects []

type Token
    Num(String)
    Word(String)

fn lex(s: String) -> Token
    match String.startsWith(s, "1")
        true -> Token.Num(s)
        false -> Token.Word(s)

fn digits(s: String) -> Result<Int, String>
    match String.len(s) > 0
        true -> Result.Ok(String.len(s))
        false -> Result.Err("empty")

fn value(t: Token) -> Result<Int, String>
    match t
        Token.Num(s) -> Result.Ok(digits(s)?)
        Token.Word(_) -> Result.Err("word")

fn run(s: String) -> Result<Int, String>
    value(lex(s))
"#,
        );
        let report = run(&mut items);
        assert_eq!(report.rewrites, 0, "{report:?}");
    }

    #[test]
    fn too_many_unknown_leaves_is_declined() {
        let mut items = prepared(
            r#"module Wide
    intent = "Two unknown leaves would copy the consumer twice."
    effects []

type Line
    App(Int)
    Blank

fn left(s: String) -> Line
    Line.App(String.len(s))

fn right(s: String) -> Line
    Line.Blank

fn parse(s: String) -> Line
    match String.startsWith(s, "l")
        true -> left(s)
        false -> match String.startsWith(s, "r")
            true -> right(s)
            false -> Line.App(0)

fn apply(l: Line) -> Int
    match l
        Line.App(n) -> n
        Line.Blank -> 0

fn step(s: String) -> Int
    apply(parse(s))
"#,
        );
        let report = run(&mut items);
        // `left` and `right` are unfolded in turn, so every leaf is known.
        assert_eq!(report.rewrites, 1, "{report:?}");
        assert_eq!(report.known_leaves, 3, "{report:?}");
    }

    #[test]
    fn a_big_consumer_is_declined_by_the_growth_budget() {
        let mut arms = String::new();
        let mut variants = String::new();
        for i in 0..40 {
            variants.push_str(&format!("    V{i}(Int)\n"));
            arms.push_str(&format!(
                "        Big.V{i}(n) -> n + {i} * String.len(\"{i}\") + Int.abs(n - {i}) + n * n\n"
            ));
        }
        let source = format!(
            r#"module Budget
    intent = "A consumer whose one copy alone exceeds the growth budget."
    effects []

type Big
{variants}
fn make(s: String) -> Big
    match String.startsWith(s, "a")
        true -> Big.V1(String.len(s))
        false -> other(s)

fn other(s: String) -> Big
    Big.V2(String.len(s) * 2)

fn use(b: Big) -> Int
    match b
{arms}
fn step(s: String) -> Int
    use(make(s))
"#
        );
        let mut items = prepared(&source);
        let report = run(&mut items);
        assert_eq!(report.rewrites, 1, "every leaf is known: {report:?}");

        let source = source.replace(
            "fn other(s: String) -> Big\n    Big.V2(String.len(s) * 2)",
            "fn other(s: String) -> Big\n    match String.len(s) > 3\n        true -> Big.V2(String.len(s) * 2)\n        false -> third(s)\n\nfn third(s: String) -> Big\n    match String.len(s) > 5\n        true -> Big.V3(1)\n        false -> fourth(s)\n\nfn fourth(s: String) -> Big\n    match String.len(s) > 7\n        true -> Big.V4(2)\n        false -> fifth(s)\n\nfn fifth(s: String) -> Big\n    Big.V5(3)",
        );
        let mut items = prepared(&source);
        let report = run(&mut items);
        assert_eq!(
            report.rewrites, 0,
            "a copy of forty arms does not fit the growth budget: {report:?}"
        );
    }

    /// The statements of `name`'s body, as one expression to walk.
    fn body_of(items: &[TopLevel], name: &str) -> Spanned<Expr> {
        let fd = items
            .iter()
            .find_map(|item| match item {
                TopLevel::FnDef(fd) if fd.name == name => Some(fd),
                _ => None,
            })
            .unwrap_or_else(|| panic!("{name} is present"));
        let FnBody::Block(stmts) = fd.body.as_ref();
        let exprs = stmts
            .iter()
            .map(|stmt| match stmt {
                Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr.clone(),
            })
            .collect();
        Spanned::bare(Expr::List(Shared::new(exprs)))
    }

    /// The first arm below `body` whose pattern is the constructor `ctor`:
    /// its binders and its body.
    fn arm_of(body: &Spanned<Expr>, ctor: &str) -> (Vec<String>, Spanned<Expr>) {
        let mut found = None;
        walk(body, &mut |e| {
            if let Expr::Match { arms, .. } = &e.node {
                for arm in arms.iter() {
                    if let Pattern::Constructor(name, binders) = &arm.pattern
                        && ctor_key(name) == ctor_key(ctor)
                        && found.is_none()
                    {
                        found = Some((binders.clone(), (*arm.body).clone()));
                    }
                }
            }
        });
        found.unwrap_or_else(|| panic!("an arm for {ctor}"))
    }

    /// Every caller spells a variable like a binder of the body its
    /// argument is put into; the fixture
    /// `tests/fixtures/rust_deforest_capture.av` runs the same cases.
    const CAPTURE: &str = r#"module Capture
    intent = "A caller's variable spelled like a binder of the body it is put into."
    effects []

type Node
    Empty
    Leaf(Int)

type Line
    App(Int, Int)
    Blank

record Point
    x: Int
    y: Int

type Pair
    Both(Int, Point)
    Neither

fn some(n: Int) -> Option<Int>
    match n > 1000000
        true -> some(n - 1)
        false -> Option.Some(n)

fn pick(m: Node, y: Int) -> Int
    match m
        Node.Empty -> y
        Node.Leaf(k) -> match k > 0
            true -> match some(k)
                Option.Some(z) -> z + y
                Option.None -> y
            false -> y

fn nested(z: Int) -> Int
    pick(Node.Leaf(z + 1), z)

fn own(m: Node, y: Int) -> Int
    match m
        Node.Empty -> y
        Node.Leaf(z) -> z + y

fn ownArm(z: Int) -> Int
    own(Node.Leaf(1), z)

fn mk(a: Int, p: Point) -> Line
    match p.y > 0
        true -> Line.App(a, p.x)
        false -> Line.Blank

fn producerParam(p: Point) -> Int
    match mk(p.x, Point(x = 5, y = 6))
        Line.App(u, v) -> u + v
        Line.Blank -> 0

fn sum(pr: Pair) -> Int
    match pr
        Pair.Both(a, p) -> a + p.x
        Pair.Neither -> 0

fn recordBinder(p: Point) -> Int
    sum(Pair.Both(p.x, Point(x = 5, y = 0)))

fn wrap(n: Int) -> Line
    match some(n)
        Option.Some(z) -> Line.App(z, n)
        Option.None -> Line.Blank

fn producerBinder(z: Int) -> Int
    match wrap(z)
        Line.App(u, v) -> u * 10 + v
        Line.Blank -> 0

fn bump(n: Int) -> Line
    k = n + 1
    Line.App(k, n)

fn producerBinding(k: Int) -> Int
    match bump(k)
        Line.App(u, v) -> u * 10 + v
        Line.Blank -> 0
"#;

    fn fused_capture() -> Vec<TopLevel> {
        let mut items = prepared(CAPTURE);
        let report = run(&mut items);
        assert_eq!(report.rewrites, 6, "every site is fused: {report:?}");
        items
    }

    /// `pick(Node.Leaf(z + 1), z)`: the consumer's `y` becomes the caller's
    /// `z`, and the `z` an arm binds below `Node.Leaf(k)` must not take it.
    #[test]
    fn an_argument_spelled_like_a_binder_nested_in_the_arm_is_not_captured() {
        let items = fused_capture();
        let (binders, body) = arm_of(&body_of(&items, "nested"), "Option.Some");
        let shown = rendered(&items, "nested");
        assert_ne!(binders[0], "z", "the arm's binder is fresh: {shown}");
        assert_eq!(count_ident(&body, &binders[0]), 1, "{shown}");
        assert_eq!(
            count_ident(&body, "z"),
            1,
            "the arm reads the caller's z: {shown}"
        );
    }

    /// `own(Node.Leaf(1), z)` against `Node.Leaf(z) -> z + y`: the arm's
    /// own binder is spelled like the argument put in for `y`.
    #[test]
    fn an_argument_spelled_like_the_arms_own_binder_is_not_captured() {
        let items = fused_capture();
        let body = body_of(&items, "ownArm");
        let shown = rendered(&items, "ownArm");
        assert_eq!(count_ident(&body, "z"), 1, "{shown}");
        assert!(shown.contains("1 + z"), "{shown}");
    }

    /// `mk(p.x, Point(x = 5, y = 6))`: the producer's `a` becomes the
    /// caller's `p.x`, and the producer's `p`, read by field, becomes the
    /// record's fields; the caller's `p.x` must not be taken for the
    /// producer's.
    #[test]
    fn an_argument_spelled_like_a_producer_parameter_read_by_field_is_not_captured() {
        let items = fused_capture();
        let body = body_of(&items, "producerParam");
        let shown = rendered(&items, "producerParam");
        assert_eq!(count_field_read(&body, "p", "x"), 1, "{shown}");
        assert!(shown.contains("p.x + 5"), "{shown}");
    }

    /// `sum(Pair.Both(p.x, Point(x = 5, y = 0)))` against
    /// `Pair.Both(a, p) -> a + p.x`: the arm's record binder is spelled
    /// like the caller's record.
    #[test]
    fn an_argument_spelled_like_an_arms_record_binder_is_not_captured() {
        let items = fused_capture();
        let body = body_of(&items, "recordBinder");
        let shown = rendered(&items, "recordBinder");
        assert_eq!(count_field_read(&body, "p", "x"), 1, "{shown}");
        assert!(shown.contains("p.x + 5"), "{shown}");
    }

    /// `wrap(z)` against a producer that binds `Option.Some(z)` of its
    /// own, and `bump(k)` against a producer with a binding `k`.
    #[test]
    fn an_argument_spelled_like_a_producers_binder_or_binding_is_not_captured() {
        let items = fused_capture();
        let (binders, body) = arm_of(&body_of(&items, "producerBinder"), "Option.Some");
        let shown = rendered(&items, "producerBinder");
        assert_ne!(binders[0], "z", "{shown}");
        assert_eq!(count_ident(&body, &binders[0]), 1, "{shown}");
        assert_eq!(count_ident(&body, "z"), 1, "{shown}");
        let body = body_of(&items, "producerBinding");
        let shown = rendered(&items, "producerBinding");
        assert_eq!(
            count_ident(&body, "k"),
            2,
            "the binding's value and the arm both read the caller's k: {shown}"
        );
        assert_eq!(count_ident(&body, "n"), 0, "{shown}");
    }

    #[test]
    fn a_lazy_call_and_an_effectful_producer_stay_calls() {
        let mut items = prepared(
            r#"module Lazy
    intent = "Deferred calls and effects are left alone."
    effects [Console.print]

type Line
    App(Int)
    Nest(Line)
    Blank

fn parse(s: String) -> Line
    Line.App(String.len(s))

fn loud(s: String) -> Line
    ! [Console.print]
    Console.print(s)
    Line.App(1)

fn apply(l: Line) -> Int
    match l
        Line.App(n) -> n
        Line.Nest(_) -> 1
        Line.Blank -> 0

fn deferred(s: String) -> Int
    apply(lazy parse(s))

fn effectful(s: String) -> Int
    ! [Console.print]
    apply(loud(s))
"#,
        );
        let report = run(&mut items);
        assert_eq!(report.rewrites, 0, "{report:?}");
    }
}
