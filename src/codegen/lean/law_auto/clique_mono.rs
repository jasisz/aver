//! Clique-propagated position-monotonicity laws.
//!
//! A user law of the shape
//!
//! ```text
//! verify parseValue law parseValueAdvances
//!     given s: String = [...]
//!     given pos: Int = [...]
//!     given v: Json = [...]
//!     given p: Int = [...]
//!     when parseValue(s, pos) == ParseResult.Ok(v, p)
//!     p >= pos => true
//! ```
//!
//! states that a fuelized member of a mutually-recursive string-position parser
//! clique never moves the cursor backwards. The recognizer is keyed ONLY on the
//! AST shape (a `when`-equality of a clique member call against an `ok(v, p)`
//! constructor, plus a `p >= pos => true` claim comparing the bound position
//! component against the call's position argument). The content — which argument
//! is the position, which constructor component is the returned position, the
//! relation — is read off the law and PROPAGATED structurally to every other
//! member of the same SCC. No function or type name is ever matched.
//!
//! The emitter proves ONE conjunction over the whole clique in Prop form by
//! ordinary `induction fuel` with rank-slotted thresholds (`T s pos rank = (len
//! - pos.toNat) * R + rank + 1`), then projects the user-named member's conjunct
//! back through the fuel wrapper (bridging the law's `== / >=` Bool surface via
//! constructor injection on the `Int` position field and `omega`). Ranks are a
//! longest-path assignment over the subgraph of same-position / whitespace-weak
//! / result-carried (semantic) edges; a cycle there makes the emitter decline
//! (fail-closed), so the law falls back to its bounded-domain proof rather than
//! a silently weaker universal.
//!
//! This leg discharges only SELF-CONTAINED cliques: every cursor-feeding callee
//! must be a clique sibling. A clique whose cursor cone reaches a non-clique
//! scanner (`parseString`, `parseNumber`, `skipWs`, …) needs a universal
//! monotonicity fact this leg does not supply, so the emitter declines (the JSON
//! `parseValue` clique lands here) and the law keeps its bounded proof. The
//! proof body is wrapped `first | (…) | sorry`, so a slipped rung is denied
//! universal credit by the `#print axioms` gate rather than emitted weaker.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Expr, FnDef, Literal, Pattern, Spanned, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::codegen::common::is_pure_fn;

use super::super::expr::aver_name_to_lean;
use super::super::types::type_annotation_to_lean;
use super::AutoProof;

// ===========================================================================
// Position-argument classification for a clique call edge.
// ===========================================================================

#[derive(Clone, Debug, PartialEq)]
enum PosClass {
    /// The callee position is the caller's `pos`, verbatim.
    Same,
    /// `G s pos` for a monotone-cursor helper `G` (whitespace-skip shape) —
    /// non-decreasing, treated like same-position for ranking. Carries `G`'s
    /// source name so emission renders and cites the actual helper, never a
    /// hardcoded name.
    Weak(String),
    /// `pos + 1` or `G s (pos + 1)` — a constant advance guarded by a
    /// char-bounds fact, so `remaining` strictly drops (rank-free). `skipws`
    /// carries the monotone-cursor helper's source name when present.
    AdvConst { skipws: Option<String> },
    /// `p'` or `G s p'`, where `p'` is a position returned by the outer
    /// scrutinee sub-call — bounded via that sub-call's own monotonicity.
    Semantic {
        skipws: Option<String>,
        binder: String,
    },
}

/// The Lean render of a callee position expression, given the classification
/// and the caller's `s` name. Monotone-cursor helpers render under their own
/// (name-blind) lean name.
fn pos_render(class: &PosClass, s: &str, pos: &str) -> String {
    let g = |g: &str| aver_name_to_lean(g);
    match class {
        PosClass::Same => pos.to_string(),
        PosClass::Weak(h) => format!("({} {s} {pos})", g(h)),
        PosClass::AdvConst { skipws: None } => format!("({pos} + 1)"),
        PosClass::AdvConst { skipws: Some(h) } => format!("({} {s} ({pos} + 1))", g(h)),
        PosClass::Semantic {
            skipws: None,
            binder,
        } => binder.clone(),
        PosClass::Semantic {
            skipws: Some(h),
            binder,
        } => format!("({} {s} {binder})", g(h)),
    }
}

/// Recognize the position expression passed to a callee's cursor slot.
/// `pos` is the caller's position parameter name; `result_binders` are the
/// names bound by an enclosing result-scrutinee match (their positions are
/// "semantic"). `weak_helpers` is the set of source fn names that have a
/// universal-shape monotone-cursor pool law (`G(s, q) >= q`) — a helper is
/// "whitespace-skip-like" ONLY because such a citable law exists about it,
/// never because of its name (litmus: content-blind, shape-keyed).
fn classify_pos(
    expr: &Spanned<Expr>,
    _s: &str,
    pos: &str,
    result_binders: &HashSet<String>,
    weak_helpers: &HashSet<String>,
) -> Option<PosClass> {
    if let Some(name) = ident_name(expr) {
        if name == pos {
            return Some(PosClass::Same);
        }
        if result_binders.contains(name) {
            return Some(PosClass::Semantic {
                skipws: None,
                binder: name.to_string(),
            });
        }
        return None;
    }
    match &expr.node {
        // pos + 1
        Expr::BinOp(BinOp::Add, l, r) if is_ident(l, pos) && is_int_lit(r, 1) => {
            Some(PosClass::AdvConst { skipws: None })
        }
        // G(s, X) for a monotone-cursor helper G (pool-law-keyed, not name-keyed)
        Expr::FnCall(callee, args) if args.len() == 2 => {
            let g = super::shared::expr_dotted_name(callee)?;
            if !weak_helpers.contains(&g) {
                return None;
            }
            if let Some(name) = ident_name(&args[1]) {
                if name == pos {
                    return Some(PosClass::Weak(g));
                }
                if result_binders.contains(name) {
                    return Some(PosClass::Semantic {
                        skipws: Some(g),
                        binder: name.to_string(),
                    });
                }
                return None;
            }
            match &args[1].node {
                Expr::BinOp(BinOp::Add, l, r) if is_ident(l, pos) && is_int_lit(r, 1) => {
                    Some(PosClass::AdvConst { skipws: Some(g) })
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Recognize a constructor application, whether it parses as `Expr::Constructor`
/// or (the common case for arg-carrying variants) as `FnCall(Type.Variant, …)`.
/// Constructors are `Type.<Uppercase>`; methods are `Type.<lowercase>`.
fn ctor_app(expr: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
    match &expr.node {
        Expr::Constructor(name, payload) => {
            let args = match payload.as_ref().map(|b| &b.node) {
                Some(Expr::Tuple(items)) => items.iter().collect(),
                Some(_) => vec![payload.as_ref().unwrap().as_ref()],
                None => vec![],
            };
            Some((name.clone(), args))
        }
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(base, variant) = &callee.node
                && ident_name(base).is_some()
                && variant.chars().next().is_some_and(|c| c.is_uppercase())
            {
                let name = super::shared::expr_dotted_name(callee)?;
                Some((name, args.iter().collect()))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// The identifier name of an expression, accepting both the source `Ident`
/// shape (laws) and the resolved-HIR `Resolved` shape (fn bodies at proof-emit).
fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) => Some(n.as_str()),
        Expr::Resolved { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

fn is_ident(e: &Spanned<Expr>, name: &str) -> bool {
    ident_name(e) == Some(name)
}

fn is_int_lit(e: &Spanned<Expr>, v: i64) -> bool {
    matches!(&e.node, Expr::Literal(Literal::Int(n)) if *n == v)
}

/// Lower a source constructor name (`Type.Variant`) to its Lean form
/// (`Type.variant`) — the variant's first letter is lower-cased, matching the
/// datatype emit.
fn ctor_to_lean(name: &str) -> String {
    fn lower_first(s: &str) -> String {
        let mut c = s.chars();
        match c.next() {
            Some(f) => f.to_lowercase().collect::<String>() + c.as_str(),
            None => String::new(),
        }
    }
    match name.rsplit_once('.') {
        Some((ty, variant)) => format!("{ty}.{}", lower_first(variant)),
        None => lower_first(name),
    }
}

// ===========================================================================
// Per-member structural analysis.
// ===========================================================================

/// A single leaf of a member body: what the arm evaluates to.
#[derive(Clone, Debug)]
enum Leaf {
    /// `ParseResult.Err(...)` — impossible against `ok v p`.
    Err,
    /// `ParseResult.Ok(_, POSEXPR)` — terminal, position is `pos` or `pos + 1`.
    OkTerminal,
    /// A call to a clique sibling at the given classified position.
    Sibling { name: String, pos: PosClass },
    /// A call to a non-clique callee at the given classified position.
    External { name: String, pos: PosClass },
}

/// How a member dispatches on its scrutinee.
#[derive(Clone, Debug)]
enum Member {
    /// `match String.charAt(s, pos)`: `None` → err, `Some(c)` → either a direct
    /// call or a nested `match c` with literal arms.
    CharAt {
        /// The `Some(c)` branch: either a single direct call or the leaves of a
        /// nested char-literal match.
        some: CharSome,
    },
    /// `match c` where `c` is a parameter (dispatch helper). Needs a char-link
    /// premise. Arms are char-literal leaves.
    DispatchParam { leaves: Vec<Leaf> },
    /// `match F(s, pos)`: err passthrough + `Ok(val, p')` → single sibling edge.
    ResultMatch {
        scrut: String,
        scrut_is_sibling: bool,
        edge: Leaf,
        /// True when the `.Err` arm precedes the `.Ok` arm in source.
        err_first: bool,
    },
    /// `match adtVar`: one constructor arm → sibling edge, wildcard → err.
    AdtMatch {
        edge: Leaf,
        /// True when the constructor arm precedes the wildcard in source.
        ctor_first: bool,
    },
}

#[derive(Clone, Debug)]
enum CharSome {
    Direct(Leaf),
    Nested(Vec<Leaf>),
}

/// A pool-law citation that discharges one of the conjunction's external
/// hypotheses. `hyp` is the fresh hypothesis name the conjunction theorem binds;
/// the projection reconstructs the hypothesis by APPLYING `theorem` (the cited
/// pool law) and bridging its `==` / `>=` Bool surface to the Prop shape. When
/// the cited law is only bounded, `theorem` carries the sampled-domain premises
/// in its TYPE, so the reconstruction fails to typecheck and the projection's
/// `first | … | sorry` floor demotes the law to bounded — fail-closed, no tier
/// oracle needed.
#[derive(Clone, Debug)]
struct Citation {
    /// Fresh hypothesis name (`hcite0`, …).
    hyp: String,
    /// Source name of the external fn this citation is about (emit lookup key).
    src_fn: String,
    kind: CiteKind,
}

#[derive(Clone, Debug)]
enum CiteKind {
    /// A sub-parser `F` whose returned position bounds a cursor edge. Hypothesis
    /// shape `∀ <binders>, F <callargs> = <ok> v p → pos ≤ p`.
    Parser {
        /// `∀`-binder list, e.g. `(s : String) (pos : Int) (e : String) …`.
        binders: String,
        /// Binder names in law-given order (passed to the cited theorem).
        arg_names: Vec<String>,
        /// The `F <callargs>` lean call.
        call: String,
        /// Lean value type and ok-ctor for the RHS `ok v p`.
        v_name: String,
        p_name: String,
        pos_name: String,
        ok_ctor: String,
        /// Number of extra call arguments after `s`, `pos` (rendered as `_` when
        /// the hypothesis is applied in a member block).
        extras_count: usize,
        /// The cited pool-law theorem base.
        theorem: String,
    },
    /// A monotone-cursor helper `G`. Hypothesis shape `∀ (s:String) (q:Int),
    /// q ≤ G s q`.
    Weak { g_lean: String, theorem: String },
}

struct MemberInfo<'a> {
    fd: &'a FnDef,
    lean: String,
    s_name: String,
    pos_name: String,
    /// Extra parameter (name, lean-type) pairs after `s`, `pos`.
    extras: Vec<(String, String)>,
    shape: Member,
    /// The set of clique out-edges (callee name + position class), for ranking.
    edges: Vec<(String, PosClass)>,
    /// True when this member dispatches on a char parameter and needs the
    /// `charAtAv s pos = some c` premise.
    dispatch_param: Option<String>,
    rank: usize,
}

/// The whole recognized clique shape.
struct CliqueShape<'a> {
    members: Vec<MemberInfo<'a>>,
    /// member name → index into `members`.
    index_of: HashMap<String, usize>,
    rep: String,
    r: usize,
    /// The value component's lean type (`Json`).
    val_type: String,
    /// The lean success constructor (`ParseResult.ok`).
    ok_ctor: String,
    /// Every constructor of the result type as `(lean_variant, arity)`, so the
    /// projection can enumerate the `cases` arms (success + all failure ones).
    variants: Vec<(String, usize)>,
    /// Pool-law citations discharging the conjunction's external hypotheses, in
    /// deterministic order (conjunction binder order).
    citations: Vec<Citation>,
    /// External source fn name → hypothesis name, for the member-block emit.
    hyp_of: HashMap<String, String>,
}

impl CliqueShape<'_> {
    /// The `(hypothesis name, extra-arg count)` of the parser citation about
    /// `fn_name`, if one was resolved.
    fn parser_cite(&self, fn_name: &str) -> Option<(&str, usize)> {
        self.citations.iter().find_map(|c| match &c.kind {
            CiteKind::Parser { extras_count, .. } if c.src_fn == fn_name => {
                Some((c.hyp.as_str(), *extras_count))
            }
            _ => None,
        })
    }
}

fn member_body_match(fd: &FnDef) -> Option<(&Spanned<Expr>, &Vec<crate::ast::MatchArm>)> {
    let tail = fd.body.tail_expr()?;
    if let Expr::Match { subject, arms } = &tail.node {
        Some((subject, arms))
    } else {
        None
    }
}

/// Peel a call `F(s, posexpr, ...)` into `(callee_name, posexpr)`. Handles both
/// ordinary `FnCall`s and the post-TCO `TailCall` form mutual siblings become.
fn as_call(expr: &Spanned<Expr>) -> Option<(String, &Spanned<Expr>)> {
    match &expr.node {
        Expr::FnCall(callee, args) if args.len() >= 2 => {
            Some((super::shared::expr_dotted_name(callee)?, &args[1]))
        }
        Expr::TailCall(tc) if tc.args.len() >= 2 => Some((tc.target.clone(), &tc.args[1])),
        _ => None,
    }
}

/// Classify one arm body into a `Leaf`.
fn classify_leaf(
    body: &Spanned<Expr>,
    s: &str,
    pos: &str,
    members: &HashSet<String>,
    result_binders: &HashSet<String>,
    ok_ctor: &str,
    weak_helpers: &HashSet<String>,
) -> Option<Leaf> {
    if let Some((name, args)) = ctor_app(body) {
        if name == ok_ctor {
            // Position (2nd component) must be `pos` or `pos + 1`.
            let ok = args.len() == 2
                && (is_ident(args[1], pos)
                    || matches!(&args[1].node, Expr::BinOp(BinOp::Add, l, r) if is_ident(l, pos) && is_int_lit(r, 1)));
            return if ok { Some(Leaf::OkTerminal) } else { None };
        }
        // Any other constructor (a failure variant) is an impossible leaf
        // against the success constructor `ok v p`.
        return Some(Leaf::Err);
    }
    let (name, posexpr) = as_call(body)?;
    let class = classify_pos(posexpr, s, pos, result_binders, weak_helpers)?;
    if members.contains(&name) {
        Some(Leaf::Sibling { name, pos: class })
    } else {
        Some(Leaf::External { name, pos: class })
    }
}

/// The result of analysing a member: its dispatch shape, its clique out-edges
/// (`callee, position-class`), and the char-dispatch parameter name if any.
type MemberAnalysis = (Member, Vec<(String, PosClass)>, Option<String>);

/// Analyse one clique member's body into a `Member` shape. Declines (`None`)
/// on any structure outside the supported vocabulary — fail-closed.
fn analyse_member(
    fd: &FnDef,
    members: &HashSet<String>,
    ok_ctor: &str,
    weak_helpers: &HashSet<String>,
) -> Option<MemberAnalysis> {
    let s = fd.params.first()?.0.clone();
    let pos = fd.params.get(1)?.0.clone();
    let (subject, arms) = member_body_match(fd)?;
    let empty = HashSet::new();
    let mut edges: Vec<(String, PosClass)> = Vec::new();

    let record = |leaf: &Leaf, edges: &mut Vec<(String, PosClass)>| {
        if let Leaf::Sibling { name, pos } = leaf {
            edges.push((name.clone(), pos.clone()));
        }
    };

    // (1) `match String.charAt(s, pos)`
    if let Some((callee, cargs)) = call_parts(subject)
        && callee == "String.charAt"
        && cargs.len() == 2
        && is_ident(&cargs[0], &s)
        && is_ident(&cargs[1], &pos)
    {
        // arms[0] = None → err ; arms[1] = Some(c) → inner
        if arms.len() != 2 {
            return None;
        }
        let some_arm = &arms[1].body;
        let some = match &some_arm.node {
            Expr::Match {
                subject: inner_subj,
                arms: inner_arms,
            } if ident_name(inner_subj).is_some() => {
                // nested `match c`
                let mut leaves = Vec::new();
                for a in inner_arms {
                    let leaf =
                        classify_leaf(&a.body, &s, &pos, members, &empty, ok_ctor, weak_helpers)?;
                    record(&leaf, &mut edges);
                    leaves.push(leaf);
                }
                CharSome::Nested(leaves)
            }
            _ => {
                let leaf =
                    classify_leaf(some_arm, &s, &pos, members, &empty, ok_ctor, weak_helpers)?;
                record(&leaf, &mut edges);
                CharSome::Direct(leaf)
            }
        };
        return Some((Member::CharAt { some }, edges, None));
    }

    // (2) `match c` where c is a parameter (dispatch helper).
    if let Some(cname) = ident_name(subject)
        && fd.params.iter().any(|(n, _)| n == cname)
        && arms
            .iter()
            .all(|a| matches!(a.pattern, Pattern::Literal(_) | Pattern::Wildcard))
    {
        let cname = cname.to_string();
        let mut leaves = Vec::new();
        for a in arms {
            let leaf = classify_leaf(&a.body, &s, &pos, members, &empty, ok_ctor, weak_helpers)?;
            record(&leaf, &mut edges);
            leaves.push(leaf);
        }
        return Some((Member::DispatchParam { leaves }, edges, Some(cname.clone())));
    }

    // (3) `match F(s, pos)` — result scrutinee.
    if let Some((scrut, sargs)) = call_parts(subject)
        && sargs.len() >= 2
        && is_ident(&sargs[0], &s)
        && is_ident(&sargs[1], &pos)
        && arms.len() == 2
    {
        // Ok arm binds (val, p'); its body is the single edge. The returned
        // position binder is "semantic" inside the ok-arm body.
        let (ok_idx, err_idx) = ok_err_arm_indices(arms, ok_ctor)?;
        let ok_arm = &arms[ok_idx];
        let binders = ctor_binders(&ok_arm.pattern)?;
        if binders.len() != 2 {
            return None;
        }
        let mut rb = HashSet::new();
        rb.insert(binders[1].clone());
        let edge = classify_leaf(&ok_arm.body, &s, &pos, members, &rb, ok_ctor, weak_helpers)?;
        record(&edge, &mut edges);
        let scrut_is_sibling = members.contains(&scrut);
        // The scrutinee sub-call is a same-position sibling edge: its returned
        // position bounds the semantic edge, and it must outrank this member.
        if scrut_is_sibling {
            edges.push((scrut.clone(), PosClass::Same));
        }
        return Some((
            Member::ResultMatch {
                scrut,
                scrut_is_sibling,
                edge,
                err_first: err_idx < ok_idx,
            },
            edges,
            None,
        ));
    }

    // (4) `match adtVar` — one constructor arm + wildcard.
    if let Some(varname) = ident_name(subject)
        && fd.params.iter().any(|(n, _)| n == varname)
        && arms.len() == 2
    {
        let (ctor_idx, wild_idx) = ctor_wild_arm_indices(arms)?;
        let ctor_arm = &arms[ctor_idx];
        // The constructor payload binder (e.g. `key`) is not a returned position,
        // so no binder is treated as semantic here.
        let edge = classify_leaf(
            &ctor_arm.body,
            &s,
            &pos,
            members,
            &empty,
            ok_ctor,
            weak_helpers,
        )?;
        record(&edge, &mut edges);
        return Some((
            Member::AdtMatch {
                edge,
                ctor_first: ctor_idx < wild_idx,
            },
            edges,
            None,
        ));
    }

    None
}

fn call_parts(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            Some((super::shared::expr_dotted_name(callee)?, args.as_slice()))
        }
        Expr::TailCall(tc) => Some((tc.target.clone(), tc.args.as_slice())),
        _ => None,
    }
}

fn ctor_binders(pat: &Pattern) -> Option<Vec<String>> {
    match pat {
        Pattern::Constructor(_, binders) => Some(binders.clone()),
        _ => None,
    }
}

/// Given two arms, return `(ok_index, err_index)`: the arm whose constructor is
/// the success constructor `ok_ctor`, and the other constructor arm.
fn ok_err_arm_indices(arms: &[crate::ast::MatchArm], ok_ctor: &str) -> Option<(usize, usize)> {
    let mut ok = None;
    let mut err = None;
    for (i, a) in arms.iter().enumerate() {
        if let Pattern::Constructor(name, _) = &a.pattern {
            if name == ok_ctor {
                ok = Some(i);
            } else {
                err = Some(i);
            }
        }
    }
    Some((ok?, err?))
}

/// Given two arms, return `(constructor_index, wildcard_index)`.
fn ctor_wild_arm_indices(arms: &[crate::ast::MatchArm]) -> Option<(usize, usize)> {
    let mut ctor = None;
    let mut wild = None;
    for (i, a) in arms.iter().enumerate() {
        match &a.pattern {
            Pattern::Constructor(_, _) => ctor = Some(i),
            Pattern::Wildcard => wild = Some(i),
            _ => {}
        }
    }
    Some((ctor?, wild?))
}

// ===========================================================================
// Rank assignment (longest path over same/weak/semantic edges).
// ===========================================================================

/// Longest-path rank over the subgraph of edges that require `rank(callee) <
/// rank(caller)` (same-position, whitespace-weak, and result-carried semantic
/// edges; constant advances are rank-free). Returns `None` on a cycle.
fn assign_ranks(members: &[MemberInfo]) -> Option<Vec<usize>> {
    let n = members.len();
    let idx: HashMap<&str, usize> = members
        .iter()
        .enumerate()
        .map(|(i, m)| (m.fd.name.as_str(), i))
        .collect();
    // rank-DAG edge iff the callee must be strictly lower-ranked (same-position,
    // whitespace-weak, and result-carried semantic edges; constant advances are
    // rank-free because their `remaining` strictly drops).
    let mut edges: Vec<(usize, usize)> = Vec::new();
    for (i, m) in members.iter().enumerate() {
        for (callee, class) in &m.edges {
            let must_drop = matches!(
                class,
                PosClass::Same | PosClass::Weak(_) | PosClass::Semantic { .. }
            );
            if must_drop && let Some(&j) = idx.get(callee.as_str()) {
                edges.push((i, j));
            }
        }
    }
    rank_dag(n, &edges)
}

/// Longest-path ranks over a DAG on `0..n` with `caller -> callee` edges: each
/// node outranks all its callees. Returns `None` on a cycle. Pure and
/// self-contained so it is unit-testable without building a whole clique.
fn rank_dag(n: usize, edges: &[(usize, usize)]) -> Option<Vec<usize>> {
    let mut adj: Vec<Vec<usize>> = vec![Vec::new(); n];
    for &(u, v) in edges {
        adj[u].push(v);
    }
    let mut rank = vec![usize::MAX; n];
    let mut on_stack = vec![false; n];
    fn dfs(
        u: usize,
        adj: &[Vec<usize>],
        rank: &mut [usize],
        on_stack: &mut [bool],
    ) -> Option<usize> {
        if on_stack[u] {
            return None; // cycle
        }
        if rank[u] != usize::MAX {
            return Some(rank[u]);
        }
        on_stack[u] = true;
        let mut best = 0usize;
        for &v in &adj[u] {
            let rv = dfs(v, adj, rank, on_stack)?;
            best = best.max(rv + 1);
        }
        on_stack[u] = false;
        rank[u] = best;
        Some(best)
    }
    for u in 0..n {
        dfs(u, &adj, &mut rank, &mut on_stack)?;
    }
    Some(rank)
}

// ===========================================================================
// Recognition entry.
// ===========================================================================

/// The value / position binding names and the target member, read off the law.
struct LawShape {
    target: String,
    ok_ctor: String,
    val_name: String,
    val_type: String,
    p_name: String,
    pos_name: String,
}

fn read_law_shape(law: &VerifyLaw) -> Option<LawShape> {
    // when: `F(s, pos) == Ctor.Ok(v, p)`
    let when = law.when.as_ref()?;
    let Expr::BinOp(BinOp::Eq, call, ctor) = &when.node else {
        return None;
    };
    let (target, cargs) = call_parts(call)?;
    if cargs.len() < 2 {
        return None;
    }
    let pos_name = match &cargs[1].node {
        Expr::Ident(n) => n.clone(),
        _ => return None,
    };
    let (ok_ctor, cargs2) = ctor_app(ctor)?;
    if cargs2.len() != 2 {
        return None;
    }
    let (val_name, p_name) = match (&cargs2[0].node, &cargs2[1].node) {
        (Expr::Ident(vn), Expr::Ident(pn)) => (vn.clone(), pn.clone()),
        _ => return None,
    };
    // claim: `p >= pos => true`
    let Expr::BinOp(BinOp::Gte, cl, cr) = &law.lhs.node else {
        return None;
    };
    if !is_ident(cl, &p_name) || !is_ident(cr, &pos_name) {
        return None;
    }
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    // value type resolved later from the law givens.
    Some(LawShape {
        target,
        ok_ctor,
        val_name,
        val_type: String::new(),
        p_name,
        pos_name,
    })
}

/// Gather the pure-fn SCC (entry scope) that owns `target`, if it is a
/// multi-member mutual clique.
fn owning_scc<'a>(target: &str, ctx: &'a CodegenContext) -> Option<Vec<&'a FnDef>> {
    let pure: Vec<&FnDef> = ctx.fn_defs.iter().filter(|fd| is_pure_fn(fd)).collect();
    if !pure.iter().any(|fd| fd.name == target) {
        return None;
    }
    let comps = crate::call_graph::ordered_fn_components(&pure, &ctx.module_prefixes);
    let comp = comps
        .into_iter()
        .find(|c| c.iter().any(|fd| fd.name == target))?;
    if comp.len() < 2 {
        return None;
    }
    Some(comp)
}

/// A monotone-cursor (`G(s,q) >= q`) pool law, usable to discharge a `Weak`
/// hypothesis. Keyed by `G`'s source name.
struct WeakRaw {
    theorem: String,
    line: usize,
    /// True iff `G`'s `>= q` law actually closes UNIVERSALLY — `G` is a native
    /// string-position scanner that graduates (the `fun_induction … simp_all …
    /// omega` closer). A bounded `>= q` law is NOT citable.
    universal: bool,
}

/// A sub-parser advance (`when F(...) == ok(v,p) => p >= pos`) pool law, usable
/// to discharge a `Parser` hypothesis. Keyed by `F`'s source name. Carries the
/// law's own `(vb, law)` so its UNIVERSALITY can be re-checked recursively (a
/// bounded sub-parser law is not citable).
struct ParserRaw<'a> {
    theorem: String,
    binders: String,
    arg_names: Vec<String>,
    call: String,
    v_name: String,
    p_name: String,
    pos_name: String,
    ok_ctor: String,
    extras_count: usize,
    line: usize,
    vb: &'a VerifyBlock,
    law: &'a VerifyLaw,
}

/// Recognize a monotone-cursor pool law (`holds`, claim `G(s,q) >= q`). The
/// helper is identified purely by this SHAPE — a citable `>=` law about it —
/// never by name (the litmus).
fn weak_law_raw(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(String, WeakRaw)> {
    if law.when.is_some() {
        return None;
    }
    let Expr::BinOp(BinOp::Gte, call, r) = &law.lhs.node else {
        return None;
    };
    let (g, cargs) = call_parts(call)?;
    if cargs.len() != 2 {
        return None;
    }
    let q = ident_name(&cargs[1])?;
    if ident_name(r) != Some(q) {
        return None;
    }
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    if law.givens.len() != 2 {
        return None;
    }
    let theorem = crate::codegen::lean::toplevel::law_theorem_base(vb, law, ctx);
    // Universal iff `G` is a graduating string-pos scanner (its `>= q` law then
    // closes by `fun_induction`; otherwise the law is bounded and not citable).
    let universal = ctx
        .fn_defs
        .iter()
        .find(|fd| fd.name == g)
        .is_some_and(|fd| {
            crate::codegen::lean::toplevel::fuel::detect_simple_string_pos_skip_literal(fd)
                .is_some()
        });
    Some((
        g,
        WeakRaw {
            theorem,
            line: vb.line,
            universal,
        },
    ))
}

/// Recognize a sub-parser advance pool law (`when F(...) == ok(v,p) => p >= pos`)
/// — the SAME claim shape the clique rung propagates, one level down. Keyed on
/// `F`'s source name.
fn parser_law_raw<'a>(
    vb: &'a VerifyBlock,
    law: &'a VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(String, ParserRaw<'a>)> {
    let ls = read_law_shape(law)?;
    // The `F(callargs)` argument idents (source names).
    let when = law.when.as_ref()?;
    let Expr::BinOp(BinOp::Eq, call, _) = &when.node else {
        return None;
    };
    let call_args: Vec<String> = match &call.node {
        Expr::FnCall(_, args) => args
            .iter()
            .filter_map(|a| ident_name(a).map(str::to_string))
            .collect(),
        Expr::TailCall(tc) => tc
            .args
            .iter()
            .filter_map(|a| ident_name(a).map(str::to_string))
            .collect(),
        _ => return None,
    };
    if call_args.len() < 2 {
        return None;
    }
    let call = format!(
        "{} {}",
        aver_name_to_lean(&ls.target),
        call_args
            .iter()
            .map(|n| aver_name_to_lean(n))
            .collect::<Vec<_>>()
            .join(" ")
    );
    let mut binders = String::new();
    let mut arg_names = Vec::new();
    for g in &law.givens {
        let name = aver_name_to_lean(&g.name);
        let ty = type_annotation_to_lean(&g.type_name);
        binders.push_str(&format!("({name} : {ty}) "));
        arg_names.push(name);
    }
    let theorem = crate::codegen::lean::toplevel::law_theorem_base(vb, law, ctx);
    Some((
        ls.target.clone(),
        ParserRaw {
            theorem,
            binders: binders.trim_end().to_string(),
            arg_names,
            call,
            v_name: aver_name_to_lean(&ls.val_name),
            p_name: aver_name_to_lean(&ls.p_name),
            pos_name: aver_name_to_lean(&ls.pos_name),
            ok_ctor: ctor_to_lean(&ls.ok_ctor),
            extras_count: call_args.len() - 2,
            line: vb.line,
            vb,
            law,
        },
    ))
}

/// Scan the ENTRY-scope law pool for monotone-cursor and sub-parser advance
/// laws. Single-file pool (the JSON target and the witness both live in one
/// file); a needed pool law in a dependency module is out of scope here — the
/// #647 dep-closure owns cross-module citation for its own rungs.
/// ponytail: entry-scope only; widen to `ctx.modules` if a cross-file clique
/// citation is ever measured to be needed.
fn collect_pool_laws<'a>(
    ctx: &'a CodegenContext,
) -> (HashMap<String, WeakRaw>, HashMap<String, ParserRaw<'a>>) {
    use crate::ast::{TopLevel, VerifyKind};
    let mut weak = HashMap::new();
    let mut parser = HashMap::new();
    for item in &ctx.items {
        let TopLevel::Verify(vb) = item else { continue };
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        if let Some((g, raw)) = weak_law_raw(vb, law, ctx) {
            weak.entry(g).or_insert(raw);
        }
        if let Some((f, raw)) = parser_law_raw(vb, law, ctx) {
            parser.entry(f).or_insert(raw);
        }
    }
    (weak, parser)
}

/// Recursion budget for the sub-parser universality probe: a cited sub-parser's
/// advance law is citable only if IT closes universally, which the same
/// `build_shape` decides one level down. Depth bounds the (topology-acyclic)
/// nesting; JSON is 2-3 deep. `TOP` marks the outermost call, so the decline
/// diagnostic fires once, not on every probe.
const PROBE_DEPTH_TOP: usize = 8;

fn build_shape<'a>(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &'a CodegenContext,
    depth: usize,
) -> Option<(CliqueShape<'a>, LawShape)> {
    if depth == 0 {
        return None;
    }
    let mut ls = read_law_shape(law)?;
    // value type from the matching given.
    ls.val_type = law
        .givens
        .iter()
        .find(|g| g.name == ls.val_name)
        .map(|g| type_annotation_to_lean(&g.type_name))?;

    let comp = owning_scc(&ls.target, ctx)?;
    let member_names: HashSet<String> = comp.iter().map(|fd| fd.name.clone()).collect();

    // Monotone-cursor helpers and sub-parser advance laws available for citation.
    let (weak_pool, parser_pool) = collect_pool_laws(ctx);
    let weak_helpers: HashSet<String> = weak_pool.keys().cloned().collect();

    let mut members: Vec<MemberInfo> = Vec::new();
    for fd in &comp {
        // every member must have `s : String`, `pos : Int` as first two params.
        let (s_name, s_ty) = fd.params.first()?.clone();
        let (pos_name, pos_ty) = fd.params.get(1)?.clone();
        if type_annotation_to_lean(&s_ty) != "String" || type_annotation_to_lean(&pos_ty) != "Int" {
            return None;
        }
        let extras: Vec<(String, String)> = fd.params[2..]
            .iter()
            .map(|(n, t)| (n.clone(), type_annotation_to_lean(t)))
            .collect();
        let (shape, edges, dispatch_param) =
            analyse_member(fd, &member_names, &ls.ok_ctor, &weak_helpers)?;
        members.push(MemberInfo {
            fd,
            lean: aver_name_to_lean(&fd.name),
            s_name,
            pos_name,
            extras,
            shape,
            edges,
            dispatch_param,
            rank: 0,
        });
    }

    let ranks = assign_ranks(&members)?;
    for (m, r) in members.iter_mut().zip(ranks) {
        m.rank = r;
    }

    let index_of: HashMap<String, usize> = members
        .iter()
        .enumerate()
        .map(|(i, m)| (m.fd.name.clone(), i))
        .collect();

    // Collect distinct external cursor dependencies: sub-parser callees whose
    // returned position feeds a cursor, and monotone-cursor helpers wrapping an
    // edge position.
    let mut parser_ext: Vec<String> = Vec::new();
    let mut weak_ext: Vec<String> = Vec::new();
    for m in &members {
        for kind in edges_and_externals(m) {
            match kind {
                EdgeKind::External(f) => {
                    if !parser_ext.contains(&f) {
                        parser_ext.push(f);
                    }
                }
                EdgeKind::UsesSkipWs(g) => {
                    if !weak_ext.contains(&g) {
                        weak_ext.push(g);
                    }
                }
                EdgeKind::Sibling => {}
            }
        }
    }
    parser_ext.sort();
    weak_ext.sort();

    // Resolve each external to a pool-law citation. The cited law must exist, be
    // declared earlier in source (topology: the cited theorem is emitted before
    // this one, mirroring the #647 emission-order guard), AND actually close
    // UNIVERSALLY — a sub-parser whose own advance law is only bounded, or a
    // whitespace helper that does not graduate, cannot discharge a universal
    // hypothesis, so citing it would only floor to `sorry`. A missing,
    // later-declared, or non-universal law makes the clique DECLINE (fail-closed)
    // with a diagnostic naming exactly what is needed — the law then keeps its
    // bounded-domain proof rather than a silently weaker universal.
    let mut citations: Vec<Citation> = Vec::new();
    let mut hyp_of: HashMap<String, String> = HashMap::new();
    let mut missing: Vec<String> = Vec::new();
    for f in &parser_ext {
        match parser_pool.get(f) {
            Some(raw)
                if raw.line < vb.line && build_shape(raw.vb, raw.law, ctx, depth - 1).is_some() =>
            {
                let hyp = format!("hcite{}", citations.len());
                hyp_of.insert(f.clone(), hyp.clone());
                citations.push(Citation {
                    hyp,
                    src_fn: f.clone(),
                    kind: CiteKind::Parser {
                        binders: raw.binders.clone(),
                        arg_names: raw.arg_names.clone(),
                        call: raw.call.clone(),
                        v_name: raw.v_name.clone(),
                        p_name: raw.p_name.clone(),
                        pos_name: raw.pos_name.clone(),
                        ok_ctor: raw.ok_ctor.clone(),
                        extras_count: raw.extras_count,
                        theorem: raw.theorem.clone(),
                    },
                });
            }
            _ => missing.push(f.clone()),
        }
    }
    for g in &weak_ext {
        match weak_pool.get(g) {
            Some(raw) if raw.line < vb.line && raw.universal => {
                let hyp = format!("hcite{}", citations.len());
                hyp_of.insert(g.clone(), hyp.clone());
                citations.push(Citation {
                    hyp,
                    src_fn: g.clone(),
                    kind: CiteKind::Weak {
                        g_lean: aver_name_to_lean(g),
                        theorem: raw.theorem.clone(),
                    },
                });
            }
            _ => missing.push(g.clone()),
        }
    }

    if !missing.is_empty() {
        // Only the outermost call diagnoses; recursive universality probes stay
        // silent (a nested decline is expected, not a user-facing gap).
        if depth == PROBE_DEPTH_TOP {
            eprintln!(
                "aver: clique cursor-monotonicity for `{}` declines — no universal \
                 advance law in the pool (declared before line {}) for: {}. The law \
                 keeps its bounded-domain proof.",
                ls.target,
                vb.line,
                missing.join(", ")
            );
        }
        return None;
    }

    // Target-member gate — SHARED by recognizer and emitter (`recognize =
    // build_shape(..).is_some()`): the law's subject fn must be a clique member
    // whose conjunct the projection can apply at `s pos v p` (no extra params).
    let target_idx = *index_of.get(&vb.fn_name)?;
    if !members[target_idx].extras.is_empty() {
        return None;
    }

    // Result type and its constructors, for the projection's `cases` arms.
    let result_type = ls.ok_ctor.rsplit_once('.').map(|(t, _)| t.to_string())?;
    let variants = lookup_variants(ctx, &result_type)?;

    let rep = members[0].lean.clone();
    let r = members.len();
    let shape = CliqueShape {
        members,
        index_of,
        rep,
        r,
        val_type: ls.val_type.clone(),
        ok_ctor: ctor_to_lean(&ls.ok_ctor),
        variants,
        citations,
        hyp_of,
    };
    Some((shape, ls))
}

/// Look up a sum type's constructors as `(lean_variant_name, arity)`.
fn lookup_variants(ctx: &CodegenContext, type_name: &str) -> Option<Vec<(String, usize)>> {
    let find = |defs: &[crate::ast::TypeDef]| -> Option<Vec<(String, usize)>> {
        defs.iter().find_map(|d| match d {
            crate::ast::TypeDef::Sum { name, variants, .. } if name == type_name => Some(
                variants
                    .iter()
                    .map(|v| {
                        let mut c = v.name.chars();
                        let lean = match c.next() {
                            Some(f) => f.to_lowercase().collect::<String>() + c.as_str(),
                            None => v.name.clone(),
                        };
                        (lean, v.fields.len())
                    })
                    .collect(),
            ),
            _ => None,
        })
    };
    find(&ctx.type_defs).or_else(|| ctx.modules.iter().find_map(|m| find(&m.type_defs)))
}

enum EdgeKind {
    /// A non-clique sub-parser callee whose returned position feeds a cursor.
    External(String),
    /// A monotone-cursor helper `G` (source name) wrapping an edge position.
    UsesSkipWs(String),
    Sibling,
}

/// Walk a member's leaves, reporting external callees and monotone-cursor
/// helper usage (each by source name).
fn edges_and_externals(m: &MemberInfo) -> Vec<EdgeKind> {
    fn visit(leaf: &Leaf, out: &mut Vec<EdgeKind>) {
        match leaf {
            Leaf::External { name, pos } => {
                out.push(EdgeKind::External(name.clone()));
                if let Some(g) = skipws_fn(pos) {
                    out.push(EdgeKind::UsesSkipWs(g.to_string()));
                }
            }
            Leaf::Sibling { pos, .. } => {
                if let Some(g) = skipws_fn(pos) {
                    out.push(EdgeKind::UsesSkipWs(g.to_string()));
                }
                out.push(EdgeKind::Sibling);
            }
            _ => {}
        }
    }
    let mut out = Vec::new();
    match &m.shape {
        Member::CharAt { some } => match some {
            CharSome::Direct(l) => visit(l, &mut out),
            CharSome::Nested(ls) => ls.iter().for_each(|l| visit(l, &mut out)),
        },
        Member::DispatchParam { leaves } => leaves.iter().for_each(|l| visit(l, &mut out)),
        Member::ResultMatch {
            scrut,
            scrut_is_sibling,
            edge,
            ..
        } => {
            if !scrut_is_sibling {
                out.push(EdgeKind::External(scrut.clone()));
            }
            visit(edge, &mut out);
        }
        Member::AdtMatch { edge, .. } => visit(edge, &mut out),
    }
    out
}

/// The monotone-cursor helper's source name a position class routes through, if
/// any (`G` in `G s pos` / `G s (pos+1)` / `G s p'`). `None` for a bare
/// same-position / `pos+1` / result-binder edge.
fn skipws_fn(pos: &PosClass) -> Option<&str> {
    match pos {
        PosClass::Weak(h) => Some(h.as_str()),
        PosClass::AdvConst { skipws: Some(h) } => Some(h.as_str()),
        PosClass::Semantic {
            skipws: Some(h), ..
        } => Some(h.as_str()),
        _ => None,
    }
}

pub(in crate::codegen::lean) fn recognize_clique_position_monotonicity(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    // Recognizer = the emitter's own plan succeeding. `build_shape` runs the
    // FULL plan (clique analysis, rank assignment, citation resolution, AND the
    // target-member/no-extras gate the projection needs), so the recognizer can
    // never class a law universal that the emitter would decline. One shared
    // function, no drift.
    build_shape(vb, law, ctx, PROBE_DEPTH_TOP).is_some()
}

// ===========================================================================
// Emission.
// ===========================================================================

/// Right-nested `∧` projection accessor for conjunct `i` of `r`.
fn proj(i: usize, r: usize) -> String {
    if i + 1 == r {
        ".2".repeat(r - 1)
    } else {
        format!("{}.1", ".2".repeat(i))
    }
}

fn underscores(n: usize) -> String {
    std::iter::repeat_n("_", n).collect::<Vec<_>>().join(" ")
}

/// Replace a semantic edge's returned-position binder with a fresh name so the
/// emitted `next` binder never collides with the conjunct's `v` / `p`.
fn rebind_semantic(leaf: &Leaf, new_binder: &str) -> Leaf {
    match leaf {
        Leaf::Sibling {
            name,
            pos: PosClass::Semantic { skipws, .. },
        } => Leaf::Sibling {
            name: name.clone(),
            pos: PosClass::Semantic {
                skipws: skipws.clone(),
                binder: new_binder.to_string(),
            },
        },
        other => other.clone(),
    }
}

/// The pre-`skipws` inner position argument a class wraps (`pos` / `(pos+1)` /
/// the semantic binder), for the monotone-cursor bound `inner ≤ G s inner`.
fn skipws_inner(class: &PosClass, pos: &str) -> Option<String> {
    match class {
        PosClass::Weak(_) => Some(pos.to_string()),
        PosClass::AdvConst { skipws: Some(_) } => Some(format!("({pos} + 1)")),
        PosClass::Semantic {
            skipws: Some(_),
            binder,
        } => Some(binder.clone()),
        _ => None,
    }
}

/// Emit the monotone-cursor bound `inner ≤ G s inner` (via the cited `Weak`
/// hypothesis) when the class routes an edge position through a helper `G`, so
/// the closing `omega` can chain across the whitespace skip.
fn emit_skipws_bound(
    class: &PosClass,
    s: &str,
    pos: &str,
    shape: &CliqueShape,
    out: &mut Vec<String>,
    indent: &str,
) {
    if let (Some(g), Some(inner)) = (skipws_fn(class), skipws_inner(class, pos))
        && let Some(hyp) = shape.hyp_of.get(g)
    {
        out.push(format!("{indent}have hws := {hyp} {s} {inner}"));
    }
}

/// Emit the closer for a single leaf inside a `split` branch. `pos` is the
/// caller position name, `s` the caller subject name. Sibling citations pass
/// `_` for the callee's extra arguments (unified from `heq`).
fn emit_leaf(
    leaf: &Leaf,
    s: &str,
    pos: &str,
    shape: &CliqueShape,
    t: &str,
    out: &mut Vec<String>,
    indent: &str,
) {
    match leaf {
        // `heq : errCtor … = okCtor v p` — impossible by constructor disjointness.
        Leaf::Err => out.push(format!("{indent}exact absurd heq (by simp)")),
        // `heq : okCtor _ posexpr = okCtor v p` — inject the position, `posexpr ∈
        // {pos, pos+1}` so `pos ≤ p` closes by `omega`.
        Leaf::OkTerminal => {
            out.push(format!("{indent}injection heq with _ hp"));
            out.push(format!("{indent}omega"));
        }
        Leaf::Sibling { name, pos: class } => {
            let idx = shape.index_of[name];
            let callee = &shape.members[idx];
            let extras = underscores(callee.extras.len());
            let extras = if extras.is_empty() {
                String::new()
            } else {
                format!(" {extras}")
            };
            let hc = if callee.dispatch_param.is_some() {
                " hc"
            } else {
                ""
            };
            emit_skipws_bound(class, s, pos, shape, out, indent);
            let posr = pos_render(class, s, pos);
            out.push(format!(
                "{indent}have hcall := ih{idx} {s} {posr}{extras} v p{hc} (by unfold {t} at *; omega) heq",
            ));
            out.push(format!("{indent}omega"));
        }
        Leaf::External { name, pos: class } => {
            // A non-clique sub-parser terminal edge: cite its advance hypothesis
            // (`F … = ok v p → pos ≤ p`), discharged in the projection from the
            // pool law. Its returned position is `p`, so `pos ≤ p` follows and,
            // for a `pos+1` advance, `omega` bridges the `+1`.
            let (hyp, extras_count) = shape
                .parser_cite(name)
                .expect("external leaf without a resolved parser citation");
            let extras = underscores(extras_count);
            let extras = if extras.is_empty() {
                String::new()
            } else {
                format!(" {extras}")
            };
            emit_skipws_bound(class, s, pos, shape, out, indent);
            let posr = pos_render(class, s, pos);
            out.push(format!(
                "{indent}have hcall := {hyp} {s} {posr}{extras} v p heq"
            ));
            out.push(format!("{indent}omega"));
        }
    }
}

/// Whether a member's `Some(c)` / dispatch branch advances into a sibling and
/// thus needs the `charAt` in-bounds facts.
fn leaves_need_bounds(leaves: &[Leaf]) -> bool {
    leaves.iter().any(|l| {
        matches!(
            l,
            Leaf::Sibling {
                pos: PosClass::AdvConst { .. },
                ..
            }
        )
    })
}

/// Emit one member's conjunct proof block.
fn emit_member_block(
    m: &MemberInfo,
    i: usize,
    shape: &CliqueShape,
    t: &str,
    out: &mut Vec<String>,
) {
    let s = &m.s_name;
    let pos = &m.pos_name;
    let extra_names: Vec<String> = m.extras.iter().map(|(n, _)| n.clone()).collect();
    let extra_str = if extra_names.is_empty() {
        String::new()
    } else {
        format!(" {}", extra_names.join(" "))
    };
    out.push(format!("    · -- ({i}) {} (rank {})", m.fd.name, m.rank));
    // intro binders + premises
    let prem = if m.dispatch_param.is_some() {
        "hc ht heq"
    } else {
        "ht heq"
    };
    out.push(format!("      intro {s} {pos}{extra_str} v p {prem}"));
    if let Some(cparam) = &m.dispatch_param {
        out.push(format!(
            "      obtain ⟨h0, hlt⟩ := String.charAt_some_bounds {s} {pos} {cparam} hc"
        ));
    }
    out.push(format!("      simp only [{}__fuel] at heq", m.lean));

    match &m.shape {
        Member::CharAt { some } => {
            out.push("      split at heq".to_string());
            out.push("      · simp at heq".to_string());
            match some {
                CharSome::Direct(leaf) => {
                    out.push("      · next c hc =>".to_string());
                    emit_leaf(leaf, s, pos, shape, t, out, "          ");
                }
                CharSome::Nested(leaves) => {
                    out.push("      · next c hc =>".to_string());
                    if leaves_need_bounds(leaves) {
                        out.push(format!(
                            "          obtain ⟨h0, hlt⟩ := String.charAt_some_bounds {s} {pos} c hc"
                        ));
                    }
                    out.push("          split at heq".to_string());
                    for leaf in leaves {
                        out.push("          ·".to_string());
                        emit_leaf(leaf, s, pos, shape, t, out, "            ");
                    }
                }
            }
        }
        Member::DispatchParam { leaves } => {
            out.push("      split at heq".to_string());
            for leaf in leaves {
                out.push("      ·".to_string());
                emit_leaf(leaf, s, pos, shape, t, out, "        ");
            }
        }
        Member::ResultMatch {
            scrut,
            scrut_is_sibling,
            edge,
            err_first,
        } => {
            out.push("      split at heq".to_string());
            // The scrutinee sub-call bounds the semantic edge: `pos ≤ scrutPos`.
            // A clique sibling supplies it from the IH conjunct; a non-clique
            // sub-parser from its cited advance hypothesis.
            let hmono = if *scrut_is_sibling {
                let scrut_idx = shape.index_of[scrut];
                let scrut_extras = underscores(shape.members[scrut_idx].extras.len());
                let scrut_extras = if scrut_extras.is_empty() {
                    String::new()
                } else {
                    format!(" {scrut_extras}")
                };
                format!(
                    "          have hmono := ih{scrut_idx} {s} {pos}{scrut_extras} scrutVal scrutPos (by unfold {t} at *; omega) hv"
                )
            } else {
                let (hyp, extras_count) = shape
                    .parser_cite(scrut)
                    .expect("external scrutinee without a resolved parser citation");
                let scrut_extras = underscores(extras_count);
                let scrut_extras = if scrut_extras.is_empty() {
                    String::new()
                } else {
                    format!(" {scrut_extras}")
                };
                format!(
                    "          have hmono := {hyp} {s} {pos}{scrut_extras} scrutVal scrutPos hv"
                )
            };
            // Fresh binder names for the sub-parser's returned (value, position)
            // so they never shadow the conjunct's own `v` / `p`.
            let edge = rebind_semantic(edge, "scrutPos");
            let emit_err = |out: &mut Vec<String>| {
                out.push("      · simp at heq".to_string());
            };
            let emit_ok = |out: &mut Vec<String>| {
                out.push("      · next scrutVal scrutPos hv =>".to_string());
                out.push(hmono.clone());
                emit_leaf(&edge, s, pos, shape, t, out, "          ");
            };
            if *err_first {
                emit_err(out);
                emit_ok(out);
            } else {
                emit_ok(out);
                emit_err(out);
            }
        }
        Member::AdtMatch {
            edge, ctor_first, ..
        } => {
            out.push("      split at heq".to_string());
            let emit_ctor = |out: &mut Vec<String>| {
                out.push("      ·".to_string());
                emit_leaf(edge, s, pos, shape, t, out, "        ");
            };
            let emit_wild = |out: &mut Vec<String>| {
                out.push("      · simp at heq".to_string());
            };
            if *ctor_first {
                emit_ctor(out);
                emit_wild(out);
            } else {
                emit_wild(out);
                emit_ctor(out);
            }
        }
    }
}

/// Build the conjunct statement (`∀ s pos <extras> v p, [premise ->] T ... ->
/// (call == ok v p) = true -> (p >= pos) = true`).
fn conjunct_statement(m: &MemberInfo, shape: &CliqueShape, t: &str) -> String {
    // The statement binds canonical `s`/`pos`; the member body walk emits
    // tactics against these same names (its params are `s`, `pos` too).
    let mut binders = String::from("(s : String) (pos : Int)");
    let mut args = String::from("s pos");
    for (n, ty) in &m.extras {
        binders.push_str(&format!(" ({n} : {ty})"));
        args.push_str(&format!(" {n}"));
    }
    binders.push_str(&format!(" (v : {}) (p : Int)", shape.val_type));
    let premise = match &m.dispatch_param {
        // A char-dispatch member links its char parameter to `charAt s pos`.
        Some(cparam) => format!("String.charAtAv s pos = some {cparam} -> "),
        None => String::new(),
    };
    let call = format!("{}__fuel fuel {args}", m.lean);
    // Proved in Prop form (`= ok v p → pos ≤ p`): constructor disjointness and
    // injection stay kernel-clean without a `LawfulBEq` on the value field.
    format!(
        "(∀ {binders}, {premise}{t} s pos {rank} ≤ fuel -> {call} = {ok} v p -> pos ≤ p)",
        rank = m.rank,
        ok = shape.ok_ctor,
    )
}

/// The Prop-shape type of a citation's external hypothesis.
fn cite_hyp_type(c: &Citation) -> String {
    match &c.kind {
        CiteKind::Parser {
            binders,
            call,
            v_name,
            p_name,
            pos_name,
            ok_ctor,
            ..
        } => format!("∀ {binders}, {call} = {ok_ctor} {v_name} {p_name} → {pos_name} ≤ {p_name}"),
        CiteKind::Weak { g_lean, .. } => {
            format!("∀ (s : String) (q : Int), q ≤ {g_lean} s q")
        }
    }
}

/// The `(hyp : <type>)` binder the conjunction theorem takes for a citation.
fn cite_hyp_binder(c: &Citation) -> String {
    format!("({} : {})", c.hyp, cite_hyp_type(c))
}

/// The projection-side reconstruction of a citation's hypothesis from the pool
/// law, at `indent`. Kept INSIDE the projection's `first | … | sorry`, so a
/// bounded (domain-restricted) cited theorem makes this block fail to typecheck
/// and the law floors to bounded — fail-closed, no tier oracle.
fn emit_cite_have(c: &Citation, indent: &str, out: &mut Vec<String>) {
    let ty = cite_hyp_type(c);
    out.push(format!("{indent}have {} : {ty} := by", c.hyp));
    match &c.kind {
        CiteKind::Parser {
            arg_names,
            call,
            v_name,
            p_name,
            ok_ctor,
            theorem,
            ..
        } => {
            let args = arg_names.join(" ");
            out.push(format!("{indent}  intro {args} heqc"));
            out.push(format!(
                "{indent}  have hbc : ({call} == {ok_ctor} {v_name} {p_name}) = true := by"
            ));
            out.push(format!(
                "{indent}    rw [heqc]; show ({v_name} == {v_name} && {p_name} == {p_name}) = true; simp"
            ));
            out.push(format!("{indent}  have hgec := {theorem} {args} hbc"));
            out.push(format!("{indent}  simpa using hgec"));
        }
        CiteKind::Weak { theorem, .. } => {
            out.push(format!("{indent}  intro s q"));
            out.push(format!("{indent}  simpa using {theorem} s q"));
        }
    }
}

pub(super) fn emit_clique_position_monotonicity_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let (shape, ls) = build_shape(vb, law, ctx, PROBE_DEPTH_TOP)?;
    let r = shape.r;
    let t = format!("{}_advanceThr", shape.rep);
    let thm = format!("{}_advance_mono", shape.rep);

    let mut lines: Vec<String> = Vec::new();

    // Threshold definition.
    lines.push(format!(
        "def {t} (s : String) (pos : Int) (rank : Nat) : Nat :=\n  (s.toList.length - pos.toNat) * {r} + rank + 1"
    ));
    lines.push(String::new());

    // Conjunction theorem.
    let conjuncts: Vec<String> = shape
        .members
        .iter()
        .map(|m| conjunct_statement(m, &shape, &t))
        .collect();
    // External hypotheses (discharged in the projection by pool-law citation).
    let hyp_binders = shape
        .citations
        .iter()
        .map(cite_hyp_binder)
        .collect::<Vec<_>>()
        .join(" ");
    if hyp_binders.is_empty() {
        lines.push(format!("theorem {thm} :"));
    } else {
        lines.push(format!("theorem {thm} {hyp_binders} :"));
    }
    lines.push("    ∀ (fuel : Nat),".to_string());
    lines.push(format!("      {} := by", conjuncts.join("\n      ∧ ")));
    lines.push("  intro fuel".to_string());
    // `first | (…) | sorry`: a non-closing branch fails to `sorry` so a slipped
    // rung is denied universal credit (the `#print axioms` whitelist gates it),
    // rather than emitting a silently weaker proof.
    lines.push("  first".to_string());
    lines.push("  | (induction fuel with".to_string());
    let holes = std::iter::repeat_n("?_", r).collect::<Vec<_>>().join(", ");
    lines.push(format!(
        "     | zero => refine ⟨{holes}⟩ <;> (intros; unfold {t} at *; omega)"
    ));
    lines.push("     | succ t ih =>".to_string());
    let ih_names = (0..r)
        .map(|i| format!("ih{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    lines.push(format!("       obtain ⟨{ih_names}⟩ := ih"));
    lines.push(format!("       refine ⟨{holes}⟩"));
    for (i, m) in shape.members.iter().enumerate() {
        let mut block = Vec::new();
        emit_member_block(m, i, &shape, &t, &mut block);
        // shift member block indentation by 3 spaces (inside `succ` case).
        for l in block {
            lines.push(format!("   {l}"));
        }
    }
    lines.push("     )".to_string());
    lines.push("  | sorry".to_string());
    lines.push(String::new());

    // Projection theorem for the user-named member.
    let target_idx = *shape.index_of.get(&vb.fn_name)?;
    let target = &shape.members[target_idx];
    // projection binders passed to the conjunct: s pos <extras> v p, using the
    // law-role names (target has no extras for the supported witness/json case).
    let target_lean = aver_name_to_lean(&vb.fn_name);
    let render = |e: &Spanned<Expr>| {
        super::super::expr::emit_expr(
            &super::super::expr::resolve_rewrite_output(e, ctx, None),
            ctx,
        )
    };
    let when_render = law.when.as_ref().map(&render)?;
    let lhs_render = render(&law.lhs);
    let rhs_render = render(&law.rhs);
    let proj_acc = proj(target_idx, r);
    // Conjunct application args: `s pos v p` (supported target carries no extra
    // parameters — same shape for the json parseValue clique and the witness).
    if !target.extras.is_empty() {
        return None;
    }
    let s_name = ls_s_name(law);
    let pos_name = &ls.pos_name;
    let v_name = &ls.val_name;
    let p_name = &ls.p_name;
    let ok_variant = shape
        .ok_ctor
        .rsplit_once('.')
        .map(|(_, v)| v)
        .unwrap_or(&shape.ok_ctor);
    lines.push(format!(
        "theorem {theorem_base} : ∀ {quant_params}, {when_render} = true -> {lhs_render} = {rhs_render} := by"
    ));
    lines.push(format!(
        "  intro {} hbeq",
        law.givens
            .iter()
            .map(|g| aver_name_to_lean(&g.name))
            .collect::<Vec<_>>()
            .join(" ")
    ));
    // Hypothesis arguments applied to the conjunction theorem, before `fuel`.
    let hyp_args = if shape.citations.is_empty() {
        String::new()
    } else {
        format!(
            "{} ",
            shape
                .citations
                .iter()
                .map(|c| c.hyp.clone())
                .collect::<Vec<_>>()
                .join(" ")
        )
    };
    lines.push("  first".to_string());
    // Open the closing branch and, when there are citations, reconstruct each
    // external hypothesis from its pool law BEFORE the case analysis (all inside
    // the `first | (…) | sorry` floor).
    if shape.citations.is_empty() {
        lines.push(format!(
            "  | (cases hF : {target_lean} {s_name} {pos_name} with"
        ));
    } else {
        lines.push("  | (".to_string());
        for c in &shape.citations {
            emit_cite_have(c, "     ", &mut lines);
        }
        lines.push(format!(
            "     cases hF : {target_lean} {s_name} {pos_name} with"
        ));
    }
    for (variant, arity) in &shape.variants {
        if variant == ok_variant {
            lines.push(format!("     | {variant} okVal okPos =>"));
            lines.push("         rw [hF] at hbeq".to_string());
            lines.push(format!(
                "         have hh : (okVal == {v_name} && okPos == {p_name}) = true := hbeq"
            ));
            lines.push("         simp only [Bool.and_eq_true] at hh".to_string());
            lines.push(format!(
                "         have hpp : okPos = {p_name} := eq_of_beq hh.2"
            ));
            lines.push("         rw [hpp] at hF".to_string());
            lines.push(format!(
                "         have key := ({thm} {hyp_args}(averStringPosFuel {s_name} {pos_name} {r})){proj_acc} {s_name} {pos_name} okVal {p_name} (by unfold {t} averStringPosFuel; omega) hF"
            ));
            lines.push("         simp [key]".to_string());
        } else {
            let binders = (0..*arity)
                .map(|k| format!("e{k}"))
                .collect::<Vec<_>>()
                .join(" ");
            let binders = if binders.is_empty() {
                String::new()
            } else {
                format!(" {binders}")
            };
            lines.push(format!(
                "     | {variant}{binders} => rw [hF] at hbeq; nomatch hbeq"
            ));
        }
    }
    lines.push("     )".to_string());
    lines.push("  | sorry".to_string());
    Some(AutoProof {
        support_lines: lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

fn ls_s_name(law: &VerifyLaw) -> String {
    // subject `s` name = first argument of the `when` call.
    if let Some(when) = &law.when
        && let Expr::BinOp(BinOp::Eq, call, _) = &when.node
        && let Expr::FnCall(_, args) = &call.node
        && let Some(first) = args.first()
        && let Expr::Ident(n) = &first.node
    {
        return n.clone();
    }
    "s".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Expr, Literal, Spanned};
    use std::collections::HashSet;

    fn ident(n: &str) -> Spanned<Expr> {
        Spanned::bare(Expr::Ident(n.to_string()))
    }

    fn call(name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        Spanned::bare(Expr::FnCall(Box::new(ident(name)), args))
    }

    fn add1(inner: Spanned<Expr>) -> Spanned<Expr> {
        Spanned::bare(Expr::BinOp(
            BinOp::Add,
            Box::new(inner),
            Box::new(Spanned::bare(Expr::Literal(Literal::Int(1)))),
        ))
    }

    fn weak_set(names: &[&str]) -> HashSet<String> {
        names.iter().map(|n| n.to_string()).collect()
    }

    // ---- edge classifier ----------------------------------------------------

    #[test]
    fn classify_pos_same_and_advance() {
        let no_binders = HashSet::new();
        let no_weak = HashSet::new();
        assert_eq!(
            classify_pos(&ident("pos"), "s", "pos", &no_binders, &no_weak),
            Some(PosClass::Same)
        );
        assert_eq!(
            classify_pos(&add1(ident("pos")), "s", "pos", &no_binders, &no_weak),
            Some(PosClass::AdvConst { skipws: None })
        );
    }

    #[test]
    fn classify_pos_weak_helper_is_pool_law_keyed_not_name_keyed() {
        let no_binders = HashSet::new();
        // A helper `trimWs` is monotone-cursor ONLY because a pool law is
        // registered about it (`weak` set), never because of its name. Any name
        // works; the literal `skipWs` is not special.
        let weak = weak_set(&["trimWs"]);
        assert_eq!(
            classify_pos(
                &call("trimWs", vec![ident("s"), ident("pos")]),
                "s",
                "pos",
                &no_binders,
                &weak
            ),
            Some(PosClass::Weak("trimWs".to_string()))
        );
        assert_eq!(
            classify_pos(
                &call("trimWs", vec![ident("s"), add1(ident("pos"))]),
                "s",
                "pos",
                &no_binders,
                &weak
            ),
            Some(PosClass::AdvConst {
                skipws: Some("trimWs".to_string())
            })
        );
        // LITMUS: the SAME call with NO pool law about `trimWs` is NOT a cursor
        // helper — classification declines (no name shortcut).
        let empty = HashSet::new();
        assert_eq!(
            classify_pos(
                &call("trimWs", vec![ident("s"), ident("pos")]),
                "s",
                "pos",
                &no_binders,
                &empty
            ),
            None
        );
        // And `skipWs` gets no special treatment either without a pool law.
        assert_eq!(
            classify_pos(
                &call("skipWs", vec![ident("s"), ident("pos")]),
                "s",
                "pos",
                &no_binders,
                &empty
            ),
            None
        );
    }

    #[test]
    fn classify_pos_semantic_from_result_binder() {
        let mut binders = HashSet::new();
        binders.insert("p2".to_string());
        let weak = weak_set(&["skipWs"]);
        // p2 (a sub-parser's returned position) → semantic.
        assert_eq!(
            classify_pos(&ident("p2"), "s", "pos", &binders, &weak),
            Some(PosClass::Semantic {
                skipws: None,
                binder: "p2".to_string()
            })
        );
        // skipWs(s, p2) → semantic through the (pool-law-keyed) cursor helper.
        assert_eq!(
            classify_pos(
                &call("skipWs", vec![ident("s"), ident("p2")]),
                "s",
                "pos",
                &binders,
                &weak
            ),
            Some(PosClass::Semantic {
                skipws: Some("skipWs".to_string()),
                binder: "p2".to_string()
            })
        );
        // An unrelated identifier is not a recognized cursor position.
        assert_eq!(
            classify_pos(&ident("acc"), "s", "pos", &binders, &weak),
            None
        );
    }

    // ---- rank assignment ----------------------------------------------------

    #[test]
    fn rank_dag_reproduces_json_clique_table() {
        // Members: 0 dispatchChar, 1 parseValue, 2 parseArray, 3 parseArrayElems,
        // 4 parseArrayNext, 5 parseObject, 6 parseObjFields, 7 parseObjKey,
        // 8 parseObjKeyValue, 9 parseObjColon, 10 parseObjNext, 11 parseObjVal.
        // Rank-DAG edges (same/weak/semantic only; constant advances excluded):
        let edges = [
            (1, 0),   // parseValue -> dispatchChar (same)
            (2, 3),   // parseArray -> parseArrayElems (same)
            (3, 1),   // parseArrayElems -> parseValue (same, scrutinee)
            (3, 4),   // parseArrayElems -> parseArrayNext (semantic)
            (5, 6),   // parseObject -> parseObjFields (same)
            (11, 1),  // parseObjVal -> parseValue (same, scrutinee)
            (11, 10), // parseObjVal -> parseObjNext (semantic)
            (8, 9),   // parseObjKeyValue -> parseObjColon (weak)
            (7, 8),   // parseObjKey -> parseObjKeyValue (semantic)
        ];
        let ranks = rank_dag(12, &edges).expect("acyclic");
        let expected = [0, 1, 3, 2, 0, 1, 0, 2, 1, 0, 0, 2];
        assert_eq!(ranks, expected, "ranks must match the design table");
    }

    #[test]
    fn rank_dag_rejects_cycle_fail_closed() {
        // A same/semantic cycle (e.g. a foreign clique where a semantic edge is
        // forced rank-up) has no valid assignment → decline.
        let edges = [(0, 1), (1, 2), (2, 0)];
        assert_eq!(rank_dag(3, &edges), None);
    }

    #[test]
    fn rank_dag_witness_three_clique() {
        // scanExpr(0) scanGroup(1) scanTail(2): scanGroup->scanExpr (same,scrut),
        // scanGroup->scanTail (semantic). scanExpr->scanGroup and scanTail->
        // scanGroup are constant advances (rank-free, excluded).
        let edges = [(1, 0), (1, 2)];
        let ranks = rank_dag(3, &edges).expect("acyclic");
        assert_eq!(ranks, [0, 1, 0]);
    }
}
