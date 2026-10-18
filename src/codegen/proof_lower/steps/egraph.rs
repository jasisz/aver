//! A table of equal terms with explanations: where evaluation stops, the
//! two sides and the hypotheses in scope go into one table that keeps
//! every term it has seen and joins the terms it learns are equal, then
//! the steps that joined them are written out for the kernel.
//!
//! The table knows no rule of the kernel. It reads only data: the
//! hypotheses (`t = v`, `a == b` true, a conjunction true, a disjunction
//! false), the equations of definitions where the shape of an argument
//! picks the arm, the cited laws and builtin facts matched against the
//! terms it holds in both directions, and closed parts it computes. Two
//! terms whose parts are joined are joined too (congruence). A law that
//! states `f(a, b) = f(b, a)` cannot loop: both sides are one entry.
//!
//! Each join remembers why it was made, so the equality of any two terms
//! of one entry is a path of joins: a hypothesis, a law instance, an
//! opened definition or a congruence, written as `trans`, `symm`, `congr`,
//! `hyp`, `law` and `unfold` steps. The comparisons the table knows are
//! handed to linear arithmetic, with `x != k` read as `x < k` or `x > k`
//! where a range brings `k` to its edge.
//!
//! Everything is bounded: terms, rounds, law instances, and the fuel the
//! table may spend proving premises, which it gives back, so a failed
//! attempt costs the rest of the proof nothing.

use std::collections::{HashMap, HashSet};

use crate::ast::BinOp;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, LawRef, Proof, WallRule};

use super::env::Env;

/// Most terms one table holds.
const MAX_NODES: usize = 400;
/// Most rounds of opening definitions and matching laws.
const MAX_ROUNDS: usize = 6;
/// Most law and rule instances one table adds.
const MAX_INSTANCES: usize = 160;
/// A term that many instances or openings away from the start matches no
/// law and opens no definition: what an instance writes is matched again
/// only so far, so laws that feed each other stop.
const MAX_GEN_DEFAULT: u8 = 3;
/// The same for opening definitions.
const MAX_OPEN_GEN_DEFAULT: u8 = 4;
/// Evaluation steps the table may spend proving premises; given back after.
const TABLE_FUEL: usize = 200;
/// Most comparisons one round tries to decide by linear arithmetic.
const MAX_DECIDE: usize = 24;
/// Most comparisons handed to linear arithmetic at once.
const MAX_FACTS: usize = 32;
/// Most tables one obligation builds.
pub(crate) const MAX_TABLES: usize = 48;
/// Largest proof a table hands back.
const MAX_PROOF: usize = 600;
/// Most cases on `x <= k` a read of `x != k` opens, one inside another.
const MAX_DISEQ: usize = 8;

type Id = usize;

fn tuned(name: &str, default: u8) -> u8 {
    std::env::var(name)
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(default)
}

#[allow(non_snake_case)]
fn MAX_GEN() -> u8 {
    tuned("AVER_TABLE_GEN", MAX_GEN_DEFAULT)
}

#[allow(non_snake_case)]
fn MAX_OPEN_GEN() -> u8 {
    tuned("AVER_TABLE_OPEN_GEN", MAX_OPEN_GEN_DEFAULT)
}

/// A premise of a join: a proof built already, or two entries of the table
/// whose terms it shows equal.
#[derive(Clone, Debug)]
enum Prem {
    Done(Proof),
    Same(Id, Id),
}

/// Why two terms were joined. Each proves `term(from) = term(to)` for the
/// two terms the join was made with, in that order.
#[derive(Clone, Debug)]
enum Why {
    Given(Proof),
    /// Same head, every part joined.
    Congr,
    Rule {
        rule: WallRule,
        subst: Vec<(String, Term)>,
        premises: Vec<Prem>,
    },
    Law {
        law: String,
        subst: Vec<(String, Term)>,
        premise: Option<Prem>,
    },
    /// Arm `arm` of a definition; the premise shows the subject equal to
    /// the arm's pattern (then a cell step when it is a list literal).
    Unfold {
        fn_id: FnId,
        arm: u32,
        args: Vec<Term>,
        binders: Vec<Term>,
        premise: Prem,
        cell: bool,
    },
    Arm {
        term: Term,
        arm: u32,
        binders: Vec<Term>,
        premise: Prem,
        cell: bool,
    },
    /// `when = !value`: the instance `l = r` that `when = value` allows
    /// (a law's `when`, a rule's one premise) would join the two Bool
    /// values.
    WhenFalse {
        when: Term,
        value: bool,
        step: Step,
        subst: Vec<(String, Term)>,
        l: Id,
        r: Id,
    },
    /// A side of `Bool.or(a, b)` is `false` where the disjunction is.
    OrFalse { or: Id, falsity: Id, left: bool },
    /// `a = !v` where `Bool.not(a) = v`.
    NotElim { not: Id, value: Id },
}

struct Node {
    key: String,
    kids: Vec<Id>,
    term: Term,
    /// How many law instances and openings stand between the term and the
    /// sides and hypotheses the table started from.
    depth: u8,
}

/// A law or rule the table matches: `lhs = rhs` over `vars`, under
/// `premises` (each `p = v`).
struct Rewrite {
    kind: RewriteKind,
    vars: Vec<String>,
    lhs: Term,
    rhs: Term,
    triggers: Vec<Term>,
}

enum RewriteKind {
    Law(Box<LawRef>),
    Wall(WallRule, Vec<Eqn>),
}

/// A law or a rule, by what a step names it with.
#[derive(Clone, Debug)]
enum Step {
    Law(String),
    Wall(WallRule),
}

/// An instance whose premise `when = value` is not shown yet: a law's
/// `when` (`value` true), or a rule's one premise.
struct Cond {
    step: Step,
    subst: Vec<(String, Term)>,
    l: Id,
    r: Id,
    when: Term,
    value: bool,
    done: bool,
}

impl Cond {
    fn why(&self, premise: Prem) -> Why {
        match &self.step {
            Step::Law(law) => Why::Law {
                law: law.clone(),
                subst: self.subst.clone(),
                premise: Some(premise),
            },
            Step::Wall(rule) => Why::Rule {
                rule: *rule,
                subst: self.subst.clone(),
                premises: vec![premise],
            },
        }
    }
}

/// One fact for linear arithmetic: `term = value`, shown by `why`.
struct Fact {
    term: Term,
    value: bool,
    why: Prem,
}

pub(crate) struct Table {
    nodes: Vec<Node>,
    exact: HashMap<(String, Vec<Id>), Id>,
    sig: HashMap<(String, Vec<Id>), Id>,
    by_key: HashMap<String, Vec<Id>>,
    parent: Vec<Id>,
    members: Vec<Vec<Id>>,
    uses: Vec<Vec<Id>>,
    edge: Vec<Option<(Id, Why, bool)>>,
    pending: Vec<(Id, Id, Why)>,
    fired: HashSet<(Id, u8)>,
    opened: Vec<bool>,
    seen: HashSet<(usize, Vec<Id>)>,
    conds: Vec<Cond>,
    rewrites: Vec<Rewrite>,
    instances: usize,
    unions: usize,
    full: bool,
    t: Id,
    f: Id,
    /// Hypothesis names the explanations have given out.
    names: std::cell::Cell<usize>,
    /// The generation new terms are added at.
    gen_now: u8,
    /// The comparisons a `match` waits for to open a definition.
    guards: Vec<Id>,
    /// For each comparison tried by linear arithmetic, how many joins the
    /// table had then: it is tried again only once it knows more.
    decided_at: HashMap<Id, usize>,
}

/// The head of `t` apart from its parts, and whether its parts are
/// entries of their own (`false` for a literal, a variable, and the terms
/// the table reads as one opaque entry).
fn head(t: &Term) -> (String, bool) {
    use ResolvedExpr as E;
    if let Some(i) = term::int_value(t) {
        return (format!("i{i}"), false);
    }
    match &t.node {
        E::Literal(l) => (format!("l{l:?}"), false),
        E::Ident(n) | E::Resolved { name: n, .. } => (format!("v{n}"), false),
        E::Attr(_, f) => (format!("a{f}"), true),
        E::Call(c, xs) => (format!("c{c:?}/{}", xs.len()), true),
        E::BinOp(o, ..) => (format!("b{o:?}"), true),
        E::Neg(_) => ("n".into(), true),
        E::Ctor(c, xs) => (format!("k{c:?}/{}", xs.len()), true),
        E::List(xs) => (format!("L{}", xs.len()), true),
        E::Tuple(xs) => (format!("T{}", xs.len()), true),
        E::RecordCreate {
            type_name, fields, ..
        } => (
            format!(
                "R{type_name}:{}",
                fields
                    .iter()
                    .map(|(n, _)| n.as_str())
                    .collect::<Vec<_>>()
                    .join(",")
            ),
            true,
        ),
        E::Match { arms, .. } => (format!("m{arms:?}"), true),
        _ => (format!("x{t:?}"), false),
    }
}

/// The parts linear arithmetic reads as unknowns in the Int term `t`:
/// everything below sums, differences and products that is not a literal.
fn atoms(t: &Term, out: &mut Vec<Term>) {
    match &t.node {
        _ if term::is_literal(t) => {}
        ResolvedExpr::BinOp(BinOp::Add | BinOp::Sub | BinOp::Mul, a, b) => {
            atoms(a, out);
            atoms(b, out);
        }
        ResolvedExpr::Neg(a) => atoms(a, out),
        _ => {
            let t = canon(t);
            if !out.contains(&t) {
                out.push(t);
            }
        }
    }
}

fn is_cmp(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::Neq
    )
}

fn is_int(t: &Term) -> bool {
    matches!(t.ty(), Some(crate::ast::Type::Int)) || term::int_value(t).is_some()
}

/// An Int comparison: `a op b` with an Int side.
fn int_cmp(t: &Term) -> Option<(BinOp, &Term, &Term)> {
    match &t.node {
        ResolvedExpr::BinOp(op, a, b) if is_cmp(*op) && (is_int(a) || is_int(b)) => {
            Some((*op, a, b))
        }
        _ => None,
    }
}

fn builtin_args<'t>(t: &'t Term, name: &str, n: usize) -> Option<&'t [Term]> {
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) if b == name && args.len() == n => {
            Some(args)
        }
        _ => None,
    }
}

/// `from = to` from the steps `(to_i, proof_i)`, each from the term before.
fn chain(from: &Term, steps: Vec<(Term, Proof)>) -> Proof {
    match steps.len() {
        0 => Proof::Refl(from.clone()),
        1 => steps.into_iter().next().expect("one step").1,
        _ => {
            let mut terms = vec![from.clone()];
            let mut proofs = Vec::new();
            for (t, p) in steps {
                terms.push(t);
                proofs.push(p);
            }
            Proof::Trans {
                terms,
                steps: proofs,
            }
        }
    }
}

/// The wall rules the table matches like laws.
const WALL: &[WallRule] = &[
    WallRule::ConcatNil,
    WallRule::ConcatCons,
    WallRule::LenCons,
    WallRule::TakeNil,
    WallRule::TakeConsLe,
    WallRule::TakeConsGt,
    WallRule::DropNil,
    WallRule::DropConsLe,
    WallRule::DropConsGt,
    WallRule::ReverseCons,
    WallRule::ContainsNil,
    WallRule::ContainsConsFound,
    WallRule::ContainsConsOther,
    WallRule::NeDef,
];

/// The parts of `pat` worth matching on their own: calls and constructors
/// that name every variable.
fn triggers(lhs: &Term, rhs: &Term, vars: &[String], inner_too: bool) -> Vec<Term> {
    fn covers(t: &Term, vars: &[String]) -> bool {
        let mut free = Vec::new();
        term::free_vars(t, &mut free);
        vars.iter().all(|v| free.contains(v))
    }
    fn is_var(t: &Term, vars: &[String]) -> bool {
        matches!(&t.node, ResolvedExpr::Ident(n) if vars.contains(n))
    }
    fn parts(t: &Term, vars: &[String], out: &mut Vec<Term>) {
        for c in term::children(t) {
            if !is_var(c, vars) && !term::is_literal(c) && covers(c, vars) {
                out.push(c.clone());
            }
            parts(c, vars, out);
        }
    }
    let mut out = Vec::new();
    for side in [lhs, rhs] {
        if !is_var(side, vars) && !term::is_literal(side) && covers(side, vars) {
            out.push(side.clone());
        }
    }
    // The smallest part that names every variable, as a pattern of its
    // own: a law about `f(m, acc)` applies wherever the table holds such a
    // call, also where the side around it is not there yet.
    let mut inner = Vec::new();
    if inner_too || out.is_empty() {
        for side in [lhs, rhs] {
            parts(side, vars, &mut inner);
        }
    }
    inner.sort_by_key(|t| format!("{t:?}").len());
    if let Some(small) = inner.into_iter().next()
        && !out.contains(&small)
    {
        out.push(small);
    }
    let mut celled = Vec::new();
    for t in &out {
        if let Some((_, cells)) = super::eval::as_cells(t)
            && !out.contains(&cells)
        {
            celled.push(cells);
        }
    }
    out.extend(celled);
    out
}

impl Table {
    fn new() -> Self {
        let mut t = Table {
            nodes: Vec::new(),
            exact: HashMap::new(),
            sig: HashMap::new(),
            by_key: HashMap::new(),
            parent: Vec::new(),
            members: Vec::new(),
            uses: Vec::new(),
            edge: Vec::new(),
            pending: Vec::new(),
            fired: HashSet::new(),
            opened: Vec::new(),
            seen: HashSet::new(),
            conds: Vec::new(),
            rewrites: Vec::new(),
            instances: 0,
            unions: 0,
            full: false,
            t: 0,
            f: 0,
            names: std::cell::Cell::new(0),
            gen_now: 0,
            guards: Vec::new(),
            decided_at: HashMap::new(),
        };
        t.t = t.add(&term::boolean(true)).expect("room for true");
        t.f = t.add(&term::boolean(false)).expect("room for false");
        t
    }

    /// A hypothesis name for a case an explanation opens.
    fn fresh(&self) -> String {
        let n = self.names.get() + 1;
        self.names.set(n);
        format!("h_table{n}")
    }

    fn root(&self, mut a: Id) -> Id {
        while self.parent[a] != a {
            a = self.parent[a];
        }
        a
    }

    fn term(&self, a: Id) -> &Term {
        &self.nodes[a].term
    }

    fn signature(&self, a: Id) -> (String, Vec<Id>) {
        let n = &self.nodes[a];
        (n.key.clone(), n.kids.iter().map(|k| self.root(*k)).collect())
    }

    /// The entry for `t`, added with its parts where it is new; `None` once
    /// the table is full.
    fn add(&mut self, t: &Term) -> Option<Id> {
        let t = canon(t);
        self.add_canon(&t)
    }

    fn add_canon(&mut self, t: &Term) -> Option<Id> {
        let (key, has_parts) = head(t);
        let mut kids = Vec::new();
        if has_parts {
            for c in term::children(t) {
                kids.push(self.add_canon(c)?);
            }
        }
        let exact = (key, kids);
        if let Some(&n) = self.exact.get(&exact) {
            return Some(n);
        }
        if self.nodes.len() >= MAX_NODES {
            self.full = true;
            return None;
        }
        let (key, kids) = exact;
        let id = self.nodes.len();
        self.nodes.push(Node {
            key: key.clone(),
            kids: kids.clone(),
            term: t.clone(),
            depth: self.gen_now,
        });
        self.parent.push(id);
        self.members.push(vec![id]);
        self.uses.push(Vec::new());
        self.edge.push(None);
        self.opened.push(false);
        self.by_key.entry(key.clone()).or_default().push(id);
        self.exact.insert((key, kids.clone()), id);
        for k in &kids {
            let r = self.root(*k);
            if !self.uses[r].contains(&id) {
                self.uses[r].push(id);
            }
        }
        let s = self.signature(id);
        match self.sig.get(&s).copied() {
            Some(m) if self.signature(m) == s => self.pending.push((id, m, Why::Congr)),
            _ => {
                self.sig.insert(s, id);
            }
        }
        Some(id)
    }

    /// The entry for `t` if the table holds it already.
    fn find(&self, t: &Term) -> Option<Id> {
        let t = term::canon_ref(t);
        self.find_canon(&t)
    }

    fn find_canon(&self, t: &Term) -> Option<Id> {
        let (key, has_parts) = head(t);
        let mut kids = Vec::new();
        if has_parts {
            for c in term::children(t) {
                kids.push(self.find_canon(c)?);
            }
        }
        self.exact.get(&(key, kids)).copied()
    }

    fn merge(&mut self, a: Id, b: Id, why: Why) {
        if self.root(a) != self.root(b) {
            self.pending.push((a, b, why));
        }
    }

    /// Point the proof tree of `a` at `a`.
    fn reroot(&mut self, a: Id) {
        let mut cur = a;
        let mut carried: Option<(Id, Why, bool)> = None;
        loop {
            let old = self.edge[cur].take();
            self.edge[cur] = carried;
            match old {
                None => break,
                Some((next, why, fwd)) => {
                    carried = Some((cur, why, !fwd));
                    cur = next;
                }
            }
        }
    }

    fn process(&mut self) {
        while let Some((a, b, why)) = self.pending.pop() {
            let (ra, rb) = (self.root(a), self.root(b));
            if ra == rb {
                continue;
            }
            self.reroot(a);
            self.edge[a] = Some((b, why, true));
            let (small, big) = if self.members[ra].len() < self.members[rb].len() {
                (ra, rb)
            } else {
                (rb, ra)
            };
            self.parent[small] = big;
            let moved = std::mem::take(&mut self.members[small]);
            self.members[big].extend(moved);
            let users = std::mem::take(&mut self.uses[small]);
            for &u in &users {
                let s = self.signature(u);
                match self.sig.get(&s).copied() {
                    Some(m) if m != u && self.signature(m) == s => {
                        if self.root(m) != self.root(u) {
                            self.pending.push((u, m, Why::Congr));
                        }
                    }
                    _ => {
                        self.sig.insert(s, u);
                    }
                }
            }
            for u in users {
                if !self.uses[big].contains(&u) {
                    self.uses[big].push(u);
                }
            }
            self.unions += 1;
        }
    }

    fn same(&self, a: Id, b: Id) -> bool {
        self.root(a) == self.root(b)
    }

    /// The Bool value the entry of `a` is joined with.
    fn bool_of(&self, a: Id) -> Option<bool> {
        if self.same(a, self.t) {
            Some(true)
        } else if self.same(a, self.f) {
            Some(false)
        } else {
            None
        }
    }

    /// A literal value among the terms joined with `a`.
    fn value_of(&self, a: Id) -> Option<Id> {
        self.members[self.root(a)].iter().copied().find(|&m| {
            let t = self.term(m);
            term::is_literal(t)
                || matches!(&t.node, ResolvedExpr::List(xs) if xs.is_empty())
        })
    }

    /// The path of joins from `a` up to the root of its proof tree.
    fn path_up(&self, a: Id) -> Vec<Id> {
        let mut out = vec![a];
        let mut cur = a;
        while let Some((next, _, _)) = &self.edge[cur] {
            cur = *next;
            out.push(cur);
        }
        out
    }

    /// `term(a) = term(b)` for two joined entries.
    fn explain(&self, a: Id, b: Id) -> Option<Proof> {
        if a == b {
            return Some(Proof::Refl(self.term(a).clone()));
        }
        let pa = self.path_up(a);
        let pb = self.path_up(b);
        let lca = *pa.iter().find(|x| pb.contains(x))?;
        let mut steps = Vec::new();
        for w in pa.windows(2) {
            if w[0] == lca {
                break;
            }
            steps.push((self.term(w[1]).clone(), self.edge_proof(w[0])?));
        }
        let mut back = Vec::new();
        for w in pb.windows(2) {
            if w[0] == lca {
                break;
            }
            back.push((w[0], w[1]));
        }
        for (x, y) in back.into_iter().rev() {
            let _ = y;
            let p = self.edge_proof(x)?;
            steps.push((self.term(x).clone(), Proof::Symm(Box::new(p))));
        }
        Some(chain(self.term(a), steps))
    }

    /// `term(x) = term(next)` for the join from `x` to the next entry up.
    fn edge_proof(&self, x: Id) -> Option<Proof> {
        let (y, why, fwd) = self.edge[x].clone()?;
        if let Why::Congr = why {
            return self.congr(x, y);
        }
        let (from, to) = if fwd { (x, y) } else { (y, x) };
        let p = self.realize(&why, from, to)?;
        Some(if fwd { p } else { Proof::Symm(Box::new(p)) })
    }

    /// `term(x) = term(y)` for two entries of one head whose parts are
    /// joined: one congruence step per part that differs.
    fn congr(&self, x: Id, y: Id) -> Option<Proof> {
        let (kx, ky) = (self.nodes[x].kids.clone(), self.nodes[y].kids.clone());
        if kx.len() != ky.len() {
            return None;
        }
        let mut cur = self.term(x).clone();
        let mut steps = Vec::new();
        for i in 0..kx.len() {
            if kx[i] == ky[i] {
                continue;
            }
            let inner = self.explain(kx[i], ky[i])?;
            let ctx = term::with_child(&cur, i, term::hole());
            let next = term::with_child(&cur, i, self.term(ky[i]).clone());
            steps.push((
                next.clone(),
                Proof::Congr {
                    ctx,
                    inner: Box::new(inner),
                },
            ));
            cur = next;
        }
        Some(chain(self.term(x), steps))
    }

    fn prem(&self, p: &Prem) -> Option<Proof> {
        match p {
            Prem::Done(p) => Some(p.clone()),
            Prem::Same(a, b) => self.explain(*a, *b),
        }
    }

    /// The subject of an arm equal to its pattern: the premise, then a
    /// cell step where the value is a list literal.
    fn arm_premise(&self, p: &Prem, cell: bool) -> Option<Proof> {
        let base = self.prem(p)?;
        if !cell {
            return Some(base);
        }
        let Prem::Same(a, b) = p else {
            return None;
        };
        let list = self.term(*b).clone();
        let to = term::cell_of(&list)?;
        Some(Proof::Trans {
            terms: vec![self.term(*a).clone(), list.clone(), to],
            steps: vec![base, Proof::Cell { list }],
        })
    }

    /// `true = false`, from `l = r` and their values.
    fn opposite_values(&self, l: Id, r: Id, l_to_r: Proof) -> Option<Proof> {
        let lv = self.bool_of(l)?;
        let rv = self.bool_of(r)?;
        if lv == rv {
            return None;
        }
        let (lit_l, lit_r) = (
            if lv { self.t } else { self.f },
            if rv { self.t } else { self.f },
        );
        let p = Proof::Trans {
            terms: vec![
                term::boolean(lv),
                self.term(l).clone(),
                self.term(r).clone(),
                term::boolean(rv),
            ],
            steps: vec![
                self.explain(lit_l, l)?,
                l_to_r,
                self.explain(r, lit_r)?,
            ],
        };
        Some(if lv { p } else { Proof::Symm(Box::new(p)) })
    }

    fn realize(&self, why: &Why, from: Id, to: Id) -> Option<Proof> {
        let _ = to;
        Some(match why {
            Why::Given(p) => p.clone(),
            Why::Congr => return self.congr(from, to),
            Why::Rule {
                rule,
                subst,
                premises,
            } => Proof::Rule {
                rule: *rule,
                subst: subst.clone(),
                premises: premises
                    .iter()
                    .map(|p| self.prem(p))
                    .collect::<Option<_>>()?,
            },
            Why::Law {
                law,
                subst,
                premise,
            } => Proof::Law {
                law: law.clone(),
                subst: subst.clone(),
                premise: match premise {
                    Some(p) => Some(Box::new(self.prem(p)?)),
                    None => None,
                },
            },
            Why::Unfold {
                fn_id,
                arm,
                args,
                binders,
                premise,
                cell,
            } => Proof::Unfold {
                fn_id: *fn_id,
                arm: *arm,
                args: args.clone(),
                binders: binders.clone(),
                premise: Some(Box::new(self.arm_premise(premise, *cell)?)),
            },
            Why::Arm {
                term: t,
                arm,
                binders,
                premise,
                cell,
            } => Proof::Arm {
                term: t.clone(),
                arm: *arm,
                binders: binders.clone(),
                premise: Box::new(self.arm_premise(premise, *cell)?),
            },
            Why::WhenFalse {
                when,
                value,
                step,
                subst,
                l,
                r,
            } => {
                let h = self.fresh();
                let applied = match step {
                    Step::Law(law) => Proof::Law {
                        law: law.clone(),
                        subst: subst.clone(),
                        premise: Some(Box::new(Proof::Hyp(h.clone()))),
                    },
                    Step::Wall(rule) => Proof::Rule {
                        rule: *rule,
                        subst: subst.clone(),
                        premises: vec![Proof::Hyp(h.clone())],
                    },
                };
                let contradiction = self.opposite_values(*l, *r, applied)?;
                let absurd = Box::new(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: when.clone(),
                    rhs: term::boolean(!value),
                });
                let hyp = Box::new(Proof::Hyp(h.clone()));
                let (if_true, if_false) = if *value { (absurd, hyp) } else { (hyp, absurd) };
                Proof::Cases {
                    on: when.clone(),
                    hyp: h,
                    if_true,
                    if_false,
                }
            }
            Why::OrFalse { or, falsity, left } => {
                let ot = self.term(*or).clone();
                let args = builtin_args(&ot, "Bool.or", 2)?.to_vec();
                let side = if *left { &args[0] } else { &args[1] };
                let h = self.fresh();
                let truth = term::boolean(true);
                let (with_true, rule, ctx, subst) = if *left {
                    (
                        term::bool_or(truth.clone(), args[1].clone()),
                        WallRule::OrTrueL,
                        term::bool_or(term::hole(), args[1].clone()),
                        vec![("b".to_string(), args[1].clone())],
                    )
                } else {
                    (
                        term::bool_or(args[0].clone(), truth.clone()),
                        WallRule::OrTrueR,
                        term::bool_or(args[0].clone(), term::hole()),
                        vec![("a".to_string(), args[0].clone())],
                    )
                };
                // true = Bool.or(true, b) = Bool.or(a, b) = false.
                let contradiction = Proof::Trans {
                    terms: vec![
                        truth.clone(),
                        canon(&with_true),
                        ot.clone(),
                        term::boolean(false),
                    ],
                    steps: vec![
                        Proof::Symm(Box::new(Proof::Rule {
                            rule,
                            subst,
                            premises: Vec::new(),
                        })),
                        Proof::Congr {
                            ctx,
                            inner: Box::new(Proof::Symm(Box::new(Proof::Hyp(h.clone())))),
                        },
                        self.explain(*or, *falsity)?,
                    ],
                };
                Proof::Cases {
                    on: canon(side),
                    hyp: h.clone(),
                    if_true: Box::new(Proof::Absurd {
                        contradiction: Box::new(contradiction),
                        lhs: canon(side),
                        rhs: term::boolean(false),
                    }),
                    if_false: Box::new(Proof::Hyp(h)),
                }
            }
            Why::NotElim { not, value } => {
                let nt = self.term(*not).clone();
                let a = canon(&builtin_args(&nt, "Bool.not", 1)?[0]);
                let v = self.bool_of(*value)?;
                let h = self.fresh();
                // In the case `a = v`: `Bool.not(a) = Bool.not(v) = !v`, and
                // `Bool.not(a) = v`.
                let not_v = canon(&term::bool_not(term::boolean(v)));
                let at_v = Proof::Trans {
                    terms: vec![nt.clone(), not_v.clone(), term::boolean(!v)],
                    steps: vec![
                        Proof::Congr {
                            ctx: term::bool_not(term::hole()),
                            inner: Box::new(Proof::Hyp(h.clone())),
                        },
                        Proof::Compute {
                            lhs: not_v,
                            rhs: term::boolean(!v),
                        },
                    ],
                };
                // v = Bool.not(a) = !v.
                let lit_v = if v { self.t } else { self.f };
                let back = self.explain(lit_v, *not)?;
                let contradiction = Proof::Trans {
                    terms: vec![term::boolean(v), nt.clone(), term::boolean(!v)],
                    steps: vec![back, at_v],
                };
                let contradiction = if v {
                    contradiction
                } else {
                    Proof::Symm(Box::new(contradiction))
                };
                let absurd = Box::new(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: a.clone(),
                    rhs: term::boolean(!v),
                });
                let hyp = Box::new(Proof::Hyp(h.clone()));
                // The case `a = v` is absurd; in the other, `a = !v` is the
                // hypothesis.
                let (if_true, if_false) = if v { (absurd, hyp) } else { (hyp, absurd) };
                Proof::Cases {
                    on: a,
                    hyp: h,
                    if_true,
                    if_false,
                }
            }
        })
    }
}

impl Table {
    /// The joins every entry of a Bool connective, an equality or a closed
    /// computation gives, until none is new (bounded).
    fn saturate(&mut self, env: &mut Env<'_>) {
        for _ in 0..8 {
            self.process();
            let before = (self.unions, self.nodes.len());
            let n = self.nodes.len();
            for id in 0..n {
                self.local_rules(env, id);
                if self.full {
                    break;
                }
            }
            self.gen_now = 0;
            self.process();
            self.retry_conds_cheap();
            self.process();
            if (self.unions, self.nodes.len()) == before || self.full {
                break;
            }
        }
    }

    fn fire(&mut self, id: Id, which: u8) -> bool {
        self.fired.insert((id, which))
    }

    fn local_rules(&mut self, env: &mut Env<'_>, id: Id) {
        self.gen_now = self.nodes[id].depth;
        let t = self.term(id).clone();
        // A list literal with an element is a cell.
        if let ResolvedExpr::List(xs) = &t.node
            && !xs.is_empty()
            && self.fire(id, 0)
            && let Some(cell) = term::cell_of(&t)
            && let Some(c) = self.add(&cell)
        {
            self.merge(id, c, Why::Given(Proof::Cell { list: t.clone() }));
        }
        for (name, and) in [("Bool.and", true), ("Bool.or", false)] {
            let Some(args) = builtin_args(&t, name, 2) else {
                continue;
            };
            let kids = self.nodes[id].kids.clone();
            // A side with a value.
            for (i, left) in [(0usize, true), (1usize, false)] {
                let Some(v) = self.bool_of(kids[i]) else {
                    continue;
                };
                if !self.fire(id, 1 + i as u8) {
                    continue;
                }
                let other = args[1 - i].clone();
                let lit = term::boolean(v);
                let rebuilt = if left {
                    if and {
                        term::bool_and(lit, other.clone())
                    } else {
                        term::bool_or(lit, other.clone())
                    }
                } else if and {
                    term::bool_and(other.clone(), lit)
                } else {
                    term::bool_or(other.clone(), lit)
                };
                let rule = match (and, v, left) {
                    (true, true, true) => WallRule::AndTrueL,
                    (true, false, true) => WallRule::AndFalseL,
                    (false, true, true) => WallRule::OrTrueL,
                    (false, false, true) => WallRule::OrFalseL,
                    (true, true, false) => WallRule::AndTrueR,
                    (true, false, false) => WallRule::AndFalseR,
                    (false, true, false) => WallRule::OrTrueR,
                    (false, false, false) => WallRule::OrFalseR,
                };
                let binder = if left { "b" } else { "a" };
                // `and(true, b) = b`, `and(false, b) = false`, `or(true, b) =
                // true`, `or(false, b) = b`.
                let to = if and == v {
                    self.add(&other)
                } else if and {
                    Some(self.f)
                } else {
                    Some(self.t)
                };
                if let (Some(r), Some(to)) = (self.add(&rebuilt), to) {
                    self.merge(
                        r,
                        to,
                        Why::Rule {
                            rule,
                            subst: vec![(binder.to_string(), canon(&other))],
                            premises: Vec::new(),
                        },
                    );
                }
            }
            // The connective's own value.
            match (and, self.bool_of(id)) {
                (true, Some(true)) if self.fire(id, 3) => {
                    for (i, rule) in [(0usize, WallRule::AndElimL), (1, WallRule::AndElimR)] {
                        self.merge(
                            kids[i],
                            self.t,
                            Why::Rule {
                                rule,
                                subst: vec![
                                    ("a".into(), canon(&args[0])),
                                    ("b".into(), canon(&args[1])),
                                ],
                                premises: vec![Prem::Same(id, self.t)],
                            },
                        );
                    }
                }
                (false, Some(false)) if self.fire(id, 3) => {
                    for (i, left) in [(0usize, true), (1, false)] {
                        self.merge(
                            kids[i],
                            self.f,
                            Why::OrFalse {
                                or: id,
                                falsity: self.f,
                                left,
                            },
                        );
                    }
                }
                _ => {}
            }
        }
        if builtin_args(&t, "Bool.not", 1).is_some()
            && let Some(v) = self.bool_of(id)
            && self.fire(id, 4)
        {
            let kid = self.nodes[id].kids[0];
            let to = if v { self.f } else { self.t };
            let value = if v { self.t } else { self.f };
            self.merge(kid, to, Why::NotElim { not: id, value });
        }
        if let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &t.node {
            let kids = self.nodes[id].kids.clone();
            let equality = is_int(a) || env.beq_is_equality(a);
            if std::env::var_os("AVER_TABLE_DEBUG_EQ").is_some() && !equality {
                eprintln!("table: eq untyped {:?} / {:?}: {}", a.ty(), b.ty(), crate::ir::proof_steps::show::term(&t, env.inputs.symbol_table));
            }
            if equality && self.bool_of(id) == Some(true) && self.fire(id, 5) {
                self.merge(
                    kids[0],
                    kids[1],
                    Why::Rule {
                        rule: WallRule::EqOfBeq,
                        subst: vec![("a".into(), canon(a)), ("b".into(), canon(b))],
                        premises: vec![Prem::Same(id, self.t)],
                    },
                );
            }
            if equality && self.same(kids[0], kids[1]) && self.fire(id, 6) {
                let refl = term::binop(BinOp::Eq, canon(a), canon(a));
                if let Some(r) = self.add(&refl) {
                    self.merge(
                        r,
                        self.t,
                        Why::Rule {
                            rule: WallRule::BeqRefl,
                            subst: vec![("a".into(), canon(a))],
                            premises: Vec::new(),
                        },
                    );
                }
            }
        }
        self.fold(id);
    }

    /// A computation whose parts all have literal values: the term with
    /// those values in place, computed.
    fn fold(&mut self, id: Id) {
        let t = self.term(id).clone();
        if term::is_literal(&t)
            || !matches!(
                t.node,
                ResolvedExpr::BinOp(..)
                    | ResolvedExpr::Neg(_)
                    | ResolvedExpr::Call(ResolvedCallee::Builtin(_), _)
                    | ResolvedExpr::Call(ResolvedCallee::Intrinsic(_), _)
            )
            || self.value_of(id).is_some()
        {
            return;
        }
        let kids = self.nodes[id].kids.clone();
        if kids.is_empty() {
            return;
        }
        let mut closed = t.clone();
        for (i, k) in kids.iter().enumerate() {
            let Some(v) = self.value_of(*k) else {
                return;
            };
            closed = term::with_child(&closed, i, self.term(v).clone());
        }
        let Some(v) = term::eval_closed(&closed) else {
            return;
        };
        let closed = canon(&closed);
        let v = canon(&v);
        if closed == v {
            return;
        }
        if let (Some(c), Some(vi)) = (self.add(&closed), self.add(&v)) {
            self.merge(
                c,
                vi,
                Why::Given(Proof::Compute {
                    lhs: closed,
                    rhs: v,
                }),
            );
        }
    }

    /// Each definition call whose `match` subject has a constructor or a
    /// literal among its joined terms, and each `match` the same way:
    /// opened at that arm.
    fn unfold_round(&mut self, env: &mut Env<'_>) {
        let n = self.nodes.len();
        for id in 0..n {
            if self.opened[id] || self.full || self.nodes[id].depth >= MAX_OPEN_GEN() {
                continue;
            }
            self.gen_now = self.nodes[id].depth + 1;
            let t = self.term(id).clone();
            match &t.node {
                ResolvedExpr::Call(ResolvedCallee::Fn(fid), args) => {
                    let Some(def) = env.def(*fid) else {
                        continue;
                    };
                    let Ok(outer) = def.outer(args) else {
                        continue;
                    };
                    let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                        if env.inputs.recursive_fns.contains(fid) {
                            continue;
                        }
                        self.opened[id] = true;
                        let Ok(body) = term::subst(&def.body, &outer) else {
                            continue;
                        };
                        if let Some(b) = self.add(&body) {
                            self.merge(
                                id,
                                b,
                                Why::Given(Proof::Unfold {
                                    fn_id: *fid,
                                    arm: 0,
                                    args: args.iter().map(canon).collect(),
                                    binders: Vec::new(),
                                    premise: None,
                                }),
                            );
                        }
                        continue;
                    };
                    let Ok(s) = term::subst(subject, &outer) else {
                        continue;
                    };
                    let Some(sid) = self.add(&s) else {
                        continue;
                    };
                    let Some((m, k, binders, cell)) = self.pick_arm(arms, sid) else {
                        self.note_guard(sid);
                        continue;
                    };
                    let Ok((_, body)) = crate::ir::proof_steps::claim::arm_equation(
                        subject, arms, k, &outer, &binders,
                    ) else {
                        continue;
                    };
                    self.opened[id] = true;
                    if let Some(b) = self.add(&body) {
                        self.merge(
                            id,
                            b,
                            Why::Unfold {
                                fn_id: *fid,
                                arm: k,
                                args: args.iter().map(canon).collect(),
                                binders,
                                premise: Prem::Same(sid, m),
                                cell,
                            },
                        );
                    }
                }
                ResolvedExpr::Match { subject, arms } => {
                    let sid = self.nodes[id].kids[0];
                    let Some((m, k, binders, cell)) = self.pick_arm(arms, sid) else {
                        self.note_guard(sid);
                        continue;
                    };
                    let _ = subject;
                    let Ok((_, body)) =
                        crate::ir::proof_steps::claim::arm_equation(subject, arms, k, &[], &binders)
                    else {
                        continue;
                    };
                    self.opened[id] = true;
                    if let Some(b) = self.add(&body) {
                        self.merge(
                            id,
                            b,
                            Why::Arm {
                                term: t.clone(),
                                arm: k,
                                binders,
                                premise: Prem::Same(sid, m),
                                cell,
                            },
                        );
                    }
                }
                _ => {}
            }
        }
        self.gen_now = 0;
        self.process();
    }

    /// A subject that is an Int comparison: linear arithmetic may decide it.
    fn note_guard(&mut self, sid: Id) {
        if int_cmp(self.term(sid)).is_some() && !self.guards.contains(&sid) {
            self.guards.push(sid);
        }
    }

    /// The arm a term joined with the subject `sid` selects, with that
    /// term, the arm's binders and whether it is read as a cell.
    fn pick_arm(
        &self,
        arms: &[crate::ir::hir::ResolvedMatchArm],
        sid: Id,
    ) -> Option<(Id, u32, Vec<Term>, bool)> {
        for &m in &self.members[self.root(sid)] {
            let v = self.term(m);
            let decisive = term::is_literal(v)
                || matches!(
                    v.node,
                    ResolvedExpr::Ctor(..) | ResolvedExpr::List(_) | ResolvedExpr::RecordCreate { .. }
                )
                || builtin_args(v, "List.prepend", 2).is_some();
            if !decisive {
                continue;
            }
            if let Some((k, binders, cell)) = super::eval::select_arm(arms, v, &[]) {
                return Some((m, k, binders, cell));
            }
        }
        None
    }

    /// The cited laws, the builtin facts and the wall rules over lists the
    /// table matches.
    fn load_rewrites(&mut self, env: &Env<'_>) {
        let mut keys: Vec<String> = Vec::new();
        let laws = env
            .cited_all
            .iter()
            .chain(env.rewrite_laws.iter())
            .chain(env.fact_rules.iter())
            .chain(env.auto_laws.iter());
        for law in laws {
            if keys.contains(&law.key) {
                continue;
            }
            keys.push(law.key.clone());
            let (lhs, rhs) = (canon(&law.lhs), canon(&law.rhs));
            let triggers = triggers(&lhs, &rhs, &law.givens, law.fact.is_none());
            if triggers.is_empty() {
                continue;
            }
            self.rewrites.push(Rewrite {
                kind: RewriteKind::Law(Box::new(law.clone())),
                vars: law.givens.clone(),
                lhs,
                rhs,
                triggers,
            });
        }
        for rule in WALL {
            let (premises, concl) = rule.schema();
            let vars: Vec<String> = rule.binders().iter().map(|s| s.to_string()).collect();
            let (lhs, rhs) = (canon(&concl.lhs), canon(&concl.rhs));
            let mut triggers = triggers(&lhs, &rhs, &vars, false);
            // A rule is read left to right, at its whole left side only.
            let cells = super::eval::as_cells(&lhs).map(|(_, c)| c);
            triggers.retain(|t| *t == lhs || Some(t) == cells.as_ref());
            if triggers.is_empty() {
                continue;
            }
            self.rewrites.push(Rewrite {
                kind: RewriteKind::Wall(*rule, premises),
                vars,
                lhs,
                rhs,
                triggers,
            });
        }
    }

    /// The ways `pat` matches the terms joined with `c`, extending `bound`.
    fn ematch(
        &self,
        pat: &Term,
        c: Id,
        vars: &[String],
        bound: Vec<(String, Id)>,
        out: &mut Vec<Vec<(String, Id)>>,
    ) {
        if out.len() >= 16 {
            return;
        }
        if let ResolvedExpr::Ident(n) = &pat.node
            && vars.contains(n)
        {
            match bound.iter().find(|(k, _)| k == n) {
                Some((_, b)) => {
                    if self.same(*b, c) {
                        out.push(bound);
                    }
                }
                None => {
                    let mut b = bound;
                    b.push((n.clone(), c));
                    out.push(b);
                }
            }
            return;
        }
        let (key, has_parts) = head(pat);
        let pkids: Vec<&Term> = if has_parts {
            term::children(pat)
        } else {
            Vec::new()
        };
        for &m in &self.members[self.root(c)] {
            if self.nodes[m].key != key || self.nodes[m].kids.len() != pkids.len() {
                continue;
            }
            self.ematch_kids(&pkids, &self.nodes[m].kids, vars, bound.clone(), out);
        }
    }

    fn ematch_kids(
        &self,
        pkids: &[&Term],
        kids: &[Id],
        vars: &[String],
        bound: Vec<(String, Id)>,
        out: &mut Vec<Vec<(String, Id)>>,
    ) {
        let Some((p, rest)) = pkids.split_first() else {
            out.push(bound);
            return;
        };
        let mut here = Vec::new();
        self.ematch(p, kids[0], vars, bound, &mut here);
        for b in here {
            self.ematch_kids(rest, &kids[1..], vars, b, out);
        }
    }

    /// One round of matching every law and rule against the table.
    fn match_round(&mut self, env: &mut Env<'_>) {
        // Every match first, then the instances nearest the start first,
        // so no law takes the whole budget from the others.
        let mut found: Vec<(u8, usize, Vec<(String, Id)>)> = Vec::new();
        for ri in 0..self.rewrites.len() {
            let triggers = self.rewrites[ri].triggers.clone();
            let vars = self.rewrites[ri].vars.clone();
            let wall = matches!(self.rewrites[ri].kind, RewriteKind::Wall(..));
            let mut mine = 0;
            for trig in &triggers {
                let (key, has_parts) = head(trig);
                let pkids: Vec<&Term> = if has_parts {
                    term::children(trig)
                } else {
                    Vec::new()
                };
                let cands = self.by_key.get(&key).cloned().unwrap_or_default();
                for n in cands {
                    if self.nodes[n].kids.len() != pkids.len() || self.nodes[n].depth >= MAX_GEN() {
                        continue;
                    }
                    let mut here = Vec::new();
                    let kids = self.nodes[n].kids.clone();
                    self.ematch_kids(&pkids, &kids, &vars, Vec::new(), &mut here);
                    for b in here {
                        let g = b
                            .iter()
                            .map(|(_, i)| self.nodes[*i].depth)
                            .chain([self.nodes[n].depth])
                            .max()
                            .unwrap_or(0);
                        if g >= MAX_GEN() && !wall {
                            continue;
                        }
                        mine += 1;
                        found.push((g, ri, b));
                    }
                    if mine >= 32 {
                        break;
                    }
                }
            }
        }
        found.sort_by_key(|(g, _, _)| *g);
        for (g, ri, b) in found {
            if self.instances >= MAX_INSTANCES || self.full {
                break;
            }
            let wall = matches!(self.rewrites[ri].kind, RewriteKind::Wall(..));
            self.gen_now = if wall { g } else { g + 1 };
            self.instance(env, ri, &b);
        }
        self.gen_now = 0;
        self.process();
    }

    /// Add the instance of rewrite `ri` at `bound`, or note it until its
    /// `when` is shown.
    fn instance(&mut self, env: &mut Env<'_>, ri: usize, bound: &[(String, Id)]) {
        let vars = self.rewrites[ri].vars.clone();
        let Some(ids) = vars
            .iter()
            .map(|v| bound.iter().find(|(k, _)| k == v).map(|(_, id)| *id))
            .collect::<Option<Vec<Id>>>()
        else {
            return;
        };
        let roots: Vec<Id> = ids.iter().map(|i| self.root(*i)).collect();
        if !self.seen.insert((ri, roots)) {
            return;
        }
        let subst: Vec<(String, Term)> = vars
            .iter()
            .cloned()
            .zip(ids.iter().map(|i| self.term(*i).clone()))
            .collect();
        let (Ok(l), Ok(r)) = (
            term::subst(&self.rewrites[ri].lhs, &subst),
            term::subst(&self.rewrites[ri].rhs, &subst),
        ) else {
            return;
        };
        let (l, r) = (canon(&l), canon(&r));
        match &self.rewrites[ri].kind {
            RewriteKind::Law(law) => {
                let law = (**law).clone();
                if law.fact.is_some() && !env.equality_types_fit(&law, &subst) {
                    return;
                }
                let when = match &law.premise {
                    None => None,
                    Some(w) => {
                        let Ok(w) = term::subst(w, &subst) else {
                            return;
                        };
                        Some(canon(&w))
                    }
                };
                self.instances += 1;
                let (Some(li), Some(rii)) = (self.add(&l), self.add(&r)) else {
                    return;
                };
                match when {
                    None => self.merge(
                        li,
                        rii,
                        Why::Law {
                            law: law.key.clone(),
                            subst,
                            premise: None,
                        },
                    ),
                    Some(w) => {
                        if let Some(c) = term::eval_closed(&w)
                            && term::bool_value(&c) == Some(false)
                        {
                            return;
                        }
                        // A law over a given of a refined type proves its
                        // premise line by line; the table proves it as one.
                        if !law.refined.is_empty() {
                            if let Some(p) = env.discharge_when(&law, &w) {
                                self.merge(
                                    li,
                                    rii,
                                    Why::Law {
                                        law: law.key.clone(),
                                        subst,
                                        premise: Some(Prem::Done(p)),
                                    },
                                );
                            }
                            return;
                        }
                        // In the table, so that what decides it joins it
                        // with a value.
                        let _ = self.add(&w);
                        self.conds.push(Cond {
                            step: Step::Law(law.key.clone()),
                            subst,
                            l: li,
                            r: rii,
                            when: w,
                            value: true,
                            done: false,
                        });
                    }
                }
            }
            RewriteKind::Wall(rule, premises) => {
                let rule = *rule;
                let premises = premises.clone();
                let mut proofs = Vec::new();
                for p in premises.clone() {
                    let (Ok(pl), Ok(pr)) =
                        (term::subst(&p.lhs, &subst), term::subst(&p.rhs, &subst))
                    else {
                        return;
                    };
                    let (pl, pr) = (canon(&pl), canon(&pr));
                    let Some(v) = term::bool_value(&pr) else {
                        return;
                    };
                    if let Some(c) = term::eval_closed(&pl) {
                        if term::bool_value(&c) != Some(v) {
                            return;
                        }
                        proofs.push(Prem::Done(Proof::Compute { lhs: pl, rhs: pr }));
                        continue;
                    }
                    let Some(pi) = self.add(&pl) else {
                        return;
                    };
                    self.process();
                    if self.bool_of(pi) == Some(v) {
                        proofs.push(Prem::Same(pi, if v { self.t } else { self.f }));
                        continue;
                    }
                    if int_cmp(&pl).is_some()
                        && let Some(p) = env.linear_proof(&pl, v)
                    {
                        proofs.push(Prem::Done(p));
                        continue;
                    }
                    // Not shown now: noted, for when the premise is known
                    // or the sides are known apart. Only a rule with one
                    // premise; another is tried again next round.
                    if premises.len() != 1 {
                        let roots: Vec<Id> = ids.iter().map(|i| self.root(*i)).collect();
                        self.seen.remove(&(ri, roots));
                        return;
                    }
                    if let Some(li) = self.add(&l)
                        && let Some(rii) = self.add(&r)
                    {
                        self.instances += 1;
                        self.conds.push(Cond {
                            step: Step::Wall(rule),
                            subst: subst.clone(),
                            l: li,
                            r: rii,
                            when: pl,
                            value: v,
                            done: false,
                        });
                    }
                    return;
                }
                self.instances += 1;
                if rule == WallRule::NeDef && !(is_int(&subst[0].1) && is_int(&subst[1].1)) {
                    return;
                }
                let (Some(li), Some(rii)) = (self.add(&l), self.add(&r)) else {
                    return;
                };
                self.merge(
                    li,
                    rii,
                    Why::Rule {
                        rule,
                        subst,
                        premises: proofs,
                    },
                );
            }
        }
    }

    /// The noted instances whose premise the table now holds.
    fn retry_conds_cheap(&mut self) {
        for i in 0..self.conds.len() {
            if self.conds[i].done {
                continue;
            }
            let w = self.conds[i].when.clone();
            let Some(wi) = self.find(&w) else {
                continue;
            };
            let v = self.conds[i].value;
            if self.bool_of(wi) == Some(v) {
                self.conds[i].done = true;
                let lit = if v { self.t } else { self.f };
                let why = self.conds[i].why(Prem::Same(wi, lit));
                let (l, r) = (self.conds[i].l, self.conds[i].r);
                self.merge(l, r, why);
            }
        }
    }

    /// The noted instances again, with their premise added to the table,
    /// then a law's `when` proved by linear arithmetic over what the table
    /// knows; one whose sides the table holds as different Bool values has
    /// its premise the other way round.
    fn retry_conds_deep(&mut self, env: &mut Env<'_>) {
        for i in 0..self.conds.len() {
            if self.conds[i].done || self.full {
                continue;
            }
            let w = self.conds[i].when.clone();
            let v = self.conds[i].value;
            let Some(wi) = self.add(&w) else {
                continue;
            };
            self.saturate(env);
            if self.conds[i].done {
                continue;
            }
            let (l, r) = (self.conds[i].l, self.conds[i].r);
            if v && let Some(p) = self.prove_true(env, &w, 2) {
                self.conds[i].done = true;
                let why = self.conds[i].why(Prem::Done(p));
                self.merge(l, r, why);
                self.process();
                continue;
            }
            if let (Some(lv), Some(rv)) = (self.bool_of(l), self.bool_of(r))
                && lv != rv
                && self.bool_of(wi).is_none()
            {
                self.conds[i].done = true;
                let why = Why::WhenFalse {
                    when: w,
                    value: v,
                    step: self.conds[i].step.clone(),
                    subst: self.conds[i].subst.clone(),
                    l,
                    r,
                };
                let other = if v { self.f } else { self.t };
                self.merge(wi, other, why);
                self.process();
            }
        }
    }

    /// `w = true`, by the table, conjunct by conjunct, or by linear
    /// arithmetic over what the table knows.
    fn prove_true(&mut self, env: &mut Env<'_>, w: &Term, depth: usize) -> Option<Proof> {
        let w = canon(w);
        if let Some(c) = term::eval_closed(&w)
            && term::bool_value(&c) == Some(true)
        {
            return Some(Proof::Compute {
                lhs: w,
                rhs: term::boolean(true),
            });
        }
        if let Some(wi) = self.add(&w) {
            self.process();
            if self.bool_of(wi) == Some(true) {
                return self.explain(wi, self.t);
            }
            if self.bool_of(wi) == Some(false) {
                return None;
            }
        }
        if let Some(args) = builtin_args(&w, "Bool.and", 2) {
            let (a, b) = (canon(&args[0]), canon(&args[1]));
            let pa = self.prove_true(env, &a, depth)?;
            let pb = self.prove_true(env, &b, depth)?;
            return Some(super::rewrite::both(&w, &b, pa, pb));
        }
        if int_cmp(&w).is_some() && depth > 0 {
            return self.with_facts(env, &w, &term::boolean(true), |env, _| {
                env.linear_proof(&w, true)
                    .or_else(|| decide_with_diseqs(env, &w, true))
            });
        }
        None
    }

    /// The comparisons the table holds true or false, with why, and each
    /// Int part of them the table holds equal to a literal, as two orders.
    fn facts(&self, env: &Env<'_>, goal: &Term) -> Vec<Fact> {
        let mut out: Vec<Fact> = Vec::new();
        for lit in [self.t, self.f] {
            let value = lit == self.t;
            for &m in &self.members[self.root(lit)] {
                let t = self.term(m);
                if int_cmp(t).is_none() {
                    continue;
                }
                if env
                    .hyp_for(t)
                    .is_some_and(|(_, v)| term::bool_value(&v) == Some(value))
                {
                    continue;
                }
                out.push(Fact {
                    term: t.clone(),
                    value,
                    why: Prem::Same(m, lit),
                });
            }
        }
        // Int parts joined with a literal: `x <= k` and `x >= k`.
        let mut parts = Vec::new();
        let mut cmps: Vec<Term> = out.iter().map(|f| f.term.clone()).collect();
        cmps.extend(
            env.hyps
                .iter()
                .filter(|(_, e)| int_cmp(&e.lhs).is_some())
                .map(|(_, e)| canon(&e.lhs)),
        );
        cmps.push(canon(goal));
        for c in &cmps {
            if int_cmp(c).is_some() {
                for s in term::children(c) {
                    atoms(s, &mut parts);
                }
            }
        }
        for s in parts {
            let Some(si) = self.find(&s) else {
                continue;
            };
            let Some(vi) = self.value_of(si) else {
                // Another Int term the part is joined with, written with
                // arithmetic (`magnitudeOf(v)` with `0 - v`): two orders
                // between them.
                let other = self.members[self.root(si)].iter().copied().find(|&m| {
                    m != si
                        && is_int(self.term(m))
                        && matches!(
                            self.term(m).node,
                            ResolvedExpr::BinOp(BinOp::Add | BinOp::Sub | BinOp::Mul, ..)
                                | ResolvedExpr::Ident(_)
                        )
                });
                if let Some(u) = other {
                    for op in [BinOp::Lte, BinOp::Gte] {
                        out.push(Fact {
                            term: canon(&term::binop(op, s.clone(), self.term(u).clone())),
                            value: true,
                            why: Prem::Same(si, u),
                        });
                    }
                }
                continue;
            };
            let k = self.term(vi).clone();
            if term::int_value(&k).is_none() {
                continue;
            }
            for op in [BinOp::Lte, BinOp::Gte] {
                // `x op k = k op k = true`, by `x = k` (see `fact_proof`).
                out.push(Fact {
                    term: canon(&term::binop(op, s.clone(), k.clone())),
                    value: true,
                    why: Prem::Same(si, vi),
                });
            }
        }
        out.truncate(MAX_FACTS);
        out
    }

    /// The proof of a fact, `term = value`.
    fn fact_proof(&self, f: &Fact) -> Option<Proof> {
        let lit = if f.value { self.t } else { self.f };
        match &f.why {
            Prem::Done(p) => Some(p.clone()),
            Prem::Same(a, b) if *b == lit => self.explain(*a, *b),
            // `x op k`: congruence from `x = k`, then compute `k op k`.
            Prem::Same(a, b) => {
                let ResolvedExpr::BinOp(op, _, _) = &f.term.node else {
                    return None;
                };
                let k = self.term(*b).clone();
                let at_k = canon(&term::binop(*op, k.clone(), k.clone()));
                Some(Proof::Trans {
                    terms: vec![f.term.clone(), at_k.clone(), term::boolean(true)],
                    steps: vec![
                        Proof::Congr {
                            ctx: term::binop(*op, term::hole(), k),
                            inner: Box::new(self.explain(*a, *b)?),
                        },
                        if term::is_literal(self.term(*b)) {
                            Proof::Compute {
                                lhs: at_k,
                                rhs: term::boolean(true),
                            }
                        } else {
                            crate::ir::proof_steps::linear::from_hyps(&at_k, true, &[])?
                        },
                    ],
                })
            }
        }
    }

    /// `gl = gr` by `prove`, with the facts of the table in scope as
    /// hypotheses; each fact the proof reads is cut in before it.
    fn with_facts(
        &mut self,
        env: &mut Env<'_>,
        gl: &Term,
        gr: &Term,
        prove: impl FnOnce(&mut Env<'_>, &[String]) -> Option<Proof>,
    ) -> Option<Proof> {
        let facts = self.facts(env, gl);
        let base = env.hyps.len();
        let mut names = Vec::new();
        for f in &facts {
            let name = env.fresh_hyp();
            env.hyps
                .push((name.clone(), Eqn::new(f.term.clone(), term::boolean(f.value))));
            names.push(name);
        }
        let found = prove(env, &names);
        env.hyps.truncate(base);
        let mut body = found?;
        for (f, name) in facts.iter().zip(&names).rev() {
            if !super::uses_hyp(&body, name) {
                continue;
            }
            let proof = self.fact_proof(f)?;
            body = if f.value {
                Proof::Have {
                    name: name.clone(),
                    fact: f.term.clone(),
                    proof: Box::new(proof),
                    body: Box::new(body),
                }
            } else {
                let contradiction = Proof::Trans {
                    terms: vec![term::boolean(true), f.term.clone(), term::boolean(false)],
                    steps: vec![Proof::Symm(Box::new(Proof::Hyp(name.clone()))), proof],
                };
                Proof::Cases {
                    on: f.term.clone(),
                    hyp: name.clone(),
                    if_true: Box::new(Proof::Absurd {
                        contradiction: Box::new(contradiction),
                        lhs: gl.clone(),
                        rhs: gr.clone(),
                    }),
                    if_false: Box::new(body),
                }
            };
        }
        Some(body)
    }

    /// Each Int comparison the table holds no value for, decided by linear
    /// arithmetic over what it knows.
    fn decide_round(&mut self, env: &mut Env<'_>) {
        let ids: Vec<Id> = (0..self.nodes.len())
            .filter(|&id| int_cmp(self.term(id)).is_some())
            .collect();
        self.decide(env, &ids, MAX_DECIDE);
    }

    /// Each of `ids` the table holds no value for, decided by linear
    /// arithmetic over what it knows, `most` at a time.
    fn decide(&mut self, env: &mut Env<'_>, ids: &[Id], most: usize) {
        let mut tried = 0;
        for &id in ids {
            if tried >= most {
                break;
            }
            if self.bool_of(id).is_some() || self.decided_at.get(&id) == Some(&self.unions) {
                continue;
            }
            self.decided_at.insert(id, self.unions);
            tried += 1;
            let t = self.term(id).clone();
            for v in [true, false] {
                let p = self.with_facts(env, &t, &term::boolean(v), |env, _| {
                    env.linear_proof(&t, v)
                });
                if let Some(p) = p {
                    let lit = if v { self.t } else { self.f };
                    self.merge(id, lit, Why::Given(p));
                    break;
                }
            }
        }
        self.process();
    }

    fn progress(&self) -> (usize, usize) {
        (self.unions, self.nodes.len())
    }

    /// `l = r`, where the table can show it.
    fn close(&mut self, env: &mut Env<'_>, l: Id, r: Id) -> Option<Proof> {
        let (lt, rt) = (self.term(l).clone(), self.term(r).clone());
        let goal = |s: &Self| s.same(l, r) || s.same(s.t, s.f);
        self.saturate(env);
        for round in 0..MAX_ROUNDS {
            if goal(self) || self.full {
                break;
            }
            let before = self.progress();
            self.unfold_round(env);
            self.saturate(env);
            if goal(self) {
                break;
            }
            // The guards the openings wait for, then the openings again.
            let guards = self.guards.clone();
            let known = self.unions;
            self.decide(env, &guards, MAX_DECIDE);
            if self.unions != known {
                self.saturate(env);
                self.unfold_round(env);
                self.saturate(env);
                if goal(self) {
                    break;
                }
            }
            self.match_round(env);
            self.saturate(env);
            if goal(self) {
                break;
            }
            let stalled = self.progress() == before;
            if stalled || round + 1 == MAX_ROUNDS {
                self.retry_conds_deep(env);
                self.saturate(env);
                if goal(self) {
                    break;
                }
                self.decide_round(env);
                self.saturate(env);
                if stalled && self.progress() == before {
                    break;
                }
            }
        }
        if self.same(l, r) {
            return self.explain(l, r);
        }
        if self.same(self.t, self.f) {
            return Some(Proof::Absurd {
                contradiction: Box::new(self.explain(self.t, self.f)?),
                lhs: lt,
                rhs: rt,
            });
        }
        // A comparison with a Bool value: linear arithmetic over the facts.
        for (side, other, flip) in [(l, r, false), (r, l, true)] {
            let Some(v) = term::bool_value(self.term(other)).or_else(|| self.bool_of(other))
            else {
                continue;
            };
            let cands: Vec<Id> = self.members[self.root(side)]
                .iter()
                .copied()
                .filter(|&m| int_cmp(self.term(m)).is_some())
                .collect();
            for c in cands {
                let ct = self.term(c).clone();
                let Some(by_linear) = self.with_facts(env, &ct, &term::boolean(v), |env, _| {
                    env.linear_proof(&ct, v)
                        .or_else(|| decide_with_diseqs(env, &ct, v))
                }) else {
                    continue;
                };
                let lit = if v { self.t } else { self.f };
                // side = c = v = other.
                let p = Proof::Trans {
                    terms: vec![
                        self.term(side).clone(),
                        ct.clone(),
                        term::boolean(v),
                        self.term(other).clone(),
                    ],
                    steps: vec![
                        self.explain(side, c)?,
                        by_linear,
                        self.explain(lit, other)?,
                    ],
                };
                return Some(if flip { Proof::Symm(Box::new(p)) } else { p });
            }
        }
        // The facts contradict each other.
        let out = self.with_facts(env, &lt, &rt, |env, _| {
            if let Some(c) = env.refute_linearly() {
                return Some(Proof::Absurd {
                    contradiction: Box::new(c),
                    lhs: lt.clone(),
                    rhs: rt.clone(),
                });
            }
            diseq_chain(env, &lt, &rt, MAX_DISEQ)
        });
        if out.is_none() && std::env::var_os("AVER_TABLE_DEBUG").is_some() {
            self.dump(env, l, r);
        }
        out
    }

    /// What the table holds, for a case it leaves open.
    fn dump(&self, env: &Env<'_>, l: Id, r: Id) {
        let names = env.inputs.symbol_table;
        let show = |t: &Term| crate::ir::proof_steps::show::term(t, names);
        eprintln!(
            "table: open `{}` = `{}`: {} terms, {} joins, {} instances, {} noted, full {}",
            show(self.term(l)),
            show(self.term(r)),
            self.nodes.len(),
            self.unions,
            self.instances,
            self.conds.iter().filter(|c| !c.done).count(),
            self.full
        );
        for (name, root) in [("left", l), ("right", r)] {
            let ms: Vec<String> = self.members[self.root(root)]
                .iter()
                .map(|m| show(self.term(*m)))
                .collect();
            eprintln!("table:   {name}: {}", ms.join("  |  "));
        }
        for lit in [self.t, self.f] {
            let ms: Vec<String> = self.members[self.root(lit)]
                .iter()
                .map(|m| show(self.term(*m)))
                .collect();
            eprintln!("table:   {}: {}", show(self.term(lit)), ms.join("  |  "));
        }
        for c in self.conds.iter().filter(|c| !c.done) {
            eprintln!("table:   noted when: {}", show(&c.when));
        }
        if std::env::var_os("AVER_TABLE_DEBUG_ALL").is_some() {
            let mut seen = HashSet::new();
            for id in 0..self.nodes.len() {
                let r = self.root(id);
                if !seen.insert(r) || self.members[r].len() < 2 {
                    continue;
                }
                let ms: Vec<String> =
                    self.members[r].iter().map(|m| show(self.term(*m))).collect();
                eprintln!("table:   class: {}", ms.join("  |  "));
            }
        }
    }
}

/// `goal = value` for an Int comparison by refuting its opposite with the
/// unequal literals in scope: a case on `goal`, the opposite case closed by
/// [`diseq_chain`].
fn decide_with_diseqs(env: &mut Env<'_>, goal: &Term, value: bool) -> Option<Proof> {
    if !env.hyps.iter().any(|(_, e)| diseq(&e.lhs, &e.rhs).is_some()) {
        return None;
    }
    let h = env.fresh_hyp();
    env.hyps
        .push((h.clone(), Eqn::new(goal.clone(), term::boolean(!value))));
    let other = diseq_chain(env, goal, &term::boolean(value), MAX_DISEQ);
    env.hyps.pop();
    let other = Box::new(other?);
    let same = Box::new(Proof::Hyp(h.clone()));
    let (if_true, if_false) = if value { (same, other) } else { (other, same) };
    Some(Proof::Cases {
        on: goal.clone(),
        hyp: h,
        if_true,
        if_false,
    })
}

/// `x` and the literal `k` of a hypothesis `(x == k) = false`, and whether
/// it is written `k == x`.
fn diseq<'t>(lhs: &'t Term, rhs: &Term) -> Option<(&'t Term, num_bigint::BigInt, bool)> {
    if term::bool_value(rhs) != Some(false) {
        return None;
    }
    let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &lhs.node else {
        return None;
    };
    match (term::int_value(a), term::int_value(b)) {
        (None, Some(k)) if is_int(a) => Some((&**a, k, false)),
        (Some(k), None) if is_int(b) => Some((&**b, k, true)),
        _ => None,
    }
}

/// `gl = gr` where the comparisons in scope leave a variable no value: for
/// `(x == k) = false` with `x >= k` the tightest lower bound, the case
/// `x <= k` makes `x == k` true (`int.eq.of_le_ge`), so only `x > k` is
/// left, and so on up, until linear arithmetic refutes the case; the same
/// down from an upper bound.
fn diseq_chain(env: &mut Env<'_>, gl: &Term, gr: &Term, depth: usize) -> Option<Proof> {
    if let Some(c) = env.refute_linearly() {
        return Some(Proof::Absurd {
            contradiction: Box::new(c),
            lhs: gl.clone(),
            rhs: gr.clone(),
        });
    }
    if depth == 0 {
        return None;
    }
    let cands: Vec<(String, Term, num_bigint::BigInt, bool)> = env
        .hyps
        .iter()
        .filter_map(|(n, e)| {
            diseq(&e.lhs, &e.rhs).map(|(x, k, flipped)| (n.clone(), canon(x), k, flipped))
        })
        .collect();
    for (name, x, k, flipped) in cands {
        let kt = term::int(&k);
        for lower in [true, false] {
            // `x >= k` and not `x >= k + 1` (or the same downward).
            let (bound_op, next) = if lower {
                (BinOp::Gte, &k + 1)
            } else {
                (BinOp::Lte, &k - 1)
            };
            let bound = canon(&term::binop(bound_op, x.clone(), kt.clone()));
            let Some(at_bound) = env.linear_proof(&bound, true) else {
                continue;
            };
            let past = canon(&term::binop(bound_op, x.clone(), term::int(&next)));
            if env.linear_proof(&past, true).is_some() {
                continue;
            }
            // The case on `x <= k` (or `x >= k`).
            let split_op = if lower { BinOp::Lte } else { BinOp::Gte };
            let on = canon(&term::binop(split_op, x.clone(), kt.clone()));
            let h = env.fresh_hyp();
            // `x <= k` and `x >= k`: one the case states, the other the
            // bound.
            let (at_most, at_least) = if lower {
                (Proof::Hyp(h.clone()), at_bound)
            } else {
                (at_bound, Proof::Hyp(h.clone()))
            };
            // `x == k` from the two orders, or `k == x` from the same two
            // read from the other side.
            let (a, b, premises) = if flipped {
                (
                    kt.clone(),
                    x.clone(),
                    vec![
                        linear_flip(&x, &kt, BinOp::Gte, at_least)?,
                        linear_flip(&x, &kt, BinOp::Lte, at_most)?,
                    ],
                )
            } else {
                (x.clone(), kt.clone(), vec![at_most, at_least])
            };
            let eq = canon(&term::binop(BinOp::Eq, a.clone(), b.clone()));
            let contradiction = Proof::Trans {
                terms: vec![term::boolean(true), eq, term::boolean(false)],
                steps: vec![
                    Proof::Symm(Box::new(Proof::Rule {
                        rule: WallRule::EqOfLeGe,
                        subst: vec![("a".into(), a), ("b".into(), b)],
                        premises,
                    })),
                    Proof::Hyp(name.clone()),
                ],
            };
            env.hyps
                .push((h.clone(), Eqn::new(on.clone(), term::boolean(false))));
            let rest = diseq_chain(env, gl, gr, depth - 1);
            env.hyps.pop();
            let Some(rest) = rest else {
                continue;
            };
            return Some(Proof::Cases {
                on,
                hyp: h,
                if_true: Box::new(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: gl.clone(),
                    rhs: gr.clone(),
                }),
                if_false: Box::new(rest),
            });
        }
    }
    None
}

/// `(k op' x) = true` from `p : (x op k) = true`, the same order read
/// from the other side (`x >= k` gives `k <= x`): one linear step.
fn linear_flip(x: &Term, k: &Term, op: BinOp, p: Proof) -> Option<Proof> {
    let stated = canon(&term::binop(op, x.clone(), k.clone()));
    let flipped_op = match op {
        BinOp::Gte => BinOp::Lte,
        BinOp::Lte => BinOp::Gte,
        _ => return None,
    };
    let goal = canon(&term::binop(flipped_op, k.clone(), x.clone()));
    let h = "h_flip".to_string();
    let linear = crate::ir::proof_steps::linear::from_hyps(
        &goal,
        true,
        &[(h.clone(), stated.clone(), true)],
    )?;
    Some(Proof::Have {
        name: h,
        fact: stated,
        proof: Box::new(p),
        body: Box::new(linear),
    })
}

impl Env<'_> {
    /// `lhs = rhs` where evaluation stopped at the two, by a table of
    /// equal terms over them and the hypotheses in scope (see the module
    /// documentation). The table spends none of the fuel or budgets the
    /// rest of the proof has; what it opens and cites is recorded only
    /// when it closes.
    ///
    /// `before` are the equations evaluation went through on the way
    /// (`lhs0 = lhs`, with its proof): the terms it read before they
    /// computed away go into the table too, so a law about one of them
    /// still applies.
    pub(crate) fn close_by_table(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        before: &[(Term, Term, Proof)],
    ) -> Option<Proof> {
        if self.in_table || self.table_runs >= MAX_TABLES {
            return None;
        }
        self.table_runs += 1;
        let fuel = self.fuel;
        let ran_dry = self.ran_dry;
        let marks = (
            self.laws.len(),
            self.used.len(),
            self.used_consts.len(),
            self.open_premises.len(),
            self.open_whens.len(),
            self.hyps.len(),
            self.next_hyp_count(),
            self.hints.len(),
        );
        let refute_fuel = self.refute_fuel;
        let normal_fuel = self.normal_fuel;
        let read_fuel = self.read_fuel;
        let read_claim_fuel = self.read_claim_fuel;
        self.fuel = fuel.min(TABLE_FUEL);
        self.in_table = true;
        let found = self.table_proof(lhs, rhs, before);
        self.in_table = false;
        self.fuel = fuel;
        self.ran_dry = ran_dry;
        self.refute_fuel = refute_fuel;
        self.normal_fuel = normal_fuel;
        self.read_fuel = read_fuel;
        self.read_claim_fuel = read_claim_fuel;
        self.laws.truncate(marks.0);
        self.used.truncate(marks.1);
        self.used_consts.truncate(marks.2);
        self.open_premises.truncate(marks.3);
        self.open_whens.truncate(marks.4);
        self.hyps.truncate(marks.5);
        self.hints.truncate(marks.7);
        let debug = std::env::var_os("AVER_STEPS_DEBUG").is_some();
        // A proof bigger than this costs the rest of the law more than the
        // case is worth: the attempt goes on without it.
        let found = found.filter(|p| p.size() <= MAX_PROOF);
        let Some(proof) = found else {
            self.rewind_hyp_count(marks.6);
            return None;
        };
        // What the proof opens and cites, carried by the script.
        let mut laws = Vec::new();
        super::laws_applied(&proof, &mut laws);
        for key in laws {
            let law = self
                .cited_all
                .iter()
                .chain(self.rewrite_laws.iter())
                .chain(self.fact_rules.iter())
                .chain(self.auto_laws.iter())
                .find(|l| l.key == key)
                .cloned();
            match law {
                Some(law) => self.use_law(law),
                None => {
                    self.rewind_hyp_count(marks.6);
                    return None;
                }
            }
        }
        let mut fns = Vec::new();
        unfolded(&proof, &mut fns);
        for id in fns {
            self.mark_used(id);
        }
        if debug {
            eprintln!(
                "table: closed `{}` = `{}`",
                crate::ir::proof_steps::show::term(lhs, self.inputs.symbol_table),
                crate::ir::proof_steps::show::term(rhs, self.inputs.symbol_table)
            );
        }
        Some(proof)
    }

    fn table_proof(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        before: &[(Term, Term, Proof)],
    ) -> Option<Proof> {
        let mut table = Table::new();
        table.names.set(self.table_names);
        table.load_rewrites(self);
        for (from, to, proof) in before {
            if term::canon_eq(from, to) {
                continue;
            }
            let (Some(a), Some(b)) = (table.add(from), table.add(to)) else {
                return None;
            };
            table.merge(a, b, Why::Given(proof.clone()));
        }
        for (name, e) in self.hyps.clone() {
            let (Some(a), Some(b)) = (table.add(&e.lhs), table.add(&e.rhs)) else {
                return None;
            };
            table.merge(a, b, Why::Given(Proof::Hyp(name)));
        }
        let l = table.add(lhs)?;
        let r = table.add(rhs)?;
        table.process();
        let out = table.close(self, l, r);
        self.table_names = table.names.get();
        out
    }
}

/// The definitions `p` opens.
fn unfolded(p: &Proof, out: &mut Vec<FnId>) {
    if let Proof::Unfold { fn_id, .. } = p
        && !out.contains(fn_id)
    {
        out.push(*fn_id);
    }
    super::each_part(p, &mut |q| unfolded(q, out));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::proof_steps::sexpr::{BuiltinsOnly, script as serialise};
    use crate::ir::proof_steps::term::var;
    use crate::ir::proof_steps::{Obligation, Script};

    fn int_var(n: &str) -> Term {
        let v = var(n);
        v.set_ty(crate::ast::Type::Int);
        v
    }

    /// A script for `lhs = rhs` over Int givens `a`, `b`, `c`, under
    /// `when` (the hypothesis `when`).
    fn script(lhs: Term, rhs: Term, when: Option<Term>, proof: Proof) -> Script {
        let givens: Vec<String> = vec!["a".into(), "b".into(), "c".into()];
        Script {
            obligation: Obligation {
                key: "f.law".into(),
                types: givens
                    .iter()
                    .map(|g| (g.clone(), crate::ast::Type::Int))
                    .collect(),
                givens,
                finite: Vec::new(),
                lists: Vec::new(),
                ints: Vec::new(),
                premise: when,
                refined: Vec::new(),
                lhs,
                rhs,
            },
            defs: Vec::new(),
            consts: Vec::new(),
            laws: Vec::new(),
            decls: Default::default(),
            sigs: Vec::new(),
            plan: None,
            proof,
        }
    }

    fn kernel(s: &Script) -> Result<(), String> {
        crate::proof_kernel::verdict(&serialise(s, &BuiltinsOnly)?).map(|_| ())
    }

    fn add(a: Term, b: Term) -> Term {
        term::binop(BinOp::Add, a, b)
    }

    #[test]
    fn a_table_joins_equal_parts_and_explains_the_congruence() {
        let (a, b, c) = (int_var("a"), int_var("b"), int_var("c"));
        let mut t = Table::new();
        let ab = t.add(&add(a.clone(), c.clone())).unwrap();
        let bc = t.add(&add(b.clone(), c.clone())).unwrap();
        let (ia, ib) = (t.find(&a).unwrap(), t.find(&b).unwrap());
        assert!(!t.same(ab, bc));
        t.merge(ia, ib, Why::Given(Proof::Hyp("when".into())));
        t.process();
        assert!(t.same(ab, bc), "congruence joins `a + c` and `b + c`");
        // The explanation is checked by the kernel under `when : a = b`,
        // stated as the premise `a == b`, read through `int.eq.of_beq`.
        let mut t = Table::new();
        let eq = t.add(&term::binop(BinOp::Eq, a.clone(), b.clone())).unwrap();
        let ab = t.add(&add(a.clone(), c.clone())).unwrap();
        let bc = t.add(&add(b.clone(), c.clone())).unwrap();
        t.merge(eq, t.t, Why::Given(Proof::Hyp("when".into())));
        t.process();
        // The local rules need an environment only for non-Int equalities.
        let (ia, ib) = (t.find(&a).unwrap(), t.find(&b).unwrap());
        t.merge(
            ia,
            ib,
            Why::Rule {
                rule: WallRule::EqOfBeq,
                subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
                premises: vec![Prem::Same(eq, t.t)],
            },
        );
        t.process();
        assert!(t.same(ab, bc));
        let proof = explain_alone(&t, ab, bc).expect("an explanation");
        let s = script(
            add(a.clone(), c.clone()),
            add(b.clone(), c.clone()),
            Some(term::binop(BinOp::Eq, a.clone(), b.clone())),
            proof,
        );
        kernel(&s).expect("the kernel accepts the explanation");
        // And refuses it for a claim it does not show.
        let wrong = script(
            add(a.clone(), c.clone()),
            add(c.clone(), b.clone()),
            Some(term::binop(BinOp::Eq, a, b)),
            explain_alone(&t, ab, bc).unwrap(),
        );
        assert!(kernel(&wrong).is_err());
    }

    #[test]
    fn a_closed_part_computes_and_two_values_joined_are_a_contradiction() {
        let a = int_var("a");
        let mut t = Table::new();
        // `when : (a + 1 == 3) = true`, and `a == 2` asked for.
        let sum = add(a.clone(), term::int(&1.into()));
        let hyp = term::binop(BinOp::Eq, sum.clone(), term::int(&3.into()));
        let h = t.add(&hyp).unwrap();
        t.merge(h, t.t, Why::Given(Proof::Hyp("when".into())));
        let ia = t.find(&a).unwrap();
        let two = t.add(&term::int(&2.into())).unwrap();
        // `a = 2` as a stated join, then `a + 1` computes to 3 through it.
        t.merge(ia, two, Why::Given(Proof::Hyp("unused".into())));
        t.process();
        let isum = t.find(&sum).unwrap();
        t.fold(isum);
        t.process();
        let three = t.find(&term::int(&3.into())).unwrap();
        assert!(t.same(isum, three), "`a + 1` computes once `a` is 2");
        // Two different literals joined: `2 == 3` computes to false, and
        // joining the two makes `true` and `false` one entry.
        let mut t = Table::new();
        let two = t.add(&term::int(&2.into())).unwrap();
        let three = t.add(&term::int(&3.into())).unwrap();
        let eq = t
            .add(&term::binop(BinOp::Eq, term::int(&2.into()), term::int(&3.into())))
            .unwrap();
        t.fold(eq);
        t.process();
        assert!(t.same(eq, t.f));
        assert!(!t.same(two, three));
    }

    #[test]
    fn the_table_stops_at_its_size_limit() {
        let mut t = Table::new();
        let mut last = None;
        for i in 0..(MAX_NODES + 10) {
            last = t.add(&add(int_var("a"), term::int(&(i as i64).into())));
            if last.is_none() {
                break;
            }
        }
        assert!(last.is_none(), "a full table adds nothing");
        assert!(t.full);
        assert!(t.nodes.len() <= MAX_NODES);
    }

    #[test]
    fn a_false_disjunction_gives_false_sides_the_kernel_accepts() {
        let (a, b) = (int_var("a"), int_var("b"));
        let zero = || term::int(&0.into());
        let p = term::binop(BinOp::Lt, a.clone(), zero());
        let q = term::binop(BinOp::Lt, b.clone(), zero());
        let or = term::bool_or(p.clone(), q.clone());
        let mut t = Table::new();
        let io = t.add(&or).unwrap();
        // `when : Bool.not(Bool.or(p, q)) = true`, read as the or false.
        let not = term::bool_not(or.clone());
        let inot = t.add(&not).unwrap();
        t.merge(inot, t.t, Why::Given(Proof::Hyp("when".into())));
        t.process();
        t.merge(
            io,
            t.f,
            Why::NotElim {
                not: inot,
                value: t.t,
            },
        );
        t.process();
        let ip = t.find(&p).unwrap();
        t.merge(
            ip,
            t.f,
            Why::OrFalse {
                or: io,
                falsity: t.f,
                left: true,
            },
        );
        t.process();
        let proof = explain_alone(&t, ip, t.f).expect("an explanation");
        let s = script(canon(&p), term::boolean(false), Some(not), proof);
        kernel(&s).expect("the kernel accepts `a < 0 = false`");
    }

    fn explain_alone(t: &Table, a: Id, b: Id) -> Option<Proof> {
        t.explain(a, b)
    }
}
