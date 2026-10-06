//! Evaluation with a proof: reduce a term towards a value, one rule at a
//! time, recording each rule. Where evaluation stops at a Bool it cannot
//! decide, the prover splits on that Bool and evaluates both sides again
//! under the new hypothesis.

use crate::ast::BinOp;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedPattern};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, WallRule};

use super::chain::{Chain, meet};
use super::env::Env;

pub(crate) struct Eval {
    pub chain: Chain,
    /// The undecided Bool evaluation stopped at, if a split would help.
    pub blocked: Option<Term>,
}

enum Step {
    Progress(Box<(Proof, Term)>),
    Done,
    Blocked(Term),
}

/// How deep evaluations may nest before the producer refuses: each level
/// costs several large frames of the compiler's own stack, and a test
/// thread has 2 MiB.
const MAX_NESTING: usize = 24;

/// Whether `part` occurs in `t`.
fn holds(t: &Term, part: &Term) -> bool {
    canon(t) == *part || term::children(t).into_iter().any(|c| holds(c, part))
}

/// Complement rules: `(a P b) = v` decides `(a Q b) = w`.
const COMPLEMENTS: [(BinOp, bool, BinOp, bool, WallRule); 12] = [
    (BinOp::Gt, false, BinOp::Lte, true, WallRule::LeOfNotGt),
    (BinOp::Gt, true, BinOp::Lte, false, WallRule::LeFalseOfGt),
    (BinOp::Lt, false, BinOp::Gte, true, WallRule::GeOfNotLt),
    (BinOp::Lt, true, BinOp::Gte, false, WallRule::GeFalseOfLt),
    (BinOp::Gte, false, BinOp::Lt, true, WallRule::LtOfNotGe),
    (BinOp::Gte, true, BinOp::Lt, false, WallRule::LtFalseOfGe),
    (BinOp::Lte, false, BinOp::Gt, true, WallRule::GtOfNotLe),
    (BinOp::Lte, true, BinOp::Gt, false, WallRule::GtFalseOfLe),
    (BinOp::Neq, false, BinOp::Eq, true, WallRule::EqOfNotNe),
    (BinOp::Neq, true, BinOp::Eq, false, WallRule::EqFalseOfNe),
    (BinOp::Eq, false, BinOp::Neq, true, WallRule::NeOfNotEq),
    (BinOp::Eq, true, BinOp::Neq, false, WallRule::NeFalseOfEq),
];

fn is_int(t: &Term) -> bool {
    matches!(t.ty(), Some(crate::ast::Type::Int)) || term::int_value(t).is_some()
}

/// Whether `part` occurs in `t`, both canonical; a `match` counts as
/// containing everything, since its arms are not children.
fn occurs(part: &Term, t: &Term) -> bool {
    *t == *part
        || matches!(t.node, ResolvedExpr::Match { .. })
        || term::children(t).into_iter().any(|c| occurs(part, c))
}

/// Every ordering of the factors of a product.
fn permutations(m: &[usize]) -> Vec<Vec<usize>> {
    if m.len() <= 1 {
        return vec![m.to_vec()];
    }
    let mut out = Vec::new();
    for i in 0..m.len() {
        let mut rest = m.to_vec();
        let first = rest.remove(i);
        for mut tail in permutations(&rest) {
            tail.insert(0, first);
            if !out.contains(&tail) {
                out.push(tail);
            }
        }
    }
    out
}

/// The arm of a match whose pattern head the value `v` has, with the
/// pattern's bindings, and whether `v` is a list literal the arm reads as a
/// cell (so the premise ends with a [`Proof::Cell`] step). `None` when no
/// arm can be selected syntactically.
fn select_arm(
    arms: &[crate::ir::hir::ResolvedMatchArm],
    v: &Term,
) -> Option<(u32, Vec<Term>, bool)> {
    use crate::ir::proof_steps::claim::{excludes, is_catch_all};
    for (i, arm) in arms.iter().enumerate() {
        let k = (i + 1) as u32;
        if is_catch_all(&arm.pattern) {
            // Chosen for this value once every earlier arm excludes it.
            return arms[..i]
                .iter()
                .all(|earlier| excludes(&earlier.pattern, v))
                .then(|| (k, vec![canon(v)], false));
        }
        match (&arm.pattern, &v.node) {
            (ResolvedPattern::Literal(l), ResolvedExpr::Literal(x)) => {
                if l == x {
                    return Some((k, Vec::new(), false));
                }
            }
            (ResolvedPattern::Ctor(c, names), ResolvedExpr::Ctor(c2, args)) => {
                if c == c2 && names.len() == args.len() {
                    return Some((k, args.clone(), false));
                }
            }
            (ResolvedPattern::EmptyList, ResolvedExpr::List(xs)) if xs.is_empty() => {
                return Some((k, Vec::new(), false));
            }
            (ResolvedPattern::Cons(..), ResolvedExpr::Call(ResolvedCallee::Builtin(b), args))
                if b == "List.prepend" && args.len() == 2 =>
            {
                return Some((k, args.clone(), false));
            }
            // A list literal with an element is a cell.
            (ResolvedPattern::Cons(..), ResolvedExpr::List(xs)) if !xs.is_empty() => {
                return Some((
                    k,
                    term::children(&term::cell_of(v)?)
                        .into_iter()
                        .cloned()
                        .collect(),
                    true,
                ));
            }
            // A literal subject meets a different literal / constructor:
            // keep looking. Anything else cannot be decided here.
            (ResolvedPattern::EmptyList, ResolvedExpr::List(xs)) if !xs.is_empty() => continue,
            (ResolvedPattern::EmptyList, ResolvedExpr::Call(..))
            | (ResolvedPattern::Cons(..), ResolvedExpr::List(_)) => continue,
            _ => return None,
        }
    }
    None
}

/// `chain` with a last [`Proof::Cell`] step when its value is a list
/// literal an arm reads as a cell.
fn with_cell(mut chain: Chain, cell: bool) -> Chain {
    if cell {
        let list = chain.cur().clone();
        let to = term::cell_of(&list).expect("select_arm saw a list literal with an element");
        chain.push(Proof::Cell { list }, to);
    }
    chain
}

/// What Euclidean division by a literal `k > 0` in `ts` gives linear
/// arithmetic, each fact with its proof: for each `a / k` or `a % k`, the
/// two orders of `a / k * k + a % k = a` (`int.div_mod_recompose`), and
/// `0 <= a % k` and `a % k < k` (`int.mod_range`). Also the quotients,
/// each inside the ones that divide it again.
fn division_facts(ts: &[&Term]) -> (Vec<(Term, Proof)>, Vec<Term>) {
    use crate::ir::hir::BuiltinIntrinsic;
    fn walk(t: &Term, mods: &mut Vec<Term>, divs: &mut Vec<Term>) {
        if let ResolvedExpr::Call(ResolvedCallee::Intrinsic(op), args) = &t.node
            && args.len() == 2
            && term::int_value(&args[1]).is_some_and(|k| k > 0.into())
        {
            let found = match op {
                BuiltinIntrinsic::IntModEuclid => Some(&mut *mods),
                BuiltinIntrinsic::IntDivEuclid => Some(&mut *divs),
                _ => None,
            };
            if let Some(found) = found
                && !found.contains(&canon(t))
            {
                found.push(canon(t));
            }
        }
        if !matches!(t.node, ResolvedExpr::Match { .. }) {
            for c in term::children(t) {
                walk(c, mods, divs);
            }
        }
    }
    let (mut mods, mut divs) = (Vec::new(), Vec::new());
    for t in ts {
        walk(t, &mut mods, &mut divs);
    }
    for m in &mods {
        if let ResolvedExpr::Call(_, args) = &m.node {
            let d = canon(&term::intrinsic(
                BuiltinIntrinsic::IntDivEuclid,
                args.clone(),
            ));
            if !divs.contains(&d) {
                divs.push(d);
            }
        }
    }
    let positive = |k: &Term| Proof::Compute {
        lhs: canon(&term::binop(BinOp::Gt, k.clone(), term::int(&0.into()))),
        rhs: term::boolean(true),
    };
    let mut out = Vec::new();
    for d in &divs {
        let ResolvedExpr::Call(_, args) = &d.node else {
            continue;
        };
        let (a, k) = (canon(&args[0]), canon(&args[1]));
        let subst = vec![("a".to_string(), a.clone()), ("k".to_string(), k.clone())];
        let Some((_, concl)) = WallRule::DivModRecompose.instantiate(&subst) else {
            continue;
        };
        let recompose = Proof::Rule {
            rule: WallRule::DivModRecompose,
            subst,
            premises: vec![positive(&k)],
        };
        let whole = canon(&concl.lhs);
        for op in [BinOp::Lte, BinOp::Gte] {
            let fact = canon(&term::binop(op, whole.clone(), a.clone()));
            let same = canon(&term::binop(op, a.clone(), a.clone()));
            let proof = Proof::Trans {
                terms: vec![fact.clone(), same.clone(), term::boolean(true)],
                steps: vec![
                    Proof::Congr {
                        ctx: term::binop(op, term::hole(), a.clone()),
                        inner: Box::new(recompose.clone()),
                    },
                    Proof::Linear {
                        goal: same,
                        value: true,
                        hyps: Vec::new(),
                        weights: vec![1.into()],
                    },
                ],
            };
            out.push((fact, proof));
        }
        let m = canon(&term::intrinsic(BuiltinIntrinsic::IntModEuclid, vec![a, k]));
        if !mods.contains(&m) {
            mods.push(m);
        }
    }
    for m in mods {
        let ResolvedExpr::Call(_, args) = &m.node else {
            continue;
        };
        let (a, k) = (canon(&args[0]), canon(&args[1]));
        let range = Proof::Rule {
            rule: WallRule::ModRange,
            subst: vec![("a".into(), a), ("k".into(), k.clone())],
            premises: vec![positive(&k)],
        };
        let low = canon(&term::binop(BinOp::Lte, term::int(&0.into()), m.clone()));
        let high = canon(&term::binop(BinOp::Lt, m.clone(), k));
        let elim = |rule: WallRule| Proof::Rule {
            rule,
            subst: vec![("a".into(), low.clone()), ("b".into(), high.clone())],
            premises: vec![range.clone()],
        };
        out.push((low.clone(), elim(WallRule::AndElimL)));
        out.push((high.clone(), elim(WallRule::AndElimR)));
    }
    // A quotient after the quotients inside it, which are smaller terms.
    fn size(t: &Term) -> usize {
        1 + term::children(t).into_iter().map(size).sum::<usize>()
    }
    divs.sort_by_key(size);
    (out, divs)
}

/// `a < c` as the least `c` with its comparison `t` against a literal, when
/// `t = value` says so: `a < c`, `a <= c - 1`, `c > a`, `c - 1 >= a`, or
/// the complement of `a >= c`.
fn upper_bound(t: &Term, value: bool, a: &Term) -> Option<num_bigint::BigInt> {
    let ResolvedExpr::BinOp(op, l, r) = &t.node else {
        return None;
    };
    let (op, c) = match (canon(l) == *a, canon(r) == *a) {
        (true, false) => (*op, term::int_value(r)?),
        (false, true) => {
            let flipped = match op {
                BinOp::Lt => BinOp::Gt,
                BinOp::Gt => BinOp::Lt,
                BinOp::Lte => BinOp::Gte,
                BinOp::Gte => BinOp::Lte,
                _ => return None,
            };
            (flipped, term::int_value(l)?)
        }
        _ => return None,
    };
    match (op, value) {
        (BinOp::Lt, true) | (BinOp::Gte, false) => Some(c),
        (BinOp::Lte, true) | (BinOp::Gt, false) => Some(c + 1),
        _ => None,
    }
}

/// Whether `t` builds or updates a record anywhere.
fn has_record(t: &Term) -> bool {
    matches!(
        t.node,
        ResolvedExpr::RecordCreate { .. } | ResolvedExpr::RecordUpdate { .. }
    ) || term::children(t).into_iter().any(has_record)
}

/// The position of the first list literal in `t` with an element that is
/// not a closed value, outside any `match`.
fn open_literal_path(t: &Term) -> Option<Vec<usize>> {
    if matches!(t.node, ResolvedExpr::Match { .. }) {
        return None;
    }
    if let ResolvedExpr::List(xs) = &t.node
        && !xs.is_empty()
        && term::eval_closed(t).is_none()
    {
        return Some(Vec::new());
    }
    term::children(t)
        .into_iter()
        .enumerate()
        .find_map(|(i, c)| {
            let mut path = open_literal_path(c)?;
            path.insert(0, i);
            Some(path)
        })
}

/// `t = t'`, where `t'` writes every list literal of `t` with an open
/// element as cells, one [`Proof::Cell`] step each; `None` when there is
/// none.
pub(crate) fn as_cells(t: &Term) -> Option<(Proof, Term)> {
    let mut cur = canon(t);
    let mut terms = vec![cur.clone()];
    let mut steps = Vec::new();
    while let Some(path) = open_literal_path(&cur) {
        if steps.len() >= 32 {
            return None;
        }
        let list = term::at(&cur, &path).clone();
        let ctx = term::context_at(&cur, &path);
        cur = canon(&term::plug(&ctx, &term::cell_of(&list)?));
        steps.push(Proof::Congr {
            ctx,
            inner: Box::new(Proof::Cell { list }),
        });
        terms.push(cur.clone());
    }
    match steps.len() {
        0 => None,
        1 => Some((steps.pop()?, cur)),
        _ => Some((Proof::Trans { terms, steps }, cur)),
    }
}

fn is_bool_value(t: &Term) -> bool {
    term::bool_value(t).is_some()
}

/// The most head steps evaluating one hypothesis may take.
const HYPOTHESIS_FUEL: usize = 100;

impl Env<'_> {
    pub(crate) fn whnf(&mut self, t: &Term) -> Result<Eval, String> {
        // Each nested evaluation is a frame on the compiler's own stack; a
        // recursive definition over long data would exhaust it.
        if self.nesting >= MAX_NESTING {
            return Err(format!(
                "evaluation nests more than {MAX_NESTING} deep at `{}`",
                crate::ir::proof_steps::show::term(t, self.inputs.symbol_table)
            ));
        }
        self.nesting += 1;
        let out = self.whnf_nested(t);
        self.nesting -= 1;
        out
    }

    fn whnf_nested(&mut self, t: &Term) -> Result<Eval, String> {
        let mut chain = Chain::new(t);
        loop {
            self.burn()?;
            let cur = chain.cur().clone();
            match self.head_step(&cur)? {
                Step::Progress(step) => {
                    let (proof, to) = *step;
                    chain.push(proof, to)
                }
                Step::Done => {
                    return Ok(Eval {
                        chain,
                        blocked: None,
                    });
                }
                Step::Blocked(g) => {
                    return Ok(Eval {
                        chain,
                        blocked: Some(g),
                    });
                }
            }
        }
    }

    /// Evaluate child `i`; on progress, the congruence step for the whole term.
    fn child(&mut self, cur: &Term, i: usize) -> Result<Result<Step, Option<Term>>, String> {
        let child = term::children(cur)[i].clone();
        let ev = self.whnf(&child)?;
        if ev.chain.is_empty() {
            return Ok(Err(ev.blocked));
        }
        let (to, proof) = ev.chain.finish();
        let ctx = term::context_at(cur, &[i]);
        let new = term::plug(&ctx, &to);
        Ok(Ok(Step::Progress(Box::new((
            Proof::Congr {
                ctx,
                inner: Box::new(proof),
            },
            new,
        )))))
    }

    /// What the facts in scope say about `cur` as it stands, before it is
    /// opened or its parts evaluated, in a fixed order: a hypothesis or
    /// one of its conjuncts, a fact inside a hypothesis that calls a
    /// predicate, an Int equality one cited law instance settles, then a
    /// cited law applied left to right.
    fn known(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        if let Some((name, value)) = self.hyp_for(cur) {
            return Ok(Some(Step::Progress(Box::new((Proof::Hyp(name), value)))));
        }
        if let Some(step) = self.hyp_as_cells(cur) {
            return Ok(Some(step));
        }
        if let Some(step) = self.same_sides(cur) {
            return Ok(Some(step));
        }
        if let Some(proof) = self.conjunct_of_hyp(cur) {
            return Ok(Some(Step::Progress(Box::new((proof, term::boolean(true))))));
        }
        if let Some(proof) = self.view_for(cur) {
            return Ok(Some(Step::Progress(Box::new((proof, term::boolean(true))))));
        }
        if let Some(step) = self.cited_int_equality(cur) {
            return Ok(Some(step));
        }
        if let Some(step) = self.decide_before_rewrite(cur) {
            return Ok(Some(step));
        }
        if let Some(step) = self.equal_by_hypothesis(cur) {
            return Ok(Some(step));
        }
        if let Some((proof, to)) = self.rewrite_with_cited(cur)? {
            return Ok(Some(Step::Progress(Box::new((proof, to)))));
        }
        Ok(None)
    }

    /// A hypothesis whose left side is `cur` once its list literals with an
    /// open element are written as cells, the way evaluation writes them
    /// (`[d]` as `List.prepend(d, [])`): an induction hypothesis states the
    /// claim as written, while the case has evaluated it.
    ///
    /// Also a hypothesis that names a constant (a definition without
    /// parameters whose body computes to a value, `capBytes()`) where `cur`
    /// has its value.
    fn hyp_as_cells(&mut self, cur: &Term) -> Option<Step> {
        let t = canon(cur);
        for (name, e) in self.hyps.clone().into_iter().rev() {
            let lhs = canon(&e.lhs);
            let mut chain = Chain::new(&lhs);
            if let Some((p, to)) = as_cells(&lhs) {
                chain.push(p, to);
            }
            if let Some((p, to)) = self.fold_constants(&chain.cur().clone()) {
                chain.push(p, to);
            }
            if chain.is_empty() || *chain.cur() != t {
                continue;
            }
            let (_, bridge) = chain.finish();
            let proof = Proof::Trans {
                terms: vec![t.clone(), lhs, canon(&e.rhs)],
                steps: vec![Proof::Symm(Box::new(bridge)), Proof::Hyp(name)],
            };
            return Some(Step::Progress(Box::new((proof, canon(&e.rhs)))));
        }
        None
    }

    /// `t = t'`, where `t'` has the value of each constant `t` names; `None`
    /// when it names none.
    pub(crate) fn fold_constants(&mut self, t: &Term) -> Option<(Proof, Term)> {
        let mut chain = Chain::new(&canon(t));
        for _ in 0..32 {
            let Some((path, p, to)) = self.constant_call(&chain.cur().clone()) else {
                break;
            };
            chain.push_at(&path, p, &to);
        }
        if chain.is_empty() {
            return None;
        }
        let (to, proof) = chain.finish();
        Some((proof, canon(&to)))
    }

    /// For each hypothesis `(a == b) = true` between Ints: `a <= b` and
    /// `a >= b`, each by rewriting `a` to `b` (`int.eq.of_beq`) and the
    /// order of `b` with itself.
    fn equalities_as_orders(&self) -> Vec<(Term, Proof)> {
        let mut out = Vec::new();
        for (name, e) in &self.hyps {
            let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &e.lhs.node else {
                continue;
            };
            if term::bool_value(&e.rhs) != Some(true) || !(is_int(a) || is_int(b)) {
                continue;
            }
            let (a, b) = (canon(a), canon(b));
            let equal = Proof::Rule {
                rule: WallRule::EqOfBeq,
                subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
                premises: vec![Proof::Hyp(name.clone())],
            };
            for op in [BinOp::Lte, BinOp::Gte] {
                let fact = canon(&term::binop(op, a.clone(), b.clone()));
                let same = canon(&term::binop(op, b.clone(), b.clone()));
                let proof = Proof::Trans {
                    terms: vec![fact.clone(), same.clone(), term::boolean(true)],
                    steps: vec![
                        Proof::Congr {
                            ctx: term::binop(op, term::hole(), b.clone()),
                            inner: Box::new(equal.clone()),
                        },
                        Proof::Linear {
                            goal: same,
                            value: true,
                            hyps: Vec::new(),
                            weights: vec![1.into()],
                        },
                    ],
                };
                out.push((fact, proof));
            }
        }
        out
    }

    /// `(a == a) = true`, by `bool.beq.refl`, for a type whose values hold
    /// no Float (`0.0 / 0.0` is not equal to itself).
    fn same_sides(&self, cur: &Term) -> Option<Step> {
        let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node else {
            return None;
        };
        let ty = a.ty().or(b.ty())?;
        if canon(a) != canon(b) || self.inputs.symbol_table.may_hold_float(ty) {
            return None;
        }
        Some(self.rule_step(WallRule::BeqRefl, vec![("a".into(), canon(a))]))
    }

    /// The first call in `t` of a definition without parameters, not a
    /// `match` and not recursive, whose body computes to a value: its
    /// position, `f() = value`, and the value.
    fn constant_call(&mut self, t: &Term) -> Option<(Vec<usize>, Proof, Term)> {
        if matches!(t.node, ResolvedExpr::Match { .. }) {
            return None;
        }
        if let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &t.node
            && args.is_empty()
            && !self.inputs.recursive_fns.contains(id)
            && let Some(def) = self.def(*id)
            && !matches!(def.body.node, ResolvedExpr::Match { .. })
            && let Some(value) = term::eval_closed(&canon(&def.body))
        {
            self.mark_used(*id);
            let body = canon(&def.body);
            let unfold = Proof::Unfold {
                fn_id: *id,
                arm: 0,
                args: Vec::new(),
                binders: Vec::new(),
                premise: None,
            };
            let proof = if body == value {
                unfold
            } else {
                Proof::Trans {
                    terms: vec![canon(t), body.clone(), value.clone()],
                    steps: vec![
                        unfold,
                        Proof::Compute {
                            lhs: body,
                            rhs: value.clone(),
                        },
                    ],
                }
            };
            return Some((Vec::new(), proof, value));
        }
        for (i, c) in term::children(t).into_iter().enumerate() {
            let c = c.clone();
            if let Some((mut path, p, v)) = self.constant_call(&c) {
                path.insert(0, i);
                return Some((path, p, v));
            }
        }
        None
    }

    /// `(x == y) = true` for Int sides one cited law instance makes equal:
    /// congruence to `y == y`, which the two orders settle.
    fn cited_int_equality(&mut self, cur: &Term) -> Option<Step> {
        let ResolvedExpr::BinOp(BinOp::Eq, x, y) = &cur.node else {
            return None;
        };
        if !(is_int(x) || is_int(y)) || self.cited_all.is_empty() {
            return None;
        }
        let eq = self.direct_equal(x, y)?;
        let same = canon(&term::binop(BinOp::Eq, (**y).clone(), (**y).clone()));
        let (refl, value) = self.decide_linearly(&same)?;
        if term::bool_value(&value) != Some(true) {
            return None;
        }
        let ctx = term::binop(BinOp::Eq, term::hole(), (**y).clone());
        Some(Step::Progress(Box::new((
            Proof::Trans {
                terms: vec![canon(cur), same, term::boolean(true)],
                steps: vec![
                    Proof::Congr {
                        ctx,
                        inner: Box::new(eq),
                    },
                    refl,
                ],
            },
            term::boolean(true),
        ))))
    }

    /// `a = k` for an Int term `a` a hypothesis or the orders make equal to
    /// `k`, by `int.eq.of_beq`: a hypothesis states `(a == k) = true`, or,
    /// for a variable, the comparisons in scope bound it by the literal `k`
    /// from both sides (`k` one of the literals a hypothesis compares it
    /// with). A stated `k` that is not a literal is taken only when no left
    /// side of a stated equality occurs in it, so a rewrite never feeds
    /// another.
    fn equal_by_hypothesis(&mut self, cur: &Term) -> Option<Step> {
        if term::int_value(cur).is_some() {
            return None;
        }
        let t = canon(cur);
        let stated_lhs = self.stated_equalities();
        let candidates: Vec<(String, Term, Term)> = self
            .hyps
            .iter()
            .rev()
            .filter(|(_, e)| term::bool_value(&e.rhs) == Some(true))
            .filter_map(|(n, e)| match &e.lhs.node {
                ResolvedExpr::BinOp(BinOp::Eq, a, k) => Some((n.clone(), canon(a), canon(k))),
                _ => None,
            })
            .filter(|(_, a, k)| {
                term::int_value(k).is_some()
                    || (is_int(a) && is_int(k) && !stated_lhs.iter().any(|l| occurs(l, k)))
            })
            .collect();
        for (n, a, k) in candidates {
            let rule = |a: Term| Proof::Rule {
                rule: WallRule::EqOfBeq,
                subst: vec![("a".into(), a), ("b".into(), k.clone())],
                premises: vec![Proof::Hyp(n.clone())],
            };
            if a == t {
                return Some(Step::Progress(Box::new((rule(a), k))));
            }
            // `a` a call of a definition that only names another term
            // (no `match`, no recursion) whose body there is `cur`: the
            // hypothesis read through that one unfolding.
            if term::int_value(&k).is_some() {
                continue;
            }
            let Some((unfold, body)) = self.wrapper_unfold(&a) else {
                continue;
            };
            if body != t {
                continue;
            }
            if let ResolvedExpr::Call(ResolvedCallee::Fn(id), _) = &a.node {
                self.mark_used(*id);
            }
            let proof = Proof::Trans {
                terms: vec![t.clone(), a.clone(), k.clone()],
                steps: vec![Proof::Symm(Box::new(unfold)), rule(a)],
            };
            return Some(Step::Progress(Box::new((proof, k))));
        }
        let (premise, k) = self.pinned(&t)?;
        Some(Step::Progress(Box::new((
            Proof::Rule {
                rule: WallRule::EqOfBeq,
                subst: vec![("a".into(), t), ("b".into(), k.clone())],
                premises: vec![premise],
            },
            k,
        ))))
    }

    /// The whole-body equation of `a`, a call of a definition whose body is
    /// not a `match` and that does not recurse, with that body at `a`'s
    /// arguments; `None` otherwise.
    fn wrapper_unfold(&mut self, a: &Term) -> Option<(Proof, Term)> {
        let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &a.node else {
            return None;
        };
        if self.inputs.recursive_fns.contains(id) {
            return None;
        }
        let def = self.def(*id)?;
        if matches!(def.body.node, ResolvedExpr::Match { .. }) {
            return None;
        }
        let body = canon(&term::subst(&def.body, &def.outer(args).ok()?).ok()?);
        let unfold = Proof::Unfold {
            fn_id: *id,
            arm: 0,
            args: args.iter().map(canon).collect(),
            binders: Vec::new(),
            premise: None,
        };
        Some((unfold, body))
    }

    /// `(x == k) = true` for a variable `x` of type Int that the
    /// comparisons in scope pin to the literal `k`, by the two orders.
    fn pinned(&mut self, x: &Term) -> Option<(Proof, Term)> {
        if !matches!(x.node, ResolvedExpr::Ident(_)) || !is_int(x) {
            return None;
        }
        let mut literals: Vec<Term> = Vec::new();
        for (_, e) in &self.hyps {
            if term::bool_value(&e.rhs).is_none() {
                continue;
            }
            let ResolvedExpr::BinOp(op, a, b) = &e.lhs.node else {
                continue;
            };
            if !matches!(op, BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte) {
                continue;
            }
            for (side, other) in [(a, b), (b, a)] {
                if canon(side) == *x
                    && term::int_value(other).is_some()
                    && !literals.contains(&canon(other))
                {
                    literals.push(canon(other));
                }
            }
        }
        for k in literals {
            let eq = canon(&term::binop(BinOp::Eq, x.clone(), k.clone()));
            if let Some((proof, value)) = self.decide_linearly(&eq)
                && term::bool_value(&value) == Some(true)
            {
                return Some((proof, k));
            }
        }
        None
    }

    fn settle(&mut self, cur: &Term, blocked: Option<Term>) -> Result<Step, String> {
        if let Some(step) = self.known(cur)? {
            return Ok(step);
        }
        if let Some(step) = self.decide_linearly(cur) {
            return Ok(Step::Progress(Box::new(step)));
        }
        if let Some(step) = self.complement(cur) {
            return Ok(step);
        }
        Ok(match blocked {
            Some(g) => Step::Blocked(g),
            None => Step::Done,
        })
    }

    /// `(a Q b) = w` by a complement rule from `(a P b) = v`: a hypothesis
    /// states it, or, for `P` an Int equality, the two orders decide it
    /// (`b != b` is false by `b == b`).
    fn complement(&mut self, cur: &Term) -> Option<Step> {
        let ResolvedExpr::BinOp(op, a, b) = &cur.node else {
            return None;
        };
        if !is_int(a) {
            return None;
        }
        for (p, v, q, w, rule) in COMPLEMENTS {
            if q != *op {
                continue;
            }
            let premise = canon(&term::binop(p, (**a).clone(), (**b).clone()));
            let proof = match self.hyp_for(&premise) {
                Some((name, value)) if term::bool_value(&value) == Some(v) => Proof::Hyp(name),
                _ if p == BinOp::Eq => match self.decide_linearly(&premise) {
                    Some((proof, value)) if term::bool_value(&value) == Some(v) => proof,
                    _ => continue,
                },
                _ => continue,
            };
            return Some(Step::Progress(Box::new((
                Proof::Rule {
                    rule,
                    subst: vec![("a".into(), (**a).clone()), ("b".into(), (**b).clone())],
                    premises: vec![proof],
                },
                term::boolean(w),
            ))));
        }
        None
    }

    /// An Int comparison decided as it stands, before a stated equality
    /// rewrites one of its parts: the hypotheses about those parts still
    /// name them as written, so after the rewrite they would no longer apply.
    fn decide_before_rewrite(&mut self, cur: &Term) -> Option<Step> {
        let ResolvedExpr::BinOp(op, a, _) = &cur.node else {
            return None;
        };
        if !is_int(a)
            || !matches!(
                op,
                BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte | BinOp::Eq | BinOp::Neq
            )
        {
            return None;
        }
        let t = canon(cur);
        let rewritten = self
            .stated_equalities()
            .iter()
            .any(|l| *l != t && term::children(&t).into_iter().any(|c| holds(c, l)));
        if !rewritten {
            return None;
        }
        if let Some(step) = self.complement(&t) {
            return Some(step);
        }
        self.decide_linearly(&t)
            .map(|step| Step::Progress(Box::new(step)))
    }

    /// The left sides of the true equalities in scope, `(a == k) = true`.
    fn stated_equalities(&self) -> Vec<Term> {
        self.hyps
            .iter()
            .filter(|(_, e)| term::bool_value(&e.rhs) == Some(true))
            .filter_map(|(_, e)| match &e.lhs.node {
                ResolvedExpr::BinOp(BinOp::Eq, a, _) => Some(canon(a)),
                _ => None,
            })
            .collect()
    }

    fn head_step(&mut self, cur: &Term) -> Result<Step, String> {
        // The term as it stands first: a fact may state it before it is
        // opened (`positiveProduct(a, b)` from a cited law, not its body).
        if matches!(
            cur.node,
            ResolvedExpr::Call(ResolvedCallee::Fn(_), _) | ResolvedExpr::BinOp(..)
        ) && let Some(step) = self.known(cur)?
        {
            return Ok(step);
        }
        match &cur.node {
            ResolvedExpr::Attr(obj, _) => {
                if matches!(obj.node, ResolvedExpr::RecordCreate { .. }) {
                    let ev = crate::ir::proof_steps::claim::claim(
                        &Proof::Proj { term: cur.clone() },
                        &empty_script(),
                        &Vec::new(),
                    )?;
                    return Ok(Step::Progress(Box::new((
                        Proof::Proj { term: cur.clone() },
                        ev.rhs,
                    ))));
                }
                match self.child(cur, 0)? {
                    Ok(step) => Ok(step),
                    Err(b) => self.settle(cur, b),
                }
            }
            ResolvedExpr::Call(ResolvedCallee::Fn(id), args) => {
                let Some(def) = self.def(*id) else {
                    if self.inputs.recursive_fns.contains(id) {
                        self.note_closed_recursion(*id);
                    }
                    return self.args_then_settle(cur);
                };
                let outer = def.outer(args)?;
                let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                    self.mark_used(*id);
                    let body = term::subst(&def.body, &outer)?;
                    return Ok(Step::Progress(Box::new((
                        Proof::Unfold {
                            fn_id: *id,
                            arm: 0,
                            args: args.clone(),
                            binders: Vec::new(),
                            premise: None,
                        },
                        body,
                    ))));
                };
                let s = term::subst(subject, &outer)?;
                // A countdown opens only where a hypothesis states its guard,
                // or its complement (`width > 0` for `width <= 0`), or the
                // guard computes: opening it one level wherever arithmetic
                // could decide the guard would trade a call the facts in
                // scope talk about for one they do not.
                // A division down to zero opens the same way, or where linear
                // arithmetic decides its guard (`digit / 256 > 0` is false
                // for `digit < 256`).
                let halving = crate::ir::proof_steps::induct::halving(&def).is_some();
                let countdown =
                    halving || crate::ir::proof_steps::induct::countdown(&def).is_some();
                if countdown
                    && term::eval_closed(&canon(&s)).is_none()
                    && self.hyp_for(&s).is_none()
                    && self.complement(&canon(&s)).is_none()
                    && (!halving || self.decide_linearly(&canon(&s)).is_none())
                {
                    return self.settle(cur, None);
                }
                let ev = self.whnf(&s)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value) {
                    Some((k, binders, cell)) => {
                        let (_, premise) = with_cell(ev.chain, cell).finish();
                        let unfold = Proof::Unfold {
                            fn_id: *id,
                            arm: k,
                            args: args.clone(),
                            binders,
                            premise: Some(Box::new(premise)),
                        };
                        self.mark_used(*id);
                        let script = self.scratch_script();
                        let eq =
                            crate::ir::proof_steps::claim::claim(&unfold, &script, &self.hyps)?;
                        Ok(Step::Progress(Box::new((unfold, eq.rhs))))
                    }
                    // Splitting on a countdown's guard would unroll the
                    // recursion one more level in every branch.
                    None if countdown => self.settle(cur, None),
                    None => {
                        let g = ev.blocked.or_else(|| {
                            (matches!(value.ty(), Some(crate::ast::Type::Bool))
                                || matches!(value.node, ResolvedExpr::BinOp(..)))
                            .then_some(value.clone())
                        });
                        self.settle(cur, g)
                    }
                }
            }
            ResolvedExpr::Call(ResolvedCallee::Builtin(name), args)
                if matches!(name.as_str(), "Bool.and" | "Bool.or") && args.len() == 2 =>
            {
                let and = name == "Bool.and";
                let left = match self.child(cur, 0)? {
                    Ok(step) => return Ok(step),
                    Err(b) => b,
                };
                if let Some(v) = term::bool_value(&args[0]) {
                    let rule = match (and, v) {
                        (true, true) => WallRule::AndTrueL,
                        (true, false) => WallRule::AndFalseL,
                        (false, true) => WallRule::OrTrueL,
                        (false, false) => WallRule::OrFalseL,
                    };
                    return Ok(self.rule_step(rule, vec![("b".into(), args[1].clone())]));
                }
                let right = match self.child(cur, 1)? {
                    Ok(step) => return Ok(step),
                    Err(b) => b,
                };
                if let Some(v) = term::bool_value(&args[1]) {
                    let rule = match (and, v) {
                        (true, true) => WallRule::AndTrueR,
                        (true, false) => WallRule::AndFalseR,
                        (false, true) => WallRule::OrTrueR,
                        (false, false) => WallRule::OrFalseR,
                    };
                    return Ok(self.rule_step(rule, vec![("a".into(), args[0].clone())]));
                }
                self.settle(cur, left.or(right))
            }
            ResolvedExpr::Call(ResolvedCallee::Builtin(name), args)
                if name == "Bool.not" && args.len() == 1 =>
            {
                let blocked = match self.child(cur, 0)? {
                    Ok(step) => return Ok(step),
                    Err(b) => b,
                };
                match term::bool_value(&args[0]) {
                    Some(true) => Ok(self.rule_step(WallRule::NotTrue, vec![])),
                    Some(false) => Ok(self.rule_step(WallRule::NotFalse, vec![])),
                    None => self.settle(cur, blocked),
                }
            }
            ResolvedExpr::Call(..) | ResolvedExpr::BinOp(..) | ResolvedExpr::Neg(_) => {
                self.args_then_settle(cur)
            }
            ResolvedExpr::Match { subject, arms } => {
                let ev = self.whnf(subject)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value) {
                    Some((k, binders, cell)) => {
                        let (_, premise) = with_cell(ev.chain, cell).finish();
                        let arm = Proof::Arm {
                            term: cur.clone(),
                            arm: k,
                            binders,
                            premise: Box::new(premise),
                        };
                        let eq = crate::ir::proof_steps::claim::claim(
                            &arm,
                            &empty_script(),
                            &self.hyps,
                        )?;
                        Ok(Step::Progress(Box::new((arm, eq.rhs))))
                    }
                    None => {
                        // Only a Bool is split on: a subject of another
                        // type (a list) is neither `true` nor `false`.
                        let g = ev.blocked.or_else(|| {
                            (matches!(value.ty(), Some(crate::ast::Type::Bool))
                                || matches!(value.node, ResolvedExpr::BinOp(..)))
                            .then_some(value.clone())
                        });
                        self.settle(cur, g)
                    }
                }
            }
            // A list literal with an open element evaluates to its first
            // element in front of the rest; a closed one keeps the shape it
            // was written in.
            ResolvedExpr::List(xs) if !xs.is_empty() && term::eval_closed(cur).is_none() => {
                let to = term::cell_of(cur).expect("a list literal with an element");
                Ok(Step::Progress(Box::new((
                    Proof::Cell { list: cur.clone() },
                    to,
                ))))
            }
            ResolvedExpr::Ident(name) => match self.constant(name) {
                // A module-level binding reads as its value.
                Some(c) => {
                    self.mark_const_used(name);
                    Ok(Step::Progress(Box::new((
                        Proof::UnfoldConst { name: name.clone() },
                        canon(&c.value),
                    ))))
                }
                None => self.settle(cur, None),
            },
            _ => self.settle(cur, None),
        }
    }

    /// Evaluate every argument; a closed result computes, an open one
    /// settles against the hypotheses.
    fn args_then_settle(&mut self, cur: &Term) -> Result<Step, String> {
        let mut blocked = None;
        for i in 0..term::children(cur).len() {
            match self.child(cur, i)? {
                Ok(step) => return Ok(step),
                Err(b) => blocked = blocked.or(b),
            }
        }
        // A closed scalar computes; a list keeps the shape the program
        // built it in, which is the shape a match on it reads.
        if !term::is_literal(cur)
            && let Some(v) = term::eval_closed(cur)
            && !matches!(v.node, ResolvedExpr::List(_))
        {
            return Ok(Step::Progress(Box::new((
                Proof::Compute {
                    lhs: cur.clone(),
                    rhs: v.clone(),
                },
                v,
            ))));
        }
        if let Some(step) = self.list_step(cur)? {
            return Ok(step);
        }
        if let Some(step) = self.map_step(cur)? {
            return Ok(step);
        }
        if let Some(step) = self.vector_step(cur)? {
            return Ok(step);
        }
        self.settle(cur, blocked)
    }

    /// A Vector builtin takes one step by its rule where the step can
    /// decide the rule's premise: a vector of a list read back as the list
    /// (and the other way), the length after a write or of a literal-size
    /// `Vector.new`, and a read below zero, past the end, after a write
    /// (at the same index or, decided, another) or of a `Vector.new`.
    fn vector_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        use crate::ir::hir::BuiltinIntrinsic;
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let builtin = |t: &Term, want: &str, n: usize| -> Option<Vec<Term>> {
            match &t.node {
                ResolvedExpr::Call(ResolvedCallee::Builtin(b), xs)
                    if b == want && xs.len() == n =>
                {
                    Some(xs.clone())
                }
                _ => None,
            }
        };
        let made = |t: &Term| -> Option<Vec<Term>> {
            match &t.node {
                ResolvedExpr::Call(ResolvedCallee::Intrinsic(BuiltinIntrinsic::VectorNew), xs)
                    if xs.len() == 2 =>
                {
                    Some(xs.clone())
                }
                _ => None,
            }
        };
        // `Option.withDefault(Vector.set(v, i, x), v)`: the vector a write
        // leaves, or the vector itself when the index is out of range.
        let written = |t: &Term| -> Option<Vec<Term>> {
            let outer = builtin(t, "Option.withDefault", 2)?;
            let set = builtin(&outer[0], "Vector.set", 3)?;
            (canon(&set[0]) == canon(&outer[1])).then_some(set)
        };
        let decided = |env: &mut Self, t: Term| -> Result<Option<(bool, Proof)>, String> {
            let ev = env.whnf(&t)?;
            let Some(v) = term::bool_value(ev.chain.cur()) else {
                return Ok(None);
            };
            Ok(Some((v, ev.chain.finish().1)))
        };
        let int = |n: i64| term::int(&n.into());
        let vlen =
            |v: &Term| term::builtin("Vector.len", vec![v.clone()], Some(crate::ast::Type::Int));
        let in_range = |at: &Term, end: Term| {
            term::bool_and(
                term::binop(BinOp::Lte, int(0), at.clone()),
                term::binop(BinOp::Lt, at.clone(), end),
            )
        };
        let bind = |names: &[&str], values: Vec<Term>| -> Vec<(String, Term)> {
            names.iter().map(|n| n.to_string()).zip(values).collect()
        };
        let (rule, subst, premises): (WallRule, Vec<(String, Term)>, Vec<Proof>) =
            match (name.as_str(), args.as_slice()) {
                ("List.fromVector", [w]) => match builtin(w, "Vector.fromList", 1) {
                    Some(l) => (WallRule::VecToListOfList, bind(&["l"], l), vec![]),
                    None => return Ok(None),
                },
                ("Vector.fromList", [l]) => match builtin(l, "List.fromVector", 1) {
                    Some(v) => (WallRule::VecOfListToList, bind(&["v"], v), vec![]),
                    None => return Ok(None),
                },
                ("Vector.len", [w]) => {
                    if let Some(set) = written(w) {
                        (WallRule::VecLenSet, bind(&["v", "i", "x"], set), vec![])
                    } else if let Some(nx) = made(w) {
                        let ok = term::binop(BinOp::Lte, int(0), nx[0].clone());
                        let Some((true, p)) = decided(self, ok)? else {
                            return Ok(None);
                        };
                        (WallRule::VecLenNew, bind(&["n", "x"], nx), vec![p])
                    } else if builtin(w, "Vector.fromList", 1).is_some() {
                        // The length of a vector made from a list is that
                        // list's, which the list rules read.
                        (
                            WallRule::VecLenToList,
                            bind(&["v"], vec![w.clone()]),
                            vec![],
                        )
                    } else {
                        return Ok(None);
                    }
                }
                ("Vector.get", [w, j]) => {
                    if let Some(set) = written(w) {
                        let Some((true, p)) = decided(self, in_range(&set[1], vlen(&set[0])))?
                        else {
                            return Ok(None);
                        };
                        if canon(&set[1]) == canon(j) {
                            (
                                WallRule::VecGetSetSame,
                                bind(&["v", "i", "x"], set),
                                vec![p],
                            )
                        } else {
                            let ne = term::binop(BinOp::Neq, set[1].clone(), j.clone());
                            let Some((true, q)) = decided(self, ne)? else {
                                return Ok(None);
                            };
                            let mut values = set;
                            values.push(j.clone());
                            (
                                WallRule::VecGetSetOther,
                                bind(&["v", "i", "x", "j"], values),
                                vec![p, q],
                            )
                        }
                    } else if let Some(nx) = made(w) {
                        let Some((true, p)) = decided(self, in_range(j, nx[0].clone()))? else {
                            return Ok(None);
                        };
                        let mut values = nx;
                        values.push(j.clone());
                        (WallRule::VecGetNew, bind(&["n", "x", "i"], values), vec![p])
                    } else if let Some((true, p)) =
                        decided(self, term::binop(BinOp::Lt, j.clone(), int(0)))?
                    {
                        (
                            WallRule::VecGetNegative,
                            bind(&["v", "i"], vec![w.clone(), j.clone()]),
                            vec![p],
                        )
                    } else if let Some((true, p)) =
                        decided(self, term::binop(BinOp::Gte, j.clone(), vlen(w)))?
                    {
                        (
                            WallRule::VecGetPastEnd,
                            bind(&["v", "i"], vec![w.clone(), j.clone()]),
                            vec![p],
                        )
                    } else {
                        return Ok(None);
                    }
                }
                ("Vector.set", [v, i, x]) => {
                    let out = term::bool_or(
                        term::binop(BinOp::Lt, i.clone(), int(0)),
                        term::binop(BinOp::Gte, i.clone(), vlen(v)),
                    );
                    let Some((true, p)) = decided(self, out)? else {
                        return Ok(None);
                    };
                    (
                        WallRule::VecSetOutOfRange,
                        bind(&["v", "i", "x"], vec![v.clone(), i.clone(), x.clone()]),
                        vec![p],
                    )
                }
                _ => return Ok(None),
            };
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Ok(Some(Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises,
            },
            concl.rhs,
        )))))
    }

    /// `Map.get`, `Map.has` or `Map.len` of the empty map or of a
    /// `Map.set` takes one step by its rule: at the key just set (the same
    /// term), at a key the step decides is another (`k != k2`), and for the
    /// size by whether the map held the key, which is split on when open.
    fn map_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let Some(map) = args.first() else {
            return Ok(None);
        };
        let bind = |names: &[&str], values: Vec<Term>| -> Vec<(String, Term)> {
            names.iter().map(|n| n.to_string()).zip(values).collect()
        };
        let empty = matches!(&map.node, ResolvedExpr::MapLiteral(kvs) if kvs.is_empty());
        let set = match &map.node {
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), parts)
                if b == "Map.set" && parts.len() == 3 =>
            {
                Some(parts.clone())
            }
            _ => None,
        };
        let decided = |env: &mut Self, t: Term| -> Result<Option<(bool, Proof)>, String> {
            let ev = env.whnf(&t)?;
            let Some(v) = term::bool_value(ev.chain.cur()) else {
                return Ok(None);
            };
            Ok(Some((v, ev.chain.finish().1)))
        };
        let (rule, subst, premises) = match (name.as_str(), args.len(), empty, set) {
            ("Map.get", 2, true, _) => (
                WallRule::MapGetEmpty,
                bind(&["k"], vec![args[1].clone()]),
                vec![],
            ),
            ("Map.has", 2, true, _) => (
                WallRule::MapHasEmpty,
                bind(&["k"], vec![args[1].clone()]),
                vec![],
            ),
            ("Map.len", 1, true, _) => (WallRule::MapLenEmpty, vec![], vec![]),
            ("Map.get" | "Map.has", 2, false, Some(p)) => {
                let get = name == "Map.get";
                if canon(&p[1]) == canon(&args[1]) {
                    let rule = if get {
                        WallRule::MapGetSetSame
                    } else {
                        WallRule::MapHasSetSame
                    };
                    (rule, bind(&["m", "k", "v"], p), vec![])
                } else {
                    let ne = term::binop(BinOp::Neq, p[1].clone(), args[1].clone());
                    let Some((true, premise)) = decided(self, ne)? else {
                        return Ok(None);
                    };
                    let rule = if get {
                        WallRule::MapGetSetOther
                    } else {
                        WallRule::MapHasSetOther
                    };
                    let mut values = p;
                    values.push(args[1].clone());
                    (rule, bind(&["m", "k", "v", "k2"], values), vec![premise])
                }
            }
            ("Map.len", 1, false, Some(p)) => {
                let held = term::builtin(
                    "Map.has",
                    vec![p[0].clone(), p[1].clone()],
                    Some(crate::ast::Type::Bool),
                );
                match decided(self, held.clone())? {
                    Some((true, premise)) => (
                        WallRule::MapLenSetPresent,
                        bind(&["m", "k", "v"], p),
                        vec![premise],
                    ),
                    Some((false, premise)) => (
                        WallRule::MapLenSetAbsent,
                        bind(&["m", "k", "v"], p),
                        vec![premise],
                    ),
                    None => return self.settle(cur, Some(canon(&held))).map(Some),
                }
            }
            _ => return Ok(None),
        };
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Ok(Some(Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises,
            },
            concl.rhs,
        )))))
    }

    /// `t = true` from a hypothesis `Bool.and(…) = true` that holds `t` as
    /// one of its conjuncts, taken apart by `bool.and.elim_l` and `_r`.
    fn conjunct_of_hyp(&self, t: &Term) -> Option<Proof> {
        fn find(t: &Term, part: &Term, proof: Proof) -> Option<Proof> {
            if canon(part) == *t {
                return Some(proof);
            }
            let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &part.node else {
                return None;
            };
            if b != "Bool.and" || args.len() != 2 {
                return None;
            }
            let elim = |rule: WallRule| Proof::Rule {
                rule,
                subst: vec![("a".into(), args[0].clone()), ("b".into(), args[1].clone())],
                premises: vec![proof.clone()],
            };
            find(t, &args[0], elim(WallRule::AndElimL))
                .or_else(|| find(t, &args[1], elim(WallRule::AndElimR)))
        }
        let t = canon(t);
        self.hyps.iter().rev().find_map(|(name, e)| {
            (term::bool_value(&e.rhs) == Some(true))
                .then(|| find(&t, &e.lhs, Proof::Hyp(name.clone())))
                .flatten()
        })
    }

    /// A list builtin whose list argument is `[]` or `List.prepend(x, a)`
    /// takes one step by its constructor equation. A count is decided
    /// first (`n <= 0`, else `n > 0`); an undecided count is where the
    /// prover splits.
    fn list_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let Some(list) = args.first() else {
            return Ok(None);
        };
        if matches!(
            name.as_str(),
            "List.concat" | "List.len" | "List.reverse" | "List.take" | "List.drop"
        ) && let ResolvedExpr::List(xs) = &list.node
            && !xs.is_empty()
            && term::eval_closed(list).is_none()
        {
            // A literal with an open element steps as a cell; a closed one
            // keeps the shape it was written in.
            let cell = term::cell_of(list).expect("a list literal with an element");
            let ctx = term::context_at(cur, &[0]);
            let to = term::plug(&ctx, &cell);
            return Ok(Some(Step::Progress(Box::new((
                Proof::Congr {
                    ctx,
                    inner: Box::new(Proof::Cell { list: list.clone() }),
                },
                to,
            )))));
        }
        let shape = match &list.node {
            ResolvedExpr::List(xs) if xs.is_empty() => None,
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), parts)
                if b == "List.prepend" && parts.len() == 2 =>
            {
                Some((parts[0].clone(), parts[1].clone()))
            }
            _ => return Ok(None),
        };
        let bind = |names: &[&str], values: Vec<Term>| -> Vec<(String, Term)> {
            names.iter().map(|n| n.to_string()).zip(values).collect()
        };
        let (rule, subst) = match (name.as_str(), args.len(), shape) {
            ("List.concat", 2, None) => (WallRule::ConcatNil, bind(&["b"], vec![args[1].clone()])),
            ("List.concat", 2, Some((x, a))) => (
                WallRule::ConcatCons,
                bind(&["x", "a", "b"], vec![x, a, args[1].clone()]),
            ),
            ("List.len", 1, None) => (WallRule::LenNil, Vec::new()),
            ("List.len", 1, Some((x, a))) => (WallRule::LenCons, bind(&["x", "a"], vec![x, a])),
            ("List.reverse", 1, None) => (WallRule::ReverseNil, Vec::new()),
            ("List.reverse", 1, Some((x, a))) => {
                (WallRule::ReverseCons, bind(&["x", "a"], vec![x, a]))
            }
            ("List.take", 2, None) => (WallRule::TakeNil, bind(&["n"], vec![args[1].clone()])),
            ("List.drop", 2, None) => (WallRule::DropNil, bind(&["n"], vec![args[1].clone()])),
            ("List.take" | "List.drop", 2, Some((x, a))) => {
                let take = name == "List.take";
                let n = args[1].clone();
                let zero = term::int(&0.into());
                let decided =
                    |env: &mut Self, op: BinOp| -> Result<Option<(bool, Proof)>, String> {
                        let ev = env.whnf(&term::binop(op, n.clone(), zero.clone()))?;
                        let Some(v) = term::bool_value(ev.chain.cur()) else {
                            return Ok(None);
                        };
                        Ok(Some((v, ev.chain.finish().1)))
                    };
                let (rule, premise) = match decided(self, BinOp::Lte)? {
                    Some((true, p)) => (
                        if take {
                            WallRule::TakeConsLe
                        } else {
                            WallRule::DropConsLe
                        },
                        p,
                    ),
                    Some((false, _)) => match decided(self, BinOp::Gt)? {
                        Some((true, p)) => (
                            if take {
                                WallRule::TakeConsGt
                            } else {
                                WallRule::DropConsGt
                            },
                            p,
                        ),
                        _ => return Ok(None),
                    },
                    None => {
                        let at_most = canon(&term::binop(BinOp::Lte, n.clone(), zero));
                        return self.settle(cur, Some(at_most)).map(Some);
                    }
                };
                let subst = bind(&["x", "a", "n"], vec![x, a, n]);
                let (_, concl) = rule.instantiate(&subst).expect("rule binders");
                return Ok(Some(Step::Progress(Box::new((
                    Proof::Rule {
                        rule,
                        subst,
                        premises: vec![premise],
                    },
                    concl.rhs,
                )))));
            }
            _ => return Ok(None),
        };
        Ok(Some(self.rule_step(rule, subst)))
    }

    fn rule_step(&self, rule: WallRule, subst: Vec<(String, Term)>) -> Step {
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises: Vec::new(),
            },
            concl.rhs,
        )))
    }

    fn scratch_script(&mut self) -> crate::ir::proof_steps::Script {
        let mut s = empty_script();
        s.defs = self.used_defs();
        s.consts = self.used_consts();
        s
    }

    /// An Int comparison the decided comparisons in scope settle by
    /// linear arithmetic, with its value. An Int equality is settled
    /// through the two orders: both hold (`int.eq.of_le_ge`), or one
    /// strict order does (`int.eq.false_of_lt`, `_gt`).
    pub(crate) fn decide_linearly(&mut self, cur: &Term) -> Option<(Proof, Term)> {
        if let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node
            && is_int(a)
        {
            let subst = vec![
                ("a".to_string(), a.as_ref().clone()),
                ("b".to_string(), b.as_ref().clone()),
            ];
            for rule in [
                WallRule::EqOfLeGe,
                WallRule::EqFalseOfLt,
                WallRule::EqFalseOfGt,
            ] {
                let (premises, concl) = rule.instantiate(&subst)?;
                let proofs = premises
                    .iter()
                    .map(|e| self.linear_proof(&e.lhs, term::bool_value(&e.rhs)?))
                    .collect::<Option<Vec<_>>>();
                if let Some(premises) = proofs {
                    return Some((
                        Proof::Rule {
                            rule,
                            subst: subst.clone(),
                            premises,
                        },
                        concl.rhs,
                    ));
                }
            }
            return None;
        }
        [true, false]
            .into_iter()
            .find_map(|value| Some((self.linear_proof(cur, value)?, term::boolean(value))))
    }

    /// The facts a hypothesis that calls a predicate states once the
    /// predicate is opened, each with its proof: the body, from the
    /// definition's whole-body equation and the hypothesis, then the body's
    /// `Bool.and` lines. Only a predicate that does not recurse, one level,
    /// and only where a step reads it; the definition opened is marked used
    /// by the caller that takes a view.
    pub(crate) fn opened_views(&mut self) -> Vec<(Term, Proof, crate::ir::identity::FnId)> {
        let mut out = Vec::new();
        for (name, e) in self.hyps.clone() {
            if term::bool_value(&e.rhs) != Some(true) {
                continue;
            }
            let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &e.lhs.node else {
                continue;
            };
            if self.inputs.recursive_fns.contains(id) {
                continue;
            }
            let Some(def) = self.def(*id) else {
                continue;
            };
            if matches!(def.body.node, ResolvedExpr::Match { .. }) {
                continue;
            }
            let args: Vec<Term> = args.iter().map(canon).collect();
            let Ok(outer) = def.outer(&args) else {
                continue;
            };
            let Ok(body) = term::subst(&def.body, &outer) else {
                continue;
            };
            let body = canon(&body);
            let from = Proof::Trans {
                terms: vec![body.clone(), canon(&e.lhs), term::boolean(true)],
                steps: vec![
                    Proof::Symm(Box::new(Proof::Unfold {
                        fn_id: *id,
                        arm: 0,
                        args,
                        binders: Vec::new(),
                        premise: None,
                    })),
                    Proof::Hyp(name.clone()),
                ],
            };
            let mut lines = Vec::new();
            super::split_and(&body, from.clone(), &mut lines);
            out.push((body, from, *id));
            if lines.len() > 1 {
                out.extend(lines.into_iter().map(|(f, p)| (f, p, *id)));
            }
        }
        out
    }

    /// A view of a predicate hypothesis that is exactly `t`, with its proof.
    pub(crate) fn view_for(&mut self, t: &Term) -> Option<Proof> {
        let t = canon(t);
        let (_, proof, id) = self.opened_views().into_iter().find(|(f, _, _)| *f == t)?;
        self.mark_used(id);
        Some(proof)
    }

    /// `goal = value` for an Int comparison `goal` that the decided
    /// comparisons in scope settle by linear arithmetic alone.
    fn plain_linear(&self, goal: &Term, value: bool) -> Option<Proof> {
        use crate::ir::proof_steps::linear;
        let mut atoms = Vec::new();
        let mut facts = vec![linear::as_nonneg(goal, !value, &mut atoms)?];
        let mut names = Vec::new();
        for (n, e) in &self.hyps {
            let Some(v) = term::bool_value(&e.rhs) else {
                continue;
            };
            if let Some(p) = linear::as_nonneg(&e.lhs, v, &mut atoms) {
                facts.push(p);
                names.push(n.clone());
            }
        }
        let weights = linear::certificate(&facts)?;
        let mut hyps = Vec::new();
        let mut kept = vec![weights[0].clone()];
        for (n, w) in names.into_iter().zip(&weights[1..]) {
            if *w != num_bigint::BigInt::from(0) {
                hyps.push(n);
                kept.push(w.clone());
            }
        }
        Some(Proof::Linear {
            goal: canon(goal),
            value,
            hyps,
            weights: kept,
        })
    }

    /// For each quotient `a / k` in `quotients` (inner first) whose
    /// dividend lies in `0 <= a < m` for a multiple `m = n * k`, the bounds
    /// `0 <= a / k` and `a / k < n` (`int.div_range`): the range of `a`
    /// comes from a quotient ranged before it, or from a comparison in
    /// scope that bounds it by a literal and linear arithmetic.
    fn quotient_ranges(&self, quotients: &[Term]) -> Vec<(Term, Proof)> {
        use num_bigint::BigInt;
        let zero = || term::int(&BigInt::from(0));
        let mut ranged: Vec<(Term, BigInt, Proof)> = Vec::new();
        let mut out = Vec::new();
        for q in quotients {
            let ResolvedExpr::Call(_, args) = &q.node else {
                continue;
            };
            let (a, k) = (canon(&args[0]), canon(&args[1]));
            let Some(kv) = term::int_value(&k) else {
                continue;
            };
            // `Bool.and(0 <= a, a < m) = true` and `m`.
            let known = ranged.iter().find(|(t, _, _)| *t == a).cloned();
            let (m, range_a) = match known {
                Some((_, n, proof)) => (n, proof),
                None => {
                    let Some(bound) = self
                        .hyps
                        .iter()
                        .filter_map(|(_, e)| upper_bound(&e.lhs, term::bool_value(&e.rhs)?, &a))
                        .min()
                    else {
                        continue;
                    };
                    let n = (&bound + &kv - 1) / &kv;
                    if n <= BigInt::from(0) {
                        continue;
                    }
                    let m = &n * &kv;
                    let low = canon(&term::binop(BinOp::Lte, zero(), a.clone()));
                    let high = canon(&term::binop(BinOp::Lt, a.clone(), term::int(&m)));
                    let (Some(p_low), Some(p_high)) = (
                        self.plain_linear(&low, true),
                        self.plain_linear(&high, true),
                    ) else {
                        continue;
                    };
                    let both = canon(&term::bool_and(low.clone(), high.clone()));
                    let half = canon(&term::bool_and(term::boolean(true), high.clone()));
                    let proof = Proof::Trans {
                        terms: vec![both, half, high.clone(), term::boolean(true)],
                        steps: vec![
                            Proof::Congr {
                                ctx: term::bool_and(term::hole(), high.clone()),
                                inner: Box::new(p_low),
                            },
                            Proof::Rule {
                                rule: WallRule::AndTrueL,
                                subst: vec![("b".into(), high)],
                                premises: Vec::new(),
                            },
                            p_high,
                        ],
                    };
                    (m, proof)
                }
            };
            if &m % &kv != BigInt::from(0) {
                continue;
            }
            let n = &m / &kv;
            let sizes = canon(&term::bool_and(
                term::binop(BinOp::Gt, k.clone(), zero()),
                term::binop(
                    BinOp::Eq,
                    term::int(&m),
                    term::binop(BinOp::Mul, term::int(&n), k.clone()),
                ),
            ));
            let range = Proof::Rule {
                rule: WallRule::DivRange,
                subst: vec![
                    ("a".into(), a.clone()),
                    ("k".into(), k.clone()),
                    ("m".into(), term::int(&m)),
                    ("n".into(), term::int(&n)),
                ],
                premises: vec![
                    range_a,
                    Proof::Compute {
                        lhs: sizes,
                        rhs: term::boolean(true),
                    },
                ],
            };
            let low = canon(&term::binop(BinOp::Lte, zero(), q.clone()));
            let high = canon(&term::binop(BinOp::Lt, q.clone(), term::int(&n)));
            let elim = |rule: WallRule| Proof::Rule {
                rule,
                subst: vec![("a".into(), low.clone()), ("b".into(), high.clone())],
                premises: vec![range.clone()],
            };
            out.push((low.clone(), elim(WallRule::AndElimL)));
            out.push((high.clone(), elim(WallRule::AndElimR)));
            ranged.push((q.clone(), n, range));
        }
        out
    }

    /// `goal = value` for an Int comparison `goal`, when its opposite and
    /// the decided comparisons in scope add up to a contradiction. The
    /// comparisons inside an opened predicate hypothesis count too; one the
    /// certificate uses is cut in just above the step.
    pub(crate) fn linear_proof(&mut self, goal: &Term, value: bool) -> Option<Proof> {
        use crate::ir::proof_steps::linear;
        let mut known: Vec<(String, crate::ir::proof_steps::Eqn)> = self
            .hyps
            .iter()
            .filter(|(_, e)| {
                term::bool_value(&e.rhs).is_some()
                    && linear::as_nonneg(&e.lhs, true, &mut Vec::new()).is_some()
            })
            .cloned()
            .collect();
        let plain = known.len();
        let views: Vec<(Term, Proof, crate::ir::identity::FnId)> = self
            .opened_views()
            .into_iter()
            .filter(|(f, _, _)| linear::as_nonneg(f, true, &mut Vec::new()).is_some())
            .collect();
        for (i, (f, _, _)) in views.iter().enumerate() {
            known.push((
                format!("h_view{i}"),
                Eqn::new(f.clone(), term::boolean(true)),
            ));
        }
        let viewed = known.len();
        let instances = self.cited_comparisons(goal);
        for (i, (f, _, _)) in instances.iter().enumerate() {
            known.push((
                format!("h_cited{i}"),
                Eqn::new(f.clone(), term::boolean(true)),
            ));
        }
        let cited = known.len();
        let mut about: Vec<&Term> = vec![goal];
        about.extend(known.iter().map(|(_, e)| &e.lhs));
        let (mut remainders, quotients) = division_facts(&about);
        remainders.extend(self.quotient_ranges(&quotients));
        remainders.extend(self.equalities_as_orders());
        for (i, (f, _)) in remainders.iter().enumerate() {
            known.push((
                format!("h_mod{i}"),
                Eqn::new(f.clone(), term::boolean(true)),
            ));
        }
        let mut atoms = Vec::new();
        let mut facts = vec![linear::as_nonneg(goal, !value, &mut atoms)?];
        for (_, e) in &known {
            facts.push(linear::as_nonneg(
                &e.lhs,
                term::bool_value(&e.rhs)?,
                &mut atoms,
            )?);
        }
        let weights = linear::certificate(&facts)?;
        // Only the hypotheses the certificate uses are named.
        let mut hyps = Vec::new();
        let mut kept = vec![weights[0].clone()];
        let mut cuts = Vec::new();
        for (k, ((n, _), w)) in known.iter().zip(&weights[1..]).enumerate() {
            if *w == num_bigint::BigInt::from(0) {
                continue;
            }
            if k < plain {
                hyps.push(n.clone());
            } else if k < viewed {
                let (fact, proof, id) = views[k - plain].clone();
                let name = self.fresh_hyp();
                self.mark_used(id);
                hyps.push(name.clone());
                cuts.push((name, fact, proof));
            } else if k >= cited {
                let (fact, proof) = remainders[k - cited].clone();
                let name = self.fresh_hyp();
                hyps.push(name.clone());
                cuts.push((name, fact, proof));
            } else {
                let (fact, proof, law) = instances[k - viewed].clone();
                let name = self.fresh_hyp();
                if !self.laws.iter().any(|l| l.key == law.key) {
                    self.laws.push(law);
                }
                hyps.push(name.clone());
                cuts.push((name, fact, proof));
            }
            kept.push(w.clone());
        }
        let mut proof = Proof::Linear {
            goal: canon(goal),
            value,
            hyps,
            weights: kept,
        };
        for (name, fact, from) in cuts.into_iter().rev() {
            proof = Proof::Have {
                name,
                fact,
                proof: Box::new(from),
                body: Box::new(proof),
            };
        }
        Some(proof)
    }

    /// The instances of the cited laws that state an Int comparison at the
    /// atoms of `goal` they are about, each with its proof. A law's givens
    /// are fixed by one of its atoms matching one of the goal's, or by one
    /// of its products matching a product of the goal factor by factor, so
    /// only facts about what the goal mentions are offered. A law may state
    /// its comparison through a predicate that only names it (no `match`,
    /// no recursion), read through that one unfolding. A law with a `when`
    /// is used only where its `when` at the instance is proved by the facts
    /// in scope, never assumed; while that `when` is being proved, only
    /// laws without one are offered, so the search does not feed itself.
    fn cited_comparisons(
        &mut self,
        goal: &Term,
    ) -> Vec<(Term, Proof, crate::ir::proof_steps::LawRef)> {
        use super::rewrite::{matches, ordered};
        use crate::ir::proof_steps::linear;
        let mut goal_atoms = Vec::new();
        let Some(goal_poly) = linear::as_nonneg(goal, true, &mut goal_atoms) else {
            return Vec::new();
        };
        let mut out: Vec<(Term, Proof, crate::ir::proof_steps::LawRef)> = Vec::new();
        for law in self.cited_all.clone() {
            if term::bool_value(&law.rhs) != Some(true)
                || (law.premise.is_some() && self.proving_cited_when)
            {
                continue;
            }
            // The comparison the law states, as written or through one
            // unfolding of a predicate that only names it.
            let stated = if linear::as_nonneg(&law.lhs, true, &mut Vec::new()).is_some() {
                law.lhs.clone()
            } else {
                match self.wrapper_unfold(&law.lhs) {
                    Some((_, body))
                        if linear::as_nonneg(&body, true, &mut Vec::new()).is_some() =>
                    {
                        body
                    }
                    _ => continue,
                }
            };
            let mut law_atoms = Vec::new();
            let Some(law_poly) = linear::as_nonneg(&stated, true, &mut law_atoms) else {
                continue;
            };
            // Pairs of law terms and goal terms, matched part by part.
            let mut pairs: Vec<Vec<(Term, Term)>> = Vec::new();
            for pat in &law_atoms {
                for atom in &goal_atoms {
                    pairs.push(vec![(pat.clone(), atom.clone())]);
                }
            }
            for lm in law_poly.keys().filter(|m| m.len() > 1 && m.len() <= 3) {
                for gm in goal_poly.keys().filter(|m| m.len() == lm.len()) {
                    for order in permutations(gm) {
                        pairs.push(
                            lm.iter()
                                .zip(&order)
                                .map(|(l, g)| (law_atoms[*l].clone(), goal_atoms[*g].clone()))
                                .collect(),
                        );
                    }
                }
            }
            for pair in pairs {
                let mut found = Vec::new();
                if !pair
                    .iter()
                    .all(|(pat, atom)| matches(pat, atom, &law.givens, &mut found))
                {
                    continue;
                }
                let Some(subst) = ordered(&law.givens, found) else {
                    continue;
                };
                let Ok(lhs) = term::subst(&law.lhs, &subst) else {
                    continue;
                };
                let lhs = canon(&lhs);
                let Ok(fact) = term::subst(&stated, &subst) else {
                    continue;
                };
                let fact = canon(&fact);
                if out.iter().any(|(f, _, _)| *f == fact) {
                    continue;
                }
                let premise = match &law.premise {
                    None => None,
                    Some(when) => {
                        let Ok(when) = term::subst(when, &subst) else {
                            continue;
                        };
                        self.proving_cited_when = true;
                        let proof = self.discharge(&when);
                        self.proving_cited_when = false;
                        match proof {
                            Some(p) => Some(Box::new(p)),
                            None => continue,
                        }
                    }
                };
                let by_law = Proof::Law {
                    law: law.key.clone(),
                    subst,
                    premise,
                };
                let proof = if fact == lhs {
                    by_law
                } else {
                    // `fact` is the body of the predicate call `lhs`.
                    let Some((unfold, body)) = self.wrapper_unfold(&lhs) else {
                        continue;
                    };
                    if body != fact {
                        continue;
                    }
                    if let ResolvedExpr::Call(ResolvedCallee::Fn(id), _) = &lhs.node {
                        self.mark_used(*id);
                    }
                    Proof::Trans {
                        terms: vec![fact.clone(), lhs, term::boolean(true)],
                        steps: vec![Proof::Symm(Box::new(unfold)), by_law],
                    }
                };
                out.push((fact, proof, law.clone()));
            }
        }
        out
    }

    /// A law the author cited, applied left to right to a term evaluation
    /// stopped at. Two cited laws that rewrite it to different terms are a
    /// refusal: the result would depend on which is tried first.
    fn rewrite_with_cited(&mut self, cur: &Term) -> Result<Option<(Proof, Term)>, String> {
        use super::rewrite::Equation;
        let mut found: Vec<(String, Proof, Term)> = Vec::new();
        for law in self.rewrite_laws.clone() {
            let eq = Equation::Law(Box::new(law.clone()));
            if let Some((p, to)) = self.try_equation(&eq, cur) {
                // A result that holds the term it rewrote would be rewritten
                // again forever (`f(x, [])` to `g(f(x, []))`).
                if !holds(&to, &canon(cur)) {
                    found.push((law.key.clone(), p, to));
                }
            }
        }
        let Some((key, p, to)) = found.first().cloned() else {
            return Ok(None);
        };
        if let Some((other, _, _)) = found.iter().find(|(_, _, o)| canon(o) != canon(&to)) {
            return Err(format!(
                "law {key} and law {other} both rewrite `{}`, to different terms; cite only one of them",
                crate::ir::proof_steps::show::term(cur, self.inputs.symbol_table)
            ));
        }
        if let Some(law) = self.rewrite_laws.iter().find(|l| l.key == key).cloned()
            && !self.laws.iter().any(|l| l.key == key)
        {
            self.laws.push(law);
        }
        Ok(Some((p, to)))
    }

    /// Evaluate `t` and then, below its head, each part in turn, so a
    /// hypothesis or cited law applies inside a constructor too.
    pub(crate) fn normalize(&mut self, t: &Term, depth: usize) -> Result<Chain, String> {
        let mut chain = self.whnf(t)?.chain;
        if depth == 0 {
            return Ok(chain);
        }
        // Parts first; when one changes, the whole may evaluate further.
        loop {
            self.burn()?;
            let mut changed = false;
            let parts = term::children(chain.cur()).len();
            for i in 0..parts {
                let part = term::children(chain.cur())[i].clone();
                let sub = self.normalize(&part, depth - 1)?;
                if !sub.is_empty() {
                    let (to, proof) = sub.finish();
                    chain.push_at(&[i], proof, &to);
                    changed = true;
                }
            }
            if !changed {
                return Ok(chain);
            }
            let again = self.whnf(chain.cur())?.chain;
            if again.is_empty() {
                return Ok(chain);
            }
            let (to, proof) = again.finish();
            chain.push(proof, to);
        }
    }

    /// Where evaluation has no Bool to split on: the parts below the heads,
    /// a ring step, contradicting hypotheses, a disjunction or predicate in
    /// scope; failing those, an undecided Int comparison inside a `Bool.and`,
    /// `Bool.or` or `Bool.not` where a side stopped, returned to split on.
    fn nothing_to_split(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        l: &Eval,
        r: &Eval,
        depth: usize,
    ) -> Result<Result<Proof, Term>, String> {
        // Nothing to split on: look below the heads, at the parts of a
        // constructor and the arguments of a call evaluation stopped at.
        let mut nl = self.normalize(lhs, 8)?;
        let nr = self.normalize(rhs, 8)?;
        if canon(nl.cur()) == canon(nr.cur()) {
            return Ok(Ok(meet(nl, nr)));
        }
        // Two Int terms that are one polynomial: the ring step.
        if is_int(nl.cur()) && crate::ir::proof_steps::ring::same_polynomial(nl.cur(), nr.cur()) {
            let (from, to) = (nl.cur().clone(), nr.cur().clone());
            nl.push(
                Proof::Ring {
                    lhs: from,
                    rhs: to.clone(),
                },
                to,
            );
            return Ok(Ok(meet(nl, nr)));
        }
        // Two terms that differ only in Int parts that are one polynomial
        // (`(u + d) - u` and `d` inside a text): a ring step at each part.
        // Not inside a record literal, which Lean may state as a value with
        // the proof of its invariant (a refined type).
        if !has_record(nl.cur())
            && !has_record(nr.cur())
            && let Some(steps) = super::rewrite::ring_bridge(nl.cur(), nr.cur())
            && !steps.is_empty()
        {
            for (proof, to) in steps {
                nl.push(proof, to);
            }
            return Ok(Ok(meet(nl, nr)));
        }
        // The same, where linear arithmetic shows such parts equal.
        if !has_record(nl.cur())
            && !has_record(nr.cur())
            && let Some(steps) = self.linear_bridge(nl.cur(), nr.cur())
            && !steps.is_empty()
        {
            for (proof, to) in steps {
                nl.push(proof, to);
            }
            return Ok(Ok(meet(nl, nr)));
        }
        // Hypotheses that contradict each other linearly: the case
        // cannot happen.
        if let Some(contradiction) = self.refute_linearly() {
            return Ok(Ok(Proof::Absurd {
                contradiction: Box::new(contradiction),
                lhs: canon(lhs),
                rhs: canon(rhs),
            }));
        }
        // A disjunction in scope: split on its left side, and where
        // that is false, its right side holds.
        if depth > 0
            && let Some(proof) = self.split_disjunction(nl.cur(), nr.cur(), depth)?
        {
            let rc = nr.cur().clone();
            nl.push(proof, rc);
            return Ok(Ok(meet(nl, nr)));
        }
        // A predicate hypothesis that chooses by a Bool: open it at the
        // arm its guard selects, splitting on the guard when it is open.
        if depth > 0
            && let Some(proof) = self.open_hypothesis(lhs, rhs, depth)?
        {
            return Ok(Ok(proof));
        }
        // A hypothesis that calls a function evaluation can open (at a
        // list cell or a constructor): evaluated, it contradicts its stated
        // value and closes the case, or its lines are cut in.
        if depth > 0
            && let Some(proof) = self.evaluate_hypothesis(lhs, rhs, depth)?
        {
            return Ok(Ok(proof));
        }
        if depth > 0
            && let Some(g) = [l.chain.cur(), r.chain.cur()]
                .into_iter()
                .find_map(|t| self.connective_comparison(t))
        {
            return Ok(Err(g));
        }
        Err(self.stopped_at(nl.cur(), nr.cur()))
    }

    /// The first Int comparison, not decided by a hypothesis, among the
    /// leaves of the `Bool.and` / `Bool.or` / `Bool.not` tree `t`.
    fn connective_comparison(&self, t: &Term) -> Option<Term> {
        match &t.node {
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), args)
                if matches!(b.as_str(), "Bool.and" | "Bool.or" | "Bool.not") =>
            {
                args.iter().find_map(|a| self.connective_comparison(a))
            }
            ResolvedExpr::BinOp(
                BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Eq | BinOp::Neq,
                a,
                b,
            ) if (is_int(a) || is_int(b)) && self.hyp_for(t).is_none() => Some(canon(t)),
            _ => None,
        }
    }

    /// Prove `lhs = rhs` by evaluating both sides, splitting on the first
    /// undecided Bool either side stops at.
    pub(crate) fn prove_by_evaluation(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Proof, String> {
        let l = self.whnf(lhs)?;
        let r = self.whnf(rhs)?;
        if canon(l.chain.cur()) == canon(r.chain.cur()) {
            return Ok(meet(l.chain, r.chain));
        }
        let g = match l.blocked.clone().or(r.blocked.clone()) {
            Some(g) => g,
            None => match self.nothing_to_split(lhs, rhs, &l, &r, depth)? {
                Ok(proof) => return Ok(proof),
                Err(g) => g,
            },
        };
        if depth == 0 || is_bool_value(&g) || self.hyp_for(&g).is_some() {
            return Err(self.stopped_at(l.chain.cur(), r.chain.cur()));
        }
        // Each branch goes on from where evaluation stopped, not from the
        // two sides again.
        let (lc, rc) = (l.chain.cur().clone(), r.chain.cur().clone());
        let hyp = self.fresh_hyp();
        let branch = |env: &mut Self, v: bool| -> Result<Proof, String> {
            env.hyps
                .push((hyp.clone(), Eqn::new(canon(&g), term::boolean(v))));
            let out = env.prove_by_evaluation(&lc, &rc, depth - 1);
            env.hyps.pop();
            out
        };
        let if_true = branch(self, true)?;
        let if_false = branch(self, false)?;
        let mut left = l.chain;
        left.push(
            Proof::Cases {
                on: canon(&g),
                hyp,
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            },
            rc,
        );
        Ok(meet(left, r.chain))
    }
}

impl Env<'_> {
    /// `true = false` (or the other way round), when the comparisons in
    /// scope contradict each other: one of them, by a linear step over
    /// the rest, has the other value.
    fn refute_linearly(&mut self) -> Option<Proof> {
        for (name, e) in self.hyps.clone() {
            let Some(said) = term::bool_value(&e.rhs) else {
                continue;
            };
            // `x != y` stated true, or `x == y` stated false, between Ints
            // the orders in scope make equal.
            if let ResolvedExpr::BinOp(op @ (BinOp::Eq | BinOp::Neq), x, y) = &e.lhs.node
                && (*op == BinOp::Neq) == said
                && is_int(x)
                && let Some((equal, value)) = self.decide_linearly(&canon(&term::binop(
                    BinOp::Eq,
                    (**x).clone(),
                    (**y).clone(),
                )))
                && term::bool_value(&value) == Some(true)
            {
                let other = match op {
                    BinOp::Eq => equal,
                    _ => Proof::Rule {
                        rule: WallRule::NeFalseOfEq,
                        subst: vec![("a".into(), canon(x)), ("b".into(), canon(y))],
                        premises: vec![equal],
                    },
                };
                return Some(Proof::Trans {
                    terms: vec![term::boolean(!said), canon(&e.lhs), term::boolean(said)],
                    steps: vec![Proof::Symm(Box::new(other)), Proof::Hyp(name)],
                });
            }
            if crate::ir::proof_steps::linear::as_nonneg(&e.lhs, said, &mut Vec::new()).is_none() {
                continue;
            }
            let Some(other) = self.linear_proof(&e.lhs, !said) else {
                continue;
            };
            return Some(Proof::Trans {
                terms: vec![term::boolean(!said), canon(&e.lhs), term::boolean(said)],
                steps: vec![Proof::Symm(Box::new(other)), Proof::Hyp(name)],
            });
        }
        None
    }

    /// `lhs = rhs` by cases on the left side `p` of a disjunction
    /// `Bool.or(p, q)` the hypotheses state (or one they open to), when
    /// neither side is decided yet: with `p` true, and with `p` false and
    /// `q` cut in as true. `None` when there is no such disjunction.
    fn split_disjunction(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Option<Proof>, String> {
        let mut found: Vec<(Term, Proof, Option<crate::ir::identity::FnId>)> = self
            .hyps
            .iter()
            .filter(|(_, e)| term::bool_value(&e.rhs) == Some(true))
            .map(|(n, e)| (canon(&e.lhs), Proof::Hyp(n.clone()), None))
            .collect();
        found.extend(
            self.opened_views()
                .into_iter()
                .map(|(f, p, id)| (f, p, Some(id))),
        );
        for (fact, from, id) in found {
            let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &fact.node else {
                continue;
            };
            if b != "Bool.or" || args.len() != 2 {
                continue;
            }
            let (p, q) = (canon(&args[0]), canon(&args[1]));
            let decided = |env: &Self, t: &Term| {
                env.hyp_for(t)
                    .and_then(|(n, v)| Some((n, term::bool_value(&v)?)))
            };
            // One side already false: the other side holds, no split.
            let one_false = match (decided(self, &p), decided(self, &q)) {
                (Some((_, true)), _) | (_, Some((_, true))) => continue,
                (Some((n, false)), None) => Some((n, q.clone(), true)),
                (None, Some((n, false))) => Some((n, p.clone(), false)),
                (Some(_), Some(_)) => continue,
                (None, None) => None,
            };
            if let Some(id) = id {
                self.mark_used(id);
            }
            if let Some((hyp, kept, left)) = one_false {
                let (rule, ctx, collapsed) = if left {
                    (
                        WallRule::OrFalseL,
                        term::bool_or(term::hole(), kept.clone()),
                        term::bool_or(term::boolean(false), kept.clone()),
                    )
                } else {
                    (
                        WallRule::OrFalseR,
                        term::bool_or(kept.clone(), term::hole()),
                        term::bool_or(kept.clone(), term::boolean(false)),
                    )
                };
                let binder = if left { "b" } else { "a" };
                let holds = Proof::Trans {
                    terms: vec![
                        kept.clone(),
                        canon(&collapsed),
                        fact.clone(),
                        term::boolean(true),
                    ],
                    steps: vec![
                        Proof::Symm(Box::new(Proof::Rule {
                            rule,
                            subst: vec![(binder.into(), kept.clone())],
                            premises: Vec::new(),
                        })),
                        Proof::Symm(Box::new(Proof::Congr {
                            ctx,
                            inner: Box::new(Proof::Hyp(hyp)),
                        })),
                        from,
                    ],
                };
                let name = self.fresh_hyp();
                self.hyps
                    .push((name.clone(), Eqn::new(kept.clone(), term::boolean(true))));
                let body = self.prove_by_evaluation(lhs, rhs, depth - 1);
                self.hyps.pop();
                return Ok(Some(Proof::Have {
                    name,
                    fact: kept,
                    proof: Box::new(holds),
                    body: Box::new(body?),
                }));
            }
            let on = self.fresh_hyp();
            let right = self.fresh_hyp();
            self.hyps
                .push((on.clone(), Eqn::new(p.clone(), term::boolean(true))));
            let if_true = self.prove_by_evaluation(lhs, rhs, depth - 1);
            self.hyps.pop();
            let if_true = if_true?;
            // `q = Bool.or(false, q) = Bool.or(p, q) = true`.
            let or_false = canon(&term::bool_or(term::boolean(false), q.clone()));
            let q_holds = Proof::Trans {
                terms: vec![q.clone(), or_false, fact.clone(), term::boolean(true)],
                steps: vec![
                    Proof::Symm(Box::new(Proof::Rule {
                        rule: WallRule::OrFalseL,
                        subst: vec![("b".into(), q.clone())],
                        premises: Vec::new(),
                    })),
                    Proof::Symm(Box::new(Proof::Congr {
                        ctx: term::bool_or(term::hole(), q.clone()),
                        inner: Box::new(Proof::Hyp(on.clone())),
                    })),
                    from,
                ],
            };
            self.hyps
                .push((on.clone(), Eqn::new(p.clone(), term::boolean(false))));
            self.hyps
                .push((right.clone(), Eqn::new(q.clone(), term::boolean(true))));
            let if_false = self.prove_by_evaluation(lhs, rhs, depth - 1);
            self.hyps.pop();
            self.hyps.pop();
            let if_false = if_false?;
            return Ok(Some(Proof::Cases {
                on: p,
                hyp: on,
                if_true: Box::new(if_true),
                if_false: Box::new(Proof::Have {
                    name: right,
                    fact: q,
                    proof: Box::new(q_holds),
                    body: Box::new(if_false),
                }),
            }));
        }
        Ok(None)
    }

    /// `lhs = rhs` once the first hypothesis `f(args) = true` not yet
    /// opened, where `f`'s body is a `match` on a Bool with one arm for
    /// `true` and one for `false`, is opened: its guard decided by a
    /// hypothesis or by linear arithmetic, or split on otherwise; in each
    /// case the lines of the selected arm (its `Bool.and` parts) are cut in
    /// as hypotheses. `None` when no hypothesis is such a call.
    fn open_hypothesis(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Option<Proof>, String> {
        let found = self.hyps.iter().rev().find_map(|(name, e)| {
            if term::bool_value(&e.rhs) != Some(true) || self.opened.contains(&canon(&e.lhs)) {
                return None;
            }
            let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &e.lhs.node else {
                return None;
            };
            Some((name.clone(), canon(&e.lhs), *id, args.clone()))
        });
        let Some((name, fact, id, args)) = found else {
            return Ok(None);
        };
        let Some(def) = self.def(id) else {
            return Ok(None);
        };
        let ResolvedExpr::Match { subject, arms } = &def.body.node else {
            return Ok(None);
        };
        let arm_of = |v: bool| {
            arms.iter().position(|a| {
                matches!(&a.pattern, ResolvedPattern::Literal(crate::ast::Literal::Bool(b)) if *b == v)
            })
        };
        let (Some(at_true), Some(at_false)) = (arm_of(true), arm_of(false)) else {
            return Ok(None);
        };
        if arms.len() != 2 {
            return Ok(None);
        }
        let args: Vec<Term> = args.iter().map(canon).collect();
        let guard = canon(&term::subst(subject, &def.outer(&args)?)?);
        self.mark_used(id);
        self.opened.push(fact.clone());
        // The arm for `value`, its lines cut in, then the claim under them.
        let open = |env: &mut Self, value: bool, premise: Proof| -> Result<Proof, String> {
            let k = (if value { at_true } else { at_false }) as u32 + 1;
            let unfold = Proof::Unfold {
                fn_id: id,
                arm: k,
                args: args.clone(),
                binders: Vec::new(),
                premise: Some(Box::new(premise)),
            };
            let script = env.scratch_script();
            let body = crate::ir::proof_steps::claim::claim(&unfold, &script, &env.hyps)?.rhs;
            let from = Proof::Trans {
                terms: vec![body.clone(), fact.clone(), term::boolean(true)],
                steps: vec![Proof::Symm(Box::new(unfold)), Proof::Hyp(name.clone())],
            };
            let mut lines = Vec::new();
            super::split_and(&body, from, &mut lines);
            let saved = env.hyps.len();
            let mut cuts = Vec::new();
            for (line, proof) in lines {
                let h = env.fresh_hyp();
                env.hyps
                    .push((h.clone(), Eqn::new(line.clone(), term::boolean(true))));
                cuts.push((h, line, proof));
            }
            let body = env.prove_by_evaluation(lhs, rhs, depth - 1);
            env.hyps.truncate(saved);
            let mut proof = body?;
            for (h, line, from) in cuts.into_iter().rev() {
                proof = Proof::Have {
                    name: h,
                    fact: line,
                    proof: Box::new(from),
                    body: Box::new(proof),
                };
            }
            Ok(proof)
        };
        let decided = match self.hyp_for(&guard) {
            Some((h, v)) => term::bool_value(&v).map(|b| (Proof::Hyp(h), b)),
            None => self
                .decide_linearly(&guard)
                .and_then(|(p, v)| Some((p, term::bool_value(&v)?))),
        };
        let out = match decided {
            Some((premise, value)) => open(self, value, premise),
            None => {
                let h = self.fresh_hyp();
                let branch = |env: &mut Self, v: bool| -> Result<Proof, String> {
                    env.hyps
                        .push((h.clone(), Eqn::new(guard.clone(), term::boolean(v))));
                    let out = open(env, v, Proof::Hyp(h.clone()));
                    env.hyps.pop();
                    out
                };
                match (branch(self, true), branch(self, false)) {
                    (Ok(t), Ok(f)) => Ok(Proof::Cases {
                        on: guard.clone(),
                        hyp: h.clone(),
                        if_true: Box::new(t),
                        if_false: Box::new(f),
                    }),
                    (Err(e), _) | (_, Err(e)) => Err(e),
                }
            }
        };
        self.opened.pop();
        out.map(Some)
    }

    /// `lhs = rhs` once the innermost hypothesis `f(args) = b` not yet
    /// evaluated, whose call evaluation takes a step on, is evaluated (the
    /// hypothesis itself out of scope meanwhile, so it does not answer for
    /// itself): when it ends at the other Bool, the case cannot happen;
    /// when it ends elsewhere and `b` is true, its `Bool.and` lines are cut
    /// in as hypotheses. `None` when no hypothesis evaluates further.
    fn evaluate_hypothesis(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Option<Proof>, String> {
        for i in (0..self.hyps.len()).rev() {
            let (name, e) = self.hyps[i].clone();
            let Some(said) = term::bool_value(&e.rhs) else {
                continue;
            };
            let fact = canon(&e.lhs);
            if !matches!(fact.node, ResolvedExpr::Call(ResolvedCallee::Fn(_), _))
                || self.opened.contains(&fact)
                || self.barren.contains(&fact)
                || self.hyps[i + 1..].iter().any(|(n, _)| *n == name)
            {
                continue;
            }
            // A short evaluation only: a hypothesis that takes long to
            // evaluate is a computation, not a shape to read. One that
            // evaluates no further is not tried again in this attempt.
            let held = self.hyps.remove(i);
            let fuel = self.fuel;
            self.fuel = fuel.min(HYPOTHESIS_FUEL);
            let ev = self.whnf(&fact);
            self.fuel = fuel - (fuel.min(HYPOTHESIS_FUEL) - self.fuel);
            self.hyps.insert(i, held);
            let Ok(ev) = ev else {
                self.barren.push(fact);
                continue;
            };
            if ev.chain.is_empty() {
                self.barren.push(fact);
                continue;
            }
            let (to, chain) = ev.chain.finish();
            // `to = said`, from the evaluation and the hypothesis.
            let from = Proof::Trans {
                terms: vec![to.clone(), fact.clone(), term::boolean(said)],
                steps: vec![Proof::Symm(Box::new(chain)), Proof::Hyp(name.clone())],
            };
            if term::bool_value(&to) == Some(!said) {
                return Ok(Some(Proof::Absurd {
                    contradiction: Box::new(from),
                    lhs: canon(lhs),
                    rhs: canon(rhs),
                }));
            }
            if !said {
                continue;
            }
            // Only lines not stated true already: with nothing new, the
            // claim would only be tried again as it was.
            let mut lines = Vec::new();
            super::split_and(&to, from, &mut lines);
            lines.retain(|(line, _)| {
                term::bool_value(line).is_none()
                    && self.hyp_for(line).and_then(|(_, v)| term::bool_value(&v)) != Some(true)
            });
            if lines.is_empty() {
                continue;
            }
            self.opened.push(fact.clone());
            let saved = self.hyps.len();
            let mut cuts = Vec::new();
            for (line, proof) in lines {
                let h = self.fresh_hyp();
                self.hyps
                    .push((h.clone(), Eqn::new(line.clone(), term::boolean(true))));
                cuts.push((h, line, proof));
            }
            let body = self.prove_by_evaluation(lhs, rhs, depth - 1);
            self.hyps.truncate(saved);
            self.opened.pop();
            let mut proof = body?;
            for (h, line, from) in cuts.into_iter().rev() {
                proof = Proof::Have {
                    name: h,
                    fact: line,
                    proof: Box::new(from),
                    body: Box::new(proof),
                };
            }
            return Ok(Some(proof));
        }
        Ok(None)
    }

    /// The refusal for two sides evaluation cannot bring together: both
    /// sides as it left them, and the hypotheses in scope.
    pub(crate) fn stopped_at(&mut self, lhs: &Term, rhs: &Term) -> String {
        use crate::ir::proof_steps::show;
        self.note_fact_hints(&[lhs, rhs]);
        let names = self.inputs.symbol_table;
        let mut s = format!(
            "evaluation stops at `{}` and `{}`",
            show::term(lhs, names),
            show::term(rhs, names)
        );
        if !self.hyps.is_empty() {
            s.push_str(", under ");
            s.push_str(
                &self
                    .hyps
                    .iter()
                    .map(|(n, e)| format!("{n}: {}", show::eqn(e, names)))
                    .collect::<Vec<_>>()
                    .join(", "),
            );
        }
        self.with_open_premises(s)
    }
}

impl Env<'_> {
    /// Each builtin fact the law does not cite whose left side matches a
    /// part of `terms`, one match each: pattern matching on the term as it
    /// stands, never a rewrite followed by another match.
    fn note_fact_hints(&mut self, terms: &[&Term]) {
        use crate::ir::proof_steps::{facts, show};
        fn parts<'t>(t: &'t Term, out: &mut Vec<&'t Term>) {
            out.push(t);
            for c in term::children(t) {
                parts(c, out);
            }
        }
        let mut all = Vec::new();
        for t in terms {
            parts(t, &mut all);
        }
        for fact in facts::all() {
            if self.rewrite_laws.iter().any(|l| l.key == fact.key) {
                continue;
            }
            let ob = &fact.script.obligation;
            if let Some(part) = all
                .iter()
                .find(|p| super::rewrite::matches(&ob.lhs, p, &ob.givens, &mut Vec::new()))
            {
                let hint = format!(
                    "`{}` rewrites `{}`; add it to `using`",
                    fact.key,
                    show::term(part, self.inputs.symbol_table)
                );
                if !self.hints.contains(&hint) {
                    self.hints.push(hint);
                }
            }
        }
    }
}

pub(crate) fn empty_script() -> crate::ir::proof_steps::Script {
    use crate::ir::proof_steps::{Obligation, Script};
    Script {
        obligation: Obligation {
            key: String::new(),
            givens: Vec::new(),
            finite: Vec::new(),
            lists: Vec::new(),
            ints: Vec::new(),
            premise: None,
            lhs: term::boolean(true),
            rhs: term::boolean(true),
        },
        defs: Vec::new(),
        consts: Vec::new(),
        laws: Vec::new(),
        proof: Proof::Refl(term::boolean(true)),
    }
}
