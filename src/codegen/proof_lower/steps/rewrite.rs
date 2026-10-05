//! Rewriting with equations: cited laws and wall rules, oriented left to
//! right, applied at the outermost-leftmost position whose premises the
//! producer can prove (closed computation, a hypothesis, or a range fact
//! derived from one).

use crate::ast::BinOp;
use crate::ir::hir::{BuiltinIntrinsic, ResolvedCallee, ResolvedExpr, ResolvedStrPart};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, LawRef, Proof, WallRule};

use super::chain::Chain;
use super::env::Env;

/// Same node apart from children.
fn head_eq(a: &Term, b: &Term) -> bool {
    use ResolvedExpr as E;
    match (&a.node, &b.node) {
        (E::Literal(x), E::Literal(y)) => {
            x == y || term::int_value(a).is_some() && term::int_value(a) == term::int_value(b)
        }
        (E::Ident(x), E::Ident(y)) => x == y,
        (E::Attr(_, f), E::Attr(_, g)) => f == g,
        (E::Call(c, xs), E::Call(d, ys)) => c == d && xs.len() == ys.len(),
        (E::BinOp(o, ..), E::BinOp(p, ..)) => o == p,
        (E::Neg(_), E::Neg(_)) => true,
        (E::Ctor(c, xs), E::Ctor(d, ys)) => c == d && xs.len() == ys.len(),
        (E::List(xs), E::List(ys)) | (E::Tuple(xs), E::Tuple(ys)) => xs.len() == ys.len(),
        (
            E::RecordCreate {
                type_name: s,
                fields: f,
                ..
            },
            E::RecordCreate {
                type_name: t,
                fields: g,
                ..
            },
        ) => s == t && f.iter().map(|x| &x.0).eq(g.iter().map(|x| &x.0)),
        (E::InterpolatedStr(p), E::InterpolatedStr(q)) => {
            p.len() == q.len()
                && p.iter().zip(q).all(|pair| match pair {
                    (ResolvedStrPart::Literal(x), ResolvedStrPart::Literal(y)) => x == y,
                    (ResolvedStrPart::Parsed(_), ResolvedStrPart::Parsed(_)) => true,
                    _ => false,
                })
        }
        _ => false,
    }
}

/// First-order matching of `pat` (variables `vars`) against `t`.
pub(crate) fn matches(
    pat: &Term,
    t: &Term,
    vars: &[String],
    out: &mut Vec<(String, Term)>,
) -> bool {
    if let ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } = &pat.node
        && vars.contains(n)
    {
        if let Some((_, bound)) = out.iter().find(|(k, _)| k == n) {
            return canon(bound) == canon(t);
        }
        out.push((n.clone(), canon(t)));
        return true;
    }
    if term::int_value(pat).is_some() || term::int_value(t).is_some() {
        return term::int_value(pat).is_some() && term::int_value(pat) == term::int_value(t);
    }
    if matches!(pat.node, ResolvedExpr::Match { .. }) {
        return false;
    }
    if !head_eq(pat, t) {
        return false;
    }
    let (ps, ts) = (term::children(pat), term::children(t));
    ps.len() == ts.len() && ps.iter().zip(ts).all(|(p, c)| matches(p, c, vars, out))
}

pub(crate) fn ordered(vars: &[String], found: Vec<(String, Term)>) -> Option<Vec<(String, Term)>> {
    vars.iter()
        .map(|v| found.iter().find(|(k, _)| k == v).cloned())
        .collect()
}

/// An equation the rewriter may apply left to right.
#[derive(Clone)]
pub(crate) enum Equation {
    Law(Box<LawRef>),
    Wall(WallRule),
    /// The whole-body equation (arm 0) of a definition.
    Unfold(crate::ir::identity::FnId),
}

impl Env<'_> {
    /// Prove `t = true`, for a Bool `t`.
    pub(crate) fn discharge(&mut self, t: &Term) -> Option<Proof> {
        let t = canon(t);
        if let Some(v) = term::eval_closed(&t)
            && term::bool_value(&v) == Some(true)
        {
            return Some(Proof::Compute {
                lhs: t,
                rhs: term::boolean(true),
            });
        }
        if let Some((name, v)) = self.hyp_for(&t)
            && term::bool_value(&v) == Some(true)
        {
            return Some(Proof::Hyp(name));
        }
        // A conjunct of a true hypothesis.
        for (name, e) in self.hyps.clone() {
            if term::bool_value(&e.rhs) != Some(true) {
                continue;
            }
            if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &e.lhs.node
                && b == "Bool.and"
                && args.len() == 2
            {
                for (i, rule) in [(0, WallRule::AndElimL), (1, WallRule::AndElimR)] {
                    if canon(&args[i]) == t {
                        return Some(Proof::Rule {
                            rule,
                            subst: vec![
                                ("a".into(), args[0].clone()),
                                ("b".into(), args[1].clone()),
                            ],
                            premises: vec![Proof::Hyp(name.clone())],
                        });
                    }
                }
            }
        }
        // A fact inside a hypothesis that calls a predicate.
        if let Some(proof) = self.view_for(&t) {
            return Some(proof);
        }
        // `0 <= e && e < n`: a range fact.
        if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
            && b == "Bool.and"
            && args.len() == 2
            && let ResolvedExpr::BinOp(BinOp::Lte, z, e) = &args[0].node
            && term::int_value(z) == Some(0.into())
            && let ResolvedExpr::BinOp(BinOp::Lt, e2, n) = &args[1].node
            && canon(e) == canon(e2)
            && let Some(n) = term::int_value(n)
            && let Some(p) = self.range(e, &n, 0)
        {
            return Some(p);
        }
        // A conjunction, one conjunct at a time.
        if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
            && b == "Bool.and"
            && args.len() == 2
        {
            return self.discharge_conjuncts(&t).ok();
        }
        // A comparison the decided comparisons in scope settle linearly.
        self.linear_proof(&t, true)
    }

    /// Prove `t = true` for a conjunction (`Bool.and`, nested) one conjunct
    /// at a time, each as [`Self::discharge`] proves a single fact, then
    /// put the conjunction back together: `Bool.and(a, b)` is
    /// `Bool.and(true, b)` by `a = true`, which is `b`, which is `true`.
    /// The first conjunct no proof is found for comes back.
    pub(crate) fn discharge_conjuncts(&mut self, t: &Term) -> Result<Proof, Box<Term>> {
        let t = canon(t);
        let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node else {
            return self.discharge(&t).ok_or(Box::new(t));
        };
        if b != "Bool.and" || args.len() != 2 {
            return self.discharge(&t).ok_or(Box::new(t));
        }
        let (a, b) = (canon(&args[0]), canon(&args[1]));
        let pa = match self.discharge(&a) {
            Some(p) => p,
            None => return Err(Box::new(self.first_open_conjunct(&a))),
        };
        let pb = match self.discharge(&b) {
            Some(p) => p,
            None => return Err(Box::new(self.first_open_conjunct(&b))),
        };
        let truth = term::boolean(true);
        Ok(Proof::Trans {
            terms: vec![
                t.clone(),
                canon(&term::bool_and(truth.clone(), b.clone())),
                b.clone(),
                truth,
            ],
            steps: vec![
                Proof::Congr {
                    ctx: term::bool_and(term::hole(), b.clone()),
                    inner: Box::new(pa),
                },
                Proof::Rule {
                    rule: WallRule::AndTrueL,
                    subst: vec![("b".into(), b)],
                    premises: Vec::new(),
                },
                pb,
            ],
        })
    }

    /// Every conjunct of `t`, innermost, that [`Self::discharge`] does not
    /// prove.
    pub(crate) fn open_conjuncts(&mut self, t: &Term) -> Vec<Term> {
        let t = canon(t);
        if self.discharge(&t).is_some() {
            return Vec::new();
        }
        if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
            && b == "Bool.and"
            && args.len() == 2
        {
            let mut out = self.open_conjuncts(&args[0]);
            out.extend(self.open_conjuncts(&args[1]));
            if !out.is_empty() {
                return out;
            }
        }
        vec![t]
    }

    /// The innermost conjunct of `t` that [`Self::discharge`] does not prove.
    pub(crate) fn first_open_conjunct(&mut self, t: &Term) -> Term {
        match self.discharge_conjuncts(t) {
            Ok(_) => canon(t),
            Err(open) => *open,
        }
    }

    fn range(&mut self, e: &Term, n: &num_bigint::BigInt, depth: usize) -> Option<Proof> {
        let want = term::bool_and(
            term::binop(BinOp::Lte, term::int(&0.into()), e.clone()),
            term::binop(BinOp::Lt, e.clone(), term::int(n)),
        );
        if let Some((name, v)) = self.hyp_for(&want)
            && term::bool_value(&v) == Some(true)
        {
            return Some(Proof::Hyp(name));
        }
        if depth > 16 {
            return None;
        }
        let ResolvedExpr::Call(ResolvedCallee::Intrinsic(BuiltinIntrinsic::IntDivEuclid), args) =
            &e.node
        else {
            return None;
        };
        let k = term::int_value(&args[1])?;
        if k <= 0.into() {
            return None;
        }
        let m = n * &k;
        let inner = self.range(&args[0], &m, depth + 1)?;
        let subst = vec![
            ("a".to_string(), args[0].clone()),
            ("k".to_string(), args[1].clone()),
            ("m".to_string(), term::int(&m)),
            ("n".to_string(), term::int(n)),
        ];
        let (premises, _) = WallRule::DivRange.instantiate(&subst)?;
        Some(Proof::Rule {
            rule: WallRule::DivRange,
            subst,
            premises: vec![
                inner,
                Proof::Compute {
                    lhs: premises[1].lhs.clone(),
                    rhs: term::boolean(true),
                },
            ],
        })
    }

    pub(crate) fn try_equation(&mut self, eq: &Equation, t: &Term) -> Option<(Proof, Term)> {
        match eq {
            Equation::Law(law) => {
                let mut found = Vec::new();
                if !matches(&law.lhs, t, &law.givens, &mut found) {
                    return None;
                }
                let subst = ordered(&law.givens, found)?;
                let premise = match &law.premise {
                    Some(when) => {
                        let when = term::subst(when, &subst).ok()?;
                        match self.discharge(&when) {
                            Some(p) => Some(Box::new(p)),
                            None => {
                                let open = self.open_conjuncts(&when);
                                self.note_open_premise(&law.key, &subst, &open);
                                return None;
                            }
                        }
                    }
                    None => None,
                };
                let rhs = term::subst(&law.rhs, &subst).ok()?;
                Some((
                    Proof::Law {
                        law: law.key.clone(),
                        subst,
                        premise,
                    },
                    rhs,
                ))
            }
            Equation::Unfold(id) => {
                let ResolvedExpr::Call(ResolvedCallee::Fn(f), args) = &t.node else {
                    return None;
                };
                if f != id {
                    return None;
                }
                let def = self.def(*id)?;
                let outer = def.outer(args).ok()?;
                let body = term::subst(&def.body, &outer).ok()?;
                self.mark_used(*id);
                Some((
                    Proof::Unfold {
                        fn_id: *id,
                        arm: 0,
                        args: args.clone(),
                        binders: Vec::new(),
                        premise: None,
                    },
                    body,
                ))
            }
            Equation::Wall(rule) => {
                let (premises, concl) = rule.schema();
                let vars: Vec<String> = rule.binders().iter().map(|s| s.to_string()).collect();
                let mut found = Vec::new();
                if !matches(&concl.lhs, t, &vars, &mut found) {
                    return None;
                }
                let subst = ordered(&vars, found)?;
                let mut proofs = Vec::new();
                for p in premises {
                    let p = Eqn::new(
                        term::subst(&p.lhs, &subst).ok()?,
                        term::subst(&p.rhs, &subst).ok()?,
                    );
                    if term::bool_value(&p.rhs) != Some(true) {
                        return None;
                    }
                    proofs.push(self.discharge(&p.lhs)?);
                }
                Some((
                    Proof::Rule {
                        rule: *rule,
                        subst: subst.clone(),
                        premises: proofs,
                    },
                    term::subst(&concl.rhs, &subst).ok()?,
                ))
            }
        }
    }

    /// The leftmost-innermost closed arithmetic subterm, computed.
    fn compute_somewhere(t: &Term, path: &mut Vec<usize>) -> Option<(Vec<usize>, Term)> {
        for (i, c) in term::children(t).into_iter().enumerate() {
            path.push(i);
            if let Some(found) = Self::compute_somewhere(c, path) {
                return Some(found);
            }
            path.pop();
        }
        if !term::is_literal(t)
            && matches!(
                t.node,
                ResolvedExpr::BinOp(..) | ResolvedExpr::Call(ResolvedCallee::Intrinsic(_), _)
            )
            && let Some(v) = term::eval_closed(t)
        {
            return Some((path.clone(), v));
        }
        None
    }

    /// The name of an equation, for a refusal.
    pub(crate) fn equation_name(&self, eq: &Equation) -> String {
        use crate::ir::proof_steps::sexpr::Names;
        match eq {
            Equation::Law(law) => format!("law {}", law.key),
            Equation::Wall(rule) => format!("rule {}", rule.id()),
            Equation::Unfold(id) => {
                format!(
                    "the definition of {}",
                    self.inputs.symbol_table.fn_name(*id)
                )
            }
        }
    }

    /// The outermost-leftmost position some equation rewrites. Where two
    /// equations rewrite the same position to different terms the outcome
    /// would depend on which one is tried first, so that is a refusal.
    fn rewrite_somewhere(
        &mut self,
        eqs: &[Equation],
        t: &Term,
        path: &mut Vec<usize>,
    ) -> Result<Option<(Vec<usize>, Proof, Term)>, String> {
        let mut found: Vec<(usize, Proof, Term)> = Vec::new();
        for (i, eq) in eqs.iter().enumerate() {
            if let Some((p, to)) = self.try_equation(eq, t) {
                // A result that holds its own redex would be rewritten again
                // forever; that equation does not apply here.
                if !holds_term(&to, &canon(t)) {
                    found.push((i, p, to));
                }
            }
        }
        if let Some((first, p, to)) = found.first().cloned() {
            if let Some((other, _, _)) = found.iter().find(|(_, _, o)| canon(o) != canon(&to)) {
                return Err(format!(
                    "{} and {} both rewrite `{}`, to different terms; cite only one of them",
                    self.equation_name(&eqs[first]),
                    self.equation_name(&eqs[*other]),
                    crate::ir::proof_steps::show::term(t, self.inputs.symbol_table)
                ));
            }
            return Ok(Some((path.clone(), p, to)));
        }
        for (i, c) in term::children(t).into_iter().enumerate() {
            path.push(i);
            if let Some(found) = self.rewrite_somewhere(eqs, c, path)? {
                return Ok(Some(found));
            }
            path.pop();
        }
        Ok(None)
    }

    /// One application of the first equation that applies at the root.
    pub(crate) fn apply_at_root(&mut self, eqs: &[Equation], t: &Term) -> Option<(Proof, Term)> {
        eqs.iter().find_map(|eq| self.try_equation(eq, t))
    }

    /// Rewrite `t` to normal form under the given laws and wall rules.
    pub(crate) fn rewrite(&mut self, t: &Term, eqs: &[Equation]) -> Result<Chain, String> {
        let mut chain = Chain::new(t);
        let mut seen: Vec<Term> = vec![canon(t)];
        for _ in 0..400 {
            self.burn()?;
            let cur = chain.cur().clone();
            if let Some((path, v)) = Self::compute_somewhere(&cur, &mut Vec::new()) {
                let sub = term::at(&cur, &path).clone();
                chain.push_at(
                    &path,
                    Proof::Compute {
                        lhs: sub,
                        rhs: v.clone(),
                    },
                    &v,
                );
                continue;
            }
            match self.rewrite_somewhere(eqs, &cur, &mut Vec::new())? {
                Some((path, proof, to)) => {
                    let name =
                        crate::ir::proof_steps::show::rule_name(&proof, self.inputs.symbol_table);
                    chain.push_at(&path, proof, &to);
                    let next = canon(chain.cur());
                    if seen.contains(&next) {
                        return Err(format!(
                            "rewriting comes back to `{}` after {name}: the cited laws loop",
                            crate::ir::proof_steps::show::term(&next, self.inputs.symbol_table)
                        ));
                    }
                    seen.push(next);
                }
                None => return Ok(chain),
            }
        }
        Err(format!(
            "rewriting keeps growing `{}` and never reaches a normal form",
            crate::ir::proof_steps::show::term(chain.cur(), self.inputs.symbol_table)
        ))
    }
}

/// Why a law cannot be a left-to-right rewrite rule: its right side holds
/// its left side again with only the givens renamed (a commutativity law,
/// for one), so rewriting with it applies it again to its own result
/// forever. A right side that holds the left side at smaller arguments (a
/// recursion step) is kept; rewriting refuses by name if it ever comes back
/// to a term it already had.
pub(crate) fn loops(law: &LawRef) -> Option<String> {
    fn holds_instance(pat: &Term, t: &Term, vars: &[String]) -> bool {
        let mut found = Vec::new();
        let renaming = matches(pat, t, vars, &mut found)
            && found
                .iter()
                .all(|(_, v)| matches!(&v.node, ResolvedExpr::Ident(n) if vars.contains(n)));
        renaming
            || term::children(t)
                .into_iter()
                .any(|c| holds_instance(pat, c, vars))
    }
    if let ResolvedExpr::Ident(n) = &law.lhs.node
        && law.givens.contains(n)
    {
        return Some(format!(
            "law {} has a bare given on its left side, which every term matches",
            law.key
        ));
    }
    holds_instance(&law.lhs, &law.rhs, &law.givens).then(|| {
        format!(
            "law {} rewrites a term into one it applies to again, so rewriting with it never stops; it is not used as a rewrite rule",
            law.key
        )
    })
}

/// Whether `part` occurs in `t`.
fn holds_term(t: &Term, part: &Term) -> bool {
    canon(t) == *part || term::children(t).into_iter().any(|c| holds_term(c, part))
}

fn is_int_term(t: &Term) -> bool {
    matches!(t.ty(), Some(crate::ast::Type::Int)) || term::int_value(t).is_some()
}

/// Steps proving `a = b` when the two differ only in Int parts that are the
/// same polynomial: a ring step at each such part, under congruence. `None`
/// when they differ anywhere else; no steps when they are the same term.
pub(crate) fn ring_bridge(a: &Term, b: &Term) -> Option<Vec<(Proof, Term)>> {
    let (a, b) = (canon(a), canon(b));
    if a == b {
        return Some(Vec::new());
    }
    if (is_int_term(&a) || is_int_term(&b)) && crate::ir::proof_steps::ring::same_polynomial(&a, &b)
    {
        return Some(vec![(
            Proof::Ring {
                lhs: a,
                rhs: b.clone(),
            },
            b,
        )]);
    }
    if !head_eq(&a, &b) || matches!(a.node, ResolvedExpr::Match { .. }) {
        return None;
    }
    let n = term::children(&a).len();
    if n != term::children(&b).len() || n == 0 {
        return None;
    }
    let mut steps = Vec::new();
    let mut cur = a.clone();
    for i in 0..n {
        let (x, y) = (
            term::children(&cur)[i].clone(),
            term::children(&b)[i].clone(),
        );
        let inner = ring_bridge(&x, &y)?;
        for (proof, to) in inner {
            let ctx = term::context_at(&cur, &[i]);
            let next = canon(&term::plug(&ctx, &to));
            steps.push((
                Proof::Congr {
                    ctx,
                    inner: Box::new(proof),
                },
                next.clone(),
            ));
            cur = next;
        }
    }
    (cur == b).then_some(steps)
}

impl Env<'_> {
    /// `x = y` by one instance of a cited law, either way round: one side
    /// of the law matches its side of the equation, which fixes every
    /// given, and the other side of the instance is the other side of the
    /// equation up to Int parts that are the same polynomial. Its `when`
    /// is discharged as any premise is. Commutativity applies this way
    /// too, since nothing is rewritten again.
    pub(crate) fn direct_equal(&mut self, x: &Term, y: &Term) -> Option<Proof> {
        let (x, y) = (canon(x), canon(y));
        for law in self.cited_all.clone() {
            for flip in [false, true] {
                let (pl, pr) = if flip {
                    (&law.rhs, &law.lhs)
                } else {
                    (&law.lhs, &law.rhs)
                };
                // Either side may fix the givens; the other is bridged.
                for (anchor, target) in [(pl, &x), (pr, &y)] {
                    let mut found = Vec::new();
                    if !matches(anchor, target, &law.givens, &mut found) {
                        continue;
                    }
                    let Some(subst) = ordered(&law.givens, found) else {
                        continue;
                    };
                    let (Ok(il), Ok(ir)) = (term::subst(pl, &subst), term::subst(pr, &subst))
                    else {
                        continue;
                    };
                    let (il, ir) = (canon(&il), canon(&ir));
                    let (Some(before), Some(after)) = (ring_bridge(&x, &il), ring_bridge(&ir, &y))
                    else {
                        continue;
                    };
                    let premise = match &law.premise {
                        None => None,
                        Some(when) => {
                            let when = term::subst(when, &subst).ok()?;
                            match self.discharge(&when) {
                                Some(p) => Some(Box::new(p)),
                                None => {
                                    let open = self.open_conjuncts(&when);
                                    self.note_open_premise(&law.key, &subst, &open);
                                    continue;
                                }
                            }
                        }
                    };
                    let mut step = Proof::Law {
                        law: law.key.clone(),
                        subst,
                        premise,
                    };
                    if flip {
                        step = Proof::Symm(Box::new(step));
                    }
                    if !self.laws.iter().any(|l| l.key == law.key) {
                        self.laws.push(law.clone());
                    }
                    let mut chain = super::chain::Chain::new(&x);
                    for (p, to) in before {
                        chain.push(p, to);
                    }
                    chain.push(step, ir.clone());
                    for (p, to) in after {
                        chain.push(p, to);
                    }
                    return Some(chain.finish().1);
                }
            }
        }
        None
    }
}
