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

fn ordered(vars: &[String], found: Vec<(String, Term)>) -> Option<Vec<(String, Term)>> {
    vars.iter()
        .map(|v| found.iter().find(|(k, _)| k == v).cloned())
        .collect()
}

/// An equation the rewriter may apply left to right.
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
        // `0 <= e && e < n`: a range fact.
        if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
            && b == "Bool.and"
            && args.len() == 2
            && let ResolvedExpr::BinOp(BinOp::Lte, z, e) = &args[0].node
            && term::int_value(z) == Some(0.into())
            && let ResolvedExpr::BinOp(BinOp::Lt, e2, n) = &args[1].node
            && canon(e) == canon(e2)
            && let Some(n) = term::int_value(n)
        {
            return self.range(e, &n, 0);
        }
        None
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

    fn try_equation(&mut self, eq: &Equation, t: &Term) -> Option<(Proof, Term)> {
        match eq {
            Equation::Law(law) => {
                let mut found = Vec::new();
                if !matches(&law.lhs, t, &law.givens, &mut found) {
                    return None;
                }
                let subst = ordered(&law.givens, found)?;
                let premise = match &law.premise {
                    Some(when) => Some(Box::new(self.discharge(&term::subst(when, &subst).ok()?)?)),
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

    fn rewrite_somewhere(
        &mut self,
        eqs: &[Equation],
        t: &Term,
        path: &mut Vec<usize>,
    ) -> Option<(Vec<usize>, Proof, Term)> {
        for eq in eqs {
            if let Some((p, to)) = self.try_equation(eq, t) {
                return Some((path.clone(), p, to));
            }
        }
        for (i, c) in term::children(t).into_iter().enumerate() {
            path.push(i);
            if let Some(found) = self.rewrite_somewhere(eqs, c, path) {
                return Some(found);
            }
            path.pop();
        }
        None
    }

    /// One application of the first equation that applies at the root.
    pub(crate) fn apply_at_root(&mut self, eqs: &[Equation], t: &Term) -> Option<(Proof, Term)> {
        eqs.iter().find_map(|eq| self.try_equation(eq, t))
    }

    /// Rewrite `t` to normal form under the given laws and wall rules.
    pub(crate) fn rewrite(&mut self, t: &Term, eqs: &[Equation]) -> Result<Chain, String> {
        let mut chain = Chain::new(t);
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
            match self.rewrite_somewhere(eqs, &cur, &mut Vec::new()) {
                Some((path, proof, to)) => chain.push_at(&path, proof, &to),
                None => return Ok(chain),
            }
        }
        Err("rewriting did not reach a normal form".into())
    }
}
