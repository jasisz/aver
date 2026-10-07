//! A split on a constructor: where evaluation stops at a call of a
//! definition whose body is a `match` on a subject of a sum or list type
//! that is not a constructor yet, the proof goes on in one case per
//! constructor of that type, with the subject stated equal to it. The
//! cases are the ones the `match` itself reads, so nothing is guessed.

use crate::ast::Type;
use crate::ir::hir::{ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedPattern};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, SplitCase, SplitCtor};

use super::env::Env;

/// The names a constructor's fields take, with their types.
type Fields = Vec<(String, Option<Type>)>;

/// A subject to split on: the call whose `match` reads it, the subject at
/// that call, and each constructor with the names its fields take in the
/// arms (or `x`) and their types.
pub(crate) struct Stuck {
    fn_id: FnId,
    args: Vec<Term>,
    on: Term,
    ctors: Vec<(SplitCtor, Fields)>,
}

fn mentions_any(t: &Term, names: &[String]) -> bool {
    let mut fv = Vec::new();
    term::free_vars(t, &mut fv);
    fv.iter().any(|n| names.contains(n))
}

impl Env<'_> {
    /// The constructors a `match` with these arms splits its subject into:
    /// `[]` and `[h, ..t]` for a list pattern, every variant of the sum type
    /// of a constructor pattern otherwise. `None` for literal or tuple
    /// patterns, a record, or Option and Result.
    fn split_ctors(
        &self,
        arms: &[crate::ir::hir::ResolvedMatchArm],
        subject_ty: Option<&Type>,
    ) -> Option<Vec<(SplitCtor, Fields)>> {
        use crate::ir::hir::BuiltinCtor;
        let mut list = false;
        let mut sum = None;
        let mut builtin = None;
        for arm in arms {
            match &arm.pattern {
                ResolvedPattern::EmptyList | ResolvedPattern::Cons(..) => list = true,
                ResolvedPattern::Ctor(ResolvedCtor::User { type_id, .. }, _) => {
                    sum = Some(*type_id)
                }
                ResolvedPattern::Ctor(ResolvedCtor::Builtin(b), _) => builtin = Some(*b),
                ResolvedPattern::Wildcard | ResolvedPattern::Ident(_) => {}
                _ => return None,
            }
        }
        let names_in = |pick: &dyn Fn(&ResolvedPattern) -> bool, n: usize| -> Vec<String> {
            arms.iter()
                .find(|a| pick(&a.pattern))
                .map(|a| match &a.pattern {
                    ResolvedPattern::Cons(h, t) => vec![h.clone(), t.clone()],
                    ResolvedPattern::Ctor(_, ns) => ns.clone(),
                    _ => Vec::new(),
                })
                .filter(|ns| ns.len() == n)
                .unwrap_or_else(|| vec!["_".to_string(); n])
                .into_iter()
                .map(|s| if s == "_" { "x".to_string() } else { s })
                .collect()
        };
        // Option and Result in the order Lean declares them (`none`
        // before `some`, `error` before `ok`), each field typed by the
        // subject's type.
        if let Some(b) = builtin {
            if list || sum.is_some() {
                return None;
            }
            let one = |c: BuiltinCtor, ty: Option<Type>| {
                let name = names_in(
                    &|p| matches!(p, ResolvedPattern::Ctor(ResolvedCtor::Builtin(x), _) if *x == c),
                    1,
                );
                (
                    SplitCtor::Ctor(ResolvedCtor::Builtin(c)),
                    vec![(name[0].clone(), ty)],
                )
            };
            return match (b, subject_ty) {
                (BuiltinCtor::OptionSome | BuiltinCtor::OptionNone, Some(Type::Option(t))) => {
                    Some(vec![
                        (
                            SplitCtor::Ctor(ResolvedCtor::Builtin(BuiltinCtor::OptionNone)),
                            Vec::new(),
                        ),
                        one(BuiltinCtor::OptionSome, Some((**t).clone())),
                    ])
                }
                (BuiltinCtor::ResultOk | BuiltinCtor::ResultErr, Some(Type::Result(ok, err))) => {
                    Some(vec![
                        one(BuiltinCtor::ResultErr, Some((**err).clone())),
                        one(BuiltinCtor::ResultOk, Some((**ok).clone())),
                    ])
                }
                _ => None,
            };
        }
        match (list, sum) {
            (true, None) => {
                let elem = match subject_ty {
                    Some(Type::List(e)) => Some((**e).clone()),
                    _ => None,
                };
                let cons = names_in(&|p| matches!(p, ResolvedPattern::Cons(..)), 2);
                Some(vec![
                    (SplitCtor::Nil, Vec::new()),
                    (
                        SplitCtor::Cons,
                        vec![
                            (cons[0].clone(), elem),
                            (cons[1].clone(), subject_ty.cloned()),
                        ],
                    ),
                ])
            }
            (false, Some(type_id)) => {
                let symbols = self.inputs.symbol_table;
                let entry = symbols.type_entry_if_present(type_id)?;
                if entry.is_product || entry.is_capability_resource {
                    return None;
                }
                let crate::ast::TypeDef::Sum { variants, .. } =
                    super::finite::type_def(self.inputs, entry.key.scope_str(), &entry.key.name)?
                else {
                    return None;
                };
                if variants.len() != entry.variants.len() {
                    return None;
                }
                let mut out = Vec::new();
                for (c, v) in entry.variants.iter().zip(variants) {
                    let ctor = ResolvedCtor::User {
                        ctor_id: *c,
                        type_id,
                        name: symbols.ctor_entry(*c).name.clone(),
                    };
                    let names = names_in(
                        &|p| matches!(p, ResolvedPattern::Ctor(ResolvedCtor::User { ctor_id, .. }, _) if ctor_id == c),
                        v.fields.len(),
                    );
                    let fields = names
                        .into_iter()
                        .zip(&v.fields)
                        .map(|(n, f)| (n, Some(crate::types::parse_type_str(f))))
                        .collect();
                    out.push((SplitCtor::Ctor(ctor), fields));
                }
                Some(out)
            }
            _ => None,
        }
    }

    /// The innermost call in `t` of a definition whose `match` stops at its
    /// subject as it stands (an argument's call before the call, the
    /// subject's own calls before the subject). A recursive definition only
    /// when `recursive`: in a goal, induction is the step for those.
    pub(crate) fn stuck_subject(&mut self, t: &Term, recursive: bool) -> Option<Stuck> {
        if matches!(t.node, ResolvedExpr::Match { .. }) {
            return None;
        }
        for c in term::children(t) {
            if let Some(found) = self.stuck_subject(c, recursive) {
                return Some(found);
            }
        }
        let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &t.node else {
            return None;
        };
        if !recursive && self.inputs.recursive_fns.contains(id) {
            return None;
        }
        let def = self.def(*id)?;
        let ResolvedExpr::Match { subject, arms } = &def.body.node else {
            return None;
        };
        let ctors = self.split_ctors(arms, subject.ty())?;
        let outer = def.outer(args).ok()?;
        let on = canon(&term::subst(subject, &outer).ok()?);
        // The subject's type, which a variable put in its place may lack.
        if on.ty().is_none()
            && let Some(ty) = subject.ty()
        {
            on.set_ty(ty.clone());
        }
        if let Some(inner) = self.stuck_subject(&on, recursive) {
            return Some(inner);
        }
        // A given of a finite type, or a field of one, is split into every
        // value instead (see [`super::finite`]).
        let mut root = &on;
        while let ResolvedExpr::Attr(o, _) = &root.node {
            root = o;
        }
        let finite = matches!(&root.node, ResolvedExpr::Ident(n) if self.finite.contains(n));
        if finite || mentions_any(&on, &self.split_binders) || self.hyp_for(&on).is_some() {
            return None;
        }
        let fuel = self.fuel;
        let ev = self.whnf(&on);
        self.fuel = fuel;
        let ev = ev.ok()?;
        if !ev.chain.is_empty() || super::eval::select_arm(arms, &on).is_some() {
            return None;
        }
        Some(Stuck {
            fn_id: *id,
            args: args.iter().map(canon).collect(),
            on,
            ctors,
        })
    }

    /// `lhs = rhs` in one case per constructor of `st.on`, each under the
    /// hypothesis that `st.on` is that constructor applied to fresh names.
    pub(crate) fn split_on(
        &mut self,
        st: Stuck,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Proof, String> {
        let hyp = self.fresh_hyp();
        let mut taken = self.givens.clone();
        for t in [lhs, rhs, &st.on] {
            term::free_vars(t, &mut taken);
        }
        for (n, e) in &self.hyps {
            taken.push(n.clone());
            term::free_vars(&e.lhs, &mut taken);
            term::free_vars(&e.rhs, &mut taken);
        }
        taken.extend(self.split_binders.iter().cloned());
        let mut cases = Vec::new();
        for (ctor, fields) in st.ctors {
            let mut binders = Vec::new();
            for (base, _) in &fields {
                let start = self.split_binders.len();
                let name = (0..)
                    .map(|k| format!("{base}_c{}", start + k))
                    .find(|c| !taken.contains(c) && self.constant(c).is_none())
                    .expect("an unused name");
                taken.push(name.clone());
                binders.push(name);
            }
            let case = SplitCase {
                ctor,
                binders: binders.clone(),
                proof: Proof::Refl(term::boolean(true)),
            };
            let value = case.value();
            if let ResolvedExpr::Ctor(_, vars) | ResolvedExpr::Call(_, vars) = &value.node {
                for (v, (_, ty)) in vars.iter().zip(&fields) {
                    if let Some(ty) = ty {
                        v.set_ty(ty.clone());
                    }
                }
            }
            if let Some(ty) = st.on.ty() {
                value.set_ty(ty.clone());
            }
            // A hypothesis that evaluated no further before may now.
            let saved = self.split_binders.len();
            let barren = std::mem::take(&mut self.barren);
            self.split_binders.extend(binders);
            self.hyps
                .push((hyp.clone(), Eqn::new(st.on.clone(), value.clone())));
            // Each hypothesis about `st.on` is stated again about the
            // constructor, so it still applies once evaluation has read
            // `st.on` as it; then one stated before the split may evaluate
            // to the other Bool, and the case cannot happen.
            let cuts = self.restated(&st.on, &hyp, &value);
            let before = self.hyps.len();
            for (name, fact, said, _, _) in &cuts {
                self.hyps
                    .push((name.clone(), Eqn::new(fact.clone(), term::boolean(*said))));
            }
            let proof = match self.refute_a_hypothesis() {
                Ok(Some(contradiction)) => Ok(Proof::Absurd {
                    contradiction: Box::new(contradiction),
                    lhs: canon(lhs),
                    rhs: canon(rhs),
                }),
                Ok(None) => self.prove_by_evaluation(lhs, rhs, depth - 1),
                Err(e) => Err(e),
            }
            .map(|body| {
                cuts.into_iter()
                    .rev()
                    .fold(body, |body, (name, fact, said, to_fact, from)| {
                        if said {
                            // `fact = e.lhs = true`.
                            return Proof::Have {
                                name,
                                proof: Box::new(Proof::Trans {
                                    terms: vec![fact.clone(), from.0.clone(), term::boolean(true)],
                                    steps: vec![Proof::Symm(Box::new(to_fact)), Proof::Hyp(from.1)],
                                }),
                                fact,
                                body: Box::new(body),
                            };
                        }
                        // A hypothesis stated false: where `fact` is true,
                        // `true = fact = e.lhs = false`.
                        let contradiction = Proof::Trans {
                            terms: vec![
                                term::boolean(true),
                                fact.clone(),
                                from.0.clone(),
                                term::boolean(false),
                            ],
                            steps: vec![
                                Proof::Symm(Box::new(Proof::Hyp(name.clone()))),
                                Proof::Symm(Box::new(to_fact)),
                                Proof::Hyp(from.1),
                            ],
                        };
                        Proof::Cases {
                            on: fact,
                            hyp: name,
                            if_true: Box::new(Proof::Absurd {
                                contradiction: Box::new(contradiction),
                                lhs: canon(lhs),
                                rhs: canon(rhs),
                            }),
                            if_false: Box::new(body),
                        }
                    })
            });
            self.hyps.truncate(before);
            self.hyps.pop();
            self.split_binders.truncate(saved);
            self.barren = barren;
            cases.push(SplitCase {
                proof: proof?,
                ..case
            });
        }
        self.mark_used(st.fn_id);
        Ok(Proof::Split {
            fn_id: st.fn_id,
            args: st.args,
            on: st.on,
            hyp,
            cases,
        })
    }

    /// Each hypothesis with a Bool value whose left side holds `on`, with
    /// `on` read as `value` throughout: a fresh name, the restated fact, the
    /// value, the proof that the old left side equals the fact (one
    /// congruence step from `split : on = value` per place `on` stands),
    /// and the old left side with the hypothesis's name. A true one is cut
    /// in; a false one by a split on the fact, whose true case contradicts
    /// it.
    #[allow(clippy::type_complexity)]
    fn restated(
        &mut self,
        on: &Term,
        split: &str,
        value: &Term,
    ) -> Vec<(String, Term, bool, Proof, (Term, String))> {
        fn places(t: &Term, part: &Term, path: &mut Vec<usize>, out: &mut Vec<Vec<usize>>) {
            if canon(t) == *part {
                out.push(path.clone());
                return;
            }
            if matches!(t.node, ResolvedExpr::Match { .. }) {
                return;
            }
            for (i, c) in term::children(t).into_iter().enumerate() {
                path.push(i);
                places(c, part, path, out);
                path.pop();
            }
        }
        let mut out = Vec::new();
        for (name, e) in self.hyps.clone() {
            let Some(said) = term::bool_value(&e.rhs) else {
                continue;
            };
            if name == split {
                continue;
            }
            let mut at = Vec::new();
            places(&e.lhs, on, &mut Vec::new(), &mut at);
            if at.is_empty() {
                continue;
            }
            let mut terms = vec![canon(&e.lhs)];
            let mut steps = Vec::new();
            let mut cur = canon(&e.lhs);
            for path in &at {
                let ctx = term::context_at(&cur, path);
                cur = canon(&term::plug(&ctx, value));
                steps.push(Proof::Congr {
                    ctx,
                    inner: Box::new(Proof::Hyp(split.to_string())),
                });
                terms.push(cur.clone());
            }
            let mut to_fact = match steps.len() {
                1 => steps.pop().expect("one step"),
                _ => Proof::Trans { terms, steps },
            };
            // A comparison is evaluated as well, so linear arithmetic reads
            // its parts as the case has them.
            if matches!(e.lhs.node, ResolvedExpr::BinOp(..))
                && let Ok(ev) = self.whnf(&cur)
                && !ev.chain.is_empty()
                && term::bool_value(ev.chain.cur()).is_none()
            {
                let (to, chain) = ev.chain.finish();
                to_fact = Proof::Trans {
                    terms: vec![canon(&e.lhs), cur.clone(), canon(&to)],
                    steps: vec![to_fact, chain],
                };
                cur = canon(&to);
            }
            out.push((self.fresh_hyp(), cur, said, to_fact, (canon(&e.lhs), name)));
        }
        out
    }
}
