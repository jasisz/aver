//! A part of a proof written out twice or more in one scope is proved once
//! there, as a cut ([`Proof::Have`]) at the top of that scope, and cited by
//! name where it stood. Both kernels check a cut's proof once and read its
//! name after, so the script and the time to check it shrink with it.
//!
//! A cut states a Bool fact, `fact = true`. A part that proves `a = true`
//! is cut as `a`; one that proves `a = b` of a type whose `==` is equality
//! as `a == b`, from which the rule `int.eq.of_beq` gives `a = b` back.
//!
//! A scope is the top of the script, or where a step puts something new in
//! scope: the body of a cut, a case of a split, of an induction. A part is
//! moved to the top of a scope over the cuts and Bool cases between, but
//! only when it reads no hypothesis they name, and, where it opens a
//! definition at an arm a hypothesis must rule earlier literal arms out of
//! (the kernel finds `(v == k) = false` among the hypotheses in scope),
//! only over cases on a Bool that is not such an equality. It is never
//! moved out of a case of a split or an induction, where the hypotheses in
//! scope are not the ones above.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::ast::BinOp;
use crate::ir::hir::ResolvedExpr;
use crate::ir::proof_steps::claim::{Hyps, claim};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, Proof, Script, WallRule};

/// Least weight (see [`weight`]) a part must have to be cut out.
const MIN_WEIGHT: usize = 24;
/// Most parts cut out at the top of one scope.
const MAX_CUTS: usize = 48;
/// Most walks over one scope.
const MAX_ROUNDS: usize = 3;

/// `script`'s proof with each part it writes out twice or more in one scope
/// proved once at the top of that scope. `beq` says whether `==` on a type is
/// equality.
pub(super) fn share(script: &Script, beq: &dyn Fn(&crate::ast::Type) -> bool) -> Proof {
    let mut hyps: Hyps = Vec::new();
    if let Some(p) = &script.obligation.premise {
        hyps.push(("when".into(), Eqn::new(canon(p), term::boolean(true))));
    }
    // Names after any a cut already made took.
    fn taken(p: &Proof, most: &mut usize) {
        if let Proof::Have { name, .. } = p
            && let Some(n) = name.strip_prefix("h_share").and_then(|n| n.parse().ok())
        {
            *most = (*most).max(n);
        }
        super::each_part(p, &mut |q| taken(q, most));
    }
    let mut names = 0;
    taken(&script.proof, &mut names);
    let mut cx = Cx { script, beq, names };
    let mut proof = script.proof.clone();
    // The cuts a script starts with (what its premise holds, the reasons
    // of a `because` chain) stay where they are: the Lean export reads
    // them there. Each of their proofs, and what they prove, is a scope.
    let mut at = &mut proof;
    while let Proof::Have {
        name,
        fact,
        proof: cut,
        body,
    } = at
    {
        cx.scope(cut, &hyps);
        hyps.push((name.clone(), Eqn::new(canon(fact), term::boolean(true))));
        at = body;
    }
    cx.scope(at, &hyps);
    proof
}

struct Cx<'a> {
    script: &'a Script,
    beq: &'a dyn Fn(&crate::ast::Type) -> bool,
    names: usize,
}

/// What one walk over a scope found about a part: how often it stands
/// there, whether every place may move to the top, its weight and the part.
struct Seen {
    count: usize,
    movable: bool,
    weight: usize,
    /// The part, kept once it is seen a second time.
    first: Option<Proof>,
}

/// What the walk carries down to a part: the hypothesis names the cuts and
/// cases above it in the scope put in scope, and whether one of the cases
/// is on an equality.
#[derive(Clone, Default)]
struct Path {
    bound: Vec<String>,
    eq_case: bool,
}

/// What a part reads: the hypotheses it names that it does not bind
/// itself, whether it opens a definition or a `match` at an arm, and
/// whether it holds a split or an induction (which reads what is in scope
/// where it stands).
#[derive(Default, Clone)]
struct Reads {
    hyps: Vec<String>,
    arms: bool,
    pinned: bool,
}

impl Reads {
    fn add(&mut self, other: Reads) {
        for h in other.hyps {
            if !self.hyps.contains(&h) {
                self.hyps.push(h);
            }
        }
        self.arms |= other.arms;
        self.pinned |= other.pinned;
    }

    fn unbind(&mut self, name: &str) {
        self.hyps.retain(|h| h != name);
    }
}

/// Hash `t` by its shape into `h`, and count its parts.
fn term_hash(t: &Term, h: &mut impl Hasher) -> usize {
    let (key, has_parts) = super::egraph::head(t);
    key.hash(h);
    let mut n = 1;
    if has_parts {
        for c in term::children(t) {
            n += term_hash(c, h);
        }
    }
    n
}

/// The number of parts of `t`.
fn parts(t: &Term) -> usize {
    1 + term::children(t).into_iter().map(parts).sum::<usize>()
}

/// Whether `t` writes a `match`.
fn has_match(t: &Term) -> bool {
    matches!(t.node, ResolvedExpr::Match { .. }) || term::children(t).into_iter().any(has_match)
}

impl Cx<'_> {
    /// Share within the scope `p` is the top of, whose hypotheses are
    /// `hyps`; then within each scope inside it.
    fn scope(&mut self, p: &mut Proof, hyps: &Hyps) {
        // The cuts made here, in the order they were made.
        let mut cuts: Vec<(String, Term, Proof)> = Vec::new();
        let mut failed: Vec<u64> = Vec::new();
        // Each round walks the scope once and cuts what it found, largest
        // saving first; a later round finds what the cuts made equal.
        for _ in 0..MAX_ROUNDS {
            let mut seen: HashMap<u64, Seen> = HashMap::new();
            for (_, _, q) in &cuts {
                walk(q, &Path::default(), &mut seen);
            }
            walk(p, &Path::default(), &mut seen);
            let mut cands: Vec<(usize, u64)> = seen
                .iter()
                .filter(|(fp, s)| {
                    s.count >= 2 && s.movable && s.weight >= MIN_WEIGHT && !failed.contains(fp)
                })
                .map(|(fp, s)| ((s.count - 1) * s.weight, *fp))
                .collect();
            cands.sort_by(|a, b| b.0.cmp(&a.0).then(a.1.cmp(&b.1)));
            let mut made = false;
            for (_, fp) in cands {
                if cuts.len() >= MAX_CUTS {
                    break;
                }
                let s = &seen[&fp];
                let Some(first) = s.first.clone() else {
                    continue;
                };
                let mut scope_hyps = hyps.clone();
                for (n, fact, _) in &cuts {
                    scope_hyps.push((n.clone(), Eqn::new(fact.clone(), term::boolean(true))));
                }
                let name = format!("h_share{}", self.names + 1);
                let Some((fact, proof, used)) = self.cut_of(&first, &scope_hyps, &name) else {
                    failed.push(fp);
                    continue;
                };
                // A cut made earlier in the round may hold some of the
                // places: count them again.
                let mut count = 0;
                for (_, _, q) in cuts.iter_mut() {
                    count += replace(q, fp, &first, &used);
                }
                count += replace(p, fp, &first, &used);
                // The cut, its statement and each place that cites it
                // against the places as they were.
                let cost = 1 + parts(&fact) + super::weight(&proof) + super::weight(&used) * count;
                if count < 2 || s.weight * count <= cost + MIN_WEIGHT {
                    for (_, _, q) in cuts.iter_mut() {
                        put_back(q, &used, &first);
                    }
                    put_back(p, &used, &first);
                    failed.push(fp);
                    continue;
                }
                self.names += 1;
                cuts.push((name, fact, proof));
                made = true;
            }
            if !made {
                break;
            }
        }
        // Each scope inside, with what it adds.
        for (_, _, q) in cuts.iter_mut() {
            self.inner(q, hyps);
        }
        self.inner(p, hyps);
        // The cuts at the top, each after the ones its proof cites.
        let mut placed: Vec<(String, Term, Proof)> = Vec::new();
        while !cuts.is_empty() {
            let i = cuts
                .iter()
                .position(|(_, _, q)| !cuts.iter().any(|(n, _, _)| super::uses_hyp(q, n)))
                .unwrap_or(0);
            placed.push(cuts.remove(i));
        }
        for (name, fact, proof) in placed.into_iter().rev() {
            let body = std::mem::replace(p, Proof::Hyp(String::new()));
            *p = Proof::Have {
                name,
                fact,
                proof: Box::new(proof),
                body: Box::new(body),
            };
        }
    }

    /// The scopes directly inside `p` (not crossing a scope's top).
    fn inner(&mut self, p: &mut Proof, hyps: &Hyps) {
        match p {
            Proof::Have {
                name,
                fact,
                proof,
                body,
            } => {
                self.inner(proof, hyps);
                let mut more = hyps.clone();
                more.push((name.clone(), Eqn::new(fact.clone(), term::boolean(true))));
                self.scope(body, &more);
            }
            Proof::Cases {
                on,
                hyp,
                if_true,
                if_false,
            } => {
                for (v, q) in [(true, if_true), (false, if_false)] {
                    let mut more = hyps.clone();
                    more.push((hyp.clone(), Eqn::new(on.clone(), term::boolean(v))));
                    self.scope(q, &more);
                }
            }
            // A case of a split or an induction states the hypotheses
            // above it otherwise: only what states itself is cut there.
            Proof::Split { cases, .. } => cases
                .iter_mut()
                .for_each(|c| self.scope(&mut c.proof, &Vec::new())),
            Proof::Enum { cases, .. } => cases.iter_mut().for_each(|c| self.scope(c, &Vec::new())),
            Proof::Induct { cases, .. } => cases.iter_mut().for_each(|c| {
                self.scope(&mut c.proof, &Vec::new());
            }),
            Proof::InductList { nil, cons, .. } | Proof::ListCases { nil, cons, .. } => {
                self.scope(nil, &Vec::new());
                self.scope(cons, &Vec::new());
            }
            _ => each_mut(p, &mut |q| self.inner(q, hyps)),
        }
    }

    /// The cut named `name` for part `q` in a scope with hypotheses
    /// `hyps`: its fact, the proof of `fact = true`, and the step that gives
    /// back what `q` proves from the cut's hypothesis.
    fn cut_of(&self, q: &Proof, hyps: &Hyps, name: &str) -> Option<(Term, Proof, Proof)> {
        let eq = claim(q, self.script, hyps).ok()?;
        let (lhs, rhs) = (canon(&eq.lhs), canon(&eq.rhs));
        if has_match(&lhs) || has_match(&rhs) {
            return None;
        }
        let named = Proof::Hyp(name.to_string());
        if term::bool_value(&rhs) == Some(true) {
            return Some((lhs, q.clone(), named));
        }
        if term::bool_value(&lhs) == Some(true) {
            return Some((
                rhs,
                Proof::Symm(Box::new(q.clone())),
                Proof::Symm(Box::new(named)),
            ));
        }
        let ty = lhs.ty().or(rhs.ty()).cloned()?;
        if !(self.beq)(&ty) {
            return None;
        }
        let fact = canon(&term::binop(BinOp::Eq, lhs.clone(), rhs.clone()));
        let at_rhs = canon(&term::binop(BinOp::Eq, rhs.clone(), rhs.clone()));
        let proof = Proof::Trans {
            terms: vec![fact.clone(), at_rhs, term::boolean(true)],
            steps: vec![
                Proof::Congr {
                    ctx: term::binop(BinOp::Eq, term::hole(), rhs.clone()),
                    inner: Box::new(q.clone()),
                },
                Proof::Rule {
                    rule: WallRule::BeqRefl,
                    subst: vec![("a".into(), rhs.clone())],
                    premises: Vec::new(),
                },
            ],
        };
        let used = Proof::Rule {
            rule: WallRule::EqOfBeq,
            subst: vec![("a".into(), lhs), ("b".into(), rhs)],
            premises: vec![named],
        };
        Some((fact, proof, used))
    }
}

/// Walk the scope `p` is in: each part's fingerprint, counted, with whether
/// it may move to the top of the scope from here. Returns the
/// fingerprint, weight and reads of `p`.
fn walk(p: &Proof, path: &Path, seen: &mut HashMap<u64, Seen>) -> (u64, usize, Reads) {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    std::mem::discriminant(p).hash(&mut h);
    let mut w = 1usize;
    let mut reads = Reads::default();
    let terms = |ts: &[&Term], h: &mut std::collections::hash_map::DefaultHasher, w: &mut usize| {
        for t in ts {
            *w += term_hash(t, h);
        }
    };
    let mut child = |q: &Proof,
                     path: &Path,
                     h: &mut std::collections::hash_map::DefaultHasher,
                     w: &mut usize,
                     reads: &mut Reads| {
        let (f, cw, r) = walk(q, path, seen);
        f.hash(h);
        *w += cw;
        reads.add(r);
    };
    match p {
        Proof::Refl(t) | Proof::Proj { term: t } | Proof::Cell { list: t } => {
            terms(&[t], &mut h, &mut w)
        }
        Proof::Hyp(n) => {
            n.hash(&mut h);
            reads.hyps.push(n.clone());
        }
        Proof::UnfoldConst { name } => name.hash(&mut h),
        Proof::Symm(q) => child(q, path, &mut h, &mut w, &mut reads),
        Proof::Trans { terms: ts, steps } => {
            terms(&ts.iter().collect::<Vec<_>>(), &mut h, &mut w);
            for q in steps {
                child(q, path, &mut h, &mut w, &mut reads);
            }
        }
        Proof::Congr { ctx, inner } => {
            terms(&[ctx], &mut h, &mut w);
            child(inner, path, &mut h, &mut w, &mut reads);
        }
        Proof::Unfold {
            fn_id,
            arm,
            args,
            binders,
            premise,
        } => {
            format!("{fn_id:?}").hash(&mut h);
            arm.hash(&mut h);
            reads.arms |= *arm > 0;
            terms(
                &args.iter().chain(binders).collect::<Vec<_>>(),
                &mut h,
                &mut w,
            );
            if let Some(q) = premise {
                child(q, path, &mut h, &mut w, &mut reads);
            }
        }
        Proof::Arm {
            term: t,
            arm,
            binders,
            premise,
        } => {
            arm.hash(&mut h);
            reads.arms = true;
            terms(
                &std::iter::once(t).chain(binders).collect::<Vec<_>>(),
                &mut h,
                &mut w,
            );
            child(premise, path, &mut h, &mut w, &mut reads);
        }
        Proof::Rule {
            rule,
            subst,
            premises,
        } => {
            format!("{rule:?}").hash(&mut h);
            for (n, t) in subst {
                n.hash(&mut h);
                terms(&[t], &mut h, &mut w);
            }
            for q in premises {
                child(q, path, &mut h, &mut w, &mut reads);
            }
        }
        Proof::Law {
            law,
            subst,
            premise,
        } => {
            law.hash(&mut h);
            for (n, t) in subst {
                n.hash(&mut h);
                terms(&[t], &mut h, &mut w);
            }
            if let Some(q) = premise {
                child(q, path, &mut h, &mut w, &mut reads);
            }
        }
        Proof::Compute { lhs, rhs } | Proof::Ring { lhs, rhs } => {
            terms(&[lhs, rhs], &mut h, &mut w)
        }
        Proof::Linear {
            goal,
            value,
            hyps,
            weights,
            cuts,
        } => {
            terms(&[goal], &mut h, &mut w);
            value.hash(&mut h);
            hyps.hash(&mut h);
            format!("{weights:?}{cuts:?}").hash(&mut h);
            reads.hyps.extend(hyps.iter().cloned());
        }
        Proof::Absurd {
            contradiction,
            lhs,
            rhs,
        } => {
            terms(&[lhs, rhs], &mut h, &mut w);
            child(contradiction, path, &mut h, &mut w, &mut reads);
        }
        Proof::Have {
            name,
            fact,
            proof,
            body,
        } => {
            name.hash(&mut h);
            terms(&[fact], &mut h, &mut w);
            child(proof, path, &mut h, &mut w, &mut reads);
            let mut inside = path.clone();
            inside.bound.push(name.clone());
            let mut r = Reads::default();
            child(body, &inside, &mut h, &mut w, &mut r);
            r.unbind(name);
            reads.add(r);
        }
        Proof::Cases {
            on,
            hyp,
            if_true,
            if_false,
        } => {
            hyp.hash(&mut h);
            terms(&[on], &mut h, &mut w);
            let mut inside = path.clone();
            inside.bound.push(hyp.clone());
            inside.eq_case |= matches!(on.node, ResolvedExpr::BinOp(BinOp::Eq, ..));
            let mut r = Reads::default();
            child(if_true, &inside, &mut h, &mut w, &mut r);
            child(if_false, &inside, &mut h, &mut w, &mut r);
            r.unbind(hyp);
            reads.add(r);
        }
        // A split or an induction is a scope of its own: written out as
        // a whole, never walked into.
        Proof::Split { .. }
        | Proof::Enum { .. }
        | Proof::Induct { .. }
        | Proof::InductList { .. }
        | Proof::ListCases { .. } => {
            // Never moved, nor is what holds it: one of its own.
            (p as *const Proof as usize).hash(&mut h);
            reads.pinned = true;
        }
    }
    let fp = h.finish();
    // A part that reads a hypothesis named on the way here, or that opens
    // an arm below a case on an equality, stays where it is.
    let movable = !reads.pinned
        && !reads.hyps.iter().any(|n| path.bound.contains(n))
        && !(reads.arms && path.eq_case);
    let e = seen.entry(fp).or_insert_with(|| Seen {
        count: 0,
        movable: true,
        weight: w,
        first: None,
    });
    e.count += 1;
    e.movable &= movable;
    if e.count == 2 && e.movable {
        e.first = Some(p.clone());
    }
    (fp, w, reads)
}

/// `p` with every part equal to `part` (fingerprint `fp`) replaced by
/// `with`, in the scope `p` is in; how many it replaced.
fn replace(p: &mut Proof, fp: u64, part: &Proof, with: &Proof) -> usize {
    if p == part && walk(p, &Path::default(), &mut HashMap::new()).0 == fp {
        *p = with.clone();
        return 1;
    }
    let mut n = 0;
    match p {
        Proof::Split { .. }
        | Proof::Enum { .. }
        | Proof::Induct { .. }
        | Proof::InductList { .. }
        | Proof::ListCases { .. } => {}
        _ => each_mut(p, &mut |q| n += replace(q, fp, part, with)),
    }
    n
}

/// `p` with each `used` that [`replace`] wrote put back to `part`.
fn put_back(p: &mut Proof, used: &Proof, part: &Proof) {
    if p == used {
        *p = part.clone();
        return;
    }
    each_mut(p, &mut |q| put_back(q, used, part));
}

/// Each part directly inside `p`.
fn each_mut(p: &mut Proof, f: &mut dyn FnMut(&mut Proof)) {
    match p {
        Proof::Symm(q) | Proof::Congr { inner: q, .. } => f(q),
        Proof::Absurd { contradiction, .. } => f(contradiction),
        Proof::Arm { premise, .. } => f(premise),
        Proof::Unfold { premise, .. } | Proof::Law { premise, .. } => {
            if let Some(q) = premise {
                f(q);
            }
        }
        Proof::Trans { steps: all, .. }
        | Proof::Rule { premises: all, .. }
        | Proof::Enum { cases: all, .. } => all.iter_mut().for_each(f),
        Proof::Cases {
            if_true, if_false, ..
        } => {
            f(if_true);
            f(if_false);
        }
        Proof::Split { cases, .. } => cases.iter_mut().for_each(|c| f(&mut c.proof)),
        Proof::Have { proof, body, .. } => {
            f(proof);
            f(body);
        }
        Proof::Induct { cases, .. } => cases.iter_mut().for_each(|c| {
            f(&mut c.proof);
            c.carry.iter_mut().flatten().for_each(&mut *f);
        }),
        Proof::InductList { nil, cons, .. } | Proof::ListCases { nil, cons, .. } => {
            f(nil);
            f(cons);
        }
        Proof::Refl(_)
        | Proof::Hyp(_)
        | Proof::Linear { .. }
        | Proof::UnfoldConst { .. }
        | Proof::Proj { .. }
        | Proof::Cell { .. }
        | Proof::Compute { .. }
        | Proof::Ring { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::proof_steps::Obligation;
    use crate::ir::proof_steps::sexpr::{BuiltinsOnly, script as serialise};

    fn int_var(n: &str) -> Term {
        let v = term::var(n);
        v.set_ty(crate::ast::Type::Int);
        v
    }

    fn add(a: Term, b: Term) -> Term {
        term::binop(BinOp::Add, a, b)
    }

    fn zero() -> Term {
        term::int(&0.into())
    }

    fn script(lhs: Term, rhs: Term, proof: Proof) -> Script {
        let givens: Vec<String> = vec!["a".into(), "b".into()];
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
                premise: None,
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

    /// `a + 0 = a` the long way round, through `(x + b) - b` three times.
    fn long_way(a: &Term, b: &Term) -> Proof {
        let mut cur = canon(&add(a.clone(), zero()));
        let mut terms = vec![cur.clone()];
        let mut steps = Vec::new();
        for _ in 0..3 {
            let next = canon(&term::binop(
                BinOp::Sub,
                add(cur.clone(), b.clone()),
                b.clone(),
            ));
            steps.push(Proof::Ring {
                lhs: cur.clone(),
                rhs: next.clone(),
            });
            terms.push(next.clone());
            cur = next;
        }
        steps.push(Proof::Ring {
            lhs: cur,
            rhs: canon(a),
        });
        terms.push(canon(a));
        Proof::Trans { terms, steps }
    }

    #[test]
    fn a_part_proved_twice_is_cut_once_and_the_kernel_accepts_both() {
        let (a, b) = (int_var("a"), int_var("b"));
        let a0 = canon(&add(a.clone(), zero()));
        let p = long_way(&a, &b);
        let lhs = canon(&add(a0.clone(), a0.clone()));
        let mid = canon(&add(a.clone(), a0.clone()));
        let rhs = canon(&add(a.clone(), a.clone()));
        let proof = Proof::Trans {
            terms: vec![lhs.clone(), mid, rhs.clone()],
            steps: vec![
                Proof::Congr {
                    ctx: add(term::hole(), a0.clone()),
                    inner: Box::new(p.clone()),
                },
                Proof::Congr {
                    ctx: add(a.clone(), term::hole()),
                    inner: Box::new(p),
                },
            ],
        };
        let s = script(lhs.clone(), rhs.clone(), proof);
        kernel(&s).expect("the kernel accepts the proof as written");
        let shared = share(&s, &|_| true);
        assert!(
            matches!(&shared, Proof::Have { name, .. } if name == "h_share1"),
            "the long way is cut out once: {shared:?}"
        );
        assert!(super::super::weight(&shared) < super::super::weight(&s.proof));
        kernel(&script(lhs, rhs, shared)).expect("the kernel accepts the shared proof");
    }

    #[test]
    fn a_part_that_reads_a_case_hypothesis_stays_in_the_case() {
        let (a, b) = (int_var("a"), int_var("b"));
        let reads = Proof::Trans {
            terms: Vec::new(),
            steps: vec![long_way(&a, &b), Proof::Hyp("h".into())],
        };
        let cases = Proof::Cases {
            on: term::binop(BinOp::Lt, a.clone(), b.clone()),
            hyp: "h".into(),
            if_true: Box::new(reads.clone()),
            if_false: Box::new(reads.clone()),
        };
        let mut seen = HashMap::new();
        walk(&cases, &Path::default(), &mut seen);
        let fp = walk(&reads, &Path::default(), &mut HashMap::new()).0;
        assert_eq!(seen[&fp].count, 2);
        assert!(!seen[&fp].movable, "it reads `h`, which the case names");
        // The part inside it that reads nothing may move.
        let inner = walk(&long_way(&a, &b), &Path::default(), &mut HashMap::new()).0;
        assert!(seen[&inner].movable);
    }
}
