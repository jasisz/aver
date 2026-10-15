//! Evaluation with a proof: reduce a term towards a value, one rule at a
//! time, recording each rule. Where evaluation stops at a Bool it cannot
//! decide, the prover splits on that Bool and evaluates both sides again
//! under the new hypothesis.

use crate::ast::BinOp;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedPattern};
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Eqn, LawRef, Proof, WallRule};

use super::chain::{Chain, meet};
use super::env::Env;
use super::rewrite::pick_rewrite;

#[derive(Clone, Debug)]
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

/// Rounds of [`Env::meet_by_rewriting`], one rewrite of each side per
/// round.
const MAX_REWRITE_ROUNDS: usize = 64;

/// Most earlier laws of the automatic pass that one stopped term is
/// rewritten with candidates of, in file order.
pub(crate) const MAX_AUTO_CANDIDATES: usize = 4;

/// Whether `part` occurs in `t`.
fn holds(t: &Term, part: &Term) -> bool {
    *term::canon_ref(t) == *part || term::children(t).into_iter().any(|c| holds(c, part))
}

/// The left argument `a` where `l` holds `Bool.or(a, b)` (or `Bool.and`)
/// at a position where `r` holds it with the arguments the other way
/// round, the two terms alike everywhere else on the way there.
fn swapped_connective(l: &Term, r: &Term) -> Option<Term> {
    fn walk(l: &Term, r: &Term) -> Option<Term> {
        if l == r {
            return None;
        }
        if let (
            ResolvedExpr::Call(ResolvedCallee::Builtin(f), xs),
            ResolvedExpr::Call(ResolvedCallee::Builtin(g), ys),
        ) = (&l.node, &r.node)
            && f == g
            && matches!(f.as_str(), "Bool.or" | "Bool.and")
            && xs.len() == 2
            && ys.len() == 2
            && xs[0] == ys[1]
            && xs[1] == ys[0]
        {
            return Some(xs[0].clone());
        }
        let (cl, cr) = (term::children(l), term::children(r));
        if cl.is_empty() || cl.len() != cr.len() {
            return None;
        }
        // The same head: `l` with the children of `r` is `r`.
        let mut same = l.clone();
        for (i, c) in cr.iter().enumerate() {
            same = term::with_child(&same, i, (*c).clone());
        }
        if same != *r {
            return None;
        }
        cl.into_iter().zip(cr).find_map(|(a, b)| walk(a, b))
    }
    walk(&canon(l), &canon(r))
}

/// Complements: `(a P b) = v` decides `(a Q b) = w`.
const COMPLEMENTS: [(BinOp, bool, BinOp, bool); 12] = [
    (BinOp::Gt, false, BinOp::Lte, true),
    (BinOp::Gt, true, BinOp::Lte, false),
    (BinOp::Lt, false, BinOp::Gte, true),
    (BinOp::Lt, true, BinOp::Gte, false),
    (BinOp::Gte, false, BinOp::Lt, true),
    (BinOp::Gte, true, BinOp::Lt, false),
    (BinOp::Lte, false, BinOp::Gt, true),
    (BinOp::Lte, true, BinOp::Gt, false),
    (BinOp::Neq, false, BinOp::Eq, true),
    (BinOp::Neq, true, BinOp::Eq, false),
    (BinOp::Eq, false, BinOp::Neq, true),
    (BinOp::Eq, true, BinOp::Neq, false),
];

/// `(a != b) = !v` from `eq : (a == b) = v`: `a != b` is
/// `Bool.not(a == b)` (`int.ne.def`), which is `Bool.not(v)`, computed.
pub(crate) fn ne_of_eq(a: &Term, b: &Term, eq: Proof, v: bool) -> Proof {
    let beq = canon(&term::binop(BinOp::Eq, a.clone(), b.clone()));
    let not_v = canon(&term::bool_not(term::boolean(v)));
    Proof::Trans {
        terms: vec![
            canon(&term::binop(BinOp::Neq, a.clone(), b.clone())),
            canon(&term::bool_not(beq)),
            not_v.clone(),
            term::boolean(!v),
        ],
        steps: vec![
            Proof::Rule {
                rule: WallRule::NeDef,
                subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
                premises: Vec::new(),
            },
            Proof::Congr {
                ctx: term::bool_not(term::hole()),
                inner: Box::new(eq),
            },
            Proof::Compute {
                lhs: not_v,
                rhs: term::boolean(!v),
            },
        ],
    }
}

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

/// Whether a hypothesis in `hyps` states that `v` is not the literal of
/// pattern `p`: `(v == k) = false`, which the kernel reads as excluding
/// the arm (an Int, text or Bool literal only).
pub(super) fn ruled_out(p: &ResolvedPattern, v: &Term, hyps: &[(String, Eqn)]) -> bool {
    use crate::ast::Literal;
    let ResolvedPattern::Literal(l) = p else {
        return false;
    };
    if !matches!(
        l,
        Literal::Int(_) | Literal::BigInt(_) | Literal::Str(_) | Literal::Bool(_)
    ) {
        return false;
    }
    let k = crate::ast::Spanned::bare(ResolvedExpr::Literal(l.clone()));
    let want = crate::ir::proof_steps::unequal(&canon(v), &k);
    let lhs = canon(&want.lhs);
    hyps.iter()
        .any(|(_, e)| *term::canon_ref(&e.lhs) == lhs && term::bool_value(&e.rhs) == Some(false))
}

/// The arm of a match whose pattern head the value `v` has, with the
/// pattern's bindings, and whether `v` is a list literal the arm reads as a
/// cell (so the premise ends with a [`Proof::Cell`] step). A literal arm
/// that a hypothesis in `hyps` rules out (see [`ruled_out`]) is passed
/// over. `None` when no arm can be selected syntactically.
pub(super) fn select_arm(
    arms: &[crate::ir::hir::ResolvedMatchArm],
    v: &Term,
    hyps: &[(String, Eqn)],
) -> Option<(u32, Vec<Term>, bool)> {
    use crate::ir::proof_steps::claim::{excludes, is_catch_all};
    for (i, arm) in arms.iter().enumerate() {
        let k = (i + 1) as u32;
        if is_catch_all(&arm.pattern) {
            // Chosen for this value once every earlier arm excludes it.
            return arms[..i]
                .iter()
                .all(|earlier| {
                    excludes(&earlier.pattern, v) || ruled_out(&earlier.pattern, v, hyps)
                })
                .then(|| (k, vec![canon(v)], false));
        }
        if ruled_out(&arm.pattern, v, hyps) {
            continue;
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

/// `k > 0` for a literal `k`, by computation.
pub(super) fn literal_positive(k: &Term) -> Option<Proof> {
    term::int_value(k).filter(|v| *v > 0.into())?;
    Some(Proof::Compute {
        lhs: canon(&term::binop(BinOp::Gt, k.clone(), term::int(&0.into()))),
        rhs: term::boolean(true),
    })
}

/// What Euclidean division by a divisor `k > 0` in `ts` gives linear
/// arithmetic, each fact with its proof: for each `a / k` or `a % k`, the
/// two orders of `a / k * k + a % k = a` (`int.div_mod_recompose`), and
/// `0 <= a % k` and `a % k < k` (`int.mod_range`). `positive` proves
/// `(k > 0) = true`; a divisor it does not is left out. Also the
/// quotients, each inside the ones that divide it again.
/// Whether `t` divides by a divisor that is not a literal, outside any
/// `match`: where [`division_facts`] asks whether a divisor is positive
/// beyond computing it.
fn open_divisor(t: &Term) -> bool {
    use crate::ir::hir::BuiltinIntrinsic;
    if let ResolvedExpr::Call(ResolvedCallee::Intrinsic(op), args) = &t.node
        && args.len() == 2
        && matches!(
            op,
            BuiltinIntrinsic::IntModEuclid | BuiltinIntrinsic::IntDivEuclid
        )
        && term::int_value(&args[1]).is_none()
    {
        return true;
    }
    !matches!(t.node, ResolvedExpr::Match { .. }) && term::children(t).into_iter().any(open_divisor)
}

fn division_facts(
    ts: &[&Term],
    positive: &mut dyn FnMut(&Term) -> Option<Proof>,
) -> (Vec<(Term, Proof)>, Vec<Term>) {
    use crate::ir::hir::BuiltinIntrinsic;
    fn walk(t: &Term, mods: &mut Vec<Term>, divs: &mut Vec<Term>) {
        if let ResolvedExpr::Call(ResolvedCallee::Intrinsic(op), args) = &t.node
            && args.len() == 2
            && term::int_value(&args[1]).is_none_or(|k| k > 0.into())
        {
            let found = match op {
                BuiltinIntrinsic::IntModEuclid => Some(&mut *mods),
                BuiltinIntrinsic::IntDivEuclid => Some(&mut *divs),
                _ => None,
            };
            if let Some(found) = found
                && !found.contains(&*term::canon_ref(t))
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
    let mut out = Vec::new();
    let mut shown: Vec<(Term, Option<Proof>)> = Vec::new();
    let mut positive = |k: &Term| {
        if let Some((_, p)) = shown.iter().find(|(t, _)| t == k) {
            return p.clone();
        }
        let p = positive(k);
        shown.push((k.clone(), p.clone()));
        p
    };
    for d in &divs {
        let ResolvedExpr::Call(_, args) = &d.node else {
            continue;
        };
        let (a, k) = (canon(&args[0]), canon(&args[1]));
        let Some(k_positive) = positive(&k) else {
            continue;
        };
        let subst = vec![("a".to_string(), a.clone()), ("k".to_string(), k.clone())];
        let Some((_, concl)) = WallRule::DivModRecompose.instantiate(&subst) else {
            continue;
        };
        let recompose = Proof::Rule {
            rule: WallRule::DivModRecompose,
            subst,
            premises: vec![k_positive],
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
                        cuts: Vec::new(),
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
        let Some(k_positive) = positive(&k) else {
            continue;
        };
        let range = Proof::Rule {
            rule: WallRule::ModRange,
            subst: vec![("a".into(), a), ("k".into(), k.clone())],
            premises: vec![k_positive],
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

/// The Int term a polynomial over `atoms` reads as: its monomials in
/// order, each an atom product times its coefficient's magnitude, added
/// or taken away by its sign, then the constant; `0` for none.
fn poly_term(p: &crate::ir::proof_steps::ring::Poly, atoms: &[Term]) -> Term {
    use num_traits::{One, Signed};
    let monomials = p.iter().filter(|(m, _)| !m.is_empty());
    let constant = p.iter().filter(|(m, _)| m.is_empty());
    let mut out: Option<Term> = None;
    for (m, c) in monomials.chain(constant) {
        let magnitude = c.abs();
        let part = match m
            .iter()
            .map(|&i| atoms[i].clone())
            .reduce(|a, b| term::binop(BinOp::Mul, a, b))
        {
            None => term::int(&magnitude),
            Some(product) if magnitude.is_one() => product,
            Some(product) => term::binop(BinOp::Mul, product, term::int(&magnitude)),
        };
        let op = if c.is_negative() {
            BinOp::Sub
        } else {
            BinOp::Add
        };
        out = Some(match out {
            None if c.is_negative() => term::binop(BinOp::Sub, term::int(&0.into()), part),
            None => part,
            Some(acc) => term::binop(op, acc, part),
        });
    }
    out.unwrap_or_else(|| term::int(&0.into()))
}

/// The facts (the opposite of the goal first) a certificate over the
/// rationals uses, by index from 1, with its weights: the opposite's,
/// then theirs.
type Settled = (
    Vec<usize>,
    Vec<num_bigint::BigInt>,
    Vec<crate::ir::proof_steps::linear::Cut>,
);

fn settle_rationally(facts: &[crate::ir::proof_steps::ring::Poly]) -> Option<Settled> {
    let weights = crate::ir::proof_steps::linear::certificate(facts)?;
    let used: Vec<usize> = (1..facts.len())
        .filter(|&i| weights[i] != 0.into())
        .collect();
    let mut kept = vec![weights[0].clone()];
    kept.extend(used.iter().map(|&i| weights[i].clone()));
    Some((used, kept, Vec::new()))
}

/// The same over ℤ: a certificate whose rows may be rounded (see
/// [`crate::ir::proof_steps::linear::Cut`]), as `omega` reads a remainder
/// by a literal.
fn settle_over_integers(facts: &[crate::ir::proof_steps::ring::Poly]) -> Option<Settled> {
    use crate::ir::proof_steps::linear;
    let cert = linear::integer_certificate(facts)?;
    let used: Vec<usize> = (1..facts.len())
        .filter(|&i| linear::uses(&cert, i))
        .collect();
    let mut keep = vec![0];
    keep.extend(used.iter().copied());
    let cert = linear::restricted(&cert, facts.len(), &keep)?;
    Some((used, cert.weights, cert.cuts))
}

/// A `Bits` builtin read as Int arithmetic by its rule: `Bits.not(a)` as
/// `0 - a - 1`, a shift left by a literal count `n` as `a * 2^n`, and a
/// mask by a literal `2^n - 1` as the remainder by `2^n`, by a literal
/// `2^n` as that bit of the quotient. The premise, that the literals are
/// such, computes.
fn bits_step(cur: &Term) -> Option<Step> {
    use crate::ir::hir::BuiltinIntrinsic;
    use num_bigint::BigInt;
    let int = |n: &BigInt| term::int(n);
    // `n` with `2^n = k`, for `k > 0` a power of two.
    let log2 = |k: &BigInt| -> Option<BigInt> {
        if *k <= BigInt::from(0) || (k & (k - BigInt::from(1))) != BigInt::from(0) {
            return None;
        }
        Some(BigInt::from(k.bits() - 1))
    };
    let shl_one = |n: &BigInt| {
        term::intrinsic(
            BuiltinIntrinsic::BitsShiftLeft,
            vec![int(&1.into()), int(n)],
        )
    };
    let power = |k: &BigInt, n: &BigInt| {
        term::bool_and(
            term::binop(BinOp::Lte, int(&0.into()), int(n)),
            term::binop(BinOp::Eq, int(k), shl_one(n)),
        )
    };
    let (rule, subst, premise): (WallRule, Vec<(String, Term)>, Option<Term>) = match &cur.node {
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), args)
            if b == "Bits.not" && args.len() == 1 =>
        {
            (WallRule::BitsNot, vec![("a".into(), canon(&args[0]))], None)
        }
        ResolvedExpr::Call(ResolvedCallee::Intrinsic(BuiltinIntrinsic::BitsShiftLeft), args)
            if args.len() == 2 =>
        {
            let n = term::int_value(&args[1])?;
            if n < BigInt::from(0) || n > BigInt::from(4096) {
                return None;
            }
            let k = BigInt::from(1) << num_traits::ToPrimitive::to_u32(&n)?;
            (
                WallRule::BitsShl,
                vec![
                    ("a".into(), canon(&args[0])),
                    ("n".into(), int(&n)),
                    ("k".into(), int(&k)),
                ],
                Some(power(&k, &n)),
            )
        }
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), args)
            if b == "Bits.and" && args.len() == 2 =>
        {
            let m = term::int_value(&args[1])?;
            let a = canon(&args[0]);
            let k = &m + 1;
            if let Some(n) = log2(&k).filter(|n| *n <= BigInt::from(4096)) {
                let premise = term::bool_and(
                    term::bool_and(
                        term::binop(BinOp::Lte, int(&0.into()), int(&n)),
                        term::binop(
                            BinOp::Eq,
                            term::binop(BinOp::Add, int(&m), int(&1.into())),
                            int(&k),
                        ),
                    ),
                    term::binop(BinOp::Eq, int(&k), shl_one(&n)),
                );
                (
                    WallRule::BitsAndLow,
                    vec![
                        ("a".into(), a),
                        ("m".into(), int(&m)),
                        ("k".into(), int(&k)),
                        ("n".into(), int(&n)),
                    ],
                    Some(premise),
                )
            } else {
                let n = log2(&m).filter(|n| *n <= BigInt::from(4096))?;
                (
                    WallRule::BitsAndBit,
                    vec![
                        ("a".into(), a),
                        ("k".into(), int(&m)),
                        ("n".into(), int(&n)),
                    ],
                    Some(power(&m, &n)),
                )
            }
        }
        _ => return None,
    };
    let (premises, concl) = rule.instantiate(&subst)?;
    let proofs = match premise {
        Some(p) => {
            let p = canon(&p);
            if premises.first().map(|e| canon(&e.lhs)) != Some(p.clone()) {
                return None;
            }
            vec![Proof::Compute {
                lhs: p,
                rhs: term::boolean(true),
            }]
        }
        None => Vec::new(),
    };
    Some(Step::Progress(Box::new((
        Proof::Rule {
            rule,
            subst,
            premises: proofs,
        },
        canon(&concl.rhs),
    ))))
}

/// `a < c` as the least `c` with its comparison `t` against a literal, when
/// `t = value` says so: `a < c`, `a <= c - 1`, `c > a`, `c - 1 >= a`, or
/// the complement of `a >= c`.
fn upper_bound(t: &Term, value: bool, a: &Term) -> Option<num_bigint::BigInt> {
    let ResolvedExpr::BinOp(op, l, r) = &t.node else {
        return None;
    };
    let (op, c) = match (*term::canon_ref(l) == *a, *term::canon_ref(r) == *a) {
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

/// Whether `t` builds or updates, anywhere, a record of a type that Lean
/// states as a value with the proof of its invariant (a refined type),
/// where a step inside the record would have to carry that proof along.
/// A type whose bare name any module refines counts.
fn has_refined_record(t: &Term, inputs: &crate::codegen::proof_lower::ProofLowerInputs) -> bool {
    let refined = match &t.node {
        ResolvedExpr::RecordCreate { type_name, .. }
        | ResolvedExpr::RecordUpdate { type_name, .. } => {
            let bare = type_name.rsplit('.').next().unwrap_or(type_name);
            crate::codegen::common::refinement_info_for(bare, inputs).is_some()
        }
        _ => false,
    };
    refined
        || term::children(t)
            .into_iter()
            .any(|c| has_refined_record(c, inputs))
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

/// The position of the first list literal in `t` with an element, closed
/// or not, outside any `match`.
fn any_literal_path(t: &Term) -> Option<Vec<usize>> {
    if matches!(t.node, ResolvedExpr::Match { .. }) {
        return None;
    }
    if let ResolvedExpr::List(xs) = &t.node
        && !xs.is_empty()
    {
        return Some(Vec::new());
    }
    term::children(t)
        .into_iter()
        .enumerate()
        .find_map(|(i, c)| {
            let mut path = any_literal_path(c)?;
            path.insert(0, i);
            Some(path)
        })
}

/// `t = t'`, where `t'` writes every list literal of `t` with an element,
/// a closed one too, as cells all the way down (`[1, 2]` as
/// `List.prepend(1, List.prepend(2, []))`), one [`Proof::Cell`] step per
/// element; `None` when there is none, or more than 64 elements.
fn every_literal_as_cells(t: &Term) -> Option<Chain> {
    let mut chain = Chain::new(&canon(t));
    while let Some(path) = any_literal_path(chain.cur()) {
        if chain.len() >= 64 {
            return None;
        }
        let list = term::at(chain.cur(), &path).clone();
        let cell = term::cell_of(&list)?;
        chain.push_at(&path, Proof::Cell { list }, &cell);
    }
    (!chain.is_empty()).then_some(chain)
}

/// Whether `t` has, outside any `match`, a list literal with an element or
/// a call without arguments: what [`as_cells`] and
/// [`Env::fold_constants`] look for before they change anything.
fn may_cell_or_fold(t: &Term) -> bool {
    match &t.node {
        ResolvedExpr::Match { .. } => false,
        ResolvedExpr::List(xs) if !xs.is_empty() => true,
        ResolvedExpr::Call(ResolvedCallee::Fn(_), args) if args.is_empty() => true,
        ResolvedExpr::TailCall { args, .. } if args.is_empty() => true,
        _ => term::children(t).into_iter().any(may_cell_or_fold),
    }
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

/// Steps writing each call in `a` as a call in `b` that it equals by ring
/// steps at its Int arguments, under congruence; each call once, outermost
/// first.
fn align_calls(a: &Term, b: &Term) -> Vec<(Proof, Term)> {
    fn calls(t: &Term, out: &mut Vec<Term>) {
        if matches!(t.node, ResolvedExpr::Call(..)) && !out.contains(&*term::canon_ref(t)) {
            out.push(canon(t));
        }
        for c in term::children(t) {
            calls(c, out);
        }
    }
    fn paths(t: &Term, part: &Term, at: &mut Vec<usize>, out: &mut Vec<Vec<usize>>) {
        if *term::canon_ref(t) == *part {
            out.push(at.clone());
            return;
        }
        for (i, c) in term::children(t).into_iter().enumerate() {
            at.push(i);
            paths(c, part, at, out);
            at.pop();
        }
    }
    let (mut ours, mut theirs) = (Vec::new(), Vec::new());
    calls(a, &mut ours);
    calls(b, &mut theirs);
    let mut steps = Vec::new();
    let mut cur = canon(a);
    for x in ours {
        if theirs.contains(&x) {
            continue;
        }
        let Some((y, bridge)) = theirs.iter().find_map(|y| {
            super::rewrite::ring_bridge(&x, y)
                .filter(|s| !s.is_empty())
                .map(|s| (y.clone(), s))
        }) else {
            continue;
        };
        let mut chain = Chain::new(&x);
        for (p, to) in bridge {
            chain.push(p, to);
        }
        let (_, proof) = chain.finish();
        let mut at = Vec::new();
        let mut found = Vec::new();
        paths(&cur, &x, &mut at, &mut found);
        for path in found {
            let ctx = term::context_at(&cur, &path);
            let next = canon(&term::plug(&ctx, &y));
            steps.push((
                Proof::Congr {
                    ctx,
                    inner: Box::new(proof.clone()),
                },
                next.clone(),
            ));
            cur = next;
        }
    }
    steps
}

fn is_bool_value(t: &Term) -> bool {
    term::bool_value(t).is_some()
}

/// The most head steps evaluating one hypothesis may take.
const HYPOTHESIS_FUEL: usize = 100;

/// The most steps evaluating one hypothesis to its normal form (its parts
/// too) may take, see [`Env::normal_fact`].
const NORMAL_FACT_FUEL: usize = 200;

/// The most calls [`Env::open_call_only`] opens, one inside the other.
const MAX_CALL_OPENINGS: usize = 4;

/// A hypothesis in its normal form ([`Env::normal_fact`]): the form, the
/// proof that the hypothesis as stated equals it, and what that proof
/// needs in the script.
#[derive(Clone)]
pub(crate) struct NormalFact {
    pub to: Term,
    pub proof: Proof,
    laws: Vec<LawRef>,
    defs: Vec<crate::ir::identity::FnId>,
    consts: Vec<String>,
}

impl Env<'_> {
    /// Whether a true `t == u` makes `t` and `u` the same value, by `t`'s
    /// type (see [`crate::ir::SymbolTable::beq_is_equality`]).
    fn beq_is_equality(&self, t: &Term) -> bool {
        t.ty()
            .is_some_and(|ty| self.inputs.symbol_table.beq_is_equality(ty))
    }

    pub(crate) fn whnf(&mut self, t: &Term) -> Result<Eval, String> {
        // Each nested evaluation is a frame on the compiler's own stack; a
        // recursive definition over long data would exhaust it.
        if self.nesting >= MAX_NESTING {
            return Err(format!(
                "evaluation nests more than {MAX_NESTING} deep at `{}`",
                crate::ir::proof_steps::show::term(t, self.inputs.symbol_table)
            ));
        }
        if self.nesting == 0
            && let Some(memo) = self.memo.clone()
        {
            return self.whnf_remembered(t, &memo);
        }
        self.nesting += 1;
        let out = self.whnf_nested(t);
        self.nesting -= 1;
        out
    }

    /// [`Self::whnf`] through the memo of [`super::memo`]: a replay where
    /// the same evaluation is kept and the fuel it took is left, else the
    /// evaluation, kept for next time.
    fn whnf_remembered(
        &mut self,
        t: &Term,
        memo: &std::rc::Rc<std::cell::RefCell<super::memo::Memo>>,
    ) -> Result<Eval, String> {
        use super::memo::{Effect, Entry, Mode, Trace, key};
        let key = key(
            t,
            &self.hyps,
            self.open_halving,
            self.proving_cited_when,
            self.discharging,
            self.wide_linear,
        );
        let kept = memo
            .borrow()
            .get(&key)
            .filter(|e| e.cost <= self.fuel)
            .cloned();
        if let Some(entry) = &kept
            && self.memo_mode == Mode::On
        {
            self.fuel -= entry.cost;
            for e in &entry.effects {
                match e {
                    Effect::Used(id) => self.mark_used(*id),
                    Effect::Const(name) => self.mark_const_used(name),
                    Effect::Law(law) => self.use_law((**law).clone()),
                    Effect::Note(note) => self.note(note.clone()),
                    Effect::Def(id) => {
                        self.def(*id);
                    }
                    Effect::OpenWhen(when) => self.note_open_when(when),
                }
            }
            self.proving_cited_when = entry.proving_cited_when;
            return Ok(entry.eval.clone());
        }
        let (fuel, named, dry) = (self.fuel, self.next_hyp_count(), self.ran_dry);
        self.ran_dry = false;
        self.trace = Some(Trace::default());
        self.nesting += 1;
        let out = self.whnf_nested(t);
        self.nesting -= 1;
        let trace = self.trace.take().unwrap_or_default();
        // Kept only when it named no hypothesis and never ran out of fuel
        // (an evaluation that gave up on a part for want of fuel may find
        // more with more).
        let starved = self.ran_dry;
        self.ran_dry = dry || starved;
        let entry = match &out {
            Ok(eval) if self.next_hyp_count() == named && !starved => Some(Entry {
                eval: eval.clone(),
                cost: fuel - self.fuel,
                effects: trace.effects,
                proving_cited_when: self.proving_cited_when,
            }),
            _ => None,
        };
        if let Some(old) = &kept
            && !entry.as_ref().is_some_and(|e| old.same(e))
        {
            panic!(
                "AVER_STEPS_MEMO=check: evaluating `{}` again differs from the kept evaluation:\nkept: {}\nnow:  {}",
                crate::ir::proof_steps::show::term(t, self.inputs.symbol_table),
                old.describe(),
                match (&entry, &out) {
                    (Some(e), _) => e.describe(),
                    (None, Err(why)) => format!("failed: {why}"),
                    (None, Ok(_)) => "not kept".to_string(),
                }
            );
        }
        if let Some(entry) = entry {
            memo.borrow_mut().insert(key, entry);
        }
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
        // A Bool literal is a value already: a hypothesis with it as a
        // conjunct (`Bool.and(head, true)`) would rewrite it to itself.
        if is_bool_value(cur) {
            return Ok(None);
        }
        if let Some((name, value)) = self.hyp_for(cur) {
            return Ok(Some(Step::Progress(Box::new((Proof::Hyp(name), value)))));
        }
        if let Some(step) = self.swapped_hyp(cur) {
            return Ok(Some(step));
        }
        if let Some(step) = self.split_bool(cur) {
            return Ok(Some(step));
        }
        if let Some(step) = self.hyp_as_cells(cur) {
            return Ok(Some(step));
        }
        if let Some(step) = self.hyp_up_to_ring(cur) {
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
            // Neither a list literal nor a call without arguments: writing
            // it as cells and folding its constants changes nothing and
            // looks up no definition.
            if !may_cell_or_fold(&e.lhs) {
                continue;
            }
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

    /// A hypothesis whose left side is a call that `cur` is once its Int
    /// arguments are read as polynomials (`f(m + n - 1)` for
    /// `f((m - 1) + n)`): an induction hypothesis states the claim at the
    /// call's arguments, while the case has evaluated them in another
    /// order. A ring step at each such part, then the hypothesis.
    fn hyp_up_to_ring(&mut self, cur: &Term) -> Option<Step> {
        if !matches!(cur.node, ResolvedExpr::Call(..)) {
            return None;
        }
        let t = canon(cur);
        for (name, e) in self.hyps.iter().rev() {
            let lhs = canon(&e.lhs);
            if !matches!(lhs.node, ResolvedExpr::Call(..)) || lhs == t {
                continue;
            }
            let Some(steps) = super::rewrite::ring_bridge(&t, &lhs) else {
                continue;
            };
            let mut chain = Chain::new(&t);
            for (p, to) in steps {
                chain.push(p, to);
            }
            let (_, bridge) = chain.finish();
            let proof = Proof::Trans {
                terms: vec![t.clone(), lhs, canon(&e.rhs)],
                steps: vec![bridge, Proof::Hyp(name.clone())],
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
            let Some((path, p, to)) = self.constant_call(chain.cur()) else {
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
    /// With `equations`, instead each Int equation `a = b` in scope (`x %
    /// 32 = 3` where a `match` chose the arm of literal 3), which is its
    /// own proof.
    fn equalities_as_orders(&self, equations: bool) -> Vec<(Term, Proof)> {
        let mut out = Vec::new();
        for (name, e) in &self.hyps {
            let (a, b, equal) = match &e.lhs.node {
                _ if term::bool_value(&e.rhs).is_none() => {
                    if !(equations && is_int(&e.lhs) && is_int(&e.rhs)) {
                        continue;
                    }
                    (canon(&e.lhs), canon(&e.rhs), Proof::Hyp(name.clone()))
                }
                ResolvedExpr::BinOp(BinOp::Eq, a, b)
                    if !equations
                        && term::bool_value(&e.rhs) == Some(true)
                        && (is_int(a) || is_int(b)) =>
                {
                    let (a, b) = (canon(a), canon(b));
                    let equal = Proof::Rule {
                        rule: WallRule::EqOfBeq,
                        subst: vec![("a".into(), a.clone()), ("b".into(), b.clone())],
                        premises: vec![Proof::Hyp(name.clone())],
                    };
                    (a, b, equal)
                }
                _ => continue,
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
                            cuts: Vec::new(),
                        },
                    ],
                };
                out.push((fact, proof));
            }
        }
        out
    }

    /// `(a == b) = v` where a hypothesis states `(b == a) = v`, by
    /// `bool.beq.symm` and that hypothesis, for a type whose `==` is
    /// equality in Lean too. One lookup, no search: only the sides of `cur`
    /// itself are swapped.
    fn swapped_hyp(&self, cur: &Term) -> Option<Step> {
        let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node else {
            return None;
        };
        if !(self.beq_is_equality(a) || self.beq_is_equality(b)) {
            return None;
        }
        let (a, b) = (canon(a), canon(b));
        if a == b {
            return None;
        }
        let swapped = canon(&term::binop(BinOp::Eq, b.clone(), a.clone()));
        let (name, value) = self.hyp_for(&swapped)?;
        let proof = Proof::Trans {
            terms: vec![canon(cur), swapped, value.clone()],
            steps: vec![
                Proof::Rule {
                    rule: WallRule::BeqSymm,
                    subst: vec![("a".into(), a), ("b".into(), b)],
                    premises: Vec::new(),
                },
                Proof::Hyp(name),
            ],
        };
        Some(Step::Progress(Box::new((proof, value))))
    }

    /// `(a == a) = true`, by `bool.beq.refl`, for a type whose values hold
    /// no Float (`0.0 / 0.0` is not equal to itself). Also `(a == b) = true`
    /// where `a` and `b` differ only in Int parts that linear arithmetic
    /// shows equal, outside a record of a refined type: `a` is rewritten to
    /// `b` first.
    fn same_sides(&mut self, cur: &Term) -> Option<Step> {
        let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node else {
            return None;
        };
        let ty = a.ty().or(b.ty())?;
        if self.inputs.symbol_table.may_hold_float(ty) {
            return None;
        }
        let (a, b) = (canon(a), canon(b));
        if a == b {
            return Some(self.rule_step(WallRule::BeqRefl, vec![("a".into(), a)]));
        }
        if is_int(&a) || has_refined_record(&a, self.inputs) || has_refined_record(&b, self.inputs)
        {
            return None;
        }
        let bridge = self.linear_bridge(&a, &b)?;
        let ctx = term::binop(BinOp::Eq, term::hole(), b.clone());
        let mut terms = vec![canon(cur)];
        let mut steps = Vec::new();
        for (proof, to) in bridge {
            steps.push(Proof::Congr {
                ctx: ctx.clone(),
                inner: Box::new(proof),
            });
            terms.push(canon(&term::binop(BinOp::Eq, to, b.clone())));
        }
        if steps.is_empty() {
            return None;
        }
        steps.push(Proof::Rule {
            rule: WallRule::BeqRefl,
            subst: vec![("a".into(), b)],
            premises: Vec::new(),
        });
        terms.push(term::boolean(true));
        Some(Step::Progress(Box::new((
            Proof::Trans { terms, steps },
            term::boolean(true),
        ))))
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
            if let Some((mut path, p, v)) = self.constant_call(c) {
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
        let mut stated_lhs: Option<Vec<Term>> = None;
        let mut candidates: Vec<(String, Term, Term)> = Vec::new();
        for (n, e) in self.hyps.iter().rev() {
            if term::bool_value(&e.rhs) != Some(true) {
                continue;
            }
            let ResolvedExpr::BinOp(BinOp::Eq, a, k) = &e.lhs.node else {
                continue;
            };
            let (a, k) = (canon(a), canon(k));
            let keep = term::int_value(&k).is_some()
                || ((is_int(&a) && is_int(&k) || self.beq_is_equality(&a))
                    && !stated_lhs
                        .get_or_insert_with(|| self.stated_equalities())
                        .iter()
                        .any(|l| occurs(l, &k)));
            if keep {
                candidates.push((n.clone(), a, k));
            }
        }
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
            let Some((unfold, body)) = self.wrapper_unfold_to(&a, Some(&t)) else {
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
        self.wrapper_unfold_to(a, None)
    }

    /// [`Self::wrapper_unfold`], or `None` early where the body cannot be
    /// `want`: substituting the arguments keeps the body's outermost part
    /// unless that part is a variable.
    fn wrapper_unfold_to(&mut self, a: &Term, want: Option<&Term>) -> Option<(Proof, Term)> {
        let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &a.node else {
            return None;
        };
        if self.inputs.recursive_fns.contains(id) {
            return None;
        }
        let def = self.def_ref(*id)?;
        if matches!(def.body.node, ResolvedExpr::Match { .. }) {
            return None;
        }
        if let Some(want) = want
            && !term::may_become(&def.body, want)
        {
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
                if *term::canon_ref(side) == *x
                    && term::int_value(other).is_some()
                    && !literals.contains(&*term::canon_ref(other))
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

    /// What to split on where a `match` whose arms read Bool literals
    /// stopped at `value`, given what evaluation found (`found`). A
    /// subject the kernel knows is a Bool by its shape (a comparison, a
    /// connective, a call of a definition that returns a Bool, a given of
    /// type Bool) is split on as it stands; any other one, such as the
    /// head of a `List<Bool>` an induction case named, is a Bool only
    /// because the arms read it as one, so the split is on
    /// `Bool.and(value, true)`, which the kernel knows is a Bool and which
    /// is `value` by the rule `and_true_r` (see [`Self::split_bool`]).
    fn bool_guard(
        &self,
        arms: &[crate::ir::hir::ResolvedMatchArm],
        value: &Term,
        found: Option<Term>,
    ) -> Option<Term> {
        use crate::ast::Literal;
        let reads_bool = arms
            .iter()
            .any(|a| matches!(a.pattern, ResolvedPattern::Literal(Literal::Bool(_))));
        if !reads_bool || term::bool_value(value).is_some() {
            return found;
        }
        let kernel_bool = match &value.node {
            ResolvedExpr::BinOp(..) => true,
            ResolvedExpr::Call(ResolvedCallee::Builtin(b), _) => {
                matches!(b.as_str(), "Bool.and" | "Bool.or" | "Bool.not")
            }
            ResolvedExpr::Call(ResolvedCallee::Fn(_), _) => {
                matches!(value.ty(), Some(crate::ast::Type::Bool))
            }
            ResolvedExpr::Ident(n) => self.finite.contains(n),
            _ => false,
        };
        match found {
            Some(g) if !term::canon_eq(&g, value) || kernel_bool => Some(g),
            _ if kernel_bool => Some(canon(value)),
            _ => Some(canon(&term::bool_and(value.clone(), term::boolean(true)))),
        }
    }

    /// `x` is `v` where a split stated `Bool.and(x, true) = v`: by
    /// `and_true_r` read backwards, then that hypothesis.
    fn split_bool(&self, cur: &Term) -> Option<Step> {
        let t = canon(cur);
        for (name, e) in self.hyps.iter().rev() {
            let Some(v) = term::bool_value(&e.rhs) else {
                continue;
            };
            if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &e.lhs.node
                && b == "Bool.and"
                && args.len() == 2
                && term::bool_value(&args[1]) == Some(true)
                && *term::canon_ref(&args[0]) == t
            {
                let wrapped = canon(&e.lhs);
                let value = term::boolean(v);
                let proof = Proof::Trans {
                    terms: vec![t.clone(), wrapped, value.clone()],
                    steps: vec![
                        Proof::Symm(Box::new(Proof::Rule {
                            rule: WallRule::AndTrueR,
                            subst: vec![("a".into(), t.clone())],
                            premises: Vec::new(),
                        })),
                        Proof::Hyp(name.clone()),
                    ],
                };
                return Some(Step::Progress(Box::new((proof, value))));
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

    /// `(a Q b) = w` from its complement `(a P b) = v`: a hypothesis
    /// states it, or, for `P` an Int equality, the two orders decide it
    /// (`b != b` is false by `b == b`), or a hypothesis about `b` and `a`
    /// does through `bool.beq.symm` (`b != a` is true where `a != b` is).
    /// Between two orders the step is linear; `!=` is read as the negation
    /// of `==` ([`ne_of_eq`], [`Env::eq_of_ne`]).
    fn complement(&mut self, cur: &Term) -> Option<Step> {
        self.complement_in(cur, true)
    }

    /// [`Env::complement`]; `swap` also reads an equality from what the
    /// hypotheses state about its sides the other way round, one level.
    fn complement_in(&mut self, cur: &Term, swap: bool) -> Option<Step> {
        let ResolvedExpr::BinOp(op, a, b) = &cur.node else {
            return None;
        };
        if !is_int(a) {
            return None;
        }
        for (p, v, q, w) in COMPLEMENTS {
            if q != *op {
                continue;
            }
            let premise = canon(&term::binop(p, (**a).clone(), (**b).clone()));
            let proof = match self.hyp_for(&premise) {
                Some((name, value)) if term::bool_value(&value) == Some(v) => Proof::Hyp(name),
                _ if p == BinOp::Eq => match self.decide_linearly(&premise) {
                    Some((proof, value)) if term::bool_value(&value) == Some(v) => proof,
                    _ => match self.eq_swapped(&premise, swap) {
                        Some((proof, value)) if value == v => proof,
                        _ => continue,
                    },
                },
                _ => continue,
            };
            let step = match (p, proof) {
                (BinOp::Eq, proof) => ne_of_eq(a, b, proof, v),
                (BinOp::Neq, proof) => self.eq_of_ne(a, b, proof, v),
                (_, Proof::Hyp(name)) => {
                    let stated = self
                        .hyps
                        .iter()
                        .rev()
                        .find(|(n, _)| *n == name)
                        .map(|(_, e)| e.lhs.clone())?;
                    match crate::ir::proof_steps::linear::from_hyps(
                        &canon(cur),
                        w,
                        &[(name, stated, v)],
                    ) {
                        Some(step) => step,
                        None => continue,
                    }
                }
                _ => continue,
            };
            return Some(Step::Progress(Box::new((step, term::boolean(w)))));
        }
        if *op == BinOp::Eq
            && let Some((proof, value)) = self.eq_swapped(cur, swap)
        {
            return Some(Step::Progress(Box::new((proof, term::boolean(value)))));
        }
        None
    }

    /// `(a == b) = !v` from `ne : (a != b) = v`, by cases on `a == b`: in
    /// the case where it is `!v`, that is the case's hypothesis; in the
    /// other, `a != b` is `Bool.not(v)` by [`ne_of_eq`], so `!v` equals
    /// `v` by `ne` and the case cannot happen.
    fn eq_of_ne(&mut self, a: &Term, b: &Term, ne: Proof, v: bool) -> Proof {
        let on = canon(&term::binop(BinOp::Eq, a.clone(), b.clone()));
        let hyp = self.fresh_hyp();
        let goal = term::boolean(!v);
        let impossible = Proof::Absurd {
            contradiction: Box::new(Proof::Trans {
                terms: vec![
                    term::boolean(!v),
                    canon(&term::binop(BinOp::Neq, a.clone(), b.clone())),
                    term::boolean(v),
                ],
                steps: vec![
                    Proof::Symm(Box::new(ne_of_eq(a, b, Proof::Hyp(hyp.clone()), v))),
                    ne,
                ],
            }),
            lhs: on.clone(),
            rhs: goal.clone(),
        };
        let holds = Proof::Hyp(hyp.clone());
        let (if_true, if_false) = if v {
            (impossible, holds)
        } else {
            (holds, impossible)
        };
        Proof::Cases {
            on,
            hyp,
            if_true: Box::new(if_true),
            if_false: Box::new(if_false),
        }
    }

    /// `(a == b) = v` from `(b == a) = v`, which a hypothesis states or a
    /// complement rule reads from one about `b != a`, by `bool.beq.symm`;
    /// only with `swap`, so the complement it asks does not swap back.
    fn eq_swapped(&mut self, cur: &Term, swap: bool) -> Option<(Proof, bool)> {
        let ResolvedExpr::BinOp(BinOp::Eq, a, b) = &cur.node else {
            return None;
        };
        let (a, b) = (canon(a), canon(b));
        if !swap || a == b || !self.beq_is_equality(&a) {
            return None;
        }
        let swapped = canon(&term::binop(BinOp::Eq, b.clone(), a.clone()));
        let (inner, value) = match self.hyp_for(&swapped) {
            Some((name, value)) => (Proof::Hyp(name), value),
            None => match self.complement_in(&swapped, false)? {
                Step::Progress(done) => *done,
                _ => return None,
            },
        };
        let v = term::bool_value(&value)?;
        let proof = Proof::Trans {
            terms: vec![canon(cur), swapped, value],
            steps: vec![
                Proof::Rule {
                    rule: WallRule::BeqSymm,
                    subst: vec![("a".into(), a), ("b".into(), b)],
                    premises: Vec::new(),
                },
                inner,
            ],
        };
        Some((proof, v))
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
                // A field of a record literal, or of an update: the value
                // written to it, or the base's field.
                if matches!(
                    obj.node,
                    ResolvedExpr::RecordCreate { .. } | ResolvedExpr::RecordUpdate { .. }
                ) {
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
                let (countdown, halving) = self.counts_down(&def);
                // In a case of an induction along the countdown itself, it
                // also opens where linear arithmetic decides its guard: the
                // call it leaves is one the induction hypothesis talks about.
                if countdown
                    && term::eval_closed(&canon(&s)).is_none()
                    && self.hyp_for(&s).is_none()
                    && self.complement(&canon(&s)).is_none()
                    && (!(halving || self.inducting.contains(id))
                        || self.decide_linearly(&canon(&s)).is_none())
                {
                    return self.settle(cur, None);
                }
                let ev = self.whnf(&s)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value, &self.hyps) {
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
                    // Splitting on a countdown's guard here would unroll
                    // the recursion one more level in every branch; it is
                    // split on only where nothing else closes the case
                    // (see `countdown_guard`).
                    None if countdown => self.settle(cur, None),
                    None => {
                        let g = ev.blocked.or_else(|| {
                            (matches!(value.ty(), Some(crate::ast::Type::Bool))
                                || matches!(value.node, ResolvedExpr::BinOp(..)))
                            .then_some(value.clone())
                        });
                        let g = self.bool_guard(arms, &value, g);
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
                    Some(v) => Ok(Step::Progress(Box::new((
                        Proof::Compute {
                            lhs: canon(cur),
                            rhs: term::boolean(!v),
                        },
                        term::boolean(!v),
                    )))),
                    None => self.settle(cur, blocked),
                }
            }
            ResolvedExpr::Call(..) | ResolvedExpr::BinOp(..) | ResolvedExpr::Neg(_) => {
                self.args_then_settle(cur)
            }
            ResolvedExpr::Match { subject, arms } => {
                let ev = self.whnf(subject)?;
                let value = ev.chain.cur().clone();
                match select_arm(arms, &value, &[]) {
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
                        let g = self.bool_guard(arms, &value, g);
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
        if let Some(step) = bits_step(cur) {
            return Ok(step);
        }
        if let Some(step) = self.default_step(cur) {
            return Ok(step);
        }
        if let Some(step) = self.division_ok_step(cur) {
            return Ok(Step::Progress(Box::new(step)));
        }
        if let Some(step) = self.division_step(cur)? {
            return Ok(step);
        }
        if let Some(step) = self.pick_step(cur, &blocked)? {
            return Ok(step);
        }
        self.settle(cur, blocked)
    }

    /// Euclidean division of an open Int term `t` by a literal `k > 0`
    /// takes one step where linear arithmetic over ℤ pins its value. Read
    /// `t` as `k * q + r`, each coefficient of `r` the remainder of its own
    /// by `k` rounded toward zero, so smaller than `k` and of its sign.
    /// Where the facts in scope put `r`, or `r` moved by one `k` towards
    /// that range, in `0 <= r < k`, `t % k` is that remainder and `t / k`
    /// is `q` (moved by one the other way); elsewhere, where `q` is not
    /// zero, `t % k` is `r % k` and `t / k` is `q + r / k`
    /// (`(h + 2016) % 2016` is `h % 2016`). Each step is the Int equality
    /// decided by its two orders and `int.eq.of_beq`, which the
    /// remainders' recomposition and range settle over ℤ, as `omega`
    /// would. What a hypothesis or a cited law states about the call comes
    /// first.
    fn division_step(&mut self, cur: &Term) -> Result<Option<Step>, String> {
        use crate::ir::hir::BuiltinIntrinsic;
        use crate::ir::proof_steps::ring::{Poly, poly};
        use num_bigint::BigInt;
        use num_traits::{Signed, Zero};
        let ResolvedExpr::Call(ResolvedCallee::Intrinsic(op), args) = &cur.node else {
            return Ok(None);
        };
        let modulo = match op {
            BuiltinIntrinsic::IntModEuclid => true,
            BuiltinIntrinsic::IntDivEuclid => false,
            _ => return Ok(None),
        };
        let [t, k_term] = args.as_slice() else {
            return Ok(None);
        };
        let Some(k) = term::int_value(k_term).filter(|k| k.is_positive()) else {
            return Ok(None);
        };
        if term::int_value(t).is_some() {
            return Ok(None);
        }
        let mut atoms = Vec::new();
        let p = poly(t, &mut atoms);
        let (mut q, mut r) = (Poly::new(), Poly::new());
        for (m, c) in &p {
            // Rounded toward zero: `c - quot * k` keeps the sign of `c`.
            let quot = c / &k;
            let rem = c - &quot * &k;
            if !quot.is_zero() {
                q.insert(m.clone(), quot);
            }
            if !rem.is_zero() {
                r.insert(m.clone(), rem);
            }
        }
        let zero = || term::int(&BigInt::from(0));
        let k_lit = term::int(&k);
        // Whether the decided comparisons in scope put `r` in `0..k`. Only
        // where each atom of `r` is in one of them: an atom no comparison
        // bounds cannot be eliminated, and this is tried at every division
        // evaluation stops at.
        let mut bounded = Vec::new();
        for (_, e) in &self.hyps {
            if let Some(v) = term::bool_value(&e.rhs) {
                crate::ir::proof_steps::linear::as_nonneg(&e.lhs, v, &mut bounded);
            }
        }
        let in_range = |env: &Self, rem: &Poly| {
            let all_bounded = rem.keys().flatten().all(|&i| bounded.contains(&atoms[i]));
            if !all_bounded {
                return false;
            }
            let r = canon(&poly_term(rem, &atoms));
            let low = canon(&term::binop(BinOp::Lte, zero(), r.clone()));
            let high = canon(&term::binop(BinOp::Lt, r, k_lit.clone()));
            env.plain_linear(&low, true).is_some() && env.plain_linear(&high, true).is_some()
        };
        // The remainder as read, then, where it has a part that is not a
        // constant, one `k` towards `0..k`: down from a positive constant
        // (`(x + 255) % 256` for `1 - 256 <= x < 1`), up from a negative
        // one or a negative coefficient (`(x - 1) % 256` for
        // `1 <= x <= 256`). The quotient moves the other way.
        let mut candidates = vec![(r.clone(), BigInt::zero())];
        if r.keys().any(|m| !m.is_empty()) {
            let constant = r.get(&Vec::new()).cloned().unwrap_or_default();
            let carry = if constant.is_positive() {
                BigInt::from(1)
            } else if constant.is_negative() || r.values().any(|c| c.is_negative()) {
                BigInt::from(-1)
            } else {
                BigInt::zero()
            };
            if !carry.is_zero() {
                let mut moved = r.clone();
                *moved.entry(Vec::new()).or_default() -= &carry * &k;
                moved.retain(|_, c| !c.is_zero());
                candidates.push((moved, carry));
            }
        }
        for (rem, carry) in candidates {
            if in_range(self, &rem) {
                let rem_term = canon(&poly_term(&rem, &atoms));
                let mut quot = q.clone();
                *quot.entry(Vec::new()).or_default() += &carry;
                quot.retain(|_, c| !c.is_zero());
                let to = if modulo {
                    rem_term
                } else {
                    canon(&poly_term(&quot, &atoms))
                };
                if to == *term::canon_ref(cur) {
                    break;
                }
                if let Some(step) = self.known(cur)? {
                    return Ok(Some(step));
                }
                if let Some(step) = self.division_equal(cur, &to, true) {
                    return Ok(Some(step));
                }
                break;
            }
        }
        // Elsewhere, the parts of `t` that are multiples of `k` come out
        // whole, the rest stays as it is: `(a * 256 + h) % 256` is
        // `h % 256`, `(a * 256 + h) / 256` is `a + h / 256`. That reads no
        // hypothesis, so it is found once for each division.
        let (whole, rest): (Poly, Poly) = p
            .iter()
            .map(|(m, c)| (m.clone(), c.clone()))
            .partition(|(_, c)| (c % &k).is_zero());
        if whole.is_empty() {
            return Ok(None);
        }
        // Only towards a division a hypothesis in scope already names: the
        // step is taken where it lets a hypothesis decide, not wherever a
        // multiple can come out (unpacking a value that is not read
        // further costs every step after it).
        let inner = canon(&term::intrinsic(
            *op,
            vec![canon(&poly_term(&rest, &atoms)), k_lit.clone()],
        ));
        if !self
            .hyps
            .iter()
            .any(|(_, e)| holds(&e.lhs, &inner) || holds(&e.rhs, &inner))
        {
            return Ok(None);
        }
        if let Some(step) = self.known(cur)? {
            return Ok(Some(step));
        }
        let key = format!("{:?}", canon(cur));
        let remembered = self.wholes.borrow().get(&key).cloned();
        let found = match remembered {
            Some(found) => found,
            None => {
                let whole: Poly = whole.into_iter().map(|(m, c)| (m, c / &k)).collect();
                let to = if modulo {
                    inner
                } else {
                    canon(&term::binop(BinOp::Add, poly_term(&whole, &atoms), inner))
                };
                let found = self.division_equal(cur, &to, false).map(|step| match step {
                    Step::Progress(b) => *b,
                    _ => unreachable!("a division step makes progress"),
                });
                self.wholes.borrow_mut().insert(key, found.clone());
                found
            }
        };
        Ok(found.map(|b| Step::Progress(Box::new(b))))
    }

    /// The step `cur = to` for a division `cur` by its two orders, each
    /// over ℤ by [`Self::division_linear`], with the decided comparisons
    /// in scope where `with_hyps`.
    fn division_equal(&mut self, cur: &Term, to: &Term, with_hyps: bool) -> Option<Step> {
        let subst = vec![("a".to_string(), canon(cur)), ("b".to_string(), to.clone())];
        let (orders, _) = WallRule::EqOfLeGe
            .instantiate(&subst)
            .expect("rule binders");
        let premises = orders
            .iter()
            .map(|e| self.division_linear(&e.lhs, term::bool_value(&e.rhs)?, with_hyps))
            .collect::<Option<Vec<_>>>()?;
        Some(Step::Progress(Box::new((
            Proof::Rule {
                rule: WallRule::EqOfBeq,
                subst: subst.clone(),
                premises: vec![Proof::Rule {
                    rule: WallRule::EqOfLeGe,
                    subst,
                    premises,
                }],
            },
            to.clone(),
        ))))
    }

    /// `goal = value` for an Int comparison `goal` about divisions by
    /// literals, from what those divisions give linear arithmetic
    /// (`int.div_mod_recompose`, `int.mod_range`) and, where `with_hyps`,
    /// the decided comparisons in scope, over ℤ: the facts a step of
    /// [`Self::division_step`] needs, without the wider reading of
    /// [`Self::linear_proof`]. The division facts the certificate uses are
    /// cut in just above it.
    fn division_linear(&mut self, goal: &Term, value: bool, with_hyps: bool) -> Option<Proof> {
        use crate::ir::proof_steps::linear;
        let mut atoms = Vec::new();
        let mut facts = vec![linear::as_nonneg(goal, !value, &mut atoms)?];
        let mut names = Vec::new();
        let in_scope = if with_hyps { &self.hyps[..] } else { &[] };
        for (n, e) in in_scope {
            let Some(v) = term::bool_value(&e.rhs) else {
                continue;
            };
            if let Some(p) = linear::as_nonneg(&e.lhs, v, &mut atoms) {
                facts.push(p);
                names.push(n.clone());
            }
        }
        let plain = names.len();
        let (divisions, _) = division_facts(&[goal], &mut literal_positive);
        for (f, _) in &divisions {
            facts.push(linear::as_nonneg(f, true, &mut atoms)?);
        }
        let (used, weights, rounded) = settle_over_integers(&facts)?;
        // The cuts are named apart from every name in scope but not from
        // the count of fresh names, so an evaluation that takes this step
        // is still remembered ([`super::memo`]): the names are local to
        // the step.
        let taken = |n: &String| self.givens.contains(n) || self.hyps.iter().any(|(h, _)| h == n);
        let mut local = (0..).map(|i| format!("h_div{i}")).filter(|n| !taken(n));
        let mut hyps = Vec::new();
        let mut cuts = Vec::new();
        for k in used.into_iter().map(|i| i - 1) {
            if k < plain {
                hyps.push(names[k].clone());
            } else {
                let (fact, proof) = divisions[k - plain].clone();
                let name = local.next().expect("names without end");
                hyps.push(name.clone());
                cuts.push((name, fact, proof));
            }
        }
        let mut proof = Proof::Linear {
            goal: canon(goal),
            value,
            hyps,
            weights,
            cuts: rounded,
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

    /// `Int.min`, `Int.max` or `Int.abs` takes one step by the rule its
    /// comparison picks: `a <= b` for the smaller and the larger of two,
    /// `a < 0` for the absolute value. What a hypothesis or a cited law
    /// states about the call comes first. An undecided comparison is where
    /// the prover splits, after what an argument stopped at (`blocked`): the
    /// inner of two nested calls is read before the outer.
    fn pick_step(&mut self, cur: &Term, blocked: &Option<Term>) -> Result<Option<Step>, String> {
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return Ok(None);
        };
        let (test, holds, fails, binders): (Term, WallRule, WallRule, &[&str]) =
            match (name.as_str(), args.as_slice()) {
                ("Int.min", [a, b]) => (
                    term::binop(BinOp::Lte, a.clone(), b.clone()),
                    WallRule::MinLe,
                    WallRule::MinGt,
                    &["a", "b"],
                ),
                ("Int.max", [a, b]) => (
                    term::binop(BinOp::Lte, a.clone(), b.clone()),
                    WallRule::MaxLe,
                    WallRule::MaxGt,
                    &["a", "b"],
                ),
                ("Int.abs", [a]) => (
                    term::binop(BinOp::Lt, a.clone(), term::int(&0.into())),
                    WallRule::AbsNeg,
                    WallRule::AbsNonneg,
                    &["a"],
                ),
                _ => return Ok(None),
            };
        if let Some(step) = self.known(cur)? {
            return Ok(Some(step));
        }
        let ev = self.whnf(&test)?;
        let Some(value) = term::bool_value(ev.chain.cur()) else {
            let on = blocked.clone().or(ev.blocked);
            return Ok(Some(Step::Blocked(on.unwrap_or_else(|| canon(&test)))));
        };
        let rule = if value { holds } else { fails };
        let subst: Vec<(String, Term)> = binders
            .iter()
            .map(|n| n.to_string())
            .zip(args.iter().cloned())
            .collect();
        let (_, concl) = rule.instantiate(&subst).expect("rule binders");
        Ok(Some(Step::Progress(Box::new((
            Proof::Rule {
                rule,
                subst,
                premises: vec![ev.chain.finish().1],
            },
            concl.rhs,
        )))))
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
            (term::canon_eq(&set[0], &outer[1])).then_some(set)
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
                        if term::canon_eq(&set[1], j) {
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
                if term::canon_eq(&p[1], &args[1]) {
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
            if *term::canon_ref(part) == *t {
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
        let list_builtin = matches!(
            name.as_str(),
            "List.concat" | "List.len" | "List.reverse" | "List.take" | "List.drop"
        );
        // A call of one of them on closed lists computes to the list it
        // gives, so a rewrite that leaves `List.reverse([128])` behind is
        // followed by its value.
        if list_builtin
            && let Some(v) = term::eval_closed(cur)
            && matches!(v.node, ResolvedExpr::List(_))
            && v != *term::canon_ref(cur)
        {
            return Ok(Some(Step::Progress(Box::new((
                Proof::Compute {
                    lhs: cur.clone(),
                    rhs: v.clone(),
                },
                v,
            )))));
        }
        // `List.contains` compares with `==`. Its rules on an element in
        // front need `(y == x)` decided, also from the comparison written
        // the other way round, and only for an element type whose `==` is
        // equality (no Float inside); the element type
        // is the list's, or the looked-for value's where the list has
        // none.
        let is_contains = name == "List.contains" && args.len() == 2;
        let contains = is_contains && {
            let elem = match list.ty() {
                Some(crate::ast::Type::List(e)) => Some((**e).clone()),
                _ => args[1].ty().cloned(),
            };
            elem.is_some_and(|t| self.inputs.symbol_table.beq_is_equality(&t))
        };
        // What a hypothesis or a cited law states about the call comes
        // before its rule.
        if is_contains && let Some(step) = self.known(cur)? {
            return Ok(Some(step));
        }
        if (list_builtin || contains)
            && let ResolvedExpr::List(xs) = &list.node
            && !xs.is_empty()
            && (contains || term::eval_closed(list).is_none())
        {
            // A literal with an open element steps as a cell; a closed one
            // keeps the shape it was written in, except under
            // `List.contains`, which reads it cell by cell.
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
            // `List.len([])` and `List.reverse([])` are closed: compute.
            ("List.len" | "List.reverse", 1, None) => {
                let value = match name.as_str() {
                    "List.len" => term::int(&0.into()),
                    _ => term::nil(),
                };
                return Ok(Some(Step::Progress(Box::new((
                    Proof::Compute {
                        lhs: canon(cur),
                        rhs: value.clone(),
                    },
                    value,
                )))));
            }
            ("List.len", 1, Some((x, a))) => (WallRule::LenCons, bind(&["x", "a"], vec![x, a])),
            ("List.reverse", 1, Some((x, a))) => {
                (WallRule::ReverseCons, bind(&["x", "a"], vec![x, a]))
            }
            ("List.contains", 2, None) => {
                (WallRule::ContainsNil, bind(&["y"], vec![args[1].clone()]))
            }
            ("List.contains", 2, Some((x, a))) if contains => {
                // The element in front is the value looked for, or the rest
                // decides, as `y == x` decides, or, by `bool.beq.symm`,
                // `x == y` (the order the VM compares in); undecided, that
                // is where the prover splits.
                let y = args[1].clone();
                let asked = term::binop(BinOp::Eq, y.clone(), x.clone());
                let ev = self.whnf(&asked)?;
                let (found, premise) = match term::bool_value(ev.chain.cur()) {
                    Some(v) => (v, ev.chain.finish().1),
                    None => {
                        let swapped = term::binop(BinOp::Eq, x.clone(), y.clone());
                        let ev = self.whnf(&swapped)?;
                        let Some(v) = term::bool_value(ev.chain.cur()) else {
                            let on = ev.blocked.unwrap_or_else(|| canon(&swapped));
                            return Ok(Some(Step::Blocked(on)));
                        };
                        let proof = Proof::Trans {
                            terms: vec![canon(&asked), canon(&swapped), term::boolean(v)],
                            steps: vec![
                                Proof::Rule {
                                    rule: WallRule::BeqSymm,
                                    subst: vec![("a".into(), y.clone()), ("b".into(), x.clone())],
                                    premises: Vec::new(),
                                },
                                ev.chain.finish().1,
                            ],
                        };
                        (v, proof)
                    }
                };
                let rule = if found {
                    WallRule::ContainsConsFound
                } else {
                    WallRule::ContainsConsOther
                };
                let subst = bind(&["x", "a", "y"], vec![x, a, y]);
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

    /// `Option.withDefault` or `Result.withDefault` takes one step by its
    /// rule once its first argument is a constructor: the value a `Some`
    /// or an `Ok` holds, the default for `None` or an `Err`.
    fn default_step(&self, cur: &Term) -> Option<Step> {
        use crate::ir::hir::{BuiltinCtor, ResolvedCtor};
        let ResolvedExpr::Call(ResolvedCallee::Builtin(name), args) = &cur.node else {
            return None;
        };
        let [o, d] = args.as_slice() else {
            return None;
        };
        let ResolvedExpr::Ctor(ResolvedCtor::Builtin(c), held) = &o.node else {
            return None;
        };
        let d = ("d".to_string(), d.clone());
        let (rule, subst) = match (name.as_str(), c, held.as_slice()) {
            ("Option.withDefault", BuiltinCtor::OptionSome, [x]) => (
                WallRule::OptionDefaultSome,
                vec![("x".into(), x.clone()), d],
            ),
            ("Option.withDefault", BuiltinCtor::OptionNone, []) => {
                (WallRule::OptionDefaultNone, vec![d])
            }
            ("Result.withDefault", BuiltinCtor::ResultOk, [x]) => {
                (WallRule::ResultDefaultOk, vec![("x".into(), x.clone()), d])
            }
            ("Result.withDefault", BuiltinCtor::ResultErr, [e]) => {
                (WallRule::ResultDefaultErr, vec![("e".into(), e.clone()), d])
            }
            _ => return None,
        };
        Some(self.rule_step(rule, subst))
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
    pub(crate) fn opened_views(
        &mut self,
    ) -> std::rc::Rc<Vec<(Term, Proof, crate::ir::identity::FnId)>> {
        // The views read only the hypotheses and the definitions, which
        // stay the same for one environment: the last ones are given again
        // for the same hypotheses, with the same lookups noted.
        if let Some(memo) = &self.views_memo
            && super::env::same_hyps(&memo.hyps, &self.hyps)
        {
            let (looked_up, views) = (memo.looked_up.clone(), std::rc::Rc::clone(&memo.views));
            for id in looked_up {
                self.traced(super::memo::Effect::Def(id));
            }
            return views;
        }
        let mut out = Vec::new();
        let mut looked_up = Vec::new();
        let hyps = self.hyps.clone();
        for (name, e) in hyps.iter().cloned() {
            if term::bool_value(&e.rhs) != Some(true) {
                continue;
            }
            let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &e.lhs.node else {
                continue;
            };
            if self.inputs.recursive_fns.contains(id) {
                continue;
            }
            looked_up.push(*id);
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
        let out = std::rc::Rc::new(out);
        self.views_memo = Some(Box::new(super::env::ViewsMemo {
            hyps,
            looked_up,
            views: std::rc::Rc::clone(&out),
        }));
        out
    }

    /// A view of a predicate hypothesis that is exactly `t`, with its proof.
    pub(crate) fn view_for(&mut self, t: &Term) -> Option<Proof> {
        let t = term::canon_ref(t);
        let views = self.opened_views();
        let (_, proof, id) = views.iter().find(|(f, _, _)| *f == *t)?;
        let (proof, id) = (proof.clone(), *id);
        self.mark_used(id);
        Some(proof)
    }

    /// `goal = value` for an Int comparison `goal` that the decided
    /// comparisons in scope settle by linear arithmetic alone.
    pub(super) fn plain_linear(&self, goal: &Term, value: bool) -> Option<Proof> {
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
        let (used, weights, cuts) =
            settle_rationally(&facts).or_else(|| settle_over_integers(&facts))?;
        let hyps = used.into_iter().map(|i| names[i - 1].clone()).collect();
        Some(Proof::Linear {
            goal: canon(goal),
            value,
            hyps,
            weights,
            cuts,
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
            .iter()
            .filter(|(f, _, _)| linear::as_nonneg(f, true, &mut Vec::new()).is_some())
            .cloned()
            .collect();
        for (i, (f, _, _)) in views.iter().enumerate() {
            known.push((
                format!("h_view{i}"),
                Eqn::new(f.clone(), term::boolean(true)),
            ));
        }
        let viewed = known.len();
        // A goal that is no comparison of Ints is settled by nothing below;
        // what reading the facts does besides is showing a divisor that is
        // not a literal positive, which may name a hypothesis, so only where
        // there is none is the rest skipped.
        if linear::as_nonneg(goal, !value, &mut Vec::new()).is_none()
            && !open_divisor(goal)
            && !known.iter().any(|(_, e)| open_divisor(&e.lhs))
        {
            return None;
        }
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
        let (mut remainders, quotients) =
            division_facts(&about, &mut |k: &Term| self.divisor_positive(k));
        remainders.extend(self.quotient_ranges(&quotients));
        remainders.extend(self.equalities_as_orders(false));
        for (i, (f, _)) in remainders.iter().enumerate() {
            known.push((
                format!("h_mod{i}"),
                Eqn::new(f.clone(), term::boolean(true)),
            ));
        }
        let modded = known.len();
        let mut atoms = Vec::new();
        let mut facts = vec![linear::as_nonneg(goal, !value, &mut atoms)?];
        for (_, e) in &known {
            facts.push(linear::as_nonneg(
                &e.lhs,
                term::bool_value(&e.rhs)?,
                &mut atoms,
            )?);
        }
        // Only where those do not settle it: the comparisons inside a true
        // conjunction in scope, and that each length the facts name is not
        // negative (`List.len.nonneg`).
        // Over ℤ, with rounded rows, only where reading the facts over
        // the rationals settles nothing.
        let mut extra = Vec::new();
        let settled = match settle_rationally(&facts) {
            Some(settled) => Some(settled),
            None if self.wide_linear => {
                extra = self.more_linear_facts(goal, &known);
                for (i, (f, _, _)) in extra.iter().enumerate() {
                    facts.push(linear::as_nonneg(f, true, &mut atoms)?);
                    known.push((
                        format!("h_more{i}"),
                        Eqn::new(f.clone(), term::boolean(true)),
                    ));
                }
                if extra.is_empty() {
                    None
                } else {
                    settle_rationally(&facts)
                }
            }
            None => None,
        };
        let settled = match settled {
            Some(settled) => Some(settled),
            None => {
                // Over ℤ the Int equations in scope count too, read as
                // two orders each, with what division by a literal in them
                // gives.
                let before = extra.len();
                let orders = self.equalities_as_orders(true);
                let terms: Vec<&Term> = orders.iter().map(|(f, _)| f).collect();
                let (mut more, quotients) =
                    division_facts(&terms, &mut |k: &Term| self.divisor_positive(k));
                more.extend(self.quotient_ranges(&quotients));
                for (f, p) in orders.into_iter().chain(more) {
                    if !known.iter().any(|(_, e)| e.lhs == f)
                        && !extra[before..].iter().any(|(g, _, _)| *g == f)
                    {
                        extra.push((f, p, None));
                    }
                }
                for (i, (f, _, _)) in extra.iter().enumerate().skip(before) {
                    facts.push(linear::as_nonneg(f, true, &mut atoms)?);
                    known.push((
                        format!("h_more{i}"),
                        Eqn::new(f.clone(), term::boolean(true)),
                    ));
                }
                settle_over_integers(&facts)
            }
        };
        // Only where those leave a goal about a product of two terms open:
        // the products of pairs of the comparisons (see [`nonlinear`]). A
        // goal without one is not helped by them, wherever else products
        // stand.
        let settled = match settled {
            Some(settled) => Some(settled),
            None if !super::nonlinear::wanted(&facts, &atoms).is_empty()
                && !self
                    .unsettled_products
                    .contains(&self.product_key(goal, value, &known)) =>
            {
                use super::nonlinear::{OPPOSITE, Source};
                // The opposite of the goal is a factor too, under a case
                // split on the goal whose other case is the hypothesis.
                let mut comparisons = vec![(
                    Eqn::new(canon(goal), term::boolean(!value)),
                    Source::Hyp(OPPOSITE.to_string()),
                )];
                for (k, (n, e)) in known.iter().enumerate() {
                    let source = if k < plain {
                        Source::Hyp(n.clone())
                    } else if k >= modded && extra[k - modded].2.is_none() {
                        Source::Fact(Box::new(extra[k - modded].1.clone()))
                    } else if k >= cited && k < modded {
                        Source::Fact(Box::new(remainders[k - cited].1.clone()))
                    } else {
                        continue;
                    };
                    comparisons.push((e.clone(), source));
                }
                let wanted = super::nonlinear::wanted(&facts, &atoms);
                let products = super::nonlinear::products(&wanted, &atoms, &comparisons);
                if products.is_empty() {
                    None
                } else {
                    let before_products = known.len();
                    for (f, p) in products {
                        facts.push(linear::as_nonneg(&f, true, &mut atoms)?);
                        known.push((
                            format!("h_more{}", extra.len()),
                            Eqn::new(f.clone(), term::boolean(true)),
                        ));
                        extra.push((f, p, None));
                    }
                    let key = self.product_key(goal, value, &known[..before_products]);
                    let r = settle_rationally(&facts).or_else(|| settle_over_integers(&facts));
                    if r.is_none() {
                        if self.unsettled_products.len() >= 256 {
                            self.unsettled_products.remove(0);
                        }
                        self.unsettled_products.push(key);
                    }
                    r
                }
            }
            None => None,
        };
        let (used, weights, rounded) = settled?;
        // Only the hypotheses the certificate uses are named.
        let mut hyps = Vec::new();
        let mut cuts = Vec::new();
        for k in used.into_iter().map(|i| i - 1) {
            let n = &known[k].0;
            if k < plain {
                hyps.push(n.clone());
            } else if k < viewed {
                let (fact, proof, id) = views[k - plain].clone();
                let name = self.fresh_hyp();
                self.mark_used(id);
                hyps.push(name.clone());
                cuts.push((name, fact, proof));
            } else if k >= modded {
                let (fact, proof, law) = extra[k - modded].clone();
                let name = self.fresh_hyp();
                if let Some(law) = law {
                    self.use_law(law);
                }
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
                self.use_law(law);
                hyps.push(name.clone());
                cuts.push((name, fact, proof));
            }
        }
        let opposite = cuts
            .iter()
            .any(|(_, _, from)| super::uses_hyp(from, super::nonlinear::OPPOSITE));
        let mut proof = Proof::Linear {
            goal: canon(goal),
            value,
            hyps,
            weights,
            cuts: rounded,
        };
        for (name, fact, from) in cuts.into_iter().rev() {
            proof = Proof::Have {
                name,
                fact,
                proof: Box::new(from),
                body: Box::new(proof),
            };
        }
        if opposite {
            let holds = Proof::Hyp(super::nonlinear::OPPOSITE.to_string());
            let (if_true, if_false) = if value {
                (holds, proof)
            } else {
                (proof, holds)
            };
            proof = Proof::Cases {
                on: canon(goal),
                hyp: super::nonlinear::OPPOSITE.to_string(),
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            };
        }
        Some(proof)
    }

    /// What [`Self::linear_proof`] reads where the facts it starts from do
    /// not settle `goal`, each with its proof and the builtin fact it cites:
    /// each Int comparison among the conjuncts of a true `Bool.and` in scope
    /// (by the `Bool.and` eliminations), then `List.len(l) >= 0` for each
    /// length `goal`, `known` and those conjuncts name (`List.len.nonneg`).
    fn more_linear_facts(
        &self,
        goal: &Term,
        known: &[(String, Eqn)],
    ) -> Vec<(Term, Proof, Option<LawRef>)> {
        use crate::ir::proof_steps::linear;
        fn lengths(t: &Term, out: &mut Vec<Term>) {
            if matches!(t.node, ResolvedExpr::Match { .. }) {
                return;
            }
            if let ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) = &t.node
                && b == "List.len"
                && args.len() == 1
                && !out.contains(&*term::canon_ref(&args[0]))
            {
                out.push(canon(&args[0]));
            }
            for c in term::children(t) {
                lengths(c, out);
            }
        }
        let stated = |t: &Term| known.iter().any(|(_, e)| *term::canon_ref(&e.lhs) == *t);
        // The atoms the goal and the facts already read: a conjunct about
        // none of them cannot take part in a certificate.
        let mut about = Vec::new();
        linear::as_nonneg(goal, true, &mut about);
        for (_, e) in known {
            linear::as_nonneg(&e.lhs, true, &mut about);
        }
        let mut out: Vec<(Term, Proof, Option<LawRef>)> = Vec::new();
        for (name, e) in &self.hyps {
            if term::bool_value(&e.rhs) != Some(true) {
                continue;
            }
            if !matches!(&e.lhs.node, ResolvedExpr::Call(ResolvedCallee::Builtin(b), _) if b == "Bool.and")
            {
                continue;
            }
            let mut lines = Vec::new();
            super::split_and(&e.lhs, Proof::Hyp(name.clone()), &mut lines);
            for (line, proof) in lines {
                let mut atoms = Vec::new();
                if linear::as_nonneg(&line, true, &mut atoms).is_some()
                    && atoms.iter().any(|a| about.contains(a))
                    && !stated(&line)
                    && !out.iter().any(|(f, _, _)| *f == line)
                {
                    out.push((line, proof, None));
                }
            }
        }
        let mut lists = Vec::new();
        lengths(goal, &mut lists);
        for (_, e) in known {
            lengths(&e.lhs, &mut lists);
        }
        for (f, _, _) in &out {
            lengths(f, &mut lists);
        }
        let Some(fact) = crate::ir::proof_steps::facts::named("List.len.nonneg") else {
            return out;
        };
        let law = fact.law_ref();
        for l in lists {
            let subst = vec![(law.givens[0].clone(), l)];
            let Ok(nonneg) = term::subst(&law.lhs, &subst) else {
                continue;
            };
            let nonneg = canon(&nonneg);
            if stated(&nonneg) || out.iter().any(|(f, _, _)| *f == nonneg) {
                continue;
            }
            let proof = Proof::Law {
                law: law.key.clone(),
                subst,
                premise: None,
            };
            out.push((nonneg, proof, Some(law.clone())));
        }
        out
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
                        let proof = self.discharge_when(&law, &when);
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
    /// Whether `t` holds a call of a definition that does not recurse and
    /// is not a `match`, whose body at the call's arguments is `target`:
    /// evaluation would open that call straight back to `target`.
    fn opens_to(&mut self, t: &Term, target: &Term) -> bool {
        if let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &t.node
            && !self.inputs.recursive_fns.contains(id)
            && let Some(def) = self.def(*id)
            && def.lets.is_empty()
            && !matches!(def.body.node, ResolvedExpr::Match { .. })
            && let Ok(outer) = def.outer(args)
            && let Ok(body) = term::subst(&def.body, &outer)
            && *term::canon_ref(&body) == *target
        {
            return true;
        }
        term::children(t)
            .into_iter()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
            .any(|c| self.opens_to(c, target))
    }

    /// One rewrite of `cur` at its root by a cited law. Where several
    /// apply, the one with the smallest result wins, the first in citation
    /// order among those of one size (see [`pick_rewrite`]).
    fn rewrite_with_cited(&mut self, cur: &Term) -> Result<Option<(Proof, Term)>, String> {
        use super::rewrite::Equation;
        let mut found: Vec<(LawRef, Proof, Term)> = Vec::new();
        for law in self.rewrite_laws.clone() {
            let eq = Equation::Law(Box::new(law.clone()));
            if let Some((p, to)) = self.try_equation(&eq, cur) {
                // A result that holds the term it rewrote would be rewritten
                // again forever (`f(x, [])` to `g(f(x, []))`), and so would
                // one that holds a call whose definition opens to it
                // (`intoBytes(ops, [])` to `List.concat([], bytesOf(ops))`
                // where `bytesOf(ops)` is `intoBytes(ops, [])`).
                if !holds(&to, &canon(cur)) && !self.opens_to(&to, &canon(cur)) {
                    found.push((law, p, to));
                }
            }
        }
        // Where no cited law applies, the earlier laws of the automatic
        // pass, the first few that match in file order.
        if found.is_empty() {
            for law in self.auto_laws.clone() {
                if found.len() == MAX_AUTO_CANDIDATES {
                    break;
                }
                let eq = Equation::Law(Box::new(law.clone()));
                if let Some((p, to)) = self.try_equation(&eq, cur)
                    && !holds(&to, &canon(cur))
                    && !self.opens_to(&to, &canon(cur))
                {
                    found.push((law, p, to));
                }
            }
        }
        let Some(k) = pick_rewrite(found.iter().map(|(_, _, to)| to)) else {
            return Ok(None);
        };
        let (law, p, to) = found.swap_remove(k);
        self.use_law(law);
        Ok(Some((p, to)))
    }

    /// Evaluate `t` and then, below its head, each part in turn, so a
    /// hypothesis or cited law applies inside a constructor too.
    pub(crate) fn normalize(&mut self, t: &Term, depth: usize) -> Result<Chain, String> {
        let chain = self.whnf(t)?.chain;
        self.normalize_below(chain, depth)
    }

    /// [`Self::normalize`] going on from `chain`, an evaluation of a term
    /// to its head under the hypotheses in scope, so it is not evaluated
    /// again.
    pub(crate) fn normalize_below(
        &mut self,
        mut chain: Chain,
        depth: usize,
    ) -> Result<Chain, String> {
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
        // constructor and the arguments of a call evaluation stopped at,
        // going on from where evaluating the sides stopped.
        let mut nl = self.normalize_below(l.chain.clone(), 8)?;
        let nr = self.normalize_below(r.chain.clone(), 8)?;
        if term::canon_eq(nl.cur(), nr.cur()) {
            return Ok(Ok(meet(nl, nr)));
        }
        // A call on one side that a call on the other is once their Int
        // arguments are read as polynomials (`f(0 + n)` and `f(n)`): the
        // left one is written as the right one, by a ring step at each
        // such argument.
        if is_int(nl.cur()) {
            for (p, to) in align_calls(nl.cur(), nr.cur()) {
                nl.push(p, to);
            }
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
        // Not inside a record of a refined type, which Lean states as a
        // value with the proof of its invariant.
        if !has_refined_record(nl.cur(), self.inputs)
            && !has_refined_record(nr.cur(), self.inputs)
            && let Some(steps) = super::rewrite::ring_bridge(nl.cur(), nr.cur())
            && !steps.is_empty()
        {
            for (proof, to) in steps {
                nl.push(proof, to);
            }
            return Ok(Ok(meet(nl, nr)));
        }
        // The same, where linear arithmetic shows such parts equal.
        if !has_refined_record(nl.cur(), self.inputs)
            && !has_refined_record(nr.cur(), self.inputs)
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
        // Hypotheses that contradict each other once read again: list
        // lengths, and normal forms (see [`super::hypotheses`]).
        if let Some(proof) = self.refute_case(lhs, rhs) {
            return Ok(Ok(proof));
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
        // A `match` stopped at a subject of a sum or list type, in a side
        // or else in a hypothesis that calls a function: one case per
        // constructor, where the hypothesis then evaluates.
        if depth > 0 {
            let mut found = [nl.cur().clone(), nr.cur().clone()]
                .iter()
                .find_map(|t| self.stuck_subject(t, false));
            for (_, e) in self.hyps.clone().iter().rev() {
                if found.is_some() {
                    break;
                }
                if matches!(e.lhs.node, ResolvedExpr::Call(ResolvedCallee::Fn(_), _))
                    && term::bool_value(&e.rhs).is_some()
                    && !self.opened.contains(&*term::canon_ref(&e.lhs))
                {
                    found = self.stuck_subject(&e.lhs, true);
                }
            }
            if let Some(st) = found {
                return self.split_on(st, lhs, rhs, depth).map(Ok);
            }
            // A `match` stopped at a list builtin over a list variable: that
            // variable as `[]` and as a cell.
            if let Some(proof) =
                self.split_list_var_under_match(&[nl.cur(), nr.cur()], lhs, rhs, depth)
            {
                return Ok(Ok(proof));
            }
            // A list variable a hypothesis says is empty or not: that
            // variable as `[]` and as a cell.
            if let Some(proof) = self.split_list_var_stated_empty(lhs, rhs, depth) {
                return Ok(Ok(proof));
            }
        }
        // An Int comparison in scope whose sides call a definition that
        // chooses by a `match`: each call evaluated, split on what its
        // `match` reads where that is open, and its value cut in.
        if depth > 0
            && let Some(proof) = self.open_comparison_calls(lhs, rhs, depth)?
        {
            return Ok(Ok(proof));
        }
        // Last, rewriting both sides to a normal form with the cited laws
        // and the builtin list facts.
        if let Some(proof) = self.meet_by_rewriting(&nl, &nr)? {
            return Ok(Ok(proof));
        }
        // Two sides that differ in the order of a `Bool.or` or a `Bool.and`
        // (`Bool.or(a, b)` and `Bool.or(b, a)`), or a side stopped at a
        // countdown whose guard is an open Int comparison: split on that
        // Bool, as a `because` line stating it either way would. With `a`
        // decided both connectives compute to one term, and the countdown
        // opens at the arm its guard selects. The sides are compared at
        // one position, never rewritten, so this is no search; a split
        // that does not close is undone.
        if depth > 0
            && let Some(proof) = self.split_on_open_order(nl.cur(), nr.cur(), depth)
        {
            let rc = nr.cur().clone();
            nl.push(proof, rc);
            return Ok(Ok(meet(nl, nr)));
        }
        // Nothing closed the case as its hypotheses are stated: evaluate
        // them the way the claim was, and the claim again under them.
        if let Some(proof) =
            self.prove_with_evaluated_hypotheses(lhs, rhs, &[nl.cur(), nr.cur()], depth)
        {
            return Ok(Ok(proof));
        }
        Err(self.stopped_at(nl.cur(), nr.cur()))
    }

    /// Where evaluation stopped at two different terms: rewrite each side
    /// toward a normal form with the cited laws and the builtin facts of
    /// [`crate::ir::proof_steps::facts::NORMALIZING`], one rewrite at a
    /// time at the outermost-leftmost position (a cited law before a fact,
    /// the smallest result among several, see [`pick_rewrite`]), each
    /// followed by evaluation again, so a literal a rewrite leaves behind
    /// computes and a hypothesis applies to what the rewrite exposed. The
    /// sides meet or the attempt gives up: when neither side changes, when
    /// a side would come back to a term it already had, or after
    /// [`MAX_REWRITE_ROUNDS`] rounds; every rewrite also burns fuel. A
    /// failed attempt leaves the fuel and the cited laws as it found them,
    /// for what the caller tries next.
    fn meet_by_rewriting(&mut self, nl: &Chain, nr: &Chain) -> Result<Option<Proof>, String> {
        use super::rewrite::Equation;
        let eqs: Vec<Equation> = self
            .rewrite_laws
            .iter()
            .chain(self.fact_rules.iter())
            .chain(self.auto_laws.iter())
            .cloned()
            .map(|l| Equation::Law(Box::new(l)))
            .collect();
        if eqs.is_empty() {
            return Ok(None);
        }
        let (fuel, laws) = (self.fuel, self.laws.clone());
        match self.rewrite_to_meet(&eqs, nl, nr) {
            Ok(Some(proof)) => Ok(Some(proof)),
            _ => {
                self.fuel = fuel;
                self.laws = laws;
                Ok(None)
            }
        }
    }

    fn rewrite_to_meet(
        &mut self,
        eqs: &[super::rewrite::Equation],
        nl: &Chain,
        nr: &Chain,
    ) -> Result<Option<Proof>, String> {
        if let Some(proof) = self.same_up_to_cells(nl, nr) {
            return Ok(Some(proof));
        }
        let mut sides = [nl.clone(), nr.clone()];
        let mut seen = [vec![canon(nl.cur())], vec![canon(nr.cur())]];
        for _ in 0..MAX_REWRITE_ROUNDS {
            let mut changed = false;
            for i in 0..2 {
                self.burn()?;
                let cur = sides[i].cur().clone();
                let Some((path, proof, to)) = self.rewrite_somewhere(eqs, &cur, &mut Vec::new())?
                else {
                    continue;
                };
                let mut next = sides[i].clone();
                next.push_at(&path, proof, &to);
                let again = self.normalize(&next.cur().clone(), 8)?;
                if !again.is_empty() {
                    let (to, proof) = again.finish();
                    next.push(proof, to);
                }
                let now = canon(next.cur());
                if seen[i].contains(&now) {
                    continue;
                }
                seen[i].push(now);
                sides[i] = next;
                changed = true;
                if let Some(proof) = self.same_up_to_cells(&sides[0], &sides[1]) {
                    return Ok(Some(proof));
                }
            }
            if !changed {
                break;
            }
        }
        Ok(None)
    }

    /// `l` and `r` meet as they stand, or once every list literal in both
    /// is written as cells and evaluated again: a literal a computation
    /// gave (`[128]`) against the cell (`List.prepend(128, rest)`) the
    /// other side built.
    fn same_up_to_cells(&mut self, l: &Chain, r: &Chain) -> Option<Proof> {
        if term::canon_eq(l.cur(), r.cur()) {
            return Some(meet(l.clone(), r.clone()));
        }
        let mut sides = [l.clone(), r.clone()];
        let mut celled = false;
        for side in sides.iter_mut() {
            let Some(cells) = every_literal_as_cells(side.cur()) else {
                continue;
            };
            celled = true;
            let (to, proof) = cells.finish();
            side.push(proof, to);
            let again = self.normalize(&side.cur().clone(), 8).ok()?;
            if !again.is_empty() {
                let (to, proof) = again.finish();
                side.push(proof, to);
            }
        }
        if !celled || !term::canon_eq(sides[0].cur(), sides[1].cur()) {
            return None;
        }
        let [l, r] = sides;
        Some(meet(l, r))
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

    /// [`Env::nothing_to_split`]'s last split: on `a` where the sides
    /// differ in the order of a connective, else on an open countdown
    /// guard in a side, or in a hypothesis where the sides are `true` and
    /// `false` (inside such a split, only on one linear arithmetic
    /// decides).
    fn split_on_open_order(&mut self, l: &Term, r: &Term, depth: usize) -> Option<Proof> {
        if let Some(a) = swapped_connective(l, r)
            && let Some(proof) = self.split_or_undo(&a, l, r, depth)
        {
            return Some(proof);
        }
        // A hypothesis is looked at only once the sides are two different
        // Bools: what is left is to show the case cannot happen.
        let hyps: Vec<Term> = if is_bool_value(l) && is_bool_value(r) {
            self.hyps.iter().map(|(_, e)| e.lhs.clone()).collect()
        } else {
            Vec::new()
        };
        let g = [l, r]
            .into_iter()
            .chain(hyps.iter())
            .find_map(|t| self.countdown_guard(t))?;
        if self.countdown_split && self.decide_linearly(&g).is_none() {
            return None;
        }
        let outer = std::mem::replace(&mut self.countdown_split, true);
        let proof = self.split_or_undo(&g, l, r, depth);
        self.countdown_split = outer;
        proof
    }

    /// The guard of the first call in `t` of a countdown (a function
    /// recursing on an Int towards a `match` on a comparison, `match n <= 0`,
    /// not a division down to zero) that evaluation left closed because
    /// nothing in scope decides it.
    fn countdown_guard(&mut self, t: &Term) -> Option<Term> {
        if let ResolvedExpr::Call(ResolvedCallee::Fn(id), args) = &t.node
            && let Some(def) = self.def(*id)
            && let ResolvedExpr::Match { subject, .. } = &def.body.node
            && self.counts_down(&def) == (true, false)
            && let Ok(outer) = def.outer(args)
            && let Ok(s) = term::subst(subject, &outer)
            && let s = canon(&s)
            && let ResolvedExpr::BinOp(
                BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Eq | BinOp::Neq,
                a,
                b,
            ) = &s.node
            && (is_int(a) || is_int(b))
            && term::eval_closed(&s).is_none()
            && self.hyp_for(&s).is_none()
            && self.complement(&s).is_none()
        {
            return Some(s);
        }
        // A call that is a part of an Int term (`pow2(n) * k`, a quotient)
        // stays closed: linear arithmetic reads it as an atom, and opening
        // it trades a call the facts in scope talk about for one they do
        // not.
        if is_int(t) && !matches!(t.node, ResolvedExpr::Call(ResolvedCallee::Fn(_), _)) {
            return None;
        }
        term::children(t)
            .into_iter()
            .find_map(|c| self.countdown_guard(c))
    }

    /// `lc = rc` by cases on `g`, each branch evaluated on from `lc` and
    /// `rc`; `None`, with the fuel, the cited laws and the hypothesis
    /// names as they were, where a branch does not close.
    fn split_or_undo(&mut self, g: &Term, lc: &Term, rc: &Term, depth: usize) -> Option<Proof> {
        if depth == 0 || is_bool_value(g) || self.hyp_for(g).is_some() {
            return None;
        }
        let (fuel, laws, named) = (self.fuel, self.laws.clone(), self.next_hyp_count());
        let hyp = self.fresh_hyp();
        let branch = |env: &mut Self, v: bool| -> Result<Proof, String> {
            env.hyps
                .push((hyp.clone(), Eqn::new(canon(g), term::boolean(v))));
            let out = env.prove_by_evaluation(lc, rc, depth - 1);
            env.hyps.pop();
            out
        };
        match branch(self, true).and_then(|t| Ok((t, branch(self, false)?))) {
            Ok((if_true, if_false)) => Some(Proof::Cases {
                on: canon(g),
                hyp,
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            }),
            Err(_) => {
                self.fuel = fuel;
                self.laws = laws;
                self.rewind_hyp_count(named);
                None
            }
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
        if term::canon_eq(l.chain.cur(), r.chain.cur()) {
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
            let stopped = [l.chain.cur(), r.chain.cur(), &g];
            if let Some(proof) = self.prove_with_evaluated_hypotheses(lhs, rhs, &stopped, depth) {
                return Ok(proof);
            }
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
    pub(crate) fn refute_linearly(&mut self) -> Option<Proof> {
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
                    _ => ne_of_eq(&canon(x), &canon(y), equal, true),
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
        // Int equations in scope that no Int has together (`x % 32 = 2`
        // and `(x + 128) % 32 = 3`): no comparison is stated, so the
        // linear step shows `0 > 0`, which computes to false.
        if self
            .hyps
            .iter()
            .any(|(_, e)| term::bool_value(&e.rhs).is_none() && is_int(&e.lhs))
        {
            let zero = || term::int(&0.into());
            let never = canon(&term::binop(BinOp::Gt, zero(), zero()));
            let shown = self.linear_proof(&never, true)?;
            return Some(Proof::Trans {
                terms: vec![term::boolean(true), never.clone(), term::boolean(false)],
                steps: vec![
                    Proof::Symm(Box::new(shown)),
                    Proof::Compute {
                        lhs: never,
                        rhs: term::boolean(false),
                    },
                ],
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
                .iter()
                .cloned()
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
            if term::bool_value(&e.rhs) != Some(true)
                || self.opened.contains(&*term::canon_ref(&e.lhs))
            {
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
            env.prove_under_lines(lines, lhs, rhs, depth)
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
            // evaluate is a computation, not a shape to read, so then only
            // its call is opened, the body left as the arm states it (its
            // lines get their normal form in `prove_under_lines`). A short
            // evaluation that runs out, and the opening after it, draw on
            // `normal_fuel`, not on the claim's fuel. One that evaluates no
            // further is not tried again in this attempt.
            let held = self.hyps.remove(i);
            let fuel = self.fuel;
            let cap = fuel.min(HYPOTHESIS_FUEL);
            self.fuel = cap;
            let whole = self.whnf(&fact);
            let spent = cap - self.fuel;
            let ev = match whole {
                Ok(ev) => {
                    self.fuel = fuel - spent;
                    Some(ev.chain)
                }
                Err(_) => {
                    self.normal_fuel = self.normal_fuel.saturating_sub(spent);
                    let cap = self.normal_fuel.min(HYPOTHESIS_FUEL);
                    self.fuel = cap;
                    let opened = self.open_call_only(&fact);
                    self.normal_fuel -= cap - self.fuel;
                    self.fuel = fuel;
                    opened
                }
            };
            self.hyps.insert(i, held);
            let Some(ev) = ev.filter(|c| !c.is_empty()) else {
                self.barren.push(fact);
                continue;
            };
            let (to, chain) = ev.finish();
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
            let body = self.prove_under_lines(lines, lhs, rhs, depth);
            self.opened.pop();
            return body.map(Some);
        }
        Ok(None)
    }

    /// `t` with its head opened while it is a call of a user function:
    /// each step selects the arm the call's `match` takes and goes no
    /// further into the body, at most [`MAX_CALL_OPENINGS`] times. `None`
    /// when a step fails; an empty chain when the head does not open.
    fn open_call_only(&mut self, t: &Term) -> Option<Chain> {
        let mut chain = Chain::new(&canon(t));
        for _ in 0..MAX_CALL_OPENINGS {
            let cur = chain.cur().clone();
            if !matches!(cur.node, ResolvedExpr::Call(ResolvedCallee::Fn(_), _)) {
                break;
            }
            match self.head_step(&cur).ok()? {
                Step::Progress(step) => {
                    let (proof, to) = *step;
                    chain.push(proof, to);
                }
                Step::Done | Step::Blocked(_) => break,
            }
        }
        Some(chain)
    }

    /// `fact` evaluated to its normal form, its parts too, under the
    /// hypotheses in scope but those stating `fact` itself (which would
    /// only give their own value back), within [`NORMAL_FACT_FUEL`] steps
    /// of `normal_fuel`, never of the claim's own fuel: that form, and the
    /// proof that `fact` equals it. `None` when evaluation changes nothing
    /// or runs out of its budget.
    pub(crate) fn normal_fact(&mut self, fact: &Term) -> Option<NormalFact> {
        let fact = canon(fact);
        let saved = self.hyps.clone();
        self.hyps.retain(|(_, h)| *term::canon_ref(&h.lhs) != fact);
        // What evaluation records on the way (definitions opened, laws
        // applied, hints) is taken back here and kept with the form, so a
        // form the proof does not read leaves the script as it was.
        let (laws, defs, consts) = (self.laws.len(), self.used.len(), self.used_consts.len());
        let (hints, open_premises) = (self.hints.len(), self.open_premises.len());
        let (opened, barren) = (self.opened.clone(), self.barren.clone());
        let fuel = self.fuel;
        let budget = self.normal_fuel.min(NORMAL_FACT_FUEL);
        self.fuel = budget;
        let out = self.normalize(&fact, 8);
        self.normal_fuel -= budget - self.fuel;
        self.fuel = fuel;
        self.hyps = saved;
        let (laws, defs, consts) = (
            self.laws.split_off(laws),
            self.used.split_off(defs),
            self.used_consts.split_off(consts),
        );
        self.hints.truncate(hints);
        self.open_premises.truncate(open_premises);
        self.opened = opened;
        self.barren = barren;
        let (to, proof) = out.ok()?.finish();
        let to = canon(&to);
        (to != fact).then_some(NormalFact {
            to,
            proof,
            laws,
            defs,
            consts,
        })
    }

    /// Record what evaluating a hypothesis to its normal form needed, once
    /// the proof reads that form.
    pub(crate) fn keep_needs(&mut self, needs: &NormalFact) {
        for law in &needs.laws {
            self.use_law(law.clone());
        }
        for id in &needs.defs {
            self.mark_used(*id);
        }
        for name in &needs.consts {
            self.mark_const_used(name);
        }
    }

    /// `lhs = rhs` by evaluation one level down, with `lines` (each with
    /// its proof that it is true) cut in as hypotheses. Each line is also
    /// stated in its normal form under the hypotheses in scope
    /// ([`Self::normal_fact`]): a line is stated about the terms as they
    /// stood before the case split and the evaluation that brought the
    /// claim here, so as written it may no longer match what the claim
    /// has become. A line whose normal form is `false` closes the case; one
    /// whose normal form is `true` adds nothing. The normal forms are cut
    /// in only where the proof reads them.
    fn prove_under_lines(
        &mut self,
        lines: Vec<(Term, Proof)>,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Proof, String> {
        let mut normal: Vec<(Term, Proof, usize)> = Vec::new();
        let mut needs: Vec<NormalFact> = Vec::new();
        for (line, proof) in &lines {
            let Some(found) = self.normal_fact(line) else {
                continue;
            };
            // `to = true`, from the evaluation and the line.
            let from = Proof::Trans {
                terms: vec![found.to.clone(), line.clone(), term::boolean(true)],
                steps: vec![Proof::Symm(Box::new(found.proof.clone())), proof.clone()],
            };
            let mut parts = Vec::new();
            super::split_and(&found.to, from, &mut parts);
            for (part, from) in parts {
                match term::bool_value(&part) {
                    Some(false) => {
                        self.keep_needs(&found);
                        return Ok(Proof::Absurd {
                            contradiction: Box::new(from),
                            lhs: canon(lhs),
                            rhs: canon(rhs),
                        });
                    }
                    Some(true) => {}
                    None => {
                        let stated = lines.iter().any(|(l, _)| *l == part)
                            || normal.iter().any(|(l, _, _)| *l == part);
                        if !stated {
                            normal.push((part, from, needs.len()));
                        }
                    }
                }
            }
            needs.push(found);
        }
        let saved = self.hyps.len();
        let mut cuts = Vec::new();
        for (line, proof) in lines {
            let h = self.fresh_hyp();
            self.hyps
                .push((h.clone(), Eqn::new(line.clone(), term::boolean(true))));
            cuts.push((h, line, proof, None));
        }
        for (line, proof, need) in normal {
            let h = self.fresh_hyp();
            self.hyps
                .push((h.clone(), Eqn::new(line.clone(), term::boolean(true))));
            cuts.push((h, line, proof, Some(need)));
        }
        let body = self.prove_by_evaluation(lhs, rhs, depth - 1);
        self.hyps.truncate(saved);
        let mut proof = body?;
        for (h, line, from, need) in cuts.into_iter().rev() {
            if let Some(k) = need {
                if !super::uses_hyp(&proof, &h) {
                    continue;
                }
                self.keep_needs(&needs[k]);
            }
            proof = Proof::Have {
                name: h,
                fact: line,
                proof: Box::new(from),
                body: Box::new(proof),
            };
        }
        Ok(proof)
    }

    /// The calls inside the Int comparisons the hypotheses decide, inner
    /// ones first, each once: calls of a definition that does not recurse
    /// and whose body is a `match`, not opened on the way here.
    fn comparison_calls(&mut self) -> Vec<Term> {
        fn calls(t: &Term, out: &mut Vec<Term>) {
            for c in term::children(t) {
                calls(c, out);
            }
            if matches!(t.node, ResolvedExpr::Call(ResolvedCallee::Fn(_), _)) && is_int(t) {
                let t = canon(t);
                if !out.contains(&t) {
                    out.push(t);
                }
            }
        }
        let mut found = Vec::new();
        for (_, e) in &self.hyps {
            if term::bool_value(&e.rhs).is_none() {
                continue;
            }
            if let ResolvedExpr::BinOp(
                BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Eq | BinOp::Neq,
                a,
                b,
            ) = &e.lhs.node
                && (is_int(a) || is_int(b))
            {
                calls(a, &mut found);
                calls(b, &mut found);
            }
        }
        found.retain(|c| {
            let ResolvedExpr::Call(ResolvedCallee::Fn(id), _) = &c.node else {
                return false;
            };
            !self.inputs.recursive_fns.contains(id) && !self.opened.contains(c)
        });
        found
            .into_iter()
            .filter(|c| {
                let ResolvedExpr::Call(ResolvedCallee::Fn(id), _) = &c.node else {
                    return false;
                };
                self.def(*id)
                    .is_some_and(|d| matches!(d.body.node, ResolvedExpr::Match { .. }))
            })
            .collect()
    }

    /// `lhs = rhs` once the calls inside the Int comparisons in scope (see
    /// [`Self::comparison_calls`]) are opened: a call evaluation takes a
    /// step on (its arguments literals, or what its `match` reads decided
    /// by a hypothesis) is cut in as `(call == value) = true`; one stopped
    /// at an undecided Bool is split on that Bool first; one stopped at a
    /// subject of a sum or list type, or an Int with literal arms, is split
    /// into the cases its `match` reads. The cases are the `match`'s own,
    /// so nothing is guessed. `None` when no such call opens.
    fn open_comparison_calls(
        &mut self,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Option<Proof>, String> {
        let calls = self.comparison_calls();
        if calls.is_empty() {
            return Ok(None);
        }
        let saved = self.opened.len();
        let out = self.open_calls(&calls, 0, false, lhs, rhs, depth);
        self.opened.truncate(saved);
        out
    }

    /// [`Self::open_comparison_calls`] from the `i`-th call on; `any` when
    /// an earlier call was cut in.
    fn open_calls(
        &mut self,
        calls: &[Term],
        i: usize,
        any: bool,
        lhs: &Term,
        rhs: &Term,
        depth: usize,
    ) -> Result<Option<Proof>, String> {
        let Some(c) = calls.get(i) else {
            if !any {
                return Ok(None);
            }
            return self.prove_by_evaluation(lhs, rhs, depth - 1).map(Some);
        };
        if self.opened.contains(c) {
            return self.open_calls(calls, i + 1, any, lhs, rhs, depth);
        }
        // A short evaluation, as for a hypothesis that calls a function.
        let fuel = self.fuel;
        self.fuel = fuel.min(HYPOTHESIS_FUEL);
        let ev = self.whnf(c);
        self.fuel = fuel - (fuel.min(HYPOTHESIS_FUEL) - self.fuel);
        let Ok(ev) = ev else {
            return self.open_calls(calls, i + 1, any, lhs, rhs, depth);
        };
        if !ev.chain.is_empty() {
            let (to, chain) = ev.chain.finish();
            let to = canon(&to);
            // `(c == to) = true`: congruence to `to == to`.
            let fact = canon(&term::binop(BinOp::Eq, c.clone(), to.clone()));
            let same = canon(&term::binop(BinOp::Eq, to.clone(), to.clone()));
            let proof = Proof::Trans {
                terms: vec![fact.clone(), same, term::boolean(true)],
                steps: vec![
                    Proof::Congr {
                        ctx: term::binop(BinOp::Eq, term::hole(), to.clone()),
                        inner: Box::new(chain),
                    },
                    Proof::Rule {
                        rule: WallRule::BeqRefl,
                        subst: vec![("a".into(), to)],
                        premises: Vec::new(),
                    },
                ],
            };
            let name = self.fresh_hyp();
            self.hyps
                .push((name.clone(), Eqn::new(fact.clone(), term::boolean(true))));
            self.opened.push(c.clone());
            let body = self.open_calls(calls, i + 1, true, lhs, rhs, depth);
            self.opened.pop();
            self.hyps.pop();
            return Ok(body?.map(|body| Proof::Have {
                name,
                fact,
                proof: Box::new(proof),
                body: Box::new(body),
            }));
        }
        if let Some(g) = ev.blocked.map(|g| canon(&g))
            && !is_bool_value(&g)
            && self.hyp_for(&g).is_none()
        {
            let hyp = self.fresh_hyp();
            let branch = |env: &mut Self, v: bool| -> Result<Option<Proof>, String> {
                env.hyps
                    .push((hyp.clone(), Eqn::new(g.clone(), term::boolean(v))));
                let out = env.open_calls(calls, i, any, lhs, rhs, depth);
                env.hyps.pop();
                out
            };
            let (Some(if_true), Some(if_false)) = (branch(self, true)?, branch(self, false)?)
            else {
                return Ok(None);
            };
            return Ok(Some(Proof::Cases {
                on: g,
                hyp,
                if_true: Box::new(if_true),
                if_false: Box::new(if_false),
            }));
        }
        if let Some(st) = self.stuck_subject(c, false) {
            return self.split_on(st, lhs, rhs, depth).map(Some);
        }
        self.open_calls(calls, i + 1, any, lhs, rhs, depth)
    }

    /// The refusal for two sides evaluation cannot bring together: both
    /// sides as it left them, and the hypotheses in scope.
    pub(crate) fn stopped_at(&mut self, lhs: &Term, rhs: &Term) -> String {
        use crate::ir::proof_steps::show;
        self.note_fact_hints(&[lhs, rhs]);
        if self.stops.len() < super::env::MAX_STOPS {
            self.stops.push(super::env::Stop {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                hyps: self.hyps.iter().map(|(_, e)| e.clone()).collect(),
            });
        }
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
    /// Each builtin fact, then each earlier law, the law does not cite
    /// whose left side matches a part of `terms` or of a hypothesis in
    /// scope (a subject split on, a `holds` comparison decided by cases),
    /// one match each: pattern matching on the term as it stands, never a
    /// rewrite followed by another match. A match where the law's `when`
    /// computes to `false`, or a hypothesis states it false, is no match.
    /// What is noted is the law's key: a candidate for the `using` line,
    /// hinted only once the law closes with it ([`super::using_hint`]).
    pub(crate) fn note_fact_hints(&mut self, terms: &[&Term]) {
        use crate::ir::proof_steps::facts;
        fn parts<'t>(t: &'t Term, out: &mut Vec<&'t Term>) {
            out.push(t);
            for c in term::children(t) {
                parts(c, out);
            }
        }
        let hyps: Vec<Term> = self.hyps.iter().map(|(_, e)| e.lhs.clone()).collect();
        let mut all = Vec::new();
        for t in terms.iter().copied().chain(&hyps) {
            parts(t, &mut all);
        }
        let candidates: Vec<crate::ir::proof_steps::LawRef> = facts::all()
            .iter()
            .map(facts::Fact::law_ref)
            .chain(self.hint_laws.iter().cloned())
            .filter(|l| !self.cited_all.iter().any(|c| c.key == l.key))
            .collect();
        for law in candidates {
            let celled = as_cells(&law.lhs).map(|(_, cells)| cells);
            let at = |p: &Term| -> Option<Vec<(String, Term)>> {
                [Some(&law.lhs), celled.as_ref()]
                    .into_iter()
                    .flatten()
                    .find_map(|pat| {
                        let mut found = Vec::new();
                        super::rewrite::matches(pat, p, &law.givens, &mut found).then_some(found)
                    })
            };
            let found = all.iter().find(|p| {
                at(p).is_some_and(|subst| {
                    let Some(when) = &law.premise else {
                        return true;
                    };
                    let Ok(when) = term::subst(when, &subst) else {
                        return true;
                    };
                    let when = canon(&when);
                    let said = term::eval_closed(&when)
                        .and_then(|v| term::bool_value(&v))
                        .or_else(|| self.hyp_for(&when).and_then(|(_, v)| term::bool_value(&v)));
                    said != Some(false)
                })
            });
            if found.is_some() && !self.hints.contains(&law.key) {
                self.hints.push(law.key);
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
            types: Vec::new(),
            finite: Vec::new(),
            lists: Vec::new(),
            ints: Vec::new(),
            premise: None,
            refined: Vec::new(),
            lhs: term::boolean(true),
            rhs: term::boolean(true),
        },
        defs: Vec::new(),
        consts: Vec::new(),
        laws: Vec::new(),
        decls: Default::default(),
        sigs: Vec::new(),
        plan: None,
        proof: Proof::Refl(term::boolean(true)),
    }
}
