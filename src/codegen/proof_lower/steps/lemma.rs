//! A hint for a law the steps leave open for want of a lemma: the text of a
//! law, stated from where they stopped, that the steps close, and under
//! which they close the open law.
//!
//! Where evaluation stops, the two sides it could not bring together often
//! say what is missing: `plus(y, Nat.Z)` against `y` in the first case of an
//! induction, `length(List.concat(y, List.prepend(h, t)))` against
//! `Nat.S(length(List.concat(y, t)))` in the second. Stated as a law, with the
//! names in it as givens, such an equation is a lemma the steps may prove by
//! an induction of its own. Which equation is true cannot be read off the
//! stop, so the steps never assume one: they state a few candidates, check
//! them, and only offer the one that passes every check as a hint. The law
//! stays open until its author adds what the hint says.
//!
//! The candidates from one stop, few and of a fixed shape:
//! - the two sides as an equation, with the side that is a call on the left
//!   (either way round when both are);
//! - where both sides are the same call or constructor and differ in one
//!   argument only, those two arguments, the same way;
//! - each of these with a part both sides share (a constant such as `0` or
//!   `[]`, or a term over the names in it) made a given of its own, so the
//!   lemma speaks of any value there, as an accumulator's law does;
//! - where the stop is a case the hypotheses should rule out (two different
//!   values), each hypothesis that a call is `true` or `false` stated the
//!   other way round: the lemma that rules the case out.
//!
//! A candidate is offered only when it holds on samples (the values the
//! module's own laws give to a given of each type), the steps close it as
//! they would once it is in the text (citing only laws they closed), and
//! the steps then close the open law citing it. When the open law still
//! stops with one lemma added, where it stops then gives the candidates for
//! a second, [`MAX_LEMMAS`] at most. Every proof is on a short budget first
//! and the hint is given only when the whole budget closes it too.

use crate::ast::{TopLevel, Type, VerifyGiven, VerifyGivenDomain, VerifyKind};
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::hir::{ResolvedCallee, ResolvedCtor, ResolvedExpr};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::sexpr::Names;
use crate::ir::proof_steps::term::{self, Term, canon};
use crate::ir::proof_steps::{Script, show};
use crate::ir::{LawTheorem, ProofIR, ProofStrategy, Quantifier, QuantifierType};

use super::Found;
use super::env::{self, Env, Stop};

/// Most candidates the steps try to prove for one lemma.
const MAX_CANDIDATES: usize = 6;
/// Most lemmas one hint adds.
const MAX_LEMMAS: usize = 2;
/// Most parts of a candidate made a given, each its own candidate.
const MAX_GENERALIZED: usize = 3;
/// Most sample cases a candidate is checked on.
const MAX_SAMPLE_CASES: usize = 12;
/// Most nodes a side of a candidate has: a lemma is a small equation.
const MAX_SIDE_NODES: usize = 30;
/// What each attempt on a candidate, and on the law citing it, may spend
/// on the first try: an eighth of [`env::FUEL`], as for a `using` hint.
const LEMMA_FUEL: usize = env::FUEL / 8;

/// Whether the hint is looked for: under `aver proof`, where hints are
/// shown.
pub(super) fn enabled(inputs: &ProofLowerInputs) -> bool {
    inputs.proof_command
}

/// The module a law is stated in, as its source reads: its functions in
/// definition order and the givens its laws write.
struct Module<'a> {
    scope: Option<String>,
    fns: Vec<String>,
    givens: Vec<&'a VerifyGiven>,
    /// The laws stated about each function, by name.
    laws: Vec<(String, String)>,
}

fn module<'a>(inputs: &'a ProofLowerInputs, scope: Option<&str>) -> Option<Module<'a>> {
    let mut out = Module {
        scope: scope.map(String::from),
        fns: Vec::new(),
        givens: Vec::new(),
        laws: Vec::new(),
    };
    let verify = |vb: &'a crate::ast::VerifyBlock, out: &mut Module<'a>| {
        if let VerifyKind::Law(law) = &vb.kind {
            out.givens.extend(law.givens.iter());
            out.laws.push((vb.fn_name.clone(), law.name.clone()));
        }
    };
    match scope {
        None => {
            for item in inputs.entry_items {
                match item {
                    TopLevel::FnDef(f) => out.fns.push(f.name.clone()),
                    TopLevel::Verify(vb) => verify(vb, &mut out),
                    _ => {}
                }
            }
        }
        Some(prefix) => {
            let m = inputs.dep_modules.iter().find(|m| m.prefix == prefix)?;
            out.fns = m.fn_defs.iter().map(|f| f.name.clone()).collect();
            for vb in &m.verify_blocks {
                verify(vb, &mut out);
            }
        }
    }
    Some(out)
}

/// Names as the module's own source writes them: its functions and types
/// without the module's prefix.
struct Local<'a> {
    names: &'a crate::ir::SymbolTable,
    prefix: Option<String>,
}

impl Local<'_> {
    fn strip(&self, name: String) -> String {
        match &self.prefix {
            Some(p) => match name.strip_prefix(&format!("{p}.")) {
                Some(rest) => rest.to_string(),
                None => name,
            },
            None => name,
        }
    }
}

impl Names for Local<'_> {
    fn fn_name(&self, id: FnId) -> String {
        self.strip(self.names.fn_name(id))
    }
    fn ctor_name(&self, ctor: &ResolvedCtor) -> String {
        self.strip(self.names.ctor_name(ctor))
    }
    fn type_name(&self, ty: &Type) -> String {
        self.strip(self.names.type_name(ty))
    }
}

/// A candidate lemma: `lhs => rhs` over the givens `vars`, each with its
/// type.
#[derive(Clone)]
struct Candidate {
    lhs: Term,
    rhs: Term,
    vars: Vec<(String, Type)>,
}

/// A candidate the steps closed, with the name it takes and what it is
/// stated about.
#[derive(Clone)]
struct Lemma {
    cand: Candidate,
    fn_id: FnId,
    name: String,
    /// The type of each given, as its `given` line writes it.
    types: Vec<String>,
    /// The earlier laws its proof applies, for its `using` line.
    pinned: Vec<String>,
    script: Script,
}

fn is_var(t: &Term) -> Option<&str> {
    match &t.node {
        ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => Some(n),
        _ => None,
    }
}

/// Whether `t` may stand on the left of a lemma: a call.
fn is_call(t: &Term) -> bool {
    matches!(
        &t.node,
        ResolvedExpr::Call(ResolvedCallee::Fn(_) | ResolvedCallee::Builtin(_), _)
    )
}

/// A value: a literal, the empty list, or a constructor without arguments.
fn is_constant(t: &Term) -> bool {
    term::is_literal(t)
        || matches!(&t.node, ResolvedExpr::List(xs) if xs.is_empty())
        || matches!(&t.node, ResolvedExpr::Ctor(_, xs) if xs.is_empty())
}

fn vars_of(t: &Term) -> Vec<String> {
    let mut out = Vec::new();
    term::free_vars(t, &mut out);
    out
}

/// The type of each name in `t`, as its terms carry it.
fn var_types(t: &Term, out: &mut Vec<(String, Type)>) {
    if let Some(n) = is_var(t) {
        if !out.iter().any(|(m, _)| m == n)
            && let Some(ty) = t.ty()
        {
            out.push((n.to_string(), ty.clone()));
        }
        return;
    }
    for c in term::children(t) {
        var_types(c, out);
    }
}

/// The type of each name in `t` that its terms leave untyped, read off
/// where it stands (`expected` is the type its place has, when known): an
/// argument of a user function has the parameter's type, the parts of a
/// list built with `List.prepend` or `List.concat` the list's, an operand
/// of arithmetic `Int`.
fn infer_types(env: &Env, t: &Term, expected: Option<Type>, out: &mut Vec<(String, Type)>) {
    let here = t.ty().cloned().or(expected);
    if let Some(n) = is_var(t) {
        if let Some(ty) = here
            && !out.iter().any(|(m, _)| m == n)
        {
            out.push((n.to_string(), ty));
        }
        return;
    }
    let mut each = |c: &Term, ty: Option<Type>| infer_types(env, c, ty, out);
    match &t.node {
        ResolvedExpr::Call(ResolvedCallee::Fn(f), args) => {
            let params = env.raw_def(*f).map(|d| d.param_types).unwrap_or_default();
            for (k, a) in args.iter().enumerate() {
                each(a, params.get(k).cloned());
            }
        }
        ResolvedExpr::Call(ResolvedCallee::Builtin(b), args) => {
            let list = here.filter(|ty| matches!(ty, Type::List(_)));
            let elem = match &list {
                Some(Type::List(e)) => Some((**e).clone()),
                _ => None,
            };
            match (b.as_str(), args.as_slice()) {
                ("List.prepend", [h, tail]) => {
                    each(h, elem);
                    each(tail, list);
                }
                ("List.concat", [a, b]) => {
                    each(a, list.clone());
                    each(b, list);
                }
                _ => args.iter().for_each(|a| each(a, None)),
            }
        }
        ResolvedExpr::BinOp(op, a, b)
            if matches!(
                op,
                crate::ast::BinOp::Add | crate::ast::BinOp::Sub | crate::ast::BinOp::Mul
            ) && here == Some(Type::Int) =>
        {
            each(a, Some(Type::Int));
            each(b, Some(Type::Int));
        }
        _ => term::children(t).into_iter().for_each(|c| each(c, None)),
    }
}

/// Every occurrence of `part` in `t` replaced by `with`.
fn replace(t: &Term, part: &Term, with: &Term) -> Term {
    if *term::canon_ref(t) == *part {
        return with.clone();
    }
    term::map_children(t, &mut |c| Ok(replace(c, part, with))).expect("replace is total")
}

fn subterms<'t>(t: &'t Term, out: &mut Vec<&'t Term>) {
    out.push(t);
    for c in term::children(t) {
        subterms(c, out);
    }
}

/// `t` with its names renamed in order of first occurrence, for telling
/// candidates apart up to the names of their givens.
fn shape_key(lhs: &Term, rhs: &Term) -> String {
    let mut names = vars_of(lhs);
    for n in vars_of(rhs) {
        if !names.contains(&n) {
            names.push(n);
        }
    }
    let map: Vec<(String, Term)> = names
        .iter()
        .enumerate()
        .map(|(k, n)| (n.clone(), term::var(&format!("v{k}"))))
        .collect();
    let show = |t: &Term| {
        term::subst(t, &map)
            .map(|t| show::term(&t, &crate::ir::proof_steps::sexpr::BuiltinsOnly))
            .unwrap_or_default()
    };
    format!("{} => {}", show(lhs), show(rhs))
}

/// The candidates one stop gives, before any check (see the module
/// comment).
fn from_stop(stop: &Stop, out: &mut Vec<(Term, Term)>) {
    let (l, r) = (canon(&stop.lhs), canon(&stop.rhs));
    if l == r {
        return;
    }
    let differ = |a: &Term, b: &Term| match (term::bool_value(a), term::bool_value(b)) {
        (Some(x), Some(y)) => x != y,
        _ => is_constant(a) && is_constant(b),
    };
    if differ(&l, &r) {
        for h in &stop.hyps {
            if let Some(v) = term::bool_value(&h.rhs)
                && is_call(&h.lhs)
            {
                out.push((canon(&h.lhs), term::boolean(!v)));
            }
        }
        return;
    }
    let mut pairs = vec![(l.clone(), r.clone())];
    // The one argument where the same call or constructor differs.
    let (mut a, mut b) = (l, r);
    loop {
        let same_head = match (&a.node, &b.node) {
            (ResolvedExpr::Call(f, xs), ResolvedExpr::Call(g, ys)) => {
                f == g && xs.len() == ys.len()
            }
            (ResolvedExpr::Ctor(f, xs), ResolvedExpr::Ctor(g, ys)) => {
                f == g && xs.len() == ys.len()
            }
            _ => false,
        };
        if !same_head {
            break;
        }
        let (xs, ys) = (term::children(&a), term::children(&b));
        let differing: Vec<usize> = (0..xs.len())
            .filter(|k| !term::canon_eq(xs[*k], ys[*k]))
            .collect();
        let [k] = differing.as_slice() else {
            break;
        };
        let (x, y) = (canon(xs[*k]), canon(ys[*k]));
        pairs.push((x.clone(), y.clone()));
        (a, b) = (x, y);
    }
    for (x, y) in pairs {
        out.push((x.clone(), y.clone()));
        out.push((y, x));
    }
}

/// `lhs => rhs` with each part both sides share made a given of its own,
/// one part at a time: the variants after the candidate itself.
fn generalized(lhs: &Term, rhs: &Term, taken: &[String]) -> Vec<(Term, Term)> {
    let mut parts = Vec::new();
    subterms(lhs, &mut parts);
    let mut seen: Vec<Term> = Vec::new();
    let mut out = Vec::new();
    for p in parts {
        let p = canon(p);
        if p == *term::canon_ref(lhs) || is_var(&p).is_some() || term::bool_value(&p).is_some() {
            continue;
        }
        if seen.contains(&p) {
            continue;
        }
        seen.push(p.clone());
        let mut inside = Vec::new();
        subterms(rhs, &mut inside);
        if !inside.iter().any(|q| *term::canon_ref(q) == p) {
            continue;
        }
        let Some(ty) = p.ty().cloned() else {
            continue;
        };
        let fresh = fresh_name("acc", taken);
        let v = term::var(&fresh);
        v.set_ty(ty);
        out.push((replace(lhs, &p, &v), replace(rhs, &p, &v)));
        if out.len() == MAX_GENERALIZED {
            break;
        }
    }
    out
}

fn fresh_name(base: &str, taken: &[String]) -> String {
    (0..)
        .map(|n| match n {
            0 => base.to_string(),
            n => format!("{base}{n}"),
        })
        .find(|n| !taken.contains(n))
        .expect("an unused name")
}

/// The candidate `lhs => rhs` as a lemma may state it: a call on the left,
/// every name on the right also on the left, each name typed, and none of
/// the names a step term gives a module's binding or a hole. The names the
/// induction gave its cases (`x2_c0`) are written as a law would name them.
fn candidate(env: &Env, lhs: &Term, rhs: &Term, avoid: &[String]) -> Option<Candidate> {
    if !is_call(lhs) || term::canon_eq(lhs, rhs) {
        return None;
    }
    let size = |t: &Term| {
        let mut all = Vec::new();
        subterms(t, &mut all);
        all.len()
    };
    if size(lhs) > MAX_SIDE_NODES || size(rhs) > MAX_SIDE_NODES {
        return None;
    }
    // A rewrite rule that does not grow the term and leaves nothing it
    // applies to again, so rewriting with it stops.
    if size(rhs) > size(lhs) {
        return None;
    }
    let left_vars = vars_of(lhs);
    let mut inside = Vec::new();
    subterms(rhs, &mut inside);
    if inside
        .iter()
        .any(|p| super::rewrite::matches(lhs, p, &left_vars, &mut Vec::new()))
    {
        return None;
    }
    let left = vars_of(lhs);
    let right = vars_of(rhs);
    if right.iter().any(|n| !left.contains(n)) {
        return None;
    }
    if left
        .iter()
        .any(|n| n.starts_with('.') || n.contains(term::HOLE))
    {
        return None;
    }
    let mut types = Vec::new();
    var_types(lhs, &mut types);
    var_types(rhs, &mut types);
    infer_types(env, lhs, None, &mut types);
    infer_types(env, rhs, None, &mut types);
    types.retain(|(n, _)| left.contains(n));
    types.sort_by_key(|(n, _)| left.iter().position(|m| m == n));
    if types.len() != left.len() {
        return None;
    }
    // Readable, distinct names.
    let mut taken: Vec<String> = avoid.to_vec();
    let mut map = Vec::new();
    let mut vars = Vec::new();
    for (n, ty) in types {
        let base = match n.split_once("_c") {
            Some((b, digits)) if !b.is_empty() && digits.chars().all(|c| c.is_ascii_digit()) => b,
            _ => n.as_str(),
        };
        let base: String = base.chars().filter(|c| c.is_ascii_alphanumeric()).collect();
        let base = if base.is_empty() {
            "x".to_string()
        } else {
            base
        };
        let name = fresh_name(&base, &taken);
        taken.push(name.clone());
        let v = term::var(&name);
        v.set_ty(ty.clone());
        map.push((n, v));
        vars.push((name, ty));
    }
    Some(Candidate {
        lhs: canon(&term::subst(lhs, &map).ok()?),
        rhs: canon(&term::subst(rhs, &map).ok()?),
        vars,
    })
}

/// The sample values for a given of type `ty`: those a law of the module
/// writes for a given of that type, else a few for `Int`, `Bool` and
/// `List<Int>`; each as the law would write its domain and as terms.
fn samples(
    inputs: &ProofLowerInputs,
    m: &Module,
    ty: &Type,
) -> Option<(String, String, Vec<Term>)> {
    let scope = m.scope.as_deref();
    let resolve = |e: &crate::ast::Spanned<crate::ast::Expr>| {
        env::qualify_bindings(&canon(&inputs.resolve_expr(e, scope)), inputs, scope, &[])
    };
    for g in &m.givens {
        let gty =
            super::finite::qualify(inputs, &crate::types::parse_type_str(&g.type_name), scope);
        if gty != *ty && gty.display() != ty.display() {
            continue;
        }
        let (text, values) = match &g.domain {
            VerifyGivenDomain::IntRange { start, end } => (
                format!("{start}..{end}"),
                (*start..=*end)
                    .take(MAX_SAMPLE_CASES)
                    .map(|v| term::int(&v.into()))
                    .collect::<Vec<_>>(),
            ),
            VerifyGivenDomain::Explicit(values) => (
                format!(
                    "[{}]",
                    values
                        .iter()
                        .map(crate::checker::expr_to_str)
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                values.iter().map(resolve).collect(),
            ),
        };
        if values.len() >= 2 {
            return Some((g.type_name.clone(), text, values));
        }
    }
    let ints = |vs: &[i64]| {
        vs.iter()
            .map(|v| term::int(&(*v).into()))
            .collect::<Vec<_>>()
    };
    match ty {
        Type::Int => Some((
            "Int".into(),
            "[-1, 0, 1, 2, 5]".into(),
            ints(&[-1, 0, 1, 2, 5]),
        )),
        Type::Bool => Some((
            "Bool".into(),
            "[true, false]".into(),
            vec![term::boolean(true), term::boolean(false)],
        )),
        Type::List(inner) if **inner == Type::Int => Some((
            "List<Int>".into(),
            "[[], [1], [2, 3]]".into(),
            vec![
                term::nil(),
                term::list(ints(&[1])),
                term::list(ints(&[2, 3])),
            ],
        )),
        _ => None,
    }
}

/// Whether `cand` holds on the samples of its givens, by evaluation: every
/// case evaluates both sides to the same value.
fn holds_on_samples(inputs: &ProofLowerInputs, cand: &Candidate, values: &[Vec<Term>]) -> bool {
    let total: usize = values.iter().map(Vec::len).product();
    let widest = values.iter().map(Vec::len).max().unwrap_or(1);
    let cases: Vec<Vec<usize>> = if total <= MAX_SAMPLE_CASES {
        (0..total)
            .map(|mut j| {
                values
                    .iter()
                    .map(|vs| {
                        let k = j % vs.len();
                        j /= vs.len();
                        k
                    })
                    .collect()
            })
            .collect()
    } else {
        // Each value of each given at least once, then mixed.
        (0..MAX_SAMPLE_CASES)
            .map(|j| {
                values
                    .iter()
                    .enumerate()
                    .map(|(k, vs)| {
                        if j < widest {
                            j % vs.len()
                        } else {
                            (j * (2 * k + 3) + k) % vs.len()
                        }
                    })
                    .collect()
            })
            .collect()
    };
    for case in cases {
        let at: Vec<(String, Term)> = cand
            .vars
            .iter()
            .zip(&case)
            .zip(values)
            .map(|(((n, _), k), vs)| (n.clone(), vs[*k].clone()))
            .collect();
        let (Ok(l), Ok(r)) = (term::subst(&cand.lhs, &at), term::subst(&cand.rhs, &at)) else {
            return false;
        };
        if Env::new(inputs).prove_by_evaluation(&l, &r, 2).is_err() {
            return false;
        }
    }
    true
}

/// The user functions `t` calls, in order of first call.
fn calls(t: &Term, out: &mut Vec<FnId>) {
    if let ResolvedExpr::Call(ResolvedCallee::Fn(f), _) = &t.node
        && !out.contains(f)
    {
        out.push(*f);
    }
    for c in term::children(t) {
        calls(c, out);
    }
}

/// Whether the source of module `own` may call function `f`: one of its
/// own, or one a module it depends on exposes.
fn callable(inputs: &ProofLowerInputs, own: Option<&str>, f: FnId) -> bool {
    let key = &inputs.symbol_table.fn_entry(f).key;
    if key.scope.as_deref() == own {
        return true;
    }
    let Some(module) = key.scope.as_deref() else {
        return false;
    };
    let depends: Option<&[String]> = match own {
        None => crate::visibility::module_decl(inputs.entry_items).map(|m| m.depends.as_slice()),
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .map(|m| m.depends.as_slice()),
    };
    if !depends.is_some_and(|d| d.iter().any(|m| m == module)) {
        return false;
    }
    inputs
        .dep_modules
        .iter()
        .find(|m| m.prefix == module)
        .is_some_and(|info| {
            let exposes = (!info.exposes.is_empty()).then_some(info.exposes.as_slice());
            crate::visibility::is_exposed(&key.name, exposes)
        })
}

/// The law `cand` as the steps prove it, about `fn_id` under `name`.
fn law_of(
    inputs: &ProofLowerInputs,
    t: &LawTheorem,
    cand: &Candidate,
    fn_id: FnId,
    name: &str,
    types: &[String],
) -> LawTheorem {
    // Each given typed as its `given` line states it.
    let scope = inputs.symbol_table.fn_entry(fn_id).key.scope_str();
    let map: Vec<(String, Term)> = cand
        .vars
        .iter()
        .zip(types)
        .map(|((n, _), ty)| {
            let v = term::var(n);
            v.set_ty(super::finite::qualify(
                inputs,
                &crate::types::parse_type_str(ty),
                scope,
            ));
            (n.clone(), v)
        })
        .collect();
    let typed = |side: &Term| term::subst(side, &map).map_or_else(|_| side.clone(), |s| canon(&s));
    LawTheorem {
        fn_id,
        law_name: name.to_string(),
        quantifiers: cand
            .vars
            .iter()
            .zip(types)
            .map(|((n, _), ty)| Quantifier {
                name: n.clone(),
                binder_type: QuantifierType::Plain(ty.clone()),
            })
            .collect(),
        premises: Vec::new(),
        claim_lhs: typed(&cand.lhs),
        claim_rhs: typed(&cand.rhs),
        strategy: ProofStrategy::Reflexive,
        induction: None,
        reasons: Vec::new(),
        reason_inductions: Vec::new(),
        using: None,
        induction_given: None,
        by_plan: None,
        steps: None,
        steps_refusal: None,
        steps_hints: Vec::new(),
        steps_auto_using: Vec::new(),
        obligation_steps: Vec::new(),
        steps_timing: Default::default(),
        ..t.clone()
    }
}

/// Law `k` of `probe` proved as it would be once in the text, on `fuel`:
/// on its own first, then with the earlier laws the steps closed when it
/// has no `using` line; its script and the earlier laws it applied that
/// way. A proof citing a law the steps left open does not count.
fn prove(
    inputs: &ProofLowerInputs,
    probe: &ProofIR,
    k: usize,
    fuel: usize,
    found: &mut Found,
) -> Option<(Script, Vec<String>)> {
    let law = &probe.law_theorems[k];
    let earlier = &probe.law_theorems[..k];
    let closed = |s: &Script| {
        s.laws
            .iter()
            .filter(|l| l.fact.is_none())
            .all(|l| super::closed_by_steps(inputs, earlier, &l.key))
    };
    if let Ok(s) = super::produce_as(inputs, probe, k, law, &[], fuel, found).law
        && closed(&s)
    {
        return Some((s, Vec::new()));
    }
    if law.using.is_some() {
        return None;
    }
    let pool = super::auto_pool(inputs, probe, k);
    if pool.is_empty() {
        return None;
    }
    let s = super::produce_as(inputs, probe, k, law, &pool, fuel, found)
        .law
        .ok()
        .filter(closed)?;
    let used: Vec<String> = pool
        .iter()
        .filter(|l| s.laws.iter().any(|x| x.key == l.key))
        .map(|l| l.key.clone())
        .collect();
    Some((s, used))
}

/// The hint for law `i`, which the steps left open and no other hint
/// closes: the lemmas, stated from where the steps stopped (`stops`), that
/// they close, and the `using` line under which they then close law `i`.
pub(super) fn hint(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    stops: &[Stop],
) -> Option<String> {
    let t = &ir.law_theorems[i];
    let skip = |why: &str| -> Option<String> {
        if std::env::var_os("AVER_STEPS_DEBUG").is_some() {
            eprintln!("lemma: {}: no lemma looked for: {why}", t.law_name);
        }
        None
    };
    if t.by_plan.is_some() || stops.is_empty() {
        return skip("no stop");
    }
    let symbols = inputs.symbol_table;
    let own = &symbols.fn_entry(t.fn_id).key;
    let m = module(inputs, own.scope.as_deref())?;
    let Some(own_at) = m.fns.iter().position(|f| *f == own.name) else {
        return skip("its function is not in its module's source");
    };
    let earlier = &ir.law_theorems[..i];
    // A `using` line that cites an open law leaves the law open whatever is
    // added.
    let base = t.using.clone().unwrap_or_default();
    if let Some(laws) = super::cited(inputs, ir, t, &base) {
        if !laws
            .iter()
            .all(|l| l.fact.is_some() || super::closed_by_steps(inputs, earlier, &l.key))
        {
            return skip("it cites an open law");
        }
    } else {
        return skip("a cited law has no theorem");
    }
    // The givens a lemma may not take: the module's bindings.
    let avoid: Vec<String> =
        crate::codegen::proof_lower::module_bindings_of(inputs, own.scope_str())
            .into_iter()
            .map(|b| b.name)
            .collect();
    let target = shape_key(
        &super::law_term(inputs, t, &t.claim_lhs),
        &super::law_term(inputs, t, &t.claim_rhs),
    );
    let target_back = shape_key(
        &super::law_term(inputs, t, &t.claim_rhs),
        &super::law_term(inputs, t, &t.claim_lhs),
    );
    let mut lemmas: Vec<Lemma> = Vec::new();
    let mut stops: Vec<Stop> = stops.to_vec();
    let mut tried: Vec<String> = vec![target, target_back];
    let names_taken = |lemmas: &[Lemma], f: &str| -> Vec<String> {
        m.laws
            .iter()
            .filter(|(g, _)| g == f)
            .map(|(_, l)| l.clone())
            .chain(lemmas.iter().map(|l| l.name.clone()))
            .collect()
    };
    let local_name = |l: &Lemma| format!("{}.{}", symbols.fn_entry(l.fn_id).key.name, l.name);
    let scratch = Env::new(inputs);
    let mut base_probe = ir.clone();
    base_probe.law_theorems.truncate(i);
    for _round in 0..MAX_LEMMAS {
        // The candidates, in the order the stops give them, each once.
        let mut raw = Vec::new();
        for s in &stops {
            from_stop(s, &mut raw);
        }
        let mut cands: Vec<(String, Candidate)> = Vec::new();
        for (l, r) in raw {
            let mut taken = vars_of(&l);
            taken.extend(vars_of(&r));
            let variants =
                std::iter::once((l.clone(), r.clone())).chain(generalized(&l, &r, &taken));
            for (l, r) in variants {
                let Some(c) = candidate(&scratch, &l, &r, &avoid) else {
                    if std::env::var_os("AVER_STEPS_DEBUG").is_some() {
                        let mut ts = Vec::new();
                        var_types(&l, &mut ts);
                        infer_types(&scratch, &l, None, &mut ts);
                        eprintln!(
                            "lemma: no candidate `{}` => `{}` (typed {:?})",
                            show::term(&l, symbols),
                            show::term(&r, symbols),
                            ts.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>()
                        );
                    }
                    continue;
                };
                let mut fns = Vec::new();
                calls(&c.lhs, &mut fns);
                calls(&c.rhs, &mut fns);
                if !fns.iter().all(|f| callable(inputs, own.scope_str(), *f)) {
                    continue;
                }
                let key = shape_key(&c.lhs, &c.rhs);
                if tried.contains(&key) || cands.iter().any(|(k, _)| *k == key) {
                    continue;
                }
                cands.push((key, c));
            }
        }
        let debug = std::env::var_os("AVER_STEPS_DEBUG").is_some();
        if debug {
            for st in &stops {
                eprintln!(
                    "lemma: stop `{}` / `{}`",
                    show::term(&st.lhs, symbols),
                    show::term(&st.rhs, symbols)
                );
            }
        }
        let mut proved = 0;
        let mut next: Option<(Lemma, Vec<Stop>)> = None;
        for (key, cand) in cands {
            if proved == MAX_CANDIDATES {
                break;
            }
            tried.push(key);
            // (a) It holds on samples.
            let Some(per_var) = cand
                .vars
                .iter()
                .map(|(_, ty)| samples(inputs, &m, ty))
                .collect::<Option<Vec<_>>>()
            else {
                continue;
            };
            let values: Vec<Vec<Term>> = per_var.iter().map(|(_, _, v)| v.clone()).collect();
            let holds = holds_on_samples(inputs, &cand, &values);
            if debug {
                eprintln!(
                    "lemma: candidate `{} => {}` holds on samples: {holds}",
                    show::term(&cand.lhs, symbols),
                    show::term(&cand.rhs, symbols)
                );
            }
            if !holds {
                continue;
            }
            let types: Vec<String> = per_var.iter().map(|(ty, _, _)| ty.clone()).collect();
            // About a function of the module defined before law `i`'s, the
            // first its left side calls, else law `i`'s own.
            let mut fns = Vec::new();
            calls(&cand.lhs, &mut fns);
            calls(&cand.rhs, &mut fns);
            let fn_id = fns
                .into_iter()
                .find(|f| {
                    let key = &symbols.fn_entry(*f).key;
                    key.scope == own.scope
                        && m.fns
                            .iter()
                            .position(|g| *g == key.name)
                            .is_some_and(|at| at < own_at)
                })
                .unwrap_or(t.fn_id);
            let fn_name = symbols.fn_entry(fn_id).key.name.clone();
            let name = (1..)
                .map(|k| match k {
                    1 => format!("{}Lemma", t.law_name),
                    k => format!("{}Lemma{k}", t.law_name),
                })
                .find(|n| !names_taken(&lemmas, &fn_name).contains(n))
                .expect("an unused name");
            let lemma_law = law_of(inputs, t, &cand, fn_id, &name, &types);
            // (b) The steps close it, standing before law `i` after the
            // lemmas found so far.
            proved += 1;
            let mut probe = base_probe.clone();
            for l in &lemmas {
                let mut law = law_of(inputs, t, &l.cand, l.fn_id, &l.name, &l.types.clone());
                law.steps = Some(l.script.clone());
                probe.law_theorems.push(law);
            }
            probe.law_theorems.push(lemma_law);
            let at = probe.law_theorems.len() - 1;
            let Some((script, pinned)) =
                prove(inputs, &probe, at, LEMMA_FUEL, &mut Found::default())
            else {
                if debug {
                    eprintln!("lemma:   steps do not close it");
                }
                continue;
            };
            probe.law_theorems[at].steps = Some(script.clone());
            let lemma = Lemma {
                cand: cand.clone(),
                fn_id,
                name,
                pinned,
                script,
                types: types.clone(),
            };
            // (c) Law `i` closes citing the lemmas.
            let mut names: Vec<String> = lemmas.iter().map(local_name).collect();
            names.push(local_name(&lemma));
            let mut found = Found::default();
            if close_target(
                inputs, &mut probe, t, &base, &names, &m, LEMMA_FUEL, &mut found,
            )
            .is_some()
            {
                let mut all = lemmas.clone();
                all.push(lemma);
                return confirm(inputs, ir, i, &m, &base, all);
            }
            if debug {
                eprintln!("lemma:   closes; the law does not close citing it");
            }
            if next.is_none() && lemmas.len() + 1 < MAX_LEMMAS {
                next = Some((lemma, found.stops));
            }
        }
        // Where the law stops with the first lemma that closed, for the
        // next.
        let (lemma, after) = next?;
        lemmas.push(lemma);
        stops = after;
    }
    None
}

/// `lemmas` named `<law>Lemma`, `<law>Lemma2`, … in order, each name one
/// no law of its function has.
fn renamed(lemmas: Vec<Lemma>, m: &Module, t: &LawTheorem) -> Vec<Lemma> {
    let mut out: Vec<Lemma> = Vec::new();
    for l in lemmas {
        let name = (1..)
            .map(|k| match k {
                1 => format!("{}Lemma", t.law_name),
                k => format!("{}Lemma{k}", t.law_name),
            })
            .find(|n| !m.laws.iter().any(|(_, law)| law == n) && !out.iter().any(|o| o.name == *n))
            .expect("an unused name");
        out.push(Lemma { name, ..l });
    }
    out
}

/// Law `t` proved standing after the lemmas in `probe`: with the lemmas
/// `names` added to its `using` line, or, for a law without one, by the
/// automatic pass over the earlier laws, when that applies a lemma. The
/// script and the `using` line it closes with.
#[allow(clippy::too_many_arguments)]
fn close_target(
    inputs: &ProofLowerInputs,
    probe: &mut ProofIR,
    t: &LawTheorem,
    base: &[String],
    names: &[String],
    m: &Module,
    fuel: usize,
    found: &mut Found,
) -> Option<(Script, Vec<String>)> {
    let mut line = base.to_vec();
    line.extend(names.iter().cloned());
    probe.law_theorems.push(LawTheorem {
        using: Some(line.clone()),
        ..t.clone()
    });
    let k = probe.law_theorems.len() - 1;
    let out = prove(inputs, probe, k, fuel, found);
    probe.law_theorems.pop();
    if let Some((script, _)) = out {
        return Some((script, line));
    }
    if t.using.is_some() {
        return None;
    }
    probe.law_theorems.push(t.clone());
    let out = prove(inputs, probe, k, fuel, &mut Found::default());
    probe.law_theorems.pop();
    let (script, used) = out?;
    let used: Vec<String> = used.iter().map(|key| local_key(key, m)).collect();
    used.iter()
        .any(|u| names.contains(u))
        .then_some((script, used))
}

/// The hint, once law `i` and every lemma close on the whole budget, as
/// they will once in the text; with only the lemmas the proof applies.
fn confirm(
    inputs: &ProofLowerInputs,
    ir: &ProofIR,
    i: usize,
    m: &Module,
    base: &[String],
    lemmas: Vec<Lemma>,
) -> Option<String> {
    let t = &ir.law_theorems[i];
    let symbols = inputs.symbol_table;
    let own = &symbols.fn_entry(t.fn_id).key;
    let local_name = |l: &Lemma| format!("{}.{}", symbols.fn_entry(l.fn_id).key.name, l.name);
    let try_with = |lemmas: &[Lemma]| -> Option<(Vec<Lemma>, Script, Vec<String>)> {
        let mut probe = ir.clone();
        probe.law_theorems.truncate(i);
        let mut out = Vec::new();
        for l in lemmas {
            let mut law = law_of(inputs, t, &l.cand, l.fn_id, &l.name, &l.types.clone());
            probe.law_theorems.push(law.clone());
            let k = probe.law_theorems.len() - 1;
            let Some((script, pinned)) = prove(inputs, &probe, k, env::FUEL, &mut Found::default())
            else {
                if std::env::var_os("AVER_STEPS_DEBUG").is_some() {
                    eprintln!("lemma: on the whole budget, {} does not close", l.name);
                }
                return None;
            };
            law.steps = Some(script.clone());
            probe.law_theorems[k] = law;
            out.push(Lemma {
                pinned,
                script,
                ..l.clone()
            });
        }
        let names: Vec<String> = lemmas.iter().map(local_name).collect();
        let closed = close_target(
            inputs,
            &mut probe,
            t,
            base,
            &names,
            m,
            env::FUEL,
            &mut Found::default(),
        );
        if closed.is_none() && std::env::var_os("AVER_STEPS_DEBUG").is_some() {
            eprintln!("lemma: on the whole budget, the law does not close with {names:?}");
        }
        let (script, line) = closed?;
        Some((out, script, line))
    };
    let (mut lemmas, script, mut line) = try_with(&lemmas)?;
    // Only the lemmas the law's proof applies, when it closes with those.
    let keys: Vec<String> = script.laws.iter().map(|l| l.key.clone()).collect();
    let key_of = |l: &Lemma| {
        let key = &symbols.fn_entry(l.fn_id).key;
        match key.scope_str() {
            Some(scope) => format!("{scope}.{}.{}", key.name, l.name),
            None => format!("{}.{}", key.name, l.name),
        }
    };
    let used: Vec<Lemma> = lemmas
        .iter()
        .filter(|l| keys.contains(&key_of(l)))
        .cloned()
        .collect();
    let used = renamed(used, m, t);
    if !used.is_empty()
        && used.len() < lemmas.len()
        && let Some((fewer, _, fewer_line)) = try_with(&used)
    {
        lemmas = fewer;
        line = fewer_line;
    }
    let names = Local {
        names: symbols,
        prefix: m.scope.clone(),
    };
    let mut text = if lemmas.len() == 1 {
        "steps close this law with a lemma stated where they stop; add this law before it:\n\n"
            .to_string()
    } else {
        "steps close this law with lemmas stated where they stop; add these laws before it:\n\n"
            .to_string()
    };
    for l in &lemmas {
        text.push_str(&format!(
            "    verify {} law {}\n",
            symbols.fn_entry(l.fn_id).key.name,
            l.name
        ));
        for ((n, ty), ty_text) in l.cand.vars.iter().zip(l.types.clone()) {
            let (_, domain, _) = samples(inputs, m, ty)?;
            text.push_str(&format!("        given {n}: {ty_text} = {domain}\n"));
        }
        if !l.pinned.is_empty() {
            let pinned: Vec<String> = l.pinned.iter().map(|k| local_key(k, m)).collect();
            text.push_str(&format!("        using [{}]\n", pinned.join(", ")));
        }
        text.push_str(&format!(
            "        {} => {}\n\n",
            show::term(&l.cand.lhs, &names),
            show::term(&l.cand.rhs, &names)
        ));
    }
    text.push_str(&format!(
        "    and this line to `{}.{}`:\n\n        using [{}]",
        own.name,
        t.law_name,
        line.join(", ")
    ));
    Some(text)
}

/// A law's key as its module's text names it: without the module's prefix.
fn local_key(key: &str, m: &Module) -> String {
    match &m.scope {
        Some(p) => key
            .strip_prefix(&format!("{p}."))
            .map_or_else(|| key.to_string(), String::from),
        None => key.to_string(),
    }
}
