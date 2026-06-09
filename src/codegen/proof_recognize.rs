//! Backend-neutral proof-lemma recognizers.
//!
//! Pure Aver-AST walks that identify the algebraic SHAPE a law exercises and
//! return source-name structs — no backend syntax. The Dafny renderer
//! (`codegen::dafny::lemmas`) and the Lean renderer
//! (`codegen::lean::law_auto::induction`) both consume these, so a single
//! recognizer drives a proof on either backend.
use crate::ast::{Expr, FnBody, FnDef, Pattern, Spanned, Stmt, TailCallData, TypeDef, VerifyLaw};
use crate::codegen::CodegenContext;

/// A canonical Peano ADT: EXACTLY one nullary constructor and exactly one unary
/// constructor whose single field is the type itself (e.g. `type Nat { Z; S(Nat) }`).
/// Shape, NOT name — the type need not be called `Nat`; keying on the name would
/// make a host-builtin collision the (wrong) criterion. A proof backend may lift
/// such a type to the host's builtin `Nat` (`Z` ↔ `0`, `S x` ↔ `x + 1`): builtin
/// `Nat` is exactly the initial algebra of this shape, so the lift is a sound
/// isomorphism — proof reasons about the SAME algebra, just a representation the
/// kernel/solver automates (structural recursion, `omega`, `simp`). Conservative
/// by construction: a third constructor, an extra field, a non-self field type,
/// or a record-shaped field all disqualify, so the lift can only ever be total.
#[derive(Clone, Debug)]
pub(crate) struct PeanoType {
    pub type_name: String,
    pub base_ctor: String,
    pub succ_ctor: String,
}

/// Recognize the canonical Peano shape on a single type definition.
pub(crate) fn detect_canonical_peano(td: &TypeDef) -> Option<PeanoType> {
    let TypeDef::Sum { name, variants, .. } = td else {
        return None;
    };
    if variants.len() != 2 {
        return None;
    }
    let mut base: Option<String> = None;
    let mut succ: Option<String> = None;
    for v in variants {
        match v.fields.len() {
            0 => {
                if base.replace(v.name.clone()).is_some() {
                    return None; // two nullary ctors — not Peano
                }
            }
            1 if v.fields[0].trim() == name => {
                if succ.replace(v.name.clone()).is_some() {
                    return None; // two succ ctors — not Peano
                }
            }
            // extra field, non-self field type, or record-shaped field: disqualify
            _ => return None,
        }
    }
    Some(PeanoType {
        type_name: name.clone(),
        base_ctor: base?,
        succ_ctor: succ?,
    })
}

/// Collect every canonical Peano type declared in the program (entry + modules).
pub(crate) fn collect_peano_types(ctx: &CodegenContext) -> Vec<PeanoType> {
    ctx.type_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.type_defs.iter()))
        .filter_map(detect_canonical_peano)
        .collect()
}

/// Role of a constructor inside a lifted Peano type.
pub(crate) enum PeanoCtor {
    /// The nullary base — lifts to `0`.
    Zero,
    /// The unary successor — lifts to `x + 1`.
    Succ,
}

/// If `type_name` names a canonical Peano type in this program, return it.
pub(crate) fn peano_type_named(ctx: &CodegenContext, type_name: &str) -> Option<PeanoType> {
    collect_peano_types(ctx)
        .into_iter()
        .find(|p| p.type_name == type_name)
}

/// Classify a short constructor name within a (possibly Peano) type.
pub(crate) fn peano_ctor_role(
    ctx: &CodegenContext,
    type_name: &str,
    ctor_short: &str,
) -> Option<PeanoCtor> {
    let p = peano_type_named(ctx, type_name)?;
    if ctor_short == p.base_ctor {
        Some(PeanoCtor::Zero)
    } else if ctor_short == p.succ_ctor {
        Some(PeanoCtor::Succ)
    } else {
        None
    }
}

/// Does function `fd` recurse structurally on a (lifted) Peano parameter? Such a
/// function must NOT be fuel-encoded on a proof backend that lifts the type to a
/// host builtin `Nat`: the recursion is then structural on `Nat` (host `Nat.rec`)
/// and a fuel wrapper would only re-introduce the unfolding barrier the lift
/// removes. Conservative: requires a parameter of a canonical Peano type.
pub(crate) fn recurses_on_peano(fd: &FnDef, ctx: &CodegenContext) -> bool {
    let peanos = collect_peano_types(ctx);
    fd.params
        .iter()
        .any(|(_, ty)| peanos.iter().any(|p| ty.trim() == p.type_name))
}

/// Collect all function names called in an expression (top-level only).
pub(crate) fn collect_called_fns(
    expr: &Spanned<Expr>,
    out: &mut std::collections::BTreeSet<String>,
) {
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&f.node) {
                // Skip builtins — only user functions need fuel
                if !name.contains('.') {
                    out.insert(name);
                }
            }
            collect_called_fns(f, out);
            for a in args {
                collect_called_fns(a, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_called_fns(l, out);
            collect_called_fns(r, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_called_fns(subject, out);
            for arm in arms {
                collect_called_fns(&arm.body, out);
            }
        }
        Expr::ErrorProp(inner) => collect_called_fns(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_called_fns(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_called_fns(e, out);
            }
        }
        Expr::List(elems) => {
            for e in elems {
                collect_called_fns(e, out);
            }
        }
        Expr::TailCall(tc) => {
            let TailCallData { target, args, .. } = tc.as_ref();
            if !target.contains('.') {
                out.insert(target.clone());
            }
            for a in args {
                collect_called_fns(a, out);
            }
        }
        Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_called_fns(e, out);
            }
        }
        Expr::Attr(obj, _) => collect_called_fns(obj, out),
        Expr::Neg(inner) => collect_called_fns(inner, out),
        _ => {}
    }
}

pub(crate) fn collect_called_fns_in_body(
    body: &FnBody,
    out: &mut std::collections::BTreeSet<String>,
) {
    match body {
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) => collect_called_fns(expr, out),
                    Stmt::Expr(expr) => collect_called_fns(expr, out),
                }
            }
        }
    }
}

pub(crate) fn short_ctor(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// Recognize a recursive left cons-append `fn A(p0, p1) = match p0 { [] -> p1;
/// [h, ..t] -> List.concat([h], A(t, p1)) }` — the canonical `++`. Drives the
/// rev anti-homomorphism's append-associativity / nil-right helper lemmas.
fn is_recursive_left_append(fd: &FnDef, _ctx: &CodegenContext) -> bool {
    if fd.params.len() != 2 {
        return false;
    }
    let p0 = fd.params[0].0.as_str();
    let p1 = fd.params[1].0.as_str();
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let ln = crate::codegen::recursion::detect::local_name_of;
    let Some(tail) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return false;
    };
    if ln(subject) != Some(p0) || arms.len() != 2 {
        return false;
    }
    let mut nil_ok = false;
    let mut cons_ok = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => nil_ok = ln(&arm.body) == Some(p1),
            Pattern::Cons(h, t) => {
                if let Expr::FnCall(callee, args) = &arm.body.node
                    && dotted(callee).as_deref() == Some("List.concat")
                    && args.len() == 2
                    && matches!(&args[0].node, Expr::List(es) if es.len() == 1 && ln(&es[0]) == Some(h.as_str()))
                    && let Expr::FnCall(rc, ra) = &args[1].node
                    && dotted(rc).as_deref().map(short_ctor) == Some(fd.name.as_str())
                    && ra.len() == 2
                    && ln(&ra[0]) == Some(t.as_str())
                    && ln(&ra[1]) == Some(p1)
                {
                    cons_ok = true;
                }
            }
            _ => {}
        }
    }
    nil_ok && cons_ok
}

/// A list-reversing fold `fn R(p0) = match p0 { [] -> []; [h, ..t] ->
/// A(R(t), [h]) }` paired with its left-append `A`. The classic anti-
/// homomorphism: `R(A(a, b)) == A(R(b), R(a))`. This recognizer is
/// backend-neutral (source names only) — the Lean backend consumes it too,
/// via [`collect_rev_ops_in_law`], to render a kernel-checked proof. (TODO:
/// relocate the recognizer to a backend-neutral module; it lives here for now.)
pub(crate) struct RevOp {
    pub rev: String,
    pub append: String,
}

fn detect_rev_fn(fd: &FnDef, ctx: &CodegenContext) -> Option<RevOp> {
    if fd.params.len() != 1 {
        return None;
    }
    let p0 = fd.params[0].0.as_str();
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let ln = crate::codegen::recursion::detect::local_name_of;
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if ln(subject) != Some(p0) || arms.len() != 2 {
        return None;
    }
    let mut nil_ok = false;
    let mut append_name: Option<String> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => {
                nil_ok = matches!(&arm.body.node, Expr::List(es) if es.is_empty())
            }
            Pattern::Cons(h, t) => {
                if let Expr::FnCall(callee, args) = &arm.body.node
                    && let Some(app) = dotted(callee)
                    && args.len() == 2
                    && let Expr::FnCall(rc, ra) = &args[0].node
                    && dotted(rc).as_deref().map(short_ctor) == Some(fd.name.as_str())
                    && ra.len() == 1
                    && ln(&ra[0]) == Some(t.as_str())
                    && matches!(&args[1].node, Expr::List(es) if es.len() == 1 && ln(&es[0]) == Some(h.as_str()))
                    && ctx
                        .fn_def_by_name(&app, ctx.active_module_scope().as_deref())
                        .is_some_and(|afd| is_recursive_left_append(afd, ctx))
                {
                    append_name = Some(app);
                }
            }
            _ => {}
        }
    }
    if nil_ok {
        append_name.map(|append| RevOp {
            rev: fd.name.clone(),
            append,
        })
    } else {
        None
    }
}

pub(crate) fn collect_rev_ops_in_law(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<RevOp> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut names);
    collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    let mut seen = std::collections::BTreeSet::new();
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(|fd| detect_rev_fn(fd, ctx))
        .filter(|op| seen.insert(op.rev.clone()))
        .collect()
}

/// A list-length fold `fn L(x: List<Int>) = match x { [] -> base; [h, ..t] ->
/// Succ(L(t)) }` where `Succ` is any unary wrapper (typically a `Nat` succ
/// ctor). For this shape the snoc law `L(s ++ [e]) == Succ(L(s))` holds
/// structurally (one extra `Succ` per appended element), which is what a
/// length-preservation proof needs (e.g. `length(rev x) == length x`).
/// `succ` carries the wrapper as written (e.g. `Nat.S`) so the renderer can
/// reproduce it verbatim.
pub(crate) struct LenFold {
    pub name: String,
    pub succ: String,
}

fn detect_len_fold(fd: &FnDef) -> Option<LenFold> {
    if fd.params.len() != 1 || fd.params[0].1.trim() != "List<Int>" {
        return None;
    }
    let p0 = fd.params[0].0.as_str();
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);
    let ln = crate::codegen::recursion::detect::local_name_of;
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if ln(subject) != Some(p0) || arms.len() != 2 {
        return None;
    }
    let mut has_base = false;
    let mut succ: Option<String> = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => has_base = true,
            Pattern::Cons(_h, t) => {
                if let Expr::FnCall(scallee, sargs) = &arm.body.node
                    && sargs.len() == 1
                    && let Some(sname) = dotted(scallee)
                    && let Expr::FnCall(rc, ra) = &sargs[0].node
                    && dotted(rc).as_deref().map(short_ctor) == Some(fd.name.as_str())
                    && ra.len() == 1
                    && ln(&ra[0]) == Some(t.as_str())
                {
                    succ = Some(sname);
                }
            }
            _ => {}
        }
    }
    if has_base {
        succ.map(|succ| LenFold {
            name: fd.name.clone(),
            succ,
        })
    } else {
        None
    }
}

pub(crate) fn collect_len_folds_in_law(law: &VerifyLaw, ctx: &CodegenContext) -> Vec<LenFold> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut names);
    collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    let mut seen = std::collections::BTreeSet::new();
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(detect_len_fold)
        .filter(|fold| seen.insert(fold.name.clone()))
        .collect()
}

/// A canonical Peano arithmetic operator a proof backend can lift to the host's
/// builtin `Nat` operation, unlocking the solver's arithmetic automation. Shape,
/// NOT name: a binary fn on a canonical Peano type `T` (returning `T`) whose body
/// is EXACTLY the standard recursion of the named operation —
///   Add: `match a { Base -> b; Succ(q) -> Succ(op(q, b)) }`            (a + b)
///   Sub: `match a { Base -> Base; Succ(q) -> match b {                 (truncated a - b)
///             Base -> a; Succ(r) -> op(q, r) } }`
///   Mul: `match a { Base -> Base; Succ(q) -> add(b, op(q, b)) }`       (a * b)
///        where `add` is itself a recognized [`NatArithKind::Add`].
/// Builtin `Nat`'s `+`/`-`/`*` ARE these equations, so the lift is a sound
/// isomorphism. Crucially the recognizer is never trusted: a backend emits a
/// kernel-CHECKED bridge `op a b = a + b` (proved by induction), so a
/// misrecognition makes that bridge proof fail — it can never mint a false
/// theorem. Conservative by construction (every structural slot is pinned).
/// `omega` decides `+`/`-`; `*` is nonlinear (no `omega`, and core Lean has no
/// `ring`) so only laws expressible via core `Nat.mul_*` lemmas (distributivity,
/// associativity) close — pure commutativity needs `ring`/Mathlib and falls back.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum NatArithKind {
    Add,
    Sub,
    Mul,
}

#[derive(Clone, Debug)]
pub(crate) struct NatArithOp {
    pub fn_name: String,
    pub kind: NatArithKind,
}

/// Extract `(short ctor/callee name, arg exprs)` from either constructor spelling:
/// `Nat.S(x)` parses as an `FnCall` to a dotted ctor name, while a bare nullary
/// `Nat.Z` is an `Expr::Constructor(_, None)`.
fn call_or_ctor(e: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
    match &e.node {
        Expr::FnCall(callee, args) => {
            let name = crate::codegen::common::expr_to_dotted_name(&callee.node)?;
            Some((short_ctor(&name).to_string(), args.iter().collect()))
        }
        Expr::Constructor(name, arg) => Some((
            short_ctor(name).to_string(),
            arg.iter().map(|b| b.as_ref()).collect(),
        )),
        // A bare nullary constructor (`Nat.Z`) is parsed as attribute access
        // `Attr(Ident("Nat"), "Z")`, not `Expr::Constructor`, in the source AST
        // these recognizers walk.
        Expr::Attr(..) => crate::codegen::common::expr_to_dotted_name(&e.node)
            .map(|name| (short_ctor(&name).to_string(), Vec::new())),
        // A tail-position self-call (`minus(z, x2)` as the whole arm body) is
        // rewritten by the TCO pass to `TailCall`, not `FnCall` — the canonical
        // `minus` recurses in tail position, so this arm is load-bearing for Sub.
        Expr::TailCall(tc) => Some((short_ctor(&tc.target).to_string(), tc.args.iter().collect())),
        _ => None,
    }
}

/// Param checks + outer `match p0` split shared by the arithmetic recognizers.
/// Returns `(p0, p1, base_arm_body, succ_binder, succ_arm_body)` for a binary fn
/// over `peano`'s type whose body matches its FIRST param into the canonical
/// base / succ(binder) arms; `None` otherwise.
fn peano_outer_split<'a>(
    fd: &'a FnDef,
    peano: &PeanoType,
) -> Option<(
    &'a str,
    &'a str,
    &'a Spanned<Expr>,
    &'a str,
    &'a Spanned<Expr>,
)> {
    if fd.params.len() != 2 {
        return None;
    }
    let (p0, t0) = &fd.params[0];
    let (p1, t1) = &fd.params[1];
    if t0 != t1 || &fd.return_type != t0 || t0.trim() != peano.type_name {
        return None;
    }
    let ln = crate::codegen::recursion::detect::local_name_of;
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    if ln(subject) != Some(p0.as_str()) || arms.len() != 2 {
        return None;
    }
    let mut base_body: Option<&Spanned<Expr>> = None;
    let mut succ_q: Option<&String> = None;
    let mut succ_body: Option<&Spanned<Expr>> = None;
    for arm in arms {
        let Pattern::Constructor(cname, binders) = &arm.pattern else {
            return None;
        };
        let short = short_ctor(cname);
        if short == peano.base_ctor && binders.is_empty() {
            base_body = Some(&arm.body);
        } else if short == peano.succ_ctor && binders.len() == 1 {
            succ_q = Some(&binders[0]);
            succ_body = Some(&arm.body);
        } else {
            return None;
        }
    }
    Some((
        p0.as_str(),
        p1.as_str(),
        base_body?,
        succ_q?.as_str(),
        succ_body?,
    ))
}

/// True iff `fd` is the canonical Peano addition over `peano`
/// (`match a { Base -> b; Succ(q) -> Succ(op(q, b)) }`). Takes the `PeanoType`
/// directly so it needs NO `CodegenContext` — the lemma-discovery layer (which
/// has only a `ProofLowerInputs` type lookup) uses this to find the monoid `⊕`
/// for a structural homomorphism conjecture.
pub(crate) fn is_canonical_add(fd: &FnDef, peano: &PeanoType) -> bool {
    let Some((_p0, p1, base_body, q, succ_body)) = peano_outer_split(fd, peano) else {
        return false;
    };
    let ln = crate::codegen::recursion::detect::local_name_of;
    let add_succ_ok = call_or_ctor(succ_body).is_some_and(|(c, a)| {
        c == peano.succ_ctor
            && a.len() == 1
            && call_or_ctor(a[0]).is_some_and(|(rc, ra)| {
                rc == fd.name && ra.len() == 2 && ln(ra[0]) == Some(q) && ln(ra[1]) == Some(p1)
            })
    });
    ln(base_body) == Some(p1) && add_succ_ok
}

/// Recognize a canonical Peano `+` / truncated `-` / `*` (see [`NatArithKind`]).
fn detect_nat_arith_op(fd: &FnDef, ctx: &CodegenContext) -> Option<NatArithKind> {
    let peano = peano_type_named(ctx, &fd.params.first()?.1)?;
    if is_canonical_add(fd, &peano) {
        return Some(NatArithKind::Add);
    }
    let (p0, p1, base_body, q, succ_body) = peano_outer_split(fd, &peano)?;
    let ln = crate::codegen::recursion::detect::local_name_of;
    let base_is_base =
        call_or_ctor(base_body).is_some_and(|(c, a)| c == peano.base_ctor && a.is_empty());

    // Sub (truncated): `Base -> Base` and `Succ(q) -> match b { Base -> a; Succ(r) -> op(q, r) }`.
    if base_is_base
        && let Expr::Match {
            subject: inner_subj,
            arms: inner_arms,
            ..
        } = &succ_body.node
        && ln(inner_subj) == Some(p1)
        && inner_arms.len() == 2
    {
        let mut inner_base_ok = false;
        let mut inner_succ_ok = false;
        for arm in inner_arms {
            let Pattern::Constructor(cname, binders) = &arm.pattern else {
                return None;
            };
            let short = short_ctor(cname);
            if short == peano.base_ctor && binders.is_empty() {
                // `minus(S q, Z) = S q = p0` (the whole first argument).
                inner_base_ok = ln(&arm.body) == Some(p0);
            } else if short == peano.succ_ctor && binders.len() == 1 {
                let r = binders[0].as_str();
                inner_succ_ok = call_or_ctor(&arm.body).is_some_and(|(rc, ra)| {
                    rc == fd.name && ra.len() == 2 && ln(ra[0]) == Some(q) && ln(ra[1]) == Some(r)
                });
            } else {
                return None;
            }
        }
        if inner_base_ok && inner_succ_ok {
            return Some(NatArithKind::Sub);
        }
    }

    // Mul: `Base -> Base` and `Succ(q) -> add(b, op(q, b))` where `add` is itself a
    // recognized canonical addition over the same Peano type. `times x y = match
    // x { Z -> Z; S z -> plus(y, times(z, y)) }`. The bridge `times a b = a * b`
    // is proved USING the add bridge, so the renderer emits the add bridge first.
    if base_is_base
        && let Some((add_fn, args)) = call_or_ctor(succ_body)
        && args.len() == 2
        && ln(args[0]) == Some(p1)
        && call_or_ctor(args[1]).is_some_and(|(rc, ra)| {
            rc == fd.name && ra.len() == 2 && ln(ra[0]) == Some(q) && ln(ra[1]) == Some(p1)
        })
        && ctx
            .fn_def_by_name(&add_fn, ctx.active_module_scope().as_deref())
            .is_some_and(|afd| afd.name != fd.name && is_canonical_add(afd, &peano))
    {
        return Some(NatArithKind::Mul);
    }

    None
}

/// A canonical Peano comparison operator (`≤` / `<`) — a binary fn on a canonical
/// Peano type RETURNING `Bool`, whose body is exactly the standard comparison
/// recursion:
///   Le: `match a { Base -> true; Succ(q) -> match b { Base -> false; Succ(r) -> op(q, r) } }`
///   Lt: `match b { Base -> false; Succ(q) -> match a { Base -> true; Succ(r) -> op(r, q) } }`
/// Note `<` matches its SECOND argument first. A backend lifts `op a b = true` to
/// the Prop `a ≤ b` / `a < b` via a kernel-proved bridge `(op a b = true) = (a R b)`,
/// handing the goal to `omega`. Same untrusted-recognizer guarantee as
/// [`NatArithKind`]: a misrecognition fails the bridge proof, never mints a theorem.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum NatCompareKind {
    Le,
    Lt,
}

#[derive(Clone, Debug)]
pub(crate) struct NatCompareOp {
    pub fn_name: String,
    pub kind: NatCompareKind,
}

/// Split a 2-arm `match` over a canonical Peano value into
/// `(base_arm_body, succ_binder, succ_arm_body)`. `None` if the arms are not
/// exactly one nullary base ctor + one unary succ ctor of `peano`.
fn split_peano_match<'a>(
    arms: &'a [crate::ast::MatchArm],
    peano: &PeanoType,
) -> Option<(&'a Spanned<Expr>, &'a str, &'a Spanned<Expr>)> {
    if arms.len() != 2 {
        return None;
    }
    let mut base: Option<&Spanned<Expr>> = None;
    let mut succ_q: Option<&str> = None;
    let mut succ_b: Option<&Spanned<Expr>> = None;
    for arm in arms {
        let Pattern::Constructor(cname, binders) = &arm.pattern else {
            return None;
        };
        let short = short_ctor(cname);
        if short == peano.base_ctor && binders.is_empty() {
            base = Some(&arm.body);
        } else if short == peano.succ_ctor && binders.len() == 1 {
            succ_q = Some(binders[0].as_str());
            succ_b = Some(&arm.body);
        } else {
            return None;
        }
    }
    Some((base?, succ_q?, succ_b?))
}

fn as_bool_lit(e: &Spanned<Expr>) -> Option<bool> {
    match &e.node {
        Expr::Literal(crate::ast::Literal::Bool(b)) => Some(*b),
        _ => None,
    }
}

/// Recognize a canonical Peano `≤` / `<` (see [`NatCompareKind`]).
fn detect_nat_compare_op(fd: &FnDef, ctx: &CodegenContext) -> Option<NatCompareKind> {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" {
        return None;
    }
    let (p0, t0) = &fd.params[0];
    let (p1, t1) = &fd.params[1];
    if t0 != t1 {
        return None;
    }
    let peano = peano_type_named(ctx, t0)?;
    let ln = crate::codegen::recursion::detect::local_name_of;
    let tail = fd.body.tail_expr()?;
    let Expr::Match { subject, arms, .. } = &tail.node else {
        return None;
    };
    let outer_on = ln(subject)?;
    let (base_body, q, succ_body) = split_peano_match(arms, &peano)?;

    // The succ arm nests a match on the OTHER param; recursion strips one succ
    // from each. The base-arm bool and the inner-base bool encode `≤` vs `<`.
    let Expr::Match {
        subject: inner_subj,
        arms: inner_arms,
        ..
    } = &succ_body.node
    else {
        return None;
    };
    let inner_on = ln(inner_subj)?;
    let (inner_base, r, inner_succ) = split_peano_match(inner_arms, &peano)?;
    let rec_ok = |first: &str, second: &str| {
        call_or_ctor(inner_succ).is_some_and(|(rc, ra)| {
            rc == fd.name && ra.len() == 2 && ln(ra[0]) == Some(first) && ln(ra[1]) == Some(second)
        })
    };

    // Le: outer on p0; `Base -> true`, inner on p1 `Base -> false`, `Succ(r) -> op(q, r)`.
    if outer_on == p0.as_str()
        && inner_on == p1.as_str()
        && as_bool_lit(base_body) == Some(true)
        && as_bool_lit(inner_base) == Some(false)
        && rec_ok(q, r)
    {
        return Some(NatCompareKind::Le);
    }

    // Lt: outer on p1; `Base -> false`, inner on p0 `Base -> true`, `Succ(r) -> op(r, q)`.
    if outer_on == p1.as_str()
        && inner_on == p0.as_str()
        && as_bool_lit(base_body) == Some(false)
        && as_bool_lit(inner_base) == Some(true)
        && rec_ok(r, q)
    {
        return Some(NatCompareKind::Lt);
    }

    None
}

/// Collect the distinct canonical Peano comparison operators a law invokes.
pub(crate) fn collect_nat_compare_ops_in_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<NatCompareOp> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut names);
    collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    let mut seen = std::collections::BTreeSet::new();
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(|fd| detect_nat_compare_op(fd, ctx).map(|kind| (fd, kind)))
        .filter(|(fd, _)| seen.insert(fd.name.clone()))
        .map(|(fd, kind)| NatCompareOp {
            fn_name: fd.name.clone(),
            kind,
        })
        .collect()
}

/// Collect the distinct canonical Peano arithmetic operators a law invokes
/// (directly or transitively), each tagged with the host op it lifts to.
pub(crate) fn collect_nat_arith_ops_in_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<NatArithOp> {
    let mut names: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    collect_called_fns(&law.lhs, &mut names);
    collect_called_fns(&law.rhs, &mut names);
    let mut transitive: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for f in &names {
        if let Some(fd) = ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()) {
            collect_called_fns_in_body(&fd.body, &mut transitive);
        }
    }
    names.extend(transitive);
    let mut seen = std::collections::BTreeSet::new();
    names
        .iter()
        .filter_map(|f| ctx.fn_def_by_name(f, ctx.active_module_scope().as_deref()))
        .filter_map(|fd| detect_nat_arith_op(fd, ctx).map(|kind| (fd, kind)))
        .filter(|(fd, _)| seen.insert(fd.name.clone()))
        .map(|(fd, kind)| NatArithOp {
            fn_name: fd.name.clone(),
            kind,
        })
        .collect()
}
