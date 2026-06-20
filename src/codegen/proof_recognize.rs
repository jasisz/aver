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

/// Whether a law is a commutativity or associativity statement of a binary fn.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum BinaryLawShape {
    /// `f a b = f b a` over two givens.
    Commutativity,
    /// `f (f a b) c = f a (f b c)` over three givens.
    Associativity,
}

/// Recognize a law as the commutativity (`f a b = f b a`) or associativity
/// (`f (f a b) c = f a (f b c)`) of `fn_name`, where each leaf argument is a
/// distinct law `given`. Pure syntactic match against the law's `lhs`/`rhs`:
/// every operand of `f` is a bare given identifier (no nested user calls, no
/// constructors), the givens used are exactly the law's givens, and `f` is the
/// only fn applied. Returns `None` for anything else — including a `when`
/// premise (a conditional law is not an unconditional algebraic identity).
///
/// This is what gates the both-args-peeling generalizing-induction emit: it
/// fires ONLY on these two genuine algebraic shapes, so a same-shaped fn used
/// in an unrelated law (`minus(plus(n,m),n) = m`, a comparison `n ≤ m+n`) is
/// NOT matched and keeps its existing proof path.
pub(crate) fn recognize_binary_law_shape(
    law: &VerifyLaw,
    fn_name: &str,
    given_names: &[String],
) -> Option<BinaryLawShape> {
    if law.when.is_some() {
        return None;
    }
    // `f(x, y)` where `f`'s short name is `fn_name` → `(x, y)`.
    let as_self_call = |e: &Spanned<Expr>| -> Option<(Spanned<Expr>, Spanned<Expr>)> {
        let (callee, args) = match &e.node {
            Expr::FnCall(c, a) => (crate::codegen::common::expr_to_dotted_name(&c.node)?, a),
            _ => return None,
        };
        if short_ctor(&callee) != short_ctor(fn_name) || args.len() != 2 {
            return None;
        }
        Some((args[0].clone(), args[1].clone()))
    };
    let given_name_of = |e: &Spanned<Expr>| -> Option<String> {
        crate::codegen::recursion::detect::local_name_of(e)
            .filter(|id| given_names.iter().any(|g| g == id))
            .map(str::to_string)
    };

    // Commutativity: `f(a, b) = f(b, a)`, both args bare distinct givens.
    if given_names.len() == 2
        && let Some((la, lb)) = as_self_call(&law.lhs)
        && let Some((ra, rb)) = as_self_call(&law.rhs)
        && let (Some(la), Some(lb), Some(ra), Some(rb)) = (
            given_name_of(&la),
            given_name_of(&lb),
            given_name_of(&ra),
            given_name_of(&rb),
        )
        && la != lb
        && la == rb
        && lb == ra
    {
        return Some(BinaryLawShape::Commutativity);
    }

    // Associativity: `f(f(a, b), c) = f(a, f(b, c))`.
    if given_names.len() == 3
        && let Some((l_inner, lc)) = as_self_call(&law.lhs)
        && let Some((la_inner, lb_inner)) = as_self_call(&l_inner)
        && let Some((ra, r_inner)) = as_self_call(&law.rhs)
        && let Some((rb_inner, rc_inner)) = as_self_call(&r_inner)
        && let (Some(la), Some(lb), Some(lc), Some(ra), Some(rb), Some(rc)) = (
            given_name_of(&la_inner),
            given_name_of(&lb_inner),
            given_name_of(&lc),
            given_name_of(&ra),
            given_name_of(&rb_inner),
            given_name_of(&rc_inner),
        )
        && la == ra
        && lb == rb
        && lc == rc
        && la != lb
        && lb != lc
        && la != lc
    {
        return Some(BinaryLawShape::Associativity);
    }

    None
}

/// `(p0, p1, base_arm_body, succ_binder, succ_arm_body)` — the decomposition
/// [`peano_outer_split`] returns for a canonical binary Peano fn.
type PeanoSplit<'a> = (
    &'a str,
    &'a str,
    &'a Spanned<Expr>,
    &'a str,
    &'a Spanned<Expr>,
);

/// Param checks + outer `match p0` split shared by the arithmetic recognizers.
/// Returns `(p0, p1, base_arm_body, succ_binder, succ_arm_body)` for a binary fn
/// over `peano`'s type whose body matches its FIRST param into the canonical
/// base / succ(binder) arms; `None` otherwise.
fn peano_outer_split<'a>(fd: &'a FnDef, peano: &PeanoType) -> Option<PeanoSplit<'a>> {
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

/// `true` iff `fd` is a STRUCTURALLY COMMUTATIVE both-args-peeling Peano binary
/// fn — the `max`/`min` shape whose two base arms are symmetric:
/// `match a { Base -> A; Succ(q) -> match b { Base -> B; Succ(w) -> Succ(op(q, w)) } }`
/// is commutative exactly when `(A, B)` is a symmetric pair: either `A = b` and
/// `B = a` (the `B` reads as the whole first arg) — the `max` shape — or `A` and
/// `B` are both the base constructor — the `min` shape. Both readings give
/// `op(Base, y) = op(y, Base)` for every `y`, which together with the symmetric
/// `Succ(op(q, w))` recursion makes `op a b = op b a` a theorem (proved by
/// `induction a generalizing b … cases b`). Used to gate emitting a kernel-proved
/// `comm` support lemma for a consumed `max`/`min`: a non-commutative
/// both-args-peeling fn is rejected here, so no spurious `comm` lemma (which
/// would only `sorry`) is ever emitted.
pub(crate) fn both_args_peeling_is_commutative(fd: &FnDef, ctx: &CodegenContext) -> Option<()> {
    if !crate::codegen::recursion::detect::recurses_decrementing_both_args(fd) {
        return None;
    }
    let peano = peano_type_named(ctx, &fd.params.first()?.1)?;
    let (p0, p1, base_body, q, succ_body) = peano_outer_split(fd, &peano)?;
    let ln = crate::codegen::recursion::detect::local_name_of;
    let is_base = |e: &Spanned<Expr>| {
        call_or_ctor(e).is_some_and(|(c, a)| c == peano.base_ctor && a.is_empty())
    };
    // succ arm is an inner match on p1 with the canonical `Succ(op(q, w))` rec.
    let Expr::Match {
        subject: inner_subj,
        arms: inner_arms,
        ..
    } = &succ_body.node
    else {
        return None;
    };
    if ln(inner_subj) != Some(p1) {
        return None;
    }
    let (inner_base, w, inner_succ) = split_peano_match(inner_arms, &peano)?;
    let rec_ok = call_or_ctor(inner_succ).is_some_and(|(c, a)| {
        c == peano.succ_ctor
            && a.len() == 1
            && call_or_ctor(a[0]).is_some_and(|(rc, ra)| {
                rc == fd.name && ra.len() == 2 && ln(ra[0]) == Some(q) && ln(ra[1]) == Some(w)
            })
    });
    if !rec_ok {
        return None;
    }
    // max shape: outer-base returns the other param (`b`), inner-base returns
    // the first param (`a`, bound as the whole `Succ(q)` value).
    let max_shape = ln(base_body) == Some(p1) && ln(inner_base) == Some(p0);
    // min shape: both base arms return the base constructor.
    let min_shape = is_base(base_body) && is_base(inner_base);
    (max_shape || min_shape).then_some(())
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
    /// Canonical Peano decidable equality (`eq Z Z = true`, `eq (S a) (S b) =
    /// eq a b`, mismatches `false`). Bridged to the Prop `a = b` via
    /// `(eq a b = true) = (a = b)`.
    Eq,
}

impl NatCompareKind {
    /// The Lean Prop relation the bridge maps `f a b = true` onto.
    pub(crate) fn prop_op(self) -> &'static str {
        match self {
            NatCompareKind::Le => "≤",
            NatCompareKind::Lt => "<",
            NatCompareKind::Eq => "=",
        }
    }

    /// The bridge theorem's name suffix (kept distinct per relation so two
    /// bridges in one law never collide, and the both-args rung can spot them).
    pub(crate) fn bridge_suffix(self) -> &'static str {
        match self {
            NatCompareKind::Le => "isNatLe",
            NatCompareKind::Lt => "isNatLt",
            NatCompareKind::Eq => "isNatEq",
        }
    }

    /// Whether the recursion drives on the SECOND argument. `<` destructures its
    /// second arg first, so its bridge inducts on `b`; `≤` and `=` drive on `a`.
    /// The bridge proof inducts on the driver and case-splits the passenger.
    pub(crate) fn induct_on_second(self) -> bool {
        matches!(self, NatCompareKind::Lt)
    }
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

    // Eq: outer on p0; the succ arm matches `≤`'s recursion shape (inner on p1,
    // `Base -> false`, `Succ(r) -> op(q, r)`), but the BASE arm — unlike `≤`/`<`,
    // whose base is a bare bool — itself nests a match on p1 (`Z -> true`, `S ->
    // false`). That second nesting is exactly Peano decidable equality.
    if outer_on == p0.as_str()
        && inner_on == p1.as_str()
        && as_bool_lit(inner_base) == Some(false)
        && rec_ok(q, r)
        && let Expr::Match {
            subject: base_subj,
            arms: base_arms,
            ..
        } = &base_body.node
        && ln(base_subj) == Some(p1.as_str())
        && let Some((bb_base, _z, bb_succ)) = split_peano_match(base_arms, &peano)
        && as_bool_lit(bb_base) == Some(true)
        && as_bool_lit(bb_succ) == Some(false)
    {
        return Some(NatCompareKind::Eq);
    }

    None
}

/// Fn names a law invokes directly plus one level of transitive callees —
/// the shared name-gathering step of the canonical-op collectors below.
fn law_called_fn_names(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> std::collections::BTreeSet<String> {
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
    names
}

/// Collect the distinct canonical Peano comparison operators a law invokes.
pub(crate) fn collect_nat_compare_ops_in_law(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<NatCompareOp> {
    collect_nat_compare_ops_for_names(&law_called_fn_names(law, ctx), ctx)
}

/// Canonical Peano comparison operators among an explicit fn-name set —
/// the name-driven core of [`collect_nat_compare_ops_in_law`]. Also used by
/// the discovery feedback loop, where a committed lemma can introduce an op
/// (e.g. `lessEq`) the law itself never mentions.
pub(crate) fn collect_nat_compare_ops_for_names(
    names: &std::collections::BTreeSet<String>,
    ctx: &CodegenContext,
) -> Vec<NatCompareOp> {
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
    collect_nat_arith_ops_for_names(&law_called_fn_names(law, ctx), ctx)
}

/// Canonical Peano arithmetic operators among an explicit fn-name set —
/// the name-driven core of [`collect_nat_arith_ops_in_law`]. Also used by
/// the discovery feedback loop, where a committed homomorphism lemma can
/// introduce an op (e.g. `plus`) the law itself never mentions.
pub(crate) fn collect_nat_arith_ops_for_names(
    names: &std::collections::BTreeSet<String>,
    ctx: &CodegenContext,
) -> Vec<NatArithOp> {
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

// ---- String-position scanner shape ----------------------------------

/// Canonical fuelized string-position scanner shape:
///
/// ```text
/// fn scan(s: String, pos: Int, carried..., pinned...) -> R
///     match String.charAt(s, pos)
///         Option.None -> EXIT
///         Option.Some(c) -> match P(c)
///             true -> SELF(s, pos + 1, carried..., <literal pins>)
///             false -> OTHER
/// ```
///
/// where the `true` arm may interpose Bool matches on PINNED params
/// (params the unique self-call fixes to a Bool literal) on the path to
/// the self-call — `scanIntTail`'s `match leadingZero` shape. The shape
/// is what makes the synthesized `<fn>__fuel_scan` lemma provable by
/// the fixed fuel-induction template: an all-`P` suffix scan runs to
/// the end of the string and returns `EXIT[pos := s.data.length]`.
///
/// CONSERVATIVE BY CONSTRUCTION — every gate below exists so that the
/// emitted companion lemma proves on every body the recognizer accepts
/// (a synthesized lemma that fails to prove is a build error in the
/// export, the worst failure mode):
/// - params: `(String, Int, ...)`, none named like a template binder;
/// - body: exactly the two-arm `String.charAt` match above;
/// - dispatch: a two-arm `true`/`false` match on `P(c)` where `P` is a
///   single-argument call on the scrutinee binder (no wildcard arms —
///   the Lean emitter's Bool-match → `ite` lowering is only certain
///   for explicit literal arms);
/// - exactly ONE self-call in the whole body, in the `true` arm, with
///   args `(s, pos + 1, ...)` where every trailing arg is the matching
///   param ident (carried) or a Bool literal (pinned);
/// - the `true` arm reduces to the self-call under the pins;
/// - `EXIT` is built from substitution-safe forms only (idents,
///   literals, calls, binops, constructors) so the lemma statement can
///   render `EXIT[pos := s.data.length, pins]` exactly.
#[derive(Clone, Debug)]
pub(crate) struct StringPosScanShape {
    /// Source name of the single-arg Bool predicate fn (`isDigit`).
    pub predicate_fn: String,
    /// For each param at index ≥ 2: `None` = carried through the
    /// self-call unchanged, `Some(b)` = pinned to the Bool literal.
    pub param_pins: Vec<Option<bool>>,
    /// The `Option.None` arm's exit expression (substitution-safe).
    pub exit_expr: Spanned<Expr>,
}

/// Names the synthesized scan-lemma template binds. A scanner whose
/// params (or predicate/exit identifiers) collide would have the
/// template shadow them — reject instead.
const SCAN_TEMPLATE_RESERVED: &[&str] = &[
    "fuel",
    "ih",
    "h0",
    "h1",
    "h2",
    "h3",
    "hch",
    "hdrop",
    "hdig",
    "hstep",
    "hrec",
    "hlt",
    "hpos",
    "ch",
    // The Lean emitter's textual end-of-string marker (`LEN_MARKER` in
    // lean/toplevel.rs): an identifier with this literal name would be
    // clobbered by the post-render replace.
    "AVERSCANLEN",
];

/// Variable reference name: matches both pre-resolver `Ident` and the
/// resolver's `Resolved { name, .. }` forms (FnDef bodies in a
/// `CodegenContext` are post-resolver).
fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
        _ => None,
    }
}

/// Recognize the canonical string-position scanner shape on a single
/// fn definition. Pure AST walk; the caller separately validates the
/// predicate fn via [`scan_predicate_fn_ok`] and that the fn actually
/// got the string-pos fuel emission.
pub(crate) fn detect_string_pos_scan(fd: &FnDef) -> Option<StringPosScanShape> {
    let dotted = |e: &Spanned<Expr>| crate::codegen::common::expr_to_dotted_name(&e.node);

    if fd.params.len() < 2 || fd.params[0].1 != "String" || fd.params[1].1 != "Int" {
        return None;
    }
    if fd
        .params
        .iter()
        .any(|(n, _)| SCAN_TEMPLATE_RESERVED.contains(&n.as_str()))
    {
        return None;
    }
    let s_name = fd.params[0].0.as_str();
    let pos_name = fd.params[1].0.as_str();

    // Body: a single charAt match (no leading bindings).
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    let Expr::FnCall(callee, args) = &subject.node else {
        return None;
    };
    if dotted(callee).as_deref() != Some("String.charAt") || args.len() != 2 {
        return None;
    }
    if ident_name(&args[0]) != Some(s_name) || ident_name(&args[1]) != Some(pos_name) {
        return None;
    }
    if arms.len() != 2 {
        return None;
    }
    let none_arm = arms.iter().find(
        |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.None" && b.is_empty()),
    )?;
    let some_arm = arms.iter().find(
        |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.Some" && b.len() == 1),
    )?;
    let Pattern::Constructor(_, some_binders) = &some_arm.pattern else {
        return None;
    };
    let c_name = some_binders[0].as_str();

    // Some arm: two-arm true/false match on `P(c)`.
    let Expr::Match {
        subject: pred_subject,
        arms: pred_arms,
    } = &some_arm.body.node
    else {
        return None;
    };
    let Expr::FnCall(pred_callee, pred_args) = &pred_subject.node else {
        return None;
    };
    let predicate_fn = dotted(pred_callee)?;
    if predicate_fn.contains('.') {
        return None; // user fn only — builtins aren't re-emittable hypotheses
    }
    if SCAN_TEMPLATE_RESERVED.contains(&predicate_fn.as_str()) {
        return None;
    }
    // A param sharing the predicate's name (or the fuel wrapper's)
    // would shadow it inside the lemma's ∀-binder — reject.
    let fuel_wrapper = format!("{}__fuel", fd.name);
    if fd
        .params
        .iter()
        .any(|(n, _)| *n == predicate_fn || *n == fuel_wrapper)
    {
        return None;
    }
    if pred_args.len() != 1 || ident_name(&pred_args[0]) != Some(c_name) {
        return None;
    }
    if pred_arms.len() != 2 {
        return None;
    }
    let true_arm = pred_arms.iter().find(|a| {
        matches!(
            &a.pattern,
            Pattern::Literal(crate::ast::Literal::Bool(true))
        )
    })?;
    pred_arms.iter().find(|a| {
        matches!(
            &a.pattern,
            Pattern::Literal(crate::ast::Literal::Bool(false))
        )
    })?;

    // Exactly one self-call in the whole body, and it lives in the true arm.
    if count_calls_to(body, &fd.name) != 1 || count_calls_to(&true_arm.body, &fd.name) != 1 {
        return None;
    }
    let self_args = find_self_call_args(&true_arm.body, &fd.name)?;
    if self_args.len() != fd.params.len() {
        return None;
    }
    if ident_name(&self_args[0]) != Some(s_name) {
        return None;
    }
    let Expr::BinOp(crate::ast::BinOp::Add, l, r) = &self_args[1].node else {
        return None;
    };
    if ident_name(l) != Some(pos_name)
        || !matches!(&r.node, Expr::Literal(crate::ast::Literal::Int(1)))
    {
        return None;
    }
    let mut param_pins: Vec<Option<bool>> = Vec::new();
    for (i, arg) in self_args.iter().enumerate().skip(2) {
        match (&arg.node, ident_name(arg)) {
            (_, Some(n)) if n == fd.params[i].0 => param_pins.push(None),
            (Expr::Literal(crate::ast::Literal::Bool(b)), _) => param_pins.push(Some(*b)),
            _ => return None,
        }
    }

    // The true arm must reduce to the self-call under the pins.
    if !reduces_to_self_call(&true_arm.body, fd, &param_pins) {
        return None;
    }

    // EXIT: substitution-safe forms; identifiers limited to the
    // scanner's own params (minus the Some-binder) and template-safe
    // callee names.
    let allowed: Vec<&str> = fd.params.iter().map(|(n, _)| n.as_str()).collect();
    if !exit_expr_substitution_safe(&none_arm.body, &allowed) {
        return None;
    }
    // The lemma template's conclusion rewrites `pos` occurrences in
    // EXIT to the string length (`rw [hpos]` must have work to do); a
    // pos-free EXIT leaves the goal already closed after `simp only`
    // and the rewrite FAILS — a build error in the export, the exact
    // failure mode this gate exists to prevent. Provable-by-construction
    // therefore requires `pos` to occur in EXIT.
    if !expr_mentions_ident(&none_arm.body, pos_name) {
        return None;
    }

    Some(StringPosScanShape {
        predicate_fn,
        param_pins,
        exit_expr: (*none_arm.body).clone(),
    })
}

/// The predicate gate shared by the scan-lemma emission (Lean backend)
/// and the `IntDecimalRoundtrip` law detector (`proof_lower`): a pure
/// single-`String`-param fn returning `Bool`. The lemma never unfolds
/// the predicate — it only needs the name to exist as an emitted def —
/// so no recursion gate is needed.
pub(crate) fn scan_predicate_fn_ok(fd: &FnDef) -> bool {
    fd.effects.is_empty()
        && fd.params.len() == 1
        && fd.params[0].1 == "String"
        && fd.return_type == "Bool"
}

fn count_calls_to(expr: &Spanned<Expr>, target: &str) -> usize {
    let mut count = 0;
    walk_calls(expr, &mut |name| {
        if name == target {
            count += 1;
        }
    });
    count
}

fn walk_calls(expr: &Spanned<Expr>, on_call: &mut impl FnMut(&str)) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = crate::codegen::common::expr_to_dotted_name(&callee.node) {
                on_call(&name);
            }
            walk_calls(callee, on_call);
            for a in args {
                walk_calls(a, on_call);
            }
        }
        Expr::TailCall(data) => {
            on_call(&data.target);
            for a in &data.args {
                walk_calls(a, on_call);
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_calls(l, on_call);
            walk_calls(r, on_call);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) | Expr::Attr(inner, _) => {
            walk_calls(inner, on_call);
        }
        Expr::Match { subject, arms } => {
            walk_calls(subject, on_call);
            for arm in arms {
                walk_calls(&arm.body, on_call);
            }
        }
        Expr::Constructor(_, Some(inner)) => walk_calls(inner, on_call),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for i in items {
                walk_calls(i, on_call);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk_calls(k, on_call);
                walk_calls(v, on_call);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                walk_calls(v, on_call);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk_calls(base, on_call);
            for (_, v) in updates {
                walk_calls(v, on_call);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                if let crate::ast::StrPart::Parsed(inner) = p {
                    walk_calls(inner, on_call);
                }
            }
        }
        _ => {}
    }
}

/// Args of the unique self-call (plain or tail-call form) inside `expr`.
fn find_self_call_args<'a>(expr: &'a Spanned<Expr>, target: &str) -> Option<&'a [Spanned<Expr>]> {
    match &expr.node {
        Expr::FnCall(callee, args)
            if crate::codegen::common::expr_to_dotted_name(&callee.node).as_deref()
                == Some(target) =>
        {
            Some(args)
        }
        Expr::TailCall(data) if data.target == target => Some(&data.args),
        Expr::Match { arms, .. } => arms
            .iter()
            .find_map(|arm| find_self_call_args(&arm.body, target)),
        _ => None,
    }
}

/// True when `expr` IS the self-call, or is a two-arm Bool match on a
/// PINNED param whose selected (pinned-literal) arm recursively reduces
/// to the self-call. The Lean emitter lowers explicit-Bool-literal
/// matches to `ite`, which the lemma template's trailing `simp`
/// computes away once the pin substitutes the scrutinee.
fn reduces_to_self_call(expr: &Spanned<Expr>, fd: &FnDef, pins: &[Option<bool>]) -> bool {
    match &expr.node {
        Expr::FnCall(callee, _)
            if crate::codegen::common::expr_to_dotted_name(&callee.node).as_deref()
                == Some(fd.name.as_str()) =>
        {
            true
        }
        Expr::TailCall(data) if data.target == fd.name => true,
        Expr::Match { subject, arms } => {
            let Some(subj) = ident_name(subject) else {
                return false;
            };
            let Some(idx) = fd.params.iter().position(|(n, _)| n.as_str() == subj) else {
                return false;
            };
            if idx < 2 {
                return false;
            }
            let Some(Some(pin)) = pins.get(idx - 2) else {
                return false;
            };
            if arms.len() != 2 {
                return false;
            }
            let selected = arms.iter().find(
                |a| matches!(&a.pattern, Pattern::Literal(crate::ast::Literal::Bool(b)) if b == pin),
            );
            let other = arms.iter().any(
                |a| matches!(&a.pattern, Pattern::Literal(crate::ast::Literal::Bool(b)) if b != pin),
            );
            match selected {
                Some(arm) if other => reduces_to_self_call(&arm.body, fd, pins),
                _ => false,
            }
        }
        _ => false,
    }
}

/// EXIT gate: only forms [`substitute_idents_in_expr`] handles, with
/// identifiers drawn from `allowed` (the scanner's params) and callee /
/// constructor names outside the template-reserved set.
/// True when `name` occurs as an identifier anywhere in an EXIT-gated
/// expression (same form set as [`exit_expr_substitution_safe`]).
fn expr_mentions_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n == name,
        Expr::Neg(inner) | Expr::ErrorProp(inner) => expr_mentions_ident(inner, name),
        Expr::BinOp(_, l, r) => expr_mentions_ident(l, name) || expr_mentions_ident(r, name),
        Expr::FnCall(callee, args) => {
            expr_mentions_ident(callee, name) || args.iter().any(|a| expr_mentions_ident(a, name))
        }
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .is_some_and(|p| expr_mentions_ident(p, name)),
        Expr::Tuple(items) | Expr::List(items) => {
            items.iter().any(|i| expr_mentions_ident(i, name))
        }
        _ => false,
    }
}

fn exit_expr_substitution_safe(expr: &Spanned<Expr>, allowed: &[&str]) -> bool {
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => allowed.contains(&n.as_str()),
        Expr::Literal(_) => true,
        Expr::Neg(inner) | Expr::ErrorProp(inner) => exit_expr_substitution_safe(inner, allowed),
        Expr::BinOp(_, l, r) => {
            exit_expr_substitution_safe(l, allowed) && exit_expr_substitution_safe(r, allowed)
        }
        Expr::FnCall(callee, args) => {
            let Some(name) = crate::codegen::common::expr_to_dotted_name(&callee.node) else {
                return false;
            };
            !SCAN_TEMPLATE_RESERVED.contains(&name.as_str())
                && args.iter().all(|a| exit_expr_substitution_safe(a, allowed))
        }
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .is_none_or(|p| exit_expr_substitution_safe(p, allowed)),
        Expr::Tuple(items) | Expr::List(items) => items
            .iter()
            .all(|i| exit_expr_substitution_safe(i, allowed)),
        _ => false,
    }
}

/// Substitute identifiers by name in an EXIT-gated expression. Total
/// over the forms [`exit_expr_substitution_safe`] admits; other forms
/// are returned unchanged (the gate guarantees they don't occur).
pub(crate) fn substitute_idents_in_expr(
    expr: &Spanned<Expr>,
    subst: &std::collections::HashMap<String, Expr>,
) -> Spanned<Expr> {
    let node = match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => match subst.get(n) {
            Some(replacement) => replacement.clone(),
            None => expr.node.clone(),
        },
        Expr::Neg(inner) => Expr::Neg(Box::new(substitute_idents_in_expr(inner, subst))),
        Expr::ErrorProp(inner) => {
            Expr::ErrorProp(Box::new(substitute_idents_in_expr(inner, subst)))
        }
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(substitute_idents_in_expr(l, subst)),
            Box::new(substitute_idents_in_expr(r, subst)),
        ),
        Expr::FnCall(callee, args) => Expr::FnCall(
            callee.clone(),
            args.iter()
                .map(|a| substitute_idents_in_expr(a, subst))
                .collect(),
        ),
        Expr::Constructor(name, payload) => Expr::Constructor(
            name.clone(),
            payload
                .as_ref()
                .map(|p| Box::new(substitute_idents_in_expr(p, subst))),
        ),
        Expr::Tuple(items) => Expr::Tuple(
            items
                .iter()
                .map(|i| substitute_idents_in_expr(i, subst))
                .collect(),
        ),
        Expr::List(items) => Expr::List(
            items
                .iter()
                .map(|i| substitute_idents_in_expr(i, subst))
                .collect(),
        ),
        other => other.clone(),
    };
    Spanned {
        node,
        line: expr.line,
        ty: std::sync::OnceLock::new(),
    }
}
