use super::super::expr::{aver_name_to_lean, emit_expr_legacy};
use crate::ast::{Expr, FnDef, Literal, Pattern, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::shared::{call_of, ctor_of, ident_of};
use super::{LaneLawFile, lane_module_id};

// ===================== FAMILY 2: bridge-shaped premises ============
//
// `when boolRel(a, b)` where boolRel mirrors Prop equality on a
// canonical Peano type (lifted to builtin Nat). All proof text below
// is a verbatim parameterization of scripts hand-validated kernel-
// genuine on the emitted Lean 4.15 projects of TIP prop_85 (zip-rev
// under length equality, [propext, Quot.sound]), prop_46/47,
// lemma_19/21 and prop_76. User fns are referenced as `_root_.<fn>`
// inside simp sets: emitted defs live at the root namespace, and a
// bare name there can resolve against a colliding core export
// (e.g. `insert` → `Insert.insert`), which fails with "proposition
// expected" — paid-for landmine from the hand validation.

/// How a negated bridge premise reaches the equality fn.
pub(super) enum BridgeNeg {
    /// `when Bool.not(eq(a, b))` — rendered `(!eq a b) = true`.
    BoolNot,
    /// `when w(a, b)` where `w` is the 2-arm not-wrapper
    /// `match eq(a, b) { true -> false, false -> true }`.
    Wrapper(String),
}

/// The hand-validated bridge-premise figures. Source fn names only;
/// the renderer converts to Lean names. Anything outside these exact
/// constellations declines at zero cost.
pub(super) enum BridgePlan {
    /// TIP prop_85: `when eq(len(xs), len(ys))`,
    /// `zip(rev(xs), rev(ys)) = revPair(zip(xs, ys))`. Needs the full
    /// probe kit: both bridges, measure lemmas over append/rev, a
    /// premise-driven shape inversion of the non-induction variable,
    /// and the snoc-distribution aux lemma.
    ZipRevLenEq {
        eq_fn: String,
        len_fn: String,
        zip_fn: String,
        rev_fn: String,
        rev_pair_fn: String,
        append_fn: String,
        append_pair_fn: String,
        /// Rendered Lean element type of the two list givens (`Int`).
        elem_ty: String,
        /// The two list givens, in quantifier order (xs = induction target).
        xs: String,
        ys: String,
    },
    /// TIP prop_46: `when eq(x, y)`, `elem(x, insert(y, z)) = true`.
    /// Bridge + subst, then list induction with the REINTRODUCTION
    /// bridge discharging the `eq x x` dispatch.
    EqElemInsert {
        eq_fn: String,
        elem_fn: String,
        or_fn: String,
        insert_fn: String,
        x: String,
        y: String,
        z: String,
    },
    /// TIP prop_47 / lemma_19: `when neq(x, y)`,
    /// `elem(x, insert(y, z)) = elem(x, z)`.
    NeqElemInsert {
        eq_fn: String,
        neg: BridgeNeg,
        elem_fn: String,
        or_fn: String,
        insert_fn: String,
        x: String,
        y: String,
        z: String,
    },
    /// TIP lemma_21: `when neq(x, y)`,
    /// `count(x, insert(y, z)) = count(x, z)`.
    NeqCountInsert {
        eq_fn: String,
        neg: BridgeNeg,
        count_fn: String,
        insert_fn: String,
        x: String,
        y: String,
        z: String,
    },
    /// TIP prop_76: `when Bool.not(eq(n, m))`,
    /// `count(n, List.concat(xs, [m])) = count(n, xs)`.
    NeqCountAppendSingleton {
        eq_fn: String,
        neg: BridgeNeg,
        count_fn: String,
        n: String,
        m: String,
        xs: String,
    },
}

/// Internal binder names of the insert/count figure templates — a
/// colliding given would be shadowed mid-proof, so such laws decline.
const BRIDGE_RESERVED_INSERT: &[&str] = &["c", "cs", "ih", "hc", "heq", "h_when"];

/// Internal binder names of the zip-rev MAIN theorem template (the
/// support lemmas are closed terms — their binders cannot collide).
const BRIDGE_RESERVED_ZIPREV: &[&str] = &[
    "z", "x2", "y", "x4", "h", "hh", "h0", "hy", "hlen", "hrevlen", "hih", "h_when",
];

fn as_bool_lit(e: &Spanned<Expr>) -> Option<bool> {
    match &e.node {
        Expr::Literal(Literal::Bool(b)) => Some(*b),
        _ => None,
    }
}

/// `List<elem>` annotation check, whitespace-insensitive.
fn is_list_of(list_ann: &str, elem_ann: &str) -> bool {
    let squash = |s: &str| s.chars().filter(|c| !c.is_whitespace()).collect::<String>();
    squash(list_ann) == format!("List<{}>", squash(elem_ann))
}

/// Split `match <ident> { Base -> e1, Succ(b) -> e2 }` over `peano`
/// into (subject ident, base body, succ binder, succ body).
fn peano_match_split<'a>(
    e: &'a Spanned<Expr>,
    peano: &crate::codegen::proof_recognize::PeanoType,
) -> Option<(&'a str, &'a Spanned<Expr>, &'a str, &'a Spanned<Expr>)> {
    let Expr::Match { subject, arms } = &e.node else {
        return None;
    };
    let subj = ident_of(subject)?;
    if arms.len() != 2 {
        return None;
    }
    let mut base = None;
    let mut succ = None;
    for arm in arms {
        let Pattern::Constructor(name, binders) = &arm.pattern else {
            return None;
        };
        let short = crate::codegen::proof_recognize::short_ctor(name);
        if short == peano.base_ctor && binders.is_empty() {
            base = Some(&arm.body);
        } else if short == peano.succ_ctor && binders.len() == 1 {
            succ = Some((binders[0].as_str(), &arm.body));
        } else {
            return None;
        }
    }
    let (q, sb) = succ?;
    Some((subj, base?, q, sb))
}

/// (subject ident, nil body, head binder, tail binder, cons body) —
/// the result of [`list_match_split`].
type ListMatchSplit<'a> = (
    &'a str,
    &'a Spanned<Expr>,
    &'a str,
    &'a str,
    &'a Spanned<Expr>,
);

/// Split `match <ident> { [] -> e1, [h, ..t] -> e2 }` into
/// (subject ident, nil body, head binder, tail binder, cons body).
fn list_match_split(e: &Spanned<Expr>) -> Option<ListMatchSplit<'_>> {
    let Expr::Match { subject, arms } = &e.node else {
        return None;
    };
    let subj = ident_of(subject)?;
    if arms.len() != 2 {
        return None;
    }
    let mut nil = None;
    let mut cons = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => nil = Some(&arm.body),
            Pattern::Cons(h, t) => cons = Some((h.as_str(), t.as_str(), &arm.body)),
            _ => return None,
        }
    }
    let (h, t, cb) = cons?;
    Some((subj, nil?, h, t, cb))
}

/// The base (`Nat.Z`) constructor of `peano` as a payload-free
/// expression — covers both `Constructor` and `Attr` parses.
fn is_peano_base(e: &Spanned<Expr>, peano: &crate::codegen::proof_recognize::PeanoType) -> bool {
    if let Some((name, args)) = ctor_of(e) {
        return crate::codegen::proof_recognize::short_ctor(&name) == peano.base_ctor
            && args.is_empty();
    }
    if let Expr::Attr(base, leaf) = &e.node {
        return ident_of(base) == Some(peano.type_name.as_str()) && *leaf == peano.base_ctor;
    }
    false
}

/// `Succ(inner)` of `peano` — returns the payload expression.
fn peano_succ_of<'a>(
    e: &'a Spanned<Expr>,
    peano: &crate::codegen::proof_recognize::PeanoType,
) -> Option<&'a Spanned<Expr>> {
    let (name, args) = ctor_of(e)?;
    (crate::codegen::proof_recognize::short_ctor(&name) == peano.succ_ctor && args.len() == 1)
        .then(|| args[0])
}

/// `[<ident>]` — a singleton list literal of exactly one identifier.
fn is_singleton_list_of_ident(e: &Spanned<Expr>, name: &str) -> bool {
    matches!(&e.node, Expr::List(items)
        if items.len() == 1 && ident_of(&items[0]) == Some(name))
}

/// Canonical Peano structural equality (`natEq`-shape): a pure binary
/// Bool fn on a canonical Peano type whose body mirrors `=` exactly —
/// `match a { Z -> match b { Z -> true, S _ -> false },
///            S x -> match b { Z -> false, S y -> rec(x, y) } }`.
/// The Peano type must be spelled `Nat`: the lift renders constructors
/// as `0` / `+ 1` while signatures keep the source type name, so only
/// the builtin spelling elaborates — and only that emission shape was
/// hand-validated.
fn is_peano_eq_fn(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" || !fd.effects.is_empty() {
        return false;
    }
    let (p0, t0) = &fd.params[0];
    let (p1, t1) = &fd.params[1];
    if t0 != t1 {
        return false;
    }
    let Some(peano) = crate::codegen::proof_recognize::peano_type_named(ctx, t0.trim()) else {
        return false;
    };
    if peano.type_name.trim() != "Nat" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, base_body, q, succ_body)) = peano_match_split(body, &peano) else {
        return false;
    };
    if subj != p0.as_str() {
        return false;
    }
    // Z arm: match b { Z -> true, S _ -> false }.
    let Some((s2, b2, _, sb2)) = peano_match_split(base_body, &peano) else {
        return false;
    };
    if s2 != p1.as_str() || as_bool_lit(b2) != Some(true) || as_bool_lit(sb2) != Some(false) {
        return false;
    }
    // S(q) arm: match b { Z -> false, S(r) -> rec(q, r) }.
    let Some((s3, b3, r, sb3)) = peano_match_split(succ_body, &peano) else {
        return false;
    };
    if s3 != p1.as_str() || as_bool_lit(b3) != Some(false) {
        return false;
    }
    call_of(sb3).is_some_and(|(rc, ra)| {
        rc == fd.name && ra.len() == 2 && ident_of(&ra[0]) == Some(q) && ident_of(&ra[1]) == Some(r)
    })
}

/// 2-arm not-wrapper over a recognized Peano equality:
/// `match eq(p0, p1) { true -> false, false -> true }`. Returns the
/// wrapped equality fn's name.
fn neg_eq_wrapper(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" || !fd.effects.is_empty() {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    let (eq_name, eq_args) = call_of(subject)?;
    let eq_fd = ctx.fn_def_by_name(&eq_name, None)?;
    if !is_peano_eq_fn(eq_fd, ctx)
        || eq_args.len() != 2
        || ident_of(&eq_args[0]) != Some(fd.params[0].0.as_str())
        || ident_of(&eq_args[1]) != Some(fd.params[1].0.as_str())
        || arms.len() != 2
    {
        return None;
    }
    let mut t_to_f = false;
    let mut f_to_t = false;
    for arm in arms {
        match (&arm.pattern, as_bool_lit(&arm.body)) {
            (Pattern::Literal(Literal::Bool(true)), Some(false)) => t_to_f = true,
            (Pattern::Literal(Literal::Bool(false)), Some(true)) => f_to_t = true,
            _ => return None,
        }
    }
    (t_to_f && f_to_t).then_some(eq_name)
}

/// 2-arm Bool-or wrapper (`barbar`-shape):
/// `match p0 { true -> true, false -> p1 }`.
fn is_bool_or_wrapper(fd: &FnDef) -> bool {
    if fd.params.len() != 2
        || fd.return_type.trim() != "Bool"
        || !fd.effects.is_empty()
        || fd.params[0].1.trim() != "Bool"
        || fd.params[1].1.trim() != "Bool"
    {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    if ident_of(subject) != Some(fd.params[0].0.as_str()) || arms.len() != 2 {
        return false;
    }
    let mut ok_t = false;
    let mut ok_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => ok_t = as_bool_lit(&arm.body) == Some(true),
            Pattern::Literal(Literal::Bool(false)) => {
                ok_f = ident_of(&arm.body) == Some(fd.params[1].0.as_str())
            }
            _ => return false,
        }
    }
    ok_t && ok_f
}

/// `elem`-shape: `fn (k: Nat, l: List<Nat>) -> Bool` with body
/// `match l { [] -> false, [z, ..xs] -> or(eq(k, z), rec(k, xs)) }`.
/// Returns the or-wrapper fn's name.
fn elem_shape(fd: &FnDef, ctx: &CodegenContext, eq_fn: &str) -> Option<String> {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" || !fd.effects.is_empty() {
        return None;
    }
    let k = fd.params[0].0.as_str();
    if !is_list_of(&fd.params[1].1, &fd.params[0].1) {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let (subj, nil_body, hd, tl, cons_body) = list_match_split(body)?;
    if subj != fd.params[1].0.as_str() || as_bool_lit(nil_body) != Some(false) {
        return None;
    }
    let (or_name, or_args) = call_of(cons_body)?;
    if or_args.len() != 2 {
        return None;
    }
    let (c1, a1) = call_of(&or_args[0])?;
    if c1 != eq_fn || a1.len() != 2 || ident_of(&a1[0]) != Some(k) || ident_of(&a1[1]) != Some(hd) {
        return None;
    }
    let (c2, a2) = call_of(&or_args[1])?;
    if c2 != fd.name || a2.len() != 2 || ident_of(&a2[0]) != Some(k) || ident_of(&a2[1]) != Some(tl)
    {
        return None;
    }
    let or_fd = ctx.fn_def_by_name(&or_name, None)?;
    is_bool_or_wrapper(or_fd).then_some(or_name)
}

/// `count`-shape: `fn (k: Nat, l: List<Nat>) -> Nat` with body
/// `match l { [] -> Z, [z, ..ys] -> match eq(k, z)
///   { true -> S(rec(k, ys)), false -> rec(k, ys) } }`.
fn count_shape(fd: &FnDef, ctx: &CodegenContext, eq_fn: &str) -> bool {
    if fd.params.len() != 2 || !fd.effects.is_empty() {
        return false;
    }
    let (k, kt) = (&fd.params[0].0, &fd.params[0].1);
    let Some(peano) = crate::codegen::proof_recognize::peano_type_named(ctx, kt.trim()) else {
        return false;
    };
    if fd.return_type.trim() != peano.type_name || !is_list_of(&fd.params[1].1, kt) {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    if subj != fd.params[1].0.as_str() || !is_peano_base(nil_body, &peano) {
        return false;
    }
    let Expr::Match { subject, arms } = &cons_body.node else {
        return false;
    };
    let dispatch_ok = call_of(subject).is_some_and(|(c, a)| {
        c == eq_fn && a.len() == 2 && ident_of(&a[0]) == Some(k) && ident_of(&a[1]) == Some(hd)
    });
    if !dispatch_ok || arms.len() != 2 {
        return false;
    }
    let rec_ok = |e: &Spanned<Expr>| {
        call_of(e).is_some_and(|(rc, ra)| {
            rc == fd.name
                && ra.len() == 2
                && ident_of(&ra[0]) == Some(k.as_str())
                && ident_of(&ra[1]) == Some(tl)
        })
    };
    let mut ok_t = false;
    let mut ok_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                ok_t = peano_succ_of(&arm.body, &peano).is_some_and(rec_ok)
            }
            Pattern::Literal(Literal::Bool(false)) => ok_f = rec_ok(&arm.body),
            _ => return false,
        }
    }
    ok_t && ok_f
}

/// `insert`-shape: `fn (k: Nat, l: List<Nat>) -> List<Nat>` with body
/// `match l { [] -> [k], [z, ..xs] -> match le(k, z)
///   { true -> List.concat([k], l), false -> List.concat([z], rec(k, xs)) } }`
/// for SOME pure binary Bool dispatch `le` — the templates split on
/// the dispatch without consuming its meaning, so its shape is free.
fn insert_shape(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 2 || !fd.effects.is_empty() {
        return false;
    }
    let (k, kt) = (&fd.params[0].0, &fd.params[0].1);
    if crate::codegen::proof_recognize::peano_type_named(ctx, kt.trim()).is_none()
        || !is_list_of(&fd.params[1].1, kt)
        || !is_list_of(&fd.return_type, kt)
    {
        return false;
    }
    let l = fd.params[1].0.as_str();
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    if subj != l || !is_singleton_list_of_ident(nil_body, k) {
        return false;
    }
    let Expr::Match { subject, arms } = &cons_body.node else {
        return false;
    };
    let dispatch_ok = call_of(subject).is_some_and(|(c, a)| {
        a.len() == 2
            && ident_of(&a[0]) == Some(k.as_str())
            && ident_of(&a[1]) == Some(hd)
            && ctx.fn_def_by_name(&c, None).is_some_and(|le| {
                le.params.len() == 2 && le.return_type.trim() == "Bool" && le.effects.is_empty()
            })
    });
    if !dispatch_ok || arms.len() != 2 {
        return false;
    }
    fn concat_of(e: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
        let (c, a) = call_of(e)?;
        (c == "List.concat" && a.len() == 2).then(|| (&a[0], &a[1]))
    }
    let mut ok_t = false;
    let mut ok_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                ok_t = concat_of(&arm.body).is_some_and(|(h, t)| {
                    is_singleton_list_of_ident(h, k) && ident_of(t) == Some(l)
                })
            }
            Pattern::Literal(Literal::Bool(false)) => {
                ok_f = concat_of(&arm.body).is_some_and(|(h, t)| {
                    is_singleton_list_of_ident(h, hd)
                        && call_of(t).is_some_and(|(rc, ra)| {
                            rc == fd.name
                                && ra.len() == 2
                                && ident_of(&ra[0]) == Some(k.as_str())
                                && ident_of(&ra[1]) == Some(tl)
                        })
                })
            }
            _ => return false,
        }
    }
    ok_t && ok_f
}

/// `len`-shape measure: `fn (xs: List<T>) -> Nat` with body
/// `match xs { [] -> Z, [_, ..ys] -> S(rec(ys)) }`.
fn len_shape(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 1 || !fd.effects.is_empty() {
        return false;
    }
    let Some(peano) = crate::codegen::proof_recognize::peano_type_named(ctx, fd.return_type.trim())
    else {
        return false;
    };
    if peano.type_name.trim() != "Nat" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, _, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    subj == fd.params[0].0.as_str()
        && is_peano_base(nil_body, &peano)
        && peano_succ_of(cons_body, &peano).is_some_and(|inner| {
            call_of(inner).is_some_and(|(rc, ra)| {
                rc == fd.name && ra.len() == 1 && ident_of(&ra[0]) == Some(tl)
            })
        })
}

/// `append`-shape: `fn (xs: List<T>, ys: List<T>) -> List<T>` with body
/// `match xs { [] -> ys, [z, ..zs] -> List.concat([z], rec(zs, ys)) }`.
fn append_shape(fd: &FnDef, elem_ann: &str) -> bool {
    if fd.params.len() != 2
        || !fd.effects.is_empty()
        || !is_list_of(&fd.params[0].1, elem_ann)
        || !is_list_of(&fd.params[1].1, elem_ann)
        || !is_list_of(&fd.return_type, elem_ann)
    {
        return false;
    }
    let ys = fd.params[1].0.as_str();
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    subj == fd.params[0].0.as_str()
        && ident_of(nil_body) == Some(ys)
        && call_of(cons_body).is_some_and(|(c, a)| {
            c == "List.concat"
                && a.len() == 2
                && is_singleton_list_of_ident(&a[0], hd)
                && call_of(&a[1]).is_some_and(|(rc, ra)| {
                    rc == fd.name
                        && ra.len() == 2
                        && ident_of(&ra[0]) == Some(tl)
                        && ident_of(&ra[1]) == Some(ys)
                })
        })
}

/// `rev`-shape: `fn (xs: List<T>) -> List<T>` with body
/// `match xs { [] -> [], [y, ..ys] -> app(rec(ys), [y]) }` where `app`
/// is an [`append_shape`] fn over the same element type. Returns the
/// append fn's name.
fn rev_shape(fd: &FnDef, ctx: &CodegenContext, elem_ann: &str) -> Option<String> {
    if fd.params.len() != 1
        || !fd.effects.is_empty()
        || !is_list_of(&fd.params[0].1, elem_ann)
        || !is_list_of(&fd.return_type, elem_ann)
    {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let (subj, nil_body, hd, tl, cons_body) = list_match_split(body)?;
    if subj != fd.params[0].0.as_str()
        || !matches!(&nil_body.node, Expr::List(items) if items.is_empty())
    {
        return None;
    }
    let (app, args) = call_of(cons_body)?;
    if args.len() != 2
        || !call_of(&args[0])
            .is_some_and(|(rc, ra)| rc == fd.name && ra.len() == 1 && ident_of(&ra[0]) == Some(tl))
        || !is_singleton_list_of_ident(&args[1], hd)
    {
        return None;
    }
    let app_fd = ctx.fn_def_by_name(&app, None)?;
    append_shape(app_fd, elem_ann).then_some(app)
}

/// `zip`-shape: `fn (xs: List<A>, ys: List<B>) -> List<Tuple<A, B>>`
/// with body `match xs { [] -> [], [z, ..x2] -> match ys
///   { [] -> [], [x3, ..x4] -> List.concat([(z, x3)], rec(x2, x4)) } }`.
fn zip_shape(fd: &FnDef, elem_ann: &str) -> bool {
    let squash = |s: &str| s.chars().filter(|c| !c.is_whitespace()).collect::<String>();
    let pair_ann = format!("Tuple<{},{}>", squash(elem_ann), squash(elem_ann));
    if fd.params.len() != 2
        || !fd.effects.is_empty()
        || !is_list_of(&fd.params[0].1, elem_ann)
        || !is_list_of(&fd.params[1].1, elem_ann)
        || !is_list_of(&fd.return_type, &pair_ann)
    {
        return false;
    }
    let is_empty_list =
        |e: &Spanned<Expr>| matches!(&e.node, Expr::List(items) if items.is_empty());
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    if subj != fd.params[0].0.as_str() || !is_empty_list(nil_body) {
        return false;
    }
    let Some((subj2, nil2, hd2, tl2, cons2)) = list_match_split(cons_body) else {
        return false;
    };
    if subj2 != fd.params[1].0.as_str() || !is_empty_list(nil2) {
        return false;
    }
    call_of(cons2).is_some_and(|(c, a)| {
        c == "List.concat"
            && a.len() == 2
            && matches!(&a[0].node, Expr::List(items)
                if items.len() == 1
                    && matches!(&items[0].node, Expr::Tuple(pair)
                        if pair.len() == 2
                            && ident_of(&pair[0]) == Some(hd)
                            && ident_of(&pair[1]) == Some(hd2)))
            && call_of(&a[1]).is_some_and(|(rc, ra)| {
                rc == fd.name
                    && ra.len() == 2
                    && ident_of(&ra[0]) == Some(tl)
                    && ident_of(&ra[1]) == Some(tl2)
            })
    })
}

/// Given-named identifiers occurring in `e` — the variable sets of the
/// ACL2 free-variables gate. Returns `None` on any expression node the
/// walker does not positively recognize: the gate must never
/// under-collect on the `when` side, so unknown structure declines.
fn given_vars_in(
    e: &Spanned<Expr>,
    givens: &std::collections::BTreeSet<&str>,
    out: &mut std::collections::BTreeSet<String>,
) -> Option<()> {
    match &e.node {
        Expr::Literal(_) => Some(()),
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => {
            if givens.contains(n.as_str()) {
                out.insert(n.clone());
            }
            Some(())
        }
        Expr::Attr(base, _) => given_vars_in(base, givens, out),
        Expr::FnCall(callee, args) => {
            given_vars_in(callee, givens, out)?;
            args.iter().try_for_each(|a| given_vars_in(a, givens, out))
        }
        Expr::TailCall(data) => data
            .args
            .iter()
            .try_for_each(|a| given_vars_in(a, givens, out)),
        Expr::BinOp(_, l, r) => {
            given_vars_in(l, givens, out)?;
            given_vars_in(r, givens, out)
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => given_vars_in(inner, givens, out),
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .map_or(Some(()), |p| given_vars_in(p, givens, out)),
        Expr::List(items) | Expr::Tuple(items) => {
            items.iter().try_for_each(|a| given_vars_in(a, givens, out))
        }
        _ => None,
    }
}

/// `(eq_fn, negation route, lhs arg, rhs arg)` — the result of
/// [`bridge_premise`].
type BridgePremise<'a> = (
    String,
    Option<BridgeNeg>,
    &'a Spanned<Expr>,
    &'a Spanned<Expr>,
);

/// Normalize a bridge-shaped `when` to
/// `(eq_fn, negation route, lhs arg, rhs arg)`.
fn bridge_premise<'a>(when: &'a Spanned<Expr>, ctx: &CodegenContext) -> Option<BridgePremise<'a>> {
    let (callee, args) = call_of(when)?;
    if callee == "Bool.not" && args.len() == 1 {
        let (inner, in_args) = call_of(&args[0])?;
        let fd = ctx.fn_def_by_name(&inner, None)?;
        return (in_args.len() == 2 && is_peano_eq_fn(fd, ctx))
            .then(|| (inner, Some(BridgeNeg::BoolNot), &in_args[0], &in_args[1]));
    }
    if args.len() != 2 {
        return None;
    }
    let fd = ctx.fn_def_by_name(&callee, None)?;
    if is_peano_eq_fn(fd, ctx) {
        return Some((callee, None, &args[0], &args[1]));
    }
    let eq_fn = neg_eq_wrapper(fd, ctx)?;
    Some((eq_fn, Some(BridgeNeg::Wrapper(callee)), &args[0], &args[1]))
}

/// Validate one when-law against the bridge-premise figures. Mirrors
/// the lane discipline: exact hand-validated constellations only,
/// everything else declines (the law stays bounded, manifest bytes
/// untouched).
pub(super) fn classify_bridge_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<BridgePlan> {
    let when = law.when.as_ref()?;
    let given_names: std::collections::BTreeSet<&str> =
        law.givens.iter().map(|g| g.name.as_str()).collect();
    if given_names.len() != law.givens.len() {
        return None;
    }

    // ACL2 free-variables gate: vars(when) ⊆ vars(lhs). A premise
    // variable unbound by the conclusion's match side would make
    // conditional rewriting guess — decline instead.
    let mut when_vars = std::collections::BTreeSet::new();
    given_vars_in(when, &given_names, &mut when_vars)?;
    let mut lhs_vars = std::collections::BTreeSet::new();
    given_vars_in(&law.lhs, &given_names, &mut lhs_vars)?;
    if !when_vars.is_subset(&lhs_vars) {
        return None;
    }

    let (eq_fn, neg, a, b) = bridge_premise(when, ctx)?;
    let given_type = |name: &str| -> Option<&str> {
        law.givens
            .iter()
            .find(|g| g.name == name)
            .map(|g| g.type_name.as_str())
    };
    let (lhs_callee, lhs_args) = call_of(&law.lhs)?;
    if lhs_callee.rsplit('.').next()? != vb.fn_name {
        return None;
    }

    // ---- ZipRevLenEq: when eq(len(xs), len(ys)) ---------------------
    if let (Some((len_a, la)), Some((len_b, lb))) = (call_of(a), call_of(b)) {
        if law.givens.len() != 2
            || len_a != len_b
            || la.len() != 1
            || lb.len() != 1
            || law
                .givens
                .iter()
                .any(|g| BRIDGE_RESERVED_ZIPREV.contains(&g.name.as_str()))
        {
            return None;
        }
        if neg.is_some() {
            return None;
        }
        let xs = law.givens[0].name.as_str();
        let ys = law.givens[1].name.as_str();
        if ident_of(&la[0]) != Some(xs) || ident_of(&lb[0]) != Some(ys) {
            return None;
        }
        let xs_ty = law.givens[0].type_name.as_str();
        if law.givens[1].type_name != xs_ty {
            return None;
        }
        // Element type from `List<T>`.
        let elem_ann = {
            let squash: String = xs_ty.chars().filter(|c| !c.is_whitespace()).collect();
            squash.strip_prefix("List<")?.strip_suffix('>')?.to_string()
        };
        let len_fd = ctx.fn_def_by_name(&len_a, None)?;
        if !len_shape(len_fd, ctx) || !is_list_of(xs_ty, &elem_ann) {
            return None;
        }
        // lhs: zip(rev(xs), rev(ys)); rhs: revPair(zip(xs, ys)).
        if lhs_args.len() != 2 {
            return None;
        }
        let (rev_a, ra) = call_of(&lhs_args[0])?;
        let (rev_b, rb) = call_of(&lhs_args[1])?;
        if rev_a != rev_b
            || ra.len() != 1
            || rb.len() != 1
            || ident_of(&ra[0]) != Some(xs)
            || ident_of(&rb[0]) != Some(ys)
        {
            return None;
        }
        let (rev_pair, rp_args) = call_of(&law.rhs)?;
        if rp_args.len() != 1 {
            return None;
        }
        let (zip_inner, zi) = call_of(&rp_args[0])?;
        if zip_inner != lhs_callee
            || zi.len() != 2
            || ident_of(&zi[0]) != Some(xs)
            || ident_of(&zi[1]) != Some(ys)
        {
            return None;
        }
        let zip_fd = ctx.fn_def_by_name(&lhs_callee, None)?;
        if !zip_shape(zip_fd, &elem_ann) {
            return None;
        }
        let rev_fd = ctx.fn_def_by_name(&rev_a, None)?;
        let append_fn = rev_shape(rev_fd, ctx, &elem_ann)?;
        let pair_ann = format!("Tuple<{elem_ann}, {elem_ann}>");
        let rev_pair_fd = ctx.fn_def_by_name(&rev_pair, None)?;
        let append_pair_fn = rev_shape(rev_pair_fd, ctx, &pair_ann)?;
        // Compound element types must parenthesize inside `List _` /
        // binder positions of the rendered templates.
        let elem_ty = {
            let t = super::super::types::type_annotation_to_lean(&elem_ann);
            if t.contains(' ') { format!("({t})") } else { t }
        };
        return Some(BridgePlan::ZipRevLenEq {
            eq_fn,
            len_fn: len_a,
            zip_fn: lhs_callee,
            rev_fn: rev_a,
            rev_pair_fn: rev_pair,
            append_fn,
            append_pair_fn,
            elem_ty,
            xs: xs.to_string(),
            ys: ys.to_string(),
        });
    }

    // ---- insert/count figures: premise args are plain Peano givens --
    let x = ident_of(a)?;
    let y = ident_of(b)?;
    if x == y {
        // `eq(x, x)` / `neq(x, x)`: the positive figure's `subst`
        // cannot eliminate a self-equality — decline (zero cost)
        // instead of a noisy tolerated build failure.
        return None;
    }
    let xt = given_type(x)?;
    if given_type(y)? != xt
        || crate::codegen::proof_recognize::peano_type_named(ctx, xt.trim()).is_none()
        || law.givens.len() != 3
        || law
            .givens
            .iter()
            .any(|g| BRIDGE_RESERVED_INSERT.contains(&g.name.as_str()))
    {
        return None;
    }
    if lhs_args.len() != 2 || ident_of(&lhs_args[0]) != Some(x) {
        return None;
    }

    // count(x, List.concat(z_list, [y])) = count(x, z_list) — prop_76.
    if let Some((cc, ca)) = call_of(&lhs_args[1])
        && cc == "List.concat"
    {
        // Only the negated premise is a validated figure over the
        // concat-singleton conclusion shape.
        let neg = neg?;
        if ca.len() != 2 || !is_singleton_list_of_ident(&ca[1], y) {
            return None;
        }
        let zs = ident_of(&ca[0])?;
        if !is_list_of(given_type(zs)?, xt) {
            return None;
        }
        let count_fd = ctx.fn_def_by_name(&lhs_callee, None)?;
        if !count_shape(count_fd, ctx, &eq_fn) {
            return None;
        }
        let (rc, ra) = call_of(&law.rhs)?;
        return (rc == lhs_callee
            && ra.len() == 2
            && ident_of(&ra[0]) == Some(x)
            && ident_of(&ra[1]) == Some(zs))
        .then(|| BridgePlan::NeqCountAppendSingleton {
            eq_fn,
            neg,
            count_fn: lhs_callee,
            n: x.to_string(),
            m: y.to_string(),
            xs: zs.to_string(),
        });
    }

    // elem/count over insert: lhs = f(x, insert(y, z)).
    let (insert_fn, ins_args) = call_of(&lhs_args[1])?;
    if ins_args.len() != 2 || ident_of(&ins_args[0]) != Some(y) {
        return None;
    }
    let z = ident_of(&ins_args[1])?;
    if !is_list_of(given_type(z)?, xt) {
        return None;
    }
    let insert_fd = ctx.fn_def_by_name(&insert_fn, None)?;
    if !insert_shape(insert_fd, ctx) {
        return None;
    }
    let head_fd = ctx.fn_def_by_name(&lhs_callee, None)?;
    match neg {
        // when eq(x, y): elem(x, insert(y, z)) = true — prop_46.
        None => {
            let or_fn = elem_shape(head_fd, ctx, &eq_fn)?;
            (as_bool_lit(&law.rhs) == Some(true)).then(|| BridgePlan::EqElemInsert {
                eq_fn,
                elem_fn: lhs_callee,
                or_fn,
                insert_fn,
                x: x.to_string(),
                y: y.to_string(),
                z: z.to_string(),
            })
        }
        // when neq(x, y): f(x, insert(y, z)) = f(x, z) — prop_47 /
        // lemma_19 (elem) and lemma_21 (count).
        Some(neg) => {
            let (rc, ra) = call_of(&law.rhs)?;
            if rc != lhs_callee
                || ra.len() != 2
                || ident_of(&ra[0]) != Some(x)
                || ident_of(&ra[1]) != Some(z)
            {
                return None;
            }
            if let Some(or_fn) = elem_shape(head_fd, ctx, &eq_fn) {
                return Some(BridgePlan::NeqElemInsert {
                    eq_fn,
                    neg,
                    elem_fn: lhs_callee,
                    or_fn,
                    insert_fn,
                    x: x.to_string(),
                    y: y.to_string(),
                    z: z.to_string(),
                });
            }
            count_shape(head_fd, ctx, &eq_fn).then(|| BridgePlan::NeqCountInsert {
                eq_fn,
                neg,
                count_fn: lhs_callee,
                insert_fn,
                x: x.to_string(),
                y: y.to_string(),
                z: z.to_string(),
            })
        }
    }
}

/// The validated zip-rev (`ZipRevLenEq`) proof, factored out of
/// [`render_bridge_law`] so the SAME proof text backs both the lane twin
/// and the main-path conditional emit. All fn-name params are already
/// Lean-mangled; `prefix` scopes the six support-lemma names (the lane
/// passes its `_universal` theorem, the main path its `<fn>_<law>` base);
/// `sab` is the test-only sabotage suffix (`""` on the main path). The
/// body is emitted at base indentation — each caller adds its own.
#[allow(clippy::too_many_arguments)]
fn zip_rev_supports_body(
    eq: &str,
    len: &str,
    zip: &str,
    rev: &str,
    revp: &str,
    app: &str,
    appp: &str,
    a_ty: &str,
    xs: &str,
    ys: &str,
    prefix: &str,
    sab: &str,
) -> (Vec<String>, String) {
    let supports = vec![
        format!(
            r#"private theorem {prefix}_bridge_eq : ∀ (a b : Nat), _root_.{eq} a b = true → a = b := by
  intro a
  induction a with
  | zero =>
    intro b h
    cases b with
    | zero => rfl
    | succ y => simp [_root_.{eq}] at h
  | succ x ih =>
    intro b h
    cases b with
    | zero => simp [_root_.{eq}] at h
    | succ y =>
      have hx := ih y (by simpa [_root_.{eq}] using h)
      omega
"#
        ),
        format!(
            r#"private theorem {prefix}_bridge_refl : ∀ (a : Nat), _root_.{eq} a a = true := by
  intro a
  induction a with
  | zero => rfl
  | succ x ih => simpa [_root_.{eq}] using ih
"#
        ),
        // Measure homomorphism over append — the premise-stepping
        // arithmetic below rides on it.
        format!(
            r#"private theorem {prefix}_len_append : ∀ (xs ys : List {a_ty}),
    _root_.{len} (_root_.{app} xs ys) = _root_.{len} xs + _root_.{len} ys := by
  intro xs
  induction xs with
  | nil => intro ys; simp [_root_.{app}, _root_.{len}]
  | cons z zs ih =>
    intro ys
    simp only [_root_.{app}, List.singleton_append, _root_.{len}, ih]
    omega
"#
        ),
        format!(
            r#"private theorem {prefix}_len_rev : ∀ (xs : List {a_ty}), _root_.{len} (_root_.{rev} xs) = _root_.{len} xs := by
  intro xs
  induction xs with
  | nil => simp [_root_.{rev}]
  | cons y ys ih =>
    simp only [_root_.{rev}, {prefix}_len_append, _root_.{len}, ih]
"#
        ),
        // Premise-driven shape inversion of the non-induction
        // variable: `len ys = 0 → ys = []`.
        format!(
            r#"private theorem {prefix}_len_zero : ∀ (ys : List {a_ty}), _root_.{len} ys = 0 → ys = [] := by
  intro ys h
  cases ys with
  | nil => rfl
  | cons y ys =>
    simp only [_root_.{len}] at h
    exact absurd h (by omega)
"#
        ),
        // The snoc-distribution aux lemma (zip over append-singleton
        // under length equality) — emitted from the validated template,
        // never as a sorry. Its own premise threads by per-step
        // stepping and vacuous discharge.
        format!(
            r#"private theorem {prefix}_snoc (x y : {a_ty}) : ∀ (as bs : List {a_ty}),
    _root_.{len} as = _root_.{len} bs →
    _root_.{zip} (_root_.{app} as [x]) (_root_.{app} bs [y])
      = _root_.{appp} (_root_.{zip} as bs) [(x, y)] := by
  intro as
  induction as with
  | nil =>
    intro bs h
    have hb : bs = [] := {prefix}_len_zero bs (by simp only [_root_.{len}] at h; omega)
    subst hb
    simp [_root_.{app}, _root_.{zip}, _root_.{appp}]
  | cons a as ih =>
    intro bs h
    cases bs with
    | nil =>
      simp only [_root_.{len}] at h
      exact absurd h (by omega)
    | cons b bs =>
      have h' : _root_.{len} as = _root_.{len} bs := by simp only [_root_.{len}] at h; omega
      simp only [_root_.{app}, List.singleton_append, _root_.{zip}, _root_.{appp}, ih bs h']
"#
        ),
    ];
    let body = format!(
        r#"intro {xs}{sab}
induction {xs} with
| nil =>
  intro {ys} h
  have h0 : _root_.{len} {ys} = 0 := by
    have hh := {prefix}_bridge_eq (_root_.{len} []) (_root_.{len} {ys}) h
    simp only [_root_.{len}] at hh
    omega
  have hy : {ys} = [] := {prefix}_len_zero {ys} h0
  subst hy
  simp [_root_.{rev}, _root_.{zip}, _root_.{revp}]
| cons z x2 ih =>
  intro {ys} h
  cases {ys} with
  | nil =>
    have hh := {prefix}_bridge_eq (_root_.{len} (z :: x2)) (_root_.{len} []) h
    simp only [_root_.{len}] at hh
    exact absurd hh (by omega)
  | cons y x4 =>
    have hlen : _root_.{len} x2 = _root_.{len} x4 := by
      have hh := {prefix}_bridge_eq (_root_.{len} (z :: x2)) (_root_.{len} (y :: x4)) h
      simp only [_root_.{len}] at hh
      omega
    have hrevlen : _root_.{len} (_root_.{rev} x2) = _root_.{len} (_root_.{rev} x4) := by
      rw [{prefix}_len_rev, {prefix}_len_rev]; exact hlen
    have hih : _root_.{zip} (_root_.{rev} x2) (_root_.{rev} x4) = _root_.{revp} (_root_.{zip} x2 x4) := by
      apply ih
      rw [hlen]
      exact {prefix}_bridge_refl (_root_.{len} x4)
    calc _root_.{zip} (_root_.{rev} (z :: x2)) (_root_.{rev} (y :: x4))
        = _root_.{zip} (_root_.{app} (_root_.{rev} x2) [z]) (_root_.{app} (_root_.{rev} x4) [y]) := by
          simp only [_root_.{rev}]
      _ = _root_.{appp} (_root_.{zip} (_root_.{rev} x2) (_root_.{rev} x4)) [(z, y)] :=
          {prefix}_snoc z y (_root_.{rev} x2) (_root_.{rev} x4) hrevlen
      _ = _root_.{appp} (_root_.{revp} (_root_.{zip} x2 x4)) [(z, y)] := by rw [hih]
      _ = _root_.{revp} (_root_.{zip} (z :: x2) (y :: x4)) := by
          simp only [_root_.{zip}, _root_.{revp}, List.singleton_append]"#
    );
    (supports, body)
}

/// True iff `law` is the zip-rev length-equality figure. The main-path
/// `omit_domain` driver reads this to flip the law-class marker to
/// `universal` for exactly this shape.
pub(in crate::codegen::lean) fn is_zip_rev_bridge(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    matches!(
        classify_bridge_law(vb, law, ctx),
        Some(BridgePlan::ZipRevLenEq { .. })
    )
}

/// Build the zip-rev universal proof (support lemmas + body) for the MAIN
/// path, `prefix`-scoping the support-lemma names so they never collide
/// with the lane twin or carry the `_law_` substring the credit audit
/// keys on. `None` for any non-zip-rev law.
pub(in crate::codegen::lean) fn zip_rev_universal_proof(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    prefix: &str,
) -> Option<(Vec<String>, String)> {
    let Some(BridgePlan::ZipRevLenEq {
        eq_fn,
        len_fn,
        zip_fn,
        rev_fn,
        rev_pair_fn,
        append_fn,
        append_pair_fn,
        elem_ty,
        xs,
        ys,
    }) = classify_bridge_law(vb, law, ctx)
    else {
        return None;
    };
    Some(zip_rev_supports_body(
        &aver_name_to_lean(&eq_fn),
        &aver_name_to_lean(&len_fn),
        &aver_name_to_lean(&zip_fn),
        &aver_name_to_lean(&rev_fn),
        &aver_name_to_lean(&rev_pair_fn),
        &aver_name_to_lean(&append_fn),
        &aver_name_to_lean(&append_pair_fn),
        &elem_ty,
        &aver_name_to_lean(&xs),
        &aver_name_to_lean(&ys),
        prefix,
        "",
    ))
}

/// Render one bridge-premise lane law: the validated proof template
/// for `plan`, support lemmas included, into a single hashed module.
/// Statement built by the SAME `law_theorem_prop` as the manifest
/// theorem with `omit_domain` — when-premise kept, sampled-domain
/// disjunctions dropped. Zero `sorry` tokens by construction.
pub(super) fn render_bridge_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    plan: &BridgePlan,
    entry_root: &str,
    entry_content: &str,
    sabotage: bool,
) -> Option<LaneLawFile> {
    let emit = |e: &Spanned<Expr>| emit_expr_legacy(e, ctx, None).replace('\n', " ");
    let fn_lean = aver_name_to_lean(&vb.fn_name);
    let law_lean = aver_name_to_lean(&law.name);
    let theorem_base = format!("{fn_lean}_law_{law_lean}");
    let theorem = format!("{theorem_base}_universal");

    let lhs_template = emit(&law.lhs);
    let rhs_template = emit(&law.rhs);
    let when_template = emit(law.when.as_ref()?);
    let lifted = std::collections::HashMap::new();
    let (prop, bounded) = super::super::toplevel::law_theorem_prop(
        law,
        ctx,
        &lhs_template,
        &rhs_template,
        Some(&when_template),
        &lifted,
        true,
    );
    debug_assert!(!bounded);
    let quant_params = law
        .givens
        .iter()
        .map(|g| {
            format!(
                "({} : {})",
                aver_name_to_lean(&g.name),
                super::super::types::type_annotation_to_lean(&g.type_name)
            )
        })
        .collect::<Vec<_>>()
        .join(" ");
    let intro_givens = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect::<Vec<_>>()
        .join(" ");

    let sab = if sabotage {
        // TEST-ONLY (`AVER_PROOF_LANE_SABOTAGE`): an unknown
        // identifier makes this module's build fail hard — the lane
        // must absorb it with zero effect on budgets and neighbors.
        "\nexact averLaneSabotageInjectedByTest"
    } else {
        ""
    };

    // Use-side Bool→Prop inversion bridge: follows the predicate's own
    // recursion (mismatch arms close from the contradicted equations,
    // the step arm feeds the IH through `simpa`).
    let bridge_eq_lemma = |eq: &str| {
        format!(
            r#"private theorem {theorem}_bridge_eq : ∀ (a b : Nat), _root_.{eq} a b = true → a = b := by
  intro a
  induction a with
  | zero =>
    intro b h
    cases b with
    | zero => rfl
    | succ y => simp [_root_.{eq}] at h
  | succ x ih =>
    intro b h
    cases b with
    | zero => simp [_root_.{eq}] at h
    | succ y =>
      have hx := ih y (by simpa [_root_.{eq}] using h)
      omega
"#
        )
    };
    // REINTRODUCTION bridge (`eq a a = true`) — without it the main
    // proof cannot instantiate its own induction hypothesis.
    let bridge_refl_lemma = |eq: &str| {
        format!(
            r#"private theorem {theorem}_bridge_refl : ∀ (a : Nat), _root_.{eq} a a = true := by
  intro a
  induction a with
  | zero => rfl
  | succ x ih => simpa [_root_.{eq}] using ih
"#
        )
    };
    // Negated-premise normalization: derive `heq : eq x y = false`
    // from `h_when` by cases on the Bool — the `true` case refutes
    // the rendered premise (`!eq` or the not-wrapper unfolding).
    let neg_norm = |eq: &str, neg: &BridgeNeg, x: &str, y: &str| {
        let on_true = match neg {
            BridgeNeg::BoolNot => "rw [hc] at h_when\n    simp at h_when".to_string(),
            BridgeNeg::Wrapper(w) => {
                format!("simp [_root_.{}, hc] at h_when", aver_name_to_lean(w))
            }
        };
        format!(
            "have heq : _root_.{eq} {x} {y} = false := by\n  cases hc : _root_.{eq} {x} {y}\n  · rfl\n  · {on_true}"
        )
    };

    let (supports, body): (Vec<String>, String) = match plan {
        BridgePlan::ZipRevLenEq {
            eq_fn,
            len_fn,
            zip_fn,
            rev_fn,
            rev_pair_fn,
            append_fn,
            append_pair_fn,
            elem_ty,
            xs,
            ys,
        } => zip_rev_supports_body(
            &aver_name_to_lean(eq_fn),
            &aver_name_to_lean(len_fn),
            &aver_name_to_lean(zip_fn),
            &aver_name_to_lean(rev_fn),
            &aver_name_to_lean(rev_pair_fn),
            &aver_name_to_lean(append_fn),
            &aver_name_to_lean(append_pair_fn),
            elem_ty,
            &aver_name_to_lean(xs),
            &aver_name_to_lean(ys),
            &theorem,
            sab,
        ),
        BridgePlan::EqElemInsert {
            eq_fn,
            elem_fn,
            or_fn,
            insert_fn,
            x,
            y,
            z,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let elem = aver_name_to_lean(elem_fn);
            let or = aver_name_to_lean(or_fn);
            let ins = aver_name_to_lean(insert_fn);
            let x = aver_name_to_lean(x);
            let y = aver_name_to_lean(y);
            let z = aver_name_to_lean(z);
            let supports = vec![bridge_eq_lemma(&eq), bridge_refl_lemma(&eq)];
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
have heq : {x} = {y} := {theorem}_bridge_eq {x} {y} h_when
subst heq
induction {z} with
| nil => simp [_root_.{ins}, _root_.{elem}, _root_.{or}, {theorem}_bridge_refl]
| cons c cs ih =>
  simp only [_root_.{ins}]
  split
  · simp [_root_.{elem}, _root_.{or}, {theorem}_bridge_refl, List.singleton_append]
  · simp only [List.singleton_append, _root_.{elem}, ih, _root_.{or}]
    split <;> simp"#
            );
            (supports, body)
        }
        BridgePlan::NeqElemInsert {
            eq_fn,
            neg,
            elem_fn,
            or_fn,
            insert_fn,
            x,
            y,
            z,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let elem = aver_name_to_lean(elem_fn);
            let or = aver_name_to_lean(or_fn);
            let ins = aver_name_to_lean(insert_fn);
            let heq = neg_norm(&eq, neg, &aver_name_to_lean(x), &aver_name_to_lean(y));
            let z = aver_name_to_lean(z);
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
{heq}
induction {z} with
| nil => simp [_root_.{ins}, _root_.{elem}, _root_.{or}, heq]
| cons c cs ih =>
  simp only [_root_.{ins}]
  split
  · simp [_root_.{elem}, _root_.{or}, heq, List.singleton_append]
  · simp only [List.singleton_append, _root_.{elem}, ih]"#
            );
            (Vec::new(), body)
        }
        BridgePlan::NeqCountInsert {
            eq_fn,
            neg,
            count_fn,
            insert_fn,
            x,
            y,
            z,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let count = aver_name_to_lean(count_fn);
            let ins = aver_name_to_lean(insert_fn);
            let heq = neg_norm(&eq, neg, &aver_name_to_lean(x), &aver_name_to_lean(y));
            let z = aver_name_to_lean(z);
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
{heq}
induction {z} with
| nil => simp [_root_.{ins}, _root_.{count}, heq]
| cons c cs ih =>
  simp only [_root_.{ins}]
  split
  · simp [_root_.{count}, heq, List.singleton_append]
  · simp only [List.singleton_append, _root_.{count}, ih]"#
            );
            (Vec::new(), body)
        }
        BridgePlan::NeqCountAppendSingleton {
            eq_fn,
            neg,
            count_fn,
            n,
            m,
            xs,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let count = aver_name_to_lean(count_fn);
            let heq = neg_norm(&eq, neg, &aver_name_to_lean(n), &aver_name_to_lean(m));
            let xs = aver_name_to_lean(xs);
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
{heq}
induction {xs} with
| nil => simp [_root_.{count}, heq]
| cons c cs ih => simp only [List.cons_append, _root_.{count}, ih]"#
            );
            (Vec::new(), body)
        }
    };

    let mut content = String::new();
    content.push_str(&format!(
        "-- Aver when-universal quarantine lane — verify law {}.{}\n\
         -- NOT part of the counted default build. Built by a separate,\n\
         -- failure-tolerated per-law `lake build` invocation; credited only\n\
         -- on per-declaration `#print axioms` evidence (whitelist: propext,\n\
         -- Classical.choice, Quot.sound). This module carries no honest-\n\
         -- floor fallback: a non-closing proof is a tolerated build failure\n\
         -- (the law stays bounded), never a counted warning.\n",
        vb.fn_name, law.name,
    ));
    content.push_str(&format!("import {entry_root}\n\n"));
    content.push_str("set_option linter.unusedVariables false\n\n");
    for support in &supports {
        content.push_str(support);
        content.push('\n');
    }
    content.push_str(&format!(
        "{}{} {}\n",
        super::super::LAW_CLASS_MARKER_PREFIX,
        theorem,
        super::super::LAW_CLASS_UNIVERSAL
    ));
    content.push_str(&format!(
        "theorem {theorem} : ∀ {quant_params}, {prop} := by\n"
    ));
    for line in body.lines() {
        if line.is_empty() {
            content.push('\n');
        } else {
            content.push_str("  ");
            content.push_str(line);
            content.push('\n');
        }
    }

    // Prop-form corollary, derived from the twin above with the fixed
    // Bool/Prop bridge tactic — same module, no separate credit.
    content.push('\n');
    let given_names: Vec<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    content.push_str(&super::shared::render_prop_corollary(
        &theorem_base,
        &quant_params,
        &given_names,
        law,
        ctx,
        &format!("{lhs_template} = {rhs_template}"),
    ));

    // L2 of the iron guard: the lane grammar has no sorry carrier.
    debug_assert!(
        !content.contains("sorry"),
        "universal-lane module must not contain a sorry token"
    );

    let module = lane_module_id(&theorem_base, &content, entry_content);
    Some(LaneLawFile {
        label: format!("{}.{}", vb.fn_name, law.name),
        companion: format!("{theorem_base}_prop"),
        theorem,
        theorem_base,
        module,
        imports: Vec::new(),
        content,
    })
}
