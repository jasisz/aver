//! Container (motive2-lift) structural induction.
//!
//! Closes an UNCONDITIONAL law about a rose-tree-style ADT fn whose recursion
//! runs *through a `List<Self>` field* — a mutual SCC of EXACTLY two pure fns in
//! the forced-pair idiom: an ADT walker `f` (`match t | Leaf => .. | Node kids
//! => .. fList kids ..`) and its canonical list-walker sibling `fList` (`match ts
//! | [] => base | x :: rest => .. f x .. fList rest ..`). Plain constructor
//! induction (`Tree.rec`) gives the node case no useful hypothesis about the
//! child list, so the legacy structural arm declines this exact shape
//! (`has_indirect_variants`). The functional-induction principle `f.induct`
//! carries the right hypotheses, but Lean never INFERS the sibling motive
//! (`motive2`); the emitter must supply it. This arm computes that lift purely
//! from the claim AST and closes the four `f.induct` cases with the engine
//! closer portfolio.
//!
//! Two claim families are handled, both MEASURED kernel-clean against the real
//! WF-lowered defs (`prompts/probe-artifacts/motive2-lift`):
//!   * INEQUALITY `f(t) >= c` (emitted as the Bool-equation `(f t >= c) = true`):
//!     `motive2 ts := (fList ts >= b0) = true`, where `b0` is READ OFF the
//!     walker's own base case (`fList [] = b0`) — NEVER copied from the claim's
//!     bound `c` (copying `1` would be false at `[]`).
//!   * EQUATIONAL `f(g(t)) = f(t)` for a numeric outer walker `f` and a transform
//!     `g` that injects `List.reverse` at its node constructor: `motive2 ts :=
//!     fList (gList ts) = fList ts` (R_naive; the dropped reverse resurfaces in
//!     the node case, closed by the reusable container lemma `fList_reverse`).
//!
//! Container-lemma family (emitted only when the node case needs it, keyed on the
//! walker × list-op, name-blind — the lemmas mention no domain constant):
//! `{uid}_{walker}_append` (`w (a ++ b) = w a + w b`) and its dependent
//! `{uid}_{walker}_reverse` (`w a.reverse = w a`), each the fixed measured
//! skeleton (plain `List` induction + `simp [walker, List.reverse_cons, append,
//! ih]` + an `omega` tail for the numeric walker).
//!
//! Fail-closed like the transparent-chain arm (#634): the whole induction sits
//! under a `first | (..) | sorry` floor (with the `AVERSPEC_SORRY:<id>` trace +
//! `record_probed` under a probe), and every support lemma is floored too, so a
//! mis-recognized shape degrades to an honest sorry — bounded, never a red build.
//! The statement is already the `∀`-universal form the emitter builds for any
//! unconditional recursive-ADT law, so no sampled-domain flip is involved; the
//! arm only fills the proof body that was otherwise a `simp [defs] <;> done`
//! sorry.

use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use super::{AutoProof, shared};
use crate::ast::{
    Expr, FnDef, Literal, Pattern, Spanned, TypeDef, TypeVariant, VerifyBlock, VerifyLaw,
};
use crate::codegen::CodegenContext;
use crate::codegen::lean::tactic_ir::{Tactic, speculative};

/// A recognized forced walker pair `(f, fList)` over an ADT `T` with a single
/// `Node(List<T>)` recursive constructor and a leaf constructor.
struct Pair {
    /// Source name of the ADT walker (`f`).
    f_src: String,
    /// Source name of the list-walker sibling (`fList`).
    flist_src: String,
    /// The recursive ADT type name.
    adt: String,
}

enum Claim {
    /// `(f t >= c) = true` — motive2 bound read off `fList`'s base literal.
    Inequality { base: i64 },
    /// `f(g(t)) = f(t)`, `g` a reverse-injecting transform with sibling `gList`.
    Equational { g_src: String, glist_src: String },
}

struct Recognized {
    pair: Pair,
    claim: Claim,
}

fn law_id(vb: &VerifyBlock, law: &VerifyLaw) -> String {
    format!("{}.{}", vb.fn_name, law.name)
}

pub(in crate::codegen::lean) fn emit_container_induction_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let Recognized { pair, claim } = recognize(vb, law, ctx)?;
    if intro_names.len() != 1 {
        return None;
    }
    let f = shared::entry_qualified_lean_name(ctx, &pair.f_src);
    let flist = shared::entry_qualified_lean_name(ctx, &pair.flist_src);
    let induct = format!("{f}.induct");
    let uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );

    let (support, motive2, cases) = match &claim {
        Claim::Inequality { base } => {
            // motive2 ts := (fList ts >= b0) = true; b0 from the walker base case.
            let motive2 = format!("fun ts => ({flist} ts >= {base}) = true");
            // f leaf / f node / fList nil / fList cons. `at *` bridges the Bool
            // hypotheses (the ih) to Prop so `omega` can use them.
            let cases = vec![
                format!("| case1 => simp [{f}]"),
                format!("| case2 kids ih => simp [{f}] at *; omega"),
                format!("| case3 => simp [{flist}]"),
                format!("| case4 x rest ih_x ih_rest => simp [{flist}] at *; omega"),
            ];
            (Vec::new(), motive2, cases)
        }
        Claim::Equational { g_src, glist_src } => {
            let g = shared::entry_qualified_lean_name(ctx, g_src);
            let glist = shared::entry_qualified_lean_name(ctx, glist_src);
            // motive2 ts := fList (gList ts) = fList ts  (R_naive).
            let motive2 = format!("fun ts => {flist} ({glist} ts) = {flist} ts");
            // Container lemmas (numeric walker × ++/reverse), floored so a
            // mis-recognition degrades to sorry rather than a red build.
            let append = format!("{uid}_{}_append", aver_name_to_lean(&pair.flist_src));
            let reverse = format!("{uid}_{}_reverse", aver_name_to_lean(&pair.flist_src));
            let support = vec![
                format!(
                    "theorem {append} : ∀ (a b : List {ty}), {flist} (a ++ b) = {flist} a + {flist} b := by\n  intro a b\n  first\n    | (induction a with\n      | nil => simp [{flist}]\n      | cons h t ih => simp [{flist}, ih]; omega)\n    | sorry",
                    ty = adt_lean(&pair.adt),
                ),
                format!(
                    "theorem {reverse} : ∀ (a : List {ty}), {flist} a.reverse = {flist} a := by\n  intro a\n  first\n    | (induction a with\n      | nil => simp [{flist}]\n      | cons h t ih => simp [List.reverse_cons, {append}, {flist}, ih]; omega)\n    | sorry",
                    ty = adt_lean(&pair.adt),
                ),
            ];
            let cases = vec![
                format!("| case1 => simp [{f}, {g}]"),
                format!("| case2 kids ih => simp [{f}, {g}, {reverse}, ih]"),
                format!("| case3 => simp [{flist}, {glist}]"),
                format!("| case4 x rest ih_x ih_rest => simp [{flist}, {glist}]; omega"),
            ];
            (support, motive2, cases)
        }
    };

    let id = law_id(vb, law);
    let floor = if speculative::probing() {
        speculative::record_probed(&id);
        format!("(trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "sorry".to_string()
    };

    let t = &intro_names[0];
    let mut induction_block = vec![format!(
        "(induction {t} using {induct}\n        (motive2 := {motive2}) with"
    )];
    for case in cases {
        induction_block.push(format!("      {case}"));
    }
    // Close the induction paren on the last case line already; append the floor.
    let last = induction_block.pop().unwrap();
    induction_block.push(format!("{last})"));
    let block = induction_block.join("\n");
    let body_line = format!("first\n    | {block}\n    | {floor}");

    Some(AutoProof {
        support_lines: support,
        body: Tactic::raw(super::intro_then(intro_names, vec![body_line])),
        replaces_theorem: false,
    })
}

/// Lean name of the entry-module ADT. The proof is emitted inside the same
/// namespace, so `List Tree` uses the bare capitalized type name.
fn adt_lean(adt: &str) -> String {
    aver_name_to_lean(adt)
}

fn recognize(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<Recognized> {
    // Unconditional laws only (the measured idiom); a `when` premise is the hard
    // conditional-inductive family, out of scope.
    if law.when.is_some() {
        return None;
    }
    // Exactly one given — the ADT induction variable.
    if law.givens.len() != 1 {
        return None;
    }
    let given = &law.givens[0];
    let t_name = &given.name;

    let pair = recognize_pair(&vb.fn_name, ctx)?;
    if pair.adt != given.type_name.trim() {
        return None;
    }

    // INEQUALITY: `(f t >= c) = true`.
    if matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true)))
        && let Expr::BinOp(crate::ast::BinOp::Gte, l, r) = &law.lhs.node
        && is_call_on(l, &pair.f_src, t_name)
        && matches!(&r.node, Expr::Literal(Literal::Int(_)))
    {
        let flist_fd = shared::find_fn_def_by_call_name(ctx, &pair.flist_src)?;
        // Numeric walker: `fList` returns `Int` and its `[]` arm is a literal.
        if flist_fd.return_type.trim() != "Int" {
            return None;
        }
        let base = list_walker_base_int(flist_fd)?;
        return Some(Recognized {
            pair,
            claim: Claim::Inequality { base },
        });
    }

    // EQUATIONAL: `f(g(t)) = f(t)`.
    if let (Expr::FnCall(rc, ra), Expr::FnCall(lc, la)) = (&law.rhs.node, &law.lhs.node)
        && shared::expr_dotted_name(rc).as_deref() == Some(pair.f_src.as_str())
        && ra.len() == 1
        && shared::is_ident(&ra[0], t_name)
        && shared::expr_dotted_name(lc).as_deref() == Some(pair.f_src.as_str())
        && la.len() == 1
        && let Expr::FnCall(gc, ga) = &la[0].node
        && ga.len() == 1
        && shared::is_ident(&ga[0], t_name)
        && let Some(g_src) = shared::expr_dotted_name(gc)
        && g_src != pair.f_src
    {
        // Outer walker must be numeric (the measured eq_naive shape); a
        // list-returning outer (involution / naturality) is a different lemma
        // family and deliberately declines to bounded here.
        let f_fd = shared::find_fn_def_by_call_name(ctx, &pair.f_src)?;
        if f_fd.return_type.trim() != "Int" {
            return None;
        }
        // `g` is its own forced pair over the same ADT, and injects `List.reverse`
        // at its node constructor (what makes `fList_reverse` the needed lemma).
        let g_pair = recognize_pair(&g_src, ctx)?;
        if g_pair.adt != pair.adt {
            return None;
        }
        let g_fd = shared::find_fn_def_by_call_name(ctx, &g_src)?;
        if g_fd.return_type.trim() != pair.adt {
            return None;
        }
        if !node_arm_injects_reverse(g_fd, &g_pair.flist_src, ctx) {
            return None;
        }
        return Some(Recognized {
            pair,
            claim: Claim::Equational {
                g_src,
                glist_src: g_pair.flist_src,
            },
        });
    }

    None
}

/// Recognize the forced walker pair whose ADT walker is `f_src`: `f` matches on an
/// ADT `T` (exactly two variants — a fieldless leaf and a `Node(List<T>)`) and
/// calls a sibling `fList` on the child list; `fList` is the canonical list-walker
/// over `List<T>` calling back `f`; and the mutual SCC is EXACTLY `{f, fList}`.
fn recognize_pair(f_src: &str, ctx: &CodegenContext) -> Option<Pair> {
    // Entry-module only: the generated `.induct` name belongs to the entry
    // module's definition, not to a dependency with the same bare name.
    if ctx.active_module_scope().is_some() {
        return None;
    }
    let f_fd = shared::find_fn_def(ctx, f_src)?;
    if !f_fd.effects.is_empty() || f_fd.params.len() != 1 {
        return None;
    }
    let (t_param, adt) = &f_fd.params[0];
    let adt = adt.trim().to_string();
    if is_dep_module_fn(ctx, f_src) {
        return None;
    }

    // ADT must be a recursive sum type with exactly a leaf + a `Node(List<T>)`.
    let variants = find_sum_variants(ctx, &adt)?;
    leaf_and_list_node(variants, &adt)?;

    // f's body: a single match on the ADT param, whose node arm binds the
    // `List<T>` field and calls a sibling on it.
    let f_body = shared::body_terminal_expr(f_fd.body.as_ref())?;
    let Expr::Match { subject, arms } = &f_body.node else {
        return None;
    };
    if !shared::is_ident(subject, t_param) {
        return None;
    }
    if arms.len() != 2 {
        return None;
    }
    // Canonical arm order — leaf (fieldless) first, `Node(List<T>)` second. The
    // joint `f.induct` numbers its cases in source-arm order (the `case1..case4`
    // layout the closers assume takes leaf/nil first); a node-first source shifts
    // the numbering and sorry-floors, so decline fail-closed rather than emit a
    // mismatched proof.
    let leaf_first = matches!(&arms[0].pattern, Pattern::Constructor(_, b) if b.is_empty());
    let node_second = matches!(&arms[1].pattern, Pattern::Constructor(_, b) if b.len() == 1);
    if !leaf_first || !node_second {
        return None;
    }
    // Find the node arm (constructor pattern binding one field) and the fn it
    // calls on that field.
    let flist_src = node_arm_sibling(arms, ctx)?;
    if flist_src == f_src {
        return None;
    }
    let flist_fd = shared::find_fn_def(ctx, &flist_src)?;
    if !flist_fd.effects.is_empty() || flist_fd.params.len() != 1 {
        return None;
    }
    if is_dep_module_fn(ctx, &flist_src) {
        return None;
    }
    // fList is a canonical list-walker over `List<T>` (nil + cons) calling back f.
    if !is_list_walker(flist_fd, &adt, f_src) {
        return None;
    }

    // Mutual SCC is exactly {f, fList}.
    let scc = mutual_scc(f_src, ctx);
    let want: BTreeSet<String> = [f_src.to_string(), flist_src.clone()].into_iter().collect();
    if scc != want {
        return None;
    }

    Some(Pair {
        f_src: f_src.to_string(),
        flist_src,
        adt,
    })
}

/// The node arm's sibling: the arm whose pattern is a constructor binding exactly
/// one field, whose body calls a single user fn on that field binder. Returns the
/// callee's source name.
fn node_arm_sibling(arms: &[crate::ast::MatchArm], ctx: &CodegenContext) -> Option<String> {
    for arm in arms {
        let Pattern::Constructor(_ctor, binders) = &arm.pattern else {
            continue;
        };
        if binders.len() != 1 {
            continue;
        }
        let field = &binders[0];
        if let Some(name) = shared::call_on_binder(&arm.body, field, ctx) {
            return Some(name);
        }
    }
    None
}

/// Whether `g`'s node arm applies `List.reverse` to a call to `glist` — the
/// reverse injection that forces the `fList_reverse` container lemma.
fn node_arm_injects_reverse(g_fd: &FnDef, glist_src: &str, ctx: &CodegenContext) -> bool {
    let Some(body) = shared::body_terminal_expr(g_fd.body.as_ref()) else {
        return false;
    };
    let Expr::Match { arms, .. } = &body.node else {
        return false;
    };
    fn find_reverse_of(e: &Spanned<Expr>, glist: &str) -> bool {
        if let Expr::FnCall(callee, args) = &e.node
            && shared::expr_dotted_name(callee).as_deref() == Some("List.reverse")
            && args.len() == 1
            && let Expr::FnCall(inner, _) = &args[0].node
            && shared::expr_dotted_name(inner).as_deref() == Some(glist)
        {
            return true;
        }
        shared::child_exprs(e)
            .into_iter()
            .any(|c| find_reverse_of(c, glist))
    }
    let _ = ctx;
    arms.iter().any(|arm| find_reverse_of(&arm.body, glist_src))
}

/// Whether `fd` is a canonical list-walker over `List<adt>`: single `List<adt>`
/// param, body a match with a `[]` arm and a `[x, ..rest]` arm that calls back
/// `f` and recurses on the tail.
fn is_list_walker(fd: &FnDef, adt: &str, f_src: &str) -> bool {
    let (param, ty) = &fd.params[0];
    if ty.trim() != format!("List<{adt}>") {
        return false;
    }
    let Some(body) = shared::body_terminal_expr(fd.body.as_ref()) else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    if !shared::is_ident(subject, param) || arms.len() != 2 {
        return false;
    }
    // Canonical order: `[]` first, `[x, ..rest]` second (matches the `case3/case4`
    // layout the closers assume; see recognize_pair's arm-order guard).
    if !matches!(arms[0].pattern, Pattern::EmptyList)
        || !matches!(arms[1].pattern, Pattern::Cons(..))
    {
        return false;
    }
    let mut saw_nil = false;
    let mut cons_calls_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => saw_nil = true,
            Pattern::Cons(head, _tail) => {
                // The cons arm applies `f` to the head element.
                cons_calls_f = shared::calls_fn_on_ident(&arm.body, f_src, head);
            }
            _ => return false,
        }
    }
    saw_nil && cons_calls_f
}

/// The `Int` literal of a numeric list-walker's `[]` base arm.
fn list_walker_base_int(fd: &FnDef) -> Option<i64> {
    let body = shared::body_terminal_expr(fd.body.as_ref())?;
    let Expr::Match { arms, .. } = &body.node else {
        return None;
    };
    for arm in arms {
        if matches!(arm.pattern, Pattern::EmptyList)
            && let Expr::Literal(Literal::Int(n)) = &arm.body.node
        {
            return Some(*n);
        }
    }
    None
}

/// The mutual-recursion SCC of `f_src` in the user pure-fn call graph: every `g`
/// with `f →* g` and `g →* f`.
fn mutual_scc(f_src: &str, ctx: &CodegenContext) -> BTreeSet<String> {
    let reach_from = |start: &str| -> BTreeSet<String> {
        let mut seen = BTreeSet::new();
        let mut stack = vec![start.to_string()];
        while let Some(cur) = stack.pop() {
            for callee in shared::direct_user_calls(&cur, ctx) {
                if seen.insert(callee.clone()) {
                    stack.push(callee);
                }
            }
        }
        seen
    };
    let forward = reach_from(f_src);
    forward
        .iter()
        .filter(|g| reach_from(g).contains(f_src))
        .cloned()
        .chain(std::iter::once(f_src.to_string()))
        .collect()
}

fn find_sum_variants<'a>(ctx: &'a CodegenContext, name: &str) -> Option<&'a Vec<TypeVariant>> {
    ctx.modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .find_map(|td| match td {
            TypeDef::Sum {
                name: n, variants, ..
            } if n == name => Some(variants),
            _ => None,
        })
}

/// Confirms a two-variant recursive sum whose recursion is exactly one
/// `Node(List<T>)` constructor plus a fieldless leaf — the case1..case4 layout
/// the closers assume. Returns `Some(())` iff both variants are present.
fn leaf_and_list_node(variants: &[TypeVariant], adt: &str) -> Option<()> {
    if variants.len() != 2 {
        return None;
    }
    let list_ty = format!("List<{adt}>");
    let mut leaf = false;
    let mut node = false;
    for v in variants {
        if v.fields.is_empty() {
            leaf = true;
        } else if v.fields.len() == 1 && v.fields[0].trim() == list_ty {
            node = true;
        }
    }
    (leaf && node).then_some(())
}

fn is_dep_module_fn(ctx: &CodegenContext, source_name: &str) -> bool {
    ctx.modules
        .iter()
        .any(|m| m.fn_defs.iter().any(|fd| fd.name == source_name))
}

/// `expr` is a single-arg call `callee(<ident>)` with `callee` = `fn_src`.
fn is_call_on(expr: &Spanned<Expr>, fn_src: &str, ident: &str) -> bool {
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    args.len() == 1
        && shared::is_ident(&args[0], ident)
        && shared::expr_dotted_name(callee).as_deref() == Some(fn_src)
}
