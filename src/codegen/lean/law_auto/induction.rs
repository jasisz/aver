/// Structural induction auto-proof strategy for recursive sum types.
///
/// This module intentionally supports only the fully structural case:
/// - one `given` is a recursive sum type
/// - recursive occurrences are direct fields of the parent type
/// - no `when` premise
///
/// Variants that recurse only through containers such as `List<T>` or
/// `Map<K, T>` are rejected here and must fall back to non-universal proof
/// paths until a genuinely generic indirect-recursion engine exists.
use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use super::super::shared::to_lower_first;
use super::super::tactic_ir::{InductionArm, Tactic};
use super::AutoProof;
use super::shared::law_simp_defs;
use crate::ast::{TypeDef, TypeVariant, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// `first | (<simp>; done) | (<simp>; omega) | sorry` — the per-arm portfolio
/// shared by the Peano bridge theorems (`<simp>` is e.g. `simp [f]` /
/// `simp [f, ih]`). A structured `First` so `--minimize` collapses the arm to
/// whichever of `; done` / `; omega` actually closed; renders inline,
/// byte-identical to the legacy string.
fn simp_done_omega_first(simp: &str) -> Tactic {
    Tactic::First(vec![
        Tactic::Leaf(format!("{simp}; done")),
        Tactic::Leaf(format!("{simp}; omega")),
        Tactic::Sorry,
    ])
}

/// Lean renderer for the backend-neutral rev anti-homomorphism recognizer
/// (`crate::codegen::proof_recognize::collect_rev_ops_in_law` — shared with the
/// Dafny backend; despite the module path it returns source-name structs only).
/// Produces the proved append-nil-right, associativity, rev-distribution, and
/// rev-INVOLUTION theorems (prepended as `support_lines`) and the distribution
/// and involution lemma names to add to the induction's `simp` set. List<Int>
/// folds lower to clean `def … termination_by`, so these close kernel-clean
/// (`#print axioms = [propext]`) — the Lean counterpart of the Dafny rev
/// strategy, SAME recognizer.
fn lean_rev_support(
    ops: &[crate::codegen::proof_recognize::RevOp],
    law_uid: &str,
) -> (Vec<String>, Vec<String>) {
    let mut support = Vec::new();
    let mut simp_extra = Vec::new();
    for op in ops {
        let r = aver_name_to_lean(&op.rev);
        let a = aver_name_to_lean(&op.append);
        let nilr = format!("{law_uid}_{a}_nilR");
        let assoc = format!("{law_uid}_{a}_assoc");
        let dist = format!("{law_uid}_{r}_revDist");
        let invol = format!("{law_uid}_{r}_revInvol");
        support.push(format!(
            "theorem {nilr} : ∀ (xa : List Int), {a} xa [] = xa := by\n  intro xa; induction xa with\n  | nil => simp [{a}]\n  | cons h t ih => simp [{a}, ih]"
        ));
        support.push(format!(
            "theorem {assoc} : ∀ (xa xb xc : List Int), {a} ({a} xa xb) xc = {a} xa ({a} xb xc) := by\n  intro xa xb xc; induction xa with\n  | nil => simp [{a}]\n  | cons h t ih => simp [{a}, ih]"
        ));
        support.push(format!(
            "theorem {dist} : ∀ (xa xb : List Int), {r} ({a} xa xb) = {a} ({r} xb) ({r} xa) := by\n  intro xa xb; induction xa with\n  | nil => simp [{r}, {a}, {nilr}]\n  | cons h t ih => simp [{r}, {a}, ih, {assoc}]"
        ));
        // rev INVOLUTION (`rev (rev x) = x`) — the missing rung for the
        // double-rev anti-homomorphism family (TIP prod/prop_11
        // `rev (app (rev x) (rev y)) = app y x`). Once the law's `simp only`
        // distributes `rev` over the inner append (the `revDist` rule above),
        // the residual `app (rev (rev y)) (rev (rev x))` only collapses to
        // `app y x` when each doubled `rev` cancels — exactly this lemma. Its
        // own cons case is proved by the SAME ladder: `simp [rev, append,
        // revDist, ih]` rewrites `rev (rev (h :: t))` = `rev (app (rev t) [h])`
        // = `app (rev [h]) (rev (rev t))` = `app [h] t` = `h :: t` under the IH.
        // PROVED, not trusted — a misrecognized rev fails this proof and
        // degrades to an honest `sorry` (caught by the `#print axioms` gate),
        // never a false theorem. Terminating as a simp rewrite (each use strips
        // two `rev`s), so adding its name to the simp set cannot loop; it only
        // ever ADDS closures to the rev-family ladders.
        support.push(format!(
            "theorem {invol} : ∀ (xa : List Int), {r} ({r} xa) = xa := by\n  intro xa; induction xa with\n  | nil => simp [{r}]\n  | cons h t ih => simp [{r}, {a}, {dist}, ih]"
        ));
        // The append fn name is needed in the main induction's simp set too
        // (revDist rewrites into `append`, which must then unfold).
        simp_extra.push(a);
        simp_extra.push(dist);
        simp_extra.push(invol);
    }
    (support, simp_extra)
}

/// Lean renderer for the backend-neutral canonical-Peano operation recognizers
/// (`collect_nat_arith_ops_in_law` + `collect_nat_compare_ops_in_law`). For each
/// user fn the law invokes that IS a standard Peano `+`/`-`/`*` or `≤`/`<`, emit
/// a kernel-CHECKED bridge lemma (proved by induction over the lifted builtin
/// `Nat`) and return its name for the law's `simp only` set. Rewriting the user
/// op to the host builtin hands the goal to `omega` (for `+`/`-`/`≤`/`<`, which
/// it decides) or to core `Nat.mul_*` lemmas (for `*`). The bridge is PROVED,
/// not trusted: a misrecognized op makes the bridge proof fail (degrading to an
/// honest `sorry` caught by the sorry-gate), never a false theorem. Names are
/// law-scoped (`law_uid`) so multiple laws in one module don't collide.
///
/// Returns `(support_theorems, simp_lemma_names, bridged_fn_lean_names)`. The
/// simp set carries the bridge names plus, when a `*` bridge is present, the
/// core distributivity / associativity lemmas (`*` is nonlinear so `omega`
/// can't close it and core Lean has no `ring`; pure-commutativity laws fall
/// through to `sorry`). `bridged_fn_lean_names` are the Lean names of the
/// lifted user fns themselves — callers building a combined `simp only` set
/// must EXCLUDE those defs (mixing a fn's def equations with its `= a + b`
/// bridge in one call leaves the rewrite stuck).
///
/// `extra_fns` extends the scan beyond the law's own call graph — the
/// discovery feedback loop passes the fns its committed lemmas mention, so an
/// op a homomorphism lemma INTRODUCES (e.g. `plus` rewriting into a law that
/// only said `double`) still gets its bridge. Empty for the plain path.
pub(super) fn lean_nat_lift_support(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    law_uid: &str,
    extra_fns: &BTreeSet<String>,
) -> (Vec<String>, Vec<String>, BTreeSet<String>) {
    use crate::codegen::proof_recognize::NatArithKind;
    let mut support = Vec::new();
    let mut simp_extra = Vec::new();
    let mut bridged_fns: BTreeSet<String> = BTreeSet::new();

    let mut arith = crate::codegen::proof_recognize::collect_nat_arith_ops_in_law(law, ctx);
    for op in crate::codegen::proof_recognize::collect_nat_arith_ops_for_names(extra_fns, ctx) {
        if !arith.iter().any(|o| o.fn_name == op.fn_name) {
            arith.push(op);
        }
    }
    // The `*` bridge proof rewrites with the `+` bridge, so resolve the addition
    // op's bridge name up front and emit Add/Sub before Mul.
    let add_bridge_name = arith
        .iter()
        .find(|op| op.kind == NatArithKind::Add)
        .map(|op| format!("{law_uid}_{}_isNatAdd", aver_name_to_lean(&op.fn_name)));
    let mut has_mul = false;
    let ordered = arith
        .iter()
        .filter(|o| o.kind != NatArithKind::Mul)
        .chain(arith.iter().filter(|o| o.kind == NatArithKind::Mul));
    for op in ordered {
        let f = aver_name_to_lean(&op.fn_name);
        bridged_fns.insert(f.clone());
        match op.kind {
            NatArithKind::Add => {
                let name = format!("{law_uid}_{f}_isNatAdd");
                // Structured per-arm `first | (simp; done) | (simp; omega) | sorry`
                // so `--minimize` collapses each arm to its winner; inline render
                // is byte-identical to the legacy string.
                let body = Tactic::Seq(vec![
                    Tactic::Leaf("intro a b".to_string()),
                    Tactic::Induction {
                        target: "a".to_string(),
                        arms: vec![
                            InductionArm {
                                pattern: "zero".to_string(),
                                body: simp_done_omega_first(&format!("simp [{f}]")),
                            },
                            InductionArm {
                                pattern: "succ k ih".to_string(),
                                body: simp_done_omega_first(&format!("simp [{f}, ih]")),
                            },
                        ],
                    },
                ]);
                support.push(super::support_theorem(
                    &format!("theorem {name} : ∀ a b, {f} a b = a + b := by"),
                    body,
                ));
                simp_extra.push(name);
            }
            NatArithKind::Sub => {
                let name = format!("{law_uid}_{f}_isNatSub");
                support.push(format!(
                    "theorem {name} : ∀ a b, {f} a b = a - b := by\n  intro a b\n  induction a generalizing b with\n  | zero => first | (simp [{f}]; done) | (simp [{f}]; omega) | sorry\n  | succ k ih => cases b with\n    | zero => first | (simp [{f}]; done) | (simp [{f}]; omega) | sorry\n    | succ j => first | (simp [{f}, ih]; done) | (simp [{f}, ih]; omega) | sorry"
                ));
                simp_extra.push(name);
            }
            NatArithKind::Mul => {
                // `times a b = a * b`; the succ case rewrites `times (k+1) b =
                // b + times k b` (def) → `b + k*b` (ih) → `(k+1)*b` via
                // `Nat.succ_mul` + commuting the sum. Needs the `+` bridge.
                let Some(add_name) = &add_bridge_name else {
                    continue;
                };
                let name = format!("{law_uid}_{f}_isNatMul");
                let body = Tactic::Seq(vec![
                    Tactic::Leaf("intro a b".to_string()),
                    Tactic::Induction {
                        target: "a".to_string(),
                        arms: vec![
                            InductionArm {
                                pattern: "zero".to_string(),
                                body: simp_done_omega_first(&format!("simp [{f}]")),
                            },
                            InductionArm {
                                pattern: "succ k ih".to_string(),
                                body: Tactic::First(vec![
                                    Tactic::Leaf(format!(
                                        "simp only [{f}, {add_name}, ih, Nat.succ_mul, Nat.add_comm]"
                                    )),
                                    Tactic::Sorry,
                                ]),
                            },
                        ],
                    },
                ]);
                support.push(super::support_theorem(
                    &format!("theorem {name} : ∀ a b, {f} a b = a * b := by"),
                    body,
                ));
                simp_extra.push(name);
                has_mul = true;
            }
        }
    }

    let mut compare = crate::codegen::proof_recognize::collect_nat_compare_ops_in_law(law, ctx);
    for op in crate::codegen::proof_recognize::collect_nat_compare_ops_for_names(extra_fns, ctx) {
        if !compare.iter().any(|o| o.fn_name == op.fn_name) {
            compare.push(op);
        }
    }
    for op in compare {
        let f = aver_name_to_lean(&op.fn_name);
        bridged_fns.insert(f.clone());
        // ONE bridge for every recognized Peano Bool relation — `≤`, `<`, `=`
        // (and any later `≥`/`>`/`≠`): the Prop-equality (propext)
        // `(f a b = true) = (a R b)`, so `simp only` rewrites the Bool goal
        // `f _ _ = true` straight into the Prop relation for `omega`. All three
        // close by the SAME double-peel — induct on the argument the fn
        // destructures first (its DRIVER: `b` for `<`, `a` for `≤`/`=`),
        // case-split the other in both arms, `simp [f(, ih)]`. They differ only
        // in the target relation and which arg drives, both read off the kind;
        // the proof template is shared. Sound-by-floor: a misrecognized shape
        // fails the bridge proof and lands on the `sorry`, never a false theorem.
        let name = format!("{law_uid}_{f}_{}", op.kind.bridge_suffix());
        let prop = op.kind.prop_op();
        let (driver, passenger) = if op.kind.induct_on_second() {
            ("b", "a")
        } else {
            ("a", "b")
        };
        support.push(format!(
            "theorem {name} : ∀ a b, ({f} a b = true) = (a {prop} b) := by\n  intro a b\n  induction {driver} generalizing {passenger} with\n  | zero => cases {passenger} <;> first | (simp [{f}]) | sorry\n  | succ k ih => cases {passenger} <;> first | (simp [{f}, ih]) | sorry"
        ));
        simp_extra.push(name);
    }

    // `*` is nonlinear: `omega` treats `a*b` as an opaque atom, and core Lean has
    // no `ring`. Add the core distributivity / associativity lemmas so laws of
    // that shape normalize to a form `omega` (over atoms) or `simp` then closes.
    // Pure-commutativity (`a*b = b*a`) is NOT in this set (it would loop) and
    // honestly falls through to the induction fallback / `sorry`.
    if has_mul {
        for lemma in [
            "Nat.mul_add",
            "Nat.add_mul",
            "Nat.mul_assoc",
            "Nat.succ_mul",
            "Nat.mul_succ",
            "Nat.mul_one",
            "Nat.one_mul",
            "Nat.mul_zero",
            "Nat.zero_mul",
        ] {
            simp_extra.push(lemma.to_string());
        }
    }

    (support, simp_extra, bridged_fns)
}

/// Recognize a canonical structural-EQUALITY fn: `f(a: T, b: T) -> Bool` over a
/// user sum type `T`, defined as the nested diagonal match
/// `match a { C0 -> match b { C0 -> true ; _ -> false } ; Ci(a') -> match b {
/// Ci(b') -> f(a', b') ; _ -> false } ; … }` (the `eqNat` shape). For such a fn
/// `∀ a, f a a = true` is provable by `induction a <;> simp_all [f]`, so it is
/// safe to emit and CITE the reflexivity lemma. Conservative by construction: a
/// fn that is NOT this exact shape is rejected, so the emitted `_refl` lemma is
/// only ever the genuinely-provable one (no `sorry`-floored lemma reaches a
/// closer's simp set, where it would taint a closing proof with `sorryAx`).
///
/// Returns `true` on match.
///
/// SOUNDNESS — the DIAGONAL is checked, not merely the shape. A near-miss like a
/// not-equal fn (`neq Z Z = false`) has the same nested-match SHAPE, but
/// `neq a a = true` is FALSE; emitting and citing its (necessarily `sorry`-
/// floored) `_refl` would taint a closing proof. So each outer constructor arm
/// `Ci` must, on its SAME-constructor inner arm, return `true` (a leaf
/// constructor) or the recursive self-call (a recursive constructor) — never
/// `false`. That makes `∀ a, f a a = true` genuinely provable by `induction a
/// <;> simp_all [f]`, so the emitted lemma is kernel-clean.
fn recognize_refl_eq_fn(fd: &crate::ast::FnDef, ctx: &CodegenContext) -> bool {
    use crate::ast::{Expr, Literal, Pattern};
    if fd.return_type.trim() != "Bool" || fd.params.len() != 2 {
        return false;
    }
    let (p0, t0) = (&fd.params[0].0, fd.params[0].1.trim());
    let (p1, t1) = (&fd.params[1].0, fd.params[1].1.trim());
    if t0 != t1 || find_sum_type(ctx, t0).is_none() {
        return false;
    }
    // Body must be a single `match p0 { … }`.
    let Some(body) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    if !matches!(&subject.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == p0) {
        return false;
    }
    // Outer-arm constructor name (handles `Nat.Z` / `Nat.S(z)` and `Z`/`S(z)`).
    let ctor_of = |p: &Pattern| -> Option<String> {
        match p {
            Pattern::Constructor(name, _) => Some(name.clone()),
            _ => None,
        }
    };
    let is_recursive_self = |e: &Expr| -> bool {
        match e {
            Expr::FnCall(callee, args) => {
                super::shared::expr_dotted_name(callee).as_deref() == Some(fd.name.as_str())
                    && args.len() == 2
            }
            // The diagonal recursive call sits in tail position, so the TCO pass
            // may have rewritten it to `TailCall` before this runs.
            Expr::TailCall(tc) => tc.target == fd.name && tc.args.len() == 2,
            _ => false,
        }
    };
    arms.iter().all(|outer| {
        let Some(octor) = ctor_of(&outer.pattern) else {
            return false;
        };
        let Expr::Match {
            subject: inner_subj,
            arms: inner_arms,
        } = &outer.body.node
        else {
            return false;
        };
        if !matches!(&inner_subj.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == p1) {
            return false;
        }
        // Off-diagonal arms may be anything Bool-shaped; the DIAGONAL arm (same
        // constructor as the outer) MUST return `true` or recurse.
        inner_arms.iter().all(|inner| {
            let diagonal = ctor_of(&inner.pattern).as_deref() == Some(octor.as_str());
            match &inner.body.node {
                Expr::Literal(Literal::Bool(b)) => !diagonal || *b,
                e if is_recursive_self(e) => true,
                // A non-recursive, non-Bool inner body is fine ONLY off-diagonal.
                _ => !diagonal,
            }
        })
    })
}

/// Proven reflexivity support lemmas (`∀ a, f a a = true`) for every recognized
/// structural-equality fn in the law's simp cone, plus their names for the rung
/// closer's simp set. Each lemma is proved by `induction a <;> simp_all [f]`
/// under a `first | … | sorry` floor (the floor only fires if recognition were
/// wrong — it cannot, by `recognize_refl_eq_fn`'s construction — so the lemma is
/// kernel-clean in practice and the cited name never taints a closing proof).
fn lean_refl_support(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    law_uid: &str,
) -> (Vec<String>, Vec<String>) {
    let mut support = Vec::new();
    let mut names = Vec::new();
    for src_name in super::shared::law_simp_source_names(ctx, vb, law) {
        let Some(fd) = ctx.fn_def_by_name(&src_name, ctx.active_module_scope().as_deref()) else {
            continue;
        };
        if !recognize_refl_eq_fn(fd, ctx) {
            continue;
        }
        let f = aver_name_to_lean(&src_name);
        let name = format!("{law_uid}_{f}_refl");
        let body = super::intro_then_first(
            &["a".to_string()],
            vec![
                format!("induction a <;> simp_all [{f}]; done"),
                format!("induction a <;> simp_all [{f}]"),
            ],
        );
        support.push(super::support_theorem(
            &format!("theorem {name} : ∀ a, {f} a a = true := by"),
            body,
        ));
        names.push(name);
    }
    (support, names)
}

/// Source names of the program fns mentioned by the given pinned discovered
/// lemmas (`ProofStrategy::SimpOverLemmas` names → `ctx.discovered_lemmas`
/// texts → token scan against every pure program fn's Lean name). Drives the
/// lemma-aware extension of [`lean_nat_lift_support`].
fn discovered_lemma_source_fns(ctx: &CodegenContext, names: &[String]) -> BTreeSet<String> {
    use std::collections::BTreeMap;
    if names.is_empty() {
        return BTreeSet::new();
    }
    let lean_index: BTreeMap<String, String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| crate::codegen::common::is_pure_fn(fd))
        .map(|fd| (aver_name_to_lean(&fd.name), fd.name.clone()))
        .collect();
    ctx.discovered_lemmas
        .iter()
        .filter(|l| names.contains(&l.name))
        .flat_map(|l| crate::codegen::lemma_discovery::mentioned_fns(&l.text, &lean_index))
        .collect()
}

/// The pinned lemmas usable as `simp` rewrite rules, as ready-to-emit simp
/// set entries (`lemma_discovery::simp_entries`): `name` for a Forward lemma
/// (program-fn-headed LHS), `← name` for a Reversed one (builtin-headed LHS,
/// program-fn-headed RHS — e.g. the trivia `(x0 ++ x1) = append x0 x1`,
/// which reversed UNFOLDS the opaque wrapper into `++` so a forward
/// homomorphism can fire), minus loop-prone forward/reversed combinations
/// (a simp cycle is an uncatchable maxHeartbeats BUILD error). The pin
/// carries EVERY in-scope lemma so embedded proofs keep their dependencies;
/// only this selection joins the simp sets.
fn discovered_simp_entries(ctx: &CodegenContext, names: &[String]) -> Vec<String> {
    if names.is_empty() {
        return Vec::new();
    }
    let program_fns: BTreeSet<String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| crate::codegen::common::is_pure_fn(fd))
        .map(|fd| aver_name_to_lean(&fd.name))
        .collect();
    let pinned: Vec<&crate::codegen::lemma_discovery::CommittedLemma> = ctx
        .discovered_lemmas
        .iter()
        .filter(|l| names.contains(&l.name))
        .collect();
    crate::codegen::lemma_discovery::simp_entries(&pinned, &program_fns)
}

/// Verbatim texts of the pinned discovered lemmas THIS law is responsible for
/// emitting. A lemma is embedded exactly once per generated file: by the
/// first law (in `proof_ir.law_theorems` order, which mirrors entry-item
/// order) whose `SimpOverLemmas` pin carries its name — later laws only
/// reference the name, which Lean resolves against the earlier definition.
/// Embedding re-proves the lemma inside the same `lake build`, so a stale
/// committed lemma fails the build loudly (the replay soundness guard) rather
/// than being trusted via the cone hash.
fn discovered_support_lines(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
    names: &[String],
) -> Vec<String> {
    if names.is_empty() {
        return Vec::new();
    }
    let Some(fn_id) = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name))
    else {
        return Vec::new();
    };
    let mut out = Vec::new();
    for name in names {
        let first_user = ctx.proof_ir.law_theorems.iter().find(|t| {
            matches!(&t.strategy,
                crate::ir::ProofStrategy::SimpOverLemmas(ns) if ns.contains(name))
        });
        let this_law_is_first =
            first_user.is_some_and(|t| t.fn_id == fn_id && t.law_name == law.name);
        if this_law_is_first
            && let Some(lemma) = ctx.discovered_lemmas.iter().find(|l| &l.name == name)
        {
            out.push(lemma.text.clone());
        }
    }
    out
}

/// Lean names of every pure program fn — the membership universe for
/// orientation / scope / source-fn analysis.
fn program_fn_lean_names(ctx: &CodegenContext) -> BTreeSet<String> {
    ctx.modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| crate::codegen::common::is_pure_fn(fd))
        .map(|fd| aver_name_to_lean(&fd.name))
        .collect()
}

/// Map a cone fn to the lean name a DEP law's statement would render it
/// as: dep-module fns are NAMESPACE-QUALIFIED (`Lib.qrev`), entry fns
/// stay bare. The module is resolved by pointer-eq against
/// `ctx.modules[i].fn_defs` (the cone holds borrows into those exact
/// slices — the same technique `recursive_pure_fn_names` uses), so the
/// cross-file gate compares QUALIFIED identity, not a bare-name collision.
fn qualified_cone_name(fd: &crate::ast::FnDef, ctx: &CodegenContext) -> String {
    let bare = aver_name_to_lean(&fd.name);
    match ctx
        .modules
        .iter()
        .find(|m| m.fn_defs.iter().any(|d| std::ptr::eq(d, fd)))
    {
        Some(m) => format!("{}.{}", m.prefix, bare),
        None => bare,
    }
}

/// Decide whether a dependency module's proven `verify … law` is
/// admissible into a consumer law's pool — the CROSS-FILE half of the
/// `earlier_law_lemmas` gate, factored out so the CONSUME side
/// (`earlier_law_lemmas`) and the EMIT side (`admitted_dep_law_theorems`,
/// which decides which dep-law theorems to emit at all) make the IDENTICAL
/// decision. Returns the namespace-qualified theorem name + statement on
/// admit, `None` on reject.
///
/// Admissibility — fail-closed at admission. VISIBILITY (gate 0) is enforced
/// upstream in `collect_verify_laws`: a law about a non-exposed subject never
/// reaches `module.verify_laws`, so this function only ever sees exposed laws.
/// The two gates checked HERE:
///   1. SHAPE — `law_as_lemma_statement` states it as a universal `∀`-rewrite
///      (declines no-givens / when-premise / singleton-const-rhs / fuel-
///      bounded — the shapes the dep export would NOT emit a universal
///      theorem for, so a citation could never resolve).
///   2. CONE — the law's statement, in QUALIFIED identity, sits inside the
///      consumer law's proof cone (`mentions ⊆ qualified_scope`) OR mentions
///      the consumer's subject and is a law about a cone fn. Qualified
///      identity closes the bare-name-collision hole: a `Lib.qrev` mention
///      matches only a cone that genuinely contains the dep module's real
///      `Lib.qrev`, never an unrelated module's same-named fn.
///
/// The RESIDUAL (kernel closure) is downstream: whether the admitted dep
/// law's `first | … | sorry` proof actually closes is decided by the Lean
/// kernel at `lake build`, not by codegen. A cited dep law that falls to
/// `sorry` taints the consumer's `#print axioms` (`sorryAx`) and flips
/// `universal:false` — the same per-declaration crediting the in-file pool
/// rides on. See `earlier_law_lemmas`' module note for why a true
/// proven-only-at-codegen gate would need a two-phase lake build.
#[allow(clippy::too_many_arguments)]
fn dep_law_admissible(
    dep_module: &crate::codegen::ModuleInfo,
    dep_prev: &VerifyBlock,
    dep_prev_law: &VerifyLaw,
    qualified_scope: &BTreeSet<String>,
    subject: &str,
    dep_index: &std::collections::BTreeMap<String, String>,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    let (bare_name, stmt) = ctx.with_module_scope(Some(dep_module.prefix.as_str()), || {
        crate::codegen::lean::toplevel::law_as_lemma_statement(dep_prev, dep_prev_law, ctx)
    })?;
    // Namespace-qualified citation; the entry imports + opens the dep.
    let name = format!("{}.{}", dep_module.prefix, bare_name);
    let text = format!("theorem {name} : {stmt} := by");
    let mentions = crate::codegen::lemma_discovery::mentioned_fns(&text, dep_index);
    if mentions.is_empty() {
        return None;
    }
    // QUALIFIED subject: the dep law's own subject fn rendered the way the
    // qualified scope holds it (`Lib.qrev`), so the decomposition arm
    // ("admit a law ABOUT a cone fn that mentions THIS subject") compares
    // like for like instead of via a bare-name collision.
    let prev_subject_qualified = format!(
        "{}.{}",
        dep_module.prefix,
        aver_name_to_lean(&dep_prev.fn_name)
    );
    if mentions.is_subset(qualified_scope)
        || (mentions.contains(subject) && qualified_scope.contains(&prev_subject_qualified))
    {
        Some((name, text))
    } else {
        None
    }
}

/// Program-wide set of `(module_prefix, theorem_base)` dep-law theorems
/// that SOME consumer law admits into its pool — the EMIT-side gate. A
/// dep law is emitted into the build ONLY if it is in this set; an
/// un-cited dep law is a complete no-op for the consumer (it contributes
/// zero to the consumer's file-wide `sorry` count — MAJOR 4). Computed
/// with the SAME `dep_law_admissible` gate the CONSUME side uses, over
/// every consumer law: the entry's own `verify … law` blocks, plus each
/// dep module's own laws acting as a consumer for laws in modules strictly
/// earlier in the topological `ctx.modules` order.
///
/// Empty when there are no dep modules (single-file path) → no dep-law
/// emission changes → byte-identical to the pre-feature output.
pub(crate) fn admitted_dep_law_theorems(
    ctx: &CodegenContext,
) -> std::collections::HashSet<(String, String)> {
    use crate::ast::{TopLevel, VerifyKind};
    let mut admitted: std::collections::HashSet<(String, String)> =
        std::collections::HashSet::new();
    if ctx.modules.is_empty() {
        return admitted;
    }

    // Each consumer law contributes the dep laws it admits. `consumer_idx`
    // is the consumer's position in `ctx.modules` (`None` = entry, which may
    // cite any dep); a module-scoped consumer may cite only modules STRICTLY
    // earlier (the topological / acyclicity guard, mirrored from the
    // CONSUME loop).
    let consider = |consumer_scope: Option<&str>,
                    consumer_vb: &VerifyBlock,
                    consumer_law: &VerifyLaw,
                    admitted: &mut std::collections::HashSet<(String, String)>| {
        ctx.with_module_scope(consumer_scope, || {
            let (qualified_scope, subject) =
                consumer_law_qualified_scope(consumer_vb, consumer_law, ctx);
            let consumer_idx =
                consumer_scope.and_then(|s| ctx.modules.iter().position(|m| m.prefix == s));
            for (module_idx, module) in ctx.modules.iter().enumerate() {
                if let Some(c) = consumer_idx
                    && module_idx >= c
                {
                    continue;
                }
                let dep_index = dep_membership_index(module, ctx);
                for dep_prev in &module.verify_laws {
                    let VerifyKind::Law(dep_prev_law) = &dep_prev.kind else {
                        continue;
                    };
                    if let Some((name, _text)) = dep_law_admissible(
                        module,
                        dep_prev,
                        dep_prev_law,
                        &qualified_scope,
                        &subject,
                        &dep_index,
                        ctx,
                    ) {
                        // `name` is `Module.<theorem_base>`; the EMIT side keys
                        // on `(prefix, theorem_base)`.
                        if let Some(base) = name.strip_prefix(&format!("{}.", module.prefix)) {
                            admitted.insert((module.prefix.clone(), base.to_string()));
                        }
                    }
                }
            }
        });
    };

    // Entry consumer laws.
    for item in &ctx.items {
        let TopLevel::Verify(vb) = item else { continue };
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        consider(None, vb, law, &mut admitted);
    }
    // Module-scoped consumer laws (a dep that itself cites an earlier dep).
    for module in &ctx.modules {
        for vb in &module.verify_laws {
            let VerifyKind::Law(law) = &vb.kind else {
                continue;
            };
            consider(Some(module.prefix.as_str()), vb, law, &mut admitted);
        }
    }
    admitted
}

/// Build the cross-file membership index for a dep module's laws: each
/// pure fn of `module` mapped QUALIFIED → QUALIFIED (`Lib.qrev` →
/// `Lib.qrev`), unioned with the entry/dep bare program index. Keyed on
/// the qualified form because a dep law's statement renders dep fns
/// namespace-qualified, so the gate compares qualified identity.
fn dep_membership_index(
    module: &crate::codegen::ModuleInfo,
    ctx: &CodegenContext,
) -> std::collections::BTreeMap<String, String> {
    let mut idx: std::collections::BTreeMap<String, String> = program_fn_lean_names(ctx)
        .into_iter()
        .map(|l| (l.clone(), l))
        .collect();
    for fd in &module.fn_defs {
        if crate::codegen::common::is_pure_fn(fd) {
            let qualified = format!("{}.{}", module.prefix, aver_name_to_lean(&fd.name));
            idx.insert(qualified.clone(), qualified);
        }
    }
    idx
}

/// The QUALIFIED proof cone of a consumer law (+ its subject), the gate
/// the cross-file admission compares against. Cone fns owned by a dep
/// module render qualified (`Lib.qrev`); entry fns and the consumer's own
/// subject stay bare. Same cone computation the in-file gate uses — only
/// the rendering is qualified so a `Lib.qrev` mention can't be satisfied
/// by an unrelated module's same-named fn.
fn consumer_law_qualified_scope(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> (BTreeSet<String>, String) {
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let mut scope: BTreeSet<String> = cone
        .pure_fns()
        .iter()
        .map(|fd| qualified_cone_name(fd, ctx))
        .collect();
    let subject = aver_name_to_lean(&vb.fn_name);
    scope.insert(subject.clone());
    (scope, subject)
}

/// The discovery feedback loop, część A: earlier proved user `verify … law`
/// blocks in the same file, usable as `simp` rewrite rules for THIS law.
///
/// Eligibility mirrors the committed-lemma planner: a sibling joins only if
/// every program fn its statement mentions is in this law's proof cone ∪
/// subject (keeps the simp set focused and bounds loop surface). Only blocks
/// EARLIER in source are eligible — source order is emit order, so the
/// referenced theorem precedes this one, and the strict ordering makes cyclic
/// lemma use impossible by construction. Each result is a `reference`
/// (`embed = false`): the theorem is already emitted, so only its NAME joins
/// the simp set; the synthesized statement text drives orientation + loop
/// analysis. Soundness rides on the same guard as everything else — if the
/// referenced law itself only `sorry`s, this law's proof inherits `sorryAx`
/// and the universal metric reports false.
fn earlier_law_lemmas(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<crate::codegen::lemma_discovery::CommittedLemma> {
    use crate::ast::{TopLevel, VerifyKind};
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let mut scope: BTreeSet<String> = cone
        .pure_fns()
        .iter()
        .map(|fd| aver_name_to_lean(&fd.name))
        .collect();
    let subject = aver_name_to_lean(&vb.fn_name);
    scope.insert(subject.clone());

    let program_index: std::collections::BTreeMap<String, String> = program_fn_lean_names(ctx)
        .into_iter()
        .map(|l| (l.clone(), l))
        .collect();

    let mut out = Vec::new();
    for item in &ctx.items {
        let TopLevel::Verify(prev) = item else {
            continue;
        };
        // Stop at the consumer law itself; only earlier blocks are eligible.
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        let Some((name, stmt)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        else {
            continue;
        };
        let text = format!("theorem {name} : {stmt} := by");
        let mentions = crate::codegen::lemma_discovery::mentioned_fns(&text, &program_index);
        if mentions.is_empty() {
            continue;
        }
        // Eligibility: either the sibling stays entirely inside the consumer's
        // proof cone (the conservative rule), OR it mentions the consumer's
        // SUBJECT fn — the strongest "this decomposes THIS law" signal, which
        // also lets a decomposition INTRODUCE a new combinator. A count-homo
        // helper `count n (a++b) = plus (count n a)(count n b)` mentions
        // `plus` (outside count-rev's cone {count,rev,eqNat}) but shares the
        // subject `count`, so it must be admitted — that `plus` is exactly the
        // combinator the decomposition needs, and its `= a+b` bridge is
        // synthesized downstream. Tight enough to stay relevant (a length-homo
        // in a count file shares neither cone nor subject → rejected); loop
        // safety is handled separately by `simp_entries`.
        // Admit by the subject rule only when the sibling is a law ABOUT a fn
        // this law's proof actually involves (its OWN subject fn ∈ scope). A
        // sibling that merely mentions the subject incidentally on its RHS —
        // `length (x ++ y) = plus …` carrying `plus` into a `plus`-commutativity
        // proof — is inert in the goal (nothing to rewrite) and only adds noise.
        // The genuine decomposition case is preserved: a count-homomorphism
        // helper IS a law about `count`, the very subject it shares.
        let prev_subject = aver_name_to_lean(&prev.fn_name);
        // THIRD rule — LHS-rooted relevance. A Forward sibling fires against the
        // consumer goal only through its LHS shape, so a helper whose LHS sits
        // ENTIRELY inside this proof's cone is relevant even when its RHS
        // introduces a new combinator outside the cone. The `even (length
        // (append x y)) = even (length (append y x))` commute needs the
        // length-homomorphism `length (append x y) = plus …` — its LHS is
        // `{length, append} ⊆ cone`, but `plus` (RHS-only) keeps it out of the
        // is_subset rule, and it never mentions the subject `even` for the
        // subject rule. Gate on the sibling's OWN subject fn ∈ scope (a law
        // genuinely ABOUT a cone fn, not unrelated noise), exactly as the
        // subject rule does; the RHS combinator's `= a + b` bridge is
        // synthesized downstream, and loop safety stays in `simp_entries`.
        let lhs_mentions = crate::codegen::lemma_discovery::lemma_lhs_fns(&text, &program_index);
        let lhs_rooted = !lhs_mentions.is_empty()
            && lhs_mentions.is_subset(&scope)
            && scope.contains(&prev_subject);
        if mentions.is_subset(&scope)
            || (mentions.contains(&subject) && scope.contains(&prev_subject))
            || lhs_rooted
        {
            out.push(crate::codegen::lemma_discovery::CommittedLemma::reference(
                name, text,
            ));
        }
    }

    // Cross-file law pool — CONSUME side: a dependency module's proven,
    // EXPOSED laws join the pool under the SAME cone ∪ subject gate as
    // in-file siblings, but compared in QUALIFIED identity (`Lib.qrev`),
    // just over a wider source set. The module DAG is acyclic and
    // `ctx.modules` is topologically ordered (every dep precedes its
    // consumers), so a cross-file citation can only point backward — the
    // analog of the in-file source-order `break` above. The dep law's
    // statement is synthesized under the dep's module scope (so its
    // expressions resolve in the dep namespace) and cited by its
    // namespace-qualified name (`M.<fn>_law_<name>`), which the entry
    // resolves via the `import M` + `open M` it already emits.
    //
    // FAIL-CLOSED AT ADMISSION — three gates, all enforced here / upstream:
    //   - VISIBILITY: a private dep law never reaches `module.verify_laws`
    //     (`collect_verify_laws` filters on the subject's `exposes`), so it
    //     cannot enter any consumer's pool.
    //   - SHAPE: `law_as_lemma_statement` declines laws it can't state
    //     universally (no givens / when-premise / singleton-const-rhs /
    //     fuel-bounded), so a citation could never point at a missing
    //     universal theorem.
    //   - CONE (QUALIFIED): a `Lib.qrev` mention matches only a cone that
    //     genuinely contains the dep module's real `Lib.qrev`, never an
    //     unrelated module's same-named fn — the bare-name-collision hole is
    //     closed by `consumer_law_qualified_scope` + `dep_membership_index`.
    // The RESIDUAL (whether the admitted dep law's `first | … | sorry`
    // proof actually closes in the kernel) is decided downstream by `lake
    // build`; a cited-but-non-closing dep law taints the consumer via
    // `sorryAx` and flips `universal:false` (the same per-declaration
    // crediting the in-file pool relies on). See `dep_law_admissible`.
    //
    // Only modules STRICTLY EARLIER in the DAG than the law being proven
    // are eligible — the cross-file analog of the in-file source-order
    // `break`, which makes a law citing ITSELF (structurally-recursive,
    // termination-failing) impossible. An entry law (active scope `None`)
    // may cite any dep.
    let (qualified_scope, qualified_subject) = consumer_law_qualified_scope(vb, law, ctx);
    let consumer_module_idx = ctx
        .active_module_scope()
        .and_then(|s| ctx.modules.iter().position(|m| m.prefix == s));
    for (module_idx, module) in ctx.modules.iter().enumerate() {
        if let Some(consumer_idx) = consumer_module_idx
            && module_idx >= consumer_idx
        {
            continue;
        }
        let dep_index = dep_membership_index(module, ctx);
        for prev in &module.verify_laws {
            let VerifyKind::Law(prev_law) = &prev.kind else {
                continue;
            };
            if let Some((name, text)) = dep_law_admissible(
                module,
                prev,
                prev_law,
                &qualified_scope,
                &qualified_subject,
                &dep_index,
                ctx,
            ) {
                out.push(crate::codegen::lemma_discovery::CommittedLemma::reference(
                    name, text,
                ));
            }
        }
    }
    out
}

/// Fast-path `simp` set for the feedback emit: the committed pinned lemmas
/// (`committed_names` → `ctx.discovered_lemmas`) PLUS the eligible earlier
/// sibling laws, run together through `lemma_discovery::simp_entries` so the
/// loop-exclusion sees the whole set (a committed Reversed rule + a sibling
/// Forward rule that would cycle is dropped — a simp loop is an uncatchable
/// maxHeartbeats build error). Siblings feed ONLY this fast path, never the
/// induction-arm simp sets, so a law that already closed on its ladder keeps
/// that ladder byte-identical as the fallback.
fn fastpath_simp_entries(
    ctx: &CodegenContext,
    committed_names: &[String],
    siblings: &[crate::codegen::lemma_discovery::CommittedLemma],
) -> Vec<String> {
    // The orientation / loop-exclusion analysis keys on whether a
    // lemma's head is a program fn. A cross-file sibling's statement
    // renders dep fns NAMESPACE-QUALIFIED (`Lib.qrev`), so the program-
    // fn set must carry those qualified forms too or the dep lemma is
    // silently classified `None` and dropped from the simp set. No dep
    // modules → no qualified names added → byte-identical to the
    // single-file path.
    let mut program_fns = program_fn_lean_names(ctx);
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            if crate::codegen::common::is_pure_fn(fd) {
                program_fns.insert(format!("{}.{}", module.prefix, aver_name_to_lean(&fd.name)));
            }
        }
    }
    let mut pool: Vec<&crate::codegen::lemma_discovery::CommittedLemma> = ctx
        .discovered_lemmas
        .iter()
        .filter(|l| committed_names.contains(&l.name))
        .collect();
    pool.extend(siblings.iter());
    if pool.is_empty() {
        return Vec::new();
    }
    crate::codegen::lemma_discovery::simp_entries(&pool, &program_fns)
}

/// Program fns mentioned by the committed pins AND the sibling laws — drives
/// the bridge scan (`lean_nat_lift_support`'s `extra_fns`) so a Peano op a
/// homomorphism introduces (`plus` rewriting into a law that only said
/// `length`) still gets its `= a + b` bridge for the fast path's `omega`.
fn feedback_source_fns(
    ctx: &CodegenContext,
    committed_names: &[String],
    siblings: &[crate::codegen::lemma_discovery::CommittedLemma],
) -> BTreeSet<String> {
    // Lean-name → SOURCE-name: the bridge collector (`collect_nat_arith_ops_
    // for_names`) resolves source names via `fn_def_by_name`, so the projection
    // must land on source names (not Lean names).
    let lean_to_source: std::collections::BTreeMap<String, String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| crate::codegen::common::is_pure_fn(fd))
        .map(|fd| (aver_name_to_lean(&fd.name), fd.name.clone()))
        .collect();
    let mut out = discovered_lemma_source_fns(ctx, committed_names);
    for s in siblings {
        out.extend(crate::codegen::lemma_discovery::mentioned_fns(
            &s.text,
            &lean_to_source,
        ));
    }
    out
}

enum VariantKind {
    Leaf,
    DirectRec,
    IndirectRec,
}

fn classify_variant(variant: &TypeVariant, type_name: &str) -> VariantKind {
    let mut has_indirect = false;
    for field in &variant.fields {
        if field.trim() == type_name {
            return VariantKind::DirectRec;
        }
        if field_type_contains_indirect(field, type_name) {
            has_indirect = true;
        }
    }
    if has_indirect {
        VariantKind::IndirectRec
    } else {
        VariantKind::Leaf
    }
}

fn field_type_contains_indirect(field_type: &str, type_name: &str) -> bool {
    if field_type.trim() == type_name {
        return false;
    }
    field_type.contains(&format!("<{}", type_name))
        || field_type.contains(&format!("{}>", type_name))
        || field_type.contains(&format!(", {}", type_name))
        || field_type.contains(&format!("{},", type_name))
}

fn find_sum_type<'a>(
    ctx: &'a CodegenContext,
    name: &str,
) -> Option<(&'a String, &'a Vec<TypeVariant>)> {
    ctx.modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .find_map(|td| match td {
            TypeDef::Sum {
                name: n, variants, ..
            } if n == name => Some((n, variants)),
            _ => None,
        })
}

fn is_recursive_sum(type_name: &str, variants: &[TypeVariant]) -> bool {
    variants
        .iter()
        .any(|variant| variants_fields_contain_type(&variant.fields, type_name))
}

fn variants_fields_contain_type(fields: &[String], type_name: &str) -> bool {
    fields.iter().any(|field| {
        field.trim() == type_name
            || field.contains(&format!("<{}", type_name))
            || field.contains(&format!("{}>", type_name))
            || field.contains(&format!(", {}", type_name))
            || field.contains(&format!("{},", type_name))
    })
}

fn find_induction_target<'a>(
    law: &'a VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(usize, &'a str, &'a str)> {
    for (index, given) in law.givens.iter().enumerate() {
        if let Some((_, variants)) = find_sum_type(ctx, &given.type_name)
            && is_recursive_sum(&given.type_name, variants)
        {
            return Some((index, &given.name, &given.type_name));
        }
    }
    None
}

fn has_indirect_variants(variants: &[TypeVariant], type_name: &str) -> bool {
    variants.iter().any(|variant| {
        matches!(
            classify_variant(variant, type_name),
            VariantKind::IndirectRec
        )
    })
}

fn premise_intro_names(law: &VerifyLaw, intro_names: &[String]) -> Vec<String> {
    let mut names = Vec::new();
    if law.when.is_some() {
        names.extend(intro_names.iter().map(|name| format!("h_{name}")));
        names.push("h_when".to_string());
    }
    names
}

/// A linear `Nat` argument `omega` can reason about after the comparison
/// bridges fire: a bare given/identifier, a Peano successor `Nat.S(x)` over
/// another such term, the zero constructor `Nat.Z`, or a numeral. A non-`S`/`Z`
/// CALL or any other term shape is rejected, so the conditional-bridge
/// recognizer only matches goals that genuinely lower to linear arithmetic. A
/// bare identifier is accepted regardless of its bound type — soundness rests on
/// the SOLE caller, [`is_peano_compare_call`], gating these as the arguments of a
/// recognized 2-arg Peano comparison fn (so they are `Nat` by that fn's
/// signature); a misuse outside that gate would over-accept, but the proof's
/// `sorry` floor + the `#print axioms` credit gate keep even that fail-closed.
fn is_linear_nat_arg(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::{Expr, Literal};
    match &expr.node {
        Expr::Ident(_) | Expr::Resolved { .. } => true,
        Expr::Literal(Literal::Int(_)) => true,
        Expr::FnCall(callee, args) => match super::shared::expr_dotted_name(callee) {
            Some(name) => match name.rsplit('.').next().unwrap_or(name.as_str()) {
                "S" => args.len() == 1 && is_linear_nat_arg(&args[0]),
                "Z" => args.is_empty(),
                _ => false,
            },
            None => false,
        },
        _ => false,
    }
}

/// A 2-arg call to a recognized canonical Peano comparison fn (`≤` / `<` / `=`)
/// whose arguments are both linear `Nat` terms. Returns `true` when the
/// expression is exactly that shape — the building block of the
/// conditional-comparison-bridge recognizer.
fn is_peano_compare_call(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> bool {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    if args.len() != 2 {
        return false;
    }
    let Some(name) = super::shared::expr_dotted_name(callee) else {
        return false;
    };
    let mut names = BTreeSet::new();
    names.insert(name);
    if crate::codegen::proof_recognize::collect_nat_compare_ops_for_names(&names, ctx).is_empty() {
        return false;
    }
    is_linear_nat_arg(&args[0]) && is_linear_nat_arg(&args[1])
}

/// The EASY conditional-comparison shape (`prop_70 leSucc`): a `when` premise
/// that is a canonical Peano Bool relation over linear `Nat` terms, and an
/// atomic conclusion `<relation>(..) => true` that is another such relation.
/// Bridging both sides to their Prop forms turns the law into a linear-`Nat`
/// implication, which `omega` discharges. Deliberately narrow: a negated /
/// compound premise, or a conclusion that is not a single comparison `= true`,
/// is rejected so the law keeps the bounded guarded-domain fallback (the HARD
/// conditional-inductive family — sortedness/insertion — is out of scope here).
fn conditional_comparison_bridge_shape(law: &VerifyLaw, ctx: &CodegenContext) -> bool {
    use crate::ast::{Expr, Literal};
    let Some(when) = &law.when else {
        return false;
    };
    // Conclusion is `<peano-compare>(..) => true` (the `=> true` / `holds`
    // surface): rhs is the literal `true`, lhs a recognized comparison call.
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return false;
    }
    if !is_peano_compare_call(&law.lhs, ctx) {
        return false;
    }
    // Premise is an un-negated recognized comparison call.
    is_peano_compare_call(when, ctx)
}

/// Whether [`emit_conditional_comparison_bridge_law`] will close this law as a
/// TRUE-universal conditional. The caller (`emit_verify_law_block`) reads this
/// to decide whether to drop the sampled-domain disjunctions from the theorem
/// statement (`omit_domain`), so the predicate must agree with the emit: it is
/// exactly the recognizer, and the shape guarantees a comparison bridge exists
/// (so the emit never declines for lack of one).
pub(in crate::codegen::lean) fn recognize_conditional_comparison_bridge(
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    conditional_comparison_bridge_shape(law, ctx)
}

/// Close an EASY conditional comparison law (`prop_70 leSucc`) as the
/// TRUE-universal `∀ givens, <when> = true -> <claim>`. Emits the canonical
/// Peano comparison bridges (`(f a b = true) = (a R b)`) for every relation the
/// law mentions — seeding the scan with the premise's fns, which
/// `lean_nat_lift_support` (lhs/rhs only) would otherwise miss — then proves
/// the goal by rewriting the premise hypothesis and the goal through those
/// bridges and discharging the linear-`Nat` residual with `omega`.
///
/// FAIL-CLOSED on two axes: (1) any shape that is not the recognized
/// pure-comparison conditional returns `None`, so the caller falls back to the
/// bounded guarded-domain `native_decide` proof (still `passed`, not
/// `universal`); (2) the proof body is floored with `sorry`, so a residual the
/// bridges + `omega` cannot close degrades to an honest sorry (caught,
/// `universal:false`) rather than a build error — the `_checked_domain`
/// cross-check still earns `passed`. The bridge support theorems are themselves
/// sound-by-floor (`lean_nat_lift_support`), so a misrecognized relation can
/// never inject a false bridge.
pub(in crate::codegen::lean) fn emit_conditional_comparison_bridge_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    if !conditional_comparison_bridge_shape(law, ctx) {
        return None;
    }
    // Law-scoped bridge name prefix, matching the other induction emits
    // (`{fn}_{law}`). Deliberately NOT the `{fn}_law_{law}` theorem base: a
    // `_law_` substring would make the audit's `is_main_law_theorem` mistake a
    // bridge support theorem for a creditable main law theorem.
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    // `lean_nat_lift_support` scans `lhs`/`rhs` for ops; seed `extra_fns` with
    // the premise's fns so a premise-only relation still gets a bridge.
    let mut extra: BTreeSet<String> = BTreeSet::new();
    if let Some(when) = &law.when {
        crate::codegen::proof_recognize::collect_called_fns(when, &mut extra);
    }
    let (bridge_support, bridge_names, _bridged) =
        lean_nat_lift_support(law, ctx, &law_uid, &extra);
    let has_compare_bridge = bridge_names
        .iter()
        .any(|n| n.ends_with("_isNatLe") || n.ends_with("_isNatLt") || n.ends_with("_isNatEq"));
    if !has_compare_bridge {
        return None;
    }
    let bridges = bridge_names.join(", ");
    // `intro <givens> h_when`; rewrite the premise hypothesis AND the goal
    // through the Prop bridges, then `omega` over the linear residual. `<;>
    // omega` (not `; omega`) so a `simp` that fully closes the goal does not
    // leave `omega` with no goals.
    let intro = format!("  intro {} h_when", intro_names.join(" "));
    let close = format!("  first | (simp only [{bridges}] at h_when ⊢ <;> omega) | sorry");
    Some(AutoProof {
        support_lines: bridge_support,
        body: Tactic::raw(vec![intro, close]),
        replaces_theorem: false,
    })
}

/// Whether a `when`-law is a conditional-INDUCTIVE list-predicate law (the
/// membership family — `prop_36` `when elem(x,y) -> elem(x, y ++ z) => true`,
/// `prop_71` `when not(eqNat(x,y)) -> elem(x, ins(y,xs)) = elem(x,xs)`): there is
/// a `List<_>` given to induct on, and the verified fn structurally recurses on
/// a list parameter (so inducting on the list given peels both sides). Distinct
/// from the flat comparison-bridge family — here the conclusion is a recursive
/// list predicate, not an `omega`-shaped comparison, so it is proved by
/// premise-threaded list induction rather than a bridge. Used by both the proof
/// emitter and the `omit_domain` statement driver, which must agree.
pub(in crate::codegen::lean) fn recognize_conditional_inductive_list(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    let Some(when) = &law.when else {
        return false;
    };
    let Some(target_idx) = find_list_induction_target(law) else {
        return false;
    };
    // The bridge family owns the flat-comparison shape; never claim it here.
    if conditional_comparison_bridge_shape(law, ctx) {
        return false;
    }
    // A non-empty unfold cone is required (the induction arms `simp` the verified
    // fn's defs); without one the portfolio has nothing to drive.
    if law_simp_defs(ctx, vb, law).is_empty() {
        return false;
    }
    // The verified fn must be a PER-ELEMENT LIST FOLD: it structurally recurses
    // on a list parameter AND its cons arm combines the HEAD element with the
    // recursive call (so inducting on the list given peels both sides in lockstep
    // and the portfolio's `split <;> simp_all` closes via the IH). This gate is
    // load-bearing — it admits `elem` (Bool) AND `count` (Nat) while excluding
    // element-SELECTORS like `last`/`head` (`prop_59`/`prop_60`): `last`'s cons
    // arm uses only the recursive tail (not the head), `head`'s only the head
    // (no recursion), so neither folds — their conclusion is an element equation
    // the membership portfolio cannot close, and admitting them would `sorry` and
    // drop `passed`.
    let Some(fd) = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref()) else {
        return false;
    };
    if !is_per_element_list_fold(fd) {
        return false;
    }
    // VALIDATED-SHAPE GATE. `omit_domain` replaces the bounded `native_decide`
    // proof (which earns `passed`) with the universal portfolio; if that portfolio
    // cannot close, its `sorry` floor would drop `passed` (sorries > budget). So
    // restrict to the two shapes the probe + a measured corpus sweep proved the
    // portfolio CLOSES, both driven by induction on the first list given:
    //   (A) the conclusion applies the verified fn to a BUILTIN `List.concat(
    //       <target>, _)` — the target is the spine the cons step peels
    //       (`(h :: t) ++ z = h :: (t ++ z)`). Narrow by construction: only the
    //       literal `List.concat` spelling qualifies, so the sole corpus hit is
    //       `prop_36`; an append written as a USER fn (`prop_37`'s `append`)
    //       reaches the portfolio through (B) instead, not here.
    //   (B) target-independent premise — the `when` does not mention the
    //       induction target, so it is a fixed side condition available in every
    //       arm while the conclusion peels structurally. The broad branch: it
    //       admits the whole neq-/eq-membership family (`prop_37`, `prop_46`,
    //       `prop_47`, `prop_71`, `lemma_19`) plus `prop_38`.
    // Other conditional-inductive shapes fail BOTH and decline — keeping their
    // bounded `passed`: a premise that mentions the target over a non-`concat`
    // conclusion (drop-relative `prop_39`, sort-relative `prop_49`,
    // `prop_42`/`prop_43`/`prop_44`), and the sortedness/insertion family that
    // needs a head-of-insert aux lemma (`prop_77`, `lemma_12`). The Bool-return
    // gate above additionally drops element-returning `last`/`head` (`prop_59`/
    // `prop_60`) and the `count`/`union`/`zip` value-returning list fns.
    let target_name = &law.givens[target_idx].name;
    let append_on_target = law_applies_verified_fn_to_concat(law, &vb.fn_name, target_name);
    let premise_target_independent = !expr_mentions_ident(when, target_name);
    append_on_target || premise_target_independent
}

/// Whether the law applies its verified fn to a `List.concat(<target>, _)` — the
/// append-monotonicity shape (`elem(x, concat(y, z))` with `y` the induction
/// target). Scans both sides of the claim.
fn law_applies_verified_fn_to_concat(law: &VerifyLaw, fn_name: &str, target: &str) -> bool {
    use crate::ast::Expr;
    fn concat_first_arg_is(expr: &crate::ast::Spanned<Expr>, target: &str) -> bool {
        let Expr::FnCall(callee, args) = &expr.node else {
            return false;
        };
        super::shared::expr_dotted_name(callee).as_deref() == Some("List.concat")
            && args
                .first()
                .is_some_and(|a| matches!(&a.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == target))
    }
    fn scan(expr: &crate::ast::Spanned<Expr>, fn_name: &str, target: &str) -> bool {
        use crate::ast::Expr;
        if let Expr::FnCall(callee, args) = &expr.node {
            if super::shared::expr_dotted_name(callee).as_deref() == Some(fn_name)
                && args.iter().any(|a| concat_first_arg_is(a, target))
            {
                return true;
            }
            if args.iter().any(|a| scan(a, fn_name, target)) {
                return true;
            }
            return scan(callee, fn_name, target);
        }
        false
    }
    scan(&law.lhs, fn_name, target) || scan(&law.rhs, fn_name, target)
}

/// Whether an expression syntactically references the identifier `name`.
/// Conservative on exotic node kinds (returns `true`): a premise whose shape we
/// do not destructure is assumed to possibly mention the target, so it is
/// EXCLUDED from the target-independent `(B)` branch rather than wrongly admitted
/// — admitting a target-dependent premise there would risk a non-closing proof
/// and a `passed` regression.
fn expr_mentions_ident(expr: &crate::ast::Spanned<crate::ast::Expr>, name: &str) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Literal(_) => false,
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n == name,
        Expr::Attr(e, _) | Expr::Neg(e) | Expr::ErrorProp(e) => expr_mentions_ident(e, name),
        Expr::FnCall(callee, args) => {
            expr_mentions_ident(callee, name) || args.iter().any(|a| expr_mentions_ident(a, name))
        }
        Expr::BinOp(_, l, r) => expr_mentions_ident(l, name) || expr_mentions_ident(r, name),
        Expr::Constructor(_, payload) => payload
            .as_ref()
            .is_some_and(|e| expr_mentions_ident(e, name)),
        Expr::List(es) | Expr::Tuple(es) => es.iter().any(|e| expr_mentions_ident(e, name)),
        Expr::Match { subject, arms } => {
            expr_mentions_ident(subject, name)
                || arms.iter().any(|arm| expr_mentions_ident(&arm.body, name))
        }
        _ => true,
    }
}

/// Whether some leaf of `expr` (descending through `match` arms) IS the bare
/// identifier `name` — i.e. a code path that RETURNS that variable directly. For
/// the cons head this is the SELECTOR signature: `last`'s `match zs { [] -> z ; …
/// }` returns the head element, `head`'s cons arm is the bare head. A fold never
/// returns the head — it only COMPARES it (`eqNat(x, z)`), so the head appears as
/// a call argument, never as a leaf.
fn any_leaf_is_ident(expr: &crate::ast::Spanned<crate::ast::Expr>, name: &str) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Match { arms, .. } => arms.iter().any(|arm| any_leaf_is_ident(&arm.body, name)),
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n == name,
        _ => false,
    }
}

/// A per-element list FOLD: the verified fn structurally recurses on a `List<_>`
/// param AND its cons arm references the head element but NEVER returns it as a
/// leaf (the head is COMPARED, not selected). Admits `elem` in BOTH spellings —
/// `barbar(eqNat(x, z), elem(x, xs))` and the explicit `match eqNat(x, z) { true
/// -> true ; false -> elem(x, xs) }` — and `count` (`match natEq(x, z) { true ->
/// S(count …) ; false -> count … }`). Excludes element SELECTORS: `last`'s cons
/// arm has a `[] -> z` leaf returning the head; `head` (which does not even
/// recurse on the list, so the structural-param gate already rejects it). The
/// cons-arm structure, not the return type, is the discriminator (so `count`'s
/// `Nat` return qualifies alongside `elem`'s `Bool`).
fn is_per_element_list_fold(fd: &crate::ast::FnDef) -> bool {
    use crate::ast::{Expr, Pattern};
    let Some(list_idx) = crate::codegen::recursion::detect::single_list_structural_param_index(fd)
    else {
        return false;
    };
    let list_param = &fd.params[list_idx].0;
    let Some(body) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    if !matches!(&subject.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == list_param)
    {
        return false;
    }
    arms.iter()
        .find_map(|arm| {
            let Pattern::Cons(head, _) = &arm.pattern else {
                return None;
            };
            Some(expr_mentions_ident(&arm.body, head) && !any_leaf_is_ident(&arm.body, head))
        })
        .unwrap_or(false)
}

/// Close a conditional-inductive list-predicate law (`prop_36`, `prop_71`) as the
/// TRUE-universal `∀ givens, <when> = true -> <claim>` by induction on the first
/// `List` given, THREADING the premise: the premise is introduced INSIDE each
/// arm (not before `induction`) so the cons induction hypothesis carries it as an
/// antecedent — the essential difference from the unconditional `emit_list_induction`
/// ladder. Each arm runs a `first | … | sorry` portfolio that unfolds the verified
/// fn's defs + the `List.*_append` peel lemmas, normalizes a negated premise
/// (`Bool.not_eq_true`), case-splits the recursive predicate's inner Bool
/// dispatcher (`split`), and discharges with `simp_all` (which applies the IH at
/// the tail using the premise-derived hypothesis). FAIL-CLOSED on two axes: any
/// non-matching shape returns `None` (caller keeps the bounded guarded-domain
/// proof), and every arm is `done`-terminated under a `sorry` floor, so a residual
/// the portfolio cannot close degrades to an honest sorry (`universal:false`,
/// `passed` via `_checked_domain`) rather than a build error or a false closure.
pub(in crate::codegen::lean) fn emit_conditional_inductive_list_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    if !recognize_conditional_inductive_list(vb, law, ctx) {
        return None;
    }
    let target_idx = find_list_induction_target(law)?;
    let target = intro_names.get(target_idx)?.clone();
    let defs = law_simp_defs(ctx, vb, law)
        .into_iter()
        .collect::<Vec<_>>()
        .join(", ");
    // The builtin `++` peel lemmas the recursive list expression needs (e.g.
    // `(hd :: tl) ++ z = hd :: (tl ++ z)`, `[x] ++ y = x :: y`).
    let with_append = format!("{defs}, List.cons_append, List.nil_append, List.singleton_append");
    // Normalize a negated Bool premise (`(!p) = true` → `p = false`) so `simp_all`
    // can use it; a no-op `try` when the premise is not negated.
    let norm = "(try simp only [Bool.not_eq_true, Bool.not_eq_false] at h_when)".to_string();
    // Peano comparison bridges (`(f a b = true) = (a R b)`) for the relations the
    // law mentions — premise included. They let `omega` discharge the dispatcher
    // branches a COMPARISON premise rules out: `prop_86` (`when lt(x,y)`) needs
    // `eqNat x y = false`, which falls out of the `<`/`=` bridges + `omega`,
    // whereas a same-relation premise (`prop_71`'s `eqNat`) needs no bridge and
    // closes on the first branch below. Sound-by-floor: a misrecognized op fails
    // its own bridge proof and degrades to a sorry, never a false bridge.
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    let mut extra: BTreeSet<String> = BTreeSet::new();
    if let Some(when) = &law.when {
        crate::codegen::proof_recognize::collect_called_fns(when, &mut extra);
    }
    let (bridge_support, bridge_names, _bridged) =
        lean_nat_lift_support(law, ctx, &law_uid, &extra);
    let cmp_bridges: Vec<String> = bridge_names
        .into_iter()
        .filter(|n| n.ends_with("_isNatLe") || n.ends_with("_isNatLt") || n.ends_with("_isNatEq"))
        .collect();
    // The `simp_all` set with the comparison bridges folded in (for the
    // omega-closing branches); bare defs when the law has no Peano comparison.
    let defs_br = if cmp_bridges.is_empty() {
        defs.clone()
    } else {
        format!("{defs}, {}", cmp_bridges.join(", "))
    };
    // Arm portfolios. The measured same-relation closures (prop_36/37/38/46/47/71,
    // lemma_19) land on the FIRST cons branch (`simp only […] at h_when ⊢ <;>
    // (split <;> simp_all)`) and the first nil branch; the comparison-premise
    // closure (prop_86) lands on the bridge+`omega` branches. The rest are a
    // `done`-terminated speculative hedge (a non-closing branch throws on its
    // `done`, so `first` advances at zero correctness cost, and at no build-time
    // cost for an admitted law since the winning branch is tried first). The
    // trailing `sorry` keeps the whole emit fail-closed.
    let body = vec![
        format!("  intro {}", intro_names.join(" ")),
        format!("  induction {target} with"),
        "  | nil =>".to_string(),
        "    first".to_string(),
        format!("    | (simp [{defs}]; done)"),
        format!("    | (intro h_when; {norm}; simp_all [{defs}]; done)"),
        format!(
            "    | (intro h_when; {norm}; simp only [{defs}] <;> (split <;> simp_all [{defs_br}]) <;> (try omega) <;> done)"
        ),
        "    | sorry".to_string(),
        "  | cons hd tl ih =>".to_string(),
        "    intro h_when".to_string(),
        format!("    {norm}"),
        "    first".to_string(),
        format!(
            "    | (simp only [{with_append}] at h_when ⊢ <;> (split <;> simp_all [{defs}]) <;> done)"
        ),
        format!("    | (simp only [{with_append}] at h_when ⊢ <;> simp_all [{defs}] <;> done)"),
        format!(
            "    | (simp only [{with_append}] <;> (split <;> simp_all [{defs}, h_when]) <;> done)"
        ),
        format!("    | (simp only [{with_append}] <;> simp_all [{defs}, h_when] <;> done)"),
        format!(
            "    | (simp only [{with_append}] <;> (split <;> simp_all [{defs_br}]) <;> (try omega) <;> done)"
        ),
        format!(
            "    | (simp only [{defs}] <;> (split <;> (simp_all [{defs_br}] <;> (try omega))) <;> done)"
        ),
        format!("    | (split <;> simp_all [{defs}, h_when] <;> done)"),
        format!("    | (simp_all [{defs}, h_when] <;> done)"),
        "    | sorry".to_string(),
    ];
    Some(AutoProof {
        support_lines: bridge_support,
        body: Tactic::raw(body),
        replaces_theorem: false,
    })
}

/// The four fns of a recognized sortedness-of-insert law, by SOURCE name.
struct SortednessPlan {
    /// The verified sorted-insertion fn (`insort`/`insert`).
    ins: String,
    /// The `sorted` Bool list predicate (premise + conclusion outer fn).
    sorted: String,
    /// The canonical Peano `≤` the insertion + sortedness compare with.
    cmp: String,
    /// The Bool `&&` combiner `sorted`'s recursive arm uses
    /// (`and`/`andB`: `f true y = y`, `f false _ = false`).
    and_fn: String,
    /// Index of the list given inducted on.
    target_idx: usize,
}

/// A canonical Bool `&&` combiner: `fn f(x: Bool, y: Bool) -> Bool` whose body is
/// `match x { true -> y ; false -> false }` (`f true y = y`, `f false _ = false`).
fn is_and_combiner(fd: &crate::ast::FnDef) -> bool {
    use crate::ast::{Expr, Literal, Pattern};
    if fd.return_type.trim() != "Bool" || fd.params.len() != 2 {
        return false;
    }
    if fd.params[0].1.trim() != "Bool" || fd.params[1].1.trim() != "Bool" {
        return false;
    }
    let (p0, p1) = (&fd.params[0].0, &fd.params[1].0);
    let Some(body) = fd.body.tail_expr() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    if !matches!(&subject.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == p0) {
        return false;
    }
    let mut true_ok = false;
    let mut false_ok = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                true_ok = matches!(&arm.body.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == p1);
            }
            Pattern::Literal(Literal::Bool(false)) => {
                false_ok = matches!(&arm.body.node, Expr::Literal(Literal::Bool(false)));
            }
            _ => return false,
        }
    }
    true_ok && false_ok
}

/// Recognize the sortedness-of-insertion shape:
/// `when sorted(L) -> sorted(<ins>(x, L)) => true`, where `<ins>` is the verified
/// fn (a sorted insertion that prepends with `List.concat`), `sorted` is the
/// adjacent-pairs Bool predicate built on a Peano `≤` and a Bool `&&` combiner.
/// This is the conditional-INDUCTIVE family the membership portfolio declines —
/// it needs a synthesized head-of-insert lower-bound lemma the bare IH cannot
/// supply.
fn recognize_conditional_sortedness(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<SortednessPlan> {
    use crate::ast::{Expr, Literal};
    let when = law.when.as_ref()?;
    // Conclusion `<sorted>(<ins>(.., L, ..)) => true`.
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    // Premise `<sorted>(L)`.
    let Expr::FnCall(p_callee, p_args) = &when.node else {
        return None;
    };
    let sorted = super::shared::expr_dotted_name(p_callee)?;
    if p_args.len() != 1 {
        return None;
    }
    let target_name = given_ident_name(&p_args[0])?.to_string();
    let target_idx = law.givens.iter().position(|g| g.name == target_name)?;
    // Conclusion LHS: same `sorted` applied to `<ins>(.., L, ..)`.
    let Expr::FnCall(c_callee, c_args) = &law.lhs.node else {
        return None;
    };
    if super::shared::expr_dotted_name(c_callee).as_deref() != Some(sorted.as_str())
        || c_args.len() != 1
    {
        return None;
    }
    let Expr::FnCall(ins_callee, ins_args) = &c_args[0].node else {
        return None;
    };
    let ins = super::shared::expr_dotted_name(ins_callee)?;
    if ins != vb.fn_name
        || !ins_args
            .iter()
            .any(|a| given_ident_name(a) == Some(target_name.as_str()))
    {
        return None;
    }
    // `ins` returns `List<_>` and recurses on its list param; `sorted` returns
    // Bool and recurses on its list param.
    let ins_fd = ctx.fn_def_by_name(&ins, ctx.active_module_scope().as_deref())?;
    let sorted_fd = ctx.fn_def_by_name(&sorted, ctx.active_module_scope().as_deref())?;
    use crate::codegen::recursion::detect::single_list_structural_param_index;
    if !ins_fd.return_type.trim().starts_with("List<")
        || single_list_structural_param_index(ins_fd).is_none()
        || sorted_fd.return_type.trim() != "Bool"
        || single_list_structural_param_index(sorted_fd).is_none()
    {
        return None;
    }
    // The single canonical Peano `≤` in the cone (the insertion + sortedness
    // comparator). Require the `Le` kind: `cmpTotal` (`f a b = false -> f b a =
    // true`) is the totality of a reflexive order, not of `<`.
    use crate::codegen::proof_recognize::NatCompareKind;
    let cmps = crate::codegen::proof_recognize::collect_nat_compare_ops_in_law(law, ctx);
    let cmp = cmps
        .iter()
        .find(|o| o.kind == NatCompareKind::Le)
        .map(|o| o.fn_name.clone())?;
    // The Bool `&&` combiner `sorted`'s recursive arm uses.
    let mut sorted_callees = BTreeSet::new();
    crate::codegen::proof_recognize::collect_called_fns_in_body(
        &sorted_fd.body,
        &mut sorted_callees,
    );
    let and_fn = sorted_callees.iter().find_map(|name| {
        ctx.fn_def_by_name(name, ctx.active_module_scope().as_deref())
            .filter(|fd| is_and_combiner(fd))
            .map(|_| name.clone())
    })?;
    Some(SortednessPlan {
        ins,
        sorted,
        cmp,
        and_fn,
        target_idx,
    })
}

/// Whether [`emit_conditional_sortedness_law`] will close this law universally —
/// the `omit_domain` statement driver reads this, kept in lockstep with the emit.
pub(in crate::codegen::lean) fn recognize_conditional_sortedness_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_conditional_sortedness(vb, law, ctx).is_some()
}

/// Close a sortedness-of-insertion law (`prop_77`, `prod/lemma_12`) as the
/// TRUE-universal `∀ x L, sorted L = true -> sorted(<ins> x L) = true`. Emits the
/// six support lemmas the bare list IH cannot supply — the `≤` bridge + its
/// totality, the HEAD-OF-INSERT lower bound (`<ins> x l`'s head is `≥` anything
/// `≤` both `x` and `l`'s old head — the structural fact the IH omits), and the
/// `sorted` head/tail destructors + cons constructor — then the main induction on
/// the list given, threading the `sorted` premise. FAIL-CLOSED: declines any
/// other shape (`None`), so the caller keeps the bounded proof; the synthesized
/// lemmas are themselves kernel-checked (a misrecognized fn fails its own lemma
/// and `#print axioms` withholds credit), and the law-scoped names avoid `_law_`
/// so the audit never mistakes a support lemma for the creditable theorem.
pub(in crate::codegen::lean) fn emit_conditional_sortedness_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let plan = recognize_conditional_sortedness(vb, law, ctx)?;
    let cmp = aver_name_to_lean(&plan.cmp);
    let ins = aver_name_to_lean(&plan.ins);
    let and_fn = aver_name_to_lean(&plan.and_fn);
    let sorted = aver_name_to_lean(&plan.sorted);
    let uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    // Law-scoped support lemma names (no `_law_`, so the audit never credits
    // them as the main theorem).
    let cmp_b = format!("{uid}_cmpB");
    let cmp_total = format!("{uid}_cmpTotal");
    let head_lb = format!("{uid}_headLb");
    let sort_tail = format!("{uid}_sortTail");
    let sort_hp = format!("{uid}_sortHeadPair");
    let sort_cons = format!("{uid}_sortConsLb");

    let support_block = format!(
        r#"theorem {cmp_b} : ∀ a b, ({cmp} a b = true) = (a ≤ b) := by
  intro a b
  induction a generalizing b with
  | zero => cases b <;> first | (simp [{cmp}]) | sorry
  | succ k ih => cases b <;> first | (simp [{cmp}, ih]) | sorry
theorem {cmp_total} : ∀ a b, {cmp} a b = false → {cmp} b a = true := by
  intro a b hp; rw [{cmp_b}]; rw [← Bool.not_eq_true, {cmp_b}, Nat.not_le] at hp; omega
theorem {head_lb} : ∀ (z x : Nat) (l : List Nat),
    {cmp} z x = true → (∀ a as, l = a :: as → {cmp} z a = true) → (∀ b bs, {ins} x l = b :: bs → {cmp} z b = true) := by
  intro z x l hzx hzhead b bs hins
  cases l with
  | nil => simp [{ins}] at hins; obtain ⟨rfl, _⟩ := hins; exact hzx
  | cons a as =>
    simp only [{ins}] at hins
    split at hins
    · simp only [List.singleton_append, List.cons.injEq] at hins
      obtain ⟨rfl, _⟩ := hins; exact hzx
    · simp only [List.singleton_append, List.cons.injEq] at hins
      obtain ⟨rfl, _⟩ := hins; exact hzhead a as rfl
theorem {sort_tail} : ∀ (a : Nat) (as : List Nat), {sorted} (a :: as) = true → {sorted} as = true := by
  intro a as hp
  cases as with
  | nil => simp [{sorted}]
  | cons a2 as2 => rw [{sorted}, {and_fn}] at hp; split at hp
                   · exact hp
                   · exact absurd hp (by simp)
theorem {sort_hp} : ∀ (a a2 : Nat) (as2 : List Nat), {sorted} (a :: a2 :: as2) = true → {cmp} a a2 = true := by
  intro a a2 as2 hp; rw [{sorted}, {and_fn}] at hp; split at hp
  · assumption
  · exact absurd hp (by simp)
theorem {sort_cons} : ∀ (z : Nat) (l : List Nat),
    (∀ a as, l = a :: as → {cmp} z a = true) → {sorted} l = true → {sorted} (z :: l) = true := by
  intro z l hlb hsl
  cases l with
  | nil => simp [{sorted}]
  | cons a as => rw [{sorted}, {and_fn}]; rw [hlb a as rfl]; simpa using hsl"#
    );
    // Emit the whole support block as ONE `support_lines` element (newlines
    // embedded) rather than per-line: `emit_verify_law_block` de-duplicates
    // support lines line-by-line (for shared private theorems across chunked
    // parts), which would silently drop a proof's REPEATED lines (two `split`
    // branches with the same `simp only`, two lemmas' `| nil => simp […]`) and
    // corrupt the multi-lemma block. As a single element the block is unique and
    // survives intact.
    let support_lines: Vec<String> = vec![support_block];

    let target = intro_names.get(plan.target_idx)?.clone();
    let body: Vec<String> = format!(
        r#"  intro {givens}
  induction {target} with
  | nil => intro _; simp [{ins}, {sorted}]
  | cons a as ih =>
    intro hsorted
    rw [{ins}]
    split
    · rename_i hxa
      rw [List.singleton_append, {sorted}, {and_fn}, hxa]
      simpa using hsorted
    · rename_i hxa
      rw [Bool.not_eq_true] at hxa
      rw [List.singleton_append]
      have hax : {cmp} a {x} = true := {cmp_total} {x} a hxa
      have hsas : {sorted} as = true := {sort_tail} a as hsorted
      have hins_sorted : {sorted} ({ins} {x} as) = true := ih hsas
      apply {sort_cons}
      · apply {head_lb} a {x} as hax
        intro a' as' has; subst has
        exact {sort_hp} a a' as' hsorted
      · exact hins_sorted"#,
        givens = intro_names.join(" "),
        x = intro_names.first()?,
    )
    .lines()
    .map(|l| l.to_string())
    .collect();

    Some(AutoProof {
        support_lines,
        body: Tactic::raw(body),
        replaces_theorem: false,
    })
}

/// `discovered` carries the lemma names of an IR-pinned
/// `ProofStrategy::SimpOverLemmas` (the discovery feedback loop): the emits
/// below add them to the law's simp sets, embed their texts (first user
/// only), and try a lemma-first `simp only … <;> omega` fast path before
/// induction. Empty for a plain `Induction` pin — the output is then
/// byte-identical to the pre-feedback emit.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_structural_induction_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    _theorem_base: &str,
    _quant_params: &str,
    _theorem_prop: &str,
    discovered: &[String],
) -> Option<AutoProof> {
    // A `when` premise blocks the structural-induction routing below — the
    // inductive arms re-establish no premise, which is the HARD
    // conditional-inductive family (sortedness/insertion) tracked under its own
    // brief (`prompts/conditional-inductive-executor.md`). The EASY conditional
    // comparison-bridge family (`prop_70 leSucc`) is closed instead by
    // `emit_conditional_comparison_bridge_law`, reached pin-independently in the
    // early arm of `emit_verify_law_forall_auto_proof_inner` BEFORE this routing.
    // This function is in fact never reached for a `when`-law today —
    // `classify_law_strategy` never pins `Induction` on one, and `SimpOverLemmas`
    // only re-pins an already-`Induction`-pinned law — so the structural path
    // simply declines every conditional law. When the conditional-inductive
    // strategy lands it will route HERE (induction on the list given, threading
    // the premise); until then, decline so the caller keeps the bounded
    // guarded-domain fallback.
    if law.when.is_some() {
        return None;
    }

    // Route induction to the variable the VERIFIED fn actually recurses on,
    // not merely the first recursive-typed `given`. A list-homomorphism like
    // `plus (count n xs) (count n ys) = count n (xs ++ ys)` has BOTH a Nat
    // given (`n`) and List givens: inducting on `n` gets nowhere — neither
    // `count` nor the append recurses on it — and falls through to `sorry`.
    // The fn under verification (`count`) structurally recurses on its LIST
    // parameter, so list-induction on the list given is what makes both sides
    // peel in lockstep (the cons IH plus `omega` for the `1 + (m+n) = (1+m)+n`
    // residual). Generic: ask which parameter shape the verified fn recurses
    // on and prefer the matching given, rather than hard-coding a precedence.
    let verified_recurses_on_list = ctx
        .fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())
        .is_some_and(|fd| {
            crate::codegen::recursion::detect::single_list_structural_param_index(fd).is_some()
        });

    let list_target = find_list_induction_target(law);
    let sum_target = find_induction_target(law, ctx);

    // (a) Verified fn recurses on a `List<T>` and the law has a list given:
    //     structural nil/cons induction on that list (the Lean counterpart to
    //     Dafny's `|xs| == 0 / xs[1..]` list-given idiom, #409 Gap A). Closes
    //     list-homomorphism universals that inducting on a co-occurring Nat
    //     given would leave at `sorry`.
    if verified_recurses_on_list && let Some(target_idx) = list_target {
        return emit_list_induction(vb, law, ctx, intro_names, target_idx, discovered);
    }

    // (a2) The verified fn peels a constructor off BOTH args synchronously
    //      (the `max`/`min` monoid shape) and the law is a commutativity /
    //      associativity statement whose givens are all that Peano type:
    //      `induction g1 generalizing <rest> with … cases <rest> …`. Inducting
    //      on a single arg (branch (b)) leaves the other arg's peel stuck at
    //      `sorry`; generalizing + case-splitting the remaining givens exposes
    //      every constructor pairing so the cons IH applies. Tried before the
    //      single-variable structural branch.
    if let Some(proof) = emit_both_args_peeling_law(vb, law, ctx, intro_names) {
        return Some(proof);
    }

    // (b) A `given` is a user-defined recursive sum type: structural induction
    //     over its variants.
    if let Some((target_idx, _target_name, type_name)) = sum_target {
        let (_, variants) = find_sum_type(ctx, type_name)?;
        if has_indirect_variants(variants, type_name) {
            return None;
        }
        return emit_simple_induction(
            vb,
            law,
            ctx,
            intro_names,
            target_idx,
            type_name,
            variants,
            discovered,
        );
    }

    // (c) No sum-type given, but a builtin `List<T>` given is present.
    if let Some(target_idx) = list_target {
        return emit_list_induction(vb, law, ctx, intro_names, target_idx, discovered);
    }

    None
}

/// First `given` whose declared type is a builtin `List<T>` — Lean's
/// nil/cons induction target.
fn find_list_induction_target(law: &VerifyLaw) -> Option<usize> {
    law.givens
        .iter()
        .position(|given| given.type_name.trim().starts_with("List<"))
}

/// Which map query a fold-homomorphism law inspects on the map built by the
/// verified fold fn. `Get` covers the `Map.get` / `Option.withDefault(Map.get…)`
/// shape (value homomorphism); `Has` covers the `Map.has` presence shape.
enum MapFoldQuery {
    Get,
    Has,
}

/// A recognized **map-fold-homomorphism** law: one side queries
/// `Map.get`/`Map.has` over `<verified-fold> <list-given>` at a per-element
/// `<key-given>`, where the verified fold's cons step updates the map at the
/// cons head (e.g. `countWords` = fold of `incCount`, itself a `Map.set` keyed
/// on the inserted word). The matched-key branch closes via the self-key Map
/// lemmas; the DIFFERENT-key branch needs the general-key prelude lemmas
/// (`AverMap.get_set_ne` / `AverMap.has_set`) with the `head ≠ key` fact the
/// `by_cases` on key-equality puts in scope.
struct MapFoldHomomorphism {
    query: MapFoldQuery,
    /// Lean name of the per-element key given (the second arg to `get`/`has`).
    key_lean: String,
}

/// Pull the dotted source callee name off a `Map.get`/`Map.has`/`…` call.
fn call_dotted_name(expr: &crate::ast::Spanned<crate::ast::Expr>) -> Option<String> {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(callee, _) => super::shared::expr_dotted_name(callee),
        _ => None,
    }
}

/// `Map.get(<fold>(<list>), <key>)` / `Map.has(<fold>(<list>), <key>)` where
/// `<fold>` is the verified fn applied to the list given and `<key>` is another
/// given. Returns the key given's name when matched.
fn map_query_over_fold(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    fold_fn: &str,
    list_given: &str,
    given_names: &[String],
) -> Option<String> {
    use crate::ast::Expr;
    let Expr::FnCall(_, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    // First arg: `<fold_fn>(<list_given>)`.
    let Expr::FnCall(map_callee, map_args) = &args[0].node else {
        return None;
    };
    if super::shared::expr_dotted_name(map_callee).as_deref() != Some(fold_fn) {
        return None;
    }
    if map_args.len() != 1
        || !matches!(&map_args[0].node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == list_given)
    {
        return None;
    }
    // Second arg: a bare given that is NOT the list itself (the per-element key).
    let key = match &args[1].node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
        _ => return None,
    };
    if key == list_given || !given_names.iter().any(|g| g == &key) {
        return None;
    }
    Some(key)
}

/// Recognize the map-fold-homomorphism shape on either side of the law. The
/// verified fn must be a single-list-structural fold whose cons step updates a
/// map via `Map.set` (so the general-key prelude lemmas are the missing piece).
fn recognize_map_fold_homomorphism(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    target_idx: usize,
) -> Option<MapFoldHomomorphism> {
    use crate::ast::Expr;
    // The verified fn folds a single list and its cons step ultimately calls
    // `Map.set` (directly or through one helper level).
    let fd = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())?;
    crate::codegen::recursion::detect::single_list_structural_param_index(fd)?;
    if !fold_step_updates_map(fd, ctx) {
        return None;
    }

    let list_given = &law.givens.get(target_idx)?.name;
    let given_names: Vec<String> = law.givens.iter().map(|g| g.name.clone()).collect();

    // Scan a side for `Option.withDefault(Map.get(fold(list), key), d)` (Get) or
    // `Map.has(fold(list), key)` (Has). `withDefault` may be the immediate
    // wrapper of a `get`, or `get` may appear bare (no default).
    let recognize_side = |side: &crate::ast::Spanned<Expr>| -> Option<(MapFoldQuery, String)> {
        match call_dotted_name(side).as_deref() {
            Some("Option.withDefault") | Some("Map.getD") => {
                let Expr::FnCall(_, args) = &side.node else {
                    return None;
                };
                let inner = args.first()?;
                if call_dotted_name(inner).as_deref() != Some("Map.get") {
                    return None;
                }
                let key = map_query_over_fold(inner, &vb.fn_name, list_given, &given_names)?;
                Some((MapFoldQuery::Get, key))
            }
            Some("Map.get") => {
                let key = map_query_over_fold(side, &vb.fn_name, list_given, &given_names)?;
                Some((MapFoldQuery::Get, key))
            }
            Some("Map.has") => {
                let key = map_query_over_fold(side, &vb.fn_name, list_given, &given_names)?;
                Some((MapFoldQuery::Has, key))
            }
            _ => None,
        }
    };

    let (query, key) = recognize_side(&law.lhs).or_else(|| recognize_side(&law.rhs))?;
    let key_lean = intro_names
        .get(given_names.iter().position(|g| g == &key)?)?
        .clone();
    Some(MapFoldHomomorphism { query, key_lean })
}

/// Whether the verified fold's cons step updates a map — its body (or a helper
/// it calls one level deep) contains a `Map.set` call. Conservative: a false
/// positive only widens the simp set of a `sorry`-floored ladder.
fn fold_step_updates_map(fd: &crate::ast::FnDef, ctx: &CodegenContext) -> bool {
    fn expr_calls_map_set(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
        use crate::ast::Expr;
        match &expr.node {
            Expr::FnCall(callee, args) => {
                super::shared::expr_dotted_name(callee).as_deref() == Some("Map.set")
                    || args.iter().any(expr_calls_map_set)
                    || expr_calls_map_set(callee)
            }
            Expr::Attr(base, _) => expr_calls_map_set(base),
            Expr::BinOp(_, l, r) => expr_calls_map_set(l) || expr_calls_map_set(r),
            Expr::Neg(inner) | Expr::ErrorProp(inner) => expr_calls_map_set(inner),
            Expr::Match { subject, arms } => {
                expr_calls_map_set(subject) || arms.iter().any(|a| expr_calls_map_set(&a.body))
            }
            Expr::Constructor(_, inner) => inner.as_deref().is_some_and(expr_calls_map_set),
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                items.iter().any(expr_calls_map_set)
            }
            _ => false,
        }
    }
    fn body_calls_map_set(fd: &crate::ast::FnDef) -> bool {
        fd.body.stmts().iter().any(|s| match s {
            crate::ast::Stmt::Expr(e) | crate::ast::Stmt::Binding(_, _, e) => expr_calls_map_set(e),
        })
    }
    fn collect_called_fns(
        expr: &crate::ast::Spanned<crate::ast::Expr>,
        out: &mut BTreeSet<String>,
    ) {
        use crate::ast::Expr;
        if let Expr::FnCall(callee, args) = &expr.node {
            if let Some(n) = super::shared::expr_dotted_name(callee) {
                out.insert(n);
            }
            args.iter().for_each(|a| collect_called_fns(a, out));
        }
        match &expr.node {
            Expr::FnCall(callee, _) => collect_called_fns(callee, out),
            Expr::Attr(base, _) => collect_called_fns(base, out),
            Expr::BinOp(_, l, r) => {
                collect_called_fns(l, out);
                collect_called_fns(r, out);
            }
            Expr::Neg(i) | Expr::ErrorProp(i) => collect_called_fns(i, out),
            Expr::Match { subject, arms } => {
                collect_called_fns(subject, out);
                arms.iter().for_each(|a| collect_called_fns(&a.body, out));
            }
            Expr::Constructor(_, Some(i)) => collect_called_fns(i, out),
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                items.iter().for_each(|i| collect_called_fns(i, out));
            }
            _ => {}
        }
    }
    if body_calls_map_set(fd) {
        return true;
    }
    let mut callees = BTreeSet::new();
    for s in fd.body.stmts() {
        match s {
            crate::ast::Stmt::Expr(e) | crate::ast::Stmt::Binding(_, _, e) => {
                collect_called_fns(e, &mut callees)
            }
        }
    }
    callees.iter().any(|name| {
        ctx.fn_def_by_name(name, ctx.active_module_scope().as_deref())
            .is_some_and(body_calls_map_set)
    })
}

/// Emit the dedicated map-fold-homomorphism proof: `induction <list>`, and in
/// the cons arm `by_cases <head> = <key>` to split matched vs different key,
/// then `cases` the inner map lookup. The matched branch closes via the self-key
/// Map lemmas; the different-key branch fires the conditional `AverMap.get_set_ne`
/// (its `head ≠ key` side-goal discharged from the `by_cases` `false` fact) and
/// the general `AverMap.has_set`. Each arm keeps a `sorry` floor so a shape the
/// kernel can't actually close degrades to an honest sorry (caught by the
/// universal metric), never an unsolved-goals build error — all tactics used
/// (`simp_all`, `omega`, the kernel-proved Map lemmas) are sound.
fn emit_map_fold_homomorphism(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    target_idx: usize,
    plan: &MapFoldHomomorphism,
) -> AutoProof {
    let simp_list = law_simp_defs(ctx, vb, law)
        .into_iter()
        .collect::<Vec<_>>()
        .join(", ");
    let target_lean = &intro_names[target_idx];
    let key = &plan.key_lean;
    let fold_lean = aver_name_to_lean(&vb.fn_name);

    // The general-key Map lemmas plus the bool/decidable bridges `simp` needs to
    // collapse the matched/different-key facts. `get_set_self`/`get_set_ne` for
    // the Get shape, `has_set` for the Has shape; including both is harmless
    // (a non-matching lemma simply never fires).
    // Do NOT add `AverMap.get`/`AverMap.has` to the cons-arm rewrite set: they
    // would unfold `has`/`get` to their `.any`/`match` forms BEFORE the
    // `*_set*` lemmas (stated over `AverMap.has`/`AverMap.get`) can match the
    // `… (set m k v) …` head. The defs are unfolded only in the nil arm.
    let map_lemmas = match plan.query {
        MapFoldQuery::Get => "AverMap.get_set_self, AverMap.get_set_ne, beq_iff_eq",
        MapFoldQuery::Has => "AverMap.has_set, beq_iff_eq",
    };
    // The Get shape leaves a `+ 1` / `0` Peano residual the IH closes under
    // `omega`; the Has shape is pure Bool with nothing for `omega` to do.
    let close = match plan.query {
        MapFoldQuery::Get => " <;> omega",
        MapFoldQuery::Has => "",
    };
    // List membership only matters for the Has shape; harmless in the unfold set
    // otherwise. `at *` rewrites the IH alongside the goal so both expose the
    // same `head ?= key` scrutinee.
    let nil_simp = format!("{simp_list}, AverMap.get, AverMap.has");

    // Cons arm. `simp only [defs] at *` unfolds the fold + map updater (exposing
    // `AverMap.set … head …` and the inner `AverMap.get … head`); `by_cases head
    // = key` splits matched vs different key; `cases` the inner lookup. Each rung
    // is gated on `; done` (or `<;> omega`, itself closing) so a `simp_all` that
    // makes progress but leaves a goal THROWS and `first` falls through — without
    // it, the goal-leaving `simp_all` counts as success and `first` would stop at
    // an unsolved-goals build error. Two sound rungs before the `sorry` floor:
    // (1) Map lemmas + IH, with `omega` (Get arithmetic residual) or `try rfl;
    // done` (the residual `(decide (key = head) || …) = (key == head || …)`, where
    // Bool `==` and `decide (… = …)` are definitionally equal for the key type);
    // (2) the same set, plain `; done`. A shape the kernel can't close degrades to
    // the honest `sorry` (caught by the universal metric), never a build error.
    let rung_close = match plan.query {
        MapFoldQuery::Get => format!("simp_all [{map_lemmas}, hkey, hget]{close}"),
        MapFoldQuery::Has => format!("simp_all [{map_lemmas}, hkey, hget]; try rfl; done"),
    };
    let cons_arm = format!(
        "| cons head tail ih => simp only [{simp_list}, List.contains_cons] at * <;> by_cases hkey : head = {key} <;> cases hget : AverMap.get ({fold_lean} tail) head <;> first | ({rung_close}) | (simp_all [{map_lemmas}, hkey, hget]; done) | sorry"
    );

    let proof_lines = vec![
        format!("  intro {}", intro_names.join(" ")),
        format!("  induction {target_lean} with"),
        format!("  | nil => first | (simp [{nil_simp}]) | sorry"),
        format!("  {cons_arm}"),
    ];

    AutoProof {
        support_lines: Vec::new(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    }
}

/// A recognized `fun_induction` target: the Lean fn name whose auto-derived
/// `<fn>.induct` splits the law's goal, plus the goal-argument names to apply it
/// at. `fun_induction <fn> <args>` matches `<fn> <args>` in the goal and case-
/// splits exactly that fn's own (possibly two-deep) case tree with the precise
/// IH — no arg choice, no `generalizing`, no tail `cases`. It CLOSES only when
/// the recursion key is a FREE VARIABLE; on a composite scrutinee it abstracts
/// the term and loses the IH, so we require every argument to be a bare given.
struct FunInductionTarget {
    fn_lean: String,
    /// Goal-argument Lean names (the intro names), in call order.
    args: Vec<String>,
}

/// Whether a fn's body case-splits with a `match` — the condition under which
/// Lean's equation compiler derives a non-trivial `<fn>.induct`, so the fn is a
/// usable `fun_induction` target (self-recursive or not).
fn fn_body_has_match(fd: &crate::ast::FnDef) -> bool {
    fn expr_has_match(expr: &crate::ast::Spanned<crate::ast::Expr>) -> bool {
        use crate::ast::Expr;
        match &expr.node {
            Expr::Match { .. } => true,
            Expr::FnCall(callee, args) => expr_has_match(callee) || args.iter().any(expr_has_match),
            Expr::Attr(base, _) | Expr::Neg(base) | Expr::ErrorProp(base) => expr_has_match(base),
            Expr::BinOp(_, l, r) => expr_has_match(l) || expr_has_match(r),
            Expr::Constructor(_, Some(inner)) => expr_has_match(inner),
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                items.iter().any(expr_has_match)
            }
            _ => false,
        }
    }
    fd.body.stmts().iter().any(|s| match s {
        crate::ast::Stmt::Expr(e) | crate::ast::Stmt::Binding(_, _, e) => expr_has_match(e),
    })
}

/// Find `fun_induction` targets in the law's goal: every call to a user-
/// recursive fn whose arguments are ALL bare free-variable givens (so the
/// recursion key is a free variable, the case `fun_induction` closes). The
/// law's SUBJECT fn (`vb.fn_name`) is ordered FIRST — it is the fn the law is
/// about and the likeliest to drive the split — then every other distinct
/// user-recursive fn called on free-var args, in goal order. A composite-arg
/// call (`butlast (xs ++ ys)`) is rejected: `fun_induction` there abstracts the
/// append term and loses the `xs`/`ys` link, so that call is not a target.
///
/// Returning ALL candidates (not just one) matters when the right driver is not
/// the subject fn: prop_29 (`elem x (ins1 x xs) = true`) has a composite-arg
/// subject call `elem (ins1 …)` (no target) but a free-var `ins1 x xs` call,
/// and inducting `ins1`'s case tree is exactly what closes it. Emitting one
/// `first` arm per candidate lets the right one win; the rest fail-through.
/// (A goal whose driver is NOT recursive — e.g. prop_49's `butlastConcat`, for
/// which Lean derives no `.induct` because the fn never self-calls — is simply
/// not a target and stays on the existing ladder.)
///
/// PURELY ADDITIVE: the targets only drive `first | (fun_induction …)… |
/// <existing ladder>` arms. Each `fun_induction` either CLOSES the goal or
/// FAILS (no theorem / goal mismatch / non-closing closer), falling to the next
/// arm and ultimately to today's ladder — it can only ADD closures.
fn find_fun_induction_targets(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Vec<FunInductionTarget> {
    // Map a given name to the Lean name it is intro'd as (givens and intro
    // names are positional, so the index lines them up).
    let given_to_intro = |name: &str| -> Option<String> {
        law.givens
            .iter()
            .position(|g| g.name == name)
            .and_then(|i| intro_names.get(i).cloned())
    };

    // A `fun_induction` target fn must have an auto-derived `<fn>.induct`. Lean's
    // equation compiler generates one ONLY for fns it compiles via structural /
    // well-founded recursion — i.e. self-recursive fns. A non-recursive matching
    // fn (`butlastConcat`, which just splits on `ys` then calls helpers) has NO
    // `.induct`, so a `fun_induction` on it errors ("No functional induction
    // theorem"); `first` would recover, but emitting a guaranteed-dead arm is
    // noise, so restrict to the recursive pure fns — exactly the `.induct` set.
    // (`fn_body_has_match` is a redundant sanity guard: a recursive fn always
    // case-splits, but a fn lifted to a non-matching builtin would not.)
    let recursive_names = crate::codegen::lean::recursive_pure_fn_names(ctx);
    let induct_fns: std::collections::HashSet<String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(ctx.fn_defs.iter())
        .filter(|fd| {
            crate::codegen::common::is_pure_fn(fd)
                && recursive_names.contains(&fd.name)
                && fn_body_has_match(fd)
        })
        .map(|fd| fd.name.clone())
        .collect();

    // Collect every in-goal call to such a fn whose args are all bare givens,
    // recording call order (outermost first via a pre-order walk).
    let mut candidates: Vec<FunInductionTarget> = Vec::new();
    fn walk(
        expr: &crate::ast::Spanned<crate::ast::Expr>,
        recursive: &std::collections::HashSet<String>,
        given_to_intro: &dyn Fn(&str) -> Option<String>,
        out: &mut Vec<FunInductionTarget>,
    ) {
        use crate::ast::Expr;
        if let Expr::FnCall(callee, args) = &expr.node
            && let Some(name) = super::shared::expr_dotted_name(callee)
            && recursive.contains(&name)
        {
            // Every argument must be a bare free-variable given — the recursion
            // key is then a free variable and `fun_induction` closes.
            let mut arg_leans = Vec::with_capacity(args.len());
            let mut all_free = !args.is_empty();
            for a in args {
                let leaf = match &a.node {
                    Expr::Ident(n) | Expr::Resolved { name: n, .. } => given_to_intro(n),
                    _ => None,
                };
                match leaf {
                    Some(l) => arg_leans.push(l),
                    None => {
                        all_free = false;
                        break;
                    }
                }
            }
            if all_free {
                out.push(FunInductionTarget {
                    fn_lean: aver_name_to_lean(&name),
                    args: arg_leans,
                });
            }
        }
        // Recurse into sub-expressions so a nested target (e.g. the RHS fn of
        // prop_49) is still found even when the outer call is composite-arg.
        match &expr.node {
            Expr::FnCall(callee, args) => {
                walk(callee, recursive, given_to_intro, out);
                args.iter()
                    .for_each(|a| walk(a, recursive, given_to_intro, out));
            }
            Expr::Attr(base, _) | Expr::Neg(base) | Expr::ErrorProp(base) => {
                walk(base, recursive, given_to_intro, out)
            }
            Expr::BinOp(_, l, r) => {
                walk(l, recursive, given_to_intro, out);
                walk(r, recursive, given_to_intro, out);
            }
            Expr::Match { subject, arms } => {
                walk(subject, recursive, given_to_intro, out);
                arms.iter()
                    .for_each(|a| walk(&a.body, recursive, given_to_intro, out));
            }
            Expr::Constructor(_, Some(inner)) => walk(inner, recursive, given_to_intro, out),
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => items
                .iter()
                .for_each(|i| walk(i, recursive, given_to_intro, out)),
            _ => {}
        }
    }
    walk(&law.lhs, &induct_fns, &given_to_intro, &mut candidates);
    walk(&law.rhs, &induct_fns, &given_to_intro, &mut candidates);

    // Order subject-first, dedup on (fn, args) keeping goal order otherwise.
    let subject_lean = aver_name_to_lean(&vb.fn_name);
    candidates.sort_by_key(|c| c.fn_lean != subject_lean);
    let mut seen: BTreeSet<(String, Vec<String>)> = BTreeSet::new();
    candidates.retain(|c| seen.insert((c.fn_lean.clone(), c.args.clone())));
    candidates
}

/// The additive `fun_induction` first-rung. Given the existing induction
/// `body_lines` (everything after `intro`), wrap them as
/// `first | (fun_induction <fn> <args> <;> closer)… | (<existing body>)`, with
/// one `fun_induction` arm per recognized `targets` entry (subject-first). The
/// closer ends in throw-on-leftover tactics (`done` / `omega` / `<;> omega`) so
/// each `fun_induction` either CLOSES the goal or FAILS, falling to the next arm
/// and ultimately to the existing body byte-for-byte (modulo a 3-space indent
/// under the final `first` arm). `simp_defs` is the law's def simp set
/// (`law_simp_defs`-derived, comma-joined).
fn wrap_with_fun_induction_rung(
    intro_line: String,
    body_lines: Vec<String>,
    targets: &[FunInductionTarget],
    simp_defs: &str,
) -> Vec<String> {
    let defs = if simp_defs.is_empty() {
        String::new()
    } else {
        format!("[{simp_defs}]")
    };
    // The closer: after `fun_induction` splits the case tree it leaves one goal
    // per arm with the precise IH. `simp_all [defs]` unfolds the law's defs (and
    // any proven reflexivity support lemmas the def set carries — see
    // `lean_refl_support`) and applies every IH; the three alternatives discharge
    // a pure-equational goal (`done`), a linear-arith residual (`omega`), or a
    // per-subgoal residual (`<;> omega`). Every alternative throws on a leftover
    // goal, so a non-closing split fails the whole rung and `first` falls to the
    // next arm — never a silent unsolved-goals build error. CRUCIALLY the closer
    // contains NO nested `induction`: inducting on a variable `fun_induction`
    // already consumed/generalized raises an ill-typed-motive error that `first`
    // does NOT backtrack over (a hard build failure that would taint the whole
    // file), so a residual needing a fresh induction (e.g. `eqNat x x = true`)
    // is discharged by a PROVEN `_refl` lemma carried in `simp_defs`, never by an
    // in-closer induction. All tactics here (`simp_all`/`omega`) terminate and
    // are sound, so the rung is purely additive and adds no axioms.
    // Last alternative: after `simp_all` normalizes, exhaust the goal's nested
    // `if`/`match` branch points with `repeat' split` before `omega`. The cheaper
    // rungs leave an unsplit `if <userBool> then …` (e.g. `if eqNat x z then …`)
    // that `omega` treats as an opaque atom and cannot relate across the two
    // sides; splitting every conditional first collapses each branch to concrete
    // arithmetic `omega` closes. `repeat' split` is core Lean (Mathlib's
    // `split_ifs` is unavailable here); it creates no new conditionals and halts
    // when none remain, so it is bounded by the finitely-many `if`s already
    // present, and the trailing `omega` throws on any non-arithmetic leftover —
    // the rung stays sound-by-floor and falls through to `sorry`. Gated LAST so
    // the cheaper `simp_all` rungs fire first. Closes the count-over-insert
    // family (count/insert/sort branching on `eqNat`/`lessEq`).
    let closer = format!(
        "first | (simp_all {defs}; done) | (simp_all {defs}; omega) | (simp_all {defs} <;> omega) | (simp_all {defs} <;> (repeat' split) <;> omega)"
    );
    let mut out = vec![intro_line, "  first".to_string()];
    for t in targets {
        out.push(format!(
            "  | (fun_induction {} {} <;> ({}))",
            t.fn_lean,
            t.args.join(" "),
            closer
        ));
    }
    out.push("  | (".to_string());
    // Re-indent the existing body under the final `first` arm and close the
    // parenthesis on the last line. The body is emitted verbatim otherwise.
    for line in &body_lines {
        out.push(format!("  {line}"));
    }
    if let Some(last) = out.last_mut() {
        last.push(')');
    }
    out
}

/// A proved `nil`-helper for a list-truncating cone fn: `f … [] … = []`, with the
/// list arg the fn case-splits set to `[]` and every other param a fresh
/// universal, and the proof case-splitting the DRIVER param (the synchronously-
/// decremented `Nat` for take/drop, or the structural list for `zip`).
///
/// SOUND BY FLOOR: the proof is `first | (… cases … <;> simp [f]) | sorry`, so a
/// fn that does NOT actually satisfy the helper degrades to a `sorry`-floored
/// (non-kernel-clean) lemma — its name, cited in the rung's `simp_all` set, then
/// taints any closure that uses it into a `sorryAx` dependency the `#print
/// axioms` universal gate catches. It can NEVER mint a false theorem.
struct NilHelper {
    text: String,
    name: String,
}

/// Emit the `nil`-helper for a take/drop-shaped cone fn — one with a single
/// structural `List` param (`single_list_structural_param_index`) AND a `Nat`
/// param it decrements synchronously (`param_decremented_in_recursion`). The
/// helper is `f a₀ … [] … aₙ = []` with the list param `[]` and the decremented
/// `Nat` the case-split driver: `intro …; cases <driver> <;> simp [f]`. (take's
/// `Z -> []` and drop's `Z -> xs`-at-`[]` both reduce, so the helper holds.)
fn take_drop_nil_helper(
    fd: &crate::ast::FnDef,
    law_uid: &str,
    ctx: &CodegenContext,
) -> Option<NilHelper> {
    use crate::codegen::recursion::detect::{
        param_decremented_in_recursion, single_list_structural_param_index,
    };
    let list_idx = single_list_structural_param_index(fd)?;
    // The synchronously-decremented Nat param (take/drop's `n`).
    let nat_idx = fd.params.iter().enumerate().position(|(i, (_, ty))| {
        i != list_idx
            && (ty.trim() == "Nat"
                || crate::codegen::proof_recognize::peano_type_named(ctx, ty.trim()).is_some())
            && param_decremented_in_recursion(fd, i)
    })?;
    let f = aver_name_to_lean(&fd.name);
    let name = format!("{law_uid}_{f}_nil");
    // Bind each param to a fresh universal name except the list param, which is
    // `[]`. The driver `Nat` is case-split; the others ride along untouched.
    let driver = "n_d";
    let args: Vec<String> = (0..fd.params.len())
        .map(|i| {
            if i == list_idx {
                "[]".to_string()
            } else if i == nat_idx {
                driver.to_string()
            } else {
                format!("p_{i}")
            }
        })
        .collect();
    let binders: Vec<String> = (0..fd.params.len())
        .filter(|i| *i != list_idx)
        .map(|i| {
            if i == nat_idx {
                driver.to_string()
            } else {
                format!("p_{i}")
            }
        })
        .collect();
    let binders = binders.join(" ");
    let body = super::intro_then_first(
        std::slice::from_ref(&binders),
        vec![format!("cases {driver} <;> simp [{f}]")],
    );
    let text = super::support_theorem(
        &format!(
            "theorem {name} : ∀ {binders}, {f} {args} = [] := by",
            args = args.join(" ")
        ),
        body,
    );
    Some(NilHelper { text, name })
}

/// Emit the `nil`-helper for a `zip`-shaped cone fn — one whose body matches its
/// FIRST list param, then (in the cons arm) its SECOND list param, recursing on
/// both tails. The needed helper is `f xs [] = []`: when the second list is
/// empty the cons arm returns `[]`, and the first-list-nil arm returns `[]` too,
/// so `cases <first list> <;> simp [f]` closes it. Returns the helper with the
/// SECOND list param `[]` and the first the case-split driver.
fn zip_nil_helper(
    fd: &crate::ast::FnDef,
    law_uid: &str,
    ctx: &CodegenContext,
) -> Option<NilHelper> {
    use crate::codegen::recursion::detect::{
        param_decremented_in_recursion, single_list_structural_param_index,
    };
    // A zip-shape has NO synchronously-decremented Nat (that is the take/drop
    // shape, handled above) — it threads two lists. Require a structural FIRST
    // list and a SECOND list param, and no decremented Nat param.
    let first_list = single_list_structural_param_index(fd)?;
    let has_decremented_nat = fd.params.iter().enumerate().any(|(i, (_, ty))| {
        i != first_list
            && (ty.trim() == "Nat"
                || crate::codegen::proof_recognize::peano_type_named(ctx, ty.trim()).is_some())
            && param_decremented_in_recursion(fd, i)
    });
    if has_decremented_nat {
        return None;
    }
    let second_list = fd.params.iter().enumerate().position(|(i, (_, ty))| {
        i != first_list && (ty.trim_start().starts_with("List<") || ty.trim() == "List")
    })?;
    let f = aver_name_to_lean(&fd.name);
    let name = format!("{law_uid}_{f}_nil");
    let driver = "xs_d";
    let args: Vec<String> = (0..fd.params.len())
        .map(|i| {
            if i == second_list {
                "[]".to_string()
            } else if i == first_list {
                driver.to_string()
            } else {
                format!("p_{i}")
            }
        })
        .collect();
    let binders: Vec<String> = (0..fd.params.len())
        .filter(|i| *i != second_list)
        .map(|i| {
            if i == first_list {
                driver.to_string()
            } else {
                format!("p_{i}")
            }
        })
        .collect();
    let binders = binders.join(" ");
    let body = super::intro_then_first(
        std::slice::from_ref(&binders),
        vec![format!("cases {driver} <;> simp [{f}]")],
    );
    let text = super::support_theorem(
        &format!(
            "theorem {name} : ∀ {binders}, {f} {args} = [] := by",
            args = args.join(" ")
        ),
        body,
    );
    Some(NilHelper { text, name })
}

/// Bare given name a goal-argument `Expr` refers to (an `Ident`/`Resolved`),
/// `None` for anything composite.
fn given_ident_name(expr: &crate::ast::Spanned<crate::ast::Expr>) -> Option<&str> {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
        _ => None,
    }
}

/// A recognized **count-composition** law: `f(op(a, b), c) = f(a, f(b, c))`
/// (in either orientation), where the verified fn `f` recurses on a `Nat` driver
/// with a nested list `match` in its `S` arm (the take/drop shape), `op` is a
/// recognized canonical Peano addition, `a`/`b` are `Nat` givens and `c` is the
/// list given. The standard proof inducts on the INNER count `b` generalizing the
/// list `c` (the outer count `a` fixed), unfolding `f` one step on both sides and
/// chaining the IH at the peeled tail — which the family's existing rungs (driver
/// = `a`, or `fun_induction`) do not produce. Returns the inner-count given's name
/// and the list given's name.
struct CountComposition {
    /// Source name of the inner `Nat` count given to induct on (`b`).
    inner_count: String,
    /// Source name of the list given to generalize over (`c`).
    list_given: String,
}

/// Recognize the count-composition shape on the law. The verified fn `f` must be
/// a single-list-structural fn with a synchronously-decremented `Nat` driver, and
/// the law's two sides must be `f(op(a, b), c)` and `f(a, f(b, c))` for a
/// recognized Peano-addition `op` over bare `Nat`/list givens.
fn recognize_count_composition(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<CountComposition> {
    use crate::ast::Expr;
    use crate::codegen::recursion::detect::{
        param_decremented_in_recursion, single_list_structural_param_index,
    };
    if law.when.is_some() {
        return None;
    }
    let fd = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())?;
    let list_idx = single_list_structural_param_index(fd)?;
    // The fn must drive on a synchronously-decremented Nat param (the take/drop
    // `n`), so its first arg is the count peeled in lockstep with the list.
    if !fd.params.iter().enumerate().any(|(i, (_, ty))| {
        i != list_idx
            && (ty.trim() == "Nat"
                || crate::codegen::proof_recognize::peano_type_named(ctx, ty.trim()).is_some())
            && param_decremented_in_recursion(fd, i)
    }) {
        return None;
    }

    // The recognized Peano-addition fns in the law's cone (so we know `op` is a
    // genuine `+`, bridged to builtin addition by `lean_nat_lift_support`).
    let add_fns: BTreeSet<String> =
        crate::codegen::proof_recognize::collect_nat_arith_ops_in_law(law, ctx)
            .into_iter()
            .filter(|op| op.kind == crate::codegen::proof_recognize::NatArithKind::Add)
            .map(|op| op.fn_name)
            .collect();
    if add_fns.is_empty() {
        return None;
    }

    // `f(op(a, b), c)`: the verified fn applied to a 2-arg add over two givens and
    // a bare list given. Returns `(a, b, c)` as source names.
    let composed = |side: &crate::ast::Spanned<Expr>| -> Option<(String, String, String)> {
        let Expr::FnCall(callee, args) = &side.node else {
            return None;
        };
        if super::shared::expr_dotted_name(callee).as_deref() != Some(vb.fn_name.as_str())
            || args.len() != 2
        {
            return None;
        }
        let Expr::FnCall(op_callee, op_args) = &args[0].node else {
            return None;
        };
        let op_name = super::shared::expr_dotted_name(op_callee)?;
        if !add_fns.contains(&op_name) || op_args.len() != 2 {
            return None;
        }
        let a = given_ident_name(&op_args[0])?.to_string();
        let b = given_ident_name(&op_args[1])?.to_string();
        let c = given_ident_name(&args[1])?.to_string();
        Some((a, b, c))
    };

    // `f(a, f(b, c))`: the verified fn nested on a bare outer count, an inner
    // verified-fn call on a second bare count and the same list. Returns
    // `(a, b, c)` as source names.
    let nested = |side: &crate::ast::Spanned<Expr>| -> Option<(String, String, String)> {
        let Expr::FnCall(callee, args) = &side.node else {
            return None;
        };
        if super::shared::expr_dotted_name(callee).as_deref() != Some(vb.fn_name.as_str())
            || args.len() != 2
        {
            return None;
        }
        let a = given_ident_name(&args[0])?.to_string();
        let Expr::FnCall(inner_callee, inner_args) = &args[1].node else {
            return None;
        };
        if super::shared::expr_dotted_name(inner_callee).as_deref() != Some(vb.fn_name.as_str())
            || inner_args.len() != 2
        {
            return None;
        }
        let b = given_ident_name(&inner_args[0])?.to_string();
        let c = given_ident_name(&inner_args[1])?.to_string();
        Some((a, b, c))
    };

    // Either orientation: one side composed, the other nested, with matching
    // `(a, b, c)`. The inner count `b` (op's second arg = nested inner first arg)
    // is the induction target; `c` is generalized.
    let (composed_side, nested_side) = composed(&law.lhs)
        .map(|c| (c, nested(&law.rhs)))
        .or_else(|| composed(&law.rhs).map(|c| (c, nested(&law.lhs))))?;
    let (ca, cb, cc) = composed_side;
    let (na, nb, nc) = nested_side?;
    if ca != na || cb != nb || cc != nc {
        return None;
    }
    // `b` and `a` must be distinct Nat givens, `c` the list given.
    let given_type = |name: &str| {
        law.givens
            .iter()
            .find(|g| g.name == name)
            .map(|g| g.type_name.trim().to_string())
    };
    let is_nat = |name: &str| -> bool {
        given_type(name).is_some_and(|ty| {
            ty == "Nat" || crate::codegen::proof_recognize::peano_type_named(ctx, &ty).is_some()
        })
    };
    let is_list = |name: &str| -> bool {
        given_type(name).is_some_and(|ty| ty.starts_with("List<") || ty == "List")
    };
    if ca == cb || !is_nat(&ca) || !is_nat(&cb) || !is_list(&cc) {
        return None;
    }
    Some(CountComposition {
        inner_count: cb,
        list_given: cc,
    })
}

/// ADDITIVE rung for the count-COMPOSITION family (`f(op(a,b), c) =
/// f(a, f(b,c))`, the `drop`/`take` composition lemmas). Inducts on the INNER
/// count `b` generalizing the list `c`, with a closer that bridges `op` to
/// builtin `+`, unfolds `f` one step on both sides, peels the list, and chains
/// the IH at the tail. The synchronous-driver rung (driver = the FIRST count `a`)
/// and `fun_induction` both fall through to `sorry` on this shape — inducting on
/// `a` peels `op(a,b)` and the outer `f`'s list at different rates, never lining
/// the IH up — so this is the missing pivot. Returns
/// `(support_helper_theorems, rung_alternative_lines)`: the rung is ONE
/// parenthesized `first` alternative meant to LEAD the existing ladder. It
/// carries NO `sorry` floor and every arm ends in `simp_all`/`simp only … <;>`
/// (which THROW on an open goal), so the alternative either CLOSES the goal or
/// FAILS and `first` falls through to the existing ladder byte-for-byte. All
/// tactics (`induction`/`cases`/`simp_all`/`omega` + the kernel-proved
/// `isNatAdd` bridge and the proved `nil`-helper) are sound, so it can only ever
/// ADD closures; a non-closing law stays `universal:false` exactly as before.
fn emit_count_composition_rung(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    law_uid: &str,
) -> Option<(Vec<String>, Vec<String>)> {
    let plan = recognize_count_composition(vb, law, ctx)?;
    let driver = law
        .givens
        .iter()
        .position(|g| g.name == plan.inner_count)
        .and_then(|i| intro_names.get(i).cloned())?;
    let list_intro = law
        .givens
        .iter()
        .position(|g| g.name == plan.list_given)
        .and_then(|i| intro_names.get(i).cloned())?;

    // The kernel-proved `op a b = a + b` bridge (so `op a (k+1)` reduces — `op`
    // recurses on its FIRST arg, which here is symbolic) and the `f _ [] = []`
    // nil-helper. Both ride the same `lean_nat_lift_support` / `take_drop_nil_helper`
    // the synchronous rung already emits; we re-derive their names and texts so
    // this rung is self-contained (the caller dedups support lines).
    let (arith_support, arith_bridges, _bridged) =
        lean_nat_lift_support(law, ctx, law_uid, &BTreeSet::new());
    let add_bridge = arith_bridges
        .iter()
        .find(|n| n.ends_with("_isNatAdd"))
        .cloned()?;

    // The `f _ [] = []` helper for the verified fn (same name the synchronous rung
    // mints).
    let mut support: Vec<String> = arith_support;
    let mut nil_simp: Vec<String> = Vec::new();
    if let Some(fd) = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())
        && let Some(h) = take_drop_nil_helper(fd, law_uid, ctx)
    {
        nil_simp.push(h.name.clone());
        support.push(h.text);
    }

    // Arm simp sets. `base`: law defs (`_root_.`-prefixed) + the add bridge + the
    // nil-helper. The `zero` arm closes by `cases c <;> simp_all [base]` (the
    // bridge turns `op a 0` into `a + 0 = a`); the `succ` arm peels the list and
    // chains the IH after rewriting `op a (k+1)` to `(a + k) + 1` (so `f` peels)
    // — `simp only [bridge, Nat.add_succ, f-defs] at *` exposes the one-step
    // unfolds, then `simp_all [base, ih]` lands the IH at the tail.
    let mut base_set: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    base_set.insert(add_bridge.clone());
    base_set.extend(nil_simp.iter().cloned());
    let base = base_set.iter().cloned().collect::<Vec<_>>().join(", ");
    // The `simp only` unfold set for the succ arm: the law's defs (so `f`'s
    // equations fire) + the bridge + `Nat.add_succ` (reassociates `a + (k+1)` to
    // `(a + k) + 1` so `f` recognizes the `_ + 1` driver).
    let mut unfold_set: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    unfold_set.insert(add_bridge.clone());
    unfold_set.insert("Nat.add_succ".to_string());
    let unfold = unfold_set.iter().cloned().collect::<Vec<_>>().join(", ");
    // The cons-arm `simp_all` set carries the IH too.
    let mut cons_set = base_set.clone();
    cons_set.insert("ih".to_string());
    let cons = cons_set.iter().cloned().collect::<Vec<_>>().join(", ");

    let rung = vec![
        "  | (".to_string(),
        format!("    induction {driver} generalizing {list_intro} with"),
        format!("    | zero => cases {list_intro} <;> simp_all [{base}]"),
        format!(
            "    | succ k ih => cases {list_intro} <;> simp only [{unfold}] at * <;> simp_all [{cons}])"
        ),
    ];
    Some((support, rung))
}

/// ADDITIVE rung for the SYNCHRONOUS take/drop/zip family. A law whose subject
/// fn recurses synchronously on a `Nat` and a `List` (take/drop) — or threads two
/// lists (zip) — closes by generalizing-induction on the DRIVER `Nat` while
/// case-splitting every OTHER `Nat`/`List` given in each arm, with the law defs
/// and the cone's proved `nil`-helpers in `simp_all`. The manual single-list
/// ladder in [`emit_list_induction`] inducts on a LIST given and gets the cons IH
/// at the wrong `Nat`; this rung inducts on the `Nat` so the IH lands at the
/// predecessor, closing `drop n (take m xs) = take (m-n) (drop n xs)` (prop_57),
/// `dropPair n (zip xs ys) = zip (dropInt n xs) (dropInt n ys)` (prop_58), and
/// nested `drop (S w) (drop x (y::zs)) = drop w (drop x zs)` (prod/lemma_04).
///
/// Returns `(support_helper_theorems, rung_alternative_lines)` — the rung is ONE
/// parenthesized `first` alternative (`| ( induction … with | zero => … | succ
/// k ih => … )`) meant to LEAD the existing ladder. It carries NO `sorry` floor
/// and every arm ends in `simp_all` (which THROWS on an open goal), so the
/// alternative either CLOSES the goal or FAILS and `first` falls through to the
/// existing ladder byte-for-byte. It NEVER selects the induction variable for the
/// existing ladder (the #567 root cause) — it is purely a leading addition. All
/// tactics (`cases`/`simp_all` + `sorry`-floored proved `nil`-helpers) are sound,
/// so it can only ever ADD closures; a non-closing law stays `universal:false`
/// exactly as before.
fn emit_synchronous_multivar_induction(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    law_uid: &str,
) -> Option<(Vec<String>, Vec<String>)> {
    use crate::codegen::recursion::detect::{
        param_decremented_in_recursion, single_list_structural_param_index,
    };
    // The DRIVER is the bare `Nat` given the SUBJECT fn decrements synchronously
    // with its structural list (take/drop's `n`, the inner `drop`'s `x` in
    // lemma_04). Mapped param-name → given-name positionally, exactly as the
    // `gen_given` Peano branch in `emit_list_induction`.
    let subject = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())?;
    let list_idx = single_list_structural_param_index(subject)?;
    let nat_param = subject
        .params
        .iter()
        .enumerate()
        .find_map(|(i, (pname, ty))| {
            (i != list_idx
                && (ty.trim() == "Nat"
                    || crate::codegen::proof_recognize::peano_type_named(ctx, ty.trim()).is_some())
                && param_decremented_in_recursion(subject, i))
            .then_some(pname)
        })?;
    let driver_given_idx = law.givens.iter().position(|g| &g.name == nat_param)?;
    let driver = intro_names.get(driver_given_idx)?.clone();

    // Partition the OTHER givens by Lean-induction relevance: `Nat`/Peano givens
    // and `List` givens get `cases`-split in each arm; everything else (a bare
    // `Int` like lemma_04's `y`) is generalized but not split. ALL non-driver
    // givens are generalized so the cons IH is the fully-quantified `∀ …, P …`.
    let mut other_intros: Vec<String> = Vec::new();
    let mut cases_intros: Vec<String> = Vec::new();
    for (i, g) in law.givens.iter().enumerate() {
        if i == driver_given_idx {
            continue;
        }
        let Some(intro) = intro_names.get(i) else {
            continue;
        };
        other_intros.push(intro.clone());
        let ty = g.type_name.trim();
        let is_nat =
            ty == "Nat" || crate::codegen::proof_recognize::peano_type_named(ctx, ty).is_some();
        let is_list = ty.starts_with("List<") || ty == "List";
        if is_nat || is_list {
            cases_intros.push(intro.clone());
        }
    }

    // Proved `nil`-helpers for every list-truncating cone fn (take/drop AND zip
    // shapes). Each is kernel-proved by its own `cases <;> simp` (sorry-floored
    // for soundness), and its name joins the arms' `simp_all` set so a residual
    // like `zip (dropInt k t) [] = []` rewrites to `[]`.
    let mut support: Vec<String> = Vec::new();
    let mut helper_names: Vec<String> = Vec::new();
    for src_name in super::shared::law_simp_source_names(ctx, vb, law) {
        let Some(fd) = ctx.fn_def_by_name(&src_name, ctx.active_module_scope().as_deref()) else {
            continue;
        };
        let helper =
            take_drop_nil_helper(fd, law_uid, ctx).or_else(|| zip_nil_helper(fd, law_uid, ctx));
        if let Some(h) = helper {
            support.push(h.text);
            helper_names.push(h.name);
        }
    }

    // The arm `simp_all` set: the law defs (`_root_.`-prefixed via `law_simp_defs`)
    // + the proved nil-helpers + the append-peeling lemmas. `List.cons_append` /
    // `List.nil_append` let the appended list (`[y] ++ zs` in lemma_04) peel a
    // cons in lockstep with the recursing fn.
    let mut simp_set: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    simp_set.extend(helper_names.iter().cloned());
    simp_set.insert("List.cons_append".to_string());
    simp_set.insert("List.nil_append".to_string());
    let simp = simp_set.into_iter().collect::<Vec<_>>().join(", ");

    // Each arm: `cases <other Nats/Lists> <;> simp_all [defs, helpers, …]`. With no
    // splittable other given, the arm is a bare `simp_all`. Both arms are
    // IDENTICAL (the predecessor `k`/its IH ride in `simp_all` automatically).
    let arm = if cases_intros.is_empty() {
        format!("simp_all [{simp}]")
    } else {
        format!(
            "cases {} <;> simp_all [{simp}]",
            cases_intros.join(" <;> cases ")
        )
    };
    let generalizing = if other_intros.is_empty() {
        String::new()
    } else {
        format!(" generalizing {}", other_intros.join(" "))
    };
    let rung = vec![
        "  | (".to_string(),
        format!("    induction {driver}{generalizing} with"),
        format!("    | zero => {arm}"),
        format!("    | succ k ih => {arm})"),
    ];
    Some((support, rung))
}

/// Lean structural induction over a builtin `List<T>` given:
/// `induction xs with | nil => simp [defs] | cons head tail ih => simp_all [defs]`.
/// `List.length_cons` is a default simp lemma, so a length-relating law over a
/// cons-recursive builder (`List.len(map(xs)) == List.len(xs)`) closes once the
/// builder's def is unfolded and the cons-case induction hypothesis is in scope.
fn emit_list_induction(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    target_idx: usize,
    discovered: &[String],
) -> Option<AutoProof> {
    // Map-fold-homomorphism: a law querying `Map.get`/`Map.has` over a
    // `Map.set`-folding builder at a per-element key. Its cons arm needs the
    // general-key prelude lemmas (`get_set_ne`/`has_set`) under a `by_cases` on
    // key-equality, which the generic ladder below does not produce. Tried first
    // for this shape (only when no discovery feedback is in play, so committed
    // discovered lemmas still route through their own ladder).
    if discovered.is_empty()
        && let Some(plan) = recognize_map_fold_homomorphism(vb, law, ctx, intro_names, target_idx)
    {
        return Some(emit_map_fold_homomorphism(
            vb,
            law,
            ctx,
            intro_names,
            target_idx,
            &plan,
        ));
    }
    let mut simp_defs: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    // Rev anti-homomorphism: prepend the proved aux lemmas (support_lines) and
    // add the rev-distribution + append fn to the simp set so the cons arm
    // closes. Shared recognizer with the Dafny backend.
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    let rev_ops = crate::codegen::proof_recognize::collect_rev_ops_in_law(law, ctx);
    let (rev_support, rev_simp) = lean_rev_support(&rev_ops, &law_uid);
    simp_defs.extend(rev_simp);
    // Discovery feedback: the COMMITTED pinned lemmas (from `--discover`) join
    // the induction arms' simp sets as rewrite rules (e.g. a count/length
    // homomorphism collapsing `g (a ++ b)`). EARLIER sibling user laws
    // (część A) feed ONLY the fast path below, never the arms — so a law that
    // already closed on its ladder keeps that ladder byte-identical here.
    let discovered_simp = discovered_simp_entries(ctx, discovered);
    let siblings = earlier_law_lemmas(vb, law, ctx);
    let fast_simp = fastpath_simp_entries(ctx, discovered, &siblings);
    // The pre-discovery arm simp set (law defs + recognizer rev rules), BEFORE
    // the committed discovered lemmas join. The do-no-harm fallback ladder
    // below inducts over THIS set so a law already universal without discovery
    // keeps proving even if a committed lemma would simp-loop the augmented
    // arms into a `sorry` (the net-negative feedback bug).
    let simp_list_plain = simp_defs.iter().cloned().collect::<Vec<_>>().join(", ");
    simp_defs.extend(discovered_simp.iter().cloned());
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let target_lean = &intro_names[target_idx];

    // SECOND list argument — a list-typed `given` OTHER than the induction
    // target. A law like `last (xs ++ ys) = lastOfTwo xs ys` inducts on `xs`,
    // but the subject's body matches the OTHER list (`lastOfTwo` matches `ys`),
    // so the residual `last (h :: ys) = match ys with [] => h | _ => last ys`
    // only closes after the second list is ALSO case-split. The induction
    // ladder peels the target's `tail`; an additive `cases <second_list>` rung
    // (run only after the existing arms fail, ending in `omega`) splits the
    // second list too. First list-typed given that is not the target.
    let second_list: Option<String> = law
        .givens
        .iter()
        .enumerate()
        .find(|(i, g)| *i != target_idx && g.type_name.trim_start().starts_with("List"))
        .and_then(|(i, _)| intro_names.get(i).cloned());

    // Generalizing-induction target: a Peano `given` the verified fn decrements
    // SYNCHRONOUSLY with the list (the `n` of `take`/`drop`, which match `n`
    // then recurse on `(z, tail)`). Inducting on the list alone gives a cons IH
    // at the WRONG `n`; the proof needs `induction list generalizing n` so the
    // IH is `∀ n, P n tail`, with `cases n` in each arm exposing the predecessor
    // (closes the synchronous Nat+List family, e.g. `take n xs ++ drop n xs =
    // xs`). The `induction X generalizing Y` + `cases Y` shape already proves the
    // canonical-Peano `Sub`/`Le`/`Lt` bridges, so it is well-trodden.
    // `(intro_name, needs_cases)`: the given to generalize over, and whether to
    // `cases` it in each arm. A Peano param the fn decrements synchronously
    // (`take`/`drop`'s `n`) is generalized AND case-split (the IH lands at the
    // predecessor); a THREADED accumulator (`qrev`'s `acc`, fed
    // `List.concat([h], acc)`) is generalized only (no scrutinee to split, the
    // IH `∀ acc, P xs acc` applies at the threaded value).
    use crate::codegen::recursion::detect::{
        param_decremented_in_recursion, param_threaded_in_recursion,
        single_list_structural_param_index,
    };
    let gen_given: Option<(String, bool)> = ctx
        .fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())
        .and_then(|fd| {
            let lidx = single_list_structural_param_index(fd)?;
            let given_intro = |fn_param: &str| -> Option<String> {
                law.givens
                    .iter()
                    .position(|g| g.name == fn_param)
                    .map(|gi| intro_names[gi].clone())
            };
            // Peano sync-decremented param → generalize + cases.
            if let Some((_, (pname, _))) = fd.params.iter().enumerate().find(|(i, (_, ty))| {
                *i != lidx
                    && (ty.trim() == "Nat"
                        || crate::codegen::proof_recognize::peano_type_named(ctx, ty.trim())
                            .is_some())
                    && param_decremented_in_recursion(fd, *i)
            }) {
                return given_intro(pname).map(|n| (n, true));
            }
            // Threaded accumulator param → generalize only.
            if let Some((_, (pname, _))) = fd
                .params
                .iter()
                .enumerate()
                .find(|(i, _)| *i != lidx && param_threaded_in_recursion(fd, *i))
            {
                return given_intro(pname).map(|n| (n, false));
            }
            None
        });

    // `simp only` set for the split fallback below. `List.cons_append`
    // ((a::l) ++ l' = a :: (l ++ l')) lets the appended list peel a cons in
    // lockstep with the recursing fn; guard against an empty `simp_list` so we
    // never emit a leading-comma `simp only [, …]` (a parse error `first`
    // could not recover from).
    let split_set = if simp_list.is_empty() {
        "List.cons_append, List.singleton_append, List.nil_append".to_string()
    } else {
        format!("{simp_list}, List.cons_append, List.singleton_append, List.nil_append")
    };
    let split_set_plain = if simp_list_plain.is_empty() {
        "List.cons_append, List.singleton_append, List.nil_append".to_string()
    } else {
        format!("{simp_list_plain}, List.cons_append, List.singleton_append, List.nil_append")
    };

    // Each arm closes fully or admits `sorry` — and crucially BUILDS either
    // way. `induction .. with | arm => tac` requires each arm's `tac` to close
    // its goal; a leftover goal is an `unsolved goals` ERROR at the arm (a hard
    // lake-build failure), NOT something a trailing `all_goals sorry` can mop
    // up (that tactic is unreachable past a failing arm). So gate each arm on
    // `first | (simp[_all] [defs]; done) | (simp[_all] [defs]; omega) | sorry`:
    // the `; done` turns a didn't-close (or no-progress) `simp` into a throw
    // that `first` catches. The second arm retries with `omega` to discharge a
    // linear-arithmetic residual the inductive hypothesis leaves behind (e.g.
    // `count(append a b) = count a + count b` needs `1 + (m + n) = (1 + m) +
    // n`) — `omega` is a sound decision procedure, so it only ever closes true
    // goals; anything it can't (rle/json roundtrips, the fuel-wrapped quicksort
    // SCC) still degrades to an honest `sorry` that lake builds — never a
    // silent unsolved-goals error.
    // The trailing `split` branch (before `sorry`) handles a recursive fn
    // whose body matches on an inner Bool/enum — e.g. `count`'s `match
    // eqNat(n, head)` — which leaves a STUCK `match` after `simp_all` because
    // the scrutinee is symbolic (`n`, `head` are universally bound). `simp
    // only [defs, List.cons_append]` unfolds the fns and peels the appended
    // cons so both sides expose the SAME scrutinee, then `split` case-splits
    // it (one goal per arm) and `simp_all <;> omega` discharges each with the
    // induction hypothesis plus the linear-arith residual. This converts the
    // count/length-homomorphism family from `sorry` to a genuine universal.
    // Purely additive: it runs only after the two `simp_all` branches fail, so
    // cases that already close are untouched, and `split`/`simp_all`/`omega`
    // are all sound — an unprovable goal still degrades to the honest `sorry`.
    // Feedback mode adds two BRIDGE branches per arm (`bridges` = the
    // canonical-Peano op bridges, e.g. `plus a b = a + b`): after the def
    // unfolds + induction hypothesis, a goal like `S (plus a b) = plus a
    // (S b)` is stuck (the op recurses on a symbolic arg) but is pure linear
    // arithmetic once bridged — `simp only [bridges] <;> omega` decides it.
    // The split variant covers the same residual under an inner Bool/enum
    // match (`try` so a goal with nothing to bridge still reaches `omega`).
    // All branches are sound, so each can only ADD closures.
    // Build the nil/cons arms over an explicit `arm_simp`/`arm_split` set, with
    // the trailing `| sorry` only when `with_sorry`. część C uses this to emit
    // TWO ladders: ladderA over the committed-only set WITHOUT sorry (so it
    // THROWS on an open arm and `first` falls through) and ladderB over the
    // committed + Forward-sibling set WITH sorry (the honest building floor).
    let mk_arms = |arm_simp: &str,
                   arm_split: &str,
                   bridges: Option<&str>,
                   cases_extra: Option<&str>,
                   with_sorry: bool|
     -> (String, String) {
        let nil_bridge = bridges
            .map(|b| format!(" | (simp [{arm_simp}]; simp only [{b}] <;> omega)"))
            .unwrap_or_default();
        let cons_bridge = bridges
            .map(|b| format!(" | (simp_all [{arm_simp}]; simp only [{b}] <;> omega)"))
            .unwrap_or_default();
        let split_bridge = bridges
            .map(|b| format!(" <;> (try simp only [{b}])"))
            .unwrap_or_default();
        // A second `cases tail` branch over a BRIDGED simp set (the law's defs
        // with each canonical-Peano op's def swapped for its proven `f a b =
        // a OP b` bridge). The bare `cases tail` branch above exposes the inner
        // constructor of a two-deep match (`last`/`butlast`), but a residual
        // like `minus (len t) 0 = len t` still defeats `omega` because `minus`
        // is an opaque user fn; rewriting it to builtin `-` via the bridge makes
        // the leftover pure linear arithmetic that `omega` decides. Additive and
        // sound (runs after the bare branch fails, `<;> omega` throws on any
        // leftover), so it can only ADD closures.
        let cases_extra_branch = cases_extra
            .map(|s| format!(" | (cases tail <;> simp_all [{s}] <;> omega)"))
            .unwrap_or_default();
        // A `split` twin of the bridged `cases tail` branch, over the SAME
        // bridged simp set (the comparison fn's def swapped for its proven
        // `f a b = true ↔ a ≤ b` bridge, so the inductive hypothesis lands in
        // arithmetic form `omega` can use). A guarded comparison law — e.g.
        // `le (len (filterZ xs)) (len xs)` where `filterZ` branches on an inner
        // `isZ head` — needs BOTH the guard split AND the bridge in ONE rung:
        // the bare split rung above unfolds the comparison fn (destroying the
        // `= true` the bridge keys on), and the `cases tail` branch never splits
        // the guard. `simp only [s]` exposes the inner `if` (the comparison fn's
        // def is NOT in `s`, so it survives for the bridge), `split` case-splits
        // it, then `simp_all [s] <;> omega` bridges the IH and closes each branch.
        // Additive and sound (runs only after the prior arms fail, ends in
        // `omega`), so it can only ADD closures.
        let split_extra_branch = cases_extra
            .map(|s| format!(" | (simp only [{s}]; split <;> simp_all [{s}] <;> omega)"))
            .unwrap_or_default();
        // GAP #3 — case-split the SECOND list argument. The subject fn may match
        // a DIFFERENT list than the induction target (`lastOfTwo` matches `ys`,
        // not the recursed `xs`), leaving a residual `… = match ys with …` that
        // no amount of `cases tail` peels. An additive `cases <second_list>`
        // rung (and a `cases tail <;> cases <second_list>` twin for the cons arm,
        // where BOTH lists drive the two-deep `last`/`butlast` shape) splits it.
        // Runs only after the prior arms fail and ends in `<;> omega`, so a
        // non-closing arm still degrades to the honest `sorry` — purely additive,
        // sound (`cases`/`simp_all`/`omega` mint no axioms), can only ADD closures.
        let second_cases_nil = second_list
            .as_deref()
            .map(|sl| format!(" | (cases {sl} <;> simp_all [{arm_simp}] <;> omega)"))
            .unwrap_or_default();
        let second_cases_cons = second_list
            .as_deref()
            .map(|sl| {
                format!(
                    " | (cases {sl} <;> simp_all [{arm_simp}] <;> omega) | (cases tail <;> cases {sl} <;> simp_all [{arm_simp}] <;> omega)"
                )
            })
            .unwrap_or_default();
        // GAP #1 — push a proved equality through a user-fn wrapper with `congr`.
        // simp+omega cannot conclude `f a = f b` from a provable `a = b` when `f`
        // is an opaque user fn (`half (length …) = half (length …)`): omega never
        // sees inside `half`. `congr 1` reduces the goal to its argument equality
        // `a = b`, which the SAME arm simp set + `omega` then discharges (the
        // homomorphism/bridge rewrites having normalized `a` and `b` to the same
        // linear-arith form). Runs only after the prior arms fail and every
        // sub-goal ends in `omega`, so it throws on a leftover and degrades to
        // `sorry` — additive and sound, can only ADD closures.
        let congr_nil =
            format!(" | (simp [{arm_simp}]; congr 1 <;> simp_all [{arm_simp}] <;> omega)");
        let congr_cons =
            format!(" | (simp_all [{arm_simp}]; congr 1 <;> simp_all [{arm_simp}] <;> omega)");
        let tail = if with_sorry { " | sorry" } else { "" };
        (
            format!(
                "| nil => first | (simp [{arm_simp}]; done) | (simp [{arm_simp}]; omega){nil_bridge} | (simp only [{arm_split}]; split <;> simp_all [{arm_simp}]{split_bridge} <;> omega){second_cases_nil}{congr_nil}{tail}"
            ),
            // Trailing `cases tail` branch: a fn whose body matches TWO levels
            // deep on the list (`last`/`butlast`: `match x | [] | y::z => match z
            // | [] | …`) generates equational lemmas keyed on the two-deep
            // pattern, so `simp [fn]` can't unfold `fn (head :: tail)` while
            // `tail` is a variable — and the earlier `split` branch then finds no
            // `match` to peel. Decomposing `tail` one level exposes the inner
            // constructor (`[head]` vs `head :: h2 :: …`), the equation fires, and
            // the cons IH lands on the recursive call. Runs only after the three
            // simp/split branches fail; `<;> omega` throws on any leftover so a
            // non-closing arm still degrades to the honest `sorry`. Sound, so it
            // can only ADD closures.
            format!(
                "| cons head tail ih => first | (simp_all [{arm_simp}]; done) | (simp_all [{arm_simp}]; omega){cons_bridge} | (simp only [{arm_split}]; split <;> simp_all [{arm_simp}]{split_bridge} <;> omega) | (cases tail <;> simp_all [{arm_simp}] <;> omega){cases_extra_branch}{split_extra_branch}{second_cases_cons}{congr_cons}{tail}"
            ),
        )
    };

    let mut proof_lines = vec![format!("  intro {}", intro_names.join(" "))];
    let mut support_lines = Vec::new();
    // Feedback mode fires when any usable rewrite rule is in scope — a
    // committed pin OR an eligible earlier sibling law (`fast_simp` carries
    // both). With neither, the emit is byte-identical to the pre-feedback
    // ladder.
    if let Some((gv, needs_cases)) = gen_given.as_ref().filter(|_| fast_simp.is_empty()) {
        // Generalizing induction. `induction list generalizing <gv>` makes the
        // cons IH `∀ <gv>, P <gv> tail`, so it applies at the recursion's
        // threaded/decremented value. For a Peano `<gv>` (`take`/`drop`'s `n`)
        // each arm `cases <gv> <;> (ladder)` splits zero/succ so the IH lands
        // at the predecessor; for a threaded accumulator (`qrev`'s `acc`) no
        // split is needed (the IH applies at `h::acc` directly). The ladder is
        // the same sound first|simp|omega|split|sorry chain.
        let ladder = |s: &str| -> String {
            format!(
                "first | ({s} [{d}]; done) | ({s} [{d}]; omega) | (simp only [{sp}]; split <;> simp_all [{d}] <;> omega) | sorry",
                d = simp_list,
                sp = split_set
            )
        };
        let wrap = |arm: &str| -> String {
            if *needs_cases {
                format!("cases {gv} <;> ({arm})")
            } else {
                arm.to_string()
            }
        };
        proof_lines.push(format!(
            "  induction {} generalizing {} with",
            target_lean, gv
        ));
        proof_lines.push(format!("  | nil => {}", wrap(&ladder("simp"))));
        proof_lines.push(format!(
            "  | cons head tail ih => {}",
            wrap(&ladder("simp_all"))
        ));
    } else if fast_simp.is_empty() && gen_given.is_none() {
        // Canonical-Peano bridges on the plain (no-discovery) path. A two-deep-
        // match law such as `butlast xs = take (minus (len xs) (S Z)) xs`
        // reduces, after `cases tail`, to a residual like `minus (len t) 0 =
        // len t` that `omega` cannot close while `minus` is an opaque user fn.
        // Emit the proven `minus a b = a - b` bridge (the SAME recognizer the
        // discovery-feedback path uses) and a bridged `cases tail` branch that
        // rewrites the residual to `len t - 0 = len t`, which `omega` decides.
        // The bridge is PROVED (a misrecognized op fails its own proof and
        // degrades to an honest `sorry`, never a false theorem); the extra
        // branch is additive (runs only after the bare arms fail, ends in
        // `omega`), so this can only ADD closures. With no Peano op in the law
        // the bridge set is empty and the emit is byte-identical to before.
        let (arith_support, arith_bridges, bridged_fns) =
            lean_nat_lift_support(law, ctx, &law_uid, &BTreeSet::new());
        let cases_extra = if arith_bridges.is_empty() {
            None
        } else {
            support_lines.extend(arith_support);
            let set: BTreeSet<String> = simp_list
                .split(", ")
                .map(String::from)
                .chain(arith_bridges.iter().cloned())
                .filter(|n| !n.is_empty() && !bridged_fns.contains(n))
                .collect();
            Some(set.into_iter().collect::<Vec<_>>().join(", "))
        };
        let (nil_arm, cons_arm) =
            mk_arms(&simp_list, &split_set, None, cases_extra.as_deref(), true);
        proof_lines.push(format!("  induction {} with", target_lean));
        proof_lines.push(format!("  {nil_arm}"));
        proof_lines.push(format!("  {cons_arm}"));
    } else {
        // Discovery feedback: before inducting, try closing the goal OUTRIGHT
        // with the available lemmas — many laws that NEED an auxiliary
        // homomorphism are a pure rewrite once it exists (e.g. `length (x ++
        // y) = plus (length y) (length x)` under the length homomorphism + the
        // `plus = +` bridge + `omega`). Two `simp only` shapes: lemmas+bridges
        // alone (the goal already matches a lemma), then with the law's def
        // unfolds added (a wrapper like `appendNat` must unfold to `++` before
        // the lemma can fire) — minus the bridged fns' own defs (def + bridge
        // in one simp call sticks). Both sound, so a miss falls through to the
        // induction ladder.
        let lemma_fns = feedback_source_fns(ctx, discovered, &siblings);
        let (arith_support, arith_bridges, bridged_fns) =
            lean_nat_lift_support(law, ctx, &law_uid, &lemma_fns);
        // Commutativity normalizers for the fast path ONLY. After a homomorphism
        // + `= a + b` bridge rewrite, a COMMUTE goal lands as `f (a + b) = f (b +
        // a)` where `f` is an opaque user fn (`even`, `half`) — `omega` never
        // sees inside `f`, so it cannot close it, but `simp`'s permutative
        // handling of `Nat.add_comm`/`Nat.mul_comm` orders both sides to the same
        // canonical form and closes by `rfl`. These are permutative rules: `simp`
        // applies them only toward its term order, so they NEVER loop. Confined to
        // the fast path's `simp only` rungs (gated on non-empty siblings /
        // committed lemmas) — the induction arms stay byte-identical, so a law
        // that already closed on its ladder is untouched. Strictly additive: a
        // `simp only` that already closed runs `<;> omega` on zero goals.
        const COMMUTE_NORMALIZERS: &[&str] = &[
            "Nat.add_comm",
            "Nat.mul_comm",
            "Int.add_comm",
            "Int.mul_comm",
        ];
        let mut fast_lemmas: Vec<String> = fast_simp.clone();
        fast_lemmas.extend(arith_bridges.iter().cloned());
        if !fast_simp.is_empty() {
            fast_lemmas.extend(COMMUTE_NORMALIZERS.iter().map(|s| s.to_string()));
        }
        let fast_unfolds: BTreeSet<String> = law_simp_defs(ctx, vb, law)
            .into_iter()
            .chain(fast_simp.iter().cloned())
            .chain(arith_bridges.iter().cloned())
            .chain(
                COMMUTE_NORMALIZERS
                    .iter()
                    .filter(|_| !fast_simp.is_empty())
                    .map(|s| s.to_string()),
            )
            .filter(|n| !bridged_fns.contains(n))
            .collect();
        let bridge_set = if arith_bridges.is_empty() {
            None
        } else {
            Some(arith_bridges.join(", "))
        };

        // część C — ARM injection of Forward siblings. Some laws need the
        // helper applied INSIDE the cons arm, not just at the top level (e.g.
        // `count n xs = count n (rev xs)`: the cons goal `count n (rev t ++
        // [h])` only collapses if the count-homomorphism rewrites in-arm).
        // The fast-path (committed + ALL siblings, Reversed included) is tried
        // first; then ladderA over the COMMITTED-only arm set WITHOUT a sorry
        // — so a previously-closing ladder closes here IDENTICALLY, but an open
        // arm THROWS and `first` falls to ladderB. ladderB injects the Forward
        // siblings into the arms (Reversed stay fast-path-only — an unfold rule
        // mixed with the fn's own def in an arm can loop, and a simp loop is an
        // uncatchable maxHeartbeats build error) and carries the sorry floor.
        // Forward homomorphisms CONSUME appends as they rewrite, so they
        // terminate in `simp_all`; the loop-exclusion in `simp_entries` already
        // dropped any cyclic forward/reversed pair. When no Forward sibling
        // adds anything beyond the committed arm set, stay single-ladder
        // (byte-identical to the committed-only feedback emit).
        let arm_forward_siblings: Vec<String> = fast_simp
            .iter()
            .filter(|e| !e.starts_with("← ") && !discovered_simp.contains(*e))
            .cloned()
            .collect();

        proof_lines.push("  first".to_string());
        // DO-NO-HARM (Houdini accept). Try the EXACT pre-discovery proof FIRST:
        // induct over `simp_list_plain` (NO committed discovered lemmas) with NO
        // `sorry`, so an open arm THROWS and `first` moves on. A law already
        // universal before discovery closes here and `first` stops — the
        // discovered-lemma tactics below are reached ONLY when the pre-discovery
        // proof cannot close. So a committed lemma can only ever ADD a proof,
        // never demote a working one into a `sorry` (the net-negative feedback
        // bug: e.g. the reversed `← (++)=append` rule simp-looping rev's
        // `revRev` ladder). Gated on a non-empty `discovered_simp` so a
        // sibling-only feedback emit (część A) stays byte-identical to before.
        if !discovered_simp.is_empty() {
            let (nil_plain, cons_plain) = mk_arms(
                &simp_list_plain,
                &split_set_plain,
                bridge_set.as_deref(),
                None,
                false,
            );
            proof_lines.push(format!("  | (induction {} with", target_lean));
            proof_lines.push(format!("     {nil_plain}"));
            proof_lines.push(format!("     {cons_plain})"));
        }
        // Structural-only rung (bridge-free), tried BEFORE the bridged rungs
        // below: the sibling laws ALONE, closed by `; done`. The bridge +
        // commute-normalizer rungs rewrite a user fn to its builtin (`plus →
        // +`) and a `S (S y)` literal to a numeral (`+ 2`) / reorder the `+`s —
        // which strands an `S (S)`-shaped sibling law (`even ((n+1)+1) = even
        // n`, `length (w ++ (x::y::z)) = ((length (w ++ z) + 1) + 1)`) so it no
        // longer matches, and `omega` — blind to the opaque user fn (`even`) —
        // then cannot finish. Trying the siblings WITHOUT the arithmetic
        // canonicalization lets the pure structural rewrite close by `rfl`
        // (prod lemma_14/lemma_16's `even`-shift targets). Gated on non-empty
        // siblings; `; done` throws on any leftover, so this is strictly
        // additive — a non-closing goal falls straight through to the rungs
        // below, and the bare-sibling set is a subset of `fast_lemmas` already
        // run there, so it adds no new simp-loop surface.
        if !fast_simp.is_empty() {
            proof_lines.push(format!("  | (simp only [{}]; done)", fast_simp.join(", ")));
        }
        proof_lines.push(format!(
            "  | (simp only [{}] <;> omega)",
            fast_lemmas.join(", ")
        ));
        // The def-unfolding variant is a distinct alternative only when it adds
        // a def beyond the lemma+bridge set; when every law def is bridged out
        // it collapses to the line above, so emit it once — not as a duplicate.
        let fast_lemmas_set: BTreeSet<String> = fast_lemmas.iter().cloned().collect();
        if fast_unfolds != fast_lemmas_set {
            proof_lines.push(format!(
                "  | (simp only [{}] <;> omega)",
                fast_unfolds.into_iter().collect::<Vec<_>>().join(", ")
            ));
        }
        if let Some((gv, needs_cases)) = gen_given.as_ref() {
            // LUKA 2 — GENERALIZING induction WITH in-arm Forward siblings.
            // This is the missing combination: the subject fn threads/decrements
            // a Peano `<gv>` synchronously with the recursing list (`take`/`drop`'s
            // `n`, or `qrev`'s threaded `acc`) AND earlier-sibling helper laws are
            // in scope. The plain feedback ladder below would induct on the list
            // WITHOUT `generalizing <gv>`, fixing the cons IH at the original
            // `<gv>` — after `cases <gv>` the residual needs the IH at the
            // predecessor, which doesn't exist, so it `sorry`s. Emitting the
            // generalizing form makes the cons IH `∀ <gv>, P <gv> tail`, so it
            // applies at the recursion's threaded/decremented value, while the
            // Forward siblings rewrite the helper-shaped residual in-arm (e.g.
            // `rev (drop n xs) = take (len xs - n) (rev xs)` needs `len (rev _)`,
            // take-all, and a take-over-append split inside the succ arm).
            //
            // SOUNDNESS / no-loop: the arm set is `simp_list ∪ arm_forward_siblings`
            // — Forward-only and loop-excluded, EXACTLY as część C's ladderB. We
            // do NOT inject Reversed (`← `) rules into the arms: an unfold rule
            // mixed with the fn's own def in an arm can simp-loop, and a simp loop
            // is an uncatchable `maxHeartbeats` build hang. Reversed rules stay on
            // the fast path above. Each arm keeps the sound
            // `first | (simp…;done) | (…;omega) | (split…) | sorry` shape, so a
            // non-closing arm degrades to an honest `sorry` (caught by the
            // universal metric), never an unsolved-goals build error.
            let gen_simp = {
                let mut v: Vec<String> = simp_list.split(", ").map(String::from).collect();
                v.extend(arm_forward_siblings.iter().cloned());
                v.retain(|s| !s.is_empty());
                v.join(", ")
            };
            let gen_split = if gen_simp.is_empty() {
                "List.cons_append".to_string()
            } else {
                format!("{gen_simp}, List.cons_append")
            };
            // Same sound chain as the gen-only branch, with the Peano-op bridge
            // (`plus a b = a + b`, `minus`'s `= a - b`) appended after the def
            // unfolds + IH: a goal like `take (minus (len t) z) (rev t ++ [h]) =
            // …` is pure once `minus` is bridged and the take-over-append sibling
            // has fired. The bridge variant only runs after the plain `simp`/`omega`
            // arms fail, so closing arms are untouched; `simp only [bridge] <;>
            // omega` is a sound decision step.
            let ladder = |s: &str| -> String {
                let bridge_arm = bridge_set
                    .as_deref()
                    .map(|b| format!(" | ({s} [{gen_simp}]; simp only [{b}] <;> omega)"))
                    .unwrap_or_default();
                let split_bridge = bridge_set
                    .as_deref()
                    .map(|b| format!(" <;> (try simp only [{b}])"))
                    .unwrap_or_default();
                format!(
                    "first | ({s} [{gen_simp}]; done) | ({s} [{gen_simp}]; omega){bridge_arm} | (simp only [{gen_split}]; split <;> simp_all [{gen_simp}]{split_bridge} <;> omega) | sorry"
                )
            };
            let wrap = |arm: &str| -> String {
                if *needs_cases {
                    format!("cases {gv} <;> ({arm})")
                } else {
                    arm.to_string()
                }
            };
            proof_lines.push(format!(
                "  | (induction {} generalizing {} with",
                target_lean, gv
            ));
            proof_lines.push(format!("     | nil => {}", wrap(&ladder("simp"))));
            proof_lines.push(format!(
                "     | cons head tail ih => {})",
                wrap(&ladder("simp_all"))
            ));
        } else if arm_forward_siblings.is_empty() {
            // No in-arm sibling to add: one committed-only ladder, with sorry.
            let (nil_arm, cons_arm) =
                mk_arms(&simp_list, &split_set, bridge_set.as_deref(), None, true);
            proof_lines.push(format!("  | (induction {} with", target_lean));
            proof_lines.push(format!("     {nil_arm}"));
            proof_lines.push(format!("     {cons_arm})"));
        } else {
            // ladderA: committed-only arms, NO sorry (throws → ladderB).
            let (nil_a, cons_a) =
                mk_arms(&simp_list, &split_set, bridge_set.as_deref(), None, false);
            proof_lines.push(format!("  | (induction {} with", target_lean));
            proof_lines.push(format!("     {nil_a}"));
            proof_lines.push(format!("     {cons_a})"));
            // ladderB: committed + Forward siblings in the arms, WITH sorry.
            let simp_b = {
                let mut v: Vec<String> = simp_list.split(", ").map(String::from).collect();
                v.extend(arm_forward_siblings.iter().cloned());
                v.retain(|s| !s.is_empty());
                v.join(", ")
            };
            let split_b = if simp_b.is_empty() {
                "List.cons_append".to_string()
            } else {
                format!("{simp_b}, List.cons_append")
            };
            let (nil_b, cons_b) = mk_arms(&simp_b, &split_b, bridge_set.as_deref(), None, true);
            proof_lines.push(format!("  | (induction {} with", target_lean));
            proof_lines.push(format!("     {nil_b}"));
            proof_lines.push(format!("     {cons_b})"));
        }
        support_lines.extend(discovered_support_lines(ctx, vb, law, discovered));
        support_lines.extend(arith_support);
    }
    support_lines.extend(rev_support);

    // ADDITIVE `fun_induction` first-rung. When the law's goal calls a user-
    // recursive fn on FREE-VARIABLE args, prepend `fun_induction <fn> <args>`
    // before the ladder above as `first | (fun_induction …) | (<ladder>)`. The
    // auto-derived `<fn>.induct` splits exactly that fn's own (possibly two-deep)
    // case tree with the precise IH — closing laws whose driving fn is an INNER
    // call the manual nil/cons-on-the-list-given ladder can't pivot on, e.g. the
    // `elem (x, ins/insert(x, xs)) = true` family (prop_29/30, prod/prop_45),
    // where inducting the INSERTING fn's case tree is what lines the goal up, and
    // the `count`/`eqNat` cons/append laws (prop_04/38). The rung's closer throws
    // on any leftover goal, so `fun_induction` either CLOSES the goal or FAILS
    // and `first` falls to the ladder above byte-for-byte. PURELY ADDITIVE: it
    // can only add closures, never remove one, and uses only sound tactics
    // (`simp_all`/`omega` + kernel-proven `_refl` lemmas), so it adds no axioms.
    // Skipped under the discovery-feedback path (the ladder there is already a
    // `first | … |` chain the rung would needlessly nest) — that path keeps its
    // own emit.
    if discovered.is_empty() && fast_simp.is_empty() {
        let targets = find_fun_induction_targets(vb, law, ctx, intro_names);
        if !targets.is_empty() {
            // Proven structural-equality reflexivity lemmas (`eqNat a a = true`)
            // for any such fn in the law's cone — the residual a `fun_induction`
            // split leaves on a matched-key arm (prop_29's `elem x (ins1 x xs)`
            // reduces to `eqNat x x = true`). The lemma is kernel-PROVEN (a
            // misrecognized fn never reaches here — see `recognize_refl_eq_fn`),
            // so adding its name to the closer's simp set is sound and cheap; it
            // replaces the unsound in-closer `induction`. The defs join
            // `simp_list` only for the rung's closer, never the existing ladder.
            let (refl_support, refl_names) = lean_refl_support(vb, law, ctx, &law_uid);
            let refl_simp_list = if refl_names.is_empty() {
                simp_list.clone()
            } else if simp_list.is_empty() {
                refl_names.join(", ")
            } else {
                format!("{simp_list}, {}", refl_names.join(", "))
            };
            support_lines.extend(refl_support);
            let intro_line = proof_lines.remove(0);
            proof_lines =
                wrap_with_fun_induction_rung(intro_line, proof_lines, &targets, &refl_simp_list);
        }

        // ADDITIVE synchronous take/drop/zip rung — prepended as the LEADING
        // `first` alternative. Built independently of the `fun_induction` rung
        // above and threaded in front of whatever ladder is now in `proof_lines`
        // (the `fun_induction`-wrapped `first` chain, or the bare ladder). The
        // rung carries no `sorry` floor and throws on any open goal, so `first`
        // either takes its closure or falls through to the rest byte-for-byte.
        // Splice a leading `first` alternative (a parenthesized rung block) in
        // front of whatever ladder is in `proof_lines`. If the ladder is already
        // a `first` chain the rung becomes its new first alternative; otherwise a
        // fresh `first | (rung) | (existing body)` wrapper is built. The rung
        // either CLOSES the goal or THROWS (no `sorry` floor), so `first` falls
        // through byte-for-byte — purely additive.
        let splice_leading_rung = |proof_lines: &mut Vec<String>, rung: Vec<String>| {
            let intro_line = proof_lines.remove(0);
            if proof_lines.first().map(String::as_str) == Some("  first") {
                let mut wrapped = vec![intro_line, "  first".to_string()];
                wrapped.extend(rung);
                wrapped.extend(proof_lines.drain(1..));
                *proof_lines = wrapped;
            } else {
                let mut wrapped = vec![intro_line, "  first".to_string()];
                wrapped.extend(rung);
                wrapped.push("  | (".to_string());
                for line in proof_lines.iter() {
                    wrapped.push(format!("  {line}"));
                }
                if let Some(last) = wrapped.last_mut() {
                    last.push(')');
                }
                *proof_lines = wrapped;
            }
        };

        if let Some((multivar_support, rung)) =
            emit_synchronous_multivar_induction(vb, law, ctx, intro_names, &law_uid)
        {
            support_lines.extend(multivar_support);
            splice_leading_rung(&mut proof_lines, rung);
        }

        // ADDITIVE count-COMPOSITION rung (`f(op(a,b), c) = f(a, f(b,c))`, the
        // drop/take composition lemmas). Inducts on the INNER count generalizing
        // the list — the pivot the synchronous-driver rung (driver = the FIRST
        // count) and `fun_induction` both miss. Spliced LAST so it LEADS the
        // `first` chain. Same additive contract: it closes or throws and falls
        // through. The bridge/nil-helper support lines may duplicate the
        // synchronous rung's; the renderer dedups support theorems by name.
        if let Some((compose_support, rung)) =
            emit_count_composition_rung(vb, law, ctx, intro_names, &law_uid)
        {
            support_lines.extend(compose_support);
            splice_leading_rung(&mut proof_lines, rung);
        }
    }

    // The leading rungs (synchronous-driver and count-composition) may each emit
    // the SAME proved support theorem (the `isNatAdd` bridge, the `f _ [] = []`
    // nil-helper). A verbatim-duplicate `theorem … := by …` block is a Lean
    // re-declaration error in the single-theorem path (which extends support
    // lines without dedup), so drop exact-duplicate support theorems here,
    // preserving first-occurrence order. Identical text = identical declaration,
    // so this never changes a proof.
    {
        let mut seen: BTreeSet<String> = BTreeSet::new();
        support_lines.retain(|line| seen.insert(line.clone()));
    }

    Some(AutoProof {
        support_lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

/// Kernel-proved commutativity (`f a b = f b a`) theorem for a both-args-peeling
/// commutative Peano fn (`max`/`min`), using the same generalizing-induction
/// ladder as [`emit_both_args_peeling_law`]. Returns `(theorem_text, name)`.
/// Proved, not trusted: a non-commutative fn never reaches here (the caller
/// gates on `both_args_peeling_is_commutative`), and even if it did the ladder
/// would degrade to `sorry` (caught by the universal metric), never minting a
/// false theorem.
fn both_args_peeling_comm_theorem(fn_lean: &str, law_uid: &str) -> (String, String) {
    let name = format!("{law_uid}_{fn_lean}_comm");
    let ladder = format!(
        "first | (cases b <;> simp_all [{fn_lean}]; done) | (cases b <;> simp_all [{fn_lean}]; omega) | sorry"
    );
    let text = format!(
        "theorem {name} : ∀ (a b : Nat), {fn_lean} a b = {fn_lean} b a := by\n  intro a b\n  induction a generalizing b with\n  | zero => {ladder}\n  | succ k ih => {ladder}"
    );
    (text, name)
}

/// Commutative both-args-peeling Peano fns in THIS law's proof cone — each gets
/// a kernel-proved `comm` support lemma whose name is injected into the
/// induction arms. `height (mirror t) = height t` reduces (after the IH) to
/// `max (height r) (height l) = max (height l) (height r)`, which only closes if
/// `max`'s commutativity is available as a rewrite. The verified fn itself is
/// excluded (a `max`-commutativity law is proven directly by
/// [`emit_both_args_peeling_law`], not via a self-referential support lemma).
/// Returns `(support_theorem_texts, lemma_names)`.
fn consumed_comm_lemmas(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    law: &VerifyLaw,
    law_uid: &str,
) -> (Vec<String>, Vec<String>) {
    let mut support = Vec::new();
    let mut names = Vec::new();
    for src_name in super::shared::law_simp_source_names(ctx, vb, law) {
        if src_name == vb.fn_name {
            continue;
        }
        let Some(fd) = ctx.fn_def_by_name(&src_name, ctx.active_module_scope().as_deref()) else {
            continue;
        };
        if crate::codegen::proof_recognize::both_args_peeling_is_commutative(fd, ctx).is_none() {
            continue;
        }
        let fn_lean = aver_name_to_lean(&src_name);
        let (text, name) = both_args_peeling_comm_theorem(&fn_lean, law_uid);
        support.push(text);
        names.push(name);
    }
    (support, names)
}

/// Generalizing induction for a commutativity / associativity law of a
/// both-args-peeling Nat fn (`max`/`min`). Fires only when:
/// - the verified fn has the synchronous two-arg-peeling recursion shape
///   (`detect::recurses_decrementing_both_args`), and
/// - every law `given` is that fn's (canonical-Peano) parameter type, and
/// - the law has 2 givens (commutativity) or 3 (associativity) — the families
///   whose proof needs the other arg(s) case-split inside each induction arm.
///
/// Emits `induction g1 generalizing g2 [g3] with | zero => … | succ k ih => …`,
/// where each arm `cases`-splits the remaining givens then runs the same sound
/// `first | (cases…<;> simp_all [defs]; done) | (… ; omega) | sorry` ladder used
/// elsewhere. `simp_all` carries the `succ`-arm IH (`∀ g2 [g3], P k g2 [g3]`)
/// automatically, so the recursion's both-args peel rewrites in lockstep. Every
/// branch is sound (`simp_all`/`omega` close only true goals; a non-closing arm
/// degrades to the honest `sorry` the universal metric catches), so this can
/// only ever ADD closures — a false law still reports `universal:false`.
fn emit_both_args_peeling_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    // 2 (comm) or 3 (assoc) givens, all the verified fn's both-args-peeling
    // Peano param type.
    if law.givens.len() != 2 && law.givens.len() != 3 {
        return None;
    }
    let fd = ctx.fn_def_by_name(&vb.fn_name, ctx.active_module_scope().as_deref())?;
    if !crate::codegen::recursion::detect::recurses_decrementing_both_args(fd) {
        return None;
    }
    let param_type = fd.params.first()?.1.trim().to_string();
    crate::codegen::proof_recognize::peano_type_named(ctx, &param_type)?;
    if law.givens.iter().any(|g| g.type_name.trim() != param_type) {
        return None;
    }
    if intro_names.len() != law.givens.len() {
        return None;
    }
    // Two shapes fire this generalizing template. (1) A genuine commutativity /
    // associativity of THIS fn over its givens (`f a b = f b a` / `f (f a b) c =
    // f a (f b c)`). (2) A 2-given RELATIONAL law that pairs a both-args-peeling
    // fn with a comparison — `eq (max a b) a = lessEq b a` (prop_24/33/34): the
    // proof needs the SAME `induction g1 generalizing g2 with … cases g2` double
    // peel, plus the comparison fn's `(le a b = true) = (a ≤ b)` bridge so the
    // residual lands in arithmetic `omega` decides. A `minus(plus(n,m),n)=m`
    // (no comparison) matches NEITHER and keeps its existing bridge proof.
    let given_names: Vec<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    let is_comm_assoc =
        crate::codegen::proof_recognize::recognize_binary_law_shape(law, &vb.fn_name, &given_names)
            .is_some();

    // Comparison bridges in the law's cone (`lessEq`/`le`/`lt` → builtin `≤`/`<`),
    // proved as self-contained support theorems. Their presence is what makes the
    // relational shape closable; an arithmetic-only law has none and falls
    // through to the single-carrier emitter unchanged.
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    let (bridge_support, bridge_names, _bridged) =
        lean_nat_lift_support(law, ctx, &law_uid, &BTreeSet::new());
    let has_compare_bridge = bridge_names
        .iter()
        .any(|n| n.ends_with("_isNatLe") || n.ends_with("_isNatLt") || n.ends_with("_isNatEq"));
    let relational = !is_comm_assoc && law.givens.len() == 2 && has_compare_bridge;
    if !is_comm_assoc && !relational {
        return None;
    }

    let simp_list = law_simp_defs(ctx, vb, law)
        .into_iter()
        .collect::<Vec<_>>()
        .join(", ");
    let induct_on = &intro_names[0];
    let rest = &intro_names[1..];
    let generalizing = rest.join(" ");
    // Case-split every remaining given so each constructor pairing is exposed;
    // `simp_all` then unfolds the defs, applies the succ-arm IH, and closes.
    let cases_prefix = rest
        .iter()
        .map(|g| format!("cases {g} <;> "))
        .collect::<String>();
    // RELATIONAL only: an additive rung that adds the comparison bridges to the
    // arm `simp_all` and finishes with `omega`. Keeping the comparison fn's def
    // AND its `= true ↔ ≤` bridge in one `simp_all` is fine (the bridge is a
    // Prop-equality, not a recursive unfold, so it does not loop), then `omega`
    // discharges the `≤`/`=` residual. Runs only after the bare `; done`/`; omega`
    // rungs fail, ends in `omega`, so it throws on a leftover and degrades to the
    // honest `sorry` — purely additive, sound, can only ADD closures. The
    // comm/assoc path keeps an empty rung and is byte-identical to before.
    let bridge_rung = if relational {
        let all_simp = if bridge_names.is_empty() {
            simp_list.clone()
        } else {
            format!("{simp_list}, {}", bridge_names.join(", "))
        };
        format!(" | ({cases_prefix}simp_all [{all_simp}] <;> omega)")
    } else {
        String::new()
    };
    let ladder = format!(
        "first | ({cases_prefix}simp_all [{simp_list}]; done) | ({cases_prefix}simp_all [{simp_list}]; omega){bridge_rung} | sorry"
    );

    let proof_lines = vec![
        format!("  intro {}", intro_names.join(" ")),
        format!("  induction {induct_on} generalizing {generalizing} with"),
        format!("  | zero => {ladder}"),
        format!("  | succ k ih => {ladder}"),
    ];

    Some(AutoProof {
        support_lines: if relational {
            bridge_support
        } else {
            Vec::new()
        },
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

#[allow(clippy::too_many_arguments)]
fn emit_simple_induction(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    target_idx: usize,
    type_name: &str,
    variants: &[TypeVariant],
    discovered: &[String],
) -> Option<AutoProof> {
    let mut simp_defs: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    // Discovery feedback: COMMITTED pins join the arm simp sets (see
    // `emit_list_induction`); EARLIER sibling laws (część A) feed only the
    // fast path. Empty `fast_simp` (no committed, no eligible sibling) keeps
    // the emit byte-identical to the pre-feedback ladder.
    let discovered_simp = discovered_simp_entries(ctx, discovered);
    let siblings = earlier_law_lemmas(vb, law, ctx);
    let fast_simp = fastpath_simp_entries(ctx, discovered, &siblings);
    simp_defs.extend(discovered_simp.iter().cloned());
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let target_lean = &intro_names[target_idx];
    let premise_names = premise_intro_names(law, intro_names);

    // Canonical-Peano operation bridges: lift any `+`/`-`/`*`/`≤`/`<` the law
    // uses to the builtin so `omega` (or core `Nat.mul_*` lemmas) decides the
    // goal directly. Kept SEPARATE from the induction's `simp` set — mixing a
    // fn's def equations with its `= a + b` bridge in one `simp` call leaves the
    // rewrite stuck — and applied as a `simp only [bridges] <;> omega` fast path
    // tried BEFORE induction.
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    let lemma_fns = feedback_source_fns(ctx, discovered, &siblings);
    let (arith_support, arith_bridges, bridged_fns) =
        lean_nat_lift_support(law, ctx, &law_uid, &lemma_fns);

    let mut intro_parts = intro_names.to_vec();
    intro_parts.extend(premise_names.iter().cloned());

    // Per-variant induction arms. Each closes fully or degrades to an honest
    // `sorry` — and BUILDS either way. `induction .. with | arm => tac` requires
    // the arm tactic to close its goal; a leftover goal is an `unsolved goals`
    // ERROR (a hard lake-build failure), not a countable `sorry`. Gate on
    // `first | (simp[_all] [defs]; done) | (simp[_all] [defs]; omega) | sorry`:
    // `; done` turns a non-closing `simp` into a throw that `first` catches; the
    // `omega` arm discharges a linear-arithmetic residual (sound — closes only
    // true goals); anything still unproved becomes an honest building `sorry`.
    //
    // When the induction target is a canonical Peano type lifted to builtin
    // `Nat`, the arm names must be Lean's `Nat` constructors (`zero`/`succ`),
    // not the user's lowercased `z`/`s`.
    let peano = crate::codegen::proof_recognize::peano_type_named(ctx, type_name);
    // Feedback mode (`SimpOverLemmas` + canonical-op bridges present): each
    // arm gets a bridge branch before `sorry` — defs/lemmas first, then
    // `simp only [bridges] <;> omega` for the arithmetic residual a stuck
    // Peano op leaves (see `emit_list_induction`'s `mk_arms`). Sound, so the
    // branch can only add closures.
    //
    // część A2 — the arm bridge fires whenever a canonical-Peano op bridge is
    // in scope, NOT only in `--discover` mode. `double x = plus x x` needs the
    // `plus = +` bridge applied INSIDE the succ arm: after `simp [double]` +
    // the IH the goal is `S (S (plus y y)) = plus (S y) (S y)`, whose RHS is
    // stuck (`plus` recurses on a symbolic arg) but is pure linear arithmetic
    // once `plus` rewrites to `+`. The top-level `simp only [bridge] <;> omega`
    // fast path can't reach it (`double x` stays opaque there), so the bridge
    // must ride the induction arm. Sound — `omega` decides only true goals; a
    // residual it can't close still degrades to the honest `sorry`.
    let arm_bridge = if !arith_bridges.is_empty() {
        Some(arith_bridges.join(", "))
    } else {
        None
    };

    // część A2 (the `max`/`min` consumer) — kernel-proved commutativity lemmas
    // for both-args-peeling Peano fns this law's cone consumes. `height (mirror
    // t) = height t` collapses (after the node-arm IH) to `max (height r)
    // (height l) = max (height l) (height r)`, closable only with `max`'s
    // commutativity in the simp set. The lemma's simp set DROPS the comm'd fn's
    // own def (unfolding `max'` alongside its `= flip` rewrite leaves the goal
    // stuck), keeping every other law def. Both branches sound: a goal needing
    // no commutativity still reaches the plain ladder, and the lemma is proved
    // (a non-commutative fn never reaches here), never minting a false theorem.
    let (comm_support, comm_lemma_names) = consumed_comm_lemmas(ctx, vb, law, &law_uid);
    let comm_arm: Option<(String, String)> = if comm_lemma_names.is_empty() {
        None
    } else {
        let commd_fns: BTreeSet<String> = comm_lemma_names
            .iter()
            .filter_map(|n| n.strip_suffix("_comm"))
            .filter_map(|s| s.strip_prefix(&format!("{law_uid}_")))
            .map(str::to_string)
            .collect();
        let mut comm_set: Vec<String> = law_simp_defs(ctx, vb, law)
            .into_iter()
            .filter(|d| !commd_fns.contains(d))
            .collect();
        comm_set.extend(comm_lemma_names.iter().cloned());
        let set = comm_set.join(", ");
        Some((
            format!(" | (simp [{set}]; done)"),
            format!(" | (simp_all [{set}]; done)"),
        ))
    };

    let mut arm_lines: Vec<String> = Vec::new();
    for variant in variants {
        let lean_variant = match &peano {
            Some(p) if variant.name == p.base_ctor => "zero".to_string(),
            Some(p) if variant.name == p.succ_ctor => "succ".to_string(),
            _ => to_lower_first(&variant.name),
        };
        let field_binders: Vec<String> = (0..variant.fields.len())
            .map(|index| format!("f{}", index))
            .collect();

        match classify_variant(variant, type_name) {
            VariantKind::Leaf => {
                let binders = if field_binders.is_empty() {
                    String::new()
                } else {
                    format!(" {}", field_binders.join(" "))
                };
                let bridge = arm_bridge
                    .as_deref()
                    .map(|b| format!(" | (simp [{d}]; simp only [{b}] <;> omega)", d = simp_list))
                    .unwrap_or_default();
                let comm = comm_arm
                    .as_ref()
                    .map(|(leaf, _)| leaf.as_str())
                    .unwrap_or_default();
                arm_lines.push(format!(
                    "| {v}{b} => first | (simp [{d}]; done) | (simp [{d}]; omega){bridge}{comm} | sorry",
                    v = lean_variant,
                    b = binders,
                    d = simp_list
                ));
            }
            VariantKind::DirectRec => {
                let ih_names: Vec<String> = variant
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, field)| field.trim() == type_name)
                    .map(|(index, _)| format!("ih{}", index))
                    .collect();

                let bridge = arm_bridge
                    .as_deref()
                    .map(|b| {
                        format!(
                            " | (simp_all [{d}]; simp only [{b}] <;> omega)",
                            d = simp_list
                        )
                    })
                    .unwrap_or_default();
                let comm = comm_arm
                    .as_ref()
                    .map(|(_, rec)| rec.as_str())
                    .unwrap_or_default();
                arm_lines.push(format!(
                    "| {v} {b} {ih} => first | (simp_all [{d}]; done) | (simp_all [{d}]; omega){bridge}{comm} | sorry",
                    v = lean_variant,
                    b = field_binders.join(" "),
                    ih = ih_names.join(" "),
                    d = simp_list
                ));
            }
            VariantKind::IndirectRec => return None,
        }
    }

    let mut proof_lines = vec![format!("  intro {}", intro_parts.join(" "))];
    if arith_bridges.is_empty() && fast_simp.is_empty() {
        // No arithmetic to lift, no committed/sibling lemmas: plain structural
        // induction.
        proof_lines.push(format!("  induction {} with", target_lean));
        proof_lines.extend(arm_lines.into_iter().map(|a| format!("  {a}")));
    } else {
        // Try the arithmetic fast path first; fall back to induction. The fast
        // path closes pure-arith identities like `(n+m)-n=m` that structural
        // induction leaves at `sorry`; the induction fallback preserves every
        // case the bare strategy already proved (a law that merely MENTIONS
        // `plus`/`minus` but needs induction just fails the fast path and
        // proceeds), so the wrapping can only ever ADD coverage. With a
        // `SimpOverLemmas` pin the discovered lemma names join the fast path
        // (and a second def-unfolding variant is tried — see
        // `emit_list_induction`); the bridged fns' own defs stay out of the
        // `simp only` calls (def + bridge in one call sticks).
        // Commute normalizers for the fast path only — see the same const in
        // `emit_list_induction` for the rationale (a `f (a + b) = f (b + a)`
        // commute residual under an opaque user fn `f` closes by `simp`'s
        // permutative ordering, never `omega`; permutative rules don't loop).
        const COMMUTE_NORMALIZERS: &[&str] = &[
            "Nat.add_comm",
            "Nat.mul_comm",
            "Int.add_comm",
            "Int.mul_comm",
        ];
        let mut fast_lemmas: Vec<String> = fast_simp.clone();
        fast_lemmas.extend(arith_bridges.iter().cloned());
        if !fast_simp.is_empty() {
            fast_lemmas.extend(COMMUTE_NORMALIZERS.iter().map(|s| s.to_string()));
        }
        proof_lines.push("  first".to_string());
        // Structural-only rung (bridge-free), tried BEFORE the bridged rung
        // below — see `emit_list_induction` for the full rationale. The `plus →
        // +` bridge plus `Nat.add_comm` rewrite a `S (S y)` literal to a numeral
        // and reorder the `+`s, stranding an `S (S)`-shaped sibling law (`even
        // ((n+1)+1) = even n`) so it no longer matches and `omega` (blind to the
        // opaque `even`) cannot finish. Trying the siblings alone lets the pure
        // structural rewrite close by `rfl` (prod lemma_16's `even`-plus-shift).
        // Gated on non-empty siblings; `; done` throws on a leftover, so it is
        // strictly additive (falls through to the bridged rung below).
        if !fast_simp.is_empty() {
            proof_lines.push(format!("  | (simp only [{}]; done)", fast_simp.join(", ")));
        }
        proof_lines.push(format!(
            "  | (simp only [{}] <;> omega)",
            fast_lemmas.join(", ")
        ));
        if !fast_simp.is_empty() {
            let fast_unfolds: BTreeSet<String> = law_simp_defs(ctx, vb, law)
                .into_iter()
                .chain(fast_simp.iter().cloned())
                .chain(arith_bridges.iter().cloned())
                .chain(COMMUTE_NORMALIZERS.iter().map(|s| s.to_string()))
                .filter(|n| !bridged_fns.contains(n))
                .collect();
            // Skip the def-unfolding variant when it collapses to the lemma+
            // bridge line above (every law def bridged out) — no duplicate alt.
            let fast_lemmas_set: BTreeSet<String> = fast_lemmas.iter().cloned().collect();
            if fast_unfolds != fast_lemmas_set {
                proof_lines.push(format!(
                    "  | (simp only [{}] <;> omega)",
                    fast_unfolds.into_iter().collect::<Vec<_>>().join(", ")
                ));
            }
        }
        proof_lines.push(format!("  | (induction {} with", target_lean));
        let last = arm_lines.len().saturating_sub(1);
        for (idx, arm) in arm_lines.into_iter().enumerate() {
            if idx == last {
                proof_lines.push(format!("     {arm})"));
            } else {
                proof_lines.push(format!("     {arm}"));
            }
        }
    }

    let mut support_lines = discovered_support_lines(ctx, vb, law, discovered);
    support_lines.extend(arith_support);
    support_lines.extend(comm_support);
    Some(AutoProof {
        support_lines,
        body: crate::codegen::lean::tactic_ir::Tactic::raw(proof_lines),
        replaces_theorem: false,
    })
}

/// `aver proof --explain` residual probe. Given the EMITTED Lean source lines of
/// ONE main law theorem (the `theorem … := by` line through its last arm), render
/// a NORMALIZATION-ONLY twin whose body strips the closing tactic cascade so
/// Lean's elaborator reports the law's residual (`unsolved goals`) instead of
/// closing it with `sorry`/`omega`/`split`. The leftover goal IS what a
/// Lemma-Calculation agent applies the IH against, so the cons arm's IH must stay
/// in canonical recursive form: each arm is replaced by `(try simp only [<the
/// theorem's own def simp set>, List.cons_append])` — def-unfold + cons-peel ONLY,
/// no `done`/`omega`/`split`/`simp_all`/`| sorry`.
///
/// Only the clean, top-level `intro …; induction <t> with | <pat> => …` shape is
/// probed (the dominant open-list / open-Nat family, e.g. prop_49 butlast). A
/// theorem with no top-level `induction … with`, or whose body is the nested
/// `first | (induction …)` feedback cascade, returns `None` — the caller leaves
/// that law's `open_goal` unset rather than emit a misleading empty residual
/// (scout risk: "no inductive probe shape → leave open_goal None").
///
/// `thm_lines` are the verbatim source lines of the theorem (leading whitespace
/// intact). `probe_name` is substituted for the original theorem name so several
/// probes can coexist in one file without clashing. Returns the probe theorem as
/// a single `\n`-joined block.
pub fn residual_probe_body(thm_lines: &[&str], probe_name: &str) -> Option<String> {
    // 1. The statement line: `theorem <name> : <stmt> := by`. Swap only the name
    //    token so the probe is independently nameable; keep the statement verbatim.
    let stmt_line = thm_lines.first()?;
    let after_kw = stmt_line.trim_start().strip_prefix("theorem ")?;
    let name_end = after_kw.find(char::is_whitespace)?;
    let rest = &after_kw[name_end..]; // ` : <stmt> := by` (or `:= by`)
    if !rest.trim_end().ends_with(":= by") {
        // Single-line `:= by native_decide` / `:= by decide` etc. — no inductive
        // skeleton to strip; not a residual-bearing shape.
        return None;
    }

    // 2. Find the top-level `induction <target> with` and `intro …` lines, plus
    //    the arm lines. A nested `first | (induction …)` cascade has no arm at the
    //    theorem's top indentation, so it is rejected (returns None) below.
    let mut intro_line: Option<&str> = None;
    let mut induction_line: Option<&str> = None;
    let mut arm_lines: Vec<&str> = Vec::new();
    let mut simp_defs: BTreeSet<String> = BTreeSet::new();
    for line in &thm_lines[1..] {
        let t = line.trim_start();
        if t.starts_with("intro ") && intro_line.is_none() {
            intro_line = Some(line);
        } else if (t.starts_with("induction ") && t.ends_with(" with")) && induction_line.is_none()
        {
            induction_line = Some(line);
        } else if t.starts_with("| ") && induction_line.is_some() {
            arm_lines.push(line);
        }
        // Harvest the def names the emitter put in this theorem's simp sets so the
        // probe normalizes with the SAME unfold set (def names only — `← `-reversed
        // and lemma helpers are skipped to keep normalization confluent).
        collect_simp_idents(line, &mut simp_defs);
    }
    let induction_line = induction_line?;
    if arm_lines.is_empty() {
        return None;
    }
    simp_defs.insert("List.cons_append".to_string());
    let simp_set = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let strip = format!("(try simp only [{simp_set}])");

    // 3. Reassemble: statement (renamed) + intro + induction + each arm's pattern
    //    head with the cascade replaced by the normalization-only `strip`. Re-
    //    indent every reconstructed line to a uniform 2-space body indent. The
    //    source `induction`/arms may sit DEEPER than the theorem's `intro`: the
    //    `fun_induction` first-rung wraps the manual ladder in a
    //    `first | (fun_induction …) | ( induction … )` cascade, nesting the
    //    `induction` and its arms one level in. Copying their original (deeper)
    //    indentation verbatim while emitting `intro` shallow yields a probe whose
    //    `intro`/`induction` columns disagree, which Lean rejects (an `introN`
    //    parse failure, never the `unsolved goals` the caller reads). Flattening
    //    to a 2-space block makes the probe a clean top-level `induction`
    //    regardless of how deep it sat in the source body.
    let mut out = vec![format!("theorem {probe_name}{rest}")];
    if let Some(intro) = intro_line {
        out.push(format!("  {}", intro.trim_start()));
    }
    out.push(format!("  {}", induction_line.trim_start()));
    for arm in arm_lines {
        // Keep everything up to and including the `=>`, drop the tactic cascade.
        let arm = arm.trim();
        if let Some(arrow) = arm.find("=>") {
            let head = arm[..arrow + 2].trim_end();
            out.push(format!("  {head} {strip}"));
        } else {
            out.push(format!("  {arm} {strip}"));
        }
    }
    Some(out.join("\n"))
}

/// Collect the def/lemma identifiers inside every `simp[_all|_only] [ … ]` bracket
/// on `line` into `set`, skipping `← `-reversed rewrites (non-confluent as
/// normalization rules) and the structural `List.cons_append` peel (re-added by
/// the caller). Used by [`residual_probe_body`] to reuse the theorem's OWN unfold
/// set for the normalization-only probe.
fn collect_simp_idents(line: &str, set: &mut BTreeSet<String>) {
    let mut rest = line;
    while let Some(open) = rest.find('[') {
        let after = &rest[open + 1..];
        let Some(close) = after.find(']') else { break };
        let inner = &after[..close];
        for tok in inner.split(',') {
            let tok = tok.trim();
            if tok.is_empty() || tok.starts_with("← ") || tok == "List.cons_append" {
                continue;
            }
            // Plain identifiers only (def/theorem names); skip anything carrying
            // tactic punctuation that slipped into a bracket scan.
            if tok
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '.' || c == '\'')
            {
                set.insert(tok.to_string());
            }
        }
        rest = &after[close + 1..];
    }
}
