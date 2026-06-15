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
use super::AutoProof;
use super::shared::law_simp_defs;
use crate::ast::{TypeDef, TypeVariant, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// Lean renderer for the backend-neutral rev anti-homomorphism recognizer
/// (`crate::codegen::proof_recognize::collect_rev_ops_in_law` — shared with the
/// Dafny backend; despite the module path it returns source-name structs only).
/// Produces the proved append-nil-right / associativity / rev-distribution
/// theorems (prepended as `support_lines`) and the distribution lemma's name to
/// add to the induction's `simp` set. List<Int> folds lower to clean
/// `def … termination_by`, so these close kernel-clean (`#print axioms =
/// [propext]`) — the Lean counterpart of the Dafny rev strategy, SAME recognizer.
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
        support.push(format!(
            "theorem {nilr} : ∀ (xa : List Int), {a} xa [] = xa := by\n  intro xa; induction xa with\n  | nil => simp [{a}]\n  | cons h t ih => simp [{a}, ih]"
        ));
        support.push(format!(
            "theorem {assoc} : ∀ (xa xb xc : List Int), {a} ({a} xa xb) xc = {a} xa ({a} xb xc) := by\n  intro xa xb xc; induction xa with\n  | nil => simp [{a}]\n  | cons h t ih => simp [{a}, ih]"
        ));
        support.push(format!(
            "theorem {dist} : ∀ (xa xb : List Int), {r} ({a} xa xb) = {a} ({r} xb) ({r} xa) := by\n  intro xa xb; induction xa with\n  | nil => simp [{r}, {a}, {nilr}]\n  | cons h t ih => simp [{r}, {a}, ih, {assoc}]"
        ));
        // The append fn name is needed in the main induction's simp set too
        // (revDist rewrites into `append`, which must then unfold).
        simp_extra.push(a);
        simp_extra.push(dist);
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
fn lean_nat_lift_support(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    law_uid: &str,
    extra_fns: &BTreeSet<String>,
) -> (Vec<String>, Vec<String>, BTreeSet<String>) {
    use crate::codegen::proof_recognize::{NatArithKind, NatCompareKind};
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
                support.push(format!(
                    "theorem {name} : ∀ a b, {f} a b = a + b := by\n  intro a b\n  induction a with\n  | zero => first | (simp [{f}]; done) | (simp [{f}]; omega) | sorry\n  | succ k ih => first | (simp [{f}, ih]; done) | (simp [{f}, ih]; omega) | sorry"
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
                support.push(format!(
                    "theorem {name} : ∀ a b, {f} a b = a * b := by\n  intro a b\n  induction a with\n  | zero => first | (simp [{f}]; done) | (simp [{f}]; omega) | sorry\n  | succ k ih => first | (simp only [{f}, {add_name}, ih, Nat.succ_mul, Nat.add_comm]) | sorry"
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
        match op.kind {
            // `(le a b = true) = (a ≤ b)`: a Prop-equality (propext) so `simp only`
            // rewrites the Bool goal `le _ _ = true` straight into `_ ≤ _` for omega.
            NatCompareKind::Le => {
                let name = format!("{law_uid}_{f}_isNatLe");
                support.push(format!(
                    "theorem {name} : ∀ a b, ({f} a b = true) = (a ≤ b) := by\n  intro a b\n  induction a generalizing b with\n  | zero => first | (simp [{f}]) | sorry\n  | succ k ih => cases b with\n    | zero => first | (simp [{f}]) | sorry\n    | succ w => first | (simp [{f}, ih]) | sorry"
                ));
                simp_extra.push(name);
            }
            // `<` matches its SECOND arg first, so the bridge inducts on `b`.
            NatCompareKind::Lt => {
                let name = format!("{law_uid}_{f}_isNatLt");
                support.push(format!(
                    "theorem {name} : ∀ a b, ({f} a b = true) = (a < b) := by\n  intro a b\n  induction b generalizing a with\n  | zero => cases a <;> first | (simp [{f}]) | sorry\n  | succ k ih => cases a <;> first | (simp [{f}, ih]) | sorry"
                ));
                simp_extra.push(name);
            }
        }
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
        if mentions.is_subset(&scope)
            || (mentions.contains(&subject) && scope.contains(&prev_subject))
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
        proof_lines,
        replaces_theorem: false,
    }
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
    simp_defs.extend(discovered_simp.iter().cloned());
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let target_lean = &intro_names[target_idx];

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
        "List.cons_append".to_string()
    } else {
        format!("{simp_list}, List.cons_append")
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
        let tail = if with_sorry { " | sorry" } else { "" };
        (
            format!(
                "| nil => first | (simp [{arm_simp}]; done) | (simp [{arm_simp}]; omega){nil_bridge} | (simp only [{arm_split}]; split <;> simp_all [{arm_simp}]{split_bridge} <;> omega){tail}"
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
                "| cons head tail ih => first | (simp_all [{arm_simp}]; done) | (simp_all [{arm_simp}]; omega){cons_bridge} | (simp only [{arm_split}]; split <;> simp_all [{arm_simp}]{split_bridge} <;> omega) | (cases tail <;> simp_all [{arm_simp}] <;> omega){tail}"
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
        let (nil_arm, cons_arm) = mk_arms(&simp_list, &split_set, None, true);
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
        let mut fast_lemmas: Vec<String> = fast_simp.clone();
        fast_lemmas.extend(arith_bridges.iter().cloned());
        let fast_unfolds: BTreeSet<String> = law_simp_defs(ctx, vb, law)
            .into_iter()
            .chain(fast_simp.iter().cloned())
            .chain(arith_bridges.iter().cloned())
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
            let (nil_arm, cons_arm) = mk_arms(&simp_list, &split_set, bridge_set.as_deref(), true);
            proof_lines.push(format!("  | (induction {} with", target_lean));
            proof_lines.push(format!("     {nil_arm}"));
            proof_lines.push(format!("     {cons_arm})"));
        } else {
            // ladderA: committed-only arms, NO sorry (throws → ladderB).
            let (nil_a, cons_a) = mk_arms(&simp_list, &split_set, bridge_set.as_deref(), false);
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
            let (nil_b, cons_b) = mk_arms(&simp_b, &split_b, bridge_set.as_deref(), true);
            proof_lines.push(format!("  | (induction {} with", target_lean));
            proof_lines.push(format!("     {nil_b}"));
            proof_lines.push(format!("     {cons_b})"));
        }
        support_lines.extend(discovered_support_lines(ctx, vb, law, discovered));
        support_lines.extend(arith_support);
    }
    support_lines.extend(rev_support);

    Some(AutoProof {
        support_lines,
        proof_lines,
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
    // The law must be a genuine commutativity / associativity of THIS fn over
    // its givens — not merely some 2-/3-given law that happens to mention a
    // both-args-peeling fn. A `minus(plus(n,m),n)=m` or a `n ≤ m+n` keeps its
    // existing bridge proof; this generalizing template fires only on
    // `f a b = f b a` / `f (f a b) c = f a (f b c)`.
    let given_names: Vec<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    crate::codegen::proof_recognize::recognize_binary_law_shape(law, &vb.fn_name, &given_names)?;

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
    let ladder = format!(
        "first | ({cases_prefix}simp_all [{simp_list}]; done) | ({cases_prefix}simp_all [{simp_list}]; omega) | sorry"
    );

    let proof_lines = vec![
        format!("  intro {}", intro_names.join(" ")),
        format!("  induction {induct_on} generalizing {generalizing} with"),
        format!("  | zero => {ladder}"),
        format!("  | succ k ih => {ladder}"),
    ];

    Some(AutoProof {
        support_lines: Vec::new(),
        proof_lines,
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
        let mut fast_lemmas: Vec<String> = fast_simp.clone();
        fast_lemmas.extend(arith_bridges.iter().cloned());
        proof_lines.push("  first".to_string());
        proof_lines.push(format!(
            "  | (simp only [{}] <;> omega)",
            fast_lemmas.join(", ")
        ));
        if !fast_simp.is_empty() {
            let fast_unfolds: BTreeSet<String> = law_simp_defs(ctx, vb, law)
                .into_iter()
                .chain(fast_simp.iter().cloned())
                .chain(arith_bridges.iter().cloned())
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
        proof_lines,
        replaces_theorem: false,
    })
}
