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
        if mentions.is_subset(&scope) || mentions.contains(&subject) {
            out.push(crate::codegen::lemma_discovery::CommittedLemma::reference(
                name, text,
            ));
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
    let program_fns = program_fn_lean_names(ctx);
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
            format!(
                "| cons head tail ih => first | (simp_all [{arm_simp}]; done) | (simp_all [{arm_simp}]; omega){cons_bridge} | (simp only [{arm_split}]; split <;> simp_all [{arm_simp}]{split_bridge} <;> omega){tail}"
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
    } else if fast_simp.is_empty() {
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
        proof_lines.push(format!(
            "  | (simp only [{}] <;> omega)",
            fast_unfolds.into_iter().collect::<Vec<_>>().join(", ")
        ));
        if arm_forward_siblings.is_empty() {
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
    // branch can only add closures; absent in plain mode (byte-identical).
    let arm_bridge = if !discovered_simp.is_empty() && !arith_bridges.is_empty() {
        Some(arith_bridges.join(", "))
    } else {
        None
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
                arm_lines.push(format!(
                    "| {v}{b} => first | (simp [{d}]; done) | (simp [{d}]; omega){bridge} | sorry",
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
                arm_lines.push(format!(
                    "| {v} {b} {ih} => first | (simp_all [{d}]; done) | (simp_all [{d}]; omega){bridge} | sorry",
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
            proof_lines.push(format!(
                "  | (simp only [{}] <;> omega)",
                fast_unfolds.into_iter().collect::<Vec<_>>().join(", ")
            ));
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
    Some(AutoProof {
        support_lines,
        proof_lines,
        replaces_theorem: false,
    })
}
