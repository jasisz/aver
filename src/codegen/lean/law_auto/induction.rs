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

/// Lean renderer for the backend-neutral canonical-Peano-arithmetic recognizer
/// (`collect_nat_arith_ops_in_law`). For each user fn the law invokes that IS
/// the standard Peano `+` / truncated `-`, emit a kernel-CHECKED bridge lemma
/// `<fn> a b = a + b` (proved by induction over the lifted builtin `Nat`) and
/// return its name for the law's `simp` set. Rewriting the user op to the host
/// builtin hands the goal to Lean's `omega`, which decides linear Nat
/// arithmetic with truncated subtraction — closing identities like `(n+m)-n=m`
/// that structural induction alone leaves at `sorry`. The bridge is PROVED, not
/// trusted: a misrecognized op makes the bridge proof fail (degrading to an
/// honest `sorry` caught by the sorry-gate), never a false theorem. Names are
/// law-scoped (`law_uid`) so multiple laws in one module don't collide.
fn lean_nat_arith_support(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    law_uid: &str,
) -> (Vec<String>, Vec<String>) {
    use crate::codegen::proof_recognize::NatArithKind;
    let mut support = Vec::new();
    let mut simp_extra = Vec::new();
    for op in crate::codegen::proof_recognize::collect_nat_arith_ops_in_law(law, ctx) {
        let f = aver_name_to_lean(&op.fn_name);
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
        }
    }
    (support, simp_extra)
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

pub(super) fn emit_structural_induction_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    _theorem_base: &str,
    _quant_params: &str,
    _theorem_prop: &str,
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
        return emit_list_induction(vb, law, ctx, intro_names, target_idx);
    }

    // (b) A `given` is a user-defined recursive sum type: structural induction
    //     over its variants.
    if let Some((target_idx, _target_name, type_name)) = sum_target {
        let (_, variants) = find_sum_type(ctx, type_name)?;
        if has_indirect_variants(variants, type_name) {
            return None;
        }
        return emit_simple_induction(vb, law, ctx, intro_names, target_idx, type_name, variants);
    }

    // (c) No sum-type given, but a builtin `List<T>` given is present.
    if let Some(target_idx) = list_target {
        return emit_list_induction(vb, law, ctx, intro_names, target_idx);
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
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let target_lean = &intro_names[target_idx];

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
    let proof_lines = vec![
        format!("  intro {}", intro_names.join(" ")),
        format!("  induction {} with", target_lean),
        format!(
            "  | nil => first | (simp [{d}]; done) | (simp [{d}]; omega) | (simp only [{s}]; split <;> simp_all [{d}] <;> omega) | sorry",
            d = simp_list,
            s = split_set
        ),
        format!(
            "  | cons head tail ih => first | (simp_all [{d}]; done) | (simp_all [{d}]; omega) | (simp only [{s}]; split <;> simp_all [{d}] <;> omega) | sorry",
            d = simp_list,
            s = split_set
        ),
    ];

    Some(AutoProof {
        support_lines: rev_support,
        proof_lines,
        replaces_theorem: false,
    })
}

fn emit_simple_induction(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    target_idx: usize,
    type_name: &str,
    variants: &[TypeVariant],
) -> Option<AutoProof> {
    let simp_defs: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let target_lean = &intro_names[target_idx];
    let premise_names = premise_intro_names(law, intro_names);

    // Canonical-Peano-arithmetic bridges: lift any `plus`/`minus` the law uses
    // to builtin `+`/`-` so `omega` can decide the goal directly. Kept SEPARATE
    // from the induction's `simp` set — mixing a fn's def equations with its
    // `= a + b` bridge in one `simp` call leaves the rewrite stuck — and applied
    // as a `simp only [bridges] <;> omega` fast path tried BEFORE induction.
    let law_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );
    let (arith_support, arith_bridges) = lean_nat_arith_support(law, ctx, &law_uid);

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
                arm_lines.push(format!(
                    "| {v}{b} => first | (simp [{d}]; done) | (simp [{d}]; omega) | sorry",
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

                arm_lines.push(format!(
                    "| {v} {b} {ih} => first | (simp_all [{d}]; done) | (simp_all [{d}]; omega) | sorry",
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
    if arith_bridges.is_empty() {
        // No arithmetic to lift: plain structural induction.
        proof_lines.push(format!("  induction {} with", target_lean));
        proof_lines.extend(arm_lines.into_iter().map(|a| format!("  {a}")));
    } else {
        // Try the arithmetic fast path first; fall back to induction. The fast
        // path closes pure-arith identities like `(n+m)-n=m` that structural
        // induction leaves at `sorry`; the induction fallback preserves every
        // case the bare strategy already proved (a law that merely MENTIONS
        // `plus`/`minus` but needs induction just fails the fast path and
        // proceeds), so the wrapping can only ever ADD coverage.
        proof_lines.push("  first".to_string());
        proof_lines.push(format!(
            "  | (simp only [{}] <;> omega)",
            arith_bridges.join(", ")
        ));
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

    Some(AutoProof {
        support_lines: arith_support,
        proof_lines,
        replaces_theorem: false,
    })
}
