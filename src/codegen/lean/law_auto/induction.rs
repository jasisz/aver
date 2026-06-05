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

use super::super::shared::to_lower_first;
use super::AutoProof;
use super::shared::law_simp_defs;
use crate::ast::{TypeDef, TypeVariant, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

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

    // (a) A `given` is a user-defined recursive sum type: structural induction
    //     over its variants.
    if let Some((target_idx, _target_name, type_name)) = find_induction_target(law, ctx) {
        let (_, variants) = find_sum_type(ctx, type_name)?;
        if has_indirect_variants(variants, type_name) {
            return None;
        }
        return emit_simple_induction(vb, law, ctx, intro_names, target_idx, type_name, variants);
    }

    // (b) A `given` is a builtin `List<T>`: structural induction via Lean's
    //     nil/cons. The Lean-side counterpart to Dafny's already-shipped
    //     `|xs| == 0 / xs[1..]` list-given idiom (#409 Gap A) — closes
    //     universal laws over user list-recursive fns that previously fell
    //     through to `sorry`.
    if let Some(target_idx) = find_list_induction_target(law) {
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
    let simp_defs: BTreeSet<String> = law_simp_defs(ctx, vb, law);
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");
    let target_lean = &intro_names[target_idx];

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
    let proof_lines = vec![
        format!("  intro {}", intro_names.join(" ")),
        format!("  induction {} with", target_lean),
        format!(
            "  | nil => first | (simp [{d}]; done) | (simp [{d}]; omega) | sorry",
            d = simp_list
        ),
        format!(
            "  | cons head tail ih => first | (simp_all [{d}]; done) | (simp_all [{d}]; omega) | sorry",
            d = simp_list
        ),
    ];

    Some(AutoProof {
        support_lines: Vec::new(),
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

    let mut proof_lines = Vec::new();
    let mut intro_parts = intro_names.to_vec();
    intro_parts.extend(premise_names.iter().cloned());
    proof_lines.push(format!("  intro {}", intro_parts.join(" ")));
    proof_lines.push(format!("  induction {} with", target_lean));

    for variant in variants {
        let lean_variant = to_lower_first(&variant.name);
        let field_binders: Vec<String> = (0..variant.fields.len())
            .map(|index| format!("f{}", index))
            .collect();

        // Each arm closes fully or degrades to an honest `sorry` — and
        // BUILDS either way. `induction .. with | arm => tac` requires the
        // arm tactic to close its goal; a leftover goal is an
        // `unsolved goals` ERROR (a hard lake-build failure), not a
        // countable `sorry`. Gate on `first | (simp[_all] [defs]; done) |
        // (simp[_all] [defs]; omega) | sorry` (matching the List-induction
        // path): `; done` turns a non-closing `simp` into a throw that `first`
        // catches; the `omega` arm then discharges any linear-arithmetic
        // residual (sound — closes only true goals); anything still unproved
        // becomes an honest building sorry rather than a false-RED hard error.
        match classify_variant(variant, type_name) {
            VariantKind::Leaf => {
                if field_binders.is_empty() {
                    proof_lines.push(format!(
                        "  | {v} => first | (simp [{d}]; done) | (simp [{d}]; omega) | sorry",
                        v = lean_variant,
                        d = simp_list
                    ));
                } else {
                    proof_lines.push(format!(
                        "  | {v} {b} => first | (simp [{d}]; done) | (simp [{d}]; omega) | sorry",
                        v = lean_variant,
                        b = field_binders.join(" "),
                        d = simp_list
                    ));
                }
            }
            VariantKind::DirectRec => {
                let ih_names: Vec<String> = variant
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, field)| field.trim() == type_name)
                    .map(|(index, _)| format!("ih{}", index))
                    .collect();

                proof_lines.push(format!(
                    "  | {v} {b} {ih} => first | (simp_all [{d}]; done) | (simp_all [{d}]; omega) | sorry",
                    v = lean_variant,
                    b = field_binders.join(" "),
                    ih = ih_names.join(" "),
                    d = simp_list
                ));
            }
            VariantKind::IndirectRec => return None,
        }
    }

    Some(AutoProof {
        support_lines: Vec::new(),
        proof_lines,
        replaces_theorem: false,
    })
}
