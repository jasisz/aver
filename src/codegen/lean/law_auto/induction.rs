/// Structural induction auto-proof strategy for recursive sum types.
///
/// When a `verify law` has a `given` over a recursive sum type, this strategy
/// generates an `induction t with` proof with per-variant case splits:
///   - Leaf variants: `simp [fn_defs...]`
///   - Direct-recursive variants: `simp_all [fn_defs...]`
///   - Indirect-recursive variants (List<T>): strong induction on Nat measure
///     when a companion helper is detected (otherwise fail closed)
use std::collections::BTreeSet;

use super::super::expr::aver_name_to_lean;
use super::super::shared::to_lower_first;
use super::AutoProof;
use super::shared::{body_terminal_expr, expr_dotted_name, find_fn_def, law_simp_defs};
use crate::ast::{Expr, MatchArm, Pattern, TypeDef, TypeVariant, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// Classification of a variant's recursion shape.
enum VariantKind {
    /// No fields reference the parent type.
    Leaf,
    /// At least one field is exactly the parent type (direct recursion).
    DirectRec,
    /// Fields reference the parent type only inside containers (List, Map, etc.).
    IndirectRec,
}

struct InductionInputs<'a> {
    vb: &'a VerifyBlock,
    law: &'a VerifyLaw,
    ctx: &'a CodegenContext,
    intro_names: &'a [String],
    theorem_base: &'a str,
    quant_params: &'a str,
    theorem_prop: &'a str,
}

fn premise_intro_names(law: &VerifyLaw, intro_names: &[String]) -> Vec<String> {
    let mut names = Vec::new();
    if law.when.is_some() {
        names.extend(intro_names.iter().map(|name| format!("h_{name}")));
        names.push("h_when".to_string());
    }
    names
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

/// Check if a type annotation string references `type_name` inside a container
/// (e.g. `List<Json>`, `Map<String, Json>`) but NOT as a bare occurrence.
fn field_type_contains_indirect(field_type: &str, type_name: &str) -> bool {
    if field_type.trim() == type_name {
        return false; // direct, not indirect
    }
    field_type.contains(&format!("<{}", type_name))
        || field_type.contains(&format!("{}>", type_name))
        || field_type.contains(&format!(", {}", type_name))
        || field_type.contains(&format!("{},", type_name))
}

/// Find a sum type definition by name in the codegen context.
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

/// Check if a sum type is self-referencing (any variant field mentions the type name).
fn is_recursive_sum(type_name: &str, variants: &[TypeVariant]) -> bool {
    variants.iter().any(|v| {
        v.fields.iter().any(|f| {
            f.trim() == type_name
                || f.contains(&format!("<{}", type_name))
                || f.contains(&format!("{}>", type_name))
                || f.contains(&format!(", {}", type_name))
                || f.contains(&format!("{},", type_name))
        })
    })
}

/// Find the first given whose type is a recursive sum type.
fn find_induction_target<'a>(
    law: &'a VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(usize, &'a str, &'a str)> {
    for (i, given) in law.givens.iter().enumerate() {
        if let Some((_, variants)) = find_sum_type(ctx, &given.type_name)
            && is_recursive_sum(&given.type_name, variants)
        {
            return Some((i, &given.name, &given.type_name));
        }
    }
    None
}

/// Check if there are any indirect-recursive variants in a sum type.
fn has_indirect_variants(variants: &[TypeVariant], type_name: &str) -> bool {
    variants
        .iter()
        .any(|v| matches!(classify_variant(v, type_name), VariantKind::IndirectRec))
}

/// Detect a companion helper function called on the bound variable of an
/// indirect-recursive variant.
///
/// Pattern: in main function's match, the arm for variant `Foo(items)` calls
/// `companionHelper(items)`, and that companion iterates the list calling back
/// to the main function on each element.
fn detect_companion_call(
    ctx: &CodegenContext,
    vb: &VerifyBlock,
    variant: &TypeVariant,
    type_name: &str,
) -> Option<String> {
    let main_fd = find_fn_def(ctx, &vb.fn_name)?;
    let main_body = body_terminal_expr(main_fd.body.as_ref())?;

    // main body should be a match expression
    let arms = match main_body {
        Expr::Match { arms, .. } => arms,
        _ => return None,
    };

    // Find the arm for this variant
    let arm = find_arm_for_variant(arms, variant, type_name)?;

    // In the arm body, find a function call that passes the bound variable
    // For a variant like JsonList(items), bound var is the actual pattern binding name
    let indirect_field_idx = find_indirect_field_idx(variant, type_name)?;

    // Get the actual bound variable name from the match pattern
    let bound_var = match &arm.pattern {
        Pattern::Constructor(_, bindings) => {
            if indirect_field_idx < bindings.len() {
                bindings[indirect_field_idx].clone()
            } else {
                format!("f{}", indirect_field_idx)
            }
        }
        _ => format!("f{}", indirect_field_idx),
    };

    // Extract companion function name from the arm body
    let companion_name = find_fn_call_passing_var(&arm.body, &bound_var, &vb.fn_name)?;

    // Verify the companion calls back to the main function
    verify_companion_calls_back(ctx, &companion_name, &vb.fn_name)?;

    Some(companion_name)
}

/// Find the index of the first field that has indirect recursion.
fn find_indirect_field_idx(variant: &TypeVariant, type_name: &str) -> Option<usize> {
    variant
        .fields
        .iter()
        .position(|f| field_type_contains_indirect(f, type_name))
}

/// Find a match arm whose pattern matches a given variant.
fn find_arm_for_variant<'a>(
    arms: &'a [MatchArm],
    variant: &TypeVariant,
    type_name: &str,
) -> Option<&'a MatchArm> {
    let qualified = format!("{}.{}", type_name, variant.name);
    arms.iter().find(|arm| match &arm.pattern {
        Pattern::Constructor(name, _) => name == &qualified || name == &variant.name,
        _ => false,
    })
}

fn expr_contains_ident(expr: &Expr, ident_name: &str) -> bool {
    match expr {
        Expr::Ident(name) => name == ident_name,
        Expr::FnCall(callee, args) => {
            expr_contains_ident(callee, ident_name)
                || args.iter().any(|arg| expr_contains_ident(arg, ident_name))
        }
        Expr::TailCall(call) => call
            .1
            .iter()
            .any(|arg| expr_contains_ident(arg, ident_name)),
        Expr::BinOp(_, l, r) => {
            expr_contains_ident(l, ident_name) || expr_contains_ident(r, ident_name)
        }
        Expr::Match { subject, arms, .. } => {
            expr_contains_ident(subject, ident_name)
                || arms
                    .iter()
                    .any(|arm| expr_contains_ident(&arm.body, ident_name))
        }
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            crate::ast::StrPart::Literal(_) => false,
            crate::ast::StrPart::Parsed(inner) => expr_contains_ident(inner, ident_name),
        }),
        Expr::Constructor(_, inner) => inner
            .as_ref()
            .is_some_and(|inner| expr_contains_ident(inner, ident_name)),
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => expr_contains_ident(inner, ident_name),
        Expr::List(items) | Expr::Tuple(items) => items
            .iter()
            .any(|item| expr_contains_ident(item, ident_name)),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_contains_ident(value, ident_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_contains_ident(base, ident_name)
                || updates
                    .iter()
                    .any(|(_, value)| expr_contains_ident(value, ident_name))
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_contains_ident(k, ident_name) || expr_contains_ident(v, ident_name)),
        Expr::Literal(_) | Expr::Resolved(_) => false,
    }
}

/// Search an expression for a function call that passes a specific variable as argument.
/// Returns the function name if found. Skips calls to `skip_fn`.
fn find_fn_call_passing_var(expr: &Expr, var_name: &str, skip_fn: &str) -> Option<String> {
    match expr {
        Expr::FnCall(callee, args) => {
            let callee_name = expr_dotted_name(callee)?;
            if callee_name != skip_fn && args.iter().any(|a| expr_contains_ident(a, var_name)) {
                return Some(callee_name);
            }
            // Recurse into args
            for arg in args {
                if let Some(name) = find_fn_call_passing_var(arg, var_name, skip_fn) {
                    return Some(name);
                }
            }
            None
        }
        Expr::BinOp(_, l, r) => find_fn_call_passing_var(l, var_name, skip_fn)
            .or_else(|| find_fn_call_passing_var(r, var_name, skip_fn)),
        Expr::Match { arms, subject, .. } => {
            if let Some(name) = find_fn_call_passing_var(subject, var_name, skip_fn) {
                return Some(name);
            }
            for arm in arms {
                if let Some(name) = find_fn_call_passing_var(&arm.body, var_name, skip_fn) {
                    return Some(name);
                }
            }
            None
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part
                    && let Some(name) = find_fn_call_passing_var(inner, var_name, skip_fn)
                {
                    return Some(name);
                }
            }
            None
        }
        Expr::Constructor(_, inner) => inner
            .as_ref()
            .and_then(|e| find_fn_call_passing_var(e, var_name, skip_fn)),
        _ => None,
    }
}

/// Verify that a companion function calls back to the main function somewhere in its body.
fn verify_companion_calls_back(
    ctx: &CodegenContext,
    companion_name: &str,
    main_fn_name: &str,
) -> Option<()> {
    let companion_fd = find_fn_def(ctx, companion_name)?;
    if expr_tree_calls_fn(companion_fd.body.as_ref(), main_fn_name) {
        Some(())
    } else {
        // Also check if companion calls a tail helper that calls main
        // e.g. renderJsonList -> renderJsonListTail -> toString
        let body_expr = body_terminal_expr(companion_fd.body.as_ref())?;
        let tail_helpers = find_all_called_fns(body_expr);
        for helper_name in tail_helpers {
            if let Some(helper_fd) = find_fn_def(ctx, &helper_name)
                && expr_tree_calls_fn(helper_fd.body.as_ref(), main_fn_name)
            {
                return Some(());
            }
        }
        None
    }
}

/// Check if a function body contains a call to a specific function name.
fn expr_tree_calls_fn(body: &crate::ast::FnBody, target: &str) -> bool {
    for stmt in body.stmts() {
        match stmt {
            crate::ast::Stmt::Expr(e) | crate::ast::Stmt::Binding(_, _, e) => {
                if expr_calls_fn(e, target) {
                    return true;
                }
            }
        }
    }
    false
}

fn expr_calls_fn(expr: &Expr, target: &str) -> bool {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_dotted_name(callee)
                && name == target
            {
                return true;
            }
            expr_calls_fn(callee, target) || args.iter().any(|a| expr_calls_fn(a, target))
        }
        Expr::TailCall(call) => {
            if call.0 == target {
                return true;
            }
            call.1.iter().any(|a| expr_calls_fn(a, target))
        }
        Expr::BinOp(_, l, r) => expr_calls_fn(l, target) || expr_calls_fn(r, target),
        Expr::Match { subject, arms, .. } => {
            expr_calls_fn(subject, target)
                || arms.iter().any(|arm| expr_calls_fn(&arm.body, target))
        }
        Expr::Constructor(_, inner) => inner.as_ref().is_some_and(|e| expr_calls_fn(e, target)),
        Expr::InterpolatedStr(parts) => parts.iter().any(|p| {
            if let crate::ast::StrPart::Parsed(inner) = p {
                expr_calls_fn(inner, target)
            } else {
                false
            }
        }),
        Expr::ErrorProp(inner) | Expr::Attr(inner, _) => expr_calls_fn(inner, target),
        Expr::List(items) | Expr::Tuple(items) => items.iter().any(|i| expr_calls_fn(i, target)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, v)| expr_calls_fn(v, target)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_calls_fn(base, target) || updates.iter().any(|(_, v)| expr_calls_fn(v, target))
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(k, v)| expr_calls_fn(k, target) || expr_calls_fn(v, target)),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => false,
    }
}

/// Find all function names called directly in an expression (non-recursive, top-level only).
fn find_all_called_fns(expr: &Expr) -> Vec<String> {
    let mut result = Vec::new();
    collect_called_fns(expr, &mut result);
    result
}

fn collect_called_fns(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_dotted_name(callee) {
                out.push(name);
            }
            for arg in args {
                collect_called_fns(arg, out);
            }
        }
        Expr::TailCall(call) => {
            out.push(call.0.clone());
            for arg in &call.1 {
                collect_called_fns(arg, out);
            }
        }
        Expr::Match { subject, arms, .. } => {
            collect_called_fns(subject, out);
            for arm in arms {
                collect_called_fns(&arm.body, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_called_fns(l, out);
            collect_called_fns(r, out);
        }
        _ => {}
    }
}

/// Collect companion definitions (the companion + any tail helpers it uses)
/// for use in `simp_all` lemmas.
fn collect_companion_defs(
    ctx: &CodegenContext,
    companion_name: &str,
    main_fn_name: &str,
) -> Vec<String> {
    let mut defs = vec![aver_name_to_lean(companion_name)];

    // Also find tail helpers called by companion
    if let Some(fd) = find_fn_def(ctx, companion_name)
        && let Some(body_expr) = body_terminal_expr(fd.body.as_ref())
    {
        for called in find_all_called_fns(body_expr) {
            if called != companion_name && called != main_fn_name {
                defs.push(aver_name_to_lean(&called));
            }
        }
    }

    defs
}

/// Main entry — returns AutoProof if law has a given over a recursive sum type.
pub(super) fn emit_structural_induction_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
    theorem_base: &str,
    quant_params: &str,
    theorem_prop: &str,
) -> Option<AutoProof> {
    if law.when.is_some() {
        return None;
    }

    let inputs = InductionInputs {
        vb,
        law,
        ctx,
        intro_names,
        theorem_base,
        quant_params,
        theorem_prop,
    };
    let (target_idx, _target_name, type_name) = find_induction_target(law, ctx)?;
    let (_, variants) = find_sum_type(ctx, type_name)?;

    if has_indirect_variants(variants, type_name) {
        return emit_strong_induction_law(&inputs, target_idx, type_name, variants);
    }

    emit_simple_induction(&inputs, target_idx, type_name, variants)
}

/// Simple structural induction for purely direct-recursive variants.
fn emit_simple_induction(
    inputs: &InductionInputs<'_>,
    target_idx: usize,
    type_name: &str,
    variants: &[TypeVariant],
) -> Option<AutoProof> {
    if has_indirect_variants(variants, type_name) {
        return None;
    }

    let simp_defs: BTreeSet<String> = law_simp_defs(inputs.ctx, inputs.vb, inputs.law);
    let simp_list = simp_defs.into_iter().collect::<Vec<_>>().join(", ");

    let target_lean = &inputs.intro_names[target_idx];
    let premise_names = premise_intro_names(inputs.law, inputs.intro_names);

    let mut proof_lines = Vec::new();

    // intro all givens + optional domain/when hypotheses
    {
        let mut intro_parts: Vec<String> = inputs.intro_names.to_vec();
        intro_parts.extend(premise_names.iter().cloned());
        proof_lines.push(format!("  intro {}", intro_parts.join(" ")));
    }

    // induction target with
    proof_lines.push(format!("  induction {} with", target_lean));

    // Per-variant case
    for variant in variants {
        let lean_variant = to_lower_first(&variant.name);
        let kind = classify_variant(variant, type_name);

        // Build binder names for variant fields
        let field_binders: Vec<String> = (0..variant.fields.len())
            .map(|i| format!("f{}", i))
            .collect();

        match kind {
            VariantKind::Leaf => {
                if field_binders.is_empty() {
                    proof_lines.push(format!("  | {} => simp [{}]", lean_variant, simp_list));
                } else {
                    proof_lines.push(format!(
                        "  | {} {} => simp [{}]",
                        lean_variant,
                        field_binders.join(" "),
                        simp_list,
                    ));
                }
            }
            VariantKind::DirectRec => {
                // Add induction hypotheses for direct-recursive fields
                let ih_names: Vec<String> = variant
                    .fields
                    .iter()
                    .enumerate()
                    .filter(|(_, f)| f.trim() == type_name)
                    .map(|(i, _)| format!("ih{}", i))
                    .collect();

                proof_lines.push(format!(
                    "  | {} {} {} => simp_all [{}]",
                    lean_variant,
                    field_binders.join(" "),
                    ih_names.join(" "),
                    simp_list,
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

/// Strong induction via Nat on measure function.
///
/// For indirect-recursive variants (e.g. `JsonList(List<Json>)`), simple structural
/// induction doesn't provide an IH for list elements. Strong induction on a Nat
/// bound with `averMeasureT j ≤ n` gives `∀ j', measure j' ≤ n → P(j')` as IH,
/// which covers list elements since their measure is smaller.
fn emit_strong_induction_law(
    inputs: &InductionInputs<'_>,
    target_idx: usize,
    type_name: &str,
    variants: &[TypeVariant],
) -> Option<AutoProof> {
    // For each indirect-recursive variant, detect the companion helper.
    // Track which variants have companion detection (resolved) vs not.
    let mut companion_defs: Vec<String> = Vec::new();
    let mut resolved_variants: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut any_resolved = false;

    for variant in variants {
        if matches!(
            classify_variant(variant, type_name),
            VariantKind::IndirectRec
        ) && let Some(companion_name) =
            detect_companion_call(inputs.ctx, inputs.vb, variant, type_name)
        {
            let defs = collect_companion_defs(inputs.ctx, &companion_name, &inputs.vb.fn_name);
            for d in defs {
                if !companion_defs.contains(&d) {
                    companion_defs.push(d);
                }
            }
            resolved_variants.insert(variant.name.clone());
            any_resolved = true;
        }
    }

    // Only use strong induction if at least one indirect variant was resolved
    if !any_resolved {
        return None;
    }

    let measure_fn = format!("averMeasure{}", type_name);
    let measure_list_fn = format!("averMeasure{}List", type_name);
    let target_lean = &inputs.intro_names[target_idx];
    let premise_names = premise_intro_names(inputs.law, inputs.intro_names);

    let simp_defs: BTreeSet<String> = law_simp_defs(inputs.ctx, inputs.vb, inputs.law);
    let mut all_simp: Vec<String> = simp_defs.into_iter().collect();
    all_simp.push(measure_fn.clone());
    all_simp.push(measure_list_fn.clone());
    for cd in &companion_defs {
        if !all_simp.contains(cd) {
            all_simp.push(cd.clone());
        }
    }
    let simp_list = all_simp.join(", ");

    let aux_name = format!("{}_aux", inputs.theorem_base);

    let mut support_lines = Vec::new();

    // Build the aux theorem with Nat induction
    // private theorem aux : ∀ (n : Nat) (params...), measure target ≤ n → [when →] prop := by
    support_lines.push(format!(
        "private theorem {} : ∀ (n : Nat) {}, {} {} ≤ n → {} := by",
        aux_name, inputs.quant_params, measure_fn, target_lean, inputs.theorem_prop
    ));

    // intro n
    support_lines.push("  intro n".to_string());

    // induction n with
    support_lines.push("  induction n with".to_string());

    // | zero => intro params h_bound [premises]; cases target <;> simp_all [defs]
    let mut zero_intro = inputs.intro_names.to_vec();
    zero_intro.push("h_bound".to_string());
    zero_intro.extend(premise_names.iter().cloned());
    support_lines.push(format!(
        "  | zero => intro {}; cases {} <;> simp_all [{}]",
        zero_intro.join(" "),
        target_lean,
        simp_list
    ));

    // | succ n ih =>
    support_lines.push("  | succ n ih =>".to_string());

    // intro params h_bound [premises]
    {
        let mut succ_intro = inputs.intro_names.to_vec();
        succ_intro.push("h_bound".to_string());
        succ_intro.extend(premise_names.iter().cloned());
        support_lines.push(format!("    intro {}", succ_intro.join(" ")));
    }

    // cases target with
    support_lines.push(format!("    cases {} with", target_lean));

    // Per-variant cases in succ branch
    for variant in variants {
        let lean_variant = to_lower_first(&variant.name);
        let kind = classify_variant(variant, type_name);

        let field_binders: Vec<String> = (0..variant.fields.len())
            .map(|i| format!("f{}", i))
            .collect();

        match kind {
            VariantKind::Leaf => {
                if field_binders.is_empty() {
                    support_lines.push(format!(
                        "    | {} => simp_all [{}]",
                        lean_variant, simp_list
                    ));
                } else {
                    support_lines.push(format!(
                        "    | {} {} => simp_all [{}]",
                        lean_variant,
                        field_binders.join(" "),
                        simp_list,
                    ));
                }
            }
            VariantKind::DirectRec => {
                support_lines.push(format!(
                    "    | {} {} => simp_all [{}]",
                    lean_variant,
                    field_binders.join(" "),
                    simp_list,
                ));
            }
            VariantKind::IndirectRec => {
                if resolved_variants.contains(&variant.name) {
                    support_lines.push(format!(
                        "    | {} {} => simp_all [{}]",
                        lean_variant,
                        field_binders.join(" "),
                        simp_list,
                    ));
                } else {
                    return None;
                }
            }
        }
    }

    // Now emit the main theorem using aux
    support_lines.push(format!(
        "theorem {} : ∀ {}, {} := by",
        inputs.theorem_base, inputs.quant_params, inputs.theorem_prop
    ));

    // intro params [premises]
    {
        let mut main_intro = inputs.intro_names.to_vec();
        main_intro.extend(premise_names.iter().cloned());
        support_lines.push(format!("  intro {}", main_intro.join(" ")));
    }

    // exact aux (measure target) params... (Nat.le_refl _) [premises]
    {
        let mut exact_args = format!(
            "  exact {} ({} {}) {}",
            aux_name, measure_fn, target_lean, target_lean
        );
        for (i, name) in inputs.intro_names.iter().enumerate() {
            if i != target_idx {
                exact_args = format!("{} {}", exact_args, name);
            }
        }
        exact_args = format!("{} (Nat.le_refl _)", exact_args);
        for premise_name in &premise_names {
            exact_args = format!("{} {}", exact_args, premise_name);
        }
        support_lines.push(exact_args);
    }

    Some(AutoProof {
        support_lines,
        proof_lines: Vec::new(),
        replaces_theorem: true,
    })
}
