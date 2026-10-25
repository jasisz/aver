use std::collections::HashSet;

use super::emit_ctx::{EmitCtx, should_borrow_param};
use super::pattern::emit_pattern;
/// Aver expressions → Rust expression strings.
pub use super::syntax::aver_name_to_rust;
pub(super) use super::syntax::{generated_ident, has_list_patterns, has_string_literal_patterns};
use crate::ast::{Literal, TypeDef};
use crate::codegen::CodegenContext;
use crate::codegen::common::resolve_module_call;
use crate::ir::hir::{
    ResolvedFnDef, ResolvedMatchArm, ResolvedPattern, ResolvedThinBodyPlan,
    classify_list_match_shape_resolved, classify_thin_fn_def_resolved,
};
use crate::ir::{
    DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan, DispatchLiteral, DispatchTableShape,
    ListMatchShape, SemanticDispatchPattern, WrapperKind,
};
use crate::types::{self, Type};

/// Render an identifier in a Rust pattern as an explicit binding.
///
/// A bare identifier in any pattern is also eligible for item resolution.
/// `name @ _` makes the binding intent explicit and prevents an in-scope Rust
/// item from changing how the pattern is resolved, while keeping all body
/// references under the original name.
pub(super) fn explicit_binding_pattern(name: &str) -> String {
    format!("{} @ _", aver_name_to_rust(name))
}

/// Render a function parameter as an explicit Rust binding pattern.
///
/// Parameters occupy pattern position just like `let` and match binders. Keep
/// the source name available to the body while making item resolution
/// impossible at the binding site (#1162).
pub(super) fn explicit_parameter_pattern(name: &str, mutable: bool) -> String {
    let binding = explicit_binding_pattern(name);
    if mutable {
        format!("mut {binding}")
    } else {
        binding
    }
}

/// Predicate adapter shared by every resolved-shape classifier — the
/// shared classifiers don't know about [`CodegenContext`] (it lives in
/// the codegen crate, classifiers live in the IR), so they take a
/// `Fn(&str) -> bool` for user-type recognition.
fn is_user_type_fn(ctx: &CodegenContext) -> impl Fn(&str) -> bool + '_ {
    move |name: &str| crate::codegen::common::is_user_type(name, ctx)
}

pub(super) fn classify_thin_fn_def_for_rust<'a>(
    fd: &'a ResolvedFnDef,
    ctx: &'a CodegenContext,
    _ectx: &'a EmitCtx,
) -> Option<ResolvedThinBodyPlan<'a>> {
    classify_thin_fn_def_resolved(fd, &is_user_type_fn(ctx))
}

pub(super) fn emit_tuple_from_vars(prefix: &str, count: usize) -> String {
    match count {
        0 => "()".to_string(),
        1 => format!("({prefix}0,)"),
        _ => format!(
            "({})",
            (0..count)
                .map(|i| format!("{prefix}{i}"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

pub(super) fn emit_result_tuple_unwrap(
    result_prefix: &str,
    value_prefix: &str,
    count: usize,
) -> String {
    let error = generated_ident("err");
    let matched_results = emit_tuple_from_vars(result_prefix, count);
    let ok_pattern = match count {
        0 => "()".to_string(),
        1 => format!("(Ok({value_prefix}0),)"),
        _ => format!(
            "({})",
            (0..count)
                .map(|i| format!("Ok({value_prefix}{i})"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    };
    let ok_tuple = emit_tuple_from_vars(value_prefix, count);

    let mut out = format!(
        "match {} {{ {} => Ok({}), {} => {{ ",
        matched_results, ok_pattern, ok_tuple, matched_results
    );
    for i in 0..count {
        out.push_str(&format!(
            "if let Err({error}) = {result_prefix}{i} {{ Err({error}) }} else "
        ));
    }
    out.push_str("{ unreachable!(\"independent product unwrap requires Result branches\") } } }");
    out
}

pub(super) fn emit_parallel_result_tuple_unwrap(
    branch_prefix: &str,
    result_prefix: &str,
    value_prefix: &str,
    count: usize,
) -> String {
    let error = generated_ident("err");
    let matched_branches = emit_tuple_from_vars(branch_prefix, count);
    let completed_pattern = match count {
        0 => "()".to_string(),
        1 => format!("(crate::ParallelBranch::Completed({result_prefix}0),)"),
        _ => format!(
            "({})",
            (0..count)
                .map(|i| format!("crate::ParallelBranch::Completed({result_prefix}{i})"))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    };

    let mut out = format!(
        "match {} {{ {} => {}, {} => {{ ",
        matched_branches,
        completed_pattern,
        emit_result_tuple_unwrap(result_prefix, value_prefix, count),
        matched_branches
    );
    for i in 0..count {
        out.push_str(&format!(
            "if let crate::ParallelBranch::Completed(Err({error})) = {branch_prefix}{i} {{ Err({error}) }} else "
        ));
    }
    out.push_str("{ panic!(\"independent product branch cancelled by sibling branch\") } } }");
    out
}

pub(super) fn emit_literal(lit: &Literal) -> String {
    match lit {
        // `from_i64` is a `const fn`, so the result is usable in const
        // position. The value is already signed, so a negative literal
        // emits `from_i64(-N)` directly (no `(-…)` wrapper — `AverInt`
        // has no std `Neg`).
        Literal::Int(i) => format!("aver_rt::AverInt::from_i64({})", i),
        // A `>i64` literal: parse the decimal digits into an `AverInt` at runtime
        // (the same `FromStr` path `Int.n` uses). Not const-foldable like the
        // `from_i64` path, but a big literal never appears in a const context.
        // The lexer validated the digits, so `unwrap` cannot fire.
        Literal::BigInt(s) => format!("{:?}.parse::<aver_rt::AverInt>().unwrap()", s),
        // A folded constant can be NaN or infinite, which have no literal.
        Literal::Float(f) if f.is_nan() => "f64::NAN".to_string(),
        Literal::Float(f) if f.is_infinite() => if *f > 0.0 {
            "f64::INFINITY"
        } else {
            "f64::NEG_INFINITY"
        }
        .to_string(),
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') || s.contains('e') || s.contains('E') {
                format!("{}f64", s)
            } else {
                format!("{}.0f64", s)
            }
        }
        // A static node: no allocation each time the literal runs.
        Literal::Str(s) => format!("::aver_rt::str_lit!({:?})", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

/// Look up whether the callee's i-th parameter is borrowed (`&T`).
/// Returns a Vec of booleans, one per parameter, indicating borrow status.
/// Uses the resolved-program view for identity, typed parameters, and TCO
/// status. A missing resolved declaration is conservative (pass by value).
pub(super) fn callee_borrow_mask(name: &str, arg_count: usize, ctx: &CodegenContext) -> Vec<bool> {
    let Some((fn_id, resolved)) = resolved_callee(name, ctx) else {
        return vec![false; arg_count];
    };
    let mut mask = param_borrow_mask(fn_id, resolved, ctx);
    mask.resize(arg_count, false);
    mask
}

/// Which params of the callee take `Option<&T>` (see `lent_option`); each of
/// them is borrowed in [`callee_borrow_mask`] too.
pub(super) fn callee_lent_option_mask(
    name: &str,
    arg_count: usize,
    ctx: &CodegenContext,
) -> Vec<bool> {
    let fn_id = resolved_callee(name, ctx).map(|(fn_id, _)| fn_id);
    (0..arg_count)
        .map(|i| fn_id.is_some_and(|id| super::lent_option::is_lent(&ctx.rust_lent_options, id, i)))
        .collect()
}

/// Which params of `fn_id` its Rust signature borrows (`&T`).
pub(super) fn param_borrow_mask(
    fn_id: crate::ir::FnId,
    resolved: &crate::ir::hir::ResolvedFnDef,
    ctx: &CodegenContext,
) -> Vec<bool> {
    // Mutual-TCO members expose borrow-by-default wrappers. A lone self-TCO
    // function borrows only the params its loop lends.
    let is_mutual_tco = ctx.mutual_tco_members.contains(&fn_id);
    if !is_mutual_tco && super::toplevel::resolved_fn_has_self_tailcall(resolved) {
        return (0..resolved.params.len())
            .map(|i| super::from_mir::self_loop_lends(ctx, fn_id, i))
            .collect();
    }
    let mut mask: Vec<bool> = resolved
        .params
        .iter()
        .enumerate()
        .map(|(i, (_, ty))| {
            should_borrow_param(ty) && !super::from_mir::mutual_param_by_value(ctx, fn_id, i)
        })
        .collect();
    // own_param graduation: a NON-TCO collection param the `own_param`
    // pass proved can use the owned Rust ABI is emitted by value (`mut p: T`,
    // see `MirFnEmitPolicy::apply_own_param` / `emit_fn_params_with_owned`),
    // so the CALLER must pass it BY VALUE, not `&T`. Clear the borrow bit
    // at every graduated param position so the call site matches the
    // graduated signature. A missing MIR fn / aliased_slots leaves the mask
    // unchanged (conservative borrow).
    clear_owned_param_borrows_of(fn_id, resolved, &mut mask, ctx);
    if !is_mutual_tco
        && let Some(program) = ctx.mir_program.as_ref()
        && let Some(mir_fn) = program.fn_by_id(fn_id)
    {
        let lent = super::from_mir::lent_string_params(mir_fn, &resolved.params, &program.builtins);
        for (borrowed, (name, _)) in mask.iter_mut().zip(&resolved.params) {
            *borrowed |= lent.contains(&aver_name_to_rust(name));
        }
    }
    mask
}

/// Clear the borrow bit for every own_param-graduated collection param of
/// the callee, so the call site passes those args by value (matching the
/// graduated `mut p: T` signature). Applies
/// `from_mir::owned_collection_param_names` (the single source of the
/// owned-param set). Graduation only happens on non-TCO emits, so this is a
/// no-op for a self-/mutual-TCO callee. The first-class fn adapter reads the
/// same set, so a function value and a direct call pass every param the same
/// way.
pub(super) fn clear_owned_param_borrows_of(
    fn_id: crate::ir::FnId,
    rfd: &crate::ir::hir::ResolvedFnDef,
    mask: &mut [bool],
    ctx: &CodegenContext,
) {
    // A self-/mutual-TCO callee never graduates a param on the path that
    // governs its call-site ABI, so skip it (its signature ABI is fixed
    // by the TCO wrapper, not by own_param). This mirrors the `has_tco`
    // guard in `emit_fn_def_with_visibility`.
    if ctx.mutual_tco_members.contains(&fn_id)
        || super::toplevel::resolved_fn_has_self_tailcall(rfd)
    {
        return;
    }
    let Some(mir_fn) = ctx.mir_program.as_ref().and_then(|p| p.fn_by_id(fn_id)) else {
        return;
    };
    let owned = super::from_mir::owned_collection_param_names(mir_fn, &rfd.params, ctx);
    if owned.is_empty() {
        return;
    }
    for (i, (pname, _)) in rfd.params.iter().enumerate() {
        if i >= mask.len() {
            break;
        }
        if owned.contains(&aver_name_to_rust(pname)) {
            mask[i] = false;
        }
    }
}

fn resolved_callee<'a>(
    name: &str,
    ctx: &'a CodegenContext,
) -> Option<(crate::ir::FnId, &'a crate::ir::hir::ResolvedFnDef)> {
    let lookup_name = if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
        crate::visibility::qualified_name(prefix, bare)
    } else {
        name.to_string()
    };
    let fn_id = crate::codegen::common::fn_id_for_dotted_name(ctx, &lookup_name)
        .or_else(|| crate::codegen::common::fn_id_for_dotted_name(ctx, name))?;
    Some((fn_id, ctx.resolved_program.fn_by_id(fn_id)?))
}

/// Given the record's `Type::Named` and a `field` name, look the field's
/// declared type up in the context's record definitions and report
/// whether it is a Copy type. Returns `false` when the record / field
/// can't be resolved (the conservative "needs a clone" answer). The
/// MIR walker calls this directly with the base local's stamped type,
/// so the elision decision matches HIR byte-for-byte.
pub(super) fn record_field_is_copy(named_ty: &Type, field: &str, ctx: &CodegenContext) -> bool {
    let Some(record_key) = crate::codegen::common::backend_named_type_key(ctx, named_ty) else {
        return false;
    };
    // Find record definition and look up field type. Iterate
    // through `(module_prefix, td)` pairs so canonical-key
    // matching (`"A.Shape"`) routes to the right module's
    // TypeDef list instead of first-bare-match-wins.
    let candidates = ctx
        .type_defs
        .iter()
        .map(|td| (None, td))
        .chain(ctx.modules.iter().flat_map(|m| {
            m.type_defs
                .iter()
                .map(move |td| (Some(m.prefix.as_str()), td))
        }));
    for (scope, td) in candidates {
        if let TypeDef::Product { name, fields, .. } = td {
            let canonical = match scope {
                Some(prefix) => format!("{}.{}", prefix, name),
                None => name.clone(),
            };
            if canonical == record_key
                && let Some((_, type_ann)) = fields.iter().find(|(n, _)| n == field)
            {
                let ty = types::parse_type_str(type_ann);
                return super::emit_ctx::is_copy_type(&ty);
            }
        }
    }
    false
}

pub(super) fn emit_dispatch_table_match<F>(
    subject: String,
    arms: &[ResolvedMatchArm],
    shape: &DispatchTableShape,
    subject_is_bare: bool,
    body_for_arm: F,
) -> String
where
    F: Fn(&ResolvedMatchArm) -> String,
{
    // Fast path: if all entries are wrapper tags (Result/Option), emit a Rust `match`
    // with move bindings instead of if-chain + clone.  This is both shorter and faster
    // because it avoids cloning the inner value.
    if let Some(code) = try_emit_wrapper_match(&subject, arms, shape, &body_for_arm) {
        return code;
    }

    let subject_name = generated_ident("dispatch_subject");
    let fallback = match &shape.default_arm {
        Some(default_arm) => emit_default_dispatch_arm(
            &subject_name,
            &arms[default_arm.arm_index],
            default_arm,
            &body_for_arm,
        ),
        None => "panic!(\"Aver Rust codegen: non-exhaustive dispatch match\")".to_string(),
    };

    let body = shape
        .entries
        .iter()
        .rev()
        .fold(fallback, |else_branch, entry| {
            let arm = &arms[entry.arm_index];
            let cond = emit_dispatch_condition(&subject_name, &entry.pattern, subject_is_bare);
            let body = emit_dispatch_arm_body(&subject_name, arm, entry, &body_for_arm);
            format!("if {} {{ {} }} else {{ {} }}", cond, body, else_branch)
        });

    format!("{{ let {} = {}; {} }}", subject_name, subject, body)
}

/// Emit `match subject { Ok(v) => ..., Err(e) => ... }` when ALL dispatch entries are
/// wrapper tags (Result.Ok/Err, Option.Some/None).  Bindings are by move (no clone).
fn try_emit_wrapper_match<F>(
    subject: &str,
    arms: &[ResolvedMatchArm],
    shape: &DispatchTableShape,
    body_for_arm: &F,
) -> Option<String>
where
    F: Fn(&ResolvedMatchArm) -> String,
{
    // All entries must be wrapper tags with payload bindings
    let all_wrappers = shape.entries.iter().all(|e| {
        matches!(
            e.pattern,
            SemanticDispatchPattern::WrapperTag(_) | SemanticDispatchPattern::NoneValue
        )
    });
    if !all_wrappers {
        return None;
    }

    let mut match_arms = Vec::new();
    for entry in &shape.entries {
        let arm = &arms[entry.arm_index];
        let body = body_for_arm(arm);
        match (&entry.pattern, &entry.binding) {
            (
                SemanticDispatchPattern::WrapperTag(kind),
                DispatchBindingPlan::WrapperPayload(name),
            ) => {
                let binding = explicit_binding_pattern(name);
                let extractor = match kind {
                    WrapperKind::ResultOk => "Ok",
                    WrapperKind::ResultErr => "Err",
                    WrapperKind::OptionSome => "Some",
                };
                match_arms.push(format!("{extractor}({binding}) => {{ {body} }}"));
            }
            (SemanticDispatchPattern::WrapperTag(kind), DispatchBindingPlan::None) => {
                let pattern = match kind {
                    WrapperKind::ResultOk => "Ok(_)",
                    WrapperKind::ResultErr => "Err(_)",
                    WrapperKind::OptionSome => "Some(_)",
                };
                match_arms.push(format!("{pattern} => {{ {body} }}"));
            }
            (SemanticDispatchPattern::NoneValue, _) => {
                match_arms.push(format!("None => {{ {body} }}"));
            }
            _ => return None,
        }
    }

    // Default arm (wildcard)
    if let Some(default_arm) = &shape.default_arm {
        let arm = &arms[default_arm.arm_index];
        let body = body_for_arm(arm);
        if let Some(name) = &default_arm.binding_name {
            let binding = explicit_binding_pattern(name);
            match_arms.push(format!("{binding} => {{ {body} }}"));
        } else {
            match_arms.push(format!("_ => {{ {body} }}"));
        }
    }

    Some(format!("match {} {{ {} }}", subject, match_arms.join(", ")))
}

fn emit_dispatch_condition(
    subject_name: &str,
    pattern: &SemanticDispatchPattern,
    subject_is_bare: bool,
) -> String {
    match pattern {
        SemanticDispatchPattern::Literal(lit) => match lit {
            // `AverInt: PartialEq`, so an equality guard works directly;
            // `AverInt` cannot be a `match` pattern literal, hence the
            // dispatch path lowers Int-literal arms to guards. A bare-`i64`
            // subject (a proven-bare counter, `Copy`) compares against a raw
            // `{K}i64` literal — comparing it against `AverInt::from_i64(K)`
            // would be `i64 == AverInt` (`rustc` E0308). This mirrors the
            // single-arm `try_emit_int_literal_match` `subject_is_bare` path
            // so the ≥2-literal-arm dispatch table stays in the fast loop.
            DispatchLiteral::Int(i) if subject_is_bare => {
                format!("{subject_name} == {i}i64")
            }
            // A literal past `AverInt`'s inline range is never built.
            DispatchLiteral::Int(i) if !super::from_mir::AVERINT_INLINE.contains(i) => {
                format!("{subject_name}.cmp_i64({i}i64).is_eq()")
            }
            DispatchLiteral::Int(i) => {
                format!("{subject_name} == aver_rt::AverInt::from_i64({i})")
            }
            DispatchLiteral::Float(f) => format!("{subject_name} == {f}f64"),
            DispatchLiteral::Bool(b) => format!("{subject_name} == {b}"),
            DispatchLiteral::Str(s) => format!("&*{subject_name} == {:?}", s),
            DispatchLiteral::Unit => format!("{subject_name} == ()"),
        },
        SemanticDispatchPattern::EmptyList => format!("{subject_name}.is_empty()"),
        SemanticDispatchPattern::NoneValue => format!("{subject_name}.is_none()"),
        SemanticDispatchPattern::WrapperTag(kind) => match kind {
            WrapperKind::ResultOk => format!("{subject_name}.is_ok()"),
            WrapperKind::ResultErr => format!("{subject_name}.is_err()"),
            WrapperKind::OptionSome => format!("{subject_name}.is_some()"),
        },
    }
}

fn emit_dispatch_arm_body(
    subject_name: &str,
    arm: &ResolvedMatchArm,
    entry: &DispatchArmPlan,
    body_for_arm: &impl Fn(&ResolvedMatchArm) -> String,
) -> String {
    let body = body_for_arm(arm);
    match (&entry.pattern, &entry.binding) {
        (
            SemanticDispatchPattern::WrapperTag(kind),
            DispatchBindingPlan::WrapperPayload(binding_name),
        ) => {
            let binding = aver_name_to_rust(binding_name);
            let binding_pattern = explicit_binding_pattern(binding_name);
            let extractor = match kind {
                WrapperKind::ResultOk => "Ok",
                WrapperKind::ResultErr => "Err",
                WrapperKind::OptionSome => "Some",
            };
            format!(
                "{{ let {binding_pattern} = if let {extractor}({binding_pattern}) = &{subject_name} {{ {binding}.clone() }} else {{ unreachable!(\"Aver Rust codegen: dispatch tag mismatch\") }}; {body} }}"
            )
        }
        _ => body,
    }
}

fn emit_default_dispatch_arm(
    subject_name: &str,
    arm: &ResolvedMatchArm,
    default_arm: &DispatchDefaultPlan,
    body_for_arm: &impl Fn(&ResolvedMatchArm) -> String,
) -> String {
    let body = body_for_arm(arm);
    match &default_arm.binding_name {
        None => body,
        Some(name) => {
            let name = aver_name_to_rust(name);
            format!(
                "{{ let {} @ _ = {}.clone(); {} }}",
                name, subject_name, body
            )
        }
    }
}

/// `lent`: `subject` is a `&AverList`, matched through the borrow (the macro
/// takes its list by value). `reuse`: the cons arm builds a list in the node
/// it takes apart (`aver_list_match_reuse!`). `heads`: the heads an arm
/// borrows from the list instead of cloning them out.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_list_match<F>(
    subject: String,
    lent: bool,
    reuse: bool,
    heads: &HashSet<String>,
    arms: &[ResolvedMatchArm],
    list_shape: Option<ListMatchShape>,
    ctx: &CodegenContext,
    body_for_arm: F,
) -> String
where
    F: Fn(&ResolvedMatchArm) -> String,
{
    // Fast path: exactly [] and [h, ..t] → use aver_list_match! macro, which
    // owns the list and takes it apart. A head an arm borrows stays on the
    // path below: a shared list lends it rather than copying it out.
    if !lent
        && heads.is_empty()
        && let Some(code) =
            try_emit_list_match_macro(&subject, reuse, arms, list_shape, ctx, &body_for_arm)
    {
        return code;
    }
    let subject_name = generated_ident("list_subject");
    let subject_ref = match lent {
        true => subject_name.clone(),
        false => format!("&{subject_name}"),
    };
    let arms_code =
        emit_list_match_arms(&subject_name, &subject_ref, heads, arms, ctx, &body_for_arm);
    format!("{{ let {} = {}; {} }}", subject_name, subject, arms_code)
}

/// Emit the aver_list_match! macro for the common []/[h,..t] two-arm pattern.
fn try_emit_list_match_macro<F>(
    subject: &str,
    reuse: bool,
    arms: &[ResolvedMatchArm],
    list_shape: Option<ListMatchShape>,
    ctx: &CodegenContext,
    body_for_arm: &F,
) -> Option<String>
where
    F: Fn(&ResolvedMatchArm) -> String,
{
    let shape = match list_shape {
        Some(shape) => shape,
        None => classify_list_match_shape_resolved(arms)?,
    };
    let empty_arm = &arms[shape.empty_arm_index];
    let cons_arm = &arms[shape.cons_arm_index];
    let ResolvedPattern::Cons(head, tail) = &cons_arm.pattern else {
        return None;
    };
    // Both names must be real idents (not _) for the macro binding
    if head == "_" || tail == "_" {
        return None;
    }
    let head_name = aver_name_to_rust(head);
    let tail_name = aver_name_to_rust(tail);
    let empty_body = emit_list_arm_body(empty_arm, ctx, body_for_arm(empty_arm));
    let cons_body = emit_list_arm_body(cons_arm, ctx, body_for_arm(cons_arm));
    // Wrap arm bodies in braces only when they contain statements (return, let, etc.)
    // to avoid Rust's "unnecessary braces" warning on simple expressions.
    let wrap = |body: &str| -> String {
        if body.contains("return ") || body.contains("let ") || body.contains(';') {
            format!("{{ {} }}", body)
        } else {
            body.to_string()
        }
    };
    if reuse {
        return Some(format!(
            "::aver_rt::aver_list_match_reuse!({subject}, [] => {}, [{head_name}, {tail_name}, {}] => {})",
            wrap(&empty_body),
            generated_ident("list_slot"),
            wrap(&cons_body)
        ));
    }
    Some(format!(
        "aver_list_match!({}, [] => {}, [{}, {}] => {})",
        subject,
        wrap(&empty_body),
        head_name,
        tail_name,
        wrap(&cons_body)
    ))
}

fn emit_list_match_arms<F>(
    subject_name: &str,
    subject_ref: &str,
    heads: &HashSet<String>,
    arms: &[ResolvedMatchArm],
    ctx: &CodegenContext,
    body_for_arm: &F,
) -> String
where
    F: Fn(&ResolvedMatchArm) -> String,
{
    let Some((first, rest)) = arms.split_first() else {
        return "panic!(\"Aver Rust codegen: empty list match\")".to_string();
    };

    let body = emit_list_arm_body(first, ctx, body_for_arm(first));
    let fallback = if rest.is_empty() {
        "panic!(\"Aver Rust codegen: non-exhaustive list match\")".to_string()
    } else {
        emit_list_match_arms(subject_name, subject_ref, heads, rest, ctx, body_for_arm)
    };

    match &first.pattern {
        ResolvedPattern::EmptyList => format!(
            "if {}.is_empty() {{ {} }} else {{ {} }}",
            subject_name, body, fallback
        ),
        // `[_, .._]` only asks whether the list is non-empty, so it needs no
        // uncons. It also must not have one: an arm without bindings lets the
        // match borrow a borrowed-parameter subject (`__list_subject` is then
        // a `&AverList`), and `list_uncons_cloned(&__list_subject)` would ask
        // `AverListMatch` of the reference (#1450). `is_empty` auto-derefs.
        ResolvedPattern::Cons(head, tail) if head == "_" && tail == "_" => format!(
            "if !{}.is_empty() {{ {} }} else {{ {} }}",
            subject_name, body, fallback
        ),
        ResolvedPattern::Cons(head, tail) => {
            let head_pat = if head == "_" {
                "_".to_string()
            } else {
                aver_name_to_rust(head)
            };
            let tail_pat = if tail == "_" {
                "_".to_string()
            } else {
                aver_name_to_rust(tail)
            };
            if heads.contains(head) {
                // The head is borrowed from the list; only the rest is a
                // new list value.
                return match tail == "_" {
                    true => format!(
                        "if let Some({head_pat}) = {subject_name}.first() {{ {body} }} else {{ {fallback} }}"
                    ),
                    false => format!(
                        "if let (Some({head_pat}), Some({tail_pat})) = ({subject_name}.first(), {subject_name}.tail()) {{ {body} }} else {{ {fallback} }}"
                    ),
                };
            }
            format!(
                "if let Some(({}, {})) = aver_rt::list_uncons_cloned({}) {{ {} }} else {{ {} }}",
                head_pat, tail_pat, subject_ref, body, fallback
            )
        }
        ResolvedPattern::Wildcard => body,
        ResolvedPattern::Ident(name) => {
            let name = aver_name_to_rust(name);
            format!("{{ let {} = {}.clone(); {} }}", name, subject_name, body)
        }
        other => {
            let pat = emit_pattern(other, false, ctx);
            format!(
                "match {} {{ {} => {{ {} }}, _ => {{ {} }} }}",
                subject_ref, pat, body, fallback
            )
        }
    }
}

fn emit_list_arm_body(arm: &ResolvedMatchArm, ctx: &CodegenContext, body: String) -> String {
    let rebindings = emit_pattern_rebindings(&arm.pattern, ctx, false, &HashSet::new());
    if rebindings.is_empty() {
        body
    } else {
        format!("{{ {}{} }}", rebindings, body)
    }
}

/// A constructor of a shared sum binds the one `Shared` that holds its fields
/// (see `pattern::shared_fields_binder`), so an arm that uses a field needs
/// it out of there. Emit `let b = __b_fields.1.clone();` for each such field,
/// wherever in the pattern its constructor stands: at the top or inside a
/// tuple, at any depth. A match on a reference (`by_ref`) binds `&Inner`, so
/// every other binder is cloned too: `let b = b.clone();`.
pub(super) fn emit_pattern_rebindings(
    pattern: &ResolvedPattern,
    ctx: &CodegenContext,
    by_ref: bool,
    lent: &HashSet<String>,
) -> String {
    let mut lines = Vec::new();
    collect_pattern_rebindings(pattern, ctx, by_ref, lent, &mut lines);
    if lines.is_empty() {
        String::new()
    } else {
        format!("{}\n            ", lines.join("\n            "))
    }
}

/// One walk over every pattern form that holds another pattern. The fields
/// of a constructor and the parts of a list pattern are binder names here
/// (nested ones are compiled into nested matches before any backend runs),
/// so a tuple is the only form to descend into.
fn collect_pattern_rebindings(
    pattern: &ResolvedPattern,
    ctx: &CodegenContext,
    by_ref: bool,
    lent: &HashSet<String>,
    lines: &mut Vec<String>,
) {
    match pattern {
        ResolvedPattern::Tuple(items) => {
            for item in items {
                collect_pattern_rebindings(item, ctx, by_ref, lent, lines);
            }
        }
        ResolvedPattern::Ctor(ctor, bindings) => {
            let fields = super::pattern::shared_fields_binder(ctor, bindings, ctx);
            for (idx, b) in bindings.iter().enumerate().filter(|(_, b)| *b != "_") {
                let borrowed = lent.contains(b);
                let b = aver_name_to_rust(b);
                match &fields {
                    Some(fields) if borrowed => lines.push(format!("let {b} = &{fields}.{idx};")),
                    Some(fields) => lines.push(format!("let {b} = {fields}.{idx}.clone();")),
                    None if by_ref && !borrowed => lines.push(format!("let {b} = {b}.clone();")),
                    None => {}
                }
            }
        }
        ResolvedPattern::Ident(name) if by_ref => {
            let name = aver_name_to_rust(name);
            lines.push(format!("let {name} = {name}.clone();"));
        }
        ResolvedPattern::Wildcard
        | ResolvedPattern::Literal(_)
        | ResolvedPattern::Ident(_)
        | ResolvedPattern::EmptyList
        | ResolvedPattern::Cons(_, _) => {}
    }
}
