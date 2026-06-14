use super::emit_ctx::{EmitCtx, should_borrow_param};
use super::pattern::emit_pattern;
use crate::ast::{Literal, TypeDef};
use crate::codegen::CodegenContext;
use crate::codegen::common::resolve_module_call;
use crate::ir::hir::{
    ResolvedFnDef, ResolvedMatchArm, ResolvedPattern, ResolvedThinBodyPlan,
    classify_list_match_shape_resolved, classify_thin_fn_def_resolved,
    semantic_constructor_from_resolved_ctor,
};
use crate::ir::vars::resolved_pattern_bindings;
use crate::ir::{
    DispatchArmPlan, DispatchBindingPlan, DispatchDefaultPlan, DispatchLiteral, DispatchTableShape,
    ListMatchShape, SemanticConstructor, SemanticDispatchPattern, WrapperKind,
};
use crate::types::{self, Type};
/// Aver expressions → Rust expression strings.
use std::collections::HashSet;

pub use super::syntax::aver_name_to_rust;
pub(super) use super::syntax::{has_list_patterns, has_string_literal_patterns};

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
            "if let Err(__err) = {result_prefix}{i} {{ Err(__err) }} else "
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
            "if let crate::ParallelBranch::Completed(Err(__err)) = {branch_prefix}{i} {{ Err(__err) }} else "
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
        Literal::Float(f) => {
            let s = f.to_string();
            if s.contains('.') || s.contains('e') || s.contains('E') {
                format!("{}f64", s)
            } else {
                format!("{}.0f64", s)
            }
        }
        Literal::Str(s) => format!("AverStr::from({:?})", s),
        Literal::Bool(b) => if *b { "true" } else { "false" }.to_string(),
        Literal::Unit => "()".to_string(),
    }
}

/// Compute borrow mask from a FnDef, checking for TCO.
/// Must mirror actual emitted signature:
/// - Mutual-TCO SCC members → wrapper with borrow-by-default (`&T`)
/// - Self-TCO only (not in SCC) → loop with `mut T`, all-false mask
/// - Non-TCO → borrow-by-default
fn borrow_mask_from_fn_def(
    fd: &crate::ast::FnDef,
    arg_count: usize,
    ctx: &CodegenContext,
) -> Vec<bool> {
    // Mutual-TCO members are emitted as wrappers with borrow-by-default,
    // even if they also have self-TailCalls. Don't skip borrow for them.
    let is_mutual_tco = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .is_some_and(|id| ctx.mutual_tco_members.contains(&id));
    if !is_mutual_tco && super::toplevel::body_has_self_tailcall(&fd.body, &fd.name) {
        return vec![false; arg_count];
    }
    fd.params
        .iter()
        .take(arg_count)
        .map(|(_, type_ann)| {
            let ty = crate::types::parse_type_str(type_ann);
            should_borrow_param(&ty)
        })
        .collect()
}

/// Look up whether the callee's i-th parameter is borrowed (`&T`).
/// Returns a Vec of booleans, one per parameter, indicating borrow status.
/// Uses the resolved-program view for typed param types; falls back to
/// `FnDef` AST annotations when the callee isn't in the view (only TCO
/// status reads from the AST `FnDef`).
/// TCO functions (self or mutual) never use borrow-by-default.
pub(super) fn callee_borrow_mask(name: &str, arg_count: usize, ctx: &CodegenContext) -> Vec<bool> {
    // First, try to find the FnDef to check for TCO (which overrides everything)
    let fd = find_fn_def_by_name(name, ctx);
    let mut mask = if let Some(fd) = fd {
        borrow_mask_from_fn_def(fd, arg_count, ctx)
    } else {
        // No FnDef found — read param types from the resolved-program
        // view. The qualified lookup hits cross-module callsites that
        // arrive as bare dotted names; the bare-name fallback covers
        // entry-scope synthesised wrappers.
        let lookup_name = if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
            crate::visibility::qualified_name(prefix, bare)
        } else {
            name.to_string()
        };
        let resolved = crate::codegen::common::fn_id_for_dotted_name(ctx, &lookup_name)
            .or_else(|| crate::codegen::common::fn_id_for_dotted_name(ctx, name))
            .and_then(|id| ctx.resolved_program.fn_by_id(id));
        match resolved {
            // Self-TCO functions always resolve via `find_fn_def_by_name`
            // above, so this path is safe for borrow-by-default on all
            // non-scalar types including Named.
            Some(rfd) => rfd
                .params
                .iter()
                .take(arg_count)
                .map(|(_, ty)| should_borrow_param(ty))
                .collect(),
            // Unknown function -- don't borrow (conservative)
            None => vec![false; arg_count],
        }
    };

    // own_param graduation: a NON-TCO collection param the `own_param`
    // pass PROVED uniquely owned is emitted owned-by-value (`mut p: T`,
    // see `MirFnEmitPolicy::apply_own_param` / `emit_fn_params_with_owned`),
    // so the CALLER must pass it BY VALUE, not `&T`. Clear the borrow bit
    // at every graduated param position so the call site matches the
    // graduated signature. (Self-/mutual-TCO callees already return an
    // all-`false` / borrow-by-default mask above; the graduation only
    // applies on the non-TCO emit, so overriding here is the single place
    // that keeps call sites and signatures in lockstep.) A missing MIR fn
    // / aliased_slots leaves the mask unchanged (conservative borrow).
    clear_owned_param_borrows(name, &mut mask, ctx);
    mask
}

/// Clear the borrow bit for every own_param-graduated collection param of
/// callee `name`, so the call site passes those args by value (matching
/// the graduated `mut p: T` signature). Resolves the callee's `MirFn` +
/// `ResolvedFnDef` by the SAME identity-keyed lookup the signature emit
/// uses, then applies `from_mir::owned_collection_param_names` (the single
/// source of the owned-param set). Graduation only happens on non-TCO
/// emits; a self-/mutual-TCO callee carries no graduated params here
/// because `apply_own_param` on those paths never populates the non-TCO
/// signature's owned set, so this is a no-op for them.
fn clear_owned_param_borrows(name: &str, mask: &mut [bool], ctx: &CodegenContext) {
    let lookup_name = if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
        crate::visibility::qualified_name(prefix, bare)
    } else {
        name.to_string()
    };
    let fn_id = crate::codegen::common::fn_id_for_dotted_name(ctx, &lookup_name)
        .or_else(|| crate::codegen::common::fn_id_for_dotted_name(ctx, name));
    let Some(fn_id) = fn_id else { return };
    // A self-/mutual-TCO callee never graduates a param on the path that
    // governs its call-site ABI, so skip it (its signature ABI is fixed
    // by the TCO wrapper, not by own_param). This mirrors the `has_tco`
    // guard in `emit_fn_def_with_visibility`.
    if let Some(fd) = find_fn_def_by_name(name, ctx) {
        let is_mutual_tco = crate::codegen::common::fn_id_for_decl(ctx, fd)
            .is_some_and(|id| ctx.mutual_tco_members.contains(&id));
        if is_mutual_tco || super::toplevel::body_has_self_tailcall(&fd.body, &fd.name) {
            return;
        }
    }
    let Some(mir_fn) = ctx.mir_program.as_ref().and_then(|p| p.fn_by_id(fn_id)) else {
        return;
    };
    let Some(rfd) = ctx.resolved_program.fn_by_id(fn_id) else {
        return;
    };
    let owned = super::from_mir::owned_collection_param_names(mir_fn, &rfd.params);
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

/// Find a FnDef by name, checking top-level, modules (qualified), and all modules (unqualified).
///
/// **syntax-discovery-only** (epic #170 invariant): used by
/// `callee_borrow_mask` to read param-type annotations from the AST
/// `FnDef` for borrow-by-default inference. Identity-sensitive call
/// dispatch already happens upstream via `ResolvedCallee::Fn(FnId)`
/// — this lookup only recovers the AST view for downstream
/// annotation reads. A future PR can route through
/// `symbol_table.fn_id_of` + `resolved_program.fn_by_id` once the
/// borrow-mask inference consumes resolved param types directly.
fn find_fn_def_by_name<'a>(name: &str, ctx: &'a CodegenContext) -> Option<&'a crate::ast::FnDef> {
    // Check top-level fn_defs
    if let Some(fd) = ctx.fn_defs.iter().find(|fd| fd.name == name) {
        return Some(fd);
    }

    // Check extra fn_defs (current module being emitted)
    if let Some(fd) = ctx.extra_fn_defs.iter().find(|fd| fd.name == name) {
        return Some(fd);
    }

    // Check module fn_defs for qualified calls
    if let Some((prefix, bare)) = resolve_module_call(name, ctx) {
        for module in &ctx.modules {
            if module.prefix == prefix
                && let Some(fd) = module.fn_defs.iter().find(|fd| fd.name == bare)
            {
                return Some(fd);
            }
        }
    }

    None
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

    let subject_name = "__dispatch_subject";
    let fallback = match &shape.default_arm {
        Some(default_arm) => emit_default_dispatch_arm(
            subject_name,
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
            let cond = emit_dispatch_condition(subject_name, &entry.pattern);
            let body = emit_dispatch_arm_body(subject_name, arm, entry, &body_for_arm);
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
                let binding = aver_name_to_rust(name);
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
            let binding = aver_name_to_rust(name);
            match_arms.push(format!("{binding} => {{ {body} }}"));
        } else {
            match_arms.push(format!("_ => {{ {body} }}"));
        }
    }

    Some(format!("match {} {{ {} }}", subject, match_arms.join(", ")))
}

fn emit_dispatch_condition(subject_name: &str, pattern: &SemanticDispatchPattern) -> String {
    match pattern {
        SemanticDispatchPattern::Literal(lit) => match lit {
            // `AverInt: PartialEq`, so an equality guard works directly;
            // `AverInt` cannot be a `match` pattern literal, hence the
            // dispatch path lowers Int-literal arms to guards.
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
            let extractor = match kind {
                WrapperKind::ResultOk => "Ok",
                WrapperKind::ResultErr => "Err",
                WrapperKind::OptionSome => "Some",
            };
            format!(
                "{{ let {binding} = if let {extractor}({binding}) = &{subject_name} {{ {binding}.clone() }} else {{ unreachable!(\"Aver Rust codegen: dispatch tag mismatch\") }}; {body} }}"
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
            format!("{{ let {} = {}.clone(); {} }}", name, subject_name, body)
        }
    }
}

pub(super) fn emit_list_match<F>(
    subject: String,
    arms: &[ResolvedMatchArm],
    list_shape: Option<ListMatchShape>,
    allow_fast_macro: bool,
    ctx: &CodegenContext,
    body_for_arm: F,
) -> String
where
    F: Fn(&ResolvedMatchArm) -> String,
{
    // Fast path: exactly [] and [h, ..t] → use aver_list_match! macro
    if allow_fast_macro
        && let Some(code) =
            try_emit_list_match_macro(&subject, arms, list_shape, ctx, &body_for_arm)
    {
        return code;
    }
    let subject_name = "__list_subject";
    let arms_code = emit_list_match_arms(subject_name, arms, ctx, &body_for_arm);
    format!("{{ let {} = {}; {} }}", subject_name, subject, arms_code)
}

/// Emit the aver_list_match! macro for the common []/[h,..t] two-arm pattern.
fn try_emit_list_match_macro<F>(
    subject: &str,
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
        emit_list_match_arms(subject_name, rest, ctx, body_for_arm)
    };

    match &first.pattern {
        ResolvedPattern::EmptyList => format!(
            "if {}.is_empty() {{ {} }} else {{ {} }}",
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
            format!(
                "if let Some(({}, {})) = aver_rt::list_uncons_cloned(&{}) {{ {} }} else {{ {} }}",
                head_pat, tail_pat, subject_name, body, fallback
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
                "match &{} {{ {} => {{ {} }}, _ => {{ {} }} }}",
                subject_name, pat, body, fallback
            )
        }
    }
}

fn emit_list_arm_body(arm: &ResolvedMatchArm, ctx: &CodegenContext, body: String) -> String {
    let rebindings = emit_pattern_rebindings(&arm.pattern, ctx);
    if rebindings.is_empty() {
        body
    } else {
        format!("{{ {}{} }}", rebindings, body)
    }
}

/// Recursive-position detection for a record/variant constructor:
/// returns the indices of constructor fields whose declared type is
/// the constructor's owning type itself (the boxed-positions a
/// `Tree.Node(left: Tree, value: Int, right: Tree)` lowers under in
/// the Rust backend via `Arc::new(...)` to keep the type sized).
///
/// #180 Phase 7: routes through `find_ctor_owning_type` (which walks
/// `ctx.symbol_table` + `ctx.items` / `ctx.modules`) instead of the
/// legacy `ctx.fn_sigs` keyed by dotted source name. Identity-safe
/// across cross-module same-bare-name twins via `TypeId` equality.
pub(super) fn constructor_boxed_positions(name: &str, ctx: &CodegenContext) -> HashSet<usize> {
    let mut out = HashSet::new();
    let Some(decl) = find_ctor_owning_type(name, ctx) else {
        return out;
    };
    // Bare-name suffix of the owning type — for a dep-module type like
    // `ast.Expr` the field annotation in the .av source is `Expr`, not
    // `ast.Expr`, so the id-less fallback must compare against the
    // bare suffix as well.
    let owning_full = decl.owning_type_name.as_deref();
    let owning_bare = owning_full.map(|n| n.rsplit_once('.').map(|(_, b)| b).unwrap_or(n));
    for (idx, field_str) in decl.field_type_strs.iter().enumerate() {
        // A field is a recursive (boxed) position iff its declared type is
        // EXACTLY the owning type — a bare nominal reference, not a generic
        // wrapping it (`left: Tree` is boxed, `kids: List<Tree>` is not).
        // Comparing the annotation string directly against the owning type's
        // full + bare names is equivalent to the old parse-then-`named_name`
        // round-trip (a bare nominal annotation IS its name), and
        // `parse_type_str` never resolved a `TypeId` here, so the parse was
        // pure name-extraction.
        let f = field_str.trim();
        if Some(f) == owning_full || Some(f) == owning_bare {
            out.insert(idx);
        }
    }
    out
}

/// Constructor declaration view — the constructor's field types +
/// the canonical identity of the type the constructor belongs to.
/// Built by walking `TypeDef`s in `ctx.items` and dep modules.
struct CtorDecl {
    /// Raw field type annotation strings in declaration order, straight
    /// from the source `TypeDef`. Recursive-field detection (the only
    /// consumer) compares these by name against the owning type, so no
    /// `parse_type_str` round-trip is needed.
    field_type_strs: Vec<String>,
    /// Owning type's canonical name — the identity used for
    /// recursive-field detection (full + bare-suffix forms).
    owning_type_name: Option<String>,
}

fn find_ctor_owning_type(name: &str, ctx: &CodegenContext) -> Option<CtorDecl> {
    use crate::ast::{TopLevel, TypeDef};

    let consider = |type_name: &str, fields: Vec<String>| -> CtorDecl {
        CtorDecl {
            field_type_strs: fields,
            owning_type_name: Some(type_name.to_string()),
        }
    };

    // Constructor name shapes the source may produce:
    //   - `Shape.Circle` (entry sum type)
    //   - `MyModule.Shape.Circle` (module sum type — dotted parent)
    //   - `Box` (entry product / single-ctor type)
    //   - `MyModule.Box` (module product)
    //
    // Walk every typedef across entry + dep modules and match either
    // the bare-name product form or the `parent.variant` sum form.
    let walk_one = |td: &TypeDef, scope: Option<&str>| -> Option<CtorDecl> {
        match td {
            TypeDef::Sum {
                name: parent,
                variants,
                ..
            } => {
                let parent_full = match scope {
                    Some(prefix) => format!("{prefix}.{parent}"),
                    None => parent.clone(),
                };
                for v in variants {
                    let bare_form = format!("{parent}.{}", v.name);
                    let full_form = format!("{parent_full}.{}", v.name);
                    if name == bare_form || name == full_form {
                        return Some(consider(&parent_full, v.fields.clone()));
                    }
                }
                None
            }
            TypeDef::Product {
                name: parent,
                fields,
                ..
            } => {
                let parent_full = match scope {
                    Some(prefix) => format!("{prefix}.{parent}"),
                    None => parent.clone(),
                };
                if name == parent || name == parent_full {
                    let fts = fields.iter().map(|(_, t)| t.clone()).collect();
                    return Some(consider(&parent_full, fts));
                }
                None
            }
        }
    };

    for item in &ctx.items {
        if let TopLevel::TypeDef(td) = item
            && let Some(decl) = walk_one(td, None)
        {
            return Some(decl);
        }
    }
    for m in &ctx.modules {
        for td in &m.type_defs {
            if let Some(decl) = walk_one(td, Some(&m.prefix)) {
                return Some(decl);
            }
        }
    }
    None
}

pub(super) fn constructor_boxed_bindings(
    name: &str,
    bindings: &[String],
    ctx: &CodegenContext,
) -> Vec<String> {
    // `constructor_boxed_positions` walks `TypeDef`s directly post-#180
    // Phase 7, so the dotted-vs-bare ambiguity is resolved by the type
    // walker rather than by canonicalising the constructor name here.
    let boxed = constructor_boxed_positions(name, ctx);
    bindings
        .iter()
        .enumerate()
        .filter_map(|(idx, b)| {
            if b != "_" && boxed.contains(&idx) {
                Some(b.clone())
            } else {
                None
            }
        })
        .collect()
}

/// When matching on a reference (`&T`), pattern bindings are `&Inner`.
/// Emit `let b = b.clone();` for each binding to produce owned values.
pub(super) fn emit_ref_match_rebindings(pattern: &ResolvedPattern) -> String {
    let bindings = resolved_pattern_bindings(pattern);
    if bindings.is_empty() {
        return String::new();
    }
    let mut lines = Vec::new();
    for b in &bindings {
        let rb = super::syntax::aver_name_to_rust(b);
        lines.push(format!("let {} = {}.clone();", rb, rb));
    }
    format!("{}\n            ", lines.join("\n            "))
}

pub(super) fn emit_pattern_rebindings(pattern: &ResolvedPattern, ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    if let ResolvedPattern::Ctor(ctor, bindings) = pattern {
        // Constructor bindings: only Box-wrapped fields (recursive types) need deref.
        // Look up the canonical "Type.Variant" name from the resolved ctor and feed it
        // to constructor_boxed_bindings, which queries fn_sigs.
        let semantic = semantic_constructor_from_resolved_ctor(ctor, &ctx.symbol_table);
        let ctor_name = match semantic {
            SemanticConstructor::TypeConstructor {
                qualified_type_name,
                variant_name,
            } => format!("{}.{}", qualified_type_name, variant_name),
            SemanticConstructor::Wrapper(_) | SemanticConstructor::NoneValue => {
                // Wrapper / None ctors never carry Box-wrapped recursive fields.
                String::new()
            }
            SemanticConstructor::Unknown(name) => name,
        };
        if !ctor_name.is_empty() {
            for b in constructor_boxed_bindings(&ctor_name, bindings, ctx) {
                let b = aver_name_to_rust(&b);
                lines.push(format!("let {} = (*{}).clone();", b, b));
            }
        }
    }
    if lines.is_empty() {
        String::new()
    } else {
        format!("{}\n            ", lines.join("\n            "))
    }
}
