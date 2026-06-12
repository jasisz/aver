/// Top-level Aver items → Lean 4 items (defs, inductives, structures, examples).
use std::collections::{HashMap, HashSet};

use super::expr::{aver_name_to_lean, emit_expr_legacy};
use super::law_auto::{emit_verify_law_forall_auto_proof, emit_verify_law_support_theorems};
use super::recurrence::{
    detect_second_order_int_linear_recurrence, recurrence_nat_helper_name, render_affine_pair_expr,
};
use super::shared::to_lower_first;
use super::types::type_annotation_to_lean;
use super::{VerifyEmitMode, sizeof_measure_param_indices};
use crate::ast::*;
use crate::codegen::CodegenContext;
use crate::codegen::recursion::{native_aux_name, rewrite_recursive_calls_body};
use crate::verify_law::canonical_spec_ref;

/// Emit a Lean 4 type definition from an Aver TypeDef.
/// Subtype helper-type names that the oracle_subtypes module emits at
/// the top of every artifact. Returned name matches the
/// `<...>InBounds` / `<...>InUnit` / `<...>Nonneg` declarations in
/// `oracle_subtypes::lean_subtypes` — keep these in sync.
fn bounded_oracle_subtype_for(method: &str) -> Option<&'static str> {
    match method {
        "Random.int" => Some("RandomIntInBounds"),
        "Random.float" => Some("RandomFloatInUnit"),
        "Time.unixMs" => Some("TimeUnixMsNonneg"),
        _ => None,
    }
}

/// Render a per-sample instantiated `when` guard with every Int literal
/// ascribed (`(4 : Int)`).
///
/// The guard is the parser's SUBSTITUTED premise: numeral literals stand
/// where Int givens stood. A bare Lean numeral in a comparison elaborates
/// as `Nat`, and with subtraction in the premise truncated `Nat`
/// subtraction changes the proposition — e.g. the probe's
/// `((((1 * 1) - 4) * ((1 * 1) - 4)) <= 4)` is TRUE over `Nat`
/// (`1 - 4 = 0`) but FALSE over `Int` (`(-3) * (-3) = 9`), so the emitted
/// `_sample_N` / `_checked_domain` theorem was FALSE AS STATED and
/// `native_decide` failed the build on a law the VM verifies. Ascribing
/// pins every literal to `Int` — the type the substituted given had.
///
/// Recurses through the operator shapes a premise is built from
/// (comparisons, arithmetic, `&&`-conjunction of multiple `when`s,
/// negation); anything else (fn calls, idents, record literals) falls
/// back to `emit_expr`, where Lean already types the positions from
/// signatures.
pub(super) fn emit_sample_guard(guard: &Spanned<Expr>, ctx: &CodegenContext) -> String {
    let active = ctx.active_module_scope();
    let resolved = ctx.resolve_expr(guard, active.as_deref());
    emit_sample_guard_resolved(&resolved, ctx)
}

fn emit_sample_guard_resolved(
    expr: &Spanned<crate::ir::hir::ResolvedExpr>,
    ctx: &CodegenContext,
) -> String {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match &expr.node {
        ResolvedExpr::Literal(Literal::Int(i)) => format!("({} : Int)", i),
        ResolvedExpr::Neg(inner) => format!("(-{})", emit_sample_guard_resolved(inner, ctx)),
        // Multiple `when` clauses parse into a `Bool.and` chain (and a
        // negated premise into `Bool.not`) — recurse through the Bool
        // combinators with the exact spellings `lean::builtins` uses so
        // literals INSIDE the conjunction stay ascribed.
        ResolvedExpr::Call(callee, args)
            if matches!(callee, ResolvedCallee::Builtin(n) if n == "Bool.and")
                && args.len() == 2 =>
        {
            format!(
                "({} && {})",
                emit_sample_guard_resolved(&args[0], ctx),
                emit_sample_guard_resolved(&args[1], ctx)
            )
        }
        ResolvedExpr::Call(callee, args)
            if matches!(callee, ResolvedCallee::Builtin(n) if n == "Bool.or")
                && args.len() == 2 =>
        {
            format!(
                "({} || {})",
                emit_sample_guard_resolved(&args[0], ctx),
                emit_sample_guard_resolved(&args[1], ctx)
            )
        }
        ResolvedExpr::Call(callee, args)
            if matches!(callee, ResolvedCallee::Builtin(n) if n == "Bool.not")
                && args.len() == 1 =>
        {
            format!("(!{})", emit_sample_guard_resolved(&args[0], ctx))
        }
        ResolvedExpr::BinOp(op, left, right) => {
            let l = emit_sample_guard_resolved(left, ctx);
            let r = emit_sample_guard_resolved(right, ctx);
            // Operator spellings mirror `expr::emit_expr` exactly.
            let op_str = match op {
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Eq => "==",
                BinOp::Neq => "!=",
                BinOp::Lt => "<",
                BinOp::Gt => ">",
                BinOp::Lte => "<=",
                BinOp::Gte => ">=",
            };
            format!("({} {} {})", l, op_str, r)
        }
        _ => super::expr::emit_expr(expr, ctx),
    }
}

pub fn emit_type_def(td: &TypeDef, ctx: &CodegenContext) -> String {
    emit_type_def_in_scope(td, ctx, None)
}

/// Module-scoped emit: `scope` carries the prefix of the module
/// whose typedefs we're currently rendering (or `None` for entry
/// items). Drives [`find_refined_type_scoped`] so a refined record
/// with a bare name resolves to the current module's canonical
/// entry instead of whichever module populated first.
pub fn emit_type_def_in_scope(td: &TypeDef, ctx: &CodegenContext, scope: Option<&str>) -> String {
    // Canonical Peano type lifted to builtin `Nat`: emit NO `inductive` — its
    // constructors/patterns are rendered as `0` / `_ + 1` and references resolve
    // to Lean's builtin `Nat`. (Skips the DecidableEq scaffold too, which keys
    // off this same emit.)
    if crate::codegen::proof_recognize::detect_canonical_peano(td).is_some() {
        return String::new();
    }
    match td {
        TypeDef::Sum { name, variants, .. } => emit_sum_type(name, variants),
        TypeDef::Product { name, fields, .. } => emit_product_type(name, fields, ctx, scope),
    }
}

/// Check if a sum type is self-referencing (any variant field mentions the type name).
// Recursive-type / field-type predicates moved to `codegen::common` so
// all three backends share a single source of truth. Re-exported below
// as pub(crate) for existing call sites in this backend.
pub(crate) use crate::codegen::common::{
    is_pure_fn, is_recursive_product, is_recursive_sum as is_recursive_type, is_recursive_type_def,
    type_def_name,
};

fn emit_sum_type(name: &str, variants: &[TypeVariant]) -> String {
    let mut lines = Vec::new();
    let is_recursive = is_recursive_type(name, variants);

    lines.push(format!("inductive {} where", name));
    for v in variants {
        let lean_name = to_lower_first(&v.name);
        if v.fields.is_empty() {
            lines.push(format!("  | {}", lean_name));
        } else {
            let field_types: Vec<String> = v
                .fields
                .iter()
                .map(|f| type_annotation_to_lean(f))
                .collect();
            // Lean inductive: fields as positional args after colon
            let fields_str = field_types
                .iter()
                .map(|t| format!("({} : {})", "_", t))
                .collect::<Vec<_>>()
                .join(" ");
            lines.push(format!("  | {} {}", lean_name, fields_str));
        }
    }

    if is_recursive {
        // #14: Recursive types cannot derive DecidableEq automatically
        lines.push("  deriving Repr, BEq, Inhabited".to_string());
    } else {
        lines.push("  deriving Repr, BEq, Inhabited, DecidableEq".to_string());
    }
    lines.join("\n")
}

fn emit_product_type(
    name: &str,
    fields: &[(String, String)],
    ctx: &CodegenContext,
    scope: Option<&str>,
) -> String {
    // Refinement-via-opaque records emit as Lean `Subtype` only
    // when the carrier is `Int`. Float-carrier records (NonNegFloat,
    // Discount, …) stay as plain `structure`, because Lean's `Float`
    // model doesn't admit universal arithmetic laws (IEEE 754: `NaN
    // ≠ NaN`, `+` not commutative across infinities), so the lift
    // would just produce uniwersalne theorems we can't prove. The
    // existing sample-based path covers them: domain values come
    // from `given a: Float = […]`, and proofs are sample-by-sample
    // via native_decide.
    if let Some(decl) = crate::codegen::common::find_refined_type_scoped(ctx, name, scope)
        && decl.carrier_type == "Int"
    {
        let carrier_ty = type_annotation_to_lean(&decl.carrier_type);
        let param = aver_name_to_lean(&decl.predicate_param);
        let predicate = super::expr::emit_expr(&decl.invariant.expr, ctx);
        return format!("abbrev {name} := {{ {param} : {carrier_ty} // {predicate} }}");
    }

    let mut lines = Vec::new();
    let is_recursive = is_recursive_product(name, fields);

    lines.push(format!("structure {} where", name));
    for (field_name, field_type) in fields {
        lines.push(format!(
            "  {} : {}",
            aver_name_to_lean(field_name),
            type_annotation_to_lean(field_type)
        ));
    }

    if is_recursive {
        lines.push("  deriving Repr, BEq, Inhabited".to_string());
    } else {
        lines.push("  deriving Repr, BEq, Inhabited, DecidableEq".to_string());
    }
    lines.join("\n")
}

fn measure_fn_name(type_name: &str) -> String {
    format!("averMeasure{}", type_name)
}

fn measure_list_fn_name(type_name: &str) -> String {
    format!("{}List", measure_fn_name(type_name))
}

fn measure_entries_fn_name(type_name: &str, key_type: &str) -> String {
    let key_suffix: String = key_type
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect();
    format!("{}Entries_{}", measure_fn_name(type_name), key_suffix)
}

fn split_top_level(s: &str, delim: char) -> Vec<String> {
    crate::codegen::common::split_type_params(s, delim)
}

fn unwrap_generic<'a>(type_name: &'a str, prefix: &str) -> Option<&'a str> {
    type_name
        .strip_prefix(prefix)
        .and_then(|rest| rest.strip_suffix('>'))
}

fn type_measure_expr(
    type_name: &str,
    value_expr: &str,
    recursive_types: &HashSet<String>,
    self_type: Option<&str>,
) -> Option<String> {
    let trimmed = type_name.trim();
    if recursive_types.contains(trimmed) {
        return Some(format!("{} {}", measure_fn_name(trimmed), value_expr));
    }

    if let Some(inner) = unwrap_generic(trimmed, "List<") {
        if recursive_types.contains(inner.trim()) {
            return Some(format!(
                "{} {}",
                measure_list_fn_name(inner.trim()),
                value_expr
            ));
        }
        if let Some((key_type, value_type)) =
            entries_measure_tuple_item(inner.trim(), recursive_types)
        {
            return Some(format!(
                "{} {}",
                measure_entries_fn_name(&value_type, &key_type),
                value_expr
            ));
        }
        let item_measure = type_measure_expr(inner, "item", recursive_types, self_type)
            .unwrap_or_else(|| "1".to_string());
        return Some(format!(
            "AverMeasure.list (fun item => {}) {}",
            item_measure, value_expr
        ));
    }

    if let Some(inner) = unwrap_generic(trimmed, "Option<") {
        if self_type == Some(inner.trim()) {
            return Some(format!(
                "(match {} with | .none => 1 | .some item => {} item + 1)",
                value_expr,
                measure_fn_name(inner.trim())
            ));
        }
        let item_measure = type_measure_expr(inner, "item", recursive_types, self_type)
            .unwrap_or_else(|| "1".to_string());
        return Some(format!(
            "AverMeasure.option (fun item => {}) {}",
            item_measure, value_expr
        ));
    }

    if let Some(inner) = unwrap_generic(trimmed, "Map<") {
        let args = split_top_level(inner, ',');
        if args.len() == 2 {
            let key_type = args[0].trim();
            let value_type = args[1].trim();
            if recursive_types.contains(value_type) {
                return Some(format!(
                    "{} (AverMap.entries {})",
                    measure_entries_fn_name(value_type, key_type),
                    value_expr
                ));
            }
            let key_measure = type_measure_expr(key_type, "entry.1", recursive_types, self_type);
            let value_measure =
                type_measure_expr(value_type, "entry.2", recursive_types, self_type);
            let entry_measure = match (key_measure, value_measure) {
                (Some(k), Some(v)) => format!("({k}) + ({v}) + 1"),
                (Some(k), None) => format!("({k}) + 1"),
                (None, Some(v)) => format!("({v}) + 1"),
                (None, None) => "1".to_string(),
            };
            return Some(format!(
                "AverMeasure.list (fun entry => {}) (AverMap.entries {})",
                entry_measure, value_expr
            ));
        }
    }

    if let Some(inner) = unwrap_generic(trimmed, "Result<") {
        let args = split_top_level(inner, ',');
        if args.len() == 2 {
            let ok_measure = type_measure_expr(&args[0], "okVal", recursive_types, self_type)
                .unwrap_or_else(|| "1".to_string());
            let err_measure = type_measure_expr(&args[1], "errVal", recursive_types, self_type)
                .unwrap_or_else(|| "1".to_string());
            return Some(format!(
                "AverMeasure.except (fun errVal => {}) (fun okVal => {}) {}",
                err_measure, ok_measure, value_expr
            ));
        }
    }

    // `Tuple<A, B>` is the canonical tuple spelling; the parenthesized
    // `(A, B)` arm below is the legacy one. Both route through
    // `tuple_parts_measure` so the two spellings stay behaviorally
    // identical.
    if let Some(inner) = unwrap_generic(trimmed, "Tuple<")
        && let Some(measure) = tuple_parts_measure(
            &split_top_level(inner, ','),
            value_expr,
            recursive_types,
            self_type,
        )
    {
        return Some(measure);
    }

    if trimmed.starts_with('(') && trimmed.ends_with(')') {
        let inner = &trimmed[1..trimmed.len() - 1];
        if let Some(measure) = tuple_parts_measure(
            &split_top_level(inner, ','),
            value_expr,
            recursive_types,
            self_type,
        ) {
            return Some(measure);
        }
    }

    None
}

/// Per-part measure for a tuple type's components — the shared body of
/// the `Tuple<...>` arm and the legacy parenthesized `(A, B)` arm of
/// [`type_measure_expr`]. Parts that carry no measure (scalars) are
/// skipped; recursive parts get their deep `averMeasure*` term. Lean
/// renders an N-tuple as a right-nested `Prod`, so part `i` of `n`
/// projects as `.2`*i followed by `.1` (the last part is `.2`*(n-1)):
/// `.1`/`.2` for pairs, `.1`/`.2.1`/`.2.2` for triples.
///
/// FUEL-WRAPPER CONTEXT ONLY (`self_type = None`): there every cited
/// `averMeasure*` is a fully-defined closed call, so a reference under
/// an `AverMeasure.list`/`.option` lambda elaborates fine. Inside a
/// type's OWN measure def (`self_type = Some`) the same term is
/// recursion through a higher-order argument — Lean cannot show
/// termination for `averMeasureT item.2` under `AverMeasure.list
/// (fun item => ...)` — so measure defs keep the old behavior (tuple
/// parts skipped) except for the entries-list reuse, which routes the
/// recursion through a structural `match` in the mutual block instead
/// (see [`entries_measure_tuple_item`]).
fn tuple_parts_measure(
    parts: &[String],
    value_expr: &str,
    recursive_types: &HashSet<String>,
    self_type: Option<&str>,
) -> Option<String> {
    if self_type.is_some() {
        return None;
    }
    let measures: Vec<String> = parts
        .iter()
        .enumerate()
        .filter_map(|(idx, part)| {
            let projection = if idx + 1 == parts.len() {
                ".2".repeat(idx)
            } else {
                format!("{}.1", ".2".repeat(idx))
            };
            type_measure_expr(
                part,
                &format!("{value_expr}{projection}"),
                recursive_types,
                self_type,
            )
        })
        .collect();
    (!measures.is_empty()).then(|| format!("({}) + 1", measures.join(" + ")))
}

/// Recognize a `Tuple<K, V>` list-item type where `V` is a recursive
/// ADT and `K` carries no measure of its own (`String`, `Int`, ...) —
/// the `List<Tuple<K, V>>` spelling of an entries list. In Lean both
/// that spelling and `Map<K, V>` render as `List (K × V)` (the map via
/// `AverMap.entries`), so both reuse the one dedicated deep entries
/// measure `averMeasure<V>Entries_<K>`: fuel bounds — and any lemmas
/// later synthesized over them — agree across the two spellings.
/// Returns `(key_type, value_type)` when the reuse applies. Keys that
/// DO carry a measure fall back to the generic per-part tuple item
/// measure (the entries measure ignores keys, which would under-count
/// their depth). The same predicate gates the emission side
/// ([`recursive_map_key_types`]) so a chosen entries measure is always
/// actually defined.
fn entries_measure_tuple_item(
    item_type: &str,
    recursive_types: &HashSet<String>,
) -> Option<(String, String)> {
    let inner = unwrap_generic(item_type, "Tuple<")?;
    let args = split_top_level(inner, ',');
    if args.len() != 2 {
        return None;
    }
    let key_type = args[0].trim();
    let value_type = args[1].trim();
    (recursive_types.contains(value_type)
        && type_measure_expr(key_type, "key", recursive_types, None).is_none())
    .then(|| (key_type.to_string(), value_type.to_string()))
}

/// Key types whose dedicated entries measure
/// (`averMeasure<value_type>Entries_<key>`) must be emitted alongside
/// `value_type`'s measure: one per `Map<K, value_type>` ref plus one
/// per `List<Tuple<K, value_type>>` ref (the two spellings of an
/// entries list — see [`entries_measure_tuple_item`], which gates the
/// reference side with the same predicate).
fn recursive_map_key_types(
    type_refs: &[String],
    value_type: &str,
    recursive_types: &HashSet<String>,
) -> Vec<String> {
    let mut key_types = Vec::new();
    for type_ref in type_refs {
        let trimmed = type_ref.trim();
        let key_type = if let Some(inner) = unwrap_generic(trimmed, "Map<") {
            let args = split_top_level(inner, ',');
            (args.len() == 2 && args[1].trim() == value_type).then(|| args[0].trim().to_string())
        } else if let Some(inner) = unwrap_generic(trimmed, "List<") {
            entries_measure_tuple_item(inner.trim(), recursive_types)
                .filter(|(_, value)| value == value_type)
                .map(|(key, _)| key)
        } else {
            None
        };
        if let Some(key_type) = key_type
            && !key_types.contains(&key_type)
        {
            key_types.push(key_type);
        }
    }
    key_types
}

fn emit_recursive_sum_measure(
    name: &str,
    variants: &[TypeVariant],
    recursive_types: &HashSet<String>,
    sig_type_refs: &[String],
) -> String {
    let mut lines = vec!["mutual".to_string()];
    lines.push(format!(
        "  def {} (value : {}) : Nat :=",
        measure_fn_name(name),
        name
    ));
    lines.push("    match value with".to_string());
    for variant in variants {
        let ctor = to_lower_first(&variant.name);
        if variant.fields.is_empty() {
            lines.push(format!("    | .{} => 1", ctor));
            continue;
        }

        let binders: Vec<String> = (0..variant.fields.len())
            .map(|idx| format!("x{idx}"))
            .collect();
        let field_measures: Vec<String> = variant
            .fields
            .iter()
            .zip(binders.iter())
            .filter_map(|(field_ty, binder)| {
                type_measure_expr(field_ty, binder, recursive_types, Some(name))
            })
            .collect();
        if field_measures.is_empty() {
            lines.push(format!("    | .{} {} => 1", ctor, binders.join(" ")));
        } else {
            lines.push(format!(
                "    | .{} {} => ({}) + 1",
                ctor,
                binders.join(" "),
                field_measures.join(" + ")
            ));
        }
    }
    lines.push(format!(
        "  def {} (items : List {}) : Nat :=",
        measure_list_fn_name(name),
        name
    ));
    lines.push("    match items with".to_string());
    lines.push("    | [] => 1".to_string());
    lines.push(format!(
        "    | head :: tail => {} head + {} tail + 1",
        measure_fn_name(name),
        measure_list_fn_name(name)
    ));
    // Entries-list refs can sit in the ADT's own fields OR only in fn
    // signatures (`fn f(entries: List<Tuple<String, T>>)` over a `T`
    // whose fields never spell the list) — scan both so every entries
    // measure `type_measure_expr` can choose is actually defined.
    let field_types: Vec<String> = variants
        .iter()
        .flat_map(|variant| variant.fields.iter().cloned())
        .chain(sig_type_refs.iter().cloned())
        .collect();
    for key_type in recursive_map_key_types(&field_types, name, recursive_types) {
        lines.push(format!(
            "  def {} (items : List ({} × {})) : Nat :=",
            measure_entries_fn_name(name, &key_type),
            key_type,
            name
        ));
        lines.push("    match items with".to_string());
        lines.push("    | [] => 1".to_string());
        lines.push(format!(
            "    | (_, value) :: tail => {} value + {} tail + 1",
            measure_fn_name(name),
            measure_entries_fn_name(name, &key_type)
        ));
    }
    lines.push("end".to_string());
    lines.join("\n")
}

fn emit_recursive_product_measure(
    name: &str,
    fields: &[(String, String)],
    recursive_types: &HashSet<String>,
    sig_type_refs: &[String],
) -> String {
    let field_measures: Vec<String> = fields
        .iter()
        .filter_map(|(field_name, field_ty)| {
            type_measure_expr(
                field_ty,
                &format!("value.{}", aver_name_to_lean(field_name)),
                recursive_types,
                Some(name),
            )
        })
        .collect();
    let body = if field_measures.is_empty() {
        "1".to_string()
    } else {
        format!("({}) + 1", field_measures.join(" + "))
    };
    let mut lines = vec![
        "mutual".to_string(),
        format!(
            "  def {} (value : {}) : Nat :=",
            measure_fn_name(name),
            name
        ),
        format!("    {}", body),
        format!(
            "  def {} (items : List {}) : Nat :=",
            measure_list_fn_name(name),
            name
        ),
        "    match items with".to_string(),
        "    | [] => 1".to_string(),
        format!(
            "    | head :: tail => {} head + {} tail + 1",
            measure_fn_name(name),
            measure_list_fn_name(name)
        ),
    ];
    let field_types: Vec<String> = fields
        .iter()
        .map(|(_, ty)| ty.clone())
        .chain(sig_type_refs.iter().cloned())
        .collect();
    for key_type in recursive_map_key_types(&field_types, name, recursive_types) {
        lines.push(format!(
            "  def {} (items : List ({} × {})) : Nat :=",
            measure_entries_fn_name(name, &key_type),
            key_type,
            name
        ));
        lines.push("    match items with".to_string());
        lines.push("    | [] => 1".to_string());
        lines.push(format!(
            "    | (_, value) :: tail => {} value + {} tail + 1",
            measure_fn_name(name),
            measure_entries_fn_name(name, &key_type)
        ));
    }
    lines.push("end".to_string());
    lines.join("\n")
}

pub fn emit_recursive_measure(
    td: &TypeDef,
    recursive_types: &HashSet<String>,
    sig_type_refs: &[String],
) -> Option<String> {
    match td {
        TypeDef::Sum { name, variants, .. } if is_recursive_type(name, variants) => Some(
            emit_recursive_sum_measure(name, variants, recursive_types, sig_type_refs),
        ),
        TypeDef::Product { name, fields, .. } if is_recursive_product(name, fields) => Some(
            emit_recursive_product_measure(name, fields, recursive_types, sig_type_refs),
        ),
        _ => None,
    }
}

/// Emit unsafe DecidableEq instance for a recursive type (#18).
/// Same `unsafeCast`-via-`@[implemented_by]` pattern as the Float
/// `DecidableEq` in the prelude (see `LEAN_PRELUDE_FLOAT_DEC_EQ`). Sound
/// here for a different reason than Float: a recursive user type's `BEq`
/// is derived structurally, so `a == b` ⟺ `a = b` propositionally — the
/// fabricated proof matches. (Float's `==` is IEEE, so its shim reflects
/// IEEE semantics instead; same mechanism, both deliberate.)
pub fn emit_recursive_decidable_eq(name: &str) -> String {
    let mut lines = Vec::new();
    lines.push(format!(
        "private unsafe def {}.unsafeDecEq (a b : {}) : Decidable (a = b) :=",
        name, name
    ));
    lines.push("  if a == b then isTrue (unsafeCast ()) else isFalse (unsafeCast ())".to_string());
    lines.push(format!("@[implemented_by {}.unsafeDecEq]", name));
    lines.push(format!(
        "private opaque {}.compDecEq (a b : {}) : Decidable (a = b)",
        name, name
    ));
    lines.push(format!(
        "instance : DecidableEq {} := {}.compDecEq",
        name, name
    ));
    lines.join("\n")
}

const STRING_POS_FUEL_VAR: &str = "fuel'";

/// Panic message baked into every fuel wrapper's exhaustion arm. This is a
/// SOUNDNESS marker, not just a diagnostic: Lean's `panic!` does NOT abort
/// evaluation — it prints `PANIC at … <this message>` and returns the type's
/// `default` value, so under `native_decide` an exhausted-fuel sample reduces
/// both sides of a model-vs-model equation to `default` and the kernel
/// certifies a vacuous (possibly FALSE) equality with `lake` still exiting 0.
/// `aver proof --check` therefore scans captured lake output for panic lines
/// ([`crate::codegen::lean::count_model_panic_lines`]) and treats any hit as
/// a hard check failure. The scan keys on Lean's generic `PANIC at ` line
/// marker — every prelude `panic!` site shares the vacuity vector, not just
/// this one — so this constant is purely the emission message; changing it
/// cannot blind the gate.
pub const PROOF_FUEL_EXHAUSTED_MSG: &str = "Aver proof fuel exhausted";

fn fuel_helper_name(name: &str) -> String {
    // Use the shared helper so the name matches what the shared AST
    // rewrite emits into `Expr::Ident(...)` call sites. The `__fuel`
    // suffix keeps the result a plain ASCII identifier regardless of
    // the source name, so no Lean-specific escaping is needed.
    crate::codegen::recursion::fuel_helper_name(name)
}

/// Simp-set names for a fuel-emitted fn cited by the
/// `SimpOverPreludeLemmas` law rung: `<name>__fuel` plus the measure
/// helper names (`averMeasure*` / `averStringPosFuel`) the wrapper's
/// fuel expression references. Rather than re-deriving the
/// plan→emission mapping (which `recognize_lex_list_wf_scc` can flip
/// per-SCC to native `termination_by`, no `__fuel` def at all), this
/// PROBES the proof-mode emission itself: re-emit the fn's SCC group
/// through the exact dispatch `transpile_unified` uses and scan the
/// text. Returns `[]` when the emission carries no `def <name>__fuel`
/// — citing a non-existent constant in `simp [...]` would be a hard
/// `unknown constant` build error, the one failure mode the rung's
/// `first | … | sorry` floor cannot catch. Cost: one re-emit of one
/// SCC per fuel-citing law (string building only, no side effects).
/// Assumes proof-mode emission — every production Lean export goes
/// through `transpile_for_proof_mode`.
pub(super) fn law_fuel_simp_names(fn_name: &str, ctx: &CodegenContext) -> Vec<String> {
    let Some(emitted) = probe_fn_scc_emission(fn_name, ctx) else {
        return Vec::new();
    };
    let fuel = fuel_helper_name(fn_name);
    if !emitted.contains(&format!("def {fuel}")) {
        return Vec::new();
    }
    let mut names = vec![fuel];
    names.extend(scan_measure_helper_names(&emitted));
    names
}

/// Re-emit the SCC group that owns `fn_name` through the exact
/// dispatch `transpile_unified` uses and return the emitted text.
/// Shared probe for [`law_fuel_simp_names`] and
/// [`law_string_pos_rank`] — see the former's doc for why probing the
/// emission beats re-deriving the plan→emission mapping. `None` when
/// the fn isn't a pure fn of any scope.
fn probe_fn_scc_emission(fn_name: &str, ctx: &CodegenContext) -> Option<String> {
    // Locate the fn's owning scope (entry first, then dep modules) and
    // the pure-fn population of that scope — the same component
    // universe `transpile_unified` routes.
    let scopes: Vec<(Option<String>, Vec<&crate::ast::FnDef>)> =
        std::iter::once((None, ctx.fn_defs.iter().collect::<Vec<_>>()))
            .chain(
                ctx.modules
                    .iter()
                    .map(|m| (Some(m.prefix.clone()), m.fn_defs.iter().collect())),
            )
            .collect();
    for (scope, fns) in scopes {
        let pure: Vec<&crate::ast::FnDef> = fns.into_iter().filter(|fd| is_pure_fn(fd)).collect();
        if !pure.iter().any(|fd| fd.name == fn_name) {
            continue;
        }
        let comps = crate::call_graph::ordered_fn_components(&pure, &ctx.module_prefixes);
        let comp = comps
            .into_iter()
            .find(|c| c.iter().any(|fd| fd.name == fn_name))?;
        let emitted = ctx.with_module_scope(scope.as_deref(), || {
            if comp.len() > 1 {
                let all_supported = comp
                    .iter()
                    .all(|fd| crate::codegen::common::fn_contract_exists_for_fn(ctx, fd));
                if all_supported {
                    emit_mutual_group_proof(&comp, ctx)
                } else {
                    emit_mutual_group(&comp, ctx)
                }
            } else if let Some(fd) = comp.first() {
                if crate::codegen::common::fn_contract_exists_for_fn(ctx, fd) {
                    emit_fn_def_proof(fd, ctx).unwrap_or_default()
                } else {
                    emit_fn_def(fd, &std::collections::HashSet::from([fd.name.clone()]), ctx)
                        .unwrap_or_default()
                }
            } else {
                String::new()
            }
        });
        return Some(emitted);
    }
    None
}

/// The `averStringPosFuel` rank literal of `fn_name`'s emitted fuel
/// wrapper (`def <fn> … := <fn>__fuel (averStringPosFuel s pos RANK)
/// …`), probed from the actual proof-mode emission so the
/// `StringEscapeRoundtrip` skeleton's `show`-line quotes the exact
/// fuel expression the wrapper carries. `None` when the fn isn't
/// fuel-emitted with a string-pos wrapper — the renderer declines
/// rather than quoting a fuel expression that doesn't exist.
pub(super) fn law_string_pos_rank(fn_name: &str, ctx: &CodegenContext) -> Option<usize> {
    let emitted = probe_fn_scc_emission(fn_name, ctx)?;
    let fuel = fuel_helper_name(fn_name);
    if !emitted.contains(&format!("def {fuel}")) {
        return None;
    }
    let marker = format!("{fuel} (averStringPosFuel ");
    let idx = emitted.find(&marker)?;
    let rest = &emitted[idx + marker.len()..];
    let mut tokens = rest.split_whitespace();
    let _string_arg = tokens.next()?;
    let _pos_arg = tokens.next()?;
    tokens.next()?.trim_end_matches(')').parse::<usize>().ok()
}

/// Harvest measure-helper identifiers (`averMeasure*`,
/// `averStringPosFuel`) from emitted Lean text. These are the names a
/// fuel wrapper's initial-fuel expression references; the
/// `SimpOverPreludeLemmas` rung needs them in its simp set so the fuel
/// value computes to a `Nat` literal before the `__fuel` equations
/// fire. Sorted + deduped for deterministic emit.
fn scan_measure_helper_names(text: &str) -> Vec<String> {
    let mut found: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for prefix in ["averMeasure", "averStringPosFuel"] {
        for (idx, _) in text.match_indices(prefix) {
            // Reject mid-identifier hits (`xaverMeasure`).
            if idx > 0
                && text[..idx]
                    .chars()
                    .next_back()
                    .is_some_and(|c| c.is_alphanumeric() || c == '_')
            {
                continue;
            }
            let rest = &text[idx..];
            let end = rest
                .find(|c: char| !(c.is_alphanumeric() || c == '_'))
                .unwrap_or(rest.len());
            found.insert(rest[..end].to_string());
        }
    }
    found.into_iter().collect()
}

fn emit_fn_param_names(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(name, _)| aver_name_to_lean(name))
        .collect::<Vec<_>>()
        .join(" ")
}

fn indent_lines(block: &str, prefix: &str) -> Vec<String> {
    block
        .lines()
        .map(|line| format!("{prefix}{line}"))
        .collect()
}

/// Neutralize Lean block-comment delimiters inside doc text. A `/-` or `-/` in
/// the text would open/close a NESTED block comment inside the `/-- ... -/` doc
/// comment Lean wraps it in, leaving the comment unterminated and breaking the
/// whole file (`error: unterminated comment`). Splitting the 2-char token with a
/// space stops it tokenizing as a delimiter while reading identically in prose
/// (e.g. an Aver `?` doc mentioning `+2/-2` renders `+2/ -2`).
pub(crate) fn sanitize_doc(text: &str) -> String {
    text.replace("/-", "/ -").replace("-/", "- /")
}

fn emit_doc_comment(desc: &Option<String>) -> Vec<String> {
    desc.as_ref()
        .map(|text| vec![format!("/-- {} -/", sanitize_doc(text))])
        .unwrap_or_default()
}

fn ret_type_or_unit(fd: &FnDef) -> String {
    if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    }
}

fn emit_fuel_helper_def(
    helper_name: &str,
    params: &str,
    ret_type: &str,
    body: &str,
    outer_indent: &str,
) -> Vec<String> {
    let branch_indent = format!("{outer_indent}    ");
    [
        vec![format!(
            "{outer_indent}def {} (fuel : Nat) {} : {} :=",
            helper_name, params, ret_type
        )],
        vec![format!("{outer_indent}  match fuel with")],
        vec![format!(
            "{outer_indent}  | 0 => panic! \"{}\"",
            PROOF_FUEL_EXHAUSTED_MSG
        )],
        vec![format!("{outer_indent}  | {} + 1 =>", STRING_POS_FUEL_VAR)],
        indent_lines(body, &branch_indent),
    ]
    .into_iter()
    .flatten()
    .collect()
}

fn emit_string_pos_wrapper(fd: &FnDef, helper_name: &str, rank_budget: usize) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let (s_name, _) = &fd.params[0];
    let (pos_name, _) = &fd.params[1];
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} (averStringPosFuel {} {} {}) {}",
            helper_name,
            aver_name_to_lean(s_name),
            aver_name_to_lean(pos_name),
            rank_budget,
            arg_names
        ),
    ]
}

fn emit_int_countdown_wrapper(fd: &FnDef, helper_name: &str, param_index: usize) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let metric_name = fd
        .params
        .get(param_index)
        .map(|(name, _)| aver_name_to_lean(name))
        .unwrap_or_else(|| "0".to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} ((Int.natAbs {}) + 1) {}",
            helper_name, metric_name, arg_names
        ),
    ]
}

fn emit_nat_linear_recurrence_fn(
    fd: &FnDef,
    shape: &super::recurrence::SecondOrderIntLinearRecurrenceShape,
    ctx: &CodegenContext,
) -> String {
    let fn_name = aver_name_to_lean(&fd.name);
    let nat_helper_name = recurrence_nat_helper_name(&fd.name);
    let lean_param = aver_name_to_lean(&shape.param_name);
    let ret_type = ret_type_or_unit(fd);
    let nat_step = render_affine_pair_expr(
        shape.recurrence,
        &format!("{nat_helper_name} n"),
        &format!("{nat_helper_name} (n + 1)"),
    );

    [
        emit_doc_comment(&fd.desc),
        vec![
            format!("private def {} : Nat -> {}", nat_helper_name, ret_type),
            format!("  | 0 => {}", emit_expr_legacy(&shape.base0, ctx, None)),
            format!("  | 1 => {}", emit_expr_legacy(&shape.base1, ctx, None)),
            format!("  | n + 2 => {}", nat_step),
            String::new(),
            format!("def {} ({} : Int) : {} :=", fn_name, lean_param, ret_type),
            format!(
                "  if {} < 0 then {} else {} {}.toNat",
                lean_param,
                emit_expr_legacy(&shape.negative_branch, ctx, None),
                nat_helper_name,
                lean_param
            ),
        ],
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_sizeof_measure_expr(fd: &FnDef, recursive_types: &HashSet<String>) -> Option<String> {
    let measure_terms: Vec<String> = sizeof_measure_param_indices(fd)
        .into_iter()
        .filter_map(|idx| {
            fd.params.get(idx).and_then(|(name, type_name)| {
                type_measure_expr(type_name, &aver_name_to_lean(name), recursive_types, None)
            })
        })
        .collect();

    (!measure_terms.is_empty()).then(|| measure_terms.join(" + "))
}

fn emit_mutual_sizeof_wrapper(
    fd: &FnDef,
    helper_name: &str,
    rank_budget: usize,
    recursive_types: &HashSet<String>,
) -> Vec<String> {
    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let fuel_expr = emit_sizeof_measure_expr(fd, recursive_types)
        .map(|measure| format!("(({}) + 1) * {}", measure, rank_budget))
        .unwrap_or_else(|| rank_budget.to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!("  {} ({}) {}", helper_name, fuel_expr, arg_names),
    ]
}

fn emit_fuelized_string_pos_fn(fd: &FnDef, ctx: &CodegenContext) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = emit_fn_body_for(fd, &rewritten, ctx);

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_string_pos_wrapper(fd, &helper_name, 1),
        emit_string_pos_scan_lemma(fd, &helper_name, ctx)
            .map(|lemma| vec![String::new(), lemma])
            .unwrap_or_default(),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

/// Companion theorem for a fuelized string-position SCANNER — the
/// general crack in the fuel-unfolding barrier (#125 family): when the
/// body matches the canonical shape `match String.charAt s pos with |
/// none => EXIT | some c => if P c then SELF(s, pos+1, …) else OTHER`
/// (recognized by `proof_recognize::detect_string_pos_scan`), emit
///
/// ```text
/// theorem <fn>__fuel_scan : ∀ fuel s pos <carried>,
///   0 ≤ pos → pos.toNat ≤ s.data.length →
///   s.data.length - pos.toNat < fuel →
///   (∀ ch ∈ s.data.drop pos.toNat, P (Char.toString ch) = true) →
///   <fn>__fuel fuel s pos <args@pins> = EXIT[pos := ↑s.data.length]
/// ```
///
/// proved by a FIXED fuel-induction template (`String.charAt_eq_of_lt`
/// / `String.charAt_none_of_ge` + `List.drop_eq_getElem_cons` + omega —
/// ported verbatim from the verified json hand proof). Universal-law
/// emissions (`IntDecimalRoundtrip`) rewrite through this lemma to run
/// a symbolic all-`P` suffix to the end of the string.
///
/// CONSERVATIVELY SHAPE-GATED: when the body does not match the exact
/// recognizer shape, NOTHING is emitted — every emission must be
/// provable by the uniform template BY CONSTRUCTION of the gate (a
/// synthesized lemma that fails to prove would be a build error in the
/// export). The predicate must also resolve to a pure single-`String`-
/// param `Bool` fn (the lemma cites it by name as a hypothesis key; it
/// is never unfolded).
fn emit_string_pos_scan_lemma(
    fd: &FnDef,
    helper_name: &str,
    ctx: &CodegenContext,
) -> Option<String> {
    let shape = crate::codegen::proof_recognize::detect_string_pos_scan(fd)?;
    let scope = ctx.active_module_scope();
    let pred_fd = ctx
        .fn_def_by_name(&shape.predicate_fn, scope.as_deref())
        .or_else(|| ctx.fn_def_by_name(&shape.predicate_fn, None))?;
    if !crate::codegen::proof_recognize::scan_predicate_fn_ok(pred_fd) {
        return None;
    }

    let s = aver_name_to_lean(&fd.params[0].0);
    let pos = aver_name_to_lean(&fd.params[1].0);
    let pred = aver_name_to_lean(&shape.predicate_fn);
    let lemma_name = format!("{helper_name}_scan");

    // Trailing args: carried params stay variables (quantified), pinned
    // params bake their Bool literal into statement + calc steps.
    let mut carried_binders: Vec<String> = Vec::new();
    let mut carried_names: Vec<String> = Vec::new();
    let mut trailing_args: Vec<String> = Vec::new();
    for (i, pin) in shape.param_pins.iter().enumerate() {
        let (name, ty) = &fd.params[i + 2];
        match pin {
            None => {
                let lean = aver_name_to_lean(name);
                carried_binders.push(format!(" ({} : {})", lean, type_annotation_to_lean(ty)));
                carried_names.push(lean.clone());
                trailing_args.push(lean);
            }
            Some(b) => trailing_args.push(b.to_string()),
        }
    }
    let args = trailing_args
        .iter()
        .map(|a| format!(" {a}"))
        .collect::<String>();
    let carried_binder_text: String = carried_binders.concat();
    let carried_intro = carried_names
        .iter()
        .map(|n| format!("{n} "))
        .collect::<String>();

    // EXIT[pos := ↑s.data.length, pinned := literal]: substitute at the
    // AST level (a unique marker stands in for the length cast, which
    // has no Aver-AST form), render through the SAME expr emitter the
    // body used, then swap the marker for the cast.
    const LEN_MARKER: &str = "AVERSCANLEN";
    let mut subst: std::collections::HashMap<String, crate::ast::Expr> =
        std::collections::HashMap::new();
    subst.insert(
        fd.params[1].0.clone(),
        crate::ast::Expr::Ident(LEN_MARKER.to_string()),
    );
    for (i, pin) in shape.param_pins.iter().enumerate() {
        if let Some(b) = pin {
            subst.insert(
                fd.params[i + 2].0.clone(),
                crate::ast::Expr::Literal(crate::ast::Literal::Bool(*b)),
            );
        }
    }
    let exit_subst =
        crate::codegen::proof_recognize::substitute_idents_in_expr(&shape.exit_expr, &subst);
    let exit = emit_expr_legacy(&exit_subst, ctx, None)
        .replace('\n', " ")
        .replace(LEN_MARKER, &format!("(({s}.data.length : Int))"));

    Some(format!(
        r#"/-- Auto-synthesized scan lemma: an all-`{pred}` suffix scan runs to the
    end of the string. Companion to the `{helper_name}` fuel def; proved by
    the fixed fuel-induction template. -/
theorem {lemma_name} :
    ∀ (fuel : Nat) ({s} : String) ({pos} : Int){carried_binder_text},
      0 ≤ {pos} → {pos}.toNat ≤ {s}.data.length →
      {s}.data.length - {pos}.toNat < fuel →
      (∀ ch ∈ {s}.data.drop {pos}.toNat, {pred} (Char.toString ch) = true) →
      {helper_name} fuel {s} {pos}{args} = {exit} := by
  intro fuel
  induction fuel with
  | zero =>
    intro {s} {pos} {carried_intro}h0 h1 h2 h3
    omega
  | succ fuel ih =>
    intro {s} {pos} {carried_intro}h0 h1 h2 h3
    by_cases hlt : {pos}.toNat < {s}.data.length
    · have hch := String.charAt_eq_of_lt {s} {pos} h0 hlt
      have hdrop := List.drop_eq_getElem_cons (l := {s}.data) (n := {pos}.toNat) hlt
      have hdig : {pred} (Char.toString ({s}.data[{pos}.toNat])) = true := by
        apply h3
        rw [hdrop]
        exact List.mem_cons_self _ _
      have hstep : ∀ ch ∈ {s}.data.drop (({pos} + 1).toNat), {pred} (Char.toString ch) = true := by
        intro ch hc
        apply h3
        rw [hdrop]
        refine List.mem_cons_of_mem _ ?_
        have he : ({pos} + 1).toNat = {pos}.toNat + 1 := by omega
        rw [he] at hc
        exact hc
      have hrec := ih {s} ({pos} + 1) {carried_intro}(by omega) (by omega) (by omega) hstep
      calc {helper_name} (fuel + 1) {s} {pos}{args}
          = {helper_name} fuel {s} ({pos} + 1){args} := by
            simp only [{helper_name}, hch, hdig]
            simp
        _ = {exit} := hrec
    · have hpos : {pos} = ({s}.data.length : Int) := by omega
      have hch := String.charAt_none_of_ge {s} {pos} h0 (by omega)
      simp only [{helper_name}, hch]
      rw [hpos]"#
    ))
}

fn strip_match_eq_binders(body: String) -> String {
    body.lines()
        .map(|line| {
            let trimmed = line.trim_start();
            let indent_len = line.len() - trimmed.len();
            let indent = &line[..indent_len];
            let Some(rest) = trimmed.strip_prefix("match h_") else {
                return line.to_string();
            };
            let Some(colon_idx) = rest.find(" : ") else {
                return line.to_string();
            };
            format!("{indent}match {}", &rest[colon_idx + 3..])
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Native `IntCountdown` emission for closed-world fns with the canonical
/// `match p { 0 -> BASE; _ -> rec(p-1, ...) }` shape. Splits the fn into:
///
/// - `<name>__aux` — the real recursion carrying an explicit `(h : p ≥ 0)`
///   precondition. Recursive callsites in its body are rewritten to call
///   `<name>__aux` instead of `<name>` with an extra `(by omega)` proof
///   obligation appended (synthesized via the
///   `OMEGA_PROOF_SENTINEL` ident — see `lean::expr::emit_expr`).
/// - `<name>` — the public wrapper preserving the original Aver signature.
///   Dispatches on `p ≥ 0` to the aux; the `p < 0` branch returns `BASE`
///   (the source's `0` arm). That falls outside the Aver well-formed
///   domain for the issue-84 fibonacci-style targets, but keeping the aux
///   private and total avoids forcing every call site (verify samples,
///   peer fn bodies) to thread proof obligations.
///
/// Lean accepts this because the aux's `termination_by p.natAbs` together
/// with `(h : p ≥ 0)` + the `_` arm's implicit `p ≠ 0` lets `omega`
/// discharge `(p - 1).natAbs < p.natAbs` mechanically.
fn emit_native_guarded_int_countdown_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
    base_arm_literal: i64,
    base_arm_body: &Spanned<crate::ir::hir::ResolvedExpr>,
    wildcard_arm_body: &Spanned<crate::ir::hir::ResolvedExpr>,
    precondition: &[Spanned<crate::ir::hir::ResolvedExpr>],
) -> String {
    let aux_name = native_aux_name(&fd.name);
    let main_name = aver_name_to_lean(&fd.name);
    let lean_aux_name = aver_name_to_lean(&aux_name);
    let Some((param_name, _)) = fd.params.get(param_index) else {
        return emit_fuelized_int_countdown_fn(fd, ctx, param_index);
    };
    let lean_pname = aver_name_to_lean(param_name);

    // Precondition: AND of caller-derived predicates, or `(p ≥ 0)`
    // when the artifact has no single external caller (free-standing
    // fns / test fixtures). Same `Spanned<Expr>`-as-predicate path
    // opaque types use, so `emit_expr` is the single emitter — no
    // parallel infrastructure.
    let precond_lean = if precondition.is_empty() {
        format!("{} ≥ 0", lean_pname)
    } else {
        precondition
            .iter()
            .map(|p| format!("({})", super::expr::emit_expr(p, ctx)))
            .collect::<Vec<_>>()
            .join(" ∧ ")
    };

    let aux_params = format!("{} (h_dom : {})", emit_fn_params(&fd.params), precond_lean);
    let main_params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);

    // Emit `if h_zero : n = LITERAL then BASE else REC` rather than
    // `match n with | LITERAL => ... | _ => ...`. The dependent `if h :
    // c then ... else` form puts `h : c` / `h : ¬c` in scope for the
    // corresponding branch, which `omega` needs to discharge `(n - 1)
    // ≥ 0` and `(n - 1).natAbs < n.natAbs` at the recursive callsite +
    // termination check. Plain `match` would leave the case-split
    // implicit (only an unnamed `casesOn` motive carries it) and
    // `omega` can't see it.
    // Resolve the recursive fn's `FnId` via the same pointer-eq path
    // `ProofIR.fn_contracts` was keyed by — `fn_id_for_decl` picks
    // the owning module's prefix when `fd` came from a dep, the
    // entry slot when it sits in `ctx.fn_defs`. Bare-name
    // `FnKey::entry(fd.name)` would collide for any module-owned
    // recursive fn whose bare name also exists at entry (the very
    // class of bug #147 phase E is killing).
    let target_fn_id = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .unwrap_or_else(|| panic!("native-guarded fn {} missing FnId", fd.name));
    let rewritten_wc = crate::codegen::recursion::rewrite_native_guarded_calls_resolved_expr(
        wildcard_arm_body,
        target_fn_id,
        &aux_name,
    );
    let base_str = super::expr::emit_expr(base_arm_body, ctx);
    let rec_str = super::expr::emit_expr(&rewritten_wc, ctx);
    let arg_names = emit_fn_param_names(&fd.params);

    let mut lines = Vec::new();
    lines.extend(emit_doc_comment(&fd.desc));
    lines.push(format!(
        "def {} {} : {} :=",
        lean_aux_name, aux_params, ret_type
    ));
    lines.push(format!(
        "  if h_zero : {} = {} then {}",
        lean_pname, base_arm_literal, base_str
    ));
    lines.push(format!("  else {}", rec_str));
    lines.push(format!("termination_by Int.natAbs {}", lean_pname));
    lines.push("decreasing_by".to_string());
    lines.push("  simp_wf".to_string());
    lines.push("  omega".to_string());
    lines.push(String::new());

    lines.push(format!(
        "def {} {} : {} :=",
        main_name, main_params, ret_type
    ));
    lines.push(format!(
        "  if h_dom : {} then {} {} h_dom",
        precond_lean, lean_aux_name, arg_names
    ));
    lines.push(format!("  else {}", base_str));

    lines.join("\n")
}

fn emit_fuelized_int_countdown_fn(fd: &FnDef, ctx: &CodegenContext, param_index: usize) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_int_countdown_wrapper(fd, &helper_name, param_index),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_fuelized_int_ascending_fn(
    fd: &FnDef,
    ctx: &CodegenContext,
    param_index: usize,
    bound_lean: &str,
) -> String {
    let helper_name = fuel_helper_name(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let rewritten = rewrite_recursive_calls_body(
        &fd.body,
        &HashSet::from([fd.name.clone()]),
        STRING_POS_FUEL_VAR,
    );
    let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

    [
        emit_doc_comment(&fd.desc),
        emit_fuel_helper_def(&helper_name, &params, &ret_type, &body, ""),
        vec![String::new()],
        emit_int_ascending_wrapper(fd, &helper_name, param_index, bound_lean),
    ]
    .into_iter()
    .flatten()
    .collect::<Vec<_>>()
    .join("\n")
}

fn emit_int_ascending_wrapper(
    fd: &FnDef,
    helper_name: &str,
    param_index: usize,
    bound_lean: &str,
) -> Vec<String> {
    let fn_name = super::expr::aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = ret_type_or_unit(fd);
    let arg_names = emit_fn_param_names(&fd.params);
    let metric_name = fd
        .params
        .get(param_index)
        .map(|(name, _)| super::expr::aver_name_to_lean(name))
        .unwrap_or_else(|| "0".to_string());
    vec![
        format!("def {} {} : {} :=", fn_name, params, ret_type),
        format!(
            "  {} ((Int.natAbs ({} - {})) + 1) {}",
            helper_name, bound_lean, metric_name, arg_names
        ),
    ]
}

/// Read the rank component of a `Fuel { Lex { .., rank } }` contract.
/// Returns `None` when the fn has no contract or the contract isn't
/// a Lex shape (non-mutual variant or non-recursive).
fn contract_lex_rank(ctx: &CodegenContext, fd: &FnDef) -> Option<usize> {
    contract_lex_params_rank(ctx, fd).map(|(_, rank)| rank)
}

/// Read both the params Vec and rank of a `Fuel { Lex { params, rank } }`
/// contract. Returns `None` for non-Lex / non-recursive / missing
/// contracts. Used by mutual-SCC dispatchers to distinguish:
///
/// - `MutualIntCountdown`: `params.len() == 1`, rank == 0
/// - `MutualStringPosAdvance`: `params.len() == 2`
/// - `MutualSizeOfRanked`: `params.is_empty()`
fn contract_lex_params_rank<'a>(
    ctx: &'a CodegenContext,
    fd: &FnDef,
) -> Option<(&'a [String], usize)> {
    let contract = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)?;
    let crate::ir::RecursionContract::Fuel {
        fuel_metric: crate::ir::FuelMetric::Lex { params, rank },
    } = contract.recursion.as_ref()?
    else {
        return None;
    };
    Some((params.as_slice(), *rank))
}

fn emit_fuelized_mutual_string_pos_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let max_rank = fns
        .iter()
        .filter_map(|fd| contract_lex_rank(ctx, fd))
        .max()
        .unwrap_or(1);

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body_for(fd, &rewritten, ctx);

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines = emit_string_pos_wrapper(fd, &helper_name, max_rank);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

fn emit_fuelized_mutual_int_countdown_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = strip_match_eq_binders(emit_fn_body_for(fd, &rewritten, ctx));

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines = emit_int_countdown_wrapper(fd, &helper_name, 0);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

/// Termination measure for Lean's native `termination_by` clause —
/// `name.length` for List/Vector/String params (Lean's stdlib
/// `List.length` is what `decreasing_tactic` knows how to chase),
/// `sizeOf name` fallback for recursive ADTs. Sum across every
/// sizeOf-relevant param.
///
/// Epic #180 Phase 4 — reads param types from the resolved fn def
/// (already typed by the typechecker) instead of re-parsing the
/// AST annotation string.
fn emit_native_termination_measure(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    let indices = crate::codegen::recursion::detect::sizeof_measure_param_indices(fd);
    if indices.is_empty() {
        return None;
    }
    // Pointer-eq scope so a same-bare-name twin never provides
    // these param types. Synthetic / mid-rewrite fns fall back to
    // on-demand resolve.
    let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    let resolved_owned = match resolved_fd {
        Some(_) => None,
        None => Some(ctx.resolve_fn_def(fd, None)),
    };
    let rfd: &crate::ir::hir::ResolvedFnDef =
        resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
    let mut terms: Vec<String> = Vec::new();
    for idx in indices {
        let (name, ty) = rfd.params.get(idx)?;
        let lean_name = aver_name_to_lean(name);
        match ty {
            crate::types::Type::List(_) | crate::types::Type::Vector(_) => {
                // `sizeOf` instead of `.length` so the user measure
                // matches what Lean's mutual-block wf elaboration
                // generates internally — `decreasing_tactic` then
                // closes the chain without `simp_wf` scrambling.
                terms.push(format!("sizeOf {lean_name}"));
            }
            // Skip Named ADTs and String: `sizeOf` decrease on a
            // recursive ADT in a multi-arg measure (`step f` from
            // `stepApp f arg`, measure `sizeOf f + sizeOf arg`)
            // needs the strict-positivity fact `sizeOf arg ≥ 1`
            // which omega doesn't get for free. Fall back to fuel
            // for those SCCs — accumulator-pattern guard already
            // filters them too, so the only effect here is a more
            // conservative measure-existence gate.
            _ => return None,
        }
    }
    (!terms.is_empty()).then(|| terms.join(" + "))
}

// ---------------------------------------------------------------------------
// Termination-as-a-law: length-monotonicity WF for computed-arg mutual SCCs.
//
// A mutual SCC like quicksort's `sort` / `sortWithPivot` recurses through
// COMPUTED list arguments (`sort (smallerOrEqual rest pivot)`) that Lean's
// structural recursion can't see decrease. Instead of a fuel encoding, this
// path synthesises + KERNEL-PROVES a length-monotonicity lemma for each
// partition helper (`(g s …).length ≤ s.length`) and emits a genuine
// well-founded `mutual` block whose per-def `decreasing_by` cites the lemma.
//
// Soundness rides on the Lean kernel: each `…_len_le` lemma is proved by the
// validated `induction … split … List.length_cons; omega` tactic, or the SCC
// is rejected and the caller falls back to fuel. A lemma that doesn't hold
// (helper grows the list) makes `lake build` fail rather than slip through.
// ---------------------------------------------------------------------------

/// How a recursive call's measure-list argument relates to the caller's
/// measure-list param.
#[derive(Clone)]
enum LexListArg {
    /// The argument is the caller's measure param itself or a `cons`-tail
    /// subterm of it — the structural-subterm case. Tie-break by rank.
    Subterm {
        /// `true` when the argument is a strict subterm (the cons-tail),
        /// i.e. the callee receives `caller_len - 1`. `false` when it is
        /// the param verbatim (callee receives `caller_len`).
        strict: bool,
    },
    /// The argument is `g(s, …)` where `g` is a non-growing structural
    /// list filter and `s` is the caller's measure param or its cons-tail.
    /// `helper` is the Aver fn name of `g`; `lemma_args` is the Lean source
    /// of `g`'s actual arguments (`rest pivot`), used to instantiate the
    /// synthesised `<g>_len_le` lemma in `decreasing_by`.
    Filtered { helper: String, lemma_args: String },
}

/// One recognised recursive edge inside the SCC, in body source order
/// (the order Lean generates `decreasing_by` goals).
struct LexListEdge {
    callee: String,
    arg: LexListArg,
}

/// A length lemma to synthesise: `(<helper> xs <rest…>).length ≤ xs.length`.
#[derive(Clone)]
struct LengthLemma {
    /// Lean name of the filter helper.
    helper_lean: String,
    /// Lean param list of the helper (`(xs : List Int) (pivot : Int)`).
    params: String,
    /// Lean names of the helper's params, in order.
    param_names: Vec<String>,
}

impl LengthLemma {
    fn lemma_name(&self) -> String {
        format!("{}_len_le", self.helper_lean)
    }
}

/// Per-fn plan for the lex-list WF mutual block.
struct LexListMemberPlan<'a> {
    fd: &'a FnDef,
    /// Lean name of the measure list param.
    list_param_lean: String,
    /// `+offset` added to `list_param.length` in the lex first component.
    offset: usize,
    /// Lex second component (rank).
    rank: usize,
    /// Recognised recursive edges, in body source order.
    edges: Vec<LexListEdge>,
}

/// Pull the single measure list param (a `List<_>` param) of a fn, as
/// (param index, Lean name). Returns `None` when the fn has zero or more
/// than one `List<_>` param (the synthesiser only reasons about a single
/// list measure) or any non-`List` sizeOf-relevant param.
fn lex_list_measure_param(fd: &FnDef, ctx: &CodegenContext) -> Option<(usize, String)> {
    let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    let resolved_owned = match resolved_fd {
        Some(_) => None,
        None => Some(ctx.resolve_fn_def(fd, None)),
    };
    let rfd: &crate::ir::hir::ResolvedFnDef =
        resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
    let mut found: Option<(usize, String)> = None;
    for (idx, (name, ty)) in rfd.params.iter().enumerate() {
        match ty {
            crate::types::Type::List(_) => {
                if found.is_some() {
                    // More than one List param — outside the single-measure
                    // shape this synthesiser reasons about.
                    return None;
                }
                found = Some((idx, aver_name_to_lean(name)));
            }
            // Any other sizeOf-relevant param (Vector / String / recursive
            // ADT) means the measure isn't a single List length — back off.
            crate::types::Type::Vector(_) | crate::types::Type::Str => return None,
            _ => {}
        }
    }
    found
}

/// True iff `g` is a non-growing structural list filter: a recursive fn
/// `(xs : List _, …) -> List _` whose body matches on its first list param
/// with `[] -> []` and `[x, ..rest] -> <if-cond> { x :: g(rest, …) | g(rest, …) }`.
/// This is exactly the shape the validated
/// `induction … split … List.length_cons; omega` proof closes, giving
/// `(g xs …).length ≤ xs.length`. Anything else returns `None`.
fn lex_list_filter_helper<'a>(helper: &'a FnDef, ctx: &CodegenContext) -> Option<&'a FnDef> {
    // First param must be `List<_>`, and the fn must return `List<_>`.
    let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, helper)
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    let resolved_owned = match resolved_fd {
        Some(_) => None,
        None => Some(ctx.resolve_fn_def(helper, None)),
    };
    let rfd: &crate::ir::hir::ResolvedFnDef =
        resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
    let first_param = rfd.params.first()?;
    if !matches!(first_param.1, crate::types::Type::List(_)) {
        return None;
    }
    if !matches!(rfd.return_type, crate::types::Type::List(_)) {
        return None;
    }
    let list_param_name = first_param.0.clone();

    // Body must be a single tail expression: `match <list_param> { … }`.
    let tail = helper.body.tail_expr()?;
    if helper.body.stmts().len() != 1 {
        return None;
    }
    let Expr::Match { subject, arms } = &tail.node else {
        return None;
    };
    // Subject must be the (first) list param.
    let subject_name = crate::codegen::recursion::detect::local_name_of(subject)?;
    if subject_name != list_param_name {
        return None;
    }
    let mut saw_nil_empty = false;
    let mut saw_cons_filter = false;
    for arm in arms {
        match &arm.pattern {
            // `[] -> []`
            Pattern::EmptyList if matches!(&arm.body.node, Expr::List(items) if items.is_empty()) =>
            {
                saw_nil_empty = true;
            }
            Pattern::Cons(head, tail_bind)
                if lex_filter_cons_arm_ok(&arm.body, helper, head, tail_bind) =>
            {
                saw_cons_filter = true;
            }
            // Any other arm (non-`[]` nil body, non-filter cons arm,
            // literal / wildcard / ctor) breaks the recognised shape.
            _ => return None,
        }
    }
    (saw_nil_empty && saw_cons_filter).then_some(helper)
}

/// A `cons`-arm body of a filter helper: `<bool-match> { true -> head :: g(tail, …) ; false -> g(tail, …) }`
/// (either branch may be the keep / drop side). Both branches must be
/// either `g(tail, …)` or `head :: g(tail, …)` — i.e. each recursive call
/// passes the cons-tail and prepends at most one element. That guarantees
/// `(g xs …).length ≤ xs.length` under the validated proof.
fn lex_filter_cons_arm_ok(
    body: &Spanned<Expr>,
    helper: &FnDef,
    head: &str,
    tail_bind: &str,
) -> bool {
    match &body.node {
        // `match <cond> { true -> …; false -> … }` over a Bool subject.
        Expr::Match { arms, .. } => arms
            .iter()
            .all(|arm| lex_filter_branch_ok(&arm.body, helper, head, tail_bind)),
        // A direct branch (no inner conditional) — accept the same leaf
        // shapes so a `[x, ..rest] -> g(rest, …)` arm is recognised too.
        _ => lex_filter_branch_ok(body, helper, head, tail_bind),
    }
}

/// Leaf of a filter cons-arm: `g(tail, …)` or `head :: g(tail, …)`
/// (`List.prepend(head, g(tail, …))`). The recursive call must target the
/// helper itself and pass the cons-tail in the first (list) position.
fn lex_filter_branch_ok(body: &Spanned<Expr>, helper: &FnDef, head: &str, tail_bind: &str) -> bool {
    // A recursive call on the cons-tail — `g(tail, …)`. The TCO pass may
    // have rewritten a tail-position self-call into `Expr::TailCall`, so
    // accept both shapes. Self-identity uses the shared
    // `canonical_callee_name` helper (syntax-discovery-only) so the
    // suffix-match category rule lives in one place.
    let self_set: HashSet<String> = std::iter::once(helper.name.clone()).collect();
    let recur_on_tail = |target: &str, args: &[Spanned<Expr>]| -> bool {
        let is_self =
            crate::codegen::recursion::detect::canonical_callee_name(target, &self_set).is_some();
        is_self
            && args
                .first()
                .and_then(crate::codegen::recursion::detect::local_name_of)
                .filter(|n| *n == tail_bind)
                .is_some()
    };
    match &body.node {
        Expr::FnCall(callee, args) => {
            let name = crate::codegen::recursion::detect::expr_to_dotted_name(callee);
            match name.as_deref() {
                // `head :: g(tail, …)` → encoded as `List.prepend(head, recur)`.
                Some("List.prepend") if args.len() == 2 => {
                    crate::codegen::recursion::detect::local_name_of(&args[0])
                        .filter(|n| *n == head)
                        .is_some()
                        && lex_filter_branch_ok(&args[1], helper, head, tail_bind)
                }
                // `g(tail, …)` — drop-element branch.
                Some(n) => recur_on_tail(n, args),
                None => false,
            }
        }
        // TCO'd tail-position self-call: `g(tail, …)`.
        Expr::TailCall(boxed) => recur_on_tail(&boxed.target, &boxed.args),
        _ => false,
    }
}

/// Lean source of a recursive call's measure argument, classified.
/// `caller_list_param` is the caller's measure param name; `tail_binders`
/// is the set of cons-tail binders of a match on that param in the caller
/// body; `scc` is the SCC member names; `ctx` resolves filter helpers.
fn classify_lex_list_arg(
    arg: &Spanned<Expr>,
    caller_list_param: &str,
    tail_binders: &HashSet<String>,
    ctx: &CodegenContext,
) -> Option<LexListArg> {
    use crate::codegen::recursion::detect::{expr_to_dotted_name, local_name_of};
    // Subterm: the param itself or a cons-tail of it.
    if let Some(name) = local_name_of(arg) {
        if name == caller_list_param {
            return Some(LexListArg::Subterm { strict: false });
        }
        if tail_binders.contains(name) {
            return Some(LexListArg::Subterm { strict: true });
        }
        return None;
    }
    // Filtered: `g(s, …)` where `s` is the param or its cons-tail and `g`
    // is a non-growing list filter defined in the program.
    if let Expr::FnCall(callee, args) = &arg.node {
        let dotted = expr_to_dotted_name(callee)?;
        let bare = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
        let first = args.first()?;
        let s = local_name_of(first)?;
        let s_ok = s == caller_list_param || tail_binders.contains(s);
        if !s_ok {
            return None;
        }
        let helper_fd = find_user_fn_by_name(ctx, &bare)?;
        // Kernel-provable non-growing filter shape required.
        lex_list_filter_helper(helper_fd, ctx)?;
        // Render the call's actual args as Lean — these instantiate the
        // synthesised `<g>_len_le` lemma in `decreasing_by`.
        let lemma_args = args
            .iter()
            .map(|a| super::expr::emit_expr_legacy(a, ctx, None))
            .collect::<Vec<_>>()
            .join(" ");
        return Some(LexListArg::Filtered {
            helper: bare,
            lemma_args,
        });
    }
    None
}

/// Locate a user fn def by bare name across entry + dep-module scopes.
fn find_user_fn_by_name<'a>(ctx: &'a CodegenContext, name: &str) -> Option<&'a FnDef> {
    ctx.fn_defs
        .iter()
        .chain(ctx.modules.iter().flat_map(|m| m.fn_defs.iter()))
        .find(|fd| fd.name == name)
}

/// Recognise a mutual SCC whose every recursive call decreases either by a
/// structural subterm (cons-tail) or through a non-growing list filter,
/// and assign each member a lex measure `(list_param.length + offset, rank)`
/// that discharges every obligation. Returns the per-member plans + the
/// distinct length lemmas to synthesise, or `None` to back off to fuel.
fn recognize_lex_list_wf_scc<'a>(
    fns: &'a [&'a FnDef],
    ctx: &CodegenContext,
) -> Option<(Vec<LexListMemberPlan<'a>>, Vec<LengthLemma>)> {
    let names: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    if fns.len() < 2 {
        // Single-fn "mutual" SCCs are emitted by the self-recursive path,
        // not here; this synthesiser targets genuine mutual blocks.
        return None;
    }

    // Per-fn body-derived classified edges (list param + tail binders are
    // consumed inline to classify each edge; only the edges survive).
    struct Raw<'a> {
        fd: &'a FnDef,
        edges: Vec<LexListEdge>,
    }
    let mut raws: Vec<Raw<'a>> = Vec::new();
    for fd in fns {
        if !is_pure_fn(fd) {
            return None;
        }
        let (_idx, list_param) = lex_list_measure_param(fd, ctx)?;
        let tail_binders =
            crate::codegen::recursion::detect::collect_list_tail_binders(fd, &list_param);
        let mut edges: Vec<LexListEdge> = Vec::new();
        for (callee_raw, args) in
            crate::codegen::recursion::detect::collect_calls_from_body(fd.body.as_ref())
        {
            let Some(callee) =
                crate::codegen::recursion::detect::canonical_callee_name(&callee_raw, &names)
            else {
                continue;
            };
            // The callee's measure-list arg position (its single List param).
            let callee_fd = fns.iter().find(|f| f.name == callee)?;
            let (callee_list_idx, _) = lex_list_measure_param(callee_fd, ctx)?;
            let arg = args.get(callee_list_idx)?;
            let classified = classify_lex_list_arg(arg, &list_param, &tail_binders, ctx)?;
            edges.push(LexListEdge {
                callee,
                arg: classified,
            });
        }
        if edges.is_empty() {
            // A member with no intra-SCC recursive call isn't part of the
            // recursion this synthesiser models.
            return None;
        }
        raws.push(Raw { fd, edges });
    }

    // Offset propagation along subterm edges: a strict cons-tail edge f→c
    // makes `len_c = len_f - 1`, so `off_c = off_f + 1` keeps the lex first
    // component equal and the rank tie-break carries the decrease. A
    // non-strict subterm edge (param verbatim) keeps `off_c = off_f`.
    // Filter edges must end up STRICTLY lower in offset (their length only
    // weakly shrinks, so the first component must drop via offset).
    let mut offset: HashMap<String, usize> = HashMap::new();
    offset.insert(raws[0].fd.name.clone(), 0);
    // Fixed-point propagation (SCC has ≤ small node count).
    let mut changed = true;
    let mut iterations = 0;
    while changed {
        changed = false;
        iterations += 1;
        if iterations > raws.len() * raws.len() + 4 {
            // Non-convergent (cyclic strict subterm path) — back off.
            return None;
        }
        for raw in &raws {
            let Some(&off_f) = offset.get(&raw.fd.name) else {
                continue;
            };
            for edge in &raw.edges {
                if let LexListArg::Subterm { strict } = &edge.arg {
                    let want = if *strict { off_f + 1 } else { off_f };
                    match offset.get(&edge.callee) {
                        Some(&existing) if existing == want => {}
                        Some(_) => return None, // conflicting offset → back off
                        None => {
                            offset.insert(edge.callee.clone(), want);
                            changed = true;
                        }
                    }
                }
            }
        }
        // Seed any node not yet reached by a subterm edge with offset 0.
        for raw in &raws {
            if !offset.contains_key(&raw.fd.name) {
                offset.insert(raw.fd.name.clone(), 0);
                changed = true;
            }
        }
    }

    // Every filter edge f→c must satisfy `off_c < off_f` so the first
    // component strictly decreases (filter only proves `≤`, never `<`).
    for raw in &raws {
        let off_f = offset[&raw.fd.name];
        for edge in &raw.edges {
            if matches!(edge.arg, LexListArg::Filtered { .. }) {
                let off_c = offset[&edge.callee];
                if off_c >= off_f {
                    return None;
                }
            }
        }
    }

    // Ranks: subterm edges must tie-break, so a strict subterm edge f→c
    // needs `rank_c < rank_f`. Assign `rank = max_off - off` so a `+1`
    // offset step (strict subterm) lowers the rank by exactly 1.
    let max_off = offset.values().copied().max().unwrap_or(0);
    let mut plans: Vec<LexListMemberPlan<'a>> = Vec::new();
    for raw in raws {
        let off = offset[&raw.fd.name];
        let rank = max_off - off;
        let (_idx, list_param_lean) = lex_list_measure_param(raw.fd, ctx)?;
        plans.push(LexListMemberPlan {
            fd: raw.fd,
            list_param_lean,
            offset: off,
            rank,
            edges: raw.edges,
        });
    }

    // Validate the rank tie-break for every subterm edge.
    let rank_of: HashMap<String, usize> =
        plans.iter().map(|p| (p.fd.name.clone(), p.rank)).collect();
    for plan in &plans {
        for edge in &plan.edges {
            if let LexListArg::Subterm { strict } = &edge.arg {
                if *strict {
                    // strict subterm: equal first component, need rank to drop.
                    if rank_of[&edge.callee] >= plan.rank {
                        return None;
                    }
                } else {
                    // non-strict subterm (param verbatim, no shrink at all):
                    // first component equal AND rank equal would loop. We
                    // can't prove termination on a no-shrink self/peer call.
                    return None;
                }
            }
        }
    }

    // Distinct length lemmas referenced by any filter edge.
    let mut lemmas: Vec<LengthLemma> = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    for plan in &plans {
        for edge in &plan.edges {
            if let LexListArg::Filtered { helper, .. } = &edge.arg {
                if !seen.insert(helper.clone()) {
                    continue;
                }
                let helper_fd = find_user_fn_by_name(ctx, helper)?;
                let helper_lean = aver_name_to_lean(helper);
                let params = emit_fn_params(&helper_fd.params);
                let param_names: Vec<String> = helper_fd
                    .params
                    .iter()
                    .map(|(n, _)| aver_name_to_lean(n))
                    .collect();
                lemmas.push(LengthLemma {
                    helper_lean,
                    params,
                    param_names,
                });
            }
        }
    }

    Some((plans, lemmas))
}

/// Emit the kernel-proved length-monotonicity lemma for one filter helper:
/// `theorem <h>_len_le : ∀ <params>, (<h> <args>).length ≤ <list_param>.length`.
/// The proof is the validated `induction … split … List.length_cons; omega`.
fn emit_length_lemma(lemma: &LengthLemma) -> String {
    let first = &lemma.param_names[0];
    let rest_args = lemma.param_names[1..].join(" ");
    let call = if rest_args.is_empty() {
        format!("{} {}", lemma.helper_lean, first)
    } else {
        format!("{} {} {}", lemma.helper_lean, first, rest_args)
    };
    // `∀`-quantify exactly the helper's params, in declared order.
    let forall_binders = lemma.params.clone();
    let intro_names = lemma.param_names.join(" ");
    let mut lines = Vec::new();
    lines.push(format!(
        "theorem {} : ∀ {}, ({}).length ≤ {}.length := by",
        lemma.lemma_name(),
        forall_binders,
        call,
        first
    ));
    lines.push(format!("  intro {}", intro_names));
    lines.push(format!("  induction {} with", first));
    lines.push(format!("  | nil => simp [{}]", lemma.helper_lean));
    lines.push(format!(
        "  | cons x rest ih => simp only [{}]; split",
        lemma.helper_lean
    ));
    lines.push("                      · simp only [List.length_cons]; omega".to_string());
    lines.push("                      · simp only [List.length_cons]; omega".to_string());
    lines.join("\n")
}

/// Emit a genuine well-founded mutual block for a length-monotonicity SCC:
/// the synthesised + kernel-proved length lemmas, then the `mutual … end`
/// block with per-def lex `termination_by (list.length + offset, rank)` and
/// `decreasing_by` clauses citing the lemmas. Returns `None` (back off to
/// fuel) when the SCC isn't recognised as length-monotone-WF.
fn emit_native_mutual_lex_list_wf_group(fns: &[&FnDef], ctx: &CodegenContext) -> Option<String> {
    // NB: unlike `emit_native_mutual_sizeof_group`, this path does NOT gate
    // on `scc_has_growing_accumulator` — a computed filter arg
    // (`smallerOrEqual rest pivot`) trips that conservative check, and
    // proving it shrinks via a synthesised length lemma is exactly the job
    // here. The classifier below rejects anything it can't prove.
    let (plans, lemmas) = recognize_lex_list_wf_scc(fns, ctx)?;

    let mut lines: Vec<String> = Vec::new();
    // Kernel-proved length lemmas first — they reference the filter helpers
    // emitted earlier in topological order, and are cited by `decreasing_by`.
    for lemma in &lemmas {
        lines.push(emit_length_lemma(lemma));
        lines.push(String::new());
    }

    lines.push("mutual".to_string());
    for plan in &plans {
        let fd = plan.fd;
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body_fn = lowered.as_ref().unwrap_or(fd);
        let body_ast = lowered
            .as_ref()
            .map(|l| l.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        let body = emit_fn_body_for(body_fn, body_ast, ctx);

        lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        for body_line in body.lines() {
            lines.push(format!("  {body_line}"));
        }
        let measure_first = if plan.offset == 0 {
            format!("{}.length", plan.list_param_lean)
        } else {
            format!("{}.length + {}", plan.list_param_lean, plan.offset)
        };
        lines.push(format!(
            "  termination_by ({}, {})",
            measure_first, plan.rank
        ));
        // One `decreasing_by` goal per recursive call, in body source order.
        lines.push("  decreasing_by".to_string());
        for edge in &plan.edges {
            match &edge.arg {
                LexListArg::Subterm { .. } => {
                    // Equal first component, decrease lives in the rank — a
                    // `Prod.Lex` goal omega can't discharge directly.
                    lines.push("    · simp_wf; exact Prod.Lex.right _ (by omega)".to_string());
                }
                LexListArg::Filtered { helper, lemma_args } => {
                    let lemma_name = format!("{}_len_le", aver_name_to_lean(helper));
                    // Cite the lemma at exactly the call's arguments so omega
                    // can relate `(g rest pivot).length` to `rest.length`.
                    lines.push(format!(
                        "    · simp_wf; have := {} {}; omega",
                        lemma_name, lemma_args
                    ));
                }
            }
        }
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}

/// Native termination emission for mutual-recursion SCCs whose
/// every member has a sizeOf measure (List / Vector / String) and a
/// classifier rank — Lean 4 `mutual ... end` block with one
/// `termination_by` per def, lex tuple `(sizeOf_sum, rank)` from
/// `MutualSizeOfRanked`. Mirrors the Dafny native path from #83.
///
/// Returns `None` when:
/// - SCC isn't fully `MutualSizeOfRanked` (caller picks fuel)
/// - Any member has no inferable sizeOf measure
/// - Growing-accumulator pattern detected (tail-rec `[x] + acc`
///   shapes won't decrease the lex tuple)
fn emit_native_mutual_sizeof_group(fns: &[&FnDef], ctx: &CodegenContext) -> Option<String> {
    let mut ranks: HashMap<String, usize> = HashMap::new();
    for fd in fns {
        if !is_pure_fn(fd) {
            return None;
        }
        // MutualSizeOfRanked carries `params: vec![]` + rank>=1; any
        // other Lex shape (single-param mutual int-countdown, two-
        // param string-pos) fails this group's pre-conditions.
        match contract_lex_params_rank(ctx, fd) {
            Some(([], rank)) => {
                ranks.insert(fd.name.clone(), rank);
            }
            _ => return None,
        }
    }
    let mut measures: HashMap<String, String> = HashMap::new();
    for fd in fns {
        let measure = emit_native_termination_measure(fd, ctx)?;
        measures.insert(fd.name.clone(), measure);
    }
    if crate::codegen::recursion::detect::scc_has_growing_accumulator(fns) {
        return None;
    }

    let mut lines: Vec<String> = vec!["mutual".to_string()];
    for fd in fns {
        let measure = measures.get(&fd.name).unwrap();
        let rank = ranks.get(&fd.name).unwrap();
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body_fn = lowered.as_ref().unwrap_or(fd);
        let body_ast = lowered
            .as_ref()
            .map(|l| l.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        let body = emit_fn_body_for(body_fn, body_ast, ctx);

        lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        for body_line in body.lines() {
            lines.push(format!("  {body_line}"));
        }
        lines.push(format!("  termination_by ({measure}, {rank})"));
        // Robust tactic chain — `decreasing_tactic` alone bottoms out
        // on simple shapes (BigInt) but Lean elaborator on multi-arg
        // mutual SCCs sometimes needs `simp_wf` to unfold sizeOf
        // before omega can close the arithmetic on lengths.
        lines.push(
            "  decreasing_by all_goals (first | decreasing_tactic | (simp_wf; (try simp_all); first | omega | (constructor <;> first | rfl | omega)))"
                .to_string(),
        );
        lines.push(String::new());
    }
    lines.push("end".to_string());
    Some(lines.join("\n"))
}

fn emit_fuelized_mutual_sizeof_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let targets: HashSet<String> = fns.iter().map(|fd| fd.name.clone()).collect();
    let recursive_types: HashSet<String> = ctx
        .modules
        .iter()
        .flat_map(|m| m.type_defs.iter())
        .chain(ctx.type_defs.iter())
        .filter(|td| is_recursive_type_def(td))
        .map(|td| type_def_name(td).to_string())
        .collect();
    let rank_budget = fns
        .iter()
        .filter_map(|fd| contract_lex_rank(ctx, fd))
        .max()
        .unwrap_or(1)
        + 1;

    let mut helper_lines = vec!["mutual".to_string()];
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        let helper_name = fuel_helper_name(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = ret_type_or_unit(fd);
        let rewritten = rewrite_recursive_calls_body(&fd.body, &targets, STRING_POS_FUEL_VAR);
        let body = emit_fn_body_for(fd, &rewritten, ctx);

        helper_lines.extend(
            emit_doc_comment(&fd.desc)
                .into_iter()
                .map(|line| format!("  {line}")),
        );
        helper_lines.extend(emit_fuel_helper_def(
            &helper_name,
            &params,
            &ret_type,
            &body,
            "  ",
        ));
        helper_lines.push(String::new());
    }
    helper_lines.push("end".to_string());

    let wrapper_lines: Vec<String> = fns
        .iter()
        .filter(|fd| is_pure_fn(fd))
        .flat_map(|fd| {
            let helper_name = fuel_helper_name(&fd.name);
            let mut lines =
                emit_mutual_sizeof_wrapper(fd, &helper_name, rank_budget, &recursive_types);
            lines.push(String::new());
            lines
        })
        .collect();

    [helper_lines, vec![String::new()], wrapper_lines]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join("\n")
}

/// Emit a Lean 4 function definition from an Aver FnDef.
/// Returns `None` if the function should be skipped (effectful, main).
pub fn emit_fn_def(
    fd: &FnDef,
    recursive_fns: &HashSet<String>,
    ctx: &CodegenContext,
) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    let mut lines = Vec::new();

    // Doc comment from description
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", sanitize_doc(desc)));
    }

    let is_recursive = recursive_fns.contains(&fd.name);
    let fn_name = aver_name_to_lean(&fd.name);

    // Parameters — lifted fn keeps the plain function type for oracle
    // bindings; the subtype constraint is enforced at the lemma level
    // (`∀ rng : RandomIntInBounds, ...`) where it bites the universal
    // claim. Threading the subtype through the operational signature
    // would force every sample binding (`theorem ..._sample_1`) to
    // wrap concrete stubs in `⟨stub, by sorry⟩`, since most user
    // stubs (e.g. `counterStub : fn p n min max := n + min`) only
    // satisfy the bound at specific `(min, max)` pairs and `decide`
    // can't discharge the `∀ min max` quantifier. Sound for the
    // universal lemma, executable for the concrete sample — that's
    // the trade.
    let params = emit_fn_params(&fd.params);

    // Return type
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };

    // partial for recursive functions
    let prefix = if is_recursive { "partial " } else { "" };

    lines.push(format!(
        "{}def {} {} : {} :=",
        prefix, fn_name, params, ret_type
    ));
    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    lines.push(emit_fn_body_for(fd, body, ctx));

    Some(lines.join("\n"))
}

/// Proof-mode function emission. Reads the contract decision from
/// `ctx.proof_ir.fn_contracts` and dispatches to the matching emit fn
/// (native guarded, fuel-encoded, pair-state Nat worker, etc.). Falls
/// back to plain `def` emission when no contract is present (non-
/// recursive fn).
pub fn emit_fn_def_proof(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    if !is_pure_fn(fd) {
        return None;
    }

    // LinearRecurrence2 — dedicated `RecursionContract::LinearRecurrence2`
    // marker. Backend still calls `detect_second_order_int_linear_
    // recurrence` to extract base cases + coefficients; the contract
    // just signals "this fn lowers as pair-state Nat worker, not fuel".
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::LinearRecurrence2)
        )
        && let Some(shape) = detect_second_order_int_linear_recurrence(fd)
    {
        return Some(emit_nat_linear_recurrence_fn(fd, &shape, ctx));
    }

    // IntCountdown now reads through ProofIR's `Fuel { NatAbsPlusOne }`
    // contract. Fuel encoding stays — native `termination_by n.natAbs`
    // would require `(n - 1).natAbs < n.natAbs` which only holds for
    // `n > 0`; Aver bodies don't always clamp to non-negative before
    // recursing (fibTR sans-guard relies on its caller). Fuel
    // sidesteps the issue.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::NatAbsPlusOne { param },
        }) = contract.recursion.as_ref()
        && let Some(param_index) = fd.params.iter().position(|(n, _)| n == param)
    {
        return Some(emit_fuelized_int_countdown_fn(fd, ctx, param_index));
    }

    // WellFoundedToNat — native well-founded def on `param.toNat`.
    // Two validated sources (see the contract docs): the
    // guard-validated floor-division countdown (`floor_div: Some`)
    // and the guarded subtractive countdown a floor-division window
    // law graduated out of fuel (`floor_div: None`). The kernel
    // re-checks the measure through `decreasing_by`: the branch
    // hypotheses of the emitted if/else chain land in the decreasing
    // goals' context, `simp [<wrapper>, Except.withDefault]` reduces
    // the literal-divisor zero-guard, and `omega` (which understands
    // `Int.toNat` and ediv by literals) closes the strict decrease.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::WellFoundedToNat { param, floor_div }) =
            contract.recursion.as_ref()
    {
        let mut lines = Vec::new();
        if let Some(desc) = &fd.desc {
            lines.push(format!("/-- {} -/", sanitize_doc(desc)));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        lines.push(format!("def {} {} : {} :=", fn_name, params, ret_type));
        let lowered = lower_pure_question_bang_for_emit(fd);
        let body = lowered
            .as_ref()
            .map(|lowered_fd| lowered_fd.body.as_ref())
            .unwrap_or(fd.body.as_ref());
        lines.push(emit_fn_body_for(fd, body, ctx));
        lines.push(format!("termination_by {}.toNat", aver_name_to_lean(param)));
        lines.push("decreasing_by".to_string());
        match floor_div {
            Some(shrink) => match &shrink.helper_fn {
                Some(helper) => lines.push(format!(
                    "  all_goals (simp [{}, Except.withDefault] <;> omega)",
                    aver_name_to_lean(helper)
                )),
                None => lines.push("  all_goals (simp [Except.withDefault] <;> omega)".to_string()),
            },
            None => lines.push("  all_goals omega".to_string()),
        }
        return Some(lines.join("\n"));
    }

    // IntCountdownGuarded now reads through ProofIR — the lowerer
    // populates `ctx.proof_ir.fn_contracts` with a `Native` contract
    // whose `precondition` + `body` carry everything the emit needs.
    // Other RecursionPlan variants still flow through `recursion_plan`
    // directly; Step 7+ migrates them one shape at a time.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Native {
            precondition,
            measure: crate::ir::Measure::NatAbsInt { param },
            body,
            ..
        }) = contract.recursion.as_ref()
    {
        // Measure binds the countdown param by name; map back to the
        // arg-position index the emit fn expects. Falls through if the
        // param somehow vanished (shouldn't happen — populator just
        // pulled it from fd.params).
        if let Some(param_index) = fd.params.iter().position(|(n, _)| n == param) {
            let precondition_clauses: Vec<crate::ast::Spanned<crate::ir::hir::ResolvedExpr>> =
                precondition.iter().map(|p| p.expr.clone()).collect();
            return Some(emit_native_guarded_int_countdown_fn(
                fd,
                ctx,
                param_index,
                body.base_arm_literal,
                &body.base_arm_body,
                &body.wildcard_arm_body,
                &precondition_clauses,
            ));
        }
    }

    // IntAscending reads `Fuel { BoundMinusParamNatAbsPlusOne }`.
    // The bound stays as `Spanned<Expr>` in the contract; backend
    // renders it through `bound_expr_to_lean` here.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::BoundMinusParamNatAbsPlusOne { param, bound },
        }) = contract.recursion.as_ref()
        && let Some(param_index) = fd.params.iter().position(|(n, _)| n == param)
    {
        let bound_lean = super::bound_expr_to_lean(bound);
        return Some(emit_fuelized_int_ascending_fn(
            fd,
            ctx,
            param_index,
            &bound_lean,
        ));
    }

    // SizeOfStructural — `Fuel { SizeOfPlusOne }`. The classifier only assigns
    // this contract when the recursion strictly shrinks a recursive sub-term
    // binder (`supports_single_sizeof_structural`), i.e. it is genuine
    // structural recursion on the user ADT's immediate sub-fields. Lean's
    // equation compiler accepts exactly that natively, so we emit a plain `def`
    // (fall through below) and let Lean infer structural termination — NO fuel.
    //
    // This is strictly better than the old fuel helper: a plain structural `def`
    // has DEFINITIONAL recursive equations (`height (Node l y r) = …` is `rfl`),
    // whereas a fuel counter destroys that (the fuel arg on a child differs from
    // the child's own measure, so `simp [f]`/`omega` can't unfold it for
    // symbolic/universal proofs — the very reason fuel forced the universal law
    // to be skipped, Issue #128). Empirically (Lean 4.15) naive structural also
    // covers mutual / accumulator / lexicographic recursion; only recursion
    // hidden inside a higher-order container combinator (e.g. `kids.map f` over a
    // nested-recursive field) needs an explicit well-founded measure, and that
    // shape is never classified SizeOfStructural.
    //
    // The Peano-lift case already fell through here for the same reason
    // (`recurses_on_peano` → structural on `Nat.rec`); it now shares the path.

    // StringPosAdvance — `Fuel { StringLenMinusPos { string, pos } }`.
    // Lean's emit reads the params from fd.params directly so the
    // contract just acts as the dispatch signal.
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd)
        && matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::StringLenMinusPos { .. },
            })
        )
    {
        return Some(emit_fuelized_string_pos_fn(fd, ctx));
    }

    let mut lines = Vec::new();
    if let Some(desc) = &fd.desc {
        lines.push(format!("/-- {} -/", sanitize_doc(desc)));
    }

    let fn_name = aver_name_to_lean(&fd.name);
    let params = emit_fn_params(&fd.params);
    let ret_type = if fd.return_type.is_empty() {
        "Unit".to_string()
    } else {
        type_annotation_to_lean(&fd.return_type)
    };
    lines.push(format!("def {} {} : {} :=", fn_name, params, ret_type));
    let lowered = lower_pure_question_bang_for_emit(fd);
    let body = lowered
        .as_ref()
        .map(|lowered_fd| lowered_fd.body.as_ref())
        .unwrap_or(fd.body.as_ref());
    lines.push(emit_fn_body_for(fd, body, ctx));

    // termination_by/decreasing_by suffix for the few contract shapes
    // that need explicit Lean termination hints (rest are no-ops —
    // their emit fns already wrote them, or Lean's elaborator infers).
    if let Some(contract) = crate::codegen::common::find_fn_contract_for_fn(ctx, fd) {
        match contract.recursion.as_ref() {
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::Lex { params, rank: 0 },
            }) if params.len() == 1 => {
                // MutualIntCountdown — every member counts down the
                // shared first-Int param.
                let lean_param = aver_name_to_lean(&params[0]);
                lines.push(format!("termination_by Int.natAbs {}", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  omega".to_string());
            }
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::SeqLenPlusOne { param },
            }) => {
                // ListStructural — Lean structural recursion on
                // `<param>.length`. The `+1` framing in the IR is
                // ignored here; Lean's elaborator wants the bare
                // length measure.
                let lean_param = aver_name_to_lean(param);
                lines.push(format!("termination_by {}.length", lean_param));
                lines.push("decreasing_by".to_string());
                lines.push("  decreasing_tactic".to_string());
            }
            _ => {}
        }
    }

    Some(lines.join("\n"))
}

fn emit_fn_params(params: &[(String, String)]) -> String {
    params
        .iter()
        .map(|(name, type_ann)| {
            let lean_type = type_annotation_to_lean(type_ann);
            let lean_name = aver_name_to_lean(name);
            format!("({} : {})", lean_name, lean_type)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn lower_pure_question_bang_for_emit(fd: &FnDef) -> Option<FnDef> {
    crate::types::checker::effect_lifting::lower_pure_question_bang_fn(fd)
        .ok()
        .flatten()
}

fn expr_uses_error_prop(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::ErrorProp(_) => true,
        Expr::FnCall(callee, args) => {
            expr_uses_error_prop(callee) || args.iter().any(expr_uses_error_prop)
        }
        Expr::Attr(obj, _) => expr_uses_error_prop(obj),
        Expr::BinOp(_, left, right) => expr_uses_error_prop(left) || expr_uses_error_prop(right),
        Expr::Neg(inner) => expr_uses_error_prop(inner),
        Expr::Match { subject, arms, .. } => {
            expr_uses_error_prop(subject) || arms.iter().any(|arm| expr_uses_error_prop(&arm.body))
        }
        Expr::Constructor(_, Some(inner)) => expr_uses_error_prop(inner),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            StrPart::Parsed(expr) => expr_uses_error_prop(expr),
            StrPart::Literal(_) => false,
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            items.iter().any(expr_uses_error_prop)
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(key, value)| expr_uses_error_prop(key) || expr_uses_error_prop(value)),
        Expr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, value)| expr_uses_error_prop(value))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_error_prop(base)
                || updates.iter().any(|(_, value)| expr_uses_error_prop(value))
        }
        Expr::TailCall(boxed) => boxed.args.iter().any(expr_uses_error_prop),
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } | Expr::Constructor(_, None) => {
            false
        }
    }
}

fn body_uses_error_prop(body: &FnBody) -> bool {
    body.stmts().iter().any(|stmt| match stmt {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_uses_error_prop(expr),
    })
}

/// Typed-HIR query: does this fn return `Result<_, _>`?
///
/// Epic #180 Phase 4 — reads the canonical type stamped on the
/// resolved fn def directly instead of re-parsing the AST return
/// type string. The typechecker has already produced the typed
/// surface; backends just consume it.
fn fn_returns_result_typed(rfd: &crate::ir::hir::ResolvedFnDef) -> bool {
    matches!(rfd.return_type, crate::types::Type::Result(_, _))
}

/// Emit one statement inside a Lean `do` block (used when the fn
/// body must thread `ErrorProp` through Lean's monadic chain).
///
/// **Epic #170 Phase 5 PR E2**: resolves each stmt once at the
/// boundary through `ctx.resolve_stmt` (scope-aware) and routes the
/// inner expression through `emit_expr` (resolved) instead of the
/// `temporary-migration-bridge` `emit_expr_legacy` adapter. Keeps the
/// fn-body emit path off the legacy resolve-on-demand surface in the
/// hot path; the remaining `emit_expr_legacy` callsites in this
/// module are all in proof-mode law/verify rewriters where the
/// upstream rewriter still produces raw AST.
fn emit_do_stmt(stmt: &Stmt, ctx: &CodegenContext, is_last: bool) -> String {
    use crate::ir::hir::ResolvedStmt;
    let scope = ctx.active_module_scope();
    let scope_ref = scope.as_deref();
    // Detect `ErrorProp(inner)` BEFORE resolve so we can route the
    // unwrapped inner through the monadic-bind / direct-emit branches
    // the same way the legacy path did.
    let (is_err_prop, target_for_resolve): (bool, std::borrow::Cow<'_, Spanned<Expr>>) = match stmt
    {
        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
            if let Expr::ErrorProp(inner) = &expr.node {
                (true, std::borrow::Cow::Owned((**inner).clone()))
            } else {
                (false, std::borrow::Cow::Borrowed(expr))
            }
        }
    };
    let resolved_expr = ctx.resolve_expr(target_for_resolve.as_ref(), scope_ref);
    let expr_str = super::expr::emit_expr(&resolved_expr, ctx);
    match (stmt, is_err_prop, is_last) {
        (Stmt::Binding(name, _, _), true, _) => {
            format!("  let {} <- {}", aver_name_to_lean(name), expr_str)
        }
        (Stmt::Binding(name, _, _), false, _) => {
            // Re-resolve the full stmt so the inner expression keeps
            // its proper `ResolvedStmt::Binding` shape (preserves
            // the type annotation if it was present).
            let resolved_stmt = ctx.resolve_stmt(stmt, scope_ref);
            if let ResolvedStmt::Binding { name: n, value, .. } = &resolved_stmt {
                format!(
                    "  let {} := {}",
                    aver_name_to_lean(n),
                    super::expr::emit_expr(value, ctx)
                )
            } else {
                format!("  let {} := {}", aver_name_to_lean(name), expr_str)
            }
        }
        (Stmt::Expr(_), true, true) => format!("  {}", expr_str),
        (Stmt::Expr(_), true, false) => format!("  let _ <- {}", expr_str),
        (Stmt::Expr(_), false, true) => format!("  {}", expr_str),
        (Stmt::Expr(_), false, false) => format!("  let _ := {}", expr_str),
    }
}

/// Emit a Lean fn body (plain — no `do` notation).
///
/// **Epic #170 Phase 5 PR E2**: resolves each top-level stmt once at
/// the boundary instead of calling the legacy adapter per expression.
/// Same migration shape as [`emit_do_stmt`].
fn emit_fn_body(body: &FnBody, ctx: &CodegenContext) -> String {
    use crate::ir::hir::ResolvedStmt;
    let scope = ctx.active_module_scope();
    let scope_ref = scope.as_deref();
    let stmts = body.stmts();
    let mut lines = Vec::new();
    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == stmts.len() - 1;
        let resolved_stmt = ctx.resolve_stmt(stmt, scope_ref);
        match &resolved_stmt {
            ResolvedStmt::Binding { name, value, .. } => {
                lines.push(format!(
                    "  let {} := {}",
                    aver_name_to_lean(name),
                    super::expr::emit_expr(value, ctx)
                ));
            }
            ResolvedStmt::Expr(expr) => {
                if is_last {
                    lines.push(format!("  {}", super::expr::emit_expr(expr, ctx)));
                } else {
                    lines.push(format!("  let _ := {}", super::expr::emit_expr(expr, ctx)));
                }
            }
        }
    }
    lines.join("\n")
}

fn emit_fn_body_result_do(body: &FnBody, ctx: &CodegenContext) -> String {
    let stmts = body.stmts();
    let mut lines = vec!["  do".to_string()];
    for (i, stmt) in stmts.iter().enumerate() {
        lines.push(emit_do_stmt(stmt, ctx, i == stmts.len() - 1));
    }
    lines.join("\n")
}

fn emit_fn_body_for(fd: &FnDef, body: &FnBody, ctx: &CodegenContext) -> String {
    // Pointer-eq scope (`fn_id_for_decl`) → resolved view by `FnId`
    // so a same-bare-name entry/dep twin never accidentally
    // provides this fn's return type. Synthetic FnDefs (TCO hoists,
    // mid-rewrite fns) the resolver never saw fall through to
    // `ctx.resolve_fn_def`'s on-demand lift.
    let resolved_fd = crate::codegen::common::fn_id_for_decl(ctx, fd)
        .and_then(|id| ctx.resolved_program.fn_by_id(id));
    let resolved_owned = match resolved_fd {
        Some(_) => None,
        None => Some(ctx.resolve_fn_def(fd, None)),
    };
    let rfd: &crate::ir::hir::ResolvedFnDef =
        resolved_fd.unwrap_or_else(|| resolved_owned.as_ref().unwrap().as_ref());
    if fn_returns_result_typed(rfd) && body_uses_error_prop(body) {
        emit_fn_body_result_do(body, ctx)
    } else {
        emit_fn_body(body, ctx)
    }
}

/// Emit verify blocks as Lean 4 `example` declarations.
///
/// `native_decide` gives executable proof checks for decidable goals.
/// `sorry` is available as explicit fallback mode.
pub fn emit_verify_block(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    case_index_start: usize,
) -> (String, usize) {
    if let VerifyKind::Law(law) = &vb.kind {
        return emit_verify_law_block(vb, law, ctx, verify_mode, case_index_start);
    }

    // Oracle v1: `verify fn trace` cases-form — mix of provable and
    // runtime-only assertions. `.result` projections reduce to
    // `lifted_fn(path, oracle...) = expected` which IS provable;
    // `.trace.length / .event / .contains / .group(..)` project
    // over a runtime-only event buffer that the lifted fn doesn't
    // carry. Emit provable ones as formal examples, comment out the
    // rest with a pointer to docs/oracle.md. Both are checked at
    // runtime via `aver verify` regardless.
    if vb.trace {
        return emit_verify_trace_block_proofs(vb, ctx, verify_mode, case_index_start);
    }

    let mut lines = Vec::new();
    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        let left_str = emit_expr_legacy(left, ctx, None);
        // Expected side: prefer the VM ground-truth literal over the source
        // RHS. A source RHS that calls a user fn (`verify f: f(x) => g(x)`)
        // routes BOTH sides through the model — vacuously true under fuel
        // exhaustion (panic returns `default` for both). The literal pins
        // the equation to the value the program actually computed. Cases
        // without an entry (verify failed/skipped, Float-carrying value —
        // decimal repr isn't bit-exact — or a shape that doesn't round-trip)
        // keep the source RHS and rely on the `--check` panic gate.
        let right_str = super::sample_literal::ground_truth_rhs(vb, ctx, case_index_start + idx)
            .unwrap_or_else(|| emit_expr_legacy(right, ctx, None));
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "example : {} = {} := by native_decide",
                    left_str, right_str
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!(
                    "example : {} = {} := by sorry",
                    left_str, right_str
                ));
            }
            VerifyEmitMode::TheoremSkeleton => {
                let theorem_name = format!(
                    "{}_verify_{}",
                    aver_name_to_lean(&vb.fn_name),
                    case_index_start + idx + 1
                );
                lines.push(format!(
                    "theorem {} : {} = {} := by",
                    theorem_name, left_str, right_str
                ));
                lines.push("  sorry".to_string());
            }
        }
    }
    (lines.join("\n"), case_index_start + vb.cases.len())
}

/// Oracle v1: emit proof-side assertions for a `verify fn trace`
/// cases-form block. Each case's LHS is inspected — `.result`
/// projections become `example : lifted_fn(root, oracle...) = rhs`,
/// which the auto-proof matcher closes via `simp [fn]` or
/// `native_decide` on concrete samples. Trace-buffer projections
/// (`.trace.length()`, `.event(k)`, `.contains(_)`, `.group(...)`)
/// stay runtime-only; emitted as comments so the proof file
/// still compiles.
fn emit_verify_trace_block_proofs(
    vb: &VerifyBlock,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    case_index_start: usize,
) -> (String, usize) {
    use crate::ast::Expr;
    let mut lines = Vec::new();
    let case_total = vb.cases.len();

    // Build a synthetic VerifyLaw for the rewriter — re-uses
    // `rewrite_effectful_calls_in_law` which wants a law handle to
    // look up given names. Cases-form trace blocks keep their givens
    // on `vb.cases_givens`; the rewriter only reads `law.givens`.
    let synthetic_law = crate::ast::VerifyLaw {
        name: String::new(),
        givens: vb.cases_givens.clone(),
        when: None,
        lhs: vb.cases.first().map(|(l, _)| l.clone()).unwrap_or_else(|| {
            crate::ast::Spanned::new(Expr::Literal(crate::ast::Literal::Unit), vb.line)
        }),
        rhs: vb.cases.first().map(|(_, r)| r.clone()).unwrap_or_else(|| {
            crate::ast::Spanned::new(Expr::Literal(crate::ast::Literal::Unit), vb.line)
        }),
        sample_guards: Vec::new(),
    };

    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        // Shape-detect the LHS. Only `.result` reduces to a formal
        // claim; everything else is runtime-only.
        let result_fn_call = match &left.node {
            Expr::Attr(inner, field) if field == "result" => match &inner.node {
                Expr::FnCall(_, _) => Some((**inner).clone()),
                _ => None,
            },
            _ => None,
        };

        let Some(fn_call) = result_fn_call else {
            let lhs_summary = emit_expr_legacy(left, ctx, None);
            lines.push(format!(
                "-- verify {} trace case {}/{}: `{}` is runtime-only (see docs/oracle.md)",
                vb.fn_name,
                idx + 1,
                case_total,
                lhs_summary,
            ));
            continue;
        };

        // Per-case bindings drive the oracle arg injection — pulls the
        // concrete stub value (e.g. `fairDie`) for each given.
        let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
        let mode = crate::codegen::common::OracleInjectionMode::SampleCaseBinding(case_bindings);
        let lhs_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            &fn_call,
            &synthetic_law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode.clone(),
        );
        let rhs_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            right,
            &synthetic_law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode,
        );

        let lhs_str = emit_expr_legacy(&lhs_rw, ctx, None);
        let rhs_str = emit_expr_legacy(&rhs_rw, ctx, None);

        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "example : {} = {} := by native_decide",
                    lhs_str, rhs_str
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!("example : {} = {} := by sorry", lhs_str, rhs_str));
            }
            VerifyEmitMode::TheoremSkeleton => {
                let theorem_name = format!(
                    "{}_trace_{}",
                    aver_name_to_lean(&vb.fn_name),
                    case_index_start + idx + 1
                );
                lines.push(format!(
                    "theorem {} : {} = {} := by",
                    theorem_name, lhs_str, rhs_str
                ));
                lines.push("  sorry".to_string());
            }
        }
    }

    (lines.join("\n"), case_index_start + case_total)
}

fn emit_verify_law_block(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    case_index_start: usize,
) -> (String, usize) {
    let mut lines = Vec::new();
    let fn_name = aver_name_to_lean(&vb.fn_name);
    let law_name = aver_name_to_lean(&law.name);
    // Oracle v1 — issue #127: a verify-trace-law's LHS that projects
    // through `.trace.{event,group,branch,length,contains}` describes
    // the runtime trace buffer, not the lifted fn's return. The lifted
    // Lean form has no `.trace` field — emitting `fn().trace.event 0`
    // as a theorem produces invalid-field-notation errors against the
    // bare return tuple, and the universal `∀ rnd, …` form is the
    // wrong shape anyway (the trace isn't a function of the oracle
    // alone, it's a function of the trace recorder). Emit only a
    // runtime-only marker — matches the cases-form trace block
    // behavior in `emit_verify_trace_block_proofs`. The `aver verify`
    // runtime path still exercises the law under the given stubs.
    if crate::codegen::common::law_lhs_has_trace_projection(&law.lhs) {
        let header = match canonical_spec_ref(&vb.fn_name, law, ctx) {
            Some(spec_ref) => format!(
                "-- verify law {}.spec {}: trace-projection LHS is runtime-only (see docs/oracle.md)",
                fn_name, spec_ref.spec_fn_name,
            ),
            None => format!(
                "-- verify law {}.{}: trace-projection LHS is runtime-only (see docs/oracle.md)",
                fn_name, law_name,
            ),
        };
        return (header, case_index_start + vb.cases.len());
    }
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, ctx);
    let theorem_base = match &spec_ref {
        Some(spec_ref) => format!(
            "{}_eq_{}",
            fn_name,
            aver_name_to_lean(&spec_ref.spec_fn_name)
        ),
        None => format!("{}_law_{}", fn_name, law_name),
    };
    // Oracle v1: rewrite calls to effectful fns in the law body so
    // they target the lifted form (see commit history in
    // `codegen/common.rs` / `codegen/dafny/toplevel.rs` for the
    // discovery that motivated this). Lemma body uses lemma-local
    // bindings; sample assertions use the concrete stub values.
    let law_lhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let law_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    // Refinement quantifier lift: when a given Int variable shows up
    // in the law body wrapped in a refinement-record constructor
    // (`Natural(value = a)`, `Positive(value = a)`, …), lift the
    // quantifier from the carrier type to the refined type so the
    // theorem statement reads `∀ (a : Natural), …` instead of
    // `∀ (a : Int), … Natural(value = a) …`. Strip the wrapper
    // constructor in the body templates so they read as
    // `add a b`, not `add (Natural(value = a)) (Natural(value = b))`.
    // The smart-constructor predicate ceases to be a per-case proof
    // obligation — it's already carried by the type.
    let mut lifted_vars: std::collections::HashMap<String, String> =
        std::collections::HashMap::new();
    for given in &law.givens {
        if let Some(refined) = crate::codegen::common::refinement_lift_for_given(
            &given.name,
            &given.type_name,
            &law_lhs,
            &law_rhs,
            ctx,
        ) {
            lifted_vars.insert(given.name.clone(), refined.to_string());
        }
    }
    let law_lhs = if lifted_vars.is_empty() {
        law_lhs
    } else {
        crate::codegen::common::strip_refinement_wrappers(&law_lhs, &lifted_vars, ctx)
    };
    let law_rhs = if lifted_vars.is_empty() {
        law_rhs
    } else {
        crate::codegen::common::strip_refinement_wrappers(&law_rhs, &lifted_vars, ctx)
    };
    let lhs_template = emit_expr_legacy(&law_lhs, ctx, None);
    let rhs_template = emit_expr_legacy(&law_rhs, ctx, None);
    // The `when` clause references the same oracle bindings the law
    // body does, so it needs the same subtype projection. Without this
    // a `when rng(root, 0, 1, 6) >= 1` clause would emit `rng ...`
    // against a `RandomIntInBounds` parameter and Lean would reject
    // the type mismatch.
    let when_template = law.when.as_ref().map(|expr| {
        let oracle_projected = crate::codegen::common::rewrite_effectful_calls_in_law(
            expr,
            law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
        );
        // Refinement lift: bare references to lifted-given idents
        // inside `when`'s comparator BinOps need `.val` projection
        // because the quantifier is now over the Subtype carrier,
        // not the underlying Int. Without this `when a >= 10` over
        // `a : Natural` emits as `a >= 10` and fails to synthesize
        // `LE Natural` / `OfNat Natural 10` in Lean.
        let val_projected =
            crate::codegen::common::project_lifted_idents_to_val(&oracle_projected, &lifted_vars);
        emit_expr_legacy(&val_projected, ctx, None)
    });
    let quant_params = law
        .givens
        .iter()
        .map(|given| {
            // Oracle v1 + 0.13 subtype encoding: classified Generative-
            // shaped effect-givens bind oracles through a subtype
            // carrier (`RandomIntInBounds` etc.) that pairs the function
            // with the bound proof. The quantifier uses the subtype, so
            // the law's claim is universally quantified over only those
            // oracles that respect the bound — strictly stronger than
            // `∀ rng : BranchPath → Int → ... → Int`. Other effect
            // kinds (Output, unclassified) keep the plain function
            // signature.
            // Refinement lift: quant param `a` lifts from `Int` to
            // the wrapping refinement record (`Natural`, `Positive`,
            // …) when the law body used `Natural(value = a)` etc.
            // See `refinement_lift_for_given` for the detection.
            let type_text = if let Some(refined) = lifted_vars.get(&given.name) {
                refined.clone()
            } else if let Some(subtype) = bounded_oracle_subtype_for(&given.type_name) {
                subtype.to_string()
            } else {
                match crate::types::checker::effect_classification::oracle_signature(
                    &given.type_name,
                ) {
                    Some(oracle_ty) => crate::codegen::lean::types::type_to_lean(&oracle_ty),
                    None => type_annotation_to_lean(&given.type_name),
                }
            };
            format!("({} : {})", aver_name_to_lean(&given.name), type_text)
        })
        .collect::<Vec<_>>()
        .join(" ");

    match &spec_ref {
        Some(spec_ref) => lines.push(format!(
            "-- verify law {}.spec {} ({} cases)",
            fn_name,
            spec_ref.spec_fn_name,
            vb.cases.len()
        )),
        None => lines.push(format!(
            "-- verify law {}.{} ({} cases)",
            fn_name,
            law_name,
            vb.cases.len()
        )),
    }
    for given in &law.givens {
        lines.push(format!(
            "-- given {}: {} = {}",
            aver_name_to_lean(&given.name),
            given.type_name,
            law_given_domain_to_lean(&given.domain, ctx)
        ));
    }
    if let Some(when_expr) = &law.when {
        // Flatten to one physical line: a `--` line comment only covers
        // the first line, so a multi-line premise (e.g. a nested Bool
        // match lowering to a multi-line `if/then/else`) would leak its
        // continuation lines out as stray Lean commands ("unexpected
        // token 'else'").
        let when_comment = emit_expr_legacy(when_expr, ctx, None).replace('\n', " ");
        lines.push(format!("-- when {when_comment}"));
    }
    // Issue #128: singleton-domain givens + RHS that references no
    // given + IR didn't pin a strategy that closes the constant-RHS
    // shape ⇒ the universal is vacuous or false (e.g.
    // `checkRight L V R = Tree.Black Empty 1 Empty`) and the
    // structural-induction fallback can't close it. Skip; sample +
    // checked_domain cover the point. Strategies that DO close
    // constant-RHS shapes (Reflexive, Commutative, Associative,
    // MapUpdatePostcondition, …) stay in the keep-set; Induction
    // / BackendDispatch / Sorry don't.
    let pinned_law_strategy = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name))
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .map(|t| &t.strategy);
    let ir_strategy_closes_const_rhs = pinned_law_strategy.is_some_and(|s| {
        !matches!(
            s,
            crate::ir::ProofStrategy::Induction { .. }
                | crate::ir::ProofStrategy::SimpOverLemmas(_)
                | crate::ir::ProofStrategy::BackendDispatch
                | crate::ir::ProofStrategy::Sorry
                // SimpOverPreludeLemmas is a best-effort rung with an
                // honest `sorry` floor — it does NOT promise to close
                // a constant-RHS shape, so a singleton-given +
                // constant-RHS law keeps today's skip (sample +
                // checked_domain cover the point) instead of gaining
                // a universal that would land as a caught sorry.
                | crate::ir::ProofStrategy::SimpOverPreludeLemmas { .. }
                // RingIdentity shares the prelude rung's honest-sorry
                // floor (`first | (…; done) | sorry`) — it does NOT
                // promise to close, so a singleton-given +
                // constant-RHS law keeps today's skip.
                | crate::ir::ProofStrategy::RingIdentity { .. }
                // IntDecimalRoundtrip shares the same honest-sorry
                // floor (`first | (…; done) | sorry`); its detector
                // also requires a given-dependent rhs, so the
                // singleton-const-rhs skip can't apply — listed for
                // the same conservatism.
                | crate::ir::ProofStrategy::IntDecimalRoundtrip { .. }
                // StringEscapeRoundtrip: same honest-sorry floor and
                // same given-dependent-rhs detector gate — listed for
                // the same conservatism.
                | crate::ir::ProofStrategy::StringEscapeRoundtrip(_)
        )
    });
    let singleton_const_rhs = !ir_strategy_closes_const_rhs
        && crate::codegen::common::all_givens_are_singletons(law)
        && crate::codegen::common::law_rhs_is_independent_of_givens(law);
    // Issue #128: a law that calls a fuel-bounded helper (a recursive
    // fn the proof-mode classifier rejected — `size`, `toSorted`,
    // `blackDepth`, …) can't be closed by the auto-proof matcher's
    // `induction t with …` chain: the goal stays under
    // `<fn>__fuel ((averMeasure _) * 3) …` which `simp` can't drive.
    // The expanded per-sample lemmas unfold fuel finitely (concrete
    // inputs) and stay decidable — skip the universal instead of
    // shipping `induction` tactics that don't close.
    //
    // EXCEPTION — `FiniteDomainCases`: closed enumeration defeats
    // fuel. The strategy's `cases` cascade reduces the universal to
    // ground goals over constant-measure constructor args, which
    // compute straight through `<fn>__fuel` wrappers (`rfl`/`decide`
    // evaluate them like the per-sample lemmas do). Skipping here
    // would drop the very theorems the strategy exists to close
    // (e.g. `parseEscape.escapeCodeRoundtrip`, whose `parseEscape`
    // is fuel-bounded), so the fuel gate does not apply.
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    let calls_fuel_bounded = crate::codegen::common::law_calls_unclassified_fn(law, &unclassified);
    let pinned_finite_domain_cases = matches!(
        pinned_law_strategy,
        Some(crate::ir::ProofStrategy::FiniteDomainCases { .. })
    );
    let skip_universal = singleton_const_rhs || (calls_fuel_bounded && !pinned_finite_domain_cases);
    if !quant_params.is_empty() && !skip_universal {
        lines.extend(emit_verify_law_support_theorems(
            vb,
            law,
            ctx,
            &theorem_base,
        ));
        let (theorem_prop, bounded_domain) = law_theorem_prop(
            law,
            ctx,
            &lhs_template,
            &rhs_template,
            when_template.as_deref(),
            &lifted_vars,
            false,
        );
        // Statement-class marker — the channel `aver proof --check`'s
        // `universal` metric keys on (see `LAW_CLASS_MARKER_PREFIX`).
        // Recorded HERE because this is where the statement was built:
        // `bounded_domain` says whether sampled-domain disjunction
        // premises bound the claim to the finite sample domain. For a
        // `replaces_theorem` auto-proof the strategy emits its own
        // (universal-form) statement; keeping this class for it is
        // conservative — a mislabel can only withhold credit, never
        // grant it. ONE exception flips the class the other way:
        // `FloorDivWindow` replaces the bounded statement with the
        // TRUE universal form `∀ givens, <when> = true -> claim`
        // (validated emission — the rendered file contains no
        // statement bounded by sampled domains for this law), so the
        // marker says `universal`. Credit stays fail-closed: the
        // `#print axioms` whitelist still decides, and a sorry'd or
        // native_decide'd proof can never be credited.
        let floor_window_universal = matches!(
            pinned_law_strategy,
            Some(crate::ir::ProofStrategy::FloorDivWindow { .. })
        );
        lines.push(format!(
            "{}{} {}",
            super::LAW_CLASS_MARKER_PREFIX,
            theorem_base,
            if bounded_domain && !floor_window_universal {
                super::LAW_CLASS_BOUNDED_DOMAIN
            } else {
                super::LAW_CLASS_UNIVERSAL
            }
        ));
        // Oracle v1: the auto-proof matchers compare law.lhs / law.rhs
        // ASTs. For effectful laws the theorem statement has been
        // rewritten to target the lifted fn (BranchPath.root() + oracle
        // args injected); the matchers need to see the same rewritten
        // form or they'll miss shapes like
        // `pickOne(root, rnd) == pickOneSpec(root, rnd)` and fall back
        // to `sorry`. Build a view of the law with the rewritten body.
        let law_for_auto_proof = crate::ast::VerifyLaw {
            name: law.name.clone(),
            givens: law.givens.clone(),
            when: law.when.clone(),
            lhs: law_lhs.clone(),
            rhs: law_rhs.clone(),
            sample_guards: law.sample_guards.clone(),
        };
        // (Removed: refinement_auto_proof — Aver-specific bypass.
        // Refinement-lifted laws now flow through law_auto via the
        // IR-pinned `ProofStrategy::LinearArithmetic { lifted: true }`.
        // The lowerer detects the `Refined(value = given)` carrier
        // shape and pins the strategy; emit_simp_omega_from_ir
        // skips by_cases when lifted=true. Step 30 retired this
        // separate code path so all laws go through one dispatch.)
        if let Some(auto_proof) = emit_verify_law_forall_auto_proof(
            vb,
            &law_for_auto_proof,
            ctx,
            verify_mode,
            &theorem_base,
            &quant_params,
            &theorem_prop,
        ) {
            lines.extend(auto_proof.support_lines);
            if !auto_proof.replaces_theorem {
                lines.push(format!(
                    "theorem {} : ∀ {}, {} := by",
                    theorem_base, quant_params, theorem_prop
                ));
            }
            lines.extend(auto_proof.proof_lines);
        } else {
            lines.push(format!(
                "theorem {} : ∀ {}, {} := by",
                theorem_base, quant_params, theorem_prop
            ));
            lines.push(
                "  -- verify law is sampled; universal proof must be provided manually".to_string(),
            );
            lines.push("  sorry".to_string());
        }
    }

    // Skip checked_domain emission for refinement-lifted laws: the
    // universal theorem already quantifies over the refined type
    // (`∀ a b : Natural`), which strictly entails any
    // sample-domain conjunction over the same body. Keeping
    // checked_domain would emit a 25+ conjunct of `add ⟨v, by …⟩`
    // calls that Lean has to run `native_decide` against, which
    // for compound predicates (`Bool.and(n ≥ 0, n ≤ 100)`) blows
    // through `maxHeartbeats`. The per-case `sample_N` theorems
    // below still get emitted as a granular cross-check.
    if !vb.cases.is_empty() && lifted_vars.is_empty() {
        let domain_theorem_name = format!("{}_checked_domain", theorem_base);
        let domain_conjuncts: Vec<String> = vb
            .cases
            .iter()
            .enumerate()
            .map(|(idx, (left, right))| {
                // Oracle v1: per-case sample rewrite. Each case has
                // its own domain-value binding (one case per value
                // in `given stub: E = [a, b, ...]`). Use the case's
                // own binding map rather than the law-level `.first()`
                // which would emit cross-case mismatches.
                let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
                let mode =
                    crate::codegen::common::OracleInjectionMode::SampleCaseBinding(case_bindings);
                let left_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
                    left,
                    law,
                    |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                    mode.clone(),
                );
                let right_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
                    right,
                    law,
                    |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
                    mode,
                );
                let left_str = emit_expr_legacy(&left_rw, ctx, None);
                // Model-vs-ground-truth: same literalization as the per-case
                // `_sample_N` theorems below (see the comment there); this
                // arm is reached only for non-refinement-lifted laws, where
                // the carrier-typed VM literal matches the statement type.
                let right_str =
                    super::sample_literal::ground_truth_rhs(vb, ctx, case_index_start + idx)
                        .unwrap_or_else(|| emit_expr_legacy(&right_rw, ctx, None));
                if let Some(guard) = law.sample_guards.get(idx) {
                    // Int-ascribed guard rendering — see `emit_sample_guard`
                    // (a bare numeral premise with subtraction elaborates as
                    // truncated `Nat` arithmetic and can change the
                    // proposition).
                    format!(
                        "({} = true -> {} = {})",
                        emit_sample_guard(guard, ctx),
                        left_str,
                        right_str
                    )
                } else {
                    format!("({} = {})", left_str, right_str)
                }
            })
            .collect::<Vec<_>>();
        // Mode-independent chunking decision. The `maxRecDepth` wall the
        // NativeDecide comment below describes is NOT specific to
        // `Decidable`-instance synthesis: plain elaboration of the nested-∧
        // STATEMENT recurses once per conjunct too, so a 512-conjunct
        // theorem fails the build identically under `--verify-mode sorry`
        // and `--verify-mode theorem-skeleton` (the proof body is never
        // reached). Every mode therefore emits the same `<name>_part<i>`
        // partition past the 36-conjunct edge; at or below the edge the
        // single-theorem emission is byte-identical to the pre-chunking
        // output in every mode.
        const CHECKED_DOMAIN_CHUNK: usize = 32;
        let checked_domain_statements: Vec<(String, String)> = if domain_conjuncts.len() > 36 {
            domain_conjuncts
                .chunks(CHECKED_DOMAIN_CHUNK)
                .enumerate()
                .map(|(part_idx, chunk)| {
                    (
                        format!("{}_part{}", domain_theorem_name, part_idx + 1),
                        chunk.join(" ∧ "),
                    )
                })
                .collect()
        } else {
            vec![(domain_theorem_name.clone(), domain_conjuncts.join(" ∧ "))]
        };
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                // `checked_domain` is one nested ∧-conjunction with N
                // implications. When the law's case body produces a
                // wrapper type (Result → `Except String T`, Option →
                // `Option T`), Lean's default `synthInstance.maxSize`
                // (~200) is too small to reach `DecidableEq` through
                // the wrapper instance at ~16+ conjuncts — `native_
                // decide` then dies with `failed to synthesize
                // Decidable`. Bumping the budget locally to the theorem
                // is cheaper than rewriting the emitter to fan the
                // cases out into N separate theorems (the per-case
                // `sample_N` theorems below already cover that view).
                //
                // GUARDED conjunctions additionally carry one premise
                // implication + decide-coerced guard per conjunct, and
                // elaboration of that chain can blow the default
                // 200_000 `maxHeartbeats` once the given product grows
                // past ~36 conjuncts (tests/fixtures/nr_wall.av's
                // 36-conjunct mulLeTrans sits at the edge). Give big guarded
                // conjunctions a per-THEOREM budget (scoped `in`, never
                // file-wide) — an honestly-false conjunct still fails
                // `native_decide` the same way, it just isn't
                // misreported as a heartbeat timeout.
                //
                // Past that edge a second elaborator limit bites:
                // `Decidable` instance synthesis recurses once per
                // nested ∧, so a given product of a few hundred cases
                // (e.g. 8×8×8 = 512 — tests/fixtures/large_domain_law
                // .av) exceeds the default `maxRecDepth` and the WHOLE
                // file fails to build — the caught-sorry floor
                // guarantee dies with it. Chunk big conjunctions into
                // `<name>_part<i>` theorems of at most
                // CHECKED_DOMAIN_CHUNK conjuncts each; the union of
                // the parts states exactly the original conjunction.
                // Single-theorem emission is byte-identical up to the
                // 36-conjunct edge (the corpus max), so existing
                // exports do not move.
                let heartbeats_budget = if law.when.is_some() && vb.cases.len() > 36 {
                    "set_option maxHeartbeats 800000 in\n"
                } else {
                    ""
                };
                for (part_name, part_prop) in &checked_domain_statements {
                    lines.push(format!(
                        "{}set_option synthInstance.maxSize 4096 in\ntheorem {} : {} := by native_decide",
                        heartbeats_budget, part_name, part_prop
                    ));
                }
            }
            VerifyEmitMode::Sorry => {
                for (part_name, part_prop) in &checked_domain_statements {
                    lines.push(format!("theorem {} : {} := by sorry", part_name, part_prop));
                }
            }
            VerifyEmitMode::TheoremSkeleton => {
                for (part_name, part_prop) in &checked_domain_statements {
                    lines.push(format!("theorem {} : {} := by", part_name, part_prop));
                    lines.push("  sorry".to_string());
                }
            }
        }
    }

    for (idx, (left, right)) in vb.cases.iter().enumerate() {
        let theorem_name = format!("{}_sample_{}", theorem_base, case_index_start + idx + 1);
        // Oracle v1: inject the case-specific stub value so a domain
        // like `given stub: E = [a, b]` produces two sample theorems —
        // one with `a`, one with `b` — instead of mismatched
        // `impl(…, a) = spec(…, b)` pairs.
        let case_bindings = vb.case_givens.get(idx).map(|v| v.as_slice()).unwrap_or(&[]);
        let mode = crate::codegen::common::OracleInjectionMode::SampleCaseBinding(case_bindings);
        let left_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            left,
            law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode.clone(),
        );
        let right_rw = crate::codegen::common::rewrite_effectful_calls_in_law(
            right,
            law,
            |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
            mode,
        );
        let left_str = emit_expr_legacy(&left_rw, ctx, None);
        // Expected side from VM ground truth: the `_sample_N` theorem is
        // `impl(sample) = spec(sample)` by construction — BOTH sides through
        // the model, vacuously provable when fuel exhaustion collapses both
        // to `default`. With a ground-truth entry it becomes
        // `impl(sample) = <value the program computed>`, which is exactly the
        // claim the sample is meant to pin. Refinement-lifted laws are
        // excluded: their statements quantify over the refined subtype, and a
        // carrier-typed literal would not elaborate — they keep the source
        // spec side (the `--check` panic gate still covers them). Misses
        // (failed/skipped at `aver verify`, Float-carrying values — decimal
        // repr isn't bit-exact — or non-round-tripping shapes) fall back to
        // the source spec side.
        let right_str = if lifted_vars.is_empty() {
            super::sample_literal::ground_truth_rhs(vb, ctx, case_index_start + idx)
                .unwrap_or_else(|| emit_expr_legacy(&right_rw, ctx, None))
        } else {
            emit_expr_legacy(&right_rw, ctx, None)
        };
        let sample_prop = if let Some(guard) = law.sample_guards.get(idx) {
            // Int-ascribed guard rendering — see `emit_sample_guard` (a bare
            // numeral premise with subtraction elaborates as truncated `Nat`
            // arithmetic and can make the theorem FALSE AS STATED).
            format!(
                "{} = true -> {} = {}",
                emit_sample_guard(guard, ctx),
                left_str,
                right_str
            )
        } else {
            format!("{} = {}", left_str, right_str)
        };
        match verify_mode {
            VerifyEmitMode::NativeDecide => {
                lines.push(format!(
                    "theorem {} : {} := by native_decide",
                    theorem_name, sample_prop
                ));
            }
            VerifyEmitMode::Sorry => {
                lines.push(format!(
                    "theorem {} : {} := by sorry",
                    theorem_name, sample_prop
                ));
            }
            VerifyEmitMode::TheoremSkeleton => {
                lines.push(format!("theorem {} : {} := by", theorem_name, sample_prop));
                lines.push("  sorry".to_string());
            }
        }
    }
    (lines.join("\n"), case_index_start + vb.cases.len())
}

/// The discovery feedback loop, część A: a proved earlier `verify … law`
/// becomes a lemma for later laws. Returns `(theorem_name, "lhs = rhs")` for a
/// law usable as a `simp` rewrite rule, mirroring `emit_verify_law_block`'s
/// name + lhs/rhs template computation EXACTLY (so the referenced name and the
/// orientation-analysis text match the actually-emitted theorem). `None` for
/// shapes that aren't a clean equational rewrite rule: trace-projection LHS
/// (no theorem emitted), a `when` premise (a conditional equation, not a plain
/// rewrite), or a refinement-lifted given (the statement quantifies over a
/// subtype — not a useful rewrite over the carrier). The name is the SAME
/// `<fn>_eq_<spec>` / `<fn>_law_<name>` the block emits, so a later law's
/// `simp [<name>]` resolves against the earlier theorem already in scope.
pub(crate) fn law_as_lemma_statement(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<(String, String)> {
    if law.when.is_some() {
        return None;
    }
    if crate::codegen::common::law_lhs_has_trace_projection(&law.lhs) {
        return None;
    }
    // A referenceable lemma must actually be EMITTED as a `∀`-theorem with the
    // name we synthesize below — otherwise a later law's `simp [<name>]` hits
    // an unknown identifier and fails the whole file's build. Decline exactly
    // the cases `emit_verify_law_block` skips the universal theorem:
    //   - no givens → no quantified theorem (only concrete samples);
    //   - `skip_universal` — a singleton-domain const-RHS law (vacuous/false
    //     universal) or a law calling a fuel-bounded recursive helper (the
    //     auto-prover can't close it, only per-sample lemmas are emitted).
    // Mirrors the guard in `emit_verify_law_block` (kept in sync by the
    // część A regression test); a false decline only forgoes a helper, never
    // references a missing theorem.
    if law.givens.is_empty() {
        return None;
    }
    let ir_strategy_closes_const_rhs = ctx
        .symbol_table
        .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name))
        .and_then(|fn_id| {
            ctx.proof_ir
                .law_theorems
                .iter()
                .find(|t| t.fn_id == fn_id && t.law_name == law.name)
        })
        .is_some_and(|t| {
            !matches!(
                t.strategy,
                crate::ir::ProofStrategy::Induction { .. }
                    | crate::ir::ProofStrategy::SimpOverLemmas(_)
                    | crate::ir::ProofStrategy::BackendDispatch
                    | crate::ir::ProofStrategy::Sorry
            )
        });
    let singleton_const_rhs = !ir_strategy_closes_const_rhs
        && crate::codegen::common::all_givens_are_singletons(law)
        && crate::codegen::common::law_rhs_is_independent_of_givens(law);
    let unclassified = crate::codegen::common::unclassified_fn_names(ctx);
    if singleton_const_rhs || crate::codegen::common::law_calls_unclassified_fn(law, &unclassified)
    {
        return None;
    }
    let fn_name = aver_name_to_lean(&vb.fn_name);
    let law_name = aver_name_to_lean(&law.name);
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, ctx);
    let theorem_base = match &spec_ref {
        Some(spec_ref) => format!(
            "{}_eq_{}",
            fn_name,
            aver_name_to_lean(&spec_ref.spec_fn_name)
        ),
        None => format!("{}_law_{}", fn_name, law_name),
    };
    let law_lhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let law_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        |n| ctx.fn_def_by_name(n, ctx.active_module_scope().as_deref()),
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    // A refinement-lifted given would change the quantifier type (the theorem
    // reads `∀ a : Natural, …`); such a statement isn't a plain rewrite rule
    // over the carrier, so decline rather than mis-orient it.
    for given in &law.givens {
        if crate::codegen::common::refinement_lift_for_given(
            &given.name,
            &given.type_name,
            &law_lhs,
            &law_rhs,
            ctx,
        )
        .is_some()
        {
            return None;
        }
    }
    let lhs = emit_expr_legacy(&law_lhs, ctx, None);
    let rhs = emit_expr_legacy(&law_rhs, ctx, None);
    Some((theorem_base, format!("{lhs} = {rhs}")))
}

/// Build the law theorem's statement body. Returns `(prop, bounded_domain)`:
/// `bounded_domain` is `true` iff sampled-domain disjunction premises
/// (`a = 0 ∨ a = 1 ∨ …`) were prepended — the statement then only claims the
/// law over the finite sample domain, NOT universally. This flag is the
/// single source of truth for the `-- aver:law-class` marker the caller
/// emits (see `LAW_CLASS_MARKER_PREFIX`): the checker's `universal` metric
/// keys on it instead of re-deriving the class from names or statements.
/// A `when`-premise alone (`… = true ->`) does NOT bound the statement —
/// it is a conditional but still universally quantified claim (the
/// refinement-lifted case, where every given's domain premise is dropped).
///
/// `omit_domain` is the when-universal quarantine lane's statement mode
/// (see `lean::universal_lane`): the SAME builder renders the lane twin
/// — `∀ givens, <when> = true -> claim` — by skipping the sampled-domain
/// disjunctions entirely, so the twin's statement provably differs from
/// the manifest theorem only by those premises (zero second renderer).
/// The manifest pipeline always passes `false`.
pub(super) fn law_theorem_prop(
    law: &VerifyLaw,
    ctx: &CodegenContext,
    lhs_template: &str,
    rhs_template: &str,
    when_template: Option<&str>,
    lifted_vars: &std::collections::HashMap<String, String>,
    omit_domain: bool,
) -> (String, bool) {
    let mut premises = Vec::new();
    let when_redundant_with_lifts = law
        .when
        .as_ref()
        .map(|w| {
            crate::codegen::common::when_is_redundant_with_refinement_lifts(w, lifted_vars, ctx)
        })
        .unwrap_or(false);
    if law.when.is_some() && !omit_domain {
        // Lifted vars are quantified over the refinement record
        // (`a : Natural`), not the carrier `Int`, so the disjunctive
        // domain premise (`a = 0 ∨ a = 1 ∨ …`) is type-mismatched
        // (Lean sees `0 : Int` against `a : Natural`). Skip the
        // domain premise for any lifted given.
        premises.extend(law.givens.iter().filter_map(|given| {
            if lifted_vars.contains_key(&given.name) {
                None
            } else {
                Some(law_given_domain_prop(given, ctx))
            }
        }));
    }
    let bounded_domain = !premises.is_empty();
    // `when` drop is only sound when the predicate is syntactically
    // equivalent (via commutator-relaxed compare) to the conjunction
    // of lifted givens' refinement invariants — otherwise stronger /
    // orthogonal user predicates would be silently lost from the
    // emitted theorem (e.g. `when a >= 10` over `a : Natural` whose
    // invariant is `a.val >= 0`). Same identity check the Dafny
    // backend uses.
    if let Some(when_expr) = when_template
        && !when_redundant_with_lifts
    {
        premises.push(format!("{when_expr} = true"));
    }
    let conclusion = format!("{lhs_template} = {rhs_template}");
    let prop = if premises.is_empty() {
        conclusion
    } else {
        format!("{} -> {}", premises.join(" -> "), conclusion)
    };
    (prop, bounded_domain)
}

fn law_given_domain_to_lean(domain: &VerifyGivenDomain, ctx: &CodegenContext) -> String {
    match domain {
        VerifyGivenDomain::IntRange { start, end } => format!("{}..{}", start, end),
        VerifyGivenDomain::Explicit(values) => format!(
            "[{}]",
            values
                .iter()
                .map(|v| emit_expr_legacy(v, ctx, None))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

fn law_given_domain_prop(given: &VerifyGiven, ctx: &CodegenContext) -> String {
    let raw_name = aver_name_to_lean(&given.name);
    // Subtype-carried oracle bindings (`rng : RandomIntInBounds`) need
    // `.val` projection on the LHS of the equality so the comparison
    // type-checks against the underlying plain function the user's
    // stub delivers. Other givens compare the raw value directly.
    let given_name = if bounded_oracle_subtype_for(&given.type_name).is_some() {
        format!("{raw_name}.val")
    } else {
        raw_name
    };
    let values = law_given_domain_values(&given.domain);
    match values.as_slice() {
        [] => "False".to_string(),
        [value] => format!("{given_name} = {}", emit_expr_legacy(value, ctx, None)),
        _ => values
            .iter()
            .map(|value| format!("{given_name} = {}", emit_expr_legacy(value, ctx, None)))
            .collect::<Vec<_>>()
            .join(" ∨ "),
    }
}

pub(super) fn law_given_domain_values(domain: &VerifyGivenDomain) -> Vec<Spanned<Expr>> {
    match domain {
        VerifyGivenDomain::IntRange { start, end } => (*start..=*end)
            .map(|n| Spanned::bare(Expr::Literal(Literal::Int(n))))
            .collect(),
        VerifyGivenDomain::Explicit(values) => values.clone(),
    }
}

/// Emit a decision block as a Lean 4 block comment.
pub fn emit_decision(db: &DecisionBlock) -> String {
    let mut lines = Vec::new();
    lines.push(format!("/- Decision: {}", db.name));
    lines.push(format!("   Date: {}", db.date));
    lines.push(format!("   Reason: {}", db.reason));
    lines.push(format!("   Chosen: {}", db.chosen.node.as_context_string()));
    if !db.rejected.is_empty() {
        lines.push(format!(
            "   Rejected: {}",
            db.rejected
                .iter()
                .map(|r| r.node.as_context_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    if !db.impacts.is_empty() {
        let impacts = db
            .impacts
            .iter()
            .map(|impact| impact.node.as_context_string())
            .collect::<Vec<_>>()
            .join(", ");
        lines.push(format!("   Impacts: {}", impacts));
    }
    if let Some(author) = &db.author {
        lines.push(format!("   Author: {}", author));
    }
    lines.push("-/".to_string());
    lines.join("\n")
}

/// Emit mutual recursion group wrapped in `mutual ... end`.
pub fn emit_mutual_group(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    let mut lines = Vec::new();
    lines.push("mutual".to_string());
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", sanitize_doc(desc)));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        // #17: All functions in mutual blocks need `partial` for termination
        lines.push(format!(
            "  partial def {} {} : {} :=",
            fn_name, params, ret_type
        ));
        let body = emit_fn_body_for(fd, &fd.body, ctx);
        // Indent body by 2 more spaces
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        lines.push(String::new());
    }
    lines.push("end".to_string());
    lines.join("\n")
}

/// Proof-mode mutual recursion emission with optional group-level termination.
pub fn emit_mutual_group_proof(fns: &[&FnDef], ctx: &CodegenContext) -> String {
    // Distinguish mutual SCC shapes by the Lex params vector:
    //   `[p]` rank 0  → MutualIntCountdown
    //   `[s, pos]`    → MutualStringPosAdvance
    //   `[]` rank >=1 → MutualSizeOfRanked
    let all_int_countdown = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, 0)) if params.len() == 1
        )
    });
    if all_int_countdown {
        return emit_fuelized_mutual_int_countdown_group(fns, ctx);
    }

    let all_string_pos = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, _)) if params.len() == 2
        )
    });
    if all_string_pos {
        return emit_fuelized_mutual_string_pos_group(fns, ctx);
    }

    let all_sizeof = fns.iter().all(|fd| {
        matches!(
            contract_lex_params_rank(ctx, fd),
            Some((params, _)) if params.is_empty()
        )
    });
    if all_sizeof {
        if let Some(code) = emit_native_mutual_sizeof_group(fns, ctx) {
            return code;
        }
        // Termination-as-a-law: try a genuine well-founded mutual block whose
        // `decreasing_by` cites synthesised, kernel-proved length lemmas for
        // computed-list-arg recursion (quicksort's `sort`/`sortWithPivot`).
        // Backs off to fuel when the SCC isn't length-monotone-WF.
        if let Some(code) = emit_native_mutual_lex_list_wf_group(fns, ctx) {
            return code;
        }
        return emit_fuelized_mutual_sizeof_group(fns, ctx);
    }

    let mut lines = Vec::new();
    lines.push("mutual".to_string());
    for fd in fns {
        if !is_pure_fn(fd) {
            continue;
        }
        if let Some(desc) = &fd.desc {
            lines.push(format!("  /-- {} -/", sanitize_doc(desc)));
        }
        let fn_name = aver_name_to_lean(&fd.name);
        let params = emit_fn_params(&fd.params);
        let ret_type = if fd.return_type.is_empty() {
            "Unit".to_string()
        } else {
            type_annotation_to_lean(&fd.return_type)
        };
        lines.push(format!("  def {} {} : {} :=", fn_name, params, ret_type));
        let body = emit_fn_body_for(fd, &fd.body, ctx);
        for line in body.lines() {
            lines.push(format!("  {}", line));
        }
        match contract_lex_params_rank(ctx, fd) {
            Some((params, 0)) if params.len() == 1 => {
                // MutualIntCountdown — every member counts down the
                // shared first-Int param. (The IR's param name is
                // canonical; we don't fall back to fd.params here.)
                let lean_first = aver_name_to_lean(&params[0]);
                lines.push(format!("  termination_by Int.natAbs {}", lean_first));
                lines.push("  decreasing_by".to_string());
                lines.push("    omega".to_string());
            }
            Some((params, rank)) if params.len() == 2 => {
                // MutualStringPosAdvance — (s, pos) shape; rank
                // distinguishes SCC members.
                let lean_s = aver_name_to_lean(&params[0]);
                let lean_pos = aver_name_to_lean(&params[1]);
                lines.push(format!(
                    "  termination_by (({}.data.length) - ({}.toNat), {})",
                    lean_s, lean_pos, rank
                ));
                lines.push("  decreasing_by".to_string());
                lines.push("    simp_wf".to_string());
            }
            Some(([], _)) => {
                // MutualSizeOfRanked — handled inside the SCC's
                // dedicated emitter; no termination_by suffix here.
            }
            _ => {}
        }
        lines.push(String::new());
    }

    lines.push("end".to_string());
    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn doc_comment_escapes_block_comment_delimiters() {
        // An Aver `?` doc mentioning `/-` or `-/` (e.g. "+2/-2") must not open or
        // close a NESTED block comment inside the `/-- ... -/` wrapper — that
        // leaves the comment unterminated and breaks the whole Lean file.
        let out = emit_doc_comment(&Some("delta +2/-2 ends with -/ token".to_string()));
        assert_eq!(out.len(), 1);
        let line = &out[0];
        let inner = line
            .strip_prefix("/-- ")
            .and_then(|s| s.strip_suffix(" -/"))
            .expect("doc comment keeps the /-- ... -/ wrapper");
        assert!(
            !inner.contains("/-"),
            "inner doc still opens a nested block comment: {line}"
        );
        assert!(
            !inner.contains("-/"),
            "inner doc still closes a nested block comment: {line}"
        );
    }
}
