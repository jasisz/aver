use std::collections::{HashMap, HashSet};

use super::expr::aver_name_to_lean;
use super::syntax::lean_ctor_name;
use super::types::type_annotation_to_lean;
use super::{is_recursive_product, is_recursive_type};
use crate::ast::*;
use crate::codegen::CodegenContext;

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

/// Emit an explicit `Inhabited` instance for a user type, for the certificate
/// model mode where `deriving` is unavailable (the checker wall rejects the
/// `deriving` token). The reused proof emission gets `Inhabited` from `deriving`;
/// it is required because proof-mode recursive functions `panic!` on fuel
/// exhaustion, and `panic!` demands `Inhabited` on the result type.
///
/// Returns `""` for the type shapes that never carry `deriving` (canonical
/// Peano — lifted to `Nat`; an `Int`-carrier refined record — emitted as a
/// `Subtype`), so callers can invoke it uniformly.
///
/// A sum's witness is the first constructor in declaration order whose
/// arguments can all be defaulted (base case: a nullary constructor) — the
/// same constructor `deriving Inhabited` tries first. An argument that
/// mentions the type itself cannot be defaulted: `default` on it asks for the
/// very instance being stated, and the certificate declines with "failed to
/// synthesize instance of type class Inhabited". The exception is a mention
/// beneath a top-level `List`/`Option`/`Map`, whose Lean defaults (`[]`,
/// `none`; a Map renders as `List (K × V)`) need no argument instance. When
/// no constructor bottoms out, no instance is emitted — the conservative
/// no-instance shape the other underivable types above already take; a model
/// that actually demands the instance then declines at the Lean build. The
/// scan is one syntactic pass: a mutually recursive type family is not
/// detected here and still declines at the Lean build as before.
///
/// A record's witness NAMES its fields — `⟨{ f := default }⟩`, never
/// `⟨default⟩` — because the outer brackets are already the `Inhabited`
/// constructor: a bare `default` between them asks for the very instance being
/// stated, and Lean answers "failed to synthesize instance of type class
/// Inhabited", which declines the whole certificate. A record whose field is
/// another record needs no extra ordering care: type defs are emitted in
/// dependency order (the inner record's structure has to precede the outer
/// one's field anyway) and each instance follows its own structure, so the
/// inner witness is always already in scope.
pub fn emit_inhabited_instance(td: &TypeDef, ctx: &CodegenContext, scope: Option<&str>) -> String {
    if crate::codegen::proof_recognize::detect_canonical_peano(td).is_some() {
        return String::new();
    }
    match td {
        TypeDef::Sum { name, variants, .. } => {
            let Some(seed) = variants.iter().find(|v| {
                v.fields
                    .iter()
                    .all(|field| field_defaults_without(field, name))
            }) else {
                return String::new();
            };
            let lean_name = aver_name_to_lean(name);
            let args = " default".repeat(seed.fields.len());
            format!(
                "instance : Inhabited {lean_name} := ⟨{lean_name}.{}{args}⟩",
                lean_ctor_name(&seed.name)
            )
        }
        TypeDef::Product { name, fields, .. } => {
            // Every decl admitted to ProofIR emits as a Subtype rather than a
            // structure and therefore never carries `deriving`.
            if crate::codegen::common::find_refined_type_scoped(ctx, name, scope).is_some() {
                return String::new();
            }
            let assignments = fields
                .iter()
                .map(|(field_name, _)| format!("{} := default", aver_name_to_lean(field_name)))
                .collect::<Vec<_>>()
                .join(", ");
            let value = if assignments.is_empty() {
                "{}".to_string()
            } else {
                format!("{{ {assignments} }}")
            };
            format!(
                "instance : Inhabited {} := ⟨{value}⟩",
                aver_name_to_lean(name)
            )
        }
    }
}

/// `default` for this constructor argument elaborates without `Inhabited` on
/// the sum type currently being seeded: either the annotation never mentions
/// that type, or the mention sits beneath a top-level `List`/`Option`/`Map`,
/// which Lean inhabits with no argument instance (`[]`, `none`; a Map renders
/// as `List (K × V)`).
fn field_defaults_without(field: &str, type_name: &str) -> bool {
    let trimmed = field.trim();
    if trimmed.ends_with('>')
        && ["List<", "Option<", "Map<"]
            .iter()
            .any(|prefix| trimmed.starts_with(prefix))
    {
        return true;
    }
    !crate::codegen::common::type_ref_contains(trimmed, type_name)
}

fn emit_sum_type(name: &str, variants: &[TypeVariant]) -> String {
    // Lean constructor spellings are first-letter-lowercased
    // (`lean_ctor_name`), so two variants of ONE type differing only in
    // first-letter case (`Accept` / `accept` — legal Aver, the parser
    // takes any Ident and PascalCase is only a lint) would silently
    // emit the SAME Lean constructor and surface as a confusing
    // duplicate-constructor error inside the generated project, far
    // from the Aver source. Reject loudly at emission time instead,
    // naming both variants. (Exact duplicates are already a typecheck
    // error, so a collision here always involves two distinct names.)
    let mut ctor_spellings: HashMap<String, &str> = HashMap::new();
    for v in variants {
        let ctor = lean_ctor_name(&v.name);
        if let Some(earlier) = ctor_spellings.insert(ctor.clone(), v.name.as_str()) {
            panic!(
                "Lean export: variants `{earlier}` and `{}` of type `{name}` both lower to \
                 Lean constructor `{ctor}` — variant names of one type may not differ only \
                 in first-letter case; rename one of them",
                v.name
            );
        }
    }

    let mut lines = Vec::new();
    let is_recursive = is_recursive_type(name, variants);

    lines.push(format!("inductive {} where", aver_name_to_lean(name)));
    for v in variants {
        let lean_name = lean_ctor_name(&v.name);
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
    // The lowerer is the single eligibility decision. Int, structural
    // containers, and nested named carriers admitted there all render through
    // the same Subtype shape; intentionally unsupported scalar carriers never
    // enter `refined_types` and keep the plain structure path below.
    if let Some(decl) = crate::codegen::common::find_refined_type_scoped(ctx, name, scope) {
        let carrier_ty = type_annotation_to_lean(&decl.carrier_type);
        let param = aver_name_to_lean(&decl.predicate_param);
        let predicate = super::expr::emit_expr(&decl.invariant.expr, ctx);
        return format!(
            "abbrev {} := {{ {param} : {carrier_ty} // {predicate} }}",
            aver_name_to_lean(name)
        );
    }

    let mut lines = Vec::new();
    let is_recursive = is_recursive_product(name, fields);

    lines.push(format!("structure {} where", aver_name_to_lean(name)));
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

pub(super) fn type_measure_expr(
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
        aver_name_to_lean(name)
    ));
    lines.push("    match value with".to_string());
    for variant in variants {
        let ctor = lean_ctor_name(&variant.name);
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
        aver_name_to_lean(name)
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
            type_annotation_to_lean(&key_type),
            aver_name_to_lean(name)
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
            aver_name_to_lean(name)
        ),
        format!("    {}", body),
        format!(
            "  def {} (items : List {}) : Nat :=",
            measure_list_fn_name(name),
            aver_name_to_lean(name)
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
            type_annotation_to_lean(&key_type),
            aver_name_to_lean(name)
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
    let name = aver_name_to_lean(name);
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
