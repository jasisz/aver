//! Aver type → wasm-gc representation.
//!
//! Two layers:
//!
//! 1. **Primitives** — `Int → i64`, `Float → f64`, `Bool → i32`,
//!    `Unit → empty`. These map directly without any module-level
//!    type-section entry.
//!
//! 2. **User types** — records and variants. Each `record Foo { … }`
//!    becomes a `(type $Foo (struct (field T_1) … (field T_N)))` in
//!    the wasm type section; the struct's type index is recorded in
//!    `TypeRegistry` so emit sites can resolve `RecordCreate { type_name }`
//!    and `Attr { obj, field }` to `struct.new` / `struct.get` against
//!    the right struct.
//!
//! Variants (`type Shape = Circle(Float) | Rect(Float, Float)`) get
//! one struct type per constructor, with the parent-type name as the
//! abstract carrier. Phase-3 keeps it simple: each constructor stands
//! alone (no subtyping yet — pattern matching dispatches via tag-by-
//! struct-type comparison through `ref.test`).

use std::collections::HashMap;

use wasm_encoder::{
    AbstractHeapType, FieldType, HeapType, RefType, StorageType, StructType, ValType,
};

use super::WasmGcError;
use super::types_discovery::{
    collect_lists_from_str, collect_maps_from_expr, collect_maps_from_str,
    collect_options_from_fn_body, collect_options_from_str, collect_results_from_builtin_uses,
    collect_results_from_str, collect_tuples_from_fn_body, collect_tuples_from_str,
    collect_vectors_from_fn_body, collect_vectors_from_str, is_primitive,
};

use crate::ast::{TopLevel, TypeDef};

/// User-type lookup tables built once before any fn body emit.
pub(super) struct TypeRegistry {
    /// `record_name → type_idx` for product (record) types.
    pub(super) records: HashMap<String, u32>,
    /// `variant_constructor_name → (parent_type_name, type_idx, fields)`.
    /// `fields` are the type strings of the constructor's positional
    /// fields (Aver variants use positional fields, not named ones).
    /// Bare variant name → every registered variant with that name
    /// (one per parent sumtype). Two sumtypes can share a variant
    /// name (e.g. `Query.ProviderSummary` and `QueryOutput.
    /// ProviderSummary` in payment_ops); without the parent in the
    /// key, one would silently shadow the other and emit would pick
    /// the wrong concrete struct type.
    pub(super) variants: HashMap<String, Vec<VariantInfo>>,
    /// `record_name → field list` so `Attr` can resolve a field name
    /// to its struct field index + type.
    pub(super) record_fields: HashMap<String, Vec<(String, String)>>,
    /// Per-instantiation `Vector<T>` slot. Key is the canonical Aver
    /// type string (e.g. `"Vector<Int>"`). Value is the wasm type idx
    /// of the underlying `(array (mut T))`. Monomorphized: each `T`
    /// reachable in the program gets its own slot, so element access
    /// is type-direct (no anyref / no boxing).
    pub(super) vector_types: HashMap<String, u32>,
    /// Insertion order for `vector_types` — used by module emit so
    /// type-section entries land at the indices the registry recorded.
    pub(super) vector_order: Vec<String>,
    /// Per-instantiation `Option<T>` slot. Same monomorphisation
    /// strategy as `vector_types`. Each `Option<T>` lowers to a
    /// `(struct (mut i32 tag) (mut T value))` — tag=0 None, tag=1
    /// Some. The `value` field carries a default for None (zero for
    /// numerics, null for ref types) so `struct.new` always has a
    /// valid initial value; pattern matching reads `tag` first and
    /// only consumes `value` on the Some branch.
    pub(super) option_types: HashMap<String, u32>,
    pub(super) option_order: Vec<String>,
    /// Per-instantiation `List<T>` slot. Each `List<T>` lowers to a
    /// recursive struct `(struct (field T) (field (ref null $list_T)))`
    /// — Cons cell. Empty list = `(ref null $list_T)` null. Self-
    /// reference is allowed within a single type definition (wasm spec
    /// implicitly makes each top-level type its own rec group).
    pub(super) list_types: HashMap<String, u32>,
    pub(super) list_order: Vec<String>,
    /// Per-instantiation `Result<T, E>` slot. Each `Result<T, E>`
    /// lowers to `(struct (mut i32 tag) (mut T ok_value) (mut E
    /// err_value))` — tag=0 Err, tag=1 Ok. Both payload fields exist
    /// concurrently because the struct can't be a sum at the wasm
    /// type level; the unused field is filled with a default (zero
    /// for primitives, null for refs).
    pub(super) result_types: HashMap<String, u32>,
    pub(super) result_order: Vec<String>,
    /// Per-instantiation `Map<K, V>` slot triple (keys array, values
    /// array, map struct). Same monomorphisation strategy as Vector /
    /// Option — each unique `Map<K, V>` reachable in the program
    /// gets its own three slots and four helper bodies (empty, set,
    /// get, len). Phase-3c MVP supports `K = String`; other K kinds
    /// surface as Unimplemented when their hash / eq helpers would
    /// need to be emitted.
    pub(super) map_types: HashMap<String, MapSlots>,
    pub(super) map_order: Vec<String>,
    /// Per-instantiation `Tuple<A, B>` slot. Each lowers to a
    /// `(struct (mut A) (mut B))`. Used by `Map.entries` (returns
    /// `List<Tuple<K, V>>`), `Map.fromList`, and `List.zip`.
    pub(super) tuple_types: HashMap<String, u32>,
    pub(super) tuple_order: Vec<String>,
    /// Per-primitive-K `(struct (mut K))` slot used to box primitive
    /// map keys. The `Map<K, V>` open-addressing layout uses
    /// `keys[i] == null` as the empty marker, which only works for
    /// ref types — boxing the primitive into a struct ref keeps the
    /// marker scheme uniform across all K kinds. Helpers internally
    /// pass raw `K_val` and box on insert / unbox on read.
    pub(super) primitive_key_box: HashMap<String, u32>,
    pub(super) primitive_key_box_order: Vec<String>,
    /// Total number of user-type slots reserved in the type section.
    /// Function types start AFTER these.
    pub(super) user_type_count: u32,
    /// Wasm type idx for the `(array i8)` String representation.
    /// Allocated lazily on first reference; `None` when no String is
    /// reachable from the program (most numeric bench scenarios).
    /// See `builtins/` README for the full repr decision.
    pub(super) string_array_type_idx: Option<u32>,
    /// Per-byte-sequence passive data segment for `String` literals.
    /// Each unique literal in the program lands at one segment idx;
    /// `ResolvedExpr::Literal(Literal::Str(_))` lowers to `array.new_data
    /// $string $segment_idx` with offset=0, size=len.
    pub(super) string_literals: Vec<Vec<u8>>,
    pub(super) string_literal_idx: HashMap<Vec<u8>, u32>,
    /// Type names that must NOT be erased to their underlying
    /// primitive by the newtype optimisation. Populated with every
    /// record/variant used as a `Map<K, *>` key — Map's open-
    /// addressing layout uses `keys[i] == null` as the empty marker,
    /// which only works when keys are emitted as ref values.
    pub(super) non_newtypable_keys: std::collections::HashSet<String>,
    /// Phase 4 (0.20) — `(struct (mut i32 socket) (mut i32 in_stream)
    /// (mut i32 out_stream) (mut i32 in_use))` slot type for an entry
    /// in the TCP connection pool. `None` when no `Tcp.*` effect is
    /// declared in any fn. The pool itself (`tcp_pool_type_idx`) is
    /// an `(array (mut $tcp_slot))` containing 256 of these.
    pub(super) tcp_slot_type_idx: Option<u32>,
    /// Phase 4 (0.20) — `(array (mut $tcp_slot))` array type carrying
    /// 256 connection slots. `None` when no `Tcp.*` effect is
    /// declared. The runtime allocates the array lazily on first
    /// `Tcp.connect` call via `array.new_default` against this idx.
    pub(super) tcp_pool_type_idx: Option<u32>,
}

#[derive(Debug, Clone)]
pub(super) struct VariantInfo {
    pub(super) parent: String,
    pub(super) type_idx: u32,
    pub(super) fields: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct MapSlots {
    /// `(array (mut K))` — keys array; element type derived from `K`.
    pub(super) keys_array: u32,
    /// `(array (mut V))` — values array; element type derived from `V`.
    pub(super) values_array: u32,
    /// `(struct (mut i32 size) (mut i32 cap) (mut keys_ref) (mut values_ref))`.
    pub(super) map: u32,
}

impl TypeRegistry {
    /// Build the registry with a `--handler` shape — pre-register
    /// HttpRequest/HttpResponse refs in case the handler fn is the
    /// only place they appear (otherwise the auto-discovery picks
    /// them up). Also intern the `"cf-ipcountry"` string literal so
    /// the synthesised `aver_http_handle` wrapper has a valid data
    /// segment to source it from.
    pub(super) fn build_with_handler(
        items: &[TopLevel],
        resolved_fn_defs: &[crate::ir::hir::ResolvedFnDef],
        _handler_active: bool,
    ) -> Self {
        // _handler_active is consumed by `items_reference_name`
        // overrides below so the rest of the builder stays
        // unchanged.
        let handler_active = _handler_active;
        let mut records = HashMap::new();
        let mut variants: HashMap<String, Vec<VariantInfo>> = HashMap::new();
        let mut record_fields = HashMap::new();
        let mut next_idx: u32 = 0;
        for item in items {
            match item {
                TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) => {
                    records.insert(name.clone(), next_idx);
                    record_fields.insert(name.clone(), fields.clone());
                    next_idx += 1;
                }
                TopLevel::TypeDef(TypeDef::Sum {
                    name, variants: vs, ..
                }) => {
                    for v in vs {
                        variants
                            .entry(v.name.clone())
                            .or_default()
                            .push(VariantInfo {
                                parent: name.clone(),
                                type_idx: next_idx,
                                fields: v.fields.clone(),
                            });
                        next_idx += 1;
                    }
                }
                _ => {}
            }
        }
        // Built-in records (`HttpRequest`, `HttpResponse`,
        // `Tcp.Connection`, `Terminal.Size`) — populate `record_fields`
        // up front so List / Map field-walking discovery can pick up
        // `Map<String, List<String>>`, but defer slot assignment to the
        // end of `build` because their fields reference String / Map /
        // List which all sit at higher slots. Wasm-gc forward references
        // outside a rec group are illegal, so the struct-type emit has
        // to wait until after the dependencies.
        let mut builtin_record_names: Vec<String> = Vec::new();
        for record in crate::codegen::builtin_records::BUILTIN_RECORDS {
            // `--handler` mode forces HttpRequest + HttpResponse to be
            // registered even if no fn signature mentions them — the
            // synthesised `aver_http_handle` wrapper builds an
            // HttpRequest from host effects and reads HttpResponse
            // back, both of which are codegen-only references.
            //
            // Phase 4.5a similarly forces `Tcp.Connection` whenever
            // any `Tcp.*` effect is declared: even programs that only
            // call `Tcp.send` (and never name the record in source)
            // need its slot allocated because the orchestrator helper
            // threads a `Tcp.Connection` through internally.
            let force_handler = handler_active
                && (record.aver_name == "HttpRequest" || record.aver_name == "HttpResponse");
            let force_tcp = record.aver_name == "Tcp.Connection"
                && items.iter().any(|item| match item {
                    TopLevel::FnDef(fd) => fd.effects.iter().any(|e| e.node.starts_with("Tcp.")),
                    _ => false,
                });
            let force = force_handler || force_tcp;
            if !force && !items_reference_name(items, record.aver_name) {
                continue;
            }
            if record_fields.contains_key(record.aver_name) {
                continue;
            }
            let mut fields_v: Vec<(String, String)> = Vec::new();
            for f in record.fields {
                let aver_ty = builtin_type_to_aver_string(&f.ty);
                fields_v.push((f.name.to_string(), aver_ty));
            }
            record_fields.insert(record.aver_name.to_string(), fields_v);
            builtin_record_names.push(record.aver_name.to_string());
        }

        // Deterministic walk order for the carrier-type discovery below
        // (Option / Result / List / Vector / Map slots derived from
        // record fields). `record_fields` is a `HashMap`, so iterating
        // it directly registers those carrier slots in a process-random
        // order — which makes the emitted type section, and thus the
        // whole module, non-reproducible across builds. Sort the names
        // once and walk that fixed order at every record-field sweep
        // that feeds an ordered `*_order` / slot allocation.
        let mut record_names_sorted: Vec<String> = record_fields.keys().cloned().collect();
        record_names_sorted.sort();

        // Allocate the String type slot first (after records/variants)
        // so any `Vector<String>` registered below sits at a higher
        // index than `$string` and can reference it without crossing
        // the rec-group boundary.
        // Force the String type slot whenever the program has any
        // fn defs at all — `body/builtins.rs` now pushes the
        // current fn name as a String literal before every effect
        // import call (the `caller_fn` trailing arg). Without the
        // slot, `emit_string_literal_bytes` can't materialise the
        // ref and validation fails for trivially-Stringless programs
        // like `fn main() -> Int { _ = Time.unixMs(); 42 }`.
        let has_fn_defs = items.iter().any(|item| matches!(item, TopLevel::FnDef(_)));
        let needs_string = has_fn_defs
            || resolved_fn_defs.iter().any(|fd| {
                fd.return_type.display().contains("String")
                    || fd
                        .params
                        .iter()
                        .any(|(_, t)| t.display().contains("String"))
                    || fn_body_produces_string(fd)
            });
        let string_array_type_idx = if needs_string {
            let idx = next_idx;
            next_idx += 1;
            Some(idx)
        } else {
            None
        };

        // Phase 4 (0.20) — TCP connection pool type slots. `$tcp_slot`
        // is a 4-field struct (socket / in_stream / out_stream / in_use
        // handles, all i32), and `$tcp_pool` is the `(array (mut $tcp_slot))`
        // that holds 256 of them. Allocated whenever a fn declares any
        // `Tcp.*` effect; effect-target wiring (whether to actually emit
        // the connection pipeline) lives in module.rs. Both slots land
        // adjacent so the array type can reference the slot type without
        // crossing a rec-group boundary.
        let needs_tcp = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd.effects.iter().any(|e| e.node.starts_with("Tcp.")),
            _ => false,
        });
        let (tcp_slot_type_idx, tcp_pool_type_idx) = if needs_tcp {
            let slot_idx = next_idx;
            next_idx += 1;
            let pool_idx = next_idx;
            next_idx += 1;
            (Some(slot_idx), Some(pool_idx))
        } else {
            (None, None)
        };

        // Discover monomorphized `Vector<T>` instantiations. Walk fn
        // signatures (params + return types) and binding annotations;
        // each unique `Vector<T>` gets its own `(array (mut T))` slot.
        // Inferred Vectors (from `Vector.new` whose annotation is
        // implicit) still surface here when the surrounding param /
        // return type spells out the element type, which is the
        // canonical bench shape today.
        let mut vector_types: HashMap<String, u32> = HashMap::new();
        let mut vector_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_vectors_from_str(
                &fd.return_type.display(),
                &mut vector_types,
                &mut vector_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_vectors_from_str(
                    &ty.display(),
                    &mut vector_types,
                    &mut vector_order,
                    &mut next_idx,
                );
            }
            collect_vectors_from_fn_body(fd, &mut vector_types, &mut vector_order, &mut next_idx);
        }
        // Record field walks — `record { nums: Vector<Int> }` only
        // shows up in the record's field list, not in any fn
        // signature. Mirror the lists / options record-field walk.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_vectors_from_str(ty, &mut vector_types, &mut vector_order, &mut next_idx);
            }
        }

        // `Result<T, E>` and `List<T>` instantiations land BEFORE
        // options/maps so that `Option<List<String>>` /
        // `Map<String, Result<...>>` can reference them by an
        // already-assigned lower idx. Without this reordering the
        // option struct's value-field forward-references the
        // post-options list slot, which wasm-gc rejects outside a
        // rec group.
        let mut result_types: HashMap<String, u32> = HashMap::new();
        let mut result_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_results_from_str(
                &fd.return_type.display(),
                &mut result_types,
                &mut result_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_results_from_str(
                    &ty.display(),
                    &mut result_types,
                    &mut result_order,
                    &mut next_idx,
                );
            }
            collect_results_from_builtin_uses(
                fd,
                &mut result_types,
                &mut result_order,
                &mut next_idx,
            );
            // Walk binding annotations too — `let r: Result<Box, MyErr> = …`
            // wouldn't be picked up by the builtin-uses scan (which only
            // looks at known dotted calls like `Disk.readText`). Without this,
            // user-typed `Result<custom, custom>` values fail to find their
            // type slot at construction time.
            use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
            let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
            for stmt in stmts {
                if let ResolvedStmt::Binding {
                    ty_ann: Some(annot),
                    ..
                } = stmt
                {
                    collect_results_from_str(
                        &annot.display(),
                        &mut result_types,
                        &mut result_order,
                        &mut next_idx,
                    );
                }
            }
        }
        let mut list_types: HashMap<String, u32> = HashMap::new();
        let mut list_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_lists_from_str(
                &fd.return_type.display(),
                &mut list_types,
                &mut list_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_lists_from_str(
                    &ty.display(),
                    &mut list_types,
                    &mut list_order,
                    &mut next_idx,
                );
            }
            // Body annotations — `nested: List<List<Int>> = [a, b]`
            // adds `List<List<Int>>` even when no fn signature
            // mentions it. Mirrors the same body-walk options
            // and vectors already do.
            collect_lists_from_fn_body(fd, &mut list_types, &mut list_order, &mut next_idx);
        }
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_lists_from_str(ty, &mut list_types, &mut list_order, &mut next_idx);
            }
        }
        if handler_active && !list_types.contains_key("List<String>") {
            list_types.insert("List<String>".to_string(), next_idx);
            list_order.push("List<String>".to_string());
            next_idx += 1;
        }
        // `Disk.listDir` host import returns `Result<List<String>, String>` —
        // ensure the underlying `List<String>` slot exists even when no
        // user-fn signature mentions it.
        let needs_list_string_for_disk = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => fd.effects.iter().any(|e| e.node == "Disk.listDir"),
            _ => false,
        });
        if needs_list_string_for_disk && !list_types.contains_key("List<String>") {
            list_types.insert("List<String>".to_string(), next_idx);
            list_order.push("List<String>".to_string());
            next_idx += 1;
        }

        // Eager `Vector<T>` registration for every discovered
        // `List<T>` — `Vector.fromList(list_call())` is the canonical
        // way wumpus-shaped programs build a Vector, but the
        // expression-walker can't infer the produced Vector<T>
        // without typing the inner list call. Cheap overproduction
        // here (a slot per List<T>) is harmless; `wasm-opt -Oz`
        // strips the unused vector helpers, and the `Vector.fromList`
        // path can finally find its pre-allocated slot.
        for list_canonical in list_order.iter() {
            if let Some(elem) = TypeRegistry::list_element_type(list_canonical) {
                let vec_canonical = format!("Vector<{}>", elem.trim());
                if !vector_types.contains_key(&vec_canonical) {
                    vector_types.insert(vec_canonical.clone(), next_idx);
                    vector_order.push(vec_canonical);
                    next_idx += 1;
                }
            }
        }

        // `Option<T>` instantiations follow the same shape — scan
        // signatures + bodies for any `Option<T>` reference and
        // allocate a struct slot per unique `T`.
        let mut option_types: HashMap<String, u32> = HashMap::new();
        let mut option_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_options_from_str(
                &fd.return_type.display(),
                &mut option_types,
                &mut option_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_options_from_str(
                    &ty.display(),
                    &mut option_types,
                    &mut option_order,
                    &mut next_idx,
                );
            }
            collect_options_from_fn_body(fd, &mut option_types, &mut option_order, &mut next_idx);
        }
        // Record field walk — `record GameState { lastAiResult:
        // Option<AiResult> }` only spells `Option<AiResult>` in the
        // record declaration. Without this walk the canonical never
        // gets registered, and a `match state.lastAiResult` arm
        // dispatcher fails to recover its slot.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_options_from_str(ty, &mut option_types, &mut option_order, &mut next_idx);
            }
        }
        // Eagerly register `Option<T>` for every `Vector<T>` — a
        // `match Vector.get(v, i) { Option.Some(x) -> ...; Option.None -> ... }`
        // requires the boxed Option<T> slot, but the surface code
        // doesn't spell out `Option<String>` in any signature.
        for vec_canonical in &vector_order {
            if let Some(elem) = TypeRegistry::vector_element_type(vec_canonical) {
                let opt = format!("Option<{}>", elem.trim());
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
        }
        // Same eager registration for every user-defined record. The
        // common shape is `fn handleAiTurn(state: GameState) -> GameState`
        // whose body emits a fallback `Option.None` (e.g. inside a
        // `RecordCreate` field that's actually `Option<X>` but the
        // emit-time hint falls back to `ctx.return_type` = `GameState`).
        // Without `Option<GameState>` in the registry the constructor
        // crashes; with it the slot exists and `wasm-opt -Oz` strips
        // unused option helpers if nothing actually instantiates them.
        for record_name in &record_names_sorted {
            let opt = format!("Option<{record_name}>");
            if !option_types.contains_key(&opt) {
                option_types.insert(opt.clone(), next_idx);
                option_order.push(opt);
                next_idx += 1;
            }
        }
        // Eagerly register `Option<V>` for every `Map<K, V>` reachable
        // anywhere — `Map.get` returns `Option<V>` and the slot has
        // to land before the Map struct does so the wasm type section
        // can reference it without a forward edge. Pre-discover the
        // pending maps the same way the actual Map block does, then
        // grab each V.
        let mut pending_maps_for_options: Vec<String> = Vec::new();
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_maps_from_str(ty, &mut pending_maps_for_options);
            }
        }
        for fd in resolved_fn_defs {
            collect_maps_from_str(&fd.return_type.display(), &mut pending_maps_for_options);
            for (_, ty) in &fd.params {
                collect_maps_from_str(&ty.display(), &mut pending_maps_for_options);
            }
        }
        if handler_active
            && !pending_maps_for_options
                .iter()
                .any(|m| m == "Map<String,List<String>>")
        {
            pending_maps_for_options.push("Map<String,List<String>>".to_string());
        }
        let mut seen_map_v: std::collections::HashSet<String> = std::collections::HashSet::new();
        for canonical in &pending_maps_for_options {
            if let Some((_, v)) = parse_map_kv(canonical)
                && seen_map_v.insert(v.to_string())
            {
                let opt = format!("Option<{v}>");
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
        }

        // `Map<K, V>` discovery — same monomorphisation strategy as
        // Vector / Option. Walk fn signatures + bodies for any
        // `Map<K, V>` reference, allocate three wasm slots per unique
        // instantiation (keys array, values array, map struct), and
        // eagerly register the matching `Option<V>` since `Map.get`
        // returns it.
        let mut map_types: HashMap<String, MapSlots> = HashMap::new();
        let mut map_order: Vec<String> = Vec::new();
        let mut pending_maps: Vec<String> = Vec::new();
        // Built-in record fields contribute too — `HttpRequest.headers`
        // / `HttpResponse.headers` carry `Map<String, List<String>>`.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_maps_from_str(ty, &mut pending_maps);
            }
        }
        for fd in resolved_fn_defs {
            collect_maps_from_str(&fd.return_type.display(), &mut pending_maps);
            for (_, ty) in &fd.params {
                collect_maps_from_str(&ty.display(), &mut pending_maps);
            }
            // Walk let-binding annotations too — `let m: Map<Person, Int> =
            // Map.set(…)` won't show up in fn signatures and the discovery
            // walker would otherwise miss the canonical entirely. Mirrors
            // what the Result walker added a few commits back.
            use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
            let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
            for stmt in stmts {
                if let ResolvedStmt::Binding {
                    ty_ann: Some(annot),
                    ..
                } = stmt
                {
                    collect_maps_from_str(&annot.display(), &mut pending_maps);
                }
                let expr = match stmt {
                    ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => e,
                };
                collect_maps_from_expr(expr, &mut pending_maps);
            }
        }
        // Dedup in encounter order.
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        for canonical in pending_maps {
            if !seen.insert(canonical.clone()) {
                continue;
            }
            // Eagerly register Option<V> — `Map.get` over this
            // instantiation returns it.
            if let Some((_, v)) = parse_map_kv(&canonical) {
                let opt = format!("Option<{v}>");
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
            // Allocate three slots: keys_array, values_array, map.
            // Order: arrays first so the struct (higher idx) can
            // reference them without crossing rec-group boundaries.
            let keys_array = next_idx;
            next_idx += 1;
            let values_array = next_idx;
            next_idx += 1;
            let map = next_idx;
            next_idx += 1;
            map_types.insert(
                canonical.clone(),
                MapSlots {
                    keys_array,
                    values_array,
                    map,
                },
            );
            map_order.push(canonical);
        }

        // Eagerly register the primitive-key box struct for every
        // `Map<K, *>` where K ∈ {Int, Float, Bool}. The map's
        // open-addressing layout uses ref-null as the empty marker,
        // and primitive K can't carry that — boxing the raw value
        // into `(struct (mut K))` lets the marker stay uniform.
        let mut primitive_key_box: HashMap<String, u32> = HashMap::new();
        let mut primitive_key_box_order: Vec<String> = Vec::new();
        for canonical in map_order.iter() {
            if let Some((k, _)) = parse_map_kv(canonical) {
                let k_trim = k.trim();
                if matches!(k_trim, "Int" | "Float" | "Bool")
                    && !primitive_key_box.contains_key(k_trim)
                {
                    primitive_key_box.insert(k_trim.to_string(), next_idx);
                    primitive_key_box_order.push(k_trim.to_string());
                    next_idx += 1;
                }
            }
        }

        // Eagerly register `List<K>` and `List<V>` for every
        // `Map<K, V>` — `Map.keys` / `Map.values` return them but
        // the canonical never appears anywhere else. Same trick as
        // the eager `Option<V>` above. Single rec group makes
        // forward refs from existing types to these new lists OK.
        for canonical in map_order.iter() {
            if let Some((k, v)) = parse_map_kv(canonical) {
                for elem in [k.trim(), v.trim()] {
                    let lst = format!("List<{elem}>");
                    if !list_types.contains_key(&lst) {
                        list_types.insert(lst.clone(), next_idx);
                        list_order.push(lst);
                        next_idx += 1;
                    }
                }
            }
        }

        // `Tuple<A, B>` discovery — fn signatures + record fields +
        // body annotations. Discovery walks the same shape as
        // results/options.
        let mut tuple_types: HashMap<String, u32> = HashMap::new();
        let mut tuple_order: Vec<String> = Vec::new();
        for fd in resolved_fn_defs {
            collect_tuples_from_str(
                &fd.return_type.display(),
                &mut tuple_types,
                &mut tuple_order,
                &mut next_idx,
            );
            for (_, ty) in &fd.params {
                collect_tuples_from_str(
                    &ty.display(),
                    &mut tuple_types,
                    &mut tuple_order,
                    &mut next_idx,
                );
            }
            // Walk the body for `ResolvedExpr::Tuple` literals — the
            // canonical `Tuple<A,B>` for a `(a, b)` literal is
            // built from the items' typed-AST element types and
            // never has to appear in any signature.
            collect_tuples_from_fn_body(fd, &mut tuple_types, &mut tuple_order, &mut next_idx);
        }
        // Record fields can carry tuple types too.
        for name in &record_names_sorted {
            let fields = &record_fields[name];
            for (_, ty) in fields {
                collect_tuples_from_str(ty, &mut tuple_types, &mut tuple_order, &mut next_idx);
            }
        }
        // Eagerly register `Tuple<K, V>` for every `Map<K, V>` —
        // `Map.entries` returns `List<Tuple<K, V>>` and `Map.fromList`
        // takes one. Plus the matching `List<Tuple<K, V>>`.
        for canonical in map_order.iter() {
            if let Some((k, v)) = parse_map_kv(canonical) {
                let tup = format!("Tuple<{},{}>", k.trim(), v.trim());
                if !tuple_types.contains_key(&tup) {
                    tuple_types.insert(tup.clone(), next_idx);
                    tuple_order.push(tup.clone());
                    next_idx += 1;
                }
                let lst = format!("List<{tup}>");
                if !list_types.contains_key(&lst) {
                    list_types.insert(lst.clone(), next_idx);
                    list_order.push(lst);
                    next_idx += 1;
                }
            }
        }
        // Eagerly register `List<Tuple<A, B>>` for every `Tuple<A, B>`
        // — `List.zip` returns it and `Map.fromList` takes it. Some
        // tuples come straight from fn signatures; this catches both.
        for canonical in tuple_order.clone().iter() {
            let lst = format!("List<{canonical}>");
            if !list_types.contains_key(&lst) {
                list_types.insert(lst.clone(), next_idx);
                list_order.push(lst);
                next_idx += 1;
            }
        }

        // Eagerly register `List<T>` and `Option<Vector<T>>` for
        // every `Vector<T>` — `List.fromVector` returns the list and
        // `Vector.set` (boxed shape) returns the option. Both
        // canonical-types only appear in those builtin returns.
        for canonical in vector_order.iter() {
            if let Some(elem) = TypeRegistry::vector_element_type(canonical) {
                let elem = elem.trim();
                let lst = format!("List<{elem}>");
                if !list_types.contains_key(&lst) {
                    list_types.insert(lst.clone(), next_idx);
                    list_order.push(lst);
                    next_idx += 1;
                }
                let opt = format!("Option<{canonical}>");
                if !option_types.contains_key(&opt) {
                    option_types.insert(opt.clone(), next_idx);
                    option_order.push(opt);
                    next_idx += 1;
                }
            }
        }

        // Now that String / List / Map slots all exist, slot-assign
        // the built-in records — they reference those types in their
        // fields, so the struct-type emit needs them at lower indices.
        for name in &builtin_record_names {
            records.insert(name.clone(), next_idx);
            next_idx += 1;
        }

        // Discover unique String literals — each gets a passive data
        // segment idx assigned in encounter order. Walk fn bodies + any
        // string literals embedded in expressions; canonicalise on
        // raw byte content (Aver strings are UTF-8).
        let mut string_literals: Vec<Vec<u8>> = Vec::new();
        let mut string_literal_idx: HashMap<Vec<u8>, u32> = HashMap::new();
        let _ = handler_active;
        for fd in resolved_fn_defs {
            collect_string_literals_in_fn(fd, &mut string_literals, &mut string_literal_idx);
        }
        // Note: caller_fn name registration moved out of `TypeRegistry`
        // — `body::CallerFnCollector` lazy-registers names during
        // codegen (every site that calls `emit_caller_fn_idx` registers
        // its self_fn_name on demand), and the post-emit phase appends
        // the resulting names as fresh passive segments after the
        // pre-walked literal segments above. Single source of truth,
        // zero AST-walker false positives.
        // `Int.mod` lowers to a boxed `Result<Int, String>` whose Err
        // arm carries a fixed "Division by zero" message — register
        // its bytes as a passive segment so the emitter has an idx
        // to pull at struct.new time, regardless of whether user
        // source ever spells the literal.
        let int_mod_used = resolved_fn_defs.iter().any(fn_body_calls_int_mod);
        if int_mod_used {
            let bytes = b"Division by zero".to_vec();
            string_literal_idx.entry(bytes.clone()).or_insert_with(|| {
                let idx = string_literals.len() as u32;
                string_literals.push(bytes);
                idx
            });
        }

        // Phase 4.2.1 (0.20) — register the placeholder error
        // message the `__rt_tcp_connect` stub returns until the real
        // DNS/connect/finish pipeline lands. Gated on `needs_tcp`
        // (any fn declares a Tcp.* effect) so non-TCP programs don't
        // carry the literal.
        if needs_tcp {
            for msg in [
                b"tcp: connect not yet implemented".as_ref(),
                b"tcp: dns resolve failed".as_ref(),
                b"tcp: dns no addresses".as_ref(),
                b"tcp: socket create failed".as_ref(),
                b"tcp: connect failed".as_ref(),
                b"tcp: write failed".as_ref(),
                b"tcp: eof".as_ref(),
                // Phase 4.7+ — aver-rt cross-backend alignment.
                // VM / self-host / wasm-gc all return `Err("Tcp.X:
                // unknown connection 'tcp-N'")` on stale handles;
                // wasip2 matches the prefix shape (we drop the
                // method name since one segment serves close /
                // writeLine / readLine).
                b"tcp: unknown connection".as_ref(),
                // Phase 4.7+ — port validation. VM message verbatim
                // (`Tcp: port N is out of range (0\u{2013}65535)`)
                // is parameterised on the port value; we ship a
                // canned message instead because the Err string is
                // built from a static data segment, not concat.
                b"tcp: port out of range".as_ref(),
                // Phase 4.7+ fix #10 — pool-limit alignment with
                // `aver-rt::tcp::connect`. Pool is 256 slots; the
                // 257th live connect must refuse rather than evict
                // the slot's existing live occupant.
                b"tcp: connection limit reached (256 max)".as_ref(),
                // Phase 4.7+ fix #17 — `Tcp.send` `stream-error.
                // last-operation-failed`. The wasi:io read variant
                // distinguishes I/O errors from a clean half-close;
                // wasip2 used to fold both into a partial-Ok return,
                // mismatching `aver-rt::tcp::send`'s explicit Err.
                b"tcp: stream error".as_ref(),
                // Phase 4.7+ fix #18 — `Tcp.send` response cap.
                // `aver-rt::tcp::send` caps at 10 MiB; wasip2 used
                // to grow the buffer unbounded.
                b"tcp: response exceeds 10 MiB limit".as_ref(),
            ] {
                let bytes = msg.to_vec();
                string_literal_idx.entry(bytes.clone()).or_insert_with(|| {
                    let idx = string_literals.len() as u32;
                    string_literals.push(bytes);
                    idx
                });
            }
        }

        // Mark every record/variant used as a `Map<K, *>` key as
        // non-newtypable so it stays a struct ref in the type
        // section — the open-addressing layout's `keys[i] == null`
        // empty marker requires that.
        let mut non_newtypable_keys: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for canonical in map_order.iter() {
            if let Some((k, _)) = parse_map_kv(canonical) {
                let k_trim = k.trim();
                if record_fields.contains_key(k_trim)
                    || variants
                        .values()
                        .flat_map(|v| v.iter())
                        .any(|v| v.parent == k_trim)
                {
                    non_newtypable_keys.insert(k_trim.to_string());
                }
            }
        }

        Self {
            records,
            variants,
            record_fields,
            vector_types,
            vector_order,
            option_types,
            option_order,
            list_types,
            list_order,
            result_types,
            result_order,
            map_types,
            map_order,
            tuple_types,
            tuple_order,
            primitive_key_box,
            primitive_key_box_order,
            user_type_count: next_idx,
            string_array_type_idx,
            string_literals,
            string_literal_idx,
            non_newtypable_keys,
            tcp_slot_type_idx,
            tcp_pool_type_idx,
        }
    }

    pub(super) fn list_type_idx(&self, canonical: &str) -> Option<u32> {
        let normalized = normalize_compound(canonical);
        if let Some(idx) = self.list_types.get(&normalized).copied() {
            return Some(idx);
        }
        // Cross-module: a fn signature may spell a record as
        // `Module.Room` while the dep's source uses bare `Room`. The
        // type registry only has one slot per record (keyed by the
        // bare name), so strip dotted prefixes from inner type names
        // and retry.
        let bare = strip_inner_dotted_prefixes(&normalized);
        if bare != normalized {
            self.list_types.get(&bare).copied()
        } else {
            None
        }
    }

    pub(super) fn list_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("List<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    pub(super) fn result_type_idx(&self, canonical: &str) -> Option<u32> {
        if let Some(idx) = self.result_types.get(canonical).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.result_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Split `Result<T, E>` into (T, E) borrowed slices.
    pub(super) fn result_te(canonical: &str) -> Option<(&str, &str)> {
        let inner = canonical
            .trim()
            .strip_prefix("Result<")?
            .strip_suffix('>')?;
        let bytes = inner.as_bytes();
        // Track both angle-bracket and paren depth — `Result<(A, B), E>`
        // (T = a tuple) has a top-level comma inside the parens we
        // need to skip past.
        let mut depth: i32 = 0;
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'<' | b'(' => depth += 1,
                b'>' | b')' => depth -= 1,
                b',' if depth == 0 => {
                    return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
                }
                _ => {}
            }
        }
        None
    }

    pub(super) fn map_slots(&self, canonical: &str) -> Option<MapSlots> {
        if let Some(s) = self.map_types.get(canonical).copied() {
            return Some(s);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.map_types.get(&bare).copied()
        } else {
            None
        }
    }

    pub(super) fn primitive_key_box_idx(&self, k_aver: &str) -> Option<u32> {
        self.primitive_key_box.get(k_aver.trim()).copied()
    }

    /// True for K kinds that need to be boxed when used as a
    /// `Map<K, *>` key (so the keys array element type stays a ref
    /// and `keys[i] == null` works as the empty marker).
    pub(super) fn is_primitive_map_key(k_aver: &str) -> bool {
        matches!(k_aver.trim(), "Int" | "Float" | "Bool")
    }

    pub(super) fn tuple_type_idx(&self, canonical: &str) -> Option<u32> {
        // Accept both `Tuple<A,B>` (internal canonical) and `(A, B)`
        // (Aver surface syntax). Normalize the latter, strip
        // whitespace, then look up.
        let normalized = normalize_tuple_canonical(canonical);
        if let Some(idx) = self.tuple_types.get(normalized.as_ref()).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(normalized.as_ref());
        if bare.as_str() != normalized.as_ref() {
            self.tuple_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Split `Tuple<A, B, C, ...>` (or `(A, B, C, ...)`) into the full
    /// element list. Depth-aware comma scan handles nested generics
    /// like `Tuple<Map<String, Int>, List<Int>>`. Returns slices of
    /// the original `canonical` so callers don't allocate; arity is
    /// arbitrary (no hardcoded 2-cap).
    pub(super) fn tuple_elements(canonical: &str) -> Option<Vec<&str>> {
        let trimmed = canonical.trim();
        let inner = if let Some(i) = trimmed
            .strip_prefix("Tuple<")
            .and_then(|s| s.strip_suffix('>'))
        {
            i
        } else if trimmed.starts_with('(') && trimmed.ends_with(')') {
            &trimmed[1..trimmed.len() - 1]
        } else {
            return None;
        };
        let bytes = inner.as_bytes();
        let mut depth: i32 = 0;
        let mut start = 0usize;
        let mut out: Vec<&str> = Vec::new();
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'(' | b'<' => depth += 1,
                b')' | b'>' => depth -= 1,
                b',' if depth == 0 => {
                    out.push(inner[start..idx].trim());
                    start = idx + 1;
                }
                _ => {}
            }
        }
        out.push(inner[start..].trim());
        if out.len() < 2 { None } else { Some(out) }
    }

    /// Convenience wrapper returning the first two elements as a pair —
    /// kept for callers that genuinely only need `(A, B)` (e.g.
    /// `List.zip` lowering, the existing 2-tuple `Map.entries`
    /// canonicals). For variadic destructure use `tuple_elements`.
    pub(super) fn tuple_ab(canonical: &str) -> Option<(&str, &str)> {
        let elems = Self::tuple_elements(canonical)?;
        if elems.len() == 2 {
            Some((elems[0], elems[1]))
        } else {
            None
        }
    }

    pub(super) fn option_type_idx(&self, canonical: &str) -> Option<u32> {
        if let Some(idx) = self.option_types.get(canonical).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.option_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Element-type Aver string for a registered `Option<T>` (analog
    /// to `vector_element_type`).
    pub(super) fn option_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("Option<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    /// Passive-data-segment idx for a String literal, allocated during
    /// `build`. Each unique byte sequence gets one segment.
    pub(super) fn string_literal_segment(&self, bytes: &[u8]) -> Option<u32> {
        self.string_literal_idx.get(bytes).copied()
    }

    /// Wasm type idx for a canonical Aver `Vector<T>` string, if the
    /// instantiation was registered during `build`.
    pub(super) fn vector_type_idx(&self, canonical: &str) -> Option<u32> {
        if let Some(idx) = self.vector_types.get(canonical).copied() {
            return Some(idx);
        }
        let bare = strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.vector_types.get(&bare).copied()
        } else {
            None
        }
    }

    /// Element-type Aver string for a registered `Vector<T>`. Used by
    /// module emit to resolve the wasm storage type of array elements.
    pub(super) fn vector_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("Vector<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    pub(super) fn record_type_idx(&self, name: &str) -> Option<u32> {
        // Direct lookup. After multi-module flatten the discovery
        // walker registers records under either the bare name (`Room`)
        // or the qualified name (`Level.Room`); the type checker
        // sometimes hands us the qualified form even when the
        // registry only knows the bare one (or vice versa). Try both.
        if let Some(idx) = self.records.get(name).copied() {
            return Some(idx);
        }
        if let Some(bare) = name.rsplit_once('.').map(|(_, b)| b)
            && let Some(idx) = self.records.get(bare).copied()
        {
            return Some(idx);
        }
        None
    }

    /// Look up a variant by bare name. Returns the first registered
    /// entry — fine when the name is unambiguous across the whole
    /// program (the common case). Callers that know the parent
    /// sumtype should use `variant_in` to disambiguate.
    pub(super) fn variant(&self, name: &str) -> Option<&VariantInfo> {
        self.variants.get(name).and_then(|v| v.first())
    }

    /// Look up a variant in a specific parent sumtype. Use this when
    /// the call site has the full `ParentType.VariantName` shape (a
    /// `Pattern::Constructor` / dotted-Attr / dotted FnCall callee)
    /// so the same bare variant name in a sibling sumtype doesn't
    /// silently shadow the right one.
    pub(super) fn variant_in(&self, parent: &str, name: &str) -> Option<&VariantInfo> {
        self.variants.get(name)?.iter().find(|v| v.parent == parent)
    }

    pub(super) fn record_field_index(&self, record: &str, field: &str) -> Option<u32> {
        // Same Module.Name → Name fallback as `record_type_idx`.
        let fields = self.record_fields.get(record).or_else(|| {
            record
                .rsplit_once('.')
                .and_then(|(_, b)| self.record_fields.get(b))
        })?;
        fields
            .iter()
            .position(|(n, _)| n == field)
            .map(|i| i as u32)
    }

    #[allow(dead_code)]
    pub(super) fn record_field_type(&self, record: &str, field: &str) -> Option<&str> {
        let fields = self.record_fields.get(record).or_else(|| {
            record
                .rsplit_once('.')
                .and_then(|(_, b)| self.record_fields.get(b))
        })?;
        fields
            .iter()
            .find(|(n, _)| n == field)
            .map(|(_, t)| t.as_str())
    }

    /// Newtype optimization: a `record Foo { x: T }` (single primitive
    /// field) or `type Foo = Foo(T)` (single-variant sum, single primitive
    /// payload) is structurally equivalent to `T`. We erase the wrapper
    /// at the wasm level — every `Foo` slot carries `T` directly,
    /// `RecordCreate { Foo, x = e }` lowers to just `e`, `Attr(_, x)`
    /// lowers to identity, `match obj { Foo.Foo(n) -> body }` binds `n`
    /// to the underlying `T` value with no `struct.get`. Same trick
    /// rustc uses for `struct UserId(u64)`.
    pub(super) fn newtype_underlying(&self, type_name: &str) -> Option<&str> {
        // Suppress newtype optimisation when the type is used as a
        // `Map<K, *>` key. Map's open-addressing layout uses
        // `keys[i] == null` as the empty marker, which only works
        // when keys land in `keys` as ref values — newtyping a key
        // record down to its underlying primitive (e.g. i64) would
        // strip the ref and break the marker.
        if self.non_newtypable_keys.contains(type_name) {
            return None;
        }
        // Record case: exactly one field, primitive type.
        if let Some(fields) = self.record_fields.get(type_name)
            && fields.len() == 1
            && is_primitive(&fields[0].1)
        {
            return Some(fields[0].1.as_str());
        }
        // Sum case: parent has exactly one variant, that variant has
        // exactly one field, that field is primitive.
        let mut variants_of_parent = self
            .variants
            .values()
            .flat_map(|v| v.iter())
            .filter(|v| v.parent == type_name);
        if let Some(only) = variants_of_parent.next()
            && variants_of_parent.next().is_none()
            && only.fields.len() == 1
            && is_primitive(&only.fields[0])
        {
            return Some(only.fields[0].as_str());
        }
        None
    }
}

/// Split a canonical `Map<K, V>` into its `K` and `V` parts (both
/// borrowed slices of the input). Returns `None` if the string
/// doesn't match the expected shape.
pub(super) fn parse_map_kv(canonical: &str) -> Option<(&str, &str)> {
    let inner = canonical.trim().strip_prefix("Map<")?.strip_suffix('>')?;
    let bytes = inner.as_bytes();
    // Track both angle-bracket and paren depth so `Map<(A, B), V>`
    // (tuple key) splits at the right comma.
    let mut depth: i32 = 0;
    for (idx, b) in bytes.iter().enumerate() {
        match b {
            b'<' | b'(' => depth += 1,
            b'>' | b')' => depth -= 1,
            b',' if depth == 0 => {
                return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
            }
            _ => {}
        }
    }
    None
}

/// True if any expression in the fn body produces a String value —
/// via a literal, an interpolation, or a String-producing builtin.
/// Used by `TypeRegistry::build` to decide whether to allocate the
/// `(array i8)` slot.
fn fn_body_produces_string(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| match s {
        ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => expr_uses_string(&e.node),
    })
}

fn expr_uses_string(expr: &crate::ir::hir::ResolvedExpr) -> bool {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match expr {
        ResolvedExpr::Call(callee, args) => {
            if let ResolvedCallee::Builtin(dotted) = callee
                && matches!(
                    dotted.as_str(),
                    "String.fromInt"
                            | "String.fromFloat"
                            | "String.len"
                            | "String.length"
                            | "String.startsWith"
                            | "String.contains"
                            | "String.slice"
                            | "String.toUpper"
                            | "String.toLower"
                            | "String.trim"
                            | "String.replace"
                            | "String.split"
                            | "String.join"
                            | "String.fromBool"
                            | "String.endsWith"
                            | "String.charAt"
                            | "String.chars"
                            | "String.byteLength"
                            | "Char.toCode"
                            | "Char.fromCode"
                            | "Byte.toHex"
                            // `Int.mod`, `Int.fromString`, `Float.fromString`,
                            // `Byte.fromHex` return Result<_, String> —
                            // touching them forces the String slot for
                            // the error payload even when the program
                            // never reads the Err arm.
                            | "Int.mod"
                            | "Int.fromString"
                            | "Float.fromString"
                            | "Byte.fromHex"
                            // Effects that produce or consume String at
                            // their boundary. The string slot has to be
                            // allocated whenever any of these is called
                            // — without that, the import signature
                            // can't even be emitted (`(ref null
                            // $string)` references an undeclared type).
                            // Mirror of `EffectName::*` whose typed
                            // signature in `effects.rs` uses
                            // `string_ref_ty`. Keep in sync.
                            | "Console.print"
                            | "Console.error"
                            | "Console.warn"
                            | "Console.readLine"
                            | "Args.get"
                            | "Args._get"
                            | "Env.get"
                            | "Env.set"
                            | "Time.now"
                            | "Request.method"
                            | "Request.url"
                            | "Request.path"
                            | "Request.query"
                            | "Request.body"
                            | "Request.headersLoad"
                            | "Request.headers"
                            | "Response.text"
                            | "Response.setHeader"
                            | "Http.send"
                            | "Http.addRequestHeader"
                            | "Terminal.print"
                            | "Terminal.setColor"
                            | "Terminal.readKey"
                )
            {
                return true;
            }
            args.iter().any(|a| expr_uses_string(&a.node))
        }
        ResolvedExpr::BinOp(_, l, r) => expr_uses_string(&l.node) || expr_uses_string(&r.node),
        ResolvedExpr::Neg(inner) => expr_uses_string(&inner.node),
        ResolvedExpr::Match { subject, arms } => {
            expr_uses_string(&subject.node) || arms.iter().any(|a| expr_uses_string(&a.body.node))
        }
        ResolvedExpr::TailCall { args, .. } => args.iter().any(|a| expr_uses_string(&a.node)),
        ResolvedExpr::Attr(obj, _) => expr_uses_string(&obj.node),
        ResolvedExpr::Ctor(_, args) => args.iter().any(|a| expr_uses_string(&a.node)),
        ResolvedExpr::RecordCreate { fields, .. } => {
            fields.iter().any(|(_, e)| expr_uses_string(&e.node))
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            expr_uses_string(&base.node) || updates.iter().any(|(_, e)| expr_uses_string(&e.node))
        }
        ResolvedExpr::Literal(crate::ast::Literal::Str(_)) => true,
        ResolvedExpr::InterpolatedStr(_) => true,
        _ => false,
    }
}

/// Walk a fn body, collecting unique String literals into a per-segment
/// table. Both `Literal::Str` and the `Literal` parts of an
/// `InterpolatedStr` count — each unique byte sequence gets a passive
/// data segment.
fn fn_body_calls_int_mod(fd: &crate::ir::hir::ResolvedFnDef) -> bool {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedFnBody, ResolvedStmt};
    fn walk(e: &ResolvedExpr) -> bool {
        match e {
            ResolvedExpr::Call(callee, args) => {
                let hit = matches!(callee, ResolvedCallee::Builtin(name) if name == "Int.mod");
                hit || args.iter().any(|a| walk(&a.node))
            }
            ResolvedExpr::Match { subject, arms } => {
                walk(&subject.node) || arms.iter().any(|a| walk(&a.body.node))
            }
            ResolvedExpr::BinOp(_, l, r) => walk(&l.node) || walk(&r.node),
            ResolvedExpr::Neg(inner) => walk(&inner.node),
            ResolvedExpr::Attr(o, _) => walk(&o.node),
            ResolvedExpr::ErrorProp(i) => walk(&i.node),
            ResolvedExpr::TailCall { args, .. } => args.iter().any(|a| walk(&a.node)),
            ResolvedExpr::List(xs)
            | ResolvedExpr::Tuple(xs)
            | ResolvedExpr::IndependentProduct(xs, _) => xs.iter().any(|x| walk(&x.node)),
            ResolvedExpr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| walk(&e.node)),
            ResolvedExpr::RecordUpdate { base, updates, .. } => {
                walk(&base.node) || updates.iter().any(|(_, e)| walk(&e.node))
            }
            ResolvedExpr::Ctor(_, args) => args.iter().any(|a| walk(&a.node)),
            _ => false,
        }
    }
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|stmt| match stmt {
        ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => walk(&e.node),
    })
}

fn collect_string_literals_in_fn(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut Vec<Vec<u8>>,
    idx: &mut HashMap<Vec<u8>, u32>,
) {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => &e.node,
        };
        collect_string_literals_in_expr(expr, out, idx);
    }
}

fn intern_literal(bytes: Vec<u8>, out: &mut Vec<Vec<u8>>, idx: &mut HashMap<Vec<u8>, u32>) {
    if !idx.contains_key(&bytes) {
        let n = out.len() as u32;
        idx.insert(bytes.clone(), n);
        out.push(bytes);
    }
}

fn collect_string_literals_in_expr(
    expr: &crate::ir::hir::ResolvedExpr,
    out: &mut Vec<Vec<u8>>,
    idx: &mut HashMap<Vec<u8>, u32>,
) {
    use crate::ast::Literal;
    use crate::ir::hir::{ResolvedExpr, ResolvedStrPart};
    match expr {
        ResolvedExpr::Literal(Literal::Str(s)) => intern_literal(s.as_bytes().to_vec(), out, idx),
        ResolvedExpr::InterpolatedStr(parts) => {
            for p in parts {
                match p {
                    ResolvedStrPart::Literal(s) => intern_literal(s.as_bytes().to_vec(), out, idx),
                    ResolvedStrPart::Parsed(inner) => {
                        collect_string_literals_in_expr(&inner.node, out, idx);
                    }
                }
            }
        }
        ResolvedExpr::Call(_, args) => {
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_string_literals_in_expr(&l.node, out, idx);
            collect_string_literals_in_expr(&r.node, out, idx);
        }
        ResolvedExpr::Neg(inner) => collect_string_literals_in_expr(&inner.node, out, idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_string_literals_in_expr(&subject.node, out, idx);
            for a in arms {
                if let crate::ir::hir::ResolvedPattern::Literal(Literal::Str(s)) = &a.pattern {
                    intern_literal(s.as_bytes().to_vec(), out, idx);
                }
                collect_string_literals_in_expr(&a.body.node, out, idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_string_literals_in_expr(&obj.node, out, idx),
        ResolvedExpr::ErrorProp(inner) => collect_string_literals_in_expr(&inner.node, out, idx),
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_string_literals_in_expr(&e.node, out, idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_string_literals_in_expr(&base.node, out, idx);
            for (_, e) in updates {
                collect_string_literals_in_expr(&e.node, out, idx);
            }
        }
        ResolvedExpr::List(items) => {
            for item in items {
                collect_string_literals_in_expr(&item.node, out, idx);
            }
        }
        ResolvedExpr::Tuple(items) | ResolvedExpr::IndependentProduct(items, _) => {
            for item in items {
                collect_string_literals_in_expr(&item.node, out, idx);
            }
        }
        ResolvedExpr::MapLiteral(entries) => {
            for (k, v) in entries {
                collect_string_literals_in_expr(&k.node, out, idx);
                collect_string_literals_in_expr(&v.node, out, idx);
            }
        }
        _ => {}
    }
}

/// Resolve an Aver type-annotation string to a wasm value type, or to
/// "no result" when the type is `Unit`. User-type names look up the
/// registry and return a nullable struct ref.
pub(super) fn aver_to_wasm(
    type_str: &str,
    registry: Option<&TypeRegistry>,
) -> Result<Option<ValType>, WasmGcError> {
    let trimmed = type_str.trim();
    if let Some(v) = primitive_to_wasm(trimmed) {
        return Ok(Some(v));
    }
    if trimmed == "Unit" {
        return Ok(None);
    }
    if let Some(reg) = registry {
        // Newtype optimization — a single-field record / single-variant
        // sum of a primitive lowers to the underlying primitive
        // everywhere. Saves an allocation per wrap and a struct.get
        // per unwrap.
        if let Some(underlying) = reg.newtype_underlying(trimmed) {
            return Ok(primitive_to_wasm(underlying));
        }
        if let Some(idx) = reg.record_type_idx(trimmed) {
            return Ok(Some(struct_ref(idx)));
        }
        // Sum type by parent name — represented as the abstract `eq`
        // ref so any variant subtype lands in the same slot. Each
        // variant constructor's type idx still emits a concrete
        // struct.new; the parent ref shape is what params/locals
        // declare.
        // Variant parent match: tolerate "Types.Tile" coming in from
        // a canonicalised type stamp when the discovery walker
        // registered the variants under bare "Tile" (Iron — A3).
        let trimmed_bare = trimmed.rsplit_once('.').map_or(trimmed, |(_, b)| b);
        if reg
            .variants
            .values()
            .flat_map(|v| v.iter())
            .any(|v| v.parent == trimmed || v.parent == trimmed_bare)
        {
            // Phase-3a: use `(ref null eq)` as the carrier — every
            // wasm-gc struct is a subtype of `eq`. Real subtype
            // hierarchies (where pattern matching tests `ref.test`
            // against concrete struct types) lands in phase 3b.
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Abstract {
                    shared: false,
                    ty: AbstractHeapType::Eq,
                },
            })));
        }
    }
    // String maps to `(ref null (array i8))` when the registry has
    // pre-allocated the array type during `build`. Unique-pointer
    // semantics aren't needed; nullable is fine because Aver's type
    // system already proves String values are non-null.
    if trimmed == "String" {
        if let Some(reg) = registry
            && let Some(idx) = reg.string_array_type_idx
        {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
        return Err(WasmGcError::Validation(
            "String reachable from a fn signature but no string type slot was allocated".into(),
        ));
    }
    // `Vector<T>` resolves to `(ref null $vector_T)`. The registry's
    // `vector_types` map is keyed on whitespace-stripped canonical
    // form so `Vector<Int>` and `Vector< Int >` collide on the same
    // slot.
    if trimmed.starts_with("Vector<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.vector_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `Option<T>` resolves to `(ref null $option_T)`.
    if trimmed.starts_with("Option<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.option_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `List<T>` — recursive Cons cell `(struct (T) (ref null $list_T))`.
    // Empty list = null ref.
    if trimmed.starts_with("List<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical = normalize_compound(trimmed);
        if let Some(idx) = reg.list_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `Result<T, E>` — `(struct (mut i32 tag) (mut T ok) (mut E err))`.
    if trimmed.starts_with("Result<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.result_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `Map<K, V>` — monomorphised per instantiation. The registry
    // discovers each unique `Map<K, V>` in fn signatures and
    // allocates a slot triple (keys array, values array, struct).
    if trimmed.starts_with("Map<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(slots) = reg.map_slots(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.map),
            })));
        }
    }
    // `Tuple<A, B>` — `(struct (mut A) (mut B))`.
    if trimmed.starts_with("Tuple<")
        && trimmed.ends_with('>')
        && let Some(reg) = registry
    {
        let canonical: String = trimmed.chars().filter(|c| !c.is_whitespace()).collect();
        if let Some(idx) = reg.tuple_type_idx(&canonical) {
            return Ok(Some(ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(idx),
            })));
        }
    }
    // `(A, B)` surface form for Tuple. Normalize to `Tuple<A,B>`
    // canonical for the registry lookup.
    if trimmed.starts_with('(')
        && trimmed.ends_with(')')
        && let Some(reg) = registry
    {
        let inner = &trimmed[1..trimmed.len() - 1];
        // Quick check: must contain at least one top-level comma.
        let mut depth: i32 = 0;
        let mut has_top_comma = false;
        for b in inner.as_bytes() {
            match b {
                b'(' | b'<' => depth += 1,
                b')' | b'>' => depth -= 1,
                b',' if depth == 0 => has_top_comma = true,
                _ => {}
            }
        }
        if has_top_comma {
            let canonical_inner: String = inner.chars().filter(|c| !c.is_whitespace()).collect();
            let canonical = format!("Tuple<{canonical_inner}>");
            if let Some(idx) = reg.tuple_type_idx(&canonical) {
                return Ok(Some(ValType::Ref(RefType {
                    nullable: true,
                    heap_type: HeapType::Concrete(idx),
                })));
            }
        }
    }
    // Built-in opaque types — `BranchPath`, `Trace`, `EffectEvent`
    // are introduced by the verify / oracle / effect-lifting pipeline
    // and only reach runtime fns whose bodies are dead from a `_start`
    // perspective. The legacy `--target wasm` backend implicitly hides
    // them inside its tagged-i64 fallback; on wasm-gc we lower the
    // param/return slot to `(ref null eq)` so the fn signature is
    // representable without committing to a concrete struct shape.
    if matches!(trimmed, "BranchPath" | "Trace" | "EffectEvent") {
        return Ok(Some(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Abstract {
                shared: false,
                ty: AbstractHeapType::Eq,
            },
        })));
    }

    // First-class Fn types reach the wasm-gc backend only through
    // verify / oracle givens (`fn pairSpec(rnd: Fn(BranchPath, Int,
    // Int, Int) -> Int) -> ...`). The body is dead from `_start` —
    // verify blocks never execute in runtime — so the param slot just
    // needs *some* representable carrier. `(ref null eq)` works the
    // same way it does for `BranchPath`. Higher-order calls in real
    // runtime code aren't supported on wasm-gc yet; if they reach
    // here through `_start`, the validator will reject them at the
    // call site and the user gets a clear error.
    if trimmed.starts_with("Fn(") {
        return Ok(Some(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Abstract {
                shared: false,
                ty: AbstractHeapType::Eq,
            },
        })));
    }

    // Compound types not yet lowered.
    Err(WasmGcError::Validation(format!(
        "aver_to_wasm: cannot lower type `{trimmed}` to a wasm representation"
    )))
}

fn primitive_to_wasm(name: &str) -> Option<ValType> {
    match name {
        "Int" => Some(ValType::I64),
        "Float" => Some(ValType::F64),
        "Bool" => Some(ValType::I32),
        _ => None,
    }
}

/// `(ref null $idx)` — nullable reference to a struct type. Aver doesn't
/// have null at the user level; the nullability is a phase-3 concession
/// because wasm-encoder's struct.new with non-null refs requires more
/// init plumbing than we have today.
pub(super) fn struct_ref(type_idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(type_idx),
    })
}

/// Result-list shape for a wasm function signature derived from an
/// Aver return type.
pub(super) fn return_results(
    type_str: &str,
    registry: Option<&TypeRegistry>,
) -> Result<Vec<ValType>, WasmGcError> {
    Ok(aver_to_wasm(type_str, registry)?.into_iter().collect())
}

/// Param-list shape for a wasm function signature.
pub(super) fn param_types(
    params: &[(String, String)],
    registry: Option<&TypeRegistry>,
) -> Result<Vec<ValType>, WasmGcError> {
    let mut out = Vec::with_capacity(params.len());
    for (_, ty) in params {
        if let Some(v) = aver_to_wasm(ty, registry)? {
            out.push(v);
        }
    }
    Ok(out)
}

/// Build the `StructType` body for a record: one `FieldType` per
/// declared field, mutable=false (Aver records are immutable; `update`
/// returns a fresh struct via `struct.new`).
pub(super) fn record_struct_type(
    fields: &[(String, String)],
    registry: &TypeRegistry,
) -> Result<StructType, WasmGcError> {
    let mut out = Vec::with_capacity(fields.len());
    for (_, ty) in fields {
        let val_ty = aver_to_wasm(ty, Some(registry))?.ok_or(WasmGcError::Validation(format!(
            "record field of type {ty} has no wasm representation"
        )))?;
        out.push(FieldType {
            element_type: StorageType::Val(val_ty),
            mutable: false,
        });
    }
    Ok(StructType {
        fields: out.into_boxed_slice(),
    })
}

/// Aver type-string for a `BuiltinType` — Map and List forms use the
/// canonical spelling the registry`s discovery pass already
/// understands.
fn builtin_type_to_aver_string(ty: &crate::codegen::builtin_records::BuiltinType) -> String {
    use crate::codegen::builtin_records::BuiltinType;
    match ty {
        BuiltinType::Int => "Int".into(),
        BuiltinType::Str => "String".into(),
        BuiltinType::Bool => "Bool".into(),
        BuiltinType::Float => "Float".into(),
        BuiltinType::ListOf(name) => format!("List<{}>", name),
        BuiltinType::MapStrListStr => "Map<String, List<String>>".into(),
    }
}

/// True iff any FnDef signature or body literal mentions the given
/// type name. Lightweight string scan over annotations + return
/// types — a structural walk would be more precise but every name
/// we register here is unique enough that substring match is OK.
fn items_reference_name(items: &[crate::ast::TopLevel], name: &str) -> bool {
    use crate::ast::TopLevel;
    items.iter().any(|item| match item {
        TopLevel::FnDef(fd) => {
            fd.return_type.contains(name)
                || fd.params.iter().any(|(_, t)| t.contains(name))
                || fd
                    .effects
                    .iter()
                    .any(|e| effect_implies_builtin_record(e.node.as_str(), name))
        }
        _ => false,
    })
}

/// Maps a declared effect (`! [Foo.bar]`) to the builtin record it
/// implicitly requires. `Terminal.size` is the canonical case —
/// returns `Terminal.Size`, so the record slot allocates as soon as
/// any fn declares the effect, even before the body runs the call.
/// Same logic underpins `--handler` auto-registering HttpRequest /
/// HttpResponse from the synthesised wrapper.
fn effect_implies_builtin_record(effect: &str, record_name: &str) -> bool {
    let needed = match effect {
        "Terminal.size" => "Terminal.Size",
        // Any Tcp.* effect that takes or returns a connection forces
        // the record slot. Tcp.connect *returns* one; the rest
        // *consume* one through their first parameter, so even a
        // program that only reads / writes / closes still needs the
        // slot allocated.
        "Tcp.connect" | "Tcp.writeLine" | "Tcp.readLine" | "Tcp.close" => "Tcp.Connection",
        // HTTP verb effects all return Result<HttpResponse, String> —
        // ensure the response record slot is allocated even when no
        // user fn signature mentions it.
        "Http.get" | "Http.head" | "Http.delete" | "Http.post" | "Http.put" | "Http.patch" => {
            "HttpResponse"
        }
        // Request / Response surface comes via the user's
        // `! [Request.method]` etc. The Aver type checker already
        // requires HttpRequest / HttpResponse in the handler's
        // signature, so they get picked up by the param/return
        // walk. No extra mapping needed here.
        _ => return false,
    };
    needed == record_name
}

/// Walk fn body for binding annotations carrying a `List<...>` type
/// the fn signatures don't already spell out. `nested: List<List<Int>>
/// = [...]` is the canonical case — the outer `List<List<Int>>` only
/// ever appears in the binding annotation. Mirrors `collect_options
/// _from_fn_body` and `collect_vectors_from_fn_body`. Also walks
/// expressions to catch builtin calls like `String.chars` whose
/// return type (`List<String>`) only appears as a stdlib signature.
fn collect_lists_from_fn_body(
    fd: &crate::ir::hir::ResolvedFnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedFnBody, ResolvedStmt};
    let ResolvedFnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let ResolvedStmt::Binding {
            ty_ann: Some(annot),
            ..
        } = stmt
        {
            collect_lists_from_str(&annot.display(), out, order, next_idx);
        }
        let expr = match stmt {
            ResolvedStmt::Binding { value: e, .. } | ResolvedStmt::Expr(e) => &e.node,
        };
        collect_lists_from_expr(expr, out, order, next_idx);
    }
}

fn collect_lists_from_expr(
    expr: &crate::ir::hir::ResolvedExpr,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match expr {
        ResolvedExpr::Call(callee, args) => {
            // `String.chars(s)` returns `List<String>` — register it
            // eagerly here since the canonical never appears in fn
            // signatures by itself.
            if let ResolvedCallee::Builtin(name) = callee
                && name == "String.chars"
            {
                let canonical = "List<String>".to_string();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
            }
            for a in args {
                collect_lists_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::BinOp(_, l, r) => {
            collect_lists_from_expr(&l.node, out, order, next_idx);
            collect_lists_from_expr(&r.node, out, order, next_idx);
        }
        ResolvedExpr::Neg(inner) => collect_lists_from_expr(&inner.node, out, order, next_idx),
        ResolvedExpr::Match { subject, arms } => {
            collect_lists_from_expr(&subject.node, out, order, next_idx);
            for arm in arms {
                collect_lists_from_expr(&arm.body.node, out, order, next_idx);
            }
        }
        ResolvedExpr::TailCall { args, .. } => {
            for a in args {
                collect_lists_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Attr(obj, _) => collect_lists_from_expr(&obj.node, out, order, next_idx),
        ResolvedExpr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_lists_from_expr(&e.node, out, order, next_idx);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            collect_lists_from_expr(&base.node, out, order, next_idx);
            for (_, e) in updates {
                collect_lists_from_expr(&e.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Ctor(_, args) => {
            for a in args {
                collect_lists_from_expr(&a.node, out, order, next_idx);
            }
        }
        ResolvedExpr::Tuple(items) | ResolvedExpr::IndependentProduct(items, _) => {
            for it in items {
                collect_lists_from_expr(&it.node, out, order, next_idx);
            }
        }
        ResolvedExpr::ErrorProp(inner) => {
            collect_lists_from_expr(&inner.node, out, order, next_idx)
        }
        ResolvedExpr::List(items) => {
            for x in items {
                collect_lists_from_expr(&x.node, out, order, next_idx);
            }
        }
        _ => {}
    }
}

/// Convert a Tuple type string in any Aver form to the internal
/// canonical `Tuple<A,B>` (whitespace-stripped). Accepts `(A, B)`
/// (surface syntax that the type checker emits) or already-canonical
/// `Tuple<A,B>`.
fn normalize_tuple_canonical(s: &str) -> std::borrow::Cow<'_, str> {
    let normalized = normalize_compound(s);
    if normalized == s {
        std::borrow::Cow::Borrowed(s)
    } else {
        std::borrow::Cow::Owned(normalized)
    }
}

/// Recursively rewrite all `(A, B)` substrings inside a type string
/// to `Tuple<A,B>` and strip whitespace. Idempotent on already-
/// canonical `Tuple<...>`. Used wherever a stable canonical form
/// must be derived: discovery, registry lookup, eager registration.
pub(super) fn normalize_compound(s: &str) -> String {
    let stripped: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    rewrite_paren_tuples(&stripped)
}

/// Strip module-qualifier dots from inner type-name tokens. Walks the
/// type string and rewrites `Module.Name` to `Name` whenever `Name`
/// follows a `<` / `,` (i.e. it's an inner type argument). Used when
/// the registry lookup for the qualified form misses — same record
/// type may be referenced through `Module.Room` from one fn and
/// through bare `Room` from another after multi-module flatten.
pub(super) fn strip_inner_dotted_prefixes(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < bytes.len() {
        out.push(bytes[i] as char);
        // After a token boundary, look for `Capital.Name` and skip
        // the `Capital.` part.
        if matches!(bytes[i], b'<' | b',' | b'(') {
            // Try to find a dotted Capital identifier: chars consisting
            // of letters/digits/underscores then `.` then more chars.
            let start = i + 1;
            let mut j = start;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            if j > start && j < bytes.len() && bytes[j] == b'.' && bytes[start].is_ascii_uppercase()
            {
                let after_dot = j + 1;
                let mut k = after_dot;
                while k < bytes.len() && (bytes[k].is_ascii_alphanumeric() || bytes[k] == b'_') {
                    k += 1;
                }
                if k > after_dot && bytes[after_dot].is_ascii_uppercase() {
                    // Emit the bare suffix, skip the prefix + dot.
                    out.push_str(&s[after_dot..k]);
                    i = k;
                    continue;
                }
            }
        }
        i += 1;
    }
    out
}

fn rewrite_paren_tuples(s: &str) -> String {
    let bytes = s.as_bytes();
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'(' {
            let mut depth: i32 = 1;
            let mut j = i + 1;
            let mut top_commas = 0;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'(' | b'<' => depth += 1,
                    b')' | b'>' => depth -= 1,
                    b',' if depth == 1 => top_commas += 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 && top_commas >= 1 {
                let inner = &s[i + 1..j - 1];
                let inner_normalized = rewrite_paren_tuples(inner);
                out.push_str("Tuple<");
                out.push_str(&inner_normalized);
                out.push('>');
                i = j;
                continue;
            }
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}
