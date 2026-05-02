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

use crate::ast::{TopLevel, TypeDef};

/// User-type lookup tables built once before any fn body emit.
pub(super) struct TypeRegistry {
    /// `record_name → type_idx` for product (record) types.
    pub(super) records: HashMap<String, u32>,
    /// `variant_constructor_name → (parent_type_name, type_idx, fields)`.
    /// `fields` are the type strings of the constructor's positional
    /// fields (Aver variants use positional fields, not named ones).
    pub(super) variants: HashMap<String, VariantInfo>,
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
    /// `Expr::Literal(Literal::Str(_))` lowers to `array.new_data
    /// $string $segment_idx` with offset=0, size=len.
    pub(super) string_literals: Vec<Vec<u8>>,
    pub(super) string_literal_idx: HashMap<Vec<u8>, u32>,
    /// Type names that must NOT be erased to their underlying
    /// primitive by the newtype optimisation. Populated with every
    /// record/variant used as a `Map<K, *>` key — Map's open-
    /// addressing layout uses `keys[i] == null` as the empty marker,
    /// which only works when keys are emitted as ref values.
    pub(super) non_newtypable_keys: std::collections::HashSet<String>,
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
    pub(super) fn build_with_handler(items: &[TopLevel], _handler_active: bool) -> Self {
        // _handler_active is consumed by `items_reference_name`
        // overrides below so the rest of the builder stays
        // unchanged.
        let handler_active = _handler_active;
        let mut records = HashMap::new();
        let mut variants = HashMap::new();
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
                        variants.insert(
                            v.name.clone(),
                            VariantInfo {
                                parent: name.clone(),
                                type_idx: next_idx,
                                fields: v.fields.clone(),
                            },
                        );
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
            let force = handler_active
                && (record.aver_name == "HttpRequest" || record.aver_name == "HttpResponse");
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

        // Allocate the String type slot first (after records/variants)
        // so any `Vector<String>` registered below sits at a higher
        // index than `$string` and can reference it without crossing
        // the rec-group boundary.
        let needs_string = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => {
                fd.return_type.contains("String")
                    || fd.params.iter().any(|(_, t)| t.contains("String"))
                    || fn_body_produces_string(fd)
            }
            _ => false,
        });
        let string_array_type_idx = if needs_string {
            let idx = next_idx;
            next_idx += 1;
            Some(idx)
        } else {
            None
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
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_vectors_from_str(
                    &fd.return_type,
                    &mut vector_types,
                    &mut vector_order,
                    &mut next_idx,
                );
                for (_, ty) in &fd.params {
                    collect_vectors_from_str(
                        ty,
                        &mut vector_types,
                        &mut vector_order,
                        &mut next_idx,
                    );
                }
                collect_vectors_from_fn_body(
                    fd,
                    &mut vector_types,
                    &mut vector_order,
                    &mut next_idx,
                );
            }
        }
        // Record field walks — `record { nums: Vector<Int> }` only
        // shows up in the record's field list, not in any fn
        // signature. Mirror the lists / options record-field walk.
        for (_, fields) in record_fields.iter() {
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
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_results_from_str(
                    &fd.return_type,
                    &mut result_types,
                    &mut result_order,
                    &mut next_idx,
                );
                for (_, ty) in &fd.params {
                    collect_results_from_str(
                        ty,
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
            }
        }
        let mut list_types: HashMap<String, u32> = HashMap::new();
        let mut list_order: Vec<String> = Vec::new();
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_lists_from_str(
                    &fd.return_type,
                    &mut list_types,
                    &mut list_order,
                    &mut next_idx,
                );
                for (_, ty) in &fd.params {
                    collect_lists_from_str(ty, &mut list_types, &mut list_order, &mut next_idx);
                }
                // Body annotations — `nested: List<List<Int>> = [a, b]`
                // adds `List<List<Int>>` even when no fn signature
                // mentions it. Mirrors the same body-walk options
                // and vectors already do.
                collect_lists_from_fn_body(fd, &mut list_types, &mut list_order, &mut next_idx);
            }
        }
        for (_, fields) in record_fields.iter() {
            for (_, ty) in fields {
                collect_lists_from_str(ty, &mut list_types, &mut list_order, &mut next_idx);
            }
        }
        if handler_active && !list_types.contains_key("List<String>") {
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
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_options_from_str(
                    &fd.return_type,
                    &mut option_types,
                    &mut option_order,
                    &mut next_idx,
                );
                for (_, ty) in &fd.params {
                    collect_options_from_str(
                        ty,
                        &mut option_types,
                        &mut option_order,
                        &mut next_idx,
                    );
                }
                collect_options_from_fn_body(
                    fd,
                    &mut option_types,
                    &mut option_order,
                    &mut next_idx,
                );
            }
        }
        // Record field walk — `record GameState { lastAiResult:
        // Option<AiResult> }` only spells `Option<AiResult>` in the
        // record declaration. Without this walk the canonical never
        // gets registered, and a `match state.lastAiResult` arm
        // dispatcher fails to recover its slot.
        for (_, fields) in record_fields.iter() {
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
        for record_name in record_fields.keys() {
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
        for (_, fields) in record_fields.iter() {
            for (_, ty) in fields {
                collect_maps_from_str(ty, &mut pending_maps_for_options);
            }
        }
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_maps_from_str(&fd.return_type, &mut pending_maps_for_options);
                for (_, ty) in &fd.params {
                    collect_maps_from_str(ty, &mut pending_maps_for_options);
                }
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
        for (_, fields) in record_fields.iter() {
            for (_, ty) in fields {
                collect_maps_from_str(ty, &mut pending_maps);
            }
        }
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_maps_from_str(&fd.return_type, &mut pending_maps);
                for (_, ty) in &fd.params {
                    collect_maps_from_str(ty, &mut pending_maps);
                }
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
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_tuples_from_str(
                    &fd.return_type,
                    &mut tuple_types,
                    &mut tuple_order,
                    &mut next_idx,
                );
                for (_, ty) in &fd.params {
                    collect_tuples_from_str(ty, &mut tuple_types, &mut tuple_order, &mut next_idx);
                }
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
        // every `Vector<T>` — `Vector.toList` returns the list and
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
        for item in items {
            if let TopLevel::FnDef(fd) = item {
                collect_string_literals_in_fn(fd, &mut string_literals, &mut string_literal_idx);
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
                    || variants.values().any(|v| v.parent == k_trim)
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
        }
    }

    pub(super) fn list_type_idx(&self, canonical: &str) -> Option<u32> {
        let normalized = normalize_compound(canonical);
        self.list_types.get(&normalized).copied()
    }

    pub(super) fn list_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("List<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    pub(super) fn result_type_idx(&self, canonical: &str) -> Option<u32> {
        self.result_types.get(canonical).copied()
    }

    /// Split `Result<T, E>` into (T, E) borrowed slices.
    pub(super) fn result_te(canonical: &str) -> Option<(&str, &str)> {
        let inner = canonical
            .trim()
            .strip_prefix("Result<")?
            .strip_suffix('>')?;
        let bytes = inner.as_bytes();
        let mut depth: i32 = 0;
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'<' => depth += 1,
                b'>' => depth -= 1,
                b',' if depth == 0 => {
                    return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
                }
                _ => {}
            }
        }
        None
    }

    pub(super) fn map_slots(&self, canonical: &str) -> Option<MapSlots> {
        self.map_types.get(canonical).copied()
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
        self.tuple_types.get(normalized.as_ref()).copied()
    }

    /// Split `Tuple<A, B>` (or `(A, B)`) into (A, B). Same depth-
    /// aware comma scan as `result_te` / `parse_map_kv`. Returns
    /// owned strings since the canonical may be normalized from
    /// Aver-surface `(A, B)` form.
    pub(super) fn tuple_ab(canonical: &str) -> Option<(&str, &str)> {
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
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'(' | b'<' => depth += 1,
                b')' | b'>' => depth -= 1,
                b',' if depth == 0 => {
                    return Some((inner[..idx].trim(), inner[idx + 1..].trim()));
                }
                _ => {}
            }
        }
        None
    }

    pub(super) fn option_type_idx(&self, canonical: &str) -> Option<u32> {
        self.option_types.get(canonical).copied()
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
        self.vector_types.get(canonical).copied()
    }

    /// Element-type Aver string for a registered `Vector<T>`. Used by
    /// module emit to resolve the wasm storage type of array elements.
    pub(super) fn vector_element_type(canonical: &str) -> Option<&str> {
        let trimmed = canonical.trim();
        let inner = trimmed.strip_prefix("Vector<")?.strip_suffix('>')?;
        Some(inner.trim())
    }

    pub(super) fn record_type_idx(&self, name: &str) -> Option<u32> {
        self.records.get(name).copied()
    }

    pub(super) fn variant(&self, name: &str) -> Option<&VariantInfo> {
        self.variants.get(name)
    }

    pub(super) fn record_field_index(&self, record: &str, field: &str) -> Option<u32> {
        self.record_fields
            .get(record)
            .and_then(|fs| fs.iter().position(|(n, _)| n == field))
            .map(|i| i as u32)
    }

    pub(super) fn record_field_type(&self, record: &str, field: &str) -> Option<&str> {
        self.record_fields
            .get(record)
            .and_then(|fs| fs.iter().find(|(n, _)| n == field))
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
        let mut variants_of_parent = self.variants.values().filter(|v| v.parent == type_name);
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

fn is_primitive(ty: &str) -> bool {
    matches!(ty.trim(), "Int" | "Float" | "Bool")
}

/// Walk a type string looking for `Vector<...>` substrings (with
/// balanced angle brackets). Each unique instantiation is registered
/// in `out` and order-tracked, with a freshly allocated wasm type idx.
/// Recurses into the element type so nested forms like
/// `Vector<Vector<Int>>` register both the outer and inner shapes —
/// the element of the outer needs a wasm-resolvable type.
fn collect_vectors_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 7 <= bytes.len() {
        if &bytes[i..i + 7] == b"Vector<" {
            // Find the matching `>` for this `Vector<`.
            let mut depth: i32 = 1;
            let mut j = i + 7;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — nested Vectors register before
                // the outer needs them as an element type. Without
                // this, `Vector<Vector<Int>>` would emit a forward
                // reference to `$vector_int` and validators reject.
                let element = &trimmed[i + 7..j - 1];
                collect_vectors_from_str(element, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Walk a fn body for builtin calls whose declared return type is a
/// `Result<T, E>` not otherwise visible in signatures. Phase-3c
/// targets the curated set the bench scenarios use; broader auto-
/// discovery would mean reading `types::checker::builtins` directly.
fn collect_results_from_builtin_uses(
    fd: &crate::ast::FnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::{Expr, FnBody, Stmt};
    fn walk(e: &Expr, out: &mut HashMap<String, u32>, order: &mut Vec<String>, next_idx: &mut u32) {
        let mut intern = |canonical: &str| {
            if !out.contains_key(canonical) {
                out.insert(canonical.to_string(), *next_idx);
                order.push(canonical.to_string());
                *next_idx += 1;
            }
        };
        match e {
            Expr::FnCall(callee, args) => {
                if let Expr::Attr(parent, member) = &callee.node
                    && let Expr::Ident(p) = &parent.node
                {
                    let dotted = format!("{}.{}", p, member);
                    match dotted.as_str() {
                        "Float.fromString" => intern("Result<Float,String>"),
                        "Int.fromString" | "Int.mod" | "Byte.fromHex" => {
                            intern("Result<Int,String>")
                        }
                        "Byte.toHex" => intern("Result<String,String>"),
                        _ => {}
                    }
                }
                walk(&callee.node, out, order, next_idx);
                for a in args {
                    walk(&a.node, out, order, next_idx);
                }
            }
            Expr::BinOp(_, l, r) => {
                walk(&l.node, out, order, next_idx);
                walk(&r.node, out, order, next_idx);
            }
            Expr::Match { subject, arms } => {
                walk(&subject.node, out, order, next_idx);
                for arm in arms {
                    walk(&arm.body.node, out, order, next_idx);
                }
            }
            Expr::TailCall(boxed) => {
                for a in &boxed.args {
                    walk(&a.node, out, order, next_idx);
                }
            }
            Expr::Attr(obj, _) => walk(&obj.node, out, order, next_idx),
            Expr::Constructor(_, payload) => {
                if let Some(p) = payload.as_deref() {
                    walk(&p.node, out, order, next_idx);
                }
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, e) in fields {
                    walk(&e.node, out, order, next_idx);
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                walk(&base.node, out, order, next_idx);
                for (_, e) in updates {
                    walk(&e.node, out, order, next_idx);
                }
            }
            Expr::List(items) => {
                for x in items {
                    walk(&x.node, out, order, next_idx);
                }
            }
            _ => {}
        }
    }
    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        let expr = match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
        };
        walk(expr, out, order, next_idx);
    }
}

/// `Tuple<A, B>` discovery — scans both `Tuple<...>` (canonical we
/// emit internally) and `(A, B)` (the surface Aver syntax that the
/// type checker renders Type::Tuple as). Both forms register under
/// the same `Tuple<A,B>` whitespace-free canonical so downstream
/// lookups work uniformly.
fn collect_tuples_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        // `Tuple<...>` form
        if i + 6 <= bytes.len() && &bytes[i..i + 6] == b"Tuple<" {
            let mut depth: i32 = 1;
            let mut j = i + 6;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                let inner = &trimmed[i + 6..j - 1];
                collect_tuples_from_str(inner, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        // `(A, B)` surface form. Parens-balanced, exactly one
        // top-level comma → tuple. Skips fn-call args and grouping
        // parens (those don't carry a comma at the top level here).
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
                let inner = &trimmed[i + 1..j - 1];
                collect_tuples_from_str(inner, out, order, next_idx);
                // Build canonical `Tuple<A,B,...>` form
                let canonical_inner: String =
                    inner.chars().filter(|c| !c.is_whitespace()).collect();
                let canonical = format!("Tuple<{canonical_inner}>");
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical);
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// `Result<T, E>` discovery — handles nested commas via depth tracking.
fn collect_results_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 7 <= bytes.len() {
        if &bytes[i..i + 7] == b"Result<" {
            let mut depth: i32 = 1;
            let mut j = i + 7;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — nested Results land in `result_order`
                // before the outer references them as T or E. Same
                // forward-ref guard as in the other collectors.
                let inner = &trimmed[i + 7..j - 1];
                collect_results_from_str(inner, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// `List<...>` discovery. Normalises paren-tuple `(A, B)` to
/// `Tuple<A,B>` first so a `List<(A, B)>` from a fn signature and
/// a `List<Tuple<A,B>>` from eager registration land on the same
/// canonical (otherwise the type section ends up with two duplicate
/// list slots whose elements are structurally equal but whose ref
/// indices diverge).
fn collect_lists_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let normalized = normalize_compound(type_str);
    let bytes = normalized.as_bytes();
    let mut i = 0;
    while i + 5 <= bytes.len() {
        if &bytes[i..i + 5] == b"List<" {
            let mut depth: i32 = 1;
            let mut j = i + 5;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                let element = &normalized[i + 5..j - 1];
                collect_lists_from_str(element, out, order, next_idx);
                let canonical = normalized[i..j].to_string();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// `Option<...>` discovery — same shape as `collect_vectors_from_str`.
fn collect_options_from_str(
    type_str: &str,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 7 <= bytes.len() {
        if &bytes[i..i + 7] == b"Option<" {
            let mut depth: i32 = 1;
            let mut j = i + 7;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — same rationale as in
                // `collect_lists_from_str`. `Option<Option<T>>`
                // (rare but legal) needs the inner Option already
                // present in `option_order` before the outer's
                // struct field references it.
                let element = &trimmed[i + 7..j - 1];
                collect_options_from_str(element, out, order, next_idx);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                if !out.contains_key(&canonical) {
                    out.insert(canonical.clone(), *next_idx);
                    order.push(canonical.clone());
                    *next_idx += 1;
                }
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Walk fn body for `Option.Some(payload)` constructors and bindings
/// that imply an `Option<T>` instantiation. Unlike Vector, the bench
/// scenarios mostly leave Option<T> implicit — `Map.get` returns
/// `Option<V>` where V is read off `Map<K,V>` — so the body walk is
/// the primary discovery path.
fn collect_options_from_fn_body(
    fd: &crate::ast::FnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::{FnBody, Stmt};
    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let Stmt::Binding(_, Some(annot), _) = stmt {
            collect_options_from_str(annot, out, order, next_idx);
        }
        let expr = match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
        };
        collect_options_from_expr(expr, out, order, next_idx);
    }
}

fn collect_options_from_expr(
    expr: &crate::ast::Expr,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::Expr;
    match expr {
        Expr::FnCall(callee, args) => {
            // `String.charAt(s, i)` and `Char.fromCode(code)` both
            // declare `Option<String>` as their return type, but the
            // canonical never appears anywhere else — eager-register
            // it here so the builtin's emit can resolve the slot.
            if let Expr::Attr(parent, member) = &callee.node {
                let parent_name = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(p) = parent_name {
                    let dotted = format!("{p}.{member}");
                    if matches!(
                        dotted.as_str(),
                        "String.charAt" | "Char.fromCode" | "Terminal.readKey"
                    ) {
                        let canonical = "Option<String>".to_string();
                        if !out.contains_key(&canonical) {
                            out.insert(canonical.clone(), *next_idx);
                            order.push(canonical);
                            *next_idx += 1;
                        }
                    }
                }
            }
            collect_options_from_expr(&callee.node, out, order, next_idx);
            for a in args {
                collect_options_from_expr(&a.node, out, order, next_idx);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_options_from_expr(&l.node, out, order, next_idx);
            collect_options_from_expr(&r.node, out, order, next_idx);
        }
        Expr::Match { subject, arms } => {
            collect_options_from_expr(&subject.node, out, order, next_idx);
            for arm in arms {
                collect_options_from_expr(&arm.body.node, out, order, next_idx);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                collect_options_from_expr(&a.node, out, order, next_idx);
            }
        }
        Expr::Attr(obj, _) => collect_options_from_expr(&obj.node, out, order, next_idx),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_options_from_expr(&e.node, out, order, next_idx);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_options_from_expr(&base.node, out, order, next_idx);
            for (_, e) in updates {
                collect_options_from_expr(&e.node, out, order, next_idx);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_options_from_expr(&p.node, out, order, next_idx);
            }
        }
        _ => {}
    }
}

/// Walk fn body looking for binding annotations and `Vector.new` calls
/// that imply a `Vector<T>` instantiation. The fill-arg's type fixes
/// `T` for `Vector.new`; binding annotations carry the type string
/// directly. Mirrors the surface-level discovery — the vector_ops
/// bench spells out `Vector<Int>` in fn signatures so this body walk
/// is a defensive backstop, not the primary discovery path.
fn collect_vectors_from_fn_body(
    fd: &crate::ast::FnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::{FnBody, Stmt};
    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let Stmt::Binding(_, Some(annot), _) = stmt {
            collect_vectors_from_str(annot, out, order, next_idx);
        }
        let expr = match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
        };
        collect_vectors_from_expr(expr, out, order, next_idx);
    }
}

fn collect_vectors_from_expr(
    expr: &crate::ast::Expr,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::{Expr, StrPart};
    match expr {
        Expr::FnCall(callee, args) => {
            collect_vectors_from_expr(&callee.node, out, order, next_idx);
            for a in args {
                collect_vectors_from_expr(&a.node, out, order, next_idx);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_vectors_from_expr(&l.node, out, order, next_idx);
            collect_vectors_from_expr(&r.node, out, order, next_idx);
        }
        Expr::Match { subject, arms } => {
            collect_vectors_from_expr(&subject.node, out, order, next_idx);
            for arm in arms {
                collect_vectors_from_expr(&arm.body.node, out, order, next_idx);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                collect_vectors_from_expr(&a.node, out, order, next_idx);
            }
        }
        Expr::Attr(obj, _) => collect_vectors_from_expr(&obj.node, out, order, next_idx),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_vectors_from_expr(&e.node, out, order, next_idx);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_vectors_from_expr(&base.node, out, order, next_idx);
            for (_, e) in updates {
                collect_vectors_from_expr(&e.node, out, order, next_idx);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_vectors_from_expr(&p.node, out, order, next_idx);
            }
        }
        Expr::InterpolatedStr(parts) => {
            // Interpolation lowers to an `array.new_fixed (array (ref
            // null $string)) N` + variadic concat. The array type
            // shares a slot with `Vector<String>` (same wasm shape),
            // so register it here even if no Aver-level signature
            // mentions Vector<String>.
            collect_vectors_from_str("Vector<String>", out, order, next_idx);
            for p in parts {
                if let StrPart::Parsed(inner) = p {
                    collect_vectors_from_expr(&inner.node, out, order, next_idx);
                }
            }
        }
        _ => {}
    }
}

/// Walk a type string looking for `Map<K, V>` substrings (with
/// balanced angle brackets), append each canonical (whitespace-
/// stripped) form to `out`. Recurses into both K and V so nested
/// types (`Map<String, Vector<Int>>`) register every reachable
/// outer + inner instantiation.
fn collect_maps_from_str(type_str: &str, out: &mut Vec<String>) {
    let trimmed = type_str.trim();
    let bytes = trimmed.as_bytes();
    let mut i = 0;
    while i + 4 <= bytes.len() {
        if &bytes[i..i + 4] == b"Map<" {
            let mut depth: i32 = 1;
            let mut j = i + 4;
            while j < bytes.len() && depth > 0 {
                match bytes[j] {
                    b'<' => depth += 1,
                    b'>' => depth -= 1,
                    _ => {}
                }
                j += 1;
            }
            if depth == 0 {
                // DFS post-order — same forward-ref guard as the
                // other compound-type collectors. Nested Maps land
                // before the outer references them as K or V.
                let inner = &trimmed[i + 4..j - 1];
                collect_maps_from_str(inner, out);
                let canonical: String = trimmed[i..j]
                    .chars()
                    .filter(|c| !c.is_whitespace())
                    .collect();
                out.push(canonical);
                i = j;
                continue;
            }
        }
        i += 1;
    }
}

/// Split a canonical `Map<K, V>` into its `K` and `V` parts (both
/// borrowed slices of the input). Returns `None` if the string
/// doesn't match the expected shape.
pub(super) fn parse_map_kv(canonical: &str) -> Option<(&str, &str)> {
    let inner = canonical.trim().strip_prefix("Map<")?.strip_suffix('>')?;
    let bytes = inner.as_bytes();
    let mut depth: i32 = 0;
    for (idx, b) in bytes.iter().enumerate() {
        match b {
            b'<' => depth += 1,
            b'>' => depth -= 1,
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
fn fn_body_produces_string(fd: &crate::ast::FnDef) -> bool {
    use crate::ast::{FnBody, Stmt};
    let FnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| match s {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => expr_uses_string(&e.node),
    })
}

fn expr_uses_string(expr: &crate::ast::Expr) -> bool {
    use crate::ast::Expr;
    match expr {
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(parent, member) = &callee.node {
                let parent_name = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(p) = parent_name {
                    let dotted = format!("{p}.{member}");
                    if matches!(
                        dotted.as_str(),
                        "Int.toString"
                            | "Float.toString"
                            | "String.len"
                            | "String.length"
                            | "String.concat"
                            | "String.startsWith"
                            | "String.contains"
                            | "String.slice"
                            | "String.toUpper"
                            | "String.toLower"
                            | "String.trim"
                            | "String.replace"
                            | "String.split"
                            | "String.join"
                            | "String.fromInt"
                            | "String.fromFloat"
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
                    ) {
                        return true;
                    }
                }
            }
            expr_uses_string(&callee.node) || args.iter().any(|a| expr_uses_string(&a.node))
        }
        Expr::BinOp(_, l, r) => expr_uses_string(&l.node) || expr_uses_string(&r.node),
        Expr::Match { subject, arms } => {
            expr_uses_string(&subject.node) || arms.iter().any(|a| expr_uses_string(&a.body.node))
        }
        Expr::TailCall(boxed) => boxed.args.iter().any(|a| expr_uses_string(&a.node)),
        Expr::Attr(obj, _) => expr_uses_string(&obj.node),
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .is_some_and(|p| expr_uses_string(&p.node)),
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| expr_uses_string(&e.node)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_uses_string(&base.node) || updates.iter().any(|(_, e)| expr_uses_string(&e.node))
        }
        Expr::Literal(crate::ast::Literal::Str(_)) => true,
        Expr::InterpolatedStr(_) => true,
        _ => false,
    }
}

/// Walk a fn body, collecting unique String literals into a per-segment
/// table. Both `Literal::Str` and the `Literal` parts of an
/// `InterpolatedStr` count — each unique byte sequence gets a passive
/// data segment.
fn collect_string_literals_in_fn(
    fd: &crate::ast::FnDef,
    out: &mut Vec<Vec<u8>>,
    idx: &mut HashMap<Vec<u8>, u32>,
) {
    use crate::ast::{FnBody, Stmt};
    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        let expr = match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
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
    expr: &crate::ast::Expr,
    out: &mut Vec<Vec<u8>>,
    idx: &mut HashMap<Vec<u8>, u32>,
) {
    use crate::ast::{Expr, Literal, StrPart};
    match expr {
        Expr::Literal(Literal::Str(s)) => intern_literal(s.as_bytes().to_vec(), out, idx),
        Expr::InterpolatedStr(parts) => {
            for p in parts {
                match p {
                    StrPart::Literal(s) => intern_literal(s.as_bytes().to_vec(), out, idx),
                    StrPart::Parsed(inner) => {
                        collect_string_literals_in_expr(&inner.node, out, idx);
                    }
                }
            }
        }
        Expr::FnCall(callee, args) => {
            collect_string_literals_in_expr(&callee.node, out, idx);
            for a in args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_string_literals_in_expr(&l.node, out, idx);
            collect_string_literals_in_expr(&r.node, out, idx);
        }
        Expr::Match { subject, arms } => {
            collect_string_literals_in_expr(&subject.node, out, idx);
            for a in arms {
                if let crate::ast::Pattern::Literal(Literal::Str(s)) = &a.pattern {
                    intern_literal(s.as_bytes().to_vec(), out, idx);
                }
                collect_string_literals_in_expr(&a.body.node, out, idx);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                collect_string_literals_in_expr(&a.node, out, idx);
            }
        }
        Expr::Attr(obj, _) => collect_string_literals_in_expr(&obj.node, out, idx),
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_string_literals_in_expr(&p.node, out, idx);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_string_literals_in_expr(&e.node, out, idx);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_string_literals_in_expr(&base.node, out, idx);
            for (_, e) in updates {
                collect_string_literals_in_expr(&e.node, out, idx);
            }
        }
        Expr::List(items) => {
            for item in items {
                collect_string_literals_in_expr(&item.node, out, idx);
            }
        }
        Expr::MapLiteral(entries) => {
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
        if reg.variants.values().any(|v| v.parent == trimmed) {
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
        "Tcp.connect" => "Tcp.Connection",
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
    fd: &crate::ast::FnDef,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::{FnBody, Stmt};
    let FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        if let Stmt::Binding(_, Some(annot), _) = stmt {
            collect_lists_from_str(annot, out, order, next_idx);
        }
        let expr = match stmt {
            Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
        };
        collect_lists_from_expr(expr, out, order, next_idx);
    }
}

fn collect_lists_from_expr(
    expr: &crate::ast::Expr,
    out: &mut HashMap<String, u32>,
    order: &mut Vec<String>,
    next_idx: &mut u32,
) {
    use crate::ast::Expr;
    match expr {
        Expr::FnCall(callee, args) => {
            // `String.chars(s)` returns `List<String>` — register it
            // eagerly here since the canonical never appears in fn
            // signatures by itself.
            if let Expr::Attr(parent, member) = &callee.node {
                let parent_name = match &parent.node {
                    Expr::Ident(n) => Some(n.as_str()),
                    Expr::Resolved { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(p) = parent_name {
                    let dotted = format!("{p}.{member}");
                    if dotted == "String.chars" {
                        let canonical = "List<String>".to_string();
                        if !out.contains_key(&canonical) {
                            out.insert(canonical.clone(), *next_idx);
                            order.push(canonical);
                            *next_idx += 1;
                        }
                    }
                }
            }
            collect_lists_from_expr(&callee.node, out, order, next_idx);
            for a in args {
                collect_lists_from_expr(&a.node, out, order, next_idx);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_lists_from_expr(&l.node, out, order, next_idx);
            collect_lists_from_expr(&r.node, out, order, next_idx);
        }
        Expr::Match { subject, arms } => {
            collect_lists_from_expr(&subject.node, out, order, next_idx);
            for arm in arms {
                collect_lists_from_expr(&arm.body.node, out, order, next_idx);
            }
        }
        Expr::TailCall(boxed) => {
            for a in &boxed.args {
                collect_lists_from_expr(&a.node, out, order, next_idx);
            }
        }
        Expr::Attr(obj, _) => collect_lists_from_expr(&obj.node, out, order, next_idx),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_lists_from_expr(&e.node, out, order, next_idx);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_lists_from_expr(&base.node, out, order, next_idx);
            for (_, e) in updates {
                collect_lists_from_expr(&e.node, out, order, next_idx);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                collect_lists_from_expr(&p.node, out, order, next_idx);
            }
        }
        Expr::List(items) => {
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
