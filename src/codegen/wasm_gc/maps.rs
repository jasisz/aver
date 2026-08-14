//! `Map<K, V>` helper bodies — per-instantiation hashtable primitives.
//!
//! Strategy: monomorphise per (K, V), same as Vector / Option. Each
//! instantiation owns four wasm fns (`empty`, `set`, `get`, `len`)
//! plus shares one pair of K-keyed helpers (`hash<K>`, `eq<K, K>`)
//! across every `Map<K, *>` that uses the same K.
//!
//! Phase-3c MVP only emits these for `K = String` (the bench shape).
//! Other K kinds surface as Unimplemented when their hash / eq would
//! need writing — see `emit_hash_for` / `emit_eq_for`.
//!
//! Open-addressing layout, fixed initial capacity, no resize:
//!
//! ```text
//! struct $map_KV {
//!   mut i32 size;
//!   mut i32 cap;
//!   mut (ref null $keys_array)   keys;
//!   mut (ref null $values_array) values;
//! }
//! ```
//!
//! Empty slot marker = `keys[i] == null` (only valid for `K` that
//! cannot legitimately be null, which Aver guarantees for ref types
//! since the type system rejects null at the source level).
//!
//! Resize is a phase-3c+ extension. Cap is fixed at 16384 entries
//! today — large enough for the bench scenarios (5000 keys), small
//! enough to keep the `array.new_default` allocation under wasmtime's
//! GC heap pressure threshold.
//!
//! Every probe loop is bounded, because a full table is reachable:
//! the 16384th key fills the last slot. The two insert helpers trap
//! once a probe walks the whole table without finding a free slot —
//! the entry has nowhere to go and the table cannot grow — and the
//! lookup helpers report the miss, which is what `remove` already did.
//! `remove`'s two loops stop on the same wrap test.
//! `capacity_helper_names` feeds the wasm `name` section so that trap
//! names the capacity it hit instead of an anonymous fn index.
//!
//! **That name survives `--optimize` only by luck.** `wasm-opt -Oz` /
//! `-O3` drop the `name` section outright, and `-g` does not buy it
//! back in the shape that traps: a program that fills a map from one
//! `Map.set` call site gets that helper inlined into its caller, so
//! the named body no longer exists and `-g` writes an empty name map.
//! An optimized artifact therefore traps with `<wasm function N>`.
//! `finalize_wasm_artifact` (src/main/commands.rs) says so on stderr
//! when it optimizes a module carrying these names.

use std::collections::HashMap;

use wasm_encoder::{BlockType, CodeSection, Function, HeapType, Instruction, RefType, ValType};

use super::WasmGcError;
use super::types::{MapSlots, TypeRegistry};
use super::wat_helper;

/// Initial bucket count — power of two so masking with `cap-1`
/// instead of `i32.rem_u` works. Sized for the bench scenarios.
const INITIAL_CAP: i32 = 16384;

/// Every name `capacity_helper_names` builds starts with this. The
/// `--optimize` path reads it back out of the finished module (see
/// `carries_capacity_helper_names` in the parent module) to tell
/// whether the pass it is about to run drops a naming channel that
/// was there.
pub(super) const CAPACITY_HELPER_NAME_PREFIX: &str = "Map.set ";

#[derive(Debug, Clone, Copy)]
pub(super) struct KeyHelpers {
    /// `hash : (ref null $K) -> i32`
    pub(super) hash: u32,
    /// `eq : (ref null $K, ref null $K) -> i32`
    pub(super) eq: u32,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct MapKVHelpers {
    pub(super) empty: u32,
    /// Clone-on-write `set` — allocates fresh keys/values arrays,
    /// `array.copy`s them, mutates the copies. Used when `ir::alias`
    /// flags the call site's map slot as alias-prone.
    pub(super) set: u32,
    /// In-place `set` — probes the source map's keys/values arrays
    /// directly, `array.set`s into them, returns a struct.new wrapping
    /// the same arrays with the updated size. Sound only when the IR
    /// alias pass + last-use proves the map slot is uniquely owned.
    pub(super) set_in_place: u32,
    pub(super) get: u32,
    pub(super) len: u32,
    /// `get_or_default(m, k, default) -> V`. Fused shape that backs
    /// `Option.withDefault(Map.get(m, k), default)` without ever
    /// allocating an `Option<V>`. Same probe loop as `get` but
    /// returns `values[idx]` directly on a key match and the supplied
    /// default on an empty slot. Read by the MIR emitter's
    /// `emit_mir_option_with_default` (from_mir/builtins.rs) for the
    /// `Map.get` → `withDefault` fusion.
    pub(super) get_or_default: u32,
    /// `get_pair(m, k) -> (i32 found, V value)`. Multi-result return
    /// that backs the fused `match Map.get(m, k) { Some(v) -> ...;
    /// None -> ... }` shape. Caller pops `value` into the binding
    /// slot, then branches on `found` — no Option<V> ever allocates.
    pub(super) get_pair: u32,
    /// `keys(m) -> List<K>`. Walks the keys array right-to-left,
    /// cons-prepending each occupied entry. Order is hash-bucket
    /// order, not insertion order.
    pub(super) keys: u32,
    /// `values(m) -> List<V>`. Same shape as `keys` but pulls from
    /// the values array, only when the corresponding key slot is
    /// occupied (`keys[i] != null`).
    pub(super) values: u32,
    /// `remove(m, k) -> m`. Linear-probe locate of `k`, then
    /// backwards-shift the contiguous probe chain so subsequent
    /// `get` calls still find their entries. Mutates `m` in place
    /// and returns the same handle (Aver semantics: same shape as
    /// `set`, the returned ref is structurally equal).
    pub(super) remove: u32,
    /// `entries(m) -> List<Tuple<K, V>>`. Right-to-left walk; per
    /// occupied slot builds a Tuple and prepends onto a cons list.
    pub(super) entries: u32,
    /// `from_list(l) -> Map<K, V>`. Walks `l`, struct.get's the
    /// (K, V) from each tuple, calls the per-(K, V) `set` helper. Read
    /// by the MIR emitter's `emit_mir_map_builtin` (from_mir/builtins.rs)
    /// for `Map.fromList`.
    pub(super) from_list: u32,
    /// `__eq_Map<K,V>(a, b) -> i32`. Structural eq — `a.size ==
    /// b.size && ∀ k ∈ a: get(b, k) == Some(a[k])`. Insertion order
    /// is intentionally ignored (matches VM's `HashMap` PartialEq +
    /// the Python/Java/Rust/Haskell mainstream).
    pub(super) eq: u32,
    /// `__hash_Map<K,V>(m) -> i32`. Order-independent commutative
    /// fold — `h = 0; for (k, v) in m: h ^= djb2(k) * 33 + djb2(v)`.
    /// XOR is commutative + associative so the result is invariant
    /// to bucket ordering.
    pub(super) hash: u32,
}

#[derive(Default)]
pub(super) struct MapHelperRegistry {
    /// K (Aver type string) → its hash+eq fn indices.
    key: HashMap<String, KeyHelpers>,
    /// Canonical `Map<K, V>` → its four method indices.
    kv: HashMap<String, MapKVHelpers>,
    /// Insertion order — drives type-section + code-section emit.
    key_order: Vec<String>,
    kv_order: Vec<String>,
    /// Per-helper wasm type indices (parallel to fn indices). Stored
    /// here so the type section emit can look them up.
    key_type_indices: HashMap<String, (u32, u32)>, // (hash type idx, eq type idx)
    kv_type_indices: HashMap<String, MapKVTypeIdx>,
}

#[derive(Debug, Clone, Copy)]
struct MapKVTypeIdx {
    empty: u32,
    /// Same wasm-fn type as `set_in_place` — `(map, k, v) -> map`.
    /// Two distinct type entries because the fn-type table is
    /// indexed by type-idx, not shape, and `assign_slots` writes
    /// each helper to its own slot.
    set: u32,
    set_in_place: u32,
    get: u32,
    len: u32,
    get_or_default: u32,
    get_pair: u32,
    keys: u32,
    values: u32,
    remove: u32,
    entries: u32,
    from_list: u32,
    eq: u32,
    hash: u32,
}

impl MapHelperRegistry {
    /// Register all helpers needed for the given map instantiations.
    /// Must be called after `BuiltinRegistry::assign_slots` so the
    /// fn-idx counter is past user fns + pure builtins.
    pub(super) fn assign_slots(
        &mut self,
        map_canonicals: &[String],
        registry: &TypeRegistry,
        next_wasm_fn_idx: &mut u32,
        next_type_idx: &mut u32,
    ) -> Result<(), WasmGcError> {
        // Collect unique K names in the same order Maps appear.
        // Adds an extra synthetic K="String" up front when any
        // user-record K transitively has a String field — the
        // record's hash/eq body delegates to `hash<String>` /
        // `eq<String>` which therefore must be registered in the
        // same module even when no `Map<String, V>` is reachable
        // from the surface code.
        let mut k_names: Vec<String> = Vec::new();
        let mut k_seen: std::collections::HashSet<String> = std::collections::HashSet::new();
        for canonical in map_canonicals {
            let (k_aver, v_aver) =
                super::types::parse_map_kv(canonical).ok_or(WasmGcError::Validation(format!(
                    "MapHelperRegistry: cannot parse K, V from `{canonical}`"
                )))?;
            // If K is a record / sum whose fields include `String`,
            // ensure String hash/eq is registered first.
            let mut needs_string = false;
            if let Some(fs) = registry.record_fields.get(k_aver) {
                needs_string |= fs.iter().any(|(_, t)| t.trim() == "String");
            }
            if registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| v.parent == k_aver)
            {
                needs_string |= registry
                    .variants
                    .values()
                    .flat_map(|vs| vs.iter())
                    .filter(|v| v.parent == k_aver)
                    .any(|v| v.fields.iter().any(|t| t.trim() == "String"));
            }
            // Map<K,V>'s structural eq + hash dispatches V via the
            // same `__hash_<V>` / `__eq_<V>` helper map K uses, so V
            // is force-registered as pseudo-K too. Skips primitive V
            // (the body emitter falls back to inline cmp for those).
            let v_aver_trim = v_aver.trim();
            if v_aver_trim == "String" {
                needs_string = true;
            }
            // A value record / sum V is NOT newtype-erased when its
            // single field is `String` (String is not a wasm-gc
            // primitive), so it routes through `emit_hash_record` /
            // `emit_eq_record`, whose String-field arm needs the
            // String key helper. Mirror the K check above for V so a
            // direct `String` field of the value record force-
            // registers `String` even when K has none.
            if let Some(fs) = registry.record_fields.get(v_aver_trim) {
                needs_string |= fs.iter().any(|(_, t)| t.trim() == "String");
            }
            if registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| v.parent == v_aver_trim)
            {
                needs_string |= registry
                    .variants
                    .values()
                    .flat_map(|vs| vs.iter())
                    .filter(|v| v.parent == v_aver_trim)
                    .any(|v| v.fields.iter().any(|t| t.trim() == "String"));
            }
            if needs_string && k_seen.insert("String".into()) {
                k_names.push("String".into());
            }
            if k_seen.insert(k_aver.to_string()) {
                k_names.push(k_aver.to_string());
            }
            // ETAP-2 carrier-`i64`: an eligible carrier V is `i64`-erased —
            // it behaves like a primitive V, so it needs NO pseudo-K
            // `__hash_/__eq_<V>` helper (a struct-shaped body over an `i64` is
            // invalid wasm). The Map's value eq/hash dispatches it inline as
            // raw `i64.eq` / `i32.wrap_i64` (the `is_eligible_carrier` arms in
            // the map value-dispatch sites).
            if !super::types::TypeRegistry::is_primitive_map_key(v_aver_trim)
                && v_aver_trim != "String"
                && !registry.is_eligible_carrier(v_aver_trim)
                && k_seen.insert(v_aver_trim.to_string())
            {
                k_names.push(v_aver_trim.to_string());
            }
        }

        // For every record / sum K, recursively collect all
        // records / sums used as field types. Each nested type
        // needs its own hash + eq helpers so the outer K's per-
        // field dispatch can call them. Pseudo-K = registered for
        // helpers but with no `Map<X, *>` reachable.
        let mut to_visit: Vec<String> = k_names
            .iter()
            .filter(|n| {
                registry.record_type_idx(n).is_some()
                    || registry
                        .variants
                        .values()
                        .flat_map(|v| v.iter())
                        .any(|v| v.parent == *n.as_str())
            })
            .cloned()
            .collect();
        while let Some(parent) = to_visit.pop() {
            // Collect every field type referenced by this parent
            // (record fields, or every variant's fields if it's a
            // sum type).
            let mut field_types: Vec<String> = Vec::new();
            if let Some(fields) = registry.record_fields.get(&parent) {
                for (_, t) in fields {
                    field_types.push(t.trim().to_string());
                }
            }
            for variant in registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .filter(|v| v.parent == parent)
            {
                for t in &variant.fields {
                    field_types.push(t.trim().to_string());
                }
            }
            for ft in field_types {
                let is_record = registry.record_type_idx(&ft).is_some();
                let is_sum = registry
                    .variants
                    .values()
                    .flat_map(|v| v.iter())
                    .any(|v| v.parent == ft);
                let is_carrier = ft.starts_with("Option<")
                    || ft.starts_with("Result<")
                    || ft.starts_with("Tuple<");
                if (is_record || is_sum || is_carrier) && k_seen.insert(ft.clone()) {
                    k_names.push(ft.clone());
                    if !is_carrier {
                        // Records / sums recurse into their own
                        // fields. Carriers don't (their helper bodies
                        // delegate via Call to eq_helpers/hash_
                        // helpers, which handle inner types
                        // themselves).
                        to_visit.push(ft.clone());
                    }
                    // String inside the nested type's fields →
                    // force-register String. Carriers' name
                    // contains "String" only when an inner type is
                    // literally `String`; cheap heuristic.
                    let mut nested_needs_string = false;
                    if let Some(fs) = registry.record_fields.get(&ft) {
                        nested_needs_string |= fs.iter().any(|(_, t)| t.trim() == "String");
                    }
                    if is_sum {
                        nested_needs_string |= registry
                            .variants
                            .values()
                            .flat_map(|vs| vs.iter())
                            .filter(|v| v.parent == ft)
                            .any(|v| v.fields.iter().any(|t| t.trim() == "String"));
                    }
                    if is_carrier {
                        nested_needs_string |= ft.contains("String");
                    }
                    if nested_needs_string && k_seen.insert("String".into()) {
                        k_names.push("String".into());
                    }
                }
            }
        }

        // First pass: assign K-keyed helpers (hash, eq) per unique K.
        for k_aver in &k_names {
            if !self.key.contains_key(k_aver) {
                let hash_type_idx = *next_type_idx;
                *next_type_idx += 1;
                let eq_type_idx = *next_type_idx;
                *next_type_idx += 1;
                let hash_fn = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                let eq_fn = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                self.key.insert(
                    k_aver.clone(),
                    KeyHelpers {
                        hash: hash_fn,
                        eq: eq_fn,
                    },
                );
                self.key_type_indices
                    .insert(k_aver.clone(), (hash_type_idx, eq_type_idx));
                self.key_order.push(k_aver.clone());
            }
        }

        // Second pass: per (K, V) helpers.
        for canonical in map_canonicals {
            if self.kv.contains_key(canonical) {
                continue;
            }
            // Nine fn type slots and nine fn idx slots:
            // empty, set, get, len, get_or_default, get_pair, keys,
            // values, remove.
            let empty_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let set_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let set_in_place_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let get_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let len_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let god_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let pair_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let keys_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let values_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let remove_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let entries_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let from_list_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let eq_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let hash_type_idx = *next_type_idx;
            *next_type_idx += 1;
            let empty_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let set_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let set_in_place_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let get_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let len_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let god_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let pair_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let keys_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let values_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let remove_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let entries_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let from_list_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let eq_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let hash_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;

            // K can be String, a user-defined record (field-by-field
            // hash + eq), or a primitive (Int / Float / Bool). Primitive
            // keys are boxed into a per-K struct ref so the open-
            // addressing `keys[i] == null` empty marker still holds.
            let (k_aver, _) = super::types::parse_map_kv(canonical).ok_or(
                WasmGcError::Validation(format!("bad map canonical `{canonical}`")),
            )?;
            let is_primitive_k = super::types::TypeRegistry::is_primitive_map_key(k_aver);
            let is_sum_k = registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| v.parent == k_aver);
            let is_carrier_k = k_aver.starts_with("Option<")
                || k_aver.starts_with("Result<")
                || k_aver.starts_with("Tuple<");
            let is_list_or_vec_k = k_aver.starts_with("List<") || k_aver.starts_with("Vector<");
            let is_map_k = k_aver.starts_with("Map<");
            if k_aver != "String"
                && registry.record_type_idx(k_aver).is_none()
                && !is_primitive_k
                && !is_sum_k
                && !is_carrier_k
                && !is_list_or_vec_k
                && !is_map_k
            {
                return Err(WasmGcError::Unimplemented(
                    "phase 3c — Map<K, V> with K not String / user-record / sum / \
                     primitive / generic-carrier (Option/Result/Tuple) / List<T> / \
                     Vector<T> / Map<K2,V2>",
                ));
            }

            self.kv.insert(
                canonical.clone(),
                MapKVHelpers {
                    empty: empty_fn,
                    set: set_fn,
                    set_in_place: set_in_place_fn,
                    get: get_fn,
                    len: len_fn,
                    get_or_default: god_fn,
                    get_pair: pair_fn,
                    keys: keys_fn,
                    values: values_fn,
                    remove: remove_fn,
                    entries: entries_fn,
                    from_list: from_list_fn,
                    eq: eq_fn,
                    hash: hash_fn,
                },
            );
            self.kv_type_indices.insert(
                canonical.clone(),
                MapKVTypeIdx {
                    empty: empty_type_idx,
                    set: set_type_idx,
                    set_in_place: set_in_place_type_idx,
                    get: get_type_idx,
                    len: len_type_idx,
                    get_or_default: god_type_idx,
                    get_pair: pair_type_idx,
                    keys: keys_type_idx,
                    values: values_type_idx,
                    remove: remove_type_idx,
                    entries: entries_type_idx,
                    from_list: from_list_type_idx,
                    eq: eq_type_idx,
                    hash: hash_type_idx,
                },
            );
            self.kv_order.push(canonical.clone());
        }
        Ok(())
    }

    pub(super) fn key_helpers(&self, k_aver: &str) -> Option<KeyHelpers> {
        self.key.get(k_aver).copied()
    }

    pub(super) fn kv_helpers(&self, canonical: &str) -> Option<MapKVHelpers> {
        self.kv.get(canonical).copied()
    }

    /// `(wasm fn idx, name)` for the two insert helpers of every
    /// registered `Map<K, V>`, ascending by index — the shape the
    /// `name` section's function subsection wants.
    ///
    /// These are the only helpers that can trap: their probe loop
    /// gives up when the fixed bucket count is full. wasm traps carry
    /// no message of their own, so the capacity rides in the fn name
    /// and comes back out in the engine's backtrace.
    pub(super) fn capacity_helper_names(&self) -> Vec<(u32, String)> {
        let mut named = Vec::with_capacity(self.kv_order.len() * 2);
        for canonical in &self.kv_order {
            let Some(h) = self.kv.get(canonical) else {
                continue;
            };
            named.push((
                h.set,
                format!("{CAPACITY_HELPER_NAME_PREFIX}{canonical} (fixed capacity {INITIAL_CAP}, no resize)"),
            ));
            named.push((
                h.set_in_place,
                format!(
                    "{CAPACITY_HELPER_NAME_PREFIX}{canonical} in place (fixed capacity {INITIAL_CAP}, no resize)"
                ),
            ));
        }
        named.sort_by_key(|(idx, _)| *idx);
        named
    }

    /// Emit fn-type entries (in slot order) for every registered
    /// helper. Caller's `TypeSection` must be at `next_type_idx`'s
    /// starting position from the assign_slots call.
    pub(super) fn emit_helper_types(
        &self,
        types: &mut wasm_encoder::TypeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        for k_aver in &self.key_order {
            let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(
                WasmGcError::Validation(format!("Map K `{k_aver}` has no wasm rep")),
            )?;
            // hash : (K) -> i32
            types.ty().function([k_val], [ValType::I32]);
            // eq : (K, K) -> i32
            types.ty().function([k_val, k_val], [ValType::I32]);
        }
        for canonical in &self.kv_order {
            let slots = registry
                .map_slots(canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "Map slots missing for `{canonical}`"
                )))?;
            let map_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.map),
            });
            let (k_aver, v_aver) = super::types::parse_map_kv(canonical).ok_or(
                WasmGcError::Validation(format!("parse_map_kv `{canonical}`")),
            )?;
            let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(
                WasmGcError::Validation(format!("Map K `{k_aver}` has no wasm rep")),
            )?;
            let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.ok_or(
                WasmGcError::Validation(format!("Map V `{v_aver}` has no wasm rep")),
            )?;
            let opt_idx = registry
                .option_type_idx(&format!("Option<{v_aver}>"))
                .ok_or(WasmGcError::Validation(format!(
                    "Option<{v_aver}> not registered (Map.get needs it)"
                )))?;
            let opt_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(opt_idx),
            });

            // empty : () -> Map
            types.ty().function([], [map_ref]);
            // set : (Map, K, V) -> Map (clone-on-write)
            types.ty().function([map_ref, k_val, v_val], [map_ref]);
            // set_in_place : (Map, K, V) -> Map (alias-free fast path)
            types.ty().function([map_ref, k_val, v_val], [map_ref]);
            // get : (Map, K) -> Option<V>
            types.ty().function([map_ref, k_val], [opt_ref]);
            // len : (Map) -> i64
            types.ty().function([map_ref], [ValType::I64]);
            // get_or_default : (Map, K, V) -> V
            types.ty().function([map_ref, k_val, v_val], [v_val]);
            // get_pair : (Map, K) -> (i32 found, V value) — multi-result
            types.ty().function([map_ref, k_val], [ValType::I32, v_val]);
            // keys : (Map) -> List<K>
            let list_k_idx = registry.list_type_idx(&format!("List<{k_aver}>")).ok_or(
                WasmGcError::Validation(format!("Map.keys: List<{k_aver}> not registered")),
            )?;
            let list_k_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_k_idx),
            });
            types.ty().function([map_ref], [list_k_ref]);
            // values : (Map) -> List<V>
            let list_v_idx = registry.list_type_idx(&format!("List<{v_aver}>")).ok_or(
                WasmGcError::Validation(format!("Map.values: List<{v_aver}> not registered")),
            )?;
            let list_v_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_v_idx),
            });
            types.ty().function([map_ref], [list_v_ref]);
            // remove : (Map, K) -> Map
            types.ty().function([map_ref, k_val], [map_ref]);
            // entries : (Map) -> List<Tuple<K, V>>
            let tup_canonical = format!("Tuple<{k_aver},{v_aver}>");
            let tup_idx =
                registry
                    .tuple_type_idx(&tup_canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "Map.entries: `{tup_canonical}` not registered"
                    )))?;
            let lt_idx = registry
                .list_type_idx(&format!("List<{tup_canonical}>"))
                .ok_or(WasmGcError::Validation(format!(
                    "Map.entries: `List<{tup_canonical}>` not registered"
                )))?;
            let lt_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(lt_idx),
            });
            types.ty().function([map_ref], [lt_ref]);
            // from_list : (List<Tuple<K, V>>) -> Map
            types.ty().function([lt_ref], [map_ref]);
            // __eq_Map<K,V> : (eqref, eqref) -> i32. Eqref params so
            // record/sum/list/vec field dispatch can call uniformly
            // (the body ref.casts both args to the typed map ref).
            let eq_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Eq,
                },
            });
            types.ty().function([eq_ref, eq_ref], [ValType::I32]);
            // __hash_Map<K,V> : (eqref) -> i32. Same eqref-shape
            // calling convention as the carrier hash helpers.
            types.ty().function([eq_ref], [ValType::I32]);
            let _ = opt_ref;
            let _ = tup_idx;
        }
        Ok(())
    }

    /// Emit one `funcs.function(<type_idx>)` entry per registered
    /// helper, in the same order as `emit_helper_types`.
    pub(super) fn emit_function_section(&self, funcs: &mut wasm_encoder::FunctionSection) {
        for k in &self.key_order {
            let (h, e) = self.key_type_indices[k];
            funcs.function(h);
            funcs.function(e);
        }
        for canonical in &self.kv_order {
            let t = self.kv_type_indices[canonical];
            funcs.function(t.empty);
            funcs.function(t.set);
            funcs.function(t.set_in_place);
            funcs.function(t.get);
            funcs.function(t.len);
            funcs.function(t.get_or_default);
            funcs.function(t.get_pair);
            funcs.function(t.keys);
            funcs.function(t.values);
            funcs.function(t.remove);
            funcs.function(t.entries);
            funcs.function(t.from_list);
            funcs.function(t.eq);
            funcs.function(t.hash);
        }
    }

    /// Emit code bodies for every registered helper, in the same
    /// order as `emit_helper_types`.
    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
        list_eq_hash: &HashMap<String, (u32, u32)>,
        carrier_eq_hash: &HashMap<String, (u32, u32)>,
    ) -> Result<(), WasmGcError> {
        let string_key_helpers = self.key.get("String").copied();
        // Snapshot every K's helpers — record hash/eq dispatch
        // needs to call helpers for nested record fields. Plus
        // virtual entries for `List<T>` field types so hash/eq
        // dispatch can call into list_helpers without a
        // separate cross-module lookup. Carriers (Option / Result
        // / Tuple) come in through `carrier_eq_hash` from the
        // `__eq_<X>` / `__hash_<X>` registries (eq_helpers /
        // hash_helpers); their map-key body is a thin proxy that
        // Calls into those.
        let mut all_key_helpers: HashMap<String, KeyHelpers> =
            self.key.iter().map(|(k, h)| (k.clone(), *h)).collect();
        for (carrier, &(eq_fn, hash_fn)) in carrier_eq_hash {
            all_key_helpers.insert(
                carrier.clone(),
                KeyHelpers {
                    hash: hash_fn,
                    eq: eq_fn,
                },
            );
        }
        for (list_canonical, &(eq_fn, hash_fn)) in list_eq_hash {
            all_key_helpers.insert(
                list_canonical.clone(),
                KeyHelpers {
                    hash: hash_fn,
                    eq: eq_fn,
                },
            );
        }
        // Each per-instantiation `Map<K,V>` carries its own structural
        // eq + hash helpers (in `MapKVHelpers`). Surface them under the
        // canonical name so a sum variant whose field is `Map<K,V>`
        // can dispatch through the same `all_key_helpers` table the
        // rest of the compound-field logic uses.
        for (map_canonical, kv) in &self.kv {
            all_key_helpers.insert(
                map_canonical.clone(),
                KeyHelpers {
                    hash: kv.hash,
                    eq: kv.eq,
                },
            );
        }
        for k_aver in &self.key_order {
            codes.function(&emit_hash_for(
                k_aver,
                registry,
                string_key_helpers,
                &all_key_helpers,
            )?);
            codes.function(&emit_eq_for(
                k_aver,
                registry,
                string_key_helpers,
                &all_key_helpers,
            )?);
        }
        for canonical in &self.kv_order {
            let (k_aver, _) = super::types::parse_map_kv(canonical).ok_or(
                WasmGcError::Validation(format!("parse_map_kv `{canonical}`")),
            )?;
            let key_h = self
                .key_helpers(k_aver)
                .ok_or(WasmGcError::Validation(format!(
                    "key helpers missing for K=`{k_aver}`"
                )))?;
            codes.function(&emit_map_empty(canonical, registry)?);
            codes.function(&emit_map_set(canonical, registry, key_h)?);
            codes.function(&emit_map_set_in_place(canonical, registry, key_h)?);
            codes.function(&emit_map_get(canonical, registry, key_h)?);
            codes.function(&emit_map_len(canonical, registry)?);
            codes.function(&emit_map_get_or_default(canonical, registry, key_h)?);
            codes.function(&emit_map_get_pair(canonical, registry, key_h)?);
            codes.function(&emit_map_keys(canonical, registry)?);
            codes.function(&emit_map_values(canonical, registry)?);
            codes.function(&emit_map_remove(canonical, registry, key_h)?);
            let helpers = self.kv[canonical];
            codes.function(&emit_map_entries(canonical, registry)?);
            codes.function(&emit_map_from_list(canonical, registry, helpers.set)?);
            // Structural eq + commutative hash for `Map<K, V>`. V's
            // hash + eq fn idxs come from `all_key_helpers` (same
            // table that drives K dispatch — V is just another
            // shape that needs the same family of helpers when V is
            // not a primitive).
            let v_helpers = v_helper_for(canonical, &all_key_helpers, registry)?;
            codes.function(&emit_map_eq(
                canonical,
                registry,
                key_h,
                v_helpers,
                helpers.get,
            )?);
            codes.function(&emit_map_hash(canonical, registry, key_h, v_helpers)?);
        }
        Ok(())
    }
}

/// Resolve hash + eq fn idx for V — looks the same shape up as K. V
/// can be a primitive (hash/eq are inline instructions, not fn calls
/// — return None and let the body emitter pick the inline path), or
/// a ref-shaped K kind we already registered (records / sums /
/// carriers / List / Vector / Map). Returns the proxy fn idxs.
fn v_helper_for(
    canonical: &str,
    all_key_helpers: &HashMap<String, KeyHelpers>,
    _registry: &TypeRegistry,
) -> Result<Option<KeyHelpers>, WasmGcError> {
    let (_, v_aver) = super::types::parse_map_kv(canonical).ok_or(WasmGcError::Validation(
        format!("v_helper_for: bad canonical `{canonical}`"),
    ))?;
    let v_aver = v_aver.trim();
    if super::types::TypeRegistry::is_primitive_map_key(v_aver) {
        // Primitive V (Int / Float / Bool) — body emitter inlines
        // the comparison + hash, no helper dispatch.
        return Ok(None);
    }
    // String + every other ref V flows through `all_key_helpers`,
    // which assign_slots force-registered as pseudo-K (so the
    // helpers exist regardless of whether the program actually
    // holds `Map<V, _>`).
    Ok(all_key_helpers.get(v_aver).copied())
}

/// `hash : (K) -> i32`. K can be `String` (DJB2 over the bytes) or
/// any user-defined record (field-by-field combine, delegating to
/// the per-K helper for String fields).
fn emit_hash_for(
    k_aver: &str,
    registry: &TypeRegistry,
    string_key_helpers: Option<KeyHelpers>,
    all_key_helpers: &HashMap<String, KeyHelpers>,
) -> Result<Function, WasmGcError> {
    if k_aver == "String" {
        return emit_hash_string(registry);
    }
    // A newtype-erased value record — a `Map<_, V>` whose `V` is a
    // single-primitive-field record — reaches its hash helper as the
    // bare carrier, not the `$V` struct: `aver_to_wasm(V)` and the
    // `values_array` element are both `aver_to_wasm(under)`. Hash it as
    // the underlying primitive. Routing through `emit_hash_record` would
    // `struct.get` the unerased struct and diverge from the erased
    // signature (the `map_keyed_by_record_with_record_value` validation
    // bug). Map keys are never erased (`newtype_underlying` returns
    // `None` for `non_newtypable_keys`), so this fires only for value
    // records — exactly where `aver_to_wasm` erases.
    if let Some(under) = registry.newtype_underlying(k_aver) {
        return emit_hash_for(under, registry, string_key_helpers, all_key_helpers);
    }
    if registry.packed_sequence(k_aver).is_some() {
        let helpers = all_key_helpers.get(k_aver).ok_or_else(|| {
            WasmGcError::Validation(format!(
                "hash_for: packed `{k_aver}` has no registered hash helper"
            ))
        })?;
        let mut f = Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::Call(helpers.hash));
        f.instruction(&Instruction::End);
        return Ok(f);
    }
    if registry.record_type_idx(k_aver).is_some() {
        return emit_hash_record(k_aver, registry, string_key_helpers, all_key_helpers);
    }
    if super::types::TypeRegistry::is_primitive_map_key(k_aver) {
        return emit_hash_primitive(k_aver, registry);
    }
    if registry
        .variants
        .values()
        .flat_map(|v| v.iter())
        .any(|v| v.parent == k_aver)
    {
        return emit_hash_sum(k_aver, registry, string_key_helpers, all_key_helpers);
    }
    if k_aver.starts_with("Option<")
        || k_aver.starts_with("Result<")
        || k_aver.starts_with("Tuple<")
        || k_aver.starts_with("List<")
        || k_aver.starts_with("Vector<")
        || k_aver.starts_with("Map<")
    {
        // Same shape as carrier eq: proxy to the per-instantiation
        // `__hash_<X>` helper. Carriers come from hash_helpers,
        // List/Vector/Map from their own registries; all merged into
        // `all_key_helpers` via the compound lookup at module
        // assembly.
        let helpers = all_key_helpers
            .get(k_aver)
            .ok_or(WasmGcError::Validation(format!(
                "hash_for: compound `{k_aver}` has no registered hash helper"
            )))?;
        let mut f = Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::Call(helpers.hash));
        f.instruction(&Instruction::End);
        return Ok(f);
    }
    Err(WasmGcError::Unimplemented(
        "phase 3c — hash for unsupported K kind",
    ))
}

fn emit_eq_for(
    k_aver: &str,
    registry: &TypeRegistry,
    string_key_helpers: Option<KeyHelpers>,
    all_key_helpers: &HashMap<String, KeyHelpers>,
) -> Result<Function, WasmGcError> {
    if k_aver == "String" {
        return emit_eq_string(registry);
    }
    // Mirror of the newtype-erasure guard in `emit_hash_for`: a
    // newtype-erased value record reaches its eq helper as two bare
    // carriers (`aver_to_wasm(V) == aver_to_wasm(under)`), so compare
    // them as the underlying primitive instead of `struct.get`-ing the
    // unerased `$V` struct (which would diverge from the erased
    // signature). Keys are never erased, so this fires only for value
    // records.
    if let Some(under) = registry.newtype_underlying(k_aver) {
        return emit_eq_for(under, registry, string_key_helpers, all_key_helpers);
    }
    if registry.packed_sequence(k_aver).is_some() {
        let helpers = all_key_helpers.get(k_aver).ok_or_else(|| {
            WasmGcError::Validation(format!(
                "eq_for: packed `{k_aver}` has no registered eq helper"
            ))
        })?;
        let mut f = Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::Call(helpers.eq));
        f.instruction(&Instruction::End);
        return Ok(f);
    }
    if registry.record_type_idx(k_aver).is_some() {
        return emit_eq_record(k_aver, registry, string_key_helpers, all_key_helpers);
    }
    if super::types::TypeRegistry::is_primitive_map_key(k_aver) {
        return emit_eq_primitive(k_aver, registry);
    }
    if registry
        .variants
        .values()
        .flat_map(|v| v.iter())
        .any(|v| v.parent == k_aver)
    {
        return emit_eq_sum(k_aver, registry, string_key_helpers, all_key_helpers);
    }
    if k_aver.starts_with("Option<")
        || k_aver.starts_with("Result<")
        || k_aver.starts_with("Tuple<")
        || k_aver.starts_with("List<")
        || k_aver.starts_with("Vector<")
        || k_aver.starts_with("Map<")
    {
        // Map-key eq for compounds proxies to `__eq_<X>` (carriers
        // from eq_helpers, List/Vector from list_helpers, Map from
        // MapHelperRegistry::kv). All merged into `all_key_helpers`
        // at module assembly time.
        let helpers = all_key_helpers
            .get(k_aver)
            .ok_or(WasmGcError::Validation(format!(
                "eq_for: compound `{k_aver}` has no registered eq helper"
            )))?;
        let mut f = Function::new([]);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::Call(helpers.eq));
        f.instruction(&Instruction::End);
        return Ok(f);
    }
    Err(WasmGcError::Unimplemented(
        "phase 3c — eq for unsupported K kind",
    ))
}

/// `hash : (K_raw) -> i32` for primitive K. Helpers consume raw
/// primitives (callers don't have to box just to compute a hash).
/// Map's keys array stores boxed refs, but `hash` runs on the raw
/// value the user passed in.
fn emit_hash_primitive(k_aver: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    match k_aver {
        "Int" => {
            // Flag-off: `i32.wrap_i64` — keeps low 32 bits. Cheap,
            // distributes poorly for tightly-clustered Int domains; bench
            // scenarios don't stress this. Flag-on (bignum): the key is an
            // `$aint` ref, so route through `__aint_hash` (an
            // `i32.wrap_i64` on a ref is invalid wasm, and all Big keys
            // would otherwise collapse into one bucket).
            super::lists::emit_aint_field_hash(&mut f, registry)?;
        }
        "Float" => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        "Bool" => {
            // Already i32 — no-op (just LocalGet then End)
        }
        _ => panic!(
            "internal compiler error: emit_hash_primitive called with \
             non-primitive K = `{k_aver}`; caller must dispatch to \
             emit_hash_record / emit_hash_sum / __wasmgc_string_hash for \
             non-primitive K. Please file at https://github.com/jasisz/aver/issues"
        ),
    }
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `eq : (K_raw, K_raw) -> i32` for primitive K. Native eq
/// instruction per K kind.
fn emit_eq_primitive(k_aver: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    match k_aver {
        // Flag-on (bignum): keys are `$aint` refs → `__aint_eq` (an
        // `i64.eq` on a ref is invalid wasm and wrong across Small/Big).
        // Flag-off: byte-identical `i64.eq`.
        "Int" => {
            super::lists::emit_aint_field_eq(&mut f, registry)?;
        }
        "Float" => {
            f.instruction(&Instruction::F64Eq);
        }
        "Bool" => {
            f.instruction(&Instruction::I32Eq);
        }
        _ => panic!(
            "internal compiler error: emit_eq_primitive called with \
             non-primitive K = `{k_aver}`; caller must dispatch to \
             emit_eq_record / emit_eq_sum / __wasmgc_string_eq for \
             non-primitive K. Please file at https://github.com/jasisz/aver/issues"
        ),
    };
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Wasm value type used as the `keys` array element. Primitive K
/// stores boxed refs (`(ref null $primitive_key_box_K)`) so the
/// open-addressing `keys[i] == null` empty marker stays uniform;
/// ref K (String / record) stores its own ref directly.
fn key_storage_val_type(k_aver: &str, registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
        Ok(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(box_idx),
        }))
    } else {
        super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(WasmGcError::Validation(format!(
            "Map key type `{k_aver}` has no wasm representation"
        )))
    }
}

/// Append the instructions that turn a stored-key value (top of
/// stack) into the raw K_val that `hash` / `eq` expect. For primitive
/// K: `struct.get $box 0` to unbox; for ref K: no-op.
fn emit_unbox_key(f: &mut Function, k_aver: &str, registry: &TypeRegistry) {
    if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
        f.instruction(&Instruction::StructGet {
            struct_type_index: box_idx,
            field_index: 0,
        });
    }
}

/// Append the instructions that turn a raw K_val (top of stack) into
/// a stored-key value ready for `array.set`. For primitive K:
/// `struct.new $box`; for ref K: no-op.
fn emit_box_key(f: &mut Function, k_aver: &str, registry: &TypeRegistry) {
    if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
        f.instruction(&Instruction::StructNew(box_idx));
    }
}

/// Heap type used by `Map.remove` for `ref.null` in the keys-array
/// store-back. Concrete idx for K kinds with their own struct type
/// (primitive K boxes, String, record, carrier, List, Vector); abstract
/// Heap type of the `ref.null` written into a cleared key slot, per
/// K kind. Matches the keys array element type exactly (a sum K uses
/// its nominal root struct).
fn key_storage_null_heap(k_aver: &str, registry: &TypeRegistry) -> HeapType {
    if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
        return HeapType::Concrete(box_idx);
    }
    if k_aver == "String"
        && let Some(s) = registry.string_array_type_idx
    {
        return HeapType::Concrete(s);
    }
    if let Some(packed) = registry.packed_sequence(k_aver) {
        return HeapType::Concrete(packed.type_idx);
    }
    if let Some(r) = registry.record_type_idx(k_aver) {
        return HeapType::Concrete(r);
    }
    if let Some(o) = registry.option_type_idx(k_aver) {
        return HeapType::Concrete(o);
    }
    if let Some(r) = registry.result_type_idx(k_aver) {
        return HeapType::Concrete(r);
    }
    if let Some(t) = registry.tuple_type_idx(k_aver) {
        return HeapType::Concrete(t);
    }
    if let Some(l) = registry.list_type_idx(k_aver) {
        return HeapType::Concrete(l);
    }
    if let Some(v) = registry.vector_type_idx(k_aver) {
        return HeapType::Concrete(v);
    }
    if let Some(slots) = registry.map_slots(k_aver) {
        return HeapType::Concrete(slots.map);
    }
    // Sum K — the nominal root struct the keys array declares as its
    // element heap type, so the array.set typechecks.
    if let Some(root) = registry.sum_root_type_idx(k_aver) {
        return HeapType::Concrete(root);
    }
    HeapType::Abstract {
        shared: false,
        ty: wasm_encoder::AbstractHeapType::Eq,
    }
}

fn string_idx(registry: &TypeRegistry) -> Result<u32, WasmGcError> {
    registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "Map<String, _> helper requires String slot".into(),
        ))
}

/// DJB2 hash over the byte content of a `(ref null $string)`.
/// `h = 5381; for b in s: h = h * 33 + b`. Standard non-cryptographic
/// hash used in legacy backend's `rt_hash_string` shape.
fn emit_hash_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let s_idx = string_idx(registry)?;
    let padding = wat_helper::padding_types(s_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (func (export "helper") (param $s (ref null $string)) (result i32)
            (local $h i32)
            (local $i i32)
            (local $n i32)
            ;; DJB2: h = 5381; for each byte: h = h * 33 + byte.
            i32.const 5381
            local.set $h
            local.get $s
            array.len
            local.set $n
            i32.const 0
            local.set $i
            (block $break
              (loop $next
                local.get $i
                local.get $n
                i32.ge_u
                br_if $break

                ;; h = (h << 5) + h + s[i]
                local.get $h
                i32.const 5
                i32.shl
                local.get $h
                i32.add
                local.get $s
                local.get $i
                array.get_u $string
                i32.add
                local.set $h

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $next))
            local.get $h)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

/// Byte-equal compare of two `(ref null $string)`. Returns 1 if equal.
fn emit_eq_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let s_idx = string_idx(registry)?;
    let padding = wat_helper::padding_types(s_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (func (export "helper") (param $a (ref null $string)) (param $b (ref null $string)) (result i32)
            (local $i i32)
            (local $n i32)
            ;; Length mismatch ⇒ 0.
            local.get $a
            array.len
            local.get $b
            array.len
            i32.ne
            (if
              (then i32.const 0 return))

            local.get $a
            array.len
            local.set $n
            i32.const 0
            local.set $i
            (block $break
              (loop $next
                local.get $i
                local.get $n
                i32.ge_u
                br_if $break

                local.get $a
                local.get $i
                array.get_u $string
                local.get $b
                local.get $i
                array.get_u $string
                i32.ne
                (if
                  (then i32.const 0 return))

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $next))
            i32.const 1)
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}

fn slots_for(canonical: &str, registry: &TypeRegistry) -> Result<MapSlots, WasmGcError> {
    registry
        .map_slots(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map slots missing for `{canonical}`"
        )))
}

/// `empty() -> Map<K, V>`. Allocates fresh keys/values arrays at
/// `INITIAL_CAP` and a struct wrapping them.
fn emit_map_empty(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let mut f = Function::new([]);
    // size = 0; cap = INITIAL_CAP; keys = array.new_default; values = array.new_default
    f.instruction(&Instruction::I32Const(0)); // size
    f.instruction(&Instruction::I32Const(INITIAL_CAP)); // cap
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(slots.keys_array));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(slots.values_array));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `set(map, k, v) -> map`. Linear-probing open-addressing insert.
/// Mutates `map` in place; returns same ref.
fn emit_map_set(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    // params: 0=map, 1=k, 2=v
    // locals: 3=cap, 4=mask, 5=idx, 6=keys (CLONED), 7=values (CLONED),
    //         8=cur_key, 9=home
    //
    // Clone-on-write: at entry we allocate fresh `keys` and `values`
    // arrays, `array.copy` the source map's contents into them, and
    // probe / mutate exclusively on the clones. The returned map struct
    // wraps the cloned arrays, so the input map's keys/values are
    // never observed mutated by anyone holding an alias of it. Without
    // this, `Vector.new(n, m)` produced N aliases of `m` and a
    // `Map.set(row, …)` on a row fetched via `Vector.get(outer, i)`
    // silently rewrote every alias of that map.
    let mut f = Function::new([
        (1, ValType::I32), // 3: cap
        (1, ValType::I32), // 4: mask
        (1, ValType::I32), // 5: idx
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.keys_array),
            }),
        ), // 6: keys (the freshly-allocated clone)
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ), // 7: values (the freshly-allocated clone)
        (1, key_storage_val_type(k_aver, registry)?), // 8: cur_key (boxed for primitive)
        (1, ValType::I32), // 9: home (probe start bucket)
    ]);
    let _ = (v_val, k_val);

    // cap = map.cap; mask = cap - 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));

    // keys = array.new_default $keys cap; array.copy keys 0 map.keys 0 cap
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayNewDefault(slots.keys_array));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: slots.keys_array,
        array_type_index_src: slots.keys_array,
    });

    // values = array.new_default $values cap; array.copy values 0 map.values 0 cap
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayNewDefault(slots.values_array));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: slots.values_array,
        array_type_index_src: slots.values_array,
    });

    // idx = hash(k) & mask; home = idx
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(9));

    // Probe from `home`, bounded by the wrap guard at the bottom.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // cur_key = keys[idx]
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(8));

    // if cur_key == null: empty slot, insert.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    // keys[idx] = box(k)  (primitive K) or k (ref K)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(1));
    emit_box_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::ArraySet(slots.keys_array));
    // values[idx] = v
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArraySet(slots.values_array));
    // return struct.new $map (map.size + 1, cap, new_keys, new_values)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // else if eq(unbox(cur_key), k): update value at idx, return clone.
    f.instruction(&Instruction::LocalGet(8));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.eq));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArraySet(slots.values_array));
    // return struct.new $map (map.size, cap, new_keys, new_values)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // else: idx = (idx + 1) & mask; continue
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
    emit_full_table_trap(&mut f, 5, 9);
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Bottom-of-probe-loop guard for the two insert helpers: `idx` back at
/// `home` means all `cap` slots were visited, every one occupied and
/// none matching the key. The table does not resize, so the entry has
/// nowhere to go — trap rather than probe forever.
///
/// Placed after the `idx = (idx + 1) & mask` step, so the loop has
/// already examined `home … home + cap - 1`: a table with a single free
/// slot still inserts, and only a genuinely full one traps.
///
/// The `name` section (see `MapHelperRegistry::capacity_helper_names`)
/// carries the capacity into the helper's name, so wasmtime's backtrace
/// says which limit was hit instead of `<wasm function N>`.
fn emit_full_table_trap(f: &mut Function, idx_local: u32, home_local: u32) {
    f.instruction(&Instruction::LocalGet(idx_local));
    f.instruction(&Instruction::LocalGet(home_local));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
}

/// The lookup half of [`emit_full_table_trap`], shared by `get`,
/// `get_or_default` and `get_pair`: `idx` back at `home` means every
/// slot was occupied and none matched, so the key is absent. A miss is
/// an answer, not an error — `remove` has always given it — so each
/// helper returns its own shape of "absent", emitted by `miss`.
///
/// Same placement rule as the insert guard: after the
/// `idx = (idx + 1) & mask` step, so the slot the probe started at is
/// examined before the wrap is declared.
fn emit_wrap_miss(
    f: &mut Function,
    idx_local: u32,
    home_local: u32,
    miss: impl FnOnce(&mut Function),
) {
    f.instruction(&Instruction::LocalGet(idx_local));
    f.instruction(&Instruction::LocalGet(home_local));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    miss(f);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
}

/// `set_in_place(map, k, v) -> map`. Same probe-and-write loop as
/// `emit_map_set` but without the entry-time `array.copy` of
/// `keys` / `values` — the caller has proven (via `ir::alias` and
/// `last_use`) that `map`'s engine arrays are uniquely owned, so
/// rewriting them in place is sound and saves two `array.new_default`
/// plus two `array.copy` per call. The returned struct still re-wraps
/// the same arrays with the updated size; callers expect a fresh
/// map handle either way.
fn emit_map_set_in_place(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let _ = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let _ = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    // params: 0=map, 1=k, 2=v
    // locals: 3=cap, 4=mask, 5=idx,
    //         6=keys (alias of map.keys), 7=values (alias of map.values),
    //         8=cur_key, 9=home
    let mut f = Function::new([
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.keys_array),
            }),
        ),
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ),
        (1, key_storage_val_type(k_aver, registry)?),
        (1, ValType::I32), // 9: home (probe start bucket)
    ]);

    // cap = map.cap; mask = cap - 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));

    // keys = map.keys; values = map.values  (no array.copy — alias-free)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(7));

    // idx = hash(k) & mask; home = idx
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(9));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // cur_key = keys[idx]
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(8));

    // empty slot: insert
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(1));
    emit_box_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::ArraySet(slots.keys_array));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArraySet(slots.values_array));
    // size + 1, cap, keys, values  (same arrays, fresh struct)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // matching key: update value
    f.instruction(&Instruction::LocalGet(8));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.eq));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArraySet(slots.values_array));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // collision: probe forward
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
    emit_full_table_trap(&mut f, 5, 9);
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `get(map, k) -> Option<V>`. Linear-probing lookup; null slot →
/// None, matching key → Some(value).
fn emit_map_get(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    let opt_canonical = format!("Option<{v_aver}>");
    let opt_idx = registry
        .option_type_idx(&opt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.get: Option<{v_aver}> not registered"
        )))?;

    let mut f = Function::new([
        (1, ValType::I32), // 2: cap
        (1, ValType::I32), // 3: mask
        (1, ValType::I32), // 4: idx
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.keys_array),
            }),
        ), // 5: keys
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ), // 6: values
        (1, key_storage_val_type(k_aver, registry)?), // 7: cur_key (boxed for prim)
        (1, ValType::I32), // 8: home (probe start bucket)
    ]);
    let _ = k_val;
    // cap, mask, keys, values
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(6));
    // idx = hash(k) & mask; home = idx
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalSet(8));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(7));
    // if cur_key == null → return None
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    emit_default_value_for(&mut f, v_val);
    f.instruction(&Instruction::StructNew(opt_idx));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // if eq(unbox(cur_key), k) → return Some(values[idx])
    f.instruction(&Instruction::LocalGet(7));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.eq));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::StructNew(opt_idx));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // else: idx = (idx + 1) & mask
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));
    // wrapped back to home → full-table miss, return None.
    emit_wrap_miss(&mut f, 4, 8, |f| {
        f.instruction(&Instruction::I32Const(0));
        emit_default_value_for(f, v_val);
        f.instruction(&Instruction::StructNew(opt_idx));
    });
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `get_or_default(map, k, default) -> V`. Same probe loop as
/// `get`, but returns `values[idx]` (or the supplied default) directly
/// — no `Option<V>` boxing on the way out. Backs the fused
/// `Option.withDefault(Map.get(m, k), default)` shape that's hot in
/// `map_lookup`-style benches: one alloc per lookup → zero.
fn emit_map_get_or_default(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    let _ = (k_val, v_val);
    // params: 0=map, 1=k, 2=default
    // locals: 3=cap, 4=mask, 5=idx, 6=keys, 7=values, 8=cur_key, 9=home
    let mut f = Function::new([
        (1, ValType::I32), // 3: cap
        (1, ValType::I32), // 4: mask
        (1, ValType::I32), // 5: idx
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.keys_array),
            }),
        ),
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ),
        (1, key_storage_val_type(k_aver, registry)?), // 8: cur_key
        (1, ValType::I32),                            // 9: home (probe start bucket)
    ]);

    // cap = map.cap; mask = cap - 1; keys = map.keys; values = map.values
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(7));

    // idx = hash(k) & mask; home = idx
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(9));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // cur_key = keys[idx]
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(8));
    // empty slot → return default
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // key match → return values[idx]
    f.instruction(&Instruction::LocalGet(8));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.eq));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // probe forward
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
    // wrapped back to home → full-table miss, return the default.
    emit_wrap_miss(&mut f, 5, 9, |f| {
        f.instruction(&Instruction::LocalGet(2));
    });
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `get_pair(map, k) -> (i32 found, V value)`. Backs the fused
/// `match Map.get(m, k) { Option.Some(v) -> ...; Option.None -> ... }`
/// shape — the caller pops `value` into the binding slot and branches
/// on `found`. No `Option<V>` ever allocates.
///
/// On an empty slot returns `(0, default<V>)` so the multi-result
/// signature stays well-typed; on a key match returns `(1, values[idx])`.
fn emit_map_get_pair(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    let _ = k_val;
    // params: 0=map, 1=k
    // locals: 2=cap, 3=mask, 4=idx, 5=keys, 6=values, 7=cur_key, 8=home
    let mut f = Function::new([
        (1, ValType::I32), // 2: cap
        (1, ValType::I32), // 3: mask
        (1, ValType::I32), // 4: idx
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.keys_array),
            }),
        ),
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ),
        (1, key_storage_val_type(k_aver, registry)?), // 7: cur_key
        (1, ValType::I32),                            // 8: home (probe start bucket)
    ]);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(6));

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalSet(8));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(7));

    // empty slot → return (0, default<V>)
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    emit_default_value_for(&mut f, v_val);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // key match → return (1, values[idx])
    f.instruction(&Instruction::LocalGet(7));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.eq));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // probe forward
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));
    // wrapped back to home → full-table miss, return (0, default<V>).
    emit_wrap_miss(&mut f, 4, 8, |f| {
        f.instruction(&Instruction::I32Const(0));
        emit_default_value_for(f, v_val);
    });
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `len(map) -> i64`. Reads `size` from the struct, widens to i64.
fn emit_map_len(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::I64ExtendI32U);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Push a default value of the given wasm type onto the stack — used
/// by `Map.get`'s None branch where the Option's `value` field needs
/// some payload (read by no well-typed Aver code, since pattern match
/// dispatches on tag first).
fn emit_default_value_for(f: &mut Function, ty: ValType) {
    match ty {
        ValType::I32 => {
            f.instruction(&Instruction::I32Const(0));
        }
        ValType::I64 => {
            f.instruction(&Instruction::I64Const(0));
        }
        ValType::F32 => {
            f.instruction(&Instruction::F32Const(0.0_f32.into()));
        }
        ValType::F64 => {
            f.instruction(&Instruction::F64Const(0.0_f64.into()));
        }
        ValType::Ref(rt) => {
            f.instruction(&Instruction::RefNull(rt.heap_type));
        }
        ValType::V128 => {
            // V128 not used by Aver primitives; emit a zero literal
            // for completeness so the helper still type-checks if a
            // future `Vector<V128>` ever surfaces.
            f.instruction(&Instruction::V128Const(0));
        }
    }
}

/// Field-by-field hash combine for a user record. Iterates each
/// field, picks an inline hash strategy from the field's Aver type,
/// and folds with `h = h * 33 + field_hash`. String fields delegate
/// to the per-K String hash helper (force-registered when any
/// record key transitively uses one). Field types we can't hash
/// natively (lists, vectors, nested records, sums) surface as
/// Unimplemented; widening that set is a follow-up.
fn emit_hash_record(
    record_name: &str,
    registry: &TypeRegistry,
    string_key_helpers: Option<KeyHelpers>,
    all_key_helpers: &HashMap<String, KeyHelpers>,
) -> Result<Function, WasmGcError> {
    let record_idx = registry
        .record_type_idx(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "hash_record: `{record_name}` not registered"
        )))?;
    let fields = registry
        .record_fields
        .get(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "hash_record: `{record_name}` has no field info"
        )))?;
    let mut f = Function::new([(1, ValType::I32) /* h */]);
    // h = 5381
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(1));
    for (i, (field_name, field_ty)) in fields.iter().enumerate() {
        // h = h * 33 (h<<5 + h)
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::I32Add);
        // push struct.get of field
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::StructGet {
            struct_type_index: record_idx,
            field_index: i as u32,
        });
        // emit field hash → i32
        match field_ty.trim() {
            "Int" => {
                // Flag-off: i64 → low 32 bits. Flag-on (bignum): the field
                // is an `$aint` ref → `__aint_hash`. ETAP-2 multi-field
                // carrier-`i64`: a bounded field erased to a native `i64`
                // hashes raw `i32.wrap_i64` even under bignum (agrees with
                // its `i64.eq`).
                super::lists::emit_record_int_field_hash(
                    &mut f,
                    registry,
                    record_name,
                    field_name,
                )?;
            }
            "Bool" => {
                // already i32
            }
            "Float" => {
                // f64 bit pattern → low 32 bits
                f.instruction(&Instruction::I64ReinterpretF64);
                f.instruction(&Instruction::I32WrapI64);
            }
            "String" => {
                let helpers = string_key_helpers.ok_or(WasmGcError::Validation(
                    "hash_record: String field needs String key helpers".into(),
                ))?;
                f.instruction(&Instruction::Call(helpers.hash));
            }
            other => {
                // Nested record / List<T> field. Both dispatch via
                // `all_key_helpers` — records were force-registered
                // as pseudo-K in `assign_slots`; list canonicals were
                // injected by `emit_helper_bodies` from list_helpers;
                // carriers (Option/Result/Tuple) come from
                // `carrier_eq_hash`.
                let lookup_key = if other.starts_with("List<") || other.starts_with("Vector<") {
                    super::types::normalize_compound(other).to_string()
                } else {
                    other.to_string()
                };
                let is_compound = other.starts_with("List<") || other.starts_with("Vector<");
                let is_carrier = other.starts_with("Option<")
                    || other.starts_with("Result<")
                    || other.starts_with("Tuple<");
                let is_sum = registry
                    .variants
                    .values()
                    .flat_map(|v| v.iter())
                    .any(|v| v.parent == other);
                if registry.record_type_idx(other).is_some() || is_compound || is_sum || is_carrier
                {
                    let inner = all_key_helpers
                        .get(&lookup_key)
                        .ok_or(WasmGcError::Validation(format!(
                            "hash_record: field `{other}` has no key helpers \
                             (record / list / vector / sum / Option / Result / Tuple T \
                              may need force-registration)"
                        )))?;
                    f.instruction(&Instruction::Call(inner.hash));
                } else {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3c — record-key field type not in \
                         {Int, Float, Bool, String, nested record, List<T>, Vector<T>, sum, \
                          Option/Result/Tuple}",
                    ));
                }
            }
        }
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(1));
    }
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Field-by-field equality for a user record. Returns 1 iff every
/// field-pair tests equal under its type's natural compare. Short-
/// circuits on first mismatch via `if … return 0`.
fn emit_eq_record(
    record_name: &str,
    registry: &TypeRegistry,
    string_key_helpers: Option<KeyHelpers>,
    all_key_helpers: &HashMap<String, KeyHelpers>,
) -> Result<Function, WasmGcError> {
    let record_idx = registry
        .record_type_idx(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "eq_record: `{record_name}` not registered"
        )))?;
    let fields = registry
        .record_fields
        .get(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "eq_record: `{record_name}` has no field info"
        )))?;
    let mut f = Function::new([]);
    for (i, (field_name, field_ty)) in fields.iter().enumerate() {
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::StructGet {
            struct_type_index: record_idx,
            field_index: i as u32,
        });
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: record_idx,
            field_index: i as u32,
        });
        match field_ty.trim() {
            // Flag-on (bignum): `$aint` ref → `__aint_eq`. Flag-off: `i64.eq`.
            // ETAP-2 multi-field carrier-`i64`: a bounded field erased to a
            // native `i64` compares raw `i64.eq` even under bignum.
            "Int" => {
                super::lists::emit_record_int_field_eq(&mut f, registry, record_name, field_name)?;
            }
            "Bool" => {
                f.instruction(&Instruction::I32Eq);
            }
            "Float" => {
                f.instruction(&Instruction::F64Eq);
            }
            "String" => {
                let helpers = string_key_helpers.ok_or(WasmGcError::Validation(
                    "eq_record: String field needs String key helpers".into(),
                ))?;
                f.instruction(&Instruction::Call(helpers.eq));
            }
            other => {
                let lookup_key = if other.starts_with("List<") || other.starts_with("Vector<") {
                    super::types::normalize_compound(other).to_string()
                } else {
                    other.to_string()
                };
                let is_compound = other.starts_with("List<") || other.starts_with("Vector<");
                let is_carrier = other.starts_with("Option<")
                    || other.starts_with("Result<")
                    || other.starts_with("Tuple<");
                let is_sum = registry
                    .variants
                    .values()
                    .flat_map(|v| v.iter())
                    .any(|v| v.parent == other);
                if registry.record_type_idx(other).is_some() || is_compound || is_sum || is_carrier
                {
                    let inner = all_key_helpers
                        .get(&lookup_key)
                        .ok_or(WasmGcError::Validation(format!(
                            "eq_record: field `{other}` has no key helpers"
                        )))?;
                    f.instruction(&Instruction::Call(inner.eq));
                } else {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3c — record-key field type not in \
                         {Int, Float, Bool, String, nested record, List<T>, Vector<T>, sum, \
                          Option/Result/Tuple}",
                    ));
                }
            }
        };
        f.instruction(&Instruction::I32Eqz);
        f.instruction(&Instruction::If(BlockType::Empty));
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Return);
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `keys(m) -> List<K>`. Walks `m.keys` right-to-left, prepending
/// each non-null key onto a cons-list accumulator. Returns hash-bucket
/// order.
///
/// That is a DIVERGENCE, not the documented behaviour: `Map.keys` is
/// specified to iterate sorted by key, which is what the VM, the compiled
/// Rust backend and the exported proof model all do. On a ten-key String
/// map the VM reads `alpha,beta,delta,...` here and wasm-gc reads
/// `beta,iota,epsilon,...`. It is internally consistent — `values` and
/// `entries` walk the same buckets, so `keys[i]` still pairs with
/// `values[i]` — but it disagrees with every other backend, and
/// `proof_trust_header` carries a carve-out saying so. Fixing it is a
/// wasm-gc change of its own.
fn emit_map_keys(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, _) = super::types::parse_map_kv(canonical).unwrap();
    let list_canonical = format!("List<{k_aver}>");
    let list_idx = registry
        .list_type_idx(&list_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.keys: `{list_canonical}` not registered"
        )))?;
    emit_map_walk_keys_to_list(slots.map, slots.keys_array, list_idx, k_aver, registry)
}

/// `values(m) -> List<V>`. Same shape as `keys` but pulls from
/// `m.values` whenever the corresponding `m.keys[i]` is non-null.
fn emit_map_values(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (_, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let list_canonical = format!("List<{v_aver}>");
    let list_idx = registry
        .list_type_idx(&list_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.values: `{list_canonical}` not registered"
        )))?;
    emit_map_walk_values_to_list(slots, registry, list_idx)
}

/// Real impl for `Map.keys` walking the keys array. Per primitive
/// K the stored values are boxed refs; unbox before consing onto
/// the result list. For ref K (String / record), no unboxing.
fn emit_map_walk_keys_to_list(
    map_idx: u32,
    keys_array_idx: u32,
    list_idx: u32,
    k_aver: &str,
    registry: &TypeRegistry,
) -> Result<Function, WasmGcError> {
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(keys_array_idx),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=map. locals: 1=keys, 2=i, 3=acc.
    let mut f = Function::new([(1, keys_ref), (1, ValType::I32), (1, list_ref)]);
    // keys = map.keys
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(1));
    // i = map.cap - 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    // acc = null
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i < 0 break
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::BrIf(1));
    // if keys[i] != null: acc = cons(unbox(keys[i]), acc)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGet(keys_array_idx));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGet(keys_array_idx));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::End);
    // i--
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Real impl for `Map.values` walking the values array (with keys-
/// array null-check for occupancy).
fn emit_map_walk_values_to_list(
    slots: super::types::MapSlots,
    _registry: &TypeRegistry,
    list_idx: u32,
) -> Result<Function, WasmGcError> {
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.keys_array),
    });
    let values_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.values_array),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=map. locals: 1=keys, 2=values, 3=i, 4=acc.
    let mut f = Function::new([
        (1, keys_ref),
        (1, values_ref),
        (1, ValType::I32),
        (1, list_ref),
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::BrIf(1));
    // if keys[i] != null: acc = cons(values[i], acc)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `__eq_Map<K,V>(a: eqref, b: eqref) -> i32`. Structural eq —
/// `a.size == b.size && ∀ k ∈ a: get(b, k) == Some(a[k])`. Order-
/// independent (matches VM's Rust HashMap PartialEq).
fn emit_map_eq(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
    v_helpers: Option<KeyHelpers>,
    get_fn_idx: u32,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let _ = keyh;
    let map_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.map),
    });
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.keys_array),
    });
    let values_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.values_array),
    });
    let v_val =
        super::types::aver_to_wasm(v_aver, Some(registry))?.ok_or(WasmGcError::Validation(
            format!("Map<{k_aver},{v_aver}>.eq: V `{v_aver}` has no wasm rep"),
        ))?;
    let opt_idx = registry
        .option_type_idx(&format!("Option<{v_aver}>"))
        .ok_or(WasmGcError::Validation(format!(
            "Map.eq: `Option<{v_aver}>` not registered"
        )))?;
    let opt_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(opt_idx),
    });
    // Locals: 2=typed map_a, 3=typed map_b, 4=cap, 5=i, 6=keys_a,
    // 7=values_a, 8=cur_key (boxed), 9=opt result, 10=v_a, 11=v_b
    let mut f = Function::new(vec![
        (1, map_ref),
        (1, map_ref),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, keys_ref),
        (1, values_ref),
        (1, key_storage_val_type(k_aver, registry)?),
        (1, opt_ref),
        (1, v_val),
        (1, v_val),
    ]);
    let map_heap = HeapType::Concrete(slots.map);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(map_heap));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefCastNonNull(map_heap));
    f.instruction(&Instruction::LocalSet(3));
    // if a.size != b.size return 0
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // cap = a.cap; keys_a = a.keys; values_a = a.values; i = 0
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(5));
    // for i in 0..cap
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::BrIf(1));
    // cur_key = keys_a[i]
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(8));
    // if cur_key != null: probe b
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    // opt = get_fn(b, unbox(cur_key))
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(8));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::Call(get_fn_idx));
    f.instruction(&Instruction::LocalSet(9));
    // if opt.tag == 0 → return 0
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // v_a = values_a[i]; v_b = opt.value
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(11));
    // if v_a != v_b → return 0
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::LocalGet(11));
    emit_v_eq(&mut f, v_aver, v_helpers, registry)?;
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // i++
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Stack: `[v_a, v_b]` of Aver type `v_aver`. Push i32 (1=eq, 0=ne).
/// Primitive V → inline cmp; ref V → `Call(v_helpers.eq)`.
fn emit_v_eq(
    f: &mut Function,
    v_aver: &str,
    v_helpers: Option<KeyHelpers>,
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    // ETAP-2 carrier-`i64`: an eligible carrier V is a native `i64` in the
    // values array → raw `i64.eq`, NOT a `__eq_<V>` helper (which is never
    // registered for it) nor `__aint_eq` (a ref helper).
    if registry.is_eligible_carrier(v_aver.trim()) {
        f.instruction(&Instruction::I64Eq);
        return Ok(());
    }
    match v_aver.trim() {
        // Flag-on (bignum): `$aint` ref → `__aint_eq`. Flag-off: `i64.eq`.
        "Int" => {
            super::lists::emit_aint_field_eq(f, registry)?;
        }
        "Bool" => {
            f.instruction(&Instruction::I32Eq);
        }
        "Float" => {
            f.instruction(&Instruction::F64Eq);
        }
        _ => {
            let h = v_helpers.ok_or(WasmGcError::Validation(format!(
                "emit_v_eq: V `{v_aver}` needs ref helpers (record/sum/carrier/list/vec/map)"
            )))?;
            f.instruction(&Instruction::Call(h.eq));
        }
    }
    Ok(())
}

/// Stack: `[v]` of Aver type `v_aver`. Push i32 hash. Primitive V →
/// inline DJB2-style mix; ref V → `Call(v_helpers.hash)`.
fn emit_v_hash(
    f: &mut Function,
    v_aver: &str,
    v_helpers: Option<KeyHelpers>,
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    // ETAP-2 carrier-`i64`: an eligible carrier V is a native `i64` →
    // raw `i32.wrap_i64`, matching its `i64.eq`.
    if registry.is_eligible_carrier(v_aver.trim()) {
        f.instruction(&Instruction::I32WrapI64);
        return Ok(());
    }
    match v_aver.trim() {
        // Flag-on (bignum): `$aint` ref → `__aint_hash`. Flag-off: wrap.
        "Int" => {
            super::lists::emit_aint_field_hash(f, registry)?;
        }
        "Bool" => {} // already i32
        "Float" => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        _ => {
            let h = v_helpers.ok_or(WasmGcError::Validation(format!(
                "emit_v_hash: V `{v_aver}` needs ref helpers"
            )))?;
            f.instruction(&Instruction::Call(h.hash));
        }
    }
    Ok(())
}

/// `__hash_Map<K,V>(m: eqref) -> i32`. XOR-fold per occupied entry of
/// `djb2(k) * 33 + djb2(v)`. XOR is commutative + associative → the
/// result is invariant to bucket / insertion order.
fn emit_map_hash(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
    v_helpers: Option<KeyHelpers>,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let map_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.map),
    });
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.keys_array),
    });
    let values_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.values_array),
    });
    // Locals: 1=typed map, 2=cap, 3=i, 4=keys, 5=values, 6=cur_key
    // (boxed), 7=h (i32 accumulator), 8=entry_h (i32 per-entry mix).
    let mut f = Function::new(vec![
        (1, map_ref),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, keys_ref),
        (1, values_ref),
        (1, key_storage_val_type(k_aver, registry)?),
        (1, ValType::I32),
        (1, ValType::I32),
    ]);
    let map_heap = HeapType::Concrete(slots.map);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(map_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    // entry_h = hash_K(unbox(cur_key)) * 33 + hash_V(values[i])
    f.instruction(&Instruction::LocalGet(6));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    // h_k * 32 (will add h_k below to become *33; OR more accurate:
    // shift-add for *33). Cheaper: do `(kh<<5) + kh + vh`.
    f.instruction(&Instruction::LocalGet(6));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    emit_v_hash(&mut f, v_aver, v_helpers, registry)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(8));
    // h ^= entry_h
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Xor);
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `remove(map, k) -> map`. Linear-probe locate the entry; if not
/// found, return the map unchanged. If found, do a backwards-shift
/// over the contiguous probe chain so subsequent `get` calls still
/// land their entries (Robin-Hood / canonical open-addressing
/// remove). Decrements `map.size`. Same-handle return (mutates in
/// place).
fn emit_map_remove(
    canonical: &str,
    registry: &TypeRegistry,
    keyh: KeyHelpers,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let _k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    let _ = v_val; // values array uses its own slot type
    // params: 0=map, 1=k.
    // locals: 2=cap, 3=mask, 4=keys, 5=values, 6=h, 7=i, 8=j,
    //         9=cur_key, 10=natural, 11=gap, 12=disp, 13=hole.
    let mut f = Function::new([
        (1, ValType::I32), // 2: cap
        (1, ValType::I32), // 3: mask
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.keys_array),
            }),
        ), // 4: keys
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ), // 5: values
        (1, ValType::I32), // 6: h
        (1, ValType::I32), // 7: i
        (1, ValType::I32), // 8: j
        (1, key_storage_val_type(k_aver, registry)?), // 9: cur_key (boxed for prim)
        (1, ValType::I32), // 10: natural
        (1, ValType::I32), // 11: gap
        (1, ValType::I32), // 12: disp
        (1, ValType::I32), // 13: hole (slot the removed key vacated)
    ]);

    // cap = map.cap; mask = cap - 1; keys = map.keys; values = map.values
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(5));

    // h = hash(k) & mask
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::LocalSet(7));

    // Find slot. probe loop.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(9));
    // if cur_key == null → not found, return map
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // if eq(unbox(cur_key), k) → break (found at i)
    f.instruction(&Instruction::LocalGet(9));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.eq));
    f.instruction(&Instruction::BrIf(1));
    // i = (i+1) & mask
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(7));
    // safety: if i wrapped to h → not found (full table miss)
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // Backwards-shift: hole = i; j = (i+1) & mask
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalSet(13));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // `j` back at the slot the removal emptied → the walk has gone all
    // the way round. It normally stops well before that: on a null slot,
    // or on an entry already sitting at its home bucket (`disp < gap`,
    // and `gap` is 1 on every iteration, so that test reads as
    // `disp == 0`). It would also stop without this guard — each shift
    // moves one entry one slot closer to home, so the sum of all
    // displacements, a non-negative integer, drops by exactly one per
    // iteration. But that is an argument about which tables are
    // reachable, and this file claims every probe loop is bounded by
    // construction, so the wrap is tested rather than reasoned about:
    // at most `cap - 1` shifts, whatever the buckets hold.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::LocalSet(9));
    // if next == null → break
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // natural = hash(unbox(next)) & mask
    f.instruction(&Instruction::LocalGet(9));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(10));
    // gap = (j - i) & mask
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(11));
    // disp = (j - natural) & mask
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(12));
    // if disp < gap → break
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::BrIf(1));
    // shift: keys[i] = next; values[i] = values[j]
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::ArraySet(slots.keys_array));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::ArraySet(slots.values_array));
    // i = j; j = (j+1) & mask
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // keys[i] = null. Heap type matches the keys array element ref —
    // see `key_storage_null_heap` for the per-K-kind table (primitive
    // box / String / record / carrier / List / Vector concrete idx,
    // nominal root for sum K).
    let null_heap = key_storage_null_heap(k_aver, registry);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::RefNull(null_heap));
    f.instruction(&Instruction::ArraySet(slots.keys_array));

    // map.size = map.size - 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.map,
        field_index: 0,
    });

    // return map
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `entries(m) -> List<Tuple<K, V>>`. Walk keys/values arrays
/// right-to-left; for each occupied slot (`keys[i] != null`) build
/// a `struct.new $tuple(k, v)` and prepend onto a cons-list
/// accumulator.
fn emit_map_entries(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let tup_canonical = format!("Tuple<{k_aver},{v_aver}>");
    let tup_idx = registry
        .tuple_type_idx(&tup_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.entries: `{tup_canonical}` not registered"
        )))?;
    let lt_canonical = format!("List<{tup_canonical}>");
    let lt_idx = registry
        .list_type_idx(&lt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.entries: `{lt_canonical}` not registered"
        )))?;
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.keys_array),
    });
    let values_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.values_array),
    });
    let lt_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(lt_idx),
    });
    // params: 0=map. locals: 1=keys, 2=values, 3=i, 4=acc.
    let mut f = Function::new([
        (1, keys_ref),
        (1, values_ref),
        (1, ValType::I32),
        (1, lt_ref),
    ]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(lt_idx)));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::BrIf(1));
    // if keys[i] != null: tup = struct.new $tuple(unbox(keys[i]),
    // values[i]); acc = cons(tup, acc)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.keys_array));
    emit_unbox_key(&mut f, k_aver, registry);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(slots.values_array));
    f.instruction(&Instruction::StructNew(tup_idx));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::StructNew(lt_idx));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `from_list(l) -> Map<K, V>`. Walks `l` from head to tail,
/// struct.get's the (K, V) from each tuple, calls the per-(K, V)
/// `set` helper to insert. Allocates a fresh empty map (via the
/// per-(K, V) `empty` shape inlined: cap = INITIAL_CAP, fresh keys
/// and values arrays) and returns it.
fn emit_map_from_list(
    canonical: &str,
    registry: &TypeRegistry,
    set_fn: u32,
) -> Result<Function, WasmGcError> {
    let slots = slots_for(canonical, registry)?;
    let (k_aver, v_aver) = super::types::parse_map_kv(canonical).unwrap();
    let tup_canonical = format!("Tuple<{k_aver},{v_aver}>");
    let tup_idx = registry
        .tuple_type_idx(&tup_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.fromList: `{tup_canonical}` not registered"
        )))?;
    let lt_canonical = format!("List<{tup_canonical}>");
    let lt_idx = registry
        .list_type_idx(&lt_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "Map.fromList: `{lt_canonical}` not registered"
        )))?;
    let map_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(slots.map),
    });
    let lt_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(lt_idx),
    });
    // params: 0=l. locals: 1=cur, 2=map, 3=tup.
    let mut f = Function::new([
        (1, lt_ref),
        (1, map_ref),
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(tup_idx),
            }),
        ),
    ]);
    // map = inline empty allocation (matches emit_map_empty)
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(slots.keys_array));
    f.instruction(&Instruction::I32Const(INITIAL_CAP));
    f.instruction(&Instruction::ArrayNewDefault(slots.values_array));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::LocalSet(2));
    // cur = l
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // tup = cur.head
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: lt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalSet(3));
    // map = set(map, tup.0, tup.1)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: tup_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: tup_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::Call(set_fn));
    f.instruction(&Instruction::LocalSet(2));
    // cur = cur.tail
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: lt_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `hash : (parent_ref) -> i32` for a user sum type. Per-variant
/// `ref.test` cascade: each constructor V_i has a tag (its
/// alphabetical index in the variant list, baked at compile time)
/// folded in DJB2-style with each V_i field's hash. Variants are
/// sorted by name for stable emit. Field-type dispatch covers
/// `{Int, Float, Bool, String}`; other field types surface as
/// Unimplemented.
fn emit_hash_sum(
    parent_name: &str,
    registry: &TypeRegistry,
    string_key_helpers: Option<KeyHelpers>,
    all_key_helpers: &HashMap<String, KeyHelpers>,
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(n, vs)| vs.iter().map(move |v| (n.clone(), v.clone())))
        .filter(|(_, v)| v.parent == parent_name)
        .collect();
    variants.sort_by(|a, b| a.0.cmp(&b.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "hash_sum: `{parent_name}` has no variants"
        )));
    }
    let mut f = Function::new([(1, ValType::I32) /* h */]);
    f.instruction(&Instruction::Block(BlockType::Empty));
    for (tag, (_v_name, info)) in variants.iter().enumerate() {
        let v_idx = info.type_idx;
        let v_heap = wasm_encoder::HeapType::Concrete(v_idx);
        // if ref.test V head: h = (5381*33+tag), then fold fields, return
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(BlockType::Empty));
        // Initial h = 5381 * 33 + tag (variant discriminator).
        f.instruction(&Instruction::I32Const(5381 * 33 + tag as i32));
        f.instruction(&Instruction::LocalSet(1));
        for (i, field_ty) in info.fields.iter().enumerate() {
            // h = h * 33
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Const(5));
            f.instruction(&Instruction::I32Shl);
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Add);
            // push field_value as i32 hash
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::RefCastNonNull(v_heap));
            f.instruction(&Instruction::StructGet {
                struct_type_index: v_idx,
                field_index: i as u32,
            });
            let field_ty_trim = field_ty.trim();
            match field_ty_trim {
                // Flag-on (bignum): `$aint` ref → `__aint_hash`. Flag-off: wrap.
                "Int" => {
                    super::lists::emit_aint_field_hash(&mut f, registry)?;
                }
                "Bool" => {}
                "Float" => {
                    f.instruction(&Instruction::I64ReinterpretF64);
                    f.instruction(&Instruction::I32WrapI64);
                }
                "String" => {
                    let helpers = string_key_helpers.ok_or(WasmGcError::Validation(
                        "hash_sum: String field needs String key helpers".into(),
                    ))?;
                    f.instruction(&Instruction::Call(helpers.hash));
                }
                _ => {
                    // Compound field — proxy via Call to the per-type
                    // hash helper assembled in `all_key_helpers`
                    // (records / sums / Option / Result / Tuple /
                    // List<T> / Vector<T> / Map<K,V>). Compound names
                    // get whitespace stripped so the lookup matches
                    // the canonical form used to register helpers.
                    let lookup_key = super::types::normalize_compound(field_ty_trim);
                    let helpers = all_key_helpers
                        .get(&lookup_key)
                        .or_else(|| all_key_helpers.get(field_ty_trim))
                        .ok_or_else(|| {
                            WasmGcError::Validation(format!(
                                "hash_sum: no helper registered for sum-variant field \
                                 type `{field_ty_trim}` of `{parent_name}`"
                            ))
                        })?;
                    f.instruction(&Instruction::Call(helpers.hash));
                }
            }
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(1));
        }
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::Return);
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::End);
    // Defensive 0 (exhaustiveness should make this unreachable).
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `eq : (parent_ref, parent_ref) -> i32` for a user sum type.
/// Per-variant `ref.test` cascade: head and needle must share a
/// constructor, then field-by-field eq with `i32.and` fold.
fn emit_eq_sum(
    parent_name: &str,
    registry: &TypeRegistry,
    string_key_helpers: Option<KeyHelpers>,
    all_key_helpers: &HashMap<String, KeyHelpers>,
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(n, vs)| vs.iter().map(move |v| (n.clone(), v.clone())))
        .filter(|(_, v)| v.parent == parent_name)
        .collect();
    variants.sort_by(|a, b| a.0.cmp(&b.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "eq_sum: `{parent_name}` has no variants"
        )));
    }
    // params: 0=head, 1=needle. No locals.
    let mut f = Function::new([]);
    f.instruction(&Instruction::Block(BlockType::Result(ValType::I32)));
    for (_v_name, info) in &variants {
        let v_idx = info.type_idx;
        let v_heap = wasm_encoder::HeapType::Concrete(v_idx);
        // if ref.test V head:
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(BlockType::Empty));
        // if ref.test V needle:
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(BlockType::Empty));
        if info.fields.is_empty() {
            f.instruction(&Instruction::I32Const(1));
        } else {
            for (i, field_ty) in info.fields.iter().enumerate() {
                f.instruction(&Instruction::LocalGet(0));
                f.instruction(&Instruction::RefCastNonNull(v_heap));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: v_idx,
                    field_index: i as u32,
                });
                f.instruction(&Instruction::LocalGet(1));
                f.instruction(&Instruction::RefCastNonNull(v_heap));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: v_idx,
                    field_index: i as u32,
                });
                let field_ty_trim = field_ty.trim();
                match field_ty_trim {
                    // Flag-on (bignum): `$aint` ref → `__aint_eq`. Flag-off: `i64.eq`.
                    "Int" => {
                        super::lists::emit_aint_field_eq(&mut f, registry)?;
                    }
                    "Bool" => {
                        f.instruction(&Instruction::I32Eq);
                    }
                    "Float" => {
                        f.instruction(&Instruction::F64Eq);
                    }
                    "String" => {
                        let helpers = string_key_helpers.ok_or(WasmGcError::Validation(
                            "eq_sum: String field needs String key helpers".into(),
                        ))?;
                        f.instruction(&Instruction::Call(helpers.eq));
                    }
                    _ => {
                        // Compound field — proxy to per-type eq helper.
                        let lookup_key = super::types::normalize_compound(field_ty_trim);
                        let helpers = all_key_helpers
                            .get(&lookup_key)
                            .or_else(|| all_key_helpers.get(field_ty_trim))
                            .ok_or_else(|| {
                                WasmGcError::Validation(format!(
                                    "eq_sum: no helper registered for sum-variant field \
                                     type `{field_ty_trim}` of `{parent_name}`"
                                ))
                            })?;
                        f.instruction(&Instruction::Call(helpers.eq));
                    }
                };
                if i > 0 {
                    f.instruction(&Instruction::I32And);
                }
            }
        }
        f.instruction(&Instruction::Br(2));
        f.instruction(&Instruction::Else);
        // head V, needle != V → 0
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Br(2));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
    }
    // Defensive — no variant matched head.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}
