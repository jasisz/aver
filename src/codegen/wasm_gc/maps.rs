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

use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, Function, HeapType, Instruction, RefType, ValType,
};

use super::WasmGcError;
use super::types::{MapSlots, TypeRegistry};

/// Initial bucket count — power of two so masking with `cap-1`
/// instead of `i32.rem_u` works. Sized for the bench scenarios.
const INITIAL_CAP: i32 = 16384;

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
    pub(super) set: u32,
    pub(super) get: u32,
    pub(super) len: u32,
    /// `get_or_default(m, k, default) -> V`. Fused shape that backs
    /// `Option.withDefault(Map.get(m, k), default)` without ever
    /// allocating an `Option<V>`. Same probe loop as `get` but
    /// returns `values[idx]` directly on a key match and the supplied
    /// default on an empty slot.
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
    /// (K, V) from each tuple, calls the per-(K, V) `set` helper.
    pub(super) from_list: u32,
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
    /// Eleven slots per (K, V): empty, set, get, len, get_or_default,
    /// get_pair, keys, values, remove, entries, from_list. Order
    /// matches `assign_slots` / `emit_function_section` /
    /// `emit_helper_bodies` exactly.
    kv_type_indices:
        HashMap<String, (u32, u32, u32, u32, u32, u32, u32, u32, u32, u32, u32)>,
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
        let mut k_seen: std::collections::HashSet<String> =
            std::collections::HashSet::new();
        for canonical in map_canonicals {
            let (k_aver, _) = super::types::parse_map_kv(canonical).ok_or(
                WasmGcError::Validation(format!(
                    "MapHelperRegistry: cannot parse K, V from `{canonical}`"
                )),
            )?;
            // If K is a record / sum whose fields include `String`,
            // ensure String hash/eq is registered first.
            let mut needs_string = false;
            if let Some(fs) = registry.record_fields.get(k_aver) {
                needs_string |= fs.iter().any(|(_, t)| t.trim() == "String");
            }
            if registry.variants.values().any(|v| v.parent == k_aver) {
                needs_string |= registry
                    .variants
                    .values()
                    .filter(|v| v.parent == k_aver)
                    .any(|v| v.fields.iter().any(|t| t.trim() == "String"));
            }
            if needs_string && k_seen.insert("String".into()) {
                k_names.push("String".into());
            }
            if k_seen.insert(k_aver.to_string()) {
                k_names.push(k_aver.to_string());
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
                    || registry.variants.values().any(|v| v.parent == *n.as_str())
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
                .filter(|v| v.parent == parent)
            {
                for t in &variant.fields {
                    field_types.push(t.trim().to_string());
                }
            }
            for ft in field_types {
                let is_record = registry.record_type_idx(&ft).is_some();
                let is_sum = registry.variants.values().any(|v| v.parent == ft);
                if (is_record || is_sum) && k_seen.insert(ft.clone()) {
                    k_names.push(ft.clone());
                    to_visit.push(ft.clone());
                    // String inside the nested type's fields →
                    // force-register String.
                    let mut nested_needs_string = false;
                    if let Some(fs) = registry.record_fields.get(&ft) {
                        nested_needs_string |=
                            fs.iter().any(|(_, t)| t.trim() == "String");
                    }
                    if is_sum {
                        nested_needs_string |= registry
                            .variants
                            .values()
                            .filter(|v| v.parent == ft)
                            .any(|v| v.fields.iter().any(|t| t.trim() == "String"));
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
                    KeyHelpers { hash: hash_fn, eq: eq_fn },
                );
                self.key_type_indices.insert(
                    k_aver.clone(),
                    (hash_type_idx, eq_type_idx),
                );
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
            let empty_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let set_fn = *next_wasm_fn_idx;
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

            // K can be String, a user-defined record (field-by-field
            // hash + eq), or a primitive (Int / Float / Bool). Primitive
            // keys are boxed into a per-K struct ref so the open-
            // addressing `keys[i] == null` empty marker still holds.
            let (k_aver, _) = super::types::parse_map_kv(canonical).ok_or(
                WasmGcError::Validation(format!("bad map canonical `{canonical}`")),
            )?;
            let is_primitive_k = super::types::TypeRegistry::is_primitive_map_key(k_aver);
            let is_sum_k = registry.variants.values().any(|v| v.parent == k_aver);
            if k_aver != "String"
                && registry.record_type_idx(k_aver).is_none()
                && !is_primitive_k
                && !is_sum_k
            {
                return Err(WasmGcError::Unimplemented(
                    "phase 3c — Map<K, V> with K not String / user-record / sum / primitive",
                ));
            }

            self.kv.insert(
                canonical.clone(),
                MapKVHelpers {
                    empty: empty_fn,
                    set: set_fn,
                    get: get_fn,
                    len: len_fn,
                    get_or_default: god_fn,
                    get_pair: pair_fn,
                    keys: keys_fn,
                    values: values_fn,
                    remove: remove_fn,
                    entries: entries_fn,
                    from_list: from_list_fn,
                },
            );
            self.kv_type_indices.insert(
                canonical.clone(),
                (
                    empty_type_idx,
                    set_type_idx,
                    get_type_idx,
                    len_type_idx,
                    god_type_idx,
                    pair_type_idx,
                    keys_type_idx,
                    values_type_idx,
                    remove_type_idx,
                    entries_type_idx,
                    from_list_type_idx,
                ),
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
            let slots = registry.map_slots(canonical).ok_or(WasmGcError::Validation(
                format!("Map slots missing for `{canonical}`"),
            ))?;
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
            // set : (Map, K, V) -> Map
            types.ty().function([map_ref, k_val, v_val], [map_ref]);
            // get : (Map, K) -> Option<V>
            types.ty().function([map_ref, k_val], [opt_ref]);
            // len : (Map) -> i64
            types.ty().function([map_ref], [ValType::I64]);
            // get_or_default : (Map, K, V) -> V
            types.ty().function([map_ref, k_val, v_val], [v_val]);
            // get_pair : (Map, K) -> (i32 found, V value) — multi-result
            types
                .ty()
                .function([map_ref, k_val], [ValType::I32, v_val]);
            // keys : (Map) -> List<K>
            let list_k_idx =
                registry
                    .list_type_idx(&format!("List<{k_aver}>"))
                    .ok_or(WasmGcError::Validation(format!(
                        "Map.keys: List<{k_aver}> not registered"
                    )))?;
            let list_k_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_k_idx),
            });
            types.ty().function([map_ref], [list_k_ref]);
            // values : (Map) -> List<V>
            let list_v_idx =
                registry
                    .list_type_idx(&format!("List<{v_aver}>"))
                    .ok_or(WasmGcError::Validation(format!(
                        "Map.values: List<{v_aver}> not registered"
                    )))?;
            let list_v_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_v_idx),
            });
            types.ty().function([map_ref], [list_v_ref]);
            // remove : (Map, K) -> Map
            types.ty().function([map_ref, k_val], [map_ref]);
            // entries : (Map) -> List<Tuple<K, V>>
            let tup_canonical = format!("Tuple<{k_aver},{v_aver}>");
            let tup_idx = registry
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
            types.ty().function([map_ref], [lt_ref.clone()]);
            // from_list : (List<Tuple<K, V>>) -> Map
            types.ty().function([lt_ref], [map_ref]);
            let _ = opt_ref;
            let _ = tup_idx;
        }
        Ok(())
    }

    /// Emit one `funcs.function(<type_idx>)` entry per registered
    /// helper, in the same order as `emit_helper_types`.
    pub(super) fn emit_function_section(
        &self,
        funcs: &mut wasm_encoder::FunctionSection,
    ) {
        for k in &self.key_order {
            let (h, e) = self.key_type_indices[k];
            funcs.function(h);
            funcs.function(e);
        }
        for canonical in &self.kv_order {
            let (em, st, gt, ln, god, pair, ks, vs, rm, en, fl) =
                self.kv_type_indices[canonical];
            funcs.function(em);
            funcs.function(st);
            funcs.function(gt);
            funcs.function(ln);
            funcs.function(god);
            funcs.function(pair);
            funcs.function(ks);
            funcs.function(vs);
            funcs.function(rm);
            funcs.function(en);
            funcs.function(fl);
        }
    }

    /// Emit code bodies for every registered helper, in the same
    /// order as `emit_helper_types`.
    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
        list_eq_hash: &HashMap<String, (u32, u32)>,
    ) -> Result<(), WasmGcError> {
        let string_key_helpers = self.key.get("String").copied();
        // Snapshot every K's helpers — record hash/eq dispatch
        // needs to call helpers for nested record fields. Plus
        // virtual entries for `List<T>` field types so hash/eq
        // dispatch can call into list_helpers without a
        // separate cross-module lookup.
        let mut all_key_helpers: HashMap<String, KeyHelpers> = self
            .key
            .iter()
            .map(|(k, h)| (k.clone(), *h))
            .collect();
        for (list_canonical, &(eq_fn, hash_fn)) in list_eq_hash {
            all_key_helpers.insert(
                list_canonical.clone(),
                KeyHelpers {
                    hash: hash_fn,
                    eq: eq_fn,
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
        }
        Ok(())
    }
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
    if registry.record_type_idx(k_aver).is_some() {
        return emit_hash_record(k_aver, registry, string_key_helpers, all_key_helpers);
    }
    if super::types::TypeRegistry::is_primitive_map_key(k_aver) {
        return emit_hash_primitive(k_aver);
    }
    if registry.variants.values().any(|v| v.parent == k_aver) {
        return emit_hash_sum(k_aver, registry, string_key_helpers);
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
    if registry.record_type_idx(k_aver).is_some() {
        return emit_eq_record(k_aver, registry, string_key_helpers, all_key_helpers);
    }
    if super::types::TypeRegistry::is_primitive_map_key(k_aver) {
        return emit_eq_primitive(k_aver);
    }
    if registry.variants.values().any(|v| v.parent == k_aver) {
        return emit_eq_sum(k_aver, registry, string_key_helpers);
    }
    Err(WasmGcError::Unimplemented(
        "phase 3c — eq for unsupported K kind",
    ))
}

/// `hash : (K_raw) -> i32` for primitive K. Helpers consume raw
/// primitives (callers don't have to box just to compute a hash).
/// Map's keys array stores boxed refs, but `hash` runs on the raw
/// value the user passed in.
fn emit_hash_primitive(k_aver: &str) -> Result<Function, WasmGcError> {
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    match k_aver {
        "Int" => {
            // i32.wrap_i64 — keeps low 32 bits. Cheap, distributes
            // poorly for tightly-clustered Int domains; bench
            // scenarios don't stress this.
            f.instruction(&Instruction::I32WrapI64);
        }
        "Float" => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        "Bool" => {
            // Already i32 — no-op (just LocalGet then End)
        }
        _ => unreachable!("emit_hash_primitive: K = `{k_aver}` not primitive"),
    }
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `eq : (K_raw, K_raw) -> i32` for primitive K. Native eq
/// instruction per K kind.
fn emit_eq_primitive(k_aver: &str) -> Result<Function, WasmGcError> {
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    match k_aver {
        "Int" => f.instruction(&Instruction::I64Eq),
        "Float" => f.instruction(&Instruction::F64Eq),
        "Bool" => f.instruction(&Instruction::I32Eq),
        _ => unreachable!("emit_eq_primitive: K = `{k_aver}` not primitive"),
    };
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Wasm value type used as the `keys` array element. Primitive K
/// stores boxed refs (`(ref null $primitive_key_box_K)`) so the
/// open-addressing `keys[i] == null` empty marker stays uniform;
/// ref K (String / record) stores its own ref directly.
fn key_storage_val_type(
    k_aver: &str,
    registry: &TypeRegistry,
) -> Result<ValType, WasmGcError> {
    if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
        Ok(ValType::Ref(RefType {
            nullable: true,
            heap_type: HeapType::Concrete(box_idx),
        }))
    } else {
        super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Map key type `{k_aver}` has no wasm representation"
            )),
        )
    }
}

/// Append the instructions that turn a stored-key value (top of
/// stack) into the raw K_val that `hash` / `eq` expect. For primitive
/// K: `struct.get $box 0` to unbox; for ref K: no-op.
fn emit_unbox_key(
    f: &mut Function,
    k_aver: &str,
    registry: &TypeRegistry,
) {
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
fn emit_box_key(
    f: &mut Function,
    k_aver: &str,
    registry: &TypeRegistry,
) {
    if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
        f.instruction(&Instruction::StructNew(box_idx));
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
    let mut f = Function::new([
        (1, ValType::I32), // local 1: h
        (1, ValType::I32), // local 2: i
        (1, ValType::I32), // local 3: n
    ]);
    // h = 5381
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(1));
    // n = arr.len
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(3));
    // i = 0
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    // loop
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // if i >= n break
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1)); // break out of block
    // h = h * 33 + s[i]   (h * 33 = (h << 5) + h)
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(1));
    // i++
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Byte-equal compare of two `(ref null $string)`. Returns 1 if equal.
fn emit_eq_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let s_idx = string_idx(registry)?;
    let mut f = Function::new([
        (1, ValType::I32), // local 2: i
        (1, ValType::I32), // local 3: n
    ]);
    // If lens differ → 0
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // n = a.len
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(3));
    // i = 0
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    // loop: while i < n: if a[i]!=b[i] return 0; i++
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End);
    Ok(f)
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
    // locals: 3=cap, 4=mask, 5=idx, 6=keys, 7=values, 8=cur_key
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
        ), // 6: keys
        (
            1,
            ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(slots.values_array),
            }),
        ), // 7: values
        (1, key_storage_val_type(k_aver, registry)?), // 8: cur_key (boxed for primitive)
    ]);
    let _ = (v_val, k_val);

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

    // idx = hash(k) & mask
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));

    // loop forever (cap is large enough that probe always finds slot)
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
    // map.size += 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::StructSet {
        struct_type_index: slots.map,
        field_index: 0,
    });
    // return map
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // else if eq(unbox(cur_key), k): update.
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
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // else: idx = (idx + 1) & mask; continue
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));
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
    let opt_idx = registry.option_type_idx(&opt_canonical).ok_or(
        WasmGcError::Validation(format!(
            "Map.get: Option<{v_aver}> not registered"
        )),
    )?;

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
    // idx = hash(k) & mask
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(4));

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
    // locals: 3=cap, 4=mask, 5=idx, 6=keys, 7=values, 8=cur_key
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

    // idx = hash(k) & mask
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(keyh.hash));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(5));

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
    // locals: 2=cap, 3=mask, 4=idx, 5=keys, 6=values, 7=cur_key
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
            f.instruction(&Instruction::F32Const(0.0.into()));
        }
        ValType::F64 => {
            f.instruction(&Instruction::F64Const(0.0.into()));
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
    let record_idx = registry.record_type_idx(record_name).ok_or(
        WasmGcError::Validation(format!("hash_record: `{record_name}` not registered")),
    )?;
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
    for (i, (_field_name, field_ty)) in fields.iter().enumerate() {
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
                // i64 → low 32 bits
                f.instruction(&Instruction::I32WrapI64);
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
                    "hash_record: String field needs String key helpers"
                        .into(),
                ))?;
                f.instruction(&Instruction::Call(helpers.hash));
            }
            other => {
                // Nested record / List<T> field. Both dispatch via
                // `all_key_helpers` — records were force-registered
                // as pseudo-K in `assign_slots`; list canonicals were
                // injected by `emit_helper_bodies` from list_helpers.
                let lookup_key = if other.starts_with("List<")
                    || other.starts_with("Vector<")
                {
                    super::types::normalize_compound(other).to_string()
                } else {
                    other.to_string()
                };
                let is_compound =
                    other.starts_with("List<") || other.starts_with("Vector<");
                let is_sum =
                    registry.variants.values().any(|v| v.parent == other);
                if registry.record_type_idx(other).is_some()
                    || is_compound
                    || is_sum
                {
                    let inner = all_key_helpers.get(&lookup_key).ok_or(
                        WasmGcError::Validation(format!(
                            "hash_record: field `{other}` has no key helpers \
                             (record / list / vector / sum T may need force-registration)"
                        )),
                    )?;
                    f.instruction(&Instruction::Call(inner.hash));
                } else {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3c — record-key field type not in \
                         {Int, Float, Bool, String, nested record, List<T>, Vector<T>, sum}",
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
    let record_idx = registry.record_type_idx(record_name).ok_or(
        WasmGcError::Validation(format!("eq_record: `{record_name}` not registered")),
    )?;
    let fields = registry
        .record_fields
        .get(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "eq_record: `{record_name}` has no field info"
        )))?;
    let mut f = Function::new([]);
    for (i, (_field_name, field_ty)) in fields.iter().enumerate() {
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
            "Int" => f.instruction(&Instruction::I64Eq),
            "Bool" => f.instruction(&Instruction::I32Eq),
            "Float" => f.instruction(&Instruction::F64Eq),
            "String" => {
                let helpers = string_key_helpers.ok_or(WasmGcError::Validation(
                    "eq_record: String field needs String key helpers"
                        .into(),
                ))?;
                f.instruction(&Instruction::Call(helpers.eq))
            }
            other => {
                let lookup_key = if other.starts_with("List<")
                    || other.starts_with("Vector<")
                {
                    super::types::normalize_compound(other).to_string()
                } else {
                    other.to_string()
                };
                let is_compound =
                    other.starts_with("List<") || other.starts_with("Vector<");
                let is_sum =
                    registry.variants.values().any(|v| v.parent == other);
                if registry.record_type_idx(other).is_some()
                    || is_compound
                    || is_sum
                {
                    let inner = all_key_helpers.get(&lookup_key).ok_or(
                        WasmGcError::Validation(format!(
                            "eq_record: field `{other}` has no key helpers"
                        )),
                    )?;
                    f.instruction(&Instruction::Call(inner.eq))
                } else {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3c — record-key field type not in \
                         {Int, Float, Bool, String, nested record, List<T>, Vector<T>, sum}",
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
/// each non-null key onto a cons-list accumulator. Returns hash-
/// bucket order (not insertion order) — same constraint Aver's
/// stdlib documents for `Map.keys`.
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
fn emit_map_values(
    canonical: &str,
    registry: &TypeRegistry,
) -> Result<Function, WasmGcError> {
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
    let mut f = Function::new([
        (1, keys_ref.clone()),
        (1, ValType::I32),
        (1, list_ref.clone()),
    ]);
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
    let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.unwrap();
    let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.unwrap();
    let _ = v_val; // values array uses its own slot type
    // params: 0=map, 1=k.
    // locals: 2=cap, 3=mask, 4=keys, 5=values, 6=h, 7=i, 8=j,
    //         9=cur_key, 10=natural, 11=gap, 12=disp.
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

    // Backwards-shift: j = (i+1) & mask
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
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

    // keys[i] = null. Heap type matches the keys array element ref:
    // primitive-key box for primitive K, String slot for K=String,
    // record slot for K=record.
    let null_heap_idx = registry
        .primitive_key_box_idx(k_aver)
        .or(registry.string_array_type_idx.filter(|_| k_aver == "String"))
        .or_else(|| registry.record_type_idx(k_aver))
        .unwrap_or(0);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(null_heap_idx)));
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
    let tup_idx = registry.tuple_type_idx(&tup_canonical).ok_or(
        WasmGcError::Validation(format!("Map.entries: `{tup_canonical}` not registered")),
    )?;
    let lt_canonical = format!("List<{tup_canonical}>");
    let lt_idx = registry.list_type_idx(&lt_canonical).ok_or(
        WasmGcError::Validation(format!("Map.entries: `{lt_canonical}` not registered")),
    )?;
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
    let tup_idx = registry.tuple_type_idx(&tup_canonical).ok_or(
        WasmGcError::Validation(format!("Map.fromList: `{tup_canonical}` not registered")),
    )?;
    let lt_canonical = format!("List<{tup_canonical}>");
    let lt_idx = registry.list_type_idx(&lt_canonical).ok_or(
        WasmGcError::Validation(format!("Map.fromList: `{lt_canonical}` not registered")),
    )?;
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
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::types::VariantInfo)> = registry
        .variants
        .iter()
        .filter(|(_, v)| v.parent == parent_name)
        .map(|(n, v)| (n.clone(), v.clone()))
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
            match field_ty.trim() {
                "Int" => {
                    f.instruction(&Instruction::I32WrapI64);
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
                    return Err(WasmGcError::Unimplemented(
                        "phase 3c — sum-variant field type not in {Int, Float, Bool, String}",
                    ));
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
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::types::VariantInfo)> = registry
        .variants
        .iter()
        .filter(|(_, v)| v.parent == parent_name)
        .map(|(n, v)| (n.clone(), v.clone()))
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
                match field_ty.trim() {
                    "Int" => f.instruction(&Instruction::I64Eq),
                    "Bool" => f.instruction(&Instruction::I32Eq),
                    "Float" => f.instruction(&Instruction::F64Eq),
                    "String" => {
                        let helpers = string_key_helpers.ok_or(WasmGcError::Validation(
                            "eq_sum: String field needs String key helpers".into(),
                        ))?;
                        f.instruction(&Instruction::Call(helpers.eq))
                    }
                    _ => {
                        return Err(WasmGcError::Unimplemented(
                            "phase 3c — sum-variant field type not in {Int, Float, Bool, String}",
                        ));
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
