//! `List<T>` and `Vector<T>` helper bodies — per-instantiation
//! list primitives plus a couple of cross-shape helpers.
//!
//! Mirrors the strategy in `maps.rs`: monomorphise per `T`, generate
//! one wasm fn per `(operation, T)` slot at module-emit time, look the
//! fn up at call sites by canonical name. The `wasm-opt -Oz` pipeline
//! strips helpers that no call site reaches.
//!
//! Helpers covered:
//!
//! - **per-`List<T>`** — `len`, `reverse`. Used by `List.len(list)` and
//!   `List.reverse(list)` surface calls.
//! - **per-`(List<T>, Vector<T>)` pair** — `from_list`. Two-pass walk
//!   (count + fill) over the cons chain. Used by `Vector.fromList(list)`.
//! - **global (T=String)** — `string_split(s, sep) -> List<String>`,
//!   `string_join(list, sep) -> String`. The bench scenarios only need
//!   `T = String`; per-T versions can be added by following the same
//!   pattern when (and if) other `T` shows up.
//!
//! Per-instance helpers consult `TypeRegistry` for the matching wasm
//! type idx (`list_type_idx`, `vector_type_idx`, `string_array_type_idx`)
//! when emitting their bodies.

use std::collections::HashMap;

use wasm_encoder::{BlockType, CodeSection, Function, HeapType, Instruction, RefType, ValType};

use super::WasmGcError;
use super::types::TypeRegistry;

#[derive(Debug, Clone, Copy)]
pub(super) struct ListOps {
    pub(super) len: u32,
    pub(super) reverse: u32,
    pub(super) concat: u32,
    pub(super) take: u32,
    pub(super) drop: u32,
    /// Per-T equality probe. `None` for T types we can't compare
    /// natively (records, sums, nested generics) — call sites surface a
    /// clear error in that case rather than silently emit garbage.
    pub(super) contains: Option<u32>,
    /// `cons : (T, list_T) -> list_T` — the canonical "prepend new
    /// head onto an existing tail" primitive. Surfaces as a fn call
    /// from `emit_list_literal` so nested list literals don't need
    /// any scratch local (which would race with the outer literal's
    /// own accumulator). Body is a single `struct.new $list_T`.
    pub(super) cons: u32,
    /// `eq : (list_T, list_T) -> i32`. Walks both cons chains in
    /// lockstep — equal heads + equal tails. Same `None`-when-T-
    /// isn't-eq-able rule as `contains` (records / sums with non-
    /// trivial fields skip the slot).
    pub(super) eq: Option<u32>,
    /// `hash : (list_T) -> i32`. Folds element hashes with DJB2
    /// `h * 33 + element_hash`. Same `None` rule as `eq`.
    pub(super) hash: Option<u32>,
}

#[derive(Debug, Clone, Copy, Default)]
pub(super) struct ListTypeIdx {
    pub(super) len: u32,
    pub(super) reverse: u32,
    pub(super) concat: u32,
    pub(super) take: u32,
    pub(super) drop: u32,
    pub(super) contains: Option<u32>,
    pub(super) cons: u32,
    pub(super) eq: Option<u32>,
    pub(super) hash: Option<u32>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct VectorFromListOps {
    pub(super) from_list: u32,
    /// `to_list : (Vector<T>) -> List<T>`. Walks the array right-to-
    /// left building a cons-list — same shape as `from_list` but in
    /// reverse. Slotted alongside `from_list` so any pair of
    /// `(List<T>, Vector<T>)` registers both helpers together.
    pub(super) to_list: u32,
    /// `eq : (Vector<T>, Vector<T>) -> i32`. Length-match + per-T
    /// element eq. None when T isn't `list_eq_kind`-able.
    pub(super) eq: Option<u32>,
    /// `hash : (Vector<T>) -> i32`. DJB2 fold over array elements.
    pub(super) hash: Option<u32>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct StringSplitOps {
    pub(super) split: u32,
    pub(super) join: u32,
}

#[derive(Default)]
pub(super) struct ListHelperRegistry {
    /// `List<T>` canonical → its method indices.
    list_ops: HashMap<String, ListOps>,
    list_order: Vec<String>,
    /// Per-T type indices for the helpers in `ListOps`. The
    /// `contains` slot is `None` exactly when `ListOps::contains` is
    /// `None` (T isn't natively eq-able).
    list_type_indices: HashMap<String, ListTypeIdx>,

    /// `List<T>` canonical → vec-from-list / vec-to-list fn indices
    /// (paired with the `Vector<T>` of the same `T` discovered in the
    /// registry).
    vfl_ops: HashMap<String, VectorFromListOps>,
    vfl_order: Vec<String>,
    /// Per-pair: `(from_list_type_idx, to_list_type_idx)`.
    vfl_type_indices: HashMap<String, (u32, u32, Option<u32>, Option<u32>)>,

    /// `Tuple<A,B>` canonical → `List.zip` fn idx. Registered when
    /// the program has `List<A>`, `List<B>`, and `List<Tuple<A,B>>`
    /// all reachable.
    zip_ops: HashMap<String, u32>,
    zip_order: Vec<String>,
    zip_type_indices: HashMap<String, u32>,

    string_split: Option<StringSplitOps>,
    /// (split_type_idx, join_type_idx)
    string_split_type_indices: Option<(u32, u32)>,
}

impl ListHelperRegistry {
    #[allow(clippy::too_many_arguments)]
    pub(super) fn assign_slots(
        &mut self,
        list_canonicals: &[String],
        vector_canonicals: &[String],
        tuple_canonicals: &[String],
        register_string_split_join: bool,
        registry: &TypeRegistry,
        next_wasm_fn_idx: &mut u32,
        next_type_idx: &mut u32,
    ) -> Result<(), WasmGcError> {
        // Per-`List<T>` helpers. `len + reverse` always; `concat +
        // take + drop` always (they're T-agnostic over the cons-cell
        // shape); `contains` only when `T` is natively eq-able.
        for canonical in list_canonicals {
            if self.list_ops.contains_key(canonical) {
                continue;
            }
            let len_type = *next_type_idx;
            *next_type_idx += 1;
            let rev_type = *next_type_idx;
            *next_type_idx += 1;
            let concat_type = *next_type_idx;
            *next_type_idx += 1;
            let take_type = *next_type_idx;
            *next_type_idx += 1;
            let drop_type = *next_type_idx;
            *next_type_idx += 1;
            let cons_type = *next_type_idx;
            *next_type_idx += 1;
            let elem =
                TypeRegistry::list_element_type(canonical).ok_or(WasmGcError::Validation(
                    format!("list canonical `{canonical}` has no parsable element type"),
                ))?;
            let contains_eq = list_eq_kind(elem.trim(), registry);
            let contains_type = if contains_eq.is_some() {
                let t = *next_type_idx;
                *next_type_idx += 1;
                Some(t)
            } else {
                None
            };
            // List eq + hash slots track the same kinds contains
            // does — primitives, String, and (since 0.16.3) record
            // / sum nominal elements that resolve through the
            // per-type `__eq_<X>` helper map. The body emitters
            // (`emit_list_eq` / `emit_list_hash`) dispatch nominal
            // elements through `Call(__eq_<name>)` / inline record
            // /sum hash now.
            let needs_list_helpers = contains_eq.is_some();
            let list_eq_type = if needs_list_helpers {
                let t = *next_type_idx;
                *next_type_idx += 1;
                Some(t)
            } else {
                None
            };
            let list_hash_type = if needs_list_helpers {
                let t = *next_type_idx;
                *next_type_idx += 1;
                Some(t)
            } else {
                None
            };
            let len_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let rev_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let concat_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let take_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let drop_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let cons_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let contains_fn = if contains_type.is_some() {
                let f = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                Some(f)
            } else {
                None
            };
            let list_eq_fn = if list_eq_type.is_some() {
                let f = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                Some(f)
            } else {
                None
            };
            let list_hash_fn = if list_hash_type.is_some() {
                let f = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                Some(f)
            } else {
                None
            };
            self.list_ops.insert(
                canonical.clone(),
                ListOps {
                    len: len_fn,
                    reverse: rev_fn,
                    concat: concat_fn,
                    take: take_fn,
                    drop: drop_fn,
                    contains: contains_fn,
                    cons: cons_fn,
                    eq: list_eq_fn,
                    hash: list_hash_fn,
                },
            );
            self.list_type_indices.insert(
                canonical.clone(),
                ListTypeIdx {
                    len: len_type,
                    reverse: rev_type,
                    concat: concat_type,
                    take: take_type,
                    drop: drop_type,
                    contains: contains_type,
                    cons: cons_type,
                    eq: list_eq_type,
                    hash: list_hash_type,
                },
            );
            self.list_order.push(canonical.clone());
        }

        // Per-(`List<T>`, `Vector<T>`) pair: from_list. Only when both
        // sides are registered.
        for canonical in list_canonicals {
            let elem =
                TypeRegistry::list_element_type(canonical).ok_or(WasmGcError::Validation(
                    format!("list canonical `{canonical}` has no parsable element type"),
                ))?;
            let vec_canonical = format!("Vector<{}>", elem.trim());
            if !vector_canonicals.iter().any(|v| v == &vec_canonical) {
                continue;
            }
            if self.vfl_ops.contains_key(canonical) {
                continue;
            }
            let from_ty = *next_type_idx;
            *next_type_idx += 1;
            let to_ty = *next_type_idx;
            *next_type_idx += 1;
            let from_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let to_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            // Vector<T> eq + hash slots match the list cap — same
            // resolvable kinds (primitive / String / nominal record-
            // sum since 0.16.3).
            let eq_kind_ok = list_eq_kind(elem.trim(), registry).is_some();
            let (vec_eq_ty, vec_hash_ty, vec_eq_fn, vec_hash_fn) = if eq_kind_ok {
                let eq_ty = *next_type_idx;
                *next_type_idx += 1;
                let hash_ty = *next_type_idx;
                *next_type_idx += 1;
                let eq_fn = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                let hash_fn = *next_wasm_fn_idx;
                *next_wasm_fn_idx += 1;
                (Some(eq_ty), Some(hash_ty), Some(eq_fn), Some(hash_fn))
            } else {
                (None, None, None, None)
            };
            self.vfl_ops.insert(
                canonical.clone(),
                VectorFromListOps {
                    from_list: from_fn,
                    to_list: to_fn,
                    eq: vec_eq_fn,
                    hash: vec_hash_fn,
                },
            );
            self.vfl_type_indices
                .insert(canonical.clone(), (from_ty, to_ty, vec_eq_ty, vec_hash_ty));
            self.vfl_order.push(canonical.clone());
        }

        // `List.zip(la, lb) -> List<Tuple<A, B>>` — per-Tuple<A,B>
        // helper. Registered when all three lists exist in the
        // registry. The zip body needs `List<Tuple<A,B>>.reverse`
        // which we already emit as part of the per-list helpers.
        for tup_canonical in tuple_canonicals {
            let (a, b) = match super::types::TypeRegistry::tuple_ab(tup_canonical) {
                Some(x) => x,
                None => continue,
            };
            let la = format!("List<{}>", a.trim());
            let lb = format!("List<{}>", b.trim());
            let lt = format!("List<{tup_canonical}>");
            if !list_canonicals.iter().any(|c| c == &la)
                || !list_canonicals.iter().any(|c| c == &lb)
                || !list_canonicals.iter().any(|c| c == &lt)
            {
                continue;
            }
            if self.zip_ops.contains_key(tup_canonical) {
                continue;
            }
            let ty = *next_type_idx;
            *next_type_idx += 1;
            let fnx = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            self.zip_ops.insert(tup_canonical.clone(), fnx);
            self.zip_type_indices.insert(tup_canonical.clone(), ty);
            self.zip_order.push(tup_canonical.clone());
        }

        if register_string_split_join && self.string_split.is_none() {
            let split_type = *next_type_idx;
            *next_type_idx += 1;
            let join_type = *next_type_idx;
            *next_type_idx += 1;
            let split_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            let join_fn = *next_wasm_fn_idx;
            *next_wasm_fn_idx += 1;
            self.string_split = Some(StringSplitOps {
                split: split_fn,
                join: join_fn,
            });
            self.string_split_type_indices = Some((split_type, join_type));
        }
        Ok(())
    }

    pub(super) fn list_ops_for(&self, canonical: &str) -> Option<ListOps> {
        self.list_ops.get(canonical).copied()
    }

    pub(super) fn vfl_ops_for(&self, list_canonical: &str) -> Option<VectorFromListOps> {
        self.vfl_ops.get(list_canonical).copied()
    }

    pub(super) fn zip_op_for(&self, tuple_canonical: &str) -> Option<u32> {
        self.zip_ops.get(tuple_canonical).copied()
    }

    pub(super) fn string_split_ops(&self) -> Option<StringSplitOps> {
        self.string_split
    }

    pub(super) fn emit_helper_types(
        &self,
        types: &mut wasm_encoder::TypeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        for canonical in &self.list_order {
            let list_idx = registry
                .list_type_idx(canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "list `{canonical}` not registered"
                )))?;
            let list_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_idx),
            });
            types.ty().function([list_ref], [ValType::I64]); // len
            types.ty().function([list_ref], [list_ref]); // reverse
            // concat : (List<T>, List<T>) -> List<T>
            types.ty().function([list_ref, list_ref], [list_ref]);
            // take : (List<T>, Int) -> List<T>
            types.ty().function([list_ref, ValType::I64], [list_ref]);
            // drop : (List<T>, Int) -> List<T>
            types.ty().function([list_ref, ValType::I64], [list_ref]);
            // Order MUST match `assign_slots`: len, reverse, concat,
            // take, drop, cons, then contains (conditional). The fn
            // idx and type idx tracks were both bumped in this exact
            // sequence.
            let elem = TypeRegistry::list_element_type(canonical).unwrap();
            let elem_val = super::types::aver_to_wasm(elem.trim(), Some(registry))?.ok_or(
                WasmGcError::Validation(format!(
                    "list element type `{elem}` has no wasm representation"
                )),
            )?;
            // cons : (T, List<T>) -> List<T> — single struct.new.
            // Emit always (every literal needs it; bodies and call sites
            // benefit from one shared helper instead of inline scratch).
            types.ty().function([elem_val, list_ref], [list_ref]);
            // contains : (List<T>, T) -> Bool — element value type comes
            // from the registry's `aver_to_wasm` for T. Skipped when T
            // isn't natively eq-able (records, sums, nested generics).
            let kind = list_eq_kind(elem.trim(), registry);
            if kind.is_some() {
                types.ty().function([list_ref, elem_val], [ValType::I32]);
            }
            if kind.is_some() {
                // eq : (List<T>, List<T>) -> i32
                types.ty().function([list_ref, list_ref], [ValType::I32]);
                // hash : (List<T>) -> i32
                types.ty().function([list_ref], [ValType::I32]);
            }
        }
        for canonical in &self.vfl_order {
            let list_idx = registry
                .list_type_idx(canonical)
                .ok_or(WasmGcError::Validation(format!(
                    "list `{canonical}` not registered"
                )))?;
            let elem = TypeRegistry::list_element_type(canonical).unwrap();
            let vec_canonical = format!("Vector<{}>", elem.trim());
            let vec_idx =
                registry
                    .vector_type_idx(&vec_canonical)
                    .ok_or(WasmGcError::Validation(format!(
                        "vector `{vec_canonical}` not registered for from_list"
                    )))?;
            let list_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_idx),
            });
            let vec_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(vec_idx),
            });
            // from_list : (List<T>) -> Vector<T>
            types.ty().function([list_ref], [vec_ref]);
            // to_list : (Vector<T>) -> List<T>
            types.ty().function([vec_ref], [list_ref]);
            let elem = TypeRegistry::list_element_type(canonical).unwrap();
            if list_eq_kind(elem.trim(), registry).is_some() {
                // eq : (Vector<T>, Vector<T>) -> i32
                types.ty().function([vec_ref, vec_ref], [ValType::I32]);
                // hash : (Vector<T>) -> i32
                types.ty().function([vec_ref], [ValType::I32]);
            }
        }
        // List.zip per-(A,B): `(List<A>, List<B>) -> List<Tuple<A,B>>`.
        for tup_canonical in &self.zip_order {
            let (a, b) = super::types::TypeRegistry::tuple_ab(tup_canonical).unwrap();
            let la_idx = registry
                .list_type_idx(&format!("List<{}>", a.trim()))
                .ok_or(WasmGcError::Validation(format!(
                    "List.zip: List<{a}> not registered"
                )))?;
            let lb_idx = registry
                .list_type_idx(&format!("List<{}>", b.trim()))
                .ok_or(WasmGcError::Validation(format!(
                    "List.zip: List<{b}> not registered"
                )))?;
            let lt_idx = registry
                .list_type_idx(&format!("List<{tup_canonical}>"))
                .ok_or(WasmGcError::Validation(format!(
                    "List.zip: List<{tup_canonical}> not registered"
                )))?;
            let la_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(la_idx),
            });
            let lb_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(lb_idx),
            });
            let lt_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(lt_idx),
            });
            types.ty().function([la_ref, lb_ref], [lt_ref]);
        }
        if self.string_split.is_some() {
            let s_idx = registry
                .string_array_type_idx
                .ok_or(WasmGcError::Validation(
                    "string slot not registered for String.split/join helpers".into(),
                ))?;
            let list_str_idx =
                registry
                    .list_type_idx("List<String>")
                    .ok_or(WasmGcError::Validation(
                        "List<String> not registered for String.split/join helpers".into(),
                    ))?;
            let s_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(s_idx),
            });
            let l_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(list_str_idx),
            });
            // split : (String, String) -> List<String>
            types.ty().function([s_ref, s_ref], [l_ref]);
            // join : (List<String>, String) -> String
            types.ty().function([l_ref, s_ref], [s_ref]);
        }
        Ok(())
    }

    pub(super) fn emit_function_section(&self, funcs: &mut wasm_encoder::FunctionSection) {
        for canonical in &self.list_order {
            let idx = self.list_type_indices[canonical];
            // Order MUST match `assign_slots`: len, reverse, concat,
            // take, drop, cons (always), then contains (when slot
            // exists). The fn idx and type idx tracks were both bumped
            // in this exact sequence.
            funcs.function(idx.len);
            funcs.function(idx.reverse);
            funcs.function(idx.concat);
            funcs.function(idx.take);
            funcs.function(idx.drop);
            funcs.function(idx.cons);
            if let Some(t) = idx.contains {
                funcs.function(t);
            }
            if let Some(t) = idx.eq {
                funcs.function(t);
            }
            if let Some(t) = idx.hash {
                funcs.function(t);
            }
        }
        for canonical in &self.vfl_order {
            let (from_t, to_t, eq_t, hash_t) = self.vfl_type_indices[canonical];
            funcs.function(from_t);
            funcs.function(to_t);
            if let Some(t) = eq_t {
                funcs.function(t);
            }
            if let Some(t) = hash_t {
                funcs.function(t);
            }
        }
        for tup_canonical in &self.zip_order {
            funcs.function(self.zip_type_indices[tup_canonical]);
        }
        if let Some((split_t, join_t)) = self.string_split_type_indices {
            funcs.function(split_t);
            funcs.function(join_t);
        }
    }

    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
        string_eq_fn_idx: Option<u32>,
        eq_helper_fn_idx: &std::collections::HashMap<String, u32>,
        hash_helper_fn_idx: &std::collections::HashMap<String, u32>,
    ) -> Result<(), WasmGcError> {
        for canonical in &self.list_order {
            // Order MUST match `assign_slots` and
            // `emit_function_section`: len, reverse, concat, take,
            // drop, cons, then contains (when present).
            codes.function(&emit_list_len(canonical, registry)?);
            codes.function(&emit_list_reverse(canonical, registry)?);
            let ops = self.list_ops[canonical];
            codes.function(&emit_list_concat(canonical, registry, ops.reverse)?);
            codes.function(&emit_list_take(canonical, registry, ops.reverse)?);
            codes.function(&emit_list_drop(canonical, registry)?);
            codes.function(&emit_list_cons(canonical, registry)?);
            if ops.contains.is_some() {
                let elem = TypeRegistry::list_element_type(canonical).unwrap();
                let kind = list_eq_kind(elem.trim(), registry).unwrap();
                codes.function(&emit_list_contains(
                    canonical,
                    registry,
                    kind.clone(),
                    string_eq_fn_idx,
                    eq_helper_fn_idx,
                )?);
                if let (Some(eq_fn), Some(_hash_fn)) = (ops.eq, ops.hash) {
                    codes.function(&emit_list_eq(
                        canonical,
                        registry,
                        kind.clone(),
                        string_eq_fn_idx,
                        eq_fn,
                        eq_helper_fn_idx,
                    )?);
                    codes.function(&emit_list_hash(
                        canonical,
                        registry,
                        kind,
                        string_eq_fn_idx,
                        ops.hash.unwrap(),
                        hash_helper_fn_idx,
                    )?);
                }
            }
        }
        for canonical in &self.vfl_order {
            codes.function(&emit_vec_from_list(canonical, registry)?);
            codes.function(&emit_vec_to_list(canonical, registry)?);
            let ops = self.vfl_ops[canonical];
            if ops.eq.is_some() {
                let elem = TypeRegistry::list_element_type(canonical).unwrap();
                let kind = list_eq_kind(elem.trim(), registry).unwrap();
                codes.function(&emit_vec_eq(
                    canonical,
                    registry,
                    kind.clone(),
                    string_eq_fn_idx,
                    eq_helper_fn_idx,
                )?);
                codes.function(&emit_vec_hash(
                    canonical,
                    registry,
                    kind,
                    string_eq_fn_idx,
                    hash_helper_fn_idx,
                )?);
            }
        }
        for tup_canonical in &self.zip_order {
            // Zip needs the per-`List<Tuple<A,B>>` reverse fn idx —
            // the body builds LIFO and reverses at the end.
            let lt_canonical = format!("List<{tup_canonical}>");
            let reverse_fn = self.list_ops_for(&lt_canonical).map(|o| o.reverse).ok_or(
                WasmGcError::Validation(format!(
                    "List.zip: reverse fn for `{lt_canonical}` not registered"
                )),
            )?;
            codes.function(&emit_list_zip(tup_canonical, registry, reverse_fn)?);
        }
        if self.string_split.is_some() {
            // string_split needs to call List<String>.reverse to flip
            // the LIFO accumulator into source order.
            let reverse_fn_idx = self.list_ops_for("List<String>").map(|o| o.reverse).ok_or(
                WasmGcError::Validation(
                    "string_split helper needs List<String>.reverse — \
                     register List<String> via list_canonicals first"
                        .into(),
                ),
            )?;
            codes.function(&emit_string_split(registry, reverse_fn_idx)?);
            codes.function(&emit_string_join(registry)?);
        }
        Ok(())
    }
}

/// What underlying eq compiles to for `T`. Returned variant decides
/// the instruction sequence emitted in `List.contains`. `None` means
/// the registry won't allocate a `contains` slot for this `List<T>`,
/// and any call site that hits one surfaces a clear error.
#[derive(Debug, Clone)]
enum ListEqKind {
    /// `i64.eq` — Int.
    I64,
    /// `f64.eq` — Float (bit-exact compare; matches `==` in Aver
    /// semantics, NaN ≠ NaN like everywhere else).
    F64,
    /// `i32.eq` — Bool.
    I32,
    /// String byte-array equality via the `__wasmgc_string_eq`
    /// builtin. Carries that builtin's wasm fn idx so the helper
    /// emit can `call $eq`.
    StringEq,
    /// User-defined record T. Field-by-field eq emitted inline
    /// (same shape as `emit_eq_record` in maps.rs but inlined here
    /// to avoid a cross-module fn idx dependency). Carries the
    /// record name; field-type dispatch picks the right per-field
    /// eq instruction.
    RecordEq(String),
    /// User-defined sum / variant T (e.g. `type Shape = Circle(Float)
    /// | Rectangle(Float, Float)`). Two values are equal iff they
    /// share a constructor AND every field-pair is equal. Emitted as
    /// a per-variant `ref.test` cascade — for each constructor V_i:
    /// if both head and needle are V_i, compare fields; if only one
    /// is V_i, return false. Carries the parent type name.
    SumEq(String),
}

fn list_eq_kind(elem: &str, registry: &TypeRegistry) -> Option<ListEqKind> {
    let trimmed = elem.trim();
    // Newtype-erased sum / record (single-variant single-field of a
    // primitive) gets its underlying primitive's eq instruction.
    if let Some(under) = registry.newtype_underlying(trimmed) {
        return list_eq_kind(under, registry);
    }
    match trimmed {
        "Int" => Some(ListEqKind::I64),
        "Float" => Some(ListEqKind::F64),
        "Bool" => Some(ListEqKind::I32),
        "String" => Some(ListEqKind::StringEq),
        other => {
            // Record / sum element gets a contains/eq/hash slot
            // whenever its fields are themselves resolvable: primitives
            // or nominal types we've already accepted. Recursive refs
            // (Tree.Node holding Tree) and nominal cross-references
            // (Item holding Color) both flow through `field_resolvable`
            // which recurses with a `seen` set so cycles terminate.
            // The inline emitters (emit_record_eq_inline /
            // emit_sum_eq_inline) handle these via `eq_helper_fn_idx`
            // dispatch + `self_fn_idx` for self-recursion since 0.16.3.
            let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();
            if registry.record_type_idx(other).is_some() {
                if record_fields_resolvable(other, registry, &mut seen) {
                    Some(ListEqKind::RecordEq(other.to_string()))
                } else {
                    None
                }
            } else if registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| v.parent == other)
            {
                if sum_fields_resolvable(other, registry, &mut seen) {
                    Some(ListEqKind::SumEq(other.to_string()))
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

/// True when every field of `record` is something the inline eq
/// emitters can dispatch: primitive, a registered record/sum
/// (recursively resolvable), or self-recursion. Cycles terminate
/// via the `seen` set.
pub(super) fn record_fields_resolvable(
    record: &str,
    registry: &TypeRegistry,
    seen: &mut std::collections::HashSet<String>,
) -> bool {
    if !seen.insert(record.to_string()) {
        return true; // already visiting — break the cycle
    }
    let Some(fields) = registry.record_fields.get(record) else {
        return false;
    };
    fields
        .iter()
        .all(|(_, t)| field_type_resolvable(t.trim(), registry, seen))
}

pub(super) fn sum_fields_resolvable(
    parent: &str,
    registry: &TypeRegistry,
    seen: &mut std::collections::HashSet<String>,
) -> bool {
    if !seen.insert(parent.to_string()) {
        return true;
    }
    registry
        .variants
        .values()
        .flat_map(|vs| vs.iter())
        .filter(|v| v.parent == parent)
        .all(|v| {
            v.fields
                .iter()
                .all(|t| field_type_resolvable(t.trim(), registry, seen))
        })
}

pub(super) fn field_type_resolvable(
    field: &str,
    registry: &TypeRegistry,
    seen: &mut std::collections::HashSet<String>,
) -> bool {
    if matches!(field, "Int" | "Float" | "Bool" | "String") {
        return true;
    }
    if registry.record_type_idx(field).is_some() {
        return record_fields_resolvable(field, registry, seen);
    }
    if registry
        .variants
        .values()
        .flat_map(|vs| vs.iter())
        .any(|v| v.parent == field)
    {
        return sum_fields_resolvable(field, registry, seen);
    }
    // Generic carriers — `Option<X>`, `Result<X,Y>`, `Tuple<…>` get
    // per-instantiation `__eq_<canonical>` helpers since 0.16.3.
    // Resolvable iff every inner type is itself resolvable.
    // List/Vector/Map field types still fall through (their dispatch
    // from `emit_record_eq_inline` is a separate followup).
    if let Some(inner) = field
        .strip_prefix("Option<")
        .and_then(|s| s.strip_suffix('>'))
    {
        return field_type_resolvable(inner.trim(), registry, seen);
    }
    if let Some(inner) = field
        .strip_prefix("Result<")
        .and_then(|s| s.strip_suffix('>'))
    {
        let bytes = inner.as_bytes();
        let mut depth: i32 = 0;
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'<' | b'(' => depth += 1,
                b'>' | b')' => depth -= 1,
                b',' if depth == 0 => {
                    let ok = inner[..idx].trim();
                    let err = inner[idx + 1..].trim();
                    return field_type_resolvable(ok, registry, seen)
                        && field_type_resolvable(err, registry, seen);
                }
                _ => {}
            }
        }
        return false;
    }
    if let Some(inner) = field
        .strip_prefix("Tuple<")
        .and_then(|s| s.strip_suffix('>'))
    {
        let bytes = inner.as_bytes();
        let mut depth: i32 = 0;
        let mut start = 0;
        for (idx, b) in bytes.iter().enumerate() {
            match b {
                b'<' | b'(' => depth += 1,
                b'>' | b')' => depth -= 1,
                b',' if depth == 0 => {
                    let elem = inner[start..idx].trim();
                    if !field_type_resolvable(elem, registry, seen) {
                        return false;
                    }
                    start = idx + 1;
                }
                _ => {}
            }
        }
        return field_type_resolvable(inner[start..].trim(), registry, seen);
    }
    false
}

fn list_idx_of(canonical: &str, registry: &TypeRegistry) -> Result<u32, WasmGcError> {
    registry
        .list_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "list `{canonical}` not registered"
        )))
}

fn vec_idx_of_pair(
    list_canonical: &str,
    registry: &TypeRegistry,
) -> Result<(u32, ValType), WasmGcError> {
    let elem = TypeRegistry::list_element_type(list_canonical).unwrap();
    let vec_canonical = format!("Vector<{}>", elem.trim());
    let vec_idx = registry
        .vector_type_idx(&vec_canonical)
        .ok_or(WasmGcError::Validation(format!(
            "vector `{vec_canonical}` not registered"
        )))?;
    let elem_val =
        super::types::aver_to_wasm(elem.trim(), Some(registry))?.ok_or(WasmGcError::Validation(
            format!("list element type `{elem}` has no wasm representation"),
        ))?;
    Ok((vec_idx, elem_val))
}

/// `len : (List<T>) -> i64`.
fn emit_list_len(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=in. locals: 1=cur, 2=count.
    let mut f = Function::new([(1, list_ref), (1, ValType::I64)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalSet(2));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
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

/// `reverse : (List<T>) -> List<T>`.
fn emit_list_reverse(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    let elem = TypeRegistry::list_element_type(canonical).unwrap();
    let elem_val =
        super::types::aver_to_wasm(elem.trim(), Some(registry))?.ok_or(WasmGcError::Validation(
            format!("list element type `{elem}` has no wasm representation"),
        ))?;
    // params: 0=in. locals: 1=cur, 2=acc, 3=val
    let mut f = Function::new([(1, list_ref), (1, list_ref), (1, elem_val)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(2));

    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
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

/// `from_list : (List<T>) -> Vector<T>`. Two-pass: count, allocate,
/// fill.
fn emit_vec_from_list(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let (vec_idx, _elem_val) = vec_idx_of_pair(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    let vec_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(vec_idx),
    });
    let mut f = Function::new([
        (1, list_ref),     // 1: cur
        (1, ValType::I32), // 2: len
        (1, vec_ref),      // 3: arr
        (1, ValType::I32), // 4: i
    ]);

    // Pass 1: count.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // arr = array.new_default $vec len
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayNewDefault(vec_idx));
    f.instruction(&Instruction::LocalSet(3));

    // Pass 2: fill.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::ArraySet(vec_idx));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `string_split : (String, String) -> List<String>`. Builds the list
/// LIFO (each chunk prepended), then calls the supplied
/// `List<String>.reverse` to flip into source order.
fn emit_string_split(
    registry: &TypeRegistry,
    reverse_fn_idx: u32,
) -> Result<Function, WasmGcError> {
    let s_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation("string slot not registered".into()))?;
    let list_idx = registry
        .list_type_idx("List<String>")
        .ok_or(WasmGcError::Validation(
            "List<String> not registered".into(),
        ))?;
    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(s_idx),
    });
    let l_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });

    // params: 0=s, 1=sep
    // locals: 2=s_len, 3=sep_len, 4=part_start, 5=search_pos,
    //         6=part_len, 7=part, 8=acc, 9=i, 10=found
    let mut f = Function::new([
        (1, ValType::I32), // 2: s_len
        (1, ValType::I32), // 3: sep_len
        (1, ValType::I32), // 4: part_start
        (1, ValType::I32), // 5: search_pos
        (1, ValType::I32), // 6: part_len
        (1, s_ref),        // 7: part
        (1, l_ref),        // 8: acc
        (1, ValType::I32), // 9: i
        (1, ValType::I32), // 10: found
    ]);

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(3));

    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(5));

    // Empty sep: per-byte split.
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    // part = array.new_default 1
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::ArrayNewDefault(s_idx));
    f.instruction(&Instruction::LocalSet(7));
    // part[0] = s[search_pos]
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::ArraySet(s_idx));
    // acc = cons(part, acc)
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(8));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block
    // reverse(acc); return
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::Call(reverse_fn_idx));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End); // if (sep empty)

    // Non-empty sep.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // s_len < sep_len → break
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32LtU);
    f.instruction(&Instruction::BrIf(1));
    // search_pos > s_len - sep_len → break
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::I32GtU);
    f.instruction(&Instruction::BrIf(1));

    // Inner cmp.
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::ArrayGetU(s_idx));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::Br(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // inner loop
    f.instruction(&Instruction::End); // inner block

    // if found
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::ArrayNewDefault(s_idx));
    f.instruction(&Instruction::LocalSet(7));
    // copy if non-zero
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: s_idx,
        array_type_index_src: s_idx,
    });
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(8));
    // search_pos += sep_len; part_start = search_pos. Then loop again.
    // Stack at this `Br`: if-block (0), outer-loop (1), outer-block (2).
    // We want to continue iterating, i.e. jump to the outer-loop
    // header — that's `Br(1)`. `Br(2)` would exit the whole search.
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(1));
    f.instruction(&Instruction::End);

    // not found: search_pos += 1
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(5));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // outer loop
    f.instruction(&Instruction::End); // outer block

    // Final chunk.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::ArrayNewDefault(s_idx));
    f.instruction(&Instruction::LocalSet(7));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: s_idx,
        array_type_index_src: s_idx,
    });
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(8));

    // reverse(acc); return.
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::Call(reverse_fn_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `string_join : (List<String>, String) -> String`. Two-pass: sum
/// total length, allocate, copy each element + sep into place.
fn emit_string_join(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let s_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation("string slot not registered".into()))?;
    let list_idx = registry
        .list_type_idx("List<String>")
        .ok_or(WasmGcError::Validation(
            "List<String> not registered".into(),
        ))?;
    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(s_idx),
    });
    let l_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=list, 1=sep
    // locals: 2=cur, 3=total, 4=first, 5=sep_len, 6=head_str, 7=head_len,
    //         8=out, 9=write_pos
    let mut f = Function::new([
        (1, l_ref),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, ValType::I32),
        (1, s_ref),
        (1, ValType::I32),
        (1, s_ref),
        (1, ValType::I32),
    ]);

    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(5));

    // Pass 1.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    // out = array.new_default $string total
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayNewDefault(s_idx));
    f.instruction(&Instruction::LocalSet(8));

    // Pass 2.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // if !first && sep_len > 0: copy sep
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: s_idx,
        array_type_index_src: s_idx,
    });
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    // head_str
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalSet(6));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(7));
    // copy head if non-empty
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::ArrayCopy {
        array_type_index_dst: s_idx,
        array_type_index_src: s_idx,
    });
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(9));
    // cur = cur.tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);

    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `concat : (List<T>, List<T>) -> List<T>`. Builds `reverse(a)`,
/// then walks it prepending onto `b`. Two passes (reverse + cons),
/// O(len(a)). The trailing `b` is shared structurally — no copy.
fn emit_list_concat(
    canonical: &str,
    registry: &TypeRegistry,
    reverse_fn: u32,
) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=a, 1=b. locals: 2=cur, 3=acc.
    let mut f = Function::new([(1, list_ref), (1, list_ref)]);
    // cur = reverse(a)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(reverse_fn));
    f.instruction(&Instruction::LocalSet(2));
    // acc = b
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(3));
    // while cur not null: acc = cons(cur.head, acc); cur = cur.tail
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `take : (List<T>, Int) -> List<T>`. Builds the prefix LIFO into an
/// accumulator then calls `reverse_fn` to flip into source order.
/// `n <= 0` returns the empty list.
fn emit_list_take(
    canonical: &str,
    registry: &TypeRegistry,
    reverse_fn: u32,
) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=in, 1=n. locals: 2=cur, 3=acc, 4=i.
    let mut f = Function::new([(1, list_ref), (1, list_ref), (1, ValType::I64)]);
    // cur = in
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    // acc = null
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(3));
    // i = 0
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalSet(4));
    // while i < n and cur not null: acc = cons(head, acc); i++; cur = tail
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64GeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // reverse(acc)
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::Call(reverse_fn));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `drop : (List<T>, Int) -> List<T>`. Walks the cons chain `n`
/// times, returns the remaining tail (shared structurally with the
/// input). `n <= 0` returns the input unchanged.
fn emit_list_drop(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=in, 1=n. locals: 2=cur, 3=i.
    let mut f = Function::new([(1, list_ref), (1, ValType::I64)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::LocalSet(3));
    // while i < n and cur not null: cur = tail; i++
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64GeS);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I64Const(1));
    f.instruction(&Instruction::I64Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `contains : (List<T>, T) -> Bool`. Walks the cons chain comparing
/// each head against the needle via the per-T eq instruction picked
/// by `kind`. For T=String dispatches to `__wasmgc_string_eq`; for
/// T=record / sum, emits inline field-by-field eq (record) or per-
/// variant `ref.test` cascade (sum).
fn emit_list_contains(
    canonical: &str,
    registry: &TypeRegistry,
    kind: ListEqKind,
    string_eq_fn_idx: Option<u32>,
    eq_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    let elem = TypeRegistry::list_element_type(canonical).unwrap();
    let elem_val =
        super::types::aver_to_wasm(elem.trim(), Some(registry))?.ok_or(WasmGcError::Validation(
            format!("list element type `{elem}` has no wasm representation"),
        ))?;
    // params: 0=in, 1=needle. local 2 = cur. RecordEq adds two
    // extra scratch locals (3 = head, 4 = needle copy) since field-
    // by-field eq needs `struct.get` against both refs multiple
    // times.
    let mut locals: Vec<(u32, ValType)> = vec![(1, list_ref)];
    if matches!(&kind, ListEqKind::RecordEq(_) | ListEqKind::SumEq(_)) {
        // Record / sum eq does multiple struct.get reads against
        // both head and needle — stash them into scratch locals
        // (3 = head, 4 = needle).
        locals.push((1, elem_val));
        locals.push((1, elem_val));
    }
    let mut f = Function::new(locals);
    // cur = in
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // compare cur.head == needle, leaving an i32 (1=eq, 0=ne) on stack
    match &kind {
        ListEqKind::RecordEq(record_name) => {
            // Stash head + needle into scratch locals 3, 4 so we
            // can struct.get fields multiple times.
            f.instruction(&Instruction::LocalGet(2));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::LocalSet(3));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::LocalSet(4));
            emit_record_eq_inline(
                &mut f,
                record_name,
                registry,
                3,
                4,
                string_eq_fn_idx,
                eq_helper_fn_idx,
                None,
            )?;
        }
        ListEqKind::SumEq(parent_name) => {
            // Same scratch dance as RecordEq — both ref.test and
            // ref.cast want repeated access to head + needle.
            f.instruction(&Instruction::LocalGet(2));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::LocalSet(3));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::LocalSet(4));
            emit_sum_eq_inline(
                &mut f,
                parent_name,
                registry,
                3,
                4,
                string_eq_fn_idx,
                eq_helper_fn_idx,
                None,
            )?;
        }
        _ => {
            f.instruction(&Instruction::LocalGet(2));
            f.instruction(&Instruction::StructGet {
                struct_type_index: list_idx,
                field_index: 0,
            });
            f.instruction(&Instruction::LocalGet(1));
            match &kind {
                ListEqKind::I64 => f.instruction(&Instruction::I64Eq),
                ListEqKind::F64 => f.instruction(&Instruction::F64Eq),
                ListEqKind::I32 => f.instruction(&Instruction::I32Eq),
                ListEqKind::StringEq => {
                    let eq_fn = string_eq_fn_idx.ok_or(WasmGcError::Validation(
                        "List.contains over String/Char needs __wasmgc_string_eq registered".into(),
                    ))?;
                    f.instruction(&Instruction::Call(eq_fn))
                }
                ListEqKind::RecordEq(_) | ListEqKind::SumEq(_) => panic!(
                    "internal compiler error: List.contains emit reached \
                     RecordEq/SumEq path; should be filtered upstream by \
                     `list_eq_kind` returning None for record/sum elements. \
                     Please file at https://github.com/jasisz/aver/issues"
                ),
            };
        }
    }
    // if eq → return true
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // cur = cur.tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Inline field-by-field eq for two records held in scratch locals.
/// Pushes a single i32 (1=eq, 0=ne) onto the stack. Field type
/// dispatch covers `{Int, Float, Bool, String}`; other field types
/// surface as Unimplemented (same constraint as `emit_eq_record` in
/// maps.rs — extending requires nested-record / list / vector eq
/// dispatch).
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_record_eq_inline(
    f: &mut Function,
    record_name: &str,
    registry: &TypeRegistry,
    head_local: u32,
    needle_local: u32,
    string_eq_fn_idx: Option<u32>,
    eq_helper_fn_idx: &std::collections::HashMap<String, u32>,
    self_fn_idx: Option<u32>,
) -> Result<(), WasmGcError> {
    let record_idx = registry
        .record_type_idx(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "List.contains: record `{record_name}` not registered"
        )))?;
    let fields = registry
        .record_fields
        .get(record_name)
        .ok_or(WasmGcError::Validation(format!(
            "List.contains: record `{record_name}` has no field info"
        )))?;
    if fields.is_empty() {
        // Two empty records always equal.
        f.instruction(&Instruction::I32Const(1));
        return Ok(());
    }
    for (i, (_, field_ty)) in fields.iter().enumerate() {
        // push head.f
        f.instruction(&Instruction::LocalGet(head_local));
        f.instruction(&Instruction::StructGet {
            struct_type_index: record_idx,
            field_index: i as u32,
        });
        // push needle.f
        f.instruction(&Instruction::LocalGet(needle_local));
        f.instruction(&Instruction::StructGet {
            struct_type_index: record_idx,
            field_index: i as u32,
        });
        // emit per-field eq → i32
        match field_ty.trim() {
            "Int" => {
                f.instruction(&Instruction::I64Eq);
            }
            "Bool" => {
                f.instruction(&Instruction::I32Eq);
            }
            "Float" => {
                f.instruction(&Instruction::F64Eq);
            }
            "String" => {
                let eq_fn = string_eq_fn_idx.ok_or(WasmGcError::Validation(
                    "List.contains record field of String type needs \
                     __wasmgc_string_eq registered"
                        .into(),
                ))?;
                f.instruction(&Instruction::Call(eq_fn));
            }
            other if other == record_name && self_fn_idx.is_some() => {
                // Recursive ref to the same record — call self.
                f.instruction(&Instruction::Call(self_fn_idx.unwrap()));
            }
            other if eq_helper_fn_idx.contains_key(other) => {
                // Nested nominal type with its own __eq_<X> helper
                // — call by fn idx. Field refs are subtypes of
                // eqref so the implicit upcast at the call site
                // is fine.
                let idx = eq_helper_fn_idx[other];
                f.instruction(&Instruction::Call(idx));
            }
            other => {
                return Err(WasmGcError::Validation(format!(
                    "record `{record_name}` field type `{other}` has no eq dispatch \
                     (not in {{Int, Float, Bool, String}}, no `__eq_{other}` helper, \
                     not self-recursive)"
                )));
            }
        }
        if i > 0 {
            f.instruction(&Instruction::I32And);
        }
    }
    Ok(())
}

/// Inline sum-type eq for two values held in scratch locals. For
/// each constructor variant of `parent_name`, test whether both head
/// and needle have that concrete type. If both: cast + field-by-
/// field eq, push result. If only one: push 0 (different variants).
/// Final i32 on stack: 1 = equal, 0 = different.
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_sum_eq_inline(
    f: &mut Function,
    parent_name: &str,
    registry: &TypeRegistry,
    head_local: u32,
    needle_local: u32,
    string_eq_fn_idx: Option<u32>,
    eq_helper_fn_idx: &std::collections::HashMap<String, u32>,
    self_fn_idx: Option<u32>,
) -> Result<(), WasmGcError> {
    // Collect all variants of this sum (use a stable order — names
    // sorted ascending — so two compiler runs produce identical wasm).
    let mut variants: Vec<(String, super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(n, vs)| vs.iter().map(move |v| (n.clone(), v.clone())))
        .filter(|(_, v)| v.parent == parent_name)
        .collect();
    variants.sort_by(|a, b| a.0.cmp(&b.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "List.contains: sum type `{parent_name}` has no variants"
        )));
    }
    // The whole cascade lives inside an `(block (result i32))` so
    // each per-variant arm can `br` out with its own i32 verdict.
    f.instruction(&Instruction::Block(BlockType::Result(ValType::I32)));
    for (_v_name, info) in &variants {
        let v_idx = info.type_idx;
        let v_heap = wasm_encoder::HeapType::Concrete(v_idx);
        // if ref.test V head:
        f.instruction(&Instruction::LocalGet(head_local));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(BlockType::Empty));
        // if ref.test V needle:
        f.instruction(&Instruction::LocalGet(needle_local));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(BlockType::Empty));
        // both V — compare fields. If no fields, push 1.
        if info.fields.is_empty() {
            f.instruction(&Instruction::I32Const(1));
        } else {
            for (i, field_ty) in info.fields.iter().enumerate() {
                f.instruction(&Instruction::LocalGet(head_local));
                f.instruction(&Instruction::RefCastNonNull(v_heap));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: v_idx,
                    field_index: i as u32,
                });
                f.instruction(&Instruction::LocalGet(needle_local));
                f.instruction(&Instruction::RefCastNonNull(v_heap));
                f.instruction(&Instruction::StructGet {
                    struct_type_index: v_idx,
                    field_index: i as u32,
                });
                match field_ty.trim() {
                    "Int" => {
                        f.instruction(&Instruction::I64Eq);
                    }
                    "Bool" => {
                        f.instruction(&Instruction::I32Eq);
                    }
                    "Float" => {
                        f.instruction(&Instruction::F64Eq);
                    }
                    "String" => {
                        let eq_fn = string_eq_fn_idx.ok_or(WasmGcError::Validation(
                            "List.contains sum field of String type needs \
                             __wasmgc_string_eq registered"
                                .into(),
                        ))?;
                        f.instruction(&Instruction::Call(eq_fn));
                    }
                    other if other == parent_name && self_fn_idx.is_some() => {
                        // Recursive ref to the same sum (Tree.Node carrying
                        // Tree fields, ditto Cons-cell shapes) — call self.
                        f.instruction(&Instruction::Call(self_fn_idx.unwrap()));
                    }
                    other if eq_helper_fn_idx.contains_key(other) => {
                        // Nested record/sum field with its own __eq_<X>
                        // helper — dispatch by fn idx. Field refs are
                        // subtypes of eqref, the call's typed args
                        // accept implicit upcast.
                        let idx = eq_helper_fn_idx[other];
                        f.instruction(&Instruction::Call(idx));
                    }
                    other => {
                        return Err(WasmGcError::Validation(format!(
                            "sum `{parent_name}` variant field type `{other}` has no eq \
                             dispatch (not primitive, no `__eq_{other}` helper, not \
                             self-recursive)"
                        )));
                    }
                }
                if i > 0 {
                    f.instruction(&Instruction::I32And);
                }
            }
        }
        f.instruction(&Instruction::Br(2)); // out of outer Block(result i32)
        f.instruction(&Instruction::Else);
        // head V, needle != V → different variants → false
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Br(2));
        f.instruction(&Instruction::End);
        f.instruction(&Instruction::End);
    }
    // No variant matched head — exhaustiveness should make this
    // unreachable, but emit a defensive `0` so the block produces a
    // well-typed i32 either way.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End); // outer block result
    Ok(())
}

/// `cons : (T, List<T>) -> List<T>`. One `struct.new $list_T`. Used
/// by `emit_list_literal` so nested literals don't need a per-call
/// scratch local (which used to clash with the outer literal's own
/// accumulator and with multi-arm match scratch — same slot, three
/// fighting writers).
fn emit_list_cons(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `to_list : (Vector<T>) -> List<T>`. Walks the array right-to-left
/// prepending each element onto a cons-list accumulator. Single
/// pass, O(len). Per-(`Vector<T>`, `List<T>`) pair — `T` reads off
/// the registered list canonical.
fn emit_vec_to_list(canonical: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let (vec_idx, _) = vec_idx_of_pair(canonical, registry)?;
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    // params: 0=vec. locals: 1=acc, 2=i.
    let mut f = Function::new([(1, list_ref), (1, ValType::I32)]);
    // acc = null
    f.instruction(&Instruction::RefNull(HeapType::Concrete(list_idx)));
    f.instruction(&Instruction::LocalSet(1));
    // i = vec.len - 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    // while i >= 0:
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32LtS);
    f.instruction(&Instruction::BrIf(1));
    // acc = cons(vec[i], acc)
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::ArrayGet(vec_idx));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::LocalSet(1));
    // i--
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Sub);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `List.zip(la, lb) -> List<Tuple<A, B>>`. Walks both input lists
/// in parallel; per pair of cons cells, builds a `Tuple<A,B>` via
/// `struct.new` and prepends onto a LIFO accumulator. Stops when
/// either list ends. Final pass: call the per-`List<Tuple<A,B>>`
/// reverse to flip into source order.
fn emit_list_zip(
    tup_canonical: &str,
    registry: &TypeRegistry,
    reverse_fn: u32,
) -> Result<Function, WasmGcError> {
    let (a, b) = super::types::TypeRegistry::tuple_ab(tup_canonical).unwrap();
    let la_idx = registry
        .list_type_idx(&format!("List<{}>", a.trim()))
        .unwrap();
    let lb_idx = registry
        .list_type_idx(&format!("List<{}>", b.trim()))
        .unwrap();
    let lt_idx = registry
        .list_type_idx(&format!("List<{tup_canonical}>"))
        .unwrap();
    let tuple_idx = registry.tuple_type_idx(tup_canonical).unwrap();
    let la_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(la_idx),
    });
    let lb_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(lb_idx),
    });
    let lt_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(lt_idx),
    });
    // params: 0=la, 1=lb. locals: 2=cur_a, 3=cur_b, 4=acc.
    let mut f = Function::new([(1, la_ref), (1, lb_ref), (1, lt_ref)]);
    // cur_a = la; cur_b = lb; acc = null
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::RefNull(HeapType::Concrete(lt_idx)));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // if cur_a is null or cur_b is null → break
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // tuple = struct.new $tuple(cur_a.head, cur_b.head)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: la_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: lb_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::StructNew(tuple_idx));
    // acc = struct.new $list_tuple(tuple, acc)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::StructNew(lt_idx));
    f.instruction(&Instruction::LocalSet(4));
    // cur_a = cur_a.tail; cur_b = cur_b.tail
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: la_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: lb_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // reverse(acc)
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::Call(reverse_fn));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `eq : (la, lb) -> i32`. Walks both cons chains in lockstep,
/// compares head pairs with the per-T eq instruction. Self-recursive
/// via `return_call` so deep lists don't blow the stack. Returns 1
/// when both lists end at the same step with all heads equal; 0
/// otherwise. Same `T` constraint as contains — only emitted when T
/// has a `list_eq_kind`.
#[allow(clippy::too_many_arguments)]
fn emit_list_eq(
    canonical: &str,
    registry: &TypeRegistry,
    kind: ListEqKind,
    string_eq_fn_idx: Option<u32>,
    self_fn_idx: u32,
    eq_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    // params: 0 = la, 1 = lb. No locals — short body.
    let mut f = Function::new([]);
    // if la is_null and lb is_null → 1
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // la is non-null. if lb is null → 0
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // both non-null. Compare heads:
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    match &kind {
        ListEqKind::I64 => {
            f.instruction(&Instruction::I64Eq);
        }
        ListEqKind::F64 => {
            f.instruction(&Instruction::F64Eq);
        }
        ListEqKind::I32 => {
            f.instruction(&Instruction::I32Eq);
        }
        ListEqKind::StringEq => {
            let eq_fn = string_eq_fn_idx.ok_or(WasmGcError::Validation(
                "List eq over String needs __wasmgc_string_eq".into(),
            ))?;
            f.instruction(&Instruction::Call(eq_fn));
        }
        ListEqKind::RecordEq(name) | ListEqKind::SumEq(name) => {
            // Nominal element — dispatch to the per-type
            // `__eq_<X>` helper. Its signature is
            // `(eqref, eqref) -> i32`; both refs on the stack are
            // subtypes of eqref so the implicit upcast is fine.
            let idx = eq_helper_fn_idx
                .get(name)
                .copied()
                .ok_or(WasmGcError::Validation(format!(
                    "List eq over `{name}`: __eq_{name} helper not registered \
                     (discovery walker should have transitively flagged it)"
                )))?;
            f.instruction(&Instruction::Call(idx));
        }
    }
    // if heads differ → 0
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // tail-call self with tails
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::ReturnCall(self_fn_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `hash : (l) -> i32`. DJB2-style fold: `h = 5381; for elem: h = h
/// * 33 + element_hash`. Element hash dispatched per `kind`. Same
/// `T` constraint as eq.
#[allow(clippy::too_many_arguments)]
fn emit_list_hash(
    canonical: &str,
    registry: &TypeRegistry,
    kind: ListEqKind,
    string_eq_fn_idx: Option<u32>,
    _self_fn_idx: u32,
    hash_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let list_idx = list_idx_of(canonical, registry)?;
    let _ = string_eq_fn_idx;
    let elem = TypeRegistry::list_element_type(canonical).unwrap();
    // params: 0=l. locals: 1=cur, 2=h. Plus per-kind extras for
    // record / sum element hash dispatch (3=elem_ref, 4=elem_hash
    // accumulator).
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });
    let mut locals: Vec<(u32, ValType)> = vec![(1, list_ref), (1, ValType::I32)];
    match &kind {
        ListEqKind::RecordEq(record_name) => {
            let r_idx = registry
                .record_type_idx(record_name)
                .ok_or(WasmGcError::Validation(format!(
                    "list hash for `List<{record_name}>`: record not registered"
                )))?;
            let r_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(r_idx),
            });
            locals.push((1, r_ref)); // 3 = elem_ref
            locals.push((1, ValType::I32)); // 4 = elem_hash
        }
        ListEqKind::SumEq(_) => {
            // Sum types lower to `(ref null eq)` (per
            // `types.rs::aver_to_wasm` for sum-parent surface
            // names). Hold the head ref as eqref; per-variant
            // `ref.cast` narrows it to the concrete variant idx
            // before reading its fields.
            let eq_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Eq,
                },
            });
            locals.push((1, eq_ref)); // 3 = elem_ref (eqref carrier)
            locals.push((1, ValType::I32)); // 4 = elem_hash
        }
        _ => {}
    }
    let mut f = Function::new(locals);
    // h = 5381
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    // cur = l
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // h = h * 33 + element_hash(cur.head)
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    // Element hash → i32. Dispatch by `kind` rather than the raw
    // `elem` string — newtype optimisation erases single-field
    // records to their underlying primitive (`Box(n: Int)` → I64),
    // so the surface name can be `"Box"` while the actual wasm
    // representation is `i64`. `list_eq_kind` resolves the newtype
    // before returning, so `kind` is the source of truth for
    // representation.
    let _ = elem;
    match &kind {
        ListEqKind::I64 => {
            f.instruction(&Instruction::I32WrapI64);
        }
        ListEqKind::I32 => {} // bool — already i32
        ListEqKind::F64 => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        ListEqKind::StringEq => {
            // Inline DJB2 over the (array i8) — short version
            // that reuses the per-fn locals would need extra
            // scratch. Cheap fallback: take array length as the
            // string "hash" — collisions are fine, eq still
            // disambiguates. Same shape the legacy backend uses
            // for non-cryptographic mix.
            f.instruction(&Instruction::ArrayLen);
        }
        ListEqKind::RecordEq(record_name) => {
            let r_idx = registry
                .record_type_idx(record_name)
                .ok_or(WasmGcError::Validation(format!(
                    "list hash dispatch: record `{record_name}` not registered"
                )))?;
            let fields = registry
                .record_fields
                .get(record_name)
                .ok_or(WasmGcError::Validation(format!(
                    "list hash dispatch: record `{record_name}` has no field info"
                )))?;
            emit_record_inline_hash(
                &mut f,
                r_idx,
                fields,
                /* elem_local */ 3,
                /* elem_hash_local */ 4,
                registry,
                hash_helper_fn_idx,
            )?;
        }
        ListEqKind::SumEq(parent_name) => {
            emit_sum_inline_hash(
                &mut f,
                parent_name,
                registry,
                /* elem_local */ 3,
                /* elem_hash_local */ 4,
                hash_helper_fn_idx,
            )?;
        }
    }
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    // cur = cur.tail
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
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

/// Inline DJB2-style hash for a sum element. Stack on entry has the
/// element eqref; on exit has an `i32` hash. Walks variants in the
/// same sorted order as `emit_sum_eq_inline` (so two compiler runs
/// produce identical bytecode + so `eq` and `hash` agree on which
/// variant to inspect first), and per matched variant mixes its
/// `type_idx` (as a stable tag) plus DJB2-folds each primitive
/// field into `elem_hash`. Variants are disjoint subtypes of the
/// parent, so at most one `ref.test` succeeds per call — non-
/// matched arms `ref.test` to false and skip silently.
#[allow(clippy::too_many_arguments)]
fn emit_sum_inline_hash(
    f: &mut Function,
    parent_name: &str,
    registry: &TypeRegistry,
    elem_local: u32,
    elem_hash_local: u32,
    hash_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<(), WasmGcError> {
    // Collect variants of this sum. Same ordering as
    // `emit_sum_eq_inline` for parity.
    let mut variants: Vec<(String, super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(n, vs)| vs.iter().map(move |v| (n.clone(), v.clone())))
        .filter(|(_, v)| v.parent == parent_name)
        .collect();
    variants.sort_by(|a, b| a.0.cmp(&b.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "list hash dispatch: sum type `{parent_name}` has no variants"
        )));
    }

    // Save eqref → elem_local; init elem_hash = 5381.
    f.instruction(&Instruction::LocalSet(elem_local));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(elem_hash_local));

    for (_v_name, info) in &variants {
        let v_idx = info.type_idx;
        let v_heap = wasm_encoder::HeapType::Concrete(v_idx);
        // if ref.test V elem_ref:
        f.instruction(&Instruction::LocalGet(elem_local));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        // Fold variant tag (type_idx as i32) into elem_hash —
        // ensures empty variants of different shape still get
        // distinct hashes.
        f.instruction(&Instruction::LocalGet(elem_hash_local));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(elem_hash_local));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::I32Const(v_idx as i32));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(elem_hash_local));
        // Per field, downcast then fold.
        for (i, field_ty) in info.fields.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(elem_hash_local));
            f.instruction(&Instruction::I32Const(5));
            f.instruction(&Instruction::I32Shl);
            f.instruction(&Instruction::LocalGet(elem_hash_local));
            f.instruction(&Instruction::I32Add);

            f.instruction(&Instruction::LocalGet(elem_local));
            f.instruction(&Instruction::RefCastNonNull(v_heap));
            f.instruction(&Instruction::StructGet {
                struct_type_index: v_idx,
                field_index: i as u32,
            });
            let resolved: String = if let Some(under) = registry.newtype_underlying(field_ty.trim())
            {
                under.to_string()
            } else {
                field_ty.trim().to_string()
            };
            match resolved.as_str() {
                "Int" => {
                    f.instruction(&Instruction::I32WrapI64);
                }
                "Bool" => {} // already i32
                "Float" => {
                    f.instruction(&Instruction::I64ReinterpretF64);
                    f.instruction(&Instruction::I32WrapI64);
                }
                "String" => {
                    f.instruction(&Instruction::ArrayLen);
                }
                other if hash_helper_fn_idx.contains_key(other) => {
                    f.instruction(&Instruction::Call(hash_helper_fn_idx[other]));
                }
                _ => {
                    // Last-resort fallback — no helper for this
                    // shape. Drop, contribute 0 (eq disambiguates).
                    f.instruction(&Instruction::Drop);
                    f.instruction(&Instruction::I32Const(0));
                }
            }
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(elem_hash_local));
        }
        f.instruction(&Instruction::End); // end if
    }

    f.instruction(&Instruction::LocalGet(elem_hash_local));
    Ok(())
}

/// Inline DJB2-style hash for a record element. Stack on entry has
/// the element ref; on exit has an `i32` hash. `elem_local` and
/// `elem_hash_local` are pre-declared scratch slots in the calling
/// fn (typed `(ref null $record_idx)` and `i32` respectively).
///
/// Per-field hash trick mirrors the primitive arms in
/// `emit_list_hash`: Int → wrap, Float → reinterpret+wrap, Bool →
/// already i32, String → array.len. Field shapes are restricted to
/// {Int, Bool, Float, String} by `list_eq_kind`'s `all_simple` gate;
/// nested records / lists trip `WasmGcError::Validation` here.
#[allow(clippy::too_many_arguments)]
fn emit_record_inline_hash(
    f: &mut Function,
    record_idx: u32,
    fields: &[(String, String)],
    elem_local: u32,
    elem_hash_local: u32,
    registry: &TypeRegistry,
    hash_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<(), WasmGcError> {
    // Save record ref → elem_local for repeated struct.get.
    f.instruction(&Instruction::LocalSet(elem_local));
    // elem_hash = 5381 (DJB2 init).
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(elem_hash_local));
    for (i, (_field_name, field_type)) in fields.iter().enumerate() {
        // elem_hash = elem_hash * 33 + field_hash
        // (= (elem_hash << 5) + elem_hash + field_hash, DJB2.)
        f.instruction(&Instruction::LocalGet(elem_hash_local));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(elem_hash_local));
        f.instruction(&Instruction::I32Add);
        // Push field value, then mix to i32.
        f.instruction(&Instruction::LocalGet(elem_local));
        f.instruction(&Instruction::StructGet {
            struct_type_index: record_idx,
            field_index: i as u32,
        });
        let resolved: String = if let Some(under) = registry.newtype_underlying(field_type.trim()) {
            under.to_string()
        } else {
            field_type.trim().to_string()
        };
        match resolved.as_str() {
            "Int" => {
                f.instruction(&Instruction::I32WrapI64);
            }
            "Bool" => {} // already i32
            "Float" => {
                f.instruction(&Instruction::I64ReinterpretF64);
                f.instruction(&Instruction::I32WrapI64);
            }
            "String" => {
                f.instruction(&Instruction::ArrayLen);
            }
            other if hash_helper_fn_idx.contains_key(other) => {
                f.instruction(&Instruction::Call(hash_helper_fn_idx[other]));
            }
            _ => {
                // No helper available — drop + 0. Collision OK; eq
                // disambiguates the bucket. Hits when the field type
                // is a generic carrier we haven't covered yet (e.g.
                // `List<X>` field in a record).
                f.instruction(&Instruction::Drop);
                f.instruction(&Instruction::I32Const(0));
            }
        }
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(elem_hash_local));
    }
    // Push final elem_hash so the caller's mix can fold it into the
    // total list hash.
    f.instruction(&Instruction::LocalGet(elem_hash_local));
    Ok(())
}

/// `eq : (Vector<T>, Vector<T>) -> i32`. Length check + element-
/// wise eq via per-T instruction. Same `T must be eq-able` rule as
/// list_eq.
fn emit_vec_eq(
    canonical: &str,
    registry: &TypeRegistry,
    kind: ListEqKind,
    string_eq_fn_idx: Option<u32>,
    eq_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let (vec_idx, _) = vec_idx_of_pair(canonical, registry)?;
    // params: 0=va, 1=vb. locals: 2=len, 3=i.
    let mut f = Function::new([(1, ValType::I32), (1, ValType::I32)]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(vec_idx));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(vec_idx));
    match &kind {
        ListEqKind::I64 => {
            f.instruction(&Instruction::I64Eq);
        }
        ListEqKind::F64 => {
            f.instruction(&Instruction::F64Eq);
        }
        ListEqKind::I32 => {
            f.instruction(&Instruction::I32Eq);
        }
        ListEqKind::StringEq => {
            let eq_fn = string_eq_fn_idx.ok_or(WasmGcError::Validation(
                "Vector eq over String needs __wasmgc_string_eq".into(),
            ))?;
            f.instruction(&Instruction::Call(eq_fn));
        }
        ListEqKind::RecordEq(name) | ListEqKind::SumEq(name) => {
            // Nominal element — `Call(__eq_<X>)`. Same eqref upcast
            // shape as in `emit_list_eq`.
            let idx = eq_helper_fn_idx
                .get(name)
                .copied()
                .ok_or(WasmGcError::Validation(format!(
                    "Vector eq over `{name}`: __eq_{name} helper not registered"
                )))?;
            f.instruction(&Instruction::Call(idx));
        }
    }
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `hash : (Vector<T>) -> i32`. DJB2 fold over array elements.
fn emit_vec_hash(
    canonical: &str,
    registry: &TypeRegistry,
    kind: ListEqKind,
    _string_eq_fn_idx: Option<u32>,
    hash_helper_fn_idx: &std::collections::HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let (vec_idx, _) = vec_idx_of_pair(canonical, registry)?;
    let elem = TypeRegistry::list_element_type(canonical).unwrap();
    // params: 0=v. locals: 1=h, 2=len, 3=i, plus per-kind extras
    // (4=elem_ref, 5=elem_hash) for record/sum element dispatch —
    // newtype optimisation may erase a record name down to its
    // underlying primitive (kind == I64 even though `elem == "Box"`),
    // so dispatch by `kind`, not the surface element string.
    let mut locals: Vec<(u32, ValType)> =
        vec![(1, ValType::I32), (1, ValType::I32), (1, ValType::I32)];
    match &kind {
        ListEqKind::RecordEq(record_name) => {
            let r_idx = registry
                .record_type_idx(record_name)
                .ok_or(WasmGcError::Validation(format!(
                    "vector hash for `Vector<{record_name}>`: record not registered"
                )))?;
            let r_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Concrete(r_idx),
            });
            locals.push((1, r_ref)); // 4 = elem_ref
            locals.push((1, ValType::I32)); // 5 = elem_hash
        }
        ListEqKind::SumEq(_) => {
            let eq_ref = ValType::Ref(RefType {
                nullable: true,
                heap_type: HeapType::Abstract {
                    shared: false,
                    ty: wasm_encoder::AbstractHeapType::Eq,
                },
            });
            locals.push((1, eq_ref)); // 4 = elem_ref (eqref)
            locals.push((1, ValType::I32)); // 5 = elem_hash
        }
        _ => {}
    }
    let mut f = Function::new(locals);
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::ArrayLen);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    // h = h * 33 + elem_hash(v[i])
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::ArrayGet(vec_idx));
    let _ = elem;
    match &kind {
        ListEqKind::I64 => {
            f.instruction(&Instruction::I32WrapI64);
        }
        ListEqKind::I32 => {} // bool — already i32
        ListEqKind::F64 => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        ListEqKind::StringEq => {
            f.instruction(&Instruction::ArrayLen);
        }
        ListEqKind::RecordEq(record_name) => {
            let r_idx = registry
                .record_type_idx(record_name)
                .ok_or(WasmGcError::Validation(format!(
                    "vector hash dispatch: record `{record_name}` not registered"
                )))?;
            let fields = registry
                .record_fields
                .get(record_name)
                .ok_or(WasmGcError::Validation(format!(
                    "vector hash dispatch: record `{record_name}` has no field info"
                )))?;
            emit_record_inline_hash(
                &mut f,
                r_idx,
                fields,
                /* elem_local */ 4,
                /* elem_hash_local */ 5,
                registry,
                hash_helper_fn_idx,
            )?;
        }
        ListEqKind::SumEq(parent_name) => {
            emit_sum_inline_hash(
                &mut f,
                parent_name,
                registry,
                /* elem_local */ 4,
                /* elem_hash_local */ 5,
                hash_helper_fn_idx,
            )?;
        }
    }
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}
