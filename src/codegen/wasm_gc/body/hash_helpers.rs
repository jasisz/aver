//! Per-record / per-sum / per-carrier hash helpers — the symmetric
//! counterpart to `eq_helpers.rs` for the hash side of nominal +
//! generic-carrier dispatch.
//!
//! ## Why
//!
//! `Map<K, V>` keyed by a record (`Map<Person, …>`) wants a
//! deterministic hash so two `Person` values that compare equal
//! collapse to the same bucket. The inline `emit_record_inline_hash`
//! / `emit_sum_inline_hash` in `lists.rs` previously fell back to
//! `drop + i32.const 0` for any non-primitive field — correct
//! (eq still disambiguates the bucket) but degenerate, every value
//! sharing primitive-prefix maps to one bucket and lookup goes
//! O(n).
//!
//! This module sets up `__hash_<X>` helpers per nominal /
//! carrier instantiation. Body emit goes through `Call(__hash_<X>)`
//! for non-primitive field values; the helper itself does the
//! shape-specific DJB2 fold. Symmetric to how `__eq_<X>` works on
//! the equality side.

use std::collections::HashMap;

use wasm_encoder::{Function, Instruction};

use super::super::WasmGcError;
use super::super::types::TypeRegistry;

/// What kind of type a registered hash helper covers. Same shapes
/// as `EqKind`, separate enum to keep the two registries
/// independent (a type may want a hash helper without an eq helper
/// — e.g. `Map<X, V>` registers hash for X without ever forcing
/// `==` on the surface).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HashKind {
    Record,
    Sum,
    OptionHash,
    ResultHash,
    TupleHash,
}

/// Per-module registry of `__hash_<TypeName>` helpers needed for
/// shape-faithful hashing inside list/vec/map helpers + per-record
/// / sum / carrier inline hash bodies.
#[derive(Default)]
pub(crate) struct HashHelperRegistry {
    order: Vec<String>,
    kinds: HashMap<String, HashKind>,
    /// `type_name -> (wasm_fn_idx, wasm_type_idx)`.
    slots: HashMap<String, (u32, u32)>,
}

impl HashHelperRegistry {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn register(&mut self, type_name: &str, kind: HashKind) {
        if !self.kinds.contains_key(type_name) {
            self.order.push(type_name.to_string());
            self.kinds.insert(type_name.to_string(), kind);
        }
    }

    /// Walk `type_name`'s fields and register `__hash_<X>` for
    /// every nominal / carrier piece reachable. Mirrors
    /// `EqHelperRegistry::register_transitive` but for hash; same
    /// resolvability gate so we don't end up with a registered
    /// helper whose body can't be emitted (a record holding e.g.
    /// `List<X>` field — list-as-field hash isn't wired yet).
    pub(crate) fn register_transitive(
        &mut self,
        type_name: &str,
        kind: HashKind,
        registry: &TypeRegistry,
    ) {
        if self.kinds.contains_key(type_name) {
            return;
        }
        // ETAP-2 carrier-`i64`: an eligible carrier is `i64`-erased — it has
        // no struct and needs no per-type `__hash_<Carrier>` helper (its hash
        // inlines as raw `i32.wrap_i64` at every use site). A struct-shaped
        // helper body over an `i64` value is invalid wasm. This is the
        // backstop covering every caller path (seed, field walk, `==`
        // discovery, nominal-in-type walk).
        if registry.is_eligible_carrier(type_name) {
            return;
        }
        let mut seen = std::collections::HashSet::new();
        let resolvable = match kind {
            HashKind::Record => {
                super::super::lists::record_fields_resolvable(type_name, registry, &mut seen)
            }
            HashKind::Sum => {
                super::super::lists::sum_fields_resolvable(type_name, registry, &mut seen)
            }
            HashKind::OptionHash | HashKind::ResultHash | HashKind::TupleHash => true,
        };
        if !resolvable {
            return;
        }
        self.register(type_name, kind);
        match kind {
            HashKind::Record => {
                if let Some(fields) = registry.record_fields.get(type_name) {
                    for (_, field_ty) in fields {
                        self.register_field_type(field_ty.trim(), registry);
                    }
                }
            }
            HashKind::Sum => {
                let variants: Vec<_> = registry
                    .variants
                    .values()
                    .flat_map(|vs| vs.iter())
                    .filter(|v| v.parent == type_name)
                    .cloned()
                    .collect();
                for v in &variants {
                    for field_ty in &v.fields {
                        self.register_field_type(field_ty.trim(), registry);
                    }
                }
            }
            // Mirror eq_helpers — recurse so direct top-level
            // registration of a carrier (seed walker etc.) still
            // discovers inner types.
            HashKind::OptionHash => {
                if let Some(inner) = type_name
                    .strip_prefix("Option<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    self.register_field_type(inner.trim(), registry);
                }
            }
            HashKind::ResultHash => {
                if let Some((ok, err)) = parse_result_kv(type_name) {
                    self.register_field_type(ok.trim(), registry);
                    self.register_field_type(err.trim(), registry);
                }
            }
            HashKind::TupleHash => {
                if let Some(elems) = parse_tuple_elems(type_name) {
                    for e in elems {
                        self.register_field_type(e.trim(), registry);
                    }
                }
            }
        }
    }

    fn register_field_type(&mut self, field_ty: &str, registry: &TypeRegistry) {
        if matches!(
            field_ty,
            "Int" | "Float" | "Bool" | "String" | "Unit" | "Byte" | "Char"
        ) {
            return;
        }
        // ETAP-2 carrier-`i64`: an eligible carrier is `i64`-erased — no
        // struct, no per-type `__hash_<Carrier>` helper; its hash is the raw
        // `i32.wrap_i64` inlined at the use site. Treat like a primitive.
        if registry.is_eligible_carrier(field_ty) {
            return;
        }
        if registry.record_fields.contains_key(field_ty) {
            self.register_transitive(field_ty, HashKind::Record, registry);
            return;
        }
        if registry
            .variants
            .values()
            .flat_map(|v| v.iter())
            .any(|v| v.parent == field_ty)
        {
            self.register_transitive(field_ty, HashKind::Sum, registry);
            return;
        }
        if let Some(inner) = field_ty
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.register_transitive(field_ty, HashKind::OptionHash, registry);
            self.register_field_type(inner.trim(), registry);
        } else if field_ty.starts_with("Result<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, HashKind::ResultHash, registry);
            if let Some((ok, err)) = parse_result_kv(field_ty) {
                self.register_field_type(ok.trim(), registry);
                self.register_field_type(err.trim(), registry);
            }
        } else if field_ty.starts_with("Tuple<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, HashKind::TupleHash, registry);
            if let Some(elems) = parse_tuple_elems(field_ty) {
                for elem in elems {
                    self.register_field_type(elem.trim(), registry);
                }
            }
        } else if let Some(inner) = field_ty
            .strip_prefix("List<")
            .and_then(|s| s.strip_suffix('>'))
        {
            // Symmetric to eq_helpers — recurse into the element so
            // `List<Option<X>>` registers `__hash_Option<X>`. The
            // list_helpers slot itself is owned by the separate
            // ListHelperRegistry; this walk only covers the carrier
            // / nominal hash dispatch the inline body needs.
            self.register_field_type(inner.trim(), registry);
        } else if let Some(inner) = field_ty
            .strip_prefix("Vector<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.register_field_type(inner.trim(), registry);
        } else if let Some(inner) = field_ty
            .strip_prefix("Map<")
            .and_then(|s| s.strip_suffix('>'))
        {
            // Mirror eq_helpers — Map<K,V> hash slot lives in
            // MapHelperRegistry; recurse so K and V's hash helpers
            // exist for the structural fold.
            let bytes = inner.as_bytes();
            let mut depth: i32 = 0;
            for (idx, b) in bytes.iter().enumerate() {
                match b {
                    b'<' | b'(' => depth += 1,
                    b'>' | b')' => depth -= 1,
                    b',' if depth == 0 => {
                        let k = inner[..idx].trim();
                        let v = inner[idx + 1..].trim();
                        self.register_field_type(k, registry);
                        self.register_field_type(v, registry);
                        return;
                    }
                    _ => {}
                }
            }
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&str, HashKind)> + '_ {
        self.order.iter().map(|n| (n.as_str(), self.kinds[n]))
    }

    pub(crate) fn assign_slots(&mut self, next_fn_idx: &mut u32, next_type_idx: &mut u32) {
        for name in &self.order {
            self.slots
                .insert(name.clone(), (*next_fn_idx, *next_type_idx));
            *next_fn_idx += 1;
            *next_type_idx += 1;
        }
    }

    pub(crate) fn lookup_fn_idx(&self, type_name: &str) -> Option<u32> {
        self.slots.get(type_name).map(|(f, _)| *f)
    }

    pub(crate) fn lookup_type_idx(&self, type_name: &str) -> Option<u32> {
        self.slots.get(type_name).map(|(_, t)| *t)
    }

    /// Emit `(eqref) -> i32` fn type for each registered helper, in
    /// the same order as `assign_slots`.
    pub(crate) fn emit_helper_types(&self, types: &mut wasm_encoder::TypeSection) {
        let eq_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Abstract {
                shared: false,
                ty: wasm_encoder::AbstractHeapType::Eq,
            },
        });
        for _ in &self.order {
            types.ty().function([eq_ref], [wasm_encoder::ValType::I32]);
        }
    }

    pub(crate) fn emit_helper_bodies(
        &self,
        codes: &mut wasm_encoder::CodeSection,
        registry: &TypeRegistry,
        string_eq_fn_idx: Option<u32>,
        compound_lookup: &HashMap<String, u32>,
    ) -> Result<(), WasmGcError> {
        let _ = string_eq_fn_idx; // String fields hash via array.len, no helper needed.
        // Same shape as eq_helpers — merge `List<T>` / `Vector<T>`
        // hash fn idxs from list_helpers so a record field of a
        // compound type can `Call(__hash_<canonical>)`.
        let mut helper_idx_map: HashMap<String, u32> = self
            .slots
            .iter()
            .map(|(n, (fn_idx, _))| (n.clone(), *fn_idx))
            .collect();
        for (canonical, fn_idx) in compound_lookup {
            helper_idx_map.insert(canonical.clone(), *fn_idx);
        }
        for name in &self.order {
            let kind = self.kinds[name];
            let self_fn_idx = self.slots.get(name).map(|(f, _)| *f);
            match kind {
                HashKind::Record => {
                    let f = emit_record_hash_body(name, registry, &helper_idx_map, self_fn_idx)?;
                    codes.function(&f);
                }
                HashKind::Sum => {
                    let f = emit_sum_hash_body(name, registry, &helper_idx_map, self_fn_idx)?;
                    codes.function(&f);
                }
                HashKind::OptionHash => {
                    let f = emit_option_hash_body(name, registry, &helper_idx_map)?;
                    codes.function(&f);
                }
                HashKind::ResultHash => {
                    let f = emit_result_hash_body(name, registry, &helper_idx_map)?;
                    codes.function(&f);
                }
                HashKind::TupleHash => {
                    let f = emit_tuple_hash_body(name, registry, &helper_idx_map)?;
                    codes.function(&f);
                }
            }
        }
        Ok(())
    }
}

/// `(eqref) -> i32` body for a record. Cast → typed, DJB2 fold over
/// fields, return.
fn emit_record_hash_body(
    name: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
    self_fn_idx: Option<u32>,
) -> Result<Function, WasmGcError> {
    let r_idx = registry
        .record_type_idx(name)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for record `{name}`: not registered"
        )))?;
    let fields = registry
        .record_fields
        .get(name)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for record `{name}`: no fields"
        )))?;
    let r_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(r_idx),
    });
    // Locals: 1 = typed record ref, 2 = h accumulator.
    let mut f = Function::new(vec![(1, r_ref), (1, wasm_encoder::ValType::I32)]);
    let r_heap = wasm_encoder::HeapType::Concrete(r_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(r_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    for (i, (field_name, field_ty)) in fields.iter().enumerate() {
        // h = h * 33 + field_hash
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: r_idx,
            field_index: i as u32,
        });
        // ETAP-2 multi-field carrier-`i64`: a bounded Int field erased to a
        // native `i64` hashes with the raw `i32.wrap_i64` (agreeing with the
        // `i64.eq` the record's eq helper picks for the same field); a boxed
        // Int field falls through to `emit_inner_hash_dispatch` (→ `__aint_hash`
        // under ℤ). An `__aint_hash` on a scalar `i64` is invalid wasm.
        if field_ty.trim() == "Int" && registry.is_eligible_carrier_field(name, field_name) {
            f.instruction(&Instruction::I32WrapI64);
        } else {
            emit_inner_hash_dispatch(
                &mut f,
                field_ty.trim(),
                registry,
                helper_idx_map,
                self_fn_idx.filter(|_| field_ty.trim() == name),
            )?;
        }
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(2));
    }
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for a sum. ref.test cascade per variant;
/// matched arm folds tag idx + variant fields.
fn emit_sum_hash_body(
    parent_name: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
    self_fn_idx: Option<u32>,
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(n, vs)| vs.iter().map(move |v| (n.clone(), v.clone())))
        .filter(|(_, v)| v.parent == parent_name)
        .collect();
    variants.sort_by(|a, b| a.0.cmp(&b.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "hash helper for sum `{parent_name}`: no variants"
        )));
    }
    // Locals: 1 = h accumulator. (Per-variant typed cast happens
    // inside the if-arm via `RefCastNonNull` → `StructGet`; we read
    // each field directly off the casted ref so no scratch slot
    // for the variant ref is needed.)
    let mut f = Function::new(vec![(1, wasm_encoder::ValType::I32)]);
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(1));
    for (_v_name, info) in &variants {
        let v_idx = info.type_idx;
        let v_heap = wasm_encoder::HeapType::Concrete(v_idx);
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::RefTestNonNull(v_heap));
        f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
        // Mix variant tag (its type_idx) into hash.
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::I32Const(v_idx as i32));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(1));
        for (i, field_ty) in info.fields.iter().enumerate() {
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Const(5));
            f.instruction(&Instruction::I32Shl);
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::RefCastNonNull(v_heap));
            f.instruction(&Instruction::StructGet {
                struct_type_index: v_idx,
                field_index: i as u32,
            });
            emit_inner_hash_dispatch(
                &mut f,
                field_ty.trim(),
                registry,
                helper_idx_map,
                self_fn_idx.filter(|_| field_ty.trim() == parent_name),
            )?;
            f.instruction(&Instruction::I32Add);
            f.instruction(&Instruction::LocalSet(1));
        }
        f.instruction(&Instruction::End);
    }
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for `Option<X>`. h held in local 2 across
/// the if-arm so the block body can be empty-typed (no stack-shape
/// constraint). DJB2-fold tag, then if Some fold inner hash too.
fn emit_option_hash_body(
    canonical: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let opt_idx = registry
        .option_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for `{canonical}`: option not registered"
        )))?;
    let inner = TypeRegistry::option_element_type(canonical).ok_or(WasmGcError::Validation(
        format!("hash helper for `{canonical}`: can't parse inner"),
    ))?;
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    // Locals: 1 = typed Option ref, 2 = h.
    let mut f = Function::new(vec![(1, opt_ref), (1, wasm_encoder::ValType::I32)]);
    let opt_heap = wasm_encoder::HeapType::Concrete(opt_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(opt_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    // h = h * 33 + tag
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    // if Some (tag != 0), mix inner hash into h.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    emit_inner_hash_dispatch(&mut f, inner.trim(), registry, helper_idx_map, None)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for `Result<X, Y>`. h held in local 2.
fn emit_result_hash_body(
    canonical: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for `{canonical}`: result not registered"
        )))?;
    let (ok_inner, err_inner) = parse_result_kv(canonical).ok_or(WasmGcError::Validation(
        format!("hash helper for `{canonical}`: can't parse inner"),
    ))?;
    let res_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    });
    let mut f = Function::new(vec![(1, res_ref), (1, wasm_encoder::ValType::I32)]);
    let res_heap = wasm_encoder::HeapType::Concrete(res_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(res_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    // h = h * 33 + tag
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    // Branch on tag.
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    // Ok arm: mix field 1 (ok) into h.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 1,
    });
    emit_inner_hash_dispatch(&mut f, ok_inner.trim(), registry, helper_idx_map, None)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Else);
    // Err arm: mix field 2 (err) into h.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Const(5));
    f.instruction(&Instruction::I32Shl);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    emit_inner_hash_dispatch(&mut f, err_inner.trim(), registry, helper_idx_map, None)?;
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `(eqref) -> i32` body for `Tuple<A, B, C, …>`. DJB2 fold per-elem.
fn emit_tuple_hash_body(
    canonical: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let tup_idx = registry
        .tuple_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "hash helper for `{canonical}`: tuple not registered"
        )))?;
    let elems = parse_tuple_elems(canonical).ok_or(WasmGcError::Validation(format!(
        "hash helper for `{canonical}`: can't parse elements"
    )))?;
    let tup_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(tup_idx),
    });
    let mut f = Function::new(vec![(1, tup_ref), (1, wasm_encoder::ValType::I32)]);
    let tup_heap = wasm_encoder::HeapType::Concrete(tup_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(tup_heap));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::I32Const(5381));
    f.instruction(&Instruction::LocalSet(2));
    for (i, elem) in elems.iter().enumerate() {
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Const(5));
        f.instruction(&Instruction::I32Shl);
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: tup_idx,
            field_index: i as u32,
        });
        emit_inner_hash_dispatch(&mut f, elem.trim(), registry, helper_idx_map, None)?;
        f.instruction(&Instruction::I32Add);
        f.instruction(&Instruction::LocalSet(2));
    }
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Stack: `[value]` of `inner` aver type. Push `i32` hash. Mirror
/// of `eq_helpers::emit_inner_eq_dispatch` but for hash.
fn emit_inner_hash_dispatch(
    f: &mut Function,
    inner: &str,
    registry: &TypeRegistry,
    helper_idx_map: &HashMap<String, u32>,
    self_fn_idx: Option<u32>,
) -> Result<(), WasmGcError> {
    // ETAP-2 carrier-`i64`: an eligible carrier field/payload is a native
    // `i64`, so its hash is a raw `i32.wrap_i64` — NOT `__aint_hash` (a ref
    // helper). Must agree with the carrier eq (`i64.eq`) so equal carriers
    // hash equal. Check before the newtype resolution.
    if registry.is_eligible_carrier(inner) {
        f.instruction(&Instruction::I32WrapI64);
        return Ok(());
    }
    let resolved: String = if let Some(under) = registry.newtype_underlying(inner) {
        under.to_string()
    } else {
        inner.to_string()
    };
    match resolved.as_str() {
        // `Int = ℤ` — an `Int` payload lowers to the `$aint` ref under
        // bignum → `__aint_hash` (agrees with `__aint_eq`; an
        // `i32.wrap_i64` on a ref is invalid wasm). This holds for BOTH a
        // genuine `Int` field and a newtype-erased Int (`Box(v: Int)`):
        // the erased field's wasm representation is still `$aint`.
        // `emit_aint_field_hash` itself falls to `i32.wrap_i64` when no
        // Int is reachable (bignum off).
        "Int" => {
            super::super::lists::emit_aint_field_hash(f, registry)?;
        }
        "Bool" => {} // already i32
        "Float" => {
            f.instruction(&Instruction::I64ReinterpretF64);
            f.instruction(&Instruction::I32WrapI64);
        }
        "String" => {
            f.instruction(&Instruction::ArrayLen);
        }
        other if Some(other) == self_fn_idx.and(Some(inner)) => {
            // Self-recursive case — caller passed `Some(self_fn_idx)`
            // when the field is a recursive ref to the parent.
            let idx = self_fn_idx.unwrap();
            f.instruction(&Instruction::Call(idx));
        }
        other if helper_idx_map.contains_key(other) => {
            f.instruction(&Instruction::Call(helper_idx_map[other]));
        }
        _other => {
            // Last-resort fallback (newtype-erased through to a
            // non-eq-able underlying, or a shape we genuinely can't
            // resolve). Drop the value, contribute 0. Same
            // collision-tolerant degradation the older inline
            // emitters used.
            f.instruction(&Instruction::Drop);
            f.instruction(&Instruction::I32Const(0));
        }
    }
    Ok(())
}

/// `Result<Ok, Err>` → `Some(("Ok", "Err"))`. Tracks angle/paren
/// depth so `Result<Map<K,V>, MyError>` splits at the right comma.
fn parse_result_kv(canonical: &str) -> Option<(&str, &str)> {
    let inner = canonical
        .trim()
        .strip_prefix("Result<")?
        .strip_suffix('>')?;
    let bytes = inner.as_bytes();
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

/// `Tuple<A, B, C>` → `Some(vec!["A", "B", "C"])`.
fn parse_tuple_elems(canonical: &str) -> Option<Vec<&str>> {
    let inner = canonical.trim().strip_prefix("Tuple<")?.strip_suffix('>')?;
    let bytes = inner.as_bytes();
    let mut depth: i32 = 0;
    let mut start = 0;
    let mut out = Vec::new();
    for (idx, b) in bytes.iter().enumerate() {
        match b {
            b'<' | b'(' => depth += 1,
            b'>' | b')' => depth -= 1,
            b',' if depth == 0 => {
                out.push(inner[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
    }
    out.push(inner[start..].trim());
    Some(out)
}
