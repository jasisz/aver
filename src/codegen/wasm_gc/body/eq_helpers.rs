//! Per-record / per-sum equality helpers for `BinOp::Eq` / `BinOp::Neq`
//! over user-defined nominal types.
//!
//! ## Why
//!
//! Aver's surface `==` on a sum type means "same variant + same fields";
//! `ref.eq` (struct identity) won't do because `Color.Red` constructed in
//! two different call sites yields two distinct refs even though they
//! denote the same enum value. Numeric `i64.eq` won't do either — the
//! lowering passes a `(ref null eq)` carrier through the BinOp default
//! path, then validation fails with "expected i64, found eqref" (the
//! checkers blocker).
//!
//! Each Sum/Record type that appears in a `BinOp::Eq` / `BinOp::Neq`
//! gets a per-type wasm helper fn `__eq_<TypeName>` registered by the
//! discovery walker. The body reuses the existing structural-eq logic
//! that powers `List.contains` over records / sums (`emit_record_eq_inline`
//! / `emit_sum_eq_inline` in `lists.rs`), wrapped as a standalone
//! `(ref null eq, ref null eq) -> i32` function so call sites just emit
//! `Call(eq_helper_idx)`.
//!
//! ## Field-type coverage
//!
//! Inherits the cap from the inline emitters: record/sum field types
//! must be in `{Int, Float, Bool, String}`. Nested records, lists,
//! vectors as fields surface as `Unimplemented` from the inline
//! emitters. Lifting that cap is a follow-up; today it covers every
//! enum-style sum (no fields) — including the `Color` enum that
//! checkers needs.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{Function, Instruction};

use super::super::WasmGcError;
use super::super::lists::{emit_record_eq_inline, emit_sum_eq_inline};
use super::super::types::TypeRegistry;

/// What kind of type a registered eq helper covers. Records and
/// sums are nominal (registry name = type name). The generic-
/// carrier kinds (Option / Result / Tuple) carry their canonical
/// instantiation string as the registry name (e.g. `"Option<Int>"`,
/// `"Result<Int,String>"`, `"Tuple<Int,String>"`) — one helper slot
/// per concrete inner shape.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EqKind {
    Record,
    Sum,
    OptionEq,
    ResultEq,
    TupleEq,
}

/// Per-module registry of `__eq_<TypeName>` helpers needed by
/// `BinOp::Eq` / `BinOp::Neq` sites. Slots allocated alongside other
/// per-instantiation helpers (Map, List, Vector); body emission reuses
/// the structural-eq routines from `lists.rs`.
#[derive(Default)]
pub(crate) struct EqHelperRegistry {
    /// Insertion order — wasm fn indices follow it.
    order: Vec<String>,
    kinds: HashMap<String, EqKind>,
    /// `type_name -> (wasm_fn_idx, wasm_type_idx)`.
    slots: HashMap<String, (u32, u32)>,
}

impl EqHelperRegistry {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn register(&mut self, type_name: &str, kind: EqKind) {
        if !self.kinds.contains_key(type_name) {
            self.order.push(type_name.to_string());
            self.kinds.insert(type_name.to_string(), kind);
        }
    }

    /// Register `type_name` and recursively register every nominal
    /// field type it transitively reaches. Needed because the
    /// emitted `__eq_<X>` body dispatches nested record/sum fields
    /// by `Call(__eq_<FieldType>)` — those helper slots have to
    /// exist even if the user never wrote `field == field` directly.
    /// `register` is idempotent on cycle (Tree → Tree.Node → Tree)
    /// so the recursion terminates.
    pub(crate) fn register_transitive(
        &mut self,
        type_name: &str,
        kind: EqKind,
        registry: &TypeRegistry,
    ) {
        if self.kinds.contains_key(type_name) {
            return;
        }
        // Skip nominal types whose fields contain unhandled shapes
        // (List/Map/Vector/Set inside fields — the inline eq emitter
        // covers Option/Result/Tuple via per-instantiation helpers
        // registered alongside the parent below). Without this guard
        // `register_field_type` silently dropped the unsupported
        // field and the helper body emit later panicked with "no eq
        // dispatch".
        let mut seen = std::collections::HashSet::new();
        let resolvable = match kind {
            EqKind::Record => {
                super::super::lists::record_fields_resolvable(type_name, registry, &mut seen)
            }
            EqKind::Sum => {
                super::super::lists::sum_fields_resolvable(type_name, registry, &mut seen)
            }
            // Carrier kinds — Option<X>/Result/Tuple have one
            // shape regardless of inner; resolvability is checked at
            // the record/sum level via `field_type_resolvable`.
            EqKind::OptionEq | EqKind::ResultEq | EqKind::TupleEq => true,
        };
        if !resolvable {
            return;
        }
        self.register(type_name, kind);
        // Walk fields and recurse on nominal types.
        match kind {
            EqKind::Record => {
                if let Some(fields) = registry.record_fields.get(type_name) {
                    for (_, field_ty) in fields {
                        self.register_field_type(field_ty.trim(), registry);
                    }
                }
            }
            EqKind::Sum => {
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
            // Carrier kinds — inner-type registration is handled by
            // the surrounding `register_field_type` arm that decides
            // to register the carrier in the first place. Nothing
            // extra to walk here (the helper body inspects the
            // canonical at emit time via `option_element_type` /
            // `parse_result_kv` / `parse_tuple_elems`).
            EqKind::OptionEq | EqKind::ResultEq | EqKind::TupleEq => {}
        }
    }

    fn register_field_type(&mut self, field_ty: &str, registry: &TypeRegistry) {
        if matches!(
            field_ty,
            "Int" | "Float" | "Bool" | "String" | "Unit" | "Byte" | "Char"
        ) {
            return;
        }
        if registry.record_fields.contains_key(field_ty) {
            self.register_transitive(field_ty, EqKind::Record, registry);
            return;
        }
        if registry
            .variants
            .values()
            .flat_map(|v| v.iter())
            .any(|v| v.parent == field_ty)
        {
            self.register_transitive(field_ty, EqKind::Sum, registry);
            return;
        }
        // Generic carriers — `Option<X>`, `Result<X,Y>`, `Tuple<…>`.
        // Each instantiation gets its own helper slot keyed by the
        // canonical string. Inner types are walked too so any
        // nominal piece (`Option<Color>` → register Color).
        if let Some(inner) = field_ty
            .strip_prefix("Option<")
            .and_then(|s| s.strip_suffix('>'))
        {
            self.register_transitive(field_ty, EqKind::OptionEq, registry);
            self.register_field_type(inner.trim(), registry);
        } else if field_ty.starts_with("Result<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, EqKind::ResultEq, registry);
            if let Some((ok, err)) = parse_result_kv(field_ty) {
                self.register_field_type(ok.trim(), registry);
                self.register_field_type(err.trim(), registry);
            }
        } else if field_ty.starts_with("Tuple<") && field_ty.ends_with('>') {
            self.register_transitive(field_ty, EqKind::TupleEq, registry);
            if let Some(elems) = parse_tuple_elems(field_ty) {
                for elem in elems {
                    self.register_field_type(elem.trim(), registry);
                }
            }
        }
        // List<T>, Vector<T>, Map<K,V> — those have their own
        // helper machinery (list_helpers/map_helpers fn idxs).
        // BinOp::Eq dispatch on collections is covered by the
        // separate body/emit.rs path; field-of-record holding a
        // collection still falls through here. Followup if real
        // programs surface it.
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&str, EqKind)> + '_ {
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

    /// Emits `(ref null eq, ref null eq) -> i32` fn type for each
    /// registered helper, in the same order as `assign_slots`.
    pub(crate) fn emit_helper_types(&self, types: &mut wasm_encoder::TypeSection) {
        let eq_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Abstract {
                shared: false,
                ty: wasm_encoder::AbstractHeapType::Eq,
            },
        });
        for _ in &self.order {
            types
                .ty()
                .function([eq_ref, eq_ref], [wasm_encoder::ValType::I32]);
        }
    }

    /// Emits the helper bodies. Each helper takes the two operands as
    /// params 0/1 (eqref) and returns i32 (1 = equal, 0 = different),
    /// reusing the structural-eq routines from `lists.rs` that already
    /// power `List.contains` over the same types.
    pub(crate) fn emit_helper_bodies(
        &self,
        codes: &mut wasm_encoder::CodeSection,
        registry: &TypeRegistry,
        string_eq_fn_idx: Option<u32>,
    ) -> Result<(), WasmGcError> {
        // Snapshot type_name → fn_idx so the inline emitters can
        // dispatch nested record/sum fields by `Call(idx)` instead
        // of erroring on `Unimplemented`. Self-recursive fields
        // (parent==field) get their own `self_fn_idx` argument so
        // recursive sum/record types (Tree.Node holding Tree, …)
        // resolve to a recursive call into the same helper.
        let helper_idx_map: HashMap<String, u32> = self
            .slots
            .iter()
            .map(|(n, (fn_idx, _))| (n.clone(), *fn_idx))
            .collect();
        for name in &self.order {
            let kind = self.kinds[name];
            let self_fn_idx = self.slots.get(name).map(|(f, _)| *f);
            match kind {
                EqKind::Sum => {
                    // Sum's emit_sum_eq_inline does its own
                    // ref.test/ref.cast cascade per variant — the
                    // raw eqref params are fine to feed in directly.
                    let mut f = Function::new(Vec::new());
                    emit_sum_eq_inline(
                        &mut f,
                        name,
                        registry,
                        0,
                        1,
                        string_eq_fn_idx,
                        &helper_idx_map,
                        self_fn_idx,
                    )?;
                    f.instruction(&Instruction::End);
                    codes.function(&f);
                }
                EqKind::Record => {
                    // Record's emit_record_eq_inline reads fields via
                    // `struct.get $record_idx <i>` straight off the
                    // local — that needs a typed `(ref null $record)`,
                    // not the eqref the helper signature carries. So
                    // declare two typed locals (idxs 2, 3), `ref.cast`
                    // each param into its slot, then drive the inline
                    // emitter against the typed locals.
                    let r_idx = registry
                        .record_type_idx(name)
                        .ok_or(WasmGcError::Validation(format!(
                            "eq helper for record `{name}`: record not registered"
                        )))?;
                    let r_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
                        nullable: true,
                        heap_type: wasm_encoder::HeapType::Concrete(r_idx),
                    });
                    let mut f = Function::new(vec![(2, r_ref)]);
                    let r_heap = wasm_encoder::HeapType::Concrete(r_idx);
                    f.instruction(&Instruction::LocalGet(0));
                    f.instruction(&Instruction::RefCastNonNull(r_heap));
                    f.instruction(&Instruction::LocalSet(2));
                    f.instruction(&Instruction::LocalGet(1));
                    f.instruction(&Instruction::RefCastNonNull(r_heap));
                    f.instruction(&Instruction::LocalSet(3));
                    emit_record_eq_inline(
                        &mut f,
                        name,
                        registry,
                        2,
                        3,
                        string_eq_fn_idx,
                        &helper_idx_map,
                        self_fn_idx,
                    )?;
                    f.instruction(&Instruction::End);
                    codes.function(&f);
                }
                EqKind::OptionEq => {
                    let f = emit_option_eq_body(name, registry, string_eq_fn_idx, &helper_idx_map)?;
                    codes.function(&f);
                }
                EqKind::ResultEq => {
                    let f = emit_result_eq_body(name, registry, string_eq_fn_idx, &helper_idx_map)?;
                    codes.function(&f);
                }
                EqKind::TupleEq => {
                    let f = emit_tuple_eq_body(name, registry, string_eq_fn_idx, &helper_idx_map)?;
                    codes.function(&f);
                }
            }
        }
        Ok(())
    }

    /// True when at least one helper needs `__wasmgc_string_eq` (for
    /// String fields). Used by module assembly to force-register the
    /// builtin slot.
    pub(crate) fn needs_string_eq(&self, registry: &TypeRegistry) -> bool {
        let mut visiting: HashSet<String> = HashSet::new();
        for name in &self.order {
            if type_has_string_field(name, self.kinds[name], registry, &mut visiting) {
                return true;
            }
        }
        false
    }
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

/// `Tuple<A, B, C>` → `Some(vec!["A", "B", "C"])`. Same depth-aware
/// split as `parse_result_kv`.
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

/// Emit `(eqref, eqref) -> i32` body for `Option<X>`. Layout:
/// `(struct (mut i32 tag) (mut X value))`, tag=0 None, tag=1 Some.
/// Compare tags first; if differ → 0; if both 0 → 1; if both 1 →
/// inner eq.
fn emit_option_eq_body(
    canonical: &str,
    registry: &TypeRegistry,
    string_eq_fn_idx: Option<u32>,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let opt_idx = registry
        .option_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "eq helper for `{canonical}`: option not registered"
        )))?;
    let inner = TypeRegistry::option_element_type(canonical).ok_or(WasmGcError::Validation(
        format!("eq helper for `{canonical}`: can't parse inner"),
    ))?;
    let opt_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(opt_idx),
    });
    let mut f = Function::new(vec![(2, opt_ref)]);
    let opt_heap = wasm_encoder::HeapType::Concrete(opt_idx);
    // Cast both eqref params → typed Option ref into locals 2, 3.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(opt_heap));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefCastNonNull(opt_heap));
    f.instruction(&Instruction::LocalSet(3));
    // if tag(lhs) != tag(rhs) → return 0
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // tags equal — if 0 (None), return 1
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // Both Some — compare inner via per-type dispatch.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: opt_idx,
        field_index: 1,
    });
    emit_inner_eq_dispatch(
        &mut f,
        inner.trim(),
        registry,
        string_eq_fn_idx,
        helper_idx_map,
    )?;
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Emit `(eqref, eqref) -> i32` body for `Result<X, Y>`. Layout:
/// `(struct (mut i32 tag) (mut X ok) (mut Y err))`, tag=0 Err,
/// tag=1 Ok. Compare tags; if differ → 0; both 0 → err eq;
/// both 1 → ok eq.
fn emit_result_eq_body(
    canonical: &str,
    registry: &TypeRegistry,
    string_eq_fn_idx: Option<u32>,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "eq helper for `{canonical}`: result not registered"
        )))?;
    let (ok_inner, err_inner) = parse_result_kv(canonical).ok_or(WasmGcError::Validation(
        format!("eq helper for `{canonical}`: can't parse inner"),
    ))?;
    let res_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_idx),
    });
    let mut f = Function::new(vec![(2, res_ref)]);
    let res_heap = wasm_encoder::HeapType::Concrete(res_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(res_heap));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefCastNonNull(res_heap));
    f.instruction(&Instruction::LocalSet(3));
    // if tag(lhs) != tag(rhs) → 0
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    // tags equal — branch on tag value: tag=1 (Ok) → field 1 eq;
    // tag=0 (Err) → field 2 eq.
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
        wasm_encoder::ValType::I32,
    )));
    // Ok arm
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 1,
    });
    emit_inner_eq_dispatch(
        &mut f,
        ok_inner.trim(),
        registry,
        string_eq_fn_idx,
        helper_idx_map,
    )?;
    f.instruction(&Instruction::Else);
    // Err arm
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::StructGet {
        struct_type_index: res_idx,
        field_index: 2,
    });
    emit_inner_eq_dispatch(
        &mut f,
        err_inner.trim(),
        registry,
        string_eq_fn_idx,
        helper_idx_map,
    )?;
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Emit `(eqref, eqref) -> i32` body for `Tuple<A, B, C, …>`.
/// Layout: `(struct field0 field1 …)`. Per-element eq, AND-fold.
fn emit_tuple_eq_body(
    canonical: &str,
    registry: &TypeRegistry,
    string_eq_fn_idx: Option<u32>,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let tup_idx = registry
        .tuple_type_idx(canonical)
        .ok_or(WasmGcError::Validation(format!(
            "eq helper for `{canonical}`: tuple not registered"
        )))?;
    let elems = parse_tuple_elems(canonical).ok_or(WasmGcError::Validation(format!(
        "eq helper for `{canonical}`: can't parse elements"
    )))?;
    let tup_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(tup_idx),
    });
    let mut f = Function::new(vec![(2, tup_ref)]);
    let tup_heap = wasm_encoder::HeapType::Concrete(tup_idx);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(tup_heap));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefCastNonNull(tup_heap));
    f.instruction(&Instruction::LocalSet(3));
    for (i, elem) in elems.iter().enumerate() {
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::StructGet {
            struct_type_index: tup_idx,
            field_index: i as u32,
        });
        f.instruction(&Instruction::LocalGet(3));
        f.instruction(&Instruction::StructGet {
            struct_type_index: tup_idx,
            field_index: i as u32,
        });
        emit_inner_eq_dispatch(
            &mut f,
            elem.trim(),
            registry,
            string_eq_fn_idx,
            helper_idx_map,
        )?;
        if i > 0 {
            f.instruction(&Instruction::I32And);
        }
    }
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Stack: `[lhs_value, rhs_value]` of `inner` aver type. Push i32
/// eq verdict. Same shape used by Option's payload, Result's two
/// arms, Tuple's per-element compares.
///
/// Newtype-optimised records (`Box(val: Int)` → i64) are handled
/// before the helper-map lookup: the actual stack values are the
/// underlying primitive, so dispatch through the primitive arm.
fn emit_inner_eq_dispatch(
    f: &mut Function,
    inner: &str,
    registry: &TypeRegistry,
    string_eq_fn_idx: Option<u32>,
    helper_idx_map: &HashMap<String, u32>,
) -> Result<(), WasmGcError> {
    // Resolve newtypes — `Box(n: Int)` reaches here as "Box" but
    // its wasm representation is i64.
    let resolved: String = if let Some(under) = registry.newtype_underlying(inner) {
        under.to_string()
    } else {
        inner.to_string()
    };
    match resolved.as_str() {
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
                "carrier eq with String inner needs __wasmgc_string_eq".into(),
            ))?;
            f.instruction(&Instruction::Call(eq_fn));
        }
        other if helper_idx_map.contains_key(other) => {
            f.instruction(&Instruction::Call(helper_idx_map[other]));
        }
        other => {
            return Err(WasmGcError::Validation(format!(
                "carrier eq inner type `{other}` has no eq dispatch"
            )));
        }
    }
    Ok(())
}

fn type_has_string_field(
    name: &str,
    kind: EqKind,
    registry: &TypeRegistry,
    visiting: &mut HashSet<String>,
) -> bool {
    if !visiting.insert(name.to_string()) {
        return false;
    }
    match kind {
        EqKind::Record => registry
            .record_fields
            .get(name)
            .map(|fs| fs.iter().any(|(_, t)| t.trim() == "String"))
            .unwrap_or(false),
        EqKind::Sum => registry
            .variants
            .values()
            .flat_map(|vs| vs.iter())
            .filter(|v| v.parent == name)
            .any(|v| v.fields.iter().any(|t| t.trim() == "String")),
        // Option<X>/Result<X,Y>/Tuple<…> — check whether any inner
        // type is `String`. Cheap string match on the canonical
        // (e.g. `"Option<String>"` contains `"String"`); accurate
        // enough since we just need to force-register
        // `__wasmgc_string_eq` when it might be called.
        EqKind::OptionEq | EqKind::ResultEq | EqKind::TupleEq => name.contains("String"),
    }
}
