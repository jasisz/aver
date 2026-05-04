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

/// What kind of nominal type a registered eq helper covers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EqKind {
    Record,
    Sum,
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
        for name in &self.order {
            let kind = self.kinds[name];
            let mut f = Function::new(Vec::new());
            // Local 0 = lhs, local 1 = rhs (function params). Reuse the
            // inline emitters from `lists.rs` which expect both operands
            // already stashed in named slots — no extra prologue needed.
            match kind {
                EqKind::Sum => emit_sum_eq_inline(&mut f, name, registry, 0, 1, string_eq_fn_idx)?,
                EqKind::Record => {
                    emit_record_eq_inline(&mut f, name, registry, 0, 1, string_eq_fn_idx)?
                }
            }
            f.instruction(&Instruction::End);
            codes.function(&f);
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
    }
}
