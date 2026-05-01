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
    /// Total number of user-type slots reserved in the type section.
    /// Function types start AFTER these.
    pub(super) user_type_count: u32,
    /// Wasm type idx for the `(array i8)` String representation.
    /// Allocated lazily on first reference; `None` when no String is
    /// reachable from the program (most numeric bench scenarios).
    /// See `builtins/` README for the full repr decision.
    pub(super) string_array_type_idx: Option<u32>,
}

#[derive(Debug, Clone)]
pub(super) struct VariantInfo {
    pub(super) parent: String,
    pub(super) type_idx: u32,
    pub(super) fields: Vec<String>,
}

impl TypeRegistry {
    /// Walk top-level items and reserve a type index for every record /
    /// variant. The returned registry has every name pre-assigned so
    /// later passes (fn signature emit, body emit) can reference them
    /// without ordering tricks.
    pub(super) fn build(items: &[TopLevel]) -> Self {
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

        // Allocate String type slot if any signature references String.
        // Primary representation: `(array i8)` — engine-managed byte
        // sequence, self-contained (no host imports).
        let needs_string = items.iter().any(|item| match item {
            TopLevel::FnDef(fd) => {
                fd.return_type.contains("String")
                    || fd.params.iter().any(|(_, t)| t.contains("String"))
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

        Self {
            records,
            variants,
            record_fields,
            user_type_count: next_idx,
            string_array_type_idx,
        }
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

    /// Same predicate but addressed by variant constructor name (so
    /// emit sites can ask "is this constructor a newtype wrapper?").
    pub(super) fn variant_is_newtype(&self, variant_name: &str) -> Option<&str> {
        let info = self.variants.get(variant_name)?;
        self.newtype_underlying(&info.parent)
    }
}

fn is_primitive(ty: &str) -> bool {
    matches!(ty.trim(), "Int" | "Float" | "Bool")
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
    // Compound types not yet lowered.
    Err(WasmGcError::Unimplemented(match trimmed {
        _ if trimmed.starts_with("List<") => "phase 3c — List<T>",
        _ if trimmed.starts_with("Tuple<") => "phase 3c — Tuple",
        _ if trimmed.starts_with("Map<") => "phase 3c — Map<K,V>",
        _ if trimmed.starts_with("Vector<") => "phase 3c — Vector<T>",
        _ if trimmed.starts_with("Result<") => "phase 3c — Result",
        _ if trimmed.starts_with("Option<") => "phase 3c — Option",
        _ => "unknown type — likely a generic / inferred parameter that needs phase 3c",
    }))
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
