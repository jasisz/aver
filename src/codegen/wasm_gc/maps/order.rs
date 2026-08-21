//! Canonical key comparators used by wasm-gc Map iteration.
//!
//! These helpers define only value order. Hashtable topology, scratch-slot
//! sorting, and materialisation of keys/values/entries stay in the parent
//! module so all three views consume the same permutation.

use std::collections::HashMap;

use wasm_encoder::{BlockType, Function, HeapType, Instruction, ValType};

use super::super::types::TypeRegistry;
use super::super::{WasmGcError, wat_helper};

/// Canonical three-way comparison for one actual Map key type. Returns
/// `-1`, `0`, or `1`, matching `src/types/map.rs::compare_keys`.
pub(super) fn emit_cmp_for(
    k_aver: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    if registry.is_eligible_carrier(k_aver) {
        return Ok(emit_cmp_i64());
    }
    if registry.packed_sequence(k_aver).is_some() {
        return emit_cmp_packed_sequence(k_aver, registry);
    }
    match k_aver {
        "String" => emit_cmp_string(registry),
        "Int" | "Bool" => emit_cmp_primitive(k_aver, registry),
        _ if registry.newtype_underlying(k_aver).is_some() => {
            let underlying = registry.newtype_underlying(k_aver).unwrap();
            emit_cmp_proxy(underlying, registry, cmp_helpers)
        }
        _ if TypeRegistry::list_element_type(k_aver).is_some() => {
            emit_cmp_list(k_aver, registry, cmp_helpers)
        }
        _ if TypeRegistry::option_element_type(k_aver).is_some() => {
            emit_cmp_option(k_aver, registry, cmp_helpers)
        }
        _ if TypeRegistry::result_te(k_aver).is_some() => {
            emit_cmp_result(k_aver, registry, cmp_helpers)
        }
        _ if TypeRegistry::tuple_elements(k_aver).is_some() => {
            emit_cmp_tuple(k_aver, registry, cmp_helpers)
        }
        _ if registry.record_type_idx(k_aver).is_some() => {
            emit_cmp_record(k_aver, registry, cmp_helpers)
        }
        _ if registry
            .variants
            .values()
            .flat_map(|variants| variants.iter())
            .any(|variant| variant.parent == k_aver) =>
        {
            emit_cmp_sum(k_aver, registry, cmp_helpers)
        }
        _ => Err(WasmGcError::Unimplemented(
            "canonical wasm-gc Map order for this key type",
        )),
    }
}

fn cmp_helper_for(
    aver_ty: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<u32, WasmGcError> {
    let canonical = super::super::types::normalize_compound(aver_ty);
    cmp_helpers
        .get(&canonical)
        .or_else(|| cmp_helpers.get(registry.canonical_type_name(&canonical)))
        .or_else(|| {
            let bare = super::super::types::strip_inner_dotted_prefixes(&canonical);
            cmp_helpers.get(&bare)
        })
        .copied()
        .ok_or_else(|| {
            WasmGcError::Validation(format!(
                "canonical comparator dependency `{canonical}` was not registered"
            ))
        })
}

fn emit_cmp_proxy(
    underlying: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let helper = cmp_helper_for(underlying, registry, cmp_helpers)?;
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::Call(helper));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_i64() -> Function {
    let mut f = Function::new([]);
    emit_cmp_i64_locals(&mut f, 0, 1);
    f.instruction(&Instruction::End);
    f
}

fn emit_cmp_i64_locals(f: &mut Function, lhs: u32, rhs: u32) {
    f.instruction(&Instruction::LocalGet(lhs));
    f.instruction(&Instruction::LocalGet(rhs));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(lhs));
    f.instruction(&Instruction::LocalGet(rhs));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

fn emit_cmp_i32_locals(f: &mut Function, lhs: u32, rhs: u32, signed: bool) {
    f.instruction(&Instruction::LocalGet(lhs));
    f.instruction(&Instruction::LocalGet(rhs));
    f.instruction(if signed {
        &Instruction::I32LtS
    } else {
        &Instruction::I32LtU
    });
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(lhs));
    f.instruction(&Instruction::LocalGet(rhs));
    f.instruction(if signed {
        &Instruction::I32GtS
    } else {
        &Instruction::I32GtU
    });
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
}

fn emit_return_if_nonzero(f: &mut Function, cmp_local: u32) {
    f.instruction(&Instruction::LocalGet(cmp_local));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(cmp_local));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
}

fn emit_cmp_record(
    record_name: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let record_idx = registry.record_type_idx(record_name).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered record `{record_name}` missing"))
    })?;
    let fields = registry.record_fields.get(record_name).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered record `{record_name}` has no field table"))
    })?;
    let mut field_order: Vec<usize> = (0..fields.len()).collect();
    field_order.sort_by(|&left, &right| fields[left].0.cmp(&fields[right].0));

    // params 0/1 are records; local 2 is the nested verdict. Locals 3/4 are
    // used only by proof-erased Int fields, whose storage is native i64 even
    // when the ordinary Int comparator consumes an AverInt ref.
    let mut f = Function::new([(1, ValType::I32), (2, ValType::I64)]);
    for field_idx in field_order {
        let (field_name, field_ty) = &fields[field_idx];
        if field_ty.trim() == "Unit" {
            continue;
        }
        if field_ty.trim() == "Int" && registry.is_eligible_carrier_field(record_name, field_name) {
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::StructGet {
                struct_type_index: record_idx,
                field_index: field_idx as u32,
            });
            f.instruction(&Instruction::LocalSet(3));
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::StructGet {
                struct_type_index: record_idx,
                field_index: field_idx as u32,
            });
            f.instruction(&Instruction::LocalSet(4));
            emit_cmp_i64_locals(&mut f, 3, 4);
        } else {
            let helper = cmp_helper_for(field_ty, registry, cmp_helpers)?;
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::StructGet {
                struct_type_index: record_idx,
                field_index: field_idx as u32,
            });
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::StructGet {
                struct_type_index: record_idx,
                field_index: field_idx as u32,
            });
            f.instruction(&Instruction::Call(helper));
        }
        f.instruction(&Instruction::LocalSet(2));
        emit_return_if_nonzero(&mut f, 2);
    }
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_tuple(
    canonical: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let tuple_idx = registry.tuple_type_idx(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered tuple `{canonical}` is not registered"))
    })?;
    let elements = TypeRegistry::tuple_elements(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered tuple `{canonical}` cannot be parsed"))
    })?;
    let mut f = Function::new([(1, ValType::I32)]);
    for (field_idx, element_ty) in elements.iter().enumerate() {
        if element_ty.trim() == "Unit" {
            continue;
        }
        let helper = cmp_helper_for(element_ty, registry, cmp_helpers)?;
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::StructGet {
            struct_type_index: tuple_idx,
            field_index: field_idx as u32,
        });
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: tuple_idx,
            field_index: field_idx as u32,
        });
        f.instruction(&Instruction::Call(helper));
        f.instruction(&Instruction::LocalSet(2));
        emit_return_if_nonzero(&mut f, 2);
    }
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_list(
    canonical: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let list_idx = registry.list_type_idx(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered list `{canonical}` is not registered"))
    })?;
    let element_ty = TypeRegistry::list_element_type(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered list `{canonical}` has no element type"))
    })?;
    let element_cmp = if element_ty.trim() == "Unit" {
        None
    } else {
        Some(cmp_helper_for(element_ty, registry, cmp_helpers)?)
    };
    // The two parameters are advanced as cursors. No cons cells are copied.
    let mut f = Function::new([(1, ValType::I32)]);
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // [] < [..], and [] == [].
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    if let Some(element_cmp) = element_cmp {
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
        f.instruction(&Instruction::Call(element_cmp));
        f.instruction(&Instruction::LocalSet(2));
        emit_return_if_nonzero(&mut f, 2);
    }
    for cursor in [0, 1] {
        f.instruction(&Instruction::LocalGet(cursor));
        f.instruction(&Instruction::StructGet {
            struct_type_index: list_idx,
            field_index: 1,
        });
        f.instruction(&Instruction::LocalSet(cursor));
    }
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Unreachable);
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_option(
    canonical: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let option_idx = registry.option_type_idx(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered option `{canonical}` is not registered"))
    })?;
    let inner = TypeRegistry::option_element_type(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered option `{canonical}` has no inner type"))
    })?;
    let inner_cmp = if inner.trim() == "Unit" {
        None
    } else {
        Some(cmp_helper_for(inner, registry, cmp_helpers)?)
    };
    // locals 2/3 are tags: None=0, Some=1, already canonical order.
    let mut f = Function::new([(2, ValType::I32)]);
    for (source, target) in [(0, 2), (1, 3)] {
        f.instruction(&Instruction::LocalGet(source));
        f.instruction(&Instruction::StructGet {
            struct_type_index: option_idx,
            field_index: 0,
        });
        f.instruction(&Instruction::LocalSet(target));
    }
    emit_cmp_i32_locals(&mut f, 2, 3, true);
    f.instruction(&Instruction::LocalSet(2));
    emit_return_if_nonzero(&mut f, 2);
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    if let Some(inner_cmp) = inner_cmp {
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::StructGet {
            struct_type_index: option_idx,
            field_index: 1,
        });
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: option_idx,
            field_index: 1,
        });
        f.instruction(&Instruction::Call(inner_cmp));
    } else {
        f.instruction(&Instruction::I32Const(0));
    }
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_result(
    canonical: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let result_idx = registry.result_type_idx(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered result `{canonical}` is not registered"))
    })?;
    let (ok_ty, err_ty) = TypeRegistry::result_te(canonical).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered result `{canonical}` cannot be parsed"))
    })?;
    let ok_cmp = (ok_ty.trim() != "Unit")
        .then(|| cmp_helper_for(ok_ty, registry, cmp_helpers))
        .transpose()?;
    let err_cmp = (err_ty.trim() != "Unit")
        .then(|| cmp_helper_for(err_ty, registry, cmp_helpers))
        .transpose()?;
    // Runtime tags are Err=0 / Ok=1, while the language order is Ok < Err.
    let mut f = Function::new([(2, ValType::I32)]);
    for (source, target) in [(0, 2), (1, 3)] {
        f.instruction(&Instruction::LocalGet(source));
        f.instruction(&Instruction::StructGet {
            struct_type_index: result_idx,
            field_index: 0,
        });
        f.instruction(&Instruction::LocalSet(target));
    }
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32Ne);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    if let Some(ok_cmp) = ok_cmp {
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::StructGet {
            struct_type_index: result_idx,
            field_index: 1,
        });
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: result_idx,
            field_index: 1,
        });
        f.instruction(&Instruction::Call(ok_cmp));
    } else {
        f.instruction(&Instruction::I32Const(0));
    }
    f.instruction(&Instruction::Else);
    if let Some(err_cmp) = err_cmp {
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::StructGet {
            struct_type_index: result_idx,
            field_index: 2,
        });
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::StructGet {
            struct_type_index: result_idx,
            field_index: 2,
        });
        f.instruction(&Instruction::Call(err_cmp));
    } else {
        f.instruction(&Instruction::I32Const(0));
    }
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_sum(
    parent_name: &str,
    registry: &TypeRegistry,
    cmp_helpers: &HashMap<String, u32>,
) -> Result<Function, WasmGcError> {
    let mut variants: Vec<(String, super::super::types::VariantInfo)> = registry
        .variants
        .iter()
        .flat_map(|(name, variants)| {
            variants
                .iter()
                .map(move |variant| (name.clone(), variant.clone()))
        })
        .filter(|(_, variant)| variant.parent == parent_name)
        .collect();
    variants.sort_by(|left, right| left.0.cmp(&right.0));
    if variants.is_empty() {
        return Err(WasmGcError::Validation(format!(
            "ordered sum `{parent_name}` has no constructors"
        )));
    }

    // locals 2/3 = alphabetical constructor rank, local 4 = payload verdict.
    let mut f = Function::new([(3, ValType::I32)]);
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::LocalSet(3));
    for (tag, (_, variant)) in variants.iter().enumerate() {
        let heap = HeapType::Concrete(variant.type_idx);
        for (source, target) in [(0, 2), (1, 3)] {
            f.instruction(&Instruction::LocalGet(source));
            f.instruction(&Instruction::RefTestNonNull(heap));
            f.instruction(&Instruction::If(BlockType::Empty));
            f.instruction(&Instruction::I32Const(tag as i32));
            f.instruction(&Instruction::LocalSet(target));
            f.instruction(&Instruction::End);
        }
    }
    emit_cmp_i32_locals(&mut f, 2, 3, true);
    f.instruction(&Instruction::LocalSet(4));
    emit_return_if_nonzero(&mut f, 4);

    // Equal constructor ranks mean both refs can be cast to the same concrete
    // payload struct. Compare positional fields left-to-right.
    for (tag, (_, variant)) in variants.iter().enumerate() {
        let heap = HeapType::Concrete(variant.type_idx);
        f.instruction(&Instruction::LocalGet(2));
        f.instruction(&Instruction::I32Const(tag as i32));
        f.instruction(&Instruction::I32Eq);
        f.instruction(&Instruction::If(BlockType::Empty));
        for (field_idx, field_ty) in variant.fields.iter().enumerate() {
            if field_ty.trim() == "Unit" {
                continue;
            }
            let helper = cmp_helper_for(field_ty, registry, cmp_helpers)?;
            f.instruction(&Instruction::LocalGet(0));
            f.instruction(&Instruction::RefCastNonNull(heap));
            f.instruction(&Instruction::StructGet {
                struct_type_index: variant.type_idx,
                field_index: field_idx as u32,
            });
            f.instruction(&Instruction::LocalGet(1));
            f.instruction(&Instruction::RefCastNonNull(heap));
            f.instruction(&Instruction::StructGet {
                struct_type_index: variant.type_idx,
                field_index: field_idx as u32,
            });
            f.instruction(&Instruction::Call(helper));
            f.instruction(&Instruction::LocalSet(4));
            emit_return_if_nonzero(&mut f, 4);
        }
        f.instruction(&Instruction::I32Const(0));
        f.instruction(&Instruction::Return);
        f.instruction(&Instruction::End);
    }
    // Defensive only: well-typed values always matched one constructor.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_packed_sequence(name: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    use crate::codegen::proof_lower::PackedIntElement;

    let packed = registry.packed_sequence(name).ok_or_else(|| {
        WasmGcError::Validation(format!("ordered packed sequence `{name}` is missing"))
    })?;
    let is_i64 = matches!(packed.layout.element, PackedIntElement::I64);
    let signed = matches!(
        packed.layout.element,
        PackedIntElement::I8
            | PackedIntElement::I16
            | PackedIntElement::I32
            | PackedIntElement::I64
    );
    // locals 2=len(a), 3=len(b), 4=index, 5/6=current elements, 7=verdict.
    let element_val = if is_i64 { ValType::I64 } else { ValType::I32 };
    let mut f = Function::new([(3, ValType::I32), (2, element_val), (1, ValType::I32)]);
    for (source, target) in [(0, 2), (1, 3)] {
        f.instruction(&Instruction::LocalGet(source));
        f.instruction(&Instruction::ArrayLen);
        f.instruction(&Instruction::LocalSet(target));
    }
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));
    for (source, target) in [(0, 5), (1, 6)] {
        f.instruction(&Instruction::LocalGet(source));
        f.instruction(&Instruction::LocalGet(4));
        match packed.layout.element {
            PackedIntElement::U8 | PackedIntElement::U16 => {
                f.instruction(&Instruction::ArrayGetU(packed.type_idx));
            }
            PackedIntElement::I8 | PackedIntElement::I16 => {
                f.instruction(&Instruction::ArrayGetS(packed.type_idx));
            }
            PackedIntElement::U32 | PackedIntElement::I32 | PackedIntElement::I64 => {
                f.instruction(&Instruction::ArrayGet(packed.type_idx));
            }
        }
        f.instruction(&Instruction::LocalSet(target));
    }
    if is_i64 {
        emit_cmp_i64_locals(&mut f, 5, 6);
    } else {
        emit_cmp_i32_locals(&mut f, 5, 6, signed);
    }
    f.instruction(&Instruction::LocalSet(7));
    emit_return_if_nonzero(&mut f, 7);
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(4));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    // Equal prefix: the shorter sequence sorts first.
    emit_cmp_i32_locals(&mut f, 2, 3, false);
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_cmp_primitive(k_aver: &str, registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let mut f = Function::new([]);
    if k_aver == "Int" && registry.bignum {
        let cmp = registry.aint_cmp_fn_idx.ok_or(WasmGcError::Validation(
            "bignum active but __aint_cmp fn idx wasn't recorded on the registry".into(),
        ))?;
        f.instruction(&Instruction::LocalGet(0));
        f.instruction(&Instruction::LocalGet(1));
        f.instruction(&Instruction::Call(cmp));
        f.instruction(&Instruction::End);
        return Ok(f);
    }

    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    match k_aver {
        "Int" => f.instruction(&Instruction::I64LtS),
        "Bool" => f.instruction(&Instruction::I32LtS),
        _ => unreachable!("primitive order helper registered for `{k_aver}`"),
    };
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    match k_aver {
        "Int" => f.instruction(&Instruction::I64GtS),
        "Bool" => f.instruction(&Instruction::I32GtS),
        _ => unreachable!("primitive order helper registered for `{k_aver}`"),
    };
    f.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::Else);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Lexicographic UTF-8 byte comparison. UTF-8 preserves Unicode scalar-value
/// order, so byte lexicographic order is the language's String codepoint order
/// without decoding or allocating characters.
fn emit_cmp_string(registry: &TypeRegistry) -> Result<Function, WasmGcError> {
    let s_idx = super::string_idx(registry)?;
    let padding = wat_helper::padding_types(s_idx);
    let wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (func (export "helper") (param $a (ref null $string)) (param $b (ref null $string)) (result i32)
            (local $i i32)
            (local $n i32)
            (local $alen i32)
            (local $blen i32)
            local.get $a array.len local.set $alen
            local.get $b array.len local.set $blen
            local.get $alen local.get $blen i32.lt_u
            (if (result i32)
              (then local.get $alen)
              (else local.get $blen))
            local.set $n
            i32.const 0 local.set $i
            (block $equal_prefix
              (loop $next
                local.get $i local.get $n i32.ge_u br_if $equal_prefix
                local.get $a local.get $i array.get_u $string
                local.get $b local.get $i array.get_u $string
                i32.lt_u
                (if (then i32.const -1 return))
                local.get $a local.get $i array.get_u $string
                local.get $b local.get $i array.get_u $string
                i32.gt_u
                (if (then i32.const 1 return))
                local.get $i i32.const 1 i32.add local.set $i
                br $next))
            local.get $alen local.get $blen i32.lt_u
            (if (result i32)
              (then i32.const -1)
              (else
                local.get $alen local.get $blen i32.gt_u
                (if (result i32) (then i32.const 1) (else i32.const 0)))))
        )
    "#
    );
    wat_helper::compile_wat_helper(&wat)
}
