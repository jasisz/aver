//! JS-facing construction and inspection helpers for custom capability ABI.
//!
//! JavaScript can pass scalar values and `externref` resources directly, but
//! cannot construct wasm-gc structs or arrays. These exports keep all guest
//! values wasm-owned while making every contract boundary shape usable by an
//! external browser/Node/Workers host.

use std::collections::{BTreeMap, HashSet};

use wasm_encoder::{
    BlockType, CodeSection, ExportKind, ExportSection, Function, FunctionSection, HeapType,
    Instruction, TypeSection, ValType,
};

use crate::ast::Type;

use super::CapabilityWasmGcPlan;
use super::WasmGcError;
use super::maps::MapKVHelpers;
use super::packed_sequences::PackedSequenceOps;
use super::types::{TypeRegistry, aver_to_wasm};

#[derive(Debug, Clone)]
enum HelperKind {
    ResultMake {
        type_idx: u32,
        ok: bool,
        active: Option<ValType>,
        inactive: ValType,
    },
    StructField {
        type_idx: u32,
        field: u32,
    },
    StructMake {
        type_idx: u32,
        fields: Vec<Option<ValType>>,
    },
    Tag {
        type_idx: u32,
    },
    OptionSome {
        type_idx: u32,
        value: Option<ValType>,
    },
    OptionNone {
        type_idx: u32,
        value: ValType,
    },
    ListCons {
        type_idx: u32,
        value: Option<ValType>,
    },
    ListNil {
        type_idx: u32,
    },
    ListIsEmpty,
    VectorNew {
        type_idx: u32,
    },
    VectorLen,
    VectorGet {
        type_idx: u32,
        value: Option<ValType>,
    },
    VectorSet {
        type_idx: u32,
        value: Option<ValType>,
    },
    SumKind {
        variants: Vec<u32>,
    },
    SumMake {
        variant_idx: u32,
        fields: Vec<Option<ValType>>,
    },
    SumField {
        variant_idx: u32,
        field: u32,
    },
}

#[derive(Debug, Clone)]
struct Helper {
    export: String,
    type_idx: u32,
    fn_idx: u32,
    kind: HelperKind,
}

#[derive(Debug, Clone, Default)]
pub(super) struct CapabilityAbi {
    helpers: Vec<Helper>,
    aliases: Vec<(String, u32)>,
}

#[derive(Debug, Clone, Copy)]
pub(super) struct IntAbiHelpers {
    pub(super) from_i64: u32,
    pub(super) to_i64_checked: u32,
    pub(super) from_decimal: u32,
    pub(super) to_decimal: u32,
}

pub(super) struct CollectionAbiHelpers<'a> {
    pub(super) maps: &'a dyn Fn(&str) -> Option<MapKVHelpers>,
    pub(super) packed_sequences: &'a dyn Fn(&str) -> Option<PackedSequenceOps>,
}

impl CapabilityAbi {
    pub(super) fn allocate(
        plan: Option<&CapabilityWasmGcPlan>,
        registry: &TypeRegistry,
        int_helpers: Option<IntAbiHelpers>,
        collection_helpers: &CollectionAbiHelpers<'_>,
        types: &mut TypeSection,
        next_type_idx: &mut u32,
        next_fn_idx: &mut u32,
    ) -> Result<Self, WasmGcError> {
        let Some(plan) = plan else {
            return Ok(Self::default());
        };
        let mut boundary = BTreeMap::<String, Type>::new();
        for interface in plan.interfaces() {
            for operation in &interface.operations {
                for ty in &operation.abi_params {
                    collect_type(ty, registry, &mut boundary, &mut HashSet::new());
                }
                collect_type(
                    &operation.abi_result,
                    registry,
                    &mut boundary,
                    &mut HashSet::new(),
                );
            }
        }

        let mut abi = Self::default();
        for ty in boundary.values() {
            abi.allocate_type(
                ty,
                registry,
                int_helpers,
                collection_helpers,
                types,
                next_type_idx,
                next_fn_idx,
            )?;
        }
        Ok(abi)
    }

    #[allow(clippy::too_many_arguments)]
    fn allocate_type(
        &mut self,
        ty: &Type,
        registry: &TypeRegistry,
        int_helpers: Option<IntAbiHelpers>,
        collection_helpers: &CollectionAbiHelpers<'_>,
        types: &mut TypeSection,
        next_type_idx: &mut u32,
        next_fn_idx: &mut u32,
    ) -> Result<(), WasmGcError> {
        let canonical = ty.display();
        let stem = helper_stem(&canonical);
        let value = |ty: &Type| aver_to_wasm(&ty.display(), Some(registry));
        let mut push =
            |export: String, params: Vec<ValType>, results: Vec<ValType>, kind: HelperKind| {
                types.ty().function(params, results);
                self.helpers.push(Helper {
                    export,
                    type_idx: *next_type_idx,
                    fn_idx: *next_fn_idx,
                    kind,
                });
                *next_type_idx += 1;
                *next_fn_idx += 1;
            };

        match ty {
            Type::Int => {
                let helpers = int_helpers.ok_or_else(|| {
                    WasmGcError::Validation(
                        "capability ABI carries Int without full-ℤ host bridges".into(),
                    )
                })?;
                for (suffix, function) in [
                    ("from_i64", helpers.from_i64),
                    ("to_i64_checked", helpers.to_i64_checked),
                    ("from_decimal", helpers.from_decimal),
                    ("to_decimal", helpers.to_decimal),
                ] {
                    self.aliases.push((format!("{stem}_{suffix}"), function));
                }
            }
            Type::Float | Type::Str | Type::Bool | Type::Unit => {}
            Type::Result(ok, err) => {
                let type_idx = registry.result_type_idx(&canonical).ok_or_else(|| {
                    WasmGcError::Validation(format!("capability ABI lacks `{canonical}` slot"))
                })?;
                let result = value(ty)?.expect("Result has a wasm value");
                let ok_value = value(ok)?;
                let err_value = value(err)?;
                push(
                    format!("{stem}_ok"),
                    ok_value.into_iter().collect(),
                    vec![result],
                    HelperKind::ResultMake {
                        type_idx,
                        ok: true,
                        active: ok_value,
                        inactive: err_value.unwrap_or(ValType::I32),
                    },
                );
                push(
                    format!("{stem}_err"),
                    err_value.into_iter().collect(),
                    vec![result],
                    HelperKind::ResultMake {
                        type_idx,
                        ok: false,
                        active: err_value,
                        inactive: ok_value.unwrap_or(ValType::I32),
                    },
                );
                push(
                    format!("{stem}_tag"),
                    vec![result],
                    vec![ValType::I32],
                    HelperKind::Tag { type_idx },
                );
                if let Some(ok_value) = ok_value {
                    push(
                        format!("{stem}_ok_value"),
                        vec![result],
                        vec![ok_value],
                        HelperKind::StructField { type_idx, field: 1 },
                    );
                }
                if let Some(err_value) = err_value {
                    push(
                        format!("{stem}_err_value"),
                        vec![result],
                        vec![err_value],
                        HelperKind::StructField { type_idx, field: 2 },
                    );
                }
            }
            Type::Option(inner) => {
                let type_idx = registry.option_type_idx(&canonical).ok_or_else(|| {
                    WasmGcError::Validation(format!("capability ABI lacks `{canonical}` slot"))
                })?;
                let option = value(ty)?.expect("Option has a wasm value");
                let inner = value(inner)?;
                push(
                    format!("{stem}_some"),
                    inner.into_iter().collect(),
                    vec![option],
                    HelperKind::OptionSome {
                        type_idx,
                        value: inner,
                    },
                );
                push(
                    format!("{stem}_none"),
                    vec![],
                    vec![option],
                    HelperKind::OptionNone {
                        type_idx,
                        value: inner.unwrap_or(ValType::I32),
                    },
                );
                push(
                    format!("{stem}_tag"),
                    vec![option],
                    vec![ValType::I32],
                    HelperKind::Tag { type_idx },
                );
                if let Some(inner) = inner {
                    push(
                        format!("{stem}_value"),
                        vec![option],
                        vec![inner],
                        HelperKind::StructField { type_idx, field: 1 },
                    );
                }
            }
            Type::List(inner) => {
                let type_idx = registry.list_type_idx(&canonical).ok_or_else(|| {
                    WasmGcError::Validation(format!("capability ABI lacks `{canonical}` slot"))
                })?;
                let list = value(ty)?.expect("List has a wasm value");
                let inner = value(inner)?;
                let mut cons_params = inner.into_iter().collect::<Vec<_>>();
                cons_params.push(list);
                push(
                    format!("{stem}_cons"),
                    cons_params,
                    vec![list],
                    HelperKind::ListCons {
                        type_idx,
                        value: inner,
                    },
                );
                push(
                    format!("{stem}_nil"),
                    vec![],
                    vec![list],
                    HelperKind::ListNil { type_idx },
                );
                push(
                    format!("{stem}_is_empty"),
                    vec![list],
                    vec![ValType::I32],
                    HelperKind::ListIsEmpty,
                );
                if let Some(inner) = inner {
                    push(
                        format!("{stem}_head"),
                        vec![list],
                        vec![inner],
                        HelperKind::StructField { type_idx, field: 0 },
                    );
                }
                push(
                    format!("{stem}_tail"),
                    vec![list],
                    vec![list],
                    HelperKind::StructField { type_idx, field: 1 },
                );
            }
            Type::Tuple(items) => {
                let type_idx = registry.tuple_type_idx(&canonical).ok_or_else(|| {
                    WasmGcError::Validation(format!("capability ABI lacks `{canonical}` slot"))
                })?;
                let tuple = value(ty)?.expect("Tuple has a wasm value");
                let fields = items.iter().map(value).collect::<Result<Vec<_>, _>>()?;
                push(
                    format!("{stem}_make"),
                    fields.iter().flatten().copied().collect(),
                    vec![tuple],
                    HelperKind::StructMake {
                        type_idx,
                        fields: fields.clone(),
                    },
                );
                for (field, field_ty) in fields.into_iter().enumerate() {
                    if let Some(field_ty) = field_ty {
                        push(
                            format!("{stem}_field_{field}"),
                            vec![tuple],
                            vec![field_ty],
                            HelperKind::StructField {
                                type_idx,
                                field: field as u32,
                            },
                        );
                    }
                }
            }
            Type::Vector(inner) => {
                let type_idx = *registry
                    .vector_types
                    .get(&canonical.replace(' ', ""))
                    .ok_or_else(|| {
                        WasmGcError::Validation(format!("capability ABI lacks `{canonical}` slot"))
                    })?;
                let vector = value(ty)?.expect("Vector has a wasm value");
                let inner = value(inner)?;
                push(
                    format!("{stem}_new"),
                    vec![ValType::I32],
                    vec![vector],
                    HelperKind::VectorNew { type_idx },
                );
                push(
                    format!("{stem}_len"),
                    vec![vector],
                    vec![ValType::I32],
                    HelperKind::VectorLen,
                );
                push(
                    format!("{stem}_get"),
                    vec![vector, ValType::I32],
                    inner.into_iter().collect(),
                    HelperKind::VectorGet {
                        type_idx,
                        value: inner,
                    },
                );
                let mut set_params = vec![vector, ValType::I32];
                set_params.extend(inner);
                push(
                    format!("{stem}_set"),
                    set_params,
                    vec![],
                    HelperKind::VectorSet {
                        type_idx,
                        value: inner,
                    },
                );
            }
            Type::Map(_, _) => {
                // The public Type display keeps a space after commas while the
                // wasm-gc map monomorphisation registry uses its compact
                // canonical spelling. Both denote the same boundary type.
                let compact = canonical.replace(' ', "");
                let helpers = (collection_helpers.maps)(&compact).ok_or_else(|| {
                    WasmGcError::Validation(format!("capability ABI lacks `{canonical}` helpers"))
                })?;
                for (suffix, fn_idx) in [
                    ("empty", helpers.empty),
                    ("set", helpers.set),
                    ("get", helpers.get),
                    ("len", helpers.len),
                ] {
                    self.aliases.push((format!("{stem}_{suffix}"), fn_idx));
                }
            }
            // backend-link-stage: ABI types are already contract-qualified and
            // resolved against the post-flatten TypeRegistry by canonical name.
            Type::Named { name, .. } => {
                if registry.is_capability_resource(name) {
                    return Ok(());
                }
                if registry.packed_sequence(name).is_some() {
                    let helpers = (collection_helpers.packed_sequences)(name).ok_or_else(|| {
                        WasmGcError::Validation(format!(
                            "capability ABI lacks packed `{name}` helpers"
                        ))
                    })?;
                    let fields = registry
                        .record_fields
                        .get(registry.canonical_type_name(name))
                        .or_else(|| {
                            name.rsplit_once('.')
                                .and_then(|(_, bare)| registry.record_fields.get(bare))
                        })
                        .ok_or_else(|| {
                            WasmGcError::Validation(format!(
                                "capability ABI lacks packed `{name}` fields"
                            ))
                        })?;
                    let [(field_name, field_type)] = fields.as_slice() else {
                        return Err(WasmGcError::Validation(format!(
                            "capability ABI packed `{name}` must have one carrier field"
                        )));
                    };
                    if field_type.replace(' ', "") != "List<Int>" {
                        return Err(WasmGcError::Validation(format!(
                            "capability ABI packed `{name}` carrier must be List<Int>"
                        )));
                    }
                    self.aliases.push((format!("{stem}_make"), helpers.pack));
                    self.aliases.push((
                        format!(
                            "{stem}_field_{}",
                            crate::codegen::wasip2::plan::encode_interface_identifier(field_name)
                        ),
                        helpers.unpack,
                    ));
                } else if let Some(type_idx) = registry.record_type_idx(name) {
                    let fields = registry
                        .record_fields
                        .get(name)
                        .or_else(|| {
                            name.rsplit_once('.')
                                .and_then(|(_, bare)| registry.record_fields.get(bare))
                        })
                        .ok_or_else(|| {
                            WasmGcError::Validation(format!("capability ABI lacks `{name}` fields"))
                        })?;
                    let record = value(ty)?.expect("record has a wasm value");
                    let field_values = fields
                        .iter()
                        .map(|(_, field)| aver_to_wasm(field, Some(registry)))
                        .collect::<Result<Vec<_>, _>>()?;
                    push(
                        format!("{stem}_make"),
                        field_values.iter().flatten().copied().collect(),
                        vec![record],
                        HelperKind::StructMake {
                            type_idx,
                            fields: field_values.clone(),
                        },
                    );
                    for (field, ((field_name, _), field_ty)) in
                        fields.iter().zip(field_values).enumerate()
                    {
                        if let Some(field_ty) = field_ty {
                            push(
                                format!(
                                    "{stem}_field_{}",
                                    crate::codegen::wasip2::plan::encode_interface_identifier(
                                        field_name
                                    )
                                ),
                                vec![record],
                                vec![field_ty],
                                HelperKind::StructField {
                                    type_idx,
                                    field: field as u32,
                                },
                            );
                        }
                    }
                } else if let Some(root_idx) = registry.sum_root_type_idx(name) {
                    let root = value(ty)?.expect("sum has a wasm value");
                    let mut variants = registry
                        .variants
                        .values()
                        .flatten()
                        .filter(|variant| {
                            variant.parent == *name
                                || name
                                    .rsplit_once('.')
                                    .is_some_and(|(_, bare)| variant.parent == bare)
                        })
                        .cloned()
                        .collect::<Vec<_>>();
                    variants.sort_by_key(|variant| variant.type_idx);
                    push(
                        format!("{stem}_kind"),
                        vec![root],
                        vec![ValType::I32],
                        HelperKind::SumKind {
                            variants: variants.iter().map(|variant| variant.type_idx).collect(),
                        },
                    );
                    for variant in variants {
                        let fields = variant
                            .fields
                            .iter()
                            .map(|field| aver_to_wasm(field, Some(registry)))
                            .collect::<Result<Vec<_>, _>>()?;
                        let variant_name = registry
                            .variants
                            .iter()
                            .find_map(|(name, candidates)| {
                                candidates
                                    .iter()
                                    .any(|candidate| candidate.type_idx == variant.type_idx)
                                    .then_some(name)
                            })
                            .expect("variant is indexed by name");
                        let variant_stem = format!(
                            "{stem}_variant_{}",
                            crate::codegen::wasip2::plan::encode_interface_identifier(variant_name)
                        );
                        push(
                            format!("{variant_stem}_make"),
                            fields.iter().flatten().copied().collect(),
                            vec![root],
                            HelperKind::SumMake {
                                variant_idx: variant.type_idx,
                                fields: fields.clone(),
                            },
                        );
                        for (field, field_ty) in fields.into_iter().enumerate() {
                            if let Some(field_ty) = field_ty {
                                push(
                                    format!("{variant_stem}_field_{field}"),
                                    vec![root],
                                    vec![field_ty],
                                    HelperKind::SumField {
                                        variant_idx: variant.type_idx,
                                        field: field as u32,
                                    },
                                );
                            }
                        }
                    }
                    let _ = root_idx;
                } else {
                    return Err(WasmGcError::Validation(format!(
                        "capability ABI cannot resolve named type `{name}`"
                    )));
                }
            }
            Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => {
                return Err(WasmGcError::Validation(format!(
                    "capability ABI cannot lower `{canonical}`"
                )));
            }
        }
        Ok(())
    }

    pub(super) fn emit_function_entries(&self, functions: &mut FunctionSection) {
        for helper in &self.helpers {
            functions.function(helper.type_idx);
        }
    }

    pub(super) fn emit_exports(&self, exports: &mut ExportSection) {
        for helper in &self.helpers {
            exports.export(&helper.export, ExportKind::Func, helper.fn_idx);
        }
        for (name, function) in &self.aliases {
            exports.export(name, ExportKind::Func, *function);
        }
    }

    pub(super) fn emit_bodies(&self, codes: &mut CodeSection) {
        for helper in &self.helpers {
            let mut function = Function::new([]);
            emit_helper(&mut function, &helper.kind);
            function.instruction(&Instruction::End);
            codes.function(&function);
        }
    }
}

fn helper_stem(canonical: &str) -> String {
    format!(
        "__cap_abi_{}",
        crate::codegen::wasip2::plan::encode_interface_identifier(canonical)
    )
}

fn collect_type(
    ty: &Type,
    registry: &TypeRegistry,
    out: &mut BTreeMap<String, Type>,
    visiting: &mut HashSet<String>,
) {
    let canonical = ty.display();
    if out.insert(canonical, ty.clone()).is_some() {
        return;
    }
    match ty {
        Type::Result(left, right) | Type::Map(left, right) => {
            collect_type(left, registry, out, visiting);
            collect_type(right, registry, out, visiting);
        }
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            collect_type(inner, registry, out, visiting)
        }
        Type::Tuple(items) | Type::Fn(items, _, _) => {
            for item in items {
                collect_type(item, registry, out, visiting);
            }
            if let Type::Fn(_, result, _) = ty {
                collect_type(result, registry, out, visiting);
            }
        }
        // backend-link-stage: walk represented definitions in the linked
        // registry to allocate host factories for their transitive fields.
        Type::Named { name, .. } if visiting.insert(name.clone()) => {
            if let Some(fields) = registry.record_fields.get(name).or_else(|| {
                name.rsplit_once('.')
                    .and_then(|(_, bare)| registry.record_fields.get(bare))
            }) {
                for (_, field) in fields {
                    collect_type(
                        &crate::types::parse_type_str(field),
                        registry,
                        out,
                        visiting,
                    );
                }
            }
            for variant in registry.variants.values().flatten().filter(|variant| {
                variant.parent == *name
                    || name
                        .rsplit_once('.')
                        .is_some_and(|(_, bare)| variant.parent == bare)
            }) {
                for field in &variant.fields {
                    collect_type(
                        &crate::types::parse_type_str(field),
                        registry,
                        out,
                        visiting,
                    );
                }
            }
            visiting.remove(name);
        }
        _ => {}
    }
}

fn emit_default(function: &mut Function, ty: ValType) {
    match ty {
        ValType::I32 => function.instruction(&Instruction::I32Const(0)),
        ValType::I64 => function.instruction(&Instruction::I64Const(0)),
        ValType::F32 => function.instruction(&Instruction::F32Const(0.0.into())),
        ValType::F64 => function.instruction(&Instruction::F64Const(0.0.into())),
        ValType::V128 => function.instruction(&Instruction::V128Const(0)),
        ValType::Ref(reference) => function.instruction(&Instruction::RefNull(reference.heap_type)),
    };
}

fn emit_fields(function: &mut Function, fields: &[Option<ValType>]) {
    let mut source = 0u32;
    for field in fields {
        if field.is_some() {
            function.instruction(&Instruction::LocalGet(source));
            source += 1;
        } else {
            function.instruction(&Instruction::I32Const(0));
        }
    }
}

fn emit_helper(function: &mut Function, kind: &HelperKind) {
    match kind {
        HelperKind::ResultMake {
            type_idx,
            ok,
            active,
            inactive,
        } => {
            function.instruction(&Instruction::I32Const(i32::from(*ok)));
            if *ok {
                if active.is_some() {
                    function.instruction(&Instruction::LocalGet(0));
                } else {
                    function.instruction(&Instruction::I32Const(0));
                }
                emit_default(function, *inactive);
            } else {
                emit_default(function, *inactive);
                if active.is_some() {
                    function.instruction(&Instruction::LocalGet(0));
                } else {
                    function.instruction(&Instruction::I32Const(0));
                }
            }
            function.instruction(&Instruction::StructNew(*type_idx));
        }
        HelperKind::StructField { type_idx, field } => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::StructGet {
                struct_type_index: *type_idx,
                field_index: *field,
            });
        }
        HelperKind::StructMake { type_idx, fields } => {
            emit_fields(function, fields);
            function.instruction(&Instruction::StructNew(*type_idx));
        }
        HelperKind::Tag { type_idx } => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::StructGet {
                struct_type_index: *type_idx,
                field_index: 0,
            });
        }
        HelperKind::OptionSome { type_idx, value } => {
            function.instruction(&Instruction::I32Const(1));
            if value.is_some() {
                function.instruction(&Instruction::LocalGet(0));
            } else {
                function.instruction(&Instruction::I32Const(0));
            }
            function.instruction(&Instruction::StructNew(*type_idx));
        }
        HelperKind::OptionNone { type_idx, value } => {
            function.instruction(&Instruction::I32Const(0));
            emit_default(function, *value);
            function.instruction(&Instruction::StructNew(*type_idx));
        }
        HelperKind::ListCons { type_idx, value } => {
            if value.is_some() {
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::LocalGet(1));
            } else {
                function.instruction(&Instruction::I32Const(0));
                function.instruction(&Instruction::LocalGet(0));
            }
            function.instruction(&Instruction::StructNew(*type_idx));
        }
        HelperKind::ListNil { type_idx } => {
            function.instruction(&Instruction::RefNull(HeapType::Concrete(*type_idx)));
        }
        HelperKind::ListIsEmpty => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::RefIsNull);
        }
        HelperKind::VectorNew { type_idx } => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::ArrayNewDefault(*type_idx));
        }
        HelperKind::VectorLen => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::ArrayLen);
        }
        HelperKind::VectorGet { type_idx, value } => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::LocalGet(1));
            function.instruction(&Instruction::ArrayGet(*type_idx));
            if value.is_none() {
                function.instruction(&Instruction::Drop);
            }
        }
        HelperKind::VectorSet { type_idx, value } => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::LocalGet(1));
            if value.is_some() {
                function.instruction(&Instruction::LocalGet(2));
            } else {
                function.instruction(&Instruction::I32Const(0));
            }
            function.instruction(&Instruction::ArraySet(*type_idx));
        }
        HelperKind::SumKind { variants } => {
            for (tag, variant) in variants.iter().enumerate() {
                function.instruction(&Instruction::LocalGet(0));
                function.instruction(&Instruction::RefTestNonNull(HeapType::Concrete(*variant)));
                function.instruction(&Instruction::If(BlockType::Empty));
                function.instruction(&Instruction::I32Const(tag as i32));
                function.instruction(&Instruction::Return);
                function.instruction(&Instruction::End);
            }
            function.instruction(&Instruction::Unreachable);
        }
        HelperKind::SumMake {
            variant_idx,
            fields,
        } => {
            emit_fields(function, fields);
            function.instruction(&Instruction::StructNew(*variant_idx));
        }
        HelperKind::SumField { variant_idx, field } => {
            function.instruction(&Instruction::LocalGet(0));
            function.instruction(&Instruction::RefCastNonNull(HeapType::Concrete(
                *variant_idx,
            )));
            function.instruction(&Instruction::StructGet {
                struct_type_index: *variant_idx,
                field_index: *field,
            });
        }
    };
}
