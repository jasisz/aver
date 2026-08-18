//! Dynamic canonical-ABI imports for program-defined capabilities.
//!
//! Fixed WASI imports remain in `wasip2_imports.rs`. This registry is
//! plan-driven: every operation in a selected complete capability
//! contract gets one core import plus one internal GC↔canonical bridge.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, EntityType, Function, FunctionSection, ImportSection, Instruction, MemArg,
    RefType, TypeSection, ValType,
};

use crate::codegen::wasip2::{
    CapabilityWitOperationPlan, CapabilityWitParameterPlan, CapabilityWitPlan, CapabilityWitType,
};

use super::WasmGcError;
use super::body::CapabilityWitCallLowering;
use super::types::TypeRegistry;

#[derive(Debug, Clone)]
struct CapabilityImport {
    interface_name: String,
    operation: CapabilityWitOperationPlan,
    import_fn_idx: Option<u32>,
    import_type_idx: Option<u32>,
    helper_fn_idx: Option<u32>,
    helper_type_idx: Option<u32>,
}

#[derive(Debug, Clone, Default)]
pub(super) struct CapabilityImportRegistry {
    order: Vec<CapabilityImport>,
}

impl CapabilityImportRegistry {
    pub(super) fn from_plan(plan: Option<&CapabilityWitPlan>) -> Self {
        let mut order = Vec::new();
        for interface in plan.into_iter().flat_map(CapabilityWitPlan::interfaces) {
            for operation in &interface.operations {
                order.push(CapabilityImport {
                    interface_name: interface.interface_name.clone(),
                    operation: operation.clone(),
                    import_fn_idx: None,
                    import_type_idx: None,
                    helper_fn_idx: None,
                    helper_type_idx: None,
                });
            }
        }
        Self { order }
    }

    pub(super) fn import_count(&self) -> u32 {
        self.order.len() as u32
    }

    pub(super) fn assign_import_slots(
        &mut self,
        first_fn_idx: u32,
        next_type_idx: &mut u32,
        types: &mut TypeSection,
    ) {
        for (offset, import) in self.order.iter_mut().enumerate() {
            import.import_fn_idx = Some(first_fn_idx + offset as u32);
            import.import_type_idx = Some(*next_type_idx);
            *next_type_idx += 1;
            types.ty().function(
                canonical_params(&import.operation),
                canonical_results(import.operation.result),
            );
        }
    }

    pub(super) fn assign_helper_slots(
        &mut self,
        registry: &TypeRegistry,
        next_fn_idx: &mut u32,
        next_type_idx: &mut u32,
        types: &mut TypeSection,
    ) -> Result<(), WasmGcError> {
        for import in &mut self.order {
            import.helper_fn_idx = Some(*next_fn_idx);
            *next_fn_idx += 1;
            import.helper_type_idx = Some(*next_type_idx);
            *next_type_idx += 1;
            types.ty().function(
                aver_params(&import.operation.params, registry)?,
                aver_results(import.operation.result, registry)?,
            );
        }
        Ok(())
    }

    pub(super) fn emit_imports(&self, imports: &mut ImportSection) {
        for import in &self.order {
            imports.import(
                &core_import_module(&import.interface_name),
                &import.operation.wit_name,
                EntityType::Function(import.import_type_idx.expect("assigned import type")),
            );
        }
    }

    pub(super) fn emit_function_section(&self, functions: &mut FunctionSection) {
        for import in &self.order {
            functions.function(import.helper_type_idx.expect("assigned helper type"));
        }
    }

    pub(super) fn call_lowerings(&self) -> HashMap<String, CapabilityWitCallLowering> {
        self.order
            .iter()
            .map(|import| {
                (
                    import.operation.canonical_name.clone(),
                    CapabilityWitCallLowering {
                        helper_fn_idx: import.helper_fn_idx.expect("assigned helper fn"),
                        params: import.operation.params.clone(),
                        result: import.operation.result,
                    },
                )
            })
            .collect()
    }

    pub(super) fn emit_helper_bodies(
        &self,
        codes: &mut CodeSection,
        str_to_lm_fn_idx: Option<u32>,
        str_from_lm_fn_idx: Option<u32>,
        cabi_realloc_fn_idx: Option<u32>,
    ) -> Result<(), WasmGcError> {
        for import in &self.order {
            let function = emit_bridge_body(
                import,
                str_to_lm_fn_idx,
                str_from_lm_fn_idx,
                cabi_realloc_fn_idx,
            )?;
            codes.function(&function);
        }
        Ok(())
    }
}

fn core_import_module(interface_name: &str) -> String {
    format!("aver:user/{interface_name}")
}

fn canonical_params(operation: &CapabilityWitOperationPlan) -> Vec<ValType> {
    let mut params = Vec::new();
    for parameter in &operation.params {
        match parameter.ty {
            CapabilityWitType::Unit => {}
            CapabilityWitType::Bool => params.push(ValType::I32),
            CapabilityWitType::F64 => params.push(ValType::F64),
            CapabilityWitType::String => {
                params.push(ValType::I32);
                params.push(ValType::I32);
            }
        }
    }
    if operation.result == CapabilityWitType::String {
        params.push(ValType::I32);
    }
    params
}

fn canonical_results(result: CapabilityWitType) -> Vec<ValType> {
    match result {
        CapabilityWitType::Unit | CapabilityWitType::String => Vec::new(),
        CapabilityWitType::Bool => vec![ValType::I32],
        CapabilityWitType::F64 => vec![ValType::F64],
    }
}

fn aver_params(
    params: &[CapabilityWitParameterPlan],
    registry: &TypeRegistry,
) -> Result<Vec<ValType>, WasmGcError> {
    params
        .iter()
        .filter_map(|parameter| match parameter.ty {
            CapabilityWitType::Unit => None,
            ty => Some(aver_type(ty, registry)),
        })
        .collect()
}

fn aver_results(
    result: CapabilityWitType,
    registry: &TypeRegistry,
) -> Result<Vec<ValType>, WasmGcError> {
    match result {
        CapabilityWitType::Unit => Ok(Vec::new()),
        ty => Ok(vec![aver_type(ty, registry)?]),
    }
}

fn aver_type(ty: CapabilityWitType, registry: &TypeRegistry) -> Result<ValType, WasmGcError> {
    match ty {
        CapabilityWitType::Unit => Err(WasmGcError::Validation(
            "Unit must not occupy a wasm value slot".into(),
        )),
        CapabilityWitType::Bool => Ok(ValType::I32),
        CapabilityWitType::F64 => Ok(ValType::F64),
        CapabilityWitType::String => {
            let index = registry.string_array_type_idx.ok_or_else(|| {
                WasmGcError::Validation(
                    "custom capability String boundary needs the wasm-gc String slot".into(),
                )
            })?;
            Ok(ValType::Ref(RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(index),
            }))
        }
    }
}

fn emit_bridge_body(
    import: &CapabilityImport,
    str_to_lm_fn_idx: Option<u32>,
    str_from_lm_fn_idx: Option<u32>,
    cabi_realloc_fn_idx: Option<u32>,
) -> Result<Function, WasmGcError> {
    let string_count = import
        .operation
        .params
        .iter()
        .filter(|parameter| parameter.ty == CapabilityWitType::String)
        .count() as u32;
    let has_string_result = import.operation.result == CapabilityWitType::String;
    let local_count = string_count * 2 + u32::from(has_string_result);
    let mut function = if local_count == 0 {
        Function::new([])
    } else {
        Function::new([(local_count, ValType::I32)])
    };

    let wasm_param_count = import
        .operation
        .params
        .iter()
        .filter(|parameter| parameter.ty != CapabilityWitType::Unit)
        .count() as u32;
    let mut wasm_param_index = 0u32;
    let mut next_local = wasm_param_count;
    let mut lowered_string_locals = HashMap::new();
    for parameter in &import.operation.params {
        if parameter.ty == CapabilityWitType::Unit {
            continue;
        }
        if parameter.ty == CapabilityWitType::String {
            let to_lm = str_to_lm_fn_idx.ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "{} needs __rt_string_to_lm",
                    import.operation.canonical_name
                ))
            })?;
            let realloc = cabi_realloc_fn_idx.ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "{} needs cabi_realloc",
                    import.operation.canonical_name
                ))
            })?;
            let ptr_local = next_local;
            let len_local = next_local + 1;
            next_local += 2;

            function.instruction(&Instruction::LocalGet(wasm_param_index));
            function.instruction(&Instruction::Call(to_lm));
            function.instruction(&Instruction::LocalSet(len_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::LocalGet(len_local));
            function.instruction(&Instruction::Call(realloc));
            function.instruction(&Instruction::LocalSet(ptr_local));
            function.instruction(&Instruction::LocalGet(ptr_local));
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalGet(len_local));
            function.instruction(&Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
            lowered_string_locals.insert(parameter.index, (ptr_local, len_local));
        }
        wasm_param_index += 1;
    }

    let retptr_local = if has_string_result {
        let realloc = cabi_realloc_fn_idx.ok_or_else(|| {
            WasmGcError::Validation(format!(
                "{} String result needs cabi_realloc",
                import.operation.canonical_name
            ))
        })?;
        let local = next_local;
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(4));
        function.instruction(&Instruction::I32Const(8));
        function.instruction(&Instruction::Call(realloc));
        function.instruction(&Instruction::LocalSet(local));
        Some(local)
    } else {
        None
    };

    wasm_param_index = 0;
    for parameter in &import.operation.params {
        match parameter.ty {
            CapabilityWitType::Unit => {}
            CapabilityWitType::Bool | CapabilityWitType::F64 => {
                function.instruction(&Instruction::LocalGet(wasm_param_index));
                wasm_param_index += 1;
            }
            CapabilityWitType::String => {
                let (ptr, len) = lowered_string_locals[&parameter.index];
                function.instruction(&Instruction::LocalGet(ptr));
                function.instruction(&Instruction::LocalGet(len));
                wasm_param_index += 1;
            }
        }
    }
    if let Some(retptr) = retptr_local {
        function.instruction(&Instruction::LocalGet(retptr));
    }
    function.instruction(&Instruction::Call(
        import.import_fn_idx.expect("assigned import fn"),
    ));

    if let Some(retptr) = retptr_local {
        let from_lm = str_from_lm_fn_idx.ok_or_else(|| {
            WasmGcError::Validation(format!(
                "{} needs __rt_string_from_lm",
                import.operation.canonical_name
            ))
        })?;
        let load = |offset| MemArg {
            offset,
            align: 2,
            memory_index: 0,
        };
        // The shared bridge decoder reads LM[0..len]. Canonical imports
        // return an allocator-owned pointer, so copy that slice into the
        // transient bridge window before invoking the one-argument decoder.
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalGet(retptr));
        function.instruction(&Instruction::I32Load(load(0)));
        function.instruction(&Instruction::LocalGet(retptr));
        function.instruction(&Instruction::I32Load(load(4)));
        function.instruction(&Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        function.instruction(&Instruction::LocalGet(retptr));
        function.instruction(&Instruction::I32Load(load(4)));
        function.instruction(&Instruction::Call(from_lm));
    }
    function.instruction(&Instruction::End);
    Ok(function)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::capability::CapabilityRegistry;

    fn operation(
        params: &[CapabilityWitType],
        result: CapabilityWitType,
    ) -> CapabilityWitOperationPlan {
        CapabilityWitOperationPlan {
            canonical_name: "Probe.call".to_string(),
            wit_name: "op-n63616c6c".to_string(),
            effectful: false,
            params: params
                .iter()
                .enumerate()
                .map(|(index, ty)| CapabilityWitParameterPlan { index, ty: *ty })
                .collect(),
            result,
        }
    }

    #[test]
    fn canonical_flat_signatures_match_the_phase_3a_wit_subset() {
        let string_result = operation(
            &[
                CapabilityWitType::Unit,
                CapabilityWitType::Bool,
                CapabilityWitType::F64,
                CapabilityWitType::String,
            ],
            CapabilityWitType::String,
        );
        assert_eq!(
            canonical_params(&string_result),
            [
                ValType::I32,
                ValType::F64,
                ValType::I32,
                ValType::I32,
                ValType::I32,
            ]
        );
        assert!(canonical_results(string_result.result).is_empty());
        assert_eq!(canonical_results(CapabilityWitType::Bool), [ValType::I32]);
        assert_eq!(canonical_results(CapabilityWitType::F64), [ValType::F64]);
        assert!(canonical_results(CapabilityWitType::Unit).is_empty());
    }

    #[test]
    fn registry_uses_the_exact_plan_names_and_full_sorted_contract() {
        let source = "\
module Echo
    kind = capability
    semantics = effectful
    exposes [healthy, echo]

operation healthy() -> Bool
    oracle = generative
    replay = recorded

operation echo(value: String) -> String
    oracle = generative
    replay = recorded
";
        let items = crate::source::parse_source(source).expect("parse Echo");
        let (contracts, errors) = CapabilityRegistry::from_module("Echo", &items);
        assert!(errors.is_empty(), "capability errors: {errors:?}");
        let required = ["Echo.echo".to_string()]
            .into_iter()
            .collect::<BTreeSet<_>>();
        let plan = CapabilityWitPlan::build(&contracts, &required).expect("WIT plan");
        let imports = CapabilityImportRegistry::from_plan(Some(&plan));

        assert_eq!(imports.order.len(), 2);
        assert_eq!(imports.order[0].operation.canonical_name, "Echo.echo");
        assert_eq!(imports.order[0].operation.wit_name, "op-n6563686f");
        assert_eq!(imports.order[1].operation.canonical_name, "Echo.healthy");
        assert_eq!(imports.order[1].operation.wit_name, "op-n6865616c746879");
        assert_eq!(
            core_import_module(&imports.order[0].interface_name),
            format!("aver:user/{}", plan.interfaces()[0].interface_name)
        );
    }
}
