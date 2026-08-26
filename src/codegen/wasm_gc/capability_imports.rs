//! Raw wasm-gc imports for program-defined capability contracts.
//!
//! Every operation is imported directly with its native wasm-gc value shape.
//! The final `i32` parameter is Aver's caller-site token, matching the fixed
//! `aver/*` host ABI. It is transport metadata, not a source argument.

use std::collections::HashMap;

use wasm_encoder::{EntityType, ImportSection, TypeSection, ValType};

use super::CapabilityWasmGcPlan;
use super::WasmGcError;
use super::types::{TypeRegistry, aver_to_wasm};

#[derive(Debug, Clone)]
struct CapabilityImport {
    module_name: String,
    field_name: String,
    canonical_name: String,
    params: Vec<crate::ast::Type>,
    result: crate::ast::Type,
    function_index: Option<u32>,
    type_index: Option<u32>,
}

#[derive(Debug, Clone, Default)]
pub(super) struct CapabilityImportRegistry {
    order: Vec<CapabilityImport>,
}

impl CapabilityImportRegistry {
    pub(super) fn from_plan(plan: Option<&CapabilityWasmGcPlan>) -> Self {
        let mut order = Vec::new();
        for interface in plan.into_iter().flat_map(CapabilityWasmGcPlan::interfaces) {
            let module_name = format!("aver:user/{}", interface.interface_name);
            for operation in &interface.operations {
                order.push(CapabilityImport {
                    module_name: module_name.clone(),
                    field_name: operation.import_name.clone(),
                    canonical_name: operation.operation.canonical_name.clone(),
                    params: operation.abi_params.clone(),
                    result: operation.abi_result.clone(),
                    function_index: None,
                    type_index: None,
                });
            }
        }
        Self { order }
    }

    pub(super) fn import_count(&self) -> u32 {
        self.order.len() as u32
    }

    pub(super) fn assign_slots(
        &mut self,
        first_function_index: u32,
        next_type_index: &mut u32,
        types: &mut TypeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        for (offset, import) in self.order.iter_mut().enumerate() {
            import.function_index = Some(first_function_index + offset as u32);
            import.type_index = Some(*next_type_index);
            *next_type_index += 1;

            let mut params = import
                .params
                .iter()
                .map(|ty| aver_to_wasm(&ty.display(), Some(registry)))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect::<Vec<_>>();
            params.push(ValType::I32);
            let results = aver_to_wasm(&import.result.display(), Some(registry))?
                .into_iter()
                .collect::<Vec<_>>();
            types.ty().function(params, results);
        }
        Ok(())
    }

    pub(super) fn emit_imports(&self, imports: &mut ImportSection) {
        for import in &self.order {
            imports.import(
                &import.module_name,
                &import.field_name,
                EntityType::Function(import.type_index.expect("assigned capability type")),
            );
        }
    }

    pub(super) fn function_indices(&self) -> HashMap<String, u32> {
        self.order
            .iter()
            .map(|import| {
                (
                    import.canonical_name.clone(),
                    import.function_index.expect("assigned capability function"),
                )
            })
            .collect()
    }

    #[cfg(test)]
    pub(super) fn import_pairs(&self) -> Vec<(String, String)> {
        self.order
            .iter()
            .map(|import| (import.module_name.clone(), import.field_name.clone()))
            .collect()
    }
}
