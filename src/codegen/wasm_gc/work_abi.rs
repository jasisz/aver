//! Versioned host scheduling boundary for pure jobs. Each worker instantiates
//! the same compiled module and calls only its job export. GC references stay
//! in that instance; the host transports the task and result as owned values.

use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, HeapType,
    ImportSection, Instruction, TypeSection, ValType,
};

use super::{
    WasmGcError,
    jobs::JobLowering,
    types::{TypeRegistry, aver_to_wasm, struct_ref},
};

pub const MODULE: &str = "aver:work/v1";
/// The four imports of [`MODULE`], in import-section order. The certificate
/// registry (`aver_cert::format::WASM_GC_CAPABILITIES`) lists exactly these.
pub const IMPORTS: [&str; 4] = ["submit", "take", "task", "complete"];
pub(super) const SUBMIT: &str = "__work_v1_submit";
pub(super) const TAKE: &str = "__work_v1_take";

#[derive(Default)]
pub(super) struct WorkImports {
    slots: Vec<(u32, u32)>,
}

impl WorkImports {
    pub(super) fn allocate(
        enabled: bool,
        first: u32,
        next: &mut u32,
        types: &mut TypeSection,
    ) -> Self {
        if !enabled {
            return Self::default();
        }
        let any = ValType::Ref(wasm_encoder::RefType::ANYREF);
        let mut slots = Vec::new();
        // submit/take carry a caller token; task/complete run only in workers.
        for (params, results) in [
            (vec![ValType::I32, any, ValType::I32], vec![any]),
            (vec![ValType::I32, any, ValType::I32], vec![any]),
            (vec![ValType::I32], vec![any]),
            (vec![ValType::I32, any], vec![]),
        ] {
            slots.push((first + slots.len() as u32, *next));
            *next += 1;
            types.ty().function(params, results);
        }
        Self { slots }
    }

    pub(super) fn count(&self) -> u32 {
        self.slots.len() as u32
    }

    pub(super) fn emit_imports(&self, imports: &mut ImportSection) {
        for (name, (_, ty)) in IMPORTS.iter().zip(&self.slots) {
            imports.import(MODULE, name, EntityType::Function(*ty));
        }
    }

    pub(super) fn function_indices(&self) -> impl Iterator<Item = (String, u32)> + '_ {
        [SUBMIT, TAKE]
            .iter()
            .zip(&self.slots)
            .map(|(name, (index, _))| (name.to_string(), *index))
    }
}

#[derive(Default)]
pub(super) struct WorkAbi {
    functions: Vec<(u32, u32)>,
}

impl WorkAbi {
    pub(super) fn allocate(
        kinds: usize,
        registry: &TypeRegistry,
        types: &mut TypeSection,
        next_type: &mut u32,
        next_fn: &mut u32,
    ) -> Result<Self, WasmGcError> {
        if kinds == 0 {
            return Ok(Self::default());
        }
        let result = registry
            .result_type_idx("Result<Work.Job,String>")
            .ok_or_else(|| {
                WasmGcError::Validation("host work needs Result<Work.Job,String>".into())
            })?;
        let string = registry
            .string_array_type_idx
            .ok_or_else(|| WasmGcError::Validation("host work needs String".into()))?;
        let mut signatures = vec![
            (vec![ValType::I64, ValType::I32], vec![struct_ref(result)]),
            (vec![struct_ref(string)], vec![struct_ref(result)]),
            (
                vec![ValType::Ref(wasm_encoder::RefType::ANYREF)],
                vec![ValType::I64],
            ),
        ];
        signatures.extend((0..kinds).map(|_| (vec![], vec![])));
        let mut functions = Vec::new();
        for (params, results) in signatures {
            types.ty().function(params, results);
            functions.push((*next_type, *next_fn));
            *next_type += 1;
            *next_fn += 1;
        }
        Ok(Self { functions })
    }

    pub(super) fn emit_function_entries(&self, section: &mut FunctionSection) {
        for (ty, _) in &self.functions {
            section.function(*ty);
        }
    }

    pub(super) fn emit_exports(&self, section: &mut ExportSection) {
        for (index, (_, function)) in self.functions.iter().enumerate() {
            let name = match index {
                0 => "__work_v1_started".into(),
                1 => "__work_v1_refused".into(),
                2 => "__work_v1_job_id".into(),
                _ => format!("__work_v1_run_{}", index - 3),
            };
            section.export(&name, ExportKind::Func, *function);
        }
    }

    pub(super) fn emit_bodies(
        &self,
        section: &mut CodeSection,
        registry: &TypeRegistry,
        jobs: Option<&JobLowering>,
        imports: &WorkImports,
    ) -> Result<(), WasmGcError> {
        if self.functions.is_empty() {
            return Ok(());
        }
        let jobs = jobs.expect("allocated host work has job lowering");
        let result = jobs.kinds[0].begin_result_idx;
        let string = registry.string_array_type_idx.expect("allocated String");
        let any = HeapType::Abstract {
            shared: false,
            ty: wasm_encoder::AbstractHeapType::Any,
        };
        let mut started = Function::new([]);
        for op in [
            Instruction::I32Const(super::types::RESULT_OK_TAG),
            Instruction::LocalGet(0),
            Instruction::I32Const(0),
            Instruction::LocalGet(1),
            Instruction::RefNull(any),
            Instruction::StructNew(jobs.job_struct_idx),
            Instruction::RefNull(HeapType::Concrete(string)),
            Instruction::StructNew(result),
            Instruction::End,
        ] {
            started.instruction(&op);
        }
        section.function(&started);
        let mut refused = Function::new([]);
        for op in [
            Instruction::I32Const(super::types::RESULT_ERR_TAG),
            Instruction::RefNull(HeapType::Concrete(jobs.job_struct_idx)),
            Instruction::LocalGet(0),
            Instruction::StructNew(result),
            Instruction::End,
        ] {
            refused.instruction(&op);
        }
        section.function(&refused);
        let mut id = Function::new([]);
        id.instruction(&Instruction::LocalGet(0));
        id.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
            jobs.job_struct_idx,
        )));
        id.instruction(&Instruction::StructGet {
            struct_type_index: jobs.job_struct_idx,
            field_index: 0,
        });
        id.instruction(&Instruction::End);
        section.function(&id);
        for kind in &jobs.kinds {
            let mut body = Function::new([]);
            body.instruction(&Instruction::I32Const(kind.kind_tag));
            body.instruction(&Instruction::I32Const(super::types::OPTION_SOME_TAG));
            body.instruction(&Instruction::I32Const(kind.kind_tag));
            body.instruction(&Instruction::Call(imports.slots[2].0));
            body.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
                kind.option_task_idx,
            )));
            if aver_to_wasm(&kind.task_aver, Some(registry))?.is_some() {
                body.instruction(&Instruction::StructGet {
                    struct_type_index: kind.option_task_idx,
                    field_index: 1,
                });
            } else {
                body.instruction(&Instruction::Drop);
            }
            body.instruction(&Instruction::Call(kind.work_fn_idx));
            if aver_to_wasm(&kind.payload_aver, Some(registry))?.is_none() {
                body.instruction(&Instruction::I32Const(0));
            }
            body.instruction(&Instruction::StructNew(kind.option_payload_idx));
            body.instruction(&Instruction::Call(imports.slots[3].0));
            body.instruction(&Instruction::End);
            section.function(&body);
        }
        Ok(())
    }
}
