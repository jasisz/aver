/// WASM module builder using wasm-encoder.
///
/// Constructs a complete WASM module from the Aver CodegenContext.
/// Pure programs have zero imports — arithmetic uses native WASM instructions.
use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, FunctionSection, Instruction, Module, TypeSection,
};

use crate::ast::{FnDef, TopLevel};
use crate::codegen::CodegenContext;

use super::expr::ExprEmitter;
use super::types::AVER_WASM_TYPE;

/// Build a complete WASM module from the Aver codegen context.
pub fn build_wasm_module(ctx: &CodegenContext) -> Result<Vec<u8>, String> {
    let mut module = Module::new();

    // Collect user-defined functions
    let fn_defs: Vec<&FnDef> = ctx
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(fd)
            } else {
                None
            }
        })
        .collect();

    // -----------------------------------------------------------------------
    // Type section: function signatures
    // -----------------------------------------------------------------------
    let mut type_section = TypeSection::new();
    let mut fn_type_indices: HashMap<String, u32> = HashMap::new();

    for (i, fd) in fn_defs.iter().enumerate() {
        let param_types = vec![AVER_WASM_TYPE; fd.params.len()];
        let result_types = vec![AVER_WASM_TYPE]; // every function returns i64
        type_section.ty().function(param_types, result_types);
        fn_type_indices.insert(fd.name.clone(), i as u32);
    }

    module.section(&type_section);

    // -----------------------------------------------------------------------
    // Function section: declare user-defined functions
    // -----------------------------------------------------------------------
    let mut function_section = FunctionSection::new();
    let mut fn_indices: HashMap<String, u32> = HashMap::new();

    for (i, fd) in fn_defs.iter().enumerate() {
        let type_idx = fn_type_indices[&fd.name];
        function_section.function(type_idx);
        // No imports, so function index == position in fn_defs
        fn_indices.insert(fd.name.clone(), i as u32);
    }

    module.section(&function_section);

    // -----------------------------------------------------------------------
    // Export section
    // -----------------------------------------------------------------------
    let mut export_section = ExportSection::new();

    if let Some(&main_idx) = fn_indices.get("main") {
        export_section.export("main", ExportKind::Func, main_idx);
        export_section.export("_start", ExportKind::Func, main_idx);
    }

    module.section(&export_section);

    // -----------------------------------------------------------------------
    // Code section: function bodies
    // -----------------------------------------------------------------------
    let mut code_section = CodeSection::new();

    for fd in &fn_defs {
        let mut emitter = ExprEmitter::new(&fn_indices);
        emitter.add_params(&fd.params);
        emitter.emit_body(&fd.body);

        let num_params = fd.params.len() as u32;
        let num_extra_locals = emitter.next_local - num_params;

        let locals: Vec<(u32, wasm_encoder::ValType)> = if num_extra_locals > 0 {
            vec![(num_extra_locals, AVER_WASM_TYPE)]
        } else {
            vec![]
        };

        let mut func = wasm_encoder::Function::new(locals);
        for instr in &emitter.instructions {
            func.instruction(instr);
        }
        func.instruction(&Instruction::End);

        code_section.function(&func);
    }

    module.section(&code_section);

    Ok(module.finish())
}
