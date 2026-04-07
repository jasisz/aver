/// WASM module builder using wasm-encoder.
///
/// Constructs a complete WASM module with:
/// - Runtime functions (bump allocator, tagged arithmetic, object helpers)
/// - User-defined functions
/// - Memory section with globals ($heap_ptr)
/// - Export section (main, _start, memory)
use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, ConstExpr, ExportKind, ExportSection, FunctionSection, GlobalSection, GlobalType,
    Instruction, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::ast::{FnDef, TopLevel};
use crate::codegen::CodegenContext;

use super::expr::ExprEmitter;
use super::runtime::{self, RuntimeFuncIndices};
use super::types::AVER_WASM_TYPE;

/// Initial heap base offset (after static data). Aligned to 8 bytes.
const HEAP_BASE: i32 = 1024;

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

    // Runtime function indices (start at 0, before user functions)
    let rt = RuntimeFuncIndices::new(0);
    let user_fn_base = rt.count;

    // -----------------------------------------------------------------------
    // Type section: function signatures
    // -----------------------------------------------------------------------
    let mut type_section = TypeSection::new();

    // Runtime function types:
    // Type 0: (i32) -> i32  — $alloc
    type_section
        .ty()
        .function(vec![ValType::I32], vec![ValType::I32]);
    // Type 1: (i64, i64) -> i64  — binops, list_cons
    type_section
        .ty()
        .function(vec![AVER_WASM_TYPE; 2], vec![AVER_WASM_TYPE]);
    // Type 2: (i32, i64) -> i64  — $wrap
    type_section
        .ty()
        .function(vec![ValType::I32, AVER_WASM_TYPE], vec![AVER_WASM_TYPE]);
    // Type 3: (i64) -> i64  — $unwrap
    type_section
        .ty()
        .function(vec![AVER_WASM_TYPE], vec![AVER_WASM_TYPE]);
    // Type 4: (i64) -> i32  — $obj_kind, $obj_tag
    type_section
        .ty()
        .function(vec![AVER_WASM_TYPE], vec![ValType::I32]);
    // Type 5: (i64, i32) -> i64  — $obj_field
    type_section
        .ty()
        .function(vec![AVER_WASM_TYPE, ValType::I32], vec![AVER_WASM_TYPE]);

    let rt_type_count = 6u32;

    // User function types (starting at rt_type_count)
    let mut fn_type_indices: HashMap<String, u32> = HashMap::new();
    for (i, fd) in fn_defs.iter().enumerate() {
        let param_types = vec![AVER_WASM_TYPE; fd.params.len()];
        let result_types = vec![AVER_WASM_TYPE];
        type_section.ty().function(param_types, result_types);
        fn_type_indices.insert(fd.name.clone(), rt_type_count + i as u32);
    }

    module.section(&type_section);

    // -----------------------------------------------------------------------
    // Function section: declare runtime + user functions
    // -----------------------------------------------------------------------
    let mut function_section = FunctionSection::new();

    // Runtime functions — assign type indices
    let rt_type_for = |func_idx: u32| -> u32 {
        match func_idx {
            i if i == rt.alloc => 0,   // (i32)->i32
            i if i == rt.int_add => 1, // (i64,i64)->i64
            i if i == rt.int_sub => 1,
            i if i == rt.int_mul => 1,
            i if i == rt.int_div => 1,
            i if i == rt.int_eq => 1,
            i if i == rt.int_lt => 1,
            i if i == rt.int_gt => 1,
            i if i == rt.int_le => 1,
            i if i == rt.int_ge => 1,
            i if i == rt.int_ne => 1,
            i if i == rt.wrap => 2,     // (i32,i64)->i64
            i if i == rt.unwrap => 3,   // (i64)->i64
            i if i == rt.obj_kind => 4, // (i64)->i32
            i if i == rt.obj_tag => 4,
            i if i == rt.obj_field => 5, // (i64,i32)->i64
            i if i == rt.list_cons => 1, // (i64,i64)->i64
            _ => panic!("Unknown runtime function index: {}", func_idx),
        }
    };

    for idx in 0..rt.count {
        function_section.function(rt_type_for(idx));
    }

    // User functions
    let mut fn_indices: HashMap<String, u32> = HashMap::new();
    for (i, fd) in fn_defs.iter().enumerate() {
        let type_idx = fn_type_indices[&fd.name];
        function_section.function(type_idx);
        fn_indices.insert(fd.name.clone(), user_fn_base + i as u32);
    }

    module.section(&function_section);

    // -----------------------------------------------------------------------
    // Memory section
    // -----------------------------------------------------------------------
    let mut memory_section = MemorySection::new();
    memory_section.memory(MemoryType {
        minimum: 1,
        maximum: Some(256),
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    module.section(&memory_section);

    // -----------------------------------------------------------------------
    // Global section: $heap_ptr
    // -----------------------------------------------------------------------
    let mut global_section = GlobalSection::new();
    // Global 0: $heap_ptr (mut i32) = HEAP_BASE
    global_section.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(HEAP_BASE),
    );
    module.section(&global_section);

    // -----------------------------------------------------------------------
    // Export section
    // -----------------------------------------------------------------------
    let mut export_section = ExportSection::new();

    if let Some(&main_idx) = fn_indices.get("main") {
        export_section.export("main", ExportKind::Func, main_idx);
        export_section.export("_start", ExportKind::Func, main_idx);
    }

    export_section.export("memory", ExportKind::Memory, 0);

    module.section(&export_section);

    // -----------------------------------------------------------------------
    // Code section: runtime functions + user functions
    // -----------------------------------------------------------------------
    let mut code_section = CodeSection::new();

    // Emit runtime function bodies
    let rt_funcs = runtime::emit_runtime_functions(&rt);
    for func in &rt_funcs {
        code_section.function(func);
    }

    // Emit user function bodies
    for fd in &fn_defs {
        let mut emitter = ExprEmitter::new(&fn_indices, &rt);
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
