/// WASM module builder using wasm-encoder.
///
/// Constructs a complete WASM module with:
/// - Host imports for effectful builtins (Console.print etc.)
/// - Runtime functions (bump allocator, tagged arithmetic, object helpers)
/// - User-defined functions (with loop-based TCO where needed)
/// - Memory section with globals ($heap_ptr)
/// - Data section with static string literals
/// - Export section (main, _start, memory)
use std::collections::{HashMap, HashSet};

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ExportKind, ExportSection, FunctionSection, GlobalSection,
    GlobalType, ImportSection, Instruction, MemorySection, MemoryType, Module, TypeSection,
    ValType,
};

use crate::ast::{Expr, FnBody, FnDef, Literal, Stmt, StrPart, TopLevel};
use crate::codegen::CodegenContext;

use super::expr::{ExprEmitter, StringLiteral};
use super::runtime::{self, RuntimeFuncIndices};
use super::types::AVER_WASM_TYPE;
use super::value;

/// Build a complete WASM module from the Aver codegen context.
pub fn build_wasm_module(ctx: &CodegenContext) -> Result<Vec<u8>, String> {
    let mut module = Module::new();

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
    // Collect string literals from AST
    // -----------------------------------------------------------------------
    let mut string_set: HashSet<String> = HashSet::new();
    for fd in &fn_defs {
        collect_strings_from_body(&fd.body, &mut string_set);
    }
    let mut sorted_strings: Vec<String> = string_set.into_iter().collect();
    sorted_strings.sort(); // deterministic order

    // Build string table: string → (data_offset, len)
    // Each string object in data section: [8-byte header][bytes padded to 8]
    let mut string_literals: HashMap<String, StringLiteral> = HashMap::new();
    let mut data_offset = 0u32;
    let mut data_bytes: Vec<u8> = Vec::new();

    for s in &sorted_strings {
        let len = s.len() as u32;
        let header = value::make_header(value::OBJ_STRING, 0, 0, len as u64);
        // Write header (8 bytes, little-endian)
        data_bytes.extend_from_slice(&header.to_le_bytes());
        // Write string bytes
        data_bytes.extend_from_slice(s.as_bytes());
        // Pad to 8-byte alignment
        let padded_len = (len as usize + 7) & !7;
        data_bytes.resize(data_bytes.len() + padded_len - s.len(), 0);

        string_literals.insert(s.clone(), (data_offset, len));
        data_offset += 8 + padded_len as u32; // header + padded bytes
    }

    let heap_base = if data_offset > 0 {
        // Align heap base to 8 bytes after static data
        ((data_offset + 7) & !7) as i32
    } else {
        1024 // default if no strings
    };

    // -----------------------------------------------------------------------
    // Determine host imports needed
    // -----------------------------------------------------------------------
    let mut needed_host_imports: Vec<&str> = Vec::new();
    let mut host_import_set: HashSet<String> = HashSet::new();
    for fd in &fn_defs {
        collect_host_calls_from_body(&fd.body, &mut host_import_set);
    }
    for name in &host_import_set {
        needed_host_imports.push(name.as_str());
    }
    needed_host_imports.sort();

    // -----------------------------------------------------------------------
    // Type section
    // -----------------------------------------------------------------------
    let mut type_section = TypeSection::new();

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
    // Type 6: (i64) -> ()  — void host imports (Console.print etc.)
    type_section.ty().function(vec![AVER_WASM_TYPE], vec![]);

    let rt_type_count = 7u32;

    // User function types
    let mut fn_type_indices: HashMap<String, u32> = HashMap::new();
    for (i, fd) in fn_defs.iter().enumerate() {
        let param_types = vec![AVER_WASM_TYPE; fd.params.len()];
        let result_types = vec![AVER_WASM_TYPE];
        type_section.ty().function(param_types, result_types);
        fn_type_indices.insert(fd.name.clone(), rt_type_count + i as u32);
    }

    module.section(&type_section);

    // -----------------------------------------------------------------------
    // Import section: host functions
    // -----------------------------------------------------------------------
    let mut import_section = ImportSection::new();
    let mut host_imports: HashMap<String, u32> = HashMap::new();
    let mut import_func_count = 0u32;

    let has_effects = !needed_host_imports.is_empty();

    for &name in &needed_host_imports {
        let rt_name = match name {
            "Console.print" => "aver_print_value",
            "Console.error" => "aver_print_error",
            "Console.warn" => "aver_print_error",
            _ => &name.replace('.', "_").to_lowercase(),
        };
        import_section.import(
            "aver_rt",
            rt_name,
            wasm_encoder::EntityType::Function(6), // (i64) -> ()
        );
        host_imports.insert(name.to_string(), import_func_count);
        import_func_count += 1;
    }

    // If using runtime, also import memory from it (wasm-merge resolves)
    if has_effects {
        import_section.import(
            "aver_rt",
            "memory",
            wasm_encoder::EntityType::Memory(MemoryType {
                minimum: 1,
                maximum: None, // unbounded — match runtime export
                memory64: false,
                shared: false,
                page_size_log2: None,
            }),
        );
    }

    if has_effects {
        module.section(&import_section);
    }

    // Runtime function indices start after imports
    let rt = RuntimeFuncIndices::new(import_func_count);
    let user_fn_base = import_func_count + rt.count;

    // -----------------------------------------------------------------------
    // Function section
    // -----------------------------------------------------------------------
    let mut function_section = FunctionSection::new();

    let rt_type_for = |func_idx: u32| -> u32 {
        let local_idx = func_idx - import_func_count;
        let rt_base = 0; // rt indices are 0-based within runtime
        match local_idx + rt_base {
            i if i == rt.alloc - import_func_count => 0,
            i if i == rt.int_add - import_func_count => 1,
            i if i == rt.int_sub - import_func_count => 1,
            i if i == rt.int_mul - import_func_count => 1,
            i if i == rt.int_div - import_func_count => 1,
            i if i == rt.int_eq - import_func_count => 1,
            i if i == rt.int_lt - import_func_count => 1,
            i if i == rt.int_gt - import_func_count => 1,
            i if i == rt.int_le - import_func_count => 1,
            i if i == rt.int_ge - import_func_count => 1,
            i if i == rt.int_ne - import_func_count => 1,
            i if i == rt.wrap - import_func_count => 2,
            i if i == rt.unwrap - import_func_count => 3,
            i if i == rt.obj_kind - import_func_count => 4,
            i if i == rt.obj_tag - import_func_count => 4,
            i if i == rt.obj_field - import_func_count => 5,
            i if i == rt.list_cons - import_func_count => 1,
            _ => panic!("Unknown runtime function index: {}", func_idx),
        }
    };

    for idx in import_func_count..(import_func_count + rt.count) {
        function_section.function(rt_type_for(idx));
    }

    let mut fn_indices: HashMap<String, u32> = HashMap::new();
    for (i, fd) in fn_defs.iter().enumerate() {
        let type_idx = fn_type_indices[&fd.name];
        function_section.function(type_idx);
        fn_indices.insert(fd.name.clone(), user_fn_base + i as u32);
    }

    module.section(&function_section);

    // -----------------------------------------------------------------------
    // Memory section — only for pure programs (no runtime imports).
    // Programs with effects import memory from aver_rt.
    // -----------------------------------------------------------------------
    if !has_effects {
        let mut memory_section = MemorySection::new();
        memory_section.memory(MemoryType {
            minimum: 1,
            maximum: Some(256),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memory_section);
    }

    // -----------------------------------------------------------------------
    // Global section
    // -----------------------------------------------------------------------
    let mut global_section = GlobalSection::new();
    global_section.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        &ConstExpr::i32_const(heap_base),
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

    if !has_effects {
        export_section.export("memory", ExportKind::Memory, 0);
    }

    module.section(&export_section);

    // -----------------------------------------------------------------------
    // Code section
    // -----------------------------------------------------------------------
    let mut code_section = CodeSection::new();

    // Runtime function bodies
    let rt_funcs = runtime::emit_runtime_functions(&rt);
    for func in &rt_funcs {
        code_section.function(func);
    }

    // Build type field index map for record field access
    let type_fields: HashMap<(String, String), u32> = build_type_fields(ctx);

    // Check which functions have TailCall expressions
    let tco_fns: HashSet<String> = fn_defs
        .iter()
        .filter(|fd| body_has_tailcall(&fd.body))
        .map(|fd| fd.name.clone())
        .collect();

    // User function bodies
    for fd in &fn_defs {
        let mut emitter = ExprEmitter::new(
            &fn_indices,
            &rt,
            &string_literals,
            &host_imports,
            &type_fields,
        );
        emitter.add_params(&fd.params);

        let needs_tco = tco_fns.contains(&fd.name);
        if needs_tco {
            emitter
                .instructions
                .push(Instruction::Loop(wasm_encoder::BlockType::Result(
                    AVER_WASM_TYPE,
                )));
            emitter.block_depth += 1;
            emitter.enable_tco_loop();
        }

        emitter.emit_body(&fd.body);

        if needs_tco {
            emitter.emit_end(); // end loop
        }

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

    // -----------------------------------------------------------------------
    // Data section: static string literals
    // -----------------------------------------------------------------------
    if !data_bytes.is_empty() {
        let mut data_section = DataSection::new();
        data_section.active(0, &ConstExpr::i32_const(0), data_bytes);
        module.section(&data_section);
    }

    Ok(module.finish())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Collect all string literals from a function body.
fn collect_strings_from_body(body: &FnBody, strings: &mut HashSet<String>) {
    match body {
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) => collect_strings_from_expr(&expr.node, strings),
                    Stmt::Expr(expr) => collect_strings_from_expr(&expr.node, strings),
                }
            }
        }
    }
}

fn collect_strings_from_expr(expr: &Expr, strings: &mut HashSet<String>) {
    match expr {
        Expr::Literal(Literal::Str(s)) => {
            strings.insert(s.clone());
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Literal(s) = part {
                    strings.insert(s.clone());
                } else if let StrPart::Parsed(e) = part {
                    collect_strings_from_expr(&e.node, strings);
                }
            }
        }
        Expr::BinOp(_, lhs, rhs) => {
            collect_strings_from_expr(&lhs.node, strings);
            collect_strings_from_expr(&rhs.node, strings);
        }
        Expr::FnCall(callee, args) => {
            collect_strings_from_expr(&callee.node, strings);
            for arg in args {
                collect_strings_from_expr(&arg.node, strings);
            }
        }
        Expr::Match { subject, arms } => {
            collect_strings_from_expr(&subject.node, strings);
            for arm in arms {
                collect_strings_from_expr(&arm.body.node, strings);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(e) = inner {
                collect_strings_from_expr(&e.node, strings);
            }
        }
        Expr::ErrorProp(e) => collect_strings_from_expr(&e.node, strings),
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_strings_from_expr(&item.node, strings);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_strings_from_expr(&expr.node, strings);
            }
        }
        Expr::TailCall(tc) => {
            for arg in &tc.1 {
                collect_strings_from_expr(&arg.node, strings);
            }
        }
        _ => {}
    }
}

/// Collect host import names (Console.print, Console.error, etc.) from a function body.
fn collect_host_calls_from_body(body: &FnBody, imports: &mut HashSet<String>) {
    match body {
        FnBody::Block(stmts) => {
            for stmt in stmts {
                match stmt {
                    Stmt::Binding(_, _, expr) => {
                        collect_host_calls_from_expr(&expr.node, imports);
                    }
                    Stmt::Expr(expr) => collect_host_calls_from_expr(&expr.node, imports),
                }
            }
        }
    }
}

fn collect_host_calls_from_expr(expr: &Expr, imports: &mut HashSet<String>) {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(base, method) = &callee.node {
                if let Expr::Ident(ns) = &base.node {
                    let qualified = format!("{}.{}", ns, method);
                    if is_host_builtin(&qualified) {
                        imports.insert(qualified);
                    }
                }
            }
            collect_host_calls_from_expr(&callee.node, imports);
            for arg in args {
                collect_host_calls_from_expr(&arg.node, imports);
            }
        }
        Expr::BinOp(_, lhs, rhs) => {
            collect_host_calls_from_expr(&lhs.node, imports);
            collect_host_calls_from_expr(&rhs.node, imports);
        }
        Expr::Match { subject, arms } => {
            collect_host_calls_from_expr(&subject.node, imports);
            for arm in arms {
                collect_host_calls_from_expr(&arm.body.node, imports);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(e) = inner {
                collect_host_calls_from_expr(&e.node, imports);
            }
        }
        Expr::ErrorProp(e) => collect_host_calls_from_expr(&e.node, imports),
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_host_calls_from_expr(&item.node, imports);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_host_calls_from_expr(&expr.node, imports);
            }
        }
        Expr::TailCall(tc) => {
            for arg in &tc.1 {
                collect_host_calls_from_expr(&arg.node, imports);
            }
        }
        _ => {}
    }
}

/// Check if a qualified name is a host builtin (effectful, needs WASM import).
fn is_host_builtin(name: &str) -> bool {
    matches!(
        name,
        "Console.print" | "Console.error" | "Console.warn" | "Console.readLine"
    )
}

/// Check if a function body contains any TailCall expressions.
fn body_has_tailcall(body: &FnBody) -> bool {
    match body {
        FnBody::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Expr(e) => expr_has_tailcall(&e.node),
            Stmt::Binding(_, _, e) => expr_has_tailcall(&e.node),
        }),
    }
}

fn expr_has_tailcall(expr: &Expr) -> bool {
    match expr {
        Expr::TailCall(_) => true,
        Expr::Match { arms, .. } => arms.iter().any(|arm| expr_has_tailcall(&arm.body.node)),
        Expr::BinOp(_, l, r) => expr_has_tailcall(&l.node) || expr_has_tailcall(&r.node),
        Expr::FnCall(c, args) => {
            expr_has_tailcall(&c.node) || args.iter().any(|a| expr_has_tailcall(&a.node))
        }
        _ => false,
    }
}

/// Build a map of (type_name, field_name) → field_index from type definitions.
fn build_type_fields(ctx: &CodegenContext) -> HashMap<(String, String), u32> {
    let mut map = HashMap::new();
    for td in &ctx.type_defs {
        if let crate::ast::TypeDef::Product { name, fields, .. } = td {
            for (i, (field_name, _field_type)) in fields.iter().enumerate() {
                map.insert((name.clone(), field_name.clone()), i as u32);
            }
        }
    }
    // Also check module type defs
    for module in &ctx.modules {
        for td in &module.type_defs {
            if let crate::ast::TypeDef::Product { name, fields, .. } = td {
                let qualified = format!("{}.{}", module.prefix, name);
                for (i, (field_name, _field_type)) in fields.iter().enumerate() {
                    map.insert((qualified.clone(), field_name.clone()), i as u32);
                    map.insert((name.clone(), field_name.clone()), i as u32);
                }
            }
        }
    }
    map
}
