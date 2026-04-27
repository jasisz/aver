/// WASM module builder using wasm-encoder.
///
/// Constructs a complete WASM module with:
/// - WASI fd_write import (only if program uses Console effects)
/// - Runtime functions (allocator, typed wrap/unwrap, print functions)
/// - User-defined functions with typed signatures from fn_sigs
/// - Memory section with globals ($heap_ptr)
/// - Data section with static string literals
/// - Export section (main, _start, memory)
use std::collections::{HashMap, HashSet};

use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, FunctionSection,
    GlobalSection, GlobalType, ImportSection, IndirectNameMap, Instruction, MemoryType, Module,
    NameMap, NameSection, StartSection, TypeSection, ValType,
};

use crate::ast::{Expr, FnBody, FnDef, Literal, Pattern, Stmt, StrPart, TopLevel};
use crate::codegen::CodegenContext;
use crate::ir::{ThinKind, classify_thin_fn_def, thin_body_plan_is_parent_thin_candidate};

use super::expr::{ExprEmitter, StringLiteral, build_variant_registry};
use super::runtime::{self, AverRuntimeImports, RuntimeFuncIndices};
use super::types::{WasmType, aver_type_to_wasm};
use super::value;

struct UserFnEntry<'a> {
    fd: &'a FnDef,
    canonical_name: String,
    module_prefix: Option<String>,
}

struct MutualTcoGroup {
    trampoline_name: String,
    member_indices: Vec<usize>,
}

#[derive(Clone)]
struct MutualTcoSlot {
    owner_index: usize,
    owner_param_index: usize,
    wasm_type: WasmType,
    aver_type: crate::types::Type,
}

struct MutualTcoLayout {
    trampoline_name: String,
    trampoline_type_idx: u32,
    trampoline_fn_idx: u32,
    return_type: WasmType,
    slots: Vec<MutualTcoSlot>,
    member_ids: HashMap<usize, u32>,
    member_param_locals: HashMap<usize, Vec<u32>>,
}

/// Build a complete WASM module from the Aver codegen context.
pub fn build_wasm_module(
    ctx: &CodegenContext,
    adapter: super::WasmAdapter,
) -> Result<Vec<u8>, String> {
    let mut module = Module::new();

    // Collect all function definitions: entry module + dependent modules
    let mut user_fns: Vec<UserFnEntry<'_>> = ctx
        .items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some(UserFnEntry {
                    fd,
                    canonical_name: fd.name.clone(),
                    module_prefix: None,
                })
            } else {
                None
            }
        })
        .collect();

    for module_info in &ctx.modules {
        for fd in &module_info.fn_defs {
            if fd.name == "main" {
                continue; // skip dependent module main functions
            }
            user_fns.push(UserFnEntry {
                fd,
                canonical_name: format!("{}.{}", module_info.prefix, fd.name),
                module_prefix: Some(module_info.prefix.clone()),
            });
        }
    }

    let mutual_tco_groups = build_mutual_tco_groups(ctx, &user_fns);

    // -----------------------------------------------------------------------
    // Collect string literals from AST
    // -----------------------------------------------------------------------
    let mut string_set: HashSet<String> = HashSet::new();
    for entry in &user_fns {
        collect_strings_from_body(&entry.fd.body, &mut string_set);
    }
    // Always include "true" and "false" for Bool→String conversion in interpolation
    string_set.insert("true".to_string());
    string_set.insert("false".to_string());
    let mut sorted_strings: Vec<String> = string_set.into_iter().collect();
    sorted_strings.sort();

    let mut string_literals: HashMap<String, StringLiteral> = HashMap::new();
    let mut data_offset = runtime::IO_SCRATCH_SIZE;
    let mut data_bytes: Vec<u8> = Vec::new();

    for s in &sorted_strings {
        let len = s.len() as u32;
        let header = value::make_header(value::OBJ_STRING, 0, 0, len as u64);
        data_bytes.extend_from_slice(&header.to_le_bytes());
        data_bytes.extend_from_slice(s.as_bytes());
        let padded_len = (len as usize + 7) & !7;
        data_bytes.resize(data_bytes.len() + padded_len - s.len(), 0);

        string_literals.insert(s.clone(), (data_offset, len));
        data_offset += 8 + padded_len as u32;
    }

    let heap_base = if data_offset > runtime::IO_SCRATCH_SIZE {
        ((data_offset + 7) & !7) as i32
    } else {
        1024
    };

    // -----------------------------------------------------------------------
    // Determine host imports needed
    // -----------------------------------------------------------------------
    let mut needed_host_imports: Vec<&str> = Vec::new();
    let mut host_import_set: HashSet<String> = HashSet::new();
    for entry in &user_fns {
        collect_host_calls_from_body(&entry.fd.body, &mut host_import_set);
    }
    for name in &host_import_set {
        needed_host_imports.push(name.as_str());
    }
    needed_host_imports.sort();

    // -----------------------------------------------------------------------
    // Type section — runtime + user function signatures
    // -----------------------------------------------------------------------
    let mut type_section = TypeSection::new();

    let rti = runtime::emit_base_type_section(&mut type_section);
    let rt_base_type_count = rti.count;

    // User function types — generate from fn_sigs
    let mut fn_type_indices: HashMap<String, u32> = HashMap::new();
    for (i, entry) in user_fns.iter().enumerate() {
        let (param_vals, result_vals) = if let Some((param_types, ret_type, _)) = ctx
            .fn_sigs
            .get(entry.canonical_name.as_str())
            .or_else(|| ctx.fn_sigs.get(&entry.fd.name))
        {
            let params: Vec<ValType> = param_types
                .iter()
                .map(|t| aver_type_to_wasm(t).to_val_type())
                .collect();
            let ret = vec![aver_type_to_wasm(ret_type).to_val_type()];
            (params, ret)
        } else {
            // Fallback: all i64
            (
                vec![ValType::I64; entry.fd.params.len()],
                vec![ValType::I64],
            )
        };
        type_section.ty().function(param_vals, result_vals);
        fn_type_indices.insert(entry.canonical_name.clone(), rt_base_type_count + i as u32);
    }

    let mut mutual_tco_layouts = Vec::new();
    for (group_idx, group) in mutual_tco_groups.iter().enumerate() {
        let layout = build_mutual_tco_layout(
            group,
            &user_fns,
            ctx,
            rt_base_type_count + user_fns.len() as u32 + group_idx as u32,
        )?;
        type_section.ty().function(
            std::iter::once(ValType::I32)
                .chain(layout.slots.iter().map(|slot| slot.wasm_type.to_val_type()))
                .collect::<Vec<_>>(),
            vec![layout.return_type.to_val_type()],
        );
        mutual_tco_layouts.push(layout);
    }

    module.section(&type_section);

    // -----------------------------------------------------------------------
    // Import section — driven by ABI table or WASI adapter
    //
    // Layout (Step 1 of the WAT runtime migration):
    //   0..3        aver_runtime.{rt_alloc, memory, heap_ptr}
    //   3..3+N      aver/* effect host imports (Aver adapter) or WASI imports
    // -----------------------------------------------------------------------
    let has_effects = !needed_host_imports.is_empty();
    let mut host_imports: HashMap<String, u32> = HashMap::new();

    let mut import_section = ImportSection::new();

    // aver_runtime imports come first — runtime owns memory and the
    // migrated runtime functions; user module reaches them via these
    // import slots. New runtime fns get appended as the WAT migration
    // grows; keep the order in sync with `AverRuntimeImports`.
    let aver_rt_imports = AverRuntimeImports {
        rt_alloc: 0,
        rt_truncate: 1,
        rt_obj_kind: 2,
        rt_obj_tag: 3,
        rt_obj_meta: 4,
        rt_obj_field: 5,
        rt_obj_field_f64: 6,
        rt_obj_field_i32: 7,
        rt_unwrap: 8,
        rt_unwrap_f64: 9,
        rt_unwrap_i32: 10,
        rt_wrap: 11,
        rt_wrap_f64: 12,
        rt_wrap_i32: 13,
        rt_str_eq: 14,
        rt_str_concat: 15,
        rt_list_cons: 16,
        rt_list_cons_f64: 17,
    };
    import_section.import(
        "aver_runtime",
        "rt_alloc",
        EntityType::Function(rti.alloc),
    );
    import_section.import(
        "aver_runtime",
        "rt_truncate",
        EntityType::Function(rti.i32_to_empty),
    );
    import_section.import(
        "aver_runtime",
        "rt_obj_kind",
        EntityType::Function(rti.obj_kind),
    );
    import_section.import(
        "aver_runtime",
        "rt_obj_tag",
        EntityType::Function(rti.obj_tag),
    );
    import_section.import(
        "aver_runtime",
        "rt_obj_meta",
        EntityType::Function(rti.obj_meta),
    );
    import_section.import(
        "aver_runtime",
        "rt_obj_field",
        EntityType::Function(rti.obj_field_i64),
    );
    import_section.import(
        "aver_runtime",
        "rt_obj_field_f64",
        EntityType::Function(rti.obj_field_f64),
    );
    import_section.import(
        "aver_runtime",
        "rt_obj_field_i32",
        EntityType::Function(rti.obj_field_i32),
    );
    import_section.import(
        "aver_runtime",
        "rt_unwrap",
        EntityType::Function(rti.unwrap_i64),
    );
    import_section.import(
        "aver_runtime",
        "rt_unwrap_f64",
        EntityType::Function(rti.unwrap_f64),
    );
    import_section.import(
        "aver_runtime",
        "rt_unwrap_i32",
        EntityType::Function(rti.unwrap_i32),
    );
    import_section.import(
        "aver_runtime",
        "rt_wrap",
        EntityType::Function(rti.wrap_i64),
    );
    import_section.import(
        "aver_runtime",
        "rt_wrap_f64",
        EntityType::Function(rti.wrap_f64),
    );
    import_section.import(
        "aver_runtime",
        "rt_wrap_i32",
        EntityType::Function(rti.wrap_i32),
    );
    import_section.import(
        "aver_runtime",
        "rt_str_eq",
        EntityType::Function(rti.i32_i32_to_i32),
    );
    import_section.import(
        "aver_runtime",
        "rt_str_concat",
        EntityType::Function(rti.i32_i32_to_i32),
    );
    import_section.import(
        "aver_runtime",
        "rt_list_cons",
        EntityType::Function(rti.list_cons_i64),
    );
    import_section.import(
        "aver_runtime",
        "rt_list_cons_f64",
        EntityType::Function(rti.list_cons_f64),
    );
    import_section.import(
        "aver_runtime",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        }),
    );
    import_section.import(
        "aver_runtime",
        "heap_ptr",
        EntityType::Global(GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        }),
    );
    let mut import_func_count = 18u32; // alloc/truncate/6 obj_*/3 unwrap_*/3 wrap_*/str_eq/str_concat/list_cons{,_f64}
    // Index of the write-to-stdout import (used by runtime's write_stdout helper)
    let mut write_stdout_import: Option<u32> = None;

    match adapter {
        super::WasmAdapter::Aver => {
            // Collect needed ABI imports from user-defined function effects only
            let user_fn_names: Vec<&str> = user_fns
                .iter()
                .map(|entry| entry.canonical_name.as_str())
                .collect();
            let needed =
                super::abi::collect_needed_imports(&ctx.fn_sigs, &user_fn_names, &host_import_set);
            for abi_entry in &needed {
                let type_idx =
                    runtime::lookup_type_index(&rti, abi_entry.params, abi_entry.results)
                        .ok_or_else(|| {
                            format!(
                                "Missing WASM type mapping for ABI import {} ({:?} -> {:?})",
                                abi_entry.import_name, abi_entry.params, abi_entry.results
                            )
                        })?;
                let idx = import_func_count;
                import_section.import(
                    super::abi::ABI_MODULE,
                    abi_entry.import_name,
                    wasm_encoder::EntityType::Function(type_idx),
                );
                host_imports.insert(abi_entry.import_name.to_string(), idx);
                if abi_entry.import_name == "console_print" {
                    write_stdout_import = Some(idx);
                }
                import_func_count += 1;
            }
            // Runtime helpers reference console_print via fd_write_buf, so keep the
            // import present even for otherwise pure modules.
            if write_stdout_import.is_none() {
                let abi_entry = super::abi::lookup("Console.print").unwrap();
                let type_idx =
                    runtime::lookup_type_index(&rti, abi_entry.params, abi_entry.results)
                        .ok_or_else(|| {
                            format!(
                                "Missing WASM type mapping for ABI import {} ({:?} -> {:?})",
                                abi_entry.import_name, abi_entry.params, abi_entry.results
                            )
                        })?;
                let idx = import_func_count;
                import_section.import(
                    super::abi::ABI_MODULE,
                    abi_entry.import_name,
                    wasm_encoder::EntityType::Function(type_idx),
                );
                write_stdout_import = Some(idx);
                import_func_count += 1;
            }
            // Always import print_value and format_value
            if let Some(abi_entry) = super::abi::lookup("Print.value") {
                let type_idx =
                    runtime::lookup_type_index(&rti, abi_entry.params, abi_entry.results)
                        .ok_or_else(|| {
                            format!(
                                "Missing WASM type mapping for ABI import {} ({:?} -> {:?})",
                                abi_entry.import_name, abi_entry.params, abi_entry.results
                            )
                        })?;
                let idx = import_func_count;
                import_section.import(
                    super::abi::ABI_MODULE,
                    abi_entry.import_name,
                    wasm_encoder::EntityType::Function(type_idx),
                );
                host_imports.insert(abi_entry.import_name.to_string(), idx);
                import_func_count += 1;
            }
            if let Some(abi_entry) = super::abi::lookup("Format.value") {
                let type_idx =
                    runtime::lookup_type_index(&rti, abi_entry.params, abi_entry.results)
                        .ok_or_else(|| {
                            format!(
                                "Missing WASM type mapping for ABI import {} ({:?} -> {:?})",
                                abi_entry.import_name, abi_entry.params, abi_entry.results
                            )
                        })?;
                let idx = import_func_count;
                import_section.import(
                    super::abi::ABI_MODULE,
                    abi_entry.import_name,
                    wasm_encoder::EntityType::Function(type_idx),
                );
                host_imports.insert(abi_entry.import_name.to_string(), idx);
                import_func_count += 1;
            }
        }
        super::WasmAdapter::Wasi => {
            // WASI compatibility mode: import fd_write
            import_section.import(
                "wasi_snapshot_preview1",
                "fd_write",
                wasm_encoder::EntityType::Function(rti.wasi_fd_write),
            );
            write_stdout_import = Some(import_func_count);
            import_func_count += 1;
        }
    }

    module.section(&import_section);

    let mut rt = RuntimeFuncIndices::new(import_func_count, aver_rt_imports);
    rt.fd_write_import = write_stdout_import.unwrap_or(0);
    rt.adapter = adapter;
    let trampoline_fn_base = import_func_count + rt.count;
    let user_fn_base = trampoline_fn_base + mutual_tco_layouts.len() as u32;

    // Map Console.print/error/warn → print dispatch (handled in ExprEmitter)
    if has_effects {
        for &name in &needed_host_imports {
            host_imports.insert(name.to_string(), 0); // marker only
        }
    }

    // -----------------------------------------------------------------------
    // Function section
    // -----------------------------------------------------------------------
    let mut function_section = FunctionSection::new();

    // Runtime functions
    for idx in import_func_count..(import_func_count + rt.count) {
        let type_idx = runtime::rt_type_index(&rt, &rti, idx, import_func_count);
        function_section.function(type_idx);
    }

    for (group_idx, layout) in mutual_tco_layouts.iter_mut().enumerate() {
        function_section.function(layout.trampoline_type_idx);
        layout.trampoline_fn_idx = trampoline_fn_base + group_idx as u32;
    }

    // User functions
    let mut fn_indices: HashMap<String, u32> = HashMap::new();
    for (i, entry) in user_fns.iter().enumerate() {
        let type_idx = fn_type_indices[&entry.canonical_name];
        function_section.function(type_idx);
        let idx = user_fn_base + i as u32;
        fn_indices.insert(entry.canonical_name.clone(), idx);
        if entry.module_prefix.is_none() {
            fn_indices.insert(entry.fd.name.clone(), idx);
        }
    }

    // The start function bumps the imported heap_ptr past the user's
    // static data on instantiate. Without this the runtime's allocator
    // (initialized to 128) would scribble over the data section.
    let start_fn_idx = user_fn_base + user_fns.len() as u32;
    function_section.function(rti.empty_to_empty);

    // Under the WASI adapter `_start` must be `() -> ()` so wasmtime can
    // dispatch it as a command. main carries an i32/i64/f64 return; emit
    // a void wrapper that calls main and drops its result.
    let wasi_start_wrapper_idx: Option<u32> = if matches!(adapter, super::WasmAdapter::Wasi)
        && fn_indices.contains_key("main")
    {
        function_section.function(rti.empty_to_empty);
        Some(start_fn_idx + 1)
    } else {
        None
    };

    module.section(&function_section);

    // Memory and the heap_ptr global are imported from aver_runtime; no
    // local memory section is emitted.

    // Build variant registry early — needed for global section (name table)
    let variant_registry = build_variant_registry(ctx);

    // -----------------------------------------------------------------------
    // Global section — local globals start at index 1 (heap_ptr is the
    // imported global at index 0, kept first to preserve every existing
    // GlobalGet/Set(0) reference in runtime + user code).
    // -----------------------------------------------------------------------
    let mut global_section = GlobalSection::new();
    for _ in 0..3 {
        global_section.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(0),
        );
    }

    // Variant name table: build tag→name mapping so hosts can display names.
    let variant_name_json = {
        let mut entries: Vec<(u32, String)> = Vec::new();
        for ((type_name, variant_name), info) in &variant_registry {
            let full = format!("{}.{}", type_name, variant_name);
            if !entries.iter().any(|(t, _)| *t == info.tag) {
                entries.push((info.tag, full));
            }
        }
        entries.sort_by_key(|(t, _)| *t);
        entries
            .iter()
            .map(|(t, n)| format!("{}:{}", t, n))
            .collect::<Vec<_>>()
            .join("|")
    };
    let variant_name_offset = runtime::IO_SCRATCH_SIZE as usize + data_bytes.len();
    let variant_name_bytes = variant_name_json.as_bytes();
    data_bytes.extend_from_slice(variant_name_bytes);

    // Global 4: variant_names_ptr, Global 5: variant_names_len
    global_section.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(variant_name_offset as i32),
    );
    global_section.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        },
        &ConstExpr::i32_const(variant_name_bytes.len() as i32),
    );

    module.section(&global_section);

    // -----------------------------------------------------------------------
    // Export section
    // -----------------------------------------------------------------------
    let mut export_section = ExportSection::new();

    if let Some(&main_idx) = fn_indices.get("main") {
        export_section.export("main", ExportKind::Func, main_idx);
        // Under WASI adapter point `_start` at the void wrapper so the
        // module is a valid wasi command (else wasmtime falls back to
        // experimental `--invoke` and prints the i32 return).
        let start_idx = wasi_start_wrapper_idx.unwrap_or(main_idx);
        export_section.export("_start", ExportKind::Func, start_idx);
    }

    // Re-export the imported memory, heap_ptr global, and rt_alloc so
    // hosts that look them up on the user instance (variant-name table
    // reader, host-side guest-string writer) keep working without
    // needing a handle to the runtime instance.
    export_section.export("memory", ExportKind::Memory, 0);
    export_section.export("alloc", ExportKind::Func, rt.alloc);
    export_section.export("$heap_ptr", ExportKind::Global, 0);
    export_section.export("$variant_names_ptr", ExportKind::Global, 4);
    export_section.export("$variant_names_len", ExportKind::Global, 5);

    module.section(&export_section);

    // -----------------------------------------------------------------------
    // Start section — runs the heap_ptr init before _start so the
    // imported allocator skips the user's static data.
    // -----------------------------------------------------------------------
    module.section(&StartSection {
        function_index: start_fn_idx,
    });

    // -----------------------------------------------------------------------
    // Code section
    // -----------------------------------------------------------------------
    let mut code_section = CodeSection::new();

    // Per-function local names collected during emission, keyed by absolute
    // function index (post imports + runtime + tco trampolines). Fed into
    // the wasm name section so disassembly shows aver source identifiers.
    let mut user_fn_local_names: Vec<(u32, Vec<(String, u32)>)> = Vec::new();

    // Runtime function bodies
    let rt_funcs = runtime::emit_runtime_functions(&rt);
    for func in &rt_funcs {
        code_section.function(func);
    }

    // Build type field index map
    let type_fields: HashMap<(String, String), u32> = build_type_fields(ctx);

    // Check which functions have TailCall expressions
    let tco_fns: HashSet<String> = user_fns
        .iter()
        .filter(|entry| {
            body_has_self_tailcall(&entry.fd.body, &entry.fd.name, &entry.canonical_name)
        })
        .map(|entry| entry.canonical_name.clone())
        .collect();

    let mut mutual_group_by_member = HashMap::new();
    for (group_idx, group) in mutual_tco_groups.iter().enumerate() {
        for member_idx in &group.member_indices {
            mutual_group_by_member.insert(*member_idx, group_idx);
        }
    }

    for (group, layout) in mutual_tco_groups.iter().zip(&mutual_tco_layouts) {
        let func = emit_mutual_tco_trampoline(
            group,
            layout,
            &user_fns,
            &fn_indices,
            &rt,
            &string_literals,
            &type_fields,
            ctx,
            &variant_registry,
            &host_imports,
        )?;
        code_section.function(&func);
    }

    // User function bodies / wrappers
    for (entry_idx, entry) in user_fns.iter().enumerate() {
        if let Some(&group_idx) = mutual_group_by_member.get(&entry_idx) {
            let func =
                emit_mutual_tco_wrapper(entry_idx, &mutual_tco_layouts[group_idx], &user_fns);
            code_section.function(&func);
            continue;
        }

        let (func, locals) = emit_plain_user_function(
            entry,
            &fn_indices,
            &rt,
            &string_literals,
            &type_fields,
            ctx,
            &variant_registry,
            &host_imports,
            tco_fns.contains(&entry.canonical_name),
        )?;
        code_section.function(&func);
        if let Some(&fn_idx) = fn_indices.get(&entry.canonical_name) {
            user_fn_local_names.push((fn_idx, locals));
        }
    }

    // Start function: bump heap_ptr to heap_base on instantiate. The
    // runtime ships with heap_ptr = 128 (top of IO scratch); the user's
    // static data lives at 128..heap_base, so without this the first
    // alloc would scribble over its own string table.
    {
        let mut start_fn = wasm_encoder::Function::new(Vec::new());
        start_fn.instruction(&Instruction::I32Const(heap_base));
        start_fn.instruction(&Instruction::GlobalSet(0));
        start_fn.instruction(&Instruction::End);
        code_section.function(&start_fn);
    }

    // WASI _start wrapper body: call main, drop its result.
    if wasi_start_wrapper_idx.is_some()
        && let Some(&main_idx) = fn_indices.get("main")
    {
        let main_ret_type = ctx
            .fn_sigs
            .get("main")
            .map(|(_, ret_type, _)| aver_type_to_wasm(ret_type))
            .unwrap_or(WasmType::I32);
        let mut wrapper = wasm_encoder::Function::new(Vec::new());
        wrapper.instruction(&Instruction::Call(main_idx));
        // main always returns one value (Aver functions return exactly
        // one wasm value via the typed ABI). Drop it.
        let _ = main_ret_type;
        wrapper.instruction(&Instruction::Drop);
        wrapper.instruction(&Instruction::End);
        code_section.function(&wrapper);
    }

    module.section(&code_section);

    // -----------------------------------------------------------------------
    // Data section
    // -----------------------------------------------------------------------
    if !data_bytes.is_empty() {
        let mut data_section = DataSection::new();
        data_section.active(
            0,
            &ConstExpr::i32_const(runtime::IO_SCRATCH_SIZE as i32),
            data_bytes,
        );
        module.section(&data_section);
    }

    // -----------------------------------------------------------------------
    // Name section: maps every function index back to a stable identifier so
    // `wasm-tools print` (and `aver compile --target wat`) read like source
    // instead of `call 18`. Stripped automatically by `wasm-opt -Oz` for
    // production builds, kept for dev/debug/audit pipelines.
    // -----------------------------------------------------------------------
    let mut func_names = NameMap::new();
    let mut named: Vec<(u32, String)> = Vec::new();
    for (name, idx) in &host_imports {
        // Skip marker entries (Aver-qualified names like "Console.print"
        // are existence flags, their idx 0 collides with rt_alloc).
        if name.contains('.') {
            continue;
        }
        if *idx > 0 || !needed_host_imports.is_empty() {
            named.push((*idx, format!("host_{}", name.replace('/', "_"))));
        }
    }
    for (idx, name) in rt.name_pairs() {
        named.push((idx, format!("rt_{}", name)));
    }
    for (name, idx) in &fn_indices {
        named.push((*idx, name.clone()));
    }
    named.push((start_fn_idx, "__aver_init_heap".to_string()));
    named.sort_by_key(|(idx, _)| *idx);
    named.dedup_by_key(|(idx, _)| *idx);
    for (idx, name) in &named {
        func_names.append(*idx, name);
    }
    let mut name_section = NameSection::new();
    name_section.functions(&func_names);

    // Per-function local names: aver-source identifiers (params + lets)
    // surface as `local.set $tuple_ptr` instead of `local.set 4` after
    // disassembly. Runtime function locals stay numbered for now —
    // each runtime/*.rs `Function::new(...)` would need a parallel
    // labelled-locals construction; tracked as a follow-up.
    user_fn_local_names.sort_by_key(|(idx, _)| *idx);
    let mut local_names = IndirectNameMap::new();
    for (fn_idx, locals) in &user_fn_local_names {
        let mut sorted = locals.clone();
        sorted.sort_by_key(|(_, idx)| *idx);
        sorted.dedup_by_key(|(_, idx)| *idx);
        let mut map = NameMap::new();
        for (name, idx) in &sorted {
            map.append(*idx, name);
        }
        local_names.append(*fn_idx, &map);
    }
    name_section.locals(&local_names);
    module.section(&name_section);

    Ok(module.finish())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn lookup_sig_for_entry<'a>(
    ctx: &'a CodegenContext,
    entry: &UserFnEntry<'_>,
) -> Option<&'a (Vec<crate::types::Type>, crate::types::Type, Vec<String>)> {
    ctx.fn_sigs
        .get(entry.canonical_name.as_str())
        .or_else(|| ctx.fn_sigs.get(&entry.fd.name))
}

fn param_types_for_entry(ctx: &CodegenContext, entry: &UserFnEntry<'_>) -> Vec<crate::types::Type> {
    if let Some((param_types, _, _)) = lookup_sig_for_entry(ctx, entry) {
        return param_types.clone();
    }

    entry
        .fd
        .params
        .iter()
        .map(|(_, ty)| crate::types::parse_type_str(ty))
        .collect()
}

fn ret_wasm_type_for_entry(ctx: &CodegenContext, entry: &UserFnEntry<'_>) -> WasmType {
    if let Some((_, ret_type, _)) = lookup_sig_for_entry(ctx, entry) {
        aver_type_to_wasm(ret_type)
    } else {
        aver_type_to_wasm(&crate::types::parse_type_str(&entry.fd.return_type))
    }
}

fn emit_default_value_to_func(func: &mut wasm_encoder::Function, wt: WasmType) {
    match wt {
        WasmType::I32 => func.instruction(&Instruction::I32Const(0)),
        WasmType::I64 => func.instruction(&Instruction::I64Const(0)),
        WasmType::F64 => func.instruction(&Instruction::F64Const(0.0)),
    };
}

fn build_emitter_locals(emitter: &ExprEmitter<'_>, num_params: u32) -> Vec<(u32, ValType)> {
    let mut locals = Vec::new();
    for idx in num_params..emitter.next_local {
        let vt = emitter
            .local_types
            .get(&idx)
            .map(|wt| wt.to_val_type())
            .unwrap_or(ValType::I64);
        locals.push((1, vt));
    }
    locals
}

#[allow(clippy::too_many_arguments)]
fn emit_plain_user_function(
    entry: &UserFnEntry<'_>,
    fn_indices: &HashMap<String, u32>,
    rt: &RuntimeFuncIndices,
    string_literals: &HashMap<String, StringLiteral>,
    type_fields: &HashMap<(String, String), u32>,
    ctx: &CodegenContext,
    variant_registry: &HashMap<(String, String), super::expr::VariantInfo>,
    host_imports: &HashMap<String, u32>,
    needs_tco: bool,
) -> Result<(wasm_encoder::Function, Vec<(String, u32)>), String> {
    let mut emitter = ExprEmitter::new(
        fn_indices,
        rt,
        string_literals,
        type_fields,
        &ctx.fn_sigs,
        ctx,
        variant_registry,
    );

    let param_types = param_types_for_entry(ctx, entry);
    emitter.add_params(&entry.fd.params, &param_types);
    emitter.fn_return_type = ret_wasm_type_for_entry(ctx, entry);
    emitter.fn_return_is_heap = lookup_sig_for_entry(ctx, entry)
        .map(|(_, ret_type, _)| emitter.is_heap_type(ret_type))
        .unwrap_or(false);
    emitter.current_fn_name = entry.canonical_name.clone();
    emitter.current_module_prefix = entry.module_prefix.clone();
    emitter.host_import_indices = host_imports.clone();
    let thin_plan = if entry.fd.effects.is_empty() && !needs_tco {
        classify_thin_fn_def(entry.fd, &emitter.ir_ctx())
    } else {
        None
    };
    emitter.is_thin = !emitter.fn_return_is_heap
        && entry.fd.effects.is_empty()
        && !needs_tco
        && thin_plan.as_ref().is_some_and(|plan| {
            matches!(plan.kind, ThinKind::Leaf | ThinKind::Dispatch)
                || matches!(plan.kind, ThinKind::Direct | ThinKind::Forward)
        });
    emitter.is_parent_thin = !emitter.fn_return_is_heap
        && entry.canonical_name != "main"
        && entry.fd.effects.is_empty()
        && !needs_tco
        && !emitter.is_thin
        && thin_plan
            .as_ref()
            .is_some_and(thin_body_plan_is_parent_thin_candidate);

    if !(emitter.is_thin || emitter.is_parent_thin) {
        let boundary_mark_local = emitter.alloc_local(WasmType::I32);
        emitter.boundary_mark_local = Some(boundary_mark_local);
        emitter.instructions.push(Instruction::GlobalGet(0));
        emitter
            .instructions
            .push(Instruction::LocalSet(boundary_mark_local));
    }

    if needs_tco {
        // Allocate iter_mark for yard semantics: each TCO iteration only
        // compacts its own allocations, not the entire frame.
        if emitter.boundary_mark_local.is_some() {
            let iter_mark = emitter.alloc_local(WasmType::I32);
            emitter.iter_mark_local = Some(iter_mark);
        }

        emitter
            .instructions
            .push(Instruction::Loop(wasm_encoder::BlockType::Result(
                emitter.fn_return_type.to_val_type(),
            )));
        emitter.block_depth += 1;
        emitter.enable_tco_loop();

        // Save heap_ptr at iteration start — survivors from previous
        // iterations live below this mark and won't be re-copied.
        if let Some(iter_mark) = emitter.iter_mark_local {
            emitter.instructions.push(Instruction::GlobalGet(0));
            emitter.instructions.push(Instruction::LocalSet(iter_mark));
        }
    }

    emitter.emit_body(&entry.fd.body);

    if !emitter.errors.is_empty() {
        return Err(emitter.errors.join("\n"));
    }

    if needs_tco {
        emitter.emit_end();
    }

    emitter.emit_boundary_truncate_or_compact_for_stack_value(
        emitter.fn_return_type,
        emitter.fn_return_is_heap,
    );

    let locals = build_emitter_locals(&emitter, entry.fd.params.len() as u32);
    let mut func = wasm_encoder::Function::new(locals);
    for instr in &emitter.instructions {
        func.instruction(instr);
    }
    func.instruction(&Instruction::End);
    let local_names: Vec<(String, u32)> =
        emitter.locals.into_iter().map(|(n, idx)| (n, idx)).collect();
    Ok((func, local_names))
}

fn build_mutual_tco_groups(
    ctx: &CodegenContext,
    user_fns: &[UserFnEntry<'_>],
) -> Vec<MutualTcoGroup> {
    let mut groups = Vec::new();
    let mut next_group_id = 1usize;

    let entry_index_by_name: HashMap<String, usize> = user_fns
        .iter()
        .enumerate()
        .filter(|(_, entry)| entry.module_prefix.is_none() && entry.fd.name != "main")
        .map(|(idx, entry)| (entry.fd.name.clone(), idx))
        .collect();
    let entry_fns: Vec<&FnDef> = ctx.fn_defs.iter().filter(|fd| fd.name != "main").collect();
    for group in crate::call_graph::tailcall_scc_components(&entry_fns) {
        let member_indices: Vec<usize> = group
            .iter()
            .filter_map(|fd| entry_index_by_name.get(&fd.name).copied())
            .collect();
        if member_indices.len() > 1 {
            groups.push(MutualTcoGroup {
                trampoline_name: format!("__mutual_tco_trampoline_{}", next_group_id),
                member_indices,
            });
            next_group_id += 1;
        }
    }

    for module in &ctx.modules {
        let module_index_by_name: HashMap<String, usize> = user_fns
            .iter()
            .enumerate()
            .filter(|(_, entry)| entry.module_prefix.as_deref() == Some(module.prefix.as_str()))
            .map(|(idx, entry)| (entry.fd.name.clone(), idx))
            .collect();

        let module_fns: Vec<&FnDef> = module.fn_defs.iter().collect();
        for group in crate::call_graph::tailcall_scc_components(&module_fns) {
            let member_indices: Vec<usize> = group
                .iter()
                .filter_map(|fd| module_index_by_name.get(&fd.name).copied())
                .collect();
            if member_indices.len() > 1 {
                groups.push(MutualTcoGroup {
                    trampoline_name: format!("__mutual_tco_trampoline_{}", next_group_id),
                    member_indices,
                });
                next_group_id += 1;
            }
        }
    }

    groups
}

fn build_mutual_tco_layout(
    group: &MutualTcoGroup,
    user_fns: &[UserFnEntry<'_>],
    ctx: &CodegenContext,
    trampoline_type_idx: u32,
) -> Result<MutualTcoLayout, String> {
    let first_entry = &user_fns[group.member_indices[0]];
    let return_type = ret_wasm_type_for_entry(ctx, first_entry);
    let mut slots = Vec::new();
    let mut member_ids = HashMap::new();
    let mut member_param_locals = HashMap::new();
    let mut next_local = 1u32;

    for (member_id, member_index) in group.member_indices.iter().copied().enumerate() {
        let entry = &user_fns[member_index];
        let member_return_type = ret_wasm_type_for_entry(ctx, entry);
        if member_return_type != return_type {
            return Err(format!(
                "mutual TCO group `{}` has mismatched return types (`{}` vs `{}`)",
                group.trampoline_name, first_entry.canonical_name, entry.canonical_name
            ));
        }

        let param_types = param_types_for_entry(ctx, entry);
        let mut param_locals = Vec::new();
        for (param_index, aver_type) in param_types.into_iter().enumerate() {
            slots.push(MutualTcoSlot {
                owner_index: member_index,
                owner_param_index: param_index,
                wasm_type: aver_type_to_wasm(&aver_type),
                aver_type,
            });
            param_locals.push(next_local);
            next_local += 1;
        }
        member_ids.insert(member_index, member_id as u32);
        member_param_locals.insert(member_index, param_locals);
    }

    Ok(MutualTcoLayout {
        trampoline_name: group.trampoline_name.clone(),
        trampoline_type_idx,
        trampoline_fn_idx: 0,
        return_type,
        slots,
        member_ids,
        member_param_locals,
    })
}

fn emit_mutual_tco_wrapper(
    entry_index: usize,
    layout: &MutualTcoLayout,
    _user_fns: &[UserFnEntry<'_>],
) -> wasm_encoder::Function {
    let mut func = wasm_encoder::Function::new(Vec::new());
    func.instruction(&Instruction::I32Const(
        *layout.member_ids.get(&entry_index).unwrap_or(&0) as i32,
    ));
    for slot in &layout.slots {
        if slot.owner_index == entry_index {
            func.instruction(&Instruction::LocalGet(slot.owner_param_index as u32));
        } else {
            emit_default_value_to_func(&mut func, slot.wasm_type);
        }
    }
    func.instruction(&Instruction::Call(layout.trampoline_fn_idx));
    func.instruction(&Instruction::End);
    func
}

fn emit_mutual_dispatch_chain(
    emitter: &mut ExprEmitter<'_>,
    group: &MutualTcoGroup,
    layout: &MutualTcoLayout,
    user_fns: &[UserFnEntry<'_>],
    member_pos: usize,
) {
    let member_index = group.member_indices[member_pos];
    let entry = &user_fns[member_index];
    let member_id = layout.member_ids[&member_index] as i32;

    emitter.instructions.push(Instruction::LocalGet(0));
    emitter.instructions.push(Instruction::I32Const(member_id));
    emitter.instructions.push(Instruction::I32Eq);
    emitter.emit_if(wasm_encoder::BlockType::Result(
        layout.return_type.to_val_type(),
    ));

    let saved_locals = std::mem::take(&mut emitter.locals);
    let saved_fn_name = emitter.current_fn_name.clone();
    let saved_module_prefix = emitter.current_module_prefix.clone();

    for ((name, _), slot) in entry
        .fd
        .params
        .iter()
        .zip(layout.member_param_locals[&member_index].iter())
    {
        emitter.locals.insert(name.clone(), *slot);
    }
    emitter.current_fn_name = entry.canonical_name.clone();
    emitter.current_module_prefix = entry.module_prefix.clone();
    emitter.emit_body(&entry.fd.body);

    emitter.locals = saved_locals;
    emitter.current_fn_name = saved_fn_name;
    emitter.current_module_prefix = saved_module_prefix;

    if member_pos + 1 < group.member_indices.len() {
        emitter.emit_else();
        emit_mutual_dispatch_chain(emitter, group, layout, user_fns, member_pos + 1);
    } else {
        emitter.emit_else();
        emitter.instructions.push(Instruction::Unreachable);
    }
    emitter.emit_end();
}

#[allow(clippy::too_many_arguments)]
fn emit_mutual_tco_trampoline(
    group: &MutualTcoGroup,
    layout: &MutualTcoLayout,
    user_fns: &[UserFnEntry<'_>],
    fn_indices: &HashMap<String, u32>,
    rt: &RuntimeFuncIndices,
    string_literals: &HashMap<String, StringLiteral>,
    type_fields: &HashMap<(String, String), u32>,
    ctx: &CodegenContext,
    variant_registry: &HashMap<(String, String), super::expr::VariantInfo>,
    host_imports: &HashMap<String, u32>,
) -> Result<wasm_encoder::Function, String> {
    let mut emitter = ExprEmitter::new(
        fn_indices,
        rt,
        string_literals,
        type_fields,
        &ctx.fn_sigs,
        ctx,
        variant_registry,
    );
    emitter.fn_return_type = layout.return_type;
    emitter.current_fn_name = layout.trampoline_name.clone();
    emitter.host_import_indices = host_imports.clone();
    emitter.mutual_tco_dispatch_local = Some(0);
    emitter.fn_return_is_heap = group
        .member_indices
        .iter()
        .filter_map(|member_index| lookup_sig_for_entry(ctx, &user_fns[*member_index]))
        .next()
        .map(|(_, ret_type, _)| emitter.is_heap_type(ret_type))
        .unwrap_or(false);
    emitter.mutual_tco_targets = group
        .member_indices
        .iter()
        .map(|member_index| {
            (
                user_fns[*member_index].canonical_name.clone(),
                (
                    layout.member_ids[member_index],
                    layout.member_param_locals[member_index].clone(),
                ),
            )
        })
        .collect();

    emitter.local_types.insert(0, WasmType::I32);
    for (slot_index, slot) in layout.slots.iter().enumerate() {
        let local_index = 1 + slot_index as u32;
        emitter.local_types.insert(local_index, slot.wasm_type);
        emitter
            .local_aver_types
            .insert(local_index, slot.aver_type.clone());
    }
    emitter.next_local = 1 + layout.slots.len() as u32;
    let boundary_mark_local = emitter.alloc_local(WasmType::I32);
    emitter.boundary_mark_local = Some(boundary_mark_local);
    emitter.instructions.push(Instruction::GlobalGet(0));
    emitter
        .instructions
        .push(Instruction::LocalSet(boundary_mark_local));

    // Yard semantics: per-iteration mark for mutual TCO trampoline.
    let iter_mark = emitter.alloc_local(WasmType::I32);
    emitter.iter_mark_local = Some(iter_mark);

    // Watermark: heap_ptr after last compaction. Compact when garbage
    // (heap_ptr - watermark) exceeds threshold. Adaptive — compacts
    // only when actual garbage warrants it, not on a fixed schedule.
    let gc_watermark = emitter.alloc_local(WasmType::I32);
    emitter.gc_watermark_local = Some(gc_watermark);
    emitter.instructions.push(Instruction::GlobalGet(0));
    emitter
        .instructions
        .push(Instruction::LocalSet(gc_watermark));

    emitter
        .instructions
        .push(Instruction::Loop(wasm_encoder::BlockType::Result(
            layout.return_type.to_val_type(),
        )));
    emitter.block_depth += 1;
    emitter.enable_tco_loop();

    // Save heap_ptr at iteration start.
    emitter.instructions.push(Instruction::GlobalGet(0));
    emitter.instructions.push(Instruction::LocalSet(iter_mark));

    emit_mutual_dispatch_chain(&mut emitter, group, layout, user_fns, 0);

    if !emitter.errors.is_empty() {
        return Err(emitter.errors.join("\n"));
    }

    emitter.emit_end();
    emitter.emit_boundary_truncate_or_compact_for_stack_value(
        emitter.fn_return_type,
        emitter.fn_return_is_heap,
    );

    let locals = build_emitter_locals(&emitter, 1 + layout.slots.len() as u32);
    let mut func = wasm_encoder::Function::new(locals);
    for instr in &emitter.instructions {
        func.instruction(instr);
    }
    func.instruction(&Instruction::End);
    Ok(func)
}

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
                collect_strings_from_pattern(&arm.pattern, strings);
                collect_strings_from_expr(&arm.body.node, strings);
            }
        }
        Expr::Constructor(_, Some(e)) => {
            collect_strings_from_expr(&e.node, strings);
        }
        Expr::ErrorProp(e) => collect_strings_from_expr(&e.node, strings),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_strings_from_expr(&item.node, strings);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_strings_from_expr(&key.node, strings);
                collect_strings_from_expr(&value.node, strings);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_strings_from_expr(&expr.node, strings);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_strings_from_expr(&base.node, strings);
            for (_, expr) in updates {
                collect_strings_from_expr(&expr.node, strings);
            }
        }
        Expr::Attr(base, _) => collect_strings_from_expr(&base.node, strings),
        Expr::TailCall(tc) => {
            for arg in &tc.args {
                collect_strings_from_expr(&arg.node, strings);
            }
        }
        _ => {}
    }
}

fn collect_strings_from_pattern(pattern: &Pattern, strings: &mut HashSet<String>) {
    match pattern {
        Pattern::Literal(Literal::Str(s)) => {
            strings.insert(s.clone());
        }
        Pattern::Tuple(items) => {
            for item in items {
                collect_strings_from_pattern(item, strings);
            }
        }
        _ => {}
    }
}

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
            if let Expr::Attr(base, method) = &callee.node
                && let Expr::Ident(ns) | Expr::Resolved { name: ns, .. } = &base.node
            {
                let qualified = format!("{}.{}", ns, method);
                if is_host_builtin(&qualified) {
                    imports.insert(qualified);
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
        Expr::Constructor(_, Some(e)) => {
            collect_host_calls_from_expr(&e.node, imports);
        }
        Expr::ErrorProp(e) => collect_host_calls_from_expr(&e.node, imports),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                collect_host_calls_from_expr(&item.node, imports);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, expr) in fields {
                collect_host_calls_from_expr(&expr.node, imports);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_host_calls_from_expr(&base.node, imports);
            for (_, expr) in updates {
                collect_host_calls_from_expr(&expr.node, imports);
            }
        }
        Expr::Attr(base, _) => collect_host_calls_from_expr(&base.node, imports),
        Expr::TailCall(tc) => {
            for arg in &tc.args {
                collect_host_calls_from_expr(&arg.node, imports);
            }
        }
        _ => {}
    }
}

fn is_host_builtin(name: &str) -> bool {
    matches!(
        name,
        "Args.get"
            | "Console.print"
            | "Console.error"
            | "Console.warn"
            | "Console.readLine"
            | "Terminal.enableRawMode"
            | "Terminal.disableRawMode"
            | "Terminal.clear"
            | "Terminal.moveTo"
            | "Terminal.print"
            | "Terminal.setColor"
            | "Terminal.resetColor"
            | "Terminal.readKey"
            | "Terminal.size"
            | "Terminal.hideCursor"
            | "Terminal.showCursor"
            | "Terminal.flush"
            | "Float.sin"
            | "Float.cos"
            | "Float.atan2"
            | "Float.pow"
            | "Random.int"
            | "Time.now"
            | "Time.unixMs"
            | "Time.sleep"
    )
}

fn body_has_self_tailcall(body: &FnBody, local_name: &str, canonical_name: &str) -> bool {
    match body {
        FnBody::Block(stmts) => stmts.iter().any(|s| match s {
            Stmt::Expr(e) => expr_has_self_tailcall(&e.node, local_name, canonical_name),
            Stmt::Binding(_, _, e) => expr_has_self_tailcall(&e.node, local_name, canonical_name),
        }),
    }
}

fn expr_has_self_tailcall(expr: &Expr, local_name: &str, canonical_name: &str) -> bool {
    match expr {
        Expr::TailCall(boxed) => boxed.target == local_name || boxed.target == canonical_name,
        Expr::Match { arms, .. } => arms
            .iter()
            .any(|arm| expr_has_self_tailcall(&arm.body.node, local_name, canonical_name)),
        Expr::BinOp(_, l, r) => {
            expr_has_self_tailcall(&l.node, local_name, canonical_name)
                || expr_has_self_tailcall(&r.node, local_name, canonical_name)
        }
        Expr::FnCall(c, args) => {
            expr_has_self_tailcall(&c.node, local_name, canonical_name)
                || args
                    .iter()
                    .any(|a| expr_has_self_tailcall(&a.node, local_name, canonical_name))
        }
        _ => false,
    }
}

fn build_type_fields(ctx: &CodegenContext) -> HashMap<(String, String), u32> {
    let mut map = HashMap::new();
    for td in &ctx.type_defs {
        if let crate::ast::TypeDef::Product { name, fields, .. } = td {
            for (i, (field_name, _field_type)) in fields.iter().enumerate() {
                map.insert((name.clone(), field_name.clone()), i as u32);
            }
        }
    }
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
    // Built-in record types (Header, HttpResponse, HttpRequest,
    // Tcp.Connection, Terminal.Size) — same shared `builtin_records`
    // table the proof backends use. Adding a new built-in record
    // automatically gets its field offsets registered here.
    for record in crate::codegen::builtin_records::BUILTIN_RECORDS {
        for (i, field) in record.fields.iter().enumerate() {
            map.insert(
                (record.aver_name.to_string(), field.name.to_string()),
                i as u32,
            );
        }
    }
    map
}
