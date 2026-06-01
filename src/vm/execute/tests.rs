use super::VM;
use crate::nan_value::{Arena, ArenaEntry, ArenaList, ArenaSymbol, NanValue, NanValueConvert};
use crate::vm::opcode::{LOAD_CONST, RETURN};
use crate::vm::types::{CallFrame, CodeStore, FnChunk};
use crate::{lexer::Lexer, parser::Parser, vm};

fn compile_vm(src: &str) -> VM {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    let mut items = parser.parse().expect("parse failed");
    crate::ir::pipeline::tco(&mut items);
    crate::ir::pipeline::resolve(&mut items);

    let mut arena = Arena::new();
    let symbols = crate::ir::SymbolTable::build(&items, &[]);
    let resolved = crate::ir::hir::resolve_program(&symbols, &items);
    let (code, globals) =
        vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut arena, None)
            .expect("compile failed");
    VM::new(code, globals, arena)
}

fn assert_no_young_refs(value: NanValue, arena: &Arena, context: &str) {
    if let Some(index) = value.heap_index() {
        assert!(
            !arena.is_young_index_in_region(index, 0),
            "{context} contained young ref {value:?}"
        );
        let entry = arena.get(index);
        match entry {
            ArenaEntry::List(list) => match list {
                ArenaList::Flat { items, .. } => {
                    for item in items.iter().copied() {
                        assert_no_young_refs(item, arena, context);
                    }
                }
                ArenaList::Prepend { head, tail, .. } => {
                    assert_no_young_refs(*head, arena, context);
                    assert_no_young_refs(*tail, arena, context);
                }
                ArenaList::Concat { left, right, .. } => {
                    assert_no_young_refs(*left, arena, context);
                    assert_no_young_refs(*right, arena, context);
                }
                ArenaList::Segments { current, rest, .. } => {
                    assert_no_young_refs(*current, arena, context);
                    for item in rest.iter().copied() {
                        assert_no_young_refs(item, arena, context);
                    }
                }
            },
            ArenaEntry::Tuple(items) | ArenaEntry::Vector(items) => {
                for item in items.iter().copied() {
                    assert_no_young_refs(item, arena, context);
                }
            }
            ArenaEntry::Map(map) => {
                for (_, (key, value)) in map.iter() {
                    assert_no_young_refs(*key, arena, context);
                    assert_no_young_refs(*value, arena, context);
                }
            }
            ArenaEntry::Record { fields, .. } => {
                for field in fields.iter().copied() {
                    assert_no_young_refs(field, arena, context);
                }
            }
            ArenaEntry::Variant { fields, .. } => {
                for field in fields.iter().copied() {
                    assert_no_young_refs(field, arena, context);
                }
            }
            ArenaEntry::Namespace { members, .. } => {
                for (_, member) in members.iter() {
                    assert_no_young_refs(*member, arena, context);
                }
            }
            ArenaEntry::Boxed(inner) => assert_no_young_refs(*inner, arena, context),
            ArenaEntry::Int(_)
            | ArenaEntry::String(_)
            | ArenaEntry::Fn(_)
            | ArenaEntry::Builtin(_) => {}
        }
    }
}

fn assert_parallel_base_context_is_static_only(
    code: &CodeStore,
    globals: &[NanValue],
    arena: &Arena,
) {
    assert_eq!(
        arena.young_len(),
        0,
        "parallel base arena must not retain young entries"
    );
    assert_eq!(
        arena.yard_len(),
        0,
        "parallel base arena must not retain yard entries"
    );
    assert_eq!(
        arena.handoff_len(),
        0,
        "parallel base arena must not retain handoff entries"
    );

    for (idx, global) in globals.iter().copied().enumerate() {
        assert_no_young_refs(global, arena, &format!("global[{idx}]"));
    }

    for chunk in &code.functions {
        for (idx, constant) in chunk.constants.iter().copied().enumerate() {
            assert_no_young_refs(constant, arena, &format!("{}::const[{idx}]", chunk.name));
        }
    }

    for (idx, symbol) in arena.symbol_entries.iter().enumerate() {
        match symbol {
            ArenaSymbol::Namespace { members, .. } => {
                for (_, member) in members.iter() {
                    assert_no_young_refs(*member, arena, &format!("symbol[{idx}]"));
                }
            }
            ArenaSymbol::Fn(_) | ArenaSymbol::Builtin(_) | ArenaSymbol::NullaryVariant { .. } => {}
        }
    }
}

#[test]
fn reentrant_call_function_returns_nested_result_without_resuming_caller() {
    let mut code = CodeStore::new();

    let caller_const = NanValue::new_int_inline(10);
    let caller_id = code.add_function(FnChunk {
        name: "caller".to_string(),
        arity: 0,
        local_count: 0,
        code: vec![LOAD_CONST, 0, 0, RETURN],
        constants: vec![caller_const],
        effects: Vec::new(),
        thin: true,
        parent_thin: false,
        leaf: false,
        no_alloc: false,
        source_file: String::new(),
        line_table: Vec::new(),
    });

    let nested_const = NanValue::new_int_inline(20);
    let nested_id = code.add_function(FnChunk {
        name: "nested".to_string(),
        arity: 0,
        local_count: 0,
        code: vec![LOAD_CONST, 0, 0, RETURN],
        constants: vec![nested_const],
        effects: Vec::new(),
        thin: true,
        parent_thin: false,
        leaf: false,
        no_alloc: false,
        source_file: String::new(),
        line_table: Vec::new(),
    });

    let mut vm = VM::new(code, Vec::new(), Arena::new());
    vm.frames.push(CallFrame {
        fn_id: caller_id,
        ip: 0,
        bp: 0,
        local_count: 0,
        arena_mark: 0,
        yard_base: 0,
        yard_mark: 0,
        handoff_mark: 0,
        globals_dirty: false,
        yard_dirty: false,
        handoff_dirty: false,
        thin: true,
        parent_thin: false,
    });

    let result = vm
        .call_function(nested_id, &[])
        .expect("nested call should return");

    assert_eq!(result.as_int(&vm.arena), 20);
    assert_eq!(vm.frames.len(), 1, "caller frame should remain suspended");
    assert_eq!(vm.frames[0].fn_id, caller_id);
}

#[test]
fn collect_live_vm_roots_drops_callback_only_stable_values() {
    let mut code = CodeStore::new();
    let string_idx = {
        let mut arena = Arena::new();
        arena.push_string("callback")
    };
    let callback_id = code.add_function(FnChunk {
        name: "callback".to_string(),
        arity: 0,
        local_count: 0,
        code: vec![LOAD_CONST, 0, 0, RETURN],
        constants: vec![NanValue::new_string(string_idx)],
        effects: Vec::new(),
        thin: false,
        parent_thin: false,
        leaf: false,
        no_alloc: false,
        source_file: String::new(),
        line_table: Vec::new(),
    });

    let mut arena = Arena::new();
    let const_idx = arena.push_string("callback");
    code.functions[callback_id as usize].constants = vec![NanValue::new_string(const_idx)];
    let mut vm = VM::new(code, Vec::new(), arena);

    let result = vm
        .call_function(callback_id, &[])
        .expect("callback should return");
    assert_eq!(
        result.to_value(&vm.arena),
        crate::value::Value::Str("callback".to_string())
    );
    assert!(
        vm.arena.stable_len() > 0,
        "top-level callback return should have promoted result into stable before cleanup"
    );

    let value = result.to_value(&vm.arena);
    vm.collect_live_vm_roots();

    assert_eq!(value, crate::value::Value::Str("callback".to_string()));
    assert_eq!(
        vm.arena.stable_len(),
        0,
        "stable should be cleaned when callback result is no longer a VM root"
    );
}

#[test]
fn profiling_tracks_opcodes_and_fast_returns() {
    let mut code = CodeStore::new();
    let fn_id = code.add_function(FnChunk {
        name: "leaf".to_string(),
        arity: 0,
        local_count: 0,
        code: vec![LOAD_CONST, 0, 0, RETURN],
        constants: vec![NanValue::new_int_inline(7)],
        effects: Vec::new(),
        thin: true,
        parent_thin: false,
        leaf: false,
        no_alloc: false,
        source_file: String::new(),
        line_table: Vec::new(),
    });

    let mut vm = VM::new(code, Vec::new(), Arena::new());
    vm.start_profiling();
    let result = vm.call_function(fn_id, &[]).expect("leaf should return");
    assert_eq!(result.as_int(&vm.arena), 7);

    let report = vm.profile_report().expect("profiling should be enabled");
    assert_eq!(report.total_opcodes, 2);
    assert!(
        report
            .opcodes
            .iter()
            .any(|entry| entry.name == "LOAD_CONST" && entry.count == 1),
        "LOAD_CONST should be counted once"
    );
    assert!(
        report
            .opcodes
            .iter()
            .any(|entry| entry.name == "RETURN" && entry.count == 1),
        "RETURN should be counted once"
    );

    let function = report
        .functions
        .iter()
        .find(|entry| entry.name == "leaf")
        .expect("leaf function should be present");
    assert_eq!(function.entries, 1);
    assert_eq!(function.fast_returns, 1);
    assert_eq!(function.slow_returns, 0);
    assert_eq!(report.returns.thin_entries, 1);
    assert_eq!(report.returns.thin_fast_returns, 1);
}

#[test]
fn parallel_base_context_rebases_string_constants_to_stable() {
    let vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n\nfn lane(frame: Int) -> String\n    wave(frame)\n\nfn other() -> Int\n    7\n\nfn main() -> Tuple<String, Int>\n    (lane(3), other())!\n",
    );

    let (code, globals, arena) = vm.build_parallel_base_context();

    for value in &globals {
        if let Some(index) = value.heap_index() {
            assert!(
                Arena::is_stable_index(index),
                "parallel globals must be stable, got heap index {index}"
            );
        }
    }

    for chunk in &code.functions {
        for constant in &chunk.constants {
            if let Some(index) = constant.heap_index() {
                assert!(
                    Arena::is_stable_index(index),
                    "parallel constant in {} must be stable, got heap index {index}",
                    chunk.name
                );
            }
        }
    }

    assert_parallel_base_context_is_static_only(&code, &globals, &arena);
}

#[test]
fn call_par_keeps_string_constants_valid_in_child_vm() {
    let mut vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n\nfn lane(frame: Int, ms: Int) -> String\n    \"{wave(frame)}  ~{String.fromInt(ms)}ms\"\n\nfn other() -> Int\n    7\n\nfn main() -> Tuple<String, Int>\n    (lane(3, 1200), other())!\n",
    );

    let result = vm.run().expect("parallel string render should not crash");
    assert_eq!(
        result.to_value(&vm.arena),
        crate::value::Value::Tuple(vec![
            crate::value::Value::Str("[.==>..]  ~1200ms".to_string()),
            crate::value::Value::Int(7),
        ])
    );
}

#[test]
fn direct_child_vm_keeps_string_constants_valid() {
    let vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n\nfn lane(frame: Int, ms: Int) -> String\n    \"{wave(frame)}  ~{String.fromInt(ms)}ms\"\n\nfn main() -> String\n    lane(3, 1200)\n",
    );

    let (code, globals, arena) = vm.build_parallel_base_context();
    let lane_id = code.find("lane").expect("lane fn should exist");
    let mut child = VM::new(code, globals, arena);
    let result = child
        .call_function(
            lane_id,
            &[NanValue::new_int_inline(3), NanValue::new_int_inline(1200)],
        )
        .expect("direct child lane call should not crash");

    assert_eq!(
        result.to_value(&child.arena),
        crate::value::Value::Str("[.==>..]  ~1200ms".to_string())
    );
}

#[test]
fn direct_vm_keeps_string_constants_valid() {
    let mut vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n\nfn lane(frame: Int, ms: Int) -> String\n    \"{wave(frame)}  ~{String.fromInt(ms)}ms\"\n\nfn main() -> String\n    lane(3, 1200)\n",
    );

    let lane_id = vm.code.find("lane").expect("lane fn should exist");
    let result = vm
        .call_function(
            lane_id,
            &[NanValue::new_int_inline(3), NanValue::new_int_inline(1200)],
        )
        .expect("direct vm lane call should not crash");

    assert_eq!(
        result.to_value(&vm.arena),
        crate::value::Value::Str("[.==>..]  ~1200ms".to_string())
    );
}

#[test]
fn direct_child_vm_keeps_nested_wave_result_valid() {
    let vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n\nfn lane(frame: Int) -> String\n    \"{wave(frame)}\"\n",
    );

    let (code, globals, arena) = vm.build_parallel_base_context();
    let lane_id = code.find("lane").expect("lane fn should exist");
    let mut child = VM::new(code, globals, arena);
    let result = child
        .call_function(lane_id, &[NanValue::new_int_inline(3)])
        .expect("nested wave render should not crash");

    assert_eq!(
        result.to_value(&child.arena),
        crate::value::Value::Str("[.==>..]".to_string())
    );
}

#[test]
fn direct_child_vm_keeps_wave_return_valid() {
    let vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n",
    );

    let (code, globals, arena) = vm.build_parallel_base_context();
    let wave_id = code.find("wave").expect("wave fn should exist");
    let mut child = VM::new(code, globals, arena);
    let result = child
        .call_function(wave_id, &[NanValue::new_int_inline(3)])
        .expect("wave child call should not crash");

    assert_eq!(
        result.to_value(&child.arena),
        crate::value::Value::Str("[.==>..]".to_string())
    );
}

#[test]
fn direct_child_vm_keeps_concat_chain_after_wave_valid() {
    let vm = compile_vm(
        "fn wave(slot: Int) -> String\n    match slot\n        0 -> \"[>.....]\"\n        1 -> \"[=>....]\"\n        2 -> \"[==>...]\"\n        3 -> \"[.==>..]\"\n        4 -> \"[..==>.]\"\n        _ -> \"[...==>]\"\n\nfn lane(frame: Int) -> String\n    \"{wave(frame)}  ~\"\n",
    );

    let (code, globals, arena) = vm.build_parallel_base_context();
    let lane_id = code.find("lane").expect("lane fn should exist");
    let mut child = VM::new(code, globals, arena);
    let result = child
        .call_function(lane_id, &[NanValue::new_int_inline(3)])
        .expect("concat chain after wave should not crash");

    assert_eq!(
        result.to_value(&child.arena),
        crate::value::Value::Str("[.==>..]  ~".to_string())
    );
}
