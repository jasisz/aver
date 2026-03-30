use super::VM;
use crate::nan_value::{Arena, NanValue};
use crate::vm::opcode::{LOAD_CONST, RETURN};
use crate::vm::types::{CallFrame, CodeStore, FnChunk};

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
