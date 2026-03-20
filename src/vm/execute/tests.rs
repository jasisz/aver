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
