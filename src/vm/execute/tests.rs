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
            | ArenaEntry::BigInt(_)
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
            crate::value::Value::int(7),
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

/// What one run of the accumulator-recursion shape from issue #886 cost the
/// collector: elements written into fresh shared bodies, and elements read while
/// deciding whether a body needed rewriting at all.
///
/// The two are not the same measurement and the difference is the point. A body
/// that is copied was also read; a body can be read in full and copied not at
/// all, which is exactly what happens when every element relocates to itself.
struct ListCopyCost {
    copied: u64,
    scanned: u64,
}

/// Run the shape at size `n` over a list of `elem_ty`, where each element is
/// built by `elem_expr` from the loop counter `n`.
///
/// The element type is the variable that matters: an `Int` element is an
/// immediate, so the collector can prove a whole body irrelevant without reading
/// it, while a `String` element carries a heap index and forces the walk.
fn list_copy_cost(n: i64, elem_ty: &str, elem_expr: &str) -> ListCopyCost {
    let src = format!(
        "fn build(n: Int, acc: List<{elem_ty}>) -> List<{elem_ty}>\n    match n > 0\n        true -> build(n - 1, List.prepend({elem_expr}, acc))\n        false -> acc\n\nfn copy(xs: List<{elem_ty}>, acc: List<{elem_ty}>) -> List<{elem_ty}>\n    match xs\n        [] -> acc\n        [head, ..tail] -> copy(tail, List.prepend(head, acc))\n\nfn main() -> Int\n    List.len(copy(build({n}, []), []))\n"
    );
    let mut vm = compile_vm(&src);
    let result = vm.run().expect("list copy program should run");
    assert_eq!(result.as_int(&vm.arena), n);
    ListCopyCost {
        copied: vm.arena.list_elements_copied(),
        scanned: vm.arena.list_elements_scanned(),
    }
}

#[test]
fn copying_a_list_of_integers_neither_rebuilds_nor_reads_shared_bodies() {
    // Destructuring one list while growing another is the shape that made the
    // collector rebuild the whole remaining input on every step. The cost is
    // pinned structurally: copies must stay proportional to the input, not to
    // its square. At these sizes the quadratic version reaches n^2/2 —
    // 80,000 and 320,000 — so the two curves are never within reach of the
    // same bound.
    //
    // Integers are immediates, so this is also the case that gets the whole fix:
    // the body is skipped without being read, and the reads go to zero along
    // with the copies. `copying_a_list_of_strings_still_reads_shared_bodies_quadratically`
    // is the same program over the element type that does not get that, and it
    // is the honest half of this pair — do not read this test as a statement
    // about lists in general.
    const BUDGET_PER_ELEMENT: u64 = 4;
    let small = list_copy_cost(400, "Int", "n");
    let large = list_copy_cost(800, "Int", "n");

    assert!(
        small.copied <= 400 * BUDGET_PER_ELEMENT && large.copied <= 800 * BUDGET_PER_ELEMENT,
        "list body copies are not linear in the input: \
         n=400 copied {} elements, n=800 copied {}",
        small.copied,
        large.copied,
    );
    assert_eq!(
        (small.scanned, large.scanned),
        (0, 0),
        "a list of immediates was read element by element; the whole point of \
         the all-immediate flag is that this body never has to be looked at",
    );
}

/// The control for the test above, and the limit of this fix.
///
/// Same program, same shape, one difference: the elements carry a heap index, so
/// the all-immediate escape does not apply and the collector has to read the
/// body on every step to find out that almost nothing in it moved. The copies
/// stay linear — the memory half of #886 is fixed for every element type — but
/// the reads are still n^2/2, and the wall clock still follows the reads: over
/// `aver run --release` this program takes 464 ms at n = 16,000, 7.1 s at
/// 64,000 and 28.3 s at 128,000, a measured exponent of 2.0, while the
/// `List<Int>` version above goes 34 / 49 / 71 ms.
///
/// So `list_elements_copied` alone reports linear growth on a program that is
/// still quadratic. This test exists so that nobody reads the pair as "lists
/// were made linear": they were not. Lists of *immediates* were. If a later
/// change does make this one linear, this test is supposed to fail — retire it
/// deliberately, do not relax the bound.
///
/// Note that "String" is not the dividing line. A string of five UTF-8 bytes or
/// fewer is NaN-boxed inline and behaves like the `Int` case; `item-1` is six,
/// which is why the elements here are spelled the way they are. The line is
/// whether the element carries a heap index at all, which also puts records,
/// variants, tuples, nested lists and big integers on this side of it.
#[test]
fn copying_a_list_of_strings_still_reads_shared_bodies_quadratically() {
    const BUDGET_PER_ELEMENT: u64 = 4;
    let small = list_copy_cost(400, "String", "\"item-{n}\"");
    let large = list_copy_cost(800, "String", "\"item-{n}\"");

    assert!(
        small.copied <= 400 * BUDGET_PER_ELEMENT && large.copied <= 800 * BUDGET_PER_ELEMENT,
        "list body copies are not linear in the input, which is the half of \
         #886 that is supposed to be fixed for every element type: \
         n=400 copied {} elements, n=800 copied {}",
        small.copied,
        large.copied,
    );
    // Measured: 80,600 reads at n=400 and 321,200 at n=800, both n^2/2 to within
    // the linear term. The bound is set at half of that so an ordinary shift in
    // how often the collector runs cannot flip it, while still sitting some
    // twenty-five times above anything a linear traversal could reach.
    assert!(
        small.scanned >= 400 * 400 / 4 && large.scanned >= 800 * 800 / 4,
        "reads over a heap-backed list body came in far under n^2/2 — if that is \
         a real improvement, this test has served its purpose and should be \
         retired deliberately rather than loosened: n=400 read {}, n=800 read {}",
        small.scanned,
        large.scanned,
    );
    assert!(
        large.scanned >= 3 * small.scanned,
        "doubling the input less than tripled the reads, so this program is no \
         longer the quadratic control this test is here to be: \
         n=400 read {}, n=800 read {}",
        small.scanned,
        large.scanned,
    );
}
