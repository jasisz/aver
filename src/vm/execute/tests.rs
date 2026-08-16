use super::VM;
use crate::nan_value::{Arena, ArenaEntry, ArenaList, ArenaSymbol, NanValue, NanValueConvert};
use crate::vm::opcode::{CONCAT, LOAD_CONST, LOAD_LOCAL, RETURN, VECTOR_SET_OR_KEEP};
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
            ArenaEntry::Tuple(items) | ArenaEntry::Vector { items, .. } => {
                for item in items.iter().copied() {
                    assert_no_young_refs(item, arena, context);
                }
            }
            ArenaEntry::Map { map, .. } => {
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

/// The count itself, away from any program: cells naming a slot, cells naming a
/// different one, and the tag question underneath.
///
/// Two handles that name the SAME arena slot under DIFFERENT tags both hold it —
/// the entry is what a mutation would be observed through, not the wrapper the
/// handle happens to wear. A predicate written as raw-bit equality would miss
/// that pair, and missing a holder is the one direction that cannot be allowed:
/// it reads as unique and a decision taken on it would rewrite something still
/// reachable.
#[test]
fn a_slot_is_held_by_every_cell_naming_it_whatever_tag_it_wears() {
    let mut arena = Arena::new();
    let held = arena.push_string("held");
    let other = arena.push_string("other");
    let mut vm = VM::new(CodeStore::new(), Vec::new(), arena);

    let handle = NanValue::new_string(held);
    let elsewhere = NanValue::new_string(other);
    assert!(
        vm.slot_is_unheld(handle),
        "an empty stack holds nothing, so the slot starts unheld",
    );
    assert!(
        !vm.slot_is_unheld(NanValue::new_int_inline(7)),
        "an immediate names no slot, so it can never be the unique holder of one",
    );

    vm.stack.push(handle);
    vm.stack.push(elsewhere);
    assert_eq!(vm.live_refs_to_slot(held), 1);
    assert_eq!(vm.live_refs_to_slot(other), 1);
    assert!(!vm.slot_is_unheld(handle));

    // A `Some(..)` over the same arena entry is a different NanValue with the
    // same heap index — a second holder, not a second slot.
    vm.stack.push(NanValue::new_some(held));
    assert_eq!(vm.live_refs_to_slot(held), 2);
    assert_eq!(vm.live_slot_refs(), 3);

    vm.stack.pop();
    vm.stack.pop();
    assert_eq!(vm.live_refs_to_slot(held), 1);
    assert!(!vm.slot_is_unheld(handle));

    vm.stack.pop();
    assert_eq!(vm.live_refs_to_slot(held), 0);
    assert_eq!(vm.live_slot_refs(), 0);
    assert!(vm.slot_is_unheld(handle));
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
        inplace_write_escaped: false,
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

/// Like [`compile_vm`], plus the two annotation passes the ownership decision
/// is made from: `last_use` marks the reads a slot never survives, and the
/// alias pass flags the collection params a caller might still hold. Without
/// them every slot looks dead-on-arrival and un-flagged, so the VM never emits
/// its owned-builtin call at all and a test could not tell a consumed
/// accumulator from a copied one.
fn compile_vm_with_ownership(src: &str) -> VM {
    let mut lexer = Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex failed");
    let mut parser = Parser::new(tokens);
    let mut items = parser.parse().expect("parse failed");
    crate::ir::pipeline::tco(&mut items);
    crate::ir::pipeline::resolve(&mut items);
    crate::ir::pipeline::last_use(&mut items);
    crate::ir::alias::annotate_program_alias_slots(&mut items);

    let mut arena = Arena::new();
    let symbols = crate::ir::SymbolTable::build(&items, &[]);
    let resolved = crate::ir::hir::resolve_program(&symbols, &items);
    let (code, globals) =
        vm::compile_program_with_mir_fallback(&resolved, &symbols, &mut arena, None)
            .expect("compile failed");
    VM::new(code, globals, arena)
}

/// What one run of the map-building shape from issue #900 cost: entries a
/// builtin duplicated because it had to preserve the map it was handed, and
/// entries the collector read while deciding whether a live map had to move.
///
/// The same split as [`ListCopyCost`], and the same reason for it. The copies
/// are the memory half — the half that put 8 GB behind 20,000 short string
/// pairs — and zero there does not mean the fold got cheap. The reads are what
/// the residual time follows, and for a map there is no element type that
/// escapes them.
struct MapBuildCost {
    copied: u64,
    scanned: u64,
}

/// Run the fold at size `n`: `n` inserts into one accumulator threaded through
/// a tail call.
///
/// `seed` is the expression the accumulator starts from, and it is the only
/// thing that varies between the seed tests below: what the fold does with the
/// map is identical, so a difference in the copies is a statement about the
/// seed alone. `key_ty`/`val_ty` and the two expressions vary independently, so
/// the same shape can be run over a map in which nothing can ever relocate.
fn map_build_cost(
    n: i64,
    seed: &str,
    seed_entries: i64,
    key_ty: &str,
    val_ty: &str,
    key: &str,
    value: &str,
) -> MapBuildCost {
    let src = format!(
        "fn build(n: Int, acc: Map<{key_ty}, {val_ty}>) -> Map<{key_ty}, {val_ty}>\n    match n > 0\n        true -> build(n - 1, Map.set(acc, {key}, {value}))\n        false -> acc\n\nfn main() -> Int\n    Map.len(build({n}, {seed}))\n"
    );
    let mut vm = compile_vm_with_ownership(&src);
    let result = vm.run().expect("map build program should run");
    assert_eq!(result.as_int(&vm.arena), n + seed_entries);
    MapBuildCost {
        copied: vm.arena.map_entries_copied(),
        scanned: vm.arena.map_entries_scanned(),
    }
}

/// The string-keyed fold from issue #900 at size `n`, seeded from a `seed` that
/// already holds `seed_entries` keys none of the fold's own keys collide with.
fn map_build_copies(n: i64, seed: &str, seed_entries: i64) -> u64 {
    map_build_cost(
        n,
        seed,
        seed_entries,
        "String",
        "String",
        "\"k{n}\"",
        "\"v{n}\"",
    )
    .copied
}

/// The same fold, with the seed bound to a name before it is passed.
///
/// Identical to [`map_build_copies`] except for one `let`, which is the whole
/// point: the fold does the same work, so a difference in the copies is a
/// statement about naming the seed and nothing else.
fn map_build_copies_from_named_seed(n: i64, seed: &str) -> u64 {
    let src = format!(
        "fn seedPairs() -> List<Tuple<String, String>>\n    [(\"s\", \"z\")]\n\nfn build(n: Int, acc: Map<String, String>) -> Map<String, String>\n    match n > 0\n        true -> build(n - 1, Map.set(acc, \"k{{n}}\", \"v{{n}}\"))\n        false -> acc\n\nfn main() -> Int\n    seed = {seed}\n    Map.len(build({n}, seed))\n"
    );
    let mut vm = compile_vm_typechecked(&src);
    let result = vm.run().expect("map build program should run");
    assert_eq!(result.as_int(&vm.arena), n + 1);
    vm.arena.map_entries_copied()
}

#[test]
fn growing_a_map_seeded_from_a_named_from_list_result_consumes_it_too() {
    // Freshness is decided twice, by two lists that have to agree.
    // `own_param::uniquely_owned` reads the call argument, so it sees
    // `Map.fromList([])` written inline at the call. `alias.rs`'s
    // `is_fresh_collection_builtin` decides a *binding*, and that is the list a
    // named seed goes through — `slot_owned` answers false for a flagged
    // non-param slot without ever looking at what the binding was built from.
    // So while `Map.fromList` was in one list and not the other, hoisting the
    // seed into a `let` put the whole of issue #900 back: n^2/2 entries copied,
    // 79,800 at n=400 and 319,600 at n=800.
    //
    // Naming a value cannot make it shared, and the control below says so: the
    // literal seed costs the same nothing under either spelling.
    let small = map_build_copies_from_named_seed(400, "Map.fromList(seedPairs())");
    let large = map_build_copies_from_named_seed(800, "Map.fromList(seedPairs())");
    let literal_small = map_build_copies_from_named_seed(400, "{\"s\" => \"z\"}");

    assert_eq!(
        (small, large),
        (0, 0),
        "a named `Map.fromList` seed was preserved rather than consumed while \
         the same seed written inline was free: n=400 copied {small} entries, \
         n=800 copied {large}",
    );
    assert_eq!(
        literal_small, 0,
        "the control moved: a named map-literal seed copied {literal_small} \
         entries, so this test is no longer isolating the fromList spelling",
    );
}

/// Like [`compile_vm_with_ownership`], but through the real pipeline, typecheck
/// included — which is what any test about a *binding*'s ownership needs.
///
/// The subset helper above stops short of a typecheck, so every binding slot
/// keeps `Type::Invalid` and `ir::alias`'s destination half, guarded by
/// `slot_is_collection`, never fires on a local at all. Param ownership is
/// unaffected (params get their types from the signature), which is why the
/// seed tests are fine on the subset; a named collection is not.
fn compile_vm_typechecked(src: &str) -> VM {
    let mut items = crate::source::parse_source(src).expect("parse failed");
    let result = crate::ir::pipeline::run(
        &mut items,
        crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        },
    );
    let tc = result.typecheck.as_ref().expect("typecheck requested");
    assert!(tc.errors.is_empty(), "typecheck failed: {:?}", tc.errors);

    let mut arena = Arena::new();
    let (code, globals) = vm::compile_program_with_mir_fallback(
        &result.resolved_items,
        &result.symbol_table,
        &mut arena,
        None,
    )
    .expect("compile failed");
    VM::new(code, globals, arena)
}

/// Entries duplicated by one `Map.set` written onto a NAMED `Map.fromList`
/// result of `n` entries — the intra-procedural half of the freshness question.
///
/// The pairs come from a recursive builder rather than a list literal on
/// purpose: a `Map.fromList` of literal pairs folds away at compile time and
/// the program then copies nothing whatever the analysis believes.
fn set_on_named_from_list_copies(n: i64) -> u64 {
    let src = format!(
        "fn pairs(n: Int, acc: List<Tuple<String, String>>) -> List<Tuple<String, String>>\n    match n > 0\n        true -> pairs(n - 1, List.prepend((\"k{{n}}\", \"v{{n}}\"), acc))\n        false -> acc\n\nfn main() -> Int\n    m = Map.fromList(pairs({n}, []))\n    Map.len(Map.set(m, \"z\", \"9\"))\n"
    );
    let mut vm = compile_vm_typechecked(&src);
    let result = vm.run().expect("map set program should run");
    assert_eq!(result.as_int(&vm.arena), n + 1);
    vm.arena.map_entries_copied()
}

#[test]
fn setting_on_a_named_from_list_result_does_not_copy_it() {
    // Freshness is decided by two lists that have to agree, and they were one
    // name apart. `own_param::uniquely_owned` reads a call ARGUMENT, so it sees
    // `Map.fromList(..)` written inline at a call site. `alias.rs`'s
    // `is_fresh_collection_builtin` decides a BINDING, and it is what a named
    // result goes through — `Vector.fromList` was in it and `Map.fromList` was
    // not, so `m = Map.fromList(..)` stayed flagged as possibly-shared and the
    // very next `Map.set` preserved the whole map by duplicating it.
    //
    // The cost is the map's own size, once — 400 entries here and 800 there, not
    // n^2/2 — because one binding is flagged, not a whole fold. That is the
    // difference between this and the seed tests above, and it is why this shape
    // is the one the missing name is visible in at all.
    let small = set_on_named_from_list_copies(400);
    let large = set_on_named_from_list_copies(800);

    assert_eq!(
        (small, large),
        (0, 0),
        "`Map.set` on a named `Map.fromList` result duplicated the whole map to \
         preserve something nothing else can reach: n=400 copied {small} \
         entries, n=800 copied {large}",
    );
}

#[test]
fn growing_a_map_seeded_from_from_list_consumes_it_instead_of_copying_it() {
    // `Map.fromList([])` is how a program with no map literal in reach spells an
    // empty accumulator, and it used to be the difference between a linear fold
    // and a quadratic one: the ownership analysis knew a map literal is a fresh
    // handle but not that `Map.fromList` is one too, so the accumulator stayed
    // flagged as possibly-shared and every insert preserved the whole map it was
    // handed. That is n^2/2 entries — 79,800 here and 319,600 at n=800 — against
    // a fold that needs to copy nothing at all.
    let small = map_build_copies(400, "Map.fromList([])", 0);
    let large = map_build_copies(800, "Map.fromList([])", 0);

    assert_eq!(
        (small, large),
        (0, 0),
        "an accumulator nothing else can reach was preserved rather than \
         consumed: n=400 copied {small} entries, n=800 copied {large}",
    );
}

#[test]
fn growing_a_map_seeded_from_a_literal_consumes_it_instead_of_copying_it() {
    // The control the test above is measured against: same fold, same inserts,
    // seeded from `{}`. This one was already free, and it stays that way.
    let small = map_build_copies(400, "{}", 0);
    let large = map_build_copies(800, "{}", 0);

    assert_eq!(
        (small, large),
        (0, 0),
        "a literal-seeded accumulator was preserved rather than consumed: \
         n=400 copied {small} entries, n=800 copied {large}",
    );
}

#[test]
fn a_non_empty_map_literal_seed_costs_nothing_the_fold_after_it_does_not() {
    // `{}` is the cheapest possible control, because a literal with no entries
    // does no work at all. A literal with entries used to be the opposite: it
    // lowers to a `LOAD_CONST` of the empty map plus one plain `CALL_BUILTIN`
    // MapSet per entry (`vm/compiler/mir.rs`, the `MirExpr::MapLiteral` arm),
    // the compiler declines every one of them, and writing k entries therefore
    // duplicated 0+1+...+(k-1) entries.
    //
    // The runtime decision sees through all of them. The first insert targets
    // the empty map the chunk holds as a constant, which every re-evaluation of
    // the literal shares — that one is refused, and duplicating an empty table
    // costs nothing anyway. The two after it target entries this evaluation has
    // just built and handed to nobody, so they are consumed, and the three
    // copies the seed used to cost are gone.
    //
    // The n=400 / n=800 pair is still what makes this a statement about the seed
    // rather than about the fold: an unowned insert INSIDE the fold would grow
    // with n and show up as a difference between the two.
    let seed = "{\"a\" => \"1\", \"b\" => \"2\", \"c\" => \"3\"}";
    let small = map_build_copies(400, seed, 3);
    let large = map_build_copies(800, seed, 3);

    assert_eq!(
        (small, large),
        (0, 0),
        "a three-entry literal seed must cost nothing and the fold after it \
         must cost nothing: n=400 copied {small} entries, n=800 copied {large}",
    );
}

/// Entries duplicated while turning a list of `n` pairs into a map with one
/// `Map.fromList` call.
fn map_from_list_copies(n: i64) -> u64 {
    let src = format!(
        "fn pairs(n: Int, acc: List<Tuple<String, String>>) -> List<Tuple<String, String>>\n    match n > 0\n        true -> pairs(n - 1, List.prepend((\"k{{n}}\", \"v{{n}}\"), acc))\n        false -> acc\n\nfn main() -> Int\n    Map.len(Map.fromList(pairs({n}, [])))\n"
    );
    let mut vm = compile_vm_with_ownership(&src);
    let result = vm.run().expect("fromList program should run");
    assert_eq!(result.as_int(&vm.arena), n);
    vm.arena.map_entries_copied()
}

#[test]
fn building_a_map_from_a_list_of_pairs_does_not_rebuild_the_table_per_entry() {
    // `Map.fromList(pairs)` is how a log or a decoded document becomes a map,
    // and it was quadratic on its own — nothing to do with which seed a fold
    // uses. The builder held the map under construction and inserted through
    // `AverMap::insert`, which takes `&self` and therefore has to preserve what
    // it is handed: `Rc::make_mut` rebuilt the whole table once per entry.
    // Building 400 entries duplicated 79,800 of them and 800 duplicated 319,600
    // — n^2/2, measured with the counter this test reads.
    //
    // The map under construction is unreachable from anywhere else, so the owned
    // insert is what belongs here, and it duplicates nothing.
    let small = map_from_list_copies(400);
    let large = map_from_list_copies(800);

    assert_eq!(
        (small, large),
        (0, 0),
        "Map.fromList rebuilt its own table while filling it: n=400 copied \
         {small} entries, n=800 copied {large}",
    );
}

#[test]
fn folding_over_a_map_of_immediates_never_reads_it() {
    // A `Map<Int, Int>` holds nothing a collection could relocate, so rewriting
    // its entries is provably the identity and the collector has no reason to
    // look at them. It used to look anyway, once per entry per collection: this
    // fold read 80,600 entries at n=400 and 321,200 at n=800 — n^2/2, the shape
    // of the whole residual time of issue #900.
    //
    // The fold is run at the allocation rate of the heap-backed control below,
    // by building a throwaway string on every step that it does not store.
    // Without that the collector hardly ever runs and a zero here would say
    // nothing about the escape.
    let small = map_build_cost(400, "{}", 0, "Int", "Int", "n", "String.len(\"pad{n}\")");
    let large = map_build_cost(800, "{}", 0, "Int", "Int", "n", "String.len(\"pad{n}\")");

    assert_eq!(
        (small.scanned, large.scanned),
        (0, 0),
        "a map of immediates was read entry by entry; the whole point of the \
         all-immediate flag is that this table never has to be looked at",
    );
    assert_eq!(
        (small.copied, large.copied),
        (0, 0),
        "the fold duplicated entries it did not have to",
    );
}

#[test]
fn folding_over_a_map_of_heap_backed_pairs_still_reads_it_on_every_step() {
    // The control for the test above, and the limit of this fix — the same role
    // `copying_a_list_of_strings_still_reads_shared_bodies_quadratically` plays
    // for lists. Same fold, same sizes, one difference: the keys and values
    // carry heap indices, so nothing can be proved about them without reading
    // them and the walk stays n^2/2.
    //
    // The bounds are the ones the retired issue-#905 tripwire held the
    // heap-backed half to, unchanged. If a later change makes this program
    // linear too, this test is supposed to fail — retire it deliberately rather
    // than loosen it.
    //
    // "String" is not the dividing line: a string of five UTF-8 bytes or fewer
    // is NaN-boxed inline and belongs with the immediates. `"k{n}"` at these
    // sizes is four, which is why the values here are spelled long enough to
    // reach the heap.
    let small = map_build_cost(
        400,
        "{}",
        0,
        "String",
        "String",
        "\"key-no-{n}\"",
        "\"val-no-{n}\"",
    );
    let large = map_build_cost(
        800,
        "{}",
        0,
        "String",
        "String",
        "\"key-no-{n}\"",
        "\"val-no-{n}\"",
    );

    assert_eq!(
        (small.copied, large.copied),
        (0, 0),
        "the fold duplicated entries it did not have to",
    );
    assert!(
        small.scanned > 0 && large.scanned > 0,
        "a live map holding heap indices was never read at all, which cannot be \
         right: skipping it would leave stale arena indices behind. n=400 read \
         {}, n=800 read {}",
        small.scanned,
        large.scanned,
    );
    // Doubling n doubles both the map's size and the number of collections that
    // walk it, so the reads go up by 4. Requiring 3 leaves a third of that
    // headroom while still excluding every linear curve, which could only
    // reach 2.
    assert!(
        large.scanned >= 3 * small.scanned,
        "doubling the input less than tripled the reads, so this is no longer \
         the quadratic control this test is here to be: n=400 read {}, n=800 \
         read {}",
        small.scanned,
        large.scanned,
    );
    // The same statement per entry, which is what makes it a claim about the
    // walk rather than about how much the program allocates.
    let small_per_entry = small.scanned / 400;
    let large_per_entry = large.scanned / 800;
    assert!(
        large_per_entry >= 3 * small_per_entry / 2,
        "reads per entry barely grew, so the walk is no longer repeating over \
         the whole map as it fills: n=400 read {} per entry, n=800 read {} per \
         entry",
        small_per_entry,
        large_per_entry,
    );
}

/// Entries the collector reads while a map built by `built` is held live across
/// a stretch of allocation that does not touch it.
///
/// The map is a `Map<Int, String>` whose values are one byte long, so they are
/// NaN-boxed inline and the whole table is immediate. `built` is the only thing
/// that varies, which makes any difference in the reads a statement about what
/// was written into the map and nothing else.
fn map_held_live_scans(built: &str) -> u64 {
    let src = format!(
        "fn build(n: Int, acc: Map<Int, String>) -> Map<Int, String>\n    match n > 0\n        true -> build(n - 1, Map.set(acc, n, \"s\"))\n        false -> acc\n\nfn churn(n: Int, m: Map<Int, String>, acc: Int) -> Int\n    match n > 0\n        true -> churn(n - 1, m, acc + String.len(\"padding-{{n}}\") + Map.len(m))\n        false -> acc\n\nfn main() -> Int\n    m = {built}\n    churn(4000, m, 0)\n"
    );
    let mut vm = compile_vm_with_ownership(&src);
    vm.run().expect("map churn program should run");
    vm.arena.map_entries_scanned()
}

#[test]
fn writing_a_heap_backed_value_into_a_map_of_immediates_puts_the_reads_back() {
    // The flag is a claim about content, so every write has to re-establish it,
    // and the direction that matters is losing it: a map that was all-immediate
    // and then had something relocatable written into it must go back to being
    // walked. Getting this wrong is silent — the collector would skip a table
    // holding arena indices and leave them pointing at where the values used to
    // be.
    //
    // Which write path this exercises: `Map.set` on a call result is not a
    // local, so the owned bit never fires and this goes through the SHARED
    // path (`set_nv` -> `push_map`, flag proved by a full scan). The O(1)
    // derivation in `set_nv_owned` — the one place the flag is derived rather
    // than proved — is pinned by
    // `folding_over_a_map_of_heap_backed_pairs_still_reads_it_on_every_step`:
    // its fold threads the accumulator as a bare param local (owned path,
    // `copied == (0,0)` proves it fires) and writes heap-backed pairs, so
    // dropping either `is_immediate()` conjunct there turns its
    // `scanned > 0` red. Do not remove that control without replacing this pin.
    //
    // Same map, same held-live stretch, one extra `Map.set` between them. The
    // value it writes is ten bytes, past the five that fit in a NaN box, so it
    // is the one entry in the table that carries a heap index.
    let immediate = map_held_live_scans("build(400, {})");
    let degraded = map_held_live_scans("Map.set(build(400, {}), 0, \"heapbacked\")");

    assert_eq!(
        immediate, 0,
        "a map built only out of immediates was read {immediate} times",
    );
    // At least the whole table once, so this cannot be satisfied by a single
    // stray read of some other map the program happens to hold.
    assert!(
        degraded >= 400,
        "one heap-backed value was written into an all-immediate map and the \
         collector went on skipping it, which leaves that value's arena index \
         un-rewritten after it moves: read {degraded} entries",
    );
}

/// Map entries the collector read while running the in-place-write shape.
///
/// The number is the descent's cost in the only form that does not need a
/// stopwatch. A map is one arena entry and carries no all-immediate escape, so
/// a boundary that descends into an out-of-region map reads it entry by entry
/// and one that leaves it alone reads nothing. The map is carried across every
/// boundary unchanged, so every entry in this count is a boundary that chose to
/// descend into it.
fn inplace_write_scan_cost(src: &str) -> u64 {
    let mut vm = compile_vm_with_ownership(src);
    vm.run().expect("in-place write program should run");
    vm.arena.map_entries_scanned()
}

/// One in-place write in `main`, then `n` iterations of a loop that runs in a
/// frame of its own.
fn armed_once_then_many_frames(n: i64) -> u64 {
    let src = format!(
        "fn buildMap(n: Int, acc: Map<Int, String>) -> Map<Int, String>\n    match n > 0\n        true -> buildMap(n - 1, Map.set(acc, n, \"row-{{n}}\"))\n        false -> acc\n\nfn touch(v: Vector<String>, s: String) -> Vector<String>\n    Option.withDefault(Vector.set(v, 0, s), v)\n\nfn spin(m: Map<Int, String>, i: Int, n: Int, tag: String) -> Int\n    match i >= n\n        true -> Map.len(m) + String.len(tag)\n        false -> spin(m, i + 1, n, String.toUpper(tag))\n\nfn main() -> Int\n    m = buildMap(32, {{}})\n    v = touch(Vector.new(2, \"seed-string\"), String.toUpper(\"payload-marker\"))\n    spin(m, 0, {n}, \"tag-value\") + Vector.len(v)\n"
    );
    inplace_write_scan_cost(&src)
}

/// The same `n` iterations, with the write inside the looping frame so every
/// boundary is armed again.
fn writing_at_every_boundary(n: i64) -> u64 {
    let src = format!(
        "fn buildMap(n: Int, acc: Map<Int, String>) -> Map<Int, String>\n    match n > 0\n        true -> buildMap(n - 1, Map.set(acc, n, \"row-{{n}}\"))\n        false -> acc\n\nfn spin(v: Vector<String>, m: Map<Int, String>, i: Int, n: Int, tag: String) -> Int\n    match i >= n\n        true -> Map.len(m) + Vector.len(v) + String.len(tag)\n        false -> spin(Option.withDefault(Vector.set(v, 0, String.toUpper(\"payload-{{i}}\")), v), m, i + 1, n, String.toUpper(tag))\n\nfn main() -> Int\n    m = buildMap(32, {{}})\n    spin(Vector.new(2, \"seed-string\"), m, 0, {n}, \"tag-value\")\n"
    );
    inplace_write_scan_cost(&src)
}

/// The flag an in-place write sets is a property of ONE frame, and this is the
/// receipt for it: a write early in `main` costs the fifty or hundred frames
/// that run afterwards exactly nothing.
///
/// A frame is pushed with the flag clear and only a write of its own — or a
/// callee handing one up on return — ever sets it, so the cost of arming a
/// frame is bounded by that frame's own boundaries. It does not become a tax
/// on the rest of the program. Doubling the iteration count leaves the number
/// unchanged to the entry; the constant that remains is the map being built
/// before the loop starts.
#[test]
fn an_early_in_place_write_does_not_arm_the_frames_that_run_after_it() {
    let fifty = armed_once_then_many_frames(50);
    let hundred = armed_once_then_many_frames(100);
    assert_eq!(
        fifty, hundred,
        "doubling the number of frames that run after one in-place write \
         changed what the collector read: 50 iterations read {fifty} map \
         entries, 100 read {hundred}",
    );
}

/// The control, and the honest half of the pair.
///
/// Same loop, same carried map, one difference: the write is inside the
/// looping frame, so every boundary is armed again and every boundary descends.
/// The reads then grow with the iteration count times the size of what the
/// frame carries — the quadratic this fix knowingly pays on the shape that used
/// to return the wrong answer, and what a remembered set at element granularity
/// would be for.
///
/// It is also what keeps the test above from being vacuous: the counter does
/// move when a boundary descends.
#[test]
fn writing_at_every_boundary_reads_the_carried_map_at_every_boundary() {
    let fifty = writing_at_every_boundary(50);
    let hundred = writing_at_every_boundary(100);
    let extra = hundred - fifty;
    assert!(
        extra >= 50 * 32,
        "fifty more armed boundaries did not read the 32-entry map they carry \
         fifty more times: 50 iterations read {fifty}, 100 read {hundred}",
    );
}

/// The depth-0 return boundary, driven through the real dispatch loop.
///
/// `call_function` records `caller_depth = frames.len()`, so the callee of a
/// host callback, an oracle stub or an HTTP handler returns through
/// `finalize_frame_return` rather than through either caller path — and a
/// vector handed to it can already be in stable, because that is where this
/// same boundary puts every result it returns.
///
/// The arming is correct here: stable is not frame-local, so the write sets
/// `inplace_write_escaped` and the barrier keeps the frame off the
/// young-truncate fast return. What it lands on instead is the path that
/// promotes to stable — which used to hand a stable root back unread and then
/// truncate the payload away underneath it.
#[test]
fn a_stable_vector_written_in_place_survives_the_depth_zero_return() {
    let mut code = CodeStore::new();
    let writer_id = code.add_function(FnChunk {
        name: "writer".to_string(),
        arity: 1,
        local_count: 1,
        // vec, 0, ("PAYLOAD-" ++ "MARKER") -> owned in-place set -> return vec.
        // The concatenation is what makes the payload a FRESH allocation above
        // the frame's mark; a string constant is already in the arena and would
        // never have been dropped.
        code: vec![
            LOAD_LOCAL,
            0,
            LOAD_CONST,
            0,
            0,
            LOAD_CONST,
            0,
            1,
            LOAD_CONST,
            0,
            2,
            CONCAT,
            VECTOR_SET_OR_KEEP,
            1,
            RETURN,
        ],
        constants: Vec::new(),
        effects: Vec::new(),
        thin: true,
        parent_thin: false,
        leaf: false,
        no_alloc: false,
        source_file: String::new(),
        line_table: Vec::new(),
    });

    // The vector and the two string constants all start in stable, which is
    // where this boundary leaves everything it returns.
    let mut arena = Arena::new();
    let mut promoted = [
        NanValue::new_vector(arena.push_vector(vec![NanValue::new_int_inline(0)])),
        NanValue::new_string(arena.push_string("PAYLOAD-")),
        NanValue::new_string(arena.push_string("MARKER")),
    ];
    arena.promote_roots_to_stable(&mut promoted, false);
    let vector = promoted[0];
    code.functions[writer_id as usize].constants =
        vec![NanValue::new_int_inline(0), promoted[1], promoted[2]];

    // A suspended caller frame, so `call_function` records a caller depth of 1
    // and the writer's own return is the depth boundary.
    let mut vm = VM::new(code, Vec::new(), arena);
    vm.frames.push(CallFrame {
        fn_id: writer_id,
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
        inplace_write_escaped: false,
        thin: true,
        parent_thin: false,
    });
    vm.start_profiling();

    let result = vm
        .call_function(writer_id, &[vector])
        .expect("writer should return");

    let report = vm.profile_report().expect("profiling should be enabled");
    assert_eq!(
        (
            report.returns.thin_fast_returns,
            report.returns.young_truncate_fast_returns,
            report.returns.thin_slow_returns,
        ),
        (0, 0, 1),
        "the write did not reroute the return onto the promoting boundary, so \
         this is no longer a test of that boundary",
    );

    // Whatever runs next takes the slot the payload had.
    let _filler = NanValue::new_string(vm.arena.push_string("JUNK-FILLER-ONE"));
    let element = vm.arena.vector_ref_value(result)[0];
    assert_eq!(
        vm.arena.get_string_value(element).to_string(),
        "PAYLOAD-MARKER",
        "the depth-0 return dropped the element written into a stable vector",
    );
}

// ── The vector fence: a static `Vector.set` grant is confirmed or
//    revoked at run time ─────────────────────────────────────────────
//
// The owned `Vector.set` empties its target's arena slot with
// `mem::take`, and the match-binder hole showed a static grant reaching
// a container-held vector — the container read back an empty vector,
// silently. `runtime_confirms_vector_grant` is the fence in front of
// that take; these pin its four answers, one bucket each, away from any
// program so the states are exact.

#[test]
fn a_vector_grant_is_confirmed_when_nothing_else_holds_the_slot() {
    let mut arena = Arena::new();
    let idx = arena.push_vector(vec![NanValue::new_int_inline(1)]);
    let mut vm = VM::new(CodeStore::new(), Vec::new(), arena);

    assert!(vm.runtime_confirms_vector_grant(NanValue::new_vector(idx)));
    let stats = vm.vector_ownership_stats();
    assert_eq!(
        (stats.grants, stats.refused_stack_holder),
        (1, 0),
        "an unheld fresh vector is exactly what the static grant claimed"
    );
}

#[test]
fn a_vector_grant_is_revoked_when_a_container_holds_the_slot() {
    let mut arena = Arena::new();
    let idx = arena.push_vector(vec![NanValue::new_int_inline(1)]);
    let handle = NanValue::new_vector(idx);
    // A tuple pushed over the handle is an off-stack holder; `Arena::push`
    // marks the vector `held_elsewhere` on the way in.
    let _tuple = arena.push_tuple(vec![handle, NanValue::new_int_inline(2)]);
    let mut vm = VM::new(CodeStore::new(), Vec::new(), arena);

    assert!(
        !vm.runtime_confirms_vector_grant(handle),
        "an arena entry still holds the slot the owned path would empty"
    );
    assert_eq!(vm.vector_ownership_stats().refused_off_stack_holder, 1);
}

#[test]
fn a_vector_grant_is_revoked_while_a_stack_cell_still_holds_the_slot() {
    let mut arena = Arena::new();
    let idx = arena.push_vector(vec![NanValue::new_int_inline(1)]);
    let handle = NanValue::new_vector(idx);
    let mut vm = VM::new(CodeStore::new(), Vec::new(), arena);

    vm.stack.push(handle);
    assert!(
        !vm.runtime_confirms_vector_grant(handle),
        "a live cell would observe the in-place mutation"
    );
    assert_eq!(vm.vector_ownership_stats().refused_stack_holder, 1);

    vm.stack.pop();
    assert!(
        vm.runtime_confirms_vector_grant(handle),
        "the same slot with the cell gone is uniquely held again"
    );
    assert_eq!(vm.vector_ownership_stats().grants, 1);
}

#[test]
fn a_vector_grant_is_revoked_unexamined_when_the_walk_outcosts_the_copy() {
    let mut arena = Arena::new();
    let idx = arena.push_vector(vec![NanValue::new_int_inline(1)]);
    let mut vm = VM::new(CodeStore::new(), Vec::new(), arena);

    // A one-element vector under a stack far past `len + WALK_SLACK`:
    // the copy is cheaper than the walk, so the fence revokes without
    // looking — and counts that it did not look, rather than reading an
    // unasked question as a clean answer.
    for _ in 0..512 {
        vm.stack.push(NanValue::new_int_inline(0));
    }
    assert!(!vm.runtime_confirms_vector_grant(NanValue::new_vector(idx)));
    assert_eq!(vm.vector_ownership_stats().unexamined_walk_too_costly, 1);
}

#[test]
fn the_empty_vector_immediate_owns_no_slot_to_grant() {
    let mut vm = VM::new(CodeStore::new(), Vec::new(), Arena::new());
    assert!(
        !vm.runtime_confirms_vector_grant(NanValue::EMPTY_VECTOR),
        "nothing to empty and nothing to keep: the copying path answers it"
    );
    let stats = vm.vector_ownership_stats();
    assert_eq!(
        (
            stats.grants,
            stats.refused_stack_holder
                + stats.refused_off_stack_holder
                + stats.unexamined_walk_too_costly
        ),
        (0, 0),
        "an immediate is not a decision, so it lands in no bucket"
    );
}

/// The dispatch wiring, end to end: a fresh last-use receiver earns the
/// static grant, the fence confirms it, and the tally says the owned
/// path really was reached — if the `CALL_BUILTIN_OWNED` route ever
/// stops consulting the fence, `grants` stays zero and this fails.
#[test]
fn a_granted_vector_set_is_confirmed_through_the_dispatch_point() {
    let mut vm = compile_vm_with_ownership(
        "fn main() -> Int\n    v = Vector.fromList([1, 2])\n    w = Option.withDefault(Vector.set(v, 0, 9), Vector.fromList([0]))\n    Option.withDefault(Vector.get(w, 0), 0 - 1)\n",
    );
    let result = vm.run().expect("vector set program should run");
    assert_eq!(result.as_int(&vm.arena), 9);
    let stats = vm.vector_ownership_stats();
    assert_eq!(
        (
            stats.grants,
            stats.refused_stack_holder + stats.refused_off_stack_holder
        ),
        (1, 0),
        "the static grant should reach the fence and be confirmed"
    );
}

/// The wrapper toll, pinned: `Vector.set` wraps its result in `Some`,
/// which boxes a heap vector, and the box marks its payload
/// held-elsewhere on the way in — so in a chain spelled through the
/// plain builtin only the FIRST granted write keeps its grant; the
/// second is revoked by the first one's own dead wrapper and copies.
/// The answer stays right either way. If this ever changes — the fence
/// learning to see through dead boxes, or the marking getting wider —
/// this test is the recorded decision that must be revisited.
#[test]
fn a_second_owned_set_in_a_non_fused_chain_pays_the_wrapper_toll() {
    let mut vm = compile_vm_with_ownership(
        "fn main() -> Int\n    v = Vector.fromList([1, 2])\n    w = Option.withDefault(Vector.set(v, 0, 9), Vector.fromList([0]))\n    u = Option.withDefault(Vector.set(w, 1, 7), Vector.fromList([0]))\n    a = Option.withDefault(Vector.get(u, 0), 0 - 1)\n    b = Option.withDefault(Vector.get(u, 1), 0 - 1)\n    a + b\n",
    );
    let result = vm.run().expect("chained vector set program should run");
    assert_eq!(result.as_int(&vm.arena), 16);
    let stats = vm.vector_ownership_stats();
    assert_eq!(
        (stats.grants, stats.refused_off_stack_holder),
        (1, 1),
        "expected the first grant confirmed and the second revoked by \
         the first result's own Some-wrapper box"
    );
}
