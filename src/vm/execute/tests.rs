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
fn a_non_empty_map_literal_seed_pays_for_its_own_entries_and_nothing_more() {
    // `{}` is the cheapest possible control, because a literal with no entries
    // does no work at all. A literal with entries does: it lowers to a
    // `LOAD_CONST` of the empty map plus one plain `CALL_BUILTIN` MapSet per
    // entry (`vm/compiler/mir.rs`, the `MirExpr::MapLiteral` arm), and those
    // inserts are not owned — the target is a stack temporary, and the first one
    // is a constant that every re-evaluation of the literal shares, so neither
    // can be consumed. Writing k entries therefore duplicates 0+1+...+(k-1)
    // entries.
    //
    // That cost is real but it is bounded by what the source says, not by what
    // the program computes: three entries here cost three copies at n=400 and
    // three at n=800. The fold itself still copies nothing, which is what makes
    // this a control rather than a second bug — an unowned insert *inside* the
    // fold would grow with n.
    let seed = "{\"a\" => \"1\", \"b\" => \"2\", \"c\" => \"3\"}";
    let small = map_build_copies(400, seed, 3);
    let large = map_build_copies(800, seed, 3);

    assert_eq!(
        (small, large),
        (3, 3),
        "a three-entry literal seed must cost 0+1+2 copies and the fold after \
         it must cost none: n=400 copied {small} entries, n=800 copied {large}",
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

/// The residual left over once the duplication is gone, and the measurement the
/// map half of issue #898 has to start from.
///
/// Copies are gone; reads are not. Every collection that sees a live map walks
/// it entry by entry to establish that nothing in it moved, and a map has no
/// counterpart of `ListBody::all_immediate` to escape that walk. The second
/// case here is the control that isolates it: a `Map<Int, Int>` in which no key
/// and no value can ever relocate, run at the same allocation rate as the
/// string one because each step builds a throwaway string it does not store.
/// Its reads come out identical to the heap-valued map's, digit for digit —
/// 80,600 at n = 400 and 321,200 at n = 800 for both, against 0 for a
/// `List<Int>` of the same size. What the map holds makes no difference at all.
///
/// The allocation rate is why the control needs the throwaway string. The same
/// `Map<Int, Int>` fold without it reads only n — 400 and 800 — not because the
/// map is skipped but because that program allocates so little that the
/// collector hardly ever runs. Reading that number as an immediate-valued
/// escape is the mistake this control exists to prevent.
///
/// Both halves are asserted quadratic on purpose. If a later change makes
/// either one linear, this test is supposed to fail — retire it deliberately
/// rather than loosen the bound.
///
/// What it is deliberately blind to is MAGNITUDE. How many entries a fold reads
/// is the map size times the number of collections that saw it, and the second
/// factor is the young collector's tuning, not anything about maps: raise the
/// threshold and every number here falls by the same constant. So the
/// assertions below are all ratios of one program against itself at two sizes,
/// where that constant cancels. A young-collector change should move the
/// figures quoted above and leave this test green; a map that stopped being
/// walked in full should fail it.
#[test]
fn a_live_map_is_read_entry_by_entry_whether_or_not_anything_in_it_can_move() {
    let heap_small = map_build_cost(400, "{}", 0, "String", "String", "\"k{n}\"", "\"v{n}\"");
    let heap_large = map_build_cost(800, "{}", 0, "String", "String", "\"k{n}\"", "\"v{n}\"");
    let imm_small = map_build_cost(400, "{}", 0, "Int", "Int", "n", "String.len(\"pad{n}\")");
    let imm_large = map_build_cost(800, "{}", 0, "Int", "Int", "n", "String.len(\"pad{n}\")");

    for (label, small, large) in [
        ("Map<String, String>", &heap_small, &heap_large),
        ("Map<Int, Int>", &imm_small, &imm_large),
    ] {
        assert_eq!(
            (small.copied, large.copied),
            (0, 0),
            "{label}: the fold duplicated entries it did not have to",
        );
        assert!(
            small.scanned > 0 && large.scanned > 0,
            "{label}: a live map was never read at all, which would mean maps \
             grew a skip like `ListBody::all_immediate` — a real improvement, \
             and the point at which this test should be retired deliberately \
             rather than loosened: n=400 read {}, n=800 read {}",
            small.scanned,
            large.scanned,
        );
        // Doubling n doubles both the map's size and the number of collections
        // that walk it, so the reads go up by 4. Requiring 3 leaves a third of
        // that headroom while still excluding every linear curve, which could
        // only reach 2.
        assert!(
            large.scanned >= 3 * small.scanned,
            "{label}: doubling the input less than tripled the reads, so this is \
             no longer the quadratic control this test is here to be: n=400 read \
             {}, n=800 read {}",
            small.scanned,
            large.scanned,
        );
        // The same statement per entry, which is what makes it a claim about
        // the walk rather than about how much the program allocates: a fold
        // that read each entry a bounded number of times would hold this ratio
        // flat.
        let small_per_entry = small.scanned / 400;
        let large_per_entry = large.scanned / 800;
        assert!(
            large_per_entry >= 3 * small_per_entry / 2,
            "{label}: reads per entry barely grew, so the walk is no longer \
             repeating over the whole map as it fills: n=400 read {} per entry, \
             n=800 read {} per entry",
            small_per_entry,
            large_per_entry,
        );
    }

    assert!(
        imm_small.scanned >= heap_small.scanned && imm_large.scanned >= heap_large.scanned,
        "a map whose keys and values can never relocate was read less than one \
         holding heap indices, which would mean maps grew an all-immediate \
         escape: immediates read {} and {}, heap-backed {} and {}",
        imm_small.scanned,
        imm_large.scanned,
        heap_small.scanned,
        heap_large.scanned,
    );
}
