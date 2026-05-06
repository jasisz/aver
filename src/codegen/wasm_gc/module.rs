//! Top-level wasm module assembly.
//!
//! Walks post-pipeline IR, assembles a wasm-gc module:
//!
//! 1. **Type section**, two layers in order:
//!    - User-type slots (records, variant constructors) — assigned by
//!      `TypeRegistry::build` so emit sites already know their indices.
//!    - Function types — one per Aver fn, plus type-0 reserved for
//!      `_start: () -> ()`.
//! 2. **Function section** — one entry per Aver fn referencing the
//!    function-type idx assigned in step 1.
//! 3. **Export section** — `_start` (always at fn idx 0) plus every
//!    user fn by name.
//! 4. **Code section** — `_start` calls `main` and drops any return
//!    value; user fns get their bodies from `body::emit_fn_body`.
//!
//! Validation runs `wasmparser` with GC + tail-call features before
//! returning bytes.

use std::collections::HashMap;

use wasm_encoder::{
    CodeSection, DataCountSection, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, Instruction, Module, TypeSection, ValType,
};

use super::WasmGcError;
use super::body::eq_helpers::{EqHelperRegistry, EqKind};
use super::body::hash_helpers::{HashHelperRegistry, HashKind};
use super::body::{FnEntry, FnMap, emit_fn_body};
use super::builtins::{BuiltinName, BuiltinRegistry};
use super::effects::{EffectName, EffectRegistry};
use super::maps::MapHelperRegistry;
use super::types::{TypeRegistry, param_types, record_struct_type, return_results};
use super::wat_helper;
use crate::types::Type as AverType;

use crate::ast::{Expr, FnDef, Stmt, TopLevel, TypeDef};

pub(super) fn emit_module(
    items: &[TopLevel],
    handler_name: Option<&str>,
) -> Result<Vec<u8>, WasmGcError> {
    let registry = TypeRegistry::build_with_handler(items, handler_name.is_some());

    // Lazy caller_fn name registry — populated during user-fn body
    // emit by `emit_caller_fn_idx` call sites. Threaded into every
    // `emit_fn_body` call via `EmitCtx::caller_fn_collector`. The
    // post-emit phase reads `collector.names` to materialise the
    // exported caller-fn name table (`__caller_fn_count` +
    // `__caller_fn_name`) and the matching passive data segments.
    let caller_fn_collector = std::cell::RefCell::new(super::body::CallerFnCollector::default());

    let fn_defs: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    // Discover used pure-builtins. Walk every fn body looking for
    // `FnCall` whose callee is `Attr(_, "method")` and the dotted
    // form is a known builtin. Discovery happens before slot
    // allocation so the registry can reserve indices in declaration
    // order.
    let mut builtin_registry = BuiltinRegistry::new();
    let mut effect_registry = EffectRegistry::new();
    let mut eq_helpers_registry = EqHelperRegistry::new();
    let mut hash_helpers_registry = HashHelperRegistry::new();
    for fd in &fn_defs {
        discover_builtins_in_fn(
            fd,
            &mut builtin_registry,
            &mut effect_registry,
            &mut eq_helpers_registry,
            &registry,
        );
    }
    // Sweep nominal element types of every registered List / Vector
    // and key types of every registered Map. The list/vec helper
    // bodies dispatch nominal element eq/hash via `Call(__eq_<X>)`
    // (since 0.16.3); without auto-registering those types here, a
    // program that holds `List<Item>` without ever writing
    // `list == list` directly would still get a list helper body
    // that calls into an unregistered `__eq_Item`. Keys of `Map<K,_>`
    // need the same: maps.rs `emit_eq_for(K)` reaches into
    // `__eq_<X>` helpers when K is a record/sum field-of-field.
    let mut nominal_seed: Vec<String> = Vec::new();
    for canonical in &registry.list_order {
        if let Some(elem) = super::types::TypeRegistry::list_element_type(canonical) {
            nominal_seed.push(elem.trim().to_string());
        }
    }
    for canonical in &registry.vector_order {
        if let Some(elem) = super::types::TypeRegistry::vector_element_type(canonical) {
            nominal_seed.push(elem.trim().to_string());
        }
    }
    for canonical in &registry.map_order {
        if let Some((k, _v)) = super::types::parse_map_kv(canonical) {
            nominal_seed.push(k.trim().to_string());
        }
    }
    for name in &nominal_seed {
        if registry.record_fields.contains_key(name) {
            eq_helpers_registry.register_transitive(name, EqKind::Record, &registry);
            hash_helpers_registry.register_transitive(name, HashKind::Record, &registry);
        } else if registry
            .variants
            .values()
            .flat_map(|v| v.iter())
            .any(|v| &v.parent == name)
        {
            eq_helpers_registry.register_transitive(name, EqKind::Sum, &registry);
            hash_helpers_registry.register_transitive(name, HashKind::Sum, &registry);
        }
    }
    // Mirror eq registry's transitive shape — every type registered
    // for eq dispatch also needs a hash helper, since list/vec/map
    // helpers and per-record/sum hash bodies dispatch through
    // `Call(__hash_<X>)` for non-primitive fields. Walk the eq
    // registry post-seed and register matching hash slots.
    let eq_snapshot: Vec<(String, EqKind)> = eq_helpers_registry
        .iter()
        .map(|(n, k)| (n.to_string(), k))
        .collect();
    for (name, kind) in &eq_snapshot {
        let hk = match kind {
            EqKind::Record => HashKind::Record,
            EqKind::Sum => HashKind::Sum,
            EqKind::OptionEq => HashKind::OptionHash,
            EqKind::ResultEq => HashKind::ResultHash,
            EqKind::TupleEq => HashKind::TupleHash,
        };
        hash_helpers_registry.register_transitive(name, hk, &registry);
    }
    // Eq helpers over records / sums with String fields need
    // `__wasmgc_string_eq` — force-register so the slot is allocated
    // before bodies emit.
    if eq_helpers_registry.needs_string_eq(&registry) {
        builtin_registry.register(BuiltinName::StringEq);
    }
    // `--handler X` shape — the synthesised `aver_http_handle`
    // wrapper reads `Request.*` and dispatches `Response.text` /
    // `Response.setHeader`, so register them up front. The user
    // handler may also touch `Http.*` / `Env.*`, which discovery
    // already picks up through `discover_builtins_in_fn`.
    if handler_name.is_some() {
        for eff in [
            EffectName::RequestMethod,
            EffectName::RequestUrl,
            EffectName::RequestQuery,
            EffectName::RequestBody,
            EffectName::RequestHeadersLoad,
            EffectName::ResponseText,
            EffectName::ResponseSetHeader,
        ] {
            effect_registry.register(eff);
        }
    }

    // List<String>/List<Char> show up as soon as the program reaches
    // for `String.split` or any List<String> literal. Their per-T
    // `contains` helper compares heads via `__wasmgc_string_eq`, so
    // force-register that builtin if any such list type is in the
    // registry — keeps slot allocation deterministic regardless of
    // whether `match` discovery already picked it up.
    if registry.list_order.iter().any(|c| c == "List<String>") {
        builtin_registry.register(BuiltinName::StringEq);
    }
    // Same trigger for `List<Record>` when the record has any
    // String field — `List.contains` over such a list does inline
    // field-by-field eq and reaches `__wasmgc_string_eq`.
    for canonical in &registry.list_order {
        if let Some(elem) = super::types::TypeRegistry::list_element_type(canonical)
            && let Some(fields) = registry.record_fields.get(elem.trim())
            && fields.iter().any(|(_, t)| t.trim() == "String")
        {
            builtin_registry.register(BuiltinName::StringEq);
            break;
        }
    }

    if fn_defs.is_empty() {
        return Err(WasmGcError::Validation(
            "module has no fn definitions".into(),
        ));
    }
    // `_start` calls `__entry__` if present (synthesised by the
    // playground / `--expr` path to wrap a user fn call with literal
    // args), otherwise `main`. Both are optional — modules that act
    // as a Worker handler (e.g. `tools/edge/handler.av`) export
    // `handler` instead and never run `_start`; when neither is
    // present, `_start` is emitted as a no-op so the module shape
    // stays valid.
    let main_idx: Option<usize> = fn_defs
        .iter()
        .position(|fd| fd.name == "__entry__")
        .or_else(|| fn_defs.iter().position(|fd| fd.name == "main"));

    let mut module = Module::new();

    // ── Type section ───────────────────────────────────────────────
    let mut types = TypeSection::new();

    // 1) User types in `TypeRegistry` order. Indices match what the
    //    registry recorded so emit sites can reference them directly.
    emit_user_types(&mut types, items, &registry)?;

    // 2) Effect import types. Imports take fn idx 0..K so their
    //    type slots come right after user types.
    let mut next_type_idx = registry.user_type_count;
    effect_registry.assign_slots(&mut next_type_idx);
    for name in effect_registry.iter() {
        let p = name.params(&registry)?;
        let r = name.results(&registry)?;
        types.ty().function(p, r);
    }

    // 3) `_start` type — () -> ().
    types.ty().function([], []);
    let start_type_idx = next_type_idx;
    next_type_idx += 1;

    // 4) One fn type per user fn. `fn_type_indices[i]` is the wasm
    //    type idx for the i-th user fn (in declaration order).
    let mut fn_type_indices: Vec<u32> = Vec::with_capacity(fn_defs.len());
    for fd in &fn_defs {
        let params = param_types(&fd.params, Some(&registry))?;
        let results = return_results(&fd.return_type, Some(&registry))?;
        types.ty().function(params, results);
        fn_type_indices.push(next_type_idx);
        next_type_idx += 1;
    }

    // 5) One fn type per registered builtin.
    let import_count = effect_registry.import_count();
    let mut next_builtin_fn_idx = import_count + 1 + (fn_defs.len() as u32);
    builtin_registry.assign_slots(&mut next_builtin_fn_idx, &mut next_type_idx);
    for name in builtin_registry.iter() {
        let p = name.params(&registry)?;
        let r = name.results(&registry)?;
        types.ty().function(p, r);
    }

    // 6) Map helper fn types (per-K hash + eq, per-(K,V) empty/set/get/len).
    let mut map_helpers = MapHelperRegistry::default();
    map_helpers.assign_slots(
        &registry.map_order,
        &registry,
        &mut next_builtin_fn_idx,
        &mut next_type_idx,
    )?;
    map_helpers.emit_helper_types(&mut types, &registry)?;

    // 7) List / Vector.fromList / String.split-join helpers — per-T
    //    instantiation list ops, plus singleton split/join when the
    //    surface code uses them.
    let needs_split_join = items_use_string_split_join(items);
    let mut list_helpers = super::lists::ListHelperRegistry::default();
    list_helpers.assign_slots(
        &registry.list_order,
        &registry.vector_order,
        &registry.tuple_order,
        needs_split_join,
        &registry,
        &mut next_builtin_fn_idx,
        &mut next_type_idx,
    )?;
    list_helpers.emit_helper_types(&mut types, &registry)?;

    // Per-(record/sum) `__eq_<TypeName>` helpers — slot allocation +
    // type emit. Bodies emitted after list helpers (they may call
    // `__wasmgc_string_eq` registered above).
    eq_helpers_registry.assign_slots(&mut next_builtin_fn_idx, &mut next_type_idx);
    eq_helpers_registry.emit_helper_types(&mut types);
    hash_helpers_registry.assign_slots(&mut next_builtin_fn_idx, &mut next_type_idx);
    hash_helpers_registry.emit_helper_types(&mut types);

    // 8a) `aver_http_handle` wrapper — `--handler X` synthesises a
    //     no-arg fn that reads request fields via the `Request.*`
    //     effects, builds an `HttpRequest`, calls the user's
    //     `handler`, then walks the response Map and dispatches per
    //     header before finalising via `Response.text`. Slot the type
    //     and fn idx now; the body lands at the end of the code
    //     section (after every helper) so the wrapper's fn idx is
    //     the highest in the module.
    let handler_wrapper: Option<HandlerWrapper> = if let Some(name) = handler_name {
        let user_idx = fn_defs
            .iter()
            .position(|fd| fd.name == name)
            .ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "--handler `{name}` doesn't match any fn in this module"
                ))
            })?;
        // wrapper is `() -> ()`; status/body land via Response.text.
        types.ty().function([], []);
        let wrapper_type = next_type_idx;
        next_type_idx += 1;
        // list_cons : (head: ref string, tail: ref list_String) -> ref list_String
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "handler wrapper requires String slot".into(),
            ))?;
        let list_idx = registry
            .list_type_idx("List<String>")
            .ok_or(WasmGcError::Validation(
                "handler wrapper requires List<String> slot".into(),
            ))?;
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(s_idx),
        });
        let l_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(list_idx),
        });
        types.ty().function([s_ref, l_ref], [l_ref]);
        let list_cons_type = next_type_idx;
        next_type_idx += 1;

        let wrapper_fn = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        let list_cons_fn = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(HandlerWrapper {
            user_handler_idx: user_idx,
            wrapper_type,
            wrapper_fn,
            list_cons_type,
            list_cons_fn,
        })
    } else {
        None
    };

    // 8) Host-bridge helpers + LM transport buffer — see
    //    `BridgeIndices` for the why. Emit only when the registry
    //    actually allocated a String slot.
    let bridge: Option<BridgeIndices> = if registry.string_array_type_idx.is_some() {
        let idx = emit_bridge_types(&mut types, &registry, &mut next_type_idx)?;
        let mut next_fn = || {
            let v = next_builtin_fn_idx;
            next_builtin_fn_idx += 1;
            v
        };
        Some(BridgeIndices {
            from_lm_type: idx.from_lm_type,
            to_lm_type: idx.to_lm_type,
            pages_type: idx.pages_type,
            grow_type: idx.grow_type,
            from_lm_fn: next_fn(),
            to_lm_fn: next_fn(),
            pages_fn: next_fn(),
            grow_fn: next_fn(),
        })
    } else {
        None
    };

    // 9) Wasm-owned value factories. JS host can't construct wasm-gc
    //    structs/variants directly, so any effect import that returns
    //    a structured ref needs per-type constructor helpers exported
    //    from the binary. Same per-instantiation pattern as
    //    `__rt_string_from_lm` / per-Map probes — host calls the
    //    factory, factory does `struct.new`, returns the ref. Emitted
    //    only when the corresponding effect is registered (DCE'd
    //    otherwise by `wasm-opt -Oz`).
    let factory_exports = allocate_factory_exports(
        &mut types,
        &mut next_type_idx,
        &mut next_builtin_fn_idx,
        &registry,
        &effect_registry,
    )?;

    // 10) Caller-fn name table exports. `__caller_fn_count() -> i32`
    //     and `__caller_fn_name(i32) -> ref null $string`. Host walks
    //     `0..count` once at instantiation, decodes each ref via the
    //     LM bridge, caches in a `Vec<String>`. Per effect call: `i32`
    //     idx flows through `params.last()` → vector index lookup,
    //     no LM round-trip on the hot path.
    //
    //     Allocated only when the program has the String slot (i.e.
    //     any fn def, since `needs_string` forces the slot whenever
    //     `has_fn_defs`). Programs without fns never emit caller_fn
    //     anywhere so the exports would be unused.
    let caller_fn_table_types: Option<(u32, u32)> =
        if let Some(string_type_idx) = registry.string_array_type_idx {
            // count: () -> i32
            types.ty().function([], [ValType::I32]);
            let count_type_idx = next_type_idx;
            next_type_idx += 1;
            // name: (i32) -> (ref null $string)
            let string_ref_ty = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(string_type_idx),
            });
            types.ty().function([ValType::I32], [string_ref_ty]);
            let name_type_idx = next_type_idx;
            // Last type allocation in this fn — `next_type_idx`
            // increment dropped to silence `unused_assignments`.
            Some((count_type_idx, name_type_idx))
        } else {
            None
        };

    module.section(&types);

    // ── Import section ─────────────────────────────────────────────
    if effect_registry.import_count() > 0 {
        let mut imports = ImportSection::new();
        for name in effect_registry.iter() {
            let (module_, field) = name.import_pair();
            let type_idx = effect_registry
                .lookup_wasm_type_idx(name)
                .expect("just-assigned effect type idx");
            imports.import(module_, field, EntityType::Function(type_idx));
        }
        module.section(&imports);
    }

    // ── Function section ───────────────────────────────────────────
    let mut funcs = FunctionSection::new();
    funcs.function(start_type_idx); // _start at wasm fn idx K
    for type_idx in &fn_type_indices {
        funcs.function(*type_idx);
    }
    for name in builtin_registry.iter() {
        let type_idx = builtin_registry
            .lookup_wasm_type_idx(name)
            .expect("just-assigned builtin type idx");
        funcs.function(type_idx);
    }
    map_helpers.emit_function_section(&mut funcs);
    list_helpers.emit_function_section(&mut funcs);
    // Eq helpers — one fn entry per registered `__eq_<TypeName>` slot.
    for (name, _kind) in eq_helpers_registry.iter() {
        let t_idx = eq_helpers_registry
            .lookup_type_idx(name)
            .expect("registered eq helper has type idx after assign_slots");
        funcs.function(t_idx);
    }
    // Hash helpers — same shape (one entry per registered slot).
    for (name, _kind) in hash_helpers_registry.iter() {
        let t_idx = hash_helpers_registry
            .lookup_type_idx(name)
            .expect("registered hash helper has type idx after assign_slots");
        funcs.function(t_idx);
    }
    if let Some(hw) = &handler_wrapper {
        funcs.function(hw.wrapper_type);
        funcs.function(hw.list_cons_type);
    }
    if let Some(b) = &bridge {
        funcs.function(b.from_lm_type);
        funcs.function(b.to_lm_type);
        funcs.function(b.pages_type);
        funcs.function(b.grow_type);
    }
    factory_exports.emit_function_entries(&mut funcs);
    // Caller-fn name table fns — fixed-shape entries (count + name),
    // their bodies land at the very end of the code section once
    // `caller_fn_collector` has all names. Idxs are recorded so
    // `module.section(&exports)` can wire them up without re-deriving
    // the position.
    let caller_fn_table_fns: Option<(u32, u32)> = caller_fn_table_types.map(|(c_ty, n_ty)| {
        let count_fn_idx = import_count + funcs.len();
        funcs.function(c_ty);
        let name_fn_idx = import_count + funcs.len();
        funcs.function(n_ty);
        (count_fn_idx, name_fn_idx)
    });
    module.section(&funcs);

    // ── Memory section (bridge LM only) ────────────────────────────
    // 1 page initial, 2048 max (128 MiB ceiling — matches Cloudflare
    // Workers' per-request memory limit). The bridge helpers grow
    // on demand: `__rt_string_to_lm` checks if it can fit the
    // outgoing array and calls `memory.grow` if not. JS host can
    // also grow upfront via `__rt_memory_grow` before writing into
    // LM with `TextEncoder.encodeInto`. Memory is not a guest heap
    // (engine GC owns that); it exists solely as a transport buffer
    // between JS host and the `(array i8)` carrier.
    if bridge.is_some() {
        let mut memories = wasm_encoder::MemorySection::new();
        memories.memory(wasm_encoder::MemoryType {
            minimum: 1,
            maximum: Some(2048),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memories);
    }

    // (caller_fn delivery moved from per-fn globals to an exported
    // name table; segment append + `__caller_fn_*` exports are
    // wired in the post-emit phase further down. Globals + their
    // start-fn init are gone.)

    // Build the fn-name → wasm-fn-idx map. With K imports:
    //   imports at idx 0..K
    //   _start at K
    //   user fn i at K+1+i
    //   builtin at K+1+N+m (assigned by builtin_registry already)
    let start_wasm_idx = import_count;
    let mut by_name: HashMap<String, FnEntry> = HashMap::new();
    for (i, fd) in fn_defs.iter().enumerate() {
        by_name.insert(
            fd.name.clone(),
            FnEntry {
                wasm_idx: import_count + 1 + (i as u32),
                return_type: fd.return_type.clone(),
            },
        );
    }
    let mut builtin_idx_lookup: HashMap<String, u32> = HashMap::new();
    for name in builtin_registry.iter() {
        let idx = builtin_registry
            .lookup_wasm_fn_idx(name)
            .expect("registered builtin has wasm fn idx");
        builtin_idx_lookup.insert(name.canonical().to_string(), idx);
    }
    let mut effect_idx_lookup: HashMap<String, u32> = HashMap::new();
    for name in effect_registry.iter() {
        let idx = effect_registry
            .lookup_wasm_fn_idx(name)
            .expect("registered effect has wasm fn idx");
        effect_idx_lookup.insert(name.canonical().to_string(), idx);
    }
    let mut map_helpers_lookup: HashMap<String, super::maps::MapKVHelpers> = HashMap::new();
    for canonical in &registry.map_order {
        if let Some(h) = map_helpers.kv_helpers(canonical) {
            map_helpers_lookup.insert(canonical.clone(), h);
        }
    }
    let mut list_ops_lookup: HashMap<String, super::lists::ListOps> = HashMap::new();
    for canonical in &registry.list_order {
        if let Some(o) = list_helpers.list_ops_for(canonical) {
            list_ops_lookup.insert(canonical.clone(), o);
        }
    }
    let mut vfl_ops_lookup: HashMap<String, super::lists::VectorFromListOps> = HashMap::new();
    for canonical in &registry.list_order {
        if let Some(o) = list_helpers.vfl_ops_for(canonical) {
            vfl_ops_lookup.insert(canonical.clone(), o);
        }
    }
    let mut zip_ops_lookup: HashMap<String, u32> = HashMap::new();
    for tup_canonical in &registry.tuple_order {
        if let Some(idx) = list_helpers.zip_op_for(tup_canonical) {
            zip_ops_lookup.insert(tup_canonical.clone(), idx);
        }
    }
    let string_split_ops = list_helpers.string_split_ops();
    let mut eq_helpers_lookup: HashMap<String, u32> = HashMap::new();
    for (name, _kind) in eq_helpers_registry.iter() {
        if let Some(fn_idx) = eq_helpers_registry.lookup_fn_idx(name) {
            eq_helpers_lookup.insert(name.to_string(), fn_idx);
        }
    }
    let fn_map = FnMap {
        by_name,
        builtins: builtin_idx_lookup,
        effects: effect_idx_lookup.clone(),
        map_helpers: map_helpers_lookup,
        list_ops: list_ops_lookup,
        vfl_ops: vfl_ops_lookup,
        zip_ops: zip_ops_lookup,
        string_split_ops,
        eq_helpers: eq_helpers_lookup,
    };

    // ── Export section ─────────────────────────────────────────────
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, start_wasm_idx);
    for (i, fd) in fn_defs.iter().enumerate() {
        let wasm_idx = import_count + 1 + (i as u32);
        exports.export(&fd.name, ExportKind::Func, wasm_idx);
    }
    if let Some(b) = &bridge {
        exports.export("__rt_string_from_lm", ExportKind::Func, b.from_lm_fn);
        exports.export("__rt_string_to_lm", ExportKind::Func, b.to_lm_fn);
        exports.export("__rt_memory_pages", ExportKind::Func, b.pages_fn);
        exports.export("__rt_memory_grow", ExportKind::Func, b.grow_fn);
        exports.export("memory", ExportKind::Memory, 0);
    }
    factory_exports.emit_exports(&mut exports);
    if let Some(hw) = &handler_wrapper {
        exports.export("aver_http_handle", ExportKind::Func, hw.wrapper_fn);
        exports.export("__rt_list_string_cons", ExportKind::Func, hw.list_cons_fn);
        // Map<String,List<String>> bridge: the JS host needs to build
        // a request-headers map to satisfy `request_headers_load`.
        // Re-export the per-instance Map helper slots under stable
        // bridge names.
        if let Some(map_h) = map_helpers.kv_helpers("Map<String,List<String>>") {
            exports.export(
                "__rt_map_string_list_string_empty",
                ExportKind::Func,
                map_h.empty,
            );
            exports.export(
                "__rt_map_string_list_string_set",
                ExportKind::Func,
                map_h.set,
            );
        }
    }
    if let Some((count_fn_idx, name_fn_idx)) = caller_fn_table_fns {
        exports.export("__caller_fn_count", ExportKind::Func, count_fn_idx);
        exports.export("__caller_fn_name", ExportKind::Func, name_fn_idx);
    }
    module.section(&exports);

    // (No StartSection — 0.16.2's caller_fn globals init is gone;
    // host reads the caller_fn name table via `__caller_fn_count`
    // + `__caller_fn_name(i)` exports at instantiation instead.)

    // Pre-pass over user fn bodies — populates `caller_fn_collector`
    // with every fn name that emits caller_fn at a call site. Needed
    // before data count + data section emit because the count of
    // passive segments is `string_literals + collector.names`, and
    // data count section must precede the code section. Real body
    // emit later in the code section calls `register` again with the
    // same names; the collector is idempotent so the idx assignment
    // matches what the call sites observed during this probe.
    for (i, fd) in fn_defs.iter().enumerate() {
        let self_wasm_idx = import_count + 1 + (i as u32);
        let mut probe = Function::new([]);
        let _ = emit_fn_body(
            &mut probe,
            fd,
            &fn_map,
            self_wasm_idx,
            &registry,
            &effect_idx_lookup,
            &caller_fn_collector,
        )?;
    }
    let caller_fn_segment_count = caller_fn_collector.borrow().names.len() as u32;

    // ── Data count section (must precede code when using passive
    //     segments via array.new_data / data.drop).
    let total_segment_count = registry.string_literals.len() as u32 + caller_fn_segment_count;
    if total_segment_count > 0 {
        let count = DataCountSection {
            count: total_segment_count,
        };
        module.section(&count);
    }

    // ── Code section ───────────────────────────────────────────────
    let mut codes = CodeSection::new();

    // _start: call main if present, drop its return value. Caller_fn
    // globals are NOT init here — the wasm-level `(start
    // __init_globals)` section handles that on instantiation, before
    // any export gets called.
    let mut start = Function::new([]);
    if let Some(idx) = main_idx {
        let main_idx_wasm = import_count + 1 + (idx as u32);
        let main_returns_value = !fn_defs[idx].return_type.trim().eq("Unit");
        start.instruction(&Instruction::Call(main_idx_wasm));
        if main_returns_value {
            start.instruction(&Instruction::Drop);
        }
    }
    start.instruction(&Instruction::End);
    codes.function(&start);

    for (i, fd) in fn_defs.iter().enumerate() {
        let self_wasm_idx = import_count + 1 + (i as u32);
        // Dry run: discover extra locals by emitting into a throwaway
        // fn. Cheaper than threading a separate pre-pass.
        let mut probe = Function::new([]);
        let extra_locals_dry = emit_fn_body(
            &mut probe,
            fd,
            &fn_map,
            self_wasm_idx,
            &registry,
            &effect_idx_lookup,
            &caller_fn_collector,
        )?;

        let local_groups: Vec<(u32, ValType)> = extra_locals_dry.iter().map(|v| (1, *v)).collect();
        let mut func = Function::new(local_groups);
        let _ = emit_fn_body(
            &mut func,
            fd,
            &fn_map,
            self_wasm_idx,
            &registry,
            &effect_idx_lookup,
            &caller_fn_collector,
        )?;
        codes.function(&func);
    }

    // Builtin helper bodies — emitted after user fns so their own
    // wasm fn indices come last. Bodies are stubs today (Unreachable);
    // real impls land in `builtins/` per phase 3c roadmap.
    builtin_registry.emit_helper_bodies(&mut codes, &registry)?;

    // Map helper bodies (hash, eq, empty, set, get, len per
    // instantiation) — emitted last so their wasm fn indices line up
    // with what `MapHelperRegistry::assign_slots` recorded.
    // Snapshot list / vector eq+hash fn idxes so map record-key
    // helpers can dispatch `List<T>` / `Vector<T>` field types
    // without cross-module lookups.
    let mut compound_eq_hash_lookup: HashMap<String, (u32, u32)> = HashMap::new();
    for canonical in &registry.list_order {
        if let Some(o) = list_helpers.list_ops_for(canonical)
            && let (Some(eq_fn), Some(hash_fn)) = (o.eq, o.hash)
        {
            compound_eq_hash_lookup.insert(canonical.clone(), (eq_fn, hash_fn));
        }
    }
    for canonical in &registry.list_order {
        // vfl_ops keyed by list canonical, but the `Vector<T>`
        // canonical is the right pseudo-K name for record-field
        // dispatch — translate.
        if let Some(elem) = TypeRegistry::list_element_type(canonical)
            && let Some(o) = list_helpers.vfl_ops_for(canonical)
            && let (Some(eq_fn), Some(hash_fn)) = (o.eq, o.hash)
        {
            compound_eq_hash_lookup.insert(format!("Vector<{}>", elem.trim()), (eq_fn, hash_fn));
        }
    }
    // Carrier eq+hash lookup — Option/Result/Tuple instantiations
    // get their helpers from eq_helpers / hash_helpers; map keys
    // proxy through these. Build the pair map by zipping the two
    // registries' fn idxs by canonical.
    let mut carrier_eq_hash_lookup: HashMap<String, (u32, u32)> = HashMap::new();
    for (name, kind) in eq_helpers_registry.iter() {
        use super::body::eq_helpers::EqKind as EK;
        if matches!(kind, EK::OptionEq | EK::ResultEq | EK::TupleEq)
            && let Some(eq_fn) = eq_helpers_registry.lookup_fn_idx(name)
            && let Some(hash_fn) = hash_helpers_registry.lookup_fn_idx(name)
        {
            carrier_eq_hash_lookup.insert(name.to_string(), (eq_fn, hash_fn));
        }
    }
    map_helpers.emit_helper_bodies(
        &mut codes,
        &registry,
        &compound_eq_hash_lookup,
        &carrier_eq_hash_lookup,
    )?;

    // List / Vector.fromList / String.split-join helper bodies.
    // Snapshot eq-helper fn idxs so list/vec eq+hash bodies can
    // dispatch nominal-element `==`/hash through `Call(__eq_<X>)`.
    let string_eq_fn_idx = builtin_registry.lookup_wasm_fn_idx(BuiltinName::StringEq);
    let eq_helper_fn_idx_map: HashMap<String, u32> = eq_helpers_registry
        .iter()
        .filter_map(|(n, _k)| {
            eq_helpers_registry
                .lookup_fn_idx(n)
                .map(|i| (n.to_string(), i))
        })
        .collect();
    let hash_helper_fn_idx_map: HashMap<String, u32> = hash_helpers_registry
        .iter()
        .filter_map(|(n, _k)| {
            hash_helpers_registry
                .lookup_fn_idx(n)
                .map(|i| (n.to_string(), i))
        })
        .collect();
    list_helpers.emit_helper_bodies(
        &mut codes,
        &registry,
        string_eq_fn_idx,
        &eq_helper_fn_idx_map,
        &hash_helper_fn_idx_map,
    )?;

    // Per-(record/sum) `__eq_<TypeName>` helper bodies — emit after
    // list helpers so any String fields can call `__wasmgc_string_eq`
    // by the index recorded above.
    eq_helpers_registry.emit_helper_bodies(&mut codes, &registry, string_eq_fn_idx)?;
    // `__hash_<X>` helper bodies — emitted right after eq helpers so
    // every nominal/carrier hash dispatch finds its target fn_idx.
    hash_helpers_registry.emit_helper_bodies(&mut codes, &registry, string_eq_fn_idx)?;

    if let Some(hw) = &handler_wrapper {
        let user_handler_wasm_idx = import_count + 1 + (hw.user_handler_idx as u32);
        codes.function(&emit_handler_wrapper(
            &registry,
            &fn_map,
            user_handler_wasm_idx,
        )?);
        codes.function(&emit_list_string_cons(&registry)?);
        let _ = hw.list_cons_type; // type idx already consumed by emit_function_section
    }

    if bridge.is_some() {
        emit_bridge_bodies(&mut codes, &registry)?;
    }

    factory_exports.emit_bodies(&mut codes, &registry)?;

    // `__caller_fn_count` + `__caller_fn_name` bodies. Emitted after
    // every helper so their fn idxs land last in the code section,
    // matching the function section allocation order. The collector
    // is fully populated at this point — every user-fn body ran
    // through the pre-pass and the real-emit pass.
    if let Some((_count_fn_idx, _name_fn_idx)) = caller_fn_table_fns {
        let names = caller_fn_collector.borrow();
        let string_idx = registry
            .string_array_type_idx
            .expect("caller_fn name table requires the $string slot");
        // Caller-fn name segments occupy the data section slot range
        // [string_literals.len()..string_literals.len()+names.len()];
        // `array.new_data` in `__caller_fn_name(i)` reads from those
        // idxs.
        let segment_base = registry.string_literals.len() as u32;

        // __caller_fn_count: pure constant.
        let mut count_fn = Function::new([]);
        count_fn.instruction(&Instruction::I32Const(names.names.len() as i32));
        count_fn.instruction(&Instruction::End);
        codes.function(&count_fn);

        // __caller_fn_name(idx) -> ref null $string. Switch on idx
        // via `br_table`; each arm materialises the matching String
        // ref via `array.new_data`. A trailing default arm returns
        // ref.null for out-of-range idxs (host shouldn't pass them,
        // but the wasm validator wants a fallthrough).
        let string_ref_ty = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        let mut name_fn = Function::new([]);
        let block_ty = wasm_encoder::BlockType::Result(string_ref_ty);
        name_fn.instruction(&Instruction::Block(block_ty));
        for (i, fn_name) in names.names.iter().enumerate() {
            let bytes = fn_name.as_bytes();
            // Inner block: if idx == i, this arm emits the ref and
            // breaks out of the outer block. Otherwise falls through
            // to the next arm.
            name_fn.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
            // if local 0 != i { br 0 } — skip to next arm.
            name_fn.instruction(&Instruction::LocalGet(0));
            name_fn.instruction(&Instruction::I32Const(i as i32));
            name_fn.instruction(&Instruction::I32Ne);
            name_fn.instruction(&Instruction::BrIf(0));
            // Match: emit ref + break to outer.
            name_fn.instruction(&Instruction::I32Const(0));
            name_fn.instruction(&Instruction::I32Const(bytes.len() as i32));
            name_fn.instruction(&Instruction::ArrayNewData {
                array_type_index: string_idx,
                array_data_index: segment_base + i as u32,
            });
            name_fn.instruction(&Instruction::Br(1));
            name_fn.instruction(&Instruction::End);
        }
        // Default arm — out-of-range idx returns ref.null.
        name_fn.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
            string_idx,
        )));
        name_fn.instruction(&Instruction::End);
        name_fn.instruction(&Instruction::End);
        codes.function(&name_fn);
    }

    module.section(&codes);

    // ── Data section ───────────────────────────────────────────────
    // Passive segments holding String literal byte sequences. Emitted
    // last; `array.new_data $string $segment_idx` reads from these.
    // Order: pre-walked program literals first, caller_fn names
    // second. `__caller_fn_name`'s body uses
    // `segment_base = registry.string_literals.len()` so its arms
    // hit the right slots regardless of how many literals the
    // program has.
    if total_segment_count > 0 {
        let mut data = DataSection::new();
        for bytes in &registry.string_literals {
            data.passive(bytes.iter().copied());
        }
        let names = caller_fn_collector.borrow();
        for fn_name in &names.names {
            data.passive(fn_name.as_bytes().iter().copied());
        }
        module.section(&data);
    }

    let bytes = module.finish();
    if let Err(e) = validate(&bytes) {
        // Dump invalid bytes for `wasm-tools print` inspection.
        let _ = std::fs::write("/tmp/aver_wasm_gc_invalid.wasm", &bytes);
        return Err(e);
    }
    Ok(bytes)
}

fn emit_user_types(
    types: &mut TypeSection,
    items: &[TopLevel],
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    // ALL user types — records, variants, string array, vectors,
    // results, lists, options, maps, builtin records — go into a
    // single explicit rec group. Inside a rec group wasm-gc allows
    // forward references between members, which lifts the strict
    // bottom-up ordering constraint that otherwise made
    // `Vector<List<Int>>` / `List<Map<K, V>>` / any cross-collection
    // nesting impossible to express. Type indices follow registry
    // insertion order exactly the way they did before the rec group;
    // the difference is that members can refer to peers at higher
    // indices without crossing a group boundary.
    use wasm_encoder::{ArrayType, CompositeInnerType, CompositeType, StructType, SubType};
    // Each entry pairs a registry-recorded type idx with the subtype
    // shape. Sorting by idx at the end guarantees the rec-group emit
    // position matches what `vector_type_idx` / `list_type_idx` /
    // `option_type_idx` / `map_slots` / `record_type_idx` recorded —
    // critical because eager registrations (`Option<Vector<T>>`,
    // `List<K>` for Map keys, etc.) interleave categories so the
    // per-collection iteration order no longer matches insertion
    // order.
    let mut entries: Vec<(u32, SubType)> = Vec::new();
    let mk_struct = |fields: Vec<wasm_encoder::FieldType>| SubType {
        is_final: true,
        supertype_idx: None,
        composite_type: CompositeType {
            inner: CompositeInnerType::Struct(StructType {
                fields: fields.into_boxed_slice(),
            }),
            shared: false,
        },
    };
    let mk_array = |elem: wasm_encoder::FieldType| SubType {
        is_final: true,
        supertype_idx: None,
        composite_type: CompositeType {
            inner: CompositeInnerType::Array(ArrayType(elem)),
            shared: false,
        },
    };
    // Records / variants — registered first in `TypeRegistry::build`,
    // idx assigned in source order. Look up the recorded idx for each.
    for item in items {
        match item {
            TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) => {
                let st = record_struct_type(fields, registry)?;
                let idx = registry
                    .record_type_idx(name)
                    .ok_or(WasmGcError::Validation(format!(
                        "record `{name}` not registered"
                    )))?;
                entries.push((idx, mk_struct(st.fields.to_vec())));
            }
            TopLevel::TypeDef(TypeDef::Sum {
                name: parent,
                variants,
                ..
            }) => {
                for v in variants {
                    let mut fields = Vec::new();
                    for ty in &v.fields {
                        let val_ty = super::types::aver_to_wasm(ty, Some(registry))?.ok_or(
                            WasmGcError::Validation(format!(
                                "variant `{}` field of type {ty} has no wasm representation",
                                v.name
                            )),
                        )?;
                        fields.push(wasm_encoder::FieldType {
                            element_type: wasm_encoder::StorageType::Val(val_ty),
                            mutable: false,
                        });
                    }
                    // Look up by (parent, variant) so two sumtypes
                    // sharing a bare variant name (e.g. payment_ops's
                    // `Query.ProviderSummary` and `QueryOutput.
                    // ProviderSummary`) each emit their own struct
                    // type idx with their own field shape — instead of
                    // both nadpisując the same entry under the `bare`
                    // key.
                    let info =
                        registry
                            .variant_in(parent, &v.name)
                            .ok_or(WasmGcError::Validation(format!(
                                "variant `{parent}.{}` not registered",
                                v.name
                            )))?;
                    entries.push((info.type_idx, mk_struct(fields)));
                }
            }
            _ => {}
        }
    }

    // String slot.
    if let Some(idx) = registry.string_array_type_idx {
        entries.push((
            idx,
            mk_array(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::I8,
                mutable: true,
            }),
        ));
    }

    // Vector<T> instantiations.
    for canonical in &registry.vector_order {
        let element =
            TypeRegistry::vector_element_type(canonical).ok_or(WasmGcError::Validation(
                format!("registered vector `{canonical}` has no parsable element type"),
            ))?;
        let elem_val =
            super::types::aver_to_wasm(element, Some(registry))?.ok_or(WasmGcError::Validation(
                format!("Vector element type `{element}` has no wasm representation"),
            ))?;
        let idx = registry
            .vector_type_idx(canonical)
            .ok_or(WasmGcError::Validation(format!(
                "vector `{canonical}` not registered"
            )))?;
        entries.push((
            idx,
            mk_array(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(elem_val),
                mutable: true,
            }),
        ));
    }

    // `Result<T, E>` — `(struct (mut i32 tag) (mut T ok) (mut E err))`.
    // Unit on either side has no wasm value; we use a dummy `i32` slot
    // so the struct shape stays uniform. The slot is never read for
    // Unit-typed sides — pattern matching only inspects the tag and
    // unwraps the *other* side.
    for canonical in &registry.result_order {
        let (t_aver, e_aver) =
            TypeRegistry::result_te(canonical).ok_or(WasmGcError::Validation(format!(
                "registered result `{canonical}` has no parsable T, E"
            )))?;
        let t_val = super::types::aver_to_wasm(t_aver, Some(registry))?.unwrap_or(ValType::I32);
        let e_val = super::types::aver_to_wasm(e_aver, Some(registry))?.unwrap_or(ValType::I32);
        let idx = registry
            .result_type_idx(canonical)
            .ok_or(WasmGcError::Validation(format!(
                "result `{canonical}` not registered"
            )))?;
        entries.push((
            idx,
            mk_struct(vec![
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(ValType::I32),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(t_val),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(e_val),
                    mutable: true,
                },
            ]),
        ));
    }

    // `List<T>` — recursive Cons cell.
    for canonical in &registry.list_order {
        let element = TypeRegistry::list_element_type(canonical).ok_or(WasmGcError::Validation(
            format!("registered list `{canonical}` has no parsable element type"),
        ))?;
        let elem_val =
            super::types::aver_to_wasm(element, Some(registry))?.ok_or(WasmGcError::Validation(
                format!("List element type `{element}` has no wasm representation"),
            ))?;
        let own_idx = registry
            .list_type_idx(canonical)
            .expect("just-registered list slot");
        let tail_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(own_idx),
        });
        entries.push((
            own_idx,
            mk_struct(vec![
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(elem_val),
                    mutable: false,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(tail_ref),
                    mutable: false,
                },
            ]),
        ));
    }

    // Option<T> — `(struct (mut i32 tag) (mut T value))`.
    for canonical in &registry.option_order {
        let element =
            TypeRegistry::option_element_type(canonical).ok_or(WasmGcError::Validation(
                format!("registered option `{canonical}` has no parsable element type"),
            ))?;
        let elem_val =
            super::types::aver_to_wasm(element, Some(registry))?.ok_or(WasmGcError::Validation(
                format!("Option element type `{element}` has no wasm representation"),
            ))?;
        let idx = registry
            .option_type_idx(canonical)
            .ok_or(WasmGcError::Validation(format!(
                "option `{canonical}` not registered"
            )))?;
        entries.push((
            idx,
            mk_struct(vec![
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(ValType::I32),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(elem_val),
                    mutable: true,
                },
            ]),
        ));
    }

    // `Map<K, V>` — three wasm types per registered instantiation
    // (keys array, values array, map struct).
    for canonical in &registry.map_order {
        let (k_aver, v_aver) = super::types::parse_map_kv(canonical).ok_or(
            WasmGcError::Validation(format!("registered map `{canonical}` has no parsable K, V")),
        )?;
        let v_val =
            super::types::aver_to_wasm(v_aver, Some(registry))?.ok_or(WasmGcError::Validation(
                format!("Map value type `{v_aver}` has no wasm representation"),
            ))?;
        // Keys array element: for primitive K, a `(ref null
        // $primitive_key_box_K)` so the empty-slot marker stays
        // uniform; for ref K (String / record), the K's own ref.
        let key_storage_val = if let Some(box_idx) = registry.primitive_key_box_idx(k_aver) {
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(box_idx),
            })
        } else {
            super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(WasmGcError::Validation(
                format!("Map key type `{k_aver}` has no wasm representation"),
            ))?
        };
        let slots = registry
            .map_slots(canonical)
            .expect("just-registered map slots");
        entries.push((
            slots.keys_array,
            mk_array(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(key_storage_val),
                mutable: true,
            }),
        ));
        entries.push((
            slots.values_array,
            mk_array(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(v_val),
                mutable: true,
            }),
        ));
        let keys_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(slots.keys_array),
        });
        let values_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(slots.values_array),
        });
        entries.push((
            slots.map,
            mk_struct(vec![
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(ValType::I32),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(ValType::I32),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(keys_ref),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(values_ref),
                    mutable: true,
                },
            ]),
        ));
    }

    // Primitive map-key boxes — `(struct (mut K_val))` per
    // primitive K used as a Map<K, *>. Boxing primitive keys keeps
    // the open-addressing layout's `keys[i] == null` empty marker
    // uniform across all K kinds (raw i64/f64/i32 has no null).
    for k_aver in &registry.primitive_key_box_order {
        let k_val =
            super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(WasmGcError::Validation(
                format!("primitive key box: K=`{k_aver}` has no wasm representation"),
            ))?;
        let idx = registry
            .primitive_key_box_idx(k_aver)
            .ok_or(WasmGcError::Validation(format!(
                "primitive key box for `{k_aver}` not registered"
            )))?;
        entries.push((
            idx,
            mk_struct(vec![wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(k_val),
                mutable: true,
            }]),
        ));
    }

    // `Tuple<A, B, ..., N>` — `(struct (mut A) (mut B) ... (mut N))`.
    // Variadic arity: 2-tuples used by Map.entries / Map.fromList /
    // List.zip; 3+ tuples used by user code (`scoreTriple`,
    // `scoreQuad`) and `(...)!` independent products.
    for canonical in &registry.tuple_order {
        let elems = TypeRegistry::tuple_elements(canonical).ok_or(WasmGcError::Validation(
            format!("registered tuple `{canonical}` has no parsable elements"),
        ))?;
        let mut fields: Vec<wasm_encoder::FieldType> = Vec::with_capacity(elems.len());
        for elem_aver in &elems {
            // Unit tuple element → i32 placeholder slot (same logic as
            // Result<Unit, E>): keeps the struct shape uniform; the
            // slot is never read because Unit has no observable value.
            let elem_val =
                super::types::aver_to_wasm(elem_aver, Some(registry))?.unwrap_or(ValType::I32);
            fields.push(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(elem_val),
                mutable: true,
            });
        }
        let idx = registry
            .tuple_type_idx(canonical)
            .ok_or(WasmGcError::Validation(format!(
                "tuple `{canonical}` not registered"
            )))?;
        entries.push((idx, mk_struct(fields)));
    }

    // Built-in records (HttpRequest / HttpResponse / Tcp.Connection /
    // Terminal.Size) — registered with their own deferred idx.
    for record in crate::codegen::builtin_records::BUILTIN_RECORDS {
        if !registry.records.contains_key(record.aver_name) {
            continue;
        }
        let fields = registry
            .record_fields
            .get(record.aver_name)
            .expect("builtin record registered without fields");
        let st = super::types::record_struct_type(fields, registry)?;
        let idx = registry
            .record_type_idx(record.aver_name)
            .ok_or(WasmGcError::Validation(format!(
                "builtin record `{}` not registered",
                record.aver_name
            )))?;
        entries.push((idx, mk_struct(st.fields.to_vec())));
    }

    // Sort entries by registry-recorded type idx so the rec-group
    // emit position matches every recorded `*_type_idx` lookup. The
    // sort is stable; equal idx values would mean a registry bug.
    entries.sort_by_key(|(idx, _)| *idx);
    let subtypes: Vec<SubType> = entries.into_iter().map(|(_, t)| t).collect();

    // The rec group counts as ONE type-section entry (single 0x4e
    // prefix + N subtypes), so route through `ty()` which advances
    // `num_added` by 1 for the whole group.
    types.ty().rec(subtypes);
    Ok(())
}

/// Walk a fn body looking for dotted builtin calls and register each
/// unique one in `registry`. Discovery happens once per module before
/// any wasm bytes get emitted, so slot allocation can run with the
/// full set known.
fn discover_builtins_in_fn(
    fd: &FnDef,
    builtins: &mut BuiltinRegistry,
    effects: &mut EffectRegistry,
    eq_helpers: &mut EqHelperRegistry,
    type_registry: &TypeRegistry,
) {
    let crate::ast::FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        discover_builtins_in_stmt(stmt, builtins, effects, eq_helpers, type_registry);
    }
}

fn discover_builtins_in_stmt(
    stmt: &Stmt,
    builtins: &mut BuiltinRegistry,
    effects: &mut EffectRegistry,
    eq_helpers: &mut EqHelperRegistry,
    type_registry: &TypeRegistry,
) {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => {
            discover_builtins_in_expr(&e.node, builtins, effects, eq_helpers, type_registry)
        }
    }
}

/// Recursively walks `t` and registers every nominal record/sum it
/// reaches in `eq_helpers`. Needed for `==` on collection types
/// whose element/key/value type is nominal — `List<Tree>`,
/// `Map<Color, Tree>`, `Option<Box>`, etc. Without this, the
/// helper-body emit (`emit_list_eq`, `emit_record_eq_inline`,
/// `emit_eq_record`) would dispatch by `Call(__eq_<Tree>)` against
/// an unregistered slot.
fn register_nominal_in_type(
    t: &AverType,
    eq_helpers: &mut EqHelperRegistry,
    type_registry: &super::types::TypeRegistry,
) {
    let canonical: String = t.display().chars().filter(|c| !c.is_whitespace()).collect();
    match t {
        AverType::Named(name) => {
            if type_registry.record_fields.contains_key(name) {
                eq_helpers.register_transitive(name, EqKind::Record, type_registry);
            } else if type_registry
                .variants
                .values()
                .flat_map(|v| v.iter())
                .any(|v| &v.parent == name)
            {
                eq_helpers.register_transitive(name, EqKind::Sum, type_registry);
            }
        }
        AverType::Option(inner) => {
            eq_helpers.register_transitive(&canonical, EqKind::OptionEq, type_registry);
            register_nominal_in_type(inner, eq_helpers, type_registry);
        }
        AverType::Result(ok, err) => {
            eq_helpers.register_transitive(&canonical, EqKind::ResultEq, type_registry);
            register_nominal_in_type(ok, eq_helpers, type_registry);
            register_nominal_in_type(err, eq_helpers, type_registry);
        }
        AverType::Tuple(items) => {
            eq_helpers.register_transitive(&canonical, EqKind::TupleEq, type_registry);
            for item in items {
                register_nominal_in_type(item, eq_helpers, type_registry);
            }
        }
        AverType::List(inner) | AverType::Vector(inner) => {
            register_nominal_in_type(inner, eq_helpers, type_registry);
        }
        AverType::Map(k, v) => {
            register_nominal_in_type(k, eq_helpers, type_registry);
            register_nominal_in_type(v, eq_helpers, type_registry);
        }
        _ => {}
    }
}

fn discover_builtins_in_expr(
    expr: &Expr,
    builtins: &mut BuiltinRegistry,
    effects: &mut EffectRegistry,
    eq_helpers: &mut EqHelperRegistry,
    type_registry: &TypeRegistry,
) {
    use crate::ast::StrPart;
    match expr {
        Expr::FnCall(callee, args) => {
            if let Expr::Attr(_parent, member) = &callee.node
                && let Some(parent_name) = expr_to_dotted_head(&callee.node)
            {
                let dotted = format!("{parent_name}.{member}");
                if let Some(name) = BuiltinName::from_dotted(&dotted) {
                    builtins.register(name);
                }
                if let Some(name) = EffectName::from_dotted(&dotted) {
                    effects.register(name);
                }
                // `Args.get()` (no args, returns List<String>) lowers
                // inline as `args_len + loop args_get(i) cons` — no
                // single host import. Force-register both effects here
                // so `emit_args_get_inline` can look them up by name.
                if dotted == "Args.get" && args.is_empty() {
                    effects.register(EffectName::ArgsLen);
                    effects.register(EffectName::ArgsGet);
                }
            }
            discover_builtins_in_expr(&callee.node, builtins, effects, eq_helpers, type_registry);
            for arg in args {
                discover_builtins_in_expr(&arg.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::BinOp(op, l, r) => {
            // String `+` lowers to `__wasmgc_concat_n`; String `==`/`!=`
            // lower to `__wasmgc_string_eq`. Both helpers must be
            // registered up front so emit can `Call` them by index.
            // Read the operand type off the typed AST — Step 3 stamps
            // every node's `ty`.
            if let Some(t) = l.ty()
                && t.display().trim() == "String"
            {
                use crate::ast::BinOp as Op;
                match op {
                    Op::Add => builtins.register(BuiltinName::StringConcatN),
                    Op::Eq | Op::Neq => builtins.register(BuiltinName::StringEq),
                    Op::Lt | Op::Gt | Op::Lte | Op::Gte => {
                        builtins.register(BuiltinName::StringCompare);
                    }
                    _ => {}
                }
            }
            // Sum/record `==`/`!=` need a per-type `__eq_<TypeName>`
            // helper — register on discovery so the slot is allocated
            // before emit runs the BinOp dispatch.
            use crate::ast::BinOp as Op;
            if matches!(op, Op::Eq | Op::Neq)
                && let Some(t) = l.ty()
                && let AverType::Named(name) = t
            {
                if type_registry.record_fields.contains_key(name) {
                    eq_helpers.register_transitive(name, EqKind::Record, type_registry);
                } else if type_registry
                    .variants
                    .values()
                    .flat_map(|v| v.iter())
                    .any(|v| &v.parent == name)
                {
                    eq_helpers.register_transitive(name, EqKind::Sum, type_registry);
                }
            }
            // List / Vector / Map / Option / Result / Tuple `==` —
            // dispatch reaches the per-element/key __eq_<X> through
            // the helper bodies, so any nominal element type also
            // needs an __eq slot. Walk the operand type recursively
            // and register every nominal we hit.
            if matches!(op, Op::Eq | Op::Neq)
                && let Some(t) = l.ty()
            {
                register_nominal_in_type(t, eq_helpers, type_registry);
            }
            discover_builtins_in_expr(&l.node, builtins, effects, eq_helpers, type_registry);
            discover_builtins_in_expr(&r.node, builtins, effects, eq_helpers, type_registry);
        }
        Expr::Match { subject, arms } => {
            discover_builtins_in_expr(&subject.node, builtins, effects, eq_helpers, type_registry);
            // String-subject match (`match path { "/" -> ... }`)
            // needs `StringEq` to compare each non-default arm's
            // literal against the subject. Register it eagerly when
            // any arm is `Pattern::Literal(Str(_))`.
            if arms.iter().any(|a| {
                matches!(
                    &a.pattern,
                    crate::ast::Pattern::Literal(crate::ast::Literal::Str(_))
                )
            }) {
                builtins.register(BuiltinName::StringEq);
            }
            for arm in arms {
                discover_builtins_in_expr(
                    &arm.body.node,
                    builtins,
                    effects,
                    eq_helpers,
                    type_registry,
                );
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                discover_builtins_in_expr(&arg.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::Attr(obj, _) => {
            discover_builtins_in_expr(&obj.node, builtins, effects, eq_helpers, type_registry)
        }
        Expr::ErrorProp(inner) => {
            discover_builtins_in_expr(&inner.node, builtins, effects, eq_helpers, type_registry)
        }
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                discover_builtins_in_expr(&p.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                discover_builtins_in_expr(&e.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            discover_builtins_in_expr(&base.node, builtins, effects, eq_helpers, type_registry);
            for (_, e) in updates {
                discover_builtins_in_expr(&e.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        // `InterpolatedStr` lowers to `array.new_fixed` + the variadic
        // concat helper. Register it here so the helper's wasm fn
        // index is allocated by the time emission runs. Each Parsed
        // part may also need `Int.toString` (if its type is Int) —
        // we conservatively register that too; unused registrations
        // are stripped by `wasm-opt -Oz`.
        Expr::InterpolatedStr(parts) => {
            // Variadic concat is mandatory; the per-type stringifiers
            // are registered conservatively whenever interpolation
            // exists in the program — unused registrations get DCE'd
            // by `wasm-opt -Oz`. Cheaper than a per-part type-driven
            // walk.
            builtins.register(BuiltinName::StringConcatN);
            builtins.register(BuiltinName::IntToString);
            builtins.register(BuiltinName::FloatToString);
            builtins.register(BuiltinName::StringFromBool);
            for p in parts {
                if let StrPart::Parsed(inner) = p {
                    discover_builtins_in_expr(
                        &inner.node,
                        builtins,
                        effects,
                        eq_helpers,
                        type_registry,
                    );
                }
            }
        }
        Expr::List(items) => {
            for item in items {
                discover_builtins_in_expr(&item.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::Tuple(items) => {
            for item in items {
                discover_builtins_in_expr(&item.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::IndependentProduct(items, _) => {
            // `?!` and `!` lower as sequential evaluation in wasm-gc,
            // but the recorder still needs the structural-scope
            // markers (`enter_group`, `set_branch`, `exit_group`) so
            // cross-backend traces from VM/self-host (which annotate
            // group_id / branch_path / effect_occurrence per effect)
            // line up with what wasm-gc emits. Eagerly register the
            // three host imports as soon as discovery sees an
            // independent product anywhere in the program.
            effects.register(EffectName::RecordEnterGroup);
            effects.register(EffectName::RecordSetBranch);
            effects.register(EffectName::RecordExitGroup);
            for item in items {
                discover_builtins_in_expr(&item.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                discover_builtins_in_expr(&k.node, builtins, effects, eq_helpers, type_registry);
                discover_builtins_in_expr(&v.node, builtins, effects, eq_helpers, type_registry);
            }
        }
        _ => {}
    }
}

/// True iff any reachable fn body calls `String.split` or `String.join`.
/// Used to gate registration of the (T=String) split/join helpers in
/// `lists::ListHelperRegistry::assign_slots`.
fn items_use_string_split_join(items: &[TopLevel]) -> bool {
    use crate::ast::{Expr, FnBody, Stmt};
    fn walk(e: &Expr) -> bool {
        match e {
            Expr::FnCall(callee, args) => {
                if let Expr::Attr(_parent, member) = &callee.node
                    && let Some(p) = expr_to_dotted_head(&callee.node)
                    && p == "String"
                    && (member == "split" || member == "join")
                {
                    return true;
                }
                walk(&callee.node) || args.iter().any(|a| walk(&a.node))
            }
            Expr::BinOp(_, l, r) => walk(&l.node) || walk(&r.node),
            Expr::Match { subject, arms } => {
                walk(&subject.node) || arms.iter().any(|a| walk(&a.body.node))
            }
            Expr::TailCall(boxed) => boxed.args.iter().any(|a| walk(&a.node)),
            Expr::Attr(obj, _) => walk(&obj.node),
            Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, e)| walk(&e.node)),
            Expr::Constructor(_, payload) => {
                payload.as_deref().map(|p| walk(&p.node)).unwrap_or(false)
            }
            Expr::List(items) => items.iter().any(|x| walk(&x.node)),
            Expr::InterpolatedStr(_) => false,
            _ => false,
        }
    }
    for item in items {
        if let TopLevel::FnDef(fd) = item {
            let FnBody::Block(stmts) = fd.body.as_ref();
            for stmt in stmts {
                let e = match stmt {
                    Stmt::Binding(_, _, e) | Stmt::Expr(e) => &e.node,
                };
                if walk(e) {
                    return true;
                }
            }
        }
    }
    false
}

/// Extract `Parent` from an `Attr(Parent, _)` callee — the parent is
/// either an Ident or a Resolved local. Anything else (chained Attr,
/// fn call result) returns None and the dispatch falls through to a
/// regular fn call.
fn expr_to_dotted_head(expr: &Expr) -> Option<&str> {
    if let Expr::Attr(parent, _) = expr {
        match &parent.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    } else {
        None
    }
}

/// `__rt_list_string_cons(head, tail) -> list`. Lets the JS host
/// build a `(ref null $list_String)` from outside without going
/// through user code; used by the host bridge that satisfies
/// `request_headers_load`.
fn emit_list_string_cons(registry: &TypeRegistry) -> Result<wasm_encoder::Function, WasmGcError> {
    let list_idx = registry
        .list_type_idx("List<String>")
        .ok_or(WasmGcError::Validation(
            "list_cons helper requires List<String> slot".into(),
        ))?;
    let mut f = wasm_encoder::Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Wasm-owned value factory exports. Effect imports that return
/// structured GC refs (`Option<String>`, records, `Result<T,E>`) can't
/// be implemented in JS directly because JS has no API to construct a
/// wasm-gc struct/variant. Instead, the binary exports per-type
/// constructor helpers; the host calls them and gets back a wasm-owned
/// ref it can hand straight to the importing code.
///
/// This is the same per-instantiation pattern as `__rt_string_from_lm`
/// and the per-(K,V) Map probes — pre-1.0 we ship one helper pair per
/// effect that needs it. Generic factories aren't worth the slot churn
/// while only three effects (`Terminal.readKey`, `Terminal.size`,
/// `Console.readLine`) cross the boundary with structured returns.
#[derive(Default)]
struct FactoryExports {
    /// `__rt_option_string_some(s)` / `__rt_option_string_none()` —
    /// emitted when `Terminal.readKey` is registered.
    opt_string_some: Option<FactorySlot>,
    opt_string_none: Option<FactorySlot>,
    /// `__rt_record_terminal_size_make(width, height)` — emitted when
    /// `Terminal.size` is registered.
    terminal_size_make: Option<FactorySlot>,
    /// `__rt_result_string_string_ok(s)` / `_err(s)` — emitted when
    /// `Console.readLine` (or any host effect that reports back a
    /// `Result<String, String>`, e.g. `Disk.readText`) is registered.
    result_string_string_ok: Option<FactorySlot>,
    result_string_string_err: Option<FactorySlot>,
    /// `__rt_result_unit_string_ok()` / `_err(s)` — emitted when any
    /// effect with a `Result<Unit, String>` return shape is registered
    /// (e.g. `Disk.writeText`, `Disk.delete`, `Tcp.close`).
    result_unit_string_ok: Option<FactorySlot>,
    result_unit_string_err: Option<FactorySlot>,
    /// `__rt_result_list_string_string_ok(list)` / `_err(s)` — emitted
    /// when an effect returning `Result<List<String>, String>` is
    /// registered (e.g. `Disk.listDir`).
    result_list_string_string_ok: Option<FactorySlot>,
    result_list_string_string_err: Option<FactorySlot>,
    /// `__rt_list_string_cons(head, tail) -> List<String>` /
    /// `__rt_list_string_nil() -> List<String>` — emitted when the
    /// host has to materialise a `List<String>` from the outside (the
    /// only case so far is `Disk.listDir`'s success arm).
    list_string_cons: Option<FactorySlot>,
    list_string_nil: Option<FactorySlot>,
    /// `__rt_record_tcp_connection_make(id, host, port)` — emitted
    /// when any `Tcp.*` effect is registered. The host hands the
    /// resulting record back as a Connection handle; subsequent
    /// `Tcp.writeLine / readLine / close` calls extract the `id`
    /// field on the host side to look up the underlying socket.
    tcp_connection_make: Option<FactorySlot>,
    /// `__rt_tcp_connection_id(c) -> String` — getter the host uses
    /// to recover the socket-pool key when dispatching writeLine /
    /// readLine / close.
    tcp_connection_id: Option<FactorySlot>,
    /// `__rt_result_tcp_connection_string_ok(c)` /
    /// `__rt_result_tcp_connection_string_err(e)` — emitted when
    /// `Tcp.connect` is registered.
    result_tcp_connection_string_ok: Option<FactorySlot>,
    result_tcp_connection_string_err: Option<FactorySlot>,
    /// `__rt_record_http_response_make(status, body, headers)` — emitted
    /// when any `Http.*` verb effect is registered.
    http_response_make: Option<FactorySlot>,
    /// `__rt_result_http_response_string_ok(r)` /
    /// `__rt_result_http_response_string_err(e)` — same gate.
    result_http_response_string_ok: Option<FactorySlot>,
    result_http_response_string_err: Option<FactorySlot>,
    /// `__rt_map_string_list_string_empty()` — empty headers map for
    /// the host to attach to its synthesised HttpResponse refs.
    map_string_list_string_empty: Option<FactorySlot>,
}

#[derive(Clone, Copy)]
struct FactorySlot {
    type_idx: u32,
    fn_idx: u32,
}

fn allocate_factory_exports(
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_fn_idx: &mut u32,
    registry: &TypeRegistry,
    effect_registry: &EffectRegistry,
) -> Result<FactoryExports, WasmGcError> {
    let mut fx = FactoryExports::default();

    // Option<String> factories — driven by `Terminal.readKey`.
    if effect_registry
        .iter()
        .any(|e| e == EffectName::TerminalReadKey)
    {
        let opt_idx = registry
            .option_type_idx("Option<String>")
            .ok_or(WasmGcError::Validation(
                "Terminal.readKey factory requires Option<String> slot".into(),
            ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Terminal.readKey factory requires String slot".into(),
            ))?;
        let s_ref = ref_null(s_idx);
        let opt_ref = ref_null(opt_idx);

        types.ty().function([s_ref], [opt_ref]);
        fx.opt_string_some = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([], [opt_ref]);
        fx.opt_string_none = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    // Terminal.Size record factory — driven by `Terminal.size`.
    if effect_registry
        .iter()
        .any(|e| e == EffectName::TerminalSize)
    {
        let rec_idx = registry
            .record_type_idx("Terminal.Size")
            .ok_or(WasmGcError::Validation(
                "Terminal.size factory requires Terminal.Size record slot".into(),
            ))?;
        let rec_ref = ref_null(rec_idx);
        types.ty().function([ValType::I64, ValType::I64], [rec_ref]);
        fx.terminal_size_make = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    // Result<String,String> factories — driven by any effect whose
    // host impl yields back a `Result<String, String>`.
    let needs_result_string_string = effect_registry.iter().any(|e| {
        matches!(
            e,
            EffectName::ConsoleReadLine
                | EffectName::DiskReadText
                | EffectName::TcpReadLine
                | EffectName::TcpSend
        )
    });
    if needs_result_string_string {
        let res_idx =
            registry
                .result_type_idx("Result<String,String>")
                .ok_or(WasmGcError::Validation(
                    "Result<String,String> factory required but slot not registered".into(),
                ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Result<String,String> factory requires String slot".into(),
            ))?;
        let s_ref = ref_null(s_idx);
        let res_ref = ref_null(res_idx);

        types.ty().function([s_ref], [res_ref]);
        fx.result_string_string_ok = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([s_ref], [res_ref]);
        fx.result_string_string_err = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    // Result<Unit, String> factories — Disk.{writeText, appendText,
    // delete, deleteDir, makeDir} all yield this shape; same for the
    // shape-equivalent Tcp.{writeLine, close, ping} effects.
    let needs_result_unit_string = effect_registry.iter().any(|e| {
        matches!(
            e,
            EffectName::DiskWriteText
                | EffectName::DiskAppendText
                | EffectName::DiskDelete
                | EffectName::DiskDeleteDir
                | EffectName::DiskMakeDir
                | EffectName::TcpWriteLine
                | EffectName::TcpClose
                | EffectName::TcpPing
        )
    });
    if needs_result_unit_string {
        let res_idx =
            registry
                .result_type_idx("Result<Unit,String>")
                .ok_or(WasmGcError::Validation(
                    "Result<Unit,String> factory required but slot not registered".into(),
                ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Result<Unit,String> factory requires String slot".into(),
            ))?;
        let s_ref = ref_null(s_idx);
        let res_ref = ref_null(res_idx);

        types.ty().function([], [res_ref]);
        fx.result_unit_string_ok = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([s_ref], [res_ref]);
        fx.result_unit_string_err = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    // Tcp.Connection record + Result<Tcp.Connection, String> — driven
    // by any Tcp.* effect (connect returns one; the rest consume one).
    let needs_tcp_connection = effect_registry.iter().any(|e| {
        matches!(
            e,
            EffectName::TcpConnect
                | EffectName::TcpWriteLine
                | EffectName::TcpReadLine
                | EffectName::TcpClose
        )
    });
    if needs_tcp_connection {
        let rec_idx = registry
            .record_type_idx("Tcp.Connection")
            .ok_or(WasmGcError::Validation(
                "Tcp.connect factory requires Tcp.Connection record slot".into(),
            ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Tcp.connect factory requires String slot".into(),
            ))?;
        let s_ref = ref_null(s_idx);
        let rec_ref = ref_null(rec_idx);

        types.ty().function([s_ref, s_ref, ValType::I64], [rec_ref]);
        fx.tcp_connection_make = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([rec_ref], [s_ref]);
        fx.tcp_connection_id = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }
    if effect_registry
        .iter()
        .any(|e| matches!(e, EffectName::TcpConnect))
    {
        let res_idx = registry
            .result_type_idx("Result<Tcp.Connection,String>")
            .ok_or(WasmGcError::Validation(
                "Tcp.connect requires Result<Tcp.Connection,String> slot".into(),
            ))?;
        let rec_idx = registry
            .record_type_idx("Tcp.Connection")
            .ok_or(WasmGcError::Validation(
                "Tcp.connect requires Tcp.Connection record slot".into(),
            ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Tcp.connect requires String slot".into(),
            ))?;
        let s_ref = ref_null(s_idx);
        let rec_ref = ref_null(rec_idx);
        let res_ref = ref_null(res_idx);

        types.ty().function([rec_ref], [res_ref]);
        fx.result_tcp_connection_string_ok = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([s_ref], [res_ref]);
        fx.result_tcp_connection_string_err = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    // HTTP response factories — driven by any verb effect.
    let needs_http_response = effect_registry.iter().any(|e| {
        matches!(
            e,
            EffectName::HttpGet
                | EffectName::HttpHead
                | EffectName::HttpDelete
                | EffectName::HttpPost
                | EffectName::HttpPut
                | EffectName::HttpPatch
        )
    });
    if needs_http_response {
        let res_idx = registry
            .result_type_idx("Result<HttpResponse,String>")
            .ok_or(WasmGcError::Validation(
                "Http.* requires Result<HttpResponse,String> slot".into(),
            ))?;
        let rec_idx = registry
            .record_type_idx("HttpResponse")
            .ok_or(WasmGcError::Validation(
                "Http.* requires HttpResponse record slot".into(),
            ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Http.* requires String slot".into(),
            ))?;
        let map_slots =
            registry
                .map_slots("Map<String,List<String>>")
                .ok_or(WasmGcError::Validation(
                    "Http.* requires Map<String,List<String>> slot".into(),
                ))?;
        let s_ref = ref_null(s_idx);
        let rec_ref = ref_null(rec_idx);
        let res_ref = ref_null(res_idx);
        let map_ref = ref_null(map_slots.map);
        let keys_ref = ref_null(map_slots.keys_array);
        let values_ref = ref_null(map_slots.values_array);
        let _ = (keys_ref, values_ref);

        types
            .ty()
            .function([ValType::I64, s_ref, map_ref], [rec_ref]);
        fx.http_response_make = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([rec_ref], [res_ref]);
        fx.result_http_response_string_ok = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([s_ref], [res_ref]);
        fx.result_http_response_string_err = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([], [map_ref]);
        fx.map_string_list_string_empty = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    // Result<List<String>, String> + List<String> builders — driven by
    // `Disk.listDir`.
    let needs_list_string_pair = effect_registry
        .iter()
        .any(|e| matches!(e, EffectName::DiskListDir));
    if needs_list_string_pair {
        let res_idx = registry
            .result_type_idx("Result<List<String>,String>")
            .ok_or(WasmGcError::Validation(
                "Result<List<String>,String> factory required but slot not registered".into(),
            ))?;
        let list_idx = registry
            .list_type_idx("List<String>")
            .ok_or(WasmGcError::Validation(
                "Result<List<String>,String> factory requires List<String> slot".into(),
            ))?;
        let s_idx = registry
            .string_array_type_idx
            .ok_or(WasmGcError::Validation(
                "Result<List<String>,String> factory requires String slot".into(),
            ))?;
        let s_ref = ref_null(s_idx);
        let list_ref = ref_null(list_idx);
        let res_ref = ref_null(res_idx);

        types.ty().function([list_ref], [res_ref]);
        fx.result_list_string_string_ok = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([s_ref], [res_ref]);
        fx.result_list_string_string_err = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([s_ref, list_ref], [list_ref]);
        fx.list_string_cons = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;

        types.ty().function([], [list_ref]);
        fx.list_string_nil = Some(FactorySlot {
            type_idx: *next_type_idx,
            fn_idx: *next_fn_idx,
        });
        *next_type_idx += 1;
        *next_fn_idx += 1;
    }

    Ok(fx)
}

fn ref_null(type_idx: u32) -> ValType {
    ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(type_idx),
    })
}

impl FactoryExports {
    fn emit_function_entries(&self, funcs: &mut FunctionSection) {
        for slot in self.iter_slots() {
            funcs.function(slot.type_idx);
        }
    }

    fn emit_exports(&self, exports: &mut ExportSection) {
        if let Some(s) = self.opt_string_some {
            exports.export("__rt_option_string_some", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.opt_string_none {
            exports.export("__rt_option_string_none", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.terminal_size_make {
            exports.export("__rt_record_terminal_size_make", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_string_string_ok {
            exports.export("__rt_result_string_string_ok", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_string_string_err {
            exports.export("__rt_result_string_string_err", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_unit_string_ok {
            exports.export("__rt_result_unit_string_ok", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_unit_string_err {
            exports.export("__rt_result_unit_string_err", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_list_string_string_ok {
            exports.export(
                "__rt_result_list_string_string_ok",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.result_list_string_string_err {
            exports.export(
                "__rt_result_list_string_string_err",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.list_string_cons {
            exports.export("__rt_list_string_cons", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.list_string_nil {
            exports.export("__rt_list_string_nil", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.tcp_connection_make {
            exports.export(
                "__rt_record_tcp_connection_make",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.tcp_connection_id {
            exports.export("__rt_tcp_connection_id", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_tcp_connection_string_ok {
            exports.export(
                "__rt_result_tcp_connection_string_ok",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.result_tcp_connection_string_err {
            exports.export(
                "__rt_result_tcp_connection_string_err",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.http_response_make {
            exports.export("__rt_record_http_response_make", ExportKind::Func, s.fn_idx);
        }
        if let Some(s) = self.result_http_response_string_ok {
            exports.export(
                "__rt_result_http_response_string_ok",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.result_http_response_string_err {
            exports.export(
                "__rt_result_http_response_string_err",
                ExportKind::Func,
                s.fn_idx,
            );
        }
        if let Some(s) = self.map_string_list_string_empty {
            exports.export(
                "__rt_map_string_list_string_empty",
                ExportKind::Func,
                s.fn_idx,
            );
        }
    }

    fn emit_bodies(
        &self,
        codes: &mut CodeSection,
        registry: &TypeRegistry,
    ) -> Result<(), WasmGcError> {
        if self.opt_string_some.is_some() {
            codes.function(&emit_factory_option_string_some(registry)?);
        }
        if self.opt_string_none.is_some() {
            codes.function(&emit_factory_option_string_none(registry)?);
        }
        if self.terminal_size_make.is_some() {
            codes.function(&emit_factory_terminal_size_make(registry)?);
        }
        if self.result_string_string_ok.is_some() {
            codes.function(&emit_factory_result_string_string_ok(registry)?);
        }
        if self.result_string_string_err.is_some() {
            codes.function(&emit_factory_result_string_string_err(registry)?);
        }
        if self.result_unit_string_ok.is_some() {
            codes.function(&emit_factory_result_unit_string_ok(registry)?);
        }
        if self.result_unit_string_err.is_some() {
            codes.function(&emit_factory_result_unit_string_err(registry)?);
        }
        if self.result_list_string_string_ok.is_some() {
            codes.function(&emit_factory_result_list_string_string_ok(registry)?);
        }
        if self.result_list_string_string_err.is_some() {
            codes.function(&emit_factory_result_list_string_string_err(registry)?);
        }
        if self.list_string_cons.is_some() {
            codes.function(&emit_factory_list_string_cons(registry)?);
        }
        if self.list_string_nil.is_some() {
            codes.function(&emit_factory_list_string_nil(registry)?);
        }
        if self.tcp_connection_make.is_some() {
            codes.function(&emit_factory_tcp_connection_make(registry)?);
        }
        if self.tcp_connection_id.is_some() {
            codes.function(&emit_factory_tcp_connection_id(registry)?);
        }
        if self.result_tcp_connection_string_ok.is_some() {
            codes.function(&emit_factory_result_tcp_connection_string_ok(registry)?);
        }
        if self.result_tcp_connection_string_err.is_some() {
            codes.function(&emit_factory_result_tcp_connection_string_err(registry)?);
        }
        if self.http_response_make.is_some() {
            codes.function(&emit_factory_http_response_make(registry)?);
        }
        if self.result_http_response_string_ok.is_some() {
            codes.function(&emit_factory_result_http_response_string_ok(registry)?);
        }
        if self.result_http_response_string_err.is_some() {
            codes.function(&emit_factory_result_http_response_string_err(registry)?);
        }
        if self.map_string_list_string_empty.is_some() {
            codes.function(&emit_factory_map_string_list_string_empty(registry)?);
        }
        Ok(())
    }

    fn iter_slots(&self) -> impl Iterator<Item = FactorySlot> + '_ {
        [
            self.opt_string_some,
            self.opt_string_none,
            self.terminal_size_make,
            self.result_string_string_ok,
            self.result_string_string_err,
            self.result_unit_string_ok,
            self.result_unit_string_err,
            self.result_list_string_string_ok,
            self.result_list_string_string_err,
            self.list_string_cons,
            self.list_string_nil,
            self.tcp_connection_make,
            self.tcp_connection_id,
            self.result_tcp_connection_string_ok,
            self.result_tcp_connection_string_err,
            self.http_response_make,
            self.result_http_response_string_ok,
            self.result_http_response_string_err,
            self.map_string_list_string_empty,
        ]
        .into_iter()
        .flatten()
    }
}

fn emit_factory_option_string_some(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let opt_idx = registry
        .option_type_idx("Option<String>")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructNew(opt_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_option_string_none(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let opt_idx = registry
        .option_type_idx("Option<String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::StructNew(opt_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_terminal_size_make(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let rec_idx = registry
        .record_type_idx("Terminal.Size")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // params (width: i64, height: i64) → struct in declaration order.
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(rec_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_string_string_ok(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<String,String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // Result layout matches `emit_result_constructor`: tag, T, E.
    // Ok: tag=1, payload=arg, E=null.
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_string_string_err(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<String,String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // Err: tag=0, T=null, payload=arg.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `Result<Unit, String>::Ok(())` factory — Unit lowers to the i32
/// placeholder slot in the Result struct.
fn emit_factory_result_unit_string_ok(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<Unit,String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // tag=1, T=i32 placeholder, E=null
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_unit_string_err(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<Unit,String>")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // tag=0, T=i32 placeholder, E=arg
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_list_string_string_ok(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<List<String>,String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // tag=1, T=arg (List<String> ref), E=null
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_list_string_string_err(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<List<String>,String>")
        .expect("checked at allocation");
    let list_idx = registry
        .list_type_idx("List<String>")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    // tag=0, T=null List<String>, E=arg
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `__rt_list_string_cons(head, tail) -> List<String>`. Same struct
/// shape as user-emitted Cons cells (head field, tail ref).
fn emit_factory_list_string_cons(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let list_idx = registry
        .list_type_idx("List<String>")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::StructNew(list_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_list_string_nil(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let list_idx = registry
        .list_type_idx("List<String>")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        list_idx,
    )));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `Tcp.Connection { id, host, port }` factory. Field order must
/// match the declaration in `builtin_records::TCP_CONNECTION`.
fn emit_factory_tcp_connection_make(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let rec_idx = registry
        .record_type_idx("Tcp.Connection")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructNew(rec_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `__rt_tcp_connection_id(c)` — read field 0 of the record.
fn emit_factory_tcp_connection_id(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let rec_idx = registry
        .record_type_idx("Tcp.Connection")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefCastNonNull(
        wasm_encoder::HeapType::Concrete(rec_idx),
    ));
    f.instruction(&Instruction::StructGet {
        struct_type_index: rec_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_tcp_connection_string_ok(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<Tcp.Connection,String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_tcp_connection_string_err(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<Tcp.Connection,String>")
        .expect("checked at allocation");
    let rec_idx = registry
        .record_type_idx("Tcp.Connection")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        rec_idx,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// `HttpResponse { status, body, headers }` factory.
fn emit_factory_http_response_make(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let rec_idx = registry
        .record_type_idx("HttpResponse")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::StructNew(rec_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_http_response_string_ok(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<HttpResponse,String>")
        .expect("checked at allocation");
    let s_idx = registry
        .string_array_type_idx
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        s_idx,
    )));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

fn emit_factory_result_http_response_string_err(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let res_idx = registry
        .result_type_idx("Result<HttpResponse,String>")
        .expect("checked at allocation");
    let rec_idx = registry
        .record_type_idx("HttpResponse")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        rec_idx,
    )));
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::StructNew(res_idx));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Empty `Map<String, List<String>>`. The map struct layout is `(size:
/// i32, cap: i32, keys_ref, values_ref)` per `MapSlots` — produce an
/// all-zero / null-ref map.
fn emit_factory_map_string_list_string_empty(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
    let slots = registry
        .map_slots("Map<String,List<String>>")
        .expect("checked at allocation");
    let mut f = Function::new([]);
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        slots.keys_array,
    )));
    f.instruction(&Instruction::RefNull(wasm_encoder::HeapType::Concrete(
        slots.values_array,
    )));
    f.instruction(&Instruction::StructNew(slots.map));
    f.instruction(&Instruction::End);
    Ok(f)
}

/// Slots reserved for the synthesised `aver_http_handle` wrapper.
struct HandlerWrapper {
    /// Position of the user's `(HttpRequest) -> HttpResponse` fn in
    /// `fn_defs`.
    user_handler_idx: usize,
    wrapper_type: u32,
    wrapper_fn: u32,
    /// Type + fn indices for `__rt_list_string_cons(head, tail) ->
    /// List<String>`. Lets the JS host build a `List<String>` from
    /// the outside (e.g. for the request-headers map's value lists).
    list_cons_type: u32,
    list_cons_fn: u32,
}

/// Synthesise the body of `aver_http_handle()`. Reads the request
/// fields via `Request.*` imports, allocates an `HttpRequest`,
/// invokes the user handler, walks the resulting `HttpResponse`'s
/// headers Map and dispatches one `Response.setHeader(name, value)`
/// per (key, value) pair before finalising via `Response.text(status,
/// body)`. Mirrors the `--bridge fetch` shape from the legacy
/// backend (`src/codegen/wasm/expr/emit.rs::emit_record_create`).
fn emit_handler_wrapper(
    registry: &TypeRegistry,
    fn_map: &super::body::FnMap,
    user_handler_wasm_idx: u32,
) -> Result<wasm_encoder::Function, WasmGcError> {
    use wasm_encoder::{BlockType, Function, HeapType, Instruction, RefType};

    let s_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "aver_http_handle wrapper requires String slot".into(),
        ))?;
    let req_idx = registry
        .records
        .get("HttpRequest")
        .copied()
        .ok_or(WasmGcError::Validation(
            "aver_http_handle wrapper requires HttpRequest record slot".into(),
        ))?;
    let resp_idx = registry
        .records
        .get("HttpResponse")
        .copied()
        .ok_or(WasmGcError::Validation(
            "aver_http_handle wrapper requires HttpResponse record slot".into(),
        ))?;
    let map_slots =
        registry
            .map_slots("Map<String,List<String>>")
            .ok_or(WasmGcError::Validation(
                "aver_http_handle wrapper requires `Map<String, List<String>>` slot".into(),
            ))?;
    let list_idx = registry
        .list_type_idx("List<String>")
        .ok_or(WasmGcError::Validation(
            "aver_http_handle wrapper requires `List<String>` slot".into(),
        ))?;

    let request_method_fn =
        fn_map
            .effects
            .get("Request.method")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Request.method effect not registered".into(),
            ))?;
    let request_url_fn =
        fn_map
            .effects
            .get("Request.url")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Request.url effect not registered".into(),
            ))?;
    let request_query_fn =
        fn_map
            .effects
            .get("Request.query")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Request.query effect not registered".into(),
            ))?;
    let request_body_fn =
        fn_map
            .effects
            .get("Request.body")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Request.body effect not registered".into(),
            ))?;
    let request_headers_load_fn =
        fn_map
            .effects
            .get("Request.headersLoad")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Request.headersLoad effect not registered".into(),
            ))?;
    let response_text_fn =
        fn_map
            .effects
            .get("Response.text")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Response.text effect not registered".into(),
            ))?;
    let response_set_header_fn =
        fn_map
            .effects
            .get("Response.setHeader")
            .copied()
            .ok_or(WasmGcError::Validation(
                "Response.setHeader effect not registered".into(),
            ))?;

    let s_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(s_idx),
    });
    let req_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(req_idx),
    });
    let resp_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(resp_idx),
    });
    let map_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(map_slots.map),
    });
    let keys_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(map_slots.keys_array),
    });
    let values_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(map_slots.values_array),
    });
    let list_ref = ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(list_idx),
    });

    // Locals layout (after the empty params):
    //  0=method, 1=url, 2=query, 3=body, 4=req_headers, 5=req,
    //  6=resp, 7=status, 8=resp_body, 9=resp_headers,
    // 10=keys_arr, 11=values_arr, 12=cap, 13=i,
    // 14=key, 15=values_list.
    let mut f = Function::new([
        (4, s_ref),
        (1, map_ref),
        (1, req_ref),
        (1, resp_ref),
        (1, ValType::I64),
        (1, s_ref),
        (1, map_ref),
        (1, keys_ref),
        (1, values_ref),
        (2, ValType::I32),
        (1, s_ref),
        (1, list_ref),
    ]);

    // Build HttpRequest from host effects.
    f.instruction(&Instruction::Call(request_method_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(0));
    f.instruction(&Instruction::Call(request_url_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(1));
    f.instruction(&Instruction::Call(request_query_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(2));
    f.instruction(&Instruction::Call(request_body_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(3));
    f.instruction(&Instruction::Call(request_headers_load_fn));
    f.instruction(&Instruction::LocalSet(4));

    // struct.new $http_request method url query body headers
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::LocalGet(2));
    f.instruction(&Instruction::LocalGet(3));
    f.instruction(&Instruction::LocalGet(4));
    f.instruction(&Instruction::StructNew(req_idx));
    f.instruction(&Instruction::LocalSet(5));

    // resp = handler(req)
    f.instruction(&Instruction::LocalGet(5));
    f.instruction(&Instruction::Call(user_handler_wasm_idx));
    f.instruction(&Instruction::LocalSet(6));

    // status = resp.status (i64)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::StructGet {
        struct_type_index: resp_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::LocalSet(7));
    // resp_body = resp.body
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::StructGet {
        struct_type_index: resp_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(8));
    // resp_headers = resp.headers (Map ref)
    f.instruction(&Instruction::LocalGet(6));
    f.instruction(&Instruction::StructGet {
        struct_type_index: resp_idx,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(9));

    // Read map cap + arrays into iteration slots.
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_slots.map,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(12));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_slots.map,
        field_index: 2,
    });
    f.instruction(&Instruction::LocalSet(10));
    f.instruction(&Instruction::LocalGet(9));
    f.instruction(&Instruction::StructGet {
        struct_type_index: map_slots.map,
        field_index: 3,
    });
    f.instruction(&Instruction::LocalSet(11));

    // i = 0
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::LocalSet(13));
    // outer block / loop over the keys array
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    // if i >= cap break
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::LocalGet(12));
    f.instruction(&Instruction::I32GeU);
    f.instruction(&Instruction::BrIf(1));

    // key = keys_arr[i]
    f.instruction(&Instruction::LocalGet(10));
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::ArrayGet(map_slots.keys_array));
    f.instruction(&Instruction::LocalSet(14));

    // if key non-null, walk values list
    f.instruction(&Instruction::LocalGet(14));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    // values_list = values_arr[i]
    f.instruction(&Instruction::LocalGet(11));
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::ArrayGet(map_slots.values_array));
    f.instruction(&Instruction::LocalSet(15));
    // Walk list: while not null: response_set_header(key, head); cur = tail.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(15));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::BrIf(1));
    // response_set_header(key, list.head)
    f.instruction(&Instruction::LocalGet(14));
    f.instruction(&Instruction::LocalGet(15));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    f.instruction(&Instruction::Call(response_set_header_fn));
    // cur = cur.tail
    f.instruction(&Instruction::LocalGet(15));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 1,
    });
    f.instruction(&Instruction::LocalSet(15));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // list loop
    f.instruction(&Instruction::End); // list block
    f.instruction(&Instruction::End); // if key non-null

    // i++
    f.instruction(&Instruction::LocalGet(13));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::LocalSet(13));
    f.instruction(&Instruction::Br(0));
    f.instruction(&Instruction::End); // outer loop
    f.instruction(&Instruction::End); // outer block

    // response_text(status, body)
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(8));
    f.instruction(&Instruction::Call(response_text_fn));

    f.instruction(&Instruction::End);
    Ok(f)
}

/// Wasm fn-type and fn-idx slots for the two `__rt_string_*` host
/// bridge exports plus the linear-memory transport buffer.
///
/// Why this exists: a JS host (e.g. Cloudflare Workers via
/// `tools/edge/`) can't directly allocate or read engine-managed
/// `(array i8)` refs without JS String Builtins (stage-4 standard,
/// not yet enabled on every host). Per-byte exports (one JS↔wasm
/// boundary crossing per byte) would dominate the workload — ~100 ns
/// per crossing × 50 KB body = 10 ms just for I/O, eclipsing the
/// actual fractal render. So we expose a tiny linear memory as a
/// bulk transport buffer and two bulk-copy helpers. JS writes a
/// UTF-8 buffer into the LM with `TextEncoder.encodeInto`, calls
/// `__rt_string_from_lm(len)` once to materialise it as a guest
/// `(array i8)`. For the return path, `__rt_string_to_lm(s)` copies
/// `s.len` bytes back to LM and returns the count; JS reads them
/// with `TextDecoder.decode(memory.subarray(0, len))`. One boundary
/// crossing per direction; the inner copy loop runs at native speed
/// inside wasm.
struct BridgeIndices {
    from_lm_type: u32,
    to_lm_type: u32,
    pages_type: u32,
    grow_type: u32,
    from_lm_fn: u32,
    to_lm_fn: u32,
    pages_fn: u32,
    grow_fn: u32,
}

struct BridgeTypeSlots {
    from_lm_type: u32,
    to_lm_type: u32,
    pages_type: u32,
    grow_type: u32,
}

fn emit_bridge_types(
    types: &mut TypeSection,
    registry: &TypeRegistry,
    next_type_idx: &mut u32,
) -> Result<BridgeTypeSlots, WasmGcError> {
    let s_idx = registry
        .string_array_type_idx
        .ok_or(WasmGcError::Validation(
            "bridge helpers require String slot to be allocated".into(),
        ))?;
    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(s_idx),
    });
    // from_lm : (len: i32) -> string  (reads bytes from LM[0..len])
    types.ty().function([ValType::I32], [s_ref]);
    let from_lm = *next_type_idx;
    *next_type_idx += 1;
    // to_lm : (s: string) -> i32      (writes s into LM[0..s.len], returns s.len)
    types.ty().function([s_ref], [ValType::I32]);
    let to_lm = *next_type_idx;
    *next_type_idx += 1;
    // pages : () -> i32  (= memory.size, in 64 KiB pages)
    types.ty().function([], [ValType::I32]);
    let pages = *next_type_idx;
    *next_type_idx += 1;
    // grow : (pages: i32) -> i32  (= memory.grow result; -1 on fail)
    types.ty().function([ValType::I32], [ValType::I32]);
    let grow = *next_type_idx;
    *next_type_idx += 1;
    Ok(BridgeTypeSlots {
        from_lm_type: from_lm,
        to_lm_type: to_lm,
        pages_type: pages,
        grow_type: grow,
    })
}

fn emit_bridge_bodies(codes: &mut CodeSection, registry: &TypeRegistry) -> Result<(), WasmGcError> {
    let s_idx = registry
        .string_array_type_idx
        .expect("bridge bodies emitted only when string slot exists");
    let padding = wat_helper::padding_types(s_idx);

    // from_lm(len) → string. Allocate `(array i8)` of `len`, then
    // copy LM[0..len] byte-by-byte. Loop over `i32.load8_u` + `array.set`.
    let from_lm_wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (memory 1)
          (func (export "helper") (param $len i32) (result (ref null $string))
            (local $arr (ref null $string))
            (local $i i32)
            local.get $len
            array.new_default $string
            local.set $arr
            i32.const 0
            local.set $i
            (block $break
              (loop $next
                local.get $i
                local.get $len
                i32.ge_u
                br_if $break

                local.get $arr
                local.get $i
                local.get $i
                i32.load8_u
                array.set $string

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $next))
            local.get $arr)
        )
    "#
    );
    codes.function(&wat_helper::compile_wat_helper(&from_lm_wat)?);

    // to_lm(s) → i32 (= s.len). Auto-grow memory if `s.len` exceeds
    // current LM capacity, then loop-write bytes to LM[0..s.len].
    let to_lm_wat = format!(
        r#"
        (module
          {padding}
          (type $string (array (mut i8)))
          (memory 1)
          (func (export "helper") (param $s (ref null $string)) (result i32)
            (local $len i32)
            (local $i i32)
            (local $needed i32)
            (local $current i32)
            local.get $s
            array.len
            local.set $len

            ;; needed = (len + 65535) >> 16
            local.get $len
            i32.const 65535
            i32.add
            i32.const 16
            i32.shr_u
            local.set $needed

            memory.size
            local.set $current

            local.get $needed
            local.get $current
            i32.gt_u
            (if
              (then
                local.get $needed
                local.get $current
                i32.sub
                memory.grow
                drop))

            i32.const 0
            local.set $i
            (block $break
              (loop $next
                local.get $i
                local.get $len
                i32.ge_u
                br_if $break

                local.get $i
                local.get $s
                local.get $i
                array.get_u $string
                i32.store8

                local.get $i
                i32.const 1
                i32.add
                local.set $i
                br $next))
            local.get $len)
        )
    "#
    );
    codes.function(&wat_helper::compile_wat_helper(&to_lm_wat)?);

    // pages() -> i32 (= memory.size). Trivially small; wasm-encoder.
    let mut pages = wasm_encoder::Function::new([]);
    pages.instruction(&Instruction::MemorySize(0));
    pages.instruction(&Instruction::End);
    codes.function(&pages);

    // grow(pages) -> i32 (= memory.grow result; -1 on fail).
    let mut grow = wasm_encoder::Function::new([]);
    grow.instruction(&Instruction::LocalGet(0));
    grow.instruction(&Instruction::MemoryGrow(0));
    grow.instruction(&Instruction::End);
    codes.function(&grow);
    Ok(())
}

fn validate(bytes: &[u8]) -> Result<(), WasmGcError> {
    use wasmparser::{Validator, WasmFeatures};

    let features = WasmFeatures::default()
        | WasmFeatures::GC
        | WasmFeatures::REFERENCE_TYPES
        | WasmFeatures::FUNCTION_REFERENCES
        | WasmFeatures::TAIL_CALL;
    let mut validator = Validator::new_with_features(features);
    validator
        .validate_all(bytes)
        .map_err(|e| WasmGcError::Validation(format!("{e}")))?;
    Ok(())
}
