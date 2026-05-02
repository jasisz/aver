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
use super::body::{FnEntry, FnMap, emit_fn_body};
use super::builtins::{BuiltinName, BuiltinRegistry};
use super::effects::{EffectName, EffectRegistry};
use super::maps::MapHelperRegistry;
use super::types::{TypeRegistry, param_types, record_struct_type, return_results};

use crate::ast::{Expr, FnDef, Stmt, TopLevel, TypeDef};

pub(super) fn emit_module(
    items: &[TopLevel],
    handler_name: Option<&str>,
) -> Result<Vec<u8>, WasmGcError> {
    let registry = TypeRegistry::build_with_handler(items, handler_name.is_some());

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
    for fd in &fn_defs {
        discover_builtins_in_fn(fd, &mut builtin_registry, &mut effect_registry);
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
        if let Some(elem) =
            super::types::TypeRegistry::list_element_type(canonical)
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
    // `main` is optional — modules that act as a Worker handler
    // (e.g. `tools/edge-gc/handler.av`) export `handler` instead and
    // never run `_start`. When absent, `_start` is emitted as a no-op
    // so the module shape stays valid.
    let main_idx: Option<usize> = fn_defs.iter().position(|fd| fd.name == "main");

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
    let fn_map = FnMap {
        by_name,
        builtins: builtin_idx_lookup,
        effects: effect_idx_lookup,
        map_helpers: map_helpers_lookup,
        list_ops: list_ops_lookup,
        vfl_ops: vfl_ops_lookup,
        zip_ops: zip_ops_lookup,
        string_split_ops,
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
    module.section(&exports);

    // ── Data count section (must precede code when using passive
    //     segments via array.new_data / data.drop).
    if !registry.string_literals.is_empty() {
        let count = DataCountSection {
            count: registry.string_literals.len() as u32,
        };
        module.section(&count);
    }

    // ── Code section ───────────────────────────────────────────────
    let mut codes = CodeSection::new();

    // _start: call main if present, drop its result on the way out;
    // otherwise emit a no-op body. Worker-shaped modules without a
    // top-level `main` rely on the host calling a different export.
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
        let extra_locals_dry = emit_fn_body(&mut probe, fd, &fn_map, self_wasm_idx, &registry)?;

        let local_groups: Vec<(u32, ValType)> = extra_locals_dry.iter().map(|v| (1, *v)).collect();
        let mut func = Function::new(local_groups);
        let _ = emit_fn_body(&mut func, fd, &fn_map, self_wasm_idx, &registry)?;
        codes.function(&func);
    }

    // Builtin helper bodies — emitted after user fns so their own
    // wasm fn indices come last. Bodies are stubs today (Unreachable);
    // real impls land in `builtins/` per phase 3c roadmap.
    builtin_registry.emit_helper_bodies(&mut codes, &registry)?;

    // Map helper bodies (hash, eq, empty, set, get, len per
    // instantiation) — emitted last so their wasm fn indices line up
    // with what `MapHelperRegistry::assign_slots` recorded.
    // Snapshot list eq/hash fn idxes so map record-key helpers can
    // dispatch List<T> field types without a cross-module lookup.
    let mut list_eq_hash_lookup: HashMap<String, (u32, u32)> = HashMap::new();
    for canonical in &registry.list_order {
        if let Some(o) = list_helpers.list_ops_for(canonical)
            && let (Some(eq_fn), Some(hash_fn)) = (o.eq, o.hash)
        {
            list_eq_hash_lookup.insert(canonical.clone(), (eq_fn, hash_fn));
        }
    }
    map_helpers.emit_helper_bodies(&mut codes, &registry, &list_eq_hash_lookup)?;

    // List / Vector.fromList / String.split-join helper bodies.
    let string_eq_fn_idx = builtin_registry.lookup_wasm_fn_idx(BuiltinName::StringEq);
    list_helpers.emit_helper_bodies(&mut codes, &registry, string_eq_fn_idx)?;

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

    module.section(&codes);

    // ── Data section ───────────────────────────────────────────────
    // Passive segments holding String literal byte sequences. Emitted
    // last; `array.new_data $string $segment_idx` reads from these.
    if !registry.string_literals.is_empty() {
        let mut data = DataSection::new();
        for bytes in &registry.string_literals {
            data.passive(bytes.iter().copied());
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
    use wasm_encoder::{
        ArrayType, CompositeInnerType, CompositeType, StructType, SubType,
    };
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
                entries.push((idx, mk_struct(st.fields.iter().copied().collect())));
            }
            TopLevel::TypeDef(TypeDef::Sum { variants, .. }) => {
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
                    let info = registry.variant(&v.name).ok_or(WasmGcError::Validation(
                        format!("variant `{}` not registered", v.name),
                    ))?;
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
        let element = TypeRegistry::vector_element_type(canonical).ok_or(
            WasmGcError::Validation(format!(
                "registered vector `{canonical}` has no parsable element type"
            )),
        )?;
        let elem_val = super::types::aver_to_wasm(element, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Vector element type `{element}` has no wasm representation"
            )),
        )?;
        let idx = registry.vector_type_idx(canonical).ok_or(WasmGcError::Validation(
            format!("vector `{canonical}` not registered"),
        ))?;
        entries.push((
            idx,
            mk_array(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(elem_val),
                mutable: true,
            }),
        ));
    }

    // `Result<T, E>` — `(struct (mut i32 tag) (mut T ok) (mut E err))`.
    for canonical in &registry.result_order {
        let (t_aver, e_aver) = TypeRegistry::result_te(canonical).ok_or(
            WasmGcError::Validation(format!(
                "registered result `{canonical}` has no parsable T, E"
            )),
        )?;
        let t_val = super::types::aver_to_wasm(t_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Result T type `{t_aver}` has no wasm representation"
            )),
        )?;
        let e_val = super::types::aver_to_wasm(e_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Result E type `{e_aver}` has no wasm representation"
            )),
        )?;
        let idx = registry.result_type_idx(canonical).ok_or(WasmGcError::Validation(
            format!("result `{canonical}` not registered"),
        ))?;
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
        let element = TypeRegistry::list_element_type(canonical).ok_or(
            WasmGcError::Validation(format!(
                "registered list `{canonical}` has no parsable element type"
            )),
        )?;
        let elem_val = super::types::aver_to_wasm(element, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "List element type `{element}` has no wasm representation"
            )),
        )?;
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
        let element = TypeRegistry::option_element_type(canonical).ok_or(
            WasmGcError::Validation(format!(
                "registered option `{canonical}` has no parsable element type"
            )),
        )?;
        let elem_val = super::types::aver_to_wasm(element, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Option element type `{element}` has no wasm representation"
            )),
        )?;
        let idx = registry.option_type_idx(canonical).ok_or(WasmGcError::Validation(
            format!("option `{canonical}` not registered"),
        ))?;
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
            WasmGcError::Validation(format!(
                "registered map `{canonical}` has no parsable K, V"
            )),
        )?;
        let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Map value type `{v_aver}` has no wasm representation"
            )),
        )?;
        // Keys array element: for primitive K, a `(ref null
        // $primitive_key_box_K)` so the empty-slot marker stays
        // uniform; for ref K (String / record), the K's own ref.
        let key_storage_val = if let Some(box_idx) =
            registry.primitive_key_box_idx(k_aver)
        {
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(box_idx),
            })
        } else {
            super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(
                WasmGcError::Validation(format!(
                    "Map key type `{k_aver}` has no wasm representation"
                )),
            )?
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
        let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "primitive key box: K=`{k_aver}` has no wasm representation"
            )),
        )?;
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

    // `Tuple<A, B>` — `(struct (mut A) (mut B))`. Used by Map.entries
    // (returns List<Tuple<K, V>>), Map.fromList, List.zip.
    for canonical in &registry.tuple_order {
        let (a_aver, b_aver) = TypeRegistry::tuple_ab(canonical).ok_or(
            WasmGcError::Validation(format!(
                "registered tuple `{canonical}` has no parsable A, B"
            )),
        )?;
        let a_val = super::types::aver_to_wasm(a_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Tuple A type `{a_aver}` has no wasm representation"
            )),
        )?;
        let b_val = super::types::aver_to_wasm(b_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Tuple B type `{b_aver}` has no wasm representation"
            )),
        )?;
        let idx = registry.tuple_type_idx(canonical).ok_or(WasmGcError::Validation(
            format!("tuple `{canonical}` not registered"),
        ))?;
        entries.push((
            idx,
            mk_struct(vec![
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(a_val),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(b_val),
                    mutable: true,
                },
            ]),
        ));
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
        entries.push((idx, mk_struct(st.fields.iter().copied().collect())));
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
) {
    let crate::ast::FnBody::Block(stmts) = fd.body.as_ref();
    for stmt in stmts {
        discover_builtins_in_stmt(stmt, builtins, effects);
    }
}

fn discover_builtins_in_stmt(
    stmt: &Stmt,
    builtins: &mut BuiltinRegistry,
    effects: &mut EffectRegistry,
) {
    match stmt {
        Stmt::Binding(_, _, e) | Stmt::Expr(e) => {
            discover_builtins_in_expr(&e.node, builtins, effects)
        }
    }
}

fn discover_builtins_in_expr(
    expr: &Expr,
    builtins: &mut BuiltinRegistry,
    effects: &mut EffectRegistry,
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
            }
            discover_builtins_in_expr(&callee.node, builtins, effects);
            for arg in args {
                discover_builtins_in_expr(&arg.node, builtins, effects);
            }
        }
        Expr::BinOp(_, l, r) => {
            discover_builtins_in_expr(&l.node, builtins, effects);
            discover_builtins_in_expr(&r.node, builtins, effects);
        }
        Expr::Match { subject, arms } => {
            discover_builtins_in_expr(&subject.node, builtins, effects);
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
                discover_builtins_in_expr(&arm.body.node, builtins, effects);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                discover_builtins_in_expr(&arg.node, builtins, effects);
            }
        }
        Expr::Attr(obj, _) => discover_builtins_in_expr(&obj.node, builtins, effects),
        Expr::Constructor(_, payload) => {
            if let Some(p) = payload.as_deref() {
                discover_builtins_in_expr(&p.node, builtins, effects);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                discover_builtins_in_expr(&e.node, builtins, effects);
            }
        }
        // `InterpolatedStr` lowers to `array.new_fixed` + the variadic
        // concat helper. Register it here so the helper's wasm fn
        // index is allocated by the time emission runs. Each Parsed
        // part may also need `Int.toString` (if its type is Int) —
        // we conservatively register that too; unused registrations
        // are stripped by `wasm-opt -Oz`.
        Expr::InterpolatedStr(parts) => {
            builtins.register(BuiltinName::StringConcatN);
            builtins.register(BuiltinName::IntToString);
            for p in parts {
                if let StrPart::Parsed(inner) = p {
                    discover_builtins_in_expr(&inner.node, builtins, effects);
                }
            }
        }
        Expr::List(items) => {
            for item in items {
                discover_builtins_in_expr(&item.node, builtins, effects);
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
            Expr::Constructor(_, payload) => payload.as_deref().map(|p| walk(&p.node)).unwrap_or(false),
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

/// Validate emitted bytes with `wasmparser` configured for the wasm-gc
/// + tail-call feature set we target.
/// `__rt_list_string_cons(head, tail) -> list`. Lets the JS host
/// build a `(ref null $list_String)` from outside without going
/// through user code; used by the host bridge that satisfies
/// `request_headers_load`.
fn emit_list_string_cons(
    registry: &TypeRegistry,
) -> Result<wasm_encoder::Function, WasmGcError> {
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
    let map_slots = registry
        .map_slots("Map<String,List<String>>")
        .ok_or(WasmGcError::Validation(
            "aver_http_handle wrapper requires `Map<String, List<String>>` slot".into(),
        ))?;
    let list_idx = registry
        .list_type_idx("List<String>")
        .ok_or(WasmGcError::Validation(
            "aver_http_handle wrapper requires `List<String>` slot".into(),
        ))?;

    let request_method_fn = fn_map
        .effects
        .get("Request.method")
        .copied()
        .ok_or(WasmGcError::Validation("Request.method effect not registered".into()))?;
    let request_url_fn = fn_map
        .effects
        .get("Request.url")
        .copied()
        .ok_or(WasmGcError::Validation("Request.url effect not registered".into()))?;
    let request_query_fn = fn_map
        .effects
        .get("Request.query")
        .copied()
        .ok_or(WasmGcError::Validation("Request.query effect not registered".into()))?;
    let request_body_fn = fn_map
        .effects
        .get("Request.body")
        .copied()
        .ok_or(WasmGcError::Validation("Request.body effect not registered".into()))?;
    let request_headers_load_fn = fn_map
        .effects
        .get("Request.headersLoad")
        .copied()
        .ok_or(WasmGcError::Validation(
            "Request.headersLoad effect not registered".into(),
        ))?;
    let response_text_fn = fn_map
        .effects
        .get("Response.text")
        .copied()
        .ok_or(WasmGcError::Validation("Response.text effect not registered".into()))?;
    let response_set_header_fn = fn_map
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
/// `tools/edge-gc/`) can't directly allocate or read engine-managed
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

fn emit_bridge_bodies(
    codes: &mut CodeSection,
    registry: &TypeRegistry,
) -> Result<(), WasmGcError> {
    let s_idx = registry
        .string_array_type_idx
        .expect("bridge bodies emitted only when string slot exists");
    // from_lm(len) -> string. Allocate `(array i8)` of `len`, copy
    // bytes from LM[0..len] via a loop of `i32.load8_u` + `array.set`.
    //
    // Locals: 1 = arr, 2 = i.
    let mut from_lm = wasm_encoder::Function::new([
        (
            1,
            ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(s_idx),
            }),
        ),
        (1, ValType::I32),
    ]);
    from_lm.instruction(&Instruction::LocalGet(0));
    from_lm.instruction(&Instruction::ArrayNewDefault(s_idx));
    from_lm.instruction(&Instruction::LocalSet(1));
    from_lm.instruction(&Instruction::I32Const(0));
    from_lm.instruction(&Instruction::LocalSet(2));
    from_lm.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    from_lm.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i >= len break
    from_lm.instruction(&Instruction::LocalGet(2));
    from_lm.instruction(&Instruction::LocalGet(0));
    from_lm.instruction(&Instruction::I32GeU);
    from_lm.instruction(&Instruction::BrIf(1));
    // arr[i] = i32.load8_u(memory[i])
    from_lm.instruction(&Instruction::LocalGet(1));
    from_lm.instruction(&Instruction::LocalGet(2));
    from_lm.instruction(&Instruction::LocalGet(2));
    from_lm.instruction(&Instruction::I32Load8U(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    from_lm.instruction(&Instruction::ArraySet(s_idx));
    // i++
    from_lm.instruction(&Instruction::LocalGet(2));
    from_lm.instruction(&Instruction::I32Const(1));
    from_lm.instruction(&Instruction::I32Add);
    from_lm.instruction(&Instruction::LocalSet(2));
    from_lm.instruction(&Instruction::Br(0));
    from_lm.instruction(&Instruction::End); // loop
    from_lm.instruction(&Instruction::End); // block
    from_lm.instruction(&Instruction::LocalGet(1));
    from_lm.instruction(&Instruction::End);
    codes.function(&from_lm);

    // to_lm(s) -> i32 (= s.len). Auto-grow memory if `s.len`
    // exceeds the current LM capacity, then loop-write bytes to
    // LM[0..s.len].
    //
    // Locals: 1 = len, 2 = i, 3 = needed_pages, 4 = current_pages.
    let mut to_lm = wasm_encoder::Function::new([(4, ValType::I32)]);
    // len = arr.len
    to_lm.instruction(&Instruction::LocalGet(0));
    to_lm.instruction(&Instruction::ArrayLen);
    to_lm.instruction(&Instruction::LocalSet(1));
    // needed_pages = (len + 65535) >> 16
    to_lm.instruction(&Instruction::LocalGet(1));
    to_lm.instruction(&Instruction::I32Const(65535));
    to_lm.instruction(&Instruction::I32Add);
    to_lm.instruction(&Instruction::I32Const(16));
    to_lm.instruction(&Instruction::I32ShrU);
    to_lm.instruction(&Instruction::LocalSet(3));
    // current_pages = memory.size
    to_lm.instruction(&Instruction::MemorySize(0));
    to_lm.instruction(&Instruction::LocalSet(4));
    // if needed_pages > current_pages: memory.grow(needed - current)
    to_lm.instruction(&Instruction::LocalGet(3));
    to_lm.instruction(&Instruction::LocalGet(4));
    to_lm.instruction(&Instruction::I32GtU);
    to_lm.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
    to_lm.instruction(&Instruction::LocalGet(3));
    to_lm.instruction(&Instruction::LocalGet(4));
    to_lm.instruction(&Instruction::I32Sub);
    to_lm.instruction(&Instruction::MemoryGrow(0));
    to_lm.instruction(&Instruction::Drop);
    to_lm.instruction(&Instruction::End);
    // i = 0
    to_lm.instruction(&Instruction::I32Const(0));
    to_lm.instruction(&Instruction::LocalSet(2));
    to_lm.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
    to_lm.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
    // if i >= len break
    to_lm.instruction(&Instruction::LocalGet(2));
    to_lm.instruction(&Instruction::LocalGet(1));
    to_lm.instruction(&Instruction::I32GeU);
    to_lm.instruction(&Instruction::BrIf(1));
    // memory[i] = arr[i]
    to_lm.instruction(&Instruction::LocalGet(2));
    to_lm.instruction(&Instruction::LocalGet(0));
    to_lm.instruction(&Instruction::LocalGet(2));
    to_lm.instruction(&Instruction::ArrayGetU(s_idx));
    to_lm.instruction(&Instruction::I32Store8(wasm_encoder::MemArg {
        offset: 0,
        align: 0,
        memory_index: 0,
    }));
    // i++
    to_lm.instruction(&Instruction::LocalGet(2));
    to_lm.instruction(&Instruction::I32Const(1));
    to_lm.instruction(&Instruction::I32Add);
    to_lm.instruction(&Instruction::LocalSet(2));
    to_lm.instruction(&Instruction::Br(0));
    to_lm.instruction(&Instruction::End); // loop
    to_lm.instruction(&Instruction::End); // block
    to_lm.instruction(&Instruction::LocalGet(1));
    to_lm.instruction(&Instruction::End);
    codes.function(&to_lm);

    // pages() -> i32 (= memory.size)
    let mut pages = wasm_encoder::Function::new([]);
    pages.instruction(&Instruction::MemorySize(0));
    pages.instruction(&Instruction::End);
    codes.function(&pages);

    // grow(pages) -> i32 (= memory.grow result; -1 on fail)
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
