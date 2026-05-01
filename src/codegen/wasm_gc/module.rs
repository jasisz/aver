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

pub(super) fn emit_module(items: &[TopLevel]) -> Result<Vec<u8>, WasmGcError> {
    let registry = TypeRegistry::build(items);

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

    if fn_defs.is_empty() {
        return Err(WasmGcError::Validation(
            "module has no fn definitions".into(),
        ));
    }
    let main_idx = fn_defs
        .iter()
        .position(|fd| fd.name == "main")
        .ok_or_else(|| WasmGcError::Validation("module has no `main` fn".into()))?;

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
    module.section(&funcs);

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
    let fn_map = FnMap {
        by_name,
        builtins: builtin_idx_lookup,
        effects: effect_idx_lookup,
        map_helpers: map_helpers_lookup,
    };

    // ── Export section ─────────────────────────────────────────────
    let mut exports = ExportSection::new();
    exports.export("_start", ExportKind::Func, start_wasm_idx);
    for (i, fd) in fn_defs.iter().enumerate() {
        let wasm_idx = import_count + 1 + (i as u32);
        exports.export(&fd.name, ExportKind::Func, wasm_idx);
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

    // _start: call main, drop result if main returns a value.
    let main_idx_wasm = import_count + 1 + (main_idx as u32);
    let main_returns_value = !fn_defs[main_idx].return_type.trim().eq("Unit");
    let mut start = Function::new([]);
    start.instruction(&Instruction::Call(main_idx_wasm));
    if main_returns_value {
        start.instruction(&Instruction::Drop);
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
    map_helpers.emit_helper_bodies(&mut codes, &registry)?;

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
    // String comes first in the type section ONLY if the registry
    // allocated a slot AND the slot index says it should be at the
    // start. Actually `TypeRegistry::build` allocates the string slot
    // AFTER all records/variants — so emit user types in registry
    // declaration order; string sits at `user_type_count - 1`.
    // Walk records/variants first, then append the string array.
    // NOTE: even when a record / variant is a newtype (erased at the
    // wasm level), we still emit its struct type slot to keep the
    // type-index assignments stable with `TypeRegistry::build`. The
    // emit code paths skip `struct.new` / `struct.get` for newtype
    // names, so the slot is dead — `wasm-opt -Oz` strips it during
    // post-processing. Cost today is a few bytes of unused type
    // section per newtype.
    for item in items {
        match item {
            TopLevel::TypeDef(TypeDef::Product {
                name: _, fields, ..
            }) => {
                let st = record_struct_type(fields, registry)?;
                types.ty().struct_(st.fields.iter().copied());
            }
            TopLevel::TypeDef(TypeDef::Sum { variants, .. }) => {
                // Each variant constructor → its own struct type.
                // Phase 3a: no shared subtype hierarchy; each variant
                // stands alone, parent type is encoded as `(ref null eq)`
                // in user-facing slots.
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
                    types.ty().struct_(fields);
                }
            }
            _ => {}
        }
    }

    // String type: `(array (mut i8))`. Emitted before any `Vector<T>`
    // slot so a `Vector<String>` array element can reference `$string`
    // by an already-assigned smaller index. Mutable=true at the wasm
    // type level so helpers (Int.toString etc.) can `array.set` to
    // fill a freshly-allocated array; Aver-side immutability is a
    // surface-language guarantee — no user-level op exposes mutation.
    if registry.string_array_type_idx.is_some() {
        types
            .ty()
            .array(&wasm_encoder::StorageType::I8, true /* mutable */);
    }

    // Vector<T> instantiations — emit one `(array (mut T))` per
    // registered slot, in registry insertion order so `vector_type_idx`
    // matches the type-section position. Mutable=true so `Vector.set`
    // can `array.set` in place; Aver-side immutability is preserved
    // by API contract (no user-level ops mutate without returning a
    // fresh handle).
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
        types
            .ty()
            .array(&wasm_encoder::StorageType::Val(elem_val), true);
    }

    // Option<T> instantiations — `(struct (mut i32 tag) (mut T value))`.
    // Emitted after vectors so an `Option<Vector<T>>` chain can land
    // safely (Option struct sits at a higher index than the inner
    // Vector slot it references). Both fields mutable for symmetry
    // with constructors emitting `struct.new` directly.
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
        types.ty().struct_([
            wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(ValType::I32),
                mutable: true,
            },
            wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(elem_val),
                mutable: true,
            },
        ]);
    }

    // `Map<K, V>` — three wasm types per registered instantiation,
    // emitted in registry insertion order (keys array, values array,
    // map struct) so the struct's nested ref fields reference the
    // arrays by an already-assigned lower idx.
    for canonical in &registry.map_order {
        let (k_aver, v_aver) = super::types::parse_map_kv(canonical).ok_or(
            WasmGcError::Validation(format!(
                "registered map `{canonical}` has no parsable K, V"
            )),
        )?;
        let k_val = super::types::aver_to_wasm(k_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Map key type `{k_aver}` has no wasm representation"
            )),
        )?;
        let v_val = super::types::aver_to_wasm(v_aver, Some(registry))?.ok_or(
            WasmGcError::Validation(format!(
                "Map value type `{v_aver}` has no wasm representation"
            )),
        )?;
        types
            .ty()
            .array(&wasm_encoder::StorageType::Val(k_val), true);
        types
            .ty()
            .array(&wasm_encoder::StorageType::Val(v_val), true);
        let slots = registry
            .map_slots(canonical)
            .expect("just-registered map slots");
        let keys_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(slots.keys_array),
        });
        let values_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(slots.values_array),
        });
        types.ty().struct_([
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
        ]);
    }

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
        _ => {}
    }
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
