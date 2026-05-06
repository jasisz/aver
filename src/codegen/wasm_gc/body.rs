//! Expression / statement → wasm-gc instructions.
//!
//! Lowering rules (snapshot — full coverage of the bench scenarios and
//! the game suite):
//!
//! - `Literal(Int|Float|Bool|Unit|Str)` → primitive consts / array.new_data.
//! - `Resolved { slot }` → `local.get slot`. Resolver assigns slots in
//!   declaration order (params first, bindings next), which matches
//!   wasm's local-indexing convention 1:1, so no remapping needed.
//! - `BinOp(Add|Sub|Mul|Div, Int, Int)` → `i64.{add,sub,mul,div_s}`,
//!   F64 variants for Float.
//! - `BinOp(<comparison>, ..)` → typed `eq/ne/lt/gt/le/ge` per operand
//!   wasm type.
//! - `FnCall(Ident, args)` → `call $idx` after pushing args left-to-right.
//! - `Stmt::Binding` → emit value, then `local.set slot` where slot
//!   is the next-available counter (same convention as resolver).
//!
//! Step 3 of the 0.16 typed-ABI refactor deleted the parallel ad-hoc
//! inference layer that lived in `body/infer.rs` + `body/slots.rs`.
//! Every emit/slot site now reads `Spanned::ty()` directly via the
//! readers in `body/infer.rs` (`aver_type_of`, `aver_type_str_of`,
//! `wasm_type_of`). Missing `ty()` panics — that's a typecheck or
//! synthesizer bug, not a recoverable codegen condition.

use std::collections::{HashMap, HashSet};

use wasm_encoder::{Function, Instruction, ValType};

use super::WasmGcError;
use super::types::TypeRegistry;

use crate::ast::{FnBody, FnDef, Stmt};
use crate::ir::CallLowerCtx;

mod builtins;
mod emit;
pub(super) mod eq_helpers;
pub(super) mod hash_helpers;
mod infer;
mod slots;

use emit::emit_expr;
use infer::aver_type_str_of;
use slots::{SlotTable, count_value_params};

/// Maps fn name → wasm fn index + return type. Built once per module.
pub(super) struct FnMap {
    pub(super) by_name: std::collections::HashMap<String, FnEntry>,
    /// Dotted builtin name → wasm fn index. Populated by
    /// `module::emit_module` from the `BuiltinRegistry` so call
    /// sites can `call $builtin_idx` for `Int.toString` etc.
    pub(super) builtins: std::collections::HashMap<String, u32>,
    /// Dotted effect name → wasm fn index (host import). Populated
    /// from `EffectRegistry`. Imports occupy fn idx 0..K so these
    /// indices are always small.
    pub(super) effects: std::collections::HashMap<String, u32>,
    /// Per-instantiation `Map<K, V>` helpers (empty / set / get / len).
    /// Key is the canonical `Map<K,V>` Aver string. Body emit looks
    /// the canonical up by inferring the type of the map argument.
    pub(super) map_helpers: std::collections::HashMap<String, super::maps::MapKVHelpers>,
    /// Per-`List<T>` helpers (len / reverse). Key = canonical Aver
    /// string `List<T>`.
    pub(super) list_ops: std::collections::HashMap<String, super::lists::ListOps>,
    /// Per-`List<T>` `Vector.fromList` helper (paired with the
    /// matching `Vector<T>` registered in the type registry).
    pub(super) vfl_ops: std::collections::HashMap<String, super::lists::VectorFromListOps>,
    /// Per-`Tuple<A,B>` `List.zip` helper. Registered when the
    /// surface code calls `List.zip` and all three lists exist
    /// in the registry.
    pub(super) zip_ops: std::collections::HashMap<String, u32>,
    /// Singleton `String.split` / `String.join` helpers (T=String).
    /// Registered when the surface code calls either.
    pub(super) string_split_ops: Option<super::lists::StringSplitOps>,
    /// Per-(record/sum) `__eq_<TypeName>` helpers used by `BinOp::Eq`
    /// / `BinOp::Neq` over nominal types. Key = bare type name.
    pub(super) eq_helpers: std::collections::HashMap<String, u32>,
    /// Per-type `__hash_<X>` helpers used by record/sum/carrier
    /// hash bodies for non-primitive field hashing. Symmetric to
    /// `eq_helpers`. Key = canonical (bare for record/sum,
    /// whitespace-free instantiation for carriers).
    pub(super) hash_helpers: std::collections::HashMap<String, u32>,
}

impl FnMap {
    /// Lookup a `List<T>` helper triple, falling back to the bare-name
    /// form when the canonical carries a module qualifier (`List<Mod.X>`
    /// → `List<X>`). Multi-module flatten can leave inner type strings
    /// in either form depending on whether the call site originated in
    /// the entry or a dep module.
    pub(super) fn list_ops_lookup(&self, canonical: &str) -> Option<&super::lists::ListOps> {
        if let Some(o) = self.list_ops.get(canonical) {
            return Some(o);
        }
        let bare = super::types::strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.list_ops.get(&bare)
        } else {
            None
        }
    }

    pub(super) fn map_helpers_lookup(&self, canonical: &str) -> Option<&super::maps::MapKVHelpers> {
        if let Some(o) = self.map_helpers.get(canonical) {
            return Some(o);
        }
        let bare = super::types::strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.map_helpers.get(&bare)
        } else {
            None
        }
    }

    pub(super) fn vfl_ops_lookup(
        &self,
        canonical: &str,
    ) -> Option<&super::lists::VectorFromListOps> {
        if let Some(o) = self.vfl_ops.get(canonical) {
            return Some(o);
        }
        let bare = super::types::strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.vfl_ops.get(&bare)
        } else {
            None
        }
    }

    pub(super) fn zip_ops_lookup(&self, canonical: &str) -> Option<u32> {
        if let Some(&o) = self.zip_ops.get(canonical) {
            return Some(o);
        }
        let bare = super::types::strip_inner_dotted_prefixes(canonical);
        if bare != canonical {
            self.zip_ops.get(&bare).copied()
        } else {
            None
        }
    }
}

pub(super) struct FnEntry {
    pub(super) wasm_idx: u32,
    /// Kept for symmetry with the other backends' fn-table entries —
    /// wasm-gc emit reads return type from `fd.return_type` directly
    /// today, but the slot stays so the JSON-shaped `FnMap` consumed
    /// by `--explain-passes` retains the field.
    #[allow(dead_code)]
    pub(super) return_type: String,
}

/// Lower the body of `fd` into the supplied wasm `Function` builder.
/// Returns the list of *extra* locals (beyond params) needed for the
/// fn signature; caller passes these to `Function::new`.
///
/// `self_wasm_idx` is the current fn's own wasm index — used for
/// emitting `return_call $self` on `Expr::TailCall` to the same fn.
/// Mutual-TCO across SCC members goes through a `return_call_indirect`
/// table; that wiring lives in module.rs.
pub(super) fn emit_fn_body(
    func: &mut Function,
    fd: &FnDef,
    fn_map: &FnMap,
    self_wasm_idx: u32,
    registry: &TypeRegistry,
    effect_idx_lookup: &HashMap<String, u32>,
    caller_fn_collector: &std::cell::RefCell<CallerFnCollector>,
) -> Result<Vec<ValType>, WasmGcError> {
    let slots = SlotTable::build_for_fn(fd, registry, fn_map)?;
    let FnBody::Block(stmts) = fd.body.as_ref();
    let last_idx = stmts.len().saturating_sub(1);

    // Precollect every `let`-bound name so `CallLowerCtx::is_local_value`
    // can recognise locals without a parallel type table — the wasm-gc
    // backend's IR shape recognition (`classify_leaf_op` /
    // `classify_call_plan`) only needs the name, not the type.
    let mut binding_names: HashSet<String> = HashSet::new();
    fn collect_names(stmts: &[Stmt], out: &mut HashSet<String>) {
        for s in stmts {
            if let Stmt::Binding(name, _, _) = s {
                out.insert(name.clone());
            }
        }
    }
    collect_names(stmts, &mut binding_names);

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: fd.name.as_str(),
        return_type: fd.return_type.as_str(),
        registry,
        resolution: fd.resolution.as_ref(),
        params: &fd.params,
        binding_names: &binding_names,
        effect_idx_lookup,
        caller_fn_collector,
    };

    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == last_idx;
        match stmt {
            Stmt::Binding(name, _annot, expr) => {
                emit_expr(func, expr, &slots, &ctx)?;
                let produces_value = aver_type_str_of(expr).trim() != "Unit";
                if name == "_" {
                    // `_ = expr` — sequence-only binding. Drop the
                    // value (if any) and move on; the resolver
                    // doesn't allocate a slot for `_`.
                    if produces_value {
                        func.instruction(&Instruction::Drop);
                    }
                    continue;
                }
                let slot = ctx
                    .self_local_slot(name)
                    .ok_or(WasmGcError::Validation(format!(
                        "binding `{name}` has no resolver slot"
                    )))?;
                // Unit expressions push nothing — there's nothing to
                // stash, and the slot itself is an i32 placeholder
                // (kept around to preserve resolver slot indices).
                if produces_value && (slot as usize) < slots.by_slot.len() {
                    func.instruction(&Instruction::LocalSet(slot));
                }
            }
            Stmt::Expr(spanned) => {
                emit_expr(func, spanned, &slots, &ctx)?;
                // Whether the expression leaves a value on the stack
                // is decided structurally (typecheck stamps `Unit` on
                // pure-effect expressions, every other shape pushes a
                // value). We avoid `aver_to_wasm` here so that a type
                // carrying an unresolved `Type::Var` (e.g. for a
                // generic call site the registry can't lower without
                // context) doesn't poison the trailing-drop decision.
                let produces_value = aver_type_str_of(spanned).trim() != "Unit";
                if !is_last && produces_value {
                    func.instruction(&Instruction::Drop);
                }
                if is_last {
                    if fd.return_type.trim() == "Unit" && produces_value {
                        func.instruction(&Instruction::Drop);
                    } else if fd.return_type.trim() != "Unit" && !produces_value {
                        return Err(WasmGcError::Validation(format!(
                            "fn `{}` returns {} but trailing expression yields no value",
                            fd.name, fd.return_type
                        )));
                    }
                }
            }
        }
    }
    func.instruction(&Instruction::End);

    Ok(slots.extra_locals(count_value_params(&fd.params)))
}

/// Lazy-populated registry of caller_fn names actually emitted by the
/// codegen. Replaces the AST walker `fn_body_emits_effect_call` from
/// 0.16.2 — every site that calls `emit_caller_fn_idx` registers the
/// fn name here on demand, so the wasm output's caller-fn name table
/// contains exactly the fns whose bodies emit a caller_fn slot. Zero
/// false positives, zero rozjazdy walker↔codegen.
#[derive(Default)]
pub(super) struct CallerFnCollector {
    /// Aver fn name → idx in the exported caller-fn table (0..N).
    pub(super) idx_by_name: HashMap<String, u32>,
    /// Insertion order — the host walks `__caller_fn_name(0..N)` and
    /// the i-th entry must match `names[i]`.
    pub(super) names: Vec<String>,
}

impl CallerFnCollector {
    /// Get-or-insert a name, returning the i32 idx the codegen emits
    /// at the call site. New names land at the end of `names`.
    pub(super) fn register(&mut self, name: &str) -> u32 {
        if let Some(&i) = self.idx_by_name.get(name) {
            return i;
        }
        let i = self.names.len() as u32;
        self.names.push(name.to_string());
        self.idx_by_name.insert(name.to_string(), i);
        i
    }
}

/// Per-fn lowering context — read-only state every emit fn needs.
pub(super) struct EmitCtx<'a> {
    pub(super) fn_map: &'a FnMap,
    pub(super) self_wasm_idx: u32,
    pub(super) self_fn_name: &'a str,
    pub(super) return_type: &'a str,
    pub(super) registry: &'a TypeRegistry,
    /// Resolver's local-name → slot map for the current fn. `None`
    /// when the fn was emitted without `resolution` populated (the
    /// pipeline always populates it for production paths; tests may
    /// pre-resolve manually).
    pub(super) resolution: Option<&'a crate::ast::FnResolution>,
    /// Param name → declared aver type. Used by `CallLowerCtx` for
    /// local-name recognition; the typed-AST refactor (Step 3) made
    /// the *type* portion redundant for emit, but the param list is
    /// still the source of truth for "is this name a param?".
    pub(super) params: &'a [(String, String)],
    /// Set of `let`-bound local names (no type information attached —
    /// types come from `Spanned::ty()` at the use site). Powers
    /// `CallLowerCtx::is_local_value` for the shared IR-level shape
    /// recognition.
    pub(super) binding_names: &'a HashSet<String>,
    /// Effect canonical name → wasm fn idx. Body emit reaches into
    /// this for the structural-scope markers around `?!` /
    /// `!` (`__record_enter_group`, `__record_set_branch`,
    /// `__record_exit_group`). Empty when the program declares no
    /// independent product anywhere — discovery only registers the
    /// three host imports if it sees one.
    pub(super) effect_idx_lookup: &'a HashMap<String, u32>,
    /// Lazy collector for caller_fn names. `emit_caller_fn_idx`
    /// registers the current fn name here on each effect call site;
    /// the eventual `__caller_fn_name(idx)` body and data segments
    /// are emitted from `names` after every fn body has been
    /// produced.
    pub(super) caller_fn_collector: &'a std::cell::RefCell<CallerFnCollector>,
}

impl<'a> EmitCtx<'a> {
    /// Look up a local-name → wasm slot. Resolver slots are 1:1 with
    /// wasm local indices.
    pub(super) fn self_local_slot(&self, name: &str) -> Option<u32> {
        self.resolution
            .as_ref()
            .and_then(|r| r.local_slots.get(name).copied())
            .map(|s| s as u32)
    }
}

/// `CallLowerCtx` impl so the shared IR-level shape recognition
/// (`classify_leaf_op`, `classify_call_plan`) can be reused here
/// instead of each backend re-implementing the same patterns. Wasm-gc
/// is single-module today so module resolution returns None; the
/// other two predicates fall out of the registry + binding/param
/// tables we already maintain.
impl<'a> CallLowerCtx for EmitCtx<'a> {
    fn is_local_value(&self, name: &str) -> bool {
        self.params.iter().any(|(n, _)| n == name) || self.binding_names.contains(name)
    }

    fn is_user_type(&self, name: &str) -> bool {
        self.registry.records.contains_key(name)
            || self.registry.variants.contains_key(name)
            || self
                .registry
                .variants
                .values()
                .flat_map(|vs| vs.iter())
                .any(|info| info.parent == name)
    }

    fn resolve_module_call<'b>(&self, _dotted: &'b str) -> Option<(&'b str, &'b str)> {
        None
    }
}
