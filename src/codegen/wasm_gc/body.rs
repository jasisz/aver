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

use crate::ir::CallLowerCtx;
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolvedExpr, ResolvedFnBody, ResolvedFnDef, ResolvedStmt};
use crate::types::Type;

mod builtins;
mod builtins_wasip2;
mod emit;
pub(super) mod eq_helpers;
mod from_mir;
pub(super) mod hash_helpers;
mod infer;
mod slots;

use emit::emit_expr;
pub(super) use from_mir::emit_fn_body_via_mir;
pub use from_mir::{CoverageReport, coverage_report};
use infer::aver_type_str_of;
use slots::{SlotTable, count_value_params};

/// Maps fn identity → wasm fn index + return type. Built once per
/// module in `module::emit_module_with`. PR 9.3c keyed dispatch by
/// stable [`crate::ir::FnId`] (mirror of the FnKey-based
/// `CodegenContext::resolve_fn_def` from PR 9.3a); pre-9.3c this
/// field was `by_name: HashMap<String, FnEntry>`, which happened to
/// work post-flatten only because every dep fn carried a module
/// prefix in its name.
pub(super) struct FnMap {
    /// FnId → wasm fn entry. `Call(Fn(id))` and the
    /// `ResolvedExpr::TailCall { target }` dispatch read this
    /// directly.
    pub(super) by_id: std::collections::HashMap<crate::ir::FnId, FnEntry>,
    /// Dotted builtin name → wasm fn index. Populated by
    /// `module::emit_module` from the `BuiltinRegistry` so call
    /// sites can `call $builtin_idx` for `String.fromInt` etc.
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

#[derive(Clone)]
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
#[allow(clippy::too_many_arguments)]
pub(super) fn emit_fn_body(
    func: &mut Function,
    rfd: &ResolvedFnDef,
    fn_map: &FnMap,
    self_wasm_idx: u32,
    registry: &TypeRegistry,
    symbol_table: &SymbolTable,
    effect_idx_lookup: &HashMap<String, u32>,
    caller_fn_collector: &std::cell::RefCell<CallerFnCollector>,
    wasip2_lowering: Option<&Wasip2Lowering>,
) -> Result<Vec<ValType>, WasmGcError> {
    let slots = SlotTable::build_for_fn(rfd, registry, fn_map)?;
    let ResolvedFnBody::Block(stmts) = rfd.body.as_ref();
    let last_idx = stmts.len().saturating_sub(1);

    // Source-shape canonical of the enclosing fn's return type. Many
    // emit helpers (Option.None hint, ErrorProp enclosing fixup, list
    // empty literal fallback) want the string form once; compute it
    // here so the per-emit-site reads stay a cheap `&str`.
    let return_type_str = rfd.return_type.display();

    // Precollect every `let`-bound name so `CallLowerCtx::is_local_value`
    // can recognise locals without a parallel type table — the wasm-gc
    // backend's IR shape recognition (`classify_leaf_op` /
    // `classify_call_plan`) only needs the name, not the type.
    let mut binding_names: HashSet<String> = HashSet::new();
    fn collect_names(stmts: &[ResolvedStmt], out: &mut HashSet<String>) {
        for s in stmts {
            if let ResolvedStmt::Binding { name, .. } = s {
                out.insert(name.clone());
            }
        }
    }
    collect_names(stmts, &mut binding_names);

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: rfd.name.as_str(),
        return_type: &return_type_str,
        registry,
        symbol_table,
        resolution: rfd.resolution.as_ref(),
        // HIR path: alias facts come straight from the resolver table.
        aliased_slots: rfd
            .resolution
            .as_ref()
            .map(|r| r.aliased_slots.as_slice())
            .unwrap_or(&[]),
        params: &rfd.params,
        binding_names: &binding_names,
        effect_idx_lookup,
        caller_fn_collector,
        wasip2_lowering,
        mir_builtins: None,
    };

    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == last_idx;
        match stmt {
            ResolvedStmt::Binding { name, value, .. } => {
                emit_expr(func, value, &slots, &ctx)?;
                let produces_value = aver_type_str_of(value).trim() != "Unit";
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
            ResolvedStmt::Expr(spanned) => {
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
                    if return_type_str.trim() == "Unit" && produces_value {
                        func.instruction(&Instruction::Drop);
                    } else if return_type_str.trim() != "Unit" && !produces_value {
                        return Err(WasmGcError::Validation(format!(
                            "fn `{}` returns {} but trailing expression yields no value",
                            rfd.name, return_type_str
                        )));
                    }
                }
            }
        }
    }
    func.instruction(&Instruction::End);

    Ok(slots.extra_locals(count_value_params(&rfd.params)))
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
    /// Shared resolver symbol table — used by emit-site dispatch to
    /// turn a `ResolvedCallee::Fn(FnId)` / `ResolvedCtor::User { type_id, .. }`
    /// back into the canonical name the wasm fn-map / variant registry
    /// is keyed by. Phase E PR 9.1 threaded this in so the migration
    /// could read identity off the resolved AST directly instead of
    /// re-deriving from source strings.
    pub(super) symbol_table: &'a SymbolTable,
    /// Resolver's local-name → slot map for the current fn. `None`
    /// when the fn was emitted without `resolution` populated (the
    /// pipeline always populates it for production paths; tests may
    /// pre-resolve manually).
    pub(super) resolution: Option<&'a crate::ast::FnResolution>,
    /// Per-slot alias-proneness for the current fn, indexed by slot.
    /// On the `ResolvedExpr` path this mirrors `resolution.aliased_slots`;
    /// on the MIR path it is sourced from `MirFn::aliased_slots`, so the
    /// owned-mutate fast path reads its gate off MIR rather than reaching
    /// back into the AST `FnResolution` side-channel. `is_aliased_slot`
    /// reads this; an out-of-range slot is `false` (not aliased → fast
    /// path sound).
    pub(super) aliased_slots: &'a [bool],
    /// Param name → resolved aver type. Used by `CallLowerCtx` for
    /// local-name recognition; the typed-AST refactor (Step 3) made
    /// the *type* portion redundant for emit, but the param list is
    /// still the source of truth for "is this name a param?".
    pub(super) params: &'a [(String, Type)],
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
    /// `Some(...)` when the wasm-gc emitter was invoked under
    /// `TargetMode::Wasip2` AND the program registers at least one
    /// of `Console.print` / `Console.error` / `Console.warn`. Carries
    /// the fn / global / helper indices the call-site lowering needs
    /// to emit canonical-ABI imports of `wasi:cli/{stdout,stderr}`
    /// and `wasi:io/streams.[method]output-stream.blocking-write-and-
    /// flush`. Phase 1.2b1.5.
    pub(super) wasip2_lowering: Option<&'a Wasip2Lowering>,
    /// Interned built-in fn names from the
    /// [`crate::ir::mir::MirProgram`] driving the MIR body emitter, so
    /// `MirCallee::Builtin(BuiltinId)` resolves to its dotted name
    /// (`program.builtins[id]`). `None` on the `ResolvedExpr` emit
    /// path, which never reads it (it carries the dotted name inline).
    pub(super) mir_builtins: Option<&'a [String]>,
}

/// Concrete fn / global / helper indices the wasip2 call-site
/// lowering reads. Built once per module in `module.rs` after the
/// import section / globals section / bridge fn indices have been
/// allocated, then handed to every `emit_fn_body` call as a
/// borrowed reference. Each `Option<...>` field is populated when
/// at least one effect that needs it is registered.
pub(super) struct Wasip2Lowering {
    // ── Phase 1.2b1.5: Console.{print, error, warn} ─────────────
    /// `wasi:cli/stdout.get-stdout` imported wasm fn idx.
    pub(super) get_stdout_fn_idx: Option<u32>,
    /// `wasi:cli/stderr.get-stderr` imported wasm fn idx.
    pub(super) get_stderr_fn_idx: Option<u32>,
    /// `wasi:io/streams.[method]output-stream.blocking-write-and-flush`
    /// imported wasm fn idx. `Some(...)` when any `Console.*` is
    /// registered; `None` otherwise.
    pub(super) blocking_write_fn_idx: Option<u32>,
    /// Mutable i32 global caching the stdout `output-stream` resource
    /// handle. Initial value `-1` ("not yet resolved").
    pub(super) stdout_handle_global: Option<u32>,
    /// Same shape for stderr.
    pub(super) stderr_handle_global: Option<u32>,
    /// `__rt_string_to_lm` helper wasm fn idx. Reused from the
    /// existing JS-bridge helper machinery — on wasip2 it stays
    /// internal (not exported) and copies utf-8 bytes from the
    /// Aver `(ref null $string)` to LM[0..len]. `Some(...)` when
    /// any string-marshalling effect (i.e., `Console.*`) is wired.
    pub(super) str_to_lm_fn_idx: Option<u32>,
    /// `__rt_println_to_lm` helper wasm fn idx. Same shape as
    /// `str_to_lm_fn_idx` but also writes `'\n'` at LM[len] before
    /// returning `len + 1`. Allocated alongside the JS-bridge
    /// helpers when any string-marshalling effect is wired; used
    /// only by `Console.print` / `Console.error` / `Console.warn`
    /// on `--target wasip2` to honour the VM and AverBridge
    /// `Console.* == println!` semantic at the bridge layer.
    pub(super) println_to_lm_fn_idx: Option<u32>,

    // ── Phase 1.4: Time.unixMs / Random.{int, float} ───────────
    /// `wasi:clocks/wall-clock.now` imported wasm fn idx.
    /// `Some(...)` when `Time.unixMs` is registered.
    pub(super) clocks_now_fn_idx: Option<u32>,
    /// `wasi:random/random.get-random-u64` imported wasm fn idx.
    /// `Some(...)` when at least one of `Random.{int, float}` is
    /// registered (the same import drives both).
    pub(super) random_u64_fn_idx: Option<u32>,

    // ── Phase 1.3.2: Args.get + shared canonical-ABI decoders ──
    /// `wasi:cli/environment.get-arguments` imported wasm fn idx.
    /// `Some(...)` when `Args.get` is registered.
    pub(super) get_arguments_fn_idx: Option<u32>,
    /// `cabi_realloc` exported wasm fn idx — the bump-allocator
    /// helper from Phase 1.3.1. Required by every list-returning
    /// canonical-ABI import (host calls it to allocate the list
    /// payload + per-element bytes in guest memory). `Some(...)`
    /// when any wasip2 import is registered.
    pub(super) cabi_realloc_fn_idx: Option<u32>,
    /// `__rt_canonical_decode_list_string(retptr) -> List<String>`
    /// helper wasm fn idx. Walks the canonical-ABI lowered form
    /// `(list_ptr i32, list_len i32)` at retptr → Aver
    /// `List<String>` (cons cells + GC `(array i8)` strings).
    /// Shared across every list-returning effect: `Args.get`
    /// today, `Disk.listDir` (Phase 1.5), more later. Emitted
    /// once per module when any consumer is registered.
    pub(super) decode_list_string_fn_idx: Option<u32>,

    // ── Phase 1.3.3: Env.get ──────────────────────────────────
    /// `wasi:cli/environment.get-environment` imported wasm fn idx.
    /// `Some(...)` when `Env.get` is registered.
    pub(super) get_environment_fn_idx: Option<u32>,
    /// `__rt_canonical_env_lookup(retptr, key_ptr, key_len) ->
    /// String` helper wasm fn idx. Walks the canonical-ABI
    /// lowered `list<tuple<string, string>>` at retptr,
    /// linear-searches for an entry whose key matches the
    /// caller-supplied LM byte range, and materialises the
    /// matching value as a fresh GC `(array i8)`. Returns an
    /// empty `(array i8)` when no match — preserves Aver
    /// `Env.get(name) -> String` semantics (no Option/Result).
    pub(super) env_get_lookup_fn_idx: Option<u32>,
    /// Phase 1.4b — `__rt_format_iso8601(secs i64, nanos i32) ->
    /// ref null $string` helper wasm fn idx. Pure-compute helper
    /// that turns the datetime returned by
    /// `wasi:clocks/wall-clock.now` into Aver's RFC3339-like
    /// `Time.now() -> String` value. Allocated whenever wasip2 +
    /// the clocks slot are wired (i.e. unconditionally with
    /// Time.unixMs); `wasm-opt -Oz` strips this when no source
    /// `Time.now` reaches the helper.
    pub(super) fmt_iso8601_fn_idx: Option<u32>,
    /// Phase 1.3.4 — `__rt_console_read_line() -> ref null
    /// $result_string_string` helper wasm fn idx. Loops 1-byte
    /// `wasi:io/streams.[method]input-stream.blocking-read` calls
    /// against the cached `wasi:cli/stdin.get-stdin` handle until
    /// `\n` or EOF, accumulates into a `cabi_realloc`-owned
    /// buffer, and returns `Result.Ok(line)` (or `Result.Err("EOF")`
    /// when the first read produces zero bytes).
    pub(super) console_read_line_fn_idx: Option<u32>,
    /// Phase 1.4c — `__rt_time_sleep(ms i64) -> ()` helper wasm
    /// fn idx. Wraps `wasi:clocks/monotonic-clock.subscribe-
    /// duration` + `wasi:io/poll.poll` + `[resource-drop]pollable`
    /// — the pollable is per-call, allocated and dropped inside
    /// the helper, so it never leaks to source-level Aver.
    pub(super) time_sleep_fn_idx: Option<u32>,
    /// Phase 1.5.1 — `__rt_disk_exists(path: ref string) -> i32`
    /// helper wasm fn idx. Lazy-fetches the first preopen
    /// descriptor (cached in the disk_preopen_handle global),
    /// marshals the path through `__rt_string_to_lm`, calls
    /// `wasi:filesystem/types.[method]descriptor.stat-at` and
    /// returns the boolean result tag (1 = exists, 0 = not).
    pub(super) disk_exists_fn_idx: Option<u32>,
    /// Phase 1.5.2 — `__rt_disk_read_text(path: ref string) ->
    /// ref null $result_string_string` helper wasm fn idx. Owns
    /// open-at + read-via-stream + blocking-read loop + per-call
    /// resource drops. Returns `Result.Ok(content)` on success,
    /// `Result.Err("…")` on any wasi failure (open / stream /
    /// read).
    pub(super) disk_read_text_fn_idx: Option<u32>,
    /// Phase 1.5.3 — `__rt_disk_write_text(path: ref string,
    /// content: ref string) -> ref null $result_unit_string`
    /// helper wasm fn idx. Owns open-at(create+truncate) +
    /// write-via-stream + blocking-write-and-flush + per-call
    /// resource drops.
    pub(super) disk_write_text_fn_idx: Option<u32>,
    /// Phase 1.5.5 — `__rt_disk_append_text(path, content) ->
    /// Result<Unit, String>`. Same body emitter as
    /// `__rt_disk_write_text` flipped to append mode (no
    /// truncate, append-via-stream instead of write-via-stream).
    pub(super) disk_append_text_fn_idx: Option<u32>,
    /// Phase 1.5.4 — single-call wasi ops sharing
    /// `emit_disk_simple_path_op` (preopen + path marshalling +
    /// 4-byte retptr + tag check). One fn each per Aver effect
    /// since the wasi op fn idx + Err message differ.
    pub(super) disk_delete_fn_idx: Option<u32>,
    pub(super) disk_delete_dir_fn_idx: Option<u32>,
    pub(super) disk_make_dir_fn_idx: Option<u32>,
    /// Phase 1.5.6 — `__rt_disk_list_dir(path: ref string) ->
    /// ref null $result_list_string_string` helper wasm fn idx.
    /// Owns open-at(directory) + read-directory + entry-iteration
    /// loop + drops; cons-builds a `List<String>` of entry names.
    pub(super) disk_list_dir_fn_idx: Option<u32>,
    /// Phase 2.0 — `__rt_http_get(url: ref string) -> ref
    /// Result<HttpResponse, String>` helper wasm fn idx. Owns
    /// the entire wasi:http/outgoing-handler.handle pipeline
    /// (URL parse, fields/request constructors, setters, handle,
    /// poll, future.get, status, consume, body.stream, drain,
    /// per-call resource drops, HttpResponse build). `Some(...)`
    /// when `Http.get` is registered.
    pub(super) http_get_fn_idx: Option<u32>,

    /// Phase 4.2.x (0.20) — `__rt_tcp_connect(host: ref string,
    /// port: i64) -> ref Result<Tcp.Connection, String>` helper
    /// wasm fn idx. `Some(...)` when `Tcp.connect` is registered.
    /// Full body shipped in 4.2.2d: lazy network init + DNS
    /// resolve loop + first-IPv4 + create-tcp-socket + start/
    /// finish-connect + pool-slot allocation + record build.
    pub(super) tcp_connect_fn_idx: Option<u32>,
    /// Phase 4.3 (0.20) — `__rt_tcp_close(conn: ref Tcp.Connection)
    /// -> ref Result<Unit, String>` helper wasm fn idx. Drops the
    /// per-slot streams + shuts down + drops the socket + marks
    /// the slot free. Idempotent on already-closed slots.
    pub(super) tcp_close_fn_idx: Option<u32>,
    /// Phase 4.4a (0.20) — `__rt_tcp_write_line(conn, line) ->
    /// ref Result<Unit, String>` helper wasm fn idx. Marshals
    /// `line + "\n"` into LM and chunked-writes through the
    /// slot's out-stream.
    pub(super) tcp_write_line_fn_idx: Option<u32>,
    /// Phase 4.4b (0.20) — `__rt_tcp_read_line(conn) -> ref
    /// Result<String, String>` helper wasm fn idx. Loops 1-byte
    /// blocking-read against slot.in_stream, terminates on '\n',
    /// EOF, or stream-error.
    pub(super) tcp_read_line_fn_idx: Option<u32>,
    /// Phase 4.5a (0.20) — `__rt_tcp_send(host, port, data) ->
    /// ref Result<String, String>` helper wasm fn idx. One-shot
    /// orchestrator: connect + writeLine + readLine + close.
    pub(super) tcp_send_fn_idx: Option<u32>,
    /// Phase 4.5b (0.20) — `__rt_tcp_ping(host, port) -> ref
    /// Result<Unit, String>` helper wasm fn idx. Light connect +
    /// close wrapper; no in-line 1s timeout for v1.
    pub(super) tcp_ping_fn_idx: Option<u32>,

    // ── Phase 4 (0.20) — TCP pool globals + GC type slots. ─────
    //
    // All five fields are write-only on Phase 4.1b; the connect /
    // close / write / read / send / ping emitters in Phase 4.2+
    // are the readers. `#[allow(dead_code)]` until then so the
    // wireup commit can land without a warning.
    /// `wasi:sockets/instance-network.instance-network` handle
    /// cache. `Some(...)` whenever the TCP slot for instance-network
    /// is registered (any Tcp.* effect that needs a connect path).
    /// -1 sentinel = "not yet fetched"; the helper calls
    /// `instance-network()` once on first use and stores the
    /// resulting handle here.
    #[allow(dead_code)]
    pub(super) network_handle_global: Option<u32>,
    /// `tcp_pool: ref null $tcp_pool` global. `Some(...)` whenever
    /// any Tcp.* effect is declared. Initialised to null; the
    /// connect helper lazy-allocates an `array.new_default $tcp_pool 256`
    /// on first call.
    #[allow(dead_code)]
    pub(super) tcp_pool_global: Option<u32>,
    /// `tcp_next_id: i32 = 0` global. Bumps after each successful
    /// `Tcp.connect`. Modulo `tcp_pool` capacity gives the slot
    /// index; the resulting `"tcp-{idx}"` string is stored in the
    /// returned `Tcp.Connection.id` field for cross-backend
    /// portability.
    #[allow(dead_code)]
    pub(super) tcp_next_id_global: Option<u32>,
    /// `$tcp_slot` wasm-gc type idx — the 4-field struct each pool
    /// entry stores. `Some(...)` when `tcp_pool_global` is.
    #[allow(dead_code)]
    pub(super) tcp_slot_type_idx: Option<u32>,
    /// `$tcp_pool` wasm-gc type idx — `(array (mut $tcp_slot))`.
    /// `Some(...)` when `tcp_pool_global` is.
    #[allow(dead_code)]
    pub(super) tcp_pool_type_idx: Option<u32>,
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

    /// Whether `slot` is flagged as alias-prone by `ir::alias`. Backends
    /// that want to skip clone-on-write must check this first; default
    /// `false` for slots outside the table (empty table, scratch
    /// slots) is the safe-but-fast answer (no aliasing suspicion →
    /// in-place is sound). The alias pass always runs in real builds,
    /// so an empty table only happens in test paths that pre-resolve
    /// manually. Reads the `aliased_slots` field, which is
    /// resolution-sourced on the HIR path and `MirFn`-sourced on the
    /// MIR path — identical bits, different home.
    pub(super) fn is_aliased_slot(&self, slot: u16) -> bool {
        self.aliased_slots
            .get(slot as usize)
            .copied()
            .unwrap_or(false)
    }

    /// True when `arg` is a `Resolved` slot whose binding is dead
    /// after this expression (`last_use=true`) AND not flagged
    /// alias-prone — the underlying engine array / struct is
    /// uniquely owned, so the alias-aware fast path is sound.
    /// Used by `Vector.set` (skips `array.copy` of the backing array)
    /// and `Map.set` (picks the `set_in_place` helper). Anonymous
    /// expressions land here as a transient stack value with no
    /// binding to alias against, which counts as uniquely owned.
    pub(super) fn arg_uniquely_owned(&self, arg: &crate::ast::Spanned<ResolvedExpr>) -> bool {
        match &arg.node {
            ResolvedExpr::Resolved { slot, last_use, .. } => {
                last_use.0 && !self.is_aliased_slot(*slot)
            }
            _ => true,
        }
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
