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
#[allow(dead_code)]
struct Wasip2Globals {
    /// Global idx of the bump-allocator cursor backing
    /// `cabi_realloc` — Phase 1.3.1. Always allocated when this
    /// struct is constructed (i.e., wasip2 imports active). Initial
    /// value `65536` (start of page 2): page 1 stays as the
    /// transient transport buffer for `__rt_string_to_lm` /
    /// `Console.*` writes; persistent `cabi_realloc` allocations
    /// grow upward from page 2.
    bump_alloc_ptr: u32,
    /// Global idx caching the `wasi:cli/stdout.get-stdout` resource
    /// handle, lazy-initialised on first use. `None` when the program
    /// does not register `Console.print` (the only effect that calls
    /// `get-stdout`). Phase 1.2b1.5 is the consumer.
    stdout_handle: Option<u32>,
    /// Same shape, for `wasi:cli/stderr.get-stderr`. Populated when
    /// `Console.error` or `Console.warn` is registered.
    stderr_handle: Option<u32>,
    /// Same shape, for `wasi:cli/stdin.get-stdin`. Populated when
    /// `Console.readLine` is registered. The resource is program-
    /// lifetime — wasmtime cleans up at component exit, so we never
    /// emit `[resource-drop]input-stream` for it.
    stdin_handle: Option<u32>,
    /// Phase 1.5.1 — caches the first `wasi:filesystem/preopens.
    /// get-directories` entry's descriptor handle. Populated when
    /// any `Disk.*` is registered. -1 sentinel = "not yet
    /// fetched" or "no preopens"; helpers retry fetch on -1.
    disk_preopen_handle: Option<u32>,
    /// Phase 4 (0.20) — caches the `wasi:sockets/instance-network.
    /// instance-network` handle for the program's lifetime. -1
    /// sentinel = "not yet fetched". Populated when any Tcp.*
    /// effect that needs a network capability is registered
    /// (`Tcp.connect`, `Tcp.send`, `Tcp.ping`). Never dropped —
    /// wasmtime releases at component exit.
    network_handle: Option<u32>,
    /// Phase 4 (0.20) — connection-pool array reference. Initialised
    /// to `ref.null $tcp_pool` and lazy-allocated to
    /// `array.new_default $tcp_pool 256` on the first call into the
    /// connect/send pipeline. Holds `(ref null $tcp_slot)` entries
    /// indexed by integer parsed out of `Tcp.Connection.id`
    /// (`"tcp-N"`). Populated whenever the TCP pool slot type
    /// (`tcp_pool_type_idx`) is allocated by `TypeRegistry`.
    tcp_pool: Option<u32>,
    /// Phase 4 (0.20) — monotonic counter for the next pool slot
    /// allocation. Starts at `0`, bumps after each `Tcp.connect`
    /// success. Wraps modulo 256 (pool capacity); a `Tcp.connect`
    /// past 256 live connections will reuse a stale slot today —
    /// good enough for v1, exhaustion handling lands as a follow-up
    /// once the pipeline is real.
    tcp_next_id: Option<u32>,
}
use super::body::hash_helpers::{HashHelperRegistry, HashKind};
use super::body::{FnEntry, FnMap, emit_fn_body};
use super::builtins::{BuiltinName, BuiltinRegistry};
use super::effects::{EffectName, EffectRegistry};
use super::maps::MapHelperRegistry;
use super::types::{TypeRegistry, param_types, record_struct_type, return_results};
use super::wasip2_helpers::{
    CabiReallocIndices, ConsoleReadLineIndices, DecodeListStringIndices, DiskExistsIndices,
    DiskListDirIndices, DiskReadTextIndices, DiskSimplePathOpIndices, DiskWriteTextIndices,
    EnvGetLookupIndices, FormatIso8601Indices, TimeSleepIndices, emit_cabi_realloc,
    emit_console_read_line, emit_decode_list_string, emit_disk_exists, emit_disk_list_dir,
    emit_disk_read_text, emit_disk_simple_path_op, emit_disk_write_text, emit_env_get_lookup,
    emit_format_iso8601, emit_time_sleep,
};
use super::wat_helper;
use crate::types::Type as AverType;

use crate::ast::{Expr, FnDef, Stmt, TopLevel, TypeDef};

pub(super) fn emit_module_with(
    items: &[TopLevel],
    handler_name: Option<&str>,
    target: super::TargetMode,
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
        } else if name.starts_with("Option<") && name.ends_with('>') {
            // Carrier element of List<Option<X>> / Vector<Option<X>>
            // / Map<Option<X>, _> — list/vec eq+hash bodies dispatch
            // each element via `Call(__eq_Option<X>)` which means
            // eq_helpers must hold the slot. Same logic for the hash
            // side. Inner type registration happens transitively.
            eq_helpers_registry.register_transitive(name, EqKind::OptionEq, &registry);
            hash_helpers_registry.register_transitive(name, HashKind::OptionHash, &registry);
        } else if name.starts_with("Result<") && name.ends_with('>') {
            eq_helpers_registry.register_transitive(name, EqKind::ResultEq, &registry);
            hash_helpers_registry.register_transitive(name, HashKind::ResultHash, &registry);
        } else if name.starts_with("Tuple<") && name.ends_with('>') {
            eq_helpers_registry.register_transitive(name, EqKind::TupleEq, &registry);
            hash_helpers_registry.register_transitive(name, HashKind::TupleHash, &registry);
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
    // `--handler X` on `--target wasm-gc` (AverBridge) — the
    // synthesised `aver_http_handle` wrapper reads `Request.*` and
    // dispatches `Response.text` / `Response.setHeader` via the JS
    // host's `aver/*` import surface, so register them up front. The
    // user handler may also touch `Http.*` / `Env.*`, which discovery
    // already picks up through `discover_builtins_in_fn`.
    //
    // `--target wasip2 --world wasi:http/proxy` uses the same
    // `handler_name` argument but takes a different codegen path:
    // the proxy wrapper decodes request fields from the host-
    // supplied `incoming-request` resource and writes the response
    // through `response-outparam.set`. No `aver/*` Request/Response
    // bridge there — pure canonical-ABI wasi:http imports.
    let proxy_mode = handler_name.is_some() && matches!(target, super::TargetMode::Wasip2);
    if handler_name.is_some() && matches!(target, super::TargetMode::AverBridge) {
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
    //
    //    Phase 1.2b1.2 — branch on `target`. AverBridge keeps the
    //    existing `aver/*` import shape (one wasm import per
    //    registered `EffectName`). Wasip2 substitutes a parallel
    //    `Wasip2ImportRegistry` whose slots speak Component-Model
    //    canonical-ABI names (`wasi:cli/stdout.get-stdout` etc.).
    //    For Phase 1.2b1.2 the wasip2 registry is empty unless the
    //    upstream effect-check let a Console.* effect through —
    //    which only happens once Phase 1.2b1.5 graduates the trio
    //    from `pending` to `wired`.
    let mut next_type_idx = registry.user_type_count;
    let mut wasip2_imports = super::wasip2_imports::Wasip2ImportRegistry::new();
    match target {
        super::TargetMode::AverBridge => {
            effect_registry.assign_slots(&mut next_type_idx);
            for name in effect_registry.iter() {
                let p = name.params(&registry)?;
                let r = name.results(&registry)?;
                types.ty().function(p, r);
            }
        }
        super::TargetMode::Wasip2 => {
            // Populate the wasip2 registry from each effect that
            // lowers on this target. The slot set per effect is:
            //   Console.print → CliGetStdout + OutputStreamBlockingWriteAndFlush
            //   Console.error → CliGetStderr + OutputStreamBlockingWriteAndFlush
            //   Console.warn  → CliGetStderr + OutputStreamBlockingWriteAndFlush
            // (warn → stderr matches the VM and wasm-gc target's
            // default semantics — `Console.warn` writes to fd 2.)
            use super::effects::EffectName;
            use super::wasip2_imports::Wasip2ImportSlot;
            for name in effect_registry.iter() {
                if !name.lowers_on_wasip2() {
                    continue;
                }
                match name {
                    EffectName::ConsolePrint => {
                        wasip2_imports.register(Wasip2ImportSlot::CliGetStdout);
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                    }
                    EffectName::ConsoleError | EffectName::ConsoleWarn => {
                        wasip2_imports.register(Wasip2ImportSlot::CliGetStderr);
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                    }
                    EffectName::TimeUnixMs | EffectName::TimeNow => {
                        wasip2_imports.register(Wasip2ImportSlot::ClocksWallClockNow);
                    }
                    EffectName::RandomInt | EffectName::RandomFloat => {
                        wasip2_imports.register(Wasip2ImportSlot::RandomGetRandomU64);
                    }
                    EffectName::ArgsGet => {
                        wasip2_imports.register(Wasip2ImportSlot::CliEnvironmentGetArguments);
                    }
                    EffectName::EnvGet => {
                        wasip2_imports.register(Wasip2ImportSlot::CliEnvironmentGetEnvironment);
                    }
                    EffectName::ConsoleReadLine => {
                        wasip2_imports.register(Wasip2ImportSlot::CliStdinGetStdin);
                        wasip2_imports.register(Wasip2ImportSlot::InputStreamBlockingRead);
                    }
                    EffectName::TimeSleep => {
                        wasip2_imports.register(Wasip2ImportSlot::ClocksMonotonicSubscribeDuration);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollPoll);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollResourceDropPollable);
                    }
                    EffectName::DiskExists => {
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesStatAt);
                    }
                    EffectName::DiskReadText => {
                        // Shares the preopens cache with Disk.exists.
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesOpenAt);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesReadViaStream);
                        wasip2_imports.register(Wasip2ImportSlot::InputStreamBlockingRead);
                        wasip2_imports
                            .register(Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor);
                        wasip2_imports.register(Wasip2ImportSlot::IoStreamsResourceDropInputStream);
                    }
                    EffectName::DiskWriteText => {
                        // Shares preopens / open-at / blocking-write-and-flush
                        // / drop-descriptor with earlier phases; adds
                        // write-via-stream and the output-stream drop.
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesOpenAt);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesWriteViaStream);
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                        wasip2_imports
                            .register(Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor);
                        wasip2_imports
                            .register(Wasip2ImportSlot::IoStreamsResourceDropOutputStream);
                    }
                    EffectName::DiskAppendText => {
                        // Same shape as writeText, but uses
                        // append-via-stream + open-flags=CREATE only.
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesOpenAt);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesAppendViaStream);
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                        wasip2_imports
                            .register(Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor);
                        wasip2_imports
                            .register(Wasip2ImportSlot::IoStreamsResourceDropOutputStream);
                    }
                    EffectName::DiskDelete => {
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesUnlinkFileAt);
                    }
                    EffectName::DiskDeleteDir => {
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesRemoveDirectoryAt);
                    }
                    EffectName::DiskMakeDir => {
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesCreateDirectoryAt);
                    }
                    EffectName::DiskListDir => {
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemPreopensGetDirectories);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesOpenAt);
                        wasip2_imports.register(Wasip2ImportSlot::FilesystemTypesReadDirectory);
                        wasip2_imports.register(
                            Wasip2ImportSlot::FilesystemTypesDirectoryEntryStreamReadDirectoryEntry,
                        );
                        wasip2_imports
                            .register(Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor);
                        wasip2_imports.register(
                            Wasip2ImportSlot::FilesystemTypesResourceDropDirectoryEntryStream,
                        );
                    }
                    EffectName::HttpGet
                    | EffectName::HttpHead
                    | EffectName::HttpDelete
                    | EffectName::HttpPost
                    | EffectName::HttpPut
                    | EffectName::HttpPatch => {
                        // GET/HEAD/DELETE all share the same wasi:http
                        // import set — only `set-method` differentiates
                        // the verb (handled inside `__rt_http_request`
                        // based on the method_tag i32 param).
                        //
                        // 16 new + 4 reused (Step D) + Step F+G's 4 +
                        // Step J's set-method = 24 new + 4 reused.
                        // Reused: `wasi:io/poll.poll` (Time.sleep
                        // pollable-wait), `[resource-drop]pollable`,
                        // `input-stream.blocking-read` (Disk.readText
                        // / Console.readLine), input-stream drop.
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesFieldsNew);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingRequestNew);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesOutgoingRequestSetScheme);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesOutgoingRequestSetAuthority);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesOutgoingRequestSetPathWithQuery);
                        wasip2_imports.register(Wasip2ImportSlot::HttpOutgoingHandlerHandle);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesFutureIncomingResponseSubscribe);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollPoll);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollResourceDropPollable);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesFutureIncomingResponseGet);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingResponseStatus);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingResponseConsume);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingBodyStream);
                        wasip2_imports.register(Wasip2ImportSlot::InputStreamBlockingRead);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingBodyFinish);
                        wasip2_imports.register(Wasip2ImportSlot::IoStreamsResourceDropInputStream);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesResourceDropOutgoingRequest);
                        wasip2_imports.register(
                            Wasip2ImportSlot::HttpTypesResourceDropFutureIncomingResponse,
                        );
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesResourceDropIncomingResponse);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesResourceDropFutureTrailers);
                        // Step F: drop incoming-body on error paths
                        // (between consume() and finish()).
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesResourceDropIncomingBody);
                        // Step G: surface real response headers via
                        // incoming-response.headers + fields.entries +
                        // [resource-drop]fields (child of response).
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingResponseHeaders);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesFieldsEntries);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesResourceDropFields);
                        // Step J: set-method for HEAD/DELETE (GET keeps
                        // constructor default — set-method call is
                        // gated by p_method != 0 inside the helper).
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesOutgoingRequestSetMethod);
                        // Step K: outgoing-body marshalling for
                        // POST/PUT/PATCH — request.body, body.write,
                        // chunked write via existing
                        // OutputStreamBlockingWriteAndFlush, body.finish,
                        // and fields.append for headers + Content-Type.
                        // outgoing-body resource-drop covers the error
                        // path between request.body() and body.finish().
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingRequestBody);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingBodyWrite);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingBodyFinish);
                        wasip2_imports.register(Wasip2ImportSlot::HttpTypesFieldsAppend);
                        wasip2_imports
                            .register(Wasip2ImportSlot::HttpTypesResourceDropOutgoingBody);
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                        wasip2_imports
                            .register(Wasip2ImportSlot::IoStreamsResourceDropOutputStream);
                    }
                    // ── Phase 4 / 0.20 "Pulse" — wasi:sockets/* ──────
                    //
                    // The Tcp.* surface shares one connect pipeline,
                    // one read loop, one write loop, plus the
                    // timeout-race for ping. Each effect registers
                    // exactly the slots its lowering needs; the union
                    // covers the whole choreography. Reuses from the
                    // HTTP path: input-stream/output-stream blocking
                    // read/write + drops, io/poll.poll + drop pollable,
                    // monotonic-clock subscribe-duration (ping only).
                    EffectName::TcpConnect => {
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress);
                        wasip2_imports.register(
                            Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
                        );
                        wasip2_imports.register(
                            Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
                        );
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpStartConnect);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpFinishConnect);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpSubscribe);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollPoll);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollResourceDropPollable);
                    }
                    EffectName::TcpWriteLine => {
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                    }
                    EffectName::TcpReadLine => {
                        wasip2_imports.register(Wasip2ImportSlot::InputStreamBlockingRead);
                    }
                    EffectName::TcpClose => {
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpShutdown);
                        wasip2_imports.register(Wasip2ImportSlot::IoStreamsResourceDropInputStream);
                        wasip2_imports
                            .register(Wasip2ImportSlot::IoStreamsResourceDropOutputStream);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket);
                    }
                    EffectName::TcpSend => {
                        // Same union as connect + write + read + close,
                        // since `__rt_tcp_send` inlines the full pipeline.
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress);
                        wasip2_imports.register(
                            Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
                        );
                        wasip2_imports.register(
                            Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
                        );
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpStartConnect);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpFinishConnect);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpSubscribe);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpShutdown);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollPoll);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollResourceDropPollable);
                        wasip2_imports
                            .register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                        wasip2_imports.register(Wasip2ImportSlot::InputStreamBlockingRead);
                        wasip2_imports
                            .register(Wasip2ImportSlot::IoStreamsResourceDropInputStream);
                        wasip2_imports
                            .register(Wasip2ImportSlot::IoStreamsResourceDropOutputStream);
                    }
                    EffectName::TcpPing => {
                        // Connect-with-timeout — racing the connect
                        // pollable against `subscribe-duration` via
                        // a two-element `poll`. Adds the monotonic
                        // clock slot on top of the connect set.
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress);
                        wasip2_imports.register(
                            Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
                        );
                        wasip2_imports.register(
                            Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
                        );
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpStartConnect);
                        wasip2_imports.register(Wasip2ImportSlot::SocketsTcpSubscribe);
                        wasip2_imports
                            .register(Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket);
                        wasip2_imports
                            .register(Wasip2ImportSlot::ClocksMonotonicSubscribeDuration);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollPoll);
                        wasip2_imports.register(Wasip2ImportSlot::IoPollResourceDropPollable);
                    }
                    _ => {} // unreachable; `lowers_on_wasip2` enumerates the wired set.
                }
            }
            // Phase 3 — `--world wasi:http/proxy` server slots. The
            // import set is independent of any effect a user fn
            // touches: the handler wrapper itself drives every
            // wasi:http/incoming-handler / response-outparam call.
            // Reuses six slots from the client path (fields entries,
            // body stream + finish + drops, input-stream / fields /
            // outgoing-body drops, blocking-read, blocking-write-and-
            // flush, output-stream drop) and the fields/outgoing-body
            // writer chain — so `Http.get` inside the user's handler
            // doesn't double-register them.
            if proxy_mode {
                use super::wasip2_imports::Wasip2ImportSlot;
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingRequestMethod);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingRequestPathWithQuery);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingRequestHeaders);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingRequestConsume);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesResourceDropIncomingRequest);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesFieldsEntries);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesResourceDropFields);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingBodyStream);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesIncomingBodyFinish);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesResourceDropIncomingBody);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesResourceDropFutureTrailers);
                wasip2_imports.register(Wasip2ImportSlot::InputStreamBlockingRead);
                wasip2_imports.register(Wasip2ImportSlot::IoStreamsResourceDropInputStream);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesFieldsNew);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesFieldsAppend);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingResponseNew);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingResponseSetStatusCode);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingResponseBody);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingBodyWrite);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesOutgoingBodyFinish);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesResourceDropOutgoingBody);
                wasip2_imports.register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
                wasip2_imports.register(Wasip2ImportSlot::IoStreamsResourceDropOutputStream);
                wasip2_imports.register(Wasip2ImportSlot::HttpTypesResponseOutparamSet);
            }
            wasip2_imports.assign_slots(&mut next_type_idx);
            for slot in wasip2_imports.iter() {
                let p = slot.params();
                let r = slot.results();
                types.ty().function(p, r);
            }
        }
    }

    // 3) Entry-point type. Three shapes drive different exports:
    //    - AverBridge: `_start: () -> ()` — the JS host calls
    //      `_start` for its side effects and discards any value
    //      `main` returns.
    //    - Wasip2 / CliCommand: `() -> i32` — canonical-ABI
    //      lowering of `wasi:cli/run.run`'s `result<_, _>` return
    //      (`0 == Ok`, `1 == Err`).
    //    - Wasip2 / HttpProxy (proxy_mode): `(req: i32, outparam:
    //      i32) -> ()` — `wasi:http/incoming-handler.handle`'s
    //      canonical-ABI signature. The body emit later in the
    //      code section walks the per-request choreography.
    let start_returns_i32 = matches!(target, super::TargetMode::Wasip2) && !proxy_mode;
    if proxy_mode {
        types.ty().function([ValType::I32, ValType::I32], []);
    } else if start_returns_i32 {
        types.ty().function([], [ValType::I32]);
    } else {
        types.ty().function([], []);
    }
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
    //
    //    `import_count` is the wasm-fn-idx offset every other
    //    function uses, so it must reflect whichever registry
    //    drove the import-type emission above (per `target`).
    let import_count: u32 = match target {
        super::TargetMode::AverBridge => effect_registry.import_count(),
        super::TargetMode::Wasip2 => wasip2_imports.import_count(),
    };
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
    let handler_wrapper: Option<HandlerWrapper> = if let Some(name) = handler_name
        && matches!(target, super::TargetMode::AverBridge)
    {
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

    // 8b) `cabi_realloc` — Phase 1.3.1. The Component Model
    //     canonical ABI requires a guest export named exactly
    //     `cabi_realloc(old_ptr i32, old_size i32, align i32,
    //     new_size i32) -> i32` whenever ANY imported function
    //     returns a list, string, or other host-allocated value.
    //     Phase 1.3.1 emits it scaffolding-style on every wasip2
    //     build that has imports active; the first real consumers
    //     (Args.get / Env.get / Console.readLine / Disk.readText)
    //     come in 1.3.2+. wit-component is happy to carry an
    //     unused export — host just never calls it.
    let cabi_realloc: Option<CabiReallocIndices> =
        if matches!(target, super::TargetMode::Wasip2) && wasip2_imports.import_count() > 0 {
            types.ty().function(
                [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
                [ValType::I32],
            );
            let realloc_type = next_type_idx;
            next_type_idx += 1;
            let realloc_fn = next_builtin_fn_idx;
            next_builtin_fn_idx += 1;
            Some(CabiReallocIndices {
                fn_type: realloc_type,
                fn_idx: realloc_fn,
            })
        } else {
            None
        };

    // 8c) `__rt_canonical_decode_list_string` — Phase 1.3.2.
    //     Shared helper that walks a canonical-ABI lowered
    //     `list<string>` retptr (`(list_ptr i32, list_len i32)`)
    //     into an Aver `List<String>` (cons cells of GC `(array
    //     i8)` strings). Emitted once per module when any list-
    //     returning effect that lowers via this shape is registered;
    //     today that's `Args.get` (more land in 1.3.3 / 1.5).
    //
    //     The fn signature uses the user module's `String` and
    //     `List<String>` engine-GC type indices, so allocation is
    //     gated on both being present in the registry. If the
    //     registry didn't carry them yet, the discovery walker
    //     would have failed earlier — defensive `Option` here just
    //     keeps the helper out of programs that never reach
    //     Args.get.
    let decode_list_string: Option<DecodeListStringIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::CliEnvironmentGetArguments)
            .is_some()
        && let (Some(string_idx), Some(list_string_idx)) = (
            registry.string_array_type_idx,
            registry.list_type_idx("List<String>"),
        ) {
        let list_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(list_string_idx),
        });
        types.ty().function([ValType::I32], [list_ref]);
        let decoder_type = next_type_idx;
        next_type_idx += 1;
        let decoder_fn = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(DecodeListStringIndices {
            fn_type: decoder_type,
            fn_idx: decoder_fn,
            string_type_idx: string_idx,
            list_string_type_idx: list_string_idx,
        })
    } else {
        None
    };

    // 8d) `__rt_canonical_env_lookup` — Phase 1.3.3.
    //     Linear-search lookup over the canonical-ABI lowered
    //     `list<tuple<string, string>>` retptr that
    //     `wasi:cli/environment.get-environment` writes to.
    //     Signature: `(retptr i32, key_ptr i32, key_len i32) ->
    //     ref null $string`. Returns the matching value as a
    //     fresh GC `(array i8)`, or an empty array when no key
    //     matches — preserves Aver's `Env.get(name) -> String`
    //     no-Option semantics.
    // Phase 1.4b — `__rt_format_iso8601(secs i64, nanos i32) ->
    // Phase 1.3.4 — `__rt_console_read_line() ->
    // Result<String, String>` body. Caches `wasi:cli/stdin.get-stdin`
    // in a wasm global (lazy-init via `-1` sentinel) and loops
    // 1-byte `wasi:io/streams.[method]input-stream.blocking-read`
    // calls until `\n` or EOF, accumulating into a `cabi_realloc`-
    // owned buffer that doubles on overflow. Returns
    // `Result.Ok(line)` on success (including partial-line-then-EOF
    // — Unix convention) and `Result.Err("EOF")` only when the
    // first read produces zero bytes.
    let console_read_line: Option<ConsoleReadLineIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::CliStdinGetStdin)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::InputStreamBlockingRead)
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(result_idx) = registry.result_type_idx("Result<String,String>")
    {
        let res_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        types.ty().function([], [res_ref]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(ConsoleReadLineIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            result_string_string_type_idx: result_idx,
        })
    } else {
        None
    };

    // Phase 1.4c — `__rt_time_sleep(ms i64)` helper. Subscribes
    // for a `ms * 1_000_000` nanosecond duration on the monotonic
    // clock, polls the resulting pollable to completion, drops the
    // pollable. Pollable is per-call (single-use), so the
    // `[resource-drop]` here is mandatory — without it every call
    // would leak a host-side handle. Allocation order matches the
    // funcs/codes append order below; getting these out of sync
    // mis-routes call-site `Call(idx)` to the wrong helper body.
    let time_sleep: Option<TimeSleepIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::ClocksMonotonicSubscribeDuration,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoPollPoll)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoPollResourceDropPollable)
            .is_some()
    {
        types.ty().function([ValType::I64], []);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(TimeSleepIndices { fn_type, fn_idx })
    } else {
        None
    };

    // Phase 1.5.1 — `__rt_disk_exists(path: ref string) -> i32`
    // helper. Lazy-fetches the first preopen descriptor (cached
    // in `disk_preopen_handle` global), marshals the path bytes
    // through `__rt_string_to_lm`, calls
    // `wasi:filesystem/types.[method]descriptor.stat-at` and
    // returns `1` for an `Ok` result, `0` for `Err` (and `0`
    // when no preopens are configured at all). The
    // `descriptor-stat` payload itself is left untouched.
    let disk_exists: Option<DiskExistsIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesStatAt)
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
    {
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref], [ValType::I32]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(DiskExistsIndices { fn_type, fn_idx })
    } else {
        None
    };

    // Phase 1.5.2 — `__rt_disk_read_text(path: ref string) ->
    // ref null $result_string_string`. Lazy-fetches the preopen,
    // calls `open-at` to obtain a per-call file descriptor,
    // calls `read-via-stream` to obtain a per-call input-stream,
    // loops `blocking-read` (chunk size 65536) until EOF, copies
    // the accumulated bytes into a fresh GC `(array i8)` for the
    // `Result.Ok` payload, then drops both the input-stream and
    // file descriptor. Any failure short-circuits to a generic
    // `Result.Err("…")` (open failure / stream failure / read
    // failure are all categorised by the operation that produced
    // the error code, ignoring the specific `error-code` enum).
    let disk_read_text: Option<DiskReadTextIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesReadViaStream,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::InputStreamBlockingRead)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropInputStream,
            )
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(result_idx) = registry.result_type_idx("Result<String,String>")
    {
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref], [r_ref]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(DiskReadTextIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            result_string_string_type_idx: result_idx,
        })
    } else {
        None
    };

    // Phase 1.5.3 — `__rt_disk_write_text(path: ref string,
    // content: ref string) -> ref null $result_unit_string`.
    // Mirrors `__rt_disk_read_text`'s skeleton (preopens cache +
    // open-at + via-stream + blocking-* + drops) flipped to the
    // write side: `open-flags = create | truncate` (`5`),
    // `descriptor-flags = WRITE` (`2`), `write-via-stream` for
    // the output-stream, `blocking-write-and-flush` for the
    // bytes themselves. Failure at any step short-circuits to
    // `Result.Err("...")`.
    let disk_write_text: Option<DiskWriteTextIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesWriteViaStream,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
            )
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(result_idx) = registry.result_type_idx("Result<Unit,String>")
    {
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref, s_ref], [r_ref]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(DiskWriteTextIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            result_unit_string_type_idx: result_idx,
        })
    } else {
        None
    };

    // Phase 1.5.5 — `__rt_disk_append_text(path, content) ->
    // Result<Unit, String>`. Reuses the same body emitter as
    // `__rt_disk_write_text` flipped to append mode (open-flags
    // = CREATE only, append-via-stream instead of
    // write-via-stream).
    let disk_append_text: Option<DiskWriteTextIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesAppendViaStream,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
            )
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(result_idx) = registry.result_type_idx("Result<Unit,String>")
    {
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref, s_ref], [r_ref]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(DiskWriteTextIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            result_unit_string_type_idx: result_idx,
        })
    } else {
        None
    };

    // Phase 1.5.4 — `Disk.delete`, `Disk.deleteDir`, `Disk.makeDir`
    // share a generic helper: lazy-init preopen, marshal path,
    // call the matching `<op>-at`, return `Result.Ok(Unit)` on Ok-tag
    // / `Result.Err(<msg>)` on Err. Each Aver effect gets its own
    // wasm fn (different op fn idx + different err message), so
    // separate Indices structs per effect — but the body emitter is
    // a single helper parametrised by the wasi op + message.
    let alloc_path_op = |types: &mut wasm_encoder::TypeSection,
                         next_type_idx: &mut u32,
                         next_builtin_fn_idx: &mut u32,
                         op_slot: super::wasip2_imports::Wasip2ImportSlot|
     -> Option<DiskSimplePathOpIndices> {
        if cabi_realloc.is_none()
            || wasip2_imports
                .lookup_wasm_fn_idx(
                    super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
                )
                .is_none()
            || wasip2_imports.lookup_wasm_fn_idx(op_slot).is_none()
        {
            return None;
        }
        let string_idx = registry.string_array_type_idx?;
        let result_idx = registry.result_type_idx("Result<Unit,String>")?;
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref], [r_ref]);
        let fn_type = *next_type_idx;
        *next_type_idx += 1;
        let fn_idx = *next_builtin_fn_idx;
        *next_builtin_fn_idx += 1;
        Some(DiskSimplePathOpIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            result_unit_string_type_idx: result_idx,
        })
    };
    let disk_delete = alloc_path_op(
        &mut types,
        &mut next_type_idx,
        &mut next_builtin_fn_idx,
        super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesUnlinkFileAt,
    );
    let disk_delete_dir = alloc_path_op(
        &mut types,
        &mut next_type_idx,
        &mut next_builtin_fn_idx,
        super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesRemoveDirectoryAt,
    );
    let disk_make_dir = alloc_path_op(
        &mut types,
        &mut next_type_idx,
        &mut next_builtin_fn_idx,
        super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesCreateDirectoryAt,
    );

    // Phase 1.5.6 — `__rt_disk_list_dir(path: ref string) ->
    // ref null $result_list_string_string`. Opens path as a
    // directory, drives `read-directory-entry` until None,
    // accumulates each entry's name into a cons-built
    // `List<String>`. Order is filesystem-dependent (matches
    // POSIX `readdir` which doesn't promise any order either);
    // call sites that need a sorted list call `List.sort` after.
    let disk_list_dir: Option<DiskListDirIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesReadDirectory)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesDirectoryEntryStreamReadDirectoryEntry)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDirectoryEntryStream)
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(list_string_idx) = registry.list_type_idx("List<String>")
        && let Some(result_idx) = registry.result_type_idx("Result<List<String>,String>")
    {
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref], [r_ref]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(DiskListDirIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            list_string_type_idx: list_string_idx,
            result_list_string_string_type_idx: result_idx,
        })
    } else {
        None
    };

    // Phase 2.0 — `__rt_http_get(url: ref string) -> ref Result<
    // HttpResponse, String>`. Owns the entire wasi:http pipeline
    // (URL parse + fields/request constructors + setters + handle
    // + poll + future.get + status + consume + body.stream + drain
    // + per-call drops + HttpResponse build). All 16 new wasi:http
    // slots and 4 reused (poll/drop-pollable/blocking-read/drop-
    // input-stream) must be present, plus String / Result / Http
    // Response / Map<String, List<String>> type slots.
    let http_get: Option<super::wasip2_http::HttpGetIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesFieldsNew)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestNew)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetScheme)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetAuthority)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetPathWithQuery)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpOutgoingHandlerHandle)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesFutureIncomingResponseSubscribe)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoPollPoll)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoPollResourceDropPollable)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesFutureIncomingResponseGet)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingResponseStatus)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingResponseConsume)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingBodyStream)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::InputStreamBlockingRead)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingBodyFinish)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropInputStream)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropOutgoingRequest)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropFutureIncomingResponse)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropIncomingResponse)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropFutureTrailers)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropIncomingBody)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingResponseHeaders)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesFieldsEntries)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropFields)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetMethod)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestBody)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingBodyWrite)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingBodyFinish)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesFieldsAppend)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropOutgoingBody)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush)
            .is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropOutputStream)
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(result_idx) = registry.result_type_idx("Result<HttpResponse,String>")
        && let Some(resp_idx) = registry.record_type_idx("HttpResponse")
        && let Some(map_slots) = registry.map_slots("Map<String,List<String>>")
        && let Some(list_string_idx) = registry.list_type_idx("List<String>")
        && let Some(opt_list_string_idx) = registry.option_type_idx("Option<List<String>>")
    {
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        // Step J + K: 5 params total — method tag, url, content-
        // type, body, user headers map. The trailing three are
        // ignored for body-less methods (GET/HEAD/DELETE); the
        // dispatcher pushes empty values in those cases.
        let map_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(map_slots.map),
        });
        types.ty().function(
            [ValType::I32, s_ref, s_ref, s_ref, map_ref],
            [r_ref],
        );
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(super::wasip2_http::HttpGetIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            result_http_response_string_type_idx: result_idx,
            http_response_type_idx: resp_idx,
            headers_keys_array_type_idx: map_slots.keys_array,
            headers_values_array_type_idx: map_slots.values_array,
            headers_map_type_idx: map_slots.map,
            list_string_type_idx: list_string_idx,
            option_list_string_type_idx: opt_list_string_idx,
        })
    } else {
        None
    };

    // Phase 4.2.1 (0.20) — `__rt_tcp_connect(host: ref string, port: i64)
    // -> ref Result<Tcp.Connection, String>`. Body has a lazy
    // network-handle init prolog (Phase 4.2.2a); the remaining tail
    // is still the stub Result.Err. Real DNS / socket / connect
    // pipeline lands in Phase 4.2.2b+. Gated on the String slot +
    // `Result<Tcp.Connection,String>` factory + the network-handle
    // global + the `instance-network` import — all populated by the
    // Phase 4.1a/b wireup whenever the program declares
    // `! [Tcp.connect]`.
    let tcp_connect: Option<super::wasip2_tcp::TcpConnectIndices> = if let (
        Some(string_idx),
        Some(result_idx),
        Some(stub_seg),
        Some(dns_seg),
        Some(no_addr_seg),
        Some(_instance_network_fn),
        Some(_resolve_addresses_fn),
        Some(_drop_resolve_stream_fn),
        Some(_stream_subscribe_fn),
        Some(_poll_fn),
        Some(_drop_pollable_fn),
        Some(_resolve_next_address_fn),
    ) = (
        registry.string_array_type_idx,
        registry.result_type_idx("Result<Tcp.Connection,String>"),
        registry.string_literal_segment(b"tcp: connect not yet implemented"),
        registry.string_literal_segment(b"tcp: dns resolve failed"),
        registry.string_literal_segment(b"tcp: dns no addresses"),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
        ),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses,
        ),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
        ),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
        ),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::IoPollPoll,
        ),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::IoPollResourceDropPollable,
        ),
        wasip2_imports.lookup_wasm_fn_idx(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress,
        ),
    ) {
        let r_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(result_idx),
        });
        let s_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(string_idx),
        });
        types.ty().function([s_ref, ValType::I64], [r_ref]);
        let fn_type = next_type_idx;
        next_type_idx += 1;
        let fn_idx = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(super::wasip2_tcp::TcpConnectIndices {
            fn_type,
            fn_idx,
            string_type_idx: string_idx,
            stub_err_segment_idx: stub_seg,
            stub_err_len: b"tcp: connect not yet implemented".len() as u32,
            dns_err_segment_idx: dns_seg,
            dns_err_len: b"tcp: dns resolve failed".len() as u32,
            no_addr_segment_idx: no_addr_seg,
            no_addr_len: b"tcp: dns no addresses".len() as u32,
        })
    } else {
        None
    };

    let env_get_lookup: Option<EnvGetLookupIndices> = if cabi_realloc.is_some()
        && wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::CliEnvironmentGetEnvironment,
            )
            .is_some()
        && let Some(string_idx) = registry.string_array_type_idx
        && let Some(option_string_idx) = registry.option_type_idx("Option<String>")
    {
        let opt_ref = ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(option_string_idx),
        });
        types
            .ty()
            .function([ValType::I32, ValType::I32, ValType::I32], [opt_ref]);
        let lookup_type = next_type_idx;
        next_type_idx += 1;
        let lookup_fn = next_builtin_fn_idx;
        next_builtin_fn_idx += 1;
        Some(EnvGetLookupIndices {
            fn_type: lookup_type,
            fn_idx: lookup_fn,
            string_type_idx: string_idx,
            option_string_type_idx: option_string_idx,
        })
    } else {
        None
    };

    // Phase 1.4b — `__rt_format_iso8601(secs i64, nanos i32) ->
    // ref null $string`. Pure-compute helper that turns the
    // datetime returned by `wasi:clocks/wall-clock.now` into the
    // RFC3339-like string Aver's `Time.now() -> String` exposes.
    // Materialises a fresh 24-byte `(array i8)` and writes
    // `YYYY-MM-DDTHH:MM:SS.mmmZ` into it. The civil_from_days
    // arithmetic mirrors `aver-rt::format_utc_rfc3339_like`.
    // Allocated whenever wasip2 + the clocks slot are wired —
    // `wasm-opt -Oz` strips this when only `Time.unixMs` reaches
    // the import (i.e. no source-level `Time.now`). Allocation
    // position is the LAST helper before factory exports because
    // the funcs/codes append phase below emits its entry last.
    let format_iso8601: Option<FormatIso8601Indices> =
        if matches!(target, super::TargetMode::Wasip2)
            && wasip2_imports
                .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::ClocksWallClockNow)
                .is_some()
            && let Some(string_idx) = registry.string_array_type_idx
        {
            let s_ref = ValType::Ref(wasm_encoder::RefType {
                nullable: true,
                heap_type: wasm_encoder::HeapType::Concrete(string_idx),
            });
            types.ty().function([ValType::I64, ValType::I32], [s_ref]);
            let fn_type = next_type_idx;
            next_type_idx += 1;
            let fn_idx = next_builtin_fn_idx;
            next_builtin_fn_idx += 1;
            Some(FormatIso8601Indices {
                fn_type,
                fn_idx,
                string_type_idx: string_idx,
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
    //
    // Same per-target branch as the import-type emission above.
    // AverBridge writes `(import "aver" "<name>" ...)` per
    // registered effect; Wasip2 writes the canonical-ABI form
    // (`(import "wasi:cli/stdout@0.2.4" "get-stdout" ...)` etc.).
    match target {
        super::TargetMode::AverBridge => {
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
        }
        super::TargetMode::Wasip2 => {
            if wasip2_imports.import_count() > 0 {
                let mut imports = ImportSection::new();
                for slot in wasip2_imports.iter() {
                    let (module_, field) = slot.module_field_pair();
                    let type_idx = wasip2_imports
                        .lookup_wasm_type_idx(slot)
                        .expect("just-assigned wasip2 import type idx");
                    imports.import(module_, field, EntityType::Function(type_idx));
                }
                module.section(&imports);
            }
        }
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
    if let Some(c) = &cabi_realloc {
        funcs.function(c.fn_type);
    }
    if let Some(d) = &decode_list_string {
        funcs.function(d.fn_type);
    }
    if let Some(c) = &console_read_line {
        funcs.function(c.fn_type);
    }
    if let Some(t) = &time_sleep {
        funcs.function(t.fn_type);
    }
    if let Some(d) = &disk_exists {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_read_text {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_write_text {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_append_text {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_delete {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_delete_dir {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_make_dir {
        funcs.function(d.fn_type);
    }
    if let Some(d) = &disk_list_dir {
        funcs.function(d.fn_type);
    }
    if let Some(h) = &http_get {
        funcs.function(h.fn_type);
    }
    if let Some(t) = &tcp_connect {
        funcs.function(t.fn_type);
    }
    if let Some(e) = &env_get_lookup {
        funcs.function(e.fn_type);
    }
    if let Some(fmt) = &format_iso8601 {
        funcs.function(fmt.fn_type);
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

    // ── Memory section ─────────────────────────────────────────────
    //
    // 1 page initial, 2048 max (128 MiB ceiling — matches Cloudflare
    // Workers' per-request memory limit). The bridge helpers grow
    // on demand: `__rt_string_to_lm` checks if it can fit the
    // outgoing array and calls `memory.grow` if not.
    //
    // Two reasons to emit memory:
    // - AverBridge with a JS-host bridge: `__rt_string_to_lm` /
    //   `__rt_string_from_lm` need transport space for the
    //   `(array i8)` ↔ `(ptr, len)` boundary the JS host reads.
    // - Wasip2 with any registered canonical-ABI import: same LM
    //   transport, same helpers internally — `wasi:io/streams.
    //   [method]output-stream.blocking-write-and-flush` takes a
    //   `(ptr, len)` lowered from a `list<u8>`, plus a 12-byte
    //   retptr scratch area for the host-written
    //   `result<_, stream-error>`. The helpers are NOT exported
    //   on wasip2 (no JS host calls them); they exist purely for
    //   internal wasm-side glue at effect call sites.
    let need_memory_for_wasip2 =
        matches!(target, super::TargetMode::Wasip2) && wasip2_imports.import_count() > 0;
    if bridge.is_some() || need_memory_for_wasip2 {
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

    // ── Globals section (wasip2 resource-handle caches) ────────────
    //
    // On `TargetMode::Wasip2`, when `Console.print` / `error` / `warn`
    // is registered, the call-site glue caches the host-supplied
    // `output-stream` resource handle in a wasm global. -1 is the
    // sentinel for "not yet initialised"; the first call evaluates
    // the matching `wasi:cli/{stdout,stderr}.get-stdout/stderr`
    // import and stores the result. Per-call branch is one
    // `i32.eq` + `if` — negligible against the syscall it guards.
    //
    // Globals are emitted only when at least one of stdout/stderr
    // is actually used (`OutputStreamBlockingWriteAndFlush` registered
    // implies at least one of `CliGetStdout` / `CliGetStderr` does
    // too). Empty Aver programs hit neither and skip the section
    // entirely — no semantic change vs. Phase 1.2b1.3.
    let wasip2_globals: Option<Wasip2Globals> =
        if matches!(target, super::TargetMode::Wasip2) && wasip2_imports.import_count() > 0 {
            let mut globals = wasm_encoder::GlobalSection::new();
            let mut next_global_idx: u32 = 0;
            // Global 0 — bump-alloc cursor for `cabi_realloc`. Initial
            // value `65536` (= page 2 base). Page 1 stays reserved for
            // the `__rt_string_to_lm` transient buffer that
            // `Console.*` writes use; persistent `cabi_realloc` heap
            // grows upward from page 2 with `memory.grow` on overflow.
            // Allocated unconditionally on the wasip2 path so the
            // cabi_realloc helper has a stable global idx to read /
            // write — Phase 1.3.1 onwards consumes it; earlier phases
            // tolerate the unused global (12 bytes of section overhead).
            globals.global(
                wasm_encoder::GlobalType {
                    val_type: ValType::I32,
                    mutable: true,
                    shared: false,
                },
                &wasm_encoder::ConstExpr::i32_const(65536),
            );
            let bump_alloc_ptr = next_global_idx;
            next_global_idx += 1;
            let stdout_handle = if wasip2_imports
                .lookup_wasm_type_idx(super::wasip2_imports::Wasip2ImportSlot::CliGetStdout)
                .is_some()
            {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(-1),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            let stderr_handle = if wasip2_imports
                .lookup_wasm_type_idx(super::wasip2_imports::Wasip2ImportSlot::CliGetStderr)
                .is_some()
            {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(-1),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            // Phase 1.3.4 — stdin handle cache global. Same lazy-init
            // pattern as stdout/stderr: starts as -1 sentinel, every
            // `Console.readLine` call site checks the global and runs
            // `wasi:cli/stdin.get-stdin` once on first read. The
            // resource is program-lifetime (wasmtime cleans up at
            // component exit) so we never emit `[resource-drop]`.
            let stdin_handle = if wasip2_imports
                .lookup_wasm_type_idx(super::wasip2_imports::Wasip2ImportSlot::CliStdinGetStdin)
                .is_some()
            {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(-1),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            // Phase 1.5.1 — disk preopen descriptor cache. -1 sentinel
            // for "not yet fetched". On first `Disk.*` call the helper
            // calls `wasi:filesystem/preopens.get-directories`, takes
            // the first entry's descriptor handle, and caches it here.
            // Program-lifetime — no `[resource-drop]descriptor` for
            // the preopen, wasmtime cleans up at component exit.
            let disk_preopen_handle = if wasip2_imports
                .lookup_wasm_type_idx(
                    super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
                )
                .is_some()
            {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(-1),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            // Phase 4 (0.20) — wasi:sockets network handle cache,
            // tcp_pool array, and tcp_next_id counter. All gated on
            // the matching slot/type registrations so non-TCP programs
            // pay nothing.
            let network_handle = if wasip2_imports
                .lookup_wasm_type_idx(
                    super::wasip2_imports::Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
                )
                .is_some()
            {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(-1),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            let tcp_pool = if let Some(pool_type_idx) = registry.tcp_pool_type_idx {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::Ref(wasm_encoder::RefType {
                            nullable: true,
                            heap_type: wasm_encoder::HeapType::Concrete(pool_type_idx),
                        }),
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::ref_null(wasm_encoder::HeapType::Concrete(
                        pool_type_idx,
                    )),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            let tcp_next_id = if registry.tcp_pool_type_idx.is_some() {
                globals.global(
                    wasm_encoder::GlobalType {
                        val_type: ValType::I32,
                        mutable: true,
                        shared: false,
                    },
                    &wasm_encoder::ConstExpr::i32_const(0),
                );
                let idx = next_global_idx;
                next_global_idx += 1;
                Some(idx)
            } else {
                None
            };
            let _ = next_global_idx;
            module.section(&globals);
            Some(Wasip2Globals {
                bump_alloc_ptr,
                stdout_handle,
                stderr_handle,
                stdin_handle,
                disk_preopen_handle,
                network_handle,
                tcp_pool,
                tcp_next_id,
            })
        } else {
            None
        };

    // Phase 1.2b1.5 — `Wasip2Lowering` collects every fn / global /
    // helper idx the call-site lowering for `Console.print` /
    // `Console.error` / `Console.warn` needs to emit canonical-ABI
    // calls instead of the AverBridge `aver/console_print` import.
    // Constructed only when wasip2 imports were registered AND the
    // bridge fn machinery is in place (the latter implies
    // `__rt_string_to_lm` has been allocated — the call site uses
    // it to marshal the Aver String into LM[0..len]).
    let wasip2_lowering: Option<super::body::Wasip2Lowering> =
        if matches!(target, super::TargetMode::Wasip2) && wasip2_imports.import_count() > 0 {
            use super::wasip2_imports::Wasip2ImportSlot;
            // Console.* needs both the bridge `__rt_string_to_lm`
            // helper and the resource-handle globals; clocks /
            // random don't. So those fields are populated only when
            // their owning effects are registered, not as a
            // precondition for `Some(...)` on the whole struct.
            Some(super::body::Wasip2Lowering {
                get_stdout_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::CliGetStdout),
                get_stderr_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::CliGetStderr),
                blocking_write_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush),
                stdout_handle_global: wasip2_globals.as_ref().and_then(|g| g.stdout_handle),
                stderr_handle_global: wasip2_globals.as_ref().and_then(|g| g.stderr_handle),
                str_to_lm_fn_idx: bridge.as_ref().map(|b| b.to_lm_fn),
                clocks_now_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::ClocksWallClockNow),
                random_u64_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::RandomGetRandomU64),
                get_arguments_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::CliEnvironmentGetArguments),
                cabi_realloc_fn_idx: cabi_realloc.as_ref().map(|c| c.fn_idx),
                decode_list_string_fn_idx: decode_list_string.as_ref().map(|d| d.fn_idx),
                get_environment_fn_idx: wasip2_imports
                    .lookup_wasm_fn_idx(Wasip2ImportSlot::CliEnvironmentGetEnvironment),
                env_get_lookup_fn_idx: env_get_lookup.as_ref().map(|e| e.fn_idx),
                fmt_iso8601_fn_idx: format_iso8601.as_ref().map(|f| f.fn_idx),
                console_read_line_fn_idx: console_read_line.as_ref().map(|c| c.fn_idx),
                time_sleep_fn_idx: time_sleep.as_ref().map(|t| t.fn_idx),
                disk_exists_fn_idx: disk_exists.as_ref().map(|d| d.fn_idx),
                disk_read_text_fn_idx: disk_read_text.as_ref().map(|d| d.fn_idx),
                disk_write_text_fn_idx: disk_write_text.as_ref().map(|d| d.fn_idx),
                disk_append_text_fn_idx: disk_append_text.as_ref().map(|d| d.fn_idx),
                disk_delete_fn_idx: disk_delete.as_ref().map(|d| d.fn_idx),
                disk_delete_dir_fn_idx: disk_delete_dir.as_ref().map(|d| d.fn_idx),
                disk_make_dir_fn_idx: disk_make_dir.as_ref().map(|d| d.fn_idx),
                disk_list_dir_fn_idx: disk_list_dir.as_ref().map(|d| d.fn_idx),
                http_get_fn_idx: http_get.as_ref().map(|h| h.fn_idx),
                tcp_connect_fn_idx: tcp_connect.as_ref().map(|t| t.fn_idx),
                network_handle_global: wasip2_globals.as_ref().and_then(|g| g.network_handle),
                tcp_pool_global: wasip2_globals.as_ref().and_then(|g| g.tcp_pool),
                tcp_next_id_global: wasip2_globals.as_ref().and_then(|g| g.tcp_next_id),
                tcp_slot_type_idx: registry.tcp_slot_type_idx,
                tcp_pool_type_idx: registry.tcp_pool_type_idx,
            })
        } else {
            None
        };

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
    if matches!(target, super::TargetMode::AverBridge) {
        for name in effect_registry.iter() {
            let idx = effect_registry
                .lookup_wasm_fn_idx(name)
                .expect("registered effect has wasm fn idx");
            effect_idx_lookup.insert(name.canonical().to_string(), idx);
        }
    }
    // On `TargetMode::Wasip2` the EffectRegistry is populated by
    // discovery but never `assign_slots`'d (the import section uses
    // the parallel `Wasip2ImportRegistry` instead), so its
    // `lookup_wasm_fn_idx` returns None for every effect. Leave
    // `effect_idx_lookup` empty: the wasip2 call-site lowering goes
    // through `ctx.wasip2_lowering`, not `ctx.fn_map.effects` /
    // `ctx.effect_idx_lookup`. Effects that the wasip2 path doesn't
    // yet lower (`?!` / `!` independent-product markers, etc.) are
    // out-of-scope for Phase 1.2b1; rejection lives upstream.
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
    // Map<K,V> structural-eq fn idxs flow through the same lookup so
    // BinOp::Eq on a Map dispatches via `Call(__eq_Map<K,V>)` (sum_
    // or_record_eq_fn → ctx.fn_map.eq_helpers). Whitespace-free
    // canonical matches what the operand's `.ty().display()` produces
    // at the call site.
    for canonical in &registry.map_order {
        if let Some(h) = map_helpers.kv_helpers(canonical) {
            eq_helpers_lookup.insert(canonical.clone(), h.eq);
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
    //
    // Entry-point export name follows the `target`. AverBridge keeps
    // `_start` (the convention every JS host the wasm-gc backend
    // serves understands). Wasip2 exports `wasi:cli/run@0.2.4#run`
    // — the canonical-ABI export name for the WIT function
    // `wasi:cli/run.run`. `wit_component::ComponentEncoder` matches
    // this name against the `wasi:cli/command` world's required
    // `run` export when binding the metadata-declared component
    // surface to the core module.
    let mut exports = ExportSection::new();
    let start_export_name: &str = match (target, proxy_mode) {
        (super::TargetMode::AverBridge, _) => "_start",
        (super::TargetMode::Wasip2, false) => "wasi:cli/run@0.2.4#run",
        // `wasi:http/incoming-handler.handle` — the proxy world's
        // sole required export. `wasmtime serve` / Spin / wasmCloud
        // route every inbound HTTP request through this.
        (super::TargetMode::Wasip2, true) => "wasi:http/incoming-handler@0.2.4#handle",
    };
    exports.export(start_export_name, ExportKind::Func, start_wasm_idx);
    for (i, fd) in fn_defs.iter().enumerate() {
        let wasm_idx = import_count + 1 + (i as u32);
        exports.export(&fd.name, ExportKind::Func, wasm_idx);
    }
    if let Some(b) = &bridge {
        // The four `__rt_*` exports are JS-host-callable only — wasip2
        // hosts (wasmtime / Spin / wasmCloud) consume the canonical-
        // ABI surface, never these names. Skip them when target is
        // Wasip2 to keep the component contract clean (no leaked JS
        // runtime names in a non-JS world).
        if matches!(target, super::TargetMode::AverBridge) {
            exports.export("__rt_string_from_lm", ExportKind::Func, b.from_lm_fn);
            exports.export("__rt_string_to_lm", ExportKind::Func, b.to_lm_fn);
            exports.export("__rt_memory_pages", ExportKind::Func, b.pages_fn);
            exports.export("__rt_memory_grow", ExportKind::Func, b.grow_fn);
        }
        exports.export("memory", ExportKind::Memory, 0);
    } else if cabi_realloc.is_some() {
        // wasip2 path with no bridge (theoretical — cabi_realloc
        // gates on `wasip2_imports.import_count() > 0` which only
        // fires when an effect that needs LM is registered, and
        // every such effect today implies a String). Defensive
        // export so the host can find memory regardless.
        exports.export("memory", ExportKind::Memory, 0);
    }
    if let Some(c) = &cabi_realloc {
        // Required by Component Model canonical ABI as the guest's
        // realloc callback. Phase 1.3.1 ships the impl; consumers
        // (Args.get / Env.get / Console.readLine / Disk.readText)
        // start landing in 1.3.2.
        exports.export("cabi_realloc", ExportKind::Func, c.fn_idx);
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

    // Pre-register the synthesised `aver_http_handle` wrapper as a
    // caller_fn entry — `emit_handler_wrapper` (much later in the
    // code section) pushes this idx as the trailing `caller_fn_idx`
    // arg for every Request.* / Response.* effect call. Has to land
    // BEFORE the pre-pass snapshot below, otherwise data section ends
    // up with one more passive segment than the data count section
    // declared, and the validator rejects the module.
    let wrapper_caller_fn_idx: Option<u32> = handler_wrapper.as_ref().map(|_| {
        caller_fn_collector
            .borrow_mut()
            .register("aver_http_handle")
    });

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
            wasip2_lowering.as_ref(),
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

    // Entry-point body. Three shapes, one slot:
    //
    // - AverBridge `_start: () -> ()` — call main, drop any return.
    //   JS host observes side effects via `aver/*` imports.
    // - Wasip2 / CliCommand `wasi:cli/run.run: () -> i32` — same
    //   call+drop, then `i32.const 0` (Ok in `result<_, _>`).
    // - Wasip2 / HttpProxy `wasi:http/incoming-handler.handle:
    //   (request, outparam) -> ()` — body is the full per-request
    //   choreography emitted via `emit_aver_http_handle`. `main`
    //   is never invoked (it's emitted as a normal user fn and
    //   never called from here); its `HttpServer.listen(_, _)`
    //   call lowered to a no-op upstream.
    if proxy_mode {
        let user_handler_idx = handler_name
            .and_then(|name| fn_defs.iter().position(|fd| fd.name == name))
            .ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "proxy handler `{}` doesn't match any fn in this module",
                    handler_name.unwrap_or("?")
                ))
            })?;
        let user_handler_wasm_idx = import_count + 1 + (user_handler_idx as u32);

        let string_idx = registry
            .string_array_type_idx
            .ok_or_else(|| WasmGcError::Validation("proxy mode requires String slot".into()))?;
        let http_request_idx = registry.record_type_idx("HttpRequest").ok_or_else(|| {
            WasmGcError::Validation("proxy mode requires HttpRequest record slot".into())
        })?;
        let http_response_idx = registry.record_type_idx("HttpResponse").ok_or_else(|| {
            WasmGcError::Validation("proxy mode requires HttpResponse record slot".into())
        })?;
        let map_slots = registry
            .map_slots("Map<String,List<String>>")
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "proxy mode requires Map<String, List<String>> slots".into(),
                )
            })?;
        let list_string_idx = registry.list_type_idx("List<String>").ok_or_else(|| {
            WasmGcError::Validation("proxy mode requires List<String> slot".into())
        })?;
        let opt_list_string_idx = registry
            .option_type_idx("Option<List<String>>")
            .ok_or_else(|| {
                WasmGcError::Validation("proxy mode requires Option<List<String>> slot".into())
            })?;
        let cabi = cabi_realloc
            .as_ref()
            .ok_or_else(|| WasmGcError::Validation("proxy mode requires cabi_realloc".into()))?
            .fn_idx;
        let bridge_ref = bridge
            .as_ref()
            .ok_or_else(|| WasmGcError::Validation("proxy mode requires bridge helpers".into()))?;
        let map_h = map_helpers
            .kv_helpers("Map<String,List<String>>")
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "proxy mode requires Map<String, List<String>> kv helpers".into(),
                )
            })?;
        let lookup = |slot: super::wasip2_imports::Wasip2ImportSlot,
                      name: &'static str|
         -> Result<u32, WasmGcError> {
            wasip2_imports.lookup_wasm_fn_idx(slot).ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "proxy mode requires {name} import (slot not allocated)"
                ))
            })
        };
        use super::wasip2_imports::Wasip2ImportSlot as Slot;
        let indices = super::wasip2_http_server::ServerHandlerIndices {
            fn_type: start_type_idx,
            fn_idx: start_wasm_idx,
            string_type_idx: string_idx,
            http_request_type_idx: http_request_idx,
            http_response_type_idx: http_response_idx,
            headers_keys_array_type_idx: map_slots.keys_array,
            headers_values_array_type_idx: map_slots.values_array,
            headers_map_type_idx: map_slots.map,
            list_string_type_idx: list_string_idx,
            option_list_string_type_idx: opt_list_string_idx,
        };
        let helpers = super::wasip2_http_server::ServerHandlerHelperFns {
            cabi_realloc_fn: cabi,
            str_to_lm_fn: bridge_ref.to_lm_fn,
            from_lm_fn: bridge_ref.from_lm_fn,
            incoming_request_method_fn: lookup(
                Slot::HttpTypesIncomingRequestMethod,
                "incoming-request.method",
            )?,
            incoming_request_path_with_query_fn: lookup(
                Slot::HttpTypesIncomingRequestPathWithQuery,
                "incoming-request.path-with-query",
            )?,
            incoming_request_headers_fn: lookup(
                Slot::HttpTypesIncomingRequestHeaders,
                "incoming-request.headers",
            )?,
            incoming_request_consume_fn: lookup(
                Slot::HttpTypesIncomingRequestConsume,
                "incoming-request.consume",
            )?,
            drop_incoming_request_fn: lookup(
                Slot::HttpTypesResourceDropIncomingRequest,
                "[resource-drop]incoming-request",
            )?,
            fields_entries_fn: lookup(Slot::HttpTypesFieldsEntries, "fields.entries")?,
            drop_fields_fn: lookup(Slot::HttpTypesResourceDropFields, "[resource-drop]fields")?,
            incoming_body_stream_fn: lookup(
                Slot::HttpTypesIncomingBodyStream,
                "incoming-body.stream",
            )?,
            incoming_body_finish_fn: lookup(
                Slot::HttpTypesIncomingBodyFinish,
                "incoming-body.finish",
            )?,
            drop_incoming_body_fn: lookup(
                Slot::HttpTypesResourceDropIncomingBody,
                "[resource-drop]incoming-body",
            )?,
            blocking_read_fn: lookup(Slot::InputStreamBlockingRead, "input-stream.blocking-read")?,
            drop_input_stream_fn: lookup(
                Slot::IoStreamsResourceDropInputStream,
                "[resource-drop]input-stream",
            )?,
            drop_future_trailers_fn: lookup(
                Slot::HttpTypesResourceDropFutureTrailers,
                "[resource-drop]future-trailers",
            )?,
            fields_new_fn: lookup(Slot::HttpTypesFieldsNew, "[constructor]fields")?,
            fields_append_fn: lookup(Slot::HttpTypesFieldsAppend, "fields.append")?,
            outgoing_response_new_fn: lookup(
                Slot::HttpTypesOutgoingResponseNew,
                "[constructor]outgoing-response",
            )?,
            set_status_code_fn: lookup(
                Slot::HttpTypesOutgoingResponseSetStatusCode,
                "outgoing-response.set-status-code",
            )?,
            outgoing_response_body_fn: lookup(
                Slot::HttpTypesOutgoingResponseBody,
                "outgoing-response.body",
            )?,
            outgoing_body_write_fn: lookup(
                Slot::HttpTypesOutgoingBodyWrite,
                "outgoing-body.write",
            )?,
            outgoing_body_finish_fn: lookup(
                Slot::HttpTypesOutgoingBodyFinish,
                "outgoing-body.finish",
            )?,
            blocking_write_fn: lookup(
                Slot::OutputStreamBlockingWriteAndFlush,
                "output-stream.blocking-write-and-flush",
            )?,
            drop_output_stream_fn: lookup(
                Slot::IoStreamsResourceDropOutputStream,
                "[resource-drop]output-stream",
            )?,
            drop_outgoing_body_fn: lookup(
                Slot::HttpTypesResourceDropOutgoingBody,
                "[resource-drop]outgoing-body",
            )?,
            response_outparam_set_fn: lookup(
                Slot::HttpTypesResponseOutparamSet,
                "[static]response-outparam.set",
            )?,
            map_set_fn: map_h.set,
            map_get_fn: map_h.get,
            user_handler_fn: user_handler_wasm_idx,
        };
        codes.function(&super::wasip2_http_server::emit_aver_http_handle(
            &indices, &helpers,
        ));
    } else {
        let mut start = Function::new([]);
        if let Some(idx) = main_idx {
            let main_idx_wasm = import_count + 1 + (idx as u32);
            let main_returns_value = !fn_defs[idx].return_type.trim().eq("Unit");
            start.instruction(&Instruction::Call(main_idx_wasm));
            if main_returns_value {
                start.instruction(&Instruction::Drop);
            }
        }
        if start_returns_i32 {
            start.instruction(&Instruction::I32Const(0));
        }
        start.instruction(&Instruction::End);
        codes.function(&start);
    }

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
            wasip2_lowering.as_ref(),
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
            wasip2_lowering.as_ref(),
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
    // Map<K,V> structural eq + commutative hash — per-instantiation
    // helpers live in MapHelperRegistry::kv. Threading them into the
    // compound lookup lets record/sum/list/vec field dispatch call
    // `__eq_Map<K,V>` / `__hash_Map<K,V>` uniformly with carriers.
    for canonical in &registry.map_order {
        if let Some(h) = map_helpers.kv_helpers(canonical) {
            compound_eq_hash_lookup.insert(canonical.clone(), (h.eq, h.hash));
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
    // Merge in list_helpers' own list/vec canonicals so `List<List<X>>`
    // / `List<Vector<X>>` element dispatch finds the inner helper.
    let string_eq_fn_idx = builtin_registry.lookup_wasm_fn_idx(BuiltinName::StringEq);
    let mut eq_helper_fn_idx_map: HashMap<String, u32> = eq_helpers_registry
        .iter()
        .filter_map(|(n, _k)| {
            eq_helpers_registry
                .lookup_fn_idx(n)
                .map(|i| (n.to_string(), i))
        })
        .collect();
    let mut hash_helper_fn_idx_map: HashMap<String, u32> = hash_helpers_registry
        .iter()
        .filter_map(|(n, _k)| {
            hash_helpers_registry
                .lookup_fn_idx(n)
                .map(|i| (n.to_string(), i))
        })
        .collect();
    for (canonical, (eq_fn, hash_fn)) in &compound_eq_hash_lookup {
        eq_helper_fn_idx_map.insert(canonical.clone(), *eq_fn);
        hash_helper_fn_idx_map.insert(canonical.clone(), *hash_fn);
    }
    list_helpers.emit_helper_bodies(
        &mut codes,
        &registry,
        string_eq_fn_idx,
        &eq_helper_fn_idx_map,
        &hash_helper_fn_idx_map,
    )?;

    // Per-(record/sum) `__eq_<TypeName>` helper bodies — emit after
    // list helpers so any String fields can call `__wasmgc_string_eq`
    // by the index recorded above. The compound eq lookup forwards
    // `List<T>` / `Vector<T>` fn idxs so a record field of type
    // `List<Option<Int>>` (etc.) can dispatch via
    // `Call(__eq_List<…>)`. Same shape on the hash side.
    let compound_eq_lookup: HashMap<String, u32> = compound_eq_hash_lookup
        .iter()
        .map(|(n, (eq, _))| (n.clone(), *eq))
        .collect();
    let compound_hash_lookup: HashMap<String, u32> = compound_eq_hash_lookup
        .iter()
        .map(|(n, (_, h))| (n.clone(), *h))
        .collect();
    eq_helpers_registry.emit_helper_bodies(
        &mut codes,
        &registry,
        string_eq_fn_idx,
        &compound_eq_lookup,
    )?;
    // `__hash_<X>` helper bodies — emitted right after eq helpers so
    // every nominal/carrier hash dispatch finds its target fn_idx.
    hash_helpers_registry.emit_helper_bodies(
        &mut codes,
        &registry,
        string_eq_fn_idx,
        &compound_hash_lookup,
    )?;

    if let Some(hw) = &handler_wrapper {
        let user_handler_wasm_idx = import_count + 1 + (hw.user_handler_idx as u32);
        // Reserve a caller_fn idx for the synthesised wrapper itself.
        // `emit_handler_wrapper` pushes this constant before every
        // host-effect `Call` to satisfy the ABI's trailing
        // `caller_fn_idx: i32` param (added in 0.16). The idx was
        // pre-registered above the pre-pass so the data count section
        // already accounts for the segment.
        let wrapper_caller_fn_idx = wrapper_caller_fn_idx
            .expect("handler_wrapper present implies wrapper_caller_fn_idx pre-registered");
        codes.function(&emit_handler_wrapper(
            &registry,
            &fn_map,
            user_handler_wasm_idx,
            wrapper_caller_fn_idx,
        )?);
        codes.function(&emit_list_string_cons(&registry)?);
        let _ = hw.list_cons_type; // type idx already consumed by emit_function_section
    }

    if bridge.is_some() {
        emit_bridge_bodies(&mut codes, &registry)?;
    }

    if cabi_realloc.is_some() {
        // `wasip2_globals` is `Some` whenever `cabi_realloc` is —
        // both gate on `wasip2_imports.import_count() > 0` and
        // `Wasip2Globals::bump_alloc_ptr` is allocated
        // unconditionally on that path. Unwrap is sound;
        // `expect` carries a louder message than a silent panic
        // if the invariant ever drifts.
        let bump_global = wasip2_globals
            .as_ref()
            .expect("cabi_realloc emit requires Wasip2Globals (same gate)")
            .bump_alloc_ptr;
        codes.function(&emit_cabi_realloc(bump_global));
    }
    if let Some(d) = &decode_list_string {
        codes.function(&emit_decode_list_string(
            d.string_type_idx,
            d.list_string_type_idx,
        ));
    }
    if let Some(c) = &console_read_line {
        let stdin_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.stdin_handle)
            .expect("console_read_line emit requires stdin_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("console_read_line emit requires cabi_realloc fn idx (gate matches)")
            .fn_idx;
        let get_stdin = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::CliStdinGetStdin)
            .expect("console_read_line emit requires CliStdinGetStdin fn idx (gate matches)");
        let blocking_read = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::InputStreamBlockingRead)
            .expect("console_read_line emit requires InputStreamBlockingRead fn idx");
        codes.function(&emit_console_read_line(
            c.string_type_idx,
            c.result_string_string_type_idx,
            stdin_global,
            cabi,
            get_stdin,
            blocking_read,
        ));
    }
    if time_sleep.is_some() {
        let cabi = cabi_realloc
            .as_ref()
            .expect("time_sleep emit requires cabi_realloc fn idx (gate matches)")
            .fn_idx;
        let subscribe = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::ClocksMonotonicSubscribeDuration,
            )
            .expect("time_sleep emit requires subscribe-duration fn idx (gate matches)");
        let poll = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoPollPoll)
            .expect("time_sleep emit requires poll fn idx (gate matches)");
        let drop_pollable = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::IoPollResourceDropPollable)
            .expect("time_sleep emit requires drop-pollable fn idx (gate matches)");
        codes.function(&emit_time_sleep(cabi, subscribe, poll, drop_pollable));
    }
    if disk_exists.is_some() {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_exists emit requires disk_preopen_handle global (gate matches)");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_exists emit requires cabi_realloc fn idx (gate matches)")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_exists emit requires bridge (string marshalling)")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_exists emit requires get-directories fn idx (gate matches)");
        let stat_at = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesStatAt)
            .expect("disk_exists emit requires stat-at fn idx (gate matches)");
        codes.function(&emit_disk_exists(
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            stat_at,
        ));
    }
    if let Some(rt) = &disk_read_text {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_read_text emit requires disk_preopen_handle global (gate matches)");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_read_text emit requires cabi_realloc fn idx (gate matches)")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_read_text emit requires bridge (string marshalling)")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_read_text emit requires get-directories fn idx");
        let open_at = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .expect("disk_read_text emit requires open-at fn idx");
        let read_via_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesReadViaStream,
            )
            .expect("disk_read_text emit requires read-via-stream fn idx");
        let blocking_read = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::InputStreamBlockingRead)
            .expect("disk_read_text emit requires blocking-read fn idx");
        let drop_descriptor = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .expect("disk_read_text emit requires drop-descriptor fn idx");
        let drop_input_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropInputStream,
            )
            .expect("disk_read_text emit requires drop-input-stream fn idx");
        codes.function(&emit_disk_read_text(
            rt.string_type_idx,
            rt.result_string_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            open_at,
            read_via_stream,
            blocking_read,
            drop_descriptor,
            drop_input_stream,
        ));
    }
    if let Some(wt) = &disk_write_text {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_write_text emit requires disk_preopen_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_write_text emit requires cabi_realloc fn idx")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_write_text emit requires bridge")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_write_text emit requires get-directories fn idx");
        let open_at = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .expect("disk_write_text emit requires open-at fn idx");
        let write_via_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesWriteViaStream,
            )
            .expect("disk_write_text emit requires write-via-stream fn idx");
        let blocking_write = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
            )
            .expect("disk_write_text emit requires blocking-write-and-flush fn idx");
        let drop_descriptor = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .expect("disk_write_text emit requires drop-descriptor fn idx");
        let drop_output_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
            )
            .expect("disk_write_text emit requires drop-output-stream fn idx");
        codes.function(&emit_disk_write_text(
            wt.string_type_idx,
            wt.result_unit_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            open_at,
            write_via_stream,
            blocking_write,
            drop_descriptor,
            drop_output_stream,
            false, // is_append: writeText uses CREATE | TRUNCATE + write-via-stream
        ));
    }
    if let Some(at) = &disk_append_text {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_append_text emit requires disk_preopen_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_append_text emit requires cabi_realloc fn idx")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_append_text emit requires bridge")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_append_text emit requires get-directories fn idx");
        let open_at = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .expect("disk_append_text emit requires open-at fn idx");
        let append_via_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesAppendViaStream,
            )
            .expect("disk_append_text emit requires append-via-stream fn idx");
        let blocking_write = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
            )
            .expect("disk_append_text emit requires blocking-write-and-flush fn idx");
        let drop_descriptor = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .expect("disk_append_text emit requires drop-descriptor fn idx");
        let drop_output_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
            )
            .expect("disk_append_text emit requires drop-output-stream fn idx");
        codes.function(&emit_disk_write_text(
            at.string_type_idx,
            at.result_unit_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            open_at,
            append_via_stream,
            blocking_write,
            drop_descriptor,
            drop_output_stream,
            true, // is_append: CREATE only + append-via-stream (no offset arg)
        ));
    }
    // Three single-call ops share `emit_disk_simple_path_op` —
    // identical pipeline (preopen + path + 4-byte retptr + tag
    // check), only the wasi op fn idx and Err message differ.
    if let Some(d) = &disk_delete {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_delete emit requires disk_preopen_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_delete emit requires cabi_realloc fn idx")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_delete emit requires bridge")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_delete emit requires get-directories fn idx");
        let op_fn = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesUnlinkFileAt,
            )
            .expect("disk_delete emit requires unlink-file-at fn idx");
        codes.function(&emit_disk_simple_path_op(
            d.string_type_idx,
            d.result_unit_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            op_fn,
            b"delete failed",
        ));
    }
    if let Some(d) = &disk_delete_dir {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_delete_dir emit requires disk_preopen_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_delete_dir emit requires cabi_realloc fn idx")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_delete_dir emit requires bridge")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_delete_dir emit requires get-directories fn idx");
        let op_fn = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesRemoveDirectoryAt,
            )
            .expect("disk_delete_dir emit requires remove-directory-at fn idx");
        codes.function(&emit_disk_simple_path_op(
            d.string_type_idx,
            d.result_unit_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            op_fn,
            b"deleteDir failed",
        ));
    }
    if let Some(d) = &disk_make_dir {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_make_dir emit requires disk_preopen_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_make_dir emit requires cabi_realloc fn idx")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_make_dir emit requires bridge")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_make_dir emit requires get-directories fn idx");
        let op_fn = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesCreateDirectoryAt,
            )
            .expect("disk_make_dir emit requires create-directory-at fn idx");
        codes.function(&emit_disk_simple_path_op(
            d.string_type_idx,
            d.result_unit_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            op_fn,
            b"makeDir failed",
        ));
    }
    if let Some(ld) = &disk_list_dir {
        let preopen_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.disk_preopen_handle)
            .expect("disk_list_dir emit requires disk_preopen_handle global");
        let cabi = cabi_realloc
            .as_ref()
            .expect("disk_list_dir emit requires cabi_realloc fn idx")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("disk_list_dir emit requires bridge")
            .to_lm_fn;
        let get_directories = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemPreopensGetDirectories,
            )
            .expect("disk_list_dir emit requires get-directories fn idx");
        let open_at = wasip2_imports
            .lookup_wasm_fn_idx(super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesOpenAt)
            .expect("disk_list_dir emit requires open-at fn idx");
        let read_directory = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesReadDirectory,
            )
            .expect("disk_list_dir emit requires read-directory fn idx");
        let read_dir_entry = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesDirectoryEntryStreamReadDirectoryEntry,
            )
            .expect("disk_list_dir emit requires read-directory-entry fn idx");
        let drop_descriptor = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor,
            )
            .expect("disk_list_dir emit requires drop-descriptor fn idx");
        let drop_dir_stream = wasip2_imports
            .lookup_wasm_fn_idx(
                super::wasip2_imports::Wasip2ImportSlot::FilesystemTypesResourceDropDirectoryEntryStream,
            )
            .expect("disk_list_dir emit requires drop-directory-entry-stream fn idx");
        codes.function(&emit_disk_list_dir(
            ld.string_type_idx,
            ld.list_string_type_idx,
            ld.result_list_string_string_type_idx,
            preopen_global,
            cabi,
            str_to_lm,
            get_directories,
            open_at,
            read_directory,
            read_dir_entry,
            drop_descriptor,
            drop_dir_stream,
        ));
    }
    if let Some(hg) = &http_get {
        let cabi = cabi_realloc
            .as_ref()
            .expect("http_get emit requires cabi_realloc fn idx (gate matches)")
            .fn_idx;
        let str_to_lm = bridge
            .as_ref()
            .expect("http_get emit requires bridge (string marshalling)")
            .to_lm_fn;
        let lookup = |slot: super::wasip2_imports::Wasip2ImportSlot, name: &'static str| -> u32 {
            wasip2_imports
                .lookup_wasm_fn_idx(slot)
                .unwrap_or_else(|| panic!("http_get emit requires {name} fn idx"))
        };
        let helpers = super::wasip2_http::HttpGetHelperFns {
            cabi_realloc_fn: cabi,
            str_to_lm_fn: str_to_lm,
            fields_new_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesFieldsNew,
                "HttpTypesFieldsNew",
            ),
            outgoing_request_new_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestNew,
                "HttpTypesOutgoingRequestNew",
            ),
            set_scheme_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetScheme,
                "HttpTypesOutgoingRequestSetScheme",
            ),
            set_authority_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetAuthority,
                "HttpTypesOutgoingRequestSetAuthority",
            ),
            set_path_with_query_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetPathWithQuery,
                "HttpTypesOutgoingRequestSetPathWithQuery",
            ),
            handle_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpOutgoingHandlerHandle,
                "HttpOutgoingHandlerHandle",
            ),
            future_subscribe_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesFutureIncomingResponseSubscribe,
                "HttpTypesFutureIncomingResponseSubscribe",
            ),
            poll_fn: lookup(super::wasip2_imports::Wasip2ImportSlot::IoPollPoll, "IoPollPoll"),
            drop_pollable_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::IoPollResourceDropPollable,
                "IoPollResourceDropPollable",
            ),
            future_get_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesFutureIncomingResponseGet,
                "HttpTypesFutureIncomingResponseGet",
            ),
            status_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingResponseStatus,
                "HttpTypesIncomingResponseStatus",
            ),
            consume_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingResponseConsume,
                "HttpTypesIncomingResponseConsume",
            ),
            body_stream_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingBodyStream,
                "HttpTypesIncomingBodyStream",
            ),
            blocking_read_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::InputStreamBlockingRead,
                "InputStreamBlockingRead",
            ),
            body_finish_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingBodyFinish,
                "HttpTypesIncomingBodyFinish",
            ),
            drop_input_stream_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropInputStream,
                "IoStreamsResourceDropInputStream",
            ),
            drop_outgoing_request_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropOutgoingRequest,
                "HttpTypesResourceDropOutgoingRequest",
            ),
            drop_future_response_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropFutureIncomingResponse,
                "HttpTypesResourceDropFutureIncomingResponse",
            ),
            drop_incoming_response_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropIncomingResponse,
                "HttpTypesResourceDropIncomingResponse",
            ),
            drop_future_trailers_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropFutureTrailers,
                "HttpTypesResourceDropFutureTrailers",
            ),
            // Step F + G additions.
            drop_incoming_body_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropIncomingBody,
                "HttpTypesResourceDropIncomingBody",
            ),
            headers_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesIncomingResponseHeaders,
                "HttpTypesIncomingResponseHeaders",
            ),
            entries_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesFieldsEntries,
                "HttpTypesFieldsEntries",
            ),
            drop_fields_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropFields,
                "HttpTypesResourceDropFields",
            ),
            from_lm_fn: bridge
                .as_ref()
                .expect("http_get emit requires bridge (from_lm helper)")
                .from_lm_fn,
            map_set_fn: fn_map
                .map_helpers
                .get("Map<String,List<String>>")
                .expect("http_get emit requires Map<String,List<String>> helpers")
                .set,
            map_get_fn: fn_map
                .map_helpers
                .get("Map<String,List<String>>")
                .expect("http_get emit requires Map<String,List<String>> helpers")
                .get,
            set_method_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestSetMethod,
                "HttpTypesOutgoingRequestSetMethod",
            ),
            outgoing_request_body_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingRequestBody,
                "HttpTypesOutgoingRequestBody",
            ),
            outgoing_body_write_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingBodyWrite,
                "HttpTypesOutgoingBodyWrite",
            ),
            outgoing_body_finish_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesOutgoingBodyFinish,
                "HttpTypesOutgoingBodyFinish",
            ),
            fields_append_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesFieldsAppend,
                "HttpTypesFieldsAppend",
            ),
            drop_outgoing_body_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::HttpTypesResourceDropOutgoingBody,
                "HttpTypesResourceDropOutgoingBody",
            ),
            blocking_write_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
                "OutputStreamBlockingWriteAndFlush",
            ),
            drop_output_stream_fn: lookup(
                super::wasip2_imports::Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
                "IoStreamsResourceDropOutputStream",
            ),
        };
        codes.function(&super::wasip2_http::emit_http_get(hg, &helpers));
    }
    // Phase 4.2.1 (0.20) — stub `__rt_tcp_connect` body. Reads the
    // pre-registered "tcp: connect not yet implemented" data segment,
    // wraps it via `__rt_result_tcp_connection_string_err`, returns.
    // No wasi:sockets calls happen here yet; the real DNS / socket /
    // connect pipeline replaces this body in Phase 4.2.2+.
    if let Some(tc) = &tcp_connect {
        let result_err_fn = factory_exports
            .result_tcp_connection_string_err
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "tcp_connect emit requires \
                     __rt_result_tcp_connection_string_err factory slot"
                        .into(),
                )
            })?
            .fn_idx;
        let lookup = |slot: super::wasip2_imports::Wasip2ImportSlot, name: &'static str| -> Result<u32, WasmGcError> {
            wasip2_imports.lookup_wasm_fn_idx(slot).ok_or_else(|| {
                WasmGcError::Validation(format!("tcp_connect emit requires {name} fn idx"))
            })
        };
        let instance_network_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
            "SocketsInstanceNetworkInstanceNetwork",
        )?;
        let resolve_addresses_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses,
            "SocketsIpNameLookupResolveAddresses",
        )?;
        let drop_resolve_stream_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
            "SocketsIpNameLookupResourceDropResolveAddressStream",
        )?;
        let stream_subscribe_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
            "SocketsIpNameLookupResolveAddressStreamSubscribe",
        )?;
        let poll_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::IoPollPoll,
            "IoPollPoll",
        )?;
        let drop_pollable_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::IoPollResourceDropPollable,
            "IoPollResourceDropPollable",
        )?;
        let resolve_next_address_fn = lookup(
            super::wasip2_imports::Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress,
            "SocketsIpNameLookupResolveNextAddress",
        )?;
        let network_handle_global = wasip2_globals
            .as_ref()
            .and_then(|g| g.network_handle)
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "tcp_connect emit requires network_handle global \
                     (Phase 4.1b wireup gate)"
                        .into(),
                )
            })?;
        let cabi_realloc_fn = cabi_realloc
            .as_ref()
            .map(|c| c.fn_idx)
            .ok_or_else(|| {
                WasmGcError::Validation("tcp_connect emit requires cabi_realloc fn idx".into())
            })?;
        let str_to_lm_fn = bridge
            .as_ref()
            .map(|b| b.to_lm_fn)
            .ok_or_else(|| {
                WasmGcError::Validation(
                    "tcp_connect emit requires bridge (__rt_string_to_lm fn idx)".into(),
                )
            })?;
        let helpers = super::wasip2_tcp::TcpConnectHelperFns {
            result_err_fn,
            instance_network_fn,
            network_handle_global,
            cabi_realloc_fn,
            str_to_lm_fn,
            resolve_addresses_fn,
            drop_resolve_stream_fn,
            stream_subscribe_fn,
            poll_fn,
            drop_pollable_fn,
            resolve_next_address_fn,
        };
        codes.function(&super::wasip2_tcp::emit_tcp_connect_stub(tc, &helpers));
    }
    if let Some(e) = &env_get_lookup {
        codes.function(&emit_env_get_lookup(
            e.string_type_idx,
            e.option_string_type_idx,
        ));
    }
    if let Some(fmt) = &format_iso8601 {
        codes.function(&emit_format_iso8601(fmt.string_type_idx));
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
            descriptor: None,
            describes: None,
        },
    };
    let mk_array = |elem: wasm_encoder::FieldType| SubType {
        is_final: true,
        supertype_idx: None,
        composite_type: CompositeType {
            inner: CompositeInnerType::Array(ArrayType(elem)),
            shared: false,
            descriptor: None,
            describes: None,
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

    // Phase 4 (0.20) — TCP connection pool slot type + array type.
    // `$tcp_slot` carries four mutable i32 handles per connection
    // (socket / in_stream / out_stream / in_use); `$tcp_pool` is the
    // `(array (mut $tcp_slot))` that holds 256 of them. The slot
    // struct is emitted first so the array element type can name it
    // by index without a forward reference.
    if let Some(slot_idx) = registry.tcp_slot_type_idx {
        entries.push((
            slot_idx,
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
                    element_type: wasm_encoder::StorageType::Val(ValType::I32),
                    mutable: true,
                },
                wasm_encoder::FieldType {
                    element_type: wasm_encoder::StorageType::Val(ValType::I32),
                    mutable: true,
                },
            ]),
        ));
    }
    if let Some(pool_idx) = registry.tcp_pool_type_idx {
        let slot_idx = registry
            .tcp_slot_type_idx
            .expect("tcp_pool_type_idx allocated without matching tcp_slot_type_idx");
        let slot_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
            nullable: true,
            heap_type: wasm_encoder::HeapType::Concrete(slot_idx),
        });
        entries.push((
            pool_idx,
            mk_array(wasm_encoder::FieldType {
                element_type: wasm_encoder::StorageType::Val(slot_ref),
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
    //
    // Skip names the user already redeclared with `record <Name>` —
    // the items pass above pushed their entry with the same registry
    // idx, so a second push from the builtin pass would duplicate the
    // struct in the rec group and slide every subsequent type index
    // up by one. Validator then complains "type index N is not a
    // function type" because effect imports / fn types end up
    // referring to the duplicated user-record slots instead of the
    // function-type slots they reserved.
    let user_record_names: std::collections::HashSet<&str> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::TypeDef(TypeDef::Product { name, .. }) => Some(name.as_str()),
            _ => None,
        })
        .collect();
    for record in crate::codegen::builtin_records::BUILTIN_RECORDS {
        if !registry.records.contains_key(record.aver_name) {
            continue;
        }
        if user_record_names.contains(record.aver_name) {
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
                // `Int.mod` lowers to a per-site `i64.rem_s` plus a
                // proxied `__int_mod_euclid` Call to fold the negative
                // result back into `[0, |b|)`. The helper isn't a
                // surface builtin (no `from_dotted` mapping); register
                // explicitly here whenever discovery hits an `Int.mod`
                // call. Both the unfused Result-wrap shape and the
                // fused `Result.withDefault(Int.mod(...), default)`
                // shape need it.
                if dotted == "Int.mod" {
                    builtins.register(BuiltinName::IntModEuclid);
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
        // part may also need `String.fromInt` (if its type is Int) —
        // we conservatively register that too; unused registrations
        // are stripped by `wasm-opt -Oz`.
        Expr::InterpolatedStr(parts) => {
            // Variadic concat is mandatory; the per-type stringifiers
            // are registered conservatively whenever interpolation
            // exists in the program — unused registrations get DCE'd
            // by `wasm-opt -Oz`. Cheaper than a per-part type-driven
            // walk.
            builtins.register(BuiltinName::StringConcatN);
            builtins.register(BuiltinName::StringFromInt);
            builtins.register(BuiltinName::StringFromFloat);
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
    caller_fn_idx: u32,
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

    // Build HttpRequest from host effects. Each effect import has a
    // trailing `caller_fn_idx: i32` per the wasm-gc ABI (every host
    // import gained this in 0.16 for record/replay attribution); push
    // the wrapper's reserved idx as that arg.
    let push_caller = |f: &mut Function| {
        f.instruction(&Instruction::I32Const(caller_fn_idx as i32));
    };
    push_caller(&mut f);
    f.instruction(&Instruction::Call(request_method_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(0));
    push_caller(&mut f);
    f.instruction(&Instruction::Call(request_url_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(1));
    push_caller(&mut f);
    f.instruction(&Instruction::Call(request_query_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(2));
    push_caller(&mut f);
    f.instruction(&Instruction::Call(request_body_fn));
    f.instruction(&Instruction::RefCastNullable(HeapType::Concrete(s_idx)));
    f.instruction(&Instruction::LocalSet(3));
    push_caller(&mut f);
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
    // response_set_header(key, list.head, caller_fn_idx)
    f.instruction(&Instruction::LocalGet(14));
    f.instruction(&Instruction::LocalGet(15));
    f.instruction(&Instruction::StructGet {
        struct_type_index: list_idx,
        field_index: 0,
    });
    push_caller(&mut f);
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

    // response_text(status, body, caller_fn_idx)
    f.instruction(&Instruction::LocalGet(7));
    f.instruction(&Instruction::LocalGet(8));
    push_caller(&mut f);
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
