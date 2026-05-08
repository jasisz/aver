//! Canonical-ABI import registry for `--target wasip2`.
//!
//! Mirrors the shape of `EffectRegistry` but speaks Component-Model
//! canonical-ABI names instead of `aver/*` host-bridge names. The
//! two registries coexist: when `target == TargetMode::Wasip2`,
//! the wasm-gc emitter populates this registry from the discovered
//! `EffectName`s (via `EffectName::lowers_on_wasip2`) and the
//! import-section emit branch in `module.rs` reads from THIS
//! registry instead of the `EffectRegistry`'s `import_pair()`.
//!
//! Why a separate registry: one Aver effect (`Console.print`) lowers
//! to MULTIPLE wasip2 imports (cache-stdout-handle + write-bytes),
//! so the existing 1-effect → 1-import shape in `EffectName`
//! cannot retrofit. See the plan in
//! `~/.claude-personal/plans/zaplanujmy-sobie-adnie-to-snug-rabin.md`.
//!
//! Phase 1.2b1.2 wires the registry skeleton + the import-section
//! branch. The slots themselves get exercised in Phase 1.2b1.5
//! when the call-site lowering for Console.print/error/warn lands.
//! Until then, programs that touch any wasip2-relevant effect are
//! still rejected upstream by `wasip2::effect_check`.

use std::collections::HashMap;

use wasm_encoder::ValType;

/// One canonical-ABI import the Phase 1.2b1 wasip2 path may need.
///
/// Canonical core wasm import names (validated against
/// `wasip2-1.0.1+wasi-0.2.4` bindgen output and
/// `wit-component-0.248.0/tests/components/`):
///
/// - module = the WIT interface qualified name including version
///   (`"wasi:cli/stdout@0.2.4"`, `"wasi:io/streams@0.2.4"`);
/// - field for free fns = kebab-case WIT name (`"get-stdout"`);
/// - field for resource methods = `"[method]<resource>.<method>"`
///   (`"[method]output-stream.blocking-write-and-flush"`);
/// - field for resource drops = `"[resource-drop]<resource>"`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Wasip2ImportSlot {
    /// `wasi:cli/stdout.get-stdout: func() -> output-stream`.
    /// Canonical-ABI signature: `() -> i32` (the resource handle).
    CliGetStdout,
    /// `wasi:cli/stderr.get-stderr: func() -> output-stream`.
    /// Canonical-ABI signature: `() -> i32`.
    CliGetStderr,
    /// `wasi:io/streams.[method]output-stream.blocking-write-and-flush:
    /// func(contents: list<u8>) -> result<_, stream-error>`.
    ///
    /// Canonical-ABI signature with the `result<_, stream-error>` lowered
    /// via retptr (host writes 12 bytes at retptr):
    ///   `(handle: i32, buf_ptr: i32, buf_len: i32, retptr: i32)`.
    /// Phase 1.2b1 ignores the retptr contents — Aver `Console.print`
    /// is `Unit`, matching the wasm-gc target's fire-and-forget
    /// semantics.
    OutputStreamBlockingWriteAndFlush,
    /// `wasi:clocks/wall-clock.now: func() -> datetime` where
    /// `datetime = record { seconds: u64, nanoseconds: u32 }`.
    ///
    /// Canonical-ABI signature with the `datetime` record lowered via
    /// retptr (16 bytes — u64 at retptr+0, u32 at retptr+8, 4 bytes
    /// of padding):
    ///   `(retptr: i32) -> ()`.
    /// Phase 1.4 drives `Time.unixMs` (computed in guest as
    /// `seconds * 1000 + nanoseconds / 1_000_000`).
    ClocksWallClockNow,
    /// `wasi:random/random.get-random-u64: func() -> u64`.
    /// Canonical-ABI signature: inline 64-bit return, `() -> i64`.
    /// Phase 1.4 drives both `Random.int(min, max)` (modulo + offset)
    /// and `Random.float()` (53-bit precision scale to `[0.0, 1.0)`).
    RandomGetRandomU64,
    /// `wasi:cli/environment.get-arguments: func() -> list<string>`.
    /// Canonical-ABI signature: list-returning, lowered via retptr.
    /// Host calls `cabi_realloc` to allocate the list backing bytes
    /// in guest memory, then writes `(ptr: i32, len: i32)` (8 bytes
    /// at the guest-supplied retptr). Each list entry is itself a
    /// string lowered as `(ptr: i32, len: i32)` — 8 bytes per entry
    /// — packed contiguously starting at `ptr`. Phase 1.3.2 drives
    /// `Args.get() -> List<String>` (the no-args user-facing form).
    CliEnvironmentGetArguments,
    /// `wasi:cli/environment.get-environment: func() ->
    /// list<tuple<string, string>>`. Canonical-ABI signature:
    /// list-returning via retptr (8 bytes: list_ptr + list_len);
    /// each entry is a flattened tuple — 16 bytes packed
    /// `(key_ptr i32, key_len i32, val_ptr i32, val_len i32)`.
    /// Phase 1.3.3 drives `Env.get(name) -> String` via a
    /// linear-search lookup helper.
    CliEnvironmentGetEnvironment,
    /// `wasi:cli/stdin.get-stdin: func() -> input-stream`.
    /// Returns the program-lifetime stdin resource handle;
    /// Phase 1.3.4 caches it in a wasm global (lazy lookup,
    /// never dropped — wasmtime cleans up at component exit).
    /// Canonical-ABI signature: `() -> i32`.
    CliStdinGetStdin,
    /// `wasi:io/streams.[method]input-stream.blocking-read:
    /// func(this: borrow<input-stream>, len: u64) ->
    ///   result<list<u8>, stream-error>`.
    ///
    /// Canonical-ABI signature with the result lowered via retptr
    /// (12 bytes — `tag i8` at offset 0, then either
    /// `(data_ptr i32, data_len i32)` for Ok or
    /// `(err_tag i8, err_handle i32)` for Err):
    ///   `(handle: i32, len: i64, retptr: i32) -> ()`.
    /// Phase 1.3.4 drives `Console.readLine() ->
    /// Result<String, String>` by looping `len = 1` reads until
    /// `\n` or EOF and accumulating bytes into a `cabi_realloc`-
    /// owned buffer.
    InputStreamBlockingRead,
    /// `wasi:clocks/monotonic-clock.subscribe-duration:
    /// func(when: duration) -> pollable` where
    /// `type duration = u64` (nanoseconds).
    ///
    /// Returns a fresh `pollable` resource handle that becomes
    /// "ready" after the requested duration elapses on the
    /// host's monotonic clock. Phase 1.4c uses it to back
    /// `Time.sleep(ms)` — the pollable is short-lived (one
    /// allocation + one poll + drop, all inside the helper),
    /// so the resource lifecycle is per-call, not program-life.
    /// Canonical-ABI signature: `(when: i64) -> i32`.
    ClocksMonotonicSubscribeDuration,
    /// `wasi:io/poll.poll: func(in: list<borrow<pollable>>) ->
    /// list<u32>` — the synchronous wait primitive of WASI 0.2.
    /// Blocks until at least one of the supplied pollables is
    /// ready, returns the indices that became ready.
    ///
    /// Canonical-ABI signature: `in` lowers to `(in_ptr i32,
    /// in_len i32)` (a contiguous list of pollable handles in
    /// LM); the result `list<u32>` lowers via retptr (8 bytes:
    /// `(out_ptr i32, out_len i32)` — the host calls
    /// `cabi_realloc` to allocate the indices buffer). Phase 1.4c
    /// `Time.sleep` ignores the returned indices (the only
    /// pollable in `in` is the duration timer; "ready" is the
    /// only outcome we care about).
    ///   `(in_ptr: i32, in_len: i32, retptr: i32) -> ()`.
    IoPollPoll,
    /// `wasi:io/poll.[resource-drop]pollable: func(this:
    /// pollable) -> ()`. Releases a pollable handle. Phase 1.4c
    /// `Time.sleep` calls this once per invocation — the pollable
    /// returned by `subscribe-duration` is single-use, so leaving
    /// it would leak host-side resources at the rate of one per
    /// sleep call.
    /// Canonical-ABI signature: `(handle: i32) -> ()`.
    IoPollResourceDropPollable,
    /// `wasi:filesystem/preopens.get-directories: func() ->
    /// list<tuple<descriptor, string>>`.
    ///
    /// Returns the program's preopened directories — the host
    /// configures these before instantiation (e.g. wasmtime CLI's
    /// `--dir`, our embedded runner preopens `.` so guest paths
    /// resolve against host CWD). Each tuple is 12 bytes packed
    /// `(descriptor i32, path_ptr i32, path_len i32)`. Phase 1.5
    /// `Disk.*` use the FIRST entry's descriptor as the resolution
    /// root and ignore the path string (CWD-relative).
    /// Canonical-ABI signature: `(retptr: i32) -> ()`.
    FilesystemPreopensGetDirectories,
    /// `wasi:filesystem/types.[method]descriptor.stat-at: func(
    ///   this: borrow<descriptor>, path-flags: path-flags,
    ///   path: string) -> result<descriptor-stat, error-code>`.
    ///
    /// Phase 1.5.1 `Disk.exists` uses this purely to check the
    /// result's tag — `Ok` ⇒ file exists, `Err` ⇒ doesn't. The
    /// `descriptor-stat` payload (size, timestamps, link count,
    /// etc.) is left untouched in the retptr buffer. retptr is
    /// 96 bytes (8-byte tag + alignment + 80-byte
    /// descriptor-stat). `path-flags = 1` (symlink-follow) so we
    /// follow symlinks like POSIX `stat`.
    /// Canonical-ABI signature:
    ///   `(handle: i32, path_flags: i32, path_ptr: i32,
    ///    path_len: i32, retptr: i32) -> ()`.
    FilesystemTypesStatAt,
    /// `wasi:filesystem/types.[method]descriptor.open-at: func(
    ///   this: borrow<descriptor>,
    ///   path-flags: path-flags,
    ///   path: string,
    ///   open-flags: open-flags,
    ///   flags: descriptor-flags,
    /// ) -> result<descriptor, error-code>`.
    ///
    /// Phase 1.5.2 `Disk.readText` uses this to open a file
    /// relative to the cached preopen descriptor; on Ok the
    /// freshly-opened descriptor handle lands at retptr+4. retptr
    /// size is 8 bytes (`tag i8` padded to 4 + descriptor handle
    /// i32). The Err branch carries an `error-code` u8 at
    /// retptr+4 — Phase 1.5.2 ignores its specific value and
    /// reports a generic "open failed".
    /// Canonical-ABI signature:
    ///   `(handle: i32, path_flags: i32, path_ptr: i32,
    ///    path_len: i32, open_flags: i32, descriptor_flags: i32,
    ///    retptr: i32) -> ()`.
    FilesystemTypesOpenAt,
    /// `wasi:filesystem/types.[method]descriptor.read-via-stream:
    ///   func(this: borrow<descriptor>, offset: filesize)
    ///   -> result<input-stream, error-code>`.
    ///
    /// Phase 1.5.2 calls this with `offset = 0` to obtain a fresh
    /// input-stream over the whole file, then loops
    /// `[method]input-stream.blocking-read` until EOF. retptr
    /// shape is identical to open-at's: 8 bytes (`tag i8` + i32
    /// handle on Ok / `error-code u8` on Err).
    /// Canonical-ABI signature:
    ///   `(handle: i32, offset: i64, retptr: i32) -> ()`.
    FilesystemTypesReadViaStream,
    /// `wasi:filesystem/types.[resource-drop]descriptor:
    ///   func(this: descriptor) -> ()`. Releases a file
    /// descriptor handle. Phase 1.5.2 calls this once per
    /// `Disk.readText` invocation for the per-call file
    /// descriptor (NOT for the cached preopen — that one is
    /// program-lifetime and freed by wasmtime at component exit).
    /// Canonical-ABI signature: `(handle: i32) -> ()`.
    FilesystemTypesResourceDropDescriptor,
    /// `wasi:io/streams.[resource-drop]input-stream:
    ///   func(this: input-stream) -> ()`. Releases a stream
    /// handle. Phase 1.5.2 calls this once per `Disk.readText`
    /// for the per-call read stream (the `Console.readLine`
    /// stdin stream is program-lifetime and never dropped).
    /// Canonical-ABI signature: `(handle: i32) -> ()`.
    IoStreamsResourceDropInputStream,
    /// `wasi:filesystem/types.[method]descriptor.write-via-stream:
    ///   func(this: borrow<descriptor>, offset: filesize)
    ///   -> result<output-stream, error-code>`.
    ///
    /// Phase 1.5.3 calls this with `offset = 0` to obtain an
    /// output-stream over an open file (created via `open-at`
    /// with `create | truncate` for `writeText`, or via
    /// `append` flag for `appendText`). retptr layout matches
    /// `read-via-stream`: 8 bytes (`tag i8` + handle/error i32).
    /// Canonical-ABI signature:
    ///   `(handle: i32, offset: i64, retptr: i32) -> ()`.
    FilesystemTypesWriteViaStream,
    /// `wasi:io/streams.[resource-drop]output-stream:
    ///   func(this: output-stream) -> ()`. Releases an
    /// output-stream handle. Phase 1.5.3 calls this once per
    /// `Disk.writeText` for the per-call write stream (the
    /// `Console.print` stdout stream is program-lifetime and
    /// never dropped). Canonical-ABI signature:
    ///   `(handle: i32) -> ()`.
    IoStreamsResourceDropOutputStream,
    /// `wasi:filesystem/types.[method]descriptor.unlink-file-at:
    ///   func(this: borrow<descriptor>, path: string)
    ///   -> result<_, error-code>`. Backs `Disk.delete` (Phase
    /// 1.5.4). Canonical-ABI signature:
    ///   `(handle: i32, path_ptr: i32, path_len: i32,
    ///    retptr: i32) -> ()`. retptr is 4 bytes — `tag` at
    /// offset 0 plus `error-code` at offset 1 on Err.
    FilesystemTypesUnlinkFileAt,
    /// `wasi:filesystem/types.[method]descriptor.remove-directory-at`
    /// — same shape as unlink-file-at; backs `Disk.deleteDir`.
    FilesystemTypesRemoveDirectoryAt,
    /// `wasi:filesystem/types.[method]descriptor.create-directory-at`
    /// — same shape as unlink-file-at; backs `Disk.makeDir`.
    FilesystemTypesCreateDirectoryAt,
    /// `wasi:filesystem/types.[method]descriptor.append-via-stream:
    ///   func(this: borrow<descriptor>) -> result<output-stream,
    ///     error-code>`. Same retptr shape as `write-via-stream`,
    /// no offset arg (the host appends at end-of-file). Backs
    /// `Disk.appendText` (Phase 1.5.5).
    /// Canonical-ABI signature:
    ///   `(handle: i32, retptr: i32) -> ()`.
    FilesystemTypesAppendViaStream,
}

impl Wasip2ImportSlot {
    /// Canonical core wasm `(module, field)` pair this slot imports.
    /// `wit_component::ComponentEncoder` matches against these names
    /// when binding the component's WIT-typed imports to the core
    /// module's plain wasm imports.
    pub(super) fn module_field_pair(self) -> (&'static str, &'static str) {
        match self {
            Wasip2ImportSlot::CliGetStdout => ("wasi:cli/stdout@0.2.4", "get-stdout"),
            Wasip2ImportSlot::CliGetStderr => ("wasi:cli/stderr@0.2.4", "get-stderr"),
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush => (
                "wasi:io/streams@0.2.4",
                "[method]output-stream.blocking-write-and-flush",
            ),
            Wasip2ImportSlot::ClocksWallClockNow => ("wasi:clocks/wall-clock@0.2.4", "now"),
            Wasip2ImportSlot::RandomGetRandomU64 => {
                ("wasi:random/random@0.2.4", "get-random-u64")
            }
            Wasip2ImportSlot::CliEnvironmentGetArguments => {
                ("wasi:cli/environment@0.2.4", "get-arguments")
            }
            Wasip2ImportSlot::CliEnvironmentGetEnvironment => {
                ("wasi:cli/environment@0.2.4", "get-environment")
            }
            Wasip2ImportSlot::CliStdinGetStdin => ("wasi:cli/stdin@0.2.4", "get-stdin"),
            Wasip2ImportSlot::InputStreamBlockingRead => (
                "wasi:io/streams@0.2.4",
                "[method]input-stream.blocking-read",
            ),
            Wasip2ImportSlot::ClocksMonotonicSubscribeDuration => {
                ("wasi:clocks/monotonic-clock@0.2.4", "subscribe-duration")
            }
            Wasip2ImportSlot::IoPollPoll => ("wasi:io/poll@0.2.4", "poll"),
            Wasip2ImportSlot::IoPollResourceDropPollable => {
                ("wasi:io/poll@0.2.4", "[resource-drop]pollable")
            }
            Wasip2ImportSlot::FilesystemPreopensGetDirectories => {
                ("wasi:filesystem/preopens@0.2.4", "get-directories")
            }
            Wasip2ImportSlot::FilesystemTypesStatAt => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.stat-at",
            ),
            Wasip2ImportSlot::FilesystemTypesOpenAt => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.open-at",
            ),
            Wasip2ImportSlot::FilesystemTypesReadViaStream => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.read-via-stream",
            ),
            Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor => {
                ("wasi:filesystem/types@0.2.4", "[resource-drop]descriptor")
            }
            Wasip2ImportSlot::IoStreamsResourceDropInputStream => {
                ("wasi:io/streams@0.2.4", "[resource-drop]input-stream")
            }
            Wasip2ImportSlot::FilesystemTypesWriteViaStream => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.write-via-stream",
            ),
            Wasip2ImportSlot::IoStreamsResourceDropOutputStream => {
                ("wasi:io/streams@0.2.4", "[resource-drop]output-stream")
            }
            Wasip2ImportSlot::FilesystemTypesUnlinkFileAt => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.unlink-file-at",
            ),
            Wasip2ImportSlot::FilesystemTypesRemoveDirectoryAt => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.remove-directory-at",
            ),
            Wasip2ImportSlot::FilesystemTypesCreateDirectoryAt => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.create-directory-at",
            ),
            Wasip2ImportSlot::FilesystemTypesAppendViaStream => (
                "wasi:filesystem/types@0.2.4",
                "[method]descriptor.append-via-stream",
            ),
        }
    }

    pub(super) fn params(self) -> Vec<ValType> {
        match self {
            Wasip2ImportSlot::CliGetStdout
            | Wasip2ImportSlot::CliGetStderr
            | Wasip2ImportSlot::CliStdinGetStdin
            | Wasip2ImportSlot::RandomGetRandomU64 => Vec::new(),
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush => {
                vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32]
            }
            // `blocking-read(this, len) -> result<list<u8>, stream-error>`
            // — `this` borrows the input-stream handle (i32), `len` is
            // u64, return lowered via retptr.
            Wasip2ImportSlot::InputStreamBlockingRead => {
                vec![ValType::I32, ValType::I64, ValType::I32]
            }
            // `now: () -> datetime` — datetime exceeds 8-byte flat
            // limit, so it returns via retptr supplied by the guest.
            Wasip2ImportSlot::ClocksWallClockNow => vec![ValType::I32],
            // `get-arguments: () -> list<string>` — list lowered via
            // retptr (8 bytes: list_ptr i32 + list_len i32). Host
            // also calls `cabi_realloc` to allocate the backing
            // bytes and per-string utf-8 buffers.
            // Same retptr-only param shape for `get-environment`
            // (the per-entry layout differs but the import takes
            // a single retptr regardless).
            Wasip2ImportSlot::CliEnvironmentGetArguments
            | Wasip2ImportSlot::CliEnvironmentGetEnvironment => vec![ValType::I32],
            // `subscribe-duration(when: duration) -> pollable` —
            // `duration` = u64 nanoseconds (i64 in flat lowering),
            // returns the pollable handle inline (i32, fits flat).
            Wasip2ImportSlot::ClocksMonotonicSubscribeDuration => vec![ValType::I64],
            // `poll(in: list<borrow<pollable>>) -> list<u32>` —
            // `in` lowered as (ptr, len), result via retptr.
            Wasip2ImportSlot::IoPollPoll => {
                vec![ValType::I32, ValType::I32, ValType::I32]
            }
            // `[resource-drop]pollable(this: pollable)` — single
            // i32 handle, no return.
            Wasip2ImportSlot::IoPollResourceDropPollable => vec![ValType::I32],
            // `get-directories: () -> list<...>` — list lowered
            // via retptr (8 bytes: list_ptr + list_len).
            Wasip2ImportSlot::FilesystemPreopensGetDirectories => vec![ValType::I32],
            // `stat-at(this, path-flags, path) -> result<...>` —
            // borrow<descriptor> + flags i32 + string (ptr,len) +
            // retptr for the result.
            Wasip2ImportSlot::FilesystemTypesStatAt => vec![
                ValType::I32, // descriptor handle
                ValType::I32, // path-flags
                ValType::I32, // path_ptr
                ValType::I32, // path_len
                ValType::I32, // retptr
            ],
            // `open-at(this, path-flags, path, open-flags, flags)
            // -> result<descriptor, error-code>` — same string
            // lowering as stat-at plus two flag i32s.
            Wasip2ImportSlot::FilesystemTypesOpenAt => vec![
                ValType::I32, // descriptor handle
                ValType::I32, // path-flags
                ValType::I32, // path_ptr
                ValType::I32, // path_len
                ValType::I32, // open-flags
                ValType::I32, // descriptor-flags
                ValType::I32, // retptr
            ],
            // `read-via-stream(this, offset) ->
            // result<input-stream, error-code>` — borrow<descriptor>,
            // u64 offset, retptr for the 8-byte result.
            Wasip2ImportSlot::FilesystemTypesReadViaStream => vec![
                ValType::I32, // descriptor handle
                ValType::I64, // offset
                ValType::I32, // retptr
            ],
            // `[resource-drop]<X>(this)` — single i32 handle,
            // no return.
            Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor
            | Wasip2ImportSlot::IoStreamsResourceDropInputStream
            | Wasip2ImportSlot::IoStreamsResourceDropOutputStream => vec![ValType::I32],
            // `write-via-stream(this, offset) ->
            // result<output-stream, error-code>` — same shape as
            // read-via-stream.
            Wasip2ImportSlot::FilesystemTypesWriteViaStream => vec![
                ValType::I32, // descriptor handle
                ValType::I64, // offset
                ValType::I32, // retptr
            ],
            // `unlink-file-at(this, path) -> result<_, error-code>`,
            // shape shared with remove-directory-at and
            // create-directory-at.
            Wasip2ImportSlot::FilesystemTypesUnlinkFileAt
            | Wasip2ImportSlot::FilesystemTypesRemoveDirectoryAt
            | Wasip2ImportSlot::FilesystemTypesCreateDirectoryAt => vec![
                ValType::I32, // descriptor handle
                ValType::I32, // path_ptr
                ValType::I32, // path_len
                ValType::I32, // retptr
            ],
            // `append-via-stream(this) -> result<output-stream, _>`
            // — no offset, retptr only.
            Wasip2ImportSlot::FilesystemTypesAppendViaStream => {
                vec![ValType::I32, ValType::I32]
            }
        }
    }

    pub(super) fn results(self) -> Vec<ValType> {
        match self {
            // Resource handle — i32 ID owned by the host.
            Wasip2ImportSlot::CliGetStdout
            | Wasip2ImportSlot::CliGetStderr
            | Wasip2ImportSlot::CliStdinGetStdin
            | Wasip2ImportSlot::ClocksMonotonicSubscribeDuration => vec![ValType::I32],
            // Result lowered via retptr — no inline return.
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush
            | Wasip2ImportSlot::InputStreamBlockingRead
            | Wasip2ImportSlot::IoPollPoll
            | Wasip2ImportSlot::IoPollResourceDropPollable
            | Wasip2ImportSlot::ClocksWallClockNow
            | Wasip2ImportSlot::CliEnvironmentGetArguments
            | Wasip2ImportSlot::CliEnvironmentGetEnvironment
            | Wasip2ImportSlot::FilesystemPreopensGetDirectories
            | Wasip2ImportSlot::FilesystemTypesStatAt
            | Wasip2ImportSlot::FilesystemTypesOpenAt
            | Wasip2ImportSlot::FilesystemTypesReadViaStream
            | Wasip2ImportSlot::FilesystemTypesResourceDropDescriptor
            | Wasip2ImportSlot::IoStreamsResourceDropInputStream
            | Wasip2ImportSlot::FilesystemTypesWriteViaStream
            | Wasip2ImportSlot::IoStreamsResourceDropOutputStream
            | Wasip2ImportSlot::FilesystemTypesUnlinkFileAt
            | Wasip2ImportSlot::FilesystemTypesRemoveDirectoryAt
            | Wasip2ImportSlot::FilesystemTypesCreateDirectoryAt
            | Wasip2ImportSlot::FilesystemTypesAppendViaStream => Vec::new(),
            // u64 return — fits in flat representation, no retptr.
            Wasip2ImportSlot::RandomGetRandomU64 => vec![ValType::I64],
        }
    }
}

/// Per-program registry of canonical-ABI imports the wasip2 emit
/// path declares. Mirrors `EffectRegistry`'s shape:
/// - `order` is the deterministic insertion sequence (also the
///   wasm fn idx assignment order — slots take fn idx `0..K`),
/// - `wasm_fn_idx` / `wasm_type_idx` are populated by `assign_slots`
///   once the type-section has run far enough to allocate slots.
#[derive(Default)]
pub(super) struct Wasip2ImportRegistry {
    order: Vec<Wasip2ImportSlot>,
    wasm_fn_idx: HashMap<Wasip2ImportSlot, u32>,
    wasm_type_idx: HashMap<Wasip2ImportSlot, u32>,
}

impl Wasip2ImportRegistry {
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Idempotent. Order of first registration is preserved.
    pub(super) fn register(&mut self, slot: Wasip2ImportSlot) {
        if !self.order.contains(&slot) {
            self.order.push(slot);
        }
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = Wasip2ImportSlot> + '_ {
        self.order.iter().copied()
    }

    pub(super) fn import_count(&self) -> u32 {
        self.order.len() as u32
    }

    /// Reserve type and fn-idx slots for each registered import.
    /// Called from `module.rs` once the type-section counter has
    /// advanced past user types but BEFORE user-fn types are
    /// allocated — wasip2 imports occupy fn idx `0..K`, exactly
    /// where `EffectRegistry` would have allocated `aver/*` imports
    /// on the AverBridge target.
    pub(super) fn assign_slots(&mut self, next_type_idx: &mut u32) {
        for (i, slot) in self.order.iter().copied().enumerate() {
            self.wasm_fn_idx.insert(slot, i as u32);
            self.wasm_type_idx.insert(slot, *next_type_idx);
            *next_type_idx += 1;
        }
    }

    /// Used by Phase 1.2b1.5 call-site lowering — kept ahead of the
    /// commit that consumes it so the registry shape is complete.
    #[allow(dead_code)]
    pub(super) fn lookup_wasm_fn_idx(&self, slot: Wasip2ImportSlot) -> Option<u32> {
        self.wasm_fn_idx.get(&slot).copied()
    }

    pub(super) fn lookup_wasm_type_idx(&self, slot: Wasip2ImportSlot) -> Option<u32> {
        self.wasm_type_idx.get(&slot).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn module_field_pair_matches_canonical_abi_names() {
        // Validated against
        //   ~/.cargo/registry/src/.../wasip2-1.0.1+wasi-0.2.4/src/imports.rs
        // and
        //   ~/.cargo/registry/src/.../wit-component-0.248.0/tests/
        //     components/adapt-stub-wasip2/module.wat
        // — these names are what `wit_component::ComponentEncoder`
        // matches against at component-build time.
        assert_eq!(
            Wasip2ImportSlot::CliGetStdout.module_field_pair(),
            ("wasi:cli/stdout@0.2.4", "get-stdout"),
        );
        assert_eq!(
            Wasip2ImportSlot::CliGetStderr.module_field_pair(),
            ("wasi:cli/stderr@0.2.4", "get-stderr"),
        );
        assert_eq!(
            Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush.module_field_pair(),
            (
                "wasi:io/streams@0.2.4",
                "[method]output-stream.blocking-write-and-flush",
            ),
        );
    }

    #[test]
    fn registry_assigns_slots_in_order() {
        let mut r = Wasip2ImportRegistry::new();
        r.register(Wasip2ImportSlot::CliGetStdout);
        r.register(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush);
        // Idempotent — second register of the same slot is a no-op.
        r.register(Wasip2ImportSlot::CliGetStdout);
        assert_eq!(r.import_count(), 2);

        let mut next_type_idx: u32 = 100;
        r.assign_slots(&mut next_type_idx);
        assert_eq!(next_type_idx, 102);

        assert_eq!(
            r.lookup_wasm_fn_idx(Wasip2ImportSlot::CliGetStdout),
            Some(0)
        );
        assert_eq!(
            r.lookup_wasm_fn_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush),
            Some(1)
        );
        assert_eq!(
            r.lookup_wasm_type_idx(Wasip2ImportSlot::CliGetStdout),
            Some(100)
        );
        assert_eq!(
            r.lookup_wasm_type_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush),
            Some(101)
        );
    }
}
