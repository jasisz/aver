//! `__rt_tcp_connect` — the big one. DNS resolve → create-socket
//! → start/finish-connect → pool slot allocate → Tcp.Connection
//! materialise pipeline. See the per-step comments inline for the
//! phase 4.2.2a–4.2.2f breakdown.

use wasm_encoder::{Function, Instruction, ValType};

use super::restore_bump;

/// Slot bundle for `__rt_tcp_connect`. Mirrors `HttpGetIndices`'s
/// shape — type idxs + helper fn idxs the body needs to resolve.
/// Phase 4.2.2a adds the network-handle global so the prolog can
/// lazy-init it; the stub body otherwise still ignores host / port.
pub(in crate::codegen::wasm_gc) struct TcpConnectIndices {
    /// `(param (ref null $string)) (param i64) (result (ref null
    /// $result_tcp_connection_string))` function type idx.
    pub fn_type: u32,
    /// Allocated wasm fn idx — call-site emitters reference this
    /// when lowering `Tcp.connect`.
    pub fn_idx: u32,
    /// Wasm-gc type idx for `(array i8)` String repr — needed by
    /// the stub body to materialise the placeholder error message.
    pub string_type_idx: u32,
    /// Passive data segment idx carrying the bytes
    /// `"tcp: connect not yet implemented"`. The stub builds an Aver
    /// String from this via `array.new_data`. Replaced with the
    /// real per-failure-mode segments in Phase 4.2.2+.
    pub stub_err_segment_idx: u32,
    /// Length of the placeholder error bytes.
    pub stub_err_len: u32,
    /// Phase 4.2.2b — passive data segment + length for the bytes
    /// `"tcp: dns resolve failed"`. Returned via Result.Err when
    /// `resolve-addresses` itself fails (host syntactically invalid,
    /// resolver permanent failure). Per-error-code dispatch lands
    /// in a follow-up — the v1 generic message keeps the body small.
    pub dns_err_segment_idx: u32,
    pub dns_err_len: u32,
    /// Phase 4.2.2b2 — `"tcp: dns no addresses"`. Returned when the
    /// resolver finished without yielding any IPv4 address: either
    /// `resolve-next-address` Err mid-stream, or the stream
    /// exhausted (`None`) before an IPv4 surfaced. IPv6-only hosts
    /// fall into this bucket today — IPv6 support waits on the
    /// full ip-socket-address lowering (out of 0.20 scope).
    pub no_addr_segment_idx: u32,
    pub no_addr_len: u32,
    /// Phase 4.2.2c — `"tcp: socket create failed"`. `create-tcp-
    /// socket(ipv4)` Err (system limit, AF unsupported on host).
    pub sock_err_segment_idx: u32,
    pub sock_err_len: u32,
    /// Phase 4.2.2c — `"tcp: connect failed"`. Covers both
    /// `start-connect` Err (invalid address) and `finish-connect`
    /// Err (connection-refused, timeout, host-unreachable, etc.).
    /// Per-error-code dispatch is a follow-up — generic message
    /// keeps the body small.
    pub conn_err_segment_idx: u32,
    pub conn_err_len: u32,
    /// Phase 4.7+ fix #7 — `"tcp: port out of range"`. Surfaces
    /// when the caller hands a port outside `0..=65535`. VM ships
    /// `Tcp: port {N} is out of range (0\u{2013}65535)`; we use a
    /// canned static string because the wasip2 Err is materialised
    /// from a data segment, not runtime formatting.
    pub port_err_segment_idx: u32,
    pub port_err_len: u32,
    /// Phase 4.7+ fix #10 — `"tcp: connection limit reached (256
    /// max)"`. Pool wraparound used to silently evict the live
    /// occupant of slot `tcp_next_id & 255`; `aver-rt::tcp::connect`
    /// refuses with this exact message. Refuses to fresh-connect
    /// before any socket / connect resources are claimed.
    pub limit_err_segment_idx: u32,
    pub limit_err_len: u32,
}

/// Pool storage + bump-heap rewind. Grouped because every
/// successful connect touches all five: it writes a new slot into
/// `tcp_pool` (typed `$tcp_pool` / element type `$tcp_slot`),
/// bumps `tcp_next_id`, and rewinds `bump_alloc_ptr` on each exit
/// path to reclaim the per-call scratch.
pub(in crate::codegen::wasm_gc) struct TcpConnectPool {
    /// `tcp_next_id: i32 = 0` mutable global. Bumps after each
    /// successful connect; slot_idx is `tcp_next_id & 255` (pool
    /// capacity = 256).
    pub tcp_next_id_global: u32,
    /// `tcp_pool: ref null $tcp_pool` mutable global. Lazy-
    /// allocated to `array.new_default $tcp_pool 256` on the first
    /// successful connect; subsequent connects
    /// `array.set $tcp_pool [slot_idx] := struct.new $tcp_slot
    /// (socket, in_stream, out_stream, 1)`.
    pub tcp_pool_global: u32,
    /// `$tcp_slot` wasm-gc type idx. Used by the `struct.new`
    /// building each pool entry.
    pub tcp_slot_type_idx: u32,
    /// `$tcp_pool` wasm-gc type idx. Used by the
    /// `array.new_default` lazy-alloc + `array.set` slot writes.
    pub tcp_pool_type_idx: u32,
    /// Phase 4.2.2f — `bump_alloc_ptr` global. Saved on entry,
    /// restored on every exit so the helper's `cabi_realloc` calls
    /// reclaim their scratch as soon as the call returns. Without
    /// this Tcp.connect leaks ~64 bytes of bump heap per call.
    pub bump_alloc_ptr_global: u32,
}

/// DNS pipeline — instance-network lazy fetch, resolve-addresses
/// retptr setup, and the resolve-next-address polling loop.
pub(in crate::codegen::wasm_gc) struct TcpConnectDns {
    /// `wasi:sockets/instance-network.instance-network: func() ->
    /// network`. Phase 4.2.2a calls this once per program; the
    /// resulting resource handle lands in `network_handle_global`.
    pub instance_network_fn: u32,
    /// `network_handle: i32 = -1` mutable global. -1 sentinel ⇒
    /// "not yet fetched"; the prolog runs `instance-network()` and
    /// stores the resulting resource handle here for program
    /// lifetime. Phase 4.2.2b onwards reads it back to thread into
    /// `resolve-addresses` / `start-connect`.
    pub network_handle_global: u32,
    /// `wasi:sockets/ip-name-lookup.resolve-addresses: func(network:
    /// borrow<network>, name: string) -> result<
    ///   resolve-address-stream, error-code>`. Canonical-ABI
    /// signature: `(network, name_ptr, name_len, retptr) -> ()`;
    /// retptr is 8 bytes (tag@0, stream_handle/err_code@4).
    pub resolve_addresses_fn: u32,
    /// `[method]resolve-address-stream.resolve-next-address:
    /// (this, retptr) -> ()`. Retptr layout:
    ///   +0: result tag (0=Ok, 1=Err)
    ///   +2: on Ok→option tag (0=None, 1=Some); on Err→error-code u8
    ///   +4: on Ok+Some→ip-address variant tag (0=ipv4, 1=ipv6)
    ///   +6..+10 (ipv4): 4× u8 octets
    ///   +6..+22 (ipv6): 8× u16 hextets
    pub resolve_next_address_fn: u32,
    /// `[resource-drop]resolve-address-stream`. Phase 4.2.2b2 keeps
    /// the stream live while looping `resolve-next-address` and
    /// drops once the first IPv4 has been pulled.
    pub drop_resolve_stream_fn: u32,
    /// `[method]resolve-address-stream.subscribe: (this) ->
    /// pollable`. Returns a fresh pollable each loop iteration
    /// that becomes ready when the next address (or EOL) is
    /// available.
    pub stream_subscribe_fn: u32,
    /// `wasi:io/poll.poll: (in_ptr, in_len, retptr) -> ()`.
    /// Single-entry pollable input lives at scratch[+56], the
    /// indices-of-ready retptr at scratch[+48]. Ignores the
    /// returned indices — single-pollable poll already implies
    /// "the only one is ready".
    pub poll_fn: u32,
    /// `[resource-drop]pollable`. Each `stream.subscribe` allocates
    /// a single-use pollable that gets dropped right after `poll`
    /// returns.
    pub drop_pollable_fn: u32,
}

/// Socket-side TCP imports — create / start / finish / drop /
/// shutdown plus the stream-drop pair used for stale-slot recovery
/// when the pool wraps past 256.
pub(in crate::codegen::wasm_gc) struct TcpConnectSocket {
    /// `wasi:sockets/tcp-create-socket.create-tcp-socket: func(
    /// family) -> result<tcp-socket, error-code>`. Family is the
    /// `ip-address-family` enum (0=ipv4, 1=ipv6); we always pass 0.
    /// Result via retptr (8 bytes: tag@0 + handle/err@4).
    pub create_tcp_socket_fn: u32,
    /// `[method]tcp-socket.start-connect: (this, network,
    /// remote-address) -> result<_, error-code>`. 14 i32 args +
    /// retptr. `remote-address` is the `ip-socket-address` variant
    /// flat-lowered to 12 i32 positions (variant tag + max(ipv4 5,
    /// ipv6 11) = 12); for ipv4 the trailing 6 positions are zero.
    pub start_connect_fn: u32,
    /// `[method]tcp-socket.finish-connect: (this, retptr) -> ()`.
    /// Retptr 12 bytes: tag@0 + (in_stream@4, out_stream@8) on
    /// Ok / error-code@4 on Err.
    pub finish_connect_fn: u32,
    /// `[method]tcp-socket.subscribe: (this) -> pollable`. Same
    /// pattern as resolve-address-stream's subscribe — block until
    /// `finish-connect` is ready.
    pub socket_subscribe_fn: u32,
    /// `[resource-drop]tcp-socket`. Phase 4.2.2c drops the socket
    /// on every error path. Phase 4.2.2d keeps it live on the
    /// happy path and stashes it in the pool slot.
    pub drop_tcp_socket_fn: u32,
}

/// String marshaling + record / result wrapping — the "build the
/// user-visible heap value" side of the helper. cabi_realloc lives
/// here because every materialisation path (retptr scratch, host
/// name buffer) goes through it.
pub(in crate::codegen::wasm_gc) struct TcpConnectMaterialize {
    /// `cabi_realloc(old, old_sz, align, new_sz)` exported
    /// allocator. Used to grab the shared 64-byte scratch + the
    /// host-name LM buffer.
    pub cabi_realloc_fn: u32,
    /// `__rt_string_to_lm(s: ref string) -> i32` (length).
    /// Marshals the Aver String's bytes into LM[0..len];
    /// `resolve-addresses` reads the host name from that range.
    pub str_to_lm_fn: u32,
    /// `__rt_tcp_format_id(slot_idx: i32) -> ref string`.
    /// Materialises `"tcp-{slot_idx}"` as a fresh Aver
    /// `(array i8)`.
    pub format_id_fn: u32,
    /// `__rt_record_tcp_connection_make(id, host, port) ->
    /// ref Tcp.Connection`. Factory generated by the existing
    /// `factory_exports` machinery.
    pub record_make_fn: u32,
    /// `__rt_result_tcp_connection_string_ok(conn) -> ref Result<
    /// Tcp.Connection, String>`. Wraps the freshly-built
    /// `Tcp.Connection` on the happy path.
    pub result_ok_fn: u32,
    /// `__rt_result_tcp_connection_string_err(message: ref string)
    /// -> ref Result<Tcp.Connection, String>`. Used at every Err
    /// return point.
    pub result_err_fn: u32,
}

/// Helper fn idxs + global idxs the connect body calls, grouped by
/// pipeline concern. The body code does `helpers.pool.tcp_pool_global`
/// etc., which keeps the 27-field bundle navigable when you're
/// reading the lowering inline. Construction site in
/// `module.rs::emit_tcp_helper_bodies` builds the four sub-bundles
/// from the surrounding wireup locals.
pub(in crate::codegen::wasm_gc) struct TcpConnectHelperFns {
    pub pool: TcpConnectPool,
    pub dns: TcpConnectDns,
    pub socket: TcpConnectSocket,
    pub materialize: TcpConnectMaterialize,
}

/// Emit the body for `__rt_tcp_connect`. Phase 4.2.2a layout:
///
/// 1. Lazy-init `network_handle` from
///    `wasi:sockets/instance-network.instance-network` on first call.
/// 2. STUB tail (Phase 4.2.1): ignore the (host, port) params, push
///    the placeholder error string, wrap via the Result.Err factory,
///    return.
///
/// The host / port args reach the locals (`local 0`, `local 1`) but
/// the body never reads them yet — that lands in 4.2.2b when the DNS
/// resolve loop replaces the stub tail.
/// Single shared retptr / pollable-input scratch block allocated
/// once per `Tcp.connect` call via `cabi_realloc(0, 0, 4, 64)`.
/// Cuts the per-call allocator traffic from 3-4 small bumps down
/// to a single 64-byte chunk and gives later phases stable offsets
/// to load from. Layout (every entry 16-aligned to leave slack):
///
/// | offset | size (B) | purpose                                |
/// |--------|----------|----------------------------------------|
/// | +0     | 8        | retptr — `resolve-addresses` result    |
/// | +16    | 24       | retptr — `resolve-next-address` result |
/// | +48    | 8        | retptr — `wasi:io/poll.poll` (out list) |
/// | +56    | 4        | input  — pollable handle for `poll`     |
///
/// Phases 4.2.2b1 only touches +0; b2 adds +16, +48, +56. The
/// block stays live for the entire helper invocation (Aver
/// `Tcp.connect` is one synchronous call), so no per-step
/// reallocation.
const SCRATCH_BLOCK_SIZE: i32 = 64;
const SCRATCH_OFFSET_RESOLVE: u32 = 0;
const SCRATCH_OFFSET_NEXT: u32 = 16;
const SCRATCH_OFFSET_POLL: u32 = 48;
const SCRATCH_OFFSET_POLLABLE_IN: u32 = 56;

pub(in crate::codegen::wasm_gc) fn emit_tcp_connect_stub(
    indices: &TcpConnectIndices,
    helpers: &TcpConnectHelperFns,
) -> Function {
    use wasm_encoder::{BlockType, MemArg};
    // Locals beyond the two params (0=host: ref string, 1=port: i64):
    //   local 2  = host_len (i32)         — bytes written by str_to_lm
    //   local 3  = scratch (i32)          — base of the 64-byte retptr block
    //   local 4  = resolve_stream (i32)   — handle from resolve-addresses Ok
    //   local 5  = pollable (i32)         — per-call subscribe handle
    //   local 6  = ipv4_a (i32)           ┐
    //   local 7  = ipv4_b (i32)           ├ first-IPv4 octets latched from
    //   local 8  = ipv4_c (i32)           │ resolve-next-address Ok+Some+ipv4
    //   local 9  = ipv4_d (i32)           ┘
    //   local 10 = socket (i32)           — tcp-socket handle from create-tcp-socket
    //   local 11 = in_stream (i32)        ┐
    //   local 12 = out_stream (i32)       ┘ unpacked from finish-connect Ok
    //   local 13 = stale_slot             — `ref null $tcp_slot` carrier used by the
    //                                       Phase 4.2.2e wraparound-drop branch.
    let stale_slot_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(helpers.pool.tcp_slot_type_idx),
    });
    // local 14 = saved_alloc (i32) — see Phase 4.2.2f bump-heap
    // rewind below. Lives next to the typed locals so every exit
    // point can restore the cursor.
    let mut f = Function::new(vec![(12u32, ValType::I32), (1u32, stale_slot_ref)]);
    let l_host_len: u32 = 2;
    let l_scratch: u32 = 3;
    let l_resolve_stream: u32 = 4;
    let l_pollable: u32 = 5;
    let l_ipv4_a: u32 = 6;
    let l_ipv4_b: u32 = 7;
    let l_ipv4_c: u32 = 8;
    let l_ipv4_d: u32 = 9;
    let l_socket: u32 = 10;
    let l_in_stream: u32 = 11;
    let l_out_stream: u32 = 12;
    let l_saved_alloc: u32 = 13;
    let l_stale_slot: u32 = 14;

    let mem4_resolve = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE),
        align: 2,
        memory_index: 0,
    };
    let mem4_resolve_off4 = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE + 4),
        align: 2,
        memory_index: 0,
    };
    let mem1_resolve = MemArg {
        offset: u64::from(SCRATCH_OFFSET_RESOLVE),
        align: 0,
        memory_index: 0,
    };
    // resolve-next-address retptr loads — all byte-granular (option,
    // variant, and ipv4 octet payloads sit at +2 / +4 / +6..+10).
    let mem1_next_outer = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_option = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 2),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_variant = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 4),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_a = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 6),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_b = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 7),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_c = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 8),
        align: 0,
        memory_index: 0,
    };
    let mem1_next_octet_d = MemArg {
        offset: u64::from(SCRATCH_OFFSET_NEXT + 9),
        align: 0,
        memory_index: 0,
    };
    let mem4_pollable_in = MemArg {
        offset: u64::from(SCRATCH_OFFSET_POLLABLE_IN),
        align: 2,
        memory_index: 0,
    };

    // ── Phase 4.2.2f — bump-heap rewind prolog. ────────────────
    // Save the cabi_realloc cursor on entry; every exit path
    // (early Returns + the trailing fall-through) restores it
    // before leaving so the helper's transient scratch is
    // reclaimed. Without this Tcp.connect leaks ~64 bytes per call.
    f.instruction(&Instruction::GlobalGet(helpers.pool.bump_alloc_ptr_global));
    f.instruction(&Instruction::LocalSet(l_saved_alloc));

    // ── Phase 4.7+ fix #7 — port validation. ───────────────────
    // VM rejects `port < 0 || port > 65535` with a Tcp-prefixed Err
    // before any DNS or socket work happens. Wasip2 used to silently
    // I32WrapI64 and pass the truncated value into start-connect,
    // surfacing the failure as the generic `Result.Err("tcp: connect
    // failed")` (or worse, a successful connect to the wrong port).
    // Surface the same shape as VM up-front instead.
    f.instruction(&Instruction::LocalGet(1)); // port (i64)
    f.instruction(&Instruction::I64Const(0));
    f.instruction(&Instruction::I64LtS);
    f.instruction(&Instruction::LocalGet(1));
    f.instruction(&Instruction::I64Const(65535));
    f.instruction(&Instruction::I64GtS);
    f.instruction(&Instruction::I32Or);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.port_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.port_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // ── Phase 4.2.2a — lazy network handle init. ───────────────
    // Pattern mirrors Console.print's stdout cache: if the global
    // is still the -1 sentinel, fetch the host network resource
    // handle once and stash it. The resource is program-lifetime
    // (wasmtime cleans up at component exit), so no drop helper.
    f.instruction(&Instruction::GlobalGet(helpers.dns.network_handle_global));
    f.instruction(&Instruction::I32Const(-1));
    f.instruction(&Instruction::I32Eq);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::Call(helpers.dns.instance_network_fn));
    f.instruction(&Instruction::GlobalSet(helpers.dns.network_handle_global));
    f.instruction(&Instruction::End);

    // ── Phase 4.2.2b — shared scratch block + DNS resolve. ─────
    // Step 1 — marshal host into LM[0..host_len].
    f.instruction(&Instruction::LocalGet(0));
    f.instruction(&Instruction::Call(helpers.materialize.str_to_lm_fn));
    f.instruction(&Instruction::LocalSet(l_host_len));

    // Step 2 — allocate the shared 64-byte scratch block via
    // cabi_realloc (one bump per `Tcp.connect`, reused by every
    // retptr / pollable-input slot the body needs). Layout
    // documented next to `SCRATCH_BLOCK_SIZE` above.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(4));
    f.instruction(&Instruction::I32Const(SCRATCH_BLOCK_SIZE));
    f.instruction(&Instruction::Call(helpers.materialize.cabi_realloc_fn));
    f.instruction(&Instruction::LocalSet(l_scratch));

    // Step 3 — resolve-addresses(network, host_ptr=0, host_len, retptr).
    // retptr lands at scratch[+0..+8].
    f.instruction(&Instruction::GlobalGet(helpers.dns.network_handle_global));
    f.instruction(&Instruction::I32Const(0)); // host_ptr (LM start)
    f.instruction(&Instruction::LocalGet(l_host_len));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.dns.resolve_addresses_fn));

    // Step 4 — read the Result tag at scratch[+0]. On Err,
    // materialise "tcp: dns resolve failed" and return early; on
    // Ok, fall through to the (still-stubbed) tail.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    // Err path: build the dns-error string + Result.Err + early return.
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.dns_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.dns_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Step 5 — Ok branch: pull the stream handle out of scratch[+4].
    // Keep it live; the loop below pulls the first IPv4 address out
    // of it via subscribe/poll/resolve-next-address, then drops the
    // stream when the address is in our locals (or when the resolver
    // gave up before yielding one).
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_resolve_stream));
    let _ = mem4_resolve; // reserved for the future option / ipv4 octet loads

    // ── Phase 4.2.2b2 — DNS resolve loop, first-IPv4 wins. ─────
    // Block 1 — break target. Loop 0 — continue target. On a
    // successful IPv4 latch we `br 1` out with octets in locals;
    // on a None / IPv6 yield we `br 0` to keep polling; on a
    // resolver Err mid-stream we drop the stream and return Err.
    f.instruction(&Instruction::Block(BlockType::Empty));
    f.instruction(&Instruction::Loop(BlockType::Empty));

    // subscribe → pollable
    f.instruction(&Instruction::LocalGet(l_resolve_stream));
    f.instruction(&Instruction::Call(helpers.dns.stream_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    // Store pollable handle at scratch[+56] for the single-entry
    // `poll(in_ptr=scratch+56, in_len=1, retptr=scratch+48)` call.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4_pollable_in));

    // poll([pollable], 1, retptr). Result list is allocated by the
    // host into scratch[+48..+56]; we don't care which index fired
    // (the only pollable is ours).
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLLABLE_IN as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLL as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.dns.poll_fn));

    // drop pollable — single-use per iteration.
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(helpers.dns.drop_pollable_fn));

    // resolve-next-address(stream, retptr=scratch+16).
    f.instruction(&Instruction::LocalGet(l_resolve_stream));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_NEXT as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.dns.resolve_next_address_fn));

    // outer tag — on Err, drop stream + return dns-no-addr Err.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_outer));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_resolve_stream));
    f.instruction(&Instruction::Call(helpers.dns.drop_resolve_stream_fn));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.no_addr_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.no_addr_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // option tag — None means the resolver has exhausted the stream
    // (WASI spec: would-block surfaces as `Err(error-code::would-block)`
    // on the outer arm, not as `Ok(None)`). For an IPv6-only host
    // we'd already have looped past every Some(ipv6) below; reaching
    // Ok(None) means no IPv4 will ever come, so we drop the stream
    // and surface `tcp: dns no addresses`. Treating None as "poll
    // again" (the previous behaviour) spins forever on those hosts.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_option));
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_resolve_stream));
    f.instruction(&Instruction::Call(helpers.dns.drop_resolve_stream_fn));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.no_addr_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.no_addr_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // ip-address variant tag — IPv6 (tag != 0) skipped today, loop
    // to look for the next yield. IPv4 falls through.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_variant));
    f.instruction(&Instruction::BrIf(0));

    // IPv4 latch — read 4 octets out of scratch[+22..+26] and stash.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_a));
    f.instruction(&Instruction::LocalSet(l_ipv4_a));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_b));
    f.instruction(&Instruction::LocalSet(l_ipv4_b));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_c));
    f.instruction(&Instruction::LocalSet(l_ipv4_c));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_next_octet_d));
    f.instruction(&Instruction::LocalSet(l_ipv4_d));

    // Break out of the loop — first IPv4 wins.
    f.instruction(&Instruction::Br(1));

    f.instruction(&Instruction::End); // loop
    f.instruction(&Instruction::End); // block

    // Stream's job is done — drop it. The octets in l_ipv4_* feed
    // start-connect below.
    f.instruction(&Instruction::LocalGet(l_resolve_stream));
    f.instruction(&Instruction::Call(helpers.dns.drop_resolve_stream_fn));

    // ── Phase 4.7+ fix #10 — pool slot reservation. ─────────────
    //
    // Lazy-init `tcp_pool` on the first connect, then refuse if
    // the target slot already holds a live (`in_use == 1`)
    // occupant. Previously this branch ran AFTER finish-connect
    // and force-dropped the live slot to make room; aver-rt::tcp
    // (VM / self-host / wasm-gc-bridge) refuses with `Err("Tcp.
    // connect: connection limit reached (256 max)")`. Match that
    // here, refusing BEFORE we burn a fresh socket + connect on
    // an address we'd have to immediately unwind.
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_pool_global));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(256));
    f.instruction(&Instruction::ArrayNewDefault(
        helpers.pool.tcp_pool_type_idx,
    ));
    f.instruction(&Instruction::GlobalSet(helpers.pool.tcp_pool_global));
    f.instruction(&Instruction::End);

    // slot = tcp_pool[tcp_next_id & 255]; if non-null && in_use ==
    // 1 → Err. Both checks use the typed `stale_slot` local so the
    // null + struct.get path stays trap-safe.
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_pool_global));
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_next_id_global));
    f.instruction(&Instruction::I32Const(255));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::ArrayGet(helpers.pool.tcp_pool_type_idx));
    f.instruction(&Instruction::LocalTee(l_stale_slot));
    f.instruction(&Instruction::RefIsNull);
    f.instruction(&Instruction::I32Eqz);
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_stale_slot));
    f.instruction(&Instruction::StructGet {
        struct_type_index: helpers.pool.tcp_slot_type_idx,
        field_index: 3, // in_use
    });
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.limit_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.limit_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End); // in_use == 1
    f.instruction(&Instruction::End); // slot is non-null

    // ── Phase 4.2.2c — create-tcp-socket + start/finish-connect. ─
    // Scratch offset +0..+8 is now free (DNS exit); reuse for the
    // socket-side retptrs:
    //   +0..+8 (create-tcp-socket result: tag@0 + handle/err@4)
    //   +0..+2 (start-connect result:     tag@0 + err@1)
    //   +0..+12 (finish-connect result:   tag@0 + (in@4, out@8) | err@4)
    // All three calls run sequentially and each fully consumes the
    // previous retptr's data before issuing the next.

    // Step 6 — create-tcp-socket(family=0=ipv4, retptr=scratch+0).
    f.instruction(&Instruction::I32Const(0)); // ip-address-family.ipv4
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.socket.create_tcp_socket_fn));

    // tag@scratch+0; Err ⇒ socket-create Err.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.sock_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.sock_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Latch socket handle at scratch+4.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_socket));

    // Step 7 — start-connect(socket, network, ipv4_socket_address,
    //                        retptr). 14 i32 args + retptr (15 total).
    // ip-socket-address flat:
    //   pos 0:  variant tag    (0 = ipv4)
    //   pos 1:  port           (u16 → i32, narrowing from Aver Int)
    //   pos 2:  octet a        (u8 → i32)
    //   pos 3:  octet b
    //   pos 4:  octet c
    //   pos 5:  octet d
    //   pos 6..11: 0×6         (ipv6 padding — unused for ipv4)
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::GlobalGet(helpers.dns.network_handle_global));
    f.instruction(&Instruction::I32Const(0)); // addr variant: ipv4
    f.instruction(&Instruction::LocalGet(1)); // port (i64 param)
    f.instruction(&Instruction::I32WrapI64);
    f.instruction(&Instruction::LocalGet(l_ipv4_a));
    f.instruction(&Instruction::LocalGet(l_ipv4_b));
    f.instruction(&Instruction::LocalGet(l_ipv4_c));
    f.instruction(&Instruction::LocalGet(l_ipv4_d));
    // 6× zero pads for the ipv6 join positions.
    for _ in 0..6 {
        f.instruction(&Instruction::I32Const(0));
    }
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.socket.start_connect_fn));

    // start-connect retptr is 2 bytes: tag@0 + err@1. Err ⇒ drop
    // socket and surface conn-failed.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.socket.drop_tcp_socket_fn));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.conn_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.conn_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Step 8 — socket.subscribe + poll([pollable], 1, retptr=scratch+48).
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.socket.socket_subscribe_fn));
    f.instruction(&Instruction::LocalSet(l_pollable));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::I32Store(mem4_pollable_in));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLLABLE_IN as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Const(SCRATCH_OFFSET_POLL as i32));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::Call(helpers.dns.poll_fn));

    f.instruction(&Instruction::LocalGet(l_pollable));
    f.instruction(&Instruction::Call(helpers.dns.drop_pollable_fn));

    // Step 9 — finish-connect(socket, retptr=scratch+0). Retptr 12B:
    //   +0: tag, +4: on Ok in_stream / on Err error-code, +8: out_stream.
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::Call(helpers.socket.finish_connect_fn));

    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load8U(mem1_resolve));
    f.instruction(&Instruction::If(BlockType::Empty));
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::Call(helpers.socket.drop_tcp_socket_fn));
    f.instruction(&Instruction::I32Const(0));
    f.instruction(&Instruction::I32Const(indices.conn_err_len as i32));
    f.instruction(&Instruction::ArrayNewData {
        array_type_index: indices.string_type_idx,
        array_data_index: indices.conn_err_segment_idx,
    });
    f.instruction(&Instruction::Call(helpers.materialize.result_err_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    f.instruction(&Instruction::Return);
    f.instruction(&Instruction::End);

    // Unpack the (input-stream, output-stream) tuple from
    // scratch+4 and scratch+8. These plus l_socket are the three
    // i32 handles Phase 4.2.2d will stash into the pool slot.
    f.instruction(&Instruction::LocalGet(l_scratch));
    f.instruction(&Instruction::I32Load(mem4_resolve_off4));
    f.instruction(&Instruction::LocalSet(l_in_stream));
    // out_stream lives at scratch+8 — same align-2 i32 load as +4
    // with a +4-offset variant of mem4_resolve_off4.
    {
        let mem4_resolve_off8 = MemArg {
            offset: u64::from(SCRATCH_OFFSET_RESOLVE + 8),
            align: 2,
            memory_index: 0,
        };
        f.instruction(&Instruction::LocalGet(l_scratch));
        f.instruction(&Instruction::I32Load(mem4_resolve_off8));
        f.instruction(&Instruction::LocalSet(l_out_stream));
    }

    // Octets aren't used past this point — they fed start-connect
    // already. Keep the local declarations stable for future hooks.
    let _ = (l_ipv4_a, l_ipv4_b, l_ipv4_c, l_ipv4_d);

    // ── Phase 4.2.2d — pool slot + Tcp.Connection materialise. ─
    //
    // Pool limit + lazy-init was moved up-front (right after the
    // DNS resolve drop, before create-tcp-socket) by Phase 4.7+
    // fix #10 so the 257th live connect refuses with
    // `Err("tcp: connection limit reached (256 max)")` instead of
    // silently evicting the existing live occupant of slot
    // `tcp_next_id & 255`. By the time we reach Step 11 the slot
    // is guaranteed null or `in_use == 0`, so `struct.new` +
    // `array.set` overwrite the entry safely.
    let _ = l_stale_slot; // local kept for ABI stability; no body usage

    // Step 11 — `tcp_pool[tcp_next_id & 255] = struct.new $tcp_slot
    // (socket, in_stream, out_stream, in_use=1, id_value=tcp_next_id)`.
    // `id_value` is the full counter snapshot (not masked) — Phase
    // 4.7 fix #2 uses it as the cross-call freshness check so a
    // stale `Tcp.Connection` carrying the same modulo-256 slot id
    // can't operate on whatever happens to live there now.
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_pool_global));
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_next_id_global));
    f.instruction(&Instruction::I32Const(255));
    f.instruction(&Instruction::I32And);
    f.instruction(&Instruction::LocalGet(l_socket));
    f.instruction(&Instruction::LocalGet(l_in_stream));
    f.instruction(&Instruction::LocalGet(l_out_stream));
    f.instruction(&Instruction::I32Const(1)); // in_use = 1
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_next_id_global));
    f.instruction(&Instruction::StructNew(helpers.pool.tcp_slot_type_idx));
    f.instruction(&Instruction::ArraySet(helpers.pool.tcp_pool_type_idx));

    // Step 12 — `id = __rt_tcp_format_id(tcp_next_id)`. Full counter
    // value, not masked — the id string is the program-level handle
    // and must be unique across the lifetime of the process. Parse
    // recovers the same i32 via `__rt_tcp_parse_id`; consumers then
    // compare it against the stored `slot.id_value` to detect stale
    // references after pool wraparound.
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_next_id_global));
    f.instruction(&Instruction::Call(helpers.materialize.format_id_fn));

    // Step 13 — bump `tcp_next_id`. Wrapping i32 add at counter
    // level is fine: the slot picker masks back to 0..255, and the
    // id_value freshness check tolerates wrap because two slots
    // 2^32 connects apart can't both have live references in any
    // realistic program.
    f.instruction(&Instruction::GlobalGet(helpers.pool.tcp_next_id_global));
    f.instruction(&Instruction::I32Const(1));
    f.instruction(&Instruction::I32Add);
    f.instruction(&Instruction::GlobalSet(helpers.pool.tcp_next_id_global));

    // Step 14 — build `Tcp.Connection { id, host, port }` via the
    // existing factory. Stack: [id]; push host (param 0) and port
    // (param 1); the factory pops all three and returns the
    // record ref.
    f.instruction(&Instruction::LocalGet(0)); // host
    f.instruction(&Instruction::LocalGet(1)); // port (i64)
    f.instruction(&Instruction::Call(helpers.materialize.record_make_fn));

    // Step 15 — wrap in `Result.Ok(conn)` and let the function
    // return naturally. The stub-err tail is gone — every exit out
    // of the helper now goes through a typed factory. Restore the
    // bump-heap cursor first so the per-call scratch / retptrs we
    // allocated through `cabi_realloc` are reclaimed.
    f.instruction(&Instruction::Call(helpers.materialize.result_ok_fn));
    restore_bump(&mut f, l_saved_alloc, helpers.pool.bump_alloc_ptr_global);
    let _ = indices.stub_err_segment_idx;
    let _ = indices.stub_err_len;
    f.instruction(&Instruction::End);
    f
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stub_emit_compiles() {
        // Smoke test — the body emitter must produce a well-formed
        // wasm Function with no panics on synthetic indices. Actual
        // wasm validation runs at module-emit time.
        let indices = TcpConnectIndices {
            fn_type: 0,
            fn_idx: 0,
            string_type_idx: 1,
            stub_err_segment_idx: 0,
            stub_err_len: b"tcp: connect not yet implemented".len() as u32,
            dns_err_segment_idx: 1,
            dns_err_len: b"tcp: dns resolve failed".len() as u32,
            no_addr_segment_idx: 2,
            no_addr_len: b"tcp: dns no addresses".len() as u32,
            sock_err_segment_idx: 3,
            sock_err_len: b"tcp: socket create failed".len() as u32,
            conn_err_segment_idx: 4,
            conn_err_len: b"tcp: connect failed".len() as u32,
            port_err_segment_idx: 5,
            port_err_len: b"tcp: port out of range".len() as u32,
            limit_err_segment_idx: 6,
            limit_err_len: b"tcp: connection limit reached (256 max)".len() as u32,
        };
        let helpers = TcpConnectHelperFns {
            pool: TcpConnectPool {
                tcp_next_id_global: 1,
                tcp_pool_global: 2,
                tcp_slot_type_idx: 3,
                tcp_pool_type_idx: 4,
                bump_alloc_ptr_global: 5,
            },
            dns: TcpConnectDns {
                instance_network_fn: 3,
                network_handle_global: 0,
                resolve_addresses_fn: 6,
                resolve_next_address_fn: 11,
                drop_resolve_stream_fn: 7,
                stream_subscribe_fn: 8,
                poll_fn: 9,
                drop_pollable_fn: 10,
            },
            socket: TcpConnectSocket {
                create_tcp_socket_fn: 12,
                start_connect_fn: 13,
                finish_connect_fn: 15,
                socket_subscribe_fn: 14,
                drop_tcp_socket_fn: 16,
            },
            materialize: TcpConnectMaterialize {
                cabi_realloc_fn: 4,
                str_to_lm_fn: 5,
                format_id_fn: 17,
                record_make_fn: 18,
                result_ok_fn: 19,
                result_err_fn: 2,
            },
        };
        let _f = emit_tcp_connect_stub(&indices, &helpers);
    }
}
