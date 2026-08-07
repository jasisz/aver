//! Allocation + funcs-section registration for the `Tcp.*` helper
//! family. Pulled out of `module.rs` so the giant `emit_module_with`
//! body stays focused on cross-helper orchestration rather than the
//! per-helper type-section / fn-idx bookkeeping.
//!
//! Two entry points:
//! - [`allocate`] consults the registry / wasi-import lookup table /
//!   factory gates and assembles a [`TcpHelpers`] bundle of
//!   `Option<Indices>` plus the two `(type_idx, fn_idx)` tuples for
//!   the format-id / parse-id glue helpers. Each `Option` is `None`
//!   when the program never named the matching effect (so no slot
//!   was reserved upstream); the allocation reserves a `next_type_idx`
//!   + `next_builtin_fn_idx` only when every gate is satisfied.
//! - [`register_funcs`] walks the bundle in allocation order and
//!   appends one `funcs.function(ty_idx)` entry per allocated
//!   helper. The order has to match what allocation chose, otherwise
//!   every `Call(idx)` in the emitted bodies shifts and the wasm
//!   validator rejects the module at cryptic offsets.
//!
//! Body emission still lives in `module.rs` because it pulls in
//! six private structures (`FactoryExports`, `Wasip2Globals`,
//! `BridgeIndices`, …) that aren't exposed outside that file. The
//! bodies follow the same `if let Some(t) = &helpers.connect { ... }`
//! pattern there.

use wasm_encoder::{FunctionSection, TypeSection, ValType};

use super::super::effects::{EffectName, EffectRegistry};
use super::super::types::TypeRegistry;
use super::super::wasip2_imports::{Wasip2ImportRegistry, Wasip2ImportSlot};
use super::{
    TcpCloseIndices, TcpConnectIndices, TcpPingIndices, TcpReadBytesIndices, TcpReadLineIndices,
    TcpSendBytesIndices, TcpSendIndices, TcpWriteBytesIndices, TcpWriteLineIndices,
};

/// Per-helper allocation bundle. Every field is `Option<_>`: `None`
/// means the program never invoked that helper's effect, so the
/// gating data segments / wasi-import slots were absent. The
/// `format_id` / `parse_id` pair is a `(type_idx, fn_idx)` tuple
/// because those two helpers have no full indices struct of their
/// own — every consumer that needs them just wants the fn idx.
pub(in crate::codegen::wasm_gc) struct TcpHelpers {
    pub connect: Option<TcpConnectIndices>,
    pub format_id: Option<(u32, u32)>,
    pub parse_id: Option<(u32, u32)>,
    pub write_line: Option<TcpWriteLineIndices>,
    pub write_bytes: Option<TcpWriteBytesIndices>,
    pub read_line: Option<TcpReadLineIndices>,
    pub read_bytes: Option<TcpReadBytesIndices>,
    pub close: Option<TcpCloseIndices>,
    pub send: Option<TcpSendIndices>,
    pub send_bytes: Option<TcpSendBytesIndices>,
    pub ping: Option<TcpPingIndices>,
}

/// Walk the registry / wasi-import lookup table once and reserve a
/// `(type_idx, fn_idx)` pair for every `Tcp.*` helper whose gates
/// fire. Mutates `types` (one `.function(...)` per allocated helper)
/// and bumps `next_type_idx` / `next_builtin_fn_idx` in lockstep.
///
/// Ordering invariant: the inner blocks reserve indices in this
/// exact sequence — connect → format_id → parse_id → write_line → write_bytes
/// → read_line → read_bytes → close → send → send_bytes → ping.
/// [`register_funcs`] and the
/// `emit_*` blocks in `module.rs` rely on it.
pub(in crate::codegen::wasm_gc) fn allocate(
    effects: &EffectRegistry,
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> TcpHelpers {
    // Every helper allocation is gated on its declared effect, not
    // just registry / wasi-import availability. Without the effect
    // gate, one Tcp.* effect registers enough shared slots (the
    // needs_tcp literals, the pool types, the wasi:io stream pair)
    // that unrelated helpers allocate coincidentally and ship as
    // dead module bytes — e.g. `Tcp.writeLine` + `Console.readLine`
    // used to emit the whole `__rt_tcp_read_line` body.
    let declares = |name: EffectName| effects.iter().any(|effect| effect == name);
    let connect = declares(EffectName::TcpConnect)
        .then(|| {
            allocate_connect(
                registry,
                wasip2_imports,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let format_id = allocate_format_id(
        registry,
        &connect,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    // parse_id is shared by every pool-consuming helper (write_line /
    // write_bytes / read_line / read_bytes / close), so it gates on
    // the union of their effects.
    let parse_id = effects
        .iter()
        .any(|effect| {
            matches!(
                effect,
                EffectName::TcpWriteLine
                    | EffectName::TcpWriteBytes
                    | EffectName::TcpReadLine
                    | EffectName::TcpReadBytes
                    | EffectName::TcpClose
            )
        })
        .then(|| allocate_parse_id(registry, types, next_type_idx, next_builtin_fn_idx))
        .flatten();
    let write_line = declares(EffectName::TcpWriteLine)
        .then(|| {
            allocate_write_line(
                registry,
                wasip2_imports,
                parse_id,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let write_bytes = declares(EffectName::TcpWriteBytes)
        .then(|| {
            allocate_write_bytes(
                registry,
                wasip2_imports,
                parse_id,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let read_line = declares(EffectName::TcpReadLine)
        .then(|| {
            allocate_read_line(
                registry,
                wasip2_imports,
                parse_id,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let read_bytes = declares(EffectName::TcpReadBytes)
        .then(|| {
            allocate_read_bytes(
                registry,
                wasip2_imports,
                parse_id,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let close = declares(EffectName::TcpClose)
        .then(|| {
            allocate_close(
                registry,
                wasip2_imports,
                parse_id,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let send = declares(EffectName::TcpSend)
        .then(|| {
            allocate_send(
                registry,
                wasip2_imports,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let send_bytes = declares(EffectName::TcpSendBytes)
        .then(|| {
            allocate_send_bytes(
                registry,
                wasip2_imports,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    let ping = declares(EffectName::TcpPing)
        .then(|| {
            allocate_ping(
                registry,
                wasip2_imports,
                types,
                next_type_idx,
                next_builtin_fn_idx,
            )
        })
        .flatten();
    TcpHelpers {
        connect,
        format_id,
        parse_id,
        write_line,
        write_bytes,
        read_line,
        read_bytes,
        close,
        send,
        send_bytes,
        ping,
    }
}

/// Append one `funcs.function(ty_idx)` entry per allocated helper,
/// in the same order [`allocate`] reserves them. Skipping or
/// reordering here breaks every `Call(idx)` in the emitted bodies.
pub(in crate::codegen::wasm_gc) fn register_funcs(
    funcs: &mut FunctionSection,
    helpers: &TcpHelpers,
) {
    if let Some(t) = &helpers.connect {
        funcs.function(t.fn_type);
    }
    if let Some((ty, _)) = helpers.format_id {
        funcs.function(ty);
    }
    if let Some((ty, _)) = helpers.parse_id {
        funcs.function(ty);
    }
    if let Some(t) = &helpers.write_line {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.write_bytes {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.read_line {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.read_bytes {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.close {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.send {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.send_bytes {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.ping {
        funcs.function(t.fn_type);
    }
}

fn allocate_connect(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpConnectIndices> {
    let string_idx = registry.string_array_type_idx?;
    let result_idx = registry.result_type_idx("Result<Tcp.Connection,String>")?;
    let stub_seg = registry.string_literal_segment(b"tcp: connect not yet implemented")?;
    let dns_seg = registry.string_literal_segment(b"tcp: dns resolve failed")?;
    let no_addr_seg = registry.string_literal_segment(b"tcp: dns no addresses")?;
    let sock_err_seg = registry.string_literal_segment(b"tcp: socket create failed")?;
    let conn_err_seg = registry.string_literal_segment(b"tcp: connect failed")?;
    let port_err_seg = registry.string_literal_segment(b"tcp: port out of range")?;
    let limit_err_seg =
        registry.string_literal_segment(b"tcp: connection limit reached (256 max)")?;
    // Every wasi-sockets import the body calls must already be
    // present in the lookup table. We don't carry the fn idxs here
    // (emit_bodies re-resolves them) — the gate proves they exist.
    let gates: &[Wasip2ImportSlot] = &[
        Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses,
        Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
        Wasip2ImportSlot::IoPollPoll,
        Wasip2ImportSlot::IoPollResourceDropPollable,
        Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress,
        Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket,
        Wasip2ImportSlot::SocketsTcpStartConnect,
        Wasip2ImportSlot::SocketsTcpSubscribe,
        Wasip2ImportSlot::SocketsTcpFinishConnect,
        Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket,
    ];
    for slot in gates {
        wasip2_imports.lookup_wasm_fn_idx(*slot)?;
    }

    let r_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(string_idx),
    });
    types.ty().function([s_ref, ValType::I64], [r_ref]);
    let fn_type = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpConnectIndices {
        fn_type,
        fn_idx,
        string_type_idx: string_idx,
        stub_err_segment_idx: stub_seg,
        stub_err_len: b"tcp: connect not yet implemented".len() as u32,
        dns_err_segment_idx: dns_seg,
        dns_err_len: b"tcp: dns resolve failed".len() as u32,
        no_addr_segment_idx: no_addr_seg,
        no_addr_len: b"tcp: dns no addresses".len() as u32,
        sock_err_segment_idx: sock_err_seg,
        sock_err_len: b"tcp: socket create failed".len() as u32,
        conn_err_segment_idx: conn_err_seg,
        conn_err_len: b"tcp: connect failed".len() as u32,
        port_err_segment_idx: port_err_seg,
        port_err_len: b"tcp: port out of range".len() as u32,
        limit_err_segment_idx: limit_err_seg,
        limit_err_len: b"tcp: connection limit reached (256 max)".len() as u32,
    })
}

fn allocate_format_id(
    registry: &TypeRegistry,
    connect: &Option<TcpConnectIndices>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<(u32, u32)> {
    connect.as_ref()?;
    let s_idx = registry
        .string_array_type_idx
        .expect("tcp_format_id allocation gated on tcp_connect which requires the string slot");
    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(s_idx),
    });
    types.ty().function([ValType::I32], [s_ref]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some((ty, fn_idx))
}

fn allocate_parse_id(
    registry: &TypeRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<(u32, u32)> {
    // The caller gates on the union of the pool-consuming effects
    // rather than the connect helper specifically. close / write_line
    // / read_line each need parse_id; a program that only consumes a
    // `Tcp.Connection` parameter (e.g. `fn handle(c: Tcp.Connection)
    // ! [Tcp.close]`) graduates close in `effect_check` without ever
    // declaring `Tcp.connect`, and would otherwise hit an `expect`
    // at emit time. The slot type idx check stays as the registry
    // availability gate.
    registry.tcp_slot_type_idx?;
    let s_idx = registry.string_array_type_idx?;
    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(s_idx),
    });
    types.ty().function([s_ref], [ValType::I32]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some((ty, fn_idx))
}

fn allocate_write_line(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    parse_id: Option<(u32, u32)>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpWriteLineIndices> {
    let string_idx = registry.string_array_type_idx?;
    let rec_idx = registry.record_type_idx("Tcp.Connection")?;
    let slot_idx = registry.tcp_slot_type_idx?;
    let pool_idx = registry.tcp_pool_type_idx?;
    let result_idx = registry.result_type_idx("Result<Unit,String>")?;
    let write_err_seg = registry.string_literal_segment(b"tcp: write failed")?;
    let unknown_seg = registry.string_literal_segment(b"tcp: unknown connection")?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush)?;
    parse_id?;

    let conn_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(rec_idx),
    });
    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(string_idx),
    });
    let res_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    types.ty().function([conn_ref, s_ref], [res_ref]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpWriteLineIndices {
        fn_type: ty,
        fn_idx,
        string_type_idx: string_idx,
        tcp_connection_type_idx: rec_idx,
        tcp_slot_type_idx: slot_idx,
        tcp_pool_type_idx: pool_idx,
        write_err_segment_idx: write_err_seg,
        write_err_len: b"tcp: write failed".len() as u32,
        unknown_segment_idx: unknown_seg,
        unknown_len: b"tcp: unknown connection".len() as u32,
    })
}

fn allocate_write_bytes(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    parse_id: Option<(u32, u32)>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpWriteBytesIndices> {
    let string_idx = registry.string_array_type_idx?;
    let bytes_idx = registry
        .packed_sequence("Bytes")
        .map(|packed| packed.type_idx)
        .or_else(|| registry.record_type_idx("Bytes"))?;
    let list_idx = registry.list_type_idx("List<Int>")?;
    let aint_idx = registry.aint_struct_idx?;
    let conn_idx = registry.record_type_idx("Tcp.Connection")?;
    let slot_idx = registry.tcp_slot_type_idx?;
    let pool_idx = registry.tcp_pool_type_idx?;
    let result_idx = registry.result_type_idx("Result<Unit,String>")?;
    let malformed = b"Tcp.writeBytes: malformed Bytes carrier";
    let malformed_seg = registry.string_literal_segment(malformed)?;
    let write_err = b"tcp: write failed";
    let write_err_seg = registry.string_literal_segment(write_err)?;
    let unknown = b"tcp: unknown connection";
    let unknown_seg = registry.string_literal_segment(unknown)?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush)?;
    parse_id?;

    let conn_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(conn_idx),
    });
    let bytes_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(bytes_idx),
    });
    let result_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    types.ty().function([conn_ref, bytes_ref], [result_ref]);
    let fn_type = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpWriteBytesIndices {
        fn_type,
        fn_idx,
        string_type_idx: string_idx,
        bytes_type_idx: bytes_idx,
        list_int_type_idx: list_idx,
        aint_struct_type_idx: aint_idx,
        tcp_connection_type_idx: conn_idx,
        tcp_slot_type_idx: slot_idx,
        tcp_pool_type_idx: pool_idx,
        malformed_segment_idx: malformed_seg,
        malformed_len: malformed.len() as u32,
        write_err_segment_idx: write_err_seg,
        write_err_len: write_err.len() as u32,
        unknown_segment_idx: unknown_seg,
        unknown_len: unknown.len() as u32,
    })
}

fn allocate_read_line(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    parse_id: Option<(u32, u32)>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpReadLineIndices> {
    let string_idx = registry.string_array_type_idx?;
    let rec_idx = registry.record_type_idx("Tcp.Connection")?;
    let slot_idx = registry.tcp_slot_type_idx?;
    let pool_idx = registry.tcp_pool_type_idx?;
    let result_idx = registry.result_type_idx("Result<String,String>")?;
    let eof_seg = registry.string_literal_segment(b"tcp: eof")?;
    let unknown_seg = registry.string_literal_segment(b"tcp: unknown connection")?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::InputStreamBlockingRead)?;
    parse_id?;

    let conn_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(rec_idx),
    });
    let res_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    types.ty().function([conn_ref], [res_ref]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpReadLineIndices {
        fn_type: ty,
        fn_idx,
        string_type_idx: string_idx,
        result_type_idx: result_idx,
        tcp_connection_type_idx: rec_idx,
        tcp_slot_type_idx: slot_idx,
        tcp_pool_type_idx: pool_idx,
        eof_segment_idx: eof_seg,
        eof_len: b"tcp: eof".len() as u32,
        unknown_segment_idx: unknown_seg,
        unknown_len: b"tcp: unknown connection".len() as u32,
    })
}

fn allocate_read_bytes(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    parse_id: Option<(u32, u32)>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpReadBytesIndices> {
    let string_idx = registry.string_array_type_idx?;
    let connection_idx = registry.record_type_idx("Tcp.Connection")?;
    let slot_idx = registry.tcp_slot_type_idx?;
    let pool_idx = registry.tcp_pool_type_idx?;
    let int_idx = registry.aint_struct_idx?;
    let list_int_idx = registry.list_type_idx("List<Int>")?;
    let result_idx = registry.result_type_idx("Result<Bytes,String>")?;
    let negative_seg = registry.string_literal_segment(b"Tcp.readBytes: count is negative")?;
    let limit_seg =
        registry.string_literal_segment(b"Tcp.readBytes: count exceeds the 10485760 byte limit")?;
    let read_limit_seg =
        registry.string_literal_segment(b"Tcp.readBytes: count exceeds the read limit")?;
    let short_read_seg = registry.string_literal_segment(b"failed to fill whole buffer")?;
    let unknown_seg = registry.string_literal_segment(b"tcp: unknown connection")?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::InputStreamBlockingRead)?;
    parse_id?;

    let connection_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(connection_idx),
    });
    let int_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(int_idx),
    });
    let result_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    types.ty().function([connection_ref, int_ref], [result_ref]);
    let fn_type = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpReadBytesIndices {
        fn_type,
        fn_idx,
        string_type_idx: string_idx,
        tcp_connection_type_idx: connection_idx,
        tcp_slot_type_idx: slot_idx,
        tcp_pool_type_idx: pool_idx,
        aint_struct_type_idx: int_idx,
        list_int_type_idx: list_int_idx,
        negative_segment_idx: negative_seg,
        negative_len: b"Tcp.readBytes: count is negative".len() as u32,
        limit_segment_idx: limit_seg,
        limit_len: b"Tcp.readBytes: count exceeds the 10485760 byte limit".len() as u32,
        read_limit_segment_idx: read_limit_seg,
        read_limit_len: b"Tcp.readBytes: count exceeds the read limit".len() as u32,
        short_read_segment_idx: short_read_seg,
        short_read_len: b"failed to fill whole buffer".len() as u32,
        unknown_segment_idx: unknown_seg,
        unknown_len: b"tcp: unknown connection".len() as u32,
    })
}

fn allocate_close(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    parse_id: Option<(u32, u32)>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpCloseIndices> {
    let rec_idx = registry.record_type_idx("Tcp.Connection")?;
    let slot_idx = registry.tcp_slot_type_idx?;
    let pool_idx = registry.tcp_pool_type_idx?;
    let result_idx = registry.result_type_idx("Result<Unit,String>")?;
    let string_idx = registry.string_array_type_idx?;
    let unknown_seg = registry.string_literal_segment(b"tcp: unknown connection")?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::SocketsTcpShutdown)?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::IoStreamsResourceDropInputStream)?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::IoStreamsResourceDropOutputStream)?;
    wasip2_imports.lookup_wasm_fn_idx(Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket)?;
    parse_id?;

    let conn_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(rec_idx),
    });
    let res_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    types.ty().function([conn_ref], [res_ref]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpCloseIndices {
        fn_type: ty,
        fn_idx,
        tcp_connection_type_idx: rec_idx,
        tcp_slot_type_idx: slot_idx,
        tcp_pool_type_idx: pool_idx,
        string_type_idx: string_idx,
        unknown_segment_idx: unknown_seg,
        unknown_len: b"tcp: unknown connection".len() as u32,
    })
}

fn allocate_send(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpSendIndices> {
    // Phase 4.7+ pass 4 — send is now ephemeral (inline DNS +
    // socket + connect, no pool). Gates on the same wasi-sockets
    // imports `Tcp.connect` uses + the wasi:io stream pair the
    // write/read pipeline talks to.
    let string_idx = registry.string_array_type_idx?;
    let res_string_idx = registry.result_type_idx("Result<String,String>")?;
    let dns_err_seg = registry.string_literal_segment(b"tcp: dns resolve failed")?;
    let no_addr_seg = registry.string_literal_segment(b"tcp: dns no addresses")?;
    let sock_err_seg = registry.string_literal_segment(b"tcp: socket create failed")?;
    let conn_err_seg = registry.string_literal_segment(b"tcp: connect failed")?;
    let port_err_seg = registry.string_literal_segment(b"tcp: port out of range")?;
    let write_err_seg = registry.string_literal_segment(b"tcp: write failed")?;
    let stream_err_seg = registry.string_literal_segment(b"tcp: stream error")?;
    let size_err_seg = registry.string_literal_segment(b"tcp: response exceeds 10 MiB limit")?;
    let gates: &[Wasip2ImportSlot] = &[
        Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses,
        Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
        Wasip2ImportSlot::IoPollPoll,
        Wasip2ImportSlot::IoPollResourceDropPollable,
        Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress,
        Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket,
        Wasip2ImportSlot::SocketsTcpStartConnect,
        Wasip2ImportSlot::SocketsTcpSubscribe,
        Wasip2ImportSlot::SocketsTcpFinishConnect,
        Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket,
        Wasip2ImportSlot::SocketsTcpShutdown,
        Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
        Wasip2ImportSlot::InputStreamBlockingRead,
        Wasip2ImportSlot::IoStreamsResourceDropInputStream,
        Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
    ];
    for slot in gates {
        wasip2_imports.lookup_wasm_fn_idx(*slot)?;
    }

    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(string_idx),
    });
    let res_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_string_idx),
    });
    types.ty().function([s_ref, ValType::I64, s_ref], [res_ref]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpSendIndices {
        fn_type: ty,
        fn_idx,
        string_type_idx: string_idx,
        result_string_string_type_idx: res_string_idx,
        dns_err_segment_idx: dns_err_seg,
        dns_err_len: b"tcp: dns resolve failed".len() as u32,
        no_addr_segment_idx: no_addr_seg,
        no_addr_len: b"tcp: dns no addresses".len() as u32,
        sock_err_segment_idx: sock_err_seg,
        sock_err_len: b"tcp: socket create failed".len() as u32,
        conn_err_segment_idx: conn_err_seg,
        conn_err_len: b"tcp: connect failed".len() as u32,
        port_err_segment_idx: port_err_seg,
        port_err_len: b"tcp: port out of range".len() as u32,
        write_err_segment_idx: write_err_seg,
        write_err_len: b"tcp: write failed".len() as u32,
        stream_err_segment_idx: stream_err_seg,
        stream_err_len: b"tcp: stream error".len() as u32,
        size_err_segment_idx: size_err_seg,
        size_err_len: b"tcp: response exceeds 10 MiB limit".len() as u32,
    })
}

fn allocate_send_bytes(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpSendBytesIndices> {
    let string_idx = registry.string_array_type_idx?;
    let bytes_idx = registry
        .packed_sequence("Bytes")
        .map(|packed| packed.type_idx)
        .or_else(|| registry.record_type_idx("Bytes"))?;
    let list_int_idx = registry.list_type_idx("List<Int>")?;
    let result_idx = registry.result_type_idx("Result<Bytes,String>")?;
    let dns_err_seg = registry.string_literal_segment(b"tcp: dns resolve failed")?;
    let no_addr_seg = registry.string_literal_segment(b"tcp: dns no addresses")?;
    let sock_err_seg = registry.string_literal_segment(b"tcp: socket create failed")?;
    let conn_err_seg = registry.string_literal_segment(b"tcp: connect failed")?;
    let port_err_seg = registry.string_literal_segment(b"tcp: port out of range")?;
    let write_err_seg = registry.string_literal_segment(b"tcp: write failed")?;
    let stream_err_seg = registry.string_literal_segment(b"tcp: stream error")?;
    let size_err_seg = registry.string_literal_segment(b"tcp: response exceeds 10 MiB limit")?;
    let gates: &[Wasip2ImportSlot] = &[
        Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses,
        Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
        Wasip2ImportSlot::IoPollPoll,
        Wasip2ImportSlot::IoPollResourceDropPollable,
        Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress,
        Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket,
        Wasip2ImportSlot::SocketsTcpStartConnect,
        Wasip2ImportSlot::SocketsTcpSubscribe,
        Wasip2ImportSlot::SocketsTcpFinishConnect,
        Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket,
        Wasip2ImportSlot::SocketsTcpShutdown,
        Wasip2ImportSlot::OutputStreamBlockingWriteAndFlush,
        Wasip2ImportSlot::InputStreamBlockingRead,
        Wasip2ImportSlot::IoStreamsResourceDropInputStream,
        Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
    ];
    for slot in gates {
        wasip2_imports.lookup_wasm_fn_idx(*slot)?;
    }

    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(string_idx),
    });
    let bytes_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(bytes_idx),
    });
    let result_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(result_idx),
    });
    types
        .ty()
        .function([s_ref, ValType::I64, bytes_ref], [result_ref]);
    let fn_type = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpSendBytesIndices {
        fn_type,
        fn_idx,
        string_type_idx: string_idx,
        bytes_type_idx: bytes_idx,
        list_int_type_idx: list_int_idx,
        result_bytes_string_type_idx: result_idx,
        aint_struct_type_idx: registry.aint_struct_idx,
        dns_err_segment_idx: dns_err_seg,
        dns_err_len: b"tcp: dns resolve failed".len() as u32,
        no_addr_segment_idx: no_addr_seg,
        no_addr_len: b"tcp: dns no addresses".len() as u32,
        sock_err_segment_idx: sock_err_seg,
        sock_err_len: b"tcp: socket create failed".len() as u32,
        conn_err_segment_idx: conn_err_seg,
        conn_err_len: b"tcp: connect failed".len() as u32,
        port_err_segment_idx: port_err_seg,
        port_err_len: b"tcp: port out of range".len() as u32,
        write_err_segment_idx: write_err_seg,
        write_err_len: b"tcp: write failed".len() as u32,
        stream_err_segment_idx: stream_err_seg,
        stream_err_len: b"tcp: stream error".len() as u32,
        size_err_segment_idx: size_err_seg,
        size_err_len: b"tcp: response exceeds 10 MiB limit".len() as u32,
    })
}

fn allocate_ping(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpPingIndices> {
    // Phase 4.7+ pass 5 fix #21 — ping is now ephemeral (inline DNS
    // + socket + connect, no pool slot, no `__rt_tcp_connect` call).
    // Gates on the same wasi-sockets imports `Tcp.connect` / `Tcp.send`
    // need, independently of whether `connect` / `close` allocated.
    let string_idx = registry.string_array_type_idx?;
    let res_unit_idx = registry.result_type_idx("Result<Unit,String>")?;
    let dns_err_seg = registry.string_literal_segment(b"tcp: dns resolve failed")?;
    let no_addr_seg = registry.string_literal_segment(b"tcp: dns no addresses")?;
    let sock_err_seg = registry.string_literal_segment(b"tcp: socket create failed")?;
    let conn_err_seg = registry.string_literal_segment(b"tcp: connect failed")?;
    let port_err_seg = registry.string_literal_segment(b"tcp: port out of range")?;
    let gates: &[Wasip2ImportSlot] = &[
        Wasip2ImportSlot::SocketsInstanceNetworkInstanceNetwork,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddresses,
        Wasip2ImportSlot::SocketsIpNameLookupResourceDropResolveAddressStream,
        Wasip2ImportSlot::SocketsIpNameLookupResolveAddressStreamSubscribe,
        Wasip2ImportSlot::IoPollPoll,
        Wasip2ImportSlot::IoPollResourceDropPollable,
        Wasip2ImportSlot::SocketsIpNameLookupResolveNextAddress,
        Wasip2ImportSlot::SocketsTcpCreateSocketCreateTcpSocket,
        Wasip2ImportSlot::SocketsTcpStartConnect,
        Wasip2ImportSlot::SocketsTcpSubscribe,
        Wasip2ImportSlot::SocketsTcpFinishConnect,
        Wasip2ImportSlot::SocketsTcpResourceDropTcpSocket,
        Wasip2ImportSlot::IoStreamsResourceDropInputStream,
        Wasip2ImportSlot::IoStreamsResourceDropOutputStream,
    ];
    for slot in gates {
        wasip2_imports.lookup_wasm_fn_idx(*slot)?;
    }

    let s_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(string_idx),
    });
    let res_ref = ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(res_unit_idx),
    });
    types.ty().function([s_ref, ValType::I64], [res_ref]);
    let ty = *next_type_idx;
    *next_type_idx += 1;
    let fn_idx = *next_builtin_fn_idx;
    *next_builtin_fn_idx += 1;
    Some(TcpPingIndices {
        fn_type: ty,
        fn_idx,
        string_type_idx: string_idx,
        dns_err_segment_idx: dns_err_seg,
        dns_err_len: b"tcp: dns resolve failed".len() as u32,
        no_addr_segment_idx: no_addr_seg,
        no_addr_len: b"tcp: dns no addresses".len() as u32,
        sock_err_segment_idx: sock_err_seg,
        sock_err_len: b"tcp: socket create failed".len() as u32,
        conn_err_segment_idx: conn_err_seg,
        conn_err_len: b"tcp: connect failed".len() as u32,
        port_err_segment_idx: port_err_seg,
        port_err_len: b"tcp: port out of range".len() as u32,
    })
}
