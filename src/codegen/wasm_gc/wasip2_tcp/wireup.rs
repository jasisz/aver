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

use super::super::types::TypeRegistry;
use super::super::wasip2_imports::{Wasip2ImportRegistry, Wasip2ImportSlot};
use super::{
    TcpCloseIndices, TcpConnectIndices, TcpPingIndices, TcpReadLineIndices, TcpSendIndices,
    TcpWriteLineIndices,
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
    pub read_line: Option<TcpReadLineIndices>,
    pub close: Option<TcpCloseIndices>,
    pub send: Option<TcpSendIndices>,
    pub ping: Option<TcpPingIndices>,
}

/// Walk the registry / wasi-import lookup table once and reserve a
/// `(type_idx, fn_idx)` pair for every `Tcp.*` helper whose gates
/// fire. Mutates `types` (one `.function(...)` per allocated helper)
/// and bumps `next_type_idx` / `next_builtin_fn_idx` in lockstep.
///
/// Ordering invariant: the inner blocks reserve indices in this
/// exact sequence — connect → format_id → parse_id → write_line →
/// read_line → close → send → ping. [`register_funcs`] and the
/// `emit_*` blocks in `module.rs` rely on it.
pub(in crate::codegen::wasm_gc) fn allocate(
    registry: &TypeRegistry,
    wasip2_imports: &Wasip2ImportRegistry,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> TcpHelpers {
    let connect = allocate_connect(
        registry,
        wasip2_imports,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let format_id = allocate_format_id(
        registry,
        &connect,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let parse_id = allocate_parse_id(
        registry,
        &connect,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let write_line = allocate_write_line(
        registry,
        wasip2_imports,
        parse_id,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let read_line = allocate_read_line(
        registry,
        wasip2_imports,
        parse_id,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let close = allocate_close(
        registry,
        wasip2_imports,
        parse_id,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let send = allocate_send(
        registry,
        &connect,
        &write_line,
        &read_line,
        &close,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    let ping = allocate_ping(
        registry,
        &connect,
        &close,
        types,
        next_type_idx,
        next_builtin_fn_idx,
    );
    TcpHelpers {
        connect,
        format_id,
        parse_id,
        write_line,
        read_line,
        close,
        send,
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
    if let Some(t) = &helpers.read_line {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.close {
        funcs.function(t.fn_type);
    }
    if let Some(t) = &helpers.send {
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
    connect: &Option<TcpConnectIndices>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<(u32, u32)> {
    connect.as_ref()?;
    let s_idx = registry
        .string_array_type_idx
        .expect("tcp_parse_id allocation gated on tcp_connect which requires the string slot");
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
    })
}

#[allow(clippy::too_many_arguments)]
fn allocate_send(
    registry: &TypeRegistry,
    connect: &Option<TcpConnectIndices>,
    write_line: &Option<TcpWriteLineIndices>,
    read_line: &Option<TcpReadLineIndices>,
    close: &Option<TcpCloseIndices>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpSendIndices> {
    connect.as_ref()?;
    write_line.as_ref()?;
    read_line.as_ref()?;
    close.as_ref()?;
    let string_idx = registry.string_array_type_idx?;
    let res_conn_idx = registry.result_type_idx("Result<Tcp.Connection,String>")?;
    let res_unit_idx = registry.result_type_idx("Result<Unit,String>")?;
    let res_string_idx = registry.result_type_idx("Result<String,String>")?;

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
        result_tcp_conn_string_type_idx: res_conn_idx,
        result_unit_string_type_idx: res_unit_idx,
    })
}

fn allocate_ping(
    registry: &TypeRegistry,
    connect: &Option<TcpConnectIndices>,
    close: &Option<TcpCloseIndices>,
    types: &mut TypeSection,
    next_type_idx: &mut u32,
    next_builtin_fn_idx: &mut u32,
) -> Option<TcpPingIndices> {
    connect.as_ref()?;
    close.as_ref()?;
    let string_idx = registry.string_array_type_idx?;
    let res_conn_idx = registry.result_type_idx("Result<Tcp.Connection,String>")?;
    let res_unit_idx = registry.result_type_idx("Result<Unit,String>")?;

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
        result_tcp_conn_string_type_idx: res_conn_idx,
    })
}
