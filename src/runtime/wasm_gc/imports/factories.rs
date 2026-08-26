//! `__rt_record_*` factory builders that materialise wasm-gc structs
//! from host-side values. Used both by the per-effect arms (real
//! call → wasm-gc Val) and by the replay decoders (trace JSON →
//! wasm-gc Val), so the engine sees identical struct shapes
//! regardless of whether the value originated from a live host call
//! or from a recording.

use super::super::RunWasmGcHost;
use super::lm::{lm_result_bytes_from_host, lm_string_from_host, lm_string_to_host};

pub(crate) fn host_option_string_some(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_option_string_some")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build an `Option<String>::None` ref via `__rt_option_string_none`.
pub(crate) fn host_option_string_none(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_option_string_none")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_option_string_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_option_string_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(value)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_option_string_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_option_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Terminal.Size(width, height)` record via the
/// `__rt_record_terminal_size_make` factory.
pub(crate) fn host_terminal_size_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    width: i64,
    height: i64,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_terminal_size_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::I64(width), Val::I64(height)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build an `Http.Response(status, body, headers)` ref via the matching
/// factory export.
pub(crate) fn host_http_response_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    status: i64,
    body: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
    headers: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_http_response_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(
        &mut *caller,
        &[Val::I64(status), Val::AnyRef(body), Val::AnyRef(headers)],
        &mut out,
    )?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_http_response_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    resp: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_http_response_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(resp)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_http_response_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_http_response_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_map_string_list_string_empty(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_map_string_list_string_empty")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Tcp.Connection` record from host-side primitives via the
/// `__rt_record_tcp_connection_make(id, host, port)` factory.
pub(crate) fn host_tcp_connection_make(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    id: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
    host: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
    port: i64,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_record_tcp_connection_make")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(
        &mut *caller,
        &[Val::AnyRef(id), Val::AnyRef(host), Val::I64(port)],
        &mut out,
    )?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Read the `id` field of a `Tcp.Connection` record via
/// `__rt_tcp_connection_id`. Returns `None` when the record ref is
/// null (shouldn't happen for a successful connect, but bail safely).
pub(crate) fn host_tcp_connection_id(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<Option<String>, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(None),
    };
    let Some(_) = any_ref else { return Ok(None) };
    let getter = caller
        .get_export("__rt_tcp_connection_id")
        .and_then(|e| e.into_func());
    let Some(getter) = getter else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    getter.call(&mut *caller, &[Val::AnyRef(any_ref)], &mut out)?;
    lm_string_to_host(caller, Some(&out[0]))
}

/// Ask the guest's nominal type hierarchy which `Tcp.Socket` variant this
/// value inhabits. Returning an explicit tag avoids guessing from payload
/// layout (`Tcp.Dial` and `Tcp.Listener` are both opaque one-field records).
pub(crate) fn host_tcp_socket_kind(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<Option<i32>, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(None),
    };
    let Some(_) = any_ref else { return Ok(None) };
    let getter = caller
        .get_export("__rt_tcp_socket_kind")
        .and_then(|e| e.into_func());
    let Some(getter) = getter else {
        return Ok(None);
    };
    let mut out = [Val::I32(-1)];
    getter.call(&mut *caller, &[Val::AnyRef(any_ref)], &mut out)?;
    Ok(match out[0] {
        Val::I32(kind) if kind >= 0 => Some(kind),
        _ => None,
    })
}

pub(crate) fn host_result_tcp_connection_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    conn: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_tcp_connection_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(conn)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_tcp_connection_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_tcp_connection_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

fn host_result_one_string_arg(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    export: &str,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let text = match lm_string_from_host(caller, text)? {
        Some(value) => value,
        None => return Ok(None),
    };
    let factory = caller.get_export(export).and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(text))], &mut out)?;
    Ok(match out[0] {
        Val::AnyRef(value) => value,
        _ => None,
    })
}

pub(crate) fn host_result_tcp_dial_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    id: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    host_result_one_string_arg(caller, "__rt_result_tcp_dial_string_ok", id)
}

pub(crate) fn host_result_tcp_dial_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    error: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    host_result_one_string_arg(caller, "__rt_result_tcp_dial_string_err", error)
}

pub(crate) fn host_result_tcp_listener_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    id: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    host_result_one_string_arg(caller, "__rt_result_tcp_listener_string_ok", id)
}

pub(crate) fn host_result_tcp_listener_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    error: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    host_result_one_string_arg(caller, "__rt_result_tcp_listener_string_err", error)
}

pub(crate) fn host_result_option_tcp_connection_some(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    connection: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_option_tcp_connection_string_some")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(connection)], &mut out)?;
    Ok(match out[0] {
        Val::AnyRef(value) => value,
        _ => None,
    })
}

pub(crate) fn host_result_option_tcp_connection_none(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_option_tcp_connection_string_none")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match out[0] {
        Val::AnyRef(value) => value,
        _ => None,
    })
}

pub(crate) fn host_result_option_tcp_connection_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    error: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    host_result_one_string_arg(
        caller,
        "__rt_result_option_tcp_connection_string_err",
        error,
    )
}

/// `Result<Unit, String>::Ok(())` via the matching factory export.
pub(crate) fn host_result_ok_unit(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_unit_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_err_unit_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_unit_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Wrap a wasm-owned `Terminal.Size` record in
/// `Result<Terminal.Size, String>::Ok`.
pub(crate) fn host_result_terminal_size_ok(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    record: Option<wasmtime::Rooted<wasmtime::AnyRef>>,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let factory = caller
        .get_export("__rt_result_terminal_size_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(record)], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    })
}

pub(crate) fn host_result_terminal_size_err(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let text = match lm_string_from_host(caller, text)? {
        Some(value) => value,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_terminal_size_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(text))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    })
}

/// Build a `Result<List<String>, String>::Ok(list)` ref. The list is
/// constructed bottom-up via repeated `__rt_list_string_cons` calls,
/// terminated by `__rt_list_string_nil`.
pub(crate) fn host_result_ok_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    items: &[String],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let nil = caller
        .get_export("__rt_list_string_nil")
        .and_then(|e| e.into_func());
    let cons = caller
        .get_export("__rt_list_string_cons")
        .and_then(|e| e.into_func());
    let factory = caller
        .get_export("__rt_result_list_string_string_ok")
        .and_then(|e| e.into_func());
    let (Some(nil), Some(cons), Some(factory)) = (nil, cons, factory) else {
        return Ok(None);
    };
    let mut tail_out = [Val::AnyRef(None)];
    nil.call(&mut *caller, &[], &mut tail_out)?;
    let mut current = match &tail_out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    };
    // Cons in reverse so the resulting list keeps the input order.
    for text in items.iter().rev() {
        let head = match lm_string_from_host(caller, text)? {
            Some(r) => r,
            None => return Ok(None),
        };
        let mut next = [Val::AnyRef(None)];
        cons.call(
            &mut *caller,
            &[Val::AnyRef(Some(head)), Val::AnyRef(current)],
            &mut next,
        )?;
        current = match &next[0] {
            Val::AnyRef(r) => *r,
            _ => None,
        };
    }
    let mut wrapped = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(current)], &mut wrapped)?;
    Ok(match &wrapped[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_err_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_list_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_ok_list_int_refs(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    items: &[wasmtime::Rooted<wasmtime::AnyRef>],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let nil = caller
        .get_export("__rt_list_int_nil")
        .and_then(|export| export.into_func());
    let cons = caller
        .get_export("__rt_list_int_cons")
        .and_then(|export| export.into_func());
    let factory = caller
        .get_export("__rt_result_list_int_string_ok")
        .and_then(|export| export.into_func());
    let (Some(nil), Some(cons), Some(factory)) = (nil, cons, factory) else {
        return Ok(None);
    };
    let mut tail = [Val::AnyRef(None)];
    nil.call(&mut *caller, &[], &mut tail)?;
    let mut current = match &tail[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    };
    for item in items.iter().rev() {
        let mut next = [Val::AnyRef(None)];
        cons.call(
            &mut *caller,
            &[Val::AnyRef(Some(*item)), Val::AnyRef(current)],
            &mut next,
        )?;
        current = match &next[0] {
            Val::AnyRef(value) => *value,
            _ => None,
        };
    }
    let mut result = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(current)], &mut result)?;
    Ok(match &result[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    })
}

pub(crate) fn host_result_err_list_int(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let text = match lm_string_from_host(caller, text)? {
        Some(value) => value,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_list_int_string_err")
        .and_then(|export| export.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut result = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(text))], &mut result)?;
    Ok(match &result[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    })
}

/// Build a `Result<Bytes, String>::Ok(bytes)` ref. Packed and boxed modules
/// copy the whole payload through linear memory in one boundary crossing. The
/// element-wise path is retained as a defensive fallback for older modules.
pub(crate) fn host_result_ok_bytes(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    items: &[i64],
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let bytes = items
        .iter()
        .map(|item| {
            u8::try_from(*item).map_err(|_| {
                wasmtime::Error::msg(format!(
                    "Bytes provider returned an octet outside 0..255: {item}"
                ))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    if let Some(result) = lm_result_bytes_from_host(caller, &bytes)? {
        return Ok(Some(result));
    }

    let nil = caller
        .get_export("__rt_list_int_nil")
        .and_then(|e| e.into_func());
    let cons = caller
        .get_export("__rt_list_int_cons")
        .and_then(|e| e.into_func());
    let from_i64 = caller
        .get_export("__rt_aint_from_i64")
        .and_then(|e| e.into_func());
    let factory = caller
        .get_export("__rt_result_bytes_string_ok")
        .and_then(|e| e.into_func());
    let (Some(nil), Some(cons), Some(from_i64), Some(factory)) = (nil, cons, from_i64, factory)
    else {
        return Ok(None);
    };
    let mut tail_out = [Val::AnyRef(None)];
    nil.call(&mut *caller, &[], &mut tail_out)?;
    let mut current = match &tail_out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    };
    for item in items.iter().rev() {
        let mut head_out = [Val::AnyRef(None)];
        from_i64.call(&mut *caller, &[Val::I64(*item)], &mut head_out)?;
        let head = match &head_out[0] {
            Val::AnyRef(r) => *r,
            _ => None,
        };
        let mut next = [Val::AnyRef(None)];
        cons.call(
            &mut *caller,
            &[Val::AnyRef(head), Val::AnyRef(current)],
            &mut next,
        )?;
        current = match &next[0] {
            Val::AnyRef(r) => *r,
            _ => None,
        };
    }
    let mut wrapped = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(current)], &mut wrapped)?;
    Ok(match &wrapped[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_err_bytes(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_bytes_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

pub(crate) fn host_result_ok_int(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    value: i64,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let from_i64 = caller
        .get_export("__rt_aint_from_i64")
        .and_then(|export| export.into_func());
    let factory = caller
        .get_export("__rt_result_int_string_ok")
        .and_then(|export| export.into_func());
    let (Some(from_i64), Some(factory)) = (from_i64, factory) else {
        return Ok(None);
    };
    let mut int = [Val::AnyRef(None)];
    from_i64.call(&mut *caller, &[Val::I64(value)], &mut int)?;
    let mut result = [Val::AnyRef(None)];
    factory.call(&mut *caller, &int, &mut result)?;
    Ok(match &result[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    })
}

pub(crate) fn host_result_err_int(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let text = match lm_string_from_host(caller, text)? {
        Some(value) => value,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_int_string_err")
        .and_then(|export| export.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut result = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(text))], &mut result)?;
    Ok(match &result[0] {
        Val::AnyRef(value) => *value,
        _ => None,
    })
}

/// Build a `Result<String,String>::Ok(text)` ref by calling the
/// module's exported factory `__rt_result_string_string_ok`. Returns
/// `Ok(None)` if the factory or string bridge isn't exported (the
/// program declared `Console.readLine` but didn't reach the type
/// registration that materialises the factory).
pub(crate) fn host_result_ok_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_string_string_ok")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Build a `Result<String,String>::Err(text)` ref via the symmetric
/// `__rt_result_string_string_err` factory.
pub(crate) fn host_result_err_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::Val;
    let s = match lm_string_from_host(caller, text)? {
        Some(r) => r,
        None => return Ok(None),
    };
    let factory = caller
        .get_export("__rt_result_string_string_err")
        .and_then(|e| e.into_func());
    let Some(factory) = factory else {
        return Ok(None);
    };
    let mut out = [Val::AnyRef(None)];
    factory.call(&mut *caller, &[Val::AnyRef(Some(s))], &mut out)?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}
