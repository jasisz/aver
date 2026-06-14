//! Type-driven `wasmtime::Val` ↔ `aver::replay::JsonValue` decoders +
//! the LM transport bridge that lets the host materialise wasm-gc
//! refs (Strings, Result/Option/Tuple structs, records) from trace
//! JSON. Everything in this module is read-only with respect to the
//! recorder: it converts already-recorded outcomes back into
//! engine-side values for replay, or surfaces a freshly-decoded
//! return value for the comparison against `recording.output`.
//!
//! Cross-module: per-effect `decode_*` helpers call back into
//! `super::imports` for the `__rt_record_*` factories that build the
//! actual wasm-gc structs. The split keeps the JSON-shape parsing
//! here and the host import dispatch / record bookkeeping there.

#![cfg(feature = "wasm")]

use super::RunWasmGcHost;
use super::imports::{
    host_http_response_make, host_map_string_list_string_empty, host_option_string_none,
    host_option_string_some, host_result_err_list_string, host_result_err_string,
    host_result_err_unit_string, host_result_http_response_err, host_result_http_response_ok,
    host_result_ok_list_string, host_result_ok_string, host_result_ok_unit,
    host_result_tcp_connection_err, host_result_tcp_connection_ok, host_tcp_connection_make,
    host_terminal_size_make, lm_string_from_host,
};

pub(crate) fn decode_main_return_typed(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    out: &[wasmtime::Val],
    ty: &aver::ast::Type,
) -> Result<aver::replay::JsonValue, String> {
    use aver::ast::Type;
    use aver::replay::JsonValue;
    match (ty, out) {
        (Type::Unit, []) | (Type::Unit, [_]) => Ok(JsonValue::Null),
        (_, []) => Err(format!("main returns no values but type is {:?}", ty)),
        (_, [single]) => decode_val_typed(store, instance, single, ty),
        (Type::Tuple(types), many) if many.len() == types.len() => {
            // Multi-value return — the wasm-gc lowering surfaces tuple
            // results directly on the wasm value stack instead of
            // wrapping them in a struct.
            let mut arr = Vec::with_capacity(many.len());
            for (v, t) in many.iter().zip(types.iter()) {
                arr.push(decode_val_typed(store, instance, v, t)?);
            }
            Ok(wrap_marker("$tuple", JsonValue::Array(arr)))
        }
        (_, _) => Err(format!(
            "main return shape {} values does not match type {:?}",
            out.len(),
            ty
        )),
    }
}

pub(super) fn wrap_marker(
    marker: &str,
    payload: aver::replay::JsonValue,
) -> aver::replay::JsonValue {
    let mut obj = std::collections::BTreeMap::new();
    obj.insert(marker.to_string(), payload);
    aver::replay::JsonValue::Object(obj)
}

pub(crate) fn decode_val_typed(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    val: &wasmtime::Val,
    ty: &aver::ast::Type,
) -> Result<aver::replay::JsonValue, String> {
    use aver::ast::Type;
    use aver::replay::JsonValue;
    use wasmtime::Val;
    match (ty, val) {
        (Type::Unit, _) => Ok(JsonValue::Null),
        (Type::Int, Val::I64(n)) => Ok(JsonValue::Int(*n)),
        (Type::Int, Val::I32(n)) => Ok(JsonValue::Int(*n as i64)),
        (Type::Float, Val::F64(b)) => Ok(JsonValue::Float(f64::from_bits(*b))),
        (Type::Float, Val::F32(b)) => Ok(JsonValue::Float(f32::from_bits(*b) as f64)),
        (Type::Bool, Val::I32(n)) => Ok(JsonValue::Bool(*n != 0)),
        (Type::Str, Val::AnyRef(opt)) => match opt {
            None => Ok(JsonValue::String(String::new())),
            Some(_) => decode_string_via_export(store, instance, val),
        },
        (Type::Result(ok_ty, err_ty), Val::AnyRef(Some(_))) => {
            decode_result_struct(store, instance, val, ok_ty, err_ty)
        }
        (Type::Option(inner), Val::AnyRef(Some(_))) => {
            decode_option_struct(store, instance, val, inner)
        }
        (Type::Tuple(types), Val::AnyRef(Some(_))) => {
            decode_tuple_struct(store, instance, val, types)
        }
        (Type::Tuple(_), Val::AnyRef(None)) => Err(
            "main returned null tuple ref — wasm-gc tuples are non-nullable structs".to_string(),
        ),
        // List / Map / Vector / Named / Fn / Var / Invalid: not yet
        // implemented. Hard error keeps the comparison honest.
        (other, _) => Err(format!(
            "wasm-gc replay: main return type {:?} not yet supported by the value decoder",
            other
        )),
    }
}

/// Convert literal `--expr` argument values (already parsed by
/// `parse_call_expression`) into `wasmtime::Val`s suitable for
/// passing as parameters to a wasm-gc-exported entry function.
/// Coverage matches the entry-call grammar's "literal-only" rule:
/// `Int` / `Float` / `Bool` / `Str` / `Unit`. Compound shapes
/// (`List`, `Tuple`, `Variant`, `Ok` / `Err` / `Some` / `None`) are
/// rejected with a clear error rather than silently coerced — the
/// wasm-gc lowering for those depends on registry state we don't
/// reconstruct from the host side.
pub(super) fn encode_entry_args_for_wasm_gc(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    args: &[aver::value::Value],
) -> Result<Vec<wasmtime::Val>, String> {
    use aver::value::Value;
    use wasmtime::Val;
    let mut out = Vec::with_capacity(args.len());
    for (idx, value) in args.iter().enumerate() {
        let val = match value {
            Value::Int(n) => match n.to_i64() {
                Some(i) => Val::I64(i),
                None => {
                    return Err(format!(
                        "wasm-gc entry arg #{}: Int value out of 64-bit range \
                         (the wasm-gc backend uses 64-bit integers)",
                        idx + 1
                    ));
                }
            },
            Value::Float(f) => Val::F64(f.to_bits()),
            Value::Bool(b) => Val::I32(if *b { 1 } else { 0 }),
            Value::Unit => continue, // Unit-typed param: no slot.
            Value::Str(s) => {
                let any_ref = lm_string_from_host_via_store(store, instance, s)?;
                Val::AnyRef(any_ref)
            }
            other => {
                return Err(format!(
                    "wasm-gc entry arg #{}: unsupported shape `{}` (entry args support \
                     Int / Float / Bool / String / Unit; nest compound values inside a \
                     helper fn and point --expr at that)",
                    idx + 1,
                    aver::value::aver_repr(other)
                ));
            }
        };
        out.push(val);
    }
    Ok(out)
}

/// `Store`-based mirror of the import-side `lm_string_from_host`:
/// copies a Rust `&str` into the LM transport region of the wasm-gc
/// module's `memory` and turns it into an Aver `String` ref via the
/// `__rt_string_from_lm` export. Used when we need to materialise
/// strings outside of an effect import callback (here: entry args
/// for `--expr` runs of `aver run --wasm-gc`).
pub(super) fn lm_string_from_host_via_store(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, String> {
    use wasmtime::Val;
    let from_lm = instance
        .get_func(&mut *store, "__rt_string_from_lm")
        .ok_or_else(|| "missing __rt_string_from_lm export".to_string())?;
    let memory = instance
        .get_memory(&mut *store, "memory")
        .ok_or_else(|| "missing memory export".to_string())?;
    let bytes = text.as_bytes();
    // Grow memory if the LM transport region is shorter than the
    // string. The transport region starts at offset 0 and the
    // module's own runtime allocates above 64 KiB, so the first page
    // is normally enough — but a literal entry arg larger than that
    // is conceivable.
    let needed_pages = (bytes.len() + 65535) >> 16;
    let cur_pages = memory.size(&store) as usize;
    if needed_pages > cur_pages {
        memory
            .grow(&mut *store, (needed_pages - cur_pages) as u64)
            .map_err(|e| format!("memory.grow for entry arg: {e:#}"))?;
    }
    memory
        .write(&mut *store, 0, bytes)
        .map_err(|e| format!("memory write for entry arg: {e:#}"))?;
    let mut out = [Val::AnyRef(None)];
    from_lm
        .call(&mut *store, &[Val::I32(bytes.len() as i32)], &mut out)
        .map_err(|e| format!("__rt_string_from_lm trap: {e:#}"))?;
    Ok(match &out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    })
}

/// Reuse the existing `__rt_string_to_lm` export to copy a wasm-gc
/// String AnyRef into host bytes. Mirrors `lm_string_to_host` but
/// takes a `Store` instead of a `Caller` (we're past the import
/// dispatch by the time `main` returns).
pub(crate) fn decode_string_via_export(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    val: &wasmtime::Val,
) -> Result<aver::replay::JsonValue, String> {
    use aver::replay::JsonValue;
    use wasmtime::Val;
    let to_lm = instance
        .get_func(&mut *store, "__rt_string_to_lm")
        .ok_or_else(|| "missing __rt_string_to_lm export".to_string())?;
    let memory = instance
        .get_memory(&mut *store, "memory")
        .ok_or_else(|| "missing memory export".to_string())?;
    let mut out = [Val::I32(0)];
    to_lm
        .call(&mut *store, std::slice::from_ref(val), &mut out)
        .map_err(|e| format!("__rt_string_to_lm trap: {e:#}"))?;
    let len = match out[0] {
        Val::I32(n) => n.max(0) as usize,
        _ => 0,
    };
    let mut buf = vec![0u8; len];
    if len > 0 {
        memory
            .read(&store, 0, &mut buf)
            .map_err(|e| format!("memory read for string decode: {e:#}"))?;
    }
    Ok(JsonValue::String(
        String::from_utf8_lossy(&buf).into_owned(),
    ))
}

/// `Result<T, E>` is a 3-field struct in wasm-gc lowering:
/// `(i32 tag, anyref ok, anyref err)`, tag=1 → Ok, tag=0 → Err.
/// Decode the active payload via the matching arm type and wrap in
/// the same `$ok`/`$err` marker the VM recorder writes, so VM and
/// wasm-gc traces stay byte-compatible.
pub(crate) fn decode_result_struct(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    val: &wasmtime::Val,
    ok_ty: &aver::ast::Type,
    err_ty: &aver::ast::Type,
) -> Result<aver::replay::JsonValue, String> {
    let (tag, fields) = read_struct(store, val)?;
    if fields.len() < 3 {
        return Err(format!(
            "Result struct expected 3 fields, got {}",
            fields.len()
        ));
    }
    if tag == 1 {
        let ok = decode_val_typed(store, instance, &fields[1], ok_ty)?;
        Ok(wrap_marker("$ok", ok))
    } else {
        let err = decode_val_typed(store, instance, &fields[2], err_ty)?;
        Ok(wrap_marker("$err", err))
    }
}

/// `Option<T>` is a 2-field struct: `(i32 tag, anyref payload)`,
/// tag=1 → Some, tag=0 → None. The recorder marker shape is
/// `{"$some": <inner>}` / `{"$none": true}`, matching the VM trace.
pub(crate) fn decode_option_struct(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    val: &wasmtime::Val,
    inner_ty: &aver::ast::Type,
) -> Result<aver::replay::JsonValue, String> {
    use aver::replay::JsonValue;
    let (tag, fields) = read_struct(store, val)?;
    if fields.len() < 2 {
        return Err(format!(
            "Option struct expected 2 fields, got {}",
            fields.len()
        ));
    }
    if tag == 1 {
        let inner = decode_val_typed(store, instance, &fields[1], inner_ty)?;
        Ok(wrap_marker("$some", inner))
    } else {
        Ok(wrap_marker("$none", JsonValue::Bool(true)))
    }
}

/// Tuple ref: positional struct, one field per element type.
pub(crate) fn decode_tuple_struct(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    instance: &wasmtime::Instance,
    val: &wasmtime::Val,
    types: &[aver::ast::Type],
) -> Result<aver::replay::JsonValue, String> {
    use aver::replay::JsonValue;
    let (_tag, fields) = read_struct(store, val)?;
    if fields.len() < types.len() {
        return Err(format!(
            "Tuple struct expected {} fields, got {}",
            types.len(),
            fields.len()
        ));
    }
    let mut arr = Vec::with_capacity(types.len());
    for (i, t) in types.iter().enumerate() {
        arr.push(decode_val_typed(store, instance, &fields[i], t)?);
    }
    Ok(wrap_marker("$tuple", JsonValue::Array(arr)))
}

/// Read all fields of a wasm-gc struct via the wasmtime GC API.
/// Returns `(tag, fields)` where `tag` is the first field if it's an
/// i32 (Result/Option/sum-variant tag), else 0. Fields are returned
/// in source order so callers can index them.
pub(super) fn read_struct(
    store: &mut wasmtime::Store<RunWasmGcHost>,
    val: &wasmtime::Val,
) -> Result<(i32, Vec<wasmtime::Val>), String> {
    use wasmtime::Val;
    let any_ref = match val {
        Val::AnyRef(Some(r)) => *r,
        Val::AnyRef(None) => return Err("expected struct ref, got null".to_string()),
        other => return Err(format!("expected AnyRef, got {:?}", other)),
    };
    let struct_ref = any_ref
        .as_struct(&*store)
        .map_err(|e| format!("anyref → struct cast: {e:#}"))?
        .ok_or_else(|| "anyref is not a struct".to_string())?;
    let ty = struct_ref
        .ty(&*store)
        .map_err(|e| format!("struct type lookup: {e:#}"))?;
    let n = ty.fields().len();
    let mut fields = Vec::with_capacity(n);
    for i in 0..n {
        fields.push(
            struct_ref
                .field(&mut *store, i)
                .map_err(|e| format!("struct field {i}: {e:#}"))?,
        );
    }
    let tag = match fields.first() {
        Some(Val::I32(n)) => *n,
        _ => 0,
    };
    Ok((tag, fields))
}
// ── Replay decoders: JsonValue → wasm-gc Val ────────────────────────
//
// These mirror the host-side `host_result_*` / `host_option_*` /
// `host_*_make` builders, but they take their inputs from the trace
// JSON instead of live Rust values. They share the same exports the
// recording path uses (`__rt_*`, `__rt_record_*_make`, etc.) — what
// changes is *who* supplies the bytes.

/// Decode a `JsonValue::String(s)` to a wasm-gc string ref via the
/// LM transport. Returns `Err` for any other JSON shape — the caller
/// already knows what shape the trace should hold from the effect's
/// declared return type.
pub(crate) fn decode_string<T: 'static>(
    caller: &mut wasmtime::Caller<'_, T>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    match json {
        aver::replay::JsonValue::String(s) => lm_string_from_host(caller, s),
        aver::replay::JsonValue::Null => Ok(None),
        other => Err(wasmtime::Error::msg(format!(
            "replay decode: expected String, got {:?}",
            other
        ))),
    }
}

/// Decode a `Result<String, String>` value from the trace to the
/// wasm-gc Result-of-string ref. Recognises the `{"$ok": <string>}`
/// and `{"$err": <string>}` markers `replay::value_to_json` emits.
pub(crate) fn decode_result_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, inner) = expect_marker(json, &["$ok", "$err"])?;
    match (marker, inner) {
        ("$ok", aver::replay::JsonValue::String(s)) => host_result_ok_string(caller, s),
        ("$err", aver::replay::JsonValue::String(s)) => host_result_err_string(caller, s),
        _ => Err(wasmtime::Error::msg(
            "replay decode Result<String, String>: unexpected payload",
        )),
    }
}

/// Decode a `Result<Unit, String>` value. `Result.Ok(())` lands as
/// `{"$ok": null}` in the trace (Unit serialises to `null`).
pub(crate) fn decode_result_unit(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, inner) = expect_marker(json, &["$ok", "$err"])?;
    match (marker, inner) {
        ("$ok", aver::replay::JsonValue::Null) => host_result_ok_unit(caller),
        ("$err", aver::replay::JsonValue::String(s)) => host_result_err_unit_string(caller, s),
        _ => Err(wasmtime::Error::msg(
            "replay decode Result<Unit, String>: unexpected payload",
        )),
    }
}

/// Decode a `Result<List<String>, String>` value.
pub(crate) fn decode_result_list_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, inner) = expect_marker(json, &["$ok", "$err"])?;
    match (marker, inner) {
        ("$ok", aver::replay::JsonValue::Array(items)) => {
            let names: Vec<String> = items
                .iter()
                .map(|v| match v {
                    aver::replay::JsonValue::String(s) => Ok(s.clone()),
                    other => Err(wasmtime::Error::msg(format!(
                        "replay decode List<String>: element is {:?}",
                        other
                    ))),
                })
                .collect::<Result<_, _>>()?;
            host_result_ok_list_string(caller, &names)
        }
        ("$err", aver::replay::JsonValue::String(s)) => host_result_err_list_string(caller, s),
        _ => Err(wasmtime::Error::msg(
            "replay decode Result<List<String>, String>: unexpected payload",
        )),
    }
}

/// Decode an `Option<String>` value. Markers: `{"$some": <string>}` /
/// `{"$none": true}`.
pub(crate) fn decode_option_string(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, inner) = expect_marker(json, &["$some", "$none"])?;
    match (marker, inner) {
        ("$some", aver::replay::JsonValue::String(s)) => host_option_string_some(caller, s),
        ("$none", _) => host_option_string_none(caller),
        _ => Err(wasmtime::Error::msg(
            "replay decode Option<String>: unexpected payload",
        )),
    }
}

/// Decode a `Result<HttpResponse, String>` value. `HttpResponse` is a
/// `$record` with `status: Int`, `body: String`, `headers: Map<…>`.
/// Headers cross as an empty map — the recording path emits the same
/// shape today.
pub(crate) fn decode_result_http_response(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, inner) = expect_marker(json, &["$ok", "$err"])?;
    match marker {
        "$err" => match inner {
            aver::replay::JsonValue::String(s) => host_result_http_response_err(caller, s),
            _ => Err(wasmtime::Error::msg(
                "replay decode Result<HttpResponse, String>.Err: payload not a String",
            )),
        },
        "$ok" => {
            let fields = expect_record(inner, "HttpResponse")?;
            let status = match fields.get("status") {
                Some(aver::replay::JsonValue::Int(n)) => *n,
                _ => 0,
            };
            let body = match fields.get("body") {
                Some(aver::replay::JsonValue::String(s)) => s.clone(),
                _ => String::new(),
            };
            let body_ref = lm_string_from_host(caller, &body)?;
            let headers_ref = host_map_string_list_string_empty(caller)?;
            let rec_ref = host_http_response_make(caller, status, body_ref, headers_ref)?;
            host_result_http_response_ok(caller, rec_ref)
        }
        _ => unreachable!(),
    }
}

/// Decode a `Result<Tcp.Connection, String>` value.
pub(crate) fn decode_result_tcp_connection(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let (marker, inner) = expect_marker(json, &["$ok", "$err"])?;
    match marker {
        "$err" => match inner {
            aver::replay::JsonValue::String(s) => host_result_tcp_connection_err(caller, s),
            _ => Err(wasmtime::Error::msg(
                "replay decode Result<Tcp.Connection, String>.Err: payload not a String",
            )),
        },
        "$ok" => {
            let fields = expect_record(inner, "Tcp.Connection")?;
            let id = match fields.get("id") {
                Some(aver::replay::JsonValue::String(s)) => s.clone(),
                _ => String::new(),
            };
            let host = match fields.get("host") {
                Some(aver::replay::JsonValue::String(s)) => s.clone(),
                _ => String::new(),
            };
            let port = match fields.get("port") {
                Some(aver::replay::JsonValue::Int(n)) => *n,
                _ => 0,
            };
            let id_ref = lm_string_from_host(caller, &id)?;
            let host_ref = lm_string_from_host(caller, &host)?;
            let rec_ref = host_tcp_connection_make(caller, id_ref, host_ref, port)?;
            host_result_tcp_connection_ok(caller, rec_ref)
        }
        _ => unreachable!(),
    }
}

/// Decode a `Terminal.Size` record.
pub(crate) fn decode_terminal_size(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    json: &aver::replay::JsonValue,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    let fields = expect_record(json, "Terminal.Size")?;
    let w = match fields.get("width") {
        Some(aver::replay::JsonValue::Int(n)) => *n,
        _ => 80,
    };
    let h = match fields.get("height") {
        Some(aver::replay::JsonValue::Int(n)) => *n,
        _ => 24,
    };
    host_terminal_size_make(caller, w, h)
}

/// Match the single-key marker wrapper `replay::value_to_json` emits
/// (`$ok`, `$err`, `$some`, `$none`, `$record`, `$tuple`, `$map`).
/// Returns the marker name and the inner payload.
pub(super) fn expect_marker<'a>(
    json: &'a aver::replay::JsonValue,
    allowed: &[&str],
) -> Result<(&'static str, &'a aver::replay::JsonValue), wasmtime::Error> {
    let aver::replay::JsonValue::Object(map) = json else {
        return Err(wasmtime::Error::msg(format!(
            "replay decode: expected wrapper Object, got {:?}",
            json
        )));
    };
    if map.len() != 1 {
        return Err(wasmtime::Error::msg(format!(
            "replay decode: wrapper Object should have 1 key, got {}",
            map.len()
        )));
    }
    let (key, val) = map.iter().next().expect("checked above");
    for tag in allowed {
        if key == tag {
            // Lifetime-extend the literal — `allowed` is `'static`-
            // borrowed through the call site.
            let static_tag: &'static str = match *tag {
                "$ok" => "$ok",
                "$err" => "$err",
                "$some" => "$some",
                "$none" => "$none",
                "$record" => "$record",
                "$tuple" => "$tuple",
                "$map" => "$map",
                _ => "$unknown",
            };
            return Ok((static_tag, val));
        }
    }
    Err(wasmtime::Error::msg(format!(
        "replay decode: unexpected marker {}, expected one of {:?}",
        key, allowed
    )))
}

/// Pull the `fields` map out of a `$record` wrapper, after checking
/// the recorded type name matches what the caller expects.
pub(super) fn expect_record<'a>(
    json: &'a aver::replay::JsonValue,
    expected_type: &str,
) -> Result<&'a std::collections::BTreeMap<String, aver::replay::JsonValue>, wasmtime::Error> {
    let (marker, payload) = expect_marker(json, &["$record"])?;
    debug_assert_eq!(marker, "$record");
    let aver::replay::JsonValue::Object(payload) = payload else {
        return Err(wasmtime::Error::msg(
            "replay decode $record: payload not an Object",
        ));
    };
    match payload.get("type") {
        Some(aver::replay::JsonValue::String(s)) if s == expected_type => {}
        Some(other) => {
            return Err(wasmtime::Error::msg(format!(
                "replay decode $record: type {:?} != expected {}",
                other, expected_type
            )));
        }
        None => return Err(wasmtime::Error::msg("replay decode $record: missing type")),
    }
    match payload.get("fields") {
        Some(aver::replay::JsonValue::Object(fields)) => Ok(fields),
        _ => Err(wasmtime::Error::msg(
            "replay decode $record: missing fields object",
        )),
    }
}
