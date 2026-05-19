//! LM (linear-memory) transport bridge between the wasm-gc engine
//! and the host. Strings cross as a pair `(byte length, payload at
//! memory offset 0)`. `__rt_string_from_lm` / `__rt_string_to_lm`
//! exports do the engine-side allocation; this module wraps them
//! into ergonomic Rust helpers + the `host_print` Console writer
//! that uses the same path.

use super::super::RunWasmGcHost;

pub(crate) fn val_i64(v: &wasmtime::Val) -> Option<i64> {
    match v {
        wasmtime::Val::I64(n) => Some(*n),
        _ => None,
    }
}

/// Print one Aver string to stdout/stderr via the LM transport bridge.
pub(crate) fn host_print(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    to_stdout: bool,
) -> Result<(), wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match params.first() {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(()),
    };
    let Some(any_ref) = any_ref else {
        return Ok(());
    };
    let to_lm = caller
        .get_export("__rt_string_to_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let (Some(to_lm), Some(memory)) = (to_lm, memory) else {
        return Ok(());
    };
    let mut out = [Val::I32(0)];
    if to_lm
        .call(&mut *caller, &[Val::AnyRef(Some(any_ref))], &mut out)
        .is_ok()
    {
        let len = match out[0] {
            Val::I32(n) => n,
            _ => 0,
        };
        let text: String = if len > 0 {
            let mut buf = vec![0u8; len as usize];
            let _ = memory.read(&caller, 0, &mut buf);
            String::from_utf8_lossy(&buf).into_owned()
        } else {
            // Empty string still emits a newline in VM semantics —
            // `write_stdout` appends the newline itself.
            String::new()
        };
        // Route through `aver::services::console` instead of direct
        // `println!` / `eprintln!` so the in-process parity fuzz
        // target's `capture_output` thread-local sees wasm-gc writes
        // too (the VM already routes here). Direct stdio bypassed
        // the buffer, hiding semantic divergence behind a "wasm-gc
        // stdout looks empty when captured" symptom.
        if to_stdout {
            crate::services::console::write_stdout_str(&text);
        } else {
            crate::services::console::write_stderr_plain_str(&text);
        }
    }
    Ok(())
}

/// Decode an Aver String ref (passed as `any_ref` in the import ABI)
/// back to a Rust `String` via the LM transport bridge.
pub(crate) fn lm_string_to_host(
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    val: Option<&wasmtime::Val>,
) -> Result<Option<String>, wasmtime::Error> {
    use wasmtime::Val;
    let any_ref = match val {
        Some(Val::AnyRef(r)) => *r,
        _ => return Ok(None),
    };
    let Some(any_ref) = any_ref else {
        return Ok(None);
    };
    let to_lm = caller
        .get_export("__rt_string_to_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let (Some(to_lm), Some(memory)) = (to_lm, memory) else {
        return Ok(None);
    };
    let mut out = [Val::I32(0)];
    to_lm.call(&mut *caller, &[Val::AnyRef(Some(any_ref))], &mut out)?;
    let len = match out[0] {
        Val::I32(n) => n.max(0) as usize,
        _ => 0,
    };
    let mut buf = vec![0u8; len];
    if len > 0 {
        memory.read(&caller, 0, &mut buf)?;
    }
    Ok(Some(String::from_utf8_lossy(&buf).into_owned()))
}

/// Materialise a host-supplied UTF-8 string as an Aver string ref via
/// the LM transport bridge: write bytes into linear memory at offset 0,
/// call `__rt_string_from_lm(len)` to wrap them in an `(array i8)`.
/// Returns the resulting AnyRef so the caller can hand it back as a
/// host import result.
pub(crate) fn lm_string_from_host<T: 'static>(
    caller: &mut wasmtime::Caller<'_, T>,
    text: &str,
) -> Result<Option<wasmtime::Rooted<wasmtime::AnyRef>>, wasmtime::Error> {
    use wasmtime::*;
    let bytes = text.as_bytes();
    let from_lm = caller
        .get_export("__rt_string_from_lm")
        .and_then(|e| e.into_func());
    let memory = caller.get_export("memory").and_then(|e| e.into_memory());
    let grow = caller
        .get_export("__rt_memory_grow")
        .and_then(|e| e.into_func());
    let pages = caller
        .get_export("__rt_memory_pages")
        .and_then(|e| e.into_func());
    let (Some(from_lm), Some(memory), Some(grow), Some(pages)) = (from_lm, memory, grow, pages)
    else {
        return Ok(None);
    };
    // Grow LM if the string doesn't fit. One page = 64 KiB; small
    // strings fit by default since the bridge ships with `(memory 1)`.
    let needed_pages = ((bytes.len() + 65535) >> 16) as i32;
    let mut current_out = [Val::I32(0)];
    pages.call(&mut *caller, &[], &mut current_out)?;
    let current_pages = match current_out[0] {
        Val::I32(n) => n,
        _ => 0,
    };
    if needed_pages > current_pages {
        let mut grow_out = [Val::I32(0)];
        grow.call(
            &mut *caller,
            &[Val::I32(needed_pages - current_pages)],
            &mut grow_out,
        )?;
    }
    memory.write(&mut *caller, 0, bytes)?;
    let mut from_lm_out = [Val::AnyRef(None)];
    from_lm.call(
        &mut *caller,
        &[Val::I32(bytes.len() as i32)],
        &mut from_lm_out,
    )?;
    let r = match &from_lm_out[0] {
        Val::AnyRef(r) => *r,
        _ => None,
    };
    Ok(r)
}
