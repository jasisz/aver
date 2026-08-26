//! Primitive calls into compiler-emitted wasm-gc provider ABI helpers.

use std::str::FromStr;

use aver::ast::Type;
use wasmtime::{Caller, Val, ValType};

use super::RunWasmGcHost;

fn helper_stem(ty: &Type) -> String {
    format!(
        "__cap_abi_{}",
        aver::codegen::wasip2::plan::encode_interface_identifier(&ty.display())
    )
}

pub(super) fn helper_call(
    caller: &mut Caller<'_, RunWasmGcHost>,
    ty: &Type,
    suffix: &str,
    params: &[Val],
) -> Result<Vec<Val>, String> {
    let name = format!("{}_{suffix}", helper_stem(ty));
    let function = caller
        .get_export(&name)
        .and_then(|export| export.into_func())
        .ok_or_else(|| format!("missing wasm-gc provider ABI helper '{name}'"))?;
    let result_types = function.ty(&*caller).results().collect::<Vec<_>>();
    let mut results = result_types
        .iter()
        .map(default_result_value)
        .collect::<Vec<_>>();
    function
        .call(&mut *caller, params, &mut results)
        .map_err(|error| format!("provider ABI helper '{name}' trapped: {error:#}"))?;
    Ok(results)
}

pub(super) fn helper_optional_call(
    caller: &mut Caller<'_, RunWasmGcHost>,
    ty: &Type,
    suffix: &str,
    params: &[Val],
) -> Result<Option<Val>, String> {
    let results = helper_call(caller, ty, suffix, params)?;
    match results.as_slice() {
        [] => Ok(None),
        [value] => Ok(Some(*value)),
        _ => Err(format!(
            "provider ABI helper '{}_{suffix}' returned {} values",
            helper_stem(ty),
            results.len()
        )),
    }
}

pub(super) fn helper_value(
    caller: &mut Caller<'_, RunWasmGcHost>,
    ty: &Type,
    suffix: &str,
    params: &[Val],
) -> Result<Val, String> {
    helper_optional_call(caller, ty, suffix, params)?.ok_or_else(|| {
        format!(
            "provider ABI helper '{}_{suffix}' returned Unit",
            helper_stem(ty)
        )
    })
}

pub(super) fn helper_optional_value(
    caller: &mut Caller<'_, RunWasmGcHost>,
    ty: &Type,
    suffix: &str,
    param: &Val,
) -> Result<Option<Val>, String> {
    helper_optional_call(caller, ty, suffix, std::slice::from_ref(param))
}

pub(super) fn helper_i32(
    caller: &mut Caller<'_, RunWasmGcHost>,
    ty: &Type,
    suffix: &str,
    params: &[Val],
) -> Result<i32, String> {
    match helper_value(caller, ty, suffix, params)? {
        Val::I32(value) => Ok(value),
        other => Err(format!(
            "provider ABI helper '{}_{suffix}' returned {other:?}, expected i32",
            helper_stem(ty)
        )),
    }
}

fn default_result_value(ty: &ValType) -> Val {
    Val::default_for_ty(ty).unwrap_or(Val::AnyRef(None))
}

pub(super) fn required_val<'a>(value: Option<&'a Val>, ty: &Type) -> Result<&'a Val, String> {
    value.ok_or_else(|| format!("expected {}, received no wasm value", ty.display()))
}

pub(super) fn expected_value(ty: &Type, value: Option<&Val>) -> String {
    format!("expected {}, received {value:?}", ty.display())
}

pub(super) fn encode_string(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &str,
) -> Result<Val, String> {
    super::super::imports::lm_string_from_host(caller, value)
        .map(Val::AnyRef)
        .map_err(|error| format!("encode String through LM: {error:#}"))
}

pub(super) fn decode_string(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
) -> Result<String, String> {
    super::super::imports::lm_string_to_host(caller, Some(value))
        .map_err(|error| format!("decode String through LM: {error:#}"))?
        .ok_or_else(|| "expected non-null String reference".to_string())
}

pub(super) fn encode_bytes(
    caller: &mut Caller<'_, RunWasmGcHost>,
    bytes: &[u8],
) -> Result<Val, String> {
    let memory = caller
        .get_export("memory")
        .and_then(|export| export.into_memory())
        .ok_or_else(|| "missing wasm-gc LM memory for Bytes".to_string())?;
    let len = i32::try_from(bytes.len())
        .map_err(|_| "Bytes payload exceeds wasm32 linear memory".to_string())?;
    let needed_pages = ((bytes.len() as u64) + 65_535) >> 16;
    let current_pages = memory.size(&*caller);
    if needed_pages > current_pages {
        memory
            .grow(&mut *caller, needed_pages - current_pages)
            .map_err(|error| format!("grow LM for Bytes: {error:#}"))?;
    }
    memory
        .write(&mut *caller, 0, bytes)
        .map_err(|error| format!("write Bytes to LM: {error:#}"))?;
    let function = caller
        .get_export("__rt_bytes_from_lm")
        .and_then(|export| export.into_func())
        .ok_or_else(|| "missing __rt_bytes_from_lm export".to_string())?;
    let mut result = [Val::AnyRef(None)];
    function
        .call(&mut *caller, &[Val::I32(len)], &mut result)
        .map_err(|error| format!("__rt_bytes_from_lm trapped: {error:#}"))?;
    Ok(result[0])
}

pub(super) fn decode_bytes(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
) -> Result<Vec<u8>, String> {
    let function = caller
        .get_export("__rt_bytes_to_lm")
        .and_then(|export| export.into_func())
        .ok_or_else(|| "missing __rt_bytes_to_lm export".to_string())?;
    let memory = caller
        .get_export("memory")
        .and_then(|export| export.into_memory())
        .ok_or_else(|| "missing wasm-gc LM memory for Bytes".to_string())?;
    let mut result = [Val::I32(0)];
    function
        .call(&mut *caller, std::slice::from_ref(value), &mut result)
        .map_err(|error| format!("__rt_bytes_to_lm trapped: {error:#}"))?;
    let len = match result[0] {
        Val::I32(value) => value.max(0) as usize,
        _ => 0,
    };
    let mut bytes = vec![0; len];
    if len > 0 {
        memory
            .read(&*caller, 0, &mut bytes)
            .map_err(|error| format!("read Bytes from LM: {error:#}"))?;
    }
    Ok(bytes)
}

pub(super) fn encode_int(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &aver_rt::AverInt,
) -> Result<Val, String> {
    let int_ty = Type::Int;
    if let Some(value) = value.to_i64() {
        return helper_value(caller, &int_ty, "from_i64", &[Val::I64(value)]);
    }
    let text = encode_string(caller, &value.to_string())?;
    let parsed = helper_value(caller, &int_ty, "from_decimal", &[text])?;
    let result_ty = Type::Result(Box::new(Type::Int), Box::new(Type::Str));
    if helper_i32(caller, &result_ty, "tag", std::slice::from_ref(&parsed))? != 1 {
        return Err("compiler-emitted Int decimal bridge rejected a valid AverInt".into());
    }
    helper_value(
        caller,
        &result_ty,
        "ok_value",
        std::slice::from_ref(&parsed),
    )
}

pub(super) fn decode_int(
    caller: &mut Caller<'_, RunWasmGcHost>,
    value: &Val,
) -> Result<aver_rt::AverInt, String> {
    let text = helper_value(
        caller,
        &Type::Int,
        "to_decimal",
        std::slice::from_ref(value),
    )?;
    let text = decode_string(caller, &text)?;
    aver_rt::AverInt::from_str(&text).map_err(|_| format!("invalid Int decimal '{text}'"))
}
