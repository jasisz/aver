/// String namespace — text manipulation helpers.
///
/// Methods:
///   String.len(s)               → Int            — char count (code points)
///   String.byteLength(s)        → Int            — byte count (UTF-8)
///   String.startsWith(s, pre)   → Bool
///   String.endsWith(s, suf)     → Bool
///   String.contains(s, sub)     → Bool
///   String.slice(s, from, to)   → String         — code-point based substring
///   String.trim(s)              → String
///   String.split(s, delim)      → List<String>
///   String.replace(s, old, new) → String
///   String.join(list, sep)      → String
///   String.charAt(s, index)     → Option<String>  — code-point based; repeated recursive access is indexed by runtime lowering
///   String.chars(s)             → List<String>   — each char as 1-char string
///   String.fromInt(n)           → String
///   String.fromFloat(f)         → String
///   String.fromBool(b)          → String
///   String.toUtf8(s)            → Bytes           — UTF-8 encoding; total
///   String.fromUtf8(bytes)      → Result<String, String>
///   String.toLower(s)           → String         — lowercase (Unicode-aware)
///   String.toUpper(s)           → String         — uppercase (Unicode-aware)
///
/// No effects required.
use crate::nan_value::{Arena, NanIntExt, NanString, NanValue};
use crate::value::RuntimeError;

/// Saturate an `Int` index to `i64` for an API that clamps to a length:
/// a magnitude past `i64` lands on the same boundary (`MIN`/`MAX`) the clamp
/// would reach anyway. No truncation, no panic.
fn saturate_index(n: &aver_rt::AverInt) -> i64 {
    match n.to_i64() {
        Some(v) => v,
        None if *n > aver_rt::AverInt::zero() => i64::MAX,
        None => i64::MIN,
    }
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "String.len" => Some(length_nv(args, arena)),
        "String.byteLength" => Some(byte_length_nv(args, arena)),
        "String.startsWith" => Some(starts_with_nv(args, arena)),
        "String.endsWith" => Some(ends_with_nv(args, arena)),
        "String.contains" => Some(contains_nv(args, arena)),
        "String.slice" => Some(slice_nv(args, arena)),
        "String.trim" => Some(trim_nv(args, arena)),
        "String.split" => Some(split_nv(args, arena)),
        "String.replace" => Some(replace_nv(args, arena)),
        "String.join" => Some(join_nv(args, arena)),
        "String.charAt" => Some(char_at_nv(args, arena)),
        "String.chars" => Some(chars_nv(args, arena)),
        "String.fromInt" => Some(from_int_nv(args, arena)),
        "String.fromFloat" => Some(from_float_nv(args, arena)),
        "String.fromBool" => Some(from_bool_nv(args, arena)),
        "String.toUtf8" => Some(to_utf8_nv(args, arena)),
        "String.fromUtf8" => Some(from_utf8_nv(args, arena)),
        "String.toLower" => Some(to_lower_nv(args, arena)),
        "String.toUpper" => Some(to_upper_nv(args, arena)),
        _ => None,
    }
}

fn nv_str(v: NanValue, arena: &Arena) -> Option<NanString<'_>> {
    if v.is_string() {
        Some(arena.get_string_value(v))
    } else {
        None
    }
}

fn length_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    let s = nv_str(args[0], arena)
        .ok_or_else(|| RuntimeError::Error("String.len: argument must be a String".to_string()))?;
    Ok(NanValue::new_int(s.chars().count() as i64, arena))
}

fn byte_length_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.byteLength() takes 1 argument, got {}",
            args.len()
        )));
    }
    let s = nv_str(args[0], arena).ok_or_else(|| {
        RuntimeError::Error("String.byteLength: argument must be a String".to_string())
    })?;
    Ok(NanValue::new_int(s.len() as i64, arena))
}

/// Encode the already-valid UTF-8 storage of an Aver `String` directly into
/// the nominal standard-library `Bytes` carrier. This is one byte walk and
/// one list allocation; it never materialises `String.chars` or code points.
fn to_utf8_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.toUtf8() takes 1 argument, got {}",
            args.len()
        )));
    }
    let Some(text) = nv_str(args[0], arena) else {
        return Err(RuntimeError::Error(
            "String.toUtf8: argument must be a String".to_string(),
        ));
    };
    let octets = text.as_bytes().to_vec();
    let values = octets
        .into_iter()
        .map(|octet| NanValue::new_int(i64::from(octet), arena))
        .collect();
    let list = NanValue::new_list(arena.push_list(values));
    let type_id = arena.find_type_id("Bytes").ok_or_else(|| {
        RuntimeError::Error("String.toUtf8: standard Bytes type is not loaded".to_string())
    })?;
    Ok(NanValue::new_record(arena.push_record(type_id, vec![list])))
}

/// Decode a nominal `Bytes` carrier as UTF-8. Invalid byte sequences are a
/// language value, not a runtime fault; malformed internal carriers remain a
/// fail-closed runtime error because user code cannot forge the opaque type.
fn from_utf8_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromUtf8() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_record() {
        return Err(RuntimeError::Error(
            "String.fromUtf8: argument must be Bytes".to_string(),
        ));
    }
    let expected = arena.find_type_id("Bytes").ok_or_else(|| {
        RuntimeError::Error("String.fromUtf8: standard Bytes type is not loaded".to_string())
    })?;
    let (actual, fields) = arena.get_record(args[0].arena_index());
    if actual != expected {
        return Err(RuntimeError::Error(
            "String.fromUtf8: argument must be Bytes".to_string(),
        ));
    }
    let Some(&values) = fields.first() else {
        return Err(RuntimeError::Error(
            "String.fromUtf8: malformed Bytes carrier".to_string(),
        ));
    };
    if !values.is_list() {
        return Err(RuntimeError::Error(
            "String.fromUtf8: malformed Bytes carrier".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(values);
    let mut octets = Vec::with_capacity(items.len());
    for (index, item) in items.into_iter().enumerate() {
        if !item.is_int() {
            return Err(RuntimeError::Error(format!(
                "String.fromUtf8: malformed byte at index {index}"
            )));
        }
        let value = item.as_aver_int(arena);
        let Some(value) = value.to_i64().and_then(|value| u8::try_from(value).ok()) else {
            return Err(RuntimeError::Error(format!(
                "String.fromUtf8: malformed byte {value} at index {index}"
            )));
        };
        octets.push(value);
    }
    match String::from_utf8(octets) {
        Ok(text) => {
            let inner = NanValue::new_string_value(&text, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(_) => {
            let error = NanValue::new_string_value("invalid UTF-8", arena);
            Ok(NanValue::new_err_value(error, arena))
        }
    }
}

fn starts_with_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.startsWith() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.startsWith: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]);
    let prefix = arena.get_string_value(args[1]);
    let result = s.starts_with(prefix.as_str());
    Ok(NanValue::new_bool(result))
}

fn ends_with_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.endsWith() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.endsWith: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]);
    let suffix = arena.get_string_value(args[1]);
    let result = s.ends_with(suffix.as_str());
    Ok(NanValue::new_bool(result))
}

fn contains_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.contains() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.contains: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]);
    let sub = arena.get_string_value(args[1]);
    let result = s.contains(sub.as_str());
    Ok(NanValue::new_bool(result))
}

fn slice_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.slice() takes 3 arguments (s, from, to), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.slice: first argument must be a String".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "String.slice: second argument must be an Int".to_string(),
        ));
    }
    if !args[2].is_int() {
        return Err(RuntimeError::Error(
            "String.slice: third argument must be an Int".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]).to_string();
    let from = saturate_index(&args[1].as_aver_int(arena));
    let to = saturate_index(&args[2].as_aver_int(arena));
    let result = aver_rt::string_slice(&s, from, to);
    Ok(NanValue::new_string_value(&result, arena))
}

fn trim_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.trim() takes 1 argument, got {}",
            args.len()
        )));
    }
    let s = nv_str(args[0], arena)
        .ok_or_else(|| RuntimeError::Error("String.trim: argument must be a String".to_string()))?;
    let result = s.trim().to_string();
    Ok(NanValue::new_string_value(&result, arena))
}

fn split_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.split() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.split: both arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]).to_string();
    let delim = arena.get_string_value(args[1]).to_string();
    let parts: Vec<NanValue> = s
        .split(&*delim)
        .map(|p| NanValue::new_string_value(p, arena))
        .collect();
    let list_idx = arena.push_list(parts);
    Ok(NanValue::new_list(list_idx))
}

fn replace_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "String.replace() takes 3 arguments (s, old, new), got {}",
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() || !args[2].is_string() {
        return Err(RuntimeError::Error(
            "String.replace: all arguments must be String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]).to_string();
    let old = arena.get_string_value(args[1]).to_string();
    let new = arena.get_string_value(args[2]).to_string();
    let result = s.replace(&*old, &new);
    Ok(NanValue::new_string_value(&result, arena))
}

fn join_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.join() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "String.join: first argument must be a List".to_string(),
        ));
    }
    if !args[1].is_string() {
        return Err(RuntimeError::Error(
            "String.join: second argument must be a String".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(args[0]);
    let sep = arena.get_string_value(args[1]).to_string();
    let mut strs: Vec<String> = Vec::with_capacity(items.len());
    for item in &items {
        if !item.is_string() {
            return Err(RuntimeError::Error(
                "String.join: list elements must be String".to_string(),
            ));
        }
        strs.push(arena.get_string_value(*item).to_string());
    }
    let result = strs.join(&sep);
    Ok(NanValue::new_string_value(&result, arena))
}

fn char_at_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "String.charAt() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.charAt: first argument must be a String".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "String.charAt: second argument must be an Int".to_string(),
        ));
    }
    let Some(idx_val) = args[1].as_aver_int(arena).to_usize() else {
        return Ok(NanValue::NONE);
    };
    let s = arena.get_string_value(args[0]);
    match s.chars().nth(idx_val) {
        Some(c) => {
            let cs = c.to_string();
            let inner = NanValue::new_string_value(&cs, arena);
            Ok(NanValue::new_some_value(inner, arena))
        }
        None => Ok(NanValue::NONE),
    }
}

fn chars_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.chars() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.chars: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]).to_string();
    let items: Vec<NanValue> = s
        .chars()
        .map(|c| NanValue::new_string_value(&c.to_string(), arena))
        .collect();
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}

fn from_int_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromInt() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "String.fromInt: argument must be an Int".to_string(),
        ));
    }
    // Exact decimal of the full ℤ value (bignum-aware via `AverInt: Display`).
    let s = format!("{}", args[0].as_aver_int(arena));
    Ok(NanValue::new_string_value(&s, arena))
}

fn from_float_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromFloat() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_float() {
        return Err(RuntimeError::Error(
            "String.fromFloat: argument must be a Float".to_string(),
        ));
    }
    let s = format!("{}", args[0].as_float());
    Ok(NanValue::new_string_value(&s, arena))
}

fn from_bool_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.fromBool() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_bool() {
        return Err(RuntimeError::Error(
            "String.fromBool: argument must be a Bool".to_string(),
        ));
    }
    let s = if args[0].as_bool() { "true" } else { "false" };
    Ok(NanValue::new_string_value(s, arena))
}

fn to_lower_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.toLower() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.toLower: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]).to_lowercase();
    Ok(NanValue::new_string_value(&s, arena))
}

fn to_upper_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "String.toUpper() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "String.toUpper: argument must be a String".to_string(),
        ));
    }
    let s = arena.get_string_value(args[0]).to_uppercase();
    Ok(NanValue::new_string_value(&s, arena))
}
