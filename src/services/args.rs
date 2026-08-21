use crate::nan_value::{Arena, NanValue};
use crate::value::RuntimeError;

/// Args service — command-line arguments.
///
/// Methods:
///   Args.get() → List<String>   ! [Args.get]
pub const DECLARED_EFFECTS: &[&str] = &["Args.get"];

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Args.get" => &["Args.get"],
        _ => &[],
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    cli_args: &[String],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Args.get" => Some(get_args_nv(args, cli_args, arena)),
        _ => None,
    }
}

fn get_args_nv(
    args: &[NanValue],
    cli_args: &[String],
    arena: &mut Arena,
) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Args.get() takes 0 arguments, got {}",
            args.len()
        )));
    }
    let items: Vec<NanValue> = cli_args
        .iter()
        .map(|s| NanValue::new_string_value(s, arena))
        .collect();
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}
