/// List namespace — list manipulation helpers.
///
/// Methods:
///   List.len(list)           → Int                    — number of elements
///   List.prepend(val, list)  → List<T>                — prepend element
///   List.take(list, n)       → List<T>                — first `n` elements
///   List.drop(list, n)       → List<T>                — all but first `n` elements
///   List.concat(a, b)        → List<T>                — concatenate two lists
///   List.reverse(list)       → List<T>                — reverse elements
///   List.contains(list, val) → Bool                   — membership by `==`
///   List.zip(a, b)           → List<Tuple<A, B>>           — pair elements from two lists
///
/// No effects required.
use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::RuntimeError;

/// Clamp an `Int` count to a `usize` for `take`/`drop`: negatives become 0,
/// values past `usize` become `usize::MAX` (take/drop all). Total by design —
/// `take`/`drop` are defined for every ℤ count. Shared with the compiled Rust
/// backend so both spell the clamp the same way.
fn clamp_count(n: &aver_rt::AverInt) -> usize {
    aver_rt::clamp_list_count(n)
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "List.len" => Some(len_nv(args, arena)),
        "List.prepend" => Some(prepend_nv(args, arena)),
        "List.take" => Some(take_nv(args, arena)),
        "List.drop" => Some(drop_nv(args, arena)),
        "List.concat" => Some(concat_nv(args, arena)),
        "List.reverse" => Some(reverse_nv(args, arena)),
        "List.contains" => Some(contains_nv(args, arena)),
        "List.zip" => Some(zip_nv(args, arena)),
        _ => None,
    }
}

fn len_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.len() argument must be a List".to_string(),
        ));
    }
    Ok(NanValue::new_int(
        arena.list_len_value(args[0]) as i64,
        arena,
    ))
}

fn prepend_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.prepend() takes 2 arguments (val, list), got {}",
            args.len()
        )));
    }
    if !args[1].is_list() {
        return Err(RuntimeError::Error(
            "List.prepend() second argument must be a List".to_string(),
        ));
    }
    let list_idx = arena.push_list_prepend(args[0], args[1]);
    Ok(NanValue::new_list(list_idx))
}

fn take_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.take() takes 2 arguments (list, n), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.take() first argument must be a List".to_string(),
        ));
    }
    let count = if args[1].is_int() {
        clamp_count(&args[1].as_aver_int(arena))
    } else {
        return Err(RuntimeError::Error(
            "List.take() second argument must be an Int".to_string(),
        ));
    };
    // Walks the prefix it keeps instead of the list it was handed: reading the
    // whole list to answer about its first `count` elements made a walk that
    // steps with `take` cost the list once per step (issue #1181).
    Ok(arena.list_take(args[0], count))
}

fn drop_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.drop() takes 2 arguments (list, n), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.drop() first argument must be a List".to_string(),
        ));
    }
    let count = if args[1].is_int() {
        clamp_count(&args[1].as_aver_int(arena))
    } else {
        return Err(RuntimeError::Error(
            "List.drop() second argument must be an Int".to_string(),
        ));
    };
    // A view over the body it was given, not a copy of the remainder: the
    // same sharing a destructured tail gets (issue #913).
    Ok(arena.list_drop(args[0], count))
}

fn concat_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.concat() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.concat() first argument must be a List".to_string(),
        ));
    }
    if !args[1].is_list() {
        return Err(RuntimeError::Error(
            "List.concat() second argument must be a List".to_string(),
        ));
    }
    if arena.list_is_empty_value(args[0]) {
        return Ok(args[1]);
    }
    if arena.list_is_empty_value(args[1]) {
        return Ok(args[0]);
    }
    let list_idx = arena.push_list_concat(args[0], args[1]);
    Ok(NanValue::new_list(list_idx))
}

fn reverse_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.reverse() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.reverse() argument must be a List".to_string(),
        ));
    }
    let mut items = arena.list_to_vec_value(args[0]);
    items.reverse();
    if items.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(items);
    Ok(NanValue::new_list(list_idx))
}

fn contains_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.contains() takes 2 arguments (list, value), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.contains() first argument must be a List".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(args[0]);
    let target = args[1];
    let found = items.iter().any(|item| item.eq_in(target, arena));
    Ok(NanValue::new_bool(found))
}

fn zip_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "List.zip() takes 2 arguments (list, list), got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "List.zip() first argument must be a List".to_string(),
        ));
    }
    if !args[1].is_list() {
        return Err(RuntimeError::Error(
            "List.zip() second argument must be a List".to_string(),
        ));
    }
    let a = arena.list_to_vec_value(args[0]);
    let b = arena.list_to_vec_value(args[1]);
    let pairs: Vec<NanValue> = a
        .iter()
        .zip(b.iter())
        .map(|(x, y)| {
            let tuple_idx = arena.push_tuple(vec![*x, *y]);
            NanValue::new_tuple(tuple_idx)
        })
        .collect();
    if pairs.is_empty() {
        return Ok(NanValue::EMPTY_LIST);
    }
    let list_idx = arena.push_list(pairs);
    Ok(NanValue::new_list(list_idx))
}
