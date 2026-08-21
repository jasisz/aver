/// Vector namespace — fixed-size indexed sequence helpers.
///
/// Methods:
///   Vector.new(size, default)     → Vector<T>
///   Vector.get(vec, idx)          → Option<T>
///   Vector.set(vec, idx, val)     → Option<Vector<T>>
///   Vector.len(vec)               → Int
///   Vector.fromList(xs)           → Vector<T>
///   List.fromVector(vec)            → List<T>
///
/// No effects required.
use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::RuntimeError;

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Vector.new" => Some(vec_new_nv(args, arena)),
        "Vector.get" => Some(vec_get_nv(args, arena)),
        "Vector.set" => Some(vec_set_nv(args, arena)),
        "Vector.len" => Some(vec_len_nv(args, arena)),
        "Vector.fromList" => Some(vec_from_list_nv(args, arena)),
        "List.fromVector" => Some(vec_to_list_nv(args, arena)),
        _ => None,
    }
}

fn vec_new_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Vector.new() takes 2 arguments (size, default), got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Vector.new: size must be an Int".to_string(),
        ));
    }
    let Some(size) = args[0].as_aver_int(arena).to_usize() else {
        // Negative, or larger than `usize` can address: cannot allocate.
        return Err(RuntimeError::Error(
            "Vector.new: size must be a non-negative, machine-sized Int".to_string(),
        ));
    };
    let items = vec![args[1]; size];
    if items.is_empty() {
        Ok(NanValue::EMPTY_VECTOR)
    } else {
        Ok(NanValue::new_vector(arena.push_vector(items)))
    }
}

fn vec_get_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "Vector.get() takes 2 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.get: first argument must be a Vector".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "Vector.get: index must be an Int".to_string(),
        ));
    }
    let Some(idx) = args[1].as_aver_int(arena).to_usize() else {
        return Ok(NanValue::NONE);
    };
    let items = arena.vector_ref_value(args[0]);
    match items.get(idx) {
        Some(&v) => Ok(NanValue::new_some_value(v, arena)),
        None => Ok(NanValue::NONE),
    }
}

/// Vector.set with sole-owned first argument — takes instead of cloning.
pub fn vec_set_nv_owned(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Vector.set() takes 3 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.set: first argument must be a Vector".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "Vector.set: index must be an Int".to_string(),
        ));
    }
    let Some(uidx) = args[1].as_aver_int(arena).to_usize() else {
        return Ok(NanValue::NONE);
    };
    let source = args[0];
    let mut items = arena.take_vector_value(source);
    if uidx >= items.len() {
        return Ok(NanValue::NONE);
    }
    items[uidx] = args[2];
    // The one element this write stores is the one child the O(1) path owes a
    // held-elsewhere mark — every other element was marked when the source
    // vector was built, and the flags live on the pointed-to entries, so they
    // survive the take-and-repush. Mirror of the owned `Map.set`'s single-pair
    // marking.
    arena.note_held_elsewhere(args[2]);
    let new_vec_idx = arena.push_inheriting_source_space(
        aver_memory::ArenaEntry::Vector {
            items,
            held_elsewhere: false,
        },
        source,
    );
    Ok(NanValue::new_some_value(
        NanValue::new_vector(new_vec_idx),
        arena,
    ))
}

fn vec_set_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::Error(format!(
            "Vector.set() takes 3 arguments, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.set: first argument must be a Vector".to_string(),
        ));
    }
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "Vector.set: index must be an Int".to_string(),
        ));
    }
    let Some(uidx) = args[1].as_aver_int(arena).to_usize() else {
        return Ok(NanValue::NONE);
    };
    let mut items = arena.clone_vector_value(args[0]);
    if uidx >= items.len() {
        return Ok(NanValue::NONE);
    }
    items[uidx] = args[2];
    let new_vec_idx = arena.push_vector(items);
    Ok(NanValue::new_some_value(
        NanValue::new_vector(new_vec_idx),
        arena,
    ))
}

fn vec_len_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.len() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "Vector.len: argument must be a Vector".to_string(),
        ));
    }
    let items = arena.vector_ref_value(args[0]);
    Ok(NanValue::new_int(items.len() as i64, arena))
}

fn vec_from_list_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Vector.fromList() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_list() {
        return Err(RuntimeError::Error(
            "Vector.fromList: argument must be a List".to_string(),
        ));
    }
    let items = arena.list_to_vec_value(args[0]);
    if items.is_empty() {
        Ok(NanValue::EMPTY_VECTOR)
    } else {
        Ok(NanValue::new_vector(arena.push_vector(items)))
    }
}

fn vec_to_list_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "List.fromVector() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_vector() {
        return Err(RuntimeError::Error(
            "List.fromVector: argument must be a Vector".to_string(),
        ));
    }
    let items = arena.clone_vector_value(args[0]);
    if items.is_empty() {
        Ok(NanValue::EMPTY_LIST)
    } else {
        let list_idx = arena.push_list(items);
        Ok(NanValue::new_list(list_idx))
    }
}
