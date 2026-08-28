/// Vector namespace — fixed-size indexed sequence helpers.
///
/// Methods:
///   Vector.new(size, default)     → Result<Vector<T>, String>
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
    let Some(size) = aver_rt::checked_vector_size(&args[0].as_aver_int(arena)) else {
        return Ok(NanValue::new_err_value(
            NanValue::new_string_value(&aver_rt::vector_size_error_message(), arena),
            arena,
        ));
    };
    let items = vec![args[1]; size];
    let vector = if items.is_empty() {
        NanValue::EMPTY_VECTOR
    } else {
        NanValue::new_vector(arena.push_vector(items))
    };
    Ok(NanValue::new_ok_value(vector, arena))
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
    // Read before taking: the take empties the entry, and the promise the
    // collector reads has to be carried across to the entry pushed below.
    let source_all_immediate =
        !source.is_empty_vector_immediate() && arena.vector_all_immediate(source.arena_index());
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
            // Every other element was already immediate or already not; the one
            // element this write stores is the only one that can change the
            // answer, and it is right here.
            all_immediate: source_all_immediate && args[2].heap_index().is_none(),
            holder_count: 0,
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

#[cfg(test)]
mod tests {
    use super::*;

    /// The answer the flag stands in for, walked from scratch.
    fn really_all_immediate(arena: &Arena, vector: NanValue) -> bool {
        arena
            .vector_ref_value(vector)
            .iter()
            .all(|element| element.heap_index().is_none())
    }

    /// Run the owned write and unwrap the vector it hands back.
    fn owned_set(arena: &mut Arena, vector: NanValue, at: i64, value: NanValue) -> NanValue {
        let index = NanValue::new_int(at, arena);
        let result =
            vec_set_nv_owned(&[vector, index, value], arena).expect("owned Vector.set succeeds");
        assert!(
            result.is_some(),
            "the index was in range, so this is a Some"
        );
        result.wrapper_inner(arena)
    }

    /// The owned `Vector.set` promises the collector exactly what it holds.
    ///
    /// It is the fourth producer of that promise and the only one outside
    /// `aver-memory` — that crate's own exactness test covers the builder, the
    /// in-place write and the mutable escape hatch, and cannot reach across the
    /// crate boundary to this one.
    ///
    /// It needs its own test because the promise is not re-proved where it is
    /// read: the collector returns a vector marked all-immediate without looking
    /// at it, so a wrong `true` here is silent. An element would keep an arena
    /// index that promotion has already renamed, and the program would go on to
    /// read whatever moved into that slot.
    #[test]
    fn the_owned_vector_set_reports_the_all_immediate_flag_exactly() {
        let mut arena = Arena::new();
        let zero = NanValue::new_int(0, &mut arena);
        let seven = NanValue::new_int(7, &mut arena);
        let five = NanValue::new_int(5, &mut arena);
        let heap_backed =
            NanValue::new_string_value("long enough that it has to live in the arena", &mut arena);
        assert!(
            heap_backed.heap_index().is_some(),
            "the test needs a value the collector could actually move"
        );

        // An immediate written over immediates keeps the promise.
        let offsets = NanValue::new_vector(arena.push_vector(vec![zero, seven]));
        let updated = owned_set(&mut arena, offsets, 1, five);
        assert!(arena.vector_all_immediate(updated.arena_index()));
        assert_eq!(
            arena.vector_all_immediate(updated.arena_index()),
            really_all_immediate(&arena, updated)
        );

        // The one heap value it stores gives the promise up.
        let mixed = owned_set(&mut arena, updated, 0, heap_backed);
        assert!(!arena.vector_all_immediate(mixed.arena_index()));
        assert_eq!(
            arena.vector_all_immediate(mixed.arena_index()),
            really_all_immediate(&arena, mixed)
        );

        // Writing an immediate over the OTHER element does not win the promise
        // back: the heap value is still in there, and the write only knows about
        // the element it wrote.
        let still_mixed = owned_set(&mut arena, mixed, 1, seven);
        assert!(!arena.vector_all_immediate(still_mixed.arena_index()));
        assert_eq!(
            arena.vector_all_immediate(still_mixed.arena_index()),
            really_all_immediate(&arena, still_mixed)
        );
    }
}
