//! BranchPath — opaque identifier for a branch in the structural tree of
//! `!`/`?!` groups. Used in Oracle-proof specs for generative effects
//! (`oracle : (BranchPath, Int, args...) -> T`) and, via the trace-API
//! `.path()` bridge, for tying trace events to oracle invocations.
//!
//! Three constructors — the only way to obtain a `BranchPath`:
//!
//! - `BranchPath.root : () -> BranchPath`                  — canonical root.
//! - `BranchPath.child : (BranchPath, Int) -> BranchPath`  — extend a path by
//!   entering a branch of a group.
//! - `BranchPath.parse : String -> BranchPath`             — parse dewey-decimal
//!   (`"2.0"`), used to bridge recordings to specs.
//!
//! Internal representation is a record `{ dewey: String }` — user code
//! cannot construct arbitrary paths because the field is not exposed as a
//! public member and record-literal construction of built-in types isn't
//! available at the surface.

use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::nan_value::{Arena, ArenaEntry, NanValue};
use crate::value::RuntimeError;

pub const TYPE_NAME: &str = "BranchPath";
const FIELD: &str = "dewey";

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["root", "child", "parse"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("{}.{}", TYPE_NAME, method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(ArenaEntry::Namespace {
        name: Rc::from(TYPE_NAME),
        members,
    });
    global.insert(TYPE_NAME.to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "BranchPath.root" => Some(root_nv(args, arena)),
        "BranchPath.child" => Some(child_nv(args, arena)),
        "BranchPath.parse" => Some(parse_nv(args, arena)),
        _ => None,
    }
}

fn make_path(dewey: &str, arena: &mut Arena) -> NanValue {
    let type_id = arena
        .find_type_id(TYPE_NAME)
        .expect("BranchPath type must be registered via register_service_types");
    let dewey_value = NanValue::new_string_value(dewey, arena);
    let record_idx = arena.push_record(type_id, vec![dewey_value]);
    NanValue::new_record(record_idx)
}

fn read_dewey(value: NanValue, arena: &Arena, method: &str) -> Result<String, RuntimeError> {
    if !value.is_record() {
        return Err(RuntimeError::Error(format!(
            "{}: argument must be a BranchPath, got non-record value",
            method
        )));
    }
    let (type_id, fields) = arena.get_record(value.arena_index());
    let expected = arena
        .find_type_id(TYPE_NAME)
        .expect("BranchPath type must be registered");
    if type_id != expected {
        return Err(RuntimeError::Error(format!(
            "{}: argument must be a BranchPath, got a different record type",
            method
        )));
    }
    let Some(field) = fields.first() else {
        return Err(RuntimeError::Error(format!(
            "{}: BranchPath record is missing its `{}` field",
            method, FIELD
        )));
    };
    if !field.is_string() {
        return Err(RuntimeError::Error(format!(
            "{}: BranchPath.{} must be a String",
            method, FIELD
        )));
    }
    Ok(arena.get_string_value(*field).to_string())
}

fn root_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "BranchPath.root() takes 0 arguments, got {}",
            args.len()
        )));
    }
    Ok(make_path("", arena))
}

fn child_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "BranchPath.child() takes 2 arguments (path, idx), got {}",
            args.len()
        )));
    }
    let parent = read_dewey(args[0], arena, "BranchPath.child")?;
    if !args[1].is_int() {
        return Err(RuntimeError::Error(
            "BranchPath.child: `idx` must be an Int".to_string(),
        ));
    }
    let idx = args[1].as_int(arena);
    if idx < 0 {
        return Err(RuntimeError::Error(format!(
            "BranchPath.child: `idx` must be non-negative, got {}",
            idx
        )));
    }
    let dewey = if parent.is_empty() {
        idx.to_string()
    } else {
        format!("{}.{}", parent, idx)
    };
    Ok(make_path(&dewey, arena))
}

fn parse_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "BranchPath.parse() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(
            "BranchPath.parse: argument must be a String".to_string(),
        ));
    }
    let raw = arena.get_string_value(args[0]).to_string();
    if !is_valid_dewey(&raw) {
        return Err(RuntimeError::Error(format!(
            "BranchPath.parse: `{}` is not a valid dewey-decimal path (expected empty string for root or dot-separated non-negative integers like \"0\", \"2.0\")",
            raw
        )));
    }
    Ok(make_path(&raw, arena))
}

fn is_valid_dewey(s: &str) -> bool {
    if s.is_empty() {
        return true;
    }
    s.split('.')
        .all(|seg| !seg.is_empty() && seg.bytes().all(|b| b.is_ascii_digit()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fresh_arena() -> Arena {
        let mut arena = Arena::new();
        crate::vm::register_service_types(&mut arena);
        arena
    }

    #[test]
    fn root_is_empty_dewey() {
        let mut arena = fresh_arena();
        let path = root_nv(&[], &mut arena).unwrap();
        let dewey = read_dewey(path, &arena, "test").unwrap();
        assert_eq!(dewey, "");
    }

    #[test]
    fn child_of_root_is_index() {
        let mut arena = fresh_arena();
        let root = root_nv(&[], &mut arena).unwrap();
        let idx = NanValue::new_int(3, &mut arena);
        let child = child_nv(&[root, idx], &mut arena).unwrap();
        let dewey = read_dewey(child, &arena, "test").unwrap();
        assert_eq!(dewey, "3");
    }

    #[test]
    fn nested_child_joins_with_dot() {
        let mut arena = fresh_arena();
        let root = root_nv(&[], &mut arena).unwrap();
        let two = NanValue::new_int(2, &mut arena);
        let branch2 = child_nv(&[root, two], &mut arena).unwrap();
        let zero = NanValue::new_int(0, &mut arena);
        let nested = child_nv(&[branch2, zero], &mut arena).unwrap();
        let dewey = read_dewey(nested, &arena, "test").unwrap();
        assert_eq!(dewey, "2.0");
    }

    #[test]
    fn parse_accepts_valid_dewey() {
        let mut arena = fresh_arena();
        for &raw in &["", "0", "1", "2.0", "10.3.4"] {
            let s = NanValue::new_string_value(raw, &mut arena);
            let path = parse_nv(&[s], &mut arena).expect(raw);
            let dewey = read_dewey(path, &arena, "test").unwrap();
            assert_eq!(dewey, raw);
        }
    }

    #[test]
    fn parse_rejects_non_dewey() {
        let mut arena = fresh_arena();
        for &raw in &[".", "1.", ".1", "1..2", "abc", "1.a", "-1", " ", "1.2.3."] {
            let s = NanValue::new_string_value(raw, &mut arena);
            let result = parse_nv(&[s], &mut arena);
            assert!(result.is_err(), "{} should be rejected", raw);
        }
    }

    #[test]
    fn child_rejects_negative_idx() {
        let mut arena = fresh_arena();
        let root = root_nv(&[], &mut arena).unwrap();
        let neg = NanValue::new_int(-1, &mut arena);
        let result = child_nv(&[root, neg], &mut arena);
        assert!(result.is_err());
    }

    #[test]
    fn child_rejects_non_path_first_arg() {
        let mut arena = fresh_arena();
        let garbage = NanValue::new_string_value("not a path", &mut arena);
        let idx = NanValue::new_int(0, &mut arena);
        let result = child_nv(&[garbage, idx], &mut arena);
        assert!(result.is_err());
    }
}
