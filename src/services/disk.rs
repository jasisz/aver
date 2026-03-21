/// Disk service — file-system I/O.
///
/// Eight methods covering the full CRUD surface for files and directories:
///   readText   — read a file as a UTF-8 string
///   writeText  — write (overwrite) a file
///   appendText — append to a file, creating it if absent
///   exists     — check whether a path exists (returns Bool, not Result)
///   delete     — remove a **file** (Err if given a directory — use deleteDir)
///   deleteDir  — recursively remove a **directory** (Err if given a file)
///   listDir    — list entry names in a directory
///   makeDir    — create a directory and all missing parents (mkdir -p)
///
/// Each method requires its own exact effect (`Disk.readText`, `Disk.writeText`, etc.).
use std::collections::HashMap;
use std::rc::Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, list_from_vec};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "readText",
        "writeText",
        "appendText",
        "exists",
        "delete",
        "deleteDir",
        "listDir",
        "makeDir",
    ] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Disk.{}", method)),
        );
    }
    global.insert(
        "Disk".to_string(),
        Value::Namespace {
            name: "Disk".to_string(),
            members,
        },
    );
}

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Disk.readText" => &["Disk.readText"],
        "Disk.writeText" => &["Disk.writeText"],
        "Disk.appendText" => &["Disk.appendText"],
        "Disk.exists" => &["Disk.exists"],
        "Disk.delete" => &["Disk.delete"],
        "Disk.deleteDir" => &["Disk.deleteDir"],
        "Disk.listDir" => &["Disk.listDir"],
        "Disk.makeDir" => &["Disk.makeDir"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Disk.readText" => Some(read_text(args)),
        "Disk.writeText" => Some(write_text(args)),
        "Disk.appendText" => Some(append_text(args)),
        "Disk.exists" => Some(exists(args)),
        "Disk.delete" => Some(delete(args)),
        "Disk.deleteDir" => Some(delete_dir(args)),
        "Disk.listDir" => Some(list_dir(args)),
        "Disk.makeDir" => Some(make_dir(args)),
        _ => None,
    }
}

// ─── Implementations ──────────────────────────────────────────────────────────

fn read_text(args: &[Value]) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.readText", args)?;
    match aver_rt::read_text(&path) {
        Ok(text) => Ok(Value::Ok(Box::new(Value::Str(text)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn write_text(args: &[Value]) -> Result<Value, RuntimeError> {
    let (path, content) = two_str_args("Disk.writeText", args)?;
    match aver_rt::write_text(&path, &content) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn append_text(args: &[Value]) -> Result<Value, RuntimeError> {
    let (path, content) = two_str_args("Disk.appendText", args)?;
    match aver_rt::append_text(&path, &content) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn exists(args: &[Value]) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.exists", args)?;
    Ok(Value::Bool(aver_rt::path_exists(&path)))
}

fn delete(args: &[Value]) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.delete", args)?;
    match aver_rt::delete_file(&path) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn delete_dir(args: &[Value]) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.deleteDir", args)?;
    match aver_rt::delete_dir(&path) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn list_dir(args: &[Value]) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.listDir", args)?;
    match aver_rt::list_dir(&path) {
        Ok(entries) => Ok(Value::Ok(Box::new(list_from_vec(
            entries.into_iter().map(Value::Str).collect(),
        )))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn make_dir(args: &[Value]) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.makeDir", args)?;
    match aver_rt::make_dir(&path) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

// ─── Argument helpers ─────────────────────────────────────────────────────────

fn one_str_arg(fn_name: &str, args: &[Value]) -> Result<String, RuntimeError> {
    match args {
        [Value::Str(s)] => Ok(s.clone()),
        [_] => Err(RuntimeError::Error(format!(
            "{}: path must be a String",
            fn_name
        ))),
        _ => Err(RuntimeError::Error(format!(
            "{}() takes 1 argument (path), got {}",
            fn_name,
            args.len()
        ))),
    }
}

fn two_str_args(fn_name: &str, args: &[Value]) -> Result<(String, String), RuntimeError> {
    match args {
        [Value::Str(a), Value::Str(b)] => Ok((a.clone(), b.clone())),
        [a, b] => Err(RuntimeError::Error(format!(
            "{}: both arguments must be Strings (got {}, {})",
            fn_name,
            crate::value::aver_repr(a),
            crate::value::aver_repr(b)
        ))),
        _ => Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments (path, content), got {}",
            fn_name,
            args.len()
        ))),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &[
        "readText",
        "writeText",
        "appendText",
        "exists",
        "delete",
        "deleteDir",
        "listDir",
        "makeDir",
    ];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Disk.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Disk"),
        members,
    });
    global.insert("Disk".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Disk.readText" => Some(read_text_nv(args, arena)),
        "Disk.writeText" => Some(write_text_nv(args, arena)),
        "Disk.appendText" => Some(append_text_nv(args, arena)),
        "Disk.exists" => Some(exists_nv(args, arena)),
        "Disk.delete" => Some(delete_nv(args, arena)),
        "Disk.deleteDir" => Some(delete_dir_nv(args, arena)),
        "Disk.listDir" => Some(list_dir_nv(args, arena)),
        "Disk.makeDir" => Some(make_dir_nv(args, arena)),
        _ => None,
    }
}

fn nv_one_str(fn_name: &str, args: &[NanValue], arena: &Arena) -> Result<String, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument (path), got {}",
            fn_name,
            args.len()
        )));
    }
    if !args[0].is_string() {
        return Err(RuntimeError::Error(format!(
            "{}: path must be a String",
            fn_name
        )));
    }
    Ok(arena.get_string_value(args[0]).to_string())
}

fn nv_two_str(
    fn_name: &str,
    args: &[NanValue],
    arena: &Arena,
) -> Result<(String, String), RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments (path, content), got {}",
            fn_name,
            args.len()
        )));
    }
    if !args[0].is_string() || !args[1].is_string() {
        return Err(RuntimeError::Error(format!(
            "{}: both arguments must be Strings (got {}, {})",
            fn_name,
            args[0].type_name(),
            args[1].type_name()
        )));
    }
    Ok((
        arena.get_string_value(args[0]).to_string(),
        arena.get_string_value(args[1]).to_string(),
    ))
}

fn nv_ok_unit(arena: &mut Arena) -> NanValue {
    NanValue::new_ok_value(NanValue::UNIT, arena)
}

fn nv_ok_str(s: &str, arena: &mut Arena) -> NanValue {
    let inner = NanValue::new_string_value(s, arena);
    NanValue::new_ok_value(inner, arena)
}

fn nv_err_str(s: &str, arena: &mut Arena) -> NanValue {
    let inner = NanValue::new_string_value(s, arena);
    NanValue::new_err_value(inner, arena)
}

fn read_text_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let path = nv_one_str("Disk.readText", args, arena)?;
    match aver_rt::read_text(&path) {
        Ok(text) => Ok(nv_ok_str(&text, arena)),
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}

fn write_text_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (path, content) = nv_two_str("Disk.writeText", args, arena)?;
    match aver_rt::write_text(&path, &content) {
        Ok(_) => Ok(nv_ok_unit(arena)),
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}

fn append_text_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let (path, content) = nv_two_str("Disk.appendText", args, arena)?;
    match aver_rt::append_text(&path, &content) {
        Ok(_) => Ok(nv_ok_unit(arena)),
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}

fn exists_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let path = nv_one_str("Disk.exists", args, arena)?;
    Ok(NanValue::new_bool(aver_rt::path_exists(&path)))
}

fn delete_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let path = nv_one_str("Disk.delete", args, arena)?;
    match aver_rt::delete_file(&path) {
        Ok(_) => Ok(nv_ok_unit(arena)),
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}

fn delete_dir_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let path = nv_one_str("Disk.deleteDir", args, arena)?;
    match aver_rt::delete_dir(&path) {
        Ok(_) => Ok(nv_ok_unit(arena)),
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}

fn list_dir_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let path = nv_one_str("Disk.listDir", args, arena)?;
    match aver_rt::list_dir(&path) {
        Ok(entries) => {
            let items: Vec<NanValue> = entries
                .into_iter()
                .map(|s| NanValue::new_string_value(&s, arena))
                .collect();
            let list_idx = arena.push_list(items);
            let inner = NanValue::new_list(list_idx);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}

fn make_dir_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    let path = nv_one_str("Disk.makeDir", args, arena)?;
    match aver_rt::make_dir(&path) {
        Ok(_) => Ok(nv_ok_unit(arena)),
        Err(e) => Ok(nv_err_str(&e.to_string(), arena)),
    }
}
