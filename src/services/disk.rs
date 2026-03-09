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
        [_, _] => Err(RuntimeError::Error(format!(
            "{}: both arguments must be Strings",
            fn_name
        ))),
        _ => Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments (path, content), got {}",
            fn_name,
            args.len()
        ))),
    }
}
