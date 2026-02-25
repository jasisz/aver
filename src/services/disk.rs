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
/// All methods require `! [Disk]`.
use std::collections::HashMap;

use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &[
        "readText", "writeText", "appendText", "exists",
        "delete", "deleteDir", "listDir", "makeDir",
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
        "Disk.readText"
        | "Disk.writeText"
        | "Disk.appendText"
        | "Disk.exists"
        | "Disk.delete"
        | "Disk.deleteDir"
        | "Disk.listDir"
        | "Disk.makeDir" => &["Disk"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: Vec<Value>) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Disk.readText"   => Some(read_text(args)),
        "Disk.writeText"  => Some(write_text(args)),
        "Disk.appendText" => Some(append_text(args)),
        "Disk.exists"     => Some(exists(args)),
        "Disk.delete"     => Some(delete(args)),
        "Disk.deleteDir"  => Some(delete_dir(args)),
        "Disk.listDir"    => Some(list_dir(args)),
        "Disk.makeDir"    => Some(make_dir(args)),
        _ => None,
    }
}

// ─── Implementations ──────────────────────────────────────────────────────────

fn read_text(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.readText", args)?;
    match std::fs::read_to_string(&path) {
        Ok(text) => Ok(Value::Ok(Box::new(Value::Str(text)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn write_text(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let (path, content) = two_str_args("Disk.writeText", args)?;
    match std::fs::write(&path, &content) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn append_text(args: Vec<Value>) -> Result<Value, RuntimeError> {
    use std::io::Write;
    let (path, content) = two_str_args("Disk.appendText", args)?;
    match std::fs::OpenOptions::new().create(true).append(true).open(&path) {
        Ok(mut f) => match f.write_all(content.as_bytes()) {
            Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
            Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
        },
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn exists(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.exists", args)?;
    Ok(Value::Bool(std::path::Path::new(&path).exists()))
}

fn delete(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.delete", args)?;
    let p = std::path::Path::new(&path);
    if p.is_dir() {
        return Ok(Value::Err(Box::new(Value::Str(
            "Disk.delete: path is a directory — use Disk.deleteDir to remove directories".to_string(),
        ))));
    }
    match std::fs::remove_file(p) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn delete_dir(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.deleteDir", args)?;
    let p = std::path::Path::new(&path);
    if !p.is_dir() {
        return Ok(Value::Err(Box::new(Value::Str(
            "Disk.deleteDir: path is not a directory — use Disk.delete to remove files".to_string(),
        ))));
    }
    match std::fs::remove_dir_all(p) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn list_dir(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.listDir", args)?;
    match std::fs::read_dir(&path) {
        Ok(entries) => {
            let mut names = Vec::new();
            for entry in entries {
                match entry {
                    Ok(e) => names.push(Value::Str(e.file_name().to_string_lossy().into_owned())),
                    Err(e) => return Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
                }
            }
            Ok(Value::Ok(Box::new(Value::List(names))))
        }
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

fn make_dir(args: Vec<Value>) -> Result<Value, RuntimeError> {
    let path = one_str_arg("Disk.makeDir", args)?;
    match std::fs::create_dir_all(&path) {
        Ok(_) => Ok(Value::Ok(Box::new(Value::Unit))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e.to_string())))),
    }
}

// ─── Argument helpers ─────────────────────────────────────────────────────────

fn one_str_arg(fn_name: &str, args: Vec<Value>) -> Result<String, RuntimeError> {
    match args.as_slice() {
        [Value::Str(s)] => Ok(s.clone()),
        [_] => Err(RuntimeError::Error(format!("{}: path must be a String", fn_name))),
        _ => Err(RuntimeError::Error(format!(
            "{}() takes 1 argument (path), got {}",
            fn_name,
            args.len()
        ))),
    }
}

fn two_str_args(fn_name: &str, args: Vec<Value>) -> Result<(String, String), RuntimeError> {
    match args.as_slice() {
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
