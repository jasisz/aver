/// Console service — terminal I/O.
///
/// Methods:
///   Console.print(msg)    — print to stdout (same as top-level `print`)
///   Console.error(msg)    — print to stderr (no colour, raw message)
///   Console.warn(msg)     — print to stderr prefixed with "[warn] "
///   Console.readLine()    — read one line from stdin; Ok(line) or Err("EOF")
///
/// Each method requires its own exact effect (`Console.print`, `Console.error`, etc.).
///
/// Output capture (Iron 0.21 Hardcore Fuzz — parity target):
///   `capture_output(|| { ... })` redirects every `Console.print` /
///   `Console.error` / `Console.warn` call inside the closure into
///   thread-local in-memory buffers. The closure's return value
///   plus `(stdout_bytes, stderr_bytes)` come back to the caller.
///   When no capture is active, the macros fall through to
///   `println!` / `eprintln!` exactly as before. The flag is per
///   thread so a long-running aver process that spawns a verify
///   worker doesn't accidentally swallow the user's output.
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc as Rc;

use crate::nan_value::{Arena, NanValue};
use crate::value::{RuntimeError, Value, aver_display};

thread_local! {
    /// Per-thread capture buffers. `None` (default) means writes go
    /// through to the process stdio fds; `Some((out, err))` means
    /// they accumulate in the buffers and the caller will retrieve
    /// them via [`finish_capture`].
    static CAPTURE: RefCell<Option<(Vec<u8>, Vec<u8>)>> = const { RefCell::new(None) };
}

/// Run `f` with all `Console.*` writes inside it redirected to
/// in-memory buffers. Returns `(f_result, stdout_bytes,
/// stderr_bytes)`. Nesting is illegal — the outer call's buffers
/// would mix with the inner's; we panic rather than silently
/// merge. The expected user is the fuzz harness, which never
/// nests captures.
pub fn capture_output<F, R>(f: F) -> (R, Vec<u8>, Vec<u8>)
where
    F: FnOnce() -> R,
{
    CAPTURE.with(|c| {
        let mut slot = c.borrow_mut();
        if slot.is_some() {
            panic!("capture_output: nested capture is not supported");
        }
        *slot = Some((Vec::new(), Vec::new()));
    });
    let result = f();
    let (out, err) = CAPTURE.with(|c| c.borrow_mut().take()).unwrap_or_default();
    (result, out, err)
}

/// Capture-aware `println!`-equivalent — embedders that bypass the
/// `Console.print` builtin (currently the wasm-gc backend's import
/// handler in `runtime::wasm_gc::imports::lm`) should call this
/// instead of raw `println!` so the thread-local `capture_output`
/// buffer sees their writes too.
pub fn write_stdout_str(s: &str) {
    write_stdout(s);
}

/// Capture-aware `eprintln!`-equivalent. Same rationale as
/// `write_stdout_str`.
pub fn write_stderr_plain_str(s: &str) {
    write_stderr_plain(s);
}

fn write_stdout(s: &str) {
    CAPTURE.with(|c| {
        if let Some((out, _)) = c.borrow_mut().as_mut() {
            out.extend_from_slice(s.as_bytes());
            out.push(b'\n');
        } else {
            println!("{}", s);
        }
    });
}

fn write_stderr_plain(s: &str) {
    CAPTURE.with(|c| {
        if let Some((_, err)) = c.borrow_mut().as_mut() {
            err.extend_from_slice(s.as_bytes());
            err.push(b'\n');
        } else {
            eprintln!("{}", s);
        }
    });
}

fn write_stderr_warn(s: &str) {
    CAPTURE.with(|c| {
        if let Some((_, err)) = c.borrow_mut().as_mut() {
            err.extend_from_slice(b"[warn] ");
            err.extend_from_slice(s.as_bytes());
            err.push(b'\n');
        } else {
            eprintln!("[warn] {}", s);
        }
    });
}

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in &["print", "error", "warn", "readLine"] {
        members.insert(
            method.to_string(),
            Value::Builtin(format!("Console.{}", method)),
        );
    }
    global.insert(
        "Console".to_string(),
        Value::Namespace {
            name: "Console".to_string(),
            members,
        },
    );
}

pub const DECLARED_EFFECTS: &[&str] = &[
    "Console.print",
    "Console.error",
    "Console.warn",
    "Console.readLine",
];

pub fn effects(name: &str) -> &'static [&'static str] {
    match name {
        "Console.print" => &["Console.print"],
        "Console.error" => &["Console.error"],
        "Console.warn" => &["Console.warn"],
        "Console.readLine" => &["Console.readLine"],
        _ => &[],
    }
}

/// Returns `Some(result)` when `name` is owned by this service, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Console.print" => Some(one_msg(name, args, write_stdout)),
        "Console.error" => Some(one_msg(name, args, write_stderr_plain)),
        "Console.warn" => Some(one_msg(name, args, write_stderr_warn)),
        "Console.readLine" => Some(read_line(args)),
        _ => None,
    }
}

// ─── Private helpers ──────────────────────────────────────────────────────────

fn one_msg(name: &str, args: &[Value], emit: impl Fn(&str)) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    if let Some(s) = aver_display(&args[0]) {
        emit(&s);
    }
    Ok(Value::Unit)
}

fn read_line(args: &[Value]) -> Result<Value, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Console.readLine() takes 0 arguments, got {}",
            args.len()
        )));
    }
    match aver_rt::read_line() {
        Ok(line) => Ok(Value::Ok(Box::new(Value::Str(line)))),
        Err(e) => Ok(Value::Err(Box::new(Value::Str(e)))),
    }
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let methods = &["print", "error", "warn", "readLine"];
    let mut members: Vec<(Rc<str>, NanValue)> = Vec::with_capacity(methods.len());
    for method in methods {
        let idx = arena.push_builtin(&format!("Console.{}", method));
        members.push((Rc::from(*method), NanValue::new_builtin(idx)));
    }
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Console"),
        members,
    });
    global.insert("Console".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Console.print" => Some(one_msg_nv(name, args, arena, write_stdout)),
        "Console.error" => Some(one_msg_nv(name, args, arena, write_stderr_plain)),
        "Console.warn" => Some(one_msg_nv(name, args, arena, write_stderr_warn)),
        "Console.readLine" => Some(read_line_nv(args, arena)),
        _ => None,
    }
}

fn one_msg_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
    emit: impl Fn(&str),
) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    if let Some(s) = args[0].display(arena) {
        emit(&s);
    }
    Ok(NanValue::UNIT)
}

fn read_line_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if !args.is_empty() {
        return Err(RuntimeError::Error(format!(
            "Console.readLine() takes 0 arguments, got {}",
            args.len()
        )));
    }
    match aver_rt::read_line() {
        Ok(line) => {
            let inner = NanValue::new_string_value(&line, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(e) => {
            let inner = NanValue::new_string_value(&e, arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}
