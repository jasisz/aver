/// Crypto namespace — cryptographic hashing over byte sequences.
///
/// Crypto is NOT a type — these are functions operating on `List<Int>` whose
/// elements are bytes (0–255). Same pattern as `Byte` and `Char`: a namespace
/// of operations over existing types.
///
/// Methods:
///   Crypto.sha256(bytes: List<Int>) → Result<List<Int>, String>  — 32-byte digest
///
/// No effects required. Hashing is deterministic and total given valid input,
/// so it carries no `! [...]` declaration and pure callers stay pure — which
/// means hashing code can be covered by ordinary `verify` blocks against the
/// published test vectors rather than needing Oracle stubs.
use std::collections::HashMap;
use std::sync::Arc as Rc;

use aver_rt::AverList;
use sha2::{Digest, Sha256};

use crate::nan_value::{Arena, NanValue, NanValueConvert};
use crate::value::{RuntimeError, Value};

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    members.insert(
        "sha256".to_string(),
        Value::Builtin("Crypto.sha256".to_string()),
    );
    global.insert(
        "Crypto".to_string(),
        Value::Namespace {
            name: "Crypto".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Crypto.sha256" => Some(sha256(args)),
        _ => None,
    }
}

fn sha256(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Crypto.sha256() takes 1 argument (bytes), got {}",
            args.len()
        )));
    }
    let bytes = match bytes_arg(&args[0], "Crypto.sha256")? {
        Ok(bytes) => bytes,
        // An element outside 0..=255 is a value error, not a type error — it is
        // still a `List<Int>` — so it surfaces as a catchable `Result.Err`,
        // matching how `Tcp.sendBytes` treats the same mistake.
        Err(msg) => return Ok(Value::Err(Box::new(Value::Str(msg)))),
    };

    let digest = Sha256::digest(&bytes);
    let items: Vec<Value> = digest.iter().map(|b| Value::int(*b as i64)).collect();
    Ok(Value::Ok(Box::new(Value::List(AverList::from_vec(items)))))
}

/// Convert a `List<Int>` argument into raw bytes.
///
/// The outer `Result` is the type check; the inner one is the value check. An
/// `Int` outside `i64` is still an `Int` — a fortiori outside byte range — so
/// it takes the catchable value-error path rather than being reported as a
/// bogus type error.
#[allow(clippy::type_complexity)]
fn bytes_arg(val: &Value, method: &str) -> Result<Result<Vec<u8>, String>, RuntimeError> {
    let items = match val {
        Value::List(items) => items,
        _ => {
            return Err(RuntimeError::Error(format!(
                "{}: argument must be a List<Int>",
                method
            )));
        }
    };
    let mut out = Vec::with_capacity(items.len());
    for (idx, item) in items.iter().enumerate() {
        let n = match item {
            Value::Int(n) => match n.to_i64() {
                Some(n) => n,
                None => {
                    return Ok(Err(format!(
                        "{}: byte {} at index {} is out of range (0\u{2013}255)",
                        method, n, idx
                    )));
                }
            },
            _ => {
                return Err(RuntimeError::Error(format!(
                    "{}: argument must be a List<Int>",
                    method
                )));
            }
        };
        match u8::try_from(n) {
            Ok(b) => out.push(b),
            Err(_) => {
                return Ok(Err(format!(
                    "{}: byte {} at index {} is out of range (0\u{2013}255)",
                    method, n, idx
                )));
            }
        }
    }
    Ok(Ok(out))
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let idx = arena.push_builtin("Crypto.sha256");
    let members: Vec<(Rc<str>, NanValue)> = vec![(Rc::from("sha256"), NanValue::new_builtin(idx))];
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Crypto"),
        members,
    });
    global.insert("Crypto".to_string(), NanValue::new_namespace(ns_idx));
}

/// Bridge: convert NanValue args to Value, hash, convert the result back.
/// The conversion is negligible next to the digest itself, so the shared
/// implementation is worth more here than a hand-written native path.
pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    if !matches!(name, "Crypto.sha256") {
        return None;
    }
    let old_args: Vec<Value> = args.iter().map(|nv| nv.to_value(arena)).collect();
    let result = call(name, &old_args)?;
    Some(result.map(|v| NanValue::from_value(&v, arena)))
}
