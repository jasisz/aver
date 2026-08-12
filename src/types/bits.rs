/// Bits namespace — a bit-level VIEW of `Int`, not a second numeric type.
///
/// Methods:
///   Bits.and(a, b)         → Int                  — pointwise conjunction
///   Bits.or(a, b)          → Int                  — pointwise disjunction
///   Bits.xor(a, b)         → Int                  — pointwise exclusive-or
///   Bits.not(x)            → Int                  — pointwise complement
///   Bits.shiftLeft(x, n)   → Result<Int, String>  — `x * 2^n`
///   Bits.shiftRight(x, n)  → Result<Int, String>  — `floor(x / 2^n)`
///   Bits.low(x, width)     → Result<Int, String>  — `x mod 2^width`
///
/// Everything here takes ordinary mathematical `Int` values and returns
/// ordinary mathematical `Int` values. There is no bit-vector, no machine
/// word, no persistent width: `Bits` only says how to READ an `Int` for the
/// duration of one call.
///
/// The reading is INFINITE TWO'S COMPLEMENT — a non-negative integer has
/// infinitely many leading zeroes, a negative one infinitely many leading
/// ones — which is the only convention that makes `and`/`or`/`xor`/`not`
/// total on ℤ without inventing a width. It gives `Bits.and(-1, x) == x`,
/// `Bits.or(-1, x) == -1` and `Bits.not(x) == -x - 1`.
///
/// Fixed-width behaviour is always REQUESTED, never implied: `Bits.low(x, w)`
/// is the value of the lowest `w` bits, so a protocol invariant reads as
/// `Bits.low(shifted, 25)` instead of an opaque `Bits.and(shifted, 33554431)`.
/// `Int` arithmetic itself still never overflows or wraps.
///
/// A negative shift count or width is refused with `Result.Err`. It never
/// panics, never reverses direction, never clamps, and never inherits the
/// host language's behaviour for oversized counts. When the count is a
/// syntactic non-negative integer literal the typechecker discharges the
/// error and the call types as plain `Int` — exactly the rule `Int.div` /
/// `Int.mod` already use for a literal divisor (see
/// `is_literal_nonneg_int_count` in `src/ast`).
///
/// No effects required.
use std::collections::HashMap;
use std::sync::Arc as Rc;

use aver_rt::{AverInt, ShiftCountError};

use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::{RuntimeError, Value};

/// Every method this namespace exposes, in one place: the `Value` dispatch,
/// the `NanValue` dispatch and both `register` functions read this list, so a
/// method cannot be callable on one path and invisible on the other.
const METHODS: &[&str] = &["and", "or", "xor", "not", "shiftLeft", "shiftRight", "low"];

/// The `Result.Err` payload for a negative shift count. Shared verbatim with
/// every other backend's model (Rust codegen, Lean prelude, Dafny prelude) —
/// a verify case that matches on the error text must see the same string on
/// all of them.
pub const NEGATIVE_SHIFT: &str = "negative shift count";

/// The `Result.Err` payload for a negative bit width.
pub const NEGATIVE_WIDTH: &str = "negative bit width";

pub fn register(global: &mut HashMap<String, Value>) {
    let mut members = HashMap::new();
    for method in METHODS {
        members.insert(
            (*method).to_string(),
            Value::Builtin(format!("Bits.{}", method)),
        );
    }
    global.insert(
        "Bits".to_string(),
        Value::Namespace {
            name: "Bits".to_string(),
            members,
        },
    );
}

pub fn effects(_name: &str) -> &'static [&'static str] {
    &[]
}

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
pub fn call(name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    match name {
        "Bits.and" => Some(binary(name, args, AverInt::bit_and)),
        "Bits.or" => Some(binary(name, args, AverInt::bit_or)),
        "Bits.xor" => Some(binary(name, args, AverInt::bit_xor)),
        "Bits.not" => Some(complement(args)),
        "Bits.shiftLeft" => Some(counted(name, args, AverInt::shift_left, NEGATIVE_SHIFT)),
        "Bits.shiftRight" => Some(counted(name, args, AverInt::shift_right, NEGATIVE_SHIFT)),
        "Bits.low" => Some(counted(name, args, AverInt::low_bits, NEGATIVE_WIDTH)),
        _ => None,
    }
}

// ─── Implementations ────────────────────────────────────────────────────────

/// The three total pointwise operations. They cannot fail: infinite two's
/// complement is defined for every pair of integers, so the return type is
/// plain `Int` with no `Result` to unwrap.
fn binary(
    name: &str,
    args: &[Value],
    op: fn(&AverInt, &AverInt) -> AverInt,
) -> Result<Value, RuntimeError> {
    let [a, b] = two_args(name, args)?;
    let (Value::Int(x), Value::Int(y)) = (a, b) else {
        return Err(RuntimeError::Error(format!(
            "{}: both arguments must be Int",
            name
        )));
    };
    Ok(Value::Int(op(x, y)))
}

fn complement(args: &[Value]) -> Result<Value, RuntimeError> {
    let [val] = one_arg("Bits.not", args)?;
    let Value::Int(n) = val else {
        return Err(RuntimeError::Error(
            "Bits.not: argument must be an Int".to_string(),
        ));
    };
    Ok(Value::Int(n.bit_not()))
}

/// The three operations that take a count: a shift amount or a bit width.
/// Only a NEGATIVE count is a `Result.Err`; a count too large to name a bit
/// position is a runtime abort, because the mathematical value exists and no
/// machine can hold it — that is not a case source code can meaningfully
/// catch, and modelling it as one would fork the proof backends off the
/// specification.
fn counted(
    name: &str,
    args: &[Value],
    op: fn(&AverInt, &AverInt) -> Result<AverInt, ShiftCountError>,
    negative_message: &str,
) -> Result<Value, RuntimeError> {
    let [a, b] = two_args(name, args)?;
    let (Value::Int(x), Value::Int(n)) = (a, b) else {
        return Err(RuntimeError::Error(format!(
            "{}: both arguments must be Int",
            name
        )));
    };
    match op(x, n) {
        Ok(v) => Ok(Value::Ok(Box::new(Value::Int(v)))),
        Err(ShiftCountError::Negative) => Ok(Value::Err(Box::new(Value::Str(
            negative_message.to_string(),
        )))),
        Err(ShiftCountError::Unrepresentable) => Err(RuntimeError::Error(format!(
            "{}: count {} is too large to name a bit position",
            name, n
        ))),
    }
}

// ─── Helpers ────────────────────────────────────────────────────────────────

fn one_arg<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 1], RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 1 argument, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0]])
}

fn two_args<'a>(name: &str, args: &'a [Value]) -> Result<[&'a Value; 2], RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    Ok([&args[0], &args[1]])
}

// ─── NanValue-native API ─────────────────────────────────────────────────────

pub fn register_nv(global: &mut HashMap<String, NanValue>, arena: &mut Arena) {
    let members: Vec<(Rc<str>, NanValue)> = METHODS
        .iter()
        .map(|method| {
            let idx = arena.push_builtin(&format!("Bits.{}", method));
            (Rc::from(*method), NanValue::new_builtin(idx))
        })
        .collect();
    let ns_idx = arena.push(crate::nan_value::ArenaEntry::Namespace {
        name: Rc::from("Bits"),
        members,
    });
    global.insert("Bits".to_string(), NanValue::new_namespace(ns_idx));
}

pub fn call_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Option<Result<NanValue, RuntimeError>> {
    match name {
        "Bits.and" => Some(binary_nv(name, args, arena, AverInt::bit_and)),
        "Bits.or" => Some(binary_nv(name, args, arena, AverInt::bit_or)),
        "Bits.xor" => Some(binary_nv(name, args, arena, AverInt::bit_xor)),
        "Bits.not" => Some(complement_nv(args, arena)),
        "Bits.shiftLeft" => Some(counted_nv(
            name,
            args,
            arena,
            AverInt::shift_left,
            NEGATIVE_SHIFT,
        )),
        "Bits.shiftRight" => Some(counted_nv(
            name,
            args,
            arena,
            AverInt::shift_right,
            NEGATIVE_SHIFT,
        )),
        "Bits.low" => Some(counted_nv(
            name,
            args,
            arena,
            AverInt::low_bits,
            NEGATIVE_WIDTH,
        )),
        _ => None,
    }
}

fn nv_two_ints(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
) -> Result<(AverInt, AverInt), RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::Error(format!(
            "{}() takes 2 arguments, got {}",
            name,
            args.len()
        )));
    }
    if !args[0].is_int() || !args[1].is_int() {
        return Err(RuntimeError::Error(format!(
            "{}: both arguments must be Int",
            name
        )));
    }
    Ok((args[0].as_aver_int(arena), args[1].as_aver_int(arena)))
}

fn binary_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
    op: fn(&AverInt, &AverInt) -> AverInt,
) -> Result<NanValue, RuntimeError> {
    let (x, y) = nv_two_ints(name, args, arena)?;
    Ok(NanValue::from_aver_int(op(&x, &y), arena))
}

fn complement_nv(args: &[NanValue], arena: &mut Arena) -> Result<NanValue, RuntimeError> {
    if args.len() != 1 {
        return Err(RuntimeError::Error(format!(
            "Bits.not() takes 1 argument, got {}",
            args.len()
        )));
    }
    if !args[0].is_int() {
        return Err(RuntimeError::Error(
            "Bits.not: argument must be an Int".to_string(),
        ));
    }
    let n = args[0].as_aver_int(arena);
    Ok(NanValue::from_aver_int(n.bit_not(), arena))
}

fn counted_nv(
    name: &str,
    args: &[NanValue],
    arena: &mut Arena,
    op: fn(&AverInt, &AverInt) -> Result<AverInt, ShiftCountError>,
    negative_message: &str,
) -> Result<NanValue, RuntimeError> {
    let (x, n) = nv_two_ints(name, args, arena)?;
    match op(&x, &n) {
        Ok(v) => {
            let inner = NanValue::from_aver_int(v, arena);
            Ok(NanValue::new_ok_value(inner, arena))
        }
        Err(ShiftCountError::Negative) => {
            let inner = NanValue::new_string_value(negative_message, arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
        Err(ShiftCountError::Unrepresentable) => Err(RuntimeError::Error(format!(
            "{}: count {} is too large to name a bit position",
            name, n
        ))),
    }
}
