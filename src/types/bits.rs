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
/// Every negative shift count or width is refused with `Result.Err`.
/// `shiftLeft` also refuses a count above the shared 16,777,216-bit
/// materialization bound; `low` applies that bound only to negative inputs,
/// whose infinite one tail would otherwise manufacture a new magnitude of
/// the requested width. `shiftRight` never grows a value, so it has no upper
/// bound and returns `0` or `-1` in O(1) once the count reaches the sign tail.
/// Literal discharge follows those same operation-specific rules.
///
/// No effects required.
use aver_rt::{AverInt, ShiftCountError};

use crate::nan_value::{Arena, NanIntExt, NanValue};
use crate::value::RuntimeError;

/// The `Result.Err` payload for a negative shift count. Shared verbatim with
/// every other backend's model (Rust codegen, Lean prelude, Dafny prelude) —
/// a verify case that matches on the error text must see the same string on
/// all of them.
pub const NEGATIVE_SHIFT: &str = "negative shift count";

/// The `Result.Err` payload for a negative bit width.
pub const NEGATIVE_WIDTH: &str = "negative bit width";

/// Returns `Some(result)` when `name` is owned by this namespace, `None` otherwise.
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
        Err(ShiftCountError::TooLarge) => {
            let message = if name == "Bits.low" {
                aver_rt::bit_width_too_large_message()
            } else {
                aver_rt::shift_count_too_large_message()
            };
            let inner = NanValue::new_string_value(&message, arena);
            Ok(NanValue::new_err_value(inner, arena))
        }
    }
}
