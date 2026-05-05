//! Random number generation + float math host imports. `Random.int`
//! and `Random.float` go through `try_replay` / `record_effect`
//! like every other effect. The float math arms (`float_sin`,
//! `float_cos`, `float_atan2`, `float_pow`) are pure operations —
//! no recording, no replay; the wasm-gc lowering imports them
//! because the engine doesn't yet expose a built-in `f64.sin`.

use super::super::RunWasmGcHost;
use super::replay_glue::{record_effect_if_recording, try_replay};

pub(super) fn dispatch(
    name: &str,
    caller: &mut wasmtime::Caller<'_, RunWasmGcHost>,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
    caller_fn: &str,
) -> Result<bool, wasmtime::Error> {
    use wasmtime::Val;
    match name {
        "random_int" => {
            let (min, max) = match (params.first(), params.get(1)) {
                (Some(Val::I64(a)), Some(Val::I64(b))) => (*a, *b),
                _ => (0, 0),
            };
            if let Some(cached) = try_replay(
                caller,
                "Random.int",
                vec![
                    aver::replay::JsonValue::Int(min),
                    aver::replay::JsonValue::Int(max),
                ],
            )? {
                let aver::replay::JsonValue::Int(v) = cached else {
                    return Err(wasmtime::Error::msg(
                        "replay Random.int: trace value is not an Int",
                    ));
                };
                results[0] = Val::I64(v);
                return Ok(true);
            }
            // aver_rt::random_int returns Result, but the wasm import
            // contract is a plain i64. The host falls back to `min` on
            // an inverted range — same surface the VM exposes.
            let v = aver_rt::random::random_int(min, max).unwrap_or(min);
            results[0] = Val::I64(v);
            record_effect_if_recording(
                caller,
                "Random.int",
                vec![
                    aver::replay::JsonValue::Int(min),
                    aver::replay::JsonValue::Int(max),
                ],
                aver::replay::JsonValue::Int(v),
                caller_fn,
            );
            Ok(true)
        }
        "random_float" => {
            if let Some(cached) = try_replay(caller, "Random.float", vec![])? {
                let aver::replay::JsonValue::Float(f) = cached else {
                    return Err(wasmtime::Error::msg(
                        "replay Random.float: trace value is not a Float",
                    ));
                };
                results[0] = Val::F64(f.to_bits());
                return Ok(true);
            }
            let f = aver_rt::random::random_float();
            results[0] = Val::F64(f.to_bits());
            record_effect_if_recording(
                caller,
                "Random.float",
                vec![],
                aver::replay::JsonValue::Float(f),
                caller_fn,
            );
            Ok(true)
        }
        "float_sin" => {
            if let Some(Val::F64(b)) = params.first() {
                results[0] = Val::F64(f64::from_bits(*b).sin().to_bits());
            }
            Ok(true)
        }
        "float_cos" => {
            if let Some(Val::F64(b)) = params.first() {
                results[0] = Val::F64(f64::from_bits(*b).cos().to_bits());
            }
            Ok(true)
        }
        "float_atan2" => {
            if let (Some(Val::F64(y)), Some(Val::F64(x))) = (params.first(), params.get(1)) {
                results[0] = Val::F64(f64::from_bits(*y).atan2(f64::from_bits(*x)).to_bits());
            }
            Ok(true)
        }
        "float_pow" => {
            if let (Some(Val::F64(b)), Some(Val::F64(e))) = (params.first(), params.get(1)) {
                results[0] = Val::F64(f64::from_bits(*b).powf(f64::from_bits(*e)).to_bits());
            }
            Ok(true)
        }
        _ => Ok(false),
    }
}
