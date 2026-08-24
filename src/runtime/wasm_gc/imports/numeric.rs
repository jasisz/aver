//! Random number generation + float math host imports. `Random.int`
//! and `Random.float` go through `try_replay` / `record_effect`
//! like every other effect. The float math arms (`float_sin`,
//! `float_cos`, `float_atan2`, `float_pow`) are pure operations —
//! no recording, no replay; the wasm-gc lowering imports them
//! because the engine doesn't yet expose a built-in `f64.sin`.

use super::super::RunWasmGcHost;
use super::super::decode::decode_result_int;
use super::factories::{host_result_err_int, host_result_ok_int};
use super::replay_glue::{json_err, json_ok, record_effect_if_recording, try_replay};
use super::tcp::{decode_guest_int, guest_int_json};

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
            let min = params
                .first()
                .ok_or_else(|| wasmtime::Error::msg("Random.int: missing min"))?;
            let max = params
                .get(1)
                .ok_or_else(|| wasmtime::Error::msg("Random.int: missing max"))?;
            let min = decode_guest_int(caller, min, "Random.int: malformed min Int")?;
            let max = decode_guest_int(caller, max, "Random.int: malformed max Int")?;
            let args = vec![guest_int_json(&min), guest_int_json(&max)];
            if let Some(cached) = try_replay(caller, "Random.int", args.clone())? {
                results[0] = Val::AnyRef(decode_result_int(caller, &cached)?);
                return Ok(true);
            }
            let min_value = aver_rt::AverInt::from_bigint(min.big);
            let max_value = aver_rt::AverInt::from_bigint(max.big);
            let (result, outcome) =
                match aver_rt::provider::standard_random_int(&min_value, &max_value) {
                    Ok(value) => {
                        let value = value
                            .to_i64()
                            .expect("Random.int result lies inside machine-range bounds");
                        (
                            host_result_ok_int(caller, value)?,
                            json_ok(aver::replay::JsonValue::Int(value)),
                        )
                    }
                    Err(fault) => (
                        host_result_err_int(caller, &fault.message)?,
                        json_err(&fault.message),
                    ),
                };
            results[0] = Val::AnyRef(result);
            record_effect_if_recording(caller, "Random.int", args, outcome, caller_fn);
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
