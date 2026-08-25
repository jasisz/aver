//! Mapping of Aver builtin/namespace functions to Rust equivalents.
//!
//! After the rust-on-MIR HIR-walker deletion (W6/Stage-3) this module
//! holds only the walker-agnostic composers (`compose_*`,
//! `emit_effectful_builtin_call_with_temps`, the `policy_check_helper`
//! / `builtin_is_effectful` predicates) that both the MIR walker
//! (`from_mir`) and the verify/effect framing call with pre-rendered
//! arg strings — no `ResolvedExpr` walking happens here anymore.

/// Try to emit a builtin call as Rust code.
/// Returns `None` if the name is not a builtin (i.e. it's a user function).
/// Builtins whose return type includes String and needs .into_aver() conversion.
pub(super) fn builtin_needs_str_conversion(name: &str) -> bool {
    matches!(
        name,
        "Console.readLine"
            | "Time.now"
            | "Int.fromString"
            | "Float.fromString"
            | "String.slice"
            | "String.charAt"
            | "String.toLower"
            | "String.toUpper"
            | "String.trim"
            | "String.trimStart"
            | "String.trimEnd"
            | "String.split"
            | "String.replace"
            | "String.replaceFirst"
            | "String.join"
            | "String.reverse"
            | "String.fromInt"
            | "String.fromFloat"
            | "String.fromBool"
            | "String.chars"
            | "String.fromCodePoint"
            | "Env.get"
            | "Terminal.readKey"
            | "Http.get"
            | "Http.head"
            | "Http.delete"
            | "Http.post"
            | "Http.put"
            | "Http.patch"
            | "Int.mod"
            | "Int.div"
            | "Bits.shiftLeft"
            | "Bits.shiftRight"
            | "Bits.low"
    )
}

pub(super) fn builtin_is_effectful(name: &str) -> bool {
    matches!(
        name.split('.').next(),
        Some("Args" | "Console" | "Http" | "Disk" | "Env" | "Random" | "Tcp" | "Terminal" | "Time")
    )
}

fn emit_effectful_builtin_call_with_temps(name: &str, args: &[String]) -> Option<String> {
    match name {
        "Console.print" => Some(format!("aver_rt::console_print(&{})", args[0])),
        "Console.error" => Some(format!("aver_rt::console_error(&{})", args[0])),
        "Console.warn" => Some(format!("aver_rt::console_warn(&{})", args[0])),
        "Console.readLine" => Some("aver_rt::read_line()".to_string()),
        "Http.get" => Some(format!("aver_rt::http::get(&{})", args[0])),
        "Http.head" => Some(format!("aver_rt::http::head(&{})", args[0])),
        "Http.delete" => Some(format!("aver_rt::http::delete(&{})", args[0])),
        "Http.post" => Some(format!(
            "aver_rt::http::post(&{}, &{}, &{}, &{})",
            args[0], args[1], args[2], args[3]
        )),
        "Http.put" => Some(format!(
            "aver_rt::http::put(&{}, &{}, &{}, &{})",
            args[0], args[1], args[2], args[3]
        )),
        "Http.patch" => Some(format!(
            "aver_rt::http::patch(&{}, &{}, &{}, &{})",
            args[0], args[1], args[2], args[3]
        )),
        "Env.get" => Some(format!("aver_rt::env_get(&{})", args[0])),
        "Env.set" => Some(format!(
            "aver_rt::env_set(&{}, &{}).map_err(aver_rt::AverStr::from)",
            args[0], args[1]
        )),
        "Args.get" => Some("aver_replay::current_cli_args()".to_string()),
        "Time.now" => Some("aver_rt::provider::standard_time_now()".to_string()),
        "Time.unixMs" => Some("aver_rt::provider::standard_time_unix_ms()".to_string()),
        "Time.sleep" => Some(format!(
            "aver_rt::provider::standard_time_sleep(&{}).map_err(|e| aver_rt::AverStr::from(e.message))",
            args[0]
        )),
        "Random.int" => Some(format!(
            "aver_rt::provider::standard_random_int(&{}, &{}).map_err(|e| aver_rt::AverStr::from(e.message))",
            args[0], args[1]
        )),
        "Random.float" => Some("aver_rt::provider::standard_random_float()".to_string()),
        "Terminal.enableRawMode" => {
            Some("aver_rt::terminal_enable_raw_mode().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.disableRawMode" => {
            Some(
                "aver_rt::terminal_disable_raw_mode().map_err(aver_rt::AverStr::from)".to_string(),
            )
        }
        "Terminal.clear" => {
            Some("aver_rt::terminal_clear().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.moveTo" => Some(format!(
            "{{ let __x = {}; let __y = {}; match (__x.to_i64(), __y.to_i64()) {{ (Some(x), Some(y)) => aver_rt::terminal_move_to(x, y).map_err(aver_rt::AverStr::from), _ => Err(aver_rt::AverStr::from(\"Terminal.moveTo: coordinates must fit a 64-bit integer\")) }} }}",
            args[0], args[1]
        )),
        "Terminal.print" => Some(format!(
            "{{ let __s = format!(\"{{}}\", {}); aver_rt::terminal_print(&__s).map_err(aver_rt::AverStr::from) }}",
            args[0]
        )),
        "Terminal.setColor" => {
            Some(format!(
                "aver_rt::terminal_set_color(&{}).map_err(aver_rt::AverStr::from)",
                args[0]
            ))
        }
        "Terminal.resetColor" => {
            Some("aver_rt::terminal_reset_color().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.readKey" => {
            Some("aver_rt::terminal_read_key()".to_string())
        }
        "Terminal.size" => Some(
            "aver_rt::terminal_size().map(|(w, h)| crate::Terminal_Size { width: aver_rt::AverInt::from_i64(w), height: aver_rt::AverInt::from_i64(h) }).map_err(aver_rt::AverStr::from)".to_string(),
        ),
        "Terminal.hideCursor" => {
            Some("aver_rt::terminal_hide_cursor().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.showCursor" => {
            Some("aver_rt::terminal_show_cursor().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.flush" => {
            Some("aver_rt::terminal_flush().map_err(aver_rt::AverStr::from)".to_string())
        }
        _ => None,
    }
}

/// Shared replay-reroute composer (security-sensitive: a dropped
/// `invoke_effect` here silently disables record/replay capture for an
/// effect). Both the HIR oracle (`emit_replay_effect_call`) and the MIR
/// walker (`from_mir::emit_mir_effectful_builtin_call`) call this with
/// the per-arg owning-binding strings already rendered by their own
/// `clone_arg` mirror, so the emitted block is byte-identical across
/// backends by construction.
///
/// `arg_clones[i]` is the Rust expression bound to `__effect_argi`. The
/// raw effect call + the arg-json both reference those temps, so they
/// depend only on the *count* of args, not on how each was rendered —
/// which is why the shared composer can stay walker-agnostic.
pub(super) fn compose_replay_effect_call(name: &str, arg_clones: &[String]) -> Option<String> {
    let effect_name = name;
    let temp_names = (0..arg_clones.len())
        .map(|idx| format!("__effect_arg{}", idx))
        .collect::<Vec<_>>();
    let raw = emit_effectful_builtin_call_with_temps(name, &temp_names)?;
    let final_result = if builtin_needs_str_conversion(name) {
        format!("({}).into_aver()", raw)
    } else {
        raw
    };

    let mut lines = Vec::new();
    lines.push("{".to_string());
    for (idx, clone) in arg_clones.iter().enumerate() {
        lines.push(format!("    let {} = {};", temp_names[idx], clone));
    }
    lines.push("    crate::cancel_checkpoint();".to_string());
    let json_args = emit_replay_effect_arg_json(effect_name, &temp_names).join(", ");
    lines.push(format!(
        "    aver_replay::invoke_effect({:?}, vec![{}], || {})",
        effect_name, json_args, final_result
    ));
    lines.push("}".to_string());
    Some(lines.join("\n"))
}

fn emit_replay_effect_arg_json(name: &str, temp_names: &[String]) -> Vec<String> {
    match name {
        "Console.print" | "Console.error" | "Console.warn" | "Terminal.print" => vec![format!(
            "serde_json::Value::String(format!(\"{{}}\", {}))",
            temp_names[0]
        )],
        _ => temp_names
            .iter()
            .map(|name| format!("aver_replay::ReplayValue::to_replay_json(&{})", name))
            .collect(),
    }
}

/// Which policy `check_*` helper guards a built-in namespace, if any.
/// Returns the helper name for `Http.` / `Disk.` / `Env.` prefixes,
/// `None` for every other (effectful or pure) builtin. Shared between
/// the HIR oracle and the MIR walker so the policy-prefix decision is
/// made in exactly one place. Capability-bound operations go through
/// the same decision: a provider call crosses the identical host
/// boundary, so the flip off the builtin table must not unstitch
/// `aver.toml` enforcement.
pub(super) fn policy_check_helper(name: &str) -> Option<&'static str> {
    if name.starts_with("Http.") {
        Some("aver_policy::check_http")
    } else if name.starts_with("Disk.") {
        Some("aver_policy::check_disk")
    } else if name.starts_with("Env.") {
        Some("aver_policy::check_env")
    } else {
        None
    }
}

/// The local the policy check and the guarded call share. The checked
/// argument is evaluated into it exactly once: emitting the argument
/// expression at both use sites moved every non-`Copy` binding it named
/// (the generated project then failed to build) and ran any effect it
/// carried twice.
const POLICY_ARG_TEMP: &str = "__policy_arg";

/// Whether [`compose_effectful_builtin`] will bind argument 0 of `name` to
/// [`POLICY_ARG_TEMP`].
///
/// The caller has to know, because the bind is a `let` — an OWNING position.
/// Every guarded raw body reads argument 0 as `&{}`, so before the temp
/// existed the argument only ever had to be borrowable; a caller that keeps
/// rendering it raw hands the temp a value it does not own, and the emitted
/// project stops building (a local read again later is `E0382`, a field read
/// through a `&T` param is `E0507`). The render for this argument goes
/// through the same owning-position step as any other, which for the MIR
/// walker is `mir_clone_arg` — the step its sibling replay temps already use.
pub(super) fn policy_binds_first_arg(name: &str, policy_active: bool) -> bool {
    policy_active && policy_check_helper(name).is_some()
}

/// Shared effectful-builtin composer (security-sensitive: a dropped
/// `check_*` here silently disables aver.toml DENY enforcement for an
/// effect). Takes the args already rendered ONCE by the caller's
/// `emit_expr` mirror and applies, in order:
///
/// 1. **raw body** — [`compose_effectful_builtin_raw`] plus the
///    `.into_aver()` post-step for String-returning builtins.
/// 2. **policy wrap** — when `policy_active` and `name` is an
///    `Http.`/`Disk.`/`Env.` call with a first arg: `cancel_checkpoint()`,
///    then arg 0 bound to [`POLICY_ARG_TEMP`], then the matching
///    `check_*(<method>, &<temp>).expect(...)`. The raw body reads the
///    same temp, so the check and the call see one value from one
///    evaluation. That bind is an OWNING position and the caller is the
///    one that has to satisfy it — see [`policy_binds_first_arg`].
/// 3. **bare framing** — every other effectful builtin gets the bare
///    `{ cancel_checkpoint(); <result> }`.
/// 4. **pure passthrough** — a non-effectful builtin returns the raw
///    body unwrapped.
///
/// Returns `None` when the raw body is not one this composer covers.
pub(super) fn compose_effectful_builtin(
    name: &str,
    args: &[String],
    policy_active: bool,
) -> Option<String> {
    let helper = if policy_active {
        policy_check_helper(name)
    } else {
        None
    };
    // The guarded arg becomes the temp everywhere the call mentions it,
    // and the expression it displaced is what the temp gets bound to.
    let mut call_args = args.to_vec();
    let checked_arg = helper.and_then(|_| {
        call_args
            .first_mut()
            .map(|arg| std::mem::replace(arg, POLICY_ARG_TEMP.to_string()))
    });

    let raw = compose_effectful_builtin_raw(name, &call_args)?;
    let result = if builtin_needs_str_conversion(name) {
        format!("({}).into_aver()", raw)
    } else {
        raw
    };

    if let (Some(helper), Some(arg)) = (helper, checked_arg) {
        return Some(format!(
            "{{ crate::cancel_checkpoint(); let {POLICY_ARG_TEMP} = {arg}; {helper}(\"{name}\", &{POLICY_ARG_TEMP}).expect(\"aver.toml policy violation\"); {result} }}"
        ));
    }

    Some(if builtin_is_effectful(name) {
        format!("{{ crate::cancel_checkpoint(); {} }}", result)
    } else {
        result
    })
}

/// Raw-body composer for the effectful builtins' NON-replay path: the
/// `aver_rt::*` / `crate::*` call before any `cancel_checkpoint` /
/// policy / `.into_aver()` wrapping. Every arm renders its args
/// by-value (the caller's `emit_arg` mirror), so this keys off the
/// pre-emitted arg strings only and is byte-identical across the HIR
/// oracle (`emit_builtin_call_inner`) and the MIR walker
/// (`from_mir::emit_mir_effectful_builtin_call`). Reached only through
/// [`compose_effectful_builtin`], which owns the framing. Returns
/// `None` for a non-effectful / unknown name.
///
/// NOTE: this is the NON-replay raw body. The replay path uses
/// [`emit_effectful_builtin_call_with_temps`], which differs for a few
/// arms (`Args.get` → `aver_replay::current_cli_args()`, `Terminal.print`
/// → `format!` instead of `aver_display`); both are reachable from both
/// backends so the divergence is preserved identically on each.
fn compose_effectful_builtin_raw(name: &str, args: &[String]) -> Option<String> {
    let a = |i: usize| args[i].as_str();
    match name {
        // ---- Console ----
        "Console.print" => Some(format!("aver_rt::console_print(&{})", a(0))),
        "Console.error" | "Console.warn" => {
            let helper = if name == "Console.warn" {
                "console_warn"
            } else {
                "console_error"
            };
            Some(format!("aver_rt::{}(&{})", helper, a(0)))
        }
        "Console.readLine" => Some("aver_rt::read_line()".to_string()),

        // ---- Http ----
        "Http.get" => Some(format!("aver_rt::http::get(&{})", a(0))),
        "Http.head" => Some(format!("aver_rt::http::head(&{})", a(0))),
        "Http.delete" => Some(format!("aver_rt::http::delete(&{})", a(0))),
        "Http.post" => Some(format!(
            "aver_rt::http::post(&{}, &{}, &{}, &{})",
            a(0),
            a(1),
            a(2),
            a(3)
        )),
        "Http.put" => Some(format!(
            "aver_rt::http::put(&{}, &{}, &{}, &{})",
            a(0),
            a(1),
            a(2),
            a(3)
        )),
        "Http.patch" => Some(format!(
            "aver_rt::http::patch(&{}, &{}, &{}, &{})",
            a(0),
            a(1),
            a(2),
            a(3)
        )),

        // ---- Disk ----

        // ---- Env ----
        "Env.get" => Some(format!("aver_rt::env_get(&{})", a(0))),
        "Env.set" => Some(format!(
            "aver_rt::env_set(&{}, &{}).map_err(aver_rt::AverStr::from)",
            a(0),
            a(1)
        )),
        "Args.get" => Some("aver_rt::cli_args().into_aver()".to_string()),

        // ---- Time ----
        "Time.now" => Some("aver_rt::provider::standard_time_now()".to_string()),
        "Time.unixMs" => Some("aver_rt::provider::standard_time_unix_ms()".to_string()),
        "Time.sleep" => Some(format!(
            "aver_rt::provider::standard_time_sleep(&{}).map_err(|e| aver_rt::AverStr::from(e.message))",
            a(0)
        )),

        // ---- Random ----
        "Random.int" => Some(format!(
            "aver_rt::provider::standard_random_int(&{}, &{}).map_err(|e| aver_rt::AverStr::from(e.message))",
            a(0),
            a(1)
        )),
        "Random.float" => Some("aver_rt::provider::standard_random_float()".to_string()),

        // ---- Terminal ----
        "Terminal.enableRawMode" => {
            Some("aver_rt::terminal_enable_raw_mode().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.disableRawMode" => {
            Some(
                "aver_rt::terminal_disable_raw_mode().map_err(aver_rt::AverStr::from)".to_string(),
            )
        }
        "Terminal.clear" => {
            Some("aver_rt::terminal_clear().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.moveTo" => Some(format!(
            "{{ let __x = {}; let __y = {}; match (__x.to_i64(), __y.to_i64()) {{ (Some(x), Some(y)) => aver_rt::terminal_move_to(x, y).map_err(aver_rt::AverStr::from), _ => Err(aver_rt::AverStr::from(\"Terminal.moveTo: coordinates must fit a 64-bit integer\")) }} }}",
            a(0),
            a(1)
        )),
        "Terminal.print" => Some(format!(
            "{{ let __s = aver_rt::aver_display(&{}); aver_rt::terminal_print(&__s).map_err(aver_rt::AverStr::from) }}",
            a(0)
        )),
        "Terminal.setColor" => Some(format!(
            "aver_rt::terminal_set_color(&{}).map_err(aver_rt::AverStr::from)",
            a(0)
        )),
        "Terminal.resetColor" => {
            Some("aver_rt::terminal_reset_color().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.readKey" => {
            Some("aver_rt::terminal_read_key()".to_string())
        }
        "Terminal.size" => Some(
            "aver_rt::terminal_size().map(|(w, h)| crate::Terminal_Size { width: aver_rt::AverInt::from_i64(w), height: aver_rt::AverInt::from_i64(h) }).map_err(aver_rt::AverStr::from)".to_string(),
        ),
        "Terminal.hideCursor" => {
            Some("aver_rt::terminal_hide_cursor().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.showCursor" => {
            Some("aver_rt::terminal_show_cursor().map_err(aver_rt::AverStr::from)".to_string())
        }
        "Terminal.flush" => {
            Some("aver_rt::terminal_flush().map_err(aver_rt::AverStr::from)".to_string())
        }

        _ => None,
    }
}
