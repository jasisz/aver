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
            | "String.repeat"
            | "String.reverse"
            | "String.fromInt"
            | "String.fromFloat"
            | "String.fromBool"
            | "String.chars"
            | "Char.fromCode"
            | "Byte.toHex"
            | "Byte.fromHex"
            | "Disk.readText"
            | "Disk.writeText"
            | "Disk.appendText"
            | "Disk.delete"
            | "Disk.deleteDir"
            | "Disk.listDir"
            | "Disk.makeDir"
            | "Env.get"
            | "Terminal.readKey"
            | "Http.get"
            | "Http.head"
            | "Http.delete"
            | "Http.post"
            | "Http.put"
            | "Http.patch"
            | "Tcp.send"
            | "Tcp.ping"
            | "Tcp.connect"
            | "Tcp.writeLine"
            | "Tcp.readLine"
            | "Tcp.close"
            | "Int.mod"
            | "Int.div"
    )
}

fn builtin_effect_name(name: &str) -> &str {
    match name {
        "SelfHostRuntime.httpServerListen" => "HttpServer.listen",
        "SelfHostRuntime.httpServerListenWith" => "HttpServer.listenWith",
        _ => name,
    }
}

pub(super) fn builtin_is_effectful(name: &str) -> bool {
    matches!(
        builtin_effect_name(name).split('.').next(),
        Some(
            "Args"
                | "Console"
                | "Http"
                | "HttpServer"
                | "Disk"
                | "Env"
                | "Random"
                | "SelfHostRuntime"
                | "Tcp"
                | "Terminal"
                | "Time"
        )
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
        "HttpServer.listen" => Some(format!(
            "{{ if let Err(e) = crate::http_server_listen({}, {}) {{ panic!(\"{{}}\", e); }} }}",
            args[0], args[1]
        )),
        "HttpServer.listenWith" => Some(format!(
            "{{ if let Err(e) = crate::http_server_listen_with({}, {}.clone(), {}) {{ panic!(\"{{}}\", e); }} }}",
            args[0], args[1], args[2]
        )),
        "SelfHostRuntime.httpServerListen" => Some(format!(
            "crate::self_host_support::http_server_listen({}, {})",
            args[0], args[1]
        )),
        "SelfHostRuntime.httpServerListenWith" => Some(format!(
            "crate::self_host_support::http_server_listen_with({}, {}.clone(), {})",
            args[0], args[1], args[2]
        )),
        "Disk.readText" => Some(format!("aver_rt::read_text(&{})", args[0])),
        "Disk.writeText" => Some(format!("aver_rt::write_text(&{}, &{})", args[0], args[1])),
        "Disk.appendText" => Some(format!("aver_rt::append_text(&{}, &{})", args[0], args[1])),
        "Disk.exists" => Some(format!("aver_rt::path_exists(&{})", args[0])),
        "Disk.delete" => Some(format!("aver_rt::delete_file(&{})", args[0])),
        "Disk.deleteDir" => Some(format!("aver_rt::delete_dir(&{})", args[0])),
        "Disk.listDir" => Some(format!("aver_rt::list_dir(&{})", args[0])),
        "Disk.makeDir" => Some(format!("aver_rt::make_dir(&{})", args[0])),
        "Env.get" => Some(format!("aver_rt::env_get(&{})", args[0])),
        "Env.set" => Some(format!(
            "aver_rt::env_set(&{}, &{}).expect(\"Env.set failed\")",
            args[0], args[1]
        )),
        "Args.get" => Some("aver_replay::current_cli_args()".to_string()),
        "Time.now" => Some("aver_rt::time_now()".to_string()),
        "Time.unixMs" => Some("aver_rt::time_unix_ms()".to_string()),
        "Time.sleep" => Some(format!("aver_rt::time_sleep({})", args[0])),
        "Random.int" => Some(format!(
            "aver_rt::random::random_int({}, {}).unwrap()",
            args[0], args[1]
        )),
        "Random.float" => Some("aver_rt::random::random_float()".to_string()),
        "Tcp.send" => Some(format!(
            "aver_rt::tcp::send(&{}, {}, &{})",
            args[0], args[1], args[2]
        )),
        "Tcp.ping" => Some(format!("aver_rt::tcp::ping(&{}, {})", args[0], args[1])),
        "Tcp.connect" => Some(format!("aver_rt::tcp::connect(&{}, {})", args[0], args[1])),
        "Tcp.writeLine" => Some(format!("aver_rt::tcp::write_line(&{}, &{})", args[0], args[1])),
        "Tcp.readLine" => Some(format!("aver_rt::tcp::read_line(&{})", args[0])),
        "Tcp.close" => Some(format!("aver_rt::tcp::close(&{})", args[0])),
        "Terminal.enableRawMode" => Some("aver_rt::terminal_enable_raw_mode().unwrap()".to_string()),
        "Terminal.disableRawMode" => {
            Some("aver_rt::terminal_disable_raw_mode().unwrap()".to_string())
        }
        "Terminal.clear" => Some("aver_rt::terminal_clear().unwrap()".to_string()),
        "Terminal.moveTo" => Some(format!(
            "aver_rt::terminal_move_to({}, {}).unwrap()",
            args[0], args[1]
        )),
        "Terminal.print" => Some(format!(
            "{{ let __s = format!(\"{{}}\", {}); aver_rt::terminal_print(&__s).unwrap() }}",
            args[0]
        )),
        "Terminal.setColor" => {
            Some(format!("aver_rt::terminal_set_color(&{}).unwrap()", args[0]))
        }
        "Terminal.resetColor" => Some("aver_rt::terminal_reset_color().unwrap()".to_string()),
        "Terminal.readKey" => Some("aver_rt::terminal_read_key()".to_string()),
        "Terminal.size" => Some(
            "{ let (w, h) = aver_rt::terminal_size().unwrap(); aver_rt::TerminalSize { width: w, height: h } }".to_string(),
        ),
        "Terminal.hideCursor" => Some("aver_rt::terminal_hide_cursor().unwrap()".to_string()),
        "Terminal.showCursor" => Some("aver_rt::terminal_show_cursor().unwrap()".to_string()),
        "Terminal.flush" => Some("aver_rt::terminal_flush().unwrap()".to_string()),
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
    let effect_name = builtin_effect_name(name);
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
        "HttpServer.listen" => vec![
            format!(
                "aver_replay::ReplayValue::to_replay_json(&{})",
                temp_names[0]
            ),
            "serde_json::Value::String(\"<handler>\".to_string())".to_string(),
        ],
        "HttpServer.listenWith" => vec![
            format!(
                "aver_replay::ReplayValue::to_replay_json(&{})",
                temp_names[0]
            ),
            format!(
                "aver_replay::ReplayValue::to_replay_json(&{})",
                temp_names[1]
            ),
            "serde_json::Value::String(\"<handler>\".to_string())".to_string(),
        ],
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
/// made in exactly one place.
fn policy_check_helper(name: &str) -> Option<&'static str> {
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

/// Shared policy-wrap / bare-frame composer (security-sensitive: a
/// dropped `check_*` here silently disables aver.toml DENY enforcement
/// for an effect). Applies, in HIR's exact order:
///
/// 1. **policy wrap** — when `policy_active` and `name` is an
///    `Http.`/`Disk.`/`Env.` call with a first arg
///    (`first_arg = Some(emitted)`): prepend `cancel_checkpoint()` +
///    the matching `check_*(<method>, &<arg0>).expect(...)`.
/// 2. **bare framing** — every other effectful builtin gets the bare
///    `{ cancel_checkpoint(); <result> }`.
/// 3. **pure passthrough** — a non-effectful builtin returns `result`
///    unwrapped.
///
/// Both backends call this with `result` already `.into_aver()`-
/// converted and `first_arg` rendered by their own `emit_expr` mirror,
/// so the emitted framing is byte-identical by construction.
pub(super) fn compose_effect_wrap(
    name: &str,
    result: String,
    policy_active: bool,
    first_arg: Option<String>,
) -> String {
    if policy_active
        && let Some(arg) = first_arg
        && let Some(helper) = policy_check_helper(name)
    {
        return format!(
            "{{ crate::cancel_checkpoint(); {helper}(\"{name}\", &{arg}).expect(\"aver.toml policy violation\"); {result} }}"
        );
    }

    if builtin_is_effectful(name) {
        format!("{{ crate::cancel_checkpoint(); {} }}", result)
    } else {
        result
    }
}

/// Shared raw-body composer for the effectful builtins' NON-replay
/// path: the `aver_rt::*` / `crate::*` call before any
/// `cancel_checkpoint` / policy / `.into_aver()` wrapping. Every arm
/// renders its args by-value (the caller's `emit_arg` mirror), so this
/// keys off the pre-emitted arg strings only and is byte-identical
/// across the HIR oracle (`emit_builtin_call_inner`) and the MIR walker
/// (`from_mir::emit_mir_effectful_builtin_call`). Returns `None` for a
/// non-effectful / unknown name.
///
/// NOTE: this is the NON-replay raw body. The replay path uses
/// [`emit_effectful_builtin_call_with_temps`], which differs for a few
/// arms (`Args.get` → `aver_replay::current_cli_args()`, `Terminal.print`
/// → `format!` instead of `aver_display`); both are reachable from both
/// backends so the divergence is preserved identically on each.
pub(super) fn compose_effectful_builtin_raw(name: &str, args: &[String]) -> Option<String> {
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

        // ---- Tcp ----
        "Tcp.connect" => Some(format!("aver_rt::tcp::connect(&{}, {})", a(0), a(1))),
        "Tcp.writeLine" => Some(format!("aver_rt::tcp::write_line(&{}, &{})", a(0), a(1))),
        "Tcp.readLine" => Some(format!("aver_rt::tcp::read_line(&{})", a(0))),
        "Tcp.close" => Some(format!("aver_rt::tcp::close(&{})", a(0))),
        "Tcp.send" => Some(format!(
            "aver_rt::tcp::send(&{}, {}, &{})",
            a(0),
            a(1),
            a(2)
        )),
        "Tcp.ping" => Some(format!("aver_rt::tcp::ping(&{}, {})", a(0), a(1))),

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

        // ---- HttpServer ----
        "HttpServer.listen" => Some(format!(
            "{{ if let Err(e) = crate::http_server_listen({}, {}) {{ panic!(\"{{}}\", e); }} }}",
            a(0),
            a(1)
        )),
        "HttpServer.listenWith" => Some(format!(
            "{{ if let Err(e) = crate::http_server_listen_with({}, {}.clone(), {}) {{ panic!(\"{{}}\", e); }} }}",
            a(0),
            a(1),
            a(2)
        )),
        "SelfHostRuntime.httpServerListen" => Some(format!(
            "crate::self_host_support::http_server_listen({}, {})",
            a(0),
            a(1)
        )),
        "SelfHostRuntime.httpServerListenWith" => Some(format!(
            "crate::self_host_support::http_server_listen_with({}, {}.clone(), {})",
            a(0),
            a(1),
            a(2)
        )),

        // ---- Disk ----
        "Disk.readText" => Some(format!("aver_rt::read_text(&{})", a(0))),
        "Disk.writeText" => Some(format!("aver_rt::write_text(&{}, &{})", a(0), a(1))),
        "Disk.appendText" => Some(format!("aver_rt::append_text(&{}, &{})", a(0), a(1))),
        "Disk.exists" => Some(format!("aver_rt::path_exists(&{})", a(0))),
        "Disk.delete" => Some(format!("aver_rt::delete_file(&{})", a(0))),
        "Disk.deleteDir" => Some(format!("aver_rt::delete_dir(&{})", a(0))),
        "Disk.listDir" => Some(format!("aver_rt::list_dir(&{})", a(0))),
        "Disk.makeDir" => Some(format!("aver_rt::make_dir(&{})", a(0))),

        // ---- Env ----
        "Env.get" => Some(format!("aver_rt::env_get(&{})", a(0))),
        "Env.set" => Some(format!(
            "aver_rt::env_set(&{}, &{}).expect(\"Env.set failed\")",
            a(0),
            a(1)
        )),
        "Args.get" => Some("aver_rt::cli_args().into_aver()".to_string()),

        // ---- Time ----
        "Time.now" => Some("aver_rt::time_now()".to_string()),
        "Time.unixMs" => Some("aver_rt::time_unix_ms()".to_string()),
        "Time.sleep" => Some(format!("aver_rt::time_sleep({})", a(0))),

        // ---- Random ----
        "Random.int" => Some(format!(
            "aver_rt::random::random_int({}, {}).unwrap()",
            a(0),
            a(1)
        )),
        "Random.float" => Some("aver_rt::random::random_float()".to_string()),

        // ---- Terminal ----
        "Terminal.enableRawMode" => {
            Some("aver_rt::terminal_enable_raw_mode().unwrap()".to_string())
        }
        "Terminal.disableRawMode" => {
            Some("aver_rt::terminal_disable_raw_mode().unwrap()".to_string())
        }
        "Terminal.clear" => Some("aver_rt::terminal_clear().unwrap()".to_string()),
        "Terminal.moveTo" => Some(format!(
            "aver_rt::terminal_move_to({}, {}).unwrap()",
            a(0),
            a(1)
        )),
        "Terminal.print" => Some(format!(
            "{{ let __s = aver_rt::aver_display(&{}); aver_rt::terminal_print(&__s).unwrap() }}",
            a(0)
        )),
        "Terminal.setColor" => Some(format!("aver_rt::terminal_set_color(&{}).unwrap()", a(0))),
        "Terminal.resetColor" => Some("aver_rt::terminal_reset_color().unwrap()".to_string()),
        "Terminal.readKey" => Some("aver_rt::terminal_read_key()".to_string()),
        "Terminal.size" => Some(
            "{ let (w, h) = aver_rt::terminal_size().unwrap(); aver_rt::TerminalSize { width: w, height: h } }".to_string(),
        ),
        "Terminal.hideCursor" => Some("aver_rt::terminal_hide_cursor().unwrap()".to_string()),
        "Terminal.showCursor" => Some("aver_rt::terminal_show_cursor().unwrap()".to_string()),
        "Terminal.flush" => Some("aver_rt::terminal_flush().unwrap()".to_string()),

        _ => None,
    }
}
