use super::expr::{clone_arg, emit_expr};
use super::liveness::{EmitCtx, compute_args_used_after_full};
/// Mapping of Aver builtin/namespace functions to Rust equivalents.
use crate::ast::Expr;
use crate::codegen::CodegenContext;

/// Try to emit a builtin call as Rust code.
/// Returns `None` if the name is not a builtin (i.e. it's a user function).
/// Builtins whose return type includes String and needs .into_aver() conversion.
fn builtin_needs_str_conversion(name: &str) -> bool {
    matches!(
        name,
        "Console.readLine"
            | "Time.now"
            | "Int.toString"
            | "Float.toString"
            | "Int.parse"
            | "Float.parse"
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
    )
}

fn builtin_effect_name(name: &str) -> &str {
    match name {
        "SelfHostRuntime.httpServerListen" => "HttpServer.listen",
        "SelfHostRuntime.httpServerListenWith" => "HttpServer.listenWith",
        _ => name,
    }
}

fn builtin_is_effectful(name: &str) -> bool {
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

fn emit_replay_effect_call(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<String> {
    let effect_name = builtin_effect_name(name);
    let arg_ctxs = compute_args_used_after_full(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
        &ectx.borrowed_params,
    );
    let temp_names = (0..args.len())
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
    for (idx, arg) in args.iter().enumerate() {
        let emitted = clone_arg(arg, ctx, &arg_ctxs[idx]);
        lines.push(format!("    let {} = {};", temp_names[idx], emitted));
    }
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

pub fn emit_builtin_call(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<String> {
    if ctx.emit_replay_runtime && builtin_is_effectful(name) {
        return emit_replay_effect_call(name, args, ctx, ectx);
    }

    let result = emit_builtin_call_inner(name, args, ctx, ectx)?;
    let arg_ctxs = compute_args_used_after_full(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
        &ectx.borrowed_params,
    );

    // Convert String-returning builtins to AverStr.
    let result = if builtin_needs_str_conversion(name) {
        format!("({}).into_aver()", result)
    } else {
        result
    };

    // Wrap Http/Disk/Env calls with policy checks when aver.toml policy is present.
    if ctx.policy.is_some() && !ctx.emit_replay_runtime {
        if name.starts_with("Http.") && !args.is_empty() {
            let url_arg = emit_expr(&args[0], ctx, &arg_ctxs[0]);
            return Some(format!(
                "{{ aver_policy::check_http(\"{}\", &{}).expect(\"aver.toml policy violation\"); {} }}",
                name, url_arg, result
            ));
        }
        if name.starts_with("Disk.") && !args.is_empty() {
            let path_arg = emit_expr(&args[0], ctx, &arg_ctxs[0]);
            return Some(format!(
                "{{ aver_policy::check_disk(\"{}\", &{}).expect(\"aver.toml policy violation\"); {} }}",
                name, path_arg, result
            ));
        }
        if name.starts_with("Env.") && !args.is_empty() {
            let key_arg = emit_expr(&args[0], ctx, &arg_ctxs[0]);
            return Some(format!(
                "{{ aver_policy::check_env(\"{}\", &{}).expect(\"aver.toml policy violation\"); {} }}",
                name, key_arg, result
            ));
        }
    }

    Some(result)
}

fn emit_builtin_call_inner(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<String> {
    let arg_ctxs = compute_args_used_after_full(
        args,
        &ectx.used_after,
        &ectx.local_types,
        &ectx.rc_wrapped,
        &ectx.borrowed_params,
    );
    let emit_arg = |idx: usize| emit_expr(&args[idx], ctx, &arg_ctxs[idx]);

    match name {
        // ---- Console ----
        "Console.print" => {
            let arg = emit_arg(0);
            Some(format!("aver_rt::console_print(&{})", arg))
        }
        "Console.error" | "Console.warn" => {
            let arg = emit_arg(0);
            let helper = if name == "Console.warn" {
                "console_warn"
            } else {
                "console_error"
            };
            Some(format!("aver_rt::{}(&{})", helper, arg))
        }
        "Console.readLine" => Some("aver_rt::read_line()".to_string()),

        // ---- Result ----
        "Result.Ok" => {
            let arg = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            Some(format!("Ok({})", arg))
        }
        "Result.Err" => {
            let arg = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            Some(format!("Err({})", arg))
        }
        "Result.withDefault" => {
            let result = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let default = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!("{}.unwrap_or({})", result, default))
        }

        // ---- Option ----
        "Option.Some" => {
            let arg = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            Some(format!("Some({})", arg))
        }
        "Option.withDefault" => {
            let opt = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let default = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!("{}.unwrap_or({})", opt, default))
        }
        "Option.toResult" => {
            let opt = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let err = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!("{}.ok_or({})", opt, err))
        }

        // ---- Int ----
        "Int.abs" => {
            let arg = emit_arg(0);
            Some(format!("{}.abs()", arg))
        }
        "Int.toFloat" => {
            let arg = emit_arg(0);
            Some(format!("({} as f64)", arg))
        }
        "Int.toString" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_string()", arg))
        }
        "Int.parse" => {
            let arg = emit_arg(0);
            Some(format!("{}.parse::<i64>().map_err(|e| e.to_string())", arg))
        }
        "Int.fromString" => {
            let arg = emit_arg(0);
            Some(format!("{}.parse::<i64>().map_err(|e| e.to_string())", arg))
        }
        "Int.min" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("{}.min({})", a, b))
        }
        "Int.max" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("{}.max({})", a, b))
        }
        "Int.rem" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("({} % {})", a, b))
        }
        "Int.mod" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!(
                "if ({b}) == 0i64 {{ Err(\"Int.mod: divisor must not be zero\".to_string()) }} else {{ Ok(({a}).rem_euclid({b})) }}"
            ))
        }

        // ---- Float ----
        "Float.abs" => {
            let arg = emit_arg(0);
            Some(format!("{}.abs()", arg))
        }
        "Float.round" => {
            let arg = emit_arg(0);
            Some(format!("{}.round() as i64", arg))
        }
        "Float.floor" => {
            let arg = emit_arg(0);
            Some(format!("{}.floor() as i64", arg))
        }
        "Float.ceil" => {
            let arg = emit_arg(0);
            Some(format!("{}.ceil() as i64", arg))
        }
        "Float.fromString" => {
            let arg = emit_arg(0);
            Some(format!("{}.parse::<f64>().map_err(|e| e.to_string())", arg))
        }
        "Float.toInt" => {
            let arg = emit_arg(0);
            Some(format!("({} as i64)", arg))
        }
        "Float.toString" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_string()", arg))
        }
        "Float.parse" => {
            let arg = emit_arg(0);
            Some(format!("{}.parse::<f64>().map_err(|e| e.to_string())", arg))
        }
        "Float.sqrt" => {
            let arg = emit_arg(0);
            Some(format!("{}.sqrt()", arg))
        }
        "Float.pow" => {
            let base = emit_arg(0);
            let exp = emit_arg(1);
            Some(format!("{}.powf({})", base, exp))
        }
        "Float.min" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("{}.min({})", a, b))
        }
        "Float.max" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("{}.max({})", a, b))
        }
        "Float.sin" => Some(format!("{}.sin()", emit_arg(0))),
        "Float.cos" => Some(format!("{}.cos()", emit_arg(0))),
        "Float.atan2" => {
            let y = emit_arg(0);
            let x = emit_arg(1);
            Some(format!("{}.atan2({})", y, x))
        }
        "Float.pi" => Some("std::f64::consts::PI".to_string()),
        "Float.fromInt" => Some(format!("{} as f64", emit_arg(0))),

        // ---- String ----
        "String.fromInt" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_string()", arg))
        }
        "String.fromFloat" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_string()", arg))
        }
        "String.fromBool" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_string()", arg))
        }
        "String.charAt" => {
            let s = emit_arg(0);
            let idx = emit_arg(1);
            Some(format!(
                "{}.chars().nth({} as usize).map(|c| c.to_string())",
                s, idx
            ))
        }
        "String.len" => {
            let arg = emit_arg(0);
            Some(format!("({}.chars().count() as i64)", arg))
        }
        "String.slice" => {
            let s = emit_arg(0);
            let from = emit_arg(1);
            let to = emit_arg(2);
            Some(format!("aver_rt::string_slice(&{}, {}, {})", s, from, to))
        }
        "String.contains" => {
            let s = emit_arg(0);
            let sub = emit_str_arg_or_deref(&args[1], &arg_ctxs[1], ctx);
            Some(format!("{}.contains({})", s, sub))
        }
        "String.startsWith" => {
            let s = emit_arg(0);
            let prefix = emit_str_arg_or_deref(&args[1], &arg_ctxs[1], ctx);
            Some(format!("{}.starts_with({})", s, prefix))
        }
        "String.endsWith" => {
            let s = emit_arg(0);
            let suffix = emit_str_arg_or_deref(&args[1], &arg_ctxs[1], ctx);
            Some(format!("{}.ends_with({})", s, suffix))
        }
        "String.trim" => {
            let arg = emit_arg(0);
            Some(format!("{}.trim().to_string()", arg))
        }
        "String.toUpper" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_uppercase()", arg))
        }
        "String.toLower" => {
            let arg = emit_arg(0);
            Some(format!("{}.to_lowercase()", arg))
        }
        "String.split" => {
            let s = emit_arg(0);
            let delim = emit_arg(1);
            Some(format!(
                "aver_rt::AverList::from_vec({}.split(&*{}).map(|s| s.to_string()).collect::<Vec<_>>())",
                s, delim
            ))
        }
        "String.join" => {
            let parts = emit_arg(0);
            let delim = emit_arg(1);
            Some(format!("aver_rt::string_join(&{}, &{})", parts, delim))
        }
        "String.replace" => {
            let s = emit_arg(0);
            let from = emit_arg(1);
            let to = emit_arg(2);
            Some(format!("{}.replace(&*{}, &*{})", s, from, to))
        }
        "String.chars" => {
            let arg = emit_arg(0);
            Some(format!(
                "aver_rt::AverList::from_vec({}.chars().map(|c| c.to_string()).collect::<Vec<_>>())",
                arg
            ))
        }
        "String.repeat" => {
            let s = emit_arg(0);
            let n = emit_arg(1);
            Some(format!("{}.repeat({} as usize)", s, n))
        }
        "String.indexOf" => {
            let s = emit_arg(0);
            let sub = emit_arg(1);
            Some(format!(
                "{}.find(&*{}).map(|i| i as i64).unwrap_or(-1i64)",
                s, sub
            ))
        }

        // ---- List ----
        "List.len" => {
            if let Expr::List(items) = &args[0]
                && items.is_empty()
            {
                Some("0i64".to_string())
            } else {
                let arg = emit_arg(0);
                Some(format!("({}.len() as i64)", arg))
            }
        }
        "List.prepend" => {
            let item = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let list = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!("aver_rt::AverList::prepend({}, &{})", item, list))
        }
        "List.concat" => {
            let left = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let right = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!("aver_rt::AverList::concat(&{}, &{})", left, right))
        }
        "List.reverse" => {
            let list = emit_expr(&args[0], ctx, &arg_ctxs[0]);
            Some(format!("{}.reverse()", list))
        }
        "List.contains" => {
            let list = emit_arg(0);
            let item = emit_arg(1);
            Some(format!("{}.contains(&{})", list, item))
        }
        "List.zip" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!(
                "aver_rt::AverList::from_vec({}.iter().zip({}.iter()).map(|(a, b)| (a.clone(), b.clone())).collect::<Vec<_>>())",
                a, b
            ))
        }
        // ---- Map ----
        "Map.empty" => Some("HashMap::new()".to_string()),
        "Map.fromList" => {
            let list = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            Some(format!(
                "{{ let mut m = HashMap::new(); for (k, v) in {}.iter().cloned() {{ m = m.insert_owned(k, v); }} m }}",
                list
            ))
        }
        "Map.entries" => {
            let map = emit_arg(0);
            Some(format!(
                "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); aver_rt::AverList::from_vec(es) }}",
                map
            ))
        }
        "Map.get" => {
            let map = emit_arg(0);
            let key = emit_arg(1);
            Some(format!("{}.get(&{}).cloned()", map, key))
        }
        "Map.set" => {
            let map = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let key = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            let val = clone_arg(&args[2], ctx, &arg_ctxs[2]);
            Some(format!("{}.insert_owned({}, {})", map, key, val))
        }
        "Map.has" => {
            let map = emit_arg(0);
            let key = emit_arg(1);
            Some(format!("{}.contains_key(&{})", map, key))
        }
        "Map.remove" => {
            let map = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let key = emit_arg(1);
            Some(format!("{}.remove_owned(&{})", map, key))
        }
        "Map.keys" => {
            let map = emit_arg(0);
            Some(format!(
                "{{ let mut ks: Vec<_> = {}.keys().cloned().collect(); ks.sort(); aver_rt::AverList::from_vec(ks) }}",
                map
            ))
        }
        "Map.values" => {
            let map = emit_arg(0);
            Some(format!(
                "aver_rt::AverList::from_vec({}.values().cloned().collect::<Vec<_>>())",
                map
            ))
        }
        "Map.len" => {
            let map = emit_arg(0);
            Some(format!("({}.len() as i64)", map))
        }

        // ---- Bool ----
        "Bool.or" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("({} || {})", a, b))
        }
        "Bool.and" => {
            let a = emit_arg(0);
            let b = emit_arg(1);
            Some(format!("({} && {})", a, b))
        }
        "Bool.not" => {
            let a = emit_arg(0);
            Some(format!("(!{})", a))
        }

        // ---- Char ----
        "Char.toCode" => {
            let arg = emit_arg(0);
            Some(format!(
                "({}.chars().next().map(|c| c as i64).unwrap_or(0i64))",
                arg
            ))
        }
        "Char.fromCode" => {
            let arg = emit_arg(0);
            Some(format!(
                "char::from_u32({} as u32).map(|c| c.to_string())",
                arg
            ))
        }

        // ---- Byte ----
        "Byte.toHex" => {
            let arg = emit_arg(0);
            Some(format!(
                "{{ let __n = {}; if (0i64..=255i64).contains(&__n) {{ Ok(format!(\"{{:02x}}\", __n as u8)) }} else {{ Err(format!(\"Byte.toHex: {{}} is out of range 0–255\", __n)) }} }}",
                arg
            ))
        }
        "Byte.fromHex" => {
            let arg = emit_arg(0);
            Some(format!(
                "{{ let __s = {}; if __s.len() != 2 {{ Err(format!(\"Byte.fromHex: expected exactly 2 hex chars, got '{{}}'\", __s)) }} else {{ u8::from_str_radix(&__s, 16).map(|n| n as i64).map_err(|_| format!(\"Byte.fromHex: invalid hex '{{}}'\", __s)) }} }}",
                arg
            ))
        }

        // ---- String.byteLength ----
        "String.byteLength" => {
            let arg = emit_arg(0);
            Some(format!("({}.len() as i64)", arg))
        }

        // ---- Vector ----
        "Vector.new" => {
            let size = emit_arg(0);
            let default = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!(
                "aver_rt::AverVector::new({} as usize, {})",
                size, default
            ))
        }
        "Vector.get" => {
            let vec = emit_arg(0);
            let idx = emit_arg(1);
            Some(format!("{}.get({} as usize).cloned()", vec, idx))
        }
        "Vector.set" => {
            let vec = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let idx = emit_arg(1);
            let val = clone_arg(&args[2], ctx, &arg_ctxs[2]);
            Some(format!("{}.set_owned({} as usize, {})", vec, idx, val))
        }
        "Vector.len" => {
            let vec = emit_arg(0);
            Some(format!("({}.len() as i64)", vec))
        }
        "Vector.fromList" => {
            let list = emit_arg(0);
            Some(format!("aver_rt::AverVector::from_vec({}.to_vec())", list))
        }
        "Vector.toList" => {
            let vec = emit_arg(0);
            Some(format!("{}.to_list()", vec))
        }

        // ---- Tcp ----
        "Tcp.connect" => {
            let host = emit_arg(0);
            let port = emit_arg(1);
            Some(format!("aver_rt::tcp::connect(&{}, {})", host, port))
        }
        "Tcp.writeLine" => {
            let conn = emit_arg(0);
            let line = emit_arg(1);
            Some(format!("aver_rt::tcp::write_line(&{}, &{})", conn, line))
        }
        "Tcp.readLine" => {
            let conn = emit_arg(0);
            Some(format!("aver_rt::tcp::read_line(&{})", conn))
        }
        "Tcp.close" => {
            let conn = emit_arg(0);
            Some(format!("aver_rt::tcp::close(&{})", conn))
        }
        "Tcp.send" => {
            let host = emit_arg(0);
            let port = emit_arg(1);
            let msg = emit_arg(2);
            Some(format!("aver_rt::tcp::send(&{}, {}, &{})", host, port, msg))
        }
        "Tcp.ping" => {
            let host = emit_arg(0);
            let port = emit_arg(1);
            Some(format!("aver_rt::tcp::ping(&{}, {})", host, port))
        }

        // ---- Http ----
        "Http.get" => {
            let url = emit_arg(0);
            Some(format!("aver_rt::http::get(&{})", url))
        }
        "Http.head" => {
            let url = emit_arg(0);
            Some(format!("aver_rt::http::head(&{})", url))
        }
        "Http.delete" => {
            let url = emit_arg(0);
            Some(format!("aver_rt::http::delete(&{})", url))
        }
        "Http.post" => {
            let url = emit_arg(0);
            let body = emit_arg(1);
            let ct = emit_arg(2);
            let headers = emit_arg(3);
            Some(format!(
                "aver_rt::http::post(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }
        "Http.put" => {
            let url = emit_arg(0);
            let body = emit_arg(1);
            let ct = emit_arg(2);
            let headers = emit_arg(3);
            Some(format!(
                "aver_rt::http::put(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }
        "Http.patch" => {
            let url = emit_arg(0);
            let body = emit_arg(1);
            let ct = emit_arg(2);
            let headers = emit_arg(3);
            Some(format!(
                "aver_rt::http::patch(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }

        // ---- HttpServer ----
        "HttpServer.listen" => {
            let port = emit_arg(0);
            let handler = emit_arg(1);
            Some(format!(
                "{{ if let Err(e) = crate::http_server_listen({}, {}) {{ panic!(\"{{}}\", e); }} }}",
                port, handler
            ))
        }
        "HttpServer.listenWith" => {
            let port = emit_arg(0);
            let context = emit_arg(1);
            let handler = emit_arg(2);
            Some(format!(
                "{{ if let Err(e) = crate::http_server_listen_with({}, {}.clone(), {}) {{ panic!(\"{{}}\", e); }} }}",
                port, context, handler
            ))
        }
        "SelfHostRuntime.httpServerListen" => {
            let port = emit_arg(0);
            let handler = emit_arg(1);
            Some(format!(
                "crate::self_host_support::http_server_listen({}, {})",
                port, handler
            ))
        }
        "SelfHostRuntime.httpServerListenWith" => {
            let port = emit_arg(0);
            let context = emit_arg(1);
            let handler = emit_arg(2);
            Some(format!(
                "crate::self_host_support::http_server_listen_with({}, {}.clone(), {})",
                port, context, handler
            ))
        }

        // ---- Disk ----
        "Disk.readText" => {
            let path = emit_arg(0);
            Some(format!("aver_rt::read_text(&{})", path))
        }
        "Disk.writeText" => {
            let path = emit_arg(0);
            let content = emit_arg(1);
            Some(format!("aver_rt::write_text(&{}, &{})", path, content))
        }
        "Disk.appendText" => {
            let path = emit_arg(0);
            let content = emit_arg(1);
            Some(format!("aver_rt::append_text(&{}, &{})", path, content))
        }
        "Disk.exists" => {
            let path = emit_arg(0);
            Some(format!("aver_rt::path_exists(&{})", path))
        }
        "Disk.delete" => {
            let path = emit_arg(0);
            Some(format!("aver_rt::delete_file(&{})", path))
        }
        "Disk.deleteDir" => {
            let path = emit_arg(0);
            Some(format!("aver_rt::delete_dir(&{})", path))
        }
        "Disk.listDir" => {
            let path = emit_arg(0);
            Some(format!("aver_rt::list_dir(&{})", path))
        }
        "Disk.makeDir" => {
            let path = emit_arg(0);
            Some(format!("aver_rt::make_dir(&{})", path))
        }

        // ---- Env ----
        "Env.get" => {
            let key = emit_arg(0);
            Some(format!("aver_rt::env_get(&{})", key))
        }
        "Env.set" => {
            let key = emit_arg(0);
            let value = emit_arg(1);
            Some(format!(
                "aver_rt::env_set(&{}, &{}).expect(\"Env.set failed\")",
                key, value
            ))
        }
        "Args.get" => Some("aver_rt::cli_args().into_aver()".to_string()),

        // ---- Time ----
        "Time.now" => Some("aver_rt::time_now()".to_string()),
        "Time.unixMs" => Some("aver_rt::time_unix_ms()".to_string()),
        "Time.sleep" => {
            let ms = emit_arg(0);
            Some(format!("aver_rt::time_sleep({})", ms))
        }

        "Random.int" => {
            let min = emit_arg(0);
            let max = emit_arg(1);
            Some(format!(
                "aver_rt::random::random_int({}, {}).unwrap()",
                min, max
            ))
        }
        "Random.float" => Some("aver_rt::random::random_float()".to_string()),

        // ---- Terminal ----
        "Terminal.enableRawMode" => {
            Some("aver_rt::terminal_enable_raw_mode().unwrap()".to_string())
        }
        "Terminal.disableRawMode" => {
            Some("aver_rt::terminal_disable_raw_mode().unwrap()".to_string())
        }
        "Terminal.clear" => Some("aver_rt::terminal_clear().unwrap()".to_string()),
        "Terminal.moveTo" => {
            let x = emit_arg(0);
            let y = emit_arg(1);
            Some(format!("aver_rt::terminal_move_to({}, {}).unwrap()", x, y))
        }
        "Terminal.print" => {
            let s = emit_arg(0);
            Some(format!(
                "{{ let __s = aver_rt::aver_display(&{}); aver_rt::terminal_print(&__s).unwrap() }}",
                s
            ))
        }
        "Terminal.setColor" => {
            let c = emit_arg(0);
            Some(format!("aver_rt::terminal_set_color(&{}).unwrap()", c))
        }
        "Terminal.resetColor" => Some("aver_rt::terminal_reset_color().unwrap()".to_string()),
        "Terminal.readKey" => Some("aver_rt::terminal_read_key()".to_string()),
        "Terminal.size" => {
            Some("{ let (w, h) = aver_rt::terminal_size().unwrap(); aver_rt::TerminalSize { width: w, height: h } }".to_string())
        }
        "Terminal.hideCursor" => Some("aver_rt::terminal_hide_cursor().unwrap()".to_string()),
        "Terminal.showCursor" => Some("aver_rt::terminal_show_cursor().unwrap()".to_string()),
        "Terminal.flush" => Some("aver_rt::terminal_flush().unwrap()".to_string()),

        _ => None,
    }
}

/// For string-accepting methods (starts_with, contains, ends_with),
/// emit a string literal as `"foo"` (no allocation) or variable as `arg.as_str()`.
fn emit_str_arg_or_deref(expr: &Expr, ectx: &EmitCtx, ctx: &CodegenContext) -> String {
    if let Expr::Literal(crate::ast::Literal::Str(s)) = expr {
        format!("{:?}", s)
    } else {
        let code = emit_expr(expr, ctx, ectx);
        format!("&*{}", code)
    }
}

#[cfg(test)]
mod tests {
    use super::emit_builtin_call;
    use crate::ast::{Expr, Literal};
    use crate::codegen::CodegenContext;
    use crate::codegen::rust::liveness::EmitCtx;
    use crate::types::Type;
    use std::collections::{HashMap, HashSet};

    fn empty_ctx() -> CodegenContext {
        CodegenContext {
            items: vec![],
            fn_sigs: HashMap::new(),
            memo_fns: HashSet::new(),
            memo_safe_types: HashSet::new(),
            type_defs: vec![],
            fn_defs: vec![],
            project_name: "test".to_string(),
            modules: vec![],
            module_prefixes: HashSet::new(),
            policy: None,
            emit_replay_runtime: false,
            runtime_policy_from_env: false,
            guest_entry: None,
            emit_self_host_support: false,
        }
    }

    #[test]
    fn list_len_empty_literal_emits_typed_free_zero() {
        let emitted = emit_builtin_call(
            "List.len",
            &[Expr::List(vec![])],
            &empty_ctx(),
            &EmitCtx::empty(),
        )
        .expect("List.len should emit");

        assert_eq!(emitted, "0i64");
    }

    #[test]
    fn http_post_preserves_nested_arg_liveness() {
        let args = vec![
            Expr::FnCall(
                Box::new(Expr::Ident("queryUrl".to_string())),
                vec![Expr::Ident("config".to_string())],
            ),
            Expr::FnCall(
                Box::new(Expr::Ident("sqlBody".to_string())),
                vec![Expr::Ident("sql".to_string())],
            ),
            Expr::Literal(Literal::Str("application/json".to_string())),
            Expr::List(vec![Expr::FnCall(
                Box::new(Expr::Ident("authHeader".to_string())),
                vec![Expr::Ident("config".to_string())],
            )]),
        ];
        let mut local_types = HashMap::new();
        local_types.insert("config".to_string(), Type::Named("DbConfig".to_string()));
        local_types.insert("sql".to_string(), Type::Str);

        let emitted = emit_builtin_call(
            "Http.post",
            &args,
            &empty_ctx(),
            &EmitCtx::for_fn(local_types),
        )
        .expect("Http.post should emit");

        assert!(emitted.contains("queryUrl(config.clone())"));
        assert!(emitted.contains("authHeader(config)"));
    }
}
