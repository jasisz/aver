use super::expr::{clone_arg, emit_expr};
use super::liveness::{EmitCtx, compute_args_used_after};
/// Mapping of Aver builtin/namespace functions to Rust equivalents.
use crate::ast::Expr;
use crate::codegen::CodegenContext;

/// Try to emit a builtin call as Rust code.
/// Returns `None` if the name is not a builtin (i.e. it's a user function).
pub fn emit_builtin_call(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<String> {
    let result = emit_builtin_call_inner(name, args, ctx, ectx)?;

    // Wrap Http/Disk/Env calls with policy checks when aver.toml policy is present.
    // Use .expect() instead of ? because the call may occur in a non-Result context
    // (e.g. Disk.exists returns Bool). Policy violations are fatal.
    if ctx.policy.is_some() {
        if name.starts_with("Http.") && !args.is_empty() {
            let url_arg = emit_expr(&args[0], ctx, ectx);
            return Some(format!(
                "{{ aver_policy::check_http(\"{}\", &{}).expect(\"aver.toml policy violation\"); {} }}",
                name, url_arg, result
            ));
        }
        if name.starts_with("Disk.") && !args.is_empty() {
            let path_arg = emit_expr(&args[0], ctx, ectx);
            return Some(format!(
                "{{ aver_policy::check_disk(\"{}\", &{}).expect(\"aver.toml policy violation\"); {} }}",
                name, path_arg, result
            ));
        }
        if name.starts_with("Env.") && !args.is_empty() {
            let key_arg = emit_expr(&args[0], ctx, ectx);
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
    let arg_ctxs = compute_args_used_after(args, &ectx.used_after, &ectx.local_types);

    match name {
        // ---- Console ----
        "Console.print" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::console_print(&{})", arg))
        }
        "Console.error" | "Console.warn" => {
            let arg = emit_expr(&args[0], ctx, ectx);
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
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.abs()", arg))
        }
        "Int.toFloat" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("({} as f64)", arg))
        }
        "Int.toString" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_string()", arg))
        }
        "Int.parse" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.parse::<i64>().map_err(|e| e.to_string())", arg))
        }
        "Int.fromString" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.parse::<i64>().map_err(|e| e.to_string())", arg))
        }
        "Int.min" => {
            let a = emit_expr(&args[0], ctx, ectx);
            let b = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.min({})", a, b))
        }
        "Int.max" => {
            let a = emit_expr(&args[0], ctx, ectx);
            let b = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.max({})", a, b))
        }
        "Int.rem" => {
            let a = emit_expr(&args[0], ctx, ectx);
            let b = emit_expr(&args[1], ctx, ectx);
            Some(format!("({} % {})", a, b))
        }
        "Int.mod" => {
            let a = emit_expr(&args[0], ctx, ectx);
            let b = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "if ({b}) == 0i64 {{ Err(\"Int.mod: divisor must not be zero\".to_string()) }} else {{ Ok(({a}).rem_euclid({b})) }}"
            ))
        }

        // ---- Float ----
        "Float.abs" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.abs()", arg))
        }
        "Float.round" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.round() as i64", arg))
        }
        "Float.floor" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.floor() as i64", arg))
        }
        "Float.ceil" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.ceil() as i64", arg))
        }
        "Float.fromString" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.parse::<f64>().map_err(|e| e.to_string())", arg))
        }
        "Float.toInt" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("({} as i64)", arg))
        }
        "Float.toString" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_string()", arg))
        }
        "Float.parse" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.parse::<f64>().map_err(|e| e.to_string())", arg))
        }
        "Float.sqrt" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.sqrt()", arg))
        }
        "Float.pow" => {
            let base = emit_expr(&args[0], ctx, ectx);
            let exp = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.powf({})", base, exp))
        }

        // ---- String ----
        "String.fromInt" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_string()", arg))
        }
        "String.fromFloat" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_string()", arg))
        }
        "String.fromBool" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_string()", arg))
        }
        "String.charAt" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let idx = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "{}.chars().nth({} as usize).map(|c| c.to_string())",
                s, idx
            ))
        }
        "String.len" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("({}.chars().count() as i64)", arg))
        }
        "String.slice" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let from = emit_expr(&args[1], ctx, ectx);
            let to = emit_expr(&args[2], ctx, ectx);
            Some(format!("aver_rt::string_slice(&{}, {}, {})", s, from, to))
        }
        "String.contains" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let sub = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.contains(&*{})", s, sub))
        }
        "String.startsWith" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let prefix = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.starts_with(&*{})", s, prefix))
        }
        "String.endsWith" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let suffix = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.ends_with(&*{})", s, suffix))
        }
        "String.trim" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.trim().to_string()", arg))
        }
        "String.toUpper" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_uppercase()", arg))
        }
        "String.toLower" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.to_lowercase()", arg))
        }
        "String.split" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let delim = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "aver_rt::AverList::from_vec({}.split(&*{}).map(|s| s.to_string()).collect::<Vec<_>>())",
                s, delim
            ))
        }
        "String.join" => {
            let parts = emit_expr(&args[0], ctx, ectx);
            let delim = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::string_join(&{}, &{})", parts, delim))
        }
        "String.replace" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let from = emit_expr(&args[1], ctx, ectx);
            let to = emit_expr(&args[2], ctx, ectx);
            Some(format!("{}.replace(&*{}, &*{})", s, from, to))
        }
        "String.chars" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "aver_rt::AverList::from_vec({}.chars().map(|c| c.to_string()).collect::<Vec<_>>())",
                arg
            ))
        }
        "String.repeat" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let n = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.repeat({} as usize)", s, n))
        }
        "String.indexOf" => {
            let s = emit_expr(&args[0], ctx, ectx);
            let sub = emit_expr(&args[1], ctx, ectx);
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
                let arg = emit_expr(&args[0], ctx, ectx);
                Some(format!("({}.len() as i64)", arg))
            }
        }
        "List.get" => {
            let list = emit_expr(&args[0], ctx, ectx);
            let idx = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.get({} as usize).cloned()", list, idx))
        }
        "List.append" => {
            let list = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            let item = clone_arg(&args[1], ctx, &arg_ctxs[1]);
            Some(format!("aver_rt::AverList::append(&{}, {})", list, item))
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
            let list = emit_expr(&args[0], ctx, ectx);
            let item = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.contains(&{})", list, item))
        }
        "List.zip" => {
            let a = emit_expr(&args[0], ctx, ectx);
            let b = emit_expr(&args[1], ctx, ectx);
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
                "{}.iter().cloned().collect::<HashMap<_, _>>()",
                list
            ))
        }
        "Map.entries" => {
            let map = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); aver_rt::AverList::from_vec(es) }}",
                map
            ))
        }
        "Map.get" => {
            let map = emit_expr(&args[0], ctx, ectx);
            let key = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.get(&{}).cloned()", map, key))
        }
        "Map.set" => {
            let map = emit_expr(&args[0], ctx, ectx);
            let key = emit_expr(&args[1], ctx, ectx);
            let val = emit_expr(&args[2], ctx, ectx);
            Some(format!(
                "{{ let mut m = {}.clone(); m.insert({}, {}); m }}",
                map, key, val
            ))
        }
        "Map.has" => {
            let map = emit_expr(&args[0], ctx, ectx);
            let key = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.contains_key(&{})", map, key))
        }
        "Map.remove" => {
            let map = emit_expr(&args[0], ctx, ectx);
            let key = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "{{ let mut m = {}.clone(); m.remove(&{}); m }}",
                map, key
            ))
        }
        "Map.keys" => {
            let map = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "{{ let mut ks: Vec<_> = {}.keys().cloned().collect(); ks.sort(); aver_rt::AverList::from_vec(ks) }}",
                map
            ))
        }
        "Map.values" => {
            let map = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "aver_rt::AverList::from_vec({}.values().cloned().collect::<Vec<_>>())",
                map
            ))
        }
        "Map.len" => {
            let map = emit_expr(&args[0], ctx, ectx);
            Some(format!("({}.len() as i64)", map))
        }

        // ---- Char ----
        "Char.toCode" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "({}.chars().next().map(|c| c as i64).unwrap_or(0i64))",
                arg
            ))
        }
        "Char.fromCode" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "char::from_u32({} as u32).map(|c| c.to_string())",
                arg
            ))
        }

        // ---- Byte ----
        "Byte.toHex" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "{{ let __n = {}; if (0i64..=255i64).contains(&__n) {{ Ok(format!(\"{{:02x}}\", __n as u8)) }} else {{ Err(format!(\"Byte.toHex: {{}} is out of range 0–255\", __n)) }} }}",
                arg
            ))
        }
        "Byte.fromHex" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "{{ let __s = {}; if __s.len() != 2 {{ Err(format!(\"Byte.fromHex: expected exactly 2 hex chars, got '{{}}'\", __s)) }} else {{ u8::from_str_radix(&__s, 16).map(|n| n as i64).map_err(|_| format!(\"Byte.fromHex: invalid hex '{{}}'\", __s)) }} }}",
                arg
            ))
        }

        // ---- String.byteLength ----
        "String.byteLength" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("({}.len() as i64)", arg))
        }

        // ---- Tcp ----
        "Tcp.connect" => {
            let host = emit_expr(&args[0], ctx, ectx);
            let port = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::tcp::connect(&{}, {})", host, port))
        }
        "Tcp.writeLine" => {
            let conn = emit_expr(&args[0], ctx, ectx);
            let line = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::tcp::write_line(&{}, &{})", conn, line))
        }
        "Tcp.readLine" => {
            let conn = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::tcp::read_line(&{})", conn))
        }
        "Tcp.close" => {
            let conn = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::tcp::close(&{})", conn))
        }
        "Tcp.send" => {
            let host = emit_expr(&args[0], ctx, ectx);
            let port = emit_expr(&args[1], ctx, ectx);
            let msg = emit_expr(&args[2], ctx, ectx);
            Some(format!("aver_rt::tcp::send(&{}, {}, &{})", host, port, msg))
        }
        "Tcp.ping" => {
            let host = emit_expr(&args[0], ctx, ectx);
            let port = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::tcp::ping(&{}, {})", host, port))
        }

        // ---- Http ----
        "Http.get" => {
            let url = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::http::get(&{})", url))
        }
        "Http.head" => {
            let url = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::http::head(&{})", url))
        }
        "Http.delete" => {
            let url = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::http::delete(&{})", url))
        }
        "Http.post" => {
            let url = emit_expr(&args[0], ctx, ectx);
            let body = emit_expr(&args[1], ctx, ectx);
            let ct = emit_expr(&args[2], ctx, ectx);
            let headers = emit_expr(&args[3], ctx, ectx);
            Some(format!(
                "aver_rt::http::post(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }
        "Http.put" => {
            let url = emit_expr(&args[0], ctx, ectx);
            let body = emit_expr(&args[1], ctx, ectx);
            let ct = emit_expr(&args[2], ctx, ectx);
            let headers = emit_expr(&args[3], ctx, ectx);
            Some(format!(
                "aver_rt::http::put(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }
        "Http.patch" => {
            let url = emit_expr(&args[0], ctx, ectx);
            let body = emit_expr(&args[1], ctx, ectx);
            let ct = emit_expr(&args[2], ctx, ectx);
            let headers = emit_expr(&args[3], ctx, ectx);
            Some(format!(
                "aver_rt::http::patch(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }

        // ---- HttpServer ----
        "HttpServer.listen" => {
            let port = emit_expr(&args[0], ctx, ectx);
            let handler = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "{{ if let Err(e) = aver_rt::http_server::listen({}, {}) {{ panic!(\"{{}}\", e); }} }}",
                port, handler
            ))
        }
        "HttpServer.listenWith" => {
            let port = emit_expr(&args[0], ctx, ectx);
            let context = emit_expr(&args[1], ctx, ectx);
            let handler = emit_expr(&args[2], ctx, ectx);
            Some(format!(
                "{{ if let Err(e) = aver_rt::http_server::listen_with({}, {}.clone(), {}) {{ panic!(\"{{}}\", e); }} }}",
                port, context, handler
            ))
        }

        // ---- Disk ----
        "Disk.readText" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::read_text(&{})", path))
        }
        "Disk.writeText" => {
            let path = emit_expr(&args[0], ctx, ectx);
            let content = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::write_text(&{}, &{})", path, content))
        }
        "Disk.appendText" => {
            let path = emit_expr(&args[0], ctx, ectx);
            let content = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::append_text(&{}, &{})", path, content))
        }
        "Disk.exists" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::path_exists(&{})", path))
        }
        "Disk.delete" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::delete_file(&{})", path))
        }
        "Disk.deleteDir" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::delete_dir(&{})", path))
        }
        "Disk.listDir" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::list_dir(&{})", path))
        }
        "Disk.makeDir" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::make_dir(&{})", path))
        }

        // ---- Env ----
        "Env.get" => {
            let key = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::env_get(&{})", key))
        }
        "Env.set" => {
            let key = emit_expr(&args[0], ctx, ectx);
            let value = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "aver_rt::env_set(&{}, &{}).expect(\"Env.set failed\")",
                key, value
            ))
        }
        "Args.get" => Some("aver_rt::cli_args()".to_string()),

        // ---- Time ----
        "Time.now" => Some("aver_rt::time_now()".to_string()),
        "Time.unixMs" => Some("aver_rt::time_unix_ms()".to_string()),
        "Time.sleep" => {
            let ms = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::time_sleep({})", ms))
        }

        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::emit_builtin_call;
    use crate::ast::Expr;
    use crate::codegen::CodegenContext;
    use crate::codegen::rust::liveness::EmitCtx;
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
}
