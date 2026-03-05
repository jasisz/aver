use super::expr::{clone_arg, emit_expr};
use super::liveness::{EmitCtx, compute_args_used_after};
/// Mapping of Aver builtin/namespace functions to Rust equivalents.
use crate::ast::Expr;
use crate::codegen::CodegenContext;

/// Check if an expression is a last-use identifier (can be moved, not cloned).
fn is_last_use_ident(expr: &Expr, ectx: &EmitCtx) -> bool {
    if let Expr::Ident(name) = expr {
        ectx.skip_clone(name)
    } else {
        false
    }
}

/// Try to emit a builtin call as Rust code.
/// Returns `None` if the name is not a builtin (i.e. it's a user function).
pub fn emit_builtin_call(
    name: &str,
    args: &[Expr],
    ctx: &CodegenContext,
    ectx: &EmitCtx,
) -> Option<String> {
    let result = emit_builtin_call_inner(name, args, ctx, ectx)?;

    // Wrap Http/Disk calls with policy checks when aver.toml policy is present.
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
            Some(format!(
                "println!(\"{{}}\", aver_rt::aver_display(&{}))",
                arg
            ))
        }
        "Console.error" | "Console.warn" => {
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "eprintln!(\"{{}}\", aver_rt::aver_display(&{}))",
                arg
            ))
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
                "{}.split(&*{}).map(|s| s.to_string()).collect::<Vec<_>>()",
                s, delim
            ))
        }
        "String.join" => {
            let parts = emit_expr(&args[0], ctx, ectx);
            let delim = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.join(&*{})", parts, delim))
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
                "{}.chars().map(|c| c.to_string()).collect::<Vec<_>>()",
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
            let arg = emit_expr(&args[0], ctx, ectx);
            Some(format!("({}.len() as i64)", arg))
        }
        "List.get" => {
            let list = emit_expr(&args[0], ctx, ectx);
            let idx = emit_expr(&args[1], ctx, ectx);
            Some(format!("{}.get({} as usize).cloned()", list, idx))
        }
        "List.push" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let item = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            if last_use {
                Some(format!("{{ let mut v = {}; v.push({}); v }}", list, item))
            } else {
                Some(format!(
                    "{{ let mut v = {}.clone(); v.push({}); v }}",
                    list, item
                ))
            }
        }
        "List.head" => {
            let list = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.first().cloned()", list))
        }
        "List.tail" => {
            let list = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "if {list}.len() > 1 {{ Some({list}[1..].to_vec()) }} else if {list}.len() == 1 {{ Some(vec![]) }} else {{ None }}",
                list = list
            ))
        }
        "List.map" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let func = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            if last_use {
                Some(format!(
                    "{}.into_iter().map(|x| {}(x)).collect::<Vec<_>>()",
                    list, func
                ))
            } else {
                Some(format!(
                    "{}.iter().map(|x| {}(x.clone())).collect::<Vec<_>>()",
                    list, func
                ))
            }
        }
        "List.filter" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let func = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            if last_use {
                // into_iter gives owned x, but filter takes &x — clone is unavoidable for the predicate call
                Some(format!(
                    "{}.into_iter().filter(|x| {}(x.clone())).collect::<Vec<_>>()",
                    list, func
                ))
            } else {
                Some(format!(
                    "{}.iter().filter(|x| {}((*x).clone())).cloned().collect::<Vec<_>>()",
                    list, func
                ))
            }
        }
        "List.fold" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let init = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            let func = emit_expr(&args[2], ctx, &arg_ctxs[2]);
            if last_use {
                Some(format!(
                    "{}.into_iter().fold({}, |acc, x| {}(acc, x))",
                    list, init, func
                ))
            } else {
                Some(format!(
                    "{}.iter().fold({}, |acc, x| {}(acc, x.clone()))",
                    list, init, func
                ))
            }
        }
        "List.find" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let func = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            if last_use {
                Some(format!(
                    "{}.into_iter().find(|x| {}(x.clone()))",
                    list, func
                ))
            } else {
                Some(format!(
                    "{}.iter().find(|x| {}((*x).clone())).cloned()",
                    list, func
                ))
            }
        }
        "List.any" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let func = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            if last_use {
                Some(format!("{}.into_iter().any(|x| {}(x))", list, func))
            } else {
                Some(format!("{}.iter().any(|x| {}((*x).clone()))", list, func))
            }
        }
        "List.flatMap" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            let func = emit_expr(&args[1], ctx, &arg_ctxs[1]);
            if last_use {
                Some(format!(
                    "{}.into_iter().flat_map(|x| {}(x)).collect::<Vec<_>>()",
                    list, func
                ))
            } else {
                Some(format!(
                    "{}.iter().flat_map(|x| {}(x.clone())).collect::<Vec<_>>()",
                    list, func
                ))
            }
        }
        "List.reverse" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            if last_use {
                Some(format!("{{ let mut v = {}; v.reverse(); v }}", list))
            } else {
                Some(format!(
                    "{{ let mut v = {}.clone(); v.reverse(); v }}",
                    list
                ))
            }
        }
        "List.sort" => {
            let list_expr = &args[0];
            let last_use = is_last_use_ident(list_expr, &arg_ctxs[0]);
            let list = emit_expr(list_expr, ctx, &arg_ctxs[0]);
            if last_use {
                Some(format!("{{ let mut v = {}; v.sort(); v }}", list))
            } else {
                Some(format!("{{ let mut v = {}.clone(); v.sort(); v }}", list))
            }
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
                "{}.iter().zip({}.iter()).map(|(a, b)| (a.clone(), b.clone())).collect::<Vec<_>>()",
                a, b
            ))
        }
        "List.range" => {
            let from = emit_expr(&args[0], ctx, ectx);
            let to = emit_expr(&args[1], ctx, ectx);
            Some(format!("({}..={}).collect::<Vec<i64>>()", from, to))
        }

        // ---- Map ----
        "Map.empty" => Some("HashMap::new()".to_string()),
        "Map.fromList" => {
            let list = clone_arg(&args[0], ctx, &arg_ctxs[0]);
            Some(format!("{}.into_iter().collect::<HashMap<_, _>>()", list))
        }
        "Map.entries" => {
            let map = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); es }}",
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
                "{{ let mut ks: Vec<_> = {}.keys().cloned().collect(); ks.sort(); ks }}",
                map
            ))
        }
        "Map.values" => {
            let map = emit_expr(&args[0], ctx, ectx);
            Some(format!("{}.values().cloned().collect::<Vec<_>>()", map))
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
            Some(format!("aver_tcp::connect(&{}, {})", host, port))
        }
        "Tcp.writeLine" => {
            let conn = emit_expr(&args[0], ctx, ectx);
            let line = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_tcp::write_line(&{}, &{})", conn, line))
        }
        "Tcp.readLine" => {
            let conn = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_tcp::read_line(&{})", conn))
        }
        "Tcp.close" => {
            let conn = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_tcp::close(&{})", conn))
        }
        "Tcp.send" => {
            let host = emit_expr(&args[0], ctx, ectx);
            let port = emit_expr(&args[1], ctx, ectx);
            let msg = emit_expr(&args[2], ctx, ectx);
            Some(format!("aver_tcp::send(&{}, {}, &{})", host, port, msg))
        }
        "Tcp.ping" => {
            let host = emit_expr(&args[0], ctx, ectx);
            let port = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_tcp::ping(&{}, {})", host, port))
        }

        // ---- Http ----
        "Http.get" => {
            let url = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_http::get(&{})", url))
        }
        "Http.head" => {
            let url = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_http::head(&{})", url))
        }
        "Http.delete" => {
            let url = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_http::delete(&{})", url))
        }
        "Http.post" => {
            let url = emit_expr(&args[0], ctx, ectx);
            let body = emit_expr(&args[1], ctx, ectx);
            let ct = emit_expr(&args[2], ctx, ectx);
            let headers = emit_expr(&args[3], ctx, ectx);
            Some(format!(
                "aver_http::post(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }
        "Http.put" => {
            let url = emit_expr(&args[0], ctx, ectx);
            let body = emit_expr(&args[1], ctx, ectx);
            let ct = emit_expr(&args[2], ctx, ectx);
            let headers = emit_expr(&args[3], ctx, ectx);
            Some(format!(
                "aver_http::put(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }
        "Http.patch" => {
            let url = emit_expr(&args[0], ctx, ectx);
            let body = emit_expr(&args[1], ctx, ectx);
            let ct = emit_expr(&args[2], ctx, ectx);
            let headers = emit_expr(&args[3], ctx, ectx);
            Some(format!(
                "aver_http::patch(&{}, &{}, &{}, &{})",
                url, body, ct, headers
            ))
        }

        // ---- HttpServer ----
        "HttpServer.listen" => {
            let port = emit_expr(&args[0], ctx, ectx);
            let handler = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_http_server::listen({}, {})", port, handler))
        }
        "HttpServer.listenWith" => {
            let port = emit_expr(&args[0], ctx, ectx);
            let context = emit_expr(&args[1], ctx, ectx);
            let handler = emit_expr(&args[2], ctx, ectx);
            Some(format!(
                "aver_http_server::listen_with({}, {}.clone(), {})",
                port, context, handler
            ))
        }

        // ---- Disk ----
        "Disk.readText" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "std::fs::read_to_string(&{}).map_err(|e| e.to_string())",
                path
            ))
        }
        "Disk.writeText" => {
            let path = emit_expr(&args[0], ctx, ectx);
            let content = emit_expr(&args[1], ctx, ectx);
            Some(format!(
                "std::fs::write(&{}, &{}).map(|_| ()).map_err(|e| e.to_string())",
                path, content
            ))
        }
        "Disk.appendText" => {
            let path = emit_expr(&args[0], ctx, ectx);
            let content = emit_expr(&args[1], ctx, ectx);
            Some(format!("aver_rt::append_text(&{}, &{})", path, content))
        }
        "Disk.exists" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("std::path::Path::new(&{}).exists()", path))
        }
        "Disk.delete" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "std::fs::remove_file(&{}).map(|_| ()).map_err(|e| e.to_string())",
                path
            ))
        }
        "Disk.deleteDir" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "std::fs::remove_dir_all(&{}).map(|_| ()).map_err(|e| e.to_string())",
                path
            ))
        }
        "Disk.listDir" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!("aver_rt::list_dir(&{})", path))
        }
        "Disk.makeDir" => {
            let path = emit_expr(&args[0], ctx, ectx);
            Some(format!(
                "std::fs::create_dir_all(&{}).map(|_| ()).map_err(|e| e.to_string())",
                path
            ))
        }

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
