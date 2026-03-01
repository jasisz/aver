/// Mapping of Aver builtin/namespace functions to Rust equivalents.
use crate::ast::Expr;
use crate::codegen::CodegenContext;
use super::expr::emit_expr;

/// Try to emit a builtin call as Rust code.
/// Returns `None` if the name is not a builtin (i.e. it's a user function).
pub fn emit_builtin_call(name: &str, args: &[Expr], ctx: &CodegenContext) -> Option<String> {
    match name {
        // ---- Console ----
        "Console.print" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("println!(\"{{}}\", aver_rt::aver_display(&{}))", arg))
        }
        "Console.error" | "Console.warn" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("eprintln!(\"{{}}\", aver_rt::aver_display(&{}))", arg))
        }
        "Console.readLine" => {
            Some("aver_rt::read_line()".to_string())
        }

        // ---- Result ----
        "Result.Ok" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("Ok({})", arg))
        }
        "Result.Err" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("Err({})", arg))
        }
        "Result.withDefault" => {
            let result = emit_expr(&args[0], ctx);
            let default = emit_expr(&args[1], ctx);
            Some(format!("{}.unwrap_or({})", result, default))
        }

        // ---- Option ----
        "Option.Some" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("Some({})", arg))
        }
        "Option.withDefault" => {
            let opt = emit_expr(&args[0], ctx);
            let default = emit_expr(&args[1], ctx);
            Some(format!("{}.unwrap_or({})", opt, default))
        }
        "Option.toResult" => {
            let opt = emit_expr(&args[0], ctx);
            let err = emit_expr(&args[1], ctx);
            Some(format!("{}.ok_or({})", opt, err))
        }

        // ---- Int ----
        "Int.abs" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.abs()", arg))
        }
        "Int.toFloat" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("({} as f64)", arg))
        }
        "Int.toString" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.to_string()", arg))
        }
        "Int.parse" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "{}.parse::<i64>().map_err(|e| e.to_string())",
                arg
            ))
        }
        "Int.fromString" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "{}.parse::<i64>().map_err(|e| e.to_string())",
                arg
            ))
        }
        "Int.min" => {
            let a = emit_expr(&args[0], ctx);
            let b = emit_expr(&args[1], ctx);
            Some(format!("{}.min({})", a, b))
        }
        "Int.max" => {
            let a = emit_expr(&args[0], ctx);
            let b = emit_expr(&args[1], ctx);
            Some(format!("{}.max({})", a, b))
        }
        "Int.rem" => {
            let a = emit_expr(&args[0], ctx);
            let b = emit_expr(&args[1], ctx);
            Some(format!("({} % {})", a, b))
        }

        // ---- Float ----
        "Float.abs" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.abs()", arg))
        }
        "Float.round" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.round() as i64", arg))
        }
        "Float.floor" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.floor() as i64", arg))
        }
        "Float.ceil" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.ceil() as i64", arg))
        }
        "Float.fromString" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "{}.parse::<f64>().map_err(|e| e.to_string())",
                arg
            ))
        }
        "Float.toInt" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("({} as i64)", arg))
        }
        "Float.toString" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.to_string()", arg))
        }
        "Float.parse" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "{}.parse::<f64>().map_err(|e| e.to_string())",
                arg
            ))
        }
        "Float.sqrt" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.sqrt()", arg))
        }
        "Float.pow" => {
            let base = emit_expr(&args[0], ctx);
            let exp = emit_expr(&args[1], ctx);
            Some(format!("{}.powf({})", base, exp))
        }

        // ---- String ----
        "String.fromInt" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.to_string()", arg))
        }
        "String.fromFloat" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.to_string()", arg))
        }
        "String.charAt" => {
            let s = emit_expr(&args[0], ctx);
            let idx = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.chars().nth({} as usize).map(|c| c.to_string())",
                s, idx
            ))
        }
        "String.len" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("({}.chars().count() as i64)", arg))
        }
        "String.slice" => {
            let s = emit_expr(&args[0], ctx);
            let from = emit_expr(&args[1], ctx);
            let to = emit_expr(&args[2], ctx);
            Some(format!("aver_rt::string_slice(&{}, {}, {})", s, from, to))
        }
        "String.contains" => {
            let s = emit_expr(&args[0], ctx);
            let sub = emit_expr(&args[1], ctx);
            Some(format!("{}.contains(&*{})", s, sub))
        }
        "String.startsWith" => {
            let s = emit_expr(&args[0], ctx);
            let prefix = emit_expr(&args[1], ctx);
            Some(format!("{}.starts_with(&*{})", s, prefix))
        }
        "String.endsWith" => {
            let s = emit_expr(&args[0], ctx);
            let suffix = emit_expr(&args[1], ctx);
            Some(format!("{}.ends_with(&*{})", s, suffix))
        }
        "String.trim" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.trim().to_string()", arg))
        }
        "String.toUpper" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.to_uppercase()", arg))
        }
        "String.toLower" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("{}.to_lowercase()", arg))
        }
        "String.split" => {
            let s = emit_expr(&args[0], ctx);
            let delim = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.split(&*{}).map(|s| s.to_string()).collect::<Vec<_>>()",
                s, delim
            ))
        }
        "String.join" => {
            let parts = emit_expr(&args[0], ctx);
            let delim = emit_expr(&args[1], ctx);
            Some(format!("{}.join(&*{})", parts, delim))
        }
        "String.replace" => {
            let s = emit_expr(&args[0], ctx);
            let from = emit_expr(&args[1], ctx);
            let to = emit_expr(&args[2], ctx);
            Some(format!("{}.replace(&*{}, &*{})", s, from, to))
        }
        "String.chars" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "{}.chars().map(|c| c.to_string()).collect::<Vec<_>>()",
                arg
            ))
        }
        "String.repeat" => {
            let s = emit_expr(&args[0], ctx);
            let n = emit_expr(&args[1], ctx);
            Some(format!("{}.repeat({} as usize)", s, n))
        }
        "String.indexOf" => {
            let s = emit_expr(&args[0], ctx);
            let sub = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.find(&*{}).map(|i| i as i64).unwrap_or(-1i64)",
                s, sub
            ))
        }

        // ---- List ----
        "List.len" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("({}.len() as i64)", arg))
        }
        "List.get" => {
            let list = emit_expr(&args[0], ctx);
            let idx = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.get({} as usize).cloned()",
                list, idx
            ))
        }
        "List.push" => {
            let list = emit_expr(&args[0], ctx);
            let item = emit_expr(&args[1], ctx);
            Some(format!(
                "{{ let mut v = {}.clone(); v.push({}); v }}",
                list, item
            ))
        }
        "List.head" => {
            let list = emit_expr(&args[0], ctx);
            Some(format!("{}.first().cloned()", list))
        }
        "List.tail" => {
            let list = emit_expr(&args[0], ctx);
            Some(format!(
                "if {list}.len() > 1 {{ Some({list}[1..].to_vec()) }} else if {list}.len() == 1 {{ Some(vec![]) }} else {{ None }}",
                list = list
            ))
        }
        "List.map" => {
            let list = emit_expr(&args[0], ctx);
            let func = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.iter().map(|x| {}(x.clone())).collect::<Vec<_>>()",
                list, func
            ))
        }
        "List.filter" => {
            let list = emit_expr(&args[0], ctx);
            let func = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.iter().filter(|x| {}((*x).clone())).cloned().collect::<Vec<_>>()",
                list, func
            ))
        }
        "List.fold" => {
            let list = emit_expr(&args[0], ctx);
            let init = emit_expr(&args[1], ctx);
            let func = emit_expr(&args[2], ctx);
            Some(format!(
                "{}.iter().fold({}, |acc, x| {}(acc, x.clone()))",
                list, init, func
            ))
        }
        "List.find" => {
            let list = emit_expr(&args[0], ctx);
            let func = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.iter().find(|x| {}((*x).clone())).cloned()",
                list, func
            ))
        }
        "List.any" => {
            let list = emit_expr(&args[0], ctx);
            let func = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.iter().any(|x| {}((*x).clone()))",
                list, func
            ))
        }
        "List.flatMap" => {
            let list = emit_expr(&args[0], ctx);
            let func = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.iter().flat_map(|x| {}(x.clone())).collect::<Vec<_>>()",
                list, func
            ))
        }
        "List.reverse" => {
            let list = emit_expr(&args[0], ctx);
            Some(format!(
                "{{ let mut v = {}.clone(); v.reverse(); v }}",
                list
            ))
        }
        "List.sort" => {
            let list = emit_expr(&args[0], ctx);
            Some(format!(
                "{{ let mut v = {}.clone(); v.sort(); v }}",
                list
            ))
        }
        "List.contains" => {
            let list = emit_expr(&args[0], ctx);
            let item = emit_expr(&args[1], ctx);
            Some(format!("{}.contains(&{})", list, item))
        }
        "List.zip" => {
            let a = emit_expr(&args[0], ctx);
            let b = emit_expr(&args[1], ctx);
            Some(format!(
                "{}.iter().zip({}.iter()).map(|(a, b)| (a.clone(), b.clone())).collect::<Vec<_>>()",
                a, b
            ))
        }
        "List.range" => {
            let from = emit_expr(&args[0], ctx);
            let to = emit_expr(&args[1], ctx);
            Some(format!("({}..={}).collect::<Vec<i64>>()", from, to))
        }

        // ---- Map ----
        "Map.empty" => {
            Some("HashMap::new()".to_string())
        }
        "Map.fromList" => {
            let list = emit_expr(&args[0], ctx);
            Some(format!("{}.into_iter().collect::<HashMap<_, _>>()", list))
        }
        "Map.entries" => {
            let map = emit_expr(&args[0], ctx);
            Some(format!(
                "{{ let mut es: Vec<_> = {}.iter().map(|(k, v)| (k.clone(), v.clone())).collect(); es.sort_by(|a, b| a.0.cmp(&b.0)); es }}",
                map
            ))
        }
        "Map.get" => {
            let map = emit_expr(&args[0], ctx);
            let key = emit_expr(&args[1], ctx);
            Some(format!("{}.get(&{}).cloned()", map, key))
        }
        "Map.set" => {
            let map = emit_expr(&args[0], ctx);
            let key = emit_expr(&args[1], ctx);
            let val = emit_expr(&args[2], ctx);
            Some(format!(
                "{{ let mut m = {}.clone(); m.insert({}, {}); m }}",
                map, key, val
            ))
        }
        "Map.has" => {
            let map = emit_expr(&args[0], ctx);
            let key = emit_expr(&args[1], ctx);
            Some(format!("{}.contains_key(&{})", map, key))
        }
        "Map.remove" => {
            let map = emit_expr(&args[0], ctx);
            let key = emit_expr(&args[1], ctx);
            Some(format!(
                "{{ let mut m = {}.clone(); m.remove(&{}); m }}",
                map, key
            ))
        }
        "Map.keys" => {
            let map = emit_expr(&args[0], ctx);
            Some(format!(
                "{{ let mut ks: Vec<_> = {}.keys().cloned().collect(); ks.sort(); ks }}",
                map
            ))
        }
        "Map.values" => {
            let map = emit_expr(&args[0], ctx);
            Some(format!(
                "{}.values().cloned().collect::<Vec<_>>()",
                map
            ))
        }
        "Map.len" => {
            let map = emit_expr(&args[0], ctx);
            Some(format!("({}.len() as i64)", map))
        }

        // ---- Char ----
        "Char.toCode" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "({}.chars().next().map(|c| c as i64).unwrap_or(0i64))",
                arg
            ))
        }
        "Char.fromCode" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "char::from_u32({} as u32).map(|c| c.to_string())",
                arg
            ))
        }

        // ---- Byte ----
        "Byte.toHex" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("format!(\"{{:02x}}\", {} as u8)", arg))
        }
        "Byte.fromHex" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!(
                "i64::from_str_radix(&{}, 16).map_err(|e| e.to_string())",
                arg
            ))
        }

        // ---- String.byteLength ----
        "String.byteLength" => {
            let arg = emit_expr(&args[0], ctx);
            Some(format!("({}.len() as i64)", arg))
        }

        // ---- Tcp ----
        "Tcp.connect" => {
            let host = emit_expr(&args[0], ctx);
            let port = emit_expr(&args[1], ctx);
            Some(format!("aver_tcp::connect(&{}, {})", host, port))
        }
        "Tcp.writeLine" => {
            let conn = emit_expr(&args[0], ctx);
            let line = emit_expr(&args[1], ctx);
            Some(format!("aver_tcp::write_line(&{}, &{})", conn, line))
        }
        "Tcp.readLine" => {
            let conn = emit_expr(&args[0], ctx);
            Some(format!("aver_tcp::read_line(&{})", conn))
        }
        "Tcp.close" => {
            let conn = emit_expr(&args[0], ctx);
            Some(format!("aver_tcp::close(&{})", conn))
        }
        "Tcp.send" => {
            let host = emit_expr(&args[0], ctx);
            let port = emit_expr(&args[1], ctx);
            let msg = emit_expr(&args[2], ctx);
            Some(format!("aver_tcp::send(&{}, {}, &{})", host, port, msg))
        }
        "Tcp.ping" => {
            let host = emit_expr(&args[0], ctx);
            let port = emit_expr(&args[1], ctx);
            Some(format!("aver_tcp::ping(&{}, {})", host, port))
        }

        // ---- Http ----
        "Http.get" => {
            let url = emit_expr(&args[0], ctx);
            Some(format!("aver_http::get(&{})", url))
        }
        "Http.head" => {
            let url = emit_expr(&args[0], ctx);
            Some(format!("aver_http::head(&{})", url))
        }
        "Http.delete" => {
            let url = emit_expr(&args[0], ctx);
            Some(format!("aver_http::delete(&{})", url))
        }
        "Http.post" => {
            let url = emit_expr(&args[0], ctx);
            let body = emit_expr(&args[1], ctx);
            let ct = emit_expr(&args[2], ctx);
            let headers = emit_expr(&args[3], ctx);
            Some(format!("aver_http::post(&{}, &{}, &{}, &{})", url, body, ct, headers))
        }
        "Http.put" => {
            let url = emit_expr(&args[0], ctx);
            let body = emit_expr(&args[1], ctx);
            let ct = emit_expr(&args[2], ctx);
            let headers = emit_expr(&args[3], ctx);
            Some(format!("aver_http::put(&{}, &{}, &{}, &{})", url, body, ct, headers))
        }
        "Http.patch" => {
            let url = emit_expr(&args[0], ctx);
            let body = emit_expr(&args[1], ctx);
            let ct = emit_expr(&args[2], ctx);
            let headers = emit_expr(&args[3], ctx);
            Some(format!("aver_http::patch(&{}, &{}, &{}, &{})", url, body, ct, headers))
        }

        // ---- HttpServer ----
        "HttpServer.listen" => {
            let port = emit_expr(&args[0], ctx);
            let handler = emit_expr(&args[1], ctx);
            Some(format!("aver_http_server::listen({}, {})", port, handler))
        }
        "HttpServer.listenWith" => {
            let port = emit_expr(&args[0], ctx);
            let context = emit_expr(&args[1], ctx);
            let handler = emit_expr(&args[2], ctx);
            Some(format!("aver_http_server::listen_with({}, {}.clone(), {})", port, context, handler))
        }

        // ---- Disk ----
        "Disk.readText" => {
            let path = emit_expr(&args[0], ctx);
            Some(format!(
                "std::fs::read_to_string(&{}).map_err(|e| e.to_string())",
                path
            ))
        }
        "Disk.writeText" => {
            let path = emit_expr(&args[0], ctx);
            let content = emit_expr(&args[1], ctx);
            Some(format!(
                "std::fs::write(&{}, &{}).map(|_| ()).map_err(|e| e.to_string())",
                path, content
            ))
        }
        "Disk.appendText" => {
            let path = emit_expr(&args[0], ctx);
            let content = emit_expr(&args[1], ctx);
            Some(format!(
                "aver_rt::append_text(&{}, &{})",
                path, content
            ))
        }
        "Disk.exists" => {
            let path = emit_expr(&args[0], ctx);
            Some(format!("std::path::Path::new(&{}).exists()", path))
        }
        "Disk.delete" => {
            let path = emit_expr(&args[0], ctx);
            Some(format!(
                "std::fs::remove_file(&{}).map(|_| ()).map_err(|e| e.to_string())",
                path
            ))
        }
        "Disk.deleteDir" => {
            let path = emit_expr(&args[0], ctx);
            Some(format!(
                "std::fs::remove_dir_all(&{}).map(|_| ()).map_err(|e| e.to_string())",
                path
            ))
        }
        "Disk.listDir" => {
            let path = emit_expr(&args[0], ctx);
            Some(format!("aver_rt::list_dir(&{})", path))
        }
        "Disk.makeDir" => {
            let path = emit_expr(&args[0], ctx);
            Some(format!(
                "std::fs::create_dir_all(&{}).map(|_| ()).map_err(|e| e.to_string())",
                path
            ))
        }

        _ => None,
    }
}
