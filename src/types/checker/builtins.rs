use super::*;

impl TypeChecker {
    pub(super) fn register_builtins(&mut self) {
        // No flat builtins — all functions live in namespaces.

        // Register built-in record field types for HttpResponse / HttpRequest and Header.
        // This enables checked dot-access: resp.status → Int, req.path → String, etc.
        // HTTP headers are `Map<String, List<String>>` — multi-value
        // semantics match HTTP (RFC 9110 same-name fields, RFC 6265
        // Set-Cookie). Keys are case-insensitive by convention; the
        // runtime normalizes incoming names to lowercase. Mirrors
        // Go's `net/http.Header = map[string][]string`.
        let header_map = || {
            Type::Map(
                Box::new(Type::Str),
                Box::new(Type::List(Box::new(Type::Str))),
            )
        };
        let net_resp_fields: &[(&str, Type)] = &[
            ("status", Type::Int),
            ("body", Type::Str),
            ("headers", header_map()),
        ];
        for (field, ty) in net_resp_fields {
            self.record_field_types.insert(
                crate::visibility::member_key("HttpResponse", field),
                ty.clone(),
            );
        }
        let net_req_fields: &[(&str, Type)] = &[
            ("method", Type::Str),
            ("path", Type::Str),
            ("query", Type::Str),
            ("body", Type::Str),
            ("headers", header_map()),
        ];
        for (field, ty) in net_req_fields {
            self.record_field_types.insert(
                crate::visibility::member_key("HttpRequest", field),
                ty.clone(),
            );
        }
        // Oracle v1: EffectEvent = { method: String, args: List<Unknown> }.
        // `args` element type is heterogeneous across effects (Int for
        // Random.int, Str for Console.print<T=Str>, etc.) — v0 types it
        // as `List<Unknown>` so users can inspect events without the
        // checker blocking on a polymorphic arg element type. Richer
        // typing ties args to the effect method's runtime_params once
        // we thread method-dispatch-driven arg types into the checker.
        let effect_event_fields: &[(&str, Type)] = &[
            ("method", Type::Str),
            ("args", Type::List(Box::new(Type::Unknown))),
            ("path", Type::Str),
        ];
        for (field, ty) in effect_event_fields {
            self.record_field_types.insert(
                crate::visibility::member_key(crate::types::effect_event::TYPE_NAME, field),
                ty.clone(),
            );
        }

        // Oracle v1: Trace = { events: List<EffectEvent> }.
        let trace_fields: &[(&str, Type)] = &[(
            "events",
            Type::List(Box::new(Type::Named(
                crate::types::effect_event::TYPE_NAME.to_string(),
            ))),
        )];
        for (field, ty) in trace_fields {
            self.record_field_types.insert(
                crate::visibility::member_key(crate::types::trace::TYPE_NAME, field),
                ty.clone(),
            );
        }
        let tcp_conn_fields: &[(&str, Type)] =
            &[("id", Type::Str), ("host", Type::Str), ("port", Type::Int)];
        for (field, ty) in tcp_conn_fields {
            self.record_field_types.insert(
                crate::visibility::member_key("Tcp.Connection", field),
                ty.clone(),
            );
        }

        let net_ret = || {
            Type::Result(
                Box::new(Type::Named("HttpResponse".to_string())),
                Box::new(Type::Str),
            )
        };
        let disk_unit = || Type::Result(Box::new(Type::Unit), Box::new(Type::Str));
        let disk_str = || Type::Result(Box::new(Type::Str), Box::new(Type::Str));
        let disk_list = || {
            Type::Result(
                Box::new(Type::List(Box::new(Type::Str))),
                Box::new(Type::Str),
            )
        };
        // Http.post/put/patch headers param: same shape as the
        // record fields above (`Map<String, List<String>>`).
        let header_list = header_map;
        let server_handler_effects = || {
            vec![
                "Args.get".to_string(),
                "Console.print".to_string(),
                "Console.error".to_string(),
                "Console.warn".to_string(),
                "Console.readLine".to_string(),
                "Http.get".to_string(),
                "Http.head".to_string(),
                "Http.delete".to_string(),
                "Http.post".to_string(),
                "Http.put".to_string(),
                "Http.patch".to_string(),
                "Disk.readText".to_string(),
                "Disk.writeText".to_string(),
                "Disk.appendText".to_string(),
                "Disk.exists".to_string(),
                "Disk.delete".to_string(),
                "Disk.deleteDir".to_string(),
                "Disk.listDir".to_string(),
                "Disk.makeDir".to_string(),
                "Env.get".to_string(),
                "Env.set".to_string(),
                "Tcp.connect".to_string(),
                "Tcp.send".to_string(),
                "Tcp.ping".to_string(),
                "Tcp.writeLine".to_string(),
                "Tcp.readLine".to_string(),
                "Tcp.close".to_string(),
                "HttpServer.listen".to_string(),
                "HttpServer.listenWith".to_string(),
                "Random.int".to_string(),
                "Random.float".to_string(),
                "Time.now".to_string(),
                "Time.unixMs".to_string(),
                "Time.sleep".to_string(),
            ]
        };
        let http_handler = || {
            Type::Fn(
                vec![Type::Named("HttpRequest".to_string())],
                Box::new(Type::Named("HttpResponse".to_string())),
                server_handler_effects(),
            )
        };
        let http_handler_with_context = || {
            Type::Fn(
                vec![Type::Unknown, Type::Named("HttpRequest".to_string())],
                Box::new(Type::Named("HttpResponse".to_string())),
                server_handler_effects(),
            )
        };
        let service_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "Args.get",
                &[],
                Type::List(Box::new(Type::Str)),
                &["Args.get"],
            ),
            (
                "Console.print",
                &[Type::Unknown],
                Type::Unit,
                &["Console.print"],
            ),
            (
                "Console.error",
                &[Type::Unknown],
                Type::Unit,
                &["Console.error"],
            ),
            (
                "Console.warn",
                &[Type::Unknown],
                Type::Unit,
                &["Console.warn"],
            ),
            (
                "Console.readLine",
                &[],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &["Console.readLine"],
            ),
            ("Http.get", &[Type::Str], net_ret(), &["Http.get"]),
            ("Http.head", &[Type::Str], net_ret(), &["Http.head"]),
            ("Http.delete", &[Type::Str], net_ret(), &["Http.delete"]),
            (
                "Http.post",
                &[Type::Str, Type::Str, Type::Str, header_list()],
                net_ret(),
                &["Http.post"],
            ),
            (
                "Http.put",
                &[Type::Str, Type::Str, Type::Str, header_list()],
                net_ret(),
                &["Http.put"],
            ),
            (
                "Http.patch",
                &[Type::Str, Type::Str, Type::Str, header_list()],
                net_ret(),
                &["Http.patch"],
            ),
            (
                "HttpServer.listen",
                &[Type::Int, http_handler()],
                Type::Unit,
                &["HttpServer.listen"],
            ),
            (
                "HttpServer.listenWith",
                &[Type::Int, Type::Unknown, http_handler_with_context()],
                Type::Unit,
                &["HttpServer.listenWith"],
            ),
            (
                "SelfHostRuntime.httpServerListen",
                &[Type::Int, Type::Unknown],
                disk_unit(),
                &["HttpServer.listen"],
            ),
            (
                "SelfHostRuntime.httpServerListenWith",
                &[Type::Int, Type::Unknown, Type::Unknown],
                disk_unit(),
                &["HttpServer.listenWith"],
            ),
            (
                "Disk.readText",
                &[Type::Str],
                disk_str(),
                &["Disk.readText"],
            ),
            (
                "Disk.writeText",
                &[Type::Str, Type::Str],
                disk_unit(),
                &["Disk.writeText"],
            ),
            (
                "Disk.appendText",
                &[Type::Str, Type::Str],
                disk_unit(),
                &["Disk.appendText"],
            ),
            ("Disk.exists", &[Type::Str], Type::Bool, &["Disk.exists"]),
            ("Disk.delete", &[Type::Str], disk_unit(), &["Disk.delete"]),
            (
                "Disk.deleteDir",
                &[Type::Str],
                disk_unit(),
                &["Disk.deleteDir"],
            ),
            ("Disk.listDir", &[Type::Str], disk_list(), &["Disk.listDir"]),
            ("Disk.makeDir", &[Type::Str], disk_unit(), &["Disk.makeDir"]),
            (
                "Env.get",
                &[Type::Str],
                Type::Option(Box::new(Type::Str)),
                &["Env.get"],
            ),
            ("Env.set", &[Type::Str, Type::Str], Type::Unit, &["Env.set"]),
            (
                "Tcp.send",
                &[Type::Str, Type::Int, Type::Str],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &["Tcp.send"],
            ),
            (
                "Tcp.ping",
                &[Type::Str, Type::Int],
                Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
                &["Tcp.ping"],
            ),
            (
                "Tcp.connect",
                &[Type::Str, Type::Int],
                Type::Result(
                    Box::new(Type::Named("Tcp.Connection".to_string())),
                    Box::new(Type::Str),
                ),
                &["Tcp.connect"],
            ),
            (
                "Tcp.writeLine",
                &[Type::Named("Tcp.Connection".to_string()), Type::Str],
                Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
                &["Tcp.writeLine"],
            ),
            (
                "Tcp.readLine",
                &[Type::Named("Tcp.Connection".to_string())],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &["Tcp.readLine"],
            ),
            (
                "Tcp.close",
                &[Type::Named("Tcp.Connection".to_string())],
                Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
                &["Tcp.close"],
            ),
            (
                "Random.int",
                &[Type::Int, Type::Int],
                Type::Int,
                &["Random.int"],
            ),
            ("Random.float", &[], Type::Float, &["Random.float"]),
            ("Time.now", &[], Type::Str, &["Time.now"]),
            ("Time.unixMs", &[], Type::Int, &["Time.unixMs"]),
            ("Time.sleep", &[Type::Int], Type::Unit, &["Time.sleep"]),
        ];
        for (name, params, ret, effects) in service_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Terminal namespace — always register signatures so static
        // analysis (playground, LSP, `aver check`) knows about them.
        // The runtime impl is still gated by `feature = "terminal"`
        // (crossterm doesn't build on wasm32-unknown-unknown). WASM
        // codegen routes Terminal.* to host imports, no runtime dep.
        {
            let terminal_sigs: &[(&str, &[Type], Type, &[&str])] = &[
                (
                    "Terminal.enableRawMode",
                    &[],
                    Type::Unit,
                    &["Terminal.enableRawMode"],
                ),
                (
                    "Terminal.disableRawMode",
                    &[],
                    Type::Unit,
                    &["Terminal.disableRawMode"],
                ),
                ("Terminal.clear", &[], Type::Unit, &["Terminal.clear"]),
                (
                    "Terminal.moveTo",
                    &[Type::Int, Type::Int],
                    Type::Unit,
                    &["Terminal.moveTo"],
                ),
                (
                    "Terminal.print",
                    &[Type::Unknown],
                    Type::Unit,
                    &["Terminal.print"],
                ),
                (
                    "Terminal.setColor",
                    &[Type::Str],
                    Type::Unit,
                    &["Terminal.setColor"],
                ),
                (
                    "Terminal.resetColor",
                    &[],
                    Type::Unit,
                    &["Terminal.resetColor"],
                ),
                (
                    "Terminal.readKey",
                    &[],
                    Type::Option(Box::new(Type::Str)),
                    &["Terminal.readKey"],
                ),
                (
                    "Terminal.size",
                    &[],
                    Type::Named("Terminal.Size".to_string()),
                    &["Terminal.size"],
                ),
                (
                    "Terminal.hideCursor",
                    &[],
                    Type::Unit,
                    &["Terminal.hideCursor"],
                ),
                (
                    "Terminal.showCursor",
                    &[],
                    Type::Unit,
                    &["Terminal.showCursor"],
                ),
                ("Terminal.flush", &[], Type::Unit, &["Terminal.flush"]),
            ];
            for (name, params, ret, effects) in terminal_sigs {
                self.insert_sig(name, params, ret.clone(), effects);
            }

            // Terminal.Size record field types
            let terminal_size_fields: &[(&str, Type)] =
                &[("width", Type::Int), ("height", Type::Int)];
            for (field, ty) in terminal_size_fields {
                self.record_field_types.insert(
                    crate::visibility::member_key("Terminal.Size", field),
                    ty.clone(),
                );
            }
        }

        // Bool namespace
        let bool_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Bool.or", &[Type::Bool, Type::Bool], Type::Bool, &[]),
            ("Bool.and", &[Type::Bool, Type::Bool], Type::Bool, &[]),
            ("Bool.not", &[Type::Bool], Type::Bool, &[]),
        ];
        for (name, params, ret, effects) in bool_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Int namespace
        let int_result = || Type::Result(Box::new(Type::Int), Box::new(Type::Str));
        let int_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Int.fromString", &[Type::Str], int_result(), &[]),
            ("Int.fromFloat", &[Type::Float], Type::Int, &[]),
            ("Int.toString", &[Type::Int], Type::Str, &[]),
            ("Int.abs", &[Type::Int], Type::Int, &[]),
            ("Int.min", &[Type::Int, Type::Int], Type::Int, &[]),
            ("Int.max", &[Type::Int, Type::Int], Type::Int, &[]),
            ("Int.mod", &[Type::Int, Type::Int], int_result(), &[]),
            ("Int.toFloat", &[Type::Int], Type::Float, &[]),
        ];
        for (name, params, ret, effects) in int_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Float namespace
        let float_result = || Type::Result(Box::new(Type::Float), Box::new(Type::Str));
        let float_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Float.fromString", &[Type::Str], float_result(), &[]),
            ("Float.fromInt", &[Type::Int], Type::Float, &[]),
            ("Float.toString", &[Type::Float], Type::Str, &[]),
            ("Float.abs", &[Type::Float], Type::Float, &[]),
            ("Float.floor", &[Type::Float], Type::Int, &[]),
            ("Float.ceil", &[Type::Float], Type::Int, &[]),
            ("Float.round", &[Type::Float], Type::Int, &[]),
            ("Float.min", &[Type::Float, Type::Float], Type::Float, &[]),
            ("Float.max", &[Type::Float, Type::Float], Type::Float, &[]),
            ("Float.sin", &[Type::Float], Type::Float, &[]),
            ("Float.cos", &[Type::Float], Type::Float, &[]),
            ("Float.sqrt", &[Type::Float], Type::Float, &[]),
            ("Float.pow", &[Type::Float, Type::Float], Type::Float, &[]),
            ("Float.atan2", &[Type::Float, Type::Float], Type::Float, &[]),
            ("Float.pi", &[], Type::Float, &[]),
        ];
        for (name, params, ret, effects) in float_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // String namespace
        let str_list = || Type::List(Box::new(Type::Str));
        let string_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("String.len", &[Type::Str], Type::Int, &[]),
            ("String.byteLength", &[Type::Str], Type::Int, &[]),
            (
                "String.startsWith",
                &[Type::Str, Type::Str],
                Type::Bool,
                &[],
            ),
            ("String.endsWith", &[Type::Str, Type::Str], Type::Bool, &[]),
            ("String.contains", &[Type::Str, Type::Str], Type::Bool, &[]),
            (
                "String.slice",
                &[Type::Str, Type::Int, Type::Int],
                Type::Str,
                &[],
            ),
            ("String.trim", &[Type::Str], Type::Str, &[]),
            ("String.split", &[Type::Str, Type::Str], str_list(), &[]),
            (
                "String.replace",
                &[Type::Str, Type::Str, Type::Str],
                Type::Str,
                &[],
            ),
            ("String.join", &[str_list(), Type::Str], Type::Str, &[]),
            (
                "String.charAt",
                &[Type::Str, Type::Int],
                Type::Option(Box::new(Type::Str)),
                &[],
            ),
            ("String.chars", &[Type::Str], str_list(), &[]),
            ("String.fromInt", &[Type::Int], Type::Str, &[]),
            ("String.fromFloat", &[Type::Float], Type::Str, &[]),
            ("String.fromBool", &[Type::Bool], Type::Str, &[]),
            ("String.toLower", &[Type::Str], Type::Str, &[]),
            ("String.toUpper", &[Type::Str], Type::Str, &[]),
        ];
        for (name, params, ret, effects) in string_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // List namespace
        let any = || Type::Unknown;
        let list_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("List.len", &[Type::List(Box::new(any()))], Type::Int, &[]),
            (
                "List.prepend",
                &[Type::Unknown, Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.take",
                &[Type::Unknown, Type::Int],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.drop",
                &[Type::Unknown, Type::Int],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.concat",
                &[Type::Unknown, Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.reverse",
                &[Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.contains",
                &[Type::Unknown, Type::Unknown],
                Type::Bool,
                &[],
            ),
            (
                "List.zip",
                &[Type::Unknown, Type::Unknown],
                Type::List(Box::new(Type::Tuple(vec![any(), any()]))),
                &[],
            ),
        ];
        for (name, params, ret, effects) in list_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Map namespace
        let map_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "Map.empty",
                &[],
                Type::Map(Box::new(any()), Box::new(any())),
                &[],
            ),
            (
                "Map.set",
                &[Type::Unknown, Type::Unknown, Type::Unknown],
                Type::Map(Box::new(any()), Box::new(any())),
                &[],
            ),
            (
                "Map.get",
                &[Type::Unknown, Type::Unknown],
                Type::Option(Box::new(any())),
                &[],
            ),
            (
                "Map.remove",
                &[Type::Unknown, Type::Unknown],
                Type::Map(Box::new(any()), Box::new(any())),
                &[],
            ),
            ("Map.has", &[Type::Unknown, Type::Unknown], Type::Bool, &[]),
            (
                "Map.keys",
                &[Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "Map.values",
                &[Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "Map.entries",
                &[Type::Unknown],
                Type::List(Box::new(Type::Tuple(vec![any(), any()]))),
                &[],
            ),
            ("Map.len", &[Type::Unknown], Type::Int, &[]),
            (
                "Map.fromList",
                &[Type::Unknown],
                Type::Map(Box::new(any()), Box::new(any())),
                &[],
            ),
        ];
        for (name, params, ret, effects) in map_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Vector namespace
        let vector_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "Vector.new",
                &[Type::Int, Type::Unknown],
                Type::Vector(Box::new(any())),
                &[],
            ),
            (
                "Vector.get",
                &[Type::Unknown, Type::Int],
                Type::Option(Box::new(any())),
                &[],
            ),
            (
                "Vector.set",
                &[Type::Unknown, Type::Int, Type::Unknown],
                Type::Option(Box::new(Type::Vector(Box::new(any())))),
                &[],
            ),
            ("Vector.len", &[Type::Unknown], Type::Int, &[]),
            (
                "Vector.fromList",
                &[Type::Unknown],
                Type::Vector(Box::new(any())),
                &[],
            ),
            (
                "Vector.toList",
                &[Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
        ];
        for (name, params, ret, effects) in vector_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Char namespace
        let char_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Char.toCode", &[Type::Str], Type::Int, &[]),
            (
                "Char.fromCode",
                &[Type::Int],
                Type::Option(Box::new(Type::Str)),
                &[],
            ),
        ];
        for (name, params, ret, effects) in char_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // BranchPath — opaque builtin used by Oracle-proof specs for
        // generative-effect oracles (`(BranchPath, Int, args...) -> T`).
        // Three constructors, no other public surface.
        let branch_path_ty = || Type::Named(crate::types::branch_path::TYPE_NAME.to_string());
        // `BranchPath.Root` is a nullary value (like `Option.None`) —
        // PascalCase, no parens. `.child` / `.parse` are methods.
        self.value_members
            .insert("BranchPath.Root".to_string(), branch_path_ty());
        let branch_path_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "BranchPath.child",
                &[branch_path_ty(), Type::Int],
                branch_path_ty(),
                &[],
            ),
            ("BranchPath.parse", &[Type::Str], branch_path_ty(), &[]),
        ];
        for (name, params, ret, effects) in branch_path_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Byte namespace
        let byte_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "Byte.toHex",
                &[Type::Int],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &[],
            ),
            (
                "Byte.fromHex",
                &[Type::Str],
                Type::Result(Box::new(Type::Int), Box::new(Type::Str)),
                &[],
            ),
        ];
        for (name, params, ret, effects) in byte_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Result.Ok / Result.Err / Option.Some — constructor signatures
        self.insert_sig(
            "Result.Ok",
            &[Type::Unknown],
            Type::Result(Box::new(Type::Unknown), Box::new(Type::Unknown)),
            &[],
        );
        self.insert_sig(
            "Result.Err",
            &[Type::Unknown],
            Type::Result(Box::new(Type::Unknown), Box::new(Type::Unknown)),
            &[],
        );
        self.insert_sig(
            "Option.Some",
            &[Type::Unknown],
            Type::Option(Box::new(Type::Unknown)),
            &[],
        );
        // Option.None — zero-arg value, not a function
        self.value_members.insert(
            "Option.None".to_string(),
            Type::Option(Box::new(Type::Unknown)),
        );

        // Result combinators
        self.insert_sig(
            "Result.withDefault",
            &[Type::Unknown, Type::Unknown],
            Type::Unknown,
            &[],
        );

        // Option combinators
        self.insert_sig(
            "Option.withDefault",
            &[Type::Unknown, Type::Unknown],
            Type::Unknown,
            &[],
        );
        self.insert_sig(
            "Option.toResult",
            &[Type::Unknown, Type::Unknown],
            Type::Result(Box::new(Type::Unknown), Box::new(Type::Unknown)),
            &[],
        );
    }
}
