use super::*;

impl TypeChecker {
    pub(super) fn register_builtins(&mut self) {
        // No flat builtins — all functions live in namespaces.

        // Register built-in record field types for HttpResponse / HttpRequest and Header.
        // This enables checked dot-access: resp.status → Int, req.path → String, etc.
        let net_resp_fields: &[(&str, Type)] = &[
            ("status", Type::Int),
            ("body", Type::Str),
            (
                "headers",
                Type::List(Box::new(Type::Named("Header".to_string()))),
            ),
        ];
        for (field, ty) in net_resp_fields {
            self.record_field_types
                .insert(format!("HttpResponse.{}", field), ty.clone());
        }
        let net_req_fields: &[(&str, Type)] = &[
            ("method", Type::Str),
            ("path", Type::Str),
            ("body", Type::Str),
            (
                "headers",
                Type::List(Box::new(Type::Named("Header".to_string()))),
            ),
        ];
        for (field, ty) in net_req_fields {
            self.record_field_types
                .insert(format!("HttpRequest.{}", field), ty.clone());
        }
        let header_fields: &[(&str, Type)] = &[("name", Type::Str), ("value", Type::Str)];
        for (field, ty) in header_fields {
            self.record_field_types
                .insert(format!("Header.{}", field), ty.clone());
        }
        let tcp_conn_fields: &[(&str, Type)] =
            &[("id", Type::Str), ("host", Type::Str), ("port", Type::Int)];
        for (field, ty) in tcp_conn_fields {
            self.record_field_types
                .insert(format!("Tcp.Connection.{}", field), ty.clone());
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
        let header_list = || Type::List(Box::new(Type::Named("Header".to_string())));
        let http_handler = || {
            Type::Fn(
                vec![Type::Named("HttpRequest".to_string())],
                Box::new(Type::Named("HttpResponse".to_string())),
                vec![
                    "Console".to_string(),
                    "Http".to_string(),
                    "Disk".to_string(),
                    "Tcp".to_string(),
                    "HttpServer".to_string(),
                ],
            )
        };
        let http_handler_with_context = || {
            Type::Fn(
                vec![Type::Unknown, Type::Named("HttpRequest".to_string())],
                Box::new(Type::Named("HttpResponse".to_string())),
                vec![
                    "Console".to_string(),
                    "Http".to_string(),
                    "Disk".to_string(),
                    "Tcp".to_string(),
                    "HttpServer".to_string(),
                ],
            )
        };
        let service_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Console.print", &[Type::Unknown], Type::Unit, &["Console"]),
            ("Console.error", &[Type::Unknown], Type::Unit, &["Console"]),
            ("Console.warn", &[Type::Unknown], Type::Unit, &["Console"]),
            (
                "Console.readLine",
                &[],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &["Console"],
            ),
            ("Http.get", &[Type::Str], net_ret(), &["Http"]),
            ("Http.head", &[Type::Str], net_ret(), &["Http"]),
            ("Http.delete", &[Type::Str], net_ret(), &["Http"]),
            (
                "Http.post",
                &[Type::Str, Type::Str, Type::Str, header_list()],
                net_ret(),
                &["Http"],
            ),
            (
                "Http.put",
                &[Type::Str, Type::Str, Type::Str, header_list()],
                net_ret(),
                &["Http"],
            ),
            (
                "Http.patch",
                &[Type::Str, Type::Str, Type::Str, header_list()],
                net_ret(),
                &["Http"],
            ),
            (
                "HttpServer.listen",
                &[Type::Int, http_handler()],
                Type::Unit,
                &["HttpServer"],
            ),
            (
                "HttpServer.listenWith",
                &[Type::Int, Type::Unknown, http_handler_with_context()],
                Type::Unit,
                &["HttpServer"],
            ),
            ("Disk.readText", &[Type::Str], disk_str(), &["Disk"]),
            (
                "Disk.writeText",
                &[Type::Str, Type::Str],
                disk_unit(),
                &["Disk"],
            ),
            (
                "Disk.appendText",
                &[Type::Str, Type::Str],
                disk_unit(),
                &["Disk"],
            ),
            ("Disk.exists", &[Type::Str], Type::Bool, &["Disk"]),
            ("Disk.delete", &[Type::Str], disk_unit(), &["Disk"]),
            ("Disk.deleteDir", &[Type::Str], disk_unit(), &["Disk"]),
            ("Disk.listDir", &[Type::Str], disk_list(), &["Disk"]),
            ("Disk.makeDir", &[Type::Str], disk_unit(), &["Disk"]),
            (
                "Tcp.send",
                &[Type::Str, Type::Int, Type::Str],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &["Tcp"],
            ),
            (
                "Tcp.ping",
                &[Type::Str, Type::Int],
                Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
                &["Tcp"],
            ),
            (
                "Tcp.connect",
                &[Type::Str, Type::Int],
                Type::Result(
                    Box::new(Type::Named("Tcp.Connection".to_string())),
                    Box::new(Type::Str),
                ),
                &["Tcp"],
            ),
            (
                "Tcp.writeLine",
                &[Type::Named("Tcp.Connection".to_string()), Type::Str],
                Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
                &["Tcp"],
            ),
            (
                "Tcp.readLine",
                &[Type::Named("Tcp.Connection".to_string())],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &["Tcp"],
            ),
            (
                "Tcp.close",
                &[Type::Named("Tcp.Connection".to_string())],
                Type::Result(Box::new(Type::Unit), Box::new(Type::Str)),
                &["Tcp"],
            ),
        ];
        for (name, params, ret, effects) in service_sigs {
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
        ];
        for (name, params, ret, effects) in float_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // String namespace
        let str_list = || Type::List(Box::new(Type::Str));
        let string_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("String.length", &[Type::Str], Type::Int, &[]),
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
            ("String.chars", &[Type::Str], str_list(), &[]),
            ("String.fromInt", &[Type::Int], Type::Str, &[]),
            ("String.fromFloat", &[Type::Float], Type::Str, &[]),
            ("String.fromBool", &[Type::Bool], Type::Str, &[]),
        ];
        for (name, params, ret, effects) in string_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // List namespace
        let any = || Type::Unknown;
        let list_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("List.len", &[Type::List(Box::new(any()))], Type::Int, &[]),
            (
                "List.map",
                &[Type::Unknown, Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.filter",
                &[Type::Unknown, Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.fold",
                &[Type::Unknown, Type::Unknown, Type::Unknown],
                any(),
                &[],
            ),
            (
                "List.get",
                &[Type::Unknown, Type::Int],
                Type::Result(Box::new(any()), Box::new(Type::Str)),
                &[],
            ),
            (
                "List.push",
                &[Type::Unknown, Type::Unknown],
                Type::List(Box::new(any())),
                &[],
            ),
            (
                "List.head",
                &[Type::Unknown],
                Type::Result(Box::new(any()), Box::new(Type::Str)),
                &[],
            ),
            (
                "List.tail",
                &[Type::Unknown],
                Type::Result(Box::new(any()), Box::new(Type::Str)),
                &[],
            ),
        ];
        for (name, params, ret, effects) in list_sigs {
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
    }
}
