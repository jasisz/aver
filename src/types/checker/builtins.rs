use super::*;

impl TypeChecker {
    pub(super) fn register_builtins(&mut self) {
        // No flat builtins — all functions live in namespaces.

        // Register built-in record field types for HttpResponse / HttpRequest.
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
            self.record_field_types
                .insert(RecordFieldKey::new("HttpResponse", *field), ty.clone());
        }
        let net_req_fields: &[(&str, Type)] = &[
            ("method", Type::Str),
            ("path", Type::Str),
            ("query", Type::Str),
            ("body", Type::Str),
            ("headers", header_map()),
        ];
        for (field, ty) in net_req_fields {
            self.record_field_types
                .insert(RecordFieldKey::new("HttpRequest", *field), ty.clone());
        }
        let effect_arg_var = || Type::Var("EffectArg".to_string());
        // `printable_var` retired in 0.16: Console.print/error/warn now
        // take String. Stringification of non-String values is the
        // caller's job (interpolation `"{x}"` or explicit
        // `Int.toString` / `Float.toString` / record-field-by-field
        // formatting). Effect ABI stays trivial across backends.
        let context_var = || Type::Var("Context".to_string());

        // Oracle v1: EffectEvent = { method: String, args: List<EffectArg> }.
        // `args` element type is heterogeneous across effects (Int for
        // Random.int, Str for Console.print<T=Str>, etc.) — v0 types it
        // as a named element type variable so users can inspect events without the
        // checker blocking on a polymorphic arg element type. Richer
        // typing ties args to the effect method's runtime_params once
        // we thread method-dispatch-driven arg types into the checker.
        let effect_event_fields: &[(&str, Type)] = &[
            ("method", Type::Str),
            ("args", Type::List(Box::new(effect_arg_var()))),
            ("path", Type::Str),
        ];
        for (field, ty) in effect_event_fields {
            self.record_field_types.insert(
                RecordFieldKey::new(crate::types::effect_event::TYPE_NAME, *field),
                ty.clone(),
            );
        }

        // Oracle v1: Trace = { events: List<EffectEvent> }.
        let trace_fields: &[(&str, Type)] = &[(
            "events",
            Type::List(Box::new(Type::named(crate::types::effect_event::TYPE_NAME))),
        )];
        for (field, ty) in trace_fields {
            self.record_field_types.insert(
                RecordFieldKey::new(crate::types::trace::TYPE_NAME, *field),
                ty.clone(),
            );
        }
        let net_ret = || Type::Result(Box::new(Type::named("HttpResponse")), Box::new(Type::Str));
        let disk_unit = || Type::Result(Box::new(Type::Unit), Box::new(Type::Str));
        // Http.post/put/patch headers param: same shape as the
        // record fields above (`Map<String, List<String>>`).
        let header_list = header_map;
        let server_handler_effects = || {
            let mut effects = vec![
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
                "Env.get".to_string(),
                "Env.set".to_string(),
                "HttpServer.listen".to_string(),
                "HttpServer.listenWith".to_string(),
            ];
            effects.extend(
                crate::stdlib::standard_capability_registry_ref()
                    .operations()
                    .filter(|operation| operation.is_effectful())
                    .map(|operation| operation.canonical_name.clone()),
            );
            effects
        };
        let http_handler = || {
            Type::Fn(
                vec![Type::named("HttpRequest")],
                Box::new(Type::named("HttpResponse")),
                server_handler_effects(),
            )
        };
        let http_handler_with_context = || {
            Type::Fn(
                vec![context_var(), Type::named("HttpRequest")],
                Box::new(Type::named("HttpResponse")),
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
            // Console.print/error/warn take String. Stringification of
            // non-String values is the caller's job (use interpolation
            // `"{x}"` or explicit `Int.toString` / `Float.toString` /
            // record-field-by-field formatting). Keeps the effect ABI
            // trivial across backends and makes value→string
            // conversion explicit at the call site, which the AI-
            // reading-code-as-letter principle of Aver wants over
            // implicit Show/Display dispatch.
            (
                "Console.print",
                &[Type::Str],
                Type::Unit,
                &["Console.print"],
            ),
            (
                "Console.error",
                &[Type::Str],
                Type::Unit,
                &["Console.error"],
            ),
            ("Console.warn", &[Type::Str], Type::Unit, &["Console.warn"]),
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
                &[Type::Int, context_var(), http_handler_with_context()],
                Type::Unit,
                &["HttpServer.listenWith"],
            ),
            // SelfHostRuntime.* are the self-host bridge calls — the
            // generated Rust passes a `Val` (sumtype carrying the
            // already-evaluated guest fn through `Val::ValFn`), not a
            // typed `Fn(...)`. Accept any handler-position argument
            // here; the runtime code path unwraps `Val::ValFn` and
            // dispatches. `HttpServer.listen` (the user-facing
            // builtin, defined above) keeps the strict `Fn(...)`
            // signature — this opening is scoped to the bridge.
            (
                "SelfHostRuntime.httpServerListen",
                &[Type::Int, Type::Var("Handler".to_string())],
                disk_unit(),
                &["HttpServer.listen"],
            ),
            (
                "SelfHostRuntime.httpServerListenWith",
                &[
                    Type::Int,
                    Type::Var("Ctx".to_string()),
                    Type::Var("Handler".to_string()),
                ],
                disk_unit(),
                &["HttpServer.listenWith"],
            ),
            (
                "Env.get",
                &[Type::Str],
                Type::Option(Box::new(Type::Str)),
                &["Env.get"],
            ),
            ("Env.set", &[Type::Str, Type::Str], Type::Unit, &["Env.set"]),
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
                    &[Type::Str],
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
                    Type::named("Terminal.Size"),
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
                self.record_field_types
                    .insert(RecordFieldKey::new("Terminal.Size", *field), ty.clone());
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
            ("Int.abs", &[Type::Int], Type::Int, &[]),
            ("Int.min", &[Type::Int, Type::Int], Type::Int, &[]),
            ("Int.max", &[Type::Int, Type::Int], Type::Int, &[]),
            // `Int.mod` / `Int.div` register the dynamic-divisor type. When
            // the divisor is a syntactic nonzero integer literal the call
            // types as plain `Int` instead — the literal-divisor discharge
            // rule in `infer/expr.rs` (`is_literal_nonzero_int_divisor`).
            ("Int.mod", &[Type::Int, Type::Int], int_result(), &[]),
            ("Int.div", &[Type::Int, Type::Int], int_result(), &[]),
        ];
        for (name, params, ret, effects) in int_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Bits namespace — a bit-level VIEW of `Int`, not a type. Every
        // parameter and every payload here is an ordinary `Int`; the
        // namespace only fixes how those integers are READ for one call
        // (infinite two's complement).
        //
        // `and` / `or` / `xor` / `not` are total: that reading is defined
        // for every integer, so there is nothing to fail on. The three
        // count-taking operations register the DYNAMIC type; when the count
        // is a syntactic non-negative integer literal they type as plain
        // `Int` instead — the literal-count discharge rule in `infer/expr.rs`
        // (`is_literal_nonneg_int_count`), the same shape as `Int.div`.
        let bits_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Bits.and", &[Type::Int, Type::Int], Type::Int, &[]),
            ("Bits.or", &[Type::Int, Type::Int], Type::Int, &[]),
            ("Bits.xor", &[Type::Int, Type::Int], Type::Int, &[]),
            ("Bits.not", &[Type::Int], Type::Int, &[]),
            ("Bits.shiftLeft", &[Type::Int, Type::Int], int_result(), &[]),
            (
                "Bits.shiftRight",
                &[Type::Int, Type::Int],
                int_result(),
                &[],
            ),
            ("Bits.low", &[Type::Int, Type::Int], int_result(), &[]),
        ];
        for (name, params, ret, effects) in bits_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Float namespace
        let float_result = || Type::Result(Box::new(Type::Float), Box::new(Type::Str));
        let float_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Float.fromString", &[Type::Str], float_result(), &[]),
            ("Float.fromInt", &[Type::Int], Type::Float, &[]),
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

        // List namespace — polymorphic over T (or A, B for List.zip).
        // `Var("T")` is a named type parameter; instantiated at the call
        // site from the expected return type or arg types. Treated as
        // "any" by `compatible()` until Etap 3 wires up bidirectional
        // instantiation.
        let t_var = || Type::Var("T".to_string());
        let list_t = || Type::List(Box::new(t_var()));
        let list_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("List.len", &[list_t()], Type::Int, &[]),
            ("List.prepend", &[t_var(), list_t()], list_t(), &[]),
            ("List.take", &[list_t(), Type::Int], list_t(), &[]),
            ("List.drop", &[list_t(), Type::Int], list_t(), &[]),
            ("List.concat", &[list_t(), list_t()], list_t(), &[]),
            ("List.reverse", &[list_t()], list_t(), &[]),
            ("List.contains", &[list_t(), t_var()], Type::Bool, &[]),
            (
                "List.zip",
                &[
                    Type::List(Box::new(Type::Var("A".to_string()))),
                    Type::List(Box::new(Type::Var("B".to_string()))),
                ],
                Type::List(Box::new(Type::Tuple(vec![
                    Type::Var("A".to_string()),
                    Type::Var("B".to_string()),
                ]))),
                &[],
            ),
        ];
        for (name, params, ret, effects) in list_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Map namespace — polymorphic over K (key) and V (value).
        let k_var = || Type::Var("K".to_string());
        let v_var = || Type::Var("V".to_string());
        let map_kv = || Type::Map(Box::new(k_var()), Box::new(v_var()));
        let map_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Map.set", &[map_kv(), k_var(), v_var()], map_kv(), &[]),
            (
                "Map.get",
                &[map_kv(), k_var()],
                Type::Option(Box::new(v_var())),
                &[],
            ),
            ("Map.remove", &[map_kv(), k_var()], map_kv(), &[]),
            ("Map.has", &[map_kv(), k_var()], Type::Bool, &[]),
            ("Map.keys", &[map_kv()], Type::List(Box::new(k_var())), &[]),
            (
                "Map.values",
                &[map_kv()],
                Type::List(Box::new(v_var())),
                &[],
            ),
            (
                "Map.entries",
                &[map_kv()],
                Type::List(Box::new(Type::Tuple(vec![k_var(), v_var()]))),
                &[],
            ),
            ("Map.len", &[map_kv()], Type::Int, &[]),
            (
                "Map.fromList",
                &[Type::List(Box::new(Type::Tuple(vec![k_var(), v_var()])))],
                map_kv(),
                &[],
            ),
        ];
        for (name, params, ret, effects) in map_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // Vector namespace — polymorphic over T.
        let vec_t = || Type::Vector(Box::new(t_var()));
        let vector_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            ("Vector.new", &[Type::Int, t_var()], vec_t(), &[]),
            (
                "Vector.get",
                &[vec_t(), Type::Int],
                Type::Option(Box::new(t_var())),
                &[],
            ),
            (
                "Vector.set",
                &[vec_t(), Type::Int, t_var()],
                Type::Option(Box::new(vec_t())),
                &[],
            ),
            ("Vector.len", &[vec_t()], Type::Int, &[]),
            (
                "Vector.fromList",
                &[Type::List(Box::new(t_var()))],
                vec_t(),
                &[],
            ),
            (
                "List.fromVector",
                &[vec_t()],
                Type::List(Box::new(t_var())),
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
        let branch_path_ty = || Type::named(crate::types::branch_path::TYPE_NAME.to_string());
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

        // Crypto namespace
        let crypto_sigs: &[(&str, &[Type], Type, &[&str])] = &[(
            "Crypto.sha256",
            &[Type::named("Bytes")],
            Type::named("Digest32"),
            &[],
        )];
        for (name, params, ret, effects) in crypto_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }
        // Result.Ok / Result.Err / Option.Some — constructor signatures.
        // Polymorphic over T (ok/some) and E (err).
        let e_var = || Type::Var("E".to_string());
        let result_te = || Type::Result(Box::new(t_var()), Box::new(e_var()));
        let option_t = || Type::Option(Box::new(t_var()));
        self.insert_sig("Result.Ok", &[t_var()], result_te(), &[]);
        self.insert_sig("Result.Err", &[e_var()], result_te(), &[]);
        self.insert_sig("Option.Some", &[t_var()], option_t(), &[]);
        // Option.None — zero-arg value, not a function
        self.value_members
            .insert("Option.None".to_string(), option_t());

        // Result combinators
        self.insert_sig("Result.withDefault", &[result_te(), t_var()], t_var(), &[]);

        // Option combinators
        self.insert_sig("Option.withDefault", &[option_t(), t_var()], t_var(), &[]);
        self.insert_sig("Option.toResult", &[option_t(), e_var()], result_te(), &[]);
    }
}
