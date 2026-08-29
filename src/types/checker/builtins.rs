use super::*;

impl TypeChecker {
    pub(super) fn register_builtins(&mut self) {
        // No flat builtins — all functions live in namespaces.

        // Register the server-side HttpRequest record. Client responses now
        // belong to the source-owned Http capability as Http.Response.
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
        let bytes_result = || Type::Result(Box::new(Type::named("Bytes")), Box::new(Type::Str));
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
            (
                "Int.toBigEndian",
                &[Type::Int, Type::Int],
                bytes_result(),
                &[],
            ),
            (
                "Int.toLittleEndian",
                &[Type::Int, Type::Int],
                bytes_result(),
                &[],
            ),
            ("Int.fromBigEndian", &[Type::named("Bytes")], Type::Int, &[]),
            (
                "Int.fromLittleEndian",
                &[Type::named("Bytes")],
                Type::Int,
                &[],
            ),
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
        // count-taking operations register the DYNAMIC type. Literal
        // discharge is operation-specific: shiftLeft/low retain the fixed
        // materialization bound, while shiftRight accepts every non-negative
        // literal because its result cannot grow.
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
            ("String.toUtf8", &[Type::Str], Type::named("Bytes"), &[]),
            (
                "String.fromUtf8",
                &[Type::named("Bytes")],
                Type::Result(Box::new(Type::Str), Box::new(Type::Str)),
                &[],
            ),
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
            (
                "Vector.new",
                &[Type::Int, t_var()],
                Type::Result(Box::new(vec_t()), Box::new(Type::Str)),
                &[],
            ),
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

        // Unicode scalar-value helpers live under their String owner.
        let code_point_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "String.firstCodePoint",
                &[Type::Str],
                Type::Option(Box::new(Type::Int)),
                &[],
            ),
            (
                "String.fromCodePoint",
                &[Type::Int],
                Type::Option(Box::new(Type::Str)),
                &[],
            ),
        ];
        for (name, params, ret, effects) in code_point_sigs {
            self.insert_sig(name, params, ret.clone(), effects);
        }

        // BranchPath — opaque builtin used by Oracle-proof specs for
        // generative-effect oracles (`(BranchPath, Int, args...) -> T`).
        // Three constructors, no other public surface.
        let branch_path_ty = || Type::named(crate::types::branch_path::TYPE_NAME.to_string());
        let branch_path_result = || Type::Result(Box::new(branch_path_ty()), Box::new(Type::Str));
        // `BranchPath.Root` is a nullary value (like `Option.None`) —
        // PascalCase, no parens. `.child` / `.parse` are methods.
        self.value_members
            .insert("BranchPath.Root".to_string(), branch_path_ty());
        let branch_path_sigs: &[(&str, &[Type], Type, &[&str])] = &[
            (
                "BranchPath.child",
                &[branch_path_ty(), Type::Int],
                branch_path_result(),
                &[],
            ),
            ("BranchPath.parse", &[Type::Str], branch_path_result(), &[]),
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
        self.insert_sig(
            "Result.fromOption",
            &[option_t(), e_var()],
            result_te(),
            &[],
        );
    }
}
