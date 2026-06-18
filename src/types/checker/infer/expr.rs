use super::*;

fn type_var(name: &str) -> Type {
    Type::Var(name.to_string())
}

/// Tailored diagnostic for an unresolved bare `Expr::Ident(name)` whose
/// text matches a known namespace handle (built-in generic carrier,
/// effect service, user-defined sum type). Returns `None` for plain
/// typos so the caller falls back to "Unknown identifier 'X'".
///
/// Aver namespaces aren't first-class values — you can't bind one to a
/// name or pass it as an argument. The hint nudges the user at the
/// shape that actually works for each category (literal / constructor /
/// dotted method call).
fn ident_namespace_diagnostic(
    name: &str,
    type_variants: &std::collections::HashMap<String, Vec<String>>,
) -> Option<String> {
    // Hard-coded categories. Service / generic-carrier names live here
    // (not in `type_variants`) so they wouldn't surface via the
    // user-type branch below. Keep this list close to the registered
    // services + the lowering-known carriers; a stale entry is harmless
    // (only fires on unresolved idents), a missing one falls through
    // to the user-type / generic path.
    const GENERIC_CARRIERS: &[&str] = &["Vector", "List", "Map", "Option", "Result", "Tuple"];
    const SERVICE_NAMESPACES: &[&str] = &[
        "Console",
        "Disk",
        "Http",
        "HttpServer",
        "Tcp",
        "Time",
        "Random",
        "Env",
        "Args",
        "Terminal",
    ];

    if GENERIC_CARRIERS.contains(&name) {
        let hint = match name {
            "Vector" => "Try `Vector.fromList([1, 2, 3])` or `Vector.<method>(...)`.",
            "List" => "Try a list literal `[1, 2, 3]` or `List.<method>(...)`.",
            "Map" => "Try a map literal `{\"a\" => 1, \"b\" => 2}` or `Map.<method>(...)`.",
            "Option" => "Try `Option.Some(value)` or `Option.None`.",
            "Result" => "Try `Result.Ok(value)` or `Result.Err(reason)`.",
            "Tuple" => "Try a tuple literal like `(a, b)`.",
            _ => unreachable!(),
        };
        return Some(format!(
            "`{name}` is a namespace, not a value — it can't be bound to a name or passed by itself. {hint}"
        ));
    }

    if SERVICE_NAMESPACES.contains(&name) {
        return Some(format!(
            "`{name}` is an effect service, not a value — call one of its methods (`{name}.<method>(...)`) instead of using the bare name."
        ));
    }

    if let Some(variants) = type_variants.get(name) {
        let suggestion = if let Some(first) = variants.first() {
            format!(" Try a constructor like `{name}.{first}(...)`.")
        } else {
            String::new()
        };
        return Some(format!(
            "`{name}` is a type, not a value — it can't be used in expression position.{suggestion}"
        ));
    }

    None
}

fn display_type_for_expected(ty: &Type) -> String {
    match ty {
        // Named vars (`Var("K")`, `Var("T")`, ...) display as their name.
        Type::Var(name) => name.clone(),
        // Recovery after an earlier error. It is not a top type.
        Type::Invalid => "Invalid".to_string(),
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(ok, err) => format!(
            "Result<{}, {}>",
            display_type_for_expected(ok),
            display_type_for_expected(err)
        ),
        Type::Option(inner) => format!("Option<{}>", display_type_for_expected(inner)),
        Type::List(inner) => format!("List<{}>", display_type_for_expected(inner)),
        Type::Vector(inner) => format!("Vector<{}>", display_type_for_expected(inner)),
        Type::Tuple(items) => format!(
            "Tuple<{}>",
            items
                .iter()
                .map(display_type_for_expected)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        Type::Map(key, value) => format!(
            "Map<{}, {}>",
            display_type_for_expected(key),
            display_type_for_expected(value)
        ),
        Type::Fn(params, ret, effects) => {
            let ps = params
                .iter()
                .map(display_type_for_expected)
                .collect::<Vec<_>>();
            if effects.is_empty() {
                format!(
                    "Fn({}) -> {}",
                    ps.join(", "),
                    display_type_for_expected(ret)
                )
            } else {
                format!(
                    "Fn({}) -> {} ! [{}]",
                    ps.join(", "),
                    display_type_for_expected(ret),
                    effects.join(", ")
                )
            }
        }
        Type::Named { name, .. } => name.clone(),
    }
}

/// True iff `ty` contains no `Type::Invalid` or `Type::Var(_)` anywhere in
/// its structure. Used to gate expected-type
/// propagation: a formal param like `Map<Var("K"), Var("V")>` must
/// NOT be passed as expected into arg inference, otherwise the
/// recogniser stamps the arg with the bare Var-bearing type and
/// breaks downstream backends. Same for `Type::Invalid` recovery nodes.
pub(in crate::types::checker) fn type_is_fully_concrete(ty: &Type) -> bool {
    match ty {
        Type::Var(_) | Type::Invalid => false,
        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit | Type::Named { .. } => true,
        Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
            type_is_fully_concrete(inner)
        }
        Type::Result(ok, err) => type_is_fully_concrete(ok) && type_is_fully_concrete(err),
        Type::Map(k, v) => type_is_fully_concrete(k) && type_is_fully_concrete(v),
        Type::Tuple(items) => items.iter().all(type_is_fully_concrete),
        Type::Fn(params, ret, _) => {
            params.iter().all(type_is_fully_concrete) && type_is_fully_concrete(ret)
        }
    }
}

/// Recogniser for a bare `Option.None` expression — the one constructor
/// with no payload to fix its `T`. Plain inference stamps it
/// `Option<Var("T")>`, which backends keyed on the stamp (the wasm-gc
/// instantiation registry in particular) cannot resolve to a concrete
/// slot. All three surface shapes the checker accepts are covered:
/// `None`, `Option.None` (attr access) and `Option.None()` (zero-arg
/// call) — the same set `try_infer_with_expected` special-cases.
pub(in crate::types::checker) fn is_bare_none_expr(expr: &Expr) -> bool {
    match expr {
        Expr::Constructor(name, None) => name == "None",
        Expr::Attr(obj, field) => {
            field == "None" && matches!(&obj.node, Expr::Ident(n) if n == "Option")
        }
        Expr::FnCall(callee, args) if args.is_empty() => is_bare_none_expr(&callee.node),
        _ => false,
    }
}

fn const_int_expr(expr: &Spanned<Expr>) -> Option<i64> {
    match &expr.node {
        Expr::Literal(crate::ast::Literal::Int(i)) => Some(*i),
        Expr::Neg(inner) => const_int_expr(inner).and_then(|v| v.checked_neg()),
        Expr::BinOp(op, left, right) => {
            let l = const_int_expr(left)?;
            let r = const_int_expr(right)?;
            match op {
                crate::ast::BinOp::Add => l.checked_add(r),
                crate::ast::BinOp::Sub => l.checked_sub(r),
                crate::ast::BinOp::Mul => l.checked_mul(r),
                crate::ast::BinOp::Div => l.checked_div(r),
                _ => None,
            }
        }
        _ => None,
    }
}

impl TypeChecker {
    /// Infer the type of `expr` and record it on the `Spanned` node so later
    /// passes can read the result without re-running inference. The actual
    /// inference logic lives in `infer_type_inner`; this wrapper exists only
    /// to keep the `set_ty` step in one place.
    pub(in super::super) fn infer_type(&mut self, expr: &Spanned<Expr>) -> Type {
        // Phase B: every stamp goes through `canonicalize_named` so
        // `Type::Named` always carries a `TypeId` whenever the
        // current checker can resolve it. The pre-phase-B matcher
        // re-resolved unresolved sides itself; round-6 caught that
        // route as the path for the entry-fallback leak (an
        // unresolved `Shape` in a dep sig silently bound to the
        // entry's `Shape`). Canonicalising once at the stamp site
        // means the matcher can rely on `id` being the
        // load-bearing signal and reject mixed `(Some, None)`
        // outright.
        let raw = self.infer_type_inner(expr);
        let t = self.canonicalize_named(raw);
        expr.set_ty(t.clone());
        t
    }

    /// Bidirectional companion to `infer_type`. When `expected` is `Some(T)`
    /// and `expr` is a generic constructor (`Option.None`, `Map.empty()`,
    /// empty `[]`, `Result.Ok(v)`, etc.) whose result type would otherwise
    /// contain unresolved named variables, propagates `T` into those positions.
    /// Falls back to plain `infer_type` for anything that doesn't need
    /// the hint.
    pub(in super::super) fn infer_type_with_expected(
        &mut self,
        expr: &Spanned<Expr>,
        expected: Option<&Type>,
    ) -> Type {
        if let Some(exp) = expected
            && let Some(ty) = self.try_infer_with_expected(expr, exp)
        {
            expr.set_ty(ty.clone());
            return ty;
        }
        self.infer_type(expr)
    }

    /// Recogniser for the generic-constructor shapes that need an expected
    /// type to produce a precise stamp. Returns `Some(concrete_type)` when
    /// the shape matches and the expected type aligns; `None` otherwise
    /// (caller falls back to plain `infer_type`).
    fn try_infer_with_expected(&mut self, expr: &Spanned<Expr>, expected: &Type) -> Option<Type> {
        match &expr.node {
            // List literals adopt expected `List<T>`. This matters even for
            // non-empty lists whose elements are generic constructors, e.g.
            // `[Option.None]` under `List<Option<PieceKind>>`.
            Expr::List(items) => match expected {
                Type::List(inner) => {
                    for (idx, item) in items.iter().enumerate() {
                        let item_ty = self.infer_type_with_expected(item, Some(inner));
                        if !self.compatible(&item_ty, inner) {
                            self.error(format!(
                                "List element {}: expected {}, got {}",
                                idx + 1,
                                inner.display(),
                                item_ty.display()
                            ));
                        }
                    }
                    Some(expected.clone())
                }
                _ => None,
            },

            // Tuple literals adopt expected `Tuple<...>` element-wise. This
            // gives generic constructors in tuple slots (for example
            // `(Option.None, n)` under `Tuple<Option<Int>, Int>`) the same
            // bidirectional context list elements already receive.
            // Deliberately NOT `Expr::IndependentProduct`: `(...)!` elements
            // must be function calls, and that shape check lives on the
            // bottom-up path — adopting the expected type here would skip it.
            Expr::Tuple(items) => match expected {
                Type::Tuple(elems) if elems.len() == items.len() => {
                    let mut out = Vec::with_capacity(items.len());
                    for (idx, (item, elem_expected)) in items.iter().zip(elems.iter()).enumerate() {
                        let item_ty = self.infer_type_with_expected(item, Some(elem_expected));
                        if !self.compatible(&item_ty, elem_expected) {
                            self.error(format!(
                                "Tuple element {}: expected {}, got {}",
                                idx + 1,
                                elem_expected.display(),
                                item_ty.display()
                            ));
                        }
                        out.push(item_ty);
                    }
                    Some(Type::Tuple(out))
                }
                _ => None,
            },

            // Empty map literal `{}` adopts expected `Map<K, V>`.
            Expr::MapLiteral(entries) if entries.is_empty() => match expected {
                Type::Map(_, _) => Some(expected.clone()),
                _ => None,
            },

            // Bare `None` constructor.
            Expr::Constructor(name, None) if name == "None" => match expected {
                Type::Option(_) => Some(expected.clone()),
                _ => None,
            },

            // `Some(v)` / `Ok(v)` / `Err(e)` — propagate inner expected.
            Expr::Constructor(name, Some(arg_box)) => match (name.as_str(), expected) {
                ("Some", Type::Option(inner)) => {
                    let inferred = self.infer_type_with_expected(arg_box, Some(inner));
                    Some(Type::Option(Box::new(inferred)))
                }
                ("Ok", Type::Result(ok, err)) => {
                    let inferred = self.infer_type_with_expected(arg_box, Some(ok));
                    Some(Type::Result(Box::new(inferred), err.clone()))
                }
                ("Err", Type::Result(ok, err)) => {
                    let inferred = self.infer_type_with_expected(arg_box, Some(err));
                    Some(Type::Result(ok.clone(), Box::new(inferred)))
                }
                _ => None,
            },

            // `Option.None` as bare attr access — value, not call.
            Expr::Attr(obj, field)
                if field == "None" && matches!(&obj.node, Expr::Ident(n) if n == "Option") =>
            {
                match expected {
                    Type::Option(_) => Some(expected.clone()),
                    _ => None,
                }
            }

            // `Foo.bar(...)` — handle generic-constructor calls.
            Expr::FnCall(callee, args) => {
                let key = Self::callee_key(&callee.node)?;
                match (key.as_str(), args.len(), expected) {
                    // Map.fromList(xs) — expected Map<K, V> gives xs the
                    // concrete List<(K, V)> element type.
                    ("Map.fromList", 1, Type::Map(k, v)) => {
                        let expected_pairs =
                            Type::List(Box::new(Type::Tuple(vec![*k.clone(), *v.clone()])));
                        let list_ty =
                            self.infer_type_with_expected(&args[0], Some(&expected_pairs));
                        if !self.compatible(&list_ty, &expected_pairs) {
                            self.error(format!(
                                "Argument 1 of 'Map.fromList': expected {}, got {}",
                                expected_pairs.display(),
                                list_ty.display()
                            ));
                        }
                        Some(expected.clone())
                    }

                    // Vector.fromList(xs) — expected Vector<T> gives xs
                    // the concrete List<T> element type.
                    ("Vector.fromList", 1, Type::Vector(inner)) => {
                        let expected_list = Type::List(inner.clone());
                        let list_ty = self.infer_type_with_expected(&args[0], Some(&expected_list));
                        if !self.compatible(&list_ty, &expected_list) {
                            self.error(format!(
                                "Argument 1 of 'Vector.fromList': expected {}, got {}",
                                expected_list.display(),
                                list_ty.display()
                            ));
                        }
                        Some(expected.clone())
                    }

                    // Map.set(m, k, v) — expected Map<K, V> gives precise
                    // context to all three args, including nested Map.empty().
                    ("Map.set", 3, Type::Map(k, v)) => {
                        let expected_map = expected.clone();
                        let map_ty = self.infer_type_with_expected(&args[0], Some(&expected_map));
                        let key_ty = self.infer_type_with_expected(&args[1], Some(k));
                        let val_ty = self.infer_type_with_expected(&args[2], Some(v));
                        if !self.compatible(&map_ty, &expected_map) {
                            self.error(format!(
                                "Argument 1 of 'Map.set': expected {}, got {}",
                                expected_map.display(),
                                map_ty.display()
                            ));
                        }
                        if !self.compatible(&key_ty, k) {
                            self.error(format!(
                                "Argument 2 of 'Map.set': expected {}, got {}",
                                k.display(),
                                key_ty.display()
                            ));
                        }
                        if !self.compatible(&val_ty, v) {
                            self.error(format!(
                                "Argument 3 of 'Map.set': expected {}, got {}",
                                v.display(),
                                val_ty.display()
                            ));
                        }
                        Some(expected_map)
                    }

                    // List.concat(a, b) — when an argument is an empty-list
                    // literal `[]`, push the expected List<T> into both args so
                    // the empty list adopts the concrete element type (e.g.
                    // `List.concat([], [])` from cartesian law-case expansion of
                    // `[]`-bearing givens would otherwise leave the result
                    // List<Var>). Only the empty-list case is intercepted; a
                    // genuine element mismatch between non-empty lists falls
                    // through to normal concat inference, which reports a precise
                    // "list element types differ" error.
                    ("List.concat", 2, Type::List(_))
                        if args
                            .iter()
                            .any(|a| matches!(&a.node, Expr::List(elems) if elems.is_empty())) =>
                    {
                        let left_ty = self.infer_type_with_expected(&args[0], Some(expected));
                        let right_ty = self.infer_type_with_expected(&args[1], Some(expected));
                        if self.compatible(&left_ty, expected)
                            && self.compatible(&right_ty, expected)
                        {
                            Some(expected.clone())
                        } else {
                            None
                        }
                    }

                    // Option.None — accidentally written as zero-arg call.
                    ("Option.None", 0, Type::Option(_)) => Some(expected.clone()),

                    // Option.Some(v) — propagate inner T.
                    ("Option.Some", 1, Type::Option(inner)) => {
                        let inferred = self.infer_type_with_expected(&args[0], Some(inner));
                        Some(Type::Option(Box::new(inferred)))
                    }

                    // Result.Ok(v) — propagate ok side.
                    ("Result.Ok", 1, Type::Result(ok, err)) => {
                        let inferred = self.infer_type_with_expected(&args[0], Some(ok));
                        Some(Type::Result(Box::new(inferred), err.clone()))
                    }

                    // Result.Err(e) — propagate err side.
                    ("Result.Err", 1, Type::Result(ok, err)) => {
                        let inferred = self.infer_type_with_expected(&args[0], Some(err));
                        Some(Type::Result(ok.clone(), Box::new(inferred)))
                    }

                    _ => None,
                }
            }

            _ => None,
        }
    }

    fn try_infer_special_call_without_expected(
        &mut self,
        fn_expr: &Spanned<Expr>,
        args: &[Spanned<Expr>],
    ) -> Option<Type> {
        let display_name = Self::callee_key(&fn_expr.node)?;
        match (display_name.as_str(), args.len()) {
            // Infer K/V from the key and value first, then push that concrete
            // Map<K,V> expectation into the receiver. This stamps nested
            // Map.empty() at the source instead of relying on backend recovery.
            ("Map.set", 3) => {
                let key_ty = self.infer_type(&args[1]);
                let val_ty = self.infer_type(&args[2]);
                let key_value_map = Type::Map(Box::new(key_ty.clone()), Box::new(val_ty.clone()));
                let map_ty = self.infer_type_with_expected(&args[0], Some(&key_value_map));
                let (mut k, mut v) = match &map_ty {
                    Type::Map(k, v) => (*k.clone(), *v.clone()),
                    Type::Invalid => (type_var("K"), type_var("V")),
                    other => {
                        self.error(format!(
                            "Argument 1 of 'Map.set': expected Map<...>, got {}",
                            other.display()
                        ));
                        (type_var("K"), type_var("V"))
                    }
                };
                if matches!(k, Type::Var(_)) {
                    k = key_ty.clone();
                } else if !self.compatible(&key_ty, &k) {
                    self.error(format!(
                        "Argument 2 of 'Map.set': expected {}, got {}",
                        k.display(),
                        key_ty.display()
                    ));
                }
                if matches!(v, Type::Var(_)) {
                    v = val_ty.clone();
                } else if !self.compatible(&val_ty, &v) {
                    self.error(format!(
                        "Argument 3 of 'Map.set': expected {}, got {}",
                        v.display(),
                        val_ty.display()
                    ));
                }
                let expected_map = Type::Map(Box::new(k), Box::new(v));
                self.infer_type_with_expected(&args[0], Some(&expected_map));
                Some(expected_map)
            }
            ("List.prepend", 2) => {
                // Infer the head first so we know the element type, then
                // walk the tail with `List<head_ty>` as the expected
                // type. Crucial for `List.prepend(x, [])` where `[]`'s
                // own inference would stamp `List<T>` (T unbound) and
                // OnceLock then refuses any later overwrite — the tail
                // would stay `List<T>` even if we computed the right
                // shape afterwards.
                let val_ty = self.infer_type(&args[0]);
                let expected_list = Type::List(Box::new(val_ty.clone()));
                let list_ty = self.infer_type_with_expected(&args[1], Some(&expected_list));
                let elem_ty = match &list_ty {
                    Type::List(inner) => *inner.clone(),
                    Type::Invalid => val_ty.clone(),
                    other => {
                        self.error(format!(
                            "Argument 2 of 'List.prepend': expected List<...>, got {}",
                            other.display()
                        ));
                        val_ty.clone()
                    }
                };
                if !matches!(elem_ty, Type::Var(_)) && !self.compatible(&val_ty, &elem_ty) {
                    self.error(format!(
                        "Argument 1 of 'List.prepend': expected {}, got {}",
                        elem_ty.display(),
                        val_ty.display()
                    ));
                }
                Some(Type::List(Box::new(elem_ty)))
            }
            _ => None,
        }
    }

    fn infer_type_inner(&mut self, expr: &Spanned<Expr>) -> Type {
        match &expr.node {
            Expr::Literal(lit) => match lit {
                crate::ast::Literal::Int(_) => Type::Int,
                crate::ast::Literal::Float(_) => Type::Float,
                crate::ast::Literal::Str(_) => Type::Str,
                crate::ast::Literal::Bool(_) => Type::Bool,
                crate::ast::Literal::Unit => Type::Unit,
            },

            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let crate::ast::StrPart::Parsed(expr) = part {
                        self.infer_type(expr);
                    }
                }
                Type::Str
            }

            Expr::Ident(name) => {
                self.used_names.insert(name.clone());
                if let Some(ty) = self.locals.get(name) {
                    ty.clone()
                } else if let Some(sig) = self.find_fn_sig(name) {
                    Self::fn_type_from_sig(sig)
                } else {
                    // Tailored diagnostic when the bare ident is actually a
                    // namespace handle (built-in carrier, service, or user
                    // sum type) instead of a value. AFL byte-havoc + real
                    // beginner intuition both produce `let x = Vector` or
                    // `match Foo { ... }` shapes; the generic "Unknown
                    // identifier" left users guessing and tripped wasm-gc
                    // codegen via `aver_type_of` on a stamp-less node.
                    let msg = ident_namespace_diagnostic(name, &self.type_variants)
                        .unwrap_or_else(|| format!("Unknown identifier '{}'", name));
                    self.error(msg);
                    Type::Invalid
                }
            }

            Expr::FnCall(fn_expr, args) => {
                if let Some(ty) = self.try_infer_special_call_without_expected(fn_expr, args) {
                    return ty;
                }
                // Use call-site line for errors when available, else fall back to fn header.
                let err_line = if expr.line > 0 {
                    expr.line
                } else {
                    self.current_fn_line.unwrap_or(1)
                };

                // Bidirectional: if we can resolve the callee's signature
                // up front, infer each arg WITH its formal param type as
                // expected. Lets generic constructors in arg position
                // (`genRooms(seed, 6, 0, [])` — last arg is List<Room>)
                // pick up T from the signature instead of stamping
                // List<T>. Falls back to standalone inference when
                // the callee is unresolvable (eg. dotted-builtin without
                // a fn_sig, or callable values).
                let formal_params: Option<Vec<Type>> = match &fn_expr.node {
                    Expr::Ident(name) => self.find_fn_sig(name).map(|s| s.params.clone()),
                    _ => Self::callee_key(&fn_expr.node)
                        .and_then(|key| self.find_fn_sig(&key).map(|s| s.params.clone())),
                };
                let arg_types: Vec<Type> = args
                    .iter()
                    .enumerate()
                    .map(|(i, a)| {
                        let expected = formal_params
                            .as_ref()
                            .and_then(|p| p.get(i))
                            .filter(|t| type_is_fully_concrete(t));
                        self.infer_type_with_expected(a, expected)
                    })
                    .collect();

                // Helper: check arity + arg types against a sig, return sig.ret
                let check_call = |tc: &mut Self, display_name: &str, sig: FnSig| -> Type {
                    if arg_types.len() != sig.params.len() {
                        tc.error_at_line(
                            err_line,
                            format!(
                                "Function '{}' expects {} argument(s), got {}",
                                display_name,
                                sig.params.len(),
                                arg_types.len()
                            ),
                        );
                        Type::Invalid
                    } else {
                        let mut subst = HashMap::new();
                        for (i, (arg_ty, param_ty)) in
                            arg_types.iter().zip(sig.params.iter()).enumerate()
                        {
                            if !tc.match_with(arg_ty, param_ty, &mut subst) {
                                tc.error_at_line(
                                    err_line,
                                    format!(
                                        "Argument {} of '{}': expected {}, got {}",
                                        i + 1,
                                        display_name,
                                        display_type_for_expected(param_ty),
                                        arg_ty.display()
                                    ),
                                );
                            }
                        }
                        Self::instantiate_type(&sig.ret, &subst)
                    }
                };
                let validate_special_call =
                    |tc: &mut Self, display_name: &str, call_args: &[Spanned<Expr>]| {
                        if display_name == "Time.sleep"
                            && call_args.len() == 1
                            && let Some(ms) = const_int_expr(&call_args[0])
                            && ms < 0
                        {
                            tc.error_at_line(
                                err_line,
                                "Argument 1 of 'Time.sleep' must be a non-negative Int constant"
                                    .to_string(),
                            );
                        }
                    };

                if let Expr::Ident(name) = &fn_expr.node {
                    if let Some(sig) = self.find_fn_sig(name).cloned() {
                        let ret = check_call(self, name, sig);
                        validate_special_call(self, name, args);
                        return ret;
                    }
                    if let Some(binding_ty) = self.binding_type(name) {
                        if let Some(sig) = Self::sig_from_callable_type(&binding_ty) {
                            return check_call(self, name, sig);
                        }
                        self.error_at_line(
                            err_line,
                            format!(
                                "Cannot call '{}': expected function, got {}",
                                name,
                                binding_ty.display()
                            ),
                        );
                        return Type::Invalid;
                    }
                    self.error_at_line(err_line, format!("Call to unknown function '{}'", name));
                    return Type::Invalid;
                }

                if let Some(display_name) = Self::callee_key(&fn_expr.node) {
                    if let Some(ty) = self.infer_list_call_type(&display_name, &arg_types) {
                        return ty;
                    }
                    if let Some(ty) = self.infer_map_call_type(&display_name, &arg_types) {
                        return ty;
                    }
                    if let Some(ty) = self.infer_vector_call_type(&display_name, &arg_types) {
                        return ty;
                    }

                    // Special-case Result.Ok/Err and Option.Some for precise type inference
                    match display_name.as_str() {
                        "Result.Ok" => {
                            let inner = arg_types.first().cloned().unwrap_or(Type::Unit);
                            return Type::Result(Box::new(inner), Box::new(type_var("E")));
                        }
                        "Result.Err" => {
                            let inner = arg_types.first().cloned().unwrap_or(Type::Unit);
                            return Type::Result(Box::new(type_var("T")), Box::new(inner));
                        }
                        "Option.Some" => {
                            let inner = arg_types.first().cloned().unwrap_or(Type::Unit);
                            return Type::Option(Box::new(inner));
                        }
                        // Option/Result combinators: propagate inner types
                        "Option.withDefault"
                            // (Option<T>, T) -> T
                            if arg_types.len() == 2 => {
                                if !matches!(&arg_types[0], Type::Option(_) | Type::Invalid) {
                                    self.error_at_line(
                                        err_line,
                                        format!(
                                            "Argument 1 of 'Option.withDefault': expected Option<T>, got {}",
                                            arg_types[0].display()
                                        ),
                                    );
                                }
                                return arg_types[1].clone();
                            }
                        "Result.withDefault"
                            // (Result<T, E>, T) -> T
                            if arg_types.len() == 2 => {
                                if !matches!(&arg_types[0], Type::Result(_, _) | Type::Invalid) {
                                    self.error_at_line(
                                        err_line,
                                        format!(
                                            "Argument 1 of 'Result.withDefault': expected Result<T, E>, got {}",
                                            arg_types[0].display()
                                        ),
                                    );
                                }
                                return arg_types[1].clone();
                            }
                        "Option.toResult"
                            // (Option<T>, E) -> Result<T, E>
                            if arg_types.len() == 2 => {
                                if !matches!(&arg_types[0], Type::Option(_) | Type::Invalid) {
                                    self.error_at_line(
                                        err_line,
                                        format!(
                                            "Argument 1 of 'Option.toResult': expected Option<T>, got {}",
                                            arg_types[0].display()
                                        ),
                                    );
                                }
                                let t = match &arg_types[0] {
                                    Type::Option(inner) => *inner.clone(),
                                    _ => type_var("T"),
                                };
                                let e = arg_types[1].clone();
                                return Type::Result(Box::new(t), Box::new(e));
                            }
                        _ => {}
                    }
                    if let Some(sig) = self.find_fn_sig(&display_name).cloned() {
                        let ret = check_call(self, &display_name, sig);
                        validate_special_call(self, &display_name, args);
                        // Cross-arg validation for `listenWith` family: the
                        // context arg (#2) must match the handler's first
                        // parameter type. Builtin sigs use `Type::Invalid`
                        // for context to allow any user-defined type, so
                        // this linkage is enforced here rather than via
                        // standard arg-type matching.
                        if matches!(
                            display_name.as_str(),
                            "HttpServer.listenWith" | "SelfHostRuntime.httpServerListenWith"
                        ) && arg_types.len() == 3
                            && let Type::Fn(handler_params, _, _) = &arg_types[2]
                            && let Some(expected_ctx) = handler_params.first()
                        {
                            let actual_ctx = &arg_types[1];
                            if !matches!(expected_ctx, Type::Invalid)
                                && !matches!(actual_ctx, Type::Invalid)
                                && !self.compatible(actual_ctx, expected_ctx)
                            {
                                self.error_at_line(
                                    err_line,
                                    format!(
                                        "Argument 2 of '{}': context type {} must match handler's first parameter type {}",
                                        display_name,
                                        actual_ctx.display(),
                                        expected_ctx.display(),
                                    ),
                                );
                            }
                        }
                        return ret;
                    }
                }

                let callee_ty = self.infer_type(fn_expr);
                if let Some(sig) = Self::sig_from_callable_type(&callee_ty) {
                    return check_call(self, "<fn value>", sig);
                }

                if !matches!(callee_ty, Type::Invalid) {
                    self.error_at_line(
                        err_line,
                        format!("Cannot call value of type {}", callee_ty.display()),
                    );
                }
                Type::Invalid
            }

            Expr::BinOp(op, left, right) => {
                // Bidirectional equality: a bare `Option.None` on one side
                // of `==` / `!=` has no payload to fix its `T`, so plain
                // inference stamps it `Option<T>` — and the stamp is
                // set-once, so the imprecision is permanent. The wasm-gc
                // backend then fails to resolve the `Option<T>`
                // instantiation slot (the verify runner synthesizes exactly
                // this shape: `__verify_X_check() -> Bool` with body
                // `f(args) == Option.None`). Infer the concrete side first
                // and propagate its type into the bare-None side; when both
                // sides are bare (or the concrete side isn't fully
                // concrete) fall back to the plain left-then-right order.
                let bare_none_eq = matches!(op, BinOp::Eq | BinOp::Neq);
                let l_bare = bare_none_eq && is_bare_none_expr(&left.node);
                let r_bare = bare_none_eq && is_bare_none_expr(&right.node);
                let (lt, rt) = if l_bare && !r_bare {
                    let rt = self.infer_type(right);
                    let lt = if type_is_fully_concrete(&rt) {
                        self.infer_type_with_expected(left, Some(&rt))
                    } else {
                        self.infer_type(left)
                    };
                    (lt, rt)
                } else {
                    let lt = self.infer_type(left);
                    let rt = if r_bare && !l_bare && type_is_fully_concrete(&lt) {
                        self.infer_type_with_expected(right, Some(&lt))
                    } else {
                        self.infer_type(right)
                    };
                    (lt, rt)
                };
                let line = if expr.line > 0 {
                    expr.line
                } else {
                    self.current_fn_line.unwrap_or(1)
                };
                self.check_binop(op, &lt, &rt, line);
                match op {
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                        Type::Bool
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        // Integer `/` is banned (see `check_binop`): it is
                        // partial. `check_binop` already emitted the error;
                        // surface `Invalid` so the value doesn't pose as a
                        // total `Int` downstream.
                        if matches!(op, BinOp::Div)
                            && matches!(lt, Type::Int)
                            && matches!(rt, Type::Int)
                        {
                            Type::Invalid
                        }
                        // Promote to Float if either side is Float
                        else if matches!(lt, Type::Float) || matches!(rt, Type::Float) {
                            Type::Float
                        } else if matches!(lt, Type::Int) && matches!(rt, Type::Int) {
                            Type::Int
                        } else if matches!(lt, Type::Str)
                            && matches!(rt, Type::Str)
                            && matches!(op, BinOp::Add)
                        {
                            Type::Str
                        } else {
                            Type::Invalid
                        }
                    }
                }
            }

            Expr::Neg(operand) => {
                let inner = self.infer_type(operand);
                match inner {
                    Type::Int => Type::Int,
                    Type::Float => Type::Float,
                    Type::Invalid => Type::Invalid,
                    Type::Var(_) => inner,
                    other => {
                        let line = if expr.line > 0 {
                            expr.line
                        } else {
                            self.current_fn_line.unwrap_or(1)
                        };
                        self.error_at_line(
                            line,
                            format!(
                                "Unary '-' expects Int or Float operand, got {}",
                                other.display()
                            ),
                        );
                        Type::Invalid
                    }
                }
            }

            Expr::Constructor(name, arg) => match name.as_str() {
                "Ok" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Result(Box::new(inner), Box::new(type_var("E")))
                }
                "Err" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Result(Box::new(type_var("T")), Box::new(inner))
                }
                "Some" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Option(Box::new(inner))
                }
                "None" => Type::Option(Box::new(type_var("T"))),
                _ => Type::Invalid,
            },

            Expr::List(elems) => {
                let inner = if let Some(first) = elems.first() {
                    let ty = self.infer_type(first);
                    // Infer remaining elements for side effects (used_names tracking).
                    for elem in &elems[1..] {
                        self.infer_type(elem);
                    }
                    ty
                } else {
                    type_var("T")
                };
                Type::List(Box::new(inner))
            }

            Expr::Tuple(items) => {
                let tys = items.iter().map(|item| self.infer_type(item)).collect();
                Type::Tuple(tys)
            }

            Expr::IndependentProduct(elements, unwrap) => {
                // Validate: each element must be a function call
                for elem in elements {
                    match &elem.node {
                        Expr::FnCall(_, _) => {}
                        Expr::ErrorProp(inner) => match &inner.node {
                            Expr::FnCall(_, _) => {}
                            _ => {
                                self.error_at_line(
                                    elem.line,
                                    "Independent product element must be a function call, e.g. (fetchA(), fetchB())?!"
                                        .to_string(),
                                );
                            }
                        },
                        _ => {
                            self.error_at_line(
                                elem.line,
                                "Independent product element must be a function call, e.g. (fetchA(), fetchB())?!"
                                    .to_string(),
                            );
                        }
                    }
                }
                if *unwrap {
                    // ?! variant: each element must be Result<T, E>, unwrap Ok types,
                    // validate Err types against the function's return error type.
                    let prop_line = if expr.line > 0 {
                        expr.line
                    } else {
                        self.current_fn_line.unwrap_or(1)
                    };
                    let mut ok_types = Vec::with_capacity(elements.len());
                    for elem in elements {
                        let ty = self.infer_type(elem);
                        match ty {
                            Type::Result(ok_ty, err_ty) => {
                                match self.current_fn_ret.clone() {
                                    Some(Type::Result(_, fn_err_ty)) => {
                                        let mut subst = HashMap::new();
                                        if !self.match_with(&err_ty, &fn_err_ty, &mut subst) {
                                            self.error_at_line(prop_line, format!(
                                                "Independent product '?!': Err type {} is incompatible with function's Err type {}",
                                                err_ty.display(),
                                                fn_err_ty.display()
                                            ));
                                        }
                                    }
                                    Some(Type::Invalid) => {}
                                    Some(other) => {
                                        self.error_at_line(prop_line, format!(
                                            "Independent product '?!' used in function returning {}, which is not Result",
                                            other.display()
                                        ));
                                    }
                                    None => {
                                        self.error_at_line(
                                            prop_line,
                                            "Independent product '?!' used outside of a function"
                                                .to_string(),
                                        );
                                    }
                                }
                                ok_types.push(*ok_ty);
                            }
                            Type::Invalid => {
                                ok_types.push(Type::Invalid);
                            }
                            other => {
                                self.error_at_line(
                                    prop_line,
                                    format!(
                                        "Independent product '?!' element must be Result, got {}",
                                        other.display()
                                    ),
                                );
                                ok_types.push(Type::Invalid);
                            }
                        }
                    }
                    Type::Tuple(ok_types)
                } else {
                    // bare ! variant: same as regular Tuple
                    let tys = elements.iter().map(|elem| self.infer_type(elem)).collect();
                    Type::Tuple(tys)
                }
            }

            Expr::MapLiteral(entries) => {
                let mut key_ty = type_var("K");
                let mut val_ty = type_var("V");

                for (key_expr, value_expr) in entries {
                    let current_key = self.infer_type(key_expr);
                    let current_val = self.infer_type(value_expr);

                    if matches!(current_key, Type::Fn { .. } | Type::Unit) {
                        self.error(format!(
                            "Map literal key type must be hashable (got {})",
                            current_key.display()
                        ));
                    }

                    if matches!(key_ty, Type::Var(_)) {
                        key_ty = current_key.clone();
                    } else if !matches!(current_key, Type::Invalid)
                        && !self.compatible(&current_key, &key_ty)
                    {
                        self.error(format!(
                            "Map literal contains incompatible key types: {} vs {}",
                            key_ty.display(),
                            current_key.display()
                        ));
                    }

                    if matches!(val_ty, Type::Var(_)) {
                        val_ty = current_val.clone();
                    } else if !matches!(current_val, Type::Invalid)
                        && !self.compatible(&current_val, &val_ty)
                    {
                        self.error(format!(
                            "Map literal contains incompatible value types: {} vs {}",
                            val_ty.display(),
                            current_val.display()
                        ));
                    }
                }

                Type::Map(Box::new(key_ty), Box::new(val_ty))
            }

            Expr::Match { subject, arms } => {
                let match_line = if expr.line > 0 {
                    expr.line
                } else {
                    self.current_fn_line.unwrap_or(1)
                };
                let subject_ty = self.infer_type(subject);
                self.check_match_exhaustiveness(&subject_ty, arms, match_line);
                self.check_match_redundancy(arms);
                // Bidirectional: propagate the enclosing fn's declared
                // return type into each arm body. Lets generic constructors
                // in arm positions (`[] -> Option.None`) pick up T without
                // requiring per-arm annotation.
                let arm_expected = self.current_fn_ret.clone();
                let arm_expected_ref = arm_expected.as_ref();
                // Infer from first arm; check remaining arms for consistency
                if let Some(first_arm) = arms.first() {
                    let first_ty = self.infer_type_with_pattern_bindings_expected(
                        &first_arm.pattern,
                        &subject_ty,
                        &first_arm.body,
                        arm_expected_ref,
                    );
                    for arm in arms.iter().skip(1) {
                        let arm_ty = self.infer_type_with_pattern_bindings_expected(
                            &arm.pattern,
                            &subject_ty,
                            &arm.body,
                            arm_expected_ref,
                        );
                        // Only report mismatch when both types are concrete.
                        // Phase B: route through `self.compatible` so the
                        // symbol table resolves either side's bare ↔
                        // canonical reference instead of the raw
                        // `Type::compatible`'s suffix fallback.
                        if !self.compatible(&first_ty, &arm_ty)
                            && !matches!(first_ty, Type::Invalid)
                            && !matches!(arm_ty, Type::Invalid)
                        {
                            self.error(format!(
                                "Match arms return incompatible types: {} vs {}",
                                first_ty.display(),
                                arm_ty.display()
                            ));
                        }
                    }
                    first_ty
                } else {
                    Type::Invalid
                }
            }

            Expr::ErrorProp(inner) => {
                let prop_line = if expr.line > 0 {
                    expr.line
                } else {
                    self.current_fn_line.unwrap_or(1)
                };
                // expr? unwraps Result<T,E> → T, propagating E as early return.
                let ty = self.infer_type(inner);
                match ty {
                    Type::Result(ok_ty, err_ty) => {
                        match self.current_fn_ret.clone() {
                            Some(Type::Result(_, fn_err_ty)) => {
                                let mut subst = HashMap::new();
                                let err_matches = matches!(err_ty.as_ref(), Type::Var(_))
                                    || self.match_with(&err_ty, &fn_err_ty, &mut subst);
                                if !err_matches {
                                    self.error_at_line(prop_line, format!(
                                        "Operator '?': Err type {} is incompatible with function's Err type {}",
                                        err_ty.display(),
                                        fn_err_ty.display()
                                    ));
                                }
                            }
                            Some(Type::Invalid) => {}
                            Some(other) => {
                                self.error_at_line(prop_line, format!(
                                    "Operator '?' used in function returning {}, which is not Result",
                                    other.display()
                                ));
                            }
                            None => {
                                self.error_at_line(
                                    prop_line,
                                    "Operator '?' used outside of a function".to_string(),
                                );
                            }
                        }
                        *ok_ty
                    }
                    Type::Invalid => Type::Invalid,
                    other => {
                        self.error_at_line(
                            prop_line,
                            format!(
                                "Operator '?' can only be applied to Result, got {}",
                                other.display()
                            ),
                        );
                        Type::Invalid
                    }
                }
            }

            Expr::Attr(obj, field) => {
                if let Some(mut parts) = Self::attr_path(&obj.node) {
                    let obj_key = parts.join(".");
                    parts.push(field.clone());
                    let key = parts.join(".");
                    // The lookups below resolve the *whole* `Vector.set`
                    // path without ever recursing into `obj` — so for
                    // namespace shapes the inner `Spanned<Expr>` (the
                    // `Ident("Vector")`) never gets `set_ty` called and
                    // codegen later panics in `aver_type_of` walking into
                    // a stamp-less node. Iron 0.21 fuzz_codegen_wasm_gc
                    // surfaced exactly this shape three times:
                    // `Vector.set`, `Option.Some`, `List.prepend` as bare
                    // expressions. Stamp the inner ident with
                    // `Type::Invalid` *only* when the lookup recognised
                    // it as namespace-shaped; doing it unconditionally
                    // poisons normal `record.field` accesses where `obj`
                    // is a local that the value-member branch wouldn't
                    // have stamped before either (but where downstream
                    // codegen needs the real record type, not `Invalid`).
                    if let Some(ty) = self.find_value_member(&key) {
                        return ty.clone();
                    }
                    if let Some(sig) = self.find_fn_sig(&key) {
                        obj.set_ty(Type::Invalid);
                        return Self::fn_type_from_sig(sig);
                    }
                    if self.has_namespace_prefix(&key) {
                        // Intermediate namespace (e.g. Models.User in Models.User.findById)
                        obj.set_ty(Type::Invalid);
                        return Type::Invalid;
                    }
                    if self.has_namespace_prefix(&obj_key) {
                        self.error(format!(
                            "Unknown member '{}.{}' (not exposed or missing)",
                            obj_key, field
                        ));
                        obj.set_ty(Type::Invalid);
                        return Type::Invalid;
                    }
                }
                // Oracle v1: `.result` / `.trace` projections on a
                // function-call expression. Only meaningful inside
                // `verify <fn> trace` cases — outside that context
                // they have no runtime support and would crash with
                // a namespace lookup error. Gate strictly.
                if matches!(&obj.node, Expr::FnCall(_, _))
                    && (field == "result" || field == "trace")
                {
                    if !self.in_verify_trace_context {
                        self.error(format!(
                            "`.{}` projection is only available inside a \
                             `verify <fn> trace` case body. Drop it, or move \
                             the expression into a verify-trace block.",
                            field
                        ));
                        return Type::Invalid;
                    }
                    if field == "result" {
                        return self.infer_type(obj);
                    }
                    return Type::named("Trace");
                }
                let obj_ty = self.infer_type(obj);
                // Oracle v1: `.contains` / `.length` / `.event` accessors
                // on a Trace value. Return types match the plan's API.
                if matches!(&obj_ty, Type::Named { name: n, .. } if n == "Trace") {
                    match field.as_str() {
                        "length" => {
                            return Type::Fn(vec![], Box::new(Type::Int), vec![]);
                        }
                        "contains" => {
                            return Type::Fn(
                                vec![type_var("TraceNeedle")],
                                Box::new(Type::Bool),
                                vec![],
                            );
                        }
                        // 0.13 Limit nail #3: `.count(M)` returns the number
                        // of events whose method matches `M`. Same argument
                        // shape as `.contains` (effect-method reference or
                        // call literal); difference is `Bool` vs `Int`. Lets
                        // users write quantitative trace laws like
                        // `result.trace.count(Http.get) == 1`.
                        "count" => {
                            return Type::Fn(
                                vec![type_var("TraceNeedle")],
                                Box::new(Type::Int),
                                vec![],
                            );
                        }
                        "event" => {
                            return Type::Fn(
                                vec![Type::Int],
                                Box::new(Type::Option(Box::new(Type::named("EffectEvent")))),
                                vec![],
                            );
                        }
                        // Oracle v1: `.group(N)` returns a sub-trace
                        // containing only events emitted inside the
                        // N-th `!`/`?!` group in source order. Subsequent
                        // `.length()` / `.event(k)` / `.contains(_)`
                        // operate on that filtered buffer.
                        "group" => {
                            return Type::Fn(
                                vec![Type::Int],
                                Box::new(Type::named("Trace")),
                                vec![],
                            );
                        }
                        // Oracle v1: `.branch(idx)` narrows a group-
                        // scoped trace further to a single branch of
                        // that `!`/`?!` group. Only meaningful after
                        // `.group(N)`; the projection runner rejects
                        // a bare `.trace.branch(idx)` call.
                        "branch" => {
                            return Type::Fn(
                                vec![Type::Int],
                                Box::new(Type::named("Trace")),
                                vec![],
                            );
                        }
                        _ => {}
                    }
                }
                match obj_ty {
                    Type::Named {
                        name: ref type_name,
                        ..
                    } => {
                        // Phase B: `opaque_types` keys are canonical
                        // "Module.Type"; resolve the bare reference
                        // through the symbol table before checking.
                        let canon = self.canonical_type_name(type_name);
                        if !self.self_host_mode && self.opaque_types.contains(&canon) {
                            self.error(format!(
                                "Cannot access field '{}' of opaque type '{}'",
                                field, type_name
                            ));
                            return Type::Invalid;
                        }
                        if let Some(field_ty) = self.find_record_field_type(type_name, field) {
                            field_ty.clone()
                        } else if self.has_record_schema(type_name) {
                            self.error(format!("Record '{}' has no field '{}'", type_name, field));
                            Type::Invalid
                        } else {
                            Type::Invalid
                        }
                    }
                    Type::Invalid => Type::Invalid,
                    other => {
                        self.error(format!(
                            "Field access on non-record type {}",
                            other.display()
                        ));
                        Type::Invalid
                    }
                }
            }

            Expr::RecordCreate { type_name, fields } => {
                self.infer_record_create_expr(type_name, fields)
            }

            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => self.infer_record_update_expr(type_name, base, updates),

            Expr::TailCall(boxed) => {
                let TailCallData { target, args, .. } = boxed.as_ref();
                // Bidirectional: propagate the target fn's formal params into
                // arg inference so generic constructors in tail-call position
                // (`parseListItems(s, pos, [])`, `loop(acc, [])`) pick up T
                // from the signature instead of stamping `List<T>` /
                // `Option<T>`. Mirrors the `Expr::FnCall` handler — TailCall
                // is the same call, just rewritten by tail-check.
                let formal_params: Option<Vec<Type>> =
                    self.find_fn_sig(target).map(|s| s.params.clone());
                for (i, arg) in args.iter().enumerate() {
                    let expected = formal_params
                        .as_ref()
                        .and_then(|p| p.get(i))
                        .filter(|t| type_is_fully_concrete(t));
                    let _ = self.infer_type_with_expected(arg, expected);
                }
                // Return type is the same as the target function's return type
                if let Some(sig) = self.find_fn_sig(target).cloned() {
                    sig.ret
                } else {
                    Type::Invalid
                }
            }

            // Resolved nodes are produced after type-checking, so should not appear here.
            // If they do (e.g. in a test), treat as Unknown.
            Expr::Resolved { .. } => Type::Invalid,
        }
    }
}
