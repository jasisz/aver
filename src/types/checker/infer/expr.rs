use super::*;

fn display_type_for_expected(ty: &Type) -> String {
    match ty {
        Type::Unknown => "Any".to_string(),
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
            "({})",
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
        Type::Named(name) => name.clone(),
    }
}

fn const_int_expr(expr: &Spanned<Expr>) -> Option<i64> {
    match &expr.node {
        Expr::Literal(crate::ast::Literal::Int(i)) => Some(*i),
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
    pub(in super::super) fn infer_type(&mut self, expr: &Spanned<Expr>) -> Type {
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
                } else if let Some(sig) = self.fn_sigs.get(name) {
                    Self::fn_type_from_sig(sig)
                } else {
                    self.error(format!("Unknown identifier '{}'", name));
                    Type::Unknown
                }
            }

            Expr::FnCall(fn_expr, args) => {
                // Use call-site line for errors when available, else fall back to fn header.
                let err_line = if expr.line > 0 {
                    expr.line
                } else {
                    self.current_fn_line.unwrap_or(1)
                };

                // Infer arg types
                let arg_types: Vec<Type> = args.iter().map(|a| self.infer_type(a)).collect();

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
                    } else {
                        for (i, (arg_ty, param_ty)) in
                            arg_types.iter().zip(sig.params.iter()).enumerate()
                        {
                            if !Self::constraint_compatible(arg_ty, param_ty) {
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
                    }
                    sig.ret
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
                    if let Some(sig) = self.fn_sigs.get(name).cloned() {
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
                        return Type::Unknown;
                    }
                    self.error_at_line(err_line, format!("Call to unknown function '{}'", name));
                    return Type::Unknown;
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
                            return Type::Result(Box::new(inner), Box::new(Type::Unknown));
                        }
                        "Result.Err" => {
                            let inner = arg_types.first().cloned().unwrap_or(Type::Unit);
                            return Type::Result(Box::new(Type::Unknown), Box::new(inner));
                        }
                        "Option.Some" => {
                            let inner = arg_types.first().cloned().unwrap_or(Type::Unit);
                            return Type::Option(Box::new(inner));
                        }
                        // Option/Result combinators: propagate inner types
                        "Option.withDefault" => {
                            // (Option<T>, T) -> T
                            if arg_types.len() == 2 {
                                return arg_types[1].clone();
                            }
                        }
                        "Result.withDefault" => {
                            // (Result<T, E>, T) -> T
                            if arg_types.len() == 2 {
                                return arg_types[1].clone();
                            }
                        }
                        "Option.toResult" => {
                            // (Option<T>, E) -> Result<T, E>
                            if arg_types.len() == 2 {
                                let t = match &arg_types[0] {
                                    Type::Option(inner) => *inner.clone(),
                                    _ => Type::Unknown,
                                };
                                let e = arg_types[1].clone();
                                return Type::Result(Box::new(t), Box::new(e));
                            }
                        }
                        _ => {}
                    }
                    if let Some(sig) = self.fn_sigs.get(&display_name).cloned() {
                        let ret = check_call(self, &display_name, sig);
                        validate_special_call(self, &display_name, args);
                        return ret;
                    }
                }

                let callee_ty = self.infer_type(fn_expr);
                if let Some(sig) = Self::sig_from_callable_type(&callee_ty) {
                    return check_call(self, "<fn value>", sig);
                }

                if !matches!(callee_ty, Type::Unknown) {
                    self.error_at_line(
                        err_line,
                        format!("Cannot call value of type {}", callee_ty.display()),
                    );
                }
                Type::Unknown
            }

            Expr::BinOp(op, left, right) => {
                let lt = self.infer_type(left);
                let rt = self.infer_type(right);
                let line = if expr.line > 0 {
                    expr.line
                } else {
                    self.current_fn_line.unwrap_or(1)
                };
                self.check_binop_expr(op, left, right, &lt, &rt, line);
                match op {
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                        Type::Bool
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        // Promote to Float if either side is Float
                        if matches!(lt, Type::Float) || matches!(rt, Type::Float) {
                            Type::Float
                        } else if matches!(lt, Type::Int) && matches!(rt, Type::Int) {
                            Type::Int
                        } else if matches!(lt, Type::Str)
                            && matches!(rt, Type::Str)
                            && matches!(op, BinOp::Add)
                        {
                            Type::Str
                        } else {
                            Type::Unknown
                        }
                    }
                }
            }

            Expr::Constructor(name, arg) => match name.as_str() {
                "Ok" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Result(Box::new(inner), Box::new(Type::Unknown))
                }
                "Err" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Result(Box::new(Type::Unknown), Box::new(inner))
                }
                "Some" => {
                    let inner = arg
                        .as_ref()
                        .map(|a| self.infer_type(a))
                        .unwrap_or(Type::Unit);
                    Type::Option(Box::new(inner))
                }
                "None" => Type::Option(Box::new(Type::Unknown)),
                _ => Type::Unknown,
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
                    Type::Unknown
                };
                Type::List(Box::new(inner))
            }

            Expr::Tuple(items) => {
                let tys = items.iter().map(|item| self.infer_type(item)).collect();
                Type::Tuple(tys)
            }

            Expr::EffectTuple(elements, unwrap) => {
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
                                        if !err_ty.compatible(&fn_err_ty) {
                                            self.error_at_line(prop_line, format!(
                                                "Effect tuple '?!': Err type {} is incompatible with function's Err type {}",
                                                err_ty.display(),
                                                fn_err_ty.display()
                                            ));
                                        }
                                    }
                                    Some(Type::Unknown) => {}
                                    Some(other) => {
                                        self.error_at_line(prop_line, format!(
                                            "Effect tuple '?!' used in function returning {}, which is not Result",
                                            other.display()
                                        ));
                                    }
                                    None => {
                                        self.error_at_line(
                                            prop_line,
                                            "Effect tuple '?!' used outside of a function"
                                                .to_string(),
                                        );
                                    }
                                }
                                ok_types.push(*ok_ty);
                            }
                            Type::Unknown => {
                                ok_types.push(Type::Unknown);
                            }
                            other => {
                                self.error_at_line(
                                    prop_line,
                                    format!(
                                        "Effect tuple '?!' element must be Result, got {}",
                                        other.display()
                                    ),
                                );
                                ok_types.push(Type::Unknown);
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
                let mut key_ty = Type::Unknown;
                let mut val_ty = Type::Unknown;

                for (key_expr, value_expr) in entries {
                    let current_key = self.infer_type(key_expr);
                    let current_val = self.infer_type(value_expr);

                    if !matches!(
                        current_key,
                        Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unknown
                    ) {
                        self.error(format!(
                            "Map literal key type must be Int, Float, String, or Bool (got {})",
                            current_key.display()
                        ));
                    }

                    if matches!(key_ty, Type::Unknown) {
                        key_ty = current_key.clone();
                    } else if !matches!(current_key, Type::Unknown)
                        && !Self::constraint_compatible(&current_key, &key_ty)
                    {
                        self.error(format!(
                            "Map literal contains incompatible key types: {} vs {}",
                            key_ty.display(),
                            current_key.display()
                        ));
                    }

                    if matches!(val_ty, Type::Unknown) {
                        val_ty = current_val.clone();
                    } else if !matches!(current_val, Type::Unknown)
                        && !Self::constraint_compatible(&current_val, &val_ty)
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
                // Infer from first arm; check remaining arms for consistency
                if let Some(first_arm) = arms.first() {
                    let first_ty = self.infer_type_with_pattern_bindings(
                        &first_arm.pattern,
                        &subject_ty,
                        &first_arm.body,
                    );
                    for arm in arms.iter().skip(1) {
                        let arm_ty = self.infer_type_with_pattern_bindings(
                            &arm.pattern,
                            &subject_ty,
                            &arm.body,
                        );
                        // Only report mismatch when both types are concrete
                        if !first_ty.compatible(&arm_ty)
                            && !matches!(first_ty, Type::Unknown)
                            && !matches!(arm_ty, Type::Unknown)
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
                    Type::Unknown
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
                                // Use compatible() (not constraint_compatible) so that
                                // Unknown err types from generic combinators (e.g.
                                // Option.toResult) are accepted without error.
                                if !err_ty.compatible(&fn_err_ty) {
                                    self.error_at_line(prop_line, format!(
                                        "Operator '?': Err type {} is incompatible with function's Err type {}",
                                        err_ty.display(),
                                        fn_err_ty.display()
                                    ));
                                }
                            }
                            Some(Type::Unknown) => {} // gradual typing — skip check
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
                    Type::Unknown => Type::Unknown,
                    other => {
                        self.error_at_line(
                            prop_line,
                            format!(
                                "Operator '?' can only be applied to Result, got {}",
                                other.display()
                            ),
                        );
                        Type::Unknown
                    }
                }
            }

            Expr::Attr(obj, field) => {
                if let Some(mut parts) = Self::attr_path(&obj.node) {
                    let obj_key = parts.join(".");
                    parts.push(field.clone());
                    let key = parts.join(".");
                    if let Some(ty) = self.value_members.get(&key) {
                        return ty.clone();
                    }
                    if let Some(sig) = self.fn_sigs.get(&key) {
                        return Self::fn_type_from_sig(sig);
                    }
                    if self.has_namespace_prefix(&key) {
                        // Intermediate namespace (e.g. Models.User in Models.User.findById)
                        return Type::Unknown;
                    }
                    if self.has_namespace_prefix(&obj_key) {
                        self.error(format!(
                            "Unknown member '{}.{}' (not exposed or missing)",
                            obj_key, field
                        ));
                        return Type::Unknown;
                    }
                }
                let obj_ty = self.infer_type(obj);
                match obj_ty {
                    Type::Named(ref type_name) => {
                        if self.opaque_types.contains(type_name) {
                            self.error(format!(
                                "Cannot access field '{}' of opaque type '{}'",
                                field, type_name
                            ));
                            return Type::Unknown;
                        }
                        let key = format!("{}.{}", type_name, field);
                        if let Some(field_ty) = self.record_field_types.get(&key) {
                            field_ty.clone()
                        } else {
                            let schema_prefix = format!("{}.", type_name);
                            let has_known_schema = self
                                .record_field_types
                                .keys()
                                .any(|k| k.starts_with(&schema_prefix));
                            if has_known_schema {
                                self.error(format!(
                                    "Record '{}' has no field '{}'",
                                    type_name, field
                                ));
                            }
                            Type::Unknown
                        }
                    }
                    Type::Unknown => Type::Unknown,
                    other => {
                        self.error(format!(
                            "Field access on non-record type {}",
                            other.display()
                        ));
                        Type::Unknown
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
                let (target, args) = boxed.as_ref();
                for arg in args {
                    let _ = self.infer_type(arg);
                }
                // Return type is the same as the target function's return type
                if let Some(sig) = self.fn_sigs.get(target).cloned() {
                    sig.ret
                } else {
                    Type::Unknown
                }
            }

            // Resolved nodes are produced after type-checking, so should not appear here.
            // If they do (e.g. in a test), treat as Unknown.
            Expr::Resolved(_) => Type::Unknown,
        }
    }
}
