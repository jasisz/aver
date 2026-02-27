use super::*;

impl TypeChecker {
    pub(super) fn infer_list_call_type(&mut self, name: &str, arg_types: &[Type]) -> Option<Type> {
        let list_option = |inner: Type| Type::Option(Box::new(inner));

        let list_inner = |tc: &mut Self, arg_ty: &Type, arg_idx: usize| -> Type {
            match arg_ty {
                Type::List(inner) => *inner.clone(),
                Type::Unknown => Type::Unknown,
                other => {
                    tc.error(format!(
                        "Argument {} of '{}': expected List<...>, got {}",
                        arg_idx,
                        name,
                        other.display()
                    ));
                    Type::Unknown
                }
            }
        };

        let expect_arity = |tc: &mut Self, expected: usize, fallback: Type| -> Result<(), Type> {
            if arg_types.len() != expected {
                tc.error(format!(
                    "Function '{}' expects {} argument(s), got {}",
                    name,
                    expected,
                    arg_types.len()
                ));
                Err(fallback)
            } else {
                Ok(())
            }
        };

        match name {
            "List.len" => {
                if let Err(fallback) = expect_arity(self, 1, Type::Int) {
                    return Some(fallback);
                }
                let _ = list_inner(self, &arg_types[0], 1);
                Some(Type::Int)
            }
            "List.get" => {
                if let Err(fallback) = expect_arity(self, 2, list_option(Type::Unknown)) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                if !Self::constraint_compatible(&arg_types[1], &Type::Int) {
                    self.error(format!(
                        "Argument 2 of '{}': expected Int, got {}",
                        name,
                        arg_types[1].display()
                    ));
                }
                Some(list_option(elem_ty))
            }
            "List.head" => {
                if let Err(fallback) = expect_arity(self, 1, list_option(Type::Unknown)) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                Some(list_option(elem_ty))
            }
            "List.tail" => {
                if let Err(fallback) =
                    expect_arity(self, 1, list_option(Type::List(Box::new(Type::Unknown))))
                {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                Some(list_option(Type::List(Box::new(elem_ty))))
            }
            "List.push" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let mut elem_ty = list_inner(self, &arg_types[0], 1);
                let val_ty = arg_types[1].clone();
                if matches!(elem_ty, Type::Unknown) {
                    elem_ty = val_ty;
                } else if !Self::constraint_compatible(&val_ty, &elem_ty) {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        elem_ty.display(),
                        val_ty.display()
                    ));
                }
                Some(Type::List(Box::new(elem_ty)))
            }
            "List.map" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                let fn_ty = &arg_types[1];
                match fn_ty {
                    Type::Fn(params, ret, _) => {
                        if params.len() != 1 {
                            self.error(format!(
                                "Argument 2 of '{}': expected Fn(T) -> U, got {}",
                                name,
                                fn_ty.display()
                            ));
                            return Some(Type::List(Box::new(Type::Unknown)));
                        }
                        let param_ty = params[0].clone();
                        if !matches!(elem_ty, Type::Unknown)
                            && !matches!(param_ty, Type::Unknown)
                            && !elem_ty.compatible(&param_ty)
                        {
                            self.error(format!(
                                "Argument 2 of '{}': mapper expects {}, list provides {}",
                                name,
                                param_ty.display(),
                                elem_ty.display()
                            ));
                        }
                        Some(Type::List(Box::new(*ret.clone())))
                    }
                    Type::Unknown => Some(Type::List(Box::new(Type::Unknown))),
                    other => {
                        self.error(format!(
                            "Argument 2 of '{}': expected function, got {}",
                            name,
                            other.display()
                        ));
                        Some(Type::List(Box::new(Type::Unknown)))
                    }
                }
            }
            "List.filter" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let mut elem_ty = list_inner(self, &arg_types[0], 1);
                let fn_ty = &arg_types[1];
                match fn_ty {
                    Type::Fn(params, ret, _) => {
                        if params.len() != 1 {
                            self.error(format!(
                                "Argument 2 of '{}': expected Fn(T) -> Bool, got {}",
                                name,
                                fn_ty.display()
                            ));
                            return Some(Type::List(Box::new(Type::Unknown)));
                        }
                        let param_ty = params[0].clone();
                        if matches!(elem_ty, Type::Unknown) {
                            elem_ty = param_ty.clone();
                        } else if !matches!(param_ty, Type::Unknown)
                            && !elem_ty.compatible(&param_ty)
                        {
                            self.error(format!(
                                "Argument 2 of '{}': predicate expects {}, list provides {}",
                                name,
                                param_ty.display(),
                                elem_ty.display()
                            ));
                        }
                        if !Self::constraint_compatible(ret, &Type::Bool) {
                            self.error(format!(
                                "Argument 2 of '{}': predicate must return Bool, got {}",
                                name,
                                ret.display()
                            ));
                        }
                        Some(Type::List(Box::new(elem_ty)))
                    }
                    Type::Unknown => Some(Type::List(Box::new(elem_ty))),
                    other => {
                        self.error(format!(
                            "Argument 2 of '{}': expected function, got {}",
                            name,
                            other.display()
                        ));
                        Some(Type::List(Box::new(Type::Unknown)))
                    }
                }
            }
            "List.fold" => {
                if let Err(fallback) = expect_arity(self, 3, Type::Unknown) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                let mut acc_ty = arg_types[1].clone();
                let fn_ty = &arg_types[2];
                match fn_ty {
                    Type::Fn(params, ret, _) => {
                        if params.len() != 2 {
                            self.error(format!(
                                "Argument 3 of '{}': expected Fn(Acc, T) -> Acc, got {}",
                                name,
                                fn_ty.display()
                            ));
                            return Some(acc_ty);
                        }
                        let param_acc = params[0].clone();
                        let param_elem = params[1].clone();

                        if matches!(acc_ty, Type::Unknown) {
                            acc_ty = param_acc.clone();
                        } else if !matches!(param_acc, Type::Unknown)
                            && !acc_ty.compatible(&param_acc)
                        {
                            self.error(format!(
                                "Argument 3 of '{}': fold accumulator param expects {}, init is {}",
                                name,
                                param_acc.display(),
                                acc_ty.display()
                            ));
                        }

                        if !matches!(elem_ty, Type::Unknown)
                            && !matches!(param_elem, Type::Unknown)
                            && !elem_ty.compatible(&param_elem)
                        {
                            self.error(format!(
                                "Argument 3 of '{}': fold item param expects {}, list has {}",
                                name,
                                param_elem.display(),
                                elem_ty.display()
                            ));
                        }

                        if matches!(acc_ty, Type::Unknown) {
                            acc_ty = *ret.clone();
                        } else if !Self::constraint_compatible(ret, &acc_ty) {
                            self.error(format!(
                                "Argument 3 of '{}': fold function must return {}, got {}",
                                name,
                                acc_ty.display(),
                                ret.display()
                            ));
                        }

                        Some(acc_ty)
                    }
                    Type::Unknown => Some(acc_ty),
                    other => {
                        self.error(format!(
                            "Argument 3 of '{}': expected function, got {}",
                            name,
                            other.display()
                        ));
                        Some(Type::Unknown)
                    }
                }
            }
            "List.zip" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let a_ty = list_inner(self, &arg_types[0], 1);
                let b_ty = list_inner(self, &arg_types[1], 2);
                Some(Type::List(Box::new(Type::Tuple(vec![a_ty, b_ty]))))
            }
            "List.flatMap" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                let fn_ty = &arg_types[1];
                match fn_ty {
                    Type::Fn(params, ret, _) => {
                        if params.len() != 1 {
                            self.error(format!(
                                "Argument 2 of '{}': expected Fn(T) -> List<U>, got {}",
                                name,
                                fn_ty.display()
                            ));
                            return Some(Type::List(Box::new(Type::Unknown)));
                        }
                        let param_ty = params[0].clone();
                        if !matches!(elem_ty, Type::Unknown)
                            && !matches!(param_ty, Type::Unknown)
                            && !elem_ty.compatible(&param_ty)
                        {
                            self.error(format!(
                                "Argument 2 of '{}': mapper expects {}, list provides {}",
                                name,
                                param_ty.display(),
                                elem_ty.display()
                            ));
                        }
                        match ret.as_ref() {
                            Type::List(inner) => Some(Type::List(inner.clone())),
                            Type::Unknown => Some(Type::List(Box::new(Type::Unknown))),
                            other => {
                                self.error(format!(
                                    "Argument 2 of '{}': function must return List<...>, got {}",
                                    name,
                                    other.display()
                                ));
                                Some(Type::List(Box::new(Type::Unknown)))
                            }
                        }
                    }
                    Type::Unknown => Some(Type::List(Box::new(Type::Unknown))),
                    other => {
                        self.error(format!(
                            "Argument 2 of '{}': expected function, got {}",
                            name,
                            other.display()
                        ));
                        Some(Type::List(Box::new(Type::Unknown)))
                    }
                }
            }
            _ => None,
        }
    }

    pub(super) fn infer_map_call_type(&mut self, name: &str, arg_types: &[Type]) -> Option<Type> {
        let map_ty = |k: Type, v: Type| Type::Map(Box::new(k), Box::new(v));
        let option_ty = |v: Type| Type::Option(Box::new(v));
        let list_ty = |v: Type| Type::List(Box::new(v));
        let tuple2 = |k: Type, v: Type| Type::Tuple(vec![k, v]);
        let is_hashable_key_type = |ty: &Type| {
            matches!(
                ty,
                Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unknown
            )
        };
        let ensure_hashable_key = |tc: &mut Self, key_ty: &Type, name: &str, arg_idx: usize| {
            if !is_hashable_key_type(key_ty) {
                tc.error(format!(
                    "Argument {} of '{}': map key type must be Int, Float, String, or Bool (got {})",
                    arg_idx,
                    name,
                    key_ty.display()
                ));
            }
        };
        let map_parts = |tc: &mut Self, arg_ty: &Type, arg_idx: usize| -> (Type, Type) {
            match arg_ty {
                Type::Map(k, v) => ((*k.clone()), (*v.clone())),
                Type::Unknown => (Type::Unknown, Type::Unknown),
                other => {
                    tc.error(format!(
                        "Argument {} of '{}': expected Map<...>, got {}",
                        arg_idx,
                        name,
                        other.display()
                    ));
                    (Type::Unknown, Type::Unknown)
                }
            }
        };
        let expect_arity = |tc: &mut Self, expected: usize, fallback: Type| -> Result<(), Type> {
            if arg_types.len() != expected {
                tc.error(format!(
                    "Function '{}' expects {} argument(s), got {}",
                    name,
                    expected,
                    arg_types.len()
                ));
                Err(fallback)
            } else {
                Ok(())
            }
        };

        match name {
            "Map.empty" => {
                if let Err(fallback) = expect_arity(self, 0, map_ty(Type::Unknown, Type::Unknown)) {
                    return Some(fallback);
                }
                Some(map_ty(Type::Unknown, Type::Unknown))
            }
            "Map.len" => {
                if let Err(fallback) = expect_arity(self, 1, Type::Int) {
                    return Some(fallback);
                }
                let _ = map_parts(self, &arg_types[0], 1);
                Some(Type::Int)
            }
            "Map.keys" => {
                if let Err(fallback) = expect_arity(self, 1, list_ty(Type::Unknown)) {
                    return Some(fallback);
                }
                let (k, _) = map_parts(self, &arg_types[0], 1);
                ensure_hashable_key(self, &k, name, 1);
                Some(list_ty(k))
            }
            "Map.values" => {
                if let Err(fallback) = expect_arity(self, 1, list_ty(Type::Unknown)) {
                    return Some(fallback);
                }
                let (_, v) = map_parts(self, &arg_types[0], 1);
                Some(list_ty(v))
            }
            "Map.entries" => {
                if let Err(fallback) =
                    expect_arity(self, 1, list_ty(tuple2(Type::Unknown, Type::Unknown)))
                {
                    return Some(fallback);
                }
                let (k, v) = map_parts(self, &arg_types[0], 1);
                ensure_hashable_key(self, &k, name, 1);
                Some(list_ty(tuple2(k, v)))
            }
            "Map.get" => {
                if let Err(fallback) = expect_arity(self, 2, option_ty(Type::Unknown)) {
                    return Some(fallback);
                }
                let (mut k, v) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                if matches!(k, Type::Unknown) {
                    k = key_ty.clone();
                } else if !Self::constraint_compatible(&key_ty, &k) {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        k.display(),
                        key_ty.display()
                    ));
                }
                ensure_hashable_key(self, &k, name, 1);
                ensure_hashable_key(self, &key_ty, name, 2);
                Some(option_ty(v))
            }
            "Map.has" => {
                if let Err(fallback) = expect_arity(self, 2, Type::Bool) {
                    return Some(fallback);
                }
                let (mut k, _) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                if matches!(k, Type::Unknown) {
                    k = key_ty.clone();
                } else if !Self::constraint_compatible(&key_ty, &k) {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        k.display(),
                        key_ty.display()
                    ));
                }
                ensure_hashable_key(self, &k, name, 1);
                ensure_hashable_key(self, &key_ty, name, 2);
                Some(Type::Bool)
            }
            "Map.remove" => {
                if let Err(fallback) = expect_arity(self, 2, map_ty(Type::Unknown, Type::Unknown)) {
                    return Some(fallback);
                }
                let (mut k, v) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                if matches!(k, Type::Unknown) {
                    k = key_ty.clone();
                } else if !Self::constraint_compatible(&key_ty, &k) {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        k.display(),
                        key_ty.display()
                    ));
                }
                ensure_hashable_key(self, &k, name, 1);
                ensure_hashable_key(self, &key_ty, name, 2);
                Some(map_ty(k, v))
            }
            "Map.set" => {
                if let Err(fallback) = expect_arity(self, 3, map_ty(Type::Unknown, Type::Unknown)) {
                    return Some(fallback);
                }
                let (mut k, mut v) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                let val_ty = arg_types[2].clone();

                if matches!(k, Type::Unknown) {
                    k = key_ty.clone();
                } else if !Self::constraint_compatible(&key_ty, &k) {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        k.display(),
                        key_ty.display()
                    ));
                }
                if matches!(v, Type::Unknown) {
                    v = val_ty.clone();
                } else if !Self::constraint_compatible(&val_ty, &v) {
                    self.error(format!(
                        "Argument 3 of '{}': expected {}, got {}",
                        name,
                        v.display(),
                        val_ty.display()
                    ));
                }
                ensure_hashable_key(self, &k, name, 1);
                ensure_hashable_key(self, &key_ty, name, 2);
                Some(map_ty(k, v))
            }
            "Map.fromList" => {
                if let Err(fallback) = expect_arity(self, 1, map_ty(Type::Unknown, Type::Unknown)) {
                    return Some(fallback);
                }
                let (k, v) = match &arg_types[0] {
                    Type::List(inner) => match inner.as_ref() {
                        Type::Tuple(elems) if elems.len() == 2 => {
                            (elems[0].clone(), elems[1].clone())
                        }
                        Type::Unknown => (Type::Unknown, Type::Unknown),
                        other => {
                            self.error(format!(
                                "Argument 1 of '{}': expected List<(K, V)>, got List<{}>",
                                name,
                                other.display()
                            ));
                            (Type::Unknown, Type::Unknown)
                        }
                    },
                    Type::Unknown => (Type::Unknown, Type::Unknown),
                    other => {
                        self.error(format!(
                            "Argument 1 of '{}': expected List<(K, V)>, got {}",
                            name,
                            other.display()
                        ));
                        (Type::Unknown, Type::Unknown)
                    }
                };
                ensure_hashable_key(self, &k, name, 1);
                Some(map_ty(k, v))
            }
            _ => None,
        }
    }

    // -----------------------------------------------------------------------
    // Type inference for expressions
    // -----------------------------------------------------------------------
    pub(super) fn infer_type(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Literal(lit) => match lit {
                crate::ast::Literal::Int(_) => Type::Int,
                crate::ast::Literal::Float(_) => Type::Float,
                crate::ast::Literal::Str(_) => Type::Str,
                crate::ast::Literal::Bool(_) => Type::Bool,
            },

            Expr::InterpolatedStr(_) => Type::Str,

            Expr::Ident(name) => {
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
                // Infer arg types
                let arg_types: Vec<Type> = args.iter().map(|a| self.infer_type(a)).collect();

                // Helper: check arity + arg types against a sig, return sig.ret
                let check_call = |tc: &mut Self, display_name: &str, sig: FnSig| -> Type {
                    if arg_types.len() != sig.params.len() {
                        tc.error(format!(
                            "Function '{}' expects {} argument(s), got {}",
                            display_name,
                            sig.params.len(),
                            arg_types.len()
                        ));
                    } else {
                        for (i, (arg_ty, param_ty)) in
                            arg_types.iter().zip(sig.params.iter()).enumerate()
                        {
                            if !Self::constraint_compatible(arg_ty, param_ty) {
                                tc.error(format!(
                                    "Argument {} of '{}': expected {}, got {}",
                                    i + 1,
                                    display_name,
                                    param_ty.display(),
                                    arg_ty.display()
                                ));
                            }
                        }
                    }
                    sig.ret
                };

                if let Expr::Ident(name) = fn_expr.as_ref() {
                    if let Some(sig) = self.fn_sigs.get(name).cloned() {
                        return check_call(self, name, sig);
                    }
                    if let Some(binding_ty) = self.binding_type(name) {
                        if let Some(sig) = Self::sig_from_callable_type(&binding_ty) {
                            return check_call(self, name, sig);
                        }
                        self.error(format!(
                            "Cannot call '{}': expected function, got {}",
                            name,
                            binding_ty.display()
                        ));
                        return Type::Unknown;
                    }
                    self.error(format!("Call to unknown function '{}'", name));
                    return Type::Unknown;
                }

                if let Some(display_name) = Self::callee_key(fn_expr) {
                    if let Some(ty) = self.infer_list_call_type(&display_name, &arg_types) {
                        return ty;
                    }
                    if let Some(ty) = self.infer_map_call_type(&display_name, &arg_types) {
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
                        return check_call(self, &display_name, sig);
                    }
                }

                let callee_ty = self.infer_type(fn_expr);
                if let Some(sig) = Self::sig_from_callable_type(&callee_ty) {
                    return check_call(self, "<fn value>", sig);
                }

                if !matches!(callee_ty, Type::Unknown) {
                    self.error(format!("Cannot call value of type {}", callee_ty.display()));
                }
                Type::Unknown
            }

            Expr::BinOp(op, left, right) => {
                let lt = self.infer_type(left);
                let rt = self.infer_type(right);
                self.check_binop(op, &lt, &rt);
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
                    self.infer_type(first)
                } else {
                    Type::Unknown
                };
                Type::List(Box::new(inner))
            }

            Expr::Tuple(items) => {
                let tys = items.iter().map(|item| self.infer_type(item)).collect();
                Type::Tuple(tys)
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

            Expr::Match {
                subject,
                arms,
                line,
            } => {
                let subject_ty = self.infer_type(subject);
                self.check_match_exhaustiveness(&subject_ty, arms, *line);
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

            Expr::Pipe(left, right) => {
                // x |> f is equivalent to f(x)
                let call = Expr::FnCall(Box::new((**right).clone()), vec![(**left).clone()]);
                self.infer_type(&call)
            }

            Expr::ErrorProp(inner) => {
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
                                    self.error(format!(
                                        "Operator '?': Err type {} is incompatible with function's Err type {}",
                                        err_ty.display(),
                                        fn_err_ty.display()
                                    ));
                                }
                            }
                            Some(Type::Unknown) => {} // gradual typing — skip check
                            Some(other) => {
                                self.error(format!(
                                    "Operator '?' used in function returning {}, which is not Result",
                                    other.display()
                                ));
                            }
                            None => {
                                self.error("Operator '?' used outside of a function".to_string());
                            }
                        }
                        *ok_ty
                    }
                    Type::Unknown => Type::Unknown,
                    other => {
                        self.error(format!(
                            "Operator '?' can only be applied to Result, got {}",
                            other.display()
                        ));
                        Type::Unknown
                    }
                }
            }

            Expr::Attr(obj, field) => {
                if let Some(mut parts) = Self::attr_path(obj) {
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
                if type_name == "Tcp.Connection" {
                    self.error(
                        "Cannot construct 'Tcp.Connection' directly. Use Tcp.connect(host, port)."
                            .to_string(),
                    );
                }

                let schema_prefix = format!("{}.", type_name);
                let mut expected = HashMap::new();
                for (key, ty) in &self.record_field_types {
                    if let Some(field_name) = key.strip_prefix(&schema_prefix) {
                        expected.insert(field_name.to_string(), ty.clone());
                    }
                }

                let mut seen = HashSet::new();
                for (field_name, expr) in fields {
                    let actual_ty = self.infer_type(expr);
                    if !seen.insert(field_name.clone()) {
                        self.error(format!(
                            "Record '{}' field '{}' provided more than once",
                            type_name, field_name
                        ));
                        continue;
                    }

                    if expected.is_empty() {
                        continue;
                    }

                    if let Some(expected_ty) = expected.get(field_name) {
                        if !Self::constraint_compatible(&actual_ty, expected_ty) {
                            self.error(format!(
                                "Record '{}' field '{}' expects {}, got {}",
                                type_name,
                                field_name,
                                expected_ty.display(),
                                actual_ty.display()
                            ));
                        }
                    } else {
                        self.error(format!(
                            "Record '{}' has no field '{}'",
                            type_name, field_name
                        ));
                    }
                }

                if !expected.is_empty() {
                    let mut required = expected.keys().cloned().collect::<Vec<_>>();
                    required.sort();
                    for field_name in required {
                        if !seen.contains(&field_name) {
                            self.error(format!(
                                "Record '{}' missing required field '{}'",
                                type_name, field_name
                            ));
                        }
                    }
                }
                Type::Named(type_name.clone())
            }

            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => {
                let base_ty = self.infer_type(base);
                let expected_ty = Type::Named(type_name.clone());
                if !Self::constraint_compatible(&base_ty, &expected_ty) {
                    self.error(format!(
                        "{}.update: base has type {}, expected {}",
                        type_name,
                        base_ty.display(),
                        type_name
                    ));
                }

                let schema_prefix = format!("{}.", type_name);
                let mut expected_fields = HashMap::new();
                for (key, ty) in &self.record_field_types {
                    if let Some(field_name) = key.strip_prefix(&schema_prefix) {
                        expected_fields.insert(field_name.to_string(), ty.clone());
                    }
                }

                for (field_name, expr) in updates {
                    let actual_ty = self.infer_type(expr);
                    if expected_fields.is_empty() {
                        continue;
                    }
                    if let Some(expected_ty) = expected_fields.get(field_name) {
                        if !Self::constraint_compatible(&actual_ty, expected_ty) {
                            self.error(format!(
                                "Record '{}' field '{}' expects {}, got {}",
                                type_name,
                                field_name,
                                expected_ty.display(),
                                actual_ty.display()
                            ));
                        }
                    } else {
                        self.error(format!(
                            "Record '{}' has no field '{}'",
                            type_name, field_name
                        ));
                    }
                }

                Type::Named(type_name.clone())
            }

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

    pub(super) fn infer_type_with_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        subject_ty: &Type,
        body: &Expr,
    ) -> Type {
        let mut bindings = Vec::new();
        self.collect_pattern_bindings(pattern, subject_ty, &mut bindings);

        let mut prev = Vec::new();
        for (bind_name, bind_ty) in bindings {
            let old = self.locals.get(&bind_name).cloned();
            prev.push((bind_name.clone(), old));
            self.locals.insert(bind_name, bind_ty);
        }

        let out_ty = self.infer_type(body);

        for (name, old) in prev {
            if let Some(old_val) = old {
                self.locals.insert(name, old_val);
            } else {
                self.locals.remove(&name);
            }
        }

        out_ty
    }

    fn pattern_constructor_binding_types(
        &self,
        ctor_name: &str,
        subject_ty: &Type,
        arity: usize,
    ) -> Vec<Type> {
        let ctor_base = ctor_name.rsplit('.').next().unwrap_or(ctor_name);
        let unknowns = || vec![Type::Unknown; arity];

        let from_sig = |name: &str| -> Option<Vec<Type>> {
            self.fn_sigs.get(name).and_then(|sig| {
                if sig.params.len() == arity {
                    Some(sig.params.clone())
                } else {
                    None
                }
            })
        };

        match subject_ty {
            Type::Result(ok_ty, err_ty) => match ctor_base {
                "Ok" if arity == 1 => return vec![*ok_ty.clone()],
                "Err" if arity == 1 => return vec![*err_ty.clone()],
                _ => {}
            },
            Type::Option(inner_ty) => match ctor_base {
                "Some" if arity == 1 => return vec![*inner_ty.clone()],
                "None" if arity == 0 => return Vec::new(),
                _ => {}
            },
            Type::Named(type_name) => {
                let qualified = if ctor_name.contains('.') {
                    ctor_name.to_string()
                } else {
                    format!("{}.{}", type_name, ctor_name)
                };
                if let Some(params) = from_sig(&qualified) {
                    return params;
                }
            }
            _ => {}
        }

        if let Some(params) = from_sig(ctor_name) {
            return params;
        }

        if !ctor_name.contains('.') {
            let suffix = format!(".{}", ctor_name);
            let mut matching = self
                .fn_sigs
                .iter()
                .filter_map(|(name, sig)| {
                    if name.ends_with(&suffix) && sig.params.len() == arity {
                        Some(sig.params.clone())
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            if matching.len() == 1 {
                return matching.pop().unwrap_or_else(unknowns);
            }
        }

        unknowns()
    }

    pub(super) fn collect_pattern_bindings(
        &self,
        pattern: &Pattern,
        subject_ty: &Type,
        out: &mut Vec<(String, Type)>,
    ) {
        match pattern {
            Pattern::Ident(name) if name != "_" => out.push((name.clone(), subject_ty.clone())),
            Pattern::Cons(head, tail) => {
                let elem_ty = match subject_ty {
                    Type::List(inner) => *inner.clone(),
                    _ => Type::Unknown,
                };
                if head != "_" {
                    out.push((head.clone(), elem_ty.clone()));
                }
                if tail != "_" {
                    out.push((tail.clone(), Type::List(Box::new(elem_ty))));
                }
            }
            Pattern::Constructor(name, bindings) => {
                let binding_tys =
                    self.pattern_constructor_binding_types(name, subject_ty, bindings.len());
                for (bind_name, bind_ty) in bindings.iter().zip(binding_tys.into_iter()) {
                    if bind_name != "_" {
                        out.push((bind_name.clone(), bind_ty));
                    }
                }
            }
            Pattern::Tuple(items) => {
                let elem_tys = match subject_ty {
                    Type::Tuple(elems) if elems.len() == items.len() => elems.clone(),
                    _ => vec![Type::Unknown; items.len()],
                };
                for (item, elem_ty) in items.iter().zip(elem_tys.iter()) {
                    self.collect_pattern_bindings(item, elem_ty, out);
                }
            }
            _ => {}
        }
    }

    // -----------------------------------------------------------------------
    // BinOp type rules
    // -----------------------------------------------------------------------
    pub(super) fn check_binop(&mut self, op: &BinOp, lt: &Type, rt: &Type) {
        if matches!(lt, Type::Unknown) || matches!(rt, Type::Unknown) {
            return; // gradual — skip
        }
        match op {
            BinOp::Add => {
                let ok = (matches!(lt, Type::Int | Type::Float)
                    && matches!(rt, Type::Int | Type::Float))
                    || (matches!(lt, Type::Str) && matches!(rt, Type::Str));
                if !ok {
                    self.error(format!(
                        "Operator '+' requires Int/Float or String on both sides, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Sub | BinOp::Mul | BinOp::Div => {
                let ok =
                    matches!(lt, Type::Int | Type::Float) && matches!(rt, Type::Int | Type::Float);
                if !ok {
                    self.error(format!(
                        "Arithmetic operator requires numeric types, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Eq | BinOp::Neq => {
                if !lt.compatible(rt) {
                    self.error(format!(
                        "Equality operator requires same types, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
            BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => {
                let ok = (matches!(lt, Type::Int | Type::Float)
                    && matches!(rt, Type::Int | Type::Float))
                    || (matches!(lt, Type::Str) && matches!(rt, Type::Str));
                if !ok {
                    self.error(format!(
                        "Comparison operator requires numeric or String types, got {} and {}",
                        lt.display(),
                        rt.display()
                    ));
                }
            }
        }
    }
}
