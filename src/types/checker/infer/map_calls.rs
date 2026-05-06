use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_map_call_type(
        &mut self,
        name: &str,
        arg_types: &[Type],
    ) -> Option<Type> {
        let map_ty = |k: Type, v: Type| Type::Map(Box::new(k), Box::new(v));
        let option_ty = |v: Type| Type::Option(Box::new(v));
        let list_ty = |v: Type| Type::List(Box::new(v));
        let tuple2 = |k: Type, v: Type| Type::Tuple(vec![k, v]);
        let is_hashable_key_type = |ty: &Type| {
            // The Map runtime hashes any heap value through rt_deep_hash,
            // so user-defined types (variants/records/tuples/lists) are
            // first-class keys alongside the built-in scalars.
            !matches!(ty, Type::Fn { .. } | Type::Unit)
        };
        let ensure_hashable_key = |tc: &mut Self, key_ty: &Type, name: &str, arg_idx: usize| {
            if !is_hashable_key_type(key_ty) {
                tc.error(format!(
                    "Argument {} of '{}': map key type must be hashable (got {})",
                    arg_idx,
                    name,
                    key_ty.display()
                ));
            }
        };
        let map_parts = |tc: &mut Self, arg_ty: &Type, arg_idx: usize| -> (Type, Type) {
            match arg_ty {
                Type::Map(k, v) => ((*k.clone()), (*v.clone())),
                Type::Invalid => (Type::Invalid, Type::Invalid),
                other => {
                    tc.error(format!(
                        "Argument {} of '{}': expected Map<...>, got {}",
                        arg_idx,
                        name,
                        other.display()
                    ));
                    (Type::Invalid, Type::Invalid)
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
            "Map.len" => {
                if let Err(fallback) = expect_arity(self, 1, Type::Int) {
                    return Some(fallback);
                }
                let _ = map_parts(self, &arg_types[0], 1);
                Some(Type::Int)
            }
            "Map.keys" => {
                if let Err(fallback) = expect_arity(self, 1, list_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                let (k, _) = map_parts(self, &arg_types[0], 1);
                ensure_hashable_key(self, &k, name, 1);
                Some(list_ty(k))
            }
            "Map.values" => {
                if let Err(fallback) = expect_arity(self, 1, list_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                let (_, v) = map_parts(self, &arg_types[0], 1);
                Some(list_ty(v))
            }
            "Map.entries" => {
                if let Err(fallback) =
                    expect_arity(self, 1, list_ty(tuple2(Type::Invalid, Type::Invalid)))
                {
                    return Some(fallback);
                }
                let (k, v) = map_parts(self, &arg_types[0], 1);
                ensure_hashable_key(self, &k, name, 1);
                Some(list_ty(tuple2(k, v)))
            }
            "Map.get" => {
                if let Err(fallback) = expect_arity(self, 2, option_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                let (mut k, v) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                if matches!(k, Type::Var(_)) {
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
                if matches!(k, Type::Var(_)) {
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
                if let Err(fallback) = expect_arity(self, 2, map_ty(Type::Invalid, Type::Invalid)) {
                    return Some(fallback);
                }
                let (mut k, v) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                if matches!(k, Type::Var(_)) {
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
                if let Err(fallback) = expect_arity(self, 3, map_ty(Type::Invalid, Type::Invalid)) {
                    return Some(fallback);
                }
                let (mut k, mut v) = map_parts(self, &arg_types[0], 1);
                let key_ty = arg_types[1].clone();
                let val_ty = arg_types[2].clone();

                if matches!(k, Type::Var(_)) {
                    k = key_ty.clone();
                } else if !Self::constraint_compatible(&key_ty, &k) {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        k.display(),
                        key_ty.display()
                    ));
                }
                if matches!(v, Type::Var(_)) {
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
                if let Err(fallback) = expect_arity(self, 1, map_ty(Type::Invalid, Type::Invalid)) {
                    return Some(fallback);
                }
                let (k, v) = match &arg_types[0] {
                    Type::List(inner) => match inner.as_ref() {
                        Type::Tuple(elems) if elems.len() == 2 => {
                            (elems[0].clone(), elems[1].clone())
                        }
                        Type::Invalid => (Type::Invalid, Type::Invalid),
                        other => {
                            self.error(format!(
                                "Argument 1 of '{}': expected List<(K, V)>, got List<{}>",
                                name,
                                other.display()
                            ));
                            (Type::Invalid, Type::Invalid)
                        }
                    },
                    Type::Invalid => (Type::Invalid, Type::Invalid),
                    other => {
                        self.error(format!(
                            "Argument 1 of '{}': expected List<(K, V)>, got {}",
                            name,
                            other.display()
                        ));
                        (Type::Invalid, Type::Invalid)
                    }
                };
                ensure_hashable_key(self, &k, name, 1);
                Some(map_ty(k, v))
            }
            _ => None,
        }
    }
}
