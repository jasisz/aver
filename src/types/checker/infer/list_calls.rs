use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_list_call_type(
        &mut self,
        name: &str,
        arg_types: &[Type],
    ) -> Option<Type> {
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
            "List.contains" => {
                if let Err(fallback) = expect_arity(self, 2, Type::Bool) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                let needle_ty = arg_types[1].clone();
                if !matches!(elem_ty, Type::Unknown)
                    && !matches!(needle_ty, Type::Unknown)
                    && !Self::constraint_compatible(&needle_ty, &elem_ty)
                {
                    self.error(format!(
                        "Argument 2 of '{}': expected {}, got {}",
                        name,
                        elem_ty.display(),
                        needle_ty.display()
                    ));
                }
                Some(Type::Bool)
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
}
