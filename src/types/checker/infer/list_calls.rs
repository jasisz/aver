use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_list_call_type(
        &mut self,
        name: &str,
        arg_types: &[Type],
    ) -> Option<Type> {
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
            "List.prepend" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let mut elem_ty = list_inner(self, &arg_types[1], 2);
                let val_ty = arg_types[0].clone();
                if matches!(elem_ty, Type::Unknown) {
                    elem_ty = val_ty;
                } else if !Self::constraint_compatible(&val_ty, &elem_ty) {
                    self.error(format!(
                        "Argument 1 of '{}': expected {}, got {}",
                        name,
                        elem_ty.display(),
                        val_ty.display()
                    ));
                }
                Some(Type::List(Box::new(elem_ty)))
            }
            "List.take" | "List.drop" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
                if !matches!(arg_types[1], Type::Int | Type::Unknown) {
                    self.error(format!(
                        "Argument 2 of '{}': expected Int, got {}",
                        name,
                        arg_types[1].display()
                    ));
                }
                Some(Type::List(Box::new(elem_ty)))
            }
            "List.concat" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let left_ty = list_inner(self, &arg_types[0], 1);
                let right_ty = list_inner(self, &arg_types[1], 2);
                let out_ty = match (&left_ty, &right_ty) {
                    (Type::Unknown, _) => right_ty,
                    (_, Type::Unknown) => left_ty,
                    _ if left_ty.compatible(&right_ty) => left_ty,
                    _ => {
                        self.error(format!(
                            "Arguments of '{}': list element types differ: {} vs {}",
                            name,
                            left_ty.display(),
                            right_ty.display()
                        ));
                        Type::Unknown
                    }
                };
                Some(Type::List(Box::new(out_ty)))
            }
            "List.reverse" => {
                if let Err(fallback) = expect_arity(self, 1, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let elem_ty = list_inner(self, &arg_types[0], 1);
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
            "List.zip" => {
                if let Err(fallback) = expect_arity(self, 2, Type::List(Box::new(Type::Unknown))) {
                    return Some(fallback);
                }
                let a_ty = list_inner(self, &arg_types[0], 1);
                let b_ty = list_inner(self, &arg_types[1], 2);
                Some(Type::List(Box::new(Type::Tuple(vec![a_ty, b_ty]))))
            }
            _ => None,
        }
    }
}
