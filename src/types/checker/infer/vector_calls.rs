use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_vector_call_type(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
        arg_types: &[Type],
    ) -> Option<Type> {
        let vector_ty = |t: Type| Type::Vector(Box::new(t));
        let option_ty = |t: Type| Type::Option(Box::new(t));
        let list_ty = |t: Type| Type::List(Box::new(t));
        let vector_elem = |tc: &mut Self, arg_ty: &Type, arg_idx: usize| -> Type {
            match arg_ty {
                Type::Vector(inner) => *inner.clone(),
                Type::Invalid => Type::Invalid,
                other => {
                    tc.error(format!(
                        "Argument {} of '{}': expected Vector<...>, got {}",
                        arg_idx,
                        name,
                        other.display()
                    ));
                    Type::Invalid
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
            "Vector.new" => {
                if let Err(fallback) = expect_arity(self, 2, vector_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                if !matches!(arg_types[0], Type::Int | Type::Invalid) {
                    self.error(format!(
                        "Argument 1 of '{}': expected Int, got {}",
                        name,
                        arg_types[0].display()
                    ));
                }
                let elem_ty = arg_types[1].clone();
                let vector = vector_ty(elem_ty);
                if crate::ast::is_literal_vector_size(&args[0]) {
                    Some(vector)
                } else {
                    Some(Type::Result(Box::new(vector), Box::new(Type::Str)))
                }
            }
            "Vector.get" => {
                if let Err(fallback) = expect_arity(self, 2, option_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                let elem = vector_elem(self, &arg_types[0], 1);
                if !matches!(arg_types[1], Type::Int | Type::Invalid) {
                    self.error(format!(
                        "Argument 2 of '{}': expected Int, got {}",
                        name,
                        arg_types[1].display()
                    ));
                }
                Some(option_ty(elem))
            }
            "Vector.set" => {
                if let Err(fallback) = expect_arity(self, 3, option_ty(vector_ty(Type::Invalid))) {
                    return Some(fallback);
                }
                let mut elem = vector_elem(self, &arg_types[0], 1);
                if !matches!(arg_types[1], Type::Int | Type::Invalid) {
                    self.error(format!(
                        "Argument 2 of '{}': expected Int, got {}",
                        name,
                        arg_types[1].display()
                    ));
                }
                let val_ty = arg_types[2].clone();
                if matches!(elem, Type::Var(_)) {
                    elem = val_ty.clone();
                } else if !self.compatible(&val_ty, &elem) {
                    let (want, got) = self.describe_type_pair(&elem, &val_ty);
                    self.error(format!(
                        "Argument 3 of '{}': expected {}, got {}",
                        name, want, got
                    ));
                }
                Some(option_ty(vector_ty(elem)))
            }
            "Vector.len" => {
                if let Err(fallback) = expect_arity(self, 1, Type::Int) {
                    return Some(fallback);
                }
                let _ = vector_elem(self, &arg_types[0], 1);
                Some(Type::Int)
            }
            "Vector.fromList" => {
                if let Err(fallback) = expect_arity(self, 1, vector_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                let elem = match &arg_types[0] {
                    Type::List(inner) => *inner.clone(),
                    Type::Invalid => Type::Invalid,
                    other => {
                        self.error(format!(
                            "Argument 1 of '{}': expected List<...>, got {}",
                            name,
                            other.display()
                        ));
                        Type::Invalid
                    }
                };
                Some(vector_ty(elem))
            }
            "List.fromVector" => {
                if let Err(fallback) = expect_arity(self, 1, list_ty(Type::Invalid)) {
                    return Some(fallback);
                }
                let elem = vector_elem(self, &arg_types[0], 1);
                Some(list_ty(elem))
            }
            _ => None,
        }
    }
}
