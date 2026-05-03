use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_type_with_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        subject_ty: &Type,
        body: &Spanned<Expr>,
    ) -> Type {
        self.infer_type_with_pattern_bindings_expected(pattern, subject_ty, body, None)
    }

    /// Same as `infer_type_with_pattern_bindings`, but threads an `expected`
    /// type into the arm body so generic constructors in arm positions
    /// (`[] -> Option.None`) pick up T from the surrounding context (fn
    /// return type, outer expected) instead of stamping `Unknown`.
    pub(in super::super) fn infer_type_with_pattern_bindings_expected(
        &mut self,
        pattern: &Pattern,
        subject_ty: &Type,
        body: &Spanned<Expr>,
        expected: Option<&Type>,
    ) -> Type {
        let mut bindings = Vec::new();
        self.collect_pattern_bindings(pattern, subject_ty, &mut bindings);

        let mut prev = Vec::new();
        for (bind_name, bind_ty) in bindings {
            let old = self.locals.get(&bind_name).cloned();
            prev.push((bind_name.clone(), old));
            self.locals.insert(bind_name, bind_ty);
        }

        let out_ty = self.infer_type_with_expected(body, expected);

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
        let unknowns = || vec![Type::Invalid; arity];

        let from_sig = |name: &str| -> Option<Vec<Type>> {
            self.find_fn_sig(name).and_then(|sig| {
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
            Type::Named(_type_name) => {
                let qualified = if ctor_name.contains('.') {
                    ctor_name.to_string()
                } else {
                    return unknowns();
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

        unknowns()
    }

    pub(in super::super) fn collect_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        subject_ty: &Type,
        out: &mut Vec<(String, Type)>,
    ) {
        match pattern {
            Pattern::Ident(name) if name != "_" => out.push((name.clone(), subject_ty.clone())),
            Pattern::Cons(head, tail) => {
                let elem_ty = match subject_ty {
                    Type::List(inner) => *inner.clone(),
                    _ => Type::Invalid,
                };
                if head != "_" {
                    out.push((head.clone(), elem_ty.clone()));
                }
                if tail != "_" {
                    out.push((tail.clone(), Type::List(Box::new(elem_ty))));
                }
            }
            Pattern::Constructor(name, bindings) => {
                // Check if this pattern matches on an opaque type's representation.
                let type_prefix = name.split('.').next().unwrap_or(name);
                if self.opaque_types.contains(type_prefix) {
                    self.error(format!(
                        "Cannot pattern match on opaque type '{}'",
                        type_prefix
                    ));
                    for bind_name in bindings {
                        if bind_name != "_" {
                            out.push((bind_name.clone(), Type::Invalid));
                        }
                    }
                    return;
                }
                let binding_tys =
                    self.pattern_constructor_binding_types(name, subject_ty, bindings.len());
                for (bind_name, bind_ty) in bindings.iter().zip(binding_tys) {
                    if bind_name != "_" {
                        out.push((bind_name.clone(), bind_ty));
                    }
                }
            }
            Pattern::Tuple(items) => {
                let elem_tys = match subject_ty {
                    Type::Tuple(elems) if elems.len() == items.len() => elems.clone(),
                    _ => vec![Type::Invalid; items.len()],
                };
                for (item, elem_ty) in items.iter().zip(elem_tys.iter()) {
                    self.collect_pattern_bindings(item, elem_ty, out);
                }
            }
            _ => {}
        }
    }
}
