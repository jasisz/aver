use super::*;

impl TypeChecker {
    /// Threads an `expected`
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
        let errors_before = self.errors.len();
        self.collect_pattern_bindings(pattern, subject_ty, &mut bindings);
        // A pattern error belongs to its arm, not to the enclosing fn.
        if body.line > 0 {
            for error in &mut self.errors[errors_before..] {
                error.line = body.line;
            }
        }

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
            Type::Named {
                name: _type_name, ..
            } => {
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

    /// The field types a constructor pattern binds, after the checks
    /// every constructor pattern gets: no match on an opaque type or a
    /// capability resource, and no `Result` / `Option` constructor
    /// against a subject of another type. `None` when the pattern is
    /// refused (its fields then bind `Invalid`).
    fn constructor_pattern_field_types(
        &mut self,
        name: &str,
        subject_ty: &Type,
        arity: usize,
    ) -> Option<Vec<Type>> {
        // Check if this pattern matches on an opaque type's representation.
        // Iron — A3: resolve the bare type prefix through
        // `sig_aliases` because `opaque_types` is keyed by the
        // canonical `Module.Type` form.
        let type_prefix = name.split('.').next().unwrap_or(name);
        let canon_prefix = self.canonical_type_name(type_prefix);
        if !self.self_host_mode && self.opaque_types.contains(&canon_prefix) {
            if self.is_capability_resource_type(type_prefix) {
                self.error(format!(
                    "Cannot pattern match on capability resource '{}'",
                    type_prefix
                ));
            } else {
                self.error(format!(
                    "Cannot pattern match on opaque type '{}'",
                    type_prefix
                ));
            }
            return None;
        }
        // A `Result` / `Option` constructor pattern against a
        // subject that is neither is always a bug: no value of the
        // subject's type can ever take the arm, so the match walks
        // off the end at runtime with no diagnostic. The literal
        // smart-constructor discharge makes this reachable by
        // ordinary edits — `match Bytes.fromList([1, 2])` used to
        // scrutinise a `Result` and now scrutinises a `Bytes` — so
        // the migration has to be loud instead of silent.
        if matches!(type_prefix, "Result" | "Option")
            && !matches!(
                subject_ty,
                Type::Result(_, _) | Type::Option(_) | Type::Invalid | Type::Var(_)
            )
        {
            self.error(format!(
                "Pattern '{}' matches a {} value, but the match subject is {}",
                name,
                type_prefix,
                subject_ty.display()
            ));
        }
        self.record_pattern_constructor_family(name, subject_ty);
        Some(self.pattern_constructor_binding_types(name, subject_ty, arity))
    }

    /// A literal pattern can only ever match a value of its own
    /// primitive type; against any other concrete subject the arm is
    /// dead, so say so. Named types (refinements, records) and
    /// not-yet-known types are left alone.
    fn check_literal_pattern(&mut self, literal: &Literal, subject_ty: &Type) {
        let fits = match (literal, subject_ty) {
            (_, Type::Invalid | Type::Var(_) | Type::Named { .. }) => return,
            (Literal::Int(_) | Literal::BigInt(_), Type::Int)
            | (Literal::Float(_), Type::Float)
            | (Literal::Str(_), Type::Str)
            | (Literal::Bool(_), Type::Bool)
            | (Literal::Unit, Type::Unit) => true,
            _ => false,
        };
        if !fits {
            let kind = match literal {
                Literal::Int(_) | Literal::BigInt(_) => "Int",
                Literal::Float(_) => "Float",
                Literal::Str(_) => "String",
                Literal::Bool(_) => "Bool",
                Literal::Unit => "Unit",
            };
            self.error(format!(
                "Literal pattern of type {} cannot match a value of type {}",
                kind,
                subject_ty.display()
            ));
        }
    }

    /// Remember, for the constructor spelled `name` in a pattern, the
    /// variants of the sum type it belongs to. The nested-pattern
    /// compiler reads it to know when the constructors of one switch
    /// cover the whole type, so it never emits a default arm no value
    /// can reach.
    fn record_pattern_constructor_family(&mut self, name: &str, subject_ty: &Type) {
        let Type::Named {
            name: type_name, ..
        } = subject_ty
        else {
            return;
        };
        if self.pattern_ctor_families.contains_key(name) {
            return;
        }
        if let Some(variants) = self.variants_for(type_name) {
            let variants = variants.clone();
            self.pattern_ctor_families
                .insert(name.to_string(), variants);
        }
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
                let Some(binding_tys) =
                    self.constructor_pattern_field_types(name, subject_ty, bindings.len())
                else {
                    for bind_name in bindings {
                        if bind_name != "_" {
                            out.push((bind_name.clone(), Type::Invalid));
                        }
                    }
                    return;
                };
                for (bind_name, bind_ty) in bindings.iter().zip(binding_tys) {
                    if bind_name != "_" {
                        out.push((bind_name.clone(), bind_ty));
                    }
                }
            }
            Pattern::ConstructorNested(name, fields) => {
                let field_tys = self
                    .constructor_pattern_field_types(name, subject_ty, fields.len())
                    .unwrap_or_else(|| vec![Type::Invalid; fields.len()]);
                for (field, field_ty) in fields.iter().zip(field_tys.iter()) {
                    self.collect_pattern_bindings(field, field_ty, out);
                }
            }
            Pattern::List { items, rest } => {
                let elem_ty = match subject_ty {
                    Type::List(inner) => *inner.clone(),
                    Type::Invalid | Type::Var(_) => Type::Invalid,
                    other => {
                        self.error(format!(
                            "List pattern matches a List value, but the match subject is {}",
                            other.display()
                        ));
                        Type::Invalid
                    }
                };
                for item in items {
                    self.collect_pattern_bindings(item, &elem_ty, out);
                }
                if let Some(rest) = rest
                    && rest != "_"
                {
                    out.push((rest.clone(), Type::List(Box::new(elem_ty))));
                }
            }
            Pattern::Literal(literal) => self.check_literal_pattern(literal, subject_ty),
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
