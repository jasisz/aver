use super::*;

impl TypeChecker {
    pub(super) fn build_signatures(&mut self, items: &[TopLevel]) {
        // Register function signatures and type defs.
        for item in items {
            match item {
                TopLevel::FnDef(f) => {
                    let mut params = Vec::new();
                    for (param_name, ty_str) in &f.params {
                        match parse_type_str_strict(ty_str) {
                            Ok(ty) => params.push(ty),
                            Err(unknown) => {
                                self.error(format!(
                                    "Function '{}': unknown type '{}' for parameter '{}'",
                                    f.name, unknown, param_name
                                ));
                                params.push(Type::Unknown);
                            }
                        }
                    }
                    let ret = match parse_type_str_strict(&f.return_type) {
                        Ok(ty) => ty,
                        Err(unknown) => {
                            self.error(format!(
                                "Function '{}': unknown return type '{}'",
                                f.name, unknown
                            ));
                            Type::Unknown
                        }
                    };
                    self.fn_sigs.insert(
                        f.name.clone(),
                        FnSig {
                            params,
                            ret,
                            effects: f.effects.iter().map(|e| e.node.clone()).collect(),
                        },
                    );
                }
                TopLevel::TypeDef(td) => {
                    self.register_type_def_sigs(td);
                }
                _ => {}
            }
        }
    }

    /// Register constructor signatures for user-defined types.
    pub(super) fn register_type_def_sigs(&mut self, td: &TypeDef) {
        match td {
            TypeDef::Sum {
                name: type_name,
                variants,
                ..
            } => {
                // Register variant names for exhaustiveness checking.
                self.type_variants.insert(
                    type_name.clone(),
                    variants.iter().map(|v| v.name.clone()).collect(),
                );
                // Register the type name in fn_sigs so `Ident("Shape")` resolves
                // to Named("Shape") without error (checked after locals in infer_type).
                self.fn_sigs.insert(
                    type_name.clone(),
                    FnSig {
                        params: vec![],
                        ret: Type::Named(type_name.clone()),
                        effects: vec![],
                    },
                );
                // Register each constructor with a qualified key: "Shape.Circle"
                for variant in variants {
                    let params: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| parse_type_str_strict(f).unwrap_or(Type::Unknown))
                        .collect();
                    let key = format!("{}.{}", type_name, variant.name);
                    if params.is_empty() {
                        // Zero-arg constructors are values in Aver (`Shape.Point`), not functions.
                        self.value_members
                            .insert(key, Type::Named(type_name.clone()));
                    } else {
                        self.fn_sigs.insert(
                            key,
                            FnSig {
                                params,
                                ret: Type::Named(type_name.clone()),
                                effects: vec![],
                            },
                        );
                    }
                }
            }
            TypeDef::Product {
                name: type_name,
                fields,
                ..
            } => {
                // Record constructors are handled via Expr::RecordCreate, not FnCall.
                // Register a dummy sig so Ident("TypeName") resolves to Named(type_name).
                let params: Vec<Type> = fields
                    .iter()
                    .map(|(_, ty_str)| parse_type_str_strict(ty_str).unwrap_or(Type::Unknown))
                    .collect();
                self.fn_sigs.insert(
                    type_name.clone(),
                    FnSig {
                        params,
                        ret: Type::Named(type_name.clone()),
                        effects: vec![],
                    },
                );
                // Register per-field types so dot-access is checked.
                for (field_name, ty_str) in fields {
                    let field_ty = parse_type_str_strict(ty_str).unwrap_or(Type::Unknown);
                    self.record_field_types
                        .insert(format!("{}.{}", type_name, field_name), field_ty);
                }
            }
        }
    }

    pub(super) fn module_decl(items: &[TopLevel]) -> Option<&Module> {
        items.iter().find_map(|item| {
            if let TopLevel::Module(m) = item {
                Some(m)
            } else {
                None
            }
        })
    }


    /// Extract a dotted path from an Expr (unwrapped, not Spanned).
    pub(super) fn attr_path(expr: &Expr) -> Option<Vec<String>> {
        match expr {
            Expr::Ident(name) => Some(vec![name.clone()]),
            Expr::Attr(inner, field) => {
                let mut parts = Self::attr_path(&inner.node)?;
                parts.push(field.clone());
                Some(parts)
            }
            _ => None,
        }
    }

    pub(super) fn attr_key(expr: &Expr) -> Option<String> {
        Self::attr_path(expr).map(|parts| parts.join("."))
    }

    pub(super) fn has_namespace_prefix(&self, key: &str) -> bool {
        let prefix = format!("{}.", key);
        self.fn_sigs.keys().any(|k| k.starts_with(&prefix))
            || self.value_members.keys().any(|k| k.starts_with(&prefix))
    }


    /// Integrate a loaded module's exported signatures into the checker.
    /// Called for each module returned by `load_module_tree` in dependency order.
    pub(super) fn integrate_module_sigs(
        &mut self,
        name: &str,
        items: &[TopLevel],
    ) -> Result<(), String> {
        let exports = crate::visibility::collect_module_exports(items);

        for fd in &exports.functions {
            let mut params = Vec::new();
            for (param_name, ty_str) in &fd.params {
                let ty = parse_type_str_strict(ty_str).map_err(|unknown| {
                    format!(
                        "Module '{}', function '{}': unknown type '{}' for parameter '{}'",
                        name, fd.name, unknown, param_name
                    )
                })?;
                params.push(ty);
            }

            let ret = parse_type_str_strict(&fd.return_type).map_err(|unknown| {
                format!(
                    "Module '{}', function '{}': unknown return type '{}'",
                    name, fd.name, unknown
                )
            })?;

            self.fn_sigs.insert(
                format!("{}.{}", name, fd.name),
                FnSig {
                    params,
                    ret,
                    effects: fd.effects.iter().map(|e| e.node.clone()).collect(),
                },
            );
        }

        for et in &exports.types {
            match et.def {
                TypeDef::Sum {
                    name: type_name,
                    variants,
                    ..
                } => {
                    if et.is_opaque {
                        self.fn_sigs.insert(
                            type_name.clone(),
                            FnSig {
                                params: vec![],
                                ret: Type::Named(type_name.clone()),
                                effects: vec![],
                            },
                        );
                        self.opaque_types.insert(type_name.clone());
                    } else {
                        self.type_variants.insert(
                            type_name.clone(),
                            variants.iter().map(|v| v.name.clone()).collect(),
                        );

                        for variant in variants {
                            let params: Vec<Type> = variant
                                .fields
                                .iter()
                                .map(|f| parse_type_str_strict(f).unwrap_or(Type::Unknown))
                                .collect();
                            let key = format!("{}.{}.{}", name, type_name, variant.name);
                            let alias = format!("{}.{}", type_name, variant.name);
                            self.sig_aliases.insert(alias, key.clone());
                            if params.is_empty() {
                                self.value_members
                                    .insert(key, Type::Named(type_name.clone()));
                            } else {
                                self.fn_sigs.insert(
                                    key,
                                    FnSig {
                                        params,
                                        ret: Type::Named(type_name.clone()),
                                        effects: vec![],
                                    },
                                );
                            }
                        }
                    }
                }
                TypeDef::Product {
                    name: type_name,
                    fields,
                    ..
                } => {
                    if et.is_opaque {
                        self.fn_sigs.insert(
                            type_name.clone(),
                            FnSig {
                                params: vec![],
                                ret: Type::Named(type_name.clone()),
                                effects: vec![],
                            },
                        );
                        self.opaque_types.insert(type_name.clone());
                    } else {
                        for (field_name, ty_str) in fields {
                            let field_ty =
                                parse_type_str_strict(ty_str).unwrap_or(Type::Unknown);
                            let key = format!("{}.{}.{}", name, type_name, field_name);
                            let alias = format!("{}.{}", type_name, field_name);
                            self.sig_aliases.insert(alias, key.clone());
                            self.record_field_types.insert(key, field_ty);
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
