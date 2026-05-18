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
                                params.push(Type::Invalid);
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
                            Type::Invalid
                        }
                    };
                    // Iron — A2: refuse silent shadowing. Pre-Iron the
                    // second `fn foo` simply overwrote the first in
                    // `fn_sigs`; the program then type-checked + ran the
                    // second definition's body, the first was dead code
                    // the user almost certainly didn't realise was being
                    // dropped. Now an explicit error fires at the
                    // duplicate's source line; the entry is still
                    // overwritten so downstream checking proceeds against
                    // the latest definition rather than poisoning every
                    // subsequent call site with a fresh error.
                    //
                    // Scope note: this check fires within a single
                    // `items` list (one module). Cross-module clashes
                    // are surfaced by the visibility / SymbolRegistry
                    // layer at integration time, not here.
                    if self.fn_sigs.contains_key(&f.name) {
                        self.error_at_line(
                            f.line,
                            format!(
                                "Function '{}' is already defined in this module",
                                f.name
                            ),
                        );
                    }
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
                        .map(|f| parse_type_str_strict(f).unwrap_or(Type::Invalid))
                        .collect();
                    let key = crate::visibility::member_key(type_name, &variant.name);
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
                    .map(|(_, ty_str)| parse_type_str_strict(ty_str).unwrap_or(Type::Invalid))
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
                    let field_ty = parse_type_str_strict(ty_str).unwrap_or(Type::Invalid);
                    self.record_field_types.insert(
                        crate::visibility::member_key(type_name, field_name),
                        field_ty,
                    );
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

    /// Populate checker maps from the shared SymbolRegistry.
    /// The registry is the canonical source — checker derives its maps from it.
    pub(super) fn integrate_registry(
        &mut self,
        registry: &crate::visibility::SymbolRegistry,
    ) -> Result<(), String> {
        use crate::visibility::SymbolKind;

        for entry in &registry.entries {
            if let Some(alias) = &entry.alias {
                self.sig_aliases
                    .insert(alias.clone(), entry.canonical_name.clone());
            }

            match &entry.kind {
                SymbolKind::Function {
                    name: fn_name,
                    params,
                    return_type,
                    effects,
                } => {
                    let mut parsed_params = Vec::new();
                    for (param_name, ty_str) in params {
                        let ty = parse_type_str_strict(ty_str).map_err(|unknown| {
                            format!(
                                "Module '{}', function '{}': unknown type '{}' for parameter '{}'",
                                entry.module, fn_name, unknown, param_name
                            )
                        })?;
                        parsed_params.push(ty);
                    }
                    let ret = parse_type_str_strict(return_type).map_err(|unknown| {
                        format!(
                            "Module '{}', function '{}': unknown return type '{}'",
                            entry.module, fn_name, unknown
                        )
                    })?;
                    self.fn_sigs.insert(
                        entry.canonical_name.clone(),
                        FnSig {
                            params: parsed_params,
                            ret,
                            effects: effects.clone(),
                        },
                    );
                }
                SymbolKind::OpaqueType { name } => {
                    self.fn_sigs.insert(
                        name.clone(),
                        FnSig {
                            params: vec![],
                            ret: Type::Named(name.clone()),
                            effects: vec![],
                        },
                    );
                    self.opaque_types.insert(name.clone());
                }
                SymbolKind::SumType { name, variants } => {
                    self.type_variants.insert(name.clone(), variants.clone());
                }
                SymbolKind::Constructor {
                    type_name,
                    field_types,
                    ..
                } => {
                    let params: Vec<Type> = field_types
                        .iter()
                        .map(|f| parse_type_str_strict(f).unwrap_or(Type::Invalid))
                        .collect();
                    if params.is_empty() {
                        self.value_members
                            .insert(entry.canonical_name.clone(), Type::Named(type_name.clone()));
                    } else {
                        self.fn_sigs.insert(
                            entry.canonical_name.clone(),
                            FnSig {
                                params,
                                ret: Type::Named(type_name.clone()),
                                effects: vec![],
                            },
                        );
                    }
                }
                SymbolKind::RecordField { field_type, .. } => {
                    let field_ty = parse_type_str_strict(field_type).unwrap_or(Type::Invalid);
                    self.record_field_types
                        .insert(entry.canonical_name.clone(), field_ty);
                }
            }
        }
        Ok(())
    }
}
