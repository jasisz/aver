use super::*;

/// Iron — A3: build a canonical "Module.Identifier" key, falling back
/// to the bare identifier when there is no surrounding module (REPL,
/// inline test programs, anonymous fixtures). Aver source allows
/// dotted module names like `Tcp.Connection`, so this helper just
/// concatenates with a single separator without sanitising.
fn canonical_name(module_name: &str, identifier: &str) -> String {
    if module_name.is_empty() {
        identifier.to_string()
    } else {
        crate::visibility::qualified_name(module_name, identifier)
    }
}

impl TypeChecker {
    pub(super) fn build_signatures(&mut self, items: &[TopLevel]) {
        // Iron — A3: every name that appears in `fn_sigs` /
        // `type_variants` / `value_members` / `record_field_types` is
        // registered under its **canonical** form
        // (`Module.Identifier`). Bare references in the surrounding
        // module's own source still need to resolve, so we mirror the
        // bare → canonical mapping into `sig_aliases` and rely on the
        // `find_*` helpers to chase the alias. Loaded dependency
        // modules go through `integrate_registry` which already does
        // the same thing via `SymbolEntry::alias`; this path is the
        // own-module counterpart.
        //
        // Two passes: TypeDefs first to populate the bare→canonical
        // alias map, then FnDefs so their param/return type
        // annotations get rewritten to canonical via that map. Doing
        // it in one pass would leave fns whose annotations reference
        // a later-in-source type with a bare `Type::Named`, and the
        // strict matcher would then reject otherwise-correct
        // programs.
        let module_name = Self::module_decl(items)
            .map(|m| m.name.clone())
            .unwrap_or_default();
        for item in items {
            if let TopLevel::TypeDef(td) = item {
                self.register_type_def_sigs(td, &module_name);
            }
        }
        for item in items {
            if let TopLevel::FnDef(f) = item {
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
                let canonical = canonical_name(&module_name, &f.name);
                // Iron — A2: refuse silent shadowing.
                if self.fn_sigs.contains_key(&canonical) {
                    self.error_at_line(
                        f.line,
                        format!("Function '{}' is already defined in this module", f.name),
                    );
                }
                let sig = FnSig {
                    params,
                    ret,
                    effects: f.effects.iter().map(|e| e.node.clone()).collect(),
                };
                self.fn_sigs.insert(canonical.clone(), sig.clone());
                if canonical != f.name {
                    self.sig_aliases.insert(f.name.clone(), canonical);
                    self.fn_sigs.insert(f.name.clone(), sig);
                }
            }
        }
    }

    /// Iron — A3: rewrite every `Type::Named(bare)` reachable from
    /// `ty` to `Type::Named(canonical)` when `bare` has a registered
    /// alias. Leaves the structure intact otherwise — `Type::Var`,
    /// primitives, `Type::List<Bare>` (recurse into inner), etc.
    pub(super) fn canonicalize_named(&self, ty: Type) -> Type {
        match ty {
            Type::Named(name) => {
                let resolved = self.sig_aliases.get(&name).cloned().unwrap_or(name);
                Type::Named(resolved)
            }
            Type::List(inner) => Type::List(Box::new(self.canonicalize_named(*inner))),
            Type::Vector(inner) => Type::Vector(Box::new(self.canonicalize_named(*inner))),
            Type::Option(inner) => Type::Option(Box::new(self.canonicalize_named(*inner))),
            Type::Result(ok, err) => Type::Result(
                Box::new(self.canonicalize_named(*ok)),
                Box::new(self.canonicalize_named(*err)),
            ),
            Type::Map(k, v) => Type::Map(
                Box::new(self.canonicalize_named(*k)),
                Box::new(self.canonicalize_named(*v)),
            ),
            Type::Tuple(items) => Type::Tuple(
                items
                    .into_iter()
                    .map(|t| self.canonicalize_named(t))
                    .collect(),
            ),
            Type::Fn(params, ret, effects) => Type::Fn(
                params
                    .into_iter()
                    .map(|t| self.canonicalize_named(t))
                    .collect(),
                Box::new(self.canonicalize_named(*ret)),
                effects,
            ),
            other => other,
        }
    }

    /// Register constructor signatures for user-defined types.
    pub(super) fn register_type_def_sigs(&mut self, td: &TypeDef, module_name: &str) {
        match td {
            TypeDef::Sum {
                name: type_name,
                variants,
                ..
            } => {
                let canonical_type = canonical_name(module_name, type_name);
                let variant_names: Vec<String> = variants.iter().map(|v| v.name.clone()).collect();
                // Register variant names for exhaustiveness under both
                // the canonical and bare keys — exhaustiveness reads
                // by `Type::Named(name)` and `name` may be either form
                // depending on which side of a cross-module boundary
                // built the type stamp.
                self.type_variants
                    .insert(canonical_type.clone(), variant_names.clone());
                if canonical_type != *type_name {
                    self.type_variants.insert(type_name.clone(), variant_names);
                }
                // Iron — A3: fn_sigs values stay source-faithful
                // (`Type::Named(bare)`) so downstream discovery /
                // codegen walkers see what the user wrote; the
                // `sig_aliases` map carries bare → canonical for the
                // matcher to resolve at comparison time.
                let type_sig = FnSig {
                    params: vec![],
                    ret: Type::Named(type_name.clone()),
                    effects: vec![],
                };
                self.fn_sigs
                    .insert(canonical_type.clone(), type_sig.clone());
                if canonical_type != *type_name {
                    self.sig_aliases
                        .insert(type_name.clone(), canonical_type.clone());
                    self.fn_sigs.insert(type_name.clone(), type_sig);
                }
                // Register each constructor with a qualified key.
                for variant in variants {
                    let params: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| parse_type_str_strict(f).unwrap_or(Type::Invalid))
                        .collect();
                    let alias_key = crate::visibility::member_key(type_name, &variant.name);
                    let canonical_key = canonical_name(module_name, &alias_key);
                    if params.is_empty() {
                        self.value_members
                            .insert(canonical_key.clone(), Type::Named(type_name.clone()));
                    } else {
                        self.fn_sigs.insert(
                            canonical_key.clone(),
                            FnSig {
                                params,
                                ret: Type::Named(type_name.clone()),
                                effects: vec![],
                            },
                        );
                    }
                    if canonical_key != alias_key {
                        self.sig_aliases.insert(alias_key, canonical_key);
                    }
                }
            }
            TypeDef::Product {
                name: type_name,
                fields,
                ..
            } => {
                let canonical_type = canonical_name(module_name, type_name);
                // Record constructors are handled via Expr::RecordCreate
                // — fn_sigs entry exists so `Ident("TypeName")` resolves
                // to `Type::Named(bare)` (Iron — A3: source-faithful).
                let params: Vec<Type> = fields
                    .iter()
                    .map(|(_, ty_str)| parse_type_str_strict(ty_str).unwrap_or(Type::Invalid))
                    .collect();
                let prod_sig = FnSig {
                    params,
                    ret: Type::Named(type_name.clone()),
                    effects: vec![],
                };
                self.fn_sigs
                    .insert(canonical_type.clone(), prod_sig.clone());
                if canonical_type != *type_name {
                    self.sig_aliases
                        .insert(type_name.clone(), canonical_type.clone());
                    self.fn_sigs.insert(type_name.clone(), prod_sig);
                }
                // Register per-field types so dot-access is checked.
                // Dual-keyed (canonical + bare) so the `Type.field`
                // prefix-strip pass in `infer/records.rs` sees the
                // bare keys while `find_record_field` lookups still
                // chase the canonical via `sig_aliases`.
                for (field_name, ty_str) in fields {
                    let field_ty = parse_type_str_strict(ty_str).unwrap_or(Type::Invalid);
                    let alias_key = crate::visibility::member_key(type_name, field_name);
                    let canonical_key = canonical_name(module_name, &alias_key);
                    self.record_field_types
                        .insert(canonical_key.clone(), field_ty.clone());
                    if canonical_key != alias_key {
                        self.sig_aliases.insert(alias_key.clone(), canonical_key);
                        self.record_field_types.insert(alias_key, field_ty);
                    }
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

        // Iron — A3: aliases first. `canonicalize_named` walks
        // `sig_aliases` to rewrite bare references in fn/variant
        // type annotations, and entries come from the registry in
        // (function, type, constructor) order — without this pre-
        // pass, a function whose param type names a type defined
        // later in the same module's entry list would canonicalize
        // to the bare name and the strict matcher would then reject
        // any call site that uses the canonical form.
        for entry in &registry.entries {
            if let Some(alias) = &entry.alias
                && matches!(
                    entry.kind,
                    SymbolKind::OpaqueType { .. }
                        | SymbolKind::SumType { .. }
                        | SymbolKind::ProductType { .. }
                )
            {
                self.sig_aliases
                    .insert(alias.clone(), entry.canonical_name.clone());
            }
        }

        for entry in &registry.entries {
            if let Some(alias) = &entry.alias
                && !matches!(
                    entry.kind,
                    SymbolKind::OpaqueType { .. }
                        | SymbolKind::SumType { .. }
                        | SymbolKind::ProductType { .. }
                )
            {
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
                    // Iron — A3: fn_sigs values stay source-faithful
                    // (bare `Type::Named(name)`); the bare alias is in
                    // `sig_aliases` for matcher resolution.
                    let canonical = entry.canonical_name.clone();
                    self.fn_sigs.insert(
                        canonical.clone(),
                        FnSig {
                            params: vec![],
                            ret: Type::Named(name.clone()),
                            effects: vec![],
                        },
                    );
                    self.opaque_types.insert(canonical);
                }
                SymbolKind::SumType { variants, .. } => {
                    let canonical = entry.canonical_name.clone();
                    self.type_variants.insert(canonical, variants.clone());
                }
                SymbolKind::ProductType { name, .. } => {
                    let canonical = entry.canonical_name.clone();
                    self.fn_sigs.insert(
                        canonical,
                        FnSig {
                            params: vec![],
                            ret: Type::Named(name.clone()),
                            effects: vec![],
                        },
                    );
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
                        .insert(entry.canonical_name.clone(), field_ty.clone());
                    // Mirror to the bare alias key so the
                    // `Type.field` prefix-strip pass in
                    // `infer/records.rs` reaches imported records too.
                    if let Some(alias) = &entry.alias {
                        self.record_field_types.insert(alias.clone(), field_ty);
                    }
                }
            }
        }
        Ok(())
    }
}
