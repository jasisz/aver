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

    pub(super) fn exposed_set(items: &[TopLevel]) -> Option<HashSet<String>> {
        Self::module_decl(items).and_then(|m| {
            if m.exposes.is_empty() {
                None
            } else {
                Some(m.exposes.iter().cloned().collect())
            }
        })
    }

    pub(super) fn module_cache_key(path: &Path) -> String {
        canonicalize_path(path).to_string_lossy().to_string()
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

    pub(super) fn cycle_display(loading: &[String], next: &str) -> String {
        let mut chain = loading
            .iter()
            .map(|key| {
                Path::new(key)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(key)
                    .to_string()
            })
            .collect::<Vec<_>>();
        chain.push(
            Path::new(next)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(next)
                .to_string(),
        );
        chain.join(" -> ")
    }

    pub(super) fn load_module_sigs(
        &mut self,
        name: &str,
        base_dir: &str,
        loading: &mut Vec<String>,
    ) -> Result<(), String> {
        let path = find_module_file(name, base_dir)
            .ok_or_else(|| format!("Module '{}' not found in '{}'", name, base_dir))?;
        let cache_key = Self::module_cache_key(&path);

        if let Some(cached) = self.module_sig_cache.get(&cache_key).cloned() {
            for (key, sig) in cached.fn_entries {
                self.fn_sigs.insert(key, sig);
            }
            for (key, ty) in cached.value_entries {
                self.value_members.insert(key, ty);
            }
            for (key, ty) in cached.record_field_entries {
                self.record_field_types.insert(key, ty);
            }
            for (type_name, variants) in cached.type_variants {
                self.type_variants.insert(type_name, variants);
            }
            for type_name in cached.opaque_types {
                self.opaque_types.insert(type_name);
            }
            return Ok(());
        }

        if loading.contains(&cache_key) {
            return Err(format!(
                "Circular import: {}",
                Self::cycle_display(loading, &cache_key)
            ));
        }

        loading.push(cache_key.clone());
        let result = (|| -> Result<ModuleSigCache, String> {
            let src = std::fs::read_to_string(&path)
                .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
            let items = parse_source(&src)
                .map_err(|e| format!("Parse error in '{}': {}", path.display(), e))?;
            require_module_declaration(&items, &path.to_string_lossy())?;
            if let Some(module) = Self::module_decl(&items) {
                let expected = name.rsplit('.').next().unwrap_or(name);
                if module.name != expected {
                    return Err(format!(
                        "Module name mismatch: expected '{}' (from '{}'), found '{}' in '{}'",
                        expected,
                        name,
                        module.name,
                        path.display()
                    ));
                }
                for dep_name in &module.depends {
                    self.load_module_sigs(dep_name, base_dir, loading)?;
                }
            }

            let exposed = Self::exposed_set(&items);
            let opaque_set: HashSet<String> = Self::module_decl(&items)
                .map(|m| m.exposes_opaque.iter().cloned().collect())
                .unwrap_or_default();
            let mut fn_entries = Vec::new();
            let mut value_entries = Vec::new();
            let mut record_field_entries = Vec::new();
            let mut type_variants = Vec::new();
            let mut opaque_types = Vec::new();
            for item in &items {
                if let TopLevel::FnDef(fd) = item {
                    let include = match &exposed {
                        Some(set) => set.contains(&fd.name),
                        None => !fd.name.starts_with('_'),
                    };
                    if !include {
                        continue;
                    }

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

                    fn_entries.push((
                        format!("{}.{}", name, fd.name),
                        FnSig {
                            params,
                            ret,
                            effects: fd.effects.iter().map(|e| e.node.clone()).collect(),
                        },
                    ));
                }
            }

            for item in &items {
                if let TopLevel::TypeDef(td) = item {
                    match td {
                        TypeDef::Sum {
                            name: type_name,
                            variants,
                            ..
                        } => {
                            let include = match &exposed {
                                Some(set) => {
                                    set.contains(type_name) || opaque_set.contains(type_name)
                                }
                                None => !type_name.starts_with('_'),
                            };
                            if !include {
                                continue;
                            }
                            let is_opaque = opaque_set.contains(type_name);

                            if is_opaque {
                                // Opaque: register dummy sig so the type name resolves,
                                // but do NOT register variants/constructors.
                                fn_entries.push((
                                    type_name.clone(),
                                    FnSig {
                                        params: vec![],
                                        ret: Type::Named(type_name.clone()),
                                        effects: vec![],
                                    },
                                ));
                                opaque_types.push(type_name.clone());
                            } else {
                                type_variants.push((
                                    type_name.clone(),
                                    variants.iter().map(|v| v.name.clone()).collect(),
                                ));

                                for variant in variants {
                                    let params: Vec<Type> = variant
                                        .fields
                                        .iter()
                                        .map(|f| parse_type_str_strict(f).unwrap_or(Type::Unknown))
                                        .collect();
                                    let key = format!("{}.{}.{}", name, type_name, variant.name);
                                    let alias_key = format!("{}.{}", type_name, variant.name);
                                    if params.is_empty() {
                                        let value_ty = Type::Named(type_name.clone());
                                        value_entries.push((key, value_ty.clone()));
                                        value_entries.push((alias_key, value_ty));
                                    } else {
                                        let sig = FnSig {
                                            params,
                                            ret: Type::Named(type_name.clone()),
                                            effects: vec![],
                                        };
                                        fn_entries.push((key, sig.clone()));
                                        fn_entries.push((alias_key, sig));
                                    }
                                }
                            }
                        }
                        TypeDef::Product {
                            name: type_name,
                            fields,
                            ..
                        } => {
                            let include = match &exposed {
                                Some(set) => {
                                    set.contains(type_name) || opaque_set.contains(type_name)
                                }
                                None => !type_name.starts_with('_'),
                            };
                            if !include {
                                continue;
                            }
                            let is_opaque = opaque_set.contains(type_name);

                            if is_opaque {
                                // Opaque: register dummy sig so the type name resolves,
                                // but do NOT register field types (blocks construction + field access).
                                fn_entries.push((
                                    type_name.clone(),
                                    FnSig {
                                        params: vec![],
                                        ret: Type::Named(type_name.clone()),
                                        effects: vec![],
                                    },
                                ));
                                opaque_types.push(type_name.clone());
                            } else {
                                for (field_name, ty_str) in fields {
                                    let field_ty =
                                        parse_type_str_strict(ty_str).unwrap_or(Type::Unknown);
                                    // Qualified key for explicit module paths.
                                    record_field_entries.push((
                                        format!("{}.{}.{}", name, type_name, field_name),
                                        field_ty.clone(),
                                    ));
                                    // Unqualified alias for common `Note.id` style.
                                    record_field_entries
                                        .push((format!("{}.{}", type_name, field_name), field_ty));
                                }
                            }
                        }
                    }
                }
            }

            Ok(ModuleSigCache {
                fn_entries,
                value_entries,
                record_field_entries,
                type_variants,
                opaque_types,
            })
        })();
        loading.pop();

        let cached = result?;
        for (key, sig) in &cached.fn_entries {
            self.fn_sigs.insert(key.clone(), sig.clone());
        }
        for (key, ty) in &cached.value_entries {
            self.value_members.insert(key.clone(), ty.clone());
        }
        for (key, ty) in &cached.record_field_entries {
            self.record_field_types.insert(key.clone(), ty.clone());
        }
        for (type_name, variants) in &cached.type_variants {
            self.type_variants
                .insert(type_name.clone(), variants.clone());
        }
        for type_name in &cached.opaque_types {
            self.opaque_types.insert(type_name.clone());
        }
        self.module_sig_cache.insert(cache_key, cached);
        Ok(())
    }
}
