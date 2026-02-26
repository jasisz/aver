use super::*;

impl TypeChecker {
    pub(super) fn build_signatures(&mut self, items: &[TopLevel]) {
        // Pass A: collect effect aliases so they can be expanded in function sigs below
        for item in items {
            if let TopLevel::EffectSet { name, effects } = item {
                self.effect_aliases.insert(name.clone(), effects.clone());
            }
        }

        // Pass B: register function signatures and type defs
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
                    // Expand effect aliases so effect checking works with concrete names
                    let effects = self.expand_effects(&f.effects);
                    self.fn_sigs.insert(
                        f.name.clone(),
                        FnSig {
                            params,
                            ret,
                            effects,
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
            } => {
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

    pub(super) fn attr_path(expr: &Expr) -> Option<Vec<String>> {
        match expr {
            Expr::Ident(name) => Some(vec![name.clone()]),
            Expr::Attr(inner, field) => {
                let mut parts = Self::attr_path(inner)?;
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

        if let Some(entries) = self.module_sig_cache.get(&cache_key).cloned() {
            for (key, sig) in entries {
                self.fn_sigs.insert(key, sig);
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
        let result = (|| -> Result<Vec<(String, FnSig)>, String> {
            let src = std::fs::read_to_string(&path)
                .map_err(|e| format!("Cannot read '{}': {}", path.display(), e))?;
            let items = parse_source(&src)
                .map_err(|e| format!("Parse error in '{}': {}", path.display(), e))?;
            let mut module_effect_aliases: HashMap<String, Vec<String>> = HashMap::new();
            for item in &items {
                if let TopLevel::EffectSet { name, effects } = item {
                    module_effect_aliases.insert(name.clone(), effects.clone());
                }
            }
            let expand_module_effects = |effects: &[String]| -> Vec<String> {
                let mut out = Vec::new();
                for effect in effects {
                    if let Some(expanded) = module_effect_aliases.get(effect) {
                        out.extend(expanded.iter().cloned());
                    } else {
                        out.push(effect.clone());
                    }
                }
                out
            };

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
            let mut entries = Vec::new();
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

                    entries.push((
                        format!("{}.{}", name, fd.name),
                        FnSig {
                            params,
                            ret,
                            effects: expand_module_effects(&fd.effects),
                        },
                    ));
                }
            }

            Ok(entries)
        })();
        loading.pop();

        let entries = result?;
        for (key, sig) in &entries {
            self.fn_sigs.insert(key.clone(), sig.clone());
        }
        self.module_sig_cache.insert(cache_key, entries);
        Ok(())
    }
}
