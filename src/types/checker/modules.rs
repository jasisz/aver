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
        // Phase B: prefer `current_module_prefix` (set by the
        // sub-checker driver to the dep module's `dep_name`) so the
        // canonical name aligns with the symbol table's `FnKey`. Fall
        // back to the interior `module X` declaration only at entry
        // scope where the symbol table doesn't use a module prefix
        // for items at all (entry items live under `FnKey::entry`).
        let module_name = self
            .current_module_prefix
            .clone()
            .or_else(|| Self::module_decl(items).map(|m| m.name.clone()))
            .unwrap_or_default();
        // Establish every local identity before resolving member annotations so
        // declaration order cannot change which nominal type an annotation names.
        for item in items {
            if let TopLevel::TypeDef(td) = item {
                let type_name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                };
                let canonical_type = canonical_name(&module_name, type_name);
                if let Some(id) = self.type_id_for_canonical(&canonical_type) {
                    self.mark_type_visible(id);
                    if canonical_type != *type_name {
                        self.merge_bare_type_alias(type_name.clone(), id);
                    }
                }
            }
        }
        for item in items {
            if let TopLevel::TypeDef(td) = item {
                self.register_type_def_sigs(td, &module_name);
            }
        }
        // Capability resources participate in ordinary function signatures:
        // hostile profiles for a resource-minting operation receive the fresh
        // resource as an explicit oracle parameter.  Register their nominal
        // identities before walking FnDefs, otherwise a perfectly valid local
        // profile such as `fn accept(..., fresh: Token)` is diagnosed as using
        // an unknown type merely because operation signatures used to be
        // installed last.
        self.register_capability_sigs();
        for item in items {
            if let TopLevel::FnDef(f) = item {
                let mut params = Vec::new();
                for (param_name, ty_str) in &f.params {
                    match parse_type_str_strict(ty_str) {
                        Ok(ty) => {
                            let canon = self.canonicalize_named(ty);
                            let ctx = format!("Function '{}', parameter '{}'", f.name, param_name);
                            self.report_ambiguous_named(&canon, f.line, &ctx);
                            // A parameter MAY be a `Fn(...)` callback (the one
                            // sanctioned first-class position) — but a `Fn`
                            // nested inside it is still rejected.
                            self.reject_fn_in_type(&canon, true, f.line, &ctx);
                            params.push(canon);
                        }
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
                    Ok(ty) => {
                        let canon = self.canonicalize_named(ty);
                        let ctx = format!("Function '{}' return type", f.name);
                        self.report_ambiguous_named(&canon, f.line, &ctx);
                        self.reject_fn_in_type(&canon, false, f.line, &ctx);
                        canon
                    }
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
                if self.fn_sig_contains_canonical(&canonical) {
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
                // Phase B: routing handled by `insert_fn_sig` — user
                // fns land in the `FnId`-keyed `fn_sigs`.
                self.insert_fn_sig(&canonical, sig);
            }
        }
    }

    /// Register provider-bound operations as callable signatures. They stay
    /// outside the `FnId` table because they have no Aver body; the resolver
    /// classifies them through `SymbolTable::capability_operation` instead.
    pub(super) fn register_capability_sigs(&mut self) {
        let resource_types: Vec<_> = self.capabilities.resource_types().cloned().collect();
        for resource in resource_types {
            self.opaque_types.insert(resource.clone());

            let Some((module, name)) = resource.rsplit_once('.') else {
                continue;
            };
            let module_id = self
                .symbol_table
                .type_id_of(&TypeKey::in_module(module, name));
            let entry_id = self.symbol_table.type_id_of(&TypeKey::entry(name));
            let id = module_id.or(entry_id);
            if let Some(id) = id {
                // A representation-less resource is nameable because its
                // operation signature exposes it, but the resource-access gates
                // still forbid construction, field access, and matching.
                self.mark_type_visible(id);
                let is_own_scope = self.current_module_prefix.as_deref() == Some(module)
                    || (self.current_module_prefix.is_none() && module_id.is_none());
                if is_own_scope {
                    self.merge_bare_type_alias(name.to_string(), id);
                }
            }
        }

        let current_scope = self.current_module_prefix.as_deref();
        let operations: Vec<_> = self.capabilities.operations().cloned().collect();
        for operation in operations {
            let visible = current_scope == Some(operation.module.as_str()) || operation.exposed;
            if !visible {
                continue;
            }
            let params = operation
                .params
                .iter()
                .map(|(_, ty)| self.canonicalize_named_in_module(ty.clone(), &operation.module))
                .collect();
            let ret =
                self.canonicalize_named_in_module(operation.return_type.clone(), &operation.module);
            let effects = if operation.is_effectful() {
                vec![operation.canonical_name.clone()]
            } else {
                Vec::new()
            };
            self.extra_sigs.insert(
                operation.canonical_name,
                FnSig {
                    params,
                    ret,
                    effects,
                },
            );
        }
    }

    /// Build a `Type::Named` for `type_name` declared inside
    /// `module_name`. Keeps the `name` field source-faithful (bare
    /// `type_name`, matching pre-phase-B's stamping convention) and
    /// populates `id` from the symbol table. Entry items declaring
    /// `module X` live under `TypeKey::entry` in the symbol table,
    /// so we probe entry scope as a fallback when the module-scoped
    /// lookup misses.
    pub(super) fn resolved_named_type(&self, type_name: &str, module_name: &str) -> Type {
        let id = if module_name.is_empty() {
            self.symbol_table
                .type_id_of(&crate::ir::TypeKey::entry(type_name))
        } else {
            self.symbol_table
                .type_id_of(&crate::ir::TypeKey::in_module(module_name, type_name))
                .or_else(|| {
                    self.symbol_table
                        .type_id_of(&crate::ir::TypeKey::entry(type_name))
                })
        };
        Type::Named {
            id,
            name: type_name.to_string(),
        }
    }

    /// Phase B: rewrite every `Type::Named { id: None, name: bare }`
    /// reachable from `ty` so that `id` and `name` reflect the
    /// canonical form resolved through the symbol table. Leaves the
    /// structure intact otherwise — `Type::Var`, primitives,
    /// `Type::List<Bare>` (recurse into inner), etc.
    /// Owner-aware variant of `canonicalize_named`. Bare `Type::Named`
    /// references inside `ty` resolve in `owner_module`'s **own
    /// resolver context**: the owner's own types plus the
    /// visibility-exposed types from each module the owner itself
    /// declares in its `depends [...]`. The contrast with
    /// `canonicalize_named` (the importer-context variant) is the
    /// whole point — peer review round 6 caught the regression where
    /// B's exported `fn pass(s: Shape) -> Shape` (with `Shape`
    /// meaning A.Shape per B's depends [A]) silently resolved against
    /// the importer's bare alias map, which was ambiguous between A
    /// and an unrelated sibling C the entry happened to also depend
    /// on. Routing the resolution through B's own depends shuts
    /// siblings out.
    ///
    /// Three resolution cases, in order:
    ///
    /// 1. Qualified form (`Module.Type`): trust the prefix, but the
    ///    referenced type must be exposed by the named module unless
    ///    the owner is the named module itself.
    /// 2. Bare in owner's own scope.
    /// 3. Bare in any of owner's own `depends [...]` — exposed
    ///    types only.
    ///
    /// No entry-scope fallback for dep modules: a bare reference in
    /// a dep that doesn't resolve via the above stays unresolved
    /// rather than silently binding to an unrelated `Shape` in the
    /// entry module.
    pub(super) fn canonicalize_named_in_module(&self, ty: Type, owner_module: &str) -> Type {
        match ty {
            // Peer review round 7: `Some(id)` is sacred even in the
            // owner-context canonicaliser. Never overwrite an
            // existing typed-identity stamp.
            Type::Named {
                id: Some(existing),
                name,
            } => Type::Named {
                id: Some(existing),
                name,
            },
            Type::Named { id: None, name } => {
                match self.resolve_in_owner_context(&name, owner_module) {
                    Some(resolved_id) => Type::Named {
                        id: Some(resolved_id),
                        name,
                    },
                    None => Type::Named { id: None, name },
                }
            }
            Type::List(inner) => Type::List(Box::new(
                self.canonicalize_named_in_module(*inner, owner_module),
            )),
            Type::Vector(inner) => Type::Vector(Box::new(
                self.canonicalize_named_in_module(*inner, owner_module),
            )),
            Type::Option(inner) => Type::Option(Box::new(
                self.canonicalize_named_in_module(*inner, owner_module),
            )),
            Type::Result(ok, err) => Type::Result(
                Box::new(self.canonicalize_named_in_module(*ok, owner_module)),
                Box::new(self.canonicalize_named_in_module(*err, owner_module)),
            ),
            Type::Map(k, v) => Type::Map(
                Box::new(self.canonicalize_named_in_module(*k, owner_module)),
                Box::new(self.canonicalize_named_in_module(*v, owner_module)),
            ),
            Type::Tuple(items) => Type::Tuple(
                items
                    .into_iter()
                    .map(|t| self.canonicalize_named_in_module(t, owner_module))
                    .collect(),
            ),
            Type::Fn(params, ret, effects) => Type::Fn(
                params
                    .into_iter()
                    .map(|t| self.canonicalize_named_in_module(t, owner_module))
                    .collect(),
                Box::new(self.canonicalize_named_in_module(*ret, owner_module)),
                effects,
            ),
            other => other,
        }
    }

    /// Resolve a single type name in `owner_module`'s context. See
    /// `canonicalize_named_in_module` for the three-case ordering.
    fn resolve_in_owner_context(&self, name: &str, owner_module: &str) -> Option<TypeId> {
        self.symbol_table
            .resolve_type_id_in(name, Some(owner_module))
    }

    pub(super) fn canonicalize_named(&self, ty: Type) -> Type {
        // Phase B: keep the `name` field source-faithful (matches
        // pre-phase-B `Type::Named(bare)` behaviour — backend codegen
        // that does string-equality lookups against its own type
        // registry needs to keep seeing what the user wrote). The
        // `id` field carries the typed identity; the matcher uses it
        // when both sides have `Some` and falls back to source-name
        // matching otherwise.
        //
        // Peer review round 7: once a stamp carries `Some(id)`, no
        // later context may overwrite it. Pre-fix the canonicaliser
        // re-stamped any `Some` value by `resolve_type_id(name)` in
        // the current scope, which let an importer's bare alias for
        // `Shape` re-bind a properly-stamped `C.Shape` value to
        // `A.Shape`. `Some(_)` is sacred; only fill in `id: None`.
        match ty {
            Type::Named {
                id: Some(existing),
                name,
            } => Type::Named {
                id: Some(existing),
                name,
            },
            Type::Named { id: None, name } => match self.resolve_type_id(&name) {
                Some(resolved_id) => Type::Named {
                    id: Some(resolved_id),
                    name,
                },
                None => Type::Named { id: None, name },
            },
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
                line,
            } => {
                let canonical_type = canonical_name(module_name, type_name);
                // Iron-followup: duplicate type defs in the same module
                // used to silently overwrite via `HashMap::insert`,
                // leaving the VM symbol table half-populated with the
                // first variant set and trying to register the second.
                // `fuzz_verify_runner` crash id:000001 minimised to a
                // `type Tree ... \n type Tree he canonical Lea` shape
                // that panicked at `vm/compiler/mod.rs:531 ctor id`
                // because the arena's variant list belonged to the
                // first decl but the symbol path used the second.
                // Reject the duplicate at typecheck so the VM never
                // sees the inconsistency.
                if self.fn_sig_contains_canonical(&canonical_type) {
                    self.error_at_line(
                        *line,
                        format!("Type '{}' is already defined in this module", type_name),
                    );
                    return;
                }
                // Iron-followup: same module-level rule applies inside
                // a single type: two variants with the same name make
                // the VM symbol table register one ctor key under two
                // distinct (variant_id, ctor_id) pairs, which the
                // intern assertion catches as a panic
                // (`vm/symbol.rs:205` — `fuzz_verify_runner` crash
                // id:000000). Reject the duplicate at typecheck.
                let mut seen_variants: std::collections::HashSet<&str> =
                    std::collections::HashSet::new();
                for variant in variants {
                    if !seen_variants.insert(variant.name.as_str()) {
                        self.error_at_line(
                            *line,
                            format!(
                                "Type '{}': variant '{}' is declared more than once",
                                type_name, variant.name
                            ),
                        );
                    }
                }
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
                // Phase B: type-as-callable goes through the unified
                // `insert_fn_sig` router. User types land in `fn_sigs`
                // keyed by `FnId` only if the type also names a
                // function — they generally don't, so this falls
                // through to `extra_sigs`. The bare-alias mirror that
                // pre-phase-B `sig_aliases` carried is subsumed by
                // `canonical_extra_key` doing the symbol-table type
                // resolution on lookup.
                let type_sig = FnSig {
                    params: vec![],
                    ret: self.resolved_named_type(type_name, module_name),
                    effects: vec![],
                };
                self.insert_fn_sig(&canonical_type, type_sig);
                // Visibility and bare aliases are registered by build_signatures.
                // Register each constructor with a qualified key.
                for variant in variants {
                    let params: Vec<Type> = variant
                        .fields
                        .iter()
                        .map(|f| {
                            let canon = self.canonicalize_named(
                                parse_type_str_strict(f).unwrap_or(Type::Invalid),
                            );
                            let ctx = format!("Type '{}', variant '{}'", type_name, variant.name);
                            self.report_ambiguous_named(&canon, *line, &ctx);
                            self.reject_fn_in_type(&canon, false, *line, &ctx);
                            canon
                        })
                        .collect();
                    let alias_key = crate::visibility::member_key(type_name, &variant.name);
                    let canonical_key = canonical_name(module_name, &alias_key);
                    if params.is_empty() {
                        self.value_members.insert(
                            canonical_key.clone(),
                            self.resolved_named_type(type_name, module_name),
                        );
                    } else {
                        self.insert_fn_sig(
                            &canonical_key,
                            FnSig {
                                params,
                                ret: self.resolved_named_type(type_name, module_name),
                                effects: vec![],
                            },
                        );
                    }
                }
            }
            TypeDef::Product {
                name: type_name,
                fields,
                line,
            } => {
                let canonical_type = canonical_name(module_name, type_name);
                // Same duplicate-type rule as the Sum arm — a record
                // re-declared in the same module would silently
                // overwrite the first via `HashMap::insert` and leave
                // downstream consumers (codegen, VM compiler, refinement
                // detector) reading whichever copy won the race.
                if self.fn_sig_contains_canonical(&canonical_type) {
                    self.error_at_line(
                        *line,
                        format!("Type '{}' is already defined in this module", type_name),
                    );
                    return;
                }
                // Duplicate field names in a record — same hazard the
                // Sum-variant check guards against, just on the
                // product side.
                let mut seen_fields: std::collections::HashSet<&str> =
                    std::collections::HashSet::new();
                for (fname, _) in fields {
                    if !seen_fields.insert(fname.as_str()) {
                        self.error_at_line(
                            *line,
                            format!(
                                "Type '{}': field '{}' is declared more than once",
                                type_name, fname
                            ),
                        );
                    }
                }
                // Record constructors flow through `Expr::RecordCreate`.
                // The fn_sigs-like entry lets `Ident("TypeName")` resolve
                // to a `Type::Named`-shaped callable for diagnostic use.
                let params: Vec<Type> = fields
                    .iter()
                    .map(|(_, ty_str)| {
                        self.canonicalize_named(
                            parse_type_str_strict(ty_str).unwrap_or(Type::Invalid),
                        )
                    })
                    .collect();
                let prod_sig = FnSig {
                    params,
                    ret: self.resolved_named_type(type_name, module_name),
                    effects: vec![],
                };
                self.insert_fn_sig(&canonical_type, prod_sig);
                // Visibility and bare aliases are registered by build_signatures.
                // Register per-field types so dot-access is checked.
                // Phase B: single entry under canonical `(Module.Type,
                // field)` — the bare-alias mirror that pre-phase-B
                // `sig_aliases` carried is subsumed by
                // `canonical_type_name` resolving through the symbol
                // table on lookup.
                for (field_name, ty_str) in fields {
                    let field_ty = self
                        .canonicalize_named(parse_type_str_strict(ty_str).unwrap_or(Type::Invalid));
                    let ctx = format!("Type '{}', field '{}'", type_name, field_name);
                    self.report_ambiguous_named(&field_ty, *line, &ctx);
                    self.reject_fn_in_type(&field_ty, false, *line, &ctx);
                    let canonical_type = if module_name != type_name {
                        canonical_name(module_name, type_name)
                    } else {
                        type_name.clone()
                    };
                    self.record_field_types
                        .insert(RecordFieldKey::new(&canonical_type, field_name), field_ty);
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
        self.all_fn_sigs().any(|(k, _)| k.starts_with(&prefix))
            || self.value_members.keys().any(|k| k.starts_with(&prefix))
    }

    /// Populate checker maps from the shared SymbolRegistry. The
    /// registry is the canonical source — checker derives its maps
    /// from it.
    ///
    /// Phase B: registry entries are stored under their canonical
    /// name (`"Module.Type"`, `"Module.fn"`, `"Module.Type.field"`).
    /// The bare → canonical alias map that pre-phase-B code
    /// maintained (`sig_aliases`) is gone; instead, lookups
    /// canonicalise through the `SymbolTable` (`resolve_type_id` /
    /// `resolve_fn_id`) at read time. Per-fn / per-type duplicate
    /// mirror entries are gone too — `insert_fn_sig` routes user fns
    /// into `fn_sigs` (FnId-keyed) and leaves builtins/constructors
    /// in `extra_sigs` (string-keyed).
    pub(super) fn integrate_registry(
        &mut self,
        registry: &crate::visibility::SymbolRegistry,
    ) -> Result<(), String> {
        use crate::visibility::SymbolKind;

        // First pass: populate the bare → typed-id alias maps AND the
        // visibility sets from every registry entry. The registry
        // itself is already filtered to exposed-only items by
        // `SymbolRegistry::from_modules`, so every `FnId` / `TypeId`
        // we see here is by definition visible to the current
        // checker. The visibility gating in `resolve_*_id` then
        // refuses look-ups against any ID that the symbol table has
        // but this pass never touched — closing the
        // qualified-private-import leak (peer review round 4).
        for entry in &registry.entries {
            match &entry.kind {
                SymbolKind::Function { name: fn_name, .. } => {
                    if let Some(id) = self
                        .symbol_table
                        .fn_id_of(&FnKey::in_module(entry.module.clone(), fn_name.clone()))
                    {
                        self.visible_fn_ids.insert(id);
                    }
                }
                SymbolKind::OpaqueType { name }
                | SymbolKind::SumType { name, .. }
                | SymbolKind::ProductType { name, .. } => {
                    if let Some(id) = self
                        .symbol_table
                        .type_id_of(&TypeKey::in_module(entry.module.clone(), name.clone()))
                    {
                        self.visible_type_ids.insert(id);
                        if let Some(alias) = entry.alias.as_deref() {
                            self.merge_bare_type_alias(alias.to_string(), id);
                        }
                    }
                }
                SymbolKind::Constructor { .. } | SymbolKind::RecordField { .. } => {
                    // Constructors / record fields aren't part of
                    // SymbolTable's fn/type id space — they're keyed
                    // by `CtorId` under their owning type. Their bare
                    // aliases stay routed through the canonical
                    // string keys in `extra_sigs` (handled by
                    // `find_fn_sig` falling back on direct lookup).
                }
            }
        }

        for entry in &registry.entries {
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
                        // Phase B (peer review round 5): canonicalise
                        // dep module sigs in the OWNER's scope so a
                        // bare `Shape` in B's `fn make() -> Shape`
                        // stamps as B.Shape regardless of what `Shape`
                        // means in the importing module.
                        parsed_params.push(self.canonicalize_named_in_module(ty, &entry.module));
                    }
                    let ret_raw = parse_type_str_strict(return_type).map_err(|unknown| {
                        format!(
                            "Module '{}', function '{}': unknown return type '{}'",
                            entry.module, fn_name, unknown
                        )
                    })?;
                    let ret = self.canonicalize_named_in_module(ret_raw, &entry.module);
                    self.insert_fn_sig(
                        &entry.canonical_name,
                        FnSig {
                            params: parsed_params,
                            ret,
                            effects: effects.clone(),
                        },
                    );
                }
                SymbolKind::OpaqueType { name } => {
                    let canonical = entry.canonical_name.clone();
                    self.insert_fn_sig(
                        &canonical,
                        FnSig {
                            params: vec![],
                            ret: self.resolved_named_type(name, &entry.module),
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
                    self.insert_fn_sig(
                        &entry.canonical_name,
                        FnSig {
                            params: vec![],
                            ret: self.resolved_named_type(name, &entry.module),
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
                        .map(|f| {
                            self.canonicalize_named_in_module(
                                parse_type_str_strict(f).unwrap_or(Type::Invalid),
                                &entry.module,
                            )
                        })
                        .collect();
                    if params.is_empty() {
                        self.value_members.insert(
                            entry.canonical_name.clone(),
                            self.resolved_named_type(type_name, &entry.module),
                        );
                    } else {
                        self.insert_fn_sig(
                            &entry.canonical_name,
                            FnSig {
                                params,
                                ret: self.resolved_named_type(type_name, &entry.module),
                                effects: vec![],
                            },
                        );
                    }
                }
                SymbolKind::RecordField { field_type, .. } => {
                    let field_ty = self.canonicalize_named_in_module(
                        parse_type_str_strict(field_type).unwrap_or(Type::Invalid),
                        &entry.module,
                    );
                    let canonical = &entry.canonical_name;
                    if let Some((canonical_type, field_name)) = canonical.rsplit_once('.') {
                        self.record_field_types
                            .insert(RecordFieldKey::new(canonical_type, field_name), field_ty);
                    }
                }
            }
        }
        Ok(())
    }
}
