use super::*;

impl TypeChecker {
    pub(super) fn check(&mut self, items: &[TopLevel], base_dir: Option<&str>) {
        // Phase B: track the entry module's prefix so bare-name
        // resolution in `resolve_fn_id` / `resolve_type_id` knows
        // which scope to try first.
        self.current_module_prefix = Self::module_decl(items).map(|m| m.name.clone());
        // Load dependency modules first. Capability contracts are registered
        // from the WHOLE closure before any signature or verify-flow check;
        // otherwise an operation in a dependency would classify as an unknown
        // fn/effect (the same entry-scoped gap `proof` used to have).
        // maps are populated before `build_signatures` resolves local
        // fn / type annotations.
        let mut loaded_modules: Vec<crate::source::LoadedModule> = Vec::new();
        if let Some(base) = base_dir
            && let Some(module) = Self::module_decl(items)
        {
            let mut roots = module.depends.clone();
            roots.extend(crate::stdlib::implicit_stdlib_deps(items));
            roots.sort();
            roots.dedup();
            match crate::source::load_module_tree(&roots, base) {
                Ok(modules) => {
                    loaded_modules = modules;
                }
                Err(e) => self.error(e),
            }
        }
        crate::stdlib::append_required_standard_capability_modules(items, &mut loaded_modules);

        self.configure_capabilities(items, &loaded_modules, base_dir);
        if !loaded_modules.is_empty() {
            self.prepare_loaded_modules(&loaded_modules);
            let visible_roots = Self::visible_module_roots(items);
            self.integrate_loaded_modules(&loaded_modules, &visible_roots);
        }

        self.build_signatures(items);

        if !loaded_modules.is_empty() {
            self.check_loaded_module_bodies(&loaded_modules);
        }

        self.check_body(items);
    }

    /// Type-check `items` against a caller-supplied list of already
    /// loaded dependency modules (skips disk IO). Used by the
    /// playground so multi-file projects stored in an in-browser map
    /// type-check without touching a filesystem.
    pub(super) fn check_with_loaded(
        &mut self,
        items: &[TopLevel],
        loaded: &[crate::source::LoadedModule],
    ) {
        // Phase B: track the entry module's prefix (see `check`).
        self.current_module_prefix = Self::module_decl(items).map(|m| m.name.clone());
        // Dependency aliases come in before local signatures so
        // `build_signatures`'s resolution sees imported types
        // (e.g. `Tile` resolves to `Types.Tile` when `Types` is in
        // `depends`).
        let mut loaded = loaded.to_vec();
        crate::stdlib::append_required_standard_capability_modules(items, &mut loaded);
        self.configure_capabilities(items, &loaded, None);
        self.prepare_loaded_modules(&loaded);
        let visible_roots = Self::visible_module_roots(items);
        self.integrate_loaded_modules(&loaded, &visible_roots);
        self.build_signatures(items);
        self.check_loaded_module_bodies(&loaded);
        self.check_body(items);
    }

    fn configure_capabilities(
        &mut self,
        entry_items: &[TopLevel],
        loaded: &[crate::source::LoadedModule],
        module_root: Option<&str>,
    ) {
        let entry_scope = Self::module_decl(entry_items)
            .map(|m| m.name.as_str())
            .unwrap_or("");
        let (mut registry, errors) =
            crate::capability::CapabilityRegistry::from_module(entry_scope, entry_items);
        self.errors
            .extend(errors.into_iter().map(|error| TypeError {
                message: error.message,
                line: error.line,
                col: 1,
                origin: None,
                secondary: None,
            }));
        for module in loaded {
            let (next, next_errors) =
                crate::capability::CapabilityRegistry::from_module(&module.dep_name, &module.items);
            registry.merge(next);
            let display_path = module_root
                .and_then(|root| module.path.strip_prefix(root).ok())
                .unwrap_or(module.path.as_path())
                .display()
                .to_string();
            // Keep LoadedModule's public parsed-module shape stable. Disk
            // analysis can enrich the exceptional error path from the file
            // the loader already resolved; preloaded/virtual modules retain
            // the correct file but deliberately omit a potentially false
            // entry-source snippet.
            let source = if next_errors.is_empty() || module_root.is_none() {
                None
            } else {
                crate::source::resolve_standard_module_source(&module.dep_name)
                    .map(|module| module.source)
                    .or_else(|| std::fs::read_to_string(&module.path).ok())
                    .map(std::sync::Arc::<str>::from)
            };
            let origin = TypeErrorOrigin {
                file: display_path,
                source,
            };
            self.errors
                .extend(next_errors.into_iter().map(|error| TypeError {
                    message: error.message,
                    line: error.line,
                    col: 1,
                    origin: Some(origin.clone()),
                    secondary: None,
                }));
        }
        self.capabilities = registry;
    }

    /// Record the complete dependency graph and resolve each module's public
    /// type surface before any single importer is checked. This is resolver
    /// context, not visibility: a facade may re-export a type declared several
    /// files below it without making every module in between globally visible.
    fn prepare_loaded_modules(&mut self, modules: &[crate::source::LoadedModule]) {
        // Phase B (peer review round 6): track each dep module's own
        // `depends` list so the per-owner type resolver
        // (`canonicalize_named_in_module`) can walk it instead of
        // falling back to the importer's context or to whichever
        // siblings happen to be in the entry's loaded tree.
        for m in modules {
            if let Some(module_decl) = TypeChecker::module_decl(&m.items) {
                self.module_depends
                    .insert(m.dep_name.clone(), module_decl.depends.clone());
            }
        }
        let pairs: Vec<_> = modules
            .iter()
            .map(|m| (m.dep_name.clone(), m.items.clone()))
            .collect();
        self.module_type_exports = crate::visibility::collect_module_type_exports(&pairs);
    }

    /// Names visible at an importer boundary: explicit dependencies plus
    /// source-typed standard modules pulled in by a builtin used in that
    /// module. The latter are ordinary nominal type owners even though users
    /// do not have to spell them in `depends [...]`.
    fn visible_module_roots(items: &[TopLevel]) -> Vec<String> {
        let mut roots = Self::module_decl(items)
            .map(|module| module.depends.clone())
            .unwrap_or_default();
        roots.extend(crate::stdlib::implicit_stdlib_deps(items));
        roots.sort();
        roots.dedup();
        roots
    }

    /// Integrate only the named dependency surfaces into this checker's
    /// ordinary lookup maps. The complete module list remains available so a
    /// type re-export can bring in the original declaration's fields and
    /// constructors while unrelated transitive modules stay hidden.
    fn integrate_loaded_modules(
        &mut self,
        modules: &[crate::source::LoadedModule],
        visible_modules: &[String],
    ) {
        self.visible_module_names
            .extend(visible_modules.iter().cloned());
        let pairs: Vec<_> = modules
            .iter()
            .map(|m| (m.dep_name.clone(), m.items.clone()))
            .collect();
        let registry = crate::visibility::SymbolRegistry::from_visible_modules(
            &pairs,
            visible_modules,
            &self.module_type_exports,
        );
        if let Err(e) = self.integrate_registry(&registry) {
            self.error(e);
        }
        self.canonicalize_source_typed_builtin_sigs();
        self.register_capability_sigs();
    }

    /// Re-stamp builtin signatures whose nominal types are owned by embedded
    /// Aver modules after those modules have entered the active symbol table.
    ///
    /// Most builtins mention primitives or host-owned records and stay raw.
    /// Crypto and binary TCP cross source-defined refinements, so their
    /// initially unresolved names must join the same `TypeId` space as
    /// imported values. The affected builtins live in
    /// `crate::stdlib::SOURCE_TYPED_BUILTINS`, shared with the compile-time
    /// implicit module loading that keeps codegen able to emit those types.
    fn canonicalize_source_typed_builtin_sigs(&mut self) {
        for (name, _) in crate::stdlib::SOURCE_TYPED_BUILTINS {
            let Some(mut sig) = self.extra_sigs.remove(*name) else {
                continue;
            };
            sig.params = sig
                .params
                .into_iter()
                .map(|ty| self.canonicalize_named(ty))
                .collect();
            sig.ret = self.canonicalize_named(sig.ret);
            self.extra_sigs.insert(name.to_string(), sig);
        }
    }

    /// Visit every function body in each loaded dependency module so the
    /// per-`Spanned<Expr>` type slot gets populated. Without this, the
    /// downstream codegen consumers (Step 2 legacy WASM, Step 1 Rust,
    /// future wasm-gc) would see `Spanned::ty() == None` for everything in
    /// dependent modules — which used to be patched over by per-backend
    /// ad-hoc inference; the typed pipeline closes that gap properly.
    ///
    /// Each module gets its own short-lived `TypeChecker` so unqualified
    /// references inside the module resolve against that module's own
    /// signatures (the parent checker only sees the qualified canonical
    /// names from `integrate_loaded_modules`). `Spanned::set_ty` writes
    /// straight to the shared AST node, so the type stamps survive the
    /// sub-checker dropping. Diagnostics from the sub-check are folded
    /// back into the parent so a real type bug in `combat.av` still
    /// surfaces alongside any error in `main.av`.
    fn check_loaded_module_bodies(&mut self, modules: &[crate::source::LoadedModule]) {
        for module in modules {
            // Phase B: clone the parent's `SymbolTable` into the sub-
            // checker so every module shares the same opaque
            // identity space. The dep module's own declarations are
            // already registered in the table (the parent built it
            // from `entry_items + dep_modules`).
            let mut sub = TypeChecker::new_with_symbols(self.symbol_table.clone());
            sub.self_host_mode = self.self_host_mode;
            sub.capabilities = self.capabilities.clone();
            sub.module_depends = self.module_depends.clone();
            sub.module_type_exports = self.module_type_exports.clone();
            // Phase B: the dep module's prefix in the symbol table is
            // its `dep_name` (the path the entry's `depends` clause
            // wrote, e.g. `Pricing.Discount`), not the interior
            // `module X` declaration inside the file. Use the
            // `dep_name` so `resolve_fn_id` finds `mkDiscount` ->
            // `FnKey::in_module(dep_name, "mkDiscount")` for own-module
            // bodies in the sub-checker.
            sub.current_module_prefix = Some(module.dep_name.clone());
            // Phase B (peer review round 6): the sub-checker for
            // module `B` must see `B`'s *own* depends — not every
            // sibling the entry happened to load. The pre-fix sent
            // `modules - self`, which let an unrelated sibling `C`
            // (also a dep of the entry) leak into `B`'s resolver
            // context and silently shadow types `B` genuinely
            // depends on. Filter `modules` by the dep names listed
            // in `B`'s own `depends [...]` declaration so the only
            // bare-name aliases the sub-checker sees come from
            // modules `B` itself imported.
            let own_depends = Self::visible_module_roots(&module.items);
            sub.integrate_loaded_modules(modules, &own_depends);
            sub.build_signatures(&module.items);
            sub.check_top_level_stmts(&module.items);
            sub.check_verify_blocks(&module.items);
            for item in &module.items {
                if let TopLevel::FnDef(f) = item {
                    sub.check_fn(f);
                }
            }
            self.errors.append(&mut sub.errors);
        }
    }

    fn check_body(&mut self, items: &[TopLevel]) {
        self.check_top_level_stmts(items);
        self.check_verify_blocks(items);
        for item in items {
            if let TopLevel::FnDef(f) = item {
                self.check_fn(f);
            }
        }
    }
}
