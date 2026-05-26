use super::*;

impl TypeChecker {
    pub(super) fn check(&mut self, items: &[TopLevel], base_dir: Option<&str>) {
        // Phase B: track the entry module's prefix so bare-name
        // resolution in `resolve_fn_id` / `resolve_type_id` knows
        // which scope to try first.
        self.current_module_prefix = Self::module_decl(items).map(|m| m.name.clone());
        // Load + integrate dependency modules FIRST so the alias
        // maps are populated before `build_signatures` resolves local
        // fn / type annotations.
        let mut loaded_modules: Vec<crate::source::LoadedModule> = Vec::new();
        if let Some(base) = base_dir
            && let Some(module) = Self::module_decl(items)
        {
            match crate::source::load_module_tree(&module.depends, base) {
                Ok(modules) => {
                    self.integrate_loaded_modules(&modules);
                    loaded_modules = modules;
                }
                Err(e) => self.error(e),
            }
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
        self.integrate_loaded_modules(loaded);
        self.build_signatures(items);
        self.check_loaded_module_bodies(loaded);
        self.check_body(items);
    }

    fn integrate_loaded_modules(&mut self, modules: &[crate::source::LoadedModule]) {
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
        let registry = crate::visibility::SymbolRegistry::from_modules(&pairs);
        if let Err(e) = self.integrate_registry(&registry) {
            self.error(e);
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
            let own_depends: Vec<String> = TypeChecker::module_decl(&module.items)
                .map(|m| m.depends.clone())
                .unwrap_or_default();
            let visible_to_sub: Vec<_> = modules
                .iter()
                .filter(|m| own_depends.iter().any(|d| d == &m.dep_name))
                .cloned()
                .collect();
            sub.integrate_loaded_modules(&visible_to_sub);
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

    // ── Memo-safety analysis ─────────────────────────────────────────────

    /// A type is memo-safe if its runtime values can be cheaply hashed and
    /// compared for equality (scalars, records/variants of scalars).
    pub(super) fn is_memo_safe(&self, ty: &Type, visiting: &mut HashSet<String>) -> bool {
        match ty {
            // String stays excluded for now: memo keys hash String content,
            // so string-heavy recursion can degrade to O(n) keying work.
            Type::Int | Type::Float | Type::Bool | Type::Unit => true,
            Type::Str => false,
            Type::Tuple(items) => items.iter().all(|item| self.is_memo_safe(item, visiting)),
            Type::List(_)
            | Type::Vector(_)
            | Type::Map(_, _)
            | Type::Fn(_, _, _)
            | Type::Invalid
            | Type::Var(_) => false,
            Type::Result(_, _) | Type::Option(_) => false,
            Type::Named { name, .. } => {
                // Prevent infinite recursion for cyclic type defs
                if !visiting.insert(name.clone()) {
                    return true;
                }
                let safe = self.named_type_memo_safe(name, visiting);
                visiting.remove(name);
                safe
            }
        }
    }

    /// Check whether a named user-defined type has only memo-safe fields.
    pub(super) fn named_type_memo_safe(&self, name: &str, visiting: &mut HashSet<String>) -> bool {
        // Iron — A5: `fields_for_type` resolves `name` through
        // `sig_aliases` and returns one `(field_name, field_type)`
        // per declared field; no dual-key arithmetic.
        let fields = self.fields_for_type(name);
        if !fields.is_empty() {
            for (_, field_ty) in fields {
                if !self.is_memo_safe(&field_ty, visiting) {
                    return false;
                }
            }
            return true;
        }

        // Check sum type variants: constructors are registered as
        // "TypeName.VariantName" with param types, or in value_members
        // for zero-arg constructors. Phase B: constructors live in
        // `extra_sigs`, never in the FnId-keyed `fn_sigs`, but
        // `all_fn_sigs` flattens both halves for the prefix scan.
        let prefix = format!("{}.", name);
        let mut found_variants = false;
        for (key, sig) in self.all_fn_sigs() {
            if key.starts_with(&prefix) && key.len() > prefix.len() {
                found_variants = true;
                for param in &sig.params {
                    if !self.is_memo_safe(param, visiting) {
                        return false;
                    }
                }
            }
        }
        for key in self.value_members.keys() {
            if key.starts_with(&prefix) && key.len() > prefix.len() {
                found_variants = true;
                // Zero-arg constructors carry no data — always safe
            }
        }
        if found_variants {
            return true;
        }

        // Unknown named type — conservatively not safe
        false
    }

    /// Compute the set of user-defined type names that are memo-safe.
    pub(super) fn compute_memo_safe_types(&self, items: &[TopLevel]) -> HashSet<String> {
        let mut safe = HashSet::new();
        for item in items {
            if let TopLevel::TypeDef(td) = item {
                let name = match td {
                    TypeDef::Sum { name, .. } | TypeDef::Product { name, .. } => name,
                };
                let mut visiting = HashSet::new();
                if self.is_memo_safe(&Type::named(name.clone()), &mut visiting) {
                    safe.insert(name.clone());
                }
            }
        }
        safe
    }
}
