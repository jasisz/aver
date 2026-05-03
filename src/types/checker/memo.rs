use super::*;

impl TypeChecker {
    pub(super) fn check(&mut self, items: &[TopLevel], base_dir: Option<&str>) {
        self.build_signatures(items);

        if let Some(base) = base_dir
            && let Some(module) = Self::module_decl(items)
        {
            match crate::source::load_module_tree(&module.depends, base) {
                Ok(modules) => {
                    self.integrate_loaded_modules(&modules);
                    self.check_loaded_module_bodies(&modules);
                }
                Err(e) => self.error(e),
            }
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
        self.build_signatures(items);
        self.integrate_loaded_modules(loaded);
        self.check_loaded_module_bodies(loaded);
        self.check_body(items);
    }

    fn integrate_loaded_modules(&mut self, modules: &[crate::source::LoadedModule]) {
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
        for (idx, module) in modules.iter().enumerate() {
            let mut sub = TypeChecker::new();
            sub.build_signatures(&module.items);
            // Pull in the OTHER modules' canonical (qualified) signatures —
            // skip self so the module sees its own types as transparent
            // (opaque enforcement only applies cross-module).
            let others: Vec<_> = modules
                .iter()
                .enumerate()
                .filter(|(i, _)| *i != idx)
                .map(|(_, m)| m.clone())
                .collect();
            sub.integrate_loaded_modules(&others);
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
            | Type::Unknown
            | Type::Var(_) => false,
            Type::Result(_, _) | Type::Option(_) => false,
            Type::Named(name) => {
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
        // Check record fields: keys are "TypeName.fieldName"
        let prefix = format!("{}.", name);
        let mut found_fields = false;
        for (key, field_ty) in &self.record_field_types {
            if key.starts_with(&prefix) {
                found_fields = true;
                if !self.is_memo_safe(field_ty, visiting) {
                    return false;
                }
            }
        }
        if found_fields {
            return true;
        }

        // Check sum type variants: constructors are registered in fn_sigs
        // as "TypeName.VariantName" with param types, or in value_members
        // for zero-arg constructors.
        let mut found_variants = false;
        for (key, sig) in &self.fn_sigs {
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
                if self.is_memo_safe(&Type::Named(name.clone()), &mut visiting) {
                    safe.insert(name.clone());
                }
            }
        }
        safe
    }
}
