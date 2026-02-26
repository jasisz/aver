use super::*;

impl TypeChecker {
    pub(super) fn check(&mut self, items: &[TopLevel], base_dir: Option<&str>) {
        self.build_signatures(items);

        if let Some(base) = base_dir {
            if let Some(module) = Self::module_decl(items) {
                let mut loading = Vec::new();
                for dep_name in &module.depends {
                    if let Err(e) = self.load_module_sigs(dep_name, base, &mut loading) {
                        self.error(e);
                    }
                }
            }
        }

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
            Type::Int | Type::Float | Type::Str | Type::Bool | Type::Unit => true,
            Type::List(_) | Type::Fn(_, _, _) | Type::Unknown => false,
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
        for (key, _) in &self.value_members {
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
