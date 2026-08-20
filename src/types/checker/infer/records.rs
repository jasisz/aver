use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_record_create_expr(
        &mut self,
        type_name: &str,
        fields: &[(String, Spanned<Expr>)],
    ) -> Type {
        // Iron — A3: `opaque_types` is keyed by canonical
        // "Module.Type"; resolve bare references through `sig_aliases`
        // before checking so an attempt to construct an imported
        // opaque type by its bare name (`Discount` from
        // `Pricing.Discount`) is caught the same way the fully
        // qualified form would be.
        let canonical_type = self.canonical_type_name(type_name);
        if !self.self_host_mode && self.opaque_types.contains(canonical_type.as_str()) {
            if self.is_capability_resource_type(type_name) {
                self.error(format!(
                    "Cannot construct capability resource '{}' — obtain it from its provider",
                    type_name,
                ));
            } else {
                self.error(format!(
                    "Cannot construct opaque type '{}' — use its module's constructor function",
                    type_name,
                ));
            }
            return self.canonicalize_named(Type::named(canonical_type));
        }

        // Iron — A5: `fields_for_type` canonicalises `type_name`
        // through `sig_aliases` so an unqualified reference picks
        // up the canonical `"Module.Type"` entries and back.
        let expected: HashMap<String, Type> = self.fields_for_type(type_name).into_iter().collect();

        let mut seen = HashSet::new();
        for (field_name, expr) in fields {
            // Bidirectional: pass declared field type as expected so that
            // generic constructors (`Option.None`, `[]`, `Map.empty()`)
            // pick up T from the field declaration instead of stamping
            // `Unknown`.
            let actual_ty = if let Some(field_ty) = expected.get(field_name) {
                self.infer_type_with_expected(expr, Some(field_ty))
            } else {
                self.infer_type(expr)
            };
            if !seen.insert(field_name.clone()) {
                self.error(format!(
                    "Record '{}' field '{}' provided more than once",
                    type_name, field_name
                ));
                continue;
            }

            if expected.is_empty() {
                continue;
            }

            if let Some(expected_ty) = expected.get(field_name) {
                if !self.compatible(&actual_ty, expected_ty) {
                    let (want, got) = self.describe_type_pair(expected_ty, &actual_ty);
                    self.error(format!(
                        "Record '{}' field '{}' expects {}, got {}",
                        type_name, field_name, want, got
                    ));
                }
            } else {
                self.error(format!(
                    "Record '{}' has no field '{}'",
                    type_name, field_name
                ));
            }
        }

        if !expected.is_empty() {
            let mut required = expected.keys().cloned().collect::<Vec<_>>();
            required.sort();
            for field_name in required {
                if !seen.contains(&field_name) {
                    self.error(format!(
                        "Record '{}' missing required field '{}'",
                        type_name, field_name
                    ));
                }
            }
        }
        // Phase B: stamp with the symbol-table TypeId so downstream
        // backends see the typed identity (peer-review #148: record
        // create/update returned `Type::named(...)` with `id: None`).
        self.canonicalize_named(Type::named(type_name.to_string()))
    }

    pub(in super::super) fn infer_record_update_expr(
        &mut self,
        type_name: &str,
        base: &Spanned<Expr>,
        updates: &[(String, Spanned<Expr>)],
    ) -> Type {
        // Iron — A3: same canonical resolution as construction so the
        // bare-named reference to an imported opaque type is caught.
        let canon = self.canonical_type_name(type_name);
        if !self.self_host_mode && self.opaque_types.contains(&canon) {
            if self.is_capability_resource_type(type_name) {
                self.error(format!(
                    "Cannot update capability resource '{}' — pass it to its capability operations",
                    type_name
                ));
            } else {
                self.error(format!(
                    "Cannot update opaque type '{}' — use its module's API",
                    type_name
                ));
            }
            return self.canonicalize_named(Type::named(type_name.to_string()));
        }
        let base_ty = self.infer_type(base);
        // Canonicalize the user-written type name so the matcher sees
        // the same form `infer_type` stamps onto the base expression.
        let expected_ty = self.canonicalize_named(Type::named(type_name.to_string()));
        if !self.compatible(&base_ty, &expected_ty) {
            let (want, got) = self.describe_type_pair(&expected_ty, &base_ty);
            self.error(format!(
                "{}.update: base has type {}, expected {}",
                type_name, got, want
            ));
        }

        // Iron — A5: same canonicalised lookup as construction.
        let expected_fields: HashMap<String, Type> =
            self.fields_for_type(type_name).into_iter().collect();

        for (field_name, expr) in updates {
            // Bidirectional: same propagation as RecordCreate so generic
            // constructors in update field positions pick up T.
            let actual_ty = if let Some(field_ty) = expected_fields.get(field_name) {
                self.infer_type_with_expected(expr, Some(field_ty))
            } else {
                self.infer_type(expr)
            };
            if expected_fields.is_empty() {
                continue;
            }
            if let Some(expected_ty) = expected_fields.get(field_name) {
                if !self.compatible(&actual_ty, expected_ty) {
                    let (want, got) = self.describe_type_pair(expected_ty, &actual_ty);
                    self.error(format!(
                        "Record '{}' field '{}' expects {}, got {}",
                        type_name, field_name, want, got
                    ));
                }
            } else {
                self.error(format!(
                    "Record '{}' has no field '{}'",
                    type_name, field_name
                ));
            }
        }

        // Phase B: stamp with the symbol-table TypeId so downstream
        // backends see the typed identity (peer-review #148: record
        // create/update returned `Type::named(...)` with `id: None`).
        self.canonicalize_named(Type::named(type_name.to_string()))
    }
}
