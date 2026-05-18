use super::*;

impl TypeChecker {
    pub(in super::super) fn infer_record_create_expr(
        &mut self,
        type_name: &str,
        fields: &[(String, Spanned<Expr>)],
    ) -> Type {
        if self.opaque_types.contains(type_name) {
            self.error(format!(
                "Cannot construct opaque type '{}' — use its module's constructor function",
                type_name
            ));
            return Type::Named(type_name.to_string());
        }

        let schema_prefix = format!("{}.", type_name);
        let mut expected = HashMap::new();
        for (key, ty) in &self.record_field_types {
            if let Some(field_name) = key.strip_prefix(&schema_prefix) {
                expected.insert(field_name.to_string(), ty.clone());
            }
        }

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
                if !Self::constraint_compatible(&actual_ty, expected_ty) {
                    self.error(format!(
                        "Record '{}' field '{}' expects {}, got {}",
                        type_name,
                        field_name,
                        expected_ty.display(),
                        actual_ty.display()
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
        Type::Named(type_name.to_string())
    }

    pub(in super::super) fn infer_record_update_expr(
        &mut self,
        type_name: &str,
        base: &Spanned<Expr>,
        updates: &[(String, Spanned<Expr>)],
    ) -> Type {
        if self.opaque_types.contains(type_name) {
            self.error(format!(
                "Cannot update opaque type '{}' — use its module's API",
                type_name
            ));
            return Type::Named(type_name.to_string());
        }
        let base_ty = self.infer_type(base);
        let expected_ty = Type::Named(type_name.to_string());
        if !Self::constraint_compatible(&base_ty, &expected_ty) {
            self.error(format!(
                "{}.update: base has type {}, expected {}",
                type_name,
                base_ty.display(),
                type_name
            ));
        }

        let schema_prefix = format!("{}.", type_name);
        let mut expected_fields = HashMap::new();
        for (key, ty) in &self.record_field_types {
            if let Some(field_name) = key.strip_prefix(&schema_prefix) {
                expected_fields.insert(field_name.to_string(), ty.clone());
            }
        }

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
                if !Self::constraint_compatible(&actual_ty, expected_ty) {
                    self.error(format!(
                        "Record '{}' field '{}' expects {}, got {}",
                        type_name,
                        field_name,
                        expected_ty.display(),
                        actual_ty.display()
                    ));
                }
            } else {
                self.error(format!(
                    "Record '{}' has no field '{}'",
                    type_name, field_name
                ));
            }
        }

        Type::Named(type_name.to_string())
    }
}
