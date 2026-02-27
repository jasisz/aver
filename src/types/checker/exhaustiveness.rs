use super::*;

impl TypeChecker {
    /// Check whether a match expression covers all possible values of the subject type.
    /// Emits a type error if any variants/cases are missing and no catch-all arm exists.
    pub(super) fn check_match_exhaustiveness(
        &mut self,
        subject_ty: &Type,
        arms: &[crate::ast::MatchArm],
    ) {
        // A catch-all pattern (_  or bare identifier) makes any match exhaustive.
        for arm in arms {
            if is_catch_all(&arm.pattern) {
                return;
            }
        }

        match subject_ty {
            Type::Bool => {
                let has_true = arms
                    .iter()
                    .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))));
                let has_false = arms
                    .iter()
                    .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))));
                let mut missing = Vec::new();
                if !has_true {
                    missing.push("true");
                }
                if !has_false {
                    missing.push("false");
                }
                if !missing.is_empty() {
                    self.error(format!(
                        "Non-exhaustive match: missing {}",
                        missing.join(", ")
                    ));
                }
            }

            Type::Result(_, _) => {
                self.check_constructor_exhaustiveness(
                    arms,
                    &["Result.Ok", "Result.Err"],
                    "Result",
                );
            }

            Type::Option(_) => {
                self.check_constructor_exhaustiveness(
                    arms,
                    &["Option.Some", "Option.None"],
                    "Option",
                );
            }

            Type::Named(name) => {
                if let Some(variant_names) = self.type_variants.get(name).cloned() {
                    let qualified: Vec<String> = variant_names
                        .iter()
                        .map(|v| format!("{}.{}", name, v))
                        .collect();
                    let qualified_refs: Vec<&str> = qualified.iter().map(|s| s.as_str()).collect();
                    self.check_constructor_exhaustiveness(arms, &qualified_refs, name);
                }
                // If the type is not in type_variants (e.g. a record type), skip checking.
            }

            Type::List(_) => {
                let has_empty = arms
                    .iter()
                    .any(|a| matches!(&a.pattern, Pattern::EmptyList));
                let has_cons = arms
                    .iter()
                    .any(|a| matches!(&a.pattern, Pattern::Cons(_, _)));
                let mut missing = Vec::new();
                if !has_empty {
                    missing.push("[]");
                }
                if !has_cons {
                    missing.push("[h, ..t]");
                }
                if !missing.is_empty() {
                    self.error(format!(
                        "Non-exhaustive match: missing {}",
                        missing.join(", ")
                    ));
                }
            }

            // Infinite domains — only exhaustive with a catch-all (already checked above).
            Type::Int | Type::Float | Type::Str | Type::Tuple(_) => {
                self.error("Non-exhaustive match: missing catch-all (_) pattern".to_string());
            }

            // Map, Fn, Unit, Unknown — skip checking.
            _ => {}
        }
    }

    /// Helper: check that all expected constructor names appear in the match arms.
    /// Accepts both qualified ("Shape.Circle") and unqualified ("Circle") pattern names.
    fn check_constructor_exhaustiveness(
        &mut self,
        arms: &[crate::ast::MatchArm],
        expected: &[&str],
        type_name: &str,
    ) {
        let present: Vec<&str> = arms
            .iter()
            .filter_map(|a| match &a.pattern {
                Pattern::Constructor(name, _) => Some(name.as_str()),
                _ => None,
            })
            .collect();

        let is_covered = |qualified: &str| -> bool {
            // Match either "Shape.Circle" or just "Circle"
            let short = qualified.rsplit('.').next().unwrap_or(qualified);
            present.iter().any(|p| *p == qualified || *p == short)
        };

        let missing: Vec<String> = expected
            .iter()
            .filter(|&&name| !is_covered(name))
            .map(|&name| {
                // Show a nice pattern: "Result.Ok(_)" for constructors with args,
                // "Option.None" for nullary constructors.
                let has_args = self
                    .fn_sigs
                    .get(name)
                    .map(|sig| !sig.params.is_empty())
                    .unwrap_or(false);
                if has_args {
                    format!("{}(_)", name)
                } else {
                    name.to_string()
                }
            })
            .collect();

        if !missing.is_empty() {
            self.error(format!(
                "Non-exhaustive match on {}: missing {}",
                type_name,
                missing.join(", ")
            ));
        }
    }
}

/// A catch-all pattern covers all possible values.
fn is_catch_all(pattern: &Pattern) -> bool {
    matches!(pattern, Pattern::Wildcard | Pattern::Ident(_))
}
