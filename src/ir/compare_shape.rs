//! Source-type facts used to order structural equality comparisons.
//!
//! A scalar-only shape contains no collection or recursive type. Comparing
//! it uses a statically bounded number of scalar comparisons, although an
//! individual String or unbounded Int comparison can still be expensive.
//! Backends resolve nominal declarations; this walk knows no target layout.

use std::collections::HashSet;

use super::{TypeId, TypeKey};
use crate::types::Type;

/// All fields of a resolved declaration, including every sum variant.
pub(crate) struct NominalFields {
    pub key: TypeKey,
    pub fields: Vec<Type>,
}

/// Stable scalar-first comparison order, or `None` for declaration order.
/// Unknown declarations and recursive shapes conservatively stay last.
pub(crate) fn cheap_fields_first(
    types: &[Type],
    scope: Option<&str>,
    resolve: &impl Fn(Option<TypeId>, &str, Option<&str>) -> Option<NominalFields>,
) -> Option<Vec<usize>> {
    let cheap: Vec<bool> = types
        .iter()
        .map(|ty| scalar_only(ty, scope, resolve, &mut HashSet::new()))
        .collect();
    let order: Vec<usize> = (0..types.len())
        .filter(|&i| cheap[i])
        .chain((0..types.len()).filter(|&i| !cheap[i]))
        .collect();
    order
        .iter()
        .enumerate()
        .any(|(i, &field)| i != field)
        .then_some(order)
}

fn scalar_only(
    ty: &Type,
    scope: Option<&str>,
    resolve: &impl Fn(Option<TypeId>, &str, Option<&str>) -> Option<NominalFields>,
    seen: &mut HashSet<TypeKey>,
) -> bool {
    match ty {
        Type::Int | Type::Float | Type::Bool | Type::Unit | Type::Str => true,
        Type::Option(inner) => scalar_only(inner, scope, resolve, seen),
        Type::Result(ok, err) => {
            scalar_only(ok, scope, resolve, seen) && scalar_only(err, scope, resolve, seen)
        }
        Type::Tuple(items) => items
            .iter()
            .all(|item| scalar_only(item, scope, resolve, seen)),
        Type::Named { id, name } => resolve(*id, name, scope).is_some_and(|decl| {
            if !seen.insert(decl.key.clone()) {
                return false;
            }
            let cheap = decl
                .fields
                .iter()
                .all(|field| scalar_only(field, decl.key.scope_str(), resolve, seen));
            seen.remove(&decl.key);
            cheap
        }),
        Type::List(_) | Type::Vector(_) | Type::Map(..) | Type::Fn(..) => false,
        Type::Var(_) | Type::Invalid => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::parse_type_str;

    fn order(types: &[&str], declarations: &[(&str, &[&str])]) -> Option<Vec<usize>> {
        let types: Vec<Type> = types.iter().map(|ty| parse_type_str(ty)).collect();
        cheap_fields_first(&types, None, &|_, name, _| {
            let (_, fields) = declarations.iter().find(|(key, _)| *key == name)?;
            Some(NominalFields {
                key: TypeKey::entry(name),
                fields: fields.iter().map(|ty| parse_type_str(ty)).collect(),
            })
        })
    }

    #[test]
    fn scalar_wrappers_move_ahead_of_collections_without_reordering_either_group() {
        assert_eq!(
            order(
                &[
                    "List<Int>",
                    "Option<Result<Tuple<Int, String>, Bool>>",
                    "Map<Int, Int>",
                    "Float",
                    "Vector<Int>",
                    "Unit",
                ],
                &[],
            ),
            Some(vec![1, 3, 5, 0, 2, 4]),
        );
        assert_eq!(order(&["Int", "String", "List<Int>"], &[]), None);
    }

    #[test]
    fn nominal_walk_rejects_cycles_and_checks_every_sum_field() {
        let declarations: &[(&str, &[&str])] = &[
            ("Tree", &["Tree", "Tree", "Int"]),
            ("A", &["Option<B>"]),
            ("B", &["A"]),
            ("Scalar", &["Option<Int>", "String"]),
            ("HasCollection", &["Int", "List<Scalar>"]),
            ("Empty", &[]),
        ];
        assert_eq!(
            order(
                &["Tree", "Scalar", "A", "Empty", "HasCollection", "Scalar"],
                declarations,
            ),
            Some(vec![1, 3, 5, 0, 2, 4]),
        );
        assert_eq!(order(&["Missing", "Int"], declarations), Some(vec![1, 0]));
    }

    #[test]
    fn field_names_resolve_in_the_declaration_owner_and_ids_survive_spelling() {
        let types = vec![
            parse_type_str("List<Int>"),
            Type::Named {
                id: Some(TypeId(42)),
                name: "AnAlias".into(),
            },
        ];
        let resolve = |id, name: &str, scope: Option<&str>| {
            let (key, fields) = if id == Some(TypeId(42)) {
                (
                    TypeKey::in_module("A", "Outer"),
                    vec![parse_type_str("Local")],
                )
            } else if name == "Local" && scope == Some("A") {
                (TypeKey::in_module("A", "Local"), vec![Type::Int])
            } else if name == "Local" && scope == Some("B") {
                (
                    TypeKey::in_module("B", "Local"),
                    vec![parse_type_str("List<Int>")],
                )
            } else {
                return None;
            };
            Some(NominalFields { key, fields })
        };
        assert_eq!(
            cheap_fields_first(&types, Some("B"), &resolve),
            Some(vec![1, 0])
        );
        assert_eq!(
            cheap_fields_first(&[parse_type_str("Local"), Type::Int], Some("B"), &resolve),
            Some(vec![1, 0]),
        );
    }

    #[test]
    fn two_module_local_types_with_the_same_bare_name_are_not_a_cycle() {
        let types = vec![parse_type_str("List<Int>"), parse_type_str("A.Local")];
        let resolve = |_, name: &str, _: Option<&str>| {
            let (key, fields) = match name {
                "A.Local" => (
                    TypeKey::in_module("A", "Local"),
                    vec![parse_type_str("B.Local")],
                ),
                "B.Local" => (TypeKey::in_module("B", "Local"), vec![Type::Int]),
                _ => return None,
            };
            Some(NominalFields { key, fields })
        };
        assert_eq!(cheap_fields_first(&types, None, &resolve), Some(vec![1, 0]));
    }
}
