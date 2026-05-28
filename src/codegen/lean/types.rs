/// Aver type → Lean 4 type string mapping.
use crate::types::Type;

/// Convert an Aver `Type` to a Lean 4 type string.
pub fn type_to_lean(ty: &Type) -> String {
    match ty {
        Type::Int => "Int".to_string(),
        Type::Float => "Float".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "Bool".to_string(),
        Type::Unit => "Unit".to_string(),
        Type::Result(ok, err) => {
            // Lean's Except has reversed order: Except Error Ok
            format!(
                "Except {} {}",
                type_to_lean_atom(err),
                type_to_lean_atom(ok)
            )
        }
        Type::Option(inner) => format!("Option {}", type_to_lean_atom(inner)),
        Type::List(inner) => format!("List {}", type_to_lean_atom(inner)),
        Type::Vector(inner) => format!("Array {}", type_to_lean_atom(inner)),
        Type::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(type_to_lean).collect();
            format!("({})", parts.join(" × "))
        }
        Type::Map(key, value) if crate::codegen::common::is_set_type(ty) => {
            format!("Finset {}", type_to_lean_atom(key))
        }
        Type::Map(key, value) => {
            // No direct Map in Lean core; use a list of pairs as approximation
            format!("List ({} × {})", type_to_lean(key), type_to_lean(value))
        }
        Type::Fn(params, ret, _effects) => {
            let mut parts: Vec<String> = params.iter().map(type_to_lean_atom).collect();
            parts.push(type_to_lean(ret));
            parts.join(" → ")
        }
        Type::Var(_) | Type::Invalid => {
            panic!(
                "Lean codegen: encountered Type::Invalid or Type::Var. \
                 This indicates unresolved typing leaked into codegen."
            )
        }
        // display-only: rendering the Lean type identifier string
        // — `name` IS the right surface, `id` carries no display
        // information. Identity-sensitive routing happens at the
        // call layer (see `backend_named_type_key`).
        Type::Named { name, .. } => {
            if name.contains('.') {
                name.replace('.', "_")
            } else {
                name.clone()
            }
        }
    }
}

/// Like type_to_lean but wraps compound types in parens for use as type arguments.
fn type_to_lean_atom(ty: &Type) -> String {
    match ty {
        Type::Result(..)
        | Type::Option(_)
        | Type::List(_)
        | Type::Vector(_)
        | Type::Fn(..)
        | Type::Map(..) => {
            format!("({})", type_to_lean(ty))
        }
        _ => type_to_lean(ty),
    }
}

/// Convert an Aver type annotation string to a Lean 4 type string.
pub fn type_annotation_to_lean(ann: &str) -> String {
    let ty = crate::types::parse_type_str(ann);
    type_to_lean(&ty)
}

#[cfg(test)]
mod tests {
    use super::type_annotation_to_lean;

    #[test]
    fn nested_result_type_arguments_are_parenthesized() {
        assert_eq!(
            type_annotation_to_lean("Result<List<Cmd>, String>"),
            "Except String (List Cmd)"
        );
    }
}
