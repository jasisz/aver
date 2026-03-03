/// Aver type → Rust type string mapping.
use crate::types::Type;

/// Convert an Aver `Type` to a Rust type string.
pub fn type_to_rust(ty: &Type) -> String {
    match ty {
        Type::Int => "i64".to_string(),
        Type::Float => "f64".to_string(),
        Type::Str => "String".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::Result(ok, err) => {
            format!("Result<{}, {}>", type_to_rust(ok), type_to_rust(err))
        }
        Type::Option(inner) => format!("Option<{}>", type_to_rust(inner)),
        Type::List(inner) => format!("Vec<{}>", type_to_rust(inner)),
        Type::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(|t| type_to_rust(t)).collect();
            format!("({})", parts.join(", "))
        }
        Type::Map(key, value) => {
            format!("HashMap<{}, {}>", type_to_rust(key), type_to_rust(value))
        }
        Type::Fn(params, ret, _effects) => {
            let ps: Vec<String> = params.iter().map(|t| type_to_rust(t)).collect();
            format!("fn({}) -> {}", ps.join(", "), type_to_rust(ret))
        }
        Type::Unknown => {
            panic!(
                "Rust codegen: encountered Type::Unknown. \
                 This indicates unresolved typing leaked into codegen."
            )
        }
        Type::Named(name) => {
            // Dotted names like Tcp.Connection → not supported in transpilation
            if name.contains('.') {
                name.replace('.', "_")
            } else {
                name.clone()
            }
        }
    }
}

/// Convert an Aver type annotation string to a Rust type string.
pub fn type_annotation_to_rust(ann: &str) -> String {
    let ty = crate::types::parse_type_str(ann);
    type_to_rust(&ty)
}
