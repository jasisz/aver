/// Aver type → Rust type string mapping.
use crate::types::Type;

/// Convert an Aver `Type` to a Rust type string.
pub fn type_to_rust(ty: &Type) -> String {
    match ty {
        Type::Int => "i64".to_string(),
        Type::Float => "f64".to_string(),
        Type::Str => "AverStr".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::Result(ok, err) => {
            format!("Result<{}, {}>", type_to_rust(ok), type_to_rust(err))
        }
        Type::Option(inner) => format!("Option<{}>", type_to_rust(inner)),
        Type::List(inner) => format!("aver_rt::AverList<{}>", type_to_rust(inner)),
        Type::Tuple(items) => {
            let parts: Vec<String> = items.iter().map(type_to_rust).collect();
            format!("({})", parts.join(", "))
        }
        Type::Map(key, value) => {
            format!(
                "aver_rt::AverMap<{}, {}>",
                type_to_rust(key),
                type_to_rust(value)
            )
        }
        Type::Vector(inner) => format!("aver_rt::AverVector<{}>", type_to_rust(inner)),
        Type::Fn(params, ret, _effects) => {
            let ps: Vec<String> = params.iter().map(type_to_rust).collect();
            format!("fn({}) -> {}", ps.join(", "), type_to_rust(ret))
        }
        Type::Unknown | Type::Var(_) | Type::Any => {
            panic!(
                "Rust codegen: encountered Type::Unknown or Type::Var. \
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
