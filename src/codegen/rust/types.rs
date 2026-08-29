use crate::codegen::CodegenContext;
use crate::codegen::common::{module_prefix_to_rust_path, type_key_for_name};
/// Aver type → Rust type string mapping.
use crate::types::Type;

/// Convert an Aver `Type` to a Rust type string.
pub fn type_to_rust(ty: &Type) -> String {
    match ty {
        Type::Int => "aver_rt::AverInt".to_string(),
        Type::Float => "f64".to_string(),
        Type::Str => "AverStr".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::Result(ok, err) => {
            format!("Result<{}, {}>", type_to_rust(ok), type_to_rust(err))
        }
        Type::Option(inner) => format!("Option<{}>", type_to_rust(inner)),
        Type::List(inner) if **inner == Type::Int => "aver_rt::AverIntList".to_string(),
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
        Type::Var(_) | Type::Invalid => {
            panic!(
                "Rust codegen: encountered Type::Invalid or Type::Var. \
                 This indicates unresolved typing leaked into codegen."
            )
        }
        // display-only: rendering the Rust type identifier string
        // — `name` IS the right surface, `id` carries no display
        // information. Identity-sensitive routing happens at the
        // call layer (see `backend_named_type_key`).
        Type::Named { name, .. } => {
            if name == crate::ir::INTERNAL_BUFFER_TYPE {
                return "aver_rt::Buffer".to_string();
            }
            if name == "String.Index" {
                return "aver_rt::StringIndex".to_string();
            }
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

/// Convert a resolved Aver type to Rust while preserving user-type ownership.
///
/// The context-free renderer above remains appropriate for compiler-owned
/// dotted names (`Tcp.Connection` -> `Tcp_Connection`). A user type such as
/// `Worker.Shape` is different: its `TypeId` / `TypeKey` names a declaration
/// emitted in the dependency module and must become
/// `crate::aver_generated::worker::Shape`, or just `Shape` while that module's
/// own file is being emitted.
pub fn type_to_rust_scoped(ty: &Type, ctx: &CodegenContext, current_scope: Option<&str>) -> String {
    match ty {
        Type::Int => "aver_rt::AverInt".to_string(),
        Type::Float => "f64".to_string(),
        Type::Str => "AverStr".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::Result(ok, err) => format!(
            "Result<{}, {}>",
            type_to_rust_scoped(ok, ctx, current_scope),
            type_to_rust_scoped(err, ctx, current_scope)
        ),
        Type::Option(inner) => {
            format!("Option<{}>", type_to_rust_scoped(inner, ctx, current_scope))
        }
        Type::List(inner) if **inner == Type::Int => "aver_rt::AverIntList".to_string(),
        Type::List(inner) => format!(
            "aver_rt::AverList<{}>",
            type_to_rust_scoped(inner, ctx, current_scope)
        ),
        Type::Tuple(items) => {
            let parts: Vec<String> = items
                .iter()
                .map(|ty| type_to_rust_scoped(ty, ctx, current_scope))
                .collect();
            format!("({})", parts.join(", "))
        }
        Type::Map(key, value) => format!(
            "aver_rt::AverMap<{}, {}>",
            type_to_rust_scoped(key, ctx, current_scope),
            type_to_rust_scoped(value, ctx, current_scope)
        ),
        Type::Vector(inner) => format!(
            "aver_rt::AverVector<{}>",
            type_to_rust_scoped(inner, ctx, current_scope)
        ),
        Type::Fn(params, ret, _effects) => {
            let params: Vec<String> = params
                .iter()
                .map(|ty| type_to_rust_scoped(ty, ctx, current_scope))
                .collect();
            format!(
                "fn({}) -> {}",
                params.join(", "),
                type_to_rust_scoped(ret, ctx, current_scope)
            )
        }
        Type::Var(_) | Type::Invalid => {
            panic!(
                "Rust codegen: encountered Type::Invalid or Type::Var. \
                 This indicates unresolved typing leaked into codegen."
            )
        }
        Type::Named { id, name } => {
            if name == crate::ir::INTERNAL_BUFFER_TYPE {
                return "aver_rt::Buffer".to_string();
            }
            if name == "String.Index" {
                return "aver_rt::StringIndex".to_string();
            }
            let key = id
                .map(|id| ctx.symbol_table.type_entry(id).key.clone())
                .or_else(|| {
                    let key = type_key_for_name(ctx, name, current_scope);
                    ctx.symbol_table.type_id_of(&key).map(|_| key)
                });
            match key {
                Some(key) if key.scope_str() == current_scope => key.name,
                Some(key) => match key.scope_str() {
                    Some(owner) => {
                        format!("{}::{}", module_prefix_to_rust_path(owner), key.name)
                    }
                    None => key.name,
                },
                // No TypeId means this is a compiler-owned named type or a
                // post-error recovery stamp. Preserve the established flat
                // aliases used by standard records/resources.
                None => name.replace('.', "_"),
            }
        }
    }
}

/// Parse an AST annotation and render it with module/type identity available.
/// Resolved-HIR consumers should prefer [`type_to_rust_scoped`] directly; this
/// bridge serves structural emitters whose source metadata is still textual.
pub fn type_annotation_to_rust_scoped(
    ann: &str,
    ctx: &CodegenContext,
    current_scope: Option<&str>,
) -> String {
    let ty = crate::types::parse_type_str(ann);
    type_to_rust_scoped(&ty, ctx, current_scope)
}
