//! Concrete smoke-test witnesses for generated laws. Quantification comes
//! from the declared types, never from these samples. Nominal witnesses use
//! only constructors visible in this module; resources remain unconstructible.
use super::*;

pub(super) fn witness(model: &Model<'_>, annotation: &str) -> Result<String, String> {
    let ty = crate::types::parse_type_str_strict(annotation)
        .map_err(|_| format!("invalid trace law type '{annotation}'"))?;
    value(model, &ty, &mut Vec::new()).ok_or_else(|| {
        format!("compositional law needs a constructible sample witness for '{annotation}'")
    })
}

fn value(model: &Model<'_>, ty: &Type, active: &mut Vec<String>) -> Option<String> {
    Some(match ty {
        Type::Int => "0".into(),
        Type::Float => "0.0".into(),
        Type::Bool => "false".into(),
        Type::Unit => "Unit".into(),
        Type::Str => "\"\"".into(),
        Type::List(_) => "[]".into(),
        Type::Map(_, _) => "{}".into(),
        Type::Vector(_) => "Vector.new()".into(),
        Type::Option(_) => "Option.None".into(),
        Type::Result(ok, err) => value(model, err, active)
            .map(|v| format!("Result.Err({v})"))
            .or_else(|| value(model, ok, active).map(|v| format!("Result.Ok({v})")))?,
        Type::Tuple(fields) => format!(
            "({})",
            fields
                .iter()
                .map(|t| value(model, t, active))
                .collect::<Option<Vec<_>>>()?
                .join(", ")
        ),
        Type::Named { name, .. } => {
            if active.contains(name) {
                return None;
            }
            let td = model.type_defs.iter().find(|td| match td {
                TypeDef::Sum {
                    name: candidate, ..
                }
                | TypeDef::Product {
                    name: candidate, ..
                } => candidate == name,
            })?;
            active.push(name.clone());
            let mut field = |annotation: &str| {
                value(
                    model,
                    &crate::types::parse_type_str_strict(annotation).ok()?,
                    active,
                )
            };
            let result = match td {
                TypeDef::Sum { variants, .. } => variants.iter().find_map(|variant| {
                    let values = variant
                        .fields
                        .iter()
                        .map(|ty| field(ty))
                        .collect::<Option<Vec<_>>>()?;
                    let payload = if values.is_empty() {
                        String::new()
                    } else {
                        format!("({})", values.join(", "))
                    };
                    Some(format!("{name}.{}{payload}", variant.name))
                }),
                TypeDef::Product { fields, .. } => fields
                    .iter()
                    .map(|(n, t)| field(t).map(|v| format!("{n} = {v}")))
                    .collect::<Option<Vec<_>>>()
                    .map(|fields| format!("{name}({})", fields.join(", "))),
            };
            active.pop();
            result?
        }
        _ => return None,
    })
}
