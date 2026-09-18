//! Concrete smoke-test witnesses for generated laws. Quantification comes
//! from the declared types, never from these samples. Nominal witnesses use
//! only constructors visible in this module; resources remain unconstructible.
use super::*;

pub(in crate::yield_lowering::trace) fn witness(
    model: &Model<'_>,
    annotation: &str,
) -> Result<String, String> {
    let ty = crate::types::parse_type_str_strict(annotation)
        .map_err(|_| format!("invalid trace law type '{annotation}'"))?;
    value(model, &ty, &mut Vec::new()).ok_or_else(|| {
        format!("compositional law needs a constructible sample witness for '{annotation}'")
    })
}

/// Stops of an imported process state, as its owner published them: the
/// variant name and the declared type of each live variable it carries. A
/// module never declares the state type of a process it imports, so this is
/// the only layout a sample of such a stop can be written from.
fn imported_stops<'a>(model: &Model<'a>, name: &str) -> Option<&'a [(String, Vec<String>)]> {
    model.imported.values().find_map(|protocol| {
        protocol
            .kinds
            .iter()
            .find(|kind| kind.state == name)
            .map(|kind| kind.variants.as_slice())
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
            });
            let stops = td.is_none().then(|| imported_stops(model, name)).flatten();
            if td.is_none() && stops.is_none() {
                return None;
            }
            active.push(name.clone());
            let mut field = |annotation: &str| {
                value(
                    model,
                    &crate::types::parse_type_str_strict(annotation).ok()?,
                    active,
                )
            };
            let constructor = |variant: &str, values: Vec<String>| {
                let payload = if values.is_empty() {
                    String::new()
                } else {
                    format!("({})", values.join(", "))
                };
                format!("{name}.{variant}{payload}")
            };
            let result = match td {
                Some(TypeDef::Sum { variants, .. }) => variants.iter().find_map(|variant| {
                    let values = variant
                        .fields
                        .iter()
                        .map(|ty| field(ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(constructor(&variant.name, values))
                }),
                Some(TypeDef::Product { fields, .. }) => fields
                    .iter()
                    .map(|(n, t)| field(t).map(|v| format!("{n} = {v}")))
                    .collect::<Option<Vec<_>>>()
                    .map(|fields| format!("{name}({})", fields.join(", "))),
                None => stops.into_iter().flatten().find_map(|(variant, live)| {
                    let values = live
                        .iter()
                        .map(|ty| field(ty))
                        .collect::<Option<Vec<_>>>()?;
                    Some(constructor(variant, values))
                }),
            };
            active.pop();
            result?
        }
        _ => return None,
    })
}
