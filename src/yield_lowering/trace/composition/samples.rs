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

/// The sample function an imported module publishes for `name`: a value of a
/// type whose constructors stay inside their owner, offered as a call any
/// importer can write. A type nobody publishes has none, and a law that needs
/// one is declined rather than written against a constructor that would not
/// resolve here.
fn published_sample<'a>(model: &Model<'a>, name: &str) -> Option<&'a str> {
    model.imported.values().find_map(|protocol| {
        protocol
            .trace
            .as_ref()?
            .segments
            .iter()
            .find_map(|segment| {
                segment
                    .params
                    .iter()
                    .zip(&segment.samples)
                    .find_map(|((_, ty), sample)| {
                        (ty == name && !sample.is_empty()).then_some(sample.as_str())
                    })
            })
    })
}

/// Stops of an imported process state, as its owner published them: the
/// variant name and the declared type of each live variable it carries. A
/// module never declares the state type of a process it imports, so this is
/// the layout a sample of such a stop falls back to when its owner published
/// no sample of its own.
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
            // The owner's own sample answers first: it crosses the boundary as
            // a value, so the state's constructors stay where they were
            // declared and the importer names none of them.
            if td.is_none()
                && let Some(sample) = published_sample(model, name)
            {
                return Some(format!("{sample}()"));
            }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yield_lowering::{ProcessTrace, ProcessTraceSegment, ProtocolKind};

    fn stop(variant: &str, live: &[&str]) -> (String, Vec<String>) {
        (
            variant.to_string(),
            live.iter().map(|ty| (*ty).to_string()).collect(),
        )
    }

    /// An exported process whose single request kind stops in `state`.
    fn owner(state: &str, variants: Vec<(String, Vec<String>)>) -> ProcessProtocol {
        ProcessProtocol {
            fn_name: "run".into(),
            params: vec![],
            return_type: "Int".into(),
            start: "__runStart".into(),
            request: "__RunRequest".into(),
            outcome: "__RunOutcome".into(),
            kinds: vec![ProtocolKind {
                name: "Claim".into(),
                operation: Some("Pool.claim".into()),
                arg_types: vec!["Int".into()],
                answer_type: Some("Option<Int>".into()),
                state: state.into(),
                answer_fn: "__runAnswerClaim".into(),
                variants,
            }],
            nests: vec![],
            trace: None,
        }
    }

    fn published(variants: Vec<(String, Vec<String>)>) -> HashMap<String, ProcessProtocol> {
        HashMap::from([(
            "Looper.run".to_string(),
            owner("Looper.__RunClaimState", variants),
        )])
    }

    #[test]
    fn samples_an_imported_stop_from_the_published_layout() {
        let imported = published(vec![
            stop("InLoopAt1", &["Looper.__LoopClaimState"]),
            stop("Await1", &["Int", "Int"]),
        ]);
        let local = owner("__RunClaimState", vec![]);
        let fn_sigs = FnSigs::new();
        let model = Model {
            protocol: &local,
            sources: &[],
            type_defs: vec![],
            segments: &[],
            fn_sigs: &fn_sigs,
            imported: &imported,
            local_protocols: &[],
            prefix: String::new(),
            upper: String::new(),
            operations: HashMap::new(),
            kinds: vec![],
        };

        // The importing module never declares the state type, so the stop is
        // written from the owner's layout, in the owner's names. The variant
        // carrying a private helper's state is passed over for one that is
        // constructible here.
        assert_eq!(
            witness(&model, "Looper.__RunClaimState").unwrap(),
            "Looper.__RunClaimState.Await1(0, 0)"
        );
        // The private helper's own stop is not a published kind, so it has no
        // layout and no sample.
        assert!(witness(&model, "Looper.__LoopClaimState").is_err());
    }

    #[test]
    fn declines_an_imported_stop_that_only_carries_an_unpublished_state() {
        let imported = published(vec![stop("InLoopAt1", &["Looper.__LoopClaimState"])]);
        let local = owner("__RunClaimState", vec![]);
        let fn_sigs = FnSigs::new();
        let model = Model {
            protocol: &local,
            sources: &[],
            type_defs: vec![],
            segments: &[],
            fn_sigs: &fn_sigs,
            imported: &imported,
            local_protocols: &[],
            prefix: String::new(),
            upper: String::new(),
            operations: HashMap::new(),
            kinds: vec![],
        };

        // Naming the helper's state in the importing module is a visibility
        // question the layout cannot answer, so the whole sample is declined
        // rather than written as a constructor that does not resolve.
        assert!(witness(&model, "Looper.__RunClaimState").is_err());
    }

    /// An owner that publishes a sample function for a segment parameter.
    fn publishing(samples: Vec<String>) -> HashMap<String, ProcessProtocol> {
        let mut protocol = owner("Looper.__RunClaimState", vec![]);
        protocol.trace = Some(ProcessTrace {
            dependencies: vec![],
            recursive: false,
            operations: vec![],
            input: "Looper.__RunTraceInput".into(),
            query: "Looper.__RunTraceQuery".into(),
            event: "Looper.__RunTraceEvent".into(),
            result: "Looper.__RunTraceRunResult".into(),
            source: "Looper.__runTraceSourceRun".into(),
            drive: "Looper.__runTraceDrive".into(),
            protocol_from: "Looper.__runProtocolTraceFrom".into(),
            segments: vec![ProcessTraceSegment {
                function: "Looper.__runAnswerClaim".into(),
                observer: "Looper.__runTraceSource__runAnswerClaim".into(),
                result: "Looper.__RunTrace__runAnswerClaimResult".into(),
                params: vec![
                    ("state".into(), "Looper.__RunClaimState".into()),
                    ("answer".into(), "Option<Int>".into()),
                ],
                cursor: "Looper.__runTraceSource__runAnswerClaimCursor".into(),
                samples,
            }],
            cursor: None,
            correspondence: None,
        });
        HashMap::from([("Looper.run".to_string(), protocol)])
    }

    fn importer<'a>(
        local: &'a ProcessProtocol,
        fn_sigs: &'a FnSigs,
        imported: &'a HashMap<String, ProcessProtocol>,
    ) -> Model<'a> {
        Model {
            protocol: local,
            sources: &[],
            type_defs: vec![],
            segments: &[],
            fn_sigs,
            imported,
            local_protocols: &[],
            prefix: String::new(),
            upper: String::new(),
            operations: HashMap::new(),
            kinds: vec![],
        }
    }

    #[test]
    fn samples_an_imported_state_through_the_owners_published_sample() {
        let imported = publishing(vec![
            "Looper.__runTraceSource__runAnswerClaimSample0".into(),
            String::new(),
        ]);
        let local = owner("__RunClaimState", vec![]);
        let fn_sigs = FnSigs::new();
        let model = importer(&local, &fn_sigs, &imported);

        // The state crosses the boundary as a value: the importer calls the
        // owner's sample and names no constructor of a type it cannot build.
        assert_eq!(
            witness(&model, "Looper.__RunClaimState").unwrap(),
            "Looper.__runTraceSource__runAnswerClaimSample0()"
        );
    }

    #[test]
    fn declines_a_state_whose_owner_publishes_no_sample() {
        // The same segment, with the sample slot left empty: nothing is
        // published for the state, and the sample stays declined.
        let imported = publishing(vec![String::new(), String::new()]);
        let local = owner("__RunClaimState", vec![]);
        let fn_sigs = FnSigs::new();
        let model = importer(&local, &fn_sigs, &imported);

        assert!(witness(&model, "Looper.__RunClaimState").is_err());
    }
}
