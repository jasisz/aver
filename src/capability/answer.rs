//! The reply sums of an answered capability (jasisz/aver#1329, leg 2.2).
//!
//! A capability the manifest marks with `answer = "Module"` is not performed:
//! every call to one of its operations is a request, and the module named by
//! the binding computes the answer from one state. What that module returns
//! is `Tuple<S, Cap.__<Op>Reply>`, and this is where `Cap.__<Op>Reply` comes
//! from — one sum per operation, generated into the capability module itself:
//!
//! ```text
//! type __ClaimReply
//!     Now(Option<Int>)
//!     Later(Wait.Wake)
//! ```
//!
//! There is one sum per operation rather than one generic `Reply<A>` because
//! Aver has no user generics and will not grow them. `Now(v)` answers the
//! request with `v`; `Later(wake)` says the module has no answer yet and what
//! would make it worth asking again — a socket or a job (`Wait.Wake.Item`), a
//! deadline (`After`), or the next turn (`NextTurn`). The coordinator discards
//! the state a `Later` returns, so "a `Later` leaves the answer state
//! unchanged" holds by construction rather than by a law.
//!
//! The generation runs at load time, on the module as parsed, before the
//! dependency walk follows its edges: the sums name `Wait.Wake`, so the
//! capability module gains `depends [Wait]` and the walk then loads `Wait` the
//! way it loads any written dependency.

use crate::ast::{CapabilityItem, Module, TopLevel, TypeDef, TypeVariant};
use crate::config::MarkedCapabilities;

/// The module holding the reply type of `Cap.op`: `Pool.claim` is answered
/// with `Pool.__ClaimReply`.
pub fn reply_type_name(operation: &str) -> String {
    let mut chars = operation.chars();
    let head = match chars.next() {
        Some(head) => head.to_uppercase().collect::<String>(),
        None => String::new(),
    };
    format!("__{head}{}Reply", chars.as_str())
}

/// The stdlib sum a `Later` carries.
const WAKE_TYPE: &str = "Wait.Wake";
/// The module that owns it, which a marked capability therefore depends on.
const WAIT_MODULE: &str = "Wait";

/// Generate the reply sums of `items` when the module it declares is a
/// capability `marked` answers, and give it `depends [Wait]` for the type
/// they carry. Returns whether anything was generated.
///
/// Idempotent: a module already carrying a reply sum for every operation is
/// left exactly as it is, so a loader that reaches one module twice does not
/// declare its types twice.
pub fn generate_reply_types(items: &mut Vec<TopLevel>, marked: &MarkedCapabilities) -> bool {
    if marked.is_empty() {
        return false;
    }
    let Some(module) = module_decl(items) else {
        return false;
    };
    if !marked.marks(&module.name) {
        return false;
    }
    let existing: Vec<String> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::TypeDef(TypeDef::Sum { name, .. })
            | TopLevel::TypeDef(TypeDef::Product { name, .. }) => Some(name.clone()),
            _ => None,
        })
        .collect();
    let mut generated: Vec<(String, TypeDef)> = Vec::new();
    for item in items.iter() {
        let TopLevel::Capability(CapabilityItem::Operation(operation)) = item else {
            continue;
        };
        let name = reply_type_name(&operation.name);
        if existing.contains(&name) || generated.iter().any(|(other, _)| other == &name) {
            continue;
        }
        generated.push((
            name.clone(),
            TypeDef::Sum {
                name,
                variants: vec![
                    TypeVariant {
                        name: "Now".to_string(),
                        fields: vec![operation.return_type.clone()],
                    },
                    TypeVariant {
                        name: "Later".to_string(),
                        fields: vec![WAKE_TYPE.to_string()],
                    },
                ],
                line: operation.line,
            },
        ));
    }
    if generated.is_empty() {
        return false;
    }

    let names: Vec<String> = generated.iter().map(|(name, _)| name.clone()).collect();
    if let Some(module) = module_decl_mut(items) {
        for name in &names {
            if !module.exposes.contains(name) {
                module.exposes.push(name.clone());
            }
        }
        if !module.depends.iter().any(|dep| dep == WAIT_MODULE) {
            module.depends.push(WAIT_MODULE.to_string());
        }
    }
    items.extend(generated.into_iter().map(|(_, def)| TopLevel::TypeDef(def)));
    true
}

fn module_decl(items: &[TopLevel]) -> Option<&Module> {
    items.iter().find_map(|item| match item {
        TopLevel::Module(module) => Some(module),
        _ => None,
    })
}

fn module_decl_mut(items: &mut [TopLevel]) -> Option<&mut Module> {
    items.iter_mut().find_map(|item| match item {
        TopLevel::Module(module) => Some(module),
        _ => None,
    })
}
