//! The reply sums of an answered capability (jasisz/aver#1329).
//!
//! A capability the manifest marks with `answer = "Module"` is not performed:
//! every call to one of its operations is a request, and the module named by
//! the binding computes the answer from one state. What that module returns
//! is `Tuple<S, Cap.<Op>Reply>`, and `Cap.<Op>Reply` is a sum the program
//! declares itself, in the capability module, beside the operation:
//!
//! ```text
//! type ClaimReply
//!     Now(Option<Int>)
//!     Later(Wait.Wake)
//!
//! operation claim(key: Int) -> Option<Int>
//! ```
//!
//! There is one sum per operation rather than one generic `Reply<A>` because
//! Aver has no user generics and will not grow them. `Now(v)` answers the
//! request with `v`; `Later(wake)` says the module has no answer yet and what
//! would make it worth asking again — a socket or a job (`Wait.Wake.Item`), a
//! deadline (`After`), or the next turn (`NextTurn`). The coordinator keeps
//! the state a `Later` returns, so a `Later` is where a module records its
//! own progress while the request stays where it is.
//!
//! The program writes the sum rather than the compiler generating it because
//! a program never writes a `__` name, and an answer function has to name its
//! reply type in its own signature. So the compiler checks the declaration
//! instead: [`reply_sum_fault`] is the check, run at the door for every
//! capability an `answer` binding names, and the message it hands back prints
//! the declaration the seam wants in the module's own names. A capability
//! nothing answers is not held to it — a host-provided capability has no
//! answer module and no reply.

use crate::ast::TypeDef;
use crate::capability::{CapabilityOperation, CapabilityRegistry};
use crate::types::Type;

/// The stdlib sum a `Later` carries.
pub const WAKE_TYPE: &str = "Wait.Wake";

/// The reply sum of `Cap.op`, as the answer module names it: `Pool.claim` is
/// answered with `Pool.ClaimReply`, `Chain.nextTarget` with
/// `Chain.NextTargetReply`.
pub fn reply_type_name(capability: &str, operation: &str) -> String {
    format!("{capability}.{}", reply_type_leaf(operation))
}

/// The reply sum's own name inside the capability module: `claim` ->
/// `ClaimReply`. Only the first letter is upper-cased.
pub fn reply_type_leaf(operation: &str) -> String {
    let mut chars = operation.chars();
    let head = match chars.next() {
        Some(head) => head.to_uppercase().collect::<String>(),
        None => String::new(),
    };
    format!("{head}{}Reply", chars.as_str())
}

/// The declaration the `answer-shape` diagnostic prints: the reply sum of
/// `operation`, spelled out in the capability's own names.
pub fn expected_reply(capability: &str, operation: &CapabilityOperation) -> String {
    format!(
        "    type {}\n        Now({})\n        Later({WAKE_TYPE})",
        reply_type_leaf(&operation.name),
        answer_type(capability, operation).display()
    )
}

/// Why the reply sum `capability` declares for `operation` is not the shape
/// the seam reads an answer through — or `None` when it is exactly that
/// shape: a sum named `<Op>Reply` with two constructors, `Now` carrying the
/// operation's result type and `Later` carrying `Wait.Wake`.
///
/// The text names the fault in the module's real names and ends with the
/// declaration wanted, so the caller only has to say which binding asked.
pub fn reply_sum_fault(
    registry: &CapabilityRegistry,
    capability: &str,
    operation: &CapabilityOperation,
) -> Option<String> {
    let name = reply_type_name(capability, &operation.name);
    let wanted = format!(
        "an answer to '{capability}.{}' is read through that sum, which is exactly:\n{}",
        operation.name,
        expected_reply(capability, operation)
    );
    let Some(declared) = registry.boundary_type(&name) else {
        return Some(format!(
            "it declares no type '{name}' beside operation '{}'; {wanted}",
            operation.name
        ));
    };
    let variants = match declared {
        TypeDef::Sum { variants, .. } => variants,
        TypeDef::Product { .. } => {
            return Some(format!("'{name}' is a record; {wanted}"));
        }
    };
    let expected = answer_type(capability, operation);
    for variant in variants {
        match variant.name.as_str() {
            "Now" => {
                let carried = variant.fields.first().map(|field| {
                    super::canonicalize_type_names(crate::types::parse_type_str(field), capability)
                });
                let agrees = variant.fields.len() == 1
                    && carried
                        .as_ref()
                        .is_some_and(|carried| same_type(carried, &expected));
                if !agrees {
                    return Some(format!(
                        "constructor '{name}.Now' carries ({}) where '{capability}.{}' answers {}; {wanted}",
                        variant.fields.join(", "),
                        operation.name,
                        expected.display()
                    ));
                }
            }
            "Later" => {
                let agrees = variant.fields.len() == 1 && variant.fields[0].trim() == WAKE_TYPE;
                if !agrees {
                    return Some(format!(
                        "constructor '{name}.Later' carries ({}) where a Later carries the wake, {WAKE_TYPE}; {wanted}",
                        variant.fields.join(", ")
                    ));
                }
            }
            other => {
                return Some(format!(
                    "sum '{name}' declares constructor '{other}', which is neither Now nor Later; {wanted}"
                ));
            }
        }
    }
    for constructor in ["Now", "Later"] {
        if !variants.iter().any(|variant| variant.name == constructor) {
            return Some(format!(
                "sum '{name}' has no constructor '{constructor}'; {wanted}"
            ));
        }
    }
    None
}

/// The operation's result type in the capability's own scope: what `Now`
/// carries.
fn answer_type(capability: &str, operation: &CapabilityOperation) -> Type {
    super::canonicalize_type_names(operation.return_type.clone(), capability)
}

/// Nominal identity for the payload comparison, the same one the binding
/// check uses for an answer function's signature.
fn same_type(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::Named { name: left, .. }, Type::Named { name: right, .. }) => left == right,
        (Type::Result(la, lb), Type::Result(ra, rb)) | (Type::Map(la, lb), Type::Map(ra, rb)) => {
            same_type(la, ra) && same_type(lb, rb)
        }
        (Type::Option(left), Type::Option(right))
        | (Type::List(left), Type::List(right))
        | (Type::Vector(left), Type::Vector(right)) => same_type(left, right),
        (Type::Tuple(left), Type::Tuple(right)) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| same_type(left, right))
        }
        _ => left == right,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const CLOCK: &str = "\
module Clock
    kind = capability
    semantics = effectful
    intent = \"The tick a ticker waits on.\"
    exposes [Tick, TickReply, tick]

type Tick
    Tock
    Closed(Int)

type TickReply
    Now(Clock.Tick)
    Later(Wait.Wake)

operation tick() -> Clock.Tick
    ? \"The next tick.\"
    oracle = generative
    replay = recorded

operation gone(key: Int) -> Unit
    ? \"This ticker is finished.\"
    oracle = generativeOutput
    replay = recorded
";

    fn registry_of(source: &str) -> CapabilityRegistry {
        let items = crate::source::parse_source(source).expect("capability parses");
        let (registry, errors) = CapabilityRegistry::from_module("Clock", &items);
        assert!(errors.is_empty(), "contract errors: {errors:?}");
        registry
    }

    fn operation<'a>(registry: &'a CapabilityRegistry, name: &str) -> &'a CapabilityOperation {
        registry
            .operation(&format!("Clock.{name}"))
            .expect("the operation is declared")
    }

    #[test]
    fn the_reply_name_uppercases_only_the_first_letter() {
        assert_eq!(reply_type_name("Pool", "claim"), "Pool.ClaimReply");
        assert_eq!(
            reply_type_name("Chain", "nextTarget"),
            "Chain.NextTargetReply"
        );
    }

    #[test]
    fn a_declared_reply_sum_of_the_right_shape_passes() {
        let registry = registry_of(CLOCK);
        assert_eq!(
            reply_sum_fault(&registry, "Clock", operation(&registry, "tick")),
            None
        );
    }

    #[test]
    fn a_missing_reply_sum_prints_the_declaration_wanted() {
        let registry = registry_of(CLOCK);
        let fault = reply_sum_fault(&registry, "Clock", operation(&registry, "gone"))
            .expect("gone has no reply sum");
        assert!(
            fault.starts_with("it declares no type 'Clock.GoneReply' beside operation 'gone'"),
            "{fault}"
        );
        assert!(
            fault.ends_with("    type GoneReply\n        Now(Unit)\n        Later(Wait.Wake)"),
            "{fault}"
        );
    }

    #[test]
    fn a_now_carrying_something_else_is_named() {
        let registry = registry_of(&CLOCK.replace("Now(Clock.Tick)", "Now(Int)"));
        let fault = reply_sum_fault(&registry, "Clock", operation(&registry, "tick"))
            .expect("the payload is wrong");
        assert!(
            fault.starts_with(
                "constructor 'Clock.TickReply.Now' carries (Int) where 'Clock.tick' answers Clock.Tick"
            ),
            "{fault}"
        );
    }

    #[test]
    fn a_bare_local_name_in_now_is_the_same_type() {
        let registry = registry_of(&CLOCK.replace("Now(Clock.Tick)", "Now(Tick)"));
        assert_eq!(
            reply_sum_fault(&registry, "Clock", operation(&registry, "tick")),
            None
        );
    }

    #[test]
    fn a_later_carrying_something_else_is_named() {
        let registry = registry_of(&CLOCK.replace("Later(Wait.Wake)", "Later(Int)"));
        let fault = reply_sum_fault(&registry, "Clock", operation(&registry, "tick"))
            .expect("the wake is wrong");
        assert!(
            fault.starts_with(
                "constructor 'Clock.TickReply.Later' carries (Int) where a Later carries the wake, Wait.Wake"
            ),
            "{fault}"
        );
    }

    #[test]
    fn a_third_constructor_is_named() {
        let registry = registry_of(&CLOCK.replace(
            "    Later(Wait.Wake)\n",
            "    Later(Wait.Wake)\n    Never\n",
        ));
        let fault = reply_sum_fault(&registry, "Clock", operation(&registry, "tick"))
            .expect("a third constructor is refused");
        assert!(
            fault.starts_with(
                "sum 'Clock.TickReply' declares constructor 'Never', which is neither Now nor Later"
            ),
            "{fault}"
        );
    }

    #[test]
    fn a_missing_constructor_is_named() {
        let registry = registry_of(&CLOCK.replace("    Later(Wait.Wake)\n", ""));
        let fault = reply_sum_fault(&registry, "Clock", operation(&registry, "tick"))
            .expect("a sum without Later is refused");
        assert!(
            fault.starts_with("sum 'Clock.TickReply' has no constructor 'Later'"),
            "{fault}"
        );
    }

    #[test]
    fn a_record_of_the_reply_name_is_named() {
        let registry = registry_of(&CLOCK.replace(
            "type TickReply\n    Now(Clock.Tick)\n    Later(Wait.Wake)\n",
            "record TickReply\n    now: Clock.Tick\n",
        ));
        let fault = reply_sum_fault(&registry, "Clock", operation(&registry, "tick"))
            .expect("a record is refused");
        assert!(
            fault.starts_with("'Clock.TickReply' is a record"),
            "{fault}"
        );
    }
}
