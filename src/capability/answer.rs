//! The shape one answer function has.
//!
//! A module that says `answers [Pool]` in its header answers every operation
//! of `Pool` with one function of the same name, `op(state, args...)`, which
//! returns the state it leaves behind and either the operation's own result
//! or the wake that says when the request is asked again:
//!
//! ```text
//! fn claim(state: State, key: Int) -> Tuple<State, Result<Pool.Assignment, Run.Wake>>
//! ```
//!
//! `Result.Ok(v)` answers the request now. `Result.Err(wake)` parks it, keeps
//! the state the function returned, and asks again once the wake has fired.
//! Both halves are types the language already has, so a capability declares
//! nothing beside its operations for a module to answer it.

use crate::ast::Type;

use super::CapabilityOperation;

/// The type a parked request carries, in the loop's own standard module.
pub const WAKE_TYPE: &str = "Run.Wake";

/// What the answer function of `operation` returns, given the state its
/// module threads: `Tuple<S, Result<R, Run.Wake>>`, where `R` is the
/// operation's result read in the capability's own scope.
pub fn answer_result(state: &Type, capability: &str, operation: &CapabilityOperation) -> Type {
    Type::Tuple(vec![
        state.clone(),
        Type::Result(
            Box::new(super::canonicalize_type_names(
                operation.return_type.clone(),
                capability,
            )),
            Box::new(Type::named(WAKE_TYPE.to_string())),
        ),
    ])
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capability::CapabilityRegistry;

    const CLOCK: &str = "\
module Clock
    kind = capability
    semantics = effectful
    exposes [Tick, tick]

type Tick
    Tock
    Closed

operation tick() -> Tick
    ? \"The next tick.\"
    oracle = generative
    replay = recorded
";

    #[test]
    fn the_answer_result_carries_the_state_the_result_and_the_wake() {
        let items = crate::source::parse_source(CLOCK).expect("capability parses");
        let (registry, errors) = CapabilityRegistry::from_module("Clock", &items);
        assert!(errors.is_empty(), "{errors:?}");
        let operation = registry
            .operations()
            .find(|operation| operation.name == "tick")
            .expect("tick");
        let state = Type::named("Ticker.State".to_string());
        assert_eq!(
            answer_result(&state, "Clock", operation).display(),
            "Tuple<Ticker.State, Result<Clock.Tick, Run.Wake>>"
        );
    }
}
