//! Host-side utilities that are not effect dispatch.
//!
//! Every standard host effect is declared by an embedded Aver capability and
//! dispatched through the checked provider registry. The sole remaining
//! module exposes capture-aware console sinks to embedded runtimes and tests.

pub mod console;

/// Every standard effect name, derived from source-owned contracts.
pub fn all_effect_names() -> Vec<&'static str> {
    crate::stdlib::standard_capability_registry_ref()
        .operations()
        .filter(|operation| operation.is_effectful())
        .map(|operation| operation.canonical_name.as_str())
        .collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn effect_names_are_exactly_the_standard_capability_operations() {
        let listed = super::all_effect_names();
        let canonical = crate::stdlib::standard_capability_registry_ref()
            .operations()
            .filter(|operation| operation.is_effectful())
            .map(|operation| operation.canonical_name.as_str())
            .collect::<Vec<_>>();
        assert_eq!(listed, canonical);
    }
}
