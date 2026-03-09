/// Returns true if a declared effect satisfies a required effect.
///
/// Rule:
/// - Exact match only: "Http.get" satisfies "Http.get"
pub fn effect_satisfies(declared: &str, required: &str) -> bool {
    declared == required
}

#[cfg(test)]
mod tests {
    use super::*;

    // -- effect_satisfies --

    #[test]
    fn exact_match() {
        assert!(effect_satisfies("Http", "Http"));
        assert!(effect_satisfies("Console", "Console"));
    }

    #[test]
    fn child_does_not_cover_parent() {
        assert!(!effect_satisfies("Http.get", "Http"));
        assert!(!effect_satisfies("Disk.read", "Disk"));
    }

    #[test]
    fn parent_does_not_cover_child() {
        assert!(!effect_satisfies("Http", "Http.get"));
        assert!(!effect_satisfies("Http", "Http.post"));
        assert!(!effect_satisfies("Disk", "Disk.read"));
    }

    #[test]
    fn different_children() {
        assert!(!effect_satisfies("Http.get", "Http.post"));
        assert!(!effect_satisfies("Disk.read", "Disk.write"));
    }
}
