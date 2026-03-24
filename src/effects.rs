/// Returns true if a declared effect satisfies a required effect.
///
/// Rules:
/// - Exact match: "Http.get" satisfies "Http.get"
/// - Namespace shorthand: "Http" satisfies "Http.get" (parent covers all children)
/// - But NOT reverse: "Http.get" does NOT satisfy "Http"
pub fn effect_satisfies(declared: &str, required: &str) -> bool {
    if declared == required {
        return true;
    }
    // Namespace shorthand: "Disk" covers "Disk.readText"
    if !declared.contains('.') {
        if let Some(prefix) = required.split('.').next() {
            return declared == prefix;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_match() {
        assert!(effect_satisfies("Http", "Http"));
        assert!(effect_satisfies("Console", "Console"));
        assert!(effect_satisfies("Http.get", "Http.get"));
        assert!(effect_satisfies("Disk.readText", "Disk.readText"));
    }

    #[test]
    fn namespace_covers_children() {
        assert!(effect_satisfies("Http", "Http.get"));
        assert!(effect_satisfies("Http", "Http.post"));
        assert!(effect_satisfies("Disk", "Disk.readText"));
        assert!(effect_satisfies("Disk", "Disk.writeText"));
        assert!(effect_satisfies("Terminal", "Terminal.clear"));
    }

    #[test]
    fn child_does_not_cover_parent() {
        assert!(!effect_satisfies("Http.get", "Http"));
        assert!(!effect_satisfies("Disk.readText", "Disk"));
    }

    #[test]
    fn different_children() {
        assert!(!effect_satisfies("Http.get", "Http.post"));
        assert!(!effect_satisfies("Disk.readText", "Disk.writeText"));
    }

    #[test]
    fn no_cross_namespace() {
        assert!(!effect_satisfies("Http", "Disk.readText"));
        assert!(!effect_satisfies("Console", "Terminal.clear"));
    }
}
