/// Marker used only inside a direct callback parameter type:
/// `Fn(A) -> B ! [_]` accepts the concrete named callback's statically known
/// effects and forwards them to the call site. It is not an ambient effect and
/// never satisfies an ordinary function effect declaration.
pub const FORWARDED_CALLBACK_EFFECT: &str = "_";

pub fn forwards_callback_effects(effects: &[String]) -> bool {
    effects == [FORWARDED_CALLBACK_EFFECT]
}

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
    if !declared.contains('.')
        && let Some(prefix) = required.split('.').next()
    {
        return declared == prefix;
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

    #[test]
    fn callback_forwarding_marker_is_not_an_ambient_effect() {
        assert!(forwards_callback_effects(&["_".to_string()]));
        assert!(!forwards_callback_effects(&[
            "_".to_string(),
            "Console.print".to_string(),
        ]));
        assert!(!effect_satisfies("_", "Console.print"));
        assert!(!effect_satisfies("Console.print", "_"));
    }
}
