use std::collections::{HashMap, HashSet};

/// Returns true if a declared effect satisfies a required effect.
///
/// Rules:
/// - Exact match: "Http" satisfies "Http"
/// - Parent covers child: "Http" satisfies "Http.get"
/// - Child does NOT cover parent: "Http.get" does NOT satisfy "Http"
/// - Different children: "Http.get" does NOT satisfy "Http.post"
pub fn effect_satisfies(declared: &str, required: &str) -> bool {
    if declared == required {
        return true;
    }
    // Parent covers child: declared "Http" satisfies required "Http.get"
    required.starts_with(&format!("{}.", declared))
}

/// Recursively expand effect aliases into concrete effects.
///
/// If an effect name appears as a key in `aliases`, it is replaced by
/// its expansion (recursively). A `seen` set prevents infinite loops
/// on cyclic alias definitions.
pub fn expand_effects(effects: &[String], aliases: &HashMap<String, Vec<String>>) -> Vec<String> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();
    for effect in effects {
        expand_one(effect, aliases, &mut seen, &mut result);
    }
    result
}

fn expand_one(
    effect: &str,
    aliases: &HashMap<String, Vec<String>>,
    seen: &mut HashSet<String>,
    out: &mut Vec<String>,
) {
    if !seen.insert(effect.to_string()) {
        // Already visited — cycle detected, skip to avoid infinite loop.
        return;
    }
    if let Some(expansion) = aliases.get(effect) {
        for child in expansion {
            expand_one(child, aliases, seen, out);
        }
    } else {
        out.push(effect.to_string());
    }
}

/// Detect cyclic alias definitions.
/// Returns a list of cycles, where each cycle is a Vec of alias names forming the loop.
pub fn find_cycles(aliases: &HashMap<String, Vec<String>>) -> Vec<Vec<String>> {
    let mut cycles = Vec::new();
    let mut visited = HashSet::new();

    for start in aliases.keys() {
        if visited.contains(start) {
            continue;
        }
        let mut path = Vec::new();
        let mut path_set = HashSet::new();
        find_cycle_dfs(
            start,
            aliases,
            &mut path,
            &mut path_set,
            &mut visited,
            &mut cycles,
        );
    }
    cycles
}

fn find_cycle_dfs(
    node: &str,
    aliases: &HashMap<String, Vec<String>>,
    path: &mut Vec<String>,
    path_set: &mut HashSet<String>,
    visited: &mut HashSet<String>,
    cycles: &mut Vec<Vec<String>>,
) {
    if path_set.contains(node) {
        // Found a cycle — extract it from the path
        let cycle_start = path.iter().position(|n| n == node).unwrap();
        cycles.push(path[cycle_start..].to_vec());
        return;
    }
    if visited.contains(node) {
        return;
    }

    path.push(node.to_string());
    path_set.insert(node.to_string());

    if let Some(children) = aliases.get(node) {
        for child in children {
            if aliases.contains_key(child) {
                find_cycle_dfs(child, aliases, path, path_set, visited, cycles);
            }
        }
    }

    path_set.remove(node);
    path.pop();
    visited.insert(node.to_string());
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
    fn parent_covers_child() {
        assert!(effect_satisfies("Http", "Http.get"));
        assert!(effect_satisfies("Http", "Http.post"));
        assert!(effect_satisfies("Disk", "Disk.read"));
    }

    #[test]
    fn child_does_not_cover_parent() {
        assert!(!effect_satisfies("Http.get", "Http"));
        assert!(!effect_satisfies("Disk.read", "Disk"));
    }

    #[test]
    fn different_children() {
        assert!(!effect_satisfies("Http.get", "Http.post"));
        assert!(!effect_satisfies("Disk.read", "Disk.write"));
    }

    // -- expand_effects --

    #[test]
    fn simple_alias() {
        let mut aliases = HashMap::new();
        aliases.insert(
            "IO".to_string(),
            vec!["Console".to_string(), "Disk".to_string()],
        );

        let result = expand_effects(&["IO".to_string()], &aliases);
        assert_eq!(result, vec!["Console", "Disk"]);
    }

    #[test]
    fn recursive_alias() {
        let mut aliases = HashMap::new();
        aliases.insert(
            "All".to_string(),
            vec!["IO".to_string(), "Http".to_string()],
        );
        aliases.insert(
            "IO".to_string(),
            vec!["Console".to_string(), "Disk".to_string()],
        );

        let result = expand_effects(&["All".to_string()], &aliases);
        assert_eq!(result, vec!["Console", "Disk", "Http"]);
    }

    #[test]
    fn cycle_detection() {
        let mut aliases = HashMap::new();
        aliases.insert("A".to_string(), vec!["B".to_string()]);
        aliases.insert(
            "B".to_string(),
            vec!["A".to_string(), "Console".to_string()],
        );

        // Should not loop forever; A→B→(A skipped, Console)
        let result = expand_effects(&["A".to_string()], &aliases);
        assert_eq!(result, vec!["Console"]);
    }

    #[test]
    fn no_alias_passthrough() {
        let aliases = HashMap::new();
        let result = expand_effects(&["Console".to_string(), "Http".to_string()], &aliases);
        assert_eq!(result, vec!["Console", "Http"]);
    }

    // -- find_cycles --

    #[test]
    fn find_cycles_detects_simple_cycle() {
        let mut aliases = HashMap::new();
        aliases.insert("A".to_string(), vec!["B".to_string()]);
        aliases.insert("B".to_string(), vec!["A".to_string()]);

        let cycles = find_cycles(&aliases);
        assert!(!cycles.is_empty());
    }

    #[test]
    fn find_cycles_no_cycle() {
        let mut aliases = HashMap::new();
        aliases.insert(
            "IO".to_string(),
            vec!["Console".to_string(), "Disk".to_string()],
        );

        let cycles = find_cycles(&aliases);
        assert!(cycles.is_empty());
    }
}
