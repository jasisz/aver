//! Warnings for module-level `effects [...]` declarations that
//! don't match what the module's functions actually use.
//!
//! Underdeclared (`fn` uses an effect outside the module's
//! `effects [...]` boundary) is already a hard type error in
//! `types::checker::check_module_effect_boundary`. This module
//! covers the soft case: the boundary lists effects that no
//! function references. Users hit this when they trim a function's
//! effects but forget to update the module header — leftover
//! entries claim a wider boundary than the code actually exercises,
//! which weakens the documentation value of `effects [...]`.

use crate::ast::TopLevel;
use crate::checker::CheckFinding;

pub fn collect_module_effects_warnings(items: &[TopLevel]) -> Vec<CheckFinding> {
    collect_module_effects_warnings_in(items, None)
}

pub fn collect_module_effects_warnings_in(
    items: &[TopLevel],
    source_file: Option<&str>,
) -> Vec<CheckFinding> {
    // Locate the module header. If `effects` is omitted entirely
    // and the module contains any function definitions, emit a soft
    // nudge to declare the boundary explicitly. Pure decision/type
    // modules don't need a boundary.
    let Some((declared_opt, declared_line, module_name, module_line)) =
        items.iter().find_map(|i| match i {
            TopLevel::Module(m) => Some((
                m.effects.clone(),
                m.effects_line,
                m.name.clone(),
                m.line,
            )),
            _ => None,
        })
    else {
        return Vec::new();
    };

    let has_fn = items.iter().any(|i| matches!(i, TopLevel::FnDef(_)));

    let Some(declared) = declared_opt else {
        if !has_fn {
            return Vec::new();
        }
        return vec![CheckFinding {
            line: module_line,
            module: Some(module_name.clone()),
            file: source_file.map(|s| s.to_string()),
            fn_name: None,
            message: format!(
                "module '{}' does not declare an effect boundary — \
                 add `effects [...]` after `intent` listing the effects \
                 functions in this module use",
                module_name
            ),
            extra_spans: vec![],
        }];
    };

    // Collect every effect any fn in this module touches. Method-level
    // entries (`Time.now`) match the same string; namespace-level fn
    // declarations (`! [Disk]`) match a declared `Disk.*` boundary
    // method only if the module declared the namespace too — otherwise
    // they're ambiguous and we skip them in the "covered" set.
    let mut used: std::collections::HashSet<String> = std::collections::HashSet::new();
    let mut used_namespaces: std::collections::HashSet<String> =
        std::collections::HashSet::new();
    for item in items {
        let TopLevel::FnDef(fd) = item else { continue };
        for eff in &fd.effects {
            let m = eff.node.as_str();
            used.insert(m.to_string());
            if let Some((ns, _)) = m.split_once('.') {
                used_namespaces.insert(ns.to_string());
            } else {
                // Namespace-level fn declaration (`! [Disk]`).
                used_namespaces.insert(m.to_string());
            }
        }
    }

    let mut warnings = Vec::new();
    for entry in declared {
        if used.contains(&entry) {
            continue;
        }
        // Namespace-level boundary entry (`Disk` covers `Disk.*`).
        // Counts as used iff at least one fn declares any
        // `<ns>.method` or the namespace itself.
        if !entry.contains('.') && used_namespaces.contains(&entry) {
            continue;
        }
        // Method-level boundary entry (`Time.now`) — must match the
        // exact string in some fn's effect list.
        warnings.push(CheckFinding {
            line: declared_line.unwrap_or(1),
            module: Some(module_name.clone()),
            file: source_file.map(|s| s.to_string()),
            fn_name: None,
            message: format!(
                "module '{}' declares effect '{}' but no function in this module uses it — \
                 trim the module's `effects [...]` to match what the code actually does",
                module_name, entry
            ),
            extra_spans: vec![],
        });
    }

    warnings
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::parse_source;

    fn warnings_for(src: &str) -> Vec<CheckFinding> {
        let items = parse_source(src).expect("parse");
        collect_module_effects_warnings(&items)
    }

    #[test]
    fn no_warning_when_every_declared_effect_is_used() {
        let src = "module M\n\
                   \x20   intent = \"t\"\n\
                   \x20   effects [Console.print]\n\
                   \nfn greet() -> Unit\n\
                   \x20   ! [Console.print]\n\
                   \x20   Console.print(\"hi\")\n";
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn warns_about_overdeclared_method_level_entry() {
        let src = "module M\n\
                   \x20   intent = \"t\"\n\
                   \x20   effects [Console.print, Time.now]\n\
                   \nfn greet() -> Unit\n\
                   \x20   ! [Console.print]\n\
                   \x20   Console.print(\"hi\")\n";
        let ws = warnings_for(src);
        assert_eq!(ws.len(), 1);
        assert!(ws[0].message.contains("Time.now"));
    }

    #[test]
    fn namespace_entry_covers_method_use() {
        let src = "module M\n\
                   \x20   intent = \"t\"\n\
                   \x20   effects [Disk]\n\
                   \nfn read() -> Result<String, String>\n\
                   \x20   ! [Disk.readText]\n\
                   \x20   Disk.readText(\"/etc/hostname\")\n";
        assert!(warnings_for(src).is_empty());
    }

    #[test]
    fn warns_when_module_omits_effects_block_with_fn() {
        // Module with a function but no `effects [...]` gets a soft
        // nudge to declare the boundary explicitly.
        let src = "module M\n\
                   \x20   intent = \"t\"\n\
                   \nfn greet() -> Unit\n\
                   \x20   ! [Console.print]\n\
                   \x20   Console.print(\"hi\")\n";
        let ws = warnings_for(src);
        assert_eq!(ws.len(), 1);
        assert!(ws[0].message.contains("does not declare an effect boundary"));
    }

    #[test]
    fn no_warning_when_module_has_no_fn() {
        // Pure decision/type module — no fns means no boundary to
        // declare. Don't badger the user with empty-effects warnings
        // for these.
        let src = "module M\n\
                   \x20   intent = \"t\"\n\
                   \ntype Color\n\
                   \x20   Red\n\
                   \x20   Blue\n";
        assert!(warnings_for(src).is_empty());
    }
}
