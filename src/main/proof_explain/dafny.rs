//! Associate diagnostics with emitted declarations, never with an adjacent law.
//! Successful sublemmas in a failing module do not receive independent credit.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use super::Catalog;
use super::backend::{Failure, Failures};

// Follow exactly the checker entry's includes. Unrelated or stale .dfy files
// in a reused output directory did not participate in this check.
fn files(root: &Path, path: &Path, seen: &mut BTreeSet<PathBuf>, output: &mut Vec<PathBuf>) {
    let Ok(path) = path.canonicalize() else {
        return;
    };
    if !path.starts_with(root) || !seen.insert(path.clone()) {
        return;
    }
    let Ok(source) = std::fs::read_to_string(&path) else {
        return;
    };
    for line in source.lines() {
        if let Some(include) = line
            .trim()
            .strip_prefix("include \"")
            .and_then(|s| s.strip_suffix('"'))
        {
            files(root, &path.parent().unwrap().join(include), seen, output);
        }
    }
    output.push(path);
}

fn locations(source: &str, catalog: &Catalog, relative: &str) -> Vec<Option<String>> {
    let mut current = None;
    let mut pending = None;
    let mut support_group = false;
    let mut out = Vec::new();
    for line in source.lines() {
        let line = line.trim_start();
        if let Some(marker) = line
            .strip_prefix("// aver:dafny-obligation ")
            .or_else(|| line.strip_prefix("// aver:dafny-law "))
            .or_else(|| line.strip_prefix("// aver:dafny-citation "))
        {
            pending = marker.split_whitespace().nth(1).map(str::to_string);
            support_group = false;
        } else if let Some(marker) = line.strip_prefix("// Law: ") {
            let local = marker.split_whitespace().next().unwrap_or("");
            pending = catalog
                .laws
                .values()
                .find(|law| {
                    law.dafny_file == relative
                        && local
                            == format!(
                                "{}.{}",
                                law.function.rsplit('.').next().unwrap_or(&law.function),
                                law.body.name
                            )
                })
                .map(|law| law.id.clone());
            support_group = true;
        } else if line.starts_with("lemma ") || line.starts_with("ghost lemma ") {
            current = pending.clone();
            if !support_group {
                pending = None;
            }
        } else if [
            "function ",
            "ghost function ",
            "opaque function ",
            "method ",
            "ghost method ",
            "predicate ",
            "datatype ",
            "module ",
        ]
        .iter()
        .any(|p| line.starts_with(p))
        {
            current = None;
            pending = None;
        }
        out.push(current.clone());
    }
    out
}

pub(super) fn scan(
    catalog: &Catalog,
    dir: &str,
    entry: &str,
    output: &str,
) -> (Failures, BTreeSet<String>) {
    let mut failures = Failures::default();
    let mut exported = BTreeSet::new();
    let Ok(root) = Path::new(dir).canonicalize() else {
        failures.unmapped = true;
        return (failures, exported);
    };
    let mut paths = Vec::new();
    files(&root, &root.join(entry), &mut BTreeSet::new(), &mut paths);
    let mut maps = BTreeMap::new();
    for path in paths {
        let Ok(source) = std::fs::read_to_string(&path) else {
            continue;
        };
        let relative = path.strip_prefix(&root).unwrap().to_string_lossy();
        let map = locations(&source, catalog, &relative);
        exported.extend(
            map.iter()
                .flatten()
                .filter(|id| catalog.claim(id).is_some())
                .cloned(),
        );
        maps.insert(path, map);
    }
    for line in output.lines() {
        let Some((location, message)) = line.split_once(": Error:") else {
            if line.starts_with("Error:") {
                failures.unmapped = true;
            }
            continue;
        };
        let Some((file, position)) = location.rsplit_once('(') else {
            failures.unmapped = true;
            continue;
        };
        let number = position
            .split(',')
            .next()
            .and_then(|s| s.parse::<usize>().ok());
        let path = root.join(file.trim()).canonicalize().ok();
        let identity = path
            .as_ref()
            .and_then(|path| maps.get(path))
            .and_then(|map| {
                number
                    .and_then(|n| n.checked_sub(1))
                    .and_then(|i| map.get(i))
            })
            .and_then(Option::as_ref);
        let Some(identity) = identity else {
            failures.unmapped = true;
            continue;
        };
        let issue = classify(message);
        let priority = |status| match status {
            "checker_error" => 3,
            "checker_limit" => 2,
            _ => 1,
        };
        if failures
            .claims
            .get(identity)
            .is_none_or(|old| priority(issue.status) >= priority(old.status))
        {
            failures.claims.insert(identity.clone(), issue);
        }
    }
    (failures, exported)
}

fn classify(message: &str) -> Failure {
    if message.contains("timed out") || message.contains("resource limit") {
        Failure {
            status: "checker_limit",
            message: "The proof checker reached its computation limit at this step; the statement has not been disproved.",
        }
    } else if ["might not hold", "could not be proved", "decreases"]
        .iter()
        .any(|s| message.contains(s))
    {
        Failure {
            status: "unproved",
            message: "This step has not been proved from the current assumptions.",
        }
    } else {
        Failure {
            status: "checker_error",
            message: "The proof checker could not check the generated proof for this step; this is not a mathematical counterexample.",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn citation_restatements_do_not_inherit_the_previous_law() {
        let source = "// aver:dafny-law previous consumer.old\nlemma previous() {}\n// aver:dafny-citation supplier Lib.f.guarded\n// Checked universal citation: f.guarded\nlemma supplier()\n{ assert false; }\nlemma unmarked() {}\n";
        let map = locations(source, &Catalog::default(), "Entry.dfy");
        assert_eq!(map[5].as_deref(), Some("Lib.f.guarded"));
        assert_eq!(map[6], None);
    }

    #[test]
    fn inventory_follows_only_the_checked_include_graph() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir(dir.path().join("Domain")).unwrap();
        std::fs::write(dir.path().join("Entry.dfy"), "include \"Domain/Lib.dfy\"\n").unwrap();
        std::fs::write(
            dir.path().join("Domain/Lib.dfy"),
            "include \"../Entry.dfy\"\n",
        )
        .unwrap();
        std::fs::write(dir.path().join("Stale.dfy"), "lemma neverChecked() {}\n").unwrap();
        let root = dir.path().canonicalize().unwrap();
        let mut paths = Vec::new();
        files(
            &root,
            &root.join("Entry.dfy"),
            &mut BTreeSet::new(),
            &mut paths,
        );
        assert_eq!(paths.len(), 2);
        assert!(!paths.iter().any(|p| p.ends_with("Stale.dfy")));
    }

    #[test]
    fn a_later_definition_never_inherits_a_law_and_limits_are_not_counterexamples() {
        let source = "// aver:dafny-obligation step f.law.because1\nlemma step()\n{ assert false; }\nfunction later(): int { 0 }\n";
        let map = locations(source, &Catalog::default(), "main.dfy");
        assert_eq!(map[2].as_deref(), Some("f.law.because1"));
        assert_eq!(map[3], None);
        assert_eq!(
            classify("Verification timed out after 30 seconds").status,
            "checker_limit"
        );
        assert_eq!(classify("assertion might not hold").status, "unproved");
        assert_eq!(classify("unknown identifier").status, "checker_error");
    }
}
