//! Parsing the emitter's explicit regions and Lean's JSON diagnostics. No
//! theorem statement is reconstructed from text or generalized by this driver.

use super::*;

pub struct Region {
    pub start: usize,
    pub body: usize,
    pub end: usize,
    pub after: usize,
    pub candidate: Candidate,
}

pub fn region(text: &str) -> Result<Option<Region>, String> {
    let mut offset = 0;
    let mut begin = None;
    for line in text.split_inclusive('\n') {
        if let Some(json) = line.trim_end().strip_prefix(BEGIN) {
            if begin.is_some() {
                return Err("nested waterfall region".into());
            }
            let c = serde_json::from_str(json)
                .map_err(|e| format!("invalid waterfall metadata: {e}"))?;
            begin = Some((offset, offset + line.len(), c));
        } else if line.trim_end() == END {
            let (start, body, candidate) = begin.ok_or("unmatched waterfall region end")?;
            return Ok(Some(Region {
                start,
                body,
                end: offset,
                after: offset + line.len(),
                candidate,
            }));
        }
        offset += line.len();
    }
    if begin.is_some() {
        Err("unterminated waterfall region".into())
    } else {
        Ok(None)
    }
}

fn namespaces(prefix: &str) -> Vec<&str> {
    let mut stack = Vec::new();
    for line in prefix.lines() {
        if let Some(name) = line.strip_prefix("namespace ") {
            stack.push(name.trim());
        } else if line.starts_with("end ") {
            stack.pop();
        }
    }
    stack
}

pub fn qualified_name(prefix: &str, name: &str) -> String {
    namespaces(prefix)
        .into_iter()
        .chain(std::iter::once(name))
        .collect::<Vec<_>>()
        .join(".")
}

pub fn close_namespaces(prefix: &str) -> String {
    namespaces(prefix)
        .into_iter()
        .rev()
        .map(|name| format!("end {name}\n"))
        .collect()
}

fn messages(output: &str) -> Vec<String> {
    output
        .lines()
        .filter_map(|line| {
            let message: serde_json::Value = serde_json::from_str(line).ok()?;
            (message["severity"] == "information")
                .then(|| message["data"].as_str().map(str::to_string))
                .flatten()
        })
        .collect()
}

pub fn axioms(output: &str, name: &str) -> Option<Vec<String>> {
    let mut found = None;
    for message in messages(output) {
        let Some(rest) = message.strip_prefix(&format!("'{name}' ")) else {
            continue;
        };
        let axioms = if rest.trim() == "does not depend on any axioms" {
            Vec::new()
        } else {
            let list = rest
                .trim()
                .strip_prefix("depends on axioms: [")?
                .strip_suffix(']')?;
            list.split(',')
                .map(str::trim)
                .filter(|s| !s.is_empty())
                .map(str::to_string)
                .collect()
        };
        if found.is_some()
            || axioms.iter().any(|axiom| {
                !matches!(
                    axiom.as_str(),
                    "propext" | "Classical.choice" | "Quot.sound"
                )
            })
        {
            return None;
        }
        found = Some(axioms);
    }
    found
}

pub fn suggestion(output: &str) -> Option<String> {
    let suggestions: Vec<_> = messages(output)
        .into_iter()
        .filter_map(|message| {
            let script = message.strip_prefix("Try this:")?.trim();
            let script = script.strip_prefix("[apply]").unwrap_or(script).trim();
            (!script.is_empty()).then(|| script.to_string())
        })
        .collect();
    (suggestions.len() == 1).then(|| suggestions[0].clone())
}

pub fn imports(source: &str) -> Vec<String> {
    source
        .lines()
        .filter_map(|line| line.strip_prefix("import "))
        .flat_map(str::split_whitespace)
        .map(str::to_string)
        .collect()
}

pub fn order(
    dir: &Path,
    path: &Path,
    generated: &BTreeSet<PathBuf>,
    seen: &mut BTreeSet<PathBuf>,
    out: &mut Vec<PathBuf>,
) -> Result<(), String> {
    if !seen.insert(path.to_path_buf()) {
        return Ok(());
    }
    let source = fs::read_to_string(path).map_err(|e| e.to_string())?;
    for import in imports(&source) {
        let dependency = dir.join(import.replace('.', "/")).with_extension("lean");
        if generated.contains(&dependency) {
            order(dir, &dependency, generated, seen, out)?;
        }
    }
    out.push(path.to_path_buf());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn message(data: &str) -> String {
        serde_json::json!({"severity": "information", "data": data}).to_string()
    }

    #[test]
    fn audit_is_exact_and_fail_closed() {
        assert_eq!(
            axioms(
                &message("'M.t' depends on axioms: [propext, Quot.sound]"),
                "M.t"
            ),
            Some(vec!["propext".into(), "Quot.sound".into()])
        );
        assert_eq!(
            axioms(&message("'M.t' does not depend on any axioms"), "M.t"),
            Some(vec![])
        );
        for data in [
            "'M.t' depends on axioms: [sorryAx]",
            "'M.t' depends on axioms: [Lean.ofReduceBool]",
            "'M.t2' does not depend on any axioms",
            "'M.t' depends on axioms: garbage",
        ] {
            assert!(axioms(&message(data), "M.t").is_none());
        }
        let duplicate = message("'M.t' does not depend on any axioms");
        assert!(axioms(&format!("{duplicate}\n{duplicate}"), "M.t").is_none());
        assert!(axioms("", "M.t").is_none());
    }

    #[test]
    fn suggestion_requires_one_complete_json_message() {
        assert_eq!(
            suggestion(&message("Try this:\n  [apply] (intro x\n    rfl)")),
            Some("(intro x\n    rfl)".into())
        );
        assert!(suggestion("Try this: sorry").is_none());
        let m = message("Try this: rfl");
        assert!(suggestion(&format!("{m}\n{m}")).is_none());
    }
}
