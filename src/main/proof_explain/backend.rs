//! Map errors from the counted build to source-claim identities. A failed
//! diagnostic probe never enters this channel and cannot change proof credit.

use std::collections::BTreeMap;
use std::path::Path;

#[derive(Clone)]
pub(super) struct Failure {
    pub status: &'static str,
    pub message: &'static str,
}

#[derive(Default)]
pub(super) struct Failures {
    pub claims: BTreeMap<String, Failure>,
    pub unmapped: bool,
}

pub(super) fn failures(dir: &str, output: &str) -> Failures {
    let mut result = Failures::default();
    let lines: Vec<_> = output.lines().collect();
    for (index, line) in lines.iter().enumerate() {
        if !line.trim_start().starts_with("error:") {
            continue;
        }
        let Some((file, line_number)) = super::super::parse_lean_decl_location(line) else {
            // Aggregate build failures accompany the located error. Other
            // errors (e.g. a missing dependency) need their own fallback.
            let message = line.trim_start().trim_start_matches("error:").trim();
            if !message.starts_with("Lean exited with code")
                && !message.starts_with("build failed")
                && !message.starts_with("some required builds logged failures")
            {
                result.unmapped = true;
            }
            continue;
        };
        // Only the emitted project can supply a declaration/claim mapping.
        let path = Path::new(&file);
        if path.is_absolute()
            || path
                .components()
                .any(|part| matches!(part, std::path::Component::ParentDir))
        {
            result.unmapped = true;
            continue;
        }
        let claim = std::fs::read_to_string(Path::new(dir).join(path))
            .ok()
            .and_then(|source| enclosing_claim(&source, line_number));
        let Some(claim) = claim else {
            result.unmapped = true;
            continue;
        };
        let continuation = std::iter::once(*line)
            .chain(
                lines[index + 1..]
                    .iter()
                    .copied()
                    .take_while(|line| !line.trim_start().starts_with("error:"))
                    .take(6),
            )
            .collect::<Vec<_>>()
            .join("\n");
        let failure = if continuation.contains("timeout")
            || continuation.contains("maximum recursion depth")
        {
            Failure {
                status: "checker_limit",
                message: "The proof checker reached its computation limit at this step; the statement has not been disproved.",
            }
        } else {
            Failure {
                status: "checker_error",
                message: "The proof checker could not check the generated proof for this step; this is not a mathematical counterexample.",
            }
        };
        result.claims.insert(claim, failure);
    }
    result
}

fn enclosing_claim(source: &str, target: usize) -> Option<String> {
    let mut labels = BTreeMap::<String, String>::new();
    let mut current = None;
    for line in source.lines().take(target) {
        if let Some(marker) = line
            .strip_prefix(aver::codegen::lean::LAW_CLASS_MARKER_PREFIX)
            .or_else(|| line.strip_prefix(aver::codegen::lean::LAW_OBLIGATION_MARKER_PREFIX))
        {
            let fields: Vec<_> = marker.split_whitespace().collect();
            if fields.len() >= 3 {
                labels.insert(fields[0].to_string(), fields[2].to_string());
            }
        }
        let declaration = line
            .strip_prefix("theorem ")
            .or_else(|| line.strip_prefix("private theorem "));
        if let Some(declaration) = declaration {
            let name = declaration.split_whitespace().next()?.trim_end_matches(':');
            current = labels.get(name).cloned();
        } else if line.starts_with("def ")
            || line.starts_with("partial def ")
            || line.starts_with("private def ")
            || line.starts_with("end ")
        {
            // An error in a subsequent function must not blame a preceding law.
            current = None;
        }
    }
    current
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn errors_belong_to_the_marked_step_not_the_following_definition() {
        let src = "-- aver:law-obligation reason universal f.law.because2\ntheorem reason : True := by\n  trivial\ndef later := 0\n";
        assert_eq!(enclosing_claim(src, 3).as_deref(), Some("f.law.because2"));
        assert_eq!(enclosing_claim(src, 4), None);
    }
}
