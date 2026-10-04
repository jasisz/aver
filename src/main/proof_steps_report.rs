//! Which obligations a step proof closed. The exporter writes every step
//! script to `proof_steps/<law>.steps`, and why it wrote none to
//! `proof_steps/<law>.refused`. A step term the Lean kernel does not accept
//! leaves `AVER_STEPS_REJECTED:<law>` in the build log: the producer wrote a
//! wrong proof, which fails the check (the law's tactics still run behind
//! it, so the rest of the report stays meaningful).

use std::collections::{BTreeMap, BTreeSet};

pub(super) struct StepsReport {
    pub emitted: BTreeSet<String>,
    pub rejected: BTreeSet<String>,
    /// Why no steps were written, by law.
    pub refused: BTreeMap<String, String>,
}

impl StepsReport {
    /// How an obligation was closed: `steps`, `tactic`, or `open`.
    pub fn closed_by(&self, obligation: &str, universal: bool) -> &'static str {
        let law = obligation
            .strip_suffix(".implication")
            .unwrap_or(obligation);
        if !universal {
            "open"
        } else if self.emitted.contains(law) && !self.rejected.contains(law) {
            "steps"
        } else {
            "tactic"
        }
    }
}

pub(super) fn collect(output_dir: &str, build_log: &str) -> StepsReport {
    let files: Vec<std::path::PathBuf> =
        std::fs::read_dir(std::path::Path::new(output_dir).join("proof_steps"))
            .map(|dir| dir.filter_map(|e| e.ok()).map(|e| e.path()).collect())
            .unwrap_or_default();
    let with = |ext: &str| -> Vec<(String, std::path::PathBuf)> {
        files
            .iter()
            .filter(|p| p.extension().is_some_and(|e| e == ext))
            .filter_map(|p| Some((p.file_stem()?.to_str()?.to_string(), p.clone())))
            .collect()
    };
    // A script Lean was given a step term for: its fallback names it in the
    // generated sources. A script the renderer could not spell in Lean
    // leaves the law to its tactics alone.
    let mut lean_sources = String::new();
    read_lean_sources(std::path::Path::new(output_dir), &mut lean_sources);
    let emitted: BTreeSet<String> = with("steps")
        .into_iter()
        .map(|(law, _)| law)
        .filter(|law| {
            lean_sources.is_empty()
                || lean_sources.contains(&format!("AVER_STEPS_REJECTED:{law}\""))
        })
        .collect();
    let refused: BTreeMap<String, String> = with("refused")
        .into_iter()
        .filter_map(|(law, p)| Some((law, std::fs::read_to_string(p).ok()?.trim().to_string())))
        .collect();
    let rejected = build_log
        .lines()
        .filter_map(|line| line.split_once("AVER_STEPS_REJECTED:"))
        .map(|(_, rest)| rest.trim().trim_end_matches('"').to_string())
        .filter(|law| emitted.contains(law))
        .collect();
    StepsReport {
        emitted,
        rejected,
        refused,
    }
}

/// Every generated `.lean` file under `dir`, the build directory aside.
fn read_lean_sources(dir: &std::path::Path, out: &mut String) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n != ".lake") {
                read_lean_sources(&path, out);
            }
        } else if path.extension().is_some_and(|e| e == "lean")
            && let Ok(text) = std::fs::read_to_string(&path)
        {
            out.push_str(&text);
        }
    }
}
