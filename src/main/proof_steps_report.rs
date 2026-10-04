//! Which obligations a step proof closed. The exporter writes every step
//! script to `proof_steps/<law>.steps`; a step term the kernel did not accept
//! leaves `AVER_STEPS_REJECTED:<law>` in the build log, and its law falls back
//! to the tactic portfolio behind it.

use std::collections::BTreeSet;

pub(super) struct StepsReport {
    pub emitted: BTreeSet<String>,
    pub rejected: BTreeSet<String>,
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
    let emitted: BTreeSet<String> =
        std::fs::read_dir(std::path::Path::new(output_dir).join("proof_steps"))
            .map(|dir| {
                dir.filter_map(|e| e.ok())
                    .filter_map(|e| {
                        let path = e.path();
                        (path.extension()? == "steps")
                            .then(|| path.file_stem()?.to_str().map(str::to_string))
                            .flatten()
                    })
                    .collect()
            })
            .unwrap_or_default();
    let rejected = build_log
        .lines()
        .filter_map(|line| line.split_once("AVER_STEPS_REJECTED:"))
        .map(|(_, rest)| rest.trim().trim_end_matches('"').to_string())
        .filter(|law| emitted.contains(law))
        .collect();
    StepsReport { emitted, rejected }
}
