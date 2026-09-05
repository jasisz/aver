//! Residuals of the original explanation obligations, from the counted Lean
//! build. These are diagnostics only: the ordinary axiom audit grants credit.

use super::{LawTier, ManifestLaw};
use std::collections::BTreeMap;

pub(super) fn attach_residuals(records: &mut [ManifestLaw], output: &str) {
    let mut goals = BTreeMap::<String, String>::new();
    let mut current: Option<String> = None;
    let mut capturing = false;
    for line in output.lines() {
        if let Some((_, marker)) = line.split_once("AVER_REASON_OPEN:") {
            let label = marker.split(':').next().unwrap_or_default().trim();
            current = Some(label.to_string());
            capturing = false;
            continue;
        }
        if line.starts_with("warning:") || line.starts_with("error:") {
            current = None;
        }
        let Some(label) = &current else { continue };
        let text = if line.starts_with("info:") {
            let Some((_, position)) = line.split_once(".lean:") else {
                continue;
            };
            let Some(payload) = position.splitn(3, ':').nth(2) else {
                continue;
            };
            capturing = true;
            payload.trim_start()
        } else if capturing {
            line
        } else {
            continue;
        };
        let goal = goals.entry(label.clone()).or_default();
        if !goal.is_empty() {
            goal.push('\n');
        }
        goal.push_str(text);
    }
    for record in records {
        if record.tier == LawTier::Failed {
            record.open_goal = goals.remove(&record.law).filter(|g| !g.trim().is_empty());
        }
    }
}
