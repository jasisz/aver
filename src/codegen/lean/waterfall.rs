//! Compiler-owned search boundaries. Only the emitter may generalize a sampled
//! law: its ordinary statement builder retains guards and refinement binders.
//! The CLI replaces these regions only after replay and a transitive axiom audit.

use std::cell::Cell;

use serde::{Deserialize, Serialize};

pub const BEGIN: &str = "-- aver:waterfall ";
pub const END: &str = "-- aver:waterfall-end";

thread_local! {
    static ENABLED: Cell<bool> = const { Cell::new(false) };
}

pub struct EmissionGuard(bool);

pub fn enable() -> EmissionGuard {
    EmissionGuard(ENABLED.replace(true))
}

impl Drop for EmissionGuard {
    fn drop(&mut self) {
        ENABLED.set(self.0);
    }
}

pub fn enabled() -> bool {
    ENABLED.get()
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Candidate {
    pub name: String,
    pub label: String,
    pub statement: String,
    pub hints: Vec<String>,
    pub obligation: bool,
    pub baseline_universal: bool,
}

impl Candidate {
    pub fn wrap(&self, lines: &mut Vec<String>, start: usize) {
        if enabled() {
            lines.insert(
                start,
                format!("{BEGIN}{}", serde_json::to_string(self).unwrap()),
            );
            lines.push(END.to_string());
        }
    }

    pub fn theorem(&self, script: &str) -> String {
        let marker = if self.obligation {
            super::LAW_OBLIGATION_MARKER_PREFIX
        } else {
            super::LAW_CLASS_MARKER_PREFIX
        };
        format!(
            "{marker}{} universal {}\nset_option maxHeartbeats 1000000 in\ntheorem {} : {} := by\n{}\n",
            self.name,
            self.label,
            self.name,
            self.statement,
            script
                .lines()
                .map(|line| format!("  {line}\n"))
                .collect::<String>()
                .trim_end()
        )
    }
}
