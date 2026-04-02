use super::JsonValue;
use super::json_to_string;
use super::session::{EffectRecord, RecordedOutcome};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EffectReplayMode {
    #[default]
    Normal,
    Record,
    Replay,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplayFailure {
    Exhausted {
        effect_type: String,
        position: usize,
    },
    Mismatch {
        seq: u32,
        expected: String,
        got: String,
    },
    ArgsMismatch {
        seq: u32,
        effect_type: String,
        expected: String,
        got: String,
    },
    Unconsumed {
        remaining: usize,
    },
}

#[derive(Debug, Clone, Default)]
pub struct EffectReplayState {
    mode: EffectReplayMode,
    recorded_effects: Vec<EffectRecord>,
    replay_effects: Vec<EffectRecord>,
    replay_pos: usize,
    validate_replay_args: bool,
    args_diff_count: usize,
    /// Stack of independent product group ids for nested products.
    group_stack: Vec<u32>,
    /// Branch path stack for nested independent products.
    /// E.g. [0, 1] means "branch 0 of outer product, branch 1 of inner product".
    branch_stack: Vec<u32>,
    /// Per-product stack of per-branch effect emission counters.
    effect_count_stack: Vec<u32>,
    /// Next group id to assign.
    next_group_id: u32,
    /// Indices within replay_effects consumed from current group (for unordered match).
    group_consumed: Vec<usize>,
}

impl EffectReplayState {
    pub fn mode(&self) -> EffectReplayMode {
        self.mode
    }

    pub fn set_normal(&mut self) {
        self.mode = EffectReplayMode::Normal;
        self.recorded_effects.clear();
        self.replay_effects.clear();
        self.replay_pos = 0;
        self.validate_replay_args = false;
        self.args_diff_count = 0;
        self.reset_group_state();
    }

    pub fn start_recording(&mut self) {
        self.mode = EffectReplayMode::Record;
        self.recorded_effects.clear();
        self.replay_effects.clear();
        self.replay_pos = 0;
        self.validate_replay_args = false;
        self.args_diff_count = 0;
        self.reset_group_state();
    }

    pub fn start_replay(&mut self, effects: Vec<EffectRecord>, validate_args: bool) {
        self.mode = EffectReplayMode::Replay;
        self.replay_effects = effects;
        self.replay_pos = 0;
        self.validate_replay_args = validate_args;
        self.recorded_effects.clear();
        self.args_diff_count = 0;
        self.reset_group_state();
    }

    pub fn take_recorded_effects(&mut self) -> Vec<EffectRecord> {
        std::mem::take(&mut self.recorded_effects)
    }

    pub fn recorded_effects(&self) -> &[EffectRecord] {
        &self.recorded_effects
    }

    pub fn replay_progress(&self) -> (usize, usize) {
        (self.replay_pos, self.replay_effects.len())
    }

    pub fn args_diff_count(&self) -> usize {
        self.args_diff_count
    }

    pub fn ensure_replay_consumed(&self) -> Result<(), ReplayFailure> {
        if self.mode == EffectReplayMode::Replay && self.replay_pos < self.replay_effects.len() {
            return Err(ReplayFailure::Unconsumed {
                remaining: self.replay_effects.len() - self.replay_pos,
            });
        }
        Ok(())
    }

    /// Enter an independent product group for recording. Returns the group id.
    pub fn enter_group(&mut self) -> u32 {
        self.next_group_id += 1;
        let id = self.next_group_id;
        self.group_stack.push(id);
        self.branch_stack.push(0); // start at branch 0
        self.effect_count_stack.push(0);
        id
    }

    /// Exit the current independent product group.
    pub fn exit_group(&mut self) {
        self.group_stack.pop();
        self.branch_stack.pop();
        self.effect_count_stack.pop();
    }

    /// Set the current branch index within the current (innermost) product.
    pub fn set_branch(&mut self, index: u32) {
        if let Some(last) = self.branch_stack.last_mut() {
            *last = index;
        }
        if let Some(last) = self.effect_count_stack.last_mut() {
            *last = 0;
        }
    }

    pub fn record_effect(
        &mut self,
        effect_type: &str,
        args: Vec<JsonValue>,
        outcome: RecordedOutcome,
        caller_fn: &str,
        source_line: usize,
    ) {
        let seq = self.recorded_effects.len() as u32 + 1;
        self.recorded_effects.push(EffectRecord {
            seq,
            effect_type: effect_type.to_string(),
            args,
            outcome,
            caller_fn: caller_fn.to_string(),
            source_line,
            group_id: self.group_stack.last().copied(),
            branch_path: if self.branch_stack.is_empty() {
                None
            } else {
                Some(self.current_branch_path())
            },
            effect_occurrence: if self.branch_stack.is_empty() {
                None
            } else {
                self.current_effect_occurrence()
            },
        });
        self.bump_effect_occurrence();
    }

    pub fn replay_effect(
        &mut self,
        effect_type: &str,
        got_args: Option<Vec<JsonValue>>,
    ) -> Result<RecordedOutcome, ReplayFailure> {
        // Check if current position is inside a group — match by type+args, not position
        if self.replay_pos < self.replay_effects.len()
            && let Some(gid) = self.replay_effects[self.replay_pos].group_id
        {
            return self.replay_effect_in_group(gid, effect_type, got_args);
        }

        // Sequential matching (original behavior)
        if self.replay_pos >= self.replay_effects.len() {
            return Err(ReplayFailure::Exhausted {
                effect_type: effect_type.to_string(),
                position: self.replay_pos + 1,
            });
        }

        let record = self.replay_effects[self.replay_pos].clone();
        if record.effect_type != effect_type {
            return Err(ReplayFailure::Mismatch {
                seq: record.seq,
                expected: record.effect_type,
                got: effect_type.to_string(),
            });
        }

        if let Some(got_args) = got_args
            && got_args != record.args
        {
            if self.validate_replay_args {
                return Err(ReplayFailure::ArgsMismatch {
                    seq: record.seq,
                    effect_type: effect_type.to_string(),
                    expected: json_to_string(&JsonValue::Array(record.args.clone())),
                    got: json_to_string(&JsonValue::Array(got_args)),
                });
            }
            self.args_diff_count += 1;
        }

        self.replay_pos += 1;
        Ok(record.outcome)
    }

    /// Match an effect within a replay group by (branch_index, type, args), not position.
    /// Falls back to (type, args) matching for recordings without branch_index.
    fn replay_effect_in_group(
        &mut self,
        group_id: u32,
        effect_type: &str,
        got_args: Option<Vec<JsonValue>>,
    ) -> Result<RecordedOutcome, ReplayFailure> {
        // Find all effects in this group that haven't been consumed yet
        let group_start = self.replay_pos;
        let group_end = self.replay_effects[group_start..]
            .iter()
            .position(|e| e.group_id != Some(group_id))
            .map(|offset| group_start + offset)
            .unwrap_or(self.replay_effects.len());

        // Search for a matching effect in the group.
        // Prefer exact branch_index match; fall back to type+args only.
        let current_bp = if self.branch_stack.is_empty() {
            None
        } else {
            Some(self.current_branch_path())
        };

        let mut fallback_idx: Option<usize> = None;
        for idx in group_start..group_end {
            if self.group_consumed.contains(&idx) {
                continue;
            }
            let record = &self.replay_effects[idx];
            if record.effect_type != effect_type {
                continue;
            }

            // Check args
            let args_ok = match (&got_args, self.validate_replay_args) {
                (Some(got), true) if *got != record.args => false,
                (Some(got), false) if *got != record.args => {
                    self.args_diff_count += 1;
                    true
                }
                _ => true,
            };
            if !args_ok {
                continue;
            }

            // Check branch_path + effect_occurrence: if both sides have them, must match.
            // If recording lacks them (old format), accept as fallback.
            let bp_match = match (&current_bp, &record.branch_path) {
                (Some(got), Some(rec)) => {
                    if got != rec {
                        continue; // different branch, skip
                    }
                    true
                }
                _ => false, // one or both lack branch_path
            };
            if bp_match {
                // Branch path matches — also check occurrence if available
                let current_occ = self.current_effect_occurrence();
                match (current_occ, record.effect_occurrence) {
                    (Some(got), Some(rec)) if got == rec => {
                        return self.consume_group_match(idx, group_start, group_end);
                    }
                    (Some(_), Some(_)) => continue, // same branch, different occurrence
                    _ => {
                        // Fallback: no occurrence info
                        if fallback_idx.is_none() {
                            fallback_idx = Some(idx);
                        }
                    }
                }
            } else if fallback_idx.is_none() {
                fallback_idx = Some(idx);
            }
        }

        // Use fallback if no exact branch match found
        if let Some(idx) = fallback_idx {
            return self.consume_group_match(idx, group_start, group_end);
        }

        // No match found in group
        Err(ReplayFailure::Mismatch {
            seq: self.replay_effects[group_start].seq,
            expected: format!("one of group {} effects", group_id),
            got: effect_type.to_string(),
        })
    }

    fn consume_group_match(
        &mut self,
        idx: usize,
        group_start: usize,
        group_end: usize,
    ) -> Result<RecordedOutcome, ReplayFailure> {
        let outcome = self.replay_effects[idx].outcome.clone();
        self.bump_effect_occurrence();
        self.group_consumed.push(idx);
        let group_size = group_end - group_start;
        if self.group_consumed.len() >= group_size {
            self.replay_pos = group_end;
            self.group_consumed.clear();
        }
        Ok(outcome)
    }

    fn reset_group_state(&mut self) {
        self.group_stack.clear();
        self.branch_stack.clear();
        self.effect_count_stack.clear();
        self.next_group_id = 0;
        self.group_consumed.clear();
    }

    fn current_branch_path(&self) -> String {
        self.branch_stack
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(".")
    }

    fn current_effect_occurrence(&self) -> Option<u32> {
        self.effect_count_stack.last().copied()
    }

    fn bump_effect_occurrence(&mut self) {
        if let Some(last) = self.effect_count_stack.last_mut() {
            *last += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn recorded_value(text: &str) -> RecordedOutcome {
        RecordedOutcome::Value(JsonValue::String(text.to_string()))
    }

    #[test]
    fn nested_groups_preserve_outer_effect_occurrence() {
        let mut state = EffectReplayState::default();

        state.start_recording();
        state.enter_group();
        state.set_branch(0);
        state.record_effect(
            "Console.print",
            vec![],
            RecordedOutcome::Value(JsonValue::Null),
            "",
            0,
        );

        state.enter_group();
        state.set_branch(1);
        state.record_effect(
            "Console.print",
            vec![],
            RecordedOutcome::Value(JsonValue::Null),
            "",
            0,
        );
        state.exit_group();

        state.record_effect(
            "Console.print",
            vec![],
            RecordedOutcome::Value(JsonValue::Null),
            "",
            0,
        );

        let effects = state.take_recorded_effects();
        assert_eq!(effects.len(), 3);
        assert_eq!(effects[0].branch_path.as_deref(), Some("0"));
        assert_eq!(effects[0].effect_occurrence, Some(0));
        assert_eq!(effects[1].branch_path.as_deref(), Some("0.1"));
        assert_eq!(effects[1].effect_occurrence, Some(0));
        assert_eq!(effects[2].branch_path.as_deref(), Some("0"));
        assert_eq!(effects[2].effect_occurrence, Some(1));
    }

    #[test]
    fn start_replay_clears_group_state() {
        let mut state = EffectReplayState::default();
        state.start_recording();
        state.enter_group();
        state.set_branch(3);
        state.record_effect(
            "Console.print",
            vec![],
            RecordedOutcome::Value(JsonValue::Null),
            "",
            0,
        );

        state.start_replay(Vec::new(), true);

        assert!(state.group_stack.is_empty());
        assert!(state.branch_stack.is_empty());
        assert!(state.effect_count_stack.is_empty());
        assert!(state.group_consumed.is_empty());
        assert_eq!(state.next_group_id, 0);
        assert_eq!(state.args_diff_count, 0);
    }

    #[test]
    fn replay_group_matching_uses_effect_occurrence() {
        let mut state = EffectReplayState::default();
        state.start_replay(
            vec![
                EffectRecord {
                    seq: 1,
                    effect_type: "Console.print".to_string(),
                    args: vec![JsonValue::String("same".to_string())],
                    outcome: recorded_value("first"),
                    caller_fn: String::new(),
                    source_line: 0,
                    group_id: Some(1),
                    branch_path: Some("0".to_string()),
                    effect_occurrence: Some(0),
                },
                EffectRecord {
                    seq: 2,
                    effect_type: "Console.print".to_string(),
                    args: vec![JsonValue::String("same".to_string())],
                    outcome: recorded_value("second"),
                    caller_fn: String::new(),
                    source_line: 0,
                    group_id: Some(1),
                    branch_path: Some("0".to_string()),
                    effect_occurrence: Some(1),
                },
            ],
            true,
        );

        state.enter_group();
        state.set_branch(0);

        let first = state
            .replay_effect(
                "Console.print",
                Some(vec![JsonValue::String("same".to_string())]),
            )
            .expect("first replay should match");
        let second = state
            .replay_effect(
                "Console.print",
                Some(vec![JsonValue::String("same".to_string())]),
            )
            .expect("second replay should match");

        assert_eq!(first, recorded_value("first"));
        assert_eq!(second, recorded_value("second"));
    }
}
