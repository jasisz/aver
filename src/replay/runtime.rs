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
    /// Current independent product group id for recording. None = sequential.
    current_group: Option<u32>,
    /// Branch path stack for nested independent products.
    /// E.g. [0, 1] means "branch 0 of outer product, branch 1 of inner product".
    branch_stack: Vec<u32>,
    /// Per-branch effect emission counter (reset when branch changes).
    branch_effect_count: u32,
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
    }

    pub fn start_recording(&mut self) {
        self.mode = EffectReplayMode::Record;
        self.recorded_effects.clear();
        self.replay_effects.clear();
        self.replay_pos = 0;
        self.validate_replay_args = false;
    }

    pub fn start_replay(&mut self, effects: Vec<EffectRecord>, validate_args: bool) {
        self.mode = EffectReplayMode::Replay;
        self.replay_effects = effects;
        self.replay_pos = 0;
        self.validate_replay_args = validate_args;
        self.recorded_effects.clear();
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
        self.current_group = Some(id);
        self.branch_stack.push(0); // start at branch 0
        id
    }

    /// Exit the current independent product group.
    pub fn exit_group(&mut self) {
        self.current_group = None;
        self.branch_stack.pop();
    }

    /// Set the current branch index within the current (innermost) product.
    pub fn set_branch(&mut self, index: u32) {
        if let Some(last) = self.branch_stack.last_mut() {
            *last = index;
        }
        self.branch_effect_count = 0;
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
            group_id: self.current_group,
            branch_path: if self.branch_stack.is_empty() {
                None
            } else {
                Some(
                    self.branch_stack
                        .iter()
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join("."),
                )
            },
            branch_occurrence: if self.branch_stack.is_empty() {
                None
            } else {
                Some(self.branch_effect_count)
            },
        });
        if !self.branch_stack.is_empty() {
            self.branch_effect_count += 1;
        }
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
            Some(
                self.branch_stack
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join("."),
            )
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

            // Check branch_path + branch_occurrence: if both sides have them, must match.
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
                let current_occ = Some(self.branch_effect_count);
                match (current_occ, record.branch_occurrence) {
                    (Some(got), Some(rec)) if got == rec => {
                        self.branch_effect_count += 1;
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
        self.group_consumed.push(idx);
        let group_size = group_end - group_start;
        if self.group_consumed.len() >= group_size {
            self.replay_pos = group_end;
            self.group_consumed.clear();
        }
        Ok(outcome)
    }
}
