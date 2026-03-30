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

    pub fn record_effect(
        &mut self,
        effect_type: &str,
        args: Vec<JsonValue>,
        outcome: RecordedOutcome,
        caller_fn: &str,
    ) {
        let seq = self.recorded_effects.len() as u32 + 1;
        self.recorded_effects.push(EffectRecord {
            seq,
            effect_type: effect_type.to_string(),
            args,
            outcome,
            caller_fn: caller_fn.to_string(),
        });
    }

    pub fn replay_effect(
        &mut self,
        effect_type: &str,
        got_args: Option<Vec<JsonValue>>,
    ) -> Result<RecordedOutcome, ReplayFailure> {
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
}
