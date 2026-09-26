//! `Run.fail` and `Run.failure`: the reason a run of the generated loop
//! ended in failure.
//!
//! A process or an answer module calls `Run.fail(message)` from wherever it
//! finds the fault, however deep in its own helpers. The loop reads the
//! reason back with `Run.failure()` once after every turn, so this provider
//! holds the one piece of state between the two: the first reason given.
//! A later `Run.fail` changes nothing, which is what makes the first failure
//! of a turn the one the run answers, in the order the turn served its slots.
//!
//! `Run.lastTurn` says how long the loop waited and worked. In a program that
//! calls it, the generated loop calls `Run.waitStarts()` right before its one
//! wait of a turn and `Run.waitEnds()` right after it returns; each reads the
//! monotonic clock once, and this provider keeps the two readings and the
//! numbers they make, counting the waits that returned as the turn number.
//! `Run` does not expose the two marks: only the loop the compiler generates
//! calls them. A program that never calls `Run.lastTurn` gets a loop
//! that calls neither, so it reads no clock here at all.

use std::sync::{Arc, Mutex};
use std::time::Instant;

use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

pub const STANDARD_RUN_NATIVE_IDENTITY: &str = "aver.standard.Run/native";
pub const STANDARD_RUN_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

/// Standard native provider for `Run`. Each instance keeps its own reason, so
/// two programs run in one host process never see each other's failure.
#[derive(Debug, Clone, Default)]
pub struct StandardRunProvider {
    failure: Arc<Mutex<Option<String>>>,
    turns: Arc<Mutex<TurnClock>>,
}

/// The loop's clock readings around its waits, and what `Run.lastTurn`
/// answers from them.
#[derive(Debug, Clone, Copy, Default)]
struct TurnClock {
    /// When the wait now in progress started.
    started: Option<Instant>,
    /// When the last wait returned: where the work of the turn after it began.
    returned: Option<Instant>,
    /// What `Run.lastTurn` answers: which turn this is (how many waits have
    /// returned), and whole milliseconds waited and worked.
    turn: i64,
    waited_ms: i64,
    worked_ms: i64,
}

fn whole_ms(from: Instant, to: Instant) -> i64 {
    i64::try_from(to.saturating_duration_since(from).as_millis()).unwrap_or(i64::MAX)
}

impl StandardRunProvider {
    /// Record `message` unless a reason is already recorded.
    pub fn fail(&self, message: &str) {
        let mut failure = self
            .failure
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        if failure.is_none() {
            *failure = Some(message.to_string());
        }
    }

    /// The first reason recorded, if any.
    pub fn failure(&self) -> Option<String> {
        self.failure
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clone()
    }

    /// The loop is about to wait: the turn before this wait worked from the
    /// return of the last wait until now. The first wait follows no turn, so
    /// the work it closes is 0.
    pub fn wait_starts(&self) {
        self.wait_starts_at(Instant::now());
    }

    /// The loop's wait returned: what `last_turn` answers from now on.
    pub fn wait_ends(&self) {
        self.wait_ends_at(Instant::now());
    }

    fn wait_starts_at(&self, now: Instant) {
        let mut turns = self.turns.lock().unwrap_or_else(|p| p.into_inner());
        turns.started = Some(now);
    }

    fn wait_ends_at(&self, now: Instant) {
        let mut turns = self.turns.lock().unwrap_or_else(|p| p.into_inner());
        let Some(started) = turns.started.take() else {
            return;
        };
        turns.waited_ms = whole_ms(started, now);
        turns.worked_ms = turns
            .returned
            .map_or(0, |returned| whole_ms(returned, started));
        turns.returned = Some(now);
        turns.turn = turns.turn.saturating_add(1);
    }

    /// `(turn, waitedMs, workedMs)` of the current turn: `(0, 0, 0)` until a
    /// wait has returned.
    pub fn last_turn(&self) -> (i64, i64, i64) {
        let turns = self.turns.lock().unwrap_or_else(|p| p.into_inner());
        (turns.turn, turns.waited_ms, turns.worked_ms)
    }
}

impl CapabilityProvider for StandardRunProvider {
    fn identity(&self) -> &str {
        STANDARD_RUN_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_RUN_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match (context.operation.as_str(), args) {
            ("Run.fail", [ProviderValue::String(message)]) => {
                self.fail(message);
                Ok(ProviderValue::Unit)
            }
            ("Run.fail", _) => Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "Run.fail expects (String message), got {} argument(s)",
                    args.len()
                ),
            )),
            ("Run.failure", []) => Ok(match self.failure() {
                Some(message) => {
                    ProviderValue::OptionSome(Box::new(ProviderValue::String(message)))
                }
                None => ProviderValue::OptionNone,
            }),
            ("Run.failure", _) => Err(ProviderFault::new(
                "invalid_arguments",
                format!("Run.failure expects no arguments, got {}", args.len()),
            )),
            ("Run.waitStarts", []) => {
                self.wait_starts();
                Ok(ProviderValue::Unit)
            }
            ("Run.waitEnds", []) => {
                self.wait_ends();
                Ok(ProviderValue::Unit)
            }
            ("Run.lastTurn", []) => {
                let (turn, waited, worked) = self.last_turn();
                Ok(ProviderValue::Record {
                    type_name: "Run.Turn".to_string(),
                    fields: vec![
                        ("turn".to_string(), ProviderValue::Int(turn.into())),
                        ("waitedMs".to_string(), ProviderValue::Int(waited.into())),
                        ("workedMs".to_string(), ProviderValue::Int(worked.into())),
                    ],
                })
            }
            (operation @ ("Run.waitStarts" | "Run.waitEnds" | "Run.lastTurn"), _) => {
                Err(ProviderFault::new(
                    "invalid_arguments",
                    format!("{operation} expects no arguments, got {}", args.len()),
                ))
            }
            (operation, _) => Err(ProviderFault::new(
                "unknown_operation",
                format!("the standard Run provider does not implement '{operation}'"),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_first_reason_is_the_one_kept() {
        let provider = StandardRunProvider::default();
        assert_eq!(provider.failure(), None);
        provider.fail("first");
        provider.fail("second");
        assert_eq!(provider.failure().as_deref(), Some("first"));
    }

    #[test]
    fn the_turn_is_zero_until_a_wait_returns_and_the_first_worked_nothing() {
        let provider = StandardRunProvider::default();
        assert_eq!(provider.last_turn(), (0, 0, 0));
        let start = Instant::now();
        provider.wait_starts_at(start);
        assert_eq!(provider.last_turn(), (0, 0, 0));
        provider.wait_ends_at(start + std::time::Duration::from_millis(250));
        assert_eq!(provider.last_turn(), (1, 250, 0));
        provider.wait_starts_at(start + std::time::Duration::from_millis(262));
        provider.wait_ends_at(start + std::time::Duration::from_millis(262));
        assert_eq!(provider.last_turn(), (2, 0, 12));
    }

    #[test]
    fn two_providers_keep_their_own_reason() {
        let one = StandardRunProvider::default();
        let other = StandardRunProvider::default();
        one.fail("mine");
        assert_eq!(other.failure(), None);
    }
}
