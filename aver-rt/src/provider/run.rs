//! `Run.fail` and `Run.failure`: the reason a run of the generated loop
//! ended in failure.
//!
//! A process or an answer module calls `Run.fail(message)` from wherever it
//! finds the fault, however deep in its own helpers. The loop reads the
//! reason back with `Run.failure()` once after every turn, so this provider
//! holds the one piece of state between the two: the first reason given.
//! A later `Run.fail` changes nothing, which is what makes the first failure
//! of a turn the one the run answers, in the order the turn served its slots.

use std::sync::{Arc, Mutex};

use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

pub const STANDARD_RUN_NATIVE_IDENTITY: &str = "aver.standard.Run/native";
pub const STANDARD_RUN_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

/// Standard native provider for `Run`. Each instance keeps its own reason, so
/// two programs run in one host process never see each other's failure.
#[derive(Debug, Clone, Default)]
pub struct StandardRunProvider {
    failure: Arc<Mutex<Option<String>>>,
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
    fn two_providers_keep_their_own_reason() {
        let one = StandardRunProvider::default();
        let other = StandardRunProvider::default();
        one.fail("mine");
        assert_eq!(other.failure(), None);
    }
}
