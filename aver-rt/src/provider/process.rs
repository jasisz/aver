use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, LazyLock, OnceLock};

use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

/// Process-global stop observation shared by every native Aver provider.
///
/// The signal callback has exactly one transition: `false -> true`. There is
/// deliberately no reset API, so repeated observations are monotonic even
/// when independent Aver branches poll concurrently.
static STOP_REQUESTED: LazyLock<Arc<AtomicBool>> =
    LazyLock::new(|| Arc::new(AtomicBool::new(false)));
static SIGNAL_HANDLER: OnceLock<Result<(), String>> = OnceLock::new();

pub struct StandardProcessProvider;

pub const STANDARD_PROCESS_NATIVE_IDENTITY: &str = "aver.standard.Process/native";
pub const STANDARD_PROCESS_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

#[cfg(any(test, all(not(unix), not(target_family = "wasm"))))]
fn mark_stop_requested(flag: &AtomicBool) {
    flag.store(true, Ordering::Release);
}

fn stop_requested(flag: &AtomicBool) -> bool {
    flag.load(Ordering::Acquire)
}

#[cfg(unix)]
fn install_signal_handler() -> Result<(), String> {
    use signal_hook::consts::{SIGINT, SIGTERM};

    signal_hook::flag::register(SIGINT, Arc::clone(&STOP_REQUESTED))
        .map_err(|error| error.to_string())?;
    signal_hook::flag::register(SIGTERM, Arc::clone(&STOP_REQUESTED))
        .map_err(|error| error.to_string())?;
    Ok(())
}

#[cfg(all(not(unix), not(target_family = "wasm")))]
fn install_signal_handler() -> Result<(), String> {
    ctrlc::set_handler(|| mark_stop_requested(STOP_REQUESTED.as_ref()))
        .map_err(|error| error.to_string())
}

#[cfg(target_family = "wasm")]
fn install_signal_handler() -> Result<(), String> {
    Err("native process signal handling is unavailable on wasm".to_string())
}

/// Observe whether SIGINT or SIGTERM requested cooperative shutdown.
///
/// Installation is attempted once per process. A host conflict while
/// installing the handler is a provider fault, not an Aver-level false value:
/// silently returning false would violate the capability contract.
pub fn standard_process_stop_requested() -> Result<bool, ProviderFault> {
    match SIGNAL_HANDLER.get_or_init(install_signal_handler) {
        Ok(()) => Ok(stop_requested(STOP_REQUESTED.as_ref())),
        Err(message) => Err(ProviderFault::new(
            "signal_handler_install_failed",
            format!("Process.stopRequested: cannot install SIGINT/SIGTERM handler: {message}"),
        )),
    }
}

impl CapabilityProvider for StandardProcessProvider {
    fn identity(&self) -> &str {
        STANDARD_PROCESS_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_PROCESS_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match context.operation.as_str() {
            "Process.stopRequested" if args.is_empty() => {
                standard_process_stop_requested().map(ProviderValue::Bool)
            }
            "Process.stopRequested" => Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "Process.stopRequested expects no arguments, got {}",
                    args.len()
                ),
            )),
            operation => Err(ProviderFault::new(
                "unknown_operation",
                format!("standard Process provider cannot invoke '{operation}'"),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stop_flag_can_only_transition_to_true() {
        let flag = AtomicBool::new(false);
        assert!(!stop_requested(&flag));
        mark_stop_requested(&flag);
        assert!(stop_requested(&flag));
        mark_stop_requested(&flag);
        assert!(stop_requested(&flag));
    }
}
