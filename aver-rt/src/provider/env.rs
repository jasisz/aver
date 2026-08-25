use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

pub struct StandardEnvProvider;

pub const STANDARD_ENV_NATIVE_IDENTITY: &str = "aver.standard.Env/native";
pub const STANDARD_ENV_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

impl CapabilityProvider for StandardEnvProvider {
    fn identity(&self) -> &str {
        STANDARD_ENV_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_ENV_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match (context.operation.as_str(), args) {
            ("Env.get", [ProviderValue::String(key)]) => Ok(crate::env_get(key)
                .map_or(ProviderValue::OptionNone, |value| {
                    ProviderValue::OptionSome(Box::new(ProviderValue::String(value)))
                })),
            ("Env.set", [ProviderValue::String(key), ProviderValue::String(value)]) => {
                Ok(match crate::env_set(key, value) {
                    Ok(()) => ProviderValue::ResultOk(Box::new(ProviderValue::Unit)),
                    Err(message) => {
                        ProviderValue::ResultErr(Box::new(ProviderValue::String(message)))
                    }
                })
            }
            (operation, _) => Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} received an invalid argument shape"),
            )),
        }
    }
}
