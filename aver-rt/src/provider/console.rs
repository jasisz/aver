use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

pub struct StandardConsoleProvider;

pub const STANDARD_CONSOLE_NATIVE_IDENTITY: &str = "aver.standard.Console/native";
pub const STANDARD_CONSOLE_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

impl CapabilityProvider for StandardConsoleProvider {
    fn identity(&self) -> &str {
        STANDARD_CONSOLE_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_CONSOLE_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        match (context.operation.as_str(), args) {
            ("Console.print", [ProviderValue::String(message)]) => {
                crate::console_print(message);
                Ok(ProviderValue::Unit)
            }
            ("Console.error", [ProviderValue::String(message)]) => {
                crate::console_error(message);
                Ok(ProviderValue::Unit)
            }
            ("Console.warn", [ProviderValue::String(message)]) => {
                crate::console_warn(message);
                Ok(ProviderValue::Unit)
            }
            ("Console.readLine", []) => Ok(match crate::read_line() {
                Ok(line) => ProviderValue::ResultOk(Box::new(ProviderValue::String(line))),
                Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
            }),
            (operation, _) => Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} received an invalid argument shape"),
            )),
        }
    }
}
