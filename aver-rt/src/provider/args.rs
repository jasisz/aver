use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

#[derive(Debug, Clone, Default)]
pub struct StandardArgsProvider {
    args: Option<Vec<String>>,
}

pub const STANDARD_ARGS_NATIVE_IDENTITY: &str = "aver.standard.Args/native";
pub const STANDARD_ARGS_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

impl StandardArgsProvider {
    pub fn new(args: Vec<String>) -> Self {
        Self { args: Some(args) }
    }
}

impl CapabilityProvider for StandardArgsProvider {
    fn identity(&self) -> &str {
        STANDARD_ARGS_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_ARGS_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        if context.operation != "Args.get" || !args.is_empty() {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!("{} expects no arguments", context.operation),
            ));
        }
        let values = self
            .args
            .clone()
            .unwrap_or_else(|| crate::cli_args().iter().map(ToString::to_string).collect());
        Ok(ProviderValue::List(
            values.into_iter().map(ProviderValue::String).collect(),
        ))
    }
}
