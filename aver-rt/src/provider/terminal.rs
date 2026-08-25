use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

pub struct StandardTerminalProvider;

pub const STANDARD_TERMINAL_NATIVE_IDENTITY: &str = "aver.standard.Terminal/native";
pub const STANDARD_TERMINAL_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

#[cfg(not(feature = "terminal"))]
fn unavailable(operation: &str) -> ProviderFault {
    ProviderFault::new(
        "binding_unavailable",
        format!("{operation}: terminal effects are not available in this build"),
    )
}

#[cfg(feature = "terminal")]
fn unit_result(result: Result<(), String>) -> ProviderValue {
    match result {
        Ok(()) => ProviderValue::ResultOk(Box::new(ProviderValue::Unit)),
        Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
    }
}

impl CapabilityProvider for StandardTerminalProvider {
    fn identity(&self) -> &str {
        STANDARD_TERMINAL_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_TERMINAL_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        #[cfg(not(feature = "terminal"))]
        {
            let _ = args;
            return Err(unavailable(&context.operation));
        }
        #[cfg(feature = "terminal")]
        match (context.operation.as_str(), args) {
            ("Terminal.enableRawMode", []) => Ok(unit_result(crate::terminal_enable_raw_mode())),
            ("Terminal.disableRawMode", []) => Ok(unit_result(crate::terminal_disable_raw_mode())),
            ("Terminal.clear", []) => Ok(unit_result(crate::terminal_clear())),
            ("Terminal.moveTo", [ProviderValue::Int(x), ProviderValue::Int(y)]) => {
                let (Some(x), Some(y)) = (x.to_i64(), y.to_i64()) else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        "Terminal.moveTo: coordinates must fit a 64-bit integer".to_string(),
                    ))));
                };
                Ok(unit_result(crate::terminal_move_to(x, y)))
            }
            ("Terminal.print", [ProviderValue::String(text)]) => {
                Ok(unit_result(crate::terminal_print(text)))
            }
            ("Terminal.setColor", [ProviderValue::String(color)]) => {
                Ok(unit_result(crate::terminal_set_color(color)))
            }
            ("Terminal.resetColor", []) => Ok(unit_result(crate::terminal_reset_color())),
            ("Terminal.readKey", []) => Ok(match crate::terminal_read_key() {
                Ok(Some(key)) => ProviderValue::ResultOk(Box::new(ProviderValue::OptionSome(
                    Box::new(ProviderValue::String(key)),
                ))),
                Ok(None) => ProviderValue::ResultOk(Box::new(ProviderValue::OptionNone)),
                Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
            }),
            ("Terminal.size", []) => Ok(match crate::terminal_size() {
                Ok((width, height)) => ProviderValue::ResultOk(Box::new(ProviderValue::Record {
                    type_name: "Terminal.Size".to_string(),
                    fields: vec![
                        ("width".to_string(), ProviderValue::Int(width.into())),
                        ("height".to_string(), ProviderValue::Int(height.into())),
                    ],
                })),
                Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
            }),
            ("Terminal.hideCursor", []) => Ok(unit_result(crate::terminal_hide_cursor())),
            ("Terminal.showCursor", []) => Ok(unit_result(crate::terminal_show_cursor())),
            ("Terminal.flush", []) => Ok(unit_result(crate::terminal_flush())),
            (operation, _) => Err(ProviderFault::new(
                "invalid_arguments",
                format!("{operation} received an invalid argument shape"),
            )),
        }
    }
}
