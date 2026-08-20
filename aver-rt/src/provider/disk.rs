use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderValue};

/// Standard native Disk provider shared by the bytecode VM and generated
/// Rust artifacts. All thirteen operations of the canonical
/// `stdlib/capabilities/disk.av` contract lower onto shared `aver_rt`
/// filesystem helpers; wasm targets keep host/WASI adapters of the same
/// contract.
pub struct StandardDiskProvider;

pub const STANDARD_DISK_NATIVE_IDENTITY: &str = "aver.standard.Disk/native";
pub const STANDARD_DISK_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

/// Map `Result<T, String>` from a filesystem helper onto the contract's
/// `Result<T, String>` shape. A failed IO operation is a VALUE in Aver,
/// not a provider fault: faults stay reserved for host boundary
/// violations (wrong arguments, panics).
fn disk_result<T>(
    result: Result<T, String>,
    ok: impl FnOnce(T) -> ProviderValue,
) -> Result<ProviderValue, ProviderFault> {
    match result {
        Ok(value) => Ok(ProviderValue::ResultOk(Box::new(ok(value)))),
        Err(message) => Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
            message,
        )))),
    }
}

impl CapabilityProvider for StandardDiskProvider {
    fn identity(&self) -> &str {
        STANDARD_DISK_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_DISK_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        fn path<'a>(operation: &str, args: &'a [ProviderValue]) -> Result<&'a str, ProviderFault> {
            match args {
                [ProviderValue::String(path)] => Ok(path),
                _ => Err(ProviderFault::new(
                    "invalid_arguments",
                    format!(
                        "{operation} expects one String argument (path), got {}",
                        args.len()
                    ),
                )),
            }
        }
        fn path_and_content<'a>(
            operation: &str,
            args: &'a [ProviderValue],
        ) -> Result<(&'a str, &'a str), ProviderFault> {
            match args {
                [ProviderValue::String(path), ProviderValue::String(content)] => {
                    Ok((path, content))
                }
                _ => Err(ProviderFault::new(
                    "invalid_arguments",
                    format!(
                        "{operation} expects two String arguments (path, content), got {}",
                        args.len()
                    ),
                )),
            }
        }
        fn path_and_bytes<'a>(
            operation: &str,
            args: &'a [ProviderValue],
        ) -> Result<(&'a str, &'a [u8]), ProviderFault> {
            match args {
                [ProviderValue::String(path), ProviderValue::Bytes(content)] => Ok((path, content)),
                _ => Err(ProviderFault::new(
                    "invalid_arguments",
                    format!(
                        "{operation} expects (String path, Bytes content), got {} argument(s)",
                        args.len()
                    ),
                )),
            }
        }
        fn path_offset_length<'a>(
            operation: &str,
            args: &'a [ProviderValue],
        ) -> Result<(&'a str, &'a crate::AverInt, &'a crate::AverInt), ProviderFault> {
            match args {
                [
                    ProviderValue::String(path),
                    ProviderValue::Int(offset),
                    ProviderValue::Int(length),
                ] => Ok((path, offset, length)),
                _ => Err(ProviderFault::new(
                    "invalid_arguments",
                    format!(
                        "{operation} expects (String path, Int offset, Int length), got {} argument(s)",
                        args.len()
                    ),
                )),
            }
        }
        let operation = context.operation.as_str();
        match operation {
            "Disk.readText" => disk_result(
                crate::read_text(path(operation, args)?),
                ProviderValue::String,
            ),
            "Disk.writeText" => {
                let (path, content) = path_and_content(operation, args)?;
                disk_result(crate::write_text(path, content), |_| ProviderValue::Unit)
            }
            "Disk.appendText" => {
                let (path, content) = path_and_content(operation, args)?;
                disk_result(crate::append_text(path, content), |_| ProviderValue::Unit)
            }
            "Disk.readBytes" => disk_result(
                crate::read_bytes(path(operation, args)?),
                ProviderValue::Bytes,
            ),
            "Disk.readBytesAt" => {
                let (path, offset, length) = path_offset_length(operation, args)?;
                disk_result(
                    crate::read_bytes_at(path, offset, length),
                    ProviderValue::Bytes,
                )
            }
            "Disk.writeBytes" => {
                let (path, content) = path_and_bytes(operation, args)?;
                disk_result(crate::write_bytes(path, content), |_| ProviderValue::Unit)
            }
            "Disk.appendBytes" => {
                let (path, content) = path_and_bytes(operation, args)?;
                disk_result(crate::append_bytes(path, content), |_| ProviderValue::Unit)
            }
            "Disk.size" => {
                disk_result(crate::file_size(path(operation, args)?), ProviderValue::Int)
            }
            "Disk.exists" => Ok(ProviderValue::Bool(crate::path_exists(path(
                operation, args,
            )?))),
            "Disk.delete" => disk_result(crate::delete_file(path(operation, args)?), |_| {
                ProviderValue::Unit
            }),
            "Disk.deleteDir" => disk_result(crate::delete_dir(path(operation, args)?), |_| {
                ProviderValue::Unit
            }),
            "Disk.listDir" => disk_result(crate::list_dir(path(operation, args)?), |entries| {
                ProviderValue::List(entries.into_iter().map(ProviderValue::String).collect())
            }),
            "Disk.makeDir" => disk_result(crate::make_dir(path(operation, args)?), |_| {
                ProviderValue::Unit
            }),
            _ => Err(ProviderFault::new(
                "unknown_operation",
                format!("standard Disk provider cannot invoke '{operation}'"),
            )),
        }
    }
}
