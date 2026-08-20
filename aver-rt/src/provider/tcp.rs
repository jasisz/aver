use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderResource, ProviderValue};

/// Standard native Tcp provider shared by the bytecode VM and generated Rust.
/// The capability boundary carries a `Tcp.Connection` as an opaque resource;
/// only this adapter can recover the host-side connection token.
pub struct StandardTcpProvider;

pub const STANDARD_TCP_NATIVE_IDENTITY: &str = "aver.standard.Tcp/native";
pub const STANDARD_TCP_FINGERPRINT: &str = concat!("aver-rt/", env!("CARGO_PKG_VERSION"));

fn tcp_result<T>(
    result: Result<T, String>,
    ok: impl FnOnce(T) -> ProviderValue,
) -> Result<ProviderValue, ProviderFault> {
    Ok(match result {
        Ok(value) => ProviderValue::ResultOk(Box::new(ok(value))),
        Err(message) => ProviderValue::ResultErr(Box::new(ProviderValue::String(message))),
    })
}

fn invalid_arguments(operation: &str, expected: &str, actual: usize) -> ProviderFault {
    ProviderFault::new(
        "invalid_arguments",
        format!("{operation} expects {expected}, got {actual} argument(s)"),
    )
}

fn host_port<'a>(
    operation: &str,
    args: &'a [ProviderValue],
) -> Result<(&'a str, i64), ProviderFault> {
    let [ProviderValue::String(host), ProviderValue::Int(port)] = args else {
        return Err(invalid_arguments(
            operation,
            "(String host, Int port)",
            args.len(),
        ));
    };
    let port = port.to_i64().ok_or_else(|| {
        ProviderFault::new(
            "integer_out_of_range",
            format!("{operation}: port must fit a 64-bit integer"),
        )
    })?;
    Ok((host, port))
}

fn connection<'a>(
    operation: &str,
    value: &'a ProviderValue,
) -> Result<&'a crate::TcpConnection, ProviderFault> {
    let ProviderValue::Resource(resource) = value else {
        return Err(ProviderFault::new(
            "invalid_arguments",
            format!("{operation} expects a Tcp.Connection resource"),
        ));
    };
    resource
        .downcast_ref::<crate::TcpConnection>()
        .ok_or_else(|| {
            ProviderFault::new(
                "invalid_resource",
                format!("{operation} received a resource from an incompatible provider"),
            )
        })
}

impl CapabilityProvider for StandardTcpProvider {
    fn identity(&self) -> &str {
        STANDARD_TCP_NATIVE_IDENTITY
    }

    fn fingerprint(&self) -> &str {
        STANDARD_TCP_FINGERPRINT
    }

    fn invoke(
        &self,
        context: &ProviderContext,
        args: &[ProviderValue],
    ) -> Result<ProviderValue, ProviderFault> {
        let operation = context.operation.as_str();
        match operation {
            "Tcp.send" => {
                let [
                    ProviderValue::String(host),
                    ProviderValue::Int(port),
                    ProviderValue::String(data),
                ] = args
                else {
                    return Err(invalid_arguments(
                        operation,
                        "(String host, Int port, String data)",
                        args.len(),
                    ));
                };
                let port = port.to_i64().ok_or_else(|| {
                    ProviderFault::new(
                        "integer_out_of_range",
                        "Tcp.send: port must fit a 64-bit integer",
                    )
                })?;
                tcp_result(crate::tcp::send(host, port, data), ProviderValue::String)
            }
            "Tcp.sendBytes" => {
                let [
                    ProviderValue::String(host),
                    ProviderValue::Int(port),
                    ProviderValue::Bytes(payload),
                ] = args
                else {
                    return Err(invalid_arguments(
                        operation,
                        "(String host, Int port, Bytes payload)",
                        args.len(),
                    ));
                };
                let port = port.to_i64().ok_or_else(|| {
                    ProviderFault::new(
                        "integer_out_of_range",
                        "Tcp.sendBytes: port must fit a 64-bit integer",
                    )
                })?;
                tcp_result(
                    crate::tcp::send_bytes(host, port, payload),
                    ProviderValue::Bytes,
                )
            }
            "Tcp.ping" => {
                let (host, port) = host_port(operation, args)?;
                tcp_result(crate::tcp::ping(host, port), |_| ProviderValue::Unit)
            }
            "Tcp.connect" => {
                let (host, port) = host_port(operation, args)?;
                tcp_result(crate::tcp::connect(host, port), |connection| {
                    ProviderValue::Resource(ProviderResource::new(connection))
                })
            }
            "Tcp.writeLine" => {
                let [connection_value, ProviderValue::String(line)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection, String line)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::write_line(connection(operation, connection_value)?, line),
                    |_| ProviderValue::Unit,
                )
            }
            "Tcp.writeBytes" => {
                let [connection_value, ProviderValue::Bytes(payload)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection, Bytes payload)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::write_bytes(connection(operation, connection_value)?, payload),
                    |_| ProviderValue::Unit,
                )
            }
            "Tcp.readLine" => {
                let [connection_value] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::read_line(connection(operation, connection_value)?),
                    ProviderValue::String,
                )
            }
            "Tcp.readBytes" => {
                let [connection_value, ProviderValue::Int(count)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection, Int count)",
                        args.len(),
                    ));
                };
                let Some(count) = count.to_i64() else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!("Tcp.readBytes: count {count} exceeds the read limit"),
                    ))));
                };
                tcp_result(
                    crate::tcp::read_bytes(connection(operation, connection_value)?, count),
                    ProviderValue::Bytes,
                )
            }
            "Tcp.close" => {
                let [connection_value] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::close(connection(operation, connection_value)?),
                    |_| ProviderValue::Unit,
                )
            }
            _ => Err(ProviderFault::new(
                "unknown_operation",
                format!("standard Tcp provider cannot invoke '{operation}'"),
            )),
        }
    }
}
