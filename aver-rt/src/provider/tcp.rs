use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderResource, ProviderValue};

/// Standard native Tcp provider shared by the bytecode VM and generated Rust.
/// The capability boundary carries a `Tcp.Connection` as a provider-owned
/// resource; only this adapter can recover the host-side connection token.
#[derive(Debug, Clone, Copy, Default)]
pub struct StandardTcpProvider {
    settings: crate::tcp::TcpSettings,
}

impl StandardTcpProvider {
    pub fn new(settings: crate::tcp::TcpSettings) -> Self {
        Self { settings }
    }

    pub fn settings(&self) -> crate::tcp::TcpSettings {
        self.settings
    }
}

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
                tcp_result(
                    crate::tcp::send_with_settings(host, port, data, self.settings),
                    ProviderValue::String,
                )
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
                    crate::tcp::send_bytes_with_settings(host, port, payload, self.settings),
                    ProviderValue::Bytes,
                )
            }
            "Tcp.ping" => {
                let (host, port) = host_port(operation, args)?;
                tcp_result(
                    crate::tcp::ping_with_settings(host, port, self.settings),
                    |_| ProviderValue::Unit,
                )
            }
            "Tcp.connect" => {
                let (host, port) = host_port(operation, args)?;
                tcp_result(
                    crate::tcp::connect_with_settings(host, port, self.settings),
                    |connection| ProviderValue::Resource(ProviderResource::new(connection)),
                )
            }
            "Tcp.poll" => {
                let [ProviderValue::Map(entries), ProviderValue::Int(timeout_ms)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Map<Int, Tcp.Connection> connections, Int timeoutMs)",
                        args.len(),
                    ));
                };
                let Some(timeout_ms) = timeout_ms.to_i64() else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!("Tcp.poll: timeoutMs {timeout_ms} exceeds the poll limit"),
                    ))));
                };
                let mut keys = Vec::with_capacity(entries.len());
                let mut connections = Vec::with_capacity(entries.len());
                for (key, value) in entries {
                    let ProviderValue::Int(key) = key else {
                        return Err(ProviderFault::new(
                            "invalid_arguments",
                            "Tcp.poll expects Int map keys",
                        ));
                    };
                    keys.push(key.clone());
                    connections.push(connection(operation, value)?.clone());
                }
                tcp_result(crate::tcp::poll(&connections, timeout_ms), |positions| {
                    let mut ready = positions
                        .into_iter()
                        .filter_map(|position| keys.get(position).cloned())
                        .collect::<Vec<_>>();
                    ready.sort();
                    ready.dedup();
                    ProviderValue::List(ready.into_iter().map(ProviderValue::Int).collect())
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
            "Tcp.readSome" => {
                let [connection_value, ProviderValue::Int(max_bytes)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection, Int maxBytes)",
                        args.len(),
                    ));
                };
                let Some(max_bytes) = max_bytes.to_i64() else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!("Tcp.readSome: maxBytes {max_bytes} exceeds the read limit"),
                    ))));
                };
                tcp_result(
                    crate::tcp::read_some(connection(operation, connection_value)?, max_bytes),
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use std::net::TcpListener;
    use std::str::FromStr;
    use std::sync::mpsc;
    use std::thread;

    fn context(operation: &str) -> ProviderContext {
        ProviderContext {
            capability: "Tcp".to_string(),
            operation: operation.to_string(),
            contract_hash: "test-contract".to_string(),
            model_hash: "test-model".to_string(),
        }
    }

    #[test]
    fn poll_preserves_and_sorts_arbitrary_precision_caller_ids() {
        let listener = TcpListener::bind(("127.0.0.1", 0)).expect("bind loopback listener");
        let port = listener.local_addr().expect("listener address").port();
        let (written_tx, written_rx) = mpsc::channel();
        let (release_tx, release_rx) = mpsc::channel();
        let server = thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("accept loopback connection");
            stream.write_all(&[7]).expect("write ready byte");
            written_tx.send(()).expect("announce write");
            release_rx.recv().expect("hold peer open");
        });
        let connection =
            crate::tcp::connect("127.0.0.1", i64::from(port)).expect("connect loopback client");
        written_rx.recv().expect("peer wrote byte");

        let large = crate::AverInt::from_str("1208925819614629174706176").expect("large Int");
        let resource = ProviderResource::new(connection.clone());
        let result = StandardTcpProvider::default()
            .invoke(
                &context("Tcp.poll"),
                &[
                    ProviderValue::Map(vec![
                        (
                            ProviderValue::Int(crate::AverInt::from(10)),
                            ProviderValue::Resource(resource.clone()),
                        ),
                        (
                            ProviderValue::Int(large.clone()),
                            ProviderValue::Resource(resource),
                        ),
                    ]),
                    ProviderValue::Int(crate::AverInt::from(1_000)),
                ],
            )
            .expect("provider invocation");

        let ProviderValue::ResultOk(ready) = result else {
            panic!("Tcp.poll should succeed: {result:?}");
        };
        let ProviderValue::List(ready) = *ready else {
            panic!("Tcp.poll should return List<Int>");
        };
        let ready = ready
            .into_iter()
            .map(|value| match value {
                ProviderValue::Int(value) => value,
                other => panic!("Tcp.poll returned non-Int key: {other:?}"),
            })
            .collect::<Vec<_>>();
        assert_eq!(ready, [crate::AverInt::from(10), large]);

        crate::tcp::close(&connection).expect("close client");
        release_tx.send(()).expect("release peer");
        server.join().expect("server thread");
    }

    #[test]
    fn read_some_validation_is_an_aver_result_error() {
        let connection =
            crate::TcpConnection::from_parts("tcp-not-open".to_string(), String::new(), 0);
        let result = StandardTcpProvider::default()
            .invoke(
                &context("Tcp.readSome"),
                &[
                    ProviderValue::Resource(ProviderResource::new(connection)),
                    ProviderValue::Int(crate::AverInt::from(0)),
                ],
            )
            .expect("validation is not a provider fault");
        let ProviderValue::ResultErr(error) = result else {
            panic!("Tcp.readSome(0) should return Result.Err: {result:?}");
        };
        let ProviderValue::String(error) = *error else {
            panic!("Tcp.readSome error must be String");
        };
        assert!(error.contains("maxBytes 0 must be positive"), "{error}");
    }
}
