use super::{CapabilityProvider, ProviderContext, ProviderFault, ProviderResource, ProviderValue};
/// Standard native Tcp provider shared by the bytecode VM and generated Rust.
/// The capability boundary carries `Tcp.Connection`, `Tcp.Dial`, and
/// `Tcp.Listener` as provider-owned resources; only this adapter can recover
/// their host-side tokens.
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

fn dial<'a>(
    operation: &str,
    value: &'a ProviderValue,
) -> Result<&'a crate::TcpDial, ProviderFault> {
    resource(operation, value, "Tcp.Dial")
}

fn listener<'a>(
    operation: &str,
    value: &'a ProviderValue,
) -> Result<&'a crate::TcpListener, ProviderFault> {
    resource(operation, value, "Tcp.Listener")
}

fn resource<'a, T: 'static>(
    operation: &str,
    value: &'a ProviderValue,
    expected: &str,
) -> Result<&'a T, ProviderFault> {
    let ProviderValue::Resource(resource) = value else {
        return Err(ProviderFault::new(
            "invalid_arguments",
            format!("{operation} expects a {expected} resource"),
        ));
    };
    resource.downcast_ref::<T>().ok_or_else(|| {
        ProviderFault::new(
            "invalid_resource",
            format!("{operation} received a resource from an incompatible provider"),
        )
    })
}

fn socket_map(
    operation: &str,
    value: &ProviderValue,
) -> Result<(Vec<crate::AverInt>, Vec<crate::tcp::TcpSocket>), ProviderFault> {
    let ProviderValue::Map(entries) = value else {
        return Err(ProviderFault::new(
            "invalid_arguments",
            format!("{operation} expects a Map<Int, Tcp.Socket>"),
        ));
    };
    let mut keys = Vec::with_capacity(entries.len());
    let mut sockets = Vec::with_capacity(entries.len());
    for (key, socket) in entries {
        let ProviderValue::Int(key) = key else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                "Tcp.poll expects Int map keys",
            ));
        };
        let ProviderValue::Variant {
            type_name,
            variant,
            fields,
        } = socket
        else {
            return Err(ProviderFault::new(
                "invalid_arguments",
                "Tcp.poll expects Tcp.Socket map values",
            ));
        };
        if type_name != "Tcp.Socket" || fields.len() != 1 {
            return Err(ProviderFault::new(
                "invalid_arguments",
                format!(
                    "Tcp.poll expects one-field Tcp.Socket variants, got {type_name}.{variant} with {} field(s)",
                    fields.len()
                ),
            ));
        }
        let socket = match variant.as_str() {
            "Listening" => crate::tcp::TcpSocket::Listening(
                resource::<crate::TcpListener>(operation, &fields[0], "Tcp.Listener")?.clone(),
            ),
            "Dialing" => crate::tcp::TcpSocket::Dialing(
                resource::<crate::TcpDial>(operation, &fields[0], "Tcp.Dial")?.clone(),
            ),
            "Connected" => crate::tcp::TcpSocket::Connected(
                resource::<crate::TcpConnection>(operation, &fields[0], "Tcp.Connection")?.clone(),
            ),
            other => {
                return Err(ProviderFault::new(
                    "invalid_arguments",
                    format!("Tcp.poll received unknown Tcp.Socket variant '{other}'"),
                ));
            }
        };
        keys.push(key.clone());
        sockets.push(socket);
    }
    Ok((keys, sockets))
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
            "Tcp.beginConnect" => {
                let (host, port) = host_port(operation, args)?;
                tcp_result(
                    crate::tcp::begin_connect_with_settings(host, port, self.settings),
                    |dial| ProviderValue::Resource(ProviderResource::new(dial)),
                )
            }
            "Tcp.dialled" => {
                let [dial_value] = args else {
                    return Err(invalid_arguments(operation, "(Tcp.Dial dial)", args.len()));
                };
                tcp_result(
                    crate::tcp::dialled(dial(operation, dial_value)?),
                    |settled| {
                        settled.map_or(ProviderValue::OptionNone, |connection| {
                            ProviderValue::OptionSome(Box::new(ProviderValue::Resource(
                                ProviderResource::new(connection),
                            )))
                        })
                    },
                )
            }
            "Tcp.listen" => {
                let [ProviderValue::Int(port), ProviderValue::Int(backlog)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Int port, Int backlog)",
                        args.len(),
                    ));
                };
                let Some(port) = port.to_i64() else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!("Tcp.listen: port {port} exceeds the host integer range"),
                    ))));
                };
                let Some(backlog) = backlog.to_i64() else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!("Tcp.listen: backlog {backlog} exceeds the host integer range"),
                    ))));
                };
                tcp_result(
                    crate::tcp::listen_with_settings(port, backlog, self.settings),
                    |listener| ProviderValue::Resource(ProviderResource::new(listener)),
                )
            }
            "Tcp.accept" => {
                let [listener_value] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Listener listener)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::accept(listener(operation, listener_value)?),
                    |accepted| {
                        accepted.map_or(ProviderValue::OptionNone, |connection| {
                            ProviderValue::OptionSome(Box::new(ProviderValue::Resource(
                                ProviderResource::new(connection),
                            )))
                        })
                    },
                )
            }
            "Tcp.peerAddress" => {
                let [connection_value] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Connection connection)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::peer_address(connection(operation, connection_value)?),
                    ProviderValue::String,
                )
            }
            "Tcp.poll" => {
                let [sockets_value, ProviderValue::Int(timeout_ms)] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Map<Int, Tcp.Socket> sockets, Int timeoutMs)",
                        args.len(),
                    ));
                };
                let Some(timeout_ms) = timeout_ms.to_i64() else {
                    return Ok(ProviderValue::ResultErr(Box::new(ProviderValue::String(
                        format!("Tcp.poll: timeoutMs {timeout_ms} exceeds the poll limit"),
                    ))));
                };
                let (keys, sockets) = socket_map(operation, sockets_value)?;
                tcp_result(crate::tcp::poll(&sockets, timeout_ms), |positions| {
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
            "Tcp.closeDial" => {
                let [dial_value] = args else {
                    return Err(invalid_arguments(operation, "(Tcp.Dial dial)", args.len()));
                };
                tcp_result(crate::tcp::close_dial(dial(operation, dial_value)?), |_| {
                    ProviderValue::Unit
                })
            }
            "Tcp.closeListener" => {
                let [listener_value] = args else {
                    return Err(invalid_arguments(
                        operation,
                        "(Tcp.Listener listener)",
                        args.len(),
                    ));
                };
                tcp_result(
                    crate::tcp::close_listener(listener(operation, listener_value)?),
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
        let connected = |resource| ProviderValue::Variant {
            type_name: "Tcp.Socket".to_string(),
            variant: "Connected".to_string(),
            fields: vec![ProviderValue::Resource(resource)],
        };
        let result = StandardTcpProvider::default()
            .invoke(
                &context("Tcp.poll"),
                &[
                    ProviderValue::Map(vec![
                        (
                            ProviderValue::Int(crate::AverInt::from(10)),
                            connected(resource.clone()),
                        ),
                        (ProviderValue::Int(large.clone()), connected(resource)),
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
