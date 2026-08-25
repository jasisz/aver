use super::{TcpSettings, TcpSocket, format_connect_error, format_io_error};
use crate::{AverStr, TcpConnection, TcpDial, TcpListener};
use socket2::{Domain, Protocol, SockAddr, Socket, Type};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, BufReader};
use std::net::{IpAddr, Ipv4Addr, SocketAddr, TcpListener as StdTcpListener, TcpStream};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

static NEXT_ID: AtomicU64 = AtomicU64::new(1);

struct PendingDial {
    stream: TcpStream,
    host: String,
    port: i64,
    deadline: Instant,
    timeout: Duration,
    #[cfg(test)]
    poll_source_disabled: bool,
}

struct ListenerState {
    listener: StdTcpListener,
    max_connections: usize,
}

thread_local! {
    static CONNECTIONS: RefCell<HashMap<String, BufReader<TcpStream>>> =
        RefCell::new(HashMap::new());
    static DIALS: RefCell<HashMap<String, PendingDial>> = RefCell::new(HashMap::new());
    static LISTENERS: RefCell<HashMap<String, ListenerState>> = RefCell::new(HashMap::new());
}

pub(super) fn live_connection_count() -> usize {
    CONNECTIONS.with(|connections| connections.borrow().len())
        + DIALS.with(|dials| dials.borrow().len())
}

pub(super) fn insert_connection(id: String, stream: TcpStream) {
    CONNECTIONS.with(|connections| {
        connections.borrow_mut().insert(id, BufReader::new(stream));
    });
}

pub(super) fn register_connection(stream: TcpStream, host: String, port: i64) -> TcpConnection {
    let id = next_id("tcp");
    insert_connection(id.clone(), stream);
    TcpConnection {
        id: AverStr::from(id),
        host: AverStr::from(host),
        port,
    }
}

pub(super) fn with_connection_io<T>(
    conn: &TcpConnection,
    operation: &str,
    io: impl FnOnce(&mut BufReader<TcpStream>) -> io::Result<T>,
) -> Result<T, String> {
    CONNECTIONS.with(|connections| {
        let mut connections = connections.borrow_mut();
        let id: &str = &conn.id;
        let Some(reader) = connections.get_mut(id) else {
            return Err(format!("{operation}: unknown connection '{}'", conn.id));
        };
        match io(reader) {
            Ok(value) => Ok(value),
            Err(error) => {
                connections.remove(id);
                Err(format_io_error(operation, &error))
            }
        }
    })
}

pub(super) fn close_connection(conn: &TcpConnection) -> Result<(), String> {
    let id: &str = &conn.id;
    match CONNECTIONS.with(|connections| connections.borrow_mut().remove(id)) {
        Some(_) => Ok(()),
        None => Err(format!("Tcp.close: unknown connection '{}'", conn.id)),
    }
}

pub(super) fn listen(
    port: u16,
    backlog: i64,
    settings: TcpSettings,
) -> Result<TcpListener, String> {
    let backlog = i32::try_from(backlog)
        .ok()
        .filter(|backlog| *backlog > 0)
        .ok_or_else(|| {
            format!("Tcp.listen: backlog {backlog} must be a positive 32-bit integer")
        })?;
    let socket = Socket::new(Domain::IPV4, Type::STREAM, Some(Protocol::TCP))
        .map_err(|error| format_io_error("Tcp.listen", &error))?;
    socket
        .set_reuse_address(true)
        .map_err(|error| format_io_error("Tcp.listen", &error))?;
    socket
        .bind(&SockAddr::from(SocketAddr::new(
            IpAddr::V4(Ipv4Addr::UNSPECIFIED),
            port,
        )))
        .map_err(|error| format_io_error("Tcp.listen", &error))?;
    socket
        .listen(backlog)
        .map_err(|error| format_io_error("Tcp.listen", &error))?;
    socket
        .set_nonblocking(true)
        .map_err(|error| format_io_error("Tcp.listen", &error))?;

    let id = next_id("tcp-listener");
    LISTENERS.with(|listeners| {
        listeners.borrow_mut().insert(
            id.clone(),
            ListenerState {
                listener: socket.into(),
                max_connections: settings.max_connections,
            },
        );
    });
    Ok(TcpListener::from_id(id))
}

pub(super) fn accept(listener: &TcpListener) -> Result<Option<TcpConnection>, String> {
    LISTENERS.with(|listeners| {
        let listeners = listeners.borrow();
        let id: &str = &listener.id;
        let Some(state) = listeners.get(id) else {
            return Err(format!("Tcp.accept: unknown listener '{}'", listener.id));
        };
        if live_connection_count() >= state.max_connections {
            return Err(format!(
                "Tcp.accept: connection limit reached ({} max)",
                state.max_connections
            ));
        }
        match state.listener.accept() {
            Ok((stream, address)) => {
                stream
                    .set_nonblocking(false)
                    .map_err(|error| format_io_error("Tcp.accept", &error))?;
                Ok(Some(register_connection(
                    stream,
                    address.ip().to_string(),
                    i64::from(address.port()),
                )))
            }
            Err(error) if error.kind() == io::ErrorKind::WouldBlock => Ok(None),
            Err(error) if error.kind() == io::ErrorKind::ConnectionAborted => Ok(None),
            Err(error) => Err(format_io_error("Tcp.accept", &error)),
        }
    })
}

pub(super) fn close_listener(listener: &TcpListener) -> Result<(), String> {
    let id: &str = &listener.id;
    match LISTENERS.with(|listeners| listeners.borrow_mut().remove(id)) {
        Some(_) => Ok(()),
        None => Err(format!(
            "Tcp.closeListener: unknown listener '{}'",
            listener.id
        )),
    }
}

pub(super) fn begin_connect(
    host: &str,
    port: i64,
    address: SocketAddr,
    settings: TcpSettings,
) -> Result<TcpDial, String> {
    if live_connection_count() >= settings.max_connections {
        return Err(format!(
            "Tcp.beginConnect: connection limit reached ({} max)",
            settings.max_connections
        ));
    }
    let domain = if address.is_ipv4() {
        Domain::IPV4
    } else {
        Domain::IPV6
    };
    let socket = Socket::new(domain, Type::STREAM, Some(Protocol::TCP))
        .map_err(|error| format_io_error("Tcp.beginConnect", &error))?;
    socket
        .set_nonblocking(true)
        .map_err(|error| format_io_error("Tcp.beginConnect", &error))?;
    match socket.connect(&SockAddr::from(address)) {
        Ok(()) => {}
        Err(error) if connect_is_in_progress(&error) => {}
        Err(error) => {
            return Err(format_connect_error(
                "Tcp.beginConnect",
                &error,
                settings.connect_timeout,
            ));
        }
    }

    let id = next_id("tcp-dial");
    DIALS.with(|dials| {
        dials.borrow_mut().insert(
            id.clone(),
            PendingDial {
                stream: socket.into(),
                host: host.to_string(),
                port,
                deadline: Instant::now() + settings.connect_timeout,
                timeout: settings.connect_timeout,
                #[cfg(test)]
                poll_source_disabled: false,
            },
        );
    });
    Ok(TcpDial::from_id(id))
}

pub(super) fn dialled(dial: &TcpDial) -> Result<Option<TcpConnection>, String> {
    let id: &str = &dial.id;
    DIALS.with(|dials| {
        let mut dials = dials.borrow_mut();
        let Some(pending) = dials.get(id) else {
            return Err(format!("Tcp.dialled: unknown dial '{}'", dial.id));
        };
        if Instant::now() >= pending.deadline {
            let timeout = pending.timeout;
            dials.remove(id);
            return Err(format!(
                "Tcp.beginConnect: socket establishment timed out (deadline: {} ms)",
                timeout.as_millis()
            ));
        }
        if let Some(error) = pending
            .stream
            .take_error()
            .map_err(|error| format_io_error("Tcp.dialled", &error))?
        {
            let timeout = pending.timeout;
            dials.remove(id);
            return Err(format_connect_error("Tcp.beginConnect", &error, timeout));
        }
        if pending.stream.peer_addr().is_err() {
            return Ok(None);
        }

        let pending = dials.remove(id).expect("checked live dial");
        pending
            .stream
            .set_nonblocking(false)
            .map_err(|error| format_io_error("Tcp.dialled", &error))?;
        Ok(Some(register_connection(
            pending.stream,
            pending.host,
            pending.port,
        )))
    })
}

pub(super) fn close_dial(dial: &TcpDial) -> Result<(), String> {
    let id: &str = &dial.id;
    match DIALS.with(|dials| dials.borrow_mut().remove(id)) {
        Some(_) => Ok(()),
        None => Err(format!("Tcp.closeDial: unknown dial '{}'", dial.id)),
    }
}

pub(super) fn peer_address(connection: &TcpConnection) -> Result<String, String> {
    CONNECTIONS.with(|connections| {
        let connections = connections.borrow();
        let id: &str = &connection.id;
        let Some(reader) = connections.get(id) else {
            return Err(format!(
                "Tcp.peerAddress: unknown connection '{}'",
                connection.id
            ));
        };
        reader
            .get_ref()
            .peer_addr()
            .map(|address| address.to_string())
            .map_err(|error| format_io_error("Tcp.peerAddress", &error))
    })
}

pub(super) fn poll(sockets: &[TcpSocket], timeout_ms: i64) -> Result<Vec<usize>, String> {
    if timeout_ms < 0 {
        return Err(format!("Tcp.poll: timeoutMs {timeout_ms} is negative"));
    }
    CONNECTIONS.with(|connection_map| {
        DIALS.with(|dial_map| {
            LISTENERS.with(|listener_map| {
                poll_borrowed(
                    sockets,
                    timeout_ms,
                    &connection_map.borrow(),
                    &dial_map.borrow(),
                    &listener_map.borrow(),
                )
            })
        })
    })
}

#[derive(Clone, Copy)]
enum PollKind {
    Connection,
    Dial,
    Listener,
}

enum PollSource<'a> {
    Stream(&'a TcpStream),
    Listener(&'a StdTcpListener),
}

struct PollGroup<'a> {
    source: PollSource<'a>,
    kind: PollKind,
    positions: Vec<usize>,
}

fn poll_borrowed<'a>(
    sockets: &[TcpSocket],
    timeout_ms: i64,
    connection_map: &'a HashMap<String, BufReader<TcpStream>>,
    dial_map: &'a HashMap<String, PendingDial>,
    listener_map: &'a HashMap<String, ListenerState>,
) -> Result<Vec<usize>, String> {
    let mut ready = Vec::new();
    let mut groups = Vec::<PollGroup<'a>>::new();
    let mut connection_groups = HashMap::<String, usize>::new();
    let mut dial_groups = HashMap::<String, usize>::new();
    let mut listener_groups = HashMap::<String, usize>::new();

    let now = Instant::now();
    let mut nearest_deadline = None::<Duration>;
    for (position, socket) in sockets.iter().enumerate() {
        match socket {
            TcpSocket::Connected(connection) => {
                let id: &str = &connection.id;
                let Some(reader) = connection_map.get(id) else {
                    return Err(format!("Tcp.poll: unknown connection '{}'", connection.id));
                };
                if !reader.buffer().is_empty() {
                    ready.push(position);
                } else {
                    push_group(
                        &mut groups,
                        &mut connection_groups,
                        id,
                        PollSource::Stream(reader.get_ref()),
                        PollKind::Connection,
                        position,
                    );
                }
            }
            TcpSocket::Dialing(dial) => {
                let id: &str = &dial.id;
                let Some(pending) = dial_map.get(id) else {
                    return Err(format!("Tcp.poll: unknown dial '{}'", dial.id));
                };
                if now >= pending.deadline {
                    ready.push(position);
                } else {
                    let remaining = pending.deadline.saturating_duration_since(now);
                    nearest_deadline =
                        Some(nearest_deadline.map_or(remaining, |old| old.min(remaining)));
                    #[cfg(test)]
                    if pending.poll_source_disabled {
                        continue;
                    }
                    push_group(
                        &mut groups,
                        &mut dial_groups,
                        id,
                        PollSource::Stream(&pending.stream),
                        PollKind::Dial,
                        position,
                    );
                }
            }
            TcpSocket::Listening(listener) => {
                let id: &str = &listener.id;
                let Some(state) = listener_map.get(id) else {
                    return Err(format!("Tcp.poll: unknown listener '{}'", listener.id));
                };
                push_group(
                    &mut groups,
                    &mut listener_groups,
                    id,
                    PollSource::Listener(&state.listener),
                    PollKind::Listener,
                    position,
                );
            }
        }
    }

    let poller = polling::Poller::new().map_err(|error| format_io_error("Tcp.poll", &error))?;
    for (key, group) in groups.iter().enumerate() {
        let event = match group.kind {
            PollKind::Dial => polling::Event::writable(key),
            PollKind::Connection | PollKind::Listener => polling::Event::readable(key),
        };
        unsafe {
            match group.source {
                PollSource::Stream(stream) => poller.add(stream, event),
                PollSource::Listener(listener) => poller.add(listener, event),
            }
            .map_err(|error| format_io_error("Tcp.poll", &error))?;
        }
    }

    let already_ready = !ready.is_empty();
    let requested = Duration::from_millis(timeout_ms as u64);
    let timeout = if already_ready {
        Duration::ZERO
    } else {
        nearest_deadline.map_or(requested, |deadline| requested.min(deadline))
    };
    let capacity = std::num::NonZeroUsize::new(groups.len().max(1)).expect("positive capacity");
    let mut events = polling::Events::with_capacity(capacity);
    poller
        .wait(&mut events, Some(timeout))
        .map_err(|error| format_io_error("Tcp.poll", &error))?;
    collect_events(&events, &groups, &mut ready);

    let after_wait = Instant::now();
    for (position, socket) in sockets.iter().enumerate() {
        if let TcpSocket::Dialing(dial) = socket
            && dial_map
                .get(dial.id.as_ref())
                .is_some_and(|pending| after_wait >= pending.deadline)
        {
            ready.push(position);
        }
    }
    ready.sort_unstable();
    ready.dedup();
    Ok(ready)
}

fn push_group<'a>(
    groups: &mut Vec<PollGroup<'a>>,
    index: &mut HashMap<String, usize>,
    id: &str,
    source: PollSource<'a>,
    kind: PollKind,
    position: usize,
) {
    if let Some(group) = index.get(id).copied() {
        groups[group].positions.push(position);
    } else {
        index.insert(id.to_string(), groups.len());
        groups.push(PollGroup {
            source,
            kind,
            positions: vec![position],
        });
    }
}

fn collect_events(events: &polling::Events, groups: &[PollGroup<'_>], ready: &mut Vec<usize>) {
    for event in events.iter() {
        if let Some(group) = groups.get(event.key) {
            let signalled = match group.kind {
                PollKind::Dial => event.writable || event.is_err().unwrap_or(false),
                PollKind::Connection | PollKind::Listener => {
                    event.readable || event.is_err().unwrap_or(false)
                }
            };
            if !signalled {
                continue;
            }
            ready.extend(&group.positions);
        }
    }
}

fn connect_is_in_progress(error: &io::Error) -> bool {
    error.kind() == io::ErrorKind::WouldBlock
        || matches!(error.raw_os_error(), Some(36 | 115 | 10035 | 10036))
}

fn next_id(prefix: &str) -> String {
    format!("{prefix}-{}", NEXT_ID.fetch_add(1, Ordering::Relaxed))
}

#[cfg(test)]
pub(super) fn connection_exists(connection: &TcpConnection) -> bool {
    CONNECTIONS.with(|connections| connections.borrow().contains_key(connection.id.as_ref()))
}

#[cfg(test)]
pub(super) fn connection_timeouts(
    connection: &TcpConnection,
) -> Option<(Option<Duration>, Option<Duration>)> {
    CONNECTIONS.with(|connections| {
        let connections = connections.borrow();
        let reader = connections.get(connection.id.as_ref())?;
        Some((
            reader.get_ref().read_timeout().ok()?,
            reader.get_ref().write_timeout().ok()?,
        ))
    })
}

#[cfg(test)]
pub(super) fn listener_local_address(listener: &TcpListener) -> SocketAddr {
    LISTENERS.with(|listeners| {
        listeners
            .borrow()
            .get(listener.id.as_ref())
            .expect("live test listener")
            .listener
            .local_addr()
            .expect("listener local address")
    })
}

#[cfg(test)]
pub(super) fn insert_test_dial(stream: TcpStream, deadline_after: Duration) -> TcpDial {
    let id = next_id("tcp-test-dial");
    DIALS.with(|dials| {
        dials.borrow_mut().insert(
            id.clone(),
            PendingDial {
                stream,
                host: "test.invalid".to_string(),
                port: 9,
                deadline: Instant::now() + deadline_after,
                timeout: deadline_after,
                poll_source_disabled: true,
            },
        );
    });
    TcpDial::from_id(id)
}
