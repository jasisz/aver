//! The other side of the coordinator example's `Wire`, for the suites that
//! run `tests/fixtures/run_all_slice/`.
//!
//! That slice answers `Wire` over real sockets: it binds a loopback listener
//! on the port it is run with, accepts one peer there, writes each block body
//! with `Tcp.writeNow` and reads the answer with `Tcp.readNow`. The peer on
//! the other end of that conversation lives here, in the test, and never in
//! the fixture — and the port is a free one this module finds rather than a
//! fixed one, so two suites can run the slice at the same time.
//!
//! Each including suite only calls the subset it needs, so `dead_code` is
//! silenced here rather than per call site.
#![allow(dead_code)]

use std::io::{Read, Write};
use std::net::TcpStream;
use std::thread;
use std::time::{Duration, Instant};

/// How many block bodies the slice fetches over the wire: `Ledger.fresh`
/// makes the run three blocks long.
pub const BODIES: usize = 3;

/// How long one of those bodies is: `Ledger.blockOf` writes `block <height>`.
pub const BODY_LEN: usize = 7;

/// How long the peer holds a body back before sending it on. The read that
/// follows a finished write is asked in the very next turn, microseconds
/// after the last byte left, so the peer has to be slower than that for the
/// read to find nothing and park on `Connected` — which is the claim these
/// suites pin. A quarter of a second is far longer than a turn and far
/// shorter than the test's own patience.
pub const ECHO_DELAY: Duration = Duration::from_millis(250);

/// The lowest port `free_port` hands out.
pub const FIRST_PORT: u16 = 20_000;

/// One past the highest port `free_port` hands out. Linux hands out the
/// local ports of outgoing connections from 32768 up by default, and macOS
/// and Windows from 49152 up, so every port below this one is outside the
/// range a connect can be given as its own local port.
pub const PAST_LAST_PORT: u16 = 32_768;

/// A loopback port nobody is listening on. The slice binds it itself: a
/// `Tcp.Listener` the slice owns is the whole point of its `Sockets` module,
/// and a fixed port would collide with a parallel run.
pub fn free_port() -> u16 {
    // Not a port the kernel just handed out: a port nobody has bound.
    //
    // An earlier version bound an ephemeral listener, read its port and
    // dropped it. The tests of one binary run in parallel, and the slice
    // takes about a second to bind after it is spawned, so while it was still
    // parsing, a sibling test's throwaway listener could come up on the very
    // same port and accept this test's peer into its backlog; dropping it
    // reset the peer, and the slice then bound the port and waited for a
    // client that was already gone. No listener is bound here at all: ports
    // are handed out from a counter seeded by the process id, so a peer can
    // only ever connect to the slice it belongs to, and the first connect
    // attempts are refused until that slice binds.
    //
    // The range has to stay below the ports the kernel gives outgoing
    // connections. The peer starts connecting before the slice has bound,
    // and each refused attempt is given a fresh local port. On Linux, an
    // attempt that happens to be given the very port it is dialling connects
    // to itself: the connect succeeds, the slice can no longer bind that
    // port, and the peer then waits forever for a body nobody will send.
    static NEXT: std::sync::atomic::AtomicU16 = std::sync::atomic::AtomicU16::new(0);
    let span = (PAST_LAST_PORT - FIRST_PORT) / 2;
    let base = FIRST_PORT + (std::process::id() % u32::from(span)) as u16;
    let taken = NEXT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    base + (taken % span)
}

/// Connects to the slice's listener, which is bound inside the first turn —
/// after the run has already been spawned — so the first attempts are
/// expected to be refused. The peer starts before the CLI parses, checks and
/// compiles the program (including JIT on wasm). Allow that startup work to
/// finish even when sibling tests compete for the CI runner's CPUs; this
/// deadline does not govern the socket conversation once connected.
///
/// A connection whose two ends are the same address is the peer talking to
/// itself, not to the slice (see `free_port`); it is dropped and the next
/// attempt made, so the peer can never wait on its own silence.
pub fn connect_when_bound(port: u16) -> TcpStream {
    let deadline = Instant::now() + Duration::from_secs(60);
    loop {
        let error = match TcpStream::connect(("127.0.0.1", port)) {
            Ok(stream) if !connected_to_itself(&stream) => return stream,
            Ok(_) => "connected to itself".to_string(),
            Err(error) => error.to_string(),
        };
        assert!(
            Instant::now() < deadline,
            "the slice never bound 127.0.0.1:{port}: {error}"
        );
        thread::sleep(Duration::from_millis(10));
    }
}

/// Whether both ends of this connection are the same address.
pub fn connected_to_itself(stream: &TcpStream) -> bool {
    matches!(
        (stream.local_addr(), stream.peer_addr()),
        (Ok(local), Ok(peer)) if local == peer
    )
}

/// One peer that connects, takes whatever the slice writes it, and answers
/// nothing at all. The connection stays open — a peer that closed it would be
/// an end of stream rather than a silence — until the run ends and its side
/// of the socket goes with it, which is what the `read_to_end` here waits for.
/// The slice's read therefore finds nothing on every ask until the deadline it
/// was given runs out, and that is what makes `Wire.Heard.TimedOut` reachable.
pub fn silent_peer(port: u16) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut stream = connect_when_bound(port);
        let mut taken = Vec::new();
        let _ = stream.read_to_end(&mut taken);
    })
}

/// One peer that takes every body the slice writes it — in as many pieces as
/// the slice sends them — holds it back long enough for the read that follows
/// to find nothing, and then sends it back as the body that peer was asked
/// for.
pub fn loopback_peer(port: u16) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut stream = connect_when_bound(port);
        for _ in 0..BODIES {
            let mut body = vec![0u8; BODY_LEN];
            stream
                .read_exact(&mut body)
                .expect("the body the slice wrote");
            thread::sleep(ECHO_DELAY);
            stream.write_all(&body).expect("send the body back");
        }
    })
}
