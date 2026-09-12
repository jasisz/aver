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

/// A loopback port nobody is listening on. The listener is bound only to be
/// told which port the kernel picked, then dropped, so the slice can bind it
/// itself: a `Tcp.Listener` the slice owns is the whole point of its
/// `Sockets` module, and a fixed port would collide with a parallel run.
pub fn free_port() -> u16 {
    // Not a port the kernel just handed out: a port nobody has bound.
    //
    // The old way bound an ephemeral listener, read its port and dropped it.
    // The tests of one binary run in parallel, and the slice takes about a
    // second to bind after it is spawned, so while it was still parsing, a
    // sibling test's throwaway listener could come up on the very same port
    // and accept this test's peer into its backlog; dropping it reset the
    // peer, and the slice then bound the port and waited for a client that was
    // already gone. No listener is bound here at all: ports are handed out
    // from a counter seeded by the process id, in a range below the kernel's
    // ephemeral ports, so a peer can only ever connect to the slice it belongs
    // to, and the first connect attempts are refused until that slice binds.
    static NEXT: std::sync::atomic::AtomicU16 = std::sync::atomic::AtomicU16::new(0);
    let base = 20_000 + (std::process::id() % 20_000) as u16;
    let taken = NEXT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
    base + (taken % 9_000)
}

/// Connects to the slice's listener, which is bound inside the first turn —
/// after the run has already been spawned — so the first attempts are
/// expected to be refused.
pub fn connect_when_bound(port: u16) -> TcpStream {
    let deadline = Instant::now() + Duration::from_secs(20);
    loop {
        match TcpStream::connect(("127.0.0.1", port)) {
            Ok(stream) => return stream,
            Err(error) => {
                assert!(
                    Instant::now() < deadline,
                    "the slice never bound 127.0.0.1:{port}: {error}"
                );
                thread::sleep(Duration::from_millis(10));
            }
        }
    }
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
