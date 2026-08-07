//! Regression — a user-written `given` stub for the byte-carrying TCP
//! methods must typecheck and actually run on the VM.
//!
//! Pre-fix, `effect_classification::oracle_signature` returned types
//! built from bare source names (`Type::Named { id: None, name: "Bytes" }`)
//! while the user's stub fn had its signature canonicalized against the
//! loaded stdlib `Bytes` module (`id: Some(..)`). The typed-identity
//! matcher rejects mixed `(Some, None)` comparisons, so the verify
//! block's stub-type check failed with an error printing two
//! identical-looking signatures, and the block never reached the VM.
//!
//! The passing run below is only possible with the user stub installed:
//! the `given` connection is fabricated, so an unstubbed dispatch of the
//! real `Tcp.readBytes` builtin can only return `Result.Err`.

use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::source::parse_source;

#[test]
fn user_given_stub_for_tcp_read_bytes_typechecks_and_runs_on_vm() {
    let src = r#"module Prog
    intent = "User stub for Tcp.readBytes runs under verify."
    depends [Bytes]
    effects [Tcp]

fn readStub(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Honest stub returning a fixed frame."
    Bytes.fromList([1, 2, 3, 4])

fn readFrame(conn: Tcp.Connection) -> Result<Bytes, String>
    ? "Read one 4-byte frame."
    ! [Tcp.readBytes]
    Tcp.readBytes(conn, 4)

verify readFrame trace
    given conn: Tcp.Connection = [Tcp.Connection(id = "fake", host = "127.0.0.1", port = 1)]
    given reader: Tcp.readBytes = [readStub]
    readFrame(conn) => Bytes.fromList([1, 2, 3, 4])
"#;
    let items = parse_source(src).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "verify_tcp_bytes_given_stub.av",
    )
    .expect("verify run must typecheck clean — a raw-named oracle signature rejects the stub");
    assert_eq!(results.len(), 1);
    let result = &results[0];
    assert_eq!(
        (result.passed, result.failed, result.skipped),
        (1, 0, 0),
        "the stubbed read must return the fixed frame; failures: {:?}",
        result.failures
    );
}
