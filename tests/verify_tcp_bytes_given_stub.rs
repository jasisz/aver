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
//! The passing run below obtains a fresh provider-owned connection resource
//! from the `Tcp.connect` Oracle stub, then proves that the byte-read stub is
//! installed and returns the nominal `Bytes` value.

use aver::diagnostics::vm_verify::run_verify_for_items_vm;
use aver::source::parse_source;

#[test]
fn user_given_stub_for_tcp_read_bytes_typechecks_and_runs_on_vm() {
    let src = r#"module Prog
    intent = "User stub for Tcp.readBytes runs under verify."
    depends [Bytes]
    effects [Tcp]

fn connectStub(path: BranchPath, n: Int, fresh: Tcp.Connection, host: String, port: Int) -> Result<Tcp.Connection, String>
    ? "Mint the provider-owned test connection."
    Result.Ok(fresh)

fn readStub(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Honest stub returning a fixed frame."
    Result.Ok(Bytes.fromList([1, 2, 3, 4]))

fn readFrame() -> Result<Bytes, String>
    ? "Read one 4-byte frame."
    ! [Tcp.connect, Tcp.readBytes]
    conn = Tcp.connect("127.0.0.1", 1)?
    Tcp.readBytes(conn, 4)

verify readFrame trace
    given opener: Tcp.connect = [connectStub]
    given reader: Tcp.readBytes = [readStub]
    readFrame() => Result.Ok(Bytes.fromList([1, 2, 3, 4]))
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
