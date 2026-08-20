//! Regression — the hostile verify pipeline must actually inject the
//! hostile effect-profile stubs for `Tcp.readBytes`.
//!
//! Pre-fix, `inject_hostile_effect_stubs_for_blocks` routed effect method
//! names through a hand-maintained `&'static str` mapping whose catch-all
//! returned `""` for the byte-carrying TCP methods (`Tcp.sendBytes`,
//! `Tcp.readBytes`, `Tcp.writeBytes`). `hostile_profiles_for("")` is empty,
//! so hostile law verification silently ran every "adversarial" case under
//! the user's own honest stub — the run below reported 0 failures.
//!
//! With injection working, the honest `normal_ok` profiles mint a fresh
//! connection resource and hand the fn a successful read. Without the
//! injected read profile, the user's `alwaysErrRead` stub makes every case
//! answer "err", so the law below reports 0 failures.

use aver::checker::VerifyCaseOutcome;
use aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode;
use aver::source::parse_source;
use aver::types::checker::hostile_effects::hostile_profiles_for;
use aver::verify_law::expand::ExpansionMode;

#[test]
fn hostile_law_run_exercises_injected_tcp_read_bytes_stubs() {
    let src = r#"module M
    intent = "Hostile verification must model Tcp.readBytes adversarially."
    depends [Bytes]
    effects [Tcp]

fn connectStub(path: BranchPath, n: Int, fresh: Tcp.Connection, host: String, port: Int) -> Result<Tcp.Connection, String>
    ? "Mint the provider-owned test connection."
    Result.Ok(fresh)

fn alwaysErrRead(path: BranchPath, n: Int, conn: Tcp.Connection, count: Int) -> Result<Bytes, String>
    ? "Keep the declared world distinguishable from the honest hostile profile."
    Result.Err("declared: no frame")

fn frameVerdict() -> String
    ? "Classify one exact-frame read."
    ! [Tcp.connect, Tcp.readBytes]
    match Tcp.connect("127.0.0.1", 1)
        Result.Err(_) -> "err"
        Result.Ok(conn) -> match Tcp.readBytes(conn, 4)
            Result.Ok(_) -> "ok"
            Result.Err(_) -> "err"

verify frameVerdict law neverReads
    given opener: Tcp.connect = [connectStub]
    given reader: Tcp.readBytes = [alwaysErrRead]
    frameVerdict() => "err"
"#;
    let items = parse_source(src).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "hostile_injection_regression.av",
        ExpansionMode::Hostile,
    )
    .expect("hostile verify run");
    assert_eq!(results.len(), 1);
    let result = &results[0];

    let connect_profiles = hostile_profiles_for("Tcp.connect");
    let profiles = hostile_profiles_for("Tcp.readBytes");
    assert!(
        profiles.len() >= 2,
        "Tcp.readBytes must ship multiple hostile profiles"
    );
    let total = result.passed + result.failed + result.skipped;
    assert_eq!(
        total,
        1 + connect_profiles.len() * profiles.len(),
        "one declared case plus the Tcp.connect × Tcp.readBytes hostile worlds"
    );

    let hostile_failures: Vec<&str> = result
        .case_results
        .iter()
        .filter(|c| {
            !matches!(
                c.outcome,
                VerifyCaseOutcome::Pass | VerifyCaseOutcome::Skipped
            )
        })
        .filter_map(|c| c.hostile_profile.as_deref())
        .collect();
    assert_eq!(
        hostile_failures,
        vec!["Tcp.connect/normal_ok + Tcp.readBytes/normal_ok"],
        "exactly the all-honest world must break the 'reads always fail' law; 0 hostile \
         failures means the read profile was never injected"
    );
}
