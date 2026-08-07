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
//! With injection working, the honest `normal_ok` profile hands the fn a
//! successful read — a verdict IMPOSSIBLE while the injected stubs are
//! absent, because without a stub the dispatched real builtin can only
//! fail (the fabricated connection id is unknown to the runtime), so
//! every case answers "err" and the law below reports 0 failures.

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

fn frameVerdict(conn: Tcp.Connection) -> String
    ? "Classify one exact-frame read."
    ! [Tcp.readBytes]
    match Tcp.readBytes(conn, 4)
        Result.Ok(_) -> "ok"
        Result.Err(_) -> "err"

verify frameVerdict law neverReads
    given conn: Tcp.Connection = [Tcp.Connection(id = "fake", host = "127.0.0.1", port = 1)]
    frameVerdict(conn) => "err"
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

    let profiles = hostile_profiles_for("Tcp.readBytes");
    assert!(
        profiles.len() >= 2,
        "Tcp.readBytes must ship multiple hostile profiles"
    );
    let total = result.passed + result.failed + result.skipped;
    assert_eq!(
        total,
        1 + profiles.len(),
        "one declared case plus one per injected Tcp.readBytes profile"
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
        vec!["Tcp.readBytes/normal_ok"],
        "exactly the honest injected profile must break the 'reads always fail' law — its \
         successful read is impossible without the injected stub (an unstubbed dispatch can \
         only fail on the fabricated connection); 0 hostile failures means the stubs were \
         never injected"
    );
}
