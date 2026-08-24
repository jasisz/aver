//! Regression — `Args.get` must appear in the verify trace.
//!
//! `VmBuiltin::effects()` had no `Self::ArgsGet` arm, so `Args.get` fell
//! through the `_ => &[]` wildcard and the VM did not treat it as an effect at
//! all. The call still read the real process arguments; it just left no trace
//! entry behind. A `trace.length()` law therefore counted one event where two
//! had fired, and — the part that matters — the law stating the *wrong* count
//! passed while the law stating the truth failed.
//!
//! `Env.get` is the control: same Snapshot dimension, same shape, but it had
//! its arm and behaved correctly the whole time. That asymmetry is what kept
//! the omission from looking like a bug.
//!
//! The structural half of this invariant — every classified effect is reported
//! by `VmBuiltin::effects()` — is pinned by
//! `every_classified_effect_is_visible_to_the_vm` in src/vm/builtin.rs. This
//! test pins the behaviour that structure is there to produce.

use aver::diagnostics::vm_verify::run_verify_for_items_vm_with_mode;
use aver::source::parse_source;
use aver::verify_law::expand::ExpansionMode;

const SRC: &str = r#"module ArgsTrace
    intent = "An unstubbed classified effect must be recorded in the verify trace."
    effects [Args, Env, Random]

fn lowDie(path: BranchPath, n: Int, min: Int, max: Int) -> Result<Int, String>
    ? "Deterministic Random.int stub returning the lower bound."
    Result.Ok(min)

fn pickArgs() -> Int
    ? "Reads Args.get unstubbed, then draws one stubbed die."
    ! [Args.get, Random.int]
    args = Args.get()
    Random.int(1, 6)

fn pickEnv() -> Int
    ? "Control: reads Env.get unstubbed, then draws one stubbed die."
    ! [Env.get, Random.int]
    home = Env.get("HOME")
    Random.int(1, 6)

verify pickArgs trace
    given rnd: Random.int = [lowDie]
    r = pickArgs()
    r.trace.length() => 2
    r.trace.contains(Args.get()) => true

verify pickEnv trace
    given rnd: Random.int = [lowDie]
    r = pickEnv()
    r.trace.length() => 2
"#;

#[test]
fn args_get_is_recorded_in_the_verify_trace_like_env_get() {
    let items = parse_source(SRC).unwrap_or_else(|e| panic!("parse failed: {e:?}"));
    let results = run_verify_for_items_vm_with_mode(
        items,
        None,
        Some(env!("CARGO_MANIFEST_DIR")),
        "regression_args_get_trace.av",
        ExpansionMode::Declared,
    )
    .expect("verify run");

    assert_eq!(results.len(), 2, "two verify blocks");
    for result in &results {
        assert_eq!(
            result.failed, 0,
            "'{}' should see both effects in its trace — a shortfall here means a classified \
             effect fired without being recorded, which silently rewrites what every \
             trace-shaped law is checked against",
            result.fn_name
        );
    }
}
