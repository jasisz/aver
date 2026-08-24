//! Contract guards for the monotonic `Process.stopRequested` capability.
//!
//! This test deliberately owns its copy of the law. It evaluates the actual
//! standard hostile-profile source through the VM, then checks every ordered
//! pair in a finite call prefix. It does not consult proof-export subtype
//! names or runtime helpers, so drift in either cannot make the guard pass by
//! construction.

use aver::nan_value::{Arena, NanValue, NanValueConvert};
use aver::types::checker::hostile_effects::{HostileProfile, hostile_profiles_for};
use aver::value::Value;
use aver::vm;

const CHECKED_CALL_PREFIX: i64 = 12;

fn profile_observations(profile: &HostileProfile) -> Vec<bool> {
    let legal_name = profile.stub_fn_name.replacen("__hostile_", "hostile__", 1);
    let legal_body = profile
        .stub_body
        .replacen(&profile.stub_fn_name, &legal_name, 1);
    let source = format!(
        "module ProcessProfileGuard\n    intent = \"evaluate one stop profile\"\n    depends []\n    effects []\n\n{legal_body}"
    );

    let mut items = aver::source::parse_source(&source).expect("parse Process hostile profile");
    aver::tco::transform_program(&mut items);
    aver::resolver::resolve_program(&mut items);
    let symbols = aver::ir::SymbolTable::build(&items, &[]);
    let resolved = aver::ir::hir::resolve_program(&symbols, &items);
    let mut arena = Arena::new();
    vm::register_service_types(&mut arena);
    let (code, globals) = vm::compile_program_with_modules(
        &resolved,
        &symbols,
        &mut arena,
        None,
        "<process-profile-guard>",
        None,
    )
    .expect("compile Process hostile profile");
    let mut machine = vm::VM::new(code, globals, arena);

    let path = Value::Variant {
        type_name: "BranchPath".to_string(),
        variant: "Root".to_string(),
        fields: Vec::new().into(),
    };
    (0..=CHECKED_CALL_PREFIX)
        .map(|call| {
            let args = [
                NanValue::from_value(&path, &mut machine.arena),
                NanValue::from_value(&Value::int(call), &mut machine.arena),
            ];
            let value = machine
                .run_named_function(&legal_name, &args)
                .expect("run Process hostile profile")
                .to_value(&machine.arena);
            let Value::Bool(value) = value else {
                panic!("{} returned non-Bool value {value:?}", profile.name);
            };
            value
        })
        .collect()
}

/// Independent copy of the contract law:
/// for every `i <= j`, a true observation at `i` requires true at `j`.
fn monotonicity_violation(observations: &[bool]) -> Option<(usize, usize)> {
    for i in 0..observations.len() {
        for j in i..observations.len() {
            if observations[i] && !observations[j] {
                return Some((i, j));
            }
        }
    }
    None
}

fn validate_profile(profile: &HostileProfile) -> Result<(), String> {
    let observations = profile_observations(profile);
    match monotonicity_violation(&observations) {
        Some((i, j)) => Err(format!(
            "profile {} violates stop monotonicity: call {i} is true but later call {j} is false; observations={observations:?}",
            profile.name
        )),
        None => Ok(()),
    }
}

#[test]
fn every_standard_process_profile_is_monotonic() {
    let profiles = hostile_profiles_for("Process.stopRequested");
    assert_eq!(
        profiles
            .iter()
            .map(|profile| profile.name)
            .collect::<Vec<_>>(),
        vec![
            "stop_never",
            "stop_immediately",
            "stop_after_one",
            "stop_after_three"
        ]
    );
    for profile in &profiles {
        validate_profile(profile).unwrap_or_else(|error| panic!("{error}"));
    }
}

#[test]
fn guard_rejects_an_injected_blinking_profile() {
    let blinking = HostileProfile {
        name: "blinking",
        stub_fn_name: "__hostile_Process_stopRequested_blinking".to_string(),
        stub_body: "fn __hostile_Process_stopRequested_blinking(path: BranchPath, call: Int) -> Bool\n    ? \"invalid injected profile: alternates forever\"\n    match Int.mod(call, 2)\n        0 -> false\n        _ -> true\n".to_string(),
    };
    let error = validate_profile(&blinking).expect_err("blinking profile must fail the guard");
    assert!(error.contains("call 1 is true but later call 2 is false"));
}

#[cfg(feature = "wasm")]
#[test]
fn embedded_wasm_gc_host_polls_the_native_process_provider() {
    use std::process::Command;

    let dir = tempfile::tempdir().expect("temporary Process wasm-gc project");
    let source_path = dir.path().join("process_poll.av");
    std::fs::write(
        &source_path,
        r#"module ProcessWasmGcHost
    intent = "exercise the embedded wasm-gc Process host binding"
    depends []
    effects [Process.stopRequested, Console.print]

fn main()
    ? "Print the initial cooperative-stop observation."
    ! [Process.stopRequested, Console.print]
    Console.print(String.fromBool(Process.stopRequested()))
"#,
    )
    .expect("write Process wasm-gc source");
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("run")
        .arg(&source_path)
        .arg("--wasm-gc")
        .output()
        .expect("run embedded wasm-gc host");
    assert!(
        output.status.success(),
        "wasm-gc Process host failed:\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "false");
}
