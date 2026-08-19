use super::*;

fn summary_from(output: &std::process::Output) -> serde_json::Value {
    let line = output
        .stdout
        .split(|byte| *byte == b'\n')
        .rev()
        .find_map(|line| {
            std::str::from_utf8(line)
                .ok()
                .filter(|text| text.starts_with('{'))
        })
        .unwrap_or_else(|| panic!("no JSON summary:\n{}", format_output(output)));
    serde_json::from_str(line).unwrap_or_else(|error| {
        panic!(
            "invalid JSON summary ({error}): {line}\n{}",
            format_output(output)
        )
    })
}

#[test]
fn lean_plain_verify_on_unreached_capability_arm_builds() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability proof test: `lake` not available");
        return;
    }

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source_dir = temp_output_dir("aver-capability-proof-src");
    std::fs::create_dir_all(&source_dir).expect("create capability source dir");
    std::fs::write(
        source_dir.join("Cap.av"),
        r#"module Cap
    intent = "One operation, so a consumer has something to dispatch."
    kind = capability
    semantics = effectful
    exposes [get]

operation get(key: String) -> Result<String, String>
    ? "What is stored under a key."
    oracle = generative
    replay = recorded
"#,
    )
    .expect("write capability module");
    std::fs::write(
        source_dir.join("main.av"),
        r#"module Main
    intent = "A store with one pure and one capability-backed arm."
    exposes [main, Backend, looked]
    depends [Cap]
    effects [Cap.get]

type Backend
    Memory(String)
    Remote

fn looked(backend: Backend, key: String) -> Result<String, String>
    ? "Answer from memory, or ask the capability."
    ! [Cap.get]
    match backend
        Backend.Memory(held) -> Result.Ok(held)
        Backend.Remote -> Cap.get(key)

fn fixed(path: BranchPath, n: Int, key: String) -> Result<String, String>
    ? "A concrete proof-side capability stub."
    Result.Ok(key)

verify looked
    looked(Backend.Memory("b"), "a") => Result.Ok("b")

verify looked
    given stub: Cap.get = [fixed]
    looked(Backend.Remote, "a") => Result.Ok("a")

fn main() -> Unit
    ? "Entry point."
    Unit
"#,
    )
    .expect("write consumer module");

    let verify = Command::new(aver_bin)
        .arg("verify")
        .arg(source_dir.join("main.av"))
        .arg("--module-root")
        .arg(&source_dir)
        .output()
        .expect("run runtime verify");
    assert!(
        verify.status.success(),
        "the Memory case must pass before proof export is exercised:\n{}",
        format_output(&verify)
    );

    let output_dir = temp_output_dir("aver-capability-proof-out");
    let proof = Command::new(aver_bin)
        .arg("proof")
        .arg(source_dir.join("main.av"))
        .arg("--module-root")
        .arg(&source_dir)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("run capability proof export");
    let summary = summary_from(&proof);
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(true), Some(0), Some(0)),
        "an unreached capability arm must still produce a buildable, sorry-free Lean claim:\n{}",
        format_output(&proof)
    );

    let main_lean =
        std::fs::read_to_string(output_dir.join("Main.lean")).expect("read generated Main.lean");
    assert!(
        main_lean.contains("example (rnd_Cap_get :"),
        "the unreached arm must quantify over its unused oracle:\n{main_lean}"
    );
    assert!(
        main_lean.contains("looked BranchPath.Root fixed Backend.remote"),
        "the reached arm with an explicit given must use that concrete stub:\n{main_lean}"
    );

    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
}

#[test]
fn lean_lifts_qualified_dependency_capability_calls() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping capability proof test: `lake` not available");
        return;
    }

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let source_dir = temp_output_dir("aver-qualified-capability-proof-src");
    std::fs::create_dir_all(source_dir.join("infra"))
        .expect("create qualified capability source dir");
    std::fs::write(
        source_dir.join("infra/kv.av"),
        r#"module Kv
    intent = "A qualified capability dependency."
    kind = capability
    semantics = effectful
    exposes [get]

operation get(key: String) -> Result<String, String>
    ? "Read one value."
    oracle = generative
    replay = recorded
"#,
    )
    .expect("write qualified capability module");
    std::fs::write(
        source_dir.join("store.av"),
        r#"module Store
    intent = "Dispatch to memory or a qualified capability."
    exposes [Backend, looked]
    depends [Infra.Kv]
    effects [Infra.Kv.get]

type Backend
    Memory(String)
    Remote

fn looked(backend: Backend, key: String) -> Result<String, String>
    ? "Answer from memory, or ask the qualified capability."
    ! [Infra.Kv.get]
    match backend
        Backend.Memory(held) -> Result.Ok(held)
        Backend.Remote -> Infra.Kv.get(key)

verify looked
    looked(Backend.Memory("b"), "a") => Result.Ok("b")
"#,
    )
    .expect("write qualified capability consumer");

    let verify = Command::new(aver_bin)
        .arg("verify")
        .arg(source_dir.join("store.av"))
        .arg("--module-root")
        .arg(&source_dir)
        .output()
        .expect("run runtime verify");
    assert!(
        verify.status.success(),
        "the Memory case must pass before proof export is exercised:\n{}",
        format_output(&verify)
    );

    let output_dir = temp_output_dir("aver-qualified-capability-proof-out");
    let proof = Command::new(aver_bin)
        .arg("proof")
        .arg(source_dir.join("store.av"))
        .arg("--module-root")
        .arg(&source_dir)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("run qualified capability proof export");
    let summary = summary_from(&proof);
    assert_eq!(
        (
            summary["passed"].as_bool(),
            summary["build_errors"].as_u64(),
            summary["sorries"].as_u64(),
        ),
        (Some(true), Some(0), Some(0)),
        "a qualified capability call must be replaced by its oracle in Lean:\n{}",
        format_output(&proof)
    );

    let store_lean =
        std::fs::read_to_string(output_dir.join("Store.lean")).expect("read generated Store.lean");
    assert!(
        store_lean.contains("rnd_Infra_Kv_get"),
        "the lifted definition must name the qualified capability oracle:\n{store_lean}"
    );
    assert!(
        !store_lean.contains("=> Infra.Kv.get "),
        "the proof artifact must not retain a host capability dispatch:\n{store_lean}"
    );

    let _ = std::fs::remove_dir_all(source_dir);
    let _ = std::fs::remove_dir_all(output_dir);
}
