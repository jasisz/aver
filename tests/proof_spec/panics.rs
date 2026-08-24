use super::*;

const UNBOUNDED_FUEL_PROBE_AV: &str = r#"module FuelProbe
    intent = "unproven fuel-bound soundness probe"

fn pairsOf(level: List<Int>, acc: List<Int>) -> List<Int>
    ? "Compute the next level behind a user-function boundary."
    match level
        [] -> List.reverse(acc)
        [left, ..rest] -> pairsOf(rest, List.concat(acc, [left]))

fn stepSum(level: List<Int>) -> Int
    ? "Start one pass over a level."
    match level
        [] -> 0
        [only, ..rest] -> stepSumPeer(only, rest, level)

fn stepSumPeer(only: Int, rest: List<Int>, level: List<Int>) -> Int
    ? "Return one item or delegate another computed pass."
    match rest
        [] -> only
        [_, .._] -> stepSumAgain(level)

fn stepSumAgain(level: List<Int>) -> Int
    ? "Compute the next level before returning to the root."
    next = pairsOf(level, [])
    stepSum(next)

verify stepSum
    stepSum([7]) => 7
"#;

#[test]
fn count_model_panic_lines_matches_lake_panic_lines() {
    // Unit check on the exact output shapes the real toolchain produces (see
    // the lake-gated tests below, which pin these against live builds):
    // lake prefixes the first panic of a build step with its info
    // diagnostic, later panics print raw. All carry `PANIC at `; success
    // lines don't. The third line is a representative non-fuel prelude
    // panic — same panic-returns-`default` vacuity vector, different message
    // — which the original fuel-marker-only scan was blind to.
    let captured = "\u{2714} [2/4] Built AverCommon\n\
        \u{2139} [3/4] Built FuelProbe\n\
        info: ././././FuelProbe.lean:27:0: PANIC at stepSum__fuel FuelProbe:8:9: Aver proof fuel exhausted\n\
        PANIC at stepSumAcc__fuel FuelProbe:19:9: Aver proof fuel exhausted\n\
        info: ././././PanicProbe.lean:11:0: PANIC at PartialBuiltin AverCommon:11:12: synthetic partial builtin\n\
        Build completed successfully.\n";
    assert_eq!(aver::codegen::lean::count_model_panic_lines(captured), 3);
    assert_eq!(
        aver::codegen::lean::count_model_panic_lines("Build completed successfully.\n"),
        0
    );
    // The widening is load-bearing: the old scan keyed on the fuel-wrapper
    // message and counts only 2 of the 3 panic lines here — the
    // non-fuel panic slips through a marker-only gate (the #473 review gap
    // this test family closes).
    let old_marker_scan = captured
        .lines()
        .filter(|l| l.contains(aver::codegen::lean::PROOF_FUEL_EXHAUSTED_MSG))
        .count();
    assert_eq!(old_marker_scan, 2);
}

/// Artifact-local fuel gate (no real `lake` needed): even a perfectly green
/// verifier cannot make a sampled claim over a fuel-lowered function pass,
/// because the emitter refuses to state the claim before `native_decide` can
/// turn exhaustion into `default`. The structured refusal is charged by
/// `--check` and names the fuel dependency.
#[cfg(unix)]
#[test]
fn proof_check_refuses_a_fuel_lowered_sample_before_native_decide() {
    use std::os::unix::fs::PermissionsExt;

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-fuel-gate-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("probe.av");
    std::fs::write(&av, UNBOUNDED_FUEL_PROBE_AV).expect("write probe.av");

    // A green fake `lake` isolates the emitter refusal: no build error, sorry,
    // or panic is available to fail the check for us.
    let shim_dir = temp_output_dir("aver-fuel-gate-shim");
    std::fs::create_dir_all(&shim_dir).expect("create shim dir");
    let shim = shim_dir.join("lake");
    std::fs::write(
        &shim,
        "#!/bin/sh\n\
         echo \"Build completed successfully.\"\n\
         exit 0\n",
    )
    .expect("write lake shim");
    std::fs::set_permissions(&shim, std::fs::Permissions::from_mode(0o755))
        .expect("chmod lake shim");
    let path_env = format!(
        "{}:{}",
        shim_dir.display(),
        std::env::var("PATH").unwrap_or_default()
    );

    let out = temp_output_dir("aver-fuel-gate-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .env("PATH", &path_env)
        .output()
        .expect("expected `aver proof --check` to run");

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));

    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the refusal must not masquerade as a sorry\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["model_panicked"].as_bool(),
        Some(false),
        "the claim must be refused before native evaluation can panic\n{}",
        format_output(&run)
    );
    assert_eq!(summary["declined"].as_u64(), Some(1));
    let declined = summary["declined_claims"]
        .as_array()
        .and_then(|claims| claims.first())
        .expect("one structured fuel refusal");
    assert_eq!(declined["kind"].as_str(), Some("cases"));
    assert_eq!(declined["claim"].as_str(), Some("stepSum"));
    assert!(
        declined["reason"]
            .as_str()
            .is_some_and(|reason| reason.contains("stepSum") && reason.contains("default value")),
        "the refusal must name the fuel dependency and risk: {declined}"
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a fuel-lowered sampled claim must fail closed even against a green verifier\n{}",
        format_output(&run)
    );
    assert!(
        !run.status.success(),
        "`aver proof --check` must exit non-zero on the charged refusal\n{}",
        format_output(&run)
    );
    let entry = std::fs::read_to_string(out.join("FuelProbe.lean")).expect("read emitted entry");
    assert!(entry.contains("-- verify stepSum: the Lean call cone reaches fuel-lowered"));
    assert!(!entry.contains("example : stepSum 20"));
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("claim(s) were declined") && !stderr.contains("model panicked"),
        "the failure must be reported as a refusal, not a post-hoc model panic\n{}",
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&shim_dir);
    let _ = std::fs::remove_dir_all(&out);
}

/// Shared, valid source for model-panic gate tests and for the total
/// first-code-point regression. The empty-string case is deliberately
/// ordinary now: `String.firstCodePoint` returns `Option.None`, and
/// `Option.withDefault` supplies zero without a hidden model panic.
const CODE_POINT_TOTAL_PROBE_AV: &str = "module PanicProbe\n\
    \x20   intent = \"total first-code-point probe\"\n\
    \n\
    fn firstCode(s: String) -> Int\n\
    \x20   ? \"Unicode code point of the first scalar, or zero when empty.\"\n\
    \x20   Option.withDefault(String.firstCodePoint(s), 0)\n\
    \n\
    verify firstCode\n\
    \x20   firstCode(\"a\") => 97\n\
    \x20   firstCode(\"\") => 0\n";

/// Widening revert probe (no real `lake` needed — a PATH shim plays a
/// false-green build with a representative non-fuel panic line): a
/// non-fuel prelude panic shares the panic-returns-`default` vacuity vector,
/// so `aver proof --check` must fail hard on it too. With the widening
/// reverted (scan keyed on the fuel-exhaustion marker only) this scenario
/// exits 0 / `passed: true` — the #473 review gap this test pins shut.
#[cfg(unix)]
#[test]
fn proof_check_charges_non_fuel_prelude_panic_as_hard_failure() {
    use std::os::unix::fs::PermissionsExt;

    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-panic-gate-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("probe.av");
    std::fs::write(&av, CODE_POINT_TOTAL_PROBE_AV).expect("write probe.av");

    // PATH shim: a fake `lake` reproducing the captured false-green output —
    // a representative non-fuel panic line shape + exit 0.
    let shim_dir = temp_output_dir("aver-panic-gate-shim");
    std::fs::create_dir_all(&shim_dir).expect("create shim dir");
    let shim = shim_dir.join("lake");
    std::fs::write(
        &shim,
        "#!/bin/sh\n\
         echo \"info: ././././PanicProbe.lean:11:0: PANIC at PartialBuiltin AverCommon:11:12: synthetic partial builtin\"\n\
         echo \"Build completed successfully.\"\n\
         exit 0\n",
    )
    .expect("write lake shim");
    std::fs::set_permissions(&shim, std::fs::Permissions::from_mode(0o755))
        .expect("chmod lake shim");
    let path_env = format!(
        "{}:{}",
        shim_dir.display(),
        std::env::var("PATH").unwrap_or_default()
    );

    let out = temp_output_dir("aver-panic-gate-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .env("PATH", &path_env)
        .output()
        .expect("expected `aver proof --check` to run");

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));

    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "shim scenario must be sorry-free (that's what makes it a false green)\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["model_panicked"].as_bool(),
        Some(true),
        "--check-json must surface a NON-fuel model panic\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(false),
        "a non-fuel prelude panic in lake output must fail the check — \
         the kernel-certified sample equations are vacuous\n{}",
        format_output(&run)
    );
    assert!(
        !run.status.success(),
        "`aver proof --check` must exit non-zero on a model panic\n{}",
        format_output(&run)
    );
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("model panicked") && stderr.contains("Aver bug"),
        "the failure must be reported as a compiler-model bug\n{}",
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&shim_dir);
    let _ = std::fs::remove_dir_all(&out);
}

/// Pins the replacement API against the real toolchain: the empty-string
/// path is total in both Aver and Lean, so its sample is kernel-checkable
/// without a model panic.
#[test]
fn proof_check_accepts_total_first_code_point_in_lake_build() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping non-fuel panic toolchain test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-panic-toolchain-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("probe.av");
    std::fs::write(&av, CODE_POINT_TOTAL_PROBE_AV).expect("write probe.av");

    let out = temp_output_dir("aver-panic-toolchain-out");
    let run = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("expected `aver proof --check` to run");

    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with("{")))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)));
    let summary: serde_json::Value =
        serde_json::from_str(json_line).unwrap_or_else(|e| panic!("bad JSON ({e}):\n{json_line}"));

    // The export must contain the empty-string sample that used to expose
    // the partial Char model.
    let entry = std::fs::read_to_string(out.join("PanicProbe.lean")).expect("read emitted entry");
    assert!(
        entry.contains("example : firstCode \"\" = 0"),
        "expected the empty-string sample in the export:\n{entry}"
    );

    assert_eq!(
        summary["sorries"].as_u64(),
        Some(0),
        "the total model must be sorry-free\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["model_panicked"].as_bool(),
        Some(false),
        "the total code-point model must not panic\n{}",
        format_output(&run)
    );
    assert_eq!(
        summary["passed"].as_bool(),
        Some(true),
        "the total empty-string sample must pass\n{}",
        format_output(&run)
    );
    assert!(
        run.status.success(),
        "`aver proof --check` must accept the total code-point model\n{}",
        format_output(&run)
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}

/// Pins the panic-marker contract against the REAL toolchain (lake-gated):
/// post-edit a freshly emitted export to force fuel 0 on the wrappers (the
/// /tmp/vac_probe/out2_vac recipe), build it, and assert that (a) lake still
/// exits 0 — the false green is real, the OLD exit-code+sorry-count criteria
/// would certify it — and (b) the emitted panic message appears in the
/// captured output and the harness's scan function counts it, so the Fix A
/// gate fails the check. Guards both directions: if Lean ever stops printing
/// the panic line (or changes its shape), this fails loudly instead of the
/// gate going silently blind.
#[test]
fn lake_build_false_greens_on_forced_fuel_exhaustion_and_scan_catches_it() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping fuel-exhaustion toolchain test: `lake` not available");
        return;
    }
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let src = temp_output_dir("aver-fuel-toolchain-src");
    std::fs::create_dir_all(&src).expect("create src dir");
    let av = src.join("probe.av");
    std::fs::write(&av, FUEL_PROBE_AV).expect("write probe.av");

    let out = temp_output_dir("aver-fuel-toolchain-out");
    let emit = Command::new(aver_bin)
        .arg("proof")
        .arg(&av)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&out)
        .output()
        .expect("expected `aver proof` to run");
    assert!(
        emit.status.success(),
        "emit failed:\n{}",
        format_output(&emit)
    );

    // Force fuel 0 on the wrappers — the exhaustion scenario. Also strip the
    // ground-truth literal (`= 210` back to `= stepSumAcc 20`) so the export is
    // the historical model-vs-model shape: this test isolates Lean's panic-line
    // contract while the artifact-local gate is covered above by an actually
    // unbounded mutual group.
    let entry = out.join("FuelProbe.lean");
    let contents = std::fs::read_to_string(&entry).expect("read emitted entry");
    let mutated = contents.replace("((Int.natAbs n) + 1)", "0").replace(
        "example : stepSum 20 = 210",
        "example : stepSum 20 = stepSumAcc 20",
    );
    assert_ne!(
        contents, mutated,
        "expected to find the fuel expression to zero out:\n{contents}"
    );
    std::fs::write(&entry, &mutated).expect("write mutated entry");

    let build = Command::new("lake")
        .arg("build")
        .current_dir(&out)
        .output()
        .expect("expected `lake build` to run");
    let captured = format!(
        "{}\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    // (a) The false green is real: exhausted fuel collapses both sides to
    // `default`, the kernel certifies the vacuous equation, lake exits 0.
    assert!(
        build.status.success(),
        "expected the model-vs-model export to false-green under fuel 0 \
         (if this starts failing, Lean's panic semantics changed — re-evaluate \
         the gate):\n{captured}"
    );
    // (b) The panic line is in the captured output and the scan the
    // `--check` harness uses counts it — the gate flips the verdict. Also
    // pin that the line still carries the emitted fuel message: if the
    // emission constant and the build output ever drift apart, this fails
    // loudly here instead of silently downgrading the diagnostic.
    assert!(
        aver::codegen::lean::count_model_panic_lines(&captured) > 0,
        "fuel-exhaustion panic line missing from lake output — the --check \
         gate would go blind:\n{captured}"
    );
    assert!(
        captured.contains(aver::codegen::lean::PROOF_FUEL_EXHAUSTED_MSG),
        "the emitted fuel-exhaustion message should appear verbatim in lake \
         output:\n{captured}"
    );

    let _ = std::fs::remove_dir_all(&src);
    let _ = std::fs::remove_dir_all(&out);
}
