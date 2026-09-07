use super::*;

#[test]
fn citation_attempt_decodes_a_private_imported_record_in_its_own_scope() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-citation-private-record");
    std::fs::create_dir_all(&dir).unwrap();
    let library = dir.join("lib.av");
    std::fs::write(
        &library,
        r#"module Lib
    exposes [identity]
    intent = "Cite an order law before simplification erases its zero argument."
    effects []

fn orderedProducts(a: Int, b: Int, factor: Int) -> Bool
    a * factor <= b * factor

verify orderedProducts law monotone
    given a: Int = [-2, 0, 1]
    given b: Int = [-1, 0, 3]
    given factor: Int = [0, 1, 4]
    when a <= b
    when factor >= 0
    because orderReason(a, b, factor)
    using []
    orderedProducts(a, b, factor) holds

fn orderReason(a: Int, b: Int, factor: Int) -> Bool
    match factor <= 0
        true -> orderedProducts(a, b, factor)
        false -> Bool.and(orderReason(a, b, factor - 1), orderedProducts(a, b, factor))

record Count
    value: Int
fn identity(value: Int) -> Int
    value
fn checked(a: Int, x: Count) -> Int
    x.value
verify checked law missingGuard
    given a: Int = [0, 1]
    given x: Count = [Count(value = -1), Count(value = 0)]
    when a >= 0
    because orderedProducts(0, a, x.value)
    using [orderedProducts.monotone]
    checked(a, x) => checked(a, x)
"#,
    )
    .unwrap();
    let entry = dir.join("entry.av");
    std::fs::write(
        &entry,
        r#"module Entry
    depends [Lib]
    intent = "Conflicting entry record metadata."
    effects []
record Count
    text: String
fn orderedProducts(a: Int, b: Int, x: Count) -> Bool
    x.text == "entry"
fn copy(value: Int) -> Int
    Lib.identity(value)
"#,
    )
    .unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        entry.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--explain", "--module-root", dir.to_str().unwrap()],
    );
    assert!(!run.status.success(), "the missing record guard must fail");
    assert_eq!(summary["passed"], false, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    let report = &summary["explanations"]["Lib.checked.missingGuard.because1"];
    assert_eq!(report["file"], library.to_str().unwrap(), "{summary}");
    let attempt = report["citation_attempts"]
        .as_array()
        .unwrap_or_else(|| panic!("missing citation attempts: {summary}"))
        .iter()
        .find(|attempt| attempt["law"] == "Lib.orderedProducts.monotone")
        .unwrap_or_else(|| panic!("missing private citation: {report}"));
    assert_eq!(attempt["phase"], "diagnostic_direct_application");
    assert_eq!(attempt["outcome"], "matched", "{attempt}");
    assert_eq!(attempt["law_status"], "universal", "{attempt}");
    assert_eq!(attempt["established_dependencies"], true, "{attempt}");
    assert!(
        attempt["premises"]
            .as_array()
            .unwrap()
            .iter()
            .any(|premise| {
                premise["status"] == "open"
                    && premise["source_form"] == "aver"
                    && premise["expression"] == "0 <= x.value"
            }),
        "the actual premise must use Lib.Count.value, not Entry.Count.text: {attempt}"
    );
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn automatic_citation_attempt_keeps_a_sorry_tainted_pool_unestablished() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-citation-automatic-tainted");
    std::fs::create_dir_all(&dir).unwrap();
    let entry = dir.join("entry.av");
    std::fs::write(
        &entry,
        r#"module AutomaticTainted
    intent = "Cite an order law before simplification erases its zero argument."
    effects []

fn orderedProducts(a: Int, b: Int, factor: Int) -> Bool
    a * factor <= b * factor

verify orderedProducts law monotone
    given a: Int = [-2, 0, 1]
    given b: Int = [-1, 0, 3]
    given factor: Int = [0, 1, 4]
    when a <= b
    when factor >= 0
    because false
    using []
    orderedProducts(a, b, factor) holds

fn orderReason(a: Int, b: Int, factor: Int) -> Bool
    match factor <= 0
        true -> orderedProducts(a, b, factor)
        false -> Bool.and(orderReason(a, b, factor - 1), orderedProducts(a, b, factor))

fn product(a: Int, b: Int) -> Int
    a * b

verify product law missingFactorGuard
    given a: Int = [0, 1, 3]
    given b: Int = [-1, 0, 4]
    when a >= 0
    because orderedProducts(0, a, b)
    product(a, b) => product(a, b)
"#,
    )
    .unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        entry.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--explain"],
    );
    assert!(!run.status.success(), "the false source reason must fail");
    assert_eq!(summary["passed"], false, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    assert_eq!(
        summary["obligations"]["product.missingFactorGuard.because1"], "failed",
        "the diagnostic must not award counted credit: {summary}"
    );
    let report = &summary["explanations"]["product.missingFactorGuard.because1"];
    assert_eq!(report["citation_mode"], "automatic", "{summary}");
    let attempts = report["citation_attempts"]
        .as_array()
        .unwrap_or_else(|| panic!("missing automatic attempts: {summary}"));
    let attempt = attempts
        .iter()
        .find(|attempt| attempt["law"] == "orderedProducts.monotone")
        .unwrap_or_else(|| panic!("missing automatically selected law: {report}"));
    assert_eq!(attempt["phase"], "diagnostic_direct_application");
    assert_eq!(attempt["outcome"], "matched", "{attempt}");
    assert_eq!(attempt["law_status"], "failed", "{attempt}");
    assert_eq!(attempt["established_dependencies"], false, "{attempt}");
    assert!(
        attempt["premises"]
            .as_array()
            .unwrap()
            .iter()
            .any(|premise| {
                premise["status"] == "open"
                    && premise["source_form"] == "aver"
                    && premise["expression"] == "0 <= b"
            }),
        "a matching failed law must still expose its actual missing premise: {attempt}"
    );
    assert!(
        attempt["available_laws"]
            .as_array()
            .unwrap()
            .iter()
            .any(|law| law["law"] == "orderedProducts.monotone" && law["status"] == "failed"),
        "the whole available citation pool must retain audit status: {attempt}"
    );
    for attempt in attempts {
        assert_eq!(attempt["established_dependencies"], false, "{attempt}");
    }
    let _ = std::fs::remove_dir_all(dir);
}

#[test]
fn automatic_citation_attempt_reports_the_actual_missing_premise() {
    if Command::new("lake").arg("--version").output().is_err() {
        return;
    }
    let dir = temp_output_dir("aver-citation-automatic-healthy");
    std::fs::create_dir_all(&dir).unwrap();
    let entry = dir.join("entry.av");
    std::fs::write(
        &entry,
        r#"module AutomaticHealthy
    intent = "Cite an order law before simplification erases its zero argument."
    effects []

fn orderedProducts(a: Int, b: Int, factor: Int) -> Bool
    a * factor <= b * factor

verify orderedProducts law monotone
    given a: Int = [-2, 0, 1]
    given b: Int = [-1, 0, 3]
    given factor: Int = [0, 1, 4]
    when a <= b
    when factor >= 0
    because orderReason(a, b, factor)
    using []
    orderedProducts(a, b, factor) holds

fn orderReason(a: Int, b: Int, factor: Int) -> Bool
    match factor <= 0
        true -> orderedProducts(a, b, factor)
        false -> Bool.and(orderReason(a, b, factor - 1), orderedProducts(a, b, factor))

fn product(a: Int, b: Int) -> Int
    a * b

verify product law missingFactorGuard
    given a: Int = [0, 1, 3]
    given b: Int = [-1, 0, 4]
    when a >= 0
    because orderedProducts(0, a, b)
    product(a, b) => product(a, b)
"#,
    )
    .unwrap();
    let (summary, run) = run_lean_check_json_with_args(
        entry.to_str().unwrap(),
        &dir.join("lean"),
        0,
        &[],
        &["--explain"],
    );
    assert!(!run.status.success(), "the missing premise must fail");
    assert_eq!(summary["passed"], false, "{summary}");
    assert_eq!(summary["build_errors"], 0, "{}", format_output(&run));
    let report = &summary["explanations"]["product.missingFactorGuard.because1"];
    assert_eq!(report["citation_mode"], "automatic", "{summary}");
    let attempt = report["citation_attempts"]
        .as_array()
        .unwrap_or_else(|| panic!("missing automatic attempts: {summary}"))
        .iter()
        .find(|attempt| attempt["law"] == "orderedProducts.monotone")
        .unwrap_or_else(|| panic!("missing automatic citation: {report}"));
    assert_eq!(attempt["phase"], "diagnostic_direct_application");
    assert_eq!(attempt["outcome"], "matched", "{attempt}");
    assert_eq!(attempt["law_status"], "universal", "{attempt}");
    assert_eq!(attempt["established_dependencies"], true, "{attempt}");
    assert!(
        attempt["premises"]
            .as_array()
            .unwrap()
            .iter()
            .any(|premise| {
                premise["status"] == "open"
                    && premise["source_form"] == "aver"
                    && premise["expression"] == "0 <= b"
            }),
        "{attempt}"
    );
    let _ = std::fs::remove_dir_all(dir);
}
