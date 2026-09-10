use super::*;

#[test]
fn dafny_explain_reports_open_source_steps_without_changing_the_gate() {
    if Command::new("dafny").arg("--version").output().is_err() {
        return;
    }
    let dir = tempfile::tempdir().unwrap();
    let file = dir.path().join("Example.av");
    let source = "module Example\n    intent = \"Source diagnostics retain the actual guard and ordered reasons.\"\n    effects []\nfn nonnegative(x: Int) -> Bool\n    x >= 0\nverify nonnegative law guarded\n    given x: Int = [2]\n    when x >= 0\n    nonnegative(x) holds\nfn identity(x: Int) -> Int\n    x\nverify identity law explained\n    given x: Int = [2]\n    when x >= 0\n    because nonnegative(x)\n    because x > 0\n    using [nonnegative.guarded]\n    identity(x) => x\n";
    std::fs::write(&file, source).unwrap();
    let run = |explain: bool, out: &str| {
        let mut command = Command::new(env!("CARGO_BIN_EXE_aver"));
        command
            .arg("proof")
            .arg(&file)
            .args(["--backend", "dafny", "--check-json", "-o"])
            .arg(dir.path().join(out));
        if explain {
            command.arg("--explain");
        }
        let output = command.output().unwrap();
        let text = String::from_utf8_lossy(&output.stdout);
        let summary: serde_json::Value = serde_json::from_str(
            text.lines()
                .rev()
                .find(|s| s.starts_with('{'))
                .unwrap_or_else(|| panic!("{}", format_output(&output))),
        )
        .unwrap();
        (summary, output)
    };
    let (plain, plain_run) = run(false, "plain");
    let (explained, explained_run) = run(true, "explained");
    assert!(!plain_run.status.success());
    assert_eq!(plain_run.status.code(), explained_run.status.code());
    for (key, value) in plain.as_object().unwrap() {
        assert_eq!(&explained[key], value, "{key}");
    }
    let report = &explained["explanations"]["identity.explained.because2"];
    assert_eq!(report["goal"], "x > 0", "{explained}");
    assert_eq!(report["status"], "unproved");
    assert_eq!(report["citations"][0]["law"], "nonnegative.guarded");
    assert_eq!(
        report["citations"][0]["requires"],
        serde_json::json!(["x >= 0"])
    );
    assert_eq!(report["citations"][0]["status"], "unresolved");
    assert_eq!(report["assumptions"][0]["expression"], "x >= 0");
    assert_eq!(report["assumptions"][1]["status"], "unresolved");
    assert_eq!(
        explained["claims"]["identity.explained.because1"]["status"],
        "unresolved"
    );
    assert!(dir.path().join("explained/proof_backend.log").exists());
    assert!(!dir.path().join("explained/proof_citations.log").exists());
    std::fs::write(&file, source.replace("because x > 0", "because x + 1 > 0")).unwrap();
    let (checked, run) = run(true, "checked");
    assert!(run.status.success(), "{}", format_output(&run));
    for claim in checked["claims"].as_object().unwrap().values() {
        assert_eq!(claim["exported"], true, "{checked}");
        assert_eq!(claim["status"], "checked", "{checked}");
    }
}
