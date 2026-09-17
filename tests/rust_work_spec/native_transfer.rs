//! Exercise the transfer at the generated program's real provider boundary.
use super::*;

#[test]
fn native_work_skips_codecs_but_recording_and_host_bindings_keep_the_value_abi() {
    let ws = temp_dir("native-transfer");
    let project = ws.join("project");
    let result = (|| -> Result<(), String> {
        compile_rust(
            "work_jobs_record",
            &project,
            "native_transfer",
            &["--with-replay"],
        )?;
        let scorer = project.join("src/aver_generated/scorer/mod.rs");
        let code = fs::read_to_string(&scorer).unwrap();
        let mut instrumented = String::new();
        let mut visits = 0;
        for line in code.lines() {
            instrumented.push_str(line);
            instrumented.push('\n');
            if line.contains("fn into_provider_value(") || line.contains("fn from_provider_value(")
            {
                instrumented.push_str("        assert!(std::env::var_os(\"AVER_TEST_FORBID_WORK_CODEC\").is_none(), \"native work traversed the value codec\");\n        eprintln!(\"work-codec-visited\");\n");
                visits += 1;
            }
        }
        assert_eq!(visits, 4, "instrument both directions for Task and Report");
        fs::write(scorer, instrumented).unwrap();
        let support = project.join("src/provider_support.rs");
        let mut code = fs::read_to_string(&support).unwrap();
        code.push_str(HOST_BINDING);
        fs::write(support, code).unwrap();
        let main = project.join("src/main.rs");
        let code = fs::read_to_string(&main).unwrap().replace(
            "fn bootstrap_provider_bindings() -> Result<(), String> {",
            "fn bootstrap_provider_bindings() -> Result<(), String> {\n    if std::env::var_os(\"AVER_TEST_WORK_HOST\").is_some() { return provider_support::install_test_work_host(); }",
        );
        fs::write(main, code).unwrap();
        let bin = cargo_build(&project, "native_transfer")?;
        let run = |env: &[(&str, &std::ffi::OsStr)], codecs: bool| -> Result<(), String> {
            let out = Command::new(&bin)
                .envs(env.iter().copied())
                .output()
                .unwrap();
            if !out.status.success() {
                return Err(format_output(&out));
            }
            let stdout = String::from_utf8_lossy(&out.stdout);
            for answer in ["job 1 scored 10 for alpha", "job 2 scored 24 for beta-two"] {
                assert!(stdout.contains(answer), "{}", format_output(&out));
            }
            assert_eq!(
                String::from_utf8_lossy(&out.stderr).contains("work-codec-visited"),
                codecs,
                "{}",
                format_output(&out)
            );
            Ok(())
        };
        run(&[("AVER_TEST_FORBID_WORK_CODEC", "1".as_ref())], false)?;
        let session = ws.join("session.json");
        run(&[("AVER_REPLAY_RECORD", session.as_os_str())], true)?;
        let ledger = fs::read_to_string(&session).unwrap();
        assert!(ledger.contains("Scorer.Task") && ledger.contains("Scorer.Report"));
        run(
            &[
                ("AVER_REPLAY_REPLAY", session.as_os_str()),
                ("AVER_TEST_FORBID_WORK_CODEC", "1".as_ref()),
            ],
            false,
        )?;
        run(&[("AVER_TEST_WORK_HOST", "replace".as_ref())], true)?;
        run(&[("AVER_TEST_WORK_HOST", "exact".as_ref())], true)?;
        for (mode, diagnostic) in [
            ("fault", "error[capability-provider-fault]"),
            ("invalid", "error[capability-provider-invalid-return]"),
        ] {
            let out = Command::new(&bin)
                .env("AVER_TEST_WORK_HOST", mode)
                .output()
                .unwrap();
            assert!(!out.status.success());
            assert!(
                String::from_utf8_lossy(&out.stderr).contains(diagnostic),
                "{}",
                format_output(&out)
            );
        }
        Ok(())
    })();
    let _ = fs::remove_dir_all(&ws);
    result.unwrap_or_else(|error| panic!("{error}"));
}

// A host that deliberately copies the compiler default's public identity.
// Selection must depend on the installed binding, never that identity string.
const HOST_BINDING: &str = r#"
struct TestWorkHost(aver_rt::provider::WorkKindProvider);
impl aver_rt::provider::CapabilityProvider for TestWorkHost {
    fn identity(&self) -> &str { "aver.work.Scorer/native" }
    fn fingerprint(&self) -> &str { aver_rt::provider::WORK_KIND_NATIVE_FINGERPRINT }
    fn invoke(&self, context: &aver_rt::provider::ProviderContext, args: &[ProviderValue]) -> Result<ProviderValue, aver_rt::provider::ProviderFault> {
        if context.operation.ends_with(".begin") {
            assert!(matches!(args, [ProviderValue::Record { .. }]), "host must receive the public value ABI");
        }
        if std::env::var("AVER_TEST_WORK_HOST").as_deref() == Ok("fault") {
            return Err(aver_rt::provider::ProviderFault::new("test-fault", "host rejected the task"));
        }
        if context.operation.ends_with(".take") && std::env::var("AVER_TEST_WORK_HOST").as_deref() == Ok("invalid") {
            return Ok(ProviderValue::ResultOk(Box::new(ProviderValue::OptionSome(Box::new(ProviderValue::Bool(true))))));
        }
        self.0.invoke(context, args)
    }
}
fn test_work_body(task: ProviderValue) -> Result<ProviderValue, String> {
    assert!(matches!(task, ProviderValue::Record { .. }));
    let answer = work_body_0(task)?;
    assert!(matches!(answer, ProviderValue::Record { .. }));
    Ok(answer)
}
pub fn install_test_work_host() -> Result<(), String> {
    let defaults = build_registry(Vec::new(), true)?;
    let spec = defaults.registry.contract("Scorer").unwrap();
    let host = ProviderBinding::new("Scorer", &spec.contract_hash, spec.operations.clone(),
        std::sync::Arc::new(TestWorkHost(aver_rt::provider::WorkKindProvider::new("Scorer", work_job_engine().clone(), test_work_body))));
    if std::env::var("AVER_TEST_WORK_HOST").as_deref() == Ok("exact") {
        let mut bindings = vec![host];
        for capability in ["Console", "Wait", "Work"] {
            if let Some(binding) = defaults.registry.binding(capability) { bindings.push(binding.clone()); }
        }
        install_provider_bindings_exact(bindings)?;
    } else {
        install_provider_bindings(vec![host])?;
    }
    preflight_required_providers()
}
"#;

#[test]
fn native_work_transfers_unit_tasks_and_unit_answers() {
    assert_same_stdout("work_jobs_unit_task");
    assert_same_stdout("work_jobs_unit_result");
}
