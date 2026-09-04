use super::*;

/// A dependency module's law that spells a builtin call (`List.concat`) in a
/// program whose closure also holds a module fn of the same bare name
/// (`Bytes.concat`): the law's simp set must name the law's own fn only. The
/// bare-name fallback of `find_fn_def_by_call_name` used to add a stray
/// `concat`, which resolves to nothing inside the dependency's namespace and
/// sent a law that closes as an entry module to `sorry` as a dependency
/// (jasisz/aver#1270). Structural pin, Lean side (no toolchain needed).
#[test]
fn proof_export_dep_law_builtin_call_never_resolves_to_module_fn() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = repo_root.join("tests/fixtures/dep_law_builtin_names");
    let output_dir = temp_output_dir("aver-proof-dep-law-names-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("Digits.lean"))
        .expect("Digits.lean must be emitted for the dependency module");
    let theorem = lean
        .split("theorem bigEndian_law_readsTheLastByteLast :")
        .nth(1)
        .expect("the dependency law theorem must be emitted");
    let body: String = theorem.lines().take(12).collect::<Vec<_>>().join("\n");
    assert!(
        !body.contains("concat"),
        "the law's simp set must not carry a bare `concat` (List.concat is a builtin, Bytes.concat is another module's fn):\n{body}"
    );
    assert!(
        lean.contains("-- aver:law-class bigEndian_law_readsTheLastByteLast universal"),
        "the dependency law must be classed universal"
    );
    // The entry law over the dependency's countdown fn is proved by fuel
    // induction and cites the dependency's accumulator law as a ground
    // instance by its namespace-qualified theorem name.
    let entry = std::fs::read_to_string(output_dir.join("DepLawEntry.lean"))
        .expect("DepLawEntry.lean must be emitted for the entry module");
    let rung = entry
        .split("theorem readBack_law_readsBackDigits :")
        .nth(1)
        .expect("the entry law theorem must be emitted");
    let rung_body: String = rung.lines().take(30).collect::<Vec<_>>().join("\n");
    assert!(
        rung_body.contains("have key : ∀ (k : Nat)"),
        "the entry law must be proved by fuel induction:\n{rung_body}"
    );
    assert!(
        rung_body.contains("Digits.digits_law_accumulatorComesFirst"),
        "the entry law must cite the dependency's accumulator law by its qualified name:\n{rung_body}"
    );
    assert!(
        entry.contains("-- aver:law-class readBack_law_readsBackDigits universal"),
        "the entry when-law must be stated universally"
    );
}

/// The same fixture through `lake`: the dependency law closes kernel-genuine.
#[test]
fn proof_dep_law_builtin_names_lean_closes_kernel_genuine() {
    if Command::new("lake").arg("--version").output().is_err() {
        eprintln!("skipping dep-law names lake test: `lake` not available");
        return;
    }
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = repo_root.join("tests/fixtures/dep_law_builtin_names");
    let output_dir = temp_output_dir("aver-proof-dep-law-names-check");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .arg("--backend")
        .arg("lean")
        .arg("-o")
        .arg(&output_dir)
        .arg("--check")
        .arg("--check-json")
        .output()
        .expect("aver proof --check --check-json should run");
    let json_line = run
        .stdout
        .split(|&b| b == b'\n')
        .rev()
        .find_map(|l| std::str::from_utf8(l).ok().filter(|s| s.starts_with('{')))
        .unwrap_or_else(|| panic!("no JSON line:\n{}", format_output(&run)))
        .to_string();
    let summary: serde_json::Value = serde_json::from_str(&json_line).expect("check-json summary");
    assert_eq!(summary["build_errors"], 0, "{json_line}");
    assert_eq!(summary["sorries"], 0, "{json_line}");
    assert_eq!(summary["universal"], true, "{json_line}");
    assert_eq!(summary["universal_laws"], 3, "{json_line}");
}
