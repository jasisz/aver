//! Character-based export accounting preserves the wall's exact walk.

use super::*;

#[test]
fn certify_export_walk_characters_across_blocks_kernel_clean() {
    if !lean_required::lake_available() {
        return;
    }
    let source_dir = temp_dir("certify-export-characters-source");
    let source = source_dir.join("exports.av");
    let mut names: Vec<String> = (0..65).map(|i| format!("effect{i}")).collect();
    // The manifest's admitted display-name boundary is 200 ASCII bytes.
    names.push(format!("long{}", "Name".repeat(49)));
    let mut program = format!(
        "module ExportCharacters\n    intent = \"Certify export accounting across block boundaries.\"\n    exposes [identity, {}]\n    effects [Console]\n\nfn identity(x: Int) -> Int\n    x\n",
        names.join(", ")
    );
    for name in &names {
        program.push_str(&format!(
            "\nfn {name}() -> Unit\n    ! [Console.print]\n    Console.print(\"hello\")\n"
        ));
    }
    std::fs::write(&source, program).unwrap();
    let (out, _) = certify_fixture(source.to_str().unwrap(), &[], "certify-export-characters");
    let cert = out.join("cert");
    let exports = std::fs::read_to_string(cert.join("ArtifactExports.lean")).unwrap();
    assert!(cert.join("ExportChars.lean").exists());
    assert!(exports.matches("theorem exports_block_").count() >= 2);
    assert!(exports.contains("(AverCert.Artifact.ExportChars.walk_eq "));
    let (ok, report) = check_certificate(&out.join("exports.wasm"), &cert);
    assert!(ok, "{report}");
    assert!(
        report.contains("source-bridges: 1 of 1 credited"),
        "{report}"
    );

    materialize_wall(&cert);
    let build = Command::new("lake")
        .current_dir(&cert)
        .args(["build", "ArtifactExports"])
        .output()
        .unwrap();
    assert!(
        build.status.success(),
        "{}",
        aver_cmd::format_output(&build)
    );
    // The generic equivalence covers every branch of the walk. These
    // small examples also pin its empty/ASCII/Unicode reading behavior.
    let mut probe = String::from(
        "import ArtifactExports\n\
         open AverCert.Artifact.ExportChars\n\
         #print axioms AverCert.Artifact.ExportChars.walk_eq\n\
         example : asciiChars [] = some [] := by decide +kernel\n\
         example : asciiChars ['a', Char.ofNat 34, Char.ofNat 39, Char.ofNat 92, Char.ofNat 0] =\n\
             some [97, 34, 39, 92, 0] := by decide +kernel\n\
         example : asciiChars [Char.ofNat 233] = none := by decide +kernel\n\
         example : asciiChars [Char.ofNat 20013] = none := by decide +kernel\n\
         example : asciiChars [Char.ofNat 129408] = none := by decide +kernel\n\
         example : walk [] 0 0 none 0 [] [] = some (none, 0) := rfl\n",
    );
    let mut blocks = 0;
    for line in exports.lines() {
        if let Some(rest) = line.strip_prefix("theorem exports_block_") {
            let index = rest.split(" :").next().unwrap();
            probe.push_str(&format!(
                "#print axioms AverCert.Artifact.exports_block_{index}\n"
            ));
            blocks += 1;
        }
    }
    std::fs::write(cert.join("ExportCharactersAudit.lean"), probe).unwrap();
    let audit = Command::new("lake")
        .current_dir(&cert)
        .args(["env", "lean", "ExportCharactersAudit.lean"])
        .output()
        .unwrap();
    let report = aver_cmd::format_output(&audit);
    assert!(audit.status.success(), "{report}");
    assert_eq!(
        report.matches("'AverCert.Artifact.exports_block_").count(),
        blocks
    );
    assert!(!report.contains("sorryAx"), "{report}");
    trim_lean_build_tree(&cert);
}
