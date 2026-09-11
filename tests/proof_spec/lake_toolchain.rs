use super::*;

/// A fresh Elan installation can have the pinned toolchain installed without
/// a default. The generated proof's pin must govern availability checks as well
/// as builds, or conditional laws silently lose universal credit (BTC #338).
#[test]
fn proof_uses_project_toolchain_without_an_elan_default() {
    let pin = include_str!("../../aver-cert/assets/wall/current/lean-toolchain").trim();
    let Ok(which) = Command::new("elan")
        .env("ELAN_TOOLCHAIN", pin)
        .args(["which", "lake"])
        .output()
    else {
        eprintln!("skipping toolchain isolation test: Elan is not installed");
        return;
    };
    assert!(
        which.status.success(),
        "Elan must have the proof toolchain installed: {}",
        format_output(&which)
    );
    let lake = PathBuf::from(String::from_utf8(which.stdout).unwrap().trim());
    let toolchain = lake.parent().unwrap().parent().unwrap();
    let elan = std::env::split_paths(&std::env::var_os("PATH").unwrap_or_default())
        .map(|dir| dir.join("elan"))
        .find(|path| path.is_file())
        .expect("Elan was just invoked from PATH");

    let root = temp_output_dir("aver-proof-no-elan-default");
    let elan_home = root.join("elan");
    let toolchains = elan_home.join("toolchains");
    let bin = root.join("bin");
    std::fs::create_dir_all(&toolchains).unwrap();
    std::fs::create_dir_all(&bin).unwrap();
    std::os::unix::fs::symlink(toolchain, toolchains.join(toolchain.file_name().unwrap())).unwrap();
    // Force the Elan proxy even when a standalone Lake precedes it on PATH.
    std::os::unix::fs::symlink(elan, bin.join("lake")).unwrap();
    let path = std::env::join_paths(std::iter::once(bin).chain(std::env::split_paths(
        &std::env::var_os("PATH").unwrap_or_default(),
    )))
    .unwrap();
    let isolated = |executable: &str| {
        let mut command = Command::new(executable);
        command
            .current_dir(&root)
            .env("PATH", &path)
            .env("ELAN_HOME", &elan_home)
            .env_remove("ELAN_TOOLCHAIN");
        command
    };
    let unavailable = isolated("lake").arg("--version").output().unwrap();
    assert!(
        !unavailable.status.success()
            && format_output(&unavailable).contains("no default toolchain"),
        "the caller must have no usable default toolchain: {}",
        format_output(&unavailable)
    );

    let fixture = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("proof-corpus/decomposed/handwritten/all_zero_sum.av");
    // Relative output also checks that availability follows the export path.
    let run = isolated(env!("CARGO_BIN_EXE_aver"))
        .arg("proof")
        .arg(fixture)
        .args(["--check-json", "--minimize", "-o", "proof"])
        .output()
        .unwrap();
    let output = format_output(&run);
    assert!(run.status.success(), "{output}");
    let stdout = String::from_utf8_lossy(&run.stdout);
    let summary: serde_json::Value = serde_json::from_str(
        stdout
            .lines()
            .rev()
            .find(|line| line.starts_with('{'))
            .unwrap(),
    )
    .unwrap();
    assert_eq!(summary["universal_laws"], 2, "{output}");
    assert_eq!(summary["bounded_laws"], 0, "{output}");
    assert_eq!(summary["sorries"], 0, "{output}");
    assert_eq!(summary["build_errors"], 0, "{output}");
    assert!(
        stdout.contains("--minimize: collapsed "),
        "minimization must also use the pinned toolchain: {output}"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("proof/lean-toolchain"))
            .unwrap()
            .trim(),
        pin
    );
    std::fs::remove_dir_all(root).unwrap();
}
