use super::*;

/// A dependency's value built by one of its functions and taken apart by
/// the next is never built for the runtime (`ir::deforest_calls`) and is
/// exported to Lean as written: the pass is below the proof line one level
/// down as well, through the dependency lowering the proof exporter asks
/// for. The entry module is covered by `tests/proof_snapshot_seam.rs`; a
/// dependency's post-pass fns are what the exporter reads, so a gate left
/// on there sent the unfolded producer (with its `__df` binders) to Lean
/// and the laws that cite the two fns to `sorry`. Structural pin, Lean side
/// (no toolchain needed), beside the same site fused for the Rust target.
#[test]
fn proof_export_of_a_dependency_keeps_the_calls_the_runtime_fuses() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let aver_bin = env!("CARGO_BIN_EXE_aver");
    let root = repo_root.join("tests/fixtures/dep_deforest");
    let output_dir = temp_output_dir("aver-proof-dep-deforest-export");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("proof")
        .arg("--examples")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .arg("-o")
        .arg(&output_dir)
        .output()
        .expect("aver proof should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let lean = std::fs::read_to_string(output_dir.join("Lines.lean"))
        .expect("Lines.lean must be emitted for the dependency module");
    let count = lean
        .split("def count ")
        .nth(1)
        .expect("the dependency's count must be emitted")
        .split("termination_by")
        .next()
        .unwrap();
    assert!(
        count.contains("apply acc (") && count.contains("parse n)"),
        "the dependency's count calls apply and parse as written:\n{count}"
    );
    assert!(
        !lean.contains("__df") && !count.contains("n * 2"),
        "no producer is unfolded into the dependency's export:\n{lean}"
    );

    // The Rust target fuses the same site: the generated count binds the
    // App leaf's fields and calls neither function.
    let rust_dir = temp_output_dir("aver-compile-dep-deforest");
    let run = Command::new(aver_bin)
        .current_dir(&repo_root)
        .arg("compile")
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(&root)
        .arg("-o")
        .arg(&rust_dir)
        .output()
        .expect("aver compile should run");
    assert!(run.status.success(), "{}", format_output(&run));
    let generated = std::fs::read_to_string(rust_dir.join("src/aver_generated/lines/mod.rs"))
        .expect("the dependency module is generated");
    let count = generated
        .split("pub fn count(")
        .nth(1)
        .expect("count is generated")
        .split("\n}\n")
        .next()
        .unwrap();
    assert!(
        count.contains("__df") && !count.contains("apply(") && !count.contains("parse("),
        "the Rust target fuses the site:\n{count}"
    );
}
