use super::*;

const DEP: &str = "module Dep\n    exposes [inc]\n    intent = \"A dependency with its own example and law.\"\n\nfn inc(n: Int) -> Int\n    n + 1\n\nverify inc\n    inc(1) => 2\n\nverify inc law incAddsOne\n    given n: Int = [0, 1]\n    inc(n) => n + 1\n";

const MAIN: &str = "module Main\n    depends [Dep]\n    exposes [twice]\n    intent = \"An entry with an example and a law.\"\n\nfn twice(n: Int) -> Int\n    n + n\n\nverify twice\n    twice(2) => 4\n\nverify twice law doubling\n    given n: Int = [0, 1, 2]\n    twice(n) => n * 2\n";

/// Export the two-module program and return `(Main.lean, Dep.lean)`.
fn export(root: &std::path::Path, extra: &[&str], name: &str) -> (String, String) {
    let destination = root.join(name);
    let output = Command::new(env!("CARGO_BIN_EXE_aver"))
        .arg("proof")
        .args(extra)
        .arg(root.join("main.av"))
        .arg("--module-root")
        .arg(root)
        .arg("-o")
        .arg(&destination)
        .output()
        .unwrap();
    assert!(output.status.success(), "{}", format_output(&output));
    (
        std::fs::read_to_string(destination.join("Main.lean")).unwrap(),
        std::fs::read_to_string(destination.join("Dep.lean")).unwrap(),
    )
}

#[test]
fn proof_states_verify_examples_only_with_the_examples_flag() {
    let root = temp_output_dir("aver-proof-examples-flag");
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("main.av"), MAIN).unwrap();
    std::fs::write(root.join("dep.av"), DEP).unwrap();

    let (main, dep) = export(&root, &[], "laws-only");
    assert!(
        !main.contains("example : twice 2") && !dep.contains("example : Dep.inc 1"),
        "without --examples no verify example is exported:\n{main}\n{dep}"
    );
    assert!(
        main.contains("theorem twice_law_doubling ") && dep.contains("theorem inc_law_incAddsOne "),
        "the laws are exported either way:\n{main}\n{dep}"
    );

    let (main, dep) = export(&root, &["--examples"], "with-examples");
    assert!(
        main.contains("example : twice 2 = (4 : Int)")
            && dep.contains("example : Dep.inc 1 = (2 : Int)"),
        "--examples exports the entry's and the dependency's examples:\n{main}\n{dep}"
    );
    assert!(
        main.contains("theorem twice_law_doubling ") && dep.contains("theorem inc_law_incAddsOne "),
        "{main}\n{dep}"
    );
    let _ = std::fs::remove_dir_all(root);
}
