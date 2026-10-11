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
fn proof_states_examples_and_law_samples_only_with_the_examples_flag() {
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
    for text in [&main, &dep] {
        assert!(
            !text.contains("_sample_") && !text.contains("_checked_domain"),
            "without --examples no law sample is exported:\n{main}\n{dep}"
        );
    }

    let (main, dep) = export(&root, &["--examples"], "with-examples");
    assert!(
        main.contains("example : twice 2 = (4 : Int)")
            && dep.contains("example : Dep.inc 1 = (2 : Int)"),
        "--examples exports the entry's and the dependency's examples:\n{main}\n{dep}"
    );
    assert!(
        main.contains("theorem twice_law_doubling_checked_domain ")
            && main.contains("theorem twice_law_doubling_sample_3 ")
            && dep.contains("theorem inc_law_incAddsOne_checked_domain ")
            && dep.contains("theorem inc_law_incAddsOne_sample_2 "),
        "--examples also exports the law samples:\n{main}\n{dep}"
    );
    assert!(
        main.contains("theorem twice_law_doubling ") && dep.contains("theorem inc_law_incAddsOne "),
        "{main}\n{dep}"
    );
    let _ = std::fs::remove_dir_all(root);
}

const LITERAL_DEP: &str = "module Dep\n    exposes [inc]\n    intent = \"A dependency whose example and law sample read another function.\"\n\nfn inc(n: Int) -> Int\n    n + 1\n\nfn plusOne(n: Int) -> Int\n    1 + n\n\nverify inc\n    inc(1) => plusOne(1)\n\nverify inc law incIsPlusOne\n    given n: Int = [3, 4]\n    inc(n) => plusOne(n)\n";

const LITERAL_MAIN: &str = "module Main\n    depends [Dep]\n    intent = \"An entry that imports the dependency.\"\n\nfn twice(n: Int) -> Int\n    Dep.inc(n) + n - 1\n\nverify twice law doubling\n    given n: Int = [5]\n    twice(n) => n * 2\n";

/// With `--examples` a dependency's law sample carries the value the program
/// computed. Without the flag the law samples are not exported at all.
#[test]
fn proof_literalizes_a_dependency_law_sample_with_examples() {
    let root = temp_output_dir("aver-proof-dependency-literal");
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join("main.av"), LITERAL_MAIN).unwrap();
    std::fs::write(root.join("dep.av"), LITERAL_DEP).unwrap();

    let (main, dep) = export(&root, &[], "laws-only");
    assert!(
        !dep.contains("_sample_") && !main.contains("_sample_"),
        "without --examples no law sample is exported:\n{main}\n{dep}"
    );
    assert!(
        dep.contains("theorem inc_law_incIsPlusOne ")
            && main.contains("theorem twice_law_doubling "),
        "the laws themselves are exported:\n{main}\n{dep}"
    );

    let (_, dep) = export(&root, &["--examples"], "with-examples");
    assert!(
        dep.contains("theorem inc_law_incIsPlusOne_sample_1 : Dep.inc 3 = (4 : Int)")
            && dep.contains("example : Dep.inc 1 = (2 : Int)"),
        "--examples keeps the law sample and states the example with the program's value:\n{dep}"
    );
    let _ = std::fs::remove_dir_all(root);
}
