//! Separately cached body proofs must retain every source bridge: both
//! across an independent source edit and across image-module boundaries.
use super::aver_cmd::{aver_command, format_output};
use super::scratch_dir::temp_dir;
use std::path::{Path, PathBuf};

fn leaf_module(module: &str, prefix: &str, edited: bool) -> String {
    // Four export-assembly slices leave a pure Right slice even if the
    // entry function shares the final slice; body slices hold 24 functions.
    let names = (0..96)
        .map(|i| format!("{prefix}{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let mut source = format!(
        "module {module}\n    intent = \"Independent leaves for bridge cache regression.\"\n    exposes [{names}]\n\n"
    );
    for i in 0..96 {
        let delta = if edited && i == 0 { 2 } else { 1 };
        source.push_str(&format!(
            "fn {prefix}{i}(x: Int) -> Int\n    x + {delta}\n\n"
        ));
    }
    source
}

fn compile(root: &Path, name: &str) -> PathBuf {
    let out = root.join(name);
    let output = aver_command()
        .current_dir(root)
        .args([
            "compile",
            "main.av",
            "--target",
            "wasm-gc",
            "--certify",
            "--examples",
            "-o",
        ])
        .arg(&out)
        .output()
        .expect("compile bridge cache fixture");
    assert!(output.status.success(), "{}", format_output(&output));
    out
}

fn check(root: &Path, package: &Path, exports: usize) -> String {
    let output = aver_command()
        .args(["cert", "check"])
        .arg(package.join("main.wasm"))
        .arg(package.join("cert"))
        .env("AVER_CERT_DATA_CACHE", root.join("data-cache"))
        .env("AVER_CERT_PRELUDE_CACHE", root.join("wall-cache"))
        .env("AVER_CERT_BUILD_JOBS", "2")
        .env("AVER_CERT_TIMINGS", "1")
        .output()
        .expect("check bridge cache fixture");
    let report = format_output(&output);
    assert!(output.status.success(), "{report}");
    assert!(
        report.contains(&format!("{exports} checked exports")),
        "{report}"
    );
    assert!(
        report.contains(&format!("source-bridges: {exports} of {exports} credited")),
        "{report}"
    );
    report
}

pub fn check_independent_edit() {
    let root = temp_dir("bridge-body-cache");
    std::fs::create_dir(root.join("cache")).unwrap();
    std::fs::write(root.join("cache/left.av"), leaf_module("Left", "a", false)).unwrap();
    std::fs::write(
        root.join("cache/right.av"),
        leaf_module("Right", "b", false),
    )
    .unwrap();
    std::fs::write(root.join("main.av"),
        "module StepCache\n    intent = \"Join two independent modules.\"\n    depends [Cache.Left, Cache.Right]\n    exposes [entry]\n\nfn entry(x: Int) -> Int\n    Cache.Left.a0(x) + Cache.Right.b0(x)\n"
    ).unwrap();
    let before = compile(&root, "before");
    // Pick a pure Right slice without assuming where the entry function is
    // placed in the compiler's function-index order.
    let parts: Vec<_> = std::fs::read_dir(before.join("cert"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|path| {
            let name = path.file_name().unwrap().to_str().unwrap();
            if !(name.starts_with("BridgeBodies") || name.starts_with("BridgeAssembly"))
                || !name.ends_with(".lean")
            {
                return false;
            }
            let text = std::fs::read_to_string(path).unwrap();
            text.contains("`Cache.Right.")
                && !text.contains("`Cache.Left.")
                && !text.contains("`StepCache.entry`")
                // Export and image partitions have different widths. An
                // export-only Right slice may still import a mixed image
                // slice at the Left/Right boundary and legitimately rebuild.
                && text.lines().filter_map(|line| line.strip_prefix("import BridgeImages"))
                    .all(|suffix| {
                        let image = std::fs::read_to_string(
                            before.join("cert").join(format!("BridgeImages{suffix}.lean"))
                        ).unwrap();
                        !image.contains("import AverModel.Cache.Left\n")
                            && !image.contains("import AverModel.StepCache\n")
                    })
        })
        .collect();
    for prefix in ["BridgeBodies", "BridgeAssembly"] {
        assert!(
            parts
                .iter()
                .any(|p| p.file_stem().unwrap().to_str().unwrap().starts_with(prefix)),
            "an independent Right {prefix} slice"
        );
    }
    let first = check(&root, &before, 193);
    for part in &parts {
        let name = part.file_stem().unwrap().to_str().unwrap();
        assert!(first.contains(&format!("Built {name} (")), "{first}");
    }

    // Exactly one function changes, in the other source module.
    std::fs::write(root.join("cache/left.av"), leaf_module("Left", "a", true)).unwrap();
    let after = compile(&root, "after");
    assert_ne!(
        std::fs::read(before.join("main.wasm")).unwrap(),
        std::fs::read(after.join("main.wasm")).unwrap()
    );
    let second = check(&root, &after, 193);
    for part in &parts {
        assert_eq!(
            std::fs::read_to_string(part).unwrap(),
            std::fs::read_to_string(after.join("cert").join(part.file_name().unwrap())).unwrap()
        );
        let name = part.file_stem().unwrap().to_str().unwrap();
        assert!(
            !second.contains(&format!("Built {name} (")),
            "unchanged {name} was rebuilt:\n{second}"
        );
    }
}

pub fn check_decoded_sum_calls() {
    let root = temp_dir("bridge-body-calls");
    let fixture =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tools/certkit/fixtures/bridge_body_calls.av");
    let source = std::fs::read_to_string(fixture).unwrap();
    // Put callers and callees in different 24-function image slices. Lean
    // may share match auxiliaries within one module, hiding the regression:
    // equal encoders from different slices need a final definitional close.
    let padding = |start: usize| {
        (start..start + 24)
            .map(|i| format!("fn pad{i}(x: Int) -> Int\n    x + {i}\n\n"))
            .collect::<String>()
    };
    let names = (0..48)
        .map(|i| format!("pad{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let source = source
        .replace("    exposes [", &format!("    exposes [{names}, "))
        .replace("fn recognised", &format!("{}fn recognised", padding(0)))
        .replace("fn pressed", &format!("{}fn pressed", padding(24)));
    std::fs::write(root.join("main.av"), source).unwrap();
    let package = compile(&root, "compiled");
    check(&root, &package, 53);
}
