fn main() {
    // Extract the pinned aver-rt version from our Cargo.toml so the Rust
    // codegen can embed it without reading files at runtime.  This works
    // both from a local checkout (inline table) and crates.io install
    // (expanded [dependencies.aver-rt] section).
    let manifest = std::fs::read_to_string("Cargo.toml").expect("failed to read Cargo.toml");

    // Try inline format: aver-rt = { ..., version = "=0.4.1", ... }
    for line in manifest.lines() {
        let line = line.trim();
        if line.starts_with("aver-rt") && line.contains("version = \"") {
            if let Some(version) = extract_version(line) {
                println!("cargo::rustc-env=AVER_RT_VERSION={version}");
                return;
            }
        }
    }

    // Try expanded format: [dependencies.aver-rt] followed by version = "..."
    let mut in_aver_rt_section = false;
    for line in manifest.lines() {
        let line = line.trim();
        if line == "[dependencies.aver-rt]" {
            in_aver_rt_section = true;
            continue;
        }
        if in_aver_rt_section {
            if line.starts_with('[') {
                break;
            }
            if line.starts_with("version") {
                if let Some(version) = extract_version(line) {
                    println!("cargo::rustc-env=AVER_RT_VERSION={version}");
                    return;
                }
            }
        }
    }

    panic!("could not find aver-rt version in Cargo.toml");
}

fn extract_version(line: &str) -> Option<&str> {
    let start = line.find("version = \"")? + 11;
    let rest = &line[start..];
    let end = rest.find('"')?;
    Some(&rest[..end])
}
