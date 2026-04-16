fn main() {
    // Extract the pinned aver-rt version from our Cargo.toml so the Rust
    // codegen can embed it without reading files at runtime.  This works
    // both from a local checkout and from a crates.io install.
    let manifest = std::fs::read_to_string("Cargo.toml").expect("failed to read Cargo.toml");
    for line in manifest.lines() {
        let line = line.trim();
        if line.starts_with("aver-rt") && line.contains("version = \"") {
            let rest = &line[line.find("version = \"").unwrap() + 11..];
            if let Some(end) = rest.find('"') {
                println!("cargo::rustc-env=AVER_RT_VERSION={}", &rest[..end]);
                return;
            }
        }
    }
    panic!("could not find aver-rt version in Cargo.toml");
}
