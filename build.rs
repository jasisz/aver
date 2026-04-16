fn main() {
    let manifest = std::fs::read_to_string("Cargo.toml").expect("failed to read Cargo.toml");

    // Inline: aver-rt = { ..., version = "=0.4.1", ... }
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("aver-rt")
            && let Some(v) = extract_version(trimmed)
        {
            emit(v);
            return;
        }
    }

    // Expanded: [dependencies.aver-rt] section
    let mut in_section = false;
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed == "[dependencies.aver-rt]" {
            in_section = true;
        } else if in_section && trimmed.starts_with('[') {
            break;
        } else if in_section
            && trimmed.starts_with("version")
            && let Some(v) = extract_version(trimmed)
        {
            emit(v);
            return;
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

fn emit(version: &str) {
    println!("cargo::rustc-env=AVER_RT_VERSION={version}");
}
