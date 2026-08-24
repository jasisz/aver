use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    // The generated-project runtime pin is read from the workspace manifest.
    // Once a build script declares explicit rerun inputs below, Cargo no
    // longer watches every package file implicitly, so keep this dependency
    // visible across the post-release `*-dev` version bump on main.
    println!("cargo::rerun-if-changed=Cargo.toml");
    let manifest = std::fs::read_to_string("Cargo.toml").expect("failed to read Cargo.toml");
    let runtime_version = find_runtime_version(&manifest)
        .unwrap_or_else(|| panic!("could not find aver-rt version in Cargo.toml"));
    println!("cargo::rustc-env=AVER_RT_VERSION={runtime_version}");

    for key in [
        "AVER_BUILD_SOURCE_KIND",
        "AVER_BUILD_SOURCE_PATH",
        "AVER_BUILD_GIT_URL",
        "AVER_BUILD_GIT_REV",
        "CARGO_HOME",
    ] {
        println!("cargo::rerun-if-env-changed={key}");
    }
    emit_build_source(detect_build_source());
}

fn find_runtime_version(manifest: &str) -> Option<&str> {
    // Inline: aver-rt = { ..., version = "=0.4.1", ... }
    for line in manifest.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("aver-rt")
            && let Some(version) = extract_version(trimmed)
        {
            return Some(version);
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
            && let Some(version) = extract_version(trimmed)
        {
            return Some(version);
        }
    }
    None
}

fn extract_version(line: &str) -> Option<&str> {
    let start = line.find("version = \"")? + 11;
    let rest = &line[start..];
    let end = rest.find('"')?;
    Some(&rest[..end])
}

#[derive(Debug)]
enum BuildSource {
    Path(PathBuf),
    Git { url: String, rev: String },
    Registry,
}

fn detect_build_source() -> BuildSource {
    if let Ok(kind) = std::env::var("AVER_BUILD_SOURCE_KIND") {
        return match kind.trim() {
            "path" => BuildSource::Path(
                std::env::var_os("AVER_BUILD_SOURCE_PATH")
                    .map(PathBuf::from)
                    .unwrap_or_else(manifest_dir),
            ),
            "git" => BuildSource::Git {
                url: required_env("AVER_BUILD_GIT_URL", "git source URL"),
                rev: required_env("AVER_BUILD_GIT_REV", "git source revision"),
            },
            "registry" => BuildSource::Registry,
            other => panic!("AVER_BUILD_SOURCE_KIND must be path, git, or registry; got '{other}'"),
        };
    }

    let manifest = manifest_dir();
    if let Some(cargo_home) = cargo_home() {
        if path_is_within(&manifest, &cargo_home.join("git/checkouts")) {
            return BuildSource::Git {
                url: cargo_git_source_url(&manifest, &cargo_home).unwrap_or_else(|| {
                    panic!(
                        "cargo git checkout '{}' does not expose its source URL; set \
                             AVER_BUILD_SOURCE_KIND=git, AVER_BUILD_GIT_URL, and \
                             AVER_BUILD_GIT_REV explicitly",
                        manifest.display()
                    )
                }),
                rev: git_value(&manifest, &["rev-parse", "HEAD"]).unwrap_or_else(|| {
                    panic!(
                        "cargo git checkout '{}' has no HEAD revision; set \
                         AVER_BUILD_SOURCE_KIND=git, AVER_BUILD_GIT_URL, and \
                         AVER_BUILD_GIT_REV explicitly",
                        manifest.display()
                    )
                }),
            };
        }
        if path_is_within(&manifest, &cargo_home.join("registry/src")) {
            return BuildSource::Registry;
        }
    }

    // Cargo includes this file in registry package sources. It also makes a
    // packaged tarball built outside CARGO_HOME retain registry semantics.
    if manifest.join(".cargo_vcs_info.json").is_file() {
        return BuildSource::Registry;
    }

    BuildSource::Path(manifest)
}

fn manifest_dir() -> PathBuf {
    std::env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."))
}

fn cargo_home() -> Option<PathBuf> {
    std::env::var_os("CARGO_HOME")
        .map(PathBuf::from)
        .or_else(|| {
            std::env::var_os("HOME")
                .map(PathBuf::from)
                .map(|home| home.join(".cargo"))
        })
        .or_else(|| {
            std::env::var_os("USERPROFILE")
                .map(PathBuf::from)
                .map(|home| home.join(".cargo"))
        })
}

fn path_is_within(path: &Path, root: &Path) -> bool {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    path.starts_with(root)
}

/// Cargo's working checkout points `remote.origin.url` at its local bare
/// cache, not at the URL passed to `cargo install --git`. The bare cache has
/// no remote config either, but its FETCH_HEAD records the original fetch URL.
/// Recover that URL so the installed binary does not emit a machine-local
/// `~/.cargo/git/db/...` dependency.
fn cargo_git_source_url(manifest: &Path, cargo_home: &Path) -> Option<String> {
    let checkouts = cargo_home.join("git/checkouts");
    let cache_key = manifest
        .strip_prefix(&checkouts)
        .ok()?
        .components()
        .next()?
        .as_os_str();
    let fetch_head =
        std::fs::read_to_string(cargo_home.join("git/db").join(cache_key).join("FETCH_HEAD"))
            .ok()?;
    fetch_head.lines().find_map(|line| {
        line.rsplit_once(" of ")
            .map(|(_, url)| url.trim())
            .filter(|url| !url.is_empty())
            .map(str::to_string)
    })
}

fn git_value(cwd: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .args(args)
        .current_dir(cwd)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let value = String::from_utf8(output.stdout).ok()?;
    let value = value.trim();
    (!value.is_empty()).then(|| value.to_string())
}

fn required_env(key: &str, description: &str) -> String {
    std::env::var(key)
        .ok()
        .map(|value| value.trim().to_string())
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| panic!("{key} is required for {description}"))
}

fn emit_build_source(source: BuildSource) {
    let (kind, path, url, rev) = match source {
        BuildSource::Path(path) => (
            "path",
            path.to_string_lossy().replace('\\', "/"),
            String::new(),
            String::new(),
        ),
        BuildSource::Git { url, rev } => ("git", String::new(), url, rev),
        BuildSource::Registry => ("registry", String::new(), String::new(), String::new()),
    };
    println!("cargo::rustc-env=AVER_BUILD_SOURCE_KIND={kind}");
    println!("cargo::rustc-env=AVER_BUILD_SOURCE_PATH={path}");
    println!("cargo::rustc-env=AVER_BUILD_GIT_URL={url}");
    println!("cargo::rustc-env=AVER_BUILD_GIT_REV={rev}");
}
