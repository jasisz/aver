//! Cached Rust host for running configured provider packages on the bytecode VM.
//!
//! The stock `aver` process cannot link a Cargo dependency after it starts.
//! Instead of a dynamic Rust ABI or per-call IPC, a project whose `aver.toml`
//! binds providers gets a tiny binary which links the ordinary Aver
//! CLI/library plus the declared provider factories. That binary runs the same
//! VM command with process-local bindings.

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};

use sha2::{Digest, Sha256};

use crate::codegen::rust::composition::{ProviderComposition, ProviderCompositionSource};
use crate::toolchain_source::ToolchainSource;

const HOST_SCHEMA: &str = "aver-provider-vm-host-v4";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ProviderHostBackend {
    Vm,
    Wasip2,
}

/// Build (once) and run the host for `composition`. `project_root` is the
/// module root the manifest was loaded from; the build notice names every
/// provider package relative to it.
pub(crate) fn run_cached_host(
    raw_args: &[OsString],
    composition: &ProviderComposition,
    backend: ProviderHostBackend,
    project_root: &Path,
) -> Result<ExitStatus, String> {
    let source = render_host_source(composition);
    let dependency_lines = render_dependency_lines(composition, backend)?;
    let runtime_identity = host_runtime_identity()?;
    let key = host_key(&source, &dependency_lines, &runtime_identity);
    let binary_name = format!("aver-provider-host-{}", &key[..16]);
    let cache = cache_root()?;
    let project = cache.join("projects").join(&key);
    let target = cache.join("target");
    let cargo_toml = render_cargo_toml(&binary_name, &dependency_lines);
    let binary =
        target
            .join("debug")
            .join(format!("{}{}", binary_name, std::env::consts::EXE_SUFFIX));
    let state_file = project.join("build-input-state");

    write_if_changed(&project.join("Cargo.toml"), &cargo_toml)?;
    write_if_changed(&project.join("src/main.rs"), &source)?;

    let input_state = build_input_state(composition)?;
    let cached_state = std::fs::read_to_string(&state_file).ok();
    if !binary.is_file() || cached_state.as_deref() != Some(&input_state) {
        let action = if binary.is_file() {
            "Rebuilding"
        } else {
            "Building"
        };
        // The consent is the `[providers]` table in the project's own
        // aver.toml; the notice says exactly what that table makes Cargo
        // build, and where each package comes from.
        let packages = composition
            .bindings
            .iter()
            .map(|binding| {
                format!(
                    "{}: {} from {}",
                    binding.capability,
                    binding.package,
                    describe_source(&binding.source, project_root)
                )
            })
            .collect::<Vec<_>>()
            .join(", ");
        eprintln!(
            "{action} provider host for {packages} (cached at {})...",
            project.display()
        );
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| OsString::from("cargo"));
        let status = Command::new(cargo)
            .arg("build")
            .arg("--quiet")
            .current_dir(&project)
            .env("CARGO_TARGET_DIR", &target)
            .status()
            .map_err(|error| format!("failed to start Cargo for provider host: {error}"))?;
        if !status.success() {
            return Err(format!(
                "provider host Cargo build failed with {}",
                display_status(status)
            ));
        }
        write_if_changed(&state_file, &input_state)?;
    }

    if !binary.is_file() {
        return Err(format!(
            "provider host build succeeded but binary '{}' is missing",
            binary.display()
        ));
    }

    Command::new(&binary)
        .args(raw_args.iter().skip(1))
        .status()
        .map_err(|error| {
            format!(
                "failed to start cached provider host '{}': {error}",
                binary.display()
            )
        })
}

/// Where a provider package comes from, for a reader of the project: a path
/// relative to the project root when the package lives nearby, its absolute
/// path otherwise, or the registry version.
pub(crate) fn describe_source(source: &ProviderCompositionSource, project_root: &Path) -> String {
    match source {
        ProviderCompositionSource::Registry { version } => {
            format!("the registry (version {version})")
        }
        ProviderCompositionSource::LocalPath { path } => {
            display_relative(path, project_root).display().to_string()
        }
    }
}

/// `path` spelled relative to `base`, both taken canonical: `.` when they
/// coincide, `..` segments for a sibling or a parent's sibling, and the
/// absolute path once the climb would exceed two levels, since a long `..`
/// chain names nothing a reader can picture.
fn display_relative(path: &Path, base: &Path) -> PathBuf {
    let base = base.canonicalize().unwrap_or_else(|_| base.to_path_buf());
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    let mut base_parts = base.components().peekable();
    let mut path_parts = path.components().peekable();
    while let (Some(left), Some(right)) = (base_parts.peek(), path_parts.peek()) {
        if left != right {
            break;
        }
        base_parts.next();
        path_parts.next();
    }
    let climb = base_parts.count();
    if climb > 2 {
        return path;
    }
    let mut relative = (0..climb).map(|_| "..").collect::<PathBuf>();
    relative.extend(path_parts);
    if relative.as_os_str().is_empty() {
        PathBuf::from(".")
    } else {
        relative
    }
}

/// A cheap invalidation stamp for local path dependencies. The deterministic
/// host key intentionally does not absorb their contents: the project path
/// remains stable while Cargo incrementally rebuilds an edited provider. We
/// skip Cargo entirely only while the relevant source trees are unchanged.
fn build_input_state(composition: &ProviderComposition) -> Result<String, String> {
    let mut roots = Vec::new();
    if let Some(aver_root) = ToolchainSource::current().local_root()
        && aver_root.join("Cargo.toml").is_file()
    {
        let aver_root = aver_root.canonicalize().map_err(|error| {
            format!(
                "cannot resolve local aver-lang path '{}': {error}",
                aver_root.display()
            )
        })?;
        let mut hasher = Sha256::new();
        hash_aver_source_state(&aver_root, &mut hasher)?;
        roots.push((aver_root, Some(format!("{:x}", hasher.finalize()))));
    }
    for binding in &composition.bindings {
        if let ProviderCompositionSource::LocalPath { path } = &binding.source {
            roots.push((path.clone(), None));
        }
    }
    roots.sort_by(|left, right| left.0.cmp(&right.0));
    roots.dedup_by(|left, right| left.0 == right.0);

    let mut hasher = Sha256::new();
    for (root, prefingerprinted) in roots {
        if let Some(state) = prefingerprinted {
            hasher.update(state.as_bytes());
        } else {
            hash_source_tree(&root, &root, &mut hasher)?;
        }
    }
    Ok(format!("{:x}\n", hasher.finalize()))
}

/// Mirror the files published in the aver-lang package plus its local runtime
/// path dependencies. In particular, do not hash examples/tests/recordings:
/// editing the Aver program being run must never invalidate the host.
fn hash_aver_source_state(root: &Path, hasher: &mut Sha256) -> Result<(), String> {
    for relative in ["Cargo.toml", "build.rs", "src", "stdlib", "wit"] {
        hash_source_path(root, &root.join(relative), hasher)?;
    }
    for relative in ["aver-memory", "aver-rt"] {
        let path = root.join(relative);
        if path.exists() {
            hash_source_tree(root, &path, hasher)?;
        }
    }
    Ok(())
}

fn hash_source_path(root: &Path, path: &Path, hasher: &mut Sha256) -> Result<(), String> {
    if path.is_dir() {
        return hash_source_tree(root, path, hasher);
    }
    let metadata = std::fs::metadata(path)
        .map_err(|error| format!("cannot inspect '{}': {error}", path.display()))?;
    hash_file_state(root, path, &metadata, hasher);
    Ok(())
}

fn hash_source_tree(root: &Path, dir: &Path, hasher: &mut Sha256) -> Result<(), String> {
    let mut entries = std::fs::read_dir(dir)
        .map_err(|error| format!("cannot inspect '{}': {error}", dir.display()))?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|error| format!("cannot inspect '{}': {error}", dir.display()))?;
    entries.sort_by_key(std::fs::DirEntry::file_name);
    for entry in entries {
        let path = entry.path();
        let file_type = entry
            .file_type()
            .map_err(|error| format!("cannot inspect '{}': {error}", path.display()))?;
        if file_type.is_dir() {
            let name = entry.file_name();
            if matches!(
                name.to_str(),
                Some(".git" | ".agents" | ".codex" | "prompts" | "target")
            ) {
                continue;
            }
            hash_source_tree(root, &path, hasher)?;
            continue;
        }
        if !file_type.is_file() {
            continue;
        }
        let metadata = entry
            .metadata()
            .map_err(|error| format!("cannot inspect '{}': {error}", path.display()))?;
        hash_file_state(root, &path, &metadata, hasher);
    }
    Ok(())
}

fn hash_file_state(root: &Path, path: &Path, metadata: &std::fs::Metadata, hasher: &mut Sha256) {
    let relative = path.strip_prefix(root).unwrap_or(path);
    let modified = metadata
        .modified()
        .ok()
        .and_then(|time| time.duration_since(std::time::UNIX_EPOCH).ok());
    let state = format!(
        "{}\0{}\0{}\0{}\n",
        relative.to_string_lossy(),
        metadata.len(),
        modified.map(|value| value.as_secs()).unwrap_or(0),
        modified.map(|value| value.subsec_nanos()).unwrap_or(0),
    );
    hasher.update(state.as_bytes());
}

fn render_host_source(composition: &ProviderComposition) -> String {
    let factories = composition
        .bindings
        .iter()
        .map(|binding| format!("        {},", binding.factory_call()))
        .collect::<Vec<_>>()
        .join("\n");
    format!(
        "fn main() {{\n    aver::cli_entry::main_with_provider_bindings(vec![\n{factories}\n    ]);\n}}\n"
    )
}

fn render_dependency_lines(
    composition: &ProviderComposition,
    backend: ProviderHostBackend,
) -> Result<Vec<String>, String> {
    let mut lines = vec![aver_dependency_line(backend)?];
    for binding in &composition.bindings {
        lines.push(binding.cargo_dependency_line());
    }
    Ok(lines)
}

fn aver_dependency_line(backend: ProviderHostBackend) -> Result<String, String> {
    let version = format!("={}", env!("CARGO_PKG_VERSION"));
    let features: &[&str] = match backend {
        ProviderHostBackend::Vm => &[],
        ProviderHostBackend::Wasip2 => &["wasip2"],
    };
    Ok(ToolchainSource::current().aver_lang_dependency(&version, features))
}

fn render_cargo_toml(binary_name: &str, dependencies: &[String]) -> String {
    format!(
        "[package]\nname = {}\nversion = \"0.0.0\"\nedition = \"2024\"\npublish = false\n\n[dependencies]\n{}\n\n[profile.dev]\ndebug = 0\n",
        toml_string(binary_name),
        dependencies.join("\n")
    )
}

fn host_runtime_identity() -> Result<String, String> {
    let rustc = std::env::var_os("RUSTC").unwrap_or_else(|| OsString::from("rustc"));
    let output = Command::new(rustc)
        .arg("-vV")
        .output()
        .map_err(|error| format!("failed to inspect Rust toolchain for provider host: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "failed to inspect Rust toolchain for provider host ({})",
            display_status(output.status)
        ));
    }
    String::from_utf8(output.stdout)
        .map_err(|_| "Rust toolchain identity is not valid UTF-8".to_string())
}

fn host_key(source: &str, dependencies: &[String], runtime_identity: &str) -> String {
    let mut hasher = Sha256::new();
    for part in [
        HOST_SCHEMA,
        env!("CARGO_PKG_VERSION"),
        std::env::consts::OS,
        std::env::consts::ARCH,
        runtime_identity,
        source,
        &dependencies.join("\n"),
    ] {
        hasher.update((part.len() as u64).to_be_bytes());
        hasher.update(part.as_bytes());
    }
    format!("{:x}", hasher.finalize())
}

fn cache_root() -> Result<PathBuf, String> {
    if let Some(path) = std::env::var_os("AVER_PROVIDER_HOST_CACHE") {
        return Ok(PathBuf::from(path));
    }
    if let Some(path) = std::env::var_os("XDG_CACHE_HOME") {
        return Ok(PathBuf::from(path).join("aver/provider-hosts"));
    }
    if let Some(user_dir) = std::env::var_os("HOME") {
        let user_dir = PathBuf::from(user_dir);
        if cfg!(target_os = "macos") {
            return Ok(user_dir.join("Library/Caches/aver/provider-hosts"));
        }
        return Ok(user_dir.join(".cache/aver/provider-hosts"));
    }
    Ok(std::env::temp_dir().join("aver-provider-hosts"))
}

fn write_if_changed(path: &Path, contents: &str) -> Result<(), String> {
    if std::fs::read_to_string(path).ok().as_deref() == Some(contents) {
        return Ok(());
    }
    let parent = path
        .parent()
        .ok_or_else(|| format!("path '{}' has no parent", path.display()))?;
    std::fs::create_dir_all(parent)
        .map_err(|error| format!("cannot create '{}': {error}", parent.display()))?;
    std::fs::write(path, contents)
        .map_err(|error| format!("cannot write '{}': {error}", path.display()))
}

fn toml_string(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch <= '\u{001f}' => {
                use std::fmt::Write;
                write!(out, "\\u{:04X}", ch as u32).expect("write to String");
            }
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn display_status(status: ExitStatus) -> String {
    status
        .code()
        .map(|code| format!("exit code {code}"))
        .unwrap_or_else(|| "termination by signal".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    use crate::capability::CapabilityRegistry;
    use crate::config::ProjectConfig;

    #[test]
    fn host_source_and_key_are_stable_across_manifest_order() {
        let one_manifest = ProjectConfig::parse(
            "[providers]\nschema=1\n\
             [[providers.bindings]]\ncapability='Zed'\ncrate='zed'\npackage='zed-p'\nversion='1'\nfactory='binding'\n\
             [[providers.bindings]]\ncapability='Alpha'\ncrate='alpha'\npackage='alpha-p'\nversion='2'\nfactory='make::binding'\n",
        )
        .unwrap()
        .provider_manifest
        .unwrap();
        let two_manifest = ProjectConfig::parse(
            "[providers]\nschema=1\n\
             [[providers.bindings]]\ncapability='Alpha'\ncrate='alpha'\npackage='alpha-p'\nversion='2'\nfactory='make::binding'\n\
             [[providers.bindings]]\ncapability='Zed'\ncrate='zed'\npackage='zed-p'\nversion='1'\nfactory='binding'\n",
        )
        .unwrap()
        .provider_manifest
        .unwrap();
        let mut registry = CapabilityRegistry::default();
        for capability in ["Alpha", "Zed"] {
            let source = format!(
                "module {capability}\n    kind = capability\n    semantics = pure\n\noperation read() -> Int\n"
            );
            let items = crate::source::parse_source(&source).unwrap();
            let (part, errors) = CapabilityRegistry::from_module(capability, &items);
            assert!(errors.is_empty());
            registry.merge(part);
        }
        let required = BTreeSet::from(["Alpha.read".to_string(), "Zed.read".to_string()]);
        let one =
            crate::codegen::rust::composition::plan(&registry, &required, Some(&one_manifest))
                .unwrap();
        let two =
            crate::codegen::rust::composition::plan(&registry, &required, Some(&two_manifest))
                .unwrap();
        assert_eq!(render_host_source(&one), render_host_source(&two));
        assert_eq!(
            render_dependency_lines(&one, ProviderHostBackend::Vm).unwrap(),
            render_dependency_lines(&two, ProviderHostBackend::Vm).unwrap()
        );
        assert!(
            render_dependency_lines(&one, ProviderHostBackend::Vm).unwrap()[0]
                .starts_with("aver = {")
        );
        assert!(
            render_dependency_lines(&one, ProviderHostBackend::Wasip2).unwrap()[0]
                .contains("features = [\"wasip2\"]")
        );
        assert_ne!(
            host_key(
                &render_host_source(&one),
                &render_dependency_lines(&one, ProviderHostBackend::Vm).unwrap(),
                "rustc-test"
            ),
            host_key(
                &render_host_source(&one),
                &render_dependency_lines(&one, ProviderHostBackend::Wasip2).unwrap(),
                "rustc-test"
            ),
            "VM and wasip2 hosts must never alias one cached binary"
        );
    }

    #[test]
    fn local_source_state_changes_without_changing_the_host_plan() {
        let root = tempfile::tempdir().unwrap();
        std::fs::write(root.path().join("Cargo.toml"), "[package]\nname='p'\n").unwrap();
        let mut before = Sha256::new();
        hash_source_tree(root.path(), root.path(), &mut before).unwrap();
        let before = format!("{:x}", before.finalize());

        std::fs::write(
            root.path().join("Cargo.toml"),
            "[package]\nname='provider'\n",
        )
        .unwrap();
        let mut after = Sha256::new();
        hash_source_tree(root.path(), root.path(), &mut after).unwrap();
        let after = format!("{:x}", after.finalize());
        assert_ne!(before, after);
    }
}
