//! Opt-in cache for the artifact-independent Lean acceptance wall.
//!
//! The cache is only a build hint. Every path still stages each source, runs
//! `lake build`, and authors a fresh witness; strict `verify` additionally
//! finishes with `leanchecker --fresh`. Any cache failure removes the copied
//! build tree so the verifier can retry from freshly staged sources. A cache
//! build that times out or cannot run is reported as a warning and degrades
//! to a plain cache miss: the mandatory proof build that follows has its own
//! wall-clock limit, so the verify/check flow stays bounded and fail-closed.
//!
//! The configured directory is trusted local state. Its integrity manifest
//! detects accidental corruption, not an active writer able to replace both
//! `.olean` outputs and Lake traces. With no environment variable, the strict
//! path uses no prelude cache.

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

use crate::lean_process::{LeanRunner, LeanStepError};
use crate::wall::Wall;

const CACHE_ENV: &str = "AVER_CERT_PRELUDE_CACHE";
const CACHE_LAYOUT_VERSION: &str = "v2-hermetic-env";

pub(crate) struct PristineWallCache {
    entry: Option<PathBuf>,
    seeded: bool,
}

impl PristineWallCache {
    pub(crate) fn prepare(build_dir: &Path, wall: &Wall, lean: &LeanRunner) -> Self {
        let Some(store) = cache_store() else {
            return Self::disabled();
        };
        let files = static_wall_files(wall);
        let key = static_wall_key(&files);
        let entry = store.join(&key);

        let seeded = match try_reuse(&entry, build_dir, &key) {
            Ok(()) => true,
            Err(()) => {
                let _ = std::fs::remove_dir_all(build_dir.join(".lake"));
                let _ = std::fs::remove_dir_all(&entry);
                populate(&store, &entry, &key, &files, lean).is_ok()
                    && try_reuse(&entry, build_dir, &key).is_ok()
            }
        };
        if !seeded {
            let _ = std::fs::remove_dir_all(build_dir.join(".lake"));
        }
        Self {
            entry: Some(entry),
            seeded,
        }
    }

    pub(crate) fn disabled() -> Self {
        Self {
            entry: None,
            seeded: false,
        }
    }

    pub(crate) fn was_seeded(&self) -> bool {
        self.seeded
    }

    /// Remove only the verifier's private copy before a clean retry. Keep the
    /// shared entry until that retry succeeds and proves the seed was at fault.
    pub(crate) fn clear_build(&self, build_dir: &Path) {
        let _ = std::fs::remove_dir_all(build_dir.join(".lake"));
    }

    /// Evict a seed only after the same sources build successfully without it.
    pub(crate) fn evict(&mut self) {
        if self.seeded
            && let Some(entry) = &self.entry
        {
            let _ = std::fs::remove_dir_all(entry);
        }
        self.seeded = false;
    }
}

fn cache_store() -> Option<PathBuf> {
    let value = std::env::var_os(CACHE_ENV)?;
    let text = value.to_string_lossy();
    if text.is_empty()
        || text == "0"
        || text.eq_ignore_ascii_case("off")
        || text.eq_ignore_ascii_case("false")
    {
        None
    } else {
        Some(PathBuf::from(value))
    }
}

/// Exact artifact-independent inputs. The lakefile limits the build to roots
/// whose complete import graph is independent of certificate DATA.
fn static_wall_files(wall: &Wall) -> Vec<(String, Vec<u8>)> {
    let mut files = wall
        .sources
        .iter()
        .map(|source| (source.name.to_string(), source.contents.as_bytes().to_vec()))
        .collect::<Vec<_>>();
    files.push((
        "lakefile.lean".to_string(),
        checker_lakefile(wall.pristine_roots).into_bytes(),
    ));
    files.push((
        "lean-toolchain".to_string(),
        wall.toolchain.as_bytes().to_vec(),
    ));
    files.sort_by(|a, b| a.0.cmp(&b.0));
    files
}

fn checker_lakefile(roots: &[&str]) -> String {
    let roots = roots
        .iter()
        .map(|root| format!("`{root}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n@[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{roots}]\n"
    )
}

/// Hash the filename-sorted, length-framed source sequence. The generated
/// lakefile binds the pristine root set, and `lean-toolchain` binds Lean.
fn static_wall_key(files: &[(String, Vec<u8>)]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(CACHE_LAYOUT_VERSION.as_bytes());
    for (name, bytes) in files {
        hasher.update((name.len() as u64).to_be_bytes());
        hasher.update(name.as_bytes());
        hasher.update((bytes.len() as u64).to_be_bytes());
        hasher.update(bytes);
    }
    format!("{:x}", hasher.finalize())
}

fn populate(
    store: &Path,
    entry: &Path,
    key: &str,
    files: &[(String, Vec<u8>)],
    lean: &LeanRunner,
) -> Result<(), ()> {
    std::fs::create_dir_all(store).map_err(|_| ())?;
    let temp = store.join(format!("tmp-{}-{}", std::process::id(), unique_nanos()));
    std::fs::create_dir(&temp).map_err(|_| ())?;

    let result = (|| {
        for (name, bytes) in files {
            std::fs::write(temp.join(name), bytes).map_err(|_| ())?;
        }
        // A cache build that times out or cannot run must NOT fail
        // verification: the cache is only a build hint, and skipping it is
        // exactly a cache miss. Soundness is unaffected because the
        // mandatory certificate proof build that follows re-does this work
        // under its own wall-clock limit and fails closed on its own. But
        // the fallback must be loud, not silent: name the step and say what
        // happens next.
        let built = lean
            .run_lake(&temp, "proof library cache build", &["build"])
            .map_err(|error| match error {
                LeanStepError::Timeout { limit, .. } => eprintln!(
                    "warning: the proof library cache build exceeded its {}-second limit; \
                     continuing without the cache",
                    limit.as_secs()
                ),
                LeanStepError::Failed(reason) => eprintln!(
                    "warning: the proof library cache build could not run; \
                     continuing without the cache: {reason}"
                ),
            })?;
        if !built.status.success() || !temp.join(".lake").is_dir() {
            return Err(());
        }
        write_integrity(&temp, key)?;
        match std::fs::rename(&temp, entry) {
            Ok(()) => Ok(()),
            Err(_) if entry.join(".lake").is_dir() => Ok(()),
            Err(_) => Err(()),
        }
    })();
    let _ = std::fs::remove_dir_all(temp);
    result
}

fn try_reuse(entry: &Path, build_dir: &Path, key: &str) -> Result<(), ()> {
    let cached_lake = entry.join(".lake");
    if !cached_lake.is_dir() || verify_integrity(entry, &cached_lake, key).is_err() {
        let _ = std::fs::remove_dir_all(entry);
        return Err(());
    }

    let destination = build_dir.join(".lake");
    let _ = std::fs::remove_dir_all(&destination);
    if copy_tree(&cached_lake, &destination).is_err()
        || verify_integrity(entry, &destination, key).is_err()
    {
        let _ = std::fs::remove_dir_all(destination);
        let _ = std::fs::remove_dir_all(entry);
        return Err(());
    }
    Ok(())
}

fn copy_tree(source: &Path, destination: &Path) -> Result<(), ()> {
    std::fs::create_dir(destination).map_err(|_| ())?;
    for entry in std::fs::read_dir(source).map_err(|_| ())? {
        let entry = entry.map_err(|_| ())?;
        let destination_entry = destination.join(entry.file_name());
        let file_type = entry.file_type().map_err(|_| ())?;
        if file_type.is_dir() {
            copy_tree(&entry.path(), &destination_entry)?;
        } else if file_type.is_file() {
            std::fs::copy(entry.path(), destination_entry).map_err(|_| ())?;
        } else {
            return Err(());
        }
    }
    Ok(())
}

fn write_integrity(entry: &Path, key: &str) -> Result<(), ()> {
    let mut manifest = format!("key {key}\n");
    for (path, hash) in lake_tree_hashes(&entry.join(".lake"))? {
        manifest.push_str(&hash);
        manifest.push_str("  ");
        manifest.push_str(&path);
        manifest.push('\n');
    }
    std::fs::write(entry.join("manifest.sha256"), manifest).map_err(|_| ())
}

fn verify_integrity(entry: &Path, lake: &Path, key: &str) -> Result<(), ()> {
    let manifest = std::fs::read_to_string(entry.join("manifest.sha256")).map_err(|_| ())?;
    let mut lines = manifest.lines();
    if lines.next() != Some(format!("key {key}").as_str()) {
        return Err(());
    }

    let mut expected = Vec::new();
    for line in lines {
        let (hash, path) = line.split_once("  ").ok_or(())?;
        if hash.len() != 64 || !hash.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(());
        }
        let relative = Path::new(path);
        if relative.is_absolute()
            || relative
                .components()
                .any(|component| !matches!(component, std::path::Component::Normal(_)))
        {
            return Err(());
        }
        expected.push((path.to_string(), hash.to_ascii_lowercase()));
    }
    expected.sort();
    if expected != lake_tree_hashes(lake)? {
        return Err(());
    }
    Ok(())
}

fn lake_tree_hashes(root: &Path) -> Result<Vec<(String, String)>, ()> {
    fn visit(root: &Path, dir: &Path, hashes: &mut Vec<(String, String)>) -> Result<(), ()> {
        for entry in std::fs::read_dir(dir).map_err(|_| ())? {
            let entry = entry.map_err(|_| ())?;
            let file_type = entry.file_type().map_err(|_| ())?;
            if file_type.is_dir() {
                visit(root, &entry.path(), hashes)?;
            } else if file_type.is_file() {
                let path = entry.path();
                let relative = path.strip_prefix(root).map_err(|_| ())?;
                let relative = relative
                    .to_str()
                    .ok_or(())?
                    .replace(std::path::MAIN_SEPARATOR, "/");
                if relative.contains(['\n', '\r']) {
                    return Err(());
                }
                let bytes = std::fs::read(path).map_err(|_| ())?;
                hashes.push((relative, format!("{:x}", Sha256::digest(bytes))));
            } else {
                return Err(());
            }
        }
        Ok(())
    }

    let mut hashes = Vec::new();
    visit(root, root, &mut hashes)?;
    hashes.sort();
    Ok(hashes)
}

fn unique_nanos() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wall::Source;

    const BASE_SOURCES: [Source; 1] = [Source {
        name: "Core.lean",
        contents: "def core := 1\n",
    }];
    const CHANGED_SOURCES: [Source; 1] = [Source {
        name: "Core.lean",
        contents: "def core := 2\n",
    }];
    const BASE_ROOTS: [&str; 1] = ["Core"];
    const CHANGED_ROOTS: [&str; 1] = ["Other"];

    fn test_wall(
        sources: &'static [Source],
        roots: &'static [&'static str],
        toolchain: &'static str,
    ) -> Wall {
        Wall {
            sources,
            pristine_roots: roots,
            toolchain,
        }
    }

    fn temp_dir(label: &str) -> PathBuf {
        let path = std::env::temp_dir().join(format!(
            "aver-cert-prelude-cache-{label}-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        std::fs::create_dir(&path).unwrap();
        path
    }

    #[test]
    fn key_binds_wall_sources_roots_and_toolchain() {
        let baseline = static_wall_key(&static_wall_files(&test_wall(
            &BASE_SOURCES,
            &BASE_ROOTS,
            "leanprover/lean4:v4.32.0\n",
        )));
        let changed_source = static_wall_key(&static_wall_files(&test_wall(
            &CHANGED_SOURCES,
            &BASE_ROOTS,
            "leanprover/lean4:v4.32.0\n",
        )));
        let changed_root = static_wall_key(&static_wall_files(&test_wall(
            &BASE_SOURCES,
            &CHANGED_ROOTS,
            "leanprover/lean4:v4.32.0\n",
        )));
        let changed_toolchain = static_wall_key(&static_wall_files(&test_wall(
            &BASE_SOURCES,
            &BASE_ROOTS,
            "leanprover/lean4:v4.33.0\n",
        )));

        assert_ne!(baseline, changed_source);
        assert_ne!(baseline, changed_root);
        assert_ne!(baseline, changed_toolchain);
    }

    #[test]
    fn current_pristine_roots_are_unique_embedded_sources() {
        let mut seen = std::collections::BTreeSet::new();
        for root in crate::wall::CURRENT.pristine_roots {
            assert!(seen.insert(*root), "duplicate pristine root: {root}");
            assert!(
                crate::wall::CURRENT
                    .sources
                    .iter()
                    .any(|source| { source.name.strip_suffix(".lean") == Some(*root) }),
                "pristine root has no embedded source: {root}"
            );
        }
    }

    #[test]
    fn integrity_rejects_changed_added_and_missing_files() {
        let entry = temp_dir("integrity");
        let lake = entry.join(".lake");
        std::fs::create_dir(&lake).unwrap();
        let artifact = lake.join("Artifact.olean");
        std::fs::write(&artifact, b"good").unwrap();
        write_integrity(&entry, "wall-key").unwrap();
        assert!(verify_integrity(&entry, &lake, "wall-key").is_ok());

        std::fs::write(&artifact, b"changed").unwrap();
        assert!(verify_integrity(&entry, &lake, "wall-key").is_err());
        std::fs::write(&artifact, b"good").unwrap();

        let extra = lake.join("Extra.olean");
        std::fs::write(&extra, b"extra").unwrap();
        assert!(verify_integrity(&entry, &lake, "wall-key").is_err());
        std::fs::remove_file(extra).unwrap();

        std::fs::remove_file(artifact).unwrap();
        assert!(verify_integrity(&entry, &lake, "wall-key").is_err());
        let _ = std::fs::remove_dir_all(entry);
    }

    #[test]
    fn corrupted_entry_is_removed_without_seeding_the_build() {
        let root = temp_dir("corrupt-reuse");
        let entry = root.join("entry");
        let lake = entry.join(".lake");
        let build = root.join("build");
        std::fs::create_dir_all(&lake).unwrap();
        std::fs::create_dir(&build).unwrap();
        std::fs::write(lake.join("Core.olean"), b"good").unwrap();
        write_integrity(&entry, "wall-key").unwrap();
        std::fs::write(lake.join("Core.olean"), b"corrupt").unwrap();

        assert!(try_reuse(&entry, &build, "wall-key").is_err());
        assert!(!entry.exists());
        assert!(!build.join(".lake").exists());
        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn valid_entry_is_copied_and_invalidation_returns_to_a_clean_build() {
        let root = temp_dir("valid-reuse");
        let entry = root.join("entry");
        let lake = entry.join(".lake");
        let build = root.join("build");
        std::fs::create_dir_all(lake.join("nested")).unwrap();
        std::fs::create_dir(&build).unwrap();
        std::fs::write(lake.join("nested/Core.olean"), b"good").unwrap();
        write_integrity(&entry, "wall-key").unwrap();

        assert!(try_reuse(&entry, &build, "wall-key").is_ok());
        assert_eq!(
            std::fs::read(build.join(".lake/nested/Core.olean")).unwrap(),
            b"good"
        );

        let mut cache = PristineWallCache {
            entry: Some(entry.clone()),
            seeded: true,
        };
        cache.clear_build(&build);
        assert!(entry.exists());
        assert!(!build.join(".lake").exists());
        assert!(cache.was_seeded());

        cache.evict();
        assert!(!entry.exists());
        assert!(!cache.was_seeded());
        let _ = std::fs::remove_dir_all(root);
    }
}
