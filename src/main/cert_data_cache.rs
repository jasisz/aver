//! Content-addressed Lake build-output cache for artifact-specific cert DATA.
//!
//! Entries contain only `.lake` plus an integrity manifest. Source inputs are
//! always staged afresh by `cert_cmd`; `lake build` still runs on every verify,
//! and the checker witness is never cached.

use std::path::{Path, PathBuf};

use sha2::{Digest, Sha256};

const CACHE_ENV: &str = "AVER_CERT_DATA_CACHE";
const CACHE_LAYOUT_VERSION: &str = "v1";

pub(crate) struct KeyMaterial<'a> {
    pub schema_version: u64,
    pub pinned_sha256: &'a [(&'a str, &'a str)],
    pub toolchain_version: &'a str,
}

/// A prepared cache lookup. Cache I/O is deliberately best-effort: failure to
/// read, validate, or publish an entry leaves verification on the fresh build.
pub(crate) struct ArtifactBuildCache {
    entry: Option<PathBuf>,
    hit: bool,
}

impl ArtifactBuildCache {
    pub(crate) fn prepare(build_dir: &Path, material: &KeyMaterial<'_>) -> Self {
        let Some(store) = cache_store() else {
            return Self {
                entry: None,
                hit: false,
            };
        };
        let Ok(key) = artifact_cache_key(build_dir, material) else {
            return Self {
                entry: None,
                hit: false,
            };
        };
        let entry = store.join(CACHE_LAYOUT_VERSION).join(key);
        let hit = try_reuse(&entry, build_dir).is_ok();
        Self {
            entry: Some(entry),
            hit,
        }
    }

    pub(crate) fn was_hit(&self) -> bool {
        self.hit
    }

    /// Remove an entry that made `lake build` fail so the caller can retry from
    /// freshly staged sources (optionally seeded only by the pristine prelude).
    pub(crate) fn invalidate(&mut self, build_dir: &Path) {
        if let Some(entry) = &self.entry {
            let _ = std::fs::remove_dir_all(entry);
        }
        let _ = std::fs::remove_dir_all(build_dir.join(".lake"));
        self.hit = false;
    }

    pub(crate) fn publish(&self, build_dir: &Path) {
        if self.hit {
            return;
        }
        let Some(entry) = &self.entry else {
            return;
        };
        let _ = try_publish(entry, build_dir);
    }
}

/// `AVER_CERT_DATA_CACHE=0|off|false` opts out. Any other explicit value is a
/// cache directory (useful for hermetic tests); otherwise use the OS user cache
/// location. Failure to identify a user cache directory disables the hint.
fn cache_store() -> Option<PathBuf> {
    if let Some(value) = std::env::var_os(CACHE_ENV) {
        let text = value.to_string_lossy();
        if text.is_empty()
            || text == "0"
            || text.eq_ignore_ascii_case("off")
            || text.eq_ignore_ascii_case("false")
        {
            return None;
        }
        return Some(PathBuf::from(value));
    }

    if let Some(xdg) = std::env::var_os("XDG_CACHE_HOME") {
        return Some(PathBuf::from(xdg).join("aver").join("cert-data"));
    }
    #[cfg(target_os = "macos")]
    if let Some(home) = std::env::var_os("HOME") {
        return Some(
            PathBuf::from(home)
                .join("Library")
                .join("Caches")
                .join("aver")
                .join("cert-data"),
        );
    }
    #[cfg(target_os = "windows")]
    if let Some(local) = std::env::var_os("LOCALAPPDATA") {
        return Some(PathBuf::from(local).join("aver").join("cert-data"));
    }
    #[cfg(not(any(target_os = "macos", target_os = "windows")))]
    if let Some(home) = std::env::var_os("HOME") {
        return Some(
            PathBuf::from(home)
                .join(".cache")
                .join("aver")
                .join("cert-data"),
        );
    }
    None
}

/// Hash all inputs that can affect artifact DATA build products. The explicit
/// manifest wall is included even though the exact freshly staged source tree
/// is also hashed: schema version, artifact sha, every audited-file sha, and
/// toolchain version are visible, independently framed invalidators. The source
/// tree then covers ArtifactBytes plus every certificate/model DATA file, so a
/// DATA edit that leaves the JSON pins untouched still cannot reuse an entry.
fn artifact_cache_key(build_dir: &Path, material: &KeyMaterial<'_>) -> Result<String, ()> {
    let mut hasher = Sha256::new();
    hash_part(&mut hasher, b"layout", CACHE_LAYOUT_VERSION.as_bytes());
    hash_part(
        &mut hasher,
        b"schema_version",
        material.schema_version.to_string().as_bytes(),
    );

    let mut pins = material.pinned_sha256.to_vec();
    pins.sort_by_key(|(name, _)| *name);
    for (name, value) in pins {
        hash_part(&mut hasher, name.as_bytes(), value.as_bytes());
    }
    hash_part(
        &mut hasher,
        b"toolchain_version",
        material.toolchain_version.as_bytes(),
    );

    for (name, bytes) in staged_source_files(build_dir)? {
        hash_part(&mut hasher, name.as_bytes(), &bytes);
    }
    Ok(format!("{:x}", hasher.finalize()))
}

fn hash_part(hasher: &mut Sha256, name: &[u8], bytes: &[u8]) {
    hasher.update((name.len() as u64).to_be_bytes());
    hasher.update(name);
    hasher.update((bytes.len() as u64).to_be_bytes());
    hasher.update(bytes);
}

fn staged_source_files(build_dir: &Path) -> Result<Vec<(String, Vec<u8>)>, ()> {
    let mut files = Vec::new();
    for entry in std::fs::read_dir(build_dir).map_err(|_| ())? {
        let entry = entry.map_err(|_| ())?;
        if !entry.file_type().map_err(|_| ())?.is_file() {
            continue;
        }
        let name = entry.file_name().to_str().ok_or(())?.to_string();
        if name == "CheckerWitness.lean" {
            continue;
        }
        files.push((name, std::fs::read(entry.path()).map_err(|_| ())?));
    }
    files.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(files)
}

fn try_reuse(entry: &Path, build_dir: &Path) -> Result<(), ()> {
    let cached_lake = entry.join(".lake");
    if !cached_lake.is_dir() {
        return Err(());
    }
    if verify_integrity(entry, &cached_lake).is_err() {
        let _ = std::fs::remove_dir_all(entry);
        return Err(());
    }

    let destination = build_dir.join(".lake");
    let _ = std::fs::remove_dir_all(&destination);
    if copy_tree(&cached_lake, &destination).is_err()
        || verify_integrity(entry, &destination).is_err()
    {
        let _ = std::fs::remove_dir_all(destination);
        let _ = std::fs::remove_dir_all(entry);
        return Err(());
    }
    Ok(())
}

fn try_publish(entry: &Path, build_dir: &Path) -> Result<(), ()> {
    let lake = build_dir.join(".lake");
    if !lake.is_dir() {
        return Err(());
    }
    let parent = entry.parent().ok_or(())?;
    std::fs::create_dir_all(parent).map_err(|_| ())?;
    let temp = parent.join(format!("tmp-{}-{}", std::process::id(), unique_nanos()));
    std::fs::create_dir(&temp).map_err(|_| ())?;
    let result = (|| {
        copy_tree(&lake, &temp.join(".lake"))?;
        write_integrity(&temp)?;
        match std::fs::rename(&temp, entry) {
            Ok(()) => Ok(()),
            Err(_) if entry.join(".lake").is_dir() => Ok(()),
            Err(_) => Err(()),
        }
    })();
    let _ = std::fs::remove_dir_all(temp);
    result
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

fn write_integrity(entry: &Path) -> Result<(), ()> {
    let mut manifest = String::new();
    for (path, hash) in lake_tree_hashes(&entry.join(".lake"))? {
        manifest.push_str(&hash);
        manifest.push_str("  ");
        manifest.push_str(&path);
        manifest.push('\n');
    }
    std::fs::write(entry.join("manifest.sha256"), manifest).map_err(|_| ())
}

fn verify_integrity(entry: &Path, lake: &Path) -> Result<(), ()> {
    let manifest = std::fs::read_to_string(entry.join("manifest.sha256")).map_err(|_| ())?;
    let mut expected = Vec::new();
    for line in manifest.lines() {
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

    #[test]
    fn cache_key_invalidates_every_explicit_input_and_staged_source() {
        let dir = std::env::temp_dir().join(format!(
            "aver-cert-data-cache-key-{}-{}",
            std::process::id(),
            unique_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("Artifact.lean"), "def artifact := 1\n").unwrap();
        let pins = [("wasm_sha256", "aa"), ("schema_sha256", "bb")];
        let material = KeyMaterial {
            schema_version: 50,
            pinned_sha256: &pins,
            toolchain_version: "leanprover/lean4:v4.31.0",
        };
        let baseline = artifact_cache_key(&dir, &material).unwrap();

        let changed_schema = KeyMaterial {
            schema_version: 51,
            ..material
        };
        assert_ne!(baseline, artifact_cache_key(&dir, &changed_schema).unwrap());

        for index in 0..pins.len() {
            let mut changed = pins;
            changed[index].1 = "changed";
            assert_ne!(
                baseline,
                artifact_cache_key(
                    &dir,
                    &KeyMaterial {
                        pinned_sha256: &changed,
                        ..material
                    }
                )
                .unwrap()
            );
        }

        let changed_toolchain = KeyMaterial {
            toolchain_version: "leanprover/lean4:v4.32.0",
            ..material
        };
        assert_ne!(
            baseline,
            artifact_cache_key(&dir, &changed_toolchain).unwrap()
        );

        std::fs::write(dir.join("Artifact.lean"), "def artifact := 2\n").unwrap();
        assert_ne!(baseline, artifact_cache_key(&dir, &material).unwrap());
        let _ = std::fs::remove_dir_all(dir);
    }
}
