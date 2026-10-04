//! A shared build of the checker-owned wall for suites that run Lake directly.
//!
//! Every guard probe stages the wall next to its own package and runs
//! `lake build`. The artifact-independent part of that build is the same in
//! every test, and it is most of the time each one takes. This module builds
//! it once per store from the exact wall sources and copies the resulting
//! `.lake/build` tree into each probe directory before its `lake build`. Lake
//! then compares every restored module's trace with the staged sources and
//! rebuilds whatever does not match, so a seed from other sources costs a
//! rebuild, never a stale module.
//!
//! This is test harness state only. The checker (`aver-cert`) has its own
//! opt-in cache, and `verify` uses neither.
//!
//! An entry is keyed on the wall sources, the toolchain and the seed lakefile,
//! and carries a SHA-256 manifest of every file under its `.lake`. The copy a
//! probe will build against is checked against that manifest. A file changed,
//! added or removed after the entry was built fails the test instead of
//! seeding it: a seed that does not match what this harness built is never
//! used and never silently replaced.
//!
//! `AVER_TEST_WALL_SEED` names the store (default: `aver-test-wall-seed` in
//! the temporary directory); `off` disables seeding, so every probe builds the
//! wall cold. The store is trusted local state, like the checker's caches: the
//! manifest catches corruption and tampering of a single file, not a writer
//! who rebuilds both a different `.olean` and its manifest.
#![allow(dead_code)]

use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::process::Command;

/// The variable naming the store.
pub const STORE_ENV: &str = "AVER_TEST_WALL_SEED";

/// Bumped whenever the entry layout or the seed lakefile shape changes.
const LAYOUT_VERSION: &str = "guard-iso-wall-seed-v1";

/// Copy the wall's pre-built `.lake/build` into `dir`, building the shared
/// entry first if this store does not have it yet. No-op when seeding is off.
pub fn seed(dir: &Path) {
    let Some(store) = store() else {
        return;
    };
    let entry = ensure_entry(&store);
    let destination = dir.join(".lake").join("build");
    let _ = std::fs::remove_dir_all(&destination);
    std::fs::create_dir_all(dir.join(".lake")).unwrap();
    copy_tree(&entry.join(".lake").join("build"), &destination);
    if let Err(reason) = check_copy(&entry, &dir.join(".lake")) {
        panic!(
            "the wall seed {} does not match the build this harness recorded ({reason}); \
             it was altered after it was built. Delete it to rebuild it from source.",
            entry.display()
        );
    }
}

fn store() -> Option<PathBuf> {
    match std::env::var_os(STORE_ENV) {
        None => Some(std::env::temp_dir().join("aver-test-wall-seed")),
        Some(value) => {
            let text = value.to_string_lossy();
            if text.is_empty() || text == "0" || text.eq_ignore_ascii_case("off") {
                None
            } else {
                Some(PathBuf::from(value))
            }
        }
    }
}

/// The files staged for the seed build: every wall source, the toolchain, and
/// a lakefile of the same shape the probes write, rooted at the
/// artifact-independent modules.
fn seed_files() -> Vec<(String, Vec<u8>)> {
    let wall = &aver::codegen::cert::wall::CURRENT;
    let mut files = wall
        .sources
        .iter()
        .map(|source| (source.name.to_string(), source.contents.as_bytes().to_vec()))
        .collect::<Vec<_>>();
    files.push((
        "lean-toolchain".to_string(),
        wall.toolchain.as_bytes().to_vec(),
    ));
    files.push((
        "lakefile.lean".to_string(),
        lakefile(wall.pristine_roots).into_bytes(),
    ));
    files.sort_by(|a, b| a.0.cmp(&b.0));
    files
}

/// The lakefile shape `cert_wall::materialize` and the synthetic-wall probes
/// write. Only the root list differs from theirs, and Lake does not trace it.
pub fn lakefile(roots: &[&str]) -> String {
    let roots = roots
        .iter()
        .map(|root| format!("`{root}"))
        .collect::<Vec<_>>()
        .join(", ");
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{roots}]\n"
    )
}

fn key(files: &[(String, Vec<u8>)]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(LAYOUT_VERSION.as_bytes());
    for (name, bytes) in files {
        hasher.update((name.len() as u64).to_be_bytes());
        hasher.update(name.as_bytes());
        hasher.update((bytes.len() as u64).to_be_bytes());
        hasher.update(bytes);
    }
    format!("{:x}", hasher.finalize())
}

/// The entry for the current wall, built under an exclusive lock so parallel
/// test processes build it once and the others wait for it.
fn ensure_entry(store: &Path) -> PathBuf {
    let files = seed_files();
    let key = key(&files);
    std::fs::create_dir_all(store).unwrap();
    let entry = store.join(&key);
    let lock = std::fs::File::create(store.join(format!("{key}.lock"))).unwrap();
    lock.lock().expect("lock the wall seed store");
    if !entry.join("manifest.sha256").is_file() {
        build_entry(store, &entry, &key, &files);
    }
    drop(lock);
    entry
}

fn build_entry(store: &Path, entry: &Path, key: &str, files: &[(String, Vec<u8>)]) {
    let temp = store.join(format!("tmp-{key}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&temp);
    std::fs::create_dir(&temp).unwrap();
    for (name, bytes) in files {
        std::fs::write(temp.join(name), bytes).unwrap();
    }
    let build = Command::new("lake")
        .current_dir(&temp)
        .arg("build")
        .output()
        .expect("lake builds the wall seed");
    assert!(
        build.status.success(),
        "the wall failed to build for the shared seed:\n{}{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );
    write_manifest(&temp, key);
    // A partially written entry is never visible: the manifest is what marks
    // an entry complete, and the whole directory appears in one rename.
    let _ = std::fs::remove_dir_all(entry);
    std::fs::rename(&temp, entry).unwrap();
}

/// Record the key and the hash of every file the seed copies.
pub fn write_manifest(entry: &Path, key: &str) {
    let mut manifest = format!("key {key}\n");
    for (path, hash) in tree_hashes(&entry.join(".lake").join("build")).unwrap() {
        manifest.push_str(&format!("{hash}  {path}\n"));
    }
    std::fs::write(entry.join("manifest.sha256"), manifest).unwrap();
}

/// Check a copied `.lake` against the entry's manifest: the key line must name
/// the current wall, and the copied `build` tree must hold exactly the
/// recorded files with the recorded hashes.
pub fn check_copy(entry: &Path, lake: &Path) -> Result<(), String> {
    check_copy_for_key(entry, lake, &key(&seed_files()))
}

pub fn check_copy_for_key(entry: &Path, lake: &Path, key: &str) -> Result<(), String> {
    let manifest = std::fs::read_to_string(entry.join("manifest.sha256"))
        .map_err(|error| format!("no manifest: {error}"))?;
    let mut lines = manifest.lines();
    if lines.next() != Some(format!("key {key}").as_str()) {
        return Err("the manifest names another wall".to_string());
    }
    let mut expected = lines
        .map(|line| {
            line.split_once("  ")
                .map(|(hash, path)| (path.to_string(), hash.to_string()))
                .ok_or_else(|| format!("malformed manifest line `{line}`"))
        })
        .collect::<Result<Vec<_>, _>>()?;
    expected.sort();
    let actual = tree_hashes(&lake.join("build"))?;
    if expected != actual {
        let expected: std::collections::BTreeMap<_, _> = expected.into_iter().collect();
        let actual: std::collections::BTreeMap<_, _> = actual.into_iter().collect();
        let differing = expected
            .keys()
            .chain(actual.keys())
            .find(|path| expected.get(*path) != actual.get(*path))
            .cloned()
            .unwrap_or_default();
        return Err(format!("`{differing}` differs"));
    }
    Ok(())
}

fn tree_hashes(root: &Path) -> Result<Vec<(String, String)>, String> {
    fn visit(root: &Path, dir: &Path, out: &mut Vec<(String, String)>) -> Result<(), String> {
        let entries =
            std::fs::read_dir(dir).map_err(|error| format!("{}: {error}", dir.display()))?;
        for entry in entries {
            let entry = entry.map_err(|error| error.to_string())?;
            let path = entry.path();
            let file_type = entry.file_type().map_err(|error| error.to_string())?;
            if file_type.is_dir() {
                visit(root, &path, out)?;
            } else if file_type.is_file() {
                let relative = path
                    .strip_prefix(root)
                    .unwrap()
                    .to_string_lossy()
                    .replace(std::path::MAIN_SEPARATOR, "/");
                let bytes = std::fs::read(&path).map_err(|error| error.to_string())?;
                out.push((relative, format!("{:x}", Sha256::digest(bytes))));
            } else {
                return Err(format!(
                    "{} is neither a file nor a directory",
                    path.display()
                ));
            }
        }
        Ok(())
    }
    let mut out = Vec::new();
    visit(root, root, &mut out)?;
    out.sort();
    Ok(out)
}

fn copy_tree(source: &Path, destination: &Path) {
    std::fs::create_dir_all(destination).unwrap();
    for entry in std::fs::read_dir(source).unwrap() {
        let entry = entry.unwrap();
        let target = destination.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), target).unwrap();
        }
    }
}
