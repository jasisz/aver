//! Filesystem materialisation that preserves build-tool freshness.
//!
//! Code generators often reproduce byte-identical files. Rewriting those
//! files changes their mtimes and makes downstream tools redo work even though
//! the generated project did not change.

use std::path::Path;

/// Write `contents` only when `path` does not already contain the same bytes.
///
/// Returns `true` when the filesystem changed and `false` for an exact no-op.
pub(crate) fn write_if_changed(path: &Path, contents: &[u8]) -> Result<bool, String> {
    match std::fs::read(path) {
        Ok(existing) if existing == contents => return Ok(false),
        Ok(_) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => {
            return Err(format!("cannot read '{}': {error}", path.display()));
        }
    }

    let parent = path
        .parent()
        .ok_or_else(|| format!("path '{}' has no parent", path.display()))?;
    std::fs::create_dir_all(parent)
        .map_err(|error| format!("cannot create '{}': {error}", parent.display()))?;
    std::fs::write(path, contents)
        .map_err(|error| format!("cannot write '{}': {error}", path.display()))?;
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::write_if_changed;

    #[test]
    fn reports_exact_no_ops_and_real_changes() {
        let dir = tempfile::tempdir().expect("temporary materialisation directory");
        let path = dir.path().join("nested/generated.rs");

        assert!(write_if_changed(&path, b"first\n").expect("initial write"));
        assert!(!write_if_changed(&path, b"first\n").expect("identical write"));
        assert!(write_if_changed(&path, b"second\n").expect("changed write"));
        assert_eq!(std::fs::read(&path).expect("read result"), b"second\n");
    }
}
