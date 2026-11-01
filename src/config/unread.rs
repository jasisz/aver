//! An `aver.toml` that no command reads.
//!
//! Every command reads `aver.toml` from the module root (`--module-root`, or
//! the working directory when none is given) and nowhere else: there is no
//! parent-directory fallback. A manifest left one level up, next to a
//! `--module-root src`, is therefore ignored, and with it settings that
//! change no output a user would notice, such as `[rust] allocator`. This
//! module finds such a file so the command can say it is not read.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};

/// The `aver.toml` a user most likely meant for the project rooted at
/// `module_root`, when `module_root` has none of its own: the one in `cwd`,
/// or the nearest one above `module_root` within its repository (the
/// ancestors up to and including the first with a `.git` entry; with no
/// repository, only the direct parent).
pub fn unread_config_near(module_root: &Path, cwd: Option<&Path>) -> Option<PathBuf> {
    if module_root.join("aver.toml").is_file() {
        return None;
    }
    let root = module_root.canonicalize().ok()?;
    let mut candidates: Vec<PathBuf> = Vec::new();
    if let Some(cwd) = cwd.and_then(|dir| dir.canonicalize().ok())
        && cwd != root
    {
        candidates.push(cwd);
    }
    let ancestors: Vec<&Path> = root.ancestors().skip(1).collect();
    let repo_top = ancestors.iter().position(|dir| dir.join(".git").exists());
    let reach = match repo_top {
        Some(index) => index + 1,
        None if root.join(".git").exists() => 0,
        None => 1,
    };
    candidates.extend(ancestors.into_iter().take(reach).map(Path::to_path_buf));
    candidates
        .into_iter()
        .map(|dir| dir.join("aver.toml"))
        .find(|path| path.is_file())
}

static WARNED: AtomicBool = AtomicBool::new(false);

/// Say once per process that the `aver.toml` near `module_root` is not
/// read, when there is one and `module_root` has none.
pub(super) fn warn_unread_config(module_root: &Path) {
    if WARNED.load(Ordering::Relaxed) {
        return;
    }
    let cwd = std::env::current_dir().ok();
    let Some(path) = unread_config_near(module_root, cwd.as_deref()) else {
        return;
    };
    if WARNED.swap(true, Ordering::Relaxed) {
        return;
    }
    eprintln!(
        "warning[config-outside-module-root]: {} is not read: aver.toml is read only from the module root ({}); move it there",
        path.display(),
        module_root.display(),
    );
}

#[cfg(test)]
mod tests {
    use super::unread_config_near;

    #[test]
    fn finds_the_manifest_one_level_above_the_module_root() {
        let project = tempfile::tempdir().expect("tempdir");
        std::fs::create_dir(project.path().join(".git")).unwrap();
        std::fs::create_dir(project.path().join("src")).unwrap();
        std::fs::write(project.path().join("aver.toml"), "[rust]\n").unwrap();
        let root = project.path().join("src");
        let found = unread_config_near(&root, None).expect("found");
        assert_eq!(
            found,
            project.path().canonicalize().unwrap().join("aver.toml")
        );
    }

    #[test]
    fn finds_the_manifest_in_the_working_directory() {
        let project = tempfile::tempdir().expect("tempdir");
        let elsewhere = tempfile::tempdir().expect("tempdir");
        std::fs::write(elsewhere.path().join("aver.toml"), "[rust]\n").unwrap();
        let found = unread_config_near(project.path(), Some(elsewhere.path())).expect("found");
        assert_eq!(
            found,
            elsewhere.path().canonicalize().unwrap().join("aver.toml")
        );
    }

    #[test]
    fn silent_when_the_module_root_has_its_own() {
        let project = tempfile::tempdir().expect("tempdir");
        std::fs::create_dir(project.path().join("src")).unwrap();
        std::fs::write(project.path().join("aver.toml"), "").unwrap();
        std::fs::write(project.path().join("src/aver.toml"), "").unwrap();
        assert_eq!(
            unread_config_near(&project.path().join("src"), Some(project.path())),
            None
        );
    }

    #[test]
    fn does_not_climb_past_the_repository() {
        let outer = tempfile::tempdir().expect("tempdir");
        std::fs::write(outer.path().join("aver.toml"), "").unwrap();
        let repo = outer.path().join("repo");
        std::fs::create_dir_all(repo.join(".git")).unwrap();
        std::fs::create_dir_all(repo.join("src")).unwrap();
        assert_eq!(unread_config_near(&repo.join("src"), None), None);
    }
}
