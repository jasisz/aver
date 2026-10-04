#![cfg(feature = "wasm")]

//! Guard — no Aver program in the repository compiles to a wasm-gc module
//! with a trap stub in it.
//!
//! The wasm-gc body emitter used to give up on a fn it could not lower and
//! ship an `unreachable` body in its place: the module compiled clean and
//! trapped the first time the fn ran. Such a fn is now a compile error
//! naming the fn and what it contains, except for a proof helper reading a
//! `BranchPath`, which nothing compiled to wasm-gc can call.
//! `AVER_WASMGC_REQUIRE_MIR=1` refuses that exception too, so this test
//! compiles every `.av` file under `examples/`, `projects/` and
//! `tests/fixtures/` / `tests/regressions/` with it set and requires that
//! the only refusals are those proof helpers. A file that is not a
//! standalone program (a dependency module, a deliberately broken fixture)
//! fails for its own reasons and is not this test's business.

use std::path::{Path, PathBuf};
use std::process::Command;

const ALLOWED: &str = "BranchPath is proof-only on wasm-gc";

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(read) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in read.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
        } else if path.extension().and_then(|s| s.to_str()) == Some("av") {
            out.push(path);
        }
    }
}

/// Compile `file` to wasm-gc with every stub refused. The module root is
/// the file's directory, or the nearest ancestor that resolves its
/// `depends`. Returns the refusal text when the backend refused a fn.
fn refusal(file: &Path, root: &Path, out_dir: &Path) -> Option<String> {
    let mut module_root = file.parent()?.to_path_buf();
    loop {
        let output = Command::new(env!("CARGO_BIN_EXE_aver"))
            .arg("compile")
            .arg("--target")
            .arg("wasm-gc")
            .arg(file)
            .arg("--module-root")
            .arg(&module_root)
            .arg("-o")
            .arg(out_dir)
            .env("AVER_WASMGC_REQUIRE_MIR", "1")
            .output()
            .expect("run aver compile");
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
        let unresolved = text.contains("Cannot find module");
        if unresolved && module_root != root && module_root.pop() {
            continue;
        }
        return text
            .lines()
            .find(|line| line.contains("wasm-gc backend cannot compile"))
            .map(str::to_owned);
    }
}

/// Every refused fn in `line` is a `BranchPath` proof helper.
fn only_allowed(line: &str) -> bool {
    let Some((_, fns)) = line.split_once("fns: ") else {
        return false;
    };
    fns.split("; fn `").all(|part| part.contains(ALLOWED))
}

#[test]
fn no_program_in_the_repository_compiles_to_a_trap_stub() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    for dir in [
        "examples",
        "projects",
        "tests/fixtures",
        "tests/regressions",
    ] {
        collect(&root.join(dir), &mut files);
    }
    files.sort();
    assert!(
        files.len() > 500,
        "corpus walk found only {} files",
        files.len()
    );

    let workers = std::thread::available_parallelism()
        .map_or(4, usize::from)
        .min(8);
    let chunks: Vec<&[PathBuf]> = files.chunks(files.len().div_ceil(workers)).collect();
    let mut refused: Vec<String> = Vec::new();
    let mut allowed: Vec<String> = Vec::new();
    std::thread::scope(|scope| {
        let handles: Vec<_> = chunks
            .iter()
            .map(|chunk| {
                let root = root.clone();
                scope.spawn(move || {
                    let out = tempfile::tempdir().expect("tempdir");
                    chunk
                        .iter()
                        .filter_map(|file| {
                            let line = refusal(file, &root, out.path())?;
                            let shown = file.strip_prefix(&root).unwrap_or(file).display();
                            Some((only_allowed(&line), format!("{shown}: {line}")))
                        })
                        .collect::<Vec<_>>()
                })
            })
            .collect();
        for handle in handles {
            for (ok, line) in handle.join().expect("worker panicked") {
                if ok {
                    allowed.push(line);
                } else {
                    refused.push(line);
                }
            }
        }
    });

    assert!(
        refused.is_empty(),
        "{} of {} programs hold a fn the wasm-gc backend cannot lower:\n  - {}",
        refused.len(),
        files.len(),
        refused.join("\n  - ")
    );
    // Today the one program with a `BranchPath` proof helper is
    // `examples/formal/oracle_independent_products.av`.
    eprintln!(
        "{} programs compiled with no trap stub; {} keep only BranchPath proof helpers",
        files.len() - allowed.len(),
        allowed.len()
    );
}
