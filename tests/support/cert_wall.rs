//! Test-only materialization for suites that invoke Lake directly.

use std::path::Path;

/// Materialize the checker-owned wall named by a generated package's
/// `format.wall_id`. Production verification performs the equivalent staging
/// in a fresh directory; certificate packages do not carry these files.
pub fn materialize(cert_dir: &Path) {
    for source in aver::codegen::cert::wall::SOURCES {
        std::fs::write(cert_dir.join(source.name), source.contents).unwrap();
    }
    std::fs::write(
        cert_dir.join("lean-toolchain"),
        aver::codegen::cert::wall::LEAN_TOOLCHAIN,
    )
    .unwrap();

    fn collect_roots(base: &Path, dir: &Path, roots: &mut Vec<String>) {
        for entry in std::fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            if entry.file_type().unwrap().is_dir() {
                collect_roots(base, &entry.path(), roots);
                continue;
            }
            if entry.path().extension().and_then(|ext| ext.to_str()) != Some("lean") {
                continue;
            }
            let relative = entry.path().strip_prefix(base).unwrap().to_path_buf();
            let mut components = relative
                .components()
                .map(|component| component.as_os_str().to_string_lossy().into_owned())
                .collect::<Vec<_>>();
            let leaf = components.last_mut().unwrap();
            leaf.truncate(leaf.len() - ".lean".len());
            roots.push(components.join("."));
        }
    }

    let mut roots = Vec::new();
    collect_roots(cert_dir, cert_dir, &mut roots);
    roots.sort();
    roots.dedup();
    let roots = roots
        .iter()
        .map(|root| format!("`{root}"))
        .collect::<Vec<_>>()
        .join(", ");
    let lakefile = format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{roots}]\n"
    );
    std::fs::write(cert_dir.join("lakefile.lean"), lakefile).unwrap();
}
