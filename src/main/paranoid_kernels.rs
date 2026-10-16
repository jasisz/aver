//! The kernels `aver proof --paranoid` runs: every proof checker the pinned
//! Lean release ships, over one `leanexport` of the proof project.
//!
//! The certificate verifier owns this list (`aver_cert::kernels`). The
//! compiler links `aver-cert` only with the wasm features, so the list is
//! repeated here, and a test built with those features requires the two to be
//! identical, entry for entry and byte for byte in the nanoda configuration.

/// Lean's standard axioms, which every kernel is told are permitted. Axioms
/// outside them are judged by the proof's own per-law audit, not here.
const PERMITTED_AXIOMS: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

/// The toolchain binary that writes the export the kernels read.
pub(super) const EXPORTER: &str = "leanexport";

/// One kernel: display name, toolchain binary, leading arguments, and whether
/// it reads a nanoda configuration (otherwise the export path) last.
pub(super) struct Kernel {
    pub name: &'static str,
    pub binary: &'static str,
    pub args: &'static [&'static str],
    pub nanoda_config: bool,
}

/// Every kernel `--paranoid` runs, Lean's own first.
pub(super) const PARANOID_KERNELS: [Kernel; 6] = [
    Kernel {
        name: "Lean",
        binary: "leanchecker",
        args: &["--silent", "--from-export"],
        nanoda_config: false,
    },
    Kernel {
        name: "Lean (hardened allocator)",
        binary: "leanchecker-paranoid",
        args: &["--silent", "--from-export"],
        nanoda_config: false,
    },
    Kernel {
        name: "lean4lean",
        binary: "lean4lean",
        args: &["--import"],
        nanoda_config: false,
    },
    Kernel {
        name: "nanoda",
        binary: "nanoda_bin",
        args: &[],
        nanoda_config: true,
    },
    Kernel {
        name: "con-leche",
        binary: "con-leche",
        args: &[],
        nanoda_config: false,
    },
    Kernel {
        name: "con-ron",
        binary: "con-ron",
        args: &[],
        nanoda_config: false,
    },
];

impl Kernel {
    pub(super) fn arguments(&self, export_path: &str, nanoda_config_path: &str) -> Vec<String> {
        let mut arguments: Vec<String> = self.args.iter().map(|a| (*a).to_string()).collect();
        arguments.push(if self.nanoda_config {
            nanoda_config_path.to_string()
        } else {
            export_path.to_string()
        });
        arguments
    }
}

/// The nanoda configuration for one export, the fields `lake check` writes.
pub(super) fn nanoda_config(export_path: &str) -> String {
    let axioms = PERMITTED_AXIOMS
        .iter()
        .map(|axiom| serde_json::Value::from(*axiom).to_string())
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "{{\"use_stdin\":false,\"export_file_path\":{},\"permitted_axioms\":[{axioms}],\
         \"unpermitted_axiom_hard_error\":false,\"num_threads\":4,\
         \"nat_extension\":true,\"string_extension\":true}}",
        serde_json::Value::from(export_path)
    )
}

#[cfg(all(test, feature = "wasm-compile"))]
mod tests {
    use super::*;

    #[test]
    fn the_list_is_the_certificate_verifiers_list() {
        let theirs = aver_cert::kernels::PARANOID_KERNELS;
        assert_eq!(PARANOID_KERNELS.len(), theirs.len());
        assert_eq!(EXPORTER, aver_cert::kernels::EXPORTER);
        assert_eq!(PERMITTED_AXIOMS, aver_cert::kernels::PERMITTED_AXIOMS);
        for (ours, theirs) in PARANOID_KERNELS.iter().zip(theirs.iter()) {
            assert_eq!(ours.name, theirs.name);
            assert_eq!(ours.binary, theirs.binary);
            assert_eq!(
                ours.arguments("/e.ndjson", "/n.json"),
                theirs.arguments("/e.ndjson", "/n.json")
            );
        }
        for path in ["/tmp/x.ndjson", "/tmp/a \"b\"\\c.ndjson"] {
            assert_eq!(nanoda_config(path), aver_cert::kernels::nanoda_config(path));
        }
    }
}
