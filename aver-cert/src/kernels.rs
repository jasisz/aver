//! The paranoid kernel set: every proof checker the pinned Lean release ships,
//! run over one `leanexport` of the checked module closure.
//!
//! The list and the argument shapes are the ones `lake check --paranoid`
//! uses (Lake's `Check.bundledKernels` plus Lean's own `leanchecker
//! --from-export`). `lake check` itself is not used: it needs a Linux `bwrap`
//! sandbox, it exports every default target of the package rather than one
//! module closure, and it rejects the whole run on any axiom outside the
//! standard three, where a certificate or proof project reports such a claim
//! as not credited and keeps judging the rest.
//!
//! Each kernel's exit status is its verdict. A kernel that is missing, cannot
//! start, or exits nonzero fails the run; no kernel is ever skipped.
//!
//! Dependency-free on purpose: the certificate verifier and `aver proof`
//! both read this one list.

/// Lean's standard axioms, the set every kernel is told is permitted. Axioms
/// outside it are reported by the callers' own audits, not here.
pub const PERMITTED_AXIOMS: [&str; 3] = ["propext", "Classical.choice", "Quot.sound"];

/// The toolchain binary that writes the NDJSON export the kernels read.
pub const EXPORTER: &str = "leanexport";

/// How a kernel is told where the export is.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum KernelInput {
    /// The export path is the last argument.
    ExportPath,
    /// The last argument is a JSON configuration naming the export (nanoda).
    NanodaConfig,
}

/// One kernel: a display name, the toolchain binary, its leading arguments,
/// how it is given the export, and whether it accepts declarations that use
/// `sorryAx`. A kernel that does not (nanoda leaves the axiom out of its
/// environment; con-leche and con-ron refuse it) reads the export without the
/// declarations that depend on `sorryAx`, none of which a caller credits.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Kernel {
    pub name: &'static str,
    pub binary: &'static str,
    pub args: &'static [&'static str],
    pub input: KernelInput,
    pub admits_sorry: bool,
}

/// Every kernel `--paranoid` runs, Lean's own first. All of them ship in the
/// pinned toolchain's `bin/` directory.
pub const PARANOID_KERNELS: [Kernel; 6] = [
    Kernel {
        name: "Lean",
        binary: "leanchecker",
        args: &["--silent", "--from-export"],
        input: KernelInput::ExportPath,
        admits_sorry: true,
    },
    Kernel {
        name: "Lean (hardened allocator)",
        binary: "leanchecker-paranoid",
        args: &["--silent", "--from-export"],
        input: KernelInput::ExportPath,
        admits_sorry: true,
    },
    Kernel {
        name: "lean4lean",
        binary: "lean4lean",
        args: &["--import"],
        input: KernelInput::ExportPath,
        admits_sorry: true,
    },
    Kernel {
        name: "nanoda",
        binary: "nanoda_bin",
        args: &[],
        input: KernelInput::NanodaConfig,
        admits_sorry: false,
    },
    Kernel {
        name: "con-leche",
        binary: "con-leche",
        args: &[],
        input: KernelInput::ExportPath,
        admits_sorry: false,
    },
    Kernel {
        name: "con-ron",
        binary: "con-ron",
        args: &[],
        input: KernelInput::ExportPath,
        admits_sorry: false,
    },
];

impl Kernel {
    /// The full command line after the binary: leading arguments, then the
    /// export path or the nanoda configuration path.
    pub fn arguments(&self, export_path: &str, nanoda_config_path: &str) -> Vec<String> {
        let mut arguments: Vec<String> = self.args.iter().map(|a| (*a).to_string()).collect();
        arguments.push(match self.input {
            KernelInput::ExportPath => export_path.to_string(),
            KernelInput::NanodaConfig => nanoda_config_path.to_string(),
        });
        arguments
    }
}

/// The nanoda configuration for one export, the same fields `lake check`
/// writes. Axioms outside [`PERMITTED_AXIOMS`] are not a hard error here: the
/// callers audit axioms per claim themselves.
pub fn nanoda_config(export_path: &str) -> String {
    let axioms = PERMITTED_AXIOMS
        .iter()
        .map(|axiom| json_string(axiom))
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "{{\"use_stdin\":false,\"export_file_path\":{},\"permitted_axioms\":[{axioms}],\
         \"unpermitted_axiom_hard_error\":false,\"num_threads\":4,\
         \"nat_extension\":true,\"string_extension\":true}}",
        json_string(export_path)
    )
}

fn json_string(text: &str) -> String {
    let mut out = String::with_capacity(text.len() + 2);
    out.push('"');
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            ch if (ch as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", ch as u32)),
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

/// An export that names none of the expected declarations is not a check of
/// anything: every kernel accepts an empty environment. Callers pass a name
/// component their root module is known to define.
pub fn export_mentions(export: &[u8], name_component: &str) -> bool {
    let needle = json_string(name_component);
    export
        .windows(needle.len())
        .any(|window| window == needle.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_kernel_gets_the_export_or_its_configuration_last() {
        for kernel in PARANOID_KERNELS {
            let arguments = kernel.arguments("/x/export.ndjson", "/x/nanoda.json");
            let last = arguments.last().unwrap();
            match kernel.input {
                KernelInput::ExportPath => assert_eq!(last, "/x/export.ndjson"),
                KernelInput::NanodaConfig => assert_eq!(last, "/x/nanoda.json"),
            }
        }
    }

    #[test]
    fn the_set_is_lean_first_and_has_no_duplicates() {
        assert_eq!(PARANOID_KERNELS[0].binary, "leanchecker");
        let mut binaries: Vec<_> = PARANOID_KERNELS.iter().map(|k| k.binary).collect();
        binaries.sort_unstable();
        binaries.dedup();
        assert_eq!(binaries.len(), PARANOID_KERNELS.len());
    }

    #[test]
    fn nanoda_config_names_the_export_and_the_standard_axioms() {
        let config = nanoda_config("/tmp/a \"b\".ndjson");
        assert!(config.contains("\"export_file_path\":\"/tmp/a \\\"b\\\".ndjson\""));
        assert!(
            config
                .contains("\"permitted_axioms\":[\"propext\",\"Classical.choice\",\"Quot.sound\"]")
        );
        assert!(config.contains("\"use_stdin\":false"));
    }

    #[test]
    fn export_mentions_matches_a_whole_json_string_only() {
        let export = br#"{"str":{"pre":1,"str":"CheckerWitness"}}"#;
        assert!(export_mentions(export, "CheckerWitness"));
        assert!(!export_mentions(export, "Checker"));
        assert!(!export_mentions(b"", "CheckerWitness"));
    }
}
