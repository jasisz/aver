//! The bridge theorems of a certificate package, which the producer spreads
//! over `BridgeProof<i>.lean` slices. Each suite uses some of these helpers.
#![allow(dead_code)]

use std::path::{Path, PathBuf};

/// The `BridgeProof<i>.lean` slices of the package in `cert`, in slice order.
pub fn bridge_proof_slices(cert: &Path) -> Vec<PathBuf> {
    let mut slices: Vec<(u32, PathBuf)> = std::fs::read_dir(cert)
        .expect("the package directory is readable")
        .filter_map(|entry| {
            let path = entry.ok()?.path();
            let name = path.file_name()?.to_str()?;
            let index = name
                .strip_prefix("BridgeProof")?
                .strip_suffix(".lean")?
                .parse()
                .ok()?;
            Some((index, path))
        })
        .collect();
    slices.sort();
    slices.into_iter().map(|(_, path)| path).collect()
}

/// Every bridge slice's text, joined in slice order.
pub fn bridge_proof_text(cert: &Path) -> String {
    bridge_proof_slices(cert)
        .iter()
        .map(|path| std::fs::read_to_string(path).expect("a bridge slice is readable"))
        .collect()
}

/// The slice that declares `theorem` (a `_root_`-qualified name).
pub fn bridge_proof_slice_of(cert: &Path, theorem: &str) -> PathBuf {
    let header = format!("theorem {theorem} :");
    bridge_proof_slices(cert)
        .into_iter()
        .find(|path| {
            std::fs::read_to_string(path)
                .expect("a bridge slice is readable")
                .contains(&header)
        })
        .unwrap_or_else(|| panic!("no bridge slice declares {theorem}"))
}
