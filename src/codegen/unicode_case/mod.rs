//! Shared Unicode case-mapping data for source-equivalent backend models.
//! Lean, Dafny and wasm-gc consume the same tables; the VM uses Rust std directly.
//!
//! # Where the numbers come from
//!
//! Every table in `tables.rs` is DERIVED FROM RUST'S OWN `char::to_lowercase`
//! / `char::to_uppercase` by probing all 1 112 064 Unicode scalars —
//! no new dependency, and the wasm-gc backend maps exactly what the VM
//! (`src/types/string.rs`) and the generated Rust
//! (`src/codegen/rust/from_mir.rs`) map. The two context predicates the
//! final-sigma rule needs are probed the same way, through
//! `str::to_lowercase` itself:
//!
//! ```text
//! cased(c)      := format!("{c}Σ").to_lowercase().ends_with('ς')
//! ignorable(c)  := !cased(c) && format!("a{c}Σ").to_lowercase().ends_with('ς')
//! ```
//!
//! These deliberately return the EFFECTIVE predicates in the order
//! `str::to_lowercase` applies them (`skip_while(Case_Ignorable).next()
//! .map(Cased)`): a scalar that Unicode marks both Cased and
//! Case_Ignorable — modifier letters such as U+02B0 — comes out of the
//! probes as ignorable-only, which is what the helper must implement.
//! The two sets are therefore disjoint by construction.
//!
//! # Regenerating
//!
//! The constants are committed, and `case_tables_match_std` re-derives
//! them from std on every test run — so a toolchain whose Unicode
//! version moved turns that test red instead of silently shipping stale
//! mappings. To refresh:
//!
//! ```text
//! cargo test -p aver-lang --lib --features wasm,wasip2 \
//!     dump_case_tables -- --ignored --nocapture
//! ```
//!
//! and replace the generated constants in `tables.rs`.

mod tables;
pub(in crate::codegen) use tables::*;

/// Integer decision expressions have identical syntax in Lean and Dafny.
#[cfg(feature = "runtime")]
pub(in crate::codegen) fn simple(runs: &[SimpleRun]) -> String {
    let Some(&(first, last, delta)) = runs.get(runs.len() / 2) else {
        return "n".into();
    };
    let mid = runs.len() / 2;
    format!(
        "(if n < {first} then {} else if n <= {last} then n + ({delta}) else {})",
        simple(&runs[..mid]),
        simple(&runs[mid + 1..]),
    )
}

#[cfg(feature = "runtime")]
pub(in crate::codegen) fn ranges(runs: &[Range]) -> String {
    let Some(&(first, last)) = runs.get(runs.len() / 2) else {
        return "false".into();
    };
    let mid = runs.len() / 2;
    format!(
        "(if n < {first} then {} else if n <= {last} then true else {})",
        ranges(&runs[..mid]),
        ranges(&runs[mid + 1..]),
    )
}

#[cfg(test)]
mod tests;
