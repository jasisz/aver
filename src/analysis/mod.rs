//! Static-analysis tier — shared substrate over typed HIR, consumed
//! by codegen (proof_lower, refinement detection, future inliner /
//! monomorphizer) and by user-facing tooling (`aver shape` CLI / LSP
//! / context).
//!
//! Pieces in this module are recognition-only: they take a typed HIR
//! snapshot and emit typed facts / patterns / relations. They do not
//! decide proof strategies, they do not render to text, they do not
//! talk to backends. Those responsibilities live one layer up
//! (`codegen::proof_lower` for meaning; `diagnostics::shape` for
//! presentation).
//!
//! See issue #232 (0.23 "Shape") for the architectural plan and the
//! peer-review notes that produced this split.

pub mod literal_refinement;
pub mod shape;
