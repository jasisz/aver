//! Compatibility surface for certificate production in `aver-lang`.
//!
//! The compiler-independent engine and soundness wall live in `aver-cert`.
//! Only the MIR-to-plan adapter remains here because it necessarily depends on
//! Aver's compiler IR.

pub use aver_cert::*;

include!("expr_fragment_from_mir.rs");
