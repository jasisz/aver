//! Compatibility surface for certificate production in `aver-lang`.
//!
//! The compiler-independent engine and soundness wall live in `aver-cert`.
//! Only the MIR-to-plan printer remains here because it necessarily depends on
//! Aver's compiler IR.

pub use aver_cert::*;

mod plan_from_mir;
pub use plan_from_mir::{
    PlanLayout, RecordLayout, SumLayout, TypeTableBuilder, print_fn, print_module,
};
