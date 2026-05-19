//! Fuzz-only AST → source unparser for Aver.
//!
//! Lives in `fuzz/mutator/` deliberately — this is **not** a
//! production formatter. It exists so AST-aware fuzz mutations can
//! round-trip back through the parser. Once the unparser lands and
//! its corpus round-trip suite passes, the custom AFL mutator
//! strategies will sit on top of it.
//!
//! Guarantees:
//! - For every AST shape the parser produces, `unparse → parse`
//!   succeeds.
//! - Unsupported variants (`Expr::TailCall`, `Expr::Resolved` —
//!   produced by post-parse passes the mutator should never see)
//!   return an explicit `UnparseError::Unsupported(_)` rather than
//!   silent fallbacks or `Debug`-shaped output.
//!
//! Not guaranteed:
//! - Pretty formatting. Output is parseable, not human-friendly.
//! - Comment preservation. The parser drops comments; the unparser
//!   has nothing to re-emit.
//! - Source-span fidelity. Line / column numbers in the output
//!   bear no relation to the input's positions.
//! - Idempotence vs `aver format`. The production formatter and
//!   this unparser produce different bytes from the same AST by
//!   design — `aver format` cares about whitespace rules, this
//!   one doesn't.
//! - Excessive parentheses are normal. We never reason about
//!   operator precedence; every `BinOp`, `Neg`, and `ErrorProp`
//!   wraps its operands. The parser handles the redundant parens.

pub mod afl_api;
pub mod mutations;
pub mod unparse;

pub use unparse::{UnparseError, unparse};
