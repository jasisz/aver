//! Phase 6 MIR optimization passes.
//!
//! Each pass is a pure function `MirProgram → MirProgram`, so the
//! pipeline composes through plain function chaining
//! (`branch_collapse(bool_match_to_if(const_fold(program)))`).
//! The VM compiler wires them up in
//! `vm::compiler::compile_program_with_mir_fallback`.
//!
//! ## Pass directory
//!
//! - [`const_fold`] (wave 5) — evaluate `BinOp` / `Neg` over
//!   literal operands at compile time.
//! - [`dead_code`] (wave 6) — drop `Let { value: <pure>, body }`
//!   bindings that `body` never reads.
//! - [`inline`] (wave 7) — replace nullary literal-only fn
//!   calls with the callee's literal.
//! - [`algebraic`] (wave 8) — rewrite Int identity shapes
//!   (`x + 0`, `x * 1`, `Neg(Neg(x))`).
//! - [`bool_match`] (wave 9) — lift two-arm `Bool` `Match`
//!   into `MirExpr::IfThenElse`.
//! - [`branch_collapse`] (wave 10) — drop the dead branch of
//!   an `IfThenElse` whose `cond` folded to a literal `Bool`.
//!
//! ## Cascades
//!
//! Two passes that look orthogonal compose in interesting ways
//! end-to-end:
//!
//! - `const_fold` → `algebraic` → `dead_code`: `let x = 1 + 0; 7`
//!   folds the value, the symbolic identity drops the BinOp,
//!   DCE drops the unused binding.
//! - `const_fold` → `bool_match` → `branch_collapse`:
//!   `match (5 == 5) { true → A; false → B }` folds the
//!   subject to `true`, lifts the match to `if true { A }
//!   else { B }`, collapses to `A`.
//!
//! ## What this layer doesn't transform (deferred)
//!
//! - General inlining with param substitution.
//! - String concatenation folding (would need intern-side
//!   allocation policy; the VM does it efficiently at runtime
//!   already).
//! - Match arms (would require pattern → literal
//!   classification plumbing; defer until there's a real perf
//!   signal).
//! - CSE / loop-invariant hoisting — future waves.

pub mod algebraic;
pub mod bool_match;
pub mod branch_collapse;
pub mod const_fold;
pub mod dead_code;
pub mod inline;

#[cfg(test)]
pub(crate) mod test_helpers;

pub use algebraic::algebraic_simplify;
pub use bool_match::bool_match_to_if;
pub use branch_collapse::branch_collapse;
pub use const_fold::const_fold;
pub use dead_code::dead_code;
pub use inline::inline_nullary_literals;
