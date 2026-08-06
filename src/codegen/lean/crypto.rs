/// Executable SHA-256 model emitted for proof projects that use
/// `Crypto.sha256`. It intentionally depends only on Lean's core UInt32
/// operations: proof exports stay hermetic and do not gain a Mathlib or native
/// crypto dependency.
pub(super) const SOURCE: &str = include_str!("crypto_model.lean");
