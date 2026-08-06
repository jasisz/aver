/// Executable SHA-256 model emitted for Dafny projects that use
/// `Crypto.sha256`. The bv32 implementation gives the verifier the same
/// modulo-2^32 semantics as the runtime intrinsic without an oracle or axiom.
pub(super) const SOURCE: &str = include_str!("crypto_model.dfy");
pub(super) const CORE_SOURCE: &str = include_str!("crypto_core_model.dfy");
