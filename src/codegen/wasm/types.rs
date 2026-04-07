/// Aver type -> WASM type mapping.
///
/// Native types — no NaN-boxing:
///   Int   → i64 (plain value)
///   Float → f64 (TODO: when float support is added)
///   Bool  → i64 (0 = false, 1 = true)
///   Unit  → i64 (0)
use wasm_encoder::ValType;

/// The default WASM value type for Aver values.
/// MVP: everything is i64 with native semantics (not NaN-boxed).
pub const AVER_WASM_TYPE: ValType = ValType::I64;
