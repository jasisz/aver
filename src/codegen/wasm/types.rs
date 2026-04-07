/// Aver type -> WASM type mapping.
///
/// Native types:
///   Int    → i64
///   Float  → f64
///   Bool   → i32 (0 = false, 1 = true)
///   Unit   → i32 (0)
///   String → i32 (heap pointer)
///   Result/Option/List/Record/Tuple → i32 (heap pointer)
use wasm_encoder::ValType;

use crate::types::Type;

/// WASM machine type for an Aver value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WasmType {
    I32,
    I64,
    F64,
}

impl WasmType {
    pub fn to_val_type(self) -> ValType {
        match self {
            WasmType::I32 => ValType::I32,
            WasmType::I64 => ValType::I64,
            WasmType::F64 => ValType::F64,
        }
    }
}

/// Map an Aver semantic type to the corresponding WASM machine type.
pub fn aver_type_to_wasm(ty: &Type) -> WasmType {
    match ty {
        Type::Int => WasmType::I64,
        Type::Float => WasmType::F64,
        Type::Bool => WasmType::I32,
        Type::Unit => WasmType::I32,
        Type::Str => WasmType::I32,
        Type::Result(..) | Type::Option(..) | Type::List(..) => WasmType::I32,
        Type::Named(..) | Type::Tuple(..) | Type::Map(..) | Type::Vector(..) => WasmType::I32,
        Type::Fn(..) => WasmType::I32,
        Type::Unknown => WasmType::I64, // fallback
    }
}
