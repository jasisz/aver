use super::*;

mod binop;
mod expr;
pub(in crate::types::checker) use expr::type_is_fully_concrete;
mod list_calls;
mod map_calls;
mod patterns;
mod records;
mod vector_calls;
