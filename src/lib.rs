extern crate self as aver;

pub mod ast;
pub mod call_graph;
pub mod checker;
pub mod codegen;
#[cfg(feature = "runtime")]
pub mod config;
pub mod effects;
#[cfg(feature = "runtime")]
#[allow(dead_code)]
#[path = "main/format_cmd.rs"]
pub mod format;
pub mod ir;
pub mod lexer;
pub mod nan_value;
pub mod parser;
#[cfg(any(feature = "wasm-compile", feature = "playground"))]
pub mod playground;
#[cfg(feature = "runtime")]
pub mod replay;
pub mod resolver;
#[cfg(feature = "runtime")]
pub mod runtime_bench_cases;
#[cfg(feature = "runtime")]
pub mod services;
pub mod source;
pub mod tail_check;
pub mod tco;
pub mod types;
pub mod value;
pub mod verify_law;
#[cfg(feature = "runtime")]
pub mod vm;
