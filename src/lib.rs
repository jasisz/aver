extern crate self as aver;

pub mod ast;
pub mod call_graph;
pub mod checker;
pub mod codegen;
pub mod config;
pub mod effects;
#[allow(dead_code)]
#[path = "main/format_cmd.rs"]
pub mod format;
pub mod interpreter;
pub mod ir;
pub mod lexer;
pub mod nan_value;
pub mod parser;
pub mod replay;
pub mod resolver;
pub mod runtime_bench_cases;
pub mod services;
pub mod source;
pub mod tail_check;
pub mod tco;
pub mod types;
pub mod value;
pub mod verify_law;
pub mod vm;
