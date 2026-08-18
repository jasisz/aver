// `Value` carries `OnceLock<…>` (typed-AST stamps + per-arm pattern
// binding slots) deep in its substructure, which is interior mutability
// — but those cells are write-once and never reach the hash code path,
// so HashMap<Value, …> is safe in practice. Silence the clippy lint at
// the crate level rather than papering over every `HashMap::new()` site.
#![allow(clippy::mutable_key_type)]

extern crate self as aver;

pub mod analysis;
pub mod ast;
pub mod ast_rewrite;
#[cfg(feature = "runtime")]
pub mod bench;
pub mod call_graph;
pub mod capability;
pub mod checker;
#[cfg(all(feature = "runtime", feature = "terminal", feature = "tty-render"))]
pub mod cli_entry;
pub mod codegen;
#[cfg(feature = "runtime")]
pub mod config;
pub mod diagnostics;
pub mod effects;
#[cfg(all(feature = "runtime", feature = "tty-render"))]
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
pub mod provider;
#[cfg(all(feature = "runtime", feature = "terminal", feature = "tty-render"))]
mod provider_vm_host;
#[cfg(feature = "runtime")]
pub mod replay;
pub mod resolver;
#[cfg(feature = "runtime")]
pub mod runtime;
#[cfg(feature = "runtime")]
pub mod runtime_bench_cases;
pub mod scc;
#[cfg(feature = "runtime")]
pub mod services;
pub mod source;
pub mod stdlib;
pub mod tail_check;
pub mod tco;
#[cfg(feature = "tty-render")]
pub mod tty_render;
pub mod types;
pub mod value;
pub mod verify_law;
pub mod visibility;
#[cfg(feature = "runtime")]
pub mod vm;
