use std::sync::Arc as Rc;

use crate::ast::*;
use crate::lexer::{Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("error[{line}:{col}]: {msg}")]
    Error {
        msg: String,
        line: usize,
        col: usize,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    /// Iron — B4: monotonically-increasing recursion counter, guarded
    /// against by `enter_recursion`. Without this the parser unwinds
    /// its stack on inputs the fuzz harness routinely produces:
    /// 2500+ nested `(...)`, `Option.Some(...)`, or `{...}` literals
    /// reach the thread's default 8 MiB stack and abort with
    /// `fatal runtime error: stack overflow` — a SIGSEGV that
    /// `panic::catch_unwind` cannot recover from, taking the whole
    /// `aver` invocation down. The cap below trades the theoretical
    /// "what if someone really wants 1000-deep expression?" against
    /// the practical "no human writes a 1000-deep expression and
    /// fuzz keeps finding them".
    recursion_depth: u32,
}

/// Iron — B4: hard cap on parser recursion depth. 128 is comfortably
/// above any hand-written Aver shape (the deepest production
/// expression in the corpus measures ~12 levels) and comfortably
/// below the threshold where the parser starts unwinding the test
/// runner's 2 MiB thread stack on the next nested `(`. The
/// trade-off is intentional — users hit the typed error long
/// before the runtime hits SIGSEGV, and the 2 MiB worker stack
/// `cargo test` allocates per thread is the binding constraint,
/// not the 8 MiB main-thread default in standalone CLI runs.
pub(super) const MAX_PARSE_DEPTH: u32 = 64;

mod blocks;
mod core;
mod expr;
mod functions;
mod module;
mod patterns;
mod types;
