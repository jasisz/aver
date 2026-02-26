use std::rc::Rc;

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
}

mod blocks;
mod core;
mod expr;
mod functions;
mod module;
mod patterns;
mod types;
