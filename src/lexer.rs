use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Int(i64),
    Float(f64),
    Str(String),
    InterpStr(Vec<(bool, String)>), // (is_expr, text)
    Bool(bool),
    // Identifiers
    Ident(String),
    // Keywords
    Module,
    Depends,
    Exposes,
    Intent,
    Type,
    Record,
    Val,
    Var,
    Reason,
    Fn,
    Effect,
    Effects,
    Service,
    Needs,
    Decision,
    Verify,
    Case,
    Match,
    Where,
    Input,
    Expect,
    Date,
    Author,
    Chosen,
    Rejected,
    Impacts,
    Ok,
    Err,
    Some,
    None,
    // Operators
    Arrow,    // ->
    Pipe,     // |>
    FatArrow, // =>
    Eq,       // ==
    Neq,      // !=
    Lte,      // <=
    Gte,      // >=
    Assign,   // =
    Bang,     // !
    Question, // ?
    Lt,       // <
    Gt,       // >
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /
    Dot,      // .
    Colon,    // :
    Comma,    // ,
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    // Structure
    Indent,
    Dedent,
    Newline,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("Lexer error [{line}:{col}]: {msg}")]
    Error {
        msg: String,
        line: usize,
        col: usize,
    },
}

fn keyword(s: &str) -> Option<TokenKind> {
    match s {
        "module" => Some(TokenKind::Module),
        "depends" => Some(TokenKind::Depends),
        "exposes" => Some(TokenKind::Exposes),
        "intent" => Some(TokenKind::Intent),
        "type" => Some(TokenKind::Type),
        "record" => Some(TokenKind::Record),
        "val" => Some(TokenKind::Val),
        "var" => Some(TokenKind::Var),
        "reason" => Some(TokenKind::Reason),
        "fn" => Some(TokenKind::Fn),
        "effect" => Some(TokenKind::Effect),
        "effects" => Some(TokenKind::Effects),
        "service" => Some(TokenKind::Service),
        "needs" => Some(TokenKind::Needs),
        "decision" => Some(TokenKind::Decision),
        "verify" => Some(TokenKind::Verify),
        "case" => Some(TokenKind::Case),
        "match" => Some(TokenKind::Match),
        "where" => Some(TokenKind::Where),
        "input" => Some(TokenKind::Input),
        "expect" => Some(TokenKind::Expect),
        "date" => Some(TokenKind::Date),
        "author" => Some(TokenKind::Author),
        "chosen" => Some(TokenKind::Chosen),
        "rejected" => Some(TokenKind::Rejected),
        "impacts" => Some(TokenKind::Impacts),
        "true" => Some(TokenKind::Bool(true)),
        "false" => Some(TokenKind::Bool(false)),
        "Ok" => Some(TokenKind::Ok),
        "Err" => Some(TokenKind::Err),
        "Some" => Some(TokenKind::Some),
        "None" => Some(TokenKind::None),
        _ => None,
    }
}

pub struct Lexer {
    chars: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    indent_stack: Vec<usize>,
    at_line_start: bool,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            chars: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            indent_stack: vec![0],
            at_line_start: true,
        }
    }

    fn error(&self, msg: impl Into<String>) -> LexerError {
        LexerError::Error {
            msg: msg.into(),
            line: self.line,
            col: self.col,
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.chars.get(self.pos + offset).copied()
    }

    fn current(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.get(self.pos).copied()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens = Vec::new();

        while self.pos < self.chars.len() {
            self.scan_token(&mut tokens)?;
        }

        // Emit remaining DEDENTs
        while self.indent_stack.len() > 1 {
            self.indent_stack.pop();
            tokens.push(Token {
                kind: TokenKind::Dedent,
                line: self.line,
                col: self.col,
            });
        }

        tokens.push(Token {
            kind: TokenKind::Eof,
            line: self.line,
            col: self.col,
        });

        Ok(tokens)
    }

    fn scan_token(&mut self, tokens: &mut Vec<Token>) -> Result<(), LexerError> {
        if self.at_line_start {
            self.handle_indentation(tokens)?;
            if self.pos >= self.chars.len() {
                return Ok(());
            }
        }

        let ch = match self.current() {
            Some(c) => c,
            None => return Ok(()),
        };

        // Skip spaces (not at line start)
        if ch == ' ' {
            self.advance();
            return Ok(());
        }

        // Newline
        if ch == '\n' {
            let line = self.line;
            let col = self.col;
            self.advance();

            let last_is_structural = tokens
                .last()
                .map(|t| {
                    matches!(
                        t.kind,
                        TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
                    )
                })
                .unwrap_or(true);

            if !tokens.is_empty() && !last_is_structural {
                tokens.push(Token {
                    kind: TokenKind::Newline,
                    line,
                    col,
                });
            }
            self.at_line_start = true;
            return Ok(());
        }

        // Carriage return
        if ch == '\r' {
            self.advance();
            return Ok(());
        }

        // Comments
        if ch == '/' && self.peek(1) == Some('/') {
            self.skip_comment();
            return Ok(());
        }

        // Strings
        if ch == '"' {
            let tok = self.scan_string()?;
            tokens.push(tok);
            return Ok(());
        }

        // Numbers
        if ch.is_ascii_digit() {
            let tok = self.scan_number()?;
            tokens.push(tok);
            return Ok(());
        }

        // Identifiers / keywords
        if ch.is_alphabetic() || ch == '_' {
            let tok = self.scan_identifier();
            tokens.push(tok);
            return Ok(());
        }

        // Operators
        let tok = self.scan_operator()?;
        tokens.push(tok);
        Ok(())
    }

    fn handle_indentation(&mut self, tokens: &mut Vec<Token>) -> Result<(), LexerError> {
        self.at_line_start = false;
        let mut indent = 0;

        while self.pos < self.chars.len() && self.chars[self.pos] == ' ' {
            indent += 1;
            self.pos += 1;
            self.col += 1;
        }

        // Empty line or comment-only line — don't emit indent/dedent
        if self.pos < self.chars.len() {
            let ch = self.chars[self.pos];
            if ch == '\n' || ch == '\r' {
                return Ok(());
            }
            if ch == '/' && self.pos + 1 < self.chars.len() && self.chars[self.pos + 1] == '/' {
                return Ok(());
            }
        } else {
            return Ok(());
        }

        let current = *self.indent_stack.last().unwrap();
        let line = self.line;

        if indent > current {
            self.indent_stack.push(indent);
            tokens.push(Token {
                kind: TokenKind::Indent,
                line,
                col: 1,
            });
        } else if indent < current {
            while self.indent_stack.len() > 1 && *self.indent_stack.last().unwrap() > indent {
                self.indent_stack.pop();
                tokens.push(Token {
                    kind: TokenKind::Dedent,
                    line,
                    col: 1,
                });
            }
            if *self.indent_stack.last().unwrap() != indent {
                return Err(self.error(format!("Invalid indentation level: {}", indent)));
            }
        }

        Ok(())
    }

    fn skip_comment(&mut self) {
        while self.pos < self.chars.len() && self.chars[self.pos] != '\n' {
            self.advance();
        }
    }

    fn scan_string(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        self.advance(); // consume opening "

        let mut parts: Vec<(bool, String)> = Vec::new(); // (is_expr, text)
        let mut current = String::new();
        let mut has_interp = false;

        loop {
            match self.current() {
                None => return Err(self.error("Unterminated string literal")),
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('{') => {
                    has_interp = true;
                    if !current.is_empty() {
                        parts.push((false, current.clone()));
                        current.clear();
                    }
                    self.advance(); // consume {
                    let mut expr_text = String::new();
                    let mut depth = 1usize;
                    while self.pos < self.chars.len() && depth > 0 {
                        match self.chars[self.pos] {
                            '{' => {
                                depth += 1;
                                expr_text.push('{');
                                self.advance();
                            }
                            '}' => {
                                depth -= 1;
                                if depth > 0 {
                                    expr_text.push('}');
                                }
                                self.advance();
                            }
                            c => {
                                expr_text.push(c);
                                self.advance();
                            }
                        }
                    }
                    parts.push((true, expr_text));
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => current.push('\n'),
                        Some('t') => current.push('\t'),
                        Some('r') => current.push('\r'),
                        Some('"') => current.push('"'),
                        Some('\\') => current.push('\\'),
                        Some(c) => current.push(c),
                        None => return Err(self.error("Unterminated string literal")),
                    }
                }
                Some('\n') => return Err(self.error("Unterminated string literal")),
                Some(c) => {
                    current.push(c);
                    self.advance();
                }
            }
        }

        if !current.is_empty() {
            parts.push((false, current));
        }

        if has_interp {
            Ok(Token {
                kind: TokenKind::InterpStr(parts),
                line,
                col,
            })
        } else {
            let plain = parts.into_iter().map(|(_, s)| s).collect::<String>();
            Ok(Token {
                kind: TokenKind::Str(plain),
                line,
                col,
            })
        }
    }

    fn scan_number(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        let mut num_str = String::new();
        let mut is_float = false;

        while let Some(c) = self.current() {
            if c.is_ascii_digit() {
                num_str.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if self.current() == Some('.') && self.peek(1).map(|c| c.is_ascii_digit()).unwrap_or(false)
        {
            is_float = true;
            num_str.push('.');
            self.advance(); // consume '.'
            while let Some(c) = self.current() {
                if c.is_ascii_digit() {
                    num_str.push(c);
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if is_float {
            let f: f64 = num_str
                .parse()
                .map_err(|_| self.error("Invalid floating-point number"))?;
            Ok(Token {
                kind: TokenKind::Float(f),
                line,
                col,
            })
        } else {
            let i: i64 = num_str
                .parse()
                .map_err(|_| self.error("Invalid integer literal"))?;
            Ok(Token {
                kind: TokenKind::Int(i),
                line,
                col,
            })
        }
    }

    fn scan_identifier(&mut self) -> Token {
        let line = self.line;
        let col = self.col;
        let mut ident = String::new();

        while let Some(c) = self.current() {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let kind = keyword(&ident).unwrap_or_else(|| TokenKind::Ident(ident));
        Token { kind, line, col }
    }

    fn scan_operator(&mut self) -> Result<Token, LexerError> {
        let line = self.line;
        let col = self.col;
        let ch = self.advance().unwrap();

        let kind = match ch {
            '-' if self.current() == Some('>') => {
                self.advance();
                TokenKind::Arrow
            }
            '|' if self.current() == Some('>') => {
                self.advance();
                TokenKind::Pipe
            }
            '=' if self.current() == Some('>') => {
                self.advance();
                TokenKind::FatArrow
            }
            '=' if self.current() == Some('=') => {
                self.advance();
                TokenKind::Eq
            }
            '!' if self.current() == Some('=') => {
                self.advance();
                TokenKind::Neq
            }
            '<' if self.current() == Some('=') => {
                self.advance();
                TokenKind::Lte
            }
            '>' if self.current() == Some('=') => {
                self.advance();
                TokenKind::Gte
            }
            '=' => TokenKind::Assign,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '!' => TokenKind::Bang,
            '?' => TokenKind::Question,
            '.' => TokenKind::Dot,
            ':' => TokenKind::Colon,
            ',' => TokenKind::Comma,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            other => return Err(self.error(format!("Unknown character: {:?}", other))),
        };

        Ok(Token { kind, line, col })
    }
}
