use std::fmt;
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
    Fn,
    Effects,
    Decision,
    Verify,
    Match,
    // Operators
    Arrow,    // ->
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
    LBrace,   // {
    RBrace,   // }
    // Structure
    Indent,
    Dedent,
    Newline,
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Int(n) => write!(f, "integer '{}'", n),
            TokenKind::Float(n) => write!(f, "float '{}'", n),
            TokenKind::Str(s) => write!(f, "string \"{}\"", s),
            TokenKind::InterpStr(_) => write!(f, "interpolated string"),
            TokenKind::Bool(b) => write!(f, "'{}'", b),
            TokenKind::Ident(s) => write!(f, "'{}'", s),
            TokenKind::Module => write!(f, "'module'"),
            TokenKind::Depends => write!(f, "'depends'"),
            TokenKind::Exposes => write!(f, "'exposes'"),
            TokenKind::Intent => write!(f, "'intent'"),
            TokenKind::Type => write!(f, "'type'"),
            TokenKind::Record => write!(f, "'record'"),
            TokenKind::Fn => write!(f, "'fn'"),
            TokenKind::Effects => write!(f, "'effects'"),
            TokenKind::Decision => write!(f, "'decision'"),
            TokenKind::Verify => write!(f, "'verify'"),
            TokenKind::Match => write!(f, "'match'"),
            TokenKind::Arrow => write!(f, "'->'"),
            TokenKind::FatArrow => write!(f, "'=>'"),
            TokenKind::Eq => write!(f, "'=='"),
            TokenKind::Neq => write!(f, "'!='"),
            TokenKind::Lte => write!(f, "'<='"),
            TokenKind::Gte => write!(f, "'>='"),
            TokenKind::Assign => write!(f, "'='"),
            TokenKind::Bang => write!(f, "'!'"),
            TokenKind::Question => write!(f, "'?'"),
            TokenKind::Lt => write!(f, "'<'"),
            TokenKind::Gt => write!(f, "'>'"),
            TokenKind::Plus => write!(f, "'+'"),
            TokenKind::Minus => write!(f, "'-'"),
            TokenKind::Star => write!(f, "'*'"),
            TokenKind::Slash => write!(f, "'/'"),
            TokenKind::Dot => write!(f, "'.'"),
            TokenKind::Colon => write!(f, "':'"),
            TokenKind::Comma => write!(f, "','"),
            TokenKind::LParen => write!(f, "'('"),
            TokenKind::RParen => write!(f, "')'"),
            TokenKind::LBracket => write!(f, "'['"),
            TokenKind::RBracket => write!(f, "']'"),
            TokenKind::LBrace => write!(f, "'{{'"),
            TokenKind::RBrace => write!(f, "'}}'"),
            TokenKind::Indent => write!(f, "indentation"),
            TokenKind::Dedent => write!(f, "end of block"),
            TokenKind::Newline => write!(f, "end of line"),
            TokenKind::Eof => write!(f, "end of file"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("error[{line}:{col}]: {msg}")]
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
        "fn" => Some(TokenKind::Fn),
        "effects" => Some(TokenKind::Effects),
        "decision" => Some(TokenKind::Decision),
        "verify" => Some(TokenKind::Verify),
        "match" => Some(TokenKind::Match),
        "true" => Some(TokenKind::Bool(true)),
        "false" => Some(TokenKind::Bool(false)),
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
                return Err(self.error(format!(
                    "Invalid indentation level: {indent}. Aver uses \
                     significant indentation with one consistent step per \
                     block — every line in the same block dedents back to \
                     a previously-opened indent level. Common cause: a \
                     wrapped `fn` signature or a multi-line argument list \
                     (Aver doesn't support either — keep each declaration \
                     on a single line, or split the body into a named \
                     helper function)."
                )));
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
                    // {{ → literal {, otherwise start interpolation
                    if self.chars.get(self.pos + 1).copied() == Some('{') {
                        current.push('{');
                        self.advance(); // first {
                        self.advance(); // second {
                    } else {
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
                }
                Some('}') => {
                    // }} → literal }, single } is just a literal character
                    if self.chars.get(self.pos + 1).copied() == Some('}') {
                        current.push('}');
                        self.advance(); // first }
                        self.advance(); // second }
                    } else {
                        current.push('}');
                        self.advance();
                    }
                }
                Some('\\') => {
                    self.advance();
                    match self.advance() {
                        Some('b') => current.push('\u{0008}'),
                        Some('f') => current.push('\u{000C}'),
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
            // Aver's `Int` is arbitrary-precision (ℤ) at runtime, but an integer
            // LITERAL is still lexed into an i64. A literal whose digits exceed the
            // 64-bit range is therefore rejected here — distinguish that (valid
            // digits, too big) from a genuinely malformed literal and point at the
            // working path (`Int.n("…")` builds a larger constant from a string)
            // instead of a cryptic "invalid literal".
            let i: i64 = match num_str.parse::<i64>() {
                Ok(v) => v,
                Err(e)
                    if matches!(
                        e.kind(),
                        std::num::IntErrorKind::PosOverflow | std::num::IntErrorKind::NegOverflow
                    ) =>
                {
                    return Err(self.error(&format!(
                        "integer literal '{num_str}' is too large for a 64-bit literal — \
                         Aver's Int is arbitrary-precision at runtime, so build a larger \
                         constant from a string with Int.n(\"{num_str}\")"
                    )));
                }
                Err(_) => return Err(self.error("Invalid integer literal")),
            };
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

        let kind = keyword(&ident).unwrap_or(TokenKind::Ident(ident));
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
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            other => return Err(self.error(format!("Unknown character: {:?}", other))),
        };

        Ok(Token { kind, line, col })
    }
}
