use super::*;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    pub(super) fn error(&self, msg: impl Into<String>) -> ParseError {
        let tok = self.current();
        ParseError::Error {
            msg: msg.into(),
            line: tok.line,
            col: tok.col,
        }
    }

    pub(super) fn current(&self) -> &Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            self.tokens.last().unwrap()
        }
    }

    #[allow(dead_code)]
    pub(super) fn peek(&self, offset: usize) -> &Token {
        let idx = self.pos + offset;
        if idx < self.tokens.len() {
            &self.tokens[idx]
        } else {
            self.tokens.last().unwrap()
        }
    }

    pub(super) fn advance(&mut self) -> &Token {
        let tok = if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            self.tokens.last().unwrap()
        };
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    pub(super) fn check_exact(&self, kind: &TokenKind) -> bool {
        &self.current().kind == kind
    }

    pub(super) fn is_newline(&self) -> bool {
        matches!(self.current().kind, TokenKind::Newline)
    }

    pub(super) fn is_indent(&self) -> bool {
        matches!(self.current().kind, TokenKind::Indent)
    }

    pub(super) fn is_dedent(&self) -> bool {
        matches!(self.current().kind, TokenKind::Dedent)
    }

    pub(super) fn is_eof(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    #[allow(dead_code)]
    pub(super) fn match_token(&mut self, kind: &TokenKind) -> Option<Token> {
        if std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind) {
            Some(self.advance().clone())
        } else {
            None
        }
    }

    pub(super) fn expect_kind(&mut self, kind: &TokenKind, msg: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind) {
            Ok(self.advance().clone())
        } else {
            Err(self.error(format!("{}: found {:?}", msg, self.current().kind)))
        }
    }

    pub(super) fn expect_exact(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        if &self.current().kind == kind {
            Ok(self.advance().clone())
        } else {
            Err(self.error(format!(
                "Expected {:?}, found {:?}",
                kind,
                self.current().kind
            )))
        }
    }

    pub(super) fn skip_newlines(&mut self) {
        while self.is_newline() {
            self.advance();
        }
    }

    pub fn parse(&mut self) -> Result<Vec<TopLevel>, ParseError> {
        let mut items = Vec::new();
        self.skip_newlines();

        while !self.is_eof() {
            if let Some(item) = self.parse_top_level()? {
                items.push(item);
            }
            self.skip_newlines();
        }

        Ok(items)
    }

    pub(super) fn parse_top_level(&mut self) -> Result<Option<TopLevel>, ParseError> {
        match &self.current().kind {
            TokenKind::Module => Ok(Some(TopLevel::Module(self.parse_module()?))),
            TokenKind::Fn => Ok(Some(TopLevel::FnDef(self.parse_fn()?))),
            TokenKind::Verify => Ok(Some(TopLevel::Verify(self.parse_verify()?))),
            TokenKind::Decision => Ok(Some(TopLevel::Decision(self.parse_decision()?))),
            TokenKind::Type => Ok(Some(TopLevel::TypeDef(self.parse_sum_type_def()?))),
            TokenKind::Record => Ok(Some(TopLevel::TypeDef(self.parse_record_def()?))),
            TokenKind::Effects => Ok(Some(self.parse_effect_set()?)),
            TokenKind::Ident(s) if s == "val" || s == "var" => {
                let kw = s.clone();
                Err(self.error(format!(
                    "Unknown keyword '{}'. Bindings are just: x = 5",
                    kw
                )))
            }
            TokenKind::Ident(_) if matches!(&self.peek(1).kind, TokenKind::Assign) => {
                let stmt = self.parse_binding()?;
                Ok(Some(TopLevel::Stmt(stmt)))
            }
            TokenKind::Newline | TokenKind::Dedent | TokenKind::Indent => {
                self.advance();
                Ok(None)
            }
            TokenKind::Eof => Ok(None),
            _ => {
                let expr = self.parse_expr()?;
                self.skip_newlines();
                Ok(Some(TopLevel::Stmt(Stmt::Expr(expr))))
            }
        }
    }
}
