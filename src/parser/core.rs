use super::*;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            recursion_depth: 0,
        }
    }

    /// Iron — B4: bump the recursion counter and surface a normal
    /// parse error if it crosses [`MAX_PARSE_DEPTH`]. Callers MUST
    /// pair this with [`Self::exit_recursion`] on every return path
    /// (use `?` early-return + match-and-exit pattern, or hold a
    /// scope guard if the recursion grows multiple call sites).
    pub(super) fn enter_recursion(&mut self) -> Result<(), ParseError> {
        if self.recursion_depth >= super::MAX_PARSE_DEPTH {
            return Err(self.error(format!(
                "Expression too deeply nested (max {} levels). Refactor with named bindings or smaller sub-expressions.",
                super::MAX_PARSE_DEPTH
            )));
        }
        self.recursion_depth += 1;
        Ok(())
    }

    pub(super) fn exit_recursion(&mut self) {
        self.recursion_depth = self.recursion_depth.saturating_sub(1);
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
    /// `Some("<<")` / `Some(">>")` when the cursor sits on two ADJACENT `<`
    /// or `>` tokens — an attempted bit shift. Adjacency (same line, next
    /// column) is what separates it from a spaced comparison; the caller is
    /// responsible for only asking in operator position, where a nested
    /// generic's closing `>>` cannot appear.
    pub(super) fn attempted_shift_operator(&self) -> Option<&'static str> {
        let (first, second) = (self.current(), self.peek(1));
        if first.line != second.line || second.col != first.col + 1 {
            return None;
        }
        match (&first.kind, &second.kind) {
            (TokenKind::Lt, TokenKind::Lt) => Some("<<"),
            (TokenKind::Gt, TokenKind::Gt) => Some(">>"),
            _ => None,
        }
    }

    pub(super) fn peek(&self, offset: usize) -> &Token {
        let idx = self.pos + offset;
        if idx < self.tokens.len() {
            &self.tokens[idx]
        } else {
            self.tokens.last().unwrap()
        }
    }

    /// Peek at the nth non-formatting token (skips Newline/Indent/Dedent).
    pub(super) fn peek_skip_formatting(&self, nth: usize) -> &Token {
        let mut count = 0;
        let mut idx = self.pos;
        while idx < self.tokens.len() {
            if !matches!(
                self.tokens[idx].kind,
                TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
            ) {
                if count == nth {
                    return &self.tokens[idx];
                }
                count += 1;
            }
            idx += 1;
        }
        self.tokens.last().unwrap()
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
            Err(self.error(format!("{}, found {}", msg, self.current().kind)))
        }
    }

    pub(super) fn expect_exact(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        if &self.current().kind == kind {
            Ok(self.advance().clone())
        } else {
            Err(self.error(format!("Expected {}, found {}", kind, self.current().kind)))
        }
    }

    pub(super) fn skip_formatting(&mut self) {
        while matches!(
            self.current().kind,
            TokenKind::Newline | TokenKind::Indent | TokenKind::Dedent
        ) {
            self.advance();
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
            TokenKind::Effects => Err(self.error(
                "`effects [...]` is a module-level declaration — it must be \
                 indented inside a `module` block. For files without a module \
                 header, the per-fn `! [...]` annotations already cover the \
                 effect surface; drop the top-level `effects` line.",
            )),
            TokenKind::Ident(s) if s == "val" || s == "var" => {
                let kw = s.clone();
                Err(self.error(format!(
                    "Unknown keyword '{}'. Bindings are just: x = 5",
                    kw
                )))
            }
            // Three contextual words, recognised only here in item
            // position. Without these arms all three fall through to
            // `parse_expr` below, and because Aver has no juxtaposition
            // `operation open` and `opaque Token` become two adjacent
            // expression statements — a SILENT MISPARSE, not a failure.
            //
            // The `peek(1)` guards keep every one of them an ordinary
            // identifier in every other position: a file that binds
            // `operation = 1` or writes `opaque` as a bare expression
            // still parses exactly as it did before.
            TokenKind::Ident(s)
                if s == "operation" && matches!(&self.peek(1).kind, TokenKind::Ident(_)) =>
            {
                Ok(Some(TopLevel::Capability(CapabilityItem::Operation(
                    self.parse_operation()?,
                ))))
            }
            TokenKind::Ident(s)
                if s == "opaque" && matches!(&self.peek(1).kind, TokenKind::Ident(_)) =>
            {
                Ok(Some(TopLevel::Capability(self.parse_opaque_decl()?)))
            }
            // There is exactly one way to declare a capability, and it
            // is not this one. A `capability Foo` block would duplicate
            // exposes / exposes opaque / depends / visibility / name
            // resolution that a module already supplies. Refused
            // explicitly, because falling through would misparse it in
            // silence.
            TokenKind::Ident(s)
                if s == "capability" && matches!(&self.peek(1).kind, TokenKind::Ident(_)) =>
            {
                let name = match &self.peek(1).kind {
                    TokenKind::Ident(n) => n.clone(),
                    _ => unreachable!(),
                };
                Err(self.error(format!(
                    "A capability is a kind of module, not a declaration inside one. \
                     Write `kind = capability` in the module header of '{}' instead of \
                     `capability {}` — the module already supplies exposes, exposes opaque, \
                     depends and name resolution.",
                    name, name
                )))
            }
            TokenKind::Ident(_)
                if matches!(&self.peek(1).kind, TokenKind::Assign | TokenKind::Colon) =>
            {
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
