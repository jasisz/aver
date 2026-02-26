use super::*;

impl Parser {
    pub(super) fn parse_module(&mut self) -> Result<Module, ParseError> {
        self.expect_exact(&TokenKind::Module)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected module name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();

        let mut depends = Vec::new();
        let mut exposes = Vec::new();
        let mut intent = String::new();

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match &self.current().kind {
                    TokenKind::Intent => {
                        intent = self.parse_module_intent()?;
                    }
                    TokenKind::Exposes => {
                        exposes = self.parse_exposes()?;
                    }
                    TokenKind::Depends => {
                        depends = self.parse_depends()?;
                    }
                    _ => break,
                }
                self.skip_newlines();
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(Module {
            name,
            depends,
            exposes,
            intent,
        })
    }

    pub(super) fn parse_module_intent(&mut self) -> Result<String, ParseError> {
        self.expect_exact(&TokenKind::Intent)?;
        self.expect_exact(&TokenKind::Colon)?;
        self.skip_newlines();

        let mut parts = Vec::new();

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match &self.current().kind {
                    TokenKind::Str(s) => {
                        parts.push(s.clone());
                        self.advance();
                    }
                    TokenKind::Newline => {
                        self.advance();
                    }
                    _ => break,
                }
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(parts.join(" "))
    }

    pub(super) fn parse_exposes(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect_exact(&TokenKind::Exposes)?;
        self.expect_exact(&TokenKind::LBracket)?;
        let mut items = Vec::new();

        while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
            match self.current().kind.clone() {
                TokenKind::Ident(s) => {
                    items.push(s);
                    self.advance();
                }
                TokenKind::Comma => {
                    self.advance();
                }
                _ => break,
            }
        }

        self.expect_exact(&TokenKind::RBracket)?;
        self.skip_newlines();
        Ok(items)
    }

    pub(super) fn parse_depends(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect_exact(&TokenKind::Depends)?;
        self.expect_exact(&TokenKind::LBracket)?;
        let mut items = Vec::new();

        while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                continue;
            }
            items.push(self.parse_qualified_ident()?);
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
            }
        }

        self.expect_exact(&TokenKind::RBracket)?;
        self.skip_newlines();
        Ok(items)
    }

    pub(super) fn parse_qualified_ident(&mut self) -> Result<String, ParseError> {
        let first = match self.current().kind.clone() {
            TokenKind::Ident(s) => {
                self.advance();
                s
            }
            _ => {
                return Err(self.error(format!(
                    "Expected identifier, found {:?}",
                    self.current().kind
                )));
            }
        };
        let mut parts = vec![first];

        while self.check_exact(&TokenKind::Dot) {
            self.advance(); // '.'
            match self.current().kind.clone() {
                TokenKind::Ident(s) => {
                    parts.push(s);
                    self.advance();
                }
                _ => {
                    return Err(self.error(format!(
                        "Expected identifier after '.', found {:?}",
                        self.current().kind
                    )));
                }
            }
        }

        Ok(parts.join("."))
    }
}
