use super::*;

impl Parser {
    pub(super) fn parse_verify(&mut self) -> Result<VerifyBlock, ParseError> {
        self.expect_exact(&TokenKind::Verify)?;
        let fn_name_tok = self.expect_kind(
            &TokenKind::Ident(String::new()),
            "Expected function name in verify block",
        )?;
        let fn_name = match fn_name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();

        let mut cases = Vec::new();

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                if self.is_newline() {
                    self.advance();
                    continue;
                }

                let left = self.parse_pipe()?;
                self.expect_exact(&TokenKind::FatArrow)?;
                let right = self.parse_pipe()?;
                cases.push((left, right));
                self.skip_newlines();
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(VerifyBlock { fn_name, cases })
    }

    // -------------------------------------------------------------------------
    // Decision block
    // -------------------------------------------------------------------------
    pub(super) fn parse_decision(&mut self) -> Result<DecisionBlock, ParseError> {
        self.expect_exact(&TokenKind::Decision)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected decision name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();

        let mut date = String::new();
        let mut reason = String::new();
        let mut chosen = String::new();
        let mut rejected = Vec::new();
        let mut impacts = Vec::new();
        let mut author = None;

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match self.current().kind.clone() {
                    TokenKind::Newline => {
                        self.advance();
                    }
                    TokenKind::Date => {
                        self.advance();
                        self.expect_exact(&TokenKind::Assign)?;
                        if let TokenKind::Str(s) = self.current().kind.clone() {
                            date = s;
                            self.advance();
                        }
                        self.skip_newlines();
                    }
                    TokenKind::Reason => {
                        self.advance();
                        self.expect_exact(&TokenKind::Assign)?;
                        self.skip_newlines();
                        reason = self.parse_multiline_text()?;
                    }
                    TokenKind::Chosen => {
                        self.advance();
                        self.expect_exact(&TokenKind::Assign)?;
                        if let TokenKind::Ident(s) = self.current().kind.clone() {
                            chosen = s;
                            self.advance();
                        }
                        self.skip_newlines();
                    }
                    TokenKind::Rejected => {
                        self.advance();
                        self.expect_exact(&TokenKind::Assign)?;
                        rejected = self.parse_ident_list()?;
                        self.skip_newlines();
                    }
                    TokenKind::Impacts => {
                        self.advance();
                        self.expect_exact(&TokenKind::Assign)?;
                        impacts = self.parse_ident_list()?;
                        self.skip_newlines();
                    }
                    TokenKind::Author => {
                        self.advance();
                        self.expect_exact(&TokenKind::Assign)?;
                        if let TokenKind::Str(s) = self.current().kind.clone() {
                            author = Some(s);
                            self.advance();
                        }
                        self.skip_newlines();
                    }
                    _ => break,
                }
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(DecisionBlock {
            name,
            date,
            reason,
            chosen,
            rejected,
            impacts,
            author,
        })
    }

    pub(super) fn parse_multiline_text(&mut self) -> Result<String, ParseError> {
        let mut parts = Vec::new();

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match self.current().kind.clone() {
                    TokenKind::Str(s) => {
                        parts.push(s);
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

    pub(super) fn parse_ident_list(&mut self) -> Result<Vec<String>, ParseError> {
        let mut items = Vec::new();

        if self.check_exact(&TokenKind::LBracket) {
            self.advance();
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
        } else if let TokenKind::Ident(s) = self.current().kind.clone() {
            items.push(s);
            self.advance();
        }

        Ok(items)
    }
}
