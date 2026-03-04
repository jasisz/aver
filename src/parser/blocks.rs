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
        let line = fn_name_tok.line;
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

        Ok(VerifyBlock {
            fn_name,
            line,
            cases,
        })
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
                if self.is_newline() {
                    self.advance();
                    continue;
                }

                let field_tok = self.expect_kind(
                    &TokenKind::Ident(String::new()),
                    "Expected decision field name",
                )?;
                let field_name = match field_tok.kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };

                self.expect_exact(&TokenKind::Assign)?;
                match field_name.as_str() {
                    "date" => {
                        if let TokenKind::Str(s) = self.current().kind.clone() {
                            date = s;
                            self.advance();
                        } else {
                            return Err(
                                self.error("Expected string value for decision 'date'".to_string())
                            );
                        }
                        self.skip_newlines();
                    }
                    "reason" => {
                        self.skip_newlines();
                        reason = self.parse_multiline_text()?;
                    }
                    "chosen" => {
                        if let TokenKind::Ident(s) = self.current().kind.clone() {
                            chosen = s;
                            self.advance();
                        } else {
                            return Err(self.error(
                                "Expected identifier value for decision 'chosen'".to_string(),
                            ));
                        }
                        self.skip_newlines();
                    }
                    "rejected" => {
                        rejected = self.parse_ident_list()?;
                        self.skip_newlines();
                    }
                    "impacts" => {
                        impacts = self.parse_ident_list()?;
                        self.skip_newlines();
                    }
                    "author" => {
                        if let TokenKind::Str(s) = self.current().kind.clone() {
                            author = Some(s);
                            self.advance();
                        } else {
                            return Err(self
                                .error("Expected string value for decision 'author'".to_string()));
                        }
                        self.skip_newlines();
                    }
                    _ => {
                        return Err(self.error(format!(
                            "Unknown decision field '{}'. Allowed: date, reason, chosen, rejected, impacts, author",
                            field_name
                        )));
                    }
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
