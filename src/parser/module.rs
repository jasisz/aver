use super::*;

impl Parser {
    pub(super) fn parse_module(&mut self) -> Result<Module, ParseError> {
        let line = self.current().line;
        self.expect_exact(&TokenKind::Module)?;
        let name = self.expect_user_identifier("Expected module name", "module names")?;
        self.skip_newlines();

        let mut depends = Vec::new();
        let mut exposes = Vec::new();
        let mut exposes_opaque = Vec::new();
        let mut exposes_line = None;
        let mut intent = String::new();
        let mut effects = None;
        let mut effects_line = None;
        let mut kind = None;
        let mut kind_line = None;
        let mut semantics = None;
        let mut semantics_line = None;

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match &self.current().kind {
                    TokenKind::Intent => {
                        intent = self.parse_module_intent()?;
                    }
                    TokenKind::Exposes => {
                        exposes_line = Some(self.current().line);
                        self.advance(); // consume 'exposes'
                        if matches!(self.current().kind, TokenKind::Ident(ref s) if s == "opaque") {
                            self.advance(); // consume 'opaque'
                            exposes_opaque = self.parse_bracket_ident_list()?;
                        } else {
                            exposes = self.parse_bracket_ident_list()?;
                        }
                    }
                    TokenKind::Depends => {
                        depends = self.parse_depends()?;
                    }
                    TokenKind::Effects => {
                        effects_line = Some(self.current().line);
                        self.advance(); // consume 'effects'
                        let list = self
                            .parse_effect_ident_list()?
                            .into_iter()
                            .map(|s| s.node)
                            .collect();
                        effects = Some(list);
                    }
                    // `kind = capability` — the ONLY way to declare a
                    // capability. Contextual: `kind` stays an ordinary
                    // identifier everywhere else, and is only read here,
                    // in module-header field position. Shape copied from
                    // `intent =` above, the header's other `name = value`
                    // field.
                    TokenKind::Ident(s) if s == "kind" => {
                        kind_line = Some(self.current().line);
                        self.advance(); // consume 'kind'
                        self.expect_exact(&TokenKind::Assign)?;
                        let value_tok = self.expect_kind(
                            &TokenKind::Ident(String::new()),
                            "Expected a module kind after 'kind =' (the only kind today is `capability`)",
                        )?;
                        kind = match value_tok.kind {
                            TokenKind::Ident(s) => Some(s),
                            _ => unreachable!(),
                        };
                    }
                    TokenKind::Ident(s) if s == "semantics" => {
                        semantics_line = Some(self.current().line);
                        self.advance(); // consume 'semantics'
                        self.expect_exact(&TokenKind::Assign)?;
                        let value_tok = self.expect_kind(
                            &TokenKind::Ident(String::new()),
                            "Expected capability semantics after 'semantics =' (`pure` or `effectful`)",
                        )?;
                        semantics = match value_tok.kind {
                            TokenKind::Ident(s) => Some(s),
                            _ => unreachable!(),
                        };
                    }
                    // An unrecognised line that still has HEADER-FIELD
                    // SHAPE — an identifier followed by `=` or `[`, the
                    // two forms every real field takes. Breaking here
                    // would leave the DEDENT unconsumed and hand the
                    // line back to `parse_top_level`, where `kimd =
                    // capability` becomes an ordinary binding; with a
                    // resolvable right-hand side that produces no
                    // diagnostic at all, so a mistyped field would
                    // silently stop being a field. Refuse it, the way
                    // `parse_decision` refuses an unknown decision
                    // field.
                    TokenKind::Ident(_)
                        if matches!(
                            &self.peek(1).kind,
                            TokenKind::Assign | TokenKind::LBracket
                        ) =>
                    {
                        return Err(self.error(format!(
                            "Unknown module header field, found {}. Allowed: intent, kind, semantics, depends, exposes, effects. \
                             If you meant a top-level binding, unindent it — bindings live at column 0, outside the header.",
                            self.current().kind
                        )));
                    }
                    // Anything else ends the header and is re-read as a
                    // top-level item. An indented `fn` / `type` / block
                    // keyword cannot be confused with a header field, so
                    // the pre-existing shape keeps working.
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
            line,
            depends,
            exposes,
            exposes_opaque,
            exposes_line,
            intent,
            effects,
            effects_line,
            kind,
            kind_line,
            semantics,
            semantics_line,
        })
    }

    pub(super) fn parse_module_intent(&mut self) -> Result<String, ParseError> {
        self.expect_exact(&TokenKind::Intent)?;
        self.expect_exact(&TokenKind::Assign)?;
        let text = self.parse_inline_or_block_text()?;
        if text.is_empty() {
            return Err(self.error("Expected string or indented text after 'intent ='".to_string()));
        }
        Ok(text)
    }

    pub(super) fn parse_bracket_ident_list(&mut self) -> Result<Vec<String>, ParseError> {
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
                    "Expected identifier, found {}",
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
                        "Expected identifier after '.', found {}",
                        self.current().kind
                    )));
                }
            }
        }

        Ok(parts.join("."))
    }
}
