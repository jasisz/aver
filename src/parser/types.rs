use super::*;

impl Parser {
    pub(super) fn parse_effect_set(&mut self) -> Result<TopLevel, ParseError> {
        self.expect_exact(&TokenKind::Effects)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected effect set name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Assign)?;
        self.expect_exact(&TokenKind::LBracket)?;
        let mut effects = Vec::new();
        while !matches!(self.current().kind, TokenKind::RBracket | TokenKind::Eof) {
            let tok = self.expect_kind(&TokenKind::Ident(String::new()), "Expected effect name")?;
            if let TokenKind::Ident(eff) = tok.kind {
                effects.push(eff);
            }
            if matches!(self.current().kind, TokenKind::Comma) {
                self.advance();
            }
        }
        self.expect_exact(&TokenKind::RBracket)?;
        self.skip_newlines();
        Ok(TopLevel::EffectSet { name, effects })
    }

    pub(super) fn parse_sum_type_def(&mut self) -> Result<TypeDef, ParseError> {
        self.expect_exact(&TokenKind::Type)?;
        let name_tok = self.expect_kind(&TokenKind::Ident(String::new()), "Expected type name")?;
        let type_name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();

        let mut variants = Vec::new();

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                if self.is_newline() {
                    self.advance();
                    continue;
                }

                let variant_tok =
                    self.expect_kind(&TokenKind::Ident(String::new()), "Expected variant name")?;
                let variant_name = match variant_tok.kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };

                let mut fields = Vec::new();
                if self.check_exact(&TokenKind::LParen) {
                    self.advance();
                    while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
                        if self.check_exact(&TokenKind::Comma) {
                            self.advance();
                            continue;
                        }
                        let ty = self.parse_type()?;
                        fields.push(ty);
                    }
                    self.expect_exact(&TokenKind::RParen)?;
                }

                variants.push(TypeVariant {
                    name: variant_name,
                    fields,
                });
                self.skip_newlines();
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(TypeDef::Sum {
            name: type_name,
            variants,
        })
    }

    pub(super) fn parse_record_def(&mut self) -> Result<TypeDef, ParseError> {
        self.expect_exact(&TokenKind::Record)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected record name")?;
        let type_name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();

        let mut fields = Vec::new();

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                if self.is_newline() {
                    self.advance();
                    continue;
                }

                let field_tok =
                    self.expect_kind(&TokenKind::Ident(String::new()), "Expected field name")?;
                let field_name = match field_tok.kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };
                self.expect_exact(&TokenKind::Colon)?;
                let field_type = self.parse_type()?;
                fields.push((field_name, field_type));
                self.skip_newlines();
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(TypeDef::Product {
            name: type_name,
            fields,
        })
    }

    pub(super) fn parse_type(&mut self) -> Result<String, ParseError> {
        if self.check_exact(&TokenKind::LParen) {
            self.advance(); // (
            let mut elems = Vec::new();
            while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
                if self.check_exact(&TokenKind::Comma) {
                    self.advance();
                    continue;
                }
                elems.push(self.parse_type()?);
                if self.check_exact(&TokenKind::Comma) {
                    self.advance();
                }
            }
            self.expect_exact(&TokenKind::RParen)?;
            if elems.len() < 2 {
                return Err(self.error("Tuple type must have at least 2 elements".to_string()));
            }
            return Ok(format!("({})", elems.join(", ")));
        }

        // Function type annotation: Fn(A, B) -> C ! [Effect]
        if let TokenKind::Ident(name) = &self.current().kind {
            if name == "Fn" && matches!(self.peek(1).kind, TokenKind::LParen) {
                self.advance(); // Fn
                self.expect_exact(&TokenKind::LParen)?;

                let mut params = Vec::new();
                while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
                    if self.check_exact(&TokenKind::Comma) {
                        self.advance();
                        continue;
                    }
                    params.push(self.parse_type()?);
                    if self.check_exact(&TokenKind::Comma) {
                        self.advance();
                    }
                }
                self.expect_exact(&TokenKind::RParen)?;
                self.expect_exact(&TokenKind::Arrow)?;

                let ret = self.parse_type()?;
                let mut out = format!("Fn({}) -> {}", params.join(", "), ret);

                if self.check_exact(&TokenKind::Bang) {
                    self.advance(); // !
                    let effects = self.parse_effect_ident_list()?;
                    out.push_str(&format!(" ! [{}]", effects.join(", ")));
                }
                return Ok(out);
            }
        }

        let mut base = match &self.current().kind {
            TokenKind::Ident(s) => {
                let s = s.clone();
                self.advance();
                if s == "Any" {
                    return Err(self.error(
                        "Type 'Any' has been removed. Use an explicit concrete type.".to_string(),
                    ));
                }
                s
            }
            _ => {
                return Err(self.error(format!(
                    "Expected a type name, found {:?}",
                    self.current().kind
                )));
            }
        };

        // Dotted type name: e.g. Tcp.Connection
        if self.check_exact(&TokenKind::Dot) {
            if let Some(Token {
                kind: TokenKind::Ident(next),
                ..
            }) = self.tokens.get(self.pos + 1)
            {
                if next.chars().next().map_or(false, |c| c.is_uppercase()) {
                    let next = next.clone();
                    self.advance(); // dot
                    self.advance(); // ident
                    base = format!("{}.{}", base, next);
                }
            }
        }

        if self.check_exact(&TokenKind::Lt) {
            self.advance(); // <
            let mut args = Vec::new();
            while !self.check_exact(&TokenKind::Gt) && !self.is_eof() {
                if self.check_exact(&TokenKind::Comma) {
                    self.advance();
                    continue;
                }
                args.push(self.parse_type()?);
                if self.check_exact(&TokenKind::Comma) {
                    self.advance();
                }
            }
            self.expect_exact(&TokenKind::Gt)?;
            base = format!("{}<{}>", base, args.join(", "));
        }

        Ok(base)
    }

    pub(super) fn parse_effect_ident_list(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect_exact(&TokenKind::LBracket)?;
        let mut effects = Vec::new();
        while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
            match self.current().kind.clone() {
                TokenKind::Ident(s) => {
                    effects.push(s);
                    self.advance();
                }
                TokenKind::Comma => {
                    self.advance();
                }
                _ => {
                    return Err(self.error(format!(
                        "Expected effect name in type annotation, found {:?}",
                        self.current().kind
                    )));
                }
            }
        }
        self.expect_exact(&TokenKind::RBracket)?;
        Ok(effects)
    }
}
