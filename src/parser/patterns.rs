use super::*;

impl Parser {
    pub(super) fn parse_match(&mut self) -> Result<Expr, ParseError> {
        self.expect_exact(&TokenKind::Match)?;
        let subject = self.parse_expr()?;
        self.skip_newlines();

        let mut arms = Vec::new();

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                if self.is_newline() {
                    self.advance();
                    continue;
                }

                let pattern = self.parse_pattern()?;
                self.expect_exact(&TokenKind::Arrow)?;
                if self.is_newline() || self.is_indent() {
                    return Err(self.error(
                        "Match arm body must follow '->' on the same line. Extract complex expressions into a named function.".to_string()
                    ));
                }
                let body = self.parse_expr()?;
                arms.push(MatchArm::new(pattern, body));
                self.skip_newlines();
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(Expr::Match {
            subject: Box::new(subject),
            arms,
        })
    }

    pub(super) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        // Iron — B4 round 4: `parse_pattern` self-recurses through
        // its LParen / tuple arms with no depth bound. A pattern
        // like `(((((...x...)))))` then walks parse_pattern N times
        // before returning, which AFL turned into a real stack-
        // overflow input. Same guard as `parse_expr` — shares the
        // counter, so a `match` arm whose subject deeply nests
        // through `parse_expr` plus a pattern that adds more layers
        // hits the cap on whichever side blows it first.
        self.enter_recursion()?;
        let result = self.parse_pattern_inner();
        self.exit_recursion();
        result
    }

    fn parse_pattern_inner(&mut self) -> Result<Pattern, ParseError> {
        match self.current().kind.clone() {
            TokenKind::Ident(ref s) if s == "_" => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            TokenKind::LBracket => {
                self.advance(); // '['

                if self.check_exact(&TokenKind::RBracket) {
                    self.advance(); // ']'
                    return Ok(Pattern::EmptyList);
                }

                let head = if let TokenKind::Ident(s) = self.current().kind.clone() {
                    self.advance();
                    s
                } else {
                    return Err(self.error(format!(
                        "Expected identifier for list head in [head, ..tail] pattern, found {}",
                        self.current().kind
                    )));
                };

                self.expect_exact(&TokenKind::Comma)?;
                self.expect_exact(&TokenKind::Dot)?;
                self.expect_exact(&TokenKind::Dot)?;

                let tail = if let TokenKind::Ident(s) = self.current().kind.clone() {
                    self.advance();
                    s
                } else {
                    return Err(self.error(format!(
                        "Expected identifier for list tail in [head, ..tail] pattern, found {}",
                        self.current().kind
                    )));
                };

                self.expect_exact(&TokenKind::RBracket)?;
                Ok(Pattern::Cons(head, tail))
            }
            TokenKind::LParen => {
                self.advance(); // '('
                let first = self.parse_pattern()?;
                if self.check_exact(&TokenKind::Comma) {
                    let mut items = vec![first];
                    while self.check_exact(&TokenKind::Comma) {
                        self.advance();
                        items.push(self.parse_pattern()?);
                    }
                    self.expect_exact(&TokenKind::RParen)?;
                    Ok(Pattern::Tuple(items))
                } else {
                    self.expect_exact(&TokenKind::RParen)?;
                    Ok(first)
                }
            }
            // Constructor patterns must be qualified: Shape.Circle, Result.Ok,
            // Domain.Types.TaskEvent.TaskStarted, etc.
            TokenKind::Ident(ref s)
                if s.chars().next().is_some_and(|c| c.is_uppercase())
                    || matches!(self.peek(1).kind, TokenKind::Dot) =>
            {
                let name = self.parse_qualified_ident()?;
                if !crate::ast::dotted_name_spells_constructor(&name) {
                    return Err(self.error(format!(
                        "Constructor patterns must be qualified like 'Result.Ok(x)' or 'Shape.Circle(r)'. Bare UpperCamel patterns like '{}' are not supported; bind the whole value with a lower-case name and access record fields via '.'.",
                        name
                    )));
                }
                let mut bindings = vec![];
                if self.check_exact(&TokenKind::LParen) {
                    self.advance();
                    while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
                        if self.check_exact(&TokenKind::Comma) {
                            self.advance();
                            continue;
                        }
                        if let TokenKind::Ident(b) = self.current().kind.clone() {
                            bindings.push(b);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.expect_exact(&TokenKind::RParen)?;
                }
                Ok(Pattern::Constructor(name, bindings))
            }
            TokenKind::Ident(s) => {
                self.advance();
                Ok(Pattern::Ident(s))
            }
            TokenKind::Int(i) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Int(i)))
            }
            TokenKind::BigInt(_) => Err(self.error(
                "integer literal patterns beyond 64 bits are not supported — \
                 match on a comparison instead, e.g. `match n == 1267650600228229401496703205376`",
            )),
            TokenKind::Float(f) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Float(f)))
            }
            TokenKind::Str(s) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Str(s)))
            }
            TokenKind::Bool(b) => {
                self.advance();
                Ok(Pattern::Literal(Literal::Bool(b)))
            }
            _ => Err(self.error(format!(
                "Expected match pattern (identifier, literal, '[]', tuple, or constructor), found {}",
                self.current().kind
            ))),
        }
    }
}
