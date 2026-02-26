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
                let body = self.parse_expr()?;
                arms.push(MatchArm {
                    pattern,
                    body: Box::new(body),
                });
                self.skip_newlines();
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(Expr::Match(Box::new(subject), arms))
    }

    pub(super) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
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
                        "Expected list head binding in pattern, found {:?}",
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
                        "Expected list tail binding in pattern, found {:?}",
                        self.current().kind
                    )));
                };

                self.expect_exact(&TokenKind::RBracket)?;
                Ok(Pattern::Cons(head, tail))
            }
            // User-defined constructor: starts with uppercase (e.g. Shape.Circle, Point)
            TokenKind::Ident(ref s) if s.chars().next().map_or(false, |c| c.is_uppercase()) => {
                let mut name = s.clone();
                self.advance();
                // Qualified constructor: Shape.Circle
                if self.check_exact(&TokenKind::Dot) {
                    if let TokenKind::Ident(ref variant) = self.peek(1).kind.clone() {
                        if variant.chars().next().map_or(false, |c| c.is_uppercase()) {
                            let variant = variant.clone();
                            self.advance(); // consume '.'
                            self.advance(); // consume variant name
                            name = format!("{}.{}", name, variant);
                        }
                    }
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
                "Unexpected token in pattern: {:?}",
                self.current().kind
            ))),
        }
    }
}
