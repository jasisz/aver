use super::*;

/// Read the head of one match-arm line: `line` (indentation stripped) is
/// `pattern -> body`; the result is the arm's pattern and the char index
/// where `->` starts. `None` for anything else — a line with no `->`, or
/// whose text before `->` is not exactly one pattern. The formatter uses
/// it to print arm patterns in their canonical spelling.
pub fn parse_match_arm_head(line: &str) -> Option<(Pattern, usize)> {
    let tokens = crate::lexer::Lexer::new(line).tokenize().ok()?;
    let arrow = tokens
        .iter()
        .position(|token| token.kind == TokenKind::Arrow)?;
    let arrow_col = tokens[arrow].col;
    let mut head: Vec<Token> = tokens[..arrow].to_vec();
    head.push(Token {
        kind: TokenKind::Eof,
        line: tokens[arrow].line,
        col: arrow_col,
    });
    let mut parser = Parser::new(head);
    let pattern = parser.parse_pattern().ok()?;
    if !parser.is_eof() {
        return None;
    }
    Some((pattern, arrow_col.checked_sub(1)?))
}

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

                // `[p1, p2, ..rest]`, `[p1, p2]`, `[..rest]`: element
                // patterns separated by commas, optionally closed by one
                // `..binder` for the remaining list.
                let mut items = Vec::new();
                let mut rest = None;
                loop {
                    if self.check_exact(&TokenKind::Dot) {
                        self.advance();
                        self.expect_exact(&TokenKind::Dot)?;
                        rest = Some(self.expect_user_identifier(
                            "Expected identifier after '..' in list pattern like [head, ..tail]",
                            "pattern binders",
                        )?);
                        if !self.check_exact(&TokenKind::RBracket) {
                            return Err(self.error(
                                "'..rest' must be the last part of a list pattern, like [a, b, ..rest]"
                                    .to_string(),
                            ));
                        }
                        break;
                    }
                    items.push(self.parse_pattern()?);
                    if self.check_exact(&TokenKind::Comma) {
                        self.advance();
                        continue;
                    }
                    break;
                }
                self.expect_exact(&TokenKind::RBracket)?;

                // `[head, ..tail]` with two binders keeps its original flat
                // form; every other shape is the general list pattern.
                if let (Some(tail), [single]) = (&rest, items.as_slice()) {
                    match single {
                        Pattern::Ident(head) => return Ok(Pattern::Cons(head.clone(), tail.clone())),
                        Pattern::Wildcard => {
                            return Ok(Pattern::Cons("_".to_string(), tail.clone()));
                        }
                        _ => {}
                    }
                }
                Ok(Pattern::List { items, rest })
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
                if crate::ast::name_is_type_like(s)
                    || matches!(self.peek(1).kind, TokenKind::Dot) =>
            {
                let name = self.parse_qualified_ident()?;
                if !crate::ast::dotted_name_spells_constructor(&name) {
                    return Err(self.error(format!(
                        "Constructor patterns must be qualified like 'Result.Ok(x)' or 'Shape.Circle(r)'. Bare UpperCamel patterns like '{}' are not supported; bind the whole value with a lower-case name and access record fields via '.'.",
                        name
                    )));
                }
                let mut fields = vec![];
                if self.check_exact(&TokenKind::LParen) {
                    self.advance();
                    while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
                        fields.push(self.parse_pattern()?);
                        if !self.check_exact(&TokenKind::Comma) {
                            break;
                        }
                        self.advance();
                    }
                    self.expect_exact(&TokenKind::RParen)?;
                }
                // Fields that are all binders (or `_`) keep the flat form
                // every backend reads; any literal, constructor, tuple or
                // list field makes it a nested constructor pattern.
                let binders: Option<Vec<String>> = fields
                    .iter()
                    .map(|field| match field {
                        Pattern::Ident(name) => Some(name.clone()),
                        Pattern::Wildcard => Some("_".to_string()),
                        _ => None,
                    })
                    .collect();
                match binders {
                    Some(bindings) => Ok(Pattern::Constructor(name, bindings)),
                    None => Ok(Pattern::ConstructorNested(name, fields)),
                }
            }
            TokenKind::Ident(_) => Ok(Pattern::Ident(self.expect_user_identifier(
                "Expected match pattern identifier",
                "pattern binders",
            )?)),
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
                "Expected match pattern (identifier, literal, list, tuple, or constructor), found {}",
                self.current().kind
            ))),
        }
    }
}
