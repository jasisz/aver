use super::*;

impl Parser {
    fn is_upper_camel_segment(name: &str) -> bool {
        name.chars().next().is_some_and(|c| c.is_uppercase())
    }

    fn is_constructor_path(path: &str) -> bool {
        path.rsplit('.')
            .next()
            .is_some_and(Self::is_upper_camel_segment)
    }

    fn reject_zero_arg_constructor_call(&self, path: &str) -> Result<(), ParseError> {
        if Self::is_constructor_path(path) && self.peek(1).kind == TokenKind::RParen {
            return Err(self.error(format!(
                "Zero-argument constructor call '{}()' is not allowed. Use '{}' (no parentheses).",
                path, path
            )));
        }
        Ok(())
    }

    /// Wrap an Expr with the current token's line.
    fn spanned(&self, expr: Expr, line: usize) -> Spanned<Expr> {
        Spanned::new(expr, line)
    }

    pub fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        // Iron — B4: every recursive descent into a sub-expression
        // (LParen, LBrace, LBracket, Option.Some(...), nested
        // interpolation, …) re-enters `parse_expr`. Counting only
        // here is sufficient — `parse_unary`'s self-recursion is
        // bounded by the same chain (`-(-(-(x)))` parses through
        // `parse_unary → parse_postfix → parse_atom → LParen →
        // parse_expr` once the first non-`-` token appears).
        self.enter_recursion()?;
        let result = self.parse_comparison();
        self.exit_recursion();
        result
    }

    pub(super) fn parse_comparison(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut left = self.parse_additive()?;

        loop {
            let line = self.current().line;
            let op = match &self.current().kind {
                TokenKind::Eq => BinOp::Eq,
                TokenKind::Neq => BinOp::Neq,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::Lte => BinOp::Lte,
                TokenKind::Gte => BinOp::Gte,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            left = self.spanned(Expr::BinOp(op, Box::new(left), Box::new(right)), line);
        }

        Ok(left)
    }

    pub(super) fn parse_additive(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let line = self.current().line;
            let op = match &self.current().kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = self.spanned(Expr::BinOp(op, Box::new(left), Box::new(right)), line);
        }

        Ok(left)
    }

    pub(super) fn parse_multiplicative(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let line = self.current().line;
            let op = match &self.current().kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = self.spanned(Expr::BinOp(op, Box::new(left), Box::new(right)), line);
        }

        Ok(left)
    }

    pub(super) fn parse_unary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        if self.check_exact(&TokenKind::Minus) {
            let line = self.current().line;
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(self.spanned(Expr::Neg(Box::new(operand)), line));
        }
        self.parse_postfix()
    }

    pub(super) fn parse_postfix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut expr = self.parse_call_or_atom()?;

        loop {
            if self.check_exact(&TokenKind::Question) && self.peek(1).kind == TokenKind::Bang {
                // ?! on tuple: independent product with Result unwrapping
                let line = self.current().line;
                if let Expr::Tuple(elements) = expr.node {
                    self.advance(); // consume ?
                    self.advance(); // consume !
                    expr = self.spanned(Expr::IndependentProduct(elements, true), line);
                } else {
                    return Err(self.error(
                        "Operator '?!' can only be applied to a tuple expression, e.g. (a, b)?!"
                            .to_string(),
                    ));
                }
            } else if self.check_exact(&TokenKind::Bang)
                && !matches!(self.peek(1).kind, TokenKind::LBracket)
            {
                // ! on tuple: independent product without unwrapping
                // (but not `! [Effects]` which is an effect declaration)
                let line = self.current().line;
                if let Expr::Tuple(elements) = expr.node {
                    self.advance(); // consume !
                    expr = self.spanned(Expr::IndependentProduct(elements, false), line);
                } else {
                    break; // bare ! on non-tuple — not ours, leave for caller
                }
            } else if self.check_exact(&TokenKind::Question) {
                let line = self.current().line;
                self.advance();
                expr = self.spanned(Expr::ErrorProp(Box::new(expr)), line);
            } else if self.check_exact(&TokenKind::Dot) {
                let dot_line = self.current().line;
                self.advance();
                let field_tok = self.expect_kind(
                    &TokenKind::Ident(String::new()),
                    "Expected field name after '.'",
                )?;
                let field = match field_tok.kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };
                expr = self.spanned(Expr::Attr(Box::new(expr), field), dot_line);
                if self.check_exact(&TokenKind::LParen) {
                    // Detect `Type.update(base, field = val, ...)` for record update
                    if let Some(path) = Self::dotted_name(&expr)
                        && path.ends_with(".update")
                    {
                        let prefix = &path[..path.len() - ".update".len()];
                        if !prefix.is_empty()
                            && prefix.chars().next().is_some_and(|c| c.is_uppercase())
                        {
                            let update_line = self.current().line;
                            self.advance(); // consume (
                            let base = self.parse_expr()?;
                            let updates = if self.check_exact(&TokenKind::Comma) {
                                self.advance();
                                self.skip_formatting();
                                self.parse_record_create_fields()?
                            } else {
                                Vec::new()
                            };
                            self.expect_exact(&TokenKind::RParen)?;
                            expr = self.spanned(
                                Expr::RecordUpdate {
                                    type_name: prefix.to_string(),
                                    base: Box::new(base),
                                    updates,
                                },
                                update_line,
                            );
                            continue;
                        }
                    }
                    if let Some(path) = Self::dotted_name(&expr) {
                        self.reject_zero_arg_constructor_call(&path)?;
                    }
                    let named_arg_start = matches!(&self.peek(1).kind, TokenKind::Ident(_))
                        && self.peek(2).kind == TokenKind::Assign;
                    if named_arg_start && let Some(path) = Self::dotted_name(&expr) {
                        // Dotted record constructor: Tcp.Connection(id = ..., host = ...)
                        let ctor_line = self.current().line;
                        self.advance();
                        let fields = self.parse_record_create_fields()?;
                        self.expect_exact(&TokenKind::RParen)?;
                        expr = self.spanned(
                            Expr::RecordCreate {
                                type_name: path,
                                fields,
                            },
                            ctor_line,
                        );
                    } else {
                        let call_line = self.current().line;
                        self.advance();
                        let args = self.parse_args()?;
                        self.expect_exact(&TokenKind::RParen)?;
                        expr = self.spanned(Expr::FnCall(Box::new(expr), args), call_line);
                    }
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub(super) fn dotted_name(expr: &Spanned<Expr>) -> Option<String> {
        match &expr.node {
            Expr::Ident(name) => Some(name.clone()),
            Expr::Attr(inner, field) => {
                let mut base = Self::dotted_name(inner)?;
                base.push('.');
                base.push_str(field);
                Some(base)
            }
            _ => None,
        }
    }

    pub(super) fn parse_call_or_atom(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let atom = self.parse_atom()?;

        if self.check_exact(&TokenKind::LParen) {
            if let Some(path) = Self::dotted_name(&atom) {
                self.reject_zero_arg_constructor_call(&path)?;
            }

            // Lookahead: is this `Name(field = value, ...)` (record creation)?
            // Detect by checking if token after `(` is `Ident` followed by `=`.
            // Use peek_skip_formatting to handle multiline constructor syntax.
            let is_record_create = if let Expr::Ident(ref name) = atom.node {
                name.chars().next().is_some_and(|c| c.is_uppercase())
                    && matches!(&self.peek_skip_formatting(1).kind, TokenKind::Ident(_))
                    && self.peek_skip_formatting(2).kind == TokenKind::Assign
            } else {
                false
            };
            let named_arg_start = matches!(&self.peek_skip_formatting(1).kind, TokenKind::Ident(_))
                && self.peek_skip_formatting(2).kind == TokenKind::Assign;

            if is_record_create && let Expr::Ident(type_name) = atom.node {
                let line = atom.line;
                self.advance(); // consume (
                let fields = self.parse_record_create_fields()?;
                self.expect_exact(&TokenKind::RParen)?;
                return Ok(self.spanned(Expr::RecordCreate { type_name, fields }, line));
            }

            // Dotted record constructor: Tcp.Connection(id = ..., host = ...)
            if named_arg_start && let Some(path) = Self::dotted_name(&atom) {
                let line = atom.line;
                self.advance();
                let fields = self.parse_record_create_fields()?;
                self.expect_exact(&TokenKind::RParen)?;
                return Ok(self.spanned(
                    Expr::RecordCreate {
                        type_name: path,
                        fields,
                    },
                    line,
                ));
            }

            let call_line = self.current().line;
            self.advance();
            let args = self.parse_args()?;
            self.expect_exact(&TokenKind::RParen)?;
            return Ok(self.spanned(Expr::FnCall(Box::new(atom), args), call_line));
        }

        Ok(atom)
    }

    /// Parse named-field arguments for record creation: `name = expr, name2 = expr2`
    pub(super) fn parse_record_create_fields(
        &mut self,
    ) -> Result<Vec<(String, Spanned<Expr>)>, ParseError> {
        let mut fields = Vec::new();
        self.skip_formatting();

        while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                self.skip_formatting();
                continue;
            }
            let name_tok =
                self.expect_kind(&TokenKind::Ident(String::new()), "Expected field name")?;
            let field_name = match name_tok.kind {
                TokenKind::Ident(s) => s,
                _ => unreachable!(),
            };
            self.expect_exact(&TokenKind::Assign)?;
            let value = self.parse_expr()?;
            fields.push((field_name, value));
            self.skip_formatting();
        }

        Ok(fields)
    }

    pub(super) fn parse_args(&mut self) -> Result<Vec<Spanned<Expr>>, ParseError> {
        let mut args = Vec::new();
        self.skip_formatting();

        while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                self.skip_formatting();
                continue;
            }
            args.push(self.parse_expr()?);
            self.skip_formatting();
        }

        Ok(args)
    }

    pub(super) fn parse_map_literal(&mut self) -> Result<Expr, ParseError> {
        self.expect_exact(&TokenKind::LBrace)?;
        let mut entries = Vec::new();
        self.skip_formatting();

        while !self.check_exact(&TokenKind::RBrace) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                self.skip_formatting();
                continue;
            }

            let key = self.parse_expr()?;
            self.skip_formatting();
            if !self.check_exact(&TokenKind::FatArrow) {
                return Err(
                    self.error("Expected '=>' between key and value in map literal".to_string())
                );
            }
            self.advance(); // =>
            self.skip_formatting();
            let value = self.parse_expr()?;
            entries.push((key, value));
            self.skip_formatting();

            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                self.skip_formatting();
            }
        }

        self.expect_exact(&TokenKind::RBrace)?;
        Ok(Expr::MapLiteral(entries))
    }

    pub(super) fn parse_atom(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let line = self.current().line;
        match self.current().kind.clone() {
            TokenKind::Int(i) => {
                self.advance();
                Ok(self.spanned(Expr::Literal(Literal::Int(i)), line))
            }
            TokenKind::Float(f) => {
                self.advance();
                Ok(self.spanned(Expr::Literal(Literal::Float(f)), line))
            }
            TokenKind::Str(s) => {
                self.advance();
                Ok(self.spanned(Expr::Literal(Literal::Str(s)), line))
            }
            TokenKind::InterpStr(parts) => {
                self.advance();
                let mut str_parts = Vec::new();
                for (is_expr, s) in parts {
                    if is_expr {
                        // Parse the interpolation expression; empty `{}` → empty literal.
                        if s.trim().is_empty() {
                            str_parts.push(StrPart::Literal(String::new()));
                        } else {
                            let mut lexer = crate::lexer::Lexer::new(&s);
                            let tokens = lexer.tokenize().map_err(|e| ParseError::Error {
                                msg: format!("Error in interpolation: {}", e),
                                line: self.current().line,
                                col: self.current().col,
                            })?;
                            let mut sub_parser = Parser::new(tokens);
                            let expr = sub_parser.parse_expr().map_err(|e| ParseError::Error {
                                msg: format!("Error in interpolation: {}", e),
                                line: self.current().line,
                                col: self.current().col,
                            })?;
                            str_parts.push(StrPart::Parsed(Box::new(expr)));
                        }
                    } else {
                        str_parts.push(StrPart::Literal(s));
                    }
                }
                Ok(self.spanned(Expr::InterpolatedStr(str_parts), line))
            }
            TokenKind::Bool(b) => {
                self.advance();
                Ok(self.spanned(Expr::Literal(Literal::Bool(b)), line))
            }
            TokenKind::Match => {
                let m = self.parse_match()?;
                Ok(self.spanned(m, line))
            }
            TokenKind::LParen => {
                self.advance();
                let first = self.parse_expr()?;
                if self.check_exact(&TokenKind::Comma) {
                    let mut items = vec![first];
                    while self.check_exact(&TokenKind::Comma) {
                        self.advance();
                        items.push(self.parse_expr()?);
                    }
                    self.expect_exact(&TokenKind::RParen)?;
                    Ok(self.spanned(Expr::Tuple(items), line))
                } else {
                    self.expect_exact(&TokenKind::RParen)?;
                    Ok(first)
                }
            }
            TokenKind::Ident(s) => {
                self.advance();
                if s == "Unit" {
                    Ok(self.spanned(Expr::Literal(Literal::Unit), line))
                } else {
                    Ok(self.spanned(Expr::Ident(s), line))
                }
            }
            TokenKind::LBracket => {
                self.advance(); // consume [
                let mut elements = Vec::new();
                self.skip_formatting();
                while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
                    if self.check_exact(&TokenKind::Comma) {
                        self.advance();
                        self.skip_formatting();
                        continue;
                    }
                    elements.push(self.parse_expr()?);
                    self.skip_formatting();
                }
                self.expect_exact(&TokenKind::RBracket)?;
                Ok(self.spanned(Expr::List(elements), line))
            }
            TokenKind::LBrace => {
                let map = self.parse_map_literal()?;
                Ok(self.spanned(map, line))
            }
            TokenKind::Fn => Err(self.error(
                "Anonymous functions are not supported in Aver. Define a top-level function and pass its name."
                    .to_string(),
            )),
            _ => Err(self.error(format!(
                "Expected expression (identifier, literal, '[', or '{{'), found {}",
                self.current().kind
            ))),
        }
    }
}
