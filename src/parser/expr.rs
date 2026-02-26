use super::*;

impl Parser {
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_pipe()
    }

    /// Parse an expression for use inside verify cases.
    /// `==` is reserved as the separator between left and right sides.
    /// Other comparisons (`!=`, `<`, `>`, `<=`, `>=`) are allowed.
    pub(super) fn parse_pipe(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while self.check_exact(&TokenKind::Pipe) {
            self.advance();
            let right = self.parse_call_or_atom()?;
            left = Expr::Pipe(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub(super) fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_additive()?;

        loop {
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
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub(super) fn parse_additive(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let op = match &self.current().kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub(super) fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_unary()?;

        loop {
            let op = match &self.current().kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            left = Expr::BinOp(op, Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    pub(super) fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        if self.check_exact(&TokenKind::Minus) {
            self.advance();
            let operand = self.parse_postfix()?;
            return Ok(Expr::BinOp(
                BinOp::Sub,
                Box::new(Expr::Literal(Literal::Int(0))),
                Box::new(operand),
            ));
        }
        self.parse_postfix()
    }

    pub(super) fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_call_or_atom()?;

        loop {
            if self.check_exact(&TokenKind::Question) {
                self.advance();
                expr = Expr::ErrorProp(Box::new(expr));
            } else if self.check_exact(&TokenKind::Dot) {
                self.advance();
                let field_tok = self.expect_kind(
                    &TokenKind::Ident(String::new()),
                    "Expected field name after '.'",
                )?;
                let field = match field_tok.kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };
                expr = Expr::Attr(Box::new(expr), field);
                if self.check_exact(&TokenKind::LParen) {
                    let named_arg_start = matches!(&self.peek(1).kind, TokenKind::Ident(_))
                        && self.peek(2).kind == TokenKind::Assign;
                    if named_arg_start {
                        if let Some(path) = Self::dotted_name(&expr) {
                            if path == "Tcp.Connection" {
                                return Err(self.error(
                                    "Cannot construct 'Tcp.Connection' directly. Use Tcp.connect(host, port)."
                                        .to_string(),
                                ));
                            }
                            return Err(self.error(format!(
                                "Named-field call syntax is only valid for direct record constructors like User(...), not '{}(...)'",
                                path
                            )));
                        }
                    }
                    self.advance();
                    let args = self.parse_args()?;
                    self.expect_exact(&TokenKind::RParen)?;
                    expr = Expr::FnCall(Box::new(expr), args);
                }
            } else {
                break;
            }
        }

        Ok(expr)
    }

    pub(super) fn dotted_name(expr: &Expr) -> Option<String> {
        match expr {
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

    pub(super) fn parse_call_or_atom(&mut self) -> Result<Expr, ParseError> {
        let atom = self.parse_atom()?;

        if self.check_exact(&TokenKind::LParen) {
            // Lookahead: is this `Name(field = value, ...)` (record creation)?
            // Detect by checking if token after `(` is `Ident` followed by `=`.
            let is_record_create = if let Expr::Ident(ref name) = atom {
                name.chars().next().map_or(false, |c| c.is_uppercase())
                    && matches!(&self.peek(1).kind, TokenKind::Ident(_))
                    && self.peek(2).kind == TokenKind::Assign
            } else {
                false
            };
            let named_arg_start = matches!(&self.peek(1).kind, TokenKind::Ident(_))
                && self.peek(2).kind == TokenKind::Assign;

            if is_record_create {
                if let Expr::Ident(type_name) = atom {
                    self.advance(); // consume (
                    let fields = self.parse_record_create_fields()?;
                    self.expect_exact(&TokenKind::RParen)?;
                    return Ok(Expr::RecordCreate { type_name, fields });
                }
            }

            if named_arg_start {
                if let Some(path) = Self::dotted_name(&atom) {
                    if path == "Tcp.Connection" {
                        return Err(self.error(
                            "Cannot construct 'Tcp.Connection' directly. Use Tcp.connect(host, port)."
                                .to_string(),
                        ));
                    }
                    return Err(self.error(format!(
                        "Named-field call syntax is only valid for direct record constructors like User(...), not '{}(...)'",
                        path
                    )));
                }
            }

            self.advance();
            let args = self.parse_args()?;
            self.expect_exact(&TokenKind::RParen)?;
            return Ok(Expr::FnCall(Box::new(atom), args));
        }

        Ok(atom)
    }

    /// Parse named-field arguments for record creation: `name = expr, name2 = expr2`
    pub(super) fn parse_record_create_fields(&mut self) -> Result<Vec<(String, Expr)>, ParseError> {
        let mut fields = Vec::new();

        while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
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
        }

        Ok(fields)
    }

    pub(super) fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                continue;
            }
            args.push(self.parse_expr()?);
        }

        Ok(args)
    }

    pub(super) fn parse_atom(&mut self) -> Result<Expr, ParseError> {
        match self.current().kind.clone() {
            TokenKind::Int(i) => {
                self.advance();
                Ok(Expr::Literal(Literal::Int(i)))
            }
            TokenKind::Float(f) => {
                self.advance();
                Ok(Expr::Literal(Literal::Float(f)))
            }
            TokenKind::Str(s) => {
                self.advance();
                Ok(Expr::Literal(Literal::Str(s)))
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
                Ok(Expr::InterpolatedStr(str_parts))
            }
            TokenKind::Bool(b) => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(b)))
            }
            TokenKind::Match => self.parse_match(),
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
                    Ok(Expr::Tuple(items))
                } else {
                    self.expect_exact(&TokenKind::RParen)?;
                    Ok(first)
                }
            }
            TokenKind::Ident(s) => {
                self.advance();
                Ok(Expr::Ident(s))
            }
            TokenKind::LBracket => {
                self.advance(); // consume [
                let mut elements = Vec::new();
                while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
                    if self.check_exact(&TokenKind::Comma) {
                        self.advance();
                        continue;
                    }
                    elements.push(self.parse_expr()?);
                }
                self.expect_exact(&TokenKind::RBracket)?;
                Ok(Expr::List(elements))
            }
            _ => Err(self.error(format!(
                "Unexpected token in expression: {:?}",
                self.current().kind
            ))),
        }
    }
}
