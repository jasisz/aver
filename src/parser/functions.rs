use super::*;

impl Parser {
    pub(super) fn parse_fn(&mut self) -> Result<FnDef, ParseError> {
        let fn_line = self.current().line;
        self.expect_exact(&TokenKind::Fn)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected function name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };

        self.expect_exact(&TokenKind::LParen)?;
        let params = self.parse_params()?;
        self.expect_exact(&TokenKind::RParen)?;

        let mut return_type = String::from("Unit");
        if self.check_exact(&TokenKind::Arrow) {
            self.advance();
            return_type = self.parse_type()?;
        }

        if self.check_exact(&TokenKind::Assign) {
            return Err(self.error(
                "Function bodies no longer use '= expr'. Write the expression as an indented line directly.".to_string(),
            ));
        }

        self.skip_newlines();

        let mut desc = None;
        let mut effects = Vec::new();
        let mut body_stmts = Vec::new();

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            // Optional description: ? "..." with optional continuation lines.
            if self.check_exact(&TokenKind::Question) {
                self.advance();
                let text = self
                    .parse_inline_string_with_continuations()
                    .map_err(|_| self.error("Expected string after '?'".to_string()))?;
                desc = Some(text);
                self.skip_newlines();
            }

            // Optional effects: ! [Effect1, Effect2]
            if self.check_exact(&TokenKind::Bang) {
                self.advance();
                if self.check_exact(&TokenKind::LBracket) {
                    effects = self.parse_effect_ident_list()?;
                } else {
                    return Err(self.error("Expected '[' after '!'".to_string()));
                }
                self.skip_newlines();
            }

            // Parse body
            body_stmts = self.parse_fn_body()?;

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(FnDef {
            name,
            line: fn_line,
            params,
            return_type,
            effects,
            desc,
            body: Rc::new(FnBody::Block(body_stmts)),
            resolution: None,
        })
    }

    pub(super) fn parse_params(&mut self) -> Result<Vec<(String, String)>, ParseError> {
        let mut params = Vec::new();

        while !self.check_exact(&TokenKind::RParen) && !self.is_eof() {
            if self.check_exact(&TokenKind::Comma) {
                self.advance();
                continue;
            }

            let name_tok =
                self.expect_kind(&TokenKind::Ident(String::new()), "Expected parameter name")?;
            let param_name = match name_tok.kind {
                TokenKind::Ident(s) => s,
                _ => unreachable!(),
            };

            if !self.check_exact(&TokenKind::Colon) {
                return Err(self.error(format!(
                    "Expected ':' and type annotation for parameter '{}'",
                    param_name
                )));
            }
            self.advance();
            let param_type = self.parse_type()?;

            params.push((param_name, param_type));
        }

        Ok(params)
    }

    pub(super) fn parse_fn_body(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();

        while !self.is_dedent() && !self.is_eof() {
            if self.is_newline() {
                self.advance();
                continue;
            }

            if self.check_exact(&TokenKind::Assign) {
                return Err(self.error(
                    "Function bodies no longer use '= expr'. Write the expression as an indented line directly.".to_string(),
                ));
            } else if matches!(&self.current().kind, TokenKind::Ident(s) if s == "val" || s == "var")
            {
                let kw = match &self.current().kind {
                    TokenKind::Ident(s) => s.clone(),
                    _ => unreachable!(),
                };
                return Err(self.error(format!(
                    "Unknown keyword '{}'. Bindings are just: x = 5",
                    kw
                )));
            } else if self.check_exact(&TokenKind::Match) {
                let expr = self.parse_match()?;
                stmts.push(Stmt::Expr(expr));
            } else if matches!(&self.current().kind, TokenKind::Ident(_))
                && matches!(&self.peek(1).kind, TokenKind::Assign | TokenKind::Colon)
            {
                let stmt = self.parse_binding()?;
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr()?;
                stmts.push(Stmt::Expr(expr));
                self.skip_newlines();
            }
        }

        Ok(stmts)
    }

    // -------------------------------------------------------------------------
    // binding: `name = expr` or `name: Type = expr`
    // -------------------------------------------------------------------------
    pub(super) fn parse_binding(&mut self) -> Result<Stmt, ParseError> {
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected variable name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        let type_ann = if self.check_exact(&TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect_exact(&TokenKind::Assign)?;
        let value = self.parse_expr()?;
        self.skip_newlines();
        Ok(Stmt::Binding(name, type_ann, value))
    }
}
