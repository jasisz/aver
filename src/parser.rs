use crate::ast::*;
use crate::lexer::{Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Parse error [{line}:{col}]: {msg}")]
    Error {
        msg: String,
        line: usize,
        col: usize,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn error(&self, msg: impl Into<String>) -> ParseError {
        let tok = self.current();
        ParseError::Error {
            msg: msg.into(),
            line: tok.line,
            col: tok.col,
        }
    }

    fn current(&self) -> &Token {
        if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            self.tokens.last().unwrap()
        }
    }

    #[allow(dead_code)]
    fn peek(&self, offset: usize) -> &Token {
        let idx = self.pos + offset;
        if idx < self.tokens.len() {
            &self.tokens[idx]
        } else {
            self.tokens.last().unwrap()
        }
    }

    fn advance(&mut self) -> &Token {
        let tok = if self.pos < self.tokens.len() {
            &self.tokens[self.pos]
        } else {
            self.tokens.last().unwrap()
        };
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        tok
    }

    #[allow(dead_code)]
    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind)
    }

    fn check_exact(&self, kind: &TokenKind) -> bool {
        &self.current().kind == kind
    }

    fn is_newline(&self) -> bool {
        matches!(self.current().kind, TokenKind::Newline)
    }

    fn is_indent(&self) -> bool {
        matches!(self.current().kind, TokenKind::Indent)
    }

    fn is_dedent(&self) -> bool {
        matches!(self.current().kind, TokenKind::Dedent)
    }

    fn is_eof(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    #[allow(dead_code)]
    fn match_token(&mut self, kind: &TokenKind) -> Option<Token> {
        if std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind) {
            Some(self.advance().clone())
        } else {
            None
        }
    }

    fn expect_kind(&mut self, kind: &TokenKind, msg: &str) -> Result<Token, ParseError> {
        if std::mem::discriminant(&self.current().kind) == std::mem::discriminant(kind) {
            Ok(self.advance().clone())
        } else {
            Err(self.error(format!("{}: found {:?}", msg, self.current().kind)))
        }
    }

    fn expect_exact(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        if &self.current().kind == kind {
            Ok(self.advance().clone())
        } else {
            Err(self.error(format!(
                "Expected {:?}, found {:?}",
                kind,
                self.current().kind
            )))
        }
    }

    fn skip_newlines(&mut self) {
        while self.is_newline() {
            self.advance();
        }
    }

    pub fn parse(&mut self) -> Result<Vec<TopLevel>, ParseError> {
        let mut items = Vec::new();
        self.skip_newlines();

        while !self.is_eof() {
            if let Some(item) = self.parse_top_level()? {
                items.push(item);
            }
            self.skip_newlines();
        }

        Ok(items)
    }

    fn parse_top_level(&mut self) -> Result<Option<TopLevel>, ParseError> {
        match &self.current().kind {
            TokenKind::Module => Ok(Some(TopLevel::Module(self.parse_module()?))),
            TokenKind::Fn => Ok(Some(TopLevel::FnDef(self.parse_fn()?))),
            TokenKind::Verify => Ok(Some(TopLevel::Verify(self.parse_verify()?))),
            TokenKind::Decision => Ok(Some(TopLevel::Decision(self.parse_decision()?))),
            TokenKind::Type => Ok(Some(TopLevel::TypeDef(self.parse_sum_type_def()?))),
            TokenKind::Record => Ok(Some(TopLevel::TypeDef(self.parse_record_def()?))),
            TokenKind::Val => {
                let stmt = self.parse_val()?;
                Ok(Some(TopLevel::Stmt(stmt)))
            }
            TokenKind::Var => {
                let stmt = self.parse_var()?;
                Ok(Some(TopLevel::Stmt(stmt)))
            }
            TokenKind::Ident(_) if matches!(&self.peek(1).kind, TokenKind::Assign) => {
                let stmt = self.parse_assign()?;
                Ok(Some(TopLevel::Stmt(stmt)))
            }
            TokenKind::Newline | TokenKind::Dedent | TokenKind::Indent => {
                self.advance();
                Ok(None)
            }
            TokenKind::Eof => Ok(None),
            _ => {
                let expr = self.parse_expr()?;
                self.skip_newlines();
                Ok(Some(TopLevel::Stmt(Stmt::Expr(expr))))
            }
        }
    }

    // -------------------------------------------------------------------------
    // Sum type definition: `type Shape` with indented variants
    // -------------------------------------------------------------------------
    fn parse_sum_type_def(&mut self) -> Result<TypeDef, ParseError> {
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

    // -------------------------------------------------------------------------
    // Record (product type) definition: `record User` with `name: Type` fields
    // -------------------------------------------------------------------------
    fn parse_record_def(&mut self) -> Result<TypeDef, ParseError> {
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

    // -------------------------------------------------------------------------
    // Module
    // -------------------------------------------------------------------------
    fn parse_module(&mut self) -> Result<Module, ParseError> {
        self.expect_exact(&TokenKind::Module)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected module name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();

        let mut depends = Vec::new();
        let mut exposes = Vec::new();
        let mut intent = String::new();

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match &self.current().kind {
                    TokenKind::Intent => {
                        intent = self.parse_module_intent()?;
                    }
                    TokenKind::Exposes => {
                        exposes = self.parse_exposes()?;
                    }
                    TokenKind::Depends => {
                        depends = self.parse_depends()?;
                    }
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
            depends,
            exposes,
            intent,
        })
    }

    fn parse_module_intent(&mut self) -> Result<String, ParseError> {
        self.expect_exact(&TokenKind::Intent)?;
        self.expect_exact(&TokenKind::Colon)?;
        self.skip_newlines();

        let mut parts = Vec::new();

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            while !self.is_dedent() && !self.is_eof() {
                match &self.current().kind {
                    TokenKind::Str(s) => {
                        parts.push(s.clone());
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

    fn parse_exposes(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect_exact(&TokenKind::Exposes)?;
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

    fn parse_depends(&mut self) -> Result<Vec<String>, ParseError> {
        self.expect_exact(&TokenKind::Depends)?;
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

    // -------------------------------------------------------------------------
    // Function definition
    // -------------------------------------------------------------------------
    fn parse_fn(&mut self) -> Result<FnDef, ParseError> {
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

        self.skip_newlines();

        let mut desc = None;
        let mut effects = Vec::new();
        let mut body_stmts = Vec::new();
        let mut body_expr = None;

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            // Optional description: ? "..."
            if self.check_exact(&TokenKind::Question) {
                self.advance();
                if let TokenKind::Str(s) = self.current().kind.clone() {
                    desc = Some(s);
                    self.advance();
                }
                self.skip_newlines();
            }

            // Optional effects: ! [Effect1, Effect2]
            if self.check_exact(&TokenKind::Bang) {
                self.advance();
                if self.check_exact(&TokenKind::LBracket) {
                    self.advance();
                    while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
                        match self.current().kind.clone() {
                            TokenKind::Ident(s) => {
                                effects.push(s);
                                self.advance();
                            }
                            TokenKind::Comma => {
                                self.advance();
                            }
                            _ => break,
                        }
                    }
                    self.expect_exact(&TokenKind::RBracket)?;
                }
                self.skip_newlines();
            }

            // Parse body
            (body_expr, body_stmts) = self.parse_fn_body()?;

            if self.is_dedent() {
                self.advance();
            }
        }

        let body = match body_expr {
            Some(e) => FnBody::Expr(e),
            None => FnBody::Block(body_stmts),
        };

        Ok(FnDef {
            name,
            params,
            return_type,
            effects,
            desc,
            body,
        })
    }

    fn parse_params(&mut self) -> Result<Vec<(String, String)>, ParseError> {
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

            let mut param_type = String::from("Any");
            if self.check_exact(&TokenKind::Colon) {
                self.advance();
                param_type = self.parse_type()?;
            }

            params.push((param_name, param_type));
        }

        Ok(params)
    }

    fn parse_type(&mut self) -> Result<String, ParseError> {
        let base = match &self.current().kind {
            TokenKind::Ident(s) => {
                let s = s.clone();
                self.advance();
                s
            }
            TokenKind::Ok => {
                self.advance();
                "Ok".to_string()
            }
            TokenKind::Err => {
                self.advance();
                "Err".to_string()
            }
            TokenKind::Some => {
                self.advance();
                "Some".to_string()
            }
            TokenKind::None => {
                self.advance();
                "None".to_string()
            }
            _ => {
                return Err(self.error(format!(
                    "Expected a type name, found {:?}",
                    self.current().kind
                )));
            }
        };

        if self.check_exact(&TokenKind::Lt) {
            self.advance();
            let mut generic_parts = vec![base, "<".to_string()];
            let mut depth = 1usize;
            let start_tok = self.current().clone();

            while depth > 0 && !self.is_eof() {
                match &self.current().kind {
                    TokenKind::Lt => {
                        depth += 1;
                        generic_parts.push("<".to_string());
                        self.advance();
                    }
                    TokenKind::Gt => {
                        depth -= 1;
                        if depth > 0 {
                            generic_parts.push(">".to_string());
                        }
                        self.advance();
                    }
                    TokenKind::Comma => {
                        generic_parts.push(", ".to_string());
                        self.advance();
                    }
                    TokenKind::Ident(s) => {
                        generic_parts.push(s.clone());
                        self.advance();
                    }
                    _ => {
                        return Err(self.error(format!(
                            "Unexpected token in generic type: {:?}",
                            self.current().kind
                        )));
                    }
                }
            }

            if depth != 0 {
                return Err(ParseError::Error {
                    msg: "Unterminated generic type annotation".to_string(),
                    line: start_tok.line,
                    col: start_tok.col,
                });
            }

            generic_parts.push(">".to_string());
            return Ok(generic_parts.join(""));
        }

        Ok(base)
    }

    fn parse_fn_body(&mut self) -> Result<(Option<Expr>, Vec<Stmt>), ParseError> {
        let mut stmts = Vec::new();

        while !self.is_dedent() && !self.is_eof() {
            if self.is_newline() {
                self.advance();
                continue;
            }

            if self.check_exact(&TokenKind::Assign) {
                // Single-expression return: = expr
                self.advance();
                let expr = self.parse_expr()?;
                self.skip_newlines();
                return Ok((Some(expr), Vec::new()));
            } else if self.check_exact(&TokenKind::Val) {
                let stmt = self.parse_val()?;
                stmts.push(stmt);
            } else if self.check_exact(&TokenKind::Var) {
                let stmt = self.parse_var()?;
                stmts.push(stmt);
            } else if self.check_exact(&TokenKind::Match) {
                let expr = self.parse_match()?;
                stmts.push(Stmt::Expr(expr));
            } else if matches!(&self.current().kind, TokenKind::Ident(_))
                && matches!(&self.peek(1).kind, TokenKind::Assign)
            {
                let stmt = self.parse_assign()?;
                stmts.push(stmt);
            } else {
                let expr = self.parse_expr()?;
                stmts.push(Stmt::Expr(expr));
                self.skip_newlines();
            }
        }

        Ok((None, stmts))
    }

    // -------------------------------------------------------------------------
    // val / var
    // -------------------------------------------------------------------------
    fn parse_val(&mut self) -> Result<Stmt, ParseError> {
        self.expect_exact(&TokenKind::Val)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected variable name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Assign)?;
        let value = self.parse_expr()?;
        self.skip_newlines();
        Ok(Stmt::Val(name, value))
    }

    fn parse_var(&mut self) -> Result<Stmt, ParseError> {
        self.expect_exact(&TokenKind::Var)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected variable name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Assign)?;
        let value = self.parse_expr()?;
        self.skip_newlines();

        let mut reason = None;
        if self.is_indent() {
            self.advance();
            self.skip_newlines();
            if self.check_exact(&TokenKind::Reason) {
                self.advance();
                self.expect_exact(&TokenKind::Colon)?;
                if let TokenKind::Str(s) = self.current().kind.clone() {
                    reason = Some(s);
                    self.advance();
                }
                self.skip_newlines();
            }
            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(Stmt::Var(name, value, reason))
    }

    fn parse_assign(&mut self) -> Result<Stmt, ParseError> {
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected variable name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Assign)?;
        let value = self.parse_expr()?;
        self.skip_newlines();
        Ok(Stmt::Assign(name, value))
    }

    // -------------------------------------------------------------------------
    // match expression
    // -------------------------------------------------------------------------
    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        self.expect_exact(&TokenKind::Match)?;
        let subject = self.parse_expr()?;
        self.expect_exact(&TokenKind::Colon)?;
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

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
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
            TokenKind::None => {
                self.advance();
                Ok(Pattern::Constructor("None".to_string(), vec![]))
            }
            TokenKind::Ok | TokenKind::Err | TokenKind::Some => {
                let name = match self.current().kind {
                    TokenKind::Ok => "Ok",
                    TokenKind::Err => "Err",
                    TokenKind::Some => "Some",
                    _ => unreachable!(),
                }
                .to_string();
                self.advance();
                let mut bindings = vec![];
                if self.check_exact(&TokenKind::LParen) {
                    self.advance();
                    if let TokenKind::Ident(s) = self.current().kind.clone() {
                        bindings.push(s);
                        self.advance();
                    }
                    self.expect_exact(&TokenKind::RParen)?;
                }
                Ok(Pattern::Constructor(name, bindings))
            }
            _ => Err(self.error(format!(
                "Unexpected token in pattern: {:?}",
                self.current().kind
            ))),
        }
    }

    // -------------------------------------------------------------------------
    // Expressions
    // -------------------------------------------------------------------------
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_pipe()
    }

    /// Parse an expression for use inside verify cases.
    /// `==` is reserved as the separator between left and right sides.
    /// Other comparisons (`!=`, `<`, `>`, `<=`, `>=`) are allowed.
    fn parse_pipe(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_comparison()?;

        while self.check_exact(&TokenKind::Pipe) {
            self.advance();
            let right = self.parse_call_or_atom()?;
            left = Expr::Pipe(Box::new(left), Box::new(right));
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_additive(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_multiplicative(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
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

    fn parse_call_or_atom(&mut self) -> Result<Expr, ParseError> {
        let atom = self.parse_atom()?;

        if self.check_exact(&TokenKind::LParen) {
            // Lookahead: is this `Name(field: value, ...)` (record creation)?
            // Detect by checking if token after `(` is `Ident` followed by `:`
            let is_record_create = if let Expr::Ident(ref name) = atom {
                name.chars().next().map_or(false, |c| c.is_uppercase())
                    && matches!(&self.peek(1).kind, TokenKind::Ident(_))
                    && self.peek(2).kind == TokenKind::Colon
            } else {
                false
            };

            if is_record_create {
                if let Expr::Ident(type_name) = atom {
                    self.advance(); // consume (
                    let fields = self.parse_record_create_fields()?;
                    self.expect_exact(&TokenKind::RParen)?;
                    return Ok(Expr::RecordCreate { type_name, fields });
                }
            }

            self.advance();
            let args = self.parse_args()?;
            self.expect_exact(&TokenKind::RParen)?;
            return Ok(Expr::FnCall(Box::new(atom), args));
        }

        Ok(atom)
    }

    /// Parse named-field arguments for record creation: `name: expr, name2: expr2`
    fn parse_record_create_fields(&mut self) -> Result<Vec<(String, Expr)>, ParseError> {
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
            self.expect_exact(&TokenKind::Colon)?;
            let value = self.parse_expr()?;
            fields.push((field_name, value));
        }

        Ok(fields)
    }

    fn parse_args(&mut self) -> Result<Vec<Expr>, ParseError> {
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

    fn parse_atom(&mut self) -> Result<Expr, ParseError> {
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
                let str_parts = parts
                    .into_iter()
                    .map(|(is_expr, s)| {
                        if is_expr {
                            StrPart::Expr(s)
                        } else {
                            StrPart::Literal(s)
                        }
                    })
                    .collect();
                Ok(Expr::InterpolatedStr(str_parts))
            }
            TokenKind::Bool(b) => {
                self.advance();
                Ok(Expr::Literal(Literal::Bool(b)))
            }
            TokenKind::None => {
                self.advance();
                Ok(Expr::Constructor("None".to_string(), None))
            }
            TokenKind::Ok | TokenKind::Err | TokenKind::Some => {
                let name = match &self.current().kind {
                    TokenKind::Ok => "Ok",
                    TokenKind::Err => "Err",
                    TokenKind::Some => "Some",
                    _ => unreachable!(),
                }
                .to_string();
                self.advance();
                let mut arg = None;
                if self.check_exact(&TokenKind::LParen) {
                    self.advance();
                    arg = Some(Box::new(self.parse_expr()?));
                    self.expect_exact(&TokenKind::RParen)?;
                }
                Ok(Expr::Constructor(name, arg))
            }
            TokenKind::Match => self.parse_match(),
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect_exact(&TokenKind::RParen)?;
                Ok(expr)
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

    // -------------------------------------------------------------------------
    // Verify block
    // -------------------------------------------------------------------------
    fn parse_verify(&mut self) -> Result<VerifyBlock, ParseError> {
        self.expect_exact(&TokenKind::Verify)?;
        let fn_name_tok = self.expect_kind(
            &TokenKind::Ident(String::new()),
            "Expected function name in verify block",
        )?;
        let fn_name = match fn_name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Colon)?;
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
    fn parse_decision(&mut self) -> Result<DecisionBlock, ParseError> {
        self.expect_exact(&TokenKind::Decision)?;
        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected decision name")?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Colon)?;
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
                        self.expect_exact(&TokenKind::Colon)?;
                        if let TokenKind::Str(s) = self.current().kind.clone() {
                            date = s;
                            self.advance();
                        }
                        self.skip_newlines();
                    }
                    TokenKind::Reason => {
                        self.advance();
                        self.expect_exact(&TokenKind::Colon)?;
                        self.skip_newlines();
                        reason = self.parse_multiline_text()?;
                    }
                    TokenKind::Chosen => {
                        self.advance();
                        self.expect_exact(&TokenKind::Colon)?;
                        if let TokenKind::Ident(s) = self.current().kind.clone() {
                            chosen = s;
                            self.advance();
                        }
                        self.skip_newlines();
                    }
                    TokenKind::Rejected => {
                        self.advance();
                        self.expect_exact(&TokenKind::Colon)?;
                        rejected = self.parse_ident_list()?;
                        self.skip_newlines();
                    }
                    TokenKind::Impacts => {
                        self.advance();
                        self.expect_exact(&TokenKind::Colon)?;
                        impacts = self.parse_ident_list()?;
                        self.skip_newlines();
                    }
                    TokenKind::Author => {
                        self.advance();
                        self.expect_exact(&TokenKind::Colon)?;
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

    fn parse_multiline_text(&mut self) -> Result<String, ParseError> {
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

    fn parse_ident_list(&mut self) -> Result<Vec<String>, ParseError> {
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
