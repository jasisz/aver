use super::*;

impl Parser {
    const VERIFY_LAW_MAX_CASES: usize = 10_000;

    fn current_ident_is(&self, expected: &str) -> bool {
        matches!(&self.current().kind, TokenKind::Ident(name) if name == expected)
    }

    fn signed_int_at_offset(&self, offset: usize) -> Option<(i64, usize)> {
        match self.peek(offset).kind.clone() {
            TokenKind::Int(n) => Some((n, 1)),
            TokenKind::Minus => match self.peek(offset + 1).kind.clone() {
                TokenKind::Int(n) => Some((-n, 2)),
                _ => None,
            },
            _ => None,
        }
    }

    fn int_range_domain_ahead(&self) -> bool {
        let Some((_, consumed_start)) = self.signed_int_at_offset(0) else {
            return false;
        };
        matches!(self.peek(consumed_start).kind, TokenKind::Dot)
            && matches!(self.peek(consumed_start + 1).kind, TokenKind::Dot)
            && self.signed_int_at_offset(consumed_start + 2).is_some()
    }

    fn parse_signed_int_literal(&mut self) -> Result<i64, ParseError> {
        match self.current().kind.clone() {
            TokenKind::Int(n) => {
                self.advance();
                Ok(n)
            }
            TokenKind::Minus => {
                self.advance();
                match self.current().kind.clone() {
                    TokenKind::Int(n) => {
                        self.advance();
                        Ok(-n)
                    }
                    _ => Err(self.error(format!(
                        "Expected integer after '-', found {}",
                        self.current().kind
                    ))),
                }
            }
            _ => Err(self.error(format!(
                "Expected integer literal in given domain, found {}",
                self.current().kind
            ))),
        }
    }

    fn domain_len(domain: &VerifyGivenDomain) -> usize {
        match domain {
            VerifyGivenDomain::IntRange { start, end } => {
                if start > end {
                    0
                } else {
                    (*end as i128 - *start as i128 + 1) as usize
                }
            }
            VerifyGivenDomain::Explicit(values) => values.len(),
        }
    }

    fn domain_values(domain: &VerifyGivenDomain) -> Vec<Expr> {
        match domain {
            VerifyGivenDomain::IntRange { start, end } => (*start..=*end)
                .map(|n| Expr::Literal(Literal::Int(n)))
                .collect(),
            VerifyGivenDomain::Explicit(values) => values.clone(),
        }
    }

    fn substitute_expr(expr: &Expr, bindings: &std::collections::HashMap<String, Expr>) -> Expr {
        match expr {
            Expr::Ident(name) => bindings
                .get(name)
                .cloned()
                .unwrap_or_else(|| Expr::Ident(name.clone())),
            Expr::Attr(obj, field) => Expr::Attr(
                Box::new(Self::substitute_expr(obj, bindings)),
                field.clone(),
            ),
            Expr::FnCall(fn_expr, args) => Expr::FnCall(
                Box::new(Self::substitute_expr(fn_expr, bindings)),
                args.iter()
                    .map(|arg| Self::substitute_expr(arg, bindings))
                    .collect(),
            ),
            Expr::BinOp(op, left, right) => Expr::BinOp(
                op.clone(),
                Box::new(Self::substitute_expr(left, bindings)),
                Box::new(Self::substitute_expr(right, bindings)),
            ),
            Expr::Match {
                subject,
                arms,
                line,
            } => Expr::Match {
                subject: Box::new(Self::substitute_expr(subject, bindings)),
                arms: arms
                    .iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern.clone(),
                        body: Box::new(Self::substitute_expr(&arm.body, bindings)),
                    })
                    .collect(),
                line: *line,
            },
            Expr::Pipe(left, right) => Expr::Pipe(
                Box::new(Self::substitute_expr(left, bindings)),
                Box::new(Self::substitute_expr(right, bindings)),
            ),
            Expr::Constructor(name, arg) => Expr::Constructor(
                name.clone(),
                arg.as_ref()
                    .map(|inner| Box::new(Self::substitute_expr(inner, bindings))),
            ),
            Expr::ErrorProp(inner) => {
                Expr::ErrorProp(Box::new(Self::substitute_expr(inner, bindings)))
            }
            Expr::InterpolatedStr(parts) => Expr::InterpolatedStr(
                parts
                    .iter()
                    .map(|part| match part {
                        StrPart::Literal(s) => StrPart::Literal(s.clone()),
                        StrPart::Parsed(inner) => {
                            StrPart::Parsed(Box::new(Self::substitute_expr(inner, bindings)))
                        }
                    })
                    .collect(),
            ),
            Expr::List(items) => Expr::List(
                items
                    .iter()
                    .map(|item| Self::substitute_expr(item, bindings))
                    .collect(),
            ),
            Expr::Tuple(items) => Expr::Tuple(
                items
                    .iter()
                    .map(|item| Self::substitute_expr(item, bindings))
                    .collect(),
            ),
            Expr::MapLiteral(entries) => Expr::MapLiteral(
                entries
                    .iter()
                    .map(|(k, v)| {
                        (
                            Self::substitute_expr(k, bindings),
                            Self::substitute_expr(v, bindings),
                        )
                    })
                    .collect(),
            ),
            Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
                type_name: type_name.clone(),
                fields: fields
                    .iter()
                    .map(|(name, value)| (name.clone(), Self::substitute_expr(value, bindings)))
                    .collect(),
            },
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => Expr::RecordUpdate {
                type_name: type_name.clone(),
                base: Box::new(Self::substitute_expr(base, bindings)),
                updates: updates
                    .iter()
                    .map(|(name, value)| (name.clone(), Self::substitute_expr(value, bindings)))
                    .collect(),
            },
            Expr::TailCall(boxed) => Expr::TailCall(Box::new((
                boxed.0.clone(),
                boxed
                    .1
                    .iter()
                    .map(|arg| Self::substitute_expr(arg, bindings))
                    .collect(),
            ))),
            Expr::Literal(lit) => Expr::Literal(lit.clone()),
            Expr::Resolved(slot) => Expr::Resolved(*slot),
        }
    }

    fn expand_law_cases_rec(
        givens: &[VerifyGiven],
        idx: usize,
        bindings: &mut std::collections::HashMap<String, Expr>,
        left: &Expr,
        right: &Expr,
        out: &mut Vec<(Expr, Expr)>,
    ) {
        if idx == givens.len() {
            out.push((
                Self::substitute_expr(left, bindings),
                Self::substitute_expr(right, bindings),
            ));
            return;
        }

        let given = &givens[idx];
        for value in Self::domain_values(&given.domain) {
            bindings.insert(given.name.clone(), value);
            Self::expand_law_cases_rec(givens, idx + 1, bindings, left, right, out);
            bindings.remove(&given.name);
        }
    }

    fn expand_law_cases(
        &self,
        givens: &[VerifyGiven],
        left: &Expr,
        right: &Expr,
    ) -> Result<Vec<(Expr, Expr)>, ParseError> {
        let mut total = 1usize;
        for given in givens {
            let len = Self::domain_len(&given.domain);
            if len == 0 {
                return Err(self.error(format!(
                    "given '{}' has empty domain in law verify block",
                    given.name
                )));
            }
            total = total.checked_mul(len).ok_or_else(|| {
                self.error(format!(
                    "Law verify expands to too many cases (> {})",
                    Self::VERIFY_LAW_MAX_CASES
                ))
            })?;
            if total > Self::VERIFY_LAW_MAX_CASES {
                return Err(self.error(format!(
                    "Law verify expands to {} cases (max {})",
                    total,
                    Self::VERIFY_LAW_MAX_CASES
                )));
            }
        }

        let mut out = Vec::with_capacity(total);
        let mut bindings = std::collections::HashMap::new();
        Self::expand_law_cases_rec(givens, 0, &mut bindings, left, right, &mut out);
        Ok(out)
    }

    fn parse_verify_given(&mut self) -> Result<VerifyGiven, ParseError> {
        if !self.current_ident_is("given") {
            return Err(self.error("Expected 'given'".to_string()));
        }
        self.advance(); // given
        let name_tok = self.expect_kind(
            &TokenKind::Ident(String::new()),
            "Expected variable name after 'given'",
        )?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.expect_exact(&TokenKind::Colon)?;
        let type_name = self.parse_type()?;
        self.expect_exact(&TokenKind::Assign)?;

        let domain = if self.int_range_domain_ahead() {
            let start = self.parse_signed_int_literal()?;
            self.expect_exact(&TokenKind::Dot)?;
            self.expect_exact(&TokenKind::Dot)?;
            let end = self.parse_signed_int_literal()?;
            if type_name != "Int" {
                return Err(self.error(format!(
                    "Range domain '{}'..'{}' requires given type Int, got {}",
                    start, end, type_name
                )));
            }
            if start > end {
                return Err(self.error(format!(
                    "Range domain start must be <= end, got {}..{}",
                    start, end
                )));
            }
            VerifyGivenDomain::IntRange { start, end }
        } else {
            let domain_expr = self.parse_pipe()?;
            let Expr::List(values) = domain_expr else {
                return Err(self.error(
                    "Given domain must be list literal ([...]) or Int range (a..b)".to_string(),
                ));
            };
            VerifyGivenDomain::Explicit(values)
        };

        if Self::domain_len(&domain) == 0 {
            return Err(self.error(format!(
                "given '{}' has empty domain in law verify block",
                name
            )));
        }

        Ok(VerifyGiven {
            name,
            type_name,
            domain,
        })
    }

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
        let mut kind = VerifyKind::Cases;
        let mut law_name = None;
        if self.current_ident_is("law") {
            self.advance(); // law
            let law_name_tok = self.expect_kind(
                &TokenKind::Ident(String::new()),
                "Expected law name after 'law'",
            )?;
            law_name = Some(match law_name_tok.kind {
                TokenKind::Ident(s) => s,
                _ => unreachable!(),
            });
        }
        self.skip_newlines();

        let mut cases = Vec::new();

        if self.is_indent() {
            self.advance();
            self.skip_newlines();

            if let Some(law_name) = law_name {
                let mut givens = Vec::new();
                while self.current_ident_is("given") {
                    let given = self.parse_verify_given()?;
                    givens.push(given);
                    self.skip_newlines();
                }

                if givens.is_empty() {
                    return Err(self.error(
                        "Law verify block must contain at least one 'given' line".to_string(),
                    ));
                }

                let left = self.parse_pipe()?;
                self.expect_exact(&TokenKind::FatArrow)?;
                let right = self.parse_pipe()?;
                self.skip_newlines();

                if !self.is_dedent() && !self.is_eof() {
                    return Err(self.error(
                        "Law verify block must contain exactly one assertion line (lhs => rhs)"
                            .to_string(),
                    ));
                }

                cases = self.expand_law_cases(&givens, &left, &right)?;
                kind = VerifyKind::Law(VerifyLaw {
                    name: law_name,
                    givens,
                    lhs: left,
                    rhs: right,
                });
            } else {
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
            }

            if self.is_dedent() {
                self.advance();
            }
        } else if law_name.is_some() {
            return Err(self.error("Law verify block requires an indented block body".to_string()));
        }

        Ok(VerifyBlock {
            fn_name,
            line,
            cases,
            kind,
        })
    }

    // -------------------------------------------------------------------------
    // Decision block
    // -------------------------------------------------------------------------
    pub(super) fn parse_decision(&mut self) -> Result<DecisionBlock, ParseError> {
        let line = self.current().line;
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
                        impacts = self.parse_decision_impacts_list()?;
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
            line,
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

    fn parse_decision_impacts_list(&mut self) -> Result<Vec<DecisionImpact>, ParseError> {
        let mut items = Vec::new();

        if self.check_exact(&TokenKind::LBracket) {
            self.advance();
            while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
                match self.current().kind.clone() {
                    TokenKind::Ident(s) => {
                        items.push(DecisionImpact::Symbol(s));
                        self.advance();
                    }
                    TokenKind::Str(s) => {
                        items.push(DecisionImpact::Semantic(s));
                        self.advance();
                    }
                    TokenKind::Comma => {
                        self.advance();
                    }
                    _ => {
                        return Err(self.error(
                            "Expected identifier or string in decision 'impacts' list".to_string(),
                        ));
                    }
                }
            }
            self.expect_exact(&TokenKind::RBracket)?;
        } else {
            return Err(self.error("Expected '[' to start list".to_string()));
        }

        Ok(items)
    }
}
