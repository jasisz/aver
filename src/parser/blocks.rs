use super::*;

/// Oracle v1: walk an expression, replacing `Ident(name)` with `value`.
/// Used by verify-trace local bindings so block-scoped aliases like
/// `expect = rnd(...)` get inlined into each case before helper
/// generation / typecheck. Recurses into every subtree that can hold
/// an `Ident`; does not descend into match-arm patterns (those
/// introduce their own bindings). Stops at `Resolved` nodes — they
/// already bind a slot and do not need string-name substitution.
fn substitute_ident(expr: &mut Spanned<Expr>, name: &str, value: &Spanned<Expr>) {
    match &mut expr.node {
        Expr::Ident(s) if s == name => {
            *expr = value.clone();
        }
        Expr::Ident(_) | Expr::Literal(_) | Expr::Resolved { .. } => {}
        Expr::Attr(inner, _) => substitute_ident(inner, name, value),
        Expr::FnCall(callee, args) => {
            substitute_ident(callee, name, value);
            for a in args {
                substitute_ident(a, name, value);
            }
        }
        Expr::BinOp(_, l, r) => {
            substitute_ident(l, name, value);
            substitute_ident(r, name, value);
        }
        Expr::Match { subject, arms } => {
            substitute_ident(subject, name, value);
            for arm in arms {
                substitute_ident(&mut arm.body, name, value);
            }
        }
        Expr::Constructor(_, payload) => {
            if let Some(inner) = payload {
                substitute_ident(inner, name, value);
            }
        }
        Expr::ErrorProp(inner) => substitute_ident(inner, name, value),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    substitute_ident(inner, name, value);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                substitute_ident(item, name, value);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                substitute_ident(k, name, value);
                substitute_ident(v, name, value);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                substitute_ident(v, name, value);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            substitute_ident(base, name, value);
            for (_, v) in updates {
                substitute_ident(v, name, value);
            }
        }
        Expr::TailCall(data) => {
            for a in &mut data.args {
                substitute_ident(a, name, value);
            }
        }
        Expr::IndependentProduct(items, _) => {
            for item in items {
                substitute_ident(item, name, value);
            }
        }
    }
}

struct ExpandedLawCases {
    cases: Vec<(Spanned<Expr>, Spanned<Expr>)>,
    sample_guards: Vec<Spanned<Expr>>,
    /// Per-case given bindings: vec of (name, value_expr) pairs.
    case_givens: Vec<Vec<(String, Spanned<Expr>)>>,
}

impl Parser {
    const VERIFY_LAW_MAX_CASES: usize = 10_000;

    fn current_ident_is(&self, expected: &str) -> bool {
        matches!(&self.current().kind, TokenKind::Ident(name) if name == expected)
    }

    /// True when current position looks like `name = expr` — an Ident
    /// followed by `=` (Assign), not `=>` (FatArrow). Used in
    /// verify-trace blocks to distinguish local bindings from case
    /// assertions (`lhs => rhs`). `given` / `where` idents are
    /// excluded so they stay handled by their own parsers.
    fn looks_like_binding(&self) -> bool {
        let TokenKind::Ident(name) = &self.current().kind else {
            return false;
        };
        if name == "given" || name == "where" {
            return false;
        }
        matches!(self.peek_skip_formatting(1).kind, TokenKind::Assign)
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

    fn domain_values(domain: &VerifyGivenDomain) -> Vec<Spanned<Expr>> {
        match domain {
            VerifyGivenDomain::IntRange { start, end } => (*start..=*end)
                .map(|n| Spanned::bare(Expr::Literal(Literal::Int(n))))
                .collect(),
            VerifyGivenDomain::Explicit(values) => values.clone(),
        }
    }

    fn substitute_expr(
        expr: &Spanned<Expr>,
        bindings: &std::collections::HashMap<String, Spanned<Expr>>,
    ) -> Spanned<Expr> {
        let line = expr.line;
        let node = match &expr.node {
            Expr::Ident(name) => {
                return bindings
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| Spanned::new(Expr::Ident(name.clone()), line));
            }
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
                *op,
                Box::new(Self::substitute_expr(left, bindings)),
                Box::new(Self::substitute_expr(right, bindings)),
            ),
            Expr::Match { subject, arms } => Expr::Match {
                subject: Box::new(Self::substitute_expr(subject, bindings)),
                arms: arms
                    .iter()
                    .map(|arm| MatchArm {
                        pattern: arm.pattern.clone(),
                        body: Box::new(Self::substitute_expr(&arm.body, bindings)),
                    })
                    .collect(),
            },
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
            Expr::IndependentProduct(items, flag) => Expr::IndependentProduct(
                items
                    .iter()
                    .map(|item| Self::substitute_expr(item, bindings))
                    .collect(),
                *flag,
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
            Expr::TailCall(boxed) => Expr::TailCall(Box::new(TailCallData::new(
                boxed.target.clone(),
                boxed
                    .args
                    .iter()
                    .map(|arg| Self::substitute_expr(arg, bindings))
                    .collect(),
            ))),
            Expr::Literal(lit) => Expr::Literal(lit.clone()),
            Expr::Resolved {
                slot,
                name,
                last_use,
            } => Expr::Resolved {
                slot: *slot,
                name: name.clone(),
                last_use: *last_use,
            },
        };
        Spanned::new(node, line)
    }

    fn expand_law_cases_rec(
        givens: &[VerifyGiven],
        idx: usize,
        bindings: &mut std::collections::HashMap<String, Spanned<Expr>>,
        when: Option<&Spanned<Expr>>,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        out: &mut ExpandedLawCases,
    ) {
        if idx == givens.len() {
            if let Some(when_expr) = when {
                out.sample_guards
                    .push(Self::substitute_expr(when_expr, bindings));
            }
            out.cases.push((
                Self::substitute_expr(left, bindings),
                Self::substitute_expr(right, bindings),
            ));
            // Snapshot current given bindings for this case
            out.case_givens.push(
                givens
                    .iter()
                    .filter_map(|g| bindings.get(&g.name).map(|e| (g.name.clone(), e.clone())))
                    .collect(),
            );
            return;
        }

        let given = &givens[idx];
        for value in Self::domain_values(&given.domain) {
            bindings.insert(given.name.clone(), value);
            Self::expand_law_cases_rec(givens, idx + 1, bindings, when, left, right, out);
            bindings.remove(&given.name);
        }
    }

    fn expand_law_cases(
        &self,
        givens: &[VerifyGiven],
        when: Option<&Spanned<Expr>>,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
    ) -> Result<ExpandedLawCases, ParseError> {
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

        let mut out = ExpandedLawCases {
            cases: Vec::with_capacity(total),
            sample_guards: Vec::with_capacity(total),
            case_givens: Vec::with_capacity(total),
        };
        let mut bindings = std::collections::HashMap::new();
        Self::expand_law_cases_rec(givens, 0, &mut bindings, when, left, right, &mut out);
        Ok(out)
    }

    /// Parse a `given` clause's type annotation.
    ///
    /// Accepts two forms:
    /// - Regular type annotation (`Int`, `String`, `List<Int>`, `Tcp.Connection`).
    /// - Effect-method reference (`Random.int`, `Console.print`) — lowercase
    ///   second segment. Oracle v1 treats these as type-like annotations
    ///   meaning "the oracle signature of this classified effect method".
    ///   The typechecker resolves the method name to its lifted signature
    ///   via `types::checker::effect_classification::oracle_signature`.
    fn parse_given_type_annotation(&mut self) -> Result<String, ParseError> {
        // Look ahead for `Upper.lower` effect-method ref (`Random.int` etc.).
        // `parse_type` expects uppercase on both sides of the dot, which
        // is the right check for regular dotted types like `Tcp.Connection`
        // but rejects classified effect methods.
        if let TokenKind::Ident(head) = &self.current().kind
            && head.chars().next().is_some_and(|c| c.is_uppercase())
            && matches!(self.peek(1).kind, TokenKind::Dot)
            && let TokenKind::Ident(tail) = &self.peek(2).kind
            && tail.chars().next().is_some_and(|c| c.is_lowercase())
        {
            let head = head.clone();
            let tail = tail.clone();
            self.advance(); // head
            self.advance(); // dot
            self.advance(); // tail
            return Ok(format!("{}.{}", head, tail));
        }
        self.parse_type()
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
        let type_name = self.parse_given_type_annotation()?;
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
            let domain_expr = self.parse_expr()?;
            let Expr::List(values) = domain_expr.node else {
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
        // Oracle v1: optional `trace` keyword after fn name enables
        // trace-aware assertions (`.result`, `.trace.*`, event literals).
        // Parses both `verify fn trace` (cases) and `verify fn trace law fnSpec`.
        let trace_mode = if self.current_ident_is("trace") {
            self.advance(); // trace
            true
        } else {
            false
        };
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
        let mut case_spans = Vec::new();
        let mut case_givens: Vec<Vec<(String, Spanned<Expr>)>> = Vec::new();
        let mut cases_givens_out: Vec<crate::ast::VerifyGiven> = Vec::new();

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

                let when = if self.current_ident_is("when") {
                    self.advance(); // when
                    let when_expr = self.parse_expr()?;
                    self.skip_newlines();
                    Some(when_expr)
                } else {
                    None
                };

                let law_start_line = self.current().line;
                let law_start_col = self.current().col;
                let left = self.parse_expr()?;
                self.expect_exact(&TokenKind::FatArrow)?;
                let right = self.parse_expr()?;
                let law_end_line = self.current().line;
                let law_end_col = self.current().col;
                self.skip_newlines();

                if !self.is_dedent() && !self.is_eof() {
                    return Err(self.error(
                        "Law verify block must contain exactly one assertion line (lhs => rhs)"
                            .to_string(),
                    ));
                }

                let ExpandedLawCases {
                    cases: expanded_cases,
                    sample_guards,
                    case_givens: expanded_givens,
                } = self.expand_law_cases(&givens, when.as_ref(), &left, &right)?;
                // All generated cases share the law assertion's span
                let law_span = SourceSpan {
                    line: law_start_line,
                    col: law_start_col,
                    end_line: law_end_line,
                    end_col: law_end_col,
                };
                case_spans = vec![law_span; expanded_cases.len()];
                case_givens = expanded_givens;
                cases = expanded_cases;
                kind = VerifyKind::Law(Box::new(VerifyLaw {
                    name: law_name,
                    givens,
                    when,
                    lhs: left,
                    rhs: right,
                    sample_guards,
                }));
            } else {
                // Oracle v1: trace-aware cases-form blocks may declare
                // `given` clauses at the top (natural extension for
                // oracles that the trace-aware impl needs). v0: given
                // stubs are silently accepted at parse time and their
                // bindings become case-local in the same way law-form
                // handles them — no cartesian expansion yet, so if the
                // stub list has >1 element we take the first for now.
                let mut cases_givens: Vec<crate::ast::VerifyGiven> = Vec::new();
                let mut local_bindings: Vec<(String, Spanned<Expr>)> = Vec::new();
                if trace_mode {
                    while self.current_ident_is("given") {
                        cases_givens.push(self.parse_verify_given()?);
                        self.skip_newlines();
                    }
                    // Oracle v1: local bindings in verify-trace block —
                    // `expect = rnd(BranchPath.root, 0, 1, 6)`. Parsed as
                    // `name = expr` lines between `given` clauses and
                    // case assertions. Substituted into each case's LHS
                    // / RHS at plan-build time.
                    while let TokenKind::Ident(_) = &self.current().kind {
                        // Must be a binding `name = expr`, not a case
                        // assertion `expr => expr`. Peek ahead for `=`
                        // before the FatArrow / end-of-line.
                        if !self.looks_like_binding() {
                            break;
                        }
                        let name_tok = self.expect_kind(
                            &TokenKind::Ident(String::new()),
                            "Expected binding name",
                        )?;
                        let name = match name_tok.kind {
                            TokenKind::Ident(s) => s,
                            _ => unreachable!(),
                        };
                        self.expect_exact(&TokenKind::Assign)?;
                        let expr = self.parse_expr()?;
                        local_bindings.push((name, expr));
                        self.skip_newlines();
                    }
                }

                while !self.is_dedent() && !self.is_eof() {
                    if self.is_newline() {
                        self.advance();
                        continue;
                    }

                    let start_line = self.current().line;
                    let start_col = self.current().col;
                    let left = self.parse_expr()?;
                    self.expect_exact(&TokenKind::FatArrow)?;
                    let right = self.parse_expr()?;
                    let end_line = self.current().line;
                    let end_col = self.current().col;
                    cases.push((left, right));
                    case_spans.push(SourceSpan {
                        line: start_line,
                        col: start_col,
                        end_line,
                        end_col,
                    });
                    self.skip_newlines();
                }

                // Oracle v1: substitute local bindings into each case's
                // LHS / RHS. Locals are block-scoped aliases like
                // `expect = rnd(...)` — syntactic substitution keeps the
                // downstream runner (helpers, projections, type-check)
                // oblivious to them. Evaluates bound exprs per reference
                // site; callers should bind deterministic values.
                if !local_bindings.is_empty() {
                    for (left, right) in cases.iter_mut() {
                        for (name, value) in &local_bindings {
                            substitute_ident(left, name, value);
                            substitute_ident(right, name, value);
                        }
                    }
                }

                // Per-case given bindings for the verify runner: each
                // case gets the same binding (single-stub slot). Law-
                // form's cartesian expansion would produce N copies of
                // each case; cases-form here keeps the explicit case
                // list and just layers the given-bound values on top.
                if !cases_givens.is_empty() {
                    let per_case_bindings: Vec<(String, Spanned<Expr>)> = cases_givens
                        .iter()
                        .filter_map(|g| match &g.domain {
                            crate::ast::VerifyGivenDomain::Explicit(vs) => {
                                vs.first().map(|v| (g.name.clone(), v.clone()))
                            }
                            _ => None,
                        })
                        .collect();
                    case_givens = vec![per_case_bindings; cases.len()];
                    cases_givens_out = cases_givens;
                }
            }

            if self.is_dedent() {
                self.advance();
            }
        } else if law_name.is_some() {
            return Err(self.error("Law verify block requires an indented block body".to_string()));
        }

        debug_assert_eq!(cases.len(), case_spans.len());
        Ok(VerifyBlock {
            fn_name,
            line,
            cases,
            case_spans,
            case_givens,
            kind,
            trace: trace_mode,
            cases_givens: cases_givens_out,
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
        let mut chosen = Spanned::bare(DecisionImpact::Semantic(String::new()));
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
                        chosen = self.parse_decision_ref("chosen")?;
                        self.skip_newlines();
                    }
                    "rejected" => {
                        rejected = self.parse_decision_refs_list("rejected")?;
                        self.skip_newlines();
                    }
                    "impacts" => {
                        impacts = self.parse_decision_refs_list("impacts")?;
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

    pub(super) fn parse_inline_string_with_continuations(&mut self) -> Result<String, ParseError> {
        if let TokenKind::Str(first) = self.current().kind.clone() {
            let mut parts = vec![first];
            self.advance();
            let checkpoint = self.pos;
            self.skip_newlines();
            if self.is_indent() {
                let tail = self.parse_multiline_text()?;
                if !tail.is_empty() {
                    parts.push(tail);
                }
            } else {
                self.pos = checkpoint;
            }

            return Ok(parts.join(" "));
        }

        Err(self.error("Expected string literal".to_string()))
    }

    pub(super) fn parse_inline_or_block_text(&mut self) -> Result<String, ParseError> {
        if matches!(self.current().kind, TokenKind::Str(_)) {
            return self.parse_inline_string_with_continuations();
        }

        self.skip_newlines();
        self.parse_multiline_text()
    }

    fn parse_decision_ref(
        &mut self,
        field_name: &str,
    ) -> Result<Spanned<DecisionImpact>, ParseError> {
        let line = self.current().line;
        match self.current().kind.clone() {
            TokenKind::Ident(s) => {
                self.advance();
                let sym = self.collect_dotted_name(s);
                Ok(Spanned::new(DecisionImpact::Symbol(sym), line))
            }
            TokenKind::Str(s) => {
                self.advance();
                Ok(Spanned::new(DecisionImpact::Semantic(s), line))
            }
            _ => Err(self.error(format!(
                "Expected identifier or string value for decision '{}'",
                field_name
            ))),
        }
    }

    fn parse_decision_refs_list(
        &mut self,
        field_name: &str,
    ) -> Result<Vec<Spanned<DecisionImpact>>, ParseError> {
        let mut items = Vec::new();

        if self.check_exact(&TokenKind::LBracket) {
            self.advance();
            while !self.check_exact(&TokenKind::RBracket) && !self.is_eof() {
                let line = self.current().line;
                match self.current().kind.clone() {
                    TokenKind::Ident(s) => {
                        self.advance();
                        let sym = self.collect_dotted_name(s);
                        items.push(Spanned::new(DecisionImpact::Symbol(sym), line));
                    }
                    TokenKind::Str(s) => {
                        items.push(Spanned::new(DecisionImpact::Semantic(s), line));
                        self.advance();
                    }
                    TokenKind::Comma => {
                        self.advance();
                    }
                    _ => {
                        return Err(self.error(format!(
                            "Expected identifier or string in decision '{}' list",
                            field_name
                        )));
                    }
                }
            }
            self.expect_exact(&TokenKind::RBracket)?;
        } else {
            return Err(self.error("Expected '[' to start list".to_string()));
        }

        Ok(items)
    }

    /// Consume `.Ident` suffixes after an initial identifier to build `"Foo.Bar.Baz"`.
    fn collect_dotted_name(&mut self, prefix: String) -> String {
        let mut name = prefix;
        while self.check_exact(&TokenKind::Dot) {
            self.advance();
            if let TokenKind::Ident(s) = self.current().kind.clone() {
                name.push('.');
                name.push_str(&s);
                self.advance();
            } else {
                break;
            }
        }
        name
    }
}
