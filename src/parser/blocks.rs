use super::*;

use crate::ast_rewrite::rewrite_idents_scoped;

/// Verify-trace local bindings: walk an expression and replace bare
/// `Ident(name)` with `value`. Pattern-bound shadowing in `match` arms
/// is handled by [`rewrite_idents_scoped`] — an inner pattern that
/// binds the same name hides the substitution for its body. The
/// helper returns a fresh expression; we `*expr = ...` back in place
/// so call sites keep their mutable-rewrite ergonomics.
fn substitute_ident(expr: &mut Spanned<Expr>, name: &str, value: &Spanned<Expr>) {
    *expr = rewrite_idents_scoped(expr, |n| if n == name { Some(value.clone()) } else { None });
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
        rewrite_idents_scoped(expr, |name| bindings.get(name).cloned())
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

                // Multiple `when` predicates compose with `Bool.and` — each
                // line tightens the world. We fold them left-to-right so
                // the runtime / proof / hostile pipelines see one
                // composite predicate they can drop cases against.
                let mut when: Option<Spanned<Expr>> = None;
                while self.current_ident_is("when") {
                    self.advance(); // when
                    let next = self.parse_expr()?;
                    self.skip_newlines();
                    when = Some(match when.take() {
                        None => next,
                        Some(prev) => {
                            let line = prev.line;
                            let bool_and = Spanned::new(
                                Expr::Attr(
                                    Box::new(Spanned::new(Expr::Ident("Bool".to_string()), line)),
                                    "and".to_string(),
                                ),
                                line,
                            );
                            Spanned::new(Expr::FnCall(Box::new(bool_and), vec![prev, next]), line)
                        }
                    });
                }

                // Oracle v1: local bindings in law form — symmetric
                // with trace-form. `expected = rnd(root(), 0, 1, 6) + ...`
                // between givens and the single `lhs => rhs` assertion.
                // Substituted into lhs / rhs before case expansion so
                // downstream (auto-proof matcher, emit) doesn't see
                // the binding at all — it's a textual shortcut.
                let mut law_locals: Vec<(String, Spanned<Expr>)> = Vec::new();
                while let TokenKind::Ident(_) = &self.current().kind {
                    if !self.looks_like_binding() {
                        break;
                    }
                    let name_tok = self
                        .expect_kind(&TokenKind::Ident(String::new()), "Expected binding name")?;
                    let name = match name_tok.kind {
                        TokenKind::Ident(s) => s,
                        _ => unreachable!(),
                    };
                    self.expect_exact(&TokenKind::Assign)?;
                    let expr = self.parse_expr()?;
                    law_locals.push((name, expr));
                    self.skip_newlines();
                }

                let law_start_line = self.current().line;
                let law_start_col = self.current().col;
                let mut left = self.parse_expr()?;
                // Claim form: `lhs => rhs` (Yields — an operational equation) OR
                // `P holds` (Holds — the Bool predicate `P` is true, sugar for
                // `P => true`). Both lower to the same `lhs`/`rhs` pair, so the
                // downstream case-expansion and codegen are unchanged.
                let mut right = if self.current_ident_is("holds") {
                    self.advance(); // holds
                    Spanned::bare(Expr::Literal(Literal::Bool(true)))
                } else {
                    self.expect_exact(&TokenKind::FatArrow)?;
                    self.skip_formatting();
                    self.parse_expr()?
                };

                // Substitute locals into the single assertion.
                for (name, value) in &law_locals {
                    substitute_ident(&mut left, name, value);
                    substitute_ident(&mut right, name, value);
                }
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
                    // Misplaced local binding (verify-trace only):
                    // `name = expr` inside the cases area parses `name`
                    // as the case LHS and then chokes on `=` where it
                    // expects `=>`. The raw "Expected '=>', found '='"
                    // message doesn't tell the user what to do. Detect
                    // the shape and surface a pointed repair.
                    if trace_mode && matches!(self.current().kind, TokenKind::Assign) {
                        return Err(self.error(
                            "verify-trace local bindings (`name = expr`) must come before \
                             any `lhs => rhs` case. Move this binding above the cases, \
                             then reference it by name in the cases."
                                .to_string(),
                        ));
                    }
                    self.expect_exact(&TokenKind::FatArrow)?;
                    // Oracle v1: allow the RHS to wrap to the next (indented) line.
                    // Long event-literal records (Http responses, nested records)
                    // don't fit on one line. COUNT the net INDENT depth crossed to
                    // reach the RHS so exactly the matching DEDENT(s) can be consumed
                    // AFTER it (below) — otherwise a wrapped RHS's closing DEDENT is
                    // taken as the end of the whole case block, and every case after a
                    // wrapped one is orphaned (parsed at the top level, surfacing a
                    // cryptic "expected expression, found '=>'" on the next case).
                    // Same-line RHS crosses no INDENT, so the depth is 0 and the post
                    // logic is a no-op — byte-identical to before for unwrapped cases.
                    let mut wrap_depth = 0i32;
                    loop {
                        match self.current().kind {
                            TokenKind::Indent => wrap_depth += 1,
                            TokenKind::Dedent => wrap_depth -= 1,
                            TokenKind::Newline => {}
                            _ => break,
                        }
                        self.advance();
                    }
                    let right = self.parse_expr()?;
                    // Span ends at the RHS (capture before skipping trailing
                    // formatting, which would otherwise point the span at the next
                    // case's start).
                    let end_line = self.current().line;
                    let end_col = self.current().col;
                    self.skip_newlines();
                    // Consume the DEDENT(s) that close the wrap; a further DEDENT that
                    // ends the case block is left for the loop's `is_dedent` check.
                    while wrap_depth > 0 && matches!(self.current().kind, TokenKind::Dedent) {
                        self.advance();
                        wrap_depth -= 1;
                    }
                    cases.push((left, right));
                    case_spans.push(SourceSpan {
                        line: start_line,
                        col: start_col,
                        end_line,
                        end_col,
                    });
                    self.skip_newlines();
                }

                // Oracle v1: cartesian expand explicit cases × given
                // domains. A user-written case list of M entries with
                // K givens whose domain sizes are [n_1, …, n_K] gets
                // M·Π n_k expanded cases, each carrying its own per-
                // given binding snapshot. This matches the law-form
                // expansion rule and lets a single verify block cover
                // multiple stub combinations (happy / error / edge).
                // Locals substitute first inside each expansion because
                // they may reference given names (e.g.
                // `expect = rnd(root, 0, 1, 6)`).
                if cases_givens.iter().any(|g| {
                    matches!(
                        &g.domain,
                        crate::ast::VerifyGivenDomain::Explicit(_)
                            | crate::ast::VerifyGivenDomain::IntRange { .. }
                    )
                }) {
                    let mut total = cases.len();
                    for g in &cases_givens {
                        let len = Self::domain_len(&g.domain);
                        if len == 0 {
                            return Err(self.error(format!(
                                "given '{}' has empty domain in verify trace block",
                                g.name
                            )));
                        }
                        total = total.checked_mul(len).ok_or_else(|| {
                            self.error("verify trace expands to too many cases".to_string())
                        })?;
                    }
                    if total > Self::VERIFY_LAW_MAX_CASES {
                        return Err(self.error(format!(
                            "verify trace expands to {} cases (max {})",
                            total,
                            Self::VERIFY_LAW_MAX_CASES
                        )));
                    }

                    let original_cases = std::mem::take(&mut cases);
                    let original_spans = std::mem::take(&mut case_spans);
                    let original_locals = local_bindings.clone();
                    let mut expanded_cases: Vec<(Spanned<Expr>, Spanned<Expr>)> =
                        Vec::with_capacity(total);
                    let mut expanded_spans: Vec<SourceSpan> = Vec::with_capacity(total);
                    let mut expanded_case_givens: Vec<Vec<(String, Spanned<Expr>)>> =
                        Vec::with_capacity(total);

                    #[allow(clippy::too_many_arguments)]
                    fn expand_one(
                        givens: &[crate::ast::VerifyGiven],
                        idx: usize,
                        bindings: &mut std::collections::HashMap<String, Spanned<Expr>>,
                        base_left: &Spanned<Expr>,
                        base_right: &Spanned<Expr>,
                        base_span: &SourceSpan,
                        locals: &[(String, Spanned<Expr>)],
                        out_cases: &mut Vec<(Spanned<Expr>, Spanned<Expr>)>,
                        out_spans: &mut Vec<SourceSpan>,
                        out_givens: &mut Vec<Vec<(String, Spanned<Expr>)>>,
                    ) {
                        if idx == givens.len() {
                            let mut left =
                                super::blocks::Parser::substitute_expr(base_left, bindings);
                            let mut right =
                                super::blocks::Parser::substitute_expr(base_right, bindings);
                            for (name, value) in locals {
                                // Substitute bindings into the local's value
                                // first (locals may reference givens).
                                let mut resolved_value = value.clone();
                                for (g_name, g_val) in bindings.iter() {
                                    substitute_ident(&mut resolved_value, g_name, g_val);
                                }
                                substitute_ident(&mut left, name, &resolved_value);
                                substitute_ident(&mut right, name, &resolved_value);
                            }
                            out_cases.push((left, right));
                            out_spans.push(base_span.clone());
                            out_givens.push(
                                givens
                                    .iter()
                                    .filter_map(|g| {
                                        bindings.get(&g.name).map(|e| (g.name.clone(), e.clone()))
                                    })
                                    .collect(),
                            );
                            return;
                        }
                        let given = &givens[idx];
                        for value in super::blocks::Parser::domain_values(&given.domain) {
                            bindings.insert(given.name.clone(), value);
                            expand_one(
                                givens,
                                idx + 1,
                                bindings,
                                base_left,
                                base_right,
                                base_span,
                                locals,
                                out_cases,
                                out_spans,
                                out_givens,
                            );
                            bindings.remove(&given.name);
                        }
                    }

                    for ((base_left, base_right), base_span) in
                        original_cases.iter().zip(original_spans.iter())
                    {
                        let mut bindings = std::collections::HashMap::new();
                        expand_one(
                            &cases_givens,
                            0,
                            &mut bindings,
                            base_left,
                            base_right,
                            base_span,
                            &original_locals,
                            &mut expanded_cases,
                            &mut expanded_spans,
                            &mut expanded_case_givens,
                        );
                    }

                    cases = expanded_cases;
                    case_spans = expanded_spans;
                    case_givens = expanded_case_givens;
                    cases_givens_out = cases_givens.clone();
                    local_bindings.clear();
                } else if !local_bindings.is_empty() {
                    // No multi-value givens: legacy path, just
                    // substitute locals (keeps tests green).
                    for (left, right) in cases.iter_mut() {
                        for (name, value) in &local_bindings {
                            substitute_ident(left, name, value);
                            substitute_ident(right, name, value);
                        }
                    }
                }

                // Per-case given bindings fallback for the verify
                // runner: expansion above only kicks in when a given
                // has Explicit or IntRange domain. For any other shape
                // (or if `cases_givens` is empty after expansion
                // cleared it), layer given-bound first-value bindings
                // on top of each case.
                if !cases_givens.is_empty() && case_givens.is_empty() {
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
                    cases_givens_out = cases_givens.clone();
                }
                if cases_givens_out.is_empty() && !cases_givens.is_empty() {
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
        let case_hostile_origins = vec![false; cases.len()];
        let case_hostile_profiles = vec![Vec::new(); cases.len()];
        let case_reverse_order = vec![false; cases.len()];
        Ok(VerifyBlock {
            fn_name,
            line,
            cases,
            case_spans,
            case_givens,
            case_hostile_origins,
            case_hostile_profiles,
            case_reverse_order,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn bare(e: Expr) -> Spanned<Expr> {
        Spanned::new(e, 1)
    }

    fn int(n: i64) -> Spanned<Expr> {
        bare(Expr::Literal(Literal::Int(n)))
    }

    fn ident(s: &str) -> Spanned<Expr> {
        bare(Expr::Ident(s.to_string()))
    }

    #[test]
    fn substitute_ident_respects_pattern_shadowing_constructor() {
        // let x = 1; match Option.Some(2) { Option.Some(x) -> x, None -> 0 }
        // The pattern arm binds `x` to 2, so the outer `x = 1`
        // substitution must NOT rewrite `x` in that arm's body.
        let mut expr = bare(Expr::Match {
            subject: Box::new(bare(Expr::Constructor(
                "Option.Some".to_string(),
                Some(Box::new(int(2))),
            ))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Constructor("Option.Some".to_string(), vec!["x".to_string()]),
                    body: Box::new(ident("x")),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                    body: Box::new(int(0)),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        });
        substitute_ident(&mut expr, "x", &int(1));
        let Expr::Match { arms, .. } = &expr.node else {
            panic!("expected match");
        };
        // The Some(x) arm body must still be `Ident("x")`, not `Literal(1)`.
        match &arms[0].body.node {
            Expr::Ident(s) => assert_eq!(s, "x"),
            other => panic!(
                "pattern-bound x was clobbered by outer substitution: {:?}",
                other
            ),
        }
    }

    #[test]
    fn substitute_ident_still_rewrites_non_shadowed_arm() {
        // match Option.Some(2) { Option.Some(_) -> x, _ -> 0 }
        // None arm binds nothing; outer `x` should be rewritten there.
        let mut expr = bare(Expr::Match {
            subject: Box::new(bare(Expr::Constructor(
                "Option.Some".to_string(),
                Some(Box::new(int(2))),
            ))),
            arms: vec![
                MatchArm {
                    pattern: Pattern::Wildcard,
                    body: Box::new(ident("x")),
                    binding_slots: std::sync::OnceLock::new(),
                },
                MatchArm {
                    pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                    body: Box::new(ident("x")),
                    binding_slots: std::sync::OnceLock::new(),
                },
            ],
        });
        substitute_ident(&mut expr, "x", &int(7));
        let Expr::Match { arms, .. } = &expr.node else {
            panic!("expected match");
        };
        for arm in arms {
            assert!(
                matches!(&arm.body.node, Expr::Literal(Literal::Int(7))),
                "non-shadowing arm body should be substituted; got {:?}",
                arm.body.node
            );
        }
    }

    #[test]
    fn substitute_expr_respects_pattern_shadowing_tuple() {
        // match (1, 2) { (a, b) -> a + b }
        // Outer `a = 99` must NOT overwrite the pattern's a.
        let input = bare(Expr::Match {
            subject: Box::new(bare(Expr::Tuple(vec![int(1), int(2)]))),
            arms: vec![MatchArm {
                pattern: Pattern::Tuple(vec![
                    Pattern::Ident("a".to_string()),
                    Pattern::Ident("b".to_string()),
                ]),
                body: Box::new(bare(Expr::BinOp(
                    BinOp::Add,
                    Box::new(ident("a")),
                    Box::new(ident("b")),
                ))),
                binding_slots: std::sync::OnceLock::new(),
            }],
        });
        let mut bindings = std::collections::HashMap::new();
        bindings.insert("a".to_string(), int(99));
        let out = Parser::substitute_expr(&input, &bindings);
        let Expr::Match { arms, .. } = &out.node else {
            panic!("expected match");
        };
        let Expr::BinOp(_, left, _) = &arms[0].body.node else {
            panic!("expected BinOp body");
        };
        assert!(
            matches!(&left.node, Expr::Ident(s) if s == "a"),
            "pattern-bound `a` shadowed outer binding but substitute_expr \
             clobbered it: {:?}",
            left.node
        );
    }
}
