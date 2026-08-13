//! Grammar for the two declarations only a capability module carries:
//! `operation` (a signature with no body, plus model attributes) and
//! `opaque NAME` (a type with no representation).
//!
//! Both words are CONTEXTUAL. Neither is in the lexer's keyword table,
//! because both are live identifiers in the corpus — `opaque` already
//! modifies `exposes`, and nothing stops a program from binding a value
//! called `operation`. They are recognised only in top-level item
//! position, by `Parser::parse_top_level`.
//!
//! Why `operation` is not a body-less `fn`: `parse_fn` accepts a `fn`
//! with no body, and `parse_fn_body` routes an unrecognised
//! `name = value` line to `parse_binding`. An `oracle = generative`
//! line under a `fn` therefore parses as an ordinary local binding and
//! the strongest residual signal is `warning[unused-binding]` — the
//! whole model half would become dead code that type-checks.

use super::*;

impl Parser {
    /// `operation name(p: T, ...) -> R` followed by an optional
    /// indented block: a `? "doc"` line and `name = value` attributes.
    pub(super) fn parse_operation(&mut self) -> Result<Operation, ParseError> {
        let line = self.current().line;
        self.advance(); // consume the contextual `operation`

        let name_tok =
            self.expect_kind(&TokenKind::Ident(String::new()), "Expected operation name")?;
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
        let mut oracle = None;
        let mut replay = None;
        let mut hostile = Vec::new();
        let mut unmodelled = Vec::new();

        if self.is_indent() {
            self.advance(); // consume INDENT
            self.skip_newlines();

            // Optional description, same shape as `fn`'s.
            if self.check_exact(&TokenKind::Question) {
                self.advance();
                let text = self
                    .parse_inline_string_with_continuations()
                    .map_err(|_| self.error("Expected string after '?'".to_string()))?;
                desc = Some(text);
                self.skip_newlines();
            }

            while !self.is_dedent() && !self.is_eof() {
                if self.is_newline() {
                    self.advance();
                    continue;
                }

                let field_tok = self.expect_kind(
                    &TokenKind::Ident(String::new()),
                    "Expected operation field name",
                )?;
                let field_line = field_tok.line;
                let field_name = match field_tok.kind {
                    TokenKind::Ident(s) => s,
                    _ => unreachable!(),
                };

                self.expect_exact(&TokenKind::Assign)?;
                match field_name.as_str() {
                    "oracle" => {
                        Self::reject_repeat(&oracle.is_some(), "oracle", field_line)?;
                        oracle = Some(self.parse_attribute_ident("oracle")?);
                    }
                    "replay" => {
                        Self::reject_repeat(&replay.is_some(), "replay", field_line)?;
                        replay = Some(self.parse_attribute_ident("replay")?);
                    }
                    "hostile" => {
                        Self::reject_repeat(&!hostile.is_empty(), "hostile", field_line)?;
                        hostile = self.parse_bracket_ident_list()?;
                    }
                    "unmodelled" => {
                        Self::reject_repeat(&!unmodelled.is_empty(), "unmodelled", field_line)?;
                        unmodelled = self.parse_bracket_ident_list()?;
                    }
                    // The arm that makes a typo'd attribute impossible
                    // to lose. Modelled on `parse_decision`'s unknown-
                    // field rejection.
                    _ => {
                        return Err(self.error(format!(
                            "Unknown operation field '{}'. Allowed: oracle, replay, hostile, unmodelled",
                            field_name
                        )));
                    }
                }
            }

            if self.is_dedent() {
                self.advance();
            }
        }

        Ok(Operation {
            name,
            line,
            params,
            return_type,
            desc,
            oracle,
            replay,
            hostile,
            unmodelled,
        })
    }

    /// `opaque ConnectionToken` — no fields, ever. An indented block
    /// under it is refused rather than ignored: a representation would
    /// bind every provider to one implementation's internals.
    pub(super) fn parse_opaque_decl(&mut self) -> Result<CapabilityItem, ParseError> {
        let line = self.current().line;
        self.advance(); // consume the contextual `opaque`

        let name_tok = self.expect_kind(
            &TokenKind::Ident(String::new()),
            "Expected a type name after 'opaque'",
        )?;
        let name = match name_tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };

        self.skip_newlines();

        if self.is_indent() {
            return Err(self.error(format!(
                "An opaque capability type has no representation, so '{}' cannot have fields. \
                 Its layout is a provider detail; naming it here would bind every provider to \
                 one implementation's internals. Use `record {}` if you wanted an ordinary type \
                 with hidden fields, and list it under `exposes opaque [...]`.",
                name, name
            )));
        }

        Ok(CapabilityItem::Opaque { name, line })
    }

    /// A single identifier on the right of an operation attribute.
    fn parse_attribute_ident(&mut self, field: &str) -> Result<String, ParseError> {
        let tok = self.expect_kind(
            &TokenKind::Ident(String::new()),
            &format!("Expected an identifier after '{} ='", field),
        )?;
        let value = match tok.kind {
            TokenKind::Ident(s) => s,
            _ => unreachable!(),
        };
        self.skip_newlines();
        Ok(value)
    }

    fn reject_repeat(already_set: &bool, field: &str, line: usize) -> Result<(), ParseError> {
        if *already_set {
            return Err(ParseError::Error {
                msg: format!("Duplicate operation field '{}'", field),
                line,
                col: 1,
            });
        }
        Ok(())
    }
}
