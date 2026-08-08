//! Derived gate for the literal smart-constructor discharge.
//!
//! A call to a recognized `List<Int>` refinement's smart constructor whose
//! single argument is a syntactic list of integer literals, every one of
//! them inside the refinement's OWN proven element interval, cannot fail:
//! the constructor's validating predicate is decided at compile time. Such
//! a call therefore types as the refined type itself instead of
//! `Result<T, String>`, and lowers straight to the carrier construction.
//!
//! ```text
//! Bytes.fromList([1, 2, 3])   : Bytes                  (discharged)
//! Bytes.fromList([1, 256])    : Result<Bytes, String>  (out of interval)
//! Bytes.fromList(values)      : Result<Bytes, String>  (not a literal list)
//! ```
//!
//! # The gate is derived, never named
//!
//! Nothing here mentions `Bytes`, `fromList`, or `0..=255`. The table is
//! built from two existing recognizers:
//!
//! * [`crate::analysis::shape::detect_module_patterns`] finds every
//!   `RefinementSmartConstructor` — the single-field record plus its
//!   validating `match pred(x) { true -> Ok(T(f = x)); false -> Err(…) }`
//!   constructor — and hands back the carrier field and the predicate AST.
//! * `packed_sequence::element_interval_from_predicate` turns that
//!   predicate into the per-element interval, via the same recursive-`all`
//!   recognizer and the same [`crate::ir::interval::interval_of_invariant`]
//!   engine the wasm-gc packed layout is derived from.
//!
//! Sharing the second step is what makes the discharge safe on wasm-gc: a
//! packed carrier stores its elements in a raw `i8`/`i16` array with no
//! range check, so "the discharge admits it" and "the packed layout can
//! store it" MUST be the same predicate. They are — literally the same
//! function.
//!
//! # Boundary
//!
//! * Every callee spelling that DENOTES the recognized constructor decides
//!   the same way — qualified (`Bytes.fromList(…)`) and bare in-module
//!   (`fromList(…)` inside `stdlib/bytes.av` itself) alike. Spelling
//!   insensitivity is not a convenience: the wasm-gc backend re-resolves a
//!   FLATTENED compile unit in which `flatten_multimodule` has renamed a
//!   dependency's `fromList` to the entry-scope `Dep_fromList` and
//!   rewritten BOTH the qualified and the in-module call sites to that one
//!   bare name. After the flatten the two spellings are indistinguishable,
//!   so a spelling-sensitive rule would discharge before it and not after
//!   — the checked/unchecked fork this rule must not create. A bare
//!   spelling shared by two recognized constructors is fail-closed: it
//!   denotes neither.
//! * Exactly one argument, and it must be a syntactic list literal whose
//!   every element is a plain integer literal with at most one unary minus
//!   (`crate::ast::literal_int_list_elements`). An identifier, a call, a
//!   `BigInt` literal, or any computed list declines.
//! * Every element must be inside the derived interval. `[65, 256]`
//!   declines against `[0, 255]`.
//! * An EMPTY list literal discharges: every element is in range
//!   vacuously, and the constructor's predicate is `true` on `[]` by the
//!   recognized shape's own base case.

use std::collections::HashSet;

use crate::analysis::shape::{ModulePattern, detect_module_patterns};
use crate::ast::{Expr, FnDef, Spanned, TopLevel};
use crate::codegen::ModuleInfo;
use crate::ir::SymbolTable;
use crate::ir::interval::Interval;

/// One recognized smart constructor over a `List<Int>` carrier whose
/// element interval the refinement itself proves.
#[derive(Debug, Clone, PartialEq)]
pub struct ListRefinementCtor {
    /// Dependency-module prefix that owns the refinement (`"Bytes"`), or
    /// `None` when the refinement is declared in the entry file.
    pub scope: Option<String>,
    /// Bare source name of the refined record (`"Bytes"`).
    pub type_name: String,
    /// The record's single carrier field (`"values"`).
    pub carrier_field: String,
    /// Bare source name of the smart constructor (`"fromList"`).
    pub constructor_fn: String,
    /// Interval proven for EVERY element of the carrier list.
    pub element_interval: Interval,
}

/// Every literal-dischargeable smart constructor in one compilation,
/// addressed by the qualified callee spelling a call site writes.
#[derive(Debug, Clone, Default)]
pub struct LiteralRefinementTable {
    ctors: Vec<ListRefinementCtor>,
    /// Prefix an entry-scope refinement is addressable under, taken from
    /// the entry file's own `module X` declaration. Aver lets a module
    /// spell its own members qualified, so `X.fromList([…])` in an entry
    /// file that declares `module X` reaches the same constructor.
    entry_prefix: Option<String>,
}

impl LiteralRefinementTable {
    /// Recognize every dischargeable constructor across the entry file and
    /// its dependency modules. Unrecognized predicate shapes, non-`List<Int>`
    /// carriers, and open or non-`i64` intervals are all omitted — the table
    /// is fail-closed, and an absent entry simply keeps the `Result` path.
    pub fn build(
        entry_items: &[TopLevel],
        dep_modules: &[ModuleInfo],
        symbols: &SymbolTable,
    ) -> Self {
        // Cheap pre-filter: the rule can only ever fire for a
        // single-field product whose carrier is `List<Int>`. Programs
        // without one — the overwhelming majority — skip the pattern
        // detection entirely.
        let has_candidate_carrier = entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .chain(dep_modules.iter().flat_map(|m| m.type_defs.iter()))
            .any(is_int_list_carrier_product);
        if !has_candidate_carrier {
            return Self::default();
        }

        let entry_fns: Vec<&FnDef> = entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .filter(|fd| crate::codegen::common::is_pure_fn(fd))
            .collect();

        // A bare record name declared in more than one scope would make the
        // per-scope lookup ambiguous downstream; the packed-layout table
        // declines those, so decline them here too.
        let mut seen: HashSet<(Option<String>, String)> = HashSet::new();
        let mut ctors = Vec::new();
        for pattern in detect_module_patterns(entry_items, dep_modules) {
            let ModulePattern::RefinementSmartConstructor {
                scope,
                type_name,
                carrier_field,
                carrier_type,
                constructor_fn,
                param_name,
                predicate,
            } = pattern
            else {
                continue;
            };
            if !is_int_list(&carrier_type) {
                continue;
            }
            let scope_fns: Vec<&FnDef> = match scope.as_deref() {
                None => entry_fns.clone(),
                Some(prefix) => dep_modules
                    .iter()
                    .filter(|m| m.prefix == prefix)
                    .flat_map(|m| m.fn_defs.iter())
                    .filter(|fd| crate::codegen::common::is_pure_fn(fd))
                    .collect(),
            };
            let resolve = |expr: &Spanned<Expr>| {
                let mut rctx = crate::ir::hir::ResolveCtx::new(symbols);
                rctx.current_module = scope.clone();
                let stmt = crate::ast::Stmt::Expr(expr.clone());
                match crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt) {
                    crate::ir::hir::ResolvedStmt::Expr(s) => s,
                    crate::ir::hir::ResolvedStmt::Binding { value, .. } => value,
                }
            };
            let Some(element_interval) =
                crate::codegen::proof_lower::packed_sequence::element_interval_from_predicate(
                    &predicate,
                    &param_name,
                    &scope_fns,
                    &resolve,
                )
            else {
                continue;
            };
            if !seen.insert((scope.clone(), type_name.clone())) {
                continue;
            }
            ctors.push(ListRefinementCtor {
                scope,
                type_name,
                carrier_field,
                constructor_fn,
                element_interval,
            });
        }

        let entry_prefix = entry_items.iter().find_map(|item| match item {
            TopLevel::Module(m) => Some(m.name.clone()),
            _ => None,
        });

        Self {
            ctors,
            entry_prefix,
        }
    }

    /// `true` when nothing in this program is dischargeable — lets callers
    /// skip the per-call-site work entirely.
    pub fn is_empty(&self) -> bool {
        self.ctors.is_empty()
    }

    /// Decide the discharge for one call site.
    ///
    /// `callee` is the dotted spelling the source wrote — qualified
    /// (`"Bytes.fromList"`) or bare (`"fromList"` from inside the owning
    /// module). Returns the constructor whose refined type the call now
    /// produces, or `None` to keep the declared `Result<T, E>` signature.
    ///
    /// Every spelling that DENOTES the recognized constructor decides the
    /// same way, deliberately: the wasm-gc backend flattens a dependency's
    /// `fromList` and every one of its call sites — qualified and
    /// in-module alike — into a single bare `Dep_fromList` before
    /// re-resolving, so a spelling-sensitive rule would discharge before
    /// the flatten and not after.
    pub fn discharge(&self, callee: &str, args: &[Spanned<Expr>]) -> Option<&ListRefinementCtor> {
        if args.len() != 1 {
            return None;
        }
        let ctor = self.resolve_ctor(callee)?;
        let elements = crate::ast::literal_int_list_elements(&args[0])?;
        elements
            .iter()
            .all(|k| ctor.element_interval.contains_point(*k))
            .then_some(ctor)
    }

    /// Which recognized constructor a callee spelling denotes, if any.
    /// Fail-closed on ambiguity: when two refinements in the program share
    /// a bare constructor name, a bare call site denotes neither.
    fn resolve_ctor(&self, callee: &str) -> Option<&ListRefinementCtor> {
        // Qualified spelling: `<dep prefix>.<ctor>`, or `<entry module
        // name>.<ctor>` for a refinement declared in the entry file.
        if let Some((prefix, fn_name)) = callee.rsplit_once('.')
            && let Some(ctor) = self.ctors.iter().find(|c| {
                c.constructor_fn == fn_name
                    && match c.scope.as_deref() {
                        Some(scope) => scope == prefix,
                        None => self.entry_prefix.as_deref() == Some(prefix),
                    }
            })
        {
            return Some(ctor);
        }
        // Bare spelling: an in-module call, or a post-flatten call to the
        // prefixed name flatten gave the dependency's function.
        let mut matches = self.ctors.iter().filter(|c| c.constructor_fn == callee);
        let first = matches.next()?;
        matches.next().is_none().then_some(first)
    }
}

/// Every call site in `items` the discharge rewrites, as
/// `(line, qualified callee)`, in source order.
///
/// Exists for the self-host boundary: the Aver-in-Aver resolver has no
/// refinement recognizer, so a discharged program would build a guest
/// `Result` where the host typechecker produced the refined type. That
/// divergence is SILENT (the guest fails much later, or not at all), so
/// the self-host driver refuses such a program up front and points at
/// the exact call sites.
pub fn discharge_sites(table: &LiteralRefinementTable, items: &[TopLevel]) -> Vec<(usize, String)> {
    let mut out = Vec::new();
    if table.is_empty() {
        return out;
    }
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                for stmt in fd.body.stmts() {
                    let (crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e)) = stmt;
                    walk(table, e, &mut out);
                }
            }
            TopLevel::Stmt(crate::ast::Stmt::Binding(_, _, e))
            | TopLevel::Stmt(crate::ast::Stmt::Expr(e)) => walk(table, e, &mut out),
            TopLevel::Verify(block) => {
                for (left, right) in &block.cases {
                    walk(table, left, &mut out);
                    walk(table, right, &mut out);
                }
            }
            TopLevel::Module(_) | TopLevel::Decision(_) | TopLevel::TypeDef(_) => {}
        }
    }
    out.sort_by_key(|(line, _)| *line);
    out
}

/// Exhaustive `Expr` walk. Deliberately has NO catch-all arm: a new
/// expression form must be classified here explicitly, or the self-host
/// rejection could silently miss a discharged call nested inside it.
fn walk(table: &LiteralRefinementTable, expr: &Spanned<Expr>, out: &mut Vec<(usize, String)>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(dotted) = crate::codegen::common::expr_to_dotted_name(&callee.node)
                && table.discharge(&dotted, args).is_some()
            {
                out.push((expr.line, dotted));
            }
            walk(table, callee, out);
            for a in args {
                walk(table, a, out);
            }
        }
        Expr::Attr(obj, _) => walk(table, obj, out),
        Expr::BinOp(_, l, r) => {
            walk(table, l, out);
            walk(table, r, out);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) | Expr::Constructor(_, Some(inner)) => {
            walk(table, inner, out)
        }
        Expr::Match { subject, arms } => {
            walk(table, subject, out);
            for arm in arms {
                walk(table, &arm.body, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    walk(table, inner, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                walk(table, item, out);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk(table, k, out);
                walk(table, v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                walk(table, value, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk(table, base, out);
            for (_, value) in updates {
                walk(table, value, out);
            }
        }
        Expr::TailCall(data) => {
            for a in &data.args {
                walk(table, a, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Constructor(_, None) | Expr::Resolved { .. } => {}
    }
}

fn is_int_list(annotation: &str) -> bool {
    matches!(
        crate::types::parse_type_str(annotation),
        crate::ast::Type::List(inner) if *inner == crate::ast::Type::Int
    )
}

fn is_int_list_carrier_product(td: &crate::ast::TypeDef) -> bool {
    matches!(
        td,
        crate::ast::TypeDef::Product { fields, .. }
            if fields.len() == 1 && is_int_list(&fields[0].1)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table_for(entry: &str, dep: Option<(&str, &str)>) -> LiteralRefinementTable {
        let parse = |src: &str| {
            let mut lexer = crate::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().expect("lex");
            crate::parser::Parser::new(tokens).parse().expect("parse")
        };
        let entry_items = parse(entry);
        let dep_modules: Vec<ModuleInfo> = dep
            .map(|(prefix, src)| {
                let items = parse(src);
                vec![ModuleInfo {
                    prefix: prefix.to_string(),
                    depends: Vec::new(),
                    type_defs: items
                        .iter()
                        .filter_map(|i| match i {
                            TopLevel::TypeDef(td) => Some(td.clone()),
                            _ => None,
                        })
                        .collect(),
                    fn_defs: items
                        .iter()
                        .filter_map(|i| match i {
                            TopLevel::FnDef(fd) => Some(fd.clone()),
                            _ => None,
                        })
                        .collect(),
                    verify_laws: Vec::new(),
                    analysis: None,
                }]
            })
            .unwrap_or_default();
        let symbols = SymbolTable::build(&entry_items, &dep_modules);
        LiteralRefinementTable::build(&entry_items, &dep_modules, &symbols)
    }

    const OCTETS: &str = r#"
module Octets
    intent = "structural refinement with no standard-library name in sight"
    exposes [fromList]
    exposes opaque [Octets]
    effects []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")
"#;

    const CONSUMER: &str = r#"
module Consumer
    intent = "calls the refinement's smart constructor"
    depends [Octets]
    exposes [go]
    effects []

fn go() -> Int
    1
"#;

    fn list(src: &str) -> Spanned<Expr> {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let items = crate::parser::Parser::new(tokens).parse().expect("parse");
        match items.into_iter().next() {
            Some(TopLevel::Stmt(crate::ast::Stmt::Expr(e))) => e,
            other => panic!("expected an expression, got {other:?}"),
        }
    }

    #[test]
    fn derives_the_element_interval_without_naming_the_refinement() {
        let table = table_for(CONSUMER, Some(("Octets", OCTETS)));
        assert_eq!(
            table.ctors,
            vec![ListRefinementCtor {
                scope: Some("Octets".to_string()),
                type_name: "Octets".to_string(),
                carrier_field: "values".to_string(),
                constructor_fn: "fromList".to_string(),
                element_interval: Interval::between(0, 255),
            }]
        );
    }

    #[test]
    fn derives_the_element_interval_for_the_real_standard_library_bytes_module() {
        let table = table_for(
            CONSUMER,
            Some(("Bytes", include_str!("../../stdlib/bytes.av"))),
        );
        assert_eq!(
            table
                .ctors
                .iter()
                .find(|c| c.type_name == "Bytes")
                .map(|c| c.element_interval),
            Some(Interval::between(0, 255))
        );
    }

    #[test]
    fn discharges_only_all_literal_in_interval_lists() {
        let table = table_for(CONSUMER, Some(("Octets", OCTETS)));
        let discharges = |src: &str| table.discharge("Octets.fromList", &[list(src)]).is_some();

        assert!(discharges("[1, 2, 3]"));
        assert!(discharges("[]"));
        assert!(discharges("[0, 255]"));
        // Out of the DERIVED interval, not a hardcoded range.
        assert!(!discharges("[65, 256]"));
        assert!(!discharges("[-1]"));
        // Beyond i64 — the syntactic predicate declines BigInt outright.
        assert!(!discharges("[65, 1208925819614629174706176]"));
        // Not a syntactic list of literals.
        assert!(!discharges("values"));
        assert!(!discharges("[double(0)]"));
        assert!(!discharges("List.concat([1], [2])"));
    }

    #[test]
    fn accepts_every_spelling_that_denotes_the_constructor() {
        let table = table_for(CONSUMER, Some(("Octets", OCTETS)));
        // Qualified, and the bare in-module / post-flatten spelling.
        assert!(
            table
                .discharge("Octets.fromList", &[list("[1, 2]")])
                .is_some()
        );
        assert!(table.discharge("fromList", &[list("[1, 2]")]).is_some());
        // A different module's same-named function denotes nothing here.
        assert!(
            table
                .discharge("Tree.fromList", &[list("[1, 2]")])
                .is_none()
        );
        assert!(table.discharge("toList", &[list("[1, 2]")]).is_none());
    }

    #[test]
    fn a_bare_spelling_shared_by_two_refinements_is_fail_closed() {
        // Two recognized constructors with the same bare name: the
        // qualified spellings still decide, the bare one denotes neither.
        let second = r#"
record Nibbles
    values: List<Int>

fn inNibbleRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 15)
            true -> inNibbleRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Nibbles, String>
    match inNibbleRange(xs)
        true -> Result.Ok(Nibbles(values = xs))
        false -> Result.Err("oob")
"#;
        let dep = format!("{OCTETS}{second}");
        let table = table_for(CONSUMER, Some(("Octets", &dep)));
        assert_eq!(table.ctors.len(), 2, "expected two recognized constructors");
        assert!(table.discharge("fromList", &[list("[1, 2]")]).is_none());
        assert!(
            table
                .discharge("Octets.fromList", &[list("[1, 2]")])
                .is_some()
        );
    }

    #[test]
    fn declines_a_record_with_no_smart_constructor() {
        let src = r#"
module Local
    intent = "a bare record that never validates anything"
    effects []

record Octets
    values: List<Int>

fn go() -> Int
    1
"#;
        let table = table_for(src, None);
        assert!(table.is_empty());
    }

    #[test]
    fn addresses_an_entry_scope_refinement_through_the_entry_module_prefix() {
        let table = table_for(OCTETS, None);
        assert!(table.discharge("Octets.fromList", &[list("[7]")]).is_some());
        assert!(
            table
                .discharge("Octets.fromList", &[list("[700]")])
                .is_none()
        );
    }
}
