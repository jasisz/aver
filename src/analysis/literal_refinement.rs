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
//! # The discharge is keyed on RESOLVED IDENTITY, never on spelling
//!
//! **Invariant.** The discharge fires only when ordinary name resolution —
//! the checker's own resolved signature, the HIR resolver's own
//! [`crate::ir::hir::ResolvedCallee::Fn`] — lands on the recognized smart
//! constructor's [`FnId`]. If the two disagree about which function the
//! call site denotes, the discharge DECLINES. That is what makes the
//! checked type and the lowered IR the same decision rather than two
//! opinions about a name.
//!
//! Spelling would be the wrong key in both directions:
//!
//! * It under-fires. The wasm-gc backend re-resolves a FLATTENED compile
//!   unit in which `flatten_multimodule` has renamed a dependency's
//!   `fromList` to the entry-scope `Dep_fromList` and rewritten BOTH the
//!   qualified and the in-module call sites to that one bare name. Keyed on
//!   identity, the flatten is invisible: the rebuilt symbol table resolves
//!   the renamed call sites to the renamed constructor's `FnId`.
//! * It OVER-fires, which is a miscompilation. An entry module that
//!   declares its own `fn fromList(xs: List<Int>) -> Int` shadows the
//!   stdlib constructor — Aver's pinned shadowing rule, so the checker
//!   types `fromList([1, 2, 3])` as `Int`. A name-keyed discharge would
//!   still have rewritten that body to a `Bytes` carrier construction, and
//!   the checked type and the emitted code would disagree.
//!
//! Two recognized constructors that collapse onto one `FnId` (one scope
//! declaring the same constructor name twice, where the symbol table keys
//! one `FnKey` to one `FnId`) are fail-closed: neither discharges, because
//! no call site can name one without naming the other.
//!
//! # Boundary
//!
//! * Exactly one argument, and it must be a syntactic list literal whose
//!   every element is a plain integer literal with at most one unary minus
//!   (`crate::ast::literal_int_list_elements`). An identifier, a call, a
//!   `BigInt` literal, or any computed list declines.
//! * Every element must be inside the derived interval. `[65, 256]`
//!   declines against `[0, 255]`.
//! * An EMPTY list literal discharges: every element is in range
//!   vacuously, and the constructor's predicate is `true` on `[]` by the
//!   recognized shape's own base case.

use std::collections::{HashMap, HashSet};

use crate::analysis::shape::{ModulePattern, detect_module_patterns};
use crate::ast::{Expr, FnDef, Spanned, TopLevel};
use crate::codegen::ModuleInfo;
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolveCtx, ResolvedCallee};
use crate::ir::identity::{FnId, FnKey};
use crate::ir::interval::Interval;

/// One recognized smart constructor over a `List<Int>` carrier whose
/// element interval the refinement itself proves.
#[derive(Debug, Clone, PartialEq)]
pub struct ListRefinementCtor {
    /// Resolved identity of the smart constructor — the ONLY key a call
    /// site is matched against. See the module-level invariant.
    pub fn_id: FnId,
    /// Dependency-module prefix that owns the refinement (`"Bytes"`), or
    /// `None` when the refinement is declared in the entry file.
    pub scope: Option<String>,
    /// Bare source name of the refined record (`"Bytes"`).
    pub type_name: String,
    /// The record's single carrier field (`"values"`).
    pub carrier_field: String,
    /// Bare source name of the smart constructor (`"fromList"`). Carried
    /// for diagnostics only; it is never a lookup key.
    pub constructor_fn: String,
    /// Interval proven for EVERY element of the carrier list.
    pub element_interval: Interval,
}

/// Every literal-dischargeable smart constructor in one compilation,
/// addressed by the [`FnId`] name resolution assigns the callee.
#[derive(Debug, Clone, Default)]
pub struct LiteralRefinementTable {
    ctors: Vec<ListRefinementCtor>,
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
            // The one key a call site is ever matched against. A
            // constructor the symbol table doesn't index cannot be named
            // by any resolved callee, so it is dropped rather than kept
            // under a spelling.
            let key = match scope.as_deref() {
                Some(prefix) => FnKey::in_module(prefix, &constructor_fn),
                None => FnKey::entry(&constructor_fn),
            };
            let Some(fn_id) = symbols.fn_id_of(&key) else {
                continue;
            };
            ctors.push(ListRefinementCtor {
                fn_id,
                scope,
                type_name,
                carrier_field,
                constructor_fn,
                element_interval,
            });
        }

        // Fail-closed on a shared identity: one scope declaring the same
        // constructor name twice collapses both refinements onto a single
        // `FnId`, and no call site can then denote one without denoting
        // the other. Neither discharges.
        let mut per_identity: HashMap<FnId, usize> = HashMap::new();
        for ctor in &ctors {
            *per_identity.entry(ctor.fn_id).or_default() += 1;
        }
        ctors.retain(|ctor| per_identity[&ctor.fn_id] == 1);

        Self { ctors }
    }

    /// `true` when nothing in this program is dischargeable — lets callers
    /// skip the per-call-site work entirely.
    pub fn is_empty(&self) -> bool {
        self.ctors.is_empty()
    }

    /// Decide the discharge for one call site.
    ///
    /// `callee` is the [`FnId`] ORDINARY NAME RESOLUTION assigned this
    /// call — the checker's own resolved signature, or the HIR resolver's
    /// own [`ResolvedCallee::Fn`]. Never a spelling: a caller that passes
    /// an identity it did not itself resolve breaks the invariant this
    /// whole module exists to hold. Returns the constructor whose refined
    /// type the call now produces, or `None` to keep the declared
    /// `Result<T, E>` signature.
    pub fn discharge(&self, callee: FnId, args: &[Spanned<Expr>]) -> Option<&ListRefinementCtor> {
        if args.len() != 1 {
            return None;
        }
        let ctor = self.ctors.iter().find(|c| c.fn_id == callee)?;
        let elements = crate::ast::literal_int_list_elements(&args[0])?;
        elements
            .iter()
            .all(|k| ctor.element_interval.contains_point(*k))
            .then_some(ctor)
    }
}

/// Every call site in `items` the discharge rewrites, as
/// `(line, callee spelling)`, in source order.
///
/// `scope` is the module prefix `items` belong to (`None` for the entry
/// file), because the callee identity is resolved exactly the way the HIR
/// resolver resolves it — through
/// [`crate::ir::hir::resolve::classify_callee`] against the same symbol
/// table and the same current-module context. Same resolver, same answer:
/// a call the rewrite will not fire on is not reported here either.
///
/// Exists for the self-host boundary: the Aver-in-Aver resolver has no
/// refinement recognizer, so a discharged program would build a guest
/// `Result` where the host typechecker produced the refined type. That
/// divergence is SILENT (the guest fails much later, or not at all), so
/// the self-host driver refuses such a program up front and points at
/// the exact call sites.
pub fn discharge_sites(
    symbols: &SymbolTable,
    scope: Option<&str>,
    items: &[TopLevel],
) -> Vec<(usize, String)> {
    let mut out = Vec::new();
    if symbols.literal_refinements().is_empty() {
        return out;
    }
    let mut ctx = ResolveCtx::new(symbols);
    ctx.current_module = scope.map(str::to_string);
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                for stmt in fd.body.stmts() {
                    let (crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e)) = stmt;
                    walk(&ctx, e, &mut out);
                }
            }
            TopLevel::Stmt(crate::ast::Stmt::Binding(_, _, e))
            | TopLevel::Stmt(crate::ast::Stmt::Expr(e)) => walk(&ctx, e, &mut out),
            TopLevel::Verify(block) => {
                for (left, right) in &block.cases {
                    walk(&ctx, left, &mut out);
                    walk(&ctx, right, &mut out);
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
fn walk(ctx: &ResolveCtx<'_>, expr: &Spanned<Expr>, out: &mut Vec<(usize, String)>) {
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let ResolvedCallee::Fn(fn_id) = crate::ir::hir::resolve::classify_callee(ctx, callee)
                && ctx
                    .symbols
                    .literal_refinements()
                    .discharge(fn_id, args)
                    .is_some()
            {
                let spelling = crate::codegen::common::expr_to_dotted_name(&callee.node)
                    .unwrap_or_else(|| ctx.symbols.fn_entry(fn_id).key.name.clone());
                out.push((expr.line, spelling));
            }
            walk(ctx, callee, out);
            for a in args {
                walk(ctx, a, out);
            }
        }
        Expr::Attr(obj, _) => walk(ctx, obj, out),
        Expr::BinOp(_, l, r) => {
            walk(ctx, l, out);
            walk(ctx, r, out);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) | Expr::Constructor(_, Some(inner)) => {
            walk(ctx, inner, out)
        }
        Expr::Match { subject, arms } => {
            walk(ctx, subject, out);
            for arm in arms {
                walk(ctx, &arm.body, out);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(inner) = part {
                    walk(ctx, inner, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                walk(ctx, item, out);
            }
        }
        Expr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk(ctx, k, out);
                walk(ctx, v, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                walk(ctx, value, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            walk(ctx, base, out);
            for (_, value) in updates {
                walk(ctx, value, out);
            }
        }
        Expr::TailCall(data) => {
            for a in &data.args {
                walk(ctx, a, out);
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

    /// Build the program's `SymbolTable` — the discharge table hangs off
    /// it, and so does the name resolution every lookup below goes
    /// through. Tests never address a constructor by spelling alone;
    /// they resolve the spelling first, exactly as the compiler does.
    fn symbols_for(entry: &str, deps: &[(&str, &str)]) -> SymbolTable {
        let parse = |src: &str| {
            let mut lexer = crate::lexer::Lexer::new(src);
            let tokens = lexer.tokenize().expect("lex");
            crate::parser::Parser::new(tokens).parse().expect("parse")
        };
        let entry_items = parse(entry);
        let dep_modules: Vec<ModuleInfo> = deps
            .iter()
            .map(|(prefix, src)| {
                let items = parse(src);
                ModuleInfo {
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
                }
            })
            .collect();
        SymbolTable::build(&entry_items, &dep_modules)
    }

    /// Decide one call site the way a compiler pass does: resolve the
    /// callee spelling against `symbols` from `scope`, then discharge on
    /// the identity that came back. An unresolvable spelling declines.
    fn discharges_from(symbols: &SymbolTable, scope: Option<&str>, call: &str) -> bool {
        let mut ctx = ResolveCtx::new(symbols);
        ctx.current_module = scope.map(str::to_string);
        let expr = expr_of(call);
        let Expr::FnCall(callee, args) = &expr.node else {
            panic!("expected a call expression, got {expr:?}");
        };
        let ResolvedCallee::Fn(fn_id) = crate::ir::hir::resolve::classify_callee(&ctx, callee)
        else {
            return false;
        };
        symbols
            .literal_refinements()
            .discharge(fn_id, args)
            .is_some()
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

    fn expr_of(src: &str) -> Spanned<Expr> {
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
        let symbols = symbols_for(CONSUMER, &[("Octets", OCTETS)]);
        let fn_id = symbols
            .fn_id_of(&FnKey::in_module("Octets", "fromList"))
            .expect("Octets.fromList must be indexed");
        assert_eq!(
            symbols.literal_refinements().ctors,
            vec![ListRefinementCtor {
                fn_id,
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
        let symbols = symbols_for(
            CONSUMER,
            &[("Bytes", include_str!("../../stdlib/bytes.av"))],
        );
        assert_eq!(
            symbols
                .literal_refinements()
                .ctors
                .iter()
                .find(|c| c.type_name == "Bytes")
                .map(|c| c.element_interval),
            Some(Interval::between(0, 255))
        );
    }

    #[test]
    fn discharges_only_all_literal_in_interval_lists() {
        let symbols = symbols_for(CONSUMER, &[("Octets", OCTETS)]);
        let discharges =
            |arg: &str| discharges_from(&symbols, None, &format!("Octets.fromList({arg})"));

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
    fn accepts_every_spelling_that_resolves_to_the_constructor() {
        let symbols = symbols_for(CONSUMER, &[("Octets", OCTETS)]);
        // Qualified from the consumer.
        assert!(discharges_from(&symbols, None, "Octets.fromList([1, 2])"));
        // Bare from inside the owning module — the in-module spelling and
        // the post-flatten spelling both resolve to the same `FnId`.
        assert!(discharges_from(
            &symbols,
            Some("Octets"),
            "fromList([1, 2])"
        ));
        // Bare from the consumer resolves to nothing at all here.
        assert!(!discharges_from(&symbols, None, "fromList([1, 2])"));
        // A different module's same-named function is a different identity.
        assert!(!discharges_from(&symbols, None, "Tree.fromList([1, 2])"));
        assert!(!discharges_from(&symbols, None, "Octets.toList([1, 2])"));
    }

    #[test]
    fn a_local_fn_shadowing_the_constructor_keeps_its_own_identity() {
        // THE MISCOMPILATION GUARD. An entry module declaring its own
        // `fromList` shadows the dependency's constructor (Aver's pinned
        // shadowing rule), so `fromList([1, 2])` in that entry denotes the
        // LOCAL fn. A spelling-keyed discharge fired here and rewrote the
        // body to a carrier construction while the checker typed the call
        // as the local fn's return type.
        let shadowing_entry = r#"
module Consumer
    intent = "declares its own fromList over the dependency's"
    depends [Octets]
    exposes [go]
    effects []

fn fromList(xs: List<Int>) -> Int
    List.len(xs)

fn go() -> Int
    fromList([1, 2])
"#;
        let symbols = symbols_for(shadowing_entry, &[("Octets", OCTETS)]);
        assert!(
            !symbols.literal_refinements().is_empty(),
            "the dependency's constructor is still recognized"
        );
        assert!(!discharges_from(&symbols, None, "fromList([1, 2])"));
        // The dependency's own constructor is untouched: still reachable
        // qualified, and still discharging.
        assert!(discharges_from(&symbols, None, "Octets.fromList([1, 2])"));
    }

    #[test]
    fn same_bare_name_in_two_modules_resolves_per_scope() {
        // Two recognized constructors, one per module, sharing a bare
        // name. Identity keying makes each in-module call discharge
        // against its OWN interval — the derived bound follows the
        // resolved callee, not the spelling.
        let nibbles = r#"
module Nibbles
    intent = "a second refinement whose constructor shares the bare name"
    exposes [fromList]
    exposes opaque [Nibbles]
    effects []

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
        let symbols = symbols_for(CONSUMER, &[("Octets", OCTETS), ("Nibbles", nibbles)]);
        assert_eq!(
            symbols.literal_refinements().ctors.len(),
            2,
            "expected two recognized constructors"
        );
        // 200 is inside Octets' interval and outside Nibbles'.
        assert!(discharges_from(&symbols, Some("Octets"), "fromList([200])"));
        assert!(!discharges_from(
            &symbols,
            Some("Nibbles"),
            "fromList([200])"
        ));
        assert!(discharges_from(&symbols, Some("Nibbles"), "fromList([15])"));
        // Qualified spellings decide the same way from any scope.
        assert!(discharges_from(&symbols, None, "Octets.fromList([200])"));
        assert!(!discharges_from(&symbols, None, "Nibbles.fromList([200])"));
    }

    #[test]
    fn two_constructors_collapsed_onto_one_identity_are_fail_closed() {
        // One module declaring `fromList` twice: the symbol table keys one
        // `FnKey` to one `FnId`, so no call site can denote one refinement
        // without denoting the other. Neither discharges.
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
        let symbols = symbols_for(CONSUMER, &[("Octets", &dep)]);
        assert!(
            symbols.literal_refinements().is_empty(),
            "a shared identity must retire both constructors, got: {:?}",
            symbols.literal_refinements().ctors
        );
        assert!(!discharges_from(&symbols, None, "Octets.fromList([1, 2])"));
        assert!(!discharges_from(
            &symbols,
            Some("Octets"),
            "fromList([1, 2])"
        ));
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
        let symbols = symbols_for(src, &[]);
        assert!(symbols.literal_refinements().is_empty());
    }

    #[test]
    fn addresses_an_entry_scope_refinement_through_the_entry_module_prefix() {
        let symbols = symbols_for(OCTETS, &[]);
        // The entry file declares `module Octets`, so both the bare and
        // the self-qualified spelling resolve to the same entry-scope fn.
        assert!(discharges_from(
            &symbols,
            Some("Octets"),
            "Octets.fromList([7])"
        ));
        assert!(discharges_from(&symbols, Some("Octets"), "fromList([7])"));
        assert!(!discharges_from(
            &symbols,
            Some("Octets"),
            "Octets.fromList([700])"
        ));
    }
}
