//! Derived gate for the literal smart-constructor discharge.
//!
//! A call to a recognized refinement's smart constructor whose single
//! argument is written out as integer literals, every one of them inside the
//! refinement's OWN proven interval, cannot fail: the constructor's
//! validating predicate is decided at compile time. Such a call therefore
//! types as the refined type itself instead of `Result<T, String>`, and
//! lowers straight to the carrier construction.
//!
//! Two carriers are recognized. A `List<Int>` field takes a list of integer
//! literals, each checked against the interval proven for every element; an
//! `Int` field takes one integer literal, checked against the interval the
//! constructor's own predicate proves.
//!
//! ```text
//! Bytes.fromList([1, 2, 3])   : Bytes                    (discharged)
//! Bytes.fromList([1, 256])    : Result<Bytes, String>    (out of interval)
//! Bytes.fromList(values)      : Result<Bytes, String>    (not a literal list)
//! Natural.fromInt(5)          : Natural                  (discharged)
//! Natural.fromInt(-1)         : Result<Natural, String>  (out of interval)
//! Natural.fromInt(2 + 3)      : Result<Natural, String>  (not a literal)
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
//! * For a `List<Int>` carrier,
//!   `packed_sequence::element_interval_from_predicate` turns that
//!   predicate into the per-element interval, via the same recursive-`all`
//!   recognizer and the same [`crate::ir::interval::interval_of_invariant`]
//!   engine the wasm-gc packed layout is derived from.
//! * For an `Int` carrier, [`crate::ir::interval::interval_of_invariant`]
//!   reads the interval straight off the predicate, exactly as
//!   `proof_lower::carrier_interval_table` does for the native-`i64`
//!   carrier layout. The engine recognizes comparisons of the parameter
//!   with integer literals joined by `Bool.and` and nothing else, and the
//!   interval it returns for those is exact, so a literal inside it makes
//!   the predicate true. A bound may be open on one side (`n >= 0`). Any
//!   other predicate yields no interval and nothing is discharged.
//!
//! Sharing the second step is what makes the discharge safe on wasm-gc: a
//! packed carrier stores its elements in a raw `i8`/`i16` array with no
//! range check, and an `Int` carrier with a bound inside `i64` is stored as
//! a raw `i64`, so "the discharge admits it" and "the layout can store it"
//! MUST be the same predicate. They are — literally the same function.
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
//! * Exactly one argument. For a `List<Int>` carrier it must be a syntactic
//!   list literal whose every element is a plain integer literal with at
//!   most one unary minus (`crate::ast::literal_int_list_elements`); for an
//!   `Int` carrier it must be one such literal
//!   (`crate::ast::single_negation_int_literal`). An identifier, a named
//!   constant, a call, arithmetic such as `2 + 3`, a `BigInt` literal, or
//!   any computed list declines. Parentheses are not part of the syntax
//!   tree, so `fromInt((5))` is `fromInt(5)`.
//! * Every literal must be inside the derived interval. `[65, 256]`
//!   declines against `[0, 255]`, and `-1` against `[0, +inf]`.
//! * An EMPTY list literal discharges: every element is in range
//!   vacuously, and the constructor's predicate is `true` on `[]` by the
//!   recognized shape's own base case.

use std::collections::{HashMap, HashSet};

use crate::analysis::shape::{ModulePattern, detect_module_patterns};
use crate::ast::{Expr, FnDef, Spanned, TopLevel};
use crate::codegen::ModuleInfo;
use crate::ir::SymbolTable;
use crate::ir::hir::{ResolveCtx, ResolvedCallee};
use crate::ir::identity::{FnId, FnKey, TypeId, TypeKey};
use crate::ir::interval::Interval;

/// What the refined record's single field holds, which decides the shape
/// of the literal argument a call must have to discharge.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LiteralCarrier {
    /// `List<Int>`: the argument is a list of integer literals.
    IntList,
    /// `Int`: the argument is one integer literal.
    Int,
}

/// One recognized smart constructor over a `List<Int>` or an `Int` carrier
/// whose interval the refinement itself proves.
#[derive(Debug, Clone, PartialEq)]
pub struct LiteralRefinementCtor {
    /// Resolved identity of the smart constructor — the ONLY key a call
    /// site is matched against. See the module-level invariant.
    pub fn_id: FnId,
    /// Resolved identity of the refined nominal record. Runtime provenance
    /// is compared by this identity, never by the source spelling.
    pub type_id: TypeId,
    /// Dependency-module prefix that owns the refinement (`"Bytes"`), or
    /// `None` when the refinement is declared in the entry file.
    pub scope: Option<String>,
    /// Bare source name of the refined record (`"Bytes"`).
    pub type_name: String,
    /// The record's single carrier field (`"values"`).
    pub carrier_field: String,
    /// What that field holds.
    pub carrier: LiteralCarrier,
    /// Bare source name of the smart constructor (`"fromList"`). Carried
    /// for diagnostics only; it is never a lookup key.
    pub constructor_fn: String,
    /// Exact carrier-projector functions in the refinement's owning module.
    /// Each has the structural body `fn p(x: T) -> List<Int> = x.<field>`.
    /// Always empty for an `Int` carrier.
    pub projector_fns: Vec<FnId>,
    /// Interval the constructor's predicate proves: of EVERY element for a
    /// `List<Int>` carrier, of the value itself for an `Int` carrier.
    pub interval: Interval,
    /// Whether every executable construction of this nominal is gated by the
    /// smart constructor (or is independently proven safe). Only closed
    /// refinements may seed runtime list provenance. Always `false` for an
    /// `Int` carrier: the literal discharge decides one written-out value
    /// and needs nothing about the record's other construction sites, and
    /// no pass reads provenance of an `Int` carrier.
    pub runtime_provenance: bool,
}

/// Every literal-dischargeable smart constructor in one compilation,
/// addressed by the [`FnId`] name resolution assigns the callee.
#[derive(Debug, Clone, Default)]
pub struct LiteralRefinementTable {
    ctors: Vec<LiteralRefinementCtor>,
}

impl LiteralRefinementTable {
    /// Recognize every dischargeable constructor across the entry file and
    /// its dependency modules. Unrecognized predicate shapes and carriers
    /// other than `List<Int>` and `Int` are omitted — the table is
    /// fail-closed, and an absent entry simply keeps the `Result` path.
    pub fn build(
        entry_items: &[TopLevel],
        dep_modules: &[ModuleInfo],
        symbols: &SymbolTable,
    ) -> Self {
        // Cheap pre-filter: the rule can only ever fire for an opaque
        // single-field product whose carrier is `List<Int>` or `Int`.
        // Programs without one — the overwhelming majority — skip the
        // pattern detection entirely.
        let has_candidate_carrier = entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .chain(dep_modules.iter().flat_map(|m| m.type_defs.iter()))
            .any(|td| {
                crate::analysis::shape::refinement_carrier(td)
                    .is_some_and(|(_, _, carrier)| literal_carrier(carrier).is_some())
            });
        if !has_candidate_carrier {
            return Self::default();
        }

        let entry_all_fns: Vec<&FnDef> = entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
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
            let Some(carrier) = literal_carrier(&carrier_type) else {
                continue;
            };
            let scope_all_fns: Vec<&FnDef> = match scope.as_deref() {
                None => entry_all_fns.clone(),
                Some(prefix) => dep_modules
                    .iter()
                    .filter(|m| m.prefix == prefix)
                    .flat_map(|m| m.fn_defs.iter())
                    .collect(),
            };
            let scope_fns: Vec<&FnDef> = scope_all_fns
                .iter()
                .copied()
                .filter(|fd| crate::codegen::common::is_pure_fn(fd))
                .collect();
            let resolve = |expr: &Spanned<Expr>| {
                let mut rctx = crate::ir::hir::ResolveCtx::new(symbols);
                rctx.current_module = scope.clone();
                let stmt = crate::ast::Stmt::Expr(expr.clone());
                match crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt) {
                    crate::ir::hir::ResolvedStmt::Expr(s) => s,
                    crate::ir::hir::ResolvedStmt::Binding { value, .. } => value,
                }
            };
            let derived = match carrier {
                LiteralCarrier::IntList => {
                    crate::codegen::proof_lower::packed_sequence::element_interval_from_predicate(
                        &predicate,
                        &param_name,
                        &scope_fns,
                        &resolve,
                    )
                }
                // The predicate itself, over the constructor's parameter:
                // the reading `proof_lower::carrier_interval_table` gives
                // the same predicate for the carrier's `i64` layout.
                LiteralCarrier::Int => {
                    let invariant = crate::ir::proof_ir::Predicate {
                        free_vars: vec![(
                            param_name.clone(),
                            crate::ir::proof_ir::QuantifierType::Plain("Int".to_string()),
                        )],
                        expr: resolve(&predicate),
                    };
                    let (interval, known) = crate::ir::interval::interval_of_invariant(&invariant);
                    known.then_some(interval)
                }
            };
            let Some(interval) = derived else {
                continue;
            };
            if !seen.insert((scope.clone(), type_name.clone())) {
                continue;
            }
            // `FnKey` is name-based within one scope. If another definition
            // shares the smart constructor's name, its call sites collapse to
            // the same identity and neither literal nor runtime discharge is
            // sound. Requiring the sole definition to be pure also prevents a
            // discharge from erasing declared effects.
            let constructor_defs = scope_all_fns
                .iter()
                .filter(|fd| fd.name == constructor_fn)
                .copied()
                .collect::<Vec<_>>();
            if constructor_defs.len() != 1
                || !crate::codegen::common::is_pure_fn(constructor_defs[0])
            {
                continue;
            }
            // The interval engine reads the predicate as a statement about
            // an integer, so the parameter it is stated over must be one.
            if carrier == LiteralCarrier::Int
                && !constructor_defs[0].params.first().is_some_and(|(_, ty)| {
                    crate::types::parse_type_str(ty) == crate::ast::Type::Int
                })
            {
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
            let type_key = match scope.as_deref() {
                Some(prefix) => TypeKey::in_module(prefix, &type_name),
                None => TypeKey::entry(&type_name),
            };
            let Some(type_id) = symbols.type_id_of(&type_key) else {
                continue;
            };
            let projector_fns = scope_fns
                .iter()
                .filter(|fd| is_exact_carrier_projector(fd, &type_name, &carrier_field))
                // Duplicate names collapse to one `FnId`; even a structurally
                // exact sibling cannot prove which body a call will execute.
                .filter(|fd| {
                    scope_all_fns
                        .iter()
                        .filter(|candidate| candidate.name == fd.name)
                        .count()
                        == 1
                })
                .filter_map(|fd| {
                    let key = match scope.as_deref() {
                        Some(prefix) => FnKey::in_module(prefix, &fd.name),
                        None => FnKey::entry(&fd.name),
                    };
                    symbols.fn_id_of(&key)
                })
                .collect();
            ctors.push(LiteralRefinementCtor {
                fn_id,
                type_id,
                scope,
                type_name,
                carrier_field,
                carrier,
                constructor_fn,
                projector_fns,
                interval,
                runtime_provenance: false,
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

        // A projected carrier proves its element interval only when every
        // inhabitant of the nominal passed an equivalent gate. Keep literal
        // discharge independent: an in-range literal remains dischargeable
        // even when the surrounding module also constructs the record
        // unsafely, but runtime provenance must fail closed in that case.
        //
        // This is a fact about lists that reach the constructor at run
        // time, so it is derived for `List<Int>` carriers only. The literal
        // discharge of an `Int` carrier decides one written-out value
        // against the predicate; another construction site cannot change
        // that answer.
        for ctor in &mut ctors {
            ctor.runtime_provenance = ctor.carrier == LiteralCarrier::IntList
                && runtime_provenance_is_closed(ctor, entry_items, dep_modules);
        }

        Self { ctors }
    }

    /// `true` when nothing in this program is dischargeable — lets callers
    /// skip the per-call-site work entirely.
    pub fn is_empty(&self) -> bool {
        self.ctors.is_empty()
    }

    /// Recognized constructors, including the identity-pinned runtime
    /// provenance metadata consumed by the MIR optimization pass.
    pub fn iter(&self) -> impl Iterator<Item = &LiteralRefinementCtor> {
        self.ctors.iter()
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
    pub fn discharge(
        &self,
        callee: FnId,
        args: &[Spanned<Expr>],
    ) -> Option<&LiteralRefinementCtor> {
        if args.len() != 1 {
            return None;
        }
        let ctor = self.ctors.iter().find(|c| c.fn_id == callee)?;
        let admitted = match ctor.carrier {
            LiteralCarrier::IntList => crate::ast::literal_int_list_elements(&args[0])?
                .iter()
                .all(|k| ctor.interval.contains_point(*k)),
            LiteralCarrier::Int => ctor
                .interval
                .contains_point(crate::ast::single_negation_int_literal(&args[0])?),
        };
        admitted.then_some(ctor)
    }
}

/// Recognize the representation-independent carrier projection that can seed
/// a runtime provenance fact. The function must live in the same scope as the
/// refinement (the caller supplies that pool), accept the nominal itself, and
/// return its sole `List<Int>` field without any computation.
fn is_exact_carrier_projector(fd: &FnDef, type_name: &str, carrier_field: &str) -> bool {
    if !crate::codegen::common::is_pure_fn(fd) || fd.params.len() != 1 {
        return false;
    }
    let (param_name, param_ty) = &fd.params[0];
    let param_matches = matches!(
        crate::types::parse_type_str(param_ty),
        crate::types::Type::Named { name, .. }
            if name.rsplit('.').next() == Some(type_name)
    );
    if !param_matches
        || !matches!(
            crate::types::parse_type_str(&fd.return_type),
            crate::types::Type::List(inner) if *inner == crate::types::Type::Int
        )
    {
        return false;
    }
    let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    matches!(
        &body.node,
        Expr::Attr(base, field)
            if field == carrier_field
                && matches!(
                    &base.node,
                    Expr::Ident(name) | Expr::Resolved { name, .. } if name == param_name
                )
    )
}

/// Prove that a nominal's carrier invariant is closed over every source-level
/// construction site. Opaque access prevents callers from constructing the
/// record, but its own module may still bypass the smart constructor. One such
/// site invalidates runtime provenance for the whole nominal.
fn runtime_provenance_is_closed(
    ctor: &LiteralRefinementCtor,
    entry_items: &[TopLevel],
    dep_modules: &[ModuleInfo],
) -> bool {
    let scan_expr = |scope: Option<&str>,
                     inside_gate: bool,
                     nominal_params: &HashSet<String>,
                     expr: &Spanned<Expr>| {
        let mut closed = true;
        crate::codegen::proof_lower::carrier_walk_expr_with_byte_payloads(
            expr,
            &HashSet::new(),
            &mut |node, byte_payloads| {
                if !closed {
                    return;
                }
                let (spelling, fields, is_update) = match node {
                    Expr::RecordCreate { type_name, fields } => {
                        (type_name.as_str(), fields.as_slice(), false)
                    }
                    Expr::RecordUpdate {
                        type_name, updates, ..
                    } => (type_name.as_str(), updates.as_slice(), true),
                    _ => return,
                };
                if !record_spelling_denotes_ctor(ctor, scope, spelling) || inside_gate {
                    return;
                }
                let independently_safe = !is_update
                    && (crate::codegen::proof_lower::construct_arg_is_in_interval(
                        fields,
                        ctor.interval,
                    ) || crate::codegen::proof_lower::construct_arg_is_finalized_bytes(
                        fields,
                        ctor.interval,
                        byte_payloads,
                    ) || crate::codegen::proof_lower::construct_arg_preserves_list_refinement(
                        fields,
                        &ctor.carrier_field,
                        nominal_params,
                    ));
                if !independently_safe {
                    closed = false;
                }
            },
        );
        closed
    };

    let scan_fn = |scope: Option<&str>, fd: &FnDef| {
        let inside_gate = scope == ctor.scope.as_deref() && fd.name == ctor.constructor_fn;
        let nominal_params: HashSet<String> = fd
            .params
            .iter()
            .filter(|(_, ty)| {
                crate::types::parse_type_str(ty)
                    .named_name()
                    .is_some_and(|name| name.rsplit('.').next() == Some(ctor.type_name.as_str()))
            })
            .map(|(name, _)| name.clone())
            .collect();
        fd.body.stmts().iter().all(|stmt| {
            let expr = match stmt {
                crate::ast::Stmt::Binding(_, _, value) | crate::ast::Stmt::Expr(value) => value,
            };
            scan_expr(scope, inside_gate, &nominal_params, expr)
        })
    };

    for item in entry_items {
        let closed = match item {
            TopLevel::FnDef(fd) => scan_fn(None, fd),
            TopLevel::Stmt(crate::ast::Stmt::Binding(_, _, value))
            | TopLevel::Stmt(crate::ast::Stmt::Expr(value)) => {
                scan_expr(None, false, &HashSet::new(), value)
            }
            _ => true,
        };
        if !closed {
            return false;
        }
    }
    for module in dep_modules {
        for fd in &module.fn_defs {
            if !scan_fn(Some(module.prefix.as_str()), fd) {
                return false;
            }
        }
    }
    true
}

fn record_spelling_denotes_ctor(
    ctor: &LiteralRefinementCtor,
    current_scope: Option<&str>,
    spelling: &str,
) -> bool {
    if current_scope == ctor.scope.as_deref() && spelling == ctor.type_name {
        return true;
    }
    match ctor.scope.as_deref() {
        Some(prefix) => spelling == format!("{prefix}.{}", ctor.type_name),
        None => false,
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
            // A capability item carries no expressions: an operation
            // has a signature and no body, and its `hostile` /
            // `unmodelled` entries are bare symbol names, not calls.
            // There is no literal to discharge, so this is a genuine
            // no-op — named explicitly rather than folded into the
            // arm below, so a later phase that gives operations an
            // expression has to come back here.
            TopLevel::Capability(_) => {}
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

/// The carrier a field annotation names, when the discharge covers it.
fn literal_carrier(annotation: &str) -> Option<LiteralCarrier> {
    match crate::types::parse_type_str(annotation) {
        crate::ast::Type::Int => Some(LiteralCarrier::Int),
        crate::ast::Type::List(inner) if *inner == crate::ast::Type::Int => {
            Some(LiteralCarrier::IntList)
        }
        _ => None,
    }
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
                let decl = crate::visibility::module_decl(&items);
                ModuleInfo {
                    bindings: crate::codegen::collect_module_bindings(&items),
                    prefix: prefix.to_string(),
                    depends: decl.map(|d| d.depends.clone()).unwrap_or_default(),
                    exposes: decl.map(|d| d.exposes.clone()).unwrap_or_default(),
                    exposes_opaque: decl.map(|d| d.exposes_opaque.clone()).unwrap_or_default(),
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
                    capability_items: Vec::new(),
                    capability_semantics: None,
                    verify_blocks: Vec::new(),
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
        let type_id = symbols
            .type_id_of(&TypeKey::in_module("Octets", "Octets"))
            .expect("Octets type must be indexed");
        assert_eq!(
            symbols.literal_refinements().ctors,
            vec![LiteralRefinementCtor {
                fn_id,
                type_id,
                scope: Some("Octets".to_string()),
                type_name: "Octets".to_string(),
                carrier_field: "values".to_string(),
                carrier: LiteralCarrier::IntList,
                constructor_fn: "fromList".to_string(),
                projector_fns: Vec::new(),
                interval: Interval::between(0, 255),
                runtime_provenance: true,
            }]
        );
    }

    #[test]
    fn derives_the_element_interval_for_the_real_standard_library_bytes_module() {
        let symbols = symbols_for(
            CONSUMER,
            &[("Bytes", include_str!("../../stdlib/bytes.av"))],
        );
        let bytes = symbols
            .literal_refinements()
            .ctors
            .iter()
            .find(|c| c.type_name == "Bytes")
            .expect("Bytes refinement");
        assert_eq!(bytes.interval, Interval::between(0, 255));
        assert!(bytes.runtime_provenance);
        assert_eq!(
            bytes.projector_fns,
            vec![
                symbols
                    .fn_id_of(&FnKey::in_module("Bytes", "octets"))
                    .expect("Bytes.octets identity")
            ]
        );
    }

    #[test]
    fn an_ungated_record_constructor_disables_runtime_provenance_only() {
        let unsafe_octets = format!(
            "{OCTETS}\n\nfn octets(value: Octets) -> List<Int>\n    value.values\n\nfn bypass(xs: List<Int>) -> Octets\n    Octets(values = xs)\n"
        );
        let symbols = symbols_for(CONSUMER, &[("Octets", &unsafe_octets)]);
        let ctor = symbols
            .literal_refinements()
            .ctors
            .iter()
            .find(|ctor| ctor.type_name == "Octets")
            .expect("literal refinement still exists");
        assert!(!ctor.runtime_provenance);
        assert_eq!(ctor.projector_fns.len(), 1);
        assert!(discharges_from(&symbols, None, "Octets.fromList([1, 2])"));
    }

    #[test]
    fn a_duplicate_projector_name_cannot_seed_runtime_provenance() {
        let ambiguous_octets = format!(
            "{OCTETS}\n\nfn octets(value: Octets) -> List<Int>\n    value.values\n\nfn octets(value: Octets) -> List<Int>\n    List.prepend(999, value.values)\n"
        );
        let symbols = symbols_for(CONSUMER, &[("Octets", &ambiguous_octets)]);
        let ctor = symbols
            .literal_refinements()
            .ctors
            .iter()
            .find(|ctor| ctor.type_name == "Octets")
            .expect("literal refinement still exists");
        assert!(ctor.runtime_provenance);
        assert!(ctor.projector_fns.is_empty());
    }

    #[test]
    fn an_ungated_top_level_constructor_disables_runtime_provenance() {
        let unsafe_octets = format!("{OCTETS}\n\nunsafe = Octets(values = [256])\n");
        let symbols = symbols_for(&unsafe_octets, &[]);
        let ctor = symbols
            .literal_refinements()
            .ctors
            .iter()
            .find(|ctor| ctor.type_name == "Octets")
            .expect("literal refinement still exists");
        assert!(!ctor.runtime_provenance);
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
        let dep = format!("{OCTETS}{second}").replace(
            "exposes opaque [Octets]",
            "exposes opaque [Octets, Nibbles]",
        );
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

    // ── `Int` carriers ──────────────────────────────────────────────────

    /// A one-field `Int` refinement whose constructor branches on `{check}`.
    fn int_refinement(check: &str) -> String {
        format!(
            r#"
module Natural
    intent = "an Int refinement with no standard-library name in sight"
    exposes [fromInt, toInt]
    exposes opaque [Natural]
    effects []

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    match {check}
        true -> Result.Ok(Natural(value = n))
        false -> Result.Err("out of range")

fn toInt(n: Natural) -> Int
    n.value
"#
        )
    }

    const INT_CONSUMER: &str = r#"
module Consumer
    intent = "calls the Int refinement's smart constructor"
    depends [Natural]
    exposes [go]
    effects []

limit = 5

fn go() -> Int
    1
"#;

    fn int_discharges(check: &str) -> impl Fn(&str) -> bool {
        let symbols = symbols_for(INT_CONSUMER, &[("Natural", &int_refinement(check))]);
        move |arg: &str| discharges_from(&symbols, None, &format!("Natural.fromInt({arg})"))
    }

    #[test]
    fn derives_the_interval_of_an_int_carrier_from_its_own_predicate() {
        let symbols = symbols_for(INT_CONSUMER, &[("Natural", &int_refinement("n >= 0"))]);
        let ctors = &symbols.literal_refinements().ctors;
        assert_eq!(ctors.len(), 1, "got: {ctors:?}");
        let ctor = &ctors[0];
        assert_eq!(ctor.carrier, LiteralCarrier::Int);
        assert_eq!(ctor.type_name, "Natural");
        assert_eq!(ctor.carrier_field, "value");
        assert_eq!(ctor.constructor_fn, "fromInt");
        // Open above: `n >= 0` proves no upper bound.
        assert_eq!(ctor.interval, Interval::ge(0));
        // Nothing reads provenance of an `Int` carrier, and its reader is
        // not a list projector.
        assert!(!ctor.runtime_provenance);
        assert!(ctor.projector_fns.is_empty());
    }

    #[test]
    fn discharges_only_a_written_out_literal_inside_an_open_interval() {
        let discharges = int_discharges("n >= 0");
        assert!(discharges("5"));
        assert!(discharges("0"));
        assert!(discharges("-0"));
        assert!(discharges("9223372036854775807"));
        // Parentheses are not in the syntax tree.
        assert!(discharges("(5)"));
        assert!(discharges("((5))"));
        // Outside the DERIVED interval.
        assert!(!discharges("-1"));
        assert!(!discharges("(-1)"));
        assert!(!discharges("-(1)"));
        // Beyond i64 — the syntactic predicate declines BigInt outright,
        // although the value is inside the open interval.
        assert!(!discharges("9223372036854775808"));
        assert!(!discharges("1208925819614629174706176"));
        // Not one written-out literal.
        assert!(!discharges("n"));
        assert!(!discharges("limit"));
        assert!(!discharges("2 + 3"));
        assert!(!discharges("- -5"));
        assert!(!discharges("Int.abs(5)"));
        assert!(!discharges("[5]"));
        assert!(!discharges("5.0"));
    }

    #[test]
    fn an_int_literal_is_checked_against_both_ends_of_a_closed_interval() {
        let discharges = int_discharges("Bool.and(n >= 0, n <= 100)");
        assert!(discharges("0"));
        assert!(discharges("100"));
        assert!(!discharges("101"));
        assert!(!discharges("-1"));
        // Strict comparisons and a literal on the left read the same way.
        let strict = int_discharges("Bool.and(0 < n, n < 10)");
        assert!(strict("1"));
        assert!(strict("9"));
        assert!(!strict("0"));
        assert!(!strict("10"));
        // A negative range takes a literal with its one minus.
        let negative = int_discharges("Bool.and(n >= -10, n <= -1)");
        assert!(negative("-10"));
        assert!(negative("-1"));
        assert!(!negative("0"));
        assert!(!negative("-11"));
    }

    #[test]
    fn a_predicate_the_interval_engine_cannot_read_discharges_nothing() {
        for check in [
            "n != 0",
            "Bool.or(n < 0, n > 10)",
            "Bool.and(n >= 0, isEven(n))",
            "n >= limit",
            "n * 2 >= 0",
            "Bool.not(n < 0)",
        ] {
            let source = format!(
                "{}\nlimit = 0\n\nfn isEven(n: Int) -> Bool\n    n == n\n",
                int_refinement(check)
            );
            let symbols = symbols_for(INT_CONSUMER, &[("Natural", &source)]);
            assert!(
                symbols.literal_refinements().is_empty(),
                "`{check}` yields no interval, got: {:?}",
                symbols.literal_refinements().ctors
            );
            assert!(!discharges_from(&symbols, None, "Natural.fromInt(5)"));
        }
    }

    #[test]
    fn an_int_record_that_is_not_opaque_or_not_an_int_discharges_nothing() {
        let open = int_refinement("n >= 0").replace("    exposes opaque [Natural]\n", "");
        let symbols = symbols_for(INT_CONSUMER, &[("Natural", &open)]);
        assert!(symbols.literal_refinements().is_empty());
        assert!(!discharges_from(&symbols, None, "Natural.fromInt(5)"));

        // `Float` and `String` carriers are outside the rule.
        let float = int_refinement("n >= 0.0")
            .replace("value: Int", "value: Float")
            .replace("n: Int", "n: Float");
        let symbols = symbols_for(INT_CONSUMER, &[("Natural", &float)]);
        assert!(symbols.literal_refinements().is_empty());
        assert!(!discharges_from(&symbols, None, "Natural.fromInt(5.0)"));
    }

    #[test]
    fn a_local_from_int_shadowing_the_constructor_keeps_its_own_identity() {
        // The entry module declares its own `fromInt`. A bare call there
        // means the local function; only the qualified spelling reaches the
        // refinement's constructor.
        let consumer = format!("{INT_CONSUMER}\nfn fromInt(n: Int) -> Int\n    n + 1\n");
        let symbols = symbols_for(&consumer, &[("Natural", &int_refinement("n >= 0"))]);
        assert!(!discharges_from(&symbols, None, "fromInt(5)"));
        assert!(discharges_from(&symbols, None, "Natural.fromInt(5)"));
        // Inside the owning module the bare spelling is the constructor.
        assert!(discharges_from(&symbols, Some("Natural"), "fromInt(5)"));
    }

    #[test]
    fn discharge_sites_reports_a_discharged_int_call() {
        let entry = format!(
            "{}\nfn five() -> Natural\n    fromInt(5)\n\nfn checked(n: Int) -> Result<Natural, String>\n    fromInt(n)\n",
            int_refinement("n >= 0")
        );
        let symbols = symbols_for(&entry, &[]);
        let items = {
            let mut lexer = crate::lexer::Lexer::new(&entry);
            crate::parser::Parser::new(lexer.tokenize().expect("lex"))
                .parse()
                .expect("parse")
        };
        let sites = discharge_sites(&symbols, None, &items);
        assert_eq!(sites.len(), 1, "got: {sites:?}");
        assert_eq!(sites[0].1, "fromInt");
    }
}
