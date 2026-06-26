//! Build `ProofIR` from a `CodegenContext`.
//!
//! The lowering producer: types live in `src/ir/proof_ir.rs`, this
//! file fills them in from a typechecked + analysed codegen
//! context. Output lands in `CodegenContext.proof_ir`; both proof
//! backends read from the same field, so any classifier-side
//! decision flows consistently to Lean and Dafny without each
//! backend re-running shape detection.
//!
//! Populates three IR sections: `refined_types` (refinement-via-
//! opaque records → Lean Subtype / Dafny subset type),
//! `fn_contracts` (per-pure-fn recursion shape: native /
//! sized-fuel / linear recurrence), and `law_theorems` (per-verify-
//! law strategy + quantifier decomposition + claim shape, with
//! Oracle-Lift'd impl-spec calls for effectful equivalence).
//!
//! `tests/proof_ir_diff.rs` pins the producer's output for each
//! canonical source pattern — divergence between the classifier and
//! the IR populator surfaces there.
//!
//! # Epic #170 Phase 7 invariant — AST discovery + typed identity
//!
//! This module is the **last consumer** of raw `crate::ast::Expr`
//! patterns in the codegen layer. That is intentional, not
//! migration debt.
//!
//! ## What's AST-shaped (syntax-discovery-only)
//!
//! Detector helpers in this file (`detect_*`, `walk_for_*`,
//! `callee_matches_name`, `call_named_args`, `binary_call_var_const`,
//! `matches_ident_expr`) walk `ast::Expr` directly. They are
//! **pattern matchers** over source shape — they look for things
//! like `match n { 0 -> base; _ -> rec(n - 1) }` or
//! `Map.has(outer(m, k), k)` to decide which `ProofStrategy` /
//! `RecursionPlan` variant lowers a given fn or law. The pattern
//! belongs in source-shape; rewriting them on `ResolvedExpr` would
//! be the same logic spelled in a different enum, no extra safety.
//!
//! Every detector helper carries a `syntax-discovery-only` comment
//! at its definition.
//!
//! ## What's identity-sensitive (typed IDs)
//!
//! Decisions that depend on **which fn / type / ctor** a name
//! refers to (not just "does this name appear") MUST go through
//! `SymbolTable` or `ProofIR.refined_types` (`TypeId`-keyed) /
//! `ProofIR.fn_contracts` (`FnId`-keyed). Examples:
//!
//! - Refinement-carrier lookups go through `find_refined_type` /
//!   `resolve_refined_type_in_with_key`, both of which canonicalise
//!   the name through the symbol table before reaching the IR map.
//! - Fn-contract lookups go through `find_fn_contract_for_fn` —
//!   pointer-eq scope on `&FnDef` resolves to the right `FnId`.
//! - The Lean native-guarded rewriter pins target by `FnId` via
//!   `rewrite_native_guarded_calls_resolved_expr` (PR 169).
//!
//! ## What stays raw-AST as a documented identity exception
//!
//! Builtin matchers (`callee_is X for X ∈ {"Bool.and", "Map.set",
//! …}`) compare against the canonical builtin namespace, which is
//! global by spec — no per-scope identity to leak. Verify-law
//! callsites all walk `vb.fn_name` (entry-only by parser grammar);
//! the `EntryFnIndex` newtype in `verify_law.rs` pins the
//! entry-only contract at the type level (PR 177).
//!
//! Full `ResolvedProofLowerView` + semantic matcher API
//! (`callee_is_builtin`, `callee_is_fn(FnId)`, `ctor_is`,
//! `ident_name`, `int_lit`) deferred per
//! `project_phase_e_scope_b_deferred` memory until a real trigger
//! lands (module-scoped verify, dotted law targets, LSP rename,
//! cross-scope inliner).

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnDef, Literal, Spanned, TopLevel, TypeDef};
use crate::codegen::common::expr_to_dotted_name;
use crate::codegen::recursion::RecursionPlan;
use crate::codegen::{CodegenContext, ModuleInfo};
use crate::ir::proof_ir::{
    DecreaseProof, FnContract, Measure, NativeIntCountdownBody, Predicate, PreservationProof,
    ProofIR, QuantifierType, RecursionContract, RefinedTypeDecl,
};

/// Backend-neutral view of the data `proof_lower` needs. Built once
/// per lowering call; lets the pipeline pass it through without
/// requiring a fully-assembled `CodegenContext` (which only exists
/// after `build_context` runs). Legacy callers still build the view
/// from `&CodegenContext` via [`ProofLowerInputs::from_ctx`].
///
/// All fields are borrows — the struct never owns memory; the pipeline
/// and `build_context` both already own the data and just lend it.
///
/// Post-Step-7c: every helper the lowerer touches
/// (`refinement_info_for`, `analyze_plans`, the `detect.rs` shape
/// checkers) reads its inputs through this view. No more
/// `&CodegenContext` reach-through — the struct stands on its own.
pub struct ProofLowerInputs<'a> {
    /// Entry-file top-level items, post-pipeline (TCO etc. applied).
    pub entry_items: &'a [TopLevel],
    /// Dependent modules already split into type/fn defs.
    pub dep_modules: &'a [ModuleInfo],
    /// Set of dep module prefix strings (e.g. `"Models.User"`).
    pub module_prefixes: &'a HashSet<String>,
    /// Recursive fn ids from the `analyze` pipeline stage. Keyed
    /// by opaque [`crate::ir::FnId`] so entry+module same-bare-name
    /// fns don't merge. Per-scope helpers below project back to
    /// `HashSet<String>` for consumers that operate on a single
    /// scope (the DAG invariant keeps bare-name unambiguous within
    /// a scope).
    pub recursive_fns: &'a HashSet<crate::ir::FnId>,
    /// Resolved-identity table (#138 phase E). When `Some`, the
    /// populate-side resolves `FnKey` / `TypeKey` to `FnId` /
    /// `TypeId` once at the IR boundary and keys `ProofIR.fn_contracts`
    /// / `ProofIR.refined_types` / `LawTheorem.fn_id` by the opaque
    /// IDs. Callers that haven't wired in the symbol-table stage
    /// pass `None` and fall through to legacy key-typed maps
    /// (transitional during phase E migration).
    pub symbol_table: &'a crate::ir::SymbolTable,
    /// Optional `ProgramShape` substrate (Stage 6b of #232). When
    /// `Some`, `refinement_info_for` reads from the typed
    /// `ModulePattern::RefinementSmartConstructor` entries instead of
    /// re-walking the AST. `None` keeps the legacy walk path —
    /// preserved for test fixtures that build `ProofLowerInputs` by
    /// hand without going through the pipeline.
    pub program_shape: Option<&'a crate::analysis::shape::ProgramShape>,
}

impl<'a> ProofLowerInputs<'a> {
    /// Build a view from a fully-assembled `CodegenContext` — used
    /// by `refresh_facts` (test helper) and by any caller that
    /// already owns a built context. Reads only the fields the
    /// lowerer actually needs.
    pub fn from_ctx(ctx: &'a CodegenContext) -> Self {
        Self {
            entry_items: &ctx.items,
            dep_modules: &ctx.modules,
            module_prefixes: &ctx.module_prefixes,
            recursive_fns: &ctx.recursive_fns,
            symbol_table: &ctx.symbol_table,
            program_shape: ctx.program_shape.as_ref(),
        }
    }

    /// All pure fn defs across entry items and dep modules, in walk
    /// order (entry first, then deps). `is_pure_fn` lives in the
    /// Lean toplevel module today; pure_fns reaches there since the
    /// pure-ness criterion is the same for every proof backend.
    pub fn pure_fns(&self) -> Vec<&'a FnDef> {
        // Order matches the legacy `lean::pure_fns(ctx)`: deps first,
        // entry last. `call_graph::ordered_fn_components` is order-
        // sensitive (SCC discovery order changes which member is
        // chosen as the representative); flipping the order shifted
        // some classifications between fuel and "outside subset".
        self.dep_modules
            .iter()
            .flat_map(|m| m.fn_defs.iter())
            .chain(self.entry_items.iter().filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            }))
            .filter(|fd| crate::codegen::common::is_pure_fn(fd))
            .collect()
    }

    /// Recursive pure fn names. Filters `recursive_fns` by pure-ness.
    /// Returns bare names (pure_fns view is the whole program here,
    /// so any FnId in `recursive_fns` that maps back to a pure fn
    /// gets its bare name surfaced for downstream classifiers).
    pub fn recursive_pure_fn_names(&self) -> HashSet<String> {
        let symbols = self.symbol_table;
        let pure_ids: HashSet<crate::ir::FnId> = self
            .pure_fns()
            .into_iter()
            .filter_map(|fd| {
                let scope = self
                    .dep_modules
                    .iter()
                    .find(|m| m.fn_defs.iter().any(|d| std::ptr::eq(d, fd)))
                    .map(|m| m.prefix.as_str());
                // **syntax-discovery-only** (epic #170 Phase 8
                // guardrail): scope was just resolved via pointer-eq
                // against dep modules — the `None` arm is the
                // correct entry-scope key by construction (same
                // shape as `fn_key_for_decl` in `codegen::common`).
                let key = match scope {
                    Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &fd.name),
                    None => crate::ir::FnKey::entry(&fd.name),
                };
                symbols.fn_id_of(&key)
            })
            .collect();
        self.recursive_fns
            .intersection(&pure_ids)
            .map(|id| symbols.fn_entry(*id).key.name.clone())
            .collect()
    }

    /// Pure fns restricted to a single scope: `None` = entry only,
    /// `Some(prefix)` = the dep module with that prefix only. Aver's
    /// module DAG invariant rules out cross-module recursion SCCs,
    /// so per-scope classification is the canonical view —
    /// `populate_fn_contracts` walks this per scope to give each
    /// `Module.fn` its own canonical key in `ir.fn_contracts`
    /// instead of letting two same-bare-name fns silently merge.
    pub fn pure_fns_in_scope(&self, scope: Option<&str>) -> Vec<&'a FnDef> {
        match scope {
            None => self
                .entry_items
                .iter()
                .filter_map(|item| match item {
                    TopLevel::FnDef(fd) => Some(fd),
                    _ => None,
                })
                .filter(|fd| crate::codegen::common::is_pure_fn(fd))
                .collect(),
            Some(prefix) => self
                .dep_modules
                .iter()
                .filter(|m| m.prefix == prefix)
                .flat_map(|m| m.fn_defs.iter())
                .filter(|fd| crate::codegen::common::is_pure_fn(fd))
                .collect(),
        }
    }

    /// Recursive pure fn names restricted to a single scope. Filters
    /// the FnId-keyed `recursive_fns` to the ones whose canonical
    /// scope matches `scope`, then projects back to bare names for
    /// scope-local consumers (DAG invariant keeps bare-name
    /// unambiguous within a single scope).
    pub fn recursive_pure_fn_names_in_scope(&self, scope: Option<&str>) -> HashSet<String> {
        let symbols = self.symbol_table;
        let pure_ids: HashSet<crate::ir::FnId> = self
            .pure_fns_in_scope(scope)
            .into_iter()
            .filter_map(|fd| {
                // **syntax-discovery-only** (epic #170 Phase 8
                // guardrail): scope is the caller's stated scope —
                // `None` = entry, `Some(prefix)` = dep module. Both
                // arms below are the correct key for the matching
                // arm; bare-name keying is safe because the caller
                // has already narrowed to a single scope.
                let key = match scope {
                    Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &fd.name),
                    None => crate::ir::FnKey::entry(&fd.name),
                };
                symbols.fn_id_of(&key)
            })
            .collect();
        self.recursive_fns
            .intersection(&pure_ids)
            .map(|id| symbols.fn_entry(*id).key.name.clone())
            .collect()
    }

    /// Iterator over (`None` = entry, `Some(prefix)` = each dep
    /// module) — drives `populate_fn_contracts`'s per-scope walk.
    pub fn scopes(&self) -> Vec<Option<String>> {
        let mut out = vec![None];
        for m in self.dep_modules {
            out.push(Some(m.prefix.clone()));
        }
        out
    }

    /// Scope of the dep module that owns `fd`, or `None` for entry
    /// module fns. Pointer-eq match against `dep_modules`, mirroring
    /// `crate::codegen::common::fn_owning_scope_for` but reading off
    /// the lowering view (which doesn't carry a full `CodegenContext`).
    pub fn fn_owning_scope(&self, fd: &FnDef) -> Option<&'a str> {
        for m in self.dep_modules {
            for f in &m.fn_defs {
                if std::ptr::eq(f, fd) {
                    return Some(m.prefix.as_str());
                }
            }
        }
        None
    }

    /// Resolve a raw-AST expression to its `ResolvedExpr` form under
    /// the given scope. ProofIR stores resolved expressions (Phase E
    /// PR 12 Scope A), so this helper is called at every producer
    /// site that lifts a `Spanned<crate::ast::Expr>` slice from the
    /// source into an IR field. Mirrors
    /// `CodegenContext::resolve_expr` but reads only the
    /// `symbol_table` carried on this view — proof lowering runs
    /// inside the pipeline, before a full `CodegenContext` exists.
    pub fn resolve_expr(
        &self,
        expr: &crate::ast::Spanned<crate::ast::Expr>,
        scope: Option<&str>,
    ) -> crate::ast::Spanned<crate::ir::hir::ResolvedExpr> {
        use crate::ir::hir::{ResolveCtx, ResolvedStmt};
        let mut rctx = ResolveCtx::new(self.symbol_table);
        rctx.current_module = scope.map(String::from);
        let stmt = crate::ast::Stmt::Expr(expr.clone());
        match crate::ir::hir::resolve::resolve_stmt_external(&rctx, &stmt) {
            ResolvedStmt::Expr(s) => s,
            ResolvedStmt::Binding { value, .. } => value,
        }
    }

    /// Names of every recursive user-defined type across entry + deps.
    pub fn recursive_type_names(&self) -> HashSet<String> {
        self.entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .chain(self.dep_modules.iter().flat_map(|m| m.type_defs.iter()))
            .filter(|td| crate::codegen::common::is_recursive_type_def(td))
            .map(|td| crate::codegen::common::type_def_name(td).to_string())
            .collect()
    }

    /// Find a fn def by name across entry + deps. Falls back to the
    /// last segment of a dotted call (e.g. `Module.fn` resolves to
    /// `fn` when no exact-match candidate exists).
    pub fn find_fn_def_by_call_name(&self, call_name: &str) -> Option<&'a FnDef> {
        let find_exact = |name: &str| -> Option<&'a FnDef> {
            self.dep_modules
                .iter()
                .flat_map(|m| m.fn_defs.iter())
                .chain(self.entry_items.iter().filter_map(|item| match item {
                    TopLevel::FnDef(fd) => Some(fd),
                    _ => None,
                }))
                .find(|fd| fd.name == name)
        };
        find_exact(call_name).or_else(|| {
            let short = call_name.rsplit('.').next()?;
            find_exact(short)
        })
    }

    /// Find a type def by bare name across entry + deps. None on miss
    /// or when the name resolves to a non-Product / non-Sum shape.
    pub fn find_type_def(&self, type_name: &str) -> Option<&'a TypeDef> {
        self.entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::TypeDef(td) => Some(td),
                _ => None,
            })
            .chain(self.dep_modules.iter().flat_map(|m| m.type_defs.iter()))
            .find(|td| crate::codegen::common::type_def_name(td) == type_name)
    }
}

/// Run every proof-export lowering in one shot — convenience for
/// callers that want a fully-populated ProofIR. The pipeline calls
/// the three `populate_*` fns directly so it can run them as
/// independent stages and short-circuit on typecheck failure.
pub fn lower(inputs: &ProofLowerInputs) -> ProofIR {
    let mut ir = ProofIR::default();
    populate_refined_types(inputs, &mut ir);
    populate_fn_contracts(inputs, &mut ir);
    populate_law_theorems(inputs, &mut ir);
    ir
}

/// Refinement-via-opaque lift. Walks every type definition (entry +
/// dep modules), classifies the records that pair a single carrier
/// field with a validating smart constructor, and emits
/// `RefinedTypeDecl` entries into `ir.refined_types`. Backends
/// (Lean → Subtype, Dafny → subset type) render these directly.
pub fn populate_refined_types(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    // Walk entry items first, then dep modules. The map is keyed by
    // opaque `TypeId` resolved through the symbol table — same
    // collision-safe shape as `fn_contracts: HashMap<FnId, _>`. The
    // typechecker explicitly permits two modules to expose distinct
    // types of the same bare name (`A.Shape` vs `B.Shape`; see
    // `tests/typechecker_spec::cross_module_same_named_types_do_not_
    // merge`); opaque IDs make their predicates impossible to merge
    // by construction. Producer resolves `TypeKey -> TypeId` once
    // here; consumers (`find_refined_type_scoped`) resolve through
    // the same symbol table at lookup time.
    //
    // SymbolTable is always present (`ProofLowerInputs.symbol_table`
    // is `&SymbolTable`, not `Option<&_>` — the pipeline builds it
    // unconditionally). Synthetic-ctx callers (test helpers) thread
    // their own through `from_ctx` / direct construction.
    let symbols = inputs.symbol_table;

    let entry_typedefs = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some((None::<&str>, td)),
        _ => None,
    });
    let module_typedefs = inputs.dep_modules.iter().flat_map(|m| {
        m.type_defs
            .iter()
            .map(move |td| (Some(m.prefix.as_str()), td))
    });

    for (module_prefix, td) in entry_typedefs.chain(module_typedefs) {
        let TypeDef::Product { name, fields, .. } = td else {
            continue;
        };
        if fields.len() != 1 {
            continue;
        }
        let type_key = match module_prefix {
            Some(prefix) => crate::ir::TypeKey::in_module(prefix.to_string(), name),
            None => crate::ir::TypeKey::entry(name),
        };
        let Some(canonical_key) = symbols.type_id_of(&type_key) else {
            // Type isn't in the symbol table — built-ins (Result.Ok
            // etc.) are excluded by construction; for user types
            // this is a wiring bug surfaced via the symbol-table
            // builder, so just skip.
            continue;
        };
        if ir.refined_types.contains_key(&canonical_key) {
            // Same TypeId already populated — possible if a module
            // is walked twice through dep aliasing. Skip so we don't
            // overwrite a verified-witness entry with a predicate-
            // eval fallback witness.
            continue;
        }
        // Scope the smart-constructor lookup to the same module the
        // record lives in. Refinement-via-opaque keeps the record
        // opaque (`exposes opaque [X]`); a smart constructor in any
        // other module couldn't reach the carrier field anyway.
        // Without the scope, two modules each declaring a `Natural`
        // with different predicates would both pick up whichever
        // smart constructor walked first.
        let Some(info) =
            crate::codegen::common::refinement_info_for_in_scope(name, inputs, module_prefix)
        else {
            continue;
        };
        let invariant = Predicate {
            free_vars: vec![(
                info.param_name.to_string(),
                crate::ir::proof_ir::QuantifierType::Plain(info.carrier_type.to_string()),
            )],
            expr: inputs.resolve_expr(info.predicate, module_prefix),
        };
        let witness = pick_witness(
            name,
            canonical_key,
            inputs,
            info.predicate,
            info.param_name,
            module_prefix,
        );
        // Round-4 finding 1: a `None` witness means we couldn't
        // exhibit any inhabitant satisfying the predicate. Inserting
        // the slot anyway makes Dafny silently fall back to
        // `witness 0` even when the predicate excludes 0 — producing
        // an unsound subset type. Skip the lift entirely: the
        // backend will emit a plain `datatype` instead, which is
        // honest about the missing invariant. The pure-fn / law
        // paths still typecheck against the plain record.
        let Some(witness) = witness else {
            continue;
        };
        ir.refined_types.insert(
            canonical_key,
            RefinedTypeDecl {
                name: name.clone(),
                carrier_type: info.carrier_type.to_string(),
                carrier_field: info.carrier_field.to_string(),
                predicate_param: info.param_name.to_string(),
                invariant,
                witness: Some(witness),
                // Filled in immediately below by `populate_refined_type_intervals`,
                // which runs the interval analysis once over the just-built
                // `refined_types` map. Left empty here so the two passes share
                // a single source of truth (`interval::analyze`) instead of
                // each construction site re-deriving the bound.
                interval: None,
                op_classes: Vec::new(),
            },
        );
    }

    // Back-fill each decl's derived interval + per-op classification by
    // running the existing per-module interval analysis over the map we
    // just built. Reuses `interval::analyze` verbatim (no forked logic),
    // so the persisted fact on every `RefinedTypeDecl` is byte-identical
    // to what `aver compile --explain-passes` reports for the same type.
    // This makes the bound a queryable fact on the standard refinement-
    // lower path — the home a future carrier-lowering codegen recognizer
    // reads via `ctx.proof_ir.refined_types` (TypeId-keyed) without
    // re-running the analysis behind the diagnostic flag.
    populate_refined_type_intervals(inputs, ir);
}

/// Attach the interval analysis result to each `RefinedTypeDecl` in
/// `ir.refined_types`. Called once at the tail of
/// [`populate_refined_types`]; the analysis is keyed by the same opaque
/// `TypeId` the decl map uses, so the join is a direct id lookup.
fn populate_refined_type_intervals(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let analysis = crate::ir::interval::analyze(&ir.refined_types, inputs);
    for (type_id, decl) in ir.refined_types.iter_mut() {
        let Some(per_type) = analysis.types.get(type_id) else {
            continue;
        };
        // `interval_known` distinguishes a recognized bound from the
        // conservative `Interval::unbounded()` decline; persist `None`
        // for the decline so consumers don't mistake `[-inf, +inf]` for
        // a real enclosure.
        decl.interval = per_type.interval_known.then_some(per_type.interval);
        decl.op_classes = per_type.ops.clone();
    }
}

/// ETAP-2 SLICE 0+1: derive, per refinement-via-opaque type in scope, the
/// constant interval its smart-constructor invariant proves over the
/// carrier. The table is keyed by the opaque type's *bare* Aver name (e.g.
/// `"IntRange"`). The bare-`i64` MIR pass matches a carrier
/// parameter/local/return slot against this key — it extracts the bare name
/// from the slot's `MirParam.ty` (which the lowerer fills with the Debug
/// form `Named { id: …, name: "IntRange" }`, NOT the bare name; see
/// `bare_named_type` in `bare_i64`) and seeds the slot with the proven bound.
///
/// The value is `interval_of_invariant(&predicate)` — byte-identical to the
/// bound [`populate_refined_type_intervals`] persists on each
/// `RefinedTypeDecl` (both build the same [`Predicate`] from
/// [`refinement_info_for_in_scope`] + [`ProofLowerInputs::resolve_expr`] and
/// run the same `interval` recognizer). No forked logic.
///
/// **Fail-closed.** A type whose invariant the recognizer does not
/// understand returns `interval_known == false`; that entry is OMITTED from
/// the table entirely, so the MIR pass never sees it and the carrier stays
/// boxed. A carrier whose proven bound does not `fits_i64` is also kept
/// (the table carries the raw `(Interval, bool)` so the seed site can apply
/// `fits_i64` itself — see `carrier_interval` in `bare_i64`).
pub fn carrier_interval_table(
    inputs: &ProofLowerInputs,
) -> HashMap<String, (crate::ir::interval::Interval, bool)> {
    let mut table = HashMap::new();

    let entry_typedefs = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some((None::<&str>, td)),
        _ => None,
    });
    let module_typedefs = inputs.dep_modules.iter().flat_map(|m| {
        m.type_defs
            .iter()
            .map(move |td| (Some(m.prefix.as_str()), td))
    });

    for (module_prefix, td) in entry_typedefs.chain(module_typedefs) {
        // Refinement-via-opaque is a single-carrier-field product; mirror the
        // exact eligibility `populate_refined_types` applies so the keyed
        // bound is the same fact the proof side carries.
        let TypeDef::Product { name, fields, .. } = td else {
            continue;
        };
        if fields.len() != 1 {
            continue;
        }
        let Some(info) =
            crate::codegen::common::refinement_info_for_in_scope(name, inputs, module_prefix)
        else {
            continue;
        };
        let invariant = Predicate {
            free_vars: vec![(
                info.param_name.to_string(),
                QuantifierType::Plain(info.carrier_type.to_string()),
            )],
            expr: inputs.resolve_expr(info.predicate, module_prefix),
        };
        let (interval, interval_known) = crate::ir::interval::interval_of_invariant(&invariant);
        // Fail-closed: an unrecognized invariant (`interval_known == false`)
        // is OMITTED, so the carrier stays boxed.
        if !interval_known {
            continue;
        }
        // Key by the bare type name — the `MirParam.ty` string. Two modules
        // may each declare a same-named carrier with different predicates;
        // the bare name collides. Keep the FIRST (entry walks before deps),
        // and on a same-name collision intersect to the tighter common bound
        // (fail-closed: a narrower interval is always still a valid
        // over-approximation of either inhabitant set, and a mismatch can
        // only ever shrink eligibility, never wrongly widen it).
        table
            .entry(name.clone())
            .and_modify(|(iv, known): &mut (crate::ir::interval::Interval, bool)| {
                *iv = iv.intersect(interval);
                *known = true;
            })
            .or_insert((interval, true));
    }

    table
}

/// ETAP-2 multi-field carrier-`i64`: derive, per `(record-type, Int-field)`
/// pair in scope, the constant interval a MULTI-ARG smart constructor's guard
/// proves over that field. This generalizes [`carrier_interval_table`] from
/// the single-`value`-field carrier TYPE to a multi-field record whose 2+-arg
/// smart constructor bounds each field independently.
///
/// The recognized shape is:
/// ```text
/// record Coord { x: Int, y: Int }                 // 2+ Int fields
/// fn coord(x: Int, y: Int) -> Result<Coord, String>
///     match <guard conjunction over x, y>
///         true  -> Result.Ok(Coord(x = x, y = y)) // param p_j -> field f_i
///         false -> Result.Err("...")
/// ```
/// The Ok-branch `RecordCreate` maps each constructor PARAM to a record FIELD
/// (`Coord(x = x, y = y)`). For each field, the guard is split on its
/// `Bool.and` tree and only the leaf comparisons mentioning that field's bound
/// param ALONE are kept (a cross-field condition like `x + y <= 50` mentions
/// two params and is DROPPED — conservative/sound: a narrower per-field bound
/// is always a valid over-approximation). Running [`interval_of_invariant`]
/// over those single-var leaves yields the field's interval.
///
/// **Fail-closed.** A field with no proven single-var `fits_i64` bound is
/// OMITTED (the field stays boxed `$AverInt`). A non-`Int` field, a param that
/// the Ok branch does not bind one-to-one to a field, or an unrecognized guard
/// all decline. The single-field path ([`carrier_interval_table`]) is
/// untouched — this table is ADDITIVE and keyed by `(record, field)`.
///
/// Returns a map keyed by `(bare record name, field name)`.
pub fn field_carrier_interval_table(
    inputs: &ProofLowerInputs,
) -> HashMap<(String, String), (crate::ir::interval::Interval, bool)> {
    let mut table = HashMap::new();

    let entry_typedefs = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some((None::<&str>, td)),
        _ => None,
    });
    let module_typedefs = inputs.dep_modules.iter().flat_map(|m| {
        m.type_defs
            .iter()
            .map(move |td| (Some(m.prefix.as_str()), td))
    });

    for (module_prefix, td) in entry_typedefs.chain(module_typedefs) {
        let TypeDef::Product { name, fields, .. } = td else {
            continue;
        };
        // Single-field products are the existing carrier-TYPE path; skip them
        // here so the two tables never both claim the same record.
        if fields.len() < 2 {
            continue;
        }
        // Every field must be a plain `Int` for the multi-field i64 erasure;
        // a record mixing Int and non-Int fields keeps its non-Int fields
        // boxed (only the Int fields with a proven bound become eligible).
        let Some((ctor, ctor_prefix)) =
            find_multi_field_smart_ctor(name, fields, inputs, module_prefix)
        else {
            continue;
        };
        // Map record FIELD name -> the constructor PARAM name that feeds it,
        // read from the Ok-branch `RecordCreate`.
        let field_to_param = match ctor_field_param_map(ctor, name, fields) {
            Some(m) => m,
            None => continue,
        };
        let guard = ctor_guard_predicate(ctor);
        let Some(guard) = guard else { continue };
        for (fname, ftype) in fields {
            if ftype.trim() != "Int" {
                continue;
            }
            let Some(param) = field_to_param.get(fname) else {
                continue;
            };
            // Project the guard onto single-variable leaves mentioning ONLY
            // this param. A cross-field leaf (two distinct params) is dropped.
            let resolved_guard = inputs.resolve_expr(guard, ctor_prefix);
            let leaves =
                crate::codegen::common::flatten_bool_and_conjuncts_resolved(&resolved_guard);
            let single_var: Vec<_> = leaves
                .into_iter()
                .filter(|leaf| resolved_leaf_mentions_only(leaf, param))
                .collect();
            if single_var.is_empty() {
                continue;
            }
            // Rebuild a conjunction predicate over the kept leaves and run the
            // SAME interval recognizer the single-field path uses.
            let conj = rebuild_bool_and(single_var);
            let invariant = Predicate {
                free_vars: vec![(param.clone(), QuantifierType::Plain("Int".to_string()))],
                expr: conj,
            };
            let (interval, interval_known) = crate::ir::interval::interval_of_invariant(&invariant);
            if !interval_known {
                continue;
            }
            // Same collision discipline as the type-keyed table: on a
            // same-`(record, field)` collision across modules, intersect to
            // the tighter common bound (fail-closed).
            table
                .entry((name.clone(), fname.clone()))
                .and_modify(|(iv, known): &mut (crate::ir::interval::Interval, bool)| {
                    *iv = iv.intersect(interval);
                    *known = true;
                })
                .or_insert((interval, true));
        }
    }

    table
}

/// Find the single recognized multi-arg smart constructor for `record_name`:
/// a pure fn `mk(p1, ..., pN) -> Result<Rec, String>` whose body is a single
/// two-arm `true -> Result.Ok(Rec(...)) | false -> Result.Err(_)` match. The
/// param count must be >= 2 (a one-arg ctor is the single-field carrier path).
/// Returns the `&FnDef` plus the module scope it was found in.
fn find_multi_field_smart_ctor<'a>(
    record_name: &str,
    fields: &[(String, String)],
    inputs: &ProofLowerInputs<'a>,
    record_scope: Option<&str>,
) -> Option<(&'a FnDef, Option<&'a str>)> {
    let entry_fns = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::FnDef(fd) => Some((None::<&str>, fd)),
        _ => None,
    });
    let module_fns = inputs.dep_modules.iter().flat_map(|m| {
        m.fn_defs
            .iter()
            .map(move |fd| (Some(m.prefix.as_str()), fd))
    });
    for (scope, fd) in entry_fns.chain(module_fns) {
        // The constructor lives in the same module as the record (opaque
        // refinement is single-module); skip a same-named fn in another scope.
        if scope != record_scope {
            continue;
        }
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        if !fd.return_type[7..].starts_with(record_name) {
            continue;
        }
        if fd.params.len() < 2 {
            continue;
        }
        let stmts = fd.body.stmts();
        if stmts.len() != 1 {
            continue;
        }
        let crate::ast::Stmt::Expr(body_expr) = &stmts[0] else {
            continue;
        };
        let Expr::Match { arms, .. } = &body_expr.node else {
            continue;
        };
        if !is_multi_field_ok_err_match(arms, record_name, fields) {
            continue;
        }
        return Some((fd, scope));
    }
    None
}

/// True iff `arms` is the canonical multi-field smart-ctor shape:
/// `true -> Result.Ok(Rec(f_i = p_j, ...))` covering EVERY field, and
/// `false -> Result.Err(_)`.
fn is_multi_field_ok_err_match(
    arms: &[crate::ast::MatchArm],
    record_name: &str,
    fields: &[(String, String)],
) -> bool {
    if arms.len() != 2 {
        return false;
    }
    let mut true_ok = false;
    let mut false_err = false;
    for arm in arms {
        match &arm.pattern {
            crate::ast::Pattern::Literal(Literal::Bool(true)) => {
                if multi_field_ok_constructor(&arm.body, record_name, fields).is_some() {
                    true_ok = true;
                }
            }
            crate::ast::Pattern::Literal(Literal::Bool(false)) => {
                if multi_field_is_err_constructor(&arm.body) {
                    false_err = true;
                }
            }
            _ => return false,
        }
    }
    true_ok && false_err
}

/// Local mirror of `common::is_err_constructor` (which is private): the arm
/// body is `Result.Err(_)`.
fn multi_field_is_err_constructor(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::Constructor(name, Some(_)) => name == "Result.Err",
        Expr::FnCall(callee, args) if args.len() == 1 => {
            matches!(expr_to_dotted_name(&callee.node), Some(name) if name == "Result.Err")
        }
        _ => false,
    }
}

/// Inspect a `Result.Ok(Rec(f_i = p_j, ...))` arm body and return the
/// field-name -> param-name map when every field is set to a bare identifier.
/// `None` if the body is not the expected `Result.Ok` of a `RecordCreate` of
/// `record_name` covering exactly the declared fields with identifier values.
fn multi_field_ok_constructor(
    expr: &Spanned<Expr>,
    record_name: &str,
    fields: &[(String, String)],
) -> Option<HashMap<String, String>> {
    let (ctor_name, ctor_arg_node) = match &expr.node {
        Expr::Constructor(name, Some(arg)) => (name.clone(), &arg.node),
        Expr::FnCall(callee, args) if args.len() == 1 => {
            let name = expr_to_dotted_name(&callee.node)?;
            (name, &args[0].node)
        }
        _ => return None,
    };
    if ctor_name != "Result.Ok" {
        return None;
    }
    let (t, create_fields) = match ctor_arg_node {
        Expr::RecordCreate { type_name, fields } => (type_name.as_str(), fields),
        _ => return None,
    };
    if t != record_name || create_fields.len() != fields.len() {
        return None;
    }
    let mut map = HashMap::new();
    for (fname, fvalue) in create_fields {
        let param = match &fvalue.node {
            Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
            _ => return None,
        };
        map.insert(fname.clone(), param);
    }
    // Every declared field must be assigned by the Ok branch.
    if fields.iter().any(|(fname, _)| !map.contains_key(fname)) {
        return None;
    }
    Some(map)
}

/// The field -> param map of the smart constructor's Ok branch (the
/// `true -> Result.Ok(Rec(...))` arm). Walks the body's single match.
fn ctor_field_param_map(
    fd: &FnDef,
    record_name: &str,
    fields: &[(String, String)],
) -> Option<HashMap<String, String>> {
    let stmts = fd.body.stmts();
    let crate::ast::Stmt::Expr(body_expr) = stmts.first()? else {
        return None;
    };
    let Expr::Match { arms, .. } = &body_expr.node else {
        return None;
    };
    for arm in arms {
        if matches!(
            &arm.pattern,
            crate::ast::Pattern::Literal(Literal::Bool(true))
        ) {
            return multi_field_ok_constructor(&arm.body, record_name, fields);
        }
    }
    None
}

/// The smart constructor's guard predicate (the `Match` subject the Ok/Err
/// arms branch on).
fn ctor_guard_predicate(fd: &FnDef) -> Option<&Spanned<Expr>> {
    let stmts = fd.body.stmts();
    let crate::ast::Stmt::Expr(body_expr) = stmts.first()? else {
        return None;
    };
    let Expr::Match { subject, .. } = &body_expr.node else {
        return None;
    };
    Some(subject)
}

/// True iff every identifier leaf mentioned in `leaf` is `param` (so the leaf
/// is a SINGLE-VARIABLE condition over that one param). A leaf naming two
/// distinct params (a cross-field condition like `x + y <= 50`) returns false
/// and is dropped by the caller — conservative/sound per-field projection.
fn resolved_leaf_mentions_only(leaf: &Spanned<crate::ir::hir::ResolvedExpr>, param: &str) -> bool {
    let mut only = true;
    let mut saw = false;
    collect_resolved_idents(leaf, &mut |name| {
        if name == param {
            saw = true;
        } else {
            only = false;
        }
    });
    only && saw
}

/// Walk a resolved expression, invoking `f` for every identifier leaf
/// (`Ident` / `Resolved`).
fn collect_resolved_idents(e: &Spanned<crate::ir::hir::ResolvedExpr>, f: &mut impl FnMut(&str)) {
    use crate::ir::hir::ResolvedExpr;
    match &e.node {
        ResolvedExpr::Ident(n) | ResolvedExpr::Resolved { name: n, .. } => f(n),
        ResolvedExpr::BinOp(_, l, r) => {
            collect_resolved_idents(l, f);
            collect_resolved_idents(r, f);
        }
        ResolvedExpr::Neg(i) => collect_resolved_idents(i, f),
        ResolvedExpr::Call(_, args) => {
            for a in args {
                collect_resolved_idents(a, f);
            }
        }
        ResolvedExpr::Attr(o, _) => collect_resolved_idents(o, f),
        _ => {}
    }
}

/// Rebuild a `Bool.and` conjunction over the kept single-variable leaves.
/// A single leaf returns itself; >1 fold into nested `Bool.and(...)` calls,
/// matching the shape [`interval_of_invariant`] recognizes.
fn rebuild_bool_and(
    mut leaves: Vec<Spanned<crate::ir::hir::ResolvedExpr>>,
) -> Spanned<crate::ir::hir::ResolvedExpr> {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    let mut acc = leaves.remove(0);
    for leaf in leaves {
        let line = acc.line;
        acc = Spanned::new(
            ResolvedExpr::Call(
                ResolvedCallee::Builtin("Bool.and".to_string()),
                vec![acc, leaf],
            ),
            line,
        );
    }
    acc
}

/// ETAP-2 multi-field carrier-`i64`: the per-`(record, field)` ELIGIBLE map —
/// [`field_carrier_interval_table`] tightened the same way the single-field
/// path tightens its proven-bound set. An entry survives only when its bound
/// is recognized AND `fits_i64` AND the owning record is NOT demoted by any
/// whole-program scan ([`multi_field_record_demotions`]):
///   - the record is constructed UNGATED (a bare `RecordCreate` outside its
///     own smart-ctor whose args are not all in-bounds literals) — that bypass
///     could store an out-of-`i64` value the construct bridge would TRAP on;
///   - the record (or a record reaching it) is used as a `Map` KEY — the
///     Map-key codegen was not updated for the i64-erased fields;
///   - the record (or a record reaching it) is used DIRECTLY as a `Map` VALUE
///     (or through a record field / `Option` / `Result` that keeps it an inline
///     struct ref in the values array) — that trips a separate, pre-existing
///     record-as-Map-value validation bug, so the whole record stays boxed
///     there. A carrier used as a `List` / `Vector` / `Tuple` ELEMENT now STAYS
///     native: the per-field record eq/hash helpers dispatch a raw `i64.eq` /
///     `i32.wrap_i64` for its i64-erased fields, so `List<Coord>` keeps
///     `(field i64)(field i64)` elements that compile + run native. (`Option` /
///     `Result` payloads are NOT demoted either — they hold the element as an
///     inline struct ref, so the smart-ctor boundary
///     `coord(...) -> Result<Coord, String>` keeps the native-i64 win.)
///
/// A demoted record keeps EVERY field boxed (the whole struct stays the
/// pre-slice all-`$AverInt` layout). wasm-gc only; the Rust path passes the
/// empty registry and never erases a field.
///
/// Returns a map keyed by `(bare record name, field name)`; an empty map
/// reproduces the pre-slice all-`$AverInt` multi-field record byte-for-byte.
pub fn field_carrier_eligible_intervals(
    inputs: &ProofLowerInputs,
    instantiations: &crate::ir::mir::InstantiationRegistry,
) -> HashMap<(String, String), (crate::ir::interval::Interval, bool)> {
    let table = field_carrier_interval_table(inputs);
    if table.is_empty() {
        return table;
    }
    // Proven-bound candidates: bound recognized AND `fits_i64`. The set of
    // RECORD names with at least one such field drives the demotion scans
    // (a demotion is per-record — a record constructed ungated / used as a
    // Map key keeps ALL its fields boxed).
    let record_candidates: HashSet<String> = table
        .iter()
        .filter(|(_, (iv, known))| *known && iv.fits_i64())
        .map(|((rec, _), _)| rec.clone())
        .collect();
    if std::env::var("AVER_CARRIER_I64_SKIP_DEMOTION").is_ok() {
        return table
            .into_iter()
            .filter(|((rec, _), (iv, known))| {
                *known && iv.fits_i64() && record_candidates.contains(rec)
            })
            .collect();
    }
    let demoted_records =
        multi_field_record_demotions(inputs, &record_candidates, &table, instantiations);
    table
        .into_iter()
        .filter(|((rec, _), (iv, known))| {
            *known
                && iv.fits_i64()
                && record_candidates.contains(rec)
                && !demoted_records.contains(rec)
        })
        .collect()
}

/// ETAP-2 multi-field carrier-`i64`: the RECORD-level fail-closed demotion
/// scan — the multi-field generalization of [`carrier_eligibility_demotions`].
/// A multi-field bounded record is demoted (every field kept boxed) when:
///   - **Scan 1 (ungated construction).** A `RecordCreate` / `RecordUpdate` of
///     the record in a fn OTHER than its recognized multi-arg smart constructor
///     can smuggle an out-of-`i64` value past the per-field gate (the construct
///     bridge `__aint_to_i64_checked` would TRAP). SAFE only when every field
///     argument is a constant literal provably inside that field's proven
///     interval; any non-literal / out-of-range field argument demotes.
///   - **Scan 2 (Map-key usage).** The record — directly or transitively as a
///     field of a record/Option/List/Tuple used as a Map KEY — reaches a
///     `Map<K, V>` key position; the Map-key codegen still expects the boxed
///     struct. Driven by the inference-complete resolved Map-key types, with
///     the textual annotation scan as a cheap backstop. Both reuse the SAME
///     `carriers_reachable_from` / `collect_map_key_carriers` closures the
///     single-field path uses.
///   - **Scan 3 (direct Map-VALUE usage).** The record reaches a `Map` VALUE
///     DIRECTLY (or through a record field / `Option` / `Result` that keeps it
///     an inline struct ref in the values array). That trips a separate,
///     pre-existing record-as-Map-value validation bug, so the record stays
///     boxed there. The walk STOPS at a `List` / `Vector` / `Tuple` boundary: a
///     carrier used as such a container ELEMENT now stays native i64 (the
///     per-field record eq/hash helpers dispatch the raw `i64.eq` /
///     `i32.wrap_i64` for an i64-erased field), so a carrier reachable only
///     THROUGH such a container — e.g. a `Map<K, List<Coord>>` value — is NOT
///     demoted. Driven by the inference-complete instantiation registry (every
///     `Map` the program uses), via `carriers_reachable_as_map_value`.
///
/// Fail-closed: a trip can only ever SHRINK the eligible set.
fn multi_field_record_demotions(
    inputs: &ProofLowerInputs,
    candidates: &HashSet<String>,
    field_intervals: &HashMap<(String, String), (crate::ir::interval::Interval, bool)>,
    instantiations: &crate::ir::mir::InstantiationRegistry,
) -> HashSet<String> {
    let mut demoted: HashSet<String> = HashSet::new();
    if candidates.is_empty() {
        return demoted;
    }

    // Map each candidate record to its recognized multi-arg smart-ctor fn name
    // (the only construct site that gates the fields). A record with no
    // recognizable smart-ctor never reached `field_carrier_interval_table`, so
    // every candidate has one — but resolve defensively and demote on failure.
    let mut ctor_fn_of: HashMap<String, String> = HashMap::new();
    let record_defs = collect_product_defs(inputs);
    for name in candidates {
        let Some((fields, scope)) = record_defs.get(name) else {
            demoted.insert(name.clone());
            continue;
        };
        match find_multi_field_smart_ctor(name, fields, inputs, *scope) {
            Some((ctor, _)) => {
                ctor_fn_of.insert(name.clone(), ctor.name.clone());
            }
            None => {
                demoted.insert(name.clone());
            }
        }
    }

    // ---- Scan 1: ungated construction --------------------------------------
    let all_fn_defs = inputs
        .entry_items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .chain(inputs.dep_modules.iter().flat_map(|m| m.fn_defs.iter()));
    for fd in all_fn_defs {
        for stmt in fd.body.stmts() {
            let expr = match stmt {
                crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => e,
            };
            carrier_walk_expr(expr, &mut |e| {
                let (type_name, create_fields): (&String, &[(String, Spanned<Expr>)]) = match e {
                    Expr::RecordCreate { type_name, fields } => (type_name, fields),
                    Expr::RecordUpdate {
                        type_name, updates, ..
                    } => (type_name, updates),
                    _ => return,
                };
                if !candidates.contains(type_name) {
                    return;
                }
                // The construct inside the record's own smart-ctor is gated.
                if ctor_fn_of.get(type_name) == Some(&fd.name) {
                    return;
                }
                // Outside the smart-ctor: safe iff EVERY provided field value is
                // a constant literal inside that field's proven interval (an
                // in-bounds literal cannot smuggle an out-of-bound value past
                // the gate). A `RecordUpdate` that omits a bounded field copies
                // it from the (already-gated) base, which is safe too.
                let all_safe = create_fields.iter().all(|(fname, value)| {
                    match field_intervals.get(&(type_name.clone(), fname.clone())) {
                        // A non-bounded field (no eligible interval) is stored
                        // boxed regardless, so it can't smuggle an i64-overflow.
                        None => true,
                        Some((iv, true)) => literal_in_interval(value, *iv),
                        Some((_, false)) => false,
                    }
                });
                if !all_safe {
                    demoted.insert(type_name.clone());
                }
            });
        }
    }

    // ---- Scan 2: Map-key usage (direct + transitive) -----------------------
    let record_fields = collect_record_fields(inputs);
    for (key, _value) in &instantiations.maps {
        carriers_reachable_from(
            key,
            candidates,
            &record_fields,
            &mut HashSet::new(),
            &mut demoted,
        );
    }
    for ty_str in all_type_annotations(inputs) {
        let ty = crate::types::parse_type_str(&ty_str);
        collect_map_key_carriers(&ty, candidates, &record_fields, &mut demoted);
    }

    // ---- Scan 3: Map-VALUE usage (direct + transitive, list/vec/tuple-stopped)
    // A multi-field carrier reachable DIRECTLY as a `Map` VALUE (or through a
    // record field / `Option` / `Result` that keeps it an inline struct ref in
    // the values backing array) keeps all its fields BOXED — that case trips a
    // separate, pre-existing record-as-Map-value validation bug that is out of
    // scope here, so we fail closed exactly as before.
    //
    // A carrier used as a `List` / `Vector` / `Tuple` ELEMENT now STAYS native
    // i64: the per-field record eq/hash helpers dispatch a raw `i64.eq` /
    // `i32.wrap_i64` for an i64-erased field (gated on `is_eligible_carrier_
    // field`), so `List<Coord>` keeps `(field i64)(field i64)` elements and
    // `List.contains` / `==` over them compiles and runs native. The reachability
    // walk therefore STOPS at a `List` / `Vector` / `Tuple` boundary — a carrier
    // only reachable THROUGH such a container (e.g. `Map<K, List<Coord>>`, where
    // the Map value is a list ref whose elements are native) is NOT demoted.
    //
    // `Option` / `Result` payloads are likewise inline struct refs, so a carrier
    // behind them as a Map value stays eligible (the common smart-ctor boundary
    // `coord(...) -> Result<Coord, String>` keeps the native-i64 win).
    let mut map_value_seen: HashSet<String> = HashSet::new();
    for (_key, value) in &instantiations.maps {
        carriers_reachable_as_map_value(
            value,
            candidates,
            &record_fields,
            &mut map_value_seen,
            &mut demoted,
        );
    }

    demoted
}

/// ETAP-2 multi-field carrier-`i64`: the Map-VALUE reachability walk for the
/// demotion scan — like [`carriers_reachable_from`] but it STOPS at a `List` /
/// `Vector` / `Tuple` boundary. A carrier stored as a `List` / `Vector` /
/// `Tuple` element keeps its i64 fields native (the container's per-element
/// eq/hash dispatches the raw i64 ops), so reaching one only THROUGH such a
/// container (e.g. a `Map<K, List<Coord>>` value, a list-of-tuples value) does
/// NOT demote it. A carrier reachable as a DIRECT Map value, or through a
/// record field / `Option` / `Result` (all of which hold it as an inline struct
/// ref in the values backing array), still demotes — the record-as-Map-value
/// validation bug that motivates this scan is unchanged by the container slice.
fn carriers_reachable_as_map_value(
    value: &crate::ast::Type,
    candidates: &HashSet<String>,
    record_fields: &HashMap<String, Vec<String>>,
    seen: &mut HashSet<String>,
    demoted: &mut HashSet<String>,
) {
    use crate::ast::Type;
    let Some(name) = value.named_name() else {
        match value {
            // `Option` / `Result` keep the payload an inline struct ref in the
            // Map values array, so a carrier behind them is still a direct
            // value — descend.
            Type::Option(a) => {
                carriers_reachable_as_map_value(a, candidates, record_fields, seen, demoted);
            }
            Type::Result(a, b) => {
                carriers_reachable_as_map_value(a, candidates, record_fields, seen, demoted);
                carriers_reachable_as_map_value(b, candidates, record_fields, seen, demoted);
            }
            // A nested `Map` value is itself a Map values array — descend into
            // its value (its key is a separate Scan-2 concern, handled there).
            Type::Map(_k, v) => {
                carriers_reachable_as_map_value(v, candidates, record_fields, seen, demoted);
            }
            // `List` / `Vector` / `Tuple` make the carrier a native container
            // ELEMENT (now eligible) — STOP, do not demote anything inside.
            Type::List(_) | Type::Vector(_) | Type::Tuple(_) => {}
            _ => {}
        }
        return;
    };
    if !seen.insert(name.to_string()) {
        return;
    }
    if candidates.contains(name) {
        demoted.insert(name.to_string());
    }
    if let Some(fields) = record_fields.get(name) {
        for field_ty in fields {
            let parsed = crate::types::parse_type_str(field_ty);
            carriers_reachable_as_map_value(&parsed, candidates, record_fields, seen, demoted);
        }
    }
}

/// A candidate record's declarations as the demotion scan needs them: its
/// `(field, type)` list plus the module scope it was found in (`None` = entry).
type ProductDef<'a> = (&'a [(String, String)], Option<&'a str>);

/// Bare `Product` type name → its [`ProductDef`]. Lets the demotion scan
/// re-locate a candidate record's field declarations + smart constructor.
fn collect_product_defs<'a>(inputs: &ProofLowerInputs<'a>) -> HashMap<String, ProductDef<'a>> {
    let mut out: HashMap<String, ProductDef<'a>> = HashMap::new();
    for item in inputs.entry_items {
        if let TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) = item {
            out.entry(name.clone()).or_insert((fields.as_slice(), None));
        }
    }
    for m in inputs.dep_modules {
        for td in &m.type_defs {
            if let TypeDef::Product { name, fields, .. } = td {
                out.entry(name.clone())
                    .or_insert((fields.as_slice(), Some(m.prefix.as_str())));
            }
        }
    }
    out
}

/// A `RecordCreate`/`RecordUpdate` field value is a constant integer literal
/// (`5` or `-5`) provably within the proven interval `iv`. Any other shape
/// (a param, a call, arithmetic) is not a provable constant ⇒ `false`.
fn literal_in_interval(value: &Spanned<Expr>, iv: crate::ir::interval::Interval) -> bool {
    let k: i128 = match &value.node {
        Expr::Literal(Literal::Int(n)) => *n as i128,
        Expr::Neg(inner) => match &inner.node {
            Expr::Literal(Literal::Int(n)) => -(*n as i128),
            _ => return false,
        },
        _ => return false,
    };
    iv.contains_point(k)
}

/// ETAP-2 carrier-`i64` SLICE 2b FOLLOW-UP: the fail-closed eligibility
/// tightening. The bare carrier interval ([`carrier_interval_table`]) proves
/// only that the smart-constructor's invariant `fits_i64`; it does NOT prove
/// that every value of the carrier type actually went through that gate, nor
/// that the carrier's codegen is exercised only in positions the i64 erasure
/// supports. This scan removes a carrier from the eligible set when either
/// assumption is violated, so the carrier stays boxed (`$AverInt`) — the
/// safe, pre-slice representation that the VM and the boxed wasm-gc path
/// agree on.
///
/// Two whole-program scans, both fail-closed (a trip can only ever SHRINK the
/// eligible set, never widen it):
///
/// 1. **Ungated construction (closes the bare-constructor bypass).** A bare
///    record constructor `IntRange(value = n)` callable in the defining
///    module bypasses the smart-ctor's `0 <= n <= 100` gate. With `n`
///    overflowing `i64` the VM keeps full precision but the wasm-gc construct
///    bridge `__aint_to_i64_checked` TRAPS. A carrier `RecordCreate`d outside
///    its own recognized smart-constructor function is therefore ineligible —
///    UNLESS the construct's carrier-field argument is a constant literal that
///    provably lies inside the carrier's proven interval. Such a literal
///    construct (`IntRange(value = 0)` against a `[0, 100]` bound) cannot
///    smuggle an out-of-bound / i64-overflowing value past the gate, so it is
///    SAFE and does not demote — that pattern is exactly the in-bounds Err
///    fallback the slice's own carriers use (`unwrap`'s `IntRange(value = 0)`).
///    A non-literal argument, or a literal OUTSIDE the interval, is ungated
///    and DOES demote (this is the `mk(n) = IntRange(value = n)` bypass).
///
/// 2. **Map-key usage (closes the i64-erased Map-KEY codegen gap).** A
///    carrier used as a `Map` KEY type — directly (`Map<IntRange, V>`) or
///    transitively as a field of a record/type used as a key
///    (`Map<Coord, V>` with `Coord { x: IntRange }`) — fails wasm validation,
///    because the Map-key codegen was not updated for the i64-erased carrier.
///    Such a carrier is ineligible (the boxed key path it used before still
///    compiles). Map VALUES are unaffected and stay eligible.
///
///    The COMPLETE source of truth for which carriers are Map keys is
///    `resolved_map_keys` — the resolved `Map<K, V>` key types harvested from
///    the typed MIR (`ir::mir::discover_instantiations`). Because those types
///    come from inference, a carrier used as a key reaches this scan whether
///    its `Map` type was written as a fn-signature annotation, a LOCAL-BINDING
///    annotation (`m: Map<IntRange, Int> = …`), or NO annotation at all
///    (`m = Map.set({}, c, 5)`). A textual annotation scan cannot see the
///    last two; the resolved key type can. The annotation scan is retained as
///    a cheap fail-closed backstop only.
///
/// Returns the set of carrier type names (bare names, the same keys
/// [`carrier_interval_table`] uses) to REMOVE from the eligible set. The
/// caller subtracts this from the proven-bound set.
///
/// `resolved_map_keys`: every `Map<K, _>` key type the program instantiates,
/// per the typed-MIR instantiation registry. This is the inference-complete
/// Map-key signal that drives Scan 2.
pub fn carrier_eligibility_demotions(
    inputs: &ProofLowerInputs,
    candidates: &HashSet<String>,
    intervals: &HashMap<String, (crate::ir::interval::Interval, bool)>,
    resolved_map_keys: &[crate::ast::Type],
) -> HashSet<String> {
    let mut demoted: HashSet<String> = HashSet::new();
    if candidates.is_empty() {
        return demoted;
    }

    // Map each candidate carrier to its recognized smart-constructor fn name.
    // `refinement_info_for_in_scope` is THE source of truth for "the
    // smart-ctor function" — the exact fn the interval recognizer keyed off.
    // For the wasm-gc path `dep_modules` is empty (the program is flattened
    // into `entry_items`), so the entry scope (`None`) resolves every
    // carrier; we still consult dep scopes for generality.
    let mut ctor_fn_of: HashMap<String, String> = HashMap::new();
    for name in candidates {
        if let Some(info) = crate::codegen::common::refinement_info_for_in_scope(name, inputs, None)
        {
            ctor_fn_of.insert(name.clone(), info.constructor_fn.to_string());
        } else {
            for m in inputs.dep_modules {
                if let Some(info) = crate::codegen::common::refinement_info_for_in_scope(
                    name,
                    inputs,
                    Some(m.prefix.as_str()),
                ) {
                    ctor_fn_of.insert(name.clone(), info.constructor_fn.to_string());
                    break;
                }
            }
        }
    }

    // ---- Scan 1: ungated construction --------------------------------------
    // Walk every fn body in the program. A `RecordCreate { type_name }` whose
    // `type_name` is a candidate carrier and which sits in a fn OTHER than
    // that carrier's smart-constructor is an ungated construct ⇒ demote,
    // UNLESS its carrier-field argument is a constant literal provably inside
    // the carrier's proven interval (an in-bounds literal can't smuggle an
    // out-of-bound value past the gate — fail-closed but not over-eager). If
    // we can't cleanly identify the smart-ctor (no entry in `ctor_fn_of`),
    // every non-literal-safe construct demotes — fail-closed.
    let all_fn_defs = inputs
        .entry_items
        .iter()
        .filter_map(|it| match it {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .chain(inputs.dep_modules.iter().flat_map(|m| m.fn_defs.iter()));
    for fd in all_fn_defs {
        for stmt in fd.body.stmts() {
            let expr = match stmt {
                crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => e,
            };
            carrier_walk_expr(expr, &mut |e| {
                // Both a fresh construct (`RecordCreate`) and a record-update
                // (`RecordUpdate`, which sets the carrier's only field to an
                // arbitrary value) can smuggle a value past the smart-ctor
                // gate; treat both the same.
                let (type_name, fields): (&String, &[(String, Spanned<Expr>)]) = match e {
                    Expr::RecordCreate { type_name, fields } => (type_name, fields),
                    Expr::RecordUpdate {
                        type_name, updates, ..
                    } => (type_name, updates),
                    _ => return,
                };
                if !candidates.contains(type_name) {
                    return;
                }
                // The construct inside the carrier's own smart-ctor is the
                // gated one — never demotes.
                if ctor_fn_of.get(type_name) == Some(&fd.name) {
                    return;
                }
                // Outside the smart-ctor: safe iff the (single) carrier field
                // is a constant literal inside the proven interval.
                let safe_literal = matches!(
                    intervals.get(type_name),
                    Some((iv, true)) if construct_arg_is_in_interval(fields, *iv)
                );
                if !safe_literal {
                    demoted.insert(type_name.clone());
                }
            });
        }
    }

    // ---- Scan 2: Map-key usage (direct + transitive) -----------------------
    // Build, per record type, the set of carrier names reachable through its
    // (transitive) fields, so a carrier nested inside a record used as a key
    // is demoted too.
    let record_fields = collect_record_fields(inputs);

    // PRIMARY (complete) source of truth: the RESOLVED Map-key types harvested
    // from the typed MIR (`discover_instantiations`). Every `Map<K, V>` the
    // program actually instantiates is here — including ones whose key type is
    // only known after INFERENCE (a local-binding `m: Map<…>` annotation that
    // the textual scan never walks, or a fully inferred `m = Map.set({}, c,
    // 5)` with no annotation at all). A textual annotation scan is
    // fundamentally incomplete for these; the resolved key type is not. Demote
    // every carrier reachable from each resolved key (directly, or
    // transitively through a record / Option / List / Tuple field — the same
    // `carriers_reachable_from` closure the annotation path uses).
    for key in resolved_map_keys {
        carriers_reachable_from(
            key,
            candidates,
            &record_fields,
            &mut HashSet::new(),
            &mut demoted,
        );
    }

    // BACKSTOP (cheap): the original textual scan over fn-param / fn-return /
    // record-field annotations. Redundant with the resolved-IR path above for
    // any Map the MIR sees, but kept so a Map type that appears ONLY in an
    // annotation position the MIR instantiation walk doesn't reach (e.g. a
    // signature whose body never constructs/uses the Map) still trips. It can
    // only ever ADD to `demoted` (fail-closed).
    for ty_str in all_type_annotations(inputs) {
        let ty = crate::types::parse_type_str(&ty_str);
        collect_map_key_carriers(&ty, candidates, &record_fields, &mut demoted);
    }

    demoted
}

/// A single-carrier-field `RecordCreate`'s field argument is a constant
/// integer literal provably within the proven interval `iv`. Handles the
/// bare literal `IntRange(value = 0)` and the negated literal
/// `IntRange(value = -5)` (parsed as `Expr::Neg(Literal(Int))`). Any other
/// shape (a parameter, a call, an arithmetic expression) is NOT a provable
/// constant ⇒ returns `false` ⇒ the construct is treated as ungated.
fn construct_arg_is_in_interval(
    fields: &[(String, Spanned<Expr>)],
    iv: crate::ir::interval::Interval,
) -> bool {
    // Refinement-via-opaque carriers are single-field products.
    let [(_, value)] = fields else {
        return false;
    };
    let k: i128 = match &value.node {
        Expr::Literal(Literal::Int(n)) => *n as i128,
        Expr::Neg(inner) => match &inner.node {
            Expr::Literal(Literal::Int(n)) => -(*n as i128),
            _ => return false,
        },
        _ => return false,
    };
    iv.contains_point(k)
}

/// Local AST visitor (the proof-lower module has no shared walker). Pre-order
/// visit of every sub-expression. Mirrors `call_graph::walk_expr`.
fn carrier_walk_expr(expr: &Spanned<Expr>, visit: &mut impl FnMut(&Expr)) {
    visit(&expr.node);
    match &expr.node {
        Expr::FnCall(func, args) => {
            carrier_walk_expr(func, visit);
            for arg in args {
                carrier_walk_expr(arg, visit);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                carrier_walk_expr(arg, visit);
            }
        }
        Expr::Attr(obj, _) => carrier_walk_expr(obj, visit),
        Expr::BinOp(_, l, r) => {
            carrier_walk_expr(l, visit);
            carrier_walk_expr(r, visit);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => carrier_walk_expr(inner, visit),
        Expr::Match { subject, arms, .. } => {
            carrier_walk_expr(subject, visit);
            for arm in arms {
                carrier_walk_expr(&arm.body, visit);
            }
        }
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
            for item in items {
                carrier_walk_expr(item, visit);
            }
        }
        Expr::MapLiteral(entries) => {
            for (k, v) in entries {
                carrier_walk_expr(k, visit);
                carrier_walk_expr(v, visit);
            }
        }
        Expr::Constructor(_, maybe) => {
            if let Some(inner) = maybe {
                carrier_walk_expr(inner, visit);
            }
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let crate::ast::StrPart::Parsed(e) = part {
                    carrier_walk_expr(e, visit);
                }
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                carrier_walk_expr(e, visit);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            carrier_walk_expr(base, visit);
            for (_, e) in updates {
                carrier_walk_expr(e, visit);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. } => {}
    }
}

/// Bare record-type name → its field type strings (`Product` types only).
/// Used by Scan 2 to chase a carrier nested inside a record used as a Map
/// key.
fn collect_record_fields(inputs: &ProofLowerInputs) -> HashMap<String, Vec<String>> {
    let mut out: HashMap<String, Vec<String>> = HashMap::new();
    let entry_tds = inputs.entry_items.iter().filter_map(|it| match it {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    });
    let dep_tds = inputs.dep_modules.iter().flat_map(|m| m.type_defs.iter());
    for td in entry_tds.chain(dep_tds) {
        if let TypeDef::Product { name, fields, .. } = td {
            out.entry(name.clone())
                .or_default()
                .extend(fields.iter().map(|(_, ty)| ty.clone()));
        }
    }
    out
}

/// Every type annotation string in the program: fn param types, fn return
/// types, and record field types. A `Map<…>` anywhere here is a Map-key
/// usage candidate for Scan 2.
fn all_type_annotations(inputs: &ProofLowerInputs) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    let push_fn = |fd: &FnDef, out: &mut Vec<String>| {
        for (_, ty) in &fd.params {
            out.push(ty.clone());
        }
        out.push(fd.return_type.clone());
    };
    for it in inputs.entry_items {
        match it {
            TopLevel::FnDef(fd) => push_fn(fd, &mut out),
            TopLevel::TypeDef(TypeDef::Product { fields, .. }) => {
                out.extend(fields.iter().map(|(_, ty)| ty.clone()));
            }
            _ => {}
        }
    }
    for m in inputs.dep_modules {
        for fd in &m.fn_defs {
            push_fn(fd, &mut out);
        }
        for td in &m.type_defs {
            if let TypeDef::Product { fields, .. } = td {
                out.extend(fields.iter().map(|(_, ty)| ty.clone()));
            }
        }
    }
    out
}

/// Walk a parsed `Type`. For every `Map<K, _>` encountered, add every
/// candidate carrier REACHABLE from `K` (directly, or transitively through a
/// record's fields) to `demoted`. Recurses through every type constructor so
/// a `Map` buried inside `List<Map<…>>`, a `Result`/`Option` payload, a
/// tuple, etc. is still found.
fn collect_map_key_carriers(
    ty: &crate::ast::Type,
    candidates: &HashSet<String>,
    record_fields: &HashMap<String, Vec<String>>,
    demoted: &mut HashSet<String>,
) {
    use crate::ast::Type;
    match ty {
        Type::Map(key, value) => {
            carriers_reachable_from(key, candidates, record_fields, &mut HashSet::new(), demoted);
            // The KEY is the hazard; recurse into both so a nested Map in
            // either position is still inspected.
            collect_map_key_carriers(key, candidates, record_fields, demoted);
            collect_map_key_carriers(value, candidates, record_fields, demoted);
        }
        Type::Result(a, b) => {
            collect_map_key_carriers(a, candidates, record_fields, demoted);
            collect_map_key_carriers(b, candidates, record_fields, demoted);
        }
        Type::Option(a) | Type::List(a) | Type::Vector(a) => {
            collect_map_key_carriers(a, candidates, record_fields, demoted);
        }
        Type::Tuple(items) => {
            for t in items {
                collect_map_key_carriers(t, candidates, record_fields, demoted);
            }
        }
        Type::Fn(params, ret, _) => {
            for p in params {
                collect_map_key_carriers(p, candidates, record_fields, demoted);
            }
            collect_map_key_carriers(ret, candidates, record_fields, demoted);
        }
        _ => {}
    }
}

/// Collect every candidate carrier reachable from a Map-KEY type `key`:
/// `key` itself if it names a carrier, plus (transitively) any carrier among
/// the fields of a record `key` names. `seen` guards self-referential
/// records.
fn carriers_reachable_from(
    key: &crate::ast::Type,
    candidates: &HashSet<String>,
    record_fields: &HashMap<String, Vec<String>>,
    seen: &mut HashSet<String>,
    demoted: &mut HashSet<String>,
) {
    use crate::ast::Type;
    let Some(name) = key.named_name() else {
        // A Map key that is itself a container (`Map<List<Carrier>, V>`) —
        // chase the inner element types too.
        match key {
            Type::Option(a) | Type::List(a) | Type::Vector(a) => {
                carriers_reachable_from(a, candidates, record_fields, seen, demoted);
            }
            Type::Tuple(items) => {
                for t in items {
                    carriers_reachable_from(t, candidates, record_fields, seen, demoted);
                }
            }
            Type::Map(k, v) => {
                carriers_reachable_from(k, candidates, record_fields, seen, demoted);
                carriers_reachable_from(v, candidates, record_fields, seen, demoted);
            }
            Type::Result(a, b) => {
                carriers_reachable_from(a, candidates, record_fields, seen, demoted);
                carriers_reachable_from(b, candidates, record_fields, seen, demoted);
            }
            _ => {}
        }
        return;
    };
    if !seen.insert(name.to_string()) {
        return;
    }
    if candidates.contains(name) {
        demoted.insert(name.to_string());
    }
    if let Some(fields) = record_fields.get(name) {
        for field_ty in fields {
            let parsed = crate::types::parse_type_str(field_ty);
            carriers_reachable_from(&parsed, candidates, record_fields, seen, demoted);
        }
    }
}

/// Walk `analyze_plans(inputs)` and populate `ProofIR.fn_contracts`.
///
/// Translation pass over the classifier output (`RecursionPlan`) —
/// no re-implementation. The diff test (`tests/proof_ir_diff.rs`)
/// pins what each `RecursionPlan` variant lowers to so divergence
/// between the classifier and the IR populator surfaces there.
/// Coverage today: `IntCountdownGuarded`, `LinearRecurrence2`,
/// `Sized*` (length / sizeOf / string-pos / int-ascending). Fuel-
/// only and Mutual* plans don't materialise as `FnContract` (their
/// recursion shape doesn't need IR-level pre-decisions; backends
/// emit fuel scaffolding inline).
pub fn populate_fn_contracts(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    // Round-5 finding: walk per-scope so two modules each with a
    // recursive `foo` (or entry + module both declaring `foo`)
    // don't collide on the bare-name `plans: HashMap<String, _>`.
    // Aver's module DAG invariant rules out cross-module recursion
    // SCCs, so per-scope classification is the canonical view and
    // each `Module.fn` gets its own slot in `ir.fn_contracts`.
    for scope in inputs.scopes() {
        let (plans, issues) =
            crate::codegen::recursion::analyze_plans_in_scope(inputs, scope.as_deref(), false);
        ir.unclassified_fns
            .extend(issues.into_iter().map(|issue| crate::ir::UnclassifiedFn {
                line: issue.line,
                message: issue.message,
            }));
        populate_fn_contracts_for_scope(inputs, ir, scope.as_deref(), &plans);
    }
}

fn populate_fn_contracts_for_scope(
    inputs: &ProofLowerInputs,
    ir: &mut ProofIR,
    scope: Option<&str>,
    plans: &HashMap<String, RecursionPlan>,
) {
    let scoped_fns: Vec<&FnDef> = inputs.pure_fns_in_scope(scope);
    let qualify = |bare: &str| -> crate::ir::FnKey {
        match scope {
            Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), bare),
            None => crate::ir::FnKey::entry(bare),
        }
    };
    // Contracts key by opaque `FnId`; SymbolTable is always present
    // (pipeline builds it unconditionally, `ProofLowerInputs.symbol_
    // table: &SymbolTable`).
    let symbols = inputs.symbol_table;

    for (fn_name, plan) in plans {
        let Some(fd) = scoped_fns.iter().find(|fd| fd.name == *fn_name) else {
            continue;
        };
        let fn_key = qualify(fn_name);
        let Some(canonical_key) = symbols.fn_id_of(&fn_key) else {
            continue;
        };

        // IntCountdown — fuel-encoded countdown on a single Int param.
        // Distinct from IntCountdownGuarded: external callers may pass
        // negatives (the classifier rejected closed-world status), so
        // backends emit a fuel helper with `n.natAbs + 1` initial fuel
        // rather than a native def with a precondition.
        if let RecursionPlan::IntCountdown { param_index } = plan {
            if let Some((param_name, _)) = fd.params.get(*param_index) {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::NatAbsPlusOne {
                                param: param_name.clone(),
                            },
                        }),
                    },
                );
            }
            continue;
        }

        // IntFloorDivCountdown — guard-validated literal-divisor
        // floor-division shrink. The classifier proved both
        // side-conditions (every self-call shrinks the param through
        // `Result.withDefault(Int.div(p, k), d)` with literal k >= 2,
        // and every self-call site's guard chain implies `p >= 1`),
        // so backends emit a native well-founded def on `p.toNat`.
        if let RecursionPlan::IntFloorDivCountdown {
            param_index,
            divisor,
            helper_fn,
        } = plan
        {
            if let Some((param_name, _)) = fd.params.get(*param_index) {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::WellFoundedToNat {
                            param: param_name.clone(),
                            floor_div: Some(crate::ir::FloorDivShrink {
                                divisor: *divisor,
                                helper_fn: helper_fn.clone(),
                            }),
                        }),
                    },
                );
            }
            continue;
        }

        // IntAscending — fuel formula `(bound - n).natAbs + 1`. The
        // bound stays as `Spanned<Expr>` so backends render it through
        // their own emitters (it can be a literal, a fn param, or a
        // small arith expression).
        if let RecursionPlan::IntAscending { param_index, bound } = plan {
            if let Some((param_name, _)) = fd.params.get(*param_index) {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::BoundMinusParamNatAbsPlusOne {
                                param: param_name.clone(),
                                bound: inputs.resolve_expr(bound, scope),
                            },
                        }),
                    },
                );
            }
            continue;
        }

        // ListStructural — structural recursion on a List<_> param.
        // Lean/Dafny don't actually use a fuel helper for this on
        // recent backends (structural recursion is natively
        // terminating); the metric stays as `SeqLenPlusOne` for
        // backend-symmetric framing, and the consumer ignores it
        // when emitting plain structural recursion.
        if let RecursionPlan::ListStructural { param_index } = plan {
            if let Some((param_name, _)) = fd.params.get(*param_index) {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::SeqLenPlusOne {
                                param: param_name.clone(),
                            },
                        }),
                    },
                );
            }
            continue;
        }

        // SizeOfStructural — recursion on a user ADT (e.g. an AST
        // type). Fuel metric `sizeOf(call_frame) + 1`. The classifier
        // doesn't pin a single bound param — `sizeOf` measures the
        // whole frame — so the IR variant carries no param name.
        if matches!(plan, RecursionPlan::SizeOfStructural) {
            ir.fn_contracts.insert(
                canonical_key,
                FnContract {
                    source_name: fn_name.clone(),
                    recursion: Some(RecursionContract::Fuel {
                        fuel_metric: crate::ir::FuelMetric::SizeOfPlusOne,
                    }),
                },
            );
            continue;
        }

        // StringPosAdvance — `(s, pos)`-shape recursion: `s` invariant
        // (first param, String), `pos` advances (second param, Int).
        // Fuel formula `s.length - pos`.
        if matches!(plan, RecursionPlan::StringPosAdvance) {
            if let (Some((string_param, _)), Some((pos_param, _))) =
                (fd.params.first(), fd.params.get(1))
            {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::StringLenMinusPos {
                                string_param: string_param.clone(),
                                pos_param: pos_param.clone(),
                            },
                        }),
                    },
                );
            }
            continue;
        }

        // Mutual-recursion SCCs — each member of the SCC gets its own
        // plan with the same family. All three lower to a Lex fuel
        // metric; the params vector + rank distinguish per-shape /
        // per-member roles.
        //
        // - MutualIntCountdown: every member counts down its first
        //   Int param; rank stays 0 (no inter-member ranking — every
        //   edge decreases the shared dimension).
        // - MutualStringPosAdvance { rank }: (s, pos) shape across
        //   the SCC; rank distinguishes members for same-measure
        //   inter-fn edges.
        // - MutualSizeOfRanked { rank }: sizeOf measures the whole
        //   call frame; rank distinguishes members. No bound param —
        //   the empty params vec signals "frame-level measure".
        match plan {
            RecursionPlan::MutualIntCountdown => {
                let params = fd
                    .params
                    .first()
                    .map(|(n, _)| vec![n.clone()])
                    .unwrap_or_default();
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::Lex { params, rank: 0 },
                        }),
                    },
                );
                continue;
            }
            RecursionPlan::MutualStringPosAdvance { rank } => {
                let params = fd.params.iter().take(2).map(|(n, _)| n.clone()).collect();
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::Lex {
                                params,
                                rank: *rank,
                            },
                        }),
                    },
                );
                continue;
            }
            RecursionPlan::MutualSizeOfRanked { rank } => {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::Lex {
                                params: Vec::new(),
                                rank: *rank,
                            },
                        }),
                    },
                );
                continue;
            }
            RecursionPlan::LinearRecurrence2 => {
                ir.fn_contracts.insert(
                    canonical_key,
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::LinearRecurrence2),
                    },
                );
                continue;
            }
            _ => {}
        }

        let RecursionPlan::IntCountdownGuarded {
            param_index,
            base_arm_literal,
            base_arm_body,
            wildcard_arm_body,
            precondition,
        } = plan
        else {
            continue;
        };
        let Some((countdown_param_name, _)) = fd.params.get(*param_index) else {
            continue;
        };

        let precondition_predicates: Vec<Predicate> = precondition
            .iter()
            .map(|clause| Predicate {
                free_vars: vec![(
                    countdown_param_name.clone(),
                    QuantifierType::Plain("Int".to_string()),
                )],
                expr: inputs.resolve_expr(clause, scope),
            })
            .collect();

        ir.fn_contracts.insert(
            canonical_key,
            FnContract {
                source_name: fn_name.clone(),
                recursion: Some(RecursionContract::Native {
                    precondition: precondition_predicates,
                    measure: Measure::NatAbsInt {
                        param: countdown_param_name.clone(),
                    },
                    preservation: PreservationProof::IntCountdownLiteralZero,
                    decrease: DecreaseProof::NatAbsCountdown,
                    body: NativeIntCountdownBody {
                        base_arm_literal: *base_arm_literal,
                        base_arm_body: inputs.resolve_expr(base_arm_body, scope),
                        wildcard_arm_body: inputs.resolve_expr(wildcard_arm_body, scope),
                    },
                }),
            },
        );
    }
}

/// Walk every verify block, lift `VerifyKind::Law` entries into
/// `ProofIR.law_theorems`.
///
/// Extracts the law's shape (quantifiers from `givens`, premises
/// from `when`, claim from `lhs == rhs`) and pins a `ProofStrategy`
/// via [`classify_law_strategy`]. Covered strategies: Reflexive,
/// Commutative / Associative / IdentityElement / AntiCommutative /
/// UnaryEqualsBinary (arithmetic wrappers), Induction (recursive
/// ADTs), LibraryAxiom (Map set/get), MapUpdatePostcondition,
/// MapKeyTrackedIncrement, SpecEquivalence{,SimpNormalized},
/// LinearIntSpecEquivalence, EffectfulSpecEquivalence (with Oracle
/// Lift), LinearArithmetic (catch-all over an unfold chain).
/// Unmatched shapes pin `BackendDispatch` and fall through to the
/// backend's residual chain (linear_recurrence2 emit + sampled /
/// guarded-domain fallback).
pub fn populate_law_theorems(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    use crate::ast::{TopLevel, VerifyKind};
    use crate::ir::{LawTheorem, Predicate, Quantifier, QuantifierType};

    let symbols = inputs.symbol_table;

    // Verify-law blocks to lower, each tagged with the prefix of the
    // dep module that owns it (`None` = entry). The entry's own verify
    // blocks come from `entry_items`; dependency modules' proven laws
    // come from `ModuleInfo.verify_laws` (the cross-file law pool — a
    // dep law is lowered with the SAME strategy classification an entry
    // law of that shape gets, so it auto-proves and can be cited by a
    // consumer). The DAG invariant keeps the bare fn name unambiguous
    // within each scope.
    let entry_verifies = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::Verify(vb) => Some((None, vb)),
        _ => None,
    });
    let dep_verifies = inputs.dep_modules.iter().flat_map(|m| {
        m.verify_laws
            .iter()
            .map(move |vb| (Some(m.prefix.as_str()), vb))
    });
    for (owning_prefix, vb) in entry_verifies.chain(dep_verifies) {
        let VerifyKind::Law(law) = &vb.kind else {
            continue;
        };

        let quantifiers: Vec<Quantifier> = law
            .givens
            .iter()
            .map(|g| Quantifier {
                name: g.name.clone(),
                binder_type: QuantifierType::Plain(g.type_name.clone()),
            })
            .collect();

        // The fn this law targets, keyed by its owning scope. For an
        // entry law the bare name resolves to an entry `FnId`; for a
        // dep law it resolves through `FnKey::in_module(prefix, name)`.
        // When the fn isn't in the symbol table (verify block targeting
        // a fn that doesn't exist), skip the law silently — the
        // typechecker / verify-driver surfaces the missing target
        // elsewhere.
        let target_key = match owning_prefix {
            Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &vb.fn_name),
            None => crate::ir::FnKey::entry(&vb.fn_name),
        };
        let Some(fn_id) = symbols.fn_id_of(&target_key) else {
            continue;
        };

        // Scope for resolving the law's expressions: derived from the
        // target fn's owning module, NOT hardcoded to entry.
        let law_scope: Option<String> = symbols
            .fn_entry(fn_id)
            .key
            .scope_str()
            .map(|s| s.to_string());
        let law_scope_ref = law_scope.as_deref();

        let premises: Vec<Predicate> = match &law.when {
            Some(when_expr) => vec![Predicate {
                free_vars: quantifiers
                    .iter()
                    .map(|q| (q.name.clone(), q.binder_type.clone()))
                    .collect(),
                expr: inputs.resolve_expr(when_expr, law_scope_ref),
            }],
            None => Vec::new(),
        };

        let strategy = classify_law_strategy(
            law,
            &vb.fn_name,
            inputs,
            &ir.refined_types,
            &ir.fn_contracts,
            law_scope_ref,
        );

        ir.law_theorems.push(LawTheorem {
            fn_id,
            law_name: law.name.clone(),
            quantifiers,
            premises,
            claim_lhs: inputs.resolve_expr(&law.lhs, law_scope_ref),
            claim_rhs: inputs.resolve_expr(&law.rhs, law_scope_ref),
            strategy,
        });
    }

    // Demand-driven well-founded graduation for the floor-division
    // window family: the figures' proof templates rest on the
    // power-of-two fn's defining equations and functional-induction
    // principle, which the fuel encoding destroys (the fuel arg on
    // the recursive call differs from the callee's own measure, so
    // nothing universal is provable through `__fuel`). Upgrade the
    // cited pow fn's contract from `Fuel { NatAbsPlusOne }` to the
    // native `WellFoundedToNat` form (`floor_div: None` — the guarded
    // subtractive countdown whose `n <= 0` base guard puts `n >= 1`
    // in the decreasing goal's context, so `omega` closes the
    // measure bare). Scoped on purpose: a pow-shaped fn in a file
    // with no recognized window law keeps its established fuel
    // emission, so nothing outside the family moves.
    let window_pow_fns: HashSet<String> = ir
        .law_theorems
        .iter()
        .filter_map(|t| match &t.strategy {
            crate::ir::ProofStrategy::FloorDivWindow { figure } => Some(match figure {
                crate::ir::FloorWindowFigure::PowPositive { pow_fn } => pow_fn.clone(),
                crate::ir::FloorWindowFigure::PowSumSplit { pow_fn } => pow_fn.clone(),
                crate::ir::FloorWindowFigure::SigWindow { pow_fn, .. } => pow_fn.clone(),
                crate::ir::FloorWindowFigure::ProductWindow { pow_fn, .. } => pow_fn.clone(),
            }),
            _ => None,
        })
        .collect();
    for pow_fn in window_pow_fns {
        let Some(fn_id) = symbols.fn_id_of(&crate::ir::FnKey::entry(&pow_fn)) else {
            continue;
        };
        let Some(contract) = ir.fn_contracts.get_mut(&fn_id) else {
            continue;
        };
        if let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::NatAbsPlusOne { param },
        }) = &contract.recursion
        {
            contract.recursion = Some(crate::ir::RecursionContract::WellFoundedToNat {
                param: param.clone(),
                floor_div: None,
            });
        }
    }
}

/// Pick the strategy `LawLower` should pin on a `(fn, law)` pair.
///
/// Decision order — specific algebraic properties first, then
/// generic linear-arithmetic catch-all, then `BackendDispatch`:
/// 1. `Reflexive` — `law.lhs ≡ law.rhs` syntactically.
/// 2. `Commutative { op }` — fn body is `a <op> b`, claim is
///    `f(a, b) = f(b, a)` (op restricted to commutative ones).
/// 3. `Associative { op }` — same body, 3 givens, assoc claim.
/// 4. `IdentityElement { op }` — `f(a, e) = a` (or `f(e, a) = a`),
///    where `e` is the op's identity. Covers Add/Mul both-sided
///    plus Sub right-sided.
/// 5. `AntiCommutative { op: Sub, neg_on_rhs }` — `f(a, b) =
///    -f(b, a)` form. Sub-only (Mul has no anti-commutative law).
/// 6. `UnaryEqualsBinary { inner_fn }` — outer fn is unary, claim
///    binds it to the inner binary fn at a constant.
/// 7. `LinearArithmetic { unfold_fns, ... }` — catch-all when the
///    law reduces to linear arith after unfolding the call chain.
/// 8. `EnumConstantFold { unfold_fns }` — ground law over fixed
///    enum/ADT constructor args, scalar return (#466).
/// 9. `FiniteDomainCases { givens }` — every given ranges over a
///    closed finite domain (Bool / fieldless enum, product ≤ 16);
///    closes by exhaustive `cases` enumeration.
/// 10. `RingIdentity { unfold_fns }` — unconditional ring identity
///     over Int-component records (cross-multiplication equality);
///     runs before the prelude-simp rung, which would otherwise claim
///     the shape and park it on a caught sorry.
/// 11. `IntDecimalRoundtrip { … }` — canonical decimal-Int
///     parse/serialize roundtrip over a recognized string-pos scanner;
///     runs before the prelude-simp rung, which would otherwise claim
///     the shape and park it on a caught sorry.
/// 12. `SimpOverPreludeLemmas { … }` — builtin-roundtrip shape; the
///     Lean backend renders it AFTER its legacy chain, so it fires
///     exactly where the bare-`sorry` universal used to.
/// 13. `BackendDispatch` — backend's ad-hoc chain decides.
///
/// (The induction/spec-equivalence/Map families detected between
/// these rungs are documented at their detector sites below.)
fn classify_law_strategy(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
    scope: Option<&str>,
) -> crate::ir::ProofStrategy {
    use crate::ir::ProofStrategy;

    // Result-pipeline chain equivalence (stage 8b of #232) — `?`
    // propagation `chain_qm(x)` vs nested-match `chain_manual(x)`.
    // Both sides unfold to the same nested match; the proof closes
    // by `unfold + repeat split`.
    if law.when.is_none()
        && let Some(s) = detect_result_pipeline_chain_equivalence(law, fn_name, inputs)
    {
        return s;
    }
    // Wrapper-over-recursion with monoidal accumulator (stage 8 of
    // #232) — runs before generic induction because its aux-lemma
    // template closes laws naive induction can't (e.g. `sum(xs) ==
    // sumDirect(xs)` where `sum(xs) = sumTR(xs, 0)`). Detected
    // when `fn_name` is registered as a `WrapperOverRecursion`
    // pattern in `ProgramShape` AND the law shape is
    // `wrapper(g) == other(g)` AND the inner fn body matches the
    // monoidal-accumulator template.
    if law.when.is_none()
        && let Some(s) = detect_wrapper_over_recursion(law, fn_name, inputs)
    {
        return s;
    }
    // Tail-recursive fold with a FIXED base param (TIP prop_35,
    // `exp x y = qexp x y one`). The 2-given inline-wrapper shape the
    // `WrapperOverRecursion` recognizer can't reach: a 3-arg loop whose
    // extra leading param is held fixed and whose combine multiplies the
    // accumulator by that fixed param. Emits the accumulator-generalization
    // lemma plus the main universal law.
    if law.when.is_none()
        && let Some(s) = detect_tailrec_fixed_base_fold(law, inputs)
    {
        return s;
    }
    // Structural induction runs first — when any given binds a
    // recursive ADT, induction over its variants is the canonical
    // proof. Reflexive could also fire on `f(t) = f(t)` for `t: Tree`
    // but induction subsumes (one trivial case per variant) and is
    // the legacy chain's first pick. `when` clauses block induction
    // — a non-closing `when` law would emit a 2-arm induction ladder
    // (2 sorries) instead of the bounded sampled-domain fallback,
    // regressing output cleanliness; a non-regressing when-aware
    // induction path is a follow-up.
    if law.when.is_none()
        && let Some(param) = detect_induction_target(law, inputs)
    {
        return ProofStrategy::Induction { param };
    }
    if law.lhs == law.rhs {
        return ProofStrategy::Reflexive;
    }
    // Binary-wrapper-shaped laws first. `wrapper_binop` returns
    // `None` for non-binary fns — unary wrappers are tried after
    // this block falls through.
    if let Some(op) = wrapper_binop(fn_name, inputs) {
        if detect_wrapper_commutative(law, fn_name, op) {
            return ProofStrategy::Commutative { op };
        }
        if detect_wrapper_associative(law, fn_name, op) {
            return ProofStrategy::Associative { op };
        }
        if detect_wrapper_identity(law, fn_name, op) {
            return ProofStrategy::IdentityElement { op };
        }
        // Sub right-identity collapses into IdentityElement —
        // same emit (`simp [fn]`), different lhs/rhs shape. The
        // detector validates the right-side `f(a, 0) = a` form
        // (`f(0, a) = -a` doesn't equal `a`, so Sub is one-sided).
        if matches!(op, crate::ast::BinOp::Sub) && detect_wrapper_sub_right_identity(law, fn_name) {
            return ProofStrategy::IdentityElement { op };
        }
        // Anti-commutative is Sub-specific (Add/Mul are
        // commutative, no anti-commutativity). The op tag keeps
        // it parameterised even though only Sub currently fires.
        if matches!(op, crate::ast::BinOp::Sub)
            && let Some(neg_on_rhs) = detect_wrapper_sub_anti_commutative(law, fn_name)
        {
            return ProofStrategy::AntiCommutative { op, neg_on_rhs };
        }
    }
    // Unary fn equal to binary fn at a constant — `fn_name` is the
    // unary outer; the binary fn name is captured for backends.
    if let Some(inner_fn) = detect_wrapper_unary_equivalence(law, fn_name, inputs) {
        return ProofStrategy::UnaryEqualsBinary { inner_fn };
    }
    // Library axiom instances — Map.has-after-set, Map.get-after-set.
    // Specific shape, single-line `simpa using axiom` emit on Lean.
    if let Some((axiom, args)) = detect_map_set_axiom(law) {
        let resolved_args: Vec<_> = args.iter().map(|a| inputs.resolve_expr(a, scope)).collect();
        return ProofStrategy::LibraryAxiom {
            axiom,
            args: resolved_args,
        };
    }
    // Tracked-counter increment: specialised body template + `+ 1`
    // rhs. Checked before the more general MapUpdatePostcondition so
    // the tighter strategy wins for this shape.
    if let Some(inc) = detect_map_key_tracked_increment(law, fn_name, inputs) {
        return ProofStrategy::MapKeyTrackedIncrement {
            outer_fn: inc.outer_fn,
            map_arg: inputs.resolve_expr(&inc.map_arg, scope),
            key_arg: inputs.resolve_expr(&inc.key_arg, scope),
        };
    }
    // Post-condition of an inline-defined map-update fn — case-split
    // over `Map.get m k` and apply the `Map.set` axioms.
    if let Some(post) = detect_map_update_postcondition(law, fn_name, inputs) {
        return ProofStrategy::MapUpdatePostcondition {
            outer_fn: post.outer_fn,
            kind: post.kind,
            map_arg: inputs.resolve_expr(&post.map_arg, scope),
            key_arg: inputs.resolve_expr(&post.key_arg, scope),
            extra_unfolds: post.extra_unfolds,
        };
    }
    // Functional equivalence of `vb.fn_name` and a same-named spec
    // fn whose body is syntactically identical to the impl's.
    if let Some(extra_unfolds) = detect_spec_equivalence(law, fn_name, inputs) {
        return ProofStrategy::SpecEquivalence { extra_unfolds };
    }
    // Broader spec equivalence — bodies differ syntactically but
    // normalize to same under substitution + arithmetic identity
    // folding. Runs after the strict `SpecEquivalence` so the
    // tighter detector wins when both would match.
    if let Some(extra_unfolds) = detect_simp_normalized_spec_equivalence(law, fn_name, inputs) {
        return ProofStrategy::SpecEquivalenceSimpNormalized { extra_unfolds };
    }
    // Linear-Int spec equivalence — substituted bodies are pure
    // linear arithmetic over Int givens; decided by `omega` / LIA.
    if let Some((unfolded_impl, unfolded_spec)) =
        detect_linear_int_spec_equivalence(law, fn_name, inputs)
    {
        return ProofStrategy::LinearIntSpecEquivalence {
            unfolded_impl: inputs.resolve_expr(&unfolded_impl, scope),
            unfolded_spec: inputs.resolve_expr(&unfolded_spec, scope),
        };
    }
    // Effectful counterpart — Oracle Lift normalises both sides
    // (oracle args injected into impl call) and the lowerer matches
    // the canonical `impl(args) == spec(args)` shape on the
    // rewritten form. Fires on real oracle-spec laws like
    // `pickPair() => pairSpec(BranchPath.Root, rnd)`.
    if let Some(spec_fn) = detect_effectful_spec_equivalence(law, fn_name, inputs) {
        return ProofStrategy::EffectfulSpecEquivalence {
            impl_fn: fn_name.to_string(),
            spec_fn,
        };
    }
    // Second-order linear recurrence (fib / fibSpec shape). Detector
    // validates impl as tail-rec wrapper, spec as direct second-order
    // recurrence, helper as their shared affine worker — all three
    // shapes pinned in `lean::recurrence`. Backends consume the
    // (impl_fn, spec_fn, helper_fn) names from IR; the proof template
    // differs per target (Lean Nat-helper + induction; Dafny still
    // pending — issue #116).
    if let Some((spec_fn, helper_fn)) =
        detect_linear_recurrence2_spec_equivalence(law, fn_name, inputs)
    {
        return ProofStrategy::LinearRecurrence2SpecEquivalence {
            impl_fn: fn_name.to_string(),
            spec_fn,
            helper_fn,
        };
    }
    // Nonnegativity over a NONLINEAR Int product (`E >= 0`) — the
    // inequality sibling of `RingIdentity`. Placed BEFORE the
    // `LinearArithmetic` catch-all because that rung claims any all-Int
    // law (these nonlinear `when`-guarded ones included) and, unable to
    // close `0 <= a*b` with `omega`, renders them on the bounded sampled
    // fallback. The detector requires a genuine variable×variable product,
    // so it claims ONLY goals `omega` provably cannot decide — every
    // linear nonnegativity law still flows to `LinearArithmetic`. NOT
    // `when`-gated: the Newton-Raphson factor-sign guards ride in as a
    // `when` premise threaded into the universal statement.
    if let Some(unfold_fns) = detect_nonlinear_nonneg(law, fn_name, inputs) {
        return ProofStrategy::NonlinearNonneg { unfold_fns };
    }
    // Linear arithmetic over an unfold chain — generic catch-all.
    // Named for the semantic, not the backend tactic.
    if let Some(plan) = detect_simp_omega_unfold(law, fn_name, inputs, refined_types) {
        return ProofStrategy::LinearArithmetic {
            unfold_fns: plan.unfold_fns,
            wrapper_return: plan.wrapper_return,
            smart_guard: plan.smart_guard,
            lifted: plan.lifted,
        };
    }
    // Ground constant-fold over fixed ADT/enum constructors — the
    // last typed fallback before `BackendDispatch`. Fires only for the
    // narrow shape no earlier detector accepts: a non-recursive fn with
    // ≥1 non-Int param, whose every non-Int param is pinned to a
    // constructor literal at the law's call site(s). LinearArithmetic
    // rejected it (non-Int param), Induction rejected it (no recursive
    // ADT given) — so this can't steal a law another strategy owns.
    if law.when.is_none()
        && let Some(unfold_fns) = detect_enum_constant_fold(law, fn_name, inputs)
    {
        return ProofStrategy::EnumConstantFold { unfold_fns };
    }
    // Closed finite-domain enumeration — the final typed fallback
    // before `BackendDispatch`. Fires when EVERY given ranges over a
    // closed, small domain (Bool or an all-fieldless user enum, ≤ 16
    // total combinations): exhaustive `cases` over the givens yields
    // ground goals per leaf, so deliberately NO call-shape inspection,
    // NO return-type gate and NO recursion gate — closed enumeration
    // makes those irrelevant (fuel-wrapped callees compute through
    // constant-measure constructor args). That is exactly why this is
    // a NEW detector and not a relaxation of `EnumConstantFold`, whose
    // literal-pinning / non-recursive / scalar-return gates are
    // load-bearing for its simp cascade.
    if law.when.is_none()
        && let Some(givens) = detect_finite_domain_cases(law, inputs)
    {
        return ProofStrategy::FiniteDomainCases { givens };
    }
    // Unconditional ring identity over Int-component records — runs
    // BEFORE the prelude-simp rung because that rung would otherwise
    // claim the shape (record givens, non-recursive pure cone) and
    // park it on a caught sorry: its minimal simp set has no AC-ring
    // normalization, and the permutational package this strategy
    // emits cannot be added there (it would loop or destroy the
    // normal forms other strategies rely on). Every earlier rung has
    // already declined: LinearArithmetic rejects non-Int record
    // givens, EnumConstantFold needs constructor-literal-pinned
    // params, FiniteDomainCases needs closed finite domains — so the
    // pin cannot steal a law a cheaper strategy closes today.
    if law.when.is_none()
        && let Some(unfold_fns) = detect_ring_identity(law, fn_name, inputs)
    {
        return ProofStrategy::RingIdentity { unfold_fns };
    }
    // Decimal-Int parse/serialize roundtrip — runs BEFORE the prelude-
    // simp rung because that rung would otherwise claim the shape (the
    // lhs cone is fuel-wrapped with measure-closed args) and park it on
    // a caught sorry the scanner barrier guarantees. The detector
    // validates the ENTIRE canonical parser shape (head-char dispatch
    // arms, single recognized scanner, slice + `Int.fromString` leaf),
    // so it cannot fire on the #469 prelude-simp laws (`finishInt` /
    // `finishNumber` / `afterIntChar` / `finishString` — wrong arity or
    // non-literal second arg at the law call site).
    if law.when.is_none()
        && let Some(s) = detect_int_decimal_roundtrip(law, fn_name, inputs, fn_contracts)
    {
        return s;
    }
    // Escaped-string parse/serialize roundtrip — the string-escape
    // sibling of the decimal roundtrip above, and like it placed
    // BEFORE the prelude-simp rung, which would otherwise claim the
    // shape (fuel-wrapped lhs cone) and park it on a caught sorry.
    // The detector validates the ENTIRE producer/consumer pair
    // (classifier escape table aligned arm-by-arm with the consumer's
    // escape dispatcher, control-escape prefix, threshold agreement,
    // fuel contracts), so it cannot fire on any shape whose
    // synthesized suffix-invariant proof would not close.
    if law.when.is_none()
        && let Some(s) = detect_string_escape_roundtrip(law, inputs, fn_contracts)
    {
        return s;
    }
    // Floor-division window family — laws over a power-of-two fn, a
    // guard-validated floor-halving binary-exponent fn, and the
    // window predicates built from them. The detectors are
    // deliberately narrow (exactly the hand-validated figures —
    // pow positivity, the pow sum homomorphism, the significand
    // window, the product window) and key on structure plus the
    // exponent fn's `WellFoundedToNat` contract, never on names.
    // Runs after every cheaper rung declined: LinearArithmetic
    // rejects the `Result.withDefault` cone and recursive callees,
    // Induction needs a recursive-ADT given, EnumConstantFold /
    // FiniteDomainCases need non-Int / closed domains — so the pin
    // cannot steal a law another strategy closes today.
    if let Some(figure) = detect_floor_window(law, fn_name, inputs, fn_contracts) {
        return ProofStrategy::FloorDivWindow { figure };
    }
    // Builtin-roundtrip simp over the prelude's spec-lemma registry —
    // the very last typed fallback. The Lean backend deliberately
    // renders this strategy AFTER its whole legacy ad-hoc chain (see
    // `lean::law_auto`), so pinning it here cannot steal a law any
    // legacy fallback closes today: it fires exactly where the
    // sampled-sorry path used to emit a bare-`sorry` universal.
    if law.when.is_none()
        && let Some(s) = detect_simp_over_prelude_lemmas(law, fn_name, inputs, fn_contracts)
    {
        return s;
    }
    ProofStrategy::BackendDispatch
}

mod finite_domain;
mod floor_window;
mod induction;
mod inequality;
mod int_decimal_roundtrip;
mod map_laws;
mod refinement;
mod ring;
mod simp;
mod spec_equivalence;
mod string_escape_roundtrip;
mod wrapper_laws;

pub(crate) use induction::LawProofCone;

use finite_domain::*;
use floor_window::*;
use induction::*;
use inequality::*;
use int_decimal_roundtrip::*;
use map_laws::*;
use refinement::*;
use ring::*;
use simp::*;
use spec_equivalence::*;
use string_escape_roundtrip::*;
use wrapper_laws::*;
