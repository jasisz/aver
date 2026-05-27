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
        let witness = pick_witness(name, inputs, info.predicate, info.param_name, module_prefix);
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
            },
        );
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

    let entry_verifies = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::Verify(vb) => Some(vb),
        _ => None,
    });
    // Dep modules don't expose verify blocks today (ModuleInfo carries
    // type_defs + fn_defs only), so the walk stays entry-side. When
    // ModuleInfo gains a `verify_blocks` field, extend here.
    for vb in entry_verifies {
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

        // Scope for resolving the law's expressions: derived from the
        // target fn's owning module, NOT hardcoded to entry. Today
        // laws-in-modules isn't shipped, so the lookup falls back to
        // entry for every fn; once dep modules carry their own verify
        // blocks (open follow-up), the same resolution path serves
        // both. Avoids re-introducing the "scope=None means entry"
        // assumption the rest of phase E worked to eliminate.
        let law_scope: Option<String> = symbols
            .fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name))
            .or_else(|| {
                inputs.dep_modules.iter().find_map(|m| {
                    symbols.fn_id_of(&crate::ir::FnKey::in_module(m.prefix.clone(), &vb.fn_name))
                })
            })
            .and_then(|id| symbols.fn_entry(id).key.scope_str().map(|s| s.to_string()));
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

        let strategy =
            classify_law_strategy(law, &vb.fn_name, inputs, &ir.refined_types, law_scope_ref);

        // Verify laws are entry-only per current model — see
        // `LawTheorem.fn_id` doc. The bare `vb.fn_name` resolves
        // through the symbol table to an entry-scope `FnId`; when
        // the fn isn't in the symbol table (verify block targeting
        // a fn that doesn't exist), skip the law silently — the
        // typechecker / verify-driver surfaces the missing target
        // elsewhere.
        let Some(fn_id) = symbols.fn_id_of(&crate::ir::FnKey::entry(&vb.fn_name)) else {
            continue;
        };
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
/// 8. `BackendDispatch` — backend's ad-hoc chain decides.
fn classify_law_strategy(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    scope: Option<&str>,
) -> crate::ir::ProofStrategy {
    use crate::ir::ProofStrategy;

    // Structural induction runs first — when any given binds a
    // recursive ADT, induction over its variants is the canonical
    // proof. Reflexive could also fire on `f(t) = f(t)` for `t: Tree`
    // but induction subsumes (one trivial case per variant) and is
    // the legacy chain's first pick. `when` clauses block induction
    // — the case-split would lose the premise binding.
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
    ProofStrategy::BackendDispatch
}

/// Internal scratch for the simp+omega detector. Carries the
/// same fields as the IR variant but lives outside the IR enum so
/// callers can build it incrementally before pinning.
struct SimpOmegaPlan {
    unfold_fns: Vec<String>,
    wrapper_return: bool,
    smart_guard: Option<crate::ir::SmartGuard>,
    /// `true` when at least one law given is used as a refinement
    /// carrier in the law body (e.g. `given a: Int` used as
    /// `Natural(value = a)`). Subtype/subset lift carries the
    /// invariant in the type, so wrapper case-split is unnecessary.
    lifted: bool,
}

fn detect_simp_omega_unfold(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
) -> Option<SimpOmegaPlan> {
    use std::collections::BTreeSet;

    let outer_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    // All law givens Int.
    if law.givens.is_empty() || law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    // Detect refinement lifts — when any given is used as a
    // `Refined(value = given)` carrier in the law body, the outer
    // fn may legitimately take the refined type (`fn add(a:
    // Natural, b: Natural)`) and unfold through the smart
    // constructor to Int arithmetic. Skip the outer-Int rejection
    // for lifted laws.
    let symbols = inputs.symbol_table;
    let lifted = law.givens.iter().any(|g| {
        refinement_lift_for_given_ir(
            &g.name,
            &law.lhs,
            &law.rhs,
            refined_types,
            symbols,
            inputs.dep_modules,
        )
        .is_some()
    });
    if !lifted && outer_fd.params.iter().any(|(_, t)| t != "Int") {
        return None;
    }

    // Seed the unfold set from the law's two sides + the outer fn.
    let mut fn_names: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut fn_names);
    collect_fn_calls_expr(&law.rhs, &mut fn_names);
    fn_names.insert(fn_name.to_string());

    // Transitive expansion through entry+dep fn bodies. Each round
    // can only add fns reachable from the new set; converges in
    // O(items). Without this, cross-module refinement smart
    // constructors (`Modules.Natural.Natural.fromInt`) wouldn't be
    // in the unfold list and the goal would carry opaque
    // match-on-Result branches simp/omega can't close.
    loop {
        let before = fn_names.len();
        let snapshot: Vec<String> = fn_names.iter().cloned().collect();
        for fd in iter_all_fn_defs(inputs) {
            if !snapshot.contains(&fd.name) {
                continue;
            }
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut fn_names);
                    }
                }
            }
        }
        if fn_names.len() == before {
            break;
        }
    }

    // Self-recursion rejection — `unfold fn` only does one step, so
    // a recursive body leaves a stale `fn` in the goal that simp
    // can't close. Check against the narrow self-only set; calling
    // a peer fn in the unfold list is fine.
    let mut wrapper_return = false;
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        let mut self_only: BTreeSet<String> = BTreeSet::new();
        self_only.insert(fd.name.clone());
        if body_calls_any_of_inputs(&fd.body, &self_only) {
            return None;
        }
        // Int-only check for the outer law fn — but skip when the
        // law is refinement-lifted (outer fn takes the refined
        // type, body unfolds through the smart constructor).
        if fd.name == fn_name && !lifted && fd.params.iter().any(|(_, t)| t != "Int") {
            return None;
        }
        let ret = fd.return_type.as_str();
        if ret != "Int" && ret != "Float" {
            wrapper_return = true;
        }
    }

    // Top-level law fn first in the unfold list — Lean needs to see
    // it in the goal before transitively-reached callees, otherwise
    // `unfold` fails outright at the outermost call layer.
    let mut ordered: Vec<String> = Vec::new();
    if fn_names.contains(fn_name) {
        ordered.push(fn_name.to_string());
    }
    for n in &fn_names {
        if n != fn_name {
            ordered.push(n.clone());
        }
    }

    let smart_guard = extract_smart_constructor_guard(&fn_names, inputs);

    Some(SimpOmegaPlan {
        unfold_fns: ordered,
        wrapper_return,
        smart_guard,
        lifted,
    })
}

/// Backend-neutral analogue of `codegen::common::refinement_lift_
/// for_given`. Walks `lhs` / `rhs` looking for a `RecordCreate {
/// type_name, fields: [(_, Ident(given))] }` shape where `type_
/// name` is a refined type whose carrier matches the given's
/// declared type. Returns the refined type name on first match.
///
/// The legacy version (common.rs) takes `&CodegenContext` and
/// borrows the type name from `ctx.items`. The lowerer reads
/// `refined_types` directly off the in-progress `ProofIR`
/// (populated by `populate_refined_types`, which runs before
/// `populate_law_theorems` in `lower(...)`).
fn refinement_lift_for_given_ir(
    given_name: &str,
    lhs: &Spanned<crate::ast::Expr>,
    rhs: &Spanned<crate::ast::Expr>,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    symbols: &crate::ir::SymbolTable,
    dep_modules: &[crate::codegen::ModuleInfo],
) -> Option<String> {
    let mut result: Option<String> = None;
    walk_for_refinement_carrier(
        lhs,
        given_name,
        refined_types,
        symbols,
        dep_modules,
        &mut result,
    );
    walk_for_refinement_carrier(
        rhs,
        given_name,
        refined_types,
        symbols,
        dep_modules,
        &mut result,
    );
    result
}

/// **syntax-discovery-only** (epic #170 Phase 7). Walks raw AST
/// looking for a `RecordCreate(type_name, [(field, Ident(given))])`
/// pattern that lifts a `given` through a refinement type's smart
/// constructor. The recursion descends into nested record-creates,
/// fn calls, and binops so a deeply-wrapped lift still gets found.
/// Identity is handed off to `resolve_refined_type_in_with_key`,
/// which canonicalises through `SymbolTable` before keying the
/// `refined_types` map — no bare-name keying past discovery.
fn walk_for_refinement_carrier(
    expr: &Spanned<crate::ast::Expr>,
    given_name: &str,
    refined_types: &std::collections::HashMap<crate::ir::TypeId, crate::ir::RefinedTypeDecl>,
    symbols: &crate::ir::SymbolTable,
    dep_modules: &[crate::codegen::ModuleInfo],
    result: &mut Option<String>,
) {
    use crate::ast::Expr;
    if result.is_some() {
        return;
    }
    match &expr.node {
        Expr::RecordCreate { type_name, fields } if fields.len() == 1 => {
            let (_, fvalue) = &fields[0];
            let matches_var = matches!(
                &fvalue.node,
                Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == given_name
            );
            if matches_var
                && let Some((type_id, _decl)) =
                    crate::codegen::common::resolve_refined_type_in_with_key(
                        refined_types,
                        symbols,
                        dep_modules,
                        type_name,
                    )
            {
                // Stringify the canonical name via the symbol table's
                // type entry. The only consumer today reads `.is_some()`
                // (see `detect_simp_omega_unfold`), but recovering a
                // human-readable id keeps the diagnostic path honest.
                *result = Some(symbols.type_entry(type_id).key.canonical());
                return;
            }
            // Even non-matching RecordCreate may contain nested
            // refinement carriers (e.g. `Foo(value = Bar(value = a))`).
            for (_, v) in fields {
                walk_for_refinement_carrier(
                    v,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::FnCall(callee, args) => {
            walk_for_refinement_carrier(
                callee,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
            for a in args {
                walk_for_refinement_carrier(
                    a,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_for_refinement_carrier(l, given_name, refined_types, symbols, dep_modules, result);
            walk_for_refinement_carrier(r, given_name, refined_types, symbols, dep_modules, result);
        }
        Expr::Match { subject, arms, .. } => {
            walk_for_refinement_carrier(
                subject,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
            for arm in arms {
                walk_for_refinement_carrier(
                    &arm.body,
                    given_name,
                    refined_types,
                    symbols,
                    dep_modules,
                    result,
                );
            }
        }
        Expr::Attr(obj, _) => {
            walk_for_refinement_carrier(
                obj,
                given_name,
                refined_types,
                symbols,
                dep_modules,
                result,
            );
        }
        _ => {}
    }
}

fn iter_all_fn_defs<'a>(inputs: &'a ProofLowerInputs<'a>) -> impl Iterator<Item = &'a FnDef> {
    inputs
        .entry_items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .chain(inputs.dep_modules.iter().flat_map(|m| m.fn_defs.iter()))
}

fn body_calls_any_of_inputs(
    body: &crate::ast::FnBody,
    names: &std::collections::BTreeSet<String>,
) -> bool {
    let mut called = std::collections::BTreeSet::new();
    for stmt in body.stmts() {
        match stmt {
            crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                collect_fn_calls_expr(e, &mut called);
            }
        }
    }
    called.iter().any(|c| names.contains(c))
}

fn collect_fn_calls_expr(
    expr: &Spanned<crate::ast::Expr>,
    out: &mut std::collections::BTreeSet<String>,
) {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = expr_to_dotted_name(&f.node) {
                // Skip uppercase namespace handles (`List.len`,
                // `Option.Some`) — those are built-in namespaces,
                // not user fns the auto-proof can unfold. The leaf
                // segment's case discriminates user fns from
                // namespace types (cross-module user calls survive
                // because the leaf fn name starts lower-case).
                let last = name.rsplit('.').next().unwrap_or(&name);
                if last.chars().next().is_some_and(|c| c.is_lowercase()) {
                    out.insert(name);
                }
            }
            for arg in args {
                collect_fn_calls_expr(arg, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_fn_calls_expr(l, out);
            collect_fn_calls_expr(r, out);
        }
        Expr::Attr(obj, _) => collect_fn_calls_expr(obj, out),
        Expr::Match { subject, arms, .. } => {
            collect_fn_calls_expr(subject, out);
            for arm in arms {
                collect_fn_calls_expr(&arm.body, out);
            }
        }
        Expr::TailCall(boxed) => {
            out.insert(boxed.target.clone());
            for arg in &boxed.args {
                collect_fn_calls_expr(arg, out);
            }
        }
        _ => {}
    }
}

/// Find a single-param smart constructor in the unfold set whose
/// body is the canonical `match <bool-subj> { true → Ok; false →
/// Err }` shape. Returns the param name + bool subject of the
/// first match.
fn extract_smart_constructor_guard(
    fn_names: &std::collections::BTreeSet<String>,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::SmartGuard> {
    use crate::ast::{Expr, MatchArm, Pattern, Stmt};
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        if !fd.return_type.starts_with("Result<") {
            continue;
        }
        if fd.params.len() != 1 {
            continue;
        }
        let (param_name, param_type) = &fd.params[0];
        if param_type != "Int" {
            continue;
        }
        let stmts = fd.body.stmts();
        if stmts.len() != 1 {
            continue;
        }
        let Stmt::Expr(body_expr) = &stmts[0] else {
            continue;
        };
        let Expr::Match { subject, arms } = &body_expr.node else {
            continue;
        };
        if !arms_match_bool_ok_err(arms) {
            continue;
        }
        let scope = inputs.fn_owning_scope(fd);
        return Some(crate::ir::SmartGuard {
            param: param_name.clone(),
            predicate: inputs.resolve_expr(subject, scope),
        });
        // Reference the type to satisfy the MatchArm import.
        #[allow(unreachable_code)]
        {
            let _: Option<&MatchArm> = None;
            let _: Option<&Pattern> = None;
        }
    }
    None
}

fn arms_match_bool_ok_err(arms: &[crate::ast::MatchArm]) -> bool {
    use crate::ast::{Expr, Literal, Pattern};
    if arms.len() != 2 {
        return false;
    }
    let starts_with_ctor = |expr: &Spanned<Expr>, name: &str| -> bool {
        match &expr.node {
            Expr::Constructor(n, _) => n == name,
            Expr::FnCall(callee, _) => {
                if let Expr::Attr(obj, field) = &callee.node
                    && let Expr::Ident(ns) = &obj.node
                {
                    format!("{ns}.{field}") == name
                } else {
                    false
                }
            }
            _ => false,
        }
    };
    let mut saw_true_ok = false;
    let mut saw_false_err = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                if starts_with_ctor(&arm.body, "Result.Ok") {
                    saw_true_ok = true;
                }
            }
            Pattern::Literal(Literal::Bool(false)) => {
                if starts_with_ctor(&arm.body, "Result.Err") {
                    saw_false_err = true;
                }
            }
            _ => return false,
        }
    }
    saw_true_ok && saw_false_err
}

/// Detect a Map library axiom instance:
///   `Map.has(Map.set(m, k, v), k) => true`        → `Map.has_set_self`
///   `Map.get(Map.set(m, k, v), k) => Option.Some(v)` → `Map.get_set_self`
/// Returns `(axiom_name, [m, k, v])` on match, either side
/// orientation. Both axioms use the same `[m, k, v]` arg order.
fn detect_map_set_axiom(
    law: &crate::ast::VerifyLaw,
) -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
    // `Map.has(Map.set(m, k, v), k) => true`
    let has_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
        let (m, k, v) = map_has_set_parts(side)?;
        if !is_bool_true(other) {
            return None;
        }
        Some((
            "Map.has_set_self".to_string(),
            vec![m.clone(), k.clone(), v.clone()],
        ))
    };
    if let Some(found) = has_side(&law.lhs, &law.rhs).or_else(|| has_side(&law.rhs, &law.lhs)) {
        return Some(found);
    }

    // `Map.get(Map.set(m, k, v), k) => Option.Some(v)`
    let get_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
        let (m, k, v) = map_get_set_parts(side)?;
        let some_v = option_some_arg(other)?;
        if some_v.node != v.node {
            return None;
        }
        Some((
            "Map.get_set_self".to_string(),
            vec![m.clone(), k.clone(), v.clone()],
        ))
    };
    get_side(&law.lhs, &law.rhs).or_else(|| get_side(&law.rhs, &law.lhs))
}

/// Internal scratch carrier — mirrors the IR variant but lives in
/// the lowerer so callers can build incrementally.
struct MapUpdatePostconditionPlan {
    outer_fn: String,
    kind: crate::ir::MapUpdatePostconditionKind,
    map_arg: Spanned<crate::ast::Expr>,
    key_arg: Spanned<crate::ast::Expr>,
    extra_unfolds: Vec<String>,
}

/// Detect a post-condition law on an inline map-update fn `outer(m,
/// k)`. Two shapes: `Map.has(outer(m, k), k) == true` (`HasAfter`),
/// or `Map.get(outer(m, k), k) == Option.Some(...)` (`GetAfter`).
/// Both require `outer`'s body to follow the "inspect get, set in
/// every arm" template (see `outer_fn_map_update_shape`).
fn detect_map_update_postcondition(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<MapUpdatePostconditionPlan> {
    use crate::ir::MapUpdatePostconditionKind;

    outer_fn_map_update_shape(fn_name, inputs)?;

    let has_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<MapUpdatePostconditionPlan> {
        if !is_bool_true(other) {
            return None;
        }
        let (map_arg, key_arg) = map_has_after_fn_call(side, fn_name)?;
        Some(MapUpdatePostconditionPlan {
            outer_fn: fn_name.to_string(),
            kind: MapUpdatePostconditionKind::HasAfter,
            map_arg: map_arg.clone(),
            key_arg: key_arg.clone(),
            extra_unfolds: Vec::new(),
        })
    };
    if let Some(plan) = has_side(&law.lhs, &law.rhs).or_else(|| has_side(&law.rhs, &law.lhs)) {
        return Some(plan);
    }

    let get_side = |side: &Spanned<crate::ast::Expr>,
                    other: &Spanned<crate::ast::Expr>|
     -> Option<MapUpdatePostconditionPlan> {
        option_some_arg(other)?;
        let (map_arg, key_arg) = map_get_after_fn_call(side, fn_name)?;
        let extra_unfolds = law_helper_unfolds(law, fn_name, inputs);
        Some(MapUpdatePostconditionPlan {
            outer_fn: fn_name.to_string(),
            kind: MapUpdatePostconditionKind::GetAfter,
            map_arg: map_arg.clone(),
            key_arg: key_arg.clone(),
            extra_unfolds,
        })
    };
    get_side(&law.lhs, &law.rhs).or_else(|| get_side(&law.rhs, &law.lhs))
}

/// Collect user helper-fn source names referenced from the law's
/// lhs/rhs/when, expanded transitively through pure (effect-free,
/// non-main) fn bodies. The outer fn is excluded — it's carried in
/// the IR variant separately. Filters out stdlib / namespace calls
/// (`Map.get`, `Option.withDefault`, …) by requiring each name to
/// resolve to a user fn def. Sorted for deterministic emit.
fn law_helper_unfolds(
    law: &crate::ast::VerifyLaw,
    outer_fn: &str,
    inputs: &ProofLowerInputs,
) -> Vec<String> {
    use std::collections::BTreeSet;

    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };

    // Seed from law sides, immediately filtering to user-fn names.
    let mut raw: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut raw);
    collect_fn_calls_expr(&law.rhs, &mut raw);
    if let Some(when_expr) = &law.when {
        collect_fn_calls_expr(when_expr, &mut raw);
    }
    let mut names: BTreeSet<String> = raw
        .into_iter()
        .filter_map(|n| resolve_user_fn(&n).map(|fd| fd.name.clone()))
        .collect();

    // Transitive expansion through pure user fn bodies.
    loop {
        let before = names.len();
        let snapshot: Vec<String> = names.iter().cloned().collect();
        for name in snapshot {
            let Some(fd) = resolve_user_fn(&name) else {
                continue;
            };
            let mut called: BTreeSet<String> = BTreeSet::new();
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut called);
                    }
                }
            }
            for c in called {
                if let Some(callee_fd) = resolve_user_fn(&c) {
                    names.insert(callee_fd.name.clone());
                }
            }
        }
        if names.len() == before {
            break;
        }
    }
    names.remove(outer_fn);
    names.into_iter().collect()
}

/// Detect functional equivalence of `fn_name` and a same-named spec
/// fn (`spec_fn_name = law.name`). Requires (a) law.name resolves to
/// a pure user fn `spec_fd` in `inputs`; (b) law's lhs/rhs are direct
/// calls — one to `fn_name`, one to `law.name` — with identical
/// argument lists; (c) `impl_fd` and `spec_fd` bodies are single-
/// terminal-expression bodies whose AST nodes match exactly.
///
/// On match, returns the unfold list: impl + spec + any user
/// helpers reachable from the law sides. Sorted for deterministic
/// emit.
fn detect_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use crate::ast::Expr;
    use std::collections::BTreeSet;

    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    if !spec_fd.effects.is_empty() || spec_fd.name == "main" {
        return None;
    }
    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let canonical_shape =
        |lhs: &Spanned<crate::ast::Expr>, rhs: &Spanned<crate::ast::Expr>| -> bool {
            let Some((l_name, l_args)) = direct_call(lhs) else {
                return false;
            };
            let Some((r_name, r_args)) = direct_call(rhs) else {
                return false;
            };
            l_name == fn_name && r_name == *spec_fn_name && l_args == r_args
        };
    if !canonical_shape(&law.lhs, &law.rhs) && !canonical_shape(&law.rhs, &law.lhs) {
        return None;
    }

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    if impl_body.node != spec_body.node {
        return None;
    }

    // Build the unfold set: impl + spec + transitively-reached user
    // helpers from law sides. Mirrors the legacy `law_simp_defs`
    // semantic but uses inputs (not CodegenContext).
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    let mut names: BTreeSet<String> = BTreeSet::new();
    names.insert(fn_name.to_string());
    names.insert(spec_fn_name.clone());
    let mut seed: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut seed);
    collect_fn_calls_expr(&law.rhs, &mut seed);
    if let Some(when_expr) = &law.when {
        collect_fn_calls_expr(when_expr, &mut seed);
    }
    for n in seed {
        if let Some(fd) = resolve_user_fn(&n) {
            names.insert(fd.name.clone());
        }
    }
    loop {
        let before = names.len();
        let snapshot: Vec<String> = names.iter().cloned().collect();
        for name in snapshot {
            let Some(fd) = resolve_user_fn(&name) else {
                continue;
            };
            let mut called: BTreeSet<String> = BTreeSet::new();
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut called);
                    }
                }
            }
            for c in called {
                if let Some(callee_fd) = resolve_user_fn(&c) {
                    names.insert(callee_fd.name.clone());
                }
            }
        }
        if names.len() == before {
            break;
        }
    }
    Some(names.into_iter().collect())
}

/// Detect functional equivalence in the broader "simp-normalized"
/// shape: same canonical impl/spec call structure as
/// [`detect_spec_equivalence`], but bodies are equivalent only
/// after arg substitution + arithmetic identity folding (drop
/// `+ 0`, `- 0`, `* 1`, fold `* 0 → 0`). Returns the unfold list
/// on match.
fn detect_simp_normalized_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use crate::ast::Expr;
    use std::collections::BTreeSet;

    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    if !spec_fd.effects.is_empty() || spec_fd.name == "main" {
        return None;
    }
    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let canonical_shape_args = |lhs: &Spanned<crate::ast::Expr>,
                                rhs: &Spanned<crate::ast::Expr>|
     -> Option<Vec<Spanned<crate::ast::Expr>>> {
        let (l_name, l_args) = direct_call(lhs)?;
        let (r_name, r_args) = direct_call(rhs)?;
        if l_name != fn_name || r_name != *spec_fn_name || l_args != r_args {
            return None;
        }
        if l_args.len() != impl_fd.params.len() || r_args.len() != spec_fd.params.len() {
            return None;
        }
        Some(l_args)
    };
    let call_args = canonical_shape_args(&law.lhs, &law.rhs)
        .or_else(|| canonical_shape_args(&law.rhs, &law.lhs))?;

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    // Reject the body-identical case — that's covered by the
    // strict `SpecEquivalence` detector running before this one.
    if impl_body.node == spec_body.node {
        return None;
    }
    let impl_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = impl_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let spec_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = spec_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let impl_normalised = simplify_identity_expr(&crate::ast_rewrite::rewrite_idents_scoped(
        impl_body,
        |name| impl_subst.get(name).cloned(),
    ));
    let spec_normalised = simplify_identity_expr(&crate::ast_rewrite::rewrite_idents_scoped(
        spec_body,
        |name| spec_subst.get(name).cloned(),
    ));
    if impl_normalised.node != spec_normalised.node {
        return None;
    }

    // Same unfold-set walk as `detect_spec_equivalence`.
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    let mut names: BTreeSet<String> = BTreeSet::new();
    names.insert(fn_name.to_string());
    names.insert(spec_fn_name.clone());
    let mut seed: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut seed);
    collect_fn_calls_expr(&law.rhs, &mut seed);
    if let Some(when_expr) = &law.when {
        collect_fn_calls_expr(when_expr, &mut seed);
    }
    for n in seed {
        if let Some(fd) = resolve_user_fn(&n) {
            names.insert(fd.name.clone());
        }
    }
    loop {
        let before = names.len();
        let snapshot: Vec<String> = names.iter().cloned().collect();
        for name in snapshot {
            let Some(fd) = resolve_user_fn(&name) else {
                continue;
            };
            let mut called: BTreeSet<String> = BTreeSet::new();
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_fn_calls_expr(e, &mut called);
                    }
                }
            }
            for c in called {
                if let Some(callee_fd) = resolve_user_fn(&c) {
                    names.insert(callee_fd.name.clone());
                }
            }
        }
        if names.len() == before {
            break;
        }
    }
    Some(names.into_iter().collect())
}

/// Detect "linear Int" spec equivalence: same canonical impl/spec
/// call shape as the other spec detectors, all givens are `Int`,
/// both impl and spec return `Int`, and the substituted bodies are
/// purely linear arithmetic expressions over the law-quantified
/// givens (only `Int` literals, given idents, `Add`, `Sub`). On
/// match returns the two substituted bodies — backends rewrite to
/// `change <impl> = <spec>` and close via their linear-arithmetic
/// decision procedure (`omega` on Lean, Z3 LIA on Dafny).
fn detect_linear_int_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<(Spanned<crate::ast::Expr>, Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    use std::collections::HashSet;

    if law.givens.is_empty() || !law.givens.iter().all(|g| g.type_name == "Int") {
        return None;
    }
    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    if !spec_fd.effects.is_empty() || spec_fd.name == "main" {
        return None;
    }
    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if impl_fd.return_type != "Int" || spec_fd.return_type != "Int" {
        return None;
    }

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let canonical_shape_args = |lhs: &Spanned<crate::ast::Expr>,
                                rhs: &Spanned<crate::ast::Expr>|
     -> Option<Vec<Spanned<crate::ast::Expr>>> {
        let (l_name, l_args) = direct_call(lhs)?;
        let (r_name, r_args) = direct_call(rhs)?;
        if l_name != fn_name || r_name != *spec_fn_name || l_args != r_args {
            return None;
        }
        if l_args.len() != impl_fd.params.len() || r_args.len() != spec_fd.params.len() {
            return None;
        }
        Some(l_args)
    };
    let call_args = canonical_shape_args(&law.lhs, &law.rhs)
        .or_else(|| canonical_shape_args(&law.rhs, &law.lhs))?;

    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    let impl_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = impl_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let spec_subst: std::collections::HashMap<String, Spanned<crate::ast::Expr>> = spec_fd
        .params
        .iter()
        .zip(call_args.iter())
        .map(|((n, _), arg)| (n.clone(), arg.clone()))
        .collect();
    let unfolded_impl =
        crate::ast_rewrite::rewrite_idents_scoped(impl_body, |name| impl_subst.get(name).cloned());
    let unfolded_spec =
        crate::ast_rewrite::rewrite_idents_scoped(spec_body, |name| spec_subst.get(name).cloned());

    let allowed_idents: HashSet<&str> = law.givens.iter().map(|g| g.name.as_str()).collect();
    if !is_linear_int_expr(&unfolded_impl, &allowed_idents)
        || !is_linear_int_expr(&unfolded_spec, &allowed_idents)
    {
        return None;
    }
    Some((unfolded_impl, unfolded_spec))
}

/// Check whether `expr` is purely linear arithmetic over `allowed_
/// idents`: only `Int` literals, allowed idents, and `Add`/`Sub`
/// BinOps. Mirrors legacy `spec::linear_int::is_linear_int_expr`.
fn is_linear_int_expr(
    expr: &Spanned<crate::ast::Expr>,
    allowed_idents: &std::collections::HashSet<&str>,
) -> bool {
    use crate::ast::{BinOp, Expr, Literal};
    match &expr.node {
        Expr::Literal(Literal::Int(_)) => true,
        Expr::Ident(name) | Expr::Resolved { name, .. } => allowed_idents.contains(name.as_str()),
        Expr::BinOp(BinOp::Add | BinOp::Sub, left, right) => {
            is_linear_int_expr(left, allowed_idents) && is_linear_int_expr(right, allowed_idents)
        }
        _ => false,
    }
}

/// Detect functional equivalence between an effectful impl fn and
/// a spec fn (different name). Runs Oracle Lift over both sides of
/// the law first — injecting `BranchPath.Root` and oracle givens
/// into every classified effectful call site — then matches the
/// canonical `impl(args) == spec(args)` direct-call shape with
/// identical argument lists on the rewritten form. Returns the
/// spec fn name on match.
///
/// Body-match is not required here (and would fail — impl and spec
/// bodies usually differ syntactically; Oracle Lift normalises them
/// to a common oracle call only after backend-side `simp` unfolds).
fn detect_effectful_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    use crate::ast::Expr;

    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if impl_fd.effects.is_empty() {
        return None;
    }
    if !impl_fd
        .effects
        .iter()
        .all(|e| crate::types::checker::effect_classification::is_classified(&e.node))
    {
        return None;
    }

    let find_fn = |name: &str| -> Option<&crate::ast::FnDef> {
        inputs
            .entry_items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .find(|fd| fd.name == name)
    };
    let rewritten_lhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.lhs,
        law,
        find_fn,
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );
    let rewritten_rhs = crate::codegen::common::rewrite_effectful_calls_in_law(
        &law.rhs,
        law,
        find_fn,
        crate::codegen::common::OracleInjectionMode::LemmaBindingProjected,
    );

    let direct_call =
        |expr: &Spanned<crate::ast::Expr>| -> Option<(String, Vec<Spanned<crate::ast::Expr>>)> {
            let Expr::FnCall(callee, args) = &expr.node else {
                return None;
            };
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => n.clone(),
                _ => return None,
            };
            Some((name, args.clone()))
        };
    let try_side = |impl_side: &Spanned<crate::ast::Expr>,
                    spec_side: &Spanned<crate::ast::Expr>|
     -> Option<String> {
        let (l_name, l_args) = direct_call(impl_side)?;
        let (r_name, r_args) = direct_call(spec_side)?;
        if l_args != r_args || l_name == r_name || l_name != fn_name {
            return None;
        }
        Some(r_name)
    };
    try_side(&rewritten_lhs, &rewritten_rhs).or_else(|| try_side(&rewritten_rhs, &rewritten_lhs))
}

/// Detect second-order linear recurrence spec equivalence (fib /
/// fibSpec pattern). impl_fn is a tail-rec wrapper dispatching on
/// `n < 0` and calling a 3-arg helper with seed pair; spec_fn is a
/// direct recurrence with `match n { 0 / 1 / _ }` arms. The shared
/// affine recurrence must match between the helper's worker and the
/// spec's `_` arm. Returns `(spec_fn_name, helper_fn_name)` on
/// match. Detection lives behind the `lean::recurrence::detect_*`
/// helpers because their AST patterns were specced there originally;
/// the data they extract is backend-neutral.
fn detect_linear_recurrence2_spec_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<(String, String)> {
    use crate::codegen::lean::recurrence::{
        detect_second_order_int_linear_recurrence, detect_tailrec_int_linear_pair_worker,
        detect_tailrec_int_linear_pair_wrapper,
    };

    let spec_fn_name = &law.name;
    if spec_fn_name == fn_name {
        return None;
    }
    if !law_references_fn(&law.lhs, spec_fn_name) && !law_references_fn(&law.rhs, spec_fn_name) {
        return None;
    }

    let impl_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    let spec_fd = inputs.find_fn_def_by_call_name(spec_fn_name)?;
    let impl_shape = detect_tailrec_int_linear_pair_wrapper(impl_fd)?;
    let spec_shape = detect_second_order_int_linear_recurrence(spec_fd)?;

    // AST-strict cross-check: negative branch + seed values must
    // match across impl wrapper and spec direct recurrence.
    if impl_shape.negative_branch.node != spec_shape.negative_branch.node
        || impl_shape.seed_prev.node != spec_shape.base0.node
        || impl_shape.seed_curr.node != spec_shape.base1.node
    {
        return None;
    }

    let helper_fd = inputs.find_fn_def_by_call_name(&impl_shape.helper_fn_name)?;
    let helper_shape = detect_tailrec_int_linear_pair_worker(helper_fd)?;
    if helper_shape.recurrence != spec_shape.recurrence {
        return None;
    }

    Some((spec_fn_name.clone(), impl_shape.helper_fn_name))
}

fn law_references_fn(expr: &Spanned<crate::ast::Expr>, target: &str) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(callee, args) => {
            let name = match &callee.node {
                Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
                _ => None,
            };
            if name == Some(target) {
                return true;
            }
            args.iter().any(|a| law_references_fn(a, target))
        }
        Expr::BinOp(_, l, r) => law_references_fn(l, target) || law_references_fn(r, target),
        Expr::Attr(base, _) => law_references_fn(base, target),
        Expr::Match { subject, arms } => {
            law_references_fn(subject, target)
                || arms.iter().any(|arm| law_references_fn(&arm.body, target))
        }
        _ => false,
    }
}

/// Validate the outer fn's body matches the "inspect get, set in
/// every arm" template:
///
/// ```text
/// fn outer(m: Map<K, V>, k: K) -> Map<K, V>
///   [v = Map.get(m, k);]?
///   match (v | Map.get(m, k)) {
///     _ -> Map.set(m, k, _)
///     _ -> Map.set(m, k, _)
///     ...
///   }
/// ```
///
/// Returns `Some(())` on match. The map/key params are positional —
/// position 0 is the map, position 1 is the key.
fn outer_fn_map_update_shape(fn_name: &str, inputs: &ProofLowerInputs) -> Option<()> {
    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 2 {
        return None;
    }
    let map_param = fd.params[0].0.as_str();
    let key_param = fd.params[1].0.as_str();
    map_update_body_matches(fd.body.stmts(), map_param, key_param).then_some(())
}

fn map_update_body_matches(stmts: &[crate::ast::Stmt], map_param: &str, key_param: &str) -> bool {
    use crate::ast::Stmt;
    if stmts.len() < 2 {
        // Even the no-let variant requires the match to be the last
        // stmt — but a single-stmt body is too short to be this shape.
        return matches!(stmts.first(), Some(Stmt::Expr(e)) if map_update_match_expr(e, map_param, key_param, None));
    }
    let Some(last) = stmts.last() else {
        return false;
    };
    let mut bound_name: Option<&str> = None;
    for stmt in &stmts[..stmts.len() - 1] {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                if !is_map_get_of_params(expr, map_param, key_param) {
                    return false;
                }
                bound_name = Some(name);
            }
            Stmt::Expr(_) => return false,
        }
    }
    match last {
        Stmt::Expr(expr) => map_update_match_expr(expr, map_param, key_param, bound_name),
        Stmt::Binding(_, _, _) => false,
    }
}

fn map_update_match_expr(
    expr: &Spanned<crate::ast::Expr>,
    map_param: &str,
    key_param: &str,
    bound_name: Option<&str>,
) -> bool {
    use crate::ast::Expr;
    let Expr::Match { subject, arms } = &expr.node else {
        return false;
    };
    if arms.len() < 2 {
        return false;
    }
    let subject_ok = match bound_name {
        Some(name) => matches_ident_expr(subject, name),
        None => is_map_get_of_params(subject, map_param, key_param),
    };
    if !subject_ok {
        return false;
    }
    arms.iter()
        .all(|arm| is_map_set_of_params(&arm.body, map_param, key_param))
}

fn is_map_get_of_params(
    expr: &Spanned<crate::ast::Expr>,
    map_param: &str,
    key_param: &str,
) -> bool {
    let Some(args) = call_named_args(expr, "Map.get") else {
        return false;
    };
    args.len() == 2
        && matches_ident_expr(&args[0], map_param)
        && matches_ident_expr(&args[1], key_param)
}

fn is_map_set_of_params(
    expr: &Spanned<crate::ast::Expr>,
    map_param: &str,
    key_param: &str,
) -> bool {
    let Some(args) = call_named_args(expr, "Map.set") else {
        return false;
    };
    args.len() == 3
        && matches_ident_expr(&args[0], map_param)
        && matches_ident_expr(&args[1], key_param)
}

struct MapKeyTrackedIncrementPlan {
    outer_fn: String,
    map_arg: Spanned<crate::ast::Expr>,
    key_arg: Spanned<crate::ast::Expr>,
}

/// Detect the canonical tracked-counter increment law:
/// `Option.withDefault(Map.get(outer(m, k), k), 0) ==
/// Option.withDefault(Map.get(m, k), 0) + 1`, where `outer` follows
/// the increment-by-one body template (see
/// [`outer_fn_map_increment_shape`]).
fn detect_map_key_tracked_increment(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<MapKeyTrackedIncrementPlan> {
    use crate::ast::{BinOp, Expr};

    outer_fn_map_increment_shape(fn_name, inputs)?;

    let side = |after: &Spanned<crate::ast::Expr>,
                rhs: &Spanned<crate::ast::Expr>|
     -> Option<MapKeyTrackedIncrementPlan> {
        let (map_arg, key_arg, default_arg) = defaulted_map_get_after_fn_call(after, fn_name)?;
        if !is_int_lit(default_arg, 0) {
            return None;
        }
        let Expr::BinOp(BinOp::Add, base, one) = &rhs.node else {
            return None;
        };
        if !is_int_lit(one, 1) {
            return None;
        }
        let (base_map, base_key, base_default) = defaulted_map_get(base)?;
        if map_arg.node != base_map.node
            || key_arg.node != base_key.node
            || default_arg.node != base_default.node
        {
            return None;
        }
        Some(MapKeyTrackedIncrementPlan {
            outer_fn: fn_name.to_string(),
            map_arg: map_arg.clone(),
            key_arg: key_arg.clone(),
        })
    };
    side(&law.lhs, &law.rhs).or_else(|| side(&law.rhs, &law.lhs))
}

/// Validate `outer(m, k)` follows the canonical tracked-counter
/// increment body:
///
/// ```text
/// let v = Map.get m k
/// match v {
///   Some(n) -> Map.set m k (n + 1)
///   None    -> Map.set m k 1
/// }
/// ```
fn outer_fn_map_increment_shape(fn_name: &str, inputs: &ProofLowerInputs) -> Option<()> {
    use crate::ast::{BinOp, Expr, Pattern, Stmt};

    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 2 {
        return None;
    }
    let map_param = fd.params[0].0.as_str();
    let key_param = fd.params[1].0.as_str();
    let stmts = fd.body.stmts();
    if stmts.len() != 2 {
        return None;
    }
    let Stmt::Binding(current, _, bound_expr) = &stmts[0] else {
        return None;
    };
    if !is_map_get_of_params(bound_expr, map_param, key_param) {
        return None;
    }
    let Stmt::Expr(last_expr) = &stmts[1] else {
        return None;
    };
    let Expr::Match { subject, arms, .. } = &last_expr.node else {
        return None;
    };
    if !matches_ident_expr(subject, current) || arms.len() != 2 {
        return None;
    }

    let some_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Constructor(name, vars) if name == "Option.Some" && vars.len() == 1 => {
            Some((vars[0].as_str(), arm.body.as_ref()))
        }
        _ => None,
    })?;
    let none_arm = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Constructor(name, vars) if name == "Option.None" && vars.is_empty() => {
            Some(arm.body.as_ref())
        }
        _ => None,
    })?;

    let (some_bound, some_body) = some_arm;
    let some_set = call_named_args(some_body, "Map.set")?;
    let none_set = call_named_args(none_arm, "Map.set")?;
    if some_set.len() != 3 || none_set.len() != 3 {
        return None;
    }
    if !matches_ident_expr(&some_set[0], map_param)
        || !matches_ident_expr(&some_set[1], key_param)
        || !matches_ident_expr(&none_set[0], map_param)
        || !matches_ident_expr(&none_set[1], key_param)
    {
        return None;
    }
    let Expr::BinOp(BinOp::Add, add_left, add_right) = &some_set[2].node else {
        return None;
    };
    if !matches_ident_expr(add_left, some_bound) || !is_int_lit(add_right, 1) {
        return None;
    }
    if !is_int_lit(&none_set[2], 1) {
        return None;
    }
    Some(())
}

/// `Option.withDefault(Map.get(outer(m, k), k), d)` — extract (m, k,
/// d) when the outer call matches `fn_name` and the lookup uses the
/// same key as the outer call's key arg.
fn defaulted_map_get_after_fn_call<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(
    &'a Spanned<crate::ast::Expr>,
    &'a Spanned<crate::ast::Expr>,
    &'a Spanned<crate::ast::Expr>,
)> {
    let (inner, default) = option_with_default_args(expr)?;
    let (map_arg, key_arg) = map_get_after_fn_call(inner, fn_name)?;
    Some((map_arg, key_arg, default))
}

/// `Option.withDefault(Map.get(m, k), d)` — extract (m, k, d) for the
/// bare lookup form (no surrounding outer call).
fn defaulted_map_get(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
)> {
    let (inner, default) = option_with_default_args(expr)?;
    let get_args = call_named_args(inner, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    Some((&get_args[0], &get_args[1], default))
}

fn option_with_default_args(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(&Spanned<crate::ast::Expr>, &Spanned<crate::ast::Expr>)> {
    let args = call_named_args(expr, "Option.withDefault")?;
    (args.len() == 2).then_some((&args[0], &args[1]))
}

fn is_int_lit(expr: &Spanned<crate::ast::Expr>, n: i64) -> bool {
    use crate::ast::{Expr, Literal};
    matches!(&expr.node, Expr::Literal(Literal::Int(m)) if *m == n)
}

/// `Map.has(outer(m, k), k)` — pick out the (m, k) from the inner
/// outer-fn call when the two key positions agree. Returns `None`
/// when the outer call doesn't match `fn_name` or shape is off.
fn map_has_after_fn_call<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<crate::ast::Expr>, &'a Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &has_args[0].node else {
        return None;
    };
    if fn_args.len() != 2
        || !callee_matches_name(callee, fn_name)
        || fn_args[1].node != has_args[1].node
    {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

/// `Map.get(outer(m, k), k)` — pick out the (m, k) from the inner
/// outer-fn call when the two key positions agree.
fn map_get_after_fn_call<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<crate::ast::Expr>, &'a Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let Expr::FnCall(callee, fn_args) = &get_args[0].node else {
        return None;
    };
    if fn_args.len() != 2
        || !callee_matches_name(callee, fn_name)
        || fn_args[1].node != get_args[1].node
    {
        return None;
    }
    Some((&fn_args[0], &fn_args[1]))
}

fn map_has_set_parts(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
)> {
    let has_args = call_named_args(expr, "Map.has")?;
    if has_args.len() != 2 {
        return None;
    }
    let set_args = call_named_args(&has_args[0], "Map.set")?;
    if set_args.len() != 3 {
        return None;
    }
    if set_args[1].node != has_args[1].node {
        return None;
    }
    Some((&set_args[0], &set_args[1], &set_args[2]))
}

fn map_get_set_parts(
    expr: &Spanned<crate::ast::Expr>,
) -> Option<(
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
    &Spanned<crate::ast::Expr>,
)> {
    let get_args = call_named_args(expr, "Map.get")?;
    if get_args.len() != 2 {
        return None;
    }
    let set_args = call_named_args(&get_args[0], "Map.set")?;
    if set_args.len() != 3 {
        return None;
    }
    if set_args[1].node != get_args[1].node {
        return None;
    }
    Some((&set_args[0], &set_args[1], &set_args[2]))
}

fn option_some_arg(expr: &Spanned<crate::ast::Expr>) -> Option<&Spanned<crate::ast::Expr>> {
    let args = call_named_args(expr, "Option.Some")?;
    (args.len() == 1).then_some(&args[0])
}

/// **syntax-discovery-only** (epic #170 Phase 7). Recognises a
/// fn-call expression whose callee's dotted source name matches
/// `full_name` exactly. Used by `Map.set` / `Map.get` axiom shape
/// detection where `full_name` is a builtin namespace path
/// (`"Map.get"` etc.) — global identity, no per-scope leak.
fn call_named_args<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    full_name: &str,
) -> Option<&'a [Spanned<crate::ast::Expr>]> {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let callee_name = expr_to_dotted_name(&callee.node)?;
    if callee_name == full_name {
        Some(args.as_slice())
    } else {
        None
    }
}

fn is_bool_true(expr: &Spanned<crate::ast::Expr>) -> bool {
    use crate::ast::{Expr, Literal};
    matches!(&expr.node, Expr::Literal(Literal::Bool(true)))
}

/// Detect a `given` that binds a recursive sum-typed ADT — the
/// induction target. Returns the given's source name on first
/// match, or `None` when no given fits.
///
/// "Recursive" means at least one variant references the type
/// itself in its field list (either bare `Tree` or wrapped like
/// `List<Tree>` / `Tree, Tree`). Indirect-via-other-types rec
/// shapes are rejected here — the backend's emit can't handle
/// them and would fail at lake-build time; better to fall through
/// to `BackendDispatch` than pin a bad strategy.
fn detect_induction_target(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    use crate::ast::TypeDef;
    for given in &law.givens {
        let Some(TypeDef::Sum {
            name: type_name,
            variants,
            ..
        }) = inputs.find_type_def(&given.type_name)
        else {
            continue;
        };
        // Require at least one variant to reference the type
        // itself — that's the recursion the induction case-split
        // pivots on.
        let direct_rec = variants.iter().any(|variant| {
            variant.fields.iter().any(|field| {
                let f = field.trim();
                f == type_name
                    || f.contains(&format!("<{}", type_name))
                    || f.contains(&format!("{}>", type_name))
                    || f.contains(&format!(", {}", type_name))
                    || f.contains(&format!("{},", type_name))
            })
        });
        if !direct_rec {
            continue;
        }
        // Reject indirect-recursion (e.g. via Option<Self> in a
        // way the backend can't case-split cleanly).
        if has_indirect_rec_variants(variants, type_name) {
            continue;
        }
        return Some(given.name.clone());
    }
    None
}

/// Mirror of `lean::law_auto::induction::has_indirect_variants` —
/// when a variant's field carries the type wrapped inside another
/// generic in a shape the per-variant emit can't decompose
/// (e.g. `Some(List<Self>)` past the simple direct-rec case),
/// the backend rejects. Replicated here so the lowerer's pin
/// matches what the backend would accept.
fn has_indirect_rec_variants(variants: &[crate::ast::TypeVariant], type_name: &str) -> bool {
    for variant in variants {
        for field in &variant.fields {
            let f = field.trim();
            // Direct match — that's the recursion we want, not "indirect".
            if f == type_name {
                continue;
            }
            // Bare `List<Tree>` / `Vec<Tree>` is fine (direct list
            // recursion); deeper nesting we conservatively reject.
            let opens = f.matches('<').count();
            if opens > 1 && f.contains(type_name) {
                return true;
            }
        }
    }
    false
}

/// Return `Some(op)` iff `fn_name` resolves to a 2-arg Int wrapper
/// `fn f(p1: Int, p2: Int) -> Int :- p1 <op> p2`. The op family
/// covers `Add` / `Mul` (commutative + associative lemmas) and
/// `Sub` (anti-commutative + right-identity); other ops fall back
/// to `BackendDispatch`.
fn wrapper_binop(fn_name: &str, inputs: &ProofLowerInputs) -> Option<crate::ast::BinOp> {
    use crate::ast::{BinOp, Expr};

    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 2 || fd.return_type != "Int" {
        return None;
    }
    let (p1, t1) = &fd.params[0];
    let (p2, t2) = &fd.params[1];
    if t1 != "Int" || t2 != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    if !matches_ident_expr(left, p1) || !matches_ident_expr(right, p2) {
        return None;
    }
    matches!(op, BinOp::Add | BinOp::Mul | BinOp::Sub).then_some(*op)
}

fn detect_wrapper_commutative(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    _op: crate::ast::BinOp,
) -> bool {
    if law.givens.len() != 2 || law.givens.iter().any(|g| g.type_name != "Int") {
        return false;
    }
    let a = &law.givens[0].name;
    let b = &law.givens[1].name;
    matches_binary_call(&law.lhs, fn_name, a, b) && matches_binary_call(&law.rhs, fn_name, b, a)
        || matches_binary_call(&law.lhs, fn_name, b, a)
            && matches_binary_call(&law.rhs, fn_name, a, b)
}

fn detect_wrapper_associative(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    _op: crate::ast::BinOp,
) -> bool {
    if law.givens.len() != 3 || law.givens.iter().any(|g| g.type_name != "Int") {
        return false;
    }
    let a = &law.givens[0].name;
    let b = &law.givens[1].name;
    let c = &law.givens[2].name;
    let nested = |side| matches_assoc_nested(side, fn_name, a, b, c);
    let flat = |side| matches_assoc_flat(side, fn_name, a, b, c);
    (nested(&law.lhs) && flat(&law.rhs)) || (nested(&law.rhs) && flat(&law.lhs))
}

/// Detect a unary↔binary wrapper equivalence shape:
/// outer side: `outer(g)` where `fn outer(p) -> p <op> K`
/// other side: `inner(g, K)` or `inner(K, g)` where `fn inner(a, b) -> a <op> b`
/// Both sides must agree on op + constant + var-position.
/// Returns the inner fn's source name, or `None` if no match.
fn detect_wrapper_unary_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return None;
    }
    let unary = unary_int_wrapper(fn_name, inputs)?;
    let g = &law.givens[0].name;

    let try_side = |call_side: &Spanned<crate::ast::Expr>,
                    other_side: &Spanned<crate::ast::Expr>|
     -> Option<String> {
        if !matches_unary_call(call_side, fn_name, g) {
            return None;
        }
        let (callee_name, var_first, lit) = binary_call_var_const(other_side, g)?;
        if lit != unary.constant || var_first != unary.var_first {
            return None;
        }
        let inner_op = wrapper_binop(&callee_name, inputs)?;
        if inner_op != unary.op {
            return None;
        }
        Some(callee_name)
    };
    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}

#[derive(Debug, Clone, Copy)]
struct UnaryIntWrapper {
    op: crate::ast::BinOp,
    constant: i64,
    var_first: bool,
}

/// Resolve `fn outer(p: Int) -> Int :- p <op> K` or `K <op> p`.
/// Returns the op + literal + which side carries the param.
fn unary_int_wrapper(fn_name: &str, inputs: &ProofLowerInputs) -> Option<UnaryIntWrapper> {
    use crate::ast::{Expr, Literal};

    let fd = inputs.find_fn_def_by_call_name(fn_name)?;
    if fd.params.len() != 1 || fd.return_type != "Int" {
        return None;
    }
    let (param, param_ty) = &fd.params[0];
    if param_ty != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::BinOp(op, left, right) = &expr.node else {
        return None;
    };
    let lit_of = |e: &Spanned<Expr>| -> Option<i64> {
        match &e.node {
            Expr::Literal(Literal::Int(n)) => Some(*n),
            _ => None,
        }
    };
    if matches_ident_expr(left, param) {
        let n = lit_of(right)?;
        return Some(UnaryIntWrapper {
            op: *op,
            constant: n,
            var_first: true,
        });
    }
    if matches_ident_expr(right, param) {
        let n = lit_of(left)?;
        return Some(UnaryIntWrapper {
            op: *op,
            constant: n,
            var_first: false,
        });
    }
    None
}

fn matches_unary_call(expr: &Spanned<crate::ast::Expr>, fn_name: &str, arg: &str) -> bool {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return false;
    };
    args.len() == 1 && callee_matches_name(callee, fn_name) && matches_ident_expr(&args[0], arg)
}

/// `inner(var, K)` or `inner(K, var)` shape. Returns
/// `(callee_name, var_first, K)` on match.
/// **syntax-discovery-only** (epic #170 Phase 7). Detects a
/// 2-arg fn call whose first arg is `Ident(var_name)` and second
/// is an `Int` literal (or symmetric). Returns the callee's dotted
/// name + the literal — caller dispatches on the name string. Used
/// only against builtin namespace methods (`Int.add`, etc.); the
/// returned name string is not stored or used for identity past
/// the dispatch site.
fn binary_call_var_const(
    expr: &Spanned<crate::ast::Expr>,
    var_name: &str,
) -> Option<(String, bool, i64)> {
    use crate::ast::{Expr, Literal};
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    let callee_name = expr_to_dotted_name(&callee.node)?;
    match (&args[0].node, &args[1].node) {
        (Expr::Ident(v) | Expr::Resolved { name: v, .. }, Expr::Literal(Literal::Int(n)))
            if v == var_name =>
        {
            Some((callee_name, true, *n))
        }
        (Expr::Literal(Literal::Int(n)), Expr::Ident(v) | Expr::Resolved { name: v, .. })
            if v == var_name =>
        {
            Some((callee_name, false, *n))
        }
        _ => None,
    }
}

fn detect_wrapper_sub_right_identity(law: &crate::ast::VerifyLaw, fn_name: &str) -> bool {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return false;
    }
    let g = &law.givens[0].name;
    matches_sub_right_identity_side(&law.lhs, &law.rhs, fn_name, g)
        || matches_sub_right_identity_side(&law.rhs, &law.lhs, fn_name, g)
}

/// Detect `sub(a, b) => -sub(b, a)` or the swapped arrangement.
/// Returns `Some(neg_on_rhs)` — `true` when the negation is on the
/// rhs (canonical direction); `false` when swapped (call on rhs,
/// negation on lhs). `None` when the shape doesn't fit.
fn detect_wrapper_sub_anti_commutative(law: &crate::ast::VerifyLaw, fn_name: &str) -> Option<bool> {
    if law.givens.len() != 2 || law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    let a = &law.givens[0].name;
    let b = &law.givens[1].name;
    if matches_binary_call(&law.lhs, fn_name, a, b)
        && matches_neg_binary_call(&law.rhs, fn_name, b, a)
    {
        return Some(true);
    }
    if matches_binary_call(&law.rhs, fn_name, a, b)
        && matches_neg_binary_call(&law.lhs, fn_name, b, a)
    {
        return Some(false);
    }
    None
}

fn detect_wrapper_identity(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    op: crate::ast::BinOp,
) -> bool {
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return false;
    }
    let identity = match op {
        crate::ast::BinOp::Add => 0,
        crate::ast::BinOp::Mul => 1,
        _ => return false,
    };
    let g = &law.givens[0].name;
    matches_identity_side(&law.lhs, &law.rhs, fn_name, g, identity)
        || matches_identity_side(&law.rhs, &law.lhs, fn_name, g, identity)
}

// ── AST matchers — ported from `lean::law_auto::shared` ─────────
//
// Kept private to proof_lower to preserve layering (proof_lower
// must not reach into lean codegen). The shapes are backend-neutral
// — pure AST pattern matching — so the duplication is local-cost
// only. A future cleanup could consolidate these into a shared
// `codegen::ast_match` module.

fn body_terminal_expr(body: &crate::ast::FnBody) -> Option<&Spanned<crate::ast::Expr>> {
    use crate::ast::Stmt;
    match body.stmts() {
        [Stmt::Expr(expr)] => Some(expr),
        _ => None,
    }
}

/// Fold the algebraic identities that survive a body-substitution
/// pass with a literal-int arg: `a + 0 == a`, `0 + a == a`,
/// `a - 0 == a`, `a * 1 == a`, `1 * a == a`, `a * 0 == 0`,
/// `0 * a == 0`. Recursive over BinOp / Neg / Attr / FnCall. Used
/// by `detect_spec_equivalence` so impl bodies like `n + 0` and
/// spec bodies like `n` are recognised as equivalent under arg
/// substitution. Matches the legacy `simp_normalized` shape.
fn simplify_identity_expr(expr: &Spanned<crate::ast::Expr>) -> Spanned<crate::ast::Expr> {
    use crate::ast::{BinOp, Expr, Literal};
    let line = expr.line;
    let int_lit = |e: &Expr| -> Option<i64> {
        match e {
            Expr::Literal(Literal::Int(n)) => Some(*n),
            _ => None,
        }
    };
    let new_node = match &expr.node {
        Expr::BinOp(op, left, right) => {
            let left = simplify_identity_expr(left);
            let right = simplify_identity_expr(right);
            match op {
                BinOp::Add => {
                    if int_lit(&left.node) == Some(0) {
                        return right;
                    } else if int_lit(&right.node) == Some(0) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                BinOp::Sub => {
                    if int_lit(&right.node) == Some(0) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                BinOp::Mul => {
                    if int_lit(&left.node) == Some(0) || int_lit(&right.node) == Some(0) {
                        Expr::Literal(Literal::Int(0))
                    } else if int_lit(&left.node) == Some(1) {
                        return right;
                    } else if int_lit(&right.node) == Some(1) {
                        return left;
                    } else {
                        Expr::BinOp(*op, Box::new(left), Box::new(right))
                    }
                }
                _ => Expr::BinOp(*op, Box::new(left), Box::new(right)),
            }
        }
        Expr::Neg(inner) => Expr::Neg(Box::new(simplify_identity_expr(inner))),
        Expr::Attr(base, field) => {
            Expr::Attr(Box::new(simplify_identity_expr(base)), field.clone())
        }
        Expr::FnCall(callee, args) => Expr::FnCall(
            Box::new(simplify_identity_expr(callee)),
            args.iter().map(simplify_identity_expr).collect(),
        ),
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(simplify_identity_expr(subject)),
            arms: arms
                .iter()
                .map(|arm| crate::ast::MatchArm {
                    pattern: arm.pattern.clone(),
                    body: Box::new(simplify_identity_expr(&arm.body)),
                    binding_slots: arm.binding_slots.clone(),
                })
                .collect(),
        },
        other => other.clone(),
    };
    Spanned::new(new_node, line)
}

/// **syntax-discovery-only** (epic #170 Phase 7). Returns true iff
/// `expr` is a bare `Ident(name)` / `Resolved { name }` matching
/// the given name. Used by shape detectors to recognise "the
/// callsite mentions this binder" — no identity lookup, just
/// source-name shape.
fn matches_ident_expr(expr: &Spanned<crate::ast::Expr>, name: &str) -> bool {
    use crate::ast::Expr;
    matches!(&expr.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == name)
}

/// **syntax-discovery-only** (epic #170 Phase 7). Compares the
/// callee's dotted source name against a target string for shape
/// recognition. EXACT MATCH ONLY — the previous suffix-match clause
/// (`name.rsplit('.').next() == Some(target)`) was an identity leak:
/// it accepted `Foo.helper` as a match for target `"helper"`. Bounded
/// today by the parser invariant (`vb.fn_name` is bare entry-only),
/// but the suffix clause stayed live as dead code that any future
/// module-scoped verify would silently exercise.
fn callee_matches_name(expr: &Spanned<crate::ast::Expr>, target: &str) -> bool {
    let Some(name) = expr_to_dotted_name(&expr.node) else {
        return false;
    };
    name == target
}

fn call2_args<'a>(
    expr: &'a Spanned<crate::ast::Expr>,
    fn_name: &str,
) -> Option<(&'a Spanned<crate::ast::Expr>, &'a Spanned<crate::ast::Expr>)> {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    if args.len() != 2 || !callee_matches_name(callee, fn_name) {
        return None;
    }
    Some((&args[0], &args[1]))
}

fn matches_binary_call(expr: &Spanned<crate::ast::Expr>, fn_name: &str, a: &str, b: &str) -> bool {
    let Some((x, y)) = call2_args(expr, fn_name) else {
        return false;
    };
    matches_ident_expr(x, a) && matches_ident_expr(y, b)
}

fn matches_assoc_nested(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
    c: &str,
) -> bool {
    let Some((ab, z)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((x, y)) = call2_args(ab, fn_name) else {
        return false;
    };
    matches_ident_expr(x, a) && matches_ident_expr(y, b) && matches_ident_expr(z, c)
}

fn matches_assoc_flat(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
    c: &str,
) -> bool {
    let Some((x, bc)) = call2_args(expr, fn_name) else {
        return false;
    };
    let Some((y, z)) = call2_args(bc, fn_name) else {
        return false;
    };
    matches_ident_expr(x, a) && matches_ident_expr(y, b) && matches_ident_expr(z, c)
}

fn matches_sub_right_identity_side(
    call_side: &Spanned<crate::ast::Expr>,
    ident_side: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    given_name: &str,
) -> bool {
    use crate::ast::{Expr, Literal};
    if !matches_ident_expr(ident_side, given_name) {
        return false;
    }
    let Some((x, y)) = call2_args(call_side, fn_name) else {
        return false;
    };
    matches_ident_expr(x, given_name)
        && matches!(&y.node, Expr::Literal(Literal::Int(n)) if *n == 0)
}

fn matches_neg_binary_call(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    a: &str,
    b: &str,
) -> bool {
    use crate::ast::Expr;
    match &expr.node {
        Expr::Neg(inner) => matches_binary_call(inner, fn_name, a, b),
        _ => false,
    }
}

fn matches_identity_side(
    call_side: &Spanned<crate::ast::Expr>,
    ident_side: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    given_name: &str,
    identity: i64,
) -> bool {
    use crate::ast::{Expr, Literal};
    if !matches_ident_expr(ident_side, given_name) {
        return false;
    }
    let Some((x, y)) = call2_args(call_side, fn_name) else {
        return false;
    };
    let is_int_lit = |e: &Spanned<Expr>, n: i64| -> bool {
        matches!(&e.node, Expr::Literal(Literal::Int(m)) if *m == n)
    };
    (matches_ident_expr(x, given_name) && is_int_lit(y, identity))
        || (is_int_lit(x, identity) && matches_ident_expr(y, given_name))
}

/// Pick an inhabitation witness: a literal value of the carrier type
/// that satisfies the refinement predicate. Backend-neutral output —
/// Dafny consumes it as `witness <W>`, Lean may later use it for a
/// `sample_X` helper. First tries the smart constructor's verify-
/// block samples (entry-module only — `ModuleInfo` doesn't surface
/// verify blocks); falls back to evaluating the predicate against
/// `[0, 1, -1]` and returning the first satisfier.
fn pick_witness(
    type_name: &str,
    inputs: &ProofLowerInputs,
    predicate: &Spanned<Expr>,
    param_name: &str,
    scope: Option<&str>,
) -> Option<String> {
    // Smart-constructor + verify-block walk, scoped to the same
    // module the type lives in. Refinement-via-opaque keeps record
    // and constructor in the same module (the carrier field is
    // opaque from outside), so this mirrors that constraint. Without
    // the scope a module-B `Natural` with predicate `n >= 10` would
    // pick up entry's `fromInt(0)` verify case, silently breaking
    // the witness invariant.
    let smart_ctor_name: Option<String> = match scope {
        None => inputs.entry_items.iter().find_map(|item| match item {
            TopLevel::FnDef(fd) if smart_ctor_matches(fd, type_name) => Some(fd.name.clone()),
            _ => None,
        }),
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .and_then(|m| {
                m.fn_defs
                    .iter()
                    .find(|fd| smart_ctor_matches(fd, type_name))
                    .map(|fd| fd.name.clone())
            }),
    };
    if let Some(smart_ctor_name) = smart_ctor_name {
        // Verify blocks live on `inputs.entry_items` only — `ModuleInfo`
        // doesn't surface verify cases. Scoped verify-block walk would
        // need a separate plumb that's not in `ProofLowerInputs` today.
        // Restrict the verify-sample fast path to entry scope; module
        // scopes fall through to the predicate-evaluation fallback,
        // which now sweeps a wider range so non-trivial predicates
        // (`n >= 10`) still get a real witness.
        if scope.is_none() {
            for item in inputs.entry_items {
                let TopLevel::Verify(vb) = item else {
                    continue;
                };
                if vb.fn_name != smart_ctor_name {
                    continue;
                }
                for (lhs, rhs) in &vb.cases {
                    if !is_result_ok(&rhs.node) {
                        continue;
                    }
                    let Expr::FnCall(_, args) = &lhs.node else {
                        continue;
                    };
                    if args.len() != 1 {
                        continue;
                    }
                    if let Some(lit) = literal_int_value(&args[0]) {
                        return Some(lit);
                    }
                }
            }
        }
    }
    // Predicate-evaluation. Two-stage: first sweep candidates
    // *extracted from the predicate AST itself* (any `Literal::Int`
    // reachable through `BinOp` / `FnCall` chains, plus the
    // neighbours `K`, `K-1`, `K+1` so `n == 5` / `n > 5` / `n < 5`
    // all land their witness). Then fall back to a fixed magnitude
    // sweep for shapes that don't mention concrete numbers
    // (`n != 0`, `Bool.and(n >= 0, n <= max())`, …). Returning
    // `None` here is intentional and `populate_refined_types`
    // skips the slot — the alternative was Dafny silently emitting
    // `witness 0` against a predicate the witness didn't satisfy.
    let mut tried = std::collections::HashSet::<i64>::new();
    let mut candidates: Vec<i64> = Vec::new();
    let mut from_ast: Vec<i64> = Vec::new();
    collect_int_literals(predicate, &mut from_ast);
    for k in from_ast {
        for delta in &[0_i64, 1, -1] {
            if let Some(c) = k.checked_add(*delta) {
                candidates.push(c);
            }
        }
    }
    candidates.extend_from_slice(&[
        0, 1, -1, 2, -2, 10, -10, 100, 1_000, 10_000, 100_000, 1_000_000,
    ]);
    for candidate in candidates {
        if !tried.insert(candidate) {
            continue;
        }
        if eval_int_bool_predicate(predicate, param_name, candidate) == Some(true) {
            return Some(candidate.to_string());
        }
    }
    None
}

fn collect_int_literals(expr: &Spanned<Expr>, out: &mut Vec<i64>) {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => out.push(*n),
        Expr::Neg(inner) => {
            if let Expr::Literal(Literal::Int(n)) = &inner.node {
                out.push(-n);
            } else {
                collect_int_literals(inner, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_int_literals(l, out);
            collect_int_literals(r, out);
        }
        Expr::FnCall(callee, args) => {
            collect_int_literals(callee, out);
            for a in args {
                collect_int_literals(a, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_int_literals(subject, out);
            for arm in arms {
                collect_int_literals(&arm.body, out);
            }
        }
        Expr::Attr(o, _) | Expr::ErrorProp(o) => collect_int_literals(o, out),
        _ => {}
    }
}

fn smart_ctor_matches(fd: &FnDef, type_name: &str) -> bool {
    // Parse the return type instead of `starts_with`-matching its
    // textual form — `Result<Nat, E>` would otherwise also match a
    // type_name of `Natural` or any other `Nat*` prefix because the
    // raw text starts with the same characters.
    if fd.params.len() != 1 {
        return false;
    }
    let parsed = crate::types::parse_type_str(&fd.return_type);
    matches!(
        parsed,
        crate::types::Type::Result(ok, _) if matches!(&*ok, crate::types::Type::Named { name: n, .. } if n == type_name)
    )
}

fn is_result_ok(expr: &Expr) -> bool {
    match expr {
        Expr::Constructor(name, _) => name == "Result.Ok",
        Expr::FnCall(callee, _) => matches!(
            &callee.node,
            Expr::Attr(obj, field)
                if field == "Ok" && matches!(&obj.node, Expr::Ident(n) if n == "Result")
        ),
        _ => false,
    }
}

fn literal_int_value(expr: &Spanned<Expr>) -> Option<String> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(n.to_string()),
        Expr::Neg(inner) => {
            let inner_str = literal_int_value(inner)?;
            Some(format!("-{inner_str}"))
        }
        _ => None,
    }
}

fn eval_int_bool_predicate(expr: &Spanned<Expr>, param_name: &str, value: i64) -> Option<bool> {
    match &expr.node {
        Expr::Literal(Literal::Bool(b)) => Some(*b),
        Expr::BinOp(op, l, r) => {
            use crate::ast::BinOp::*;
            let li = eval_int_arith(l, param_name, value)?;
            let ri = eval_int_arith(r, param_name, value)?;
            Some(match op {
                Lt => li < ri,
                Gt => li > ri,
                Lte => li <= ri,
                Gte => li >= ri,
                Eq => li == ri,
                Neq => li != ri,
                _ => return None,
            })
        }
        Expr::FnCall(callee, args) if args.len() == 2 => {
            let name = expr_to_dotted_name(&callee.node)?;
            match name.as_str() {
                "Bool.and" => Some(
                    eval_int_bool_predicate(&args[0], param_name, value)?
                        && eval_int_bool_predicate(&args[1], param_name, value)?,
                ),
                "Bool.or" => Some(
                    eval_int_bool_predicate(&args[0], param_name, value)?
                        || eval_int_bool_predicate(&args[1], param_name, value)?,
                ),
                _ => None,
            }
        }
        _ => None,
    }
}

fn eval_int_arith(expr: &Spanned<Expr>, param_name: &str, value: i64) -> Option<i64> {
    match &expr.node {
        Expr::Literal(Literal::Int(n)) => Some(*n),
        Expr::Ident(name) | Expr::Resolved { name, .. } if name == param_name => Some(value),
        Expr::BinOp(op, l, r) => {
            use crate::ast::BinOp::*;
            let li = eval_int_arith(l, param_name, value)?;
            let ri = eval_int_arith(r, param_name, value)?;
            match op {
                Add => Some(li.checked_add(ri)?),
                Sub => Some(li.checked_sub(ri)?),
                Mul => Some(li.checked_mul(ri)?),
                Div => Some(li.checked_div(ri)?),
                _ => None,
            }
        }
        Expr::Neg(inner) => Some(-eval_int_arith(inner, param_name, value)?),
        _ => None,
    }
}
