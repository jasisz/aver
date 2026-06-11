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

        let strategy = classify_law_strategy(
            law,
            &vb.fn_name,
            inputs,
            &ir.refined_types,
            &ir.fn_contracts,
            law_scope_ref,
        );

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
/// 8. `EnumConstantFold { unfold_fns }` — ground law over fixed
///    enum/ADT constructor args, scalar return (#466).
/// 9. `FiniteDomainCases { givens }` — every given ranges over a
///    closed finite domain (Bool / fieldless enum, product ≤ 16);
///    closes by exhaustive `cases` enumeration.
/// 10. `IntDecimalRoundtrip { … }` — canonical decimal-Int
///     parse/serialize roundtrip over a recognized string-pos scanner;
///     runs before the prelude-simp rung, which would otherwise claim
///     the shape and park it on a caught sorry.
/// 11. `SimpOverPreludeLemmas { … }` — builtin-roundtrip shape; the
///     Lean backend renders it AFTER its legacy chain, so it fires
///     exactly where the bare-`sorry` universal used to.
/// 12. `BackendDispatch` — backend's ad-hoc chain decides.
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

    // Match-dispatcher fold equivalence (stage 8c of #232) — two
    // self-recursive `MatchDispatcherFold` fns over the same list
    // param. Closes by structural induction on `xs` + `omega` on
    // each arm.
    if law.when.is_none()
        && let Some(s) = detect_match_dispatcher_fold_equivalence(law, fn_name, inputs)
    {
        return s;
    }
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

/// Detector for [`crate::ir::ProofStrategy::IntDecimalRoundtrip`] — the
/// canonical decimal-Int parse/serialize roundtrip
/// (`examples/data/json.av` `parseNumber.fromIntRoundtrip`).
///
/// Law shape: no `when`, exactly one `given n: Int`, lhs
/// `parse(ser(C(n)), 0)`, rhs `Ok(C(n), String.len(ser(C(n))))`. Fn
/// shape, validated transitively and EXACTLY (every gate is load-
/// bearing for the Lean emission's fixed proof skeleton — see the
/// variant doc):
///
/// ```text
/// parse(s, start)   = match charAt(s, start) { None -> _,
///                       Some(c) -> match c { "-" -> neg(s, start+1, start),
///                                            "0" -> scan(s, start+1, start, true),
///                                            _   -> pos(s, start, c) } }
/// pos(s, start, c)  = match P(c) { true -> scan(s, start+1, start, false), false -> _ }
/// neg(s, p, start)  = match charAt(s, p) { None -> _,
///                       Some(c) -> match c { "0" -> scan(s, p+1, start, true),
///                                            _   -> sign(s, p, start, c) } }
/// sign(s, p, start, c) = match P(c) { true -> scan(s, p+1, start, false), false -> _ }
/// scan              = recognized string-pos scanner (predicate P,
///                     pins [carried, false], string-pos fuel contract,
///                     exit finish(s, start, p, false))
/// finish(s, a, e, asFloat) = num = String.slice(s, a, e);
///                     match asFloat { true -> _, false -> fint(num, e) }
/// fint(num, p)      = match Int.fromString(num) { Result.Ok(v) -> Ok(C(v), p), _ -> _ }
/// ser               = match … { C(x) -> String.fromInt(x), … }
/// ```
///
/// A shape that deviates anywhere pins nothing (falls through to the
/// prelude-simp rung / `BackendDispatch`) — the conservative gate is
/// what lets the emission cite the scanner's synthesized
/// `<fn>__fuel_scan` lemma knowing it exists.
fn detect_int_decimal_roundtrip(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<crate::ir::ProofStrategy> {
    use crate::ast::{Expr, Literal, Pattern, Stmt};

    if law.when.is_some() || law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return None;
    }
    let given = law.givens[0].name.as_str();
    // The Lean emission's fixed skeleton binds these names; a colliding
    // given would be shadowed mid-proof.
    const SKELETON_RESERVED: &[&str] = &[
        "m", "d", "ds", "x", "hx", "hm", "hnd", "hsl", "hch", "hch0", "hch1", "hlen", "hmk",
        "hds10", "hdigits", "hfuel", "harm", "heq", "hdisp1", "hts", "hfin", "h0", "h1", "h2",
        "hlen0", "hslice",
    ];
    if SKELETON_RESERVED.contains(&given) {
        return None;
    }

    fn ident_of(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
            _ => None,
        }
    }
    fn call_of(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
        match &e.node {
            Expr::FnCall(callee, args) => {
                Some((expr_to_dotted_name(&callee.node)?, args.as_slice()))
            }
            Expr::TailCall(data) => Some((data.target.clone(), data.args.as_slice())),
            _ => None,
        }
    }
    // Constructor application: `Type.Variant(args…)` parses as a FnCall
    // on a dotted uppercase-leaf name; resolved bodies may carry
    // `Expr::Constructor`.
    fn ctor_of(e: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
        match &e.node {
            Expr::FnCall(callee, args) => {
                let name = expr_to_dotted_name(&callee.node)?;
                let leaf = name.rsplit('.').next()?;
                if !leaf.chars().next().is_some_and(|c| c.is_uppercase()) {
                    return None;
                }
                Some((name, args.iter().collect()))
            }
            Expr::Constructor(name, payload) => {
                let args: Vec<&Spanned<Expr>> = match payload.as_deref() {
                    None => Vec::new(),
                    Some(Spanned {
                        node: Expr::Tuple(items),
                        ..
                    }) => items.iter().collect(),
                    Some(single) => vec![single],
                };
                Some((name.clone(), args))
            }
            _ => None,
        }
    }
    fn is_ident(e: &Spanned<Expr>, name: &str) -> bool {
        ident_of(e) == Some(name)
    }
    fn is_plus_one(e: &Spanned<Expr>, name: &str) -> bool {
        matches!(&e.node, Expr::BinOp(crate::ast::BinOp::Add, l, r)
            if ident_of(l) == Some(name)
                && matches!(&r.node, Expr::Literal(Literal::Int(1))))
    }
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        (fd.effects.is_empty() && fd.name != "main").then_some(fd)
    };
    fn single_match(fd: &FnDef) -> Option<(&Spanned<Expr>, &[crate::ast::MatchArm])> {
        let [Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let Expr::Match { subject, arms } = &body.node else {
            return None;
        };
        Some((subject, arms.as_slice()))
    }
    /// (Some-binder name, Some arm) of a 2-arm charAt(p0, p1) match.
    fn charat_match(fd: &FnDef) -> Option<(String, &crate::ast::MatchArm)> {
        let (subject, arms) = single_match(fd)?;
        let (callee, args) = call_of(subject)?;
        if callee != "String.charAt"
            || args.len() != 2
            || !is_ident(&args[0], &fd.params[0].0)
            || !is_ident(&args[1], &fd.params[1].0)
            || arms.len() != 2
        {
            return None;
        }
        arms.iter()
            .any(|a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.None" && b.is_empty()))
            .then_some(())?;
        let some_arm = arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.Some" && b.len() == 1),
        )?;
        let Pattern::Constructor(_, binders) = &some_arm.pattern else {
            return None;
        };
        Some((binders[0].clone(), some_arm))
    }
    /// `match P(c) { true -> scan(s, pos+1, start, false), false -> _ }`
    /// dispatcher tail shared by `pos_fn` and `sign_fn`. Returns
    /// (predicate name, scanner name).
    fn digit_dispatch(
        fd: &FnDef,
        s_param: &str,
        pos_param: &str,
        start_param: &str,
        c_param: &str,
    ) -> Option<(String, String)> {
        let (subject, arms) = single_match(fd)?;
        let (pred, pred_args) = call_of(subject)?;
        if pred.contains('.') || pred_args.len() != 1 || !is_ident(&pred_args[0], c_param) {
            return None;
        }
        if arms.len() != 2
            || !arms
                .iter()
                .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))
        {
            return None;
        }
        let true_arm = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))?;
        let (scan, scan_args) = call_of(&true_arm.body)?;
        (scan_args.len() == 4
            && is_ident(&scan_args[0], s_param)
            && is_plus_one(&scan_args[1], pos_param)
            && is_ident(&scan_args[2], start_param)
            && matches!(&scan_args[3].node, Expr::Literal(Literal::Bool(false))))
        .then_some((pred, scan))
    }

    // ---- Law shape -------------------------------------------------
    let (lhs_callee, lhs_args) = call_of(&law.lhs)?;
    if lhs_callee.rsplit('.').next()? != fn_name || lhs_args.len() != 2 {
        return None;
    }
    let ser_arg = &lhs_args[0];
    if !matches!(&lhs_args[1].node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let (ser_name, ser_args) = call_of(ser_arg)?;
    if ser_args.len() != 1 {
        return None;
    }
    let ctor_expr = &ser_args[0];
    let (ctor_name, ctor_args) = ctor_of(ctor_expr)?;
    if ctor_args.len() != 1 || !is_ident(ctor_args[0], given) {
        return None;
    }
    // The constructor's variant must carry exactly one Int field — the
    // serializer's ADT measure is then constant on `C(n)` for free `n`
    // (fuel computes; `ser(C(n)) = String.fromInt n` can be `rfl`).
    {
        let (type_name, variant_name) = ctor_name.rsplit_once('.')?;
        let Some(crate::ast::TypeDef::Sum { variants, .. }) = inputs.find_type_def(type_name)
        else {
            return None;
        };
        variants
            .iter()
            .any(|v| v.name == variant_name && v.fields.len() == 1 && v.fields[0].trim() == "Int")
            .then_some(())?;
    }
    let (ok_name, rhs_args) = ctor_of(&law.rhs)?;
    if rhs_args.len() != 2 || rhs_args[0].node != ctor_expr.node {
        return None;
    }
    let (len_callee, len_args) = call_of(rhs_args[1])?;
    if len_callee != "String.len" || len_args.len() != 1 || len_args[0].node != ser_arg.node {
        return None;
    }

    // ---- Serializer: has the `C(x) -> String.fromInt(x)` arm --------
    let ser_fd = resolve_user_fn(&ser_name)?;
    {
        let (_, arms) = single_match(ser_fd)?;
        arms.iter()
            .any(|a| {
                let Pattern::Constructor(n, binders) = &a.pattern else {
                    return false;
                };
                if n != &ctor_name || binders.len() != 1 {
                    return false;
                }
                call_of(&a.body).is_some_and(|(callee, args)| {
                    callee == "String.fromInt" && args.len() == 1 && is_ident(&args[0], &binders[0])
                })
            })
            .then_some(())?;
    }

    // ---- Parser: head-char dispatch ---------------------------------
    let parse_fd = resolve_user_fn(fn_name)?;
    if parse_fd.params.len() != 2
        || parse_fd.params[0].1 != "String"
        || parse_fd.params[1].1 != "Int"
    {
        return None;
    }
    let (p_s, p_start) = (parse_fd.params[0].0.as_str(), parse_fd.params[1].0.as_str());
    let (c_name, some_arm) = charat_match(parse_fd)?;
    let Expr::Match {
        subject: c_subject,
        arms: c_arms,
    } = &some_arm.body.node
    else {
        return None;
    };
    if !is_ident(c_subject, &c_name) || c_arms.len() != 3 {
        return None;
    }
    // Arm order is load-bearing: the emission's `split` bullets follow
    // the textual arm order of the emitted Lean match.
    if !matches!(&c_arms[0].pattern, Pattern::Literal(Literal::Str(s)) if s == "-")
        || !matches!(&c_arms[1].pattern, Pattern::Literal(Literal::Str(s)) if s == "0")
        || !matches!(&c_arms[2].pattern, Pattern::Wildcard)
    {
        return None;
    }
    let (neg_name, neg_args) = call_of(&c_arms[0].body)?;
    (neg_args.len() == 3
        && is_ident(&neg_args[0], p_s)
        && is_plus_one(&neg_args[1], p_start)
        && is_ident(&neg_args[2], p_start))
    .then_some(())?;
    let (scan_zero, zero_args) = call_of(&c_arms[1].body)?;
    (zero_args.len() == 4
        && is_ident(&zero_args[0], p_s)
        && is_plus_one(&zero_args[1], p_start)
        && is_ident(&zero_args[2], p_start)
        && matches!(&zero_args[3].node, Expr::Literal(Literal::Bool(true))))
    .then_some(())?;
    let (pos_name, pos_args) = call_of(&c_arms[2].body)?;
    (pos_args.len() == 3
        && is_ident(&pos_args[0], p_s)
        && is_ident(&pos_args[1], p_start)
        && is_ident(&pos_args[2], &c_name))
    .then_some(())?;

    // ---- pos_fn / neg_fn / sign_fn ----------------------------------
    let pos_fd = resolve_user_fn(&pos_name)?;
    if pos_fd.params.len() != 3 {
        return None;
    }
    let (pred_a, scan_a) = digit_dispatch(
        pos_fd,
        &pos_fd.params[0].0,
        &pos_fd.params[1].0,
        &pos_fd.params[1].0,
        &pos_fd.params[2].0,
    )?;

    let neg_fd = resolve_user_fn(&neg_name)?;
    if neg_fd.params.len() != 3 {
        return None;
    }
    let (n_s, n_pos, n_start) = (
        neg_fd.params[0].0.as_str(),
        neg_fd.params[1].0.as_str(),
        neg_fd.params[2].0.as_str(),
    );
    let (c2_name, neg_some_arm) = charat_match(neg_fd)?;
    let Expr::Match {
        subject: c2_subject,
        arms: c2_arms,
    } = &neg_some_arm.body.node
    else {
        return None;
    };
    if !is_ident(c2_subject, &c2_name) || c2_arms.len() != 2 {
        return None;
    }
    if !matches!(&c2_arms[0].pattern, Pattern::Literal(Literal::Str(s)) if s == "0")
        || !matches!(&c2_arms[1].pattern, Pattern::Wildcard)
    {
        return None;
    }
    let (scan_b, nz_args) = call_of(&c2_arms[0].body)?;
    (nz_args.len() == 4
        && is_ident(&nz_args[0], n_s)
        && is_plus_one(&nz_args[1], n_pos)
        && is_ident(&nz_args[2], n_start)
        && matches!(&nz_args[3].node, Expr::Literal(Literal::Bool(true))))
    .then_some(())?;
    let (sign_name, sign_args) = call_of(&c2_arms[1].body)?;
    (sign_args.len() == 4
        && is_ident(&sign_args[0], n_s)
        && is_ident(&sign_args[1], n_pos)
        && is_ident(&sign_args[2], n_start)
        && is_ident(&sign_args[3], &c2_name))
    .then_some(())?;

    let sign_fd = resolve_user_fn(&sign_name)?;
    if sign_fd.params.len() != 4 {
        return None;
    }
    let (pred_b, scan_c) = digit_dispatch(
        sign_fd,
        &sign_fd.params[0].0,
        &sign_fd.params[1].0,
        &sign_fd.params[2].0,
        &sign_fd.params[3].0,
    )?;

    // One scanner, one predicate, everywhere.
    if pred_a != pred_b || scan_a != scan_b || scan_a != scan_c || scan_a != scan_zero {
        return None;
    }
    let scanner_name = scan_a;
    let pred_name = pred_a;

    // ---- Scanner: recognized shape + string-pos fuel contract -------
    let scan_fd = resolve_user_fn(&scanner_name)?;
    let shape = crate::codegen::proof_recognize::detect_string_pos_scan(scan_fd)?;
    if shape.predicate_fn != pred_name || shape.param_pins != vec![None, Some(false)] {
        return None;
    }
    let pred_fd = resolve_user_fn(&pred_name)?;
    if !crate::codegen::proof_recognize::scan_predicate_fn_ok(pred_fd) {
        return None;
    }
    let scan_key = match inputs.fn_owning_scope(scan_fd) {
        Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &scan_fd.name),
        None => crate::ir::FnKey::entry(&scan_fd.name),
    };
    let scan_contract = inputs
        .symbol_table
        .fn_id_of(&scan_key)
        .and_then(|id| fn_contracts.get(&id))?;
    if !matches!(
        scan_contract.recursion,
        Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::StringLenMinusPos { .. },
        })
    ) {
        return None;
    }
    // Exit: `finish(s, start, pos, false)`.
    let (finish_name, exit_args) = call_of(&shape.exit_expr)?;
    (exit_args.len() == 4
        && is_ident(&exit_args[0], &scan_fd.params[0].0)
        && is_ident(&exit_args[1], &scan_fd.params[2].0)
        && is_ident(&exit_args[2], &scan_fd.params[1].0)
        && matches!(&exit_args[3].node, Expr::Literal(Literal::Bool(false))))
    .then_some(())?;

    // ---- finish_fn: slice + asFloat dispatch -------------------------
    let finish_fd = resolve_user_fn(&finish_name)?;
    if finish_fd.params.len() != 4 {
        return None;
    }
    let [
        Stmt::Binding(num_name, _, slice_expr),
        Stmt::Expr(finish_match),
    ] = finish_fd.body.stmts()
    else {
        return None;
    };
    {
        let (slice_callee, slice_args) = call_of(slice_expr)?;
        (slice_callee == "String.slice"
            && slice_args.len() == 3
            && is_ident(&slice_args[0], &finish_fd.params[0].0)
            && is_ident(&slice_args[1], &finish_fd.params[1].0)
            && is_ident(&slice_args[2], &finish_fd.params[2].0))
        .then_some(())?;
    }
    let Expr::Match {
        subject: float_subject,
        arms: float_arms,
    } = &finish_match.node
    else {
        return None;
    };
    if !is_ident(float_subject, &finish_fd.params[3].0) || float_arms.len() != 2 {
        return None;
    }
    float_arms
        .iter()
        .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))
        .then_some(())?;
    let int_arm = float_arms
        .iter()
        .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))?;
    let (finish_int_name, fi_args) = call_of(&int_arm.body)?;
    (fi_args.len() == 2
        && is_ident(&fi_args[0], num_name)
        && is_ident(&fi_args[1], &finish_fd.params[2].0))
    .then_some(())?;

    // ---- finish_int_fn: Int.fromString leaf rebuilding the law's rhs --
    let fint_fd = resolve_user_fn(&finish_int_name)?;
    if fint_fd.params.len() != 2 {
        return None;
    }
    {
        let (subject, arms) = single_match(fint_fd)?;
        let (callee, args) = call_of(subject)?;
        (callee == "Int.fromString" && args.len() == 1 && is_ident(&args[0], &fint_fd.params[0].0))
            .then_some(())?;
        let ok_arm = arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Result.Ok" && b.len() == 1),
        )?;
        let Pattern::Constructor(_, ok_binders) = &ok_arm.pattern else {
            return None;
        };
        let (arm_ok_name, arm_args) = ctor_of(&ok_arm.body)?;
        (arm_ok_name == ok_name
            && arm_args.len() == 2
            && is_ident(arm_args[1], &fint_fd.params[1].0))
        .then_some(())?;
        let (arm_ctor, arm_ctor_args) = ctor_of(arm_args[0])?;
        (arm_ctor == ctor_name
            && arm_ctor_args.len() == 1
            && is_ident(arm_ctor_args[0], &ok_binders[0]))
        .then_some(())?;
    }

    Some(crate::ir::ProofStrategy::IntDecimalRoundtrip {
        parse_fn: fn_name.to_string(),
        neg_fn: neg_name,
        pos_fn: pos_name,
        sign_fn: sign_name,
        scanner_fn: scanner_name,
        predicate_fn: pred_name,
        finish_fn: finish_name,
        finish_int_fn: finish_int_name,
        serializer_fn: ser_name,
    })
}

/// Builtin-roundtrip detector for [`ProofStrategy::SimpOverPreludeLemmas`]
/// (the last typed fallback before `BackendDispatch`, after
/// `detect_finite_domain_cases`). Fires for a no-when law with givens
/// whose lhs calls resolve to user fns that are each either:
/// - NON-recursive — these seed the transitive unfold cone (expanded
///   through non-recursive bodies only), or
/// - recursive AND classified (a `FnContract` exists — never a
///   `partial def`, whose missing equation lemmas would make `simp
///   [fn]` a hard build error) AND called with measure-closed args at
///   every lhs call site: each arg is a literal or a constructor whose
///   payload is literals/idents over scalar-only variant fields, so
///   every fuel-metric family (`natAbs`, length, `sizeOf`, string-pos)
///   computes to a `Nat` literal and simp drives the `__fuel`
///   equations through (empirically `toString' (Json.jsonInt n) =
///   String.fromInt n` is plain `rfl` despite fuel).
///
/// Recursive fns reached only TRANSITIVELY (inside cone bodies) do
/// not block and are left out of the unfold set — under the law's
/// pinned literal args their branches are usually dead (simp computes
/// the dispatching match away, e.g. `afterIntChar … "]"` never reaches
/// `scanFracFirst`); a live one just stops simp, which the emit's
/// `first | (simp …; done) | sorry` alternation catches as an honest
/// `sorry`, never a build error.
///
/// The builtin call names observed across the law sides + every cone
/// body (recursive ones included — `String.fromInt` lives inside
/// `toString`'s body) become the registry keys the Lean emitter maps
/// to prelude spec lemma names (`lean::prelude_spec_lemmas_for_builtins`,
/// single source of truth next to the lemmas themselves). String `+`
/// is recorded as the synthetic `String.concat` marker so the
/// `HAdd`-normalising `String.add_eq_append` fires for laws that
/// concatenate without naming any `String.*` builtin.
fn detect_simp_over_prelude_lemmas(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<crate::ir::ProofStrategy> {
    use std::collections::BTreeSet;

    if law.givens.is_empty() {
        return None;
    }
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        if !fd.effects.is_empty() || fd.name == "main" {
            return None;
        }
        Some(fd)
    };
    // The law must actually exercise its subject fn in the lhs — the
    // unfold list is anchored on it (subject first, mirroring
    // `detect_enum_constant_fold`'s ordering contract).
    resolve_user_fn(fn_name)?;
    let recursive = inputs.recursive_pure_fn_names();

    // Seed from direct lhs calls only (the rhs of roundtrip laws is a
    // constructor expectation; a user fn there would also show up in
    // the lhs cone for every shape this detector targets).
    let mut lhs_calls: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut lhs_calls);
    let mut cone: BTreeSet<String> = BTreeSet::new();
    let mut fuel_fns: BTreeSet<String> = BTreeSet::new();
    for name in &lhs_calls {
        let Some(fd) = resolve_user_fn(name) else {
            continue;
        };
        if !recursive.contains(&fd.name) {
            cone.insert(fd.name.clone());
            continue;
        }
        // Recursive direct seed: classified + measure-closed or bust.
        // **syntax-discovery-only** (epic #170 Phase 8 guardrail):
        // `fn_owning_scope` resolves the owning dep module via
        // pointer-eq; its `None` IS the entry scope by construction,
        // so the entry key is the correct typed identity here (same
        // shape as `ProofLowerInputs::recursive_pure_fn_names`).
        let fn_key = match inputs.fn_owning_scope(fd) {
            Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &fd.name),
            None => crate::ir::FnKey::entry(&fd.name),
        };
        let classified = inputs
            .symbol_table
            .fn_id_of(&fn_key)
            .is_some_and(|id| fn_contracts.contains_key(&id));
        if !classified || !calls_have_measure_closed_args(&law.lhs, &fd.name, inputs) {
            return None;
        }
        fuel_fns.insert(fd.name.clone());
    }
    if !cone.contains(fn_name) && !fuel_fns.contains(fn_name) {
        return None;
    }

    // Transitive cone expansion through NON-recursive bodies only.
    loop {
        let before = cone.len();
        let snapshot: Vec<String> = cone.iter().cloned().collect();
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
                if let Some(callee) = resolve_user_fn(&c)
                    && !recursive.contains(&callee.name)
                {
                    cone.insert(callee.name.clone());
                }
            }
        }
        if cone.len() == before {
            break;
        }
    }

    // Registry keys: builtin calls across law sides + every cone /
    // fuel fn body (fuel bodies carry the partner builtins —
    // `String.fromInt` lives inside `toString`).
    let mut builtins: BTreeSet<String> = BTreeSet::new();
    collect_builtin_calls_expr(&law.lhs, &mut builtins);
    collect_builtin_calls_expr(&law.rhs, &mut builtins);
    for name in cone.iter().chain(fuel_fns.iter()) {
        if let Some(fd) = resolve_user_fn(name) {
            for stmt in fd.body.stmts() {
                match stmt {
                    crate::ast::Stmt::Binding(_, _, e) | crate::ast::Stmt::Expr(e) => {
                        collect_builtin_calls_expr(e, &mut builtins);
                    }
                }
            }
        }
    }

    // Subject first (Lean peels the outermost call layer first), then
    // the sorted rest — mirrors `detect_enum_constant_fold`.
    let mut unfold_fns: Vec<String> = Vec::new();
    if cone.contains(fn_name) {
        unfold_fns.push(fn_name.to_string());
    }
    unfold_fns.extend(cone.iter().filter(|n| *n != fn_name).cloned());
    Some(crate::ir::ProofStrategy::SimpOverPreludeLemmas {
        unfold_fns,
        fuel_fns: fuel_fns.into_iter().collect(),
        builtins: builtins.into_iter().collect(),
    })
}

/// True when every call to `target` inside `expr` passes only
/// measure-closed args — see [`expr_is_measure_closed`]. Used by the
/// `SimpOverPreludeLemmas` detector to admit a recursive (fuel-
/// emitted) fn as a direct lhs seed: closed args mean the fuel value
/// computes to a `Nat` literal, so simp can drive the `__fuel`
/// equations regardless of which fuel-metric family the classifier
/// picked.
fn calls_have_measure_closed_args(
    expr: &Spanned<crate::ast::Expr>,
    target: &str,
    inputs: &ProofLowerInputs,
) -> bool {
    use crate::ast::Expr;
    let self_ok = match &expr.node {
        Expr::FnCall(callee, args) => {
            let is_target = expr_to_dotted_name(&callee.node)
                .is_some_and(|n| n.rsplit('.').next().unwrap_or(&n) == target);
            !is_target || args.iter().all(|a| expr_is_measure_closed(a, inputs))
        }
        _ => true,
    };
    if !self_ok {
        return false;
    }
    match &expr.node {
        Expr::FnCall(callee, args) => {
            calls_have_measure_closed_args(callee, target, inputs)
                && args
                    .iter()
                    .all(|a| calls_have_measure_closed_args(a, target, inputs))
        }
        Expr::BinOp(_, l, r) => {
            calls_have_measure_closed_args(l, target, inputs)
                && calls_have_measure_closed_args(r, target, inputs)
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            calls_have_measure_closed_args(inner, target, inputs)
        }
        Expr::Attr(obj, _) => calls_have_measure_closed_args(obj, target, inputs),
        Expr::Constructor(_, Some(inner)) => calls_have_measure_closed_args(inner, target, inputs),
        Expr::List(elems) | Expr::Tuple(elems) => elems
            .iter()
            .all(|e| calls_have_measure_closed_args(e, target, inputs)),
        _ => true,
    }
}

/// Measure-closedness of a call arg fed to a recursive (fuel-emitted)
/// fn: every fuel-metric family (`natAbs n`, list/string length,
/// `sizeOf`/`averMeasure*`) reduces to a `Nat` literal when the arg is
/// - a literal (possibly negated), or
/// - a constructor of a user `Sum` type whose matched variant has only
///   scalar fields (`Int`/`Float`/`String`/`Bool`) and whose payload
///   exprs are literals or idents — the ADT measure is constant on
///   such a constructor regardless of the payload values (e.g.
///   `averMeasureJson (Json.jsonInt n) = 2` for free `n`).
///
/// A bare ident is OPEN (a free `String` given feeds `s.length`-shaped
/// fuel; a free `Int` feeds `natAbs`), as is anything else.
fn expr_is_measure_closed(expr: &Spanned<crate::ast::Expr>, inputs: &ProofLowerInputs) -> bool {
    use crate::ast::Expr;
    let payload_atom = |e: &Spanned<Expr>| -> bool {
        matches!(
            e.node,
            Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved { .. }
        ) || matches!(&e.node, Expr::Neg(inner) if matches!(inner.node, Expr::Literal(_)))
    };
    let variant_fields_scalar = |ctor_name: &str| -> bool {
        let (type_name, variant_name) = match ctor_name.rsplit_once('.') {
            Some(pair) => pair,
            None => return false,
        };
        let Some(crate::ast::TypeDef::Sum { variants, .. }) = inputs.find_type_def(type_name)
        else {
            return false;
        };
        variants.iter().any(|v| {
            v.name == variant_name
                && v.fields
                    .iter()
                    .all(|f| matches!(f.trim(), "Int" | "Float" | "String" | "Bool"))
        })
    };
    match &expr.node {
        Expr::Literal(_) => true,
        Expr::Neg(inner) => matches!(inner.node, Expr::Literal(_)),
        Expr::Constructor(name, payload) => {
            let payload_ok = match payload.as_deref() {
                None => true,
                Some(Spanned {
                    node: Expr::Tuple(items),
                    ..
                }) => items.iter().all(payload_atom),
                Some(single) => payload_atom(single),
            };
            payload_ok && variant_fields_scalar(name)
        }
        // `Type.Variant(args…)` parses as a FnCall on a dotted
        // uppercase-leaf name; `Type.Variant` (fieldless) as an Attr.
        Expr::FnCall(callee, args) => expr_to_dotted_name(&callee.node).is_some_and(|n| {
            n.rsplit('.')
                .next()
                .is_some_and(|leaf| leaf.chars().next().is_some_and(|c| c.is_uppercase()))
                && variant_fields_scalar(&n)
                && args.iter().all(payload_atom)
        }),
        Expr::Attr(obj, field) => {
            let head = match &obj.node {
                Expr::Ident(n) => n.clone(),
                _ => return false,
            };
            field.chars().next().is_some_and(|c| c.is_uppercase())
                && variant_fields_scalar(&format!("{head}.{field}"))
        }
        _ => false,
    }
}

/// Collect builtin (`Namespace.method`) call names — the complement of
/// [`collect_fn_calls_expr`], which deliberately FILTERS the builtin
/// scalar namespaces out of the user-fn unfold set. Records every
/// dotted callee whose head segment starts uppercase and whose leaf is
/// lowercase (`String.slice`, `Int.fromString`, `List.concat`, …) plus
/// the synthetic `String.concat` marker for `+` over a string operand
/// (literal or interpolation — the type-blind AST signal for the
/// custom `HAdd String` instance). Drives the registry keys of
/// [`ProofStrategy::SimpOverPreludeLemmas`].
fn collect_builtin_calls_expr(
    expr: &Spanned<crate::ast::Expr>,
    out: &mut std::collections::BTreeSet<String>,
) {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(f, args) => {
            if let Some(name) = expr_to_dotted_name(&f.node)
                && name.contains('.')
            {
                let head = name.split('.').next().unwrap_or(&name);
                let leaf = name.rsplit('.').next().unwrap_or(&name);
                if head.chars().next().is_some_and(|c| c.is_uppercase())
                    && leaf.chars().next().is_some_and(|c| c.is_lowercase())
                {
                    out.insert(name);
                }
            }
            collect_builtin_calls_expr(f, out);
            for arg in args {
                collect_builtin_calls_expr(arg, out);
            }
        }
        Expr::BinOp(op, l, r) => {
            let is_stringy = |e: &Spanned<Expr>| {
                matches!(
                    &e.node,
                    Expr::Literal(crate::ast::Literal::Str(_)) | Expr::InterpolatedStr(_)
                )
            };
            if matches!(op, crate::ast::BinOp::Add) && (is_stringy(l) || is_stringy(r)) {
                out.insert("String.concat".to_string());
            }
            collect_builtin_calls_expr(l, out);
            collect_builtin_calls_expr(r, out);
        }
        Expr::Attr(obj, _) => collect_builtin_calls_expr(obj, out),
        Expr::Match { subject, arms, .. } => {
            collect_builtin_calls_expr(subject, out);
            for arm in arms {
                collect_builtin_calls_expr(&arm.body, out);
            }
        }
        Expr::TailCall(boxed) => {
            for arg in &boxed.args {
                collect_builtin_calls_expr(arg, out);
            }
        }
        Expr::ErrorProp(inner) | Expr::Neg(inner) => collect_builtin_calls_expr(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_builtin_calls_expr(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_builtin_calls_expr(e, out);
            }
        }
        Expr::List(elems) | Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_builtin_calls_expr(e, out);
            }
        }
        _ => {}
    }
}

/// Closed finite-domain enumeration detector (the last typed fallback
/// before `BackendDispatch`, after `detect_enum_constant_fold`). Fires
/// iff the law has at least one given and EVERY given's type has a
/// closed, small, enumerable domain:
/// - `Bool` (domain size 2), or
/// - a user-declared enum (`TypeDef::Sum`) whose constructors are ALL
///   fieldless (domain size = constructor count),
///
/// with the product of domain sizes ≤ 16. Exhaustive `cases` over the
/// givens then yields one closed ground goal per domain combination,
/// which `rfl` / `decide` compute out — even through fuel wrappers
/// (constant-measure constructor args compute through fuel) — so
/// unlike `detect_enum_constant_fold` there is deliberately NO
/// call-shape inspection, NO return-type gate and NO recursion gate.
/// A leaf the cascade can't close degrades to an honest caught `sorry`
/// in the Lean emit, never a false universal claim. Returns the given
/// names in intro order (the Lean emitter's `cases` targets); `None`
/// otherwise.
fn detect_finite_domain_cases(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    if law.givens.is_empty() {
        return None;
    }
    let mut domain_product: usize = 1;
    for given in &law.givens {
        let size = finite_domain_size(&given.type_name, inputs)?;
        domain_product = domain_product.checked_mul(size)?;
        if domain_product > 16 {
            return None;
        }
    }
    Some(law.givens.iter().map(|g| g.name.clone()).collect())
}

/// Size of a type's closed enumerable domain: 2 for `Bool`, the
/// constructor count for a user-declared enum whose constructors are
/// ALL fieldless. `None` for everything else — open domains
/// (`Int` / `String` / collections), payload-carrying ADTs (whose
/// `cases` would introduce fresh free variables the per-leaf
/// `rfl`/`decide` cascade can't compute out), records, and effect
/// oracle types (`Random.int`, …) which `find_type_def` doesn't
/// resolve.
fn finite_domain_size(type_name: &str, inputs: &ProofLowerInputs) -> Option<usize> {
    if type_name == "Bool" {
        return Some(2);
    }
    match inputs.find_type_def(type_name)? {
        crate::ast::TypeDef::Sum { variants, .. }
            if variants.iter().all(|v| v.fields.is_empty()) =>
        {
            Some(variants.len())
        }
        _ => None,
    }
}

/// Ground enum/ADT constant-fold detector (new fallback before
/// `BackendDispatch`). Fires when the verified fn has at least one
/// non-Int (ADT/enum) param and the law pins *every* such param to a
/// constructor literal at the call site — so the constructor selects a
/// fixed match arm and the whole non-recursive call tree folds to a
/// closed term. Any scalar `given`s are quantified but irrelevant to
/// the chosen branch. Returns the ordered unfold list (fn first, then
/// transitively-reached callees) on match; `None` otherwise.
///
/// CONSERVATIVE by construction:
/// - rejects when the fn has no non-Int param (LinearArithmetic /
///   omega owns the all-Int shape);
/// - rejects unless *every* non-Int param at *every* call to `fn_name`
///   in lhs/rhs is a constructor literal (an unpinned ADT arg would
///   leave a free variable the `split`/`rfl` cascade can't discharge);
/// - rejects when any fn in the unfold set is self-recursive (a single
///   `simp only [fn]` step leaves a stale recursive call);
/// - rejects when the verified fn calls a builtin-collection method
///   (`List.*` / `Map.* `/ `Vector.*`) — those don't fold to a ground
///   term by `split`/`rfl`/`decide`.
fn detect_enum_constant_fold(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<Vec<String>> {
    use std::collections::BTreeSet;

    let outer_fd = inputs.find_fn_def_by_call_name(fn_name)?;
    // Need at least one non-Int (ADT/enum) param. An all-Int fn is the
    // LinearArithmetic/omega shape; this detector must not poach it.
    let non_int_params: Vec<&str> = outer_fd
        .params
        .iter()
        .enumerate()
        .filter(|(_, (_, t))| t != "Int")
        .map(|(i, _)| outer_fd.params[i].1.as_str())
        .collect();
    if non_int_params.is_empty() {
        return None;
    }
    // The result must be a scalar (`Int` / `Bool`) ground equality the
    // `split`/`rfl`/`decide` cascade can discharge. A wrapper return
    // (Result/Option/record) is out of scope for this fallback.
    let ret = outer_fd.return_type.as_str();
    if ret != "Int" && ret != "Bool" {
        return None;
    }

    // Every call to `fn_name` in lhs/rhs must pin each non-Int param
    // position to a constructor literal. The Int positions may carry
    // the quantified scalar givens (they're irrelevant to the branch).
    let mut saw_call = false;
    let mut ok = true;
    check_enum_calls_pinned(&law.lhs, fn_name, outer_fd, &mut saw_call, &mut ok);
    check_enum_calls_pinned(&law.rhs, fn_name, outer_fd, &mut saw_call, &mut ok);
    if !saw_call || !ok {
        return None;
    }

    // Build the transitive unfold set from both law sides + the outer
    // fn, exactly like `detect_simp_omega_unfold`.
    let mut fn_names: BTreeSet<String> = BTreeSet::new();
    collect_fn_calls_expr(&law.lhs, &mut fn_names);
    collect_fn_calls_expr(&law.rhs, &mut fn_names);
    fn_names.insert(fn_name.to_string());
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

    // Self-recursion rejection — a single unfold step can't close a
    // recursive call. Mirrors the `detect_simp_omega_unfold` guard.
    for fd in iter_all_fn_defs(inputs) {
        if !fn_names.contains(&fd.name) {
            continue;
        }
        let mut self_only: BTreeSet<String> = BTreeSet::new();
        self_only.insert(fd.name.clone());
        if body_calls_any_of_inputs(&fd.body, &self_only) {
            return None;
        }
    }

    // Order: top-level law fn first (Lean's `simp only`/`unfold` peels
    // the outermost call layer first), then the transitively-reached
    // callees.
    let mut ordered: Vec<String> = Vec::new();
    if fn_names.contains(fn_name) {
        ordered.push(fn_name.to_string());
    }
    for n in &fn_names {
        if n != fn_name {
            ordered.push(n.clone());
        }
    }
    Some(ordered)
}

/// Walk an expr looking for calls to `fn_name`; for each, verify every
/// non-Int param position carries a constructor literal. Sets `saw_call`
/// on the first matched call and clears `ok` on any unpinned ADT arg.
fn check_enum_calls_pinned(
    expr: &Spanned<crate::ast::Expr>,
    fn_name: &str,
    fd: &FnDef,
    saw_call: &mut bool,
    ok: &mut bool,
) {
    use crate::ast::Expr;
    match &expr.node {
        Expr::FnCall(callee, args) => {
            if let Some(name) = expr_to_dotted_name(&callee.node) {
                let leaf = name.rsplit('.').next().unwrap_or(&name);
                if leaf == fn_name && args.len() == fd.params.len() {
                    *saw_call = true;
                    for (i, (_, ptype)) in fd.params.iter().enumerate() {
                        if ptype != "Int" && !is_constructor_literal(&args[i].node) {
                            *ok = false;
                        }
                    }
                }
            }
            check_enum_calls_pinned(callee, fn_name, fd, saw_call, ok);
            for a in args {
                check_enum_calls_pinned(a, fn_name, fd, saw_call, ok);
            }
        }
        Expr::BinOp(_, l, r) => {
            check_enum_calls_pinned(l, fn_name, fd, saw_call, ok);
            check_enum_calls_pinned(r, fn_name, fd, saw_call, ok);
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => {
            check_enum_calls_pinned(inner, fn_name, fd, saw_call, ok);
        }
        Expr::Attr(obj, _) => check_enum_calls_pinned(obj, fn_name, fd, saw_call, ok),
        Expr::List(elems) | Expr::Tuple(elems) => {
            for e in elems {
                check_enum_calls_pinned(e, fn_name, fd, saw_call, ok);
            }
        }
        _ => {}
    }
}

/// True when `expr` is a fixed ADT/enum constructor literal — either a
/// built-in `Expr::Constructor` (`Option.Some`, `Result.Ok`) or a
/// dotted `Type.Variant` access (`CellContent.Empty`, `Color.Black`)
/// where both segments start uppercase (a user enum variant, not a
/// builtin scalar-namespace method like `Int.max`).
fn is_constructor_literal(expr: &crate::ast::Expr) -> bool {
    use crate::ast::Expr;
    match expr {
        Expr::Constructor(_, _) => true,
        Expr::Attr(obj, field) => {
            let head_upper = matches!(
                &obj.node,
                Expr::Ident(n) if n.chars().next().is_some_and(|c| c.is_uppercase())
            );
            let field_upper = field.chars().next().is_some_and(|c| c.is_uppercase());
            head_upper && field_upper
        }
        _ => false,
    }
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
                //
                // Builtin *scalar*-namespace methods (`Int.max`,
                // `Float.abs`, `Bool.and`, `String.len`, …) have a
                // lowercase leaf but lower to **core** Lean ops, not
                // to unfoldable user defs — `Int.max` becomes `max`,
                // there is no constant named `Int.max` in scope. Citing
                // them in `simp only [...]` is a hard `unknown constant`
                // error, so the scalar namespaces are excluded by their
                // HEAD segment. Cross-module USER calls (`MyModule.helper`)
                // keep flowing through — only the four builtin scalar
                // namespaces are filtered.
                let last = name.rsplit('.').next().unwrap_or(&name);
                let head = name.split('.').next().unwrap_or(&name);
                let is_builtin_scalar_ns = matches!(head, "Int" | "Float" | "Bool" | "String");
                if !is_builtin_scalar_ns && last.chars().next().is_some_and(|c| c.is_lowercase()) {
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
        // Value-carrying expressions whose sub-exprs hold fn calls the proof's
        // unfold set needs. Crucially `ErrorProp` (`?`): a refinement-pipeline
        // body like `na = fromInt(a)?; add(na, nb)?` hides every call behind `?`,
        // so without this arm the unfold set misses `fromInt`/`add` and the
        // LinearArithmetic tactic stalls with "simp made no progress". Mirrors
        // `proof_recognize::collect_called_fns`.
        Expr::ErrorProp(inner) | Expr::Neg(inner) => collect_fn_calls_expr(inner, out),
        Expr::Constructor(_, Some(arg)) => collect_fn_calls_expr(arg, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, e) in fields {
                collect_fn_calls_expr(e, out);
            }
        }
        Expr::List(elems) | Expr::Tuple(elems) | Expr::IndependentProduct(elems, _) => {
            for e in elems {
                collect_fn_calls_expr(e, out);
            }
        }
        _ => {}
    }
}

/// Collect the user-type *names* referenced by a type annotation string
/// (`List<Run>`, `Result<Tree, String>`, `(A, B)`, …) into `out`. Parses
/// the annotation with [`parse_type_annotation`] and walks the resulting
/// `Type`, harvesting every `Type::Named` leaf. Builtin scalars and the
/// collection constructors contribute nothing themselves — only the named
/// (potential-ADT) leaves land; the caller filters those to actual user
/// `TypeDef`s. Drives [`LawProofCone`]'s `types` alphabet.
fn collect_named_types_in_annotation(
    annotation: &str,
    out: &mut std::collections::BTreeSet<String>,
) {
    fn walk(ty: &crate::types::Type, out: &mut std::collections::BTreeSet<String>) {
        use crate::types::Type;
        match ty {
            // syntax-discovery-only: collects named types from a source type
            // annotation into the LawProofCone alphabet; `name` is the discovery
            // cone key, not a backend-routing / output identity decision.
            Type::Named { name, .. } => {
                out.insert(name.clone());
            }
            Type::Result(a, b) | Type::Map(a, b) => {
                walk(a, out);
                walk(b, out);
            }
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => walk(inner, out),
            Type::Tuple(items) => {
                for t in items {
                    walk(t, out);
                }
            }
            Type::Fn(args, ret, _) => {
                for a in args {
                    walk(a, out);
                }
                walk(ret, out);
            }
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Bool
            | Type::Unit
            | Type::Var(_)
            | Type::Invalid => {}
        }
    }
    walk(
        &crate::codegen::common::parse_type_annotation(annotation),
        out,
    );
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

/// The scoped pure-function dependency closure of a `verify ... law` — the
/// set of pure user functions a law's proof is allowed to mention, plus the
/// ADTs those functions range over.
///
/// Phase 0/2 of the lemma-discovery charter (`prompts/lemma-discovery.md`):
/// the various strategy detectors each re-derive the pure-fn closure ad hoc
/// (`law_helper_unfolds`, the spec-equivalence unfold list, etc.). This is
/// the single reusable computation they share, and the substrate the term
/// enumerator (`codegen::lemma_discovery`) builds on. `pure_fns` is the
/// enumeration *vocabulary*; `types` is the type-directed generator's
/// *signature alphabet* — the ADTs reachable from those fns' parameter and
/// return annotations, which the enumerator needs to mint typed variables.
///
/// `law_helper_unfolds` consumes only `pure_fns` (transitively reachable
/// helper source names, outer fn excluded, sorted for deterministic emit).
/// `types` lands now because its consumer — the enumerator — has landed.
/// Already-proven theorems and a content hash (the cache key) get added
/// when *their* consumers (the proof cache / replay) land — not before.
pub(crate) struct LawProofCone<'a> {
    /// Pure user fns transitively reachable from the law's lhs/rhs/when,
    /// in deterministic (sorted-by-name) order, excluding the law's own
    /// subject fn.
    pure_fns: Vec<&'a FnDef>,
    /// User-defined ADTs (`Sum`/`Product` type defs) transitively reachable
    /// from `pure_fns`' parameter and return type annotations, in
    /// deterministic (sorted-by-name) order. Builtin scalars (`Int`,
    /// `String`, …) and builtin records that don't resolve to a user
    /// `TypeDef` are dropped — only types the enumerator can construct or
    /// case-split on survive.
    types: Vec<&'a TypeDef>,
}

impl<'a> LawProofCone<'a> {
    /// Build the cone: seed from the law sides (lhs/rhs/when), keep only
    /// pure user fns (no effects, not `main`), transitively expand through
    /// their bodies, then drop the law's own subject (`outer_fn`) — each
    /// backend unfolds that one separately. Finally resolve the ADTs those
    /// fns range over from their signatures (`types`).
    pub(crate) fn compute(
        law: &crate::ast::VerifyLaw,
        outer_fn: &str,
        inputs: &ProofLowerInputs<'a>,
    ) -> Self {
        use std::collections::BTreeSet;

        let resolve_user_fn = |name: &str| -> Option<&'a FnDef> {
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

        // Resolve the (sorted) names back to defs — `names` only ever held
        // canonical `fd.name`s of pure user fns, so every entry re-resolves.
        let pure_fns: Vec<&'a FnDef> = names.iter().filter_map(|n| resolve_user_fn(n)).collect();

        // Type alphabet: ADTs reachable from the cone fns' signatures, then
        // transitively through those types' own field annotations (a record
        // field can name another ADT). Seed from every param + return type
        // string of every pure fn in the cone.
        let mut type_names: BTreeSet<String> = BTreeSet::new();
        for fd in &pure_fns {
            for (_, ann) in &fd.params {
                collect_named_types_in_annotation(ann, &mut type_names);
            }
            collect_named_types_in_annotation(&fd.return_type, &mut type_names);
        }
        loop {
            let before = type_names.len();
            let snapshot: Vec<String> = type_names.iter().cloned().collect();
            for name in snapshot {
                let Some(td) = inputs.find_type_def(&name) else {
                    continue;
                };
                match td {
                    crate::ast::TypeDef::Sum { variants, .. } => {
                        for v in variants {
                            for field_ty in &v.fields {
                                collect_named_types_in_annotation(field_ty, &mut type_names);
                            }
                        }
                    }
                    crate::ast::TypeDef::Product { fields, .. } => {
                        for (_, field_ty) in fields {
                            collect_named_types_in_annotation(field_ty, &mut type_names);
                        }
                    }
                }
            }
            if type_names.len() == before {
                break;
            }
        }
        // Keep only names that resolve to a user `TypeDef`; builtin scalars
        // (`Int`/`String`/…) and unregistered builtin records drop out here.
        let types: Vec<&'a TypeDef> = type_names
            .iter()
            .filter_map(|n| inputs.find_type_def(n))
            .collect();

        Self { pure_fns, types }
    }

    /// The cone's pure-fn names, deterministic (sorted) order, excluding the
    /// law's subject. Exactly the legacy `law_helper_unfolds` result.
    fn unfold_names(&self) -> Vec<String> {
        self.pure_fns.iter().map(|fd| fd.name.clone()).collect()
    }

    /// The cone's pure-fn vocabulary — the functions the term enumerator
    /// may apply. Deterministic (sorted-by-name) order, law subject excluded.
    pub(crate) fn pure_fns(&self) -> &[&'a FnDef] {
        &self.pure_fns
    }

    /// The cone's ADT type alphabet — the user types the enumerator can mint
    /// typed variables of or case-split on. Deterministic (sorted) order.
    pub(crate) fn types(&self) -> &[&'a TypeDef] {
        &self.types
    }
}

fn law_helper_unfolds(
    law: &crate::ast::VerifyLaw,
    outer_fn: &str,
    inputs: &ProofLowerInputs,
) -> Vec<String> {
    LawProofCone::compute(law, outer_fn, inputs).unfold_names()
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
/// Stage 8b of #232: detect `chain_qm(g) == chain_manual(g)` where
/// `chain_qm` is a `ModulePattern::ResultPipelineChain` (the `?`-chain
/// fn) and `chain_manual` is a manual `match Result.Err -> Err`
/// nested chain over the same step fns. Returns a payload carrying
/// both fn names + the ordered step list so backends can emit the
/// unfold list without re-walking the AST.
/// Stage 8c of #232: detect `fold(g) == spec(g)` where both `fold`
/// and `spec` are registered `ModulePattern::MatchDispatcherFold`
/// fns over a `List<T>` param. Both sides are list folds with
/// identical match structure; structural induction on `xs` plus
/// `omega` per arm closes the equivalence — emit lives on the
/// backend.
fn detect_match_dispatcher_fold_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::ProofStrategy> {
    use crate::analysis::shape::ModulePattern;
    use crate::ast::Expr;

    fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    let shape = inputs.program_shape?;

    if law.givens.len() != 1 {
        return None;
    }
    let given_name = &law.givens[0].name;

    // fn_name must be a MatchDispatcherFold in the shape.
    let fold_fn_pinned = shape.patterns.iter().any(|p| {
        matches!(
            p,
            ModulePattern::MatchDispatcherFold { fn_name: n, .. } if n == fn_name
        )
    });
    if !fold_fn_pinned {
        return None;
    }

    // Extract `fold(g)` and `spec(g)` from law sides.
    let extract = |expr: &Spanned<Expr>| -> Option<String> {
        let Expr::FnCall(callee, args) = &expr.node else {
            return None;
        };
        let name = ident_name(callee)?;
        if args.len() != 1 {
            return None;
        }
        if ident_name(&args[0])? != given_name {
            return None;
        }
        Some(name.to_string())
    };
    let lhs_call = extract(&law.lhs)?;
    let rhs_call = extract(&law.rhs)?;
    let (fold_fn, spec_fn) = if lhs_call == fn_name && rhs_call != fn_name {
        (lhs_call, rhs_call)
    } else if rhs_call == fn_name && lhs_call != fn_name {
        (rhs_call, lhs_call)
    } else {
        return None;
    };

    // The spec side must also be a MatchDispatcherFold — otherwise
    // the structural-induction template's `simp` step on the RHS
    // won't have a fold to unfold.
    let spec_pinned = shape.patterns.iter().any(|p| {
        matches!(
            p,
            ModulePattern::MatchDispatcherFold { fn_name: n, .. } if n == &spec_fn
        )
    });
    if !spec_pinned {
        return None;
    }

    Some(crate::ir::ProofStrategy::MatchDispatcherFold { fold_fn, spec_fn })
}

fn detect_result_pipeline_chain_equivalence(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::ProofStrategy> {
    use crate::analysis::shape::ModulePattern;
    use crate::ast::{Expr, Pattern, Stmt};

    fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    let shape = inputs.program_shape?;

    if law.givens.len() != 1 {
        return None;
    }
    let given_name = &law.givens[0].name;

    // Confirm fn_name is a ResultPipelineChain in the shape, and
    // pull the step fn list from there — post-pipeline AST has
    // already desugared `?` into nested matches, so the shape
    // (built from pre-resolver source items) is the authoritative
    // record of the original step list.
    let (chain_qm_fn, step_fns) = shape.patterns.iter().find_map(|p| match p {
        ModulePattern::ResultPipelineChain {
            fn_name: n,
            step_fns,
            ..
        } if n == fn_name => Some((n.clone(), step_fns.clone())),
        _ => None,
    })?;

    // Law shape: `chain_qm(g) == chain_manual(g)` (either side).
    let extract = |expr: &Spanned<Expr>| -> Option<String> {
        let Expr::FnCall(callee, args) = &expr.node else {
            return None;
        };
        let name = ident_name(callee)?;
        if args.len() != 1 {
            return None;
        }
        if ident_name(&args[0])? != given_name {
            return None;
        }
        Some(name.to_string())
    };
    let lhs_call = extract(&law.lhs);
    let rhs_call = extract(&law.rhs);
    let chain_manual_fn = match (lhs_call, rhs_call) {
        (Some(l), Some(r)) if l == chain_qm_fn && r != chain_qm_fn => r,
        (Some(l), Some(r)) if r == chain_qm_fn && l != chain_qm_fn => l,
        _ => return None,
    };

    if step_fns.len() < 2 {
        return None;
    }

    // Verify the manual fn is a nested `match Result.Err -> Err`
    // chain calling the same step fns. Heuristic: walk body, look
    // for `Match { subject: Call(step, _), arms: [Err -> Err, Ok -> ...] }`
    // pattern. Counts how many step calls show up.
    let manual_fd = inputs.find_fn_def_by_call_name(&chain_manual_fn)?;
    let mut manual_steps: Vec<String> = Vec::new();
    fn walk_manual<'a>(
        expr: &'a Spanned<Expr>,
        steps: &mut Vec<String>,
        ident_name: &dyn Fn(&'a Spanned<Expr>) -> Option<&'a str>,
    ) {
        if let Expr::Match { subject, arms } = &expr.node
            && let Expr::FnCall(callee, _) = &subject.node
            && let Some(n) = ident_name(subject).or_else(|| ident_name(callee))
        {
            let has_err_pass = arms.iter().any(|a| {
                let pat_is_err = matches!(
                    &a.pattern,
                    Pattern::Constructor(c, _) if c == "Result.Err" || c.ends_with(".Err")
                );
                // Body shape: either `Expr::Constructor("Result.Err", _)`
                // (pre-resolver) or `Expr::FnCall(Attr(Ident("Result"), "Err"), _)`
                // (post-resolver — `Result.Err(...)` is treated as a
                // method-attr call once names are wired up).
                let body_is_err = match &a.body.node {
                    Expr::Constructor(c, _) => c == "Result.Err" || c.ends_with(".Err"),
                    Expr::FnCall(callee, _) => matches!(
                        &callee.node,
                        Expr::Attr(base, attr)
                            if attr == "Err"
                                && matches!(&base.node, Expr::Ident(b) if b == "Result")
                    ),
                    _ => false,
                };
                pat_is_err && body_is_err
            });
            if has_err_pass {
                steps.push(n.to_string());
            }
            for a in arms {
                walk_manual(&a.body, steps, ident_name);
            }
        }
    }
    let manual_stmts = manual_fd.body.stmts();
    if manual_stmts.len() != 1 {
        return None;
    }
    let Stmt::Expr(manual_root) = &manual_stmts[0] else {
        return None;
    };
    walk_manual(manual_root, &mut manual_steps, &ident_name);
    if manual_steps.len() < 2 {
        return None;
    }

    Some(crate::ir::ProofStrategy::ResultPipelineChain {
        chain_qm_fn,
        chain_manual_fn,
        step_fns,
    })
}

/// Stage 8 of #232: detect `wrapper(g) == other(g)` where `wrapper`
/// is a `ModulePattern::WrapperOverRecursion` and the inner fn has a
/// monoidal-accumulator shape we know how to emit Dafny support
/// theorems for. Returns the typed strategy carrying enough payload
/// for the backend to emit the aux acc-decomposition lemma without
/// re-walking the AST.
fn detect_wrapper_over_recursion(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
) -> Option<crate::ir::ProofStrategy> {
    use crate::analysis::shape::ModulePattern;
    use crate::ast::{BinOp, Expr, Stmt};

    // Pre-pipeline AST has `Expr::Ident(n)`; post-pipeline the
    // resolver rewrites local/param idents to `Expr::Resolved { name }`.
    // Both forms identify the same surface name.
    fn ident_name(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) => Some(n.as_str()),
            Expr::Resolved { name, .. } => Some(name.as_str()),
            _ => None,
        }
    }

    let shape = inputs.program_shape?;

    // Only handle single-given laws — the template is parameterized
    // by one universal `xs`.
    if law.givens.len() != 1 {
        return None;
    }
    let given_name = &law.givens[0].name;

    // Read the loop fn + its fold step from the shared `AccumulatorFold` pattern
    // (recognized once by `analysis::shape`, consumed by both this strategy and
    // the `--discover` lemma chains) — no bespoke inner-body re-walk for the OP.
    // The monoidal strategy needs an additive/multiplicative inline step and an
    // identity finish (the nil arm returns the accumulator).
    let (inner_fn, combine_op) = shape.patterns.iter().find_map(|p| match p {
        ModulePattern::AccumulatorFold {
            wrapper_fn,
            loop_fn,
            step_op: Some(op),
            finish_fn: None,
            ..
        } if wrapper_fn == fn_name && matches!(op, BinOp::Add | BinOp::Mul) => {
            Some((loop_fn.clone(), *op))
        }
        _ => None,
    })?;
    let wrapper_fn = fn_name.to_string();

    // Law shape: `wrapper(g) == other(g)` (either side).
    let extract = |expr: &Spanned<crate::ast::Expr>| -> Option<String> {
        let Expr::FnCall(callee, args) = &expr.node else {
            return None;
        };
        let name = ident_name(callee)?;
        if args.len() != 1 {
            return None;
        }
        if ident_name(&args[0])? != given_name {
            return None;
        }
        Some(name.to_string())
    };
    let lhs_call = extract(&law.lhs);
    let rhs_call = extract(&law.rhs);
    let other_fn = match (lhs_call, rhs_call) {
        (Some(l), Some(r)) if l == wrapper_fn && r != wrapper_fn => r,
        (Some(l), Some(r)) if r == wrapper_fn && l != wrapper_fn => l,
        _ => return None,
    };

    // Wrapper body must be `inner(<given>, neutral)` where neutral is
    // 0 for Add and 1 for Mul. Verify against the fn def to be sure.
    let wrapper_fd = inputs.find_fn_def_by_call_name(&wrapper_fn)?;
    let wstmts = wrapper_fd.body.stmts();
    if wstmts.len() != 1 {
        return None;
    }
    let Stmt::Expr(wbody) = &wstmts[0] else {
        return None;
    };
    let (wcallee_name, wargs) = match &wbody.node {
        Expr::FnCall(c, a) => (ident_name(c)?, a.clone()),
        Expr::TailCall(td) => (td.target.as_str(), td.args.clone()),
        _ => return None,
    };
    if wcallee_name != inner_fn {
        return None;
    }
    if wargs.len() != 2 {
        return None;
    }
    let expected_neutral: i64 = match combine_op {
        BinOp::Add | BinOp::Sub => 0,
        BinOp::Mul => 1,
        _ => return None,
    };
    let neutral_matches = matches!(
        &wargs[1].node,
        Expr::Literal(crate::ast::Literal::Int(n)) if *n == expected_neutral
    );
    if !neutral_matches {
        return None;
    }

    Some(crate::ir::ProofStrategy::WrapperOverRecursion {
        wrapper_fn,
        inner_fn,
        other_fn,
        combine_op,
    })
}

fn detect_induction_target(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
) -> Option<String> {
    // Stage 7 of #232: read the eligibility set from `ProgramShape`
    // when threaded through `ProofLowerInputs`. The shape pass
    // computes the same direct-rec + not-indirect-rec predicate
    // once per compilation; the inline scan below is the
    // pre-shape fallback so callers without a shape (legacy test
    // fixtures, tools that build `ProofLowerInputs` by hand) keep
    // working unchanged.
    if let Some(shape) = inputs.program_shape {
        for given in &law.givens {
            if shape.inductable_sum_types.contains(&given.type_name) {
                return Some(given.name.clone());
            }
        }
        // Builtin `List<T>` given — structural induction via nil/cons (#409).
        // Reached only AFTER the other strategy detectors in
        // `populate_law_theorems` have declined this law, so a law with a
        // working bespoke strategy (e.g. quicksort's ordering/idempotent) keeps
        // it; only laws nobody else classified fall through to list induction.
        if let Some(given) = list_induction_given(law) {
            return Some(given);
        }
        return None;
    }
    detect_induction_target_legacy(law, inputs)
}

/// A `given` whose declared type is a builtin `List<T>` — the structural
/// induction target name, if any.
fn list_induction_given(law: &crate::ast::VerifyLaw) -> Option<String> {
    law.givens
        .iter()
        .find(|g| g.type_name.trim().starts_with("List<"))
        .map(|g| g.name.clone())
}

fn detect_induction_target_legacy(
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
        if has_indirect_rec_variants(variants, type_name) {
            continue;
        }
        return Some(given.name.clone());
    }
    list_induction_given(law)
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
    type_id: crate::ir::TypeId,
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
            TopLevel::FnDef(fd)
                if smart_ctor_matches(fd, type_id, type_name, inputs.symbol_table, scope) =>
            {
                Some(fd.name.clone())
            }
            _ => None,
        }),
        Some(prefix) => inputs
            .dep_modules
            .iter()
            .find(|m| m.prefix == prefix)
            .and_then(|m| {
                m.fn_defs
                    .iter()
                    .find(|fd| {
                        smart_ctor_matches(fd, type_id, type_name, inputs.symbol_table, scope)
                    })
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

/// Does `fd` look like a smart constructor for `type_id` (1-param
/// fn whose return type is `Result<T, _>` where `T` is the
/// refined nominal)?
///
/// Epic #180 Phase 6 follow-up — id-aware comparison. The raw
/// `name`-equality path used to silently accept a smart ctor
/// from module B whose return type was `Result<A.Natural, _>`
/// when looking for module B's own `Natural`, because the
/// inner Named's bare `name` field was just `"Natural"` and
/// both shapes match on string. Resolving the inner Named
/// through the symbol table within the current scope hands us
/// the actual `TypeId` and lets us compare opaque identities.
///
/// `type_name` stays in the signature as the fallback for
/// builtins / unresolved refs the symbol table doesn't index
/// (matches the [`crate::codegen::common::backend_named_type_key`]
/// fallback semantics).
fn smart_ctor_matches(
    fd: &FnDef,
    type_id: crate::ir::TypeId,
    type_name: &str,
    symbols: &crate::ir::SymbolTable,
    scope: Option<&str>,
) -> bool {
    if fd.params.len() != 1 {
        return false;
    }
    let parsed = crate::types::parse_type_str(&fd.return_type);
    let crate::types::Type::Result(ok, _) = parsed else {
        return false;
    };
    // syntax-discovery-only: pulls the inner Named's `name` out
    // of the raw `parse_type_str` parse to feed into the
    // symbol-table resolution below. The identity decision is
    // the `TypeId` comparison built from that name plus scope;
    // the bare `name` is just an intermediate handle.
    let crate::types::Type::Named { name: n, .. } = &*ok else {
        return false;
    };
    // Prefer id-direct: resolve the inner Named's name against
    // the symbol table within the current scope, then compare
    // `TypeId`s. Identity-safe across cross-module same-bare-name
    // twins by construction.
    let name_is_qualified = n.contains('.');
    let resolved_id = if name_is_qualified {
        n.rsplit_once('.').and_then(|(prefix, bare)| {
            symbols.type_id_of(&crate::ir::TypeKey::in_module(prefix.to_string(), bare))
        })
    } else if let Some(prefix) = scope {
        symbols
            .type_id_of(&crate::ir::TypeKey::in_module(
                prefix.to_string(),
                n.clone(),
            ))
            .or_else(|| symbols.type_id_of(&crate::ir::TypeKey::entry(n.clone())))
    } else {
        symbols.type_id_of(&crate::ir::TypeKey::entry(n.clone()))
    };
    match resolved_id {
        Some(id) => id == type_id,
        None => n == type_name,
    }
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
