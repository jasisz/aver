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

mod finite_domain;
mod induction;
mod int_decimal_roundtrip;
mod map_laws;
mod refinement;
mod simp;
mod spec_equivalence;
mod wrapper_laws;

pub(crate) use induction::LawProofCone;

use finite_domain::*;
use induction::*;
use int_decimal_roundtrip::*;
use map_laws::*;
use refinement::*;
use simp::*;
use spec_equivalence::*;
use wrapper_laws::*;
