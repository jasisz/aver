//! Build `ProofIR` from a `CodegenContext`.
//!
//! The lowering producer side of the Step 1 / Step 2 split: types
//! live in `src/ir/proof_ir.rs`, the function that fills them in
//! from a typechecked + analysed codegen context lives here. Output
//! lands in `CodegenContext.proof_ir`; both proof backends read
//! from the same field, so any classifier-side decision flows
//! consistently to Lean and Dafny without each backend re-running
//! shape detection.
//!
//! **Step 2 scope**: this commit only populates
//! `ProofIR.refined_types` — refinement-via-opaque records lifted
//! to subtype on Lean / subset type on Dafny. `fn_contracts` and
//! `law_theorems` are intentionally left empty; backends still go
//! through the legacy `codegen::recursion::RecursionPlan` and the
//! ad-hoc law-lowering path in `lean::toplevel`. Step 3 onwards
//! migrates backends to read from ProofIR one feature at a time.
//!
//! A diff test (`tests/proof_ir_diff.rs`) asserts the new producer
//! agrees with the legacy `refinement_info_for` + Dafny
//! `refinement_witness_for` walk on every flagship refinement
//! example — once the test is stable the legacy walkers become dead
//! code in their consumers (Step 3 / Step 4 deletes them).

use std::collections::HashSet;

use crate::ast::{Expr, FnDef, Literal, Spanned, TopLevel, TypeDef};
use crate::codegen::common::{expr_to_dotted_name, refinement_info_for};
use crate::codegen::recursion::{RecursionPlan, analyze_plans};
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
    /// Recursive fn names from the `analyze` pipeline stage. Used by
    /// `analyze_plans` to short-circuit non-recursive fns.
    pub recursive_fns: &'a HashSet<String>,
}

impl<'a> ProofLowerInputs<'a> {
    /// Build a view from a fully-assembled `CodegenContext`. Used by
    /// `refresh_facts` (test helper) and by the migration-window
    /// `build_context` path before Step 7e moves lowering into the
    /// pipeline. Reads only the fields the lowerer actually needs.
    pub fn from_ctx(ctx: &'a CodegenContext) -> Self {
        Self {
            entry_items: &ctx.items,
            dep_modules: &ctx.modules,
            module_prefixes: &ctx.module_prefixes,
            recursive_fns: &ctx.recursive_fns,
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

    /// Recursive pure fn names. Filters `recursive_fns` (populated by
    /// the analyze pipeline stage) by pure-ness.
    pub fn recursive_pure_fn_names(&self) -> HashSet<String> {
        let pure_names: HashSet<String> = self
            .pure_fns()
            .into_iter()
            .map(|fd| fd.name.clone())
            .collect();
        self.recursive_fns
            .iter()
            .filter(|name| pure_names.contains(name.as_str()))
            .cloned()
            .collect()
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

/// Run both proof-export lowerings in one shot — convenience for
/// callers that want a fully-populated ProofIR. The pipeline uses
/// `populate_refined_types` and `populate_fn_contracts` directly
/// because the two are independent stages there (Step 7h split).
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
    // Walk entry items first, then dep modules. Both feed into the
    // same map keyed by bare type name — consumers (Lean / Dafny
    // emit paths) always query by bare name because that's what
    // they see in the AST nodes (`Expr::RecordCreate { type_name:
    // "Natural", ... }`, `TypeDef::Product { name: "Natural", ... }`).
    // Aver's module DAG invariant + typechecker's duplicate-type
    // rejection (PR #89) make name collisions a compile error, so
    // bare-name keying is safe.
    let entry_typedefs = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some(td),
        _ => None,
    });
    let module_typedefs = inputs.dep_modules.iter().flat_map(|m| m.type_defs.iter());

    for td in entry_typedefs.chain(module_typedefs) {
        let TypeDef::Product { name, fields, .. } = td else {
            continue;
        };
        if fields.len() != 1 {
            continue;
        }
        if ir.refined_types.contains_key(name) {
            // Already classified via another path (typically the
            // entry walk picked it up first); skip the dep-module
            // duplicate so we don't overwrite a verified-witness
            // entry with a predicate-eval fallback witness.
            continue;
        }
        let Some(info) = refinement_info_for(name, inputs) else {
            continue;
        };
        let invariant = Predicate {
            free_vars: vec![(
                info.param_name.to_string(),
                crate::ir::proof_ir::QuantifierType::Plain(info.carrier_type.to_string()),
            )],
            expr: info.predicate.clone(),
        };
        let witness = pick_witness(name, inputs, info.predicate, info.param_name);
        ir.refined_types.insert(
            name.clone(),
            RefinedTypeDecl {
                name: name.clone(),
                carrier_type: info.carrier_type.to_string(),
                carrier_field: info.carrier_field.to_string(),
                predicate_param: info.param_name.to_string(),
                invariant,
                witness,
            },
        );
    }
}

/// Walk `analyze_plans(ctx)` and populate `ProofIR.fn_contracts`.
///
/// **Step 5 scope**: only `IntCountdownGuarded` plans translate to
/// a `FnContract`. It's the most proof-heavy variant — caller-
/// derived precondition + preservation + decrease — so it sets the
/// pattern for the rest. All other `RecursionPlan` variants (Fuel-
/// emitted shapes, `LinearRecurrence2`, `Mutual*`) are skipped here
/// and continue going through the legacy `RecursionPlan` path
/// directly on the consumer side. Subsequent Steps add one variant
/// per commit, with their own diff tests.
///
/// The lowering is intentionally a **translation pass** over the
/// existing classifier output, not a re-implementation: backends
/// keep reading `RecursionPlan` during the migration window, the
/// diff test (`tests/proof_ir_diff.rs`) asserts both sides agree on
/// the fibTR flagship, and once every variant is covered we delete
/// the consumer-side `RecursionPlan` reads in a later Step.
pub fn populate_fn_contracts(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let (plans, issues) = analyze_plans(inputs);
    ir.unclassified_fns
        .extend(issues.into_iter().map(|issue| crate::ir::UnclassifiedFn {
            line: issue.line,
            message: issue.message,
        }));
    let all_fns: Vec<&FnDef> = inputs
        .dep_modules
        .iter()
        .flat_map(|m| m.fn_defs.iter())
        .chain(inputs.entry_items.iter().filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        }))
        .collect();

    for (fn_name, plan) in &plans {
        let Some(fd) = all_fns.iter().find(|fd| fd.name == *fn_name) else {
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
                    fn_name.clone(),
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
                    fn_name.clone(),
                    FnContract {
                        source_name: fn_name.clone(),
                        recursion: Some(RecursionContract::Fuel {
                            fuel_metric: crate::ir::FuelMetric::BoundMinusParamNatAbsPlusOne {
                                param: param_name.clone(),
                                bound: bound.clone(),
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
                    fn_name.clone(),
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
                fn_name.clone(),
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
                    fn_name.clone(),
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
                    fn_name.clone(),
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
                    fn_name.clone(),
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
                    fn_name.clone(),
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
                    fn_name.clone(),
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
                expr: clause.clone(),
            })
            .collect();

        ir.fn_contracts.insert(
            fn_name.clone(),
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
                        base_arm_body: base_arm_body.clone(),
                        wildcard_arm_body: wildcard_arm_body.clone(),
                    },
                }),
            },
        );
    }
}

/// Walk every verify block, lift `VerifyKind::Law` entries into
/// `ProofIR.law_theorems`.
///
/// **Step 23 scope**: extract the law's shape — quantifiers from
/// `givens`, premises from `when` (when non-empty), the claim from
/// `lhs == rhs`. Strategy stays `ProofStrategy::BackendDispatch`; the
/// backend's existing ad-hoc chain (rfl / induction / arithmetic
/// wrapper / spec equiv / map laws / simp+omega / guarded domain)
/// still decides which proof tactic emits. Subsequent Steps move
/// concrete strategy decisions into the lowerer, one shape at a time.
pub fn populate_law_theorems(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    use crate::ast::{TopLevel, VerifyKind};
    use crate::ir::{LawTheorem, Predicate, Quantifier, QuantifierType};

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

        let premises: Vec<Predicate> = match &law.when {
            Some(when_expr) => vec![Predicate {
                free_vars: quantifiers
                    .iter()
                    .map(|q| (q.name.clone(), q.binder_type.clone()))
                    .collect(),
                expr: when_expr.clone(),
            }],
            None => Vec::new(),
        };

        let strategy = classify_law_strategy(law, &vb.fn_name, inputs, &ir.refined_types);

        ir.law_theorems.push(LawTheorem {
            fn_name: vb.fn_name.clone(),
            law_name: law.name.clone(),
            quantifiers,
            premises,
            claim_lhs: law.lhs.clone(),
            claim_rhs: law.rhs.clone(),
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
    refined_types: &std::collections::HashMap<String, crate::ir::RefinedTypeDecl>,
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
        return ProofStrategy::LibraryAxiom { axiom, args };
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
    refined_types: &std::collections::HashMap<String, crate::ir::RefinedTypeDecl>,
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
    let lifted = law.givens.iter().any(|g| {
        refinement_lift_for_given_ir(&g.name, &law.lhs, &law.rhs, refined_types).is_some()
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
    refined_types: &std::collections::HashMap<String, crate::ir::RefinedTypeDecl>,
) -> Option<String> {
    let mut result: Option<String> = None;
    walk_for_refinement_carrier(lhs, given_name, refined_types, &mut result);
    walk_for_refinement_carrier(rhs, given_name, refined_types, &mut result);
    result
}

fn walk_for_refinement_carrier(
    expr: &Spanned<crate::ast::Expr>,
    given_name: &str,
    refined_types: &std::collections::HashMap<String, crate::ir::RefinedTypeDecl>,
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
            if matches_var && let Some(decl) = refined_types.get(type_name) {
                *result = Some(decl.name.clone());
                return;
            }
            // Even non-matching RecordCreate may contain nested
            // refinement carriers (e.g. `Foo(value = Bar(value = a))`).
            for (_, v) in fields {
                walk_for_refinement_carrier(v, given_name, refined_types, result);
            }
        }
        Expr::FnCall(callee, args) => {
            walk_for_refinement_carrier(callee, given_name, refined_types, result);
            for a in args {
                walk_for_refinement_carrier(a, given_name, refined_types, result);
            }
        }
        Expr::BinOp(_, l, r) => {
            walk_for_refinement_carrier(l, given_name, refined_types, result);
            walk_for_refinement_carrier(r, given_name, refined_types, result);
        }
        Expr::Match { subject, arms, .. } => {
            walk_for_refinement_carrier(subject, given_name, refined_types, result);
            for arm in arms {
                walk_for_refinement_carrier(&arm.body, given_name, refined_types, result);
            }
        }
        Expr::Attr(obj, _) => {
            walk_for_refinement_carrier(obj, given_name, refined_types, result);
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
        return Some(crate::ir::SmartGuard {
            param: param_name.clone(),
            predicate: (**subject).clone(),
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
        if &some_v.node != &v.node {
            return None;
        }
        Some((
            "Map.get_set_self".to_string(),
            vec![m.clone(), k.clone(), v.clone()],
        ))
    };
    get_side(&law.lhs, &law.rhs).or_else(|| get_side(&law.rhs, &law.lhs))
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
/// `fn f(p1: Int, p2: Int) -> Int :- p1 <op> p2`. The op family is
/// restricted to those with commutative/associative lemmas
/// (`Add`, `Mul`); other binary wrappers (e.g. `Sub`) lower through
/// the backend chain (Step 26+ pins sub anti-commutative).
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

fn matches_ident_expr(expr: &Spanned<crate::ast::Expr>, name: &str) -> bool {
    use crate::ast::Expr;
    matches!(&expr.node, Expr::Ident(n) | Expr::Resolved { name: n, .. } if n == name)
}

fn callee_matches_name(expr: &Spanned<crate::ast::Expr>, target: &str) -> bool {
    let Some(name) = expr_to_dotted_name(&expr.node) else {
        return false;
    };
    name == target || name.rsplit('.').next() == Some(target)
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
) -> Option<String> {
    let smart_ctor_name = inputs.entry_items.iter().find_map(|item| match item {
        TopLevel::FnDef(fd)
            if fd.return_type.starts_with("Result<")
                && fd.return_type[7..].starts_with(type_name)
                && fd.params.len() == 1 =>
        {
            Some(fd.name.clone())
        }
        _ => None,
    });
    if let Some(smart_ctor_name) = smart_ctor_name {
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
    for candidate in [0i64, 1, -1] {
        if eval_int_bool_predicate(predicate, param_name, candidate) == Some(true) {
            return Some(candidate.to_string());
        }
    }
    None
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
