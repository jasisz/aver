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

/// Walk every type definition in the inputs (entry items + dependent
/// modules), classify the refinement-via-opaque ones, and build a
/// `ProofIR` carrying their decided shapes. Pure function — never
/// reads side effects, never mutates inputs.
pub fn lower(inputs: &ProofLowerInputs) -> ProofIR {
    let mut ir = ProofIR::default();
    populate_refined_types(inputs, &mut ir);
    populate_fn_contracts(inputs, &mut ir);
    ir
}

fn populate_refined_types(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
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
fn populate_fn_contracts(inputs: &ProofLowerInputs, ir: &mut ProofIR) {
    let (plans, _issues) = analyze_plans(inputs);
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
        let Some(fd) = all_fns.iter().find(|fd| fd.name == *fn_name) else {
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
