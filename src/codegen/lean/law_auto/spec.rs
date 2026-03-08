use crate::ast::{BinOp, Expr, FnDef, MatchArm, Pattern, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;
use crate::verify_law::canonical_spec_ref;

use super::super::expr::{aver_name_to_lean, emit_expr};
use super::super::types::type_annotation_to_lean;
use super::AutoProof;
use super::intro_then;
use super::shared::{
    body_terminal_expr, callee_matches_name, find_fn_def, law_simp_defs, matches_ident,
    matches_int_lit,
};

#[derive(Clone, Copy)]
struct FibonacciImplShape<'a> {
    helper_name: &'a str,
}

fn fib_nat_helper_name(fn_name: &str) -> String {
    format!("{}__nat", aver_name_to_lean(fn_name))
}

fn matches_bool_arm(arm: &MatchArm, value: bool) -> bool {
    matches!(&arm.pattern, Pattern::Literal(crate::ast::Literal::Bool(v)) if *v == value)
}

fn matches_int_arm(arm: &MatchArm, value: i64) -> bool {
    matches!(&arm.pattern, Pattern::Literal(crate::ast::Literal::Int(v)) if *v == value)
}

fn matches_self_sub_call(expr: &Expr, fn_name: &str, param_name: &str, amount: i64) -> bool {
    let Expr::FnCall(callee, args) = expr else {
        return false;
    };
    if !callee_matches_name(callee, fn_name) || args.len() != 1 {
        return false;
    }
    matches!(
        &args[0],
        Expr::BinOp(BinOp::Sub, left, right)
            if matches_ident(left, param_name) && matches_int_lit(right, amount)
    )
}

fn detects_fibonacci_spec(fd: &FnDef) -> bool {
    let [(param_name, param_type)] = fd.params.as_slice() else {
        return false;
    };
    if param_type != "Int" {
        return false;
    }
    let Some(expr) = body_terminal_expr(fd.body.as_ref()) else {
        return false;
    };
    let Expr::Match { subject, arms, .. } = expr else {
        return false;
    };
    let Expr::BinOp(BinOp::Lt, left, right) = subject.as_ref() else {
        return false;
    };
    if !matches_ident(left, param_name)
        || !matches_int_lit(right, 0)
        || arms.len() != 2
        || !matches_bool_arm(&arms[0], true)
        || !matches_bool_arm(&arms[1], false)
        || !matches_int_lit(&arms[0].body, 0)
    {
        return false;
    }
    let Expr::Match {
        subject: inner_subject,
        arms: inner_arms,
        ..
    } = arms[1].body.as_ref()
    else {
        return false;
    };
    if !matches_ident(inner_subject, param_name)
        || inner_arms.len() != 3
        || !matches_int_arm(&inner_arms[0], 0)
        || !matches_int_lit(&inner_arms[0].body, 0)
        || !matches_int_arm(&inner_arms[1], 1)
        || !matches_int_lit(&inner_arms[1].body, 1)
        || !matches!(inner_arms[2].pattern, Pattern::Wildcard)
    {
        return false;
    }
    matches!(
        inner_arms[2].body.as_ref(),
        Expr::BinOp(BinOp::Add, left, right)
            if matches_self_sub_call(left, &fd.name, param_name, 1)
                && matches_self_sub_call(right, &fd.name, param_name, 2)
    )
}

fn detect_fibonacci_impl(fd: &FnDef) -> Option<FibonacciImplShape<'_>> {
    let [(param_name, param_type)] = fd.params.as_slice() else {
        return None;
    };
    if param_type != "Int" {
        return None;
    }
    let expr = body_terminal_expr(fd.body.as_ref())?;
    let Expr::Match { subject, arms, .. } = expr else {
        return None;
    };
    let Expr::BinOp(BinOp::Lt, left, right) = subject.as_ref() else {
        return None;
    };
    if !matches_ident(left, param_name)
        || !matches_int_lit(right, 0)
        || arms.len() != 2
        || !matches_bool_arm(&arms[0], true)
        || !matches_bool_arm(&arms[1], false)
        || !matches_int_lit(&arms[0].body, 0)
    {
        return None;
    }
    let Expr::FnCall(callee, args) = arms[1].body.as_ref() else {
        return None;
    };
    if args.len() != 3
        || !matches_ident(&args[0], param_name)
        || !matches_int_lit(&args[1], 0)
        || !matches_int_lit(&args[2], 1)
    {
        return None;
    }
    let Expr::Ident(helper_name) = callee.as_ref() else {
        return None;
    };
    Some(FibonacciImplShape { helper_name })
}

fn detects_fibonacci_worker(fd: &FnDef) -> bool {
    let [(n_name, n_type), (a_name, a_type), (b_name, b_type)] = fd.params.as_slice() else {
        return false;
    };
    if n_type != "Int" || a_type != "Int" || b_type != "Int" {
        return false;
    }
    let Some(expr) = body_terminal_expr(fd.body.as_ref()) else {
        return false;
    };
    let Expr::Match { subject, arms, .. } = expr else {
        return false;
    };
    if !matches_ident(subject, n_name)
        || arms.len() != 2
        || !matches_int_arm(&arms[0], 0)
        || !matches_ident(&arms[0].body, a_name)
        || !matches!(arms[1].pattern, Pattern::Wildcard)
    {
        return false;
    }
    let args: &[Expr] = match arms[1].body.as_ref() {
        Expr::FnCall(callee, args) if callee_matches_name(callee, &fd.name) => args.as_slice(),
        Expr::TailCall(boxed) if boxed.0 == fd.name => boxed.1.as_slice(),
        _ => return false,
    };
    if args.len() != 3 {
        return false;
    }
    matches!(
        (&args[0], &args[1], &args[2]),
        (
            Expr::BinOp(BinOp::Sub, left0, right0),
            Expr::Ident(next_a),
            Expr::BinOp(BinOp::Add, left2, right2)
        ) if matches_ident(left0, n_name)
            && matches_int_lit(right0, 1)
            && next_a == b_name
            && matches_ident(left2, a_name)
            && matches_ident(right2, b_name)
    )
}

fn emit_fibonacci_spec_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    let [intro_name] = intro_names else {
        return None;
    };
    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs)?;
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    let spec_fd = find_fn_def(ctx, &spec_ref.spec_fn_name)?;
    let impl_shape = detect_fibonacci_impl(impl_fd)?;
    if !detects_fibonacci_spec(spec_fd) {
        return None;
    }
    let helper_fd = find_fn_def(ctx, impl_shape.helper_name)?;
    if !detects_fibonacci_worker(helper_fd) {
        return None;
    }
    let theorem_base = format!(
        "{}_eq_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&spec_ref.spec_fn_name)
    );
    let advance_name = format!("{}__advanceNat", theorem_base);
    let step_name = format!("{}__helper_step_nat", theorem_base);
    let state_name = format!("{}__helper_state", theorem_base);
    let pair_name = format!("{}__advance_eq_spec_pair", theorem_base);
    let impl_name = aver_name_to_lean(&vb.fn_name);
    let helper_name = aver_name_to_lean(impl_shape.helper_name);
    let helper_fuel_name = format!("{}__fuel", helper_name);
    let spec_name = aver_name_to_lean(&spec_ref.spec_fn_name);
    let spec_nat_name = fib_nat_helper_name(&spec_ref.spec_fn_name);
    let ret_type = type_annotation_to_lean(&impl_fd.return_type);

    let support_lines = vec![
        format!(
            "private def {} : Nat -> {} -> {} -> ({} × {})",
            advance_name, ret_type, ret_type, ret_type, ret_type
        ),
        format!("  | 0, a, b => (a, b)"),
        format!("  | Nat.succ m, a, b => {} m b (a + b)", advance_name),
        String::new(),
        format!(
            "private theorem {} : ∀ (m : Nat) (a b : {}), {} (Int.ofNat (m + 1)) a b = {} (Int.ofNat m) b (a + b) := by",
            step_name, ret_type, helper_name, helper_name
        ),
        "  intro m a b".to_string(),
        format!("  rw [{}, {}]", helper_name, helper_name),
        format!("  simp [Int.ofNat_succ, {}]", helper_fuel_name),
        "  split".to_string(),
        "  · rename_i h".to_string(),
        "    exfalso".to_string(),
        "    exact (Int.ofNat_ne_zero.mpr (Nat.succ_ne_zero m)) h".to_string(),
        "  · rfl".to_string(),
        String::new(),
        format!(
            "private theorem {} : ∀ (m : Nat) (a b : {}), {} (Int.ofNat m) a b = ({} m a b).1 := by",
            state_name, ret_type, helper_name, advance_name
        ),
        "  intro m".to_string(),
        "  induction m with".to_string(),
        "  | zero =>".to_string(),
        "      intro a b".to_string(),
        format!(
            "      simp [{}, {}, {}]",
            helper_name, helper_fuel_name, advance_name
        ),
        "  | succ m ih =>".to_string(),
        "      intro a b".to_string(),
        format!("      rw [{}]", step_name),
        format!("      simpa [{}] using ih b (a + b)", advance_name),
        String::new(),
        format!(
            "private theorem {} : ∀ (m i : Nat), {} m ({} i) ({} (i + 1)) = ({} (i + m), {} (i + m + 1)) := by",
            pair_name, advance_name, spec_nat_name, spec_nat_name, spec_nat_name, spec_nat_name
        ),
        "  intro m".to_string(),
        "  induction m with".to_string(),
        "  | zero =>".to_string(),
        "      intro i".to_string(),
        format!("      simp [{}]", advance_name),
        "  | succ m ih =>".to_string(),
        "      intro i".to_string(),
        format!(
            "      simpa [{}, Nat.add_assoc, Nat.add_left_comm, Nat.add_comm, {}, Int.add_comm] using ih (i + 1)",
            advance_name, spec_nat_name
        ),
        String::new(),
    ];

    let proof_lines = intro_then(
        intro_names,
        vec![
            format!("by_cases hneg : {} < 0", intro_name),
            format!("· simp [{}, {}, hneg]", impl_name, spec_name),
            format!(
                "· have hnonneg : 0 ≤ {} := (Int.not_lt).mp hneg",
                intro_name
            ),
            format!("  simp [{}, {}, hneg]", impl_name, spec_name),
            format!("  rw [← Int.toNat_of_nonneg hnonneg]"),
            "  calc".to_string(),
            format!(
                "    {} (Int.ofNat {}.toNat) 0 1 = ({} {}.toNat 0 1).1 := {} {}.toNat 0 1",
                helper_name, intro_name, advance_name, intro_name, state_name, intro_name
            ),
            format!(
                "    _ = ({} (0 + {}.toNat), {} (0 + {}.toNat + 1)).1 := by",
                spec_nat_name, intro_name, spec_nat_name, intro_name
            ),
            format!(
                "      simpa [{}] using congrArg Prod.fst ({} {}.toNat 0)",
                spec_nat_name, pair_name, intro_name
            ),
            format!("    _ = {} {}.toNat := by simp", spec_nat_name, intro_name),
        ],
    );

    Some(AutoProof {
        support_lines,
        proof_lines,
        replaces_theorem: false,
    })
}

pub(super) fn emit_spec_function_equivalence_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    if let Some(auto_proof) = emit_fibonacci_spec_equivalence_law(vb, law, ctx, intro_names) {
        return Some(auto_proof);
    }

    let spec_ref = canonical_spec_ref(&vb.fn_name, law, &ctx.fn_sigs)?;
    let impl_fd = find_fn_def(ctx, &vb.fn_name)?;
    let spec_fd = find_fn_def(ctx, &spec_ref.spec_fn_name)?;
    let impl_body = body_terminal_expr(impl_fd.body.as_ref())?;
    let spec_body = body_terminal_expr(spec_fd.body.as_ref())?;
    if emit_expr(impl_body, ctx) != emit_expr(spec_body, ctx) {
        return None;
    }

    let try_side = |impl_side: &Expr, spec_side: &Expr| -> Option<AutoProof> {
        let Expr::FnCall(impl_callee, impl_args) = impl_side else {
            return None;
        };
        let Expr::FnCall(spec_callee, spec_args) = spec_side else {
            return None;
        };
        if !callee_matches_name(impl_callee, &vb.fn_name)
            || !callee_matches_name(spec_callee, &spec_ref.spec_fn_name)
            || impl_args != spec_args
        {
            return None;
        }

        let simp_defs = law_simp_defs(ctx, vb, law).into_iter().collect::<Vec<_>>();
        Some(AutoProof {
            support_lines: Vec::new(),
            proof_lines: intro_then(
                intro_names,
                vec![format!("simpa [{}]", simp_defs.join(", "))],
            ),
            replaces_theorem: false,
        })
    };

    try_side(&law.lhs, &law.rhs).or_else(|| try_side(&law.rhs, &law.lhs))
}
