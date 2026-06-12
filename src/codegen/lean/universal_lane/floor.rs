//! FAMILY 3 — coarse-floor stability: a `when`-law stating that the
//! floored quotient is stable when the numerator moves inside one open
//! cell of a finer grid. Shape (over a 2-Int-param floor wrapper `f`
//! built on `Int.div` with a 0 default for the zero divisor):
//!
//! ```text
//! when  b > 0  ∧  d > 0  ∧  w*b < a  ∧  a < (w+1)*b
//! f(a, b*d) == f(w, d)  =>  true
//! ```
//!
//! The lane proof instantiates a hand-validated arithmetic core (proved
//! kernel-clean over core Lean v4.15: `Int.ediv_add_emod` bounds on
//! `w/d` lifted through `Int.le_ediv_iff_mul_le`, nonlinear products
//! aligned by `calc`/`rw` so `omega` only ever sees identical opaque
//! atoms), then discharges the `Int.div` Result wrapper with the
//! divisor-nonzero facts. Anything off this exact shape declines at
//! zero cost. See the module doc in `mod.rs` for the lane's crediting
//! and iron-guard model.

use super::super::expr::{aver_name_to_lean, emit_expr_legacy};
use crate::ast::{BinOp, Expr, Literal, Pattern, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::shared::{call_of, ident_of, when_conjuncts};
use super::{LaneLawFile, lane_module_id};

/// Recognized law, with the given names mapped to their roles and the
/// premise conjuncts located (any order within a right-nested spine).
pub(super) struct FloorPlan {
    /// Role-ordered given names: numerator, fine step, cell index,
    /// coarsening factor — the `(a, b, w, d)` of the statement.
    roles: [String; 4],
    /// Premise conjunct index per role premise, in the order the core
    /// lemma consumes them: `b > 0`, `d > 0`, `w*b < a`, `a < (w+1)*b`.
    hyps: [usize; 4],
}

/// Proof-local binder names of the rendered twin; a colliding `given`
/// would be shadowed mid-proof, so such laws decline.
const FLOOR_RESERVED: &[&str] = &["h_when", "h0", "h1", "h2", "h3", "key", "hbd", "hd0"];

fn is_int_lit(e: &Spanned<Expr>, v: i64) -> bool {
    matches!(&e.node, Expr::Literal(Literal::Int(n)) if *n == v)
}

/// `name > 0`.
fn is_gt_zero(e: &Spanned<Expr>, name: &str) -> bool {
    matches!(&e.node, Expr::BinOp(BinOp::Gt, l, r)
        if ident_of(l) == Some(name) && is_int_lit(r, 0))
}

/// `w * b < a`.
fn is_cell_low(e: &Spanned<Expr>, w: &str, b: &str, a: &str) -> bool {
    let Expr::BinOp(BinOp::Lt, l, r) = &e.node else {
        return false;
    };
    if ident_of(r) != Some(a) {
        return false;
    }
    matches!(&l.node, Expr::BinOp(BinOp::Mul, lw, lb)
        if ident_of(lw) == Some(w) && ident_of(lb) == Some(b))
}

/// `a < (w + 1) * b`.
fn is_cell_high(e: &Spanned<Expr>, a: &str, w: &str, b: &str) -> bool {
    let Expr::BinOp(BinOp::Lt, l, r) = &e.node else {
        return false;
    };
    if ident_of(l) != Some(a) {
        return false;
    }
    let Expr::BinOp(BinOp::Mul, rl, rb) = &r.node else {
        return false;
    };
    if ident_of(rb) != Some(b) {
        return false;
    }
    matches!(&rl.node, Expr::BinOp(BinOp::Add, aw, one)
        if ident_of(aw) == Some(w) && is_int_lit(one, 1))
}

/// The subject must be the floor wrapper: two Int params -> Int, pure,
/// body = `match Int.div(p0, p1) { Ok(q) -> q; Err(_) -> 0 }`.
fn is_floor_div_wrapper(vb: &VerifyBlock, ctx: &CodegenContext) -> bool {
    let Some(fd) = ctx.fn_def_by_name(&vb.fn_name, None) else {
        return false;
    };
    if !fd.effects.is_empty()
        || fd.params.len() != 2
        || fd.params.iter().any(|(_, ty)| ty != "Int")
        || fd.return_type != "Int"
    {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    let Some((callee, args)) = call_of(subject) else {
        return false;
    };
    if callee != "Int.div"
        || args.len() != 2
        || ident_of(&args[0]) != Some(fd.params[0].0.as_str())
        || ident_of(&args[1]) != Some(fd.params[1].0.as_str())
        || arms.len() != 2
    {
        return false;
    }
    let ok_arm = arms.iter().any(|arm| {
        matches!(&arm.pattern, Pattern::Constructor(n, binders)
            if n == "Result.Ok"
                && binders.len() == 1
                && ident_of(&arm.body) == Some(binders[0].as_str()))
    });
    let err_arm = arms.iter().any(|arm| {
        matches!(&arm.pattern, Pattern::Constructor(n, binders)
            if n == "Result.Err" && binders.len() == 1 && is_int_lit(&arm.body, 0))
    });
    ok_arm && err_arm
}

/// Validate one when-law against the coarse-floor stability shape.
/// Any deviation declines — the law simply stays bounded, the manifest
/// bytes untouched.
pub(super) fn classify_floor_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<FloorPlan> {
    let when = law.when.as_ref()?;
    if law.givens.len() != 4 || law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    // Claim: `f(a, b*d) == f(w, d)` over the subject fn.
    let Expr::BinOp(BinOp::Eq, claim_l, claim_r) = &law.lhs.node else {
        return None;
    };
    let (lc, la) = call_of(claim_l)?;
    let (rc, ra) = call_of(claim_r)?;
    if lc.rsplit('.').next()? != vb.fn_name || rc != lc || la.len() != 2 || ra.len() != 2 {
        return None;
    }
    let a = ident_of(&la[0])?;
    let Expr::BinOp(BinOp::Mul, mul_b, mul_d) = &la[1].node else {
        return None;
    };
    let b = ident_of(mul_b)?;
    let d = ident_of(mul_d)?;
    let w = ident_of(&ra[0])?;
    if ident_of(&ra[1])? != d {
        return None;
    }
    // The four roles are distinct names covering the givens exactly.
    let roles = [a, b, w, d];
    let given_names: Vec<&str> = law.givens.iter().map(|g| g.name.as_str()).collect();
    let mut sorted_roles = roles.to_vec();
    sorted_roles.sort_unstable();
    sorted_roles.dedup();
    if sorted_roles.len() != 4 || roles.iter().any(|r| !given_names.contains(r)) {
        return None;
    }
    if given_names.iter().any(|g| FLOOR_RESERVED.contains(g)) {
        return None;
    }
    if !is_floor_div_wrapper(vb, ctx) {
        return None;
    }
    // Premise: exactly the four cell conjuncts, located by shape.
    let conjuncts = when_conjuncts(when)?;
    if conjuncts.len() != 4 {
        return None;
    }
    let find = |pred: &dyn Fn(&Spanned<Expr>) -> bool| -> Option<usize> {
        conjuncts.iter().position(|c| pred(c))
    };
    let hyps = [
        find(&|c| is_gt_zero(c, b))?,
        find(&|c| is_gt_zero(c, d))?,
        find(&|c| is_cell_low(c, w, b, a))?,
        find(&|c| is_cell_high(c, a, w, b))?,
    ];
    let mut sorted_hyps = hyps.to_vec();
    sorted_hyps.sort_unstable();
    sorted_hyps.dedup();
    if sorted_hyps.len() != 4 {
        return None;
    }
    Some(FloorPlan {
        roles: roles.map(str::to_string),
        hyps,
    })
}

/// The hand-validated arithmetic core (kernel axioms
/// `[propext, Quot.sound]` on core Lean v4.15), rendered as a private
/// lane-local lemma — never a hole.
fn floor_core_lemma(name: &str) -> String {
    format!(
        r#"/-- Floors at a coarser grid are stable across an open finer-grid
cell: `W*b < a < (W+1)*b` and `D > 0` give `a / (b*D) = W / D`. -/
private theorem {name} (a b W D : Int) (hb : 0 < b) (hD : 0 < D)
    (hlo : W * b < a) (hhi : a < (W + 1) * b) :
    a / (b * D) = W / D := by
  have hbD : 0 < b * D := Int.mul_pos hb hD
  have hmod : D * (W / D) + W % D = W := Int.ediv_add_emod W D
  have hr0 : 0 ≤ W % D := Int.emod_nonneg W (by omega)
  have hrD : W % D < D := Int.emod_lt_of_pos W hD
  have hqDW : (W / D) * D ≤ W := by
    calc (W / D) * D = D * (W / D) := Int.mul_comm _ _
      _ ≤ D * (W / D) + W % D := Int.le_add_of_nonneg_right hr0
      _ = W := hmod
  have hW1 : W + 1 ≤ (W / D + 1) * D := by
    calc W + 1 = D * (W / D) + W % D + 1 := by rw [hmod]
      _ ≤ D * (W / D) + D := by omega
      _ = (W / D + 1) * D := by
            rw [Int.add_mul, Int.one_mul, Int.mul_comm]
  have hlow : (W / D) * (b * D) ≤ a := by
    have h1 : ((W / D) * D) * b ≤ W * b :=
      Int.mul_le_mul_of_nonneg_right hqDW (Int.le_of_lt hb)
    have h2 : (W / D) * (b * D) = ((W / D) * D) * b := by
      rw [Int.mul_assoc, Int.mul_comm b D]
    omega
  have hup : a < (W / D + 1) * (b * D) := by
    have h1 : (W + 1) * b ≤ ((W / D + 1) * D) * b :=
      Int.mul_le_mul_of_nonneg_right hW1 (Int.le_of_lt hb)
    have h2 : (W / D + 1) * (b * D) = ((W / D + 1) * D) * b := by
      rw [Int.mul_assoc, Int.mul_comm b D]
    omega
  have hle : W / D ≤ a / (b * D) := (Int.le_ediv_iff_mul_le hbD).mpr hlow
  have hlt : a / (b * D) < W / D + 1 := by
    rcases Int.lt_trichotomy (a / (b * D)) (W / D + 1) with h | h | h
    · exact h
    · have hge : W / D + 1 ≤ a / (b * D) := by rw [h]; exact Int.le_refl _
      have := (Int.le_ediv_iff_mul_le hbD).mp hge
      omega
    · have := (Int.le_ediv_iff_mul_le hbD).mp (Int.le_of_lt h)
      omega
  omega
"#
    )
}

pub(super) fn render_floor_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    plan: &FloorPlan,
    entry_root: &str,
    entry_content: &str,
    sabotage: bool,
) -> Option<LaneLawFile> {
    let emit = |e: &Spanned<Expr>| emit_expr_legacy(e, ctx, None);

    let fn_lean = aver_name_to_lean(&vb.fn_name);
    let law_lean = aver_name_to_lean(&law.name);
    let theorem_base = format!("{fn_lean}_law_{law_lean}");
    let theorem = format!("{theorem_base}_universal");
    let core = format!("{theorem_base}__floor_stable");

    let lhs_template = emit(&law.lhs);
    let rhs_template = emit(&law.rhs).replace('\n', " ");
    let when_template = emit(law.when.as_ref()?).replace('\n', " ");

    // Same statement builder as the manifest theorem, minus the
    // sampled-domain disjunctions (`omit_domain`) — the twin is the
    // manifest claim with the bounding premises removed.
    let lifted = std::collections::HashMap::new();
    let (prop, bounded) = super::super::toplevel::law_theorem_prop(
        law,
        ctx,
        &lhs_template,
        &rhs_template,
        Some(&when_template),
        &lifted,
        true,
    );
    debug_assert!(!bounded);
    let given_lean: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let quant_params = given_lean
        .iter()
        .map(|g| format!("({g} : Int)"))
        .collect::<Vec<_>>()
        .join(" ");

    let [a, b, w, d] = &plan.roles;
    let (a, b, w, d) = (
        aver_name_to_lean(a),
        aver_name_to_lean(b),
        aver_name_to_lean(w),
        aver_name_to_lean(d),
    );
    let [hb, hd, hlo, hhi] = plan.hyps.map(|i| format!("h{i}"));

    let sabotage_line = if sabotage {
        // TEST-ONLY (`AVER_PROOF_LANE_SABOTAGE`): an unknown identifier
        // makes this module's build fail hard — the lane must absorb it
        // with zero effect on budgets and neighbors.
        "\n  exact averLaneSabotageInjectedByTest"
    } else {
        ""
    };

    let mut content = String::new();
    content.push_str(&format!(
        "-- Aver when-universal quarantine lane — verify law {}.{}\n\
         -- NOT part of the counted default build. Built by a separate,\n\
         -- failure-tolerated per-law `lake build` invocation; credited only\n\
         -- on per-declaration `#print axioms` evidence (whitelist: propext,\n\
         -- Classical.choice, Quot.sound). This module carries no honest-\n\
         -- floor fallback: a non-closing proof is a tolerated build failure\n\
         -- (the law stays bounded), never a counted warning.\n",
        vb.fn_name, law.name,
    ));
    content.push_str(&format!("import {entry_root}\n\n"));
    content.push_str("set_option linter.unusedVariables false\n\n");
    content.push_str(&floor_core_lemma(&core));
    content.push('\n');
    content.push_str(&format!(
        "{}{} {}\n",
        super::super::LAW_CLASS_MARKER_PREFIX,
        theorem,
        super::super::LAW_CLASS_UNIVERSAL
    ));
    content.push_str(&format!(
        "theorem {theorem} : ∀ {quant_params}, {prop} := by\n"
    ));
    content.push_str(&format!(
        "  intro {} h_when{sabotage_line}\n",
        given_lean.join(" ")
    ));
    content.push_str("  simp only [Bool.and_eq_true, decide_eq_true_eq] at h_when\n");
    content.push_str("  obtain ⟨h0, h1, h2, h3⟩ := h_when\n");
    content.push_str(&format!(
        "  have key := {core} {a} {b} {w} {d} {hb} {hd} {hlo} {hhi}\n"
    ));
    content.push_str(&format!("  have hbd : ¬ ({b} * {d} = 0) := by\n"));
    content.push_str(&format!("    have := Int.mul_pos {hb} {hd}\n"));
    content.push_str("    omega\n");
    content.push_str(&format!("  have hd0 : ¬ ({d} = 0) := by omega\n"));
    content.push_str(&format!(
        "  simp only [{fn_lean}, hbd, hd0, if_false, beq_iff_eq, beq_self_eq_true]\n"
    ));
    content.push_str("  simpa using key\n");

    // Prop-form corollary, derived from the twin above with the fixed
    // Bool/Prop bridge tactic — same module, no separate credit.
    content.push('\n');
    let given_names: Vec<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    content.push_str(&super::shared::render_prop_corollary(
        &theorem_base,
        &quant_params,
        &given_names,
        law,
        ctx,
        &format!("{lhs_template} = {rhs_template}"),
    ));

    // L2 of the iron guard: the lane grammar has no sorry carrier.
    debug_assert!(
        !content.contains("sorry"),
        "universal-lane module must not contain a sorry token"
    );

    let module = lane_module_id(&theorem_base, &content, entry_content);

    Some(LaneLawFile {
        label: format!("{}.{}", vb.fn_name, law.name),
        companion: format!("{theorem_base}_prop"),
        theorem,
        theorem_base,
        module,
        imports: Vec::new(),
        content,
    })
}
