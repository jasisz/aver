//! Lean rendering of proof steps (`crate::ir::proof_steps`).
//!
//! A step script becomes one explicit term: `Eq.trans`, `Eq.symm`,
//! `congrArg`, and an application of one lemma per rule. The Lean-specific
//! parts are exactly two tables: wall rule → `AverSteps.*` lemma name (the
//! lemmas live in the prelude, proved once), and law → theorem name.
//! Definitions are opened through `__aver_unfold_<n>` lemmas stated next
//! to the law, one per (function, arm), never through Lean's own equation
//! numbering. Every intermediate equation is stated with `show`, so the
//! kernel checks the data and elaboration never searches.

use std::collections::BTreeMap;

use crate::ast::{BinOp, Spanned, VerifyKind};
use crate::codegen::CodegenContext;
use crate::ir::hir::{ResolvedCallee, ResolvedExpr, ResolvedPattern};
use crate::ir::identity::FnId;
use crate::ir::proof_steps::check::{Hyps, arm_equation, conclusion};
use crate::ir::proof_steps::term::{self, HOLE, Term};
use crate::ir::proof_steps::{Eqn, Proof, Script, WallRule};

use super::expr::{aver_name_to_lean, emit_expr};
use super::types::type_to_lean;

/// The prelude section the rendered terms rely on. Each lemma is the Lean
/// statement of one wall rule (`crate::ir::proof_steps::rules`).
pub(crate) const LEAN_PRELUDE_AVER_STEPS: &str = r#"namespace AverSteps
theorem bool_cases {P : Prop} (c : Bool) (hf : c = false → P) (ht : c = true → P) : P := by
  cases c
  · exact hf rfl
  · exact ht rfl
theorem and_true_l (b : Bool) : (true && b) = b := rfl
theorem and_false_l (b : Bool) : (false && b) = false := rfl
theorem and_true_r (a : Bool) : (a && true) = a := by cases a <;> rfl
theorem and_false_r (a : Bool) : (a && false) = false := by cases a <;> rfl
theorem or_true_l (b : Bool) : (true || b) = true := rfl
theorem or_false_l (b : Bool) : (false || b) = b := rfl
theorem or_true_r (a : Bool) : (a || true) = true := by cases a <;> rfl
theorem or_false_r (a : Bool) : (a || false) = a := by cases a <;> rfl
theorem not_true : (!true) = false := rfl
theorem not_false : (!false) = true := rfl
theorem and_elim_l (a b : Bool) (h : (a && b) = true) : a = true := by
  cases a <;> cases b <;> first | rfl | exact absurd h (by decide)
theorem and_elim_r (a b : Bool) (h : (a && b) = true) : b = true := by
  cases a <;> cases b <;> first | rfl | exact absurd h (by decide)
theorem le_of_not_gt (a b : Int) (h : decide (a > b) = false) : decide (a <= b) = true :=
  decide_eq_true (Int.not_lt.mp (of_decide_eq_false h))
theorem le_false_of_gt (a b : Int) (h : decide (a > b) = true) : decide (a <= b) = false :=
  decide_eq_false (Int.not_le.mpr (of_decide_eq_true h))
theorem ge_of_not_lt (a b : Int) (h : decide (a < b) = false) : decide (a >= b) = true :=
  decide_eq_true (Int.not_lt.mp (of_decide_eq_false h))
theorem ge_false_of_lt (a b : Int) (h : decide (a < b) = true) : decide (a >= b) = false :=
  decide_eq_false (Int.not_le.mpr (of_decide_eq_true h))
theorem lt_of_not_ge (a b : Int) (h : decide (a >= b) = false) : decide (a < b) = true :=
  decide_eq_true (Int.not_le.mp (of_decide_eq_false h))
theorem lt_false_of_ge (a b : Int) (h : decide (a >= b) = true) : decide (a < b) = false :=
  decide_eq_false (Int.not_lt.mpr (of_decide_eq_true h))
theorem gt_of_not_le (a b : Int) (h : decide (a <= b) = false) : decide (a > b) = true :=
  decide_eq_true (Int.not_le.mp (of_decide_eq_false h))
theorem gt_false_of_le (a b : Int) (h : decide (a <= b) = true) : decide (a > b) = false :=
  decide_eq_false (Int.not_lt.mpr (of_decide_eq_true h))
theorem eq_of_not_ne (a b : Int) (h : (a != b) = false) : (a == b) = true := by
  simpa [bne] using h
theorem eq_false_of_ne (a b : Int) (h : (a != b) = true) : (a == b) = false := by
  simpa [bne] using h
theorem ne_of_not_eq (a b : Int) (h : (a == b) = false) : (a != b) = true := by
  simp [bne, h]
theorem ne_false_of_eq (a b : Int) (h : (a == b) = true) : (a != b) = false := by
  simp [bne, h]
theorem add_comm (a b : Int) : a + b = b + a := Int.add_comm a b
theorem mul_comm (a b : Int) : a * b = b * a := Int.mul_comm a b
theorem add_assoc (a b c : Int) : a + b + c = a + (b + c) := Int.add_assoc a b c
theorem mul_assoc (a b c : Int) : a * b * c = a * (b * c) := Int.mul_assoc a b c
theorem add_zero (a : Int) : a + 0 = a := Int.add_zero a
theorem zero_add (a : Int) : 0 + a = a := Int.zero_add a
theorem mul_one (a : Int) : a * 1 = a := Int.mul_one a
theorem one_mul (a : Int) : 1 * a = a := Int.one_mul a
theorem sub_zero (a : Int) : a - 0 = a := Int.sub_zero a
theorem div_mod_recompose (a k : Int) (_h : decide (k > 0) = true) : a / k * k + a % k = a :=
  Int.ediv_mul_add_emod a k
theorem div_range (a k m n : Int) (h1 : (decide (0 <= a) && decide (a < m)) = true)
    (h2 : (decide (k > 0) && (m == n * k)) = true) :
    (decide (0 <= a / k) && decide (a / k < n)) = true := by
  simp only [Bool.and_eq_true, decide_eq_true_eq, beq_iff_eq] at h1 h2 ⊢
  obtain ⟨h0, hm⟩ := h1
  obtain ⟨hk, rfl⟩ := h2
  exact ⟨Int.ediv_nonneg h0 (Int.le_of_lt hk), (Int.ediv_lt_iff_lt_mul hk).mpr hm⟩
end AverSteps"#;

/// The only rule-specific Lean table.
fn lemma(rule: WallRule) -> &'static str {
    match rule {
        WallRule::AndTrueL => "AverSteps.and_true_l",
        WallRule::AndFalseL => "AverSteps.and_false_l",
        WallRule::AndTrueR => "AverSteps.and_true_r",
        WallRule::AndFalseR => "AverSteps.and_false_r",
        WallRule::OrTrueL => "AverSteps.or_true_l",
        WallRule::OrFalseL => "AverSteps.or_false_l",
        WallRule::OrTrueR => "AverSteps.or_true_r",
        WallRule::OrFalseR => "AverSteps.or_false_r",
        WallRule::NotTrue => "AverSteps.not_true",
        WallRule::NotFalse => "AverSteps.not_false",
        WallRule::AndElimL => "AverSteps.and_elim_l",
        WallRule::AndElimR => "AverSteps.and_elim_r",
        WallRule::LeOfNotGt => "AverSteps.le_of_not_gt",
        WallRule::LeFalseOfGt => "AverSteps.le_false_of_gt",
        WallRule::GeOfNotLt => "AverSteps.ge_of_not_lt",
        WallRule::GeFalseOfLt => "AverSteps.ge_false_of_lt",
        WallRule::LtOfNotGe => "AverSteps.lt_of_not_ge",
        WallRule::LtFalseOfGe => "AverSteps.lt_false_of_ge",
        WallRule::GtOfNotLe => "AverSteps.gt_of_not_le",
        WallRule::GtFalseOfLe => "AverSteps.gt_false_of_le",
        WallRule::EqOfNotNe => "AverSteps.eq_of_not_ne",
        WallRule::EqFalseOfNe => "AverSteps.eq_false_of_ne",
        WallRule::NeOfNotEq => "AverSteps.ne_of_not_eq",
        WallRule::NeFalseOfEq => "AverSteps.ne_false_of_eq",
        WallRule::AddComm => "AverSteps.add_comm",
        WallRule::MulComm => "AverSteps.mul_comm",
        WallRule::AddAssoc => "AverSteps.add_assoc",
        WallRule::MulAssoc => "AverSteps.mul_assoc",
        WallRule::AddZero => "AverSteps.add_zero",
        WallRule::ZeroAdd => "AverSteps.zero_add",
        WallRule::MulOne => "AverSteps.mul_one",
        WallRule::OneMul => "AverSteps.one_mul",
        WallRule::SubZero => "AverSteps.sub_zero",
        WallRule::DivModRecompose => "AverSteps.div_mod_recompose",
        WallRule::DivRange => "AverSteps.div_range",
    }
}

/// A rendered step proof: the local unfold lemmas (`have …`) and the term
/// that proves the obligation once the givens and the `when` hypothesis are
/// introduced.
pub(crate) struct Rendered {
    pub support: Vec<String>,
    pub term: String,
}

impl Rendered {
    /// The tactic text of the step branch: every local lemma, then the term.
    pub(crate) fn tactic(&self) -> String {
        let mut s = String::new();
        for h in &self.support {
            s.push_str(h);
            s.push_str("; ");
        }
        s.push_str("exact ");
        s.push_str(&self.term);
        s
    }
}

struct Renderer<'a> {
    script: &'a Script,
    ctx: &'a CodegenContext,
    /// Lean theorem names of cited laws, by law key.
    laws: BTreeMap<String, String>,
    /// Emitted unfold lemmas, by (function, arm).
    unfolds: BTreeMap<(FnId, u32), String>,
    support: Vec<String>,
}

fn is_prop_comparison(t: &Term) -> bool {
    matches!(
        t.node,
        ResolvedExpr::BinOp(BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte, ..)
    )
}

fn is_bool_term(t: &Term) -> bool {
    matches!(t.ty(), Some(crate::ast::Type::Bool))
        || is_prop_comparison(t)
        || term::bool_value(t).is_some()
        || matches!(&t.node, ResolvedExpr::BinOp(BinOp::Eq | BinOp::Neq, ..))
        || matches!(&t.node, ResolvedExpr::Call(ResolvedCallee::Builtin(b), _)
            if matches!(b.as_str(), "Bool.and" | "Bool.or" | "Bool.not"))
}

impl Renderer<'_> {
    fn expr(&self, t: &Term) -> String {
        // A dependency's binding reads as `Lib.base`; Lean spells each
        // segment of that path on its own (`Lib.local'`).
        let qualified: Vec<(String, Term)> = self
            .script
            .consts
            .iter()
            .filter(|c| c.name.contains('.'))
            .map(|c| {
                let read = term::var(&super::syntax::aver_path_to_lean(&c.name));
                if let Some(ty) = c.value.ty() {
                    read.set_ty(ty.clone());
                }
                (c.name.clone(), read)
            })
            .collect();
        let renamed;
        let t = if qualified.is_empty() {
            t
        } else {
            renamed = term::subst(t, &qualified).unwrap_or_else(|_| t.clone());
            &renamed
        };
        let s = emit_expr(t, self.ctx);
        if is_bool_term(t) {
            format!("({s} : Bool)")
        } else {
            format!("({s})")
        }
    }

    fn eqn(&self, e: &Eqn) -> String {
        format!("{} = {}", self.expr(&e.lhs), self.expr(&e.rhs))
    }

    fn hyp_name(&self, name: &str) -> String {
        if name == "when" {
            "h_when".to_string()
        } else {
            name.to_string()
        }
    }

    fn concl(&self, p: &Proof, hyps: &Hyps) -> Result<Eqn, String> {
        conclusion(p, self.script, hyps)
    }

    fn proof(&mut self, p: &Proof, hyps: &Hyps) -> Result<String, String> {
        let eq = self.concl(p, hyps)?;
        let body = match p {
            // A module-level binding is a Lean `def` with no parameters;
            // its value is its definitional unfolding.
            Proof::Refl(_) | Proof::Proj { .. } | Proof::UnfoldConst { .. } => "rfl".to_string(),
            Proof::Symm(inner) => format!("Eq.symm {}", self.proof(inner, hyps)?),
            Proof::Trans { steps, .. } => {
                let mut parts = Vec::new();
                for s in steps {
                    parts.push(self.proof(s, hyps)?);
                }
                let mut acc = parts.pop().expect("trans has steps");
                while let Some(prev) = parts.pop() {
                    acc = format!("(Eq.trans {prev} {acc})");
                }
                acc
            }
            Proof::Congr { ctx, inner } => {
                let inner_eq = self.concl(inner, hyps)?;
                let binder = match inner_eq
                    .lhs
                    .ty()
                    .filter(|ty| crate::types::checker::type_is_fully_concrete(ty))
                {
                    Some(ty) => format!("({HOLE} : {})", type_to_lean(ty)),
                    None => HOLE.to_string(),
                };
                let body = self.expr(ctx);
                format!(
                    "congrArg (fun {binder} => {body}) {}",
                    self.proof(inner, hyps)?
                )
            }
            Proof::Unfold {
                fn_id,
                arm,
                args,
                binders,
                premise,
            } => {
                let name = self.unfold_lemma(*fn_id, *arm)?;
                let mut s = name;
                for a in args.iter().chain(binders) {
                    s.push(' ');
                    s.push_str(&self.expr(a));
                }
                if let Some(p) = premise {
                    s.push(' ');
                    s.push_str(&self.proof(p, hyps)?);
                }
                s
            }
            Proof::Arm {
                term: t,
                arm,
                binders,
                premise,
            } => {
                let ResolvedExpr::Match { arms, .. } = &t.node else {
                    return Err("arm: not a match".into());
                };
                let tactic = arm_tactic(&arms[*arm as usize - 1].pattern, t, "h_arm")?;
                let _ = binders;
                format!("(fun h_arm => by {tactic}) {}", self.proof(premise, hyps)?)
            }
            // A `when` that is a bare comparison is stated `(a < b) = true`,
            // which Lean reads as an equation of Props; recover the Bool
            // equation the steps use.
            Proof::Hyp(name)
                if name == "when"
                    && self
                        .script
                        .obligation
                        .premise
                        .as_ref()
                        .is_some_and(is_prop_comparison) =>
            {
                "decide_eq_true (of_eq_true (h_when.trans (eq_self true)))".to_string()
            }
            Proof::Hyp(name) => self.hyp_name(name),
            Proof::Rule {
                rule,
                subst,
                premises,
            } => {
                let mut s = lemma(*rule).to_string();
                for (_, v) in subst {
                    s.push(' ');
                    s.push_str(&self.expr(v));
                }
                for p in premises {
                    s.push(' ');
                    s.push_str(&self.proof(p, hyps)?);
                }
                s
            }
            Proof::Law {
                law,
                subst,
                premise,
            } => {
                let mut s = self
                    .laws
                    .get(law)
                    .cloned()
                    .ok_or_else(|| format!("law {law} has no theorem to cite"))?;
                for (_, v) in subst {
                    s.push(' ');
                    s.push_str(&self.expr(v));
                }
                if let Some(p) = premise {
                    s.push(' ');
                    s.push_str(&self.proof(p, hyps)?);
                }
                s
            }
            Proof::Compute { .. } => "by decide".to_string(),
            Proof::Cases {
                on,
                hyp,
                if_true,
                if_false,
            } => {
                let with = |v: bool| {
                    let mut h = hyps.clone();
                    h.push((hyp.clone(), Eqn::new(on.clone(), term::boolean(v))));
                    h
                };
                let pf = self.proof(if_false, &with(false))?;
                let pt = self.proof(if_true, &with(true))?;
                format!(
                    "AverSteps.bool_cases {} (fun {hyp} => {pf}) (fun {hyp} => {pt})",
                    self.expr(on)
                )
            }
        };
        Ok(format!("(show {} from {body})", self.eqn(&eq)))
    }

    /// A local `have __aver_unfold_<n> : ∀ (x…) (y…) [h], f x… = arm`,
    /// proved once inside the step branch: if Lean cannot prove it, only
    /// that branch fails and the law falls back to its portfolio.
    fn unfold_lemma(&mut self, fn_id: FnId, arm: u32) -> Result<String, String> {
        if let Some(name) = self.unfolds.get(&(fn_id, arm)) {
            return Ok(name.clone());
        }
        let def = self
            .script
            .def(fn_id)
            .ok_or_else(|| "unfold: no definition".to_string())?;
        let fd = fn_def(self.ctx, fn_id)
            .ok_or_else(|| format!("unfold: {} has no source definition", def.name))?;
        let xs: Vec<Term> = (0..def.params.len())
            .map(|i| term::var(&format!("x{i}")))
            .collect();
        let call = Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(fn_id), xs.clone()));
        let outer = def.outer(&xs)?;
        let mut binders = String::new();
        let mut names: Vec<String> = Vec::new();
        for (i, (_, ty)) in fd.params.iter().enumerate() {
            binders.push_str(&format!(
                " (x{i} : {})",
                super::types::type_annotation_to_lean(ty)
            ));
            names.push(format!("x{i}"));
        }
        let f_name = emit_expr(
            &Spanned::bare(ResolvedExpr::Call(ResolvedCallee::Fn(fn_id), Vec::new())),
            self.ctx,
        );
        let name = format!("__aver_unfold_{}", self.unfolds.len());
        let statement;
        let tactic;
        if arm == 0 {
            let body = term::subst(&def.body, &outer)?;
            statement = format!("{} = {}", self.expr(&call), self.expr(&body));
            tactic = "rfl".to_string();
        } else {
            let ResolvedExpr::Match { subject, arms } = &def.body.node else {
                return Err("unfold: not a match".into());
            };
            let pat = &arms[arm as usize - 1].pattern;
            let n = term::pattern_binders(pat).len();
            let ys: Vec<Term> = (0..n).map(|i| term::var(&format!("y{i}"))).collect();
            for i in 0..n {
                binders.push_str(&format!(" (y{i} : _)"));
                names.push(format!("y{i}"));
            }
            let (premise, body) = arm_equation(subject, arms, arm, &outer, &ys)?;
            binders.push_str(&format!(" (h : {})", self.eqn(&premise)));
            names.push("h".to_string());
            statement = format!("{} = {}", self.expr(&call), self.expr(&body));
            let subject = term::subst(subject, &outer)?;
            tactic = arm_tactic(pat, &subject, "h")?;
        }
        let intro = if names.is_empty() {
            String::new()
        } else {
            format!("intro {}; ", names.join(" "))
        };
        let quantified = if binders.is_empty() {
            statement
        } else {
            format!("∀{binders}, {statement}")
        };
        self.support.push(format!(
            "have {name} : {quantified} := (by {intro}unfold {f_name}; {tactic})"
        ));
        self.unfolds.insert((fn_id, arm), name.clone());
        Ok(name)
    }
}

/// The tactic that selects a known arm of the unfolded body, given
/// `h : subject = pattern`.
fn arm_tactic(pat: &ResolvedPattern, subject: &Term, h: &str) -> Result<String, String> {
    Ok(match pat {
        ResolvedPattern::Literal(crate::ast::Literal::Bool(v)) => {
            let positive = *v;
            let evidence = if is_prop_comparison(subject) {
                if positive {
                    format!("(of_decide_eq_true {h})")
                } else {
                    format!("(of_decide_eq_false {h})")
                }
            } else if positive {
                h.to_string()
            } else {
                format!("(by simp [{h}])")
            };
            if positive {
                format!("exact ite_eq_left {evidence}")
            } else {
                format!("exact ite_eq_right {evidence}")
            }
        }
        // A variable subject is replaced outright; otherwise rewrite it.
        // Either way the selected arm is then a definitional reduction.
        _ if matches!(subject.node, ResolvedExpr::Ident(_)) => format!("subst {h}; rfl"),
        _ => format!("rw [{h}]; try rfl"),
    })
}

fn fn_def(ctx: &CodegenContext, id: FnId) -> Option<&crate::ast::FnDef> {
    let key = &ctx.symbol_table.fn_entry(id).key;
    match key.scope_str() {
        Some(prefix) => ctx
            .modules
            .iter()
            .find(|m| m.prefix == prefix)?
            .fn_defs
            .iter()
            .find(|f| f.name == key.name),
        None => ctx.items.iter().find_map(|item| match item {
            crate::ast::TopLevel::FnDef(f) if f.name == key.name => Some(f),
            _ => None,
        }),
    }
}

fn same_file_blocks(ctx: &CodegenContext) -> Vec<&crate::ast::VerifyBlock> {
    match ctx.active_module_scope().as_deref() {
        Some(prefix) => ctx
            .modules
            .iter()
            .find(|m| m.prefix == prefix)
            .map(|m| m.verify_blocks.iter().collect())
            .unwrap_or_default(),
        None => ctx
            .items
            .iter()
            .filter_map(|item| match item {
                crate::ast::TopLevel::Verify(vb) => Some(vb),
                _ => None,
            })
            .collect(),
    }
}

/// The Lean theorem a law key names, as `dependencies` in the reason
/// emitter finds it.
fn law_theorem(key: &str, ctx: &CodegenContext) -> Option<String> {
    let own = ctx.active_module_scope();
    let local_key = |prefix: Option<&str>, fn_name: &str, law: &str| match prefix {
        Some(p) => format!("{p}.{fn_name}.{law}"),
        None => format!("{fn_name}.{law}"),
    };
    for b in same_file_blocks(ctx) {
        let VerifyKind::Law(l) = &b.kind else {
            continue;
        };
        if key == local_key(own.as_deref(), &b.fn_name, &l.name) {
            return super::toplevel::law_as_lemma_statement(b, l, ctx).map(|(name, _)| name);
        }
    }
    for module in &ctx.modules {
        for block in &module.verify_laws {
            let VerifyKind::Law(l) = &block.kind else {
                continue;
            };
            if key == local_key(Some(&module.prefix), &block.fn_name, &l.name) {
                return ctx.with_module_scope(Some(&module.prefix), || {
                    super::toplevel::law_as_lemma_statement(block, l, ctx)
                        .map(|(name, _)| format!("{}.{}", aver_name_to_lean(&module.prefix), name))
                });
            }
        }
    }
    None
}

/// Render `script` as a term for its law's theorem, after `intro` of the
/// givens (and `h_when`).
pub(crate) fn render(script: &Script, ctx: &CodegenContext) -> Result<Rendered, String> {
    let mut laws = BTreeMap::new();
    for l in &script.laws {
        if let Some(name) = law_theorem(&l.key, ctx) {
            laws.insert(l.key.clone(), name);
        }
    }
    let mut r = Renderer {
        script,
        ctx,
        laws,
        unfolds: BTreeMap::new(),
        support: Vec::new(),
    };
    let mut hyps = Hyps::new();
    if let Some(p) = &script.obligation.premise {
        hyps.push(("when".into(), Eqn::new(p.clone(), term::boolean(true))));
    }
    let term = r.proof(&script.proof, &hyps)?;
    Ok(Rendered {
        support: r.support,
        term,
    })
}

/// The marker a rejected step proof leaves in the build log.
pub(crate) const STEPS_REJECTED_MARKER: &str = "AVER_STEPS_REJECTED:";

/// Wrap a portfolio so the step term is tried first:
/// `first | (intro …; exact <term>) | (trace <rejected>; <portfolio>)`.
pub(crate) fn lead_portfolio(
    rendered: &Rendered,
    intro: &[String],
    label: &str,
    portfolio: crate::codegen::lean::tactic_ir::Tactic,
) -> crate::codegen::lean::tactic_ir::Tactic {
    use crate::codegen::lean::tactic_ir::Tactic;
    let intro_line = if intro.is_empty() {
        String::new()
    } else {
        format!("intro {}; ", intro.join(" "))
    };
    // Leaves may carry a baked-in indent; new leaves take the same one so
    // the rendered block stays aligned.
    let pad = " ".repeat(portfolio.leaf_min_indent().unwrap_or(0));
    Tactic::First(vec![
        Tactic::Leaf(format!("{pad}{intro_line}{}", rendered.tactic())),
        Tactic::Seq(vec![
            Tactic::Leaf(format!("{pad}trace \"{STEPS_REJECTED_MARKER}{label}\"")),
            portfolio,
        ]),
    ])
}

/// One `proof_steps/<law>.steps` file per law with a step proof: the
/// serialised script the Aver replayer checks.
pub(crate) fn step_files(ctx: &CodegenContext) -> Vec<(String, String)> {
    ctx.proof_ir
        .law_theorems
        .iter()
        .filter_map(|t| {
            let script = t.steps.as_ref()?;
            let text = crate::ir::proof_steps::sexpr::script(script, &ctx.symbol_table).ok()?;
            Some((format!("proof_steps/{}.steps", script.obligation.key), text))
        })
        .collect()
}
