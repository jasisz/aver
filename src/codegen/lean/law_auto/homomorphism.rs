//! CONTENT-BLIND homomorphism rung — ONE recognizer + ONE parameterized
//! skeleton that closes ANY law of the shape
//!
//! ```text
//!     subject (a OP1 b) = OP2 (subject a) (subject b)
//! ```
//!
//! where `subject` is a recursive fn (so its `.induct` principle exists), `OP1`
//! is the argument-combine and `OP2` the result-combine. The three operators are
//! captured STRUCTURALLY from the law's AST — there is no `pow2` / `times` /
//! `sameValue` / `+` literal anywhere in the recognizer. The same rung therefore
//! closes the integer power-of-two homomorphism (`pow2 (m + n) = pow2 m * pow2 n`,
//! `OP1 = +`, `OP2 = *`, guarded arithmetic recurrence) AND a list-length
//! homomorphism (`len (cat xs ys) = len xs + len ys`, `OP1 = cat`, `OP2 = +`,
//! structural recurrence) — the cross-domain witness that it is shape-only.
//!
//! The skeleton is the de-risked, kernel-checked one (core Lean 4.31, no Mathlib):
//!
//! ```text
//!     induction <OP1-first-arg> using <subject>.induct
//!       BASE: unfold subject (its base equation) + OP2 left-identity (subject base = OP2.id)
//!       STEP: unfold subject (both sides) + apply IH + OP2 associativity
//! ```
//!
//! The UNFOLD slot is the only meaning-sensitive part, and it has a 2-WAY
//! dispatch keyed on the subject's recurrence SHAPE (read from its body), NOT on
//! its meaning:
//!
//! * a `p <= 0`-guarded arithmetic countdown (e.g. `pow2`) cannot be unfolded by
//!   a blind `simp only [eq_def]` (the guarded equation rewrites back into itself
//!   and loops), so each step is a controlled single `rw` of the conditional
//!   equation with the guard discharged by `omega`, plus an `omega`
//!   index-normalization of the `OP1` peel (`(m + n) - 1 = (m - 1) + n`);
//! * a structural ADT recurrence (e.g. `len` over a cons list) terminates under
//!   `simp_all only [subject, OP1]`, which also folds the inductive hypothesis.
//!
//! Both branches are meaning-blind (they are driven by the recurrence shape and
//! `omega`), and every leaf sits under a `first | (proof) | sorry` floor, so a
//! function that is NOT actually a homomorphism degrades to a `sorry` the
//! `#print axioms` whitelist rejects — never a false theorem.

use super::AutoProof;
use super::aver_name_to_lean;
use super::recursive_pure_fn_names;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name, law_simp_defs};
use crate::ast::{BinOp, Expr, FnDef, Literal, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The result-combine operator `OP2`, captured from the law's rhs `BinOp`. Only
/// the additive (`+`, identity `0`) and multiplicative (`*`, identity `1`)
/// commutative-monoid carriers over `Int` are admitted — the two whose
/// associativity is a core-Lean lemma. The identity / associativity Lean names
/// are DERIVED from the captured operator, not hardcoded into the recognizer.
#[derive(Clone, Copy, PartialEq)]
enum Op2 {
    Add,
    Mul,
}

impl Op2 {
    fn from_binop(op: BinOp) -> Option<Self> {
        match op {
            BinOp::Add => Some(Op2::Add),
            BinOp::Mul => Some(Op2::Mul),
            _ => None,
        }
    }
    /// The left-identity LITERAL the subject's base arm must equal for the
    /// homomorphism to hold (`subject base = OP2.id`).
    fn identity_int(self) -> i64 {
        match self {
            Op2::Add => 0,
            Op2::Mul => 1,
        }
    }
    /// The core-Lean associativity lemma for `OP2` — selected by the captured
    /// operator, the only meaning the rung reads off `OP2`.
    fn assoc_lemma(self) -> &'static str {
        match self {
            Op2::Add => "Int.add_assoc",
            Op2::Mul => "Int.mul_assoc",
        }
    }
    fn symbol(self) -> &'static str {
        match self {
            Op2::Add => "+",
            Op2::Mul => "*",
        }
    }
}

/// The argument-combine operator `OP1`, captured from the subject's single
/// argument in the law's lhs.
enum Op1 {
    /// Arithmetic combine `a + b` — pairs with a guarded `Int` countdown subject.
    Add,
    /// Structural combine `cat a b` (a 2-arg recursive fn) — pairs with a
    /// structural ADT recurrence subject. Carries the fn's source name.
    Concat(String),
}

/// The recurrence shape of the subject, picked by reading its body. Drives the
/// 2-way UNFOLD dispatch.
enum Recurrence {
    /// `match p <= 0 { true -> BASE; false -> STEP }`, `p : Int`. `step_arm` is
    /// the rendered `false`-arm with the parameter canonicalized to `n`.
    Guarded { base_arm: String, step_arm: String },
    /// `match xs { BaseCtor -> BASE; RecCtor(..) -> STEP }` over an ADT.
    Structural,
}

struct HomShape {
    subject_src: String,
    op1: Op1,
    op2: Op2,
    /// `OP1`'s first argument (the induction variable), as the law wrote it.
    arg0: Spanned<Expr>,
    recurrence: Recurrence,
}

/// Structural identity of two atom expressions (the law's givens) by rendered
/// Lean text — robust to `Ident` vs `Resolved`.
fn same_atom(a: &Spanned<Expr>, b: &Spanned<Expr>, ctx: &CodegenContext) -> bool {
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    render(a) == render(b)
}

/// `subject(arg)` for a single-arg call to `subject_src` — returns the argument.
fn subject_call_arg<'a>(e: &'a Spanned<Expr>, subject_src: &str) -> Option<&'a Spanned<Expr>> {
    let Expr::FnCall(callee, args) = &e.node else {
        return None;
    };
    if expr_dotted_name(callee).as_deref() != Some(subject_src) || args.len() != 1 {
        return None;
    }
    Some(&args[0])
}

/// The two match arms of a recursive subject, classified into the NON-recursive
/// base arm and the recursive step arm (the one containing a call to `subject`).
/// Returns `(base_body, step_body, recursive_call_arg)`.
fn classify_arms<'a>(
    subj_fd: &'a FnDef,
    subject_src: &str,
) -> Option<(&'a Spanned<Expr>, &'a Spanned<Expr>, &'a Spanned<Expr>)> {
    let [Stmt::Expr(body)] = subj_fd.body.stmts() else {
        return None;
    };
    let Expr::Match { arms, .. } = &body.node else {
        return None;
    };
    if arms.len() != 2 {
        return None;
    }
    let recursive_arg = |e: &'a Spanned<Expr>| -> Option<&'a Spanned<Expr>> {
        let mut found = None;
        collect_subject_call(e, subject_src, &mut found);
        found
    };
    let (mut base, mut step, mut rec_arg) = (None, None, None);
    for arm in arms {
        if let Some(arg) = recursive_arg(&arm.body) {
            step = Some(arm.body.as_ref());
            rec_arg = Some(arg);
        } else {
            base = Some(arm.body.as_ref());
        }
    }
    Some((base?, step?, rec_arg?))
}

/// First `subject(arg)` call reachable in `e` (the subject's recursive call).
fn collect_subject_call<'a>(
    e: &'a Spanned<Expr>,
    subject_src: &str,
    out: &mut Option<&'a Spanned<Expr>>,
) {
    if out.is_some() {
        return;
    }
    if let Some(arg) = subject_call_arg(e, subject_src) {
        *out = Some(arg);
        return;
    }
    match &e.node {
        Expr::FnCall(callee, args) => {
            collect_subject_call(callee, subject_src, out);
            for a in args {
                collect_subject_call(a, subject_src, out);
            }
        }
        Expr::BinOp(_, l, r) => {
            collect_subject_call(l, subject_src, out);
            collect_subject_call(r, subject_src, out);
        }
        Expr::Neg(x) | Expr::Attr(x, _) | Expr::ErrorProp(x) => {
            collect_subject_call(x, subject_src, out)
        }
        Expr::Constructor(_, Some(x)) => collect_subject_call(x, subject_src, out),
        _ => {}
    }
}

fn expr_mentions_param(expr: &Spanned<Expr>, param_name: &str, param_slot: Option<u16>) -> bool {
    match &expr.node {
        Expr::Ident(name) => name == param_name,
        Expr::Resolved { slot, name, .. } => param_slot
            .map(|param_slot| *slot == param_slot)
            .unwrap_or(name == param_name),
        Expr::Attr(inner, _) | Expr::ErrorProp(inner) | Expr::Neg(inner) => {
            expr_mentions_param(inner, param_name, param_slot)
        }
        Expr::FnCall(callee, args) => {
            expr_mentions_param(callee, param_name, param_slot)
                || args
                    .iter()
                    .any(|arg| expr_mentions_param(arg, param_name, param_slot))
        }
        Expr::BinOp(_, l, r) => {
            expr_mentions_param(l, param_name, param_slot)
                || expr_mentions_param(r, param_name, param_slot)
        }
        Expr::Match { subject, arms } => {
            expr_mentions_param(subject, param_name, param_slot)
                || arms
                    .iter()
                    .any(|arm| expr_mentions_param(&arm.body, param_name, param_slot))
        }
        Expr::Constructor(_, Some(arg)) => expr_mentions_param(arg, param_name, param_slot),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            crate::ast::StrPart::Literal(_) => false,
            crate::ast::StrPart::Parsed(inner) => {
                expr_mentions_param(inner, param_name, param_slot)
            }
        }),
        Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => items
            .iter()
            .any(|item| expr_mentions_param(item, param_name, param_slot)),
        Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
            expr_mentions_param(k, param_name, param_slot)
                || expr_mentions_param(v, param_name, param_slot)
        }),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_mentions_param(value, param_name, param_slot)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_mentions_param(base, param_name, param_slot)
                || updates
                    .iter()
                    .any(|(_, value)| expr_mentions_param(value, param_name, param_slot))
        }
        Expr::TailCall(call) => call
            .args
            .iter()
            .any(|arg| expr_mentions_param(arg, param_name, param_slot)),
        Expr::Literal(_) | Expr::Constructor(_, None) => false,
    }
}

/// Whether `step_body` is `HEAD <op2> subject(REC)` (or `subject(REC) <op2> HEAD`)
/// where `HEAD` is constant with respect to the subject's recursion parameter.
/// This is the load-bearing content-blind gate that the subject's recurrence
/// combines its sub-result with the SAME operator the law claims for `OP2`.
/// Without it the induction step's associativity finisher would not apply.
fn step_head_is_op2(
    step_body: &Spanned<Expr>,
    op2: Op2,
    subject_src: &str,
    param_name: &str,
    param_slot: Option<u16>,
) -> bool {
    let Expr::BinOp(op, l, r) = &step_body.node else {
        return false;
    };
    if Op2::from_binop(*op) != Some(op2) {
        return false;
    }
    let is_rec = |e: &Spanned<Expr>| subject_call_arg(e, subject_src).is_some();
    match (is_rec(l), is_rec(r)) {
        (true, false) => !expr_mentions_param(r, param_name, param_slot),
        (false, true) => !expr_mentions_param(l, param_name, param_slot),
        _ => false,
    }
}

/// Whether `e` is the integer literal `v`.
fn is_int_lit(e: &Spanned<Expr>, v: i64) -> bool {
    matches!(&e.node, Expr::Literal(Literal::Int(n)) if *n == v)
}

/// Recognize the homomorphism shape `subject(OP1(a, b)) = OP2(subject(a),
/// subject(b))`, capturing `subject` / `OP1` / `OP2` from the AST. Returns
/// `None` (decline) unless every structural gate holds, so a non-homomorphism
/// law is never claimed.
fn recognize(_vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Option<HomShape> {
    // rhs = OP2(subject a, subject b), OP2 a captured binop.
    let Expr::BinOp(op2_binop, ra, rb) = &law.rhs.node else {
        return None;
    };
    let op2 = Op2::from_binop(*op2_binop)?;

    // Subject is the callee shared by both rhs operands; capture it from one and
    // require the other to match (name-blind in the subject).
    let Expr::FnCall(ra_callee, ra_args) = &ra.node else {
        return None;
    };
    if ra_args.len() != 1 {
        return None;
    }
    let subject_src = expr_dotted_name(ra_callee)?;
    let a = &ra_args[0];
    let b = subject_call_arg(rb, &subject_src)?;

    // lhs = subject(OP1(a, b)).
    let lhs_arg = subject_call_arg(&law.lhs, &subject_src)?;

    // OP1 captured from the subject's single argument: either `a + b` or a 2-arg
    // recursive combine `cat(a, b)`. The operands must be the SAME atoms the rhs
    // applies the subject to.
    let op1 = match &lhs_arg.node {
        Expr::BinOp(BinOp::Add, x, y) if same_atom(x, a, ctx) && same_atom(y, b, ctx) => Op1::Add,
        Expr::FnCall(cat_callee, cat_args) if cat_args.len() == 2 => {
            if !same_atom(&cat_args[0], a, ctx) || !same_atom(&cat_args[1], b, ctx) {
                return None;
            }
            let cat_src = expr_dotted_name(cat_callee)?;
            let cat_short = cat_src.rsplit('.').next().unwrap_or(&cat_src).to_string();
            if !recursive_pure_fn_names(ctx).contains(&cat_short) {
                return None;
            }
            Op1::Concat(cat_src)
        }
        _ => return None,
    };

    // Subject must be recursive (so `.induct` exists).
    let subject_short = subject_src
        .rsplit('.')
        .next()
        .unwrap_or(&subject_src)
        .to_string();
    if !recursive_pure_fn_names(ctx).contains(&subject_short) {
        return None;
    }
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if !subj_fd.effects.is_empty() {
        return None;
    }
    let (param_name, _) = subj_fd.params.first()?;
    let param_slot = subj_fd
        .resolution
        .as_ref()
        .and_then(|resolution| resolution.local_slots.get(param_name).copied())
        .or(Some(0));

    // Classify the subject's two arms; gate base = OP2.identity and step's head
    // operator = OP2 (the homomorphism algebra).
    let (base_body, step_body, _rec_arg) = classify_arms(subj_fd, &subject_src)?;
    if !is_int_lit(base_body, op2.identity_int()) {
        return None;
    }
    if !step_head_is_op2(step_body, op2, &subject_src, param_name, param_slot) {
        return None;
    }

    // Recurrence-shape dispatch. A guarded `p <= 0` countdown pairs with `OP1 = +`
    // (arithmetic peel, omega-normalized); a structural ADT recurrence pairs with
    // `OP1 = cat` (constructor peel under `simp_all`).
    let recurrence = match (
        super::recursive_mono::recursive_decrement_arms(subj_fd, ctx),
        &op1,
    ) {
        (Some((base_arm, step_arm)), Op1::Add) => Recurrence::Guarded { base_arm, step_arm },
        (None, Op1::Concat(_)) => Recurrence::Structural,
        _ => return None,
    };

    Some(HomShape {
        subject_src,
        op1,
        op2,
        arg0: a.clone(),
        recurrence,
    })
}

/// Statement-builder hook: whether the homomorphism emit will close this law
/// universally (so the caller drops the sampled domain and classes it
/// `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_homomorphism(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize(vb, law, ctx).is_some()
}

/// Close a homomorphism law with the de-risked content-blind skeleton. Emits its
/// OWN TRUE-universal theorem (`replaces_theorem`) under a `first | (…) | sorry`
/// floor.
pub(super) fn emit_homomorphism_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let shape = recognize(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let subj = aver_name_to_lean(&shape.subject_src);
    let lhs = render(&law.lhs);
    let rhs = render(&law.rhs);
    let assoc = shape.op2.assoc_lemma();

    let text = match &shape.recurrence {
        Recurrence::Guarded { base_arm, step_arm } => {
            // `OP1 = +`, guarded `Int` countdown. The induction is on the first
            // exponent; the peel `(m + n) - 1 = (m - 1) + n` is omega-normalized
            // and each unfold is a single guarded `rw`. `OP2`'s left-identity
            // base goal (`subject n = id <op2> subject n`) closes by `omega`
            // (the multiplier/addend is the literal `0`/`1`).
            let op2sym = shape.op2.symbol();
            let when = render(law.when.as_ref()?);
            let givens: Vec<String> = law
                .givens
                .iter()
                .map(|g| aver_name_to_lean(&g.name))
                .collect();
            let (g0, g1) = (givens.first()?, givens.get(1)?);
            // Deterministic rewrite chain (no `sorry` floor): the recognizer's
            // gates (subject's base arm = OP2 identity, its step head operator =
            // OP2) guarantee every rewrite closes, exactly as the content-aware
            // template this rung replaces did. A structural surprise surfaces as a
            // loud build error, never silent credit.
            format!(
                r#"theorem {base}__hom_lo (n : Int) (h : n <= 0) : {subj} n = {base_arm} := by
  rw [{subj}.eq_def, if_pos h]
theorem {base}__hom_hi (n : Int) (h : ¬n <= 0) : {subj} n = {step_arm} := by
  rw [{subj}.eq_def, if_neg h]
theorem {base}__hom_add (m n : Int) (hn : 0 <= n) (hm : 0 <= m) :
    {subj} (m + n) = {subj} m {op2sym} {subj} n := by
  induction m using {subj}.induct with
  | case1 m h =>
      have hm0 : m = 0 := Int.le_antisymm h hm
      subst hm0
      rw [show (0 : Int) + n = n by omega, {base}__hom_lo 0 (by omega)]
      omega
  | case2 m h ih =>
      rw [{base}__hom_hi (m + n) (by omega), {base}__hom_hi m (by omega),
          show m + n - 1 = (m - 1) + n by omega, ih (by omega)]
      rw [{assoc}]
theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = {rhs} := by
  intro {g0} {g1} h_when
  simp only [Bool.and_eq_true, decide_eq_true_eq, ge_iff_le] at h_when
  exact {base}__hom_add {g0} {g1} h_when.2 h_when.1"#,
                base = theorem_base,
            )
        }
        Recurrence::Structural => {
            // `OP1 = cat`, structural ADT recurrence. The induction is on the
            // first list; `simp_all only [OP1, subject]` unfolds both sides and
            // folds the IH (terminating — structural). The base closes by `omega`
            // (the `OP2` left-identity over the unfolded base) and the step by the
            // `OP2` associativity, tried as a small portfolio.
            let arg0 = render(&shape.arg0);
            let intros: Vec<String> = law
                .givens
                .iter()
                .map(|g| aver_name_to_lean(&g.name))
                .collect();
            let mut unfold: Vec<String> = law_simp_defs(ctx, vb, law).into_iter().collect();
            if let Op1::Concat(cat_src) = &shape.op1 {
                let cat = super::shared::entry_qualified_lean_name(ctx, cat_src);
                if !unfold.contains(&cat) {
                    unfold.push(cat);
                }
            }
            let unfold_set = unfold.join(", ");
            format!(
                r#"theorem {base} : ∀ {quant_params}, {lhs} = {rhs} := by
  intro {intros}
  induction {arg0} using {subj}.induct <;>
    first
    | (simp_all only [{unfold_set}]; omega)
    | (simp_all only [{unfold_set}]; rw [{assoc}])
    | (simp_all only [{unfold_set}, {assoc}])
    | sorry"#,
                base = theorem_base,
                intros = intros.join(" "),
            )
        }
    };

    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}
