//! Lean renderer for the generic rational-order TRANSITIVITY-CHAIN rung.
//!
//! Where `frac_order_chain` closes the ONE reciprocal-magnitude *envelope*
//! composition (Lemma 8.2.4: a `<=` conclusion `isNonNeg (minus (2^BIG) A)`
//! assembled from a scale-by-two + doubling + monotonicity citation), THIS rung
//! closes the complementary, broader shape: a canonical STRICT Fraction
//! comparison `lessThan L R` whose premises already spell out a short
//! comparison CHAIN linking the two conclusion endpoints
//!
//! ```text
//!   L  <  n1  (<|<=)  n2  (<|<=)  ...  R
//! ```
//!
//! Its first consumer is Lemma 8.1.1 (p.26): after two rounded Newton steps the
//! reciprocal error `|d·sd2 − 1|` is `< 2^-28`, which the paper proves as the
//! two-step strict transitivity `|d·sd2 − 1| < a1²+s2 < finalErrorBudget <
//! 2^-28` — two premise links plus one GROUND link (a comparison of the two
//! nullary error-budget literals, closed by `decide`).
//!
//! ## What is generic here
//!
//! - The recognizer is name-blind and AST-keyed: it reads the STRICT comparison
//!   `lessThan L R` off the subject fn body, then searches the flattened
//!   premises for a length-1..3 chain of order links (`lessThan X Y` = strict
//!   edge `X→Y`; `isNonNeg (minus Y X)` = non-strict edge `X→Y`) whose endpoints
//!   render-match `L` and `R`. A missing top link between two GROUND (closed,
//!   no quantified-variable) nodes is supplied as a `decide` ground link — also
//!   an edge. Nothing keys on a law, given, or figure name.
//! - The emitted proof is a self-contained kit of GENERIC, kernel-checked,
//!   core-Lean (no Mathlib, no `native_decide`) helper lemmas about the exact
//!   rational order — `sq_nonneg`/`sq_pos`, `frac_lt_imp_le` (`<` ⟹ `<=`),
//!   `frac_le_trans` (`<=` transitivity, `grind`-found ring identity + positive
//!   square cancel), `lessThan_right_bottom_ne` (a strict comparison forces its
//!   right denominator nonzero), and `frac_lt_le_trans` (`a<b`, `b<=c` ⟹ `a<c`)
//!   — plus a fixed have-sequence assembler: keep the first (strict) link, fold
//!   the rest into a single `<=` bound via `frac_le_trans`, and close with
//!   `frac_lt_le_trans`. No pool law is cited; the assembly cites only these
//!   self-emitted helpers and `decide` on ground links.
//!
//! ## Admission boundary (kept narrow so the emit always closes what it admits)
//!
//! - Conclusion is STRICT `lessThan L R` (the `<=` envelope shape belongs to
//!   `frac_order_chain` / `frac_monotone_compose`; disjoint conclusion heads).
//! - The FIRST chain link (from `L`) is a strict premise edge, so the strict
//!   conclusion is carried by `frac_lt_le_trans`.
//! - Every chain node from the SECOND onward is GROUND (closed): their
//!   denominator-nonzero side conditions are discharged by `decide`. The lower
//!   endpoint `L` and the first interior node `n1` may carry quantified
//!   variables (`n1`'s nonzero guard is derived from the first strict link).
//!
//! The whole assembly sits under a `first | (…) | sorry` floor: a shape the
//! recognizer admits but whose closing slips falls to a caught `sorry`, so
//! credit stays fail-closed behind the `#print axioms` whitelist and a passing
//! project never turns red.

use super::AutoProof;
use super::aver_name_to_lean;
use super::shared::{expr_dotted_name, find_fn_def_by_call_name};
use crate::ast::{Expr, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// `(short_callee_name, args)` when `expr` is a function call. The short name is
/// the last dotted component, so a qualified `Domain.Rational.lessThan` call
/// matches the primitive `lessThan` — keyed on the rational ALGEBRA primitive.
fn as_call(expr: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    let Expr::FnCall(callee, args) = &expr.node else {
        return None;
    };
    let dotted = expr_dotted_name(callee)?;
    let short = dotted.rsplit('.').next().unwrap_or(&dotted).to_string();
    Some((short, args.as_slice()))
}

/// A call to the named primitive (matched on its SHORT name) with exactly `n`
/// args, returning the args.
fn call_named<'a>(expr: &'a Spanned<Expr>, name: &str, n: usize) -> Option<&'a [Spanned<Expr>]> {
    let (short, args) = as_call(expr)?;
    (short == name && args.len() == n).then_some(args)
}

/// Flatten a left-nested `Bool.and(Bool.and(…), w)` `when` tree into conjuncts.
fn flatten_and<'a>(expr: &'a Spanned<Expr>, out: &mut Vec<&'a Spanned<Expr>>) {
    if let Some(args) = call_named(expr, "and", 2) {
        flatten_and(&args[0], out);
        flatten_and(&args[1], out);
    } else {
        out.push(expr);
    }
}

/// Deep-clone `e`, replacing any free `Ident`/`Resolved` named in `map` by the
/// mapped expression — substitutes the subject fn's parameters by the law call's
/// argument terms.
fn substitute(
    e: &Spanned<Expr>,
    map: &std::collections::HashMap<String, Spanned<Expr>>,
) -> Spanned<Expr> {
    let node = match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => {
            if let Some(rep) = map.get(n) {
                return rep.clone();
            }
            e.node.clone()
        }
        Expr::BinOp(op, a, b) => Expr::BinOp(
            *op,
            Box::new(substitute(a, map)),
            Box::new(substitute(b, map)),
        ),
        Expr::Neg(a) => Expr::Neg(Box::new(substitute(a, map))),
        Expr::Attr(b, f) => Expr::Attr(Box::new(substitute(b, map)), f.clone()),
        Expr::FnCall(c, args) => Expr::FnCall(
            Box::new(substitute(c, map)),
            args.iter().map(|x| substitute(x, map)).collect(),
        ),
        other => other.clone(),
    };
    Spanned::bare(node)
}

/// Whether `e` mentions any identifier in `vars` — i.e. it is NOT closed. A node
/// that mentions no quantified given is "ground": its Lean render reduces to a
/// closed term whose denominator-nonzero / comparison facts `decide` can settle.
fn mentions_var(e: &Spanned<Expr>, vars: &[String]) -> bool {
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => vars.iter().any(|v| v == n),
        Expr::BinOp(_, a, b) => mentions_var(a, vars) || mentions_var(b, vars),
        Expr::Neg(a) | Expr::Attr(a, _) | Expr::ErrorProp(a) => mentions_var(a, vars),
        Expr::FnCall(c, args) => {
            mentions_var(c, vars) || args.iter().any(|x| mentions_var(x, vars))
        }
        Expr::Match { subject, arms } => {
            mentions_var(subject, vars) || arms.iter().any(|a| mentions_var(&a.body, vars))
        }
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, v)| mentions_var(v, vars)),
        Expr::RecordUpdate { base, updates, .. } => {
            mentions_var(base, vars) || updates.iter().any(|(_, v)| mentions_var(v, vars))
        }
        Expr::List(xs) | Expr::Tuple(xs) | Expr::IndependentProduct(xs, _) => {
            xs.iter().any(|x| mentions_var(x, vars))
        }
        Expr::Constructor(_, Some(inner)) => mentions_var(inner, vars),
        _ => false,
    }
}

/// A directed order edge `left → right` collected from one premise conjunct.
struct Edge {
    left: String,
    right: String,
    right_closed: bool,
    /// `true`: strict (`lessThan`); `false`: non-strict (`isNonNeg (minus …)`).
    strict: bool,
    /// Index of the source conjunct in the flattened `when` (the hypothesis
    /// `hc{idx}` after `obtain`).
    conj_idx: usize,
}

/// One assembled chain link. The lower endpoint is recovered positionally (from
/// `lo` plus the earlier links' targets), so only the target `to` is stored.
enum Link {
    /// A strict premise edge `… < to`, hypothesis `hc{conj_idx}`.
    PremiseStrict { to: String, conj_idx: usize },
    /// A non-strict premise edge `… <= to`, hypothesis `hc{conj_idx}`.
    PremiseNonstrict { to: String, conj_idx: usize },
    /// A ground link `… < to` (both endpoints closed), closed by `decide`.
    Ground { to: String },
}

impl Link {
    fn to(&self) -> &str {
        match self {
            Link::PremiseStrict { to, .. }
            | Link::PremiseNonstrict { to, .. }
            | Link::Ground { to, .. } => to,
        }
    }
}

/// The recognized generic transitivity-chain law.
pub(super) struct FracOrderTransitivity {
    /// The conclusion's lower endpoint `L` (rendered Lean).
    lo: String,
    /// The chain links `L → n1 → … → R`.
    links: Vec<Link>,
    /// Subject fn Lean name (for the `simp only [_root_.<subject>]` unfold).
    subject: String,
    /// Rational order primitive Lean names (qualified), derived from the body.
    lessthan: String,
    isnonneg: String,
    minus: String,
    /// The number of flattened `when` conjuncts (drives the `obtain` pattern),
    /// paired with the set of conjunct indices actually referenced.
    n_conj: usize,
    used_conj: Vec<usize>,
}

/// Depth-bounded search for a chain `lo → … → hi` using premise edges plus a
/// terminal ground link. Prefers premise edges; a ground link is only taken
/// straight to `hi` when both endpoints are closed. Returns the first path of
/// length `1..=3` found.
#[allow(clippy::too_many_arguments)]
fn find_chain(
    cur: &str,
    cur_closed: bool,
    hi: &str,
    hi_closed: bool,
    edges: &[Edge],
    used: &mut Vec<bool>,
    depth: usize,
    path: &mut Vec<Link>,
) -> bool {
    if cur == hi {
        return !path.is_empty();
    }
    if depth >= 3 {
        return false;
    }
    for (ei, e) in edges.iter().enumerate() {
        if used[ei] || e.left != cur {
            continue;
        }
        used[ei] = true;
        path.push(if e.strict {
            Link::PremiseStrict {
                to: e.right.clone(),
                conj_idx: e.conj_idx,
            }
        } else {
            Link::PremiseNonstrict {
                to: e.right.clone(),
                conj_idx: e.conj_idx,
            }
        });
        if find_chain(
            &e.right,
            e.right_closed,
            hi,
            hi_closed,
            edges,
            used,
            depth + 1,
            path,
        ) {
            return true;
        }
        path.pop();
        used[ei] = false;
    }
    // Terminal ground link straight to `hi` (both closed): the top link of an
    // error-budget chain, closed by `decide`.
    if cur_closed && hi_closed && cur != hi {
        path.push(Link::Ground { to: hi.to_string() });
        return true;
    }
    false
}

/// Recognize a generic strict rational-order transitivity chain. Pure /
/// name-blind. Declines (so the law keeps its bounded sampled fallback / another
/// rung) unless every structural gate holds.
pub(super) fn recognize_frac_order_transitivity_shape(
    _vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<FracOrderTransitivity> {
    // Conditional `subject(args) holds` ⇒ `subject(args) = true`.
    law.when.as_ref()?;
    if !matches!(law.rhs.node, Expr::Literal(crate::ast::Literal::Bool(true))) {
        return None;
    }
    let Expr::FnCall(callee, call_args) = &law.lhs.node else {
        return None;
    };
    let subject_src = expr_dotted_name(callee)?;
    let subj_fd = find_fn_def_by_call_name(ctx, &subject_src)?;
    if subj_fd.return_type.trim() != "Bool"
        || subj_fd.params.len() != call_args.len()
        || !subj_fd.effects.is_empty()
    {
        return None;
    }
    // Subject body must be a canonical STRICT comparison `lessThan L R`.
    let [Stmt::Expr(body)] = subj_fd.body.stmts() else {
        return None;
    };
    let lt = call_named(body, "lessThan", 2)?;
    let Expr::FnCall(lt_callee, _) = &body.node else {
        return None;
    };
    let lessthan = aver_name_to_lean(&expr_dotted_name(lt_callee)?);
    // The rational module prefix (e.g. `Domain.Rational`), read off `lessThan`,
    // so `isNonNeg` / `minus` are the SAME module's primitives — derived, not
    // hardcoded.
    let rat_prefix = lessthan.rsplit_once('.').map(|(p, _)| p.to_string())?;
    let isnonneg = format!("{rat_prefix}.isNonNeg");
    let minus = format!("{rat_prefix}.minus");

    // Substitute subject params → the law call's argument terms so the
    // conclusion endpoints are expressed in the law's given namespace.
    let mut map: std::collections::HashMap<String, Spanned<Expr>> =
        std::collections::HashMap::new();
    for ((pname, _), arg) in subj_fd.params.iter().zip(call_args.iter()) {
        map.insert(pname.clone(), arg.clone());
    }
    let lo_e = substitute(&lt[0], &map);
    let hi_e = substitute(&lt[1], &map);

    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let lo = render(&lo_e);
    let hi = render(&hi_e);
    if lo == hi {
        return None;
    }
    let given_names: Vec<String> = law.givens.iter().map(|g| g.name.clone()).collect();
    let hi_closed = !mentions_var(&hi_e, &given_names);

    // Collect order edges from the flattened premises.
    let when = law.when.as_ref()?;
    let mut conj: Vec<&Spanned<Expr>> = Vec::new();
    flatten_and(when, &mut conj);
    let n_conj = conj.len();
    let mut edges: Vec<Edge> = Vec::new();
    for (i, c) in conj.iter().enumerate() {
        if let Some(a) = call_named(c, "lessThan", 2) {
            edges.push(Edge {
                left: render(&a[0]),
                right: render(&a[1]),
                right_closed: !mentions_var(&a[1], &given_names),
                strict: true,
                conj_idx: i,
            });
        } else if let Some(nn) = call_named(c, "isNonNeg", 1)
            && let Some(m) = call_named(&nn[0], "minus", 2)
        {
            // `isNonNeg (minus Y X)` means `X <= Y`: non-strict edge `X → Y`.
            edges.push(Edge {
                left: render(&m[1]),
                right: render(&m[0]),
                right_closed: !mentions_var(&m[0], &given_names),
                strict: false,
                conj_idx: i,
            });
        }
    }
    if edges.is_empty() {
        return None;
    }

    let mut used = vec![false; edges.len()];
    let mut links: Vec<Link> = Vec::new();
    if !find_chain(&lo, true, &hi, hi_closed, &edges, &mut used, 0, &mut links) {
        return None;
    }

    // Strict conclusion ⇒ the FIRST link must be a strict premise edge (so
    // `frac_lt_le_trans` carries the strict conclusion). A pure-ground first
    // link is declined.
    if !matches!(links.first(), Some(Link::PremiseStrict { .. })) {
        return None;
    }
    // Every node from the SECOND interior node onward (i.e. every link `to`
    // except the first link's) must be GROUND: their denominator-nonzero side
    // conditions are discharged by `decide`.
    if links.iter().skip(1).any(|l| {
        let t = l.to();
        // A link's target is ground iff no edge that PRODUCED it carried a
        // variable — for premise links check the source edge, for ground links
        // it is closed by construction.
        match l {
            Link::Ground { .. } => false,
            _ => edges
                .iter()
                .find(|e| e.right == t)
                .map(|e| !e.right_closed)
                .unwrap_or(true),
        }
    }) {
        return None;
    }

    let mut used_conj: Vec<usize> = links
        .iter()
        .filter_map(|l| match l {
            Link::PremiseStrict { conj_idx, .. } | Link::PremiseNonstrict { conj_idx, .. } => {
                Some(*conj_idx)
            }
            Link::Ground { .. } => None,
        })
        .collect();
    used_conj.sort_unstable();
    used_conj.dedup();

    Some(FracOrderTransitivity {
        lo,
        links,
        subject: aver_name_to_lean(&subject_src),
        lessthan,
        isnonneg,
        minus,
        n_conj,
        used_conj,
    })
}

/// Statement-builder hook: whether the transitivity-chain emit will close this
/// law universally (so the caller drops the sampled domain and classes it
/// `universal`, keeping statement and proof in lockstep).
pub(in crate::codegen::lean) fn recognize_frac_order_transitivity(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    recognize_frac_order_transitivity_shape(vb, law, ctx).is_some()
}

/// Close a generic strict rational-order transitivity-chain law. Emits the
/// self-contained generic order kit plus the fixed have-sequence assembler as a
/// TRUE-universal theorem (`replaces_theorem`), wrapped `first | (…) | sorry`.
pub(super) fn emit_frac_order_transitivity_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    theorem_base: &str,
    quant_params: &str,
) -> Option<AutoProof> {
    let c = recognize_frac_order_transitivity_shape(vb, law, ctx)?;
    let render = |e: &Spanned<Expr>| super::super::expr::emit_expr_legacy(e, ctx, None);
    let when = render(law.when.as_ref()?);
    let lhs = render(&law.lhs);
    let intros: Vec<String> = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect();
    let text = render_chain(theorem_base, quant_params, &intros.join(" "), &when, &lhs, &c);
    Some(AutoProof {
        support_lines: text.lines().map(|l| l.to_string()).collect(),
        body: crate::codegen::lean::tactic_ir::Tactic::raw(Vec::new()),
        replaces_theorem: true,
    })
}

/// The generic rational-order helper kit (kernel-checked, core-Lean) plus the
/// fixed have-sequence assembler. Every helper is scoped to `{base}__` so two
/// such laws in one file never collide; every primitive name is a parameter — no
/// per-figure literal.
fn render_chain(
    base: &str,
    quant_params: &str,
    intros: &str,
    when: &str,
    lhs: &str,
    c: &FracOrderTransitivity,
) -> String {
    let FracOrderTransitivity {
        lo,
        links,
        subject,
        lessthan,
        isnonneg,
        minus,
        n_conj,
        used_conj,
    } = c;
    let p = format!("{base}__");
    let kit = format!(
        r#"theorem {p}sq_nonneg (x : Int) : 0 ≤ x * x := by
  cases Int.le_total 0 x with
  | inl h => exact Int.mul_nonneg h h
  | inr h => exact Int.mul_nonneg_of_nonpos_of_nonpos h h
theorem {p}sq_pos {{x : Int}} (hx : x ≠ 0) : 0 < x * x := by
  cases Int.lt_or_gt_of_ne hx with
  | inl h => exact Int.mul_pos_of_neg_of_neg h h
  | inr h => exact Int.mul_pos h h
theorem {p}frac_lt_imp_le (a b : Fraction) (h : {lessthan} a b = true) :
    {isnonneg} ({minus} b a) = true := by
  simp only [{lessthan}, {isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at h ⊢
  have hid : (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)
           = (b.top*b.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(b.bottom*b.bottom) := by grind
  omega
theorem {p}frac_le_trans (a b cc : Fraction) (hb : b.bottom ≠ 0)
    (hab : {isnonneg} ({minus} b a) = true) (hbc : {isnonneg} ({minus} cc b) = true) :
    {isnonneg} ({minus} cc a) = true := by
  simp only [{isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hP : 0 ≤ ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom) :=
    Int.mul_nonneg hab ({p}sq_nonneg cc.bottom)
  have hQ : 0 ≤ ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom) :=
    Int.mul_nonneg hbc ({p}sq_nonneg a.bottom)
  have hid : ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom)
              + ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom)
            = (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    grind
  have hsum : 0 ≤ (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    rw [← hid]; omega
  exact Int.nonneg_of_mul_nonneg_right hsum ({p}sq_pos hb)
theorem {p}lessThan_right_bottom_ne (x y : Fraction) (h : {lessthan} x y = true) : y.bottom ≠ 0 := by
  intro h0
  simp only [{lessthan}, decide_eq_true_eq] at h
  rw [h0] at h
  simp only [Int.mul_zero, Int.zero_mul] at h
  omega
theorem {p}frac_lt_le_trans (a b cc : Fraction) (hc : cc.bottom ≠ 0)
    (hab : {lessthan} a b = true) (hbc : {isnonneg} ({minus} cc b) = true) :
    {lessthan} a cc = true := by
  have hb : b.bottom ≠ 0 := {p}lessThan_right_bottom_ne a b hab
  simp only [{lessthan}, {isnonneg}, {minus}, decide_eq_true_eq, ge_iff_le] at hab hbc ⊢
  have hab' : 0 < (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom) := by
    have hid_ab : (b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)
                = (b.top*b.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(b.bottom*b.bottom) := by grind
    omega
  have hP : 0 < ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom) :=
    Int.mul_pos hab' ({p}sq_pos hc)
  have hQ : 0 ≤ ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom) :=
    Int.mul_nonneg hbc ({p}sq_nonneg a.bottom)
  have hid : ((b.top*a.bottom - a.top*b.bottom) * (b.bottom*a.bottom)) * (cc.bottom*cc.bottom)
              + ((cc.top*b.bottom - b.top*cc.bottom) * (cc.bottom*b.bottom)) * (a.bottom*a.bottom)
            = (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    grind
  have hsum : 0 < (b.bottom*b.bottom) * ((cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)) := by
    rw [← hid]; omega
  have hid_goal : (cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom)
                = (cc.top*cc.bottom)*(a.bottom*a.bottom) - (a.top*a.bottom)*(cc.bottom*cc.bottom) := by grind
  have hT3nn : 0 ≤ (cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom) :=
    Int.nonneg_of_mul_nonneg_right (Int.le_of_lt hsum) ({p}sq_pos hb)
  have hT3ne : (cc.top*a.bottom - a.top*cc.bottom) * (cc.bottom*a.bottom) ≠ 0 := by
    intro h0; rw [h0, Int.mul_zero] at hsum; omega
  omega"#
    );

    // The `obtain` pattern over the flattened `when`: left-nested tuple, naming
    // referenced conjuncts `hc{i}` and discarding the rest with `_`.
    let mut names: Vec<String> = (0..*n_conj)
        .map(|i| {
            if used_conj.contains(&i) {
                format!("hc{i}")
            } else {
                "_".to_string()
            }
        })
        .collect();
    let mut obtain = names.remove(0);
    for nm in names {
        obtain = format!("⟨{obtain}, {nm}⟩");
    }

    // The have-sequence assembler.
    let mut steps: Vec<String> = Vec::new();
    // node[0] = lo, node[i] = links[i-1].to().
    let node = |i: usize| -> String {
        if i == 0 {
            lo.clone()
        } else {
            links[i - 1].to().to_string()
        }
    };
    let k = links.len();
    // First (strict) link hypothesis.
    let Link::PremiseStrict {
        conj_idx: first_idx,
        ..
    } = &links[0]
    else {
        unreachable!("recognizer guarantees a strict first link")
    };
    steps.push(format!("have h0 := hc{first_idx}"));

    if k == 1 {
        // Conclusion IS the single strict premise.
        steps.push("exact h0".to_string());
    } else {
        // Fold links[1..] into a single `<=` bound `n1 <= R`.
        for (j, link) in links.iter().enumerate().skip(1) {
            let from = node(j);
            let to = node(j + 1);
            match link {
                Link::PremiseStrict { conj_idx, .. } => steps.push(format!(
                    "have e{j} := {p}frac_lt_imp_le ({from}) ({to}) hc{conj_idx}"
                )),
                Link::PremiseNonstrict { conj_idx, .. } => {
                    steps.push(format!("have e{j} := hc{conj_idx}"))
                }
                Link::Ground { .. } => {
                    steps.push(format!(
                        "have g{j} : {lessthan} ({from}) ({to}) = true := by decide"
                    ));
                    steps.push(format!(
                        "have e{j} := {p}frac_lt_imp_le ({from}) ({to}) g{j}"
                    ));
                }
            }
        }
        // Fold: acc = e1, then frac_le_trans through the ground middles.
        let n1 = node(1);
        steps.push("have acc := e1".to_string());
        for j in 2..k {
            let mid = node(j);
            let nxt = node(j + 1);
            steps.push(format!(
                "have acc := {p}frac_le_trans ({n1}) ({mid}) ({nxt}) (by decide) acc e{j}"
            ));
        }
        let rr = node(k);
        steps.push(format!(
            "exact {p}frac_lt_le_trans ({lo}) ({n1}) ({rr}) (by decide) h0 acc"
        ));
    }

    let mut block: Vec<String> = Vec::new();
    block.push("     simp only [Bool.and_eq_true] at h_when".to_string());
    block.push(format!("     obtain {obtain} := h_when"));
    block.push(format!("     simp only [_root_.{subject}]"));
    for s in &steps {
        block.push(format!("     {s}"));
    }
    // Wrap the last line's closing paren onto the block.
    let last = block.len() - 1;
    block[last] = format!("{})", block[last]);

    let assembly = format!(
        "set_option maxHeartbeats 4000000 in\n\
         theorem {base} : ∀ {quant_params}, {when} = true -> {lhs} = true := by\n  \
         intro {intros} h_when\n  \
         first\n  \
         | ({body}\n  \
         | sorry",
        body = {
            // strip the leading indentation of the first block line so it sits
            // right after `| (`
            let mut it = block.iter();
            let head = it.next().unwrap().trim_start().to_string();
            let rest: Vec<String> = it.cloned().collect();
            if rest.is_empty() {
                head
            } else {
                format!("{head}\n{}", rest.join("\n"))
            }
        }
    );

    format!("{kit}\n{assembly}")
}
