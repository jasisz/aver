//! Consumption figure: a later `when`-law (the CONSUMER) proves
//! universally by applying an EARLIER lane-proved `when`-law (the
//! HELPER) at matched instantiations, instead of needing its own
//! recognized proof family.
//!
//! Recognition is the premise-subset rule:
//! 1. the helper's conclusion-LHS shape is matched into the consumer's
//!    proof cone (the consumer subject's body at the law's arguments),
//!    yielding a substitution over the helper's givens;
//! 2. the substitution is completed by matching the helper's premise
//!    conjuncts against the consumer's premise conjuncts (the ACL2
//!    free-variables discipline: every substitution image is a given
//!    bound by the consumer's own statement);
//! 3. EVERY helper premise conjunct under the substitution must be one
//!    of the consumer's premise conjuncts. A consumer missing even one
//!    conjunct DECLINES — no lane module, zero cost, never a failing
//!    build.
//!
//! The emitted proof opens with explicit `have` applications of the
//! helper's plain-logic companion (`<helper>_prop`) at the matched
//! instantiations, then unfolds the subject and rewrites with those
//! local facts. The application is ALWAYS an explicit `have` against
//! local hypotheses — never a `first | exact <companion>` alternative,
//! which would silently absorb a broken companion's axioms into a
//! green build. The fail-closed walls are the lane's own: a helper
//! that did not prove is never emitted (its absence makes the
//! consumer's import fail the tolerated build), and the consumer's
//! credit keys on its OWN per-declaration `#print axioms` line, where
//! any absorbed axiom surfaces.
//!
//! Measured subset shipped here: ONE helper per consumer (at any
//! number of instantiations), substitution images restricted to plain
//! Int given names, consumer subject = a pure non-recursive Bool fn
//! whose body is `lhs == rhs`. Compound instantiation images (helper
//! applied at product terms) are hand-validated but not yet
//! recognized.

use std::collections::BTreeMap;

use super::super::expr::{aver_name_to_lean, emit_expr_legacy};
use crate::ast::{BinOp, Expr, Literal, Spanned, Stmt, VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

use super::shared::{call_of, ident_of, when_conjuncts};
use super::{LaneLawFile, lane_module_id};

/// A source-earlier lane-emitted when-law, viewed as a helper
/// candidate for a later consumer.
pub(super) struct HelperView<'a> {
    /// Index of the helper's module in the emitted lane file list —
    /// the dependency edge the consumer records.
    pub(super) file: usize,
    /// The helper's plain-logic companion theorem name
    /// (`<fn>_law_<law>_prop`).
    pub(super) companion: String,
    /// The helper law's AST (premise conjuncts + conclusion shape).
    pub(super) law: &'a VerifyLaw,
}

/// One matched application site of the helper inside the consumer's
/// cone.
struct Instantiation {
    /// Substitution images per helper given, in helper-given order
    /// (consumer given names, Lean-rendered at emission).
    args: Vec<String>,
    /// Consumer premise-conjunct index per helper premise conjunct, in
    /// helper-conjunct order.
    hyps: Vec<usize>,
}

/// Recognized consumption: which helper, at which instantiations.
pub(super) struct ConsumptionPlan {
    pub(super) helper_file: usize,
    helper_companion: String,
    instantiations: Vec<Instantiation>,
    /// Number of consumer premise conjuncts (for the `obtain` pattern).
    conjuncts: usize,
}

/// Substitution over the helper's given names; images are consumer
/// given names (idents only — the measured subset).
type Sigma = BTreeMap<String, String>;

/// Structural equality over the law/cone expression grammar,
/// normalizing `Ident` vs `Resolved` by name and calls by dotted
/// callee name. Anything outside the grammar compares unequal (and so
/// declines upstream).
fn expr_eq(a: &Spanned<Expr>, b: &Spanned<Expr>) -> bool {
    if let (Some(x), Some(y)) = (ident_of(a), ident_of(b)) {
        return x == y;
    }
    if let (Some((ca, aa)), Some((cb, ab))) = (call_of(a), call_of(b)) {
        return ca == cb
            && aa.len() == ab.len()
            && aa.iter().zip(ab.iter()).all(|(x, y)| expr_eq(x, y));
    }
    match (&a.node, &b.node) {
        (Expr::Literal(x), Expr::Literal(y)) => x == y,
        (Expr::BinOp(o1, l1, r1), Expr::BinOp(o2, l2, r2)) => {
            o1 == o2 && expr_eq(l1, l2) && expr_eq(r1, r2)
        }
        (Expr::Neg(x), Expr::Neg(y)) => expr_eq(x, y),
        _ => false,
    }
}

/// `true` iff `e` contains a call whose callee resolves to `name`
/// (via the shared suffix-canonicalizing helper) — the non-recursion
/// gate on the consumer subject, whose proof relies on a plain
/// `unfold`.
fn calls_fn(e: &Spanned<Expr>, name: &str) -> bool {
    let targets: std::collections::HashSet<String> = std::iter::once(name.to_string()).collect();
    fn walk(e: &Spanned<Expr>, targets: &std::collections::HashSet<String>) -> bool {
        if let Some((callee, args)) = call_of(e) {
            if crate::codegen::recursion::detect::call_matches_any(&callee, targets) {
                return true;
            }
            return args.iter().any(|a| walk(a, targets));
        }
        match &e.node {
            Expr::BinOp(_, l, r) => walk(l, targets) || walk(r, targets),
            Expr::Neg(x) => walk(x, targets),
            _ => false,
        }
    }
    walk(e, &targets)
}

/// `true` iff `pat` occurs as a subterm of `e` (under [`expr_eq`]).
fn occurs_in(e: &Spanned<Expr>, pat: &Spanned<Expr>) -> bool {
    if expr_eq(e, pat) {
        return true;
    }
    match &e.node {
        Expr::BinOp(_, l, r) => occurs_in(l, pat) || occurs_in(r, pat),
        Expr::Neg(x) => occurs_in(x, pat),
        Expr::FnCall(_, args) => args.iter().any(|a| occurs_in(a, pat)),
        Expr::TailCall(data) => data.args.iter().any(|a| occurs_in(a, pat)),
        _ => false,
    }
}

/// Clone `e` with every occurrence of `pat` (under [`expr_eq`])
/// replaced by `repl` — the emission-time simulation of what `rw`
/// will do to the goal, so non-closing shapes decline instead of
/// failing a tolerated build.
fn replace_all(e: &Spanned<Expr>, pat: &Spanned<Expr>, repl: &Spanned<Expr>) -> Spanned<Expr> {
    if expr_eq(e, pat) {
        return repl.clone();
    }
    let node = match &e.node {
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(replace_all(l, pat, repl)),
            Box::new(replace_all(r, pat, repl)),
        ),
        Expr::Neg(x) => Expr::Neg(Box::new(replace_all(x, pat, repl))),
        Expr::FnCall(callee, args) => Expr::FnCall(
            callee.clone(),
            args.iter().map(|a| replace_all(a, pat, repl)).collect(),
        ),
        other => other.clone(),
    };
    Spanned::new(node, e.line)
}

/// Clone a law-side expression with the substitution applied to its
/// given idents.
fn apply_sigma(e: &Spanned<Expr>, sigma: &Sigma) -> Spanned<Expr> {
    if let Some(name) = ident_of(e)
        && let Some(image) = sigma.get(name)
    {
        return Spanned::new(Expr::Ident(image.clone()), e.line);
    }
    let node = match &e.node {
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(apply_sigma(l, sigma)),
            Box::new(apply_sigma(r, sigma)),
        ),
        Expr::Neg(x) => Expr::Neg(Box::new(apply_sigma(x, sigma))),
        Expr::FnCall(callee, args) => Expr::FnCall(
            callee.clone(),
            args.iter().map(|a| apply_sigma(a, sigma)).collect(),
        ),
        other => other.clone(),
    };
    Spanned::new(node, e.line)
}

/// Clone the subject body with each param occurrence replaced by the
/// law-call argument name. `None` when the body leaves the supported
/// grammar (literals, idents, comparisons/arithmetic, plain calls).
fn substitute_params(e: &Spanned<Expr>, map: &BTreeMap<&str, &str>) -> Option<Spanned<Expr>> {
    if let Some(name) = ident_of(e) {
        let node = match map.get(name) {
            Some(arg) => Expr::Ident((*arg).to_string()),
            None => Expr::Ident(name.to_string()),
        };
        return Some(Spanned::new(node, e.line));
    }
    let node = match &e.node {
        Expr::Literal(_) => e.node.clone(),
        Expr::BinOp(op, l, r) => Expr::BinOp(
            *op,
            Box::new(substitute_params(l, map)?),
            Box::new(substitute_params(r, map)?),
        ),
        Expr::Neg(x) => Expr::Neg(Box::new(substitute_params(x, map)?)),
        Expr::FnCall(callee, args) => Expr::FnCall(
            callee.clone(),
            args.iter()
                .map(|a| substitute_params(a, map))
                .collect::<Option<Vec<_>>>()?,
        ),
        _ => return None,
    };
    Some(Spanned::new(node, e.line))
}

/// Match the helper-side shape `pat` against the consumer-side term
/// `target`, binding helper givens (`vars`) to consumer given idents
/// (`images`) in `sigma`. Consistent rebinding required; everything
/// else is structural.
fn match_shape(
    pat: &Spanned<Expr>,
    target: &Spanned<Expr>,
    vars: &[&str],
    images: &[&str],
    sigma: &mut Sigma,
) -> bool {
    if let Some(v) = ident_of(pat)
        && vars.contains(&v)
    {
        let Some(image) = ident_of(target) else {
            return false;
        };
        if !images.contains(&image) {
            return false;
        }
        return match sigma.get(v) {
            Some(prev) => prev == image,
            None => {
                sigma.insert(v.to_string(), image.to_string());
                true
            }
        };
    }
    if let (Some((cp, ap)), Some((ct, at))) = (call_of(pat), call_of(target)) {
        return cp == ct
            && ap.len() == at.len()
            && ap
                .iter()
                .zip(at.iter())
                .all(|(p, t)| match_shape(p, t, vars, images, sigma));
    }
    match (&pat.node, &target.node) {
        (Expr::Literal(x), Expr::Literal(y)) => x == y,
        (Expr::BinOp(o1, l1, r1), Expr::BinOp(o2, l2, r2)) => {
            o1 == o2
                && match_shape(l1, l2, vars, images, sigma)
                && match_shape(r1, r2, vars, images, sigma)
        }
        (Expr::Neg(x), Expr::Neg(y)) => match_shape(x, y, vars, images, sigma),
        _ => ident_of(pat).is_some() && ident_of(pat) == ident_of(target),
    }
}

/// The premise-subset completion: map every helper conjunct (in
/// order) onto SOME consumer conjunct under `sigma`, extending `sigma`
/// for helper givens the conclusion match left unbound. Small
/// backtracking search; first full assignment wins.
fn complete_premises(
    h_conj: &[&Spanned<Expr>],
    t_conj: &[&Spanned<Expr>],
    vars: &[&str],
    images: &[&str],
    sigma: &Sigma,
) -> Option<(Sigma, Vec<usize>)> {
    fn go(
        i: usize,
        h_conj: &[&Spanned<Expr>],
        t_conj: &[&Spanned<Expr>],
        vars: &[&str],
        images: &[&str],
        sigma: &Sigma,
        picked: &mut Vec<usize>,
    ) -> Option<Sigma> {
        let Some(pat) = h_conj.get(i) else {
            return Some(sigma.clone());
        };
        for (j, cand) in t_conj.iter().enumerate() {
            let mut next = sigma.clone();
            if match_shape(pat, cand, vars, images, &mut next) {
                picked.push(j);
                if let Some(done) = go(i + 1, h_conj, t_conj, vars, images, &next, picked) {
                    return Some(done);
                }
                picked.pop();
            }
        }
        None
    }
    let mut picked = Vec::new();
    let done = go(0, h_conj, t_conj, vars, images, sigma, &mut picked)?;
    Some((done, picked))
}

/// Try to match one side of the consumer's cone against the helper's
/// conclusion-LHS, then complete the substitution from the premises.
fn match_side(
    side: &Spanned<Expr>,
    helper_lhs: &Spanned<Expr>,
    h_conj: &[&Spanned<Expr>],
    t_conj: &[&Spanned<Expr>],
    vars: &[&str],
    images: &[&str],
) -> Option<(Sigma, Vec<usize>)> {
    let mut sigma = Sigma::new();
    if !match_shape(helper_lhs, side, vars, images, &mut sigma) {
        return None;
    }
    let (sigma, hyps) = complete_premises(h_conj, t_conj, vars, images, &sigma)?;
    // Every helper given must be bound (the instantiation must be a
    // closed application).
    if vars.iter().any(|v| !sigma.contains_key(*v)) {
        return None;
    }
    Some((sigma, hyps))
}

/// Recognize a consumer law against the source-earlier helpers. First
/// matching helper wins (single-helper consumption — the measured
/// subset).
pub(super) fn classify_consumption(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    helpers: &[HelperView],
) -> Option<ConsumptionPlan> {
    let when = law.when.as_ref()?;
    if law.givens.is_empty() || law.givens.iter().any(|g| g.type_name != "Int") {
        return None;
    }
    if !matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        return None;
    }
    // Claim: a plain call of the subject over given idents.
    let (callee, call_args) = call_of(&law.lhs)?;
    if callee.rsplit('.').next()? != vb.fn_name {
        return None;
    }
    let given_names: Vec<&str> = law.givens.iter().map(|g| g.name.as_str()).collect();
    let mut arg_names = Vec::with_capacity(call_args.len());
    for arg in call_args {
        let name = ident_of(arg)?;
        if !given_names.contains(&name) {
            return None;
        }
        arg_names.push(name);
    }
    // Subject: pure, Bool-valued, non-recursive, body `lhs == rhs`.
    let fd = ctx.fn_def_by_name(&vb.fn_name, None)?;
    if !fd.effects.is_empty() || fd.params.len() != arg_names.len() || fd.return_type != "Bool" {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::BinOp(BinOp::Eq, body_l, body_r) = &body.node else {
        return None;
    };
    if calls_fn(body, &vb.fn_name) {
        return None;
    }
    // The cone: the subject body at the law's arguments.
    let param_map: BTreeMap<&str, &str> = fd
        .params
        .iter()
        .zip(arg_names.iter())
        .map(|((p, _), a)| (p.as_str(), *a))
        .collect();
    let left = substitute_params(body_l, &param_map)?;
    let right = substitute_params(body_r, &param_map)?;
    // Consumer premise conjuncts (right-nested conjunction, >= 2 — the
    // single-atom premise renders at a different Bool/Prop seam and is
    // not in the measured subset).
    let t_conj = when_conjuncts(when)?;
    if t_conj.len() < 2 {
        return None;
    }
    // Proof-local binder hygiene: declines instead of shadowing.
    let reserved: Vec<String> = std::iter::once("h_when".to_string())
        .chain((0..t_conj.len()).map(|i| format!("h{i}")))
        .chain((0..2).map(|i| format!("hh{i}")))
        .collect();
    if given_names.iter().any(|g| reserved.iter().any(|r| r == g)) {
        return None;
    }

    for helper in helpers {
        let Some(h_when) = helper.law.when.as_ref() else {
            continue;
        };
        if helper.law.givens.iter().any(|g| g.type_name != "Int") {
            continue;
        }
        if !matches!(&helper.law.rhs.node, Expr::Literal(Literal::Bool(true))) {
            continue;
        }
        // Helper conclusion must be an equation pinned `=> true` — its
        // companion then concludes the plain equality `lhs = rhs` the
        // consumer rewrites with.
        let Expr::BinOp(BinOp::Eq, h_lhs, h_rhs) = &helper.law.lhs.node else {
            continue;
        };
        let Some(h_conj) = when_conjuncts(h_when) else {
            continue;
        };
        let h_vars: Vec<&str> = helper.law.givens.iter().map(|g| g.name.as_str()).collect();

        let mut sites: Vec<(Sigma, Vec<usize>)> = Vec::new();
        for side in [&left, &right] {
            if let Some(site) = match_side(side, h_lhs, &h_conj, &t_conj, &h_vars, &given_names)
                && !sites.iter().any(|(s, _)| s == &site.0)
            {
                sites.push(site);
            }
        }
        if sites.is_empty() {
            continue;
        }
        // Simulate the rewrites: after replacing every matched
        // conclusion-LHS image with its RHS image, both sides must be
        // syntactically identical (`simp` then closes the reflexive
        // `==`). Otherwise decline — never emit a non-closing proof.
        let mut sim_l = left.clone();
        let mut sim_r = right.clone();
        let mut used: Vec<&(Sigma, Vec<usize>)> = Vec::new();
        for site in &sites {
            let pat = apply_sigma(h_lhs, &site.0);
            let repl = apply_sigma(h_rhs, &site.0);
            if !occurs_in(&sim_l, &pat) && !occurs_in(&sim_r, &pat) {
                continue;
            }
            sim_l = replace_all(&sim_l, &pat, &repl);
            sim_r = replace_all(&sim_r, &pat, &repl);
            used.push(site);
        }
        if used.is_empty() || !expr_eq(&sim_l, &sim_r) {
            continue;
        }
        let instantiations = used
            .into_iter()
            .map(|(sigma, hyps)| Instantiation {
                args: h_vars
                    .iter()
                    .map(|v| sigma.get(*v).expect("bound by match_side").clone())
                    .collect(),
                hyps: hyps.clone(),
            })
            .collect();
        return Some(ConsumptionPlan {
            helper_file: helper.file,
            helper_companion: helper.companion.clone(),
            instantiations,
            conjuncts: t_conj.len(),
        });
    }
    None
}

pub(super) fn render_consumption(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    plan: &ConsumptionPlan,
    entry_root: &str,
    entry_content: &str,
    sabotage: bool,
) -> Option<LaneLawFile> {
    let emit = |e: &Spanned<Expr>| emit_expr_legacy(e, ctx, None);

    let fn_lean = aver_name_to_lean(&vb.fn_name);
    let law_lean = aver_name_to_lean(&law.name);
    let theorem_base = format!("{fn_lean}_law_{law_lean}");
    let theorem = format!("{theorem_base}_universal");

    let lhs_template = emit(&law.lhs);
    let rhs_template = emit(&law.rhs).replace('\n', " ");
    let when_template = emit(law.when.as_ref()?).replace('\n', " ");

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

    let sabotage_line = if sabotage {
        // TEST-ONLY (`AVER_PROOF_LANE_SABOTAGE`): see the twin comment
        // in the other renderers.
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
         -- Classical.choice, Quot.sound).\n\
         -- This law's proof CONSUMES an earlier proven law of this file: it\n\
         -- imports that law's lane module and applies its plain-logic\n\
         -- companion via explicit `have` at the matched instantiations. If\n\
         -- the helper failed to prove, its module does not build and this\n\
         -- module's tolerated build fails with it — both stay bounded.\n",
        vb.fn_name, law.name,
    ));
    content.push_str(&format!("import {entry_root}\n\n"));
    content.push_str("set_option linter.unusedVariables false\n\n");
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
    let obtain_pat: Vec<String> = (0..plan.conjuncts).map(|i| format!("h{i}")).collect();
    content.push_str(&format!("  obtain ⟨{}⟩ := h_when\n", obtain_pat.join(", ")));
    for (i, inst) in plan.instantiations.iter().enumerate() {
        let args: Vec<String> = inst.args.iter().map(|a| aver_name_to_lean(a)).collect();
        let hyps: Vec<String> = inst.hyps.iter().map(|j| format!("h{j}")).collect();
        content.push_str(&format!(
            "  have hh{i} := {} {} {}\n",
            plan.helper_companion,
            args.join(" "),
            hyps.join(" ")
        ));
    }
    content.push_str(&format!("  unfold {fn_lean}\n"));
    let rw_list: Vec<String> = (0..plan.instantiations.len())
        .map(|i| format!("hh{i}"))
        .collect();
    content.push_str(&format!("  rw [{}]\n", rw_list.join(", ")));
    content.push_str("  simp\n");

    // Prop-form corollary, derived from the twin above — same module,
    // no separate credit; a later law may consume THIS law through it.
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

    // The helper import (and the module-hash folding of the helper's
    // content) is wired by the caller via `with_lane_imports`, which
    // also records the dependency edge in the lane index.
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

#[cfg(test)]
mod tests {
    use super::*;

    fn sp(node: Expr) -> Spanned<Expr> {
        Spanned::new(node, 0)
    }
    fn ident(n: &str) -> Spanned<Expr> {
        sp(Expr::Ident(n.to_string()))
    }
    fn int(v: i64) -> Spanned<Expr> {
        sp(Expr::Literal(Literal::Int(v)))
    }
    fn bin(op: BinOp, l: Spanned<Expr>, r: Spanned<Expr>) -> Spanned<Expr> {
        sp(Expr::BinOp(op, Box::new(l), Box::new(r)))
    }
    fn call(name: &str, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        sp(Expr::FnCall(Box::new(ident(name)), args))
    }

    /// Conclusion-LHS matching binds the helper's givens through
    /// nested structure, consistently.
    #[test]
    fn match_shape_binds_through_products() {
        // pattern: f(a, b * d) — target: f(xn, xs * scale)
        let pat = call(
            "binFloor",
            vec![ident("a"), bin(BinOp::Mul, ident("b"), ident("d"))],
        );
        let target = call(
            "binFloor",
            vec![ident("xn"), bin(BinOp::Mul, ident("xs"), ident("scale"))],
        );
        let vars = ["a", "b", "d", "w"];
        let images = ["xn", "xs", "scale", "cell"];
        let mut sigma = Sigma::new();
        assert!(match_shape(&pat, &target, &vars, &images, &mut sigma));
        assert_eq!(sigma.get("a").map(String::as_str), Some("xn"));
        assert_eq!(sigma.get("b").map(String::as_str), Some("xs"));
        assert_eq!(sigma.get("d").map(String::as_str), Some("scale"));
        // a different callee never matches
        let other = call(
            "otherFloor",
            vec![ident("xn"), bin(BinOp::Mul, ident("xs"), ident("scale"))],
        );
        let mut sigma2 = Sigma::new();
        assert!(!match_shape(&pat, &other, &vars, &images, &mut sigma2));
        // an image outside the consumer's givens never binds
        let stray = call(
            "binFloor",
            vec![ident("zz"), bin(BinOp::Mul, ident("xs"), ident("scale"))],
        );
        let mut sigma3 = Sigma::new();
        assert!(!match_shape(&pat, &stray, &vars, &images, &mut sigma3));
    }

    /// Premise completion binds the helper givens the conclusion left
    /// free (the cell index) and records which consumer conjunct pays
    /// for each helper conjunct.
    #[test]
    fn premise_subset_completes_free_given_and_maps_indices() {
        // helper premises: b > 0; d > 0; w*b < a; a < (w+1)*b
        let h0 = bin(BinOp::Gt, ident("b"), int(0));
        let h1 = bin(BinOp::Gt, ident("d"), int(0));
        let h2 = bin(
            BinOp::Lt,
            bin(BinOp::Mul, ident("w"), ident("b")),
            ident("a"),
        );
        let h3 = bin(
            BinOp::Lt,
            ident("a"),
            bin(BinOp::Mul, bin(BinOp::Add, ident("w"), int(1)), ident("b")),
        );
        let h_conj_owned = [h0, h1, h2, h3];
        let h_conj: Vec<&Spanned<Expr>> = h_conj_owned.iter().collect();
        // consumer premises: xs > 0; scale > 0; cell*xs < xn; xn < (cell+1)*xs
        let t0 = bin(BinOp::Gt, ident("xs"), int(0));
        let t1 = bin(BinOp::Gt, ident("scale"), int(0));
        let t2 = bin(
            BinOp::Lt,
            bin(BinOp::Mul, ident("cell"), ident("xs")),
            ident("xn"),
        );
        let t3 = bin(
            BinOp::Lt,
            ident("xn"),
            bin(
                BinOp::Mul,
                bin(BinOp::Add, ident("cell"), int(1)),
                ident("xs"),
            ),
        );
        let t_conj_owned = [t0, t1, t2, t3];
        let t_conj: Vec<&Spanned<Expr>> = t_conj_owned.iter().collect();
        let vars = ["a", "b", "w", "d"];
        let images = ["xn", "xs", "cell", "scale"];
        // conclusion-LHS match bound a, b, d; w is still free
        let mut seed = Sigma::new();
        seed.insert("a".to_string(), "xn".to_string());
        seed.insert("b".to_string(), "xs".to_string());
        seed.insert("d".to_string(), "scale".to_string());
        let (sigma, hyps) =
            complete_premises(&h_conj, &t_conj, &vars, &images, &seed).expect("must complete");
        assert_eq!(sigma.get("w").map(String::as_str), Some("cell"));
        assert_eq!(hyps, vec![0, 1, 2, 3]);
        // drop one consumer conjunct (`scale > 0`) -> the subset rule
        // fails and the consumer must decline.
        let t_missing: Vec<&Spanned<Expr>> = [&t_conj_owned[0], &t_conj_owned[2], &t_conj_owned[3]]
            .into_iter()
            .collect();
        assert!(complete_premises(&h_conj, &t_missing, &vars, &images, &seed).is_none());
    }

    /// The rewrite simulation: both cone sides must collapse to the
    /// same term, otherwise the figure declines.
    #[test]
    fn rewrite_simulation_requires_closing_shape() {
        let lhs_img = call(
            "binFloor",
            vec![ident("xn"), bin(BinOp::Mul, ident("xs"), ident("scale"))],
        );
        let rhs_img = call("binFloor", vec![ident("cell"), ident("scale")]);
        let other = call(
            "binFloor",
            vec![ident("yn"), bin(BinOp::Mul, ident("ys"), ident("scale"))],
        );
        // replacing the left image yields the right image -> equal
        let rewritten = replace_all(&lhs_img, &lhs_img, &rhs_img);
        assert!(expr_eq(&rewritten, &rhs_img));
        // an unmatched side stays distinct -> the caller must decline
        let rewritten_other = replace_all(&other, &lhs_img, &rhs_img);
        assert!(!expr_eq(&rewritten_other, &rhs_img));
        assert!(occurs_in(
            &bin(BinOp::Eq, lhs_img.clone(), other.clone()),
            &lhs_img
        ));
    }
}
