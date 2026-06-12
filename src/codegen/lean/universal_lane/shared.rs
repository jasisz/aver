//! AST recognition helpers shared by both lane families
//! (sign-premise and bridge-premise).

use super::super::expr::{aver_name_to_lean, emit_expr_legacy};
use crate::ast::{BinOp, Expr, Literal, Spanned, VerifyLaw};
use crate::codegen::CodegenContext;

pub(super) fn ident_of(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
        _ => None,
    }
}

pub(super) fn call_of(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    match &e.node {
        Expr::FnCall(callee, args) => Some((
            crate::codegen::common::expr_to_dotted_name(&callee.node)?,
            args.as_slice(),
        )),
        Expr::TailCall(data) => Some((data.target.clone(), data.args.as_slice())),
        _ => None,
    }
}

pub(super) fn ctor_of(e: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
    match &e.node {
        Expr::FnCall(callee, args) => {
            let name = crate::codegen::common::expr_to_dotted_name(&callee.node)?;
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

/// Flatten a `when` predicate into the consumer-facing PROP premises of
/// the derived corollary. The lane twin carries the premise at the Bool
/// seam (`<when> = true`); the corollary normalizes it so a downstream
/// consumer applies the helper with zero bridging:
/// - a conjunction (`Bool.and(a, b)`) splits into its conjuncts, so
///   each lands as its own Prop arrow rather than one `&&`-composed
///   Bool equality;
/// - a bare comparison atom (`n >= 0`, `w * b < a`) lowers to the
///   Prop-level relation (it elaborates Prop-level already — the twin's
///   `= true` only coerces `true` into Prop), surfaced WITHOUT the
///   trailing `= true`;
/// - everything else (an opaque Bool predicate fn, a `==`/`!=` atom)
///   stays at the Bool seam as `<atom> = true` — still consumable, the
///   consumer defers any further unfolding.
fn flatten_when_props(when: &Spanned<Expr>, ctx: &CodegenContext, out: &mut Vec<String>) {
    // `Bool.and(a, b)` — the canonical conjunction the lane families
    // build their `when` from — flattens into independent conjuncts.
    if let Some((callee, args)) = call_of(when)
        && callee == "Bool.and"
        && args.len() == 2
    {
        flatten_when_props(&args[0], ctx, out);
        flatten_when_props(&args[1], ctx, out);
        return;
    }
    let rendered = emit_expr_legacy(when, ctx, None).replace('\n', " ");
    match &when.node {
        // Order/(in)equality comparisons elaborate at the Prop level;
        // keep the relation, drop the Bool coercion.
        Expr::BinOp(BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte, _, _) => out.push(rendered),
        // Anything else stays at the Bool seam (`= true`).
        _ => out.push(format!("{rendered} = true")),
    }
}

/// Prop-level conclusion of the derived corollary. The lane twin proves
/// `<lhs> = <rhs>`; for a Bool-valued law whose claim is a `==` comparison
/// pinned `=> true` (the `floorQ(..) == floorQ(..)` shape), the consumer
/// wants the Prop equality `<l> = <r>` rather than the Bool equation
/// `(<l> == <r>) = true` — `beq_iff_eq` bridges the two in the proof.
/// Every other law keeps the twin's own `<lhs> = <rhs>` conclusion.
fn corollary_conclusion(law: &VerifyLaw, ctx: &CodegenContext, conclusion: &str) -> String {
    let rhs_true = matches!(&law.rhs.node, Expr::Literal(Literal::Bool(true)));
    if rhs_true && let Expr::BinOp(op @ (BinOp::Eq | BinOp::Neq), l, r) = &law.lhs.node {
        let ls = emit_expr_legacy(l, ctx, None).replace('\n', " ");
        let rs = emit_expr_legacy(r, ctx, None).replace('\n', " ");
        let rel = if matches!(op, BinOp::Eq) { "=" } else { "≠" };
        return format!("{ls} {rel} {rs}");
    }
    conclusion.to_string()
}

/// Derive the Prop-form corollary `<theorem_base>_prop` from the lane
/// twin `<theorem_base>_universal` with the fixed Bool/Prop bridge
/// tactic set ({`Bool.and_eq_true`, `decide_eq_true_eq`, `beq_iff_eq`}
/// composed under `simpa`). The corollary's STATEMENT moves all three
/// measured seams to the Prop side (conjunction split, bare comparison
/// un-coerced, `==` conclusion lowered); its PROOF reconstructs the
/// twin's Bool premise from the Prop hypotheses and lowers the twin's
/// conclusion — both via a small fixed `first` ladder so the single
/// derivation closes every emitted seam combination.
///
/// Returned as a text block to APPEND to the twin's module (same
/// definition site, one module). It is NOT a separately credited
/// declaration: the lane index records only the twin's `_universal`
/// name, so the crediting probe (`#print axioms <theorem>`) never
/// keys on the corollary — and because the corollary lives in the
/// same module and references the twin, a sabotaged or non-closing
/// twin takes the corollary's build down with it (fail-closed: a
/// corollary can never outlive a dead twin).
pub(super) fn render_prop_corollary(
    theorem_base: &str,
    quant_params: &str,
    given_names: &[String],
    law: &VerifyLaw,
    ctx: &CodegenContext,
    conclusion: &str,
) -> String {
    let corollary = format!("{theorem_base}_prop");
    let twin = format!("{theorem_base}_universal");

    let mut props = Vec::new();
    if let Some(when) = law.when.as_ref() {
        flatten_when_props(when, ctx, &mut props);
    }
    let hyp_names: Vec<String> = (0..props.len()).map(|i| format!("hp{i}")).collect();
    let hyp_binders: String = props
        .iter()
        .map(|p| format!("{p} -> "))
        .collect::<Vec<_>>()
        .concat();
    let concl = corollary_conclusion(law, ctx, conclusion);

    let givens_applied = given_names
        .iter()
        .map(|g| aver_name_to_lean(g))
        .collect::<Vec<_>>()
        .join(" ");
    let givens_applied = if givens_applied.is_empty() {
        String::new()
    } else {
        format!(" {givens_applied}")
    };
    let intro_names: Vec<String> = given_names
        .iter()
        .map(|g| aver_name_to_lean(g))
        .chain(hyp_names.iter().cloned())
        .collect();
    let intro_line = if intro_names.is_empty() {
        String::new()
    } else {
        format!("  intro {}\n", intro_names.join(" "))
    };

    // Discharge the twin's Bool premise from the Prop hypotheses. The
    // ladder is fixed and covers every emitted seam: a split `&&`
    // conjunction (`Bool.and_eq_true` + the anonymous constructor over
    // the un-curried Prop conjuncts), a single bare comparison
    // (`decide_eq_true_eq` then `assumption`), and an opaque Bool atom
    // kept as `= true` (direct `assumption`/`exact`).
    let hyp_tuple = if hyp_names.is_empty() {
        "True.intro".to_string()
    } else {
        format!("⟨{}⟩", hyp_names.join(", "))
    };
    let discharge = format!(
        "(by\n    first\n      \
         | (simp only [Bool.and_eq_true, decide_eq_true_eq]\n         \
         first\n           | exact {hyp_tuple}\n           \
         | (repeat' apply And.intro) <;> assumption\n           \
         | (repeat' constructor) <;> assumption\n           | assumption)\n      \
         | (simp only [decide_eq_true_eq]; assumption)\n      \
         | assumption\n      | (first | exact {first_hyp} | (simpa using {first_hyp})))",
        first_hyp = hyp_names
            .first()
            .cloned()
            .unwrap_or_else(|| "rfl".to_string()),
    );

    let mut block = String::new();
    block.push_str(
        "-- Prop-form corollary: the same proved claim with the Bool/Prop\n\
         -- seams normalized (premise conjuncts un-curried to Prop, the\n\
         -- conclusion lowered) so a downstream consumer applies it with no\n\
         -- bridging. Derived purely from the twin above; carries no credit\n\
         -- of its own (the lane index probes only the `_universal` name) and\n\
         -- cannot outlive a broken twin (same module, references it).\n",
    );
    block.push_str(&format!(
        "theorem {corollary} : ∀ {quant_params}, {hyp_binders}{concl} := by\n"
    ));
    block.push_str(&intro_line);
    block.push_str(&format!(
        "  have hu := {twin}{givens_applied} {discharge}\n"
    ));
    block.push_str("  simpa [beq_iff_eq] using hu\n");
    block
}
