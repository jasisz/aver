//! Keystone strategy: the non-recursive laws-as-lemmas composition arm.
//!
//! Split out of `induction.rs` with no behavior change; see `mod.rs`. Recognizes
//! a claim with no list to induct on and closes it by `grind` over the earlier
//! sibling laws (the pool) plus the cone, including the signed power-of-two
//! homomorphism normalizer and the order-bridge citations.

use super::super::super::expr::aver_name_to_lean;
use super::super::super::tactic_ir::Tactic;
use super::super::AutoProof;
use super::super::shared::law_simp_defs;
use super::floor_bound::{emit_rational_floor_family, rational_floor_shape, rf_bare_basename};
use super::*;
use crate::ast::{VerifyBlock, VerifyLaw};
use crate::codegen::CodegenContext;

/// The verify-law blocks of the SAME module as `vb`, in source order — the pool
/// the keystone's laws-as-lemmas composition searches for earlier siblings. When
/// `vb` is the entry's own law it lives in `ctx.items`; when `vb` belongs to a
/// DEPENDENCY module (the cross-file re-emit path — a dep law admitted by a
/// consumer and re-proved inside the dep's namespace) it lives in that module's
/// `verify_laws`, NOT `ctx.items` (which holds the unrelated entry's blocks).
/// Searching the right collection keeps the keystone firing identically in the
/// standalone export and the dependency export: the entry case returns exactly
/// the `ctx.items` verify blocks (byte-identical to the old `for item in
/// &ctx.items` iteration), the dependency case finds the module's siblings.
fn enclosing_verify_blocks<'a>(vb: &VerifyBlock, ctx: &'a CodegenContext) -> Vec<&'a VerifyBlock> {
    use crate::ast::TopLevel;
    for module in &ctx.modules {
        if module
            .verify_laws
            .iter()
            .any(|b| b.line == vb.line && b.fn_name == vb.fn_name)
        {
            return module.verify_laws.iter().collect();
        }
    }
    ctx.items
        .iter()
        .filter_map(|it| match it {
            TopLevel::Verify(b) => Some(b),
            _ => None,
        })
        .collect()
}

/// Keystone — the NON-recursive analog of [`recognize_conditional_inductive_generic`].
/// A conditional claim with NO list to induct on, decomposed purely by citing
/// the laws-as-lemmas pool: EARLIER sibling laws in the file abstract whatever
/// recursion the cone hides (a `pow2` homomorphism, a `mulLeMonoRight`
/// monotonicity), so `grind` over those pool laws + the cone closes the goal
/// WITHOUT inducting — grind e-matches each pool law, instantiates it, discharges
/// its premise from context, and finishes the residual ring / linear identity.
/// This is The Method's composition step made generic: the algebraic content
/// lives as Aver helper laws (proven on their own), and the engine only supplies
/// the citation skeleton, never a per-figure template.
///
/// Speculative: the probe is the oracle (grind either closes from the pool or it
/// does not), and `default` is OFF — a no-probe transpile keeps the bounded
/// sampled statement, so a law `grind`+pool cannot close never regresses off its
/// sound bounded fallback.
pub(in crate::codegen::lean) fn recognize_pool_composition_generic(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> bool {
    // A non-conditional law is admitted ONLY for the equational power-of-two
    // homomorphism composition: an earlier-sibling homomorphism law about a
    // recursive power-of-two cone fn (detected name-blind via its `FloorDivWindow`
    // figure) cited through the pow2 normalizer — the SIGNED pow2 homomorphism
    // `2^(m+n) = 2^m·2^n` for ANY integers, whose sign branches each reduce to a
    // product of `pow2` of nonneg atoms the normalizer canonicalizes. Every other
    // unconditional shape keeps its own strategy (RingIdentity, LinearArithmetic,
    // SimpOverPreludeLemmas, …): this can never steal one, because admission here
    // requires both a power-of-two normalizer fn AND a non-empty equational pool,
    // which those shapes do not have. (`when`-laws keep the full keystone surface —
    // order-bridge, rational-floor and no-pool definitional arms below.)
    if law.when.is_none()
        && (keystone_pow2_fn(vb, law, ctx).is_none()
            || keystone_pool_names(vb, law, ctx).is_empty())
    {
        return false;
    }
    // The inductive path owns list-recursion laws; this is the residue with no
    // list-induction target.
    if find_list_induction_target(law).is_some() {
        return false;
    }
    // Exclusive with the comparison-bridge arm (it runs first).
    if recognize_conditional_comparison_bridge(law, ctx) {
        return false;
    }
    // Decline a law the DETERMINISTIC interval-monotonicity rung already closes:
    // it has its own complete universal proof (a `replaces_theorem` helper kit +
    // assembly, separate-arrow statement), so the keystone must not speculatively
    // steal it. Stealing it would only matter under the probe — the keystone's
    // bare `grind` cannot close an interval-monotonicity goal and would fall to
    // its sorry floor — but the theft also swaps the law's STATEMENT shape (the
    // keystone uses the single-conjunction `omit_domain` form, interval-mono uses
    // separate arrows). Any later law that CITES this universal (the
    // multi-citation arm) then sees a different premise shape in the probe than
    // in the commit and mis-probes. Declining here keeps interval-monotonicity
    // laws deterministic in BOTH passes, so citations stay stable.
    if super::super::recognize_interval_monotonicity(vb, law, ctx) {
        return false;
    }
    // Decline the exact-rational positivity / at-least-one / monotonicity shapes
    // over an opaque `Fraction` cone fn `F`: each has a dedicated DETERMINISTIC
    // `replaces_theorem` closer (citing `F`'s recursive-positivity / homomorphism
    // pool laws and chaining through the generic Fraction order kit), dispatched
    // before the keystone. The keystone's bare `grind` over the pool can never
    // assemble that chain, so under the probe it would only fall to its sorry
    // floor; declining here keeps the law deterministic in BOTH passes and lets
    // the dedicated rung own its (separate-arrow) universal statement, exactly as
    // the interval-monotonicity guard above does.
    if super::super::recognize_frac_positivity(vb, law, ctx)
        || super::super::recognize_frac_geone(vb, law, ctx)
        || super::super::recognize_frac_monotone_compose(vb, law, ctx)
    {
        return false;
    }
    // Decline the general recursive-monotonicity shape for the same reason: its
    // dedicated rung is a DETERMINISTIC `replaces_theorem` closer (the shared
    // recursive-mono kit + a monotonicity citation), dispatched before the
    // keystone. The keystone's bare `grind` over the pool can never close it (it
    // needs `f.induct`), so declining here keeps the law deterministic in BOTH
    // passes and lets the dedicated rung own its (separate-arrow) statement.
    if super::super::recognize_recursive_monotone(vb, law, ctx) {
        return false;
    }
    // Decline the rational-order chaining shape (the reciprocal-magnitude
    // composition): its dedicated rung is a DETERMINISTIC `replaces_theorem`
    // closer (the `frac_le_trans` kit + the cited monotonicity/homomorphism
    // chain), dispatched before the keystone. The keystone's bare `grind` over
    // the pool can never synthesize the chain's intermediate magnitudes, so
    // under the probe it would only fall to its sorry floor; declining here
    // keeps the law deterministic in BOTH passes and lets the dedicated rung
    // own its (separate-arrow) universal statement, exactly as the
    // interval-monotonicity / pow2-monotonicity guards above do.
    if super::super::recognize_frac_order_chain(vb, law, ctx) {
        return false;
    }
    // The claim goes through the subject fn (`holds`, or an equational `=> rhs`).
    if !matches!(&law.lhs.node, crate::ast::Expr::FnCall(..)) {
        return false;
    }
    // Fail closed on refinement-lifted givens. A `@Nat>=0`-style refined given
    // takes a DIFFERENT statement path: the `conditional_universal` gate requires
    // `lifted_vars.is_empty()` (verify.rs), so a lifted law's universal statement
    // carries the domain through the lifted quantifier / extra premises — which the
    // keystone's fixed `intro <givens> h_when` proof shape does not match (the intro
    // count would disagree with the statement's binders). Mirroring that gate here
    // keeps the recognizer's contract symmetric across EVERY call site (statement
    // and emit), so a lifted law can never trigger a keystone proof out of step with
    // its statement. (No corpus law hits this today; this is a fail-closed guard.)
    if law.givens.iter().any(|g| {
        crate::codegen::common::refinement_lift_for_given(
            &g.name,
            &g.type_name,
            &law.lhs,
            &law.rhs,
            ctx,
        )
        .is_some()
    }) {
        return false;
    }
    // This rung earns its keep over the (when-gated-off) plain simp / grind
    // rungs in two ways, both probe-decided:
    //   * a citable POOL — an earlier law whose subject fn is in this law's
    //     call cone (the EQUATIONAL pool, e.g. a `pow2` homomorphism abstracting
    //     cone recursion) or an earlier inequality `L <op> R` matching subterms
    //     of the goal / premises (the ORDER-BRIDGE pool, e.g. a `mulLeMonoRight`
    //     monotonicity feeding a transitivity step); OR
    //   * NO pool at all, but the conditional goal is a flat definitional
    //     identity `grind` closes outright once the cone is unfolded — a
    //     record-field projection through `trunc` / `fpScale`, a Bool predicate
    //     that reduces to `rfl` under the premises. The plain grind rung NEVER
    //     runs on a `when`-law (it is gated to `law.when.is_none()`), so without
    //     this arm such a law has no universal path and silently degrades to the
    //     bounded sampled statement.
    // The speculative probe is the oracle in BOTH cases: a candidate that does
    // not close falls back to bounded (default `false`), so admitting the
    // no-pool case is fail-safe and leaves a non-probe `transpile` byte-identical.
    //
    // The no-pool case is gated to laws WITHOUT a dedicated template strategy of
    // their own. A `when`-law pinned to a bespoke template (the `FloorDivWindow`
    // pow / window family — its own PowSumSplit / SigWindow / ProductWindow
    // proofs need functional induction the keystone's bare `grind` can't do) is
    // dispatched LATER and must keep that template; admitting it here would let
    // the probe steal it with a grind that can only fail. Pool-citing laws skip
    // this gate entirely (they are never `FloorDivWindow`-shaped and the pool is
    // the whole point).
    let has_pool = !keystone_pool_names(vb, law, ctx).is_empty()
        || !keystone_order_bridge_citations(vb, law, ctx).is_empty();
    // The rational-over-floor sign/magnitude family (Lemma 7.2.2) cites its
    // earlier window/magnitude helper laws through its OWN dedicated arm
    // (`rf_citations`), not the equational/order-bridge pools, so admit it here
    // regardless of those pools or any catch-all strategy pin.
    let rational_floor = rational_floor_shape(vb, law, ctx).is_some();
    if !has_pool && !rational_floor {
        use crate::ir::ProofStrategy;
        match super::super::law_strategy_for(ctx, &vb.fn_name, &law.name) {
            None
            | Some(ProofStrategy::BackendDispatch)
            | Some(ProofStrategy::LinearArithmetic { .. }) => {}
            _ => return false,
        }
    }
    let id = format!("{}.{}", vb.fn_name, law.name);
    super::super::super::tactic_ir::speculative::admits(&id, false)
}

/// The laws-as-lemmas pool for the keystone: the Lean theorem NAMES of EARLIER
/// sibling laws whose SUBJECT fn lies in this law's call cone. Looser than
/// [`earlier_law_lemmas`] (the inductive simp path) on purpose — the keystone's
/// whole point is to cite a law ABOUT a recursive cone fn (a `pow2`
/// homomorphism) to abstract that recursion, which the strict pure-fn-subset
/// eligibility excludes. `grind` instantiates and discharges each cited law;
/// an irrelevant one is simply unused, so subject-in-cone is the right gate.
///
/// Pool membership is decided DETERMINISTICALLY by `law_as_lemma_statement`
/// (static IR / source-order checks), NOT by the speculative `admits` tracking
/// that gates the keystone law itself — so the cited set is identical in the
/// probe build and the committed build. Soundness of citing a pool member does
/// not rest on this stability anyway: if a cited law's own proof carries `sorry`,
/// the citing theorem inherits `sorryAx` and the `--check` whitelist refuses it
/// credit (see the `pinned_when_universal` note in `law_as_lemma_statement`).
fn keystone_pool_names(vb: &VerifyBlock, law: &VerifyLaw, ctx: &CodegenContext) -> Vec<String> {
    use crate::ast::VerifyKind;
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let cone_fns: std::collections::HashSet<String> =
        cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    let mut out = Vec::new();
    for prev in enclosing_verify_blocks(vb, ctx) {
        // Only blocks EARLIER in source — source order is emit order, so the
        // cited theorem precedes this one and cyclic use is impossible.
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !cone_fns.contains(&prev.fn_name) {
            continue;
        }
        if let Some((name, _stmt)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        {
            out.push(name);
        }
    }
    out
}

/// The SUBJECT fns (Lean names) of the keystone's cited equational pool —
/// the fn each cited pool law is a rewrite ABOUT. Mirrors
/// [`keystone_pool_names`] one-for-one but returns the subject fn instead
/// of the lemma name. The keystone keeps these fns FOLDED (out of its
/// `simp only` cone unfold) so the cited rewrite can e-match the goal term:
/// the floorDiv exact-cancel `floor(s·pow(b), pow(a)) = s·pow(b−a)` can only
/// fire while `floorDiv` is still abstract, exactly as the `pow2`
/// homomorphism needs `pow2` abstract (recursive fns are folded for the same
/// reason). Shape-keyed and name-blind — driven by which laws are cited, not
/// by any hard-coded fn name.
fn keystone_pool_subject_fns(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<String> {
    use crate::ast::VerifyKind;
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let cone_fns: std::collections::HashSet<String> =
        cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    let mut out = Vec::new();
    for prev in enclosing_verify_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !cone_fns.contains(&prev.fn_name) {
            continue;
        }
        if crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx).is_some() {
            out.push(aver_name_to_lean(&prev.fn_name));
        }
    }
    out
}

/// The cited pool laws (by Lean theorem name) that REGISTER THEMSELVES as
/// `@[grind]` via a `grind_pattern` — the `FloorPow2Cancel` exact-division
/// cancel, keyed on its floor term. They are already in `grind`'s lemma set
/// globally, so re-listing them in `grind [...]` only earns a "redundant
/// parameter" warning. The keystone drops them from its explicit hint list
/// (but still keeps their subject fn FOLDED, via [`keystone_pool_subject_fns`],
/// so their floor term survives the cone unfold for the pattern to fire).
fn keystone_self_registering_pool_names(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> std::collections::HashSet<String> {
    use crate::ast::VerifyKind;
    use crate::ir::{FloorWindowFigure, ProofStrategy};
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let cone_fns: std::collections::HashSet<String> =
        cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    let mut out = std::collections::HashSet::new();
    for prev in enclosing_verify_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !cone_fns.contains(&prev.fn_name) {
            continue;
        }
        if !matches!(
            super::super::law_strategy_for(ctx, &prev.fn_name, &prev_law.name),
            Some(ProofStrategy::FloorDivWindow {
                figure: FloorWindowFigure::FloorPow2Cancel { .. }
            })
        ) {
            continue;
        }
        if let Some((name, _stmt)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        {
            out.insert(name);
        }
    }
    out
}

/// The power-of-two function the keystone's cited pool reasons about, if any:
/// the `pow_fn` of an EARLIER sibling law (in this law's cone) whose proof
/// strategy is a `FloorDivWindow` figure (the power-of-two homomorphism /
/// window family), returned in Lean form. `None` when no cited pool law is
/// about a power-of-two fn — the keystone then keeps its plain simp+grind
/// emission unchanged.
///
/// Shape-keyed and name-blind: the trigger is the `FloorDivWindow` figure
/// (which the lowerer only pins after its `is_pow2_shape` gate), never a
/// hard-coded fn name. When present, the keystone prepends the generic
/// [`pow2_linear_form_normalizer_support`](super::super::floor_window) stack so
/// `grind` can canonicalize `pow` of ANY nonneg linear form.
pub(super) fn keystone_pow2_fn(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<String> {
    use crate::ast::VerifyKind;
    use crate::ir::{FloorWindowFigure, ProofStrategy};
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let cone_fns: std::collections::HashSet<String> =
        cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    for prev in enclosing_verify_blocks(vb, ctx) {
        // Only earlier-in-source siblings — same gate as `keystone_pool_names`.
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !cone_fns.contains(&prev.fn_name) {
            continue;
        }
        let Some(ProofStrategy::FloorDivWindow { figure }) =
            super::super::law_strategy_for(ctx, &prev.fn_name, &prev_law.name)
        else {
            continue;
        };
        let pow_fn = match figure {
            FloorWindowFigure::PowPositive { pow_fn }
            | FloorWindowFigure::PowSumSplit { pow_fn }
            | FloorWindowFigure::SigWindow { pow_fn, .. }
            | FloorWindowFigure::ProductWindow { pow_fn, .. }
            | FloorWindowFigure::FloorPow2Window { pow_fn, .. }
            | FloorWindowFigure::FloorPow2Cancel { pow_fn, .. } => pow_fn,
        };
        return Some(aver_name_to_lean(&pow_fn));
    }
    None
}

/// A SIGNED-power-of-two homomorphism citation for the keystone: when an
/// earlier sibling law proves the homomorphism of a `Fraction`-valued
/// signed power-of-two cone fn (`2^k` faithful for every integer `k`),
/// and that fn is in this law's cone, the keystone can canonicalize a
/// cross-multiplied product of it through the proven law — the `Fraction`
/// analog of [`keystone_pow2_fn`]. All Lean names; `cited` is the proven
/// homomorphism law theorem to cite.
///
/// Shape-keyed and name-blind: the trigger is (a) an earlier `… holds` law
/// whose inlined body is `sameValue(F(m+n), times(F(m), F(n)))` for a
/// single-arg fn `F`, (b) `F` returning `Fraction` with a `k < 0` sign
/// match (the signed-power-of-two shape), and (c) `F`'s underlying integer
/// power-of-two being exactly the one [`keystone_pow2_fn`] already
/// canonicalizes (so both normalizers are present and consistent). No
/// hard-coded fn or law name.
struct Pow2SignedCite {
    sgn: String,
    pow: String,
    hom_fn: String,
    cited: String,
}

fn keystone_pow2signed_cite(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<Pow2SignedCite> {
    use crate::ast::{Expr, Literal, VerifyKind};
    let int_pow = keystone_pow2_fn(vb, law, ctx)?;
    let inputs = crate::codegen::proof_lower::ProofLowerInputs::from_ctx(ctx);
    let cone = crate::codegen::proof_lower::LawProofCone::compute(law, &vb.fn_name, &inputs);
    let cone_fns: std::collections::HashSet<String> =
        cone.pure_fns().iter().map(|fd| fd.name.clone()).collect();
    let all_fns: Vec<&crate::ast::FnDef> = inputs.pure_fns();
    for prev in enclosing_verify_blocks(vb, ctx) {
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        if !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true))) {
            continue;
        }
        let Some(inlined) = inline_fn_call(&prev_law.lhs, ctx) else {
            continue;
        };
        let Some(sgn_dotted) = match_signed_homomorphism_shape(&inlined.node) else {
            continue;
        };
        let sgn_basename = sgn_dotted
            .rsplit('.')
            .next()
            .unwrap_or(&sgn_dotted)
            .to_string();
        if !cone_fns.contains(&sgn_basename) {
            continue;
        }
        let Some(sgn_fd) = all_fns.iter().copied().find(|fd| fd.name == sgn_basename) else {
            continue;
        };
        if !signed_pow2_shape(sgn_fd, &int_pow) {
            continue;
        }
        // The signed homomorphism only earns its keep when the goal applies `F`
        // to a SUM of exponents — produced by a cone fn that builds a record with
        // an ADDITIVE field (e.g. a float product's `exp = a.exp + b.exp`). A law
        // that applies `F` only to a bare variable (`F(f.exp)`, the
        // nonneg-exponent agreement) needs `F` UNFOLDED and reduced by its sign
        // premise, not folded + canonicalized — folding it there would strand the
        // single `F(var)` term. Gate on that additive-record shape (name-blind):
        // no cone fn building a sum-valued field ⇒ no `F(sum)` ⇒ decline.
        if !cone_has_additive_record(&cone_fns, &sgn_basename, &prev.fn_name, &all_fns) {
            continue;
        }
        let Some((thm_name, _stmt)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        else {
            continue;
        };
        return Some(Pow2SignedCite {
            sgn: aver_name_to_lean(&sgn_basename),
            pow: int_pow,
            hom_fn: aver_name_to_lean(&prev.fn_name),
            cited: thm_name,
        });
    }
    None
}

/// Match the signed-power-of-two homomorphism body shape
/// `sameValue(F(A + B), times(F(A), F(B)))`, returning `F`'s dotted name.
/// `sameValue` / `times` are matched by basename (the Rational ops), `F`
/// by structural consistency across the three calls — name-blind in `F`.
fn match_signed_homomorphism_shape(e: &crate::ast::Expr) -> Option<String> {
    use crate::ast::{BinOp, Expr};
    let Expr::FnCall(callee, args) = e else {
        return None;
    };
    if args.len() != 2 {
        return None;
    }
    let cname = super::super::shared::expr_dotted_name(callee)?;
    if cname.rsplit('.').next() != Some("sameValue") {
        return None;
    }
    let Expr::FnCall(f0, f0args) = &args[0].node else {
        return None;
    };
    if f0args.len() != 1 {
        return None;
    }
    let f0name = super::super::shared::expr_dotted_name(f0)?;
    let Expr::BinOp(BinOp::Add, a, b) = &f0args[0].node else {
        return None;
    };
    let Expr::FnCall(tcallee, targs) = &args[1].node else {
        return None;
    };
    if targs.len() != 2 {
        return None;
    }
    let tname = super::super::shared::expr_dotted_name(tcallee)?;
    if tname.rsplit('.').next() != Some("times") {
        return None;
    }
    let Expr::FnCall(fa, faargs) = &targs[0].node else {
        return None;
    };
    let Expr::FnCall(fb, fbargs) = &targs[1].node else {
        return None;
    };
    if faargs.len() != 1 || fbargs.len() != 1 {
        return None;
    }
    if super::super::shared::expr_dotted_name(fa)? != f0name
        || super::super::shared::expr_dotted_name(fb)? != f0name
    {
        return None;
    }
    if faargs[0].node != a.node || fbargs[0].node != b.node {
        return None;
    }
    Some(f0name)
}

/// `F` is a signed-power-of-two fn: one `Int` param, returns `Fraction`,
/// body is a `k < 0` sign match, and it calls the SAME integer
/// power-of-two `int_pow` (Lean name) the keystone already canonicalizes.
pub(super) fn signed_pow2_shape(fd: &crate::ast::FnDef, int_pow: &str) -> bool {
    use crate::ast::{BinOp, Expr, Stmt};
    let [(p, ty)] = fd.params.as_slice() else {
        return false;
    };
    if ty != "Int" || fd.return_type.rsplit('.').next() != Some("Fraction") {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    let Expr::BinOp(BinOp::Lt, sl, sr) = &subject.node else {
        return false;
    };
    if super::super::shared::expr_dotted_name(sl).as_deref() != Some(p)
        || !matches!(&sr.node, Expr::Literal(crate::ast::Literal::Int(0)))
    {
        return false;
    }
    if arms.len() != 2 {
        return false;
    }
    // The body must canonicalize through the SAME integer power-of-two the
    // keystone's pow2 normalizer uses — collect every fn the arms call and
    // require `int_pow` among them (name-blind: matched by Lean name equality
    // with the already-detected `int_pow`, never a literal).
    let mut called: Vec<String> = Vec::new();
    for arm in arms {
        collect_fncall_names(&arm.body.node, &mut called);
    }
    // Compare on the BARE basename: a cross-module consumer holds `int_pow`
    // qualified (`Domain.Fprep.pow2`) while the signed fn's own body calls the
    // pow fn by its in-module bare name (`pow2`) — match the tails so the
    // recognizer fires for a dep-module signed power of two too.
    let want = rf_bare_basename(int_pow);
    called
        .iter()
        .any(|n| rf_bare_basename(&aver_name_to_lean(n)) == want)
}

/// Collect the dotted names of every `FnCall` in `e` (recursively).
fn collect_fncall_names(e: &crate::ast::Expr, out: &mut Vec<String>) {
    use crate::ast::Expr;
    match e {
        Expr::FnCall(callee, args) => {
            if let Some(n) = super::super::shared::expr_dotted_name(callee) {
                out.push(n);
            }
            for a in args {
                collect_fncall_names(&a.node, out);
            }
        }
        Expr::BinOp(_, a, b) => {
            collect_fncall_names(&a.node, out);
            collect_fncall_names(&b.node, out);
        }
        Expr::Neg(a) => collect_fncall_names(&a.node, out),
        Expr::Attr(b, _) => collect_fncall_names(&b.node, out),
        Expr::RecordCreate { fields, .. } => {
            for (_, v) in fields {
                collect_fncall_names(&v.node, out);
            }
        }
        Expr::Match { subject, arms } => {
            collect_fncall_names(&subject.node, out);
            for arm in arms {
                collect_fncall_names(&arm.body.node, out);
            }
        }
        _ => {}
    }
}

/// True when some fn in `cone_fns` (other than the signed fn itself or the
/// homomorphism fn) builds a record with at least one field whose value is a
/// `BinOp::Add` — the static signature of a fn that produces a sum-valued
/// exponent the signed power-of-two is then applied to (e.g. a float product's
/// `exp = a.exp + b.exp`). Name-blind: keyed on the `RecordCreate` + `Add`
/// shape, never on a fn name.
fn cone_has_additive_record(
    cone_fns: &std::collections::HashSet<String>,
    sgn_basename: &str,
    hom_basename: &str,
    all_fns: &[&crate::ast::FnDef],
) -> bool {
    for fd in all_fns {
        if !cone_fns.contains(&fd.name) || fd.name == sgn_basename || fd.name == hom_basename {
            continue;
        }
        for stmt in fd.body.stmts() {
            if let crate::ast::Stmt::Expr(e) = stmt
                && expr_has_additive_record_field(&e.node)
            {
                return true;
            }
        }
    }
    false
}

/// Recursively: does `e` contain a `RecordCreate` with a field whose value is a
/// `BinOp::Add`?
fn expr_has_additive_record_field(e: &crate::ast::Expr) -> bool {
    use crate::ast::Expr;
    match e {
        Expr::RecordCreate { fields, .. } => fields.iter().any(|(_, v)| {
            matches!(&v.node, Expr::BinOp(crate::ast::BinOp::Add, _, _))
                || expr_has_additive_record_field(&v.node)
        }),
        Expr::FnCall(c, args) => {
            expr_has_additive_record_field(&c.node)
                || args.iter().any(|a| expr_has_additive_record_field(&a.node))
        }
        Expr::BinOp(_, a, b) => {
            expr_has_additive_record_field(&a.node) || expr_has_additive_record_field(&b.node)
        }
        Expr::Neg(a) => expr_has_additive_record_field(&a.node),
        Expr::Attr(b, _) => expr_has_additive_record_field(&b.node),
        Expr::Match { subject, arms } => {
            expr_has_additive_record_field(&subject.node)
                || arms
                    .iter()
                    .any(|arm| expr_has_additive_record_field(&arm.body.node))
        }
        Expr::RecordUpdate { base, updates, .. } => {
            updates.iter().any(|(_, v)| {
                matches!(&v.node, Expr::BinOp(crate::ast::BinOp::Add, _, _))
                    || expr_has_additive_record_field(&v.node)
            }) || expr_has_additive_record_field(&base.node)
        }
        _ => false,
    }
}

/// One eligible ORDER-BRIDGE citation for the keystone: an EARLIER sibling law
/// asserting an inequality `L <op> R` (its subject fn's body is a single
/// comparison, both sides non-atomic, `… holds`) whose two sides match subterms
/// of the citing law's goal / premises under one consistent substitution of the
/// cited law's givens. `name` is the Prop-form RESTATEMENT theorem (a `grind`
/// hint for the citing law); `support` is that theorem's declaration plus its
/// `grind_pattern` over the two comparison sides. The restatement is DERIVED
/// from the cited law's Bool theorem (`simpa … using (<thm> …)`), so deleting
/// the Aver law breaks it — a genuine citation, not a re-proof.
struct OrderBridgeCitation {
    name: String,
    support: Vec<String>,
}

/// The Lean infix for a comparison `BinOp`, or `None` for a non-comparison op
/// (an order law's body must be one of these four).
fn comparison_op_lean(op: crate::ast::BinOp) -> Option<&'static str> {
    use crate::ast::BinOp;
    match op {
        BinOp::Lt => Some("<"),
        BinOp::Gt => Some(">"),
        BinOp::Lte => Some("<="),
        BinOp::Gte => Some(">="),
        _ => None,
    }
}

/// The variable NAME of a leaf, treating a source `Ident` and a resolver-pass
/// `Resolved` slot uniformly — function bodies are emitted in resolved form
/// (`Resolved`), while law claims/premises are source form (`Ident`), and the
/// order-bridge matches one against the other.
pub(super) fn expr_var_name(e: &crate::ast::Expr) -> Option<&str> {
    match e {
        crate::ast::Expr::Ident(n) => Some(n),
        crate::ast::Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

/// An atomic term (bare variable or literal) is neither a valid `grind_pattern`
/// trigger nor a meaningful comparison side, so an order law with an atomic side
/// is rejected.
fn expr_is_atomic(e: &crate::ast::Expr) -> bool {
    expr_var_name(e).is_some() || matches!(e, crate::ast::Expr::Literal(_))
}

/// Structural equality on arithmetic terms, span-insensitive and identifying a
/// source `Ident(n)` with a resolved `Resolved{name:n}` (used to re-bind a
/// matched pattern variable consistently).
pub(super) fn arith_eq(a: &crate::ast::Expr, b: &crate::ast::Expr) -> bool {
    use crate::ast::Expr;
    match (expr_var_name(a), expr_var_name(b)) {
        (Some(na), Some(nb)) => return na == nb,
        (None, None) => {}
        _ => return false,
    }
    match (a, b) {
        (Expr::Literal(x), Expr::Literal(y)) => x == y,
        (Expr::BinOp(o1, a1, b1), Expr::BinOp(o2, a2, b2)) => {
            o1 == o2 && arith_eq(&a1.node, &a2.node) && arith_eq(&b1.node, &b2.node)
        }
        (Expr::Neg(x), Expr::Neg(y)) => arith_eq(&x.node, &y.node),
        (Expr::FnCall(c1, a1), Expr::FnCall(c2, a2)) => {
            a1.len() == a2.len()
                && arith_eq(&c1.node, &c2.node)
                && a1.iter().zip(a2).all(|(p, q)| arith_eq(&p.node, &q.node))
        }
        (Expr::Attr(b1, f1), Expr::Attr(b2, f2)) => f1 == f2 && arith_eq(&b1.node, &b2.node),
        _ => false,
    }
}

/// Whether `name` occurs as a free variable anywhere in the arithmetic term.
fn ident_occurs(e: &crate::ast::Expr, name: &str) -> bool {
    use crate::ast::Expr;
    if expr_var_name(e) == Some(name) {
        return true;
    }
    match e {
        Expr::BinOp(_, a, b) => ident_occurs(&a.node, name) || ident_occurs(&b.node, name),
        Expr::Neg(a) => ident_occurs(&a.node, name),
        Expr::FnCall(c, args) => {
            ident_occurs(&c.node, name) || args.iter().any(|x| ident_occurs(&x.node, name))
        }
        Expr::Attr(b, _) => ident_occurs(&b.node, name),
        _ => false,
    }
}

/// Push every arithmetic subterm of `expr` (itself included) onto `out`, by
/// value. Walks the product/sum/comparison/call/negation/field skeleton; opaque
/// leaves (matches, constructors, …) stop the recursion.
fn collect_arith_subterms(
    expr: &crate::ast::Spanned<crate::ast::Expr>,
    out: &mut Vec<crate::ast::Expr>,
) {
    use crate::ast::Expr;
    out.push(expr.node.clone());
    match &expr.node {
        Expr::BinOp(_, a, b) => {
            collect_arith_subterms(a, out);
            collect_arith_subterms(b, out);
        }
        Expr::Neg(a) => collect_arith_subterms(a, out),
        Expr::FnCall(c, args) => {
            collect_arith_subterms(c, out);
            for a in args {
                collect_arith_subterms(a, out);
            }
        }
        Expr::Attr(b, _) => collect_arith_subterms(b, out),
        _ => {}
    }
}

/// Deep-clone `e`, replacing any free `Ident(n)` with `map[n]` (substitute a
/// function's parameters by a call's argument terms). `Spanned::bare` is fine —
/// the result feeds the Lean expr emitter, which ignores spans.
pub(super) fn substitute_idents(
    e: &crate::ast::Spanned<crate::ast::Expr>,
    map: &std::collections::HashMap<String, crate::ast::Spanned<crate::ast::Expr>>,
) -> crate::ast::Spanned<crate::ast::Expr> {
    use crate::ast::{Expr, Spanned};
    let node = match &e.node {
        Expr::Ident(n) => {
            if let Some(rep) = map.get(n) {
                return rep.clone();
            }
            Expr::Ident(n.clone())
        }
        // Function bodies are emitted in resolved form, so a parameter reference
        // is a `Resolved` slot, not a source `Ident`. Substitute it by name too.
        Expr::Resolved { name, .. } => {
            if let Some(rep) = map.get(name) {
                return rep.clone();
            }
            e.node.clone()
        }
        Expr::BinOp(op, a, b) => Expr::BinOp(
            *op,
            Box::new(substitute_idents(a, map)),
            Box::new(substitute_idents(b, map)),
        ),
        Expr::Neg(a) => Expr::Neg(Box::new(substitute_idents(a, map))),
        Expr::FnCall(c, args) => Expr::FnCall(
            Box::new(substitute_idents(c, map)),
            args.iter().map(|x| substitute_idents(x, map)).collect(),
        ),
        Expr::Attr(b, f) => Expr::Attr(Box::new(substitute_idents(b, map)), f.clone()),
        // Record construction / update: a value-level fn (`fpValue`, `fpTrunc`)
        // whose body builds a record must have its FIELD expressions substituted
        // too, or a parameter referenced only inside a field (e.g. `fpTrunc`'s
        // `n` inside `sigBits = floorDiv(.. pow2(n-1) ..)`) would survive
        // un-renamed — the catch-all `other.clone()` did not recurse here.
        Expr::RecordCreate { type_name, fields } => Expr::RecordCreate {
            type_name: type_name.clone(),
            fields: fields
                .iter()
                .map(|(fname, v)| (fname.clone(), substitute_idents(v, map)))
                .collect(),
        },
        Expr::RecordUpdate {
            type_name,
            base,
            updates,
        } => Expr::RecordUpdate {
            type_name: type_name.clone(),
            base: Box::new(substitute_idents(base, map)),
            updates: updates
                .iter()
                .map(|(fname, v)| (fname.clone(), substitute_idents(v, map)))
                .collect(),
        },
        Expr::Match { subject, arms } => Expr::Match {
            subject: Box::new(substitute_idents(subject, map)),
            arms: arms
                .iter()
                .map(|arm| crate::ast::MatchArm {
                    body: Box::new(substitute_idents(&arm.body, map)),
                    ..arm.clone()
                })
                .collect(),
        },
        other => other.clone(),
    };
    Spanned::bare(node)
}

/// Inline a `fn(args)` call: substitute the callee's parameters by `args` in its
/// (single-expression) body. Returns the inlined body — for an order law this is
/// the comparison `L <op> R`; for the citing law it is the goal arithmetic. Bails
/// on anything that is not a direct call to a known single-tail function with a
/// matching arity.
pub(super) fn inline_fn_call(
    call: &crate::ast::Spanned<crate::ast::Expr>,
    ctx: &CodegenContext,
) -> Option<crate::ast::Spanned<crate::ast::Expr>> {
    use crate::ast::Expr;
    let Expr::FnCall(callee, args) = &call.node else {
        return None;
    };
    let name = super::super::shared::expr_dotted_name(callee)?;
    let fd = ctx.fn_def_by_name(&name, ctx.active_module_scope().as_deref())?;
    if fd.params.len() != args.len() {
        return None;
    }
    let body = fd.body.tail_expr()?;
    let mut map: std::collections::HashMap<String, crate::ast::Spanned<crate::ast::Expr>> =
        std::collections::HashMap::new();
    for ((pname, _), arg) in fd.params.iter().zip(args.iter()) {
        map.insert(pname.clone(), arg.clone());
    }
    Some(substitute_idents(body, &map))
}

/// First-order matching: does `pat` (over pattern variables `vars`) match the
/// ground term `term`, extending `subst` consistently? Re-binding a variable
/// checks `arith_eq` (span-insensitive, `Ident`/`Resolved`-agnostic).
fn match_arith_pattern(
    pat: &crate::ast::Expr,
    term: &crate::ast::Expr,
    vars: &std::collections::HashSet<String>,
    subst: &mut std::collections::HashMap<String, crate::ast::Expr>,
) -> bool {
    use crate::ast::Expr;
    // Leaf variable (source `Ident` or resolved slot): a pattern variable binds /
    // re-checks; a non-variable leaf must be the same variable.
    if let Some(pname) = expr_var_name(pat) {
        if vars.contains(pname) {
            return match subst.get(pname) {
                Some(prev) => arith_eq(prev, term),
                None => {
                    subst.insert(pname.to_string(), term.clone());
                    true
                }
            };
        }
        return expr_var_name(term) == Some(pname);
    }
    match pat {
        Expr::Literal(l) => matches!(term, Expr::Literal(tl) if tl == l),
        Expr::Neg(a) => {
            if let Expr::Neg(ta) = term {
                match_arith_pattern(&a.node, &ta.node, vars, subst)
            } else {
                false
            }
        }
        Expr::BinOp(op, a, b) => {
            if let Expr::BinOp(top, ta, tb) = term {
                op == top
                    && match_arith_pattern(&a.node, &ta.node, vars, subst)
                    && match_arith_pattern(&b.node, &tb.node, vars, subst)
            } else {
                false
            }
        }
        Expr::FnCall(c, args) => {
            if let Expr::FnCall(tc, targs) = term {
                args.len() == targs.len()
                    && match_arith_pattern(&c.node, &tc.node, vars, subst)
                    && args
                        .iter()
                        .zip(targs.iter())
                        .all(|(p, t)| match_arith_pattern(&p.node, &t.node, vars, subst))
            } else {
                false
            }
        }
        Expr::Attr(b, f) => {
            if let Expr::Attr(tb, tf) = term {
                f == tf && match_arith_pattern(&b.node, &tb.node, vars, subst)
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Whether both comparison sides match subterms of the citing law under ONE
/// consistent substitution (a variable shared between the sides — `c` in
/// `a*c`/`b*c` — must map to the same citing subterm on both).
fn order_sides_match_subterms(
    lhs_side: &crate::ast::Expr,
    rhs_side: &crate::ast::Expr,
    vars: &std::collections::HashSet<String>,
    subterms: &[crate::ast::Expr],
) -> bool {
    for t1 in subterms {
        let mut s = std::collections::HashMap::new();
        if match_arith_pattern(lhs_side, t1, vars, &mut s) {
            for t2 in subterms {
                let mut s2 = s.clone();
                if match_arith_pattern(rhs_side, t2, vars, &mut s2) {
                    return true;
                }
            }
        }
    }
    false
}

/// The keystone's ORDER-BRIDGE pool for `law`: every earlier sibling inequality
/// law whose two comparison sides match subterms of this law's goal / premises,
/// each carried as a Prop-form restatement + `grind_pattern` (see
/// [`OrderBridgeCitation`]). Deterministic (static IR + source order), so the
/// recognizer, the statement gate, and the emit all see the same set.
fn keystone_order_bridge_citations(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Vec<OrderBridgeCitation> {
    use crate::ast::{Expr, Literal, Spanned, VerifyKind};

    // Subterms of the citing law's GOAL (the inlined subject body) and PREMISES.
    let mut subterms: Vec<Expr> = Vec::new();
    if let Some(when) = &law.when {
        collect_arith_subterms(when, &mut subterms);
    }
    if let Some(goal) = inline_fn_call(&law.lhs, ctx) {
        collect_arith_subterms(&goal, &mut subterms);
    }
    collect_arith_subterms(&law.rhs, &mut subterms);
    if subterms.is_empty() {
        return Vec::new();
    }

    let citing_uid = format!(
        "{}_{}",
        aver_name_to_lean(&vb.fn_name),
        aver_name_to_lean(&law.name)
    );

    let mut out = Vec::new();
    for prev in enclosing_verify_blocks(vb, ctx) {
        // Only EARLIER blocks — source order is emit order, so the cited theorem
        // precedes this one and cyclic citation is impossible.
        if prev.line == vb.line && prev.fn_name == vb.fn_name {
            break;
        }
        let VerifyKind::Law(prev_law) = &prev.kind else {
            continue;
        };
        // The cited law must assert the inequality HOLDS (`… holds` / `=> true`),
        // so its Bool theorem concludes `<fn> args = true` and the restatement is
        // sound; and it must expose that universal theorem to derive from.
        if !matches!(&prev_law.rhs.node, Expr::Literal(Literal::Bool(true))) {
            continue;
        }
        let Some((thm_name, _stmt)) =
            crate::codegen::lean::toplevel::law_as_lemma_statement(prev, prev_law, ctx)
        else {
            continue;
        };
        // ORDER-LAW SHAPE: the cited fn's body is a single comparison `L <op> R`,
        // inlined at the law's call args, with BOTH sides non-atomic.
        let Some(inlined) = inline_fn_call(&prev_law.lhs, ctx) else {
            continue;
        };
        let Expr::BinOp(op, l_box, r_box) = &inlined.node else {
            continue;
        };
        let Some(op_lean) = comparison_op_lean(*op) else {
            continue;
        };
        let lhs_side = &l_box.node;
        let rhs_side = &r_box.node;
        if expr_is_atomic(lhs_side) || expr_is_atomic(rhs_side) {
            continue;
        }
        let given_names: Vec<String> = prev_law.givens.iter().map(|g| g.name.clone()).collect();
        if given_names.is_empty() {
            continue;
        }
        // Every quantified given must occur in a comparison side, so the
        // `grind_pattern` over the two sides covers all the restatement's binders
        // (`grind_pattern` errors on an uncovered variable).
        if !given_names
            .iter()
            .all(|g| ident_occurs(lhs_side, g) || ident_occurs(rhs_side, g))
        {
            continue;
        }
        let vars: std::collections::HashSet<String> = given_names.iter().cloned().collect();
        if !order_sides_match_subterms(lhs_side, rhs_side, &vars, &subterms) {
            continue;
        }

        // Build the Prop-form restatement DERIVED from the Bool theorem. The name
        // carries neither `_law_` nor `_eq_`, so the `--check` audit never mistakes
        // a restatement for a creditable main-law theorem.
        let rname = format!(
            "{}_order_{}_{}",
            citing_uid,
            aver_name_to_lean(&prev.fn_name),
            aver_name_to_lean(&prev_law.name)
        );
        let cited_fn_lean = aver_name_to_lean(&prev.fn_name);
        let quant = prev_law
            .givens
            .iter()
            .map(|g| {
                format!(
                    "({} : {})",
                    aver_name_to_lean(&g.name),
                    crate::codegen::lean::types::type_annotation_to_lean(&g.type_name)
                )
            })
            .collect::<Vec<_>>()
            .join(" ");
        let names_csv = given_names
            .iter()
            .map(|n| aver_name_to_lean(n))
            .collect::<Vec<_>>()
            .join(" ");
        let l_span: &Spanned<Expr> = l_box;
        let r_span: &Spanned<Expr> = r_box;
        let l_lean = super::super::super::expr::emit_expr_legacy(l_span, ctx, None);
        let r_lean = super::super::super::expr::emit_expr_legacy(r_span, ctx, None);
        let (premise_clause, intro, apply_args) = match &prev_law.when {
            Some(when) => {
                let p = super::super::super::expr::emit_expr_legacy(when, ctx, None);
                (
                    format!("{p} = true -> "),
                    format!("  intro {names_csv} hp"),
                    format!("{names_csv} hp"),
                )
            }
            None => (
                String::new(),
                format!("  intro {names_csv}"),
                names_csv.clone(),
            ),
        };
        let support = vec![
            format!(
                "theorem {rname} : ∀ {quant}, {premise_clause}({l_lean}) {op_lean} ({r_lean}) := by"
            ),
            intro,
            format!(
                "  simpa only [{cited_fn_lean}, decide_eq_true_eq] using ({thm_name} {apply_args})"
            ),
            format!("grind_pattern {rname} => ({l_lean}), ({r_lean})"),
        ];
        out.push(OrderBridgeCitation {
            name: rname,
            support,
        });
    }
    out
}

/// Emit the keystone proof: `intro <givens> h_when; first | (simp only [<cone>,
/// <bridges>] at h_when ⊢ <;> grind [<cone>, <pool law names>]) | <floor>`. The
/// `simp` unfolds the cone and bridges the Bool comparison / splits a
/// conjunctive guard; `grind` then composes the laws-as-lemmas pool (cone defs
/// AND the earlier-law theorem names) to close the residual. The `first | … |
/// sorry` floor keeps credit fail-closed; under the speculative probe the floor
/// carries the `AVERSPEC_SORRY:<fn.law>` trace so a non-closing portfolio is
/// observable and the law falls back to its bounded statement.
pub(in crate::codegen::lean) fn emit_pool_composition_generic_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    intro_names: &[String],
) -> Option<AutoProof> {
    if !recognize_pool_composition_generic(vb, law, ctx) {
        return None;
    }
    // The rational-over-floor sign/magnitude family (Lemma 7.2.2 sign half) takes
    // a dedicated cite-window + pow-positivity + `aver_int_order` / sign-split
    // arm, and deliberately bypasses the `pow2` homomorphism normalizer below
    // (it explodes on the squared-denominator rational goals). Every other
    // keystone law declines `rational_floor_shape` and keeps the emission below.
    if let Some(proof) = emit_rational_floor_family(vb, law, ctx, intro_names) {
        return Some(proof);
    }
    // `simp only [<cone>]` unfolds the cone — but RECURSIVE cone fns (`pow2`)
    // must be EXCLUDED: `simp only [pow2]` errors trying to reduce the recursive
    // match on a symbolic argument, failing the whole arm. They stay abstract
    // and the pool law (the `pow2` homomorphism) reasons about them instead.
    // `law_simp_defs` emits fully-qualified `_root_.<name>` simp targets, but
    // `recursive_pure_fn_names` (via `aver_name_to_lean`) yields bare names. Strip
    // the `_root_.` namespace prefix on BOTH sides before comparing, or the
    // recursive filter silently misses (`"_root_.pow2" != "pow2"`) and a
    // recursive cone fn leaks into `simp only`, breaking simp on its symbolic
    // recursive match (the measured `e2` failure mode).
    // Compare on the BARE basename. `recursive_pure_fn_names` yields bare names
    // (`pow2`), but `law_simp_defs` QUALIFIES a dep-module fn (`Domain.Fprep.pow2`,
    // and entry fns carry a `_root_.` prefix). Stripping only `_root_.` left a dep
    // recursive fn (`Domain.Fprep.pow2`) unmatched against the bare `pow2`, so it
    // leaked into `simp only [...]` — and `simp only` on a recursive fn's symbolic
    // match ERRORS, failing the whole keystone arm (the cross-module miss that
    // sent every dep-cone conditional law to its bounded fallback). Drop the
    // `_root_.` prefix AND the module path on both sides so a dep recursive cone
    // fn is excluded exactly like an entry one.
    fn bare_basename(n: &str) -> &str {
        let n = n.strip_prefix("_root_.").unwrap_or(n);
        n.rsplit('.').next().unwrap_or(n)
    }
    let mut abstract_fns: std::collections::HashSet<String> =
        super::super::recursive_pure_fn_names(ctx)
            .iter()
            .map(|n| bare_basename(&aver_name_to_lean(n)).to_string())
            .collect();
    // Also keep FOLDED any cone fn that a CITED pool law is keyed on: that
    // law is a rewrite ABOUT the fn (the floorDiv exact-cancel is about
    // `floorDiv`), so it can only e-match while the fn is still abstract —
    // the same reason a recursive cone fn (the `pow2` homomorphism's `pow2`)
    // is excluded. Without this the `simp only` would unfold `floorDiv` to its
    // `Result.withDefault`/`Int.div` body and the cancel citation would find
    // nothing to fire on.
    for subj in keystone_pool_subject_fns(vb, law, ctx) {
        abstract_fns.insert(bare_basename(&subj).to_string());
    }
    // SIGNED power-of-two homomorphism: when an earlier law proves the
    // homomorphism of a `Fraction`-valued signed `2^k` cone fn `F` and the goal
    // applies `F` to a sum of exponents, keep `F` FOLDED (like a recursive cone
    // fn) so the cited cross-multiply canonicalizer can e-match its `F(m+n)` /
    // `F(m+n+1)` terms — unfolding `F` would bury the float-product `if` inside
    // `F`'s sign match and explode the split. Name-blind via the recognizer.
    let pow2signed = keystone_pow2signed_cite(vb, law, ctx);
    if let Some(cite) = &pow2signed {
        abstract_fns.insert(bare_basename(&cite.sgn).to_string());
    }
    let defs: Vec<String> = law_simp_defs(ctx, vb, law)
        .into_iter()
        .filter(|d| !abstract_fns.contains(bare_basename(d)))
        .collect();
    let defs_csv = defs.join(", ");
    // `grind` gets the laws-as-lemmas pool names (not the cone defs): the `simp`
    // already unfolds the non-recursive cone, and feeding grind the recursive defs
    // risks it re-unfolding into a loop. Two pools, both fed to the same `grind`:
    //   * EQUATIONAL — earlier laws whose subject fn is in the cone (e.g. a `pow2`
    //     homomorphism). grind e-matches each, instantiates it, discharges its
    //     premise from context, and closes the residual ring/linear identity.
    //   * ORDER-BRIDGE — each cited inequality law, restated in Prop form with a
    //     `grind_pattern` over its two comparison sides (emitted as `support_lines`
    //     BEFORE this theorem). The pattern fires when grind sees those sides in
    //     the goal/premises, instantiating the order fact (e.g. `a*c ≤ b*c`) that
    //     omega/grind then chains with a premise (`b*c ≤ m`) to the conclusion.
    // Drop self-registering (`grind_pattern`-annotated, so already `@[grind]`)
    // cancel laws from the explicit hint list — `grind` picks them up globally,
    // and re-listing them only warns. They stay folded via `abstract_fns` above
    // so their floor term survives for the pattern to e-match.
    let self_registering = keystone_self_registering_pool_names(vb, law, ctx);
    let pool_names: Vec<String> = keystone_pool_names(vb, law, ctx)
        .into_iter()
        .filter(|n| !self_registering.contains(n))
        .collect();
    let order_citations = keystone_order_bridge_citations(vb, law, ctx);
    let mut hints: Vec<String> = pool_names;
    hints.extend(order_citations.iter().map(|c| c.name.clone()));
    let mut support_lines: Vec<String> = Vec::new();
    for citation in &order_citations {
        support_lines.extend(citation.support.iter().cloned());
    }

    // GENERIC pow2-of-nonneg-linear-form normalizer. When the cited pool
    // reasons about a power-of-two fn (any `is_pow2_shape` fn, detected via the
    // cited law's `FloorDivWindow` figure — name-blind), prepend its normalizer
    // support stack (the homomorphism keyed on the PRODUCT side + the successor
    // step, both with `grind_pattern`s) and add those lemmas to the grind hints.
    // This is what lets `grind` canonicalize `pow` of a REARRANGED / shifted
    // exponent — `pow (w_a + w_b - 2)` unifies with `pow (w_a-1) * pow (w_b-1)`
    // by the product pattern + linear-arith congruence, and an odd offset peels
    // via the successor pattern — closing the homomorphism COMPOSITION (e.g. the
    // folklore `fpMulValue`) that the default LHS (`pow (m+n)`) e-match misses on
    // a subtraction. Scoped to a fresh per-law prefix: no global pattern clash.
    let pow2_normalizer = keystone_pow2_fn(vb, law, ctx).map(|pow| {
        let nlbase = format!(
            "{}_law_{}__pow2lf",
            aver_name_to_lean(&vb.fn_name),
            law.name
        );
        let support =
            super::super::floor_window::pow2_linear_form_normalizer_support(&nlbase, &pow);
        (nlbase, support)
    });
    if let Some((nlbase, support)) = &pow2_normalizer {
        support_lines.extend(support.lines().map(|l| l.to_string()));
        hints.push(format!("{nlbase}__pow_add"));
        hints.push(format!("{nlbase}__pow_succ"));
    }
    // GENERIC signed-power-of-two homomorphism normalizer (the `Fraction`-level
    // companion of the `pow2lf` integer normalizer above). Emits a self-contained
    // support stack about the signed fn `F` — its positivity, `F(1) = 2/1`, and
    // the cross-multiplied homomorphism CITED from the proven Aver law, keyed on
    // the SUM side `F(m+n)` / `F(m+n+1)` — so `grind` canonicalizes `F` of a
    // summed/shifted exponent the way the integer normalizer canonicalizes `pow`
    // of a summed/shifted one. Scoped to a fresh per-law prefix.
    if let Some(cite) = &pow2signed {
        let sgnbase = format!(
            "{}_law_{}__pow2sgn",
            aver_name_to_lean(&vb.fn_name),
            law.name
        );
        let support = super::super::floor_window::pow2_signed_homomorphism_normalizer_support(
            &sgnbase,
            &cite.pow,
            &cite.sgn,
            &cite.hom_fn,
            &cite.cited,
        );
        support_lines.extend(support.lines().map(|l| l.to_string()));
        hints.push(format!("{sgnbase}__sgn_add"));
        hints.push(format!("{sgnbase}__sgn_add_succ"));
    }
    let grind_hints = hints.join(", ");
    // With no citable pool and no normalizer the hint list is empty; emit a bare
    // `grind` (not `grind []`) for the flat definitional close.
    let grind_call = if grind_hints.is_empty() {
        "grind".to_string()
    } else {
        format!("grind [{grind_hints}]")
    };

    // A non-conditional law (the unconditional signed pow2 homomorphism) has no
    // premise to introduce, so `intro` binds only the givens and `simp` rewrites
    // the goal alone (`at ⊢`); a `when`-law also introduces and simplifies the
    // premise hypothesis (`at h_when ⊢`).
    let (intro, simp_at) = if law.when.is_some() {
        (
            format!("  intro {} h_when", intro_names.join(" ")),
            "at h_when ⊢",
        )
    } else {
        (format!("  intro {}", intro_names.join(" ")), "at ⊢")
    };
    // Bridge set matches the de-risked `e1`: `Bool.and_eq_true` splits the `&&`
    // guard, `decide_eq_true_eq` peels `decide p = true` to `p`. The earlier
    // `ge_iff_le, gt_iff_lt` are dropped — they are unnecessary for this shape
    // (`grind` orders relations itself) and `e1` closes without them.
    //
    // The signed-power-of-two homomorphism path adds `beq_iff_eq`: with `F`
    // FOLDED the goal stays a `Fraction` `sameValue` (a `==`), and turning it into
    // a Prop `=` lets `grind`'s commutative-ring solver consume the cited
    // cross-multiply facts (it rejects a bare `decide (… == …)` wrapper). Added
    // ONLY on the signed path — the integer-only pow2 laws close without it, so
    // their emission stays byte-identical.
    let bridge_extra = if pow2signed.is_some() {
        ", beq_iff_eq"
    } else {
        ""
    };
    let close = if pow2_normalizer.is_some() {
        // A multiplication-value law (`fpMul`) normalizes its significand with
        // an `if`-branch (shift 0 vs 1); `repeat' split` discharges that branch
        // before `grind`, and the `first | (repeat' split <;> …) | …` covers both
        // the branching and the flat (no-`if`) shapes uniformly — the bare `grind`
        // arm is the path the scaling / product-exponent laws (no `if`) take.
        // `repeat'` (not a single `split`) is load-bearing for the SIGNED pow2
        // homomorphism: its goal carries THREE independent `pow2Signed` sign
        // matches (for `m`, `n`, and `m+n`), and `grind` does not case-split the
        // residual matches itself — every sign arm must be peeled before `grind`
        // sees a product of `pow2` of nonneg atoms the normalizer can canonicalize.
        // For a single-match law `repeat' split` peels exactly the one match, so
        // the fpMul / fpScale / product-exponent laws are unaffected.
        format!(
            "  | (simp only [{defs_csv}, Bool.and_eq_true, decide_eq_true_eq{bridge_extra}] {simp_at} <;> (first | ((repeat' split) <;> {grind_call}) | {grind_call}))"
        )
    } else {
        format!(
            "  | (simp only [{defs_csv}, Bool.and_eq_true, decide_eq_true_eq{bridge_extra}] {simp_at} <;> {grind_call})"
        )
    };
    let floor = if super::super::super::tactic_ir::speculative::probing() {
        let id = format!("{}.{}", vb.fn_name, law.name);
        super::super::super::tactic_ir::speculative::record_probed(&id);
        format!("  | (trace \"AVERSPEC_SORRY:{id}\"; sorry)")
    } else {
        "  | sorry".to_string()
    };
    Some(AutoProof {
        support_lines,
        body: Tactic::raw(vec![intro, "  first".to_string(), close, floor]),
        replaces_theorem: false,
    })
}
