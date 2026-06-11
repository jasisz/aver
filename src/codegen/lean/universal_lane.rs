//! When-universal quarantine lane — genuine universal proofs for
//! scalar-sign `when`-laws, emitted OUTSIDE the counted build.
//!
//! Today a `when`-law's manifest theorem is domain-bounded by
//! construction (`law_theorem_prop` prepends the sampled-domain
//! disjunctions), so even a tactic-proved `when`-law only ever earns
//! `universal:false`. This module emits, for a narrow validated
//! family, a TWIN theorem in the TRUE universal form
//! `∀ givens, <when> = true -> claim` into a quarantined, NON-DEFAULT
//! `lean_lib` (one lib + one hashed module per law, srcDir
//! `universal_lane/`, importing the manifest entry root).
//!
//! IRON GUARD — budget inflation is mechanically impossible:
//! - the manifest pipeline is untouched: the law keeps its
//!   `BackendDispatch` strategy and its bounded guarded-domain
//!   theorem byte-for-byte (this module only ADDS files);
//! - lane scripts contain ZERO `sorry` tokens (enforced by a debug
//!   assertion here and a grep test in `proof_spec`) — there is
//!   nothing for the sorry budget to count even in principle;
//! - the counted `lake build` only builds the `@[default_target]`
//!   lib; lane libs are built by SEPARATE, failure-TOLERATED
//!   invocations at per-law granularity (`aver proof --check`), so a
//!   hard failure (elaboration error, maxHeartbeats) costs exactly
//!   "that law stays bounded" — budgets, `passed` and neighbors are
//!   out of reach by construction;
//! - credit keys on PER-DECLARATION evidence (`#print axioms` ⊆
//!   {propext, Classical.choice, Quot.sound} against the built lane
//!   module), never on an invocation exit code; module names are
//!   content-hashed so a stale `.olean` can never masquerade as a
//!   fresh success.
//!
//! The prover content comes in two recognized families:
//!
//! FAMILY 1 — scalar-sign: the law's `when` is a sign guard (`n > 0` /
//! `n < 0` / `n >= 0`) on a single Int given, and the LHS enters the
//! SAME canonical decimal parser pipeline an `IntDecimalRoundtrip`
//! pin (#470) already certified for the file — the lane proof is a
//! segment/polarity parameterization of that skeleton
//! (`law_auto/decimal.rs`), with the premise consumed ONCE at the
//! existing `rcases` sign split. Exactly the five empirically
//! validated (kernel-clean on the emitted json project, Lean 4.15)
//! entry-segment × polarity combinations are recognized; everything
//! else declines at zero cost:
//! - digit-dispatch wrapper entry, `when n >= 0` (dispatchNumberOrErr)
//! - `pos_fn` entry, `when n > 0` (startNumberDigits)
//! - `neg_fn` entry, `when n < 0` (parseNumberSign)
//! - `sign_fn` entry, `when n < 0` (startSignDigit)
//! - scanner entry with pinned `n == 0` Bool arg, `when n >= 0`
//!   (scanIntTail)
//!
//! FAMILY 2 — bridge-shaped premise: the `when` is `boolRel(a, b) =
//! true` where `boolRel` is a recursive Bool fn mirroring a Prop
//! relation — concretely a canonical Peano structural equality
//! (`natEq`-shape, lifted to builtin `Nat`), possibly negated through
//! `Bool.not` or a 2-arm not-wrapper fn. The probe-proven mechanics
//! (TIP prop_85 hand proof, kernel-genuine `[propext, Quot.sound]`):
//! (1) a use-side Bool→Prop inversion bridge `(natEq a b = true) → a
//! = b`; (2) a REINTRODUCTION bridge `natEq a a = true` (without it
//! the emitter cannot instantiate its own induction hypothesis);
//! (3) per-step premise stepping (`simp only [measure-fns] at h` +
//! `omega`, re-bridged for the structurally smaller call); (4) for
//! the zip-rev figure, a snoc-distribution aux lemma (zip over
//! append-singleton under length equality) emitted as a lane-local
//! lemma from the validated template — never as a sorry. Exactly the
//! hand-validated figures are recognized (see [`BridgePlan`]);
//! everything else declines. An ACL2-style free-variables gate
//! additionally requires vars(when) ⊆ vars(lhs) — a premise variable
//! unbound by the conclusion's match side would make conditional
//! rewriting guess, so such laws decline instead.
//!
//! Paid-for landmines from the hand-proof probe, baked into the
//! rendered tactic text:
//! - the rendered premise is decide-coerced (`(n > 0) = true`) —
//!   normalized via `of_decide_eq_true h_when` with a `simpa`
//!   fallback;
//! - Lean 4.15 `omega` ATOMIZES `Int.ofNat m` / `Int.negSucc m` —
//!   the zero-case discharge goes `intro h0; subst h0; exact absurd
//!   hn (by decide)`, and vacuous branches pair the core
//!   `Int.negSucc_lt_zero` / `Int.ofNat_nonneg` facts with `omega`
//!   over the shared atom;
//! - the one genuinely new lemma vs the #470 skeleton is the
//!   slice-head bridge `String.slice s 0 1 = Char.toString
//!   (digitChar d)` (and its `1 2` negative twin), closing by
//!   `rw [hmk]; rfl`.
//!
//! TEST HOOKS (env vars read in `cmd_proof_lean`, fail-safe direction
//! only): `AVER_PROOF_LANE_SABOTAGE=<label>` breaks one lane module's
//! proof (the tolerated per-law build fails, that law stays bounded,
//! the counted build is untouched — the iron-guard test);
//! `AVER_PROOF_NO_UNIVERSAL_LANE=1` disables the lane entirely and
//! retires a stale index. Neither can grant credit, only withhold it.

use super::expr::{aver_name_to_lean, emit_expr_legacy};
use crate::ast::{
    BinOp, Expr, FnDef, Literal, Pattern, Spanned, Stmt, TopLevel, VerifyBlock, VerifyLaw,
};
use crate::codegen::CodegenContext;

/// Subdirectory (relative to the proof output dir) holding lane
/// modules. Deliberately NOT the dir root: `lean_universal_proof`'s
/// non-recursive scan of root `.lean` files must never see lane
/// theorems, so the file-level `universal` flag keeps its
/// counted-build semantics.
pub const LANE_SUBDIR: &str = "universal_lane";

/// Machine-readable index of emitted lane laws, written to the proof
/// output dir. `aver proof --check` consumes it to run the
/// failure-tolerated per-law builds and the `#print axioms` crediting
/// probes; its absence simply means `when_universal: 0`.
pub const LANE_MANIFEST_FILE: &str = "_aver_universal_lane.json";

/// One emitted lane law: a single hashed Lean module hosting the
/// universal twin theorem, exposed as its own non-default `lean_lib`.
pub struct LaneLawFile {
    /// `fn.law` label for surfacing (`dispatchNumberOrErr.fromIntRoundtrip`).
    pub label: String,
    /// Twin theorem name (`<fn>_law_<law>_universal`).
    pub theorem: String,
    /// Module = file stem = lib name, content-hashed so a stale
    /// `.olean` under an old name is unreachable.
    pub module: String,
    /// Full `.lean` source. Contains NO `sorry` token.
    pub content: String,
}

/// The five validated entry-segment × polarity combinations. Each
/// renders a dedicated, hand-validated proof template; recognizing
/// only these is what keeps lane build failures (tolerated but noisy)
/// rare.
enum LanePlan {
    /// `wrapper(ser(C(n)), 0, slice(ser(C(n)), 0, 1))`, `when n >= 0`;
    /// wrapper = `match pred(c) { true -> parse(s, pos), false -> _ }`.
    DigitDispatchNonNeg,
    /// `pos_fn(ser(C(n)), 0, slice(ser(C(n)), 0, 1))`, `when n > 0`.
    PosSegmentPos,
    /// `neg_fn(ser(C(n)), 1, 0)`, `when n < 0`.
    NegSegmentNeg,
    /// `sign_fn(ser(C(n)), 1, 0, slice(ser(C(n)), 1, 2))`, `when n < 0`.
    SignSegmentNeg,
    /// `scanner(ser(C(n)), 1, 0, n == 0)`, `when n >= 0`.
    ScannerNonNeg,
}

/// Premise polarity, recognized from the `when` expression.
#[derive(Clone, Copy, PartialEq)]
enum Polarity {
    Pos,
    Neg,
    NonNeg,
}

/// Names captured by the file's `IntDecimalRoundtrip` IR pin (source
/// names; the lane re-reads them from `proof_ir.law_theorems` — the
/// manifest strategy itself is never touched).
struct PinNames {
    parse_fn: String,
    neg_fn: String,
    pos_fn: String,
    sign_fn: String,
    scanner_fn: String,
    predicate_fn: String,
    finish_fn: String,
    finish_int_fn: String,
    serializer_fn: String,
}

/// Names the rendered skeleton binds; a colliding `given` would be
/// shadowed mid-proof. Superset of the manifest skeleton's reserved
/// list (`proof_lower::detect_int_decimal_roundtrip`) plus the
/// lane-only hypothesis names.
const LANE_RESERVED: &[&str] = &[
    "m",
    "d",
    "ds",
    "x",
    "hx",
    "hm",
    "hnd",
    "hsl",
    "hch",
    "hch0",
    "hch1",
    "hlen",
    "hmk",
    "hds10",
    "hdigits",
    "hfuel",
    "harm",
    "harm0",
    "heq",
    "hdisp1",
    "hts",
    "hfin",
    "h0",
    "h1",
    "h2",
    "hlen0",
    "hslice",
    "h_when",
    "hn",
    "hb",
    "hheadslice",
    "hnn",
    "hneg",
    "ch",
    "hc",
    "k",
];

/// Generate the lane files for every recognized when-law in the
/// entry scope. `entry_content` (the emitted manifest entry `.lean`)
/// is folded into each module's content hash so any manifest change
/// retires the old module names. `sabotage` is a TEST-ONLY hook
/// (`AVER_PROOF_LANE_SABOTAGE`): a label substring whose matching
/// law gets a deliberately failing tactic injected — the executable
/// proof that one broken lane proof cannot touch budgets or
/// neighbors.
pub fn generate(
    ctx: &CodegenContext,
    entry_content: &str,
    sabotage: Option<&str>,
) -> Vec<LaneLawFile> {
    let pins = collect_pins(ctx);
    let entry_root = crate::codegen::common::entry_basename(ctx);
    let mut out = Vec::new();
    for item in &ctx.items {
        let TopLevel::Verify(vb) = item else { continue };
        let crate::ast::VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let sabotage_this = sabotage
            .is_some_and(|s| format!("{}.{}", vb.fn_name, law.name).contains(s) && !s.is_empty());
        // Family 1: scalar-sign over an IntDecimalRoundtrip pin.
        let mut rendered = false;
        for pin in &pins {
            let Some(plan) = classify_lane_law(vb, law, ctx, pin) else {
                continue;
            };
            if let Some(file) = render_lane_law(
                vb,
                law,
                ctx,
                pin,
                &plan,
                &entry_root,
                entry_content,
                sabotage_this,
            ) {
                out.push(file);
                rendered = true;
            }
            break; // first matching pin wins
        }
        if rendered {
            continue;
        }
        // Family 2: bridge-shaped premise (recursive Bool equality
        // bridge over a canonical Peano type).
        if let Some(plan) = classify_bridge_law(vb, law, ctx)
            && let Some(file) = render_bridge_law(
                vb,
                law,
                ctx,
                &plan,
                &entry_root,
                entry_content,
                sabotage_this,
            )
        {
            out.push(file);
        }
    }
    out
}

/// Append one non-default `lean_lib` per lane law to the generated
/// lakefile. Appended AFTER the `@[default_target]` lib: the counted
/// `lake build` never builds these, and `lean_lakefile_roots` (which
/// keys the `#print axioms` prober for the counted build) reads only
/// the FIRST `roots :=` line, so the file-level `universal` flag
/// semantics are untouched.
pub fn lakefile_with_lane_libs(lakefile: &str, lane: &[LaneLawFile]) -> String {
    let mut out = lakefile.to_string();
    for law in lane {
        out.push_str(&format!(
            "\nlean_lib «{}» where\n  srcDir := \"{}\"\n  roots := #[`{}]\n",
            law.module, LANE_SUBDIR, law.module
        ));
    }
    out
}

/// The machine-readable lane index (`LANE_MANIFEST_FILE` content).
pub fn lane_manifest_json(lane: &[LaneLawFile]) -> String {
    let laws: Vec<serde_json::Value> = lane
        .iter()
        .map(|l| {
            serde_json::json!({
                "law": l.label,
                "theorem": l.theorem,
                "module": l.module,
            })
        })
        .collect();
    serde_json::to_string_pretty(&serde_json::json!({
        "version": 1,
        "laws": laws,
    }))
    .unwrap_or_else(|_| "{}".to_string())
}

fn collect_pins(ctx: &CodegenContext) -> Vec<PinNames> {
    ctx.proof_ir
        .law_theorems
        .iter()
        .filter_map(|t| match &t.strategy {
            crate::ir::ProofStrategy::IntDecimalRoundtrip {
                parse_fn,
                neg_fn,
                pos_fn,
                sign_fn,
                scanner_fn,
                predicate_fn,
                finish_fn,
                finish_int_fn,
                serializer_fn,
            } => Some(PinNames {
                parse_fn: parse_fn.clone(),
                neg_fn: neg_fn.clone(),
                pos_fn: pos_fn.clone(),
                sign_fn: sign_fn.clone(),
                scanner_fn: scanner_fn.clone(),
                predicate_fn: predicate_fn.clone(),
                finish_fn: finish_fn.clone(),
                finish_int_fn: finish_int_fn.clone(),
                serializer_fn: serializer_fn.clone(),
            }),
            _ => None,
        })
        .collect()
}

fn ident_of(e: &Spanned<Expr>) -> Option<&str> {
    match &e.node {
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
        _ => None,
    }
}

fn call_of(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
    match &e.node {
        Expr::FnCall(callee, args) => Some((
            crate::codegen::common::expr_to_dotted_name(&callee.node)?,
            args.as_slice(),
        )),
        Expr::TailCall(data) => Some((data.target.clone(), data.args.as_slice())),
        _ => None,
    }
}

fn ctor_of(e: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
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

fn is_int_lit(e: &Spanned<Expr>, v: i64) -> bool {
    matches!(&e.node, Expr::Literal(Literal::Int(n)) if *n == v)
}

/// `String.slice(<ser>, a, b)` with the SAME serializer expression the
/// law's first argument carries.
fn is_ser_slice(e: &Spanned<Expr>, ser_arg: &Spanned<Expr>, a: i64, b: i64) -> bool {
    let Some((callee, args)) = call_of(e) else {
        return false;
    };
    callee == "String.slice"
        && args.len() == 3
        && args[0].node == ser_arg.node
        && is_int_lit(&args[1], a)
        && is_int_lit(&args[2], b)
}

/// Sign-guard polarity of the `when` expression over `given`:
/// `n > 0` / `0 < n` → Pos, `n < 0` / `0 > n` → Neg,
/// `n >= 0` / `0 <= n` → NonNeg. Anything else declines.
fn when_polarity(when: &Spanned<Expr>, given: &str) -> Option<Polarity> {
    let Expr::BinOp(op, l, r) = &when.node else {
        return None;
    };
    let direct = ident_of(l) == Some(given) && is_int_lit(r, 0);
    let mirrored = is_int_lit(l, 0) && ident_of(r) == Some(given);
    match (op, direct, mirrored) {
        (BinOp::Gt, true, false) | (BinOp::Lt, false, true) => Some(Polarity::Pos),
        (BinOp::Lt, true, false) | (BinOp::Gt, false, true) => Some(Polarity::Neg),
        (BinOp::Gte, true, false) | (BinOp::Lte, false, true) => Some(Polarity::NonNeg),
        _ => None,
    }
}

/// Validate one when-law against one `IntDecimalRoundtrip` pin.
/// Mirrors the manifest detector's law-shape gates (serializer
/// sub-term, single-Int-field constructor, `Ok(C(n), String.len(ser))`
/// rhs) and then matches the LHS against exactly the five validated
/// entry-segment shapes. Any deviation declines — the law simply
/// stays bounded, manifest bytes untouched.
fn classify_lane_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    pin: &PinNames,
) -> Option<LanePlan> {
    let when = law.when.as_ref()?;
    if law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return None;
    }
    let given = law.givens[0].name.as_str();
    if LANE_RESERVED.contains(&given) {
        return None;
    }
    let polarity = when_polarity(when, given)?;

    // ---- law shape: lhs enters the pipeline on the serializer ------
    let (lhs_callee, lhs_args) = call_of(&law.lhs)?;
    if lhs_callee.rsplit('.').next()? != vb.fn_name {
        return None;
    }
    let ser_arg = lhs_args.first()?;
    let (ser_name, ser_args) = call_of(ser_arg)?;
    if ser_name != pin.serializer_fn || ser_args.len() != 1 {
        return None;
    }
    let ctor_expr = &ser_args[0];
    let (ctor_name, ctor_args) = ctor_of(ctor_expr)?;
    if ctor_args.len() != 1 || ident_of(ctor_args[0]) != Some(given) {
        return None;
    }
    // Serializer must carry the `C(x) -> String.fromInt(x)` arm — the
    // same gate the manifest detector validated for ITS law's ctor;
    // re-checked here because this law may name a different variant.
    {
        let fd = ctx.fn_def_by_name(&pin.serializer_fn, None)?;
        if !fd.effects.is_empty() {
            return None;
        }
        let [Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let Expr::Match { arms, .. } = &body.node else {
            return None;
        };
        arms.iter()
            .any(|a| {
                let Pattern::Constructor(n, binders) = &a.pattern else {
                    return false;
                };
                if n != &ctor_name || binders.len() != 1 {
                    return false;
                }
                call_of(&a.body).is_some_and(|(callee, args)| {
                    callee == "String.fromInt"
                        && args.len() == 1
                        && ident_of(&args[0]) == Some(binders[0].as_str())
                })
            })
            .then_some(())?;
    }
    // rhs: `Ok(C(n), String.len(ser(C(n))))`.
    let (_, rhs_args) = ctor_of(&law.rhs)?;
    if rhs_args.len() != 2 || rhs_args[0].node != ctor_expr.node {
        return None;
    }
    let (len_callee, len_args) = call_of(rhs_args[1])?;
    if len_callee != "String.len" || len_args.len() != 1 || len_args[0].node != ser_arg.node {
        return None;
    }

    // ---- entry segment (the five validated combos only) ------------
    if vb.fn_name == pin.pos_fn {
        return (polarity == Polarity::Pos
            && lhs_args.len() == 3
            && is_int_lit(&lhs_args[1], 0)
            && is_ser_slice(&lhs_args[2], ser_arg, 0, 1))
        .then_some(LanePlan::PosSegmentPos);
    }
    if vb.fn_name == pin.neg_fn {
        return (polarity == Polarity::Neg
            && lhs_args.len() == 3
            && is_int_lit(&lhs_args[1], 1)
            && is_int_lit(&lhs_args[2], 0))
        .then_some(LanePlan::NegSegmentNeg);
    }
    if vb.fn_name == pin.sign_fn {
        return (polarity == Polarity::Neg
            && lhs_args.len() == 4
            && is_int_lit(&lhs_args[1], 1)
            && is_int_lit(&lhs_args[2], 0)
            && is_ser_slice(&lhs_args[3], ser_arg, 1, 2))
        .then_some(LanePlan::SignSegmentNeg);
    }
    if vb.fn_name == pin.scanner_fn {
        let pinned_zero_eq = lhs_args.len() == 4
            && matches!(&lhs_args[3].node, Expr::BinOp(BinOp::Eq, l, r)
                if ident_of(l) == Some(given) && is_int_lit(r, 0));
        return (polarity == Polarity::NonNeg
            && pinned_zero_eq
            && is_int_lit(&lhs_args[1], 1)
            && is_int_lit(&lhs_args[2], 0))
        .then_some(LanePlan::ScannerNonNeg);
    }
    // Digit-dispatch wrapper: `match pred(c) { true -> parse(s, pos),
    // false -> _ }` over params (s, pos, c).
    if polarity == Polarity::NonNeg
        && lhs_args.len() == 3
        && is_int_lit(&lhs_args[1], 0)
        && is_ser_slice(&lhs_args[2], ser_arg, 0, 1)
    {
        let fd = ctx.fn_def_by_name(&vb.fn_name, None)?;
        if !fd.effects.is_empty() || fd.params.len() != 3 {
            return None;
        }
        let [Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let Expr::Match { subject, arms } = &body.node else {
            return None;
        };
        let (pred, pred_args) = call_of(subject)?;
        if pred != pin.predicate_fn
            || pred_args.len() != 1
            || ident_of(&pred_args[0]) != Some(fd.params[2].0.as_str())
            || arms.len() != 2
            || !arms
                .iter()
                .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))
        {
            return None;
        }
        let true_arm = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))?;
        let (callee, args) = call_of(&true_arm.body)?;
        return (callee == pin.parse_fn
            && args.len() == 2
            && ident_of(&args[0]) == Some(fd.params[0].0.as_str())
            && ident_of(&args[1]) == Some(fd.params[1].0.as_str()))
        .then_some(LanePlan::DigitDispatchNonNeg);
    }
    None
}

/// FNV-1a 64-bit — content hash for module names.
fn fnv1a64(bytes: &[u8]) -> u64 {
    let mut h: u64 = 0xcbf29ce484222325;
    for b in bytes {
        h ^= *b as u64;
        h = h.wrapping_mul(0x100000001b3);
    }
    h
}

/// Content-hashed lane module name (`U_<theorem-base>_<hash>`): the
/// hash covers the module's own content AND the manifest entry file,
/// so a stale `.olean` under an old name is unreachable after any
/// change to either. Shared by both lane families.
fn lane_module_id(theorem_base: &str, content: &str, entry_content: &str) -> String {
    let hash = fnv1a64(content.as_bytes()) ^ fnv1a64(entry_content.as_bytes()).rotate_left(1);
    let sanitized: String = theorem_base
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect();
    format!("U_{sanitized}_{:08x}", (hash & 0xffff_ffff) as u32)
}

#[allow(clippy::too_many_arguments)]
fn render_lane_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    pin: &PinNames,
    plan: &LanePlan,
    entry_root: &str,
    entry_content: &str,
    sabotage: bool,
) -> Option<LaneLawFile> {
    let emit = |e: &Spanned<Expr>| emit_expr_legacy(e, ctx, None);

    let fn_lean = aver_name_to_lean(&vb.fn_name);
    let law_lean = aver_name_to_lean(&law.name);
    let theorem_base = format!("{fn_lean}_law_{law_lean}");
    let theorem = format!("{theorem_base}_universal");

    let n = aver_name_to_lean(&law.givens[0].name);
    let lhs_template = emit(&law.lhs);
    let rhs_template = emit(&law.rhs).replace('\n', " ");
    let when_template = emit(law.when.as_ref()?).replace('\n', " ");

    // Same statement builder as the manifest theorem, minus the
    // sampled-domain disjunctions (`omit_domain`) — the twin is
    // provably the manifest claim with the bounding premises removed.
    let lifted = std::collections::HashMap::new();
    let (prop, bounded) = super::toplevel::law_theorem_prop(
        law,
        ctx,
        &lhs_template,
        &rhs_template,
        Some(&when_template),
        &lifted,
        true,
    );
    debug_assert!(!bounded);
    let quant_params = format!("({n} : Int)");

    // Names for the proof body.
    let Expr::FnCall(_, lhs_args) = &law.lhs.node else {
        return None;
    };
    let ser_arg = lhs_args.first()?;
    let ser_text = emit(ser_arg);
    if !rhs_template.contains(&ser_text) {
        return None;
    }
    let rhs = rhs_template.replace(&ser_text, &format!("(String.fromInt {n})"));

    let parse = aver_name_to_lean(&pin.parse_fn);
    let neg = aver_name_to_lean(&pin.neg_fn);
    let posf = aver_name_to_lean(&pin.pos_fn);
    let sign = aver_name_to_lean(&pin.sign_fn);
    let scan = aver_name_to_lean(&pin.scanner_fn);
    let pred = aver_name_to_lean(&pin.predicate_fn);
    let finish = aver_name_to_lean(&pin.finish_fn);
    let fint = aver_name_to_lean(&pin.finish_int_fn);
    let scan_lemma = format!(
        "{}_scan",
        crate::codegen::recursion::fuel_helper_name(&pin.scanner_fn)
    );
    let pred_lemma = format!("{theorem}_digit_pred");

    let mut ser_simp: Vec<String> = vec![aver_name_to_lean(&pin.serializer_fn)];
    for name in super::toplevel::law_fuel_simp_names(&pin.serializer_fn, ctx) {
        if !ser_simp.contains(&name) {
            ser_simp.push(name);
        }
    }
    let ser_simp = ser_simp.join(", ");

    // ---- shared proof blocks ---------------------------------------
    let prologue = format!(
        r#"intro {n} h_when
have hn : {when_template} := by
  first
    | exact of_decide_eq_true h_when
    | simpa using h_when
have hts : {ser_text} = String.fromInt {n} := by
  first
    | rfl
    | simp [{ser_simp}]
rw [hts]
have hfin : {finish} (String.fromInt {n}) 0 ((String.fromInt {n}).data.length : Int) false
    = {rhs} := by
  have h1 : ¬ (((String.fromInt {n}).data.length : Int) < 0) := by omega
  have hslice : String.slice (String.fromInt {n}) 0 ((String.fromInt {n}).data.length : Int) = String.fromInt {n} := by
    simp [String.slice, String.toList, h1]
  have hlen0 : (String.fromInt {n}).data.length = (String.fromInt {n}).length := rfl
  have h2 : {finish} (String.fromInt {n}) 0 ((String.fromInt {n}).data.length : Int) false
      = {fint} (String.slice (String.fromInt {n}) 0 ((String.fromInt {n}).data.length : Int)) ((String.fromInt {n}).data.length : Int) := by
    simp [{finish}]
  rw [h2, hslice]
  simp [{fint}, Int.fromString_fromInt {n}, hlen0]
rcases {n} with m | m"#
    );

    let ofnat_vacuous = r#"· exfalso
  first
    | omega
    | (have hnn : 0 ≤ Int.ofNat m := Int.ofNat_nonneg m
       omega)
    | (have hnn : 0 ≤ Int.ofNat m := Int.ofNat_zero_le m
       omega)"#
        .to_string();

    let negsucc_vacuous = r#"· exfalso
  first
    | omega
    | (have hneg : Int.negSucc m < 0 := Int.negSucc_lt_zero m
       omega)"#
        .to_string();

    // Zero sub-case (`m = 0` under `hm`, already substituted): the
    // whole pipeline computes on the closed string "0".
    let zero_case = r#"subst hm
have h0 : String.fromInt (Int.ofNat 0) = "0" := by
  show String.mk (AverDigits.natDigitsChars 0) = "0"
  unfold AverDigits.natDigitsChars
  rw [AverDigits.natDigits.eq_1]
  decide
rw [h0]
rfl"#
        .to_string();

    // ofNat-nonzero head facts (digit-list exposure).
    let pos_head = r#"have hsl : (String.fromInt (Int.ofNat m)).data = (AverDigits.natDigits m).map AverDigits.digitChar := rfl
rcases hnd : AverDigits.natDigits m with _ | ⟨d, ds⟩
· exact absurd hnd (AverDigits.natDigits_nonempty m)
· have hd10 : d < 10 := AverDigits.natDigits_digits_lt_ten m d (by rw [hnd]; exact List.mem_cons_self _ _)
  have hdne0 : d ≠ 0 := AverDigits.natDigits_head_ne_zero m hm d ds hnd
  have hlen : (String.fromInt (Int.ofNat m)).data.length = ds.length + 1 := by
    rw [hsl, hnd]; simp"#
        .to_string();

    let pos_hmk = r#"have hmk : String.fromInt (Int.ofNat m) = String.mk ((d :: ds).map AverDigits.digitChar) := by
  rw [← hnd]
  rfl"#
        .to_string();

    let pos_hch = r#"have hch : String.charAt (String.fromInt (Int.ofNat m)) 0
    = some (Char.toString (AverDigits.digitChar d)) := by
  rw [hmk]
  rfl"#
        .to_string();

    let pos_headslice = r#"have hheadslice : String.slice (String.fromInt (Int.ofNat m)) 0 1
    = Char.toString (AverDigits.digitChar d) := by
  rw [hmk]
  first
    | rfl
    | simp [String.slice, String.toList, Char.toString]"#
        .to_string();

    let pos_digits_fuel = format!(
        r#"have hds10 : ∀ x ∈ ds, x < 10 := fun x hx =>
  AverDigits.natDigits_digits_lt_ten m x (by rw [hnd]; exact List.mem_cons_of_mem _ hx)
have hdigits : ∀ ch ∈ (String.fromInt (Int.ofNat m)).data.drop ((1 : Int)).toNat,
    {pred} (Char.toString ch) = true := by
  intro ch hc
  rw [hsl, hnd] at hc
  simp at hc
  rcases hc with ⟨x, hx, rfl⟩
  exact {pred_lemma} x (hds10 x hx)
have hfuel : averStringPosFuel (String.fromInt (Int.ofNat m)) 1 1
    = ((String.fromInt (Int.ofNat m)).data.length - ((1 : Int)).toNat) + 1 := by
  simp [averStringPosFuel]"#
    );

    let pos_scan_close = format!(
        r#"simp only [{scan}]
rw [{scan_lemma} (averStringPosFuel (String.fromInt (Int.ofNat m)) 1 1)
      (String.fromInt (Int.ofNat m)) 1 0 (by omega) (by omega)
      (by rw [hfuel]; omega) hdigits]
exact hfin"#
    );

    // negSucc head facts.
    let neg_head = r#"have hsl : (String.fromInt (Int.negSucc m)).data = '-' :: (AverDigits.natDigits (m + 1)).map AverDigits.digitChar := rfl
rcases hnd : AverDigits.natDigits (m + 1) with _ | ⟨d, ds⟩
· exact absurd hnd (AverDigits.natDigits_nonempty (m + 1))
· have hd10 : d < 10 := AverDigits.natDigits_digits_lt_ten (m + 1) d (by rw [hnd]; exact List.mem_cons_self _ _)
  have hdne0 : d ≠ 0 := AverDigits.natDigits_head_ne_zero (m + 1) (by omega) d ds hnd
  have hlen : (String.fromInt (Int.negSucc m)).data.length = ds.length + 2 := by
    rw [hsl, hnd]; simp"#
        .to_string();

    let neg_digits_fuel = format!(
        r#"have hds10 : ∀ x ∈ ds, x < 10 := fun x hx =>
  AverDigits.natDigits_digits_lt_ten (m + 1) x (by rw [hnd]; exact List.mem_cons_of_mem _ hx)
have hdigits : ∀ ch ∈ (String.fromInt (Int.negSucc m)).data.drop ((2 : Int)).toNat,
    {pred} (Char.toString ch) = true := by
  intro ch hc
  rw [hsl, hnd] at hc
  simp at hc
  rcases hc with ⟨x, hx, rfl⟩
  exact {pred_lemma} x (hds10 x hx)
have hfuel : averStringPosFuel (String.fromInt (Int.negSucc m)) 2 1
    = ((String.fromInt (Int.negSucc m)).data.length - ((2 : Int)).toNat) + 1 := by
  simp [averStringPosFuel]"#
    );

    let neg_scan_close = format!(
        r#"simp only [{scan}]
rw [{scan_lemma} (averStringPosFuel (String.fromInt (Int.negSucc m)) 2 1)
      (String.fromInt (Int.negSucc m)) 2 0 (by omega) (by omega)
      (by rw [hfuel]; omega) hdigits]
exact hfin"#
    );

    // ---- per-plan branch bodies ------------------------------------
    let (ofnat_branch, negsucc_branch) = match plan {
        LanePlan::PosSegmentPos => {
            let tail = format!(
                r#"{pos_hmk}
{pos_headslice}
{pos_digits_fuel}
rw [hheadslice]
have harm : {posf} (String.fromInt (Int.ofNat m)) 0 (Char.toString (AverDigits.digitChar d))
    = {scan} (String.fromInt (Int.ofNat m)) 1 0 false := by
  simp [{posf}, {pred_lemma} d hd10]
rw [harm]
{pos_scan_close}"#
            );
            let branch = format!(
                "· have hm : m ≠ 0 := by\n    intro h0\n    subst h0\n    exact absurd hn (by decide)\n  {head}\n{tail}",
                head = indent_block(&pos_head, 2),
                tail = indent_block_all(&tail, 4),
            );
            (branch, negsucc_vacuous)
        }
        LanePlan::DigitDispatchNonNeg => {
            let tail = format!(
                r#"{pos_hmk}
{pos_hch}
{pos_headslice}
{pos_digits_fuel}
rw [hheadslice]
have harm0 : {fn_lean} (String.fromInt (Int.ofNat m)) 0 (Char.toString (AverDigits.digitChar d))
    = {parse} (String.fromInt (Int.ofNat m)) 0 := by
  simp [{fn_lean}, {pred_lemma} d hd10]
rw [harm0]
simp only [{parse}, hch]
split
· rename_i heq
  exact absurd heq (AverDigits.digitChar_toString_ne_minus d hd10)
· rename_i heq
  exact absurd heq (AverDigits.digitChar_toString_ne_zero d hd10 hdne0)
· have harm : {posf} (String.fromInt (Int.ofNat m)) 0 (Char.toString (AverDigits.digitChar d))
      = {scan} (String.fromInt (Int.ofNat m)) 1 0 false := by
    simp [{posf}, {pred_lemma} d hd10]
  rw [harm]
  {close}"#,
                close = indent_block(&pos_scan_close, 2),
            );
            let branch = format!(
                "· by_cases hm : m = 0\n  · {zero}\n  · {head}\n{tail}",
                zero = indent_block(&zero_case, 4),
                head = indent_block(&pos_head, 4),
                tail = indent_block_all(&tail, 6),
            );
            (branch, negsucc_vacuous)
        }
        LanePlan::ScannerNonNeg => {
            let tail = format!(
                r#"have hb : (Int.ofNat m == 0) = false := by
  first
    | (rcases m with _ | k
       · exact absurd rfl hm
       · rfl)
    | simp [hm]
{pos_digits_fuel}
rw [hb]
{pos_scan_close}"#
            );
            let branch = format!(
                "· by_cases hm : m = 0\n  · {zero}\n  · {head}\n{tail}",
                zero = indent_block(&zero_case, 4),
                head = indent_block(&pos_head, 4),
                tail = indent_block_all(&tail, 6),
            );
            (branch, negsucc_vacuous)
        }
        LanePlan::NegSegmentNeg => {
            let tail = format!(
                r#"have hch1 : String.charAt (String.fromInt (Int.negSucc m)) 1
    = some (Char.toString (AverDigits.digitChar d)) := by
  have h := String.charAt_eq_of_lt (String.fromInt (Int.negSucc m)) 1 (by omega) (by omega)
  simpa [hsl, hnd] using h
{neg_digits_fuel}
simp only [{neg}, hch1]
split
· rename_i heq
  exact absurd heq (AverDigits.digitChar_toString_ne_zero d hd10 hdne0)
· have harm : {sign} (String.fromInt (Int.negSucc m)) 1 0 (Char.toString (AverDigits.digitChar d))
      = {scan} (String.fromInt (Int.negSucc m)) 2 0 false := by
    simp [{sign}, {pred_lemma} d hd10]
  rw [harm]
  {close}"#,
                close = indent_block(&neg_scan_close, 2),
            );
            let branch = format!(
                "· {head}\n{tail}",
                head = indent_block(&neg_head, 2),
                tail = indent_block_all(&tail, 4),
            );
            (ofnat_vacuous, branch)
        }
        LanePlan::SignSegmentNeg => {
            let tail = format!(
                r#"have hmk : String.fromInt (Int.negSucc m) = String.mk ('-' :: (d :: ds).map AverDigits.digitChar) := by
  rw [← hnd]
  rfl
have hheadslice : String.slice (String.fromInt (Int.negSucc m)) 1 2
    = Char.toString (AverDigits.digitChar d) := by
  rw [hmk]
  first
    | rfl
    | simp [String.slice, String.toList, Char.toString]
{neg_digits_fuel}
rw [hheadslice]
have harm : {sign} (String.fromInt (Int.negSucc m)) 1 0 (Char.toString (AverDigits.digitChar d))
    = {scan} (String.fromInt (Int.negSucc m)) 2 0 false := by
  simp [{sign}, {pred_lemma} d hd10]
rw [harm]
{neg_scan_close}"#
            );
            let branch = format!(
                "· {head}\n{tail}",
                head = indent_block(&neg_head, 2),
                tail = indent_block_all(&tail, 4),
            );
            (ofnat_vacuous, branch)
        }
    };

    let sabotage_line = if sabotage {
        // TEST-ONLY (`AVER_PROOF_LANE_SABOTAGE`): an unknown
        // identifier makes this module's build fail hard — the lane
        // must absorb it with zero effect on budgets and neighbors.
        "\nexact averLaneSabotageInjectedByTest"
    } else {
        ""
    };

    let inner = format!("{prologue}{sabotage_line}\n{ofnat_branch}\n{negsucc_branch}");

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
    content.push_str(&format!(
        r#"private theorem {pred_lemma} : ∀ d : Nat, d < 10 → {pred} (Char.toString (AverDigits.digitChar d)) = true := by
  intro d h
  rcases d with _|_|_|_|_|_|_|_|_|_|d
  all_goals first | decide | omega
"#
    ));
    content.push('\n');
    content.push_str(&format!(
        "{}{} {}\n",
        super::LAW_CLASS_MARKER_PREFIX,
        theorem,
        super::LAW_CLASS_UNIVERSAL
    ));
    content.push_str(&format!(
        "theorem {theorem} : ∀ {quant_params}, {prop} := by\n"
    ));
    for line in inner.lines() {
        if line.is_empty() {
            content.push('\n');
        } else {
            content.push_str("  ");
            content.push_str(line);
            content.push('\n');
        }
    }

    // L2 of the iron guard: the lane grammar has no sorry carrier.
    debug_assert!(
        !content.contains("sorry"),
        "universal-lane module must not contain a sorry token"
    );

    let module = lane_module_id(&theorem_base, &content, entry_content);

    Some(LaneLawFile {
        label: format!("{}.{}", vb.fn_name, law.name),
        theorem,
        module,
        content,
    })
}

// ===================== FAMILY 2: bridge-shaped premises ============
//
// `when boolRel(a, b)` where boolRel mirrors Prop equality on a
// canonical Peano type (lifted to builtin Nat). All proof text below
// is a verbatim parameterization of scripts hand-validated kernel-
// genuine on the emitted Lean 4.15 projects of TIP prop_85 (zip-rev
// under length equality, [propext, Quot.sound]), prop_46/47,
// lemma_19/21 and prop_76. User fns are referenced as `_root_.<fn>`
// inside simp sets: emitted defs live at the root namespace, and a
// bare name there can resolve against a colliding core export
// (e.g. `insert` → `Insert.insert`), which fails with "proposition
// expected" — paid-for landmine from the hand validation.

/// How a negated bridge premise reaches the equality fn.
enum BridgeNeg {
    /// `when Bool.not(eq(a, b))` — rendered `(!eq a b) = true`.
    BoolNot,
    /// `when w(a, b)` where `w` is the 2-arm not-wrapper
    /// `match eq(a, b) { true -> false, false -> true }`.
    Wrapper(String),
}

/// The hand-validated bridge-premise figures. Source fn names only;
/// the renderer converts to Lean names. Anything outside these exact
/// constellations declines at zero cost.
enum BridgePlan {
    /// TIP prop_85: `when eq(len(xs), len(ys))`,
    /// `zip(rev(xs), rev(ys)) = revPair(zip(xs, ys))`. Needs the full
    /// probe kit: both bridges, measure lemmas over append/rev, a
    /// premise-driven shape inversion of the non-induction variable,
    /// and the snoc-distribution aux lemma.
    ZipRevLenEq {
        eq_fn: String,
        len_fn: String,
        zip_fn: String,
        rev_fn: String,
        rev_pair_fn: String,
        append_fn: String,
        append_pair_fn: String,
        /// Rendered Lean element type of the two list givens (`Int`).
        elem_ty: String,
        /// The two list givens, in quantifier order (xs = induction target).
        xs: String,
        ys: String,
    },
    /// TIP prop_46: `when eq(x, y)`, `elem(x, insert(y, z)) = true`.
    /// Bridge + subst, then list induction with the REINTRODUCTION
    /// bridge discharging the `eq x x` dispatch.
    EqElemInsert {
        eq_fn: String,
        elem_fn: String,
        or_fn: String,
        insert_fn: String,
        x: String,
        y: String,
        z: String,
    },
    /// TIP prop_47 / lemma_19: `when neq(x, y)`,
    /// `elem(x, insert(y, z)) = elem(x, z)`.
    NeqElemInsert {
        eq_fn: String,
        neg: BridgeNeg,
        elem_fn: String,
        or_fn: String,
        insert_fn: String,
        x: String,
        y: String,
        z: String,
    },
    /// TIP lemma_21: `when neq(x, y)`,
    /// `count(x, insert(y, z)) = count(x, z)`.
    NeqCountInsert {
        eq_fn: String,
        neg: BridgeNeg,
        count_fn: String,
        insert_fn: String,
        x: String,
        y: String,
        z: String,
    },
    /// TIP prop_76: `when Bool.not(eq(n, m))`,
    /// `count(n, List.concat(xs, [m])) = count(n, xs)`.
    NeqCountAppendSingleton {
        eq_fn: String,
        neg: BridgeNeg,
        count_fn: String,
        n: String,
        m: String,
        xs: String,
    },
}

/// Internal binder names of the insert/count figure templates — a
/// colliding given would be shadowed mid-proof, so such laws decline.
const BRIDGE_RESERVED_INSERT: &[&str] = &["c", "cs", "ih", "hc", "heq", "h_when"];

/// Internal binder names of the zip-rev MAIN theorem template (the
/// support lemmas are closed terms — their binders cannot collide).
const BRIDGE_RESERVED_ZIPREV: &[&str] = &[
    "z", "x2", "y", "x4", "h", "hh", "h0", "hy", "hlen", "hrevlen", "hih", "h_when",
];

fn as_bool_lit(e: &Spanned<Expr>) -> Option<bool> {
    match &e.node {
        Expr::Literal(Literal::Bool(b)) => Some(*b),
        _ => None,
    }
}

/// `List<elem>` annotation check, whitespace-insensitive.
fn is_list_of(list_ann: &str, elem_ann: &str) -> bool {
    let squash = |s: &str| s.chars().filter(|c| !c.is_whitespace()).collect::<String>();
    squash(list_ann) == format!("List<{}>", squash(elem_ann))
}

/// Split `match <ident> { Base -> e1, Succ(b) -> e2 }` over `peano`
/// into (subject ident, base body, succ binder, succ body).
fn peano_match_split<'a>(
    e: &'a Spanned<Expr>,
    peano: &crate::codegen::proof_recognize::PeanoType,
) -> Option<(&'a str, &'a Spanned<Expr>, &'a str, &'a Spanned<Expr>)> {
    let Expr::Match { subject, arms } = &e.node else {
        return None;
    };
    let subj = ident_of(subject)?;
    if arms.len() != 2 {
        return None;
    }
    let mut base = None;
    let mut succ = None;
    for arm in arms {
        let Pattern::Constructor(name, binders) = &arm.pattern else {
            return None;
        };
        let short = crate::codegen::proof_recognize::short_ctor(name);
        if short == peano.base_ctor && binders.is_empty() {
            base = Some(&arm.body);
        } else if short == peano.succ_ctor && binders.len() == 1 {
            succ = Some((binders[0].as_str(), &arm.body));
        } else {
            return None;
        }
    }
    let (q, sb) = succ?;
    Some((subj, base?, q, sb))
}

/// (subject ident, nil body, head binder, tail binder, cons body) —
/// the result of [`list_match_split`].
type ListMatchSplit<'a> = (
    &'a str,
    &'a Spanned<Expr>,
    &'a str,
    &'a str,
    &'a Spanned<Expr>,
);

/// Split `match <ident> { [] -> e1, [h, ..t] -> e2 }` into
/// (subject ident, nil body, head binder, tail binder, cons body).
fn list_match_split(e: &Spanned<Expr>) -> Option<ListMatchSplit<'_>> {
    let Expr::Match { subject, arms } = &e.node else {
        return None;
    };
    let subj = ident_of(subject)?;
    if arms.len() != 2 {
        return None;
    }
    let mut nil = None;
    let mut cons = None;
    for arm in arms {
        match &arm.pattern {
            Pattern::EmptyList => nil = Some(&arm.body),
            Pattern::Cons(h, t) => cons = Some((h.as_str(), t.as_str(), &arm.body)),
            _ => return None,
        }
    }
    let (h, t, cb) = cons?;
    Some((subj, nil?, h, t, cb))
}

/// The base (`Nat.Z`) constructor of `peano` as a payload-free
/// expression — covers both `Constructor` and `Attr` parses.
fn is_peano_base(e: &Spanned<Expr>, peano: &crate::codegen::proof_recognize::PeanoType) -> bool {
    if let Some((name, args)) = ctor_of(e) {
        return crate::codegen::proof_recognize::short_ctor(&name) == peano.base_ctor
            && args.is_empty();
    }
    if let Expr::Attr(base, leaf) = &e.node {
        return ident_of(base) == Some(peano.type_name.as_str()) && *leaf == peano.base_ctor;
    }
    false
}

/// `Succ(inner)` of `peano` — returns the payload expression.
fn peano_succ_of<'a>(
    e: &'a Spanned<Expr>,
    peano: &crate::codegen::proof_recognize::PeanoType,
) -> Option<&'a Spanned<Expr>> {
    let (name, args) = ctor_of(e)?;
    (crate::codegen::proof_recognize::short_ctor(&name) == peano.succ_ctor && args.len() == 1)
        .then(|| args[0])
}

/// `[<ident>]` — a singleton list literal of exactly one identifier.
fn is_singleton_list_of_ident(e: &Spanned<Expr>, name: &str) -> bool {
    matches!(&e.node, Expr::List(items)
        if items.len() == 1 && ident_of(&items[0]) == Some(name))
}

/// Canonical Peano structural equality (`natEq`-shape): a pure binary
/// Bool fn on a canonical Peano type whose body mirrors `=` exactly —
/// `match a { Z -> match b { Z -> true, S _ -> false },
///            S x -> match b { Z -> false, S y -> rec(x, y) } }`.
/// The Peano type must be spelled `Nat`: the lift renders constructors
/// as `0` / `+ 1` while signatures keep the source type name, so only
/// the builtin spelling elaborates — and only that emission shape was
/// hand-validated.
fn is_peano_eq_fn(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" || !fd.effects.is_empty() {
        return false;
    }
    let (p0, t0) = &fd.params[0];
    let (p1, t1) = &fd.params[1];
    if t0 != t1 {
        return false;
    }
    let Some(peano) = crate::codegen::proof_recognize::peano_type_named(ctx, t0.trim()) else {
        return false;
    };
    if peano.type_name.trim() != "Nat" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, base_body, q, succ_body)) = peano_match_split(body, &peano) else {
        return false;
    };
    if subj != p0.as_str() {
        return false;
    }
    // Z arm: match b { Z -> true, S _ -> false }.
    let Some((s2, b2, _, sb2)) = peano_match_split(base_body, &peano) else {
        return false;
    };
    if s2 != p1.as_str() || as_bool_lit(b2) != Some(true) || as_bool_lit(sb2) != Some(false) {
        return false;
    }
    // S(q) arm: match b { Z -> false, S(r) -> rec(q, r) }.
    let Some((s3, b3, r, sb3)) = peano_match_split(succ_body, &peano) else {
        return false;
    };
    if s3 != p1.as_str() || as_bool_lit(b3) != Some(false) {
        return false;
    }
    call_of(sb3).is_some_and(|(rc, ra)| {
        rc == fd.name && ra.len() == 2 && ident_of(&ra[0]) == Some(q) && ident_of(&ra[1]) == Some(r)
    })
}

/// 2-arm not-wrapper over a recognized Peano equality:
/// `match eq(p0, p1) { true -> false, false -> true }`. Returns the
/// wrapped equality fn's name.
fn neg_eq_wrapper(fd: &FnDef, ctx: &CodegenContext) -> Option<String> {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" || !fd.effects.is_empty() {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    let (eq_name, eq_args) = call_of(subject)?;
    let eq_fd = ctx.fn_def_by_name(&eq_name, None)?;
    if !is_peano_eq_fn(eq_fd, ctx)
        || eq_args.len() != 2
        || ident_of(&eq_args[0]) != Some(fd.params[0].0.as_str())
        || ident_of(&eq_args[1]) != Some(fd.params[1].0.as_str())
        || arms.len() != 2
    {
        return None;
    }
    let mut t_to_f = false;
    let mut f_to_t = false;
    for arm in arms {
        match (&arm.pattern, as_bool_lit(&arm.body)) {
            (Pattern::Literal(Literal::Bool(true)), Some(false)) => t_to_f = true,
            (Pattern::Literal(Literal::Bool(false)), Some(true)) => f_to_t = true,
            _ => return None,
        }
    }
    (t_to_f && f_to_t).then_some(eq_name)
}

/// 2-arm Bool-or wrapper (`barbar`-shape):
/// `match p0 { true -> true, false -> p1 }`.
fn is_bool_or_wrapper(fd: &FnDef) -> bool {
    if fd.params.len() != 2
        || fd.return_type.trim() != "Bool"
        || !fd.effects.is_empty()
        || fd.params[0].1.trim() != "Bool"
        || fd.params[1].1.trim() != "Bool"
    {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return false;
    };
    if ident_of(subject) != Some(fd.params[0].0.as_str()) || arms.len() != 2 {
        return false;
    }
    let mut ok_t = false;
    let mut ok_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => ok_t = as_bool_lit(&arm.body) == Some(true),
            Pattern::Literal(Literal::Bool(false)) => {
                ok_f = ident_of(&arm.body) == Some(fd.params[1].0.as_str())
            }
            _ => return false,
        }
    }
    ok_t && ok_f
}

/// `elem`-shape: `fn (k: Nat, l: List<Nat>) -> Bool` with body
/// `match l { [] -> false, [z, ..xs] -> or(eq(k, z), rec(k, xs)) }`.
/// Returns the or-wrapper fn's name.
fn elem_shape(fd: &FnDef, ctx: &CodegenContext, eq_fn: &str) -> Option<String> {
    if fd.params.len() != 2 || fd.return_type.trim() != "Bool" || !fd.effects.is_empty() {
        return None;
    }
    let k = fd.params[0].0.as_str();
    if !is_list_of(&fd.params[1].1, &fd.params[0].1) {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let (subj, nil_body, hd, tl, cons_body) = list_match_split(body)?;
    if subj != fd.params[1].0.as_str() || as_bool_lit(nil_body) != Some(false) {
        return None;
    }
    let (or_name, or_args) = call_of(cons_body)?;
    if or_args.len() != 2 {
        return None;
    }
    let (c1, a1) = call_of(&or_args[0])?;
    if c1 != eq_fn || a1.len() != 2 || ident_of(&a1[0]) != Some(k) || ident_of(&a1[1]) != Some(hd) {
        return None;
    }
    let (c2, a2) = call_of(&or_args[1])?;
    if c2 != fd.name || a2.len() != 2 || ident_of(&a2[0]) != Some(k) || ident_of(&a2[1]) != Some(tl)
    {
        return None;
    }
    let or_fd = ctx.fn_def_by_name(&or_name, None)?;
    is_bool_or_wrapper(or_fd).then_some(or_name)
}

/// `count`-shape: `fn (k: Nat, l: List<Nat>) -> Nat` with body
/// `match l { [] -> Z, [z, ..ys] -> match eq(k, z)
///   { true -> S(rec(k, ys)), false -> rec(k, ys) } }`.
fn count_shape(fd: &FnDef, ctx: &CodegenContext, eq_fn: &str) -> bool {
    if fd.params.len() != 2 || !fd.effects.is_empty() {
        return false;
    }
    let (k, kt) = (&fd.params[0].0, &fd.params[0].1);
    let Some(peano) = crate::codegen::proof_recognize::peano_type_named(ctx, kt.trim()) else {
        return false;
    };
    if fd.return_type.trim() != peano.type_name || !is_list_of(&fd.params[1].1, kt) {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    if subj != fd.params[1].0.as_str() || !is_peano_base(nil_body, &peano) {
        return false;
    }
    let Expr::Match { subject, arms } = &cons_body.node else {
        return false;
    };
    let dispatch_ok = call_of(subject).is_some_and(|(c, a)| {
        c == eq_fn && a.len() == 2 && ident_of(&a[0]) == Some(k) && ident_of(&a[1]) == Some(hd)
    });
    if !dispatch_ok || arms.len() != 2 {
        return false;
    }
    let rec_ok = |e: &Spanned<Expr>| {
        call_of(e).is_some_and(|(rc, ra)| {
            rc == fd.name
                && ra.len() == 2
                && ident_of(&ra[0]) == Some(k.as_str())
                && ident_of(&ra[1]) == Some(tl)
        })
    };
    let mut ok_t = false;
    let mut ok_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                ok_t = peano_succ_of(&arm.body, &peano).is_some_and(rec_ok)
            }
            Pattern::Literal(Literal::Bool(false)) => ok_f = rec_ok(&arm.body),
            _ => return false,
        }
    }
    ok_t && ok_f
}

/// `insert`-shape: `fn (k: Nat, l: List<Nat>) -> List<Nat>` with body
/// `match l { [] -> [k], [z, ..xs] -> match le(k, z)
///   { true -> List.concat([k], l), false -> List.concat([z], rec(k, xs)) } }`
/// for SOME pure binary Bool dispatch `le` — the templates split on
/// the dispatch without consuming its meaning, so its shape is free.
fn insert_shape(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 2 || !fd.effects.is_empty() {
        return false;
    }
    let (k, kt) = (&fd.params[0].0, &fd.params[0].1);
    if crate::codegen::proof_recognize::peano_type_named(ctx, kt.trim()).is_none()
        || !is_list_of(&fd.params[1].1, kt)
        || !is_list_of(&fd.return_type, kt)
    {
        return false;
    }
    let l = fd.params[1].0.as_str();
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    if subj != l || !is_singleton_list_of_ident(nil_body, k) {
        return false;
    }
    let Expr::Match { subject, arms } = &cons_body.node else {
        return false;
    };
    let dispatch_ok = call_of(subject).is_some_and(|(c, a)| {
        a.len() == 2
            && ident_of(&a[0]) == Some(k.as_str())
            && ident_of(&a[1]) == Some(hd)
            && ctx.fn_def_by_name(&c, None).is_some_and(|le| {
                le.params.len() == 2 && le.return_type.trim() == "Bool" && le.effects.is_empty()
            })
    });
    if !dispatch_ok || arms.len() != 2 {
        return false;
    }
    fn concat_of(e: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
        let (c, a) = call_of(e)?;
        (c == "List.concat" && a.len() == 2).then(|| (&a[0], &a[1]))
    }
    let mut ok_t = false;
    let mut ok_f = false;
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Bool(true)) => {
                ok_t = concat_of(&arm.body).is_some_and(|(h, t)| {
                    is_singleton_list_of_ident(h, k) && ident_of(t) == Some(l)
                })
            }
            Pattern::Literal(Literal::Bool(false)) => {
                ok_f = concat_of(&arm.body).is_some_and(|(h, t)| {
                    is_singleton_list_of_ident(h, hd)
                        && call_of(t).is_some_and(|(rc, ra)| {
                            rc == fd.name
                                && ra.len() == 2
                                && ident_of(&ra[0]) == Some(k.as_str())
                                && ident_of(&ra[1]) == Some(tl)
                        })
                })
            }
            _ => return false,
        }
    }
    ok_t && ok_f
}

/// `len`-shape measure: `fn (xs: List<T>) -> Nat` with body
/// `match xs { [] -> Z, [_, ..ys] -> S(rec(ys)) }`.
fn len_shape(fd: &FnDef, ctx: &CodegenContext) -> bool {
    if fd.params.len() != 1 || !fd.effects.is_empty() {
        return false;
    }
    let Some(peano) = crate::codegen::proof_recognize::peano_type_named(ctx, fd.return_type.trim())
    else {
        return false;
    };
    if peano.type_name.trim() != "Nat" {
        return false;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, _, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    subj == fd.params[0].0.as_str()
        && is_peano_base(nil_body, &peano)
        && peano_succ_of(cons_body, &peano).is_some_and(|inner| {
            call_of(inner).is_some_and(|(rc, ra)| {
                rc == fd.name && ra.len() == 1 && ident_of(&ra[0]) == Some(tl)
            })
        })
}

/// `append`-shape: `fn (xs: List<T>, ys: List<T>) -> List<T>` with body
/// `match xs { [] -> ys, [z, ..zs] -> List.concat([z], rec(zs, ys)) }`.
fn append_shape(fd: &FnDef, elem_ann: &str) -> bool {
    if fd.params.len() != 2
        || !fd.effects.is_empty()
        || !is_list_of(&fd.params[0].1, elem_ann)
        || !is_list_of(&fd.params[1].1, elem_ann)
        || !is_list_of(&fd.return_type, elem_ann)
    {
        return false;
    }
    let ys = fd.params[1].0.as_str();
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    subj == fd.params[0].0.as_str()
        && ident_of(nil_body) == Some(ys)
        && call_of(cons_body).is_some_and(|(c, a)| {
            c == "List.concat"
                && a.len() == 2
                && is_singleton_list_of_ident(&a[0], hd)
                && call_of(&a[1]).is_some_and(|(rc, ra)| {
                    rc == fd.name
                        && ra.len() == 2
                        && ident_of(&ra[0]) == Some(tl)
                        && ident_of(&ra[1]) == Some(ys)
                })
        })
}

/// `rev`-shape: `fn (xs: List<T>) -> List<T>` with body
/// `match xs { [] -> [], [y, ..ys] -> app(rec(ys), [y]) }` where `app`
/// is an [`append_shape`] fn over the same element type. Returns the
/// append fn's name.
fn rev_shape(fd: &FnDef, ctx: &CodegenContext, elem_ann: &str) -> Option<String> {
    if fd.params.len() != 1
        || !fd.effects.is_empty()
        || !is_list_of(&fd.params[0].1, elem_ann)
        || !is_list_of(&fd.return_type, elem_ann)
    {
        return None;
    }
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let (subj, nil_body, hd, tl, cons_body) = list_match_split(body)?;
    if subj != fd.params[0].0.as_str()
        || !matches!(&nil_body.node, Expr::List(items) if items.is_empty())
    {
        return None;
    }
    let (app, args) = call_of(cons_body)?;
    if args.len() != 2
        || !call_of(&args[0])
            .is_some_and(|(rc, ra)| rc == fd.name && ra.len() == 1 && ident_of(&ra[0]) == Some(tl))
        || !is_singleton_list_of_ident(&args[1], hd)
    {
        return None;
    }
    let app_fd = ctx.fn_def_by_name(&app, None)?;
    append_shape(app_fd, elem_ann).then_some(app)
}

/// `zip`-shape: `fn (xs: List<A>, ys: List<B>) -> List<Tuple<A, B>>`
/// with body `match xs { [] -> [], [z, ..x2] -> match ys
///   { [] -> [], [x3, ..x4] -> List.concat([(z, x3)], rec(x2, x4)) } }`.
fn zip_shape(fd: &FnDef, elem_ann: &str) -> bool {
    let squash = |s: &str| s.chars().filter(|c| !c.is_whitespace()).collect::<String>();
    let pair_ann = format!("Tuple<{},{}>", squash(elem_ann), squash(elem_ann));
    if fd.params.len() != 2
        || !fd.effects.is_empty()
        || !is_list_of(&fd.params[0].1, elem_ann)
        || !is_list_of(&fd.params[1].1, elem_ann)
        || !is_list_of(&fd.return_type, &pair_ann)
    {
        return false;
    }
    let is_empty_list =
        |e: &Spanned<Expr>| matches!(&e.node, Expr::List(items) if items.is_empty());
    let [Stmt::Expr(body)] = fd.body.stmts() else {
        return false;
    };
    let Some((subj, nil_body, hd, tl, cons_body)) = list_match_split(body) else {
        return false;
    };
    if subj != fd.params[0].0.as_str() || !is_empty_list(nil_body) {
        return false;
    }
    let Some((subj2, nil2, hd2, tl2, cons2)) = list_match_split(cons_body) else {
        return false;
    };
    if subj2 != fd.params[1].0.as_str() || !is_empty_list(nil2) {
        return false;
    }
    call_of(cons2).is_some_and(|(c, a)| {
        c == "List.concat"
            && a.len() == 2
            && matches!(&a[0].node, Expr::List(items)
                if items.len() == 1
                    && matches!(&items[0].node, Expr::Tuple(pair)
                        if pair.len() == 2
                            && ident_of(&pair[0]) == Some(hd)
                            && ident_of(&pair[1]) == Some(hd2)))
            && call_of(&a[1]).is_some_and(|(rc, ra)| {
                rc == fd.name
                    && ra.len() == 2
                    && ident_of(&ra[0]) == Some(tl)
                    && ident_of(&ra[1]) == Some(tl2)
            })
    })
}

/// Given-named identifiers occurring in `e` — the variable sets of the
/// ACL2 free-variables gate. Returns `None` on any expression node the
/// walker does not positively recognize: the gate must never
/// under-collect on the `when` side, so unknown structure declines.
fn given_vars_in(
    e: &Spanned<Expr>,
    givens: &std::collections::BTreeSet<&str>,
    out: &mut std::collections::BTreeSet<String>,
) -> Option<()> {
    match &e.node {
        Expr::Literal(_) => Some(()),
        Expr::Ident(n) | Expr::Resolved { name: n, .. } => {
            if givens.contains(n.as_str()) {
                out.insert(n.clone());
            }
            Some(())
        }
        Expr::Attr(base, _) => given_vars_in(base, givens, out),
        Expr::FnCall(callee, args) => {
            given_vars_in(callee, givens, out)?;
            args.iter().try_for_each(|a| given_vars_in(a, givens, out))
        }
        Expr::TailCall(data) => data
            .args
            .iter()
            .try_for_each(|a| given_vars_in(a, givens, out)),
        Expr::BinOp(_, l, r) => {
            given_vars_in(l, givens, out)?;
            given_vars_in(r, givens, out)
        }
        Expr::Neg(inner) | Expr::ErrorProp(inner) => given_vars_in(inner, givens, out),
        Expr::Constructor(_, payload) => payload
            .as_deref()
            .map_or(Some(()), |p| given_vars_in(p, givens, out)),
        Expr::List(items) | Expr::Tuple(items) => {
            items.iter().try_for_each(|a| given_vars_in(a, givens, out))
        }
        _ => None,
    }
}

/// `(eq_fn, negation route, lhs arg, rhs arg)` — the result of
/// [`bridge_premise`].
type BridgePremise<'a> = (
    String,
    Option<BridgeNeg>,
    &'a Spanned<Expr>,
    &'a Spanned<Expr>,
);

/// Normalize a bridge-shaped `when` to
/// `(eq_fn, negation route, lhs arg, rhs arg)`.
fn bridge_premise<'a>(when: &'a Spanned<Expr>, ctx: &CodegenContext) -> Option<BridgePremise<'a>> {
    let (callee, args) = call_of(when)?;
    if callee == "Bool.not" && args.len() == 1 {
        let (inner, in_args) = call_of(&args[0])?;
        let fd = ctx.fn_def_by_name(&inner, None)?;
        return (in_args.len() == 2 && is_peano_eq_fn(fd, ctx))
            .then(|| (inner, Some(BridgeNeg::BoolNot), &in_args[0], &in_args[1]));
    }
    if args.len() != 2 {
        return None;
    }
    let fd = ctx.fn_def_by_name(&callee, None)?;
    if is_peano_eq_fn(fd, ctx) {
        return Some((callee, None, &args[0], &args[1]));
    }
    let eq_fn = neg_eq_wrapper(fd, ctx)?;
    Some((eq_fn, Some(BridgeNeg::Wrapper(callee)), &args[0], &args[1]))
}

/// Validate one when-law against the bridge-premise figures. Mirrors
/// the lane discipline: exact hand-validated constellations only,
/// everything else declines (the law stays bounded, manifest bytes
/// untouched).
fn classify_bridge_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
) -> Option<BridgePlan> {
    let when = law.when.as_ref()?;
    let given_names: std::collections::BTreeSet<&str> =
        law.givens.iter().map(|g| g.name.as_str()).collect();
    if given_names.len() != law.givens.len() {
        return None;
    }

    // ACL2 free-variables gate: vars(when) ⊆ vars(lhs). A premise
    // variable unbound by the conclusion's match side would make
    // conditional rewriting guess — decline instead.
    let mut when_vars = std::collections::BTreeSet::new();
    given_vars_in(when, &given_names, &mut when_vars)?;
    let mut lhs_vars = std::collections::BTreeSet::new();
    given_vars_in(&law.lhs, &given_names, &mut lhs_vars)?;
    if !when_vars.is_subset(&lhs_vars) {
        return None;
    }

    let (eq_fn, neg, a, b) = bridge_premise(when, ctx)?;
    let given_type = |name: &str| -> Option<&str> {
        law.givens
            .iter()
            .find(|g| g.name == name)
            .map(|g| g.type_name.as_str())
    };
    let (lhs_callee, lhs_args) = call_of(&law.lhs)?;
    if lhs_callee.rsplit('.').next()? != vb.fn_name {
        return None;
    }

    // ---- ZipRevLenEq: when eq(len(xs), len(ys)) ---------------------
    if let (Some((len_a, la)), Some((len_b, lb))) = (call_of(a), call_of(b)) {
        if law.givens.len() != 2
            || len_a != len_b
            || la.len() != 1
            || lb.len() != 1
            || law
                .givens
                .iter()
                .any(|g| BRIDGE_RESERVED_ZIPREV.contains(&g.name.as_str()))
        {
            return None;
        }
        if neg.is_some() {
            return None;
        }
        let xs = law.givens[0].name.as_str();
        let ys = law.givens[1].name.as_str();
        if ident_of(&la[0]) != Some(xs) || ident_of(&lb[0]) != Some(ys) {
            return None;
        }
        let xs_ty = law.givens[0].type_name.as_str();
        if law.givens[1].type_name != xs_ty {
            return None;
        }
        // Element type from `List<T>`.
        let elem_ann = {
            let squash: String = xs_ty.chars().filter(|c| !c.is_whitespace()).collect();
            squash.strip_prefix("List<")?.strip_suffix('>')?.to_string()
        };
        let len_fd = ctx.fn_def_by_name(&len_a, None)?;
        if !len_shape(len_fd, ctx) || !is_list_of(xs_ty, &elem_ann) {
            return None;
        }
        // lhs: zip(rev(xs), rev(ys)); rhs: revPair(zip(xs, ys)).
        if lhs_args.len() != 2 {
            return None;
        }
        let (rev_a, ra) = call_of(&lhs_args[0])?;
        let (rev_b, rb) = call_of(&lhs_args[1])?;
        if rev_a != rev_b
            || ra.len() != 1
            || rb.len() != 1
            || ident_of(&ra[0]) != Some(xs)
            || ident_of(&rb[0]) != Some(ys)
        {
            return None;
        }
        let (rev_pair, rp_args) = call_of(&law.rhs)?;
        if rp_args.len() != 1 {
            return None;
        }
        let (zip_inner, zi) = call_of(&rp_args[0])?;
        if zip_inner != lhs_callee
            || zi.len() != 2
            || ident_of(&zi[0]) != Some(xs)
            || ident_of(&zi[1]) != Some(ys)
        {
            return None;
        }
        let zip_fd = ctx.fn_def_by_name(&lhs_callee, None)?;
        if !zip_shape(zip_fd, &elem_ann) {
            return None;
        }
        let rev_fd = ctx.fn_def_by_name(&rev_a, None)?;
        let append_fn = rev_shape(rev_fd, ctx, &elem_ann)?;
        let pair_ann = format!("Tuple<{elem_ann}, {elem_ann}>");
        let rev_pair_fd = ctx.fn_def_by_name(&rev_pair, None)?;
        let append_pair_fn = rev_shape(rev_pair_fd, ctx, &pair_ann)?;
        // Compound element types must parenthesize inside `List _` /
        // binder positions of the rendered templates.
        let elem_ty = {
            let t = super::types::type_annotation_to_lean(&elem_ann);
            if t.contains(' ') { format!("({t})") } else { t }
        };
        return Some(BridgePlan::ZipRevLenEq {
            eq_fn,
            len_fn: len_a,
            zip_fn: lhs_callee,
            rev_fn: rev_a,
            rev_pair_fn: rev_pair,
            append_fn,
            append_pair_fn,
            elem_ty,
            xs: xs.to_string(),
            ys: ys.to_string(),
        });
    }

    // ---- insert/count figures: premise args are plain Peano givens --
    let x = ident_of(a)?;
    let y = ident_of(b)?;
    if x == y {
        // `eq(x, x)` / `neq(x, x)`: the positive figure's `subst`
        // cannot eliminate a self-equality — decline (zero cost)
        // instead of a noisy tolerated build failure.
        return None;
    }
    let xt = given_type(x)?;
    if given_type(y)? != xt
        || crate::codegen::proof_recognize::peano_type_named(ctx, xt.trim()).is_none()
        || law.givens.len() != 3
        || law
            .givens
            .iter()
            .any(|g| BRIDGE_RESERVED_INSERT.contains(&g.name.as_str()))
    {
        return None;
    }
    if lhs_args.len() != 2 || ident_of(&lhs_args[0]) != Some(x) {
        return None;
    }

    // count(x, List.concat(z_list, [y])) = count(x, z_list) — prop_76.
    if let Some((cc, ca)) = call_of(&lhs_args[1])
        && cc == "List.concat"
    {
        // Only the negated premise is a validated figure over the
        // concat-singleton conclusion shape.
        let neg = neg?;
        if ca.len() != 2 || !is_singleton_list_of_ident(&ca[1], y) {
            return None;
        }
        let zs = ident_of(&ca[0])?;
        if !is_list_of(given_type(zs)?, xt) {
            return None;
        }
        let count_fd = ctx.fn_def_by_name(&lhs_callee, None)?;
        if !count_shape(count_fd, ctx, &eq_fn) {
            return None;
        }
        let (rc, ra) = call_of(&law.rhs)?;
        return (rc == lhs_callee
            && ra.len() == 2
            && ident_of(&ra[0]) == Some(x)
            && ident_of(&ra[1]) == Some(zs))
        .then(|| BridgePlan::NeqCountAppendSingleton {
            eq_fn,
            neg,
            count_fn: lhs_callee,
            n: x.to_string(),
            m: y.to_string(),
            xs: zs.to_string(),
        });
    }

    // elem/count over insert: lhs = f(x, insert(y, z)).
    let (insert_fn, ins_args) = call_of(&lhs_args[1])?;
    if ins_args.len() != 2 || ident_of(&ins_args[0]) != Some(y) {
        return None;
    }
    let z = ident_of(&ins_args[1])?;
    if !is_list_of(given_type(z)?, xt) {
        return None;
    }
    let insert_fd = ctx.fn_def_by_name(&insert_fn, None)?;
    if !insert_shape(insert_fd, ctx) {
        return None;
    }
    let head_fd = ctx.fn_def_by_name(&lhs_callee, None)?;
    match neg {
        // when eq(x, y): elem(x, insert(y, z)) = true — prop_46.
        None => {
            let or_fn = elem_shape(head_fd, ctx, &eq_fn)?;
            (as_bool_lit(&law.rhs) == Some(true)).then(|| BridgePlan::EqElemInsert {
                eq_fn,
                elem_fn: lhs_callee,
                or_fn,
                insert_fn,
                x: x.to_string(),
                y: y.to_string(),
                z: z.to_string(),
            })
        }
        // when neq(x, y): f(x, insert(y, z)) = f(x, z) — prop_47 /
        // lemma_19 (elem) and lemma_21 (count).
        Some(neg) => {
            let (rc, ra) = call_of(&law.rhs)?;
            if rc != lhs_callee
                || ra.len() != 2
                || ident_of(&ra[0]) != Some(x)
                || ident_of(&ra[1]) != Some(z)
            {
                return None;
            }
            if let Some(or_fn) = elem_shape(head_fd, ctx, &eq_fn) {
                return Some(BridgePlan::NeqElemInsert {
                    eq_fn,
                    neg,
                    elem_fn: lhs_callee,
                    or_fn,
                    insert_fn,
                    x: x.to_string(),
                    y: y.to_string(),
                    z: z.to_string(),
                });
            }
            count_shape(head_fd, ctx, &eq_fn).then(|| BridgePlan::NeqCountInsert {
                eq_fn,
                neg,
                count_fn: lhs_callee,
                insert_fn,
                x: x.to_string(),
                y: y.to_string(),
                z: z.to_string(),
            })
        }
    }
}

/// Render one bridge-premise lane law: the validated proof template
/// for `plan`, support lemmas included, into a single hashed module.
/// Statement built by the SAME `law_theorem_prop` as the manifest
/// theorem with `omit_domain` — when-premise kept, sampled-domain
/// disjunctions dropped. Zero `sorry` tokens by construction.
fn render_bridge_law(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    ctx: &CodegenContext,
    plan: &BridgePlan,
    entry_root: &str,
    entry_content: &str,
    sabotage: bool,
) -> Option<LaneLawFile> {
    let emit = |e: &Spanned<Expr>| emit_expr_legacy(e, ctx, None).replace('\n', " ");
    let fn_lean = aver_name_to_lean(&vb.fn_name);
    let law_lean = aver_name_to_lean(&law.name);
    let theorem_base = format!("{fn_lean}_law_{law_lean}");
    let theorem = format!("{theorem_base}_universal");

    let lhs_template = emit(&law.lhs);
    let rhs_template = emit(&law.rhs);
    let when_template = emit(law.when.as_ref()?);
    let lifted = std::collections::HashMap::new();
    let (prop, bounded) = super::toplevel::law_theorem_prop(
        law,
        ctx,
        &lhs_template,
        &rhs_template,
        Some(&when_template),
        &lifted,
        true,
    );
    debug_assert!(!bounded);
    let quant_params = law
        .givens
        .iter()
        .map(|g| {
            format!(
                "({} : {})",
                aver_name_to_lean(&g.name),
                super::types::type_annotation_to_lean(&g.type_name)
            )
        })
        .collect::<Vec<_>>()
        .join(" ");
    let intro_givens = law
        .givens
        .iter()
        .map(|g| aver_name_to_lean(&g.name))
        .collect::<Vec<_>>()
        .join(" ");

    let sab = if sabotage {
        // TEST-ONLY (`AVER_PROOF_LANE_SABOTAGE`): an unknown
        // identifier makes this module's build fail hard — the lane
        // must absorb it with zero effect on budgets and neighbors.
        "\nexact averLaneSabotageInjectedByTest"
    } else {
        ""
    };

    // Use-side Bool→Prop inversion bridge: follows the predicate's own
    // recursion (mismatch arms close from the contradicted equations,
    // the step arm feeds the IH through `simpa`).
    let bridge_eq_lemma = |eq: &str| {
        format!(
            r#"private theorem {theorem}_bridge_eq : ∀ (a b : Nat), _root_.{eq} a b = true → a = b := by
  intro a
  induction a with
  | zero =>
    intro b h
    cases b with
    | zero => rfl
    | succ y => simp [_root_.{eq}] at h
  | succ x ih =>
    intro b h
    cases b with
    | zero => simp [_root_.{eq}] at h
    | succ y =>
      have hx := ih y (by simpa [_root_.{eq}] using h)
      omega
"#
        )
    };
    // REINTRODUCTION bridge (`eq a a = true`) — without it the main
    // proof cannot instantiate its own induction hypothesis.
    let bridge_refl_lemma = |eq: &str| {
        format!(
            r#"private theorem {theorem}_bridge_refl : ∀ (a : Nat), _root_.{eq} a a = true := by
  intro a
  induction a with
  | zero => rfl
  | succ x ih => simpa [_root_.{eq}] using ih
"#
        )
    };
    // Negated-premise normalization: derive `heq : eq x y = false`
    // from `h_when` by cases on the Bool — the `true` case refutes
    // the rendered premise (`!eq` or the not-wrapper unfolding).
    let neg_norm = |eq: &str, neg: &BridgeNeg, x: &str, y: &str| {
        let on_true = match neg {
            BridgeNeg::BoolNot => "rw [hc] at h_when\n    simp at h_when".to_string(),
            BridgeNeg::Wrapper(w) => {
                format!("simp [_root_.{}, hc] at h_when", aver_name_to_lean(w))
            }
        };
        format!(
            "have heq : _root_.{eq} {x} {y} = false := by\n  cases hc : _root_.{eq} {x} {y}\n  · rfl\n  · {on_true}"
        )
    };

    let (supports, body): (Vec<String>, String) = match plan {
        BridgePlan::ZipRevLenEq {
            eq_fn,
            len_fn,
            zip_fn,
            rev_fn,
            rev_pair_fn,
            append_fn,
            append_pair_fn,
            elem_ty,
            xs,
            ys,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let len = aver_name_to_lean(len_fn);
            let zip = aver_name_to_lean(zip_fn);
            let rev = aver_name_to_lean(rev_fn);
            let revp = aver_name_to_lean(rev_pair_fn);
            let app = aver_name_to_lean(append_fn);
            let appp = aver_name_to_lean(append_pair_fn);
            let a_ty = elem_ty;
            let xs = aver_name_to_lean(xs);
            let ys = aver_name_to_lean(ys);
            let supports = vec![
                bridge_eq_lemma(&eq),
                bridge_refl_lemma(&eq),
                // Measure homomorphism over append — the premise-stepping
                // arithmetic below rides on it.
                format!(
                    r#"private theorem {theorem}_len_append : ∀ (xs ys : List {a_ty}),
    _root_.{len} (_root_.{app} xs ys) = _root_.{len} xs + _root_.{len} ys := by
  intro xs
  induction xs with
  | nil => intro ys; simp [_root_.{app}, _root_.{len}]
  | cons z zs ih =>
    intro ys
    simp only [_root_.{app}, List.singleton_append, _root_.{len}, ih]
    omega
"#
                ),
                format!(
                    r#"private theorem {theorem}_len_rev : ∀ (xs : List {a_ty}), _root_.{len} (_root_.{rev} xs) = _root_.{len} xs := by
  intro xs
  induction xs with
  | nil => simp [_root_.{rev}]
  | cons y ys ih =>
    simp only [_root_.{rev}, {theorem}_len_append, _root_.{len}, ih]
"#
                ),
                // Premise-driven shape inversion of the non-induction
                // variable: `len ys = 0 → ys = []`.
                format!(
                    r#"private theorem {theorem}_len_zero : ∀ (ys : List {a_ty}), _root_.{len} ys = 0 → ys = [] := by
  intro ys h
  cases ys with
  | nil => rfl
  | cons y ys =>
    simp only [_root_.{len}] at h
    exact absurd h (by omega)
"#
                ),
                // The snoc-distribution aux lemma (zip over
                // append-singleton under length equality) — emitted
                // from the validated template, never as a sorry. Its
                // own premise threads by per-step stepping (C) and
                // vacuous discharge (D).
                format!(
                    r#"private theorem {theorem}_snoc (x y : {a_ty}) : ∀ (as bs : List {a_ty}),
    _root_.{len} as = _root_.{len} bs →
    _root_.{zip} (_root_.{app} as [x]) (_root_.{app} bs [y])
      = _root_.{appp} (_root_.{zip} as bs) [(x, y)] := by
  intro as
  induction as with
  | nil =>
    intro bs h
    have hb : bs = [] := {theorem}_len_zero bs (by simp only [_root_.{len}] at h; omega)
    subst hb
    simp [_root_.{app}, _root_.{zip}, _root_.{appp}]
  | cons a as ih =>
    intro bs h
    cases bs with
    | nil =>
      simp only [_root_.{len}] at h
      exact absurd h (by omega)
    | cons b bs =>
      have h' : _root_.{len} as = _root_.{len} bs := by simp only [_root_.{len}] at h; omega
      simp only [_root_.{app}, List.singleton_append, _root_.{zip}, _root_.{appp}, ih bs h']
"#
                ),
            ];
            let body = format!(
                r#"intro {xs}{sab}
induction {xs} with
| nil =>
  intro {ys} h
  have h0 : _root_.{len} {ys} = 0 := by
    have hh := {theorem}_bridge_eq (_root_.{len} []) (_root_.{len} {ys}) h
    simp only [_root_.{len}] at hh
    omega
  have hy : {ys} = [] := {theorem}_len_zero {ys} h0
  subst hy
  simp [_root_.{rev}, _root_.{zip}, _root_.{revp}]
| cons z x2 ih =>
  intro {ys} h
  cases {ys} with
  | nil =>
    have hh := {theorem}_bridge_eq (_root_.{len} (z :: x2)) (_root_.{len} []) h
    simp only [_root_.{len}] at hh
    exact absurd hh (by omega)
  | cons y x4 =>
    have hlen : _root_.{len} x2 = _root_.{len} x4 := by
      have hh := {theorem}_bridge_eq (_root_.{len} (z :: x2)) (_root_.{len} (y :: x4)) h
      simp only [_root_.{len}] at hh
      omega
    have hrevlen : _root_.{len} (_root_.{rev} x2) = _root_.{len} (_root_.{rev} x4) := by
      rw [{theorem}_len_rev, {theorem}_len_rev]; exact hlen
    have hih : _root_.{zip} (_root_.{rev} x2) (_root_.{rev} x4) = _root_.{revp} (_root_.{zip} x2 x4) := by
      apply ih
      rw [hlen]
      exact {theorem}_bridge_refl (_root_.{len} x4)
    calc _root_.{zip} (_root_.{rev} (z :: x2)) (_root_.{rev} (y :: x4))
        = _root_.{zip} (_root_.{app} (_root_.{rev} x2) [z]) (_root_.{app} (_root_.{rev} x4) [y]) := by
          simp only [_root_.{rev}]
      _ = _root_.{appp} (_root_.{zip} (_root_.{rev} x2) (_root_.{rev} x4)) [(z, y)] :=
          {theorem}_snoc z y (_root_.{rev} x2) (_root_.{rev} x4) hrevlen
      _ = _root_.{appp} (_root_.{revp} (_root_.{zip} x2 x4)) [(z, y)] := by rw [hih]
      _ = _root_.{revp} (_root_.{zip} (z :: x2) (y :: x4)) := by
          simp only [_root_.{zip}, _root_.{revp}, List.singleton_append]"#
            );
            (supports, body)
        }
        BridgePlan::EqElemInsert {
            eq_fn,
            elem_fn,
            or_fn,
            insert_fn,
            x,
            y,
            z,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let elem = aver_name_to_lean(elem_fn);
            let or = aver_name_to_lean(or_fn);
            let ins = aver_name_to_lean(insert_fn);
            let x = aver_name_to_lean(x);
            let y = aver_name_to_lean(y);
            let z = aver_name_to_lean(z);
            let supports = vec![bridge_eq_lemma(&eq), bridge_refl_lemma(&eq)];
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
have heq : {x} = {y} := {theorem}_bridge_eq {x} {y} h_when
subst heq
induction {z} with
| nil => simp [_root_.{ins}, _root_.{elem}, _root_.{or}, {theorem}_bridge_refl]
| cons c cs ih =>
  simp only [_root_.{ins}]
  split
  · simp [_root_.{elem}, _root_.{or}, {theorem}_bridge_refl, List.singleton_append]
  · simp only [List.singleton_append, _root_.{elem}, ih, _root_.{or}]
    split <;> simp"#
            );
            (supports, body)
        }
        BridgePlan::NeqElemInsert {
            eq_fn,
            neg,
            elem_fn,
            or_fn,
            insert_fn,
            x,
            y,
            z,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let elem = aver_name_to_lean(elem_fn);
            let or = aver_name_to_lean(or_fn);
            let ins = aver_name_to_lean(insert_fn);
            let heq = neg_norm(&eq, neg, &aver_name_to_lean(x), &aver_name_to_lean(y));
            let z = aver_name_to_lean(z);
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
{heq}
induction {z} with
| nil => simp [_root_.{ins}, _root_.{elem}, _root_.{or}, heq]
| cons c cs ih =>
  simp only [_root_.{ins}]
  split
  · simp [_root_.{elem}, _root_.{or}, heq, List.singleton_append]
  · simp only [List.singleton_append, _root_.{elem}, ih]"#
            );
            (Vec::new(), body)
        }
        BridgePlan::NeqCountInsert {
            eq_fn,
            neg,
            count_fn,
            insert_fn,
            x,
            y,
            z,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let count = aver_name_to_lean(count_fn);
            let ins = aver_name_to_lean(insert_fn);
            let heq = neg_norm(&eq, neg, &aver_name_to_lean(x), &aver_name_to_lean(y));
            let z = aver_name_to_lean(z);
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
{heq}
induction {z} with
| nil => simp [_root_.{ins}, _root_.{count}, heq]
| cons c cs ih =>
  simp only [_root_.{ins}]
  split
  · simp [_root_.{count}, heq, List.singleton_append]
  · simp only [List.singleton_append, _root_.{count}, ih]"#
            );
            (Vec::new(), body)
        }
        BridgePlan::NeqCountAppendSingleton {
            eq_fn,
            neg,
            count_fn,
            n,
            m,
            xs,
        } => {
            let eq = aver_name_to_lean(eq_fn);
            let count = aver_name_to_lean(count_fn);
            let heq = neg_norm(&eq, neg, &aver_name_to_lean(n), &aver_name_to_lean(m));
            let xs = aver_name_to_lean(xs);
            let body = format!(
                r#"intro {intro_givens} h_when{sab}
{heq}
induction {xs} with
| nil => simp [_root_.{count}, heq]
| cons c cs ih => simp only [List.cons_append, _root_.{count}, ih]"#
            );
            (Vec::new(), body)
        }
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
    for support in &supports {
        content.push_str(support);
        content.push('\n');
    }
    content.push_str(&format!(
        "{}{} {}\n",
        super::LAW_CLASS_MARKER_PREFIX,
        theorem,
        super::LAW_CLASS_UNIVERSAL
    ));
    content.push_str(&format!(
        "theorem {theorem} : ∀ {quant_params}, {prop} := by\n"
    ));
    for line in body.lines() {
        if line.is_empty() {
            content.push('\n');
        } else {
            content.push_str("  ");
            content.push_str(line);
            content.push('\n');
        }
    }

    // L2 of the iron guard: the lane grammar has no sorry carrier.
    debug_assert!(
        !content.contains("sorry"),
        "universal-lane module must not contain a sorry token"
    );

    let module = lane_module_id(&theorem_base, &content, entry_content);
    Some(LaneLawFile {
        label: format!("{}.{}", vb.fn_name, law.name),
        theorem,
        module,
        content,
    })
}

/// Re-indent a multi-line block for splicing at `spaces` depth — the
/// FIRST line is spliced in place (after a bullet/binder), later
/// lines get the pad. Mirrors `law_auto::decimal::indent_block`.
fn indent_block(block: &str, spaces: usize) -> String {
    let pad = " ".repeat(spaces);
    block
        .lines()
        .enumerate()
        .map(|(i, l)| {
            if i == 0 || l.is_empty() {
                l.to_string()
            } else {
                format!("{pad}{l}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// Like [`indent_block`] but pads EVERY line including the first —
/// for blocks spliced after a newline at a fixed column.
fn indent_block_all(block: &str, spaces: usize) -> String {
    let pad = " ".repeat(spaces);
    block
        .lines()
        .map(|l| {
            if l.is_empty() {
                l.to_string()
            } else {
                format!("{pad}{l}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}
