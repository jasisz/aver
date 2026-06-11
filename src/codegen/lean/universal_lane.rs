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
//! The v1 prover content is the scalar-sign family: the law's `when`
//! is a sign guard (`n > 0` / `n < 0` / `n >= 0`) on a single Int
//! given, and the LHS enters the SAME canonical decimal parser
//! pipeline an `IntDecimalRoundtrip` pin (#470) already certified for
//! the file — the lane proof is a segment/polarity parameterization
//! of that skeleton (`law_auto/decimal.rs`), with the premise
//! consumed ONCE at the existing `rcases` sign split. Exactly the
//! five empirically validated (kernel-clean on the emitted json
//! project, Lean 4.15) entry-segment × polarity combinations are
//! recognized; everything else declines at zero cost:
//! - digit-dispatch wrapper entry, `when n >= 0` (dispatchNumberOrErr)
//! - `pos_fn` entry, `when n > 0` (startNumberDigits)
//! - `neg_fn` entry, `when n < 0` (parseNumberSign)
//! - `sign_fn` entry, `when n < 0` (startSignDigit)
//! - scanner entry with pinned `n == 0` Bool arg, `when n >= 0`
//!   (scanIntTail)
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
use crate::ast::{BinOp, Expr, Literal, Pattern, Spanned, Stmt, TopLevel, VerifyBlock, VerifyLaw};
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
    if pins.is_empty() {
        return Vec::new();
    }
    let entry_root = crate::codegen::common::entry_basename(ctx);
    let mut out = Vec::new();
    for item in &ctx.items {
        let TopLevel::Verify(vb) = item else { continue };
        let crate::ast::VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        for pin in &pins {
            let Some(plan) = classify_lane_law(vb, law, ctx, pin) else {
                continue;
            };
            let sabotage_this = sabotage.is_some_and(|s| {
                format!("{}.{}", vb.fn_name, law.name).contains(s) && !s.is_empty()
            });
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
            }
            break; // first matching pin wins
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
    let module = format!("U_{sanitized}_{:08x}", (hash & 0xffff_ffff) as u32);

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
