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

use crate::ast::TopLevel;
use crate::codegen::CodegenContext;

mod bridge;
mod shared;
mod sign;

use bridge::{classify_bridge_law, render_bridge_law};
use sign::{classify_lane_law, collect_pins, render_lane_law};

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
