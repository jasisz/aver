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
#[derive(Clone)]
pub struct LaneLawFile {
    /// `fn.law` label for surfacing (`dispatchNumberOrErr.fromIntRoundtrip`).
    pub label: String,
    /// Twin theorem name (`<fn>_law_<law>_universal`).
    pub theorem: String,
    /// Companion theorem name (`<fn>_law_<law>_prop`), derived in the
    /// same module (CH-1). Carried alongside the twin so the collision
    /// guard can range over BOTH emitted declaration names.
    pub companion: String,
    /// Base name (`<fn>_law_<law>`) the twin/companion share — kept so
    /// the module hash can be re-derived after folding in helper
    /// content (lane imports).
    pub theorem_base: String,
    /// Module = file stem = lib name, content-hashed so a stale
    /// `.olean` under an old name is unreachable.
    pub module: String,
    /// Module names of the SOURCE-EARLIER lane helpers this module
    /// imports — the dependency edges the lane manifest records. Empty
    /// until a consumption figure (CH-3) populates it; the import
    /// machinery itself ([`with_lane_imports`]) is built here.
    pub imports: Vec<String>,
    /// Full `.lean` source. Contains NO `sorry` token.
    pub content: String,
}

/// One lane law the collision guard refused to emit: its twin or
/// companion name clashed with a theorem already emitted (a manifest
/// theorem, or an earlier lane law's twin/companion). Surfaced in the
/// lane detail artifact as an honest note rather than letting the
/// tolerated build fail and silently withhold a neighbor's credit.
pub struct OmittedLaw {
    /// `fn.law` label for surfacing.
    pub label: String,
    /// The emitted name that collided (`<fn>_law_<law>_universal` or
    /// `…_prop`).
    pub collides: String,
    /// Plain-language reason, written into the detail artifact.
    pub note: String,
}

/// Result of generating the lane for an entry scope: the emitted lane
/// modules plus the laws the collision guard honestly omitted.
pub struct LaneOutput {
    pub files: Vec<LaneLawFile>,
    pub omitted: Vec<OmittedLaw>,
}

/// Generate the lane files for every recognized when-law in the
/// entry scope. `entry_content` (the emitted manifest entry `.lean`)
/// is folded into each module's content hash so any manifest change
/// retires the old module names. `sabotage` is a TEST-ONLY hook
/// (`AVER_PROOF_LANE_SABOTAGE`): a label substring whose matching
/// law gets a deliberately failing tactic injected — the executable
/// proof that one broken lane proof cannot touch budgets or
/// neighbors.
///
/// COLLISION GUARD: before a lane module is emitted, its twin
/// (`…_universal`) and companion (`…_prop`) names are checked against
/// the names already in play — the counted-build manifest theorems
/// (parsed from `entry_content`) and every earlier lane law's twin and
/// companion. A clash (e.g. a sibling law literally named
/// `<law>_universal` or `<law>_prop`) would make the tolerated build
/// fail and SILENTLY withhold a neighbor's credit; instead the
/// colliding law is honestly OMITTED (no module, no credit attempt) and
/// recorded in [`LaneOutput::omitted`] so the lane detail artifact can
/// note it. The neighbor keeps its module and its credit.
///
/// `chain` is a TEST-ONLY hook (`AVER_PROOF_LANE_CHAIN`): with it set,
/// each emitted lane law imports every SOURCE-EARLIER emitted lane law
/// ([`with_lane_imports`]), wiring a real lane-to-lane dependency graph
/// against live `lake`. No emitter path drives imports yet (the
/// consumption figure is CH-3); the hook lets the two-module-chain
/// sabotage and stale-`.olean`-retirement gates exercise the
/// machinery end to end.
pub fn generate(
    ctx: &CodegenContext,
    entry_content: &str,
    sabotage: Option<&str>,
    chain: bool,
) -> LaneOutput {
    let pins = collect_pins(ctx);
    let entry_root = crate::codegen::common::entry_basename(ctx);
    // Names already emitted into the counted build (manifest theorems)
    // seed the guard; each emitted lane law then adds its twin and
    // companion before the next law is checked.
    let mut emitted_names = manifest_theorem_names(entry_content);
    let mut out: Vec<LaneLawFile> = Vec::new();
    let mut omitted = Vec::new();
    for item in &ctx.items {
        let TopLevel::Verify(vb) = item else { continue };
        let crate::ast::VerifyKind::Law(law) = &vb.kind else {
            continue;
        };
        let sabotage_this = sabotage
            .is_some_and(|s| format!("{}.{}", vb.fn_name, law.name).contains(s) && !s.is_empty());
        // Render the candidate (first matching family wins).
        let mut candidate: Option<LaneLawFile> = None;
        for pin in &pins {
            let Some(plan) = classify_lane_law(vb, law, ctx, pin) else {
                continue;
            };
            candidate = render_lane_law(
                vb,
                law,
                ctx,
                pin,
                &plan,
                &entry_root,
                entry_content,
                sabotage_this,
            );
            break; // first matching pin wins
        }
        if candidate.is_none()
            && let Some(plan) = classify_bridge_law(vb, law, ctx)
        {
            candidate = render_bridge_law(
                vb,
                law,
                ctx,
                &plan,
                &entry_root,
                entry_content,
                sabotage_this,
            );
        }
        let Some(file) = candidate else { continue };
        // Collision guard: refuse to emit if either emitted name clashes
        // with an existing manifest or earlier emitted theorem name.
        if let Some(collides) = collides_with(&file, &emitted_names) {
            omitted.push(OmittedLaw {
                label: file.label.clone(),
                collides: collides.clone(),
                note: format!(
                    "the universal-proof theorem `{collides}` would reuse a name another \
                     theorem in this proof already has; this law is skipped (no universal \
                     credit) so the name clash cannot fail a side build and silently strip \
                     a neighboring law of its credit"
                ),
            });
            continue;
        }
        emitted_names.insert(file.theorem.clone());
        emitted_names.insert(file.companion.clone());
        // Test hook: wire this law to import every source-earlier
        // emitted lane law, exercising the import + hash-folding
        // machinery against live lake.
        let file = if chain && !out.is_empty() {
            let helpers: Vec<(String, String)> = out
                .iter()
                .map(|h| (h.module.clone(), h.content.clone()))
                .collect();
            with_lane_imports(&file, &helpers, entry_content)
        } else {
            file
        };
        out.push(file);
    }
    LaneOutput {
        files: out,
        omitted,
    }
}

/// Twin or companion name that clashes with an already-emitted theorem
/// name, if any. Both are checked (the review measured both hazards).
fn collides_with(
    file: &LaneLawFile,
    emitted: &std::collections::HashSet<String>,
) -> Option<String> {
    if emitted.contains(&file.theorem) {
        return Some(file.theorem.clone());
    }
    if emitted.contains(&file.companion) {
        return Some(file.companion.clone());
    }
    None
}

/// Theorem names emitted into the counted build, parsed from the
/// manifest entry `.lean` — every `theorem <name>` / `private theorem
/// <name>` declaration. This is the ground truth the collision guard
/// ranges over (a sibling law named `<law>_universal` / `<law>_prop`
/// surfaces here as a `<fn>_law_<law>_universal` / `…_prop` manifest
/// theorem, the exact name a neighbor's lane twin/companion would
/// claim).
fn manifest_theorem_names(entry_content: &str) -> std::collections::HashSet<String> {
    let mut names = std::collections::HashSet::new();
    for line in entry_content.lines() {
        let trimmed = line.trim_start();
        let rest = trimmed
            .strip_prefix("theorem ")
            .or_else(|| trimmed.strip_prefix("private theorem "));
        if let Some(rest) = rest {
            let name: String = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '\'')
                .collect();
            if !name.is_empty() {
                names.insert(name);
            }
        }
    }
    names
}

/// Fold SOURCE-EARLIER lane helpers into a consumer lane module:
/// inject an `import <module>` line per helper (right after the entry
/// import) and re-derive the module hash so the helper CONTENT is part
/// of the consumer's identity — editing a helper renames every
/// consumer module, retiring stale `.olean`s. The dependency edges are
/// recorded on [`LaneLawFile::imports`].
///
/// This is the lane-imports MACHINERY. Nothing in the emitter drives it
/// yet (the consumption figure is CH-3); it is exercised by tests and
/// stands ready for that figure to call. `helpers` is the list of
/// `(module, content)` of the lane modules to import; `entry_content`
/// is the same manifest seed `generate` folds in, so the consumer hash
/// stays a pure function of (its own text, its helpers' text, the
/// manifest).
pub fn with_lane_imports(
    file: &LaneLawFile,
    helpers: &[(String, String)],
    entry_content: &str,
) -> LaneLawFile {
    if helpers.is_empty() {
        return file.clone();
    }
    // Insert the helper imports right after the LAST existing `import`
    // line (the entry-root import the renderers always emit first), so
    // lake resolves and orders the lane module graph automatically.
    let import_block: String = helpers
        .iter()
        .map(|(module, _)| format!("import {module}\n"))
        .collect();
    let mut content = String::new();
    let mut inserted = false;
    let lines: Vec<&str> = file.content.lines().collect();
    let last_import_idx = lines
        .iter()
        .rposition(|l| l.trim_start().starts_with("import "));
    for (i, line) in lines.iter().enumerate() {
        content.push_str(line);
        content.push('\n');
        if !inserted && Some(i) == last_import_idx {
            content.push_str(&import_block);
            inserted = true;
        }
    }
    if !inserted {
        // No existing import line (defensive): prepend the block.
        content = format!("{import_block}{}", file.content);
    }

    // Re-derive the module id folding in every helper's content, so a
    // changed helper retires the consumer's old module name.
    let helper_contents: Vec<&str> = helpers.iter().map(|(_, c)| c.as_str()).collect();
    let module = lane_module_id_with_deps(
        &file.theorem_base,
        &content,
        entry_content,
        &helper_contents,
    );

    LaneLawFile {
        module,
        imports: helpers.iter().map(|(m, _)| m.clone()).collect(),
        content,
        ..file.clone()
    }
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
/// Records the dependency edges (`imports`) so a consumer's
/// failure-tolerated build can be ordered after its helpers and a
/// reader can see the lane-to-lane graph; also carries the laws the
/// collision guard honestly omitted (so `--check` can surface them in
/// the detail artifact without re-deriving the clash).
pub fn lane_manifest_json(lane: &LaneOutput) -> String {
    let laws: Vec<serde_json::Value> = lane
        .files
        .iter()
        .map(|l| {
            serde_json::json!({
                "law": l.label,
                "theorem": l.theorem,
                "module": l.module,
                "imports": l.imports,
            })
        })
        .collect();
    let omitted: Vec<serde_json::Value> = lane
        .omitted
        .iter()
        .map(|o| {
            serde_json::json!({
                "law": o.label,
                "collides": o.collides,
                "note": o.note,
            })
        })
        .collect();
    serde_json::to_string_pretty(&serde_json::json!({
        "version": 1,
        "laws": laws,
        "omitted": omitted,
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
    lane_module_id_with_deps(theorem_base, content, entry_content, &[])
}

/// [`lane_module_id`] with imported-helper content folded in: each
/// helper's content rotates into the hash so editing a helper changes
/// every consumer module's name (stale-`.olean` retirement across the
/// import chain). A consumer with no helpers hashes byte-identically to
/// `lane_module_id` (the rotation reduces to the empty fold).
fn lane_module_id_with_deps(
    theorem_base: &str,
    content: &str,
    entry_content: &str,
    helper_contents: &[&str],
) -> String {
    let mut hash = fnv1a64(content.as_bytes()) ^ fnv1a64(entry_content.as_bytes()).rotate_left(1);
    for (i, helper) in helper_contents.iter().enumerate() {
        // Rotate by a position-dependent amount so two helpers swapping
        // order yield a different hash (the import order is meaningful
        // to lake's elaboration); +2 keeps it distinct from the entry
        // rotation above.
        hash ^= fnv1a64(helper.as_bytes()).rotate_left((i as u32 + 2) % 64);
    }
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
