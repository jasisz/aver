//! Proof intermediate representation.
//!
//! Single decision substrate the Lean and Dafny proof exporters
//! consume. Backends render text from a fully-resolved `ProofIR` —
//! they do not classify shapes, do not derive contracts, do not
//! decide between native and fuel emit. Every decision happens once
//! in the `proof_lower` pipeline stage; both backends see the same
//! decision and either render it consistently or fail consistently.
//!
//! Replaces the ad-hoc "guess and emit" pattern that grew across
//! `src/codegen/{common,recursion,lean,dafny}` during 0.22.0 with a
//! single typed model. Each variant that says "emit native" or
//! "lift to subtype" carries inside its payload everything the
//! backend needs and everything the classifier proved — the type
//! system makes it impossible to produce a "native" decision
//! without also producing the side-conditions that justify it.
//!
//! Coverage today: `refined_types` (refinement-via-opaque records),
//! `fn_contracts` (per-pure-fn recursion shape), `law_theorems`
//! (per-verify-law strategy + quantifier + claim decomposition).
//!
//! ## Invariant after #147 phase E PR 12 Scope A
//!
//! - **Backend-facing IR fields are resolved.** Every
//!   `Spanned<...>` on this module's public structs carries
//!   [`crate::ir::hir::ResolvedExpr`], not raw `ast::Expr`.
//!   `proof_lower::populate_*` resolves at the producer site through
//!   the symbol table under the correct scope (entry / dep-module
//!   prefix); backends walk the resolved form directly via
//!   `emit_expr`, no `emit_expr_legacy` adapter for IR-sourced
//!   expressions.
//! - **Identity-sensitive decisions use typed IDs.**
//!   `fn_contracts` is keyed by [`FnId`] (not bare name);
//!   `refined_types` is keyed by [`TypeId`]; `law_theorems` carry
//!   the target fn's `FnId`. The Lean/Dafny native-guarded rewriter
//!   pins target by `FnId` (via `fn_id_for_decl`), not bare name —
//!   regression-pinned by
//!   `proof_export_module_owned_native_guarded_resolves_correct_fn_id`.
//! - **`proof_lower` internal AST discovery is intentional.**
//!   The classifier's pattern matchers still walk raw `ast::Expr`
//!   inside this stage. Source-pattern matching is the natural
//!   shape for the discovery work (refinement carrier search,
//!   `Map.set` axiom detection, spec-equivalence comparison); a
//!   resolved walker would be the same logic spelled in a different
//!   enum. Identity-sensitive sites that COULD leak across scope
//!   were audited and are bounded today by:
//!     1. `vb.fn_name` parses as a single `Ident` (verify blocks
//!        target entry-only fns by current grammar)
//!     2. Builtin matchers (`"Bool.and"`, `"Map.set"`, …) compare
//!        global namespace methods that have no per-scope identity.
//! - **Full `ResolvedProofLowerView` + semantic matcher API is
//!   deferred** until a real trigger lands: module-scoped verify
//!   blocks, dotted verify targets / laws over dep-module fns, LSP
//!   rename, inliner / monomorphizer / cross-scope transforms. Each
//!   of those unbounds the "entry-only" assumption above. When it
//!   ships, the right architecture is a typed
//!   `ProofLowerInputs::resolved_fn_view(fd)` + matcher helpers
//!   (`callee_is_builtin`, `callee_is_fn(fn_id)`, `ctor_is`,
//!   `ident_name`, `int_lit`) — not a mechanical
//!   `Expr -> ResolvedExpr` rewrite of the discovery walkers.

use std::collections::HashMap;

use crate::ast::Spanned;

// Identity keys for named declarations crossing module boundaries
// (`FnKey`, `TypeKey`, `LawKey`) live in `crate::ir::identity` —
// they're general identity primitives, not proof-specific. Today's
// proof flow is where the bare-string bug class first surfaced
// (reviewer rounds 5/6), but other backends with multi-module emit
// (VM, Rust codegen, WASM) will reach for the same types when they
// hit the same bug class. Re-exported here for ergonomics.
pub use crate::ir::identity::{CtorId, FnId, FnKey, LawKey, ModuleId, TypeId, TypeKey};

/// Output of the `proof_lower` pipeline stage. Every decision the
/// proof backends will make is materialised here; backends become
/// pure renderers.
///
/// `ProofIR` is intentionally NOT a closed superset of the AST — it
/// only carries facts that proof export needs. Source-faithful
/// emission of plain fns / verify cases still flows through the
/// untyped AST path, same as runtime backends (VM, Rust, WASM).
#[derive(Debug, Clone, Default)]
pub struct ProofIR {
    /// Every refinement-lifted user type, keyed by opaque [`TypeId`]
    /// from the symbol table. Same-bare-name refined records in two
    /// modules (`A.Natural` vs `B.Natural`) get distinct IDs, so
    /// their predicates never merge. Includes types declared in the
    /// entry items and in dependent modules; name resolution happens
    /// once in `populate_refined_types`, consumers look up directly
    /// by id through `ctx.symbol_table`.
    pub refined_types: HashMap<TypeId, RefinedTypeDecl>,
    /// Per-pure-fn contract describing what proof artifact the fn
    /// lowers to (native / fuel / structural / linear recurrence).
    /// Keyed by opaque [`FnId`] from the symbol table — name
    /// resolution happens once in `populate_fn_contracts`;
    /// consumers thereafter use `ctx.symbol_table` to resolve
    /// `&FnDef → FnId` and look up directly. Cross-module
    /// same-bare-name fns get distinct IDs, so the lookup is
    /// unambiguous without per-call-site scope plumbing.
    pub fn_contracts: HashMap<FnId, FnContract>,
    /// Per-verify-law theorem decomposed into quantifiers, premises,
    /// and claim with all wrapper-strip / val-projection / drop-vs-
    /// keep decisions baked in, plus the pinned proof strategy.
    pub law_theorems: Vec<LawTheorem>,
    /// Recursive pure fns whose shape fell outside every recognised
    /// pattern. Surfaced as diagnostics ("recursive function 'foo'
    /// is outside proof subset (...)") and steers the consumer to
    /// either skip the fn or emit it as a partial/axiom fallback.
    /// Carried in ProofIR so consumers don't re-run the classifier
    /// just to see what failed.
    pub unclassified_fns: Vec<UnclassifiedFn>,
}

/// A recursive pure fn the contract classifier couldn't match against
/// any supported shape. Carries the source line + a human-readable
/// reason string so backends can render a diagnostic without
/// inventing prose.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnclassifiedFn {
    pub line: usize,
    pub message: String,
}

/// Refinement smart-constructor guard a `SimpOmegaUnfold` strategy
/// found in the law's fn unfold chain. `param` is the smart
/// constructor's input parameter name; `predicate` is the Bool
/// subject of its `match true/false → Ok/Err` body. Backends emit
/// `by_cases h_<v> : <substituted predicate>` for each law-given by
/// rewriting `param` to `<v>` inside the predicate.
#[derive(Debug, Clone)]
pub struct SmartGuard {
    pub param: String,
    pub predicate: Spanned<crate::ir::hir::ResolvedExpr>,
}

/// A refinement-lifted user type — opaque record with a single
/// carrier field, paired with a validating smart constructor. The
/// presence of this decl in `ProofIR.refined_types` is the
/// decision: "emit this as a subtype on Lean and a subset type on
/// Dafny". Backends never re-decide.
#[derive(Debug, Clone)]
pub struct RefinedTypeDecl {
    /// Source-level type name (e.g. `"Natural"`). NOT canonicalised
    /// — backends emit using the source name; canonical form is the
    /// map key.
    pub name: String,
    /// Carrier annotation from the record's single field (typically
    /// `"Int"`). Drives the Lean Subtype underlying type and the
    /// Dafny subset type's base.
    pub carrier_type: String,
    /// Carrier-field source name (e.g. `"value"`). Lean uses `.val`
    /// to project Subtype values regardless of source name; Dafny's
    /// subset binds the source name in its predicate.
    pub carrier_field: String,
    /// Smart constructor's input parameter name (e.g. `"n"`) — the
    /// invariant predicate's free variable.
    pub predicate_param: String,
    /// Bool predicate that every value of the refined type must
    /// satisfy, in terms of `predicate_param`. Comes from the smart
    /// constructor's `match <pred> { true -> Ok(...); false -> Err(...)
    /// }` subject.
    pub invariant: Predicate,
    /// Inhabitation witness: a literal value of `carrier_type` that
    /// the lowerer verified satisfies `invariant`. Resolved by first
    /// trying the smart constructor's verify block (`fromX(K) =>
    /// Ok(...)` for some literal K — verified by the user via
    /// `aver verify`), then evaluating the predicate against small
    /// candidates as a fallback.
    ///
    /// Why the IR carries this even though only Dafny's subset type
    /// strictly *requires* a non-emptiness witness: it's a fact
    /// about the type (∃ v : carrier, invariant(v) holds), not a
    /// Dafny-specific syntactic obligation. Backends use it as they
    /// see fit:
    ///
    /// - Dafny: emits `type X = v: int | P v witness <W>`. Required
    ///   for the subset type to be inhabited and elaborable.
    /// - Lean: currently unused — propositional `Subtype` may be
    ///   empty, so `{ v : Int // P v }` elaborates regardless. Step
    ///   N+1 could emit a `def sample_X : X := ⟨W, by decide⟩` for
    ///   roundtrip / test convenience.
    /// - Future Z3 / Coq / etc.: same fact, rendered per target.
    ///
    /// `None` when no satisfier was found. Backends that require a
    /// witness must either reject the type or fall back to a target-
    /// default (Dafny picks `0` and crosses fingers).
    pub witness: Option<String>,
    /// Constant integer interval over-approximating `invariant`, as
    /// derived by [`crate::ir::interval::interval_of_invariant`] from
    /// the same predicate. `Some([lo, hi])` when the invariant shape
    /// was recognized (a comparison / `Bool.and` against integer
    /// literals); `None` when the analysis declined (unrecognized
    /// shape — `Bool.or`, non-literal bound, structural carrier).
    ///
    /// Persisted here so a carrier-lowering codegen recognizer (the
    /// next slice of the Int-semantics effort) can read the bound
    /// directly off the `TypeId`-keyed decl — the same identity key
    /// the interval analysis uses — without re-running the analysis
    /// behind the `--explain-passes` diagnostic flag. The value is
    /// identical to what `aver compile --explain-passes` reports for
    /// the same type; both paths call the one `interval` analysis.
    pub interval: Option<crate::ir::interval::Interval>,
    /// Per-arithmetic-op overflow classification, in module-walk
    /// order. Each entry pairs the operation's source name with its
    /// [`crate::ir::interval::OpClass`] — `OverflowFree` when every
    /// `i64` intermediate across the op body provably fits `i64`
    /// (the carrier-lowering candidate), `NeedsWiderScratch` /
    /// `Unbounded` otherwise. Empty when the type exposes no
    /// carrier arithmetic or when `interval` is `None`.
    ///
    /// Populated by [`crate::ir::interval::analyze`] over the same
    /// `ProofLowerInputs` `populate_refined_types` consumed, so the
    /// classification is byte-identical to the `--explain-passes`
    /// report. The codegen recognizer reads this to flag a carrier
    /// "raw-i64-eligible" per op.
    pub op_classes: Vec<(String, crate::ir::interval::OpClass)>,
}

impl RefinedTypeDecl {
    /// Whether this refined type may have a **raw `i64` carrier** — the
    /// gate a later codegen slice will trust to lower the bignum `Int`
    /// carrier to a machine word. Derived ONLY from the persisted
    /// [`Self::interval`] and [`Self::op_classes`] facts; it never
    /// re-runs the interval analysis or inspects the invariant syntax.
    ///
    /// Returns `true` IFF, conservatively, the type is provably safe to
    /// store and operate on as a raw `i64`:
    ///
    /// - [`Self::interval`] is `Some` — the analysis recognized the
    ///   invariant shape (a `None` is the analysis's conservative
    ///   *decline*, never eligible); AND
    /// - that interval **fits `i64`** ([`crate::ir::interval::Interval::fits_i64`]),
    ///   which holds IFF **both bounds are finite and within
    ///   `[i64::MIN, i64::MAX]`**. This single test subsumes
    ///   "two-sided" (an open / one-sided bound is `±inf`, which never
    ///   fits) and the `i64`-range check — a `Natural` (`[0, +inf]`) or
    ///   any interval wider than `i64` is rejected here; AND
    /// - **every** entry in [`Self::op_classes`] is
    ///   [`crate::ir::interval::OpClass::OverflowFree`] — a single
    ///   `NeedsWiderScratch` or `Unbounded` op means some carrier
    ///   arithmetic can wrap a raw `i64` before the smart constructor's
    ///   guard re-validates, so the whole carrier stays bignum.
    ///
    /// Anything else → `false`. This is conservative in exactly the
    /// soundness-critical direction: a wrongly-`true` answer would let
    /// slice 4 lower a carrier whose ops can wrap, silently reintroducing
    /// the model-vs-runtime gap the whole mechanism exists to close. The
    /// predicate declines whenever the facts do not *prove* safety.
    ///
    /// ## Empty `op_classes`
    ///
    /// A type with a finite-`i64` interval but **no** carrier-reading
    /// arithmetic ops (e.g. only `fromInt` / `toInt`, which
    /// [`crate::ir::interval::classify_ops_in_scope`] skips) is reported
    /// **eligible**. The decision is defensible because both soundness
    /// obligations are met vacuously: storage of any inhabitant fits
    /// `i64` (it is within the proven interval), and there is no op that
    /// could overflow a raw `i64` (the `all(...)` over an empty op set is
    /// `true`). The only thing the raw carrier adds over bignum — an op
    /// that must not wrap before the guard — has nothing to apply to.
    /// This is "eligible for storage", which is exactly what the gate
    /// asks. If a future op is added to the type, it is re-classified and
    /// can demote the type then; the determination is recomputed from the
    /// persisted facts every time, never cached.
    pub fn raw_i64_eligible(&self) -> bool {
        // Delegates to the single shared recognizer so this persisted-
        // fact gate and the `--explain-passes` diagnostic can never
        // diverge about which types are eligible.
        crate::ir::interval::raw_i64_eligible(
            self.interval,
            self.op_classes.iter().map(|(_, class)| class),
        )
    }
}

/// Per-pure-fn proof contract — what recursion shape (if any) the
/// lowerer pinned for emit.
#[derive(Debug, Clone)]
pub struct FnContract {
    pub source_name: String,
    /// `None` means non-recursive (plain emit). `Some` says native /
    /// fuel / structural / whatever the lowerer decided, with all
    /// side-conditions inlined.
    pub recursion: Option<RecursionContract>,
}

/// Recursion-shape decision. Each variant carries everything its
/// emit needs AND the side-conditions the lowerer proved to choose
/// it. The variants intentionally cannot be constructed without
/// their side-conditions — backends cannot render `Native` without
/// the lowerer having proved preservation + decrease.
#[derive(Debug, Clone)]
pub enum RecursionContract {
    /// Fuel-encoded fallback. No side-conditions to prove; works
    /// for any shape the classifier accepted as recursive.
    Fuel {
        /// Symbolic measure feeding the wrapper (`natAbs n + 1`,
        /// `|xs| + 1`, etc.). Backends translate per target.
        fuel_metric: FuelMetric,
    },
    /// Affine second-order linear recurrence on `Int`, shape
    /// `f(n) = a*f(n-1) + b*f(n-2)` with literal `0`/`1` base cases
    /// and an `n < 0` guard. Lowered to a private Nat pair-state
    /// worker (Lean / Dafny both emit native structural recursion on
    /// the Nat counter, no fuel). The lowerer doesn't carry the
    /// shape coefficients yet — backends still pattern-match the
    /// fn body via `lean::recurrence::detect_second_order_int_
    /// linear_recurrence`. Step N+1 could materialise them here.
    LinearRecurrence2,
    /// Native recursion with explicit precondition. Lowerer proved
    /// both `preservation` (rec args stay in domain) and `decrease`
    /// (measure strictly drops) before constructing this variant.
    /// Currently specialised to the IntCountdown-literal-zero shape
    /// (`match p { 0 -> BASE; _ -> rec(p-1, ...) }`); other native-
    /// recursion shapes (e.g. linear recurrence on a pair-state
    /// worker) will land as additional `RecursionContract` variants.
    Native {
        /// Conjunction of precondition clauses, kept as a vector so
        /// backends can render one `requires` per clause (Dafny) or
        /// fold into a single `&&` chain (Lean). Empty means "no
        /// caller-derived precondition" — the backend synthesises a
        /// fibTR-style default (`param ≥ 0`) at emit time.
        precondition: Vec<Predicate>,
        /// Symbolic measure (e.g. `natAbs(n)`). Backends render per
        /// target language (`Int.natAbs n` on Lean, `n` with a
        /// `requires n >= 0` clause on Dafny).
        measure: Measure,
        /// Side-condition tag: lowerer attests the recursive args
        /// preserve the precondition. Empty enum payload — its
        /// existence in the type is the proof, not its content.
        preservation: PreservationProof,
        /// Same for the decreasing measure.
        decrease: DecreaseProof,
        /// Body decomposition for the IntCountdown-literal-zero shape:
        /// the literal int that selects the base arm, the base arm's
        /// body, and the wildcard arm's body. Carried so backends can
        /// render the `if h : p = <lit> then base else rec(p-1, ...)`
        /// switch without re-walking the source AST. The literal is
        /// always `0` today — the `IntCountdownLiteralZero`
        /// preservation marker attests it; carrying the value as data
        /// keeps the IR shape forward-compatible with future
        /// preservation proofs that admit other literals.
        body: NativeIntCountdownBody,
    },
    /// Well-founded native def on `param.toNat` — graduates a fn out
    /// of the fuel/partial encoding so it stays kernel-transparent
    /// (Lean: `termination_by param.toNat` + a `decreasing_by` the
    /// kernel re-checks; Dafny: `decreases if param >= 0 then param
    /// else 0` with NO synthesized `requires`, so total callers stay
    /// wellformed). Two validated sources:
    ///
    /// - `floor_div: Some(..)` — every self-call shrinks `param` by a
    ///   literal-divisor floor division
    ///   (`Result.withDefault(Int.div(p, k), d)` with literal k >= 2,
    ///   possibly through a unary wrapper fn), and the classifier
    ///   verified the guard chain enclosing every self-call site
    ///   implies `p >= 1` — so `p / k < p` and the measure strictly
    ///   drops. Never guessed: a fn whose guards don't justify the
    ///   shrink keeps its prior (partial/opaque) emission.
    /// - `floor_div: None` — guard-protected subtractive countdown
    ///   (`p - k`, literal k >= 1, guards imply `p >= 1`), graduated
    ///   out of fuel on demand by the floor-division window law
    ///   family, whose proof templates need the fn's defining
    ///   equations and functional-induction principle.
    WellFoundedToNat {
        /// The decreasing Int parameter (source name).
        param: String,
        /// `Some` for the floor-division shrink; `None` for the
        /// guarded subtractive countdown.
        floor_div: Option<FloorDivShrink>,
    },
}

/// Payload of [`RecursionContract::WellFoundedToNat`] for the
/// floor-division shrink shape.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloorDivShrink {
    /// The literal divisor (>= 2).
    pub divisor: i64,
    /// `Some(name)` when the self-call shrinks through a unary
    /// wrapper fn whose body is exactly
    /// `Result.withDefault(Int.div(x, divisor), <int literal>)`;
    /// `None` when the `Result.withDefault(Int.div(p, k), d)` call
    /// is inlined at the self-call site. Lean's `decreasing_by`
    /// unfolds the wrapper by name.
    pub helper_fn: Option<String>,
}

/// Body decomposition for the `IntCountdown-literal-zero` native
/// shape. Each field is a slice of the source AST the lowerer
/// extracted while classifying; backends render them directly
/// without re-walking the source.
#[derive(Debug, Clone)]
pub struct NativeIntCountdownBody {
    /// The literal int that selects the base arm. Always `0` today;
    /// future preservation proofs may admit other literals, so the
    /// value is carried as data rather than baked into the marker.
    pub base_arm_literal: i64,
    /// AST for the base arm's body (`match p { 0 -> THIS; _ -> ... }`).
    pub base_arm_body: Spanned<crate::ir::hir::ResolvedExpr>,
    /// AST for the wildcard arm's body — the recursive call site.
    pub wildcard_arm_body: Spanned<crate::ir::hir::ResolvedExpr>,
}

/// Fuel metric for the fallback fuel-encoded emit path.
#[derive(Debug, Clone)]
pub enum FuelMetric {
    /// `n.natAbs + 1` — classic IntCountdown fuel.
    NatAbsPlusOne { param: String },
    /// `(bound - n).natAbs + 1` — IntAscending: param climbs toward
    /// a bound expression. Backends render the bound through their
    /// own `Spanned<Expr>` emitter (Lean: `bound_expr_to_lean`,
    /// Dafny: `emit_expr` over int subset).
    BoundMinusParamNatAbsPlusOne {
        param: String,
        bound: Spanned<crate::ir::hir::ResolvedExpr>,
    },
    /// `xs.length + 1` — List/String structural recursion.
    SeqLenPlusOne { param: String },
    /// `sizeOf(x) + 1` — structural recursion on a user-defined
    /// recursive ADT (e.g. `Term::App(f, arg)`). The classifier
    /// doesn't pin the bound param — sizeOf walks the whole call
    /// frame — so this variant carries no param name.
    SizeOfPlusOne,
    /// `s.length - pos` — StringPosAdvance: a `String` carrier stays
    /// invariant, an `Int` position climbs toward its length.
    StringLenMinusPos {
        string_param: String,
        pos_param: String,
    },
    /// Lexicographic pair for mutual recursion SCCs.
    Lex { params: Vec<String>, rank: usize },
}

/// Symbolic termination measure. Backend-agnostic.
#[derive(Debug, Clone)]
pub enum Measure {
    NatAbsInt { param: String },
    SeqLen { param: String },
    Lex(Vec<Measure>),
}

/// Marker that the lowerer constructed a proof of preservation
/// (recursive args stay in the precondition's domain). The variants
/// describe HOW the proof was constructed so future maintainers can
/// trace why a given shape was accepted as native.
#[derive(Debug, Clone)]
pub enum PreservationProof {
    /// `match p { 0 -> base; _ -> rec(p-1, ...) }` under `p ≥ 0`
    /// precondition. Wildcard arm gives `p ≠ 0`, combined with
    /// `p ≥ 0` yields `p ≥ 1`, so `p - 1 ≥ 0`.
    IntCountdownLiteralZero,
}

/// Symmetric marker for the decreasing measure.
#[derive(Debug, Clone)]
pub enum DecreaseProof {
    /// `natAbs(p - 1) < natAbs(p)` under `p ≥ 0 ∧ p ≠ 0`.
    NatAbsCountdown,
}

/// Lowered verify-law theorem. All projection decisions (`.val`
/// vs bare ident, wrapper strip, when-keep vs when-drop) are
/// already baked into the fields below; backends render directly.
#[derive(Debug, Clone)]
pub struct LawTheorem {
    /// Opaque identity of the fn this law targets, resolved through
    /// `SymbolTable` at populate time (phase E3). Verify laws are
    /// entry-only per the current model, so this is effectively
    /// always an entry-scope `FnId` today; once laws-in-modules
    /// lands the same `FnId` will distinguish two same-bare-name
    /// recursive fns across modules without any per-callsite scope
    /// plumbing.
    pub fn_id: FnId,
    pub law_name: String,
    pub quantifiers: Vec<Quantifier>,
    /// Premises in order. Already includes `when` if it carries
    /// information beyond the refinement invariants (the lowerer
    /// performs the bijective syntactic equivalence check).
    pub premises: Vec<Predicate>,
    /// LHS = RHS claim. Wrapper-stripped, lifted-var-aware (bare
    /// idents for arg positions, `.val` projections inside
    /// comparator BinOps if the lowerer determined this is needed).
    pub claim_lhs: Spanned<crate::ir::hir::ResolvedExpr>,
    pub claim_rhs: Spanned<crate::ir::hir::ResolvedExpr>,
    pub strategy: ProofStrategy,
}

/// A universally-quantified variable in a law theorem. Carries
/// enough type info for backends to render the binder correctly
/// (`(a : Natural)` for refined Int, `(a : Int)` for plain int,
/// `(rng : RandomIntInBounds)` for oracle).
#[derive(Debug, Clone)]
pub struct Quantifier {
    pub name: String,
    pub binder_type: QuantifierType,
}

#[derive(Debug, Clone)]
pub enum QuantifierType {
    /// Plain Aver type, rendered as-is on each backend.
    Plain(String),
    /// Refinement-lifted: source declared `given a: Int`, body used
    /// `Natural(value = a)`, so the quantifier binds at the refined
    /// type. The carried `refined_type` key looks up in
    /// `ProofIR.refined_types`.
    RefinedTo { refined_type: String },
    /// Oracle subtype: classified Generative-shape effect-givens
    /// bind oracles wrapped in a subtype carrier
    /// (`RandomIntInBounds`, `RandomFloatInUnit`,
    /// `TimeUnixMsNonneg`).
    OracleSubtype(String),
}

/// Algebraic / proof-theoretic shape of a verify-law theorem.
///
/// **Naming rule**: variants describe **what the law says**, not
/// **how a backend proves it**. The IR is target-agnostic — Lean
/// maps `Commutative { op: Add }` to `simp [fn, Int.add_comm]`,
/// Dafny maps the same variant to its own lemma vocabulary, a Z3
/// backend could ship a different tactic again. Tactic names
/// (`SimpOverLemmas`, `simp+omega`) do not appear in variant names;
/// Driver of a [`ProofStrategy::WrapperOverRecursion`] inner loop —
/// the structure the recursion shrinks. `List` is the original
/// `sum_acc` shape (`match xs { [] -> acc; h::t -> loop(t, step) }`);
/// `PeanoNat` is the `factTR` countdown (`match n { Z -> acc; S(m) ->
/// loop(m, combine(n, acc)) }`). The two need different induction
/// skeletons (`nil`/`cons` vs `zero`/`succ`) and, for `Mul`, different
/// closing tactics (`omega` can't discharge a nonlinear residual).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WrapperDriver {
    /// Structural fold over a `List<_>` first parameter.
    List,
    /// Countdown over a Peano-`Nat` ADT first parameter. Carries the
    /// ADT's source type name and whether the folded value (the matched
    /// subject) is the combine fn's FIRST argument (`mul(n, acc)` →
    /// `true`; `mul(acc, n)` → `false`) so the backend rewrite matches
    /// the def's actual step term.
    PeanoNat {
        type_name: String,
        value_first: bool,
    },
}

/// `LinearArithmetic` is named for the semantic, not the tactic.
#[derive(Debug, Clone)]
pub enum ProofStrategy {
    /// `rfl` / definitional equality — `lhs ≡ rhs` syntactically.
    Reflexive,
    /// `simp` chain over named lemmas. The discovery feedback loop
    /// (`lemma_discovery::committed`) pins this when a committed
    /// `DiscoveredLemmas.lean` holds kernel-proved lemmas in-scope
    /// for an `Induction` law: the names are the discovered theorem
    /// names, and the Lean renderer reuses the induction ladder with
    /// those lemmas embedded + joined to its simp sets. Pinned by
    /// the CLI (post-lowering re-pin), never by
    /// `classify_law_strategy` — discovery feedback is opt-in via
    /// the committed artifact.
    SimpOverLemmas(Vec<String>),
    /// `∀ a b, f(a, b) = f(b, a)` — commutativity of the law's fn,
    /// whose body reduces to `a <op> b`. The `op` tag lets backends
    /// pick their own lemma vocabulary (Lean: `Int.add_comm`,
    /// Dafny: built-in arithmetic axioms).
    Commutative { op: crate::ast::BinOp },
    /// `∀ a b c, f(f(a,b),c) = f(a,f(b,c))` — associativity of `f`.
    Associative { op: crate::ast::BinOp },
    /// `∀ a, f(a, e) = a` (or the swapped `f(e, a) = a`) — the
    /// identity-element law for the underlying op (`e` = `0` for
    /// Add / Sub, `1` for Mul). Backends emit `simp [fn]` (the
    /// wrapper's body unfolds to the identity equation, which simp
    /// closes); the variant doesn't need a `side` field because
    /// the emit is symmetric — Sub is naturally one-sided (only
    /// right-identity), Add/Mul accept either side. The lowerer
    /// guarantees the law's actual shape matches the op's identity
    /// behaviour before pinning.
    IdentityElement { op: crate::ast::BinOp },
    /// `∀ a b, f(a, b) = -f(b, a)` (or the swapped negation).
    /// `neg_on_rhs` records which side carries the `-` wrap so
    /// backends with directional lemmas (Lean's `Int.neg_sub b a :
    /// -(b - a) = a - b`) can flip via `.symm` correctly.
    AntiCommutative {
        op: crate::ast::BinOp,
        /// `true` for `f(a, b) = -f(b, a)` (negation on rhs);
        /// `false` for the swapped arrangement.
        neg_on_rhs: bool,
    },
    /// `∀ a, g(a) = f(a, c)` or `f(c, a)` — the unary fn `g` is
    /// the binary fn `f` with one argument bound to constant `c`.
    /// Backends unfold both fns to expose the underlying op; the
    /// IR carries `inner_fn` (the binary's source name) so the
    /// unfold list is unambiguous.
    UnaryEqualsBinary {
        /// Source-level name of the binary fn the unary one equals.
        inner_fn: String,
    },
    /// "Linear arithmetic over an unfold chain" — the law's two
    /// sides reduce to a flat linear equation on Int once every
    /// reachable user fn is unfolded. Generic catch-all for Int
    /// laws that don't fit a named algebraic property. The IR
    /// captures the unfold list + wrapper-return signal +
    /// refinement smart-constructor guard; backends translate to
    /// their decision procedure (Lean: `simp + omega`, Dafny: Z3
    /// linear int prover). Named for the **semantic** ("linear
    /// arithmetic"), not the Lean tactic.
    LinearArithmetic {
        /// Ordered fn unfold list. Top-level law fn first — Lean's
        /// `unfold` resolves left-to-right and the call layer the
        /// tactic peels at each step must match the goal shape.
        unfold_fns: Vec<String>,
        /// `true` when at least one fn in `unfold_fns` returns a
        /// wrapper (Result, Option, …). Drives extra case-split
        /// machinery in the emit — pure linear-arithmetic provers
        /// can't close constructor-equality goals, so the wrapper
        /// case splits on the smart-constructor guard first.
        wrapper_return: bool,
        /// Smart-constructor guard pulled from a refinement
        /// `fromX(p: Int) -> Result<X, _>` in the unfold chain.
        /// `Some` when one was found; `None` falls back to a
        /// conservative `(n ≥ 0)` default when `wrapper_return`
        /// forces case-splitting.
        smart_guard: Option<SmartGuard>,
        /// `true` when at least one law given is lifted to a
        /// refinement type (`given a: Int` used as `Refined(value
        /// = a)` in the law body). The Subtype/subset lift carries
        /// the invariant in the type, so the by_cases case-split
        /// that `wrapper_return` would otherwise force is
        /// unnecessary — backends emit a plain unfold + simp
        /// against arithmetic lemmas.
        lifted: bool,
    },
    /// Structural induction on a recursive ADT parameter.
    Induction { param: String },
    /// Library axiom instance — the law instantiates a named
    /// data-structure axiom (e.g. AverMap's `has_set_self` or
    /// `get_set_self`). Backends map the axiom name to their
    /// lemma vocabulary (Lean: `AverMap.has_set_self`; Dafny:
    /// its own set/lookup axioms; Z3: built-in array theory).
    /// Args carry the call-site expressions the axiom applies to.
    LibraryAxiom {
        /// Canonical axiom name. Recognised values today:
        /// `"Map.has_set_self"`, `"Map.get_set_self"`. Open string
        /// so future axioms (List, Set, Array, …) extend without
        /// enum churn.
        axiom: String,
        /// Arguments in the order the axiom expects. For Map
        /// axioms: `[m, k, v]` (the map, key, value the axiom
        /// reasons about).
        args: Vec<Spanned<crate::ir::hir::ResolvedExpr>>,
    },
    /// Post-condition of an inline-defined map-update fn. The outer
    /// fn `outer(m, k)` has body shape `let v = Map.get m k; match v
    /// { Some(_) -> Map.set m k _; None -> Map.set m k _ }` — i.e. it
    /// inspects the existing value and writes some new value at key
    /// `k` in every arm. The law asserts a post-condition on that
    /// update — `Map.has(outer(m, k), k) == true` (`HasAfter`), or
    /// `Map.get(outer(m, k), k) == Option.Some(...)` (`GetAfter`).
    ///
    /// Backends emit a 2-step proof: unfold the outer fn, case-split
    /// on `Map.get m k` (the same value `outer` inspected), apply the
    /// `Map.set`-axioms on each branch. Named after the law's
    /// algebraic content, not the Lean tactic.
    MapUpdatePostcondition {
        /// Source name of the outer update fn.
        outer_fn: String,
        /// Which post-condition the law asserts.
        kind: MapUpdatePostconditionKind,
        /// The map argument as it appears at the law's call site.
        map_arg: Spanned<crate::ir::hir::ResolvedExpr>,
        /// The key argument as it appears at the law's call site.
        key_arg: Spanned<crate::ir::hir::ResolvedExpr>,
        /// Additional helper-fn source names to unfold on top of
        /// `outer_fn` — only used for `GetAfter`, where the rhs's
        /// `Option.Some(...)` typically wraps the prior value via a
        /// pure user helper (e.g. `addOne(...)`). Source names;
        /// backends translate to their lemma vocabulary.
        extra_unfolds: Vec<String>,
    },
    /// Counter-increment specialisation of [`MapUpdatePostcondition`].
    /// The outer fn `outer(m, k)` is the canonical "tracked counter"
    /// shape:
    ///
    /// ```text
    /// let v = Map.get m k
    /// match v {
    ///   Some(n) -> Map.set m k (n + 1)
    ///   None    -> Map.set m k 1
    /// }
    /// ```
    ///
    /// The law states the algebraic content:
    /// `Option.withDefault(Map.get(outer(m, k), k), 0) ==
    /// Option.withDefault(Map.get(m, k), 0) + 1` — get-or-default
    /// after the increment equals the prior get-or-default plus 1.
    /// Tighter than [`MapUpdatePostcondition`] because both the body
    /// template AND the rhs `+ 1` shape are pinned.
    MapKeyTrackedIncrement {
        /// Source name of the outer increment fn.
        outer_fn: String,
        /// The map argument as it appears at the law's call site.
        map_arg: Spanned<crate::ir::hir::ResolvedExpr>,
        /// The key argument as it appears at the law's call site.
        key_arg: Spanned<crate::ir::hir::ResolvedExpr>,
    },
    /// Functional equivalence between an impl fn and a (declared)
    /// spec fn — the law states `impl(args) == spec(args)` and the
    /// two fn bodies are syntactically identical (after typecheck).
    /// Backends close the goal by unfolding both fns; their bodies
    /// reduce to the same term and the equality holds by reflexivity
    /// modulo simp normalisation. Lean emits `simpa [<unfolds>]`,
    /// Dafny would reveal both and let Z3 prove the equivalence.
    /// Named for the algebraic content (functional equivalence),
    /// not the backend tactic.
    SpecEquivalence {
        /// All user fn source names to unfold — impl + spec + any
        /// transitively-reached helpers from law sides. Source
        /// names; backends translate to their lemma vocabulary.
        extra_unfolds: Vec<String>,
    },
    /// Broader [`SpecEquivalence`] for cases where impl and spec
    /// bodies are NOT syntactically identical but normalize to the
    /// same expression under arg substitution + simp arithmetic
    /// identity folding (`a + 0 == a`, `a * 1 == a`, `a * 0 ==
    /// 0`). Backends close via `simp` (no `simpa` — there's no
    /// trivial-rfl goal to discharge; simp normalisation does the
    /// closing). Same `extra_unfolds` payload as `SpecEquivalence`.
    SpecEquivalenceSimpNormalized {
        /// All user fn source names to unfold — impl + spec + any
        /// transitively-reached helpers from law sides.
        extra_unfolds: Vec<String>,
    },
    /// Linear-Int spec equivalence — impl and spec bodies are both
    /// linear arithmetic expressions over Int givens (only
    /// `Literal::Int`, given idents, `Add`, `Sub`) after arg
    /// substitution. Bodies may differ syntactically but the
    /// equivalence is decidable by a linear-arithmetic solver
    /// (Presburger / `omega` / Z3 LIA). Backends emit a `change
    /// <impl_unfolded> = <spec_unfolded>` rewrite then close via
    /// their decision procedure; the IR carries the substituted
    /// expressions so the backend can render them via its own
    /// `emit_expr`.
    LinearIntSpecEquivalence {
        /// Impl body with formal params substituted by call-site
        /// args. Linear-arithmetic-only after substitution.
        unfolded_impl: Spanned<crate::ir::hir::ResolvedExpr>,
        /// Spec body with formal params substituted by call-site
        /// args. Linear-arithmetic-only after substitution.
        unfolded_spec: Spanned<crate::ir::hir::ResolvedExpr>,
    },
    /// Functional equivalence between an effectful impl fn and a
    /// spec fn. Same "claim states `impl(args) == spec(args)`"
    /// content as [`SpecEquivalence`], but the law's source-level
    /// shape is non-canonical (impl call usually omits oracle args
    /// the spec call carries explicitly). The lowerer runs an
    /// Oracle Lift over both sides — injecting oracle args from
    /// `given oracle: Random.int = ...` into every classified
    /// effectful call site — and matches the canonical shape on the
    /// rewritten form. Backends emit `simp [impl, spec]`; both
    /// definitions unfold to the same oracle call after lifting.
    EffectfulSpecEquivalence {
        /// Source name of the impl fn (= `vb.fn_name`).
        impl_fn: String,
        /// Source name of the spec fn (the other side of the law).
        spec_fn: String,
    },
    /// Second-order linear recurrence spec equivalence — impl is a
    /// tail-recursive Int linear-pair wrapper (e.g. `fib` dispatching
    /// on `n < 0` and calling a 3-arg `fibTR(n, 0, 1)` helper) and
    /// spec is a direct second-order recurrence (`match n { 0 -> b0;
    /// 1 -> b1; _ -> recurrence(spec(n-1), spec(n-2)) }`). The
    /// impl's helper implements the same affine recurrence as the
    /// spec's `_` arm. Both Lean and Dafny render via a Nat-keyed
    /// helper + shift lemma + helper-seed bridge; the algebraic
    /// content (a fixed-point of the recurrence) is the same in both
    /// targets but the syntactic proof template differs per backend.
    LinearRecurrence2SpecEquivalence {
        /// Source name of the impl (tail-recursive wrapper) fn.
        impl_fn: String,
        /// Source name of the spec (direct recurrence) fn.
        spec_fn: String,
        /// Source name of the worker fn called by `impl_fn`.
        helper_fn: String,
    },
    /// Bounded universal: case-split over the declared `given`
    /// domain, dispatch each case to a per-sample lemma.
    BoundedUniversal,
    /// Two `MatchDispatcherFold` fns over the same `List<T>` param
    /// compute the same result via slightly different but structurally
    /// identical match-on-list bodies. The law states
    /// `fold_fn(xs) == spec_fn(xs)`; the proof closes by induction on
    /// `xs` with both nil and cons cases reducing to arithmetic
    /// identities. Demonstrated by `examples/data/list_length_fold.av`
    /// (`lengthFwd(xs) = 1 + lengthFwd(t)` vs
    /// `lengthSwap(xs) = lengthSwap(t) + 1` — equivalent via
    /// commutativity, closes via `omega`).
    ///
    /// Stage 8c of #232 — third typed-pattern consumer in
    /// `proof_lower`.
    MatchDispatcherFold {
        /// Source name of the LHS fold fn (the law's surrounding fn).
        fold_fn: String,
        /// Source name of the RHS spec fn — also a list-fold of the
        /// same shape.
        spec_fn: String,
    },
    /// `?`-propagating Result chain equals a manual `match`-version:
    /// the law states `chain_qm(x) == chain_manual(x)` where the
    /// LHS uses `?` for short-circuit Err propagation and the RHS
    /// writes the same flow as nested `match Result.Err -> Err`
    /// arms. Both sides unfold to the same nested match; the proof
    /// closes by unfolding all step fns and case-splitting on each
    /// step's Result discriminator. Demonstrated by
    /// `examples/core/result_chain.av`. Stage 8b of #232.
    ResultPipelineChain {
        /// Source name of the `?`-chain fn (the wrapper). LHS of the law.
        chain_qm_fn: String,
        /// Source name of the manual `match`-chain fn. RHS of the law.
        chain_manual_fn: String,
        /// Source names of every step fn the two chains thread
        /// through, in pipeline order. Drives the unfold list for
        /// both backends.
        step_fns: Vec<String>,
    },
    /// Monoidal-accumulator wrapper-over-recursion: a non-recursive
    /// `wrapper_fn(xs) = inner_fn(xs, neutral)` paired with a direct-
    /// recurrence `other_fn` such that the law states
    /// `wrapper_fn(xs) == other_fn(xs)`. The inner fn has shape
    /// `match xs { [] -> acc; [h, ..t] -> inner_fn(t, acc <op> h) }`
    /// where `<op>` is monoidal (`Add` / `Mul` / `Sub` on Int) with
    /// known neutral element. Strategy emits an aux accumulator-
    /// decomposition lemma plus the main universal lemma; Z3 closes
    /// both via list induction. Demonstrated by `examples/data/sum_acc.av`.
    ///
    /// Stage 8 of #232 — first ProofStrategy variant that consumes
    /// a `ModulePattern` from `analysis::shape`.
    WrapperOverRecursion {
        /// Source name of the non-recursive wrapper (e.g. `"sum"`).
        wrapper_fn: String,
        /// Source name of the self-recursive inner (e.g. `"sumTR"`).
        inner_fn: String,
        /// Source name of the direct-recurrence fn the law compares
        /// the wrapper against (e.g. `"sumDirect"`).
        other_fn: String,
        /// Binary op the inner threads through its accumulator
        /// (`Add` / `Mul` / `Sub`). Drives the aux lemma's RHS.
        combine_op: crate::ast::BinOp,
        /// Driver of the inner recursion: a `List<_>` structural fold
        /// (the additive `sum_acc` shape) or a Peano-`Nat` countdown
        /// (`factTR`). Selects the backend's induction skeleton and the
        /// closing tactic family.
        driver: WrapperDriver,
        /// For a Peano-`Nat` fold whose step is a named monoid fn
        /// (`mul(n, acc)` / `plus(n, acc)`), the combine fn's source
        /// name. `None` for an inline-binop `List` fold (`acc + h`).
        combine_fn: Option<String>,
    },
    /// Ground constant-fold over fixed ADT/enum constructor
    /// arguments. The law's call(s) pin every non-Int param of the
    /// verified fn to a constructor literal (`CellContent.Empty`,
    /// `Color.Black`); any scalar `given`s are quantified but irrelevant
    /// to the chosen branch (the constructor selects a fixed arm). The
    /// verified fn and its transitively-reached callees are
    /// non-recursive, so the whole call tree folds to a closed term and
    /// the goal becomes a decidable ground equality. Backends unfold the
    /// fn + callees (the same `unfold_fns` list the LinearArithmetic
    /// detector builds) and close with a `split`/`rfl`/`decide` cascade.
    /// Demonstrated by `examples/games/checkers/ai.av`
    /// (`centerBonus.emptyNeutral`, `pieceValue.antisymmetry`,
    /// `pieceValue.kingWorthTripleMan`). Named for the algebraic content
    /// (constant-folding over a fixed constructor), not the Lean tactic.
    EnumConstantFold {
        /// Ordered fn unfold list — top-level law fn first, then
        /// transitively-reached non-recursive callees. Source names;
        /// backends translate to their lemma vocabulary.
        unfold_fns: Vec<String>,
    },
    /// Closed finite-domain enumeration over the law's givens. Every
    /// given ranges over a closed, small domain — `Bool` or a
    /// user-declared enum whose constructors are ALL fieldless — with
    /// the product of domain sizes ≤ 16, so exhaustive `cases` over
    /// the givens yields ground goals that compute out (`rfl` /
    /// `decide`). Fuel-wrapped callees are NOT an obstacle:
    /// constant-measure constructor args compute through fuel. The
    /// detector deliberately has NO call-shape inspection, NO
    /// return-type gate and NO recursion gate — closed enumeration
    /// makes those irrelevant, which is why this is a NEW strategy and
    /// not a relaxation of [`ProofStrategy::EnumConstantFold`], whose
    /// literal-pinning / non-recursive / scalar-return gates are
    /// load-bearing for its simp cascade. Motivating shapes:
    /// `examples/data/json.av` `parseLiteral.boolRoundtrip` (closes
    /// genuinely with `intro b; cases b <;> rfl`) and the `EscapeCode`
    /// laws (`escapeJsonChar.encodesEscapeCode`,
    /// `parseEscape.escapeCodeRoundtrip`). A non-closing leaf degrades
    /// to an honest caught `sorry` — never a build error and never
    /// `native_decide`.
    FiniteDomainCases {
        /// Law given names in intro order — the Lean emitter's
        /// `cases` targets. Source names; backends translate.
        givens: Vec<String>,
    },
    /// Builtin-roundtrip simp over the prelude's spec-lemma registry —
    /// the last typed fallback before `BackendDispatch`, and the only
    /// strategy the Lean backend renders AFTER its legacy ad-hoc chain
    /// (so it fires precisely where the sampled-sorry fallback used to
    /// emit a bare-`sorry` universal). A no-when law whose lhs call cone
    /// reduces to builtin String/Int operations once the user fns
    /// unfold: the Lean emit is `intro <givens>; first | (simp [<unfold
    /// set>, <registry lemmas>, Int.add_sub_cancel]; done) | sorry`. The
    /// `done` + `first | … | sorry` alternation is the honest floor — a
    /// simp that fails OR leaves a residual goal degrades to a caught
    /// `sorry`, NEVER a build error and NEVER `native_decide`.
    /// Motivating shapes: `examples/data/json.av`
    /// `finishInt.fromCanonicalInt` (closes via
    /// `Int.fromString_fromInt`), `finishNumber.fromCanonicalIntSlice` /
    /// `afterIntChar.terminatedIntRoundtrip` (slice-prefix lemmas
    /// through the `toString` fuel wrapper) and
    /// `finishString.plainSegmentRoundtrip` (`String.slice_append_prefix`
    /// + `String.intercalate_singleton`).
    ///
    /// Deliberately a NEW variant and not a reuse of
    /// [`ProofStrategy::SimpOverLemmas`]: that variant is the discovery
    /// feedback loop's re-pin channel (`lemma_discovery::committed`
    /// re-pins an `Induction` law when committed *discovered* lemma
    /// texts are in scope, and the backend routes it through the
    /// induction emit with embedded lemma bodies). This strategy carries
    /// no lemma texts and never inducts — it names *static prelude*
    /// lemmas that the Lean emitter ships demand-driven (see
    /// `lean::prelude_spec_lemmas_for_builtins`, the single source of
    /// truth for the builtin → lemma-name registry). Keeping the two
    /// apart means neither the discovery CLI nor `committed.rs` ever
    /// has to reason about this variant.
    SimpOverPreludeLemmas {
        /// Ordered fn unfold list — law subject fn first, then the
        /// transitively-reached NON-recursive callees (sorted).
        /// Source names; backends translate.
        unfold_fns: Vec<String>,
        /// Recursive (fuel-emitted) fns called DIRECTLY in the law lhs
        /// with measure-closed args (constructor-headed over
        /// scalar-only payloads, or literals) — the fuel value
        /// computes to a Nat literal, so simp drives the `__fuel`
        /// equations through. The Lean emitter expands each name to
        /// wrapper + `<name>__fuel` + its measure-helper names.
        /// Recursive fns reached only transitively (inside cone
        /// bodies) are NOT listed: they stay opaque — usually dead
        /// branches under the law's pinned literal args, and if live
        /// the simp falls to the honest caught `sorry`.
        fuel_fns: Vec<String>,
        /// Builtin call names observed in the law sides + cone bodies
        /// (sorted; includes the synthetic `String.concat` marker for
        /// string `+`). Registry keys — the Lean emitter maps them to
        /// prelude spec lemma names via
        /// `prelude_spec_lemmas_for_builtins`.
        builtins: Vec<String>,
    },
    /// Decimal-Int parse/serialize roundtrip over the canonical
    /// single-scanner decimal parser shape: the law states
    /// `parse(ser(C(n)), 0) = Ok(C(n), String.len(ser(C(n))))` for an
    /// unconstrained `given n: Int`, where `parse` dispatches the head
    /// char (`"-"` → sign path, `"0"` → leading-zero scan, `_` → digit
    /// path), both paths funnel into ONE recognized fuelized
    /// string-position scanner (`proof_recognize::detect_string_pos_scan`
    /// — the same gate that makes the Lean backend synthesize the
    /// scanner's `<fn>__fuel_scan` companion lemma), and the cone
    /// bottoms out in `String.fromInt` / `Int.fromString`.
    ///
    /// The detector validates the ENTIRE canonical shape (arm literals,
    /// arm order, scanner pins, finish-fn slice + `Int.fromString`
    /// leaf), so the Lean emission can render the fixed
    /// sign-split proof skeleton ported from the verified json hand
    /// proof: serializer reduces by `rfl` (ADT-measure fuel),
    /// `rcases Int.ofNat | Int.negSucc`, head-char dispatch via
    /// `String.mk`-form `rfl`, `split` + `digitChar` contradiction
    /// lemmas, the synthesized scan lemma, and `Int.fromString_fromInt`
    /// at the `finish_int_fn` leaf. The whole emission is wrapped in
    /// `first | (… ; done) | sorry` — a non-closing case degrades to a
    /// caught honest `sorry`, never a build error, and `native_decide`
    /// never appears. Dafny treats the pin as `BackendDispatch`
    /// (exports byte-identical).
    ///
    /// Demonstrated by `examples/data/json.av`
    /// `parseNumber.fromIntRoundtrip` — the first universal close
    /// through the fuel-unfolding barrier on a string whose length is
    /// symbolic.
    IntDecimalRoundtrip {
        /// Subject parser fn (`parseNumber`). Source names throughout.
        parse_fn: String,
        /// `"-"`-arm continuation (`parseNumberSign`).
        neg_fn: String,
        /// Wildcard-arm digit dispatcher (`startNumberDigits`).
        pos_fn: String,
        /// Sign path's digit dispatcher (`startSignDigit`).
        sign_fn: String,
        /// The recognized fuelized scanner (`scanIntTail`).
        scanner_fn: String,
        /// The scanner's char-class predicate (`isDigit`).
        predicate_fn: String,
        /// Scanner exit continuation (`finishNumber`).
        finish_fn: String,
        /// Int leaf — slices + `Int.fromString` (`finishInt`).
        finish_int_fn: String,
        /// Serializer the law's lhs feeds the parser (`toString`).
        serializer_fn: String,
    },
    /// String escape/parse roundtrip over the canonical
    /// segment-chunking string scanner: the law states
    /// `parse(<open> + escape(s) + <terminator>, 1) =
    /// Ok(StrCtor(s), String.len(escape(s)) + 2)` for an unconstrained
    /// `given s: String` (or the same claim entered at the scanner
    /// itself with `pos = segmentStart = 1, chunks = []`). The
    /// producer is a per-char classifier fold (two-char escape table +
    /// hex control escapes + printable passthrough); the consumer is a
    /// fuel mutual SCC (scan / escape-dispatch / validate / unicode
    /// chain) whose per-arm shapes the detector validates EXACTLY —
    /// see [`StringEscapeRoundtripPin`] for every captured name and
    /// literal, and `proof_lower::string_escape_roundtrip` for the
    /// gates.
    ///
    /// The Lean emission renders the suffix-invariant proof skeleton
    /// ported from the verified json hand proof (kernel-checked on
    /// Lean 4.15, #print axioms = [propext, Quot.sound]): a
    /// drop-form suffix-cursor prelude, the producer fold's
    /// accumulator homomorphism, one step lemma per consumer fuel
    /// arm, and a chunk invariant with the carried scanner state
    /// (segmentStart, chunks) universally quantified, closed by
    /// per-char classification. Every synthesized lemma carries a
    /// `first | (…; done) | sorry` floor — a template regression
    /// degrades to caught honest sorries (loud budget red), never a
    /// build error, and `native_decide` never appears. Dafny treats
    /// the pin as `BackendDispatch` (exports byte-identical).
    ///
    /// Demonstrated by `examples/data/json.av`
    /// `escapeJsonString.parseStringRoundtrip` and
    /// `parseStringChunk.escapedStringRoundtrip` — the parser
    /// workhorse pair that closes json's pinned Lean budget to 0.
    StringEscapeRoundtrip(Box<StringEscapeRoundtripPin>),
    /// Unconditional ring identity over Int-component records — the
    /// algebra-law family of an exact-rationals library (a record
    /// with Int numerator/denominator fields, non-normalizing
    /// arithmetic, equality by cross-multiplication): add/mul
    /// commutativity and associativity, distributivity, neg/sub
    /// normal forms, identity elements. The law has no `when`, every
    /// given is `Int` or a record whose fields are ALL `Int` (at
    /// least one such record given), and the claim's whole unfold
    /// cone is non-recursive pure arithmetic — record constructions
    /// / field projections, Int literals, `+`, `-`, `*`, unary
    /// negation — with the equality bottoming out in Int `==`
    /// (a Bool comparator fn applied at the law root, compared to
    /// `true`) or direct value equality of two such arithmetic
    /// expressions. Both sides are then polynomial identities:
    /// distributing products over sums and AC-normalizing monomials
    /// and sums makes the two sides' monomial multisets identical,
    /// no coefficient collection needed.
    ///
    /// The Lean emit is `intro <givens>; first | (simp [<unfold
    /// cone>, <fixed core AC-ring lemma package>]; done) | sorry` —
    /// the honest caught-`sorry` floor; never `native_decide`, never
    /// a build error. The package is SCOPED TO THIS STRATEGY's
    /// emission: its permutational rewrites (`Int.mul_comm`,
    /// `Int.add_comm`, …) loop or destroy the normal forms other
    /// strategies' simp sets rely on, so they are never added to the
    /// shared prelude registry. Dafny needs no special handling —
    /// Z3 decides these nonlinear identities push-button — and
    /// treats the pin like `BackendDispatch` (exports stay
    /// byte-identical). Demonstrated by `examples/data/rational.av`.
    RingIdentity {
        /// Ordered fn unfold list — law subject fn first, then the
        /// transitively-reached callees (sorted). Source names;
        /// backends translate to their lemma vocabulary.
        unfold_fns: Vec<String>,
    },
    /// Floor-division window family — laws over a power-of-two fn
    /// (`match n <= 0 { true -> 1; false -> 2 * pow(n - 1) }`), a
    /// floor-halving binary-exponent fn (the
    /// [`RecursionContract::WellFoundedToNat`] class with divisor 2),
    /// and the scaled-significand / bit-width window predicates built
    /// from them. Each [`FloorWindowFigure`] is a fully-validated
    /// shape with a fixed proof template on both backends (Lean: the
    /// core `Int.le_ediv_iff_mul_le` / `Int.ediv_lt_iff_lt_mul`
    /// floor bridges + power algebra by functional induction; Dafny:
    /// a proved division-window prelude + branch-split helper
    /// lemmas). The recognizers are deliberately narrow — exactly the
    /// hand-validated figures; everything else declines and keeps
    /// the prior emission.
    FloorDivWindow { figure: FloorWindowFigure },
    /// No automated strategy — emit with `sorry` (Lean) / `assume
    /// {:axiom}` (Dafny). User fills in manually.
    Sorry,
    /// Lowerer has not pinned a strategy for this law; the backend's
    /// `or_else` chain decides. Today reached by linear-recurrence-
    /// spec equivalence (Lean-specific, ~50-line support theorems
    /// stay in the backend) and the sampled / guarded-domain
    /// fallback. The backend treats `BackendDispatch` as "fall
    /// through to ad-hoc strategy chain"; pinned variants above
    /// short-circuit to a known emit.
    BackendDispatch,
}

/// The recognized figures of [`ProofStrategy::FloorDivWindow`]. All
/// fn names are source names; backends translate. Every figure's
/// quantifier names come from the law's givens (captured implicitly —
/// backends render them through the law's own given list).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FloorWindowFigure {
    /// `pow(n) >= 1 => true` with no premise — positivity of the
    /// power-of-two fn, by functional induction.
    PowPositive { pow_fn: String },
    /// `when m >= 0; n >= 0 -> pow(m + n) == pow(m) * pow(n)` — the
    /// power homomorphism, by functional induction on the first
    /// exponent.
    PowSumSplit { pow_fn: String },
    /// `when b >= 1; a >= b; n >= 1 -> window(a, b, n) == true`
    /// where `window` checks `pow(n-1) <= sig(a,b,n) < pow(n)`,
    /// `sig` scales by `pow(n-1-e)` and floor-divides, and `e` is
    /// the floor-halving binary exponent of a/b.
    SigWindow {
        pow_fn: String,
        halve_fn: String,
        exp_fn: String,
        sig_fn: String,
        window_fn: String,
    },
    /// `when fits(j, m); fits(k, n) -> claim(j, k, m, n) == true`
    /// where `fits` is the `pow(m-1) <= j < pow(m)` window predicate
    /// and `claim` states the product window
    /// `pow(m+n-2) <= j*k < pow(m+n)`.
    ProductWindow {
        pow_fn: String,
        fits_fn: String,
        claim_fn: String,
    },
}

/// Parameter pack for [`ProofStrategy::StringEscapeRoundtrip`] —
/// every fn name and literal the Lean renderer's suffix-invariant
/// proof skeleton quotes. All fn names are source names (backends
/// translate); all chars/codes are the SOURCE literals the detector
/// read off the validated arm patterns, so the renderer can rebuild
/// them as Lean literals without re-walking the AST.
#[derive(Debug, Clone)]
pub struct StringEscapeRoundtripPin {
    /// The scanner SCC member the law enters (`parseStringChunk`).
    /// Body: charAt dispatch over { terminator → finish, escape-char
    /// → escape dispatch, default → validate }.
    pub scan_fn: String,
    /// Escape dispatcher (`parseEscape`): slices the open segment,
    /// then maps escape letters to decoded chars / the unicode hop.
    pub escape_fn: String,
    /// Default-arm validator (`validateChar`): control chars error,
    /// printable chars extend the open segment.
    pub validate_fn: String,
    /// Terminator continuation (`finishString`): slice + join + Ok.
    pub finish_fn: String,
    /// `\uXXXX` reader head (`parseUnicode`): readHex4 + codepoint.
    pub unicode_fn: String,
    /// Codepoint surrogate filter (`parseUnicodeCodePoint`).
    pub codepoint_fn: String,
    /// Decoded-codepoint continuation (`applyCodePoint`):
    /// `Char.fromCode` + chunk flush back into the scanner.
    pub apply_fn: String,
    /// Four-hex-digit reader (`readHex4`), separately fueled on
    /// `count` climbing to the literal bound 4.
    pub read_hex_fn: String,
    /// User hex-digit valuation (`hexVal : String -> Option<Int>`).
    pub hex_val_fn: String,
    /// High-surrogate guard (`isHighSurrogate`): `cp >= MIN && …`.
    pub high_surrogate_fn: String,
    /// Low-surrogate guard (`isLowSurrogate`).
    pub low_surrogate_fn: String,
    /// Producer wrapper (`escapeJsonString`): `fold(String.chars(s), "")`.
    pub producer_fn: String,
    /// Producer accumulator fold (`escapeJsonChars`).
    pub fold_fn: String,
    /// Per-char classifier (`escapeJsonChar`): two-char escape table
    /// + default to the control classifier.
    pub classifier_fn: String,
    /// Control classifier (`escapeControlChar`): equality ladder +
    /// `code < threshold → control escape` + printable passthrough.
    pub control_fn: String,
    /// Hex control escape (`controlCodeEscape`): `Byte.toHex` +
    /// 4-char prefix.
    pub control_escape_fn: String,
    /// Success ctor of the law's rhs, source spelling
    /// (`"ParseResult.Ok"`).
    pub ok_ctor: String,
    /// String-payload ctor inside the success ctor
    /// (`"Json.JsonString"`).
    pub str_ctor: String,
    /// Scan terminator char (`'"'` — the finish arm's literal).
    pub terminator: char,
    /// Escape introducer char (`'\\'` — the escape arm's literal,
    /// also the first char of every two-char escape output).
    pub escape_char: char,
    /// Hex-escape letter (`'u'` — second char of the control-escape
    /// prefix, the consumer's unicode arm literal).
    pub unicode_letter: char,
    /// Two-char escape table, producer-derived and consumer-aligned.
    pub pairs: Vec<EscapePairSpec>,
    /// Control threshold (`32`): producer hex-escapes below it, the
    /// consumer validator rejects below it. Gated `<= 256`.
    pub control_threshold: i64,
    /// `cp >= MIN` bound of the high-surrogate guard (`55296`).
    pub high_surrogate_min: i64,
    /// `cp >= MIN` bound of the low-surrogate guard (`56320`).
    /// The Lean renderer probes the scanner SCC's emitted
    /// `averStringPosFuel` rank itself (it must match the emission
    /// byte-for-byte), so the rank is deliberately NOT pinned here.
    pub low_surrogate_min: i64,
}

/// One two-char escape: the producer emits `[escape_char, letter]`
/// for `decoded`; the consumer's escape dispatcher maps `letter`
/// back to `decoded`.
#[derive(Debug, Clone)]
pub struct EscapePairSpec {
    /// The unescaped source char (`'\n'`).
    pub decoded: char,
    /// The escape letter following the escape introducer (`'n'`).
    pub letter: char,
    /// `true` when the pair comes from the control classifier's
    /// equality ladder (`code == 8 → "\\b"`), `false` for a
    /// classifier literal arm (`"\n" → "\\n"`). Drives which
    /// disequality form the chunk-invariant ladder cases on
    /// (`c.toNat = K` vs `c = '<lit>'`).
    pub from_control_ladder: bool,
}

/// Discriminator for [`ProofStrategy::MapUpdatePostcondition`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapUpdatePostconditionKind {
    /// Law shape: `Map.has(outer(m, k), k) == true`.
    HasAfter,
    /// Law shape: `Map.get(outer(m, k), k) == Option.Some(...)`.
    GetAfter,
}

/// A bool predicate with explicit free-variable context. Stays in
/// `Spanned<Expr>` form so backends can route through their
/// existing `emit_expr` paths; the context is what gives backends
/// the information they need to project (e.g. `.val`) without
/// re-walking the AST.
#[derive(Debug, Clone)]
pub struct Predicate {
    /// Variables the predicate may reference, in declaration order.
    /// Each entry tells the backend what type the var has in the
    /// target language — same logic as `Quantifier.binder_type`.
    pub free_vars: Vec<(String, QuantifierType)>,
    /// The expression. Already in the target variable space (e.g.
    /// caller-derived predicates have had caller-arg names
    /// substituted to callee-param names).
    pub expr: Spanned<crate::ir::hir::ResolvedExpr>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::hir::ResolvedExpr;
    use crate::ir::interval::{Interval, OpClass};

    /// A throwaway invariant predicate — `raw_i64_eligible` never reads
    /// it (it derives only from the persisted `interval` / `op_classes`),
    /// so a trivial `n` stub is enough to build a `RefinedTypeDecl`.
    fn stub_predicate() -> Predicate {
        Predicate {
            free_vars: vec![("n".to_string(), QuantifierType::Plain("Int".to_string()))],
            expr: Spanned::new(ResolvedExpr::Ident("n".to_string()), 0),
        }
    }

    /// Build a `RefinedTypeDecl` directly from the two persisted facts
    /// the recognizer reads, bypassing the pipeline entirely.
    fn decl(interval: Option<Interval>, ops: Vec<(&str, OpClass)>) -> RefinedTypeDecl {
        RefinedTypeDecl {
            name: "T".to_string(),
            carrier_type: "Int".to_string(),
            carrier_field: "value".to_string(),
            predicate_param: "n".to_string(),
            invariant: stub_predicate(),
            witness: Some("0".to_string()),
            interval,
            op_classes: ops.into_iter().map(|(n, c)| (n.to_string(), c)).collect(),
        }
    }

    #[test]
    fn two_sided_fits_i64_all_overflow_free_is_eligible() {
        // The IntRange shape: [0,100], single `add` op OverflowFree.
        let d = decl(
            Some(Interval::between(0, 100)),
            vec![("add", OpClass::OverflowFree)],
        );
        assert!(d.raw_i64_eligible());
    }

    #[test]
    fn one_overflow_free_one_unbounded_is_not_eligible() {
        // ALL ops must be OverflowFree — a single Unbounded op demotes.
        let d = decl(
            Some(Interval::between(0, 100)),
            vec![
                ("add", OpClass::OverflowFree),
                ("scaledAdd", OpClass::Unbounded),
            ],
        );
        assert!(!d.raw_i64_eligible());
    }

    #[test]
    fn one_needs_wider_scratch_is_not_eligible() {
        let d = decl(
            Some(Interval::between(0, 100)),
            vec![("widePath", OpClass::NeedsWiderScratch)],
        );
        assert!(!d.raw_i64_eligible());
    }

    #[test]
    fn two_sided_interval_not_fitting_i64_is_not_eligible() {
        // [0, i64::MAX + 1] is two-sided and finite but exceeds i64, so
        // the carrier could not be stored in a machine word.
        let d = decl(
            Some(Interval::between(0, i64::MAX as i128 + 1)),
            vec![("add", OpClass::OverflowFree)],
        );
        assert!(!d.raw_i64_eligible());
    }

    #[test]
    fn declined_interval_none_is_not_eligible() {
        // `interval: None` is the analysis's conservative decline (an
        // unrecognized invariant shape) — never eligible.
        let d = decl(None, vec![("add", OpClass::OverflowFree)]);
        assert!(!d.raw_i64_eligible());
    }

    #[test]
    fn one_sided_interval_is_not_eligible() {
        // A `Natural`-shaped [0, +inf]: `fits_i64` is false because the
        // upper bound is open, so the type is rejected even with no ops.
        let d = decl(Some(Interval::ge(0)), vec![]);
        assert!(!d.raw_i64_eligible());
    }

    #[test]
    fn two_sided_fits_i64_empty_ops_is_eligible() {
        // Empty op_classes: a finite-i64 interval with no carrier-reading
        // arithmetic. Decision: ELIGIBLE — storage fits i64 and the
        // all-OverflowFree check over an empty op set is vacuously true,
        // so there is no op that could wrap a raw i64. See the doc-comment
        // on `raw_i64_eligible` for the full reasoning.
        let d = decl(Some(Interval::between(0, 100)), vec![]);
        assert!(d.raw_i64_eligible());
    }
}
