//! Proof intermediate representation.
//!
//! Single decision substrate the Lean and Dafny proof exporters
//! consume. Backends render text from a fully-resolved `ProofIR` —
//! they do not classify shapes, do not derive contracts, do not
//! decide between native and fuel emit. Every decision happens once
//! in the `proof_lower` pipeline stage; both backends see the same
//! decision and either render it consistently or fail consistently.
//!
//! The architectural goal: replace the ad-hoc "guess and emit"
//! pattern that grew across `src/codegen/{common,recursion,lean,
//! dafny}` during 0.22.0 with a single typed model. Each variant
//! that says "emit native" or "lift to subtype" carries inside its
//! payload everything the backend needs and everything the
//! classifier proved — the type system makes it impossible to
//! produce a "native" decision without also producing the side-
//! conditions that justify it.
//!
//! **Status**: skeleton. Step 1 (this file) defines the types. Step
//! 2 wires `proof_lower` to populate `ProofIR.refined_types` for
//! refinement-via-opaque records; backends still go through the old
//! `codegen::common::refinement_info_for` path and tests verify
//! both paths produce equivalent decisions. Steps 3+ migrate one
//! backend at a time, then extend coverage to recursion contracts
//! and law theorems.

use std::collections::HashMap;

use crate::ast::Spanned;

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
    /// Every refinement-lifted user type, keyed by canonical type
    /// name (`Module.Natural` or bare `Natural` when no enclosing
    /// module). Includes types declared in the entry items and in
    /// dependent modules — the lowerer normalises both into the
    /// same map so backends don't have to walk two corpora.
    pub refined_types: HashMap<String, RefinedTypeDecl>,
    /// Per-pure-fn contract describing what proof artifact the fn
    /// lowers to. Currently a stub (empty); Step 5+ populates it
    /// when migrating the `RecursionPlan` machinery.
    pub fn_contracts: HashMap<String, FnContract>,
    /// Per-verify-law theorem decomposed into quantifiers, premises,
    /// and claim with all wrapper-strip / val-projection / drop-vs-
    /// keep decisions baked in. Currently empty; populated when
    /// migrating the law emit path.
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
    pub predicate: Spanned<crate::ast::Expr>,
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
}

/// Per-pure-fn proof contract. Placeholder shape — Step 5+ fills
/// this in with recursion plan migration.
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
        /// caller-derived precondition" — the lowerer leaves the
        /// fibTR-style default (`param ≥ 0`) synthesis to the
        /// backend for now; Step 6+ moves that into the lowerer.
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
    pub base_arm_body: Spanned<crate::ast::Expr>,
    /// AST for the wildcard arm's body — the recursive call site.
    pub wildcard_arm_body: Spanned<crate::ast::Expr>,
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
        bound: Spanned<crate::ast::Expr>,
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
    pub fn_name: String,
    pub law_name: String,
    pub quantifiers: Vec<Quantifier>,
    /// Premises in order. Already includes `when` if it carries
    /// information beyond the refinement invariants (the lowerer
    /// performs the bijective syntactic equivalence check).
    pub premises: Vec<Predicate>,
    /// LHS = RHS claim. Wrapper-stripped, lifted-var-aware (bare
    /// idents for arg positions, `.val` projections inside
    /// comparator BinOps if the lowerer determined this is needed).
    pub claim_lhs: Spanned<crate::ast::Expr>,
    pub claim_rhs: Spanned<crate::ast::Expr>,
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

/// Auto-proof strategy the lowerer chose for the universal theorem.
/// Backends translate to their tactic vocabulary (Lean: `simp;
/// omega`, Dafny: empty body, etc.).
#[derive(Debug, Clone)]
pub enum ProofStrategy {
    /// `rfl` / definitional equality.
    Reflexive,
    /// `simp` chain over named lemmas (e.g. `[Int.add_comm,
    /// Int.mul_comm]`).
    SimpOverLemmas(Vec<String>),
    /// Commutative law over a 2-arg Int-Int wrapper:
    /// `wrapper(a, b) => wrapper(b, a)`. Backend renders as
    /// `simp [<wrapper_name>, <op-comm-lemma>]` where the lemma
    /// is derived from `op` (`Add → Int.add_comm`, `Mul → Int.
    /// mul_comm`). Op stays in IR so backends pick their own
    /// lemma vocabulary (Dafny would use a different incantation).
    WrapperCommutative { op: crate::ast::BinOp },
    /// Associative law over the same shape:
    /// `wrapper(wrapper(a,b),c) => wrapper(a,wrapper(b,c))`.
    WrapperAssociative { op: crate::ast::BinOp },
    /// Identity-element law (`wrapper(a, 0) => a` for `Add`,
    /// `wrapper(a, 1) => a` for `Mul`). The identity literal is
    /// determined by `op` — backends compute it directly.
    WrapperIdentity { op: crate::ast::BinOp },
    /// Right-identity over a 2-arg Sub wrapper: `sub(a, 0) => a`.
    /// Sub-specific because subtraction's identity is one-sided —
    /// the symmetric `0 - a` is `-a`, not `a`, so it doesn't fit
    /// `WrapperIdentity`.
    WrapperSubRightIdentity,
    /// Anti-commutative law over a 2-arg Sub wrapper:
    /// `sub(a, b) => -sub(b, a)` or the swapped arrangement.
    /// `neg_on_rhs` records which side carries the negation —
    /// drives the `.symm` flip on backends that prove via
    /// `Int.neg_sub`.
    WrapperSubAntiCommutative {
        /// `true` for `sub(a, b) => -sub(b, a)`, `false` for the
        /// swapped form `-sub(b, a) => sub(a, b)`.
        neg_on_rhs: bool,
    },
    /// Equivalence between a unary wrapper and a binary wrapper
    /// over the same op, e.g. `fn addOne(a) -> a + 1` plus
    /// `verify addOne law identityViaAdd ... addOne(a) => add(a, 1)`.
    /// The IR captures the inner binary fn name so the backend
    /// renders `simp [<outer>, <inner>]` without re-scanning the
    /// AST for the equivalent.
    WrapperUnaryEquivalence {
        /// Source-level name of the inner binary wrapper the unary
        /// fn equals (the law's "other side" calls this).
        inner_fn: String,
    },
    /// `simp + omega` over an unfolded fn chain — the generic
    /// catch-all for Int laws whose lhs/rhs reduce to flat linear
    /// arithmetic once every reachable fn is unfolded. Triggers when
    /// every given is `Int` and the transitive fn-call closure of
    /// the law's two sides is non-recursive. The backend emits a
    /// `simp only [<unfold_fns>] <;> omega` chain, optionally
    /// wrapped in `by_cases` when a refinement smart constructor
    /// sits in the chain (its guard goes on top so omega doesn't
    /// face an `Except.ok` / `Except.err` split).
    SimpOmegaUnfold {
        /// Ordered fn unfold list. Top-level law fn comes first —
        /// Lean's `unfold` resolves left-to-right and the call
        /// layer the tactic peels at each step must match the goal
        /// shape.
        unfold_fns: Vec<String>,
        /// `true` when at least one fn in `unfold_fns` returns a
        /// wrapper (Result, Option, …). Drives the by_cases branch
        /// in the emit — `omega` is a linear-arithmetic decision
        /// procedure that can't close constructor-equality goals,
        /// so the wrapper case splits on the smart-constructor
        /// guard predicate first.
        wrapper_return: bool,
        /// Smart-constructor guard pulled from the first refinement
        /// `fromX(p: Int) -> Result<X, _>` in the unfold chain.
        /// `Some` when one was found; `None` falls back to the
        /// conservative `(n ≥ 0)` predicate on the wrapper-return
        /// path. The pair carries the smart constructor's parameter
        /// name (so the law-quantified var can be substituted in)
        /// and the Bool subject of the constructor's `match`.
        smart_guard: Option<SmartGuard>,
    },
    /// Structural induction on a recursive ADT parameter.
    Induction { param: String },
    /// Bounded universal: case-split over the declared `given`
    /// domain, dispatch each case to a per-sample lemma.
    BoundedUniversal,
    /// No automated strategy — emit with `sorry` (Lean) / `assume
    /// {:axiom}` (Dafny). User fills in manually.
    Sorry,
    /// Lowerer has not pinned a strategy yet; the backend's
    /// existing `or_else` chain decides. Placeholder during the
    /// Step 23+ migration — every law theorem starts here, and
    /// subsequent Steps move concrete strategies (Reflexive,
    /// Induction, …) into the lowerer one shape at a time. The
    /// backend treats `BackendDispatch` as "fall through to ad-hoc
    /// strategy chain", same behaviour as pre-migration.
    BackendDispatch,
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
    pub expr: Spanned<crate::ast::Expr>,
}
