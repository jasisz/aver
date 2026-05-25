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
    /// lowers to (native / fuel / structural / linear recurrence).
    pub fn_contracts: HashMap<String, FnContract>,
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

/// Algebraic / proof-theoretic shape of a verify-law theorem.
///
/// **Naming rule**: variants describe **what the law says**, not
/// **how a backend proves it**. The IR is target-agnostic — Lean
/// maps `Commutative { op: Add }` to `simp [fn, Int.add_comm]`,
/// Dafny maps the same variant to its own lemma vocabulary, a Z3
/// backend could ship a different tactic again. Tactic names
/// (`SimpOverLemmas`, `simp+omega`) do not appear in variant names;
/// `LinearArithmetic` is named for the semantic, not the tactic.
#[derive(Debug, Clone)]
pub enum ProofStrategy {
    /// `rfl` / definitional equality — `lhs ≡ rhs` syntactically.
    Reflexive,
    /// `simp` chain over named lemmas (e.g. `[Int.add_comm,
    /// Int.mul_comm]`). Legacy draft variant; not yet emitted by
    /// the lowerer — kept for future use when a strategy wants to
    /// hand the backend a specific lemma list.
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
        args: Vec<Spanned<crate::ast::Expr>>,
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
        map_arg: Spanned<crate::ast::Expr>,
        /// The key argument as it appears at the law's call site.
        key_arg: Spanned<crate::ast::Expr>,
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
        map_arg: Spanned<crate::ast::Expr>,
        /// The key argument as it appears at the law's call site.
        key_arg: Spanned<crate::ast::Expr>,
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
        unfolded_impl: Spanned<crate::ast::Expr>,
        /// Spec body with formal params substituted by call-site
        /// args. Linear-arithmetic-only after substitution.
        unfolded_spec: Spanned<crate::ast::Expr>,
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
    pub expr: Spanned<crate::ast::Expr>,
}
