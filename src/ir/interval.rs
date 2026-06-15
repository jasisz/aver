//! Per-module interval analysis over refinement-type carriers.
//!
//! This is a **pure, read-only diagnostic**. It derives, for each
//! refinement-via-opaque type the proof side already lifted into
//! [`crate::ir::proof_ir::RefinedTypeDecl`], a constant integer
//! interval `[lo, hi]` that over-approximates the type's invariant,
//! and then classifies every arithmetic operation the defining module
//! exposes on that type as one of [`OpClass::OverflowFree`],
//! [`OpClass::NeedsWiderScratch`], or [`OpClass::Unbounded`].
//!
//! Nothing here changes codegen, the runtime, or proof output. The
//! result is surfaced only through `aver compile --explain-passes`
//! and stored on [`crate::ir::pipeline::PipelineResult`] for future
//! opt-in consumers (the carrier-lowering recognizer is a later
//! slice). See `prompts/int-semantics-refinement-perf-optin.md` for
//! the why.
//!
//! ## Soundness direction
//!
//! The dangerous mistake is wrongly certifying an operation as
//! [`OpClass::OverflowFree`] — that would let a future codegen lower
//! an intermediate to a raw `i64` that can actually wrap. So the
//! analysis is conservative in exactly one direction: when any input
//! shape is unrecognized, or any operand is unbounded, it **declines**
//! (`Unbounded` / `interval_known = false`). It never over-claims a
//! bound it cannot derive.
//!
//! The interval carrier is `i128` internally so the analysis can
//! represent and reason about values *outside* `i64` without itself
//! wrapping — this is what lets it prove `a + b` for `a, b ∈ [0, 100]`
//! stays `≤ 200 < i64::MAX`. All `i128` arithmetic **saturates** to
//! ±infinity on overflow; it never wraps.

use std::collections::HashMap;

use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, Spanned, Stmt};
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::ir::TypeId;
use crate::ir::proof_ir::{Predicate, RefinedTypeDecl};

/// One endpoint of an [`Interval`]. `Finite(k)` is an exact `i128`
/// bound; `NegInf` / `PosInf` are the open ends the saturating
/// arithmetic produces when a value escapes the `i128` range or when
/// the source invariant is one-sided (e.g. `Natural`'s `n >= 0`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Bound {
    NegInf,
    Finite(i128),
    PosInf,
}

impl Bound {
    /// `true` for a `Finite` bound that fits the `i64` range.
    fn fits_i64(self) -> bool {
        match self {
            Bound::Finite(k) => k >= i64::MIN as i128 && k <= i64::MAX as i128,
            Bound::NegInf | Bound::PosInf => false,
        }
    }

    /// Saturating addition over the extended integers. Any `inf`
    /// dominates; two `Finite` bounds add in `i128` and saturate to
    /// the matching infinity on overflow (never wrap).
    fn add(self, other: Bound) -> Bound {
        match (self, other) {
            // ∞ + (-∞) cannot arise for a well-formed interval (a
            // `lo` is never `PosInf` and a `hi` is never `NegInf`),
            // but guard it anyway: collapse to the conservative open
            // end so we never fabricate a finite bound.
            (Bound::PosInf, Bound::NegInf) | (Bound::NegInf, Bound::PosInf) => Bound::NegInf,
            (Bound::PosInf, _) | (_, Bound::PosInf) => Bound::PosInf,
            (Bound::NegInf, _) | (_, Bound::NegInf) => Bound::NegInf,
            (Bound::Finite(a), Bound::Finite(b)) => match a.checked_add(b) {
                Some(s) => Bound::Finite(s),
                None if a > 0 => Bound::PosInf,
                None => Bound::NegInf,
            },
        }
    }

    /// Negate an endpoint. `Finite(i128::MIN)` cannot be negated in
    /// `i128`, so it saturates to `PosInf` rather than wrapping.
    fn neg(self) -> Bound {
        match self {
            Bound::NegInf => Bound::PosInf,
            Bound::PosInf => Bound::NegInf,
            Bound::Finite(k) => match k.checked_neg() {
                Some(n) => Bound::Finite(n),
                None => Bound::PosInf,
            },
        }
    }

    /// Saturating multiplication of two endpoints. The sign of an
    /// infinity is determined by the sign of the finite factor.
    fn mul(self, other: Bound) -> Bound {
        // Resolve the sign and magnitude of each factor.
        let sign = |b: Bound| -> i32 {
            match b {
                Bound::NegInf => -1,
                Bound::PosInf => 1,
                Bound::Finite(0) => 0,
                Bound::Finite(k) => {
                    if k > 0 {
                        1
                    } else {
                        -1
                    }
                }
            }
        };
        // 0 * ∞ is treated as 0: a `Finite(0)` factor pins the
        // product to 0 regardless of the other endpoint. This is the
        // correct interval-arithmetic answer because a 0 endpoint
        // only arises from a `Finite(0)` operand, never from an open
        // end (an open end has no finite witness to multiply).
        if matches!(self, Bound::Finite(0)) || matches!(other, Bound::Finite(0)) {
            return Bound::Finite(0);
        }
        match (self, other) {
            (Bound::Finite(a), Bound::Finite(b)) => match a.checked_mul(b) {
                Some(p) => Bound::Finite(p),
                None => {
                    if sign(self) * sign(other) >= 0 {
                        Bound::PosInf
                    } else {
                        Bound::NegInf
                    }
                }
            },
            _ => {
                if sign(self) * sign(other) >= 0 {
                    Bound::PosInf
                } else {
                    Bound::NegInf
                }
            }
        }
    }

    /// Order for picking a `min` lower bound: `NegInf < Finite < PosInf`.
    pub fn min(self, other: Bound) -> Bound {
        if self.le(other) { self } else { other }
    }

    /// Order for picking a `max` upper bound.
    pub fn max(self, other: Bound) -> Bound {
        if self.le(other) { other } else { self }
    }

    /// `self <= other` over the extended integers.
    fn le(self, other: Bound) -> bool {
        match (self, other) {
            (Bound::NegInf, _) => true,
            (_, Bound::NegInf) => false,
            (_, Bound::PosInf) => true,
            (Bound::PosInf, _) => false,
            (Bound::Finite(a), Bound::Finite(b)) => a <= b,
        }
    }
}

/// A constant integer interval `[lo, hi]` over the extended integers.
/// Produced by [`interval_of_invariant`] from a refinement predicate
/// and propagated bottom-up through an operation body by the abstract
/// interpreter in [`classify_op`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Interval {
    pub lo: Bound,
    pub hi: Bound,
}

impl Interval {
    /// The whole extended integer line — the conservative "I know
    /// nothing" answer the analysis returns whenever it declines.
    pub fn unbounded() -> Interval {
        Interval {
            lo: Bound::NegInf,
            hi: Bound::PosInf,
        }
    }

    /// A single point `[k, k]`.
    pub fn point(k: i128) -> Interval {
        Interval {
            lo: Bound::Finite(k),
            hi: Bound::Finite(k),
        }
    }

    /// `[k, +inf]` — the over-approximation of `n >= k`.
    pub fn ge(k: i128) -> Interval {
        Interval {
            lo: Bound::Finite(k),
            hi: Bound::PosInf,
        }
    }

    /// `[-inf, k]` — the over-approximation of `n <= k`.
    pub fn le(k: i128) -> Interval {
        Interval {
            lo: Bound::NegInf,
            hi: Bound::Finite(k),
        }
    }

    /// `[lo, hi]` from two literal bounds.
    pub fn between(lo: i128, hi: i128) -> Interval {
        Interval {
            lo: Bound::Finite(lo),
            hi: Bound::Finite(hi),
        }
    }

    /// Intersection — used to combine the conjuncts of a compound
    /// `Bool.and` guard into a single two-sided interval.
    pub fn intersect(self, other: Interval) -> Interval {
        Interval {
            lo: self.lo.max(other.lo),
            hi: self.hi.min(other.hi),
        }
    }

    /// Convex hull (worst-case widening join) — the merge operator for
    /// control-flow joins and the per-value `worst` accumulator the
    /// general range pass uses. If either operand escapes `i64`, so does
    /// the hull, keeping the headroom verdict sound. Public mirror of the
    /// crate-private `join` free fn so the bare-`i64` consumer (outside
    /// this module) can widen without re-deriving the rule.
    pub fn hull(self, other: Interval) -> Interval {
        Interval {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }

    /// `true` when both bounds are finite and within the `i64` range.
    /// This is the headroom test: an intermediate whose interval
    /// `fits_i64` cannot overflow a raw `i64` before the smart
    /// constructor's guard re-validates it.
    pub fn fits_i64(self) -> bool {
        self.lo.fits_i64() && self.hi.fits_i64()
    }

    /// `true` when neither bound is infinite (the interval is a real
    /// finite range, even if wider than `i64`).
    fn is_finite(self) -> bool {
        matches!(self.lo, Bound::Finite(_)) && matches!(self.hi, Bound::Finite(_))
    }

    /// Saturating interval addition. (Inherent, not `std::ops::Add`:
    /// the operation saturates rather than panicking, so the std trait
    /// would carry the wrong contract.)
    #[allow(clippy::should_implement_trait)]
    pub fn add(self, other: Interval) -> Interval {
        Interval {
            lo: self.lo.add(other.lo),
            hi: self.hi.add(other.hi),
        }
    }

    /// Saturating interval subtraction (`a - b = a + (-b)`, with the
    /// endpoints flipped so `lo` stays the lower bound).
    #[allow(clippy::should_implement_trait)]
    pub fn sub(self, other: Interval) -> Interval {
        Interval {
            lo: self.lo.add(other.hi.neg()),
            hi: self.hi.add(other.lo.neg()),
        }
    }

    /// Saturating interval multiplication. The product range is the
    /// min/max over the four endpoint products, which is the standard
    /// sound interval-arithmetic rule and handles negative operands.
    #[allow(clippy::should_implement_trait)]
    pub fn mul(self, other: Interval) -> Interval {
        let products = [
            self.lo.mul(other.lo),
            self.lo.mul(other.hi),
            self.hi.mul(other.lo),
            self.hi.mul(other.hi),
        ];
        let mut lo = products[0];
        let mut hi = products[0];
        for p in &products[1..] {
            lo = lo.min(*p);
            hi = hi.max(*p);
        }
        Interval { lo, hi }
    }
}

/// Classification of an operation's worst-case `i64` intermediate.
///
/// **`OverflowFree` means EVERY `i64` intermediate across the WHOLE op
/// body provably fits `i64` — the smart-constructor guard (`fromInt` /
/// `fromX`) is STILL REQUIRED.** The verdict is the join over the final
/// tail interval, every intermediate subexpression interval, and every
/// earlier binding's interval (see [`classify_op`]): a single
/// out-of-`i64` intermediate anywhere — even one whose value never
/// reaches the tail — pulls the class out of `OverflowFree`. It does
/// NOT mean "the result is in range without the guard": the result of
/// e.g. `IntRange.add([0,100], [0,100]) = [0,200]` fits `i64` but
/// exceeds the type's `[0,100]` bound, so `fromInt` must still run to
/// re-validate the invariant. A future codegen recognizer that lowers
/// the arithmetic to raw `i64` on the strength of this class must keep
/// the `fromInt` call. Dropping it reintroduces the model-vs-runtime
/// gap this whole mechanism exists to close.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpClass {
    /// The arithmetic intermediate provably fits `i64`, so it can run
    /// on raw `i64` without wrapping before the guard re-validates.
    OverflowFree,
    /// The intermediate exceeds `i64` but is a proven-finite range, so
    /// a future codegen could compute it in a wider scratch type
    /// (`i128` or bignum) and then narrow through the guard. None of
    /// the current example types reach this band; it exists so the
    /// classifier is honest about the middle case rather than
    /// collapsing it into `Unbounded`.
    NeedsWiderScratch,
    /// The intermediate has no derivable finite bound — typically
    /// because an operand is a one-sided refinement (`[0, +inf]`) or a
    /// plain `Int` (unbounded by construction). The honest decline:
    /// the analysis cannot certify the operation as native-`i64`-safe.
    Unbounded,
}

impl OpClass {
    /// Classify an arithmetic intermediate interval. Conservative:
    /// only a fully-`i64`-fitting interval earns `OverflowFree`.
    pub fn of_interval(i: Interval) -> OpClass {
        if i.fits_i64() {
            OpClass::OverflowFree
        } else if i.is_finite() {
            OpClass::NeedsWiderScratch
        } else {
            OpClass::Unbounded
        }
    }

    /// Stable lowercase label for diagnostics / JSON.
    pub fn label(self) -> &'static str {
        match self {
            OpClass::OverflowFree => "overflow_free",
            OpClass::NeedsWiderScratch => "needs_wider_scratch",
            OpClass::Unbounded => "unbounded",
        }
    }
}

/// The raw-`i64`-carrier recognizer, factored to a single place so the
/// `--explain-passes` diagnostic ([`RefinedTypeInterval::raw_i64_eligible`])
/// and the persisted-fact gate
/// ([`crate::ir::proof_ir::RefinedTypeDecl::raw_i64_eligible`]) can never
/// disagree about which types are eligible.
///
/// Returns `true` IFF a refined type may have a raw `i64` carrier:
///
/// - `interval` is `Some` — a recognized enclosure (`None` is the
///   analysis's conservative decline); AND
/// - that interval [`Interval::fits_i64`] — both bounds finite AND within
///   `[i64::MIN, i64::MAX]`, which is exactly "two-sided and machine-word
///   sized" (a one-sided / open bound is `±inf`, which never fits); AND
/// - every op in `ops` is [`OpClass::OverflowFree`] — a single
///   `NeedsWiderScratch` / `Unbounded` op means some carrier arithmetic
///   can wrap a raw `i64` before the guard re-validates.
///
/// Conservative in the soundness-critical direction: a wrongly-`true`
/// answer would license a later codegen to lower a carrier whose ops can
/// wrap. An **empty** `ops` slice with a finite-`i64` interval is
/// eligible — storage fits `i64` and the `all(...)` is vacuously true, so
/// nothing can overflow. See the `RefinedTypeDecl` method doc for the
/// full empty-op reasoning.
///
/// WHITELIST SEMANTICS — load-bearing for any consumer that lowers a
/// carrier to `i64`. `ops` is the set of carrier-*arithmetic* ops the
/// analysis examined (a fn taking the refined type whose own body computes
/// an arithmetic intermediate over the carrier). It is NOT every op the
/// type exposes: an op whose overflow risk lives inside a *called helper*
/// — e.g. `fromInt(widen(r.value))` with no arithmetic operator in its own
/// body — is intentionally not enumerated. So `true` does NOT assert
/// "every operation on the type is overflow-free". It asserts only: the
/// carrier may be *stored* as `i64`, and the enumerated `OverflowFree` ops
/// may compute on it with direct `i64` arithmetic. Any consumer that
/// lowers the carrier to `i64` MUST therefore convert every OTHER read of
/// the carrier (helper calls, projections, unenumerated ops) to a bignum
/// `Int` — never raw-`i64` arithmetic outside the enumerated ops. That is
/// the carrier-lowering contract the bignum runtime must honour; it is
/// what keeps an unenumerated helper-call op sound (its `r.value` is read
/// as a bignum `Int`, so the helper's arithmetic cannot wrap).
pub fn raw_i64_eligible<'a>(
    interval: Option<Interval>,
    ops: impl IntoIterator<Item = &'a OpClass>,
) -> bool {
    let Some(interval) = interval else {
        return false;
    };
    if !interval.fits_i64() {
        return false;
    }
    ops.into_iter().all(|c| *c == OpClass::OverflowFree)
}

/// Per-refined-type interval analysis result.
#[derive(Debug, Clone)]
pub struct RefinedTypeInterval {
    /// Opaque type identity — the same key the type carries in
    /// `ProofIR.refined_types`, so two same-named types in different
    /// modules stay distinct.
    pub type_id: TypeId,
    /// Source-level type name (for diagnostics only; not a key).
    pub name: String,
    /// The derived constant interval over-approximating the invariant.
    /// `Interval::unbounded()` when the shape was unrecognized.
    pub interval: Interval,
    /// `true` when [`interval_of_invariant`] recognized the invariant
    /// shape and the interval is a real (non-trivial) enclosure;
    /// `false` when it declined (the interval is `unbounded()`).
    pub interval_known: bool,
    /// Per-op classification, in module-walk order. Each entry pairs
    /// the operation's source name with its [`OpClass`].
    pub ops: Vec<(String, OpClass)>,
}

impl RefinedTypeInterval {
    /// Whether this type may have a raw `i64` carrier — the diagnostic
    /// mirror of the persisted-fact gate on `RefinedTypeDecl`. Delegates
    /// to the shared [`raw_i64_eligible`] so the two paths agree by
    /// construction. The persisted decl stores `interval: None` for a
    /// declined invariant, so this passes `Some(self.interval)` only when
    /// `interval_known` to match that exact representation.
    pub fn raw_i64_eligible(&self) -> bool {
        let interval = self.interval_known.then_some(self.interval);
        raw_i64_eligible(interval, self.ops.iter().map(|(_, c)| c))
    }
}

/// Whole-analysis result, keyed for cheap programmatic lookup.
#[derive(Debug, Clone, Default)]
pub struct IntervalAnalysisResult {
    /// One entry per refined type the analysis saw, keyed by opaque
    /// `TypeId`.
    pub types: HashMap<TypeId, RefinedTypeInterval>,
}

impl IntervalAnalysisResult {
    /// Number of types analyzed.
    pub fn types_analyzed(&self) -> usize {
        self.types.len()
    }

    /// Types whose invariant yielded a two-sided constant interval
    /// (both bounds finite) — the carrier-lowering candidates.
    pub fn two_sided_bounded(&self) -> usize {
        self.types
            .values()
            .filter(|t| t.interval_known && t.interval.is_finite())
            .count()
    }

    /// Total ops across all types classified `OverflowFree`.
    pub fn ops_overflow_free(&self) -> usize {
        self.count_ops(OpClass::OverflowFree)
    }

    /// Total ops across all types classified `NeedsWiderScratch`.
    pub fn ops_needs_wider(&self) -> usize {
        self.count_ops(OpClass::NeedsWiderScratch)
    }

    /// Total ops across all types classified `Unbounded`.
    pub fn ops_unbounded(&self) -> usize {
        self.count_ops(OpClass::Unbounded)
    }

    /// Refined types whose carrier may lower to a raw `i64`
    /// ([`RefinedTypeInterval::raw_i64_eligible`]) — the
    /// recognizer's headline count, surfaced by `--explain-passes`. This
    /// is the "proof the recognizer fires on the right types" metric;
    /// nothing in codegen / runtime / proof consumes it.
    pub fn raw_i64_eligible(&self) -> usize {
        self.types.values().filter(|t| t.raw_i64_eligible()).count()
    }

    fn count_ops(&self, class: OpClass) -> usize {
        self.types
            .values()
            .flat_map(|t| t.ops.iter())
            .filter(|(_, c)| *c == class)
            .count()
    }
}

/// Derive a constant interval from a refinement invariant.
///
/// Returns `(interval, interval_known)`. Recognizes exactly the
/// comparison / `Bool.and` shapes the refinement examples produce; any
/// other shape (`Bool.or`, a bare identifier, a structural carrier,
/// non-literal bounds) yields `(Interval::unbounded(), false)` — the
/// conservative decline.
///
/// The free variable matched against the bound is taken from
/// `pred.free_vars[0]` (the smart constructor's parameter). Operand-
/// flipped comparisons (`k <= n` as well as `n >= k`) are normalized.
pub fn interval_of_invariant(pred: &Predicate) -> (Interval, bool) {
    let Some((var, _)) = pred.free_vars.first() else {
        return (Interval::unbounded(), false);
    };
    match interval_of_resolved(&pred.expr, var) {
        Some(i) => (i, true),
        None => (Interval::unbounded(), false),
    }
}

/// Recognize a single resolved predicate expression as an interval
/// over `var`. `None` = unrecognized shape (caller declines).
fn interval_of_resolved(
    expr: &Spanned<crate::ir::hir::ResolvedExpr>,
    var: &str,
) -> Option<Interval> {
    use crate::ir::hir::{ResolvedCallee, ResolvedExpr};
    match &expr.node {
        // Compound guard `Bool.and(l, r)` → intersect both sides.
        // Recurses, so deeply-nested conjunctions also collapse.
        ResolvedExpr::Call(ResolvedCallee::Builtin(name), args)
            if name == "Bool.and" && args.len() == 2 =>
        {
            let l = interval_of_resolved(&args[0], var)?;
            let r = interval_of_resolved(&args[1], var)?;
            Some(l.intersect(r))
        }
        // A comparison between the predicate variable and a literal.
        ResolvedExpr::BinOp(op, lhs, rhs) => interval_of_comparison(*op, lhs, rhs, var),
        // Anything else (Bool.or, Bool.not, arbitrary predicate, the
        // bare variable) is not a recognized interval shape.
        _ => None,
    }
}

/// Recognize `var <op> k` (or the operand-flipped `k <op> var`) as an
/// interval. Only `>`, `>=`, `<`, `<=` against an integer literal
/// produce a bound; `==` / `!=` and non-literal operands decline.
fn interval_of_comparison(
    op: BinOp,
    lhs: &Spanned<crate::ir::hir::ResolvedExpr>,
    rhs: &Spanned<crate::ir::hir::ResolvedExpr>,
    var: &str,
) -> Option<Interval> {
    // Identify which side is the variable and which is the literal,
    // normalizing the operator if the operands are flipped.
    let (op, k) = if is_var(lhs, var) {
        (op, int_literal(rhs)?)
    } else if is_var(rhs, var) {
        (flip_comparison(op)?, int_literal(lhs)?)
    } else {
        return None;
    };
    let k = k as i128;
    match op {
        BinOp::Gte => Some(Interval::ge(k)),    // n >= k  → [k, +inf]
        BinOp::Gt => Some(Interval::ge(k + 1)), // n >  k  → [k+1, +inf]
        BinOp::Lte => Some(Interval::le(k)),    // n <= k  → [-inf, k]
        BinOp::Lt => Some(Interval::le(k - 1)), // n <  k  → [-inf, k-1]
        _ => None,
    }
}

/// `true` when the resolved expression is a reference to `var`
/// (whether it survived as a bare `Ident` or carries a resolved slot).
fn is_var(expr: &Spanned<crate::ir::hir::ResolvedExpr>, var: &str) -> bool {
    use crate::ir::hir::ResolvedExpr;
    match &expr.node {
        ResolvedExpr::Ident(name) => name == var,
        ResolvedExpr::Resolved { name, .. } => name == var,
        _ => false,
    }
}

/// Extract an integer literal from a resolved leaf, if it is one.
fn int_literal(expr: &Spanned<crate::ir::hir::ResolvedExpr>) -> Option<i64> {
    use crate::ir::hir::ResolvedExpr;
    match &expr.node {
        ResolvedExpr::Literal(Literal::Int(k)) => Some(*k),
        ResolvedExpr::Neg(inner) => match &inner.node {
            ResolvedExpr::Literal(Literal::Int(k)) => Some(-*k),
            _ => None,
        },
        _ => None,
    }
}

/// Operator with operands swapped: `a < b` ⇔ `b > a`, etc.
fn flip_comparison(op: BinOp) -> Option<BinOp> {
    match op {
        BinOp::Lt => Some(BinOp::Gt),
        BinOp::Gt => Some(BinOp::Lt),
        BinOp::Lte => Some(BinOp::Gte),
        BinOp::Gte => Some(BinOp::Lte),
        _ => None,
    }
}

/// Classify one operation function over a refined type.
///
/// `op_fn` is the raw-AST fn def the defining module exposes (e.g.
/// `IntRange.add`). `carrier_interval` is the interval of the refined
/// type's carrier. `refined_type_name` is the source name of that
/// type so the analyzer can tell a refined-typed parameter apart from
/// a plain `Int` one.
///
/// The body is abstractly interpreted bottom-up over [`Interval`]: a
/// `param.value` access on a refined-typed parameter contributes
/// `carrier_interval`; an integer literal contributes a point; `+` /
/// `-` / `*` combine sub-intervals via saturating interval arithmetic;
/// the smart-constructor call (`fromInt(inner)`, where the callee is
/// the type's actual constructor `constructor_fn`) is transparent — the
/// classified intermediate is the argument feeding the guard. ANY OTHER
/// call (a user helper, a builtin, an unknown callee), a plain-`Int`
/// operand entering the arithmetic, or any unrecognized shape evaluates
/// to `Interval::unbounded()` — the conservative decline.
///
/// The verdict is NOT the tail interval alone. Every binding in the
/// op body and every intermediate subexpression node within an
/// expression joins its own interval into a worst-case accumulator;
/// the returned [`OpClass`] is `of_interval` over the JOIN of the tail
/// interval and every intermediate/binding interval. So an out-of-`i64`
/// (or unbounded) value computed in an earlier statement, or buried in
/// a tail subexpression that later cancels out, still demotes the op
/// below `OverflowFree`.
pub fn classify_op(
    op_fn: &FnDef,
    carrier_interval: Interval,
    refined_type_name: &str,
    carrier_field: &str,
    constructor_fn: &str,
) -> OpClass {
    // Map each parameter name to whether it carries the refined type.
    let mut refined_params: HashMap<&str, bool> = HashMap::new();
    for (pname, ptype) in &op_fn.params {
        refined_params.insert(pname.as_str(), ptype == refined_type_name);
    }
    let ctx = OpCtx {
        carrier_interval,
        carrier_field,
        refined_params: &refined_params,
        constructor_fn,
    };

    // Worst-case (least-headroom) interval seen anywhere in the body.
    // Starts at a point so that a body with no recognized arithmetic
    // intermediate stays neutral; every binding and the tail join into
    // it. `worst` is the running join the evaluator widens at each
    // arithmetic / call / negation node.
    let mut worst = Interval::point(0);

    // Walk every statement in execution order. Each binding's value
    // expression and the tail expression are evaluated; both their
    // final intervals and every intermediate node they contain land in
    // `worst`.
    let FnBody::Block(stmts) = op_fn.body.as_ref();
    for stmt in stmts {
        let value = match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => e,
        };
        let i = eval_expr(value, &ctx, &mut worst);
        worst = join(worst, i);
    }

    OpClass::of_interval(worst)
}

/// Read-only context threaded through the op-body abstract evaluator.
struct OpCtx<'a> {
    carrier_interval: Interval,
    carrier_field: &'a str,
    refined_params: &'a HashMap<&'a str, bool>,
    /// The refined type's actual smart constructor name (`"fromInt"`).
    /// ONLY a one-arg call to this exact callee is peeled transparently;
    /// every other call evaluates to `Interval::unbounded()`.
    constructor_fn: &'a str,
}

/// Convex hull of two intervals — the "worst case includes both"
/// join. Widening to the union keeps the headroom verdict sound: if
/// either operand escapes `i64`, so does the join.
fn join(a: Interval, b: Interval) -> Interval {
    Interval {
        lo: a.lo.min(b.lo),
        hi: a.hi.max(b.hi),
    }
}

/// Abstractly evaluate a raw-AST expression to its interval, joining
/// the result of every arithmetic / negation / constructor-call node
/// into `worst` so the caller can classify over the whole body rather
/// than just the final value. Unknown or unbounded shapes evaluate to
/// `Interval::unbounded()`, which propagates through the arithmetic and
/// forces an `Unbounded` class — the conservative direction.
fn eval_expr(expr: &Spanned<Expr>, ctx: &OpCtx<'_>, worst: &mut Interval) -> Interval {
    match &expr.node {
        Expr::Literal(Literal::Int(k)) => Interval::point(*k as i128),
        // `param.value` where `param` is a refined-typed parameter and
        // the field is the carrier field → the carrier interval. Any
        // other field access (or access on a non-refined param) is an
        // unknown integer. The object leaf may be a bare `Ident` (proof
        // mode, resolve off) or a `Resolved` slot (resolve on) — both
        // carry the param name.
        Expr::Attr(obj, field) => {
            if field == ctx.carrier_field
                && let Some(pname) = param_name(obj)
                && ctx.refined_params.get(pname).copied() == Some(true)
            {
                ctx.carrier_interval
            } else {
                Interval::unbounded()
            }
        }
        Expr::BinOp(op, lhs, rhs) => {
            let l = eval_expr(lhs, ctx, worst);
            let r = eval_expr(rhs, ctx, worst);
            let result = match op {
                BinOp::Add => l.add(r),
                BinOp::Sub => l.sub(r),
                BinOp::Mul => l.mul(r),
                // Division and comparisons don't feed the headroom
                // question we model; decline rather than guess.
                _ => Interval::unbounded(),
            };
            // This arithmetic node is itself an `i64` intermediate —
            // record it before it is folded into an enclosing op (which
            // may cancel it back into range, as in `(a + MAX) - MAX`).
            *worst = join(*worst, result);
            result
        }
        Expr::Neg(inner) => {
            let result = Interval::point(0).sub(eval_expr(inner, ctx, worst));
            *worst = join(*worst, result);
            result
        }
        // The smart constructor is transparent: the intermediate we
        // care about is the value handed to the guard, i.e. its
        // argument. ONLY the type's real constructor (`constructor_fn`)
        // is peeled — a one-arg helper like `widen(x)` must NOT be
        // treated as identity, or its widened return value would be
        // hidden. Every non-constructor call is opaque → unbounded.
        Expr::FnCall(callee, args)
            if args.len() == 1 && is_constructor_call(callee, ctx.constructor_fn) =>
        {
            eval_expr(&args[0], ctx, worst)
        }
        // A bare identifier of a refined param read without `.value`,
        // a plain-`Int` param, any non-constructor call, or any other
        // shape: unbounded.
        _ => Interval::unbounded(),
    }
}

/// `true` for a `FnCall` whose callee is a bare identifier naming the
/// refined type's actual smart constructor. A top-level fn name like
/// `fromInt` stays an `Ident` through the resolver (it has no local
/// slot), so checking `Ident` + name equality covers both proof mode
/// (resolve off) and the resolved pipeline. Module-qualified or
/// computed callees, and any other helper, are not the constructor.
fn is_constructor_call(callee: &Spanned<Expr>, constructor_fn: &str) -> bool {
    matches!(&callee.node, Expr::Ident(name) if name == constructor_fn)
}

/// Extract the parameter name a leaf refers to, whether it survived as
/// a bare `Ident` (resolve off) or was rewritten to a `Resolved` slot
/// (resolve on). `None` for any other expression.
fn param_name(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) => Some(name.as_str()),
        Expr::Resolved { name, .. } => Some(name.as_str()),
        _ => None,
    }
}

/// Run the interval analysis over every refined type in `refined`,
/// classifying each one's module-exposed arithmetic ops.
///
/// `refined` is `ProofIR.refined_types`, already keyed by opaque
/// `TypeId` and scoped per module by `populate_refined_types`. `inputs`
/// is the same [`ProofLowerInputs`] the proof-lower stage consumed, so
/// the op fns are looked up with the identical module-scoped discipline
/// (never bare-name matching).
pub fn analyze(
    refined: &HashMap<TypeId, RefinedTypeDecl>,
    inputs: &ProofLowerInputs<'_>,
) -> IntervalAnalysisResult {
    let mut result = IntervalAnalysisResult::default();
    let symbols = inputs.symbol_table;

    for (type_id, decl) in refined {
        let (interval, interval_known) = interval_of_invariant(&decl.invariant);

        // Find the module scope this type lives in by resolving its
        // opaque `TypeId` back through the symbol table, then walk that
        // scope's fns for arithmetic ops over the type. Same scoping
        // discipline as `populate_refined_types` — two same-named
        // types in different modules never share an op set.
        let scope = scope_of_type(*type_id, decl, inputs, symbols);

        // Recover this type's actual smart constructor name in the SAME
        // module scope, so the op-body evaluator only peels the real
        // `fromInt`-style wrapper transparently (never an arbitrary
        // one-arg helper). If the refinement shape can't be re-resolved
        // (it always can here — the type was lifted from it), there is
        // no trustworthy constructor to gate on, so every op declines.
        let constructor_fn = crate::codegen::common::refinement_info_for_in_scope(
            &decl.name,
            inputs,
            scope.as_deref(),
        )
        .map(|info| info.constructor_fn.to_string());

        let ops = classify_ops_in_scope(
            decl,
            interval,
            interval_known,
            constructor_fn.as_deref(),
            inputs.pure_fns_in_scope(scope.as_deref()),
        );

        result.types.insert(
            *type_id,
            RefinedTypeInterval {
                type_id: *type_id,
                name: decl.name.clone(),
                interval,
                interval_known,
                ops,
            },
        );
    }
    result
}

/// Resolve the module scope (`None` = entry, `Some(prefix)` = a dep
/// module) that declares the refined type with `type_id`. Matches the
/// `TypeKey` the symbol table holds for that id against the candidate
/// scopes, so the answer is keyed by opaque identity, never bare name.
fn scope_of_type(
    type_id: TypeId,
    decl: &RefinedTypeDecl,
    inputs: &ProofLowerInputs<'_>,
    symbols: &crate::ir::SymbolTable,
) -> Option<String> {
    for scope in inputs.scopes() {
        let key = match &scope {
            Some(prefix) => crate::ir::TypeKey::in_module(prefix.clone(), &decl.name),
            None => crate::ir::TypeKey::entry(&decl.name),
        };
        if symbols.type_id_of(&key) == Some(type_id) {
            return scope;
        }
    }
    None
}

/// Classify every arithmetic op a module exposes over the refined
/// type. An op qualifies when it takes at least one parameter of the
/// refined type; ops with no refined-typed parameter (the smart
/// constructor `fromInt(n: Int)`, the unwrapper `toInt(n: T) -> Int`)
/// are skipped — they aren't arithmetic over two carriers.
fn classify_ops_in_scope(
    decl: &RefinedTypeDecl,
    interval: Interval,
    interval_known: bool,
    constructor_fn: Option<&str>,
    fns: Vec<&FnDef>,
) -> Vec<(String, OpClass)> {
    let mut ops = Vec::new();
    for fd in fns {
        // The op must take the refined type as a parameter AND its
        // body must do carrier arithmetic (a `param.value` access).
        // `toInt` takes the refined type but just projects the
        // carrier — no arithmetic intermediate — so it never needs an
        // overflow verdict.
        let takes_refined = fd.params.iter().any(|(_, t)| t == &decl.name);
        if !takes_refined || !body_does_carrier_arithmetic(fd, &decl.carrier_field) {
            continue;
        }
        // When the carrier interval is unknown (declined invariant), or
        // we couldn't recover the smart constructor to gate the
        // transparent peel on, the op is necessarily `Unbounded` —
        // there's no derived bound to reason from, and without the
        // constructor name no call can be safely treated as identity.
        let class = match (interval_known, constructor_fn) {
            (true, Some(ctor)) => classify_op(fd, interval, &decl.name, &decl.carrier_field, ctor),
            _ => OpClass::Unbounded,
        };
        ops.push((fd.name.clone(), class));
    }
    ops
}

/// `true` when the fn body's tail expression contains a `BinOp`
/// (Add/Sub/Mul) anywhere — i.e. it computes an arithmetic
/// intermediate over the carrier rather than just projecting it.
fn body_does_carrier_arithmetic(fd: &FnDef, _carrier_field: &str) -> bool {
    let FnBody::Block(stmts) = fd.body.as_ref();
    stmts.iter().any(|s| match s {
        Stmt::Expr(e) | Stmt::Binding(_, _, e) => expr_has_arithmetic(e),
    })
}

/// Recursively scan for an arithmetic `BinOp` node.
fn expr_has_arithmetic(expr: &Spanned<Expr>) -> bool {
    match &expr.node {
        Expr::BinOp(BinOp::Add | BinOp::Sub | BinOp::Mul, _, _) => true,
        Expr::BinOp(_, l, r) => expr_has_arithmetic(l) || expr_has_arithmetic(r),
        Expr::FnCall(_, args) => args.iter().any(expr_has_arithmetic),
        Expr::Attr(o, _) => expr_has_arithmetic(o),
        Expr::Neg(i) | Expr::ErrorProp(i) => expr_has_arithmetic(i),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::SourceLine;
    use crate::ir::hir::ResolvedExpr;
    use crate::ir::proof_ir::QuantifierType;

    const LINE: SourceLine = 0;

    fn sp_r(node: ResolvedExpr) -> Spanned<ResolvedExpr> {
        Spanned::new(node, LINE)
    }

    fn ident_r(name: &str) -> Spanned<ResolvedExpr> {
        sp_r(ResolvedExpr::Ident(name.to_string()))
    }

    fn int_r(k: i64) -> Spanned<ResolvedExpr> {
        sp_r(ResolvedExpr::Literal(Literal::Int(k)))
    }

    fn cmp_r(
        op: BinOp,
        l: Spanned<ResolvedExpr>,
        r: Spanned<ResolvedExpr>,
    ) -> Spanned<ResolvedExpr> {
        sp_r(ResolvedExpr::BinOp(op, Box::new(l), Box::new(r)))
    }

    fn pred(expr: Spanned<ResolvedExpr>) -> Predicate {
        Predicate {
            free_vars: vec![("n".to_string(), QuantifierType::Plain("Int".to_string()))],
            expr,
        }
    }

    // ── Bound saturating arithmetic ─────────────────────────────────

    #[test]
    fn bound_posinf_plus_finite_is_posinf() {
        assert_eq!(Bound::PosInf.add(Bound::Finite(5)), Bound::PosInf);
        assert_eq!(Bound::Finite(5).add(Bound::PosInf), Bound::PosInf);
    }

    #[test]
    fn bound_finite_mul_exact_in_i128() {
        // 100 * 100 = 10_000 — exact, no saturation.
        assert_eq!(
            Bound::Finite(100).mul(Bound::Finite(100)),
            Bound::Finite(10_000)
        );
    }

    #[test]
    fn bound_i64_max_squared_is_exact_finite_not_i64() {
        // `(i64::MAX)^2 ≈ 2^126` fits i128 (max ≈ 2^127), so the
        // product is an EXACT finite i128 value — it does NOT wrap and
        // does NOT need to saturate. The soundness consequence: such a
        // product is finite-but-outside-i64, so the op that produced
        // it classifies `NeedsWiderScratch`, never `OverflowFree`.
        let m = Bound::Finite(i64::MAX as i128);
        let expected = (i64::MAX as i128) * (i64::MAX as i128);
        assert_eq!(m.mul(m), Bound::Finite(expected));
        assert!(!Bound::Finite(expected).fits_i64());
    }

    #[test]
    fn bound_i128_overflow_saturates_not_wraps() {
        // The keystone soundness unit test: a product that overflows
        // i128 itself MUST saturate to ±inf, never wrap to a small (or
        // negative) finite value that would fake headroom. `i128::MAX *
        // i128::MAX` is the canonical case.
        let big = Bound::Finite(i128::MAX);
        assert_eq!(big.mul(big), Bound::PosInf);
        // Opposite signs saturate to -inf.
        let neg = Bound::Finite(i128::MIN + 1);
        assert_eq!(big.mul(neg), Bound::NegInf);
        // Addition overflow saturates too.
        assert_eq!(big.add(Bound::Finite(1)), Bound::PosInf);
        assert_eq!(
            Bound::Finite(i128::MIN).add(Bound::Finite(-1)),
            Bound::NegInf
        );
    }

    #[test]
    fn bound_neg_min_saturates() {
        // i128::MIN cannot be negated in i128 → saturates to +inf.
        assert_eq!(Bound::Finite(i128::MIN).neg(), Bound::PosInf);
    }

    #[test]
    fn interval_add_keeps_finite_in_band() {
        let a = Interval::between(0, 100);
        let sum = a.add(a);
        assert_eq!(sum, Interval::between(0, 200));
        assert!(sum.fits_i64());
    }

    #[test]
    fn interval_mul_handles_negatives() {
        // [-2, 3] * [-2, 3] → min of {4,-6,-6,9} = -6, max = 9.
        let a = Interval::between(-2, 3);
        assert_eq!(a.mul(a), Interval::between(-6, 9));
    }

    // ── interval_of_invariant: 5 recognized shapes ─────────────────

    #[test]
    fn invariant_ge_natural() {
        // n >= 0  → [0, +inf]
        let (i, known) = interval_of_invariant(&pred(cmp_r(BinOp::Gte, ident_r("n"), int_r(0))));
        assert!(known);
        assert_eq!(i, Interval::ge(0));
    }

    #[test]
    fn invariant_gt_positive() {
        // n > 0  → [1, +inf]
        let (i, known) = interval_of_invariant(&pred(cmp_r(BinOp::Gt, ident_r("n"), int_r(0))));
        assert!(known);
        assert_eq!(i, Interval::ge(1));
    }

    #[test]
    fn invariant_lte() {
        // n <= 100  → [-inf, 100]
        let (i, known) = interval_of_invariant(&pred(cmp_r(BinOp::Lte, ident_r("n"), int_r(100))));
        assert!(known);
        assert_eq!(i, Interval::le(100));
    }

    #[test]
    fn invariant_lt() {
        // n < 10  → [-inf, 9]
        let (i, known) = interval_of_invariant(&pred(cmp_r(BinOp::Lt, ident_r("n"), int_r(10))));
        assert!(known);
        assert_eq!(i, Interval::le(9));
    }

    #[test]
    fn invariant_bool_and_intrange() {
        // Bool.and(n >= 0, n <= 100)  → [0, 100]
        use crate::ir::hir::ResolvedCallee;
        let and = sp_r(ResolvedExpr::Call(
            ResolvedCallee::Builtin("Bool.and".to_string()),
            vec![
                cmp_r(BinOp::Gte, ident_r("n"), int_r(0)),
                cmp_r(BinOp::Lte, ident_r("n"), int_r(100)),
            ],
        ));
        let (i, known) = interval_of_invariant(&pred(and));
        assert!(known);
        assert_eq!(i, Interval::between(0, 100));
    }

    #[test]
    fn invariant_operand_flipped() {
        // 0 <= n  (literal on the left) normalizes to n >= 0 → [0, +inf]
        let (i, known) = interval_of_invariant(&pred(cmp_r(BinOp::Lte, int_r(0), ident_r("n"))));
        assert!(known);
        assert_eq!(i, Interval::ge(0));
    }

    // ── interval_of_invariant: declined shapes ─────────────────────

    #[test]
    fn invariant_bool_or_declines() {
        use crate::ir::hir::ResolvedCallee;
        let or = sp_r(ResolvedExpr::Call(
            ResolvedCallee::Builtin("Bool.or".to_string()),
            vec![
                cmp_r(BinOp::Gte, ident_r("n"), int_r(0)),
                cmp_r(BinOp::Lte, ident_r("n"), int_r(100)),
            ],
        ));
        let (i, known) = interval_of_invariant(&pred(or));
        assert!(!known);
        assert_eq!(i, Interval::unbounded());
    }

    #[test]
    fn invariant_bare_ident_declines() {
        let (i, known) = interval_of_invariant(&pred(ident_r("n")));
        assert!(!known);
        assert_eq!(i, Interval::unbounded());
    }

    #[test]
    fn invariant_non_literal_bound_declines() {
        // n >= m  (m is another variable, not a literal) → decline.
        let (i, known) =
            interval_of_invariant(&pred(cmp_r(BinOp::Gte, ident_r("n"), ident_r("m"))));
        assert!(!known);
        assert_eq!(i, Interval::unbounded());
    }

    // ── fits_i64 boundary ──────────────────────────────────────────

    #[test]
    fn fits_i64_at_boundary() {
        assert!(Interval::between(0, i64::MAX as i128).fits_i64());
        // One past i64::MAX no longer fits → NeedsWiderScratch band.
        let over = Interval::between(0, i64::MAX as i128 + 1);
        assert!(!over.fits_i64());
        assert!(over.is_finite());
        assert_eq!(OpClass::of_interval(over), OpClass::NeedsWiderScratch);
    }

    #[test]
    fn opclass_overflow_free_vs_unbounded() {
        assert_eq!(
            OpClass::of_interval(Interval::between(0, 200)),
            OpClass::OverflowFree
        );
        // [0, +inf] (Natural) is NOT overflow-free.
        assert_eq!(OpClass::of_interval(Interval::ge(0)), OpClass::Unbounded);
    }
}
