//! Int "unboxing": pick a bare `i64` representation for provably-bounded,
//! non-escaping `Int` values so the Rust backend emits native integer
//! arithmetic instead of the default arbitrary-precision `aver_rt::AverInt`.
//!
//! This is a **read-only codegen analysis** — it never mutates the
//! `MirProgram`. It produces a [`BareI64Facts`] table the Rust walker reads
//! to select `i64` vs `AverInt` at each emit site, exactly the way the Rust
//! walker reads `aliased_slots` (the `own_param` pass) to select owned vs
//! borrowed collection params.
//!
//! ## What reuses the #511 interval domain
//!
//! The arithmetic-bound half reuses [`crate::ir::interval`] verbatim: the
//! `Interval` lattice element (i128-saturating, never wraps), its
//! `add`/`sub`/`mul`/`hull`/`fits_i64`, the `OpClass` verdict band, and the
//! `raw_i64_eligible` gate. We only swap the LEAF source — instead of a
//! refined-type carrier read, the leaf is an SSA-value (`LocalId`) range
//! query over the `MirFn` body.
//!
//! ## What reuses the existing escape / use-flow machinery
//!
//! The escape half mirrors the NOTION the `own_param` pass already uses (a
//! single use-flow scan over the body, defaulting every unrecognized
//! position to "escapes"), but with a REPRESENTATION-escape predicate
//! ("does this Int reach a general-Int context") rather than the ownership-
//! escape predicate ("does this collection leave the frame"). The call
//! graph used for the cross-frame summary is the `MirCallee::Fn` /
//! `MirTailCall` edge set, the same edges `own_param` walks.
//!
//! ## Soundness — fail-closed (the C0 guard)
//!
//! A value is `Bare` ONLY when PROVEN `raw_i64_eligible` (interval `Some`,
//! `fits_i64`, every participating op `OverflowFree`) AND non-escaping. Any
//! missing or unknown fact ⇒ `Boxed` (`AverInt`), never `Bare`. So a bug
//! here is a MISSED optimization (lost speed), never a wrong value — the
//! opposite of the wasm-gc silent-wrong-value risk. A wrongly-bare value
//! would reintroduce silent two's-complement wrapping, and additionally
//! the emitted Rust would not type-check (a bare value reaching an
//! `AverInt` slot is a `rustc` error), which is itself a backstop.

use std::collections::{HashMap, HashSet};

use crate::ast::{BinOp, Literal, Spanned, Type};
use crate::ir::FnId;
use crate::ir::interval::{Interval, OpClass, raw_i64_eligible};

use super::super::expr::{MirCallee, MirExpr, MirPattern};
use super::super::program::{LocalId, MirFn, MirProgram};

/// ETAP-2 SLICE 0+1 — per-carrier-type proven bound, keyed by the opaque
/// type's bare Aver name (the `MirParam.ty` string). Built once by
/// [`crate::codegen::proof_lower::carrier_interval_table`] and threaded into
/// [`analyze`]. The `bool` is the `interval_known` bit from
/// [`crate::ir::interval::interval_of_invariant`]: only a recognized,
/// `fits_i64` bound makes a carrier slot bare-eligible. An EMPTY table
/// (the default at every non-carrier call site — the VM facts path, tests)
/// reproduces the pre-slice all-`Int` behavior byte-for-byte.
pub type CarrierIntervals = HashMap<String, (Interval, bool)>;

/// ETAP-2 multi-field carrier-`i64` — per-`(record-type, field)` proven bound,
/// keyed by the bare record name + field name. Built by
/// [`crate::codegen::proof_lower::field_carrier_eligible_intervals`] (already
/// tightened through the same demotion scans as the single-field set) and
/// threaded into [`analyze`] alongside [`CarrierIntervals`]. Lets
/// [`FnBareFacts::carrier_project_interval`] recognize a DIRECT bounded-field
/// read — `Project(rec, "x")` where `rec`'s stamped type is a bounded record
/// and field `x` is eligible — as a raw i64 leaf carrying `x`'s bound (#550
/// stored the field as a native `i64`, so the `struct.get` yields i64). An
/// EMPTY table (the default at every non-wasm-gc call site — the VM facts path,
/// the Rust backend, tests) reproduces the pre-slice all-`Int` behavior.
pub type FieldCarrierIntervals = HashMap<(String, String), (Interval, bool)>;

/// The proven interval for a carrier whose Aver type name is `ty`, returned
/// ONLY when the table holds a recognized bound (`interval_known`) that
/// `fits_i64`. Any other case (`ty` not a carrier, an unrecognized invariant
/// omitted upstream, or a bound too wide for `i64`) yields `None` — the
/// fail-closed decline that keeps the carrier boxed.
///
/// `ty` is a `MirParam.ty` string, which the lowerer fills with
/// `format!("{:?}", Type)` — so a named carrier type renders as the Debug
/// form `Named { id: Some(TypeId(N)), name: "IntRange" }`, NOT the bare
/// `"IntRange"`. We extract the bare `name:` to match the table key (which
/// is keyed by the bare type name from `populate_refined_types`).
///
/// ## Seam gate (`CARRIER_BARE_ELIGIBLE`)
///
/// This is the analysis half of carrier-`i64` lowering. The codegen half —
/// flipping a bare-carrier function slot to a native `i64` on the wasm-gc /
/// Rust backends — is NOT in place yet: a carrier is a refinement-via-opaque
/// single-field record that the wasm-gc registry already *newtype-erases* to
/// its underlying `Int` ref (`$aint`), and the body-emit path (`Project` of
/// the carrier field, `RecordCreate` of the carrier, the smart-constructor
/// `Result.Ok(IntRange(..))` boundary, and the same on Rust which does NOT
/// erase the carrier at all) still expects that ref. Flagging a carrier slot
/// bare in `MirFnRepr` therefore desyncs the body from the (still-boxed)
/// signature and emits invalid wasm. So the seam ships GATED OFF — exactly
/// the discipline 2a used (`ENABLE_BARE_SLOTS = false`) before 2b flipped it.
/// The table, threading, name-extraction, escape-coupling and summary
/// integration below are all LIVE and tested; flipping this `const` to
/// `true` (after the body-emit bridge lands) turns the slice on with no
/// other change to this file.
const CARRIER_BARE_ELIGIBLE: bool = true;

fn carrier_interval(ty: &str, carrier: &CarrierIntervals) -> Option<Interval> {
    if !CARRIER_BARE_ELIGIBLE {
        return None;
    }
    let bare = bare_named_type(ty)?;
    let (iv, known) = carrier.get(bare).copied()?;
    if known && iv.fits_i64() {
        Some(iv)
    } else {
        None
    }
}

/// Extract the bare type name from a `MirParam.ty` Debug string for a
/// `Type::Named`. Returns the `name: "X"` payload as `X`, or `None` when the
/// string is not a `Named { … }` Debug form (e.g. `"Int"`, `"Result(…)"`).
/// The Debug format of `Type::Named { id, name }` is stable
/// (`Named { id: …, name: "X" }`); we slice out the quoted `name:` field.
fn bare_named_type(ty: &str) -> Option<&str> {
    let rest = ty.strip_prefix("Named {")?;
    let after = rest.split("name:").nth(1)?;
    let start = after.find('"')? + 1;
    let end = after[start..].find('"')? + start;
    Some(&after[start..end])
}

/// Representation chosen for a value: a native machine `i64` or the
/// default arbitrary-precision `AverInt`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Repr {
    /// Native `i64` — emitted only when proven sound.
    Bare,
    /// `aver_rt::AverInt` — the safe default.
    Boxed,
}

/// Per-value representation-selection fact, keyed by `LocalId` within a
/// `MirFn`. `repr == Bare` ⟺ `raw_i64_eligible(interval, ops) && !escapes`.
#[derive(Debug, Clone)]
pub struct ValueFact {
    /// Derived interval over-approximation (`None` = the analysis could
    /// not derive a bound, the conservative decline).
    pub interval: Option<Interval>,
    /// Worst-case op class over every arithmetic node the value flows
    /// through. `OverflowFree` is the only band that may be `Bare`.
    pub op_class: OpClass,
    /// `true` when the value reaches a general-Int context (returned as a
    /// general Int, passed to a general-Int param, stored in an aggregate,
    /// or stringified). A `Bare` value never escapes by construction.
    pub escapes: bool,
    /// The chosen representation.
    pub repr: Repr,
}

impl ValueFact {
    /// The safe default — `Boxed`, unknown interval, declined.
    fn boxed() -> Self {
        Self {
            interval: None,
            op_class: OpClass::Unbounded,
            escapes: true,
            repr: Repr::Boxed,
        }
    }

    pub fn is_bare(&self) -> bool {
        self.repr == Repr::Bare
    }
}

/// Per-`MirFn` representation facts the Rust walker consumes.
#[derive(Debug, Clone, Default)]
pub struct FnBareFacts {
    /// Per-`LocalId` value fact. A slot absent from this map defaults to
    /// `Boxed` (fail-closed).
    pub values: HashMap<LocalId, ValueFact>,
    /// Per-param-index representation: `true` ⟺ the param is emitted as a
    /// bare `i64` in the fn signature (and every caller converts at the
    /// boundary). Indexed by declaration order, same indexing
    /// `own_param`'s `aliased_slots` uses.
    pub bare_params: Vec<bool>,
    /// `true` ⟺ the fn's return type is emitted as a bare `i64`.
    pub bare_return: bool,
    /// ETAP-2 carrier-`i64` (wasm-gc only): slots holding a bare carrier
    /// value (an eligible refinement-via-opaque carrier whose wasm storage IS
    /// a native `i64`) mapped to the carrier's PROVEN `fits_i64` interval.
    /// A `Project(Local(slot), "value")` over such a slot reads the i64
    /// directly (no `$AverInt` project bridge) and contributes the carrier's
    /// interval to the surrounding arithmetic, so `c.value + c.value` over a
    /// `[0,100]` carrier stays in the OverflowFree band. EMPTY on the Rust
    /// backend (it keeps the carrier struct) and whenever no eligible carrier
    /// is in scope — the byte-identical default.
    pub carrier_slots: HashMap<LocalId, Interval>,
    /// ETAP-2 carrier-`i64` (wasm-gc only): the eligible-carrier type table
    /// (`bare type name → (proven interval, recognized)`), restricted to the
    /// registry's eligible set. Lets `carrier_project_interval` recognize a
    /// NESTED carrier-field read — `Project(Project(rec, "coord"), "value")`,
    /// where the inner `rec.coord` is stamped an eligible carrier type — as a
    /// raw i64 leaf carrying `coord`'s proven bound. The #550 storage erasure
    /// already made the carrier field a native `i64`, so the inner field read
    /// yields i64 and the outer `.value` is identity. EMPTY on the Rust
    /// backend / whenever no eligible carrier is in scope — the byte-identical
    /// default (the nested form then never fires, only the `Local`-base form).
    pub carrier_types: CarrierIntervals,
    /// ETAP-2 multi-field carrier-`i64` (wasm-gc only): the eligible
    /// `(record, field)` bound table. Lets `carrier_project_interval` recognize
    /// a DIRECT bounded-field read — `Project(rec, "x")` where `rec`'s stamped
    /// type is a bounded record and `(record, "x")` is eligible — as a raw i64
    /// leaf carrying `x`'s proven bound. The #550 storage erasure stored that
    /// field as a native `i64`, so the `struct.get` yields i64 and the read is a
    /// bare leaf for the surrounding arithmetic. EMPTY on the Rust backend /
    /// whenever no bounded multi-field record is in scope — the byte-identical
    /// default (the direct-field form then never fires).
    pub field_carrier_intervals: FieldCarrierIntervals,
}

impl FnBareFacts {
    /// Is the value bound to `slot` emitted as a bare `i64`?
    pub fn is_bare(&self, slot: LocalId) -> bool {
        self.values.get(&slot).is_some_and(ValueFact::is_bare)
    }

    /// Is param index `i` bare in the signature?
    pub fn param_is_bare(&self, i: usize) -> bool {
        self.bare_params.get(i).copied().unwrap_or(false)
    }

    /// The interval over-approximation of a value bound to `slot`, when the
    /// analysis derived one and proved the slot `Bare`. `None` for a boxed
    /// or unknown slot — the conservative decline.
    fn bare_slot_interval(&self, slot: LocalId) -> Option<Interval> {
        let fact = self.values.get(&slot)?;
        if !fact.is_bare() {
            return None;
        }
        fact.interval
    }

    /// ETAP-2 carrier-`i64`: the proven interval of a `.value` `Project` whose
    /// BASE renders an eligible carrier value (wasm storage IS i64). Two base
    /// shapes qualify, both reading a native i64:
    ///   - a `Local` in `carrier_slots` — a bare carrier PARAM/local (the #551
    ///     param-level form);
    ///   - a NESTED carrier-field read — a `Project` whose result `ty()` is an
    ///     eligible carrier type (`carrier_types`), e.g. `rec.coord` in
    ///     `rec.coord.value`. The #550 storage erasure made `coord` a native
    ///     `i64` field, so the inner `struct.get` yields i64 and the outer
    ///     `.value` is identity. The general field-of-field-of-… case is
    ///     covered: ANY base whose `ty()` is an eligible carrier renders i64.
    ///
    /// In both cases the `.value` read is a native i64 carrying the carrier's
    /// smart-constructor bound, a bare leaf for the surrounding arithmetic.
    /// `None` for any other base — the conservative, fail-closed decline (the
    /// boxed `$AverInt` project bridge runs).
    pub fn carrier_project_interval(&self, e: &MirExpr) -> Option<Interval> {
        let MirExpr::Project(p) = e else {
            return None;
        };
        // Param/local bare carrier slot (#551).
        if let MirExpr::Local(local) = &p.node.base.node
            && let Some(iv) = self.carrier_slots.get(&local.node.slot).copied()
        {
            return Some(iv);
        }
        // Multi-field direct bounded-field read: `Project(rec, "x")` where
        // `rec`'s stamped type is a bounded record and `(record, "x")` is an
        // eligible field. The #550 storage erasure stored `x` as a native `i64`,
        // so the `struct.get` yields raw i64 — a bare leaf carrying `x`'s proven
        // bound. (The base `rec` is a `Coord` struct ref, NOT itself an eligible
        // carrier, so this does not overlap the single-field `.value` paths.)
        if let Some(iv) = self.field_carrier_field_interval(&p.node.base, &p.node.field) {
            return Some(iv);
        }
        // Nested carrier-field read: the base is any expression whose stamped
        // type is an eligible carrier (a field read, or transitively a field of
        // a field). The #550 storage erasure made that carrier a native `i64`,
        // so the base renders raw i64 and its `.value` is identity. Fail-closed:
        // if the base has no stamped type, or the type is not an eligible
        // carrier, decline (boxed).
        self.base_renders_eligible_carrier(&p.node.base)
    }

    /// ETAP-2 multi-field carrier-`i64`: the proven interval of field `field`
    /// read off `base`, when `base`'s stamped type is a bounded record and
    /// `(record, field)` is an eligible bounded field. #550 stored that field
    /// as a native `i64`, so the `struct.get` reads raw i64 directly — a bare
    /// leaf. `None` when `base` has no stamped record type, or the
    /// `(record, field)` pair is not eligible / not `fits_i64` — fail-closed.
    fn field_carrier_field_interval(
        &self,
        base: &Spanned<MirExpr>,
        field: &str,
    ) -> Option<Interval> {
        // Exact-name lookup only — the table is keyed by post-flatten
        // `TypeDef` names, and a qualified→bare fallback would hand a
        // collision-renamed dep record an unrelated type's bounded-field fact.
        let name = base.ty().and_then(Type::named_name)?;
        let (iv, known) = self
            .field_carrier_intervals
            .get(&(name.to_string(), field.to_string()))
            .copied()?;
        (known && iv.fits_i64()).then_some(iv)
    }

    /// The proven interval of `base` when it renders an eligible carrier value
    /// as a native `i64` — i.e. `base.ty()` is an eligible carrier type held in
    /// `carrier_types`. This is the NESTED-field recognition: a carrier-typed
    /// field read (`rec.coord`) was erased to an i64 field by #550, so reading
    /// its `.value` is identity over that i64. We additionally require `base`
    /// to be a `Project` so the recognition matches EXACTLY the i64-rendering
    /// positions the wasm-gc emitter skips the project bridge for (a carrier
    /// field read); a carrier-typed `Local`/`Call`/etc. base is NOT recognized
    /// here (those go through `carrier_slots` or stay boxed) — fail-closed.
    fn base_renders_eligible_carrier(&self, base: &Spanned<MirExpr>) -> Option<Interval> {
        if !matches!(base.node, MirExpr::Project(_)) {
            return None;
        }
        // Exact-name lookup only — same rationale as
        // `field_carrier_field_interval`.
        let name = base.ty().and_then(Type::named_name)?;
        let (iv, known) = self.carrier_types.get(name).copied()?;
        (known && iv.fits_i64()).then_some(iv)
    }

    /// ETAP-2 carrier-`i64`: does `e` read a bare carrier's i64 `.value`
    /// (`Project(Local(bare_carrier_slot), _)`)? Such a read renders raw i64
    /// on wasm-gc (the project bridge is skipped), so it is a bare leaf.
    pub fn is_carrier_project(&self, e: &MirExpr) -> bool {
        self.carrier_project_interval(e).is_some()
    }

    /// Compute the result interval of an EXPRESSION TREE built only from
    /// bare leaves (`Local`s the analysis proved `Bare`, `Int` literals) and
    /// `Add`/`Sub`/`Mul`/`Neg` nodes, using the saturating #511 interval
    /// arithmetic. Returns `None` if any leaf is non-bare / unknown, the node
    /// is an unsupported shape, OR any INTERMEDIATE sub-result leaves `i64` —
    /// the conservative decline.
    ///
    /// This is the SINGLE SOURCE OF TRUTH for "what interval does this
    /// inline compound evaluate to": both the analysis's `tail_value_is_bare`
    /// and the Rust backend's `mir_expr_is_bare_i64` route a compound through
    /// here, so neither can accept a tree whose result leaves `i64`.
    ///
    /// SOUNDNESS: gating only the WHOLE-TREE result is not enough — a
    /// transient out-of-`i64` intermediate (`(n + i64::MAX) - i64::MAX`,
    /// whose inner `Add` is `[MAX+1, …]` but whose final value narrows back
    /// into range) would lower this node's raw-`i64` op and WRAP before the
    /// enclosing op runs. So every node's result is checked against `i64`
    /// here, mirroring `eval_interval`'s `worst`-join on the analysis side;
    /// a single escaping sub-result declines the whole compound to boxed.
    pub fn bare_expr_interval(&self, e: &MirExpr) -> Option<Interval> {
        match e {
            MirExpr::Literal(l) => match l.node {
                Literal::Int(k) => Some(Interval::point(k as i128)),
                _ => None,
            },
            MirExpr::Local(local) => self.bare_slot_interval(local.node.slot),
            // ETAP-2 carrier-`i64`: a bare carrier's `.value` is a native i64
            // leaf carrying the carrier's proven bound.
            MirExpr::Project(_) => self.carrier_project_interval(e),
            MirExpr::Neg(inner) => {
                let r = Interval::point(0).sub(self.bare_expr_interval(&inner.node)?);
                r.fits_i64().then_some(r)
            }
            MirExpr::BinOp(b) => {
                let l = self.bare_expr_interval(&b.node.lhs.node)?;
                let r = self.bare_expr_interval(&b.node.rhs.node)?;
                let result = match b.node.op {
                    BinOp::Add => l.add(r),
                    BinOp::Sub => l.sub(r),
                    BinOp::Mul => l.mul(r),
                    _ => return None,
                };
                result.fits_i64().then_some(result)
            }
            _ => None,
        }
    }

    /// Is `e` a bare-`i64`-eligible expression? A `Local`/`Int` leaf the
    /// analysis proved `Bare`, or an `Add`/`Sub`/`Mul`/`Neg` tree over such
    /// leaves WHOSE RESULT INTERVAL provably fits `i64` (every intermediate
    /// stays `OverflowFree` under the saturating interval arithmetic).
    ///
    /// SOUNDNESS (BUG 2): a compound is bare ONLY when its result interval
    /// ⊆ `i64`. An overflowing compound like `n + i64::MAX` (result
    /// `[MAX+1, …]`, outside `i64`) is NOT bare, so codegen must emit the
    /// boxed `AverInt` arithmetic with `from_i64` boundary conversions — the
    /// raw-`i64` path would silently wrap (`overflow-checks = false`).
    pub fn expr_is_bare_i64(&self, e: &MirExpr) -> bool {
        match e {
            // A direct bare leaf: `Local` proven `Bare`, or an `Int` literal
            // (an exact point — its `i64`-fit is checked by the enclosing
            // compound's interval; a standalone literal is always a sound
            // `{N}i64` constant on a bare path the analysis already gated).
            MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
            MirExpr::Local(local) => self.is_bare(local.node.slot),
            // ETAP-2 carrier-`i64`: a bare carrier's `.value` reads raw i64.
            MirExpr::Project(_) => self.is_carrier_project(e),
            // A compound: require the WHOLE-TREE result interval to fit i64.
            MirExpr::Neg(_) | MirExpr::BinOp(_) => self.bare_expr_interval(e).is_some_and(|iv| {
                raw_i64_eligible(Some(iv), std::iter::once(&OpClass::OverflowFree))
            }),
            _ => false,
        }
    }
}

/// Whole-program representation-selection facts, keyed by `FnId`. Built
/// once per compilation alongside the optimized `MirProgram`; the Rust
/// backend reads a per-fn slice at signature + body emit.
#[derive(Debug, Clone, Default)]
pub struct BareI64Facts {
    fns: HashMap<FnId, FnBareFacts>,
}

impl BareI64Facts {
    /// Per-fn facts, or `None` when the fn is absent (fail-closed: the
    /// caller then treats every value as `Boxed`).
    pub fn for_fn(&self, id: FnId) -> Option<&FnBareFacts> {
        self.fns.get(&id)
    }

    /// Total values proven `Bare` across the whole program — a diagnostic
    /// counter (proof the recognizer fired), nothing in codegen keys off it.
    pub fn bare_values(&self) -> usize {
        self.fns
            .values()
            .flat_map(|f| f.values.values())
            .filter(|v| v.is_bare())
            .count()
    }
}

/// Entry point: compute the bare-`i64` representation facts for `program`.
///
/// The analysis is whole-program: a tail-recursion counter that crosses a
/// self-tail-call frame can only go bare if the param it crosses to is
/// ALSO bare, so the per-fn param/return summary is computed against the
/// visible `MirCallee::Fn` / `MirTailCall` call graph. A dependency-module
/// fragment (callers unseen) bails to all-`Boxed`, exactly like
/// `own_param`.
pub fn analyze(program: &MirProgram, carrier: &CarrierIntervals) -> BareI64Facts {
    analyze_with_fields(program, carrier, &FieldCarrierIntervals::new())
}

/// [`analyze`] plus the multi-field carrier table (`field_carrier`): the
/// wasm-gc entry threads the eligible `(record, field)` bounds so a DIRECT
/// bounded-field read renders as a raw i64 leaf. Every other caller goes
/// through [`analyze`] (empty field table) and keeps the byte-identical
/// pre-slice behavior.
pub fn analyze_with_fields(
    program: &MirProgram,
    carrier: &CarrierIntervals,
    field_carrier: &FieldCarrierIntervals,
) -> BareI64Facts {
    // Diagnostic / bench-differential escape hatch: skip the analysis so a
    // run keeps the conservative all-Boxed baseline.
    if std::env::var("AVER_NO_BARE_I64").is_ok() {
        return BareI64Facts::default();
    }
    // Whole-program gate: a bare param/return changes a fn's Rust ABI, so
    // EVERY caller must be visible to convert at the boundary. A
    // dependency-module fragment is missing the entry/sibling call sites,
    // so bail to all-Boxed.
    if program.external_callers_possible || program.modules.len() > 1 {
        return BareI64Facts::default();
    }

    // Param Int-typedness per fn (a bare param must itself be `Int`).
    let mut int_params: HashMap<FnId, Vec<bool>> = HashMap::new();
    for (id, f) in program.iter() {
        int_params.insert(*id, f.params.iter().map(|p| ty_str_is_int(&p.ty)).collect());
    }

    // The minimal cross-frame summary: which params are bare, whether the
    // return is bare. The field table only adds NEW bare leaves (a direct
    // bounded-field read), which the body pass reads via the per-fn facts; the
    // cross-frame param/return summary keys off `carrier` exactly as before.
    let summary = compute_summary(program, &int_params, carrier, field_carrier);

    let mut fns: HashMap<FnId, FnBareFacts> = HashMap::new();
    for (id, f) in program.iter() {
        fns.insert(*id, analyze_fn(f, &summary, carrier, field_carrier));
    }

    BareI64Facts { fns }
}

/// Cross-frame summary: per-fn, which params are bare and whether the
/// return is bare. Computed as a monotone-descending fixpoint (a param /
/// return starts optimistically bare, demoted to boxed when any
/// constraint fails), mirroring `own_param`'s lattice.
struct Summary {
    bare_params: HashMap<FnId, Vec<bool>>,
    bare_return: HashMap<FnId, bool>,
    /// Per-param TIGHT recurrence interval (`compute_bare_param_intervals`),
    /// so the body pass can seed a bare counter with its PROVEN range
    /// (`[K-step, entry]`) instead of the full `i64` line. This is what keeps
    /// `n - 1` / `acc * n` over a bare counter in the OverflowFree band: a
    /// full-`i64` seed makes every `+`/`-`/`*` over the counter look like it
    /// could overflow, demoting the fast decrement to a boxed round-trip. A
    /// `None` entry (or a missing param) falls back to the full-`i64` seed.
    bare_param_intervals: HashMap<FnId, Vec<Option<Interval>>>,
}

impl Summary {
    fn param_bare(&self, id: FnId, i: usize) -> bool {
        self.bare_params
            .get(&id)
            .and_then(|v| v.get(i).copied())
            .unwrap_or(false)
    }

    fn return_bare(&self, id: FnId) -> bool {
        self.bare_return.get(&id).copied().unwrap_or(false)
    }

    /// The proven recurrence interval for param `i` of `id`, if one was
    /// derived. `None` ⇒ the body pass seeds the full `i64` range.
    fn param_interval(&self, id: FnId, i: usize) -> Option<Interval> {
        self.bare_param_intervals
            .get(&id)
            .and_then(|v| v.get(i).copied())
            .flatten()
    }
}

fn compute_summary(
    program: &MirProgram,
    int_params: &HashMap<FnId, Vec<bool>>,
    carrier: &CarrierIntervals,
    field_carrier: &FieldCarrierIntervals,
) -> Summary {
    // Address-taken fns (name appears as a `FnValue`) have callers we
    // cannot attribute — pin all their params/return boxed.
    let mut address_taken: HashSet<String> = HashSet::new();
    for (_, f) in program.iter() {
        collect_fn_values(&f.body.node, &mut address_taken);
    }

    // Seed: optimistic. A param is candidate-bare iff it is Int-typed and
    // the fn is not externally reachable (main / address-taken). The
    // return is candidate-bare iff Int-typed and not externally reachable.
    let mut bare_params: HashMap<FnId, Vec<bool>> = HashMap::new();
    let mut bare_return: HashMap<FnId, bool> = HashMap::new();
    for (id, f) in program.iter() {
        let externally_reachable = f.name == "main" || address_taken.contains(&f.name);
        let ip = &int_params[id];
        let seed: Vec<bool> = ip.iter().map(|&i| i && !externally_reachable).collect();
        bare_params.insert(*id, seed);
        bare_return.insert(*id, ty_str_is_int(&f.return_type) && !externally_reachable);
    }

    // Collect visible call edges (`Call(Fn)` + `TailCall`) with their args.
    let mut edges: Vec<CallEdge> = Vec::new();
    for (caller, f) in program.iter() {
        collect_call_edges(*caller, &f.body.node, &mut edges);
    }

    // A fn's return may be bare only when it has at least one visible
    // NON-tail (ordinary `Call(Fn)`) caller — a fn with no such caller is
    // externally reachable (a library entry), so changing its return ABI
    // to `i64` would break an unseen caller. (A self-tail-call is the
    // recurrence edge, not an external consumer, so it does not count.)
    let mut has_ordinary_caller: HashSet<FnId> = HashSet::new();
    for edge in &edges {
        if !edge.is_tail_self() {
            has_ordinary_caller.insert(edge.target);
        }
    }
    for (id, br) in bare_return.iter_mut() {
        if !has_ordinary_caller.contains(id) {
            *br = false;
        }
    }

    // Tight per-param recurrence intervals, computed ONCE: they are a pure
    // function of the recurrence shape + literal entry callers, independent
    // of the fixpoint's escape/return state, so they stay valid across
    // iterations. A bare counter is seeded with this proven range in the
    // body pass instead of the full `i64` line, keeping `n - 1` / `acc * n`
    // OverflowFree (see `Summary::bare_param_intervals`).
    //
    // Produced by a SOUND guard-seeded interval FIXPOINT over the call graph
    // (`compute_bare_param_intervals`): the entry literals seed an interval
    // per `(fn, param)`, every self-tail back-edge is JOINED as a recurrence
    // transfer, an equality-decrement guard installs a convex floor, and
    // widening guarantees termination. Any unrecognized / unfloored shape
    // widens out of `i64` and maps to `None` (boxed). Drop-in: the output
    // type and both readers (condition B + the `analyze_fn` seed) are
    // unchanged.
    let bare_param_intervals = compute_bare_param_intervals(program, &edges, &address_taken);

    loop {
        let mut changed = false;

        // (A) A bare callee param stays bare only if every caller can
        //     supply a bare/literal/bounded value at that position.
        for edge in &edges {
            let Some(callee_params) = bare_params.get(&edge.target).cloned() else {
                continue;
            };
            for (i, arg) in edge.args.iter().enumerate() {
                if !callee_params.get(i).copied().unwrap_or(false) {
                    continue; // already boxed — skip
                }
                let ok =
                    arg_supplies_bare(&arg.node, edge.caller, program, &bare_params, &bare_return);
                if !ok {
                    demote_param(&mut bare_params, edge.target, i, &mut changed);
                }
            }
        }

        // (B) The counter must remain in `i64` range across the recurrence
        //     and the literal-bounded entry. A param is bare only when its
        //     derived interval (precomputed in `bare_param_intervals`)
        //     provably fits `i64`.
        for (id, _f) in program.iter() {
            let ip = &int_params[id];
            for (i, &is_int) in ip.iter().enumerate() {
                if !bare_params[id].get(i).copied().unwrap_or(false) {
                    continue;
                }
                if !is_int {
                    demote_param(&mut bare_params, *id, i, &mut changed);
                    continue;
                }
                let bound = bare_param_intervals
                    .get(id)
                    .and_then(|v| v.get(i).copied())
                    .flatten();
                let eligible = bound.is_some_and(|iv| {
                    raw_i64_eligible(Some(iv), std::iter::once(&OpClass::OverflowFree))
                });
                if !eligible {
                    demote_param(&mut bare_params, *id, i, &mut changed);
                }
            }
        }

        // (B2) A bare PARAM must not ESCAPE in its own body. If the counter
        //      flows into a general-Int context (a boxed callee param, a
        //      builtin like `Float.fromInt(n)`, an aggregate, or a
        //      stringify), its representation must be `AverInt` — emitting
        //      a bare `i64` param whose body reads it through an `AverInt`
        //      method is a `rustc` type error (and a missed conversion).
        //      This couples the signature (driven by `bare_params`) to the
        //      body's escape predicate so the two never disagree. The
        //      escape set depends on the current bare-param seed (a bare
        //      callee param does not escape its arg), so recompute it inside
        //      the fixpoint; demotion is monotone.
        let tmp = Summary {
            bare_params: bare_params.clone(),
            bare_return: bare_return.clone(),
            bare_param_intervals: bare_param_intervals.clone(),
        };
        for (id, f) in program.iter() {
            let mut escaping: HashSet<LocalId> = HashSet::new();
            scan_escapes(&f.body.node, &tmp, &mut escaping);
            for (i, p) in f.params.iter().enumerate() {
                if bare_params[id].get(i).copied().unwrap_or(false) && escaping.contains(&p.local) {
                    demote_param(&mut bare_params, *id, i, &mut changed);
                }
            }
        }

        // (C) The return is bare only when the body's tail value is itself
        //     bare-eligible. Recompute the body facts against the current
        //     seed and demote when the body says the return cannot be bare.
        for (id, f) in program.iter() {
            if !bare_return.get(id).copied().unwrap_or(false) {
                continue;
            }
            let tmp = Summary {
                bare_params: bare_params.clone(),
                bare_return: bare_return.clone(),
                bare_param_intervals: bare_param_intervals.clone(),
            };
            let facts = analyze_fn(f, &tmp, carrier, field_carrier);
            if !facts.bare_return && bare_return.insert(*id, false) != Some(false) {
                changed = true;
            }
        }

        // (C2) A fn's return is bare only if EVERY caller consumes the
        //      result in an i64-safe position. A bare `i64` return value
        //      consumed in a general-Int context (an `AverInt`-method
        //      arithmetic operand, a boxed callee param, an aggregate
        //      field) would be a `rustc` type error and a missed
        //      conversion. We do NOT insert per-call-site return
        //      conversions in this slice, so the conservative rule is:
        //      mark `bare_return` only when every call result is discarded,
        //      stringified, or fed where a bare value is accepted (a bare
        //      param). Any other use demotes the callee's return. This is
        //      what keeps factorial/countdown (their results are discarded
        //      or stringified by `main`) bare while a return flowing into
        //      arithmetic stays boxed.
        let unsafe_returns = collect_unsafe_return_consumers(program, &bare_params);
        for id in &unsafe_returns {
            if bare_return.get(id).copied().unwrap_or(false)
                && bare_return.insert(*id, false) != Some(false)
            {
                changed = true;
            }
        }

        // (C3) A bare-returning callee whose result is bound to a `let` is
        //      safe ONLY when the binding slot is itself bare (a fresh `i64`
        //      that stays raw). If the binding is BOXED — its later uses
        //      escape into a general-Int context (`from.x + dx`, an aggregate,
        //      a boxed param) so `analyze_fn` declined it — then storing the
        //      raw `i64` call result into the `AverInt` slot is unsound: a
        //      `rustc` type error on Rust, a wasm VALIDATION error on wasm-gc
        //      (no per-store coercion). The C2 scan deliberately does NOT flag
        //      a `let`-bound call (the binding's uses are scanned as their own
        //      positions), but a `Local` use never flags its originating call,
        //      so this binding-aware step closes that gap: recompute each
        //      caller's per-slot facts and demote any bare-returning callee
        //      bound to a non-bare slot. (Mirror of C2's discipline, keyed on
        //      the binding repr instead of the consumer position.)
        let tmp = Summary {
            bare_params: bare_params.clone(),
            bare_return: bare_return.clone(),
            bare_param_intervals: bare_param_intervals.clone(),
        };
        for (_caller, f) in program.iter() {
            let facts = analyze_fn(f, &tmp, carrier, field_carrier);
            let mut demote: Vec<FnId> = Vec::new();
            collect_let_bound_boxed_returns(&f.body.node, &facts, &bare_return, &mut demote);
            for target in demote {
                if bare_return.get(&target).copied().unwrap_or(false)
                    && bare_return.insert(target, false) != Some(false)
                {
                    changed = true;
                }
            }
        }

        if !changed {
            break;
        }
    }

    Summary {
        bare_params,
        bare_return,
        bare_param_intervals,
    }
}

/// Walk `e`, collecting any `Call(Fn(target))` that is the immediate VALUE
/// of a `Let` whose binding slot the caller's per-slot analysis (`facts`)
/// declined to make bare, AND whose `target` the summary currently marks
/// `bare_return`. Such a callee's raw `i64` result would be stored into a
/// boxed `AverInt` binding slot with no boundary conversion — unsound. The
/// fixpoint demotes each collected `target`'s `bare_return` (C3).
fn collect_let_bound_boxed_returns(
    e: &MirExpr,
    facts: &FnBareFacts,
    bare_return: &HashMap<FnId, bool>,
    out: &mut Vec<FnId>,
) {
    if let MirExpr::Let(l) = e
        && let MirExpr::Call(c) = &l.node.value.node
        && let MirCallee::Fn(target) = c.node.callee
        && bare_return.get(&target).copied().unwrap_or(false)
        && !facts.is_bare(l.node.binding)
    {
        out.push(target);
    }
    visit_children(e, &mut |c| {
        collect_let_bound_boxed_returns(c, facts, bare_return, out)
    });
}

/// The set of callee `FnId`s whose return value is consumed in at least
/// one i64-UNSAFE position by some caller. A bare `i64` return reaching
/// such a position (an `AverInt` arithmetic operand, a boxed callee param,
/// an aggregate field, a stringify-incompatible context) would not
/// type-check, and this slice inserts no per-call-site return conversion —
/// so any unsafe consumer demotes the callee's `bare_return`.
///
/// SAFE consumers (do NOT demote): a discarded result (a `Let` whose
/// binding is never re-read as a boxed value — approximated as: the call
/// is the whole RHS and the binding is a fresh i64), a stringify
/// (`String.fromInt` / interpolation embed), the direct tail value of a
/// fn whose own return is bare, and an argument at a bare callee param
/// index. Everything else is unsafe.
fn collect_unsafe_return_consumers(
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
) -> HashSet<FnId> {
    let mut unsafe_set: HashSet<FnId> = HashSet::new();
    for (_, f) in program.iter() {
        scan_return_consumers(&f.body.node, program, bare_params, &mut unsafe_set);
    }
    unsafe_set
}

/// Walk `e`, flagging every `Call(Fn(target))` whose RESULT lands in an
/// i64-unsafe position. A call reached as a direct child of a SAFE position
/// (a discard statement, a stringify, a bare param arg, a tail/return that
/// is itself bare) is not flagged; one reached anywhere else IS. We
/// approximate by walking each node and classifying the position of any
/// DIRECT `Call(Fn)` child; nested calls are handled when the walk reaches
/// their own parent.
fn scan_return_consumers(
    e: &MirExpr,
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
    unsafe_set: &mut HashSet<FnId>,
) {
    // Helper: flag a direct `Call(Fn)` child as unsafe (its result is
    // consumed in a general-Int position here).
    let flag_if_call = |child: &MirExpr, unsafe_set: &mut HashSet<FnId>| {
        if let MirExpr::Call(c) = child
            && let MirCallee::Fn(t) = c.node.callee
        {
            unsafe_set.insert(t);
        }
    };

    match e {
        // Arithmetic / negation operands are i64-UNSAFE for a boxed result
        // (the boxed-path emit calls `AverInt` methods on the operand).
        MirExpr::BinOp(b) => {
            flag_if_call(&b.node.lhs.node, unsafe_set);
            flag_if_call(&b.node.rhs.node, unsafe_set);
        }
        MirExpr::Neg(inner) => flag_if_call(&inner.node, unsafe_set),
        // Aggregate fields/elements are general-Int contexts.
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for it in items {
                flag_if_call(&it.node, unsafe_set);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                flag_if_call(&k.node, unsafe_set);
                flag_if_call(&v.node, unsafe_set);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                flag_if_call(&a.node, unsafe_set);
            }
        }
        MirExpr::RecordCreate(r) => {
            for fld in &r.node.fields {
                flag_if_call(&fld.value.node, unsafe_set);
            }
        }
        MirExpr::RecordUpdate(u) => {
            flag_if_call(&u.node.base.node, unsafe_set);
            for fld in &u.node.updates {
                flag_if_call(&fld.value.node, unsafe_set);
            }
        }
        // A call's args: a call result passed at a BOXED callee param index
        // is unsafe; at a BARE index it is safe (the param accepts i64). A
        // builtin/intrinsic/fn-value callee that takes Int args treats the
        // result as a general Int (e.g. `String.fromInt` reads the value) —
        // those are SAFE for stringify but UNSAFE for arithmetic builtins.
        // We conservatively treat only `String.fromInt` and interpolation
        // (handled below) as the stringify-safe sinks; every other builtin
        // Int arg is unsafe.
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(target) => {
                for (i, a) in c.node.args.iter().enumerate() {
                    let bare_param = bare_params
                        .get(&target)
                        .and_then(|v| v.get(i).copied())
                        .unwrap_or(false);
                    if !bare_param {
                        flag_if_call(&a.node, unsafe_set);
                    }
                }
            }
            MirCallee::Builtin(bid) => {
                let name = program.builtin_name(bid);
                // `String.fromInt(x)` stringifies its arg — `x.to_string()`
                // works on both `i64` and `AverInt`, so a bare return here
                // is SAFE. Every other builtin reads the Int generally.
                if name != "String.fromInt" {
                    for a in &c.node.args {
                        flag_if_call(&a.node, unsafe_set);
                    }
                }
            }
            MirCallee::Intrinsic(_) | MirCallee::LocalSlot { .. } => {
                for a in &c.node.args {
                    flag_if_call(&a.node, unsafe_set);
                }
            }
        },
        // A `TailCall`'s args carry the SAME consumer discipline as a
        // `Call(Fn)`: a call result passed at the tail-callee's BOXED param
        // index is consumed in a general-Int (`AverInt`) position and so is
        // unsafe; at a BARE param it is safe. Without this arm a bare-
        // returning call used as a tail-call arg at a boxed param
        // (`loop(i - 1, signOf(i))`, `signOf` bare-return, `loop`'s `acc`
        // boxed) was NEVER flagged, so `signOf.bare_return` stayed true and
        // its raw `i64` result flowed into the `AverInt` param — a silent
        // wrong value on Rust, a wasm VALIDATION error on wasm-gc (where
        // there is no per-call coercion). Mirror of the `MirCallee::Fn` arm.
        MirExpr::TailCall(tc) => {
            for (i, a) in tc.node.args.iter().enumerate() {
                let bare_param = bare_params
                    .get(&tc.node.target)
                    .and_then(|v| v.get(i).copied())
                    .unwrap_or(false);
                if !bare_param {
                    flag_if_call(&a.node, unsafe_set);
                }
            }
        }
        // Interpolation embeds stringify their values — `to_string()` works
        // on `i64` too, so a call result there is SAFE (do not flag).
        // A `Let` whose VALUE is a call: the binding holds the result; we
        // cannot cheaply prove the binding is only re-read safely, so be
        // conservative and DO NOT flag here (the binding's later uses are
        // scanned as their own positions). This keeps the discard /
        // tail-return case bare while still flagging direct arithmetic /
        // aggregate / boxed-param uses above.
        _ => {}
    }

    // Recurse into all children to catch nested consumer positions.
    visit_children(e, &mut |c| {
        scan_return_consumers(c, program, bare_params, unsafe_set)
    });
}

fn demote_param(
    bare_params: &mut HashMap<FnId, Vec<bool>>,
    id: FnId,
    i: usize,
    changed: &mut bool,
) {
    if let Some(v) = bare_params.get_mut(&id)
        && let Some(slot) = v.get_mut(i)
        && *slot
    {
        *slot = false;
        *changed = true;
    }
}

/// A visible call edge `target(args…)` made from `caller`. `tail_self` is
/// `true` for a `MirTailCall` whose target is the caller itself (the
/// recurrence edge).
struct CallEdge {
    target: FnId,
    caller: FnId,
    args: Vec<Spanned<MirExpr>>,
    tail_self: bool,
}

impl CallEdge {
    fn is_tail_self(&self) -> bool {
        self.tail_self
    }
}

fn collect_call_edges(caller: FnId, e: &MirExpr, out: &mut Vec<CallEdge>) {
    match e {
        MirExpr::Call(c) => {
            if let MirCallee::Fn(target) = c.node.callee {
                out.push(CallEdge {
                    target,
                    caller,
                    args: c.node.args.clone(),
                    tail_self: false,
                });
            }
        }
        MirExpr::TailCall(tc) => {
            out.push(CallEdge {
                target: tc.node.target,
                caller,
                args: tc.node.args.clone(),
                tail_self: tc.node.target == caller,
            });
        }
        _ => {}
    }
    visit_children(e, &mut |c| collect_call_edges(caller, c, out));
}

/// Can the call argument `arg` (evaluated in `caller`) supply a bare /
/// bounded value at a bare callee param?
///
/// - A literal `Int` is always supplyable bare.
/// - The recurrence arithmetic `n - 1`, `acc * n`, `acc + n` … over bare
///   operands stays bare (the `i64`-bound is checked separately).
/// - A read of a caller param that is itself bare is supplyable.
/// - A call to another fn whose return is bare supplies bare.
/// - Anything else cannot be supplied bare ⇒ the callee param demotes.
fn arg_supplies_bare(
    arg: &MirExpr,
    caller: FnId,
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
    bare_return: &HashMap<FnId, bool>,
) -> bool {
    match arg {
        MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
        MirExpr::Neg(inner) => {
            arg_supplies_bare(&inner.node, caller, program, bare_params, bare_return)
        }
        MirExpr::Local(local) => local_supplies_bare(local.node.slot, caller, program, bare_params),
        MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul) => {
            arg_supplies_bare(&b.node.lhs.node, caller, program, bare_params, bare_return)
                && arg_supplies_bare(&b.node.rhs.node, caller, program, bare_params, bare_return)
        }
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(target) => bare_return.get(&target).copied().unwrap_or(false),
            _ => false,
        },
        _ => false,
    }
}

/// Is the caller's slot `slot` a bare param? (Let-bound locals are not
/// tracked across frames in this minimal summary — they demote.)
fn local_supplies_bare(
    slot: LocalId,
    caller: FnId,
    program: &MirProgram,
    bare_params: &HashMap<FnId, Vec<bool>>,
) -> bool {
    let Some(f) = program.fn_by_id(caller) else {
        return false;
    };
    for (i, p) in f.params.iter().enumerate() {
        if p.local == slot {
            return bare_params
                .get(&caller)
                .and_then(|v| v.get(i).copied())
                .unwrap_or(false);
        }
    }
    false
}

// ── The guard-seeded interval FIXPOINT (the bound producer) ──────────────
//
// `compute_bare_param_intervals` replaces the hand-rolled closed-form
// recurrence recognizer with a SOUND per-`(FnId, param-index)` interval
// fixpoint over the call graph. The output type and both consumers are
// unchanged (drop-in): condition B and the `analyze_fn` seed read the same
// `HashMap<FnId, Vec<Option<Interval>>>`.
//
// SOUNDNESS (the C0 obligation). Every interval is built bottom-up from
// `Interval::unbounded()` / literal points using ONLY:
//   - `hull` (the entry-seed join and the per-round state join) — enlarging,
//   - `widen` (the termination operator) — enlarging (superset of `next`),
//   - the single `intersect` with the equality-decrement `floor` — the ONLY
//     narrowing, and it is narrowed against a convex bound that is
//     PATH-GUARANTEED-TRUE on every recursive iteration (a descending
//     counter whose reachability gate proves it lands exactly on `K`, so it
//     is `>= K-step` on every back-edge — see `guard_floor`).
// So a cell mapped to `Some(iv)` with `iv.fits_i64()` is a genuine superset
// of the param's real reachable value-set, hence `⊆ i64`; any unrecognized
// or unfloored shape widens to ±inf ⇒ `None` ⇒ boxed (lose speed, never a
// wrong value). The floor is the only trusted, locally-auditable obligation.

/// The number of join rounds before widening kicks in. With the every-round
/// floor a countdown/factorial counter reaches its fixpoint within `UNROLL`
/// rounds, so widen only ever fires on a genuinely-unbounded endpoint.
const UNROLL: usize = 2;

/// Per-`(FnId, param-index)` interval fixpoint. Replaces the closed-form
/// `param_recurrence_bound`. Produces the SAME output map (drop-in): for each
/// param cell, `Some(iv)` when the solved interval `fits_i64`, else `None`.
///
/// The mechanics, per non-pinned `(f, i)` (see `solve_scc`):
///   1. SEED = hull of every entry literal at index `i` over NON-self-tail
///      caller edges (the entry envelope). No literal entry, or a non-literal
///      entry arg, or no entry caller ⇒ ⊤ ⇒ `None` (boxed).
///   2. FLOOR = the gated equality-decrement convex bound (`guard_floor`),
///      or ⊤ when any of the four preconditions fails.
///   3. Iterate the SCC worklist: every round joins ALL self-tail back-edges
///      (so a second growing path can never be missed), meets the floor, and
///      widens once `round >= UNROLL`.
///
/// Pinned to ⊤ (→ `None`): every param of `main` / an address-taken fn (its
/// callers are unknowable, so its entry envelope is unknown).
fn compute_bare_param_intervals(
    program: &MirProgram,
    edges: &[CallEdge],
    address_taken: &HashSet<String>,
) -> HashMap<FnId, Vec<Option<Interval>>> {
    // State: one interval cell per param; ⊤ = `unbounded()`.
    let mut state: HashMap<FnId, Vec<Interval>> = HashMap::new();
    // Which `(fn, param)` cells are PINNED to ⊤ (externally-reachable fns —
    // callers unknowable, so the entry envelope is unknown). Mirrors the
    // existing seed at `compute_summary` (`f.name == "main" || address_taken`).
    let mut pinned: HashMap<FnId, Vec<bool>> = HashMap::new();
    for (id, f) in program.iter() {
        let externally_reachable = f.name == "main" || address_taken.contains(&f.name);
        state.insert(*id, vec![Interval::unbounded(); f.params.len()]);
        pinned.insert(*id, vec![externally_reachable; f.params.len()]);
    }

    // Per-`(fn, param)` floor + entry seed, computed ONCE (pure functions of
    // the recurrence shape + literal entries, independent of the iteration).
    let mut floors: HashMap<FnId, Vec<Interval>> = HashMap::new();
    let mut seeds: HashMap<FnId, Vec<Interval>> = HashMap::new();
    for (id, f) in program.iter() {
        let mut fv = Vec::with_capacity(f.params.len());
        let mut sv = Vec::with_capacity(f.params.len());
        for i in 0..f.params.len() {
            fv.push(guard_floor(f, i, edges));
            sv.push(seed_interval(f, i, edges));
        }
        floors.insert(*id, fv);
        seeds.insert(*id, sv);
    }

    // Initialise the iterate at the SEED (the entry envelope) for every
    // non-pinned cell — this is the BOTTOM of the ascending Kleene iteration:
    // the join (`hull`) only ENLARGES, so starting at ⊤ would stay ⊤ forever.
    // The recurrence transfer then joins the floored back-edge descent into
    // this seed each round. A cell whose seed is already ⊤ (unbounded entry)
    // stays ⊤ ⇒ `None` (boxed). Pinned cells keep their ⊤ seed.
    for (id, f) in program.iter() {
        for i in 0..f.params.len() {
            if !pinned[id][i] {
                state.get_mut(id).unwrap()[i] = seeds[id][i];
            }
        }
    }

    // Iterate over SCCs of the static caller→callee graph, callee-before-
    // caller (each SCC runs its own local worklist to a fixpoint). Self-
    // recursive counters (countdown/factorial) are size-1 SCCs with a
    // self-edge; the transfer for index `i` reads only that cell's own state,
    // so the per-SCC fixpoint is self-contained.
    let nodes: Vec<FnId> = program.iter().map(|(id, _)| *id).collect();
    let mut graph: HashMap<FnId, Vec<FnId>> = HashMap::new();
    for edge in edges {
        graph.entry(edge.caller).or_default().push(edge.target);
    }
    // `tarjan_sccs` returns components ordered by least member; the static
    // call graph is a DAG across components, but the transfer never reads
    // another fn's cell in this lean scope, so any order converges. Process
    // each SCC's local fixpoint independently.
    for scc in crate::scc::tarjan_sccs::<FnId>(&nodes, &graph) {
        solve_scc(program, edges, &scc, &seeds, &floors, &pinned, &mut state);
    }

    // Final mapping: `Some(iv)` ONLY for a param that carries a recognized
    // equality-decrement recurrence (a non-⊤ floor) AND whose solved interval
    // `fits_i64`; otherwise `None` (boxed). The floor gate is what keeps this
    // BYTE-IDENTICAL to the old closed-form producer: the old code returned a
    // bound only when `recurrence_for_param` recognized the recurrence AND the
    // reachability gate held — exactly the cells whose `guard_floor` is non-⊤.
    // A non-recurrent param with a finite literal seed (e.g. `twice(10)`,
    // `n + n`) would be a SOUND-but-NEW win; Phase A withholds it (boxes, as
    // today) so the swap is observably identical on every known shape.
    let top = Interval::unbounded();
    let mut out: HashMap<FnId, Vec<Option<Interval>>> = HashMap::new();
    for (id, f) in program.iter() {
        let cells = &state[id];
        let fl = &floors[id];
        let ivs: Vec<Option<Interval>> = (0..f.params.len())
            .map(|i| {
                let iv = cells[i];
                if fl[i] != top && iv.fits_i64() {
                    Some(iv)
                } else {
                    None
                }
            })
            .collect();
        out.insert(*id, ivs);
    }
    out
}

/// Test-only: produce the bare-`i64` param intervals for `program`, rebuilding
/// the `edges` + `address_taken` inputs the same way `compute_summary` does.
/// Lets the golden tests assert the produced interval VALUE (byte-identity),
/// not just `param_is_bare`.
#[cfg(test)]
fn compute_param_intervals_for_test(program: &MirProgram) -> HashMap<FnId, Vec<Option<Interval>>> {
    let mut address_taken: HashSet<String> = HashSet::new();
    for (_, f) in program.iter() {
        collect_fn_values(&f.body.node, &mut address_taken);
    }
    let mut edges: Vec<CallEdge> = Vec::new();
    for (caller, f) in program.iter() {
        collect_call_edges(*caller, &f.body.node, &mut edges);
    }
    compute_bare_param_intervals(program, &edges, &address_taken)
}

/// Run the per-`(fn, param)` interval worklist for one SCC to a fixpoint.
/// Copies the `compute_summary` `loop { changed=false; …; if !changed break }`
/// template. Pinned cells (externally-reachable fns) stay ⊤.
fn solve_scc(
    program: &MirProgram,
    edges: &[CallEdge],
    scc: &[FnId],
    seeds: &HashMap<FnId, Vec<Interval>>,
    floors: &HashMap<FnId, Vec<Interval>>,
    pinned: &HashMap<FnId, Vec<bool>>,
    state: &mut HashMap<FnId, Vec<Interval>>,
) {
    let mut round = 0usize;
    loop {
        let mut changed = false;
        for &fid in scc {
            let Some(f) = program.fn_by_id(fid) else {
                continue;
            };
            for i in 0..f.params.len() {
                if pinned[&fid][i] {
                    continue; // externally reachable — stays ⊤.
                }
                let floor = floors[&fid][i];
                let seed = seeds[&fid][i];

                // The recurrence transfer: JOIN every self-tail back-edge's
                // arg at index `i`, evaluated under the caller env where each
                // counter slot is its CURRENT state interval, the floor
                // applied. Joining ALL back-edges is what makes the multi-
                // back-edge (#538) class structurally impossible — a second
                // growing path is hulled in, never skipped.
                let mut back: Option<Interval> = None;
                for edge in edges {
                    if edge.target != fid || !edge.is_tail_self() {
                        continue;
                    }
                    let Some(arg) = edge.args.get(i) else {
                        continue;
                    };
                    let env = caller_env(f, &state[&fid], floors[&fid].as_slice());
                    let iv = eval_interval_pub(&arg.node, &env);
                    back = Some(match back {
                        Some(prev) => prev.hull(iv),
                        None => iv,
                    });
                }

                // incoming = (seed ⊔ back) ∩ floor — the floor meet EVERY
                // round caps the bounded side so widen never fires on it.
                let pre = match back {
                    Some(b) => seed.hull(b),
                    None => seed,
                };
                let incoming = pre.intersect(floor);
                let joined = state[&fid][i].hull(incoming);
                let next = if round < UNROLL {
                    joined
                } else {
                    state[&fid][i].widen(joined)
                };
                if next != state[&fid][i] {
                    state.get_mut(&fid).unwrap()[i] = next;
                    changed = true;
                }
            }
        }
        round += 1;
        if !changed {
            break;
        }
    }
}

/// Build the caller env for the recurrence transfer. A FLOORED param (its
/// `guard_floor` is a real convex bound, not ⊤) is read at its FLOOR — the
/// path-guaranteed-true superset of that counter's value-set on the recursive
/// branch — so `counter - step` is evaluated over the bounded counter and the
/// descent caps at the floor in a single round (no per-round march that widen
/// would blow to ±inf). A param with NO floor (⊤) is read at its current
/// iterate `state[i]` (the seed-grown value); the floor-intersect there is a
/// no-op (⊤), so it stays the raw state.
fn caller_env(f: &MirFn, cells: &[Interval], floors: &[Interval]) -> HashMap<LocalId, Interval> {
    let top = Interval::unbounded();
    let mut env: HashMap<LocalId, Interval> = HashMap::new();
    for (i, p) in f.params.iter().enumerate() {
        // A FLOORED param is read at its floor — the path-guaranteed bound on
        // the recursive branch (`[K-step, entry]`) — so `counter - step`
        // evaluates over the full bounded range and the descent lands at the
        // floor in ONE round (no per-round march that widen would blow to
        // ±inf). An UNFLOORED param (floor ⊤) is read at its current iterate.
        let iv = if floors[i] == top {
            cells[i]
        } else {
            floors[i]
        };
        env.insert(p.local, iv);
    }
    env
}

/// Wrap `eval_interval` with a fresh `worst` accumulator and return
/// `worst.hull(result)`, preserving the transient-out-of-`i64` demotion: a
/// back-edge arg whose computation passes through an out-of-`i64`
/// intermediate widens the counter out of `i64` ⇒ `!fits_i64` ⇒ boxed.
fn eval_interval_pub(e: &MirExpr, env: &HashMap<LocalId, Interval>) -> Interval {
    let mut worst = Interval::point(0);
    // The recurrence back-edge args are `counter - step` over Int counters; a
    // carrier `.value` projection never appears here, so the carrier facts are
    // empty (a `Project` evaluates to `unbounded()`, the safe decline).
    let no_carriers = FnBareFacts::default();
    let iv = eval_interval(e, env, &mut worst, &no_carriers);
    worst.hull(iv)
}

/// The SEED interval for param `i` of `f`: the convex hull of every literal
/// entry value passed at index `i` by a NON-self-tail caller edge (the entry
/// envelope). A non-literal entry arg, or no entry caller at all, ⇒ ⊤ (the
/// param is unbounded, mapping to `None`). Self-tail edges are the recurrence
/// back-edges (handled by the transfer), never seeds.
fn seed_interval(f: &MirFn, i: usize, edges: &[CallEdge]) -> Interval {
    let mut entry: Option<Interval> = None;
    let mut saw_entry = false;
    for edge in edges {
        if edge.target != f.fn_id || edge.is_tail_self() {
            continue;
        }
        saw_entry = true;
        let Some(arg) = edge.args.get(i) else {
            return Interval::unbounded();
        };
        let Some(iv) = literal_interval(&arg.node) else {
            return Interval::unbounded();
        };
        entry = Some(match entry {
            Some(prev) => prev.hull(iv),
            None => iv,
        });
    }
    match (saw_entry, entry) {
        (true, Some(iv)) => iv,
        _ => Interval::unbounded(),
    }
}

/// The equality-decrement FLOOR for param `i` of `f`: the convex interval
/// `between(K - step, entry_hi)`, installed ONLY when ALL FOUR of the
/// #538/#539/#541/#519 preconditions hold; otherwise ⊤ (no narrowing, the
/// joined descent widens, the param boxes — same as today).
///
/// The four preconditions (each reuses a kept helper):
///   (a) `guard_literal_for` finds an EQUALITY guard `K` on the counter
///       (comparison guards get NO floor in this phase — Phase A boxes them);
///   (b) the `K` base arm is TERMINAL (`!equality_guard_arm_recurses`) — the
///       #541 invariant: a self-recursing base arm never stops at `K`;
///   (c) every self-tail back-edge arg at index `i` is the SAME single
///       `counter - step` monotone decrement (`walk_self_tailcall_steps`
///       agrees on one `step`) — the #538 invariant: disagreeing or growing
///       back-edges leave the floor uninstalled;
///   (d) reachability: `entry >= K && (entry - K) % step == 0` for EVERY
///       entry literal (the #519/BUG-1 invariant) — the descent must LAND on
///       `K`, never step over it.
///
/// When all hold, the floor is `between(K - step, entry_hi)`, reproducing the
/// old `param_recurrence_bound`'s exact `[min(entry.lo, K-step), max(entry.hi,
/// K)]` (the gate guarantees `entry.lo >= K` so `min(entry.lo, K-step)=K-step`
/// and `entry.hi >= K` so `max(entry.hi, K)=entry.hi`) — BYTE-IDENTICAL on
/// every known shape.
fn guard_floor(f: &MirFn, i: usize, edges: &[CallEdge]) -> Interval {
    let unfloored = Interval::unbounded();
    let Some(param_slot) = f.params.get(i).map(|p| p.local) else {
        return unfloored;
    };

    // (a) equality guard K on the counter.
    let Some((guard_k, guard_kind)) = guard_literal_for(&f.body.node, param_slot) else {
        return unfloored;
    };
    if guard_kind != GuardKind::Equality {
        return unfloored; // comparison guard ⇒ no floor in Phase A.
    }

    // (a') the guard K must DOMINATE the recursion: its literal arm and a
    //      self-tail-call must be sibling arms of the SAME `match counter`.
    //      Otherwise `guard_literal_for` may have picked a `K` from an
    //      unrelated `match counter` (a dead binding, a different branch)
    //      while the recursion is stopped by a different base value, making
    //      `[K-step, entry]` fiction and the counter unbounded below.
    if !guard_dominates_recursion(&f.body.node, f.fn_id, param_slot, guard_k) {
        return unfloored;
    }

    // (b) the K base arm must be TERMINAL (not self-recursing).
    if equality_guard_arm_recurses(&f.body.node, f.fn_id, param_slot, guard_k) {
        return unfloored;
    }

    // (c) every self-tail back-edge at index `i` is the SAME single
    //     `counter - step` decrement.
    let mut step: Option<i128> = None;
    let mut saw_self_tail = false;
    let all_same_decrement = walk_self_tailcall_steps(
        &f.body.node,
        f.fn_id,
        param_slot,
        i,
        &mut step,
        &mut saw_self_tail,
    );
    if !saw_self_tail || !all_same_decrement {
        return unfloored;
    }
    let Some(step) = step else {
        return unfloored;
    };

    // (d) reachability per entry literal: `entry >= K && (entry-K)%step == 0`.
    //     We also recompute the entry hull here (the floor's upper endpoint).
    let mut entry: Option<Interval> = None;
    let mut saw_entry = false;
    for edge in edges {
        if edge.target != f.fn_id || edge.is_tail_self() {
            continue;
        }
        saw_entry = true;
        let Some(arg) = edge.args.get(i) else {
            return unfloored;
        };
        let Some(iv) = literal_interval(&arg.node) else {
            return unfloored;
        };
        let v = match iv.lo {
            crate::ir::interval::Bound::Finite(v) => v,
            _ => return unfloored,
        };
        if v < guard_k || (v - guard_k) % step != 0 {
            // The decrement never lands on `K` from this entry — diverges.
            return unfloored;
        }
        entry = Some(match entry {
            Some(prev) => prev.hull(iv),
            None => iv,
        });
    }
    let entry = match (saw_entry, entry) {
        (true, Some(iv)) => iv,
        _ => return unfloored, // no entry caller ⇒ unbounded ⇒ no floor.
    };

    // The convex floor: `[min(entry.lo, K-step), max(entry.hi, K)]`. The gate
    // guarantees every entry literal is `>= K`, so `entry.lo >= K > K-step`
    // and `entry.hi >= K`, hence this equals `[K-step, entry.hi]` — exactly
    // the old `param_recurrence_bound` combine (byte-identical).
    let lo = entry.lo.min(Interval::point(guard_k - step).lo);
    let hi = entry.hi.max(Interval::point(guard_k).hi);
    Interval { lo, hi }
}

/// How the base-case guard `K` compares the counter against the literal.
/// ONLY an `Equality` guard (`match counter { K -> … }` / the lowered
/// `IfThenElse` over `counter == K`) lets the decrement sequence be proven
/// to LAND on `K`; every comparison-direction guard (`<`, `<=`, `>`, `>=`)
/// is a half-bounded test that does not pin the exact stopping value, so it
/// cannot be used to bound the recurrence's far endpoint — those DECLINE.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GuardKind {
    /// `counter == K` (a `Match` literal arm, or `IfThenElse` over `Eq`).
    Equality,
    /// `<`, `<=`, `>`, `>=` against `K` — a half-bounded comparison whose
    /// exact stopping value is unknown. The conservative decline.
    Comparison,
}

/// Walk for a `Match`/`IfThenElse` that guards the counter against a
/// literal `K`. Returns the first literal guard `(K, kind)` found on the
/// counter — `kind` distinguishes an equality guard (the only kind that
/// pins the stopping value) from a comparison guard.
fn guard_literal_for(e: &MirExpr, counter: LocalId) -> Option<(i128, GuardKind)> {
    match e {
        MirExpr::Match(m) => {
            // A `match counter { K -> … }` literal arm is an EQUALITY guard:
            // the arm fires exactly when `counter == K`.
            if subject_is_local(&m.node.subject.node, counter) {
                for arm in &m.node.arms {
                    if let MirPattern::Literal(Literal::Int(k)) = &arm.pattern {
                        return Some((*k as i128, GuardKind::Equality));
                    }
                }
            }
            guard_literal_for(&m.node.subject.node, counter).or_else(|| {
                m.node
                    .arms
                    .iter()
                    .find_map(|arm| guard_literal_for(&arm.body.node, counter))
            })
        }
        MirExpr::IfThenElse(ite) => {
            if let MirExpr::BinOp(b) = &ite.node.cond.node
                && matches!(
                    b.node.op,
                    BinOp::Eq | BinOp::Lt | BinOp::Lte | BinOp::Gt | BinOp::Gte
                )
            {
                // Only `==` pins the exact stopping value; `<`/`<=`/`>`/`>=`
                // are half-bounded tests whose stopping value is unknown.
                let kind = if matches!(b.node.op, BinOp::Eq) {
                    GuardKind::Equality
                } else {
                    GuardKind::Comparison
                };
                if subject_is_local(&b.node.lhs.node, counter)
                    && let MirExpr::Literal(l) = &b.node.rhs.node
                    && let Literal::Int(k) = l.node
                {
                    return Some((k as i128, kind));
                }
                if subject_is_local(&b.node.rhs.node, counter)
                    && let MirExpr::Literal(l) = &b.node.lhs.node
                    && let Literal::Int(k) = l.node
                {
                    return Some((k as i128, kind));
                }
            }
            guard_literal_for(&ite.node.cond.node, counter)
                .or_else(|| guard_literal_for(&ite.node.then_branch.node, counter))
                .or_else(|| guard_literal_for(&ite.node.else_branch.node, counter))
        }
        MirExpr::Let(l) => guard_literal_for(&l.node.value.node, counter)
            .or_else(|| guard_literal_for(&l.node.body.node, counter)),
        _ => {
            let mut found = None;
            visit_children(e, &mut |c| {
                if found.is_none() {
                    found = guard_literal_for(c, counter);
                }
            });
            found
        }
    }
}

/// Walk `e` and validate the arg at index `i` of EVERY self-`TailCall` to
/// `self_fn`. Each such arg MUST be the monotone `counter - step` decrement
/// (a positive literal `step`), and every path must agree on the SAME
/// `step` — the first one seen pins it (recorded in `step`). `saw_self_tail`
/// is set when at least one self-tail-call carries an arg at `i`.
///
/// Returns `false` (and stops contributing) the moment any self-tail-call's
/// arg at `i` is NOT the agreed decrement — a growing `counter + lit`, a
/// different/opposite step, or a non-`counter`-based expr. A `false` result
/// means the param is not a provably-bounded counter and must box
/// (fail-closed). A self-tail-call with too few args (no slot `i`) is
/// skipped: the param at `i` does not participate in that call.
fn walk_self_tailcall_steps(
    e: &MirExpr,
    self_fn: FnId,
    counter: LocalId,
    i: usize,
    step: &mut Option<i128>,
    saw_self_tail: &mut bool,
) -> bool {
    let mut ok = true;
    if let MirExpr::TailCall(tc) = e
        && tc.node.target == self_fn
        && let Some(arg) = tc.node.args.get(i)
    {
        *saw_self_tail = true;
        match decrement_step(&arg.node, counter) {
            // First decrement path pins the step; later paths must match it.
            Some(s) => match *step {
                None => *step = Some(s),
                Some(prev) if prev != s => ok = false,
                Some(_) => {}
            },
            // Not a `counter - lit` decrement (a growth path, a different
            // shape, or not counter-based) ⇒ unbounded recurrence.
            None => ok = false,
        }
    }
    visit_children(e, &mut |c| {
        if !walk_self_tailcall_steps(c, self_fn, counter, i, step, saw_self_tail) {
            ok = false;
        }
    });
    ok
}

/// Does the equality-guard base-case arm (`match counter { K -> body }`, the
/// arm whose literal pattern equals `guard_k`) itself self-recurse?
///
/// `guard_literal_for` treats that literal arm as the counter's STOPPING
/// point, which only holds when the arm TERMINATES. If its body tail-calls
/// `self_fn`, the counter does NOT stop at `K` — it runs past the guard, so
/// the `[K - step, entry]` recurrence bound is fiction and the param can
/// leave `i64` (the recursive-base-arm hole). The caller declines (boxes)
/// when this returns `true`. Fail-closed: the common non-recursive base arm
/// (`match n { 0 -> acc; … }`) returns `false` and stays bare.
fn equality_guard_arm_recurses(
    e: &MirExpr,
    self_fn: FnId,
    counter: LocalId,
    guard_k: i128,
) -> bool {
    if let MirExpr::Match(m) = e
        && subject_is_local(&m.node.subject.node, counter)
    {
        for arm in &m.node.arms {
            if let MirPattern::Literal(Literal::Int(k)) = &arm.pattern
                && *k as i128 == guard_k
                && contains_self_tailcall(&arm.body.node, self_fn)
            {
                return true;
            }
        }
    }
    let mut found = false;
    visit_children(e, &mut |c| {
        if !found {
            found = equality_guard_arm_recurses(c, self_fn, counter, guard_k);
        }
    });
    found
}

/// Does `e` contain a `TailCall` to `self_fn` anywhere in its subtree?
fn contains_self_tailcall(e: &MirExpr, self_fn: FnId) -> bool {
    if let MirExpr::TailCall(tc) = e
        && tc.node.target == self_fn
    {
        return true;
    }
    let mut found = false;
    visit_children(e, &mut |c| {
        if !found {
            found = contains_self_tailcall(c, self_fn);
        }
    });
    found
}

/// Does the equality guard `K` DOMINATE the recursion — does the `K` base case
/// and a self-tail-call live in MUTUALLY-EXCLUSIVE branches of the SAME control
/// node on the counter?
///
/// `guard_literal_for` returns a `K` literal found ANYWHERE on the counter,
/// including a `match counter` that does not gate the recursion at all (a dead
/// `let` binding, an unrelated branch). If the recursion is actually stopped by
/// a DIFFERENT control node (a different base value — e.g. the real base case is
/// `i64::MAX`, not `0`), the `[K-step, entry]` floor is fiction and the counter
/// runs unbounded toward `-inf` (the guard-dominance hole). Requiring the
/// recursive self-tail-call to sit in the `counter != K` branch — and NOT in the
/// `counter == K` (base) branch — proves the counter genuinely descends toward
/// `K` and stops there. Two lowered forms count:
///   - `match counter { K -> base; … rec … }` — the `K` literal arm and a
///     self-tail-call are SIBLING arms.
///   - `if counter == K { base } else { … rec … }` (the lowering of
///     `match counter == K { true -> base; false -> rec }`) — `Eq` is symmetric,
///     so either operand order; the recursion must be in the `else`
///     (`counter != K`) branch and absent from the `then` (`== K`) branch.
///
/// Fail-closed: no dominating node ⇒ no floor ⇒ the descent widens ⇒ boxed.
fn guard_dominates_recursion(e: &MirExpr, self_fn: FnId, counter: LocalId, guard_k: i128) -> bool {
    if let MirExpr::Match(m) = e
        && subject_is_local(&m.node.subject.node, counter)
    {
        let mut has_k_arm = false;
        let mut has_sibling_recursion = false;
        for arm in &m.node.arms {
            let is_k = matches!(
                &arm.pattern,
                MirPattern::Literal(Literal::Int(k)) if *k as i128 == guard_k
            );
            if is_k {
                has_k_arm = true;
            } else if contains_self_tailcall(&arm.body.node, self_fn) {
                has_sibling_recursion = true;
            }
        }
        if has_k_arm && has_sibling_recursion {
            return true;
        }
    }
    // The lowered `match counter == K { true -> base; false -> rec }` form:
    // `IfThenElse { cond: counter == K, then = base, else = rec }`. The
    // recursion must be in the `else` (counter != K) branch and absent from the
    // `then` (counter == K) branch — a self-tail-call under `== K` would recurse
    // AT the base case (unbounded), exactly the recursive-base-arm hole.
    if let MirExpr::IfThenElse(ite) = e
        && let MirExpr::BinOp(b) = &ite.node.cond.node
        && matches!(b.node.op, BinOp::Eq)
        && cond_compares_counter_to_k(&b.node.lhs.node, &b.node.rhs.node, counter, guard_k)
        && contains_self_tailcall(&ite.node.else_branch.node, self_fn)
        && !contains_self_tailcall(&ite.node.then_branch.node, self_fn)
    {
        return true;
    }
    let mut found = false;
    visit_children(e, &mut |c| {
        if !found {
            found = guard_dominates_recursion(c, self_fn, counter, guard_k);
        }
    });
    found
}

/// `true` when `lhs OP rhs` compares the `counter` local against the literal
/// `guard_k` (either operand order). Used by the `Eq`-guard dominance check.
fn cond_compares_counter_to_k(
    lhs: &MirExpr,
    rhs: &MirExpr,
    counter: LocalId,
    guard_k: i128,
) -> bool {
    fn is_k_lit(e: &MirExpr, guard_k: i128) -> bool {
        if let MirExpr::Literal(l) = e
            && let Literal::Int(k) = l.node
        {
            return k as i128 == guard_k;
        }
        false
    }
    (subject_is_local(lhs, counter) && is_k_lit(rhs, guard_k))
        || (subject_is_local(rhs, counter) && is_k_lit(lhs, guard_k))
}

/// If `e` is `counter - K` (a positive literal `K`), return `K`. The only
/// monotone-decrement shape we recognize today.
fn decrement_step(e: &MirExpr, counter: LocalId) -> Option<i128> {
    if let MirExpr::BinOp(b) = e
        && matches!(b.node.op, BinOp::Sub)
        && subject_is_local(&b.node.lhs.node, counter)
        && let MirExpr::Literal(l) = &b.node.rhs.node
        && let Literal::Int(k) = l.node
        && k > 0
    {
        return Some(k as i128);
    }
    None
}

fn subject_is_local(e: &MirExpr, slot: LocalId) -> bool {
    matches!(e, MirExpr::Local(l) if l.node.slot == slot)
}

/// Extract a bounded literal interval from a call argument (a bare `Int`
/// literal, or a negated one). `None` for any non-literal — the
/// conservative decline.
fn literal_interval(arg: &MirExpr) -> Option<Interval> {
    match arg {
        MirExpr::Literal(l) => match l.node {
            Literal::Int(k) => Some(Interval::point(k as i128)),
            _ => None,
        },
        MirExpr::Neg(inner) => match &inner.node {
            MirExpr::Literal(l) => match l.node {
                Literal::Int(k) => Some(Interval::point(-(k as i128))),
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

// ── Per-fn body analysis (range + escape) ───────────────────────────────

/// Compute the per-`LocalId` value facts for `f` given the cross-frame
/// `summary` (which params/returns are bare). The body walk derives an
/// interval per slot (bottom-up over the `Let` chain), an escape predicate
/// (a single use-flow scan), and combines them via `raw_i64_eligible`.
fn analyze_fn(
    f: &MirFn,
    summary: &Summary,
    carrier: &CarrierIntervals,
    field_carrier: &FieldCarrierIntervals,
) -> FnBareFacts {
    let mut facts = FnBareFacts::default();

    // 1. Range: seed param intervals from the summary. A bare param is
    //    seeded with its PROVEN recurrence interval (`[K-step, entry]`) when
    //    one was derived, so an in-body `n - 1` / `acc * n` over the counter
    //    stays in the OverflowFree band; a bare param with no tight interval
    //    falls back to the full-`i64` line (still sound — bare ⟹ confined to
    //    `i64` by construction); a boxed param is unbounded.
    //
    //    ETAP-2 carrier-`i64` — carrier params (wasm-gc only). A param whose
    //    annotated type is a refinement-via-opaque carrier
    //    (`carrier_interval(&p.ty, carrier)` is `Some`) has its wasm storage
    //    ALREADY erased to a native `i64` (the registry's `eligible_carriers`
    //    set, applied uniformly by `aver_to_wasm`), INDEPENDENT of this Int-
    //    bareness analysis. So a carrier param is NOT an Int-bare slot — the
    //    value `c` is the carrier (a record), never a raw Int operand — and we
    //    deliberately do NOT mark it `bare_params` / `Repr::Bare`: a carrier
    //    value flows to a carrier param with NO conversion (both are i64), so
    //    flagging it bare would make the call site spuriously `Unbox` the i64.
    //    What the carrier param DOES enable is a raw `.value` read: a
    //    `Project(Local(carrier_slot), "value")` reads the i64 directly (no
    //    `$AverInt` project bridge) and contributes the carrier's PROVEN bound
    //    to the surrounding arithmetic. We record that bound in `carrier_slots`
    //    so `bare_expr_interval` / `expr_is_bare_i64` treat the `.value` read
    //    as a bare leaf — `c.value + c.value` over a `[0,100]` carrier then
    //    stays OverflowFree, while `c.value * c.value` over a `[0,2^40]`
    //    carrier overflows i64 and the compound declines (boxed) — the C0
    //    soundness gate is the SAME interval fixpoint, just with a carrier-
    //    projection leaf source. The bound is sound by construction: the type
    //    is opaque + the only constructor is the guarded smart constructor, so
    //    every inhabitant provably lies in the interval.
    let mut intervals: HashMap<LocalId, Interval> = HashMap::new();
    let mut bare_params = vec![false; f.params.len()];
    let mut carrier_slots: HashMap<LocalId, Interval> = HashMap::new();
    for (i, p) in f.params.iter().enumerate() {
        let recurrence_bare = summary.param_bare(f.fn_id, i);
        bare_params[i] = recurrence_bare;
        if let Some(cv) = carrier_interval(&p.ty, carrier) {
            // Carrier param: its `.value` reads raw i64 with this proven bound.
            // The param value `c` itself stays a (boxed/struct) carrier slot —
            // not an Int-bare slot — so its `intervals` entry is unbounded
            // (it is never an arithmetic operand directly).
            carrier_slots.insert(p.local, cv);
            intervals.insert(p.local, Interval::unbounded());
            continue;
        }
        let iv = if recurrence_bare {
            summary
                .param_interval(f.fn_id, i)
                .filter(|iv| iv.fits_i64())
                .unwrap_or_else(|| Interval::between(i64::MIN as i128, i64::MAX as i128))
        } else {
            Interval::unbounded()
        };
        intervals.insert(p.local, iv);
    }
    // Record the carrier-projection facts on the result so the rewrite + emit
    // can recognize a bare carrier's `.value` read. (Set before the combine so
    // `tail_value_is_bare` / `expr_is_bare_i64` see them via `&facts`.)
    facts.carrier_slots = carrier_slots;
    // The eligible-carrier type table powers the NESTED carrier-field
    // recognition (`Project(Project(rec, "coord"), "value")` whose inner
    // `rec.coord` is stamped an eligible carrier). EMPTY on the Rust backend /
    // no-eligible-carrier baseline, so the nested form never fires there.
    facts.carrier_types = carrier.clone();
    // ETAP-2 multi-field carrier-`i64`: the eligible `(record, field)` bound
    // table powers the DIRECT bounded-field recognition (`Project(rec, "x")`
    // whose `rec` is a bounded record and `(record, "x")` is eligible). EMPTY
    // on the Rust backend / no-eligible-record baseline, so the direct-field
    // form never fires there. Set BEFORE the let-chain walk so a field read
    // bound to a slot is recognized as a bare leaf.
    facts.field_carrier_intervals = field_carrier.clone();

    // 2. Walk the body's `Let` chain, deriving an interval (+ worst-join
    //    op class) for each bound slot. The carrier-projection facts let the
    //    walk read a bare carrier's `.value` (param-level OR nested field) as
    //    its proven i64 interval.
    let mut op_classes: HashMap<LocalId, OpClass> = HashMap::new();
    walk_let_chain(&f.body.node, &mut intervals, &mut op_classes, &facts);

    // 3. Escape: a single use-flow scan. A slot escapes if any use-site is
    //    a general-Int context.
    let mut escaping: HashSet<LocalId> = HashSet::new();
    scan_escapes(&f.body.node, summary, &mut escaping);

    // 4. Combine.
    for (slot, iv) in &intervals {
        let op_class = op_classes
            .get(slot)
            .copied()
            .unwrap_or(OpClass::OverflowFree);
        let escapes = escaping.contains(slot);
        let eligible = raw_i64_eligible(Some(*iv), std::iter::once(&op_class));
        let repr = if eligible && !escapes {
            Repr::Bare
        } else {
            Repr::Boxed
        };
        facts.values.insert(
            *slot,
            ValueFact {
                interval: Some(*iv),
                op_class,
                escapes,
                repr,
            },
        );
    }
    // Params absent from the Let walk still get a fact (their seed). Every
    // param is in `intervals`, so the combine above already inserted its
    // value fact — this `or_insert_with` is a backstop for any param the
    // combine somehow skipped. The recurrence path keeps its `summary` seed;
    // a carrier param is NOT Int-bare (its `intervals` entry is unbounded), so
    // the backstop boxes it — the value `c` is a carrier slot, only its
    // `.value` read goes raw (via `carrier_slots`).
    for (i, p) in f.params.iter().enumerate() {
        let seeded_bare = summary.param_bare(f.fn_id, i);
        facts.values.entry(p.local).or_insert_with(|| {
            if seeded_bare {
                ValueFact {
                    interval: intervals.get(&p.local).copied(),
                    op_class: OpClass::OverflowFree,
                    escapes: false,
                    repr: Repr::Bare,
                }
            } else {
                ValueFact::boxed()
            }
        });
    }
    facts.bare_params = bare_params;

    // 5. Return: bare iff the summary says so AND the body's tail value is
    //    itself bare-eligible.
    facts.bare_return =
        summary.return_bare(f.fn_id) && tail_value_is_bare(&f.body.node, &facts, &escaping);

    facts
}

/// Walk the `Let` chain, recording each bound slot's interval and worst-
/// join op class. `env` holds param + already-bound intervals and is grown
/// in place; branches recurse so nested bindings are seen. `facts` carries
/// the carrier-projection facts (`carrier_slots` + `carrier_types`) so a bare
/// carrier's `.value` read (`Project`, param-level OR nested field) contributes
/// its proven interval to an arithmetic binding.
fn walk_let_chain(
    e: &MirExpr,
    env: &mut HashMap<LocalId, Interval>,
    op_classes: &mut HashMap<LocalId, OpClass>,
    facts: &FnBareFacts,
) {
    match e {
        MirExpr::Let(l) => {
            let mut worst = Interval::point(0);
            let iv = eval_interval(&l.node.value.node, env, &mut worst, facts);
            env.insert(l.node.binding, iv);
            // The binding's op class is the worst-join over its value
            // expression (so a transient out-of-i64 intermediate demotes).
            op_classes.insert(l.node.binding, OpClass::of_interval(worst.hull(iv)));
            walk_let_chain(&l.node.value.node, env, op_classes, facts);
            walk_let_chain(&l.node.body.node, env, op_classes, facts);
        }
        // A `match subject { y -> … }` Ident-binding arm ALIASES the subject:
        // the binder `y` carries the subject's interval. Without this, a
        // base-case `match n { y -> y }` (subj_ret) leaves `y` unknown, so the
        // body pass can't prove `y` bare and the return-repr / escape facts
        // disagree with codegen (which declares `y` at the subject's bare
        // type). Recognize the alias so `y` inherits `n`'s bound — the same
        // representation codegen emits. The subject's interval is evaluated in
        // the current `env` (the subject is already in scope).
        MirExpr::Match(m) => {
            let mut worst = Interval::point(0);
            let subj_iv = eval_interval(&m.node.subject.node, env, &mut worst, facts);
            walk_let_chain(&m.node.subject.node, env, op_classes, facts);
            for arm in &m.node.arms {
                if let MirPattern::Bind(slot, _) = &arm.pattern {
                    // Alias: the binder takes the subject's interval + op
                    // class (the subject is read verbatim, no new arithmetic).
                    env.entry(*slot).or_insert(subj_iv);
                    op_classes
                        .entry(*slot)
                        .or_insert_with(|| OpClass::of_interval(subj_iv));
                }
                walk_let_chain(&arm.body.node, env, op_classes, facts);
            }
        }
        _ => {
            visit_children(e, &mut |c| walk_let_chain(c, env, op_classes, facts));
        }
    }
}

/// Abstractly evaluate a MIR expression to its interval, joining each
/// arithmetic node into `worst` (so a transient out-of-i64 intermediate
/// demotes the binding). Unknown shapes evaluate to `unbounded()`. `facts`
/// carries the carrier-projection facts so a `.value` read over a bare carrier
/// — a param/local (`carrier_slots`) OR a nested carrier-field read
/// (`Project(Project(..))` whose inner type is an eligible carrier) — evaluates
/// to the carrier's proven bound rather than `unbounded()`, the
/// carrier-projection leaf.
fn eval_interval(
    e: &MirExpr,
    env: &HashMap<LocalId, Interval>,
    worst: &mut Interval,
    facts: &FnBareFacts,
) -> Interval {
    match e {
        MirExpr::Literal(l) => match l.node {
            Literal::Int(k) => Interval::point(k as i128),
            _ => Interval::unbounded(),
        },
        MirExpr::Local(local) => env
            .get(&local.node.slot)
            .copied()
            .unwrap_or_else(Interval::unbounded),
        // ETAP-2 carrier-`i64`: `c.value` over a bare carrier (param/local or a
        // nested carrier field) reads the native i64 with the carrier's proven
        // bound; any other `Project` declines to `unbounded()`.
        MirExpr::Project(_) => facts
            .carrier_project_interval(e)
            .unwrap_or_else(Interval::unbounded),
        MirExpr::Neg(inner) => {
            let r = Interval::point(0).sub(eval_interval(&inner.node, env, worst, facts));
            *worst = worst.hull(r);
            r
        }
        MirExpr::BinOp(b) => {
            let l = eval_interval(&b.node.lhs.node, env, worst, facts);
            let r = eval_interval(&b.node.rhs.node, env, worst, facts);
            let result = match b.node.op {
                BinOp::Add => l.add(r),
                BinOp::Sub => l.sub(r),
                BinOp::Mul => l.mul(r),
                _ => Interval::unbounded(),
            };
            *worst = worst.hull(result);
            result
        }
        _ => Interval::unbounded(),
    }
}

/// Single use-flow escape scan. A slot escapes if it (a) is passed to a
/// general (boxed) Int param of a callee, (b) is stored in an aggregate,
/// or (c) is stringified. We over-approximate: any unrecognized use of a
/// slot defaults to escaping.
fn scan_escapes(e: &MirExpr, summary: &Summary, out: &mut HashSet<LocalId>) {
    match e {
        // Aggregates: every Int element/field escapes — INCLUDING a bare
        // leaf reached through a `BinOp`/`Neg` (the aggregate emit does not
        // convert a bare compound), hence `*_deep` (BUG 3).
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for it in items {
                mark_operand_escapes_deep(&it.node, out);
                scan_escapes(&it.node, summary, out);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                mark_operand_escapes_deep(&k.node, out);
                mark_operand_escapes_deep(&v.node, out);
                scan_escapes(&k.node, summary, out);
                scan_escapes(&v.node, summary, out);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                mark_operand_escapes_deep(&a.node, out);
                scan_escapes(&a.node, summary, out);
            }
        }
        MirExpr::RecordCreate(r) => {
            for fld in &r.node.fields {
                mark_operand_escapes_deep(&fld.value.node, out);
                scan_escapes(&fld.value.node, summary, out);
            }
        }
        MirExpr::RecordUpdate(u) => {
            mark_operand_escapes_deep(&u.node.base.node, out);
            scan_escapes(&u.node.base.node, summary, out);
            for fld in &u.node.updates {
                mark_operand_escapes_deep(&fld.value.node, out);
                scan_escapes(&fld.value.node, summary, out);
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for it in &ip.node.items {
                mark_operand_escapes_deep(&it.node, out);
                scan_escapes(&it.node, summary, out);
            }
        }
        // Stringification: every embedded value escapes (including a bare
        // leaf inside a `BinOp`/`Neg` — the interpolation emit stringifies
        // the whole compound, not the operands).
        MirExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let super::super::expr::MirStrPart::Expr(ex) = p {
                    mark_operand_escapes_deep(&ex.node, out);
                    scan_escapes(&ex.node, summary, out);
                }
            }
        }
        // A call: an arg at a BOXED callee-param index escapes; an arg at a
        // BARE param index converts at the boundary and does NOT escape. A
        // builtin / intrinsic / fn-value callee is conservative — every Int
        // arg escapes (we don't model builtin param reps).
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(target) => {
                for (i, a) in c.node.args.iter().enumerate() {
                    // A user-fn boxed-param position: the boxed-arithmetic
                    // emit converts a bare compound operand, so only a DIRECT
                    // bare `Local` escapes here (shallow), not a leaf buried
                    // in a `BinOp` (which `boxed_int_operand` converts).
                    if !summary.param_bare(target, i) {
                        mark_operand_escapes(&a.node, out);
                    }
                    scan_escapes(&a.node, summary, out);
                }
            }
            // A builtin / intrinsic / fn-value callee is a NON-converting
            // position: its Int args emit without `boxed_int_operand`, so a
            // bare leaf inside a `BinOp`/`Neg` arg (`String.fromInt(n + 1)`)
            // escapes too — `*_deep` (BUG 3).
            _ => {
                for a in &c.node.args {
                    mark_operand_escapes_deep(&a.node, out);
                    scan_escapes(&a.node, summary, out);
                }
            }
        },
        // A tail call: same, against the (self) callee's bare params.
        MirExpr::TailCall(tc) => {
            for (i, a) in tc.node.args.iter().enumerate() {
                if !summary.param_bare(tc.node.target, i) {
                    mark_operand_escapes(&a.node, out);
                }
                scan_escapes(&a.node, summary, out);
            }
        }
        // Control flow + arithmetic: recurse. A bare Local in tail/return
        // or arithmetic-operand position is consumed bare (no escape).
        _ => {
            visit_children(e, &mut |c| scan_escapes(c, summary, out));
        }
    }
}

/// Mark the slot of a value flowing into a USER-FN-CALL boxed-param
/// position (an ordinary `Call(Fn)` / `TailCall` arg at a boxed index).
///
/// Only a bare `Local` read placed verbatim there escapes — its
/// representation must then be `AverInt` (the value itself crosses the
/// boundary). An ARITHMETIC expression (`acc * n`) at such a position does
/// NOT escape its operands: the boxed-arithmetic emit (`boxed_int_operand`)
/// converts each bare operand with `from_i64` and the FRESH result is what
/// crosses, so the operand keeps its own bare representation. Flagging a
/// counter read inside `acc * n` was the bug that demoted the factorial
/// counter — hence this variant does NOT recurse into `BinOp` / `Neg`.
fn mark_operand_escapes(e: &MirExpr, out: &mut HashSet<LocalId>) {
    if let MirExpr::Local(l) = e {
        out.insert(l.node.slot);
    }
}

/// Mark every bare-`Local` leaf reaching a NON-CONVERTING escaping
/// position — an aggregate element/field (`[n + 1]`, a tuple/map/record
/// slot), a stringify embed, or a builtin/intrinsic Int arg
/// (`String.fromInt(n + 1)`).
///
/// BUG 3: unlike the user-fn boxed-param position, these emit sites do NOT
/// run `boxed_int_operand`, so a bare compound like `n + 1` is emitted as
/// raw `(n + 1)i64` straight into an `AverInt`-typed slot — a `rustc` type
/// mismatch, and (without `overflow-checks`) a wrong value if the compound
/// could wrap. So a bare leaf reaching these positions THROUGH a `BinOp` /
/// `Neg` tree must escape (→ boxed), exactly like a direct `Local` does.
/// Hence this variant RECURSES into `BinOp` / `Neg` (a literal leaf carries
/// no slot, so it is harmlessly ignored).
fn mark_operand_escapes_deep(e: &MirExpr, out: &mut HashSet<LocalId>) {
    match e {
        MirExpr::Local(l) => {
            out.insert(l.node.slot);
        }
        MirExpr::Neg(inner) => mark_operand_escapes_deep(&inner.node, out),
        MirExpr::BinOp(b) => {
            mark_operand_escapes_deep(&b.node.lhs.node, out);
            mark_operand_escapes_deep(&b.node.rhs.node, out);
        }
        _ => {}
    }
}

/// Does the fn's tail value evaluate to a bare-eligible value? The tail is
/// the base-case value(s) reached after the `Let` chain / branches. We
/// require every reachable tail leaf to be a bare-eligible Local or a bare
/// literal; anything else demotes the return.
fn tail_value_is_bare(e: &MirExpr, facts: &FnBareFacts, escaping: &HashSet<LocalId>) -> bool {
    match e {
        MirExpr::Local(l) => facts.is_bare(l.node.slot) && !escaping.contains(&l.node.slot),
        MirExpr::Literal(l) => matches!(l.node, Literal::Int(_)),
        MirExpr::Let(let_node) => tail_value_is_bare(&let_node.node.body.node, facts, escaping),
        MirExpr::Match(m) => m
            .node
            .arms
            .iter()
            .all(|arm| tail_value_is_bare(&arm.body.node, facts, escaping)),
        MirExpr::IfThenElse(ite) => {
            tail_value_is_bare(&ite.node.then_branch.node, facts, escaping)
                && tail_value_is_bare(&ite.node.else_branch.node, facts, escaping)
        }
        // A self-tail-call's value is the recurrence, not a base value; it
        // does not constrain the return repr.
        MirExpr::TailCall(_) => true,
        MirExpr::Return(inner) => tail_value_is_bare(&inner.node, facts, escaping),
        // A compound `Add`/`Sub`/`Mul` tail value is bare ONLY when its
        // RESULT interval provably fits `i64` (BUG 2): a tree whose operands
        // are each bare can still overflow (`n + i64::MAX`), so route through
        // the interval-checked `expr_is_bare_i64` — the same gate codegen
        // uses, so analysis and emit never disagree. (A `Bare` leaf already
        // carries `escapes == false`, so an escaping operand is `Boxed` and
        // `expr_is_bare_i64` declines it.)
        MirExpr::BinOp(b) if matches!(b.node.op, BinOp::Add | BinOp::Sub | BinOp::Mul) => {
            facts.expr_is_bare_i64(e)
        }
        // ETAP-2 carrier-`i64`: a tail `c.value` over a bare carrier reads raw
        // i64, so the fn may return bare i64 (skip the project bridge).
        MirExpr::Project(_) => facts.is_carrier_project(e),
        _ => false,
    }
}

// ── shared traversal + helpers ──────────────────────────────────────────

fn collect_fn_values(e: &MirExpr, out: &mut HashSet<String>) {
    if let MirExpr::FnValue(name) = e {
        out.insert(name.clone());
    }
    visit_children(e, &mut |c| collect_fn_values(c, out));
}

/// `true` when the source type-annotation string is exactly `Int`.
fn ty_str_is_int(ty: &str) -> bool {
    ty == "Int"
}

/// `true` when the `Type` stamp is exactly `Int`. Exposed for the Rust
/// walker's consumption sites.
pub fn type_is_int(ty: Option<&Type>) -> bool {
    matches!(ty, Some(Type::Int))
}

/// Test-only re-export of the immediate-children walk, so sibling-module
/// tests (`bare_i64_rewrite`) can recurse without duplicating the match.
#[cfg(test)]
pub(crate) fn tests_visit_children(e: &MirExpr, f: &mut dyn FnMut(&MirExpr)) {
    visit_children(e, f)
}

/// Apply `f` to every immediate sub-expression of `e`. Kept in sync with
/// the exhaustive walk in `own_param.rs` / `instantiations.rs`. `pub(crate)`
/// so the sibling `bare_i64_rewrite` pass (the wasm-gc `mutual_recursion_box_set`
/// call-graph walk) can reuse this one exhaustive child enumeration instead
/// of forking a second copy.
pub(crate) fn visit_children(e: &MirExpr, f: &mut dyn FnMut(&MirExpr)) {
    match e {
        MirExpr::Literal(_) | MirExpr::Local(_) | MirExpr::FnValue(_) => {}
        MirExpr::Let(l) => {
            f(&l.node.value.node);
            f(&l.node.body.node);
        }
        MirExpr::Call(c) => {
            for a in &c.node.args {
                f(&a.node);
            }
        }
        MirExpr::TailCall(tc) => {
            for a in &tc.node.args {
                f(&a.node);
            }
        }
        MirExpr::BinOp(b) => {
            f(&b.node.lhs.node);
            f(&b.node.rhs.node);
        }
        MirExpr::Neg(inner)
        | MirExpr::Try(inner)
        | MirExpr::Return(inner)
        | MirExpr::Box(inner)
        | MirExpr::Unbox(inner) => f(&inner.node),
        MirExpr::Match(m) => {
            f(&m.node.subject.node);
            for arm in &m.node.arms {
                f(&arm.body.node);
            }
        }
        MirExpr::Construct(c) => {
            for a in &c.node.args {
                f(&a.node);
            }
        }
        MirExpr::RecordCreate(r) => {
            for field in &r.node.fields {
                f(&field.value.node);
            }
        }
        MirExpr::RecordUpdate(u) => {
            f(&u.node.base.node);
            for field in &u.node.updates {
                f(&field.value.node);
            }
        }
        MirExpr::Project(p) => f(&p.node.base.node),
        MirExpr::IfThenElse(ite) => {
            f(&ite.node.cond.node);
            f(&ite.node.then_branch.node);
            f(&ite.node.else_branch.node);
        }
        MirExpr::List(items) | MirExpr::Tuple(items) => {
            for i in items {
                f(&i.node);
            }
        }
        MirExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                f(&k.node);
                f(&v.node);
            }
        }
        MirExpr::InterpolatedStr(parts) => {
            for p in parts {
                if let super::super::expr::MirStrPart::Expr(e) = p {
                    f(&e.node);
                }
            }
        }
        MirExpr::IndependentProduct(ip) => {
            for i in &ip.node.items {
                f(&i.node);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::mir::{lower_program, optimize};
    use crate::source::parse_source;

    fn facts_for(src: &str) -> (MirProgram, BareI64Facts) {
        facts_for_with_carrier(src, &CarrierIntervals::new())
    }

    /// Like [`facts_for`] but threads a caller-supplied carrier table —
    /// used by the carrier-slot tests. The empty-table caller
    /// ([`facts_for`]) reproduces the pre-slice behavior exactly.
    fn facts_for_with_carrier(src: &str, carrier: &CarrierIntervals) -> (MirProgram, BareI64Facts) {
        let mut items = parse_source(src).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        assert!(
            result
                .typecheck
                .as_ref()
                .is_none_or(|t| t.errors.is_empty()),
            "typecheck errors: {:?}",
            result.typecheck.map(|t| t.errors)
        );
        let mir_items: Vec<crate::ir::hir::ResolvedTopLevel> = result.resolved_items.clone();
        let program = optimize(lower_program(&mir_items));
        let facts = analyze(&program, carrier);
        (program, facts)
    }

    /// Build the carrier-interval table for `src` through the same
    /// refinement-via-opaque derivation the codegen entries use. Returns
    /// the parsed items + symbol table the table borrows from, so the
    /// caller can keep them alive while running `analyze`.
    fn carrier_table_for(
        items: &[crate::ast::TopLevel],
        symbols: &crate::ir::SymbolTable,
    ) -> CarrierIntervals {
        let empty_prefixes: HashSet<String> = HashSet::new();
        let empty_recursive: HashSet<crate::ir::FnId> = HashSet::new();
        let inputs = crate::codegen::proof_lower::ProofLowerInputs {
            entry_items: items,
            dep_modules: &[],
            module_prefixes: &empty_prefixes,
            recursive_fns: &empty_recursive,
            symbol_table: symbols,
            program_shape: None,
        };
        crate::codegen::proof_lower::carrier_interval_table(&inputs)
    }

    fn fn_id_by_name<'a>(program: &'a MirProgram, name: &str) -> crate::ir::FnId {
        program
            .iter()
            .find(|(_, f)| f.name == name)
            .map(|(id, _)| *id)
            .unwrap_or_else(|| panic!("fn `{name}` not in program"))
    }

    /// ETAP-2 SLICE 0+1 carrier-slot source. `carrier` seeds the bound;
    /// the empty-carrier variant is the revert-test baseline.
    const CARRIER_SRC: &str = r#"
module Toy
    intent = "t"
    depends []

record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("err")

fn toInt(c: IntRange) -> Int
    c.value

fn doubled(c: IntRange) -> Int
    c.value + c.value

fn main() -> Int
    match fromInt(50)
        Result.Ok(c)  -> toInt(c) + doubled(c)
        Result.Err(_) -> 0
"#;

    /// Drive `CARRIER_SRC` through the pipeline + carrier table; return the
    /// MIR program + analysis facts.
    fn carrier_facts(carrier_on: bool) -> (MirProgram, BareI64Facts) {
        let mut items = parse_source(CARRIER_SRC).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        let carrier = if carrier_on {
            carrier_table_for(&items, &result.symbol_table)
        } else {
            CarrierIntervals::new()
        };
        let mir_items = result.resolved_items.clone();
        let program = optimize(lower_program(&mir_items));
        let facts = analyze(&program, &carrier);
        (program, facts)
    }

    #[test]
    fn bare_named_type_extracts_name_from_debug() {
        // The lowerer fills `MirParam.ty` with `format!("{:?}", Type)`, so a
        // named carrier renders as the Debug form. Pin the extractor.
        assert_eq!(
            bare_named_type("Named { id: Some(TypeId(0)), name: \"IntRange\" }"),
            Some("IntRange")
        );
        assert_eq!(
            bare_named_type("Named { id: None, name: \"Natural\" }"),
            Some("Natural")
        );
        // Non-Named (primitive / compound) debug strings decline.
        assert_eq!(bare_named_type("Int"), None);
        assert_eq!(
            bare_named_type("Result(Named { id: None, name: \"X\" }, Str)"),
            None
        );
    }

    #[test]
    fn carrier_interval_table_derives_proven_bound() {
        // The table is keyed by the bare carrier type name and carries the
        // exact `[0, 100]` bound the proof side persists — byte-identical
        // derivation, fail-closed on an unrecognized invariant.
        let mut items = parse_source(CARRIER_SRC).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        let table = carrier_table_for(&items, &result.symbol_table);
        let (iv, known) = table.get("IntRange").copied().expect("IntRange in table");
        assert!(known, "the [0,100] invariant is recognized");
        assert!(iv.fits_i64(), "the carrier bound fits i64");
        assert_eq!(iv, Interval::between(0, 100), "exact proven carrier bound");
    }

    #[test]
    fn carrier_seam_enabled_records_carrier_projection_slots() {
        // The carrier-`i64` arithmetic seam is ON (`CARRIER_BARE_ELIGIBLE ==
        // true`). A carrier param is NOT marked an Int-bare param/slot — the
        // value `c` is the carrier (i64 STORAGE, never a raw Int operand), so
        // flagging it bare would make a call site spuriously `Unbox` it. What
        // the seam DOES is record the carrier param's slot in `carrier_slots`
        // (with its proven bound) so a `c.value` read renders raw i64. Pin
        // both: param NOT Int-bare, slot recorded as a carrier-projection
        // source with the `[0,100]` bound.
        assert!(
            CARRIER_BARE_ELIGIBLE,
            "this test pins the enabled carrier-arithmetic seam"
        );
        let (program, facts) = carrier_facts(true);
        for name in ["toInt", "doubled"] {
            let id = fn_id_by_name(&program, name);
            let f = facts.for_fn(id).expect("carrier facts");
            assert!(
                !f.param_is_bare(0),
                "`{name}`'s carrier param is NOT an Int-bare param (no spurious \
                 Unbox at the call site)"
            );
            let pf = program.fn_by_id(id).expect("mir fn");
            let carrier_slot = pf.params[0].local;
            assert_eq!(
                f.carrier_slots.get(&carrier_slot).copied(),
                Some(Interval::between(0, 100)),
                "`{name}`'s carrier param is a carrier-projection source with the \
                 proven [0,100] bound"
            );
        }
    }

    #[test]
    fn carrier_off_table_records_no_carrier_slots() {
        // With the carrier table EMPTY (the Rust backend / no-eligible-carrier
        // baseline), no carrier-projection slot is recorded — the byte-
        // identical pre-slice behavior.
        let (program, facts) = carrier_facts(false);
        for name in ["toInt", "doubled"] {
            let id = fn_id_by_name(&program, name);
            let f = facts.for_fn(id).expect("carrier facts");
            assert!(
                f.carrier_slots.is_empty(),
                "with the carrier table empty, `{name}` records no carrier slot"
            );
        }
    }

    /// NESTED carrier-FIELD source: `Holder { c: IntRange }` whose `nestedAdd`
    /// reads `h.c.value + h.c.value` (a `Project(Project(..))`). The base
    /// `h.c` is stamped the eligible carrier `IntRange`, so the analysis treats
    /// each nested `.value` as a raw i64 leaf at the `[0,100]` bound and the sum
    /// stays OverflowFree (bare); a wide `0..2^40` carrier's `c.value * c.value`
    /// overflows i64 and DECLINES (boxed) — the same fixpoint, fed the nested
    /// leaf. With the carrier table OFF, the nested form never fires.
    const NESTED_CARRIER_SRC: &str = r#"
module Toy
    intent = "t"
    depends []

record IntRange
    value: Int

record Holder
    c: IntRange

record Wide
    value: Int

record WideHolder
    c: Wide

fn fromInt(n: Int) -> Result<IntRange, String>
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err("err")

fn fromWide(n: Int) -> Result<Wide, String>
    match Bool.and(n >= 0, n <= 1099511627776)
        true  -> Result.Ok(Wide(value = n))
        false -> Result.Err("err")

fn nestedAdd(h: Holder) -> Int
    h.c.value + h.c.value

fn nestedWideMul(h: WideHolder) -> Int
    h.c.value * h.c.value

fn main() -> Int
    0
"#;

    fn nested_carrier_facts(carrier_on: bool) -> (MirProgram, BareI64Facts) {
        let mut items = parse_source(NESTED_CARRIER_SRC).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        assert!(
            result
                .typecheck
                .as_ref()
                .is_none_or(|t| t.errors.is_empty()),
            "typecheck errors: {:?}",
            result.typecheck.map(|t| t.errors)
        );
        let carrier = if carrier_on {
            carrier_table_for(&items, &result.symbol_table)
        } else {
            CarrierIntervals::new()
        };
        let mir_items = result.resolved_items.clone();
        let program = optimize(lower_program(&mir_items));
        let facts = analyze(&program, &carrier);
        (program, facts)
    }

    /// Find the `Project(Project(..), _)` nested-field read inside `e` and
    /// query the carrier-projection recognizer on it.
    fn find_nested_project_interval(e: &MirExpr, facts: &FnBareFacts) -> Option<Interval> {
        if let MirExpr::Project(p) = e
            && matches!(p.node.base.node, MirExpr::Project(_))
            && let Some(iv) = facts.carrier_project_interval(e)
        {
            return Some(iv);
        }
        let mut found = None;
        visit_children(e, &mut |c| {
            if found.is_none() {
                found = find_nested_project_interval(c, facts);
            }
        });
        found
    }

    #[test]
    fn nested_carrier_field_in_range_is_bare_leaf() {
        // The carrier table is ON: the nested `h.c.value` read is recognized as
        // a raw i64 leaf carrying the carrier's proven `[0,100]` bound, so the
        // `+` over it is OverflowFree (the binding goes bare).
        let (program, facts) = nested_carrier_facts(true);
        let id = fn_id_by_name(&program, "nestedAdd");
        let f = facts.for_fn(id).expect("nestedAdd facts");
        let pf = program.fn_by_id(id).expect("mir fn");
        let iv = find_nested_project_interval(&pf.body.node, f)
            .expect("the nested h.c.value read is recognized as a carrier projection");
        assert_eq!(
            iv,
            Interval::between(0, 100),
            "the nested-field read carries the carrier's proven [0,100] bound"
        );
        // The whole `h.c.value + h.c.value` tree is a bare i64 expression.
        let sum =
            find_carrier_sum(&pf.body.node).expect("the body contains the `c.value + c.value` add");
        assert!(
            f.expr_is_bare_i64(sum),
            "in-range nested-field sum is bare-i64 eligible (OverflowFree)"
        );
    }

    #[test]
    fn nested_carrier_field_off_table_declines() {
        // With the carrier table EMPTY, the nested form never fires — the read
        // is not recognized as a carrier projection (fail-closed → boxed).
        let (program, facts) = nested_carrier_facts(false);
        let id = fn_id_by_name(&program, "nestedAdd");
        let f = facts.for_fn(id).expect("nestedAdd facts");
        let pf = program.fn_by_id(id).expect("mir fn");
        assert!(
            find_nested_project_interval(&pf.body.node, f).is_none(),
            "with the carrier table empty, a nested-field read is NOT a carrier projection"
        );
    }

    #[test]
    fn nested_carrier_field_wide_mul_declines() {
        // A wide `0..2^40` nested-field carrier: `h.c.value * h.c.value` reaches
        // up to 2^80, which overflows i64. Even though each nested read IS a
        // recognized carrier projection, the PRODUCT leaves i64, so the compound
        // declines to boxed — the same C0 gate as the param-level form.
        let (program, facts) = nested_carrier_facts(true);
        let id = fn_id_by_name(&program, "nestedWideMul");
        let f = facts.for_fn(id).expect("nestedWideMul facts");
        let pf = program.fn_by_id(id).expect("mir fn");
        // The nested leaf IS recognized (carries the wide bound) …
        let iv = find_nested_project_interval(&pf.body.node, f)
            .expect("the nested wide read is still a recognized carrier projection");
        assert!(iv.fits_i64(), "the [0,2^40] carrier bound itself fits i64");
        // … but the `*` product does NOT, so the compound is NOT bare-i64.
        let mul = find_carrier_sum(&pf.body.node)
            .expect("the body contains the `c.value * c.value` multiply");
        assert!(
            !f.expr_is_bare_i64(mul),
            "the wide-bound nested-field multiply overflows i64 and stays boxed"
        );
    }

    /// Find the `Add`/`Mul` `BinOp` whose operands are nested-field carrier
    /// reads (the `c.value + c.value` / `c.value * c.value` body). Manual
    /// recursion over the tail-bearing node shapes (the body is a single arith
    /// node, possibly wrapped in `Return`/`Let`/`Match`), returning a borrowed
    /// reference (so it cannot route through the `visit_children` closure).
    fn find_carrier_sum(e: &MirExpr) -> Option<&MirExpr> {
        match e {
            MirExpr::BinOp(b)
                if matches!(b.node.op, BinOp::Add | BinOp::Mul)
                    && matches!(b.node.lhs.node, MirExpr::Project(_))
                    && matches!(b.node.rhs.node, MirExpr::Project(_)) =>
            {
                Some(e)
            }
            MirExpr::Return(inner) | MirExpr::Box(inner) | MirExpr::Unbox(inner) => {
                find_carrier_sum(&inner.node)
            }
            MirExpr::Let(l) => {
                find_carrier_sum(&l.node.value.node).or_else(|| find_carrier_sum(&l.node.body.node))
            }
            MirExpr::Match(m) => m
                .node
                .arms
                .iter()
                .find_map(|a| find_carrier_sum(&a.body.node)),
            _ => None,
        }
    }

    #[test]
    fn countdown_counter_is_bare() {
        let src = r#"
module Countdown
    intent = "t"
    depends []

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(20000)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "countdown");
        let f = facts.for_fn(id).expect("countdown facts");
        assert!(
            f.param_is_bare(0),
            "the countdown counter must be proven bare i64"
        );
    }

    #[test]
    fn factorial_counter_is_bare() {
        let src = r#"
module Factorial
    intent = "t"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Int
    factorial(10, 1)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "factorial");
        let f = facts.for_fn(id).expect("factorial facts");
        assert!(
            f.param_is_bare(0),
            "the factorial counter `n` must be proven bare i64"
        );
    }

    #[test]
    fn unbounded_param_stays_boxed() {
        // A fn called with a non-literal arg cannot bound its counter, so
        // the param must stay boxed (fail-closed).
        let src = r#"
module M
    intent = "t"
    depends []

fn down(n: Int) -> Int
    match n
        0 -> 0
        _ -> down(n - 1)

fn caller(x: Int) -> Int
    down(x)

fn main() -> Int
    caller(5)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "down");
        let f = facts.for_fn(id).expect("down facts");
        // `down` is called with `x` (a non-literal param of `caller`), and
        // `caller`'s `x` comes from a literal — but the minimal summary
        // only bounds DIRECT literal callers, so `down`'s counter is not
        // provably bounded here and must stay boxed.
        assert!(
            !f.param_is_bare(0),
            "a counter reached via a non-literal arg must stay boxed (fail-closed)"
        );
    }

    #[test]
    fn non_recursive_fn_param_stays_boxed() {
        // A non-recursive fn has no recurrence to bound its param; with no
        // base-case guard the counter is unbounded ⇒ boxed (fail-closed).
        let src = r#"
module M
    intent = "t"
    depends []

fn twice(n: Int) -> Int
    n + n

fn main() -> Int
    twice(10)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "twice");
        let f = facts.for_fn(id).expect("twice facts");
        assert!(
            !f.param_is_bare(0),
            "a param with no bounding recurrence must stay boxed"
        );
    }

    #[test]
    fn counter_stored_in_aggregate_demotes_via_escape() {
        // A bounded counter whose value is STORED in a list escapes (reaches
        // a general-Int aggregate), so the analysis must not mark the
        // value bare. We assert the body's escape predicate flags it: a
        // counter read into `[n]` is a general-Int aggregate store.
        let src = r#"
module M
    intent = "t"
    depends []

fn collect(n: Int) -> List<Int>
    match n
        0 -> [0]
        _ -> [n]

fn main() -> List<Int>
    collect(10)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "collect");
        let f = facts.for_fn(id).expect("collect facts");
        // The return type is `List<Int>`, not `Int`, so the return is never
        // bare; and `n` flows into the `[n]` aggregate, an escape. The
        // param must stay boxed.
        assert!(
            !f.param_is_bare(0),
            "a counter stored in a List aggregate escapes → must stay boxed"
        );
        assert!(!f.bare_return, "a List<Int> return is never bare i64");
    }

    #[test]
    fn worst_join_demotes_transient_out_of_i64_intermediate() {
        // The reused #511 worst-join discipline: `(n + i64::MAX) - i64::MAX`
        // cancels back into range, but the transient `n + i64::MAX`
        // overflows i64, so the binding's op class must NOT be
        // `OverflowFree`. Exercise the `eval_interval` + worst-join the body
        // pass uses, over a bare-param-seeded `n ∈ [0, 10]`.
        let mut env = HashMap::new();
        let n = LocalId(0);
        env.insert(n, Interval::between(0, 10));
        let big = i64::MAX as i128;

        // Build the MIR for `(n + MAX) - MAX`.
        let lit = |k: i128| Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Int(k as i64))));
        let local_n = Spanned::bare(MirExpr::Local(Spanned::bare(
            super::super::super::expr::MirLocal::at(n),
        )));
        let add = Spanned::bare(MirExpr::BinOp(Spanned::bare(
            super::super::super::expr::MirBinOp {
                op: BinOp::Add,
                lhs: Box::new(local_n),
                rhs: Box::new(lit(big)),
            },
        )));
        let sub = MirExpr::BinOp(Spanned::bare(super::super::super::expr::MirBinOp {
            op: BinOp::Sub,
            lhs: Box::new(add),
            rhs: Box::new(lit(big)),
        }));

        let mut worst = Interval::point(0);
        let no_carriers = FnBareFacts::default();
        let result = eval_interval(&sub, &env, &mut worst, &no_carriers);
        // The final value cancels back into [0, 10] (fits i64) …
        assert!(result.fits_i64(), "final value cancels back into range");
        // … but the worst-join saw the transient `n + i64::MAX` (out of i64),
        // so the op class over the whole expression is NOT OverflowFree.
        assert_ne!(
            OpClass::of_interval(worst.hull(result)),
            OpClass::OverflowFree,
            "the transient out-of-i64 intermediate must demote below OverflowFree"
        );
    }

    // ── BUG 1: recurrence guard REACHABILITY ────────────────────────────

    /// A decrement counter whose step does NOT divide `entry - K` steps OVER
    /// the equality guard `K` and diverges to -inf — the certified interval
    /// is fiction, so the param must NOT be bare (congruence skip).
    #[test]
    fn congruence_skip_declines() {
        let src = r#"
module M
    intent = "t"
    depends []

fn loopit(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> loopit(n - 4611686018427387905, acc)

fn main() -> Int
    loopit(4, 0)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "loopit");
        let f = facts.for_fn(id).expect("loopit facts");
        assert!(
            !f.param_is_bare(0),
            "a counter that steps OVER its equality guard (4 % (2^62+1) != 0) must box"
        );
    }

    /// Entry below the guard: the decrement moves AWAY from `K`, diverging —
    /// must box.
    #[test]
    fn entry_below_guard_declines() {
        let src = r#"
module M
    intent = "t"
    depends []

fn down(n: Int, acc: Int) -> Int
    match n
        100 -> acc
        _ -> down(n - 1, acc)

fn main() -> Int
    down(5, 0)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "down");
        let f = facts.for_fn(id).expect("down facts");
        assert!(
            !f.param_is_bare(0),
            "entry 5 < guard 100 decrements away from K → diverges → must box"
        );
    }

    /// Odd entry, step 2 toward guard 0: parity-skip, never lands on 0 —
    /// must box.
    #[test]
    fn parity_skip_declines() {
        let src = r#"
module M
    intent = "t"
    depends []

fn skip(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> skip(n - 2, acc)

fn main() -> Int
    skip(25, 0)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "skip");
        let f = facts.for_fn(id).expect("skip facts");
        assert!(
            !f.param_is_bare(0),
            "odd entry 25 with step 2 toward guard 0 steps over 0 → must box"
        );
    }

    /// Even entry, step 2 toward guard 0: the sequence LANDS on 0 — the fix
    /// declines only UNREACHABLE guards, not all step>1, so this stays BARE.
    #[test]
    fn divisible_reachable_guard_stays_bare() {
        let src = r#"
module M
    intent = "t"
    depends []

fn skip(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> skip(n - 2, acc)

fn main() -> Int
    skip(24, 0)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "skip");
        let f = facts.for_fn(id).expect("skip facts");
        assert!(
            f.param_is_bare(0),
            "even entry 24 with step 2 reaches guard 0 → must stay bare (win survives)"
        );
    }

    // ── BUG 2: compound interval gate ───────────────────────────────────

    /// A compound `n + i64::MAX` whose result interval leaves `i64` is NOT
    /// bare, even though both operands are bare — `expr_is_bare_i64` must
    /// decline it so codegen boxes the arithmetic.
    #[test]
    fn overflowing_compound_is_not_bare() {
        let mut facts = FnBareFacts::default();
        let n = LocalId(0);
        // `n` is a bare counter confined to `[1, 1]`.
        facts.values.insert(
            n,
            ValueFact {
                interval: Some(Interval::point(1)),
                op_class: OpClass::OverflowFree,
                escapes: false,
                repr: Repr::Bare,
            },
        );
        let big = i64::MAX as i128;
        let local_n = Spanned::bare(MirExpr::Local(Spanned::bare(
            super::super::super::expr::MirLocal::at(n),
        )));
        let lit = Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Int(big as i64))));
        let add = MirExpr::BinOp(Spanned::bare(super::super::super::expr::MirBinOp {
            op: BinOp::Add,
            lhs: Box::new(local_n),
            rhs: Box::new(lit),
        }));
        assert!(
            !facts.expr_is_bare_i64(&add),
            "`n + i64::MAX` (result [MAX+1, MAX+1]) leaves i64 → must NOT be bare"
        );
    }

    /// A compound `n - 1` over a tight bare counter `[0, 20000]` STAYS in
    /// i64 → bare (the legitimate fast decrement must survive).
    #[test]
    fn in_range_compound_stays_bare() {
        let mut facts = FnBareFacts::default();
        let n = LocalId(0);
        facts.values.insert(
            n,
            ValueFact {
                interval: Some(Interval::between(-1, 20000)),
                op_class: OpClass::OverflowFree,
                escapes: false,
                repr: Repr::Bare,
            },
        );
        let local_n = Spanned::bare(MirExpr::Local(Spanned::bare(
            super::super::super::expr::MirLocal::at(n),
        )));
        let lit = Spanned::bare(MirExpr::Literal(Spanned::bare(Literal::Int(1))));
        let sub = MirExpr::BinOp(Spanned::bare(super::super::super::expr::MirBinOp {
            op: BinOp::Sub,
            lhs: Box::new(local_n),
            rhs: Box::new(lit),
        }));
        assert!(
            facts.expr_is_bare_i64(&sub),
            "`n - 1` over a tight `[-1, 20000]` counter stays in i64 → bare"
        );
    }

    // ── BUG 3: escape scan recurses into BinOp ──────────────────────────

    /// A bare counter reaching a `List<Int>` aggregate THROUGH a `BinOp`
    /// (`[n + 1]`) escapes — the escape scan must mark it so the param boxes
    /// (the aggregate emit does not convert a bare compound).
    #[test]
    fn binop_in_aggregate_escapes() {
        let src = r#"
module M
    intent = "t"
    depends []

fn collect(n: Int) -> List<Int>
    match n
        1 -> [n + 1]
        _ -> collect(n - 1)

fn main() -> List<Int>
    collect(2)
"#;
        let (program, facts) = facts_for(src);
        let id = fn_id_by_name(&program, "collect");
        let f = facts.for_fn(id).expect("collect facts");
        assert!(
            !f.param_is_bare(0),
            "a counter reaching `[n + 1]` through a BinOp escapes the aggregate → must box"
        );
    }

    // ── BOUNDARY-COMPLETENESS regressions (PR #519 four defects) ─────────
    //
    // Each defect was a valid Aver program whose emitted Rust failed to
    // compile because the analysis/codegen disagreed on a value's
    // representation at a use position. The fix is fail-closed: the counter
    // stays bare (fast loop preserved) while the single crossing converts at
    // the boundary, OR the escaping value demotes. These assert the
    // analysis-observable half (the codegen boundary conversions are covered
    // by `tests/rust_codegen_regression.rs`).

    /// Defect Q4: a bare compound `n + 1` flows as a Call arg to a BOXED
    /// param (`keep(x: Int)`). The counter stays bare — codegen converts the
    /// arg with `from_i64` at the boxed-param boundary (the value itself does
    /// not cross; a fresh `AverInt` does), so the fast loop is preserved.
    #[test]
    fn call_arg_to_boxed_param_keeps_counter_bare() {
        let src = r#"
module M
    intent = "t"
    depends []

fn keep(x: Int) -> Int
    x

fn down(n: Int) -> Int
    match n
        0 -> keep(n + 1)
        _ -> down(n - 1)

fn main() -> Int
    down(2)
"#;
        let (program, facts) = facts_for(src);
        let down = facts
            .for_fn(fn_id_by_name(&program, "down"))
            .expect("down facts");
        // `down`'s counter stays bare (converted at the boxed-param boundary).
        assert!(
            down.param_is_bare(0),
            "the down counter stays bare; the boxed-param arg `n + 1` converts at the boundary"
        );
        // `keep`'s param is a general-Int (boxed) — it has a non-literal,
        // non-bare-supplyable caller arg shape, so it never goes bare.
        let keep = facts
            .for_fn(fn_id_by_name(&program, "keep"))
            .expect("keep facts");
        assert!(
            !keep.param_is_bare(0),
            "keep's general-Int param stays boxed (no bounding recurrence)"
        );
    }

    /// Defect Q5: a fn `g` whose return is proven bare is consumed by `h`
    /// whose own return is the general Int. `g` keeps its bare return + bare
    /// counter (the fast loop); codegen boxes the call result with `from_i64`
    /// at `h`'s return crossing. The whole-program summary must still mark
    /// `g.bare_return` (the consumer demotion is a CODEGEN conversion, not an
    /// analysis demotion, so the win is preserved).
    #[test]
    fn bare_return_consumed_by_boxed_return_fn_stays_bare() {
        let src = r#"
module M
    intent = "t"
    depends []

fn g(n: Int) -> Int
    match n
        0 -> 0
        _ -> g(n - 1)

fn h() -> Int
    g(2)

fn main() -> Int
    h()
"#;
        let (program, facts) = facts_for(src);
        let g = facts.for_fn(fn_id_by_name(&program, "g")).expect("g facts");
        assert!(g.param_is_bare(0), "g's bounded counter stays bare");
        assert!(
            g.bare_return,
            "g's return stays bare; the boxed consumer `h` converts at its return boundary"
        );
    }

    /// Defect subj_ret (opus Area 3): a bare counter `n` aliased through an
    /// inner match binding `match n { y -> y }`. The alias must be TRACKED so
    /// `y` inherits `n`'s bare interval — otherwise the body facts and codegen
    /// (which declares `y` at `n`'s bare type) disagree. The counter stays
    /// bare; the return crossing boxes `y` with `from_i64`.
    #[test]
    fn match_binding_alias_is_tracked_bare() {
        let src = r#"
module M
    intent = "t"
    depends []

fn loopit(n: Int) -> Int
    match n
        0 -> match n
            y -> y
        _ -> loopit(n - 1)

fn main() -> Int
    loopit(3)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "loopit"))
            .expect("loopit facts");
        assert!(
            f.param_is_bare(0),
            "the counter stays bare; the aliased binding `y` inherits its bare interval"
        );
        // The aliased binding `y` must carry a fact (not absent → unknown):
        // it aliases the bare param, so the analysis proves it bare and the
        // codegen agrees on the representation at the return crossing.
        let bare_y = f.values.values().any(|v| v.is_bare());
        assert!(
            bare_y,
            "at least the counter / its bare alias is proven bare"
        );
    }

    /// Defect esc_match (opus Area 3, escaping alias): a bare compound
    /// `let x = n - 1` aliased into an `Int` aggregate `[x, x]`. `x` must
    /// DEMOTE (it reaches a general-Int aggregate); the counter `n` stays
    /// bare. Codegen boxes the binding value with `from_i64` at the let
    /// crossing.
    #[test]
    fn match_let_alias_into_aggregate_demotes() {
        let src = r#"
module M
    intent = "t"
    depends []

fn loopit(n: Int) -> List<Int>
    match n
        0 -> match n - 1
            x -> [x, x]
        _ -> loopit(n - 1)

fn main() -> List<Int>
    loopit(4)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "loopit"))
            .expect("loopit facts");
        // The counter `n` stays bare (its only escaping use is via the boxed
        // `x` binding, not `n` itself).
        assert!(f.param_is_bare(0), "the counter `n` stays bare");
        // The `List<Int>` return is never bare.
        assert!(!f.bare_return, "a List<Int> return is never bare i64");
    }

    /// Defect marms: a bare counter whose guard has ≥2 Int-literal base-case
    /// arms lowers to the dispatch-table match path. The counter legitimately
    /// stays bare and is compared against the literals — the codegen dispatch
    /// path must emit `subject == {K}i64` (not `i64 == AverInt`). This asserts
    /// the analysis keeps the multi-literal-arm counter bare (the codegen
    /// dispatch-bare path is covered by `tests/rust_codegen_regression.rs`).
    #[test]
    fn multi_literal_arm_counter_stays_bare() {
        let src = r#"
module M
    intent = "t"
    depends []

fn loopit(n: Int, acc: Int) -> Int
    match n
        2 -> acc
        0 -> acc
        _ -> loopit(n - 1, acc + 1)

fn main() -> Int
    loopit(5, 0)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "loopit"))
            .expect("loopit facts");
        assert!(
            f.param_is_bare(0),
            "a ≥2-literal-arm bounded counter stays bare (dispatch path emits `== Ki64`)"
        );
    }

    /// Multi-tail-call soundness hole: a counter with TWO self-tail-call
    /// paths at the same index — one decrements (`n - 1`), one GROWS (`n +
    /// 1_000_000_000_000_000_000`). The pre-fix recurrence recognizer
    /// stopped at the FIRST (decrement) path and seeded the param's interval
    /// from the decrement alone, marking it bare; at runtime the growth path
    /// drove `n` out of `i64` range and the emitted native `i64` op
    /// (`n + n`) silently wrapped in release (the C0 bug — caught only by the
    /// `overflow-checks` panic in dev). The fix requires EVERY self-tail-call
    /// arg at the index to be the SAME monotone decrement; a second growing
    /// path makes the recurrence unbounded ⇒ the param must BOX (fail-closed).
    #[test]
    fn multi_tailcall_growing_path_demotes_to_boxed() {
        let src = r#"
module M
    intent = "t"
    depends []

fn loopit(n: Int, phase: Int) -> Int
    match n
        0 -> n + n
        _ -> match phase
            0 -> n + n
            5000 -> loopit(n - 1, phase)
            _ -> loopit(n + 1000000000000000000, phase - 1)

fn main() -> Int
    loopit(8, 5)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "loopit"))
            .expect("loopit facts");
        // The counter `n` has a SECOND, growing self-tail-call path, so it is
        // NOT a provably-bounded counter — it must box. Pre-fix this asserted
        // `param_is_bare(0) == true` (the soundness hole): the recognizer saw
        // only the `n - 1` path.
        assert!(
            !f.param_is_bare(0),
            "a counter with a growing second self-tail-call path is unbounded → must box"
        );
    }

    /// Recursive-base-arm soundness hole: the equality-guard arm (`match n {
    /// 0 -> … }`) is treated as the counter's stopping point, but its body
    /// itself self-recurses (`0 -> loopit(n - 1)`). The counter therefore
    /// never stops at the guard `0` — it runs past it toward `-inf` (in ℤ) /
    /// wraps (as bare `i64`), so the `[K - step, entry]` bound is fiction.
    /// Pre-fix, `guard_literal_for` accepted the `0` arm as an equality guard
    /// without checking it terminates, and `walk_self_tailcall_steps` counted
    /// the base arm's `n - 1` as a valid decrement ⇒ `param_is_bare(0) == true`
    /// (the hole). The fix declines when the equality-guard arm self-recurses.
    #[test]
    fn recursive_base_arm_declines() {
        let src = r#"
module M
    intent = "t"
    depends []

fn loopit(n: Int) -> Int
    match n
        0 -> loopit(n - 1)
        9223372036854775807 -> n + 1
        _ -> loopit(n - 1)

fn main() -> Int
    loopit(5)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "loopit"))
            .expect("loopit facts");
        // The equality-guard `0` arm self-recurses, so `0` is not a stopping
        // value and the counter is unbounded below — it must box. Pre-fix this
        // asserted `param_is_bare(0) == true` (the soundness hole).
        assert!(
            !f.param_is_bare(0),
            "a counter whose equality-guard base arm self-recurses is unbounded → must box"
        );
    }

    /// Guard-dominance hole: the `0` literal arm that `guard_literal_for`
    /// latches onto lives in a DEAD `match n` binding, NOT in the match that
    /// actually gates the recursion (whose base case is `i64::MAX`). The
    /// counter decrements toward `-inf`, never stopping at `0`, so the
    /// `[K-step, entry]` floor is fiction. The fix requires the `K` arm and a
    /// self-tail-call to be sibling arms of the SAME `match counter`
    /// (`guard_dominates_recursion`); here they are not, so the param boxes.
    /// Found by the cross-vendor panel on the fixpoint PR — a latent hole
    /// pre-existing in the hand-rolled recognizers.
    #[test]
    fn non_dominating_guard_declines() {
        let src = r#"
module M
    intent = "t"
    depends []

fn bad(n: Int) -> Int
    witness = match n
        0 -> 0
        _ -> 0
    match n
        9223372036854775807 -> n
        _ -> bad(n - 1)

fn main() -> Int
    bad(5)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "bad"))
            .expect("bad facts");
        // The `0` guard does not dominate the recursion (its match is a dead
        // binding); the real base case is `i64::MAX`, never reached descending
        // from 5 — the counter is unbounded below and must box.
        assert!(
            !f.param_is_bare(0),
            "a counter whose equality guard does not dominate the recursion is unbounded → must box"
        );
    }

    /// The OTHER idiomatic countdown shape — `match n == 0 { true -> …;
    /// false -> down(n-1) }` — lowers to `IfThenElse { cond: n == 0, then,
    /// else }`. Its guard dominates the recursion (rec in the `n != 0` else
    /// branch, base in the `== 0` then branch), so the counter must STAY bare.
    /// Guards the over-box the dominance gate would otherwise cause on the
    /// `Eq`-cond form (caught by the empirical panel).
    #[test]
    fn comparison_equality_countdown_stays_bare() {
        let src = r#"
module M
    intent = "t"
    depends []

fn down(n: Int) -> Int
    match n == 0
        true -> 0
        false -> down(n - 1)

fn main() -> Int
    down(20000)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "down"))
            .expect("down facts");
        // `n == 0` dominates (rec in the `!= 0` branch), so the counter is
        // bounded `[0, 20000]` and stays bare — same as `match n { 0 -> … }`.
        assert!(
            f.param_is_bare(0),
            "a `match n == 0` countdown's counter dominates and must stay bare (no over-box)"
        );
    }

    // ── FIXPOINT producer: interval-VALUE goldens (byte-identity) ───────

    /// The produced interval for a `(fn, param)`, read straight off the
    /// fixpoint producer (not just `param_is_bare`). Pins the VALUE.
    fn produced_interval(program: &MirProgram, fn_name: &str, i: usize) -> Option<Interval> {
        let id = fn_id_by_name(program, fn_name);
        let ivs = compute_param_intervals_for_test(program);
        ivs.get(&id).and_then(|v| v.get(i).copied()).flatten()
    }

    /// Countdown's counter interval must equal the OLD closed-form value
    /// `[K-step, entry] = [-1, 20000]` (the old `param_recurrence_bound`
    /// combine `[min(E.lo, K-step), max(E.hi, K)]` for K=0, step=1, E=20000).
    /// This pins byte-identity at the VALUE level, not just `param_is_bare`.
    #[test]
    fn countdown_interval_is_K_minus_step_to_entry() {
        let src = r#"
module Countdown
    intent = "t"
    depends []

fn countdown(n: Int) -> Int
    match n
        0 -> 0
        _ -> countdown(n - 1)

fn main() -> Int
    countdown(20000)
"#;
        let (program, _facts) = facts_for(src);
        assert_eq!(
            produced_interval(&program, "countdown", 0),
            Some(Interval::between(-1, 20000)),
            "countdown's counter interval must be byte-identical to the old [K-step, entry]"
        );
    }

    /// Factorial: `n` (the counter) is `[K-step, entry] = [-1, 10]`; `acc`
    /// (the growing accumulator) has no guard, so it widens out of i64 → None.
    #[test]
    fn factorial_n_interval_is_K_minus_step_to_entry_and_acc_is_none() {
        let src = r#"
module Factorial
    intent = "t"
    depends []

fn factorial(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> factorial(n - 1, acc * n)

fn main() -> Int
    factorial(10, 1)
"#;
        let (program, _facts) = facts_for(src);
        assert_eq!(
            produced_interval(&program, "factorial", 0),
            Some(Interval::between(-1, 10)),
            "factorial `n` interval must be byte-identical to [K-step, entry] = [-1, 10]"
        );
        assert_eq!(
            produced_interval(&program, "factorial", 1),
            None,
            "factorial `acc` grows unbounded → boxed (None)"
        );
    }

    /// The #519 modular non-landing decline, at the VALUE level: step 2,
    /// guard 0, entry 5 — `(5-0) % 2 != 0` withholds the floor, so the param
    /// is boxed (None), not `Some([0,5])` (the under-approximation the gate
    /// closes). Pins that the decline survives the fixpoint rewrite.
    #[test]
    fn step_two_modular_nonlanding_interval_is_none() {
        let src = r#"
module M
    intent = "t"
    depends []

fn down(n: Int, acc: Int) -> Int
    match n
        0 -> acc
        _ -> down(n - 2, acc)

fn main() -> Int
    down(5, 0)
"#;
        let (program, facts) = facts_for(src);
        let f = facts
            .for_fn(fn_id_by_name(&program, "down"))
            .expect("down facts");
        assert!(
            !f.param_is_bare(0),
            "odd entry 5, step 2 toward guard 0 steps over 0 → must box"
        );
        assert_eq!(
            produced_interval(&program, "down", 0),
            None,
            "the modular-hole counter must be None (the gate withholds the floor)"
        );
    }

    /// Termination + boxing: an unguarded unit decrement with no reachable
    /// base case (the guard subject is a DIFFERENT param, so the counter `n`
    /// is never stopped) must (a) terminate the solve — no hang — and (b) map
    /// the counter to None (boxed), exercising widen on the descending `lo`.
    #[test]
    fn widen_terminates_unbounded_decrement() {
        let src = r#"
module M
    intent = "t"
    depends []

fn spin(n: Int, k: Int) -> Int
    match k
        0 -> n
        _ -> spin(n - 1, k - 1)

fn caller(k: Int) -> Int
    spin(7, k)

fn main() -> Int
    caller(3)
"#;
        let (program, facts) = facts_for(src);
        // `spin`'s `n` has a unit decrement but its guard is on `k`, not `n`;
        // `n` has a literal entry (7) but no equality guard ON `n`, so no
        // floor is installed → the descent widens → None (boxed). The solve
        // must terminate (this test returning at all proves no hang).
        let f = facts
            .for_fn(fn_id_by_name(&program, "spin"))
            .expect("spin facts");
        assert!(
            !f.param_is_bare(0),
            "an unguarded decrement counter (guard is on another param) must box"
        );
        assert_eq!(
            produced_interval(&program, "spin", 0),
            None,
            "the unguarded decrement counter must widen to None, not hang"
        );
    }

    // ---- multi-field carrier bound attribution -----------------------------

    /// Build the per-`(record, field)` carrier-interval table for `src`
    /// through the same multi-field derivation the codegen entry uses.
    fn field_table_for(
        src: &str,
    ) -> HashMap<(String, String), (crate::ir::interval::Interval, bool)> {
        let mut items = parse_source(src).expect("parse");
        let cfg = crate::ir::pipeline::PipelineConfig {
            typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
            ..Default::default()
        };
        let result = crate::ir::pipeline::run(&mut items, cfg);
        let empty_prefixes: HashSet<String> = HashSet::new();
        let empty_recursive: HashSet<crate::ir::FnId> = HashSet::new();
        let inputs = crate::codegen::proof_lower::ProofLowerInputs {
            entry_items: &items,
            dep_modules: &[],
            module_prefixes: &empty_prefixes,
            recursive_fns: &empty_recursive,
            symbol_table: &result.symbol_table,
            program_shape: None,
        };
        crate::codegen::proof_lower::field_carrier_interval_table(&inputs)
    }

    #[test]
    fn field_carrier_per_field_intervals() {
        // A 2-arg smart ctor bounding each field independently → each field
        // gets its own proven interval.
        let src = r#"
module Toy
    intent = "t"
    depends []

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 100), Bool.and(y >= 0, y <= 200))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("err")

fn main() -> Int
    match coord(1, 2)
        Result.Ok(c)  -> c.x
        Result.Err(_) -> 0
"#;
        let table = field_table_for(src);
        let (ix, kx) = table
            .get(&("Coord".to_string(), "x".to_string()))
            .copied()
            .expect("x field bound");
        let (iy, ky) = table
            .get(&("Coord".to_string(), "y".to_string()))
            .copied()
            .expect("y field bound");
        assert!(kx && ky, "both fields recognized");
        use crate::ir::interval::Bound;
        assert_eq!(ix.lo, Bound::Finite(0));
        assert_eq!(ix.hi, Bound::Finite(100));
        assert_eq!(iy.lo, Bound::Finite(0));
        assert_eq!(iy.hi, Bound::Finite(200));
    }

    #[test]
    fn field_carrier_cross_field_condition_dropped() {
        // A cross-field leaf (`x + y <= 50`) mentions two params; it is dropped
        // from each field's projection, so each field keeps only its own
        // single-variable bound. The bound is a sound over-approximation.
        let src = r#"
module Toy
    intent = "t"
    depends []

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(Bool.and(x >= 0, x <= 100), Bool.and(y >= 0, x + y <= 50))
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("err")

fn main() -> Int
    0
"#;
        let table = field_table_for(src);
        // x keeps its single-var [0, 100] (the cross-field `x + y <= 50` drops).
        let (ix, kx) = table
            .get(&("Coord".to_string(), "x".to_string()))
            .copied()
            .expect("x field bound");
        assert!(kx);
        use crate::ir::interval::Bound;
        assert_eq!(ix.lo, Bound::Finite(0));
        assert_eq!(ix.hi, Bound::Finite(100));
        // y has only `y >= 0` as a single-var leaf (the upper bound was the
        // dropped cross-field condition) → no fits_i64 upper bound, so the
        // interval is recognized-but-unbounded-above; it is NOT eligible.
        let y = table.get(&("Coord".to_string(), "y".to_string())).copied();
        if let Some((iy, ky)) = y {
            assert!(
                !(ky && iy.fits_i64()),
                "y with only a lower bound must not be a fits_i64 eligible field"
            );
        }
    }

    #[test]
    fn field_carrier_only_one_field_bounded() {
        // A mixed record: one field gated, the other not mentioned in the
        // guard at all. Only the gated field gets a bound.
        let src = r#"
module Toy
    intent = "t"
    depends []

record Coord
    x: Int
    y: Int

fn coord(x: Int, y: Int) -> Result<Coord, String>
    match Bool.and(x >= 0, x <= 100)
        true  -> Result.Ok(Coord(x = x, y = y))
        false -> Result.Err("err")

fn main() -> Int
    0
"#;
        let table = field_table_for(src);
        assert!(
            table.contains_key(&("Coord".to_string(), "x".to_string())),
            "the gated field x is bounded"
        );
        let y = table.get(&("Coord".to_string(), "y".to_string())).copied();
        assert!(
            y.is_none_or(|(iv, k)| !(k && iv.fits_i64())),
            "the ungated field y must not be an eligible bounded field"
        );
    }

    #[test]
    fn field_carrier_mis_fire_no_smart_ctor() {
        // A plain 2-field record with NO smart constructor → no bound is
        // attributed to any field (the table is empty for it).
        let src = r#"
module Toy
    intent = "t"
    depends []

record Coord
    x: Int
    y: Int

fn main() -> Int
    Coord(x = 1, y = 2).x
"#;
        let table = field_table_for(src);
        assert!(
            !table.contains_key(&("Coord".to_string(), "x".to_string())),
            "a record with no smart ctor attributes no field bound"
        );
    }
}
