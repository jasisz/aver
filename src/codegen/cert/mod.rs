//! Stage B artifact-certificate emitter: `aver compile --target wasm-gc --certify`.
//!
//! Emits, next to `<name>.wasm`, a self-contained Lean `cert/` project that
//! `lake build`s green with kernel-clean theorems for the user functions that
//! fall into the three measured classes:
//!
//! * straight-line `Int -> Int` add-a-constant (the `addTwo` kill-fast shape),
//! * single-argument self-recursion of the `sumTo` shape
//!   (`match n <= 0 { true -> 0; false -> n + f(n - 1) }`),
//! * two-argument accumulator self-recursion of the `countDown` shape
//!   (`match n <= 0 { true -> acc; false -> f(n - 1, acc + n) }`).
//!
//! Everything else is FAIL-CLOSED: listed in `cert-manifest.json` as
//! `source-level-only` with a reason. No weaker theorem is ever emitted.
//!
//! The certified-function bodies are read back from the module bytes the
//! compiler just emitted (the same bytes whose sha256 the certificate pins),
//! matched against the two structural templates, and re-rendered as
//! `CertPrelude.WInstr` data. A function whose real emitted body does not match
//! a template is declined — so the `WInstr` data in `Module.lean` is exactly
//! the shape present in the hashed bytes.
//!
//! `aver cert verify` re-runs this same audited pipeline on the hash-verified
//! bytes (`rederive_certificate`) and pins the re-derived `code`/`host`/`self`/
//! `carrier` values plus consumed runtime contracts into its checker-authored
//! witness with `rfl` against the proven manifest — so the `WInstr` data and
//! contracts the kernel theorem actually reasons about are forced to equal what
//! the bytes decode to, not merely trusted. This is trusted via inspection of
//! the disassembler, not by an in-kernel wasm decode proof (a full kernel
//! decoder is a deferred residual).

use sha2::{Digest, Sha256};
use std::path::Path;

/// The Stage-A semantics prelude, single source of truth, embedded so the
/// emitter is self-contained.
pub const CERT_PRELUDE: &str = include_str!("../../../tools/certkit/prelude/CertPrelude.lean");
pub const LEAN_TOOLCHAIN: &str = include_str!("../../../tools/certkit/prelude/lean-toolchain");

/// The audited statement schema, single source of truth, embedded so both the
/// emitter and the `aver cert verify` checker pin the exact same bytes. The
/// consumer trusts the certificate by checking the final theorem NAME, the
/// manifest LITERAL, and the hash of THIS file plus the prelude — never Lean
/// proof syntax. Fixed content (no per-build parts) so its sha256 is known to
/// the checker at compile time.
pub const CERT_SCHEMA: &str = include_str!("Schema.lean");

/// Emitted-fragment profile and runtime ABI identifiers recorded in the
/// manifest. Stable strings the checker echoes; bumped when the certified
/// fragment or the runtime import surface changes.
pub const PROFILE_ID: &str = "AverUserProfile/v0";
pub const RUNTIME_ABI: &str = "aver-wasm-gc/0";
/// Certification level of a v0 artifact certificate: conditional on the named
/// runtime contracts (see the consult level naming L0/L1/L2/L3).
pub const CERT_LEVEL: &str = "L1";
pub const CERT_SCHEMA_VERSION: u32 = 6;
pub const BOX_CONTRACT: &str = "__rt_aint_from_i64 (box i64 -> carrier)";
pub const INT_ADD_CONTRACT: &str =
    "Int.add (carrier add = exact integer addition on represented values)";
pub const INT_SUB_CONTRACT: &str =
    "Int.sub (carrier sub = exact integer subtraction on represented values)";
pub const STRING_EQ_CONTRACT: &str =
    "String.eq (WVal byte-array equality; non-arrays compare false)";
/// The one approved final-theorem statement line. `aver cert verify` confirms
/// this exact line is present in `Final.lean` (name + `Holds manifest`), which
/// is what pins the statement without matching arbitrary Lean syntax.
pub const FINAL_THEOREM: &str = "AverCert.Final.cert";
pub const FINAL_STATEMENT_LINE: &str =
    "theorem AverCert.Final.cert : AverCert.Schema.Holds manifest";

/// sha256 of a byte slice, lowercase hex.
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex(&h.finalize())
}

/// The content hashes of the audited schema and semantics prelude as embedded
/// in THIS binary — the checker's anchor: a cert whose on-disk `Schema.lean` /
/// `CertPrelude.lean` do not hash to these is not the audited version.
pub fn audited_schema_sha() -> String {
    sha256_hex(CERT_SCHEMA.as_bytes())
}
pub fn audited_prelude_sha() -> String {
    sha256_hex(CERT_PRELUDE.as_bytes())
}

/// A user function recovered from the emitted module.
#[derive(Clone)]
struct UserFn {
    name: String,
    wasm_idx: u32,
    arity: usize,
    /// Byte-level parameter type kinds from the declared function signature.
    params: Vec<TyKind>,
    /// Byte-level result type kind from the declared function signature.
    result: Option<TyKind>,
    nlocals: usize,
    ops: Vec<Op>,
    /// call targets in body order, for reason reporting.
    calls: Vec<u32>,
    has_loop_or_branch: bool,
}

#[derive(Clone)]
struct CodeEntry {
    nlocals: usize,
    ops: Vec<Op>,
    calls: Vec<u32>,
    has_loop_or_branch: bool,
    host_role: Option<HostRole>,
    host_ops: Vec<HostOp>,
}

/// The minimal opcode surface the two templates need. Anything else is `Other`
/// (which forces a decline) — a certified body never contains an `Other`.
#[derive(Clone, PartialEq)]
enum Op {
    LocalGet(u32),
    LocalSet(u32),
    I64Const(i64),
    I32Const(i32),
    F64Const(u64),
    RefTest(u32),
    RefCast(u32),
    StructNew(u32, u32),
    StructGet(u32, u32),
    ArrayNewData(u32, Vec<u8>),
    ArrayNewDataUnresolved {
        type_idx: u32,
        data_idx: u32,
        offset: i32,
        len: i32,
    },
    RefNull,
    RefIsNull,
    I64LeS,
    I64GeS,
    I32LtS,
    I32GtS,
    If,
    Else,
    End,
    Call(u32),
    ReturnCall(u32),
    Other,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum HostRole {
    Add,
    Sub,
    StringEq,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum HostOp {
    LocalGet(u32),
    LocalSet(u32),
    I32Const(i32),
    ArrayLen,
    ArrayGetU(u32),
    I32Ne,
    I32GeU,
    I32Add,
    If,
    Block,
    Loop,
    Br(u32),
    BrIf(u32),
    Return,
    End,
    Other,
}

/// The non-recursive operand of a body-consumed fuel recursion's combinator
/// `f n = if n≤0 then base else <combine>`, where `<combine>` applies a host
/// arithmetic helper to the self-call result and this operand. From the bytes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BodyOperand {
    /// The descending input `n` (`local.get 0`), as in `sumTo`'s `n + f(n-1)`.
    Input,
    /// A boxed integer literal, as in `2 + f(n-1)`.
    Const(i64),
}

/// Which arithmetic contract the body-recursion combinator obeys. The bignum
/// `add` and `mul` helpers are not byte-distinguishable (both use i64 add/sub/mul
/// internally), so this is read from the MODEL operator — the trusted source of
/// what the function computes, the same spec the whole certificate is stated
/// against. The checker re-derives it from the model too, so the host still pins.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Combinator {
    /// `f(n-1)` combined with the other operand by integer `+` (host `add`).
    Add,
    /// integer `*` (host `mul`).
    Mul,
}

impl Combinator {
    /// The obligation host slot / theorem contract param this combinator draws.
    fn param(self) -> &'static str {
        match self {
            Combinator::Add => "add",
            Combinator::Mul => "mul",
        }
    }
}

/// Byte-level summary of one wasm value type in a function signature. Typed
/// admission gates key on these (the shape of the claim as the BYTES declare
/// it) — never on the source model's types, and never on a bare parameter
/// count.
#[derive(Clone, Copy, PartialEq, Eq)]
enum TyKind {
    /// Abstract `eq` reference — the emitter's parameter type for a user ADT
    /// value that the body dispatches on.
    Eqref,
    /// Concrete reference to module type `t` (e.g. the Int carrier struct).
    Ref(u32),
    I64,
    I32,
    F64,
    Other,
}

/// One recognised leaf of a `VariantDispatch` hit arm: what the arm computes
/// from the variant's Int payload.
#[derive(Clone, PartialEq)]
enum ArmLeaf {
    /// Return the projected payload unchanged.
    Proj,
    /// Combine the payload with a boxed constant through a contracted host:
    /// `k op x` when `const_first`, else `x op k`.
    HostOp {
        role: HostRole,
        k: i64,
        const_first: bool,
    },
}

/// The straight-line integer shape of one function inside a composition's call
/// closure. Every shape is unary (`Int -> Int`), non-recursive, branch-free, and
/// its simulation lemma is provable over the caller's composed code table by the
/// probe's straight-line skeleton (rcases the host/callee `Option`, cite, close).
#[derive(Clone)]
enum LeafShape {
    /// `[localGet 0, localGet 0, call add]` — model `x + x`.
    SelfSum { add_idx: u32 },
    /// `[localGet 0, call c1, ..., call cm]` (m >= 1), each `ci` a user function
    /// in the closure — model `cm (... (c1 x))`. The composition point.
    Chain { calls: Vec<u32> },
}

/// One function in a composition caller's transitive call closure: its verbatim
/// body (for the shared `CodeTbl`), its self index, and its recognised shape.
#[derive(Clone)]
struct ClosureEntry {
    name: String,
    self_idx: u32,
    nlocals: usize,
    ops: Vec<Op>,
    shape: LeafShape,
}

#[derive(Clone, PartialEq)]
enum VerbatimDefault {
    Null,
    F64Bits(u64),
    Array { type_idx: u32, bytes: Vec<u8> },
}

#[derive(Clone, PartialEq)]
enum StringEqDefault {
    Input,
    Verbatim(VerbatimDefault),
}

type StringEqVerbatimChain = (
    Vec<(VerbatimDefault, VerbatimDefault)>,
    StringEqDefault,
    u32,
);

#[derive(Clone, PartialEq)]
enum ConstructorField {
    Local(u32),
    Null,
}

/// A certified function and the template holes extracted from its body.
enum Cert {
    /// Generic non-recursive certificate. The inner shape still carries the
    /// byte-derived face and proof parameters; the outer class records that the
    /// non-recursive walker admitted it.
    NonRecursive { inner: Box<Cert> },
    /// `fn(x: Int) -> Int = x + k`; box=`box_idx`, add=`add_idx`.
    StraightLine {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        k: i64,
        box_idx: u32,
        add_idx: u32,
    },
    /// Single-argument fuel self-recursion `f n = if n≤0 then BASE else <combine>`;
    /// box/add/sub host helpers. All of the shape below is DATA recovered from the
    /// bytes; only the descent (`n-1`) and the host `add` combinator are pinned:
    /// - `base_k`: the literal returned in the base arm (sumTo's `0`, but any).
    /// - `other` + `rec_first`: the `add` combines the self-call result `f(n-1)`
    ///   with `other` (the input `n`, or a boxed constant); `rec_first` records
    ///   which side the recursive result sits on — `f(n-1) + n` vs `n + f(n-1)`.
    Recursive {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        box_idx: u32,
        /// The combinator helper index (whichever arithmetic contract it obeys).
        add_idx: u32,
        sub_idx: u32,
        base_k: i64,
        rec_first: bool,
        other: BodyOperand,
        /// `+` or `*`, read from the model operator (see [`Combinator`]).
        combinator: Combinator,
    },
    /// countDown-shape two-argument accumulator recursion; box/add/sub host helpers.
    AccumulatorRecursive {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        box_idx: u32,
        add_idx: u32,
        sub_idx: u32,
    },
    /// Non-recursive constructor: local arguments wrapped by `struct.new`.
    AdtConstructor {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        struct_idx: u32,
        field_count: u32,
        arity: usize,
        fields: Vec<ConstructorField>,
        ops: Vec<Op>,
    },
    /// Non-recursive record/variant field projection.
    FieldProjection {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        struct_idx: u32,
        field_idx: u32,
        ops: Vec<Op>,
    },
    /// Non-recursive two-branch "widened" match projecting one integer-payload
    /// variant of a user inductive, with a boxed-`0` default for every other
    /// variant: `match j { JsonInt(n) -> n; _ -> 0 }`. Generalises the fixed
    /// three-variant match to any inductive with a single projected Int variant.
    WidenedIntMatch {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        /// Struct type index of the projected (integer-payload) variant.
        hit_variant_idx: u32,
        box_idx: u32,
        ops: Vec<Op>,
    },
    /// Non-recursive two-branch match projecting one variant's first field
    /// VERBATIM (as a raw `WVal`), defaulting to the null reference for every
    /// other variant: `match j { JsonList(items) -> items; _ -> [] }` where the
    /// empty list lowers to `ref.null`. No claim about the projected value's
    /// meaning — `Cod := WVal`, `verbatimRepr` — so it needs no carrier/string
    /// representation.
    VerbatimWidenedMatch {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        hit_variant_idx: u32,
        default: VerbatimDefault,
        ops: Vec<Op>,
    },
    /// Non-recursive `ref.test` dispatch over a user enum where EVERY arm returns
    /// a distinct VERBATIM constant (a String literal `array.new_data`, a null, or
    /// an f64), e.g. `match code { Quote -> "\""; Backslash -> "\\"; ... }`
    /// (`unescapedChar`). No host, no arithmetic, no field projection — each tag
    /// maps to a byte-derived constant `WVal`. Certified `Cod := WVal`,
    /// `verbatimRepr`, model `{name}Model : WVal → WVal`; reuses the verbatim
    /// widened face. The generalisation of the verbatim widened match from "one
    /// projected hit + a default" to "k constant arms".
    VerbatimVariantDispatch {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        /// `(variant tag, its verbatim constant)`, in dispatch order.
        arms: Vec<(u32, VerbatimDefault)>,
        /// The terminal else constant.
        default: VerbatimDefault,
        ops: Vec<Op>,
    },
    /// Non-recursive String-literal dispatch whose condition is delegated to a
    /// contracted `String.eq` helper. The certified user body stays loop-free:
    /// it constructs byte-derived string constants with `array.new_data`, calls
    /// the exact String.eq host slot, then returns a byte-derived constant or
    /// the original input verbatim.
    StringEqVerbatimMatch {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        string_eq_idx: u32,
        arms: Vec<(VerbatimDefault, VerbatimDefault)>,
        default: StringEqDefault,
        ops: Vec<Op>,
    },
    /// Non-recursive Int -> Bool range predicate: two nested carrier comparisons
    /// against constants, `match cp >= k_lo { true -> cp <= k_hi; false -> false }`
    /// (the `isHighSurrogate`/`isLowSurrogate` shape). Certified over the canonical
    /// small-carrier domain (the constants and every code point fit i64), so the
    /// bignum comparison arms are dead in the proof.
    IntRangePredicate {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        k_lo: i64,
        k_hi: i64,
        ops: Vec<Op>,
    },
    /// General non-recursive variant dispatch over one user inductive: a chain
    /// of `ref.test` branches (each else-arm continuing the chain) whose hit
    /// arms each reduce to one recognised leaf — payload projection, or a
    /// contracted host add/sub combining the payload with an integer constant —
    /// and whose terminal else is a boxed integer constant. Recognised from the
    /// parsed instruction tree, so arm count, arm order, per-arm semantics and
    /// the default value are free; no full opcode sequence is pinned.
    VariantDispatch {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        box_idx: u32,
        add_idx: Option<u32>,
        sub_idx: Option<u32>,
        /// `(variant struct tag, leaf)` in dispatch order.
        arms: Vec<(u32, ArmLeaf)>,
        /// The terminal else: a boxed integer constant.
        default_k: i64,
        ops: Vec<Op>,
    },
    /// Cross-function composition: a non-recursive `Int -> Int` caller whose body
    /// is a unary chain of calls to other user functions, each of which is itself
    /// a straight-line integer shape (self-sum or a nested chain). The obligation
    /// carries the caller's ENTIRE call closure in one `CodeTbl`, and the caller's
    /// simulation lemma cites each callee's simulation lemma at its call site.
    Composition {
        name: String,
        self_idx: u32,
        carrier: u32,
        /// The whole closure (caller + all transitively-reached callees, incl.
        /// the caller's own chain entry), sorted by `self_idx`. Every entry's
        /// body goes into the shared `CodeTbl`.
        closure: Vec<ClosureEntry>,
        /// Runtime contracts consumed anywhere in the closure.
        has_add: bool,
        has_sub: bool,
        has_box: bool,
    },
    /// One member of a mutually-recursive SCC `f n = if n≤0 then base else g(n-1)`
    /// (each member tail-calls the next in the cycle). Every member of the SCC is
    /// a certified export sharing ONE proof: a conjunction fuel-induction over the
    /// whole SCC where each cross-call is discharged by the matching conjunct of
    /// the induction hypothesis (the `mutual_sim` shape). The lowest-`self_idx`
    /// member is the "primary" that emits the shared code table + the conjunction
    /// bridge + `mutual_sim`; every member emits its own obligation citing the
    /// matching conjunct. Carries the whole (sorted) SCC so any member's cert can
    /// render the shared block.
    MutualRecursion {
        name: String,
        self_idx: u32,
        carrier: u32,
        box_idx: u32,
        sub_idx: u32,
        /// This member's index in `scc` (which conjunct of `mutual_sim`). The
        /// member's own `nlocals`/`base_k` live in `scc[position]`.
        position: usize,
        /// The whole SCC, sorted by `self_idx`.
        scc: Vec<MutualMember>,
    },
}

/// One function of a mutually-recursive SCC. The recogniser verified this
/// member's body against the fixed shape `f n = if n≤0 then base else g(n-1)`,
/// so the shared code table is reconstructed losslessly from these byte-derived
/// fields (base literal, cross-call target) the way `Cert::Recursive` is — no
/// need to carry the raw ops.
#[derive(Clone)]
struct MutualMember {
    name: String,
    self_idx: u32,
    nlocals: usize,
    /// Literal returned in the base arm (`n ≤ 0`).
    base_k: i64,
    /// The SCC member this one tail-calls in its step arm.
    cross_idx: u32,
}

impl Cert {
    fn inner(&self) -> &Cert {
        match self {
            Cert::NonRecursive { inner } => inner,
            _ => self,
        }
    }

    fn name(&self) -> &str {
        match self.inner() {
            Cert::StraightLine { name, .. }
            | Cert::Recursive { name, .. }
            | Cert::AccumulatorRecursive { name, .. }
            | Cert::AdtConstructor { name, .. }
            | Cert::FieldProjection { name, .. }
            | Cert::WidenedIntMatch { name, .. }
            | Cert::VerbatimWidenedMatch { name, .. }
            | Cert::VerbatimVariantDispatch { name, .. }
            | Cert::StringEqVerbatimMatch { name, .. }
            | Cert::IntRangePredicate { name, .. }
            | Cert::VariantDispatch { name, .. }
            | Cert::Composition { name, .. }
            | Cert::MutualRecursion { name, .. } => name,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    fn self_idx(&self) -> u32 {
        match self.inner() {
            Cert::StraightLine { self_idx, .. }
            | Cert::Recursive { self_idx, .. }
            | Cert::AccumulatorRecursive { self_idx, .. }
            | Cert::AdtConstructor { self_idx, .. }
            | Cert::FieldProjection { self_idx, .. }
            | Cert::WidenedIntMatch { self_idx, .. }
            | Cert::VerbatimWidenedMatch { self_idx, .. }
            | Cert::VerbatimVariantDispatch { self_idx, .. }
            | Cert::StringEqVerbatimMatch { self_idx, .. }
            | Cert::IntRangePredicate { self_idx, .. }
            | Cert::VariantDispatch { self_idx, .. }
            | Cert::Composition { self_idx, .. }
            | Cert::MutualRecursion { self_idx, .. } => *self_idx,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    fn carrier(&self) -> u32 {
        match self.inner() {
            Cert::StraightLine { carrier, .. }
            | Cert::Recursive { carrier, .. }
            | Cert::AccumulatorRecursive { carrier, .. }
            | Cert::AdtConstructor { carrier, .. }
            | Cert::FieldProjection { carrier, .. }
            | Cert::WidenedIntMatch { carrier, .. }
            | Cert::VerbatimWidenedMatch { carrier, .. }
            | Cert::VerbatimVariantDispatch { carrier, .. }
            | Cert::StringEqVerbatimMatch { carrier, .. }
            | Cert::IntRangePredicate { carrier, .. }
            | Cert::VariantDispatch { carrier, .. }
            | Cert::Composition { carrier, .. }
            | Cert::MutualRecursion { carrier, .. } => *carrier,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    fn arity(&self) -> usize {
        match self.inner() {
            Cert::StraightLine { .. } | Cert::Recursive { .. } | Cert::MutualRecursion { .. } => 1,
            Cert::AccumulatorRecursive { .. } => 2,
            Cert::AdtConstructor { arity, .. } => *arity,
            Cert::FieldProjection { .. }
            | Cert::WidenedIntMatch { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::IntRangePredicate { .. }
            | Cert::VariantDispatch { .. }
            | Cert::Composition { .. } => 1,
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The Lean expression for the model this export simulates.
    fn model_expr(&self) -> String {
        match self.inner() {
            Cert::StraightLine { k, .. } => format!("fun ns => ns.headD 0 + ({k})"),
            Cert::Recursive { name, .. }
            | Cert::Composition { name, .. }
            | Cert::MutualRecursion { name, .. } => {
                format!("fun ns => {name} (ns.headD 0)")
            }
            Cert::AccumulatorRecursive { name, .. } => {
                format!("fun ns => {name} (ns.headD 0) ((ns.drop 1).headD 0)")
            }
            Cert::AdtConstructor { .. }
            | Cert::FieldProjection { .. }
            | Cert::WidenedIntMatch { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::IntRangePredicate { .. }
            | Cert::VariantDispatch { .. } => "fun x => x".to_string(),
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The Lean expression for the 4-arg host builder in `Obligation` shape
    /// (`add → sub → mul → stringEq → HostTbl`). Every named host keeps its own arity; this
    /// wraps it to the obligation shape, ignoring the contracts it does not wire.
    fn host_expr(&self) -> String {
        match self.inner() {
            Cert::StraightLine { name, .. } => {
                format!("fun add _ _ _ => CertModule.{name}Host add")
            }
            Cert::Recursive {
                name, combinator, ..
            } => {
                // Draw the combinator slot (`add` or `mul`) from the obligation.
                format!(
                    "fun add sub mul _ => CertModule.{name}Host {} sub",
                    combinator.param()
                )
            }
            Cert::AccumulatorRecursive { name, .. } | Cert::Composition { name, .. } => {
                format!("fun add sub _ _ => CertModule.{name}Host add sub")
            }
            // The whole SCC shares one host (box + sub only), named after the
            // primary (lowest-`self_idx`) member; every member's obligation points
            // at it. `add`/`mul` are ignored (mutual has no combinator).
            Cert::MutualRecursion { scc, .. } => {
                format!("fun _ sub _ _ => CertModule.{}Host sub", scc[0].name)
            }
            Cert::AdtConstructor { name, .. } | Cert::FieldProjection { name, .. } => {
                format!("fun _ _ _ _ => CertModule.{name}Host")
            }
            Cert::WidenedIntMatch { name, .. }
            | Cert::VerbatimWidenedMatch { name, .. }
            | Cert::VerbatimVariantDispatch { name, .. }
            | Cert::IntRangePredicate { name, .. } => {
                format!("fun _ _ _ _ => CertModule.{name}Host")
            }
            Cert::StringEqVerbatimMatch { name, .. } => {
                format!("fun _ _ _ stringEq => CertModule.{name}Host stringEq")
            }
            Cert::VariantDispatch { name, .. } => {
                format!("fun add sub _ _ => CertModule.{name}Host add sub")
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    /// The source-level `Dom`/`Cod` type names recorded in the manifest JSON so
    /// `aver cert verify`/`explain` can surface WHAT is certified without reading
    /// Lean. Display-only (the semantic content is what the witness pins);
    /// rendered ASCII-safe.
    fn source_dom_cod(&self, model_info: &ModelInfo) -> (String, String) {
        let ascii = |s: &str| ascii_type_name(s);
        match self.inner() {
            Cert::StraightLine { .. }
            | Cert::Recursive { .. }
            | Cert::AccumulatorRecursive { .. }
            | Cert::Composition { .. }
            | Cert::MutualRecursion { .. } => ("List Int".to_string(), "Int".to_string()),
            Cert::FieldProjection { .. } => ("WVal x WVal".to_string(), "WVal".to_string()),
            Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. } => ("WVal".to_string(), "WVal".to_string()),
            Cert::IntRangePredicate { .. } => ("Int".to_string(), "Bool".to_string()),
            Cert::VariantDispatch { name, .. } | Cert::WidenedIntMatch { name, .. } => {
                let dom = model_info
                    .fns
                    .get(name)
                    .and_then(|s| s.params.first())
                    .map(|s| ascii(s))
                    .unwrap_or_else(|| "Op".to_string());
                (dom, "Int".to_string())
            }
            Cert::AdtConstructor { arity, .. } => {
                if adt_constructor_uses_model(self, model_info) {
                    let cod = model_info
                        .fns
                        .get(self.name())
                        .map(|s| ascii(&s.ret))
                        .unwrap_or_else(|| "Unit".to_string());
                    ("Int".to_string(), cod)
                } else {
                    let dom = if *arity == 1 {
                        "WVal".to_string()
                    } else {
                        "WVal x WVal".to_string()
                    };
                    (dom, "WVal".to_string())
                }
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
}

/// Render a Lean/source type name as printable ASCII for the manifest JSON: the
/// common math glyphs `×`/`→` become `x`/`->`, and any other non-ASCII byte is
/// dropped. Keeps a hostile-free, injection-free label the checker can display.
fn ascii_type_name(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '×' => out.push('x'),
            '→' => out.push_str("->"),
            c if c.is_ascii_graphic() || c == ' ' => out.push(c),
            _ => {}
        }
    }
    out
}

/// Classification of every user function in the module.
pub struct Analysis {
    certs: Vec<Cert>,
    declined: Vec<(String, String)>,
    carrier: Option<u32>,
    contracts: Vec<String>,
}

impl Analysis {
    pub fn certified_names(&self) -> Vec<String> {
        self.certs.iter().map(|c| c.name().to_string()).collect()
    }
    pub fn declined(&self) -> &[(String, String)] {
        &self.declined
    }
}

/// Disassemble the emitted module and classify each user function. `model_files`
/// are the reused `aver proof` Lean model; the recursion classifier reads the
/// combinator operator (`+`/`*`) from them since the bytes cannot tell the bignum
/// helpers apart.
pub fn analyze(wasm_bytes: &[u8], model_files: &[(String, String)]) -> Result<Analysis, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let model_ops = model_step_ops(model_files);

    // Index the user functions so the composition pass can walk the call graph.
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();

    let mut certs = Vec::new();
    let mut declined = Vec::new();
    for f in &user_fns {
        match classify(
            f,
            box_idx,
            carrier,
            &user_idx_set,
            &fns,
            &host_roles,
            &model_ops,
        ) {
            Ok(c) => certs.push(c),
            Err(reason) => declined.push((f.name.clone(), reason)),
        }
    }

    // Named runtime contracts actually consumed by the certified functions.
    let contracts = runtime_contracts_for_certs(&certs);

    Ok(Analysis {
        certs,
        declined,
        carrier,
        contracts,
    })
}

fn runtime_contracts_for_certs(certs: &[Cert]) -> Vec<String> {
    let mut contracts = Vec::new();
    let mut has_box = false;
    let mut has_add = false;
    let mut has_sub = false;
    let mut has_string_eq = false;
    for c in certs {
        match c.inner() {
            Cert::StraightLine { .. } => {
                has_box = true;
                has_add = true;
            }
            Cert::Recursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::AccumulatorRecursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
            Cert::AdtConstructor { .. }
            | Cert::FieldProjection { .. }
            | Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::IntRangePredicate { .. } => {}
            Cert::StringEqVerbatimMatch { .. } => {
                has_string_eq = true;
            }
            Cert::MutualRecursion { .. } => {
                // The shared host wires box + sub (no combinator).
                has_box = true;
                has_sub = true;
            }
            Cert::WidenedIntMatch { .. } => {
                has_box = true;
            }
            Cert::VariantDispatch {
                add_idx, sub_idx, ..
            } => {
                has_box = true;
                has_add |= add_idx.is_some();
                has_sub |= sub_idx.is_some();
            }
            Cert::Composition {
                has_add: a,
                has_sub: s,
                has_box: b,
                ..
            } => {
                has_add |= *a;
                has_sub |= *s;
                has_box |= *b;
            }
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }
    if has_box {
        contracts.push(BOX_CONTRACT.to_string());
    }
    if has_add {
        contracts.push(INT_ADD_CONTRACT.to_string());
    }
    if has_sub {
        contracts.push(INT_SUB_CONTRACT.to_string());
    }
    if has_string_eq {
        contracts.push(STRING_EQ_CONTRACT.to_string());
    }
    contracts
}

/// A certified obligation re-derived straight from the module bytes: the
/// `CodeTbl` body value, the fully-expanded host-builder value, the self
/// function index and the carrier type index — every one a pure function of the
/// hash-verified bytes, via the SAME audited `disassemble` → `classify` pipeline
/// the emitter uses. `aver cert verify` splices these into its checker-authored
/// witness and pins each with `rfl` against the matching `manifest.obligations`
/// projection, so an obligation whose `code`/`host`/`self`/`carrier` diverge
/// from the bytes (a fabricated or vacuous body) fails the kernel witness.
///
/// Trusted by inspection of the Aver disassembler (the consumer's own binary),
/// not by an in-kernel wasm decode proof; a full kernel decoder is a deferred
/// residual.
pub struct RederivedObligation {
    pub name: String,
    /// The `fun fn => ...` `CodeTbl` value (`render_code_value`).
    pub code: String,
    /// The fully-expanded host builder value (`render_host_value`).
    pub host: String,
    /// `Obligation.self`: the self function index in the module.
    pub self_idx: u32,
    /// `Obligation.carrier`: the Int carrier struct type index.
    pub carrier: u32,
    /// The BYTE-derived typed face: which standard `Dom`/`Cod`/`domRepr`/`codRepr`
    /// forms the honest emitter renders for this class. `aver cert verify` pins
    /// these into its witness, so a manifest weakening the semantic face
    /// (`Dom := Empty`, `codRepr := fun _ _ _ => True`, `domRepr := fun _ _ _ => False`,
    /// or a nerfed arity) fails a kernel `rfl`/`HEq.rfl` and is DECLINED.
    pub face: ObligationFace,
}

pub struct RederivedCertificate {
    pub obligations: Vec<RederivedObligation>,
    pub contracts: Vec<String>,
}

/// The byte-derived semantic face of a certified obligation — enough for the
/// checker to reconstruct, WITHOUT the source model, the standard typed forms
/// the honest emitter rendered. Derived from `classify` over the hash-verified
/// bytes, never from the untrusted JSON/Lean manifest.
#[derive(Clone)]
pub enum ObligationFace {
    /// Integer classes (straight-line / self-recursive / accumulator):
    /// `Dom := List Int`, `Cod := Int`, `codRepr := intRepr`,
    /// `domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = arity`.
    /// `arity` is the byte-bound argument count of the class (restores the
    /// v2 arity binding the v3 domRepr moved into an attacker-editable literal).
    IntList { arity: usize },
    /// Field projection: `Dom := WVal × WVal`, `Cod := WVal`,
    /// `codRepr := verbatimRepr`,
    /// `domRepr := fun _ p vs => vs = [.structv struct_idx [p.1, p.2]]`.
    Projection { struct_idx: u32 },
    /// Int -> Bool range predicate: `Dom := Int`, `Cod := Bool`,
    /// `codRepr := boolRepr`,
    /// `domRepr := fun _ cp vs => vs = [carrierSmall carrier cp]` (the canonical
    /// small-carrier domain — every constant and code point fits i64).
    IntPredicate,
    /// Verbatim widened match: `Dom := WVal`, `Cod := WVal`,
    /// `codRepr := verbatimRepr`, `domRepr := fun _ v vs => vs = [v]`. The model
    /// (a raw projection with a null default) is a read declaration; behaviour is
    /// pinned by executable interpreter tripwires.
    VerbatimWidened,
    /// ADT variant match: `Cod := Int`, `codRepr := intRepr`. `Dom` and
    /// `domRepr` are stated over a user-inductive `Repr` the checker cannot
    /// re-derive from bytes — a read declaration (only `Nonempty Dom` is pinned).
    AdtMatch,
    /// ADT constructor (verbatim pack or user-model): the whole typed face is
    /// over a user `Repr`/model the checker cannot re-derive from bytes — a read
    /// declaration. Only `Nonempty Dom` is pinned; an executable interpreter
    /// tripwire in the emitted certificate forces the constructor's behaviour.
    AdtConstructor,
}

impl ObligationFace {
    fn of_cert(c: &Cert) -> ObligationFace {
        match c.inner() {
            Cert::StraightLine { .. }
            | Cert::Recursive { .. }
            | Cert::AccumulatorRecursive { .. }
            | Cert::Composition { .. } => ObligationFace::IntList { arity: c.arity() },
            Cert::FieldProjection { struct_idx, .. } => ObligationFace::Projection {
                struct_idx: *struct_idx,
            },
            Cert::IntRangePredicate { .. } => ObligationFace::IntPredicate,
            Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. } => ObligationFace::VerbatimWidened,
            Cert::VariantDispatch { .. } | Cert::WidenedIntMatch { .. } => ObligationFace::AdtMatch,
            Cert::AdtConstructor { .. } => ObligationFace::AdtConstructor,
            // Each SCC member is an ordinary integer simulation face (arity 1);
            // the shared proof is what differs, not the typed obligation shape.
            Cert::MutualRecursion { .. } => ObligationFace::IntList { arity: 1 },
            Cert::NonRecursive { .. } => unreachable!(),
        }
    }

    /// One-line human description of the certified face for `verify`/`explain`:
    /// the trusted class plus the standard `Dom`/`Cod`/`codRepr` forms. `dom`
    /// and `cod` are the (charset-gated, untrusted) source type names, shown for
    /// the classes whose typed domain/codomain is a user declaration.
    pub fn describe(&self, dom: Option<&str>, cod: Option<&str>) -> String {
        match self {
            ObligationFace::IntList { arity } => format!(
                "class: integer simulation  |  Dom: List Int (arity {arity})  Cod: Int  codRepr: intRepr"
            ),
            ObligationFace::Projection { .. } => {
                "class: field projection  |  Dom: WVal x WVal  Cod: WVal  codRepr: verbatimRepr"
                    .to_string()
            }
            ObligationFace::IntPredicate => {
                "class: Int range predicate  |  Dom: Int (canonical small carrier)  Cod: Bool  codRepr: boolRepr"
                    .to_string()
            }
            ObligationFace::VerbatimWidened => {
                "class: verbatim widened match  |  Dom: WVal  Cod: WVal  codRepr: verbatimRepr  (model is a read declaration; behaviour pinned by an interpreter tripwire)"
                    .to_string()
            }
            ObligationFace::AdtMatch => {
                let d = dom.unwrap_or("<user type>");
                format!(
                    "class: ADT variant match  |  Dom: {d} (source-declared Repr, read)  Cod: Int  codRepr: intRepr"
                )
            }
            ObligationFace::AdtConstructor => {
                let d = dom.unwrap_or("<user type>");
                let cc = cod.unwrap_or("<user type>");
                format!(
                    "class: ADT constructor  |  Dom: {d}  Cod: {cc}  (typed face is a source-declared read declaration; behaviour pinned by an interpreter tripwire)"
                )
            }
        }
    }

    /// Lean `example` lines pinning the typed face of the obligation at position
    /// `idx` in `AverCert.manifest.obligations` to the STANDARD forms of this
    /// byte-derived class. Empty for `AdtConstructor` (read residue). A manifest
    /// that ships a weaker `Dom`/`Cod`/`domRepr`/`codRepr` fails one of these
    /// checks, so the witness does not check and `verify` declines.
    pub fn witness_pins(&self, idx: usize) -> String {
        // Reduce `obligations[idx]? = some o` to a concrete obligation, then
        // substitute so the dependent `codRepr`/`domRepr` fields have concrete
        // types before the `HEq.rfl`.
        let reduce = "simp only [AverCert.manifest, List.getElem?_cons_zero, \
                      List.getElem?_cons_succ, Option.some.injEq] at h";
        let obl = "AverCert.manifest.obligations";
        let mut s = String::new();
        // Domain inhabitation (every class): the obligation's `Dom` is a nonempty
        // type, so a `Dom := Empty` (which makes `holds` vacuously true) has no
        // `default` instance and this fails. Per-index so it is robust to an
        // obligation count that diverges from the manifest.
        s.push_str(&format!(
            "example : ∀ o, {obl}[{idx}]? = some o → Nonempty o.Dom := by\n  \
             intro o h\n  {reduce}\n  subst h; exact ⟨default⟩\n"
        ));
        match self {
            ObligationFace::IntList { arity } => {
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Dom) = some (List Int) := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Cod) = some Int := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o → \
                     HEq o.codRepr (@AverCert.Schema.intRepr o.carrier) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o →\n    \
                     HEq o.domRepr (fun (S : AverCert.Schema.CarrierSpec o.carrier) \
                     (ns : List Int) (vs : List CertPrelude.WVal) =>\n      \
                     AverCert.Schema.ReprAll S.Repr ns vs ∧ ns.length = {arity}) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
            }
            ObligationFace::Projection { struct_idx } => {
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Dom) = \
                     some (CertPrelude.WVal × CertPrelude.WVal) := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Cod) = some CertPrelude.WVal := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o → \
                     HEq o.codRepr (@AverCert.Schema.verbatimRepr o.carrier) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o →\n    \
                     HEq o.domRepr (fun (_S : AverCert.Schema.CarrierSpec o.carrier) \
                     (p : CertPrelude.WVal × CertPrelude.WVal) (vs : List CertPrelude.WVal) =>\n      \
                     vs = [.structv {struct_idx} [p.1, p.2]]) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
            }
            ObligationFace::IntPredicate => {
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Dom) = some Int := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Cod) = some Bool := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o → \
                     HEq o.codRepr (@AverCert.Schema.boolRepr o.carrier) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o →\n    \
                     HEq o.domRepr (fun (_S : AverCert.Schema.CarrierSpec o.carrier) \
                     (cp : Int) (vs : List CertPrelude.WVal) =>\n      \
                     vs = [CertPrelude.carrierSmall o.carrier cp]) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
            }
            ObligationFace::VerbatimWidened => {
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Dom) = some CertPrelude.WVal := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Cod) = some CertPrelude.WVal := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o → \
                     HEq o.codRepr (@AverCert.Schema.verbatimRepr o.carrier) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o →\n    \
                     HEq o.domRepr (fun (_S : AverCert.Schema.CarrierSpec o.carrier) \
                     (v : CertPrelude.WVal) (vs : List CertPrelude.WVal) =>\n      \
                     vs = [v]) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
            }
            ObligationFace::AdtMatch => {
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Cod) = some Int := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o → \
                     HEq o.codRepr (@AverCert.Schema.intRepr o.carrier) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
            }
            ObligationFace::AdtConstructor => {}
        }
        s
    }
}

/// Re-derive one [`RederivedObligation`] per user function that classifies into
/// a certified template, in module (obligation) order. The order and length
/// match `render_manifest_lean`'s `obligations` list, so the checker's
/// list-equality `rfl`s bind position for position.
pub fn rederive_obligations(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
) -> Result<Vec<RederivedObligation>, String> {
    Ok(rederive_certificate(wasm_bytes, model_files)?.obligations)
}

/// Re-derive the byte-bound certificate face: obligations plus the runtime
/// contracts those byte-classified obligations actually consume.
pub fn rederive_certificate(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
) -> Result<RederivedCertificate, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles) = disassemble(wasm_bytes)?;
    let model_ops = model_step_ops(model_files);
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();
    let mut certs = Vec::new();
    for f in &user_fns {
        if let Ok(c) = classify(
            f,
            box_idx,
            carrier,
            &user_idx_set,
            &fns,
            &host_roles,
            &model_ops,
        ) {
            certs.push(c);
        }
    }
    let contracts = runtime_contracts_for_certs(&certs);
    let obligations = certs
        .iter()
        .map(|c| RederivedObligation {
            name: c.name().to_string(),
            code: render_code_value(c),
            host: render_host_value(c),
            self_idx: c.self_idx(),
            carrier: c.carrier(),
            face: ObligationFace::of_cert(c),
        })
        .collect();
    Ok(RederivedCertificate {
        obligations,
        contracts,
    })
}

// ---- disassembly ---------------------------------------------------------

type DisasmResult = (
    Vec<UserFn>,
    u32,
    std::collections::HashSet<u32>,
    Option<u32>,
    std::collections::HashMap<u32, HostRole>,
);

fn disassemble(wasm_bytes: &[u8]) -> Result<DisasmResult, String> {
    use wasmparser::{
        CompositeInnerType, DataKind, Operator, Parser, Payload, StorageType, ValType,
    };

    let mut num_imported_funcs: u32 = 0;
    // defined-function index -> declared type index
    let mut func_type_idx: Vec<u32> = Vec::new();
    // type index -> byte-level signature (param kinds, result kind) for func types
    let mut type_sigs: std::collections::HashMap<u32, (Vec<TyKind>, Option<TyKind>)> =
        std::collections::HashMap::new();
    // type index -> struct field count
    let mut struct_field_counts: std::collections::HashMap<u32, u32> =
        std::collections::HashMap::new();
    // type indices for the string byte-array carrier `(array (mut i8))`.
    let mut string_byte_array_types: std::collections::HashSet<u32> =
        std::collections::HashSet::new();
    // export name -> func index
    let mut exports: Vec<(String, u32)> = Vec::new();
    let mut code_entries: Vec<CodeEntry> = Vec::new();
    let mut data_segments: Vec<Option<Vec<u8>>> = Vec::new();
    let mut carrier: Option<u32> = None;
    let mut next_type_idx: u32 = 0;

    for payload in Parser::new(0).parse_all(wasm_bytes) {
        let payload = payload.map_err(|e| format!("wasm parse: {e}"))?;
        match payload {
            Payload::TypeSection(reader) => {
                for rg in reader {
                    let rg = rg.map_err(|e| format!("type read: {e}"))?;
                    for sub in rg.into_types() {
                        let idx = next_type_idx;
                        next_type_idx += 1;
                        match &sub.composite_type.inner {
                            CompositeInnerType::Func(ft) => {
                                let kind = |vt: &ValType| match vt {
                                    ValType::I64 => TyKind::I64,
                                    ValType::I32 => TyKind::I32,
                                    ValType::F64 => TyKind::F64,
                                    ValType::Ref(rt) => match heap_type_index(rt.heap_type()) {
                                        Some(t) => TyKind::Ref(t),
                                        None => match rt.heap_type() {
                                            wasmparser::HeapType::Abstract {
                                                ty: wasmparser::AbstractHeapType::Eq,
                                                ..
                                            } => TyKind::Eqref,
                                            _ => TyKind::Other,
                                        },
                                    },
                                    _ => TyKind::Other,
                                };
                                type_sigs.insert(
                                    idx,
                                    (
                                        ft.params().iter().map(kind).collect(),
                                        ft.results().first().map(kind),
                                    ),
                                );
                            }
                            // Int carrier: 3 fields, {i64, ref, i32}.
                            CompositeInnerType::Struct(st)
                                if carrier.is_none()
                                    && st.fields.len() == 3
                                    && matches!(
                                        st.fields[0].element_type,
                                        StorageType::Val(ValType::I64)
                                    )
                                    && matches!(
                                        st.fields[2].element_type,
                                        StorageType::Val(ValType::I32)
                                    ) =>
                            {
                                carrier = Some(idx);
                                struct_field_counts.insert(idx, st.fields.len() as u32);
                            }
                            CompositeInnerType::Struct(st) => {
                                struct_field_counts.insert(idx, st.fields.len() as u32);
                            }
                            CompositeInnerType::Array(at)
                                if matches!(at.0.element_type, StorageType::I8) =>
                            {
                                string_byte_array_types.insert(idx);
                            }
                            _ => {}
                        }
                    }
                }
            }
            Payload::ImportSection(reader) => {
                // Compact import encoding groups imports; iterate each group.
                for group in reader {
                    let group = group.map_err(|e| format!("import read: {e}"))?;
                    for imp in group {
                        let (_, imp) = imp.map_err(|e| format!("import read: {e}"))?;
                        if let wasmparser::TypeRef::Func(_) = imp.ty {
                            num_imported_funcs += 1;
                        }
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                for t in reader {
                    func_type_idx.push(t.map_err(|e| format!("func read: {e}"))?);
                }
            }
            Payload::ExportSection(reader) => {
                for ex in reader {
                    let ex = ex.map_err(|e| format!("export read: {e}"))?;
                    if ex.kind == wasmparser::ExternalKind::Func {
                        exports.push((ex.name.to_string(), ex.index));
                    }
                }
            }
            Payload::CodeSectionEntry(body) => {
                let mut nlocals = 0usize;
                let mut lr = body
                    .get_locals_reader()
                    .map_err(|e| format!("locals reader: {e}"))?;
                for _ in 0..lr.get_count() {
                    let (n, _ty) = lr.read().map_err(|e| format!("locals read: {e}"))?;
                    nlocals += n as usize;
                }
                let mut ops = Vec::new();
                let mut calls = Vec::new();
                let mut has_loop_or_branch = false;
                let mut saw_i64_add = false;
                let mut saw_i64_sub = false;
                let mut first_i64_arith = None;
                let mut host_ops = Vec::new();
                let mut opr = body
                    .get_operators_reader()
                    .map_err(|e| format!("ops reader: {e}"))?;
                while !opr.eof() {
                    let op = opr.read().map_err(|e| format!("op read: {e}"))?;
                    host_ops.push(host_op(&op));
                    let mapped = match op {
                        Operator::LocalGet { local_index } => Op::LocalGet(local_index),
                        Operator::LocalSet { local_index } => Op::LocalSet(local_index),
                        Operator::I64Const { value } => Op::I64Const(value),
                        Operator::I32Const { value } => Op::I32Const(value),
                        Operator::F64Const { value } => Op::F64Const(value.bits()),
                        Operator::RefTestNonNull { hty } | Operator::RefTestNullable { hty } => {
                            heap_type_index(hty).map(Op::RefTest).unwrap_or(Op::Other)
                        }
                        Operator::RefCastNonNull { hty } | Operator::RefCastNullable { hty } => {
                            heap_type_index(hty).map(Op::RefCast).unwrap_or(Op::Other)
                        }
                        Operator::StructNew { struct_type_index } => Op::StructNew(
                            struct_type_index,
                            struct_field_counts
                                .get(&struct_type_index)
                                .copied()
                                .unwrap_or(0),
                        ),
                        Operator::StructGet {
                            struct_type_index,
                            field_index,
                        } => Op::StructGet(struct_type_index, field_index),
                        Operator::ArrayNewData {
                            array_type_index,
                            array_data_index,
                        } => {
                            let literal_operands =
                                match (ops.get(ops.len().wrapping_sub(2)), ops.last()) {
                                    (Some(Op::I32Const(0)), Some(Op::I32Const(len))) => Some(*len),
                                    _ => None,
                                };
                            if let Some(len) = literal_operands {
                                Op::ArrayNewDataUnresolved {
                                    type_idx: array_type_index,
                                    data_idx: array_data_index,
                                    offset: 0,
                                    len,
                                }
                            } else {
                                Op::Other
                            }
                        }
                        Operator::RefNull { .. } => Op::RefNull,
                        Operator::RefIsNull => Op::RefIsNull,
                        Operator::I64LeS => Op::I64LeS,
                        Operator::I64GeS => Op::I64GeS,
                        Operator::I64Add => {
                            saw_i64_add = true;
                            first_i64_arith.get_or_insert(HostRole::Add);
                            Op::Other
                        }
                        Operator::I64Sub => {
                            saw_i64_sub = true;
                            first_i64_arith.get_or_insert(HostRole::Sub);
                            Op::Other
                        }
                        Operator::I32LtS => Op::I32LtS,
                        Operator::I32GtS => Op::I32GtS,
                        Operator::If { .. } => Op::If,
                        Operator::Else => Op::Else,
                        Operator::End => Op::End,
                        Operator::Call { function_index } => {
                            calls.push(function_index);
                            Op::Call(function_index)
                        }
                        Operator::ReturnCall { function_index } => {
                            calls.push(function_index);
                            Op::ReturnCall(function_index)
                        }
                        Operator::Loop { .. }
                        | Operator::Block { .. }
                        | Operator::Br { .. }
                        | Operator::BrIf { .. }
                        | Operator::BrTable { .. } => {
                            has_loop_or_branch = true;
                            Op::Other
                        }
                        _ => Op::Other,
                    };
                    ops.push(mapped);
                }
                let host_role = match (saw_i64_add, saw_i64_sub) {
                    (true, false) => Some(HostRole::Add),
                    (false, true) => Some(HostRole::Sub),
                    _ => first_i64_arith,
                };
                code_entries.push(CodeEntry {
                    nlocals,
                    ops,
                    calls,
                    has_loop_or_branch,
                    host_role,
                    host_ops,
                });
            }
            Payload::DataSection(reader) => {
                for data in reader {
                    let data = data.map_err(|e| format!("data read: {e}"))?;
                    match data.kind {
                        DataKind::Passive => data_segments.push(Some(data.data.to_vec())),
                        DataKind::Active { .. } => data_segments.push(None),
                    }
                }
            }
            _ => {}
        }
    }

    // Runtime helper names never certified as code.
    let is_runtime = |name: &str| {
        name.starts_with("__rt_")
            || name.starts_with("__caller")
            || name == "_start"
            || name == "memory"
    };

    let box_idx = exports
        .iter()
        .find(|(n, _)| n == "__rt_aint_from_i64")
        .map(|(_, i)| *i)
        .ok_or_else(|| "module has no __rt_aint_from_i64 box helper".to_string())?;

    // user export name -> wasm func index
    let mut user_exports: Vec<(String, u32)> = exports
        .iter()
        .filter(|(n, _)| !is_runtime(n))
        .cloned()
        .collect();
    user_exports.sort_by_key(|(_, i)| *i);

    let user_idx_set: std::collections::HashSet<u32> =
        user_exports.iter().map(|(_, i)| *i).collect();

    let host_roles = code_entries
        .iter()
        .enumerate()
        .filter_map(|(def_idx, entry)| {
            let sig = func_type_idx
                .get(def_idx)
                .and_then(|ti| type_sigs.get(ti))
                .cloned()
                .unwrap_or((Vec::new(), None));
            let role = if is_string_eq_host(entry, &sig.0, sig.1, &string_byte_array_types) {
                Some(HostRole::StringEq)
            } else {
                entry.host_role
            };
            role.map(|role| (num_imported_funcs + def_idx as u32, role))
        })
        .collect::<std::collections::HashMap<_, _>>();

    let mut user_fns = Vec::new();
    for (name, wasm_idx) in user_exports {
        let Some(def_idx) = wasm_idx.checked_sub(num_imported_funcs) else {
            continue;
        };
        let Some(entry) = code_entries.get(def_idx as usize).cloned() else {
            continue;
        };
        let ops = resolve_data_ops(entry.ops, &data_segments);
        let (params, result) = func_type_idx
            .get(def_idx as usize)
            .and_then(|ti| type_sigs.get(ti))
            .cloned()
            .unwrap_or((Vec::new(), None));
        user_fns.push(UserFn {
            name,
            wasm_idx,
            arity: params.len(),
            params,
            result,
            nlocals: entry.nlocals,
            ops,
            calls: entry.calls,
            has_loop_or_branch: entry.has_loop_or_branch,
        });
    }

    Ok((user_fns, box_idx, user_idx_set, carrier, host_roles))
}

fn host_op(op: &wasmparser::Operator<'_>) -> HostOp {
    match op {
        wasmparser::Operator::LocalGet { local_index } => HostOp::LocalGet(*local_index),
        wasmparser::Operator::LocalSet { local_index } => HostOp::LocalSet(*local_index),
        wasmparser::Operator::I32Const { value } => HostOp::I32Const(*value),
        wasmparser::Operator::ArrayLen => HostOp::ArrayLen,
        wasmparser::Operator::ArrayGetU { array_type_index } => {
            HostOp::ArrayGetU(*array_type_index)
        }
        wasmparser::Operator::I32Ne => HostOp::I32Ne,
        wasmparser::Operator::I32GeU => HostOp::I32GeU,
        wasmparser::Operator::I32Add => HostOp::I32Add,
        wasmparser::Operator::If { .. } => HostOp::If,
        wasmparser::Operator::Block { .. } => HostOp::Block,
        wasmparser::Operator::Loop { .. } => HostOp::Loop,
        wasmparser::Operator::Br { relative_depth } => HostOp::Br(*relative_depth),
        wasmparser::Operator::BrIf { relative_depth } => HostOp::BrIf(*relative_depth),
        wasmparser::Operator::Return => HostOp::Return,
        wasmparser::Operator::End => HostOp::End,
        _ => HostOp::Other,
    }
}

fn is_string_eq_host(
    entry: &CodeEntry,
    params: &[TyKind],
    result: Option<TyKind>,
    string_byte_array_types: &std::collections::HashSet<u32>,
) -> bool {
    let [TyKind::Ref(lhs), TyKind::Ref(rhs)] = params else {
        return false;
    };
    if lhs != rhs || result != Some(TyKind::I32) || entry.nlocals != 2 || !entry.calls.is_empty() {
        return false;
    }
    use HostOp::*;
    let t = *lhs;
    if !string_byte_array_types.contains(&t) {
        return false;
    }
    let expected = [
        LocalGet(0),
        ArrayLen,
        LocalGet(1),
        ArrayLen,
        I32Ne,
        If,
        I32Const(0),
        Return,
        End,
        LocalGet(0),
        ArrayLen,
        LocalSet(2),
        I32Const(0),
        LocalSet(3),
        Block,
        Loop,
        LocalGet(3),
        LocalGet(2),
        I32GeU,
        BrIf(1),
        LocalGet(0),
        LocalGet(3),
        ArrayGetU(t),
        LocalGet(1),
        LocalGet(3),
        ArrayGetU(t),
        I32Ne,
        If,
        I32Const(0),
        Return,
        End,
        LocalGet(3),
        I32Const(1),
        I32Add,
        LocalSet(3),
        Br(0),
        End,
        End,
        I32Const(1),
        End,
    ];
    entry.host_ops.as_slice() == expected
}

fn resolve_data_ops(ops: Vec<Op>, data_segments: &[Option<Vec<u8>>]) -> Vec<Op> {
    ops.into_iter()
        .map(|op| match op {
            Op::ArrayNewDataUnresolved {
                type_idx,
                data_idx,
                offset,
                len,
            } if offset == 0 && len >= 0 => {
                let Some(Some(bytes)) = data_segments.get(data_idx as usize) else {
                    return Op::Other;
                };
                if bytes.len() == len as usize {
                    Op::ArrayNewData(type_idx, bytes.clone())
                } else {
                    Op::Other
                }
            }
            Op::ArrayNewDataUnresolved { .. } => Op::Other,
            other => other,
        })
        .collect()
}

fn heap_type_index(hty: wasmparser::HeapType) -> Option<u32> {
    match hty {
        wasmparser::HeapType::Concrete(idx) | wasmparser::HeapType::Exact(idx) => {
            idx.as_module_index()
        }
        wasmparser::HeapType::Abstract { .. } => None,
    }
}

// ---- classification ------------------------------------------------------

fn classify(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    fns: &std::collections::HashMap<u32, &UserFn>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
    model_ops: &std::collections::HashMap<String, char>,
) -> Result<Cert, String> {
    // Fuel self-recursion (single-argument `n + f(n-1)` / `n * f(n-1)` and
    // two-argument accumulator), recognised structurally from the instruction
    // tree. The base value is data (any literal / the accumulator) and the
    // combinator operation comes from the model, not a pinned constant.
    if let Some(cert) = recognize_fueled_recursion(f, box_idx, carrier, host_roles, model_ops) {
        return Ok(cert);
    }

    // Mutual-recursion SCC (each member tail-calls the next; the cycle closes),
    // proven by one conjunction fuel-induction over the whole SCC where every
    // cross-call is discharged by the matching conjunct of the induction
    // hypothesis (the `mutual_sim` shape validated in
    // probe-artifacts/mutual-fuel-probe/MutualSpike-twodefs.lean). The lowest-
    // `self_idx` member emits the shared code table + bridge + `mutual_sim`;
    // every member emits its own obligation citing the matching conjunct.
    if let Some(cert) = recognize_mutual_scc(f, box_idx, carrier, user_idx_set, fns, host_roles) {
        return Ok(cert);
    }

    if let Some(cert) = walk_nonrecursive(f, box_idx, carrier, user_idx_set, host_roles) {
        return Ok(Cert::NonRecursive {
            inner: Box::new(cert),
        });
    }

    // ---- decline with an honest reason -----------------------------------
    // Arity 2 is a supported signature (the accumulator-recursion template),
    // so a 2-argument function that did not match falls through to the
    // shape-based reasons below instead of a contradictory signature message.
    if f.arity != 1 && f.arity != 2 {
        return Err(format!(
            "unsupported signature ({} params); Stage-B templates cover one-argument Int functions and two-argument accumulator recursion",
            f.arity
        ));
    }
    if f.has_loop_or_branch {
        return Err(
            "body uses loops/branches outside the certified straight-line/recursive fragment"
                .to_string(),
        );
    }
    // Cross-function composition: a unary chain caller whose entire call closure
    // is itself certified by the straight-line integer shapes. A chain caller
    // whose closure leaves the classes declines with a specific reason.
    match try_composition(f, box_idx, carrier, user_idx_set, fns) {
        CompositionOutcome::Certified(c) => return Ok(*c),
        CompositionOutcome::Declined(reason) => return Err(reason),
        CompositionOutcome::NotApplicable => {}
    }
    let calls_other_user = f
        .calls
        .iter()
        .any(|c| *c != f.wasm_idx && user_idx_set.contains(c));
    if calls_other_user {
        return Err(
            "calls other user functions (cross-function / mutual recursion), outside Stage-B scope"
                .to_string(),
        );
    }
    if f.ops.iter().any(|o| matches!(o, Op::Other)) {
        return Err(
            "body uses opcodes outside the certified fragment (strings / ADTs / effects / tail calls)"
                .to_string(),
        );
    }
    Err("body does not match a certified template (straight-line add-constant, single-argument self-recursion, two-argument accumulator recursion, or non-recursive ADT constructor/projection/match)".to_string())
}

#[derive(Clone)]
enum InstrNode {
    Op(Op),
    IfElse(Vec<InstrNode>, Vec<InstrNode>),
}

struct StructuralBody {
    normalized_ops: Vec<Op>,
    tree: Vec<InstrNode>,
}

fn walk_nonrecursive(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    if f.arity == 0 || f.arity > 2 {
        return None;
    }
    let body = structural_body(f, box_idx, user_idx_set, host_roles)?;
    nr_straightline(f, &body, box_idx, carrier, host_roles)
        .or_else(|| nr_adt_constructor(f, &body, box_idx, carrier))
        .or_else(|| nr_field_projection(f, &body, carrier))
        .or_else(|| nr_ref_dispatch_match(f, &body, box_idx, carrier, host_roles))
        .or_else(|| nr_verbatim_variant_dispatch(f, &body, carrier))
        .or_else(|| nr_string_eq_verbatim_match(f, &body, carrier, host_roles))
        .or_else(|| nr_int_range_predicate(f, &body, carrier))
        .or_else(|| nr_variant_dispatch(f, &body, box_idx, carrier, host_roles))
}

/// General variant dispatch: walk a `ref.test` chain whose hit arms each
/// reduce to one recognised leaf and whose terminal else is a boxed constant.
/// Anything off this grammar returns `None` (falls through to the honest
/// decline reasons). Recognition keys on the parsed tree only — no full
/// opcode sequence is pinned, so arm count, order and per-arm semantics are
/// free within the leaf vocabulary.
fn nr_variant_dispatch(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    let carrier = carrier?;
    // Typed admission: the byte signature must be exactly "one user ADT value
    // in, one Int carrier out" — the claim-shape as the bytes declare it, not
    // a bare parameter count.
    if f.params.as_slice() != [TyKind::Eqref] || f.result != Some(TyKind::Ref(carrier)) {
        return None;
    }
    let (arms, default_k) = dispatch_chain(&body.tree, box_idx, host_roles)?;
    if arms.is_empty() {
        return None;
    }
    // No duplicate variant tags.
    let mut tags: Vec<u32> = arms.iter().map(|(t, _)| *t).collect();
    tags.sort_unstable();
    tags.dedup();
    if tags.len() != arms.len() {
        return None;
    }
    // At most one host helper per contract role across all arms.
    let mut add_idx = None;
    let mut sub_idx = None;
    for op in body.normalized_ops.iter() {
        let Op::Call(idx) = op else { continue };
        match host_roles.get(idx) {
            Some(HostRole::Add) => {
                if add_idx.is_some_and(|a: u32| a != *idx) {
                    return None;
                }
                add_idx = Some(*idx);
            }
            Some(HostRole::Sub) => {
                if sub_idx.is_some_and(|s: u32| s != *idx) {
                    return None;
                }
                sub_idx = Some(*idx);
            }
            Some(HostRole::StringEq) => return None,
            None => {}
        }
    }
    Some(Cert::VariantDispatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        arms,
        default_k,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

/// Parse `[localGet 0, refTest t, ifElse hit els]` where `els` continues the
/// chain or terminates in a boxed constant. Returns the arms in dispatch order
/// plus the default constant.
fn dispatch_chain(
    nodes: &[InstrNode],
    box_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<(Vec<(u32, ArmLeaf)>, i64)> {
    let [
        InstrNode::Op(Op::LocalGet(0)),
        InstrNode::Op(Op::RefTest(tag)),
        InstrNode::IfElse(hit, els),
    ] = nodes
    else {
        // Terminal else: a boxed integer constant.
        return match nodes {
            [InstrNode::Op(Op::I64Const(k)), InstrNode::Op(Op::Call(b))] if *b == box_idx => {
                Some((Vec::new(), *k))
            }
            _ => None,
        };
    };
    if has_branch(hit) {
        return None;
    }
    let leaf = leaf_of_arm(&node_ops(hit), *tag, box_idx, host_roles)?;
    let (mut rest, default_k) = dispatch_chain(els, box_idx, host_roles)?;
    rest.insert(0, (*tag, leaf));
    Some((rest, default_k))
}

/// Classify one hit arm as a leaf. The arm must open with the payload
/// projection `localGet 0; refCast tag; structGet tag 0`; the remainder is
/// either empty (projection), a boxed constant fed to a contracted host with
/// the payload first, or — through the emitter's one-local spill — the
/// constant first. Anything else: no leaf.
fn leaf_of_arm(
    ops: &[Op],
    tag: u32,
    box_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<ArmLeaf> {
    use Op::*;
    let rest = match ops {
        [LocalGet(0), RefCast(t), StructGet(t2, 0), rest @ ..] if t == &tag && t2 == &tag => rest,
        _ => return None,
    };
    let role = |idx: &u32| host_roles.get(idx).copied();
    match rest {
        [] => Some(ArmLeaf::Proj),
        // payload first: x op k
        [I64Const(k), Call(b), Call(h)] if *b == box_idx => Some(ArmLeaf::HostOp {
            role: role(h)?,
            k: *k,
            const_first: false,
        }),
        // constant first through the spill local: k op x
        [LocalSet(n), I64Const(k), Call(b), LocalGet(n2), Call(h)] if *b == box_idx && n == n2 => {
            Some(ArmLeaf::HostOp {
                role: role(h)?,
                k: *k,
                const_first: true,
            })
        }
        _ => None,
    }
}

fn structural_body(
    f: &UserFn,
    box_idx: u32,
    user_idx_set: &std::collections::HashSet<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<StructuralBody> {
    if f.has_loop_or_branch {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    if ops
        .iter()
        .any(|op| matches!(op, Op::Other | Op::ReturnCall(_)))
    {
        return None;
    }
    for op in ops {
        if let Op::Call(idx) = op {
            if *idx == f.wasm_idx || user_idx_set.contains(idx) {
                return None;
            }
            if *idx != box_idx && !host_roles.contains_key(idx) {
                return None;
            }
        }
    }
    let normalized_ops = normalize_local_hops(ops);
    let mut pos = 0usize;
    let tree = parse_instr_tree(&normalized_ops, &mut pos, false)?;
    if pos != normalized_ops.len() {
        return None;
    }
    Some(StructuralBody {
        normalized_ops,
        tree,
    })
}

fn parse_instr_tree(ops: &[Op], pos: &mut usize, nested: bool) -> Option<Vec<InstrNode>> {
    let mut out = Vec::new();
    while *pos < ops.len() {
        match &ops[*pos] {
            Op::Else | Op::End if nested => break,
            Op::If => {
                *pos += 1;
                let then_b = parse_instr_tree(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::Else)) {
                    return None;
                }
                *pos += 1;
                let else_b = parse_instr_tree(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::End)) {
                    return None;
                }
                *pos += 1;
                out.push(InstrNode::IfElse(then_b, else_b));
            }
            Op::Else | Op::End => return None,
            op => {
                out.push(InstrNode::Op(op.clone()));
                *pos += 1;
            }
        }
    }
    Some(out)
}

fn normalize_local_hops(ops: &[Op]) -> Vec<Op> {
    let mut aliases = std::collections::HashMap::<u32, u32>::new();
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < ops.len() {
        if let (Some(Op::LocalGet(src)), Some(Op::LocalSet(dst))) = (ops.get(i), ops.get(i + 1)) {
            let src = *aliases.get(src).unwrap_or(src);
            aliases.insert(*dst, src);
            i += 2;
            continue;
        }
        let op = match &ops[i] {
            Op::LocalGet(idx) => Op::LocalGet(*aliases.get(idx).unwrap_or(idx)),
            other => other.clone(),
        };
        out.push(op);
        i += 1;
    }

    let mut changed = true;
    while changed {
        changed = false;
        let mut compact = Vec::new();
        let mut j = 0usize;
        while j < out.len() {
            if j + 2 < out.len()
                && matches!(
                    out[j],
                    Op::StructGet(..)
                        | Op::RefCast(..)
                        | Op::I64Const(..)
                        | Op::I32Const(..)
                        | Op::F64Const(..)
                        | Op::RefNull
                        | Op::ArrayNewData(..)
                )
                && matches!((&out[j + 1], &out[j + 2]), (Op::LocalSet(a), Op::LocalGet(b)) if a == b)
            {
                compact.push(out[j].clone());
                j += 3;
                changed = true;
            } else {
                compact.push(out[j].clone());
                j += 1;
            }
        }
        out = compact;
    }
    out
}

fn flat_ops(nodes: &[InstrNode]) -> Vec<&Op> {
    let mut out = Vec::new();
    collect_flat_ops(nodes, &mut out);
    out
}

fn collect_flat_ops<'a>(nodes: &'a [InstrNode], out: &mut Vec<&'a Op>) {
    for node in nodes {
        match node {
            InstrNode::Op(op) => out.push(op),
            InstrNode::IfElse(then_b, else_b) => {
                collect_flat_ops(then_b, out);
                collect_flat_ops(else_b, out);
            }
        }
    }
}

fn node_ops(nodes: &[InstrNode]) -> Vec<Op> {
    flat_ops(nodes).into_iter().cloned().collect()
}

fn has_branch(nodes: &[InstrNode]) -> bool {
    nodes.iter().any(|node| match node {
        InstrNode::Op(_) => false,
        InstrNode::IfElse(..) => true,
    })
}

fn nr_straightline(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    use Op::*;
    if has_branch(&body.tree) {
        return None;
    }
    let [LocalGet(0), I64Const(k), Call(b), Call(a)] = body.normalized_ops.as_slice() else {
        return None;
    };
    if *b != box_idx || host_roles.get(a) != Some(&HostRole::Add) {
        return None;
    }
    Some(Cert::StraightLine {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        k: *k,
        box_idx,
        add_idx: *a,
    })
}

fn nr_adt_constructor(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
) -> Option<Cert> {
    use Op::*;
    if has_branch(&body.tree) || f.arity == 0 || f.arity > 2 {
        return None;
    }
    let ops = &body.normalized_ops;
    let (last, prefix) = ops.split_last()?;
    let StructNew(struct_idx, field_count) = last else {
        return None;
    };
    if *struct_idx == carrier? {
        return None;
    }
    let mut fields = Vec::new();
    for op in prefix {
        match op {
            LocalGet(i) if (*i as usize) < f.arity => fields.push(ConstructorField::Local(*i)),
            RefNull => fields.push(ConstructorField::Null),
            _ => return None,
        }
    }
    if fields.len() != *field_count as usize {
        return None;
    }
    let mut seen_locals = fields
        .iter()
        .filter_map(|field| match field {
            ConstructorField::Local(i) => Some(*i),
            ConstructorField::Null => None,
        })
        .collect::<Vec<_>>();
    seen_locals.sort_unstable();
    seen_locals.dedup();
    if seen_locals != (0..f.arity as u32).collect::<Vec<_>>() {
        return None;
    }
    if f.calls.iter().any(|c| *c == f.wasm_idx || *c != box_idx) {
        return None;
    }
    Some(Cert::AdtConstructor {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        struct_idx: *struct_idx,
        field_count: *field_count,
        arity: f.arity,
        fields,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn nr_field_projection(f: &UserFn, body: &StructuralBody, carrier: Option<u32>) -> Option<Cert> {
    use Op::*;
    if has_branch(&body.tree) || !f.calls.is_empty() {
        return None;
    }
    let carrier = carrier?;
    let mut gets = body
        .normalized_ops
        .iter()
        .filter_map(|op| match op {
            StructGet(t, field) => Some((*t, *field)),
            _ => None,
        })
        .collect::<Vec<_>>();
    if gets.len() != 1 {
        return None;
    }
    let (struct_idx, field_idx) = gets.pop()?;
    if struct_idx == carrier || field_idx > 1 {
        return None;
    }
    Some(Cert::FieldProjection {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        struct_idx,
        field_idx,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn nr_ref_dispatch_match(
    f: &UserFn,
    body: &StructuralBody,
    box_idx: u32,
    carrier: Option<u32>,
    _host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    let carrier = carrier?;
    if body
        .normalized_ops
        .iter()
        .take_while(|op| !matches!(op, Op::RefTest(_)))
        .any(|op| matches!(op, Op::StructNew(..)))
    {
        return None;
    }
    for pair in body.tree.windows(2) {
        let [
            InstrNode::Op(Op::RefTest(hit)),
            InstrNode::IfElse(hit_arm, miss_arm),
        ] = pair
        else {
            continue;
        };
        if *hit == carrier {
            continue;
        }
        let hit_ops = node_ops(hit_arm);
        if !hit_ops
            .iter()
            .any(|op| matches!(op, Op::StructGet(t, 0) if t == hit))
        {
            continue;
        }
        let miss_ops = node_ops(miss_arm);
        if matches!(miss_ops.as_slice(), [Op::I64Const(0), Op::Call(b)] if *b == box_idx) {
            return Some(Cert::WidenedIntMatch {
                name: f.name.clone(),
                self_idx: f.wasm_idx,
                nlocals: f.nlocals,
                carrier,
                hit_variant_idx: *hit,
                box_idx,
                ops: strip_trailing_end(&f.ops).to_vec(),
            });
        }
        if f.calls.is_empty()
            && let Some(default) = verbatim_default_from_ops(&miss_ops)
        {
            return Some(Cert::VerbatimWidenedMatch {
                name: f.name.clone(),
                self_idx: f.wasm_idx,
                nlocals: f.nlocals,
                carrier,
                hit_variant_idx: *hit,
                default,
                ops: strip_trailing_end(&f.ops).to_vec(),
            });
        }
    }
    None
}

fn verbatim_default_from_ops(ops: &[Op]) -> Option<VerbatimDefault> {
    match ops {
        [Op::RefNull] => Some(VerbatimDefault::Null),
        [Op::F64Const(bits)] => Some(VerbatimDefault::F64Bits(*bits)),
        [
            Op::I32Const(0),
            Op::I32Const(_),
            Op::ArrayNewData(type_idx, bytes),
        ] => Some(VerbatimDefault::Array {
            type_idx: *type_idx,
            bytes: bytes.clone(),
        }),
        _ => None,
    }
}

/// A `ref.test` dispatch over a user enum where EVERY arm — hit arms and the
/// terminal else — returns a VERBATIM constant (a String-literal `array.new_data`,
/// a null, or an f64), with no host, no arithmetic, no field projection and no
/// user calls (`unescapedChar`). The generalisation of the verbatim widened match
/// from "one projected hit + a default" to "k constant arms". Certified over
/// `Cod := WVal` / `verbatimRepr`, so no carrier or string representation is
/// needed. Distinct from the variant dispatch (whose hit arms project an Int
/// payload) and the verbatim widened match (whose single hit arm projects a
/// field); a pure-constant hit arm matches neither.
fn nr_verbatim_variant_dispatch(
    f: &UserFn,
    body: &StructuralBody,
    carrier: Option<u32>,
) -> Option<Cert> {
    let carrier = carrier?;
    if !f.calls.is_empty() {
        return None;
    }
    // Every arm is a pure verbatim constant: no box, host or any call op.
    if body
        .normalized_ops
        .iter()
        .any(|op| matches!(op, Op::Call(_)))
    {
        return None;
    }
    let (arms, default) = verbatim_dispatch_chain(&body.tree)?;
    if arms.is_empty() {
        return None;
    }
    let mut tags: Vec<u32> = arms.iter().map(|(t, _)| *t).collect();
    tags.sort_unstable();
    tags.dedup();
    if tags.len() != arms.len() {
        return None;
    }
    Some(Cert::VerbatimVariantDispatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        arms,
        default,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn nr_string_eq_verbatim_match(
    f: &UserFn,
    body: &StructuralBody,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    if f.arity != 1 {
        return None;
    }
    let [TyKind::Ref(param_ty)] = f.params.as_slice() else {
        return None;
    };
    if f.result != Some(TyKind::Ref(*param_ty)) {
        return None;
    }
    let (arms, default, string_eq_idx) = string_eq_verbatim_chain(&body.tree, host_roles)?;
    if arms.len() != 1 {
        return None;
    }
    Some(Cert::StringEqVerbatimMatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        string_eq_idx,
        arms,
        default,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn string_eq_verbatim_chain(
    nodes: &[InstrNode],
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<StringEqVerbatimChain> {
    use InstrNode::{IfElse, Op as Nop};
    let ops = node_ops(nodes);
    if matches!(ops.as_slice(), [Op::LocalGet(0)]) {
        return Some((Vec::new(), StringEqDefault::Input, 0));
    }
    if let Some(k) = verbatim_default_from_ops(&ops) {
        return Some((Vec::new(), StringEqDefault::Verbatim(k), 0));
    }

    let [
        Nop(Op::LocalGet(0)),
        maybe_cast @ ..,
        Nop(Op::I32Const(0)),
        Nop(Op::I32Const(_)),
        Nop(Op::ArrayNewData(type_idx, bytes)),
        Nop(Op::Call(eq_idx)),
        IfElse(hit, els),
    ] = nodes
    else {
        return None;
    };
    if !matches!(maybe_cast, [Nop(Op::RefCast(_))] | []) {
        return None;
    }
    if host_roles.get(eq_idx) != Some(&HostRole::StringEq) {
        return None;
    }
    let needle = VerbatimDefault::Array {
        type_idx: *type_idx,
        bytes: bytes.clone(),
    };
    let hit = verbatim_default_from_ops(&node_ops(hit))?;
    let (mut rest, default, rest_eq) = string_eq_verbatim_chain(els, host_roles)?;
    if rest_eq != 0 && rest_eq != *eq_idx {
        return None;
    }
    let mut arms = vec![(needle, hit)];
    arms.append(&mut rest);
    Some((arms, default, *eq_idx))
}

/// Parse `[localGet 0, refTest t, ifElse hit els]` where each `hit` is a verbatim
/// constant and `els` continues the chain or terminates in a verbatim constant.
/// Mirrors `dispatch_chain`, but every leaf is a byte-derived constant instead of
/// an Int projection.
fn verbatim_dispatch_chain(
    nodes: &[InstrNode],
) -> Option<(Vec<(u32, VerbatimDefault)>, VerbatimDefault)> {
    let [
        InstrNode::Op(Op::LocalGet(0)),
        InstrNode::Op(Op::RefTest(tag)),
        InstrNode::IfElse(hit, els),
    ] = nodes
    else {
        // Terminal else: a verbatim constant.
        return verbatim_default_from_ops(&node_ops(nodes)).map(|d| (Vec::new(), d));
    };
    if has_branch(hit) {
        return None;
    }
    let hit_const = verbatim_default_from_ops(&node_ops(hit))?;
    let (mut rest, default) = verbatim_dispatch_chain(els)?;
    rest.insert(0, (*tag, hit_const));
    Some((rest, default))
}

fn nr_int_range_predicate(f: &UserFn, body: &StructuralBody, carrier: Option<u32>) -> Option<Cert> {
    if f.arity != 1 || !f.calls.is_empty() {
        return None;
    }
    let carrier = carrier?;
    // The claim is a CONJUNCTION, so the branch structure is load-bearing: the
    // `>= k_lo` comparison feeds the final decision, whose then-arm evaluates
    // `<= k_hi` and whose else-arm is the literal boolean false. A flat scan
    // over both branches would also accept the disjunction spelled
    // `match x >= lo { true -> true; false -> x <= hi }` and certify a range
    // claim the body does not compute.
    let (InstrNode::IfElse(then_b, else_b), head) = body.tree.split_last()? else {
        return None;
    };
    if node_ops(else_b) != [Op::I32Const(0)] {
        return None;
    }
    let head_ops = node_ops_of(head);
    let then_ops = node_ops(then_b);
    let lo = single_comparison_bound(&head_ops, carrier, |op| matches!(op, Op::I64GeS))?;
    let hi = single_comparison_bound(&then_ops, carrier, |op| matches!(op, Op::I64LeS))?;
    if single_comparison_bound(&head_ops, carrier, |op| matches!(op, Op::I64LeS)).is_some()
        || single_comparison_bound(&then_ops, carrier, |op| matches!(op, Op::I64GeS)).is_some()
    {
        return None;
    }
    Some(Cert::IntRangePredicate {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        k_lo: lo,
        k_hi: hi,
        ops: strip_trailing_end(&f.ops).to_vec(),
    })
}

fn node_ops_of(nodes: &[InstrNode]) -> Vec<Op> {
    let mut out = Vec::new();
    collect_flat_ops(nodes, &mut out);
    out.into_iter().cloned().collect()
}

/// The bound `k` of the single `[structGet(carrier, 0), i64Const k, <cmp>]`
/// window in `ops` — `None` when absent or ambiguous (two occurrences).
fn single_comparison_bound(ops: &[Op], carrier: u32, cmp: fn(&Op) -> bool) -> Option<i64> {
    let mut found = None;
    for win in ops.windows(3) {
        if let [Op::StructGet(c, 0), Op::I64Const(k), op] = win
            && *c == carrier
            && cmp(op)
        {
            if found.is_some() {
                return None;
            }
            found = Some(*k);
        }
    }
    found
}

/// Outcome of the cross-function composition pass on a caller.
enum CompositionOutcome {
    /// The caller and its whole closure classify — a composition certificate.
    Certified(Box<Cert>),
    /// The caller IS a unary user-call chain, but its closure leaves the
    /// certified classes (an out-of-class callee, or a cycle). The specific,
    /// honest reason the caller declines.
    Declined(String),
    /// The caller is not a unary composition chain at all — let the ordinary
    /// decline reasons apply.
    NotApplicable,
}

/// Recognise one function as a straight-line integer shape usable inside a
/// composition closure: a self-sum (`x + x`) or a unary chain of user calls.
/// Returns `None` for anything else (a runtime/ADT/branch body, wrong arity).
fn classify_leaf_shape(
    f: &UserFn,
    user_idx_set: &std::collections::HashSet<u32>,
) -> Option<LeafShape> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    // Self-sum: [localGet 0, localGet 0, call add] where `add` is a host helper.
    if let [LocalGet(0), LocalGet(0), Call(a)] = ops
        && *a != f.wasm_idx
        && !user_idx_set.contains(a)
    {
        return Some(LeafShape::SelfSum { add_idx: *a });
    }
    // Unary chain: [localGet 0, call c1, ..., call cm] (m >= 1), each ci a user
    // function other than the caller itself. No other opcodes.
    if let Some((LocalGet(0), rest)) = ops.split_first()
        && !rest.is_empty()
        && rest.iter().all(|op| match op {
            Call(c) => *c != f.wasm_idx && user_idx_set.contains(c),
            _ => false,
        })
    {
        let calls = rest
            .iter()
            .map(|op| match op {
                Call(c) => *c,
                _ => unreachable!(),
            })
            .collect();
        return Some(LeafShape::Chain { calls });
    }
    None
}

/// Try to certify `f` as a cross-function composition. `f` qualifies only if its
/// own body is a unary user-call chain; then its transitive call closure must be
/// wholly covered by the straight-line integer shapes.
fn try_composition(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    fns: &std::collections::HashMap<u32, &UserFn>,
) -> CompositionOutcome {
    // Only a unary CHAIN caller (one that actually calls other user functions)
    // is a composition; a self-sum / non-chain body is handled elsewhere.
    match classify_leaf_shape(f, user_idx_set) {
        Some(LeafShape::Chain { .. }) => {
            let Some(carrier) = carrier else {
                return CompositionOutcome::Declined(
                    "carrier struct type not found in module".to_string(),
                );
            };
            let mut closure: std::collections::HashMap<u32, ClosureEntry> =
                std::collections::HashMap::new();
            let mut path: Vec<u32> = Vec::new();
            if let Err(reason) =
                collect_closure(f.wasm_idx, fns, user_idx_set, &mut closure, &mut path)
            {
                return CompositionOutcome::Declined(reason);
            }
            let mut entries: Vec<ClosureEntry> = closure.into_values().collect();
            entries.sort_by_key(|e| e.self_idx);
            let (has_add, has_sub, has_box) = closure_contracts(&entries);
            let _ = box_idx;
            CompositionOutcome::Certified(Box::new(Cert::Composition {
                name: f.name.clone(),
                self_idx: f.wasm_idx,
                carrier,
                closure: entries,
                has_add,
                has_sub,
                has_box,
            }))
        }
        _ => CompositionOutcome::NotApplicable,
    }
}

/// DFS the call graph from `idx`, classifying every reached function as a
/// straight-line integer shape. `path` is the active DFS stack (cycle guard);
/// `closure` collects each entry once. Fails closed on any out-of-class callee
/// or any cycle.
fn collect_closure(
    idx: u32,
    fns: &std::collections::HashMap<u32, &UserFn>,
    user_idx_set: &std::collections::HashSet<u32>,
    closure: &mut std::collections::HashMap<u32, ClosureEntry>,
    path: &mut Vec<u32>,
) -> Result<(), String> {
    if closure.contains_key(&idx) {
        return Ok(());
    }
    if path.contains(&idx) {
        let name = fns.get(&idx).map(|f| f.name.as_str()).unwrap_or("?");
        return Err(format!(
            "cycle in the call graph through user function `{name}`; composition requires an acyclic closure"
        ));
    }
    let Some(uf) = fns.get(&idx) else {
        return Err(
            "a callee in the composition closure is not an in-module user function".to_string(),
        );
    };
    let Some(shape) = classify_leaf_shape(uf, user_idx_set) else {
        return Err(format!(
            "callee `{}` is outside the certified composition classes (not a unary self-sum or a unary user-call chain)",
            uf.name
        ));
    };
    path.push(idx);
    if let LeafShape::Chain { calls } = &shape {
        for c in calls {
            collect_closure(*c, fns, user_idx_set, closure, path)?;
        }
    }
    path.pop();
    closure.insert(
        idx,
        ClosureEntry {
            name: uf.name.clone(),
            self_idx: idx,
            nlocals: uf.nlocals,
            ops: strip_trailing_end(&uf.ops).to_vec(),
            shape,
        },
    );
    Ok(())
}

/// `(has_add, has_sub, has_box)` runtime contracts consumed across the closure.
/// v1 leaves consume only carrier `add`; the flags keep the manifest honest as
/// the leaf vocabulary grows.
fn closure_contracts(entries: &[ClosureEntry]) -> (bool, bool, bool) {
    let mut has_add = false;
    for e in entries {
        if let LeafShape::SelfSum { .. } = e.shape {
            has_add = true;
        }
    }
    (has_add, false, false)
}

fn strip_trailing_end(ops: &[Op]) -> &[Op] {
    match ops.last() {
        Some(Op::End) => &ops[..ops.len() - 1],
        _ => ops,
    }
}

/// Structurally recognise fuel self-recursion from the parsed instruction tree:
///   f n     = if n≤0 then BASE else n + f (n-1)        (body-consumed, arity 1)
///   f n acc = if n≤0 then acc  else f (n-1) (acc + n)   (tail accumulator, arity 2)
/// The carrier-sign predicate preamble, the descent (`n-1`) and the combinator
/// (host `add`) are pinned; the BASE literal of the arity-1 shape is DATA (any
/// value, recovered from the bytes, not the fixed `0`). Recognition keys on the
/// tree shape + host roles — never on names or the base constant.
fn recognize_fueled_recursion(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
    model_ops: &std::collections::HashMap<String, char>,
) -> Option<Cert> {
    use Op::*;
    let carrier = carrier?;
    if f.arity != 1 && f.arity != 2 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    // Only the self-call, box, and contracted host helpers may be called; no
    // foreign user calls, no opaque ops. Must actually recurse.
    let mut recurses = false;
    for op in ops {
        match op {
            Op::Call(idx) | Op::ReturnCall(idx) => {
                if *idx == f.wasm_idx {
                    recurses = true;
                } else if *idx != box_idx && !host_roles.contains_key(idx) {
                    return None;
                }
            }
            Op::Other => return None,
            _ => {}
        }
    }
    if !recurses {
        return None;
    }
    let normalized = normalize_local_hops(ops);
    let mut pos = 0usize;
    let tree = parse_instr_tree(&normalized, &mut pos, false)?;
    if pos != normalized.len() {
        return None;
    }
    // preamble: localGet 0; structGet carrier 1; refIsNull;
    //           IfElse(sign-predicate); IfElse(base-arm, step-arm)
    let [
        InstrNode::Op(LocalGet(0)),
        InstrNode::Op(StructGet(c1, 1)),
        InstrNode::Op(RefIsNull),
        InstrNode::IfElse(pred_small, pred_big),
        InstrNode::IfElse(base_arm, step_arm),
    ] = tree.as_slice()
    else {
        return None;
    };
    if *c1 != carrier {
        return None;
    }
    // n≤0 predicate: small = [localGet 0, structGet c 0, i64Const 0, i64LeS];
    //               big   = [localGet 0, structGet c 2, i32Const 0, i32LtS]
    let small_ok = matches!(
        node_ops(pred_small).as_slice(),
        [LocalGet(0), StructGet(cc, 0), I64Const(0), I64LeS] if *cc == carrier
    );
    let big_ok = matches!(
        node_ops(pred_big).as_slice(),
        [LocalGet(0), StructGet(cc, 2), I32Const(0), I32LtS] if *cc == carrier
    );
    if !small_ok || !big_ok {
        return None;
    }
    let is_host = |idx: &u32, role: HostRole| host_roles.get(idx) == Some(&role);
    if f.arity == 1 {
        // base arm: [i64Const k, call box] — any literal k (the data-driven base).
        let base_k = match node_ops(base_arm).as_slice() {
            [I64Const(k), Call(b)] if *b == box_idx => *k,
            _ => return None,
        };
        // step arm: `<op>(_, _)` combining the self-call `f(sub(n,1))` with the
        // input `n` or a boxed constant, in either operand order — recovered by
        // symbolically executing the straight-line step (descent pinned to n-1).
        let (sub_idx, add_idx, rec_first, other) =
            parse_body_step(&node_ops(step_arm), box_idx, f.wasm_idx, host_roles)?;
        // Whether the combinator is `+` or `*` is not byte-distinguishable; read
        // it from the model operator, and decline fail-closed on anything else.
        let combinator = match model_ops.get(&f.name) {
            Some('+') => Combinator::Add,
            Some('*') => Combinator::Mul,
            _ => return None,
        };
        // The anti-vacuity guards evaluate the model at fixed samples; a large
        // multiplier or base can exceed i128 — decline fail-closed rather than
        // overflow-panic in the emitter.
        if [3i64, 0, -4]
            .iter()
            .any(|&s| eval_body_recursion(s, base_k, other, combinator).is_none())
        {
            return None;
        }
        Some(Cert::Recursive {
            name: f.name.clone(),
            self_idx: f.wasm_idx,
            nlocals: f.nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            base_k,
            rec_first,
            other,
            combinator,
        })
    } else {
        // arity 2: accumulator tail recursion; base arm returns the accumulator.
        if !matches!(node_ops(base_arm).as_slice(), [LocalGet(1)]) {
            return None;
        }
        // step arm: f(n-1, acc+n) as
        //   [localGet 0, i64Const 1, call box, call SUB, localGet 1, localGet 0, call ADD, returnCall SELF]
        let (sub_idx, add_idx) = match node_ops(step_arm).as_slice() {
            [
                LocalGet(0),
                I64Const(1),
                Call(b),
                Call(sub),
                LocalGet(1),
                LocalGet(0),
                Call(add),
                ReturnCall(sc),
            ] if *b == box_idx
                && *sc == f.wasm_idx
                && is_host(sub, HostRole::Sub)
                && is_host(add, HostRole::Add) =>
            {
                (*sub, *add)
            }
            _ => return None,
        };
        Some(Cert::AccumulatorRecursive {
            name: f.name.clone(),
            self_idx: f.wasm_idx,
            nlocals: f.nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
        })
    }
}

/// Recognise one mutual-recursion member body from the parsed tree:
///   `f n = if n≤0 then base else g (n-1)` — a tail cross-call to another user
/// function. Returns `(base_k, cross_idx, sub_idx)`; descent pinned to `n-1`.
/// Shares the carrier-sign preamble/predicate with `recognize_fueled_recursion`.
fn recognize_mutual_member(
    f: &UserFn,
    box_idx: u32,
    carrier: u32,
    user_idx_set: &std::collections::HashSet<u32>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<(i64, u32, u32)> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    // The only call is the box; the only tail-call is one other user function; no
    // self-call, no foreign user CALL, no opaque ops.
    let mut cross: Option<u32> = None;
    for op in ops {
        match op {
            Op::ReturnCall(idx) => {
                if *idx == f.wasm_idx || !user_idx_set.contains(idx) {
                    return None;
                }
                if cross.is_some_and(|c| c != *idx) {
                    return None;
                }
                cross = Some(*idx);
            }
            Op::Call(idx) if *idx != box_idx && !host_roles.contains_key(idx) => {
                return None;
            }
            Op::Other => return None,
            _ => {}
        }
    }
    let cross = cross?;
    let normalized = normalize_local_hops(ops);
    let mut pos = 0usize;
    let tree = parse_instr_tree(&normalized, &mut pos, false)?;
    if pos != normalized.len() {
        return None;
    }
    let [
        InstrNode::Op(LocalGet(0)),
        InstrNode::Op(StructGet(c1, 1)),
        InstrNode::Op(RefIsNull),
        InstrNode::IfElse(pred_small, pred_big),
        InstrNode::IfElse(base_arm, step_arm),
    ] = tree.as_slice()
    else {
        return None;
    };
    if *c1 != carrier {
        return None;
    }
    if !matches!(
        node_ops(pred_small).as_slice(),
        [LocalGet(0), StructGet(cc, 0), I64Const(0), I64LeS] if *cc == carrier
    ) || !matches!(
        node_ops(pred_big).as_slice(),
        [LocalGet(0), StructGet(cc, 2), I32Const(0), I32LtS] if *cc == carrier
    ) {
        return None;
    }
    let base_k = match node_ops(base_arm).as_slice() {
        [I64Const(k), Call(b)] if *b == box_idx => *k,
        _ => return None,
    };
    // step arm: [localGet 0, i64Const 1, call box, call SUB, returnCall CROSS]
    let sub_idx = match node_ops(step_arm).as_slice() {
        [LocalGet(0), I64Const(1), Call(b), Call(sub), ReturnCall(cc)]
            if *b == box_idx && *cc == cross && host_roles.get(sub) == Some(&HostRole::Sub) =>
        {
            *sub
        }
        _ => return None,
    };
    Some((base_k, cross, sub_idx))
}

/// Recognise a mutually-recursive SCC starting from `f`: follow the tail
/// cross-calls; the chain must return to `f` (a simple cycle) with every member
/// on it a mutual member. Produces this member's `Cert::MutualRecursion` carrying
/// the whole SCC (sorted by `self_idx` so the checker re-derives the same set).
fn recognize_mutual_scc(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
    fns: &std::collections::HashMap<u32, &UserFn>,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<Cert> {
    let carrier = carrier?;
    let (_, _, sub_idx) = recognize_mutual_member(f, box_idx, carrier, user_idx_set, host_roles)?;
    // Walk the cross-call chain from f; it must close back to f.
    let mut cycle: Vec<MutualMember> = Vec::new();
    let mut cur = f.wasm_idx;
    loop {
        let uf = fns.get(&cur)?;
        let (base_k, cross_idx, s) =
            recognize_mutual_member(uf, box_idx, carrier, user_idx_set, host_roles)?;
        if s != sub_idx {
            return None; // all members share one sub contract
        }
        cycle.push(MutualMember {
            name: uf.name.clone(),
            self_idx: cur,
            nlocals: uf.nlocals,
            base_k,
            cross_idx,
        });
        cur = cross_idx;
        if cur == f.wasm_idx {
            break;
        }
        if cycle.len() > 64 || cycle.iter().any(|m| m.self_idx == cur) {
            return None; // not a simple cycle through f
        }
    }
    if cycle.len() < 2 {
        return None; // a 1-cycle is ordinary self-recursion, handled elsewhere
    }
    let mut scc = cycle;
    scc.sort_by_key(|m| m.self_idx);
    let position = scc.iter().position(|m| m.self_idx == f.wasm_idx)?;
    Some(Cert::MutualRecursion {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        carrier,
        box_idx,
        sub_idx,
        position,
        scc,
    })
}

/// Symbolically execute a body-consumed fuel recursion's step arm — a
/// straight-line stack program ending in the host `add` — and recover
/// `(sub, add, rec_first, other)`: which host helpers are the descent/combinator,
/// which side of the `add` the recursive result sits on, and what the other
/// operand is. The descent is pinned to `sub(input, box 1)` = `n-1`; anything the
/// evaluator cannot account for (foreign locals, a second add, a non-descent
/// self-call argument) fails, so the recogniser stays fail-closed.
fn parse_body_step(
    ops: &[Op],
    box_idx: u32,
    self_idx: u32,
    host_roles: &std::collections::HashMap<u32, HostRole>,
) -> Option<(u32, u32, bool, BodyOperand)> {
    use Op::*;
    #[derive(Clone, Copy, PartialEq)]
    enum V {
        Input,
        IntLit(i64),
        Boxed(i64),
        Descent,
        Rec,
    }
    // The step is `<push operand A> <push operand B> add`; the trailing `add`
    // combines the top two stack values.
    let (last, init) = ops.split_last()?;
    let Call(add_idx) = last else { return None };
    if host_roles.get(add_idx) != Some(&HostRole::Add) {
        return None;
    }
    let mut st: Vec<V> = Vec::new();
    let mut sub_idx: Option<u32> = None;
    for op in init {
        match op {
            LocalGet(0) => st.push(V::Input),
            I64Const(k) => st.push(V::IntLit(*k)),
            Call(idx) if *idx == box_idx => {
                let V::IntLit(k) = st.pop()? else { return None };
                st.push(V::Boxed(k));
            }
            Call(idx) if *idx == self_idx => {
                if st.pop()? != V::Descent {
                    return None;
                }
                st.push(V::Rec);
            }
            Call(idx) if host_roles.get(idx) == Some(&HostRole::Sub) => {
                let b = st.pop()?;
                let a = st.pop()?;
                if a != V::Input || b != V::Boxed(1) {
                    return None;
                }
                if sub_idx.is_some_and(|s| s != *idx) {
                    return None;
                }
                sub_idx = Some(*idx);
                st.push(V::Descent);
            }
            _ => return None,
        }
    }
    // exactly the two `add` operands remain.
    let [a, b] = st.as_slice() else { return None };
    let operand = |v: &V| match v {
        V::Input => Some(BodyOperand::Input),
        V::Boxed(k) => Some(BodyOperand::Const(*k)),
        _ => None,
    };
    let (rec_first, other) = if *a == V::Rec {
        (true, operand(b)?)
    } else if *b == V::Rec {
        (false, operand(a)?)
    } else {
        return None;
    };
    Some((sub_idx?, *add_idx, rec_first, other))
}

/// The combinator operator of each `X__fuel` model definition's else-branch:
/// `+` (add) or `*` (mul). The bytes cannot distinguish the bignum helpers, so
/// this is the trusted source of the operation. Both the emitter (`analyze`) and
/// the checker (`rederive_certificate`) build the SAME map from the model, so the
/// re-derived host still pins. The descent (`n - 1`) uses `-`, so it never
/// confuses the scan; the recognised body shapes carry no other arithmetic.
fn model_step_ops(model_files: &[(String, String)]) -> std::collections::HashMap<String, char> {
    let mut ops = std::collections::HashMap::new();
    for (path, content) in model_files {
        if !path.ends_with(".lean") {
            continue;
        }
        let lines: Vec<&str> = content.lines().collect();
        for i in 0..lines.len() {
            let Some(rest) = lines[i].trim().strip_prefix("def ") else {
                continue;
            };
            let Some(fuel_pos) = rest.find("__fuel ") else {
                continue;
            };
            let name = rest[..fuel_pos].to_string();
            for l in lines.iter().skip(i).take(8) {
                if let Some(p) = l.find("else ") {
                    let els = &l[p + 5..];
                    let op = if els.contains('*') {
                        Some('*')
                    } else if els.contains('+') {
                        Some('+')
                    } else {
                        None
                    };
                    if let Some(op) = op {
                        ops.insert(name.clone(), op);
                    }
                    break;
                }
            }
        }
    }
    ops
}

// ---- model evaluation (anti-vacuity guard values) ------------------------
// The descent (`n-1`) is the pinned shape; the base is data and the combinator
// (`+` or `*`) is read from the model — so these compute the model value for any
// admitted base and operator without a per-function evaluator.

/// `f n = if n≤0 then base else other <op> f (n-1)` (body-consumed self-recursion),
/// where `<op>` is `+` or `*`. Both combinators commute, so operand order does not
/// affect the value; only the operator and the non-recursive operand do. Computed
/// with checked `i128`: a large multiplier or base can exceed the range, and the
/// caller declines fail-closed rather than emit a wrong (or overflowing) guard.
fn eval_body_recursion(
    n: i64,
    base: i64,
    other: BodyOperand,
    combinator: Combinator,
) -> Option<i128> {
    if n <= 0 {
        Some(base as i128)
    } else {
        let o = match other {
            BodyOperand::Input => n as i128,
            BodyOperand::Const(k) => k as i128,
        };
        let rec = eval_body_recursion(n - 1, base, other, combinator)?;
        match combinator {
            Combinator::Add => o.checked_add(rec),
            Combinator::Mul => o.checked_mul(rec),
        }
    }
}

/// `f n acc = if n≤0 then acc else f (n-1) (acc + n)` (accumulator tail recursion).
fn eval_accumulator(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        eval_accumulator(n - 1, acc + n)
    }
}

#[derive(Default)]
struct ModelInfo {
    fns: std::collections::HashMap<String, FnSig>,
    inductives: std::collections::HashMap<String, InductiveInfo>,
}

struct FnSig {
    params: Vec<String>,
    ret: String,
}

struct InductiveInfo {
    ctors: Vec<CtorInfo>,
}

struct CtorInfo {
    name: String,
    fields: Vec<String>,
}

impl ModelInfo {
    fn from_files(model_files: &[(String, String)]) -> Self {
        let mut info = Self::default();
        for (path, content) in model_files {
            if !path.ends_with(".lean") {
                continue;
            }
            info.parse_lean(content);
        }
        info
    }

    fn parse_lean(&mut self, content: &str) {
        let lines: Vec<&str> = content.lines().collect();
        let mut i = 0usize;
        while i < lines.len() {
            let line = lines[i].trim();
            if let Some(name) = line
                .strip_prefix("inductive ")
                .and_then(|s| s.split_whitespace().next())
            {
                i += 1;
                let mut ctors = Vec::new();
                while i < lines.len() {
                    let l = lines[i].trim();
                    if !l.starts_with('|') {
                        break;
                    }
                    let rest = l.trim_start_matches('|').trim();
                    let ctor_name = rest.split_whitespace().next().unwrap_or("").to_string();
                    let mut fields = Vec::new();
                    let mut tail = rest[ctor_name.len()..].trim();
                    while let Some(start) = tail.find("(_ : ") {
                        let after = &tail[start + 5..];
                        if let Some(end) = after.find(')') {
                            fields.push(after[..end].trim().to_string());
                            tail = &after[end + 1..];
                        } else {
                            break;
                        }
                    }
                    ctors.push(CtorInfo {
                        name: ctor_name,
                        fields,
                    });
                    i += 1;
                }
                self.inductives
                    .insert(name.to_string(), InductiveInfo { ctors });
                continue;
            }
            if line.starts_with("def ")
                && line.contains(" : ")
                && line.ends_with(":=")
                && let Some((name, sig)) = parse_def_sig(line)
            {
                self.fns.insert(name, sig);
            }
            i += 1;
        }
    }
}

fn parse_def_sig(line: &str) -> Option<(String, FnSig)> {
    let rest = line.strip_prefix("def ")?;
    let name = rest.split_whitespace().next()?.to_string();
    let after_name = rest[name.len()..].trim();
    let before_assign = after_name.strip_suffix(":=")?.trim();
    let ret_colon = before_assign.rfind(" : ")?;
    let params_part = before_assign[..ret_colon].trim();
    let ret = before_assign[ret_colon + 3..].trim().to_string();
    let mut params = Vec::new();
    let mut tail = params_part;
    while let Some(start) = tail.find('(') {
        let after = &tail[start + 1..];
        let end = after.find(')')?;
        let param = &after[..end];
        if let Some((_, ty)) = param.split_once(" : ") {
            params.push(ty.trim().to_string());
        }
        tail = &after[end + 1..];
    }
    Some((name, FnSig { params, ret }))
}

// ---- rendering -----------------------------------------------------------

/// Write the full `cert/` project. `model_files` are the (path, content) pairs
/// from the reused `aver proof` Lean emission (AverCommon + model modules).
pub fn write_project(
    out_dir: &Path,
    wasm_name: &str,
    wasm_bytes: &[u8],
    analysis: &Analysis,
    model_files: &[(String, String)],
) -> Result<(), String> {
    let cert_dir = out_dir.join("cert");
    std::fs::create_dir_all(&cert_dir).map_err(|e| format!("create cert dir: {e}"))?;

    // Copy in the semantics prelude + toolchain (single source of truth).
    write(&cert_dir, "CertPrelude.lean", CERT_PRELUDE)?;
    write(&cert_dir, "lean-toolchain", LEAN_TOOLCHAIN)?;

    // Copy the model files (AverCommon + <Module>.lean) verbatim.
    let mut model_roots: Vec<String> = Vec::new();
    for (path, content) in model_files {
        if path == "lakefile.lean" || path == "lean-toolchain" {
            continue;
        }
        write(&cert_dir, path, &sanitize_model_for_cert(content))?;
        if let Some(stem) = path.strip_suffix(".lean") {
            model_roots.push(stem.to_string());
        }
    }
    let model_info = ModelInfo::from_files(model_files);

    let sha = {
        let mut h = Sha256::new();
        h.update(wasm_bytes);
        hex(&h.finalize())
    };

    write(&cert_dir, "Contracts.lean", &render_contracts(analysis))?;
    write(
        &cert_dir,
        "Module.lean",
        &render_module(analysis, wasm_name, &sha),
    )?;
    // Audited statement schema (fixed) + generated manifest literal + the one
    // final theorem that composes the per-export obligations.
    write(&cert_dir, "Schema.lean", CERT_SCHEMA)?;
    write(
        &cert_dir,
        "Manifest.lean",
        &render_manifest_lean(analysis, &model_roots, &model_info, &sha),
    )?;
    write(
        &cert_dir,
        "Certificate.lean",
        &render_certificate(analysis, &model_roots, &model_info),
    )?;
    write(&cert_dir, "Final.lean", &render_final(analysis))?;
    write(&cert_dir, "lakefile.lean", &render_lakefile(&model_roots))?;

    // Content hashes the checker re-verifies: the audited schema and the
    // semantics prelude. Pinning these plus the final theorem name and the
    // manifest literal is the whole trust story.
    let schema_sha = sha256_hex(CERT_SCHEMA.as_bytes());
    let prelude_sha = sha256_hex(CERT_PRELUDE.as_bytes());
    std::fs::write(
        cert_dir.join("cert-manifest.json"),
        render_manifest(
            analysis,
            &model_info,
            wasm_name,
            &sha,
            &schema_sha,
            &prelude_sha,
        ),
    )
    .map_err(|e| format!("write manifest: {e}"))?;
    Ok(())
}

fn write(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    std::fs::write(dir.join(name), content).map_err(|e| format!("write {name}: {e}"))
}

fn sanitize_model_for_cert(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    for line in content.lines() {
        if line.trim_start().starts_with("deriving ") {
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

fn render_contracts(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "/-\n  Named runtime-layer contracts consumed by the certificates in this project.\n\n\
         Each is threaded as an explicit HYPOTHESIS of the certificate theorems (the\n\
         `hadd` / `hAdd` / `hSub` / `boxRef` faces in `Certificate.lean`), never as a\n\
         Lean `axiom`, so `#print axioms` on every certificate theorem stays on the\n\
         core whitelist `[propext, Classical.choice, Quot.sound]`. The obligations\n\
         below are the \"prove once per toolchain release\" runtime layer; the\n\
         machine-readable list is `cert-manifest.json`.\n\n",
    );
    if analysis.contracts.is_empty() {
        s.push_str("  (no user function was certified — no runtime contracts consumed)\n");
    } else {
        for c in &analysis.contracts {
            s.push_str(&format!("  * {c}\n"));
        }
    }
    s.push_str("-/\n");
    s
}

fn render_module(analysis: &Analysis, wasm_name: &str, sha: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "-- Emitted user-function bodies as `CertPrelude.WInstr` data, plus the\n\
         -- sha256 of the final `{wasm_name}.wasm` bytes (pinned).\n\
         import CertPrelude\n\nnamespace CertModule\nopen CertPrelude\n\n",
    ));
    s.push_str(&format!(
        "/-- sha256 of the certified `{wasm_name}.wasm` module bytes. -/\n\
         def wasmSha256 : String := \"{sha}\"\n\n",
    ));
    for c in &analysis.certs {
        s.push_str(&render_code_def(c));
        s.push('\n');
        s.push_str(&render_host_def(c));
        s.push('\n');
    }
    s.push_str("end CertModule\n");
    s
}

/// The runtime host-contract wiring for a certified body, as data in
/// `CertModule` so both the certificate proofs and the manifest reference the
/// one definition.
fn render_host_def(c: &Cert) -> String {
    match c.inner() {
        Cert::StraightLine {
            name,
            carrier,
            box_idx,
            add_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add contracts). -/\n\
             def {name}Host (add : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  else none\n",
        ),
        Cert::Recursive {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            combinator,
            ..
        } => {
            let cp = combinator.param();
            format!(
                "/-- Runtime host wiring for `{name}` (box + {cp} + sub contracts). -/\n\
                 def {name}Host ({cp} sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
                 if fn = {box_idx} then some (1, boxRef {carrier})\n  \
                 else if fn = {add_idx} then some (2, {cp})\n  \
                 else if fn = {sub_idx} then some (2, sub)\n  else none\n",
            )
        }
        Cert::AccumulatorRecursive {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add + sub contracts). -/\n\
             def {name}Host (add sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  \
             else if fn = {sub_idx} then some (2, sub)\n  else none\n",
        ),
        Cert::AdtConstructor { name, .. }
        | Cert::FieldProjection { name, .. }
        | Cert::VerbatimWidenedMatch { name, .. }
        | Cert::VerbatimVariantDispatch { name, .. }
        | Cert::IntRangePredicate { name, .. } => format!(
            "/-- Runtime host wiring for `{name}` (no host calls). -/\n\
             def {name}Host : HostTbl := fun _ => none\n",
        ),
        Cert::StringEqVerbatimMatch {
            name,
            string_eq_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (String.eq contract). -/\n\
             def {name}Host (stringEq : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {string_eq_idx} then some (2, stringEq)\n  else none\n",
        ),
        Cert::WidenedIntMatch {
            name,
            carrier,
            box_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box contract for the default `0`). -/\n\
             def {name}Host : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  else none\n",
        ),
        Cert::VariantDispatch {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => {
            let a = if add_idx.is_some() { "add" } else { "_add" };
            let s = if sub_idx.is_some() { "sub" } else { "_sub" };
            let mut chain = format!("if fn = {box_idx} then some (1, boxRef {carrier})");
            if let Some(i) = add_idx {
                chain.push_str(&format!("\n  else if fn = {i} then some (2, add)"));
            }
            if let Some(i) = sub_idx {
                chain.push_str(&format!("\n  else if fn = {i} then some (2, sub)"));
            }
            format!(
                "/-- Runtime host wiring for `{name}` (box + contracted arithmetic). -/\n\
                 def {name}Host ({a} {s} : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
                 {chain}\n  else none\n",
            )
        }
        Cert::Composition { name, closure, .. } => format!(
            "/-- Runtime host wiring for `{name}`'s call closure (add contract). -/\n\
             def {name}Host (add _sub : List WVal → Option WVal) : HostTbl := fun fn =>\n    {}\n",
            compose_host_arms(closure),
        ),
        // The whole SCC shares ONE host, emitted once by the primary member.
        Cert::MutualRecursion {
            scc,
            position,
            carrier,
            box_idx,
            sub_idx,
            ..
        } => {
            if *position != 0 {
                String::new()
            } else {
                let primary = &scc[0].name;
                format!(
                    "/-- Runtime host wiring for the mutual-recursive SCC `{primary}` (box + sub). -/\n\
                     def {primary}Host (sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
                     if fn = {box_idx} then some (1, boxRef {carrier})\n  \
                     else if fn = {sub_idx} then some (2, sub)\n  else none\n",
                )
            }
        }
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

fn render_code_def(c: &Cert) -> String {
    // The SCC shares ONE code table (all members' arms), named after the primary
    // (lowest-`self_idx`) member and emitted once by it.
    if let Cert::MutualRecursion { scc, position, .. } = c.inner() {
        if *position != 0 {
            return String::new();
        }
        let primary = &scc[0].name;
        return format!(
            "/-- Verbatim shared code table for the mutual-recursive SCC `{primary}` \
             (one arm per member). -/\n\
             def {primary}Code : CodeTbl := {value}\n",
            value = render_code_value(c),
        );
    }
    let doc = match c.inner() {
        Cert::StraightLine { .. } => "straight-line add-constant",
        Cert::Recursive { .. } => "self-recursive",
        Cert::AccumulatorRecursive { .. } => "accumulator self-recursive",
        Cert::AdtConstructor { .. } => "ADT constructor",
        Cert::FieldProjection { .. } => "field projection",
        Cert::WidenedIntMatch { .. } => "widened Int variant match",
        Cert::VerbatimWidenedMatch { .. } => "verbatim widened variant match",
        Cert::VerbatimVariantDispatch { .. } => "verbatim variant dispatch",
        Cert::StringEqVerbatimMatch { .. } => "verbatim String equality match",
        Cert::IntRangePredicate { .. } => "Int range predicate",
        Cert::VariantDispatch { .. } => "general variant dispatch",
        Cert::Composition { .. } => "cross-function composition, whole call closure",
        Cert::MutualRecursion { .. } => "mutual-recursive SCC",
        Cert::NonRecursive { .. } => unreachable!(),
    };
    format!(
        "/-- Verbatim emitted body of `{name}` ({doc}). -/\n\
         def {name}Code : CodeTbl := {value}\n",
        name = c.name(),
        value = render_code_value(c),
    )
}

/// The `CodeTbl` VALUE (the `fun fn => ...` lambda, no `def` wrapper) a
/// certified body decodes to. This is the term the checker splices, verbatim,
/// into `CheckerWitness.lean` and pins with `rfl` against
/// `manifest.obligations.map (·.code)`, so a `{name}Code` def in the cert's
/// `Module.lean` that diverges from the bytes fails the kernel witness. Kept
/// byte-identical to the RHS `render_code_def` emits so the emitted `Module.lean`
/// is unchanged.
/// An `Int` literal for Lean source: negatives parenthesised so `.i64Const -7`
/// does not misparse; non-negatives bare (byte-identical to the shipped `0`).
fn lean_int_lit(k: i64) -> String {
    if k < 0 {
        format!("({k})")
    } else {
        k.to_string()
    }
}

/// `i128` variant for anti-vacuity guard values, which can exceed `i64` for a
/// multiplication recursion (`Int` in Lean is unbounded, so the wide value is a
/// faithful guard).
fn lean_int_lit128(k: i128) -> String {
    if k < 0 {
        format!("({k})")
    } else {
        k.to_string()
    }
}

fn render_code_value(c: &Cert) -> String {
    match c.inner() {
        Cert::StraightLine {
            self_idx,
            nlocals,
            k,
            box_idx,
            add_idx,
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨1, {nlocals}, \
             [.localGet 0, .i64Const ({k}), .call {box_idx}, .call {add_idx}]⟩ else none",
        ),
        Cert::Recursive {
            self_idx,
            nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            base_k,
            rec_first,
            other,
            ..
        } => {
            let base = lean_int_lit(*base_k);
            // The step arm pushes the two `add` operands (the recursive result and
            // the other operand) in their recognised order, then calls `add`.
            let rec_ops = format!(
                ".localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, .call {self_idx}"
            );
            let other_ops = match other {
                BodyOperand::Input => ".localGet 0".to_string(),
                BodyOperand::Const(k) => format!(".i64Const {}, .call {box_idx}", lean_int_lit(*k)),
            };
            let (a_ops, b_ops) = if *rec_first {
                (&rec_ops, &other_ops)
            } else {
                (&other_ops, &rec_ops)
            };
            let step = format!("{a_ops}, {b_ops}, .call {add_idx}");
            format!(
                "fun fn =>\n  \
                 if fn = {self_idx} then some ⟨1, {nlocals},\n    \
                 [ .localGet 0, .localSet 1,\n      \
                 .localGet 1, .structGet {carrier} 1, .refIsNull,\n      \
                 .ifElse [.localGet 1, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
                 [.localGet 1, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
                 .ifElse [.i64Const {base}, .call {box_idx}]\n              \
                 [{step}] ]⟩\n  else none",
            )
        }
        Cert::AccumulatorRecursive {
            self_idx,
            nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨2, {nlocals},\n    \
             [ .localGet 0, .localSet 2,\n      \
             .localGet 2, .structGet {carrier} 1, .refIsNull,\n      \
             .ifElse [.localGet 2, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
             [.localGet 2, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
             .ifElse [.localGet 1]\n              \
             [.localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, \
             .localGet 1, .localGet 0, .call {add_idx}, .returnCall {self_idx}] ]⟩\n  else none",
        ),
        Cert::AdtConstructor {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::FieldProjection {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::WidenedIntMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::VerbatimWidenedMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::VerbatimVariantDispatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::StringEqVerbatimMatch {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::IntRangePredicate {
            self_idx,
            nlocals,
            ops,
            ..
        }
        | Cert::VariantDispatch {
            self_idx,
            nlocals,
            ops,
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨{arity}, {nlocals}, {body}⟩ else none",
            arity = c.arity(),
            body = render_ops_value(ops),
        ),
        Cert::Composition { closure, .. } => render_closure_code_value(closure),
        // Shared code table over the whole SCC: one `if fn = self then …` arm per
        // member, in `self_idx` order. Each arm is the fixed mutual body shape
        // (base boxes the member's literal; the else tail-calls the member's
        // cross target on `n-1`) — the same lossless template `Cert::Recursive`
        // uses, so it is byte-identical to the recognised bytes and to the term
        // the checker re-derives.
        Cert::MutualRecursion {
            scc,
            carrier,
            box_idx,
            sub_idx,
            ..
        } => {
            let mut s = String::from("fun fn =>\n  ");
            for (i, m) in scc.iter().enumerate() {
                let kw = if i == 0 { "if" } else { "else if" };
                let base = lean_int_lit(m.base_k);
                s.push_str(&format!(
                    "{kw} fn = {self_idx} then some ⟨1, {nlocals},\n    \
                     [ .localGet 0, .localSet 1,\n      \
                     .localGet 1, .structGet {carrier} 1, .refIsNull,\n      \
                     .ifElse [.localGet 1, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
                     [.localGet 1, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
                     .ifElse [.i64Const {base}, .call {box_idx}]\n              \
                     [.localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, .returnCall {cross}] ]⟩\n  ",
                    self_idx = m.self_idx,
                    nlocals = m.nlocals,
                    cross = m.cross_idx,
                ));
            }
            s.push_str("else none");
            s
        }
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

/// The multi-entry `CodeTbl` VALUE for a composition: one `if fn = i then …`
/// arm per function in the caller's whole call closure, in `self_idx` order.
/// The checker re-derives this from the bytes and pins the WHOLE table with one
/// `rfl`, so every callee body the caller's proof reduces through is byte-bound.
fn render_closure_code_value(closure: &[ClosureEntry]) -> String {
    let mut s = String::from("fun fn =>\n  ");
    for (i, e) in closure.iter().enumerate() {
        let kw = if i == 0 { "if" } else { "else if" };
        s.push_str(&format!(
            "{kw} fn = {idx} then some ⟨1, {nlocals}, {body}⟩\n  ",
            idx = e.self_idx,
            nlocals = e.nlocals,
            body = render_ops_value(&e.ops),
        ));
    }
    s.push_str("else none");
    s
}

/// The `Obligation.host` builder VALUE for a certified body, FULLY EXPANDED to
/// the box/add/sub wiring on the byte-derived indices — deliberately NOT a
/// reference to `CertModule.{name}Host` (which an attacker edits). The checker
/// splices this and pins it with `rfl` against `manifest.obligations.map
/// (·.host)`, so a nerfed host (e.g. `fun _ _ _ => none`, which would make
/// `holds` vacuous even with an honest `code`) fails the kernel witness.
/// Definitionally equal to the honest `render_host_def` builder.
fn render_host_value(c: &Cert) -> String {
    match c.inner() {
        Cert::StraightLine {
            carrier,
            box_idx,
            add_idx,
            ..
        } => format!(
            "fun add _ _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    else none",
        ),
        Cert::Recursive {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            combinator,
            ..
        } => {
            let cp = combinator.param();
            format!(
                "fun add sub mul _ => fun fn =>\n    \
                 if fn = {box_idx} then some (1, boxRef {carrier})\n    \
                 else if fn = {add_idx} then some (2, {cp})\n    \
                 else if fn = {sub_idx} then some (2, sub)\n    else none",
            )
        }
        Cert::AccumulatorRecursive {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "fun add sub _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
        Cert::AdtConstructor { .. }
        | Cert::FieldProjection { .. }
        | Cert::VerbatimWidenedMatch { .. }
        | Cert::VerbatimVariantDispatch { .. }
        | Cert::IntRangePredicate { .. } => "fun _ _ _ _ => fun _ => none".to_string(),
        Cert::StringEqVerbatimMatch { string_eq_idx, .. } => format!(
            "fun _ _ _ stringEq => fun fn =>\n    \
             if fn = {string_eq_idx} then some (2, stringEq)\n    else none"
        ),
        Cert::WidenedIntMatch {
            carrier, box_idx, ..
        } => format!(
            "fun _ _ _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    else none",
        ),
        Cert::VariantDispatch {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => {
            let a = if add_idx.is_some() { "add" } else { "_" };
            let s = if sub_idx.is_some() { "sub" } else { "_" };
            let mut chain = format!("if fn = {box_idx} then some (1, boxRef {carrier})");
            if let Some(i) = add_idx {
                chain.push_str(&format!("\n    else if fn = {i} then some (2, add)"));
            }
            if let Some(i) = sub_idx {
                chain.push_str(&format!("\n    else if fn = {i} then some (2, sub)"));
            }
            format!("fun {a} {s} _ _ => fun fn =>\n    {chain}\n    else none")
        }
        Cert::Composition { closure, .. } => {
            format!(
                "fun add _sub _ _ => fun fn =>\n    {}",
                compose_host_arms(closure)
            )
        }
        // Fully-expanded shared host (box + sub), definitionally equal to the
        // obligation's `fun _ sub _ => CertModule.{primary}Host sub`.
        Cert::MutualRecursion {
            carrier,
            box_idx,
            sub_idx,
            ..
        } => format!(
            "fun _ sub _ _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
        Cert::NonRecursive { .. } => unreachable!(),
    }
}

/// The host-table arms for a composition closure: each carrier-`add` helper the
/// closure calls wired to the `add` contract parameter, terminated by `none`.
/// v1 leaves consume only `add`; the arms grow with the leaf vocabulary.
fn compose_host_arms(closure: &[ClosureEntry]) -> String {
    let mut adds: Vec<u32> = closure
        .iter()
        .filter_map(|e| match e.shape {
            LeafShape::SelfSum { add_idx } => Some(add_idx),
            LeafShape::Chain { .. } => None,
        })
        .collect();
    adds.sort_unstable();
    adds.dedup();
    let mut s = String::new();
    for (i, a) in adds.iter().enumerate() {
        let kw = if i == 0 { "if" } else { "else if" };
        s.push_str(&format!("{kw} fn = {a} then some (2, add)\n    "));
    }
    s.push_str("else none");
    s
}

#[derive(Clone)]
enum LeanInstr {
    Simple(String),
    IfElse(Vec<LeanInstr>, Vec<LeanInstr>),
}

fn render_ops_value(ops: &[Op]) -> String {
    let mut pos = 0usize;
    let instrs = parse_lean_instrs(ops, &mut pos, false).unwrap_or_default();
    render_lean_instr_list(&instrs)
}

fn parse_lean_instrs(ops: &[Op], pos: &mut usize, nested: bool) -> Option<Vec<LeanInstr>> {
    let mut out = Vec::new();
    while *pos < ops.len() {
        match &ops[*pos] {
            Op::Else | Op::End if nested => break,
            Op::If => {
                *pos += 1;
                let then_b = parse_lean_instrs(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::Else)) {
                    return None;
                }
                *pos += 1;
                let else_b = parse_lean_instrs(ops, pos, true)?;
                if !matches!(ops.get(*pos), Some(Op::End)) {
                    return None;
                }
                *pos += 1;
                out.push(LeanInstr::IfElse(then_b, else_b));
            }
            Op::Else | Op::End => return None,
            op => {
                out.push(LeanInstr::Simple(render_simple_op(op)?));
                *pos += 1;
            }
        }
    }
    Some(out)
}

fn render_simple_op(op: &Op) -> Option<String> {
    Some(match op {
        Op::LocalGet(i) => format!(".localGet {i}"),
        Op::LocalSet(i) => format!(".localSet {i}"),
        Op::I64Const(n) => format!(".i64Const ({n})"),
        Op::I32Const(n) => format!(".i32Const ({n})"),
        Op::F64Const(bits) => format!(".f64Const 0x{bits:016x}"),
        Op::RefTest(t) => format!(".refTest {t}"),
        Op::RefCast(t) => format!(".refCast {t}"),
        Op::StructNew(t, n) => format!(".structNew {t} {n}"),
        Op::StructGet(t, f) => format!(".structGet {t} {f}"),
        Op::ArrayNewData(t, bytes) => format!(".arrayNewData {t} {}", render_nat_list(bytes)),
        Op::RefNull => ".refNull".to_string(),
        Op::RefIsNull => ".refIsNull".to_string(),
        Op::I64LeS => ".i64LeS".to_string(),
        Op::I64GeS => ".i64GeS".to_string(),
        Op::I32LtS => ".i32LtS".to_string(),
        Op::I32GtS => ".i32GtS".to_string(),
        Op::Call(f) => format!(".call {f}"),
        Op::ReturnCall(f) => format!(".returnCall {f}"),
        Op::ArrayNewDataUnresolved { .. } | Op::If | Op::Else | Op::End | Op::Other => {
            return None;
        }
    })
}

fn render_nat_list(bytes: &[u8]) -> String {
    let parts = bytes
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

fn render_wval(default: &VerbatimDefault) -> String {
    match default {
        VerbatimDefault::Null => ".null".to_string(),
        VerbatimDefault::F64Bits(bits) => format!(".f64v 0x{bits:016x}"),
        VerbatimDefault::Array { type_idx, bytes } => {
            format!(".arr {type_idx} {}", render_array_elements(bytes))
        }
    }
}

fn render_wval_arg(default: &VerbatimDefault) -> String {
    format!("({})", render_wval(default))
}

fn render_string_eq_default(default: &StringEqDefault, input: &str) -> String {
    match default {
        StringEqDefault::Input => input.to_string(),
        StringEqDefault::Verbatim(k) => render_wval(k),
    }
}

fn render_array_elements(bytes: &[u8]) -> String {
    let parts = bytes
        .iter()
        .map(|b| format!(".i32v {}", *b as i32))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

fn render_default_guard(default: &VerbatimDefault) -> String {
    match default {
        VerbatimDefault::Null => {
            "(fun w => match w with | .null => some 0 | _ => none) = some 0".to_string()
        }
        VerbatimDefault::F64Bits(bits) => format!(
            "(fun w => match w with | .f64v bits => some bits | _ => none) = some (0x{bits:016x} : UInt64)"
        ),
        VerbatimDefault::Array { type_idx, bytes } => format!(
            "(fun w => match w with | .arr t es => if t = {type_idx} ∧ es.length = {} then some 0 else none | _ => none) = some 0",
            bytes.len()
        ),
    }
}

fn render_lean_instr_list(instrs: &[LeanInstr]) -> String {
    let parts = instrs
        .iter()
        .map(render_lean_instr)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

fn render_lean_instr(instr: &LeanInstr) -> String {
    match instr {
        LeanInstr::Simple(s) => s.clone(),
        LeanInstr::IfElse(t, e) => format!(
            ".ifElse {} {}",
            render_lean_instr_list(t),
            render_lean_instr_list(e)
        ),
    }
}

fn render_certificate(
    analysis: &Analysis,
    model_roots: &[String],
    model_info: &ModelInfo,
) -> String {
    let mut s = String::new();
    s.push_str("import CertPrelude\nimport Module\nimport Schema\nimport Manifest\n");
    for r in model_roots {
        s.push_str(&format!("import {r}\n"));
    }
    s.push_str(
        "\nset_option linter.unusedSimpArgs false\n\
         set_option linter.unusedVariables false\n\
         set_option maxRecDepth 1000000\n\n\
         namespace CertProofs\nopen CertPrelude CertModule AverCert AverCert.Schema\n\n",
    );
    for c in &analysis.certs {
        match c.inner() {
            Cert::StraightLine { .. } => s.push_str(&render_straightline_cert(c)),
            Cert::Recursive { .. } | Cert::AccumulatorRecursive { .. } => {
                s.push_str(&render_fueled_recursion_cert(c))
            }
            Cert::AdtConstructor { .. } => s.push_str(&render_adt_constructor_cert(c, model_info)),
            Cert::FieldProjection {
                name,
                self_idx,
                carrier,
                struct_idx,
                field_idx,
                ..
            } => s.push_str(&render_struct_verbatim_cert(
                name,
                *self_idx,
                *carrier,
                *struct_idx,
                StructVerbatimShape::Project {
                    field_idx: *field_idx,
                },
            )),
            Cert::WidenedIntMatch { .. } | Cert::VariantDispatch { .. } => {
                s.push_str(&render_adt_match_cert(c, model_info))
            }
            Cert::IntRangePredicate { .. } => s.push_str(&render_int_range_predicate_cert(c)),
            Cert::VerbatimWidenedMatch { .. } | Cert::VerbatimVariantDispatch { .. } => {
                s.push_str(&render_verbatim_wval_match_cert(c))
            }
            Cert::StringEqVerbatimMatch { .. } => {
                s.push_str(&render_string_eq_verbatim_match_cert(c))
            }
            Cert::Composition { .. } => s.push_str(&render_composition_cert(c)),
            Cert::MutualRecursion { .. } => s.push_str(&render_mutual_recursion_cert(c)),
            Cert::NonRecursive { .. } => unreachable!(),
        }
        s.push('\n');
    }
    s.push_str("end CertProofs\n");
    s
}

fn render_straightline_cert(c: &Cert) -> String {
    let c = c.inner();
    let Cert::StraightLine {
        name,
        self_idx,
        carrier,
        k,
        box_idx,
        add_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let g1 = k + 3;
    let g2 = k - 5;
    let _ = (box_idx, add_idx);
    format!(
        r#"/-! ### {name} — straight-line certificate (carrier type {carrier}) -/

/-- The VERBATIM emitted body of `{name}` maps any representation of `n` to a
    representation of `n + {k}`, for ALL `n : ℤ`, under the named runtime
    contract `hadd` (carrier add = exact integer addition on represented values). -/
theorem {name}_wasm_certified
    (S : ReprSpec {carrier})
    (add : List WVal → Option WVal)
    (hadd : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w, wFuncN {name}Code ({name}Host add) 1 {self_idx} [v] = some w ∧ S.Repr (n + {k}) w := by
  intro n v hv
  obtain ⟨w, hw, hrepr⟩ := hadd n {k} v (carrierSmall {carrier} {k}) hv (S.smallIntro {k})
  refine ⟨w, ?_, hrepr⟩
  simp only [wFuncN, {name}Code, {name}Host, boxRef, carrierSmall, initLocals,
    wRunF, popArgs, List.getElem?_cons_zero, List.length, List.take, List.drop,
    List.reverse, List.replicate, if_true, reduceIte]
  simp only [carrierSmall] at hw
  simp [hw]

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the
    model value `n + {k}` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
    (S : ReprSpec {carrier})
    (add : List WVal → Option WVal)
    (hadd : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w m, wFuncN {name}Code ({name}Host add) 1 {self_idx} [v] = some w ∧ S.Repr m w ∧ m = n + {k} :=
  fun n v hv =>
    let ⟨w, hrun, hrepr⟩ := {name}_wasm_certified S add hadd n v hv
    ⟨w, n + {k}, hrun, hrepr, rfl⟩

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
def {name}HostRef : HostTbl := {name}Host (addRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g1}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} (-5)]).bind carrierToInt)
      = some ({g2}) := by native_decide

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem). Partial correctness over any fuel and representation. -/
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel ns vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hv htail =>
    rename_i n v ns vs
    cases htail with
    | nil =>
      cases fuel with
      | zero => simp only [wFuncN, reduceCtorEq] at hrun
      | succ f =>
        rcases hc : add [v, carrierSmall {carrier} ({k})] with _ | r
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun
          subst hrun
          simpa [AverCert.Schema.intRepr] using hadd n ({k}) v (carrierSmall {carrier} ({k})) r hv (S.smallIntro ({k})) hc
    | cons _ _ =>
      simp at harity
"#
    )
}

/// The single fuel-induction arm. One entry point for both recognised shapes:
/// the body-consumed recursion (`f n = if n≤0 then base else <combine>`, generic
/// over the base literal and the `add` combinator's operand and order) and the
/// two-argument tail accumulator (`f n acc = if n≤0 then acc else f (n-1) (acc+n)`).
/// The `induction fuel` skeleton, the model-side fuel bridge, and the carrier-sign
/// dispatch are shared; the shapes differ only in how the step arm reconstructs.
/// The `Repr` carrier hypotheses shared by every fuel-recursion certificate
/// theorem (`_wasm_certified` and `_wasm_faithful`, single- and two-argument
/// shapes alike): the representation relation and its four faces over carrier
/// `carrier`. Emitted once here instead of inline in each theorem.
fn recursion_repr_hyps(carrier: u32) -> String {
    format!(
        r#"    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)"#
    )
}

/// The per-shape pieces of one fuel-recursion certificate spliced into the shared
/// skeleton by [`render_fueled_recursion_cert`]. The single-argument body
/// recursion and the two-argument tail accumulator share the `induction fuel`
/// spine, the fuel bridge structure, the faithfulness composition and the schema
/// obligation; they differ only in these fields (arity, the threaded `acc`, and
/// the step reconstruction).
struct FuelPieces {
    doc_kind: &'static str,
    cert_kind: &'static str,
    vars: &'static str,
    bridge: String,
    comb_hyps: String,
    concl: String,
    zero_body: String,
    succ_body: String,
    faithful_concl: String,
    faithful_body: String,
    guards: String,
    simulates: String,
}

/// The single fuel-recursion certificate arm: one shared skeleton (doc header,
/// the shared `Repr` hypotheses, the two `#print axioms` lines, the anti-vacuity
/// and obligation structure) into which the recognised shape — single-argument
/// combinator recursion or two-argument tail accumulator — splices its pieces.
fn render_fueled_recursion_cert(c: &Cert) -> String {
    let name = c.name();
    let carrier = c.carrier();
    let hyps = recursion_repr_hyps(carrier);
    let p = match c.inner() {
        Cert::Recursive { .. } => recursive_fuel_pieces(c),
        Cert::AccumulatorRecursive { .. } => accumulator_fuel_pieces(c),
        _ => unreachable!(),
    };
    let FuelPieces {
        doc_kind,
        cert_kind,
        vars,
        bridge,
        comb_hyps,
        concl,
        zero_body,
        succ_body,
        faithful_concl,
        faithful_body,
        guards,
        simulates,
    } = p;
    format!(
        r#"/-! ### {name} — {doc_kind} certificate (carrier type {carrier}) -/

{bridge}

/-- THE CERTIFICATE THEOREM: partial correctness of the VERBATIM emitted
    {cert_kind} body against the generated model, for ALL {vars} : ℤ. -/
theorem {name}_wasm_certified
{hyps}
{comb_hyps}
{concl}
  intro fuel
  induction fuel with
  | zero =>
{zero_body}
  | succ fuel ih =>
{succ_body}

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the model
    value `{name} {vars}` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
{hyps}
{comb_hyps}
{faithful_concl}
{faithful_body}

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
{guards}

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem): the emitted {cert_kind} body simulates the model `{name}`. -/
{simulates}
"#
    )
}

fn recursive_fuel_pieces(c: &Cert) -> FuelPieces {
    let Cert::Recursive {
        name,
        self_idx,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        base_k,
        rec_first,
        other,
        combinator,
        ..
    } = c
    else {
        unreachable!()
    };
    let (rec_first, other, combinator) = (*rec_first, *other, *combinator);
    // Combinator contract, selected by the model operator: `+`/host `add` vs
    // `*`/host `mul`. `op` is the Lean operator, `cparam` the host param the
    // theorem binds, `chyp` its contract hypothesis, `cref` the reference face for
    // the anti-vacuity guard, `coblig` the obligation's contract in `_simulates`.
    let (op, cparam, chyp, cref, coblig) = match combinator {
        Combinator::Add => ("+", "add", "hAdd", "addRef", "hadd"),
        Combinator::Mul => ("*", "mul", "hMul", "mulRef", "hmul"),
    };
    // base + anti-vacuity guard values are data-driven from the recognised base
    // and combinator.
    let base = lean_int_lit(*base_k);
    // Guards are validated to fit i128 at recognition; expect that here.
    let ev = |s: i64| {
        eval_body_recursion(s, *base_k, other, combinator)
            .expect("guard fits (validated at recognition)")
    };
    let g3 = ev(3);
    let g0 = lean_int_lit128(ev(0));
    let gneg = lean_int_lit128(ev(-4));
    let _ = (box_idx, add_idx, sub_idx);
    // The combinator combines the recursive result `{name}(n-1)` with `other`
    // (the input, or a boxed constant); the proof cites `chyp` with the operands
    // in their recognised order.
    let other_expr = |input: &str| match other {
        BodyOperand::Input => input.to_string(),
        BodyOperand::Const(k) => lean_int_lit(k),
    };
    let step_rhs = {
        let rec_expr = format!("{name} (n - 1)");
        if rec_first {
            format!("{rec_expr} {op} {}", other_expr("n"))
        } else {
            format!("{} {op} {rec_expr}", other_expr("n"))
        }
    };
    // Per carrier arm: `input` names the model int (`s` small / `n` big) and
    // `input_wval` its byte form; produce the `add [..]` operand list and the
    // `hAdd ..` argument prefix (everything before the trailing `hadd`).
    let combinator_arm = |input: &str, input_wval: &str| -> (String, String) {
        let other_wval = match other {
            BodyOperand::Input => input_wval.to_string(),
            BodyOperand::Const(k) => format!("carrierSmall {carrier} {}", lean_int_lit(k)),
        };
        let other_repr = match other {
            BodyOperand::Input => "hv".to_string(),
            BodyOperand::Const(k) => format!("(hsmall_intro {})", lean_int_lit(k)),
        };
        let rec_int = format!("({name} ({input} - 1))");
        if rec_first {
            (
                format!("[vr, {other_wval}]"),
                format!("{rec_int} {} _ _ wa hrr {other_repr}", other_expr(input)),
            )
        } else {
            (
                format!("[{other_wval}, vr]"),
                format!("{} {rec_int} _ _ wa {other_repr} hrr", other_expr(input)),
            )
        }
    };
    let (add_small, hadd_small) = combinator_arm(
        "s",
        &format!(".structv {carrier} [.i64v s, .null, .i32v sg]"),
    );
    let (add_big, hadd_big) = combinator_arm(
        "n",
        &format!(".structv {carrier} [.i64v s, .arr lty les, .i32v sg]"),
    );
    FuelPieces {
        doc_kind: "self-recursive",
        cert_kind: "recursive",
        vars: "n",
        bridge: format!(
            r#"-- model-side fuel bridge (the cap-induction pattern at R = 1).
theorem {name}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {name}__fuel k1 n = {name}__fuel k2 n := by
  intro t
  induction t with
  | zero => intro k1 k2 n ht _ _; omega
  | succ t ih =>
      intro k1 k2 n ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [{name}__fuel, hn]
      · have hrec := ih m1 m2 (n - 1) (by omega) (by omega) (by omega)
        simp only [{name}__fuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem {name}_fuel_stable (k : Nat) (n : Int) (h : n.natAbs < k) :
    {name}__fuel k n = {name} n :=
  {name}_fuel_irrel (n.natAbs + k + 1) k (n.natAbs + 1) n (by omega) h (by omega)

theorem {name}_step (n : Int) (hn : ¬ n ≤ 0) : {name} n = {step_rhs} := by
  have h0 : {name} n = {name}__fuel (n.natAbs + 1) n := rfl
  rw [h0]
  simp only [{name}__fuel]
  rw [if_neg hn, {name}_fuel_stable n.natAbs (n - 1) (by omega)]

theorem {name}_base (n : Int) (hn : n ≤ 0) : {name} n = {base} := by
  have h0 : {name} n = {name}__fuel (n.natAbs + 1) n := rfl
  rw [h0]; simp [{name}__fuel, hn]"#
        ),
        comb_hyps: format!(
            r#"    ({cparam} sub : List WVal → Option WVal)
    ({chyp} : ∀ a b va vb w, Repr a va → Repr b vb → {cparam} [va, vb] = some w → Repr (a {op} b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :"#
        ),
        concl: format!(
            r#"    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [v] = some w →
      Repr ({name} n) w := by"#
        ),
        zero_body: "      intro n v w hv hrun\n      simp [wFuncN] at hrun".to_string(),
        succ_body: format!(
            r#"      intro n v w hv hrun
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rw [{name}_base s hle, ← hrun]
          exact hsmall_intro {base}
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .null, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({name} (s - 1)) vr := ih (s - 1) vd vr hrd hrec
              rcases hadd : {cparam} {add_small} with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step s hle, ← hrun]
                exact {chyp} {hadd_small} hadd
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rw [{name}_base n hn0, ← hrun]
          exact hsmall_intro {base}
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .arr lty les, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (n - 1) vd :=
              hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({name} (n - 1)) vr := ih (n - 1) vd vr hrd hrec
              rcases hadd : {cparam} {add_big} with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step n hn0, ← hrun]
                exact {chyp} {hadd_big} hadd"#
        ),
        faithful_concl: format!(
            r#"    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host {cparam} sub) fuel {self_idx} [v] = some w →
      ∃ m : Int, Repr m w ∧ m = {name} n :="#
        ),
        faithful_body: format!(
            r#"  fun fuel n v w hv hrun =>
    ⟨{name} n,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig {cparam} sub {chyp} hSub fuel n v w hv hrun,
     rfl⟩"#
        ),
        guards: format!(
            r#"def {name}HostRef : HostTbl := {name}Host ({cref} {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 0]).bind carrierToInt)
      = some {g0} := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4)]).bind carrierToInt)
      = some {gneg} := by native_decide"#
        ),
        simulates: format!(
            r#"theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel ns vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hv htail =>
      rename_i n v ns vs
      cases htail with
      | nil =>
          simpa [AverCert.Schema.intRepr] using {name}_wasm_certified S.Repr S.car S.smallIntro S.smallElim S.bigElim
            {cparam} sub {coblig} hsub fuel n v w hv hrun
      | cons _ _ =>
          simp at harity"#
        ),
    }
}

fn accumulator_fuel_pieces(c: &Cert) -> FuelPieces {
    let Cert::AccumulatorRecursive {
        name,
        self_idx,
        carrier,
        ..
    } = c
    else {
        unreachable!()
    };
    let g3 = eval_accumulator(3, 0);
    let g4 = eval_accumulator(3, 4);
    let gneg = eval_accumulator(-4, 9);
    FuelPieces {
        doc_kind: "accumulator self-recursive",
        cert_kind: "accumulator-recursive",
        vars: "n acc",
        bridge: format!(
            r#"-- model-side fuel bridge (fuel induction; the IH is quantified over both args).
theorem {name}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n acc : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {name}__fuel k1 n acc = {name}__fuel k2 n acc := by
  intro t
  induction t with
  | zero => intro k1 k2 n acc ht _ _; omega
  | succ t ih =>
      intro k1 k2 n acc ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [{name}__fuel, hn]
      · have hrec := ih m1 m2 (n - 1) (acc + n) (by omega) (by omega) (by omega)
        simp only [{name}__fuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem {name}_fuel_stable (k : Nat) (n acc : Int) (h : n.natAbs < k) :
    {name}__fuel k n acc = {name} n acc :=
  {name}_fuel_irrel (n.natAbs + k + 1) k (n.natAbs + 1) n acc (by omega) h (by omega)

theorem {name}_step (n acc : Int) (hn : ¬ n ≤ 0) :
    {name} n acc = {name} (n - 1) (acc + n) := by
  have h0 : {name} n acc = {name}__fuel (n.natAbs + 1) n acc := rfl
  rw [h0]
  simp only [{name}__fuel]
  rw [if_neg hn, {name}_fuel_stable n.natAbs (n - 1) (acc + n) (by omega)]

theorem {name}_base (n acc : Int) (hn : n ≤ 0) : {name} n acc = acc := by
  have h0 : {name} n acc = {name}__fuel (n.natAbs + 1) n acc := rfl
  rw [h0]; simp [{name}__fuel, hn]"#
        ),
        comb_hyps: r#"    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :"#
            .to_string(),
        concl: format!(
            r#"    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal), Repr n vn → Repr acc vacc →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vn, vacc] = some w →
      Repr ({name} n acc) w := by"#
        ),
        zero_body: "      intro n acc vn vacc w hvn hvacc hrun\n      simp [wFuncN] at hrun"
            .to_string(),
        succ_body: format!(
            r#"      intro n acc vn vacc w hvn hvacc hrun
      rcases hcar n vn hvn with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hvn
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rw [{name}_base s acc hle, ← hrun]
          exact hvacc
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .null, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hvn (hsmall_intro 1) hsub
            rcases hadd : add [vacc, .structv {carrier} [.i64v s, .null, .i32v sg]] with _ | va
            · simp [hadd] at hrun
            · simp only [hadd] at hrun
              have hra : Repr (acc + s) va :=
                hAdd acc s _ _ va hvacc hvn hadd
              rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd, va] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec, Option.some.injEq] at hrun
                rw [{name}_step s acc hle, ← hrun]
                exact ih (s - 1) (acc + s) vd va vr hrd hra hrec
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hvn
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rw [{name}_base n acc hn0, ← hrun]
          exact hvacc
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .arr lty les, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (n - 1) vd :=
              hSub n 1 _ _ vd hvn (hsmall_intro 1) hsub
            rcases hadd : add [vacc, .structv {carrier} [.i64v s, .arr lty les, .i32v sg]] with _ | va
            · simp [hadd] at hrun
            · simp only [hadd] at hrun
              have hra : Repr (acc + n) va :=
                hAdd acc n _ _ va hvacc hvn hadd
              rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd, va] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec, Option.some.injEq] at hrun
                rw [{name}_step n acc hn0, ← hrun]
                exact ih (n - 1) (acc + n) vd va vr hrd hra hrec"#
        ),
        faithful_concl: format!(
            r#"    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal), Repr n vn → Repr acc vacc →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vn, vacc] = some w →
      ∃ m : Int, Repr m w ∧ m = {name} n acc :="#
        ),
        faithful_body: format!(
            r#"  fun fuel n acc vn vacc w hvn hvacc hrun =>
    ⟨{name} n acc,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig add sub hAdd hSub fuel n acc vn
       vacc w hvn hvacc hrun,
     rfl⟩"#
        ),
        guards: format!(
            r#"def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3, carrierSmall {carrier} 0]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3, carrierSmall {carrier} 4]).bind carrierToInt)
      = some ({g4}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4), carrierSmall {carrier} 9]).bind carrierToInt)
      = some {gneg} := by native_decide"#
        ),
        simulates: format!(
            r#"theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel ns vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hvn htail =>
      rename_i n vn ns1 vs1
      cases htail with
      | nil =>
          simp at harity
      | cons hvacc htail2 =>
          rename_i acc vacc ns2 vs2
          cases htail2 with
          | nil =>
              simpa [AverCert.Schema.intRepr] using {name}_wasm_certified S.Repr S.car S.smallIntro S.smallElim S.bigElim
                add sub hadd hsub fuel n acc vn vacc w hvn hvacc hrun
          | cons _ _ =>
              simp at harity"#
        ),
    }
}

/// Post-order (callees-before-callers) topological order of a composition
/// closure, starting the DFS at the caller so the caller comes last. Every
/// closure is an acyclic user-call DAG (enforced by `collect_closure`).
fn compose_topo_order(caller_idx: u32, closure: &[ClosureEntry]) -> Vec<u32> {
    let by_idx: std::collections::HashMap<u32, &ClosureEntry> =
        closure.iter().map(|e| (e.self_idx, e)).collect();
    let mut order = Vec::new();
    let mut seen = std::collections::HashSet::new();
    fn dfs(
        idx: u32,
        by_idx: &std::collections::HashMap<u32, &ClosureEntry>,
        seen: &mut std::collections::HashSet<u32>,
        order: &mut Vec<u32>,
    ) {
        if !seen.insert(idx) {
            return;
        }
        if let Some(e) = by_idx.get(&idx)
            && let LeafShape::Chain { calls } = &e.shape
        {
            for c in calls {
                dfs(*c, by_idx, seen, order);
            }
        }
        order.push(idx);
    }
    dfs(caller_idx, &by_idx, &mut seen, &mut order);
    order
}

/// Evaluate a closure entry's integer model on a concrete input (for the
/// anti-vacuity `native_decide` guard values). Mirrors the leaf models exactly.
fn compose_eval(idx: u32, x: i64, by_idx: &std::collections::HashMap<u32, &ClosureEntry>) -> i64 {
    match by_idx.get(&idx).map(|e| &e.shape) {
        Some(LeafShape::SelfSum { .. }) => x + x,
        Some(LeafShape::Chain { calls }) => {
            let mut acc = x;
            for c in calls {
                acc = compose_eval(*c, acc, by_idx);
            }
            acc
        }
        None => x,
    }
}

/// Longest chain of code-calls from `idx` down to a leaf (fuel budget for the
/// `native_decide` guards: each level burns one unit in `wFuncN`).
fn compose_depth(idx: u32, by_idx: &std::collections::HashMap<u32, &ClosureEntry>) -> usize {
    match by_idx.get(&idx).map(|e| &e.shape) {
        Some(LeafShape::Chain { calls }) => {
            1 + calls
                .iter()
                .map(|c| compose_depth(*c, by_idx))
                .max()
                .unwrap_or(0)
        }
        _ => 1,
    }
}

/// The cross-function composition certificate: a simulation lemma per closure
/// entry over the caller's SHARED code table (callee lemmas first, the caller's
/// `_wasm_certified` last), the anti-vacuity guards, and the schema obligation.
/// Content-blind: the only per-function inputs are DATA (the closure entries,
/// their call indices and model names), never a hand-tuned proof.
fn render_composition_cert(c: &Cert) -> String {
    let Cert::Composition {
        name,
        self_idx,
        carrier,
        closure,
        ..
    } = c
    else {
        unreachable!()
    };
    let by_idx: std::collections::HashMap<u32, &ClosureEntry> =
        closure.iter().map(|e| (e.self_idx, e)).collect();
    let lemma_name = |idx: u32| -> String {
        if idx == *self_idx {
            format!("{name}_wasm_certified")
        } else {
            format!("{name}__sim_{idx}")
        }
    };
    let sig = |concl_model: &str| -> String {
        format!(
            "    (S : CarrierSpec {carrier}) (add sub : List WVal → Option WVal)\n\
             \x20   (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)\n\
             \x20   (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w) :\n\
             \x20   ∀ (fuel : Nat) (x : Int) (v w : WVal), S.Repr x v →\n\
             \x20     wFuncN {name}Code ({name}Host add sub) fuel {{IDX}} [v] = some w → S.Repr ({concl_model}) w"
        )
    };

    let mut s = format!(
        "/-! ### {name} — cross-function composition certificate (carrier type {carrier}) -/\n\n"
    );

    for idx in compose_topo_order(*self_idx, closure) {
        let e = by_idx[&idx];
        let head = format!(
            "theorem {}\n{}",
            lemma_name(idx),
            sig(&format!("{} x", e.name))
        )
        .replace("{IDX}", &idx.to_string());
        match &e.shape {
            LeafShape::SelfSum { .. } => {
                s.push_str(&format!(
                    "-- callee `{ename}`: self-sum leaf, over the shared closure table.\n{head} := by\n  \
                     intro fuel x v w hv hrun\n  \
                     cases fuel with\n  \
                     | zero => simp only [wFuncN, reduceCtorEq] at hrun\n  \
                     | succ f =>\n      \
                     rcases hc : add [v, v] with _ | r <;>\n        \
                     simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun\n      \
                     subst hrun\n      \
                     exact hadd x x v v r hv hv hc\n\n",
                    ename = e.name,
                ));
            }
            LeafShape::Chain { calls } => {
                let mut body = String::new();
                // one `rcases … <;> simp … at hrun` per call site (threading m1, m2, …).
                for (i, c_idx) in calls.iter().enumerate() {
                    let arg = if i == 0 {
                        "[v]".to_string()
                    } else {
                        format!("[m{i}]")
                    };
                    body.push_str(&format!(
                        "      rcases h{h} : wFuncN {name}Code ({name}Host add sub) f {c_idx} {arg} with _ | m{h} <;>\n        \
                         simp [wFuncN, wRunF, {name}Code, {name}Host, popArgs, initLocals, h{h}] at hrun\n",
                        h = i + 1,
                    ));
                }
                body.push_str("      subst hrun\n");
                // cite the callee simulation lemma at each site, threading the model.
                let mut model_arg = "x".to_string();
                for (i, c_idx) in calls.iter().enumerate() {
                    let (vin, hrepr) = if i == 0 {
                        ("v".to_string(), "hv".to_string())
                    } else {
                        (format!("m{i}"), format!("r{i}"))
                    };
                    body.push_str(&format!(
                        "      have r{h} := {lem} S add sub hadd hsub f ({model_arg}) {vin} m{h} {hrepr} h{h}\n",
                        h = i + 1,
                        lem = lemma_name(*c_idx),
                    ));
                    model_arg = format!("{} ({})", by_idx[c_idx].name, model_arg);
                }
                body.push_str(&format!("      exact r{}\n\n", calls.len()));
                s.push_str(&format!(
                    "-- {label} `{ename}`: unary user-call chain; cites each callee lemma.\n{head} := by\n  \
                     intro fuel x v w hv hrun\n  \
                     cases fuel with\n  \
                     | zero => simp only [wFuncN, reduceCtorEq] at hrun\n  \
                     | succ f =>\n{body}",
                    ename = e.name,
                    label = if idx == *self_idx { "caller" } else { "callee" },
                ));
            }
        }
    }

    s.push_str(&format!("#print axioms {name}_wasm_certified\n\n"));

    // anti-vacuity guards: run the whole closure on concrete inputs.
    let g_fuel = compose_depth(*self_idx, &by_idx) + 2;
    let g3 = compose_eval(*self_idx, 3, &by_idx);
    let gm5 = compose_eval(*self_idx, -5, &by_idx);
    s.push_str(&format!(
        "-- anti-vacuity: the emitted closure actually RUNS on concrete inputs.\n\
         def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})\n\
         example :\n    \
         ((wFuncN {name}Code {name}HostRef {g_fuel} {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt) = some ({g3}) := by\n  \
         native_decide\n\
         example :\n    \
         ((wFuncN {name}Code {name}HostRef {g_fuel} {self_idx} [carrierSmall {carrier} (-5)]).bind carrierToInt) = some ({gm5}) := by\n  \
         native_decide\n\n"
    ));

    // the schema obligation: bridge the caller lemma to `Obligation.holds`.
    s.push_str(&format!(
        "/-- Schema-shaped simulation obligation for `{name}` (composed by the single\n\
        \x20   final theorem): the emitted body simulates `{name}` by citing its callees. -/\n\
         theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by\n  \
         intro S add sub mul stringEq hadd hsub hmul hStringEq fuel ns vs w hrepr hrun\n  \
         simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢\n  \
         obtain ⟨hrepr, harity⟩ := hrepr\n  \
         cases hrepr with\n  \
         | nil => simp at harity\n  \
         | cons hv htail =>\n    \
         rename_i n v ns vs\n    \
         cases htail with\n    \
         | nil =>\n      \
         simpa [AverCert.Schema.intRepr] using\n        \
         {name}_wasm_certified S add sub hadd hsub fuel n v w hv hrun\n    \
         | cons _ _ => simp at harity\n"
    ));

    s
}

fn render_adt_constructor_cert(c: &Cert, model_info: &ModelInfo) -> String {
    let c = c.inner();
    let Cert::AdtConstructor {
        name,
        self_idx,
        carrier,
        struct_idx,
        field_count,
        arity,
        fields,
        ..
    } = c
    else {
        unreachable!()
    };
    if !adt_constructor_uses_model(c, model_info) {
        let _ = field_count;
        return render_struct_verbatim_cert(
            name,
            *self_idx,
            *carrier,
            *struct_idx,
            StructVerbatimShape::Pack {
                arity: *arity,
                fields,
            },
        );
    }
    let sig = model_info.fns.get(name);
    let _ = sig
        .and_then(|s| model_info.inductives.get(&s.ret))
        .and_then(|i| i.ctors.first());
    format!(
        r#"/-! ### {name} — ADT constructor certificate (carrier type {carrier}) -/

theorem {name}_wasm_certified (host : HostTbl) :
    ∀ (v : WVal), wFuncN {name}Code host 1 {self_idx} [v] = some (.structv {struct_idx} [v]) := by
  intro v
  simp [wFuncN, {name}Code, wRunF, popArgs, initLocals]

#print axioms {name}_wasm_certified

-- Executable tripwire: run the constructor on an Int 7 and decode field 0 of the
-- built struct back to `some 7`. Unlike a bare `= none` (which a TRAP also
-- satisfies), this forces the emitted body to genuinely pack its argument.
example :
    ((wFuncN {name}Code {name}Host 1 {self_idx} [carrierSmall {carrier} 7]).bind
      (fun r => match r with
        | .structv _ (f :: _) => carrierToInt f
        | _ => none))
      = some 7 := by native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel n vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨v, rfl, hv⟩ := hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      simp [wFuncN, wRunF, {name}Code, popArgs, initLocals] at hrun
      subst hrun
      exact ⟨v, rfl, by simpa [AverCert.Schema.intRepr] using hv⟩
"#
    )
}

/// Which end of the `struct.new`/verbatim spine a certificate proves: packing
/// arguments INTO a variant struct (the dual of) reading a field OUT of one.
/// Both are one-step `cases fuel` proofs over `WVal`/`verbatimRepr` with no host,
/// so [`render_struct_verbatim_cert`] emits them through one skeleton.
enum StructVerbatimShape<'a> {
    /// Verbatim constructor: wrap `arity` arguments into variant `struct_idx`.
    Pack {
        arity: usize,
        fields: &'a [ConstructorField],
    },
    /// Field projection: read field `field_idx` (0 or 1) out of a two-field struct.
    Project { field_idx: u32 },
}

/// The `struct.new`/verbatim certificate arm: a single one-step `cases fuel`
/// proof over `WVal`/`verbatimRepr` (no host), shared by the verbatim
/// constructor (packs its arguments into variant `struct_idx`) and the field
/// projection (reads a field back out) — structural duals that pop the raw
/// arguments, reduce the interpreter and close by `rfl`. The two ends differ
/// only in the statement's argument shape, the executable tripwire and the
/// obligation's argument destructuring.
fn render_struct_verbatim_cert(
    name: &str,
    self_idx: u32,
    carrier: u32,
    struct_idx: u32,
    shape: StructVerbatimShape,
) -> String {
    let (title, binders, input, rhs, intro, destructure, tripwire) = match shape {
        StructVerbatimShape::Pack { arity, fields } => {
            let built = render_constructor_fields(fields);
            let (binders, input, intro, destructure) = if arity == 1 {
                (
                    "(a : WVal)".to_string(),
                    "[a]".to_string(),
                    "intro a".to_string(),
                    String::new(),
                )
            } else {
                (
                    "(a b : WVal)".to_string(),
                    "[a, b]".to_string(),
                    "intro a b".to_string(),
                    "  rcases p with ⟨a, b⟩\n".to_string(),
                )
            };
            let concrete = if arity == 1 {
                format!("[carrierSmall {carrier} 7]")
            } else {
                format!("[carrierSmall {carrier} 7, carrierSmall {carrier} 9]")
            };
            let tripwire = format!(
                r#"-- Executable tripwire: pack concrete carriers and decode field 0 back to
-- `some 7`. A trapping body yields `none`, so this forces a real struct build.
example :
    ((wFuncN {name}Code {name}Host 1 {self_idx} {concrete}).bind
      (fun r => match r with
        | .structv _ (f :: _) => carrierToInt f
        | _ => none))
      = some 7 := by native_decide"#
            );
            (
                "verbatim constructor certificate".to_string(),
                binders,
                input,
                format!("(.structv {struct_idx} {built})"),
                intro,
                destructure,
                tripwire,
            )
        }
        StructVerbatimShape::Project { field_idx } => {
            let expected = if field_idx == 0 { "a" } else { "b" };
            let forced = if field_idx == 0 { 7 } else { 9 };
            let tripwire = format!(
                r#"-- Executable tripwire: project the field from a concrete two-carrier struct and
-- decode it back. A trapping body yields `none`, so this forces a real read.
example :
    ((wFuncN {name}Code {name}Host 1 {self_idx}
        [.structv {struct_idx} [carrierSmall {carrier} 7, carrierSmall {carrier} 9]]).bind carrierToInt)
      = some {forced} := by native_decide"#
            );
            (
                "field projection certificate".to_string(),
                "(a b : WVal)".to_string(),
                format!("[.structv {struct_idx} [a, b]]"),
                expected.to_string(),
                "intro a b".to_string(),
                "  rcases p with ⟨a, b⟩\n".to_string(),
                tripwire,
            )
        }
    };
    format!(
        r#"/-! ### {name} — {title} -/

theorem {name}_wasm_certified (host : HostTbl) :
    ∀ {binders}, wFuncN {name}Code host 1 {self_idx} {input} = some {rhs} := by
  {intro}
  simp [wFuncN, {name}Code, wRunF, popArgs, initLocals]

#print axioms {name}_wasm_certified

{tripwire}

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel p vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
{destructure}  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      simp [wFuncN, wRunF, {name}Code, popArgs, initLocals] at hrun
      subst hrun
      rfl
"#
    )
}

fn render_constructor_fields(fields: &[ConstructorField]) -> String {
    let parts = fields
        .iter()
        .map(|field| match field {
            ConstructorField::Local(0) => "a".to_string(),
            ConstructorField::Local(1) => "b".to_string(),
            ConstructorField::Local(i) => format!("x{i}"),
            ConstructorField::Null => ".null".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{parts}]")
}

/// The per-class ingredients of one ADT-match certificate: the pieces that
/// differ between the fully-modelled variant dispatch and the widened Int match,
/// spliced into ONE shared `cases o` skeleton by [`render_adt_match_cert`].
struct AdtMatchPlan {
    doc_title: String,
    /// The theorem header up to (not including) the trailing ` :` — the binder
    /// block differs (variant carries the add/sub host contracts, widened none).
    header: String,
    ty: String,
    /// The domain-representation relation: the shared, faithful `{ty}Repr` when
    /// every constructor is modelled, the per-function weak `{name}DomRepr` when
    /// only one integer-payload variant is projected.
    repr: String,
    host_term: String,
    host_ref_def: String,
    /// The explicit per-constructor `cases o` arms, already joined.
    cases: String,
    guards: String,
    /// The `_simulates` citation of `{name}_wasm_certified` (its argument list
    /// differs by the host contracts).
    sim_cite: String,
}

/// Leaf-polymorphic ADT variant-match certificate: ONE explicit `cases o` proof
/// spine shared by the fully-modelled variant dispatch (every constructor
/// faithfully represented, closing through the runtime add/sub contracts) and
/// the widened Int match (one integer-payload constructor projected faithfully,
/// every other constructor opaque and boxing the default). The walker emits the
/// spine; each constructor closes with its OWN honest closer against its OWN
/// honest domain representation, so neither faithfulness level is weakened to fit
/// the other (the exact fold that a single shared `Repr` would have banned).
fn render_adt_match_cert(c: &Cert, model_info: &ModelInfo) -> String {
    let inner = c.inner();
    let name = inner.name();
    let self_idx = inner.self_idx();
    let plan = match inner {
        Cert::VariantDispatch { .. } => adt_match_variant_plan(inner, model_info),
        Cert::WidenedIntMatch { .. } => adt_match_widened_plan(inner, model_info),
        _ => unreachable!(),
    };
    let AdtMatchPlan {
        doc_title,
        header,
        ty,
        repr,
        host_term,
        host_ref_def,
        cases,
        guards,
        sim_cite,
    } = match plan {
        Ok(p) => p,
        Err(early) => return early,
    };
    format!(
        r#"{doc_title}

{header} :
    ∀ (fuel : Nat) (o : {ty}) (v w : WVal), {repr} S o v →
      wFuncN {name}Code {host_term} fuel {self_idx} [v] = some w →
      S.Repr ({name} o) w := by
  intro fuel
  cases fuel with
  | zero => intro o v w hv hrun; simp [wFuncN] at hrun
  | succ f =>
      intro o v w hv hrun
      cases o with
{cases}

#print axioms {name}_wasm_certified

{host_ref_def}
{guards}

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel o vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨v, rfl, hv⟩ := hrepr
  simpa [AverCert.Schema.intRepr] using {sim_cite}
"#
    )
}

/// The variant-dispatch plan: every constructor of a fully-modelled inductive is
/// faithful (`{ty}Repr`); its arm is a payload projection, a payload/const host
/// combination, or the terminal default. Closes through the add/sub contracts.
fn adt_match_variant_plan(c: &Cert, model_info: &ModelInfo) -> Result<AdtMatchPlan, String> {
    let Cert::VariantDispatch {
        name,
        self_idx,
        carrier,
        add_idx,
        sub_idx,
        arms,
        default_k,
        ..
    } = c
    else {
        unreachable!()
    };
    let ty = model_info
        .fns
        .get(name)
        .and_then(|s| s.params.first())
        .map(|s| s.as_str())
        .unwrap_or("Op");
    let repr = format!("{ty}Repr");
    let host_ref = format!("{name}HostRef");
    let Some(ind) = model_info.inductives.get(ty) else {
        return Err(format!(
            "-- {name}: no source inductive for {ty}\nexample : False := by decide\n"
        ));
    };
    let base = arms.iter().map(|(t, _)| *t).min().unwrap_or(0);
    let arm_of_tag =
        |tag: u32| -> Option<&ArmLeaf> { arms.iter().find(|(t, _)| *t == tag).map(|(_, l)| l) };
    let simp_run = format!(
        "simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32, popArgs, initLocals] at hrun"
    );

    let mut cases = Vec::new();
    let mut guards = Vec::new();
    for (i, ctor) in ind.ctors.iter().enumerate() {
        let tag = base + i as u32;
        let cn = &ctor.name;
        let payload = !ctor.fields.is_empty();
        if arm_of_tag(tag).is_some() && !payload {
            return Err(format!(
                "-- {name}: variant tag mapping mismatch ({cn} is nullary but tag {tag} is dispatched)\nexample : False := by decide\n"
            ));
        }
        match arm_of_tag(tag) {
            Some(ArmLeaf::Proj) => {
                cases.push(format!(
                    "      | {cn} x =>\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          {simp_run}\n          subst hrun\n          simpa [{name}] using hcx"
                ));
                guards.push(format!(
                    "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} [carrierSmall {carrier} 5]]).bind carrierToInt)\n    = some 5 := by native_decide"
                ));
            }
            Some(ArmLeaf::HostOp {
                role,
                k,
                const_first,
            }) => {
                let (hostfn, hyp) = match role {
                    HostRole::Add => ("add", "hadd"),
                    HostRole::Sub => ("sub", "hsub"),
                    HostRole::StringEq => unreachable!(),
                };
                let (operands, cite) = if *const_first {
                    (
                        format!("[carrierSmall {carrier} ({k}), cx]"),
                        format!(
                            "{hyp} ({k}) x (carrierSmall {carrier} ({k})) cx w' (S.smallIntro ({k})) hcx hs"
                        ),
                    )
                } else {
                    (
                        format!("[cx, carrierSmall {carrier} ({k})]"),
                        format!(
                            "{hyp} x ({k}) cx (carrierSmall {carrier} ({k})) w' hcx (S.smallIntro ({k})) hs"
                        ),
                    )
                };
                cases.push(format!(
                    "      | {cn} x =>\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          {simp_run}\n          rcases hs : {hostfn} {operands} with _ | w' <;> simp [hs] at hrun\n          subst hrun\n          have := {cite}\n          simpa [{name}] using this"
                ));
                let sample = 5i64;
                let expected = if *const_first {
                    match role {
                        HostRole::Add => k + sample,
                        HostRole::Sub => k - sample,
                        HostRole::StringEq => unreachable!(),
                    }
                } else {
                    match role {
                        HostRole::Add => sample + k,
                        HostRole::Sub => sample - k,
                        HostRole::StringEq => unreachable!(),
                    }
                };
                guards.push(format!(
                    "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} [carrierSmall {carrier} {sample}]]).bind carrierToInt)\n    = some ({expected}) := by native_decide"
                ));
            }
            None => {
                if payload {
                    cases.push(format!(
                        "      | {cn} x =>\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          {simp_run}\n          subst hrun\n          simpa [{name}] using S.smallIntro ({default_k})"
                    ));
                    guards.push(format!(
                        "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} [carrierSmall {carrier} 5]]).bind carrierToInt)\n    = some ({default_k}) := by native_decide"
                    ));
                } else {
                    cases.push(format!(
                        "      | {cn} =>\n          subst hv\n          {simp_run}\n          subst hrun\n          simpa [{name}] using S.smallIntro ({default_k})"
                    ));
                    guards.push(format!(
                        "example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv {tag} []]).bind carrierToInt)\n    = some ({default_k}) := by native_decide"
                    ));
                }
            }
        }
    }
    let add_ref = if add_idx.is_some() {
        format!("(addRef {carrier})")
    } else {
        "(fun _ => none)".to_string()
    };
    let sub_ref = if sub_idx.is_some() {
        format!("(subRef {carrier})")
    } else {
        "(fun _ => none)".to_string()
    };
    Ok(AdtMatchPlan {
        doc_title: format!(
            "/-! ### {name} — general variant dispatch certificate (carrier type {carrier}) -/"
        ),
        header: format!(
            "theorem {name}_wasm_certified\n    (S : CarrierSpec {carrier})\n    (add sub : List WVal → Option WVal)\n    (hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w → S.Repr (a + b) w)\n    (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w)"
        ),
        ty: ty.to_string(),
        repr,
        host_term: format!("({name}Host add sub)"),
        host_ref_def: format!("def {host_ref} : HostTbl := {name}Host {add_ref} {sub_ref}"),
        cases: cases.join("\n"),
        guards: guards.join("\n"),
        sim_cite: format!("{name}_wasm_certified S add sub hadd hsub fuel o v w hv hrun"),
    })
}

/// The widened-match plan: the single integer-payload constructor is faithful
/// (projects its carrier), every other constructor is opaque — represented only
/// as "some struct of a different type" and boxing the default `0`. The honest
/// per-function `{name}DomRepr` keeps the heterogeneous variants opaque instead
/// of forcing them into an integer carrier they cannot inhabit.
fn adt_match_widened_plan(c: &Cert, model_info: &ModelInfo) -> Result<AdtMatchPlan, String> {
    let Cert::WidenedIntMatch {
        name,
        self_idx,
        carrier,
        hit_variant_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let Some((_ty, ind, hit_ctor)) = widened_match_info(c, model_info) else {
        return Err(format!(
            "-- {name}: widened match info unavailable\nexample : False := by decide\n"
        ));
    };
    let ty = model_info
        .fns
        .get(name)
        .and_then(|s| s.params.first())
        .cloned()
        .unwrap_or_else(|| "Op".to_string());
    let other_idx = if *hit_variant_idx == 0 { 1 } else { 0 };

    let mut cases = Vec::new();
    for ctor in &ind.ctors {
        if ctor.name == hit_ctor {
            cases.push(format!(
                "      | {cn} x =>\n          simp only [{name}DomRepr] at hv\n          obtain ⟨cx, rfl, hcx⟩ := hv\n          simp [wFuncN, wRunF, {name}Code, {name}Host, b32, popArgs, initLocals] at hrun\n          subst hrun\n          simpa [{name}] using hcx",
                cn = ctor.name,
            ));
        } else {
            let binders = match ctor.fields.len() {
                0 => String::new(),
                1 => " x".to_string(),
                n => " _".repeat(n),
            };
            cases.push(format!(
                "      | {cn}{binders} =>\n          simp only [{name}DomRepr] at hv\n          obtain ⟨t, fs, rfl, hne⟩ := hv\n          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32, popArgs, initLocals, hne] at hrun\n          subst hrun\n          simpa [{name}] using S.smallIntro 0",
                cn = ctor.name,
            ));
        }
    }
    let guards = format!(
        "example :\n    ((wFuncN {name}Code {name}HostRef 4 {self_idx}\n        [.structv {hit_variant_idx} [carrierSmall {carrier} 42]]).bind carrierToInt)\n      = some 42 := by native_decide\nexample :\n    ((wFuncN {name}Code {name}HostRef 4 {self_idx}\n        [.structv {other_idx} []]).bind carrierToInt)\n      = some 0 := by native_decide"
    );
    Ok(AdtMatchPlan {
        doc_title: format!(
            "/-! ### {name} — widened Int match certificate (carrier type {carrier}) -/"
        ),
        header: format!("theorem {name}_wasm_certified (S : CarrierSpec {carrier})"),
        ty,
        repr: format!("{name}DomRepr"),
        host_term: format!("{name}Host"),
        host_ref_def: format!(
            "-- Executable tripwires: the projected variant reads its carrier, every other\n-- variant boxes the default `0`. A trapping body yields `none`, forcing a run.\ndef {name}HostRef : HostTbl := {name}Host"
        ),
        cases: cases.join("\n"),
        guards,
        sim_cite: format!("{name}_wasm_certified S fuel o v w hv hrun"),
    })
}

/// A `Cod := WVal` / `verbatimRepr` match certificate: the shared theorem
/// wrapper (statement, `cases v`, obligation) with the per-shape holes filled by
/// a plan. The widened match (one projected hit + a default) and the variant
/// dispatch (a constant per tag) are two ends of ONE arm — same face, same
/// `cases v` spine — differing only in the per-arm leaf.
struct VerbatimWvalPlan {
    doc_title: String,
    doc_comment: String,
    cases_body: String,
    guards_comment: String,
    guards: String,
}

fn render_verbatim_wval_match_cert(c: &Cert) -> String {
    let name = c.name();
    let self_idx = c.self_idx();
    let plan = match c.inner() {
        Cert::VerbatimWidenedMatch { .. } => verbatim_widened_plan(c),
        Cert::VerbatimVariantDispatch { .. } => verbatim_variant_dispatch_plan(c),
        _ => unreachable!(),
    };
    let VerbatimWvalPlan {
        doc_title,
        doc_comment,
        cases_body,
        guards_comment,
        guards,
    } = plan;
    format!(
        r#"{doc_title}

{doc_comment}
theorem {name}_wasm_certified :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN {name}Code {name}Host (fuel + 1) {self_idx} [v] = some w →
      w = {name}Model v := by
  intro fuel v w hrun
  cases v with
{cases_body}

#print axioms {name}_wasm_certified

{guards_comment}
def {name}HostRef : HostTbl := {name}Host
{guards}
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel v vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      have hc := {name}_wasm_certified f v w hrun
      simpa [AverCert.Schema.verbatimRepr] using hc
"#
    )
}

fn verbatim_widened_plan(c: &Cert) -> VerbatimWvalPlan {
    let Cert::VerbatimWidenedMatch {
        name,
        self_idx,
        carrier,
        hit_variant_idx,
        default,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let hit = hit_variant_idx;
    let other = if *hit_variant_idx == 0 { 1 } else { 0 };
    let default_guard = render_default_guard(default);
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, b32, popArgs, initLocals, List.set"
    );
    VerbatimWvalPlan {
        doc_title: format!(
            "/-! ### {name} — verbatim widened match certificate (carrier type {carrier}) -/"
        ),
        doc_comment: format!(
            "/-- The VERBATIM emitted body reads the first field of variant `{hit}` and\n    \
             returns it as-is, or the null reference for any other value, for ALL inputs\n    \
             `v : WVal` (partial correctness — a trap makes no claim). -/"
        ),
        cases_body: format!(
            "  | i32v n => simp [{evalset}] at hrun\n  \
             | i64v n => simp [{evalset}] at hrun\n  \
             | f64v b => simp [{evalset}] at hrun\n  \
             | null => simp_all [{evalset}]\n  \
             | arr t es =>\n      \
             by_cases ht : t = {hit}\n      \
             · subst ht; simp [{evalset}] at hrun\n      \
             · simp_all [{evalset}]\n  \
             | structv t fs =>\n      \
             by_cases ht : t = {hit}\n      \
             · subst ht\n        \
             cases fs with\n        \
             | nil => simp [{evalset}] at hrun\n        \
             | cons x rest => simp_all [{evalset}]\n      \
             · simp_all [{evalset}]"
        ),
        guards_comment:
            "-- Executable tripwires: the projected variant returns its first field, every\n\
             -- other value returns the byte-derived default literal."
                .to_string(),
        guards: format!(
            "example :\n    \
             (wFuncN {name}Code {name}HostRef 4 {self_idx} [.structv {hit} [.i64v 42]]).bind\n      \
             (fun w => match w with | .i64v n => some n | _ => none) = some 42 := by native_decide\n\
             example :\n    \
             (wFuncN {name}Code {name}HostRef 4 {self_idx} [.structv {other} []]).bind {default_guard} := by\n  \
             native_decide\n"
        ),
    }
}

fn verbatim_variant_dispatch_plan(c: &Cert) -> VerbatimWvalPlan {
    let Cert::VerbatimVariantDispatch {
        name,
        self_idx,
        carrier,
        arms,
        default,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, b32, popArgs, initLocals, List.set"
    );
    let dispatch = verbatim_tag_dispatch(arms, 6, &evalset);
    let default_tag = arms.iter().map(|(t, _)| *t).max().unwrap_or(0) + 1;
    let mut guards = String::new();
    for (tag, konst) in arms {
        guards.push_str(&format!(
            "example :\n    (wFuncN {name}Code {name}HostRef 8 {self_idx} [.structv {tag} []]).bind {} := by\n  native_decide\n",
            render_default_guard(konst),
        ));
    }
    guards.push_str(&format!(
        "example :\n    (wFuncN {name}Code {name}HostRef 8 {self_idx} [.structv {default_tag} []]).bind {} := by\n  native_decide\n",
        render_default_guard(default),
    ));
    VerbatimWvalPlan {
        doc_title: format!(
            "/-! ### {name} — verbatim variant dispatch certificate (carrier type {carrier}) -/"
        ),
        doc_comment:
            "/-- The VERBATIM emitted body dispatches on the variant and returns a byte-derived\n    \
             constant for each tag, for ALL inputs `v : WVal` (partial correctness — a trap\n    \
             makes no claim). -/"
                .to_string(),
        cases_body: format!(
            "  | i32v n => simp_all [{evalset}]\n  \
             | i64v n => simp_all [{evalset}]\n  \
             | f64v b => simp_all [{evalset}]\n  \
             | null => simp_all [{evalset}]\n  \
             | arr t es =>\n{dispatch}\n  \
             | structv t fs =>\n{dispatch}"
        ),
        guards_comment: "-- Executable tripwires: each variant returns its byte-derived constant."
            .to_string(),
        guards,
    }
}

/// The nested `by_cases` block dispatching on a struct/array type index `t`: one
/// `by_cases hI : t = tagI` per arm (each closing `subst; simp_all` to its
/// constant), terminating in `simp_all` for the default. `base` is the leading
/// indent of the first `by_cases`. Mirrors HueSpike.lean's proven shape.
fn verbatim_tag_dispatch(arms: &[(u32, VerbatimDefault)], base: usize, evalset: &str) -> String {
    let mut out = String::new();
    for (i, (tag, _)) in arms.iter().enumerate() {
        let ind = base + i * 2;
        let sp = " ".repeat(ind);
        if i == 0 {
            out.push_str(&sp);
        }
        out.push_str(&format!("by_cases h{i} : t = {tag}\n"));
        out.push_str(&format!("{sp}· subst h{i}\n"));
        out.push_str(&format!("{sp}  simp_all [{evalset}]\n"));
        out.push_str(&format!("{sp}· "));
    }
    out.push_str(&format!("simp_all [{evalset}]"));
    out
}

fn render_string_eq_verbatim_match_cert(c: &Cert) -> String {
    let Cert::StringEqVerbatimMatch {
        name,
        self_idx,
        arms,
        default,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    let [(needle, hit)] = arms.as_slice() else {
        unreachable!()
    };
    let VerbatimDefault::Array {
        type_idx: string_ty,
        bytes: _,
    } = needle
    else {
        unreachable!()
    };
    let needle_w = render_wval(needle);
    let needle_arg = render_wval_arg(needle);
    let hit_guard = render_default_guard(hit);
    let miss_input = VerbatimDefault::Array {
        type_idx: *string_ty,
        bytes: vec![120],
    };
    let miss_guard = match default {
        StringEqDefault::Input => render_default_guard(&miss_input),
        StringEqDefault::Verbatim(k) => render_default_guard(k),
    };
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, {name}Model, b32, popArgs, initLocals, List.set"
    );
    format!(
        r#"/-! ### {name} — String.eq host-contract verbatim match certificate -/

/-- The VERBATIM emitted body compares the input with one byte-derived String
    literal via the contracted `String.eq` host slot, then returns either the
    byte-derived hit literal or the byte-exact default. The loop inside the
    helper is outside this user-code proof and enters only through `hStringEq`. -/
theorem {name}_wasm_certified
    (stringEq : List WVal → Option WVal)
    (hStringEq : ∀ a b w, stringEq [a, b] = some w → w = b32 (stringEqW a b)) :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN {name}Code ({name}Host stringEq) (fuel + 1) {self_idx} [v] = some w →
      w = {name}Model v := by
  intro fuel v w hrun
  cases v with
  | i32v n => simp [{evalset}] at hrun
  | i64v n => simp [{evalset}] at hrun
  | f64v bits => simp [{evalset}] at hrun
  | null => simp [{evalset}] at hrun
  | structv t fs =>
      by_cases ht : t = {string_ty}
      · subst ht
        cases hcall : stringEq [.structv {string_ty} fs, {needle_w}] with
        | none =>
            simp [{evalset}, hcall] at hrun
        | some got =>
            have hgot : got = b32 (stringEqW (.structv {string_ty} fs) {needle_arg}) :=
              hStringEq (.structv {string_ty} fs) {needle_arg} got hcall
            have hmatch : stringEqW (.structv {string_ty} fs) {needle_arg} = false := by rfl
            simp [{evalset}, hcall, hgot, hmatch] at hrun
            simpa [{name}Model, hmatch] using hrun.symm
      · simp [{evalset}, ht] at hrun
  | arr t es =>
      by_cases ht : t = {string_ty}
      · subst ht
        cases hcall : stringEq [.arr {string_ty} es, {needle_w}] with
        | none =>
            simp [{evalset}, hcall] at hrun
        | some got =>
            have hgot : got = b32 (stringEqW (.arr {string_ty} es) {needle_arg}) :=
              hStringEq (.arr {string_ty} es) {needle_arg} got hcall
            cases hmatch : stringEqW (.arr {string_ty} es) {needle_arg}
            · simp [{evalset}, hcall, hgot, hmatch] at hrun
              simpa [{name}Model, hmatch] using hrun.symm
            · simp [{evalset}, hcall, hgot, hmatch] at hrun
              simpa [{name}Model, hmatch] using hrun.symm
      · simp [{evalset}, ht] at hrun

#print axioms {name}_wasm_certified

-- Executable tripwires: one equal input takes the hit arm; a distinct byte
-- string takes the default arm. The host reference is the executable face of
-- the same String.eq contract used abstractly above.
def {name}HostRef : HostTbl := {name}Host stringEqRef
example :
    (wFuncN {name}Code {name}HostRef 8 {self_idx} [{needle_w}]).bind {hit_guard} := by
  native_decide
example :
    (wFuncN {name}Code {name}HostRef 8 {self_idx} [.arr {string_ty} [.i32v 120]]).bind {miss_guard} := by
  native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel v vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      have hc := {name}_wasm_certified stringEq hStringEq f v w hrun
      simpa [AverCert.Schema.verbatimRepr] using hc
"#,
    )
}

fn render_int_range_predicate_cert(c: &Cert) -> String {
    let c = c.inner();
    let Cert::IntRangePredicate {
        name,
        self_idx,
        carrier,
        k_lo,
        k_hi,
        ..
    } = c
    else {
        unreachable!()
    };
    let inside = k_lo; // in [k_lo, k_hi]
    let outside = k_lo - 1; // below k_lo
    let evalset = format!(
        "wFuncN, wRunF, {name}Code, {name}Host, carrierSmall, b32, popArgs, initLocals, {name}"
    );
    format!(
        r#"/-! ### {name} — Int range predicate certificate (carrier type {carrier}) -/

/-- The VERBATIM emitted body maps the canonical small carrier of `cp` to the
    boolean `{name} cp = (cp >= {k_lo} && cp <= {k_hi})`, for ALL `cp : ℤ`. The
    bignum comparison arms are dead over this domain (small carrier ⇒ null limbs). -/
theorem {name}_wasm_certified (S : CarrierSpec {carrier}) :
    ∀ (fuel : Nat) (cp : Int),
      wFuncN {name}Code {name}Host (fuel + 1) {self_idx} [carrierSmall {carrier} cp]
        = some (b32 ({name} cp)) := by
  intro fuel cp
  by_cases h1 : ({k_lo} : Int) ≤ cp
  · by_cases h2 : cp ≤ ({k_hi} : Int)
    · simp [{evalset}, ge_iff_le, h1, h2]
    · simp [{evalset}, ge_iff_le, h1, h2]
  · simp [{evalset}, ge_iff_le, h1]

#print axioms {name}_wasm_certified

-- Executable tripwires: a value inside the range yields `true` (1), a value
-- below the low bound yields `false` (0). Decodes the i32 boolean to an Int.
def {name}HostRef : HostTbl := {name}Host
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} ({inside})]).bind
        (fun w => match w with | .i32v n => some n | _ => none)) = some 1 := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} ({outside})]).bind
        (fun w => match w with | .i32v n => some n | _ => none)) = some 0 := by native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel cp vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      rw [{name}_wasm_certified S f cp] at hrun
      simp only [Option.some.injEq] at hrun
      subst hrun
      simp [AverCert.Schema.boolRepr]
"#
    )
}

fn render_user_repr_defs(analysis: &Analysis, model_info: &ModelInfo) -> String {
    let mut out = String::new();
    let mut emitted = std::collections::HashSet::new();
    for c in &analysis.certs {
        let Some((ty, indices)) = adt_repr_indices(c, model_info) else {
            continue;
        };
        if !emitted.insert(ty.clone()) {
            continue;
        }
        let Some(ind) = model_info.inductives.get(&ty) else {
            continue;
        };
        out.push_str(&format!(
            "def {ty}Repr (S : CarrierSpec {}) : {ty} → WVal → Prop\n",
            c.carrier()
        ));
        for (i, ctor) in ind.ctors.iter().enumerate() {
            let idx = indices.get(i).copied().unwrap_or(i as u32);
            if ctor.fields.is_empty() {
                out.push_str(&format!("  | .{}, v => v = .structv {idx} []\n", ctor.name));
            } else if ctor.fields.len() == 1 && ctor.fields[0] == "Int" {
                out.push_str(&format!(
                    "  | .{} x, v => ∃ cx, v = .structv {idx} [cx] ∧ S.Repr x cx\n",
                    ctor.name
                ));
            } else {
                out.push_str(&format!("  | .{}, _ => False\n", ctor.name));
            }
        }
        out.push('\n');
    }
    // Per-function domain-representation relations for widened Int matches. Each
    // is keyed on the projected variant's byte-derived struct index: the hit
    // constructor is represented as that struct carrying a single Int carrier,
    // every other constructor as any struct of a DIFFERENT type — enough to make
    // the projection theorem provable and non-vacuous. This is a read
    // declaration (the ADT face is not kernel-re-derived), so its exact shape is
    // untrusted; the checker pins only `Cod = Int`, `codRepr = intRepr` and
    // `Nonempty Dom`.
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::WidenedIntMatch {
            hit_variant_idx, ..
        } = c
        else {
            continue;
        };
        let Some((ty, ind, hit_ctor)) = widened_match_info(c, model_info) else {
            continue;
        };
        out.push_str(&format!(
            "def {name}DomRepr (S : CarrierSpec {carrier}) : {ty} → WVal → Prop\n",
            name = c.name(),
            carrier = c.carrier(),
        ));
        for ctor in &ind.ctors {
            let binders = " _".repeat(ctor.fields.len());
            if ctor.name == hit_ctor {
                out.push_str(&format!(
                    "  | .{ctor} x, v => ∃ cx, v = .structv {hit_variant_idx} [cx] ∧ S.Repr x cx\n",
                    ctor = ctor.name,
                ));
            } else {
                out.push_str(&format!(
                    "  | .{ctor}{binders}, v => ∃ t fs, v = .structv t fs ∧ t ≠ {hit_variant_idx}\n",
                    ctor = ctor.name,
                ));
            }
        }
        out.push('\n');
    }
    // Shared model definitions for verbatim widened matches: a single named
    // function referenced by both the obligation `model` and the certificate
    // proof, so the two match on the SAME compiled term (an inline match would
    // elaborate to two distinct, non-defeq auxiliaries).
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::VerbatimWidenedMatch {
            hit_variant_idx, ..
        } = c
        else {
            continue;
        };
        out.push_str(&format!(
            "def {name}Model : CertPrelude.WVal → CertPrelude.WVal\n  \
             | .structv {hit_variant_idx} (x :: _) => x\n  \
             | _ => {default}\n\n",
            name = c.name(),
            default = match c {
                Cert::VerbatimWidenedMatch { default, .. } => render_wval(default),
                _ => unreachable!(),
            },
        ));
    }
    // Model for a verbatim variant dispatch: each tag maps to its byte-derived
    // constant. The body's `ref.test i` matches the WVal type index for BOTH
    // `.structv i` and `.arr i`, and constant arms never fail on `.arr` (unlike a
    // field projection), so the model must dispatch on the type index for both
    // carriers; anything else falls to the terminal default.
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::VerbatimVariantDispatch { arms, default, .. } = c else {
            continue;
        };
        let mut body = String::new();
        for (tag, konst) in arms {
            body.push_str(&format!("  | .structv {tag} _ => {}\n", render_wval(konst)));
        }
        for (tag, konst) in arms {
            body.push_str(&format!("  | .arr {tag} _ => {}\n", render_wval(konst)));
        }
        out.push_str(&format!(
            "def {name}Model : CertPrelude.WVal → CertPrelude.WVal\n{body}  | _ => {default}\n\n",
            name = c.name(),
            default = render_wval(default),
        ));
    }
    // Model for String-literal dispatch: the String.eq helper is a host
    // contract, so the model uses the audited prelude's byte-array equality
    // predicate over raw `WVal` arrays and returns byte-derived constants.
    for c in &analysis.certs {
        let c = c.inner();
        let Cert::StringEqVerbatimMatch { arms, default, .. } = c else {
            continue;
        };
        let mut body = String::new();
        for (needle, hit) in arms {
            body.push_str(&format!(
                "  if stringEqW v {} then {}\n  else",
                render_wval_arg(needle),
                render_wval(hit)
            ));
        }
        body.push_str(&format!(" {}\n", render_string_eq_default(default, "v")));
        out.push_str(&format!(
            "def {name}Model (v : CertPrelude.WVal) : CertPrelude.WVal :=\n{body}\n",
            name = c.name(),
        ));
    }
    out
}

/// For a widened Int match: the model inductive name, its constructor list, and
/// the name of the single integer-payload constructor the body projects (the
/// unique `fields == ["Int"]` constructor). `None` — so the class declines by a
/// failed render — if the model type is unknown or the projected constructor is
/// not unique.
fn widened_match_info<'a>(
    c: &Cert,
    model_info: &'a ModelInfo,
) -> Option<(String, &'a InductiveInfo, String)> {
    let c = c.inner();
    let Cert::WidenedIntMatch { name, .. } = c else {
        return None;
    };
    let ty = model_info.fns.get(name)?.params.first()?.clone();
    let ind = model_info.inductives.get(&ty)?;
    let mut int_ctors = ind.ctors.iter().filter(|ct| ct.fields == ["Int"]);
    let hit = int_ctors.next()?.name.clone();
    if int_ctors.next().is_some() {
        return None;
    }
    Some((ty, ind, hit))
}

/// Whether an ADT constructor certificate can name its real model type: a
/// single-field constructor whose codomain is a user inductive (so
/// `render_user_repr_defs` emits a `<Ty>Repr` and the model is `<Ty>.<ctor>`).
/// Anything else — a multi-field constructor, or a constructor over a builtin
/// compound codomain like `List (String × Json)` that has no user Repr — is
/// certified as a verbatim pack instead (the dual of a field projection), which
/// makes no claim about a recursive representation (deferred, see the model
/// stop-loss on recursive-type Repr).
fn adt_constructor_uses_model(c: &Cert, model_info: &ModelInfo) -> bool {
    let c = c.inner();
    let Cert::AdtConstructor {
        name,
        field_count,
        arity,
        fields,
        ..
    } = c
    else {
        return false;
    };
    *field_count == 1
        && *arity == 1
        && fields.as_slice() == [ConstructorField::Local(0)]
        && model_info
            .fns
            .get(name)
            .map(|s| model_info.inductives.contains_key(&s.ret))
            .unwrap_or(false)
}

/// `(Dom type, `vs`-shape, struct-field list)` for a verbatim pack constructor
/// of the given field count. The domain is the raw argument `WVal`s (a single
/// value or a pair), and the model packs them into the variant struct verbatim.
fn verbatim_ctor_shape(
    arity: usize,
    fields: &[ConstructorField],
) -> (&'static str, String, String) {
    let args = fields
        .iter()
        .map(|field| match field {
            ConstructorField::Local(0) if arity == 1 => "p".to_string(),
            ConstructorField::Local(0) => "p.1".to_string(),
            ConstructorField::Local(1) => "p.2".to_string(),
            ConstructorField::Local(i) => format!("p.{i}"),
            ConstructorField::Null => ".null".to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    if arity == 1 {
        ("WVal", "[p]".to_string(), format!("[{args}]"))
    } else {
        ("WVal × WVal", "[p.1, p.2]".to_string(), format!("[{args}]"))
    }
}

fn adt_repr_indices(c: &Cert, model_info: &ModelInfo) -> Option<(String, Vec<u32>)> {
    match c.inner() {
        Cert::VariantDispatch { name, arms, .. } => {
            let ty = model_info.fns.get(name)?.params.first()?.clone();
            let ind = model_info.inductives.get(&ty)?;
            // Struct tags are assigned per constructor in declaration order;
            // anchor the base on the smallest dispatched tag. A mis-anchored
            // base renders an unprovable `Repr` and fails the lake build —
            // never a false certificate.
            let base = arms.iter().map(|(t, _)| *t).min()?;
            Some((ty, (0..ind.ctors.len()).map(|i| base + i as u32).collect()))
        }
        Cert::AdtConstructor {
            name, struct_idx, ..
        } => {
            let ty = model_info.fns.get(name)?.ret.clone();
            let ind = model_info.inductives.get(&ty)?;
            let base = *struct_idx;
            let mut indices = Vec::new();
            for i in 0..ind.ctors.len() {
                indices.push(base + i as u32);
            }
            Some((ty, indices))
        }
        _ => None,
    }
}

/// The `And` projection selecting conjunct `pos` of a right-nested `k`-way
/// conjunction: `.1` for the first, `.2.…​.2.1` for the middle, `.2.…​.2` for the
/// last. For the two-member SCC this is `.1` / `.2`.
fn conjunct_proj(pos: usize, k: usize) -> String {
    let mut s = String::new();
    for _ in 0..pos {
        s.push_str(".2");
    }
    if pos + 1 < k {
        s.push_str(".1");
    }
    s
}

/// Evaluate the mutual model at `scc[pos]` on input `n` under partial-correctness
/// semantics (`f n = if n ≤ 0 then base else cross (n-1)`) to obtain the
/// anti-vacuity guard values. Terminates: `n` strictly descends to the base.
fn eval_mutual(scc: &[MutualMember], pos: usize, n: i64) -> i64 {
    if n <= 0 {
        return scc[pos].base_k;
    }
    let cross_pos = scc
        .iter()
        .position(|m| m.self_idx == scc[pos].cross_idx)
        .expect("mutual SCC cross target is a member");
    eval_mutual(scc, cross_pos, n - 1)
}

/// One conjunct's proof block inside `{primary}_mutual_sim`: the carrier-sign
/// dispatch for member `m`, whose base arm boxes `m.base_k` and whose step arm
/// tail-calls its cross target (self index `m.cross_idx`, model `cross_name`),
/// discharged by the matching conjunct `cross_ih` of the fuel induction
/// hypothesis. A verbatim port of the validated probe block, parameterised by
/// name, carrier and the byte-derived indices/literals.
fn mutual_sim_member_block(
    primary: &str,
    c_ty: u32,
    m: &MutualMember,
    cross_name: &str,
    cross_ih: &str,
) -> String {
    let base_lit = lean_int_lit(m.base_k);
    let cs = m.cross_idx;
    let mn = &m.name;
    format!(
        r#"      · intro n v w hv hrun
        rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
        · have hs := hsmall_elim n s sg hv; subst hs
          by_cases hle : s ≤ (0 : Int)
          · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hle] at hrun
            rw [{mn}_base s hle, ← hrun]; exact hsmall_intro {base_lit}
          · simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hle] at hrun
            rcases hsub : sub [.structv {c_ty} [.i64v s, .null, .i32v sg], carrierSmall {c_ty} 1] with _ | vd
            · simp [hsub] at hrun
            · simp only [hsub] at hrun
              have hrd : Repr (s - 1) vd := hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
              rcases hrec : wFuncN {primary}Code ({primary}Host sub) fuel {cs} [vd] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec] at hrun
                have hrr : Repr ({cross_name} (s - 1)) vr := {cross_ih} (s - 1) vd vr hrd hrec
                rw [{mn}_step s hle]; rw [Option.some.injEq] at hrun; rw [← hrun]; exact hrr
        · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
          by_cases hlt : sg < (0 : Int)
          · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
            simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hlt] at hrun
            rw [{mn}_base n hn0, ← hrun]; exact hsmall_intro {base_lit}
          · have hn0 : ¬ n ≤ 0 := by
              intro hle; have : ¬ n < 0 := fun h => hlt (hsign.mpr h); omega
            simp [wFuncN, wRunF, {primary}Code, {primary}Host, boxRef, b32, popArgs, initLocals, hlt] at hrun
            rcases hsub : sub [.structv {c_ty} [.i64v s, .arr lty les, .i32v sg], carrierSmall {c_ty} 1] with _ | vd
            · simp [hsub] at hrun
            · simp only [hsub] at hrun
              have hrd : Repr (n - 1) vd := hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
              rcases hrec : wFuncN {primary}Code ({primary}Host sub) fuel {cs} [vd] with _ | vr
              · simp [hrec] at hrun
              · simp only [hrec] at hrun
                have hrr : Repr ({cross_name} (n - 1)) vr := {cross_ih} (n - 1) vd vr hrd hrec
                rw [{mn}_step n hn0]; rw [Option.some.injEq] at hrun; rw [← hrun]; exact hrr"#,
    )
}

/// The certificate block for a mutually-recursive SCC. Emitted ONCE, by the
/// primary (lowest-`self_idx`) member; the shared block proves ONE conjunction
/// `induction fuel` over all members (`{primary}_mutual_sim`), each cross-call
/// discharged by the matching conjunct of the IH, on top of a conjunction
/// fuel-irrelevance bridge and per-member base/step lemmas. Every member's
/// obligation cites its conjunct. (Two-member SCC for now; the k>2 conjunction
/// is a follow-up.)
fn render_mutual_recursion_cert(c: &Cert) -> String {
    let Cert::MutualRecursion {
        scc,
        carrier,
        position,
        ..
    } = c.inner()
    else {
        unreachable!()
    };
    if *position != 0 {
        // The shared block is emitted once, by the primary member.
        return String::new();
    }
    let k = scc.len();
    let c_ty = *carrier;
    let primary = &scc[0].name;
    // The SCC position of each member's cross-call target.
    let cross_pos: Vec<usize> = scc
        .iter()
        .map(|m| {
            scc.iter()
                .position(|x| x.self_idx == m.cross_idx)
                .expect("cross target is an SCC member")
        })
        .collect();
    let holes = std::iter::repeat_n("?_", k).collect::<Vec<_>>().join(", ");

    let mut s = String::new();
    s.push_str(&format!(
        "/-! ### {primary} — mutual-recursive SCC certificate (carrier type {c_ty}) -/\n\n"
    ));

    // ---- model-side fuel bridge (conjunction; one cap-induction at R = 1) ----
    let bridge_conj = scc
        .iter()
        .map(|m| format!("{m}__fuel k1 n = {m}__fuel k2 n", m = m.name))
        .collect::<Vec<_>>()
        .join(" ∧ ");
    let all_fuel = scc
        .iter()
        .map(|m| format!("{}__fuel", m.name))
        .collect::<Vec<_>>()
        .join(", ");
    let hb_names = (0..k)
        .map(|i| format!("hb{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let bridge_rewrites = scc
        .iter()
        .enumerate()
        .map(|(p, m)| {
            format!(
                "        · simp only [{m}__fuel]; rw [if_neg hn, if_neg hn, hb{cp}]",
                m = m.name,
                cp = cross_pos[p],
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    s.push_str(&format!(
        r#"-- model-side fuel bridge over the whole SCC (the cap-induction pattern at R = 1).
theorem {primary}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {bridge_conj} := by
  intro t
  induction t with
  | zero => intro k1 k2 n ht _ _; omega
  | succ t ih =>
      intro k1 k2 n ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · refine ⟨{holes}⟩ <;> simp [{all_fuel}, hn]
      · have hstep : (n - 1).natAbs < t := by
          have h1n : (1 : Int) ≤ n := by omega
          have h2n : (n - 1).natAbs = n.natAbs - 1 := by omega
          omega
        obtain ⟨{hb_names}⟩ := ih m1 m2 (n - 1) hstep (by omega) (by omega)
        refine ⟨{holes}⟩
{bridge_rewrites}

"#,
    ));

    // ---- per-member base/step lemmas -----------------------------------------
    for (p, m) in scc.iter().enumerate() {
        let cn = &scc[cross_pos[p]].name;
        let base_lit = lean_int_lit(m.base_k);
        let proj = conjunct_proj(cross_pos[p], k);
        let mn = &m.name;
        s.push_str(&format!(
            r#"theorem {mn}_base (n : Int) (hn : n ≤ 0) : {mn} n = {base_lit} := by
  simp [{mn}, {mn}__fuel, hn]

theorem {mn}_step (n : Int) (hn : ¬ n ≤ 0) : {mn} n = {cn} (n - 1) := by
  simp only [{mn}, {mn}__fuel]
  rw [if_neg hn]
  show {cn}__fuel n.natAbs (n - 1) = {cn} (n - 1)
  rw [{cn}]
  exact ({primary}_fuel_irrel (n.natAbs + (n - 1).natAbs + 2) n.natAbs ((n - 1).natAbs + 1)
    (n - 1) (by omega) (by omega) (by omega)){proj}

"#,
        ));
    }

    // ---- the conjunction simulation theorem (one `induction fuel`) -----------
    let sim_conj = scc
        .iter()
        .map(|m| {
            format!(
                "      (∀ n v w, Repr n v →\n        wFuncN {primary}Code ({primary}Host sub) fuel {self_idx} [v] = some w → Repr ({m} n) w)",
                self_idx = m.self_idx,
                m = m.name,
            )
        })
        .collect::<Vec<_>>()
        .join(" ∧\n");
    let ih_names = (0..k)
        .map(|i| format!("ih{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let member_blocks = scc
        .iter()
        .enumerate()
        .map(|(p, m)| {
            mutual_sim_member_block(
                primary,
                c_ty,
                m,
                &scc[cross_pos[p]].name,
                &format!("ih{}", cross_pos[p]),
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    s.push_str(&format!(
        r#"/-- THE CERTIFICATE THEOREM: partial correctness of the VERBATIM emitted mutual
    bodies against the generated models, for ALL n : ℤ, as ONE conjunction proven
    by a single `induction fuel` (each cross-call discharged by the matching IH
    conjunct). -/
theorem {primary}_mutual_sim
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {c_ty} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {c_ty} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {c_ty} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {c_ty} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {c_ty} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (sub : List WVal → Option WVal)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat),
{sim_conj} := by
  intro fuel
  induction fuel with
  | zero =>
      refine ⟨{holes}⟩ <;> · intro n v w hv hrun; simp [wFuncN] at hrun
  | succ fuel ih =>
      obtain ⟨{ih_names}⟩ := ih
      refine ⟨{holes}⟩
{member_blocks}

#print axioms {primary}_mutual_sim

"#,
    ));

    // ---- anti-vacuity: the emitted bodies actually RUN -----------------------
    s.push_str(&format!(
        "-- anti-vacuity: the emitted bodies actually RUN on concrete inputs.\n\
         def {primary}HostRef : HostTbl := {primary}Host (subRef {c_ty})\n"
    ));
    for (p, m) in scc.iter().enumerate() {
        for &input in &[4i64, 3, -1] {
            let expected = eval_mutual(scc, p, input);
            s.push_str(&format!(
                "example :\n    \
                 ((wFuncN {primary}Code {primary}HostRef 40 {self_idx} [carrierSmall {c_ty} {inp}]).bind carrierToInt)\n      \
                 = some {exp} := by native_decide\n",
                self_idx = m.self_idx,
                inp = lean_int_lit(input),
                exp = lean_int_lit128(expected as i128),
            ));
        }
    }

    // ---- per-member schema obligations (each cites its conjunct) -------------
    for (p, m) in scc.iter().enumerate() {
        let proj = conjunct_proj(p, k);
        let mn = &m.name;
        s.push_str(&format!(
            r#"
/-- Schema-shaped simulation obligation for `{mn}` (composed by the single final
    theorem): the emitted mutual body simulates the model `{mn}` via the matching
    conjunct of `{primary}_mutual_sim`. -/
theorem {mn}_simulates : AverCert.Schema.Obligation.holds {mn}Ob := by
  intro S add sub mul stringEq hadd hsub hmul hStringEq fuel ns vs w hrepr hrun
  simp only [{mn}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨hrepr, harity⟩ := hrepr
  cases hrepr with
  | nil =>
      simp at harity
  | cons hv htail =>
      rename_i n v ns vs
      cases htail with
      | nil =>
          simpa [AverCert.Schema.intRepr] using ({primary}_mutual_sim S.Repr S.car S.smallIntro S.smallElim S.bigElim
            sub hsub fuel){proj} n v w hv hrun
      | cons _ _ =>
          simp at harity
"#,
        ));
    }

    s
}

fn render_obligation_def(c: &Cert, model_info: &ModelInfo) -> String {
    let name = c.name();
    match c.inner() {
        Cert::AdtConstructor {
            struct_idx,
            field_count,
            ..
        } if adt_constructor_uses_model(c, model_info) => {
            let sig = model_info.fns.get(name);
            let ret = sig.map(|s| s.ret.as_str()).unwrap_or("Unit");
            let ctor = sig
                .and_then(|s| model_info.inductives.get(&s.ret))
                .and_then(|i| i.ctors.first())
                .map(|c| c.name.as_str())
                .unwrap_or("mk");
            let _ = (struct_idx, field_count);
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := Int, Cod := {ret},\n    \
                 domRepr := fun S n vs => ∃ v, vs = [v] ∧ intRepr S n v,\n    \
                 codRepr := fun S x w => {ret}Repr S x w,\n    \
                 model := fun n => {ret}.{ctor} n }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::AdtConstructor {
            struct_idx,
            arity,
            fields,
            ..
        } => {
            // Verbatim pack certificate (dual of the field projection): the body
            // wraps its `field_count` arguments into variant `struct_idx`. No
            // claim about a recursive model representation — `Cod := WVal` and
            // `verbatimRepr` pin the output to the constructed struct byte-for-byte.
            let (dom, pat, args) = verbatim_ctor_shape(*arity, fields);
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {dom}, Cod := WVal,\n    \
                 domRepr := fun _ p vs => vs = {pat},\n    \
                 codRepr := fun S v w => verbatimRepr S v w,\n    \
                 model := fun p => .structv {struct_idx} {args} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::FieldProjection {
            struct_idx,
            field_idx,
            ..
        } => {
            let model = if *field_idx == 0 { "p.1" } else { "p.2" };
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := WVal × WVal, Cod := WVal,\n    \
                 domRepr := fun _ p vs => vs = [.structv {struct_idx} [p.1, p.2]],\n    \
                 codRepr := fun S v w => verbatimRepr S v w,\n    \
                 model := fun p => {model} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::VariantDispatch { .. } => {
            let ty = model_info
                .fns
                .get(name)
                .and_then(|s| s.params.first())
                .map(|s| s.as_str())
                .unwrap_or("Op");
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {ty}, Cod := Int,\n    \
                 domRepr := fun S o vs => ∃ v, vs = [v] ∧ {ty}Repr S o v,\n    \
                 codRepr := fun S n w => intRepr S n w,\n    \
                 model := {name} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::WidenedIntMatch { .. } => {
            let ty = model_info
                .fns
                .get(name)
                .and_then(|s| s.params.first())
                .map(|s| s.as_str())
                .unwrap_or("Op");
            format!(
                "abbrev {name}Ob : Schema.Obligation :=\n  \
                 {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
                 code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
                 Dom := {ty}, Cod := Int,\n    \
                 domRepr := fun S o vs => ∃ v, vs = [v] ∧ {name}DomRepr S o v,\n    \
                 codRepr := fun S n w => intRepr S n w,\n    \
                 model := {name} }}\n\n",
                carrier = c.carrier(),
                host = c.host_expr(),
                self_idx = c.self_idx(),
            )
        }
        Cert::IntRangePredicate { carrier, .. } => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := Int, Cod := Bool,\n    \
             domRepr := fun _S cp vs => vs = [carrierSmall {carrier} cp],\n    \
             codRepr := fun S b w => boolRepr S b w,\n    \
             model := {name} }}\n\n",
            host = c.host_expr(),
            self_idx = c.self_idx(),
        ),
        Cert::VerbatimWidenedMatch { .. }
        | Cert::VerbatimVariantDispatch { .. }
        | Cert::StringEqVerbatimMatch { .. } => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := WVal, Cod := WVal,\n    \
             domRepr := fun _S v vs => vs = [v],\n    \
             codRepr := fun S x w => verbatimRepr S x w,\n    \
             model := {name}Model }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
        ),
        // Each SCC member is its own integer-simulation obligation, but the `code`
        // and `host` fields point at the ONE shared table/host named after the
        // primary member (`scc[0]`); the model is this member's own function.
        Cert::MutualRecursion { scc, .. } => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{primary}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = 1,\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := fun ns => {name} (ns.headD 0) }}\n\n",
            primary = scc[0].name,
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
        ),
        _ => format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx},\n    \
             Dom := List Int, Cod := Int,\n    \
             domRepr := fun S ns vs => ReprAll S.Repr ns vs ∧ ns.length = {arity},\n    \
             codRepr := fun S n w => intRepr S n w,\n    \
             model := {model} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            model = c.model_expr(),
            arity = c.arity(),
        ),
    }
}

/// The generated manifest literal, mirroring `cert-manifest.json`: the subject
/// metadata plus one `Obligation` per certified export. This is the LITERAL the
/// consumer pins.
fn render_manifest_lean(
    analysis: &Analysis,
    model_roots: &[String],
    model_info: &ModelInfo,
    sha: &str,
) -> String {
    let mut s = String::new();
    s.push_str("import Schema\nimport Module\n");
    for r in model_roots {
        s.push_str(&format!("import {r}\n"));
    }
    s.push_str(
        "\nset_option linter.unusedVariables false\n\n\
         namespace AverCert\nopen AverCert.Schema CertPrelude\n\n",
    );
    s.push_str(&render_user_repr_defs(analysis, model_info));
    // One obligation def per certified export.
    for c in &analysis.certs {
        s.push_str(&render_obligation_def(c, model_info));
    }
    // Subject + manifest.
    let exports = analysis
        .certs
        .iter()
        .map(|c| format!("\"{}\"", c.name()))
        .collect::<Vec<_>>()
        .join(", ");
    let contracts = analysis
        .contracts
        .iter()
        .map(|c| lean_str(c))
        .collect::<Vec<_>>()
        .join(", ");
    let obligations = analysis
        .certs
        .iter()
        .map(|c| format!("{}Ob", c.name()))
        .collect::<Vec<_>>()
        .join(", ");
    s.push_str(&format!(
        "def manifest : Schema.Manifest :=\n  \
         {{ subject :=\n      \
         {{ artifactHash := \"{sha}\",\n        \
         profile := \"{PROFILE_ID}\",\n        \
         abi := \"{RUNTIME_ABI}\",\n        \
         exports := [{exports}],\n        \
         contracts := [{contracts}] }},\n    \
         obligations := [{obligations}] }}\n\n\
         end AverCert\n",
    ));
    s
}

/// The single final theorem: `AverCert.Final.cert : Holds manifest`, proved by
/// composing the per-export `_simulates` obligations. No other final theorem is
/// emitted; the checker pins this exact statement line.
fn render_final(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "import Certificate\nimport Manifest\nimport Schema\n\n\
         set_option maxRecDepth 1000000\n\
         set_option linter.unusedSimpArgs false\n\n\
         open AverCert AverCert.Schema\n\n",
    );
    s.push_str(
        "/-- THE single artifact certificate: the pinned module hash is this module's\n\
        hash, and every certified export simulates its model under the named runtime\n\
        contracts. Proof composes the per-export obligations; nothing else. -/\n",
    );
    s.push_str(&format!("{FINAL_STATEMENT_LINE} := by\n"));
    if analysis.certs.is_empty() {
        s.push_str(
            "  refine ⟨rfl, ?_⟩\n  \
             intro o ho\n  \
             simp only [manifest, List.mem_nil_iff, List.not_mem_nil] at ho\n",
        );
    } else {
        s.push_str("  refine ⟨rfl, ?_⟩\n  intro o ho\n");
        s.push_str(
            "  simp only [manifest, List.mem_cons, List.mem_singleton, List.mem_nil_iff,\n    \
             List.not_mem_nil, or_false] at ho\n",
        );
        // `rcases` with one `rfl` per obligation, split on the disjunction.
        let pattern = std::iter::repeat_n("rfl", analysis.certs.len())
            .collect::<Vec<_>>()
            .join(" | ");
        s.push_str(&format!("  rcases ho with {pattern}\n"));
        // Every resulting goal is closed by exactly one export's obligation.
        let arms = analysis
            .certs
            .iter()
            .map(|c| format!("exact ⟨rfl, CertProofs.{}_simulates⟩", c.name()))
            .collect::<Vec<_>>()
            .join("\n    | ");
        s.push_str(&format!("  all_goals\n    first\n    | {arms}\n"));
    }
    s.push_str(&format!("\n#print axioms {FINAL_THEOREM}\n"));
    s
}

fn render_lakefile(model_roots: &[String]) -> String {
    let mut roots = vec!["`CertPrelude".to_string(), "`Contracts".to_string()];
    for r in model_roots {
        roots.push(format!("`{r}"));
    }
    roots.push("`Module".to_string());
    roots.push("`Schema".to_string());
    roots.push("`Manifest".to_string());
    roots.push("`Certificate".to_string());
    roots.push("`Final".to_string());
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{}]\n",
        roots.join(", ")
    )
}

fn render_manifest(
    analysis: &Analysis,
    model_info: &ModelInfo,
    wasm_name: &str,
    sha: &str,
    schema_sha: &str,
    prelude_sha: &str,
) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    s.push_str(&format!("  \"schema_version\": {CERT_SCHEMA_VERSION},\n"));
    s.push_str(&format!("  \"wasm\": \"{wasm_name}.wasm\",\n"));
    s.push_str(&format!("  \"wasm_sha256\": \"{sha}\",\n"));
    s.push_str(&format!("  \"level\": \"{CERT_LEVEL}\",\n"));
    s.push_str(&format!("  \"profile\": \"{PROFILE_ID}\",\n"));
    s.push_str(&format!("  \"abi\": \"{RUNTIME_ABI}\",\n"));
    s.push_str(&format!("  \"final_theorem\": \"{FINAL_THEOREM}\",\n"));
    s.push_str(&format!("  \"schema_sha256\": \"{schema_sha}\",\n"));
    s.push_str(&format!("  \"prelude_sha256\": \"{prelude_sha}\",\n"));
    if let Some(c) = analysis.carrier {
        s.push_str(&format!("  \"carrier_type_index\": {c},\n"));
    } else {
        s.push_str("  \"carrier_type_index\": null,\n");
    }
    s.push_str("  \"runtime_contracts\": [");
    for (i, c) in analysis.contracts.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!("\n    {}", json_str(c)));
    }
    if !analysis.contracts.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    s.push_str("  \"certified\": [");
    for (i, c) in analysis.certs.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        let kind = match c.inner() {
            Cert::StraightLine { .. } => "straight-line",
            Cert::Recursive { .. } => "self-recursive",
            Cert::AccumulatorRecursive { .. } => "multi-argument self-recursive",
            Cert::AdtConstructor { .. } => "adt-constructor",
            Cert::FieldProjection { .. } => "field-projection",
            Cert::WidenedIntMatch { .. } => "widened-int-match",
            Cert::VerbatimWidenedMatch { .. } => "verbatim-widened-match",
            Cert::VerbatimVariantDispatch { .. } => "verbatim-variant-dispatch",
            Cert::StringEqVerbatimMatch { .. } => "verbatim-widened-match",
            Cert::IntRangePredicate { .. } => "int-range-predicate",
            Cert::VariantDispatch { .. } => "variant-dispatch",
            Cert::Composition { .. } => "cross-function-composition",
            Cert::MutualRecursion { .. } => "mutual-recursive",
            Cert::NonRecursive { .. } => unreachable!(),
        };
        let (dom, cod) = c.source_dom_cod(model_info);
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"class\": \"{}\", \"policy\": \"simulatesModel\", \
             \"level\": \"{}\", \"dom\": {}, \"cod\": {}, \"theorem\": \"CertProofs.{}_wasm_certified\"}}",
            json_str(c.name()),
            kind,
            CERT_LEVEL,
            json_str(&dom),
            json_str(&cod),
            c.name()
        ));
    }
    if !analysis.certs.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    s.push_str("  \"source_level_only\": [");
    for (i, (name, reason)) in analysis.declined.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"reason\": {}}}",
            json_str(name),
            json_str(reason)
        ));
    }
    if !analysis.declined.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("]\n}\n");
    s
}

/// A Lean string literal (escapes `"` and `\`); contract descriptions never
/// contain control characters.
fn lean_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn json_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}
