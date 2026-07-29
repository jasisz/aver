/// Policy presets carried by the Rust manifest schema and independently
/// re-derived by `aver cert verify` from the classified artifact bytes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CertificationPolicy {
    SimulatesModel,
    SimulatesModelTotally,
}

impl CertificationPolicy {
    pub fn manifest_name(self) -> &'static str {
        match self {
            Self::SimulatesModel => "simulatesModel",
            Self::SimulatesModelTotally => "simulatesModelTotally",
        }
    }

    pub fn lean_value(self) -> &'static str {
        match self {
            Self::SimulatesModel => ".simulatesModel",
            Self::SimulatesModelTotally => ".simulatesModelTotally",
        }
    }

    pub fn level(self) -> &'static str {
        match self {
            Self::SimulatesModel => "L1",
            Self::SimulatesModelTotally => "L3",
        }
    }
}

/// Closed v48 measure vocabulary. New variants require a schema bump and a
/// corresponding in-kernel `checkTerm` branch; unknown JSON measures never
/// fall back to this one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TerminationMeasure {
    IntNatAbs { param_idx: u32 },
}

/// Claim-axis termination data. It is deliberately absent from recursion
/// plans and their hashes: the verifier re-derives this value from the admitted
/// recursion family and pins it separately on the obligation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TerminationWitness {
    pub measure: TerminationMeasure,
    pub descent: i64,
}

impl TerminationWitness {
    pub fn lean_value(self) -> String {
        match self.measure {
            TerminationMeasure::IntNatAbs { param_idx } => format!(
                "({{ measure := .intNatAbs {param_idx}, descent := ({}) }} : AverCert.Schema.TerminationWitness)",
                self.descent
            ),
        }
    }
}

/// A certified function and the template holes extracted from its body.
enum Cert {
    /// Generic non-recursive certificate. The inner shape still carries the
    /// byte-derived face and proof parameters; the outer class records that the
    /// non-recursive walker admitted it.
    NonRecursive { inner: Box<Cert> },
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
        /// Declared type index of the exported function, carried for the
        /// byte-first `recursion-plan-v1` artifact claim's function binding.
        /// It does not enter the fuel-induction obligation.
        type_idx: u32,
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
        /// Raw code-entry bytes of the export. The recognizer normalizes
        /// local-alias hops before classification, so a certified body may be
        /// byte-noisier than the canonical template; the `recursion-plan-v1`
        /// claim is emitted ONLY when the canonical plan lowering reproduces
        /// exactly these bytes (otherwise the export stays on the legacy
        /// witness route — never an unverifiable claim).
        code_entry_bytes: Vec<u8>,
    },
    /// countDown-shape two-argument accumulator recursion; box/add/sub host helpers.
    AccumulatorRecursive {
        name: String,
        self_idx: u32,
        type_idx: u32,
        nlocals: usize,
        carrier: u32,
        box_idx: u32,
        add_idx: u32,
        sub_idx: u32,
        /// See `Recursive::code_entry_bytes`.
        code_entry_bytes: Vec<u8>,
    },
    /// Non-recursive constructor: local arguments wrapped by `struct.new`.
    AdtConstructor {
        name: String,
        self_idx: u32,
        type_idx: u32,
        nlocals: usize,
        carrier: u32,
        struct_idx: u32,
        field_count: u32,
        elem_ty: TyKind,
        arity: usize,
        fields: Vec<ConstructorField>,
        ops: Vec<Op>,
    },
    /// Non-recursive record/variant field projection.
    FieldProjection {
        name: String,
        self_idx: u32,
        type_idx: u32,
        nlocals: usize,
        carrier: u32,
        struct_idx: u32,
        field_count: u32,
        field_idx: u32,
        result_ty: TyKind,
        code_entry_bytes: Vec<u8>,
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
        /// Raw code-entry bytes of the export. The `int-dispatch-v1` claim is
        /// emitted ONLY when the canonical plan lowering reproduces exactly
        /// these bytes (otherwise the export stays on the legacy witness
        /// route), exactly like `Cert::VerbatimWidenedMatch::code_entry_bytes`.
        /// The claim binds the export by name (anonymous `FuncBinding`
        /// witness), so no code/type index is carried.
        code_entry_bytes: Vec<u8>,
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
        /// Raw code-entry bytes of the export. The `verbatim-plan-v1` claim is
        /// emitted ONLY when the canonical plan lowering reproduces exactly
        /// these bytes (otherwise analysis declines the export before rendering),
        /// exactly like `Cert::Recursive::code_entry_bytes`. The claim binds the
        /// export by name (anonymous `FuncBinding` witness), so no code/type
        /// index is carried.
        code_entry_bytes: Vec<u8>,
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
        /// Raw code-entry bytes of the export; see
        /// `VerbatimWidenedMatch::code_entry_bytes`.
        code_entry_bytes: Vec<u8>,
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
        type_idx: u32,
        nlocals: usize,
        carrier: u32,
        string_eq_idx: u32,
        arms: Vec<(VerbatimDefault, VerbatimDefault)>,
        default: StringEqDefault,
        ops: Vec<Op>,
    },
    /// Non-recursive String transformation whose body builds a small container
    /// of string arrays (one or more literals from `array.new_data` plus the
    /// function argument), then invokes the contracted `String.concat` host
    /// helper once and returns the byte-concatenated result. The certified body
    /// is loop-free; only the helper is abstract. The simplest beachhead shape
    /// is `fn(s) = prefix ++ s` (or `s ++ suffix`) — a single literal concatenated
    /// with the input on either side.
    StringConcatVerbatimMatch {
        name: String,
        self_idx: u32,
        type_idx: u32,
        nlocals: usize,
        /// The module's Int-carrier struct type index, or `None` when the module
        /// emits no Int carrier at all. Concatenation never touches the carrier,
        /// so unlike every other family this one certifies in both states; the
        /// index only picks the locals prelude the emitter reserved, and the
        /// wall re-derives that choice from the type section rather than taking
        /// it from here (`CertDecode.carrierState`).
        carrier: Option<u32>,
        /// The wasm func index of the String.concat host helper.
        string_concat_idx: u32,
        /// The container type index (the array-of-string-arrays built by
        /// `array.new_fixed`).
        container_ty: u32,
        /// The string byte-array result type returned by the concat helper.
        result_ty: u32,
        /// The literal prefixes (applied before the input). May be empty.
        prefixes: Vec<VerbatimDefault>,
        /// The literal suffixes (applied after the input). May be empty.
        suffixes: Vec<VerbatimDefault>,
        ops: Vec<Op>,
    },
    /// Non-recursive typed expression fragment. The byte-bound body lowers from
    /// a small ordered ANF representation plan (`ExprFragmentPlan`). When the
    /// producer/checker also has a source-level view, `source_plan` preserves
    /// that `SymPlan` instead of making renderers recover it from the
    /// representation shape.
    ExprFragment {
        name: String,
        self_idx: u32,
        type_idx: u32,
        nlocals: usize,
        carrier: u32,
        source_plan: Option<SymPlan>,
        plan: ExprFragmentPlan,
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
        /// Raw code-entry bytes of the export; see
        /// `WidenedIntMatch::code_entry_bytes`.
        code_entry_bytes: Vec<u8>,
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
    /// Declared type index of this member's exported function, carried for the
    /// byte-first `mutual-plan-v1` artifact claim's function binding. It does
    /// not enter the fuel-induction obligation.
    type_idx: u32,
    nlocals: usize,
    /// Literal returned in the base arm (`n ≤ 0`).
    base_k: i64,
    /// The SCC member this one tail-calls in its step arm.
    cross_idx: u32,
    /// Raw code-entry bytes of this member's export. The `mutual-plan-v1` claim
    /// is emitted ONLY when the canonical plan lowering reproduces exactly these
    /// bytes (otherwise analysis declines the member before rendering — never
    /// an unverifiable claim), exactly like `Cert::Recursive::code_entry_bytes`.
    code_entry_bytes: Vec<u8>,
}
