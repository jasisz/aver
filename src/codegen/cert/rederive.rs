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
    /// Position of the export's user function in the module's byte-derived user
    /// function list. The verifier uses this to merge byte-derived legacy
    /// obligations with plan-first expr-fragment sidecar obligations without
    /// trusting JSON order.
    pub func_order: usize,
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
    /// For expression fragments, the byte-derived defined-function index into
    /// the code section (`funcIdx - importedFuncCount`). Lean's relevant Wasm
    /// slicer re-derives and pins this through `FuncBinding`.
    pub fragment_code_idx: Option<u32>,
    /// For expression fragments, the byte-derived function-section type index.
    /// This connects export/function routing to the actual declared function
    /// signature slot before later in-kernel signature decoding lands.
    pub fragment_type_idx: Option<u32>,
    /// For expression fragments, the byte-derived local count recorded in the
    /// `WCode` body. The current plan-first lowering emits one scratch carrier
    /// local; this pin keeps the artifact acceptance bridge tied to the schema
    /// code table rather than only to the lowered instruction list.
    pub fragment_nlocals: Option<u32>,
    /// For expression fragments, the canonical plan sidecar attached to this
    /// obligation. During `aver cert verify`, transitional byte re-derivation
    /// may populate this first, then the plan-first sidecar checker overlays the
    /// obligation with the checked sidecar plan after canonical code-entry
    /// equality succeeds.
    pub fragment_plan: Option<FragmentPlanSidecar>,
    /// The same checked expression-fragment plan rendered as a Lean
    /// `ExprFragmentRawPlan` term. `aver cert verify` pins
    /// `manifest.exprFragmentPlans` to these checker-rendered terms so
    /// `Plans.lean` cannot drift from the sidecar/body pair.
    pub fragment_plan_lean: Option<String>,
    /// For expression fragments whose checked representation plan can be
    /// projected into the source-level symbolic grammar, the canonical
    /// `sym-fragment-v1` sidecar.
    pub fragment_sym_plan: Option<FragmentPlanSidecar>,
    /// For expression fragments whose checked representation plan can be
    /// projected into the source-level symbolic grammar, the same verifier
    /// result rendered as a `SymRawPlan` term. This feeds the preferred v2
    /// artifact claim shape; representation-only fragments leave it absent.
    pub fragment_sym_plan_lean: Option<String>,
    /// For field-projection fragments, the byte-derived struct-table entries
    /// (`source type name -> wasm struct type index`) this export pins. The
    /// verifier unions these into the module-wide struct table its witnesses
    /// and artifact claims cite; empty for non-projection fragments.
    pub fragment_struct_entries: Vec<(String, u32)>,
    /// For expression fragments, the verifier-rendered `List WInstr` body that
    /// the checked plan canonically lowers to. The checker witness pins
    /// `PlanLower.lowerExprFragmentBody carrier plan = some body`.
    pub fragment_lowered_body_lean: Option<String>,
    /// For expression fragments, the verifier-rendered canonical raw code-entry
    /// bytes that the checked plan lowers to. The checker witness pins
    /// `PlanBytes.lowerExprFragmentCodeEntry carrier plan = some bytes`.
    pub fragment_lowered_code_entry_lean: Option<String>,
    /// For `string-concat-v1`, the byte-derived target-bound concat plan.
    pub string_concat_plan: Option<FragmentPlanSidecar>,
    /// For `string-concat-v1`, the byte-derived source-level symbolic view of
    /// the concat plan. This is the JSON/sidecar counterpart of
    /// `string_concat_sym_plan_lean`.
    pub string_concat_sym_plan: Option<FragmentPlanSidecar>,
    /// The same checked string-concat plan rendered as a Lean
    /// `StringConcatRawPlan` term. `aver cert verify` pins
    /// `manifest.stringConcatPlans` to these checker-rendered terms so
    /// `Plans.lean` cannot drift from the verified sidecar/body pair.
    pub string_concat_plan_lean: Option<String>,
    /// The source-level `SymRawPlan` view of the same byte-derived string
    /// concat shape. The checker witness requires this to explain the
    /// target-bound `StringConcatRawPlan`.
    pub string_concat_sym_plan_lean: Option<String>,
    /// For `string-concat-v1`, the byte-derived defined-function index into the
    /// code section (`funcIdx - importedFuncCount`).
    pub string_concat_code_idx: Option<u32>,
    /// For `string-concat-v1`, the byte-derived function-section type index.
    pub string_concat_type_idx: Option<u32>,
    /// For `string-concat-v1`, the verifier-rendered `List WInstr` body that the
    /// checked source plan canonically lowers to.
    pub string_concat_lowered_body_lean: Option<String>,
    /// For `string-concat-v1`, the verifier-rendered canonical raw code-entry
    /// bytes that the checked source plan lowers to.
    pub string_concat_lowered_code_entry_lean: Option<String>,
    /// The string byte-array type used by `array.new_data` and returned by the
    /// concat helper.
    pub string_concat_result_ty: Option<u32>,
    /// The array-of-string-arrays container type used by `array.new_fixed`.
    pub string_concat_container_ty: Option<u32>,
    /// The wasm function index of the `String.concat` host helper.
    pub string_concat_func_idx: Option<u32>,
    /// For `string-eq-v1`, the byte-derived target-bound equality dispatch plan.
    pub string_eq_plan: Option<FragmentPlanSidecar>,
    /// For `string-eq-v1`, the byte-derived source-level symbolic view.
    pub string_eq_sym_plan: Option<FragmentPlanSidecar>,
    /// The same checked String.eq plan rendered as a Lean `StringEqRawPlan` term.
    pub string_eq_plan_lean: Option<String>,
    /// The source-level `SymRawPlan` view of the same byte-derived String.eq shape.
    pub string_eq_sym_plan_lean: Option<String>,
    /// For `string-eq-v1`, the byte-derived defined-function index.
    pub string_eq_code_idx: Option<u32>,
    /// For `string-eq-v1`, the byte-derived function-section type index.
    pub string_eq_type_idx: Option<u32>,
    /// For `string-eq-v1`, the verifier-rendered `List WInstr` body.
    pub string_eq_lowered_body_lean: Option<String>,
    /// For `string-eq-v1`, the verifier-rendered canonical raw code-entry bytes.
    pub string_eq_lowered_code_entry_lean: Option<String>,
    /// The string byte-array type used by the String.eq dispatch.
    pub string_eq_string_ty: Option<u32>,
    /// The wasm function index of the `String.eq` host helper.
    pub string_eq_func_idx: Option<u32>,
    /// For `construct-v1`, the byte-derived target-bound ADT constructor plan.
    pub construct_plan: Option<FragmentPlanSidecar>,
    /// For `construct-v1`, the byte-derived source-level symbolic constructor
    /// view. This is reconstructed from the checked source model plus the
    /// byte-derived constructor shape.
    pub construct_sym_plan: Option<FragmentPlanSidecar>,
    /// The same checked construct plan rendered as a Lean `ConstructRawPlan`
    /// term. `aver cert verify` pins `manifest.constructPlans` to these
    /// checker-rendered terms.
    pub construct_plan_lean: Option<String>,
    /// The source-level `SymRawPlan` view of the same byte-derived constructor
    /// shape.
    pub construct_sym_plan_lean: Option<String>,
    /// For `construct-v1`, the byte-derived defined-function index into the
    /// code section (`funcIdx - importedFuncCount`).
    pub construct_code_idx: Option<u32>,
    /// For `construct-v1`, the byte-derived function-section type index.
    pub construct_type_idx: Option<u32>,
    /// For `construct-v1`, the verifier-rendered `List WInstr` body that the
    /// checked constructor plan canonically lowers to.
    pub construct_lowered_body_lean: Option<String>,
    /// For `construct-v1`, the verifier-rendered canonical raw code-entry bytes
    /// that the checked constructor plan lowers to.
    pub construct_lowered_code_entry_lean: Option<String>,
    /// For `recursion-plan-v1` (fuel-recursion), the byte-derived recursion plan
    /// rendered as a Lean `RecursionRawPlan` term. `aver cert verify` pins
    /// `manifest.recursionPlans` to this checker-rendered term.
    pub recursion_plan_lean: Option<String>,
    /// For `recursion-plan-v1`, the per-export byte-derived host-role table
    /// (box/combinator/sub) rendered as the Lean `List (HostRole × Nat)`
    /// literal the recursion claim carries.
    pub recursion_host_table_lean: Option<String>,
    /// For `recursion-plan-v1`, the byte-derived defined-function index.
    pub recursion_code_idx: Option<u32>,
    /// For `recursion-plan-v1`, the byte-derived function-section type index.
    pub recursion_type_idx: Option<u32>,
    /// For `recursion-plan-v1`, the verifier-rendered `List WInstr` body the
    /// checked recursion plan canonically lowers to (equal to `Module.lean`).
    pub recursion_lowered_body_lean: Option<String>,
    /// For `recursion-plan-v1`, the verifier-rendered canonical raw code-entry
    /// bytes the checked recursion plan lowers to.
    pub recursion_lowered_code_entry_lean: Option<String>,
    /// For `mutual-plan-v1` (mutual-recursion member), the byte-derived member
    /// plan rendered as a Lean `MutualRawPlan` term. `aver cert verify` pins
    /// `manifest.mutualPlans` to this checker-rendered term.
    pub mutual_plan_lean: Option<String>,
    /// For `mutual-plan-v1`, the byte-derived SCC box/sub host-role table
    /// rendered as the Lean `List (HostRole × Nat)` literal the claim carries.
    pub mutual_host_table_lean: Option<String>,
    /// For `mutual-plan-v1`, the byte-derived SCC member-index set rendered as
    /// the Lean `List Nat` literal the claim threads as member-call context.
    pub mutual_member_set_lean: Option<String>,
    /// For `mutual-plan-v1`, this member's byte-derived defined-function index.
    pub mutual_code_idx: Option<u32>,
    /// For `mutual-plan-v1`, this member's byte-derived function-section type index.
    pub mutual_type_idx: Option<u32>,
    /// For `mutual-plan-v1`, the verifier-rendered `List WInstr` body this
    /// member's checked plan lowers to (equal to its arm of the shared table).
    pub mutual_lowered_body_lean: Option<String>,
    /// For `mutual-plan-v1`, the verifier-rendered canonical raw code-entry
    /// bytes this member's checked plan lowers to.
    pub mutual_lowered_code_entry_lean: Option<String>,
    /// For `verbatim-plan-v1` (a `Cod := WVal` verbatim `ref.test`-dispatch
    /// match), the byte-derived plan rendered as a Lean `VerbatimRawPlan` term.
    /// `aver cert verify` pins `manifest.verbatimPlans` to this checker-rendered
    /// term. There is no host/self call to bind, so no host-table/member-set/
    /// lowered-body/code-entry companion fields are needed: the byte-equality
    /// gate is the whole soundness binding and the claim's witness is anonymous.
    pub verbatim_plan_lean: Option<String>,
    /// For `int-dispatch-v1` (a `Cod := Int` ADT-match: general variant
    /// dispatch or widened Int match), the byte-derived plan rendered as a Lean
    /// `IntDispatchRawPlan` term. `aver cert verify` pins
    /// `manifest.intDispatchPlans` to this checker-rendered term. The claim's
    /// witness is anonymous like the verbatim family's (no code/type index is
    /// carried); unlike it the arms consume host contracts, so the claim also
    /// carries the byte-derived role table below.
    pub int_dispatch_plan_lean: Option<String>,
    /// For `int-dispatch-v1`, the per-export byte-derived host-role table
    /// (box, plus add/sub exactly when wired) rendered as the Lean
    /// `List (HostRole × Nat)` literal the claim carries.
    pub int_dispatch_host_table_lean: Option<String>,
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
    /// Typed expression fragment: `Dom` is the product of all lifted wasm
    /// parameters, `Cod` is the lifted root type, and `domRepr` is the exact
    /// wasm argument vector reconstructed from the byte-derived parameter types.
    ExprFragment {
        params: Vec<FragTy>,
        result: FragTy,
    },
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
        // A host-call expr fragment with the straight-line integer face keeps
        // the full-strength integer simulation face (List Int / ReprAll /
        // intRepr), exactly like the legacy straight-line class.
        if c.int_add_face().is_some() {
            return ObligationFace::IntList { arity: 1 };
        }
        // A projection-faced expr fragment keeps the legacy field-projection
        // semantic face (WVal x WVal / verbatimRepr), exactly like the legacy
        // class it migrates.
        if let Some(face) = c.project_face() {
            return ObligationFace::Projection {
                struct_idx: face.struct_idx,
            };
        }
        match c.inner() {
            Cert::StraightLine { .. }
            | Cert::Recursive { .. }
            | Cert::AccumulatorRecursive { .. }
            | Cert::Composition { .. } => ObligationFace::IntList { arity: c.arity() },
            Cert::FieldProjection { struct_idx, .. } => ObligationFace::Projection {
                struct_idx: *struct_idx,
            },
            Cert::ExprFragment { plan, .. } => ObligationFace::ExprFragment {
                params: plan.params.clone(),
                result: plan.result,
            },
            Cert::VerbatimWidenedMatch { .. }
            | Cert::VerbatimVariantDispatch { .. }
            | Cert::StringEqVerbatimMatch { .. }
            | Cert::StringConcatVerbatimMatch { .. } => ObligationFace::VerbatimWidened,
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
            ObligationFace::ExprFragment { params, result } => {
                let repr = match result {
                    FragTy::F64 => "floatBitsRepr",
                    FragTy::BoolI32 => "boolRepr",
                    FragTy::IntCarrier
                    | FragTy::I64
                    | FragTy::RawI32
                    | FragTy::Ref
                    | FragTy::AdtRef => "verbatimRepr",
                };
                format!(
                    "class: expr-fragment-v1  |  Dom: {}  Cod: {}  codRepr: {repr}",
                    expr_fragment_dom_type(params),
                    result.lean_dom_type()
                )
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
            ObligationFace::ExprFragment { params, result } => {
                let dom = expr_fragment_dom_type(params);
                let cod = result.lean_dom_type();
                let dom_repr = expr_fragment_dom_repr_list(params, "p", "o.carrier");
                let cod_repr = match result {
                    FragTy::F64 => "AverCert.Schema.floatBitsRepr",
                    FragTy::BoolI32 => "AverCert.Schema.boolRepr",
                    FragTy::IntCarrier
                    | FragTy::I64
                    | FragTy::RawI32
                    | FragTy::Ref
                    | FragTy::AdtRef => "AverCert.Schema.verbatimRepr",
                };
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Dom) = some ({dom}) := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ({obl}[{idx}]?).map (fun o => o.Cod) = some {cod} := rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o → \
                     HEq o.codRepr (@{cod_repr} o.carrier) := by\n  \
                     intro o h\n  {reduce}\n  subst h; exact HEq.rfl\n"
                ));
                s.push_str(&format!(
                    "example : ∀ o, {obl}[{idx}]? = some o →\n    \
                     HEq o.domRepr (fun (_S : AverCert.Schema.CarrierSpec o.carrier) \
                     (p : {dom}) (vs : List CertPrelude.WVal) =>\n      \
                     vs = {dom_repr}) := by\n  \
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
    rederive_certificate_inner(wasm_bytes, model_files, &[])
}

/// Re-derive only non-expression-fragment obligations. The verifier uses this
/// for plan-first checking: expr fragments are admitted by checked sidecar plans
/// plus canonical code-entry byte equality, not by the old byte lifter.
/// `plan_covered_exports` are the export names the manifest routes through
/// checked plan sidecars; they are excluded from legacy byte classification BY
/// NAME so a plan-first export that also happens to match a legacy template
/// (e.g. the straight-line integer shape) does not produce a duplicate
/// obligation for the same function. This never widens acceptance: an export
/// claimed as plan-first that fails its sidecar check is declined outright.
pub fn rederive_certificate_without_expr_fragments(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
    plan_covered_exports: &[String],
) -> Result<RederivedCertificate, String> {
    rederive_certificate_inner(wasm_bytes, model_files, plan_covered_exports)
}

fn rederive_certificate_inner(
    wasm_bytes: &[u8],
    model_files: &[(String, String)],
    plan_covered_exports: &[String],
) -> Result<RederivedCertificate, String> {
    let (user_fns, box_idx, user_idx_set, carrier, host_roles, _frag_host_table, _struct_field_counts) =
        disassemble(wasm_bytes)?;
    let model_ops = model_step_ops(model_files);
    let model_info = ModelInfo::from_files(model_files);
    let fns: std::collections::HashMap<u32, &UserFn> =
        user_fns.iter().map(|f| (f.wasm_idx, f)).collect();
    let mut certs = Vec::new();
    for (func_order, f) in user_fns.iter().enumerate() {
        if plan_covered_exports.contains(&f.name) {
            continue;
        }
        let classified = classify_without_expr_fragment(
            f,
            box_idx,
            carrier,
            &user_idx_set,
            &fns,
            &host_roles,
            &model_ops,
        );
        if let Ok(c) = classified {
            certs.push((func_order, c));
        }
    }
    let contracts = runtime_contracts_for_certs(certs.iter().map(|(_, c)| c));
    let obligations = certs
        .iter()
        .map(|(func_order, c)| RederivedObligation {
            name: c.name().to_string(),
            func_order: *func_order,
            code: render_code_value(c),
            host: render_host_value(c),
            self_idx: c.self_idx(),
            carrier: c.carrier(),
            face: ObligationFace::of_cert(c),
            fragment_code_idx: match c.inner() {
                Cert::ExprFragment { code_idx, .. } => Some(*code_idx),
                _ => None,
            },
            fragment_type_idx: match c.inner() {
                Cert::ExprFragment { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            fragment_nlocals: match c.inner() {
                Cert::ExprFragment { nlocals, .. } => Some(*nlocals as u32),
                _ => None,
            },
            fragment_plan: match c.inner() {
                Cert::ExprFragment { plan, .. } => Some(expr_fragment_sidecar(c.name(), plan)),
                _ => None,
            },
            fragment_plan_lean: match c.inner() {
                Cert::ExprFragment { plan, .. } => Some(expr_fragment_plan_lean_value(plan)),
                _ => None,
            },
            fragment_sym_plan_lean: match c.inner() {
                Cert::ExprFragment {
                    source_plan, plan, ..
                } => expr_fragment_source_plan(source_plan, plan).map(|sym| sym_plan_lean_value(&sym)),
                _ => None,
            },
            fragment_sym_plan: match c.inner() {
                Cert::ExprFragment {
                    source_plan, plan, ..
                } => expr_fragment_source_plan(source_plan, plan)
                    .map(|sym| sym_fragment_sidecar(c.name(), &sym)),
                _ => None,
            },
            fragment_struct_entries: match c.inner() {
                Cert::ExprFragment {
                    source_plan: Some(source_plan),
                    plan,
                    ..
                } => expr_fragment_struct_table_entries(source_plan, plan).unwrap_or_default(),
                _ => Vec::new(),
            },
            fragment_lowered_body_lean: match c.inner() {
                Cert::ExprFragment { ops, .. } => Some(render_ops_value(ops)),
                _ => None,
            },
            fragment_lowered_code_entry_lean: match c.inner() {
                Cert::ExprFragment { carrier, plan, .. } => {
                    lower_expr_fragment_plan_code_entry_bytes(plan, *carrier)
                        .ok()
                        .map(|bytes| render_byte_list(&bytes))
                }
                _ => None,
            },
            string_concat_plan: match c.inner() {
                Cert::StringConcatVerbatimMatch { .. } => {
                    string_concat_plan_from_cert(c).map(|plan| string_concat_sidecar(c.name(), &plan))
                }
                _ => None,
            },
            string_concat_sym_plan: match c.inner() {
                Cert::StringConcatVerbatimMatch { .. } => string_concat_plan_from_cert(c)
                    .map(|plan| string_concat_sym_plan_from_plan(&plan))
                    .map(|plan| sym_fragment_sidecar(c.name(), &plan)),
                _ => None,
            },
            string_concat_plan_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch { .. } => {
                    string_concat_plan_from_cert(c).map(|plan| string_concat_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_concat_sym_plan_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch { .. } => {
                    string_concat_sym_plan_from_cert(c).map(|plan| sym_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_concat_code_idx: match c.inner() {
                Cert::StringConcatVerbatimMatch { code_idx, .. } => Some(*code_idx),
                _ => None,
            },
            string_concat_type_idx: match c.inner() {
                Cert::StringConcatVerbatimMatch { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            string_concat_lowered_body_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch {
                    result_ty,
                    container_ty,
                    string_concat_idx,
                    ..
                } => string_concat_plan_from_cert(c)
                    .and_then(|plan| {
                        lower_string_concat_plan(
                            &plan,
                            *result_ty,
                            *container_ty,
                            *string_concat_idx,
                        )
                        .ok()
                    })
                    .map(|ops| render_ops_value(&ops)),
                _ => None,
            },
            string_concat_lowered_code_entry_lean: match c.inner() {
                Cert::StringConcatVerbatimMatch {
                    carrier,
                    result_ty,
                    container_ty,
                    string_concat_idx,
                    ..
                } => string_concat_plan_from_cert(c)
                    .and_then(|plan| {
                        lower_string_concat_plan_code_entry_bytes(
                            &plan,
                            *carrier,
                            *result_ty,
                            *container_ty,
                            *string_concat_idx,
                        )
                        .ok()
                    })
                    .map(|bytes| render_byte_list(&bytes)),
                _ => None,
            },
            string_concat_result_ty: match c.inner() {
                Cert::StringConcatVerbatimMatch { result_ty, .. } => Some(*result_ty),
                _ => None,
            },
            string_concat_container_ty: match c.inner() {
                Cert::StringConcatVerbatimMatch { container_ty, .. } => Some(*container_ty),
                _ => None,
            },
            string_concat_func_idx: match c.inner() {
                Cert::StringConcatVerbatimMatch {
                    string_concat_idx, ..
                } => Some(*string_concat_idx),
                _ => None,
            },
            string_eq_plan: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => {
                    string_eq_plan_from_cert(c).map(|plan| string_eq_sidecar(c.name(), &plan))
                }
                _ => None,
            },
            string_eq_sym_plan: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => string_eq_plan_from_cert(c)
                    .map(|plan| string_eq_sym_plan_from_plan(&plan))
                    .map(|plan| sym_fragment_sidecar(c.name(), &plan)),
                _ => None,
            },
            string_eq_plan_lean: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => {
                    string_eq_plan_from_cert(c).map(|plan| string_eq_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_eq_sym_plan_lean: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => {
                    string_eq_sym_plan_from_cert(c).map(|plan| sym_plan_lean_value(&plan))
                }
                _ => None,
            },
            string_eq_code_idx: match c.inner() {
                Cert::StringEqVerbatimMatch { code_idx, .. } => Some(*code_idx),
                _ => None,
            },
            string_eq_type_idx: match c.inner() {
                Cert::StringEqVerbatimMatch { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            string_eq_lowered_body_lean: match c.inner() {
                Cert::StringEqVerbatimMatch { string_eq_idx, .. } => {
                    string_eq_string_ty_from_cert(c).and_then(|string_ty| {
                        string_eq_plan_from_cert(c)
                            .and_then(|plan| {
                                lower_string_eq_plan(&plan, string_ty, *string_eq_idx).ok()
                            })
                            .map(|ops| render_ops_value(&ops))
                    })
                }
                _ => None,
            },
            string_eq_lowered_code_entry_lean: match c.inner() {
                Cert::StringEqVerbatimMatch {
                    carrier,
                    string_eq_idx,
                    ..
                } => {
                    string_eq_string_ty_from_cert(c).and_then(|string_ty| {
                        string_eq_plan_from_cert(c)
                            .and_then(|plan| {
                                lower_string_eq_plan_code_entry_bytes(
                                    &plan,
                                    *carrier,
                                    string_ty,
                                    *string_eq_idx,
                                )
                                .ok()
                            })
                            .map(|bytes| render_byte_list(&bytes))
                    })
                }
                _ => None,
            },
            string_eq_string_ty: match c.inner() {
                Cert::StringEqVerbatimMatch { .. } => string_eq_string_ty_from_cert(c),
                _ => None,
            },
            string_eq_func_idx: match c.inner() {
                Cert::StringEqVerbatimMatch { string_eq_idx, .. } => Some(*string_eq_idx),
                _ => None,
            },
            construct_plan: match c.inner() {
                Cert::AdtConstructor { .. } => {
                    construct_plan_from_cert(c).map(|plan| construct_sidecar(c.name(), &plan))
                }
                _ => None,
            },
            construct_sym_plan: match c.inner() {
                Cert::AdtConstructor { .. } => adt_constructor_sym_plan_from_cert(c, &model_info)
                    .map(|plan| sym_fragment_sidecar(c.name(), &plan)),
                _ => None,
            },
            construct_plan_lean: match c.inner() {
                Cert::AdtConstructor { .. } => {
                    construct_plan_from_cert(c).map(|plan| construct_plan_lean_value(&plan))
                }
                _ => None,
            },
            construct_sym_plan_lean: match c.inner() {
                Cert::AdtConstructor { .. } => adt_constructor_sym_plan_from_cert(c, &model_info)
                    .map(|plan| sym_plan_lean_value(&plan)),
                _ => None,
            },
            construct_code_idx: match c.inner() {
                Cert::AdtConstructor { code_idx, .. } => Some(*code_idx),
                _ => None,
            },
            construct_type_idx: match c.inner() {
                Cert::AdtConstructor { type_idx, .. } => Some(*type_idx),
                _ => None,
            },
            construct_lowered_body_lean: match c.inner() {
                Cert::AdtConstructor { .. } => construct_plan_from_cert(c)
                    .and_then(|plan| lower_construct_plan(&plan).ok())
                    .map(|ops| render_ops_value(&ops)),
                _ => None,
            },
            construct_lowered_code_entry_lean: match c.inner() {
                Cert::AdtConstructor { carrier, .. } => construct_plan_from_cert(c)
                    .and_then(|plan| lower_construct_plan_code_entry_bytes(&plan, *carrier).ok())
                    .map(|bytes| render_byte_list(&bytes)),
                _ => None,
            },
            recursion_plan_lean: recursion_plan_from_cert(c)
                .map(|plan| recursion_plan_lean_value(&plan)),
            recursion_host_table_lean: match c.inner() {
                Cert::Recursive {
                    box_idx,
                    add_idx,
                    sub_idx,
                    ..
                }
                | Cert::AccumulatorRecursive {
                    box_idx,
                    add_idx,
                    sub_idx,
                    ..
                } => Some(recursion_host_table_lean_value(*box_idx, *add_idx, *sub_idx)),
                _ => None,
            },
            recursion_code_idx: match c.inner() {
                Cert::Recursive { code_idx, .. } | Cert::AccumulatorRecursive { code_idx, .. } => {
                    Some(*code_idx)
                }
                _ => None,
            },
            recursion_type_idx: match c.inner() {
                Cert::Recursive { type_idx, .. } | Cert::AccumulatorRecursive { type_idx, .. } => {
                    Some(*type_idx)
                }
                _ => None,
            },
            recursion_lowered_body_lean: match c.inner() {
                Cert::Recursive { carrier, .. } | Cert::AccumulatorRecursive { carrier, .. } => {
                    recursion_plan_from_cert(c)
                        .and_then(|plan| lower_expr_fragment_plan(&plan, *carrier).ok())
                        .map(|ops| render_ops_value(&ops))
                }
                _ => None,
            },
            recursion_lowered_code_entry_lean: match c.inner() {
                Cert::Recursive { carrier, .. } | Cert::AccumulatorRecursive { carrier, .. } => {
                    recursion_plan_from_cert(c)
                        .and_then(|plan| {
                            lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier).ok()
                        })
                        .map(|bytes| render_byte_list(&bytes))
                }
                _ => None,
            },
            mutual_plan_lean: mutual_plan_from_cert(c).map(|plan| mutual_plan_lean_value(&plan)),
            mutual_host_table_lean: match c.inner() {
                Cert::MutualRecursion {
                    box_idx, sub_idx, ..
                } => Some(mutual_host_table_lean_value(*box_idx, *sub_idx)),
                _ => None,
            },
            mutual_member_set_lean: match c.inner() {
                Cert::MutualRecursion { scc, .. } => Some(mutual_member_set_lean_value(scc)),
                _ => None,
            },
            mutual_code_idx: match c.inner() {
                Cert::MutualRecursion { position, scc, .. } => {
                    scc.get(*position).map(|m| m.code_idx)
                }
                _ => None,
            },
            mutual_type_idx: match c.inner() {
                Cert::MutualRecursion { position, scc, .. } => {
                    scc.get(*position).map(|m| m.type_idx)
                }
                _ => None,
            },
            mutual_lowered_body_lean: match c.inner() {
                Cert::MutualRecursion { carrier, .. } => mutual_plan_from_cert(c)
                    .and_then(|plan| lower_expr_fragment_plan(&plan, *carrier).ok())
                    .map(|ops| render_ops_value(&ops)),
                _ => None,
            },
            mutual_lowered_code_entry_lean: match c.inner() {
                Cert::MutualRecursion { carrier, .. } => mutual_plan_from_cert(c)
                    .and_then(|plan| {
                        lower_expr_fragment_plan_code_entry_bytes(&plan, *carrier).ok()
                    })
                    .map(|bytes| render_byte_list(&bytes)),
                _ => None,
            },
            verbatim_plan_lean: verbatim_plan_from_cert(c)
                .map(|plan| verbatim_plan_lean_value(&plan)),
            int_dispatch_plan_lean: int_dispatch_plan_from_cert(c)
                .map(|plan| int_dispatch_plan_lean_value(&plan)),
            int_dispatch_host_table_lean: match int_dispatch_plan_from_cert(c) {
                Some(_) => int_dispatch_host_table_from_cert(c)
                    .map(|hosts| int_dispatch_host_table_lean_value(&hosts)),
                None => None,
            },
        })
        .collect();
    Ok(RederivedCertificate {
        obligations,
        contracts,
    })
}
