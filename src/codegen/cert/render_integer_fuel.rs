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
    /// Optional L3 theorem block. Empty for every family outside the promoted
    /// unary add/sub descent-by-one class.
    total: String,
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
        total,
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

{total}

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
