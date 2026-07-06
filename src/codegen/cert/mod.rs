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
//! bytes (`rederive_obligations`) and pins the re-derived `code`/`host`/`self`/
//! `carrier` values into its checker-authored witness with `rfl` against the
//! proven `manifest.obligations` — so the `WInstr` data the kernel theorem
//! actually reasons about is forced to equal what the bytes decode to, not
//! merely trusted. This is trusted via inspection of the disassembler, not by an
//! in-kernel wasm decode proof (a full kernel decoder is a deferred residual).

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
pub const CERT_SCHEMA_VERSION: u32 = 3;
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
struct UserFn {
    name: String,
    wasm_idx: u32,
    arity: usize,
    nlocals: usize,
    ops: Vec<Op>,
    /// call targets in body order, for reason reporting.
    calls: Vec<u32>,
    has_loop_or_branch: bool,
}

/// The minimal opcode surface the two templates need. Anything else is `Other`
/// (which forces a decline) — a certified body never contains an `Other`.
#[derive(Clone, PartialEq)]
enum Op {
    LocalGet(u32),
    LocalSet(u32),
    I64Const(i64),
    I32Const(i32),
    RefTest(u32),
    RefCast(u32),
    StructNew(u32, u32),
    StructGet(u32, u32),
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

/// A certified function and the template holes extracted from its body.
enum Cert {
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
    /// sumTo-shape self-recursion; box/add/sub host helpers.
    Recursive {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        box_idx: u32,
        add_idx: u32,
        sub_idx: u32,
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
    /// Non-recursive three-variant ADT match: Add x -> x; Neg x -> 0 - x; Zero -> 0.
    AdtMatch {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        add_variant_idx: u32,
        neg_variant_idx: u32,
        zero_variant_idx: u32,
        box_idx: u32,
        sub_idx: u32,
        ops: Vec<Op>,
    },
}

impl Cert {
    fn name(&self) -> &str {
        match self {
            Cert::StraightLine { name, .. }
            | Cert::Recursive { name, .. }
            | Cert::AccumulatorRecursive { name, .. }
            | Cert::AdtConstructor { name, .. }
            | Cert::FieldProjection { name, .. }
            | Cert::AdtMatch { name, .. } => name,
        }
    }
    fn self_idx(&self) -> u32 {
        match self {
            Cert::StraightLine { self_idx, .. }
            | Cert::Recursive { self_idx, .. }
            | Cert::AccumulatorRecursive { self_idx, .. }
            | Cert::AdtConstructor { self_idx, .. }
            | Cert::FieldProjection { self_idx, .. }
            | Cert::AdtMatch { self_idx, .. } => *self_idx,
        }
    }
    fn carrier(&self) -> u32 {
        match self {
            Cert::StraightLine { carrier, .. }
            | Cert::Recursive { carrier, .. }
            | Cert::AccumulatorRecursive { carrier, .. }
            | Cert::AdtConstructor { carrier, .. }
            | Cert::FieldProjection { carrier, .. }
            | Cert::AdtMatch { carrier, .. } => *carrier,
        }
    }
    fn arity(&self) -> usize {
        match self {
            Cert::StraightLine { .. } | Cert::Recursive { .. } => 1,
            Cert::AccumulatorRecursive { .. } => 2,
            Cert::AdtConstructor { field_count, .. } => *field_count as usize,
            Cert::FieldProjection { .. } | Cert::AdtMatch { .. } => 1,
        }
    }
    /// The Lean expression for the model this export simulates.
    fn model_expr(&self) -> String {
        match self {
            Cert::StraightLine { k, .. } => format!("fun ns => ns.headD 0 + ({k})"),
            Cert::Recursive { name, .. } => format!("fun ns => {name} (ns.headD 0)"),
            Cert::AccumulatorRecursive { name, .. } => {
                format!("fun ns => {name} (ns.headD 0) ((ns.drop 1).headD 0)")
            }
            Cert::AdtConstructor { .. } | Cert::FieldProjection { .. } | Cert::AdtMatch { .. } => {
                "fun x => x".to_string()
            }
        }
    }
    /// The Lean expression for the 2-arg host builder in `Obligation` shape
    /// (`add → sub → HostTbl`); straight-line ignores `sub`.
    fn host_expr(&self) -> String {
        match self {
            Cert::StraightLine { name, .. } => format!("fun add _ => CertModule.{name}Host add"),
            Cert::Recursive { name, .. } | Cert::AccumulatorRecursive { name, .. } => {
                format!("CertModule.{name}Host")
            }
            Cert::AdtConstructor { name, .. } | Cert::FieldProjection { name, .. } => {
                format!("fun _ _ => CertModule.{name}Host")
            }
            Cert::AdtMatch { name, .. } => format!("CertModule.{name}Host"),
        }
    }
    /// The source-level `Dom`/`Cod` type names recorded in the manifest JSON so
    /// `aver cert verify`/`explain` can surface WHAT is certified without reading
    /// Lean. Display-only (the semantic content is what the witness pins);
    /// rendered ASCII-safe.
    fn source_dom_cod(&self, model_info: &ModelInfo) -> (String, String) {
        let ascii = |s: &str| ascii_type_name(s);
        match self {
            Cert::StraightLine { .. }
            | Cert::Recursive { .. }
            | Cert::AccumulatorRecursive { .. } => ("List Int".to_string(), "Int".to_string()),
            Cert::FieldProjection { .. } => ("WVal x WVal".to_string(), "WVal".to_string()),
            Cert::AdtMatch { name, .. } => {
                let dom = model_info
                    .fns
                    .get(name)
                    .and_then(|s| s.params.first())
                    .map(|s| ascii(s))
                    .unwrap_or_else(|| "Op".to_string());
                (dom, "Int".to_string())
            }
            Cert::AdtConstructor { field_count, .. } => {
                if adt_constructor_uses_model(self, model_info) {
                    let cod = model_info
                        .fns
                        .get(self.name())
                        .map(|s| ascii(&s.ret))
                        .unwrap_or_else(|| "Unit".to_string());
                    ("Int".to_string(), cod)
                } else {
                    let dom = if *field_count == 1 {
                        "WVal".to_string()
                    } else {
                        "WVal x WVal".to_string()
                    };
                    (dom, "WVal".to_string())
                }
            }
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

/// Disassemble the emitted module and classify each user function.
pub fn analyze(wasm_bytes: &[u8]) -> Result<Analysis, String> {
    let (user_fns, box_idx, user_idx_set, carrier) = disassemble(wasm_bytes)?;

    let mut certs = Vec::new();
    let mut declined = Vec::new();
    for f in &user_fns {
        match classify(f, box_idx, carrier, &user_idx_set) {
            Ok(c) => certs.push(c),
            Err(reason) => declined.push((f.name.clone(), reason)),
        }
    }

    // Named runtime contracts actually consumed by the certified functions.
    let mut contracts = Vec::new();
    let mut has_box = false;
    let mut has_add = false;
    let mut has_sub = false;
    for c in &certs {
        match c {
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
            Cert::AdtConstructor { .. } | Cert::FieldProjection { .. } => {}
            Cert::AdtMatch { .. } => {
                has_box = true;
                has_sub = true;
            }
        }
    }
    if has_box {
        contracts.push("__rt_aint_from_i64 (box i64 -> carrier)".to_string());
    }
    if has_add {
        contracts.push(
            "Int.add (carrier add = exact integer addition on represented values)".to_string(),
        );
    }
    if has_sub {
        contracts.push(
            "Int.sub (carrier sub = exact integer subtraction on represented values)".to_string(),
        );
    }

    Ok(Analysis {
        certs,
        declined,
        carrier,
        contracts,
    })
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
        match c {
            Cert::StraightLine { .. }
            | Cert::Recursive { .. }
            | Cert::AccumulatorRecursive { .. } => ObligationFace::IntList { arity: c.arity() },
            Cert::FieldProjection { struct_idx, .. } => ObligationFace::Projection {
                struct_idx: *struct_idx,
            },
            Cert::AdtMatch { .. } => ObligationFace::AdtMatch,
            Cert::AdtConstructor { .. } => ObligationFace::AdtConstructor,
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
pub fn rederive_obligations(wasm_bytes: &[u8]) -> Result<Vec<RederivedObligation>, String> {
    let (user_fns, box_idx, user_idx_set, carrier) = disassemble(wasm_bytes)?;
    let mut out = Vec::new();
    for f in &user_fns {
        if let Ok(c) = classify(f, box_idx, carrier, &user_idx_set) {
            out.push(RederivedObligation {
                name: c.name().to_string(),
                code: render_code_value(&c),
                host: render_host_value(&c),
                self_idx: c.self_idx(),
                carrier: c.carrier(),
                face: ObligationFace::of_cert(&c),
            });
        }
    }
    Ok(out)
}

// ---- disassembly ---------------------------------------------------------

type DisasmResult = (
    Vec<UserFn>,
    u32,
    std::collections::HashSet<u32>,
    Option<u32>,
);

fn disassemble(wasm_bytes: &[u8]) -> Result<DisasmResult, String> {
    use wasmparser::{CompositeInnerType, Operator, Parser, Payload, StorageType, ValType};

    let mut num_imported_funcs: u32 = 0;
    // defined-function index -> declared type index
    let mut func_type_idx: Vec<u32> = Vec::new();
    // type index -> arity (param count) for func types
    let mut type_arity: std::collections::HashMap<u32, usize> = std::collections::HashMap::new();
    // type index -> struct field count
    let mut struct_field_counts: std::collections::HashMap<u32, u32> =
        std::collections::HashMap::new();
    // export name -> func index
    let mut exports: Vec<(String, u32)> = Vec::new();
    // per defined-function code entry: (nlocals, ops, calls, has_loop_or_branch)
    let mut code_entries: Vec<(usize, Vec<Op>, Vec<u32>, bool)> = Vec::new();
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
                                type_arity.insert(idx, ft.params().len());
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
                let mut opr = body
                    .get_operators_reader()
                    .map_err(|e| format!("ops reader: {e}"))?;
                while !opr.eof() {
                    let op = opr.read().map_err(|e| format!("op read: {e}"))?;
                    let mapped = match op {
                        Operator::LocalGet { local_index } => Op::LocalGet(local_index),
                        Operator::LocalSet { local_index } => Op::LocalSet(local_index),
                        Operator::I64Const { value } => Op::I64Const(value),
                        Operator::I32Const { value } => Op::I32Const(value),
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
                        Operator::RefIsNull => Op::RefIsNull,
                        Operator::I64LeS => Op::I64LeS,
                        Operator::I64GeS => Op::I64GeS,
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
                code_entries.push((nlocals, ops, calls, has_loop_or_branch));
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

    let mut user_fns = Vec::new();
    for (name, wasm_idx) in user_exports {
        let Some(def_idx) = wasm_idx.checked_sub(num_imported_funcs) else {
            continue;
        };
        let Some((nlocals, ops, calls, has_loop_or_branch)) =
            code_entries.get(def_idx as usize).cloned()
        else {
            continue;
        };
        let arity = func_type_idx
            .get(def_idx as usize)
            .and_then(|ti| type_arity.get(ti))
            .copied()
            .unwrap_or(0);
        user_fns.push(UserFn {
            name,
            wasm_idx,
            arity,
            nlocals,
            ops,
            calls,
            has_loop_or_branch,
        });
    }

    Ok((user_fns, box_idx, user_idx_set, carrier))
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
) -> Result<Cert, String> {
    // Strip a trailing `End` (function end) for the straight-line match.
    let ops: &[Op] = match f.ops.last() {
        Some(Op::End) => &f.ops[..f.ops.len() - 1],
        _ => &f.ops,
    };

    // Straight-line add-constant: [localGet 0, i64Const k, call box, call add].
    if let [Op::LocalGet(0), Op::I64Const(k), Op::Call(b), Op::Call(a)] = ops
        && *b == box_idx
        && *a != f.wasm_idx
        && !user_idx_set.contains(a)
        && f.arity == 1
    {
        let carrier =
            carrier.ok_or_else(|| "carrier struct type not found in module".to_string())?;
        return Ok(Cert::StraightLine {
            name: f.name.clone(),
            self_idx: f.wasm_idx,
            nlocals: f.nlocals,
            carrier,
            k: *k,
            box_idx,
            add_idx: *a,
        });
    }

    // sumTo-shape self-recursion.
    if let Some(cert) = match_recursive(f, box_idx) {
        return Ok(cert);
    }

    // countDown-shape accumulator self-recursion.
    if let Some(cert) = match_accumulator_recursive(f, box_idx) {
        return Ok(cert);
    }

    if let Some(cert) = match_adt_constructor(f, box_idx, carrier) {
        return Ok(cert);
    }

    if let Some(cert) = match_field_projection(f, carrier) {
        return Ok(cert);
    }

    if let Some(cert) = match_adt_match(f, box_idx, carrier) {
        return Ok(cert);
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

fn match_adt_constructor(f: &UserFn, box_idx: u32, carrier: Option<u32>) -> Option<Cert> {
    use Op::*;
    let ops = strip_trailing_end(&f.ops);
    let (last, prefix) = ops.split_last()?;
    let StructNew(struct_idx, field_count) = last else {
        return None;
    };
    // The renderer models a 1-field constructor over a user inductive with its
    // real model type, and a 1- or 2-field constructor over any other codomain
    // as a verbatim pack (dual of the field projection). A wider struct has no
    // faithful rendering in this class, so it declines (fail-closed).
    if *struct_idx == carrier? || *field_count as usize != f.arity || f.arity == 0 || f.arity > 2 {
        return None;
    }
    for (i, op) in prefix.iter().enumerate() {
        if *op != LocalGet(i as u32) {
            return None;
        }
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
        ops: ops.to_vec(),
    })
}

fn match_field_projection(f: &UserFn, carrier: Option<u32>) -> Option<Cert> {
    use Op::*;
    let ops = strip_trailing_end(&f.ops);
    let [LocalGet(0), StructGet(struct_idx, field_idx)] = ops else {
        return None;
    };
    if f.arity != 1 || *struct_idx == carrier? || !f.calls.is_empty() {
        return None;
    }
    Some(Cert::FieldProjection {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        struct_idx: *struct_idx,
        field_idx: *field_idx,
        ops: ops.to_vec(),
    })
}

fn match_adt_match(f: &UserFn, box_idx: u32, carrier: Option<u32>) -> Option<Cert> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = strip_trailing_end(&f.ops);
    let [
        LocalGet(0),
        LocalSet(scrut0),
        LocalGet(scrut1),
        RefTest(add_ty),
        If,
        LocalGet(scrut2),
        RefCast(add_cast),
        StructGet(add_get, 0),
        LocalSet(add_local),
        LocalGet(add_local2),
        Else,
        LocalGet(scrut3),
        RefTest(neg_ty),
        If,
        LocalGet(scrut4),
        RefCast(neg_cast),
        StructGet(neg_get, 0),
        LocalSet(neg_local),
        I64Const(0),
        Call(box_call0),
        LocalGet(neg_local2),
        Call(sub_idx),
        Else,
        I64Const(0),
        Call(box_call1),
        End,
        End,
    ] = ops
    else {
        return None;
    };
    if scrut0 != scrut1
        || scrut0 != scrut2
        || scrut0 != scrut3
        || scrut0 != scrut4
        || add_ty != add_cast
        || add_ty != add_get
        || neg_ty != neg_cast
        || neg_ty != neg_get
        || add_local != add_local2
        || neg_local != neg_local2
        || *box_call0 != box_idx
        || *box_call1 != box_idx
        || *sub_idx == f.wasm_idx
    {
        return None;
    }
    let zero_variant_idx = neg_ty.checked_add(1)?;
    Some(Cert::AdtMatch {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier: carrier?,
        add_variant_idx: *add_ty,
        neg_variant_idx: *neg_ty,
        zero_variant_idx,
        box_idx,
        sub_idx: *sub_idx,
        ops: ops.to_vec(),
    })
}

fn strip_trailing_end(ops: &[Op]) -> &[Op] {
    match ops.last() {
        Some(Op::End) => &ops[..ops.len() - 1],
        _ => ops,
    }
}

/// Match the exact sumTo operator template, extracting `carrier` (from the
/// `struct.get` type index), `sub`, `add`, and confirming `self`/`box`.
fn match_recursive(f: &UserFn, box_idx: u32) -> Option<Cert> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = &f.ops;
    let carrier = match ops.get(3) {
        Some(StructGet(c, 1)) => *c,
        _ => return None,
    };
    let l = match (ops.first(), ops.get(1)) {
        (Some(LocalGet(0)), Some(LocalSet(l))) => *l,
        _ => return None,
    };
    let expected_prefix = [
        LocalGet(0),
        LocalSet(l),
        LocalGet(l),
        StructGet(carrier, 1),
        RefIsNull,
        If,
        LocalGet(l),
        StructGet(carrier, 0),
        I64Const(0),
        I64LeS,
        Else,
        LocalGet(l),
        StructGet(carrier, 2),
        I32Const(0),
        I32LtS,
        End,
        If,
        I64Const(0),
        Call(box_idx),
        Else,
    ];
    if ops.len() < expected_prefix.len() {
        return None;
    }
    if ops[..expected_prefix.len()] != expected_prefix[..] {
        return None;
    }
    // recursion tail: localGet 0, localGet 0, i64Const 1, call box, call SUB,
    //                 call SELF, call ADD, End, End
    let tail = &ops[expected_prefix.len()..];
    let (b2, sub_idx, self_call, add_idx) = match tail {
        [
            LocalGet(0),
            LocalGet(0),
            I64Const(1),
            Call(b2),
            Call(sub),
            Call(sc),
            Call(add),
            End,
            End,
        ] => (*b2, *sub, *sc, *add),
        _ => return None,
    };
    if b2 != box_idx || self_call != f.wasm_idx {
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
    })
}

/// Match the exact countDown accumulator-recursion operator template,
/// extracting `carrier` (from the `struct.get` type index), `sub`, `add`, and
/// confirming `self`/`box`.
fn match_accumulator_recursive(f: &UserFn, box_idx: u32) -> Option<Cert> {
    use Op::*;
    if f.arity != 2 {
        return None;
    }
    let ops = &f.ops;
    let carrier = match ops.get(3) {
        Some(StructGet(c, 1)) => *c,
        _ => return None,
    };
    let l = match (ops.first(), ops.get(1)) {
        (Some(LocalGet(0)), Some(LocalSet(l))) => *l,
        _ => return None,
    };
    let expected_prefix = [
        LocalGet(0),
        LocalSet(l),
        LocalGet(l),
        StructGet(carrier, 1),
        RefIsNull,
        If,
        LocalGet(l),
        StructGet(carrier, 0),
        I64Const(0),
        I64LeS,
        Else,
        LocalGet(l),
        StructGet(carrier, 2),
        I32Const(0),
        I32LtS,
        End,
        If,
        LocalGet(1),
        Else,
    ];
    if ops.len() < expected_prefix.len() {
        return None;
    }
    if ops[..expected_prefix.len()] != expected_prefix[..] {
        return None;
    }
    let tail = &ops[expected_prefix.len()..];
    let (sub_idx, add_idx, self_call) = match tail {
        [
            LocalGet(0),
            I64Const(1),
            Call(b),
            Call(sub),
            LocalGet(1),
            LocalGet(0),
            Call(add),
            ReturnCall(sc),
            End,
            End,
        ] if *b == box_idx => (*sub, *add, *sc),
        _ => return None,
    };
    if self_call != f.wasm_idx {
        return None;
    }
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

// ---- model evaluation (anti-vacuity guard values) ------------------------

fn eval_sumto(n: i64) -> i64 {
    if n <= 0 { 0 } else { n + eval_sumto(n - 1) }
}

fn eval_countdown(n: i64, acc: i64) -> i64 {
    if n <= 0 {
        acc
    } else {
        eval_countdown(n - 1, acc + n)
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
    match c {
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
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add + sub contracts). -/\n\
             def {name}Host (add sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  \
             else if fn = {sub_idx} then some (2, sub)\n  else none\n",
        ),
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
        Cert::AdtConstructor { name, .. } | Cert::FieldProjection { name, .. } => format!(
            "/-- Runtime host wiring for `{name}` (no host calls). -/\n\
             def {name}Host : HostTbl := fun _ => none\n",
        ),
        Cert::AdtMatch {
            name,
            carrier,
            box_idx,
            sub_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + sub contracts). -/\n\
             def {name}Host (_add sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {sub_idx} then some (2, sub)\n  else none\n",
        ),
    }
}

fn render_code_def(c: &Cert) -> String {
    let doc = match c {
        Cert::StraightLine { .. } => "straight-line add-constant",
        Cert::Recursive { .. } => "self-recursive",
        Cert::AccumulatorRecursive { .. } => "accumulator self-recursive",
        Cert::AdtConstructor { .. } => "ADT constructor",
        Cert::FieldProjection { .. } => "field projection",
        Cert::AdtMatch { .. } => "ADT variant match",
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
fn render_code_value(c: &Cert) -> String {
    match c {
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
            ..
        } => format!(
            "fun fn =>\n  \
             if fn = {self_idx} then some ⟨1, {nlocals},\n    \
             [ .localGet 0, .localSet 1,\n      \
             .localGet 1, .structGet {carrier} 1, .refIsNull,\n      \
             .ifElse [.localGet 1, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
             [.localGet 1, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
             .ifElse [.i64Const 0, .call {box_idx}]\n              \
             [.localGet 0, .localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, \
             .call {self_idx}, .call {add_idx}] ]⟩\n  else none",
        ),
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
        | Cert::AdtMatch {
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
    }
}

/// The `Obligation.host` builder VALUE for a certified body, FULLY EXPANDED to
/// the box/add/sub wiring on the byte-derived indices — deliberately NOT a
/// reference to `CertModule.{name}Host` (which an attacker edits). The checker
/// splices this and pins it with `rfl` against `manifest.obligations.map
/// (·.host)`, so a nerfed host (e.g. `fun _ _ _ => none`, which would make
/// `holds` vacuous even with an honest `code`) fails the kernel witness.
/// Definitionally equal to the honest `render_host_def` builder.
fn render_host_value(c: &Cert) -> String {
    match c {
        Cert::StraightLine {
            carrier,
            box_idx,
            add_idx,
            ..
        } => format!(
            "fun add _ => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    else none",
        ),
        Cert::Recursive {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "fun add sub => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
        Cert::AccumulatorRecursive {
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "fun add sub => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {add_idx} then some (2, add)\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
        Cert::AdtConstructor { .. } | Cert::FieldProjection { .. } => {
            "fun _ _ => fun _ => none".to_string()
        }
        Cert::AdtMatch {
            carrier,
            box_idx,
            sub_idx,
            ..
        } => format!(
            "fun _ sub => fun fn =>\n    \
             if fn = {box_idx} then some (1, boxRef {carrier})\n    \
             else if fn = {sub_idx} then some (2, sub)\n    else none",
        ),
    }
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
        Op::RefTest(t) => format!(".refTest {t}"),
        Op::RefCast(t) => format!(".refCast {t}"),
        Op::StructNew(t, n) => format!(".structNew {t} {n}"),
        Op::StructGet(t, f) => format!(".structGet {t} {f}"),
        Op::RefIsNull => ".refIsNull".to_string(),
        Op::I64LeS => ".i64LeS".to_string(),
        Op::I64GeS => ".i64GeS".to_string(),
        Op::I32LtS => ".i32LtS".to_string(),
        Op::I32GtS => ".i32GtS".to_string(),
        Op::Call(f) => format!(".call {f}"),
        Op::ReturnCall(f) => format!(".returnCall {f}"),
        Op::If | Op::Else | Op::End | Op::Other => return None,
    })
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
        match c {
            Cert::StraightLine { .. } => s.push_str(&render_straightline_cert(c)),
            Cert::Recursive { .. } => s.push_str(&render_recursive_cert(c)),
            Cert::AccumulatorRecursive { .. } => s.push_str(&render_accumulator_recursive_cert(c)),
            Cert::AdtConstructor { .. } => s.push_str(&render_adt_constructor_cert(c, model_info)),
            Cert::FieldProjection { .. } => s.push_str(&render_field_projection_cert(c)),
            Cert::AdtMatch { .. } => s.push_str(&render_adt_match_cert(c, model_info)),
        }
        s.push('\n');
    }
    s.push_str("end CertProofs\n");
    s
}

fn render_straightline_cert(c: &Cert) -> String {
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
  intro S add sub hadd hsub fuel ns vs w hrepr hrun
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

fn render_recursive_cert(c: &Cert) -> String {
    let Cert::Recursive {
        name,
        self_idx,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let g3 = eval_sumto(3);
    let _ = (box_idx, add_idx, sub_idx);
    format!(
        r#"/-! ### {name} — self-recursive certificate (carrier type {carrier}) -/

-- model-side fuel bridge (the cap-induction pattern at R = 1).
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

theorem {name}_step (n : Int) (hn : ¬ n ≤ 0) : {name} n = n + {name} (n - 1) := by
  have h0 : {name} n = {name}__fuel (n.natAbs + 1) n := rfl
  rw [h0]
  simp only [{name}__fuel]
  rw [if_neg hn, {name}_fuel_stable n.natAbs (n - 1) (by omega)]

theorem {name}_base (n : Int) (hn : n ≤ 0) : {name} n = 0 := by
  have h0 : {name} n = {name}__fuel (n.natAbs + 1) n := rfl
  rw [h0]; simp [{name}__fuel, hn]

/-- THE CERTIFICATE THEOREM: partial correctness of the VERBATIM emitted
    recursive body against the generated model, for ALL n : ℤ. -/
theorem {name}_wasm_certified
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [v] = some w →
      Repr ({name} n) w := by
  intro fuel
  induction fuel with
  | zero =>
      intro n v w hv hrun
      simp [wFuncN] at hrun
  | succ fuel ih =>
      intro n v w hv hrun
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rw [{name}_base s hle, ← hrun]
          exact hsmall_intro 0
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .null, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({name} (s - 1)) vr := ih (s - 1) vd vr hrd hrec
              rcases hadd : add [.structv {carrier} [.i64v s, .null, .i32v sg], vr] with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step s hle, ← hrun]
                exact hAdd s ({name} (s - 1)) _ _ wa hv hrr hadd
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rw [{name}_base n hn0, ← hrun]
          exact hsmall_intro 0
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
            rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({name} (n - 1)) vr := ih (n - 1) vd vr hrd hrec
              rcases hadd : add [.structv {carrier} [.i64v s, .arr lty les, .i32v sg], vr] with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step n hn0, ← hrun]
                exact hAdd n ({name} (n - 1)) _ _ wa hv hrr hadd

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the model
    value `{name} n` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [v] = some w →
      ∃ m : Int, Repr m w ∧ m = {name} n :=
  fun fuel n v w hv hrun =>
    ⟨{name} n,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig add sub hAdd hSub fuel n v w hv hrun,
     rfl⟩

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 0]).bind carrierToInt)
      = some 0 := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4)]).bind carrierToInt)
      = some 0 := by native_decide

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem): the emitted recursive body simulates the model `{name}`. -/
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel ns vs w hrepr hrun
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
            add sub hadd hsub fuel n v w hv hrun
      | cons _ _ =>
          simp at harity
"#
    )
}

fn render_accumulator_recursive_cert(c: &Cert) -> String {
    let Cert::AccumulatorRecursive {
        name,
        self_idx,
        carrier,
        ..
    } = c
    else {
        unreachable!()
    };
    let g3 = eval_countdown(3, 0);
    let g4 = eval_countdown(3, 4);
    format!(
        r#"/-! ### {name} — accumulator self-recursive certificate (carrier type {carrier}) -/

-- model-side fuel bridge (fuel induction; the IH is quantified over both args).
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
  rw [h0]; simp [{name}__fuel, hn]

/-- THE CERTIFICATE THEOREM: partial correctness of the VERBATIM emitted
    accumulator-recursive body against the generated model, for ALL n acc : ℤ. -/
theorem {name}_wasm_certified
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal), Repr n vn → Repr acc vacc →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vn, vacc] = some w →
      Repr ({name} n acc) w := by
  intro fuel
  induction fuel with
  | zero =>
      intro n acc vn vacc w hvn hvacc hrun
      simp [wFuncN] at hrun
  | succ fuel ih =>
      intro n acc vn vacc w hvn hvacc hrun
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
                exact ih (n - 1) (acc + n) vd va vr hrd hra hrec

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the model
    value `{name} n acc` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (n acc : Int) (vn vacc w : WVal), Repr n vn → Repr acc vacc →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vn, vacc] = some w →
      ∃ m : Int, Repr m w ∧ m = {name} n acc :=
  fun fuel n acc vn vacc w hvn hvacc hrun =>
    ⟨{name} n acc,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig add sub hAdd hSub fuel n acc vn
       vacc w hvn hvacc hrun,
     rfl⟩

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3, carrierSmall {carrier} 0]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3, carrierSmall {carrier} 4]).bind carrierToInt)
      = some ({g4}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4), carrierSmall {carrier} 9]).bind carrierToInt)
      = some 9 := by native_decide

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem): the emitted accumulator-recursive body simulates the model `{name}`. -/
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel ns vs w hrepr hrun
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
              simp at harity
"#
    )
}

fn render_adt_constructor_cert(c: &Cert, model_info: &ModelInfo) -> String {
    let Cert::AdtConstructor {
        name,
        self_idx,
        carrier,
        struct_idx,
        field_count,
        ..
    } = c
    else {
        unreachable!()
    };
    if !adt_constructor_uses_model(c, model_info) {
        return render_verbatim_constructor_cert(
            name,
            *self_idx,
            *carrier,
            *struct_idx,
            *field_count,
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
  intro S add sub hadd hsub fuel n vs w hrepr hrun
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

/// Verbatim pack constructor certificate (dual of the field projection): the
/// body wraps its arguments into variant `struct_idx`. The obligation carries
/// `Cod := WVal` with `verbatimRepr`, so the proof mirrors the field
/// projection's — pop the raw arguments, reduce the interpreter, `rfl`.
fn render_verbatim_constructor_cert(
    name: &str,
    self_idx: u32,
    carrier: u32,
    struct_idx: u32,
    field_count: u32,
) -> String {
    // Binders + input list + built struct for one vs two fields.
    let (binders, input, built, intro, split) = if field_count == 1 {
        ("(a : WVal)", "[a]", "[a]", "intro a", "")
    } else {
        (
            "(a b : WVal)",
            "[a, b]",
            "[a, b]",
            "intro a b",
            "  rcases p with ⟨a, b⟩\n",
        )
    };
    // Concrete forcing input: pack carrier value(s) and decode field 0 back.
    let concrete = if field_count == 1 {
        format!("[carrierSmall {carrier} 7]")
    } else {
        format!("[carrierSmall {carrier} 7, carrierSmall {carrier} 9]")
    };
    format!(
        r#"/-! ### {name} — verbatim constructor certificate -/

theorem {name}_wasm_certified (host : HostTbl) :
    ∀ {binders}, wFuncN {name}Code host 1 {self_idx} {input} = some (.structv {struct_idx} {built}) := by
  {intro}
  simp [wFuncN, {name}Code, wRunF, popArgs, initLocals]

#print axioms {name}_wasm_certified

-- Executable tripwire: pack concrete carriers and decode field 0 back to
-- `some 7`. A trapping body yields `none`, so this forces a real struct build.
example :
    ((wFuncN {name}Code {name}Host 1 {self_idx} {concrete}).bind
      (fun r => match r with
        | .structv _ (f :: _) => carrierToInt f
        | _ => none))
      = some 7 := by native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel p vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
{split}  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      simp [wFuncN, wRunF, {name}Code, popArgs, initLocals] at hrun
      subst hrun
      rfl
"#
    )
}

fn render_field_projection_cert(c: &Cert) -> String {
    let Cert::FieldProjection {
        name,
        self_idx,
        carrier,
        struct_idx,
        field_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let expected = if *field_idx == 0 { "a" } else { "b" };
    // Concrete forcing input: project field `field_idx` from a two-carrier
    // struct and decode it back. Field 0 carries 7, field 1 carries 9.
    let forced = if *field_idx == 0 { 7 } else { 9 };
    format!(
        r#"/-! ### {name} — field projection certificate -/

theorem {name}_wasm_certified (host : HostTbl) :
    ∀ (a b : WVal), wFuncN {name}Code host 1 {self_idx} [.structv {struct_idx} [a, b]] = some {expected} := by
  intro a b
  simp [wFuncN, {name}Code, wRunF, popArgs, initLocals]

#print axioms {name}_wasm_certified

-- Executable tripwire: project the field from a concrete two-carrier struct and
-- decode it back. A trapping body yields `none`, so this forces a real read.
example :
    ((wFuncN {name}Code {name}Host 1 {self_idx}
        [.structv {struct_idx} [carrierSmall {carrier} 7, carrierSmall {carrier} 9]]).bind carrierToInt)
      = some {forced} := by native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel p vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  rcases p with ⟨a, b⟩
  subst hrepr
  cases fuel with
  | zero => simp [wFuncN] at hrun
  | succ f =>
      simp [wFuncN, wRunF, {name}Code, popArgs, initLocals] at hrun
      subst hrun
      rfl
"#
    )
}

fn render_adt_match_cert(c: &Cert, model_info: &ModelInfo) -> String {
    let Cert::AdtMatch {
        name,
        self_idx,
        carrier,
        box_idx: _,
        sub_idx: _,
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
    format!(
        r#"/-! ### {name} — ADT match certificate (carrier type {carrier}) -/

theorem {name}_wasm_certified
    (S : CarrierSpec {carrier})
    (sub : List WVal → Option WVal)
    (hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w → S.Repr (a - b) w) :
    ∀ (fuel : Nat) (o : {ty}) (v w : WVal), {repr} S o v →
      wFuncN {name}Code ({name}Host (fun _ => none) sub) fuel {self_idx} [v] = some w →
      S.Repr ({name} o) w := by
  intro fuel
  cases fuel with
  | zero => intro o v w hv hrun; simp [wFuncN] at hrun
  | succ f =>
      intro o v w hv hrun
      cases o with
      | add x =>
          obtain ⟨cx, rfl, hcx⟩ := hv
          simp [wFuncN, wRunF, {name}Code, {name}Host, b32, popArgs, initLocals] at hrun
          subst hrun
          simpa [{name}] using hcx
      | neg x =>
          obtain ⟨cx, rfl, hcx⟩ := hv
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32, popArgs, initLocals] at hrun
          rcases hs : sub [carrierSmall {carrier} 0, cx] with _ | w' <;> simp [hs] at hrun
          subst hrun
          have := hsub 0 x (carrierSmall {carrier} 0) cx w' (S.smallIntro 0) hcx hs
          simpa [{name}] using this
      | zero =>
          subst hv
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32, popArgs, initLocals] at hrun
          subst hrun
          simpa [{name}] using S.smallIntro 0

#print axioms {name}_wasm_certified

def {host_ref} : HostTbl := {name}Host (fun _ => none) (subRef {carrier})
example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv 0 [carrierSmall {carrier} 7]]).bind carrierToInt)
    = some 7 := by native_decide
example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv 1 [carrierSmall {carrier} 3]]).bind carrierToInt)
    = some (-3) := by native_decide
example : ((wFuncN {name}Code {host_ref} 4 {self_idx} [.structv 2 []]).bind carrierToInt)
    = some 0 := by native_decide

theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel o vs w hrepr hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  obtain ⟨v, rfl, hv⟩ := hrepr
  simpa [AverCert.Schema.intRepr] using {name}_wasm_certified S sub hsub fuel o v w hv hrun
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
    out
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
    let Cert::AdtConstructor {
        name, field_count, ..
    } = c
    else {
        return false;
    };
    *field_count == 1
        && model_info
            .fns
            .get(name)
            .map(|s| model_info.inductives.contains_key(&s.ret))
            .unwrap_or(false)
}

/// `(Dom type, `vs`-shape, struct-field list)` for a verbatim pack constructor
/// of the given field count. The domain is the raw argument `WVal`s (a single
/// value or a pair), and the model packs them into the variant struct verbatim.
fn verbatim_ctor_shape(field_count: u32) -> (&'static str, &'static str, &'static str) {
    if field_count == 1 {
        ("WVal", "[p]", "[p]")
    } else {
        ("WVal × WVal", "[p.1, p.2]", "[p.1, p.2]")
    }
}

fn adt_repr_indices(c: &Cert, model_info: &ModelInfo) -> Option<(String, Vec<u32>)> {
    match c {
        Cert::AdtMatch {
            name,
            add_variant_idx,
            neg_variant_idx,
            zero_variant_idx,
            ..
        } => {
            let ty = model_info.fns.get(name)?.params.first()?.clone();
            Some((
                ty,
                vec![*add_variant_idx, *neg_variant_idx, *zero_variant_idx],
            ))
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

fn render_obligation_def(c: &Cert, model_info: &ModelInfo) -> String {
    let name = c.name();
    match c {
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
            field_count,
            ..
        } => {
            // Verbatim pack certificate (dual of the field projection): the body
            // wraps its `field_count` arguments into variant `struct_idx`. No
            // claim about a recursive model representation — `Cod := WVal` and
            // `verbatimRepr` pin the output to the constructed struct byte-for-byte.
            let (dom, pat, args) = verbatim_ctor_shape(*field_count);
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
        Cert::AdtMatch { .. } => {
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
        let kind = match c {
            Cert::StraightLine { .. } => "straight-line",
            Cert::Recursive { .. } => "self-recursive",
            Cert::AccumulatorRecursive { .. } => "multi-argument self-recursive",
            Cert::AdtConstructor { .. } => "adt-constructor",
            Cert::FieldProjection { .. } => "field-projection",
            Cert::AdtMatch { .. } => "adt-match",
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
