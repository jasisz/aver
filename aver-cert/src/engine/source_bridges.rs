// Included from engine/mod.rs (engine feature) — see the include! list there.

// The plan-equals-source bridge: the kernel-checked identification of the
// plan a record projection-compute obligation evaluates with the transpiled
// source function the model modules and the law-claims speak about.
//
// Before this surface the two halves of a certificate met only "by
// construction": `Holds` says the bytes simulate `recordComputeModel
// Plans.<fn>Plan.body`, while `Laws.lean` and `aver proof` speak about
// `<Module>.<fn>`. Nothing in the kernel said those are the same function.
// One theorem per bridged export now does, at exactly the encoders the face's
// representation relation uses.

/// One declared plan-equals-source bridge, as the manifest transports it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceBridge {
    /// The certified export this bridge is about (`Domain_Rational_plus`).
    pub export: String,
    /// Fully qualified name of the package's bridge theorem.
    pub theorem: String,
    /// The bridge theorem's universal statement, on one line.
    pub statement: String,
    /// Fully qualified name of the package's corollary, which conjoins the
    /// bridge with the artifact-level `Holds` fact.
    pub corollary: String,
    /// Fully qualified Lean name of the source function the bridge identifies
    /// the plan with. Reported by `explain`; never on the verdict line.
    pub model: String,
}

/// Lean namespace every bridge theorem, corollary and helper lemma lives in.
pub const BRIDGE_NAMESPACE: &str = "AverCert.Bridge";

/// The suffix the corollary name carries over the export name.
pub const BRIDGE_COROLLARY_SUFFIX: &str = "_certified";

impl SourceBridge {
    /// The theorem name a bridge for `export` must declare.
    pub fn theorem_name(export: &str) -> String {
        format!("{BRIDGE_NAMESPACE}.{export}")
    }

    /// The corollary name a bridge for `export` must declare.
    pub fn corollary_name(export: &str) -> String {
        format!("{BRIDGE_NAMESPACE}.{export}{BRIDGE_COROLLARY_SUFFIX}")
    }
}

/// How one source value of the face's admitted shapes is encoded as the wall's
/// `RecordComputeBridge.SVal`. These are the ONLY three shapes the v1
/// projection-compute face carries; anything else (a nested record, a record
/// with a non-Int field, a Float or String leaf) has no `SVal` image and gets
/// no bridge rather than an invented encoding.
#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceEncoding {
    Int,
    Bool,
    /// A record whose fields are all Ints, as `(qualified type, accessors in
    /// declaration order)`.
    Record(String, Vec<String>),
}

impl SourceEncoding {
    /// `SVal` term for the source value `value` (already a Lean term).
    fn encode(&self, value: &str) -> String {
        match self {
            SourceEncoding::Int => format!("_root_.RecordComputeBridge.SVal.i ({value})"),
            SourceEncoding::Bool => format!("_root_.RecordComputeBridge.SVal.b ({value})"),
            SourceEncoding::Record(ty, fields) => {
                let leaves = fields
                    .iter()
                    .map(|field| format!("_root_.{ty}.{field} ({value})"))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("_root_.RecordComputeBridge.SVal.r [{leaves}]")
            }
        }
    }

    /// The source Lean type this encoding is for.
    fn lean_type(&self) -> String {
        match self {
            SourceEncoding::Int => "Int".to_string(),
            SourceEncoding::Bool => "Bool".to_string(),
            SourceEncoding::Record(ty, _) => format!("_root_.{ty}"),
        }
    }

    /// Whether this encoding is the source image of the plan-level type the
    /// byte-checked face carries at the same position.
    fn matches(&self, ty: FragTy) -> bool {
        matches!(
            (self, ty),
            (SourceEncoding::Int, FragTy::IntCarrier)
                | (SourceEncoding::Bool, FragTy::BoolI32)
                | (SourceEncoding::Record(_, _), FragTy::AdtRef)
        )
    }
}

/// Everything the renderer needs for one bridged export. The manifest-facing
/// subset is [`SourceBridge`]; the rest is rendering data derived from the
/// same analysis and never transported.
struct BridgePlan {
    bridge: SourceBridge,
    /// Index of this export's obligation in `manifest.obligations`, which is
    /// how the corollary reaches `<name>Ob.holds` out of `HoldsCore`.
    obligation_index: usize,
    /// Parameter encodings in declaration order.
    params: Vec<SourceEncoding>,
    /// Result encoding.
    result: SourceEncoding,
    /// Qualified Lean name of the source function.
    model: String,
}

impl BridgePlan {
    fn binder_names(&self) -> Vec<String> {
        (0..self.params.len()).map(|i| format!("x{i}")).collect()
    }

    /// `[enc x0, enc x1, …]` — the encoded argument list.
    fn encoded_args(&self) -> String {
        let encoded = self
            .params
            .iter()
            .enumerate()
            .map(|(index, encoding)| encoding.encode(&format!("x{index}")))
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{encoded}]")
    }

    /// `<Module>.<fn> x0 x1` — the source call.
    fn source_call(&self) -> String {
        let args = self.binder_names().join(" ");
        if args.is_empty() {
            format!("_root_.{}", self.model)
        } else {
            format!("_root_.{} {args}", self.model)
        }
    }
}

/// The bridge statement for a plan: the plan's model evaluated at the encoded
/// arguments is the encoded source result. Fully `_root_.`-qualified so the
/// text means the same in the package's `Bridge.lean` and in the
/// checker-authored pin, whatever namespaces the package declares.
fn bridge_statement(plan: &BridgePlan, plan_def: &str) -> String {
    let binders = plan
        .params
        .iter()
        .enumerate()
        .map(|(index, encoding)| format!("(x{index} : {})", encoding.lean_type()))
        .collect::<Vec<_>>()
        .join(" ");
    let quantifier = if binders.is_empty() {
        String::new()
    } else {
        format!("∀ {binders}, ")
    };
    format!(
        "{quantifier}_root_.AverCert.StandardFace.recordComputeModel \
         _root_.AverCert.Plans.{plan_def}.body {} = \
         _root_.Option.some ({})",
        plan.encoded_args(),
        plan.result.encode(&plan.source_call()),
    )
}

/// Mirror of the checker's `validate_source_bridge_candidate`, kept as a
/// DEFENSIVE gate for the same reason the law surface keeps one: a single
/// entry the checker would hard-reject fails candidate parsing for the WHOLE
/// package before Lean runs, so a bridge the gates refuse is simply not
/// declared. Declining a bridge is fail-closed — the export stays certified,
/// its model just stays the plan.
fn bridge_survives_checker_gates(bridge: &SourceBridge) -> bool {
    let plain_dotted = |value: &str| {
        !value.is_empty()
            && value.len() <= MAX_BRIDGE_NAME_LEN
            && value.split('.').all(|segment| {
                let mut chars = segment.chars();
                matches!(chars.next(), Some(first) if first.is_ascii_alphabetic() || first == '_')
                    && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
            })
    };
    if !plain_dotted(&bridge.export)
        || bridge.export.contains('.')
        || !plain_dotted(&bridge.theorem)
        || !plain_dotted(&bridge.corollary)
        || !plain_dotted(&bridge.model)
    {
        return false;
    }
    if bridge.theorem != SourceBridge::theorem_name(&bridge.export)
        || bridge.corollary != SourceBridge::corollary_name(&bridge.export)
    {
        return false;
    }
    statement_is_single_plain_line(&bridge.statement, MAX_BRIDGE_STATEMENT_LEN)
        && statement_is_root_qualified(&bridge.statement)
}

/// Mirror of the checker's root-qualification gate: every dotted name in a
/// bridge statement must be spelled `_root_.`-first, because the checker's pin
/// elaborates the statement at the root namespace with no `open`.
fn statement_is_root_qualified(statement: &str) -> bool {
    statement
        .split(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '\''))
        .filter(|token| token.contains('.'))
        .all(|token| token.starts_with("_root_."))
}

/// Longest name and statement the bridge surface transports. The statement cap
/// is the law surface's, for the same reason: it bounds what the anti-injection
/// gate has to police in one `example` type.
pub const MAX_BRIDGE_NAME_LEN: usize = 200;
pub const MAX_BRIDGE_STATEMENT_LEN: usize = 2000;

/// The statement gate both the producer and the checker apply: one plain
/// term-position line, with balanced delimiters so it cannot escape the single
/// `(...)` the pin wraps it in.
pub fn statement_is_single_plain_line(statement: &str, max_len: usize) -> bool {
    if statement.is_empty()
        || statement.len() > max_len
        || statement.chars().any(char::is_control)
        || statement.contains(":=")
        || statement.contains("--")
        || statement.contains("/-")
    {
        return false;
    }
    let mut depth: Vec<char> = Vec::new();
    for character in statement.chars() {
        let matched = match character {
            '(' | '[' | '{' | '⟨' => {
                depth.push(character);
                true
            }
            ')' => depth.pop() == Some('('),
            ']' => depth.pop() == Some('['),
            '}' => depth.pop() == Some('{'),
            '⟩' => depth.pop() == Some('⟨'),
            _ => true,
        };
        if !matched {
            return false;
        }
    }
    depth.is_empty()
}

/// The source encoding a written model type denotes, or `None` when the type
/// has no `SVal` image in the v1 face.
fn source_encoding(
    model_info: &ModelInfo,
    prefix: &str,
    written: &str,
) -> Option<SourceEncoding> {
    match written {
        "Int" => Some(SourceEncoding::Int),
        "Bool" => Some(SourceEncoding::Bool),
        _ => {
            let (qualified, info) = model_info.resolve_structure(prefix, written)?;
            // Only an all-Int record has an `SVal.r` image: `takeInts` pops
            // boxed integers, so a Bool or nested field is unrepresentable
            // there. Refuse rather than invent an encoding.
            if info.fields.is_empty() || info.fields.iter().any(|(_, ty)| ty != "Int") {
                return None;
            }
            Some(SourceEncoding::Record(
                qualified,
                info.fields.iter().map(|(name, _)| name.clone()).collect(),
            ))
        }
    }
}

/// Derive one export's bridge plan, or the reason it gets none.
///
/// Every step is a cross-check between the SOURCE signature the model modules
/// declare and the byte-checked plan the face already pinned: same arity, same
/// per-position kind (record / Int carrier / Bool), and — for a record — the
/// same number of Int leaves as the byte-derived record declaration. A
/// disagreement means the producer would be guessing at the encoders, so it
/// declares no bridge.
fn bridge_plan_for(
    c: &Cert,
    obligation_index: usize,
    model_info: &ModelInfo,
) -> Result<BridgePlan, String> {
    if c.record_compute_face().is_none() {
        return Err("export is not certified through the record projection-compute face".into());
    }
    let Cert::ExprFragment { plan, .. } = c.inner() else {
        return Err("export carries no expression-fragment plan".into());
    };
    let export = c.name().to_string();
    let Some(sig) = model_info.model_fn_sig(&export) else {
        return Err("no unambiguous transpiled model signature for this export".into());
    };
    if sig.params.len() != plan.params.len() {
        return Err(format!(
            "source arity {} does not match the plan's {}",
            sig.params.len(),
            plan.params.len()
        ));
    }
    let mut params = Vec::with_capacity(sig.params.len());
    for (index, written) in sig.params.iter().enumerate() {
        let Some(encoding) = source_encoding(model_info, &sig.prefix, written) else {
            return Err(format!(
                "parameter {index} has source type `{written}`, which has no source-value \
                 encoding in this face (only Int, Bool and all-Int records do)"
            ));
        };
        if !encoding.matches(plan.params[index]) {
            return Err(format!(
                "parameter {index} source type `{written}` does not match the plan's type"
            ));
        }
        params.push(encoding);
    }
    let Some(result) = source_encoding(model_info, &sig.prefix, &sig.ret) else {
        return Err(format!(
            "result type `{}` has no source-value encoding in this face \
             (only Int, Bool and all-Int records do)",
            sig.ret
        ));
    };
    if !result.matches(plan.result) {
        return Err(format!(
            "result source type `{}` does not match the plan's type",
            sig.ret
        ));
    }
    // A record encoding lists exactly the leaves the byte-derived record
    // declaration carries, all of them Int carriers. This is the one place the
    // source-side field list meets the byte side.
    let record_arity = params
        .iter()
        .chain(std::iter::once(&result))
        .filter_map(|encoding| match encoding {
            SourceEncoding::Record(_, fields) => Some(fields.len()),
            _ => None,
        })
        .collect::<Vec<_>>();
    if !record_arity.is_empty() {
        let Some((_, leaves)) = c.record_decl() else {
            return Err("the plan names a record but carries no record declaration".into());
        };
        if leaves.iter().any(|leaf| *leaf != RecordLeaf::IntCarrier) {
            return Err("the record declaration carries a non-Int leaf".into());
        }
        if record_arity.iter().any(|count| *count != leaves.len()) {
            return Err(format!(
                "source record field count does not match the {} byte-derived leaves",
                leaves.len()
            ));
        }
    }
    let mut bridge_plan = BridgePlan {
        bridge: SourceBridge {
            theorem: SourceBridge::theorem_name(&export),
            corollary: SourceBridge::corollary_name(&export),
            statement: String::new(),
            model: sig.lean_name.clone(),
            export: export.clone(),
        },
        obligation_index,
        params,
        result,
        model: sig.lean_name.clone(),
    };
    bridge_plan.bridge.statement = bridge_statement(&bridge_plan, &format!("{export}Plan"));
    if !bridge_survives_checker_gates(&bridge_plan.bridge) {
        return Err(
            "statement or identifiers would be refused by the checker's source-bridge gates".into(),
        );
    }
    Ok(bridge_plan)
}

/// Plan one bridge per record projection-compute export, in obligation order.
/// The second component names every export that got no bridge and why, so the
/// producer can say what it declined instead of losing it silently.
fn plan_source_bridges(
    analysis: &Analysis,
    model_info: &ModelInfo,
) -> (Vec<BridgePlan>, Vec<(String, String)>) {
    let mut planned = Vec::new();
    let mut declined = Vec::new();
    for (index, c) in analysis.certs.iter().enumerate() {
        if c.record_compute_face().is_none() {
            continue;
        }
        match bridge_plan_for(c, index, model_info) {
            Ok(plan) => planned.push(plan),
            Err(reason) => declined.push((c.name().to_string(), reason)),
        }
    }
    (planned, declined)
}

/// The eight named host contracts `Obligation.holds` threads, spelled at the
/// obligation's own carrier specification. The composed corollary restates
/// them because it restates `holds` itself with the model replaced.
const BRIDGE_HOST_CONTRACTS: &str = concat!(
    "      (∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = _root_.Option.some w → S.Repr (a + b) w ∧ S.Canon w) →\n",
    "      (∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = _root_.Option.some w → S.Repr (a - b) w ∧ S.Canon w) →\n",
    "      (∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = _root_.Option.some w → S.Repr (a * b) w ∧ S.Canon w) →\n",
    "      (∀ a b w, stringEq [a, b] = _root_.Option.some w → w = _root_.CertPrelude.b32 (_root_.CertPrelude.stringEqW a b)) →\n",
    "      (∀ resultTy parts c, stringConcat resultTy [parts] = _root_.Option.some c → _root_.CertPrelude.stringConcatW resultTy parts = _root_.Option.some c) →\n",
    "      (∀ n v r, S.Repr n v → toIndex [v] = _root_.Option.some r → r = _root_.CertPrelude.WVal.i32v (_root_.CertPrelude.toIndexW n)) →\n",
    "      (∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb → cmp [va, vb] = _root_.Option.some r → r = _root_.CertPrelude.WVal.i32v (_root_.CertPrelude.cmpW a b)) →\n",
    "      (∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb → eq [va, vb] = _root_.Option.some r → r = _root_.CertPrelude.WVal.i32v (_root_.CertPrelude.eqW a b)) →\n",
);

/// The named hypotheses the corollary's proof re-applies, in `holds` order.
const BRIDGE_HOST_BINDERS: &str =
    "S add sub mul stringEq stringConcat toIndex cmp eq hAdd hSub hMul hStringEq \
     hStringConcat hToIndex hCmp hEq";

/// `manifest.obligations` membership for the obligation at `index`, as an
/// explicit `List.Mem` term. `decide` is not available here (an `Obligation`
/// carries `Type` fields), and the index is exactly what the renderer knows.
fn obligation_membership_term(index: usize) -> String {
    let mut term = "_root_.List.Mem.head _".to_string();
    for _ in 0..index {
        term = format!("_root_.List.Mem.tail _ ({term})");
    }
    term
}

/// Render the package's `Bridge.lean`.
///
/// Per bridged export it emits three declarations:
///
/// * `AverCert.Bridge.<export>` — the bridge itself, the plan's model at the
///   encoded arguments equals the encoded source result. Its proof is a FIXED
///   tactic script (the producer cannot run Lean): definitional `rfl` closes
///   every arithmetic and inline-sign-template shape, and the three `cmpW`
///   lemmas below close the host-comparison shapes. A shape no alternative
///   closes falls through to `sorry`, which costs that bridge its credit and
///   nothing else.
/// * `AverCert.Bridge.<export>_sourceModel` — the reader-facing composition:
///   the obligation restated with the plan model replaced by the source
///   function, conjoined with `Holds`.
/// * `AverCert.Bridge.<export>_certified` — the corollary the manifest names
///   and the checker's pin cites. Its `Holds` conjunct is taken from
///   `_sourceModel`, so the checker's axiom audit of the pin walks the composed
///   corollary too instead of leaving it an unaudited theorem beside the claim.
fn render_bridge_lean(plans: &[BridgePlan]) -> String {
    let mut s = String::new();
    s.push_str(
        "-- Plan-equals-source bridges of this certificate. Each bridge identifies\n\
         -- the plan an export's obligation evaluates with the transpiled source\n\
         -- function the model modules and the law-claims speak about, at exactly\n\
         -- the encoders the face's representation relation uses. Every name is\n\
         -- `_root_`-qualified so the statement means the same here and in the\n\
         -- checker-authored pin.\n\
         import Manifest\n\
         import Final\n\n\
         set_option autoImplicit false\n\n\
         set_option maxRecDepth 200000\n\n\
         /-- `__aint_cmp`'s three-way verdict decides the source strict order. -/\n\
         theorem AverCert.Bridge.cmpLtDecide (a b : Int) :\n    \
           decide (_root_.CertPrelude.cmpW a b < 0) = decide (a < b) := by\n  \
           simp [_root_.AverCert.StandardFace.cmpW_lt_iff]\n\n\
         theorem AverCert.Bridge.cmpGtDecide (a b : Int) :\n    \
           decide (_root_.CertPrelude.cmpW a b > 0) = decide (b < a) := by\n  \
           simp [_root_.AverCert.StandardFace.cmpW_gt_iff]\n\n\
         theorem AverCert.Bridge.cmpGeDecide (a b : Int) :\n    \
           decide (_root_.CertPrelude.cmpW a b ≥ 0) = decide (b ≤ a) := by\n  \
           simp [_root_.AverCert.StandardFace.cmpW_ge_iff]\n\n",
    );
    for plan in plans {
        let export = &plan.bridge.export;
        let binders = plan.binder_names().join(" ");
        let intro = if binders.is_empty() {
            String::new()
        } else {
            format!("  intro {binders}\n")
        };
        let bridge_at = if binders.is_empty() {
            format!("_root_.AverCert.Bridge.{export}")
        } else {
            format!("_root_.AverCert.Bridge.{export} {binders}")
        };
        let source_binders = plan
            .params
            .iter()
            .enumerate()
            .map(|(index, encoding)| format!("(x{index} : {}) ", encoding.lean_type()))
            .collect::<String>();
        let applied_binders = if binders.is_empty() {
            String::new()
        } else {
            format!("{binders} ")
        };
        // Concatenated rather than interpolated wherever a statement is
        // involved: a source type or accessor carrying `{`/`}` must stay inert
        // text, exactly as in `render_laws_lean`.
        s.push_str("/-- plan-equals-source bridge for `");
        s.push_str(export);
        s.push_str("`: the plan this export's obligation evaluates IS `");
        s.push_str(&plan.model);
        s.push_str("`. -/\ntheorem _root_.AverCert.Bridge.");
        s.push_str(export);
        s.push_str(" :\n    ");
        s.push_str(&plan.bridge.statement);
        s.push_str(" := by\n");
        s.push_str(&intro);
        s.push_str(
            "  first\n    \
             | rfl\n    \
             | exact congrArg (fun v => _root_.Option.some (_root_.RecordComputeBridge.SVal.b v))\n        \
                 (_root_.AverCert.Bridge.cmpLtDecide _ _)\n    \
             | exact congrArg (fun v => _root_.Option.some (_root_.RecordComputeBridge.SVal.b v))\n        \
                 (_root_.AverCert.Bridge.cmpGtDecide _ _)\n    \
             | exact congrArg (fun v => _root_.Option.some (_root_.RecordComputeBridge.SVal.b v))\n        \
                 (_root_.AverCert.Bridge.cmpGeDecide _ _)\n    \
             | sorry\n\n",
        );
        s.push_str("/-- `");
        s.push_str(export);
        s.push_str("`'s obligation with the plan model replaced by `");
        s.push_str(&plan.model);
        s.push_str(
            "`:\n    the emitted body, run on a represented argument under the named host\n    \
             contracts, yields a represented result of the SOURCE function. -/\n\
             theorem _root_.AverCert.Bridge.",
        );
        s.push_str(export);
        s.push_str("_sourceModel :\n    (∀ (S : _root_.AverCert.Schema.CarrierSpec _root_.AverCert.");
        s.push_str(export);
        s.push_str(
            "Ob.carrier)\n       \
             (add sub mul stringEq : List _root_.CertPrelude.WVal → Option _root_.CertPrelude.WVal)\n       \
             (stringConcat : Nat → List _root_.CertPrelude.WVal → Option _root_.CertPrelude.WVal)\n       \
             (toIndex cmp eq : List _root_.CertPrelude.WVal → Option _root_.CertPrelude.WVal),\n",
        );
        s.push_str(BRIDGE_HOST_CONTRACTS);
        s.push_str("      ∀ (fuel : Nat) ");
        s.push_str(&source_binders);
        s.push_str("(vs : List _root_.CertPrelude.WVal) (w : _root_.CertPrelude.WVal),\n        _root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob.domRepr S ");
        s.push_str(&plan.encoded_args());
        s.push_str(" vs →\n        _root_.CertPrelude.wFuncN _root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob.code (_root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob.host add sub mul stringEq stringConcat toIndex cmp eq) fuel _root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob.self vs = _root_.Option.some w →\n        _root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob.codRepr S (_root_.Option.some (");
        s.push_str(&plan.result.encode(&plan.source_call()));
        s.push_str(")) w)\n    ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) := by\n  \
                    refine ⟨?_, _root_.AverCert.Final.cert⟩\n  intro ");
        s.push_str(BRIDGE_HOST_BINDERS);
        s.push_str(" fuel ");
        s.push_str(&applied_binders);
        s.push_str("vs w hDom hRun\n  have hHolds : _root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob.holds :=\n    _root_.AverCert.Final.cert.2.2.2 _root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob (");
        s.push_str(&obligation_membership_term(plan.obligation_index));
        s.push_str(")\n  have hSim := hHolds ");
        s.push_str(BRIDGE_HOST_BINDERS);
        s.push_str(" fuel ");
        s.push_str(&plan.encoded_args());
        s.push_str(" vs w hDom hRun\n  simpa only [_root_.AverCert.");
        s.push_str(export);
        s.push_str("Ob, ");
        s.push_str(&bridge_at);
        s.push_str("] using hSim\n\n");
        s.push_str("/-- The claim the manifest names: the bridge conjoined with the\n    \
                    artifact-level `Holds` fact, so one kernel-checked name ties the\n    \
                    plan-equals-source identity to exactly the certified bytes. -/\n\
                    theorem _root_.AverCert.Bridge.");
        s.push_str(export);
        s.push_str(BRIDGE_COROLLARY_SUFFIX);
        s.push_str(" :\n    (");
        s.push_str(&plan.bridge.statement);
        s.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  ⟨_root_.AverCert.Bridge.");
        s.push_str(export);
        s.push_str(", (_root_.AverCert.Bridge.");
        s.push_str(export);
        s.push_str("_sourceModel).2⟩\n\n#print axioms _root_.AverCert.Bridge.");
        s.push_str(export);
        s.push_str(BRIDGE_COROLLARY_SUFFIX);
        s.push_str("\n\n");
    }
    s
}

/// Every model function a law statement mentions, in first-appearance order.
///
/// The scan is textual over the statement the emitter itself wrote: a token is
/// a maximal run of Lean identifier characters, and it counts when it is the
/// qualified name of a def the model modules declare. Over-recognition is
/// harmless (an extra TRUE conjunct); under-recognition only costs the law its
/// bridge upgrade, so the direction of failure is fail-closed for the claim.
fn law_statement_model_fns(statement: &str, model_info: &ModelInfo) -> Vec<String> {
    let mut found: Vec<String> = Vec::new();
    for token in statement.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '.')) {
        let token = token.trim_matches('.');
        if token.is_empty() || !model_info.is_model_fn(token) {
            continue;
        }
        if !found.iter().any(|seen| seen == token) {
            found.push(token.to_string());
        }
    }
    found
}

/// The bridges that cover every model function a law mentions, or `None` when
/// some mentioned function has no bridge. `Some(vec![])` means the law
/// mentions no model function at all and is left exactly as it was.
fn law_bridge_coverage(
    statement: &str,
    model_info: &ModelInfo,
    bridges: &[SourceBridge],
) -> Option<Vec<usize>> {
    let mut covering = Vec::new();
    for model in law_statement_model_fns(statement, model_info) {
        let index = bridges.iter().position(|bridge| bridge.model == model)?;
        if !covering.contains(&index) {
            covering.push(index);
        }
    }
    Some(covering)
}
