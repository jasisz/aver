// ---- rendering the schema-9 package -----------------------------------------

/// Target artifact whose delivered bytes are bound by a certificate package.
///
/// The certificate engine always analyzes and renders facts about a core Wasm
/// module. For raw wasm-gc the delivered artifact and that core are the same
/// bytes. For wasip2 they are deliberately distinct: the component is hashed
/// as the delivered artifact, while the wall consumes the exact core-module
/// payload declared inside that component.
pub enum CertificateArtifact<'a> {
    WasmGc {
        file_name: &'a str,
        module_bytes: &'a [u8],
    },
    Wasip2 {
        file_name: &'a str,
        component_bytes: &'a [u8],
        embedded_core_module: &'a [u8],
        envelope: crate::format::Wasip2ComponentEnvelopeDeclaration,
    },
}

impl CertificateArtifact<'_> {
    fn validate(&self) -> Result<(), String> {
        match self {
            Self::WasmGc { module_bytes, .. } => {
                if module_bytes.is_empty() {
                    return Err("cannot certify an empty wasm-gc module".to_string());
                }
            }
            Self::Wasip2 {
                component_bytes,
                embedded_core_module,
                envelope,
                ..
            } => {
                let (_, declared_core, _) = envelope.split_component(component_bytes).ok_or_else(|| {
                    "wasip2 certificate envelope does not split the delivered component by its declared lengths"
                        .to_string()
                })?;
                if declared_core != *embedded_core_module {
                    return Err(
                        "wasip2 certificate core bytes do not equal the envelope-declared component slice"
                            .to_string(),
                    );
                }
            }
        }
        Ok(())
    }

    pub fn file_name(&self) -> &str {
        match self {
            Self::WasmGc { file_name, .. } | Self::Wasip2 { file_name, .. } => file_name,
        }
    }

    fn delivered_bytes(&self) -> &[u8] {
        match self {
            Self::WasmGc { module_bytes, .. } => module_bytes,
            Self::Wasip2 {
                component_bytes, ..
            } => component_bytes,
        }
    }

    pub fn core_module_bytes(&self) -> &[u8] {
        match self {
            Self::WasmGc { module_bytes, .. } => module_bytes,
            Self::Wasip2 {
                embedded_core_module,
                ..
            } => embedded_core_module,
        }
    }

    pub fn target(&self) -> &'static str {
        match self {
            Self::WasmGc { .. } => crate::format::TARGET_WASM_GC,
            Self::Wasip2 { .. } => crate::format::TARGET_WASIP2,
        }
    }

    fn abi(&self) -> &'static str {
        match self {
            Self::WasmGc { .. } => crate::format::RUNTIME_ABI_WASM_GC,
            Self::Wasip2 { .. } => crate::format::RUNTIME_ABI_WASIP2,
        }
    }

    fn wasip2_component_envelope(
        &self,
    ) -> Option<crate::format::Wasip2ComponentEnvelopeDeclaration> {
        match self {
            Self::WasmGc { .. } => None,
            Self::Wasip2 { envelope, .. } => Some(*envelope),
        }
    }
}

/// What the producer refused to declare, by surface. Each entry is
/// `(name, reason)`; neither kind changes which exports are certified.
#[derive(Debug, Default)]
pub struct ProjectDeclines {
    pub law_claims: Vec<(String, String)>,
    pub source_bridges: Vec<(String, String)>,
}

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
            c if (c as u32) < 0x20 => out.push_str(&format!("\\u{:04x}", c as u32)),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn write(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    let path = dir.join(name);
    std::fs::write(&path, content).map_err(|e| format!("write {}: {e}", path.display()))
}

/// A String as the Lean list of its characters. A String literal is
/// definitionally `String.ofList` of exactly this list, which the kernel
/// checks without building the String's bytes. Printable ASCII other than
/// the quote and escape characters stays a plain literal (so the checker's
/// lexical gate sees no string opener); every other character is spelled by
/// its code point.
fn lean_char_list(s: &str) -> String {
    let chars = s
        .chars()
        .map(|c| {
            if (' '..='~').contains(&c) && !matches!(c, '\'' | '\\' | '"') {
                format!("'{c}'")
            } else {
                format!("(Char.ofNat {})", c as u32)
            }
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{chars}]")
}

/// A list of Strings as the Lean list of their character lists, one per
/// line after `separator`.
fn lean_char_lists(items: &[String], separator: &str) -> String {
    let lists = items
        .iter()
        .map(|item| lean_char_list(item))
        .collect::<Vec<_>>()
        .join(&format!(",{separator}"));
    format!("[{lists}]")
}

fn plan_def_name(func_idx: u32) -> String {
    format!("fn{func_idx}")
}

fn render_plans(analysis: &Analysis) -> String {
    let mut s = String::from(
        "-- The certificate's plans: every planned function's optimized MIR body,\n\
         -- printed 1:1 into the one plan grammar, and the declared type layout.\n\
         -- Producer data: the wall lowers each plan and pins the result to the\n\
         -- function's code entry, and confirms the layout against the bytes.\n\
         import SchemaCore\n\n\
         namespace AverCert.Plans\n\
         open AverCert.Schema AverCert.Grammar\n\n",
    );
    s.push_str(&format!(
        "def types : TypeTable :=\n  {}\n\n",
        analysis.types.lean()
    ));
    for e in &analysis.entries {
        s.push_str(&format!(
            "/-- `{}` (function {}). -/\ndef {} : FnPlan :=\n  {}\n\n",
            e.name.replace('-', "_"),
            e.func_idx,
            plan_def_name(e.func_idx),
            e.plan.lean()
        ));
    }
    let entries = analysis
        .entries
        .iter()
        .map(|e| {
            format!(
                "⟨{}, {}, {}, {}, {}⟩",
                lean_str(&e.name),
                e.exported,
                e.func_idx,
                e.group,
                plan_def_name(e.func_idx)
            )
        })
        .collect::<Vec<_>>()
        .join(",\n   ");
    s.push_str(&format!("def fnPlans : List FnEntry :=\n  [{entries}]\n\n"));
    s.push_str("end AverCert.Plans\n");
    s
}

fn lean_pairs(items: &[(String, String)]) -> String {
    format!(
        "[{}]",
        items
            .iter()
            .map(|(a, b)| format!("({}, {})", lean_str(a), lean_str(b)))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn lean_strings(items: &[String]) -> String {
    format!(
        "[{}]",
        items
            .iter()
            .map(|x| lean_str(x))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn declared_uncertified(analysis: &Analysis) -> Vec<(String, String)> {
    analysis
        .module_envelope
        .declared_uncertified(analysis.certified_names(), &analysis.declined)
}

fn render_manifest_lean(analysis: &Analysis, sha: &str, target: &str, abi: &str) -> String {
    let roles = match &analysis.roles {
        Some(r) => format!("some {}", r.roles_lean_value()),
        None => "(none : Option CertDecode.AddSub.Roles)".to_string(),
    };
    let params = match analysis
        .roles
        .as_ref()
        .and_then(|r| r.arith_params_record_lean(analysis.carrier))
    {
        Some(p) => format!("some {p}"),
        None => "(none : Option ArithTemplateDerisk.ArithHostParams)".to_string(),
    };
    let strings = format!(
        "[{}]",
        analysis
            .string_roles
            .iter()
            .map(|(idx, role)| format!("({idx}, {})", role.lean_value()))
            .collect::<Vec<_>>()
            .join(", ")
    );
    let start = match analysis.module_envelope.start {
        Some(i) => format!("some {i}"),
        None => "none".to_string(),
    };
    format!(
        "-- The certificate's manifest: the subject (artifact identity, exports,\n\
         -- helper indices and the contracts it is conditional on), the plans and\n\
         -- the obligations, which are exactly the ones the wall derives.\n\
         import SchemaCore\n\
         import AcceptedArtifactCore\n\
         import Plans\n\n\
         namespace AverCert\n\
         open AverCert.Schema\n\n\
         def subject : Subject :=\n  \
           {{ artifactHash := {sha}\n    \
             target := {target}, profile := {profile}, abi := {abi}\n    \
             artifactRoot := {root}\n    \
             exports := {exports}\n    \
             declaredUncertified := {declared}\n    \
             capabilities := {capabilities}\n    \
             start := {start}\n    \
             hostRoleTable := {roles}\n    \
             arithParams := {params}\n    \
             stringHostRoles := {strings}\n    \
             contracts := {contracts} }}\n\n\
         def manifest : Manifest :=\n  \
           {{ subject := subject, types := Plans.types, fnPlans := Plans.fnPlans,\n    \
             obligations := AverCert.AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans }}\n\n\
         end AverCert\n",
        sha = lean_str(sha),
        target = lean_str(target),
        profile = lean_str(PROFILE_ID),
        abi = lean_str(abi),
        root = lean_str(ARTIFACT_CERTIFICATE_ROOT),
        exports = lean_strings(&analysis.certified_names()),
        declared = lean_pairs(&declared_uncertified(analysis)),
        capabilities = lean_pairs(&analysis.module_envelope.capabilities),
        contracts = lean_strings(&analysis.contracts),
    )
}

fn render_artifact_host_roles(analysis: &Analysis, params: &str) -> String {
    let roles = analysis.roles.expect("a carriered module declares roles");
    let leaf = |name: &str, idx: Option<u32>| {
        let idx = idx.map_or_else(|| "none".to_string(), |idx| format!("(some {idx})"));
        format!(
            "theorem decodedHostRole_{name} : AverCert.AcceptedArtifact.arithRoleCheck \
             AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen \
             ArithTemplateDerisk.ArithRole.{name} {idx} {params} = true := by decide +kernel"
        )
    };
    let leaves = [
        leaf("box", roles.box_idx),
        leaf("toIndex", roles.to_index_idx),
        leaf("add", roles.add_idx),
        leaf("sub", roles.sub_idx),
        leaf("mul", roles.mul_idx),
        leaf("cmp", roles.cmp_idx),
        leaf("eq", roles.eq_idx),
        leaf("divmod", roles.divmod_idx),
    ]
    .join("\n\n");
    format!(
        "-- Per-role helper template pins, each in its own `decide +kernel`\n\
         -- declaration, in a separate compilation unit.\n\
         import AcceptedArtifact\n\
         import ArtifactBytes\n\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.Artifact\n\n\
         {leaves}\n\n\
         end AverCert.Artifact\n"
    )
}

/// Plans checked per kernel declaration in `ArtifactPlans.lean`.
const PLAN_CHUNK: usize = 32;

/// The per-entry plan checks (`entryAccepted`), `PLAN_CHUNK` entries per
/// `decide +kernel` declaration, in their own compilation unit: one check
/// over every plan of a large module exhausts the kernel's memory, and every
/// declaration decodes the module's sections again, so the chunk trades the
/// two (on btc-listener's 173 plans: 8 per chunk took 227 seconds, 32 took
/// 124 seconds at a 7.4 GiB peak). The chunks are chained from the last one
/// back to the whole list.
fn render_artifact_plans(analysis: &Analysis) -> Vec<(String, String)> {
    let n = analysis.entries.len();
    let header = "set_option maxRecDepth 200000\n\
         set_option maxHeartbeats 1600000\n\n\
         namespace AverCert.Artifact\n\
         open AverCert AverCert.Schema AverCert.AcceptedArtifact AverCert.TypeTable\n\n";
    let plan_ok = "/-- One plan's acceptance check against the staged artifact bytes. -/\n\
         noncomputable abbrev planOk : FnEntry → Bool :=\n  \
           entryAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen\n    \
           (mctxOf AverCert.manifest.subject AverCert.manifest.types AverCert.manifest.fnPlans)\n    \
           AverCert.manifest.fnPlans\n\n";
    let starts: Vec<usize> = (0..n.max(1)).step_by(PLAN_CHUNK).collect();
    let last = *starts.last().expect("at least one chunk");
    let from_last = format!(
        "theorem plans_from_{last} : (AverCert.manifest.fnPlans.drop {last}).all planOk = true := by\n  \
         decide +kernel\n\n"
    );
    let chunk = |k: usize| {
        format!(
            "theorem plans_chunk_{k} :\n    \
             ((AverCert.manifest.fnPlans.drop {k}).take {PLAN_CHUNK}).all planOk = true := by\n  \
             decide +kernel\n\n"
        )
    };
    let chain = |k: usize, next: usize| {
        format!(
            "theorem plans_from_{k} : (AverCert.manifest.fnPlans.drop {k}).all planOk = true := by\n  \
             rw [← List.take_append_drop {PLAN_CHUNK} (AverCert.manifest.fnPlans.drop {k}),\n    \
             List.all_append, plans_chunk_{k}, List.drop_drop]\n  \
             simpa only [Nat.reduceAdd, Bool.true_and] using plans_from_{next}\n\n"
        )
    };
    let imports = "import AcceptedArtifact\nimport ArtifactBytes\nimport Manifest\n\n";
    let mut body = String::new();
    let mut chained = String::new();
    for (i, k) in starts.iter().enumerate().rev().skip(1) {
        body.push_str(&chunk(*k));
        chained.push_str(&chain(*k, starts[i + 1]));
    }
    let end = "theorem plans_all : AverCert.manifest.fnPlans.all planOk = true := plans_from_0\n\n\
         end AverCert.Artifact\n";
    if !splits_artifact_modules(analysis) {
        return vec![(
            "ArtifactPlans.lean".to_string(),
            format!(
                "-- The per-plan acceptance checks, a few plans per `decide +kernel`\n\
                 -- declaration, chained into the check over every plan.\n\
                 {imports}{header}{plan_ok}{from_last}{body}{chained}{end}"
            ),
        )];
    }
    // One module per chunk, so a parallel Lake checks the chunks at once.
    let mut files = vec![(
        "ArtifactPlanCheck.lean".to_string(),
        format!(
            "-- The per-plan acceptance check the chunk modules decide.\n\
             {imports}{header}{plan_ok}end AverCert.Artifact\n"
        ),
    )];
    let mut chunk_imports = String::new();
    for k in &starts {
        let theorem = if *k == last { from_last.clone() } else { chunk(*k) };
        files.push((
            format!("ArtifactPlans{k}.lean"),
            format!(
                "-- One chunk of the per-plan acceptance checks.\n\
                 import ArtifactPlanCheck\n\n\
                 {header}{theorem}end AverCert.Artifact\n"
            ),
        ));
        chunk_imports.push_str(&format!("import ArtifactPlans{k}\n"));
    }
    files.push((
        "ArtifactPlans.lean".to_string(),
        format!(
            "-- The per-plan acceptance checks of the chunk modules, chained into\n\
             -- the check over every plan.\n\
             {chunk_imports}\n{header}{chained}{end}"
        ),
    ));
    files
}

/// Whether the package spreads its byte facts over several modules: a module
/// with more plans than one chunk checks the chunks, and the heaviest whole-
/// module facts, in modules of their own, which Lake builds in parallel when
/// it is given more than one worker (`AVER_CERT_BUILD_JOBS`). A smaller
/// package keeps one module per step, since each module pays its imports.
fn splits_artifact_modules(analysis: &Analysis) -> bool {
    analysis.entries.len() > PLAN_CHUNK
}

const ARTIFACT_HEADER: &str = "set_option maxRecDepth 200000\n\
     -- Elaboration cost grows with the artifact; this moves a resource\n\
     -- limit only (no axiom, no hypothesis, nothing the kernel accepts).\n\
     set_option maxHeartbeats 1600000\n\n\
     namespace AverCert.Artifact\n\
     open AverCert AverCert.Schema AverCert.AcceptedArtifact\n\n";

/// `Artifact.lean` (and, for a split package, the modules it imports): the
/// artifact data and the byte facts of its acceptance.
fn render_artifact(
    analysis: &Analysis,
    envelope: Option<crate::format::Wasip2ComponentEnvelopeDeclaration>,
) -> Vec<(String, String)> {
    let envelope = match envelope {
        None => "none".to_string(),
        Some(env) => format!(
            "some {{ prefixLen := {}, embeddedCoreModuleLen := {}, suffixLen := {} }}",
            env.prefix_len, env.embedded_core_module_len, env.suffix_len
        ),
    };
    let closure = &analysis.module_envelope.closure;
    let nats = |xs: &[u32]| {
        format!(
            "[{}]",
            xs.iter().map(u32::to_string).collect::<Vec<_>>().join(", ")
        )
    };
    let (roles_import, roles_proof) = match (
        &analysis.roles,
        analysis
            .roles
            .as_ref()
            .and_then(|r| r.arith_params_record_lean(analysis.carrier)),
    ) {
        (Some(r), Some(params)) => (
            "import ArtifactHostRoles\n".to_string(),
            format!(
                "theorem roles_ok : decodedHostRoleTable data := by\n  \
                 dsimp only [decodedHostRoleTable, data]\n  \
                 rw [show AverCert.manifest.subject.hostRoleTable = some {} from rfl,\n      \
                 show AverCert.manifest.subject.arithParams = some {params} from rfl]\n  \
                 simp only [arithTableCheck, decodedHostRole_box, decodedHostRole_toIndex, \
                 decodedHostRole_add, decodedHostRole_sub, decodedHostRole_mul, decodedHostRole_cmp, \
                 decodedHostRole_eq, decodedHostRole_divmod, Bool.and_true, Bool.true_and]\n  \
                 decide +kernel",
                r.roles_lean_value()
            ),
        ),
        _ => (
            String::new(),
            "theorem roles_ok : decodedHostRoleTable data := by\n  \
             unfold decodedHostRoleTable; decide +kernel"
                .to_string(),
        ),
    };
    let data = format!(
        "noncomputable def data : ArtifactData :=\n  \
           {{ modBytes := AverCert.ArtifactBytes.modBytes, modLen := AverCert.ArtifactBytes.modLen,\n    \
             manifest := AverCert.manifest, wasip2ComponentEnvelope := {envelope},\n    \
             closureFuel := {fuel},\n    \
             closureClaim := ⟨{roots}, {helpers}, {admitted}⟩ }}\n\n",
        fuel = analysis.module_envelope.closure_fuel,
        roots = nats(&closure.roots),
        helpers = nats(&closure.helpers),
        admitted = nats(&closure.admitted),
    );
    let strings = "theorem strings_ok : decodedStringHostRoles data := by\n  \
         unfold decodedStringHostRoles; decide +kernel\n\n";
    let closure_ok =
        "theorem closure_ok : closureIsolation data = true := by decide +kernel\n\n";
    let exports = format!(
        "theorem framing_ok : CertDecode.moduleFramingValid data.modBytes data.modLen = true := by\n  \
           decide +kernel\n\n\
         theorem exports_ok : exportsAccounted data = true :=\n  \
           exportsAccounted_of_chars data\n    \
           {obligation_names}\n    \
           {declared_names}\n    \
           rfl rfl (by decide +kernel)\n\n\
         theorem imports_ok : importsWithinCapabilities data = true := by decide +kernel\n\n\
         theorem start_ok : startAccounted data = true := by decide +kernel\n\n",
        obligation_names = lean_char_lists(
            &analysis
                .entries
                .iter()
                .filter(|e| e.exported)
                .map(|e| e.name.clone())
                .collect::<Vec<_>>(),
            "\n     "
        ),
        declared_names = lean_char_lists(
            &declared_uncertified(analysis)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            "\n     "
        ),
    );
    let rest = format!(
        "theorem plans_ok : plansAccepted data = true :=\n  \
           plansAccepted_of_parts data plans_all (by decide +kernel)\n\n\
         {roles_proof}\n\n\
         theorem axes_ok : AverCert.ClaimAxes.checked data = true := by decide +kernel\n\n"
    );
    let tail = "theorem whole_ok : acceptedWholeModule data :=\n  \
           ⟨framing_ok, exports_ok, imports_ok, start_ok, closure_ok⟩\n\n\
         theorem envelope_ok : artifactEnvelopeAccepted AverCert.ArtifactComponentBytes.componentBytes\n    \
           AverCert.ArtifactComponentBytes.componentLen data = true := by decide +kernel\n\n\
         end AverCert.Artifact\n";
    let base_imports = "import AcceptedArtifact\nimport ArtifactBytes\nimport Manifest\n";
    if !splits_artifact_modules(analysis) {
        return vec![(
            "Artifact.lean".to_string(),
            format!(
                "-- The artifact data and the byte facts of its acceptance, each by\n\
                 -- `decide +kernel` against the checker-staged `ArtifactBytes`.\n\
                 {base_imports}\
                 import ArtifactPlans\n\
                 {roles_import}\n\
                 {ARTIFACT_HEADER}\
                 {data}{rest}{strings}{exports}{closure_ok}{tail}"
            ),
        )];
    }
    let part = |comment: &str, body: &str| {
        format!(
            "-- {comment}\n\
             import ArtifactData\n\n\
             {ARTIFACT_HEADER}\
             {body}\
             end AverCert.Artifact\n"
        )
    };
    vec![
        (
            "ArtifactData.lean".to_string(),
            format!(
                "-- The artifact data the byte facts speak about.\n\
                 {base_imports}\n\
                 {ARTIFACT_HEADER}\
                 {data}\
                 end AverCert.Artifact\n"
            ),
        ),
        (
            "ArtifactStrings.lean".to_string(),
            part("The String helper roles, decoded from the module.", strings),
        ),
        (
            "ArtifactClosure.lean".to_string(),
            part("The certified closure's isolation.", closure_ok),
        ),
        (
            "ArtifactInterface.lean".to_string(),
            part("The module's framing, exports, imports and start function.", &exports),
        ),
        (
            "Artifact.lean".to_string(),
            format!(
                "-- The remaining byte facts of the artifact's acceptance, joined with\n\
                 -- the ones proved in the modules imported below.\n\
                 import ArtifactData\n\
                 import ArtifactStrings\n\
                 import ArtifactClosure\n\
                 import ArtifactInterface\n\
                 import ArtifactPlans\n\
                 {roles_import}\n\
                 {ARTIFACT_HEADER}\
                 {rest}{tail}"
            ),
        ),
    ]
}

fn render_final() -> String {
    format!(
        "import Artifact\nimport AcceptanceSoundness\n\n\
         open AverCert AverCert.Schema\n\n\
         /-- THE certificate theorem: every certified export's emitted function\n\
         simulates its plan's model, from the acceptance's byte facts. -/\n\
         {FINAL_STATEMENT_LINE} :=\n  \
         AcceptanceSoundness.accept_sound CertModule.wasmSha256 AverCert.Artifact.data\n    \
         rfl rfl rfl rfl AverCert.Artifact.plans_ok\n\n\
         #print axioms {FINAL_THEOREM}\n"
    )
}

fn render_artifact_certificate() -> String {
    "import Artifact\nimport Final\n\n\
     namespace AverCert.Artifact\n\n\
     theorem certificate : AverCert.AcceptedArtifact.accepted data :=\n  \
     ⟨AverCert.Final.cert, envelope_ok, rfl, rfl, plans_ok, roles_ok, strings_ok, axes_ok, whole_ok⟩\n\n\
     #print axioms AverCert.Artifact.certificate\n\n\
     end AverCert.Artifact\n"
        .to_string()
}

fn render_module(file_name: &str, sha: &str) -> String {
    format!(
        "-- The sha256 of the certified `{}` bytes.\n\
         import CertPrelude\n\n\
         namespace CertModule\n\n\
         def wasmSha256 : String := {}\n\n\
         end CertModule\n",
        file_name.replace('\n', " "),
        lean_str(sha)
    )
}

fn json_list<T>(items: &[T], render: impl Fn(&T) -> String) -> String {
    if items.is_empty() {
        return "[]".to_string();
    }
    format!(
        "[\n{}\n  ]",
        items
            .iter()
            .map(|x| format!("    {}", render(x)))
            .collect::<Vec<_>>()
            .join(",\n")
    )
}

fn render_manifest_json(
    analysis: &Analysis,
    artifact_file_name: &str,
    sha: &str,
    target: &str,
    abi: &str,
    envelope: Option<crate::format::Wasip2ComponentEnvelopeDeclaration>,
    laws: &[LawClaim],
    law_bridges: &[Vec<String>],
    bridges: &[SourceBridge],
    declined_bridges: &[(String, String)],
) -> String {
    let any_total = analysis.certified.iter().any(|c| c.total);
    let any_partial = analysis.certified.iter().any(|c| !c.total);
    let level = match (any_partial, any_total) {
        (true, true) => "mixed L1/L3",
        (false, true) => "L3",
        _ => CERT_LEVEL,
    };
    let mut s = String::from("{\n");
    s.push_str(&format!("  \"schema_version\": {CERT_SCHEMA_VERSION},\n"));
    s.push_str(&format!(
        "  \"format\": {{\"version\": {}, \"wall_id\": {}}},\n",
        wall::FORMAT_VERSION,
        json_str(wall::current_id())
    ));
    s.push_str(&format!("  \"wasm\": {},\n", json_str(artifact_file_name)));
    s.push_str(&format!("  \"wasm_sha256\": \"{sha}\",\n"));
    s.push_str(&format!("  \"target\": {},\n", json_str(target)));
    s.push_str(&format!("  \"level\": \"{level}\",\n"));
    s.push_str(&format!("  \"profile\": \"{PROFILE_ID}\",\n"));
    s.push_str(&format!("  \"abi\": {},\n", json_str(abi)));
    if let Some(envelope) = envelope {
        s.push_str(&format!(
            "  \"{}\": {{\"{}\": {}, \"{}\": {}, \"{}\": {}, \"{}\": {}}},\n",
            crate::format::WASIP2_COMPONENT_ENVELOPE_FIELD,
            crate::format::WASIP2_COMPONENT_ENVELOPE_KIND_FIELD,
            json_str(envelope.kind()),
            crate::format::WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD,
            envelope.prefix_len,
            crate::format::WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD,
            envelope.embedded_core_module_len,
            crate::format::WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD,
            envelope.suffix_len,
        ));
    }
    s.push_str(&format!("  \"final_theorem\": \"{FINAL_THEOREM}\",\n"));
    s.push_str(&format!(
        "  \"artifact_certificate_root\": \"{ARTIFACT_CERTIFICATE_ROOT}\",\n"
    ));
    match analysis.carrier {
        Some(c) => s.push_str(&format!("  \"carrier_type_index\": {c},\n")),
        None => s.push_str("  \"carrier_type_index\": null,\n"),
    }
    s.push_str(&format!(
        "  \"runtime_contracts\": {},\n",
        json_list(&analysis.contracts, |c| json_str(c))
    ));
    // A law cites the bridges only when its bridged corollary is declared,
    // which needs every cited bridge in the package.
    let bridge_exports: Vec<&str> = bridges.iter().map(|b| b.export.as_str()).collect();
    s.push_str(&format!(
        "  \"laws\": {},\n",
        json_list(
            &laws.iter().zip(law_bridges).collect::<Vec<_>>(),
            |(claim, cited)| {
                let cited: Vec<&String> = if cited.iter().all(|e| bridge_exports.contains(&e.as_str())) {
                    cited.iter().collect()
                } else {
                    Vec::new()
                };
                format!(
                    "{{\"label\": {}, \"theorem\": {}, \"statement\": {}, \"corollary\": {}, \"bridges\": [{}]}}",
                    json_str(&claim.label),
                    json_str(&claim.qualified()),
                    json_str(&claim.statement),
                    json_str(&claim.corollary()),
                    cited.iter().map(|e| json_str(e)).collect::<Vec<_>>().join(", ")
                )
            }
        )
    ));
    s.push_str(&format!(
        "  \"sourceBridges\": {},\n",
        json_list(bridges, SourceBridge::to_json)
    ));
    s.push_str(&format!(
        "  \"sourceBridgesDeclined\": {},\n",
        json_list(declined_bridges, |(e, r)| format!(
            "{{\"export\": {}, \"reason\": {}}}",
            json_str(e),
            json_str(r)
        ))
    ));
    s.push_str(&format!(
        "  \"declaredUncertified\": {},\n",
        json_list(&declared_uncertified(analysis), |(n, r)| format!(
            "{{\"name\": {}, \"reason\": {}}}",
            json_str(n),
            json_str(r)
        ))
    ));
    s.push_str(&format!(
        "  \"capabilities\": {},\n",
        json_list(&analysis.module_envelope.capabilities, |(m, n)| format!(
            "{{\"module\": {}, \"name\": {}}}",
            json_str(m),
            json_str(n)
        ))
    ));
    match analysis.module_envelope.start {
        Some(i) => s.push_str(&format!(
            "  \"start\": {{\"present\": true, \"function_index\": {i}}},\n"
        )),
        None => s.push_str("  \"start\": {\"present\": false, \"function_index\": null},\n"),
    }
    let role = |i: Option<u32>| i.map_or_else(|| "null".to_string(), |i| i.to_string());
    match &analysis.roles {
        Some(r) => s.push_str(&format!(
            "  \"hostRoleTable\": {{\"box\": {}, \"add\": {}, \"mul\": {}, \"sub\": {}, \"toIndex\": {}, \"cmp\": {}, \"eq\": {}, \"divmod\": {}}},\n",
            role(r.box_idx),
            role(r.add_idx),
            role(r.mul_idx),
            role(r.sub_idx),
            role(r.to_index_idx),
            role(r.cmp_idx),
            role(r.eq_idx),
            role(r.divmod_idx),
        )),
        None => s.push_str("  \"hostRoleTable\": null,\n"),
    }
    s.push_str(&format!(
        "  \"stringHostRoles\": [{}],\n",
        analysis
            .string_roles
            .iter()
            .map(|(i, r)| format!(
                "{{\"function_index\": {i}, \"role\": {}}}",
                json_str(r.manifest_value())
            ))
            .collect::<Vec<_>>()
            .join(", ")
    ));
    s.push_str(&format!(
        "  \"certified\": {},\n",
        json_list(&analysis.certified, |c| {
            let (policy, level, termination) = if c.total {
                (
                    "simulatesModelTotally",
                    "L3",
                    ", \"termination_witness\": {\"measure\": {\"kind\": \"intNatAbs\", \"param_index\": 0}, \"descent\": -1}",
                )
            } else {
                ("simulatesModel", "L1", "")
            };
            format!(
                "{{\"name\": {}, \"class\": \"{PLAN_CLASS}\", \"facets\": [{}], \"policy\": \"{policy}\", \"level\": \"{level}\", \"theorem\": \"{FN_CLAIM_DISCHARGE_THEOREM}\"{termination}}}",
                json_str(&c.name),
                c.facets.iter().map(|f| json_str(f)).collect::<Vec<_>>().join(", ")
            )
        })
    ));
    s.push_str(&format!(
        "  \"source_level_only\": {}\n",
        json_list(&analysis.declined, |(n, r)| format!(
            "{{\"name\": {}, \"reason\": {}}}",
            json_str(n),
            json_str(r)
        ))
    ));
    s.push_str("}\n");
    s
}

/// Write the artifact-specific `cert/` package: the plans, the manifest, the
/// artifact data with its byte-fact proofs, the final theorem, and the JSON
/// manifest the checker reads. Any existing `cert/` directory is replaced.
///
/// Every law-claim or source bridge the producer refused to declare comes back
/// in [`ProjectDeclines`] with its reason (the bridge list is also written to
/// the manifest as `sourceBridgesDeclined`).
pub fn write_project(
    out_dir: &Path,
    artifact: CertificateArtifact<'_>,
    analysis: &Analysis,
    model: &SourceModel,
) -> Result<ProjectDeclines, String> {
    artifact.validate()?;
    let cert_dir = out_dir.join("cert");
    match std::fs::remove_dir_all(&cert_dir) {
        Ok(()) => {}
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
        Err(error) => return Err(format!("replace cert dir: {error}")),
    }
    std::fs::create_dir_all(&cert_dir).map_err(|e| format!("create cert dir: {e}"))?;
    let sha = sha256_hex(artifact.delivered_bytes());
    let target = artifact.target();
    let abi = artifact.abi();
    let envelope = artifact.wasip2_component_envelope();

    write(
        &cert_dir,
        "Module.lean",
        &render_module(artifact.file_name(), &sha),
    )?;
    write(&cert_dir, "Plans.lean", &render_plans(analysis))?;
    write(
        &cert_dir,
        "Manifest.lean",
        &render_manifest_lean(analysis, &sha, target, abi),
    )?;
    if let Some(params) = analysis
        .roles
        .as_ref()
        .and_then(|r| r.arith_params_record_lean(analysis.carrier))
    {
        write(
            &cert_dir,
            "ArtifactHostRoles.lean",
            &render_artifact_host_roles(analysis, &params),
        )?;
    }
    for (name, text) in render_artifact_plans(analysis) {
        write(&cert_dir, &name, &text)?;
    }
    for (name, text) in render_artifact(analysis, envelope) {
        write(&cert_dir, &name, &text)?;
    }
    write(&cert_dir, "Final.lean", &render_final())?;
    write(
        &cert_dir,
        "ArtifactCertificate.lean",
        &render_artifact_certificate(),
    )?;

    // The source model, the plan-equals-source bridges and the law-claims.
    // The model files ship only when a bridge or a law-claim speaks about
    // them: a package without either builds no model at all.
    let surfaces = plan_surfaces(analysis, model);
    if surfaces.bridge_lean.is_some() || surfaces.laws_lean.is_some() {
        for (path, content) in &surfaces.model.files {
            write_nested(&cert_dir, path, content)?;
        }
    }
    let bridges: &[SourceBridge] = if surfaces.bridge_lean.is_some() {
        &surfaces.bridges
    } else {
        &[]
    };
    if let Some((proofs, corollaries, parts)) = &surfaces.bridge_lean {
        for (name, text) in parts {
            write(&cert_dir, name, text)?;
        }
        write(&cert_dir, &format!("{BRIDGE_PROOF_MODULE}.lean"), proofs)?;
        write(&cert_dir, "Bridge.lean", corollaries)?;
    }
    if let Some(laws_lean) = &surfaces.laws_lean {
        write(&cert_dir, "Laws.lean", laws_lean)?;
    }
    std::fs::write(
        cert_dir.join("cert-manifest.json"),
        render_manifest_json(
            analysis,
            artifact.file_name(),
            &sha,
            target,
            abi,
            envelope,
            &surfaces.law_claims,
            &surfaces.law_bridge_exports,
            bridges,
            &surfaces.declined_bridges,
        ),
    )
    .map_err(|e| format!("write manifest: {e}"))?;
    Ok(ProjectDeclines {
        law_claims: surfaces.declined_laws,
        source_bridges: surfaces.declined_bridges,
    })
}

/// Write a model file, which may sit in a module subdirectory
/// (`Domain/Rational.lean`).
fn write_nested(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| format!("create directory for {name}: {e}"))?;
    }
    std::fs::write(&path, content).map_err(|e| format!("write {}: {e}", path.display()))
}
