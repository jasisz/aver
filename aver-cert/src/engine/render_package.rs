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

/// Pairs of Strings as the Lean list of pairs of their character lists.
fn lean_char_pairs(items: &[(String, String)]) -> String {
    let pairs = items
        .iter()
        .map(|(a, b)| format!("({}, {})", lean_char_list(a), lean_char_list(b)))
        .collect::<Vec<_>>()
        .join(",\n     ");
    format!("[{pairs}]")
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
    s.push_str(&analysis.types.lean_decls("types"));
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

/// The declared-uncertified exports, in the order of the wall's name keys
/// (`WasmSlice.seqKey` of the name's code points: one digit of 21 bits per
/// code point, the first the lowest), so the export accounting finds them
/// already sorted (`SortedKeys.sortedOr`) instead of sorting them in the
/// kernel. The order is a convenience only: any other order is sorted by the
/// kernel and accepted the same.
fn declared_uncertified(analysis: &Analysis) -> Vec<(String, String)> {
    let mut declared = analysis
        .module_envelope
        .declared_uncertified(analysis.certified_names(), &analysis.declined);
    declared.sort_by_cached_key(|(name, _)| name_key_order(name));
    declared
}

/// A sort key whose order is the numeric order of `WasmSlice.seqKey` over a
/// name's code points: more code points is larger (every digit is at least
/// 1), and at equal length the last code point is the most significant.
fn name_key_order(name: &str) -> (usize, Vec<u32>) {
    let mut codes: Vec<u32> = name.chars().map(u32::from).collect();
    codes.reverse();
    (codes.len(), codes)
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
    // A big module's export lists do not fit one declaration: they are
    // written in pieces (`lean_list_in_pieces`) ahead of `subject`. The pieces
    // live under `AverCert.Plans`, not under `AverCert.subject`: the checker's
    // audit refuses a package name that extends another declared constant.
    let mut pieces = String::new();
    let string_items = |items: &[String]| items.iter().map(|x| lean_str(x)).collect::<Vec<_>>();
    let pair_items = |items: &[(String, String)]| {
        items
            .iter()
            .map(|(a, b)| format!("({}, {})", lean_str(a), lean_str(b)))
            .collect::<Vec<_>>()
    };
    let exports = lean_list_in_pieces(
        &mut pieces,
        "Plans.subject_exports",
        "String",
        &string_items(&analysis.certified_names()),
    );
    let declared = lean_list_in_pieces(
        &mut pieces,
        "Plans.subject_declaredUncertified",
        "String × String",
        &pair_items(&declared_uncertified(analysis)),
    );
    let capabilities = lean_list_in_pieces(
        &mut pieces,
        "Plans.subject_capabilities",
        "String × String",
        &pair_items(&analysis.module_envelope.capabilities),
    );
    let contracts = lean_list_in_pieces(
        &mut pieces,
        "Plans.subject_contracts",
        "String",
        &string_items(&analysis.contracts),
    );
    format!(
        "-- The certificate's manifest: the subject (artifact identity, exports,\n\
         -- helper indices and the contracts it is conditional on), the plans and\n\
         -- the obligations, which are exactly the ones the wall derives.\n\
         import SchemaCore\n\
         import AcceptedArtifactCore\n\
         import Plans\n\n\
         namespace AverCert\n\
         open AverCert.Schema\n\n\
         {pieces}\
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
    )
}

fn render_artifact_host_roles(analysis: &Analysis, params: &str, layout: bool) -> String {
    let roles = analysis.roles.expect("a carriered module declares roles");
    // With a declared layout, each helper body is read from it (one slice)
    // instead of decoding the code section in every declaration.
    let proof = if layout {
        "by\n  rw [AverCert.DeclaredLayout.arithRoleCheck_of_layout layout_ok] <;> decide +kernel"
    } else {
        "by decide +kernel"
    };
    let leaf = |name: &str, idx: Option<u32>| {
        let idx = idx.map_or_else(|| "none".to_string(), |idx| format!("(some {idx})"));
        format!(
            "theorem decodedHostRole_{name} : AverCert.AcceptedArtifact.arithRoleCheck \
             AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen \
             ArithTemplateDerisk.ArithRole.{name} {idx} {params} = true := {proof}"
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
         import ArtifactBytes\n\
         {layout_import}\n\
         set_option maxRecDepth 200000\n\n\
         namespace AverCert.Artifact\n\n\
         {leaves}\n\n\
         end AverCert.Artifact\n",
        layout_import = if layout { "import ArtifactLayout\n" } else { "" },
    )
}

/// Plans checked per kernel declaration in `ArtifactPlans.lean`.
const PLAN_CHUNK: usize = 32;

/// The per-entry plan checks (`entryAccepted`), `PLAN_CHUNK` entries per
/// declaration, chained from the last chunk back to the whole list. With a
/// declared layout (`ArtifactLayout.lean`) a chunk proves them through
/// `DeclaredLayout.entries_of_fast`: every module fact of a plan is read from
/// the confirmed declaration (its code entry by offset, its type index and
/// function type, its export entry by position), so no chunk decodes a
/// section other than the export section or searches for an export. Without
/// one (a package with no plans) each chunk is decided directly.
fn render_artifact_plans(analysis: &Analysis, layout: bool) -> Vec<(String, String)> {
    let n = analysis.entries.len();
    let header = format!(
        "set_option maxRecDepth 200000\n\
         set_option maxHeartbeats 1600000\n\n\
         namespace AverCert.Artifact\n\
         open AverCert AverCert.Schema AverCert.AcceptedArtifact AverCert.TypeTable{}\n\n",
        if layout { " AverCert.DeclaredLayout" } else { "" }
    );
    let split = splits_artifact_modules(analysis);
    let mut plan_ok = "/-- One plan's acceptance check against the staged artifact bytes. -/\n\
         noncomputable abbrev planOk : FnEntry → Bool :=\n  \
           entryAccepted AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen\n    \
           (mctxOf AverCert.manifest.subject AverCert.manifest.types AverCert.manifest.fnPlans)\n    \
           AverCert.manifest.fnPlans\n\n"
        .to_string();
    if layout {
        plan_ok.push_str(
            "theorem types_ok : fnTypesConfirmed AverCert.ArtifactBytes.modBytes\n    \
               AverCert.ArtifactBytes.modLen fnTypes = true := by\n  \
               rw [fnTypesConfirmed, types_cut]; decide +kernel\n\n\
             theorem names_ok : exportNamesDistinct AverCert.ArtifactBytes.modBytes\n    \
               AverCert.ArtifactBytes.modLen = true :=\n  ",
        );
        // A split package proves the export accounting in its own module, and
        // the accounting decides the names distinct; a small one decides them.
        plan_ok.push_str(if split {
            "AverCert.SortedKeys.exportNamesDistinct_of_accounted exports_ok\n\n"
        } else {
            "by rw [exportNamesDistinct, exports_cut]; decide +kernel\n\n"
        });
    }
    let proof = |decls: &str| {
        if layout {
            format!(
                ":=\n  entries_of_fast layout_ok types_ok names_ok (ds := {decls}) rfl\n    \
                 (by rw [entriesFast, exports_cut]; decide +kernel)\n\n"
            )
        } else {
            ":= by\n  decide +kernel\n\n".to_string()
        }
    };
    let starts: Vec<usize> = (0..n.max(1)).step_by(PLAN_CHUNK).collect();
    let last = *starts.last().expect("at least one chunk");
    let from_last = format!(
        "theorem plans_from_{last} : (AverCert.manifest.fnPlans.drop {last}).all planOk = true {}",
        proof(&format!("fnDecls.drop {last}"))
    );
    let chunk = |k: usize| {
        format!(
            "theorem plans_chunk_{k} :\n    \
             ((AverCert.manifest.fnPlans.drop {k}).take {PLAN_CHUNK}).all planOk = true {}",
            proof(&format!("(fnDecls.drop {k}).take {PLAN_CHUNK}"))
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
    let imports = format!(
        "import AcceptedArtifact\nimport ArtifactBytes\nimport Manifest\n{}{}\n",
        if layout { "import ArtifactLayout\n" } else { "" },
        if layout && split { "import ArtifactInterface\n" } else { "" }
    );
    let mut body = String::new();
    let mut chained = String::new();
    for (i, k) in starts.iter().enumerate().rev().skip(1) {
        body.push_str(&chunk(*k));
        chained.push_str(&chain(*k, starts[i + 1]));
    }
    let end = "theorem plans_all : AverCert.manifest.fnPlans.all planOk = true := plans_from_0\n\n\
         end AverCert.Artifact\n";
    if !split {
        return vec![(
            "ArtifactPlans.lean".to_string(),
            format!(
                "-- The per-plan acceptance checks, a few plans per declaration,\n\
                 -- chained into the check over every plan.\n\
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
    layout: bool,
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
                 decodedHostRole_eq, decodedHostRole_divmod, Bool.and_true, Bool.true_and,\n    \
                 AverCert.DeclaredLayout.Chars.carrierHelperAbsent_eq,\n    \
                 AverCert.DeclaredLayout.Chars.boxIdx_eq, AverCert.DeclaredLayout.Chars.toIndexIdx_eq,\n    \
                 AverCert.DeclaredLayout.Chars.cmpIdx_eq{cuts}]\n  \
                 {carrier}decide +kernel",
                r.roles_lean_value(),
                cuts = if layout { ", exports_cut" } else { "" },
                // The carrier is read through the type-section cut. Its
                // definition is a `match` on the decoded type section, so it
                // is unfolded by its unconditional equation with matcher
                // reduction off: otherwise `simp` evaluates the whole type
                // decode in the elaborator before the cut can rewrite it.
                carrier = if layout {
                    "simp -iota only [CertDecode.carrierState.eq_def, types_cut]\n  "
                } else {
                    ""
                },
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
    // The String roles are decided through `roleTableFast`, which reads a
    // function's signature only when its type has a helper's shape.
    // With a declared layout they read the type and code sections through
    // their confirmed cuts.
    let strings = if layout {
        "theorem strings_ok : decodedStringHostRoles data := by\n  \
         dsimp only [decodedStringHostRoles, data]\n  \
         rw [← AverCert.DeclaredLayout.StringFast.roleTableFast_eq,\n    \
         AverCert.DeclaredLayout.StringFast.roleTableFast, CertDecode.StringHost.decodeTypeSigs,\n    \
         CertDecode.StringHost.bodyLocs, types_cut, code_cut]\n  \
         decide +kernel\n\n"
    } else {
        "theorem strings_ok : decodedStringHostRoles data := by\n  \
         unfold decodedStringHostRoles\n  \
         rw [← AverCert.DeclaredLayout.StringFast.roleTableFast_eq]; decide +kernel\n\n"
    };
    // With a declared layout the closure scan reads each member's code entry
    // from it (one slice) instead of decoding the code section per member.
    let closure_ok = if layout {
        "theorem closure_ok : closureIsolation data = true :=\n  \
         AverCert.DeclaredLayout.closureIsolation_of_layout layout_ok\n    \
         (AverCert.SortedKeys.closureIsolationL_of_S (by decide +kernel))\n\n"
    } else {
        "theorem closure_ok : closureIsolation data = true := by decide +kernel\n\n"
    };
    let layout_import = if layout {
        "import ArtifactLayout\nimport SortedKeys\n"
    } else {
        ""
    };
    let exports = format!(
        "theorem framing_ok : CertDecode.moduleFramingValid data.modBytes data.modLen = true := by\n  \
           decide +kernel\n\n\
         theorem exports_ok : exportsAccounted data = true :=\n  \
           exportsAccounted_of_chars data\n    \
           {obligation_names}\n    \
           {declared_names}\n    \
           rfl rfl {exports_proof}\n\n\
         theorem imports_ok : importsWithinCapabilities data = true :=\n  \
           AverCert.DeclaredLayout.Chars.importsWithinCapabilities_of_chars data\n    \
           {capabilities}\n    \
           rfl (by decide +kernel)\n\n\
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
        // With a declared layout the export section is read through its cut.
        exports_proof = if layout {
            "(AverCert.SortedKeys.exportsAccountedOf_of_fast exports_cut (by decide +kernel))"
        } else {
            "(by decide +kernel)"
        },
        capabilities = lean_char_pairs(&analysis.module_envelope.capabilities),
        declared_names = lean_char_lists(
            &declared_uncertified(analysis)
                .into_iter()
                .map(|(name, _)| name)
                .collect::<Vec<_>>(),
            "\n     "
        ),
    );
    // With a declared layout the helper types are read from it. The
    // definitions that `match` on the decoded type section are unfolded by
    // their unconditional equations with matcher reduction off, so the type
    // section is read only through its cut, in the kernel: `simp` with the
    // definitions themselves evaluates the whole type decode in the
    // elaborator first (btc-listener: over 10 minutes per theorem).
    // With a declared layout every conjunct of the rest is its own
    // declaration (`rest_parts`), joined by `plansAcceptedRestL_of_parts`:
    // one kernel check over all of them keeps every conjunct's terms alive
    // until the last one ends (btc-listener: 367 s and 15 GB as one
    // declaration, 80 s and at most 9 GB as its parts).
    let rest_proof = if layout {
        "(AverCert.DeclaredLayout.plansAcceptedRest_of_layout layout_ok\n    \
         (AverCert.DeclaredLayout.plansAcceptedRestL_of_parts rest_indices rest_types rest_data\n      \
         rest_roles rest_eqref rest_newtypes rest_inhabited rest_cons))"
    } else {
        "(by decide +kernel)"
    };
    let rest_parts = rest_parts(layout);
    let rest = format!(
        "theorem plans_ok : plansAccepted data = true :=\n  \
           plansAccepted_of_parts data plans_all {rest_proof}\n\n\
         {roles_proof}\n\n\
         theorem axes_ok : AverCert.ClaimAxes.checked data = true := by decide +kernel\n\n"
    );
    let tail = "theorem whole_ok : acceptedWholeModule data :=\n  \
           ⟨framing_ok, exports_ok, imports_ok, start_ok, closure_ok⟩\n\n\
         theorem envelope_ok : artifactEnvelopeAccepted AverCert.ArtifactComponentBytes.componentBytes\n    \
           AverCert.ArtifactComponentBytes.componentLen data = true := by decide +kernel\n\n\
         end AverCert.Artifact\n";
    let base_imports =
        "import AcceptedArtifact\nimport DeclaredLayout\nimport ArtifactBytes\nimport Manifest\n";
    if !splits_artifact_modules(analysis) {
        let rest_all: String = rest_parts.iter().map(|(_, body)| body.as_str()).collect();
        return vec![(
            "Artifact.lean".to_string(),
            format!(
                "-- The artifact data and the byte facts of its acceptance, each by\n\
                 -- `decide +kernel` against the checker-staged `ArtifactBytes`.\n\
                 {base_imports}\
                 import ArtifactPlans\n\
                 {layout_import}\
                 {roles_import}\n\
                 {ARTIFACT_HEADER}\
                 {data}{rest_all}{rest}{strings}{exports}{closure_ok}{tail}"
            ),
        )];
    }
    let part = |comment: &str, imports: &str, body: &str| {
        format!(
            "-- {comment}\n\
             import ArtifactData\n\
             {imports}\n\
             {ARTIFACT_HEADER}\
             {body}\
             end AverCert.Artifact\n"
        )
    };
    // The parts of the rest, grouped into modules: the heavy ones alone, so
    // that each builds in a process of its own (and in parallel with more
    // than one Lake worker), the light ones together.
    let mut rest_modules: Vec<(String, String)> = Vec::new();
    for (module, body) in &rest_parts {
        match rest_modules.iter_mut().find(|(name, _)| name == module) {
            Some((_, text)) => text.push_str(body),
            None => rest_modules.push((module.to_string(), body.clone())),
        }
    }
    let rest_imports: String = rest_modules
        .iter()
        .map(|(module, _)| format!("import {module}\n"))
        .collect();
    let mut files = vec![
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
            part("The String helper roles, decoded from the module.", layout_import, strings),
        ),
        (
            "ArtifactClosure.lean".to_string(),
            part("The certified closure's isolation.", layout_import, closure_ok),
        ),
        (
            "ArtifactInterface.lean".to_string(),
            part(
                "The module's framing, exports, imports and start function.",
                layout_import,
                &exports,
            ),
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
                 {rest_imports}\
                 {roles_import}\n\
                 {ARTIFACT_HEADER}\
                 {rest}{tail}"
            ),
        ),
    ];
    for (module, body) in rest_modules {
        files.push((
            format!("{module}.lean"),
            part(
                "Conjuncts of the plans' acceptance other than the per-plan checks.",
                "import ArtifactLayout\nimport SortedKeys\n",
                &body,
            ),
        ));
    }
    files
}

/// The conjuncts of `DeclaredLayout.plansAcceptedRestL`, one declaration
/// each, with the module a split package proves it in. Empty without a
/// declared layout, where the rest is decided whole. The helper types and
/// the type table read the type section through its cut: the definitions
/// that `match` on the decoded section are unfolded by their unconditional
/// equations with matcher reduction off, so `simp` does not evaluate the
/// whole type decode in the elaborator before the cut can rewrite it
/// (btc-listener: over 10 minutes per theorem).
fn rest_parts(layout: bool) -> Vec<(&'static str, String)> {
    if !layout {
        return Vec::new();
    }
    const BYTES: &str = "AverCert.ArtifactBytes.modBytes AverCert.ArtifactBytes.modLen";
    const DECLS: &str = "AverCert.manifest.subject AverCert.manifest.types AverCert.manifest.fnPlans";
    const TYPES_PLANS: &str = "AverCert.manifest.types AverCert.manifest.fnPlans";
    let m = format!("(AverCert.TypeTable.mctxOf {DECLS})");
    vec![
        (
            "ArtifactRest",
            format!(
                "theorem rest_indices : indicesDistinct {m} AverCert.manifest.fnPlans = true :=\n  \
                 AverCert.SortedKeys.indicesDistinct_of_bits (by decide +kernel)\n\n"
            ),
        ),
        (
            "ArtifactRestTypes",
            format!(
                "theorem rest_types : AverCert.TypeTable.typeTableConfirmed {BYTES}\n    \
                 {DECLS} = true := by\n  \
                 simp -iota only [AverCert.TypeTable.typeTableConfirmed.eq_def,\n    \
                 AverCert.TypeTable.carrierConfirmed.eq_def, CertDecode.carrierState.eq_def, types_cut]\n  \
                 decide +kernel\n\n"
            ),
        ),
        (
            "ArtifactRestData",
            format!(
                "theorem rest_data : AverCert.TypeTable.dataConfirmed {BYTES}\n    \
                 {DECLS} = true := by\n  \
                 decide +kernel\n\n"
            ),
        ),
        (
            "ArtifactRestRoles",
            format!(
                "theorem rest_roles : AverCert.DeclaredLayout.roleTypesPinnedL layout {BYTES}\n    \
                 {m} = true := by\n  \
                 simp -iota only [AverCert.DeclaredLayout.roleTypesPinnedL,\n    \
                 AverCert.DeclaredLayout.roleTypePinnedL, AverCert.WasmSlice.typeSectionMatches.eq_def,\n    \
                 types_cut]\n  \
                 decide +kernel\n\n"
            ),
        ),
        (
            "ArtifactRest",
            format!(
                "theorem rest_eqref : AverCert.TypeTable.eqrefConfined {TYPES_PLANS} = true := by\n  \
                 decide +kernel\n\n\
                 theorem rest_newtypes : AverCert.TypeTable.newtypesGrounded AverCert.manifest.types = true := by\n  \
                 decide +kernel\n\n\
                 theorem rest_inhabited : AverCert.TypeTable.typesInhabited {m}\n    \
                 {TYPES_PLANS} = true := by\n  \
                 decide +kernel\n\n\
                 theorem rest_cons : AverCert.AcceptedArtifact.consPinned {TYPES_PLANS} = true := by\n  \
                 decide +kernel\n\n"
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
    surfaces: &Surfaces,
) -> String {
    let laws = &surfaces.law_claims;
    let law_bridges = &surfaces.law_bridge_exports;
    let bridges = surfaces.packaged_bridges();
    let declined_bridges = &surfaces.declined_bridges;
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

    // `Module.lean` (the artifact hash `Schema.Holds` reads) is not written:
    // the wall imports it, so the checker renders it from the bytes it reads.
    write(&cert_dir, "Plans.lean", &render_plans(analysis))?;
    // A package with plans declares the module layout its byte checks read.
    let layout = !analysis.entries.is_empty();
    if layout {
        write(
            &cert_dir,
            "ArtifactLayout.lean",
            &render_artifact_layout(artifact.core_module_bytes(), analysis)?,
        )?;
    }
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
            &render_artifact_host_roles(analysis, &params, layout),
        )?;
    }
    for (name, text) in render_artifact_plans(analysis, layout) {
        write(&cert_dir, &name, &text)?;
    }
    for (name, text) in render_artifact(analysis, envelope, layout) {
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
            &surfaces,
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

#[cfg(test)]
mod name_key_order_tests {
    use super::name_key_order;

    /// `WasmSlice.seqKey` of a name's code points, for names short enough
    /// to fit a `u128` (five 21-bit digits).
    fn seq_key(name: &str) -> u128 {
        name.chars().rev().fold(0u128, |key, c| {
            u128::from(u32::from(c)) + 1 + 2_097_152 * key
        })
    }

    /// The producer's sort order is the numeric order of the wall's keys,
    /// so a declared list it sorts passes `strictly` without a kernel sort.
    #[test]
    fn name_key_order_is_the_wall_key_order() {
        let names = [
            "", "a", "b", "z", "ab", "ba", "zz", "aaa", "_st", "size", "é", "aé", "éa", "mem",
            "__a", "abcde", "bbcde", "abcdf",
        ];
        for a in names {
            for b in names {
                assert_eq!(
                    name_key_order(a).cmp(&name_key_order(b)),
                    seq_key(a).cmp(&seq_key(b)),
                    "{a:?} vs {b:?}"
                );
            }
        }
    }
}
