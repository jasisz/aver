# Aver Artifact Certificate Format Specification

This document is the normative specification of the Aver artifact certificate format: the on-disk `cert/` package emitted by `aver compile --target wasm-gc --certify` and the acceptance behavior required of a verifier that consumes it. The intended audience is an independent reimplementor of the verifier. The reference implementation is the standalone `aver-cert` crate; every requirement stated here is enforced by that implementation, and an appendix maps each section to the source files that define the behavior. This is not an overview document; see [certification.md](certification.md) for the user-facing description and [certification-architecture.md](certification-architecture.md) for the data-flow narrative.

The key words MUST, MUST NOT, and MAY are normative. Wherever this document says a value is *kernel-pinned*, the reference verifier proves the stated equality inside the Lean kernel (by `rfl` in a checker-authored witness or as a conjunct of the accepted-artifact proposition); a conforming verifier MUST NOT accept a package for which any pinned equality fails. Wherever this document says a value is *declared-only*, the value is transported for display and is deliberately not part of the verified claim; a conforming verifier MUST NOT present declared-only values as verified facts.

> **TODO-decision: format name.** The format has no frozen public name. This document uses the descriptive phrase "Aver artifact certificate format"; a short stable name (for registries, media types, file signatures) is an open decision and MUST be settled before the format is declared frozen.

## 1. Versioning and identity

Three version-like identities govern acceptance:

| Identity | Current value | Where it lives | What it versions |
|---|---|---|---|
| Package layout version | `1` (`FORMAT_VERSION`) | `cert-manifest.json` `format.version` | The `cert/` directory layout and the transport envelope |
| Statement schema version | `2` (`CERT_SCHEMA_VERSION`) | `cert-manifest.json` `schema_version` | The certificate statement schema: manifest fields, obligation shapes, plan grammars |
| Wall identity | `sha256:<64 lowercase hex>` | `cert-manifest.json` `format.wall_id` | The exact checker-owned Lean soundness wall plus its pinned Lean toolchain |

A conforming verifier MUST reject a package whose `schema_version` is not exactly the schema version it implements, whose `format.version` is not exactly `1`, or whose `format.wall_id` does not name a wall embedded in the verifier itself. There is no version negotiation, no downgrade path, and no filesystem, environment, or network fallback for resolving a wall.

Schema version 2 differs from version 1 in exactly one point: the subject's `hostRoleTable` became optional. A module without the Int-carrier box helper declares `null`, and the acceptance proof pins that declaration against a byte-derived proof that the `__rt_aint_from_i64` helper export is absent (see section 4.3). A version-2 verifier MUST NOT accept a `schema_version: 1` package.

The wall identity is computed, not assigned. It is the SHA-256 of a domain-separated, sorted, length-framed encoding of every wall source file plus the Lean toolchain pin, formatted as `sha256:` followed by 64 lowercase hex digits. The exact encoding: the ASCII bytes `aver-certificate-wall\0v1\0`, then the file count as a big-endian `u64`, then for each file in ascending filename order: the filename length as big-endian `u64`, the filename bytes, the contents length as big-endian `u64`, the contents bytes. The file set is the 33 embedded `.lean` wall sources plus one synthetic file named `lean-toolchain` whose contents are the toolchain pin (currently `leanprover/lean4:v4.32.0`). The reference verifier recomputes this digest over its own embedded sources on first use and aborts if it disagrees with the compiled-in constant, so a verifier binary cannot silently ship a wall that does not match its advertised identity. A reimplementation MUST resolve `format.wall_id` only against wall source sets whose recomputed identity is byte-exact; it MUST NOT resolve a wall by name, path, or prefix.

> **TODO-decision: freeze criteria.** Neither `format.version = 1` nor `schema_version = 2` is declared frozen yet. The criteria for freezing (what constitutes a compatible extension versus a version bump, and whether a frozen schema admits additive optional fields) are an open decision. Until freeze, every schema change bumps `schema_version` and verifiers reject non-matching versions exactly.

> **TODO-decision: wall registry policy.** The reference verifier embeds exactly one wall and resolves only that identity. Whether a released verifier may embed several walls (for grace-window verification of older packages), and the deprecation policy for retired walls, is an open decision.

## 2. Package layout

A certificate package is one directory, conventionally named `cert/`, emitted next to `<name>.wasm`. The producer removes any pre-existing `cert/` directory before writing, so a package is always a complete, self-consistent emission. The package contains exactly:

| File | Role |
|---|---|
| `cert-manifest.json` | Transport and reporting envelope (section 4). The only non-Lean file. |
| `Plans.lean` | The sole authoritative plan data: every accepted plan as a Lean value, plus `rfl` examples binding each plan through the audited checkers/lowerers to exact code-entry bytes (section 6). |
| `Manifest.lean` | The Lean manifest literal (`AverCert.manifest : Schema.Manifest`): subject data, per-export obligation definitions, and the plan lists (section 5). |
| `Module.lean` | Certified function bodies re-rendered as `CertPrelude.WInstr` data (`CertModule.*Code`), host tables, and the pinned `CertModule.wasmSha256` string. |
| `Contracts.lean` | Human-auditable restatement of the named runtime contracts. Contracts are hypotheses of the certificate theorems, never Lean `axiom`s. |
| `Certificate.lean` | Per-export simulation proofs and their composition into the schema obligations. |
| `Final.lean` | The single final schema theorem `AverCert.Final.cert : AverCert.Schema.Holds manifest`. |
| `Artifact.lean` | The artifact claim data (`AverCert.Artifact.data : ArtifactData`), the per-family acceptance proof bundles, and `acceptedWithFinal`. |
| `ArtifactSoundness.lean` | Per-artifact glue instantiating the artifact-independent acceptance-soundness wall at the real module hash. |
| `ArtifactCertificate.lean` | The public proof root: `theorem AverCert.Artifact.certificate : AverCert.AcceptedArtifact.accepted AverCert.Artifact.data`, followed by `#print axioms`. |
| Model modules (`AverCommon.lean`, `<Module>.lean`, ...) | The `aver proof` Lean model emission, copied with every `deriving` line stripped (the staging token scan rejects `deriving`). These carry the source-model definitions the obligations reference. |

The package MUST NOT contain — and the verifier ignores or rejects if present — `ArtifactBytes.lean` (checker-generated from the actual `.wasm`, section 7), `CheckerWitness.lean` (checker-authored, section 8), `lakefile.lean`, `lean-toolchain`, any wall source file, any build cache, or plan sidecar files (`*.plan`, JSON plan ASTs). Package-supplied files with checker-owned names are silently ignored during staging; case-variant near-collisions are rejected (section 9, stage 5).

Every `.lean` file in the package is untrusted data. It participates in the verdict only after the staging gates of section 9 and only because the Lean kernel accepts the resulting proof against checker-pinned facts.

## 3. Trust vocabulary: kernel-pinned versus declared-only

The manifest JSON is a transport envelope. The authoritative statement is the Lean value `AverCert.manifest` in `Manifest.lean` together with the claim data in `Artifact.lean`, and the verifier's checker witness pins the JSON's trust-bearing fields to that Lean value by `rfl` (section 8). Consequently every `cert-manifest.json` field is in exactly one of two classes:

- **Kernel-pinned**: the witness (or the accepted-artifact proposition itself) states an equality between the JSON-derived value and the Lean manifest/claim data, and the wall further binds that Lean data to the artifact bytes. Tampering with the JSON, the Lean data, or the bytes independently makes elaboration fail.
- **Declared-only**: transported for reporting. The reference verifier either never reads the field on the acceptance path or reads it only to print it with an explicit "declared" label. The `certified[].dom` / `certified[].cod` strings are the deliberate example: the source-facing domain/codomain prose is display-only and unpinned **by design**, because the pinned semantic face lives in the typed `Obligation` (Dom/Cod types and representation relations) and in `StandardFace`, not in a string. The CERTIFIED/CHECKED report MUST print only kernel-pinned facts; `explain` MAY print declared-only values with an explicit label.

## 4. `cert-manifest.json`, schema version 2

The manifest is a single JSON object. The reference parser is strict about the fields it reads: a missing or mistyped required field is a hard error. String fields that are later interpolated into the checker witness pass a candidate gate first: at most 200 bytes, every byte in `0x20..=0x7E`, and neither `"` nor `\` (this makes Lean string-literal injection unrepresentable). The gated strings are: each certified export's `name`, `class`, `dom`, `cod`; every `runtime_contracts` entry; every `declaredUncertified` name and reason; every `capabilities` module and name; `profile`; and `abi`.

> **TODO-decision: top-level strictness.** Nested objects are matched exactly (an unexpected or missing key inside `start`, `hostRoleTable`, `stringHostRoles[]`, `declaredUncertified[]`, or `capabilities[]` is an error), but unknown top-level members are currently ignored by the reference verifier. Whether a frozen schema requires rejecting unknown top-level members is an open decision; producers MUST NOT rely on the current leniency.

### 4.1 Top-level fields

| Field | Type | Trust class | Meaning and constraints |
|---|---|---|---|
| `schema_version` | integer | verifier-checked | MUST be exactly `2`. |
| `format` | object `{version, wall_id}` | verifier-checked | `version` MUST be exactly `1`; `wall_id` MUST resolve to an embedded wall (section 1). |
| `wasm` | string | declared-only | The artifact filename the producer emitted next to the package. Never read on the acceptance path; the artifact identity is the file the caller passes to the verifier. |
| `wasm_sha256` | string, 64 lowercase hex | kernel-pinned | SHA-256 of the exact artifact bytes. The verifier MUST recompute the hash of the supplied `.wasm` and reject on mismatch; the witness additionally pins `manifest.subject.artifactHash` to the recomputed hash, and `Schema.Holds` conjoins `artifactHash = CertModule.wasmSha256` (section 7). |
| `level` | string | declared-only | `"L1"`, `"L3"`, or `"mixed L1/L3"`. The verifier computes its own level banner from the pinned policies and never reads this field. |
| `profile` | string | kernel-pinned | Emitted-fragment profile identifier, currently `"AverUserProfile/v1"`. Pinned to `subject.profile`. |
| `abi` | string | kernel-pinned | Runtime ABI identifier, currently `"aver-wasm-gc/0"`. Pinned to `subject.abi`. |
| `final_theorem` | string | declared-only | `"AverCert.Final.cert"`. Informational; the checker consumes the artifact root below, not this name. |
| `artifact_certificate_root` | string | verifier-checked and kernel-pinned | MUST be exactly `"AverCert.Artifact.certificate"`. Checked in Rust, pinned to `subject.artifactRoot` by the witness, and re-checked in-kernel by `subjectMatchesArtifactRoot`. |
| `carrier_type_index` | integer or `null` | declared-only | The wasm type index of the Int carrier struct, `null` for carrierless modules. The kernel derives the carrier independently via `CertDecode.decodeCarrier`; this field is reporting convenience. |
| `runtime_contracts` | array of strings | kernel-pinned | The named runtime contracts every certificate is conditional on. Pinned to `subject.contracts`; the in-kernel `ClaimAxes` check independently derives the exact required contract set from the checked claims. |
| `declaredUncertified` | array of `{name, reason}` | kernel-pinned | Exports outside the claimed obligations, each with a reason. Pinned to `subject.declaredUncertified`; the in-kernel `exportsAccounted` check requires that every byte-derived module export is either a claimed obligation (matching name, kind, and function index) or listed here, with both lists duplicate-free, disjoint, and free of phantom names. |
| `capabilities` | array of `{module, name}` | kernel-pinned | The exact effect-import surface, in import-section order. Pinned to `subject.capabilities`; the in-kernel `importsWithinCapabilities` check requires the byte-derived import section to equal this list exactly and every pair to be a member of the kernel-owned `CAPABILITY_REGISTRY` (the closed list of `("aver", ...)` host imports; the verifier crate carries the same list as `WASM_GC_CAPABILITIES` so the compiler cannot silently broaden it). |
| `start` | object `{present, function_index}` | kernel-pinned | Exactly these two keys. `present: false` MUST pair with `function_index: null`; `present: true` MUST pair with a `u32`. Pinned to `subject.start`; the in-kernel `startAccounted` check equates it with the byte-derived start-section decode. |
| `hostRoleTable` | `null` or object `{box, add, mul, sub}` | kernel-pinned | See section 4.3. |
| `stringHostRoles` | array of `{function_index, role}` | kernel-pinned | `role` MUST be `"stringEq"` or `"stringConcat"`; `function_index` a `u32`. Pinned to `subject.stringHostRoles` and bound in-kernel to `CertDecode.StringHost.roleTable` recomputed from the bytes. |
| `certified` | array of objects | mixed; see 4.2 | One entry per certified export, in manifest-obligation order. |
| `source_level_only` | array of `{name, reason}` | declared-only | Functions the producer declined to certify, with reasons. Printed by `explain` under an explicit informational heading; never read on the acceptance path. Fail-closed emission: a declined function gets an entry here, never a weaker theorem. |

### 4.2 `certified[]` entries

| Field | Type | Trust class | Meaning and constraints |
|---|---|---|---|
| `name` | string | kernel-pinned | The export name. Pinned three ways: `manifest.obligations.map export_`, `subject.exports`, and the first component of the `StandardFace.reportEntries` pairs. |
| `class` | string | kernel-pinned | The admitted family label. Pinned as the second component of `StandardFace.reportEntries`, which the **wall** derives from the checked claim family and plan — the producer cannot choose a more favourable label. Current labels: `expr-fragment-v1`, `verbatim-string-eq`, `verbatim-string-concat`, `adt-constructor`, `self-recursive`, `multi-argument self-recursive`, `mutual-recursive`, `verbatim-dispatch`, `int-dispatch`, `field-projection`, `cross-function-composition`. |
| `policy` | string | kernel-pinned | MUST be `"simulatesModel"` or `"simulatesModelTotally"`; anything else is rejected in Rust. Pinned to `manifest.obligations.map policy` and independently re-derived in-kernel by `ClaimAxes`. |
| `level` | string | declared-only | `"L1"` for `simulatesModel`, `"L3"` for `simulatesModelTotally`. Redundant with `policy`; not read by the verifier. |
| `dom`, `cod` | strings | **declared-only by design** | Source-facing domain/codomain prose. Never pinned by any witness line and MUST NOT appear on the CERTIFIED/CHECKED report; `explain` prints them labeled `manifest face (declared, not kernel-pinned)`. The verified face is the typed `Obligation.Dom`/`Cod` with their representation relations, enforced by `StandardFace`. |
| `theorem` | string | declared-only | The discharge theorem the producer used (e.g. `AcceptanceSoundness.exprFragment_claim_discharges`). Informational; acceptance consumes the single artifact root, not per-export theorem names. |
| `termination_witness` | object, optional | kernel-pinned | MUST be absent when `policy` is `"simulatesModel"` and present when `"simulatesModelTotally"` (both directions are hard errors). Shape: `{"measure": {"kind": "intNatAbs", "param_index": <u32>}, "descent": <i64>}`; `kind` MUST be `"intNatAbs"`. Pinned to `manifest.obligations.map termination?`. In-kernel, `ClaimAxes` accepts only the canonical witness `{measure := .intNatAbs 0, descent := -1}` and `Schema.checkTerm`/`checkTermMutual` verify it against the byte-bound recursion plan (floor guard at `n ≤ 0`, recursive argument exactly `sub(n, box 1)`). |

### 4.3 `hostRoleTable` and the three-state strict decode

`hostRoleTable` declares the module-wide box/add/mul/sub host-helper table used by plan lowering. Its JSON forms are: `null` (the module has no Int-carrier runtime), or an object with exactly the keys `box`, `add`, `mul`, `sub`, each a `u32` or `null` (an individual role may be unbound while the table as a whole exists). This maps to `Subject.hostRoleTable : Option CertDecode.AddSub.Roles` in the Lean manifest.

The kernel binds the declaration with a single unconditional equality against a strict three-state byte decode, `CertDecode.AddSub.roleTableStrict : Nat → Nat → Option (Option Roles)`:

- `some none` — the module is byte-provably carrierless: the export section decodes strictly and no export is named `__rt_aint_from_i64`. The manifest MUST declare `hostRoleTable: null`. A malformed or undecodable export section never certifies absence.
- `some (some t)` — the helper export is present and the module-wide role scan succeeded with exactly table `t`. The manifest MUST declare exactly `t` (`add`/`mul`/`sub` are admitted only as unique carrier-binop candidates; ambiguity or absence declines the individual role to `null` inside the table).
- `none` — the helper is present but the role scan failed. This poisoned state equals `some v` for **no** manifest value `v`, so no declaration can close the acceptance pin `roleTableStrict modBytes modLen = some subject.hostRoleTable`. In particular a carriered module whose scan fails cannot masquerade as carrierless, and a carrierless declaration cannot borrow a carriered module's table.

For claim matching, an absent table binds no roles at all (`Subject.hostRoles` maps `none` to the all-`none` table), so any claim citing a box/add/mul/sub role in a carrierless module fails to match — strictly fail-closed, never a default index.

## 5. Obligations, claims, and policies

The Lean manifest (`Schema.Manifest`) carries the subject, eleven per-family plan association lists keyed by export name (`symFragmentPlans`, `stringEqPlans`, `stringConcatPlans`, `constructPlans`, `exprFragmentPlans`, `recursionPlans`, `mutualPlans`, `compositionPlans`, `verbatimPlans`, `intDispatchPlans`, `fieldProjectionPlans`), and `obligations : List Obligation`.

An `Obligation` is the typed statement unit: export name, `policy`, optional `termination?` witness, `totalityRole` (default `.addSub`), the carrier type index, the emitted code table `code : CodeTbl` and host-table builder `host`, the export's own function index `self`, the source types `Dom`/`Cod`, the representation relations `domRepr : CarrierSpec carrier → Dom → List WVal → Prop` and `codRepr : CarrierSpec carrier → Cod → WVal → Prop`, and the source model `model : Dom → Cod`.

**`simulatesModel` (level L1, partial simulation).** `Obligation.holds` states: for every carrier specification `S`, every host implementation of add/sub/mul/String.eq/String.concat satisfying the named contract laws (integer add/sub/mul preserve representation; String.eq computes byte equality; String.concat computes byte concatenation), every fuel, and every represented domain value — if the emitted body evaluates to a result, that result represents `model x`. Vacuous on trap or fuel exhaustion. The contracts are explicit hypotheses of the theorem, never axioms.

**`simulatesModelTotally` (level L3, total simulation).** `Obligation.holdsTotal` additionally assumes totality of the host helpers selected by the obligation's `totalityRole` (`.addSub`: add and sub total on represented operands; `.mul`: additionally mul total, admitted only for a byte-pinned unary recursion whose combine role is mul) and promises an actual result at fuel `n.natAbs + 1`, where `n` is the checked integer counter (the first domain argument). The termination evidence is the canonical witness of section 4.2, checked against the byte-bound plan by `Schema.checkTerm` (unary and accumulator recursion) or `Schema.checkTermMutual` (mutual SCC members, whose recursive edge is a tail call to a sibling member).

The artifact-independent proposition is `HoldsCore m`: every obligation satisfies the denotation selected by its policy. The full schema proposition, `Schema.Holds m`, conjoins `m.subject.artifactHash = CertModule.wasmSha256`, binding the statement to the artifact hash literal carried in `Module.lean` (which the witness in turn pins to the recomputed hash of the actual bytes).

The public proof root accepted by the verifier is `AverCert.Artifact.certificate : AverCert.AcceptedArtifact.accepted AverCert.Artifact.data`, where `accepted` conjoins: `Schema.Holds` of the manifest; `subjectMatchesArtifactRoot`; `fragmentClaimObligationsInManifest` (every claim's obligation literal is found in the manifest by export name); `claimsMatchManifest` (the claim-side plan pairs equal the manifest plan lists, family by family, with the sym-plan encoder output equated to `exprFragmentPlans`); `StandardFace.checkedFaces` (section 5.1); `ClaimAxes.checked = true` (section 5.2); `decodedNonExprFacts` (module-wide host-role and string-role decodes plus per-claim code/carrier/struct-field decodes recomputed from bytes); and `acceptedFragments` (per-family byte acceptance plus the whole-module accounting of section 5.3).

### 5.1 StandardFace: the admitted semantic face

`StandardFace.checkedFaces` requires, per claim family, that the obligation's carrier, `Dom`, `Cod`, `domRepr`, `codRepr`, complete host builder, and (for reconstructible families) model equal the standard face selected by the checked family and plan. Faces bind the whole host function, not probe points, so an unmentioned input cannot turn a contract into a trap. Every role/index pair a claim cites must agree with the byte-decoded role table (`hostTableBound`), with pairwise-distinct indices. `claimExportsUnique` requires that one export is claimed by exactly one family across all families. User-ADT domain meaning, representation interpretation, and models that cannot be reconstructed from Wasm remain explicit read declarations: for those faces only the reconstructible parts are forced, and the theorem is conditional on the declared meaning (see section 10).

`StandardFace.reportEntries` derives the public `(export, class)` report pairs in manifest-obligation order from the checked claims — the class labels of section 4.2 are outputs of this function, and the checker witness pins the JSON report to them.

### 5.2 ClaimAxes: policy, termination, totality, contracts

`ClaimAxes.checked` recomputes, from the checked plans alone, the axis triple (policy, termination witness, totality role) each obligation must carry — every non-recursive family is forced to the partial axis; recursion families are forced to total with the role classified from the byte-bound plan shape — and the exact runtime-contract set the manifest must disclose. A totality claim can therefore never be smuggled in through a JSON label, and a contract can never be omitted from the disclosure.

### 5.3 Whole-module accounting

`acceptedWholeModule` conjoins byte-derived checks over the entire module: `moduleFramingValid` (strict section framing); `exportsAccounted` and `importsWithinCapabilities` and `startAccounted` (section 4.1); and `closureIsolation` — the direct-call closure of all certified roots, recomputed from the code section by an in-kernel scanner, must equal the declared root/helper partition, with no imports reachable, no instruction channels outside the certified profile, and no shared memory declared. `manifestObligationsClaimed` and `manifestObligationExportsUnique` close the coverage direction: every manifest obligation is claimed by some family and obligation export names are pairwise distinct, so an unclaimed obligation cannot ride into the accepted artifact unchecked.

## 6. Plan grammar and the canonical byte-lowering contract

Plans are the producer's untrusted explanation of *why* a function body has its certified meaning. They are serialized once, as Lean values in `Plans.lean` (namespace `AverCert.Plans`), and referenced by the manifest plan lists and the artifact claims. There is no JSON plan AST in the package; a JSON projection would be a second authority that must agree with the first, so it is deliberately absent.

### 6.1 Plan profiles

Every raw-plan structure carries a `profile` string that MUST match the value its checker expects:

| Profile string | Lean type | Family |
|---|---|---|
| `sym-fragment-v1` | `SymRawPlan` | Source-level symbolic plan (SymPlan), the portable source-meaning IR |
| `expr-fragment-v1` | `ExprFragmentRawPlan` | Representation-level ANF fragment |
| `recursion-plan-v1` | `RecursionRawPlan` | Unary / accumulator integer fuel-recursion |
| `mutual-plan-v1` | `MutualRawPlan` | One member of a mutually-recursive integer SCC |
| `composition-plan-v1` | `CompositionRawPlan` | Cross-function direct-call composition shape |
| `verbatim-plan-v1` | `VerbatimRawPlan` | Verbatim `ref.test`-dispatch (`Cod := WVal`) |
| `int-dispatch-v1` | `IntDispatchRawPlan` | Int-valued ADT dispatch (`Cod := Int`) |
| `string-eq-v1` | `StringEqRawPlan` | One-literal `String.eq` match |
| `string-concat-v1` | `StringConcatRawPlan` | Literal-affix `String.concat` |
| `construct-v1` | `ConstructRawPlan` | ADT constructor `struct.new` witness |
| `field-projection-v1` | `FieldProjectionRawPlan` | Bare tuple/record field projection |

### 6.2 SymPlan: the source-level grammar

`SymRawPlan` is `{profile, params : List SymTy, result : SymTy, body : SymBlock}`. A `SymBlock` is an ordered ANF node list plus a `result` node id; each `SymNode` is `{id, ty : SymTy, kind}` where `id` must equal the node's position in the block (checked before lowering). `SymTy` is `int | float | bool | string | named name | app1 name arg | app2 name left right` — deliberately with no raw-`WVal` escape hatch. `SymNodeKind` is: `param index`, `constBool`, `constInt`, `constFloatBits`, `constStringBytes`, `prim op args` (with `SymPrim` = `floatAdd | floatMul | floatLe | intAdd | stringEq | stringConcat`), `construct typeName ctorName args`, `emptyList elemTy`, `projectField typeName field fieldTy value`, `intConstCmp op value constant` (with `SymIntCmp` = `eq | lt | le | ge`), and `ifElse cond thenBlock elseBlock`.

A SymPlan carries no wasm indices. The audited in-wall encoder `PlanCheck.encodeSymRawPlanToExprFragmentRawPlan` maps it to an `ExprFragmentRawPlan` **under the byte-derived host-role and struct tables** — plan-supplied indices can never enter the encoder — and acceptance equates the encoder output with the manifest's `exprFragmentPlans` entry, so the source-level claim and the byte-level plan cannot drift apart.

### 6.3 Fragment IR: the representation-level grammar

`ExprFragmentRawPlan` is `{profile, params : List FragTy, result : FragTy, body : FragBlock}` with the same positional-id ANF discipline. `FragTy` is `f64 | boolI32 | intCarrier | i64 | rawI32 | ref | adtRef`; `FragTy.sourceTy?` projects only `f64/boolI32/intCarrier` to source types, so raw limbs cannot silently acquire source meaning. `FragNodeKind` is: `local index`, `constBool`, `constI64`, `constI32`, `constF64Bits`, `structGet field receiver` (Int-carrier limb reads), `structGetUser tyIdx field value` (whole user-struct projection; the type index is node data bound to the bytes, mirroring `hostCall`), `refIsNull`, `prim op args` (with `FragPrim` = `f64Add | f64Mul | f64Le | i64Eq | i64LeS | i64LtS | i64GeS | i32LtS | i32GtS`), `hostCall role funcIdx args` (with `HostRole` = `box | add | mul | sub`; the resolved index is bound both to the bytes and to the decoded role table), `selfCall tail funcIdx args` (recursion families only; bound to the decoded self index), and `ifElse`.

The dispatch-family grammars (`verbatim-plan-v1`, `int-dispatch-v1`) are separate because their multi-use scrutinee spills through scratch locals, which pure ANF cannot express; their leaf/cascade constructors, and the remaining per-family plan structures, are defined in `SchemaCore.lean` and are the normative reference for field-level detail. Two representative invariants: `IntDispatchRawPlan` arms name host helpers by **role** only (the byte-derived role table parameterizes the lowerers, so a plan cannot cite an index), and scratch-local numbering is a fixed function of the arm count rather than plan data.

### 6.4 The canonical byte-lowering contract

The load-bearing invariant of the whole format is: **the certified function's bytes are the canonical lowering of its checked plan, and both sides of that equality are computed inside the Lean kernel.** For every accepted plan the wall establishes, by definitional reduction (`rfl`):

1. **Structural check**: the family checker in `PlanCheck` accepts the raw plan (`checkSymRawPlan`, `checkExprFragmentRawPlan`, `checkRecursionRawPlan` + the context-sensitive `checkRecursionPlanShape` against the byte-derived self index and role table, `checkMutualRawPlan` + `checkMutualPlanShape` against the byte-derived SCC member set, `checkVerbatimPlan`, `checkIntDispatchRawPlan`, `checkStringEqRawPlan`, `checkStringConcatRawPlan`, `checkConstructRawPlan`, `checkFieldProjectionRawPlan`, `checkCompositionRawPlan`).
2. **Semantic lowering**: `PlanLower.lower*Body` maps the plan to the exact `CertPrelude.WInstr` body literal that `Module.lean` carries and the obligation's `code` table evaluates — so the proved simulation is about precisely the lowered semantics.
3. **Byte lowering**: `PlanBytes.lower*CodeEntry` maps the plan to the exact Wasm code-entry byte sequence (local declarations + expression body + ULEB128 body-size prefix; ULEB/SLEB encoders are defined in-wall). This is a plan-first byte **encoder** for the checked profiles, not a general Wasm assembler.
4. **Byte-origin binding**: `WasmSlice.exactFuncBindingForExport modBytes modLen exportNameBytes codeEntry` finds, in the actual artifact bytes, the export with that exact name and requires its code entry to equal the lowered bytes, returning the pinned `FuncBinding {funcIdx, typeIdx, codeEntry}`. Where the family needs signature or type facts, additional byte-derived matchers pin them (`funcTypeMatches`, `projectionStructTypeMatches`, `projectionFuncTypeMatches`, `listConstructStructTypeMatches`, ...).

`Plans.lean` states these equalities as `example : ... := rfl` lines per export, and the acceptance predicates in `AcceptedArtifactCore.lean` / `ExprFragmentAccepted.lean` aggregate the same equalities into the accepted-artifact proof, so deleting an example from `Plans.lean` cannot weaken acceptance. A conforming verifier MUST NOT substitute an out-of-kernel (e.g. Rust-side) reimplementation of any of these four steps on its acceptance path.

## 7. Byte binding

Three mechanisms bind the certificate to one exact artifact:

**Hash pinning.** The verifier MUST read the caller-supplied `.wasm`, compute its SHA-256, and reject unless it equals `wasm_sha256`. The witness pins `manifest.subject.artifactHash` to the recomputed hash string, and `Schema.Holds` conjoins `artifactHash = CertModule.wasmSha256`, so the JSON envelope, the Lean manifest, the packaged `Module.lean`, and the actual bytes must all agree.

**ArtifactBytes injection.** The verifier MUST generate `ArtifactBytes.lean` itself from the bytes it read — the package has no opportunity to supply a different numeral. The encoding is a single little-endian natural: `modBytes = Σ bytes[i] · 256^i` (rendered as one hex numeral with the byte sequence reversed), plus the explicit `modLen = <byte count>`. The length is soundness-relevant because trailing `0x00` bytes are not represented in the numeral. Every in-kernel decode (`CertDecode`, `WasmSlice`) reads from this pair using shift/mask arithmetic; the witness pins `Artifact.data.modBytes` / `Artifact.data.modLen` to the checker-generated module's values, so the packaged claim data provably talks about the injected bytes.

**Wall identity.** `format.wall_id` names the exact audited Lean source set (plus toolchain) the proof must elaborate against, per section 1. The verifier materializes the embedded wall sources into the build directory itself; packaged files with wall-source names are ignored. Changing any audited wall source or the toolchain pin changes the identity, and a certificate naming an unknown identity MUST be rejected.

## 8. The checker witness, the axiom whitelist, and the fail-closed guard

After the package builds, the verifier authors `CheckerWitness.lean` — never accepted from the package — containing, in order: `rfl` pins equating `Artifact.data.modBytes/modLen/manifest` with the checker-generated `ArtifactBytes` values and `AverCert.manifest`; `rfl` pins of every kernel-pinned manifest field of section 4 (`artifactHash` to the recomputed hash, `artifactRoot`, obligation export names, `subject.exports`, `StandardFace.reportEntries` to the exact `(name, class)` pair list, policies, termination witnesses, contracts, `declaredUncertified`, `capabilities`, `start`, `hostRoleTable` — rendered as `none` or `some {box, add, mul, sub}` mirroring the JSON — `stringHostRoles`, `profile`, `abi`); the theorem `AverCertChecker.checked : AverCert.AcceptedArtifact.accepted AverCert.Artifact.data := AverCert.Artifact.certificate`, which forces the packaged root to exist at exactly the accepted type; and finally the axiom guard.

The axiom guard is a checker-authored `run_cmd` that collects the axiom closure of `AverCertChecker.checked` via `Lean.collectAxioms` and throws unless every axiom is in the whitelist `[propext, Classical.choice, Quot.sound]`. The guard is fail-closed by construction: it runs at elaboration of the witness, any thrown error fails the build, and a failed build is a rejected certificate. The package cannot carry a competing or defanged guard because `run_cmd` (and every other elaboration-executing token) is banned from package files by the staging scan of section 9 — only the checker can author executable elaboration code, and it authors exactly this guard. A conforming verifier MUST enforce the same whitelist on the axiom closure of the accepted root and MUST treat any additional axiom — including `sorryAx` — as rejection.

## 9. The verification pipeline

A conforming verifier MUST execute the following stages in order and MUST reject (nonzero exit, no CERTIFIED output) on the first failure. Stage numbers reference the reference implementation in `aver-cert/src/verifier.rs`.

1. **Wasm validity gate.** Read the artifact bytes and run a complete standard WebAssembly validator over them (`wasmparser::Validator::validate_all` in the reference). This is the one retained Rust semantic gate: the Lean wall decodes every trust-bearing section and instruction it consumes, but it is not a complete Wasm validation algorithm (stack/control typing included), so artifact acceptance is only stated over validator-accepted modules.
2. **Manifest parse and envelope checks.** Compute SHA-256 of the bytes; parse `cert-manifest.json` strictly; require `schema_version = 2`, `wasm_sha256` equal to the recomputed hash, `format.version = 1`, `format.wall_id` resolving to an embedded wall, and `artifact_certificate_root = "AverCert.Artifact.certificate"`; then parse the candidate fields of section 4 with the exact-object and policy/termination coupling rules and the printable-ASCII candidate gate.
3. **Build-directory assembly.** Create a fresh private build directory (mode `0700` on Unix, under a checker-selected temp root).
4. **Staging of package data.** For each regular file directly in the cert directory (subdirectories and non-files are skipped): ignore it unless its name ends in `.lean`; silently skip checker-owned names (`ArtifactBytes.lean`, `lakefile.lean`, `CheckerWitness.lean`, and every wall source name).
5. **Name sanitation and shadow rejection.** The module root MUST match `^[A-Za-z][A-Za-z0-9_]*\.lean$`. Reject any root that case-insensitively collides with a toolchain root (`Init`, `Lake`, `Lean`, `Std`), a wall source root, or `ArtifactBytes`/`CheckerWitness`/`lakefile`.
6. **Elaboration-code scan.** Reject any staged file whose text contains any of the twenty tokens `#eval`, `run_cmd`, `run_elab`, `run_tac`, `initialize`, `builtin_initialize`, `macro`, `macro_rules`, `elab`, `elab_rules`, `syntax`, `notation`, `unsafe`, `implemented_by`, `extern`, `deriving`, `attribute`, `@[`, `«`, `open Lean`. The check is a plain substring scan over the whole file — comments included — and is deliberately overbroad: package Lean files are data and definitions only.
7. **Checker-owned materialization.** Write the embedded wall sources, the generated `ArtifactBytes.lean` (section 7), a checker-authored `lakefile.lean` whose roots are the sorted, deduplicated staged+wall module roots, and the pinned `lean-toolchain`.
8. **Hermetic Lean build.** Run `lake build` through the pinned toolchain: the canonical Elan installation (`ELAN_HOME` or `~/.elan`) is the single bootstrap trust anchor, invoked by absolute path as `elan run --install <pinned-toolchain> lake ...` with a **cleared environment** (no inherited `LEAN_PATH`/`LEAN_SRC_PATH`/`ELAN_TOOLCHAIN`/`PATH`/`HOME`), implicit Lake artifact caches disabled, and `TMPDIR`/`TMP`/`TEMP` redirected into a checker-owned directory. Build caches are opt-in only (`AVER_CERT_DATA_CACHE`, keyed on schema version, pinned hash, wall id, and toolchain; `AVER_CERT_PRELUDE_CACHE` for the artifact-independent wall prefix) and are trusted local state; a cache-assisted build that fails is retried once from clean before rejecting.
9. **Witness elaboration.** Author `CheckerWitness.lean` (section 8) and elaborate it (`lake env lean -o ... CheckerWitness.lean`). This runs on **every** invocation, outside any cache, so the manifest pins and the axiom guard can never be replayed from stale build products. Failure MUST be reported as the certificate not binding to this artifact.
10. **Fresh whole-closure kernel replay** (`verify` only). Run `lake env leanchecker --fresh CheckerWitness`: the pinned toolchain's `leanchecker` re-checks the witness module and its entire import closure in a fresh declaration environment, so nothing is inherited from the elaboration environment of stage 9. The developer preflight `check` omits exactly this stage, trusts the freshly built or explicitly cached `.olean` closure, and MUST report `CHECKED`, never `CERTIFIED`; it MUST NOT be used as a release or admission gate.
11. **Verdict mapping.** Any failure above → `DECLINED` with a reason, nonzero exit. Zero certified exports with everything passing → the **admission-only** verdict: the banner `NO CERTIFIED EXPORTS (admission only, no behavioral claims)` and a nonzero exit — a package that proves nothing about any export MUST NOT exit successfully, even though its whole-module accounting held. Otherwise → `CERTIFIED`, exit zero, printing the artifact path, export count, level (`L1` / `L3` / `mixed L1/L3` computed from the pinned policies), and one line per export containing only kernel-pinned facts: name, policy, and the class label mapped from the pinned `reportEntries`. The verdict IS the exit code; `explain`/`inspect` run the same strict check and additionally print declared-only values under explicit labels.

## 10. Trust model

What a positive verdict does **not** trust:

- **The Aver compiler and its certificate producer.** The classifier, disassembler, rederiver, and Lean renderer in `aver-cert`'s producer feature are diagnostics for emission; none of them is linked into or re-run on the verifier's acceptance path. A verifier reimplementation needs none of that code.
- **Manifest prose.** Everything declared-only in section 4 — `dom`/`cod` strings, `level`, `theorem`, `wasm`, `carrier_type_index`, `source_level_only`, and every human-readable reason string — can lie without affecting the verdict, and MUST be presented as declarations, not findings.
- **Any Rust-side reconstruction of the claim.** The reference verifier performs no parallel verdict computation: it does not disassemble the module, reconstruct obligations, or compare classifications. Rust owns transport and process orchestration; the facts come from the kernel.
- **Package-supplied build infrastructure.** Wall sources, lakefile, toolchain pin, artifact bytes, and witness are checker-owned; packaged files with those names are ignored or rejected.

What a positive verdict does trust — the honest TCB:

- **The Lean kernel and toolchain** (pinned Lean 4.32 elaborator, kernel, Lake, and `leanchecker`), resolved through the local Elan installation as bootstrap trust anchor. `leanchecker --fresh` is a fresh-environment replay from the same distribution, not an independently implemented second kernel; the architecture MUST NOT be described as having two diverse kernels.
- **The embedded wall sources** — the statement schema, decoders, checkers, lowerers, faces, axes, and soundness/discharge theorems of sections 5–6 are audited Lean code; their identity is what `wall_id` pins. The schema of claims is part of the TCB: a certificate is only as meaningful as `Obligation.holds`/`holdsTotal` and the acceptance conjuncts.
- **The Rust transport harness** — file reading, hashing, JSON envelope checks, staging gates, hermetic process construction, and verdict mapping (`verifier.rs`, `wall.rs`, `lean_process.rs`, `format.rs`, `main.rs`).
- **`wasmparser`** for full WebAssembly validity (stage 1 rationale).
- **SHA-256 collision resistance**, for both the artifact hash and the wall identity.
- **The named runtime contracts** — the L1 theorems are conditional on the host helpers obeying the disclosed laws, and the L3 theorems additionally on the disclosed totality; proving the shipped runtime satisfies them is a per-toolchain-release obligation outside the certificate.
- **Explicit source read declarations** — user-ADT domain meaning, representation interpretation, and non-reconstructible models (section 5.1) are conditions of the theorem, not derived facts.
- **Explicitly configured cache directories**, if any, as trusted local state.

## 11. Appendix: section-to-source traceability

| Section | Authoritative sources (repository paths) |
|---|---|
| 1 Versioning, wall identity | `aver-cert/src/format.rs` (`FORMAT_VERSION`, `CERT_SCHEMA_VERSION`, `CURRENT_WALL_ID`, `ARTIFACT_CERTIFICATE_ROOT`), `aver-cert/src/wall.rs` (`compute_id`, `current_id`, `resolve`, `SOURCES`, `LEAN_TOOLCHAIN`), `aver-cert/assets/wall/current/lean-toolchain` |
| 2 Package layout | `aver-cert/src/engine/render_project.rs` (`write_project`, `sanitize_model_for_cert`, `render_artifact_certificate`, `render_artifact_soundness`), `aver-cert/src/engine/render_manifest.rs` (`render_final`), `src/main/commands.rs` (certify entry, model emission reuse), `tests/cert_certify_spec.rs` |
| 3, 4 Manifest schema, pinned vs declared | `aver-cert/src/engine/render_manifest.rs` (`render_manifest`, `render_manifest_lean`), `aver-cert/src/verifier.rs` (`trusted_check`, `read_candidates`, `parse_termination`, `exact_object_fields`, `gate_candidate`, `report_face`, `manifest_face`, `explain`), `aver-cert/src/engine/mod.rs` (`PROFILE_ID`, `RUNTIME_ABI`, `CERT_LEVEL`, contract strings), `tests/snapshots/cert_certify_spec__add_one_certificate_package.snap` (worked example) |
| 4.3 Three-state host-role decode | `aver-cert/assets/wall/current/CertDecode.lean` (`AddSub.Roles`, `roleTable`, `carrierHelperAbsent`, `roleTableStrict`, `StringHost.roleTable`), `aver-cert/assets/wall/current/AcceptedArtifactCore.lean` (`decodedHostRoleTable`, `decodedStringHostRoles`), `aver-cert/assets/wall/current/SchemaCore.lean` (`Subject`, `Subject.hostRoles`) |
| 5 Obligations, policies, acceptance | `aver-cert/assets/wall/current/SchemaCore.lean` (`Obligation`, `holds`, `holdsTotal`, `HoldsCore`, `Policy`, `TotalityRole`, `TerminationWitness`, `checkTerm`, `checkTermMutual`, `CAPABILITY_REGISTRY`), `aver-cert/assets/wall/current/Schema.lean` (`Holds`), `aver-cert/assets/wall/current/AcceptedArtifact.lean` (`accepted`), `aver-cert/assets/wall/current/AcceptedArtifactCore.lean` (`ArtifactData`, coverage/accounting/closure defs), `aver-cert/assets/wall/current/StandardFace.lean` (`checkedFaces`, `reportEntries`, `hostTableBound`), `aver-cert/assets/wall/current/ClaimAxes.lean` (`checked`, `canonicalTermination`) |
| 6 Plan grammar, byte-lowering contract | `aver-cert/assets/wall/current/SchemaCore.lean` (all raw-plan types and node grammars), `aver-cert/assets/wall/current/PlanCheck.lean` (checkers, `encodeSymRawPlanToExprFragmentRawPlan`, profile strings), `aver-cert/assets/wall/current/PlanLower.lean`, `aver-cert/assets/wall/current/PlanBytes.lean` (ULEB/SLEB, `lower*CodeEntry`), `aver-cert/assets/wall/current/WasmSlice.lean` (`FuncBinding`, `exactFuncBindingForExport`, type matchers, closure scanner), `aver-cert/src/engine/render_project.rs` (`render_expr_fragment_plans` — the emitted `rfl` surface) |
| 7 Byte binding | `aver-cert/src/verifier.rs` (hash checks, `assemble_build`), `aver-cert/src/wall.rs` (`render_artifact_bytes`), `aver-cert/src/engine/render_project.rs` (`render_artifact_bytes_lean`, byte-numeral comments), `aver-cert/assets/wall/current/WasmSlice.lean` (byte-cursor model) |
| 8 Witness and axiom guard | `aver-cert/src/verifier.rs` (`checker_witness`, `AXIOM_WHITELIST`, `CHECKED_ROOT`) |
| 9 Pipeline | `aver-cert/src/verifier.rs` (`verify`, `check`, `trusted_check`, staging and scan functions, `CODE_EXEC_TOKENS`, `FRESH_REPLAY_ARGS`), `aver-cert/src/lean_process.rs` (hermetic subprocess boundary), `aver-cert/src/cache.rs`, `aver-cert/src/prelude_cache.rs` (opt-in caches), `aver-cert/src/main.rs` (verdict/exit mapping, banners), `src/main/cli.rs` (`aver cert` subcommands) |
| 10 Trust model | `aver-cert/src/verifier.rs` (module header, validator comment), `docs/certification.md`, `docs/certification-architecture.md`, `aver-cert/src/format.rs` (`WASM_GC_CAPABILITIES`), `aver-cert/src/lean_process.rs` (trust anchor and environment tests) |
