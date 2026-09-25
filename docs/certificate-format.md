# Aver Artifact Certificate Format Specification

This document specifies the Aver artifact certificate format: the `cert/` package that `aver compile --target wasm-gc --certify` (or `--target wasip2 --certify`) writes, and what a verifier must do to accept it. It also holds the trust inventory (section 12) and the versioning policy (section 13). It is written for someone reimplementing the verifier. The reference implementation is the standalone `aver-cert` crate. The document separates what acceptance requires, which is normative, from what the producer happens to emit, which a verifier must not rely on. Places where the reference implementation enforces less than a reader might expect are marked *known gap*. Section 14 maps each section to its source files. [certification.md](certification.md) is the user guide and [certification-architecture.md](certification-architecture.md) explains the architecture.

The key words MUST, MUST NOT, SHOULD and MAY are normative. A value is *kernel-pinned* when the reference verifier proves its equality inside the Lean kernel, either by `rfl` in the checker-authored witness or as a conjunct of the accepted-artifact proposition; a verifier MUST NOT accept a package for which a pinned equality fails. A value is *declared-only* when it is transported for display and is not part of the verified claim; a verifier MUST NOT present it as a verified fact.

> **TODO-decision: format name.** The format has no frozen public name. A short stable name (for registries, media types, file signatures) is an open decision and MUST be settled before the format is declared frozen.

## 1. Versioning and identity

Three identities govern acceptance:

| Identity | Current value | Where it lives | What it versions |
|---|---|---|---|
| Package layout version | `1` (`FORMAT_VERSION`) | `cert-manifest.json` `format.version` | The `cert/` directory layout and the transport envelope |
| Statement schema version | `9` (`CERT_SCHEMA_VERSION`) | `cert-manifest.json` `schema_version` | The statement: manifest fields, the plan grammar, the obligation shape |
| Wall identity | `sha256:<64 lowercase hex>` | `cert-manifest.json` `format.wall_id` | The exact checker-owned Lean wall plus its pinned Lean toolchain |

A verifier MUST reject a package whose `schema_version` is not exactly the version it implements, whose `format.version` is not exactly `1`, or whose `format.wall_id` does not name a wall embedded in the verifier. There is no version negotiation, no downgrade path, and no filesystem, environment or network fallback for resolving a wall.

Schema history, one line per bump. Each bump is exact-match, so a verifier of version N rejects every package of version N-1.

- 2: the subject's `hostRoleTable` became optional; `null` is pinned to a byte-derived proof that the module exports no `__rt_aint_from_i64` function.
- 3: `hostRoleTable` gained the `toIndex` key.
- 4: `hostRoleTable` gained the `cmp` and `eq` keys.
- 5: the manifest gained the `target` field, and `Schema.Holds` checks target, profile and ABI against wall constants.
- 6: `target = "wasip2"` with the component envelope of section 4.4.
- 7: the `laws` array (law-claims).
- 8: the `sourceBridges` array and the `bridges` key of every law entry.
- 9: every obligation is stated over one plan grammar. The Lean manifest carries one plan list (`fnPlans`), each entry a function's optimized MIR body printed 1:1, and a declared type layout (`types`) that the wall confirms against the type and data sections. The obligations are exactly the ones the wall derives from the plans, and their model is the plan's fuel-indexed meaning. Every certified export reports the class `source-plan-v1` with wall-derived facets. `hostRoleTable` gained the `divmod` key. Source-bridges are restated over the plan grammar, in two statement kinds (section 9).

The wall identity is computed, not assigned. It is the SHA-256 of a domain-separated, sorted, length-framed encoding of every wall source file plus the toolchain pin, written as `sha256:` and 64 lowercase hex digits. The encoding is: the ASCII bytes `aver-certificate-wall\0v1\0`; the file count as a big-endian `u64`; then, for each file in ascending filename order, the filename length as a big-endian `u64`, the filename bytes, the contents length as a big-endian `u64`, and the contents. The file set is the 24 embedded `.lean` wall sources plus one synthetic file named `lean-toolchain`, whose contents are the embedded toolchain file verbatim: the ASCII bytes `leanprover/lean4:v4.34.0` and one trailing newline. The newline is hashed; a reimplementation that hashes the trimmed pin computes a different identity. The current embedded wall identity is `sha256:ed89b143414bdff0bfadb49a49bc1e7d8c537365b69549c65f7e82fbccf73cef` (`CURRENT_WALL_ID` in `format.rs`). The reference verifier recomputes the digest over its embedded sources on first use and aborts if it differs from the compiled-in constant. A reimplementation MUST resolve `format.wall_id` only against source sets whose recomputed identity matches byte for byte, never by name, path or prefix.

> **TODO-decision: freeze criteria.** Neither `format.version = 1` nor `schema_version = 9` is frozen. What counts as a compatible extension, and whether a frozen schema admits additive optional fields, is open. Until a freeze, every schema change bumps `schema_version`, and verifiers reject non-matching versions exactly.

> **TODO-decision: wall registry policy.** The reference verifier embeds exactly one wall. Whether a release may embed several walls, for a grace window on older packages, is open; section 13 describes the re-certification this forces today.

## 2. Package layout

### 2.1 What the producer emits (convention, not acceptance)

A package is one directory, conventionally `cert/`, next to `<name>.wasm`. The producer deletes any existing `cert/` first. Except for `cert-manifest.json`, no row below is an acceptance requirement; section 2.2 states the file-level contract.

| File | Role |
|---|---|
| `cert-manifest.json` | Transport and report envelope (section 4). The only non-Lean file. |
| `Plans.lean` | `AverCert.Plans.types : TypeTable` and one `FnPlan` per planned function, collected in `AverCert.Plans.fnPlans : List FnEntry`. |
| `Manifest.lean` | `AverCert.subject` and `AverCert.manifest`, whose `obligations` field is literally `AcceptedArtifact.obligationsOf subject Plans.types Plans.fnPlans`. |
| `ArtifactLayout.lean` | The declared module layout (`DeclaredLayout.Layout`): the imported function count; for every defined function its type index and the byte offset and length of its code entry, as packed tables; the exact function type of every planned function; every plan entry's name, as characters, with its export position; and the section cuts, the byte length of every top-level entry of the type section, of every export and of every code entry. Producer data: `types_cut`, `exports_cut` and `code_cut` confirm the cuts and `layout_ok` the layout against the staged bytes (section 7.7). |
| `ArtifactHostRoles.lean` | Carriered modules only: one `decide +kernel` theorem per Int helper role, each proving `arithRoleCheck` for that role. |
| `ArtifactPlans.lean` | `plans_all`: every plan passes `entryAccepted`, proved 32 plans per `decide +kernel` declaration and chained. A package with more than 32 plans puts each chunk in its own `ArtifactPlans<k>.lean` over `ArtifactPlanCheck.lean`. |
| `Artifact.lean` | `AverCert.Artifact.data : ArtifactData` and the byte facts of acceptance: `plans_ok`, `roles_ok`, `strings_ok`, `axes_ok`, `framing_ok`, `exports_ok`, `imports_ok`, `start_ok`, `closure_ok`, `whole_ok`, `envelope_ok`. A package with more than 32 plans moves the data and the heaviest facts to `ArtifactData.lean`, `ArtifactStrings.lean`, `ArtifactClosure.lean` and `ArtifactInterface.lean`, which Lake can build in parallel. |
| `Final.lean` | `theorem AverCert.Final.cert : AverCert.Schema.Holds manifest`, proved by `AcceptanceSoundness.accept_sound` from `plans_ok`. |
| `ArtifactCertificate.lean` | The public root `theorem AverCert.Artifact.certificate : AverCert.AcceptedArtifact.accepted data`, followed by `#print axioms`. |
| `AverModel/**.lean` | The `aver proof` Lean model of the source, in certificate mode. Shipped only when the package declares a source-bridge or a law-claim. |
| `BridgeDefs.lean`, `BridgeSteps<i>.lean`, `BridgeProof.lean`, `Bridge.lean` | Source-bridge proofs (section 9). Emitted only when `sourceBridges` is nonempty. |
| `Laws.lean` | One corollary per `laws` entry (section 9). Emitted only when `laws` is nonempty. |

Model files ship under the reserved directory `AverModel/`, whatever the Aver module is called, so no model root can equal or prefix a package, wall or toolchain root. The Lean namespaces inside stay as emitted. Certificate mode differs from `aver proof` export in three ways the staging gates require: the prelude pieces the token scan refuses (`AverBits` with its `@[simp]` equations, and the `syntax`/`macro_rules` tactic `aver_int_order`) are imported from the wall module `ModelPrelude` instead of emitted; a type keeps only the `deriving` clauses stage 7 admits and states its `Inhabited` instance explicitly; and every theorem is prefixed with `#guard_msgs (drop error) in`, so a proof that fails (a `maxHeartbeats` timeout included) leaves `sorryAx` for the axiom audit to find instead of failing the build. The wrapper changes no declaration and admits nothing the kernel did not check. Before shipping, the producer runs the checker's own file-name, case-collision and token rules (`aver-cert/src/lean_gate.rs`) over every model file; a model that would fail them is not shipped, and every bridge and law-claim is declined with the reason.

The producer never emits, and MUST NOT emit, `ArtifactBytes.lean`, `ArtifactComponentBytes.lean`, `Module.lean`, `CheckerWitness.lean`, `CheckerAudit.lean`, `lakefile.lean`, `lean-toolchain`, any wall source, any build cache, or plan sidecars (`*.plan`, JSON plan trees).

### 2.2 What acceptance requires of the file set

Acceptance does not depend on layout. The staging rules of section 11 (stages 5 to 7) are the whole file-level contract. Every regular file directly in the package directory whose name ends in `.lean` (case-sensitively) is staged, unless its name is checker-owned, in which case it is ignored. A `.lean` file in a subdirectory is staged at its relative path only when the staged top-level `Manifest.lean`, `Certificate.lean`, `Bridge.lean` or `Laws.lean` has an `import` line naming its dotted module name. That admission list is written by the untrusted producer, so it limits the build set and is not a security boundary: every staged file passes the same gates, and the verdict rests on the checker-authored witness.

Consequences a reimplementor MUST get right:

- There is no closed file set. Extra `.lean` roots are staged and built like any other package file, and a missing conventional file is not itself an error. The verdict depends on whether the staged set elaborates and the root holds at its pinned type.
- `Plans.lean` is not special. The witness imports `AcceptedArtifact`, `ArtifactBytes`, `Manifest`, `Artifact` and `ArtifactCertificate`, plus `Laws` when `laws` is nonempty and `Bridge` when `sourceBridges` is nonempty. The plans are whatever `AverCert.manifest.fnPlans` evaluates to, wherever the package defines them.
- Every staged `.lean` file is untrusted data. It affects the verdict only through the kernel accepting the witness.

A module root that case-insensitively equals or has a dotted prefix equal to a toolchain root, a wall root or a checker-owned root is rejected (stage 6). Two staged paths equal ASCII-case-insensitively are rejected. *Known gap:* the `.lean` suffix test is case-sensitive, so `ArtifactBytes.LEAN` is ignored rather than rejected.

## 3. Trust vocabulary

The JSON manifest is a transport envelope. The authoritative statement is the Lean value `AverCert.manifest` together with `AverCert.Artifact.data`, and the checker witness pins the JSON's trust-bearing fields to that Lean data by `rfl` (section 10). Every JSON field is therefore one of:

- **Kernel-pinned**: the witness or the accepted-artifact proposition states an equality between the JSON-derived value and the Lean data, and the wall binds that Lean data to the bytes. Changing the JSON, the Lean data or the bytes on its own makes elaboration fail.
- **Declared-only**: transported for reporting. The verifier either never reads it on the acceptance path or prints it with an explicit "declared" label. The CERTIFIED and CHECKED report MUST print only kernel-pinned facts; `explain` MAY print declared-only values under a label.

## 4. `cert-manifest.json`, schema version 9

The manifest is one JSON object. The reference parser is strict about the fields it reads: a missing or mistyped required field is an error. Strings that the witness interpolates pass a candidate gate first: at most 200 bytes, every byte in `0x20..=0x7E`, and neither `"` nor `\`. The gated strings are each certified export's `name`, `class` and `facets`; every `runtime_contracts` entry; every `declaredUncertified` name and reason; every `capabilities` module and name; `target`; `profile`; and `abi`.

> **TODO-decision: top-level strictness.** Nested objects are matched exactly, but unknown top-level members are ignored by the reference verifier. Whether a frozen schema rejects them is open; producers MUST NOT rely on the leniency.

### 4.1 Top-level fields

| Field | Type | Trust class | Meaning and constraints |
|---|---|---|---|
| `schema_version` | integer | verifier-checked | MUST be exactly `9`. |
| `format` | object `{version, wall_id}` | verifier-checked | `version` MUST be `1`; `wall_id` MUST resolve to an embedded wall. |
| `wasm` | string | declared-only | The artifact file name the producer wrote. The artifact identity is the file passed to the verifier. |
| `wasm_sha256` | 64 lowercase hex | kernel-pinned | SHA-256 of the delivered artifact (the whole component for wasip2). The verifier MUST recompute it and reject a mismatch; the witness pins `subject.artifactHash` to the recomputed value, and `Schema.Holds` requires `artifactHash = CertModule.wasmSha256`, a string the verifier renders into the checker-owned `Module.lean` from the same hash. |
| `target` | string | verifier-checked, kernel-pinned | `"wasm-gc"` or `"wasip2"`. Read before WebAssembly validation to select the preparation path. Pinned to `subject.target`; `Schema.Holds` checks the target and ABI pair. |
| `level` | string | declared-only | `"L1"`, `"L3"` or `"mixed L1/L3"`. The verifier computes its own level from the pinned policies. |
| `profile` | string | verifier-checked, kernel-pinned | MUST be `"AverUserProfile/v1"`. Pinned to `subject.profile` and checked against `expectedProfile`. |
| `abi` | string | verifier-checked, kernel-pinned | `"aver-wasm-gc/0"` for `wasm-gc`, `"aver-wasip2/0"` for `wasip2`. Pinned to `subject.abi`. |
| `final_theorem` | string | declared-only | `"AverCert.Final.cert"`. |
| `artifact_certificate_root` | string | verifier-checked, kernel-pinned | MUST be `"AverCert.Artifact.certificate"`. Pinned to `subject.artifactRoot` and re-checked by `subjectMatchesArtifactRoot`. |
| `carrier_type_index` | integer or `null` | declared-only | The Int carrier struct index. The wall derives the carrier itself (`CertDecode.carrierState`, `TypeTable.carrierConfirmed`). |
| `runtime_contracts` | array of strings | kernel-pinned | Pinned to `subject.contracts`. The wall's `ClaimAxes.contractsMatch` requires exact list equality with the list it derives (section 7.4), so order matters. |
| `laws` | array of objects | kernel-pinned | REQUIRED, may be empty. Section 9.3. |
| `sourceBridges` | array of objects | kernel-pinned | REQUIRED, may be empty. Section 9.2. |
| `sourceBridgesDeclined` | array of `{export, reason}` | declared-only | Certified exports the producer declared no bridge for, with its reason. `explain` prints it under an informational heading. |
| `declaredUncertified` | array of `{name, reason}` | kernel-pinned (names); reasons pinned, not validated | Exports that are not obligations. Pinned to `subject.declaredUncertified`. `exportsAccounted` consumes the names only (section 7.5); each reason is transported prose. |
| `capabilities` | array of `{module, name}` | kernel-pinned | The import section, in order. Pinned to `subject.capabilities`; `importsWithinCapabilities` requires the byte-derived imports to equal it and each pair to be in the target's registry (`WASM_GC_CAPABILITIES`, `WASIP2_CAPABILITIES` with 80 pairs) or in the exact custom-capability namespace. |
| `start` | object `{present, function_index}` | kernel-pinned | `present: false` pairs with `function_index: null`, `present: true` with a `u32`. Pinned to `subject.start` and checked by `startAccounted`. |
| `hostRoleTable` | `null` or object `{box, add, mul, sub, toIndex, cmp, eq, divmod}` | kernel-pinned | Section 4.3. |
| `stringHostRoles` | array of `{function_index, role}` | kernel-pinned | `role` is `"stringEq"` or `"stringConcat"`. Pinned to `subject.stringHostRoles` and bound in the kernel to `CertDecode.StringHost.roleTable` recomputed from the bytes: every defined function is classified, matches are listed in defined-function order, duplicates kept, and the comparison is exact list equality. |
| `certified` | array of objects | mixed; section 4.2 | One entry per certified export, in obligation order. |
| `wasip2ComponentEnvelope` | object | kernel-pinned when `target = "wasip2"` | Required for wasip2, rejected for wasm-gc. Section 4.4. |
| `source_level_only` | array of `{name, reason}` | declared-only | Functions the producer declined, with reasons. `explain` prints it. |

The reference verifier never reads `wasm`, `level`, `final_theorem`, `carrier_type_index`, `certified[].level`, `certified[].theorem`, `sourceBridgesDeclined` or `source_level_only` on the acceptance path, and does not type-check them.

### 4.2 `certified[]` entries

| Field | Type | Trust class | Meaning and constraints |
|---|---|---|---|
| `name` | string | kernel-pinned | The export name. Pinned as `manifest.obligations.map export_`, as `subject.exports`, and as the first component of `ClaimAxes.reportEntries` and `ClaimAxes.reportFacets`. |
| `class` | string | kernel-pinned | MUST be `"source-plan-v1"`; any other value is rejected in Rust. Pinned as the second component of `ClaimAxes.reportEntries`. |
| `facets` | array of strings | kernel-pinned | Pinned to `ClaimAxes.reportFacets`, which the wall derives from the plan and its call group, in the fixed order `recursive`, `mutual`, `calls`, `records`, `variants`, `strings`, `floats`. |
| `policy` | string | kernel-pinned | `"simulatesModel"` or `"simulatesModelTotally"`; anything else is rejected in Rust. Pinned to `manifest.obligations.map policy`, which the wall derives (section 8). |
| `level` | string | declared-only | `"L1"` or `"L3"`. |
| `theorem` | string | declared-only | `"AcceptanceSoundness.fn_claim_discharges"`. Informational. |
| `termination_witness` | object, conditional | kernel-pinned | MUST be absent for `simulatesModel` and present for `simulatesModelTotally`. Shape `{"measure": {"kind": "intNatAbs", "param_index": <u32>}, "descent": <i64>}`. Pinned to `manifest.obligations.map termination?`; the wall only ever derives the canonical witness `{measure := .intNatAbs 0, descent := -1}`. |

### 4.3 `hostRoleTable`: declared and confirmed Int helpers

`hostRoleTable` declares the function index of each Int runtime helper: `box` (`__rt_aint_from_i64`), `add`, `sub`, `mul`, `toIndex` (`__aint_to_index`), `cmp` (`__aint_cmp`), `eq` (`__aint_eq`) and `divmod` (`__aint_divmod`). Its forms are `null`, or an object with exactly those eight keys, each a `u32` or `null`. It maps to `Subject.hostRoleTable : Option CertDecode.AddSub.Roles`.

The Lean manifest also carries `Subject.arithParams`, which has no JSON form: six declared indices (`carrier`, `limb`, `decompose`, `normalize`, `strip`, `umagCmp`) from which the wall synthesizes the helper bodies. Nothing in it is computed from bytes.

`AcceptedArtifact.arithTableCheck` binds the declarations to the bytes. No byte is scanned to discover a role. Exactly two shapes pass, and mixing `null` with an object across the table and `arithParams` fails:

- **Carrierless**: `hostRoleTable` and `arithParams` are both `null`, and `CertDecode.AddSub.carrierHelperAbsent` holds: the export section decodes strictly and has no function export named `__rt_aint_from_i64`. That is all the fact says. It does not prove the module has no Int carrier type or no Int code, and a verifier MUST NOT present it as more.
- **Carriered**: both are present, the `__rt_aint_from_i64` function export exists, and:
  - `CertDecode.carrierState` finds a carrier struct at exactly `arithParams.carrier`, so the declared carrier is confirmed by the type section and not only by bodies the wall synthesized from it;
  - `box`, `toIndex` and `cmp` equal the indices of their named function exports (`null` exactly when the export is absent);
  - `checkArithHostParams` holds: every index is below `2^32`, the range in which the wall's LEB encoders are exact;
  - every non-`null` role's code body equals the template `ArithTemplateDerisk.arithHelperBody` synthesizes for that role from `arithParams`. A `null` role passes vacuously; no plan can call it, because an undeclared index lowers to one no code entry can encode.

`add`, `sub`, `mul`, `eq` and `divmod` are bound by template only. `eq` has no name pin because the emitter exports `__aint_eq` only when user code marks it live, while an Int literal match calls it anyway. `box`, `toIndex` and `cmp` carry both pins, and a reimplementation MUST enforce both: the name equality is the only pin with force on a `null` declaration, and the template equality is the only pin on the code behind an honestly named export.

The template equality fixes which code runs at a role index. It says nothing about what that code computes. The contracts of section 5.2 remain hypotheses of the theorem.

The declared function type of every present role is pinned separately (`roleTypesPinned`, section 7.2).

### 4.4 Wasip2 component envelope

Schema 6 and later admit `target = "wasip2"`, `abi = "aver-wasip2/0"` through the top-level field `wasip2ComponentEnvelope`. It is required for wasip2 and rejected for wasm-gc. It is a length-only object:

```json
"wasip2ComponentEnvelope": {
  "kind": "prefix-core-suffix/v1",
  "prefix_len": 123,
  "embedded_core_module_len": 456,
  "suffix_len": 789
}
```

The verifier splits the delivered component as `prefix ++ embedded_core_module ++ suffix` by those lengths, stages the whole component in checker-owned `ArtifactComponentBytes.lean` and the core in checker-owned `ArtifactBytes.lean`, and the wall's `artifactEnvelopeAccepted` checks that the split core equals the bytes the decoders read. The verifier MUST NOT parse, scan or navigate the component to find the core. The producer's `wit-component` wrapper may find the split while building the package; that is not an acceptance rule.

## 5. The statement

### 5.1 Manifest

The Lean manifest (`SchemaCore.Manifest`) has four fields:

- `subject : Subject`: artifact hash, target, profile, ABI, artifact root, export names, declared-uncertified pairs, capabilities, start, `hostRoleTable`, `arithParams`, `stringHostRoles`, and the contract list;
- `types : TypeTable`: the declared layout (section 7.3);
- `fnPlans : List FnEntry`: every planned function. `FnEntry` is `{name, exported, funcIdx, group, plan}`; `name` is the export name, or `#<funcIdx>` for an internal callee; `group` is the function's call group, callees first;
- `obligations : List Obligation`.

An `Obligation` is `{export_, policy, termination?, totalityRole, carrier, layout, code, host, self, sig, model}`. The acceptance requires the obligations to be exactly `AcceptedArtifact.obligationsOf subject types fnPlans` (`obligationsDerived`), one per exported entry, so no field is a producer choice: `layout` is the lowering context the wall builds from the declarations (`TypeTable.mctxOf`), `code` maps each planned function index to the lowering of its plan, `host` wires the contract functions at their role indices, `sig` is the plan's signature, `model` is the fuel-indexed meaning of the plans (`groupModel`, where a body at fuel `k + 1` runs with every callee at fuel `k`, as the interpreter peels fuel), and the policy axes come from the totality check (section 8).

### 5.2 Denotation

`Obligation.holds` (policy `simulatesModel`, level L1): for every carrier specification `S`, every helper implementation `h` satisfying `HostContracts S h`, every fuel, every list of source values `svs` well-typed at the plan's parameters (`HasTyL`), and every list of wasm values `ws` representing them (`SReprL`), if the interpreter run `wFuncN code (host h) fuel self ws` returns `r`, then the model at that fuel returns some `sv` that `r` represents and that is well-typed at the plan's result. It is vacuous on a trap or on fuel exhaustion.

Representation (`Grammar.SRepr`) reads values off the declared layout: an Int is a canonical carrier (`CanonRepr`: `S.Repr n w ∧ S.Canon w`); a Bool is `i32` 0 or 1; a record is the struct of its type, and a one-field record is its field's value; a variant is the struct of its constructor; `Option` and `Result` are the struct of their instantiation with the tag in field 0 and an arbitrary filler in the unused payload field; a Float is its `f64` bits; a String is the `$string` array of its bytes; a Vector is the array of its elements, fewer than `2^31`; a List is `null` or a `{head, tail}` cons struct; an opaque value is itself.

`HostContracts S h` is the list of named runtime contracts, as hypotheses and never as Lean axioms:

- `add`, `sub`, `mul`: on represented operands, a returned result represents the exact sum, difference or product and is canonical;
- `cmp`, `eq`: on a canonical represented pair, the returned `i32` is the exact three-way sign or equality;
- `stringEq`: the result is byte equality of the two arrays;
- `stringConcat`: the result is the concatenation of the container's string arrays, at the declared result type;
- `toIndex`: the result is the `i32` index, or `-1` outside `[0, 2^31)`;
- `divmod`: on a canonical represented pair with a nonzero divisor and `want_mod` 0 or 1, the result represents Lean's `a / b` (`Int.ediv`) or `a % b` (`Int.emod`, in `[0, |b|)`) and is canonical.

A helper that returns nothing makes its premise vacuous; no contract demands trap-freedom. The box helper is not a contract: the obligation wires the wall's own `boxRef`, the model of the pinned box template.

`Obligation.holdsTotal` (policy `simulatesModelTotally`, level L3): `holds`, and, under `HostTotal` (add and sub return on represented operands, and mul too when `totalityRole = .mul`), every well-typed represented input has an Int first argument `n`, the run at fuel `n.natAbs + 1` returns `r`, and the model at that fuel returns a value `r` represents.

`HoldsCore m` says every obligation satisfies the denotation its policy selects. `Schema.Holds m` adds `m.subject.artifactHash = CertModule.wasmSha256`, `m.subject.profile = expectedProfile` and `artifactTargetAbiAccepted m.subject.target m.subject.abi = true`.

### 5.3 The accepted artifact

The root is `AverCert.Artifact.certificate : AverCert.AcceptedArtifact.accepted AverCert.Artifact.data`, where `ArtifactData` is `{modBytes, modLen, manifest, wasip2ComponentEnvelope, closureFuel, closureClaim}` and `accepted` is the conjunction of:

1. `Schema.Holds artifact.manifest`;
2. `artifactEnvelopeAccepted` over the checker's `ArtifactComponentBytes` (identity for wasm-gc, the length split for wasip2);
3. `subjectMatchesArtifactRoot`;
4. `obligationsDerived`;
5. `plansAccepted` (section 7);
6. `decodedHostRoleTable` (section 4.3);
7. `decodedStringHostRoles`;
8. `ClaimAxes.checked`, which is `contractsMatch` (section 7.4);
9. `acceptedWholeModule` (section 7.5).

`AcceptanceSoundness.accept_sound` proves the `Holds` conjunct from conjuncts 4 and 5 and the hash, profile and target premises, so a package derives `Holds` rather than asserting it. `AcceptanceSoundness.accepted_nonvacuous` shows that every certified export of an accepted artifact has well-typed arguments and an inhabited result type, so no obligation holds only because its hypothesis cannot be met.

## 6. The plan

### 6.1 Grammar

A plan (`Grammar.FnPlan`) is `{sig, nslots, locals, body}`: the signature, the resolver slot count (parameters and every binder), the wasm types of the declared locals after the parameters, and the body. The body is the optimized MIR body (`src/ir/mir/expr.rs`, `MirExpr`) printed 1:1 into `Grammar.Expr`, which keeps MIR's node names. There is no hand-designed IR and no classifier. The printer (`src/codegen/cert/plan_from_mir.rs`) declines a whole function, naming the node, pattern or type, when anything falls outside the admitted subset:

- `literal`: Int in the i64 range, Bool, Float by bit pattern, String by UTF-8 bytes;
- `local slot`, where the slot is the resolver `LocalId`, equal to the wasm local index;
- `let_ binding value body` for a named `Let`;
- `call (.fn idx)`, `call (.builtin b)` for `Bool.and`, `Bool.or`, `Bool.not` and `List.prepend`, `call (.lazy b)` for `Option.withDefault` and `Result.withDefault` (including the fused `Option.withDefault(Vector.get(v, i), <literal>)` and `Result.withDefault(Int.div|Int.mod(a, b), <Int literal>)`), and `call (.intrinsic i)` for Euclidean division or remainder by a nonzero literal;
- `tailCall target args`;
- `binOp` without `Div`: Int arithmetic and the six comparisons, Bool `==` and `!=`, Float comparisons except `!=`, String `+`, `==` and `!=`;
- `neg`, which the grammar has but the producer declines, because the negation helper has no wall template;
- `ifThenElse`;
- `recordCreate` with fields in declared order, and `project`, both for records of two or more fields;
- `match_` with the arm shapes the emitter lowers: an Int literal cascade with a catch-all last, a two-arm Bool match, a two-arm Option or Result match, a user-variant `ref.test` cascade of two or more arms covering every constructor, a String literal cascade with `_` last, and a single-arm flat tuple destructure;
- `construct` for user constructors and `Some`, `None`, `Ok`, `Err`, carrying the node's type;
- `interp` whose parts are all Strings;
- `list t []`, the empty list.

The printer also declines a function that declares effects, uses raw i64 slots, has no MIR body, or whose parameters are not slots `0..n`.

### 6.2 Typing and meaning

`Grammar.tyOf` types a body over the slot environment and the lowering context. It admits only what `GrammarLower` can lower and `GrammarSound` covers, and it rejects arm shapes and operand types outside section 6.1. `planTyped` requires `tyOf` to return exactly `sig.ret` over the parameters, with the slot count consistent with the declared locals. The simulation theorem is false without typing: `struct.new` and the helpers accept operands the source would reject.

`Grammar.eval` gives the source meaning, strict in every argument except the lazy default of `withDefault`. `groupModel` ties calls together by fuel.

### 6.3 Lowering

`GrammarLower` is a port of the wasm-gc MIR emitter (`src/codegen/wasm_gc/body/from_mir/`) for the admitted nodes. One lowering produces both the instruction tree the interpreter runs (`fnCode`) and the code-entry bytes the acceptance compares (`codeEntryBytes`, locals vector and size prefix included), so the proved code and the pinned bytes come from one tree.

Every lowering choice is a function of the plan and the declarations, never a plan flag: an Int comparison against a literal looks for the literal on the left first and flips the operator, re-emits a bare local operand and stashes any other operand in the const-compare scratch local; the scratch locals sit after the resolver slots in the order the emitter reserves them; an `if` takes its block type from the then-branch type; a `tailCall` is `return_call` and a `call` is `call`, as MIR marked them. A reimplementation MUST compute both images inside the kernel and MUST NOT replace them with an out-of-kernel lowering.

An index the declarations do not provide lowers to `TypeTable.absent k`, a value outside the u32 range that no encoder accepts, so a plan citing an undeclared type or helper cannot match any code entry.

## 7. Pins

`plansAccepted` is the conjunction of sections 7.1 to 7.3, evaluated inside the kernel against `ArtifactBytes`.

### 7.1 Code entry

For each `FnEntry` (`entryAccepted`):

- `planTyped` holds;
- the function is found by its plan's own code entry: an exported entry through `WasmSlice.exactFuncBindingForExport` under its export name, an internal callee through `funcBindingByFuncIndex` at `funcIdx`, and either way the module's code entry MUST equal `codeEntryBytes` exactly;
- the bound function index equals `funcIdx`;
- the declared function type equals the plan's signature read through the layout (`sigPinned`);
- `callsOrdered`: every `call` and `tailCall` target is a planned function in the same call group or an earlier one. A call to an unplanned index declines.

`indicesDistinct` requires the role indices and the planned function indices to be pairwise distinct, so every call resolves to one contract or one plan.

`fn_claim_discharges` does not depend on the grouping: it applies `GrammarSound.fn_certified_group` to all plans at once by fuel induction. The grouping only affects L3 (section 8) and `callsOrdered`.

### 7.2 Helper roles

- `decodedHostRoleTable`: section 4.3.
- `decodedStringHostRoles`: section 4.1.
- `roleTypesPinned`: every present role's declared function type is exactly the type its role fixes: `box` `i64 -> carrier`; `add`, `sub`, `mul` `carrier carrier -> carrier`; `cmp` and `eq` `carrier carrier -> i32`; `toIndex` `carrier -> i32`; `divmod` `carrier carrier i32 -> carrier`; String equality `$string $string -> i32`; concatenation `Vector<String> -> $string`. Body equality does not constrain the declared type, and a helper declared at a supertype still validates by subtyping, so this conjunct is required.

### 7.3 Type table and data segments

The type table (`Schema.TypeTable`) declares the carrier and its magnitude array, `$string`, `Vector<String>`, records and tuples (`RecordDecl {tid, struct, fields}`), sums (`SumDecl {tid, root, ctors}`), `Option`, `Result`, `Vector` and `List` instantiations, opaque pass-through types, and the data segment of each string literal. `TypeTable.typeTableConfirmed` requires:

- the type section decodes, and its first rectype is an explicit rec group (`0x4e`); every struct and array the table names is an entry of that group;
- `keysUnique`: record and sum type ids are unique;
- `carrierConfirmed`: the carrier is the one `CertDecode.carrierState` finds, or absent exactly when there is none, and a present carrier's field 1 is a nullable reference to the declared magnitude array, itself an `i64` array;
- no struct index serves two declarations;
- field storage: every record, tuple, constructor, `Option` (`{i32, T}`), `Result` (`{i32, T, E}`), `List` (`{T, ref null self}`) and `Vector` entry stores exactly the representation of its declared field types; `$string` is `(array (mut i8))` and `Vector<String>` is `(array (ref null $string))`; a one-field record is represented by its field's value, whose heap type must be the record's declared index;
- every sum passes `Grammar.sumOk` (not a one-constructor, one-field newtype, and distinct constructor structs) and `GrammarLower.S3Pin`: the root is a non-final empty struct and every constructor struct is declared `sub final` under the root, read from the raw bytes of the opening rec group. This is what makes the interpreter's exact `ref.test` agree with the wasm subtype test (section 12).

`TypeTable.dataConfirmed` requires every declared literal-to-segment entry to name a passive data segment holding exactly those bytes, and every string literal of every plan (literal nodes and literal match arms) to name such a segment (`GrammarLower.DataPin`).

`TypeTable.declsWellFormed` requires the declarations to be inhabited: `eqref` appears only as the subject-scratch local; no chain of one-field records loops; and every declared record and sum, and every parameter and result type of every plan, has a finite value (`inhabited`, proved sound by `inhabTy_sound`). Without it an obligation over an uninhabitable type would hold of any code.

A record's `tid` is the plans' own name for a type and is declared, not bound: a wrong id renames a confirmed layout and cannot change it.

### 7.4 Runtime contracts

`ClaimAxes.contractsMatch` requires `subject.contracts` to equal, as a list, the contracts the wall derives: a helper's contract appears when some plan's lowering calls that helper's index, and the totality contracts appear when some obligation is L3. The strings, in this canonical order, are:

1. `__rt_aint_from_i64 (box i64 -> carrier)`
2. `Int.add (carrier add = exact integer addition on represented values; result canonical)`
3. `Int.sub (carrier sub = exact integer subtraction on represented values; result canonical)`
4. `Int.mul (carrier mul = exact integer multiplication on represented values; result canonical)`
5. `String.eq (WVal byte-array equality; non-arrays compare false)`
6. `String.concat (container-of-string-arrays -> byte-concatenated array)`
7. `__aint_to_index (carrier -> i32 array index; [0, 2^31) passes, else -1)`
8. `__aint_cmp (canonical carrier pair -> i32 sign; -1 less, 0 equal, 1 greater)`
9. `__aint_eq (canonical carrier pair -> i32 boolean; 1 when equal, else 0)`
10. `__aint_divmod (canonical carrier pair, nonzero divisor, want_mod 0 or 1 -> canonical Euclidean quotient (0) or remainder in [0, |b|) (1))`
11. `Int.add (carrier add = exact integer addition on represented values; result canonical); total on represented values`
12. `Int.sub (carrier sub = exact integer subtraction on represented values; result canonical); total on represented values`
13. `Int.mul (carrier mul = exact integer multiplication on represented values; result canonical); total on represented values` (only when an L3 obligation has role `.mul`)

The producer's copies live in `aver-cert/src/engine/mod.rs`, and a unit test compares them with the wall's.

### 7.5 Whole module

`acceptedWholeModule` requires:

- `moduleFramingValid`: strict section framing;
- `exportsAccounted`: every export of the module is either an obligation (same name, kind and function index) or named in `declaredUncertified`; both lists are duplicate-free, disjoint, and name only real exports;
- `importsWithinCapabilities`: section 4.1;
- `startAccounted`: the start section equals `subject.start`;
- `closureIsolation`: the direct-call closure of the certified roots, recomputed from the code section, equals the declared roots plus helpers; the roots are exactly the obligations' function indices; a reachable import or a rejected instruction fails the scan; and the module declares no shared memory.

### 7.6 Artifact bytes and identity

**Hash.** The verifier MUST hash the delivered artifact and reject unless it equals `wasm_sha256`. The witness pins `subject.artifactHash` to that hash by `rfl`. `Schema.Holds` also compares it with `CertModule.wasmSha256`, which the verifier renders into the checker-owned `Module.lean` from the hash it computed; a package `Module.lean` is ignored. The wall's `Schema` imports `Module`, so no package module is in the wall's import closure: a package declaration cannot become the target of a name the wall resolves.

**Byte injection.** The verifier MUST generate `ArtifactBytes.lean` from the bytes it read. The encoding is one little-endian natural, `modBytes = Σ bytes[i] · 256^i`, written as hex numerals of at most 1024 bytes each, each chunk shifted to its byte offset and joined with `|||`, in a `noncomputable` definition, plus `modLen`, the byte count. The length matters because trailing zero bytes do not show in the numeral. Every in-kernel decoder reads this pair. The witness pins `Artifact.data.modBytes` and `modLen` to the generated values. `ArtifactComponentBytes.lean` carries the delivered component the same way.

**Wall.** `format.wall_id` names the exact wall source set and toolchain (section 1). The verifier writes the embedded wall into the build directory itself; package files with wall names are ignored.

### 7.7 Declared layout and fast readings

The kernel is slow at searching and decoding bytes and at comparing Strings. Three devices make the checks cheaper. Each is proved equal to, or sufficient for, the check it replaces, so the statements of sections 7.1 to 7.5 do not change, and a wrong declaration only declines.

**Declared layout.** The package declares where things are instead of the checks searching for them. `DeclaredLayout.layoutConfirmed` decodes the import, function and code sections once, in full, and requires every defined function's declared type index and code entry (the exact slice at the declared offset and length) to equal what the decoders read. `fnTypesConfirmed` requires every declared function type to be the type-section entry at its index. A plan entry then reads its code entry, type index and function type from the confirmed declarations, and an exported entry reads its export entry at its declared position; `exportNamesDistinct` (every export name distinct, decided on numeric name keys) makes that the one entry the name search of `entryAccepted` would find. `entries_of_fast`, `closureIsolation_of_layout`, `arithRoleCheck_of_layout` and `plansAcceptedRest_of_layout` connect the fast checks to the ones they replace.

**Section cuts.** The decoders read a section as one little-endian numeral and a length, one byte at a time, and every byte read shifts the rest of the section. The kernel keeps each of those numerals until the declaration is checked, so decoding a section of `S` bytes costs memory quadratic in `S`: the export section of a 740 KB module ran past a 16 GB heap. The package therefore declares, for the type, export and code sections, the byte length of every entry (`typeCuts`, `exportCuts`, `codeCuts`). `ByteWindow` cuts the section at those lengths into one window per entry and decodes every window on its own, and `Ext` lemmas show that each entry reader returns on its window what it returns inside the section. One declaration per section (`types_cut`, `exports_cut`, `code_cut`) decides that every window decodes and fills its window exactly, and proves the section's decoder equal to a lazy reading of the windows (`typesLazy`, `exportsLazy`, `codeLazy`) that decodes an entry only when a check reads it. A wrong cut makes some window fail to decode or to fill its window, and the package declines. `SortedKeys` replaces the balanced-tree set checks of the export accounting and the closure isolation by merge sorts and walks over sorted numeric keys, with a proof that what they decide implies the tree-based checks (`exportsAccountedOf_of_fast`, `closureIsolationL_of_S`); the export names' distinctness is read from the accounting (`exportNamesDistinct_of_accounted`). None of this changes what is accepted: the acceptance statement still reads the decoders, and the cuts only choose how the kernel evaluates them.

**String helper roles.** `StringHost.roleTable` classifies every defined function by the signature of its type. `StringFast.roleTableFast` first folds the signature list into a bitmap of the types an eq or concat helper can have (two reference parameters of one string-array type and an `i32` result, or one reference parameter and a string-array result), and reads a signature only for a function of such a type. `roleTableFast_eq` proves the two tables equal, so `decodedStringHostRoles` is decided through the fast one.

**Names as characters.** The kernel converts and compares String values by rebuilding their UTF-8 bytes, in time quadratic in their length. A package therefore states the capability names it declares as character lists, checked against the manifest's Strings by `rfl`, and `Chars.importsWithinCapabilities_of_chars` decides `importsWithinCapabilities` over their code points. `Chars.boxIdx_eq`, `toIndexIdx_eq`, `cmpIdx_eq` and `carrierHelperAbsent_eq` read the helper export names on the raw bytes of the export entries.

## 8. Policies and totality

The policy of an obligation comes from `GrammarTotal.groupPolicy` over the plans of its call group: L3 (`simulatesModelTotally`, the canonical witness, and the group's role) when `checkTermGroup` passes, L1 (`simulatesModel`) otherwise. No manifest field can set it.

`checkTermGroup` admits a group when every member has only Int parameters (at least one), an Int or Bool result, and a body `if n <= 0 then base else step` over parameter 0, where `base` and `step` are built from Int and Bool literals, parameters, Int `+ - *` and calls to group members, every member call passes `n - 1` (literally `binOp .sub (local 0) (literal 1)`) as its first argument, and `step` makes at least one such call. The role is `.mul` when a member multiplies, else `.addSub`. Division, matches, records and calls outside the group keep a function at L1. `GrammarTotal.fn_certified_total_of_check` proves `holdsTotal` for every member of a passing group.

## 9. Bridges and law-claims

### 9.1 Statement kinds

Every obligation's model is the plan. A source-bridge states that this model computes a named transpiled source function through source-value encoders. `GrammarBridge.lean` defines the two kinds, with `o := exportObligation manifest "<export>"`:

- `Exact`: `∃ o, … = some o ∧ (∀ x…, ArgsTyped o [enc x…]) ∧ (∃ k, ∀ fuel, k ≤ fuel → ∀ x…, o.model fuel [enc x…] = some (encRes (<model> x…)))`;
- `Adequate`: `∃ o, … = some o ∧ (∀ x…, ArgsTyped o [enc x…]) ∧ (∀ fuel x… v, o.model fuel [enc x…] = some v → v = encRes (<model> x…))`.

`ArgsTyped` says the encoded arguments are well-typed at the plan's parameters, so `holds` applies to them. `GrammarBridge.adequate_transfer` states what an adequate bridge gives for the bytes: a returning run on represented encoded arguments returns a represented image of the source result. An adequate bridge says nothing about termination; it holds vacuously of a model that never returns, and a verifier MUST NOT present it as a totality claim.

The producer proves a bridge from one step lemma per function (`GrammarBridge.Step`): the plan body, with every call answered by the callees' source images, returns the function's own image. `exact_of_step` assembles exact bridges for a call closure without recursion; `bridge_of_step` assembles adequate bridges for any closure by one fuel induction. The producer attempts no bridge for a plan of more than 100 nodes.

### 9.2 `sourceBridges`

Each entry is an exact object `{export, theorem, corollary, model, kind, params, result}`. **The entry carries structure, never statement text.** The verifier renders the statement from `(export, model, kind, params, result)` with `aver-cert/src/bridge_statement.rs`, the renderer the producer also uses, and pins the package's corollary at exactly that text. A verifier MUST NOT accept a statement supplied by the package.

- `export`: a plain undotted identifier naming an entry of `certified[]`, listed at most once.
- `theorem`: exactly `AverCert.Bridge.<export>`; `corollary`: exactly `AverCert.Bridge.<export>_certified`.
- `model`: the fully qualified source function, a plain dotted identifier of at most 200 bytes, `'` allowed after a segment's first character.
- `kind`: `"exact"` or `"adequate"`.
- `params`: one encoder per parameter, in declaration order; `result`: one encoder.

An encoder is an exact object in one of eleven closed forms: `{"kind": "int"}`, `"bool"`, `"float"`, `"string"`; `{"kind": "record", "tid": n, "type": T, "fields": [{"accessor": A, "encoder": E}, …]}`; `{"kind": "sum", "tid": n, "type": T, "ctors": [{"ctor": C, "fields": [E, …]}, …]}`; `{"kind": "option", "elem": E}`; `{"kind": "result", "ok": E, "err": E}`; `{"kind": "tuple", "tid": n, "elems": [E, …]}` with two or more elements; `{"kind": "list", "elem": E}`; `{"kind": "vector", "elem": E}`. Names are `_root_.`-qualified plain dotted identifiers, every accessor is a field of its record type and every constructor a constructor of its sum type, and nesting is at most 8 deep and 256 nodes. Anything else declines the package. The encodings are `Int ↦ SVal.i`, `Bool ↦ SVal.b`, `Float ↦ SVal.f (Float.toBits x)`, `String ↦ SVal.s (GrammarBridge.strBytes x)`, a record ↦ `SVal.record tid` of its accessors in order, a sum ↦ `SVal.variant tid c` with `c` the constructor's position in `ctors`, `Option` ↦ `SVal.none`/`SVal.some`, `Result` (Lean `Except E T`) ↦ `SVal.ok`/`SVal.err`, a tuple ↦ `SVal.record tid` of its components, a list ↦ `SVal.nil`/`SVal.cons`, a vector ↦ `SVal.vec`.

The rendered statement is an application of the wall's own definitions: `_root_.AverCert.GrammarBridge.Exact _root_.AverCert.manifest "<export>" (fun (x : X) => [enc x…]) (fun (x : X) => encRes (<model> x…))` for `exact`, and the same with `GrammarBridge.Adequate` for `adequate`. `X` is the parameter type for one parameter, `_root_.Prod T0 (_root_.Prod T1 …)` for several (each parameter read as its `Prod.fst`/`Prod.snd` component of `x`) and `_root_.Unit` for none. The order, quantifiers and numbers of section 9.1 therefore live in the wall, which was elaborated without the package. The parts the verifier renders contain no operator that resolves through an instance, and every type id and constructor position in an encoder is a `nat_lit`, so a package instance such as `LE Nat` cannot change what the pin says. The producer proves its own expanded form (one binder per parameter, ordinary numerals) and restates it at the pinned statement in its `_certified` corollary; an instance that changed what the expanded text means makes that restatement fail rather than weakening the pin.

The rendered text passes the statement gate (nonempty, at most 16000 bytes, no control character, no `:=`, `--`, `/-` or backtick, no identifier token outside a literal or `«…»` identifier with a `.`-separated segment `set_option` or `open` (a term-level `set_option … in` would bypass the option whitelist of stage 7, and a term-level `open … in` would change what the statement's names resolve to), no `s!`/`r` string, balanced `()[]{}⟨⟩` counted outside string literals, char literals and `«…»` identifiers, every literal terminated), and every dotted name in it MUST be `_root_.`-qualified, because the pin elaborates at the root namespace.

The checker's audit program (section 10) also holds every record and sum an encoder reads to its elaborated declaration. A record encoder MUST list exactly the structure's fields, in declaration order; the structure MUST NOT be a proposition and no field may be a proof. A sum encoder MUST list exactly the inductive's constructors in order, each with exactly its number of fields, none of them a proof. Without this, a record with an unlisted field such as `h : False` would let a bridge quantify over no value and hold vacuously.

### 9.3 `laws`

Each entry is an exact object `{label, theorem, statement, corollary, bridges}`. `label` is the source `module.fn.law` identity and `theorem` the fully qualified model theorem; both are plain dotted identifiers of at most 200 bytes, and only `theorem` may carry `'` after a segment's first character. `corollary` MUST be the label with every `.` replaced by `_`, and corollaries are unique. `statement` is the theorem's universal statement on one line and passes the statement gate. `bridges` is an array of export names, each declared in `sourceBridges` and listed at most once. When nonempty it MUST be exactly the declared bridges whose `model` appears as `_root_.<model>`, an identifier token of `statement` (a maximal run of ASCII alphanumerics, `_`, `.` and `'`, stripped of leading and trailing dots), each once, in first-appearance order, and the statement MUST NOT spell any declared bridge's `model` another way: neither the bare name nor a token ending in `.<model>` that does not start with `_root_.` (`Evil.Tiny.addTwo`). Such a spelling is where a namespace or a binder could make the name mean another constant, so a law listing bridges that carries one declines the package. `law_mentioned_bridges` in `aver-cert/src/bridge_statement.rs` states that rule, and the producer and the verifier both apply it; any other nonempty list declines the package before Lean runs. The producer fills the list when every model function the statement mentions has a bridge, and leaves it empty otherwise.

`statement` is read at the root namespace (section 10), not in the namespace of `theorem`, which the package chooses: inside `namespace Evil`, Lean would resolve the text `Tiny.addTwo` to a package constant `Evil.Tiny.addTwo` when one is declared. The compiler's emitter writes a law's statement for the namespace its theorem is emitted in, so the producer rewrites it before it ships it (`root_qualify_statement` in `aver-cert/src/engine/law_claims.rs`): every name that resolves there to a name the model modules declare becomes `_root_.<that name>`, and binders, keywords, literals, projections and core names stay as written. A name the rewrite resolves wrongly costs only that law's credit, since its corollary then no longer checks against the model theorem. The verifier cannot tell a model function without a bridge from any other identifier, so it cannot refuse an empty list on that ground; the pinned `_bridged` statement lists exactly which bridges the claim carries.

The package's `Laws.lean` proves `AverCert.Laws.<corollary> : (statement) ∧ Holds manifest`, at the root namespace. A claim with a nonempty `bridges` also proves `AverCert.Laws.<corollary>_bridged`, the same conjunction plus the rendered statements of the listed bridges in order. The `_bridged` name is derived, never transported.

### 9.4 Credit

Each law pin, bridged-law pin and bridge pin has two outcomes once it elaborates: **credited** when its axiom closure stays inside the whitelist, and **not credited** when it does not (`sorryAx`, `Lean.ofReduceBool`, a user axiom), with the offending axioms named in the report. A pin that does not elaborate means the package does not prove what it declared, and the whole package is declined. Credit never changes the verdict or the exit code; those belong to the exports.

The law pin and the bridged-law pin are separate on purpose. A bridge whose proof falls to `sorry` taints every declaration citing it, and one wider corollary would have removed the credit of every law that only mentions the bridged function. A verifier MUST audit the two separately and MUST NOT let bridge credit move law credit.

## 10. Checker witness and audit program

After the package builds, the verifier writes `CheckerWitness.lean`, which is never read from the package. It is data for the kernel: theorems only, no `import Lean` and no command that runs code. It imports `AcceptedArtifact`, `ArtifactBytes`, `Manifest`, `Artifact`, `Laws` and `Bridge` when declared, and `ArtifactCertificate`. Every name in it is `_root_`-qualified, so a package declaration placed where an unqualified name would resolve first (for example `AverCertChecker.AverCert.AcceptedArtifact.accepted`, under the witness's own namespace) cannot be reached. `_root_` does not settle a dotted name on its own: Lean resolves `A.b.c` to the longest prefix that is a declared constant and reads the rest as field accesses, so `_root_.AverCert.manifest.subject.contracts` would mean a package constant `AverCert.manifest.subject`, or `AverCert.manifest.subject.contracts` itself, if one were declared. No name in the witness therefore continues past a package constant. The package constants it names, `AverCert.manifest`, `AverCert.Artifact.data`, `AverCert.Artifact.certificate` and the law and bridge corollaries, are named in full, and every field is read through the wall structure's projection function: `_root_.AverCert.Schema.Subject.contracts (_root_.AverCert.Schema.Manifest.subject _root_.AverCert.manifest)`, `_root_.AverCert.AcceptedArtifact.ArtifactData.manifest _root_.AverCert.Artifact.data`, `_root_.List.map _root_.AverCert.Schema.Obligation.policy (_root_.AverCert.Schema.Manifest.obligations _root_.AverCert.manifest)`. A projection is a wall constant under a wall namespace, where the audit refuses every package declaration other than a private or Lean-auxiliary one (item 1 below; none of those is a field name), and the audit also refuses a package constant that extends another declared constant's name (item 1 below). Every numeral is a `nat_lit` and every `Int` is built from `Int.ofNat` or `Int.negSucc`, so no `OfNat` instance takes part in what a pin says. It contains, in order:

- one bridge pin per `sourceBridges` entry, at the root with no `open`: `def _root_.AverCertChecker.bridge_statement_<i> : Prop := (<rendered statement>)`, then `theorem _root_.AverCertChecker.bridge_pin_<i> : _root_.AverCertChecker.bridge_statement_<i> ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) := _root_.AverCert.Bridge.<export>_certified`;
- one law pin per `laws` entry, at the root with no `open`: `def _root_.AverCertChecker.law_statement_<i> : Prop := (<statement>)`, then `theorem _root_.AverCertChecker.law_pin_<i> : _root_.AverCertChecker.law_statement_<i> ∧ (… Holds …) := _root_.AverCert.Laws.<corollary>`, and, for a claim with bridges, `_root_.AverCertChecker.bridged_law_pin_<j>` at `law_statement_<i> ∧ (… Holds …) ∧ bridge_statement_<k> ∧ …`, numbered over the bridged claims. Each statement is elaborated alone, as its own definition, and the pins conjoin the definitions, so no statement text can change how a conjunction associates; the package's corollary checks against the pin only if its own type is that conjunction. No statement is read inside the model theorem's namespace: the package names that namespace, and a package constant in it could capture a name of the statement;
- the report pins, each a theorem `_root_.AverCertChecker.report_pin_<k>` (21 of them), every field read through its projection function as above: that `Artifact.data.modBytes`, `modLen`, `manifest` and `wasip2ComponentEnvelope` are the checker's bytes, `AverCert.manifest` and the declared envelope; that `subject.artifactHash` is the recomputed hash; that `artifactRoot`, the obligation export names, `subject.exports`, the policies, the termination witnesses, `contracts`, `declaredUncertified`, `capabilities`, `start`, `hostRoleTable`, `stringHostRoles`, `target`, `profile` and `abi` are their JSON values; and that `ClaimAxes.reportEntries` and `ClaimAxes.reportFacets` are the JSON's names, class and facets. Each is proved by `rfl`, the two report-entry pins by `first | rfl | decide +kernel`;
- `theorem _root_.AverCertChecker.checked : _root_.AverCert.AcceptedArtifact.accepted _root_.AverCert.Artifact.data := _root_.AverCert.Artifact.certificate`, which forces the package root to exist at exactly the accepted type.

A pin that does not elaborate declines the package.

The axiom guard and the audit of what the package declared are a separate checker-authored program, `CheckerAudit.lean`, run as `lake env lean --run CheckerAudit.lean` after the witness is built. Its code is elaborated with only the Lean toolchain in scope, so no instance, notation or declaration a package ships can change what it computes. (An audit elaborated inside the package's environment could be subverted, for example by a package `BEq Lean.Name` instance that made every axiom compare equal to a whitelisted one.) At run time it loads the built `CheckerWitness` environment and, in order:

1. declines a package that declares any constant under the reserved `AverCertChecker` prefix; then any package constant under a wall or checker namespace root (`AcceptanceSoundness`, `ArithTemplateDerisk`, `AverBits`, `AverCertAudit`, `AverCertChecker`, `CertDecode`, `CertModule`, `CertPrelude`, `InterpreterSequencing`), any under `AverCert` other than exactly `AverCert.manifest` and `AverCert.subject` or a name inside the producer's own `AverCert.Artifact`, `AverCert.Bridge`, `AverCert.Final`, `AverCert.Laws` and `AverCert.Plans` (so `AverCert.manifest.subject` and `AverCert.Artifact` itself are refused), and any whose name has `AverCert` or `AverCertChecker` after its first component (`AverCert.AcceptedArtifact.AverCert.ClaimAxes.checked`). Lean resolves a dotted name in the innermost enclosing namespace first, so such a name is where a wall reference could land. It then declines any package constant under `AverCert` whose name extends another declared constant (`AverCert.Artifact.data.manifest` extends `AverCert.Artifact.data`), because Lean resolves a dotted name to the longest prefix that is a constant, so such a name is where a field read of that constant could land; the producer writes the pieces of a long subject list as `AverCert.Plans.subject_<field>_<k>` for this reason. Exactly two kinds of package constant are exempt from both rules. One is a private constant, which no other module can name. The other is an auxiliary Lean declares beside a constant (`leanAuxiliary`): beside a constant the package does not declare, a reserved name such as an equation lemma, which Lean realizes in the package module that first unfolds that constant and which states its own fact (Lean refuses a user declaration of a reserved name whose parent exists, and such a parent exists before every package module); and, beside a package constant, an internal `_`-prefixed compiler constant, an abstracted `proof_<k>`, a matcher `match_<k>`, an equation `eq_<k>`, or an unfolding lemma `eq_def` or `eq_unfold`, recognised by name. A reserved name beside a package constant is not exempt as such, because a package can declare `V.h.eq_1` itself before it declares `V.h`. None of the exempt names is a field name of a wall structure;
2. declines a package module that carries a parser extension (notation, syntax, a mixfix operator, a token), a scoped instance, or an instance outside the admitted forms. The class of an instance is read off its elaborated type, so an alias (`abbrev Order := LE`) or a class parent projection cannot disguise it. Admitted are: instances of `Decidable`, `DecidableEq`, `DecidableRel`, `DecidablePred`, `Nonempty`, `ReflBEq` and `LawfulBEq` at any type (they carry proofs, so they cannot make a proposition mean something else, and a false one needs an axiom the guard sees); `Inhabited`, `BEq` and `SizeOf` at an inductive type a package module declares; instances of a class a package module declares; and exactly two data instances over core types, `Coe Int Float` with value `⟨fun n => Float.ofInt n⟩` and `HAdd String String String` with value `⟨String.append⟩`, compared structurally. Anything else (`LE Nat`, `OfNat Nat n`, `BEq Lean.Name`, …) declines the package;
3. checks every record and sum a bridge encoder reads (section 9.2); then the law statements: the witness reads each one at the root, where a `_root_.<model>` spelling means exactly the root constant whatever else the package declares (a package constant `Evil.Tiny.addTwo` beside the model `Tiny.addTwo` is harmless), and the audit declines when the value of `law_statement_<i>` of a law with bridges does not use every listed bridge's `model` constant (`Expr.getUsedConstants`), which a `_root_.<model>` spelled where elaboration drops it (a type ascription's type) would otherwise pass;
4. collects the axioms of `AverCertChecker.checked` and of every report pin with `Lean.collectAxioms`, and declines on any axiom outside `[propext, Classical.choice, Quot.sound]`. A report pin closed by `decide +kernel` through a `sorry`-backed package decision procedure carries `sorryAx` into this audit;
5. collects the axioms of every `law_pin_<i>`, `bridged_law_pin_<j>` and `bridge_pin_<i>` (the pins, never the package's bare corollary names) and logs one line per pin: `AVER_LAW_AUDIT`, `AVER_LAW_BRIDGE_AUDIT` or `AVER_BRIDGE_AUDIT`, then the pin name, then `ok` or `axioms <name>[,<name>…]`.

A decline is the line `AVER_AUDIT_DECLINE <reason>` and a nonzero exit. Completion is the line `AVER_AUDIT_OK`; a run that ends without it declines. Names are matched as exact, fully qualified `Lean.Name`s, with no prefix or namespace matching. The verifier reads the per-pin lines back and requires exactly one well-formed line per declared pin; a missing, renamed, repeated or malformed line declines the package. Only an `ok` line grants credit, and `ok` is a keyword in its own field, so an axiom literally named `ok` cannot pass for one. The two axiom audits differ only in what a violation costs: an axiom outside the whitelist under the accepted root or a report pin rejects the certificate, while a law or bridge pin loses only its own credit (section 9.4).

The witness and the audit run on every `verify` and `check`, after any cache restore, and are never cached. `collectAxioms` reads, for an imported declaration, the axiom data that the Lean process that built its module recorded in the `.olean`; the checker builds every module itself, so that data is its own. The final replay re-checks terms but enforces no axiom policy, so the audit is load-bearing. The token gate of stage 7 is a hardening layer in front of it; the audit and kernel acceptance are what the verdict rests on.

## 11. Verification pipeline

A verifier MUST run these stages in order and MUST reject on the first failure (nonzero exit, no CERTIFIED output). The reference is `aver-cert/src/verifier.rs`.

1. **Manifest gate.** Read the artifact and `cert-manifest.json`. Require `schema_version = 9`, the target, profile and ABI of section 4.1 (one admitted pair: `wasm-gc` with `aver-wasm-gc/0`, or `wasip2` with `aver-wasip2/0`), and the envelope field exactly when the target is `wasip2`.
2. **Wasm validity.** For `wasm-gc`, run a complete WebAssembly validator over the bytes (`wasmparser::Validator::validate_all`) and require a core module. For `wasip2`, require a valid component, split it by the declared lengths, and require the core slice to be a valid core module. *Known gap:* the enabled proposal set is `wasmparser` 0.248's default, inherited from the dependency rather than stated here.
3. **Envelope checks.** Hash the artifact and require `wasm_sha256`; require `format.version = 1`, a `wall_id` naming an embedded wall, and `artifact_certificate_root`; parse the candidates of section 4 with the exact-object rules, the policy and termination coupling, the class check, the bridge rendering and gates, and the candidate gate.
4. **Build directory.** Create a fresh private build directory (mode `0700` on Unix) under a checker-chosen temporary root.
5. **Staging.** Stage each regular top-level `.lean` file, skipping checker-owned names (`ArtifactBytes.lean`, `ArtifactComponentBytes.lean`, `Module.lean`, `lakefile.lean`, `CheckerWitness.lean`, `CheckerAudit.lean` and every wall source). Walk subdirectories, skipping dot-directories at every depth and rejecting nesting deeper than 16 levels. Stage a nested file only when a staged top-level `Manifest.lean`, `Certificate.lean`, `Bridge.lean` or `Laws.lean` contains an import line for its dotted name. The import scan is literal: each line is trimmed, a leading `import ` is stripped, and the rest is the module name; comments are not parsed. Nested files stage in sorted order. Two staged paths equal ASCII-case-insensitively are rejected.
6. **Names.** Every path segment MUST match `^[A-Za-z][A-Za-z0-9_]*`, with `.lean` on the last. The dotted root and each of its dotted prefixes MUST NOT case-insensitively equal `Init`, `Lake`, `Lean`, `Std`, a wall root, `ArtifactBytes`, `ArtifactComponentBytes`, `Module`, `CheckerWitness`, `CheckerAudit` or `lakefile`.
7. **Package text gate.** Reject a staged file that carries a refused construct in code position. The file is tokenized after lossy UTF-8 decoding into identifiers (a dotted name is one token; `'`, `!` and `?` continue an identifier), `#`-commands and symbols, and the rules read tokens, never substrings, so whitespace, line breaks and comments between the words of a construct (`open  Lean`, `open /- -/ Lean`, `set_option` and its option on the next line) change nothing. The tokenizer skips only normal string literals, the string parts of an `s!` interpolated string (its `{…}` terms are tokenized as code), line comments and block comments, and treats any lexical ambiguity (a raw string prefix, another interpolation prefix such as `m!`, an interpolation term holding a comment or a raw string, an unterminated string or comment) as code for the rest of the file. Refused are: (a) an identifier any of whose `.`-separated components is one of `run_cmd`, `run_elab`, `run_meta`, `run_tac`, `initialize`, `builtin_initialize`, `macro`, `macro_rules`, `elab`, `elab_rules`, `syntax`, `notation`, `infix`, `infixl`, `infixr`, `prefix`, `postfix`, `binder_predicate`, `declare_syntax_cat`, `unif_hint`, `export`, `scoped`, `unsafe`, `implemented_by`, `extern`, `attribute`, `simproc`, `dsimproc`, `simproc_decl`, `dsimproc_decl`, `builtin_simproc`, `builtin_dsimproc`, `register_simp_attr`, `register_option`, `register_builtin_option`, `deriving` outside its admitted clause, or the reserved `AverCertChecker`; (b) the symbols `@[`, `«` and `»`; (c) any `#`-command other than `#guard_msgs`, `#print` and `#check`; (d) `set_option`, as a command or in `set_option … in`, whose option is not `autoImplicit`, `relaxedAutoImplicit`, `maxHeartbeats`, `maxRecDepth`, `smartUnfolding`, `synthInstance.maxSize`, `synthInstance.maxHeartbeats` or a `linter.` option (nothing under `debug.`: `debug.skipKernelTC` adds declarations the kernel never checks); (e) `open` of a namespace rooted at `Lean` or `Lake`, or `namespace` entering one, with or without a leading `_root_.`. `deriving` has two admitted forms, each alone on the rest of its line: `deriving C, …` with every `C` in `BEq`, `DecidableEq`, `Inhabited`, and `deriving instance C, … for T, …` with every `C` in `ReflBEq`, `LawfulBEq` and every `T` a plain dotted identifier; the next token after the line may be neither `,` nor `with`, because Lean would continue the clause across the newline. `instance` is not refused here: the model needs a few, and which ones a package may declare is decided on the elaborated declarations by the audit program (section 10). The reference is `aver-cert/src/lean_gate.rs`, which the producer runs too. The gate is hardening; the audit program and kernel acceptance with the fresh replay are what the verdict rests on.
8. **Materialization.** Write the wall sources, `ArtifactBytes.lean`, `ArtifactComponentBytes.lean`, `Module.lean` (`CertModule.wasmSha256`, the hash computed in stage 3), a checker-authored `lakefile.lean` whose roots are the sorted union of the staged roots, the wall roots, the two byte modules and `Module`, and the pinned `lean-toolchain`. The lakefile passes the heap ceiling (`--memory`, `AVER_CERT_MEMORY_LIMIT_MB`, default 16384) to every worker.
9. **Build.** Run `lake build` through the pinned toolchain. The canonical Elan installation (`ELAN_HOME` or `~/.elan`) is the bootstrap trust anchor, invoked by absolute path with a cleared environment, implicit Lake caches disabled and temporary directories inside the checker's tree. `AVER_CERT_BUILD_JOBS` sets the worker count (default 1). Every Lean subprocess runs under a per-step wall-clock limit, 15 minutes by default, replaced by `AVER_CERT_PHASE_TIMEOUT_SECS` up to one day; on expiry its process tree is killed and the package is declined. The one exception is the optional prelude-cache build, whose timeout becomes a warning and a cache miss. Caches are opt-in and trusted local state: `AVER_CERT_DATA_CACHE` is keyed on a layout version, the schema version, the artifact hash, the wall id, the toolchain, and the name and contents of every staged file (recursively, witness excluded); on a miss, it restores each staged package module (wall sources excluded) whose outputs it stored under a key over a module-layout version, the wall id, the toolchain and schema versions, the module's relative name and contents, and the keys of the staged modules its `import` lines name, each entry with its own integrity manifest; Lake recomputes every restored module's input trace and rebuilds a module whose trace disagrees, so a key that misses an input costs a rebuild, not a stale module. `AVER_CERT_PRELUDE_CACHE` caches the artifact-independent wall. A cache-assisted build that fails is retried once from clean. Only `check` uses the caches: `verify` ignores both variables, with a notice, and builds from the staged sources alone.
10. **Witness and audit.** Write `CheckerWitness.lean` (section 10) and elaborate it with `lake env lean -o … CheckerWitness.lean`; then write `CheckerAudit.lean` and run it with `lake env lean --run CheckerAudit.lean`. Both run on every invocation, outside any cache. A witness failure is reported as the certificate not binding to this artifact; an audit decline, or an audit that does not complete, declines the package.
11. **Fresh replay** (`verify`, `explain`, `inspect`). Run `lake env leanchecker --fresh CheckerWitness`, which re-checks the witness and its whole import closure (the wall, the artifact certificate, the model modules, `Laws.lean` and the bridge modules) in a fresh declaration environment. A lemma a package module added without a kernel check fails here, whether it supports the root, a law or a bridge; the stage-7 gate refuses `debug.skipKernelTC`, and this is the backstop. The witness imports no `Lean`, which keeps the replay to the certificate's own closure. `check` is the only mode that skips this stage; it trusts the built `.olean` files and MUST report `CHECKED`, never `CERTIFIED`. `AVER_CERT_PARALLEL_REPLAY` is a developer override that names another replayer binary; release runs leave it unset.
12. **Verdict.** A failure prints `DECLINED` (`verify`), `CHECK FAILED` (`check`) or `error:` (`explain`, `inspect`) and exits nonzero. Zero certified exports with every stage passing prints `NO CERTIFIED EXPORTS (admission only, no behavioral claims)` for `verify`, `NO CHECKED EXPORTS (developer preflight only, no behavioral claims)` for `check`, or `NO CERTIFIED EXPORTS` for `explain`, and exits nonzero. Otherwise the verifier prints `CERTIFIED` or `CHECKED`, the artifact, the export count, the level computed from the pinned policies, and per export only pinned facts: name, policy, and `class: source-plan-v1` with its facets. When the manifest declares them, the summary adds `; law-claims: N of M credited`, `; bridged-laws: N of M credited` and `; source-bridges: N of M credited`, followed by one line per uncredited pin naming its axioms. `explain` also prints a model line per export, one `domain:` line stating that every Int input is assumed to be a canonical carrier (section 12), the contracts, the declared law statements, the rendered bridge statements, and the declared-only `sourceBridgesDeclined` and `source_level_only` lists. The model line is `model: plan (the export's optimized MIR body)`; with a credited exact bridge it starts `model: plan ≡ <model>`, and with a credited adequate bridge `model: plan ≡ <model> wherever the plan returns`. *Known gap:* `explain` reads `cert-manifest.json` a second time, after the positive report, for the declared law statements and the declined lists. If that read fails, it exits nonzero after printing a positive report, and the lists can come from a different manifest than the one verified. A reimplementation SHOULD reuse the bytes it verified.

## 12. Trust inventory

A `CERTIFIED` verdict is as strong as the items below. They are listed one by one because several sit on the adversarial input path and are not kernel-checked.

A verdict does not trust:

- **The compiler and the producer.** The MIR printer, the producer's Rust twins of the wall functions and the Lean renderer run only during `aver compile --certify`. A wrong producer yields a declined certificate, not a wrong verdict.
- **Declared-only fields.** They can lie without affecting the verdict and MUST be presented as declarations.
- **Rust reconstruction.** The verifier does not disassemble the module, rebuild obligations or print plans.
- **Package build infrastructure.** Wall sources, lakefile, toolchain pin, byte modules and witness are checker-owned.
- **Caches as evidence.** Package caches are ignored, `verify` ignores configured caches, and under `check` a local cache cannot skip the witness or the audit.
- **Diagnostics.** A diagnostic can explain a decline, not upgrade one.

A verdict trusts:

- **The Lean toolchain.** Lean 4.34's elaborator, kernel, Lake and `leanchecker`, resolved through the local Elan installation. `leanchecker --fresh` comes from the same distribution; there is no second, independently written kernel yet, and the design MUST NOT be described as having two diverse kernels. The verifier pins the toolchain name, not its binaries, so Elan and its download channel are part of the anchor.
- **The embedded wall.** The statement (`Obligation.holds`, `holdsTotal`, `HostContracts`, `SRepr`), the interpreter, the decoders, the lowering and the acceptance predicate are audited Lean code whose identity `wall_id` pins.
- **The wall's wasm model as a model of real engines.** The kernel proves the lowering, the interpreter and the decoders consistent with each other and binds them to the bytes. That the interpreter matches real engines is an assumption, supported by tests: `tests/cert_decode_spec.rs` checks the in-kernel decoder against an independent Python oracle (`tools/certkit/decode_ref.py`), and the cross-backend differential suites compare emitted wasm under Wasmtime with the VM.
- **The Rust transport path.** File reading, hashing, JSON checks, staging gates, process isolation, timeouts and verdict mapping (`verifier.rs`, `wall.rs`, `lean_gate.rs`, `lean_process.rs`, `format.rs`, `bridge_statement.rs`, `main.rs`).
- **The checker's audit program** (`aver-cert/src/checker_audit.lean`), which decides the admitted instances, the parser-extension ban, the encoder shapes and the axiom whitelist (section 10). It is Lean code the checker writes and compiles without the package, but nothing proves it correct.
- **`wasmparser`** for full WebAssembly validity, on attacker-chosen input.
- **SHA-256** collision resistance, for the artifact hash and the wall identity.
- **The runtime contracts, verbatim as disclosed in section 7.4.** The L1 theorem assumes the helpers obey them, and L3 also assumes the disclosed totality. The comparison and division contracts are stated on canonical pairs, because the helpers decide on the carrier's shape and only canonicity makes that agree with integer order. Proving that the shipped runtime satisfies the contracts is an obligation of each toolchain release. `tests/cert_intcmp_differential.rs` runs the real helpers under Wasmtime across every band edge, checks that boxing, add, sub, mul, Euclidean div and mod and the bitwise producers return canonical carriers, and checks `__aint_cmp` and `__aint_eq` against an outside oracle.
- **Canonical Int inputs.** `SRepr` requires every Int, at any depth of an argument, to be a canonical carrier. The runtime builds only canonical carriers: the i64 fast paths build a `Small` directly, and every path that can produce a limb-carrying result ends in the normalization epilogue. A host that fabricates its own carrier words is outside the claim. `explain` prints this assumption once for every certificate, as its `domain:` line.
- **Helpers as pure value functions.** `Schema.HostFns` models each helper as a function from argument values to an optional result, and the interpreter has no heap: a struct or array is an immutable value. The theorem assumes a helper mutates nothing reachable from its arguments. The bytes do not enforce this: the type table pins field storage but not mutability, so the carrier's fields and `$string` (`(array (mut i8))`) may be mutable.
- **Unpinned sub-routines.** The add, sub, mul, cmp and divmod templates call four factored sub-routines at declared indices (`arithParams.decompose`, `normalize`, `strip`, `umagCmp`). The wall confirms the templates call those indices, but not what their bodies are. The contracts for these helpers therefore rest on code the certificate does not look at.
- **The division contract.** A plan that divides is certified under the `__aint_divmod` contract. It says nothing at `b = 0`; every admitted plan either divides by a nonzero literal (the typing requires it) or tests the divisor for zero on its carrier before the call, and the wall proves that test exact on any represented word. No division plan is L3.
- **GC subtyping facts (S-3).** The interpreter's `ref.test` and `ref.cast` compare type indices exactly; wasm GC tests subtyping. Under `S3Pin` the two agree on the constructor structs of one sum (`GrammarSound.ctor_refTest_exact`; for an accepted artifact, `AcceptanceSoundness.refTest_exact_of_accepted`). The argument uses two facts of the wasm GC specification, carried as the explicit hypothesis `GrammarSound.GcTestSpec` and not as axioms: subtyping is reflexive, and a type declared final has no subtype in its own rec group other than itself.
- **L3 and real stacks.** `holdsTotal` says the function returns within fuel `n.natAbs + 1` in the wall's interpreter, where fuel counts nested calls and nothing else runs out. A real engine has a bounded stack and heap, so a large input can still trap on stack exhaustion or allocation failure. L3 MUST NOT be read as "the engine returns for every input".
- **The plan as the claim, where no bridge is credited.** The model of every obligation is the plan. A credited bridge makes "the plan computes `<model>`" a kernel-checked theorem, but still trusts (i) the encoders, which the producer chooses and the verifier constrains only in shape, and (ii) the model definition, which the package's own model modules carry, so the bridge is proved relative to the compiler's transpilation. An adequate bridge is partial correctness only. Without a credited bridge, the plan's correspondence to the source rests on the compiler that printed it. A verifier MUST NOT present an uncredited bridge as establishing the identity, or a law-claim with empty `bridges` as a statement about the bytes alone.
- **Model instance values (open).** The audit admits `Inhabited`, `BEq` and `SizeOf` instances at an inductive type a package module declares by class and type, not by value. A law statement that compares model values with `==` means what the package's `BEq` instance says, and the producer's derived instance is not checked to be the structural one. Pinning these values is an open item.
- **Type ids.** A record or sum's `tid` is the plans' name for a type; the layout it names is confirmed, the name is not.
- **Configured cache directories**, for `check` only, as trusted local state. Their integrity manifests detect accidental corruption, not a writer who replaces the `.olean` files and the Lake traces together.

## 13. Versioning and freeze policy

The three identities live in `aver-cert/src/format.rs` (`FORMAT_VERSION`, `CERT_SCHEMA_VERSION`, `CURRENT_WALL_ID`). The crate's `0.1.x` version is a package-manager label; no check reads it.

- A package layout change bumps `FORMAT_VERSION`. A pure widening that leaves every previously verifiable package verifying identically does not bump it.
- A statement change bumps `CERT_SCHEMA_VERSION` (history in section 1). A schema change also changes the wall identity, since the schema types are wall sources, but most wall changes are not schema changes.
- A wall source or toolchain change changes the computed identity. `CURRENT_WALL_ID` is a hand-maintained constant; the verifier recomputes the identity on first use and aborts on a mismatch, so a stale constant fails closed at run time rather than at build time.

A certificate does not expire. It verifies against a verifier that embeds the wall it names, and each release embeds one wall. When the wall changes, the new verifier rejects every earlier package at the envelope gate, and the holder re-certifies with the matching compiler. Re-certification usually re-emits the module, so the new certificate is about the new bytes. An old verifier keeps verifying the packages of its era; archival verification means keeping that verifier, an Elan installation that can still resolve its toolchain, and a platform that runs both.

> **OPEN DECISION: freeze criterion.** Neither `FORMAT_VERSION = 1` nor `CERT_SCHEMA_VERSION = 9` is frozen, and the wall changes at the pace of ordinary development. The proposal on the table, not adopted here, is to declare a freeze candidate only when (a) module framing and validation move into the wall, so `wasmparser` leaves the adversarial path; (b) the wall build mechanically checks that every subject-visible fact is pinned against a byte decoder; and (c) N consecutive months pass with no soundness-motivated change to the wall or the schema.

## 14. Appendix: section-to-source map

| Section | Sources |
|---|---|
| 1 Versioning, wall identity | `aver-cert/src/format.rs` (`FORMAT_VERSION`, `CERT_SCHEMA_VERSION`, `PLAN_CLASS`, `CURRENT_WALL_ID`), `aver-cert/src/wall.rs` (`compute_id`, `current_id`, `resolve`, `SOURCES`, `LEAN_TOOLCHAIN`), `aver-cert/assets/wall/current/lean-toolchain` |
| 2 Package layout | `aver-cert/src/engine/render_package.rs` (`write_project`, `render_plans`, `render_manifest_lean`, `render_artifact`, `render_artifact_plans`, `render_final`), `aver-cert/src/engine/layout.rs` (`render_artifact_layout`), `aver-cert/src/engine/source_bridges.rs` (`render_bridge_lean`, `MODEL_PACKAGE_DIR`), `aver-cert/src/verifier.rs` (`assemble_build`, `is_checker_owned`, `checker_witness`) |
| 3, 4 Manifest | `aver-cert/src/verifier.rs` (`trusted_check`, `read_manifest_identity`, `require_supported_identity`, `read_candidates`, `parse_termination`, `exact_object_fields`, `gate_candidate`), `aver-cert/src/engine/render_package.rs` (`render_manifest_json`), `aver-cert/src/engine/mod.rs` (contract strings) |
| 4.3 Host roles | `aver-cert/assets/wall/current/CertDecode.lean` (`AddSub.Roles`, `boxIdx`, `toIndexIdx`, `cmpIdx`, `carrierHelperAbsent`, `carrierState`, `StringHost.roleTable`), `aver-cert/assets/wall/current/ArithTemplateDerisk.lean` (`ArithHostParams`, `checkArithHostParams`, `arithHelperBody`), `aver-cert/assets/wall/current/AcceptedArtifactCore.lean` (`arithRoleCheck`, `arithTableCheck`) |
| 4.4 Wasip2 envelope | `aver-cert/assets/wall/current/Wasip2Envelope.lean`, `aver-cert/assets/wall/current/AcceptedArtifactCore.lean` (`artifactEnvelopeAccepted`), `aver-cert/src/verifier.rs` (`prepare_wasip2_artifact_with_declared_envelope`) |
| 5 Statement | `aver-cert/assets/wall/current/SchemaCore.lean` (`TypeTable`, `FnEntry`, `HostFns`, `HostContracts`, `HostTotal`, `Obligation`, `Manifest`, `HoldsCore`), `aver-cert/assets/wall/current/SchemaBase.lean` (`Subject`, `Policy`, `TotalityRole`, `CarrierSpec`, `CanonRepr`), `aver-cert/assets/wall/current/Schema.lean` (`Holds`), `aver-cert/assets/wall/current/AcceptedArtifact.lean` (`accepted`), `aver-cert/assets/wall/current/AcceptanceSoundness.lean` (`fn_claim_discharges`, `accept_sound`, `accepted_nonvacuous`) |
| 6 Plan | `src/codegen/cert/plan_from_mir.rs`, `aver-cert/src/engine/plan.rs`, `aver-cert/assets/wall/current/Grammar.lean` (`Expr`, `FnPlan`, `tyOf`, `planTyped`, `eval`, `groupModel`, `SRepr`), `aver-cert/assets/wall/current/GrammarLower.lean` (`fnCode`, `codeEntryBytes`), `aver-cert/assets/wall/current/GrammarSound.lean` (`agreement`, `fn_certified_group`) |
| 7 Pins | `aver-cert/assets/wall/current/AcceptedArtifactCore.lean` (`entryAccepted`, `callsOrdered`, `sigPinned`, `roleTypesPinned`, `indicesDistinct`, `plansAccepted`, `obligationsOf`, `exportsAccounted`, `importsWithinCapabilities`, `startAccounted`, `closureIsolation`), `aver-cert/assets/wall/current/TypeTable.lean` (`mctxOf`, `typeTableConfirmed`, `dataConfirmed`, `declsWellFormed`), `aver-cert/assets/wall/current/GrammarLower.lean` (`S3Pin`, `DataPin`), `aver-cert/assets/wall/current/ClaimAxes.lean` (`contractsMatch`, `reportEntries`, `reportFacets`), `aver-cert/assets/wall/current/DeclaredLayout.lean` (`layoutConfirmed`, `fnTypesConfirmed`, `exportNamesDistinct`, `StringFast`, `Chars`), `aver-cert/assets/wall/current/ByteWindow.lean` (`typesLazy`, `exportsLazy`, `codeLazy`), `aver-cert/assets/wall/current/SortedKeys.lean` (`exportsAccountedOf_of_fast`, `closureIsolationL_of_S`), `aver-cert/src/wall.rs` (`render_artifact_bytes`) |
| 8 Totality | `aver-cert/assets/wall/current/GrammarTotal.lean` (`checkTermGroup`, `groupPolicy`, `fn_certified_total_of_check`) |
| 9 Bridges and laws | `aver-cert/assets/wall/current/GrammarBridge.lean` (`Exact`, `Adequate`, `ArgsTyped`, `adequate_transfer`, `exact_of_step`, `bridge_of_step`), `aver-cert/src/bridge_statement.rs` (`render_bridge_statement`, `pinned_from_expanded`, `law_mentioned_bridges`), `aver-cert/src/engine/source_bridges.rs`, `aver-cert/src/engine/law_claims.rs`, `aver-cert/src/verifier.rs` (`validate_law_candidate`, `validate_source_bridge_candidate`, `read_source_encoder`) |
| 10 Witness and audit | `aver-cert/src/verifier.rs` (`checker_witness`, `REPORT_PIN_PREFIX`, `checker_audit`, `AXIOM_WHITELIST`, `CHECKED_ROOT`, `parse_law_audits`, `parse_bridged_law_audits`, `parse_bridge_audits`), `aver-cert/src/checker_audit.lean` |
| 11 Pipeline | `aver-cert/src/verifier.rs` (`verify`, `check`, `explain`, `trusted_check`, `assemble_build`, `kernel_replay_args`), `aver-cert/src/lean_gate.rs` (`REFUSED_WORDS`, `lean_module_root`), `aver-cert/src/lean_process.rs`, `aver-cert/src/cache.rs`, `aver-cert/src/prelude_cache.rs`, `aver-cert/src/main.rs` |
| 12 Trust inventory | the files above, `tests/cert_decode_spec.rs`, `tools/certkit/decode_ref.py`, `tests/cert_intcmp_differential.rs` |
| 13 Versioning | `aver-cert/src/format.rs`, `aver-cert/src/wall.rs` (`current_id`, `resolve`) |
