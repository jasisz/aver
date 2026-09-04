# bridge-talos — audit project for the wall's `wRunF`

Status: AUDIT ARTIFACT. This directory is NOT part of the wall (`aver-cert/assets/wall/`
is the only source `aver-cert/src/wall.rs` embeds and `CURRENT_WALL_ID` hashes), NOT
shipped in any certificate package, and NOT read by the verifier. Nothing under
`aver-cert/src`, `tools/` or `tests/` enumerates `aver-cert/assets/**`; the wall's
`.gitignore` and CI cache keys reach only `assets/wall/current/**`.

What it is: a Lean 4 project that states and proves, for the compute-face profile of
`prompts/brief-talos-bridge.md` (rev 3, §3), that a successful run of the wall's
structural interpreter `CertPrelude.wRunF` is matched by a normally terminating run
of Talos — an independent small-step WebAssembly interpreter in Lean — on the
translated instruction list. Direction: wall ⇒ Talos (refinement), one way. The wall
side is a VERBATIM copy of the relevant `CertPrelude` lines (`Bridge/AverMin.lean`
names the source lines and the wall id).

## Pins

- Talos: `https://github.com/cajal-technologies/talos.git` at commit
  `a6a34484c1cbe65b6e9b38dfaa24b8bb6ca06d27` — the head of Talos PR #232
  (`codex-semantic-spec-interface`, open on 2026-09-04), which adds
  `Interpreter/Wasm/Host/Run.lean` (`ExportCall`, `startExportConfig?`,
  `RunsExportWith`); previously `6fd26867bc11b57f5f18c2ca834195d055e14d69` (main,
  2026-09-02). Lean package `interpreter/`, pulled with Lake's `subDir`. AGPL-3.0;
  nothing of it is bundled or embedded here — Lake fetches it at build time. When the
  PR merges, move the pin to the merge commit; if the branch is rewritten, the pinned
  sha still resolves as long as GitHub keeps the object.
- Mathlib comes transitively through Talos's own `lakefile.toml` (`v4.32.2`, resolved
  rev in `lake-manifest.json`).
- Toolchain: `leanprover/lean4:v4.32.2` (Talos's; the wall is on the same release).
- Wall id of the copied `CertPrelude`/`SchemaCore` lines:
  `sha256:4331f2e67c965b3c6aca0121a1d46406f3a8a084fe80deecc8d8745df56f9e3f`
  (`aver-cert/src/format.rs CURRENT_WALL_ID`).

## Build

```
cd aver-cert/assets/bridge-talos
lake update                      # clones Talos + Mathlib; Mathlib's post-update hook
                                 # fetches the olean cache (`lake exe cache get`)
LEAN_NUM_THREADS=6 lake build Interpreter.Wasm.Host.Run   # Talos, ~4 min on 6 threads
LEAN_NUM_THREADS=6 lake build   # the bridge itself, seconds
```

`.lake/` is ignored and is large (about 7.6 GB, almost all Mathlib; the bridge's own
`.lake/build` is 28 MB). The build has no network needs after `lake update`. A clean build
of the bridge modules alone takes about ten seconds (9.8 s wall on 6 threads, 18 modules;
`AverMin` 2.5 s, `Coverage` 1.4 s, `Bridge` 1.1 s, everything else under a second);
`lake build Bridge.Axioms` prints the axiom audit, `lake build Bridge.Smoke` the smoke
comparison, `lake build Bridge.Tripwire` the profile enumeration. The files written after
the first experiment (`Coverage`, `Tripwire`, `Contracts`, `Adapter`, `Accepted`, `Smoke`)
set `autoImplicit false`; the earlier ones do not yet.

## Layout

| file | content |
| --- | --- |
| `Bridge/AverMin.lean` | verbatim `CertPrelude` lines 78–395 (`WVal`, `WInstr`, `wRunF`, `wFuncN`, `initLocals`, …) and `SchemaCore` `HostRole`/`TypeDecl`/`checkRecordDecl` |
| `Bridge/Rel.lean` | value relation `R`/`Rs` (heap-indexed), monotonicity under heap extension, the LOCALS relation `RLocals` with Talos's split `params`/`locals` |
| `Bridge/Env.lean` | `TranslateEnv` (imports, struct sorts, carrier), value sorts `HasSort`/`Sorted`, `envOfClaim` (projection of host-role table + certified `TypeDecl`s) |
| `Bridge/Translate.lean` | `translate`/`translateList` for the profile, the stack typing `HasTy`, `typed_run` (typed wall runs return `.ok`, keep the stack beneath, preserve sorts) |
| `Bridge/Config.lean` | synthetic module (one function exported as `exportName`, host slots as imports, declared struct types), initial configuration (`initConfig_synth`: Talos's `initConfig` builds it), the `HostSimulation` assumption |
| `Bridge/HostCall.lean` | spike (a): `bridge_hostCall` — a wall `call` of a host slot is one Talos `callHostReturn` step |
| `Bridge/IfElse.lean` | spike (b): `bridge_ifElse` — a wall `ifElse` is `Step.iff`, the branch, `Step.exitControl` |
| `Bridge/EnvOfClaim.lean` | spike (c): `envOfClaim` is a projection of the declared envelope (host half, struct half), the k5 instance |
| `Bridge/Instr.lean` | one `Step` per remaining profile instruction: locals, constants, `structGet`/`structNew`, `refIsNull`, the nine comparisons |
| `Bridge/Bridge.lean` | `bridge_run` (induction on `HasTy`, framed) and the export theorem `wFuncN_terminatesWith`, stated as Talos's `TerminatesWith` over `initialConfig` |
| `Bridge/Coverage.lean` | the coverage lemma (brief §3): `lowerExprFragmentBody carrier plan = some instrs` for a plan in the profile ⇒ `HasTy env (Γof plan.params) [] instrs [sortOfFragTy plan.result]` and `translateList env instrs = some _`, by induction over the wall's `lowerNodesFuel`/`lowerBlockFuel` with the checker's facts read one node at a time |
| `Bridge/Tripwire.lean` | fail-closed enumeration: `wInstrInProfile` (every `WInstr` constructor) with `translate_eq_none_of_out`, and 27 checked sample plans, one per `FragNodeKind` constructor, lowered by the wall and translated (`#eval` fails the build on disagreement) |
| `Bridge/Contracts.lean` | `HostSorts_of_contracts`: `HostSorts` for the compute face's real host table (`StandardFace.recordComputeSlots`) from the five `Obligation.holds` contract hypotheses (`ComputeContracts`, verbatim) and distinct indices; `hostTableBound_nodup`: the `Nodup` the host half needs is the claim check's `hostTableIndicesDistinct` |
| `Bridge/Adapter.lean` | the concrete Talos host: `adapterEnv` reads each slot's arguments back along their sorts, applies the wall's abstract contract function, reifies the result into the heap; `HostSimulation_adapter` (any machine-shaped table) and `HostSimulation_recordCompute` (the compute face's `recordComputeSlots` under the contracts) |
| `Bridge/Accepted.lean` | the profile half of brief §9 (4): `planInProfile_of_recordCompute` (a plan the compute-face classifier admits is in the profile relative to `envOfClaim`); the three hypotheses the envelope lacks are named in its header |
| `Bridge/RunsExport.lean` | the composition sentence at Talos's export boundary: `startExportConfig?_synth` (Talos's `startExportConfig?` on sorted, related arguments enters exactly `initialConfig`) and `recordCompute_runsExport` (over the declared envelope of an accepted record projection-compute claim, `RunsExportWith (adapterEnv …) (synthModule (envOfClaim …) …) exportName call (fun ret => ∃ v, ret.values = [v] ∧ R ret.final.gcHeap v w)` for the wall's `wFuncN` result `w`) |
| `Bridge/Axioms.lean` | `#print axioms` of every theorem (all `[propext, Classical.choice, Quot.sound]` or fewer) |
| `Bridge/Smoke.lean` | the k5 claims' PLANS (verbatim from the certificate package) lowered by the wall, translated over `envOfClaim`, entered through `startExportConfig?`, run in Talos with the adapter host over the reference faces and compared with `wFuncN`; the declared-data hypotheses of `recordCompute_runsExport` evaluated on the k5 data (not part of the proof) |

## Log

- **Setup.** Lake project pinned to Talos `6fd26867`; `lake update` 1.5 min (Mathlib
  cache already present locally: 8639 files decompressed in 17 s); Talos
  `Interpreter.Wasm.SmallStep` + `Decoder.Wat` + `Validate` 5.5 min wall on 6 threads
  (`SmallStep` alone 201 s). `.lake` 7.5 GB.
- **Step 1 — `AverMin.lean`.** Verbatim copy, checked by `diff` (command in the file
  header). Nothing trimmed from `WInstr`/`wRunF`; the profile restriction lives in
  `translate`.
- **Step 2 — `Rel.lean`.** Probe's `R`/`Rs` and lemmas, plus `Rs_set`, `Rs_append_inv`,
  prefix monotonicity (`R_prefix`), inversions (`R_structv`, `R_null`, `R_i64v`,
  `R_i32v`, `R_b32`), the locals relation (`RLocals`, `Locals.get_eq`,
  `Locals.set?_eq`). No block/label relation is needed as a separate object: Talos's
  control stack is universally quantified in every lemma and `ifElse` pushes and pops
  exactly one `.block` frame (see `IfElse.lean`).
- **Step 3 — `Env.lean`, `Translate.lean`.** `translate` is `none` outside the profile.
  Finding: the wall's `wRunF` is untyped where Talos is not (`refIsNull` on a number,
  `localSet` past the end, an `if` branch consuming the stack beneath it), so the
  bridge needs a stack typing; `HasTy` is that judgment, with `if` branches typed from
  the empty stack (wasm's rule). `typed_run` is proved: a typed run returns `.ok`,
  touches only the typed prefix, preserves sorts. The sort of a struct field comes
  from the DECLARED struct table (`HasSort` is table-indexed), and host results must
  be well-sorted (`HostSorts`) — the wall's contracts give that for represented
  operands only. Deriving `HasTy` from `PlanCheck` for lowered plans is the coverage
  lemma of brief §3 and is NOT done here.
- **Step 4 — `Config.lean`.** Synthetic module + `initialConfig`;
  `initSingleModuleConfig_synth` shows it is what Talos's own entry point builds.
  `HostSimulation` stated (heap frame as a prefix relation).
- **Step 5 — spike (a), `HostCall.lean`.** CLOSED in one round (`bridge_hostCall`,
  elaboration 0.3 s). Talos rule: `Step.callHostReturn` (SmallStep.lean:3558–3573).
  The heap-frame half of `HostSimulation` is used (`Rs_prefix` on the locals and the
  rest of the stack); without it the lemma does not close.
- **Step 6 — spike (b), `IfElse.lean`.** CLOSED in one round (`bridge_ifElse`, 0.4 s).
  Talos rules: `Step.iff` (3499–3516) and `Step.exitControl` (3261–3270). The block
  type is inert for `Step` (arities only). The branch is discharged through a
  hypothesis in the shape of the whole-list lemma (`BranchSim`), so nested `ifElse`
  is the same lemma one level down; the wall-side condition is `BranchPushesOne`,
  which `typed_run` supplies for typed branches.
- **Step 7 — spike (c), `EnvOfClaim.lean`.** CLOSED in one round (0.3 s). Host half:
  every import of the synthetic module is an entry of the claim's host-role table with
  the role's fixed signature (`envOfClaim_import_role`), and the index the wall's own
  `hostRoleIdx?` resolves a role to is that import (`hostRoleIdx?_slotLookup`, needs
  distinct indices in the table). Struct half: the environment's field sorts for a
  certified record are the sort projection of exactly the type-section entry
  `lowerTypeDecl` produces — the entry `StandardFace` pins by equality
  (`declEntry?_lowerTypeDecl`). No byte is read. WHAT THE ENVELOPE LACKS: a
  symbolic-fragment claim names user structs only by `structTable` (name → index);
  the field layout of a struct index comes from a SEPARATE `typedecl-v1` record claim
  of the same certificate (k5: `Fraction` = index 0 from the `zeroFraction`/
  `oneFraction` record declarations), so `envOfClaim` takes the certificate's record
  declarations as an input; a struct index cited by a compute plan and by no record
  claim has no declared layout and `translate` refuses its `structNew`. Also erased but
  harmless: `if` block types (inert for `Step`), local types (fixed by
  `PlanBytes.singleCarrierLocalBodyBytes`: one nullable carrier reference).
- **Step 8 — assembly, `Instr.lean` + `Bridge.lean`.** CLOSED. `bridge_run` is one
  induction on the typing derivation (twelve cases), stated FRAMED (typed prefix over an
  arbitrary stack beneath), which is what lets an `if` branch — typed from the empty
  stack — reuse the lemma at the empty prefix; nested `ifElse` needs nothing more.
  `wFuncN_terminatesWith`: `wFuncN code host fuel self vs = some w` ⇒ `∃ trace v store',
  Steps (initialConfig …) trace ⟨.done [v], store'⟩ ∧ R store'.wasm.gcHeap v w`, and the same
  as Talos's `TerminatesWith`. Premises: `HostSimulation`, `HostSorts`, `HasTy` for the
  body, `translateList = some`, sorted and related arguments. `#print axioms` on every
  theorem: `[propext, Classical.choice, Quot.sound]` (some fewer). Elaboration: whole
  bridge under 10 s cold; `Bridge.lean` 1.0 s, `Instr.lean` 0.7 s, `Translate.lean` 0.9 s.
- **Step 10 — coverage, `Coverage.lean` + `Tripwire.lean` (brief §9 (1)).** CLOSED.
  `AverMin.lean` gains the verbatim plan grammar, checker (`checkBlockFuel`,
  `checkExprFragmentRawPlan`) and lowering (`lowerNodesFuel`/`lowerBlockFuel`,
  the sign template); diff commands in the file header. The checker's local
  `let inferNodeKindTy` is given a name and `checkBlockFuel (cf+1) params block`
  is `rfl`-equal to `checkNodes (inferNodeKindTy cf params) [] block.nodes && …`
  (`checkBlockFuel_succ`) — the one place the copy is trusted. `coverage`:
  the lowerer's symbolic stack IS the typing context (`σ = stack.map (sortAt
  nodes)`, `sortOfFragTy : FragTy → STy`), induction on the lowering fuel with
  the checker's and the profile's fuel universally quantified inside (the
  lowerer burns fuel per node, the checker per nesting level); `ifElse` is the
  block lemma one level down, `intSignCmp` is typed once against the carrier
  layout (`HasTy_intSignCmpTemplate`, `localSet` of `params.length`).
  `HasTy_translates` closes the second half (the typing rules' side conditions
  are `translate`'s). Findings: (a) the checker types `constI64`/`constI32` at
  ANY `Int` — the machine band is a profile condition (`nodeInProfile`), which
  the record-compute classifier does pin (`recordComputeNodeOk`); (b)
  `translate` is type-blind on `structGet`, so a `structGetUser` of an
  UNDECLARED struct translates and is refused only by `HasTy` — the coverage
  lemma, not the translation, is the gate (tripwire sample); (c) the
  `structNew`/`structGetUser` conditions of `nodeInProfile` (declared sorts =
  argument sorts, declared field at the index) are exactly the envelope gap of
  brief §9. Elaboration: `Coverage.lean` 1.2 s, `Tripwire.lean` 0.5 s; axioms
  `[propext, Quot.sound]`.
- **Step 11 — sorts refined to the contracts' domains; `HostSorts` derived (brief §9 (2)),
  `Nodup` derived (§9 (5)).** CLOSED. The wall's `_hadd/_hsub/_hmul` speak about
  REPRESENTED operands and `_hCmp/_hEq` about CANONICAL ones; the old `HostSorts`
  asked for well-sorted results on every `.ref` argument (null, any struct), which no
  contract gives. The sort language now says what the contracts need: `STy.car` = a
  represented canonical carrier under the wall's `CarrierSpec S` (`HasSort env S w
  .car := ∃ n, S.Repr n w ∧ S.Canon w`), `STy.i64b` = an `i64` band literal (the one
  operand shape under which `boxRef` returns a canonical word, `canonSmall`), and
  every sort-carrying object (`HasSort`/`Sorted`/`HasTy`/`typed_run`/`bridge_run`/
  `HostSimulation`/`HostSorts`/the export theorems) takes `S`. Inclusion (`SubSort`:
  `i64b ≤ i64`, `car ≤ ref` — the carrier's fields are sorted by its declared layout,
  `CarrierDeclared`) enters at exactly three typing rules: `localSet` (the sign
  template stores a carrier into the `.ref` scratch local), the `structGet` receiver
  (`IsRef`), and the `if` join; no general weakening rule exists. `roleSig`: `box :
  [i64b] → car`, `add/sub/mul : [car, car] → car`, `cmp/eq : [car, car] → i32`;
  `scalarSort .intCarrier = .car` (a record's Int field is a canonical carrier by the
  compute face's domain representation, `SRepr`). The coverage lemma follows suit:
  `sortAt` reads the node KIND (`sortOfNode`: an `i64` literal node is `.i64b`), the
  profile demands that `box` box a literal (the emitter's `i64.const k; call box`
  idiom), and `coverage` concludes at the result node's sort with `SubSort` to the
  declared result sort. `Contracts.lean`: `HostSorts_of_contracts ht C decls S add sub
  mul cmp eq (hc : ComputeContracts S add sub mul cmp eq) (hnd :
  hostTableIndicesDistinct ht = true) : HostSorts (envOfClaim ht C decls) S
  (recordComputeSlots C add sub mul cmp eq ht)` — slot by slot
  (`recordComputeSlots_getElem`, with distinct indices), `box` by `smallIntro` +
  `canonSmall`, `add/sub/mul` by the contracts' `Repr ∧ Canon` conclusion, `cmp/eq`
  by their exact `i32` conclusion, `toIndex` vacuous (trap-only slot). And
  `hostTableBound_nodup`: `hostTableIndicesDistinct` IS `natListNoDup` of the index
  column, so the `Nodup` of §9 (5) is a claim-check fact the wall already imposes
  (first conjunct of `StandardFace.hostTableBound`), not a new hypothesis.
  `HostSimulation.invoke` also gained the premise `Sorted env S ws sig.params` (the
  typed run only calls with sorted arguments; a concrete host reads carrier shapes
  off the sorts — Step 12). Elaboration: `Contracts.lean` 0.3 s, `Bridge.lean` 1.1 s;
  axioms `[propext, Classical.choice, Quot.sound]` or fewer, unchanged.
- **Step 12 — the concrete host, `Adapter.lean` (brief §9 (3), the adapter alternative of
  §4.3).** CLOSED. `adapterFn sig hf` is a Talos `HostFn` whose `invoke` reads the
  arguments back into wall values ALONG THEIR SORTS (`readArg`: `i32`/`i64` words are
  their integers, a `.car` argument is the carrier struct read off the heap with its limb
  array read as `i32` words; `readArgs_of_Rs`: sorted, related arguments read back
  exactly), applies the abstract `hf`, and REIFIES a defined result into the heap
  (`reify`: numbers become words, structs and arrays are allocated bottom-up, the old
  heap is a prefix — `reify_spec`); it traps where the contract is undefined.
  `adapterEnv env host` wires every import of `env` to the wall's table entry at its
  function index. `HostSimulation_adapter`: for any table whose results on sorted
  arguments are machine-shaped, over an environment whose imports take no `.ref`
  argument; `HostSimulation_recordCompute`: the instance for `recordComputeSlots` under
  the verbatim contracts (`recordComputeSlots_machine`: `add/sub/mul` results are
  represented hence machine words, `cmp/eq` return `cmpW/eqW ∈ {-1,0,1}`, `box` returns
  `carrierSmall C k` for a band `k`). ONE PREMISE BEYOND THE CONTRACTS, about the
  representation: `CarrierMachine S` — the carrier specification's words are wasm words
  (`i64` small and `i32` sign in band, limbs `null` or an array of in-band `i32` words).
  `CarrierSpec.car` fixes the three-field shape but not the bands or the limb element
  type, and `Obligation.holds` quantifies over every `CarrierSpec`; without it no Talos
  value relates to a represented word with an out-of-band small field, so no host could
  simulate the abstract table. The runtime's representation satisfies it (`wat/*.wat`,
  limbs are `(array (mut i32))`). The abstract `HostSimulation` stays the theorem's
  interface; `HostSimulation.invoke` carries the `Sorted` premise since Step 11.
  Elaboration: `Adapter.lean` 0.5 s; axioms `[propext, Classical.choice, Quot.sound]`.
- **Step 13 — composition over the accepted artifact, `Accepted.lean` (brief §9 (4)).**
  CLOSED, with three named extra hypotheses. Hypotheses taken in the wall's own shapes
  (verbatim in `AverMin.lean`): `hostTableBound roles ht`, the classifier's three Bool
  facts (`recordComputeNodeOk` on every node, every cited struct index is the face's,
  `planTypedB`), `params` all `adtRef`, an all-Int non-empty `checkRecordDecl` record
  declaration at the face's index (the declaration `typeSectionMatches` pins), and
  `lowerExprFragmentBody carrier plan = some body`. `planInProfile_of_recordCompute`
  derives the profile predicate node by node (the host half via `hostRoleIdx?_slotLookup`
  with the claim check's `Nodup`; the only `.i64`-typed nodes of an admitted plan are
  `constI64` literals, so `box` boxes a literal; the struct half from the record
  declaration), and `recordCompute_terminatesWith` composes it with `coverage_envOfClaim`,
  `HostSorts_of_contracts`, `HostSimulation_recordCompute` and `wFuncN_terminatesWith`:
  the Talos configuration is exactly `synthModule (envOfClaim ht C [.record structIdx
  fields]) (params.map sortOfFragTy) t 1 body'` with `translateList … body = some body'`
  and the adapter host, and `envOfClaim` consumes nothing but the role table, the carrier
  index and the pinned record declaration. WHAT THE DECLARED ENVELOPE LACKS (stated, not
  invented): (1) the band of `constI32` literals — `recordComputeNodeOk` pins `constI64`
  and the sign literal only; (2) struct ARITY agreement — `structNew`'s operand count
  and `structGetUser`'s field index against the declaration's field count (both
  byte-pinned separately, never related: a mismatch is invalid wasm the wall does not
  validate) — this is the envelope gap of brief §9 made precise; (3) `structIdx ≠ carrier`
  (byte-derived from the two entries' shapes, not declared). Elaboration 0.7 s; axioms
  `[propext, Classical.choice, Quot.sound]`.
- **Step 14 — smoke through the claim path, `Smoke.lean`.** The three k5 plans verbatim
  from the certificate package (`scratchpad/k5b/cert/Plans.lean`, not in the repo),
  lowered by `lowerExprFragmentBody 3` (equal to the package's `Module.lean` bodies,
  checked), translated over `k5Env = envOfClaim k5HostTable 3 [Fraction]`, run in Talos
  with `adapterEnv` over `recordComputeSlots 3 … k5HostTable` wired to the small-int
  reference faces: `plus(1/2,1/3)` = 5/6 (18 Talos steps), `plus(7/9,-2/5)` = 17/45 (18),
  `isNonNeg` on 1/2, -3/4, 0/1 (16 each), `lessThan` both ways (26 each) — 7/7 agree with
  `wFuncN`; and all 12 declared-data hypotheses of `recordCompute_terminatesWith`
  (including the three extra ones) evaluate to `true` on `plus`, `lessThan`, `isNonNeg`.
  Elaboration 0.6 s.
- **Step 9 — smoke, `Smoke.lean`.** The three k5 bodies (verbatim `WCode` from the
  package's `Module.lean`) agree between `wFuncN` (small-int faces) and Talos
  (`translate` + `runSteps`, heap host): `plus(1/2,1/3)` = 5/6 (18 Talos steps),
  `plus(7/9,-2/5)` = 17/45, `isNonNeg` on 1/2, -3/4, 0/1, `lessThan` both ways. Smoke only.
- **Step 15 (2026-09-04) — Talos's export boundary, `RunsExport.lean`; pin moved to PR #232.**
  CLOSED. Pin: `lakefile.toml` now names the head of Talos PR #232
  (`a6a34484c1cbe65b6e9b38dfaa24b8bb6ca06d27`, `codex-semantic-spec-interface`); `lake
  update Interpreter` fetched it directly (Mathlib cache hit, 17 s), `Interpreter.Wasm.Host.Run`
  built in one go (SmallStep 222 s on 6 threads, Host.Run 0.4 s); the unchanged bridge
  built green against it, so nothing was vendored. Baseline `wc -l`: 17 files, 6438 lines
  (Accepted 352, Bridge 498, Config 183, Axioms 44, Smoke 218).
  WHAT `startExportConfig?` EXPECTS: `env : HostEnv α` — the adapter `adapterEnv α env host`
  IS one, no wrapping; it builds `initConfig { module := m, host := env } entry call.initial
  call.arguments` after `m.findExport op = some entry`, `m.funcSig? entry = some sig`,
  `call.arguments.length == sig.params.length` and the private `exportArgumentsMatch`
  (each argument, source order, matches its declared parameter type; `.anyref _` matches
  `.anyref`). Two consequences for `Config.lean`: (a) the synthetic module now EXPORTS its
  function (`exports := [{ name := exportName, funcIdx := env.imports.length }]`; the name
  is fixed, `"aver"`, since Talos resolves it to the entry index and nothing else reads it);
  (b) `synthInstance` is `{ module := m, host := hostEnv }` with `resolvedImports` at its
  default, exactly the instance `startExportConfig?` builds — `Step` consults
  `resolvedImports` only for an import WITHOUT a host function (`callCrossInstance`,
  `hnoHost : currentHost.funcs.length ≤ functionIndex`, SmallStep.lean:3377–3381), and every
  import here has one. `ExportCall.ofHost` does not apply: it fixes `initial :=
  m.initialStore` (empty heap), and the arguments are records living in the heap; so the
  theorem takes an arbitrary `call : ExportCall α` with `Rs call.initial.gcHeap
  call.arguments.reverse vs` (Talos's operand-stack order, top first — their ABI note, stated
  once). THE THEOREM: `recordCompute_runsExport` concludes `RunsExportWith (adapterEnv α env
  host) (synthModule env (params.map sortOfFragTy) t 1 body') exportName call (fun ret => ∃ v,
  ret.values = [v] ∧ R ret.final.gcHeap v w)` — `RunsExportWith`, not `…Outcome`: the wall's
  run gives an actual `.done` trace and `Step` is deterministic, so a trap on the same call is
  impossible, not merely unobserved. Proof: the old composition + `startExportConfig?_synth`
  (`findExport`/`funcSig?` of the synthetic module by `simp`; the argument guard from
  `exportValueMatches_of_R`: a sorted, related argument of each profile sort matches
  `valueTypeOf` of that sort — the two private guards are named with Batteries'
  `open private … from Interpreter.Wasm.Host.Run`; then `initConfig_synth`). Elaboration
  0.6 s; axioms `[propext, Classical.choice, Quot.sound]` (`startExportConfig?_synth`:
  `[propext, Quot.sound]`). REDUNDANT NOW, deleted: `wFuncN_TerminatesWith` (Bridge.lean, 19
  lines — `wFuncN_terminatesWith` states `TerminatesWith` directly, which is what
  `RunsExportWith` wraps); `recordCompute_terminatesWith` (Accepted.lean, 67 lines — its
  `∃ trace v store', Steps (initialConfig …) …` conclusion is recovered from
  `recordCompute_runsExport` by `startExportConfig?_synth`, so the composition sentence lives
  once, at the boundary); `initSingleModuleConfig_synth` became `initConfig_synth` (same
  proof, over the instance the boundary builds — `initSingleModuleConfig` is no longer on the
  path). No determinism lemma of our own existed (`RunsExportWith.deterministic` is Talos's
  to use); argument-order plumbing: the theorem takes `call.arguments` as Talos hands them
  and the only `.reverse` left is in the relation to the wall's list. `Smoke.lean` enters
  through `startExportConfig?` now (its guards evaluate on the k5 calls): 7/7 agree, 12/12
  declared-data hypotheses hold. After: 18 files, 6564 lines (Accepted 281, Bridge 479, Config
  196, Axioms 45, RunsExport 202, Smoke 218) — net +126, of which the new file's header and
  the boundary lemmas; the composition theorem itself moved, not grew.
  THE ARITY QUESTION (Q4). `startExportConfig?` checks the export name, the argument COUNT
  and the argument TYPES of the call (Host/Run.lean: `guard (call.arguments.length ==
  signature.params.length)`, `guard (exportArgumentsMatch m call signature)`) — nothing about
  the body. `Config.Safe`/`ValidConfig` (SmallStep.lean:6974–6980) is semantic — "no
  reachable configuration makes `stepChecked?` fail" — not a syntactic validator, and it is
  not a premise of `RunsExportWith`. Talos's module validator DOES check both struct arities:
  `Module.validate` (Validate.lean:1496) runs `m.checkFuncStraight f` on every function
  (1627–1629), which stack-types the body with `Instruction.straightSig` (941): `.structNew t
  ↦ (m.structFields? t).map fun fs => ((fs.map (·.storage.vt)).reverse, [.ref false
  (.concrete t)])` (1133–1136) pops exactly the declared field count, and `.structGet t f ↦
  (m.structField? t f).map …` (1127–1129) needs the field to exist. WHERE IT STOPS SHORT:
  `straightSig` returns `none` for an instruction it does not model and `Program.checkTypes`
  then ACCEPTS the whole function ("Unsupported instructions conservatively make the check
  succeed", 1466–1467; `| none => pure none` 1459–1460, `| none => return none` 1462) — so a
  `struct.get` whose field index is out of range on a declared struct passes
  `checkFuncStraight` (only `struct.set` fields are checked separately, 1593–1596), and any
  unmodelled instruction earlier in the body switches the arity check off for the rest.
  More to the point, NONE of it is reachable from the theorem: `RunsExportWith` never
  invokes `Module.validate`, and the module it would validate is the SYNTHETIC one, whose
  type section is generated from the same declaration the translation reads — `translate`
  already refuses a `structNew` whose operand count differs from the declared field count
  (Translate.lean:61–63) and `HasTy` types `structGetUser` against the declared sorts, so a
  Talos validation of `synthModule` could only re-derive what `translateList … = some body'`
  already says. The hypothesis `harity` is about the PLAN versus the pinned declaration,
  needed before any wasm exists; Talos never sees the plan. KEPT. Same verdict for `hi32`:
  Talos's `.const` carries a `UInt32` (Syntax.lean:279), always in band; the band condition
  is on the wall's unbounded `Int` literal and is what lets `translate` emit the word at all
  (Translate.lean:59) — nothing on Talos's side can speak to it. KEPT. And `hne : structIdx ≠
  carrier`: a consistency condition between two of OUR inputs to `envOfClaim` (which entry
  `structSorts?` finds at the index), not a validity condition of any module; Talos's
  `gcTypeRefs` range check (1584–1585) sees one type section and cannot notice it. KEPT.

## What is NOT proved here (the list, not an estimate)

Items 1–5 of the first experiment are closed (Steps 10–13). What the export theorem still
takes as a premise, and where each premise comes from:

1. **The wall's contracts** — `ComputeContracts S add sub mul cmp eq`, verbatim the five
   arithmetic/comparison hypotheses of `Obligation.holds`. As in the wall: assumed runtime
   laws, validated empirically (`tests/cert_intcmp_differential.rs`). Phase 3 territory.
2. **`CarrierMachine S`** — the carrier specification's words are wasm words (in-band
   `i64` small and `i32` sign, limbs `null` or an array of in-band `i32` words). A premise
   about the representation `Obligation.holds` abstracts over; the runtime's satisfies it.
   Without it no Talos value relates to a represented word at all.
3. **The three envelope hypotheses of `Accepted.lean`** — `constI32` band, struct arity
   agreement, `structIdx ≠ carrier`. Byte-derivable in principle, not declared; the
   arity one is the real gap (a mismatch is invalid wasm the wall never validates).
   Talos's validator would catch the arity mismatch in a module it validates, but
   `RunsExportWith` does not validate, and the module here is the synthetic one (Step 15).
4. **Sorted arguments** — `Sorted env S vs (params.map sortOfFragTy)`: records whose Int
   fields are represented canonical carriers. The wall's `recordComputeDomRepr`
   (`SReprAll`) says exactly that; the one-line bridge `SRepr → HasSort` is not written.
5. **The pins are consumed as declared data.** `typeSectionMatches`, `PlanBytes`,
   `carrierState` bind the declaration, the body and the carrier index to the bytes; the
   bridge reads the declaration, the lowered body and the index, never a byte. The
   composition sentence therefore says: the Talos module the theorem is about is built
   from exactly what those pins pin — not that Talos has decoded the artifact. Its export
   is entered through Talos's own boundary (`startExportConfig?`), on a call whose
   initial store carries the argument records; `ExportCall.ofHost` (empty initial store)
   is not the shape of a call with reference arguments.
