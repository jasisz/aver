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
  `6fd26867bc11b57f5f18c2ca834195d055e14d69` (Lean package `interpreter/`, pulled with
  Lake's `subDir`). AGPL-3.0; nothing of it is bundled or embedded here — Lake fetches
  it at build time.
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
LEAN_NUM_THREADS=6 lake build Interpreter.Wasm.SmallStep   # Talos, ~4 min on 6 threads
LEAN_NUM_THREADS=6 lake build   # the bridge itself, seconds
```

`.lake/` is ignored and is large (about 7.5 GB, almost all Mathlib). The build has no
network needs after `lake update`.

## Layout

| file | content |
| --- | --- |
| `Bridge/AverMin.lean` | verbatim `CertPrelude` lines 78–395 (`WVal`, `WInstr`, `wRunF`, `wFuncN`, `initLocals`, …) and `SchemaCore` `HostRole`/`TypeDecl`/`checkRecordDecl` |
| `Bridge/Rel.lean` | value relation `R`/`Rs` (heap-indexed), monotonicity under heap extension, the LOCALS relation `RLocals` with Talos's split `params`/`locals` |
| `Bridge/Env.lean` | `TranslateEnv` (imports, struct sorts, carrier), value sorts `HasSort`/`Sorted`, `envOfClaim` (projection of host-role table + certified `TypeDecl`s) |
| `Bridge/Translate.lean` | `translate`/`translateList` for the profile, the stack typing `HasTy`, `typed_run` (typed wall runs return `.ok`, keep the stack beneath, preserve sorts) |
| `Bridge/Config.lean` | synthetic module (one function, host slots as imports, declared struct types), initial configuration, the `HostSimulation` assumption |
| `Bridge/HostCall.lean` | spike (a): `bridge_hostCall` — a wall `call` of a host slot is one Talos `callHostReturn` step |
| `Bridge/IfElse.lean` | spike (b): `bridge_ifElse` — a wall `ifElse` is `Step.iff`, the branch, `Step.exitControl` |
| `Bridge/EnvOfClaim.lean` | spike (c): `envOfClaim` is a projection of the declared envelope (host half, struct half), the k5 instance |

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
