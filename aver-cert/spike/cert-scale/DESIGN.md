# Certificate checking at scale (spike)

Status: design spike, measure-and-prototype only. Nothing here is wired into the checker. Branch `spike/cert-scale`, prototype under `aver-cert/spike/cert-scale/`.

Subject: btc-listener at `5698c8e`, built with the `aver` of `origin/main` at `edc6b70e6` (`aver compile --target wasm-gc --certify`). Module: 799,495 bytes, 62 imports, 5,451 defined functions, 833 plans (823 certified exports), 3,701 exports, 5,512 top-level type entries (the first a rec group of 1,140 subtypes), 846 functions in the certified closure. Machine: 12 cores, 32 GB, Lean 4.34.0.

## 1. Verdict

The module bytes are not the problem. Materialising the whole 800 KB module as one `Nat` costs 0.1 s of kernel time (`framing_ok`, `start_ok`, `envelope_ok` all take 0.11 s). The cost comes from four things, in this order:

1. **Monolithic declarations.** One `decide +kernel` that walks a whole section, or all plans, keeps every intermediate term in the kernel's caches until it ends. `plans_ok` costs 368 s and 15.1 GB as one declaration; its eleven conjuncts cost 80 s together when each is its own declaration, and none needs more than 9.2 GB.
2. **Per-byte structural decoding.** The decoders turn bytes into `List Nat`, `Option`, `Prod` and constructor trees one byte at a time. That runs at roughly 0.3 ms per byte (closure scan: 54 KB in 18 s) and dominates everything that decodes.
3. **Quadratic algorithms in the kernel.** `indicesDistinct` (`decide List.Nodup` over 844 numbers: 34 s, 9.2 GB), `msort` of 3,701 export keys (35 s, where a linear `strictly` over the same keys in sorted order takes 1.4 s), `groupMembers` filtered once per obligation in the report pins (`report_pin_9`: 63 s, 12.1 GB), and `String.toList` of name literals (45 ms per name, about 170 s for all 3,763 names).
4. **Repeated work.** `axes_ok` lowers all 833 plans again (17 s); the audit collects axioms from scratch for each of about 700 roots (246 s).

The redesign below removes all four from the byte path without changing what acceptance says. On btc the byte path, part measured on prototypes and part estimated from split measurements, comes to about 240 s serial with a peak of 1.6 GB, against 687 s and 15.1 GB today. Every prototyped declaration fits in 1.6 GB, so the byte path can run with several Lean workers in parallel. The byte path is then no longer the bottleneck: the source bridges (392 s, `BridgeProof` 9 GB), the witness report pins (116 s, 12.1 GB) and the audit (246 s) are, and section 9 covers them.

## 2. Baseline

`aver-cert check` with `AVER_CERT_PHASE_TIMEOUT_SECS=9000`, one Lean worker (the default):

| Phase | Wall | Peak RSS |
|---|---|---|
| certificate proof build (`lake build`, 230 modules) | 1,283 s | 15.1 GB |
| artifact witness check | 112 s | 12.1 GB |
| artifact audit | 246 s | < 1 GB |
| total | 1,642 s | 15.1 GB |

The build, by module group (lake's per-module times):

| Group | Wall |
|---|---|
| byte facts (`Artifact`, `ArtifactInterface`, `ArtifactStrings`, `ArtifactLayout`, `ArtifactClosure`, `ArtifactHostRoles`, `ArtifactPlanCheck`) | 572 s |
| source bridges (`BridgeProof`, `BridgeDefs`, `BridgeSteps*`, `Bridge`) | 392 s |
| source model (`AverModel.*`) | 142 s |
| plan chunks (`ArtifactPlans*`, 27 modules of 32 plans) | 116 s |
| wall | 41 s |
| package data (`Plans`, `Manifest`, bytes) | 13 s |

Every heavy theorem, re-elaborated alone in the kept build directory (`lake env lean --profile`, `/usr/bin/time -l`). Import of the byte-fact closure alone is 0.9 s and 0.67 GB; kernel time is the profiler's "type checking", elaboration is everything else.

| Declaration | What it reduces | Kernel | Elab | Peak RSS |
|---|---|---|---|---|
| `plans_ok` (all of `plansAcceptedRestL` at once) | type section, rec group, data section, all plans' data pins, inhabitation, `Nodup` | 367 s | 0 | 15.1 GB |
| `strings_ok` | type signatures, function section, all 5,451 code headers | 58 s | 0 | 12.7 GB |
| `exports_ok` | export section, 3,763 names as char lists, `msort` | 43 s | 25 s | 10.7 GB |
| `types_cut` | whole type section on windows | 27 s | 0 | 5.9 GB |
| `closure_ok` | 846 code entries, instruction scan | 23 s | 0 | 6.1 GB |
| `axes_ok` | lowers all 833 plans again | 17 s | 0 | 4.3 GB |
| `roles_ok` | 8 helper templates, helper export names, carrier | 14 s | 0 | 4.9 GB |
| `code_cut` | all 5,451 code entry windows | 13 s | 0 | 3.7 GB |
| `plans_chunk_640` (one of 27) | 32 plans, `entryFast` | 8.2 s | 0.1 s | 3.5 GB |
| `layout_ok` | function section, code locations | 5.7 s | 0 | 4.3 GB |
| `exports_cut` | export section windows | 4.0 s | 0 | 1.9 GB |
| `imports_ok` | import section, capability names | 3.1 s | 0.7 s | 2.0 GB |
| `framing_ok`, `start_ok`, `envelope_ok` | whole module as one `Nat` | 0.11 s each | 0 | 1.1 GB |

`plans_ok`, split into one declaration per conjunct:

| Conjunct | Kernel | Peak RSS |
|---|---|---|
| `indicesDistinct` (`decide (List.Nodup ...)`, 844 numbers) | 33.6 s | 9.2 GB |
| `dataConfirmed` | 13.2 s | 5.3 GB |
| `typeTableConfirmed` | 12.8 s | 4.8 GB |
| `roleTypesPinnedL` | 8.6 s | 3.2 GB |
| `firstRecGroup` (the 11.6 KB rec group as one window) | 3.8 s | 2.6 GB |
| `typesInhabited`, `decodeData`, `decodeTypes`, `consPinned`, `eqrefConfined`, `newtypesGrounded` | 0.1 to 0.6 s each | < 1.4 GB |
| sum of the parts | 80 s | |
| the same conjuncts as one declaration | 367 s | 15.1 GB |

The witness, split the same way:

| Pin | Kernel | Peak RSS |
|---|---|---|
| `report_pin_9` (`reportFacets`) | 62.6 s | 12.1 GB |
| `report_pin_10` (policies) | 22.3 s | 5.2 GB |
| `report_pin_11` (termination witnesses) | 22.7 s | 5.2 GB |
| `report_pin_8` (`reportEntries`) | 8.5 s | 2.8 GB |
| every other pin (bridges, laws, `rfl` pins) | 2.8 s + 3.9 s elab | 1.7 GB |

### Where the bytes go

Whole module or whole section, in one declaration: `plans_ok` (type section, rec group, data section), `strings_ok` (type, function and code sections), `exports_ok` and `exports_cut` (export section), `types_cut` (type section), `code_cut` and `layout_ok` (function and code sections), `imports_ok`. Once per function, but many functions per declaration: `closure_ok` (846), `axes_ok` (833), each plan chunk (32). Once per obligation over all functions, which is quadratic: `report_pin_8` to `report_pin_11`.

## 3. What the kernel must see

For each claim the kernel needs a fact about a small, known part of the module, plus arithmetic that shows the parts are where they are said to be and cover everything. It never needs a section as one term.

**(a) Function bodies: packed equality.** A planned function's code entry is the lowering of its plan. The kernel lowers the plan (`codeEntryBytes`, as today), packs the byte list into one numeral, and compares it with `Nat.beq` against the entry's bytes read from the module. It reads only the chunks the entry touches:

```lean
def join (w : Nat) : List Nat → Nat            -- little-endian w-byte chunks
def slice (n off len : Nat) : Nat := (n >>> (8 * off)) % 2 ^ (8 * len)
def window (w : Nat) (cs : List Nat) (off len : Nat) : Nat :=   -- only the covering chunks
  slice (join w ((cs.drop (off / w)).take ((off % w + len + w - 1) / w))) (off % w) len
theorem window_eq (hw : 0 < w) (hfit : chunksFit w cs = true) (off len : Nat)
    (hin : off / w ≤ cs.length) : window w cs off len = slice (join w cs) off len
def codePacked (cs : List Nat) (L : Layout) (M : MCtx) (f : Nat) (p : FnPlan) : Bool := ...
  -- bytesOk bs && bs.length == L.len k && window 1024 cs (L.off k) (L.len k) == pack bs
theorem codeOld_of_packed (hfit : chunksFit 1024 cs = true) (h : codePacked cs L M f p = true) :
    codeOld (join 1024 cs) L M f p = true   -- codeOld: today's `L.entry n k == bs` conjunct
```

All of this is proved in `ScaleBytes.lean` without `sorry`. The checker renders the module as 1 KiB chunks (`chunks : List Nat`), which it already does internally; `chunksFit` is decided once. Each plan is its own declaration.

**(b) Module structure: declared blocks, decoded locally.** Every section is read as blocks of about 64 consecutive entries at producer-declared module offsets. A block reads its bytes once through `window` and decodes each entry on its own sub-window (`ByteWindow.seqWin`, which exists). Global facts come from small declared summaries checked entry by entry: the callee list of each closure function, the shape bitmap of the String-helper candidate types, the classification of each export (obligation or declared-uncertified), and the sorted order of every set whose distinctness matters. The kernel checks each summary locally and then does arithmetic or a linear `strictly` over the summaries. It never sorts.

**(c) Coverage: tiling by arithmetic.** The producer declares every section header offset and every block start. The kernel checks the magic and version, reads the id and size LEB at each declared header, requires each payload to end where the next header starts and the last to end at `modLen`, reads each vector count, and requires the blocks to chain (`tiled`: block `b + 1` starts at block `b`'s start plus the sum of its lengths, the first at the first entry, the last ending at the section's end). Measured: 0.02 s for the framing and at most 0.13 s per section for the tiling.

**(d) Small declarations.** Every piece is one declaration of bounded size: one plan, or one block of about 64 entries. The kernel's caches are freed after each. A module holds about 100 of them, and modules build in parallel.

**(e) Algorithms that fit the kernel.** Distinctness by `strictly` over keys in producer-declared order: the emitter sorts the export section by the wall's name key, which is free because export order has no meaning in wasm. Group tables built once, not filtered per obligation. Facts computed once per plan (roles called, facets, policy) and reused by `axes_ok` and the report pins.

## 4. Soundness

The acceptance statement does not change. `AcceptedArtifact.accepted`, `plansAccepted`, `acceptedWholeModule`, the decoders they are written with, `Schema.Holds`, `GrammarSound.agreement`, `fn_certified_group`, `GrammarTotal.fn_certified_total_of_check` and `AcceptanceSoundness.accept_sound` are untouched. The redesign adds cheap checks and proves that each implies the conjunct it replaces, the same pattern as `DeclaredLayout` (`entries_of_fast`, `closureIsolation_of_layout`), `ByteWindow` (`codeLocs_eq_lazy`) and `SortedKeys` (`exportsAccountedOf_of_fast`) today. A wrong declaration can only make a cheap check fail.

`agreement` and `fn_certified_group` never see bytes: they are about `codeOf M fns`, the lowering, and the interpreter. Bytes enter only through `entryAccepted`, which says the module's code entry equals `codeEntryBytes M plan`. `codeOld_of_packed` gives that equality from the packed check once `layoutConfirmed` holds, and `entryAccepted_of_fast` already turns it into `entryAccepted`. So the restatement over packed slices is one lemma per replaced reading, and none touches the simulation proofs.

Proved in the prototype: `join_append`, `join_lt`, `slice_drop`, `slice_take`, `window_eq`, `takeBytes_pack`, `pack_lt`, `entry_of_packed`, `codeOld_of_packed`.

Still to prove, one per migration step:

1. `layoutConfirmed_of_tiled`: the declared code offsets tile the code section and each entry's size LEB and locals decode on its window, so `codeLocs` is `some` and matches the layout. The existing `seqWin_append`, `seqWin_mod`, `cutWin_eq` and `codeLocs_eq_lazy` carry most of it; the new part is that a block's `window` is the block's slice of the section payload, which follows from `window_eq` and `isolateBytes_eq`.
2. The same for the type section (with the rec group cut into its subtypes), the export section, the function section and the data section.
3. `moduleFramingValid_of_declared` and `modulePayload_of_declared`: the declared headers are the headers `sectionTable` walks.
4. `closureFoldB_of_declared`: if every admitted function's scan equals its declared callee list and the fold over the declared lists yields the admitted set, the fold over the real scans yields the same set. Induction on fuel with the invariant `seen ⊆ admitted`.
5. `exportsAccounted_of_blocks`: per-entry classification plus `strictly` over the keys in section order.
6. `roleTable_of_blocks`: the String-helper classification per block against a declared shape bitmap.
7. `indicesDistinct_of_strictly`, and the per-plan facts behind `ClaimAxes.checked` and the report pins.

## 5. Trust points

No new trust point is needed. The kernel remains the only judge, the implication lemmas are kernel-checked wall theorems, the axioms stay `propext`, `Classical.choice` and `Quot.sound`, and nothing uses `native_decide`, `ofReduceBool` or mathlib.

The checker renders the bytes as chunks instead of as one `|||` numeral. It renders from the bytes it read, as now, and the change is a few lines of `wall.rs`. `chunksFit` is decided in the kernel, and `modLen` stays pinned. Either the checker defines `modBytes := join 1024 chunks`, or one declaration proves `join 1024 chunks = modBytes` (measured: 1.2 s, 1.8 GB). A balanced join would lower that memory.

Two shortcuts would add trust, and this design rejects both: reading section framing or export-name distinctness from `wasmparser` instead of the kernel, and a Rust-computed layout that the kernel does not confirm.

The emitter change (sorted exports) is producer-side. A module whose exports are not sorted declines. It is never accepted wrongly.

## 6. Hostile producers

- **Lying slice.** The declared offset or length of a function is wrong. The packed window is not `pack (codeEntryBytes M plan)`, or the entry's size LEB disagrees with its declared length, or the tiling breaks. Declines.
- **Overlapping or gapped slices.** `tiled` requires each block to start exactly where the previous one ends, and each entry inside a block to start where the previous entry ends (`seqWin`), from the section's first entry to its end. Two entries cannot share a byte and no byte is skipped. The count LEB must equal the number of declared entries.
- **Framing lies.** Each header is read at its declared offset. Its size LEB fixes where the next header must be, and the chain must end at `modLen` exactly, so a header cannot be declared inside a payload or past the end. The payload a block reads is fixed by the header, not by the producer.
- **Reading past the end.** Past the last chunk `window` reads zeros. The framing chain ends at `modLen`, and every block lies inside a payload, so no accepted reading reaches past `modLen`.
- **Lying summaries.** A wrong callee list fails that function's scan equality. A wrong shape bit fails that type's check. A wrong export classification fails that entry's comparison, or the cover check that every obligation and every declared name is used exactly once. A wrong order fails `strictly`.
- **Lying chunks.** Chunks come from the checker, not the package. `chunksFit` stops a chunk wider than 1 KiB from changing the reading.

## 7. Prototype results

All prototypes run in the kept btc build directory against the real wall, `Plans.lean`, `Manifest.lean` and `ArtifactLayout.lean`. Each theorem is its own declaration, `decide +kernel`, with no `sorry`.

| Prototype | Declarations | Kernel | Wall (serial) | Peak RSS |
|---|---|---|---|---|
| (i) code equality only, packed (`codePacked`) | 833 | 46 s | 55 s | 1.56 GB |
| (i) same, today's reading (`L.entry modBytes k == bs`), first 100 plans | 100 | 14.4 s | 15.4 s | 2.4 GB |
| (i) lowering alone (`codeEntryBytes`), first 100 plans | 100 | 2.4 s | 3.4 s | 0.8 GB |
| (i) full per-plan check (typing, call order, packed code, signature) | 833 | 99 s | 112 s | 1.59 GB |
| (ii) type section: 1,140 subtypes + 5,511 entries, blocks of 64 | 105 | 18 s | | 0.67 GB |
| (ii) export section decode + per-entry key, kind, index | 58 | 5 s | | 0.74 GB |
| (ii) function section | 86 | 2 s | | 0.67 GB |
| (ii) closure: 846 per-function scans on windows | 14 | 18 s | | 1.17 GB |
| (ii) export-key distinctness, `strictly` over sorted keys | 1 | 1.4 s | 5.9 s | 0.67 GB |
| (iii) code headers: 5,451 entries (size LEB + locals) | 86 | 9 s | | 0.67 GB |
| (iii) framing (11 headers) and 5 section tilings | 6 | 0.4 s | | 0.67 GB |
| (ii) + (iii) together (15 modules) | 355 | 50 s | 60 to 68 s | 1.17 GB |
| bytes as chunks (`ProtoBytes`: 781 hex numerals, `chunksFit`) | 1 | 0.04 s | 4.5 s | 0.67 GB |

Per plan the full check costs about 120 ms, half of it the lowering and packing. Today it costs about 256 ms (a 32-plan chunk takes 8.2 s). The one outlier is the largest function (4,870 bytes), whose file peaks at 1.56 GB because `pack` is quadratic in the entry length. Packing 32-byte limbs first would remove that.

The byte path, prototyped and estimated:

| Piece | Today | New | How the new number was obtained |
|---|---|---|---|
| per-plan checks (`plans_all`) | 116 s, 3.5 GB | 112 s, 1.6 GB | measured (i) |
| `plans_ok` (rest) | 367 s, 15.1 GB | about 40 s, < 2 GB | measured split conjuncts (80 s), minus `indicesDistinct` by `strictly` (34 s to ~0), `firstRecGroup` by subtype blocks (3.8 s to 2.7 s); `typeTableConfirmed` and `dataConfirmed` still need blocks to drop below 2 GB |
| `types_cut`, `code_cut`, `exports_cut`, `layout_ok` | 50 s, 5.9 GB | 34 s, 0.7 GB | measured (ii) + (iii) blocks |
| `closure_ok` | 23 s, 6.1 GB | 18 s, 1.2 GB | measured; about 1 s if planned functions use their plans' call targets (section 10) |
| `strings_ok` | 58 s, 12.7 GB | about 10 s, < 1 GB | estimate: per-block classification over the decoded function section, with a declared shape bitmap |
| `exports_ok` | 68 s, 10.7 GB | 7 s + names | measured blocks and `strictly`; names: see section 10 |
| `axes_ok` | 17 s, 4.3 GB | ~0 | reuse the per-plan facts |
| `roles_ok`, `imports_ok`, other | 21 s, 4.9 GB | about 15 s, < 2 GB | split per role |
| byte path total | 687 s, 15.1 GB | about 240 s + names, 1.6 GB | |

Every new declaration fits in 1.6 GB, so the byte path can build with 4 to 8 Lean workers. At 4 workers it is about a minute.

Against the target of 1 to 2 GB and a few minutes for the whole check: the byte path meets it. The whole check does not yet, because of the parts in section 9.

## 8. Linear in functions

The new byte path costs about 120 ms per plan, about 3 ms per 64-entry block of any section, one tiling check per section, and one framing check. Nothing depends on the module's byte count except the one-time chunk file (4.5 s, mostly numeral parsing). Doubling the module doubles the number of declarations and leaves each declaration's size and memory unchanged.

## 9. Outside the byte path (measured, not prototyped)

- **Witness report pins: 116 s, 12.1 GB.** `reportFacets`, `policy` and `termination?` over all obligations each call `groupMembers`, which filters all 833 plans once per obligation, so the cost is quadratic. Fix: the package proves the report lists per block of obligations and the witness cites the joined lemma at the checker's statement (the pin's statement stays checker-owned; only its proof term changes). Also build the group table once.
- **Audit: 246 s.** `axiomsOf` runs `collectAxioms` from an empty state for each of about 700 roots and walks the shared closure every time. Fix: one `CollectAxioms` state threaded through all roots, reporting per root from the memo. Expected a few seconds.
- **Source bridges: 392 s build, `BridgeProof` alone 243 s and 9 GB.** Same shape as the byte path: one large module. Split it per bridge, or per group of bridges, before anything else.
- **Source model: 142 s** over 150 modules. Parallel builds help directly.

With the byte path, the witness and the audit fixed and 4 workers, btc should check in about 6 to 7 minutes, and never faster than `BridgeProof`'s 243 s while it is one module. The peak then comes from `BridgeProof` (9 GB) until it is split.

## 10. Risks and open points

- **Names as Strings.** The kernel converts a `String` literal to code points at about 45 ms per name, so 3,763 names cost about 170 s this way. Today's char-list route costs about 70 s (25 s of it elaborating the char literals). A `rfl` pin `"name" = String.ofList (unpackChars len key)` costs 83 ms of kernel time per 64 names but about 1.9 s of elaboration, because the elaborator runs its own `isDefEq` first. What remains open is a kernel-only way to tie the manifest's Strings to packed name keys. Options: a checker-authored pin proved without the elaborator's defeq check, or a schema change that carries export names as byte numerals next to the display Strings. Either one makes names about 5 s in total.
- **Closure over planned functions.** Scanning the bytes of the 823 planned functions repeats work: their bytes are the lowering, and the lowering's calls are `callTargets` of the plan. A wall lemma `scanClosureCodeEntry (codeEntryBytes M p) = some (lowered call targets)` would leave only the 23 helpers to scan. It is a decoder-after-encoder proof over the instruction fragment, so real work, but it is proved once.
- **`typeTableConfirmed` and `dataConfirmed`** index a 1,140-element list per declared type (`grp[idx]?`). They need to move into the subtype blocks: each block confirms the declarations whose struct index falls in it.
- **The lemmas in section 4 are not written yet.** They follow existing patterns, but each is a PR of its own, and a mistake there is a soundness bug, not a speed bug. The `sorry`-free, axiom-audited build stays the guard.
- **Kernel memory is not freed back to the OS inside one process.** A module of 100 small declarations still peaks higher than any one of them (`ProtoCode600`: 1.56 GB with one 4,870-byte function). Keep modules to about 100 declarations and keep the largest single function in mind.
- **Parallel builds multiply memory.** At 1.6 GB per worker, 4 workers need about 7 GB. `AVER_CERT_BUILD_JOBS` should default to the machine's memory divided by the largest module's peak.
- **Producer-emitted offsets for every entry** make the package larger. The layout for btc is already 370 KB. Blocks of 64 keep it at one start offset per block plus the lengths, which are already emitted as cuts.

## 11. Migration plan

Each step is one PR. Each keeps `accepted` unchanged, adds its lemma to the wall (new `wall_id`), and switches the renderer to the new proof. Order by payoff per risk:

1. **Split the monoliths, no new wall logic.** Render `plans_ok` as one declaration per conjunct, and `ArtifactInterface`, `ArtifactStrings` and `Artifact` as one theorem per module where they are independent. Replace `indicesDistinct`'s `decide List.Nodup` with a `strictly (msort ...)` lemma. Expected: peak from 15.1 GB to about 9 GB, `plans_ok` from 367 s to about 45 s.
2. **Audit memo and report pins.** One `CollectAxioms` state in `CheckerAudit.lean`, plus package lemmas for the report lists, proved per block and cited by the witness. Expected: 246 s + 116 s down to about 20 s, witness peak from 12 GB to about 2 GB.
3. **Chunked bytes.** `wall.rs` renders `chunks`; the wall gains `ScaleBytes` (`join`, `window`, `window_eq`, `chunksFit`); `modBytes` is `join 1024 chunks`, or one bridge declaration.
4. **Packed code entries.** `codePacked` and `codeOld_of_packed`; plan modules become one declaration per plan (about 100 per module). `axes_ok` reads the per-plan role bitmaps.
5. **Declared framing and code tiling.** `framingOk`, `tiled`, `layoutConfirmed_of_tiled`. `code_cut` and `layout_ok` become blocks.
6. **Type section blocks,** including the rec group's subtypes. `typeTableConfirmed` moves into the blocks.
7. **Export blocks and sorted exports.** The emitter sorts exports by the wall's name key; `exportsAccounted_of_blocks`; decide the names question from section 10 here.
8. **String-helper classification per block,** with the declared shape bitmap.
9. **Closure from declared callee lists,** then, separately, the planned-function callee lemma.
10. **Split `BridgeProof` per bridge.** The largest remaining module.

After step 2 the peak is set by `strings_ok` (12.7 GB) and `exports_ok` (10.7 GB) until steps 7 and 8. After step 8 every byte-path module is under 2 GB.

## 12. Reproducing

`aver-cert/spike/cert-scale/` on the branch holds the Lean prototypes (`ScaleBytes.lean`, `ScaleSections.lean`, `ProtoFullDefs.lean`) and the generators and scripts (`gen_*.py`, `split*.py`, `measure.sh`, `buildsum.py`, `percat.py`). The verifier on the branch keeps its build directory when `AVER_CERT_KEEP_BUILD_DEVONLY` is set; this is a spike-only change and must not merge. To reproduce: compile and check btc with that variable set, move the kept directory to `build/`, copy the Lean prototypes in, compile `ScaleBytes`, `ScaleSections`, `ProtoBytes`, `ProtoCtx` and `ProtoFullDefs` to `.olean` with `lake env lean -o`, run the generators, then run `measure.sh` on the generated modules.
