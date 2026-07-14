# Certkit Stage A — prelude + differential harness

Stage A builds the certificate *semantics* for the measured Aver wasm-gc
user-code fragment and validates it three ways (Aver VM, real wasm engine, Lean
interpreter). No certificate emitter and no `src/` changes — this is the
semantics layer plus its tooling.

## Deliverables

| Path | What |
|------|------|
| `prelude/CertPrelude.lean` | Semantics: `WVal`, `WInstr` (all 39 opcodes), stratified `wRunF` + fuelled `wFuncN`, `ReprSpec`, executable host-contract faces (`boxRef`, `addRef`, `subRef`, `leRef`…). |
| `prelude/CertPreludeSanity.lean` | Ported simulation theorem `addTwo_wasm_certified` (kernel-clean) + 32 `native_decide` anti-vacuity guards executing every opcode. |
| `prelude/lakefile.lean`, `lean-toolchain` | Standalone lake project, pinned `leanprover/lean4:v4.32.0` (same as `aver proof`). |
| `wat.py` | Minimal `wasm-tools print` reader (types, data, exports, user-fn bodies). |
| `extract.py` | `.wasm` -> Lean `CodeTbl` term + `*.meta.json` metadata. |
| `diff_harness.py` | Three-way differential + coverage matrix. |
| `fixtures/*.av` | `certprobe`, `certprobe2` (copied) + `certkit_zoo`, `certkit_ops` (new). |

## The measured fragment: 39 opcodes, not 34

The probe inventory said "34 distinct opcodes". Re-measuring across the full
`examples/data` + `examples/core` corpus (34 modules) plus the fixtures — with a
body parser that does NOT drop instructions inside `if`/`else` blocks (the first
cut of the parser did, which is likely where the "34" undercount came from) —
the real user-code surface is 39 distinct opcodes. All 39 are modelled in the
prelude and exercised by the harness. Scope is unchanged in spirit (no loops, no
`br`/`br_if`, no linear memory in user code); the extra ones are
arithmetic/comparison variants (`f64.mul`, `f64.ge`, `f64.gt`, `i64.gt_s`,
`i32.gt_s`, `return`, `i64.eqz`, ...) the narrower inventory folded.

## Architecture (kept exactly as the probes measured)

- `wRunF` is structural on the instruction tree; calls into code go through an
  opaque `callee` parameter. Fuel lives only in `wFuncN`, burned solely on
  call-into-code. This is the stratification lesson from probe #2.
- Runtime helpers are not interpreted. They are named host contracts. The
  prelude ships executable reference faces (`boxRef`/`addRef`/`subRef`/`leRef`
  ...) so the harness runs end to end; the certificate theorem keeps them
  abstract (`ReprSpec` + `h7`-style hypotheses).
- Instruction immediates are resolved at extraction time: `structNew` carries
  its field count, `arrayNewData` carries the resolved data-segment bytes. The
  semantics therefore needs only `host`/`ar`/`callee`, matching the probe shape.
- `WVal` carries f64 as its `UInt64` bit pattern, keeping it bit-exact while
  side-stepping the missing `DecidableEq` on `Float`.

## Prelude: kernel-clean

`lake build` is clean. `#print axioms addTwo_wasm_certified`:

```
'CertPrelude.addTwo_wasm_certified' depends on axioms: [propext, Classical.choice, Quot.sound]
```

Whitelist only, no `sorryAx`. The 32 `native_decide` guards are executable
sanity outside the proof budget: each forces `wFuncN` to compute a decoded
result on a concrete input (Int, Bool, f64, ADT, String, list, tail recursion,
and every residual ALU opcode), so no theorem can close vacuously.

## Differential harness: 1440 cases, zero divergences

Seeded (deterministic, seed `20260705`), `CERTKIT_N=60` inputs per user
function. Each input runs through the Aver VM (`aver run`), the real wasm engine
(`aver run --wasm-gc`), and the Lean interpreter (`wFuncN` on Repr-boxed
inputs). Floats are compared bit-exact (Lean emits the raw IEEE-754 bits;
VM/wasm round-trip decimals are re-packed) because Lean's `toString Float` is
lossy — all three engines agree to the bit, including sub-ULP cases where the VM
and wasm formatters print different decimals for the same double.

This finite decimal-generated sample did not cover arbitrary raw-bit NaN
payloads, so it is not evidence for an exact-bit Float arithmetic theorem over
all `UInt64` inputs. The certification gate now declines Float-producing
`f64.add`/`f64.mul` plans in the general WebAssembly profile, and a separate
raw-bit Wasmtime regression exercises payload-bearing NaNs under both the
general and canonicalizing engine profiles.

```
fixtures: 4   cases compared: 1440   divergences: 0

  certkit_ops      testable=12 cases= 720 skipped=0
  certkit_zoo      testable= 9 cases= 540 skipped=0
  certprobe        testable= 1 cases=  60 skipped=0
  certprobe2       testable= 2 cases= 120 skipped=0

  cross-engine opcodes : 33/39
  interp-guard opcodes :  6/39  (validated by CertPreludeSanity native_decide guards)

RESULT: PASS — zero divergences, all 39 opcodes exercised (cross-engine + interpreter guards).
```

Reproduce: `cargo build --bin aver --features wasm` then
`python3 tools/certkit/diff_harness.py` (set `CERTKIT_N` to scale; 60 gives 1440
cases in ~33 s).

## Per-opcode coverage matrix

Two tiers. cross-engine = the opcode sits in a function validated on real inputs
against both the VM and the wasm engine. interp-guard = the opcode is modelled
and executed with a checked result by the Lean interpreter (`native_decide`
guard), but driving it end-to-end through the external engines would require
runtime contracts Stage A deliberately keeps abstract (string interpolation,
`Result`/tag machinery, list builders).

| opcode | tier | | opcode | tier |
|---|---|---|---|---|
| array.new_data | cross-engine | | i64.const | cross-engine |
| array.new_fixed | interp-guard | | i64.eq | cross-engine |
| call | cross-engine | | i64.eqz | interp-guard |
| else | cross-engine | | i64.ge_s | cross-engine |
| end | cross-engine | | i64.gt_s | cross-engine |
| f64.add | cross-engine | | i64.le_s | cross-engine |
| f64.const | cross-engine | | i64.lt_s | cross-engine |
| f64.div | cross-engine | | if | cross-engine |
| f64.eq | cross-engine | | local.get | cross-engine |
| f64.ge | cross-engine | | local.set | cross-engine |
| f64.gt | cross-engine | | ref.cast | cross-engine |
| f64.le | cross-engine | | ref.is_null | cross-engine |
| f64.lt | cross-engine | | ref.null | interp-guard |
| f64.mul | cross-engine | | ref.test | cross-engine |
| f64.sub | cross-engine | | return | interp-guard |
| i32.and | interp-guard | | return_call | cross-engine |
| i32.const | cross-engine | | struct.get | cross-engine |
| i32.eq | interp-guard | | struct.new | cross-engine |
| i32.gt_s | cross-engine | | | |
| i32.le_s | cross-engine | | | |
| i32.lt_s | cross-engine | | | |

The 6 interp-guard opcodes and why they are not cross-engine:

- `array.new_fixed` — arises in user code from string interpolation (collecting
  the string parts). Driving it end-to-end needs the int->string and
  string-concat runtime contracts, out of Stage A scope. Modelled + guarded
  (`cArrFixed` builds `[10,20]` and the decode is checked).
- `i32.eq` — arises from `Result`/`Option` tag dispatch. Needs `Result` boxing.
  Guarded (`cI32Eq`).
- `i32.and` — arises from independent-product (`!`) lowering. Guarded (`cAnd`).
- `i64.eqz` — an emitter zero-test optimisation the fixtures did not trigger
  (`n == 0` variable-vs-literal lowered to `i64.eq`). Guarded (`cI64Eqz`).
- `ref.null` — arises when constructing a null-tailed / empty collection, whose
  value is a list/ref not comparable as a scalar. Guarded (`cRefNull`).
- `return` — the plain (non-tail) early return, emitted by the `?` `Result`
  propagation operator; the function returns a `Result`, not a scalar. Guarded
  (`cRet`).

## Runtime-helper identification

Helpers are never interpreted. `box` is identified by its export name
(`__rt_aint_from_i64`); `Int.add`/`sub`/`mul` and the `Int` comparison helpers
are identified by a normalised body fingerprint — the harness compiles tiny
`op = a OP b` reference modules, records each helper body with every numeric
immediate masked (call/type/local indices shift between modules; the opcode
structure is fixed per release), and matches fixture call-indices against that
table. `Int.mul` has no reference face in Stage A; a function reaching it is
reported as skipped (fail-loud), not silently mis-run. No fixture reaches it.

## Semantics decisions resolved by testing (the harness is the oracle)

- f64 bit-exactness across engines. Confirmed: VM, wasm, and Lean produce
  identical IEEE-754 bits for `+ - * /` and all comparisons over 1440 cases,
  including cases where VM and wasm print different decimals for the same double
  (e.g. `-2.0000000000000013` vs `-2.0000000000000012`). Comparison is therefore
  done on bits, not decimals.
- `i32.and` on the boolean domain. Modelled as logical AND on `{0,1}` (the only
  operands the emitter produces). Marked `ponytail:` in the source with the
  upgrade path (32-bit two's-complement) if a non-boolean operand ever appears;
  the differential is the tripwire.
- `ref.cast` on a type mismatch traps (returns `none`); in emitted code a cast
  is always guarded by a preceding `ref.test`, so it is identity in practice.

## Walls / deviations (banked honestly)

1. Big-integer inputs are not cross-engine tested. Generated `Int` inputs stay
   in the i64 small-carrier range. Feeding a limb-backed big integer would
   require reproducing the runtime's bignum limb encoding on the Lean side —
   exactly the runtime-contract territory Stage A keeps behind
   `addRef`/`subRef`/`carrierToInt`. Consequence: the big-carrier guard branch
   (`ref.is_null`->false -> `struct.get ... 2` (sign) -> `i32.lt_s`) is exercised
   by the Lean anti-vacuity guards (which construct big carriers directly) but
   not by the external engines with small inputs. `i32.lt_s` still lands
   cross-engine via the chained-comparison path in `cmp3`.
2. `Int.mul` reference face not written. No fixture needs it; adding it is a
   4-line contract when one does.
3. ASCII-only string decode. `describe`'s string results are decoded byte->char
   on the Lean side assuming ASCII (true for the fixtures). Multibyte UTF-8
   would need a real decoder.
4. `return` / `i32.eq` / `i32.and` / `i64.eqz` / `ref.null` / `array.new_fixed`
   are interpreter-guarded, not cross-engine — see the table above. Each is
   entangled with a runtime contract (strings, `Result`, list builders) or is an
   emitter optimisation the scalar fixtures do not hit. Their opcode semantics is
   validated by execution; only the end-to-end engine agreement is deferred.
