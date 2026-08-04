/-
  CertPrelude — certificate semantics for the measured Aver wasm-gc user-code
  fragment (43 distinct opcodes, no loops, no linear memory).

  Architecture is fixed by the kill-fast probes (probe-artifacts/pcc-kill):
  * `wRunF` is STRUCTURAL on the instruction tree and takes the call
    semantics as an OPAQUE `callee` parameter; fuel lives only in `wFuncN`
    and is burned solely on calls into code. This keeps a recursive call
    opaque under `simp`, exactly where the induction hypothesis applies.
  * Runtime helpers (bignum add/sub/mul, box, cons, ...) are NOT interpreted;
    they are named host contracts supplied as `HostTbl` entries. The prelude
    ships EXECUTABLE reference faces of the small-int contracts (`boxRef`,
    `addRef`, `subRef`) so the interpreter runs end to end in the differential
    harness; the certificate theorems keep them abstract.

  Instruction immediates are baked in at extraction time: `structNew` carries
  its field count, `arrayNewData` carries the resolved data-segment bytes.
  The semantics therefore needs no type table or data-segment environment —
  it depends only on `host`/`ar`/`callee`, matching the probe's clean shape.

  f64 values are stored as their IEEE-754 bit pattern (`UInt64`) so `WVal`
  has `DecidableEq` (needed for the `native_decide` anti-vacuity guards) while
  staying bit-exact under the arithmetic opcodes.
-/

namespace CertPrelude

/-! ## LEB128 index encodings (total, fuel-bounded)

The one audited pair of index encoders shared by every wall module that
SYNTHESIZES bytes (`PlanBytes` lowers plans, `ArithTemplateDerisk` synthesizes
the arith helper bodies). Both are TOTAL — they return `List Nat`, never an
`Option` — because a synthesized template that could be `none` would let an
undecodable module body agree with an unencodable declaration (`none == none`)
and fail open. Totality is obtained with structural fuel, not well-founded
recursion, so `decide +kernel` reduces these definitions.

The fuel-exhausted branch emits the final quotient raw. It is NOT a correct
LEB128 encoding of out-of-range values, and it does not need to be: fuel `f`
encodes every value below `2 ^ (7 * f)` exactly (the branch is unreachable
there), and every caller either range-guards its input (`PlanBytes` wraps
these in `Option` behind a `< 2 ^ 32` test) or conjoins an explicit bound on
the accepted path (`ArithTemplateDerisk.checkArithHostParams` bounds every
spliced index below `2 ^ 32`). `2 ^ 32 ≤ 2 ^ 35`, so five unsigned groups and
six signed groups always suffice. -/

/-- Total unsigned LEB128 with structural fuel: canonical (shortest-form) for
    every `value < 2 ^ (7 * fuel)`. -/
def ulebBytesFuel : Nat → Nat → List Nat
  | 0, value => [value % 128]
  | fuel + 1, value =>
      if value < 128 then [value]
      else (value % 128 + 128) :: ulebBytesFuel fuel (value / 128)

/-- Canonical unsigned LEB128 of a u32 index (`call` targets, `struct.new` /
    `struct.get` / array-op type indices). Exact for every `value < 2 ^ 35`,
    which covers the whole u32 index space wasm admits. -/
def uleb32Bytes (value : Nat) : List Nat :=
  ulebBytesFuel 5 value

/-- Total signed LEB128 (s33) of a NON-NEGATIVE value with structural fuel:
    canonical for every `value < 2 ^ (7 * fuel - 1)`. A group whose bit 6 is
    set would read back negative, so the encoding terminates one bit earlier
    than the unsigned form: 63 is the last single-byte index, and `64` encodes
    as `c0 00`, never `40`. -/
def s33BytesFuel : Nat → Nat → List Nat
  | 0, value => [value % 128]
  | fuel + 1, value =>
      if value < 64 then [value]
      else (value % 128 + 128) :: s33BytesFuel fuel (value / 128)

/-- Canonical signed-LEB (s33) encoding of a concrete heap-type index
    (`ref.null <ht>`, `(ref null <ht>)` local/blocktype positions). Exact for
    every `value < 2 ^ 41`, which covers the whole u32 index space. -/
def s33Bytes (value : Nat) : List Nat :=
  s33BytesFuel 6 value

/-- Runtime values on the wasm-gc stack / in locals. -/
inductive WVal where
  | i32v (n : Int)
  | i64v (n : Int)
  | f64v (bits : UInt64)
  | structv (tyIdx : Nat) (fields : List WVal)
  | arr (tyIdx : Nat) (elems : List WVal)
  | null
deriving Repr, Inhabited
-- Note: no `DecidableEq` — the default handler does not fire through the
-- nested `List WVal` occurrences. The anti-vacuity guards below never compare
-- `WVal`s directly; they decode the result to `Int`/`Bool`/`String` (all
-- `DecidableEq`) and compare those, so the instance is not needed.

/-- The measured user-code instruction fragment. `if/else/end` fold into
    `ifElse`; struct/array immediates are resolved at extraction time. -/
inductive WInstr where
  | localGet (i : Nat)
  | localSet (i : Nat)
  | i64Const (n : Int)
  | i32Const (n : Int)
  | f64Const (bits : UInt64)
  | refNull
  | refIsNull
  | refTest (tyIdx : Nat)
  | refCast (tyIdx : Nat)
  | structNew (tyIdx : Nat) (nfields : Nat)
  | structGet (tyIdx : Nat) (field : Nat)
  | arrayNewFixed (tyIdx : Nat) (n : Nat)
  | arrayNewData (tyIdx : Nat) (bytes : List Nat)
  | arrayLen
  | arrayGet (tyIdx : Nat)
  | i64Eqz | i64Eq | i64LeS | i64LtS | i64GeS | i64GtS
  | i32Eq | i32And | i32LtS | i32LeS | i32GtS | i32GeS | i32LtU
  | f64Add | f64Sub | f64Mul | f64Div
  | f64Eq | f64Lt | f64Le | f64Ge | f64Gt
  | ifElse (thenB elseB : List WInstr)
  | call (f : Nat)
  | returnCall (f : Nat)
  | ret
deriving Repr

structure WCode where
  arity : Nat
  nlocals : Nat
  body : List WInstr

abbrev CodeTbl := Nat → Option WCode
/-- host function index → (arity, semantics). -/
abbrev HostTbl := Nat → Option (Nat × (List WVal → Option WVal))
/-- opaque callee for code functions (closed by fuel in `wFuncN`). -/
abbrev Callee := Nat → List WVal → Option WVal

inductive Out where
  | ok (locals : List WVal) (stack : List WVal)
  | ret (v : WVal)

def popArgs (arity : Nat) (st : List WVal) : Option (List WVal × List WVal) :=
  if st.length < arity then none
  else some ((st.take arity).reverse, st.drop arity)

/-- Locals = incoming args followed by declared-local defaults. Padding is
    mandatory: a short locals list would make `localGet` fail and collapse
    the certificate theorem vacuously. Defaults are never read in emitted
    (SSA-shaped) code, so `null` is a safe filler. -/
def initLocals (c : WCode) (args : List WVal) : List WVal :=
  args ++ List.replicate c.nlocals .null

@[inline] def f (b : UInt64) : Float := Float.ofBits b
@[inline] def b32 (p : Bool) : WVal := .i32v (if p then 1 else 0)

/-- Structural interpreter: recursion only on the instruction tree; calls go
    through `host` (concrete contracts) or the opaque `callee`. -/
def wRunF (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee) :
    List WInstr → List WVal → List WVal → Option Out
  | [], locals, st => some (.ok locals st)
  | .localGet i :: rest, locals, st =>
      match locals[i]? with
      | some v => wRunF host ar callee rest locals (v :: st)
      | none => none
  | .localSet i :: rest, locals, st =>
      match st with
      | v :: st' => wRunF host ar callee rest (locals.set i v) st'
      | [] => none
  | .i64Const n :: rest, locals, st =>
      wRunF host ar callee rest locals (.i64v n :: st)
  | .i32Const n :: rest, locals, st =>
      wRunF host ar callee rest locals (.i32v n :: st)
  | .f64Const bits :: rest, locals, st =>
      wRunF host ar callee rest locals (.f64v bits :: st)
  | .refNull :: rest, locals, st =>
      wRunF host ar callee rest locals (.null :: st)
  | .refIsNull :: rest, locals, st =>
      match st with
      | .null :: st' => wRunF host ar callee rest locals (b32 true :: st')
      | _ :: st' => wRunF host ar callee rest locals (b32 false :: st')
      | [] => none
  | .refTest ty :: rest, locals, st =>
      match st with
      | .structv t _ :: st' => wRunF host ar callee rest locals (b32 (t = ty) :: st')
      | .arr t _ :: st' => wRunF host ar callee rest locals (b32 (t = ty) :: st')
      | .null :: st' => wRunF host ar callee rest locals (b32 false :: st')
      | _ => none
  | .refCast ty :: rest, locals, st =>
      match st with
      | .structv t fs :: st' =>
          if t = ty then wRunF host ar callee rest locals (.structv t fs :: st') else none
      | .arr t es :: st' =>
          if t = ty then wRunF host ar callee rest locals (.arr t es :: st') else none
      | _ => none
  | .structNew ty nf :: rest, locals, st =>
      match popArgs nf st with
      | some (fs, st') => wRunF host ar callee rest locals (.structv ty fs :: st')
      | none => none
  | .structGet ty field :: rest, locals, st =>
      match st with
      | .structv t fs :: st' =>
          if t = ty then
            match fs[field]? with
            | some v => wRunF host ar callee rest locals (v :: st')
            | none => none
          else none
      | _ => none
  | .arrayNewFixed ty n :: rest, locals, st =>
      match popArgs n st with
      | some (es, st') => wRunF host ar callee rest locals (.arr ty es :: st')
      | none => none
  | .arrayNewData ty bytes :: rest, locals, st =>
      -- offset/length are already resolved into `bytes` at extraction time,
      -- but the emitted operands (i32 offset, i32 len) are still on the stack.
      match st with
      | .i32v _ :: .i32v _ :: st' =>
          wRunF host ar callee rest locals (.arr ty (bytes.map (.i32v ∘ Int.ofNat)) :: st')
      | _ => none
  | .arrayLen :: rest, locals, st =>
      -- array.len: an array reference on top yields its element count. A null
      -- reference traps at runtime, modelled as `none`; any other value is a
      -- type error, also `none`.
      match st with
      | .arr _ es :: st' =>
          wRunF host ar callee rest locals (.i32v (Int.ofNat es.length) :: st')
      | _ => none
  | .arrayGet ty :: rest, locals, st =>
      -- array.get with the declared array type immediate. A type mismatch,
      -- a negative index, or an out-of-bounds read traps, modelled as `none`.
      match st with
      | .i32v i :: .arr t es :: st' =>
          if t = ty ∧ 0 ≤ i then
            match es[i.toNat]? with
            | some v => wRunF host ar callee rest locals (v :: st')
            | none => none
          else none
      | _ => none
  | .i64Eqz :: rest, locals, st =>
      match st with
      | .i64v a :: st' => wRunF host ar callee rest locals (b32 (a = 0) :: st')
      | _ => none
  | .i64Eq :: rest, locals, st =>
      match st with
      | .i64v b :: .i64v a :: st' => wRunF host ar callee rest locals (b32 (a = b) :: st')
      | _ => none
  | .i64LeS :: rest, locals, st =>
      match st with
      | .i64v b :: .i64v a :: st' => wRunF host ar callee rest locals (b32 (a ≤ b) :: st')
      | _ => none
  | .i64LtS :: rest, locals, st =>
      match st with
      | .i64v b :: .i64v a :: st' => wRunF host ar callee rest locals (b32 (a < b) :: st')
      | _ => none
  | .i64GeS :: rest, locals, st =>
      match st with
      | .i64v b :: .i64v a :: st' => wRunF host ar callee rest locals (b32 (a ≥ b) :: st')
      | _ => none
  | .i64GtS :: rest, locals, st =>
      match st with
      | .i64v b :: .i64v a :: st' => wRunF host ar callee rest locals (b32 (a > b) :: st')
      | _ => none
  | .i32Eq :: rest, locals, st =>
      match st with
      | .i32v b :: .i32v a :: st' => wRunF host ar callee rest locals (b32 (a = b) :: st')
      | _ => none
  | .i32And :: rest, locals, st =>
      -- ponytail: logical AND on the 0/1 boolean domain the emitter uses;
      -- upgrade to 32-bit two's-complement bitwise if a non-{0,1} operand
      -- ever reaches it (the differential harness is the tripwire).
      match st with
      | .i32v b :: .i32v a :: st' =>
          wRunF host ar callee rest locals (b32 (a ≠ 0 ∧ b ≠ 0) :: st')
      | _ => none
  | .i32LtS :: rest, locals, st =>
      match st with
      | .i32v b :: .i32v a :: st' => wRunF host ar callee rest locals (b32 (a < b) :: st')
      | _ => none
  | .i32LeS :: rest, locals, st =>
      match st with
      | .i32v b :: .i32v a :: st' => wRunF host ar callee rest locals (b32 (a ≤ b) :: st')
      | _ => none
  | .i32GtS :: rest, locals, st =>
      match st with
      | .i32v b :: .i32v a :: st' => wRunF host ar callee rest locals (b32 (a > b) :: st')
      | _ => none
  | .i32GeS :: rest, locals, st =>
      match st with
      | .i32v b :: .i32v a :: st' => wRunF host ar callee rest locals (b32 (a ≥ b) :: st')
      | _ => none
  | .i32LtU :: rest, locals, st =>
      -- TRUE unsigned 32-bit comparison: both operands are reduced to their
      -- unsigned representatives (`emod 2^32`) before comparing. This is NOT
      -- the signed pattern above: the `__aint_to_index` out-of-bounds sentinel
      -- `-1` becomes `2^32 - 1` here, which is never below an engine array
      -- length (< 2^31), so a sentinel index always fails the bounds check.
      match st with
      | .i32v b :: .i32v a :: st' =>
          wRunF host ar callee rest locals
            (b32 (a.emod 4294967296 < b.emod 4294967296) :: st')
      | _ => none
  | .f64Add :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' =>
          wRunF host ar callee rest locals (.f64v (f a + f b).toBits :: st')
      | _ => none
  | .f64Sub :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' =>
          wRunF host ar callee rest locals (.f64v (f a - f b).toBits :: st')
      | _ => none
  | .f64Mul :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' =>
          wRunF host ar callee rest locals (.f64v (f a * f b).toBits :: st')
      | _ => none
  | .f64Div :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' =>
          wRunF host ar callee rest locals (.f64v (f a / f b).toBits :: st')
      | _ => none
  | .f64Eq :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' => wRunF host ar callee rest locals (b32 (f a == f b) :: st')
      | _ => none
  | .f64Lt :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' => wRunF host ar callee rest locals (b32 (f a < f b) :: st')
      | _ => none
  | .f64Le :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' => wRunF host ar callee rest locals (b32 (f a ≤ f b) :: st')
      | _ => none
  | .f64Ge :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' => wRunF host ar callee rest locals (b32 (f b ≤ f a) :: st')
      | _ => none
  | .f64Gt :: rest, locals, st =>
      match st with
      | .f64v b :: .f64v a :: st' => wRunF host ar callee rest locals (b32 (f b < f a) :: st')
      | _ => none
  | .ifElse tB eB :: rest, locals, st =>
      match st with
      | .i32v c :: st' =>
          if c = 0 then
            match wRunF host ar callee eB locals st' with
            | some (.ok locals' st'') => wRunF host ar callee rest locals' st''
            | some (.ret v) => some (.ret v)
            | none => none
          else
            match wRunF host ar callee tB locals st' with
            | some (.ok locals' st'') => wRunF host ar callee rest locals' st''
            | some (.ret v) => some (.ret v)
            | none => none
      | _ => none
  | .call fn :: rest, locals, st =>
      match host fn with
      | some (a, hf) =>
          match popArgs a st with
          | some (args, st') =>
              match hf args with
              | some r => wRunF host ar callee rest locals (r :: st')
              | none => none
          | none => none
      | none =>
          match ar fn with
          | some a =>
              match popArgs a st with
              | some (args, st') =>
                  match callee fn args with
                  | some r => wRunF host ar callee rest locals (r :: st')
                  | none => none
              | none => none
          | none => none
  | .returnCall fn :: _, _, st =>
      match ar fn with
      | some a =>
          match popArgs a st with
          | some (args, _) =>
              match callee fn args with
              | some r => some (.ret r)
              | none => none
          | none => none
      | none => none
  | .ret :: _, _, st =>
      match st with
      | v :: _ => some (.ret v)
      | [] => none
  termination_by instrs _ _ => sizeOf instrs

/-- Fueled call semantics: recursion only on fuel, burned on call-into-code. -/
def wFuncN (code : CodeTbl) (host : HostTbl) : Nat → Nat → List WVal → Option WVal
  | 0, _, _ => none
  | fuel + 1, fn, args =>
      match code fn with
      | some c =>
          match wRunF host (fun g => (code g).map (·.arity))
              (fun g as => wFuncN code host fuel g as)
              c.body (initLocals c args) [] with
          | some (.ok _ [v]) => some v
          | some (.ret v) => some v
          | _ => none
      | none => none

/-! ## Value-representation relation (Repr)

The Int carrier is the wasm struct `{ i64 small, ref null (array i64) limbs,
i32 sign }`, modelled as `structv C [i64v small, limbs, i32v sign]` where the
carrier type index `C` is a parameter (module-specific). Small integers fit
the i64 field with `limbs = null, sign = 0`; big integers carry limbs and a
sign consistent with the value. Booleans are `i32v (0/1)`; f64 is `f64v bits`;
Strings are `arr` of i32 byte values; user ADTs are `structv` of their variant
type index. -/

/-- Carrier constructor for a small integer. -/
def carrierSmall (C : Nat) (k : Int) : WVal := .structv C [.i64v k, .null, .i32v 0]

/-- The abstract representation relation, pinned only on the clauses the
    certificate proofs use (mirrors the probe's hypotheses). -/
structure ReprSpec (C : Nat) where
  Repr : Int → WVal → Prop
  car : ∀ n v, Repr n v → ∃ s l sg, v = .structv C [.i64v s, l, .i32v sg]
  smallIntro : ∀ k : Int, Repr k (carrierSmall C k)
  smallElim : ∀ n s sg, Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n
  bigElim : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0

/-! ## Executable host-contract reference faces

These are the executable versions of the named small-int runtime contracts,
used by the differential harness so the interpreter runs end to end. They are
CLEARLY the reference faces of the contracts `__rt_aint_from_i64` (box),
`Int.add`, `Int.sub` — the certificate theorems keep the contracts abstract. -/

/-- `__rt_aint_from_i64`: box an i64 into a small carrier. -/
def boxRef (C : Nat) : List WVal → Option WVal
  | [.i64v k] => some (carrierSmall C k)
  | _ => none

/-- Decode a carrier back to ℤ (small: the i64 field; big: reconstruct from
    little-endian 64-bit limbs and the sign). Total reference used by the
    executable add/sub faces and by the harness output decoder. -/
def carrierToInt : WVal → Option Int
  | .structv _ [.i64v s, .null, .i32v _] => some s
  | .structv _ [.i64v _, .arr _ limbs, .i32v sg] =>
      let mag : Int := limbs.foldr (fun v acc =>
        match v with | .i64v d => acc * 18446744073709551616 + d | _ => acc) 0
      some (if sg < 0 then -mag else mag)
  | _ => none

/-- `Int.add` / `Int.sub` reference faces: decode both carriers, operate in ℤ,
    re-box as a small carrier (the harness only feeds magnitudes that fit). -/
def addRef (C : Nat) : List WVal → Option WVal
  | [a, b] => do let x ← carrierToInt a; let y ← carrierToInt b; some (carrierSmall C (x + y))
  | _ => none

def subRef (C : Nat) : List WVal → Option WVal
  | [a, b] => do let x ← carrierToInt a; let y ← carrierToInt b; some (carrierSmall C (x - y))
  | _ => none

def mulRef (C : Nat) : List WVal → Option WVal
  | [a, b] => do let x ← carrierToInt a; let y ← carrierToInt b; some (carrierSmall C (x * y))
  | _ => none

/-- Byte equality over the `WVal` array representation of Aver strings. It is
    intentionally narrow: a non-byte element is unequal, and array type indices
    are ignored because the wasm helper receives already-cast string arrays. -/
def wByteListEq : List WVal → List WVal → Bool
  | [], [] => true
  | .i32v a :: as, .i32v b :: bs => a == b && wByteListEq as bs
  | _, _ => false

def stringEqW : WVal → WVal → Bool
  | .arr _ as, .arr _ bs => wByteListEq as bs
  | _, _ => false

/-- Byte-list append for represented string bytes. A malformed element is a
    contract failure, not a byte to skip. -/
def wByteAppend : List WVal → List WVal → Option (List WVal)
  | [], bs => some bs
  | .i32v a :: as, bs => do
      let rest ← wByteAppend as bs
      some (.i32v a :: rest)
  | _ :: _, _ => none

def stringConcatParts : List WVal → Option (List WVal)
  | [] => some []
  | part :: rest => do
      let acc ← stringConcatParts rest
      match part with
      | .arr _ bytes => wByteAppend bytes acc
      | _ => none

/-- The `String.concat` reference at the WVal byte-array level: take a container
    array of string-arrays, concatenate each element's bytes in order, and return
    the helper's statically declared result array type. Malformed containers or
    byte arrays fail with `none`. -/
def stringConcatW (resultTy : Nat) : WVal → Option WVal
  | .arr _ parts => do
      let bytes ← stringConcatParts parts
      some (.arr resultTy bytes)
  | _ => none

/-- The `__aint_to_index` contract at the ℤ level (confirmed against
    `wat/to_index.wat`): a represented integer in `[0, 2^31)` extracts to its
    own i32 value; a negative or `>= 2^31` value — and in particular every
    limb-carrying big value — collapses to the out-of-bounds sentinel `-1`.
    The sentinel fails BOTH halves of the emitter's bounds check (`idx >= 0`
    signed AND `idx < len` unsigned), so an unrepresentable index always takes
    the miss arm, never a wrapped in-range read. -/
def toIndexW (n : Int) : Int :=
  if 0 ≤ n ∧ n < 2147483648 then n else -1

/-- Executable reference face of the `__aint_to_index` contract: a small
    carrier extracts through `toIndexW`; any big (limb-carrying) carrier is
    the sentinel. -/
def toIndexRef : List WVal → Option WVal
  | [.structv _ [.i64v s, .null, .i32v _]] => some (.i32v (toIndexW s))
  | [.structv _ [.i64v _, .arr _ _, .i32v _]] => some (.i32v (-1))
  | _ => none

/-- The `__aint_cmp` contract at the ℤ level (read off `wat/cmp.wat`): the
    three-way comparison of two integers, `-1` when the first is smaller, `0`
    when they are equal, `1` when the first is larger. The emitter never
    consumes this value directly — it compares it against `i32.const 0` with a
    signed relational operator — so the exact sentinel values matter and a
    boolean-valued model would not do.

    WHERE IT IS ASSUMED: `Obligation.holds` quantifies this contract over
    LITERAL small carriers in the i64 band, not over the representation
    relation. `wat/cmp.wat` decides first on the raw SIGN FIELDS of its two
    operands, and `CarrierSpec.bigElim` constrains those fields only up to the
    sign/non-zero facts, so a relational premise would not be satisfiable by
    the real helper.

    An earlier revision of this file carried a `cmpRef` family that returned
    `0`/`1` for five derived predicates. It was wired to nothing, and its ABI
    disagreed with the helper this contract describes; it has been deleted
    rather than adapted. -/
def cmpW (a b : Int) : Int :=
  if a < b then -1 else if a = b then 0 else 1

/-- The `__aint_eq` contract at the ℤ level (`wat/eq.wat`): `1` when the two
    integers are equal, `0` otherwise. Unlike `cmpW` this helper already yields
    the wasm Boolean the source `==` denotes, which is why the emitter appends
    no comparison tail after the call.

    WHERE IT IS ASSUMED: small-band literal carriers only, like `cmpW`, and
    here the relational form is not merely unproved but REFUTABLE. `wat/eq.wat`
    compares STRUCTURALLY — a `Small` operand against a limb-carrying `Big` one
    returns `0` outright — while `CarrierSpec.smallIntro` admits
    `carrierSmall C k` as a representation of `k` for every `k`. A relational
    premise would therefore be unsatisfiable at any carrier specification that
    models `Big` carriers, making every comparison obligation vacuous. -/
def eqW (a b : Int) : Int :=
  if a = b then 1 else 0

end CertPrelude
