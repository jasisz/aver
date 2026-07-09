/-
  CertPrelude — certificate semantics for the measured Aver wasm-gc user-code
  fragment (39 distinct opcodes, no loops, no linear memory).

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
  | i64Eqz | i64Eq | i64LeS | i64LtS | i64GeS | i64GtS
  | i32Eq | i32And | i32LtS | i32LeS | i32GtS
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

/-- `String.eq` executable reference face: compare represented string byte arrays
    and return the i32 boolean the wasm helper produces. Certificate theorems use
    the abstract contract; this reference exists for interpreter tripwires. -/
def stringEqRef : List WVal → Option WVal
  | [a, b] => some (b32 (stringEqW a b))
  | _ => none

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

/-- `String.concat` executable reference face: takes a container `arr` of
    string-arrays as a single argument and returns the byte-concatenated array.
    Certificate theorems use the abstract contract; this reference exists for
    interpreter tripwires. -/
def stringConcatRef (resultTy : Nat) : List WVal → Option WVal
  | [parts] => stringConcatW resultTy parts
  | _ => none

/-- Int comparison contract faces: decode both carriers, compare in ℤ, return
    the i32 boolean the wasm helper produces (`<= < >= > ==`). -/
def cmpRef (p : Int → Int → Bool) : List WVal → Option WVal
  | [a, b] => do let x ← carrierToInt a; let y ← carrierToInt b; some (.i32v (if p x y then 1 else 0))
  | _ => none

def leRef : List WVal → Option WVal := cmpRef (fun x y => decide (x ≤ y))
def ltRef : List WVal → Option WVal := cmpRef (fun x y => decide (x < y))
def geRef : List WVal → Option WVal := cmpRef (fun x y => decide (x ≥ y))
def gtRef : List WVal → Option WVal := cmpRef (fun x y => decide (x > y))
def eqRef : List WVal → Option WVal := cmpRef (fun x y => decide (x = y))

end CertPrelude
