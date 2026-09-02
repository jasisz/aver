/-!
# Verbatim fragment of the wall's `CertPrelude`

Audit copy for the Talos bridge. Everything below the header is copied
VERBATIM from `aver-cert/assets/wall/current/CertPrelude.lean`, lines 78–395
(the value type `WVal`, the measured instruction tree `WInstr`, `WCode`,
`CodeTbl`/`HostTbl`/`Callee`, `Out`, `popArgs`, `initLocals`, `f`, `b32`, the
structural interpreter `wRunF` and the fuelled call semantics `wFuncN`).
Wall id: `aver-cert/src/format.rs CURRENT_WALL_ID` =
`sha256:4331f2e67c965b3c6aca0121a1d46406f3a8a084fe80deecc8d8745df56f9e3f`.

Nothing is trimmed: the copy carries every `WInstr` constructor and every
`wRunF` arm, so the profile restriction lives entirely in `Translate.lean`
(`translate` returns `none` outside the profile). The LEB128 encoders
(lines 28–76) and the reference faces after `wFuncN` (lines 397–552) are not
copied: neither is read by the bridge.

Check the copy with:
`diff <(sed -n 78,395p aver-cert/assets/wall/current/CertPrelude.lean) <(sed -n '/^namespace CertPrelude$/,/^end CertPrelude$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1d;$d')`
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
end CertPrelude

/-!
## Verbatim fragment of the wall's `SchemaCore` (declared-envelope shapes)

Copied VERBATIM from `aver-cert/assets/wall/current/SchemaCore.lean` (same wall
id as above): `HostRole` (lines 487–515), `TypeDecl` (lines 1079–1091) and the
stage-1 record admission `typeDeclIsScalarLeaf`/`checkRecordDecl` (lines
1262–1278). These are the claim-side data `Env.envOfClaim` projects from; the
hand-written `DecidableEq` block and the lowering to `CertDecode.TypeEntry`
are not copied (the bridge never decodes bytes).

Check with:
`diff <(sed -n '487,515p;1079,1091p;1262,1278p' aver-cert/assets/wall/current/SchemaCore.lean) <(sed -n '/^namespace AverCert.Schema$/,/^end AverCert.Schema$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1d;$d')`
-/

namespace AverCert.Schema
/-- Runtime host helper roles admitted by `expr-fragment-v1`. Each role fixes a
    representation-level type signature (checked by `PlanCheck`); the resolved
    wasm function index is carried on the node and bound both to the module
    bytes and to the decoded role table by artifact acceptance. -/
inductive HostRole where
  | box
  | add
  | mul
  | sub
  /-- `__aint_to_index`: extract a wasm array index from a represented integer
      (`[0, 2^31)` passes through; anything else, including every big value,
      collapses to the `-1` out-of-bounds sentinel). Consumed only by the
      monolithic fused vector-read node, never as a standalone `hostCall`. -/
  | toIndex
  /-- `__aint_cmp`: three-way comparison of two Int carriers, yielding the raw
      `i32` sentinel `-1`/`0`/`1` (`CertPrelude.cmpW`). The emitter never reads
      it as a Boolean: it always follows the call with `i32.const 0` and a
      signed relational operator, so the node's result type is `rawI32`, not
      `boolI32`. The ASSUMED CONTRACT covers a CANONICAL CARRIER PAIR only —
      see the note on `Obligation.holds`. -/
  | cmp
  /-- `__aint_eq`: equality of two Int carriers, yielding the `0`/`1` wasm
      Boolean directly (`CertPrelude.eqW`). Unlike `cmp` its result IS the
      source-level Boolean, so the node's result type is `boolI32` and the
      emitter appends no comparison tail. Its assumed contract is over a
      canonical carrier pair for the same reason `cmp`'s is, and for one more:
      `__aint_eq` decides a `Small`/`Big` pair structurally. -/
  | eq
deriving Repr, DecidableEq
/-- A user type declaration carried in the certified Plan. `intCarrier`,
    `boolScalar` and `floatScalar` are the admitted scalar leaves; `record`
    carries its wasm struct type index and its source-order fields; `variant`
    is present but unreachable in stage 1 (its representation arm is `False`).
    `DecidableEq` is written by hand: the derived handler does not fire through
    the nested `List TypeDecl` occurrences. -/
inductive TypeDecl where
  | intCarrier
  | boolScalar
  | floatScalar
  | record (idx : Nat) (fields : List TypeDecl)
  | variant (idx root : Nat) (ctors : List TypeDecl)
deriving Repr
def typeDeclIsScalarLeaf : TypeDecl → Bool
  | .intCarrier => true
  | .boolScalar => true
  | .floatScalar => true
  | .record _ _ => false
  | .variant _ _ _ => false

/-- Stage-1 record admission: a `record` head whose every field is a scalar
    leaf, with at least one field. Explicit arms over every constructor
    (fail-closed, no wildcard), so extending `TypeDecl` forces a decision
    here before any new shape can reach the record-parameter face. -/
def checkRecordDecl : TypeDecl → Bool
  | .record _ fields => !fields.isEmpty && fields.all typeDeclIsScalarLeaf
  | .intCarrier => false
  | .boolScalar => false
  | .floatScalar => false
  | .variant _ _ _ => false
end AverCert.Schema
