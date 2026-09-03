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
`diff <(sed -n 78,395p aver-cert/assets/wall/current/CertPrelude.lean) <(sed -n '/^namespace CertPrelude$/,/^end CertPrelude$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1d;$d' | head -n 318)`
(the file has a second, smaller `CertPrelude` block further down — `-- BEGIN CertPrelude fragment 2` — with its own check)
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

/-!
## Verbatim fragments used by the `envOfClaim` composition lemma

`CertDecode` type-section shapes (CertDecode.lean lines 160–197), the wall's
lowering of a certified record declaration to its type-section entry
(SchemaCore.lean lines 1132–1152, the entry `StandardFace` pins by equality
through `WasmSlice.typeSectionMatches`), and the host-role lookup the plan
encoder resolves roles with (PlanCheck.lean lines 869–876). Same wall id.

Check with:
`diff <(sed -n '160,197p' aver-cert/assets/wall/current/CertDecode.lean) <(sed -n '/^namespace CertDecode$/,/^end CertDecode$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1d;$d')`
`diff <(sed -n '1132,1152p' aver-cert/assets/wall/current/SchemaCore.lean) <(sed -n '/^namespace AverCert.Schema.Lowering$/,/^end AverCert.Schema.Lowering$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1d;$d;/^open AverCert.Schema$/d')`
`diff <(sed -n '869,876p' aver-cert/assets/wall/current/PlanCheck.lean) <(sed -n '/^namespace AverCert.PlanCheck$/,/^end AverCert.PlanCheck$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1d;$d;/^open AverCert.Schema$/d')`

(The lowering lines are wrapped in the sub-namespace `AverCert.Schema.Lowering`
only so that this file can `open AverCert.Schema` for `TypeDecl`; the wall
declares them directly in `AverCert.Schema`.)
-/

namespace CertDecode

/-- Exact wasm value-type form admitted by this certificate profile. Tags are
    retained because nullability (`0x63`/`0x64`) and abstract shorthands are
    byte-significant even when a downstream projection ignores the detail. -/
inductive ValType
  | numeric (tag : Nat)
  | abstract (tag : Nat)
  | ref (tag : Nat) (heap : Int)
deriving Repr, DecidableEq

inductive StorageType
  | packed (tag : Nat)
  | val (type : ValType)
deriving Repr, DecidableEq

structure FieldType where
  storage : StorageType
  mutability : Nat
deriving Repr, DecidableEq

inductive CompositeType
  | funcType (params results : List ValType)
  | structType (fields : List FieldType)
  | arrayType (field : FieldType)
deriving Repr, DecidableEq

inductive SubtypeForm
  | plain
  | sub (supertypes : List Nat)
  | subFinal (supertypes : List Nat)
deriving Repr, DecidableEq

/-- One flattened subtype in absolute type-index order. -/
structure TypeEntry where
  form : SubtypeForm
  composite : CompositeType
deriving Repr, DecidableEq

end CertDecode

namespace AverCert.Schema.Lowering
open AverCert.Schema
/-- The wasm-gc field storage a scalar leaf lowers to. `intCarrier` is a
    nullable reference to the module's Int carrier struct index `C`; `boolScalar`
    is `i32`; `floatScalar` is `f64`. Record fields are IMMUTABLE (`mutability
    0`), unlike the carrier's own mutable fields. Non-scalar leaves fail closed. -/
def lowerScalarStorage (C : Nat) : TypeDecl → Option CertDecode.FieldType
  | .intCarrier => some ⟨.val (.ref 0x63 (Int.ofNat C)), 0⟩
  | .boolScalar => some ⟨.val (.numeric 0x7f), 0⟩
  | .floatScalar => some ⟨.val (.numeric 0x7c), 0⟩
  | _ => none

/-- Lower a Plan type declaration to its expected wasm-gc type-section entry.
    Stage 1: a `record` becomes a `.plain` struct whose fields are the pointwise
    scalar-storage lowering of its source-order fields; a field that is not a
    scalar leaf makes the whole `mapM` fail closed. `fuel` is the recursion floor
    for future nested records; at `0`, and for every non-record declaration, the
    lowering returns `none` (fail-closed). -/
def lowerTypeDecl (C : Nat) : Nat → TypeDecl → Option CertDecode.TypeEntry
  | 0, _ => none
  | _fuel + 1, .record _idx fields =>
      (fields.mapM (lowerScalarStorage C)).map (fun fts => ⟨.plain, .structType fts⟩)
  | _fuel + 1, _ => none
end AverCert.Schema.Lowering

namespace AverCert.PlanCheck
open AverCert.Schema
/-- Look up the resolved wasm function index for one host role in the
    byte-derived role table an artifact claim carries. A role the table lacks
    fail-closes the encoding (`none`). -/
def hostRoleIdx? (hostTable : List (HostRole × Nat)) (role : HostRole) : Option Nat :=
  match hostTable with
  | [] => none
  | (r, idx) :: rest => if r = role then some idx else hostRoleIdx? rest role

end AverCert.PlanCheck

/-!
## Verbatim fragments for the coverage lemma (`Coverage.lean`)

The plan grammar (`FragTy`, `SymIntCmp`, `FragPrim`, `FragNodeKind`/`FragNode`/
`FragBlock`, `ExprFragmentRawPlan`, `fragTyIsRecordScalar`; SchemaCore.lean
lines 314–327, 384–390, 458–485, 517–585, 590–595, 1313–1320), the plan
checker (`PlanCheck.lean` lines 20–21, 26–27, 32–35, 42–45, 57–61, 63–67,
69–72, 74–77, 95–151, 153–187, 191–193, 226–234, 243–341, 434–504, 776–778,
1456–1465: the typing helpers, `checkBlockFuel`/`checkBlock`, the Float
boundary, `checkExprFragmentRawPlan`, `natListNoDup`/`hostTableIndicesDistinct`)
and the canonical lowering (`PlanLower.lean` lines 19–201: `primInstr`, the
symbolic-stack helpers, the two monolithic templates, `lowerNodesFuel`/
`lowerBlockFuel`, `lowerBlock`, `lowerExprFragmentBody`). Same wall id.

Check with:
`diff <(sed -n '314,327p;384,390p;458,485p;517,585p;590,595p;1313,1320p' aver-cert/assets/wall/current/SchemaCore.lean) <(sed -n '/^-- BEGIN SchemaCore fragment 2$/,/^-- END SchemaCore fragment 2$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,2d;$d' | sed '$d')`
`diff <(sed -n '20,21p;26,27p;32,35p;42,45p;57,61p;63,67p;69,72p;74,77p;95,151p;153,187p;191,193p;226,234p;243,341p;434,504p;776,778p;1456,1465p' aver-cert/assets/wall/current/PlanCheck.lean) <(sed -n '/^-- BEGIN PlanCheck fragment 2$/,/^-- END PlanCheck fragment 2$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,3d;$d' | sed '$d')`
`diff <(sed -n '19,201p' aver-cert/assets/wall/current/PlanLower.lean) <(sed -n '/^-- BEGIN PlanLower fragment$/,/^-- END PlanLower fragment$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,4d;$d' | sed '$d')`
-/

-- BEGIN SchemaCore fragment 2
namespace AverCert.Schema
inductive FragTy where
  | f64
  | boolI32
  | intCarrier
  | i64
  | rawI32
  | ref
  /-- Opaque user-ADT / record reference. Unlike `ref` (an Int-carrier limb),
      this is a whole user struct/array reference handled verbatim. The concrete
      wasm type index is never part of the type: it lives on the projecting
      node (`structGetUser`) and is bound to the module bytes by the byte-exact
      gate, mirroring how `hostCall` carries its resolved function index. -/
  | adtRef
deriving Repr, DecidableEq
inductive SymIntCmp where
  | eq
  | lt
  | le
  | ge
  | gt
deriving Repr, DecidableEq
inductive FragPrim where
  | f64Add
  | f64Mul
  | f64Le
  | f64Ge
  | f64Lt
  | f64Gt
  | f64Eq
  | i64Eq
  | i64LeS
  | i64LtS
  | i64GeS
  | i64GtS
  | i32Eq
  | i32LtS
  | i32GtS
  /-- `i32.ge_s`: the tail the emitter appends to a `__aint_cmp` call for a
      source-level `>=`. The signed relational family is admitted one member at
      a time, as a plan that needs it appears; `i32.le_s` has an interpreter
      clause and a `WInstr` constructor already but no admitted plan, so it is
      deliberately still outside `FragPrim`. -/
  | i32GeS
  /-- `i32.and` restricted to the Boolean domain: `PlanCheck` types it only
      over two `boolI32` operands (NOT the loose `hasI32Ty`), because bitwise
      AND of arbitrary raw i32 values can produce a non-Boolean result
      (`2 and 2 = 2`) and the interpreter models the operation on {0,1}. -/
  | i32And
deriving Repr, DecidableEq
mutual
  /-- A single typed ANF node in an expression-fragment plan. -/
  inductive FragNodeKind where
    | local (index : Nat)
    | constBool (value : Bool)
    | constI64 (value : Int)
    | constI32 (value : Int)
    | constF64Bits (bits : Nat)
    | structGet (field : Nat) (receiver : Nat)
    /-- Projection of `field` out of a user struct of wasm type `tyIdx` (a whole
        record/ADT, not the Int carrier). The type index is node data bound to
        the module bytes by the byte-exact gate and validated against the
        struct context decoded from the artifact, mirroring `hostCall`'s
        resolved function index. -/
    | structGetUser (tyIdx : Nat) (field : Nat) (value : Nat)
    | refIsNull (value : Nat)
    | prim (op : FragPrim) (args : List Nat)
    | hostCall (role : HostRole) (funcIdx : Nat) (args : List Nat)
    /-- A self-recursive call to the function being certified. `tail` selects
        `return_call` (tail position, `0x12`) over `call` (`0x10`). `funcIdx` is
        the resolved self function index; it is bound to the module bytes by the
        byte-exact gate and validated against the decoded self index, exactly as
        `hostCall` binds its resolved index. The plan never invents it. -/
    | selfCall (tail : Bool) (funcIdx : Nat) (args : List Nat)
    | ifElse (cond : Nat) (thenBlock elseBlock : FragBlock)
    /-- Monolithic fused bounds-checked vector read: the exact emitter template
        `to_index/ge_s // to_index/len/lt_u // and // if (array.get) (box d)`
        over locals 0 (vector) and 1 (index). `arrTy` is the vector's wasm
        array type index; `toIndexIdx`/`boxIdx` are the resolved
        `__aint_to_index` / box helper indices, bound to the module bytes by
        the byte-exact gate and to the byte-derived role table by acceptance.
        The node reads locals directly and consumes no operand stack values. -/
    | vectorGetOrDefault (arrTy toIndexIdx boxIdx : Nat) (default : Int)
    /-- Construction of a user struct of wasm type `tyIdx` from `args` (source
        field order). The type index is node data bound to the module bytes by
        the byte-exact gate, mirroring how `structGetUser` binds its projection
        index. -/
    | structNew (tyIdx : Nat) (args : List Nat)
    /-- The emitter's monolithic sign template for comparing a COMPUTED Int
        carrier against an i64 literal, without calling `__aint_cmp`
        (`from_mir/builtins.rs::emit_aint_cmp_const`). The operand is already on
        the stack; the template stashes it in the scratch local, branches on
        `limbs = null`, and decides either by the native i64 compare of the
        `small` field against `constant` or — for a limb-carrying operand,
        whose value is outside the i64 band — by the sign field alone.

        `scratch` is the local slot the template writes; the checker pins it to
        `params.length`, the one declared scratch every plan-first island
        reserves, so the template can never clobber a parameter. `constant` is
        pinned to the i64 band, which is what makes the sign arm exact. Like
        `vectorGetOrDefault` this is ONE node: the whole instruction list
        lowers and runs together. -/
    | intSignCmp (op : SymIntCmp) (constant : Int) (scratch : Nat) (value : Nat)
  deriving Repr

  /-- A typed value definition. `id` must match its position in the containing
      block; `PlanCheck` enforces this before lowering. -/
  structure FragNode where
    id   : Nat
    ty   : FragTy
    kind : FragNodeKind
  deriving Repr

  /-- Ordered ANF block. `result` is the id of the value yielded by the block. -/
  structure FragBlock where
    nodes  : List FragNode
    result : Nat
  deriving Repr
end
structure ExprFragmentRawPlan where
  profile : String
  params  : List FragTy
  result  : FragTy
  body    : FragBlock
deriving Repr
def fragTyIsRecordScalar : FragTy → Bool
  | .intCarrier => true
  | .boolI32 => true
  | .f64 => true
  | .i64 => false
  | .rawI32 => false
  | .ref => false
  | .adtRef => false
end AverCert.Schema
-- END SchemaCore fragment 2

-- BEGIN PlanCheck fragment 2
namespace AverCert.PlanCheck
open AverCert.Schema
def sameTy (a b : FragTy) : Bool :=
  if a = b then true else false
def lookupNode (nodes : List FragNode) (id : Nat) : Option FragNode :=
  nodes[id]?
def lookupTy (nodes : List FragNode) (id : Nat) : Option FragTy :=
  match lookupNode nodes id with
  | some n => some n.ty
  | none => none
def hasTy (nodes : List FragNode) (id : Nat) (expected : FragTy) : Bool :=
  match lookupTy nodes id with
  | some got => sameTy got expected
  | none => false
def hasI32Ty (nodes : List FragNode) (id : Nat) : Bool :=
  match lookupTy nodes id with
  | some .rawI32 => true
  | some .boolI32 => true
  | _ => false
def carrierFieldTy? : Nat → Option FragTy
  | 0 => some .i64
  | 1 => some .ref
  | 2 => some .rawI32
  | _ => none
def isCarrierLimbField (nodes : List FragNode) (id : Nat) : Bool :=
  match lookupNode nodes id with
  | some { kind := .structGet 1 _, .. } => true
  | _ => false
def argsHaveTys (nodes : List FragNode) : List Nat → List FragTy → Bool
  | [], [] => true
  | arg :: args, ty :: tys => hasTy nodes arg ty && argsHaveTys nodes args tys
  | _, _ => false
def primResultTy? (nodes : List FragNode) (op : FragPrim) (args : List Nat) :
    Option FragTy :=
  match op with
  | .f64Add =>
      if argsHaveTys nodes args [.f64, .f64] then some .f64 else none
  | .f64Mul =>
      if argsHaveTys nodes args [.f64, .f64] then some .f64 else none
  | .f64Le =>
      if argsHaveTys nodes args [.f64, .f64] then some .boolI32 else none
  | .f64Ge =>
      if argsHaveTys nodes args [.f64, .f64] then some .boolI32 else none
  | .f64Lt =>
      if argsHaveTys nodes args [.f64, .f64] then some .boolI32 else none
  | .f64Gt =>
      if argsHaveTys nodes args [.f64, .f64] then some .boolI32 else none
  | .f64Eq =>
      if argsHaveTys nodes args [.f64, .f64] then some .boolI32 else none
  | .i64Eq =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64LeS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64LtS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64GeS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i64GtS =>
      if argsHaveTys nodes args [.i64, .i64] then some .boolI32 else none
  | .i32Eq =>
      match args with
      | [a, b] => if hasI32Ty nodes a && hasI32Ty nodes b then some .boolI32 else none
      | _ => none
  | .i32LtS =>
      match args with
      | [a, b] => if hasI32Ty nodes a && hasI32Ty nodes b then some .boolI32 else none
      | _ => none
  | .i32GtS =>
      match args with
      | [a, b] => if hasI32Ty nodes a && hasI32Ty nodes b then some .boolI32 else none
      | _ => none
  | .i32GeS =>
      match args with
      | [a, b] => if hasI32Ty nodes a && hasI32Ty nodes b then some .boolI32 else none
      | _ => none
  -- SOUNDNESS: `i32.and` must NOT use the loose `hasI32Ty` the comparisons
  -- use. Bitwise AND over arbitrary `rawI32` operands can yield a value
  -- outside {0,1} (`2 and 2 = 2`), so declaring `boolI32` for it would let a
  -- non-Boolean flow into every consumer that reads the result as a `Bool`;
  -- on top of that, the interpreter's `.i32And` clause models the operation
  -- on the {0,1} domain, where it coincides with wasm's bitwise `i32.and`
  -- only when both operands are Booleans. Requiring `.boolI32` on BOTH
  -- operands is therefore load-bearing twice over.
  | .i32And =>
      match args with
      | [a, b] =>
          if hasTy nodes a .boolI32 && hasTy nodes b .boolI32 then some .boolI32
          else none
      | _ => none
/-- Static registry of host-helper role type signatures. `box` takes one raw
    `i64` and returns the Int carrier; each arithmetic role takes two Int
    carriers and returns the Int carrier. The resolved wasm function index is
    not checked here (it is bound to the module bytes by the byte-exact gate
    and to the in-kernel decoded role table); this is purely the
    representation-level type discipline. -/
def hostCallResultTy? (nodes : List FragNode) (role : HostRole) (args : List Nat) :
    Option FragTy :=
  match role with
  | .box => if argsHaveTys nodes args [.i64] then some .intCarrier else none
  | .add =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none
  | .mul =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none
  | .sub =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .intCarrier else none
  -- `__aint_to_index` is consumed only inside the monolithic fused
  -- vector-read node; a standalone host call to it has no admitted face.
  | .toIndex => none
  -- `__aint_cmp` leaves the carrier: it takes two represented integers and
  -- returns the raw three-way sign, which the emitter always feeds into a
  -- signed comparison against `i32.const 0`. Typing it `rawI32` rather than
  -- `boolI32` is load-bearing: `-1` is a perfectly good result here and would
  -- be a lie as a Boolean, and the `boolI32`-only consumers (`i32.and`, the
  -- fragment's `if` condition) must not accept it unfiltered.
  | .cmp =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .rawI32 else none
  -- `__aint_eq` already yields the source-level Boolean (`0`/`1`), so its
  -- result IS `boolI32` and needs no comparison tail. Read this as a TYPING
  -- rule, not as a proved range: the `{0, 1}` guarantee is contract-backed
  -- only inside the certified small band (`Obligation.holds`'s `_hEq` is
  -- quantified over literal small carriers), and outside it the typing rests
  -- on the pinned helper body alone.
  | .eq =>
      if argsHaveTys nodes args [.intCarrier, .intCarrier] then some .boolI32 else none
def fragArgsAllTy (nodes : List FragNode) (expected : FragTy) : List Nat → Bool
  | [] => true
  | arg :: args => hasTy nodes arg expected && fragArgsAllTy nodes expected args
/-- Hard cap for recursive plan checking. Exceeding it is a fail-closed
    unsupported fragment, matching the producer's profile-limit discipline. -/
abbrev maxFuel : Nat := 10000

/-- Decidable membership of the i64 band `[-2^63, 2^63)`. The sign template's
    literal must live there: the limb-carrying arm decides on the sign field
    alone, and that is only exact against a literal the band contains. -/
def inI64Band (value : Int) : Bool :=
  decide (-(2 ^ 63 : Int) ≤ value) && decide (value < (2 ^ 63 : Int))
def checkBlockFuel : Nat → List FragTy → FragBlock → Bool
  | 0, _, _ => false
  | fuel + 1, params, block =>
      let inferNodeKindTy (checked : List FragNode) (node : FragNode) :
          Option FragTy :=
        match node.kind with
        | .local index => params[index]?
        | .constBool _ => some .boolI32
        | .constI64 _ => some .i64
        | .constI32 _ => some .rawI32
        | .constF64Bits _ => some .f64
        | .structGet field receiver =>
            if hasTy checked receiver .intCarrier then carrierFieldTy? field else none
        -- v1 admits three field reads out of a user struct: the opaque
        -- reference-field projection (`adtRef`, flowed verbatim through the
        -- field-projection face), the scalar `i32` tag/discriminant read
        -- (`rawI32`, e.g. the Option/Result tag) that a tag-dispatch feeds into
        -- `i32`-typed primitives, and the record-declaration scalar field read
        -- (`intCarrier`/`boolI32`/`f64` via `fragTyIsRecordScalar`), whose
        -- declared type is confirmed against the module's type section by the
        -- record face's equality pin over the certified Plan declaration. The
        -- plan DECLARES which via `node.ty`; the byte-exact gate and decoded
        -- struct context bind `tyIdx`/`field` and confirm the field's real
        -- storage. A wrong declaration lowers to bytes whose read yields the
        -- wrong `WVal` kind and traps (fail-closed). The scalar admission
        -- cannot leak into the generic path: `genericFragmentAllowedFuel`
        -- rejects EVERY `structGetUser` node outright, so a scalar-typed read
        -- is only acceptable through the record-parameter classify branch,
        -- whose exact two-node shape and byte pins gate it.
        | .structGetUser _tyIdx _field value =>
            if hasTy checked value .adtRef &&
                (node.ty = .adtRef || node.ty = .rawI32 ||
                  fragTyIsRecordScalar node.ty)
            then some node.ty else none
        -- Construction of a user struct from already-computed values: yields
        -- the opaque reference. Field-count/type agreement with the module's
        -- type section is byte-side work (the type index is bound by the
        -- byte-exact gate); the structural check demands the args exist as
        -- typed nodes. Like `structGetUser`, the generic path never admits it
        -- (`genericFragmentAllowedFuel` rejects it outright).
        | .structNew _tyIdx args =>
            if !args.isEmpty &&
                args.all (fun a => (lookupNode checked a).isSome) then
              some .adtRef
            else none
        | .refIsNull value =>
            if hasTy checked value .ref && isCarrierLimbField checked value
            then some .boolI32
            else none
        | .prim op args => primResultTy? checked op args
        | .hostCall role _funcIdx args => hostCallResultTy? checked role args
        -- A self-call yields the Int carrier when every argument is an Int
        -- carrier. `funcIdx` is not typed here; artifact acceptance binds it to
        -- the byte-derived self index,
        -- mirroring `hostCall`.
        | .selfCall _tail _funcIdx args =>
            if !args.isEmpty && fragArgsAllTy checked .intCarrier args then
              some .intCarrier
            else none
        | .ifElse cond thenBlock elseBlock =>
            if hasTy checked cond .boolI32 &&
               checkBlockFuel fuel params thenBlock &&
               checkBlockFuel fuel params elseBlock then
              match lookupNode thenBlock.nodes thenBlock.result,
                    lookupNode elseBlock.nodes elseBlock.result with
              | some t, some e => if t.ty = e.ty then some t.ty else none
              | _, _ => none
            else none
        -- The monolithic fused vector read hard-references locals 0 (vector)
        -- and 1 (index), so it types only under exactly that param prefix.
        | .vectorGetOrDefault _arrTy _toIndexIdx _boxIdx _default =>
            if params[0]? = some .adtRef && params[1]? = some .intCarrier then
              some .intCarrier
            else none
        -- The sign template consumes ONE Int carrier and yields the source
        -- Boolean. Two pins live here rather than in a face: the scratch slot
        -- is exactly the one declared local (`params.length`), so the template
        -- cannot write over a parameter, and the literal must be inside the
        -- i64 band, which is what makes its limb-carrying arm exact.
        | .intSignCmp _op constant scratch value =>
            if hasTy checked value .intCarrier && scratch = params.length &&
                inI64Band constant then
              some .boolI32
            else none
      let rec checkNodes (checked : List FragNode) : List FragNode → Bool
        | [] => true
        | node :: rest =>
            node.id = checked.length &&
              (match inferNodeKindTy checked node with
              | some ty => sameTy node.ty ty
              | none => false) &&
              checkNodes (checked ++ [node]) rest
      checkNodes [] block.nodes &&
        match lookupNode block.nodes block.result with
        | some n => n.id = block.result && block.result + 1 = block.nodes.length
        | none => false

def checkBlock (params : List FragTy) (block : FragBlock) : Bool :=
  checkBlockFuel maxFuel params block
/-- Exhaustive so extending `FragPrim` requires an explicit NaN-profile choice. -/
def primNeedsRelationalFloatResult : FragPrim → Bool
  | .f64Add => true
  | .f64Mul => true
  | .f64Le => false
  | .f64Ge => false
  | .f64Lt => false
  | .f64Gt => false
  | .f64Eq => false
  | .i64Eq => false
  | .i64LeS => false
  | .i64LtS => false
  | .i64GeS => false
  | .i64GtS => false
  | .i32Eq => false
  | .i32LtS => false
  | .i32GtS => false
  | .i32GeS => false
  | .i32And => false

/-- The general WebAssembly profile gives `f64.add`/`f64.mul` a set-valued
    result when they produce NaN: more than one sign/payload bit pattern may
    be valid. The current Float codomain face names one exact `UInt64`, so a
    Float-result plan containing either operation needs a relational result
    model before it can be admitted. Fuel exhaustion rejects fail-closed. The
    node match is deliberately exhaustive so extending `FragNodeKind` forces an
    explicit decision about nested blocks and Float-bit observation here.

    A Bool-result plan is intentionally outside this gate. In the current
    grammar the Float-to-Bool primitives are `f64.le`, `f64.ge`, `f64.lt`,
    `f64.gt` and `f64.eq`; each is an IEEE-754 ORDERED comparison, so its
    result is false for every NaN operand independently of that NaN's sign or
    payload. The single Float comparison general Wasm defines as UNORDERED,
    `f64.ne` (true whenever an operand is NaN, including `NaN != NaN`), is
    deliberately absent from `FragPrim`: it does not follow from this clause by
    analogy and would need its own decision here before it could be admitted. -/
def blockNeedsRelationalFloatResultFuel : Nat → FragBlock → Bool
  | 0, _ => true
  | fuel + 1, block =>
      block.nodes.any fun node =>
        match node.kind with
        | .prim op _ => primNeedsRelationalFloatResult op
        | .ifElse _ thenBlock elseBlock =>
            blockNeedsRelationalFloatResultFuel fuel thenBlock ||
              blockNeedsRelationalFloatResultFuel fuel elseBlock
        | .local _ => false
        | .constBool _ => false
        | .constI64 _ => false
        | .constI32 _ => false
        | .constF64Bits _ => false
        | .structGet _ _ => false
        | .structGetUser _ _ _ => false
        | .refIsNull _ => false
        | .hostCall _ _ _ => false
        | .selfCall _ _ _ => false
        | .vectorGetOrDefault _ _ _ _ => false
        | .structNew _ _ => false
        | .intSignCmp _ _ _ _ => false

def exactBitFloatResultAllowed (plan : ExprFragmentRawPlan) : Bool :=
  match plan.result with
  | .f64 => !blockNeedsRelationalFloatResultFuel maxFuel plan.body
  | _ => true

def checkExprFragmentRawPlan (plan : ExprFragmentRawPlan) : Bool :=
  plan.profile = "expr-fragment-v1" &&
    exactBitFloatResultAllowed plan &&
    checkBlock plan.params plan.body &&
    match lookupNode plan.body.nodes plan.body.result with
    | some n => sameTy n.ty plan.result
    | none => false
def natListNoDup : List Nat → Bool
  | [] => true
  | n :: rest => (!rest.contains n) && natListNoDup rest
/-- Whether a byte-derived host-role table maps its roles to pairwise DISTINCT
    function indices. The Int-face plan names host helpers by ROLE only and the
    byte lowering substitutes table indices, so with a duplicated table (e.g.
    `add` and `sub` claiming the same index) two plans differing only in an
    arm's role would lower to identical bytes — the byte-equality gate would be
    blind to the role. Requiring distinct indices restores the gate's
    discrimination; the honest table is byte-derived from the strict role
    markers, which are unique per role. -/
def hostTableIndicesDistinct (hostTable : List (HostRole × Nat)) : Bool :=
  natListNoDup (hostTable.map (fun e => e.2))
end AverCert.PlanCheck
-- END PlanCheck fragment 2

-- BEGIN PlanLower fragment
namespace AverCert.PlanLower
open AverCert.Schema
open CertPrelude
def primInstr : FragPrim → WInstr
  | .f64Add => .f64Add
  | .f64Mul => .f64Mul
  | .f64Le => .f64Le
  | .f64Ge => .f64Ge
  | .f64Lt => .f64Lt
  | .f64Gt => .f64Gt
  | .f64Eq => .f64Eq
  | .i64Eq => .i64Eq
  | .i64LeS => .i64LeS
  | .i64LtS => .i64LtS
  | .i64GeS => .i64GeS
  | .i64GtS => .i64GtS
  | .i32Eq => .i32Eq
  | .i32LtS => .i32LtS
  | .i32GtS => .i32GtS
  | .i32GeS => .i32GeS
  | .i32And => .i32And

def popExpected : List Nat → Nat → Option (List Nat)
  | got :: rest, expected => if got = expected then some rest else none
  | [], _ => none

def popExpectedAll : List Nat → List Nat → Option (List Nat)
  | stack, [] => some stack
  | stack, expected :: rest =>
      match popExpected stack expected with
      | some stack' => popExpectedAll stack' rest
      | none => none

/-- Semantic lowering uses the checker's one canonical recursive-plan budget. -/
abbrev maxFuel : Nat := AverCert.PlanCheck.maxFuel

/-- The fused `Option.withDefault(Vector.get(vec, idx), d)` template exactly as
    the wasm-gc emitter produces it (`from_mir/builtins.rs`): extract the index
    through `__aint_to_index`, test `idx >= 0 (signed) AND idx < len
    (unsigned)`, read the element on hit, box the literal default on miss.
    Holes: `toIndexIdx` (the `__aint_to_index` function index), `boxIdx` (the
    `__rt_aint_from_i64` function index), `arrTy` (the vector's array type
    index), `d` (the literal default). Locals pinned: vec = 0, idx = 1. -/
def vectorGetOrDefaultTemplate
    (toIndexIdx boxIdx arrTy : Nat) (d : Int) : List WInstr :=
  [ .localGet 1, .call toIndexIdx, .i32Const 0, .i32GeS,
    .localGet 1, .call toIndexIdx,
    .localGet 0, .arrayLen, .i32LtU,
    .i32And,
    .ifElse
      [.localGet 0, .localGet 1, .call toIndexIdx, .arrayGet arrTy]
      [.i64Const d, .call boxIdx] ]

/-- The emitter's inline sign template for `carrier OP i64-literal`
    (`from_mir/builtins.rs::emit_aint_cmp_const`), as a `WInstr` list. The
    operand is consumed off the stack into `scratch`; the `limbs = null` test
    picks the native i64 compare of the `small` field, and the limb-carrying
    arm decides on the sign field alone — `eq` needs no field read there at
    all, because a canonical limb-carrying carrier never equals an i64
    literal. Holes: `carrier` (the Int carrier struct index), `scratch` (the
    declared scratch local), `op`, `k`. -/
def intSignCmpSmallPrim : SymIntCmp → FragPrim
  | .eq => .i64Eq
  | .lt => .i64LtS
  | .le => .i64LeS
  | .ge => .i64GeS
  | .gt => .i64GtS

def intSignCmpBigArm (carrier scratch : Nat) : SymIntCmp → List WInstr
  | .eq => [.i32Const 0]
  | .lt => [.localGet scratch, .structGet carrier 2, .i32Const 0, .i32LtS]
  | .le => [.localGet scratch, .structGet carrier 2, .i32Const 0, .i32LtS]
  | .ge => [.localGet scratch, .structGet carrier 2, .i32Const 0, .i32GtS]
  | .gt => [.localGet scratch, .structGet carrier 2, .i32Const 0, .i32GtS]

def intSignCmpTemplate (carrier scratch : Nat) (op : SymIntCmp) (k : Int) :
    List WInstr :=
  [ .localSet scratch,
    .localGet scratch, .structGet carrier 1, .refIsNull,
    .ifElse
      [.localGet scratch, .structGet carrier 0, .i64Const k,
        primInstr (intSignCmpSmallPrim op)]
      (intSignCmpBigArm carrier scratch op) ]

mutual
  def lowerNodesFuel :
      Nat → Nat → List FragNode → List Nat → Option (List WInstr × List Nat)
    | 0, _, _, _ => none
    | _fuel + 1, _carrier, [], stack => some ([], stack)
    | fuel + 1, carrier, node :: rest, stack =>
        let lowered? : Option (List WInstr × List Nat) :=
          match node.kind with
          | .local index =>
              some ([.localGet index], node.id :: stack)
          | .constBool value =>
              some ([.i32Const (if value then 1 else 0)], node.id :: stack)
          | .constI64 value =>
              some ([.i64Const value], node.id :: stack)
          | .constI32 value =>
              some ([.i32Const value], node.id :: stack)
          | .constF64Bits bits =>
              some ([.f64Const (UInt64.ofNat bits)], node.id :: stack)
          | .structGet field receiver =>
              match popExpected stack receiver with
              | some stack' => some ([.structGet carrier field], node.id :: stack')
              | none => none
          | .structGetUser tyIdx field value =>
              match popExpected stack value with
              | some stack' => some ([.structGet tyIdx field], node.id :: stack')
              | none => none
          | .structNew tyIdx args =>
              match popExpectedAll stack args.reverse with
              | some stack' =>
                  some ([.structNew tyIdx args.length], node.id :: stack')
              | none => none
          | .refIsNull value =>
              match popExpected stack value with
              | some stack' => some ([.refIsNull], node.id :: stack')
              | none => none
          | .prim op args =>
              match popExpectedAll stack args.reverse with
              | some stack' => some ([primInstr op], node.id :: stack')
              | none => none
          | .hostCall _role funcIdx args =>
              match popExpectedAll stack args.reverse with
              | some stack' => some ([.call funcIdx], node.id :: stack')
              | none => none
          | .selfCall tail funcIdx args =>
              match popExpectedAll stack args.reverse with
              | some stack' =>
                  some ([if tail then .returnCall funcIdx else .call funcIdx],
                    node.id :: stack')
              | none => none
          -- Values already on the symbolic stack stay beneath the branch,
          -- exactly as the wasm `if` leaves the remaining operand stack in
          -- place (`InterpreterSequencing.wRunF_frame`).
          | .ifElse cond thenBlock elseBlock =>
              match popExpected stack cond with
              | some stack' =>
                  match lowerBlockFuel fuel carrier thenBlock,
                        lowerBlockFuel fuel carrier elseBlock with
                  | some thenInstrs, some elseInstrs =>
                      some ([.ifElse thenInstrs elseInstrs], node.id :: stack')
                  | _, _ => none
              | none => none
          | .vectorGetOrDefault arrTy toIndexIdx boxIdx default =>
              -- Monolithic template over pinned locals 0/1; it consumes no
              -- stack operands, so it is canonical only as the sole value.
              match stack with
              | [] =>
                  some (vectorGetOrDefaultTemplate toIndexIdx boxIdx arrTy default,
                    [node.id])
              | _ => none
          | .intSignCmp op k scratch value =>
              -- Monolithic template: pops its one operand off the symbolic
              -- stack exactly like `refIsNull`, then emits the whole
              -- stash/branch/compare sequence.
              match popExpected stack value with
              | some stack' =>
                  some (intSignCmpTemplate carrier scratch op k, node.id :: stack')
              | none => none
        match lowered? with
        | some (instrs, stack') =>
            match lowerNodesFuel fuel carrier rest stack' with
            | some (restInstrs, finalStack) => some (instrs ++ restInstrs, finalStack)
            | none => none
        | none => none

  def lowerBlockFuel : Nat → Nat → FragBlock → Option (List WInstr)
    | 0, _, _ => none
    | fuel + 1, carrier, block =>
        match lowerNodesFuel fuel carrier block.nodes [] with
        | some (instrs, [result]) =>
            if result = block.result then some instrs else none
        | _ => none
end

def lowerBlock (carrier : Nat) (block : FragBlock) : Option (List WInstr) :=
  lowerBlockFuel maxFuel carrier block

def lowerExprFragmentBody (carrier : Nat) (plan : ExprFragmentRawPlan) :
    Option (List WInstr) :=
  if AverCert.PlanCheck.checkExprFragmentRawPlan plan then
    lowerBlock carrier plan.body
  else
    none
end AverCert.PlanLower
-- END PlanLower fragment

/-!
## Verbatim fragments for the host contracts (`Contracts.lean`, `Adapter.lean`, `Accepted.lean`)

`CertPrelude.carrierSmall`/`boxRef`/`cmpW`/`eqW` (CertPrelude.lean lines
407–408, 427–430, 533–534, 549–550); `Schema.CarrierSpec` (SchemaCore.lean
lines 998–1013) and the five arithmetic/comparison hypotheses of
`Obligation.holds` (lines 1512–1517, 1521–1524) — the structure
`ComputeContracts` below has exactly those lines as its fields (Lean accepts
the binder syntax `(name : type)` for structure fields, so the text is the
wall's); `RecordComputeBridge.CanonRepr`/`roleArity`/`roleFn`/`nodeTypedB`/
`planTypedB` (RecordComputeBridge.lean lines 47–53, 95–111, 1709–1737);
`StandardFace.decodedRoleIdx`/`hostTableBound`/`recordComputeSlots`/
`fragNodeStructIdx?`/`recordComputeNodeOk` (StandardFace.lean lines 35–52,
869–883, 959–987); `CertDecode.AddSub.Roles` (CertDecode.lean lines 979–987).
Same wall id.

Check with:
`diff <(sed -n '407,408p;427,430p;533,534p;549,550p' aver-cert/assets/wall/current/CertPrelude.lean) <(sed -n '/^-- BEGIN CertPrelude fragment 2$/,/^-- END CertPrelude fragment 2$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,2d;$d' | sed '$d')`
`diff <(sed -n '998,1013p' aver-cert/assets/wall/current/SchemaCore.lean) <(sed -n '/^-- BEGIN SchemaCore fragment 3$/,/^-- END SchemaCore fragment 3$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,3d;$d' | sed '$d')`
`diff <(sed -n '1512,1517p;1521,1524p' aver-cert/assets/wall/current/SchemaCore.lean) <(sed -n '/^-- BEGIN SchemaCore contracts$/,/^-- END SchemaCore contracts$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,5d;$d' | sed '$d')`
`diff <(sed -n '47,53p;95,111p;1709,1737p' aver-cert/assets/wall/current/RecordComputeBridge.lean) <(sed -n '/^-- BEGIN RecordComputeBridge fragment$/,/^-- END RecordComputeBridge fragment$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,3d;$d' | sed '$d')`
`diff <(sed -n '35,52p;869,883p;959,987p' aver-cert/assets/wall/current/StandardFace.lean) <(sed -n '/^-- BEGIN StandardFace fragment$/,/^-- END StandardFace fragment$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,4d;$d' | sed '$d')`
`diff <(sed -n '979,987p' aver-cert/assets/wall/current/CertDecode.lean) <(sed -n '/^-- BEGIN CertDecode fragment 2$/,/^-- END CertDecode fragment 2$/p' aver-cert/assets/bridge-talos/Bridge/AverMin.lean | sed '1,2d;$d' | sed '$d')`
-/

-- BEGIN CertPrelude fragment 2
namespace CertPrelude
/-- Carrier constructor for a small integer. -/
def carrierSmall (C : Nat) (k : Int) : WVal := .structv C [.i64v k, .null, .i32v 0]
/-- `__rt_aint_from_i64`: box an i64 into a small carrier. -/
def boxRef (C : Nat) : List WVal → Option WVal
  | [.i64v k] => some (carrierSmall C k)
  | _ => none
def cmpW (a b : Int) : Int :=
  if a < b then -1 else if a = b then 0 else 1
def eqW (a b : Int) : Int :=
  if a = b then 1 else 0
end CertPrelude
-- END CertPrelude fragment 2

-- BEGIN SchemaCore fragment 3
namespace AverCert.Schema
open CertPrelude
structure CarrierSpec (C : Nat) where
  Repr : Int → WVal → Prop
  Canon : WVal → Prop
  car : ∀ n v, Repr n v →
    (∃ s sg, v = .structv C [.i64v s, .null, .i32v sg]) ∨
    (∃ s lty les sg, v = .structv C [.i64v s, .arr lty les, .i32v sg])
  smallIntro : ∀ k : Int, Repr k (carrierSmall C k)
  smallElim : ∀ n s sg, Repr n (.structv C [.i64v s, .null, .i32v sg]) → s = n
  bigElim : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0
  canonSmall : ∀ k : Int,
      Canon (carrierSmall C k) ↔ (-(2 ^ 63 : Int) ≤ k ∧ k < 2 ^ 63)
  canonBig : ∀ n s lty les sg,
      Repr n (.structv C [.i64v s, .arr lty les, .i32v sg]) →
      Canon (.structv C [.i64v s, .arr lty les, .i32v sg]) →
      ¬(-(2 ^ 63 : Int) ≤ n ∧ n < 2 ^ 63) ∧ sg ≠ 0
end AverCert.Schema
-- END SchemaCore fragment 3

-- BEGIN SchemaCore contracts
namespace AverCert.Schema
open CertPrelude
structure ComputeContracts {C : Nat} (S : CarrierSpec C)
    (add sub mul cmp eq : List CertPrelude.WVal → Option CertPrelude.WVal) : Prop where
    (_hadd : ∀ a b va vb w, S.Repr a va → S.Repr b vb → add [va, vb] = some w →
      S.Repr (a + b) w ∧ S.Canon w)
    (_hsub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → sub [va, vb] = some w →
      S.Repr (a - b) w ∧ S.Canon w)
    (_hmul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → mul [va, vb] = some w →
      S.Repr (a * b) w ∧ S.Canon w)
    (_hCmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
      cmp [va, vb] = some r → r = .i32v (cmpW a b))
    (_hEq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
      eq [va, vb] = some r → r = .i32v (eqW a b))
end AverCert.Schema
-- END SchemaCore contracts

-- BEGIN CertDecode fragment 2
namespace CertDecode.AddSub
structure Roles where
  box : Option Nat
  add : Option Nat
  mul : Option Nat
  sub : Option Nat
  toIndex : Option Nat
  cmp : Option Nat
  eq : Option Nat
  deriving DecidableEq, Repr
end CertDecode.AddSub
-- END CertDecode fragment 2

-- BEGIN RecordComputeBridge fragment
namespace RecordComputeBridge
open CertPrelude AverCert.Schema
/-- A represented carrier word that is additionally in the runtime's normal
    form. Every carrier this face ever holds is canonical: parameters and
    record fields by the face's domain representation, box/add/sub/mul results
    by their contracts. Canonicity is what makes the two STRUCTURAL helpers
    (`__aint_cmp`, `__aint_eq`) and the inline sign template exact. -/
def CanonRepr {C : Nat} (S : CarrierSpec C) (n : Int) (w : WVal) : Prop :=
  S.Repr n w ∧ S.Canon w
/-- The wasm arity of each host role's signature. -/
def roleArity : HostRole → Nat
  | .box => 1
  | .toIndex => 1
  | _ => 2

/-- The contract function a used role denotes; the one role the v1 face never
    admits (`toIndex`) maps to the trap-only function. -/
def roleFn (box add sub mul cmp eq : List WVal → Option WVal) :
    HostRole → List WVal → Option WVal
  | .box => box
  | .add => add
  | .sub => sub
  | .mul => mul
  | .cmp => cmp
  | .eq => eq
  | .toIndex => fun _ => none
def nodeTypedB (structIdx : Nat) (tyOf : Nat → FragTy)
    (params : List FragTy) (node : FragNode) : Bool :=
  match node.kind with
  | .local index => params[index]? == some (tyOf node.id)
  | .constI64 _ => tyOf node.id == .i64
  | .constI32 _ => tyOf node.id == .rawI32
  | .structGetUser _ _ value =>
      tyOf value == .adtRef && tyOf node.id == .intCarrier
  | .structNew tyIdx args =>
      tyIdx == structIdx &&
        (args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .adtRef)
  | .prim _ args =>
      args.all (fun a => tyOf a == .rawI32) && tyOf node.id == .boolI32
  | .hostCall .box _ args =>
      args.all (fun a => tyOf a == .i64) && tyOf node.id == .intCarrier
  | .hostCall .cmp _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .rawI32
  | .hostCall .eq _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .boolI32
  | .hostCall _ _ args =>
      args.all (fun a => tyOf a == .intCarrier) && tyOf node.id == .intCarrier
  | .intSignCmp _ _ scratch value =>
      tyOf value == .intCarrier && scratch == params.length &&
        tyOf node.id == .boolI32
  | _ => true

def planTypedB (structIdx : Nat) (tyOf : Nat → FragTy)
    (params : List FragTy) (nodes : List FragNode) : Bool :=
  nodes.all (nodeTypedB structIdx tyOf params)
end RecordComputeBridge
-- END RecordComputeBridge fragment

-- BEGIN StandardFace fragment
namespace AverCert.StandardFace
open AverCert.Schema
open CertPrelude
def decodedRoleIdx (roles : CertDecode.AddSub.Roles) : HostRole → Option Nat
  | .box => roles.box
  | .add => roles.add
  | .mul => roles.mul
  | .sub => roles.sub
  | .toIndex => roles.toIndex
  | .cmp => roles.cmp
  | .eq => roles.eq

/-- Every role/index pair used by a claim must agree with the unique table
    decoded from the module. Family checkers already require every role their
    plan consumes; distinct indices make the lookup extensional and reject
    duplicate or aliased role entries. -/
def hostTableBound
    (roles : CertDecode.AddSub.Roles)
    (hostTable : List (HostRole × Nat)) : Bool :=
  AverCert.PlanCheck.hostTableIndicesDistinct hostTable &&
    hostTable.all fun entry => decodedRoleIdx roles entry.1 == some entry.2
def recordComputeSlots
    (carrier : Nat) (add sub mul cmp eq : List WVal → Option WVal) :
    List (HostRole × Nat) → HostTbl
  | [] => fun _ => none
  | (role, idx) :: rest => fun fn =>
      if fn = idx then
        some (match role with
          | .box => ((1 : Nat), boxRef carrier)
          | .add => ((2 : Nat), add)
          | .mul => ((2 : Nat), mul)
          | .sub => ((2 : Nat), sub)
          | .eq => ((2 : Nat), eq)
          | .cmp => ((2 : Nat), cmp)
          | .toIndex => ((1 : Nat), fun _ => none))
      else recordComputeSlots carrier add sub mul cmp eq rest fn
/-- The user-struct index a node cites, if any. -/
def fragNodeStructIdx? : FragNodeKind → Option Nat
  | .structGetUser tyIdx _ _ => some tyIdx
  | .structNew tyIdx _ => some tyIdx
  | _ => none

/-- Executable admission of one node kind against the byte-derived role
    table: exactly the bridge's v1 node set, with every host call citing the
    table's index for its role at the role's arity. The two i64-band checks
    are the sign template's exactness condition and the boxing helper's
    canonicity condition; both are decided here rather than assumed. -/
def recordComputeNodeOk
    (hostTable : List (HostRole × Nat)) : FragNodeKind → Bool
  | .local _ => true
  | .constI64 value => AverCert.PlanCheck.inI64Band value
  | .constI32 _ => true
  | .structGetUser _ _ _ => true
  | .structNew _ _ => true
  | .prim .i32LtS args => args.length == 2
  | .prim .i32GtS args => args.length == 2
  | .prim .i32GeS args => args.length == 2
  | .intSignCmp _ constant _ _ => AverCert.PlanCheck.inI64Band constant
  | .hostCall role f args =>
      (AverCert.PlanCheck.hostRoleIdx? hostTable role == some f) &&
        (match role with
          | .box => args.length == 1
          | .add | .sub | .mul | .cmp | .eq => args.length == 2
          | _ => false)
  | _ => false
end AverCert.StandardFace
-- END StandardFace fragment
