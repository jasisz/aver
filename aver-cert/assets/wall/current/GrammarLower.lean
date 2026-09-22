/- GrammarLower — the lowering of a `Grammar` plan to wasm-gc, as a port of
   the MIR emitter (`src/codegen/wasm_gc/body/from_mir/**`) for exactly the
   admitted nodes (P2a, not yet wired).

   `lowerB` makes the emitter's choices from the same tree, by the same
   predicates, and never from a plan flag:

   * an `Int` comparison with an `Int` literal operand is the bignum
     const-compare tag branch (`emit_mir_numeric_binop`, builtins.rs). The
     literal is looked for on the LEFT first (`const_on_left`, then the
     operator is flipped, `flip_cmp`), then on the right. The other operand
     is RE-EMITTED per read when it is a bare `Local`
     (`aint_const_cmp_operand_is_reemittable`), and otherwise evaluated once
     and stashed in the const-compare scratch local, which is the slot right
     after the resolver slots (`SlotTable::build_for_fn`);
   * an `Int` comparison without a literal calls `__aint_cmp` and compares
     the verdict with `0`, or calls `__aint_eq` (plus `i32.eqz` for `!=`)
     (`emit_aint_binop`);
   * `Bool` `==` / `!=` are `i32.eq` / `i32.ne`; the operand type is read
     from the left operand, as the emitter reads `bop.lhs.ty()`;
   * `TailCall` is `return_call`, `Call` is `call` (the MIR decides tail
     position, the typing checks it);
   * a named `Let` is `value; local.set binding; body` — a single-use copy is
     kept as a local, exactly as the emitter keeps it;
   * `IfThenElse` takes its block type from the then-branch's type.

   ONE lowering carries both images: `lowerB` yields instructions whose `if`
   carries its block type. `eraseL` forgets the block types (the audited
   interpreter's `WInstr` tree), and `encBL` writes the bytes. So the
   instructions the simulation theorem runs and the bytes a certificate pins
   come from the same tree by construction. -/
import Grammar
import PlanBytes

namespace AverCert.Grammar
open CertPrelude AverCert.Schema

/-! ## Instructions with block types -/

inductive BI where
  | op (i : WInstr)
  | ifElse (bt : Option Ty) (thenB elseB : List BI)

mutual
  def eraseI : BI → WInstr
    | .op i => i
    | .ifElse _ t e => .ifElse (eraseL t) (eraseL e)
  def eraseL : List BI → List WInstr
    | [] => []
    | x :: xs => eraseI x :: eraseL xs
end

theorem eraseL_append (xs ys : List BI) : eraseL (xs ++ ys) = eraseL xs ++ eraseL ys := by
  induction xs with
  | nil => simp [eraseL]
  | cons x xs ih => simp [eraseL, ih]

theorem eraseL_ops (is : List WInstr) : eraseL (is.map BI.op) = is := by
  induction is with
  | nil => rfl
  | cons i is ih => simp [eraseL, eraseI, ih]

/-! ## Emitter templates -/

def BinOp.flip : BinOp → BinOp
  | .lt => .gt
  | .gt => .lt
  | .lte => .gte
  | .gte => .lte
  | op => op

/-- `mir_int_literal`: an `Int` literal operand. -/
def litInt? : Expr → Option Int
  | .literal (.int k) => some k
  | _ => none

/-- `aint_const_cmp_operand_is_reemittable`: a bare local. -/
def slot? : Expr → Option Nat
  | .local i => some i
  | _ => none

def MCtx.arithIdx (M : MCtx) : BinOp → Nat
  | .add => M.add
  | .sub => M.sub
  | _ => M.mul

/-- Small arm of the const compare: native i64 compare of the `small` field. -/
def smallCmpInstr : BinOp → WInstr
  | .eq => .i64Eq
  | .neq => .i64Ne
  | .lt => .i64LtS
  | .gt => .i64GtS
  | .lte => .i64LeS
  | _ => .i64GeS

/-- Big arm of the const compare: the sign decides every order relation, and a
    Big value never equals an i64 constant. -/
def bigCmpArm (C s : Nat) : BinOp → List WInstr
  | .lt | .lte => [.localGet s, .structGet C 2, .i32Const 0, .i32LtS]
  | .gt | .gte => [.localGet s, .structGet C 2, .i32Const 0, .i32GtS]
  | .eq => [.i32Const 0]
  | .neq => [.i32Const 1]
  | _ => []

/-- The const-compare tag branch over the carrier held in local `s`
    (`emit_aint_cmp_const_reemit`; `emit_aint_cmp_const` is `local.set s`
    followed by this). -/
def cmpArmB (C s : Nat) (op : BinOp) (k : Int) : List BI :=
  [ .op (.localGet s), .op (.structGet C 1), .op .refIsNull,
    .ifElse (some .bool)
      [.op (.localGet s), .op (.structGet C 0), .op (.i64Const k), .op (smallCmpInstr op)]
      ((bigCmpArm C s op).map .op) ]

/-- Int against Int without a literal (`emit_aint_binop`). -/
def intCmpTail (M : MCtx) : BinOp → List WInstr
  | .eq => [.call M.eq]
  | .neq => [.call M.eq, .i32Eqz]
  | .lt => [.call M.cmp, .i32Const 0, .i32LtS]
  | .gt => [.call M.cmp, .i32Const 0, .i32GtS]
  | .lte => [.call M.cmp, .i32Const 0, .i32LeS]
  | _ => [.call M.cmp, .i32Const 0, .i32GeS]

def boolCmpInstr : BinOp → WInstr
  | .eq => .i32Eq
  | _ => .i32Ne

def builtinInstr : Builtin → WInstr
  | .boolAnd => .i32And
  | .boolOr => .i32Or
  | .boolNot => .i32Eqz

/-! ## The lowering -/

mutual
  def lowerB (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) : Expr → List BI
    | .literal (.int k) => [.op (.i64Const k), .op (.call M.box)]
    | .literal (.bool v) => [.op (.i32Const (if v then 1 else 0))]
    | .local i => [.op (.localGet i)]
    | .let_ b v body =>
        lowerB M n Γ false v ++ [.op (.localSet b)] ++
          lowerB M n (match tyOf M n Γ false v with
            | some T => upd Γ b T
            | none => Γ) tail body
    | .call (.fn f) args => lowerArgsB M n Γ args ++ [.op (.call f)]
    | .call (.builtin bi) args => lowerArgsB M n Γ args ++ [.op (builtinInstr bi)]
    | .tailCall f args => lowerArgsB M n Γ args ++ [.op (.returnCall f)]
    | .binOp op l r =>
        match tyOf M n Γ false l with
        | some .bool =>
            lowerB M n Γ false l ++ lowerB M n Γ false r ++ [.op (boolCmpInstr op)]
        | _ =>
            if op.isArith then
              lowerB M n Γ false l ++ lowerB M n Γ false r ++ [.op (.call (M.arithIdx op))]
            else
              match litInt? l, litInt? r with
              | some k, _ =>
                  match slot? r with
                  | some i => cmpArmB M.carrier i op.flip k
                  | none =>
                      lowerB M n Γ false r ++ [.op (.localSet n)] ++
                        cmpArmB M.carrier n op.flip k
              | none, some k =>
                  match slot? l with
                  | some i => cmpArmB M.carrier i op k
                  | none =>
                      lowerB M n Γ false l ++ [.op (.localSet n)] ++ cmpArmB M.carrier n op k
              | none, none =>
                  lowerB M n Γ false l ++ lowerB M n Γ false r ++ (intCmpTail M op).map .op
    | .neg e => lowerB M n Γ false e ++ [.op (.call M.neg)]
    | .ifThenElse c t e =>
        lowerB M n Γ false c ++
          [.ifElse (tyOf M n Γ tail t) (lowerB M n Γ tail t) (lowerB M n Γ tail e)]
    | .recordCreate tid fs =>
        lowerArgsB M n Γ fs ++ [.op (.structNew (M.structOf tid) fs.length)]
    | .project tid i base => lowerB M n Γ false base ++ [.op (.structGet (M.structOf tid) i)]
  def lowerArgsB (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) : List Expr → List BI
    | [] => []
    | e :: es => lowerB M n Γ false e ++ lowerArgsB M n Γ es
end

/-- The instructions the interpreter runs. -/
def lowerW (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) (e : Expr) :
    List WInstr :=
  eraseL (lowerB M n Γ tail e)

def lowerArgsW (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (es : List Expr) : List WInstr :=
  eraseL (lowerArgsB M n Γ es)

/-- The instruction tree of one function. -/
def fnCode (M : MCtx) (p : FnPlan) : WCode :=
  { arity := p.sig.params.length, nlocals := p.locals.length,
    body := lowerW M p.nslots (paramsΓ p.sig.params) true p.body }

/-! ## The byte image -/

open AverCert.PlanBytes in
/-- Opcode bytes of one instruction of the admitted fragment (`none` for any
    other instruction, so an unexpected instruction fails closed). -/
def encW : WInstr → Option (List Nat)
  | .localGet i => (uleb32 i).map ([0x20] ++ ·)
  | .localSet i => (uleb32 i).map ([0x21] ++ ·)
  | .i64Const k => (sleb64 k).map ([0x42] ++ ·)
  | .i32Const k => (sleb32 k).map ([0x41] ++ ·)
  | .call f => (uleb32 f).map ([0x10] ++ ·)
  | .returnCall f => (uleb32 f).map ([0x12] ++ ·)
  | .structNew t _ => (uleb32 t).map ([0xfb, 0x00] ++ ·)
  | .structGet t fld =>
      match uleb32 t, uleb32 fld with
      | some a, some b => some ([0xfb, 0x02] ++ a ++ b)
      | _, _ => none
  | .refIsNull => some [0xd1]
  | .i32Eqz => some [0x45]
  | .i32Eq => some [0x46]
  | .i32Ne => some [0x47]
  | .i32LtS => some [0x48]
  | .i32GtS => some [0x4a]
  | .i32LeS => some [0x4c]
  | .i32GeS => some [0x4e]
  | .i64Eq => some [0x51]
  | .i64Ne => some [0x52]
  | .i64LtS => some [0x53]
  | .i64GtS => some [0x55]
  | .i64LeS => some [0x57]
  | .i64GeS => some [0x59]
  | .i32And => some [0x71]
  | .i32Or => some [0x72]
  | _ => none

open AverCert.PlanBytes in
/-- Value-type bytes of a source type: the Int carrier and records are
    nullable concrete references, Bool is `i32`. -/
def valTy (M : MCtx) : Ty → Option (List Nat)
  | .int => (s33HeapIdx M.carrier).map ([0x63] ++ ·)
  | .bool => some [0x7f]
  | .record tid => (s33HeapIdx (M.structOf tid)).map ([0x63] ++ ·)

mutual
  def encBI (M : MCtx) : BI → Option (List Nat)
    | .op i => encW i
    | .ifElse bt t e =>
        match bt.bind (valTy M), encBL M t, encBL M e with
        | some btB, some tB, some eB => some ([0x04] ++ btB ++ tB ++ [0x05] ++ eB ++ [0x0b])
        | _, _, _ => none
  def encBL (M : MCtx) : List BI → Option (List Nat)
    | [] => some []
    | x :: xs =>
        match encBI M x, encBL M xs with
        | some a, some b => some (a ++ b)
        | _, _ => none
end

/-- One `(1, type)` local group per declared local, as the emitter declares
    them (`module.rs`, `Function::new` over `(1, ty)` pairs). -/
def localGroups (M : MCtx) : List Ty → Option (List Nat)
  | [] => some []
  | t :: ts =>
      match valTy M t, localGroups M ts with
      | some a, some b => some ([0x01] ++ a ++ b)
      | _, _ => none

open AverCert.PlanBytes in
/-- The exact code entry (size prefix included) of one function. -/
def codeEntryBytes (M : MCtx) (p : FnPlan) : Option (List Nat) :=
  match uleb32 p.locals.length, localGroups M p.locals,
      encBL M (lowerB M p.nslots (paramsΓ p.sig.params) true p.body) with
  | some cnt, some groups, some body =>
      let entry := cnt ++ groups ++ body ++ [0x0b]
      (uleb32 entry.length).map (· ++ entry)
  | _, _, _ => none

end AverCert.Grammar
