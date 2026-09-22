/- GrammarLower — the lowering of a `Grammar` plan to wasm-gc, as a port of
   the MIR emitter (`src/codegen/wasm_gc/body/from_mir/**`) for exactly the
   admitted nodes (P2a/P2b, not yet wired).

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
   * `IfThenElse` takes its block type from the then-branch's type;
   * an Int `Match` is the literal cascade (`emit_mir_int_cascade`): the
     subject RE-EMITTED per arm, the literal boxed and compared with
     `__aint_eq`, the catch-all binder stored from one more subject run;
   * a two-arm Bool `Match` is one `if` on the subject;
   * an Option / Result `Match` stashes the subject in the subject scratch,
     `ref.cast`s it and reads the tag field, then extracts the payload
     binder (a dead binder too, `noSlot` skipped) before the arm body;
   * a user-variant `Match` stashes the subject and runs the `ref.test`
     cascade over the arms, the LAST arm untested
     (`emit_mir_variant_dispatch`); binders are extracted by
     `ref.cast` + `struct.get` per field;
   * `withDefault` stashes the carrier and runs the default only in `else`;
   * `Construct` pushes the tag, the payload and the default filler of the
     other side, then `struct.new` (`constructors.rs`).

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
  /-- `ref.null ht`: the interpreter's `refNull`, with its heap type for
      the bytes. -/
  | nullOf (ht : Nat)

mutual
  def eraseI : BI → WInstr
    | .op i => i
    | .ifElse _ t e => .ifElse (eraseL t) (eraseL e)
    | .nullOf _ => .refNull
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

/-- `emit_default_value`: the filler of the unused payload field. -/
def dfltB (M : MCtx) : Ty → List BI
  | .int => [.op (.i64Const 0), .nullOf M.mag, .op (.i32Const 0), .op (.structNew M.carrier 3)]
  | .bool => [.op (.i32Const 0)]
  | .record tid => [.nullOf (M.structOf tid)]
  | .sum tid => [.nullOf (M.sumRoot tid)]
  | .option t => [.nullOf (M.optStruct t)]
  | .result t e => [.nullOf (M.resStruct t e)]
  | .eqref => []

/-- Read field `i` of the struct `idx` held (as `eqref`) in the subject
    scratch `ss` into binder `b`; nothing for an ignored binder. -/
def bindFieldB (ss idx i b : Nat) : List BI :=
  if b = noSlot then []
  else [.op (.localGet ss), .op (.refCast idx), .op (.structGet idx i), .op (.localSet b)]

/-- The binders of a variant arm, fields `i, i+1, …`. -/
def extractB (ss idx : Nat) : Nat → List Nat → List BI
  | _, [] => []
  | i, b :: bs => bindFieldB ss idx i b ++ extractB ss idx (i + 1) bs

/-- The tag test of an Option / Result held in the subject scratch. -/
def tagTestB (ss idx : Nat) : List BI :=
  [.op (.localGet ss), .op (.refCast idx), .op (.structGet idx 0), .op (.i32Const 1), .op .i32Eq]

/-! ## The lowering -/

mutual
  def lowerB (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) : Expr → List BI
    | .literal (.int k) => [.op (.i64Const k), .op (.call M.box)]
    | .literal (.bool v) => [.op (.i32Const (if v then 1 else 0))]
    | .local i => [.op (.localGet i)]
    | .let_ b v body =>
        lowerB M X Γ false v ++ [.op (.localSet b)] ++
          lowerB M X (match tyOf M X.n Γ false v with
            | some T => upd Γ b T
            | none => Γ) tail body
    | .call (.fn f) args => lowerArgsB M X Γ args ++ [.op (.call f)]
    | .call (.builtin bi) args => lowerArgsB M X Γ args ++ [.op (builtinInstr bi)]
    | .tailCall f args => lowerArgsB M X Γ args ++ [.op (.returnCall f)]
    | .binOp op l r =>
        match tyOf M X.n Γ false l with
        | some .bool =>
            lowerB M X Γ false l ++ lowerB M X Γ false r ++ [.op (boolCmpInstr op)]
        | _ =>
            if op.isArith then
              lowerB M X Γ false l ++ lowerB M X Γ false r ++ [.op (.call (M.arithIdx op))]
            else
              match litInt? l, litInt? r with
              | some k, _ =>
                  match slot? r with
                  | some i => cmpArmB M.carrier i op.flip k
                  | none =>
                      lowerB M X Γ false r ++ [.op (.localSet X.cmp)] ++
                        cmpArmB M.carrier X.cmp op.flip k
              | none, some k =>
                  match slot? l with
                  | some i => cmpArmB M.carrier i op k
                  | none =>
                      lowerB M X Γ false l ++ [.op (.localSet X.cmp)] ++
                        cmpArmB M.carrier X.cmp op k
              | none, none =>
                  lowerB M X Γ false l ++ lowerB M X Γ false r ++ (intCmpTail M op).map .op
    | .neg e => lowerB M X Γ false e ++ [.op (.call M.neg)]
    | .ifThenElse c t e =>
        lowerB M X Γ false c ++
          [.ifElse (tyOf M X.n Γ tail t) (lowerB M X Γ tail t) (lowerB M X Γ tail e)]
    | .recordCreate tid fs =>
        lowerArgsB M X Γ fs ++ [.op (.structNew (M.structOf tid) fs.length)]
    | .project tid i base => lowerB M X Γ false base ++ [.op (.structGet (M.structOf tid) i)]
    | .call (.lazy lb) args =>
        match args with
        | [o, d] =>
            match lb, tyOf M X.n Γ false o with
            | .optWithDefault, some (.option t) =>
                lowerB M X Γ false o ++ [.op (.localSet X.subj)] ++
                  tagTestB X.subj (M.optStruct t) ++
                  [.ifElse (some t)
                    [.op (.localGet X.subj), .op (.refCast (M.optStruct t)),
                      .op (.structGet (M.optStruct t) 1)]
                    (lowerB M X Γ false d)]
            | .resWithDefault, some (.result t e) =>
                lowerB M X Γ false o ++ [.op (.localSet X.subj)] ++
                  tagTestB X.subj (M.resStruct t e) ++
                  [.ifElse (some t)
                    [.op (.localGet X.subj), .op (.refCast (M.resStruct t e)),
                      .op (.structGet (M.resStruct t e) 1)]
                    (lowerB M X Γ false d)]
            | _, _ => []
        | _ => []
    | .construct c ty args =>
        match c, ty with
        | .user tid k, _ =>
            lowerArgsB M X Γ args ++ [.op (.structNew (M.ctorStruct tid k) args.length)]
        | .some, .option t =>
            [.op (.i32Const 1)] ++ lowerArgsB M X Γ args ++ [.op (.structNew (M.optStruct t) 2)]
        | .none, .option t =>
            [.op (.i32Const 0)] ++ dfltB M t ++ lowerArgsB M X Γ args ++
              [.op (.structNew (M.optStruct t) 2)]
        | .ok, .result t e =>
            [.op (.i32Const 1)] ++ lowerArgsB M X Γ args ++ dfltB M e ++
              [.op (.structNew (M.resStruct t e) 3)]
        | .err, .result t e =>
            [.op (.i32Const 0)] ++ dfltB M t ++ lowerArgsB M X Γ args ++
              [.op (.structNew (M.resStruct t e) 3)]
        | _, _ => []
    | .match_ s arms =>
        match tyOf M X.n Γ false s with
        | some .int =>
            lowerIntArms M X Γ tail (lowerB M X Γ false s)
              (tyOf M X.n Γ tail (.match_ s arms)) arms
        | some .bool =>
            lowerB M X Γ false s ++
              lowerBoolArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) arms
        | some (.option t) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerOptArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) t arms
        | some (.result t e) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerResArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) t e arms
        | some (.sum tid) =>
            lowerB M X Γ false s ++ [.op (.localSet X.subj)] ++
              lowerVarArms M X Γ tail (tyOf M X.n Γ tail (.match_ s arms)) tid arms
        | _ => []
  def lowerArgsB (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) : List Expr → List BI
    | [] => []
    | e :: es => lowerB M X Γ false e ++ lowerArgsB M X Γ es
  /-- `emit_mir_int_cascade`: `sc` is the subject's code, re-emitted per
      literal arm; the first catch-all ends the cascade. -/
  def lowerIntArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) (sc : List BI)
      (bt : Option Ty) : Arms → List BI
    | .nil => []
    | .cons p b rest =>
        match p with
        | .litInt k =>
            sc ++ [.op (.i64Const k), .op (.call M.box), .op (.call M.eq),
              .ifElse bt (lowerB M X Γ tail b) (lowerIntArms M X Γ tail sc bt rest)]
        | .wild => lowerB M X Γ tail b
        | .bind s => sc ++ [.op (.localSet s)] ++ lowerB M X (upd Γ s .int) tail b
        | _ => []
  /-- The emitter's Bool `match`: one `if` on the subject. -/
  def lowerBoolArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) : Arms → List BI
    | .cons (.litBool v) t (.cons _ e _) =>
        if v then [.ifElse bt (lowerB M X Γ tail t) (lowerB M X Γ tail e)]
        else [.ifElse bt (lowerB M X Γ tail e) (lowerB M X Γ tail t)]
    | _ => []
  /-- `emit_mir_option_match`, the subject already in the subject scratch. -/
  def lowerOptArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (t : Ty) : Arms → List BI
    | .cons p1 b1 (.cons p2 b2 _) =>
        match optPick p1 p2 with
        | some (false, sb) =>
            tagTestB X.subj (M.optStruct t) ++
              [.ifElse bt
                (bindFieldB X.subj (M.optStruct t) 1 sb ++
                  lowerB M X ((bindOne X.n Γ sb t).getD Γ) tail b1)
                (lowerB M X Γ tail b2)]
        | some (true, sb) =>
            tagTestB X.subj (M.optStruct t) ++
              [.ifElse bt
                (bindFieldB X.subj (M.optStruct t) 1 sb ++
                  lowerB M X ((bindOne X.n Γ sb t).getD Γ) tail b2)
                (lowerB M X Γ tail b1)]
        | none => []
    | _ => []
  /-- `emit_mir_result_match`: the `Ok` payload is field 1, the `Err` one
      field 2. -/
  def lowerResArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (t e : Ty) : Arms → List BI
    | .cons p1 b1 (.cons p2 b2 _) =>
        match resPick p1 p2 with
        | some (false, ob, eb) =>
            tagTestB X.subj (M.resStruct t e) ++
              [.ifElse bt
                (bindFieldB X.subj (M.resStruct t e) 1 ob ++
                  lowerB M X ((bindOne X.n Γ ob t).getD Γ) tail b1)
                (bindFieldB X.subj (M.resStruct t e) 2 eb ++
                  lowerB M X ((bindOne X.n Γ eb e).getD Γ) tail b2)]
        | some (true, ob, eb) =>
            tagTestB X.subj (M.resStruct t e) ++
              [.ifElse bt
                (bindFieldB X.subj (M.resStruct t e) 1 ob ++
                  lowerB M X ((bindOne X.n Γ ob t).getD Γ) tail b2)
                (bindFieldB X.subj (M.resStruct t e) 2 eb ++
                  lowerB M X ((bindOne X.n Γ eb e).getD Γ) tail b1)]
        | none => []
    | _ => []
  /-- `emit_mir_variant_arm_cascade`: `ref.test` each arm's constructor, the
      last arm untested; a `_` arm ends the cascade. -/
  def lowerVarArms (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool)
      (bt : Option Ty) (tid : Nat) : Arms → List BI
    | .nil => []
    | .cons p b .nil =>
        match p with
        | .ctor (.user tid' c) bs =>
            extractB X.subj (M.ctorStruct tid' c) 0 bs ++
              lowerB M X ((varArmΓ M X.n Γ tid p).getD Γ) tail b
        | _ => lowerB M X Γ tail b
    | .cons p b (.cons p' b' r) =>
        match p with
        | .ctor (.user tid' c) bs =>
            [.op (.localGet X.subj), .op (.refTest (M.ctorStruct tid' c)),
              .ifElse bt
                (extractB X.subj (M.ctorStruct tid' c) 0 bs ++
                  lowerB M X ((varArmΓ M X.n Γ tid p).getD Γ) tail b)
                (lowerVarArms M X Γ tail bt tid (.cons p' b' r))]
        | .wild => lowerB M X Γ tail b
        | _ => []
end

/-- The instructions the interpreter runs. -/
def lowerW (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (tail : Bool) (e : Expr) :
    List WInstr :=
  eraseL (lowerB M X Γ tail e)

def lowerArgsW (M : MCtx) (X : LCtx) (Γ : Nat → Option Ty) (es : List Expr) : List WInstr :=
  eraseL (lowerArgsB M X Γ es)

/-- The instruction tree of one function. -/
def fnCode (M : MCtx) (p : FnPlan) : WCode :=
  { arity := p.sig.params.length, nlocals := p.locals.length,
    body := lowerW M p.lctx (paramsΓ p.sig.params) true p.body }

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
  | .refTest t => (s33HeapIdx t).map ([0xfb, 0x14] ++ ·)
  | .refCast t => (s33HeapIdx t).map ([0xfb, 0x16] ++ ·)
  | _ => none

open AverCert.PlanBytes in
/-- Value-type bytes of a source type: the Int carrier, records, sums (their
    root struct), Option and Result are nullable concrete references, Bool is
    `i32`, and the subject scratch is `eqref`. -/
def valTy (M : MCtx) : Ty → Option (List Nat)
  | .int => (s33HeapIdx M.carrier).map ([0x63] ++ ·)
  | .bool => some [0x7f]
  | .record tid => (s33HeapIdx (M.structOf tid)).map ([0x63] ++ ·)
  | .sum tid => (s33HeapIdx (M.sumRoot tid)).map ([0x63] ++ ·)
  | .option t => (s33HeapIdx (M.optStruct t)).map ([0x63] ++ ·)
  | .result t e => (s33HeapIdx (M.resStruct t e)).map ([0x63] ++ ·)
  | .eqref => some [0x6d]

open AverCert.PlanBytes in
mutual
  def encBI (M : MCtx) : BI → Option (List Nat)
    | .op i => encW i
    | .ifElse bt t e =>
        match bt.bind (valTy M), encBL M t, encBL M e with
        | some btB, some tB, some eB => some ([0x04] ++ btB ++ tB ++ [0x05] ++ eB ++ [0x0b])
        | _, _, _ => none
    | .nullOf ht => (s33HeapIdx ht).map ([0xd0] ++ ·)
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
      encBL M (lowerB M p.lctx (paramsΓ p.sig.params) true p.body) with
  | some cnt, some groups, some body =>
      let entry := cnt ++ groups ++ body ++ [0x0b]
      (uleb32 entry.length).map (· ++ entry)
  | _, _, _ => none

end AverCert.Grammar
