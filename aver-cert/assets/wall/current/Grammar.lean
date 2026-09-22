/- Grammar — the one-grammar certificate plan (P2a-P2c, not yet wired).

   The plan IS the optimized MIR function body (`src/ir/mir/expr.rs`
   `MirExpr`), restricted to an admitted subset and printed 1:1 into this
   Lean data. There is no hand-designed IR and no classifier: `Expr` keeps
   MIR's node names and shape, so a mechanical printer can serialize a
   `MirFn` body into it and decline every other node by name.

   This file holds the grammar, its typing (`tyOf`) and its source semantics
   (`eval`, `groupModel`). `GrammarLower` ports the wasm-gc MIR emitter for
   exactly these nodes, and `GrammarSound` proves the simulation theorem.

   Admitted subset (and how it maps to `MirExpr`):

   * `literal (.int k)` / `literal (.bool b)` / `literal (.float bits)` /
     `literal (.str bytes)` — `Literal(Int)` (always in the i64 band;
     `Literal::BigInt` is declined), `Literal(Bool)`, `Literal(Float)` by its
     bit pattern and `Literal(Str)` by its UTF-8 bytes.
   * `local slot` — `Local`; the slot is the resolver `LocalId`, which is the
     wasm local index 1:1.
   * `let_ binding value body` — a NAMED `Let` (an empty `binding_name`, the
     synthetic drop form, is declined).
   * `call (.fn idx) args` — `Call { callee: Fn(..) }`, the callee by its
     wasm function index; `call (.builtin b) args` — `Call { callee:
     Builtin(..) }` for `Bool.and`, `Bool.or`, `Bool.not`, `List.prepend`.
   * `tailCall target args` — `TailCall`, target by wasm function index.
   * `binOp op lhs rhs` — `BinOp` with `ast::BinOp` minus `Div`, over two
     `Int` operands (arithmetic and the six comparisons), two `Bool`
     operands (`==` and `!=`), two `Float` operands (the comparisons but
     `!=`; no arithmetic) or two `String` operands (`+`, `==`, `!=`).
   * `neg e` — `Neg` on `Int`.
   * `ifThenElse c t e` — `IfThenElse`.
   * `recordCreate tid fields` — `RecordCreate` whose fields are written in
     declared order (the printer lists the values in that order); records
     with fewer than two fields are declined, because the emitter lowers a
     one-field record as a newtype.
   * `project tid field base` — `Project`, the field by declared index.
   * `call (.lazy b) [opt, dflt]` — `Call { callee: Builtin(..) }` for
     `Option.withDefault` / `Result.withDefault` (the boxed path: the default
     is evaluated only on the `None` / `Err` side, as the emitter does), and
     the fused `Option.withDefault(Vector.get(v, i), <literal>)` over two
     bare locals (the emitter's bounds-checked `array.get`).
   * `interp parts` — `InterpolatedStr` whose parts are all `String` (a
     literal part printed as a string literal).
   * `list t []` — the empty `List(..)` literal, with its element type.
   * `construct c ty args` — `Construct`; `c` is the constructor
     (`MirCtor::User(CtorId)` as type id + constructor index, or a built-in
     `Some`/`None`/`Ok`/`Err`) and `ty` is the node's stamped type
     (`Option<T>`, `Result<T, E>` or the sum), which the emitter reads for
     the struct index and the default filler.
   * `match_ subject arms` — `Match`, the arms 1:1 (`MirMatchArm` pattern and
     body). Patterns: `wild`, `litInt`, `litBool`, `litStr`, `bind slot`,
     `ctor c bindings`, `tuple bindings` (the bindings are the resolver
     slots, `noSlot` for `_`). The typing admits exactly the arm shapes the
     emitter lowers with first-match meaning: an Int literal cascade with a
     catch-all last, a two-arm Bool match, the two-arm Option / Result tag
     dispatch, a user variant `ref.test` cascade of two or more arms that
     covers every constructor, a String literal cascade with `_` last, and
     the single-arm flat tuple destructure.

   Values: `SVal` has nested records (`record tid fields`; a one-field record
   is a newtype, represented as its field's value), user variants (`variant
   tid ctor fields`), Option / Result values that carry their instantiation,
   Floats (bits), Strings (bytes), Vectors, Lists (`nil` / `cons`) and opaque
   pass-through values (a `Map` field). A tuple instantiation is a record
   type id of the type table. -/
import RecordComputeBridge

namespace AverCert.Grammar
open CertPrelude AverCert.Schema

/-! ## Grammar -/

/-- Source types. `record tid` is a user record and `sum tid` a user sum type
    by type id; `option` / `result` carry their instantiation. `eqref` is the
    type of the subject-scratch local only: no source value has it. -/
inductive Ty where
  | int
  | bool
  | record (tid : Nat)
  | sum (tid : Nat)
  | option (t : Ty)
  | result (t e : Ty)
  | eqref
  | float
  | string
  /-- `Vector<T>`: a wasm array of the element representation. -/
  | vec (t : Ty)
  /-- `List<T>`: `null` or a cons cell struct `{head, tail}`. -/
  | list (t : Ty)
  /-- A type the plan only passes through (a `Map` field of a constructor):
      no operation reads it, and its values are the wasm values themselves. -/
  | opaque (tid : Nat)
deriving DecidableEq, Repr

structure Sig where
  params : List Ty
  ret : Ty
deriving DecidableEq, Repr

/-- `ast::Literal`, admitted part. -/
inductive Lit where
  | int (k : Int)
  | bool (b : Bool)
  /-- `Literal(Float)` by its IEEE-754 bit pattern. -/
  | float (bits : UInt64)
  /-- `Literal(Str)` by its UTF-8 bytes. -/
  | str (bytes : List Nat)
deriving DecidableEq, Repr

/-- `ast::BinOp` without `Div`. -/
inductive BinOp where
  | add | sub | mul | eq | neq | lt | gt | lte | gte
deriving DecidableEq, Repr

/-- `MirCallee::Builtin`, admitted part (by dotted name). -/
inductive Builtin where
  | boolAnd | boolOr | boolNot
  /-- `List.prepend(head, tail)`: one cons cell. -/
  | listPrepend
  /-- `Vector.get(v, i)`: admitted only fused under `Option.withDefault` with
      a literal default (the emitter's bounds-checked `array.get`). -/
  | vecGet
deriving DecidableEq, Repr

/-- Builtins whose second argument the emitter evaluates only on one side
    of the tag test (`Option.withDefault`, `Result.withDefault`, boxed path). -/
inductive LazyBuiltin where
  | optWithDefault | resWithDefault
deriving DecidableEq, Repr

/-- `MirCallee`, admitted part. -/
inductive MirCallee where
  | fn (idx : Nat)
  | builtin (b : Builtin)
  | lazy (b : LazyBuiltin)
deriving DecidableEq, Repr

/-- `MirCtor`: a user constructor (type id, constructor index in declaration
    order) or a built-in one. -/
inductive CtorTag where
  | user (tid c : Nat)
  | some | none | ok | err
deriving DecidableEq, Repr

/-- The resolver's slot for an ignored binder (`_`), `u16::MAX`. -/
def noSlot : Nat := 65535

/-- `MirPattern`, admitted part. -/
inductive Pat where
  | wild
  | litInt (k : Int)
  | litBool (b : Bool)
  | bind (slot : Nat)
  | ctor (c : CtorTag) (bindings : List Nat)
  /-- `Literal(Str)` pattern, by its bytes. -/
  | litStr (bytes : List Nat)
  /-- A flat tuple destructure: one slot per component (`noSlot` for `_`). -/
  | tuple (bindings : List Nat)
deriving DecidableEq, Repr

mutual
  /-- `MirExpr`, admitted part. -/
  inductive Expr where
    | literal (l : Lit)
    | local (slot : Nat)
    | let_ (binding : Nat) (value body : Expr)
    | call (callee : MirCallee) (args : List Expr)
    | tailCall (target : Nat) (args : List Expr)
    | binOp (op : BinOp) (lhs rhs : Expr)
    | neg (e : Expr)
    | ifThenElse (cond thenB elseB : Expr)
    | recordCreate (tid : Nat) (fields : List Expr)
    | project (tid : Nat) (field : Nat) (base : Expr)
    | match_ (subject : Expr) (arms : Arms)
    | construct (c : CtorTag) (ty : Ty) (args : List Expr)
    /-- `InterpolatedStr` whose parts are all `String`: a literal part is
        printed as a string literal, an embed as its expression. -/
    | interp (parts : List Expr)
    /-- `List(items)` with its element type (the stamped instantiation);
        only the empty literal `[]` is admitted. -/
    | list (elem : Ty) (items : List Expr)
  /-- The arms of a `Match`, in source order. -/
  inductive Arms where
    | nil
    | cons (pat : Pat) (body : Expr) (rest : Arms)
end

def BinOp.isArith : BinOp → Bool
  | .add | .sub | .mul => true
  | _ => false

def BinOp.isEquality : BinOp → Bool
  | .eq | .neq => true
  | _ => false

/-- The Float comparisons the emitter lowers to one `f64` instruction
    (`!=` would need `f64.ne`, which the interpreter does not model). -/
def BinOp.isFloatCmp : BinOp → Bool
  | .eq | .lt | .gt | .lte | .gte => true
  | _ => false

/-- The operations on two `String` operands: `+` (concatenation) and
    `==` / `!=` (the ordering comparisons call a helper this grammar does
    not admit). -/
def BinOp.isStrOp : BinOp → Bool
  | .add | .eq | .neq => true
  | _ => false

/-- Module context: the byte-derived indices a lowering needs (Int carrier,
    host helpers, struct index per record type id, per variant constructor,
    per Option / Result instantiation, the root struct of each sum, the
    carrier's magnitude array) and the declarations the typing reads (record
    field types, constructor field types, callee signatures). -/
structure MCtx where
  carrier : Nat
  box : Nat
  add : Nat
  sub : Nat
  mul : Nat
  neg : Nat
  cmp : Nat
  eq : Nat
  structOf : Nat → Nat
  recFields : Nat → Option (List Ty)
  sigs : Nat → Option Sig
  sumCtors : Nat → Option (List (List Ty)) := fun _ => none
  ctorStruct : Nat → Nat → Nat := fun _ _ => 0
  sumRoot : Nat → Nat := fun _ => 0
  optStruct : Ty → Nat := fun _ => 0
  resStruct : Ty → Ty → Nat := fun _ _ => 0
  mag : Nat := 0
  /-- The String array type (`$string`, `(array (mut i8))`). -/
  str : Nat := 0
  /-- The passive data segment holding a string literal's bytes. -/
  strSeg : List Nat → Nat := fun _ => 0
  /-- The `Vector<String>` array type the concatenation helper takes. -/
  strVec : Nat := 0
  /-- `__wasmgc_concat_n`, `__wasmgc_string_eq`, `__aint_to_index`. -/
  concat : Nat := 0
  streq : Nat := 0
  toIndex : Nat := 0
  /-- The array type of `Vector<T>`, the cons struct of `List<T>`, and the
      heap type of an opaque type. -/
  vecStruct : Ty → Nat := fun _ => 0
  listStruct : Ty → Nat := fun _ => 0
  opaqueStruct : Nat → Nat := fun _ => 0

/-- One function's plan: signature, the resolver slot count (parameters and
    every binder; the const-compare scratch local sits at this index), the
    declared locals past the parameters (their wasm types, byte-pinned), and
    the body. -/
structure FnPlan where
  sig : Sig
  nslots : Nat
  locals : List Ty
  body : Expr

/-- Point update of a slot map. -/
def upd {α : Type} (f : Nat → Option α) (i : Nat) (a : α) : Nat → Option α :=
  fun j => if j = i then some a else f j

/-- Bind a pattern's binders in order (skipping `noSlot`), each fresh and
    below the slot count `n`; `none` on a length mismatch or a clash. -/
def bindTys (n : Nat) (Γ : Nat → Option Ty) : List Nat → List Ty → Option (Nat → Option Ty)
  | [], [] => some Γ
  | b :: bs, t :: ts =>
      if b = noSlot then bindTys n Γ bs ts
      else if b < n ∧ Γ b = none then bindTys n (upd Γ b t) bs ts else none
  | _, _ => none

/-- The field types of constructor `c` of sum `tid`. -/
def ctorFields (M : MCtx) (tid c : Nat) : Option (List Ty) :=
  (M.sumCtors tid).bind (·[c]?)

/-- A sum the emitter lowers as structs: declared, not a newtype (one
    constructor with one field, which the emitter may erase to its payload),
    and with distinct struct indices for distinct constructors (so an exact
    `ref.test` tells the constructors apart). -/
def sumOk (M : MCtx) (tid : Nat) : Bool :=
  match M.sumCtors tid with
  | some cs =>
      !(cs.length == 1 && (cs.map List.length) == [1]) &&
        (List.range cs.length).all fun a => (List.range cs.length).all fun b =>
          M.ctorStruct tid a != M.ctorStruct tid b || a == b
  | none => false

/-- A one-field record is a newtype: the emitter erases it to its field's
    value (`newtype_underlying`), so its values are represented as that
    value. -/
def MCtx.newtype (M : MCtx) (tid : Nat) : Bool :=
  match M.recFields tid with
  | some [_] => true
  | _ => false

/-- A type with a default filler value (the emitter's `emit_default_value`). -/
def Ty.hasDefault : Ty → Bool
  | .int | .bool | .record _ | .sum _ | .option _ | .result _ _ => true
  | _ => false

def Pat.isWild : Pat → Bool
  | .wild => true
  | _ => false

def Arms.length : Arms → Nat
  | .nil => 0
  | .cons _ _ rest => rest.length + 1

/-- The Int cascade needs a literal first arm, so the subject is evaluated
    at least once on every path. -/
def Arms.firstLit : Arms → Bool
  | .cons (.litInt _) _ _ => true
  | _ => false

/-- Constructor `c` is reached by some arm: a wildcard, or an arm of `c`. -/
def coversB (c : Nat) : Arms → Bool
  | .nil => false
  | .cons .wild _ _ => true
  | .cons (.ctor (.user _ c') _) _ rest => c' == c || coversB c rest
  | .cons _ _ rest => coversB c rest

/-- Every constructor of the sum is reached: the untested last arm of the
    emitter's cascade is then exactly the remaining constructor. -/
def varExhaustive (M : MCtx) (tid : Nat) (arms : Arms) : Bool :=
  match M.sumCtors tid with
  | some cs => (List.range cs.length).all fun c => coversB c arms
  | none => false

/-- The emitter's Option arm pick for the admitted shapes: `(swap, binder)`,
    where `swap` says the `Some` arm is the second one. -/
def optPick : Pat → Pat → Option (Bool × Nat)
  | .ctor .some [b], .ctor .none [] => some (false, b)
  | .ctor .some [b], .wild => some (false, b)
  | .ctor .none [], .ctor .some [b] => some (true, b)
  | .ctor .none [], .wild => some (true, noSlot)
  | _, _ => none

/-- The emitter's Result arm pick: `(swap, okBinder, errBinder)`, where
    `swap` says the `Ok` arm is the second one. -/
def resPick : Pat → Pat → Option (Bool × Nat × Nat)
  | .ctor .ok [a], .ctor .err [b] => some (false, a, b)
  | .ctor .ok [a], .wild => some (false, a, noSlot)
  | .ctor .err [b], .ctor .ok [a] => some (true, a, b)
  | .ctor .err [b], .wild => some (true, noSlot, b)
  | _, _ => none

/-- The fused `Option.withDefault(Vector.get(v, i), <literal>)` shape
    (`emit_mir_option_with_default`): the vector and index slots when the
    vector and the index are bare locals. Any other operand shape is
    declined (the emitter re-evaluates both operands per read). -/
def vecGetOr? : LazyBuiltin → Expr → Expr → Option (Nat × Nat)
  | .optWithDefault, .call (.builtin .vecGet) [.local v, .local i], .literal _ => some (v, i)
  | _, _, _ => none

/-- Every part of an interpolation is a `String`, and there is one. -/
def allStr : List Ty → Bool
  | [] => false
  | [.string] => true
  | .string :: ts => allStr ts
  | _ => false

/-- One binder of a built-in payload. -/
def bindOne (n : Nat) (Γ : Nat → Option Ty) (b : Nat) (t : Ty) : Option (Nat → Option Ty) :=
  bindTys n Γ [b] [t]

/-- The typing environment of one variant-cascade arm. -/
def varArmΓ (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tid : Nat) : Pat → Option (Nat → Option Ty)
  | .ctor (.user tid' c) bs =>
      if tid' = tid then
        match ctorFields M tid c with
        | some fts => bindTys n Γ bs fts
        | none => none
      else none
  | .wild => some Γ
  | _ => none

/-! ## Typing

One checker for every node. `n` is the resolver slot count: a `let` binder
must be below it and not yet bound (fresh), so the scratch local at `n` is
never a binder. `tail` is the position: `tailCall` is typed only in tail
position (its `return_call` leaves the function). -/

def builtinTy : Builtin → List Ty → Option Ty
  | .boolAnd, [.bool, .bool] => some .bool
  | .boolOr, [.bool, .bool] => some .bool
  | .boolNot, [.bool] => some .bool
  | .listPrepend, [t, .list t'] => if t = t' then some (.list t) else none
  | _, _ => none

/-- `withDefault` over a subject and default of these types. -/
def lazyTy : LazyBuiltin → Ty → Ty → Option Ty
  | .optWithDefault, .option t, d => if t = d ∧ t.hasDefault then some t else none
  | .resWithDefault, .result t _, d => if t = d ∧ t.hasDefault then some t else none
  | _, _, _ => none

/-- The type a constructor node builds from argument types `ts`. -/
def ctorTy (M : MCtx) : CtorTag → Ty → List Ty → Option Ty
  | .user tid c, .sum tid', ts =>
      if tid = tid' ∧ sumOk M tid ∧ ctorFields M tid c = some ts then some (.sum tid) else none
  | .some, .option t, ts => if ts = [t] ∧ t.hasDefault then some (.option t) else none
  | .none, .option t, ts => if ts = [] ∧ t.hasDefault then some (.option t) else none
  | .ok, .result t e, ts =>
      if ts = [t] ∧ t.hasDefault ∧ e.hasDefault then some (.result t e) else none
  | .err, .result t e, ts =>
      if ts = [e] ∧ t.hasDefault ∧ e.hasDefault then some (.result t e) else none
  | _, _, _ => none

mutual
  def tyOf (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) :
      Expr → Option Ty
    | .literal (.int k) => if AverCert.PlanCheck.inI64Band k then some .int else none
    | .literal (.bool _) => some .bool
    | .literal (.float _) => some .float
    | .literal (.str _) => some .string
    | .local i => Γ i
    | .let_ b v body =>
        if b < n ∧ Γ b = none then
          match tyOf M n Γ false v with
          | some T => tyOf M n (upd Γ b T) tail body
          | none => none
        else none
    | .call (.fn f) args =>
        match M.sigs f, tysOf M n Γ args with
        | some sig, some ts => if ts = sig.params then some sig.ret else none
        | _, _ => none
    | .call (.builtin bi) args =>
        match tysOf M n Γ args with
        | some ts => builtinTy bi ts
        | none => none
    | .tailCall f args =>
        if tail then
          match M.sigs f, tysOf M n Γ args with
          | some sig, some ts => if ts = sig.params then some sig.ret else none
          | _, _ => none
        else none
    | .binOp op l r =>
        match tyOf M n Γ false l, tyOf M n Γ false r with
        | some .int, some .int => if op.isArith then some .int else some .bool
        | some .bool, some .bool => if op.isEquality then some .bool else none
        | some .float, some .float => if op.isFloatCmp then some .bool else none
        | some .string, some .string =>
            if op = .add then some .string else if op.isStrOp then some .bool else none
        | _, _ => none
    | .neg e =>
        match tyOf M n Γ false e with
        | some .int => some .int
        | _ => none
    | .ifThenElse c t e =>
        match tyOf M n Γ false c, tyOf M n Γ tail t, tyOf M n Γ tail e with
        | some .bool, some T, some T' => if T = T' then some T else none
        | _, _, _ => none
    | .recordCreate tid fs =>
        match M.recFields tid, tysOf M n Γ fs with
        | some fts, some ts => if 2 ≤ fts.length ∧ ts = fts then some (.record tid) else none
        | _, _ => none
    | .project tid i base =>
        match tyOf M n Γ false base, M.recFields tid with
        | some (.record tid'), some fts =>
            if tid' = tid ∧ 2 ≤ fts.length then fts[i]? else none
        | _, _ => none
    | .call (.lazy lb) args =>
        match args with
        | [o, d] =>
            match vecGetOr? lb o d with
            | some (v, i) =>
                match Γ v, Γ i, tyOf M n Γ false d with
                | some (.vec t), some .int, some td => if td = t then some t else none
                | _, _, _ => none
            | none =>
                match tyOf M n Γ false o, tyOf M n Γ false d with
                | some to, some td => lazyTy lb to td
                | _, _ => none
        | _ => none
    | .construct c ty args =>
        match tysOf M n Γ args with
        | some ts => ctorTy M c ty ts
        | none => none
    | .interp parts =>
        match tysOf M n Γ parts with
        | some ts => if allStr ts then some .string else none
        | none => none
    | .list t items =>
        match items with
        | [] => some (.list t)
        | _ => none
    | .match_ s arms =>
        match tyOf M n Γ false s with
        | some .int => if arms.firstLit then tyIntArms M n Γ tail arms else none
        | some .bool => tyBoolArms M n Γ tail arms
        | some (.option t) => tyOptArms M n Γ tail t arms
        | some (.result t e) => tyResArms M n Γ tail t e arms
        | some (.sum tid) =>
            if sumOk M tid ∧ varExhaustive M tid arms ∧ 2 ≤ arms.length then
              tyVarArms M n Γ tail tid arms
            else none
        | some .string => tyStrArms M n Γ tail arms
        | some (.record tid) => tyTupArms M n Γ tail tid arms
        | _ => none
  def tysOf (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) : List Expr → Option (List Ty)
    | [] => some []
    | e :: es =>
        match tyOf M n Γ false e, tysOf M n Γ es with
        | some t, some ts => some (t :: ts)
        | _, _ => none
  /-- Int literal cascade: literal arms, then one catch-all (`_` or a binder)
      as the last arm. -/
  def tyIntArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) : Arms → Option Ty
    | .nil => none
    | .cons p b rest =>
        match p, rest with
        | .litInt k, _ =>
            if AverCert.PlanCheck.inI64Band k then
              match tyOf M n Γ tail b, tyIntArms M n Γ tail rest with
              | some t, some t' => if t = t' then some t else none
              | _, _ => none
            else none
        | .wild, .nil => tyOf M n Γ tail b
        | .bind s, .nil =>
            if s < n ∧ Γ s = none ∧ s ≠ noSlot then tyOf M n (upd Γ s .int) tail b else none
        | _, _ => none
  /-- Two-arm Bool match: a literal arm, then the other literal or `_`. -/
  def tyBoolArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) : Arms → Option Ty
    | .cons (.litBool v) t (.cons p e .nil) =>
        if p = .litBool (!v) ∨ p = .wild then
          match tyOf M n Γ tail t, tyOf M n Γ tail e with
          | some a, some b => if a = b then some a else none
          | _, _ => none
        else none
    | _ => none
  /-- Two-arm Option match (`optPick` shapes); the `Some` binder is fresh. -/
  def tyOptArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) (t : Ty) :
      Arms → Option Ty
    | .cons p1 b1 (.cons p2 b2 .nil) =>
        match optPick p1 p2 with
        | some (swap, sb) =>
            match bindOne n Γ sb t with
            | some Γs =>
                match swap with
                | false =>
                    match tyOf M n Γs tail b1, tyOf M n Γ tail b2 with
                    | some a, some b => if a = b then some a else none
                    | _, _ => none
                | true =>
                    match tyOf M n Γs tail b2, tyOf M n Γ tail b1 with
                    | some a, some b => if a = b then some a else none
                    | _, _ => none
            | none => none
        | none => none
    | _ => none
  /-- Two-arm Result match (`resPick` shapes); binders fresh. -/
  def tyResArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) (t e : Ty) :
      Arms → Option Ty
    | .cons p1 b1 (.cons p2 b2 .nil) =>
        match resPick p1 p2 with
        | some (swap, ob, eb) =>
            match bindOne n Γ ob t, bindOne n Γ eb e with
            | some Γo, some Γe =>
                match swap with
                | false =>
                    match tyOf M n Γo tail b1, tyOf M n Γe tail b2 with
                    | some a, some b => if a = b then some a else none
                    | _, _ => none
                | true =>
                    match tyOf M n Γo tail b2, tyOf M n Γe tail b1 with
                    | some a, some b => if a = b then some a else none
                    | _, _ => none
            | _, _ => none
        | none => none
    | _ => none
  /-- User-variant cascade: constructor arms of this sum, `_` only last. -/
  def tyVarArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) (tid : Nat) :
      Arms → Option Ty
    | .nil => none
    | .cons p b .nil =>
        match varArmΓ M n Γ tid p with
        | some Γ' => tyOf M n Γ' tail b
        | none => none
    | .cons p b (.cons p' b' r) =>
        if p.isWild then none
        else
          match varArmΓ M n Γ tid p with
          | some Γ' =>
              match tyOf M n Γ' tail b, tyVarArms M n Γ tail tid (.cons p' b' r) with
              | some a, some c => if a = c then some a else none
              | _, _ => none
          | none => none
  /-- String literal cascade (`emit_mir_string_match`): literal arms, then
      one `_` as the last arm (the emitter tests every literal arm before
      its single default, so a default anywhere else would not be
      first-match, and a binder default would never be bound). -/
  def tyStrArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) : Arms → Option Ty
    | .nil => none
    | .cons p b rest =>
        match p, rest with
        | .litStr _, _ =>
            match tyOf M n Γ tail b, tyStrArms M n Γ tail rest with
            | some t, some t' => if t = t' then some t else none
            | _, _ => none
        | .wild, .nil => tyOf M n Γ tail b
        | _, _ => none
  /-- The single-arm flat tuple destructure (`emit_mir_tuple_match`) over a
      tuple instantiation, which the type table lists as a record of two or
      more fields; at least one component is bound (so the destructure reads
      the stashed subject). -/
  def tyTupArms (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) (tid : Nat) :
      Arms → Option Ty
    | .cons (.tuple bs) b .nil =>
        match M.recFields tid with
        | some fts =>
            if 2 ≤ fts.length ∧ bs.any (· != noSlot) then
              match bindTys n Γ bs fts with
              | some Γ' => tyOf M n Γ' tail b
              | none => none
            else none
        | none => none
    | _ => none
end

/-! ## Source values -/

/-- Source values. Option and Result values carry their instantiation, so a
    value alone names the struct that represents it. -/
inductive SVal where
  | i (n : Int)
  | b (v : Bool)
  | record (tid : Nat) (fields : List SVal)
  | variant (tid c : Nat) (fields : List SVal)
  | none (t : Ty)
  | some (t : Ty) (v : SVal)
  | ok (t e : Ty) (v : SVal)
  | err (t e : Ty) (v : SVal)
  /-- A Float by its IEEE-754 bit pattern. -/
  | f (bits : UInt64)
  /-- A String by its UTF-8 bytes. -/
  | s (bytes : List Nat)
  /-- A `Vector<t>`. -/
  | vec (t : Ty) (vs : List SVal)
  /-- The empty `List<t>` and a cons cell of a `List<t>`. -/
  | nil (t : Ty)
  | cons (t : Ty) (h tl : SVal)
  /-- A value of an opaque type: the wasm value itself. -/
  | w (v : CertPrelude.WVal)
deriving Repr

mutual
  /-- A source value inhabits a type, over the module's record and sum
      declarations. -/
  def HasTy (M : MCtx) : SVal → Ty → Prop
    | .i _, .int => True
    | .b _, .bool => True
    | .record tid fs, .record tid' =>
        tid = tid' ∧ ∃ fts, M.recFields tid = some fts ∧ HasTyL M fs fts
    | .variant tid c fs, .sum tid' =>
        tid = tid' ∧ ∃ fts, ctorFields M tid c = some fts ∧ HasTyL M fs fts
    | .none t, .option t' => t = t'
    | .some t v, .option t' => t = t' ∧ HasTy M v t
    | .ok t e v, .result t' e' => t = t' ∧ e = e' ∧ HasTy M v t
    | .err t e v, .result t' e' => t = t' ∧ e = e' ∧ HasTy M v e
    | .f _, .float => True
    | .s _, .string => True
    | .vec t vs, .vec t' => t = t' ∧ HasTyAll M vs t
    | .nil t, .list t' => t = t'
    | .cons t h tl, .list t' => t = t' ∧ HasTy M h t ∧ HasTy M tl (.list t)
    | .w _, .opaque _ => True
    | _, _ => False
  def HasTyL (M : MCtx) : List SVal → List Ty → Prop
    | [], [] => True
    | v :: vs, t :: ts => HasTy M v t ∧ HasTyL M vs ts
    | _, _ => False
  /-- Every element has type `t`. -/
  def HasTyAll (M : MCtx) : List SVal → Ty → Prop
    | [], _ => True
    | v :: vs, t => HasTy M v t ∧ HasTyAll M vs t
end

/-! ## Source semantics

`F f` is the meaning of callee `f` (already specialised to a fuel level);
nothing here knows any function body. Evaluation is pure and strict, except
that `withDefault` evaluates its default only on the `None` / `Err` side (as
the emitted code does), and a `Match` takes its FIRST matching arm. -/

def intBin : BinOp → Int → Int → SVal
  | .add, x, y => .i (x + y)
  | .sub, x, y => .i (x - y)
  | .mul, x, y => .i (x * y)
  | .eq, x, y => .b (decide (x = y))
  | .neq, x, y => .b (decide (x ≠ y))
  | .lt, x, y => .b (decide (x < y))
  | .gt, x, y => .b (decide (x > y))
  | .lte, x, y => .b (decide (x ≤ y))
  | .gte, x, y => .b (decide (x ≥ y))

def boolBin : BinOp → Bool → Bool → Option SVal
  | .eq, x, y => some (.b (x == y))
  | .neq, x, y => some (.b (x != y))
  | _, _, _ => none

/-- Float comparisons, read exactly as the audited interpreter reads the
    `f64` instruction the emitter picks (IEEE-754: every comparison with a
    NaN is false, and `-0.0 == 0.0`). -/
def floatBin : BinOp → UInt64 → UInt64 → Option SVal
  | .eq, x, y => some (.b (CertPrelude.f x == CertPrelude.f y))
  | .lt, x, y => some (.b (decide (CertPrelude.f x < CertPrelude.f y)))
  | .gt, x, y => some (.b (decide (CertPrelude.f y < CertPrelude.f x)))
  | .lte, x, y => some (.b (decide (CertPrelude.f x ≤ CertPrelude.f y)))
  | .gte, x, y => some (.b (decide (CertPrelude.f y ≤ CertPrelude.f x)))
  | _, _, _ => none

/-- String concatenation and byte equality. -/
def strBin : BinOp → List Nat → List Nat → Option SVal
  | .add, x, y => some (.s (x ++ y))
  | .eq, x, y => some (.b (x == y))
  | .neq, x, y => some (.b (x != y))
  | _, _, _ => none

/-- The bytes of a list of Strings, concatenated in order. -/
def strCat : List SVal → Option (List Nat)
  | [] => some []
  | .s x :: rest => (strCat rest).map (x ++ ·)
  | _ => none

def builtinEval : Builtin → List SVal → Option SVal
  | .boolAnd, [.b x, .b y] => some (.b (x && y))
  | .boolOr, [.b x, .b y] => some (.b (x || y))
  | .boolNot, [.b x] => some (.b (!x))
  | .listPrepend, [h, .nil t] => some (.cons t h (.nil t))
  | .listPrepend, [h, .cons t x r] => some (.cons t h (.cons t x r))
  | .vecGet, [.vec t vs, .i n] =>
      if 0 ≤ n ∧ n < vs.length then (vs[n.toNat]?).map (.some t) else some (.none t)
  | _, _ => none

/-- The value a constructor node builds. -/
def ctorVal : CtorTag → Ty → List SVal → Option SVal
  | .user tid c, _, vs => some (.variant tid c vs)
  | .some, .option t, [v] => some (.some t v)
  | .none, .option t, [] => some (.none t)
  | .ok, .result t e, [v] => some (.ok t e v)
  | .err, .result t e, [v] => some (.err t e v)
  | _, _, _ => none

/-- A pattern against a value: `none` when it does not match, else the
    binders with the values they take (in field order). -/
def patMatch : Pat → SVal → Option (List Nat × List SVal)
  | .wild, _ => some ([], [])
  | .litInt k, .i x => if x = k then some ([], []) else none
  | .litBool v, .b x => if x = v then some ([], []) else none
  | .bind s, v => some ([s], [v])
  | .ctor (.user tid c) bs, .variant tid' c' fs =>
      if tid = tid' ∧ c = c' then some (bs, fs) else none
  | .ctor .some bs, .some _ v => some (bs, [v])
  | .ctor .none bs, .none _ => some (bs, [])
  | .ctor .ok bs, .ok _ _ v => some (bs, [v])
  | .ctor .err bs, .err _ _ v => some (bs, [v])
  | .litStr k, .s x => if x = k then some ([], []) else none
  | .tuple bs, .record _ fs => some (bs, fs)
  | _, _ => none

/-- Bind the binders in order, skipping `noSlot`; `none` on a length
    mismatch. -/
def bindVals (env : Nat → Option SVal) : List Nat → List SVal → Option (Nat → Option SVal)
  | [], [] => some env
  | b :: bs, v :: vs => if b = noSlot then bindVals env bs vs else bindVals (upd env b v) bs vs
  | _, _ => none

mutual
  def eval (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) :
      Expr → Option SVal
    | .literal (.int k) => some (.i k)
    | .literal (.bool v) => some (.b v)
    | .literal (.float bits) => some (.f bits)
    | .literal (.str bytes) => some (.s bytes)
    | .local i => env i
    | .let_ b v body =>
        match eval F env v with
        | some x => eval F (upd env b x) body
        | none => none
    | .call (.fn f) args =>
        match evalArgs F env args with
        | some vs => F f vs
        | none => none
    | .call (.builtin bi) args =>
        match evalArgs F env args with
        | some vs => builtinEval bi vs
        | none => none
    | .tailCall f args =>
        match evalArgs F env args with
        | some vs => F f vs
        | none => none
    | .binOp op l r =>
        match eval F env l, eval F env r with
        | some (.i x), some (.i y) => some (intBin op x y)
        | some (.b x), some (.b y) => boolBin op x y
        | some (.f x), some (.f y) => floatBin op x y
        | some (.s x), some (.s y) => strBin op x y
        | _, _ => none
    | .neg e =>
        match eval F env e with
        | some (.i x) => some (.i (-x))
        | _ => none
    | .ifThenElse c t e =>
        match eval F env c with
        | some (.b true) => eval F env t
        | some (.b false) => eval F env e
        | _ => none
    | .recordCreate tid fs =>
        match evalArgs F env fs with
        | some vs => some (.record tid vs)
        | none => none
    | .project _ i base =>
        match eval F env base with
        | some (.record _ fs) => fs[i]?
        | _ => none
    | .call (.lazy lb) args =>
        match args with
        | [o, d] =>
            match lb, eval F env o with
            | .optWithDefault, some (.some _ v) => some v
            | .optWithDefault, some (.none _) => eval F env d
            | .resWithDefault, some (.ok _ _ v) => some v
            | .resWithDefault, some (.err _ _ _) => eval F env d
            | _, _ => none
        | _ => none
    | .construct c ty args =>
        match evalArgs F env args with
        | some vs => ctorVal c ty vs
        | none => none
    | .match_ s arms =>
        match eval F env s with
        | some v => evalArms F env v arms
        | none => none
    | .interp parts =>
        match evalArgs F env parts with
        | some vs => (strCat vs).map .s
        | none => none
    | .list t items =>
        match items with
        | [] => some (.nil t)
        | _ => none
  def evalArgs (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) :
      List Expr → Option (List SVal)
    | [] => some []
    | e :: es =>
        match eval F env e, evalArgs F env es with
        | some v, some vs => some (v :: vs)
        | _, _ => none
  /-- First-match: the first arm whose pattern matches runs, with its
      binders bound. -/
  def evalArms (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (v : SVal) :
      Arms → Option SVal
    | .nil => none
    | .cons p b rest =>
        match patMatch p v with
        | some (bs, vs) =>
            match bindVals env bs vs with
            | some env' => eval F env' b
            | none => none
        | none => evalArms F env v rest
end

def argsEnv (vs : List SVal) : Nat → Option SVal := fun i => vs[i]?

def paramsΓ (ts : List Ty) : Nat → Option Ty := fun i => ts[i]?

/-- The lowering's local layout: `n` resolver slots (parameters and binders),
    then the scratch locals the emitter reserves after them
    (`SlotTable::build_for_fn`): the subject scratch (`eqref`, present when
    the function has a constructor match) and the const-compare scratch. -/
structure LCtx where
  n : Nat
  cmp : Nat
  subj : Nat

/-- Scratch positions as a WALL function of the declared locals: the subject
    scratch is the `eqref` local right after the resolver slots, and the
    const-compare scratch follows it (or sits there when there is none). -/
def FnPlan.lctx (p : FnPlan) : LCtx :=
  if p.locals[p.nslots - p.sig.params.length]? = some .eqref then
    { n := p.nslots, cmp := p.nslots + 1, subj := p.nslots }
  else
    { n := p.nslots, cmp := p.nslots, subj := p.nslots }

/-- The typing and slot-layout checks one plan must pass: parameters sit
    below the slot count, the scratch locals are declared, and the body has
    the declared result type in tail position. -/
def planTyped (M : MCtx) (p : FnPlan) : Bool :=
  decide (p.sig.params.length ≤ p.nslots) &&
    decide (p.nslots ≤ p.sig.params.length + p.locals.length) &&
    decide (tyOf M p.nslots (paramsΓ p.sig.params) true p.body = some p.sig.ret)

/-- The meaning of a group of functions (one SCC), fuel-indexed exactly as
    `wFuncN` peels fuel: at fuel `k + 1` a member's body runs with every
    callee at fuel `k` — members through this model, functions outside the
    group through `outer`. -/
def groupModel (outer : Nat → Nat → List SVal → Option SVal)
    (G : Nat → Option FnPlan) : Nat → Nat → List SVal → Option SVal
  | 0, f, args =>
      match G f with
      | some _ => none
      | none => outer 0 f args
  | k + 1, f, args =>
      match G f with
      | some p => eval (groupModel outer G k) (argsEnv args) p.body
      | none => outer (k + 1) f args

end AverCert.Grammar
