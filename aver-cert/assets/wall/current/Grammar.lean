/- Grammar — the one-grammar certificate plan (P2a, not yet wired).

   The plan IS the optimized MIR function body (`src/ir/mir/expr.rs`
   `MirExpr`), restricted to an admitted subset and printed 1:1 into this
   Lean data. There is no hand-designed IR and no classifier: `Expr` keeps
   MIR's node names and shape, so a mechanical printer can serialize a
   `MirFn` body into it and decline every other node by name.

   This file holds the grammar, its typing (`tyOf`) and its source semantics
   (`eval`, `groupModel`). `GrammarLower` ports the wasm-gc MIR emitter for
   exactly these nodes, and `GrammarSound` proves the simulation theorem.

   Admitted subset (and how it maps to `MirExpr`):

   * `literal (.int k)` / `literal (.bool b)` — `Literal(Int)` (always in the
     i64 band; `Literal::BigInt` is declined) and `Literal(Bool)`.
   * `local slot` — `Local`; the slot is the resolver `LocalId`, which is the
     wasm local index 1:1.
   * `let_ binding value body` — a NAMED `Let` (an empty `binding_name`, the
     synthetic drop form, is declined).
   * `call (.fn idx) args` — `Call { callee: Fn(..) }`, the callee by its
     wasm function index; `call (.builtin b) args` — `Call { callee:
     Builtin(..) }` for `Bool.and`, `Bool.or`, `Bool.not`.
   * `tailCall target args` — `TailCall`, target by wasm function index.
   * `binOp op lhs rhs` — `BinOp` with `ast::BinOp` minus `Div`, over two
     `Int` operands (arithmetic and the six comparisons) or two `Bool`
     operands (`==` and `!=`).
   * `neg e` — `Neg` on `Int`.
   * `ifThenElse c t e` — `IfThenElse`.
   * `recordCreate tid fields` — `RecordCreate` whose fields are written in
     declared order (the printer lists the values in that order); records
     with fewer than two fields are declined, because the emitter lowers a
     one-field record as a newtype.
   * `project tid field base` — `Project`, the field by declared index.

   Values: `SVal` extends the scalar values with nested records
   (`record tid fields`), so records with Int, Bool and record fields are all
   covered. Variants, Option/Result, String, lists and Float are P2b. -/
import RecordComputeBridge

namespace AverCert.Grammar
open CertPrelude AverCert.Schema

/-! ## Grammar -/

/-- Source types. `record tid` is a user record by its type id. -/
inductive Ty where
  | int
  | bool
  | record (tid : Nat)
deriving DecidableEq, Repr

structure Sig where
  params : List Ty
  ret : Ty
deriving DecidableEq, Repr

/-- `ast::Literal`, admitted part. -/
inductive Lit where
  | int (k : Int)
  | bool (b : Bool)
deriving DecidableEq, Repr

/-- `ast::BinOp` without `Div`. -/
inductive BinOp where
  | add | sub | mul | eq | neq | lt | gt | lte | gte
deriving DecidableEq, Repr

/-- `MirCallee::Builtin`, admitted part (by dotted name). -/
inductive Builtin where
  | boolAnd | boolOr | boolNot
deriving DecidableEq, Repr

/-- `MirCallee`, admitted part. -/
inductive MirCallee where
  | fn (idx : Nat)
  | builtin (b : Builtin)
deriving DecidableEq, Repr

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
deriving Repr

def BinOp.isArith : BinOp → Bool
  | .add | .sub | .mul => true
  | _ => false

def BinOp.isEquality : BinOp → Bool
  | .eq | .neq => true
  | _ => false

/-- Module context: the byte-derived indices a lowering needs (Int carrier,
    host helpers, struct index per record type id) and the declarations the
    typing reads (record field types, callee signatures). -/
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

/-! ## Typing

One checker for every node. `n` is the resolver slot count: a `let` binder
must be below it and not yet bound (fresh), so the scratch local at `n` is
never a binder. `tail` is the position: `tailCall` is typed only in tail
position (its `return_call` leaves the function). -/

def builtinTy : Builtin → List Ty → Option Ty
  | .boolAnd, [.bool, .bool] => some .bool
  | .boolOr, [.bool, .bool] => some .bool
  | .boolNot, [.bool] => some .bool
  | _, _ => none

mutual
  def tyOf (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) (tail : Bool) :
      Expr → Option Ty
    | .literal (.int k) => if AverCert.PlanCheck.inI64Band k then some .int else none
    | .literal (.bool _) => some .bool
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
  def tysOf (M : MCtx) (n : Nat) (Γ : Nat → Option Ty) : List Expr → Option (List Ty)
    | [] => some []
    | e :: es =>
        match tyOf M n Γ false e, tysOf M n Γ es with
        | some t, some ts => some (t :: ts)
        | _, _ => none
end

/-! ## Source values -/

inductive SVal where
  | i (n : Int)
  | b (v : Bool)
  | record (tid : Nat) (fields : List SVal)
deriving Repr

mutual
  /-- A source value inhabits a type, over the record declarations `R`. -/
  def HasTy (R : Nat → Option (List Ty)) : SVal → Ty → Prop
    | .i _, .int => True
    | .b _, .bool => True
    | .record tid fs, .record tid' => tid = tid' ∧ ∃ fts, R tid = some fts ∧ HasTyL R fs fts
    | _, _ => False
  def HasTyL (R : Nat → Option (List Ty)) : List SVal → List Ty → Prop
    | [], [] => True
    | v :: vs, t :: ts => HasTy R v t ∧ HasTyL R vs ts
    | _, _ => False
end

/-! ## Source semantics

`F f` is the meaning of callee `f` (already specialised to a fuel level);
nothing here knows any function body. Evaluation is strict and pure. -/

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

def builtinEval : Builtin → List SVal → Option SVal
  | .boolAnd, [.b x, .b y] => some (.b (x && y))
  | .boolOr, [.b x, .b y] => some (.b (x || y))
  | .boolNot, [.b x] => some (.b (!x))
  | _, _ => none

mutual
  def eval (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) :
      Expr → Option SVal
    | .literal (.int k) => some (.i k)
    | .literal (.bool v) => some (.b v)
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
  def evalArgs (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) :
      List Expr → Option (List SVal)
    | [] => some []
    | e :: es =>
        match eval F env e, evalArgs F env es with
        | some v, some vs => some (v :: vs)
        | _, _ => none
end

def argsEnv (vs : List SVal) : Nat → Option SVal := fun i => vs[i]?

def paramsΓ (ts : List Ty) : Nat → Option Ty := fun i => ts[i]?

/-- The typing and slot-layout checks one plan must pass: parameters sit
    below the slot count, the scratch local at `nslots` is declared, and the
    body has the declared result type in tail position. -/
def planTyped (M : MCtx) (p : FnPlan) : Bool :=
  decide (p.sig.params.length ≤ p.nslots) &&
    decide (p.nslots < p.sig.params.length + p.locals.length) &&
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
