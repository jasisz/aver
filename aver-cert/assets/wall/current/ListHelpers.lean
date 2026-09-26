/- ListHelpers — the `List<T>` runtime helpers, pinned and proved.

   A `List.len` / `reverse` / `concat` / `take` / `drop` / `contains` call
   is a call of a per-instantiation helper the compiler emits
   (`src/codegen/wasm_gc/lists.rs`), and a `take` / `drop` count first goes
   through `__aint_to_i64_sat` (`builtins/wat/to_i64_sat.wat`). Each helper
   is a `loop` over the cons cells.

   This file gives each helper its TEMPLATE: the instruction tree the
   emitter writes, as a function of the declared indices (the cons struct,
   the element type, the reverse helper, the equality helper, the Int
   carrier). The acceptance pins every declared helper's code entry to the
   template's bytes (`hBodyBytes`), exactly as `ArithTemplateDerisk` pins the
   Int helpers.

   Unlike the Int helpers, nothing about a List helper is a runtime
   contract. The template also RUNS: `hRun` is the audited interpreter's
   structured-control extension (`block`, `loop`, `br`, `br_if`, `if`
   without `else`, `if (result i64)`, `return`, and `i64.add`, which wraps
   modulo 2^64), and it runs every other instruction with `wRunF` itself.
   A helper's meaning in an obligation's host table IS its template run by
   `hRun` (`lenSem`, `revSem`, ...), and the theorems at the end of this file
   prove what those runs compute, for example
   `revSem M t [wList L ws] = some (wList L ws.reverse)`. So the host table
   and the byte pin speak about the same instruction tree, and what a helper
   computes is a theorem, not an assumption.

   The run is fuel-bounded by the length of the list argument: a helper run
   over a list of `2 ^ 63` or more cons cells is not modelled (it yields
   `none`, which only makes an obligation vacuous on that input). No such
   list fits in a 64-bit address space, and it is the bound under which the
   `i64` counters of `len`, `take` and `drop` never wrap.

   `contains` compares with the element type's equality: `__aint_eq` (Int)
   and `__wasmgc_string_eq` (String) by their named contracts, `i32.eq`
   (Bool) directly. -/
import GrammarLower

namespace AverCert.ListHelpers
open CertPrelude AverCert.Grammar

/-! ## The structured-control interpreter -/

/-- A helper instruction: one straight-line instruction (`b`, run by the
    audited `wRunF`), or structured control. Every `block`, `loop` and
    `ifThen` has the empty block type. -/
inductive HI where
  | b (x : BI)
  | i64Add
  | block (body : List HI)
  | loop (body : List HI)
  | br (depth : Nat)
  | brIf (depth : Nat)
  | ifThen (body : List HI)
  | ifElseI64 (thenB elseB : List HI)
  | ret

/-- The outcome of a helper instruction sequence: it falls through, it
    branches to the label `depth` levels out (with the locals and the stack
    at the branch), or it returns. -/
inductive HOut where
  | ok (locals stack : List WVal)
  | br (depth : Nat) (locals stack : List WVal)
  | ret (v : WVal)

/-- `i64.add`: two's-complement addition modulo `2 ^ 64`, read back signed. -/
def wrapI64 (x : Int) : Int :=
  (x + 9223372036854775808) % 18446744073709551616 - 9223372036854775808

/-- One straight-line instruction, run by the audited interpreter. A helper
    calls only the functions `host` names. -/
def step1 (host : HostTbl) (x : BI) (l st : List WVal) : Option Out :=
  wRunF host (fun _ => none) (fun _ _ => none) [eraseI x] l st

/-- The helper interpreter. Every instruction costs one unit of fuel along
    its chain, and a `loop` costs one per iteration; at fuel `0` nothing
    runs. A `br` to a `block` continues after it with the block's entry
    stack, a `br` to a `loop` runs the loop again with its entry stack; a
    branch out of an `if (result i64)` is not modelled (`none`). -/
def hRun (host : HostTbl) : Nat → List HI → List WVal → List WVal → Option HOut
  | 0, _, _, _ => none
  | _ + 1, [], l, st => some (.ok l st)
  | k + 1, .b x :: rest, l, st =>
      match step1 host x l st with
      | some (.ok l' st') => hRun host k rest l' st'
      | some (.ret v) => some (.ret v)
      | none => none
  | k + 1, .i64Add :: rest, l, st =>
      match st with
      | .i64v y :: .i64v x :: st' => hRun host k rest l (.i64v (wrapI64 (x + y)) :: st')
      | _ => none
  | k + 1, .block body :: rest, l, st =>
      match hRun host k body l st with
      | some (.ok l' st') => hRun host k rest l' st'
      | some (.br 0 l' _) => hRun host k rest l' st
      | some (.br (d + 1) l' st') => some (.br d l' st')
      | some (.ret v) => some (.ret v)
      | none => none
  | k + 1, .loop body :: rest, l, st =>
      match hRun host k body l st with
      | some (.ok l' st') => hRun host k rest l' st'
      | some (.br 0 l' _) => hRun host k (.loop body :: rest) l' st
      | some (.br (d + 1) l' st') => some (.br d l' st')
      | some (.ret v) => some (.ret v)
      | none => none
  | _ + 1, .br d :: _, l, st => some (.br d l st)
  | k + 1, .brIf d :: rest, l, st =>
      match st with
      | .i32v c :: st' => if c = 0 then hRun host k rest l st' else some (.br d l st')
      | _ => none
  | k + 1, .ifThen body :: rest, l, st =>
      match st with
      | .i32v c :: st' =>
          if c = 0 then hRun host k rest l st'
          else
            match hRun host k body l st' with
            | some (.ok l' st'') => hRun host k rest l' st''
            | some (.br 0 l' _) => hRun host k rest l' st'
            | some (.br (d + 1) l' st'') => some (.br d l' st'')
            | some (.ret v) => some (.ret v)
            | none => none
      | _ => none
  | k + 1, .ifElseI64 tB eB :: rest, l, st =>
      match st with
      | .i32v c :: st' =>
          match hRun host k (if c = 0 then eB else tB) l st' with
          | some (.ok l' st'') => hRun host k rest l' st''
          | some (.ret v) => some (.ret v)
          | _ => none
      | _ => none
  | _ + 1, .ret :: _, _, st =>
      match st with
      | v :: _ => some (.ret v)
      | [] => none

/-- A declared local of a helper: `i64`, `i32`, a nullable reference, or the
    value type of a source type (the element local of `reverse`). -/
inductive LTy where
  | i64
  | i32
  | ref (ht : Nat)
  | val (t : Ty)

/-- The default value wasm gives a declared local. -/
def LTy.dflt : LTy → WVal
  | .i64 => .i64v 0
  | .i32 => .i32v 0
  | .ref _ => .null
  | .val .bool => .i32v 0
  | .val .float => .f64v 0
  | .val _ => .null

def LTy.bytes (M : MCtx) : LTy → Option (List Nat)
  | .i64 => some [0x7e]
  | .i32 => some [0x7f]
  | .ref ht => (s33HeapIdx ht).map ([0x63] ++ ·)
  | .val t => valTy M t

/-- A helper's code: its parameter count, its declared locals and its body. -/
structure HCode where
  params : Nat
  locals : List LTy
  body : List HI

/-- A call of a helper: the arguments, then the declared locals at their
    defaults; the result is the one value left at the end or returned. -/
def hCall (host : HostTbl) (c : HCode) (fuel : Nat) (args : List WVal) : Option WVal :=
  if args.length = c.params then
    match hRun host fuel c.body (args ++ c.locals.map LTy.dflt) [] with
    | some (.ok _ [v]) => some v
    | some (.ret v) => some v
    | _ => none
  else none

/-! ## Bytes -/

mutual
  def encH (M : MCtx) : HI → Option (List Nat)
    | .b x => encBI M x
    | .i64Add => some [0x7c]
    | .block body => (encHL M body).map fun bs => [0x02, 0x40] ++ bs ++ [0x0b]
    | .loop body => (encHL M body).map fun bs => [0x03, 0x40] ++ bs ++ [0x0b]
    | .br d => (uleb32 d).map ([0x0c] ++ ·)
    | .brIf d => (uleb32 d).map ([0x0d] ++ ·)
    | .ifThen body => (encHL M body).map fun bs => [0x04, 0x40] ++ bs ++ [0x0b]
    | .ifElseI64 tB eB =>
        match encHL M tB, encHL M eB with
        | some a, some b => some ([0x04, 0x7e] ++ a ++ [0x05] ++ b ++ [0x0b])
        | _, _ => none
    | .ret => some [0x0f]
  def encHL (M : MCtx) : List HI → Option (List Nat)
    | [] => some []
    | x :: xs =>
        match encH M x, encHL M xs with
        | some a, some b => some (a ++ b)
        | _, _ => none
end

/-- One `(1, type)` group per declared local, as the emitter declares them. -/
def localsBytes (M : MCtx) : List LTy → Option (List Nat)
  | [] => some []
  | t :: ts =>
      match t.bytes M, localsBytes M ts with
      | some a, some b => some ([0x01] ++ a ++ b)
      | _, _ => none

/-- A helper's code entry without its size prefix: the locals vector, the
    body and `end` (what `AcceptedArtifact.bodyBytesAtFuncIndex` reads). -/
def hBodyBytes (M : MCtx) (c : HCode) : Option (List Nat) :=
  match uleb32 c.locals.length, localsBytes M c.locals, encHL M c.body with
  | some n, some g, some b => some (n ++ g ++ b ++ [0x0b])
  | _, _, _ => none

/-! ## The templates -/

section Templates
variable (L : Nat)

def lg (i : Nat) : HI := .b (.op (.localGet i))
def ls (i : Nat) : HI := .b (.op (.localSet i))
def isNull : HI := .b (.op .refIsNull)
def hd : HI := .b (.op (.structGet L 0))
def tl : HI := .b (.op (.structGet L 1))
def cons : HI := .b (.op (.structNew L 2))
def i64c (k : Int) : HI := .b (.op (.i64Const k))

/-- `len` (`emit_list_len`): count the cells in an `i64`. -/
def lenBody : List HI :=
  [lg 1, isNull, .brIf 1, lg 2, i64c 1, .i64Add, ls 2, lg 1, tl L, ls 1, .br 0]

def lenCode : HCode :=
  { params := 1, locals := [.ref L, .i64],
    body := [lg 0, ls 1, i64c 0, ls 2, .block [.loop (lenBody L)], lg 2] }

/-- `reverse` (`emit_list_reverse`): cons every head onto an accumulator. -/
def revBody : List HI :=
  [lg 1, isNull, .brIf 1, lg 1, hd L, ls 3, lg 3, lg 2, cons L, ls 2, lg 1, tl L, ls 1, .br 0]

def revCode (t : Ty) : HCode :=
  { params := 1, locals := [.ref L, .ref L, .val t],
    body := [lg 0, ls 1, .b (.nullOf L), ls 2, .block [.loop (revBody L)], lg 2] }

/-- `concat` (`emit_list_concat`): reverse the first list, then cons its
    cells onto the second. -/
def catBody : List HI :=
  [lg 2, isNull, .brIf 1, lg 2, hd L, lg 3, cons L, ls 3, lg 2, tl L, ls 2, .br 0]

def catCode (R : Nat) : HCode :=
  { params := 2, locals := [.ref L, .ref L],
    body := [lg 0, .b (.op (.call R)), ls 2, lg 1, ls 3, .block [.loop (catBody L)], lg 3] }

/-- `take` (`emit_list_take`): cons at most `n` heads onto an accumulator,
    then reverse it. -/
def takeBody : List HI :=
  [lg 4, lg 1, .b (.op .i64GeS), .brIf 1, lg 2, isNull, .brIf 1,
   lg 2, hd L, lg 3, cons L, ls 3, lg 4, i64c 1, .i64Add, ls 4, lg 2, tl L, ls 2, .br 0]

def takeCode (R : Nat) : HCode :=
  { params := 2, locals := [.ref L, .ref L, .i64],
    body := [lg 0, ls 2, .b (.nullOf L), ls 3, i64c 0, ls 4, .block [.loop (takeBody L)],
      lg 3, .b (.op (.call R))] }

/-- `drop` (`emit_list_drop`): step past at most `n` cells. -/
def dropBody : List HI :=
  [lg 3, lg 1, .b (.op .i64GeS), .brIf 1, lg 2, isNull, .brIf 1,
   lg 2, tl L, ls 2, lg 3, i64c 1, .i64Add, ls 3, .br 0]

def dropCode : HCode :=
  { params := 2, locals := [.ref L, .i64],
    body := [lg 0, ls 2, i64c 0, ls 3, .block [.loop (dropBody L)], lg 2] }

/-- `contains` (`emit_list_contains`): compare each head with the needle by
    `eqX`, and return `1` at the first equal one. -/
def hasBody (eqX : BI) : List HI :=
  [lg 2, isNull, .brIf 1, lg 2, hd L, lg 1, .b eqX, .ifThen [.b (.op (.i32Const 1)), .ret],
   lg 2, tl L, ls 2, .br 0]

def hasCode (eqX : BI) : HCode :=
  { params := 2, locals := [.ref L],
    body := [lg 0, ls 2, .block [.loop (hasBody L eqX)], .b (.op (.i32Const 0))] }

end Templates

/-- `__aint_to_i64_sat` (`to_i64_sat.wat`) over the carrier struct `C`: a
    Small carrier's `i64`, else `i64::MAX` or `i64::MIN` by the sign. -/
def satCode (C : Nat) : HCode :=
  { params := 1, locals := [],
    body := [lg 0, .b (.op (.structGet C 1)), isNull,
      .ifElseI64 [lg 0, .b (.op (.structGet C 0))]
        [lg 0, .b (.op (.structGet C 2)), .b (.op (.i32Const 0)), .b (.op .i32GtS),
         .ifElseI64 [i64c 9223372036854775807] [i64c (-9223372036854775808)]]] }

/-- The equality instruction `contains` uses for element type `t`. -/
def hasEq (M : MCtx) : Ty → Option BI
  | .int => some (.op (.call M.eq))
  | .string => some (.op (.call M.streq))
  | .bool => some (.op .i32Eq)
  | _ => none

/-- The template of the `List<t>` helper of role `r`: `concat` and `take`
    call the reverse helper the type table declares for `t`. -/
def helperCode (M : MCtx) (r : ListRole) (t : Ty) : Option HCode :=
  let L := M.listStruct t
  match r with
  | .len => some (lenCode L)
  | .reverse => some (revCode L t)
  | .concat => (M.listHelper .reverse t).map (catCode L)
  | .take => (M.listHelper .reverse t).map (takeCode L)
  | .drop => some (dropCode L)
  | .contains => (hasEq M t).map (hasCode L)

/-! ## Meaning: each helper is its template, run -/

/-- The number of cons cells a value leads with. -/
def listDepth : WVal → Nat
  | .structv _ [_, tl] => listDepth tl + 1
  | _ => 0

/-- The run's fuel: enough for every template over a list of `listDepth`
    cells, and none at all from `2 ^ 63` cells on. -/
def hFuel : List WVal → Nat
  | w :: _ => if listDepth w < 9223372036854775808 then listDepth w + 64 else 0
  | [] => 0

def noHost : HostTbl := fun _ => none

/-- A host table naming one function. -/
def oneHost (f a : Nat) (g : List WVal → Option WVal) : HostTbl :=
  fun x => if x = f then some (a, g) else none

def lenSem (L : Nat) (args : List WVal) : Option WVal :=
  hCall noHost (lenCode L) (hFuel args) args

def revSem (L : Nat) (t : Ty) (args : List WVal) : Option WVal :=
  hCall noHost (revCode L t) (hFuel args) args

def catSem (L R : Nat) (t : Ty) (args : List WVal) : Option WVal :=
  hCall (oneHost R 1 (revSem L t)) (catCode L R) (hFuel args) args

def takeSem (L R : Nat) (t : Ty) (args : List WVal) : Option WVal :=
  hCall (oneHost R 1 (revSem L t)) (takeCode L R) (hFuel args) args

def dropSem (L : Nat) (args : List WVal) : Option WVal :=
  hCall noHost (dropCode L) (hFuel args) args

def hasSem (L : Nat) (eqX : BI) (host : HostTbl) (args : List WVal) : Option WVal :=
  hCall host (hasCode L eqX) (hFuel args) args

/-- `__aint_to_i64_sat` is straight-line: its fuel is fixed. -/
def satSem (C : Nat) (args : List WVal) : Option WVal :=
  hCall noHost (satCode C) 16 args

/-- The meaning of the `List<t>` helper of role `r`: its template, run.
    `contains` calls the Int or String equality helper, whose meanings are
    `eqI` / `eqS` (the obligation's contract functions). -/
def helperSem (M : MCtx) (eqI eqS : List WVal → Option WVal) (r : ListRole) (t : Ty) :
    List WVal → Option WVal :=
  let L := M.listStruct t
  match r with
  | .len => lenSem L
  | .reverse => revSem L t
  | .concat =>
      match M.listHelper .reverse t with
      | some R => catSem L R t
      | none => fun _ => none
  | .take =>
      match M.listHelper .reverse t with
      | some R => takeSem L R t
      | none => fun _ => none
  | .drop => dropSem L
  | .contains =>
      match t with
      | .int => hasSem L (.op (.call M.eq)) (oneHost M.eq 2 eqI)
      | .string => hasSem L (.op (.call M.streq)) (oneHost M.streq 2 eqS)
      | .bool => hasSem L (.op .i32Eq) noHost
      | _ => fun _ => none

/-- The contract helpers a List helper's template calls: `contains` over Int
    calls `__aint_eq`, over String `__wasmgc_string_eq`. -/
def innerCalls (M : MCtx) : ListRole → Ty → List Nat
  | .contains, .int => [M.eq]
  | .contains, .string => [M.streq]
  | _, _ => []


/-! ## What the helpers compute -/

/-- A list of values as cons cells of the struct `L`. -/
def wList (L : Nat) : List WVal → WVal
  | [] => .null
  | x :: xs => .structv L [x, wList L xs]

/-- The saturating `Int` → `i64` conversion. -/
def satI64 (n : Int) : Int :=
  if n < -9223372036854775808 then -9223372036854775808
  else if 9223372036854775807 < n then 9223372036854775807 else n

theorem listDepth_wList (L : Nat) : ∀ xs, listDepth (wList L xs) = xs.length
  | [] => rfl
  | _ :: xs => by simp [wList, listDepth, listDepth_wList L xs]

theorem hFuel_wList (L : Nat) (xs : List WVal) (rest : List WVal) :
    hFuel (wList L xs :: rest) =
      if xs.length < 9223372036854775808 then xs.length + 64 else 0 := by
  simp [hFuel, listDepth_wList]

/-! ### Control steps

Each equation below is one case of `hRun`. The proofs run a template with
these alone (never `hRun` itself), so a `loop` is unfolded only where a proof
asks for it. -/

section Steps
variable (host : HostTbl)

@[simp] theorem hRun_nil (k : Nat) (l st : List WVal) :
    hRun host (k + 1) [] l st = some (.ok l st) := by
  simp only [hRun]

@[simp] theorem hRun_zero (is : List HI) (l st : List WVal) : hRun host 0 is l st = none := by
  cases is <;> simp only [hRun]

theorem hRun_b (k : Nat) (x : BI) (rest : List HI) (l st : List WVal) :
    hRun host (k + 1) (.b x :: rest) l st =
      match step1 host x l st with
      | some (.ok l' st') => hRun host k rest l' st'
      | some (.ret v) => some (.ret v)
      | none => none := by
  simp only [hRun]

theorem hRun_i64Add (k : Nat) (rest : List HI) (l : List WVal) (x y : Int) (st : List WVal) :
    hRun host (k + 1) (.i64Add :: rest) l (.i64v y :: .i64v x :: st) =
      hRun host k rest l (.i64v (wrapI64 (x + y)) :: st) := by
  simp only [hRun]

theorem hRun_block (k : Nat) (body rest : List HI) (l st : List WVal) :
    hRun host (k + 1) (.block body :: rest) l st =
      match hRun host k body l st with
      | some (.ok l' st') => hRun host k rest l' st'
      | some (.br 0 l' _) => hRun host k rest l' st
      | some (.br (d + 1) l' st') => some (.br d l' st')
      | some (.ret v) => some (.ret v)
      | none => none := by
  simp only [hRun]

theorem hRun_loop (k : Nat) (body rest : List HI) (l st : List WVal) :
    hRun host (k + 1) (.loop body :: rest) l st =
      match hRun host k body l st with
      | some (.ok l' st') => hRun host k rest l' st'
      | some (.br 0 l' _) => hRun host k (.loop body :: rest) l' st
      | some (.br (d + 1) l' st') => some (.br d l' st')
      | some (.ret v) => some (.ret v)
      | none => none := by
  simp only [hRun]

theorem hRun_br (k d : Nat) (rest : List HI) (l st : List WVal) :
    hRun host (k + 1) (.br d :: rest) l st = some (.br d l st) := by
  simp only [hRun]

theorem hRun_brIf (k d : Nat) (rest : List HI) (l : List WVal) (c : Int) (st : List WVal) :
    hRun host (k + 1) (.brIf d :: rest) l (.i32v c :: st) =
      if c = 0 then hRun host k rest l st else some (.br d l st) := by
  simp only [hRun]

theorem hRun_ifThen (k : Nat) (body rest : List HI) (l : List WVal) (c : Int) (st : List WVal) :
    hRun host (k + 1) (.ifThen body :: rest) l (.i32v c :: st) =
      if c = 0 then hRun host k rest l st
      else
        match hRun host k body l st with
        | some (.ok l' st'') => hRun host k rest l' st''
        | some (.br 0 l' _) => hRun host k rest l' st
        | some (.br (d + 1) l' st'') => some (.br d l' st'')
        | some (.ret v) => some (.ret v)
        | none => none := by
  simp only [hRun]

theorem hRun_ifElse (k : Nat) (tB eB rest : List HI) (l : List WVal) (c : Int) (st : List WVal) :
    hRun host (k + 1) (.ifElseI64 tB eB :: rest) l (.i32v c :: st) =
      match hRun host k (if c = 0 then eB else tB) l st with
      | some (.ok l' st'') => hRun host k rest l' st''
      | some (.ret v) => some (.ret v)
      | _ => none := by
  simp only [hRun]

theorem hRun_ret (k : Nat) (rest : List HI) (l : List WVal) (v : WVal) (st : List WVal) :
    hRun host (k + 1) (.ret :: rest) l (v :: st) = some (.ret v) := by
  simp only [hRun]

end Steps

theorem popArgs_two' (a b : WVal) (st : List WVal) :
    popArgs 2 (b :: a :: st) = some ([a, b], st) := by
  simp [popArgs]

theorem popArgs_one' (a : WVal) (st : List WVal) : popArgs 1 (a :: st) = some ([a], st) := by
  simp [popArgs]

theorem wrapI64_id (x : Int) (h0 : 0 ≤ x) (h1 : x < 9223372036854775808) : wrapI64 x = x := by
  unfold wrapI64; omega

/-- A fuel of at least `m` is `k + m` for some `k`. -/
theorem fuel_split {n m : Nat} (h : m ≤ n) : ∃ k, n = k + m := ⟨n - m, by omega⟩

/-! ### `len` -/

theorem lenBody_cons (host : HostTbl) (L n : Nat) (hn : 11 ≤ n) (a x t : WVal) (m : Int)
    (st : List WVal) :
    hRun host n (lenBody L) [a, .structv L [x, t], .i64v m] st =
      some (.br 0 [a, t, .i64v (wrapI64 (m + 1))] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [lenBody, hRun_b, hRun_brIf, hRun_i64Add, hRun_br, step1, eraseI, wRunF, lg, ls, isNull,
    tl, i64c, b32]

theorem lenBody_nil (host : HostTbl) (L n : Nat) (hn : 3 ≤ n) (a c : WVal) (st : List WVal) :
    hRun host n (lenBody L) [a, .null, c] st = some (.br 1 [a, .null, c] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [lenBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32]

theorem lenLoop (host : HostTbl) (L : Nat) : ∀ (xs : List WVal) (n : Nat) (a : WVal) (m : Int)
    (st : List WVal), xs.length + 12 ≤ n → 0 ≤ m → m + xs.length < 9223372036854775808 →
    hRun host n [.loop (lenBody L)] [a, wList L xs, .i64v m] st =
      some (.br 0 [a, .null, .i64v (m + xs.length)] st)
  | [], n, a, m, st, hn, _, _ => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, lenBody_nil host L k (by simp at hn; omega)]
      simp
  | x :: xs, n, a, m, st, hn, h0, h1 => by
      simp only [List.length_cons] at hn h1
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, lenBody_cons host L k (by omega)]
      simp only
      rw [wrapI64_id _ (by omega) (by omega),
        lenLoop host L xs k a (m + 1) st (by omega) (by omega) (by omega)]
      rw [show m + 1 + (xs.length : Int) = m + ((xs.length + 1 : Nat) : Int) by omega]
      rfl

theorem lenLoop0 (host : HostTbl) (L : Nat) (xs : List WVal) (n : Nat) (a v : WVal)
    (hn : xs.length + 12 ≤ n) (hl : xs.length < 9223372036854775808) (hv : v = .i64v 0) :
    hRun host n [.loop (lenBody L)] [a, wList L xs, v] [] =
      some (.br 0 [a, .null, .i64v xs.length] []) := by
  subst hv
  rw [lenLoop host L xs n a 0 [] hn (by omega) (by omega)]
  simp

theorem lenSem_spec (L : Nat) (xs : List WVal) (r : WVal)
    (h : lenSem L [wList L xs] = some r) :
    r = .i64v xs.length ∧ xs.length < 9223372036854775808 := by
  unfold lenSem hCall at h
  rw [hFuel_wList] at h
  by_cases hl : xs.length < 9223372036854775808
  · simp only [hl, ↓reduceIte] at h
    have h0 := lenLoop0 noHost L xs (xs.length + 59) (wList L xs) (.i64v 0) (by omega) hl rfl
    simp [lenCode, LTy.dflt, hRun_b, hRun_block, step1, eraseI, wRunF, lg, ls, i64c, h0] at h
    exact ⟨h.symm, hl⟩
  · simp [hl] at h

/-! ### `reverse` -/

theorem revBody_cons (host : HostTbl) (L n : Nat) (hn : 14 ≤ n) (a x t acc v : WVal)
    (st : List WVal) :
    hRun host n (revBody L) [a, .structv L [x, t], acc, v] st =
      some (.br 0 [a, t, .structv L [x, acc], x] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [revBody, hRun_b, hRun_brIf, hRun_br, step1, eraseI, wRunF, lg, ls, isNull, hd, tl, cons,
    b32, popArgs_two']

theorem revBody_nil (host : HostTbl) (L n : Nat) (hn : 3 ≤ n) (a acc v : WVal) (st : List WVal) :
    hRun host n (revBody L) [a, .null, acc, v] st = some (.br 1 [a, .null, acc, v] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [revBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32]

theorem revLoop (host : HostTbl) (L : Nat) : ∀ (xs accL : List WVal) (n : Nat) (a v : WVal)
    (st : List WVal), xs.length + 15 ≤ n →
    hRun host n [.loop (revBody L)] [a, wList L xs, wList L accL, v] st =
      some (.br 0 [a, .null, wList L (xs.reverse ++ accL), xs.foldl (fun _ y => y) v] st)
  | [], accL, n, a, v, st, hn => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, revBody_nil host L k (by simp at hn; omega)]
      simp
  | x :: xs, accL, n, a, v, st, hn => by
      simp only [List.length_cons] at hn
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, revBody_cons host L k (by omega)]
      simp only
      have ih := revLoop host L xs (x :: accL) k a x st (by omega)
      simp only [wList] at ih
      rw [ih]
      simp

theorem revLoop0 (host : HostTbl) (L : Nat) (xs : List WVal) (n : Nat) (a v : WVal)
    (hn : xs.length + 15 ≤ n) :
    ∃ v', hRun host n [.loop (revBody L)] [a, wList L xs, .null, v] [] =
      some (.br 0 [a, .null, wList L xs.reverse, v'] []) :=
  ⟨_, by simpa [wList] using revLoop host L xs [] n a v [] hn⟩

theorem revSem_eq (L : Nat) (t : Ty) (xs : List WVal) (h : xs.length < 9223372036854775808) :
    revSem L t [wList L xs] = some (wList L xs.reverse) := by
  unfold revSem hCall
  rw [hFuel_wList]
  obtain ⟨v', hv⟩ := revLoop0 noHost L xs (xs.length + 59) (wList L xs) (LTy.dflt (.val t))
    (by omega)
  simp [h, revCode, hRun_b, hRun_block, step1, eraseI, wRunF, lg, ls, hv]

theorem revSem_spec (L : Nat) (t : Ty) (xs : List WVal) (r : WVal)
    (h : revSem L t [wList L xs] = some r) : r = wList L xs.reverse := by
  by_cases hl : xs.length < 9223372036854775808
  · rw [revSem_eq L t xs hl] at h
    exact (Option.some.inj h).symm
  · unfold revSem hCall at h
    rw [hFuel_wList] at h
    simp [hl] at h

/-! ### `concat` -/

theorem catBody_cons (host : HostTbl) (L n : Nat) (hn : 12 ≤ n) (a b x t acc : WVal)
    (st : List WVal) :
    hRun host n (catBody L) [a, b, .structv L [x, t], acc] st =
      some (.br 0 [a, b, t, .structv L [x, acc]] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [catBody, hRun_b, hRun_brIf, hRun_br, step1, eraseI, wRunF, lg, ls, isNull, hd, tl, cons,
    b32, popArgs_two']

theorem catBody_nil (host : HostTbl) (L n : Nat) (hn : 3 ≤ n) (a b acc : WVal) (st : List WVal) :
    hRun host n (catBody L) [a, b, .null, acc] st = some (.br 1 [a, b, .null, acc] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [catBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32]

theorem catLoop (host : HostTbl) (L : Nat) : ∀ (xs accL : List WVal) (n : Nat) (a b : WVal)
    (st : List WVal), xs.length + 13 ≤ n →
    hRun host n [.loop (catBody L)] [a, b, wList L xs, wList L accL] st =
      some (.br 0 [a, b, .null, wList L (xs.reverse ++ accL)] st)
  | [], accL, n, a, b, st, hn => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, catBody_nil host L k (by simp at hn; omega)]
      simp
  | x :: xs, accL, n, a, b, st, hn => by
      simp only [List.length_cons] at hn
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, catBody_cons host L k (by omega)]
      simp only
      have ih := catLoop host L xs (x :: accL) k a b st (by omega)
      simp only [wList] at ih
      rw [ih]
      simp

theorem catSem_spec (L R : Nat) (t : Ty) (xs ys : List WVal) (r : WVal)
    (h : catSem L R t [wList L xs, wList L ys] = some r) : r = wList L (xs ++ ys) := by
  unfold catSem hCall at h
  rw [hFuel_wList] at h
  by_cases hl : xs.length < 9223372036854775808
  · have hr := revSem_eq L t xs hl
    have h0 := catLoop (oneHost R 1 (revSem L t)) L xs.reverse ys (xs.length + 58)
      (wList L xs) (wList L ys) [] (by simp only [List.length_reverse]; omega)
    simp [hl, catCode, LTy.dflt, hRun_b, hRun_block, step1, eraseI, wRunF, lg, ls, oneHost,
      popArgs_one', hr, h0] at h
    exact h.symm
  · simp [hl] at h

/-! ### `take` and `drop` -/

theorem take_min_length (xs : List WVal) (n : Nat) : xs.take (min n xs.length) = xs.take n := by
  rcases Nat.le_total n xs.length with h | h
  · rw [Nat.min_eq_left h]
  · rw [Nat.min_eq_right h, List.take_of_length_le (Nat.le_refl _), List.take_of_length_le h]

theorem drop_min_length (xs : List WVal) (n : Nat) : xs.drop (min n xs.length) = xs.drop n := by
  rcases Nat.le_total n xs.length with h | h
  · rw [Nat.min_eq_left h]
  · rw [Nat.min_eq_right h, List.drop_of_length_le (Nat.le_refl _), List.drop_of_length_le h]

theorem takeBody_stop (host : HostTbl) (L n : Nat) (hn : 4 ≤ n) (a cur acc : WVal) (c j : Int)
    (hj : c ≤ j) (st : List WVal) :
    hRun host n (takeBody L) [a, .i64v c, cur, acc, .i64v j] st =
      some (.br 1 [a, .i64v c, cur, acc, .i64v j] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [takeBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, b32, hj]

theorem takeBody_nil (host : HostTbl) (L n : Nat) (hn : 7 ≤ n) (a acc : WVal) (c j : Int)
    (hj : j < c) (st : List WVal) :
    hRun host n (takeBody L) [a, .i64v c, .null, acc, .i64v j] st =
      some (.br 1 [a, .i64v c, .null, acc, .i64v j] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have : ¬ (c ≤ j) := by omega
  simp [takeBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32, this]

theorem takeBody_cons (host : HostTbl) (L n : Nat) (hn : 20 ≤ n) (a x t acc : WVal) (c j : Int)
    (hj : j < c) (st : List WVal) :
    hRun host n (takeBody L) [a, .i64v c, .structv L [x, t], acc, .i64v j] st =
      some (.br 0 [a, .i64v c, t, .structv L [x, acc], .i64v (wrapI64 (j + 1))] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have : ¬ (c ≤ j) := by omega
  simp [takeBody, hRun_b, hRun_brIf, hRun_i64Add, hRun_br, step1, eraseI, wRunF, lg, ls, isNull,
    hd, tl, cons, i64c, b32, popArgs_two', this]

theorem takeLoop (host : HostTbl) (L : Nat) : ∀ (ys accL : List WVal) (n : Nat) (a : WVal)
    (c j : Int) (st : List WVal), ys.length + 21 ≤ n → 0 ≤ j →
    j + ys.length < 9223372036854775808 →
    hRun host n [.loop (takeBody L)] [a, .i64v c, wList L ys, wList L accL, .i64v j] st =
      some (.br 0 [a, .i64v c, wList L (ys.drop (min (c - j).toNat ys.length)),
        wList L ((ys.take (min (c - j).toNat ys.length)).reverse ++ accL),
        .i64v (j + (min (c - j).toNat ys.length : Nat))] st)
  | ys, accL, n, a, c, j, st, hn, h0, h1 => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      by_cases hj : c ≤ j
      · rw [hRun_loop, takeBody_stop host L k (by omega) a _ _ c j hj]
        have hm : (c - j).toNat = 0 := by omega
        simp [hm]
      · have hj' : j < c := by omega
        match ys with
        | [] =>
            rw [hRun_loop, wList, takeBody_nil host L k (by simp at hn; omega) a _ c j hj']
            simp [wList]
        | y :: ys' =>
            simp only [List.length_cons] at hn h1
            rw [hRun_loop, wList, takeBody_cons host L k (by omega) a y _ _ c j hj']
            simp only
            rw [wrapI64_id _ (by omega) (by omega)]
            have ih := takeLoop host L ys' (y :: accL) k a c (j + 1) st (by omega) (by omega)
              (by omega)
            simp only [wList] at ih
            rw [ih]
            have hm : min (c - j).toNat (ys'.length + 1) =
                min (c - (j + 1)).toNat ys'.length + 1 := by omega
            rw [List.length_cons, hm, List.drop_succ_cons, List.take_succ_cons, List.reverse_cons,
              List.append_assoc, List.singleton_append,
              show j + 1 + ((min (c - (j + 1)).toNat ys'.length : Nat) : Int) =
                j + ((min (c - (j + 1)).toNat ys'.length + 1 : Nat) : Int) by omega]

theorem takeSem_spec (L R : Nat) (t : Ty) (xs : List WVal) (c : Int) (r : WVal)
    (h : takeSem L R t [wList L xs, .i64v c] = some r) :
    r = wList L (xs.take c.toNat) ∧ xs.length < 9223372036854775808 := by
  unfold takeSem hCall at h
  rw [hFuel_wList] at h
  by_cases hl : xs.length < 9223372036854775808
  · have h0 := takeLoop (oneHost R 1 (revSem L t)) L xs [] (xs.length + 57) (wList L xs) c 0 []
      (by omega) (Int.le_refl 0) (by omega)
    have hlen : ((xs.take (min (c - 0).toNat xs.length)).reverse ++ []).length <
        9223372036854775808 := by
      simp; omega
    have hr := revSem_eq L t _ hlen
    simp only [List.append_nil, Int.sub_zero] at hr h0 hlen
    simp only [wList] at h0
    simp only [take_min_length] at hr
    simp [hl, takeCode, LTy.dflt, hRun_b, hRun_block, step1, eraseI, wRunF, lg, ls, i64c,
      oneHost, popArgs_one', h0, hr, take_min_length] at h
    exact ⟨by first | exact (Option.some.inj h).symm | exact h.symm, hl⟩
  · simp [hl] at h

theorem dropBody_stop (host : HostTbl) (L n : Nat) (hn : 4 ≤ n) (a cur : WVal) (c j : Int)
    (hj : c ≤ j) (st : List WVal) :
    hRun host n (dropBody L) [a, .i64v c, cur, .i64v j] st =
      some (.br 1 [a, .i64v c, cur, .i64v j] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [dropBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, b32, hj]

theorem dropBody_nil (host : HostTbl) (L n : Nat) (hn : 7 ≤ n) (a : WVal) (c j : Int)
    (hj : j < c) (st : List WVal) :
    hRun host n (dropBody L) [a, .i64v c, .null, .i64v j] st =
      some (.br 1 [a, .i64v c, .null, .i64v j] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have : ¬ (c ≤ j) := by omega
  simp [dropBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32, this]

theorem dropBody_cons (host : HostTbl) (L n : Nat) (hn : 15 ≤ n) (a x t : WVal) (c j : Int)
    (hj : j < c) (st : List WVal) :
    hRun host n (dropBody L) [a, .i64v c, .structv L [x, t], .i64v j] st =
      some (.br 0 [a, .i64v c, t, .i64v (wrapI64 (j + 1))] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  have : ¬ (c ≤ j) := by omega
  simp [dropBody, hRun_b, hRun_brIf, hRun_i64Add, hRun_br, step1, eraseI, wRunF, lg, ls, isNull,
    tl, i64c, b32, this]

theorem dropLoop (host : HostTbl) (L : Nat) : ∀ (ys : List WVal) (n : Nat) (a : WVal)
    (c j : Int) (st : List WVal), ys.length + 16 ≤ n → 0 ≤ j →
    j + ys.length < 9223372036854775808 →
    hRun host n [.loop (dropBody L)] [a, .i64v c, wList L ys, .i64v j] st =
      some (.br 0 [a, .i64v c, wList L (ys.drop (min (c - j).toNat ys.length)),
        .i64v (j + (min (c - j).toNat ys.length : Nat))] st)
  | ys, n, a, c, j, st, hn, h0, h1 => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      by_cases hj : c ≤ j
      · rw [hRun_loop, dropBody_stop host L k (by omega) a _ c j hj]
        have hm : (c - j).toNat = 0 := by omega
        simp [hm]
      · have hj' : j < c := by omega
        match ys with
        | [] =>
            rw [hRun_loop, wList, dropBody_nil host L k (by simp at hn; omega) a c j hj']
            simp [wList]
        | y :: ys' =>
            simp only [List.length_cons] at hn h1
            rw [hRun_loop, wList, dropBody_cons host L k (by omega) a y _ c j hj']
            simp only
            rw [wrapI64_id _ (by omega) (by omega),
              dropLoop host L ys' k a c (j + 1) st (by omega) (by omega) (by omega)]
            have hm : min (c - j).toNat (ys'.length + 1) =
                min (c - (j + 1)).toNat ys'.length + 1 := by omega
            rw [List.length_cons, hm, List.drop_succ_cons,
              show j + 1 + ((min (c - (j + 1)).toNat ys'.length : Nat) : Int) =
                j + ((min (c - (j + 1)).toNat ys'.length + 1 : Nat) : Int) by omega]

theorem dropSem_spec (L : Nat) (xs : List WVal) (c : Int) (r : WVal)
    (h : dropSem L [wList L xs, .i64v c] = some r) :
    r = wList L (xs.drop c.toNat) ∧ xs.length < 9223372036854775808 := by
  unfold dropSem hCall at h
  rw [hFuel_wList] at h
  by_cases hl : xs.length < 9223372036854775808
  · have h0 := dropLoop noHost L xs (xs.length + 59) (wList L xs) c 0 []
      (by omega) (Int.le_refl 0) (by omega)
    simp [hl, dropCode, LTy.dflt, hRun_b, hRun_block, step1, eraseI, wRunF, lg, ls, i64c, h0,
      drop_min_length] at h
    exact ⟨by first | exact (Option.some.inj h).symm | exact h.symm, hl⟩
  · simp [hl] at h

/-! ### `contains` -/

/-- A list of values paired with booleans, pointwise related. -/
def Rel2 (R : WVal → Bool → Prop) : List WVal → List Bool → Prop
  | [], [] => True
  | w :: ws, b :: bs => R w b ∧ Rel2 R ws bs
  | _, _ => False

/-- The equality instruction answers `b` for an element, or fails. -/
def EqAns (host : HostTbl) (eqX : BI) (x : WVal) (w : WVal) (b : Bool) : Prop :=
  ∀ l st, step1 host eqX l (x :: w :: st) = none ∨
    step1 host eqX l (x :: w :: st) = some (.ok l (b32 b :: st))

theorem hasBody_nil (host : HostTbl) (L n : Nat) (hn : 3 ≤ n) (eqX : BI) (a x : WVal)
    (st : List WVal) :
    hRun host n (hasBody L eqX) [a, x, .null] st = some (.br 1 [a, x, .null] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  simp [hasBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, b32]

theorem hasBody_cons (host : HostTbl) (L n : Nat) (hn : 12 ≤ n) (eqX : BI) (a x w t : WVal)
    (b : Bool) (st : List WVal) (he : EqAns host eqX x w b) (o : HOut)
    (h : hRun host n (hasBody L eqX) [a, x, .structv L [w, t]] st = some o) :
    (b = true ∧ o = .ret (.i32v 1)) ∨ (b = false ∧ o = .br 0 [a, x, t] st) := by
  obtain ⟨k, rfl⟩ := fuel_split hn
  rcases he [a, x, .structv L [w, t]] st with hq | hq <;> simp only [step1] at hq
  · simp [hasBody, hRun_b, hRun_brIf, step1, eraseI, wRunF, lg, isNull, hd, b32, hq] at h
  · cases b <;>
      simp [hasBody, hRun_b, hRun_brIf, hRun_ifThen, hRun_br, hRun_ret, step1, eraseI, wRunF,
        lg, ls, isNull, hd, tl, b32, hq] at h <;>
      simp [h]

theorem hasLoop (host : HostTbl) (L : Nat) (eqX : BI) (x : WVal) :
    ∀ (ws : List WVal) (bs : List Bool) (n : Nat) (a : WVal) (st : List WVal) (o : HOut),
    Rel2 (EqAns host eqX x) ws bs → ws.length + 13 ≤ n →
    hRun host n [.loop (hasBody L eqX)] [a, x, wList L ws] st = some o →
    (o = .ret (.i32v 1) ∧ bs.any id = true) ∨ (o = .br 0 [a, x, .null] st ∧ bs.any id = false)
  | [], [], n, a, st, o, _, hn, h => by
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList, hasBody_nil host L k (by simp at hn; omega)] at h
      simp at h
      simp [h]
  | w :: ws, b :: bs, n, a, st, o, hr, hn, h => by
      simp only [List.length_cons] at hn
      obtain ⟨k, rfl⟩ := fuel_split (show 1 ≤ n by omega)
      rw [hRun_loop, wList] at h
      cases hb : hRun host k (hasBody L eqX) [a, x, .structv L [w, wList L ws]] st with
      | none => simp [hb] at h
      | some o' =>
          rcases hasBody_cons host L k (by omega) eqX a x w (wList L ws) b st hr.1 o' hb
            with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
          · simp [hb] at h
            simp [h]
          · simp only [hb] at h
            rcases hasLoop host L eqX x ws bs k a st o hr.2 (by omega) h with h' | h'
            · left; simpa using h'
            · right; simpa using h'
  | [], _ :: _, _, _, _, _, hr, _, _ => by simp [Rel2] at hr
  | _ :: _, [], _, _, _, _, hr, _, _ => by simp [Rel2] at hr

theorem hasSem_spec (L : Nat) (eqX : BI) (host : HostTbl) (ws : List WVal) (x : WVal)
    (bs : List Bool) (hr : Rel2 (EqAns host eqX x) ws bs) (r : WVal)
    (h : hasSem L eqX host [wList L ws, x] = some r) : r = b32 (bs.any id) := by
  unfold hasSem hCall at h
  rw [hFuel_wList] at h
  by_cases hl : ws.length < 9223372036854775808
  · simp only [hl, ↓reduceIte, hasCode, LTy.dflt, List.map, List.length_cons, List.length_nil,
      List.cons_append, List.nil_append] at h
    rw [show ws.length + 64 = (ws.length + 61) + 1 + 1 + 1 by omega] at h
    simp only [hRun_b, step1, lg, ls, eraseI, wRunF, List.getElem?_cons_zero, List.set_cons_succ,
      List.set_cons_zero, reduceIte] at h
    simp only [hRun_block] at h
    cases hl2 : hRun host (ws.length + 61) [.loop (hasBody L eqX)]
        [wList L ws, x, wList L ws] [] with
    | none => simp [hl2] at h
    | some o =>
        rcases hasLoop host L eqX x ws bs _ (wList L ws) [] o hr (by omega) hl2
          with ⟨rfl, ha⟩ | ⟨rfl, ha⟩
        · simp [hl2] at h
          rw [← h, ha]; rfl
        · simp [hl2, hRun_b, step1, eraseI, wRunF] at h
          rw [← h, ha]; rfl
  · simp [hl] at h

/-! ### `__aint_to_i64_sat` -/

/-- What the saturated count keeps: a take / drop over fewer than `2 ^ 63`
    cells sees the same count. -/
def satOk (n c : Int) : Prop :=
  ∀ m : Nat, m < 9223372036854775808 → min c.toNat m = min n.toNat m

theorem satSem_spec {C : Nat} (S : AverCert.Schema.CarrierSpec C) (n : Int) (w r : WVal)
    (hw : AverCert.Schema.CanonRepr S n w) (h : satSem C [w] = some r) :
    ∃ c, r = .i64v c ∧ satOk n c := by
  obtain ⟨hrep, hcan⟩ := hw
  rcases S.car n w hrep with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
  · have hs := S.smallElim n s sg hrep
    subst hs
    simp [satSem, hCall, satCode, hRun_b, hRun_ifElse, step1, eraseI, wRunF, lg, isNull,
      b32] at h
    exact ⟨s, h.symm, fun _ _ => rfl⟩
  · obtain ⟨hsg, hne⟩ := S.bigElim n s lty les sg hrep
    obtain ⟨hband, hsg0⟩ := S.canonBig n s lty les sg hrep hcan
    by_cases hpos : 0 < sg
    · simp [satSem, hCall, satCode, hRun_b, hRun_ifElse, step1, eraseI, wRunF, lg, isNull, b32,
        i64c, hpos] at h
      refine ⟨_, h.symm, fun m hm => ?_⟩
      have : 9223372036854775808 ≤ n := by omega
      omega
    · have hneg : sg < 0 := by omega
      simp [satSem, hCall, satCode, hRun_b, hRun_ifElse, step1, eraseI, wRunF, lg, isNull, b32,
        i64c, hpos] at h
      refine ⟨_, h.symm, fun m hm => ?_⟩
      have : n < 0 := hsg.mp hneg
      omega

/-! ### The typing of a List builtin -/

theorem helper_some {M : MCtx} {r : ListRole} {t : Ty} (h : (M.listHelper r t).isSome = true) :
    ∃ f, M.listHelper r t = some f := Option.isSome_iff_exists.mp h

theorem builtinTy_listLen {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .listLen ts = some T) :
    ∃ t f, ts = [.list t] ∧ T = .int ∧ M.listHelper .len t = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, rest⟩⟩
  · simp [builtinTy] at h
  · cases t0 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := helper_some hs
      exact ⟨t, f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_listReverse {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .listReverse ts = some T) :
    ∃ t f, ts = [.list t] ∧ T = .list t ∧ M.listHelper .reverse t = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, rest⟩⟩
  · simp [builtinTy] at h
  · cases t0 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := helper_some hs
      exact ⟨t, f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_listConcat {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .listConcat ts = some T) :
    ∃ t f, ts = [.list t, .list t] ∧ T = .list t ∧ M.listHelper .concat t = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> cases t1 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t t'
    split at h
    · rename_i hs
      obtain ⟨rfl, hs⟩ := hs
      obtain ⟨f, hf⟩ := helper_some hs
      exact ⟨t, f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_listTake {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .listTake ts = some T) :
    ∃ t f, ts = [.list t, .int] ∧ T = .list t ∧ M.listHelper .take t = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> cases t1 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := helper_some hs
      exact ⟨t, f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_listDrop {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .listDrop ts = some T) :
    ∃ t f, ts = [.list t, .int] ∧ T = .list t ∧ M.listHelper .drop t = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> cases t1 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t
    split at h
    · rename_i hs
      obtain ⟨f, hf⟩ := helper_some hs
      exact ⟨t, f, rfl, (Option.some.inj h).symm, hf⟩
    · cases h
  · simp [builtinTy] at h

theorem builtinTy_listContains {M : MCtx} {ts : List Ty} {T : Ty}
    (h : builtinTy M .listContains ts = some T) :
    ∃ t f, ts = [.list t, t] ∧ T = .bool ∧ t.containsEq = true ∧
      M.listHelper .contains t = some f := by
  rcases ts with _ | ⟨t0, _ | ⟨t1, _ | ⟨t2, rest⟩⟩⟩
  · simp [builtinTy] at h
  · simp [builtinTy] at h
  · cases t0 <;> simp only [builtinTy] at h <;> try cases h
    rename_i t
    split at h
    · rename_i hs
      obtain ⟨rfl, hc, hs⟩ := hs
      obtain ⟨f, hf⟩ := helper_some hs
      exact ⟨t, f, rfl, (Option.some.inj h).symm, hc, hf⟩
    · cases h
  · simp [builtinTy] at h

end AverCert.ListHelpers
