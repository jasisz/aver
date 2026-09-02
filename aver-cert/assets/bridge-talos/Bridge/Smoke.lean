import Bridge.EnvOfClaim
import Bridge.Config

/-!
# Smoke test (not part of the proof)

The three k5 compute bodies `Domain_Rational_plus` / `isNonNeg` / `lessThan`,
copied from the certificate package's `Module.lean` (the wall's `WCode`
tables, function indices 5/13/12, the code the wall lowers from
`Plans.lean`), run on the wall side through `wFuncN` with small-int host
faces, and on the Talos side through `translate` + `runSteps` with a heap host
environment. Both results are decoded to integers/Booleans and compared.
`lake build Bridge.Smoke` prints the comparison.
-/

namespace Bridge.Smoke
open Wasm Wasm.SmallStep CertPrelude Bridge

/-! ## The bodies (verbatim from `scratchpad/k5b/cert/Module.lean`) -/

def plusBody : List WInstr :=
  [.localGet 0, .structGet 0 0, .localGet 1, .structGet 0 1, .call 26, .localGet 1, .structGet 0 0,
   .localGet 0, .structGet 0 1, .call 26, .call 24, .localGet 0, .structGet 0 1, .localGet 1,
   .structGet 0 1, .call 26, .structNew 0 2]

def lessThanBody : List WInstr :=
  [.localGet 0, .structGet 0 0, .localGet 0, .structGet 0 1, .call 26, .localGet 1, .structGet 0 1,
   .localGet 1, .structGet 0 1, .call 26, .call 26, .localGet 1, .structGet 0 0, .localGet 1,
   .structGet 0 1, .call 26, .localGet 0, .structGet 0 1, .localGet 0, .structGet 0 1, .call 26,
   .call 26, .call 30, .i32Const (0), .i32LtS]

def isNonNegBody : List WInstr :=
  [.localGet 0, .structGet 0 0, .localGet 0, .structGet 0 1, .call 26, .localSet 1, .localGet 1,
   .structGet 3 1, .refIsNull,
   .ifElse [.localGet 1, .structGet 3 0, .i64Const (0), .i64GeS]
           [.localGet 1, .structGet 3 2, .i32Const (0), .i32GtS]]

def codeTbl : CodeTbl := fun fn =>
  if fn = 5 then some ⟨2, 1, plusBody⟩
  else if fn = 12 then some ⟨2, 1, lessThanBody⟩
  else if fn = 13 then some ⟨1, 1, isNonNegBody⟩
  else none

/-! ## Wall side: small-int faces of the host slots -/

def cmpW (a b : Int) : Int := if a < b then -1 else if a = b then 0 else 1
def eqW (a b : Int) : Int := if a = b then 1 else 0

def carrierW (k : Int) : WVal := .structv 3 [.i64v k, .null, .i32v 0]
def fractionW (a b : Int) : WVal := .structv 0 [carrierW a, carrierW b]

def smallOf : WVal → Option Int
  | .structv 3 [.i64v s, .null, .i32v _] => some s
  | _ => none

def wBin (f : Int → Int → Int) : List WVal → Option WVal
  | [a, b] => do let x ← smallOf a; let y ← smallOf b; some (carrierW (f x y))
  | _ => none

def wCmp (f : Int → Int → Int) : List WVal → Option WVal
  | [a, b] => do let x ← smallOf a; let y ← smallOf b; some (.i32v (f x y))
  | _ => none

def wallHost : HostTbl := fun f =>
  if f = 23 then some (1, fun | [.i64v k] => some (carrierW k) | _ => none)
  else if f = 24 then some (2, wBin (· + ·))
  else if f = 26 then some (2, wBin (· * ·))
  else if f = 25 then some (2, wBin (· - ·))
  else if f = 30 then some (2, wCmp cmpW)
  else if f = 31 then some (2, wCmp eqW)
  else none

def decodeW : WVal → String
  | .structv 0 [a, b] => s!"{smallOf a}/{smallOf b}"
  | .i32v n => s!"i32 {n}"
  | w => s!"other {repr w}"

/-! ## Talos side: heap carriers and a host environment -/

def carrierT (k : Int) : GcObject :=
  .struct 3 [.i64 (Int64.ofInt k).toUInt64, .anyref none, .i32 0]

def fractionT (top bottom : Nat) : GcObject :=
  .struct 0 [.anyref (some (.struct top)), .anyref (some (.struct bottom))]

def readSmall (heap : List GcObject) (a : Nat) : Option Int :=
  match heap[a]? with
  | some (.struct 3 [.i64 s, .anyref none, .i32 _]) => some s.toInt64.toInt
  | _ => none

def refAddr? : Value → Option Nat
  | .anyref (some (.struct a)) => some a
  | _ => none

def allocCarrier (st : Store Unit) (k : Int) : Value × Store Unit :=
  (.anyref (some (.struct st.gcHeap.length)), { st with gcHeap := st.gcHeap ++ [carrierT k] })

def binHost (f : Int → Int → Int) : HostFn Unit :=
  { params := [.anyref, .anyref], results := [.anyref]
    invoke := fun st args =>
      match args with
      | [a, b] =>
        match refAddr? a >>= readSmall st.gcHeap, refAddr? b >>= readSmall st.gcHeap with
        | some x, some y => let (v, st') := allocCarrier st (f x y); .Return [v] st'
        | _, _ => .Trap st "smoke: non-small carrier"
      | _ => .Trap st "smoke: arity" }

def cmpHost (f : Int → Int → Int) : HostFn Unit :=
  { params := [.anyref, .anyref], results := [.i32]
    invoke := fun st args =>
      match args with
      | [a, b] =>
        match refAddr? a >>= readSmall st.gcHeap, refAddr? b >>= readSmall st.gcHeap with
        | some x, some y => .Return [.i32 (Int32.ofInt (f x y)).toUInt32] st
        | _, _ => .Trap st "smoke: non-small carrier"
      | _ => .Trap st "smoke: arity" }

def boxHost : HostFn Unit :=
  { params := [.i64], results := [.anyref]
    invoke := fun st args =>
      match args with
      | [.i64 u] => let (v, st') := allocCarrier st u.toInt64.toInt; .Return [v] st'
      | _ => .Trap st "smoke: box arity" }

def toIndexHost : HostFn Unit :=
  { params := [.anyref], results := [.i32], invoke := fun st _ => .Trap st "smoke: toIndex unused" }

/-- Positional over `k5Env.imports` = slots `[23, 24, 26, 25, 35, 30, 31]`. -/
def hostEnvK5 : HostEnv Unit :=
  { funcs := [boxHost, binHost (· + ·), binHost (· * ·), binHost (· - ·), toIndexHost,
              cmpHost cmpW, cmpHost eqW] }

def decodeT (heap : List GcObject) : Value → String
  | .anyref (some (.struct r)) =>
    match heap[r]? with
    | some (.struct 0 [.anyref (some (.struct t)), .anyref (some (.struct b))]) =>
        s!"{readSmall heap t}/{readSmall heap b}"
    | _ => s!"struct {r} (not a fraction)"
  | .i32 n => s!"i32 {n.toInt32.toInt}"
  | v => s!"other {repr v}"

/-! ## Running both sides -/

structure Case where
  name : String
  fn : Nat
  paramSorts : List STy
  result : STy
  body : List WInstr
  /-- Fractions `(a, b)` per argument. -/
  args : List (Int × Int)

def runWall (c : Case) : String :=
  match wFuncN codeTbl wallHost 8 c.fn (c.args.map fun (a, b) => fractionW a b) with
  | some w => decodeW w
  | none => "none"

def runTalos (c : Case) : String :=
  match translateList k5Env c.body with
  | none => "translate failed"
  | some body' =>
    let m := synthModule k5Env c.paramSorts c.result 1 body'
    -- heap: 2k carriers, then k fractions
    let carriers := c.args.foldr (fun (a, b) acc => carrierT a :: carrierT b :: acc) []
    let fracs := (List.range c.args.length).map fun i => fractionT (2 * i) (2 * i + 1)
    let base := carriers.length
    let store0 : Store Unit := { (m.initialStore : Store Unit) with gcHeap := carriers ++ fracs }
    let args : List Value := (List.range c.args.length).map fun i => .anyref (some (.struct (base + i)))
    match initSingleModuleConfig m hostEnvK5 k5Env.imports.length store0 args.reverse with
    | .error e => s!"init error: {e.message}"
    | .ok cfg =>
      let tr := runSteps 100000 cfg
      match tr.result with
      | .success [v] st => s!"{decodeT st.wasm.gcHeap v} ({tr.trace.length} steps)"
      | .success vs _ => s!"unexpected results {repr vs}"
      | .trapped reason _ => s!"trap: {reason.message}"
      | .outOfFuel _ => "out of fuel"
      | .internalError e _ => s!"internal error: {e.message}"

def cases : List Case :=
  [ { name := "plus(1/2, 1/3)", fn := 5, paramSorts := [.ref, .ref], result := .ref, body := plusBody,
      args := [(1, 2), (1, 3)] },
    { name := "plus(7/9, -2/5)", fn := 5, paramSorts := [.ref, .ref], result := .ref, body := plusBody,
      args := [(7, 9), (-2, 5)] },
    { name := "isNonNeg(1/2)", fn := 13, paramSorts := [.ref], result := .i32, body := isNonNegBody,
      args := [(1, 2)] },
    { name := "isNonNeg(-3/4)", fn := 13, paramSorts := [.ref], result := .i32, body := isNonNegBody,
      args := [(-3, 4)] },
    { name := "isNonNeg(0/1)", fn := 13, paramSorts := [.ref], result := .i32, body := isNonNegBody,
      args := [(0, 1)] },
    { name := "lessThan(1/2, 1/3)", fn := 12, paramSorts := [.ref, .ref], result := .i32,
      body := lessThanBody, args := [(1, 2), (1, 3)] },
    { name := "lessThan(1/3, 1/2)", fn := 12, paramSorts := [.ref, .ref], result := .i32,
      body := lessThanBody, args := [(1, 3), (1, 2)] } ]

def report : IO Unit := do
  for c in cases do
    let w := runWall c
    let t := runTalos c
    let agree := t.startsWith w
    IO.println s!"{c.name}: wall = {w}; talos = {t}; agree = {agree}"

#eval report

end Bridge.Smoke
