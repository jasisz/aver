import Bridge.Accepted

set_option autoImplicit false

/-!
# Smoke test through the claim path (not part of the proof)

The three k5 compute claims `Domain_Rational_plus` / `isNonNeg` / `lessThan`,
starting from what the CLAIM carries: the raw `expr-fragment-v1` plans
(verbatim from the k5 certificate package's `Plans.lean`, lines 117, 357, 405 —
the package `aver certify` builds for `Domain_Rational`, kept in this
session's scratchpad, `scratchpad/k5b/cert`; its host-role table is the
manifest's `hostRoleTable`, its record declaration `Fraction` = struct 0
`[intCarrier, intCarrier]`), lowered by the wall's own `lowerExprFragmentBody 3`
(checked against the bodies the package's `Module.lean` carries), translated
over `envOfClaim` (= `k5Env`), and run in Talos with the ADAPTER host
(`adapterEnv`) over the wall-side small-int faces `wallHost`. The wall side
runs the same lowered bodies through `wFuncN` with the same `wallHost`. Both
results are decoded and compared.

Also executed here: `planInProfile k5Env plan` for the three plans, and the
declared-data hypotheses of `recordCompute_terminatesWith` on the k5 data
(`hostTableBound` against the manifest's decoded roles, `recordComputeNodeOk`,
the struct-index agreement, `planTypedB`, the record declaration, and the
three extra hypotheses of `Accepted.lean`) — all decidable, all `true`.
-/

namespace Bridge.Smoke
open Wasm Wasm.SmallStep CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.PlanLower
  AverCert.StandardFace RecordComputeBridge Bridge

/-! ## The claims' plans (verbatim, `scratchpad/k5b/cert/Plans.lean`) -/

def Domain_Rational_plusPlan : ExprFragmentRawPlan := { profile := "expr-fragment-v1", params := [.adtRef, .adtRef], result := .adtRef, body := ({ nodes := [{ id := 0, ty := .adtRef, kind := .local 0 }, { id := 1, ty := .intCarrier, kind := .structGetUser 0 0 0 }, { id := 2, ty := .adtRef, kind := .local 1 }, { id := 3, ty := .intCarrier, kind := .structGetUser 0 1 2 }, { id := 4, ty := .intCarrier, kind := .hostCall .mul 26 [1, 3] }, { id := 5, ty := .adtRef, kind := .local 1 }, { id := 6, ty := .intCarrier, kind := .structGetUser 0 0 5 }, { id := 7, ty := .adtRef, kind := .local 0 }, { id := 8, ty := .intCarrier, kind := .structGetUser 0 1 7 }, { id := 9, ty := .intCarrier, kind := .hostCall .mul 26 [6, 8] }, { id := 10, ty := .intCarrier, kind := .hostCall .add 24 [4, 9] }, { id := 11, ty := .adtRef, kind := .local 0 }, { id := 12, ty := .intCarrier, kind := .structGetUser 0 1 11 }, { id := 13, ty := .adtRef, kind := .local 1 }, { id := 14, ty := .intCarrier, kind := .structGetUser 0 1 13 }, { id := 15, ty := .intCarrier, kind := .hostCall .mul 26 [12, 14] }, { id := 16, ty := .adtRef, kind := .structNew 0 [10, 15] }], result := 16 } : FragBlock) }

def Domain_Rational_lessThanPlan : ExprFragmentRawPlan := { profile := "expr-fragment-v1", params := [.adtRef, .adtRef], result := .boolI32, body := ({ nodes := [{ id := 0, ty := .adtRef, kind := .local 0 }, { id := 1, ty := .intCarrier, kind := .structGetUser 0 0 0 }, { id := 2, ty := .adtRef, kind := .local 0 }, { id := 3, ty := .intCarrier, kind := .structGetUser 0 1 2 }, { id := 4, ty := .intCarrier, kind := .hostCall .mul 26 [1, 3] }, { id := 5, ty := .adtRef, kind := .local 1 }, { id := 6, ty := .intCarrier, kind := .structGetUser 0 1 5 }, { id := 7, ty := .adtRef, kind := .local 1 }, { id := 8, ty := .intCarrier, kind := .structGetUser 0 1 7 }, { id := 9, ty := .intCarrier, kind := .hostCall .mul 26 [6, 8] }, { id := 10, ty := .intCarrier, kind := .hostCall .mul 26 [4, 9] }, { id := 11, ty := .adtRef, kind := .local 1 }, { id := 12, ty := .intCarrier, kind := .structGetUser 0 0 11 }, { id := 13, ty := .adtRef, kind := .local 1 }, { id := 14, ty := .intCarrier, kind := .structGetUser 0 1 13 }, { id := 15, ty := .intCarrier, kind := .hostCall .mul 26 [12, 14] }, { id := 16, ty := .adtRef, kind := .local 0 }, { id := 17, ty := .intCarrier, kind := .structGetUser 0 1 16 }, { id := 18, ty := .adtRef, kind := .local 0 }, { id := 19, ty := .intCarrier, kind := .structGetUser 0 1 18 }, { id := 20, ty := .intCarrier, kind := .hostCall .mul 26 [17, 19] }, { id := 21, ty := .intCarrier, kind := .hostCall .mul 26 [15, 20] }, { id := 22, ty := .rawI32, kind := .hostCall .cmp 30 [10, 21] }, { id := 23, ty := .rawI32, kind := .constI32 (0 : Int) }, { id := 24, ty := .boolI32, kind := .prim .i32LtS [22, 23] }], result := 24 } : FragBlock) }

def Domain_Rational_isNonNegPlan : ExprFragmentRawPlan := { profile := "expr-fragment-v1", params := [.adtRef], result := .boolI32, body := ({ nodes := [{ id := 0, ty := .adtRef, kind := .local 0 }, { id := 1, ty := .intCarrier, kind := .structGetUser 0 0 0 }, { id := 2, ty := .adtRef, kind := .local 0 }, { id := 3, ty := .intCarrier, kind := .structGetUser 0 1 2 }, { id := 4, ty := .intCarrier, kind := .hostCall .mul 26 [1, 3] }, { id := 5, ty := .boolI32, kind := .intSignCmp .ge (0 : Int) 1 4 }], result := 5 } : FragBlock) }

/-- The manifest's decoded role table (`cert-manifest.json` / `Manifest.lean`:
    `hostRoleTable`). -/
def k5Roles : CertDecode.AddSub.Roles :=
  { box := some 23, add := some 24, mul := some 26, sub := some 25, toIndex := some 35,
    cmp := some 30, eq := some 31 }

/-! ## The bodies the package's `Module.lean` carries (the earlier smoke's copies) -/

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

/-! ## Wall side: small-int faces of the host slots -/

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

/-- The compute face's host table over the reference faces. -/
def wallHost : HostTbl :=
  recordComputeSlots 3 (wBin (· + ·)) (wBin (· - ·)) (wBin (· * ·)) (wCmp cmpW) (wCmp eqW) k5HostTable

def decodeW : WVal → String
  | .structv 0 [a, b] => s!"{smallOf a}/{smallOf b}"
  | .i32v n => s!"i32 {n}"
  | w => s!"other {repr w}"

/-! ## Talos side: the adapter over the same wall host -/

def carrierT (k : Int) : GcObject :=
  .struct 3 [.i64 (Int64.ofInt k).toUInt64, .anyref none, .i32 0]

def fractionT (top bottom : Nat) : GcObject :=
  .struct 0 [.anyref (some (.struct top)), .anyref (some (.struct bottom))]

def readSmall (heap : List GcObject) (a : Nat) : Option Int :=
  match heap[a]? with
  | some (.struct 3 [.i64 s, .anyref none, .i32 _]) => some s.toInt64.toInt
  | _ => none

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
  plan : ExprFragmentRawPlan
  expected : List WInstr
  /-- Fractions `(a, b)` per argument. -/
  args : List (Int × Int)

def sortsOf (plan : ExprFragmentRawPlan) : List STy := plan.params.map sortOfFragTy

def runWall (c : Case) (body : List WInstr) : String :=
  let code : CodeTbl := fun fn => if fn = 0 then some ⟨c.plan.params.length, 1, body⟩ else none
  match wFuncN code wallHost 8 0 (c.args.map fun (a, b) => fractionW a b) with
  | some w => decodeW w
  | none => "none"

def runTalos (c : Case) (body : List WInstr) : String :=
  match translateList k5Env body with
  | none => "translate failed"
  | some body' =>
    let m := synthModule k5Env (sortsOf c.plan) (sortOfFragTy c.plan.result) 1 body'
    -- heap: 2k carriers, then k fractions
    let carriers := c.args.foldr (fun (a, b) acc => carrierT a :: carrierT b :: acc) []
    let fracs := (List.range c.args.length).map fun i => fractionT (2 * i) (2 * i + 1)
    let base := carriers.length
    let store0 : Store Unit := { (m.initialStore : Store Unit) with gcHeap := carriers ++ fracs }
    let args : List Value := (List.range c.args.length).map fun i => .anyref (some (.struct (base + i)))
    match initSingleModuleConfig m (adapterEnv Unit k5Env wallHost) k5Env.imports.length store0
        args.reverse with
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
  [ { name := "plus(1/2, 1/3)", plan := Domain_Rational_plusPlan, expected := plusBody,
      args := [(1, 2), (1, 3)] },
    { name := "plus(7/9, -2/5)", plan := Domain_Rational_plusPlan, expected := plusBody,
      args := [(7, 9), (-2, 5)] },
    { name := "isNonNeg(1/2)", plan := Domain_Rational_isNonNegPlan, expected := isNonNegBody,
      args := [(1, 2)] },
    { name := "isNonNeg(-3/4)", plan := Domain_Rational_isNonNegPlan, expected := isNonNegBody,
      args := [(-3, 4)] },
    { name := "isNonNeg(0/1)", plan := Domain_Rational_isNonNegPlan, expected := isNonNegBody,
      args := [(0, 1)] },
    { name := "lessThan(1/2, 1/3)", plan := Domain_Rational_lessThanPlan, expected := lessThanBody,
      args := [(1, 2), (1, 3)] },
    { name := "lessThan(1/3, 1/2)", plan := Domain_Rational_lessThanPlan, expected := lessThanBody,
      args := [(1, 3), (1, 2)] } ]

/-! ## The declared-data hypotheses of `recordCompute_terminatesWith` on k5 -/

def k5Fields : List TypeDecl := [.intCarrier, .intCarrier]

def tyOfB (plan : ExprFragmentRawPlan) : Nat → FragTy :=
  fun nodeId => ((plan.body.nodes[nodeId]?).map (fun n => n.ty)).getD .i64

/-- Every hypothesis of the composition sentence that is declared data, as a
    Bool over the k5 claim (`structIdx` = 0, carrier = 3). -/
def compositionHyps (plan : ExprFragmentRawPlan) : List (String × Bool) :=
  [ ("hostTableBound", hostTableBound k5Roles k5HostTable),
    ("recordComputeNodeOk", plan.body.nodes.all fun n => recordComputeNodeOk k5HostTable n.kind),
    ("struct index = 0",
      (plan.body.nodes.filterMap fun n => fragNodeStructIdx? n.kind).all (· == 0)),
    ("planTypedB", planTypedB 0 (tyOfB plan) plan.params plan.body.nodes),
    ("params all adtRef", plan.params.all (· == .adtRef)),
    ("fields all intCarrier", k5Fields.all fun f => match f with | .intCarrier => true | _ => false),
    ("checkRecordDecl", checkRecordDecl (.record 0 k5Fields)),
    ("lowerExprFragmentBody = some", (lowerExprFragmentBody 3 plan).isSome),
    ("extra: constI32 in band", plan.body.nodes.all fun n =>
      match n.kind with | .constI32 v => decide (i32Band v) | _ => true),
    ("extra: struct arity agrees", plan.body.nodes.all fun n =>
      match n.kind with
      | .structNew 0 args => args.length == k5Fields.length
      | .structGetUser 0 field _ => field < k5Fields.length
      | _ => true),
    ("extra: structIdx ≠ carrier", 0 != 3),
    ("planInProfile", planInProfile k5Env plan) ]

def report : IO Unit := do
  for c in cases do
    let some body := lowerExprFragmentBody 3 c.plan
      | throw (IO.userError s!"{c.name}: the wall refuses the claim's plan")
    unless (repr body).pretty == (repr c.expected).pretty do
      throw (IO.userError s!"{c.name}: lowered body differs from the package's Module.lean")
    let w := runWall c body
    let t := runTalos c body
    let agree := t.startsWith w
    IO.println s!"{c.name}: wall = {w}; talos (adapter) = {t}; agree = {agree}"
    unless agree do throw (IO.userError s!"{c.name}: disagreement")
  for (name, plan) in [("plus", Domain_Rational_plusPlan), ("lessThan", Domain_Rational_lessThanPlan),
      ("isNonNeg", Domain_Rational_isNonNegPlan)] do
    for (h, b) in compositionHyps plan do
      unless b do throw (IO.userError s!"{name}: composition hypothesis `{h}` is false on k5")
    IO.println s!"{name}: all {(compositionHyps plan).length} declared-data hypotheses of \
      recordCompute_terminatesWith hold"

#eval report

end Bridge.Smoke
