import Bridge.Coverage

/-!
# Tripwire: the profile is enumerated on both grammars

Fail-closed companions of `Coverage.lean` (brief §3: "tripwire enumerujący
ctory jest dodatkiem, nie zamiennikiem").

* `nodeInProfile` (Coverage.lean) has an explicit arm for EVERY
  `FragNodeKind` constructor and `primInProfile` for every `FragPrim`; a
  constructor added to the wall's grammar (the verbatim copy in `AverMin.lean`
  is diffed against the wall) makes those matches non-exhaustive and the build
  stops.
* `wInstrInProfile` below has an explicit arm for every `WInstr` constructor,
  and `translate_eq_none_of_out` proves it is exactly the set `translate`
  refuses unconditionally (the in-profile constructors carry side conditions:
  bands, declared struct sorts, an import slot).
* `samples` is one minimal checked plan per `FragNodeKind` constructor (and
  per out-of-profile primitive), run through the wall's own
  `lowerExprFragmentBody` and the bridge's `translateList` over the k5
  environment: every in-profile sample must lower, translate and pass
  `planInProfile`; every out-of-profile sample must lower (the wall admits
  it), fail `planInProfile`, and translate exactly as the table says — most
  are refused by `translate` itself, but a `structGetUser` of an UNDECLARED
  struct index translates (`translate` is type-blind on `structGet`, like
  Talos's `Step`) and is refused only by the typing `HasTy` — which is why
  the coverage lemma, not the translation, is the gate. The `#eval` throws —
  and the build fails — on any disagreement.
-/

namespace Bridge.Tripwire
open Wasm CertPrelude AverCert.Schema AverCert.PlanCheck AverCert.PlanLower Bridge

/-! ## `WInstr` side -/

/-- Which `WInstr` constructors `translate` can accept. Every constructor is
    listed; no wildcard. -/
def wInstrInProfile : WInstr → Bool
  | .localGet _ => true
  | .localSet _ => true
  | .i64Const _ => true
  | .i32Const _ => true
  | .f64Const _ => false
  | .refNull => false
  | .refIsNull => true
  | .refTest _ => false
  | .refCast _ => false
  | .structNew _ _ => true
  | .structGet _ _ => true
  | .arrayNewFixed _ _ => false
  | .arrayNewData _ _ => false
  | .arrayLen => false
  | .arrayGet _ => false
  | .i64Eqz => false
  | .i64Eq => true
  | .i64LeS => true
  | .i64LtS => true
  | .i64GeS => true
  | .i64GtS => true
  | .i32Eq => true
  | .i32And => false
  | .i32LtS => true
  | .i32LeS => false
  | .i32GtS => true
  | .i32GeS => true
  | .i32LtU => false
  | .f64Add => false
  | .f64Sub => false
  | .f64Mul => false
  | .f64Div => false
  | .f64Eq => false
  | .f64Lt => false
  | .f64Le => false
  | .f64Ge => false
  | .f64Gt => false
  | .ifElse _ _ => true
  | .call _ => true
  | .returnCall _ => false
  | .ret => false

/-- Outside the table, `translate` is `none` for every environment. -/
theorem translate_eq_none_of_out (env : TranslateEnv) (i : WInstr) (h : wInstrInProfile i = false) :
    translate env i = none := by
  cases i <;> simp_all [wInstrInProfile, translate]

/-- Inside the table, `translate` is `some` exactly under the side condition
    of the corresponding typing rule (the coverage lemma is the proof that a
    checked, in-profile plan meets them); this is the executable twin. -/
theorem translate_some_imp_in (env : TranslateEnv) (i : WInstr) (x : Instruction)
    (h : translate env i = some x) : wInstrInProfile i = true := by
  cases hc : wInstrInProfile i with
  | true => rfl
  | false =>
    rw [translate_eq_none_of_out env i hc] at h
    simp at h

/-! ## `FragNodeKind` side: one checked sample per constructor -/

structure Sample where
  name : String
  /-- `true`: in profile — must lower, translate and pass `planInProfile`;
      `false`: out of profile — must lower (the wall admits it), fail
      `planInProfile`, and translate iff `translates`. -/
  inProfile : Bool
  translates : Bool
  plan : ExprFragmentRawPlan

def node (id : Nat) (ty : FragTy) (kind : FragNodeKind) : FragNode := { id, ty, kind }

def mk (name : String) (inProfile : Bool) (params : List FragTy) (result : FragTy)
    (nodes : List FragNode) (translates : Bool := inProfile) : Sample :=
  { name, inProfile, translates,
    plan := { profile := "expr-fragment-v1", params, result,
              body := { nodes, result := nodes.length - 1 } } }

/-- Over `k5Env` (carrier 3, `Fraction` = struct 0 `[ref, ref]`, host slots
    23/24/26/25/35/30/31). -/
def samples : List Sample :=
  [ mk "local" true [.intCarrier] .intCarrier [node 0 .intCarrier (.local 0)],
    mk "constBool" true [] .boolI32 [node 0 .boolI32 (.constBool true)],
    mk "constI64 (in band)" true [] .i64 [node 0 .i64 (.constI64 5)],
    mk "constI64 (out of band)" false [] .i64 [node 0 .i64 (.constI64 (2 ^ 70))],
    mk "constI32 (in band)" true [] .rawI32 [node 0 .rawI32 (.constI32 7)],
    mk "constI32 (out of band)" false [] .rawI32 [node 0 .rawI32 (.constI32 (2 ^ 40))],
    mk "constF64Bits" false [] .f64 [node 0 .f64 (.constF64Bits 0)],
    mk "structGet (carrier limb)" true [.intCarrier] .i64
      [node 0 .intCarrier (.local 0), node 1 .i64 (.structGet 0 0)],
    mk "structGetUser (declared record)" true [.adtRef] .intCarrier
      [node 0 .adtRef (.local 0), node 1 .intCarrier (.structGetUser 0 0 0)],
    mk "structGetUser (undeclared struct: typing refuses, translation does not)" false
      [.adtRef] .intCarrier
      [node 0 .adtRef (.local 0), node 1 .intCarrier (.structGetUser 7 0 0)] (translates := true),
    mk "structNew (declared record)" true [.intCarrier, .intCarrier] .adtRef
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.local 1),
       node 2 .adtRef (.structNew 0 [0, 1])],
    mk "structNew (arity disagrees with the declaration)" false [.intCarrier] .adtRef
      [node 0 .intCarrier (.local 0), node 1 .adtRef (.structNew 0 [0])],
    mk "refIsNull" true [.intCarrier] .boolI32
      [node 0 .intCarrier (.local 0), node 1 .ref (.structGet 1 0), node 2 .boolI32 (.refIsNull 1)],
    mk "prim i64LtS" true [] .boolI32
      [node 0 .i64 (.constI64 1), node 1 .i64 (.constI64 2), node 2 .boolI32 (.prim .i64LtS [0, 1])],
    mk "prim i32GeS" true [] .boolI32
      [node 0 .rawI32 (.constI32 0), node 1 .rawI32 (.constI32 1),
       node 2 .boolI32 (.prim .i32GeS [0, 1])],
    mk "prim f64Le" false [.f64, .f64] .boolI32
      [node 0 .f64 (.local 0), node 1 .f64 (.local 1), node 2 .boolI32 (.prim .f64Le [0, 1])],
    mk "prim i32And" false [] .boolI32
      [node 0 .boolI32 (.constBool true), node 1 .boolI32 (.constBool false),
       node 2 .boolI32 (.prim .i32And [0, 1])],
    mk "hostCall add (table index)" true [.intCarrier, .intCarrier] .intCarrier
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.local 1),
       node 2 .intCarrier (.hostCall .add 24 [0, 1])],
    mk "hostCall add (index not in the table)" false [.intCarrier, .intCarrier] .intCarrier
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.local 1),
       node 2 .intCarrier (.hostCall .add 99 [0, 1])],
    mk "hostCall box" true [] .intCarrier
      [node 0 .i64 (.constI64 5), node 1 .intCarrier (.hostCall .box 23 [0])],
    mk "hostCall cmp" true [.intCarrier, .intCarrier] .rawI32
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.local 1),
       node 2 .rawI32 (.hostCall .cmp 30 [0, 1])],
    mk "hostCall eq" true [.intCarrier, .intCarrier] .boolI32
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.local 1),
       node 2 .boolI32 (.hostCall .eq 31 [0, 1])],
    mk "selfCall" false [.intCarrier] .intCarrier
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.selfCall false 5 [0])],
    mk "selfCall (tail)" false [.intCarrier] .intCarrier
      [node 0 .intCarrier (.local 0), node 1 .intCarrier (.selfCall true 5 [0])],
    mk "ifElse (nested)" true [] .i64
      [node 0 .boolI32 (.constBool true),
       node 1 .i64 (.ifElse 0
         { nodes := [node 0 .boolI32 (.constBool false),
                     node 1 .i64 (.ifElse 0 { nodes := [node 0 .i64 (.constI64 1)], result := 0 }
                                             { nodes := [node 0 .i64 (.constI64 2)], result := 0 })],
           result := 1 }
         { nodes := [node 0 .i64 (.constI64 3)], result := 0 })],
    mk "vectorGetOrDefault" false [.adtRef, .intCarrier] .intCarrier
      [node 0 .intCarrier (.vectorGetOrDefault 0 35 23 0)],
    mk "intSignCmp" true [.intCarrier] .boolI32
      [node 0 .intCarrier (.local 0), node 1 .boolI32 (.intSignCmp .lt 0 1 0)] ]

def checkSample (s : Sample) : Except String Unit := do
  let some body := lowerExprFragmentBody 3 s.plan
    | throw s!"{s.name}: the wall refuses the sample (checkExprFragmentRawPlan / lowering)"
  let translated := (translateList k5Env body).isSome
  let inProf := planInProfile k5Env s.plan
  if s.inProfile && !s.translates then
    throw s!"{s.name}: an in-profile sample must translate (coverage)"
  if translated != s.translates then
    throw s!"{s.name}: translate = {translated}, expected {s.translates}"
  if inProf != s.inProfile then
    throw s!"{s.name}: planInProfile = {inProf}, expected {s.inProfile}"

/-- Every `FragNodeKind` constructor has a sample (checked by name against
    the constructor list, so a new constructor without a sample fails here). -/
def kindNames : List String :=
  ["local", "constBool", "constI64", "constI32", "constF64Bits", "structGet", "structGetUser",
   "refIsNull", "prim", "hostCall", "selfCall", "ifElse", "vectorGetOrDefault", "structNew",
   "intSignCmp"]

def kindName : FragNodeKind → String
  | .local _ => "local"
  | .constBool _ => "constBool"
  | .constI64 _ => "constI64"
  | .constI32 _ => "constI32"
  | .constF64Bits _ => "constF64Bits"
  | .structGet _ _ => "structGet"
  | .structGetUser _ _ _ => "structGetUser"
  | .refIsNull _ => "refIsNull"
  | .prim _ _ => "prim"
  | .hostCall _ _ _ => "hostCall"
  | .selfCall _ _ _ => "selfCall"
  | .ifElse _ _ _ => "ifElse"
  | .vectorGetOrDefault _ _ _ _ => "vectorGetOrDefault"
  | .structNew _ _ => "structNew"
  | .intSignCmp _ _ _ _ => "intSignCmp"

def sampledKinds : List String :=
  samples.flatMap fun s => s.plan.body.nodes.map fun n => kindName n.kind

def run : IO Unit := do
  match samples.forM checkSample with
  | .error e => throw (IO.userError e)
  | .ok () => pure ()
  for k in kindNames do
    unless sampledKinds.contains k do
      throw (IO.userError s!"no sample exercises FragNodeKind.{k}")
  IO.println s!"tripwire: {samples.length} samples agree with the profile; \
    {(samples.filter (·.inProfile)).length} in, {(samples.filter (!·.inProfile)).length} out"

#eval run

end Bridge.Tripwire
