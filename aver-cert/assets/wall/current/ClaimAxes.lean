/-
Canonical claim axes and report data derived inside Lean.

Policy, termination evidence and totality role are fields of the derived
obligations (`AcceptedArtifact.obligationsOf`, from the wall's termination
check). What is left here is disclosure: the runtime contracts the certificate
is conditional on, computed from the helper calls the plans' lowerings make,
and the report entries (one class for every plan, D2, plus facets derived from
the plans). None of these is a producer choice; the checker witness pins them
against the JSON manifest.
-/
import AcceptedArtifactCore

namespace AverCert.ClaimAxes

open AverCert.Schema
open AverCert.Grammar
open AverCert.TypeTable
open AverCert.AcceptedArtifact
open CertPrelude

/-- The one report class of a certified export. -/
def planClass : String := "source-plan-v1"

mutual
  /-- Every function index a lowered body calls (`call` and `return_call`). -/
  def wCalls : WInstr → List Nat
    | .call f => [f]
    | .returnCall f => [f]
    | .ifElse t e => wCallsL t ++ wCallsL e
    | _ => []
  def wCallsL : List WInstr → List Nat
    | [] => []
    | i :: is => wCalls i ++ wCallsL is
end

/-- The helper calls of every planned function's lowering. -/
def usedCalls (M : MCtx) (fns : List FnEntry) : List Nat :=
  (fns.map fun e => wCallsL (fnCode M e.plan).body).flatten

structure ContractUse where
  box : Bool := false
  add : Bool := false
  sub : Bool := false
  mul : Bool := false
  stringEq : Bool := false
  stringConcat : Bool := false
  toIndex : Bool := false
  cmp : Bool := false
  eq : Bool := false
  divmod : Bool := false
  addTotal : Bool := false
  subTotal : Bool := false
  mulTotal : Bool := false
deriving Repr, DecidableEq

/-- The contracts one artifact depends on: a helper contract when some
    lowering calls that helper, and the totality contracts of every L3
    obligation's role. -/
def contractUse (artifact : ArtifactData) : ContractUse :=
  let m := artifact.manifest
  let M := mctxOf m.subject m.types m.fnPlans
  let calls := usedCalls M m.fnPlans
  let total := m.obligations.any fun o => o.policy == .simulatesModelTotally
  let totalMul := m.obligations.any fun o =>
    o.policy == .simulatesModelTotally && o.totalityRole == .mul
  { box := calls.contains M.box
    add := calls.contains M.add
    sub := calls.contains M.sub
    mul := calls.contains M.mul
    stringEq := calls.contains M.streq
    stringConcat := calls.contains M.concat
    toIndex := calls.contains M.toIndex
    cmp := calls.contains M.cmp
    eq := calls.contains M.eq
    divmod := calls.contains M.divmod
    addTotal := total
    subTotal := total
    mulTotal := totalMul }

def boxContract : String :=
  "__rt_aint_from_i64 (box i64 -> carrier)"
def addContract : String :=
  "Int.add (carrier add = exact integer addition on represented values; result canonical)"
def subContract : String :=
  "Int.sub (carrier sub = exact integer subtraction on represented values; result canonical)"
def mulContract : String :=
  "Int.mul (carrier mul = exact integer multiplication on represented values; result canonical)"
def stringEqContract : String :=
  "String.eq (WVal byte-array equality; non-arrays compare false)"
def stringConcatContract : String :=
  "String.concat (container-of-string-arrays -> byte-concatenated array)"
def toIndexContract : String :=
  "__aint_to_index (carrier -> i32 array index; [0, 2^31) passes, else -1)"
def cmpContract : String :=
  "__aint_cmp (canonical carrier pair -> i32 sign; -1 less, 0 equal, 1 greater)"
def eqContract : String :=
  "__aint_eq (canonical carrier pair -> i32 boolean; 1 when equal, else 0)"
def divmodContract : String :=
  "__aint_divmod (canonical carrier pair, nonzero divisor, want_mod 0 or 1 -> canonical Euclidean quotient (0) or remainder in [0, |b|) (1))"
def addTotalContract : String :=
  "Int.add (carrier add = exact integer addition on represented values; result canonical); total on represented values"
def subTotalContract : String :=
  "Int.sub (carrier sub = exact integer subtraction on represented values; result canonical); total on represented values"
def mulTotalContract : String :=
  "Int.mul (carrier mul = exact integer multiplication on represented values; result canonical); total on represented values"

def ContractUse.contracts (use : ContractUse) : List String :=
  (if use.box then [boxContract] else []) ++
  (if use.add then [addContract] else []) ++
  (if use.sub then [subContract] else []) ++
  (if use.mul then [mulContract] else []) ++
  (if use.stringEq then [stringEqContract] else []) ++
  (if use.stringConcat then [stringConcatContract] else []) ++
  (if use.toIndex then [toIndexContract] else []) ++
  (if use.cmp then [cmpContract] else []) ++
  (if use.eq then [eqContract] else []) ++
  (if use.divmod then [divmodContract] else []) ++
  (if use.addTotal then [addTotalContract] else []) ++
  (if use.subTotal then [subTotalContract] else []) ++
  (if use.mulTotal then [mulTotalContract] else [])

def requiredContracts (artifact : ArtifactData) : List String :=
  (contractUse artifact).contracts

def contractsMatch (artifact : ArtifactData) : Bool :=
  requiredContracts artifact == artifact.manifest.subject.contracts

/-! ### Report data -/

mutual
  /-- Facet flags of one plan body: `(calls, records, variants, strings,
      floats)`. -/
  def facetsE : Expr → List String
    | .literal (.str _) => ["strings"]
    | .literal (.float _) => ["floats"]
    | .literal _ => []
    | .local _ => []
    | .let_ _ v body => facetsE v ++ facetsE body
    | .call (.fn _) args => "calls" :: facetsL args
    | .call _ args => facetsL args
    | .tailCall _ args => "calls" :: facetsL args
    | .binOp _ l r => facetsE l ++ facetsE r
    | .neg e => facetsE e
    | .ifThenElse c t e => facetsE c ++ facetsE t ++ facetsE e
    | .recordCreate _ fs => "records" :: facetsL fs
    | .project _ _ b => "records" :: facetsE b
    | .match_ s arms => facetsE s ++ facetsA arms
    | .construct _ _ args => "variants" :: facetsL args
    | .interp parts => "strings" :: facetsL parts
    | .list _ items => facetsL items
  def facetsL : List Expr → List String
    | [] => []
    | e :: es => facetsE e ++ facetsL es
  def facetsA : Arms → List String
    | .nil => []
    | .cons p b rest =>
        (match p with
         | .ctor _ _ => ["variants"]
         | .litStr _ => ["strings"]
         | .tuple _ => ["records"]
         | _ => []) ++ facetsE b ++ facetsA rest
end

/-- The facets of one planned function, in a fixed order, derived from its
    plan and its group: `recursive` (its group calls itself), `mutual` (a group
    of two or more), then the constructs its body uses. -/
def facetsOf (fns : List FnEntry) (e : FnEntry) : List String :=
  let grp := groupMembers fns e.group
  let body := facetsE e.plan.body
  let recursive := grp.any fun m => (callTargets m.2.body).any fun t => grp.any (·.1 == t)
  ["recursive", "mutual", "calls", "records", "variants", "strings", "floats"].filter fun f =>
    if f == "recursive" then recursive
    else if f == "mutual" then recursive && decide (2 ≤ grp.length)
    else body.contains f

/-- `(export, class)` for every certified export, in obligation order. -/
def reportEntries (artifact : ArtifactData) : List (String × String) :=
  artifact.manifest.obligations.map fun o => (o.export_, planClass)

/-- `(export, facets)` for every exported planned function. -/
def reportFacets (artifact : ArtifactData) : List (String × List String) :=
  (artifact.manifest.fnPlans.filter (·.exported)).map fun e =>
    (e.name, facetsOf artifact.manifest.fnPlans e)

/-- All producer-selectable claim metadata the wall canonicalizes. -/
def checked (artifact : ArtifactData) : Bool :=
  contractsMatch artifact

end AverCert.ClaimAxes
