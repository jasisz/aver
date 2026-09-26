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
import SortedKeys

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

/-! ### Report data, one call group at a time

`groupMembers fns g` walks every plan, and the facets, policies and
termination witnesses of the report ask it once per exported plan, so a
kernel check of the whole report walks every plan once per export. The
producer lists the plans group by group (callees first, `group`
increasing), so the plans of one group are one run of the list. `runsOf`
cuts the list into its runs in one pass. When the runs' groups are
strictly increasing each run is all of its group's members, and the
report is computed once per run. The functions below decide that on the
runs and fall back to the definitions otherwise, so each is equal to what
it replaces for every list of plans (`reportFacets_eq_fast`,
`map_policy_of_derived`, `map_termination_of_derived`); only the time it
takes depends on the order. -/

/-- The plans cut into maximal runs of consecutive entries of one group,
    each with that group. -/
def runsOf : List FnEntry → List (Nat × List FnEntry)
  | [] => []
  | e :: es =>
      match runsOf es with
      | (g, ms) :: rest =>
          if e.group == g then (g, e :: ms) :: rest else (e.group, [e]) :: (g, ms) :: rest
      | [] => [(e.group, [e])]

theorem runsOf_spec : ∀ fns : List FnEntry,
    (runsOf fns).flatMap (·.2) = fns ∧ ∀ r ∈ runsOf fns, ∀ e ∈ r.2, e.group = r.1
  | [] => ⟨rfl, fun _ hr => by cases hr⟩
  | e :: es => by
      obtain ⟨ih1, ih2⟩ := runsOf_spec es
      unfold runsOf
      cases h : runsOf es with
      | nil =>
          rw [h] at ih1
          simp only [List.flatMap_nil] at ih1
          subst ih1
          refine ⟨rfl, ?_⟩
          intro r hr x hx
          simp only [List.mem_singleton] at hr
          subst hr
          obtain rfl : x = e := by simpa using hx
          rfl
      | cons r0 rest =>
          obtain ⟨g, ms⟩ := r0
          rw [h] at ih1 ih2
          simp only
          split
          · rename_i hg
            refine ⟨?_, ?_⟩
            · simp only [List.flatMap_cons] at ih1 ⊢
              rw [← ih1]; rfl
            · intro r hr x hx
              cases hr with
              | head =>
                  cases hx with
                  | head => exact beq_iff_eq.mp hg
                  | tail _ hx' => exact ih2 (g, ms) List.mem_cons_self x hx'
              | tail _ hr' => exact ih2 r (List.mem_cons_of_mem _ hr') x hx
          · refine ⟨?_, ?_⟩
            · simp only [List.flatMap_cons] at ih1 ⊢
              rw [← ih1]; rfl
            · intro r hr x hx
              cases hr with
              | head =>
                  obtain rfl : x = e := by simpa using hx
                  rfl
              | tail _ hr' => exact ih2 r hr' x hx

/-- With distinct groups, the members of a run's group are that run. -/
theorem filter_group_of_runs : ∀ (runs : List (Nat × List FnEntry)),
    (runs.map (·.1)).Nodup → (∀ r ∈ runs, ∀ e ∈ r.2, e.group = r.1) →
    ∀ r ∈ runs, (runs.flatMap (·.2)).filter (·.group == r.1) = r.2
  | [], _, _, _, hr => by cases hr
  | r0 :: rest, hnd, hg, r, hr => by
      simp only [List.map_cons, List.nodup_cons, List.mem_map, not_exists, not_and] at hnd
      obtain ⟨hout, hnd'⟩ := hnd
      have hg' : ∀ r ∈ rest, ∀ e ∈ r.2, e.group = r.1 :=
        fun r hr => hg r (List.mem_cons_of_mem _ hr)
      -- A run keeps exactly the members of its own group.
      have hall : ∀ r ∈ r0 :: rest, r.2.filter (·.group == r.1) = r.2 := fun r hr =>
        List.filter_eq_self.mpr fun e he => beq_iff_eq.mpr (hg r hr e he)
      -- A run of another group keeps none.
      have hnone : ∀ r' ∈ r0 :: rest, r'.1 ≠ r.1 → r'.2.filter (·.group == r.1) = [] :=
        fun r' hr' hne => List.filter_eq_nil_iff.mpr fun e he h =>
          hne ((hg r' hr' e he).symm.trans (beq_iff_eq.mp h))
      rw [List.flatMap_cons, List.filter_append]
      cases hr with
      | head =>
          rw [hall r0 List.mem_cons_self]
          have hrest : (rest.flatMap (·.2)).filter (·.group == r0.1) = [] := by
            rw [List.filter_flatMap]
            exact List.flatMap_eq_nil_iff.mpr fun r' hr' =>
              hnone r' (List.mem_cons_of_mem _ hr') fun he => hout r' hr' he
          rw [hrest, List.append_nil]
      | tail _ hr' =>
          have hne : r0.1 ≠ r.1 := fun he => hout r hr' he.symm
          rw [hnone r0 List.mem_cons_self hne, List.nil_append]
          exact filter_group_of_runs rest hnd' hg' r hr'

/-- Anything computed per exported plan from its group's members, computed
    per run instead. -/
theorem map_groupMembers_of_runs {α : Type} (f : List (Nat × FnPlan) → FnEntry → α)
    (fns : List FnEntry) (h : ((runsOf fns).map (·.1)).Nodup) :
    (fns.filter (·.exported)).map (fun e => f (groupMembers fns e.group) e) =
      (runsOf fns).flatMap (fun r =>
        (r.2.filter (·.exported)).map (f (r.2.map fun e => (e.funcIdx, e.plan)))) := by
  obtain ⟨hflat, hg⟩ := runsOf_spec fns
  have hmem : ∀ r ∈ runsOf fns, groupMembers fns r.1 = r.2.map fun e => (e.funcIdx, e.plan) :=
    fun r hr => by
      unfold groupMembers
      conv => lhs; rw [← hflat]
      rw [filter_group_of_runs (runsOf fns) h hg r hr]
  -- Walk the runs, keeping `fns` itself inside `groupMembers`.
  have key : ∀ runs : List (Nat × List FnEntry), (∀ r ∈ runs, r ∈ runsOf fns) →
      ((runs.flatMap (·.2)).filter (·.exported)).map (fun e => f (groupMembers fns e.group) e) =
        runs.flatMap (fun r => (r.2.filter (·.exported)).map
          (f (r.2.map fun e => (e.funcIdx, e.plan)))) := by
    intro runs hsub
    induction runs with
    | nil => rfl
    | cons r rest ih =>
        rw [List.flatMap_cons, List.filter_append, List.map_append, List.flatMap_cons,
          ih (fun r' hr' => hsub r' (List.mem_cons_of_mem _ hr'))]
        congr 1
        apply List.map_congr_left
        intro e he
        have hr := hsub r List.mem_cons_self
        rw [hg r hr e (List.mem_filter.mp he).1, hmem r hr]
  have := key (runsOf fns) (fun _ h => h)
  rwa [hflat] at this

/-- The members of a group as `groupMembers` lists them, given a run. -/
def runMembers (ms : List FnEntry) : List (Nat × FnPlan) :=
  ms.map fun e => (e.funcIdx, e.plan)

/-- `facetsOf`, with the group's members given. -/
def facetsIn (grp : List (Nat × FnPlan)) (e : FnEntry) : List String :=
  let body := facetsE e.plan.body
  let recursive := grp.any fun m => (callTargets m.2.body).any fun t => grp.any (·.1 == t)
  ["recursive", "mutual", "calls", "records", "variants", "strings", "floats"].filter fun f =>
    if f == "recursive" then recursive
    else if f == "mutual" then recursive && decide (2 ≤ grp.length)
    else body.contains f

theorem facetsOf_eq (fns : List FnEntry) (e : FnEntry) :
    facetsOf fns e = facetsIn (groupMembers fns e.group) e := rfl

/-- The runs' groups, which `strictly` decides distinct. -/
def runGroups (fns : List FnEntry) : List Nat := (runsOf fns).map (·.1)

/-- `reportFacets`, once per run. -/
def reportFacetsFast (fns : List FnEntry) : List (String × List String) :=
  if _root_.AverCert.SortedKeys.strictly (runGroups fns) then
    (runsOf fns).flatMap fun r =>
      (r.2.filter (·.exported)).map fun e => (e.name, facetsIn (runMembers r.2) e)
  else (fns.filter (·.exported)).map fun e => (e.name, facetsOf fns e)

theorem reportFacets_eq_fast (artifact : ArtifactData) :
    reportFacets artifact = reportFacetsFast artifact.manifest.fnPlans := by
  unfold reportFacets reportFacetsFast
  split
  · rename_i h
    exact map_groupMembers_of_runs (fun grp e => (e.name, facetsIn grp e)) _
      (_root_.AverCert.SortedKeys.strictly_nodup h)
  · rfl

/-- The obligations' policies, once per run. -/
def policiesFast (fns : List FnEntry) : List Policy :=
  if _root_.AverCert.SortedKeys.strictly (runGroups fns) then
    (runsOf fns).flatMap fun r =>
      (r.2.filter (·.exported)).map fun _ => (groupPolicy (runMembers r.2)).1
  else (fns.filter (·.exported)).map fun e => (axesOf fns e).1

/-- The obligations' termination witnesses, once per run. -/
def terminationsFast (fns : List FnEntry) : List (Option TerminationWitness) :=
  if _root_.AverCert.SortedKeys.strictly (runGroups fns) then
    (runsOf fns).flatMap fun r =>
      (r.2.filter (·.exported)).map fun _ => (groupPolicy (runMembers r.2)).2.1
  else (fns.filter (·.exported)).map fun e => (axesOf fns e).2.1

theorem map_policy_of_derived {artifact : ArtifactData} (h : obligationsDerived artifact) :
    artifact.manifest.obligations.map (·.policy) = policiesFast artifact.manifest.fnPlans := by
  unfold obligationsDerived at h
  rw [h]
  unfold obligationsOf policiesFast
  rw [List.map_map]
  split
  · rename_i hs
    exact map_groupMembers_of_runs (fun grp _ => (groupPolicy grp).1) _
      (_root_.AverCert.SortedKeys.strictly_nodup hs)
  · rfl

theorem map_termination_of_derived {artifact : ArtifactData} (h : obligationsDerived artifact) :
    artifact.manifest.obligations.map (·.termination?) =
      terminationsFast artifact.manifest.fnPlans := by
  unfold obligationsDerived at h
  rw [h]
  unfold obligationsOf terminationsFast
  rw [List.map_map]
  split
  · rename_i hs
    exact map_groupMembers_of_runs (fun grp _ => (groupPolicy grp).2.1) _
      (_root_.AverCert.SortedKeys.strictly_nodup hs)
  · rfl

/-- All producer-selectable claim metadata the wall canonicalizes. -/
def checked (artifact : ArtifactData) : Bool :=
  contractsMatch artifact

end AverCert.ClaimAxes
