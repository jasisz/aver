/- LUKA-1 v3: full expr-fragment generic certificate assembly. -/
import V3ExprFragmentFull
import V3StrongFuel
import V3IfElse

set_option maxRecDepth 1000000
set_option maxHeartbeats 8000000

namespace V3ExprFragmentGeneric
open CertPrelude AverCert.Schema AverCert.PlanLower
open V3ExprFragmentFull V3StrongFuel

/-! ## The call/grammar fence

`checkExprFragmentRawPlan` checks the representation types and ANF stack
discipline.  Runtime call arities are necessarily a separate hypothesis: they
live in the byte-derived host/code tables, not in the raw plan.  `CallsOK` is
the exact missing fence.  It also excludes `selfCall`, whose proof face is the
separate recursion family rather than expr-fragment-v1.

The nested grammar is genuinely recursive through `ifElse`; the explicit
size measure is what makes this definition robust to the mutually nested
`FragNodeKind`/`FragBlock` declarations. -/

mutual
  def kindCallsOK (host : HostTbl) (ar : Nat -> Option Nat)
      (kind : FragNodeKind) : Prop :=
    match kind with
    | .hostCall _ f args =>
        (exists hf, host f = some (args.length, hf)) \/
        (host f = none /\ ar f = some args.length)
    | .selfCall _ _ _ => False
    | .ifElse _ t e => blockCallsOK host ar t /\ blockCallsOK host ar e
    | _ => True
  termination_by (sizeOf kind, 1)
  decreasing_by all_goals simp_wf; omega

  def nodesCallsOK (host : HostTbl) (ar : Nat -> Option Nat)
      (nodes : List FragNode) : Prop :=
    match nodes with
    | [] => True
    | n :: ns => kindCallsOK host ar n.kind /\ nodesCallsOK host ar ns
  termination_by (sizeOf nodes, 2)
  decreasing_by all_goals cases n <;> simp_wf <;> omega

  def blockCallsOK (host : HostTbl) (ar : Nat -> Option Nat)
      (b : FragBlock) : Prop :=
    nodesCallsOK host ar b.nodes
  termination_by (sizeOf b, 3)
  decreasing_by all_goals cases b <;> simp_wf <;> omega
end

/-! ## Fuel-indexed lowering/execution correctness -/

def NodesCorrect (fuel : Nat) : Prop :=
  forall (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (carrier : Nat) (nodes : List FragNode) (symStack : List Nat)
    (instrs : List WInstr) (finalStack : List Nat),
    nodesCallsOK host ar nodes ->
    lowerNodesFuel fuel carrier nodes symStack = some (instrs, finalStack) ->
    forall (locals stack : List WVal) out,
      runNodesFuel host ar callee fuel carrier nodes symStack locals stack = some out ->
      wRunF host ar callee instrs locals stack = some out

def BlockCorrect (fuel : Nat) : Prop :=
  forall (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (carrier : Nat) (block : FragBlock) (instrs : List WInstr),
    blockCallsOK host ar block ->
    lowerBlockFuel fuel carrier block = some instrs ->
    forall locals out,
      runBlockFuel host ar callee fuel carrier block locals = some out ->
      wRunF host ar callee instrs locals [] = some out

theorem oneThenNodes (fuel : Nat) (hcorrect : NodesCorrect fuel)
    (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (carrier : Nat) (rest : List FragNode) (nextSym : List Nat)
    (instr : WInstr) (restInstrs : List WInstr) (finalStack : List Nat)
    (hcalls : nodesCallsOK host ar rest)
    (hlow : lowerNodesFuel fuel carrier rest nextSym =
      some (restInstrs, finalStack))
    (locals stack : List WVal) (out : Out)
    (hrun :
      (match wRunF host ar callee [instr] locals stack with
       | some (.ok locals' stack') =>
           runNodesFuel host ar callee fuel carrier rest nextSym locals' stack'
       | some (.ret value) => some (.ret value)
       | none => none) = some out) :
    wRunF host ar callee ([instr] ++ restInstrs) locals stack = some out := by
  rw [wRunF_append]
  cases hs : wRunF host ar callee [instr] locals stack with
  | none => simp [hs] at hrun
  | some stepOut =>
      cases stepOut with
      | ret value => simpa [seqOut, hs] using hrun
      | ok locals' stack' =>
          simp only [hs] at hrun
          simp only [seqOut]
          exact hcorrect host ar callee carrier rest nextSym restInstrs finalStack
            hcalls hlow locals' stack' out hrun

theorem runBlockFuel_ok_stack
    (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (fuel carrier : Nat) (block : FragBlock) (locals locals' stack : List WVal)
    (h : runBlockFuel host ar callee fuel carrier block locals =
      some (.ok locals' stack)) : exists value, stack = [value] := by
  cases fuel with
  | zero => simp [runBlockFuel] at h
  | succ fuel =>
      simp only [runBlockFuel] at h
      cases hr : runNodesFuel host ar callee fuel carrier block.nodes [] locals [] with
      | none => simp [hr] at h
      | some out =>
          rw [hr] at h
          cases out with
          | ret value => simp at h
          | ok ls st =>
              cases st with
              | nil => simp at h
              | cons value tail =>
                  cases tail with
                  | nil =>
                      have hs : ls = locals' /\ [value] = stack := by simpa using h
                      exact ⟨value, hs.2.symm⟩
                  | cons value' tail => simp at h

theorem runPrim_eq_single
    (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (op : FragPrim) (locals stack : List WVal) :
    wRunF host ar callee [primInstr op] locals stack =
      match runPrim op stack with
      | some stack' => some (.ok locals stack')
      | none => none := by
  cases op <;> cases stack <;> try simp [runPrim, primInstr, wRunF]
  all_goals rename_i a stack
  all_goals cases a <;> cases stack <;> try simp [runPrim, primInstr, wRunF]
  all_goals rename_i b stack
  all_goals cases b <;> simp [runPrim, primInstr, wRunF]

/- The mutual theorem is intentionally stated in the strong-induction shape
   left by the architect fork. -/
theorem mutualCorrectStep :
    forall fuel,
      (forall smaller, smaller < fuel -> NodesCorrect smaller /\ BlockCorrect smaller) ->
      NodesCorrect fuel /\ BlockCorrect fuel := by
  intro fuel ihStrong
  cases fuel with
  | zero =>
      constructor
      · intro host ar callee carrier nodes symS instrs finalS _ hlow
        simp [lowerNodesFuel] at hlow
      · intro host ar callee carrier block instrs _ hlow
        simp [lowerBlockFuel] at hlow
  | succ fuel =>
      have ih : NodesCorrect fuel /\ BlockCorrect fuel :=
        ihStrong fuel (Nat.lt_succ_self fuel)
      constructor
      · intro host ar callee carrier nodes
        induction nodes with
        | nil =>
            intro symS instrs finalS _ hlow locals stack out hrun
            simp only [lowerNodesFuel, Option.some.injEq, Prod.mk.injEq] at hlow
            obtain ⟨rfl, rfl⟩ := hlow
            simpa [runNodesFuel, wRunF] using hrun
        | cons node rest restIH =>
            intro symS instrs finalS hcalls hlow locals stack out hrun
            simp only [nodesCallsOK] at hcalls
            obtain ⟨hcall, hcallsRest⟩ := hcalls
            simp only [lowerNodesFuel] at hlow
            cases hk : node.kind <;> simp only [hk] at hlow hcall <;>
              simp only [runNodesFuel, hk] at hrun
            next index =>
              cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS) with
              | none => simp [hrest] at hlow
              | some pair =>
                  obtain ⟨restInstrs, fin⟩ := pair
                  rw [hrest] at hlow
                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                  obtain ⟨rfl, rfl⟩ := hlow
                  exact oneThenNodes fuel ih.1 host ar callee carrier rest
                    (node.id :: symS) (.localGet index) restInstrs fin
                    hcallsRest hrest locals stack out hrun
            next value =>
              cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS) with
              | none => simp [hrest] at hlow
              | some pair =>
                  obtain ⟨restInstrs, fin⟩ := pair
                  rw [hrest] at hlow
                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                  obtain ⟨rfl, rfl⟩ := hlow
                  exact oneThenNodes fuel ih.1 host ar callee carrier rest
                    (node.id :: symS) (.i32Const (if value then 1 else 0)) restInstrs fin
                    hcallsRest hrest locals stack out hrun
            next value =>
              cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS) with
              | none => simp [hrest] at hlow
              | some pair =>
                  obtain ⟨restInstrs, fin⟩ := pair
                  rw [hrest] at hlow
                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                  obtain ⟨rfl, rfl⟩ := hlow
                  exact oneThenNodes fuel ih.1 host ar callee carrier rest
                    (node.id :: symS) (.i64Const value) restInstrs fin
                    hcallsRest hrest locals stack out hrun
            next value =>
              cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS) with
              | none => simp [hrest] at hlow
              | some pair =>
                  obtain ⟨restInstrs, fin⟩ := pair
                  rw [hrest] at hlow
                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                  obtain ⟨rfl, rfl⟩ := hlow
                  exact oneThenNodes fuel ih.1 host ar callee carrier rest
                    (node.id :: symS) (.i32Const value) restInstrs fin
                    hcallsRest hrest locals stack out hrun
            next bits =>
              cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS) with
              | none => simp [hrest] at hlow
              | some pair =>
                  obtain ⟨restInstrs, fin⟩ := pair
                  rw [hrest] at hlow
                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                  obtain ⟨rfl, rfl⟩ := hlow
                  exact oneThenNodes fuel ih.1 host ar callee carrier rest
                    (node.id :: symS) (.f64Const (UInt64.ofNat bits)) restInstrs fin
                    hcallsRest hrest locals stack out hrun
            next field receiver =>
              cases hp : popExpected symS receiver with
              | none => simp [hp] at hlow hrun
              | some symS' =>
                  simp only [hp] at hlow hrun
                  cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS') with
                  | none => simp [hrest] at hlow
                  | some pair =>
                      obtain ⟨restInstrs, fin⟩ := pair
                      rw [hrest] at hlow
                      simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                      obtain ⟨rfl, rfl⟩ := hlow
                      exact oneThenNodes fuel ih.1 host ar callee carrier rest
                        (node.id :: symS') (.structGet carrier field) restInstrs fin
                        hcallsRest hrest locals stack out hrun
            next tyIdx field value =>
              cases hp : popExpected symS value with
              | none => simp [hp] at hlow hrun
              | some symS' =>
                  simp only [hp] at hlow hrun
                  cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS') with
                  | none => simp [hrest] at hlow
                  | some pair =>
                      obtain ⟨restInstrs, fin⟩ := pair
                      rw [hrest] at hlow
                      simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                      obtain ⟨rfl, rfl⟩ := hlow
                      exact oneThenNodes fuel ih.1 host ar callee carrier rest
                        (node.id :: symS') (.structGet tyIdx field) restInstrs fin
                        hcallsRest hrest locals stack out hrun
            next value =>
              cases hp : popExpected symS value with
              | none => simp [hp] at hlow hrun
              | some symS' =>
                  simp only [hp] at hlow hrun
                  cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS') with
                  | none => simp [hrest] at hlow
                  | some pair =>
                      obtain ⟨restInstrs, fin⟩ := pair
                      rw [hrest] at hlow
                      simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                      obtain ⟨rfl, rfl⟩ := hlow
                      exact oneThenNodes fuel ih.1 host ar callee carrier rest
                        (node.id :: symS') .refIsNull restInstrs fin
                        hcallsRest hrest locals stack out hrun
            next op args =>
              cases hp : popExpectedAll symS args.reverse with
              | none => simp [hp] at hlow hrun
              | some symS' =>
                  simp only [hp] at hlow hrun
                  cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS') with
                  | none => simp [hrest] at hlow
                  | some pair =>
                      obtain ⟨restInstrs, fin⟩ := pair
                      rw [hrest] at hlow
                      simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                      obtain ⟨rfl, rfl⟩ := hlow
                      exact oneThenNodes fuel ih.1 host ar callee carrier rest
                        (node.id :: symS') (primInstr op) restInstrs fin
                        hcallsRest hrest locals stack out hrun
            next role funcIdx args =>
              cases hp : popExpectedAll symS args.reverse with
              | none => simp [hp] at hlow hrun
              | some symS' =>
                  simp only [hp] at hlow hrun
                  cases hrest : lowerNodesFuel fuel carrier rest (node.id :: symS') with
                  | none => simp [hrest] at hlow
                  | some pair =>
                      obtain ⟨restInstrs, fin⟩ := pair
                      rw [hrest] at hlow
                      simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                      obtain ⟨rfl, rfl⟩ := hlow
                      exact oneThenNodes fuel ih.1 host ar callee carrier rest
                        (node.id :: symS') (.call funcIdx) restInstrs fin
                        hcallsRest hrest locals stack out hrun
            next tail funcIdx args =>
              exact (by simpa [kindCallsOK] using hcall : False).elim
            next cond thenBlock elseBlock =>
              have hbranches : blockCallsOK host ar thenBlock /\
                  blockCallsOK host ar elseBlock := by
                simpa [kindCallsOK] using hcall
              cases hp : popExpected symS cond with
              | none => simp [hp] at hlow hrun
              | some popped =>
                  cases popped with
                  | cons x xs => simp [hp] at hlow hrun
                  | nil =>
                      simp only [hp] at hlow hrun
                      cases ht : lowerBlockFuel fuel carrier thenBlock with
                      | none => simp [ht] at hlow
                      | some thenInstrs =>
                          rw [ht] at hlow
                          cases he : lowerBlockFuel fuel carrier elseBlock with
                          | none => simp [he] at hlow
                          | some elseInstrs =>
                              rw [he] at hlow
                              simp only at hlow
                              cases hrest : lowerNodesFuel fuel carrier rest [node.id] with
                              | none => simp [hrest] at hlow
                              | some pair =>
                                  obtain ⟨restInstrs, fin⟩ := pair
                                  rw [hrest] at hlow
                                  simp only [Option.some.injEq, Prod.mk.injEq] at hlow
                                  obtain ⟨rfl, rfl⟩ := hlow
                                  cases stack with
                                  | nil => simp at hrun
                                  | cons cv stackTail =>
                                      cases cv <;> try simp at hrun
                                      next c =>
                                        cases stackTail with
                                        | cons x xs => simp at hrun
                                        | nil =>
                                            by_cases hc : c = 0
                                            · have hbcall := hbranches.2
                                              cases hb : runBlockFuel host ar callee fuel carrier
                                                  elseBlock locals with
                                              | none => simp [finishWith, hc, hb] at hrun
                                              | some branchOut =>
                                                  have hbw := ih.2 host ar callee carrier elseBlock
                                                    elseInstrs hbcall he locals branchOut hb
                                                  cases branchOut with
                                                  | ret value =>
                                                      simp [finishWith, hc, hb] at hrun
                                                      subst out
                                                      rw [wRunF_append]
                                                      simp [seqOut, wRunF, hc, hbw]
                                                  | ok locals' branchStack =>
                                                      obtain ⟨value, rfl⟩ :=
                                                        runBlockFuel_ok_stack host ar callee fuel
                                                          carrier elseBlock locals locals'
                                                          branchStack hb
                                                      have hstep : wRunF host ar callee
                                                          [.ifElse thenInstrs elseInstrs]
                                                          locals [.i32v c] =
                                                          some (.ok locals' [value]) := by
                                                        simp [wRunF, hc, hbw]
                                                      simp [finishWith, wRunF, hc, hb] at hrun
                                                      apply oneThenNodes fuel ih.1 host ar callee
                                                        carrier rest [node.id]
                                                        (.ifElse thenInstrs elseInstrs)
                                                        restInstrs fin hcallsRest hrest locals
                                                        [.i32v c] out
                                                      simpa [hstep] using hrun
                                            · have hbcall := hbranches.1
                                              cases hb : runBlockFuel host ar callee fuel carrier
                                                  thenBlock locals with
                                              | none => simp [finishWith, hc, hb] at hrun
                                              | some branchOut =>
                                                  have hbw := ih.2 host ar callee carrier thenBlock
                                                    thenInstrs hbcall ht locals branchOut hb
                                                  cases branchOut with
                                                  | ret value =>
                                                      simp [finishWith, hc, hb] at hrun
                                                      subst out
                                                      rw [wRunF_append]
                                                      simp [seqOut, wRunF, hc, hbw]
                                                  | ok locals' branchStack =>
                                                      obtain ⟨value, rfl⟩ :=
                                                        runBlockFuel_ok_stack host ar callee fuel
                                                          carrier thenBlock locals locals'
                                                          branchStack hb
                                                      have hstep : wRunF host ar callee
                                                          [.ifElse thenInstrs elseInstrs]
                                                          locals [.i32v c] =
                                                          some (.ok locals' [value]) := by
                                                        simp [wRunF, hc, hbw]
                                                      simp [finishWith, wRunF, hc, hb] at hrun
                                                      apply oneThenNodes fuel ih.1 host ar callee
                                                        carrier rest [node.id]
                                                        (.ifElse thenInstrs elseInstrs)
                                                        restInstrs fin hcallsRest hrest locals
                                                        [.i32v c] out
                                                      simpa [hstep] using hrun
      · intro host ar callee carrier block instrs hcalls hlow locals out hrun
        simp only [lowerBlockFuel] at hlow
        cases hn : lowerNodesFuel fuel carrier block.nodes [] with
        | none => simp [hn] at hlow
        | some pair =>
            obtain ⟨is, fs⟩ := pair
            rw [hn] at hlow
            cases fs with
            | nil => simp at hlow
            | cons r rs =>
                cases rs with
                | cons r' rs => simp at hlow
                | nil =>
                    by_cases hr : r = block.result
                    · subst r
                      have his : is = instrs := by simpa using hlow
                      subst instrs
                      simp only [blockCallsOK] at hcalls
                      simp only [runBlockFuel, hn] at hrun
                      cases hrn : runNodesFuel host ar callee fuel carrier
                          block.nodes [] locals [] with
                      | none => simp [hrn] at hrun
                      | some nodeOut =>
                          rw [hrn] at hrun
                          cases nodeOut with
                          | ret value =>
                              simp at hrun
                              subst out
                              exact ih.1 host ar callee carrier block.nodes [] is
                                [block.result] hcalls hn locals [] _ hrn
                          | ok locals' stack' =>
                              cases stack' with
                              | nil => simp at hrun
                              | cons value tail =>
                                  cases tail with
                                  | nil =>
                                      simp at hrun
                                      subst out
                                      exact ih.1 host ar callee carrier block.nodes [] is
                                        [block.result] hcalls hn locals [] _ hrn
                                  | cons value' tail => simp at hrun
                    · simp [hr] at hlow

theorem mutualCorrect (fuel : Nat) : NodesCorrect fuel /\ BlockCorrect fuel := by
  exact Nat.strongRecOn fuel (fun n ih => mutualCorrectStep n ih)

/-! ## Audited-plan generic certificate

The input values are supplied by the per-obligation semantic bridge.  This
keeps the lowering theorem representation-polymorphic: comparison obligations
may still choose the honest `carrierSmall` domain, while contracted integer
operations may retain their stronger arbitrary-`S.Repr` domain and Bool
fragments may use their canonical `b32` inputs.

`evalSymRawPlan` begins at the checked source `SymRawPlan`, invokes the audited
encoder, and evaluates the resulting structured plan.  Thus the theorem is
simultaneously gated by source encoding, `checkExprFragmentRawPlan`, and the
audited `lowerBlock`. -/

theorem exprfragment_generic_certified {C : Nat} (S : CarrierSpec C)
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (code : CodeTbl) (host : HostTbl)
    (symPlan : SymRawPlan) (plan : ExprFragmentRawPlan)
    (hencode : AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      hostTable structTable symPlan = some plan)
    (hcheck : AverCert.PlanCheck.checkExprFragmentRawPlan plan = true)
    (instrs : List WInstr)
    (hlower : lowerBlock C plan.body = some instrs)
    (self nlocals fuel : Nat)
    (hself : code self = some ⟨plan.params.length, nlocals, instrs⟩)
    (inputs : List WVal)
    (hinputArity : inputs.length = plan.params.length)
    (hcalls : blockCallsOK host (fun g => (code g).map (fun c => c.arity))
      plan.body)
    (modelLocals : List WVal) (result : WVal)
    (heval : evalSymRawPlan hostTable structTable host
      (fun g => (code g).map (fun c => c.arity))
      (fun g args => wFuncN code host fuel g args)
      C symPlan
      (initLocals ⟨plan.params.length, nlocals, instrs⟩ inputs) =
      some (.ok modelLocals [result])) :
    wFuncN code host (fuel + 1) self inputs = some result := by
  have hevalBlock : runBlock host
      (fun g => (code g).map (fun c => c.arity))
      (fun g args => wFuncN code host fuel g args)
      C plan.body
      (initLocals ⟨plan.params.length, nlocals, instrs⟩ inputs) =
      some (.ok modelLocals [result]) := by
    simp only [evalSymRawPlan, hencode] at heval
    exact heval
  change runBlockFuel host
      (fun g => (code g).map (fun (c : WCode) => c.arity))
      (fun g args => wFuncN code host fuel g args)
      maxFuel C plan.body
      (initLocals ⟨plan.params.length, nlocals, instrs⟩ inputs) =
      some (.ok modelLocals [result]) at hevalBlock
  have hwrun := (mutualCorrect maxFuel).2 host
    (fun g => (code g).map (fun (c : WCode) => c.arity))
    (fun g args => wFuncN code host fuel g args)
    C plan.body instrs hcalls hlower
    (initLocals ⟨plan.params.length, nlocals, instrs⟩ inputs)
    (.ok modelLocals [result]) hevalBlock
  simp [wFuncN, hself, hwrun]

/-! The source comparison bridge, stated at exactly the honest domain. -/

def sourceIntCmp : SymIntCmp -> Int -> Int -> Bool
  | .eq, n, k => n = k
  | .lt, n, k => n < k
  | .le, n, k => n <= k
  | .ge, n, k => n >= k

def smallCmpInstr : SymIntCmp -> WInstr
  | .eq => .i64Eq
  | .lt => .i64LtS
  | .le => .i64LeS
  | .ge => .i64GeS

def bigCmpInstrs (C : Nat) : SymIntCmp -> List WInstr
  | .eq => [.i32Const 0]
  | .lt => [.localGet 0, .structGet C 2, .i32Const 0, .i32LtS]
  | .le => [.localGet 0, .structGet C 2, .i32Const 0, .i32LtS]
  | .ge => [.localGet 0, .structGet C 2, .i32Const 0, .i32GtS]

def intConstCmpInstrs (C : Nat) (op : SymIntCmp) (k : Int) : List WInstr :=
  [.localGet 0, .structGet C 1, .refIsNull,
   .ifElse
     [.localGet 0, .structGet C 0, .i64Const k, smallCmpInstr op]
     (bigCmpInstrs C op)]

/-- All four source integer comparisons agree with their representation
    lowering on `carrierSmall`.  The `CarrierSpec` appears in the codomain
    relation, while the executable premise is deliberately not widened to an
    arbitrary `S.Repr`. -/
theorem carrierSmall_intConstCmp_bridge {C : Nat} (S : CarrierSpec C)
    (op : SymIntCmp) (n k : Int) :
    wRunF (fun _ => none) (fun _ => none) (fun _ _ => none)
      (intConstCmpInstrs C op k) [carrierSmall C n] [] =
        some (.ok [carrierSmall C n] [b32 (sourceIntCmp op n k)]) /\
    boolRepr S (sourceIntCmp op n k) (b32 (sourceIntCmp op n k)) := by
  cases op <;> simp [intConstCmpInstrs, smallCmpInstr, bigCmpInstrs,
    sourceIntCmp, wRunF, carrierSmall, b32, boolRepr]



end V3ExprFragmentGeneric
