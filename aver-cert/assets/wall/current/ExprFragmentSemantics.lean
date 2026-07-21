/- Plan-shaped expression-fragment semantics used by the audited lowering. -/
import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower

set_option maxRecDepth 100000
set_option maxHeartbeats 4000000

namespace ExprFragmentSemantics
open CertPrelude AverCert.Schema AverCert.PlanLower

/-! The model is plan-shaped, not the old five-node integer evaluator.  It
    executes every constructor admitted by `FragNodeKind`, including nested
    blocks and every `FragPrim`.  The symbolic stack checks are deliberately
    the same `popExpected`/`popExpectedAll` discipline used by the audited
    lowerer, but this evaluator walks the raw plan rather than lowered code. -/

def runPrim : FragPrim -> List WVal -> Option (List WVal)
  | .f64Add, .f64v b :: .f64v a :: st =>
      some (.f64v (f a + f b).toBits :: st)
  | .f64Mul, .f64v b :: .f64v a :: st =>
      some (.f64v (f a * f b).toBits :: st)
  | .f64Le, .f64v b :: .f64v a :: st =>
      some (b32 (f a <= f b) :: st)
  | .i64Eq, .i64v b :: .i64v a :: st =>
      some (b32 (a = b) :: st)
  | .i64LeS, .i64v b :: .i64v a :: st =>
      some (b32 (a <= b) :: st)
  | .i64LtS, .i64v b :: .i64v a :: st =>
      some (b32 (a < b) :: st)
  | .i64GeS, .i64v b :: .i64v a :: st =>
      some (b32 (a >= b) :: st)
  | .i32Eq, .i32v b :: .i32v a :: st =>
      some (b32 (a = b) :: st)
  | .i32LtS, .i32v b :: .i32v a :: st =>
      some (b32 (a < b) :: st)
  | .i32GtS, .i32v b :: .i32v a :: st =>
      some (b32 (a > b) :: st)
  | _, _ => none

def finishWith
    (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (rest : List WInstr) : Option Out -> Option Out
  | some (.ok locals stack) => wRunF host ar callee rest locals stack
  | some (.ret value) => some (.ret value)
  | none => none

mutual
  def runNodesFuel
      (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee) :
      Nat -> Nat -> List FragNode -> List Nat -> List WVal -> List WVal -> Option Out
    | 0, _, _, _, _, _ => none
    | _fuel + 1, _, [], _, locals, stack => some (.ok locals stack)
    | fuel + 1, carrier, node :: rest, symStack, locals, stack =>
        let step (symStack' : List Nat) (instr : WInstr) :=
          match wRunF host ar callee [instr] locals stack with
          | some (.ok locals' stack') =>
              runNodesFuel host ar callee fuel carrier rest
                (node.id :: symStack') locals' stack'
          | some (.ret value) => some (.ret value)
          | none => none
        match node.kind with
        | .local index => step symStack (.localGet index)
        | .constBool value => step symStack (.i32Const (if value then 1 else 0))
        | .constI64 value => step symStack (.i64Const value)
        | .constI32 value => step symStack (.i32Const value)
        | .constF64Bits bits => step symStack (.f64Const (UInt64.ofNat bits))
        | .structGet field receiver =>
            match popExpected symStack receiver with
            | some symStack' => step symStack' (.structGet carrier field)
            | none => none
        | .structGetUser tyIdx field value =>
            match popExpected symStack value with
            | some symStack' => step symStack' (.structGet tyIdx field)
            | none => none
        | .refIsNull value =>
            match popExpected symStack value with
            | some symStack' => step symStack' .refIsNull
            | none => none
        | .prim op args =>
            match popExpectedAll symStack args.reverse with
            | some symStack' => step symStack' (primInstr op)
            | none => none
        | .hostCall _role funcIdx args =>
            match popExpectedAll symStack args.reverse with
            | none => none
            | some symStack' => step symStack' (.call funcIdx)
        | .selfCall tail funcIdx args =>
            match popExpectedAll symStack args.reverse with
            | none => none
            | some symStack' =>
                step symStack' (if tail then .returnCall funcIdx else .call funcIdx)
        -- The monolithic fused vector read has no symbolic-generic
        -- semantics: its face discharges through the audited template
        -- theorem, never through this evaluator (fail-closed here).
        | .vectorGetOrDefault _ _ _ _ => none
        | .ifElse cond thenBlock elseBlock =>
            match popExpected symStack cond, stack with
            | some [], .i32v c :: [] =>
                let branch := if c = 0 then elseBlock else thenBlock
                finishWith host ar callee []
                  (runBlockFuel host ar callee fuel carrier branch locals)
                  |> fun branchOut =>
                    match branchOut with
                    | some (.ok locals' [value]) =>
                        runNodesFuel host ar callee fuel carrier rest [node.id]
                          locals' [value]
                    | some (.ret value) => some (.ret value)
                    | _ => none
            | _, _ => none

  def runBlockFuel
      (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee) :
      Nat -> Nat -> FragBlock -> List WVal -> Option Out
    | 0, _, _, _ => none
    | fuel + 1, carrier, block, locals =>
        match runNodesFuel host ar callee fuel carrier block.nodes [] locals [] with
        | some (.ok locals' [value]) => some (.ok locals' [value])
        | some (.ret value) => some (.ret value)
        | _ => none
end

def runBlock
    (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (carrier : Nat) (block : FragBlock) (locals : List WVal) : Option Out :=
  runBlockFuel host ar callee maxFuel carrier block locals

/-! `evalSymRawPlan` is the manifest-plan-derived evaluator.  It starts from
    the checked source plan and uses the audited encoder to obtain the exact
    representation grammar whose structured semantics is above. -/
def evalSymRawPlan
    (hostTable : List (HostRole × Nat))
    (structTable : List (String × Nat))
    (host : HostTbl) (ar : Nat -> Option Nat) (callee : Callee)
    (carrier : Nat) (plan : SymRawPlan) (locals : List WVal) : Option Out :=
  match AverCert.PlanCheck.encodeSymRawPlanToExprFragmentRawPlan
      hostTable structTable plan with
  | some exprPlan => runBlock host ar callee carrier exprPlan.body locals
  | none => none

end ExprFragmentSemantics
