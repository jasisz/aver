/- V3 STRING FAMILY PORT -- generic String.eq and String.concat certificates.

   The model is explicit and the helper implementations remain abstract.  The
   only facts used about them are the named contracts from SchemaCore.holds.
   Literal strings are interpreted exactly as array.new_data does. -/
import CertPrelude
import SchemaCore
import PlanCheck
import PlanLower

set_option maxRecDepth 100000

namespace V3String
open CertPrelude AverCert.Schema AverCert.PlanLower

/-! ## Model and reusable invariant -/

def byteArray (ty : Nat) (bytes : List Nat) : WVal :=
  .arr ty (bytes.map (fun b => .i32v (Int.ofNat b)))

def evalEqResult (stringTy : Nat) (input : WVal) : StringEqResult → WVal
  | .input => input
  | .literal chunk => byteArray stringTy chunk.bytes

def evalStringEq (stringTy : Nat) (plan : StringEqRawPlan) (input : WVal) : WVal :=
  if stringEqW input (byteArray stringTy plan.needle.bytes) then
    evalEqResult stringTy input plan.hit
  else
    evalEqResult stringTy input plan.default

def evalConcatChunks (stringTy : Nat) : List StringConcatChunk → List WVal
  | [] => []
  | chunk :: rest => byteArray stringTy chunk.bytes :: evalConcatChunks stringTy rest

def evalConcatParts (stringTy : Nat) (plan : StringConcatRawPlan) (input : WVal) : List WVal :=
  evalConcatChunks stringTy plan.prefixes ++
    [input] ++ evalConcatChunks stringTy plan.suffixes

def evalStringConcat (stringTy containerTy : Nat)
    (plan : StringConcatRawPlan) (input : WVal) : WVal :=
  (stringConcatW stringTy (.arr containerTy (evalConcatParts stringTy plan input))).getD .null

def checkStringEqFamily (namedIdx citedIdx : Nat) (plan : StringEqRawPlan) : Bool :=
  AverCert.PlanCheck.checkStringEqRawPlan plan && decide (citedIdx = namedIdx)

def checkStringConcatFamily (namedIdx citedIdx : Nat) (plan : StringConcatRawPlan) : Bool :=
  AverCert.PlanCheck.checkStringConcatRawPlan plan && decide (citedIdx = namedIdx)

inductive ValKind where
  | exact (v : WVal)
  | arr (ty : Nat) (bytes : List Nat)

def ValOK : ValKind → WVal → Prop
  | .exact v, w => w = v
  | .arr ty bytes, w => w = byteArray ty bytes

def StackOK : List ValKind → List WVal → Prop
  | [], [] => True
  | k :: ks, w :: ws => ValOK k w ∧ StackOK ks ws
  | _, _ => False

theorem byteArray_eq (ty : Nat) (bytes : List Nat) :
    .arr ty (bytes.map (.i32v ∘ Int.ofNat)) = byteArray ty bytes := by
  simp [byteArray, Function.comp_def]

theorem evalConcatChunks_length (stringTy : Nat) : ∀ chunks,
    (evalConcatChunks stringTy chunks).length = chunks.length := by
  intro chunks
  induction chunks <;> simp [evalConcatChunks, *]

theorem runConcatChunks
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (stringTy : Nat) (locals : List WVal) :
    ∀ (chunks : List StringConcatChunk) (st : List WVal) (rest : List WInstr),
      wRunF host ar callee
          (lowerStringConcatChunks stringTy chunks ++ rest) locals st =
        wRunF host ar callee rest locals
          ((evalConcatChunks stringTy chunks).reverse ++ st) := by
  intro chunks
  induction chunks with
  | nil => intro st rest; rfl
  | cons chunk chunks ih =>
      intro st rest
      simp [lowerStringConcatChunks, lowerStringConcatChunk, evalConcatChunks,
        wRunF, byteArray, Function.comp_def, ih, List.append_assoc]

theorem runEqResult
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (stringTy : Nat) (input : WVal) (locals : List WVal)
    (hlocal : locals[0]? = some input) (st : List WVal) :
    ∀ (result : StringEqResult) (rest : List WInstr),
      wRunF host ar callee (lowerStringEqResult stringTy result ++ rest) locals st =
        wRunF host ar callee rest locals (evalEqResult stringTy input result :: st) := by
  intro result rest
  cases result with
  | input => simp [lowerStringEqResult, evalEqResult, wRunF, hlocal]
  | literal chunk =>
      simp [lowerStringEqResult, lowerStringEqChunk, evalEqResult, byteArray,
        wRunF, Function.comp_def]

theorem runEqTail
    (host : HostTbl) (ar : Nat → Option Nat) (callee : Callee)
    (stringTy stringEqFuncIdx : Nat) (stringEq : List WVal → Option WVal)
    (hhost : host stringEqFuncIdx = some (2, stringEq))
    (hStringEq : ∀ a b w,
      stringEq [a, b] = some w → w = b32 (stringEqW a b))
    (plan : StringEqRawPlan) (input : WVal) (locals : List WVal)
    (hlocal : locals[0]? = some input) :
    wRunF host ar callee
      (lowerStringEqChunk stringTy plan.needle ++
        [.call stringEqFuncIdx,
          .ifElse (lowerStringEqResult stringTy plan.hit)
            (lowerStringEqResult stringTy plan.default)])
      locals [input] =
      match stringEq [input, byteArray stringTy plan.needle.bytes] with
      | none => none
      | some _ => some (.ok locals [evalStringEq stringTy plan input]) := by
  simp only [lowerStringEqChunk, List.cons_append, List.nil_append, wRunF,
    hhost, popArgs, Function.comp_def]
  simp [popArgs]
  simp only [byteArray]
  cases hcall : stringEq [input,
      .arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b)))] with
  | none => simp [hcall]
  | some got =>
      have hgot := hStringEq input
        (.arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b)))) got hcall
      cases heq : stringEqW input
        (.arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b))))
      · have hr := runEqResult host ar callee stringTy input locals hlocal []
          plan.default []
        have heq' : stringEqW input
            (.arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b)))) = false := heq
        have hr' : wRunF host ar callee
            (lowerStringEqResult stringTy plan.default) locals [] =
            some (.ok locals [evalEqResult stringTy input plan.default]) := by
          simpa [wRunF] using hr
        rw [hgot]
        simp only [b32, heq', Bool.false_eq_true, ↓reduceIte, wRunF]
        rw [hr']
        simp [evalStringEq, byteArray]
        intro hc
        have hc' : stringEqW input
            (.arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b)))) = true := by
          simpa using hc
        rw [heq'] at hc'
        simp at hc'
      · have hr := runEqResult host ar callee stringTy input locals hlocal []
          plan.hit []
        have heq' : stringEqW input
            (.arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b)))) = true := heq
        have hr' : wRunF host ar callee
            (lowerStringEqResult stringTy plan.hit) locals [] =
            some (.ok locals [evalEqResult stringTy input plan.hit]) := by
          simpa [wRunF] using hr
        rw [hgot]
        simp only [b32, heq', ↓reduceIte, wRunF]
        simp
        rw [hr']
        simp [evalStringEq, byteArray]
        intro hc
        have hc' : stringEqW input
            (.arr stringTy (plan.needle.bytes.map (fun b => .i32v (Int.ofNat b)))) = false := by
          simpa using hc
        rw [heq'] at hc'
        simp at hc'

theorem popConcatStack (stringTy : Nat) (prefixes suffixes : List StringConcatChunk)
    (v : WVal) :
    popArgs (prefixes.length + 1 + suffixes.length)
        ((evalConcatChunks stringTy suffixes).reverse ++
          v :: (evalConcatChunks stringTy prefixes).reverse) =
      some (evalConcatChunks stringTy prefixes ++ [v] ++
        evalConcatChunks stringTy suffixes, []) := by
  have hlen :
      ((evalConcatChunks stringTy suffixes).reverse ++
        v :: (evalConcatChunks stringTy prefixes).reverse).length =
        prefixes.length + 1 + suffixes.length := by
    simp [evalConcatChunks_length]
    omega
  have ht : List.take (prefixes.length + 1 + suffixes.length)
      ((evalConcatChunks stringTy suffixes).reverse ++
        v :: (evalConcatChunks stringTy prefixes).reverse) =
      ((evalConcatChunks stringTy suffixes).reverse ++
        v :: (evalConcatChunks stringTy prefixes).reverse) := by
    rw [← hlen]
    exact List.take_length
  simp [popArgs, hlen, ht, List.append_assoc]

/-! ## Generic String.concat certificate -/

theorem generic_string_concat_certified
    (stringTy containerTy concatFuncIdx : Nat)
    (plan : StringConcatRawPlan)
    (code : CodeTbl) (host : HostTbl) (self : Nat)
    (stringConcat : Nat → List WVal → Option WVal)
    (hStringConcat : ∀ resultTy parts c,
      stringConcat resultTy [parts] = some c → stringConcatW resultTy parts = some c)
    (hcheck : AverCert.PlanCheck.checkStringConcatRawPlan plan = true)
    (instrs : List WInstr)
    (hlow : lowerStringConcatBody stringTy containerTy concatFuncIdx plan = some instrs)
    (hself : code self = some ⟨1, 1, instrs⟩)
    (hhost : host concatFuncIdx = some (1, stringConcat stringTy)) :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN code host (fuel + 1) self [v] = some w →
      w = evalStringConcat stringTy containerTy plan v := by
  intro fuel v w hrun
  have hbody : instrs =
      lowerStringConcatChunks stringTy plan.prefixes ++ [.localGet 0] ++
      lowerStringConcatChunks stringTy plan.suffixes ++
      [.arrayNewFixed containerTy (plan.prefixes.length + 1 + plan.suffixes.length),
        .call concatFuncIdx] := by
    simp [lowerStringConcatBody, hcheck] at hlow
    simpa [List.append_assoc] using hlow.symm
  subst instrs
  simp only [wFuncN, hself, initLocals] at hrun
  simp only [List.append_assoc] at hrun
  rw [runConcatChunks] at hrun
  simp [wRunF] at hrun
  rw [runConcatChunks] at hrun
  simp only [wRunF, hhost] at hrun
  rw [popConcatStack] at hrun
  simp only [evalConcatParts, List.singleton_append] at hrun
  cases hcall : stringConcat stringTy
      [.arr containerTy (evalConcatParts stringTy plan v)] with
  | none =>
      have hcall' : stringConcat stringTy
          [.arr containerTy (evalConcatChunks stringTy plan.prefixes ++
            v :: evalConcatChunks stringTy plan.suffixes)] = none := by
        simpa [evalConcatParts, List.append_assoc] using hcall
      simp [popArgs, hcall'] at hrun
  | some got =>
      have hcall' : stringConcat stringTy
          [.arr containerTy (evalConcatChunks stringTy plan.prefixes ++
            v :: evalConcatChunks stringTy plan.suffixes)] = some got := by
        simpa [evalConcatParts, List.append_assoc] using hcall
      have hgot : stringConcatW stringTy
          (.arr containerTy (evalConcatParts stringTy plan v)) = some got :=
        hStringConcat stringTy (.arr containerTy (evalConcatParts stringTy plan v)) got hcall
      simp [popArgs, hcall'] at hrun
      subst w
      simp [evalStringConcat, hgot]

/-! ## Generic String.eq certificate -/

theorem generic_string_eq_certified
    (stringTy stringEqFuncIdx : Nat)
    (plan : StringEqRawPlan)
    (code : CodeTbl) (host : HostTbl) (self : Nat)
    (stringEq : List WVal → Option WVal)
    (hStringEq : ∀ a b w,
      stringEq [a, b] = some w → w = b32 (stringEqW a b))
    (hcheck : AverCert.PlanCheck.checkStringEqRawPlan plan = true)
    (instrs : List WInstr)
    (hlow : lowerStringEqBody stringTy stringEqFuncIdx plan = some instrs)
    (hself : code self = some ⟨1, 2, instrs⟩)
    (hhost : host stringEqFuncIdx = some (2, stringEq)) :
    ∀ (fuel : Nat) (v w : WVal),
      wFuncN code host (fuel + 1) self [v] = some w →
      w = evalStringEq stringTy plan v := by
  intro fuel v w hrun
  have hbody : instrs =
      [.localGet 0, .localSet 1, .localGet 1, .refCast stringTy] ++
      lowerStringEqChunk stringTy plan.needle ++
      [.call stringEqFuncIdx,
        .ifElse (lowerStringEqResult stringTy plan.hit)
          (lowerStringEqResult stringTy plan.default)] := by
    simp [lowerStringEqBody, hcheck] at hlow
    exact hlow.symm
  subst instrs
  cases v with
  | i32v n => simp [wFuncN, wRunF, hself, initLocals, List.set] at hrun
  | i64v n => simp [wFuncN, wRunF, hself, initLocals, List.set] at hrun
  | f64v bits => simp [wFuncN, wRunF, hself, initLocals, List.set] at hrun
  | null => simp [wFuncN, wRunF, hself, initLocals, List.set] at hrun
  | structv ty fields =>
      by_cases hty : ty = stringTy
      · subst ty
        simp only [wFuncN, hself] at hrun
        simp [List.append_assoc, wRunF, initLocals, List.set] at hrun
        rw [runEqTail host (fun g => (code g).map (·.arity))
          (fun g as => wFuncN code host fuel g as) stringTy stringEqFuncIdx
          stringEq hhost hStringEq plan (.structv stringTy fields)
          [.structv stringTy fields, .structv stringTy fields, .null] rfl] at hrun
        cases hc : stringEq [.structv stringTy fields,
          byteArray stringTy plan.needle.bytes] with
        | none => simp [hc] at hrun
        | some got => simp [hc] at hrun; exact hrun.symm
      · simp [wFuncN, wRunF, hself, initLocals, List.set, hty] at hrun
  | arr ty elems =>
      by_cases hty : ty = stringTy
      · subst ty
        simp only [wFuncN, hself] at hrun
        simp [List.append_assoc, wRunF, initLocals, List.set] at hrun
        rw [runEqTail host (fun g => (code g).map (·.arity))
          (fun g as => wFuncN code host fuel g as) stringTy stringEqFuncIdx
          stringEq hhost hStringEq plan (.arr stringTy elems)
          [.arr stringTy elems, .arr stringTy elems, .null] rfl] at hrun
        cases hc : stringEq [.arr stringTy elems,
          byteArray stringTy plan.needle.bytes] with
        | none => simp [hc] at hrun
        | some got => simp [hc] at hrun; exact hrun.symm
      · simp [wFuncN, wRunF, hself, initLocals, List.set, hty] at hrun

#print axioms runConcatChunks
#print axioms runEqResult
#print axioms generic_string_concat_certified
#print axioms generic_string_eq_certified

end V3String
