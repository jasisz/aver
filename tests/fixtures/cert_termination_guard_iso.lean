import ArtifactCertificate

open CertPrelude AverCert AverCert.Schema
set_option linter.unnecessarySimpa false

def honestWitness : TerminationWitness :=
  { measure := .intNatAbs 0, descent := -1 }
def wrongMeasureWitness : TerminationWitness :=
  { measure := .intNatAbs 1, descent := -1 }
def wrongDescentWitness : TerminationWitness :=
  { measure := .intNatAbs 0, descent := 1 }

def honestTotalOb : Obligation :=
  { AverCert.sumFromOb with
      policy := .simulatesModelTotally
      termination? := some honestWitness }
def wrongMeasureOb : Obligation :=
  { honestTotalOb with termination? := some wrongMeasureWitness }
def wrongDescentOb : Obligation :=
  { honestTotalOb with termination? := some wrongDescentWitness }
def missingWitnessOb : Obligation :=
  { honestTotalOb with termination? := none }

-- Literal `recursionPlanAccepted` copy with only its `checkTerm` conjunct removed.
def recursionPlanAcceptedWithoutCheckTerm
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (hostTable : List (HostRole × Nat))
    (plan : RecursionRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkRecursionRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerRecursionBody carrier plan = some body ∧
      AverCert.PlanBytes.lowerRecursionCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.exactFuncBindingForExport
        modBytes modLen exportNameBytes codeEntry = some binding ∧
      binding.funcIdx = obligation.self ∧
      AverCert.PlanCheck.checkRecursionPlanShape binding.funcIdx hostTable
        obligation.totalityRole plan = true ∧
      AverCert.WasmSlice.funcTypeMatches
        modBytes modLen binding.typeIdx plan.params.length carrier = true ∧
      AverCert.WasmSlice.hostTableFuncTypesMatch
        modBytes modLen carrier hostTable = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.params.length,
               nlocals := AverCert.AcceptedArtifact.recursionNLocals plan, body := body }

abbrev modBytes := AverCert.ArtifactBytes.modBytes
abbrev modLen := AverCert.ArtifactBytes.modLen
abbrev nameBytes : AverCert.WasmSlice.ByteSeq :=
  [115, 117, 109, 70, 114, 111, 109]
abbrev hostTable : List (HostRole × Nat) :=
  [(.box, 10), (.add, 11), (.sub, 12)]
abbrev plan := AverCert.Plans.sumFromRecursionPlan
abbrev accumulatorPlan := AverCert.Plans.countDownRecursionPlan

-- The honest witness passes; each hostile witness fails at `checkTerm` itself.
example : checkTerm plan honestWitness = true := rfl
example : checkTerm plan wrongMeasureWitness = false := rfl
example : checkTerm plan wrongDescentWitness = false := rfl

-- The generalized checker reads the counter from argument zero while allowing
-- the accumulator in argument one to be threaded through the recursive call.
-- Swapping the measure to the accumulator or reversing descent still fails.
example : checkTerm accumulatorPlan honestWitness = true := rfl
example : checkTerm accumulatorPlan wrongMeasureWitness = false := rfl
example : checkTerm accumulatorPlan wrongDescentWitness = false := rfl

theorem honestAccepted : AverCert.AcceptedArtifact.recursionPlanAccepted
    modBytes modLen nameBytes "sumFrom" 2 hostTable plan AverCert.sumFromOb := by
  have hfrags : AverCert.AcceptedArtifact.acceptedFragments AverCert.Artifact.data :=
    AverCert.Artifact.certificate.2.2.2.2.2.2.2.2
  have hrec : AverCert.AcceptedArtifact.acceptedRecursionFragments AverCert.Artifact.data :=
    hfrags.2.2.2.2.1
  simpa [AverCert.AcceptedArtifact.acceptedRecursionFragments,
    AverCert.AcceptedArtifact.recursionClaimsAccepted,
    AverCert.AcceptedArtifact.recursionClaimAccepted,
    AverCert.AcceptedArtifact.recursionPlanForExport,
    AverCert.AcceptedArtifact.namedPlanForExport,
    AverCert.Artifact.data, AverCert.Artifact.recursionClaims,
    AverCert.manifest, plan, modBytes, modLen, nameBytes, hostTable] using hrec.1

theorem weakenedAccepts (obligation : Obligation)
    (hfields : obligation.export_ = AverCert.sumFromOb.export_ ∧
      obligation.carrier = AverCert.sumFromOb.carrier ∧
      obligation.totalityRole = AverCert.sumFromOb.totalityRole ∧
      obligation.self = AverCert.sumFromOb.self ∧
      obligation.code = AverCert.sumFromOb.code) :
    recursionPlanAcceptedWithoutCheckTerm
      modBytes modLen nameBytes "sumFrom" 2 hostTable plan obligation := by
  rcases honestAccepted with ⟨hexport, hcarrier, hraw, _hterm, hrest⟩
  rcases hfields with ⟨hexport', hcarrier', htotality', hself', hcode'⟩
  refine ⟨?_, ?_, hraw, ?_⟩
  · simpa [hexport'] using hexport
  · simpa [hcarrier'] using hcarrier
  · simpa [htotality', hself', hcode'] using hrest

-- The one-conjunct-weakened copy accepts both hostile obligations.
example : recursionPlanAcceptedWithoutCheckTerm
    modBytes modLen nameBytes "sumFrom" 2 hostTable plan wrongMeasureOb :=
  weakenedAccepts wrongMeasureOb ⟨rfl, rfl, rfl, rfl, rfl⟩
example : recursionPlanAcceptedWithoutCheckTerm
    modBytes modLen nameBytes "sumFrom" 2 hostTable plan wrongDescentOb :=
  weakenedAccepts wrongDescentOb ⟨rfl, rfl, rfl, rfl, rfl⟩

-- The real predicate rejects exactly where the weakened copy differs.
example : ¬ AverCert.AcceptedArtifact.recursionPlanAccepted
    modBytes modLen nameBytes "sumFrom" 2 hostTable plan wrongMeasureOb := by
  intro h
  rcases h with ⟨_, _, _, hterm, _⟩
  contradiction
example : ¬ AverCert.AcceptedArtifact.recursionPlanAccepted
    modBytes modLen nameBytes "sumFrom" 2 hostTable plan wrongDescentOb := by
  intro h
  rcases h with ⟨_, _, _, hterm, _⟩
  contradiction
example : ¬ AverCert.AcceptedArtifact.recursionPlanAccepted
    modBytes modLen nameBytes "sumFrom" 2 hostTable plan missingWitnessOb := by
  intro h
  rcases h with ⟨_, _, _, hterm, _⟩
  contradiction

-- Witness mutation cannot change any byte-derived obligation binding.
example : honestTotalOb.self = wrongMeasureOb.self ∧
    honestTotalOb.carrier = wrongMeasureOb.carrier ∧
    honestTotalOb.code = wrongMeasureOb.code ∧
    honestTotalOb.host = wrongMeasureOb.host := by
  exact ⟨rfl, rfl, rfl, rfl⟩
example : honestTotalOb.self = wrongDescentOb.self ∧
    honestTotalOb.carrier = wrongDescentOb.carrier ∧
    honestTotalOb.code = wrongDescentOb.code ∧
    honestTotalOb.host = wrongDescentOb.host := by
  exact ⟨rfl, rfl, rfl, rfl⟩
