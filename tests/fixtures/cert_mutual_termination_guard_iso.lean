import Artifact

open CertPrelude AverCert AverCert.Schema

def honestMutualWitness : TerminationWitness :=
  { measure := .intNatAbs 0, descent := -1 }
def wrongMutualWitness : TerminationWitness :=
  { measure := .intNatAbs 0, descent := 1 }

def wrongEvenOb : Obligation :=
  { AverCert.isEvenOb with termination? := some wrongMutualWitness }

-- Literal `mutualPlanAccepted` copy with only its termination conjunct removed.
def mutualPlanAcceptedWithoutCheckTerm
    (modBytes modLen : Nat)
    (exportNameBytes : AverCert.WasmSlice.ByteSeq)
    (exportName : String)
    (carrier : Nat)
    (memberSet : List Nat)
    (hostTable : List (HostRole × Nat))
    (plan : MutualRawPlan)
    (obligation : Obligation) : Prop :=
  obligation.export_ = exportName ∧
    obligation.carrier = carrier ∧
    AverCert.PlanCheck.checkMutualRawPlan plan = true ∧
    ∃ body codeEntry binding,
      AverCert.PlanLower.lowerMutualBody carrier plan = some body ∧
      AverCert.PlanBytes.lowerMutualCodeEntry carrier plan = some codeEntry ∧
      AverCert.WasmSlice.codeEntryForExport modBytes modLen exportNameBytes = some codeEntry ∧
      AverCert.WasmSlice.funcBindingForExport modBytes modLen exportNameBytes = some binding ∧
      binding.funcIdx = obligation.self ∧
      binding.codeEntry = codeEntry ∧
      AverCert.PlanCheck.checkMutualPlanShape memberSet hostTable plan = true ∧
      AverCert.WasmSlice.funcTypeMatches
        modBytes modLen binding.typeIdx plan.params.length carrier = true ∧
      obligation.code binding.funcIdx =
        some { arity := plan.params.length,
               nlocals := AverCert.AcceptedArtifact.mutualNLocals plan, body := body }

abbrev mutualModBytes := AverCert.ArtifactBytes.modBytes
abbrev mutualModLen := AverCert.ArtifactBytes.modLen
abbrev evenNameBytes : AverCert.WasmSlice.ByteSeq := [105, 115, 69, 118, 101, 110]
abbrev mutualHostTable : List (HostRole × Nat) := [(.box, 7), (.sub, 9)]
abbrev mutualMemberSet : List Nat := [1, 2]
abbrev evenPlan := AverCert.Plans.isEvenMutualPlan

-- Both honest members carry the same accepted measure; one hostile member
-- fails at the termination decision while the sibling remains unchanged.
example : checkTermMutual evenPlan honestMutualWitness = true := rfl
example : checkTermMutual AverCert.Plans.isOddMutualPlan honestMutualWitness = true := rfl
example : checkTermMutual evenPlan wrongMutualWitness = false := rfl
example : wrongEvenOb.termination? ≠ AverCert.isOddOb.termination? := by decide

theorem honestMutualAccepted : AverCert.AcceptedArtifact.mutualPlanAccepted
    mutualModBytes mutualModLen evenNameBytes "isEven" 2 mutualMemberSet
    mutualHostTable evenPlan AverCert.isEvenOb := by
  have hfrags : AverCert.AcceptedArtifact.acceptedFragments AverCert.Artifact.data :=
    AverCert.Artifact.certificate.2.2.2.2.2
  have hmutual : AverCert.AcceptedArtifact.acceptedMutualRecursionFragments AverCert.Artifact.data :=
    hfrags.2.2.2.2.2.1
  simpa [AverCert.AcceptedArtifact.acceptedMutualRecursionFragments,
    AverCert.AcceptedArtifact.mutualRecursionClaimsAccepted,
    AverCert.AcceptedArtifact.mutualRecursionClaimAccepted,
    AverCert.AcceptedArtifact.mutualPlanForExport,
    AverCert.Artifact.data, AverCert.Artifact.mutualRecursionClaims,
    AverCert.manifest, evenPlan, mutualModBytes, mutualModLen, evenNameBytes,
    mutualHostTable, mutualMemberSet] using hmutual.1.1

theorem weakenedMutualAccepts : mutualPlanAcceptedWithoutCheckTerm
    mutualModBytes mutualModLen evenNameBytes "isEven" 2 mutualMemberSet
    mutualHostTable evenPlan wrongEvenOb := by
  rcases honestMutualAccepted with ⟨hexport, hcarrier, hraw, _hterm, hrest⟩
  exact ⟨hexport, hcarrier, hraw, by simpa [wrongEvenOb] using hrest⟩

-- The shipped predicate rejects exactly at the one conjunct omitted above.
example : ¬ AverCert.AcceptedArtifact.mutualPlanAccepted
    mutualModBytes mutualModLen evenNameBytes "isEven" 2 mutualMemberSet
    mutualHostTable evenPlan wrongEvenOb := by
  intro h
  rcases h with ⟨_, _, _, hterm, _⟩
  contradiction

-- Witness mutation cannot alter byte-derived bindings, and only one member was
-- made hostile: the other obligation remains the honest total obligation.
example : wrongEvenOb.self = AverCert.isEvenOb.self ∧
    wrongEvenOb.carrier = AverCert.isEvenOb.carrier ∧
    wrongEvenOb.code = AverCert.isEvenOb.code ∧
    wrongEvenOb.host = AverCert.isEvenOb.host := by
  exact ⟨rfl, rfl, rfl, rfl⟩
example : AverCert.isOddOb.policy = .simulatesModelTotally ∧
    AverCert.isOddOb.termination? = some honestMutualWitness := by
  exact ⟨rfl, rfl⟩
