import Lake
open Lake DSL

package «certprelude» where
  version := v!"0.1.0"

@[default_target]
lean_lib «CertPrelude» where
  srcDir := "."
  roots := #[`CertPrelude, `CertPreludeSanity, `CertDecode, `SchemaCore,
    `SchemaSanity,
    `PlanCheck, `PlanLower, `PlanBytes, `WasmSlice, `Wasip2Envelope,
    `ExprFragmentAccepted,
    `AcceptedArtifactCore, `IntDispatchSoundness, `EnvelopeLowering,
    `EnvelopeBridge, `EnvelopeBridgeConstruct, `EnvelopeBridgeString,
    `EnvelopeAcceptTransport, `ConstructVerbatimSoundness, `StringSoundness,
    `WidenedEnvelope, `DeclaredIndexEnvelope, `DeclaredEnvelopeAcceptTransport,
    `ClaimAxes, `ExprFragmentSemantics, `InterpreterSequencing,
    `RecordComputeBridge,
    `ExprFragmentSoundness, `FieldProjectionSoundness, `StandardFace,
    `RecursionSoundness, `MutualRecursionSoundness,
    `CompositionSoundness, `AcceptanceSoundnessCore, `DischargeExprFragment,
    `DischargeFieldProjection, `DischargeConstruct, `DischargeVerbatim,
    `DischargeString, `DischargeIntDispatch, `DischargeRecursion,
    `DischargeComposition, `AcceptanceSoundness, `ArithTemplateDerisk]
