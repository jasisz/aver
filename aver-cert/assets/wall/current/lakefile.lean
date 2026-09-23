import Lake
open Lake DSL

package «certprelude» where
  version := v!"0.1.0"

@[default_target]
lean_lib «CertPrelude» where
  srcDir := "."
  roots := #[`CertPrelude, `CertPreludeSanity, `CertDecode, `SchemaBase, `SchemaCore,
    `SchemaSanity, `WasmSlice, `Wasip2Envelope, `ArithTemplateDerisk,
    `InterpreterSequencing, `Grammar, `GrammarLower, `GrammarSound, `GrammarTotal,
    `TypeTable, `AcceptedArtifactCore, `ClaimAxes, `AcceptanceSoundnessCore,
    `AcceptanceSoundness, `GrammarBridge, `ModelPrelude]
