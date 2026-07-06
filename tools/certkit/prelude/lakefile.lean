import Lake
open Lake DSL

package «certprelude» where
  version := v!"0.1.0"

@[default_target]
lean_lib «CertPrelude» where
  srcDir := "."
  roots := #[`CertPrelude, `CertPreludeSanity, `CertDecode]
