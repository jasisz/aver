import Lake
open Lake DSL

package «sumacc» where
  version := v!"0.1.0"

@[default_target]
lean_lib «SumAcc» where
  srcDir := "."
  roots := #[`SumAcc, `AverCommon]
