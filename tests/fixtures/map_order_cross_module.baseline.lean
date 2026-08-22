import AverCommon
import MapKeys

open MapKeys

set_option linter.unusedVariables false

set_option maxRecDepth 1000000

set_option autoImplicit false

namespace MapOrderCrossModule

/-- The value paired with the lowest key, read through another module. -/
def firstTupleValue (m : List ((Int × Int) × Int)) : Int :=
  match MapKeys.floatValues m with
  | x :: rest => x
  | _ => 0

-- verify law firstTupleValue.firstIsLowestKeysValue: map iteration order is not exported — the proof model has no ordering for Tuple<Int, Int> keys, so the sequence such a map iterates in is not carried into the export; the program itself orders them the same way on every backend

end MapOrderCrossModule