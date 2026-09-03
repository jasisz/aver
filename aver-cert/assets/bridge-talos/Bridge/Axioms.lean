import Bridge.Bridge
import Bridge.EnvOfClaim
import Bridge.Coverage
import Bridge.Contracts
import Bridge.Adapter
import Bridge.Accepted
import Bridge.Tripwire

/-!
# Axiom audit

Every theorem of the bridge must rest on `propext`, `Classical.choice` and
`Quot.sound` only; `HostSimulation`/`HostSorts`/`HasTy` are premises of the
statements, never axioms. `lake build Bridge.Axioms` prints the lists.
-/

#print axioms Bridge.typed_run
#print axioms Bridge.bridge_hostCall
#print axioms Bridge.bridge_ifElse
#print axioms Bridge.bridge_structNew
#print axioms Bridge.bridge_i64Cmp
#print axioms Bridge.bridge_i32Cmp
#print axioms Bridge.declEntry?_lowerTypeDecl
#print axioms Bridge.hostRoleIdx?_slotLookup
#print axioms Bridge.envOfClaim_import_role
#print axioms Bridge.initSingleModuleConfig_synth
#print axioms Bridge.bridge_run
#print axioms Bridge.wFuncN_terminatesWith
#print axioms Bridge.wFuncN_TerminatesWith
#print axioms Bridge.checkBlockFuel_succ
#print axioms Bridge.HasTy_translates
#print axioms Bridge.cov
#print axioms Bridge.coverage
#print axioms Bridge.coverage_envOfClaim
#print axioms Bridge.Tripwire.translate_eq_none_of_out
#print axioms Bridge.hostTableBound_nodup
#print axioms Bridge.recordComputeSlots_getElem
#print axioms Bridge.HostSorts_of_contracts
#print axioms Bridge.reify_spec
#print axioms Bridge.readArg_of_R
#print axioms Bridge.HostSimulation_adapter
#print axioms Bridge.HostSimulation_recordCompute
#print axioms Bridge.planInProfile_of_recordCompute
#print axioms Bridge.recordCompute_terminatesWith
