import Bridge.Translate

/-!
# Talos configuration for one certified export

The synthetic module has ONE function (the export's lowered body, translated),
one IMPORT per environment slot (the wall's host contracts are abstract slots
of `HostTbl`; in Talos they are imports resolved by a `HostEnv`), and the
declared struct types. It is NOT the certified artifact: brief §1 — the
bridge is a theorem about `translate (envOfClaim …) (lower plan)`, and its
connection to the pinned bytes is the composition with the wall's byte pins.

Types in the synthetic module are inert for `Step` (`Config.lean` documents
which facts each rule reads): imports carry only their arities, struct types
only their field count and (un)packedness, the function only its arity, its
number of nullable-reference locals and its body.

## `HostSimulation`

The wall's contract for a host slot is a partial function on structural
values, `hf : List WVal → Option WVal`. Talos's `callHostReturn`
(SmallStep.lean:3558) runs the CONCRETE `hostFunction.invoke store.wasm args`
and reads back `.Return results wasm'`. `HostSimulation` ties the two: on
related arguments a defined contract result is matched by a `.Return` of one
related value, and the host only ever EXTENDS the heap (old heap is a prefix
of the new one). The frame half is a new hypothesis the wall does not state
(its `_hadd`/`_hmul` know no heap); it is what keeps every other live
reference — the locals, the rest of the stack — related across the call.
-/

namespace Bridge
open Wasm Wasm.SmallStep CertPrelude
open AverCert.Schema (CarrierSpec)

def valueTypeOf : STy → ValueType
  | .i32 => .i32
  | .i64 => .i64
  | .i64b => .i64
  | .f64 => .f64
  | .ref => .anyref
  | .car => .anyref

def fieldTypeOf (t : STy) : FieldType := { storage := .val (valueTypeOf t), isMut := false }

theorem fieldTypeOf_pack (t : STy) (v : Value) : (fieldTypeOf t).pack v = v := by
  cases t <;> cases v <;> rfl

def importDecl (s : ImportSig) : ImportDecl :=
  { module := "aver", name := s!"slot{s.slot}",
    params := s.params.map valueTypeOf, results := [valueTypeOf s.result] }

def gcTypeDefOf (structs : List (Nat × List STy)) (i : Nat) : GcTypeDef :=
  match structSorts? structs i with
  | some fs => { comp := .struct (fs.map fieldTypeOf) }
  | none => { comp := .struct [] }

/-- Every non-parameter local of a compute-face body is the one nullable
    carrier-reference scratch slot (`PlanBytes.singleCarrierLocalBodyBytes`);
    its Talos zero is `.anyref none`, the value the wall's `null` padding
    relates to. -/
def localType (carrier : Nat) : ValueType := .ref true (.concrete carrier)

theorem localType_zero (carrier : Nat) : (localType carrier).zero = .anyref none := rfl

def synthFunction (env : TranslateEnv) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (body : Program) : Function :=
  { params := paramSorts.map valueTypeOf
    locals := List.replicate nlocals (localType env.carrier)
    body
    results := [valueTypeOf result] }

/-- The name under which the synthetic module exports its one function.
    Talos enters a module by export NAME (`Module.findExport`); the name is
    not semantic — it resolves to the function after the imports — so the
    bridge fixes one. -/
def exportName : String := "aver"

def synthModule (env : TranslateEnv) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (body : Program) : Module :=
  { funcs := [synthFunction env paramSorts result nlocals body]
    exports := [{ name := exportName, funcIdx := env.imports.length }]
    imports := env.imports.map importDecl
    gcTypes := (List.range (structsBound env.structs)).map (gcTypeDefOf env.structs) }

/-- The instance Talos's export boundary builds (`startExportConfig?`:
    `{ module := m, host := env }`). `resolvedImports` is left at its default:
    `Step` consults it only for an import WITHOUT a host function
    (`callCrossInstance`, `hnoHost`), and every import here has one
    (`HostSimulation.resolved`). -/
def synthInstance (m : Module) (hostEnv : HostEnv α) : ModuleInstance α :=
  { module := m, host := hostEnv }

def synthRuntime (m : Module) (hostEnv : HostEnv α) : RuntimeEnv α :=
  { instances := #[synthInstance m hostEnv], entry := ⟨0⟩ }

@[simp] theorem synthRuntime_currentModule (m : Module) (hostEnv : HostEnv α) :
    (synthRuntime m hostEnv).currentModule = m := by
  simp [synthRuntime, synthInstance]

@[simp] theorem synthRuntime_currentHost (m : Module) (hostEnv : HostEnv α) :
    (synthRuntime m hostEnv).currentHost = hostEnv := by
  simp [synthRuntime, synthInstance]

/-! ### What `Step` reads off the synthetic module -/

theorem synthModule_structFields {env : TranslateEnv} {ty : Nat} {fs : List STy}
    (h : structSorts? env.structs ty = some fs) (paramSorts result nlocals body) :
    (synthModule env paramSorts result nlocals body).structFields? ty = some (fs.map fieldTypeOf) := by
  have hlt := structSorts?_lt_bound h
  simp [Module.structFields?, Module.gcComposite?, synthModule, List.getElem?_range hlt, gcTypeDefOf, h]

theorem synthModule_imports_length (env : TranslateEnv) (paramSorts result nlocals body) :
    (synthModule env paramSorts result nlocals body).imports.length = env.imports.length := by
  simp [synthModule]

theorem synthModule_imports_getElem? {env : TranslateEnv} {i : Nat} {sig : ImportSig}
    (h : env.imports[i]? = some sig) (paramSorts result nlocals body) :
    (synthModule env paramSorts result nlocals body).imports[i]? = some (importDecl sig) := by
  simp [synthModule, h]

theorem importDecl_params_length (sig : ImportSig) :
    (importDecl sig).params.length = sig.params.length := by
  simp [importDecl]

theorem importDecl_results_length (sig : ImportSig) :
    (importDecl sig).results.length = 1 := by
  simp [importDecl]

theorem synthModule_funcs_getElem? (env : TranslateEnv) (paramSorts result nlocals body) :
    (synthModule env paramSorts result nlocals body).funcs[0]? =
      some (synthFunction env paramSorts result nlocals body) := rfl

/-! ### Initial configuration -/

/-- The thread Talos starts the export in: parameters in `params`, the
    declared locals zeroed, empty stack, the translated body, one result. -/
def initialThread (fn : Function) (args : List Value) : ThreadState α :=
  { locals := fn.toLocals args
    code := fn.body
    resultArity := fn.results.length
    callerRemainder := [] }

def initialConfig (m : Module) (hostEnv : HostEnv α) (fn : Function)
    (store0 : Store α) (args : List Value) : Config α :=
  ⟨.running (initialThread fn args), { runtime := synthRuntime m hostEnv, wasm := store0 }⟩

/-- `initialConfig` is what Talos's `initConfig` builds for the function after
    the imports, given the arguments in Talos's stack order (last argument
    first). `RunsExport.lean` lifts this to the export boundary
    (`startExportConfig?`). -/
theorem initConfig_synth (env : TranslateEnv) (paramSorts : List STy) (result : STy)
    (nlocals : Nat) (body : Program) (hostEnv : HostEnv α) (store0 : Store α)
    (args : List Value) (hlen : args.length = paramSorts.length) :
    initConfig (synthInstance (synthModule env paramSorts result nlocals body) hostEnv)
        env.imports.length store0 args.reverse =
      .ok (initialConfig (synthModule env paramSorts result nlocals body) hostEnv
        (synthFunction env paramSorts result nlocals body) store0 args) := by
  have hnp : (synthFunction env paramSorts result nlocals body).numParams = args.length := by
    simp [synthFunction, Function.numParams, hlen]
  have ht : List.take args.length args.reverse = args.reverse := by
    simpa using List.take_length (l := args.reverse)
  have hd : List.drop args.length args.reverse = [] := by
    simp
  simp only [initConfig, synthInstance, synthModule_imports_length, Nat.lt_irrefl,
    if_false, Nat.sub_self, synthModule_funcs_getElem?, hnp, ht, hd, List.reverse_reverse]
  rfl

/-! ### The host assumption -/

/-- Brief §4.3: the concrete Talos host environment simulates the wall's
    abstract host table through the environment's import table.

    * `resolved`: every import has a resolver (Talos's `callHostReturn` needs
      `currentHost.funcs[i]? = some _`).
    * `invoke`: on SORTED arguments (the typed run only calls with sorted
      arguments; the sorts fix the carrier shapes a concrete host reads) related
      by `Rs` (under the current heap), a
      DEFINED contract result `hf ws = some w` is matched by the resolver
      returning exactly one value related to `w`, with the old heap a PREFIX
      of the new one (the heap frame). Nothing is assumed when the contract is
      undefined, matching the wall's partial-correctness reading.

    The wall's arity agreement (`host f = some (sig.params.length, _)`) is
    `HostSorts` (Translate.lean), which the sort discipline needs anyway. -/
structure HostSimulation (env : TranslateEnv) (S : CarrierSpec env.carrier) (host : HostTbl)
    (hostEnv : HostEnv α) : Prop where
  resolved : ∀ (i : Nat) (sig : ImportSig), env.imports[i]? = some sig → ∃ hfn, hostEnv.funcs[i]? = some hfn
  invoke : ∀ (f i : Nat) (sig : ImportSig) (hf : List WVal → Option WVal) (hfn : HostFn α),
    slotLookup? env.imports f = some (i, sig) →
    host f = some (sig.params.length, hf) →
    hostEnv.funcs[i]? = some hfn →
    ∀ (st : Store α) (args : List Value) (ws : List WVal) (w : WVal),
      Sorted env S ws sig.params → Rs st.gcHeap args ws → hf ws = some w →
      ∃ r st', hfn.invoke st args = .Return [r] st' ∧ st.gcHeap <+: st'.gcHeap ∧ R st'.gcHeap r w

end Bridge
