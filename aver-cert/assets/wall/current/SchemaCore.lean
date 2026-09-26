-- AverCert statement schema core (audited, fixed), statement schema 9.
--
-- The single final certificate theorem is
--   `AverCert.Final.cert : AverCert.Schema.Holds manifest`.
-- This file holds every artifact-independent part of that statement: the
-- manifest data (subject, declared type layout, the function plans and the
-- obligations) and the denotation of an obligation. The thin `Schema.lean`
-- shim adds only the artifact-hash equality from `Module.lean`.
--
-- Schema 9 states every obligation over the one plan grammar (`Grammar`):
-- the model is the plan's fuel-indexed meaning, and the face is the plan's
-- signature read through the byte-pinned layout (`Grammar.HasTy`,
-- `Grammar.SRepr`). There are no per-family domains, codomains or
-- representation fields left for a producer to choose.
import SchemaBase
import Grammar

namespace AverCert.Schema
open CertPrelude
open AverCert.Grammar (Ty Sig FnPlan MCtx SVal HasTy HasTyL SRepr SReprL)

/-! ### The declared module layout (type table)

The type table names, for every source type a plan mentions, the wasm type
index that represents it. It is DECLARED data: `TypeTable.lean` confirms every
entry against the module's type section (declare-and-confirm), and every
index that the lowering writes into a code entry is confirmed a second time by
the code-entry byte equality. -/

/-- A record, or a tuple instantiation, by type id: its struct index and its
    field types in declared order. A one-field record is a newtype: the emitter
    erases it to its field's value, and `struct` names the heap type of that
    value (its field's own representation), never a struct of its own. -/
structure RecordDecl where
  tid : Nat
  struct : Nat
  fields : List Ty
deriving Repr, DecidableEq

/-- A user sum type by type id: its root struct and, per constructor in
    declaration order, the constructor's struct index and field types. -/
structure SumDecl where
  tid : Nat
  root : Nat
  ctors : List (Nat × List Ty)
deriving Repr, DecidableEq

structure TypeTable where
  /-- The Int carrier struct; `none` exactly in a module without one. -/
  carrier : Option Nat
  /-- The carrier's magnitude (limb) array. -/
  mag : Option Nat
  /-- `$string`, `(array (mut i8))`. -/
  str : Option Nat
  /-- `Vector<String>`, the argument array of the concatenation helper. -/
  strVec : Option Nat
  records : List RecordDecl
  sums : List SumDecl
  /-- `Option<T>` and `Result<T, E>` instantiations: `{i32 tag, T}` and
      `{i32 tag, T, E}`. -/
  options : List (Ty × Nat)
  results : List (Ty × Ty × Nat)
  /-- `Vector<T>` arrays and `List<T>` cons structs `{T, ref null self}`. -/
  vecs : List (Ty × Nat)
  lists : List (Ty × Nat)
  /-- Opaque pass-through types by type id: their heap type. -/
  opaques : List (Nat × Nat)
  /-- The passive data segment holding each string literal's bytes. -/
  strSegs : List (List Nat × Nat)
  /-- The cons helper of each `List<T>` a non-empty literal builds: the
      function the literal calls once per item. The acceptance pins its plan
      to the wall's cons plan (`Grammar.isConsPlan`). -/
  listCons : List (Ty × Nat) := []
deriving Repr

/-- One planned function: the plan is the function's MIR body printed 1:1
    (`Grammar.FnPlan`). `exported` functions are bound to the module by their
    export name, internal callees by their function index; `name` is the export
    name, or `#<funcIdx>` for an internal callee. `group` is the function's
    call group (its SCC, in callee-first order): a function calls only
    functions of its own group or of an earlier one. -/
structure FnEntry where
  name : String
  exported : Bool
  funcIdx : Nat
  group : Nat
  plan : FnPlan

/-! ### Runtime helpers and their named contracts -/

/-- The runtime helper functions an obligation's host table wires. The box
    helper is not among them: the obligation wires the wall's own `boxRef`,
    whose body the acceptance pins by template equality. -/
structure HostFns where
  add : List WVal → Option WVal
  sub : List WVal → Option WVal
  mul : List WVal → Option WVal
  cmp : List WVal → Option WVal
  eq : List WVal → Option WVal
  stringEq : List WVal → Option WVal
  stringConcat : Nat → List WVal → Option WVal
  toIndex : List WVal → Option WVal
  divmod : List WVal → Option WVal

/-- The named runtime contracts, exactly the premises schema 8 assumed:
    integer add/sub/mul are exact with a canonical result; the three-way
    comparison and the equality helper are exact on a CANONICAL CARRIER PAIR
    (both helpers decide structurally, so on an arbitrary represented pair
    they are not exact at all; canonicity is what rules that pair out, and
    `tests/cert_intcmp_differential.rs` checks the assumption against the
    running helpers); String equality is byte equality; String concatenation
    concatenates the byte arrays of its container argument into an array of
    its declared result type; `__aint_to_index` maps a represented Int to its
    `i32` index or the `-1` sentinel; `__aint_divmod(a, b, want_mod)` on a
    CANONICAL CARRIER PAIR with a nonzero divisor returns the canonical
    Euclidean quotient (`want_mod = 0`, Lean's `Int` `/`, which is
    `Int.ediv`) or remainder (`want_mod = 1`, `%`, `Int.emod`, in
    `[0, |b|)`). A helper that returns `none` makes its premise vacuous: none
    of these demands trap-freedom. -/
structure HostContracts {C : Nat} (S : CarrierSpec C) (h : HostFns) : Prop where
  add : ∀ a b va vb w, S.Repr a va → S.Repr b vb → h.add [va, vb] = some w →
    S.Repr (a + b) w ∧ S.Canon w
  sub : ∀ a b va vb w, S.Repr a va → S.Repr b vb → h.sub [va, vb] = some w →
    S.Repr (a - b) w ∧ S.Canon w
  mul : ∀ a b va vb w, S.Repr a va → S.Repr b vb → h.mul [va, vb] = some w →
    S.Repr (a * b) w ∧ S.Canon w
  cmp : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    h.cmp [va, vb] = some r → r = .i32v (cmpW a b)
  eq : ∀ a b va vb r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    h.eq [va, vb] = some r → r = .i32v (eqW a b)
  stringEq : ∀ a b w, h.stringEq [a, b] = some w → w = b32 (stringEqW a b)
  stringConcat : ∀ resultTy parts c, h.stringConcat resultTy [parts] = some c →
    stringConcatW resultTy parts = some c
  toIndex : ∀ n v r, S.Repr n v → h.toIndex [v] = some r → r = .i32v (toIndexW n)
  divmod : ∀ a b va vb m r, S.Repr a va → S.Repr b vb → S.Canon va → S.Canon vb →
    b ≠ 0 → (m = 0 ∨ m = 1) → h.divmod [va, vb, .i32v m] = some r →
    S.Repr (if m = 1 then a % b else a / b) r ∧ S.Canon r

/-- The totality premises of an L3 obligation, selected by its totality role:
    add and sub return on represented operands, and mul does too when the
    role is `.mul`. -/
structure HostTotal {C : Nat} (S : CarrierSpec C) (h : HostFns) (role : TotalityRole) :
    Prop where
  add : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, h.add [va, vb] = some w
  sub : ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, h.sub [va, vb] = some w
  mul : role = .mul → ∀ a b va vb, S.Repr a va → S.Repr b vb → ∃ w, h.mul [va, vb] = some w

/-! ### Obligations -/

/-- One certified export. `code` / `host` / `self` pin the emitted function
    and its runtime wiring; `layout` is the byte-pinned module layout the
    representation is read at; `sig` is the plan's signature and `model` the
    plan's fuel-indexed meaning (at fuel `k + 1` the body runs with every
    callee at fuel `k`, exactly as `wFuncN` peels fuel). The acceptance
    requires every obligation to be the one the wall derives from the plans
    (`AcceptedArtifact.obligationsOf`), so none of these fields is a producer
    choice. -/
structure Obligation where
  export_ : String
  policy  : Policy
  termination? : Option TerminationWitness := none
  totalityRole : TotalityRole := .addSub
  carrier : Nat
  layout  : MCtx
  code    : CodeTbl
  host    : HostFns → HostTbl
  self    : Nat
  sig     : Sig
  model   : Nat → List SVal → Option SVal

/-- Denotation of `simulatesModel`: under any carrier specification `S` and
    any runtime helpers obeying the named contracts, a run of the emitted
    function on represented, well-typed arguments that returns (at any fuel)
    returns a represented, well-typed result of the plan's model at that fuel.
    Partial correctness: vacuous on a trap or on fuel exhaustion. -/
def Obligation.holds (o : Obligation) : Prop :=
  ∀ (S : CarrierSpec o.carrier) (h : HostFns), HostContracts S h →
    ∀ (fuel : Nat) (svs : List SVal) (ws : List WVal) (r : WVal),
      HasTyL o.layout svs o.sig.params → SReprL S o.layout svs ws →
      wFuncN o.code (o.host h) fuel o.self ws = some r →
      ∃ sv, o.model fuel svs = some sv ∧ SRepr S o.layout sv r ∧ HasTy o.layout sv o.sig.ret

/-- Denotation of `simulatesModelTotally`: `holds`, and, under the totality
    premises its role selects, every well-typed represented input has an Int
    first argument `n`, the run at fuel `n.natAbs + 1` returns, and the model
    at that fuel is defined and represented by the result. -/
def Obligation.holdsTotal (o : Obligation) : Prop :=
  o.holds ∧
  ∀ (S : CarrierSpec o.carrier) (h : HostFns), HostContracts S h →
    HostTotal S h o.totalityRole →
    ∀ (svs : List SVal) (ws : List WVal),
      HasTyL o.layout svs o.sig.params → SReprL S o.layout svs ws →
      ∃ n tl, svs = .i n :: tl ∧ ∃ r sv,
        wFuncN o.code (o.host h) (n.natAbs + 1) o.self ws = some r ∧
        o.model (n.natAbs + 1) svs = some sv ∧ SRepr S o.layout sv r ∧
        HasTy o.layout sv o.sig.ret

/-- The manifest: the subject, the declared type layout, every planned
    function (`Plans.lean` data, one list), and the certified obligations. -/
structure Manifest where
  subject     : Subject
  types       : TypeTable
  fnPlans     : List FnEntry
  obligations : List Obligation

/-- The artifact-independent part of the audited certificate proposition:
    each export satisfies the denotation selected by its policy. -/
def HoldsCore (m : Manifest) : Prop :=
  ∀ o ∈ m.obligations,
    match o.policy with
    | .simulatesModel => o.holds
    | .simulatesModelTotally => o.holdsTotal

end AverCert.Schema
