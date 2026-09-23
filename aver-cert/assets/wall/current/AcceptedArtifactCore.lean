-- Lean-side artifact acceptance for the one plan grammar (statement schema 9).
--
-- A certificate declares its plans (`Manifest.fnPlans`), the module layout
-- (`Manifest.types`) and the runtime helper indices (`Subject`). This file
-- derives the obligations from them (`obligationsOf`: the wall computes the
-- code, the host wiring, the model and the policy axes), and states the byte
-- facts the acceptance checks: every plan's lowering IS the function's code
-- entry, every call goes to a planned function of the same or an earlier
-- group, the helper indices are the byte-pinned helpers, and the whole module
-- is accounted for.
import TypeTable
import GrammarTotal
import WasmSlice
import CertDecode
import ArithTemplateDerisk
import Wasip2Envelope

namespace AverCert.AcceptedArtifact
open AverCert.Schema
open CertPrelude
open AverCert.Grammar
open AverCert.TypeTable

/-- Exact whole-module direct-call closure claimed by the producer and
    independently recomputed from the artifact bytes. `roots` are precisely the
    certified obligation function indices; every other admitted member is
    explicitly classified as a helper. -/
structure ClosureClaim where
  roots    : List Nat
  helpers  : List Nat
  admitted : List Nat

/-- The checker-facing artifact data: the module bytes, the manifest, the
    optional wasip2 component envelope, and the direct-call closure claim. The
    plans live in the manifest alone. -/
structure ArtifactData where
  modBytes           : Nat
  modLen             : Nat
  manifest           : AverCert.Schema.Manifest
  wasip2ComponentEnvelope : Option AverCert.Wasip2Envelope.ComponentEnvelope
  closureFuel        : Nat
  closureClaim       : ClosureClaim

/-! ### The derived obligations

Nothing below is a producer choice: the host table, the code table, the model
and the policy axes are functions of the plans, the type table and the
subject. -/

/-- The host table an obligation runs against, as an association list keyed
    by role index: the wall's own `boxRef` at the box index, the contract
    functions at their role indices, and the trap-only function at the (never
    declared) negation index. -/
def hostAssoc (M : MCtx) (h : HostFns) : List (Nat × (Nat × (List WVal → Option WVal))) :=
  [(M.box, (1, boxRef M.carrier)), (M.add, (2, h.add)), (M.sub, (2, h.sub)),
   (M.mul, (2, h.mul)), (M.neg, (1, fun _ => none)), (M.cmp, (2, h.cmp)), (M.eq, (2, h.eq)),
   (M.concat, (1, h.stringConcat M.str)), (M.streq, (2, h.stringEq)),
   (M.toIndex, (1, h.toIndex))]

def hostOf (M : MCtx) (h : HostFns) : HostTbl := fun f => (hostAssoc M h).lookup f

/-- The role indices of a lowering context, in `hostAssoc` order. -/
def roleIndices (M : MCtx) : List Nat :=
  [M.box, M.add, M.sub, M.mul, M.neg, M.cmp, M.eq, M.concat, M.streq, M.toIndex]

/-- The emitted code of every planned function: its plan's lowering. -/
def codeOf (M : MCtx) (fns : List FnEntry) : CodeTbl := fun f => (planOf fns f).map (fnCode M)

/-- The meaning of every planned function: ONE group model over all plans (at
    fuel `k + 1` a body runs with every callee at fuel `k`). -/
def modelOf (fns : List FnEntry) : Nat → Nat → List SVal → Option SVal :=
  groupModel (fun _ _ _ => none) (planOf fns)

/-- The members of call group `g`, as `(function index, plan)` pairs. -/
def groupMembers (fns : List FnEntry) (g : Nat) : List (Nat × FnPlan) :=
  (fns.filter (·.group == g)).map fun e => (e.funcIdx, e.plan)

/-- The policy axes of a planned function, from its group's termination check
    (`GrammarTotal.groupPolicy`): L3 with the canonical witness and the group's
    role when the check passes, L1 otherwise. Never a manifest flag. -/
def axesOf (fns : List FnEntry) (e : FnEntry) :
    Policy × Option TerminationWitness × TotalityRole :=
  groupPolicy (groupMembers fns e.group)

def obligationOf (s : Subject) (tt : TypeTable) (fns : List FnEntry) (e : FnEntry) :
    Obligation :=
  { export_ := e.name
    policy := (axesOf fns e).1
    termination? := (axesOf fns e).2.1
    totalityRole := (axesOf fns e).2.2
    carrier := (mctxOf s tt fns).carrier
    layout := mctxOf s tt fns
    code := codeOf (mctxOf s tt fns) fns
    host := hostOf (mctxOf s tt fns)
    self := e.funcIdx
    sig := e.plan.sig
    model := fun fuel => modelOf fns fuel e.funcIdx }

/-- The certified obligations: one per exported planned function. -/
def obligationsOf (s : Subject) (tt : TypeTable) (fns : List FnEntry) : List Obligation :=
  (fns.filter (·.exported)).map (obligationOf s tt fns)

/-! ### Calls -/

mutual
  /-- The function indices a plan calls (`call` and `tailCall`). -/
  def callTargets : Expr → List Nat
    | .literal _ => []
    | .local _ => []
    | .let_ _ v body => callTargets v ++ callTargets body
    | .call (.fn f) args => f :: argsTargets args
    | .call _ args => argsTargets args
    | .tailCall f args => f :: argsTargets args
    | .binOp _ l r => callTargets l ++ callTargets r
    | .neg e => callTargets e
    | .ifThenElse c t e => callTargets c ++ callTargets t ++ callTargets e
    | .recordCreate _ fs => argsTargets fs
    | .project _ _ b => callTargets b
    | .match_ s arms => callTargets s ++ armsTargets arms
    | .construct _ _ args => argsTargets args
    | .interp parts => argsTargets parts
    | .list _ items => argsTargets items
  def argsTargets : List Expr → List Nat
    | [] => []
    | e :: es => callTargets e ++ argsTargets es
  def armsTargets : Arms → List Nat
    | .nil => []
    | .cons _ b rest => callTargets b ++ armsTargets rest
end

/-- S-4: every call of a plan targets a planned function of the same call
    group or of an earlier one. Two groups can therefore never vouch for each
    other, and a call to an unplanned index (a helper disguised as a callee,
    or any other function) declines. -/
def callsOrdered (fns : List FnEntry) (e : FnEntry) : Bool :=
  (callTargets e.plan.body).all fun t =>
    match entryOf fns t with
    | some e' => decide (e'.group ≤ e.group)
    | none => false

/-! ### Binding a plan to its function -/

def stringBytes (s : String) : AverCert.WasmSlice.ByteSeq :=
  s.toList.map Char.toNat

/-- The declared function type of the bound function is exactly the plan's
    signature, read through the layout. -/
def sigPinned (n len : Nat) (M : MCtx) (sig : Sig) (typeIdx : Nat) : Bool :=
  match sig.params.mapM (valTyD M), valTyD M sig.ret with
  | some ps, some r =>
      AverCert.WasmSlice.typeSectionMatches (AverCert.WasmSlice.checkFuncTypeExact ps [r]) n len
        typeIdx
  | _, _ => false

/-- The function a planned entry is bound to, selected by the plan's own code
    entry: an exported entry through its export name, an internal callee
    through its function index. Either way the module's code entry must be
    EXACTLY `codeEntryBytes` of the plan (locals vector included). -/
def boundFunction (n len : Nat) (M : MCtx) (e : FnEntry) :
    Option AverCert.WasmSlice.FuncBinding :=
  match codeEntryBytes M e.plan with
  | none => none
  | some bytes =>
      if e.exported then
        AverCert.WasmSlice.exactFuncBindingForExport n len (stringBytes e.name) bytes
      else
        (AverCert.WasmSlice.funcBindingByFuncIndex n len e.funcIdx).filter
          (fun b => b.codeEntry == bytes)

/-- One planned function: its plan types at its signature, its lowering is
    the code entry of the function at its declared index, the declared
    function type is its signature, and its calls are ordered. -/
def entryAccepted (n len : Nat) (M : MCtx) (fns : List FnEntry) (e : FnEntry) : Bool :=
  planTyped M e.plan &&
  (match boundFunction n len M e with
   | some b => b.funcIdx == e.funcIdx && sigPinned n len M e.plan.sig b.typeIdx
   | none => false) &&
  callsOrdered fns e

/-! ### Host roles -/

/-- The declared function type of each present role at its index; an absent
    role (`TypeTable.absent`) has no function and is never called. -/
def roleTypePinned (n len : Nat) (idx : Nat) (params results : List CertDecode.ValType) : Bool :=
  if idx < 4294967296 then
    match AverCert.WasmSlice.funcBindingByFuncIndex n len idx with
    | some b =>
        AverCert.WasmSlice.typeSectionMatches
          (AverCert.WasmSlice.checkFuncTypeExact params results) n len b.typeIdx
    | none => false
  else true

def refN (i : Nat) : CertDecode.ValType := .ref 0x63 (Int.ofNat i)

/-- Every present helper declares exactly the function type its role fixes:
    `box` `i64 -> carrier`, the arithmetic helpers `carrier carrier -> carrier`,
    `cmp` / `eq` `carrier carrier -> i32`, `toIndex` `carrier -> i32`, String
    equality `$string $string -> i32`, and concatenation
    `Vector<String> -> $string`, whose result type is the one the obligation
    wires into its contract (`hostOf`). -/
def roleTypesPinned (n len : Nat) (M : MCtx) : Bool :=
  let c := refN M.carrier
  roleTypePinned n len M.box [.numeric 0x7e] [c] &&
  roleTypePinned n len M.add [c, c] [c] &&
  roleTypePinned n len M.sub [c, c] [c] &&
  roleTypePinned n len M.mul [c, c] [c] &&
  roleTypePinned n len M.cmp [c, c] [.numeric 0x7f] &&
  roleTypePinned n len M.eq [c, c] [.numeric 0x7f] &&
  roleTypePinned n len M.toIndex [c] [.numeric 0x7f] &&
  roleTypePinned n len M.streq [refN M.str, refN M.str] [.numeric 0x7f] &&
  roleTypePinned n len M.concat [refN M.strVec] [refN M.str]

/-- Role indices are pairwise distinct and distinct from every planned
    function, so the host table resolves each role to its own contract and no
    planned function is a helper. Planned indices are unique. -/
def indicesDistinct (M : MCtx) (fns : List FnEntry) : Bool :=
  decide (roleIndices M ++ fns.map (·.funcIdx)).Nodup

/-! ### Helper bodies -/

/-- Body bytes of the defined function at absolute wasm function index `idx`
    (the import base is applied before indexing the code section), or `none`
    when `idx` is not a defined-function index. The bytes are the code entry
    WITHOUT its leading size-LEB — the locals vector followed by the
    instruction body — matching exactly what `arithHelperBody` synthesizes.
    `codeLocs` isolates each entry (`entryN`, `entryLen`, size-LEB included);
    re-reading that size LEB yields the locals+body region as `esz` bytes. -/
def bodyBytesAtFuncIndex (n len idx : Nat) : Option (List Nat) :=
  match CertDecode.funcImportBase n len, CertDecode.codeLocs n len with
  | some nimp, some locs =>
      if nimp ≤ idx then
        match locs[idx - nimp]? with
        | some loc =>
            match CertDecode.readU loc.entryN loc.entryLen with
            | some (esz, bodyN, _) => some (CertDecode.takeBytes esz bodyN)
            | none => none
        | none => none
      else none
  | _, _ => none

/-- One declared arith role pinned by TEMPLATE equality: the real code-section
    body at the declared function index equals the canonical helper body
    synthesized from the declared indices alone (`arithHelperBody`). An unbound
    role (`none`) is vacuously pinned — no claim can cite an absent role, so no
    plan can use it. No byte is scanned to DISCOVER a role; a wrong declaration
    synthesizes the wrong bytes and fails this equality. -/
def arithRoleCheck (n len : Nat) (role : ArithTemplateDerisk.ArithRole)
    (idx? : Option Nat) (p : ArithTemplateDerisk.ArithHostParams) : Bool :=
  match idx? with
  | none => true
  | some idx =>
      bodyBytesAtFuncIndex n len idx == some (ArithTemplateDerisk.arithHelperBody role p)

/-- The whole-module arith host-role pin — declare-and-confirm, no fingerprint.
    A byte-provably carrierless module (`__rt_aint_from_i64` export absent)
    declares no table and no params. A carriered module declares both, and every
    declared role index is pinned to its canonical helper body by template
    equality. The three-state consistency between presence of the helper export
    and presence of the declaration is preserved: mixing `some`/`none` across the
    table and params fails closed. No byte scan DISCOVERS a role here: the
    module-wide arith scanner that predated this pin has been deleted from the
    wall rather than left in place as an unreachable decoder.

    A declared table also has to name a carrier that the TYPE SECTION shows:
    `carrierState n len = some (some p.carrier)`. Every other conjunct here reads
    the export section, the code section, or nothing at all, so without this one
    `p.carrier` would be a free index spliced only into the helper bodies the
    wall synthesizes. The type table pins its own carrier declaration to the
    same decoder (`TypeTable.carrierConfirmed`), so the helper bodies and the
    plans' representation agree about where the carrier lives.

    `box`, `toIndex` and `cmp` carry a SECOND pin, to their runtime export
    names (#736 for `box`), and both pins are kept because they constrain
    different things. The name equality says at WHICH INDEX the role may be
    declared; the template equality says WHICH BYTES sit there. Only the name
    equality has any force on a `none` declaration, since `arithRoleCheck` is
    vacuous on `none` by design — dropping it would turn "this module exports no
    `__aint_to_index`" from a byte-proved fact into a producer's free choice.
    Conversely only the template equality constrains the code at an
    honestly-named export, which the name equality never reads.

    The `toIndex` pins are load-bearing, not decorative: the fused vector-read
    lowering calls an ABSTRACT contract function at the declared index and never
    interprets that function's body, so without them a package could declare
    any same-signature function as the index helper and certify a law the bytes
    do not satisfy. `none` on both sides is the honest reading for a carriered
    module that exports no `__aint_to_index`; a claim citing the role then fails
    to match, because `Subject.hostRoles` binds it to `none`.

    The comparison helper `cmp` inherits that argument word for word — a
    comparison also calls an abstract contract at a declared index.

    The equality helper `eq` is pinned by TEMPLATE only, like `add`, `sub` and
    `mul`. The emitter exports `__aint_eq` only when some user code path marks
    it live, while an Int literal `match` calls it all the same, so a name pin
    would decline every such module. Dropping the name pin keeps everything the
    pins establish: a `some` declaration must sit on a function whose body IS
    the equality template (so the three-way helper, whose body differs, can
    never be declared as `eq`, and `cmp` stays name-bound), and a `none`
    declaration lowers every call to `eq` to `absent 7`, which no code entry
    encodes, so a plan citing an undeclared `eq` declines.

    What none of this establishes: the template equality identifies the code
    behind a role, never its meaning. The add/sub/mul/box/index-extraction and
    comparison contracts stay explicit hypotheses of `Obligation.holds` and stay
    disclosed by `ClaimAxes`. Pinning bytes narrows the artifact, not the
    trusted-computing base. -/
def arithTableCheck (n len : Nat) (roles? : Option CertDecode.AddSub.Roles)
    (params? : Option ArithTemplateDerisk.ArithHostParams) : Bool :=
  match roles?, params? with
  | none, none => CertDecode.AddSub.carrierHelperAbsent n len
  | some roles, some p =>
      !CertDecode.AddSub.carrierHelperAbsent n len &&
      (CertDecode.carrierState n len == some (some p.carrier)) &&
      (roles.box == CertDecode.AddSub.boxIdx n len) &&
      (roles.toIndex == CertDecode.AddSub.toIndexIdx n len) &&
      (roles.cmp == CertDecode.AddSub.cmpIdx n len) &&
      ArithTemplateDerisk.checkArithHostParams p &&
      arithRoleCheck n len .box roles.box p &&
      arithRoleCheck n len .toIndex roles.toIndex p &&
      arithRoleCheck n len .add roles.add p &&
      arithRoleCheck n len .sub roles.sub p &&
      arithRoleCheck n len .mul roles.mul p &&
      arithRoleCheck n len .cmp roles.cmp p &&
      arithRoleCheck n len .eq roles.eq p
  | _, _ => false

/-- Bind the declared host-role table and arith indices to the module bytes by
    TEMPLATE equality. Replaces the earlier byte-fingerprint decode: the
    certificate DECLARES which function index carries each helper (plus the
    carrier/limb/sub-routine indices the bodies mention) and the wall
    SYNTHESIZES the canonical helper body from that declaration and pins the real
    code bytes equal to it. `box`, `toIndex` and `cmp` stay name-bound as
    well, and the carrierless class stays proved by the absent
    `__rt_aint_from_i64` export (#736 intact). -/
def decodedHostRoleTable (artifact : ArtifactData) : Prop :=
  arithTableCheck artifact.modBytes artifact.modLen
    artifact.manifest.subject.hostRoleTable artifact.manifest.subject.arithParams = true

/-- Decode every String.eq/String.concat role exactly once for the whole module.
    Unlike add/sub, the result is a list because every matching function is
    classified independently; duplicate roles at distinct indices are retained. -/
def decodedStringHostRoles (artifact : ArtifactData) : Prop :=
  CertDecode.StringHost.roleTable artifact.modBytes artifact.modLen =
    some artifact.manifest.subject.stringHostRoles


/-! ### Whole-module interface accounting and certified-closure isolation -/

def stringListNodup (xs : List String) : Bool :=
  AverCert.WasmSlice.indexedNodup xs

def lowerHexByte (byte : Nat) : Bool :=
  (decide (48 ≤ byte) && decide (byte ≤ 57)) ||
  (decide (97 ≤ byte) && decide (byte ≤ 102))

def customCapabilityModuleTail : Nat → AverCert.WasmSlice.ByteSeq → Bool
  | count, 45 :: 99 :: hash =>
      decide (0 < count) && count % 2 == 0 && hash.length == 64 &&
        hash.all lowerHexByte
  | count, byte :: rest =>
      lowerHexByte byte && customCapabilityModuleTail (count + 1) rest
  | _, [] => false

/-- Contract-derived raw wasm-gc imports are opaque capabilities, admitted
    only under the compiler's injective UTF-8-hex namespace. The complete
    contract hash is transport identity; the manifest and import-section fold
    still pin the exact pair byte-for-byte. -/
def customCapabilityImport (capability : String × String) : Bool :=
  let modulePrefix := stringBytes "aver:user/cap-n"
  let operationPrefix := stringBytes "op-n"
  let moduleBytes := stringBytes capability.1
  let operationBytes := stringBytes capability.2
  let operationTail := operationBytes.drop operationPrefix.length
  modulePrefix.isPrefixOf moduleBytes &&
  customCapabilityModuleTail 0 (moduleBytes.drop modulePrefix.length) &&
  operationPrefix.isPrefixOf operationBytes &&
  !operationTail.isEmpty && operationTail.length % 2 == 0 &&
  operationTail.all lowerHexByte

def byteSeqListNodup (xs : List AverCert.WasmSlice.ByteSeq) : Bool :=
  AverCert.WasmSlice.indexedNodup xs

def certifiedExportEntries
    (manifest : AverCert.Schema.Manifest) : List AverCert.WasmSlice.ExportEntry :=
  manifest.obligations.map (fun obligation =>
    { name := stringBytes obligation.export_, kind := 0, idx := obligation.self })

def declaredUncertifiedNames
    (manifest : AverCert.Schema.Manifest) : List AverCert.WasmSlice.ByteSeq :=
  manifest.subject.declaredUncertified.map (fun entry => stringBytes entry.1)

structure ExportKey where
  name : AverCert.WasmSlice.ByteSeq
  kind : Nat
  idx : Nat
deriving Ord

def exportEntryKey (entry : AverCert.WasmSlice.ExportEntry) : ExportKey :=
  ⟨entry.name, entry.kind, entry.idx⟩

/-- Every byte-derived module export is classified exactly once: either the
    function/name/index of a claimed obligation or an explicit uncertified
    declaration. Both declaration lists are duplicate-free, disjoint and have
    no phantom names absent from the export section. -/
def exportsAccounted (artifact : ArtifactData) : Bool :=
  match AverCert.WasmSlice.enumExports artifact.modBytes artifact.modLen with
  | none => false
  | some actual =>
      let certified := certifiedExportEntries artifact.manifest
      let declared := declaredUncertifiedNames artifact.manifest
      let actualNames := actual.map (fun entry => entry.name)
      let certifiedNames := certified.map (fun entry => entry.name)
      let actualNameIndex := AverCert.WasmSlice.orderedSet actualNames
      let declaredIndex := AverCert.WasmSlice.orderedSet declared
      let actualEntryIndex := AverCert.WasmSlice.orderedSet (actual.map exportEntryKey)
      let certifiedEntryIndex := AverCert.WasmSlice.orderedSet (certified.map exportEntryKey)
      byteSeqListNodup actualNames &&
      byteSeqListNodup certifiedNames &&
      byteSeqListNodup declared &&
      certifiedNames.all (fun name => !declaredIndex.contains name) &&
      actual.all (fun entry =>
        certifiedEntryIndex.contains (exportEntryKey entry) ||
          declaredIndex.contains entry.name) &&
      certified.all (fun entry => actualEntryIndex.contains (exportEntryKey entry)) &&
      declared.all actualNameIndex.contains

def capabilityBytes (capability : String × String) :
    AverCert.WasmSlice.ByteSeq × AverCert.WasmSlice.ByteSeq :=
  (stringBytes capability.1, stringBytes capability.2)

/-- The manifest capability list is exact (including import order), contains no
    duplicates, and every import is either in the finite standard registry for
    the manifest target or in the exact contract-derived custom-capability
    namespace. Non-capability and malformed imports therefore fail here. -/
def importsWithinCapabilities (artifact : ArtifactData) : Bool :=
  let declared := artifact.manifest.subject.capabilities
  stringListNodup (declared.map (fun capability => capability.1 ++ "." ++ capability.2)) &&
  declared.all (fun capability =>
    (AverCert.Schema.capabilityRegistryForTarget artifact.manifest.subject.target).contains capability ||
      customCapabilityImport capability) &&
  match AverCert.WasmSlice.enumImportNames artifact.modBytes artifact.modLen with
  | some actual => actual == declared.map capabilityBytes
  | none => false

/-- The manifest declares absence/presence and, when present, the exact start
    function index read from section 8. -/
def startAccounted (artifact : ArtifactData) : Bool :=
  AverCert.WasmSlice.startFuncIndex artifact.modBytes artifact.modLen ==
    some artifact.manifest.subject.start

/-- One union closure over all certified roots. Closure of a union is the union
    of each root's closure, while scanning shared helpers only once. The exact
    admitted set is partitioned into certified roots and helpers; imports and
    rejected opcodes make `closureFold` return `none`. Shared memory is rejected
    as a declaration-level hidden channel. -/
def closureIsolation (artifact : ArtifactData) : Bool :=
  let claim := artifact.closureClaim
  let certified := artifact.manifest.obligations.map (fun obligation => obligation.self)
  AverCert.WasmSlice.natListNodup claim.roots &&
  AverCert.WasmSlice.natListNodup claim.helpers &&
  AverCert.WasmSlice.natListNodup claim.admitted &&
  AverCert.WasmSlice.natSetEq claim.roots certified &&
  claim.roots.all (fun root => !AverCert.WasmSlice.natMem root claim.helpers) &&
  AverCert.WasmSlice.natSetEq claim.admitted (claim.roots ++ claim.helpers) &&
  AverCert.WasmSlice.noSharedMemory artifact.modBytes artifact.modLen &&
  match AverCert.WasmSlice.closureFold artifact.modBytes artifact.modLen
      artifact.closureFuel claim.roots [] with
  | some actual => AverCert.WasmSlice.natSetEq actual claim.admitted
  | none => false

def acceptedWholeModule (artifact : ArtifactData) : Prop :=
  CertDecode.moduleFramingValid artifact.modBytes artifact.modLen = true ∧
  exportsAccounted artifact = true ∧
  importsWithinCapabilities artifact = true ∧
  startAccounted artifact = true ∧
  closureIsolation artifact = true


/-! ### The plans against the bytes -/

/-- Every byte fact about the plans: role and planned indices distinct, every
    planned function bound to its code entry (and signature, and ordered
    calls), the type table confirmed against the type section, every string
    literal against its data segment, and every present helper's declared
    type. Last, the declarations are well formed (`TypeTable.declsWellFormed`):
    `eqref` only on the subject scratch, no newtype cycle, and every declared
    type and every signature type inhabited, so no obligation is vacuous
    (`AcceptanceSoundness.accepted_nonvacuous`). -/
def plansAccepted (artifact : ArtifactData) : Bool :=
  let m := artifact.manifest
  let M := mctxOf m.subject m.types m.fnPlans
  indicesDistinct M m.fnPlans &&
  m.fnPlans.all (entryAccepted artifact.modBytes artifact.modLen M m.fnPlans) &&
  typeTableConfirmed artifact.modBytes artifact.modLen m.subject m.types m.fnPlans &&
  dataConfirmed artifact.modBytes artifact.modLen m.subject m.types m.fnPlans &&
  roleTypesPinned artifact.modBytes artifact.modLen M &&
  declsWellFormed m.subject m.types m.fnPlans

/-- The manifest's obligations are exactly the ones the wall derives from its
    plans: no obligation field is producer data. -/
def obligationsDerived (artifact : ArtifactData) : Prop :=
  artifact.manifest.obligations =
    obligationsOf artifact.manifest.subject artifact.manifest.types artifact.manifest.fnPlans

def artifactCoreBytes (artifact : ArtifactData) : AverCert.Wasip2Envelope.ByteSeq :=
  AverCert.Wasip2Envelope.ComponentEnvelope.bytes artifact.modBytes artifact.modLen

/-- Check a single wasip2 envelope declaration against delivered component bytes
    and the already-selected core module bytes. The split is length-driven only:
    it never parses component syntax or searches for a core module. -/
def wasip2EnvelopeAccepted
    (env : AverCert.Wasip2Envelope.ComponentEnvelope)
    (componentBytes componentLen modBytes modLen : Nat) : Bool :=
  env.embeddedCoreModuleLen != 0 &&
  componentLen == env.prefixLen + env.embeddedCoreModuleLen + env.suffixLen &&
  match env.split componentBytes componentLen with
  | some (_, core, _) =>
      core == AverCert.Wasip2Envelope.ComponentEnvelope.bytes modBytes modLen
  | none => false

/-- Bind the target artifact bytes to the core module bytes used by the existing
    wasm decoders.  For wasm-gc, the delivered artifact is the core module.  For
    wasip2, the delivered artifact is a component and the manifest-declared
    prefix/core/suffix lengths must split that component so that its embedded
    core byte sequence is exactly `artifact.modBytes`.  No component syntax is
    parsed here. -/
def artifactEnvelopeAccepted
    (componentBytes componentLen : Nat) (artifact : ArtifactData) : Bool :=
  match artifact.wasip2ComponentEnvelope with
  | none =>
      artifact.manifest.subject.target == expectedWasmGcArtifactTarget &&
      artifact.manifest.subject.abi == expectedRuntimeAbiWasmGc &&
      componentBytes == artifact.modBytes &&
      componentLen == artifact.modLen
  | some env =>
      artifact.manifest.subject.target == expectedWasip2ArtifactTarget &&
      artifact.manifest.subject.abi == expectedRuntimeAbiWasip2 &&
      wasip2EnvelopeAccepted env componentBytes componentLen artifact.modBytes artifact.modLen

def expectedArtifactRoot : String :=
  "AverCert.Artifact.certificate"

def subjectMatchesArtifactRoot (artifact : ArtifactData) : Prop :=
  artifact.manifest.subject.artifactRoot = expectedArtifactRoot


end AverCert.AcceptedArtifact
