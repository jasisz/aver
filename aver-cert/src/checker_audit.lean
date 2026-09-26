-- Authored by aver-cert; never accepted from the certificate.
--
-- The checker's audit program. It is elaborated with only the Lean toolchain
-- in scope: no certificate module is imported while this file is elaborated,
-- so nothing a package declares (an instance, a notation, a name in some
-- namespace) can change what the code below computes. At run time it loads
-- the built `CheckerWitness` environment and inspects it.
import Lean

namespace AverCertAudit

open Lean

/-- The certificate package's own modules (validated module roots). -/
def packageModules : List Name := @PACKAGE_MODULES@

/-- The axiom whitelist. -/
def allowed : List Name := @ALLOWED@

/-- Roots whose closure must stay inside the whitelist: the accepted
    artifact and every report pin. -/
def strictRoots : List Name := @STRICT_ROOTS@

/-- Every record a bridge encoder reads, with the accessors it lists. -/
def recordShapes : List (Name × List Name) := @RECORDS@

/-- Every sum a bridge encoder matches on, with the constructors it lists and
    the number of fields it encodes for each. -/
def sumShapes : List (Name × List (Name × Nat)) := @SUMS@

/-- The namespace roots the wall and the checker declare in. A package
    declares nothing under them. -/
def wallRoots : List Name := @WALL_ROOTS@

/-- The only constants a package declares directly under `AverCert`: the
    manifest's `subject` and `manifest`. -/
def packageAverCertLeaves : List Name := @PACKAGE_AVERCERT_LEAVES@

/-- The only namespaces under `AverCert` a package declares in: its plans,
    byte facts, final theorem, bridges and law corollaries. -/
def packageAverCertChildren : List Name := @PACKAGE_AVERCERT_CHILDREN@

/-- Why the package constant `n` is refused for where it is declared, if it
    is. A package name under a wall namespace, or one nesting `AverCert`
    below its first component, is where a dotted reference in the wall or the
    witness could resolve first, since Lean tries the innermost enclosing
    namespace before the root. Under `AverCert` the admitted names are exact
    shapes: `AverCert.manifest`, `AverCert.subject`, and names at least one
    component deep in a producer namespace. The caller exempts exactly two
    kinds of package constant from this rule: a private constant, and a
    `leanAuxiliary` one. -/
def namespaceRefusal (n : Name) : Option String :=
  match n.eraseMacroScopes.components with
  | [] => none
  | first :: rest =>
    if rest.any (fun c => c == `AverCert || c == `AverCertChecker) then
      some s!"a certificate module declares {n}, which nests a checker namespace"
    else if first == `AverCert then
      match rest with
      | [leaf] =>
        if packageAverCertLeaves.contains leaf then none
        else some s!"a certificate module declares {n} inside the checker's AverCert namespace"
      | second :: _ :: _ =>
        if packageAverCertChildren.contains second then none
        else some s!"a certificate module declares {n} inside the checker's AverCert namespace"
      | [] => some s!"a certificate module declares {n} inside the checker's AverCert namespace"
    else if wallRoots.contains first then
      some s!"a certificate module declares {n} inside the checker's {first} namespace"
    else none

/-- The longest proper prefix of `n` that is itself a declared constant, if
    any. Lean resolves a dotted identifier to the longest prefix that is a
    constant and reads the rest as fields, so a package constant
    `AverCert.manifest.subject` is what `AverCert.manifest.subject.contracts`
    would mean. The witness reads every field through a wall projection
    function; this refusal keeps a second layer under that. -/
def extendedConstant (env : Environment) (n : Name) : Option Name := Id.run do
  let mut p := n.getPrefix
  for _ in [0:n.getNumParts] do
    if p.isAnonymous then return none
    if env.contains p then return some p
    p := p.getPrefix
  return none

/-- Whether `s` is `<stem><k>`, where `k` is digits, possibly joined by
    underscores (`match_1_1`), and starts with a digit. -/
def numberedAs (stem s : String) : Bool :=
  let rest := s.toList.drop stem.length
  s.startsWith stem &&
    (match rest with
     | c :: _ => c.isDigit
     | [] => false) &&
    rest.all (fun c => c.isDigit || c == '_')

/-- Whether the last component of `n` is `proof_<k>`, `match_<k>` or
    `eq_<k>`, the names Lean gives an abstracted proof, a matcher and an
    equation lemma. (The environment the audit imports does not rebuild the
    matcher extension's state, so a matcher is recognised by its name.) -/
def numberedAuxiliary : Name → Bool
  | .str _ s => numberedAs "proof_" s || numberedAs "match_" s || numberedAs "eq_" s
  | _ => false

/-- Whether the last component of `n` is `eq_def` or `eq_unfold`, the
    names of the unfolding lemmas Lean realizes for a definition. -/
def unfoldAuxiliary : Name → Bool
  | .str _ s => s == "eq_def" || s == "eq_unfold"
  | _ => false

/-- Whether `n` is an auxiliary Lean itself declares beside the constant
    `n.getPrefix`. Two cases:
    * beside a constant the package does NOT declare (a wall or core
      constant), a reserved name such as an equation lemma. That parent
      existed before every package module, and Lean refuses a user
      declaration of a reserved name whose parent exists, so such a
      constant can only have been realized by Lean, in the package module
      that first unfolded the parent. (Beside a package constant a reserved
      name is not exempt on that ground: a package can declare `V.h.eq_1`
      itself before it declares `V.h`.)
    * beside a constant the package declares, an internal (`_`-prefixed)
      compiler constant, an abstracted `proof_<k>`, a matcher `match_<k>`,
      an equation `eq_<k>`, or an unfolding lemma `eq_def` or `eq_unfold`,
      recognised by name alone.
    None of these is a field name a wall structure has, so no field read
    can land on one. -/
def leanAuxiliary (env : Environment) (inPkg : Name → Bool) (n : Name) : Bool :=
  let parent := n.getPrefix
  env.contains parent &&
    (if inPkg parent then n.isInternal || numberedAuxiliary n || unfoldAuxiliary n
     else isReservedName env n)

/-- Every bridged law's statement definition in the witness, with the model
    constants its bridges are about. -/
def lawModelUses : List (Name × List Name) := @LAW_MODEL_USES@

def lawRoots : List Name := @LAW_ROOTS@
def bridgedLawRoots : List Name := @BRIDGED_LAW_ROOTS@
def bridgeRoots : List Name := @BRIDGE_ROOTS@

/-- Classes whose instances carry proofs, not choices: an instance of one of
    these cannot make a proposition mean something else, and a false one
    needs an axiom the audit sees. Admitted at any type. -/
def proofClasses : List Name :=
  [``Decidable, ``DecidableEq, ``DecidableRel, ``DecidablePred, ``Nonempty,
   ``ReflBEq, ``LawfulBEq]

/-- Data classes admitted only at a type the package itself declares as an
    inductive type: `==`, a default value, and the `SizeOf` instance Lean
    generates for every inductive type, of the model's own records and sums. -/
def ownTypeClasses : List Name := [``Inhabited, ``BEq, ``SizeOf]

def decline (reason : String) : IO UInt32 := do
  IO.println ("@DECLINE_MARKER@ " ++ reason)
  return 1

def moduleOf (env : Environment) (n : Name) : Option Name :=
  match env.getModuleIdxFor? n with
  | some idx => env.header.moduleNames[idx.toNat]?
  | none => none

def inPackage (env : Environment) (n : Name) : Bool :=
  match moduleOf env n with
  | some m => packageModules.contains m
  | none => false

-- The axiom memo begins (a unit test elaborates this part alone).
/-! ### Axioms, collected once for every root

`Lean.collectAxioms` starts from nothing on every call: the environment
the audit imports does not load the extension that stores each imported
constant's axioms, so every call walks the root's whole closure again,
and the roots of a large certificate share most of theirs. The walk below
reads the same edges `collectAxioms` reads (a constant's type, the value
of a definition, theorem or opaque, the constructors of an inductive), and
keeps one memo for all roots.

The memo holds a constant's axioms only once they are final. Constants
can reach each other in a cycle (an inductive and its constructors), and
while such a constant is still being walked what has been collected for
it is partial, so the walk finds the cycles (Tarjan's strongly connected
components) and records one set for a whole component when it is
complete: the axioms of its members and of everything they reach. What a
root reports is therefore exactly the axioms reachable from it, the same
set `collectAxioms` returns on its own, whichever roots were asked
before. -/

structure AxiomMemo where
  /-- The final axiom set of every constant whose component is complete. -/
  done : NameMap NameSet := {}
  /-- The depth-first index and low link of every constant being walked. -/
  index : NameMap Nat := {}
  low : NameMap Nat := {}
  /-- Constants being walked whose component is not complete, in visit order. -/
  stack : Array Name := #[]
  onStack : NameSet := {}
  /-- Axioms found so far for a constant being walked: its own, and the
      final sets of the successors outside its component. -/
  partialAxioms : NameMap NameSet := {}
  next : Nat := 0

/-- The constants `collectAxioms` visits after `c`, and whether `c` is itself
    an axiom. -/
def axiomEdges (env : Environment) (c : Name) : Bool × Array Name :=
  let used (e : Expr) : Array Name := e.getUsedConstants
  match env.checked.get.find? c with
  | some (.axiomInfo v) => (true, used v.type)
  | some (.defnInfo v) => (false, used v.type ++ used v.value)
  | some (.thmInfo v) => (false, used v.type ++ used v.value)
  | some (.opaqueInfo v) => (false, used v.type ++ used v.value)
  | some (.quotInfo _) => (false, #[])
  | some (.ctorInfo v) => (false, used v.type)
  | some (.recInfo v) => (false, used v.type)
  | some (.inductInfo v) => (false, used v.type ++ v.ctors.toArray)
  | none => (false, #[])

partial def axiomWalk (env : Environment) (c : Name) : StateM AxiomMemo Unit := do
  let s ← get
  if s.done.contains c || s.index.contains c then return
  let (isAxiom, succs) := axiomEdges env c
  modify fun s => { s with
    index := s.index.insert c s.next, low := s.low.insert c s.next, next := s.next + 1,
    stack := s.stack.push c, onStack := s.onStack.insert c,
    partialAxioms := s.partialAxioms.insert c (if isAxiom then ({} : NameSet).insert c else {}) }
  for w in succs do
    axiomWalk env w
    let s ← get
    match s.done.find? w with
    | some axs =>
      -- `w` is in a complete component: its set is final.
      let mine := (s.partialAxioms.find? c).getD {}
      set { s with partialAxioms := s.partialAxioms.insert c (axs.foldl (·.insert ·) mine) }
    | none =>
      -- `w` is on the stack, in `c`'s component: joined when it completes.
      let lw := (s.low.find? w).getD 0
      let lc := (s.low.find? c).getD 0
      if lw < lc then set { s with low := s.low.insert c lw }
  let s ← get
  if s.low.find? c == s.index.find? c then
    -- `c` roots a component: pop it and give every member the joined set.
    let mut members : Array Name := #[]
    let mut stack := s.stack
    repeat
      match stack.back? with
      | none => break
      | some m =>
        stack := stack.pop
        members := members.push m
        if m == c then break
    let joined := members.foldl
      (fun acc m => ((s.partialAxioms.find? m).getD {}).foldl (·.insert ·) acc) ({} : NameSet)
    set { s with
      stack := stack,
      onStack := members.foldl (·.erase ·) s.onStack,
      partialAxioms := members.foldl (·.erase ·) s.partialAxioms,
      done := members.foldl (·.insert · joined) s.done }

/-- The axioms reachable from `n`, sorted, reading and extending `memo`. -/
def axiomsOf (env : Environment) (memo : IO.Ref AxiomMemo) (n : Name) : IO (Array Name) := do
  let s ← memo.get
  let s := (axiomWalk env n).run s |>.2
  memo.set s
  return ((s.done.find? n).getD {}).toArray.qsort Name.lt
-- The axiom memo ends.

def runMeta (env : Environment) (x : MetaM α) : IO α := do
  let (a, _) ← (x.run' {} {}).toIO
    { fileName := "<aver-cert audit>", fileMap := default } { env := env }
  return a

/-- Whether the type of the declaration `n` ends in `Prop` (it is a
    proposition former, e.g. a structure or an inductive in `Prop`). -/
def isPropFormer (env : Environment) (type : Expr) : IO Bool :=
  runMeta env (Meta.forallTelescopeReducing type fun _ body => return body.isProp)

/-- Why a record encoder is refused, if it is: the type must be a structure
    outside `Prop`, the encoder must list exactly its fields in declaration
    order, and no field may be a proof. A proof field (`h : False`) or an
    unlisted field would let a bridge quantify over values the plan never
    sees, or over none at all. -/
def recordRefusal (env : Environment) (ty : Name) (fields : List Name) : IO (Option String) := do
  let some info := env.find? ty | return some s!"bridge record {ty} is not declared"
  unless isStructure env ty do
    return some s!"bridge record {ty} is not a structure"
  if ← isPropFormer env info.type then
    return some s!"bridge record {ty} is a proposition"
  unless (getStructureFields env ty).toList == fields do
    return some s!"the bridge encoder of {ty} does not list exactly its fields in order"
  for field in fields do
    let some proj := getProjFnForField? env ty field
      | return some s!"bridge record {ty} has no projection for {field}"
    let some projInfo := env.find? proj
      | return some s!"bridge record {ty} has no projection for {field}"
    let proof ← runMeta env (Meta.forallTelescopeReducing projInfo.type fun _ body => Meta.isProp body)
    if proof then
      return some s!"bridge record {ty} has the proof field {field}"
  return none

/-- Why a sum encoder is refused, if it is: the type must be an inductive
    outside `Prop` whose constructors are exactly the listed ones in order,
    each with the listed number of fields, none of them a proof. -/
def sumRefusal (env : Environment) (ty : Name) (ctors : List (Name × Nat)) : IO (Option String) := do
  let some (.inductInfo info) := env.find? ty | return some s!"bridge sum {ty} is not an inductive type"
  if ← isPropFormer env info.type then
    return some s!"bridge sum {ty} is a proposition"
  unless info.ctors == ctors.map (·.1) do
    return some s!"the bridge encoder of {ty} does not list exactly its constructors in order"
  for (ctor, fields) in ctors do
    let some (.ctorInfo ci) := env.find? ctor | return some s!"bridge constructor {ctor} is not declared"
    unless ci.numFields == fields do
      return some s!"the bridge encoder of {ctor} does not encode exactly its fields"
    let proof ← runMeta env (Meta.forallTelescopeReducing ci.type fun xs _ => do
      let mut found := false
      for x in xs.toList.drop ci.numParams do
        if ← Meta.isProp (← Meta.inferType x) then found := true
      return found)
    if proof then
      return some s!"bridge constructor {ctor} has a proof field"
  return none

/-- Whether `cls` is declared a class by the module that declares it. Read
    off that module's own class-extension entries: the environment the audit
    imports does not rebuild the extension's state, so `isClass` would answer
    false for every imported class. -/
def declaredClass (env : Environment) (cls : Name) : Bool :=
  match env.getModuleIdxFor? cls with
  | some idx => (classExtension.getModuleEntries env idx).any (·.name == cls)
  | none => false

/-- The conclusion of a (non-reducing) pi telescope. -/
def conclusion : Expr → Expr
  | .forallE _ _ body _ => conclusion body
  | e => e

/-- The value an admitted core instance must have, when its class and
    arguments are one of the two data instances the model prelude declares
    over core types: `Coe Int Float` and `HAdd String String String`. -/
def expectedCoreValue (cls : Name) (args : Array Expr) : Option Expr :=
  let one := Level.succ Level.zero
  if cls == ``Coe && args.size == 2 && args[0]! == mkConst ``Int && args[1]! == mkConst ``Float then
    some (mkAppN (mkConst ``Coe.mk [one, one])
      #[mkConst ``Int, mkConst ``Float,
        .lam `n (mkConst ``Int) (mkApp (mkConst ``Float.ofInt) (.bvar 0)) .default])
  else if cls == ``HAdd && args.size == 3 && args.all (· == mkConst ``String) then
    some (mkAppN (mkConst ``HAdd.mk [Level.zero, Level.zero, Level.zero])
      #[mkConst ``String, mkConst ``String, mkConst ``String, mkConst ``String.append])
  else none

/-- Why the instance `inst` (declared in a package module) is refused, if it
    is. The class is read off the ELABORATED type: a name alias or a class
    parent projection does not change the constant at its head. -/
def instanceRefusal (env : Environment) (inst : Name) : Option String :=
  match env.find? inst with
  | none => some s!"instance {inst} has no declaration"
  | some info =>
    let concl := conclusion info.type
    match concl.getAppFn with
    | .const cls _ =>
      let args := concl.getAppArgs
      if proofClasses.contains cls then none
      else if ownTypeClasses.contains cls then
        match args[0]?.map Expr.getAppFn with
        | some (.const ty _) =>
          match env.find? ty with
          | some (.inductInfo _) =>
            if inPackage env ty then none
            else some s!"instance {inst} of {cls} is at a type the package does not declare"
          | _ => some s!"instance {inst} of {cls} is not at an inductive type"
        | _ => some s!"instance {inst} of {cls} is not at a named type"
      else if inPackage env cls && declaredClass env cls then none
      else match expectedCoreValue cls args with
        | some expected =>
          match info.value? with
          | some value =>
            if value == expected then none
            else some s!"instance {inst} of {cls} does not have the admitted value"
          | none => some s!"instance {inst} of {cls} has no value"
        | none => some s!"instance {inst} of class {cls} is not admitted in a certificate"
    | _ => some s!"instance {inst} has no class at its head"

def main : IO UInt32 := do
  initSearchPath (← findSysroot)
  let env ← importModules #[{ module := `CheckerWitness }] {}
  -- One axiom memo for every root below (see `AxiomMemo`).
  let memo ← IO.mkRef ({} : AxiomMemo)
  -- 1. Names reserved for the checker's witness.
  for (name, _) in env.constants.map₁.toList do
    if (`AverCertChecker).isPrefixOf name && !(moduleOf env name == some `CheckerWitness) then
      return ← decline s!"a certificate module declares {name} under the checker's reserved prefix"
  -- 1b. Names under the wall's and the checker's namespaces.
  --     Exactly two kinds of package constant are exempt from both rules of
  --     this step, because no name the wall or the witness writes resolves
  --     to them: a private one (a match splitter or other auxiliary Lean
  --     builds while a package proof unfolds a wall definition), which no
  --     other module can name, and a `leanAuxiliary` one: a reserved name
  --     (an equation lemma) realized for a wall or core constant, which
  --     states that constant's own fact, or, beside a package constant, an
  --     internal compiler constant or a numbered or unfolding auxiliary.
  --     A package constant under `AverCert` must also not extend another
  --     declared constant's name, since a dotted reference to that constant's
  --     fields would resolve to it.
  for (name, _) in env.constants.map₁.toList do
    if inPackage env name then
      let auxiliary := isPrivateName name || leanAuxiliary env (inPackage env) name
      unless auxiliary do
        if let some reason := namespaceRefusal name then
          return ← decline reason
        if name.getRoot == `AverCert then
          if let some parent := extendedConstant env name then
            return ← decline s!"a certificate module declares {name}, which extends the declared constant {parent}, so a field read of {parent} could resolve to it"
  -- 2. Parser extensions and instances declared by package modules.
  for m in packageModules do
    match env.getModuleIdx? m with
    | none => pure ()
    | some idx =>
      unless (Parser.parserExtension.ext.getModuleEntries env idx).isEmpty do
        return ← decline s!"certificate module {m} extends the parser (notation, syntax or an operator)"
      for entry in Meta.instanceExtension.ext.getModuleEntries env idx do
        match entry with
        | .scoped ns _ =>
          return ← decline s!"certificate module {m} declares a scoped instance in {ns}"
        | .global e =>
          match e.globalName? with
          | none => return ← decline s!"certificate module {m} declares an anonymous instance"
          | some inst =>
            match instanceRefusal env inst with
            | some reason => return ← decline reason
            | none => pure ()
  -- 3. The shapes the bridge encoders read.
  for (ty, fields) in recordShapes do
    if let some reason ← recordRefusal env ty fields then
      return ← decline reason
  for (ty, ctors) in sumShapes do
    if let some reason ← sumRefusal env ty ctors then
      return ← decline reason
  -- 3b. The law statements. The witness elaborates them at the root, where a
  --     `_root_.`-spelled model name is exactly the root constant, whatever
  --     else the package declares; this re-checks the outcome on the
  --     elaborated terms: each bridged law's statement must use every model
  --     constant its bridges are about.
  for (stmt, models) in lawModelUses do
    let some info := env.find? stmt
      | return ← decline s!"the witness does not declare {stmt}"
    let some value := info.value?
      | return ← decline s!"the law statement {stmt} has no value"
    let used := value.getUsedConstants
    for model in models do
      unless used.contains model do
        return ← decline s!"the law statement {stmt} does not use the bridged model {model}"
  -- 4. The accepted root and the report pins: whitelisted axioms only.
  for root in strictRoots do
    if (env.find? root).isNone then
      return ← decline s!"the witness does not declare {root}"
    for used in ← axiomsOf env memo root do
      unless allowed.contains used do
        return ← decline s!"non-whitelisted axiom: {used} (under {root})"
  -- 5. Per-claim audit lines, read back by the checker.
  for root in lawRoots ++ bridgedLawRoots ++ bridgeRoots do
    if (env.find? root).isNone then
      return ← decline s!"the witness does not declare {root}"
  let audit (marker : String) (roots : List Name) : IO Unit := do
    for root in roots do
      let offending := (← axiomsOf env memo root).filter (fun used => !allowed.contains used)
      if offending.isEmpty then
        IO.println s!"{marker} {root} ok"
      else
        let names := ",".intercalate (offending.toList.map Name.toString)
        IO.println s!"{marker} {root} axioms {names}"
  audit "@LAW_MARKER@" lawRoots
  audit "@BRIDGED_LAW_MARKER@" bridgedLawRoots
  audit "@BRIDGE_MARKER@" bridgeRoots
  IO.println "@OK_MARKER@"
  return 0

end AverCertAudit

def main : IO UInt32 := AverCertAudit.main
