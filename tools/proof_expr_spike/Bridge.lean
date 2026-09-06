import Lean
import ExprSpike

open Lean Meta Elab Term Command

set_option maxHeartbeats 200000
set_option debug.skipKernelTC false
set_option linter.unusedSimpArgs false

namespace ProofExprSpike

theorem andStep {a b : Bool} (ha : a = true) (hb : a = true → b = true) :
    (a && b) = true := by simp_all

def field [FromJson α] (j : Json) (key : String) : TermElabM α :=
  match j.getObjValAs? α key with
  | .ok x => pure x
  | .error e => throwError "{e}"

def parseTactic (text : String) : TermElabM Syntax := do
  match Parser.runParserCategory (← getEnv) `tactic text with
  | .ok stx => return stx
  | .error e => throwError "{e}"

def runTactic (goal : MVarId) (stx : Syntax) : TermElabM Unit := do
  let remaining ← Tactic.run goal <| Tactic.withoutRecover <| Tactic.evalTactic stx
  unless remaining.isEmpty do throwError "unsolved leaves: {remaining}"

/-- Same leaf search and lemma cone for text and Expr. Failed simp rolls back. -/
def leafText (definitions : Array String) : TermElabM String := do
  let mut grind := definitions.toList
  for name in definitions do
    if (← getEnv).contains (name.toName ++ `eq_def) then
      grind := ("= " ++ name ++ ".eq_def") :: grind
  return "(first | (simp_all [" ++ String.intercalate ", " definitions.toList ++
    "]; done) | omega | grind [" ++ String.intercalate ", " grind ++ "])"

/-- A verified left fact is in scope while proving the right fact. -/
partial def facts (goal : MVarId) (leaf : Syntax) (path : String) : TermElabM Unit :=
  goal.withContext do
    let saved ← saveState
    let children? ← try
      pure (some (← goal.apply (mkConst ``andStep)))
    catch _ =>
      saved.restore
      pure none
    match children? with
    | some children =>
      for (child, i) in children.zipIdx do
        let (_, body) ← child.intros
        facts body leaf s!"{path}/fact{i + 1}"
    | none =>
      try runTactic goal leaf
      catch e => throwError "{path}: {e.toMessageData}"

/-- No proof-structure parser/elaborator: these calls assign real Expr values. -/
def exprBuild (type : Expr) (stage : Json) (leaf : Syntax) : TermElabM Expr := do
  let shape : String ← field stage "shape"
  let unfold : String ← field stage "unfold"
  let label : String ← field stage "label"
  let line : Nat ← field stage "line"
  let branchLines : Array Nat ← field stage "branches"
  let root ← mkFreshExprMVar type
  let goal := root.mvarId!
  let finish := fun (body : MVarId) (index : Nat) => do
    let (_, body) ← body.intros
    let body ← if unfold.isEmpty then pure body else unfoldTarget body unfold.toName
    let branch := if shape == "plain" then "" else
      s!"/branch{index + 1}@{branchLines[index]?.getD line}"
    facts body leaf s!"{label}@{line}{branch}"
  if shape == "induction" then
    -- Accumulator parameters remain quantified in the induction hypothesis.
    let (major, rest) ← goal.intro1
    let branches ← rest.induction major ``List.rec
    for (branch, i) in branches.toList.zipIdx do finish branch.mvarId i
  else if shape == "cases" then
    let (major, rest) ← goal.intro1
    let branches ← rest.cases major
    for (branch, i) in branches.toList.zipIdx do finish branch.mvarId i
  else
    finish goal 0
  let proof ← instantiateMVars root
  if proof.hasMVar then throwError "unresolved metavariable"
  return proof

/-- Controlled text lane: same structural plan and leaf solver as exprBuild. -/
def textBuild (type : Expr) (stage : Json) (leaf : String) : TermElabM Expr := do
  let shape : String ← field stage "shape"
  let unfold : String ← field stage "unfold"
  let skeleton :=  if shape == "induction" then "intro major; induction major <;> intros"
    else if shape == "cases" then "intro major; cases major <;> intros"
    else "intros"
  let unfolding := if unfold.isEmpty then "" else "; all_goals unfold " ++ unfold
  let code := "(" ++ skeleton ++ unfolding ++
    "; all_goals (repeat' first | apply ProofExprSpike.andStep | intro)" ++
    "; all_goals " ++ leaf ++ ")"
  let root ← mkFreshExprMVar type
  runTactic root.mvarId! (← parseTactic code)
  let proof ← instantiateMVars root
  if proof.hasMVar then throwError "unresolved metavariable"
  return proof

/-- Reject shortcuts even if hidden behind another declaration. -/
partial def auditDependencies (pending : List Name) (seen : NameSet := {}) :
    TermElabM Unit := do
  match pending with
  | [] => pure ()
  | name :: rest =>
    if seen.contains name then return ← auditDependencies rest seen
    let seen := seen.insert name
    if (name.toString.splitOn "_law_").length > 1 ||
        name.toString.startsWith "SpikeStage." || name.toString.startsWith "SpikeLaw." then
      throwError "baseline law dependency forbidden: {name}"
    let info ← getConstInfo name
    let more := info.type.getUsedConstants.toList ++
      (info.value?.map (·.getUsedConstants.toList)).getD []
    auditDependencies (more ++ rest) seen

/-- addDecl schedules checking asynchronously; force the kernel result for measurement. -/
def kernelCheck (decl : Declaration) : TermElabM Unit := do
  let env := (← getEnv).toKernelEnv
  match Kernel.Environment.addDecl env (← getOptions) decl with
  | .ok _ => pure ()
  | .error e => throwKernelException e

def checked (name : Name) (type proof : Expr) : TermElabM (Nat × Array Name) := do
  if proof.hasMVar || proof.hasFVar then throwError "open proof term"
  auditDependencies proof.getUsedConstants.toList
  let start ← IO.monoNanosNow
  let declaration := Declaration.thmDecl {
    name := name
    levelParams := []
    type := type
    value := proof
  }
  kernelCheck declaration
  let elapsed := (← IO.monoNanosNow) - start
  addDecl declaration
  let axioms ← collectAxioms name
  for ax in axioms do
    unless #[``propext, ``Classical.choice, ``Quot.sound].contains ax do
      throwError "untrusted axiom: {ax}"
  return (elapsed, axioms)

/-- Lambda/application composition uses freshly constructed proof values. -/
def compose (type : Expr) (givenCount : Nat) (guard : Bool) (proofs : Array Expr) :
    TermElabM Expr := do
  forallTelescope type fun args _ => do
    unless args.size == givenCount + (if guard then 1 else 0) do
      throwError "unexpected original statement binders"
    let givens := args.extract 0 givenCount
    let guards := args.extract givenCount args.size
    let mut reasons := #[]
    for proof in proofs[:proofs.size - 1] do
      reasons := reasons.push (mkAppN proof (givens ++ reasons ++ guards))
    mkLambdaFVars args (mkAppN proofs.back! (givens ++ reasons ++ guards))

def runLaw (law : Json) (lane : String) (index : Nat) (output : System.FilePath) :
    TermElabM Json := do
  let target : String ← field law "theorem"
  let definitions : Array String ← field law "definitions"
  let stages : Array Json ← field law "stages"
  let leaf ← leafText definitions
  let mut proofs := #[]
  let mut rows := #[]
  for (stage, i) in stages.toList.zipIdx do
    let label : String ← field stage "label"
    let type := (← getConstInfo (← field stage "theorem" : String).toName).type
    let start ← IO.monoNanosNow
    let proof ← try
      withCurrHeartbeats do
        if lane == "expr" then exprBuild type stage (← parseTactic leaf)
        else textBuild type stage leaf
    catch e =>
      return Json.mkObj [("law", toJson target), ("lane", toJson lane),
        ("status", toJson "rejected"), ("stage", toJson label),
        ("diagnostic", toJson (← e.toMessageData.toString)), ("stages", toJson rows)]
    let elapsed := (← IO.monoNanosNow) - start
    let (checkNs, axioms) ← checked (`SpikeStage ++ s!"{lane}_{index}_{i}".toName) type proof
    proofs := proofs.push proof
    rows := rows.push <| Json.mkObj [("label", toJson label),
      ("construct_ns", toJson elapsed), ("check_ns", toJson checkNs),
      ("axioms", toJson (axioms.map Name.toString))]
  let info ← getConstInfo target.toName
  let start ← IO.monoNanosNow
  let proof ← compose info.type (← field law "given_count") (← field law "guard") proofs
  let composeNs := (← IO.monoNanosNow) - start
  let (checkNs, axioms) ← checked (`SpikeLaw ++ s!"{lane}_{index}".toName) info.type proof
  -- Raw Expr is a machine artifact; the source plan remains the readable view.
  let pretty := (← ppExpr proof).pretty
  let raw := reprStr proof
  IO.FS.writeFile (output / s!"{lane}-{index}.term.txt") pretty
  IO.FS.writeFile (output / s!"{lane}-{index}.expr.txt") raw
  return Json.mkObj [("law", toJson target), ("lane", toJson lane),
    ("status", toJson "universal"), ("stages", toJson rows),
    ("compose_ns", toJson composeNs), ("check_ns", toJson checkNs),
    ("pretty_bytes", toJson pretty.utf8ByteSize), ("raw_bytes", toJson raw.utf8ByteSize),
    ("axioms", toJson (axioms.map Name.toString))]

elab "#run_spike " path:str output:str : command => liftTermElabM do
  let plan ← match Json.parse (← IO.FS.readFile path.getString) with
    | .ok j => pure j
    | .error e => throwError "{e}"
  let out : System.FilePath := output.getString
  IO.FS.createDirAll out
  let laws : Array Json ← field plan "laws"
  let mut rows := #[]
  let lanes := (plan.getObjValAs? (Array String) "lanes").toOption.getD #["text", "expr"]
  for lane in lanes do
    for (law, i) in laws.toList.zipIdx do
      -- Fresh search caches prevent one lane from benefiting from another.
      rows := rows.push (← withFreshCache <| runLaw law lane i out)
  -- A malformed proof must be rejected by the kernel, not merely by our audit.
  let type := (← getConstInfo (← field laws[0]! "theorem" : String).toName).type
  let saved ← saveState
  let rejected ← try
    kernelCheck (.thmDecl {
      name := `SpikeCorrupt
      levelParams := []
      type := type
      value := mkConst ``True.intro
    })
    pure false
  catch _ => pure true
  saved.restore
  unless rejected do throwError "kernel accepted the corruption canary"
  let shortcutRejected ← try
    auditDependencies [(← field laws[0]! "theorem" : String).toName]
    pure false
  catch _ => pure true
  unless shortcutRejected do throwError "baseline shortcut escaped the audit"
  IO.FS.writeFile (out / "results.json") (Json.pretty <| Json.mkObj [
    ("laws", toJson rows), ("corrupt_rejected", toJson rejected), ("shortcut_rejected", toJson shortcutRejected)])
  logInfo "Expr spike completed; results.json contains both lanes and rejection diagnostics."

end ProofExprSpike
