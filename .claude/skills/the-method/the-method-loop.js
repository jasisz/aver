export const meta = {
  name: 'the-method',
  description: 'The Method: an agent proposes helper lemmas -> aver tests -> Lean/Z3 judges -> refine, with an independent verify gate. Closes open Aver proof laws on any Aver project.',
  phases: [
    { title: 'Method', detail: 'one agent per open task: propose helper lemmas, test with aver, refine until Lean closes or budget out' },
    { title: 'Verify', detail: "independent re-verify of each self-reported closure (fresh dir) — the proposer's own verdict is not trusted; on success the verified decomposition is PERSISTED to decomposed/ so it is recoverable, never re-guessed" },
  ],
}

// Inputs (via Workflow `args`): either an array of task .av paths, or { tasks: [...], attempts? }.
// Paths are relative to the project root (the cwd the workflow runs in) or absolute.
let a = typeof args === 'string'
  ? (() => { try { return JSON.parse(args) } catch { return args.trim() ? [args.trim()] : null } })()
  : args
const TASKS = Array.isArray(a) ? a : (a && Array.isArray(a.tasks) ? a.tasks : [])
const MAX_ATTEMPTS = (a && a.attempts) || 4
const MODEL = (a && a.model) || undefined  // optional model override for the proposer (e.g. 'haiku','sonnet'); undefined = inherit
if (!TASKS.length) {
  log('the-method: pass one or more Aver task .av paths as args, e.g. { tasks: ["path/to/task.av"] }')
  return { error: 'no tasks given' }
}
log(`The Method: ${TASKS.length} task(s), up to ${MAX_ATTEMPTS} attempts each${MODEL ? `, proposer model=${MODEL}` : ''}`)

const RESULT_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['task', 'closed', 'attempts', 'helperLaws', 'finalError', 'summary'],
  properties: {
    task: { type: 'string' },
    closed: { type: 'boolean', description: 'true iff the augmented task reached "universal":true on Lean' },
    attempts: { type: 'integer' },
    helperLaws: {
      type: 'array',
      items: {
        type: 'object', additionalProperties: false, required: ['name', 'source'],
        properties: { name: { type: 'string' }, source: { type: 'string', description: 'the helper law as valid Aver source' } },
      },
    },
    finalError: { type: 'string', description: 'empty if closed, else short Lean error / reason it stayed open' },
    summary: { type: 'string', description: '2-3 sentences: what was tried and why it did/did not work' },
  },
}

const FIND_AVER = 'Locate the aver binary: try ./target/release/aver, then ./target/debug/aver, then `aver` on PATH; if none exists, run `cargo build --bin aver` first.'
const SAFETY = 'SAFETY: READ-ONLY on the project. Do ALL edits on COPIES under /tmp. Never run state-changing git commands and never modify project files.'
// The verify gate is the ONE agent allowed a single project write: persisting the verified
// decomposition to decomposed/ so wins are durable on disk during the run (not scraped from a
// possibly-truncated result), and so re-runs replay instead of re-guessing.
const VERIFY_SAFETY = 'SAFETY: do ALL proof/verification work on COPIES under /tmp. The ONLY project-file write you may make is creating/overwriting the single decomposed entry described below. Never run state-changing git commands; never touch any other project file.'

const METHOD_PROMPT = (t) => `You ARE "The Method": close an OPEN Aver proof obligation by proposing auxiliary helper lemmas, testing them with the Aver toolchain, and refining on failure. You propose; the Lean kernel / Z3 judges.

YOU ARE A CONJECTURER, NOT A PROVER. Reason ONLY about Aver: the datatypes, the functions, the open law, and what auxiliary Aver law would unblock it. Do NOT open, read, or reason about the generated Lean/Dafny files; do NOT reason about Lean tactics, induction strategy, the proof residual / goal state, simp sets, fuel, or the Aver prover's internals — discharging the proof is the toolchain's job, not yours. Your ONLY output is the right Aver helper law(s). If an attempt does not close, propose a DIFFERENT or additional Aver LAW (or fix the law's statement / sample domains) — never try to debug or fix the proof itself. Keep your reasoning short: long tactic-level analysis means you have drifted out of your job and should step back to "what true Aver law is missing?".

You are working inside an Aver project (the current working directory).
- ${FIND_AVER}
- TARGET task file (contains an OPEN "verify ... law"): ${t}  (relative to the project root, or absolute).

${SAFETY}

STEP 1 — understand the mechanism for THIS project:
- Read the target task: its datatypes, functions, and the OPEN law.
- If the project has example decompositions (e.g. a "decomposed/" directory of already-solved tasks), read one solved task plus its base to learn EXACTLY how helper laws are written into a .av. The closing command sequence is:
    <aver> proof <scratch.av> --discover -o <dir>            (proves + commits the helper lemmas)
    <aver> proof <scratch.av> --check --check-json --backend lean -o <dir>   (SAME dir; closed <=> output contains "universal":true)

STEP 2 — the loop (max ${MAX_ATTEMPTS} attempts; stop the instant it closes):
1. Propose 1-3 TRUE, GENERAL helper laws about the task's OWN functions that the main proof needs — a missing homomorphism / associativity / distributivity / an equation relating subterms of the goal. Do NOT merely restate the goal.
2. Copy the target to a fresh /tmp scratch and splice the helper laws in BEFORE the target "verify ... law" line. ORDER MATTERS (appending at the END can fail to fire); RENDERING MATTERS (how a constructor/operator is written changes how it elaborates). Valid Aver only: first-order, no closures.
3. Run discover then check into a FRESH /tmp dir. Closed <=> "universal":true with "sorries":0.
4. Retry once on a transient lake error. On failure, read the Lean error / see which helper failed, refine, try again.

Return: task="${t}"; closed (bool); attempts used; helperLaws = the WINNING set [{name, source}] if closed (else your best attempt); finalError ("" if closed); summary (2-3 sentences).`

// VERIFY GATE — an autonomous agent's self-reported `closed` is not trustworthy on its own. An
// INDEPENDENT agent re-checks each claimed closure from scratch before we count it.
const VERIFY_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['verified', 'note', 'persistedPath'],
  properties: {
    verified: { type: 'boolean', description: 'true ONLY if you yourself observed "universal":true with "sorries":0 on the augmented task' },
    note: { type: 'string', description: '1-2 sentences: observed universal/sorries values, or why it failed' },
    persistedPath: { type: 'string', description: 'project-relative path of the decomposed/ entry you wrote and re-confirmed (empty string if verified=false or the persist step failed)' },
  },
}

const VERIFY_PROMPT = (t, helperLaws) => `INDEPENDENT verification gate. Do NOT trust any prior "closed" claim — verify from scratch yourself.
You are in an Aver project. ${FIND_AVER}
- TARGET (OPEN in baseline): ${t}
- candidate helper laws (JSON): ${JSON.stringify(helperLaws)}

Steps:
1. Copy ${t} to a fresh /tmp scratch.
2. Splice the helper laws in BEFORE the target "verify ... law" line (order and rendering matter — appending at the end can fail). Use the helper source strings verbatim.
3. Run: <aver> proof <scratch> --discover -o <freshdir> ; then <aver> proof <scratch> --check --check-json --backend lean -o <freshdir>.
4. verified = true ONLY if the check output contains "universal":true AND "sorries":0. Retry once on a transient lake error.
5. Sanity: confirm the BASE task (no helpers) is still OPEN ("universal":true ABSENT).
6. PERSIST (only if verified) — make the win durable and recoverable so it is never re-guessed:
   - Destination = the target path with its "/tip/" segment replaced by "/decomposed/" (e.g. proof-corpus/tip/prod/prop_38.av -> proof-corpus/decomposed/prod/prop_38.av; proof-corpus/tip/isaplanner/prop_76.av -> proof-corpus/decomposed/isaplanner/prop_76.av). If the path has no "/tip/" segment, use proof-corpus/decomposed/<basename>. Create parent dirs if needed.
   - Convention (read one existing decomposed/ entry + its base to match it EXACTLY): the entry is the base file with the helper laws (and any helper "fn" definitions they introduce) spliced in BEFORE the target "verify ... law" block — same module name, same functions, nothing else changed.
   - Write your verified augmented .av to that destination, then re-run the closing sequence ON THE WRITTEN FILE and confirm it still shows "universal":true,"sorries":0. Set persistedPath to that project-relative path.
   - If verified=false, or the write or re-check fails, set persistedPath="".
${VERIFY_SAFETY}
Return: verified (bool), note, persistedPath.`

phase('Method')
// pipeline: each task is proposed, then (if self-reported closed) independently verified — no
// barrier, so verification starts the moment an agent finishes.
const results = (await pipeline(
  TASKS,
  (t) => agent(METHOD_PROMPT(t), { label: `method:${t}`, phase: 'Method', schema: RESULT_SCHEMA, agentType: 'general-purpose', model: MODEL }),
  (r, t) => {
    if (!r) return { task: t, closed: false, verified: false, attempts: 0, helperLaws: [], finalError: 'agent died', summary: '' }
    if (!r.closed) return { ...r, verified: false }
    return agent(VERIFY_PROMPT(r.task, r.helperLaws), { label: `verify:${r.task}`, phase: 'Verify', schema: VERIFY_SCHEMA, agentType: 'general-purpose' })
      .then((v) => ({ ...r, verified: !!(v && v.verified), verifyNote: v ? v.note : 'verifier died', persistedPath: v ? (v.persistedPath || '') : '' }))
  },
)).filter(Boolean)

for (const r of results) {
  const status = r.closed ? (r.verified ? 'CLOSED+VERIFIED' : 'self-CLOSED but VERIFY FAILED') : 'open'
  log(`${r.task}: ${status}`)
}
const verifiedClosed = results.filter((r) => r.closed && r.verified)
const overReported = results.filter((r) => r.closed && !r.verified)
const persisted = verifiedClosed.filter((r) => r.persistedPath)
log(`The Method: ${verifiedClosed.length}/${results.length} verified-closed${overReported.length ? ` (${overReported.length} self-reported but failed verification)` : ''}; ${persisted.length} persisted to decomposed/`)
const notPersisted = verifiedClosed.filter((r) => !r.persistedPath)
if (notPersisted.length) log(`WARNING: ${notPersisted.length} verified win(s) NOT persisted: ${notPersisted.map((r) => r.task).join(', ')}`)
return {
  verifiedClosed: verifiedClosed.length,
  selfReported: results.filter((r) => r.closed).length,
  total: results.length,
  persistedCount: persisted.length,
  persistedPaths: persisted.map((r) => r.persistedPath),
  winners: verifiedClosed.map((r) => ({ task: r.task, helperLaws: r.helperLaws, persistedPath: r.persistedPath || '' })),
  results,
}
