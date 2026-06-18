export const meta = {
  name: 'the-method',
  description: 'The Method: a CONJECTURER agent proposes helper lemmas -> a separate RUNNER agent tests with aver -> Lean/Z3 judges -> refine, with an independent verify gate. The conjecturer is capability-fenced (read-only, no toolchain, never sees the proof) so it cannot drift into tactic/prover debugging. Closes open Aver proof laws on any Aver project.',
  phases: [
    { title: 'Method', detail: 'per open task: a read-only conjecturer proposes helper laws; a mechanical runner splices + tests with aver and returns ONLY the Aver-level verdict; refine until Lean closes or the attempt cap is hit' },
    { title: 'Verify', detail: "independent re-verify of each self-reported closure (fresh dir) — the run's own verdict is not trusted; on success the verified decomposition is PERSISTED to decomposed/ so it is recoverable, never re-guessed" },
  ],
}

// Inputs (via Workflow `args`): either an array of task .av paths, or { tasks: [...], attempts? , model? }.
// Paths are relative to the project root (the cwd the workflow runs in) or absolute.
let a = typeof args === 'string'
  ? (() => { try { return JSON.parse(args) } catch { return args.trim() ? [args.trim()] : null } })()
  : args
const TASKS = Array.isArray(a) ? a : (a && Array.isArray(a.tasks) ? a.tasks : [])
const MAX_ATTEMPTS = (a && a.attempts) || 3
const MODEL = (a && a.model) || undefined  // optional model override for the CONJECTURER only (e.g. 'haiku','sonnet'); undefined = inherit. Runner + verify gate stay default = trustworthy.
if (!TASKS.length) {
  log('the-method: pass one or more Aver task .av paths as args, e.g. { tasks: ["path/to/task.av"] }')
  return { error: 'no tasks given' }
}
log(`The Method: ${TASKS.length} task(s), up to ${MAX_ATTEMPTS} attempts each${MODEL ? `, conjecturer model=${MODEL}` : ''}`)

// ---- THE CAPABILITY FENCE ----------------------------------------------------------------------
// The proposer/runner split is the fence: "agent proposes, kernel decides" made STRUCTURAL, not
// asked. The conjecturer (`the-method-proposer`, tools: Read+Glob ONLY) physically cannot run the
// toolchain or open a generated .lean/.dfy, so it cannot slide from conjecturing into debugging
// tactics/the proof residual — the measured dominant cost sink (a "must close" model spiralling on
// the Lean goal state). The runner (`the-method-runner`) does the mechanical splice+run and returns
// ONLY an Aver-level verdict; the Lean residual never crosses back into the conjecturer's context.
// See feedback_explain_residual_internal_only + project_tip_isaplanner_split (cost diagnosis).

const LAWS_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['helperLaws', 'rationale'],
  properties: {
    helperLaws: {
      type: 'array',
      items: {
        type: 'object', additionalProperties: false, required: ['name', 'source'],
        properties: { name: { type: 'string' }, source: { type: 'string', description: 'the helper law (plus any fn it introduces) as valid first-order Aver source' } },
      },
    },
    rationale: { type: 'string', description: '1-2 sentences, Aver-level only: what these laws say and why the proof needs them' },
  },
}

const RUN_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['universal', 'sorries', 'allHelpersVerified', 'note'],
  properties: {
    universal: { type: 'boolean', description: 'value of "universal" from the --check-json line' },
    sorries: { type: 'integer', description: 'value of "sorries" from the --check-json line' },
    allHelpersVerified: { type: 'boolean', description: 'true iff every spliced helper passed its own bounded check' },
    note: { type: 'string', description: 'ONE line, AVER-LEVEL ONLY (which helper failed its bounded check, or "all helpers verified, target law still open"). NEVER quote Lean/Dafny, tactics, or the proof residual.' },
  },
}

const VERIFY_SCHEMA = {
  type: 'object', additionalProperties: false,
  required: ['verified', 'note', 'persistedPath'],
  properties: {
    verified: { type: 'boolean', description: 'true ONLY if you yourself observed "universal":true with "sorries":0 on the augmented task' },
    note: { type: 'string', description: '1-2 sentences: observed universal/sorries values, or why it failed' },
    persistedPath: { type: 'string', description: 'project-relative path of the decomposed/ entry you wrote and re-confirmed (empty string if verified=false or the persist step failed)' },
  },
}

const PROPOSE_PROMPT = (t, attempt, history) => `Propose the Aver helper law(s) to unblock an OPEN proof obligation. You are the CONJECTURER (read-only; you cannot run anything or see any proof — that is by design).

- TARGET task file (contains the OPEN "verify … law"): ${t}  (relative to the project root, or absolute).
- Attempt ${attempt} of ${MAX_ATTEMPTS}.
${history.length ? `- Aver-level outcome of previous attempt(s) (NO prover internals — use it to propose a DIFFERENT/ADDITIONAL law, fix a statement/sample domain, or ladder one level deeper):\n${JSON.stringify(history)}` : '- This is the first attempt.'}

Read the target (its datatypes, functions, the open law). If a decomposed/ directory exists, read one solved entry + its base to learn EXACTLY how helper laws (and any fn they introduce) are written as valid Aver. Then return 1-3 TRUE, GENERAL helper laws about the task's OWN functions that the proof needs (a missing homomorphism / associativity / distributivity / an equation relating subterms of the goal) — do NOT restate the goal. Valid Aver only: first-order, no closures. Keep rationale short and Aver-level.`

const RUN_PROMPT = (t, helperLaws) => `Mechanically test a fixed helper-law set against an Aver task and report ONLY the Aver-level verdict.
- TARGET task: ${t}  (relative to the project root, or absolute).
- helper laws to splice (JSON, verbatim): ${JSON.stringify(helperLaws)}

Copy ${t} to a fresh /tmp scratch; splice the helpers (and any fn they introduce) in BEFORE the target "verify … law" line (order + rendering matter); run \`<aver> proof <scratch> --discover -o <dir>\` then \`<aver> proof <scratch> --check --check-json --backend lean -o <dir>\` (retry once on a transient lake error). Read ONLY the --check-json summary line. Return universal, sorries, allHelpersVerified, and a one-line AVER-LEVEL note. Do NOT read or reason about the generated Lean/Dafny, tactics, or the proof residual.`

const VERIFY_PROMPT = (t, helperLaws) => `INDEPENDENT verification gate. Do NOT trust any prior "closed" claim — verify from scratch yourself, then persist if it holds.
- TARGET (OPEN in baseline): ${t}
- candidate helper laws (JSON): ${JSON.stringify(helperLaws)}

Copy ${t} to a fresh /tmp scratch; splice the helpers in BEFORE the target "verify … law" line (use the source strings verbatim; order + rendering matter); run \`<aver> proof <scratch> --discover -o <freshdir>\` then \`<aver> proof <scratch> --check --check-json --backend lean -o <freshdir>\` (retry once on a transient lake error). verified=true ONLY if the check output contains "universal":true AND "sorries":0. Confirm the BASE task (no helpers) is still OPEN. If verified, PERSIST: destination = the target path with its "/tip/" segment replaced by "/decomposed/" (no "/tip/" → proof-corpus/decomposed/<basename>); match an existing decomposed/ entry's convention EXACTLY (base file + spliced helpers/fns, nothing else); write it, re-run the closing sequence on the WRITTEN file, confirm "universal":true,"sorries":0, and set persistedPath. Else persistedPath="". Return verified, note, persistedPath.`

phase('Method')

// One independent chain per task, run concurrently. Within a chain the propose->run loop is
// sequential (each attempt refines on the prior Aver-level verdict); on a close, the verify gate
// runs. Wrapped in thunks so cross-task concurrency is preserved; phase is set explicitly on each
// agent (parallel/await => avoid racing the global phase() cursor).
async function runOneTask(t) {
  const history = []
  let won = null
  let lastNote = ''
  let attemptsUsed = 0
  for (let attempt = 1; attempt <= MAX_ATTEMPTS; attempt++) {
    attemptsUsed = attempt
    const prop = await agent(PROPOSE_PROMPT(t, attempt, history),
      { label: `propose:${t}#${attempt}`, phase: 'Method', schema: LAWS_SCHEMA, agentType: 'the-method-proposer', model: MODEL })
    if (!prop || !Array.isArray(prop.helperLaws) || !prop.helperLaws.length) {
      lastNote = prop ? 'conjecturer returned no laws' : 'conjecturer died'
      history.push({ laws: [], note: lastNote })
      continue
    }
    const run = await agent(RUN_PROMPT(t, prop.helperLaws),
      { label: `run:${t}#${attempt}`, phase: 'Method', schema: RUN_SCHEMA, agentType: 'the-method-runner' })
    if (run && run.universal === true && run.sorries === 0) {
      won = { helperLaws: prop.helperLaws, rationale: prop.rationale || '' }
      lastNote = ''
      break
    }
    lastNote = run ? run.note : 'runner died'
    history.push({ laws: prop.helperLaws, note: lastNote, allHelpersVerified: run ? run.allHelpersVerified : false })
  }

  const base = { task: t, attempts: attemptsUsed }
  if (!won) {
    return { ...base, closed: false, verified: false, helperLaws: history.length ? (history[history.length - 1].laws || []) : [], finalError: lastNote || 'did not close within attempt cap', persistedPath: '' }
  }
  const v = await agent(VERIFY_PROMPT(t, won.helperLaws),
    { label: `verify:${t}`, phase: 'Verify', schema: VERIFY_SCHEMA, agentType: 'the-method-verifier' })
  return {
    ...base,
    closed: true,
    helperLaws: won.helperLaws,
    finalError: '',
    summary: won.rationale,
    verified: !!(v && v.verified),
    verifyNote: v ? v.note : 'verifier died',
    persistedPath: v ? (v.persistedPath || '') : '',
  }
}

const results = (await parallel(TASKS.map((t) => () => runOneTask(t)))).filter(Boolean)

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
