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
const MODEL = (a && a.model) || undefined  // optional model override for the CONJECTURER (e.g. 'haiku','sonnet'); undefined = inherit (session model).
// The RUNNER is mechanical, BUT it must read the --check-json verdict ACCURATELY: the loop only
// invokes the verify gate on a self-reported close, so a runner that UNDER-reports (says open when
// universal:true) silently drops a genuine win — measured: a haiku runner under-reported a real
// self-contained closure (prop_06), and the cost saving was marginal (the token cost is dominated by
// the aver/lake output it ingests + the verify gate, not the runner's model). So the runner defaults
// to the session model; pass runnerModel:'haiku' only if you accept that lost-win risk.
const RUNNER_MODEL = (a && a.runnerModel) || undefined
// The verify gate stays on the default (session) model unless overridden: it is the trust anchor and
// does the careful persist-to-decomposed/ write.
const VERIFIER_MODEL = (a && a.verifierModel) || undefined
if (!TASKS.length) {
  log('the-method: pass one or more Aver task .av paths as args, e.g. { tasks: ["path/to/task.av"] }')
  return { error: 'no tasks given' }
}
log(`The Method: ${TASKS.length} task(s), up to ${MAX_ATTEMPTS} attempts each${MODEL ? `, conjecturer=${MODEL}` : ''}${RUNNER_MODEL ? `, runner=${RUNNER_MODEL}` : ''}${VERIFIER_MODEL ? `, verifier=${VERIFIER_MODEL}` : ''}`)

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
  required: ['checkPassed', 'verifyClean', 'universal', 'sorries', 'lawStatus', 'note'],
  properties: {
    checkPassed: { type: 'boolean', description: 'false iff the `aver check` pre-flight reported a parse/type error (the spliced helper is malformed) — in that case neither verify nor the proof was run and the note carries the verbatim aver error' },
    verifyClean: { type: 'boolean', description: 'result of the CHEAP `aver verify` sieve (bounded sample eval, no Lean) run BEFORE the proof: true iff every law (helpers + target) passes with ZERO violations. false means a law is FALSE on its samples (e.g. a Nat-vs-Int bridge) — a real Aver-level refutation the kernel can miss; the proof is then NOT run.' },
    universal: { type: 'boolean', description: 'value of "universal" from the --check-json line (only meaningful when verifyClean=true; the proof is skipped otherwise)' },
    sorries: { type: 'integer', description: 'value of "sorries" from the --check-json line' },
    lawStatus: {
      type: 'array',
      description: 'per-law status read from proof_manifest.json (laws[].tier; a declared law ABSENT from the manifest is open). AVER-LEVEL ONLY — map tiers to these words; do NOT include Lean axiom names, theorem names, or the word "sorry".',
      items: {
        type: 'object', additionalProperties: false, required: ['law', 'status'],
        properties: {
          law: { type: 'string', description: 'the Aver law name (e.g. "appendBridge", or the target law name)' },
          status: { type: 'string', enum: ['proven', 'sample-only', 'open', 'false-on-samples'], description: 'proven = manifest tier universal (kernel-genuine); sample-only = tier bounded (passes samples, not universal); open = declared but absent from the manifest (unproved); false-on-samples = `aver verify` found a counterexample (the law is FALSE / type-confused, e.g. a Nat-vs-Int bridge — must be replaced, not laddered)' },
        },
      },
    },
    note: { type: 'string', description: 'ONE line, AVER-LEVEL ONLY (e.g. "all helpers proven, target open" or a parse/type error). NEVER quote Lean/Dafny, tactics, the proof residual, or Lean axiom names.' },
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
${history.length ? `- Aver-level outcome of previous attempt(s) — each has a per-law \`lawStatus\` map (proven = holds universally; sample-only = passes samples but NOT universally, so restate/strengthen it; open = the law, or the TARGET, is still unproved). These are FACTS about which Aver laws closed, NOT prover internals — read them to decide your next law: if every helper is "proven" but the target is "open", you are MISSING a law (add the next one / ladder deeper); if a helper is "sample-only" or "open", that helper itself is wrong or too weak — fix or replace it. Do NOT reason about WHY the target's proof fails (that is the prover's job):\n${JSON.stringify(history)}` : '- This is the first attempt.'}

FIRST read \`llms.txt\` at the project root to lock the EXACT Aver grammar (the \`verify <fn> law\` block, \`given <name>: <Type> = [<samples>]\`, the single \`lhs => rhs\` equation). Then read the target (its datatypes, functions, the open law); if a decomposed/ directory exists, read one solved entry + its base to learn EXACTLY how helper laws (and any fn they introduce) are written as valid Aver. Return TRUE, GENERAL helper laws the proof needs (a missing homomorphism / associativity / distributivity / an equation relating subterms of the goal) — do NOT restate the goal.
There is NO discovery backfill: every auxiliary law the proof needs must be in YOUR proposed set. In particular, when a task fn is DEFINED in terms of a builtin (e.g. \`append\` built on \`List.concat\`, or fns whose proof must line up with \`List.len\`/\`List.reverse\`), include the BRIDGE law for EVERY such builtin used, not just some — e.g. \`append(x, y) => List.concat(x, y)\`, \`length(x) => List.len(x)\`, \`rev(x) => List.reverse(x)\`. A common miss is supplying two bridges but forgetting the third.
Valid Aver only: first-order, no closures, no match/if/chained-=> in a law body (use a recursive helper fn instead); a given's declared type MUST match its sample values. Keep rationale short and Aver-level.`

const RUN_PROMPT = (t, helperLaws) => `Mechanically test a fixed helper-law set against an Aver task and report ONLY the Aver-level verdict.
- TARGET task: ${t}  (relative to the project root, or absolute).
- helper laws to splice (JSON, verbatim): ${JSON.stringify(helperLaws)}

Copy ${t} to a fresh /tmp scratch; splice the helpers (and any fn they introduce) in BEFORE the target "verify … law" line (order + rendering matter). PRE-FLIGHT FIRST: run \`<aver> check <scratch>\` — block ONLY on a real parse/type error (\`error[parse...]\` or \`error[type-error]\`); IGNORE \`error[missing-verify]\` and \`error[module-size]\` (project-hygiene lints, NOT proof problems — a helper referencing a task fn like plus/lessEq without its own verify block is fine and proves regardless). If there is a genuine parse/type error, STOP, do NOT run anything else, and return checkPassed:false, verifyClean:false, universal:false, sorries:0, note=that verbatim error. CHEAP SIEVE NEXT — run \`<aver> verify <scratch>\` (bounded sample eval, NO Lean): if ANY law is violated (a "✗"/"verify-mismatch"/"law violated"), set verifyClean:false, STOP, do NOT run the proof, and report which law is false on samples (status false-on-samples in lawStatus, note e.g. "law lengthBridge violated bounded verify 0/3 — false on samples, not a valid Aver law"). This catches a false / type-confused law (e.g. a Nat-vs-Int bridge) instantly and independently of Lean. Only if verify is fully clean (verifyClean:true): run \`<aver> proof <scratch> --check --check-json --backend lean -o <dir>\` (retry once on a transient lake error). Do NOT use --discover — the inline proposed laws are the ONLY auxiliary lemmas; the goal must close by them + our auto-prover, not by the enumerative recognizer. Read the --check-json summary line for universal/sorries, AND read \`<dir>/proof_manifest.json\` for per-law status: its \`laws[]\` lists each tiered law as {law, tier} (tier "universal" -> status "proven"; "bounded" -> "sample-only"); a law you spliced (or the target law) that is ABSENT from laws[] is "open". Return checkPassed:true, verifyClean:true, universal, sorries, lawStatus (one entry per helper + the target, using proven/sample-only/open), and a one-line AVER-LEVEL note. Do NOT read or reason about the generated Lean/Dafny, tactics, the proof residual, or the manifest's \`axioms\`/\`theorem\` fields (Lean internals) — those never go in your output.`

const VERIFY_PROMPT = (t, helperLaws) => `INDEPENDENT verification gate. Do NOT trust any prior "closed" claim — verify from scratch yourself, then persist if it holds.
- TARGET (OPEN in baseline): ${t}
- candidate helper laws (JSON): ${JSON.stringify(helperLaws)}

Copy ${t} to a fresh /tmp scratch; splice the helpers in BEFORE the target "verify … law" line (use the source strings verbatim; order + rendering matter). FIRST run the CHEAP SIEVE \`<aver> verify <scratch>\` (bounded, no Lean): EVERY law (helpers + target) must pass with ZERO violations — a "✗"/verify-mismatch means a law is FALSE on samples (e.g. a Nat-returning fn bridged to an Int-returning builtin), a real Aver-level refutation the kernel can wrongly accept, so verified=false immediately. Only if verify is clean: run \`<aver> proof <scratch> --check --check-json --backend lean -o <freshdir>\` (retry once on a transient lake error). Do NOT use --discover — closure must be self-contained (the inline laws + our auto-prover only). verified=true ONLY if BOTH aver verify is clean AND the check output has "universal":true with "sorries":0. Confirm the helpers are actually NEEDED by running the PRISTINE original ${t} UNCHANGED (no copy, no splice) in its OWN fresh -o dir, SEPARATE from every spliced dir: \`<aver> proof ${t} --check --check-json --backend lean -o <BASE_DIR>\`. Parse the LAST \`^{\` JSON line from THAT command's own stdout — NEVER reuse the spliced scratch file or the spliced -o dir (its proof_manifest.json + cached lake build encode the CLOSED result and will lie). The base must be OPEN ("universal":false / "sorries">0 / "universal_laws":0); the spliced run reports "universal_laws":N with N = #helpers+1, so a genuine decomposition has the base at "universal_laws":0. If the pristine base already shows "universal":true,"sorries":0, the helpers were never needed → verified=false. If verified, PERSIST: destination = the target path with its "/tip/" segment replaced by "/decomposed/" (no "/tip/" → proof-corpus/decomposed/<basename>); match an existing decomposed/ entry's convention EXACTLY (base file + spliced helpers/fns, nothing else); write it, re-run BOTH \`<aver> verify <written>\` (zero violations) AND \`<aver> proof <written> --check --check-json --backend lean\` ("universal":true,"sorries":0) on the WRITTEN file, and set persistedPath. Else persistedPath="". Return verified, note, persistedPath.`

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
      { label: `run:${t}#${attempt}`, phase: 'Method', schema: RUN_SCHEMA, agentType: 'the-method-runner', model: RUNNER_MODEL })
    if (run && run.verifyClean === true && run.universal === true && run.sorries === 0) {
      won = { helperLaws: prop.helperLaws, rationale: prop.rationale || '' }
      lastNote = ''
      break
    }
    lastNote = run ? run.note : 'runner died'
    history.push({ laws: prop.helperLaws, note: lastNote, lawStatus: run ? (run.lawStatus || []) : [], verifyClean: run ? run.verifyClean : false })
  }

  const base = { task: t, attempts: attemptsUsed }
  if (!won) {
    return { ...base, closed: false, verified: false, helperLaws: history.length ? (history[history.length - 1].laws || []) : [], finalError: lastNote || 'did not close within attempt cap', persistedPath: '' }
  }
  const v = await agent(VERIFY_PROMPT(t, won.helperLaws),
    { label: `verify:${t}`, phase: 'Verify', schema: VERIFY_SCHEMA, agentType: 'the-method-verifier', model: VERIFIER_MODEL })
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
