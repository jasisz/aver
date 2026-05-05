// Browser-side mirror of `aver::replay::EffectReplayState`. Drives
// `--record` / `--replay` semantics for the native wasm-gc playground
// path: the host (`AverBrowserHost`) routes every effect call through
// `record_or_dispatch`, which appends to the trace in Recording mode
// or pulls the next outcome from the trace in Replay mode.
//
// Trace JSON is byte-compatible with the VM / self-host / native
// wasm-gc CLI recorders: same `(group_id, branch_path,
// effect_occurrence)` tuple, same `$ok` / `$err` / `$some` / `$none`
// / `$record` markers, same `RecordedOutcome::Value` /
// `RuntimeError` discriminator. A trace dropped onto the playground
// from `aver run --record` (any backend) replays through this
// machinery; a trace recorded here downloads as a plain `.replay.json`
// the CLI replayer accepts.

export const REPLAY_MODE = Object.freeze({
    NORMAL: "normal",
    RECORDING: "recording",
    REPLAYING: "replaying",
});

export class EffectReplayState {
    constructor() {
        this.mode = REPLAY_MODE.NORMAL;
        this.recordedEffects = [];
        // Replay-only state.
        this.replayEffects = [];
        this.replayPos = 0;
        this.checkArgs = false;
        this.argsDiffCount = 0;
        // Structural-scope tracking — independent products (`?!` / `!`).
        this.nextGroupId = 0;
        this.groupStack = [];
        this.branchStack = [];
        this.effectCountStack = [];
    }

    setNormal() {
        this.mode = REPLAY_MODE.NORMAL;
        this.replayEffects = [];
        this.replayPos = 0;
        this.argsDiffCount = 0;
    }

    startRecording() {
        this.mode = REPLAY_MODE.RECORDING;
        this.recordedEffects = [];
        this.replayEffects = [];
        this.replayPos = 0;
        this.argsDiffCount = 0;
        this.resetScope();
    }

    startReplay(effects, validateArgs) {
        this.mode = REPLAY_MODE.REPLAYING;
        this.replayEffects = Array.isArray(effects) ? effects.slice() : [];
        this.replayPos = 0;
        this.argsDiffCount = 0;
        this.checkArgs = !!validateArgs;
        this.resetScope();
    }

    resetScope() {
        this.nextGroupId = 0;
        this.groupStack = [];
        this.branchStack = [];
        this.effectCountStack = [];
    }

    enterGroup() {
        this.nextGroupId += 1;
        this.groupStack.push(this.nextGroupId);
        this.branchStack.push(0);
        this.effectCountStack.push(0);
        return this.nextGroupId;
    }

    setBranch(index) {
        if (this.branchStack.length === 0) return;
        this.branchStack[this.branchStack.length - 1] = index | 0;
        // New branch resets the per-branch effect occurrence counter.
        this.effectCountStack[this.effectCountStack.length - 1] = 0;
    }

    exitGroup() {
        if (this.groupStack.length === 0) return;
        this.groupStack.pop();
        this.branchStack.pop();
        this.effectCountStack.pop();
    }

    currentGroupId() {
        return this.groupStack.length > 0
            ? this.groupStack[this.groupStack.length - 1]
            : null;
    }

    currentBranchPath() {
        if (this.branchStack.length === 0) return null;
        return this.branchStack.map((b) => String(b)).join(".");
    }

    bumpEffectOccurrence() {
        if (this.effectCountStack.length === 0) return null;
        const idx = this.effectCountStack.length - 1;
        const v = this.effectCountStack[idx];
        this.effectCountStack[idx] = v + 1;
        return v;
    }

    /// Append an effect to the trace if we're in Recording mode.
    /// `outcome` is `{ kind: "value", value: <JSON> }` or
    /// `{ kind: "runtime_error", message: <string> }`.
    recordEffect(effectType, args, outcome, callerFn = "main", sourceLine = 0) {
        if (this.mode !== REPLAY_MODE.RECORDING) return;
        const groupId = this.currentGroupId();
        const branchPath = this.currentBranchPath();
        const effectOccurrence =
            groupId !== null ? this.bumpEffectOccurrence() : null;
        this.recordedEffects.push({
            seq: this.recordedEffects.length + 1,
            type: effectType,
            args: args ?? [],
            outcome: outcome ?? { kind: "value", value: null },
            caller_fn: callerFn,
            source_line: sourceLine,
            ...(groupId !== null ? { group_id: groupId } : {}),
            ...(branchPath !== null ? { branch_path: branchPath } : {}),
            ...(effectOccurrence !== null
                ? { effect_occurrence: effectOccurrence }
                : {}),
        });
    }

    /// Pull the next outcome from the trace if we're in Replay mode.
    /// Returns `{ outcome: <JSON outcome> }` on a hit, `{ skip: true }`
    /// when we're not in Replay mode (caller falls back to the real
    /// effect), or throws on sequence / args mismatch (under
    /// `--check-args`) / exhausted trace.
    replayEffect(effectType, args) {
        if (this.mode !== REPLAY_MODE.REPLAYING) {
            return { skip: true };
        }
        if (this.replayPos >= this.replayEffects.length) {
            throw new Error(
                `Replay exhausted at effect '${effectType}': trace had ${this.replayEffects.length} entries`
            );
        }
        const entry = this.replayEffects[this.replayPos];
        if (entry.type !== effectType) {
            throw new Error(
                `Replay mismatch at #${this.replayPos + 1}: expected '${entry.type}', got '${effectType}'`
            );
        }
        if (!argsEqual(entry.args ?? [], args ?? [])) {
            if (this.checkArgs) {
                throw new Error(
                    `Replay args mismatch at #${this.replayPos + 1} for '${effectType}'`
                );
            }
            this.argsDiffCount += 1;
        }
        this.replayPos += 1;
        return { outcome: entry.outcome };
    }

    takeRecordedEffects() {
        const out = this.recordedEffects;
        this.recordedEffects = [];
        return out;
    }

    replayProgress() {
        return [this.replayPos, this.replayEffects.length];
    }

    ensureReplayConsumed() {
        if (this.mode !== REPLAY_MODE.REPLAYING) return;
        if (this.replayPos < this.replayEffects.length) {
            const remaining = this.replayEffects.length - this.replayPos;
            throw new Error(
                `Replay incomplete: ${remaining} unconsumed effect(s) past program's last replay_effect call`
            );
        }
    }
}

/// Structural equality on the JSON shape the recorder produces. Match
/// what `aver::replay::JsonValue` Eq does in Rust — recurse through
/// arrays / objects, strict equality on primitives.
function argsEqual(a, b) {
    if (a === b) return true;
    if (Array.isArray(a) && Array.isArray(b)) {
        if (a.length !== b.length) return false;
        for (let i = 0; i < a.length; i += 1) {
            if (!argsEqual(a[i], b[i])) return false;
        }
        return true;
    }
    if (
        a !== null &&
        b !== null &&
        typeof a === "object" &&
        typeof b === "object"
    ) {
        const ka = Object.keys(a).sort();
        const kb = Object.keys(b).sort();
        if (ka.length !== kb.length) return false;
        for (let i = 0; i < ka.length; i += 1) {
            if (ka[i] !== kb[i]) return false;
            if (!argsEqual(a[ka[i]], b[ka[i]])) return false;
        }
        return true;
    }
    if (typeof a === "bigint" || typeof b === "bigint") {
        // BigInt vs Number: coerce both to BigInt when compatible so a
        // recorded `5n` matches a live `5` (the JSON parser doesn't
        // produce BigInt by default — wasm-gc args may).
        try {
            return BigInt(a) === BigInt(b);
        } catch {
            return false;
        }
    }
    return false;
}
