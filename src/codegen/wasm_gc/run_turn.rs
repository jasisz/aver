//! `Run.waitStarts`, `Run.waitEnds` and `Run.lastTurn`, lowered inline on
//! both wasm targets.
//!
//! In a program that reads `Run.lastTurn`, the generated loop marks its one
//! wait of a turn: `Run.waitStarts()` right before it and `Run.waitEnds()`
//! right after it returns. Each mark reads a monotonic clock once, in
//! nanoseconds, and the module keeps what it needs in five i64 globals:
//!
//! - `started`: when the wait now in progress started;
//! - `returned`: when the last wait returned, `-1` before the first one;
//! - `waited`, `worked`: what `Run.lastTurn` answers, in whole milliseconds;
//! - `turn`: how many waits have returned, the turn `Run.lastTurn` names.
//!
//! `Run.waitEnds` makes the numbers: the turn counts one more, waited is this
//! wait, worked is the stretch from the return of the wait before it to the
//! start of this one, or 0 when there was none. Until a wait has returned all
//! three are 0.
//!
//! `Run` does not expose the two marks, so only the generated loop calls
//! them. The clock is the host's on wasm-gc (`aver.run_wait_starts` and
//! `aver.run_wait_ends`, which the recorder sees as the two marks) and
//! `wasi:clocks/monotonic-clock.now` on wasip2. On wasm-gc `Run.lastTurn`
//! also hands the three numbers through `aver.run_last_turn`, which a live host
//! answers unchanged and a replay answers with the recorded ones. A component
//! records nothing, so there it reads the globals directly.

use wasm_encoder::{Function, Instruction};

use super::WasmGcError;
use super::body::{EmitCtx, SlotTable};

/// The five globals the loop's waits are measured in.
#[derive(Debug, Clone, Copy)]
pub(super) struct RunTurnGlobals {
    pub(super) started: u32,
    pub(super) returned: u32,
    pub(super) waited: u32,
    pub(super) worked: u32,
    pub(super) turn: u32,
}

impl RunTurnGlobals {
    /// Initial values, in allocation order: `started`, `returned` (`-1`: no
    /// wait has returned yet), `waited`, `worked`, `turn`.
    pub(super) const INITIAL: [i64; 5] = [0, -1, 0, 0, 0];
}

const NANOS_PER_MS: i64 = 1_000_000;

fn globals(ctx: &EmitCtx<'_>) -> Result<RunTurnGlobals, WasmGcError> {
    ctx.fn_map.run_turn_globals.ok_or(WasmGcError::Validation(
        "Run.lastTurn or a mark of the loop's wait is called, but the module allocated no turn globals"
            .into(),
    ))
}

/// Push one reading of the monotonic clock, in nanoseconds: the host's
/// through `dotted`'s recorder import on wasm-gc, WASI's on wasip2.
fn emit_clock(func: &mut Function, dotted: &str, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    if let Some(lowering) = ctx.wasip2_lowering {
        let now = lowering
            .clocks_monotonic_now_fn_idx
            .ok_or(WasmGcError::Validation(format!(
                "{dotted} on wasip2: the monotonic clock import is missing"
            )))?;
        func.instruction(&Instruction::Call(now));
        return Ok(());
    }
    let import = ctx
        .fn_map
        .effects
        .get(dotted)
        .copied()
        .ok_or(WasmGcError::Validation(format!(
            "{dotted} on wasm-gc: its import is missing"
        )))?;
    super::body::emit_caller_fn_idx(func, ctx)?;
    func.instruction(&Instruction::Call(import));
    Ok(())
}

/// `Run.waitStarts()`: keep when the wait starts. Leaves nothing (`Unit`).
pub(super) fn emit_wait_starts(func: &mut Function, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    let g = globals(ctx)?;
    emit_clock(func, "Run.waitStarts", ctx)?;
    func.instruction(&Instruction::GlobalSet(g.started));
    Ok(())
}

/// `Run.waitEnds()`: make what `Run.lastTurn` answers from now on. Leaves
/// nothing (`Unit`).
pub(super) fn emit_wait_ends(func: &mut Function, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    let g = globals(ctx)?;
    // worked = returned < 0 ? 0 : (started - returned) / 1ms
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::GlobalGet(g.started));
    func.instruction(&Instruction::GlobalGet(g.returned));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I64Const(NANOS_PER_MS));
    func.instruction(&Instruction::I64DivS);
    func.instruction(&Instruction::GlobalGet(g.returned));
    func.instruction(&Instruction::I64Const(0));
    func.instruction(&Instruction::I64LtS);
    func.instruction(&Instruction::Select);
    func.instruction(&Instruction::GlobalSet(g.worked));
    // returned = now; waited = (returned - started) / 1ms
    emit_clock(func, "Run.waitEnds", ctx)?;
    func.instruction(&Instruction::GlobalSet(g.returned));
    func.instruction(&Instruction::GlobalGet(g.returned));
    func.instruction(&Instruction::GlobalGet(g.started));
    func.instruction(&Instruction::I64Sub);
    func.instruction(&Instruction::I64Const(NANOS_PER_MS));
    func.instruction(&Instruction::I64DivS);
    func.instruction(&Instruction::GlobalSet(g.waited));
    // turn += 1
    func.instruction(&Instruction::GlobalGet(g.turn));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::GlobalSet(g.turn));
    Ok(())
}

/// `Run.lastTurn()`: the three numbers as a `Run.Turn` record.
pub(super) fn emit_last_turn(
    func: &mut Function,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let g = globals(ctx)?;
    let record = ctx
        .registry
        .record_type_idx("Run.Turn")
        .ok_or(WasmGcError::Validation(
            "Run.lastTurn answers Run.Turn, and that record was not registered".into(),
        ))?;
    let (turn, waited, worked) = slots.run_turn_scratch();
    func.instruction(&Instruction::GlobalGet(g.turn));
    func.instruction(&Instruction::GlobalGet(g.waited));
    func.instruction(&Instruction::GlobalGet(g.worked));
    // Through the recorder on wasm-gc: live the numbers come back unchanged,
    // and a replay hands back the ones the recording holds.
    if let Some(&record_import) = ctx.fn_map.effects.get("Run.lastTurn") {
        super::body::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(record_import));
    }
    func.instruction(&Instruction::LocalSet(worked));
    func.instruction(&Instruction::LocalSet(waited));
    func.instruction(&Instruction::LocalSet(turn));
    func.instruction(&Instruction::LocalGet(turn));
    lift_to_int(func, ctx)?;
    func.instruction(&Instruction::LocalGet(waited));
    lift_to_int(func, ctx)?;
    func.instruction(&Instruction::LocalGet(worked));
    lift_to_int(func, ctx)?;
    func.instruction(&Instruction::StructNew(record));
    Ok(())
}

/// `Int = ℤ`: an i64 on the stack into the `Int` carrier a record field
/// holds. A no-op when the module keeps `Int` as a plain i64.
fn lift_to_int(func: &mut Function, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    if ctx.registry.bignum {
        let from_i64 =
            ctx.fn_map
                .builtins
                .get("__aint_from_i64")
                .copied()
                .ok_or(WasmGcError::Validation(
                    "bignum active but __aint_from_i64 helper not registered".into(),
                ))?;
        func.instruction(&Instruction::Call(from_i64));
    }
    Ok(())
}
