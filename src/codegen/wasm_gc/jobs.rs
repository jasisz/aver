//! Jobs run inline on wasm-gc and wasip2 (jasisz/aver#1329).
//!
//! A component and a wasm-gc module are single-threaded, so a job cannot run
//! beside the turn. It runs *in* the turn instead: `begin(task)` calls the
//! bound function at once and hands back a handle that already carries the
//! answer. That makes the handle the whole job — the `$job` struct's four
//! fields are its identity, its state, the kind that minted it and the
//! answer — so this lowering needs no table, no slot counter and no global
//! beyond the one that mints ids.
//!
//! The semantics are the VM's, word for word: a second `take` answers
//! `work: job already taken`, a take after a cancel answers `work: job
//! cancelled`, and a handle offered to another kind's `take` answers `work:
//! this job was not started by job kind 'K'`. Only the wall clock differs —
//! a job is done the moment it began.
//!
//! Both wasm targets share every byte of this: the module answers the
//! capability itself, so there is nothing target-specific to say.

use wasm_encoder::{Function, HeapType, Instruction, RefType, ValType};

use super::WasmGcError;
use super::body::EmitCtx;
use super::types::{OPTION_SOME_TAG, RESULT_ERR_TAG, RESULT_OK_TAG, TypeRegistry};

/// The `state` field of a `$job`: the answer is there for the taking.
pub(super) const JOB_STATE_DONE: i32 = 0;
/// The `state` field of a `$job`: the answer has been collected.
pub(super) const JOB_STATE_TAKEN: i32 = 1;
/// The `state` field of a `$job`: the job was cancelled, answer dropped.
pub(super) const JOB_STATE_CANCELLED: i32 = 2;

/// What a second `take` of one handle answers.
pub(super) const ALREADY_TAKEN: &str = "work: job already taken";
/// What taking a cancelled job answers.
pub(super) const CANCELLED: &str = "work: job cancelled";

/// What one kind's `take` answers when handed another kind's handle.
pub(super) fn foreign_handle_message(capability: &str) -> String {
    format!("work: this job was not started by job kind '{capability}'")
}

/// Every error message a job kind's inline lowering can produce, so the
/// string-literal segment table can carry them whether or not the program
/// ever spells one.
pub(super) fn job_error_messages(kinds: &[crate::capability::work::JobKindPlan]) -> Vec<Vec<u8>> {
    if kinds.is_empty() {
        return Vec::new();
    }
    let mut messages = vec![
        ALREADY_TAKEN.as_bytes().to_vec(),
        CANCELLED.as_bytes().to_vec(),
    ];
    for kind in kinds {
        messages.push(foreign_handle_message(&kind.shape.capability).into_bytes());
    }
    messages
}

/// One job kind, resolved against the emitted module.
pub(super) struct JobKindLowering {
    /// The capability that declares the kind, `Validation`.
    pub(super) capability: String,
    /// Canonical name of its `begin` operation.
    pub(super) begin: String,
    /// Canonical name of its `take` operation.
    pub(super) take: String,
    /// The `kind` field value a handle of this kind carries. A handle's kind
    /// is what lets one kind's `take` refuse another kind's job.
    pub(super) kind_tag: i32,
    /// Wasm index of the bound function, `work = "Node.validate"`.
    pub(super) work_fn_idx: u32,
    /// The payload type `R` as Aver spells it.
    pub(super) payload_aver: String,
    /// The task type `T` as Aver spells it.
    pub(super) task_aver: String,
    /// `Option<T>` slot — how the task crosses to the recorder.
    pub(super) option_task_idx: u32,
    /// `Option<R>` slot.
    pub(super) option_payload_idx: u32,
    /// `Result<Option<R>, String>` slot — what `take` answers and what the
    /// handle's `value` field carries, already built at `begin`.
    pub(super) take_result_idx: u32,
    /// `Result<Work.Job, String>` slot — what `begin` answers.
    pub(super) begin_result_idx: u32,
}

/// Everything the call-site emitter needs to lower `begin`, `take` and
/// `Work.cancel` inline.
pub(super) struct JobLowering {
    pub(super) kinds: Vec<JobKindLowering>,
    /// The `$job` struct slot.
    pub(super) job_struct_idx: u32,
    /// The mutable `i64` global that mints handle ids. The one piece of
    /// module state a job needs: two handles of one program are never the
    /// same job, and the recorder reads the id as the handle's identity.
    pub(super) next_id_global: u32,
}

impl JobLowering {
    pub(super) fn kind_for_begin(&self, dotted: &str) -> Option<&JobKindLowering> {
        self.kinds.iter().find(|kind| kind.begin == dotted)
    }

    pub(super) fn kind_for_take(&self, dotted: &str) -> Option<&JobKindLowering> {
        self.kinds.iter().find(|kind| kind.take == dotted)
    }

    /// Whether `dotted` is any job-kind operation of this program.
    pub(super) fn answers(&self, dotted: &str) -> bool {
        self.kind_for_begin(dotted).is_some() || self.kind_for_take(dotted).is_some()
    }
}

fn struct_ref(idx: u32) -> ValType {
    ValType::Ref(RefType {
        nullable: true,
        heap_type: HeapType::Concrete(idx),
    })
}

/// Push a `(ref null $string)` for one of the fixed job messages.
fn emit_message(func: &mut Function, message: &str, ctx: &EmitCtx<'_>) -> Result<(), WasmGcError> {
    super::body::emit_string_literal_bytes(func, message.as_bytes(), ctx)
}

/// `i32.const <tag>; <ok>; <err>; struct.new $result` for a `Result` whose
/// `Err` arm is the `String` this pushes.
fn emit_err_result(
    func: &mut Function,
    result_idx: u32,
    ok_placeholder: &str,
    message: &str,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    func.instruction(&Instruction::I32Const(RESULT_ERR_TAG));
    super::body::emit_default_value(func, ok_placeholder, ctx.registry)?;
    emit_message(func, message, ctx)?;
    func.instruction(&Instruction::StructNew(result_idx));
    Ok(())
}

/// `begin(task)`: run the bound function inline, mint a handle carrying the
/// answer it produced, and answer `Ok(handle)`.
///
/// The answer is stored as the exact `Result<Option<R>, String>` value the
/// matching `take` will hand back, which is why a payload of any type fits
/// one `anyref` field: an `Option` is a struct whatever `R` is.
///
/// `emit_task` pushes the task argument; it runs between the handle's id and
/// the call, which is stack-legal because it leaves exactly one value.
pub(super) fn emit_begin(
    func: &mut Function,
    kind: &JobKindLowering,
    jobs: &JobLowering,
    handle_local: u32,
    slots: &super::body::SlotTable,
    ctx: &EmitCtx<'_>,
    emit_task: impl FnOnce(&mut Function) -> Result<Option<bool>, WasmGcError>,
) -> Result<Option<()>, WasmGcError> {
    // The task is read twice: the bound function runs it, and — where the
    // recorder exists — the turn is told what it was. Park it once.
    let task_local =
        match crate::codegen::wasm_gc::types::aver_to_wasm(&kind.task_aver, Some(ctx.registry))? {
            Some(ty) => {
                if emit_task(func)?.is_none() {
                    return Ok(None);
                }
                let local = slots.job_task_scratch(kind.kind_tag, ty);
                func.instruction(&Instruction::LocalSet(local));
                Some(local)
            }
            // A `Unit` task has no wasm value at all; the expression still runs.
            None => {
                if emit_task(func)?.is_none() {
                    return Ok(None);
                }
                None
            }
        };

    // The handle's id, then the bump. `global.get` leaves the id on the
    // stack and the second read is what the increment is written from, so
    // no local is needed.
    func.instruction(&Instruction::GlobalGet(jobs.next_id_global));
    func.instruction(&Instruction::GlobalGet(jobs.next_id_global));
    func.instruction(&Instruction::I64Const(1));
    func.instruction(&Instruction::I64Add);
    func.instruction(&Instruction::GlobalSet(jobs.next_id_global));

    func.instruction(&Instruction::I32Const(JOB_STATE_DONE));
    func.instruction(&Instruction::I32Const(kind.kind_tag));

    // The answer, built as `Result.Ok(Option.Some(<the function's result>))`.
    func.instruction(&Instruction::I32Const(RESULT_OK_TAG));
    func.instruction(&Instruction::I32Const(OPTION_SOME_TAG));
    if let Some(local) = task_local {
        func.instruction(&Instruction::LocalGet(local));
    }
    func.instruction(&Instruction::Call(kind.work_fn_idx));
    func.instruction(&Instruction::StructNew(kind.option_payload_idx));
    super::body::emit_default_value(func, "String", ctx.registry)?;
    func.instruction(&Instruction::StructNew(kind.take_result_idx));

    func.instruction(&Instruction::StructNew(jobs.job_struct_idx));
    func.instruction(&Instruction::LocalSet(handle_local));

    // What the recorder sees: `<Kind>.begin` with the task it was given and
    // the handle it answered, in the turn it happened in. Only the bridge
    // target has a recorder; a component records nothing.
    if let Some(&record) = ctx.fn_map.effects.get(WORK_BEGIN_EFFECT) {
        func.instruction(&Instruction::I32Const(kind.kind_tag));
        func.instruction(&Instruction::I32Const(OPTION_SOME_TAG));
        match task_local {
            Some(local) => func.instruction(&Instruction::LocalGet(local)),
            None => func.instruction(&Instruction::I32Const(0)),
        };
        func.instruction(&Instruction::StructNew(kind.option_task_idx));
        func.instruction(&Instruction::LocalGet(handle_local));
        super::body::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(record));
    }

    func.instruction(&Instruction::I32Const(RESULT_OK_TAG));
    func.instruction(&Instruction::LocalGet(handle_local));
    super::body::emit_default_value(func, "String", ctx.registry)?;
    func.instruction(&Instruction::StructNew(kind.begin_result_idx));
    Ok(Some(()))
}

/// The canonical name the `begin` recording import is keyed by.
pub(super) const WORK_BEGIN_EFFECT: &str = "__work_begin";
/// The canonical name the `take` recording import is keyed by.
pub(super) const WORK_TAKE_EFFECT: &str = "__work_take";

/// `take(job)`: the four answers the VM gives, in the VM's words.
///
/// The handle on the stack is consumed into a local first, because every
/// answer reads it more than once.
pub(super) fn emit_take(
    func: &mut Function,
    kind: &JobKindLowering,
    jobs: &JobLowering,
    handle_local: u32,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    use wasm_encoder::BlockType;

    func.instruction(&Instruction::LocalSet(handle_local));
    let result_block = BlockType::Result(struct_ref(kind.take_result_idx));
    // The recorder's view of `<Kind>.take`: the handle it was given and the
    // answer this turn produced. In replay the host hands back the recorded
    // answer instead, so a turn that answered `None` on the VM answers
    // `None` here too.
    let record = ctx.fn_map.effects.get(WORK_TAKE_EFFECT).copied();
    if record.is_some() {
        func.instruction(&Instruction::I32Const(kind.kind_tag));
        func.instruction(&Instruction::LocalGet(handle_local));
    }

    // A handle belongs to the kind that started it. `Work.Job` is one type,
    // so this is the only thing that can tell two kinds apart.
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 2,
    });
    func.instruction(&Instruction::I32Const(kind.kind_tag));
    func.instruction(&Instruction::I32Ne);
    func.instruction(&Instruction::If(result_block));
    emit_err_result(
        func,
        kind.take_result_idx,
        &format!("Option<{}>", kind.payload_aver),
        &foreign_handle_message(&kind.capability),
        ctx,
    )?;
    func.instruction(&Instruction::Else);

    // Cancelled: the answer was dropped when the cancel landed.
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::I32Const(JOB_STATE_CANCELLED));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(result_block));
    emit_err_result(
        func,
        kind.take_result_idx,
        &format!("Option<{}>", kind.payload_aver),
        CANCELLED,
        ctx,
    )?;
    func.instruction(&Instruction::Else);

    // Already taken: one answer belongs to whoever took it first.
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::I32Const(JOB_STATE_TAKEN));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(result_block));
    emit_err_result(
        func,
        kind.take_result_idx,
        &format!("Option<{}>", kind.payload_aver),
        ALREADY_TAKEN,
        ctx,
    )?;
    func.instruction(&Instruction::Else);

    // The answer the bound function produced at `begin`, and the slot is
    // taken from here on.
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::I32Const(JOB_STATE_TAKEN));
    func.instruction(&Instruction::StructSet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 3,
    });
    func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
        kind.take_result_idx,
    )));
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    func.instruction(&Instruction::End);
    if let Some(record) = record {
        super::body::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(record));
        func.instruction(&Instruction::RefCastNullable(HeapType::Concrete(
            kind.take_result_idx,
        )));
    }
    Ok(())
}

/// `Work.cancel(job)`: drop the answer of a job that is still there for the
/// taking, and change nothing for one already taken or already cancelled.
///
/// It answers `Unit`, so the handle is consumed and nothing is left behind.
pub(super) fn emit_cancel(
    func: &mut Function,
    jobs: &JobLowering,
    handle_local: u32,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    use wasm_encoder::BlockType;

    func.instruction(&Instruction::LocalSet(handle_local));
    // The turn's record of the cancel, in the order the VM records it.
    if let Some(&record) = ctx.fn_map.effects.get(crate::capability::work::WORK_CANCEL) {
        func.instruction(&Instruction::LocalGet(handle_local));
        super::body::emit_caller_fn_idx(func, ctx)?;
        func.instruction(&Instruction::Call(record));
    }
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::StructGet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 1,
    });
    func.instruction(&Instruction::I32Const(JOB_STATE_DONE));
    func.instruction(&Instruction::I32Eq);
    func.instruction(&Instruction::If(BlockType::Empty));
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::I32Const(JOB_STATE_CANCELLED));
    func.instruction(&Instruction::StructSet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 1,
    });
    // The answer goes with it: a cancelled job never hands one back, so
    // keeping it alive would only pin whatever it holds.
    func.instruction(&Instruction::LocalGet(handle_local));
    func.instruction(&Instruction::RefNull(HeapType::Abstract {
        shared: false,
        ty: wasm_encoder::AbstractHeapType::Any,
    }));
    func.instruction(&Instruction::StructSet {
        struct_type_index: jobs.job_struct_idx,
        field_index: 3,
    });
    func.instruction(&Instruction::End);
    Ok(())
}

/// Resolve one plan's job kinds against the emitted module's slots.
///
/// Every failure here is a program the front door should already have
/// refused — a job kind with no `work` binding, or one whose bound function
/// is not in the flattened program — so each answer names what was missing
/// rather than emitting a module that traps.
pub(super) fn build_lowering(
    kinds: &[crate::capability::work::JobKindPlan],
    registry: &TypeRegistry,
    fn_wasm_idx: impl Fn(&str) -> Option<u32>,
    next_id_global: u32,
) -> Result<Option<JobLowering>, WasmGcError> {
    if kinds.is_empty() {
        return Ok(None);
    }
    let job_struct_idx = registry.job_struct_idx.ok_or(WasmGcError::Validation(
        "a job kind needs the `Work.Job` handle slot to be allocated".into(),
    ))?;
    let mut lowered = Vec::with_capacity(kinds.len());
    for (index, kind) in kinds.iter().enumerate() {
        let capability = kind.shape.capability.clone();
        let function = kind.function.as_ref().ok_or_else(|| {
            WasmGcError::Validation(format!(
                "job kind '{capability}' has no `work = \"Module.function\"` binding; the program door refuses that before codegen"
            ))
        })?;
        let work_fn_idx = fn_wasm_idx(function).ok_or_else(|| {
            WasmGcError::Validation(format!(
                "job kind '{capability}' is bound to '{function}', which the flattened program does not declare"
            ))
        })?;
        let payload_aver = kind.shape.payload.display();
        let task_aver = kind.shape.task.display();
        let option_task_canonical = format!("Option<{task_aver}>");
        let option_task_idx = registry
            .option_type_idx(&option_task_canonical)
            .ok_or_else(|| {
                WasmGcError::Validation(format!(
                    "job kind '{capability}' needs the `{option_task_canonical}` slot but none was registered"
                ))
            })?;
        let option_canonical = format!("Option<{payload_aver}>");
        let option_payload_idx = registry.option_type_idx(&option_canonical).ok_or_else(|| {
            WasmGcError::Validation(format!(
                "job kind '{capability}' needs the `{option_canonical}` slot but none was registered"
            ))
        })?;
        let take_canonical = format!("Result<{option_canonical},String>");
        let take_result_idx = registry.result_type_idx(&take_canonical).ok_or_else(|| {
            WasmGcError::Validation(format!(
                "job kind '{capability}' needs the `{take_canonical}` slot but none was registered"
            ))
        })?;
        let begin_canonical = format!("Result<{},String>", crate::capability::work::WORK_JOB);
        let begin_result_idx = registry.result_type_idx(&begin_canonical).ok_or_else(|| {
            WasmGcError::Validation(format!(
                "job kind '{capability}' needs the `{begin_canonical}` slot but none was registered"
            ))
        })?;
        lowered.push(JobKindLowering {
            begin: kind.begin.canonical_name.clone(),
            take: kind.take.canonical_name.clone(),
            capability,
            kind_tag: index as i32,
            work_fn_idx,
            payload_aver,
            task_aver,
            option_task_idx,
            option_payload_idx,
            take_result_idx,
            begin_result_idx,
        });
    }
    Ok(Some(JobLowering {
        kinds: lowered,
        job_struct_idx,
        next_id_global,
    }))
}
