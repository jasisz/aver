//! Expression / statement → wasm-gc instructions.
//!
//! Phase 2 lowering rules (Int / Float / Bool / Unit only):
//!
//! - `Literal(Int|Float|Bool)` → `i64.const` / `f64.const` / `i32.const`.
//! - `Resolved { slot }` → `local.get slot`. Resolver assigns slots in
//!   declaration order (params first, bindings next), which matches
//!   wasm's local-indexing convention 1:1, so no remapping needed.
//! - `BinOp(Add|Sub|Mul|Div, Int, Int)` → `i64.{add,sub,mul,div_s}`.
//! - `BinOp(<comparison>, Int, Int)` → `i64.{eq,ne,lt_s,gt_s,le_s,ge_s}`.
//! - `FnCall(Ident, args)` → `call $idx` after pushing args left-to-right.
//! - `Stmt::Binding` → emit value, then `local.set slot` where slot
//!   is the next-available counter (same convention as resolver).
//!
//! Match / TailCall / dotted calls / compound types belong to phases
//! 3+ and surface as explicit `Unimplemented` errors so a partially-
//! lowered IR can't reach the encoder.

use std::collections::HashMap;

use wasm_encoder::{Function, Instruction, ValType};

use super::WasmGcError;
use super::types::aver_to_wasm;

use crate::ast::{BinOp, Expr, FnBody, FnDef, Literal, MatchArm, Pattern, Spanned, Stmt};
// Re-export the trait so the IntoStatic impls below stay private.
#[allow(unused_imports)]
use IntoStatic as _;

/// Maps fn name → wasm fn index + return type. Built once per module.
pub(super) struct FnMap {
    pub(super) by_name: HashMap<String, FnEntry>,
}

pub(super) struct FnEntry {
    pub(super) wasm_idx: u32,
    pub(super) return_type: String,
}

/// Per-fn slot table — one entry per local (param or binding) in
/// resolver-allocation order. Slot N maps to `wasm local N`.
struct SlotTable {
    /// Element index = slot number; element value = wasm ValType.
    by_slot: Vec<ValType>,
}

impl SlotTable {
    /// Reserve slots for params (in declaration order, skipping `Unit`-typed
    /// ones since wasm has no zero-width values).
    fn from_params(params: &[(String, String)]) -> Result<Self, WasmGcError> {
        let mut by_slot = Vec::with_capacity(params.len());
        for (_, ty) in params {
            if let Some(v) = aver_to_wasm(ty)? {
                by_slot.push(v);
            }
        }
        Ok(Self { by_slot })
    }

    fn declare(&mut self, ty: ValType) -> u32 {
        let slot = self.by_slot.len() as u32;
        self.by_slot.push(ty);
        slot
    }

    fn extra_locals(&self, params_count: usize) -> Vec<ValType> {
        self.by_slot.iter().skip(params_count).copied().collect()
    }
}

fn count_value_params(params: &[(String, String)]) -> usize {
    params.iter().filter(|(_, ty)| ty.trim() != "Unit").count()
}

/// Lower the body of `fd` into the supplied wasm `Function` builder.
/// Returns the list of *extra* locals (beyond params) needed for the
/// fn signature; caller passes these to `Function::new`.
///
/// `self_wasm_idx` is the current fn's own wasm index — used for
/// emitting `return_call $self` on `Expr::TailCall` to the same fn.
/// Mutual-TCO across SCC members goes through a `return_call_indirect`
/// table; that wiring lives in module.rs once phase 4b lands.
pub(super) fn emit_fn_body(
    func: &mut Function,
    fd: &FnDef,
    fn_map: &FnMap,
    self_wasm_idx: u32,
) -> Result<Vec<ValType>, WasmGcError> {
    let mut slots = SlotTable::from_params(&fd.params)?;
    let FnBody::Block(stmts) = fd.body.as_ref();
    let last_idx = stmts.len().saturating_sub(1);

    let ctx = EmitCtx {
        fn_map,
        self_wasm_idx,
        self_fn_name: fd.name.as_str(),
        return_type: fd.return_type.as_str(),
    };

    for (i, stmt) in stmts.iter().enumerate() {
        let is_last = i == last_idx;
        match stmt {
            Stmt::Binding(_, annot, expr) => {
                let ty = infer_aver_type(&expr.node, &ctx)?;
                let wasm_ty = aver_to_wasm(annot.as_deref().unwrap_or(ty))?;
                emit_expr(func, &expr.node, &slots, &ctx)?;
                if let Some(wasm_ty) = wasm_ty {
                    let slot = slots.declare(wasm_ty);
                    func.instruction(&Instruction::LocalSet(slot));
                }
            }
            Stmt::Expr(spanned) => {
                emit_expr(func, &spanned.node, &slots, &ctx)?;
                let aver_ty = infer_aver_type(&spanned.node, &ctx)?;
                let produces_value = aver_to_wasm(aver_ty)?.is_some();
                if !is_last && produces_value {
                    func.instruction(&Instruction::Drop);
                }
                if is_last {
                    if fd.return_type.trim() == "Unit" && produces_value {
                        func.instruction(&Instruction::Drop);
                    } else if fd.return_type.trim() != "Unit" && !produces_value {
                        return Err(WasmGcError::Validation(format!(
                            "fn `{}` returns {} but trailing expression yields no value",
                            fd.name, fd.return_type
                        )));
                    }
                }
            }
        }
    }
    func.instruction(&Instruction::End);

    Ok(slots.extra_locals(count_value_params(&fd.params)))
}

/// Per-fn lowering context — read-only state every emit fn needs.
struct EmitCtx<'a> {
    fn_map: &'a FnMap,
    self_wasm_idx: u32,
    self_fn_name: &'a str,
    return_type: &'a str,
}

/// Type inference over the limited shape phase 2/4 emits. Returns the
/// Aver type string. Errors on shapes that belong to a later phase,
/// with a message pointing at it.
fn infer_aver_type(expr: &Expr, ctx: &EmitCtx<'_>) -> Result<&'static str, WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(_)) => Ok("Int"),
        Expr::Literal(Literal::Float(_)) => Ok("Float"),
        Expr::Literal(Literal::Bool(_)) => Ok("Bool"),
        Expr::Literal(Literal::Unit) => Ok("Unit"),
        Expr::Resolved { .. } | Expr::Ident(_) => {
            // Phase-2/4 is Int-dominant; until per-slot type tracking
            // lands, assume Int. The user-visible types are already
            // proven by the type checker; we only need this info to
            // pick the right wasm op.
            Ok("Int")
        }
        Expr::BinOp(op, _, _) => Ok(binop_result(*op)),
        Expr::FnCall(callee, _) => {
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3 — dotted / method calls",
                    ));
                }
            };
            let entry = ctx
                .fn_map
                .by_name
                .get(name)
                .ok_or(WasmGcError::Validation(format!("unknown fn `{name}`")))?;
            Ok(static_type_str(&entry.return_type))
        }
        Expr::Match { arms, .. } => {
            // Match result type = arm body type; arms are required by
            // the type checker to agree, so any arm tells us. Phase
            // 4 only accepts non-empty matches.
            arms.first()
                .map(|a| infer_aver_type(&a.body.node, ctx))
                .unwrap_or(Err(WasmGcError::Validation("match has no arms".into())))?
                .into_static()
        }
        // Tail calls are statements at the wasm level (no value pushed
        // back to the caller's frame); for inference purposes we report
        // the enclosing fn's return type.
        Expr::TailCall(_) => Ok(static_type_str(ctx.return_type)),
        _ => Err(WasmGcError::Unimplemented(
            "expression shape outside phase 2/4",
        )),
    }
}

trait IntoStatic {
    fn into_static(self) -> Result<&'static str, WasmGcError>;
}

impl IntoStatic for Result<&'static str, WasmGcError> {
    fn into_static(self) -> Result<&'static str, WasmGcError> {
        self
    }
}

impl IntoStatic for &'static str {
    fn into_static(self) -> Result<&'static str, WasmGcError> {
        Ok(self)
    }
}

fn binop_result(op: BinOp) -> &'static str {
    match op {
        BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => "Int",
        BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte => "Bool",
    }
}

fn static_type_str(ty: &str) -> &'static str {
    match ty.trim() {
        "Int" => "Int",
        "Float" => "Float",
        "Bool" => "Bool",
        "Unit" => "Unit",
        _ => "Int", // phase-2 fallback — phase 3 introduces real type plumbing
    }
}

/// Emit instructions for `expr`. Caller manages stack effect — this
/// function pushes one value (or zero for `Unit`) for every call.
fn emit_expr(
    func: &mut Function,
    expr: &Expr,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    match expr {
        Expr::Literal(Literal::Int(n)) => {
            func.instruction(&Instruction::I64Const(*n));
        }
        Expr::Literal(Literal::Float(f)) => {
            func.instruction(&Instruction::F64Const((*f).into()));
        }
        Expr::Literal(Literal::Bool(b)) => {
            func.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
        }
        Expr::Literal(Literal::Unit) => {}
        Expr::Literal(_) => {
            return Err(WasmGcError::Unimplemented(
                "phase 3 — String / Char literals",
            ));
        }
        Expr::Ident(_) => {
            return Err(WasmGcError::Unimplemented(
                "bare Ident reached emitter (resolver should have produced Resolved)",
            ));
        }
        Expr::Resolved { slot, .. } => {
            func.instruction(&Instruction::LocalGet(*slot as u32));
        }
        Expr::BinOp(op, l, r) => {
            emit_expr(func, &l.node, slots, ctx)?;
            emit_expr(func, &r.node, slots, ctx)?;
            let inst = match op {
                BinOp::Add => Instruction::I64Add,
                BinOp::Sub => Instruction::I64Sub,
                BinOp::Mul => Instruction::I64Mul,
                BinOp::Div => Instruction::I64DivS,
                BinOp::Eq => Instruction::I64Eq,
                BinOp::Neq => Instruction::I64Ne,
                BinOp::Lt => Instruction::I64LtS,
                BinOp::Gt => Instruction::I64GtS,
                BinOp::Lte => Instruction::I64LeS,
                BinOp::Gte => Instruction::I64GeS,
            };
            func.instruction(&inst);
        }
        Expr::FnCall(callee, args) => {
            let name = match &callee.node {
                Expr::Ident(n) => n.as_str(),
                Expr::Resolved { name, .. } => name.as_str(),
                _ => {
                    return Err(WasmGcError::Unimplemented(
                        "phase 3 — dotted / method calls",
                    ));
                }
            };
            for arg in args {
                emit_expr(func, &arg.node, slots, ctx)?;
            }
            let entry = ctx
                .fn_map
                .by_name
                .get(name)
                .ok_or(WasmGcError::Validation(format!(
                    "call to unknown fn `{name}`"
                )))?;
            func.instruction(&Instruction::Call(entry.wasm_idx));
        }
        Expr::Match { subject, arms } => emit_match(func, subject, arms, slots, ctx)?,
        Expr::TailCall(boxed) => emit_tail_call(func, &boxed.target, &boxed.args, slots, ctx)?,
        _ => {
            return Err(WasmGcError::Unimplemented(
                "expression shape outside phase 2/4",
            ));
        }
    }
    Ok(())
}

/// Lower `match subject { arm0; arm1; ...; default }` into a cascade
/// of `if`/`else` blocks. Phase-4 shape:
/// - subject must be `Int` or `Bool`,
/// - patterns are `Literal(Int|Bool)` or `Wildcard`,
/// - exactly one wildcard, at the end (the type checker has already
///   verified exhaustiveness, so this is a structural simplification).
///
/// Strategy: stash the subject in a fresh local slot, then for each
/// non-wildcard arm emit `local.get $subj; <pat-const>; eq; (if … else)`.
/// The wildcard arm runs in the innermost `else`. Same shape works for
/// Bool subjects (single `if` over the boolean).
fn emit_match(
    func: &mut Function,
    subject: &Spanned<Expr>,
    arms: &[MatchArm],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if arms.is_empty() {
        return Err(WasmGcError::Validation("match has no arms".into()));
    }
    let result_ty_str = infer_aver_type(&arms[0].body.node, ctx)?;
    let result_wasm = aver_to_wasm(result_ty_str)?;
    let block_ty = match result_wasm {
        Some(v) => wasm_encoder::BlockType::Result(v),
        None => wasm_encoder::BlockType::Empty,
    };

    // Bool subject — special-case to a single `if`/`else`. No subject
    // local needed (wasm `if` consumes the i32 directly).
    let subject_ty = infer_aver_type(&subject.node, ctx)?;
    if subject_ty == "Bool" {
        if arms.len() != 2 {
            return Err(WasmGcError::Unimplemented(
                "phase 4 — Bool match must have exactly 2 arms (true / false)",
            ));
        }
        // Find which arm is `true` and which is `false`. Wildcard
        // counts as the "other" branch.
        let mut true_body: Option<&Spanned<Expr>> = None;
        let mut false_body: Option<&Spanned<Expr>> = None;
        for arm in arms {
            match &arm.pattern {
                Pattern::Literal(Literal::Bool(true)) => true_body = Some(&arm.body),
                Pattern::Literal(Literal::Bool(false)) => false_body = Some(&arm.body),
                Pattern::Wildcard => {
                    if true_body.is_none() {
                        true_body = Some(&arm.body);
                    } else {
                        false_body = Some(&arm.body);
                    }
                }
                _ => {
                    return Err(WasmGcError::Unimplemented(
                        "phase 4 — Bool match supports only Bool literals + wildcard",
                    ));
                }
            }
        }
        let t = true_body.ok_or(WasmGcError::Validation(
            "Bool match missing true arm".into(),
        ))?;
        let f = false_body.ok_or(WasmGcError::Validation(
            "Bool match missing false arm".into(),
        ))?;
        emit_expr(func, &subject.node, slots, ctx)?;
        func.instruction(&Instruction::If(block_ty));
        emit_expr(func, &t.node, slots, ctx)?;
        func.instruction(&Instruction::Else);
        emit_expr(func, &f.node, slots, ctx)?;
        func.instruction(&Instruction::End);
        return Ok(());
    }

    if subject_ty != "Int" {
        return Err(WasmGcError::Unimplemented(
            "phase 4 — match subject must be Int or Bool",
        ));
    }

    // Int subject — cascade. We need a subject scratch local; phase 4
    // hasn't reserved one ahead of time, so we synthesise one here as
    // a fresh slot in the *current* table. The caller already finished
    // local declarations, but `Function` accepts arbitrary local
    // indices ≤ count — module.rs handles count via the dry-run pass.
    //
    // Practical limitation today: we can't grow `slots` mid-emit
    // because slot allocation lives in module.rs's two-pass build.
    // Workaround: use a trailing scratch slot reserved by the
    // module-level pre-pass — simpler approach is to recompute the
    // subject expression at each comparison. Subjects are typically
    // a single `local.get` so the cost is one instruction per arm.
    //
    // This keeps phase 4 contained — phase 5 cleanup can switch to a
    // proper temp-local once we add a per-fn local-allocator.
    let mut wildcard_body: Option<&Spanned<Expr>> = None;
    let mut typed_arms: Vec<(i64, &Spanned<Expr>)> = Vec::new();
    for arm in arms {
        match &arm.pattern {
            Pattern::Literal(Literal::Int(n)) => typed_arms.push((*n, &arm.body)),
            Pattern::Wildcard => wildcard_body = Some(&arm.body),
            _ => {
                return Err(WasmGcError::Unimplemented(
                    "phase 4 — Int match supports only Int literal patterns + wildcard",
                ));
            }
        }
    }
    let wildcard = wildcard_body.ok_or(WasmGcError::Unimplemented(
        "phase 4 — Int match without wildcard (exhaustive Int matching needs phase 5)",
    ))?;

    emit_int_match_cascade(func, subject, &typed_arms, wildcard, block_ty, slots, ctx)?;
    Ok(())
}

fn emit_int_match_cascade(
    func: &mut Function,
    subject: &Spanned<Expr>,
    typed_arms: &[(i64, &Spanned<Expr>)],
    wildcard: &Spanned<Expr>,
    block_ty: wasm_encoder::BlockType,
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    if typed_arms.is_empty() {
        // No typed arms left — just emit wildcard.
        emit_expr(func, &wildcard.node, slots, ctx)?;
        return Ok(());
    }
    let (pat_lit, body) = typed_arms[0];
    emit_expr(func, &subject.node, slots, ctx)?;
    func.instruction(&Instruction::I64Const(pat_lit));
    func.instruction(&Instruction::I64Eq);
    func.instruction(&Instruction::If(block_ty));
    emit_expr(func, &body.node, slots, ctx)?;
    func.instruction(&Instruction::Else);
    emit_int_match_cascade(
        func,
        subject,
        &typed_arms[1..],
        wildcard,
        block_ty,
        slots,
        ctx,
    )?;
    func.instruction(&Instruction::End);
    Ok(())
}

/// Lower `Expr::TailCall { target, args }` into a native wasm tail
/// call. For a self-recursive call (target == current fn), emit
/// `return_call $self`. Mutual TCO across SCC peers is a phase-4b
/// extension that wires a function table; today it surfaces as
/// `Unimplemented` so the user sees a clear bump line.
fn emit_tail_call(
    func: &mut Function,
    target: &str,
    args: &[Spanned<Expr>],
    slots: &SlotTable,
    ctx: &EmitCtx<'_>,
) -> Result<(), WasmGcError> {
    let entry = ctx
        .fn_map
        .by_name
        .get(target)
        .ok_or(WasmGcError::Validation(format!(
            "tail call to unknown fn `{target}`"
        )))?;
    for arg in args {
        emit_expr(func, &arg.node, slots, ctx)?;
    }
    if target == ctx.self_fn_name {
        func.instruction(&Instruction::ReturnCall(ctx.self_wasm_idx));
    } else {
        // Direct (non-self) tail call to a known fn — wasm-gc still
        // supports `return_call` here. Mutual-TCO via a function
        // table is a phase 4b refinement once we have an SCC bench
        // that needs it.
        func.instruction(&Instruction::ReturnCall(entry.wasm_idx));
    }
    Ok(())
}
