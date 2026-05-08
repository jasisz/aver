/// Expression emission: walks the Aver AST and emits WASM instructions.
///
/// Uses the shared `ir::` lowering infrastructure (CallPlan, MatchDispatchPlan,
/// SemanticConstructor) instead of raw AST pattern matching.
///
/// Typed ABI: Int->i64, Float->f64, Bool->i32, String->i32(ptr),
/// Result/Option/List/Record/Variant->i32(ptr). No tagged values for scalars.
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::codegen::CodegenContext;
use crate::codegen::common::is_user_type;
use crate::ir::{CallLowerCtx, ThinBodyCtx};
use crate::types::Type;

use super::runtime::RuntimeFuncIndices;
use super::types::{WasmType, aver_type_to_wasm};

mod builtins;
mod emit;
mod infer;
mod match_emit;

pub(super) use infer::{VariantInfo, build_variant_registry};

/// Interned string literal: (data_offset_in_memory, byte_length).
pub(super) type StringLiteral = (u32, u32);

// ---------------------------------------------------------------------------
// IR context adapter
// ---------------------------------------------------------------------------

/// Adapter for the shared IR lowering layer.
pub(super) struct WasmCallCtx<'a> {
    pub ctx: &'a CodegenContext,
    pub locals: &'a HashMap<String, u32>,
    pub current_module_prefix: Option<&'a str>,
}

impl CallLowerCtx for WasmCallCtx<'_> {
    fn is_local_value(&self, name: &str) -> bool {
        self.locals.contains_key(name)
    }

    fn is_user_type(&self, name: &str) -> bool {
        is_user_type(name, self.ctx)
    }

    fn resolve_module_call<'a>(&self, dotted: &'a str) -> Option<(&'a str, &'a str)> {
        let mut best = None;
        for (dot_idx, _) in dotted.match_indices('.') {
            let prefix = &dotted[..dot_idx];
            let suffix = &dotted[dot_idx + 1..];
            if self.ctx.module_prefixes.contains(prefix)
                && best.is_none_or(|existing: (&str, &str)| prefix.len() > existing.0.len())
            {
                best = Some((prefix, suffix));
            }
        }
        best
    }
}

impl ThinBodyCtx for WasmCallCtx<'_> {
    fn find_fn_def<'a>(&'a self, name: &str) -> Option<&'a crate::ast::FnDef> {
        if let Some((prefix, bare)) = self.resolve_module_call(name) {
            return self
                .ctx
                .modules
                .iter()
                .find(|module| module.prefix == prefix)
                .and_then(|module| module.fn_defs.iter().find(|fd| fd.name == bare));
        }

        if let Some(prefix) = self.current_module_prefix
            && let Some(fd) = self
                .ctx
                .modules
                .iter()
                .find(|module| module.prefix == prefix)
                .and_then(|module| module.fn_defs.iter().find(|fd| fd.name == name))
        {
            return Some(fd);
        }

        self.ctx
            .extra_fn_defs
            .iter()
            .find(|fd| fd.name == name)
            .or_else(|| self.ctx.fn_defs.iter().find(|fd| fd.name == name))
    }
}

/// Context for emitting expressions within a single function body.
pub(super) struct ExprEmitter<'a> {
    pub locals: HashMap<String, u32>,
    pub next_local: u32,
    pub fn_indices: &'a HashMap<String, u32>,
    pub rt: &'a RuntimeFuncIndices,
    pub instructions: Vec<Instruction<'a>>,
    pub string_literals: &'a HashMap<String, StringLiteral>,
    pub type_fields: &'a HashMap<(String, String), u32>,
    pub block_depth: u32,
    pub tco_loop_depth: Option<u32>,
    pub fn_sigs: &'a HashMap<String, (Vec<Type>, Type, Vec<String>)>,
    pub local_types: HashMap<u32, WasmType>,
    pub local_aver_types: HashMap<u32, Type>,
    pub ctx: &'a CodegenContext,
    pub variant_registry: &'a HashMap<(String, String), VariantInfo>,
    /// Current function's return type (set by emitter before body emission).
    pub fn_return_type: WasmType,
    /// ABI host import indices: import_name -> function index.
    pub host_import_indices: HashMap<String, u32>,
    /// Current function name (for self-TCO check).
    pub current_fn_name: String,
    /// Prefix of the dependent module currently being emitted, if any.
    pub current_module_prefix: Option<String>,
    /// Dispatch local used by a mutual-TCO trampoline, if this emitter is
    /// emitting a trampoline body instead of a plain function.
    pub mutual_tco_dispatch_local: Option<u32>,
    /// Canonical fn name -> (member id, parameter local slots inside the
    /// trampoline signature) for mutual-TCO lowering.
    pub mutual_tco_targets: HashMap<String, (u32, Vec<u32>)>,
    /// Codegen diagnostics collected while emitting the current function.
    pub errors: Vec<String>,
    /// Current frame mark used by boundary compaction on function return.
    pub boundary_mark_local: Option<u32>,
    /// Per-iteration mark for TCO loops (yard semantics): only compact
    /// allocations from the current iteration, not the entire frame.
    pub iter_mark_local: Option<u32>,
    /// Heap pointer after last compaction — compact when garbage exceeds threshold.
    pub gc_watermark_local: Option<u32>,
    /// Whether the current function result is a heap pointer in the typed ABI.
    pub fn_return_is_heap: bool,
    /// Thin frame: no boundary compaction or truncate needed on return.
    pub is_thin: bool,
    /// Parent-thin frame: borrow caller young lane; skip local boundary cleanup.
    pub is_parent_thin: bool,
    /// Pure no-alloc frame: classified by the shared `ir::compute_alloc_info`
    /// pass against `WasmAllocPolicy`. The body never produces a heap object,
    /// so all GC framing (boundary mark save, rt_truncate at exit, TCO
    /// watermark check) is dead work and gets elided. Set up front by
    /// `emit_plain_user_function` and `emit_mutual_tco_trampoline`.
    pub is_no_alloc: bool,
    /// Uniform-signature mutual-TCO group: every member shares the same
    /// WASM-typed param list, so the trampoline keeps a single shared
    /// slot row. Tail-calls within the group can write args directly to
    /// the target slots in reverse stack order, skipping the per-iter
    /// "evaluate → tmp → target" double copy.
    pub mutual_tco_uniform: bool,
}

impl<'a> ExprEmitter<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        fn_indices: &'a HashMap<String, u32>,
        rt: &'a RuntimeFuncIndices,
        string_literals: &'a HashMap<String, StringLiteral>,
        type_fields: &'a HashMap<(String, String), u32>,
        fn_sigs: &'a HashMap<String, (Vec<Type>, Type, Vec<String>)>,
        ctx: &'a CodegenContext,
        variant_registry: &'a HashMap<(String, String), VariantInfo>,
    ) -> Self {
        ExprEmitter {
            locals: HashMap::new(),
            next_local: 0,
            fn_indices,
            rt,
            instructions: Vec::new(),
            string_literals,
            type_fields,
            block_depth: 0,
            tco_loop_depth: None,
            fn_sigs,
            local_types: HashMap::new(),
            local_aver_types: HashMap::new(),
            ctx,
            variant_registry,
            fn_return_type: WasmType::I32,
            current_fn_name: String::new(),
            current_module_prefix: None,
            mutual_tco_dispatch_local: None,
            mutual_tco_targets: HashMap::new(),
            host_import_indices: HashMap::new(),
            errors: Vec::new(),
            boundary_mark_local: None,
            iter_mark_local: None,
            gc_watermark_local: None,
            fn_return_is_heap: false,
            is_thin: false,
            is_parent_thin: false,
            is_no_alloc: false,
            mutual_tco_uniform: false,
        }
    }

    pub(super) fn ir_ctx(&self) -> WasmCallCtx<'_> {
        WasmCallCtx {
            ctx: self.ctx,
            locals: &self.locals,
            current_module_prefix: self.current_module_prefix.as_deref(),
        }
    }

    pub fn enable_tco_loop(&mut self) {
        self.tco_loop_depth = Some(self.block_depth);
    }

    pub fn add_params(&mut self, params: &[(String, String)], param_types: &[Type]) {
        for (i, (name, _type_ann)) in params.iter().enumerate() {
            let idx = self.next_local;
            self.locals.insert(name.clone(), idx);
            if let Some(ty) = param_types.get(i) {
                self.local_types.insert(idx, aver_type_to_wasm(ty));
                self.local_aver_types.insert(idx, ty.clone());
            } else {
                self.local_types.insert(idx, WasmType::I64);
            }
            self.next_local += 1;
        }
    }

    pub(super) fn alloc_local(&mut self, wt: WasmType) -> u32 {
        let idx = self.next_local;
        self.next_local += 1;
        self.local_types.insert(idx, wt);
        idx
    }

    /// Emit body of a function.
    pub fn emit_body(&mut self, body: &crate::ast::FnBody) {
        match body {
            crate::ast::FnBody::Block(stmts) => self.emit_block(stmts),
        }
    }

    pub fn emit_end(&mut self) {
        self.instructions.push(Instruction::End);
        if self.block_depth > 0 {
            self.block_depth -= 1;
        }
    }

    pub(super) fn emit_if(&mut self, bt: wasm_encoder::BlockType) {
        self.instructions.push(Instruction::If(bt));
        self.block_depth += 1;
    }

    pub(super) fn emit_else(&mut self) {
        self.instructions.push(Instruction::Else);
    }

    pub(super) fn codegen_error(&mut self, message: impl Into<String>) {
        self.errors
            .push(format!("{}: {}", self.current_fn_name, message.into()));
    }

    pub(super) fn resolve_user_fn_name(&self, name: &str) -> String {
        if name.contains('.') {
            return name.to_string();
        }

        if let Some(prefix) = self.current_module_prefix.as_deref() {
            let qualified = format!("{prefix}.{name}");
            if self.fn_indices.contains_key(&qualified) || self.fn_sigs.contains_key(&qualified) {
                return qualified;
            }
        }

        name.to_string()
    }

    pub(super) fn emit_default_value(&mut self, wt: WasmType) {
        match wt {
            WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
            WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
            WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0_f64.into())),
        }
    }

    pub(super) fn is_heap_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Int
            | Type::Float
            | Type::Bool
            | Type::Unit
            | Type::Fn(_, _, _)
            | Type::Invalid
            | Type::Var(_) => false,
            Type::Str
            | Type::Result(_, _)
            | Type::Option(_)
            | Type::List(_)
            | Type::Tuple(_)
            | Type::Map(_, _)
            | Type::Vector(_)
            | Type::Named(_) => true,
        }
    }

    pub(super) fn ptr_mask_for_types(&self, tys: &[Type]) -> u16 {
        tys.iter().enumerate().fold(0u16, |mask, (idx, ty)| {
            if idx < 16 && self.is_heap_type(ty) {
                mask | (1u16 << idx)
            } else {
                mask
            }
        })
    }

    pub(super) fn ptr_mask_for_exprs<'b, I>(&self, exprs: I) -> u16
    where
        I: IntoIterator<Item = &'b crate::ast::Spanned<crate::ast::Expr>>,
    {
        exprs
            .into_iter()
            .enumerate()
            .fold(0u16, |mask, (idx, expr)| {
                if idx < 16 && self.expr_is_heap_ptr_spanned(expr) {
                    mask | (1u16 << idx)
                } else {
                    mask
                }
            })
    }

    pub(super) fn emit_boundary_truncate_or_compact_for_stack_value(
        &mut self,
        wt: WasmType,
        is_heap: bool,
    ) {
        if self.is_thin || self.is_parent_thin {
            let value_local = self.alloc_local(wt);
            self.instructions.push(Instruction::LocalSet(value_local));
            self.instructions.push(Instruction::LocalGet(value_local));
            return;
        }

        let Some(mark_local) = self.boundary_mark_local else {
            return;
        };

        let value_local = self.alloc_local(wt);
        self.instructions.push(Instruction::LocalSet(value_local));
        self.instructions.push(Instruction::LocalGet(mark_local));
        if is_heap {
            self.instructions
                .push(Instruction::Call(self.rt.collect_begin));
            self.instructions.push(Instruction::LocalGet(value_local));
            self.instructions
                .push(Instruction::Call(self.rt.retain_i32));
            self.instructions.push(Instruction::LocalSet(value_local));
            self.instructions
                .push(Instruction::Call(self.rt.collect_end));
            self.instructions.push(Instruction::LocalGet(value_local));
            self.instructions
                .push(Instruction::Call(self.rt.rebase_i32));
            self.instructions.push(Instruction::LocalSet(value_local));
        } else {
            self.instructions.push(Instruction::Call(self.rt.truncate));
        }
        self.instructions.push(Instruction::LocalGet(value_local));
    }

    pub(super) fn emit_boundary_return_from_stack(&mut self, wt: WasmType, is_heap: bool) {
        self.emit_boundary_truncate_or_compact_for_stack_value(wt, is_heap);
        self.instructions.push(Instruction::Return);
    }
}
