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
use crate::ir::CallLowerCtx;
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
    /// Codegen diagnostics collected while emitting the current function.
    pub errors: Vec<String>,
}

impl<'a> ExprEmitter<'a> {
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
            host_import_indices: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub(super) fn ir_ctx(&self) -> WasmCallCtx<'_> {
        WasmCallCtx {
            ctx: self.ctx,
            locals: &self.locals,
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

    pub(super) fn emit_default_value(&mut self, wt: WasmType) {
        match wt {
            WasmType::I32 => self.instructions.push(Instruction::I32Const(0)),
            WasmType::I64 => self.instructions.push(Instruction::I64Const(0)),
            WasmType::F64 => self.instructions.push(Instruction::F64Const(0.0)),
        }
    }
}
