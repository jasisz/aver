/// Expression emission: walks the Aver AST and emits WASM instructions.
///
/// All values are tagged i64 (4-bit tag + 60-bit payload).
/// Arithmetic goes through runtime helpers ($int_add etc.) that extract/repack payloads.
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::ast::{BinOp, Expr, FnBody, Literal, MatchArm, Pattern, Spanned, Stmt, StrPart};

use super::runtime::RuntimeFuncIndices;
use super::value;

/// Interned string literal: (data_offset_in_memory, byte_length).
pub(super) type StringLiteral = (u32, u32);

/// Context for emitting expressions within a single function body.
pub(super) struct ExprEmitter<'a> {
    pub locals: HashMap<String, u32>,
    pub next_local: u32,
    pub fn_indices: &'a HashMap<String, u32>,
    pub rt: &'a RuntimeFuncIndices,
    pub instructions: Vec<Instruction<'a>>,
    /// String literal table: string content → (data_offset, length).
    pub string_literals: &'a HashMap<String, StringLiteral>,
    /// Host import function indices: "Console.print" → func idx, etc.
    pub host_imports: &'a HashMap<String, u32>,
    /// Type field name lookup: (type_name, field_name) → field_index.
    pub type_fields: &'a HashMap<(String, String), u32>,
    /// Current nesting depth of structured control flow (if/else/block).
    /// Used to compute br depth for TCO loop.
    pub block_depth: u32,
    /// Whether this function is wrapped in a TCO loop.
    pub tco_loop_depth: Option<u32>,
}

impl<'a> ExprEmitter<'a> {
    pub fn new(
        fn_indices: &'a HashMap<String, u32>,
        rt: &'a RuntimeFuncIndices,
        string_literals: &'a HashMap<String, StringLiteral>,
        host_imports: &'a HashMap<String, u32>,
        type_fields: &'a HashMap<(String, String), u32>,
    ) -> Self {
        ExprEmitter {
            locals: HashMap::new(),
            next_local: 0,
            fn_indices,
            rt,
            instructions: Vec::new(),
            string_literals,
            host_imports,
            type_fields,
            block_depth: 0,
            tco_loop_depth: None,
        }
    }

    /// Set TCO loop depth (called by emitter before emitting body).
    pub fn enable_tco_loop(&mut self) {
        self.tco_loop_depth = Some(self.block_depth);
    }

    pub fn add_params(&mut self, params: &[(String, String)]) {
        for (name, _type_ann) in params {
            let idx = self.next_local;
            self.locals.insert(name.clone(), idx);
            self.next_local += 1;
        }
    }

    pub fn emit_body(&mut self, body: &FnBody) {
        match body {
            FnBody::Block(stmts) => self.emit_block(stmts),
        }
    }

    fn emit_block(&mut self, stmts: &[Stmt]) {
        if stmts.is_empty() {
            self.emit_const(value::CONST_UNIT);
            return;
        }

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            match stmt {
                Stmt::Binding(name, _type_ann, expr) => {
                    self.emit_expr(&expr.node);
                    let idx = self.next_local;
                    self.locals.insert(name.clone(), idx);
                    self.next_local += 1;
                    self.instructions.push(Instruction::LocalSet(idx));
                    if is_last {
                        self.emit_const(value::CONST_UNIT);
                    }
                }
                Stmt::Expr(expr) => {
                    self.emit_expr(&expr.node);
                    if !is_last {
                        self.instructions.push(Instruction::Drop);
                    }
                }
            }
        }
    }

    fn emit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(lit) => self.emit_literal(lit),

            Expr::Ident(name) => {
                if let Some(&idx) = self.locals.get(name) {
                    self.instructions.push(Instruction::LocalGet(idx));
                } else {
                    self.emit_const(value::CONST_UNIT);
                }
            }

            Expr::Resolved(slot) => {
                self.instructions.push(Instruction::LocalGet(*slot as u32));
            }

            Expr::BinOp(op, lhs, rhs) => {
                self.emit_expr(&lhs.node);
                self.emit_expr(&rhs.node);
                let rt_fn = match op {
                    BinOp::Add => self.rt.int_add,
                    BinOp::Sub => self.rt.int_sub,
                    BinOp::Mul => self.rt.int_mul,
                    BinOp::Div => self.rt.int_div,
                    BinOp::Eq => self.rt.int_eq,
                    BinOp::Neq => self.rt.int_ne,
                    BinOp::Lt => self.rt.int_lt,
                    BinOp::Gt => self.rt.int_gt,
                    BinOp::Lte => self.rt.int_le,
                    BinOp::Gte => self.rt.int_ge,
                };
                self.instructions.push(Instruction::Call(rt_fn));
            }

            Expr::FnCall(callee, args) => {
                self.emit_fn_call(callee, args);
            }

            Expr::Match { subject, arms } => {
                self.emit_match(subject, arms);
            }

            Expr::Constructor(name, inner) => {
                self.emit_constructor(name, inner);
            }

            Expr::ErrorProp(inner) => {
                // Evaluate inner, check if Err → return Err, else unwrap Ok
                self.emit_expr(&inner.node);
                let val_local = self.next_local;
                self.next_local += 1;
                self.instructions.push(Instruction::LocalSet(val_local));
                // Check if HeapRef
                self.instructions.push(Instruction::LocalGet(val_local));
                self.instructions.push(Instruction::I64Const(60));
                self.instructions.push(Instruction::I64ShrU);
                self.instructions
                    .push(Instruction::I64Const(value::TAG_HEAP as i64));
                self.instructions.push(Instruction::I64Eq);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                // Check if Err (obj_kind==WRAPPER && obj_tag==WRAP_ERR)
                self.instructions.push(Instruction::LocalGet(val_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_WRAPPER as i32));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                self.instructions.push(Instruction::LocalGet(val_local));
                self.instructions.push(Instruction::Call(self.rt.obj_tag));
                self.instructions
                    .push(Instruction::I32Const(value::WRAP_ERR as i32));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Result(wasm_encoder::ValType::I64));
                // Is Err → return the Err value as-is (early return)
                self.instructions.push(Instruction::LocalGet(val_local));
                self.instructions.push(Instruction::Return);
                self.emit_else();
                // Is Ok → unwrap
                self.instructions.push(Instruction::LocalGet(val_local));
                self.instructions.push(Instruction::Call(self.rt.unwrap));
                self.emit_end();
                self.emit_else();
                // Not a wrapper → pass through
                self.instructions.push(Instruction::LocalGet(val_local));
                self.emit_end();
                self.emit_else();
                // Not a HeapRef → pass through
                self.instructions.push(Instruction::LocalGet(val_local));
                self.emit_end();
            }

            Expr::InterpolatedStr(parts) => {
                // MVP: concatenate parts. For now just emit the first literal or Unit.
                if parts.len() == 1 {
                    match &parts[0] {
                        StrPart::Literal(s) => self.emit_string_literal(s),
                        StrPart::Parsed(expr) => self.emit_expr(&expr.node),
                    }
                } else {
                    // Multi-part interpolation: emit Unit for now
                    // TODO: implement string concatenation runtime
                    self.emit_const(value::CONST_UNIT);
                }
            }

            Expr::List(items) => {
                // Build list from right to left using list_cons
                if items.is_empty() {
                    self.emit_const(value::CONST_EMPTY_LIST);
                } else {
                    // Start with empty list, prepend from last to first
                    self.emit_const(value::CONST_EMPTY_LIST);
                    for item in items.iter().rev() {
                        // Stack: [tail]
                        let tail_local = self.next_local;
                        self.next_local += 1;
                        self.instructions.push(Instruction::LocalSet(tail_local));
                        self.emit_expr(&item.node); // head
                        self.instructions.push(Instruction::LocalGet(tail_local)); // tail
                        self.instructions.push(Instruction::Call(self.rt.list_cons));
                    }
                }
            }

            Expr::Tuple(items) => {
                // Allocate tuple on heap: header + N fields
                if items.is_empty() {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    let count = items.len();
                    // Alloc: 8 (header) + count * 8 (fields)
                    let size = 8 + count * 8;
                    let ptr_local = self.next_local;
                    self.next_local += 1;
                    self.instructions.push(Instruction::I32Const(size as i32));
                    self.instructions.push(Instruction::Call(self.rt.alloc));
                    self.instructions.push(Instruction::LocalSet(ptr_local));
                    // Store header
                    self.instructions.push(Instruction::LocalGet(ptr_local));
                    self.instructions
                        .push(Instruction::I64Const(value::make_header(
                            value::OBJ_TUPLE,
                            0,
                            0,
                            count as u64,
                        ) as i64));
                    self.instructions
                        .push(Instruction::I64Store(wasm_encoder::MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));
                    // Store fields
                    for (i, item) in items.iter().enumerate() {
                        self.instructions.push(Instruction::LocalGet(ptr_local));
                        self.emit_expr(&item.node);
                        self.instructions
                            .push(Instruction::I64Store(wasm_encoder::MemArg {
                                offset: (8 + i * 8) as u64,
                                align: 3,
                                memory_index: 0,
                            }));
                    }
                    // Return HeapRef
                    self.instructions.push(Instruction::I64Const(
                        (value::TAG_HEAP << value::TAG_SHIFT) as i64,
                    ));
                    self.instructions.push(Instruction::LocalGet(ptr_local));
                    self.instructions.push(Instruction::I64ExtendI32U);
                    self.instructions.push(Instruction::I64Or);
                }
            }

            Expr::RecordCreate { type_name, fields } => {
                let count = fields.len();
                let size = 8 + count * 8;
                let ptr_local = self.next_local;
                self.next_local += 1;
                self.instructions.push(Instruction::I32Const(size as i32));
                self.instructions.push(Instruction::Call(self.rt.alloc));
                self.instructions.push(Instruction::LocalSet(ptr_local));
                // Store header (type_id=0 for now — TODO: proper type registry)
                self.instructions.push(Instruction::LocalGet(ptr_local));
                self.instructions
                    .push(Instruction::I64Const(value::make_header(
                        value::OBJ_RECORD,
                        0,
                        0,
                        count as u64,
                    ) as i64));
                self.instructions
                    .push(Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                // Store fields in declaration order
                for (i, (_name, expr)) in fields.iter().enumerate() {
                    self.instructions.push(Instruction::LocalGet(ptr_local));
                    self.emit_expr(&expr.node);
                    self.instructions
                        .push(Instruction::I64Store(wasm_encoder::MemArg {
                            offset: (8 + i * 8) as u64,
                            align: 3,
                            memory_index: 0,
                        }));
                }
                // Return HeapRef
                self.instructions.push(Instruction::I64Const(
                    (value::TAG_HEAP << value::TAG_SHIFT) as i64,
                ));
                self.instructions.push(Instruction::LocalGet(ptr_local));
                self.instructions.push(Instruction::I64ExtendI32U);
                self.instructions.push(Instruction::I64Or);
                let _ = type_name; // TODO: use type_name for type_id lookup
            }

            Expr::Attr(base_expr, field_name) => {
                // Field access on a record: base.field → obj_field(base, idx)
                self.emit_expr(&base_expr.node);
                // Try to resolve field index from type_fields
                // For now use sequential index based on field_name hash
                // TODO: proper field index resolution from type metadata
                let field_idx = if let Expr::Ident(_base_name) = &base_expr.node {
                    // Look up field index — for now just use 0 as fallback
                    self.type_fields
                        .iter()
                        .find(|((_, f), _)| f == field_name)
                        .map(|(_, &idx)| idx)
                        .unwrap_or(0)
                } else {
                    0
                };
                self.instructions
                    .push(Instruction::I32Const(field_idx as i32));
                self.instructions.push(Instruction::Call(self.rt.obj_field));
            }

            Expr::TailCall(tc) => {
                let (fn_name, args) = tc.as_ref();
                if let Some(loop_depth) = self.tco_loop_depth {
                    // Self-TCO: update params and branch back to loop
                    for arg in args {
                        self.emit_expr(&arg.node);
                    }
                    // Save args to temps (reverse order to avoid stomping)
                    let arg_count = args.len();
                    let tmp_base = self.next_local;
                    self.next_local += arg_count as u32;
                    for i in (0..arg_count).rev() {
                        self.instructions
                            .push(Instruction::LocalSet(tmp_base + i as u32));
                    }
                    // Copy temps back to param locals (params are locals 0..N)
                    for i in 0..arg_count {
                        self.instructions
                            .push(Instruction::LocalGet(tmp_base + i as u32));
                        self.instructions.push(Instruction::LocalSet(i as u32));
                    }
                    // Branch to TCO loop: br depth = current_depth - loop_depth
                    let br_depth = self.block_depth - loop_depth;
                    self.instructions.push(Instruction::Br(br_depth));
                    // Mark unreachable — code after Br is never executed but
                    // WASM validator needs to know the stack is invalid here
                    self.instructions.push(Instruction::Unreachable);
                } else {
                    // No TCO loop — fall back to regular recursive call
                    for arg in args {
                        self.emit_expr(&arg.node);
                    }
                    if let Some(&fn_idx) = self.fn_indices.get(fn_name.as_str()) {
                        self.instructions.push(Instruction::Call(fn_idx));
                    } else {
                        for _ in args {
                            self.instructions.push(Instruction::Drop);
                        }
                        self.emit_const(value::CONST_UNIT);
                    }
                }
                let _ = fn_name;
            }

            Expr::RecordUpdate { .. } | Expr::MapLiteral(_) | Expr::IndependentProduct(_, _) => {
                // Not yet supported
                self.emit_const(value::CONST_UNIT);
            }
        }
    }

    fn emit_fn_call(&mut self, callee: &Spanned<Expr>, args: &[Spanned<Expr>]) {
        for arg in args {
            self.emit_expr(&arg.node);
        }

        match &callee.node {
            Expr::Ident(name) => {
                if let Some(&fn_idx) = self.fn_indices.get(name.as_str()) {
                    self.instructions.push(Instruction::Call(fn_idx));
                } else {
                    for _ in args {
                        self.instructions.push(Instruction::Drop);
                    }
                    self.emit_const(value::CONST_UNIT);
                }
            }
            Expr::Attr(base, method) => {
                let qualified = if let Expr::Ident(ns) = &base.node {
                    format!("{}.{}", ns, method)
                } else {
                    method.clone()
                };
                if let Some(&fn_idx) = self.fn_indices.get(qualified.as_str()) {
                    self.instructions.push(Instruction::Call(fn_idx));
                } else if qualified == "Result.Ok" && args.len() == 1 {
                    // Result.Ok(x) → wrap(WRAP_OK, x)
                    // args already on stack from push above — but we pushed all args first.
                    // We need to re-emit: drop the already-pushed arg, emit wrap properly.
                    // Actually args are already on the stack. Push tag before them.
                    // Wait — args were pushed before this match. So stack has [arg].
                    // We need [tag, arg] for $wrap. Insert tag under arg.
                    // Simplest: use a local to reorder.
                    let tmp = self.next_local;
                    self.next_local += 1;
                    self.instructions.push(Instruction::LocalSet(tmp)); // save arg
                    self.instructions
                        .push(Instruction::I32Const(value::WRAP_OK as i32));
                    self.instructions.push(Instruction::LocalGet(tmp)); // restore arg
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                } else if qualified == "Result.Err" && args.len() == 1 {
                    let tmp = self.next_local;
                    self.next_local += 1;
                    self.instructions.push(Instruction::LocalSet(tmp));
                    self.instructions
                        .push(Instruction::I32Const(value::WRAP_ERR as i32));
                    self.instructions.push(Instruction::LocalGet(tmp));
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                } else if qualified == "Option.Some" && args.len() == 1 {
                    let tmp = self.next_local;
                    self.next_local += 1;
                    self.instructions.push(Instruction::LocalSet(tmp));
                    self.instructions
                        .push(Instruction::I32Const(value::WRAP_SOME as i32));
                    self.instructions.push(Instruction::LocalGet(tmp));
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                } else if qualified == "Option.None" {
                    for _ in args {
                        self.instructions.push(Instruction::Drop);
                    }
                    self.emit_const(value::CONST_NONE);
                } else if let Some(&host_fn) = self.host_imports.get(qualified.as_str()) {
                    // Host import (Console.print, etc.) — call and push Unit
                    self.instructions.push(Instruction::Call(host_fn));
                    self.emit_const(value::CONST_UNIT);
                } else if qualified == "List.prepend" && args.len() == 2 {
                    // List.prepend(item, list) → list_cons(item, list)
                    // args already on stack: [item, list]
                    self.instructions.push(Instruction::Call(self.rt.list_cons));
                } else {
                    // Unknown builtin — drop args, return Unit
                    for _ in args {
                        self.instructions.push(Instruction::Drop);
                    }
                    self.emit_const(value::CONST_UNIT);
                }
            }
            _ => {
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.emit_const(value::CONST_UNIT);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Match
    // -----------------------------------------------------------------------

    fn emit_match(&mut self, subject: &Spanned<Expr>, arms: &[MatchArm]) {
        self.emit_expr(&subject.node);
        let subj_local = self.next_local;
        self.next_local += 1;
        self.instructions.push(Instruction::LocalSet(subj_local));

        // Use void if/else + result local pattern.
        // This allows TailCall arms to Br without leaving a value on stack.
        let result_local = self.next_local;
        self.next_local += 1;
        self.emit_const(value::CONST_UNIT);
        self.instructions.push(Instruction::LocalSet(result_local));

        self.emit_match_arms_void(subj_local, result_local, arms, 0);

        self.instructions.push(Instruction::LocalGet(result_local));
    }

    /// Emit match arms using void if/else + result local.
    /// Arms store their result to `result_local`. TailCall arms can safely Br.
    fn emit_match_arms_void(
        &mut self,
        subj_local: u32,
        result_local: u32,
        arms: &[MatchArm],
        idx: usize,
    ) {
        if idx >= arms.len() {
            return; // result_local already has Unit
        }

        let arm = &arms[idx];
        let is_last = idx == arms.len() - 1;

        match &arm.pattern {
            Pattern::Wildcard => {
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
            }
            Pattern::Ident(name) => {
                let bind_local = self.next_local;
                self.next_local += 1;
                self.locals.insert(name.clone(), bind_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::LocalSet(bind_local));
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
            }
            Pattern::Literal(lit) => {
                let encoded = match lit {
                    Literal::Int(n) => value::encode_int(*n),
                    Literal::Bool(b) => {
                        if *b {
                            value::CONST_TRUE
                        } else {
                            value::CONST_FALSE
                        }
                    }
                    _ => value::CONST_UNIT,
                };
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I64Const(encoded as i64));
                self.instructions.push(Instruction::I64Eq);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_match_arms_void(subj_local, result_local, arms, idx + 1);
                }
                self.emit_end();
            }
            Pattern::Constructor(ctor_name, bindings) => {
                self.emit_constructor_pattern_void(
                    subj_local,
                    result_local,
                    ctor_name,
                    bindings,
                    arm,
                    arms,
                    idx,
                );
            }
            Pattern::EmptyList => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I64Const(value::CONST_EMPTY_LIST as i64));
                self.instructions.push(Instruction::I64Eq);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_match_arms_void(subj_local, result_local, arms, idx + 1);
                }
                self.emit_end();
            }
            Pattern::Cons(head_name, tail_name) => {
                // Check: is HeapRef
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I64Const(60));
                self.instructions.push(Instruction::I64ShrU);
                self.instructions
                    .push(Instruction::I64Const(value::TAG_HEAP as i64));
                self.instructions.push(Instruction::I64Eq);
                self.emit_if(wasm_encoder::BlockType::Empty);
                // Check obj_kind == OBJ_LIST_CONS
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_LIST_CONS as i32));
                self.instructions.push(Instruction::I32Eq);
                self.emit_if(wasm_encoder::BlockType::Empty);
                // Bind head and tail
                let head_local = self.next_local;
                self.next_local += 1;
                self.locals.insert(head_name.clone(), head_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Const(0));
                self.instructions.push(Instruction::Call(self.rt.obj_field));
                self.instructions.push(Instruction::LocalSet(head_local));

                let tail_local = self.next_local;
                self.next_local += 1;
                self.locals.insert(tail_name.clone(), tail_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I32Const(1));
                self.instructions.push(Instruction::Call(self.rt.obj_field));
                self.instructions.push(Instruction::LocalSet(tail_local));

                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_match_arms_void(subj_local, result_local, arms, idx + 1);
                }
                self.emit_end(); // end inner if
                if !is_last {
                    self.emit_else();
                    self.emit_match_arms_void(subj_local, result_local, arms, idx + 1);
                }
                self.emit_end(); // end outer if
            }
            Pattern::Tuple(_) => {
                // TODO
            }
        }
    }

    fn emit_constructor_pattern_void(
        &mut self,
        subj_local: u32,
        result_local: u32,
        ctor_name: &str,
        bindings: &[String],
        arm: &MatchArm,
        arms: &[MatchArm],
        idx: usize,
    ) {
        let is_last = idx == arms.len() - 1;

        let wrapper_tag = match ctor_name {
            "Result.Ok" => Some(value::WRAP_OK),
            "Result.Err" => Some(value::WRAP_ERR),
            "Option.Some" => Some(value::WRAP_SOME),
            "Option.None" => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I64Const(value::CONST_NONE as i64));
                self.instructions.push(Instruction::I64Eq);
                self.emit_if(wasm_encoder::BlockType::Empty);
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::LocalSet(result_local));
                if !is_last {
                    self.emit_else();
                    self.emit_match_arms_void(subj_local, result_local, arms, idx + 1);
                }
                self.emit_end();
                return;
            }
            _ => None,
        };

        if let Some(expected_tag) = wrapper_tag {
            // Check: is HeapRef
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::I64Const(60));
            self.instructions.push(Instruction::I64ShrU);
            self.instructions
                .push(Instruction::I64Const(value::TAG_HEAP as i64));
            self.instructions.push(Instruction::I64Eq);
            self.emit_if(wasm_encoder::BlockType::Empty);
            // Check obj_kind == OBJ_WRAPPER
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::Call(self.rt.obj_kind));
            self.instructions
                .push(Instruction::I32Const(value::OBJ_WRAPPER as i32));
            self.instructions.push(Instruction::I32Eq);
            self.emit_if(wasm_encoder::BlockType::Empty);
            // Check obj_tag == expected
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::Call(self.rt.obj_tag));
            self.instructions
                .push(Instruction::I32Const(expected_tag as i32));
            self.instructions.push(Instruction::I32Eq);
            self.emit_if(wasm_encoder::BlockType::Empty);

            // Bind inner value
            if let Some(binding_name) = bindings.first() {
                let bind_local = self.next_local;
                self.next_local += 1;
                self.locals.insert(binding_name.clone(), bind_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.unwrap));
                self.instructions.push(Instruction::LocalSet(bind_local));
            }

            self.emit_expr(&arm.body.node);
            self.instructions.push(Instruction::LocalSet(result_local));

            // Close all three ifs
            for _ in 0..3 {
                if !is_last {
                    self.emit_else();
                    self.emit_match_arms_void(subj_local, result_local, arms, idx + 1);
                }
                self.emit_end();
            }
        } else {
            // Unknown constructor
        }
    }

    // -----------------------------------------------------------------------
    // Constructors
    // -----------------------------------------------------------------------

    fn emit_constructor(&mut self, name: &str, inner: &Option<Box<Spanned<Expr>>>) {
        match name {
            "Result.Ok" => {
                if let Some(expr) = inner {
                    self.instructions
                        .push(Instruction::I32Const(value::WRAP_OK as i32));
                    self.emit_expr(&expr.node);
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                } else {
                    self.emit_const(value::CONST_UNIT);
                }
            }
            "Result.Err" => {
                if let Some(expr) = inner {
                    self.instructions
                        .push(Instruction::I32Const(value::WRAP_ERR as i32));
                    self.emit_expr(&expr.node);
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                } else {
                    self.emit_const(value::CONST_UNIT);
                }
            }
            "Option.Some" => {
                if let Some(expr) = inner {
                    self.instructions
                        .push(Instruction::I32Const(value::WRAP_SOME as i32));
                    self.emit_expr(&expr.node);
                    self.instructions.push(Instruction::Call(self.rt.wrap));
                } else {
                    self.emit_const(value::CONST_UNIT);
                }
            }
            "Option.None" => {
                self.emit_const(value::CONST_NONE);
            }
            _ => {
                // Unknown constructor — emit inner if present, else Unit
                if let Some(expr) = inner {
                    self.emit_expr(&expr.node);
                } else {
                    self.emit_const(value::CONST_UNIT);
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Literals
    // -----------------------------------------------------------------------

    fn emit_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Int(i) => {
                let encoded = value::encode_int(*i);
                self.instructions
                    .push(Instruction::I64Const(encoded as i64));
            }
            Literal::Float(f) => {
                // MVP: store as raw bits in Int tag (lossy but functional for now)
                let bits = f.to_bits();
                self.instructions.push(Instruction::I64Const(bits as i64));
            }
            Literal::Bool(b) => {
                self.emit_const(if *b {
                    value::CONST_TRUE
                } else {
                    value::CONST_FALSE
                });
            }
            Literal::Str(s) => {
                self.emit_string_literal(s);
            }
            Literal::Unit => {
                self.emit_const(value::CONST_UNIT);
            }
        }
    }

    fn emit_const(&mut self, bits: u64) {
        self.instructions.push(Instruction::I64Const(bits as i64));
    }

    /// Push an If instruction and track block depth.
    fn emit_if(&mut self, bt: wasm_encoder::BlockType) {
        self.instructions.push(Instruction::If(bt));
        self.block_depth += 1;
    }

    /// Push Else instruction (no depth change — same block).
    fn emit_else(&mut self) {
        self.instructions.push(Instruction::Else);
    }

    /// Push End instruction and decrement block depth.
    pub fn emit_end(&mut self) {
        self.instructions.push(Instruction::End);
        if self.block_depth > 0 {
            self.block_depth -= 1;
        }
    }

    /// Emit a HeapRef pointing to a static string object in the data section.
    fn emit_string_literal(&mut self, s: &str) {
        if let Some(&(offset, _len)) = self.string_literals.get(s) {
            // String object is already in data section at `offset`.
            // Return HeapRef pointing to it.
            self.instructions.push(Instruction::I64Const(
                (value::TAG_HEAP << value::TAG_SHIFT) as i64,
            ));
            self.instructions.push(Instruction::I64Const(offset as i64));
            self.instructions.push(Instruction::I64Or);
        } else {
            // Unknown string — should not happen if emitter collects all literals
            self.emit_const(value::CONST_UNIT);
        }
    }
}
