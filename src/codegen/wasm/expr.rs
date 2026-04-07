/// Expression emission: walks the Aver AST and emits WASM instructions.
///
/// All values are tagged i64 (4-bit tag + 60-bit payload).
/// Arithmetic goes through runtime helpers ($int_add etc.) that extract/repack payloads.
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::ast::{BinOp, Expr, FnBody, Literal, MatchArm, Pattern, Spanned, Stmt};

use super::runtime::RuntimeFuncIndices;
use super::value;

/// Context for emitting expressions within a single function body.
pub(super) struct ExprEmitter<'a> {
    pub locals: HashMap<String, u32>,
    pub next_local: u32,
    pub fn_indices: &'a HashMap<String, u32>,
    pub rt: &'a RuntimeFuncIndices,
    pub instructions: Vec<Instruction<'a>>,
}

impl<'a> ExprEmitter<'a> {
    pub fn new(fn_indices: &'a HashMap<String, u32>, rt: &'a RuntimeFuncIndices) -> Self {
        ExprEmitter {
            locals: HashMap::new(),
            next_local: 0,
            fn_indices,
            rt,
            instructions: Vec::new(),
        }
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

            // MVP fallback: unsupported expressions return Unit
            _ => {
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
        // Evaluate subject into a local
        self.emit_expr(&subject.node);
        let subj_local = self.next_local;
        self.next_local += 1;
        self.instructions.push(Instruction::LocalSet(subj_local));

        // Emit as nested if/else chain (block per arm)
        // Each arm: check pattern → if match, emit body; else try next arm
        self.emit_match_arms(subj_local, arms, 0);
    }

    fn emit_match_arms(&mut self, subj_local: u32, arms: &[MatchArm], idx: usize) {
        if idx >= arms.len() {
            // No arm matched — return Unit (non-exhaustive, shouldn't happen with typechecker)
            self.emit_const(value::CONST_UNIT);
            return;
        }

        let arm = &arms[idx];
        let is_last = idx == arms.len() - 1;

        match &arm.pattern {
            Pattern::Wildcard => {
                // Always matches — emit body directly
                self.emit_expr(&arm.body.node);
            }
            Pattern::Ident(name) => {
                // Bind subject to local, emit body
                let bind_local = self.next_local;
                self.next_local += 1;
                self.locals.insert(name.clone(), bind_local);
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::LocalSet(bind_local));
                self.emit_expr(&arm.body.node);
            }
            Pattern::Literal(lit) => {
                // Compare subject with encoded literal
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
                self.instructions
                    .push(Instruction::If(wasm_encoder::BlockType::Result(
                        wasm_encoder::ValType::I64,
                    )));
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::Else);
                if is_last {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    self.emit_match_arms(subj_local, arms, idx + 1);
                }
                self.instructions.push(Instruction::End);
            }
            Pattern::Constructor(ctor_name, bindings) => {
                self.emit_constructor_pattern(subj_local, ctor_name, bindings, arm, arms, idx);
            }
            Pattern::EmptyList => {
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I64Const(value::CONST_EMPTY_LIST as i64));
                self.instructions.push(Instruction::I64Eq);
                self.instructions
                    .push(Instruction::If(wasm_encoder::BlockType::Result(
                        wasm_encoder::ValType::I64,
                    )));
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::Else);
                if is_last {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    self.emit_match_arms(subj_local, arms, idx + 1);
                }
                self.instructions.push(Instruction::End);
            }
            Pattern::Cons(head_name, tail_name) => {
                // Check: is HeapRef && obj_kind == OBJ_LIST_CONS
                // Extract tag from subject
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::I64Const(60));
                self.instructions.push(Instruction::I64ShrU);
                self.instructions
                    .push(Instruction::I64Const(value::TAG_HEAP as i64));
                self.instructions.push(Instruction::I64Eq);
                self.instructions
                    .push(Instruction::If(wasm_encoder::BlockType::Result(
                        wasm_encoder::ValType::I64,
                    )));
                // Check obj_kind
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions.push(Instruction::Call(self.rt.obj_kind));
                self.instructions
                    .push(Instruction::I32Const(value::OBJ_LIST_CONS as i32));
                self.instructions.push(Instruction::I32Eq);
                self.instructions
                    .push(Instruction::If(wasm_encoder::BlockType::Result(
                        wasm_encoder::ValType::I64,
                    )));
                // Bind head = field[0], tail = field[1]
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
                self.instructions.push(Instruction::Else);
                if is_last {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    self.emit_match_arms(subj_local, arms, idx + 1);
                }
                self.instructions.push(Instruction::End);
                self.instructions.push(Instruction::Else);
                if is_last {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    self.emit_match_arms(subj_local, arms, idx + 1);
                }
                self.instructions.push(Instruction::End);
            }
            Pattern::Tuple(_) => {
                // TODO: tuple patterns
                self.emit_const(value::CONST_UNIT);
            }
        }
    }

    fn emit_constructor_pattern(
        &mut self,
        subj_local: u32,
        ctor_name: &str,
        bindings: &[String],
        arm: &MatchArm,
        arms: &[MatchArm],
        idx: usize,
    ) {
        let is_last = idx == arms.len() - 1;

        // Determine expected wrapper tag
        let wrapper_tag = match ctor_name {
            "Result.Ok" => Some(value::WRAP_OK),
            "Result.Err" => Some(value::WRAP_ERR),
            "Option.Some" => Some(value::WRAP_SOME),
            "Option.None" => {
                // None is immediate, not a heap object
                self.instructions.push(Instruction::LocalGet(subj_local));
                self.instructions
                    .push(Instruction::I64Const(value::CONST_NONE as i64));
                self.instructions.push(Instruction::I64Eq);
                self.instructions
                    .push(Instruction::If(wasm_encoder::BlockType::Result(
                        wasm_encoder::ValType::I64,
                    )));
                self.emit_expr(&arm.body.node);
                self.instructions.push(Instruction::Else);
                if is_last {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    self.emit_match_arms(subj_local, arms, idx + 1);
                }
                self.instructions.push(Instruction::End);
                return;
            }
            _ => None, // User-defined variants — TODO
        };

        if let Some(expected_tag) = wrapper_tag {
            // Check: is HeapRef
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::I64Const(60));
            self.instructions.push(Instruction::I64ShrU);
            self.instructions
                .push(Instruction::I64Const(value::TAG_HEAP as i64));
            self.instructions.push(Instruction::I64Eq);
            self.instructions
                .push(Instruction::If(wasm_encoder::BlockType::Result(
                    wasm_encoder::ValType::I64,
                )));
            // Check obj_kind == OBJ_WRAPPER
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::Call(self.rt.obj_kind));
            self.instructions
                .push(Instruction::I32Const(value::OBJ_WRAPPER as i32));
            self.instructions.push(Instruction::I32Eq);
            self.instructions
                .push(Instruction::If(wasm_encoder::BlockType::Result(
                    wasm_encoder::ValType::I64,
                )));
            // Check obj_tag == expected
            self.instructions.push(Instruction::LocalGet(subj_local));
            self.instructions.push(Instruction::Call(self.rt.obj_tag));
            self.instructions
                .push(Instruction::I32Const(expected_tag as i32));
            self.instructions.push(Instruction::I32Eq);
            self.instructions
                .push(Instruction::If(wasm_encoder::BlockType::Result(
                    wasm_encoder::ValType::I64,
                )));

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

            // Close all three ifs with else → fallthrough
            for _ in 0..3 {
                self.instructions.push(Instruction::Else);
                if is_last {
                    self.emit_const(value::CONST_UNIT);
                } else {
                    self.emit_match_arms(subj_local, arms, idx + 1);
                }
                self.instructions.push(Instruction::End);
            }
        } else {
            // Unknown constructor — fallthrough
            self.emit_const(value::CONST_UNIT);
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
                // MVP: strings not yet in data section — emit as Unit
                let _ = s;
                self.emit_const(value::CONST_UNIT);
            }
            Literal::Unit => {
                self.emit_const(value::CONST_UNIT);
            }
        }
    }

    fn emit_const(&mut self, bits: u64) {
        self.instructions.push(Instruction::I64Const(bits as i64));
    }
}
