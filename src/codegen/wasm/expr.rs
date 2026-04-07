/// Expression emission: walks the Aver AST and emits native WASM instructions.
///
/// All values are i64 with native semantics:
///   Int: plain i64 value
///   Bool: 0 (false) or 1 (true) as i64
///   Unit: 0 as i64
///   a + b → i64.add (one instruction, no dispatch)
use std::collections::HashMap;

use wasm_encoder::Instruction;

use crate::ast::{BinOp, Expr, FnBody, Literal, Spanned, Stmt};

/// Context for emitting expressions within a single function body.
pub(super) struct ExprEmitter<'a> {
    /// Map from local variable name -> WASM local index.
    pub locals: HashMap<String, u32>,
    /// Next available local index.
    pub next_local: u32,
    /// Map from Aver function name -> WASM function index.
    pub fn_indices: &'a HashMap<String, u32>,
    /// Accumulated WASM instructions.
    pub instructions: Vec<Instruction<'a>>,
}

impl<'a> ExprEmitter<'a> {
    pub fn new(fn_indices: &'a HashMap<String, u32>) -> Self {
        ExprEmitter {
            locals: HashMap::new(),
            next_local: 0,
            fn_indices,
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
            self.instructions.push(Instruction::I64Const(0)); // Unit
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
                        self.instructions.push(Instruction::I64Const(0)); // Unit
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
                    self.instructions.push(Instruction::I64Const(0)); // unknown → Unit
                }
            }

            Expr::Resolved(slot) => {
                self.instructions.push(Instruction::LocalGet(*slot as u32));
            }

            Expr::BinOp(op, lhs, rhs) => {
                self.emit_expr(&lhs.node);
                self.emit_expr(&rhs.node);
                match op {
                    // Native integer arithmetic — one WASM instruction each
                    BinOp::Add => self.instructions.push(Instruction::I64Add),
                    BinOp::Sub => self.instructions.push(Instruction::I64Sub),
                    BinOp::Mul => self.instructions.push(Instruction::I64Mul),
                    BinOp::Div => self.instructions.push(Instruction::I64DivS),
                    // Comparisons: i64 → i64 (0 or 1)
                    BinOp::Eq => {
                        self.instructions.push(Instruction::I64Eq);
                        self.instructions.push(Instruction::I64ExtendI32U);
                    }
                    BinOp::Neq => {
                        self.instructions.push(Instruction::I64Ne);
                        self.instructions.push(Instruction::I64ExtendI32U);
                    }
                    BinOp::Lt => {
                        self.instructions.push(Instruction::I64LtS);
                        self.instructions.push(Instruction::I64ExtendI32U);
                    }
                    BinOp::Gt => {
                        self.instructions.push(Instruction::I64GtS);
                        self.instructions.push(Instruction::I64ExtendI32U);
                    }
                    BinOp::Lte => {
                        self.instructions.push(Instruction::I64LeS);
                        self.instructions.push(Instruction::I64ExtendI32U);
                    }
                    BinOp::Gte => {
                        self.instructions.push(Instruction::I64GeS);
                        self.instructions.push(Instruction::I64ExtendI32U);
                    }
                }
            }

            Expr::FnCall(callee, args) => {
                self.emit_fn_call(callee, args);
            }

            // MVP fallback: unsupported expressions return 0 (Unit)
            _ => {
                self.instructions.push(Instruction::I64Const(0));
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
                    self.instructions.push(Instruction::I64Const(0));
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
                } else {
                    for _ in args {
                        self.instructions.push(Instruction::Drop);
                    }
                    self.instructions.push(Instruction::I64Const(0));
                }
            }
            _ => {
                for _ in args {
                    self.instructions.push(Instruction::Drop);
                }
                self.instructions.push(Instruction::I64Const(0));
            }
        }
    }

    fn emit_literal(&mut self, lit: &Literal) {
        match lit {
            Literal::Int(i) => {
                self.instructions.push(Instruction::I64Const(*i));
            }
            Literal::Float(f) => {
                // Store f64 bits as i64 (MVP — proper f64 support later)
                self.instructions
                    .push(Instruction::I64Const(f.to_bits() as i64));
            }
            Literal::Bool(b) => {
                self.instructions
                    .push(Instruction::I64Const(if *b { 1 } else { 0 }));
            }
            Literal::Str(_) => {
                self.instructions.push(Instruction::I64Const(0)); // MVP
            }
            Literal::Unit => {
                self.instructions.push(Instruction::I64Const(0));
            }
        }
    }
}
