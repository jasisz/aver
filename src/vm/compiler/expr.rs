use super::{CompileError, FnCompiler};
use crate::ast::{BinOp, Expr, Literal, Spanned, Stmt, StrPart};
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;

impl<'a> FnCompiler<'a> {
    pub(super) fn compile_body(&mut self, stmts: &[Stmt]) -> Result<(), CompileError> {
        if stmts.is_empty() {
            self.emit_op(LOAD_UNIT);
            self.emit_op(RETURN);
            return Ok(());
        }

        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            self.compile_stmt(stmt, is_last)?;
        }

        self.emit_op(RETURN);
        Ok(())
    }

    pub(super) fn compile_stmt(&mut self, stmt: &Stmt, is_tail: bool) -> Result<(), CompileError> {
        match stmt {
            Stmt::Binding(name, _type_ann, expr) => {
                self.compile_expr(expr)?;
                if let Some(&slot) = self.local_slots.get(name) {
                    self.emit_op(STORE_LOCAL);
                    self.emit_u8(slot as u8);
                }
            }
            Stmt::Expr(expr) => {
                self.compile_expr(expr)?;
                if !is_tail {
                    self.emit_op(POP);
                }
            }
        }
        Ok(())
    }

    pub(super) fn compile_expr(&mut self, expr: &Spanned<Expr>) -> Result<(), CompileError> {
        self.note_line(expr.line);

        if self.try_compile_leaf_expr(&expr.node)? {
            return Ok(());
        }

        match &expr.node {
            Expr::Literal(lit) => self.compile_literal(lit),
            Expr::Resolved(slot) => {
                self.emit_op(LOAD_LOCAL);
                self.emit_u8(*slot as u8);
                Ok(())
            }
            Expr::Ident(name) => self.compile_ident(name),
            Expr::BinOp(op, left, right) => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;
                self.emit_binop(*op);
                Ok(())
            }
            Expr::FnCall(fn_expr, args) => self.compile_call(fn_expr, args),
            Expr::TailCall(boxed) => {
                let (target, args) = boxed.as_ref();
                self.compile_tail_call(target, args)
            }
            Expr::Match { subject, arms } => self.compile_match(subject, arms),
            Expr::Constructor(name, arg) => self.compile_constructor(name, arg.as_deref()),
            Expr::ErrorProp(inner) => self.compile_error_prop(inner),
            Expr::List(items) => self.compile_list(items),
            Expr::InterpolatedStr(parts) => self.compile_interpolated_str(parts),
            Expr::RecordCreate { type_name, fields } => {
                self.compile_record_create(type_name, fields)
            }
            Expr::RecordUpdate {
                type_name,
                base,
                updates,
            } => self.compile_record_update(type_name, base, updates),
            Expr::Attr(obj, field) => self.compile_attr(obj, field),
            Expr::Tuple(items) => self.compile_tuple(items),
            Expr::IndependentProduct(items, unwrap) => {
                self.compile_independent_product(items, *unwrap)
            }
            Expr::MapLiteral(entries) => self.compile_map(entries),
        }
    }

    pub(super) fn compile_literal(&mut self, lit: &Literal) -> Result<(), CompileError> {
        match lit {
            Literal::Int(i) => {
                let nv = NanValue::new_int(*i, self.arena);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            Literal::Float(f) => {
                let nv = NanValue::new_float(*f);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            Literal::Bool(true) => self.emit_op(LOAD_TRUE),
            Literal::Bool(false) => self.emit_op(LOAD_FALSE),
            Literal::Unit => self.emit_op(LOAD_UNIT),
            Literal::Str(s) => {
                let nv = NanValue::new_string_value(s, self.arena);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
        }
        Ok(())
    }

    fn compile_ident(&mut self, name: &str) -> Result<(), CompileError> {
        if let Some(&slot) = self.local_slots.get(name) {
            self.emit_op(LOAD_LOCAL);
            self.emit_u8(slot as u8);
        } else if let Some(&idx) = self.global_names.get(name) {
            self.emit_op(LOAD_GLOBAL);
            self.emit_u16(idx);
        } else if let Some(&fn_id) = self.module_scope.get(name) {
            let qualified_name = self.code_store.get(fn_id).name.clone();
            let symbol_id = self
                .symbols
                .find(&qualified_name)
                .ok_or_else(|| CompileError {
                    msg: format!("missing VM symbol for module function: {}", qualified_name),
                })?;
            let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
            self.emit_op(LOAD_CONST);
            self.emit_u16(idx);
        } else if let Some(symbol_id) = self.symbols.find(name)
            && self
                .symbols
                .get(symbol_id)
                .is_some_and(|info| info.kind.is_some())
        {
            let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
            self.emit_op(LOAD_CONST);
            self.emit_u16(idx);
        } else {
            return Err(CompileError {
                msg: format!("undefined variable: {}", name),
            });
        }
        Ok(())
    }

    fn emit_binop(&mut self, op: BinOp) {
        match op {
            BinOp::Add => self.emit_op(ADD),
            BinOp::Sub => self.emit_op(SUB),
            BinOp::Mul => self.emit_op(MUL),
            BinOp::Div => self.emit_op(DIV),
            BinOp::Eq => self.emit_op(EQ),
            BinOp::Lt => self.emit_op(LT),
            BinOp::Gt => self.emit_op(GT),
            BinOp::Neq => {
                self.emit_op(EQ);
                self.emit_op(NOT);
            }
            BinOp::Lte => {
                self.emit_op(GT);
                self.emit_op(NOT);
            }
            BinOp::Gte => {
                self.emit_op(LT);
                self.emit_op(NOT);
            }
        }
    }

    fn compile_error_prop(&mut self, inner: &Spanned<Expr>) -> Result<(), CompileError> {
        self.compile_expr(inner)?;
        self.emit_op(PROPAGATE_ERR);
        Ok(())
    }

    fn compile_list(&mut self, items: &[Spanned<Expr>]) -> Result<(), CompileError> {
        if items.is_empty() {
            self.emit_op(LIST_NIL);
            return Ok(());
        }
        for item in items {
            self.compile_expr(item)?;
        }
        self.emit_op(LIST_NEW);
        self.emit_u8(items.len() as u8);
        Ok(())
    }

    fn compile_interpolated_str(&mut self, parts: &[StrPart]) -> Result<(), CompileError> {
        if parts.is_empty() {
            let nv = NanValue::new_string_value("", self.arena);
            let cidx = self.add_constant(nv);
            self.emit_op(LOAD_CONST);
            self.emit_u16(cidx);
            return Ok(());
        }

        let mut first = true;
        for part in parts {
            match part {
                StrPart::Literal(s) => {
                    let nv = NanValue::new_string_value(s, self.arena);
                    let cidx = self.add_constant(nv);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(cidx);
                }
                StrPart::Parsed(expr) => {
                    self.compile_expr(expr)?;
                    let empty_nv = NanValue::new_string_value("", self.arena);
                    let empty_const = self.add_constant(empty_nv);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(empty_const);
                    self.emit_op(CONCAT);
                }
            }
            if !first {
                self.emit_op(CONCAT);
            }
            first = false;
        }
        Ok(())
    }

    fn compile_record_create(
        &mut self,
        type_name: &str,
        fields: &[(String, Spanned<Expr>)],
    ) -> Result<(), CompileError> {
        let type_id = self
            .resolve_type_id(type_name)
            .ok_or_else(|| CompileError {
                msg: format!("unknown record type: {}", type_name),
            })?;

        let field_names = self.arena.get_field_names(type_id).to_vec();
        for expected_name in &field_names {
            let (_, expr) = fields
                .iter()
                .find(|(n, _)| n == expected_name)
                .ok_or_else(|| CompileError {
                    msg: format!("missing field {} in record {}", expected_name, type_name),
                })?;
            self.compile_expr(expr)?;
        }

        self.emit_op(RECORD_NEW);
        self.emit_u16(type_id as u16);
        self.emit_u8(field_names.len() as u8);
        Ok(())
    }

    fn compile_record_update(
        &mut self,
        type_name: &str,
        base: &Spanned<Expr>,
        updates: &[(String, Spanned<Expr>)],
    ) -> Result<(), CompileError> {
        let type_id = self
            .resolve_type_id(type_name)
            .ok_or_else(|| CompileError {
                msg: format!("unknown record type: {}", type_name),
            })?;
        let field_names = self.arena.get_field_names(type_id).to_vec();
        let mut updated_fields = Vec::with_capacity(updates.len());

        self.compile_expr(base)?;

        for (field_idx, field_name) in field_names.iter().enumerate() {
            if let Some((_, update_expr)) = updates.iter().find(|(n, _)| n == field_name) {
                self.compile_expr(update_expr)?;
                updated_fields.push(field_idx as u8);
            }
        }

        self.emit_op(RECORD_UPDATE);
        self.emit_u16(type_id as u16);
        self.emit_u8(updated_fields.len() as u8);
        for field_idx in updated_fields {
            self.emit_u8(field_idx);
        }
        Ok(())
    }

    fn compile_tuple(&mut self, items: &[Spanned<Expr>]) -> Result<(), CompileError> {
        for item in items {
            self.compile_expr(item)?;
        }
        self.emit_op(TUPLE_NEW);
        self.emit_u8(items.len() as u8);
        Ok(())
    }

    fn compile_independent_product(
        &mut self,
        items: &[Spanned<Expr>],
        unwrap: bool,
    ) -> Result<(), CompileError> {
        // Push a callable value plus its args for each branch, then emit CALL_PAR
        // with per-branch arities. Runtime dispatch resolves the callable the same
        // way as CALL_VALUE, so aliases like `f = foo; (f(1), f(2))!` work in VM.
        let mut arg_counts: Vec<u8> = Vec::with_capacity(items.len());

        for item in items {
            // Unwrap ErrorProp wrapper if present (from ?! surface syntax)
            let call_expr = match &item.node {
                Expr::ErrorProp(inner) => &inner.node,
                other => other,
            };
            match call_expr {
                Expr::FnCall(fn_expr, args) => {
                    self.compile_expr(fn_expr)?;
                    for arg in args {
                        self.compile_expr(arg)?;
                    }
                    arg_counts.push(args.len() as u8);
                }
                _ => {
                    // Fallback: compile normally (sequential) and use TUPLE_NEW
                    // This shouldn't happen if typechecker validates properly
                    for item in items {
                        self.compile_expr(item)?;
                    }
                    self.emit_op(TUPLE_NEW);
                    self.emit_u8(items.len() as u8);
                    return Ok(());
                }
            }
        }

        // Emit CALL_PAR: count unwrap [argc:u8 × count]
        self.emit_op(CALL_PAR);
        self.emit_u8(items.len() as u8);
        self.emit_u8(if unwrap { 1 } else { 0 });
        for argc in arg_counts {
            self.emit_u8(argc);
        }
        Ok(())
    }

    fn compile_map(
        &mut self,
        entries: &[(Spanned<Expr>, Spanned<Expr>)],
    ) -> Result<(), CompileError> {
        let empty_map = self.arena.push_map(crate::nan_value::PersistentMap::new());
        let nv = NanValue::new_map(empty_map);
        let idx = self.add_constant(nv);
        self.emit_op(LOAD_CONST);
        self.emit_u16(idx);

        for (key, value) in entries {
            self.compile_expr(key)?;
            self.compile_expr(value)?;
            let symbol_id = self.symbols.intern_builtin(VmBuiltin::MapSet);
            self.emit_op(CALL_BUILTIN);
            self.emit_u32(symbol_id);
            self.emit_u8(3);
        }
        Ok(())
    }
}
