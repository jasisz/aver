use super::{CompileError, FnCompiler};
use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::hir::{ResolvedExpr, ResolvedStmt, ResolvedStrPart};
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;

impl<'a> FnCompiler<'a> {
    pub(super) fn compile_body(&mut self, stmts: &[ResolvedStmt]) -> Result<(), CompileError> {
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

    pub(super) fn compile_stmt(
        &mut self,
        stmt: &ResolvedStmt,
        is_tail: bool,
    ) -> Result<(), CompileError> {
        match stmt {
            ResolvedStmt::Binding { name, value, .. } => {
                self.compile_expr(value)?;
                if let Some(&slot) = self.local_slots.get(name) {
                    self.emit_op(STORE_LOCAL);
                    self.emit_u8(slot as u8);
                }
            }
            ResolvedStmt::Expr(value) => {
                self.compile_expr(value)?;
                if !is_tail {
                    self.emit_op(POP);
                }
            }
        }
        Ok(())
    }

    pub(super) fn compile_expr(
        &mut self,
        expr: &Spanned<ResolvedExpr>,
    ) -> Result<(), CompileError> {
        self.note_line(expr.line);

        if self.try_compile_leaf_expr(&expr.node)? {
            return Ok(());
        }

        match &expr.node {
            ResolvedExpr::Literal(lit) => self.compile_literal(lit),
            ResolvedExpr::Resolved { slot, last_use, .. } => {
                self.emit_op(if last_use.0 { MOVE_LOCAL } else { LOAD_LOCAL });
                self.emit_u8(*slot as u8);
                Ok(())
            }
            ResolvedExpr::Ident(name) => self.compile_ident(name),
            ResolvedExpr::BinOp(op, left, right) => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;
                self.emit_binop_typed(*op, left.ty(), right.ty());
                Ok(())
            }
            ResolvedExpr::Neg(inner) => {
                self.compile_expr(inner)?;
                // Iron — B1: pick a typed NEG when the operand's
                // `Spanned::ty()` resolves to a numeric primitive.
                // Skips the `is_int / is_float` branch in `NEG` and
                // (for Float) preserves `-0.0` because the typed
                // dispatch operates on the float bit pattern, not on
                // `0 - x` desugar.
                let op = match inner.ty() {
                    Some(crate::ast::Type::Int) => crate::vm::opcode::NEG_INT,
                    Some(crate::ast::Type::Float) => crate::vm::opcode::NEG_FLOAT,
                    _ => crate::vm::opcode::NEG,
                };
                self.emit_op(op);
                Ok(())
            }
            ResolvedExpr::Call(callee, args) => self.compile_call(callee, args),
            ResolvedExpr::TailCall { target, args } => self.compile_tail_call(*target, args),
            ResolvedExpr::Match { subject, arms } => self.compile_match(subject, arms),
            ResolvedExpr::Ctor(ctor, args) => self.compile_ctor(ctor, args),
            ResolvedExpr::ErrorProp(inner) => self.compile_error_prop(inner),
            ResolvedExpr::List(items) => self.compile_list(items),
            ResolvedExpr::InterpolatedStr(parts) => self.compile_interpolated_str(parts),
            ResolvedExpr::RecordCreate {
                type_id,
                type_name,
                fields,
            } => self.compile_record_create(*type_id, type_name, fields),
            ResolvedExpr::RecordUpdate {
                type_id,
                type_name,
                base,
                updates,
            } => self.compile_record_update(*type_id, type_name, base, updates),
            ResolvedExpr::Attr(obj, field) => self.compile_attr(obj, field),
            ResolvedExpr::Tuple(items) => self.compile_tuple(items),
            ResolvedExpr::IndependentProduct(items, unwrap) => {
                self.compile_independent_product(items, *unwrap)
            }
            ResolvedExpr::MapLiteral(entries) => self.compile_map(entries),
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

    pub(super) fn compile_ident(&mut self, name: &str) -> Result<(), CompileError> {
        if let Some(&slot) = self.local_slots.get(name) {
            self.emit_op(LOAD_LOCAL);
            self.emit_u8(slot as u8);
        } else if let Some(&idx) = self.global_names().get(name) {
            self.emit_op(LOAD_GLOBAL);
            self.emit_u16(idx);
        } else if let Some(&fn_id) = self.module_scope().get(name) {
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

    /// Emit the bytecode for a binary operator, picking a typed-
    /// specialised opcode when the operand types let us bypass the
    /// runtime tag dispatch. Falls back to the generic opcode when
    /// types aren't known or aren't a typed-opcode-eligible pair.
    fn emit_binop_typed(
        &mut self,
        op: BinOp,
        lhs_ty: Option<&crate::ast::Type>,
        rhs_ty: Option<&crate::ast::Type>,
    ) {
        let both_int = matches!(
            (lhs_ty, rhs_ty),
            (Some(crate::ast::Type::Int), Some(crate::ast::Type::Int))
        );
        let both_float = matches!(
            (lhs_ty, rhs_ty),
            (Some(crate::ast::Type::Float), Some(crate::ast::Type::Float))
        );
        let lt_op = if both_int {
            LT_INT
        } else if both_float {
            LT_FLOAT
        } else {
            LT
        };
        let gt_op = if both_int {
            GT_INT
        } else if both_float {
            GT_FLOAT
        } else {
            GT
        };
        let add_op = if both_int {
            ADD_INT
        } else if both_float {
            ADD_FLOAT
        } else {
            ADD
        };
        let sub_op = if both_int {
            SUB_INT
        } else if both_float {
            SUB_FLOAT
        } else {
            SUB
        };
        let mul_op = if both_int {
            MUL_INT
        } else if both_float {
            MUL_FLOAT
        } else {
            MUL
        };
        // No `DIV_INT`: integer division traps on `b == 0` and the
        // generic `arith_div` already does that branch + propagates a
        // typed runtime error. Float division has well-defined IEEE
        // 754 semantics for `b == 0.0` (inf/-inf/NaN), so we can skip
        // the runtime check entirely with `DIV_FLOAT`.
        let div_op = if both_float { DIV_FLOAT } else { DIV };
        match op {
            BinOp::Add => self.emit_op(add_op),
            BinOp::Sub => self.emit_op(sub_op),
            BinOp::Mul => self.emit_op(mul_op),
            BinOp::Div => self.emit_op(div_op),
            BinOp::Eq => self.emit_op(if both_int { EQ_INT } else { EQ }),
            BinOp::Lt => self.emit_op(lt_op),
            BinOp::Gt => self.emit_op(gt_op),
            BinOp::Neq => {
                self.emit_op(if both_int { EQ_INT } else { EQ });
                self.emit_op(NOT);
            }
            BinOp::Lte => {
                self.emit_op(gt_op);
                self.emit_op(NOT);
            }
            BinOp::Gte => {
                self.emit_op(lt_op);
                self.emit_op(NOT);
            }
        }
    }

    fn compile_error_prop(&mut self, inner: &Spanned<ResolvedExpr>) -> Result<(), CompileError> {
        self.compile_expr(inner)?;
        self.emit_op(PROPAGATE_ERR);
        Ok(())
    }

    fn compile_list(&mut self, items: &[Spanned<ResolvedExpr>]) -> Result<(), CompileError> {
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

    fn compile_interpolated_str(&mut self, parts: &[ResolvedStrPart]) -> Result<(), CompileError> {
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
                ResolvedStrPart::Literal(s) => {
                    let nv = NanValue::new_string_value(s, self.arena);
                    let cidx = self.add_constant(nv);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(cidx);
                }
                ResolvedStrPart::Parsed(expr) => {
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
        _type_id: Option<crate::ir::identity::TypeId>,
        type_name: &str,
        fields: &[(String, Spanned<ResolvedExpr>)],
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
        _type_id: Option<crate::ir::identity::TypeId>,
        type_name: &str,
        base: &Spanned<ResolvedExpr>,
        updates: &[(String, Spanned<ResolvedExpr>)],
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

    fn compile_tuple(&mut self, items: &[Spanned<ResolvedExpr>]) -> Result<(), CompileError> {
        for item in items {
            self.compile_expr(item)?;
        }
        self.emit_op(TUPLE_NEW);
        self.emit_u8(items.len() as u8);
        Ok(())
    }

    fn compile_independent_product(
        &mut self,
        items: &[Spanned<ResolvedExpr>],
        unwrap: bool,
    ) -> Result<(), CompileError> {
        // Push a callable value plus its args for each branch, then emit CALL_PAR
        // with per-branch arities. Runtime dispatch resolves the callable the same
        // way as CALL_VALUE, so aliases like `f = foo; (f(1), f(2))!` work in VM.
        let mut arg_counts: Vec<u8> = Vec::with_capacity(items.len());

        for item in items {
            // Unwrap ErrorProp wrapper if present (from ?! surface syntax)
            let call_expr = match &item.node {
                ResolvedExpr::ErrorProp(inner) => &inner.node,
                other => other,
            };
            match call_expr {
                ResolvedExpr::Call(callee, args) => {
                    self.compile_callee_as_value(callee)?;
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
        entries: &[(Spanned<ResolvedExpr>, Spanned<ResolvedExpr>)],
    ) -> Result<(), CompileError> {
        let empty_map = self.arena.push_map(crate::nan_value::PersistentMap::new());
        let nv = NanValue::new_map(empty_map);
        let idx = self.add_constant(nv);
        self.emit_op(LOAD_CONST);
        self.emit_u16(idx);

        for (key, value) in entries {
            self.compile_expr(key)?;
            self.compile_expr(value)?;
            let symbol_id = self.symbols.intern_builtin(VmBuiltin::MapSet)?;
            self.emit_op(CALL_BUILTIN);
            self.emit_u32(symbol_id);
            self.emit_u8(3);
        }
        Ok(())
    }
}
