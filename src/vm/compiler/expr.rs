use super::{CompileError, FnCompiler};
use crate::ast::Literal;
use crate::nan_value::NanValue;
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;

impl<'a> FnCompiler<'a> {
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
}
