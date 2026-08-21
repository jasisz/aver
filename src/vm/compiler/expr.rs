use super::{CompileError, FnCompiler, operand_u8};
use crate::ast::Literal;
use crate::nan_value::{NanIntExt, NanValue};
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;
use aver_rt::AverInt;
use std::str::FromStr;

impl<'a> FnCompiler<'a> {
    pub(super) fn compile_literal(&mut self, lit: &Literal) -> Result<(), CompileError> {
        match lit {
            Literal::Int(i) => {
                let nv = NanValue::new_int(*i, self.arena);
                let idx = self.add_constant(nv);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            Literal::BigInt(s) => {
                // A `>i64` literal: build the exact `AverInt` from its decimal
                // digits (the same string→bignum path `Int.n` uses) and store it
                // as an arena-backed heap constant, never an inline NaN-box. The
                // lexer validated the digits, so `from_str` cannot fail.
                let n = AverInt::from_str(s).expect("lexer-validated big integer literal");
                let nv = NanValue::from_aver_int(n, self.arena);
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

    /// Resolve a fn-referenced-as-a-value name (`MirExpr::FnValue`, its
    /// only caller). `self.local_slots` is EMPTY for resolved fns (see
    /// the field doc): a `FnValue` name survived the resolver, so no
    /// local with that spelling was in scope at the use site and the
    /// honest answers are module fns / globals / builtin symbols. The
    /// local branch fires only on the no-resolution fallback map
    /// (params only), where a bare param read can reach `FnValue`.
    pub(super) fn compile_ident(&mut self, name: &str) -> Result<(), CompileError> {
        if let Some(&slot) = self.local_slots.get(name) {
            self.emit_op(LOAD_LOCAL);
            self.emit_u8(operand_u8(
                slot as usize,
                &format!("function `{}` uses local slot {slot}", self.name()),
            )?);
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
