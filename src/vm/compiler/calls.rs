use super::{CallTarget, CompileError, FnCompiler};
use crate::ast::Expr;
use crate::nan_value::NanValue;
use crate::vm::builtin::VmBuiltin;
use crate::vm::opcode::*;
use crate::vm::symbol::VmSymbolTable;

impl<'a> FnCompiler<'a> {
    /// Resolve a dotted path (Ns, member) to what it means.
    fn resolve_dotted_call(&self, ns: &str, method: &str) -> CallTarget {
        let qualified = format!("{}.{}", ns, method);
        if let Some(symbol_id) = self.symbols.find(&qualified) {
            return self.resolve_symbol_call_target(symbol_id, qualified);
        }

        if let Some(namespace_symbol_id) = self.symbols.resolve_namespace_path(ns)
            && let Some(member_symbol_id) = self.symbols.find(method)
            && let Some(member) = self
                .symbols
                .resolve_member(namespace_symbol_id, member_symbol_id)
        {
            if let Some(symbol_id) = self.symbols.resolve_symbol_ref(member) {
                return self.resolve_symbol_call_target(symbol_id, qualified);
            }
            if member.bits() == NanValue::NONE.bits() {
                return CallTarget::None_;
            }
        }
        CallTarget::UnknownQualified(qualified)
    }

    fn resolve_symbol_call_target(&self, symbol_id: u32, qualified: String) -> CallTarget {
        if let Some(fn_id) = self.symbols.resolve_function(symbol_id) {
            return CallTarget::KnownFn(fn_id);
        }
        if let Some(builtin) = self.symbols.resolve_builtin(symbol_id) {
            return CallTarget::Builtin(builtin);
        }
        if let Some(kind) = self.symbols.resolve_wrapper(symbol_id) {
            return CallTarget::Wrapper(kind);
        }
        if let Some(value) = self.symbols.resolve_constant(symbol_id)
            && value.bits() == NanValue::NONE.bits()
        {
            return CallTarget::None_;
        }
        if let Some(ctor) = self.symbols.resolve_variant_ctor(symbol_id) {
            return CallTarget::Variant(ctor.type_id, ctor.variant_id);
        }
        CallTarget::UnknownQualified(qualified)
    }

    fn extract_dotted_path(&self, expr: &Expr) -> Option<(String, String)> {
        if let Expr::Attr(obj, method) = expr {
            let path = self.flatten_path(obj)?;
            if path.chars().next().is_some_and(|c| c.is_uppercase()) {
                return Some((path, method.clone()));
            }
        }
        Option::None
    }

    fn flatten_path(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::Ident(name) => Some(name.clone()),
            Expr::Attr(inner, field) => Some(format!("{}.{}", self.flatten_path(inner)?, field)),
            _ => Option::None,
        }
    }

    pub(super) fn resolve_type_id(&self, name: &str) -> Option<u32> {
        self.arena.find_type_id(name).or_else(|| {
            name.rsplit('.')
                .next()
                .filter(|short| *short != name)
                .and_then(|short| self.arena.find_type_id(short))
        })
    }

    fn resolve_fn_id(&self, name: &str) -> Option<u32> {
        self.module_scope
            .get(name)
            .copied()
            .or_else(|| self.code_store.find(name))
    }

    pub(super) fn resolve_call_target(&self, expr: &Expr) -> Option<CallTarget> {
        match expr {
            Expr::Ident(name) => self.resolve_fn_id(name).map(CallTarget::KnownFn),
            _ => self
                .extract_dotted_path(expr)
                .map(|(ns, method)| self.resolve_dotted_call(&ns, &method)),
        }
    }

    pub(super) fn compile_call(
        &mut self,
        fn_expr: &Expr,
        args: &[Expr],
    ) -> Result<(), CompileError> {
        if let Some(target) = self.resolve_call_target(fn_expr) {
            return self.compile_resolved_call(target, args);
        }
        self.compile_expr(fn_expr)?;
        for arg in args {
            self.compile_expr(arg)?;
        }
        self.emit_op(CALL_VALUE);
        self.emit_u8(args.len() as u8);
        Ok(())
    }

    fn compile_resolved_call(
        &mut self,
        target: CallTarget,
        args: &[Expr],
    ) -> Result<(), CompileError> {
        match target {
            CallTarget::KnownFn(fn_id) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                self.emit_op(CALL_KNOWN);
                self.emit_u16(fn_id as u16);
                self.emit_u8(args.len() as u8);
            }
            CallTarget::Wrapper(kind) => {
                if let Some(arg) = args.first() {
                    self.compile_expr(arg)?;
                } else {
                    self.emit_op(LOAD_UNIT);
                }
                self.emit_op(WRAP);
                self.emit_u8(kind);
            }
            CallTarget::None_ => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            CallTarget::Variant(type_id, variant_id) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                self.emit_op(VARIANT_NEW);
                self.emit_u16(type_id as u16);
                self.emit_u16(variant_id);
                self.emit_u8(args.len() as u8);
            }
            CallTarget::Builtin(builtin) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                match builtin {
                    VmBuiltin::ListLen => self.emit_op(LIST_LEN),
                    VmBuiltin::ListGet => self.emit_op(LIST_GET),
                    VmBuiltin::ListAppend => self.emit_op(LIST_APPEND),
                    VmBuiltin::ListPrepend => self.emit_op(LIST_PREPEND),
                    _ => {
                        let symbol_id = self.symbols.intern_builtin(builtin);
                        self.emit_op(CALL_BUILTIN);
                        self.emit_u32(symbol_id);
                        self.emit_u8(args.len() as u8);
                    }
                }
            }
            CallTarget::UnknownQualified(qualified) => {
                return Err(CompileError {
                    msg: format!("unknown builtin or namespace member: {}", qualified),
                });
            }
        }
        Ok(())
    }

    pub(super) fn compile_tail_call(
        &mut self,
        target: &str,
        args: &[Expr],
    ) -> Result<(), CompileError> {
        for arg in args {
            self.compile_expr(arg)?;
        }

        if target == self.name {
            self.emit_op(TAIL_CALL_SELF);
            self.emit_u8(args.len() as u8);
        } else if let Some(fn_id) = self.resolve_fn_id(target) {
            self.emit_op(TAIL_CALL_KNOWN);
            self.emit_u16(fn_id as u16);
            self.emit_u8(args.len() as u8);
        } else {
            return Err(CompileError {
                msg: format!("unknown tail call target: {}", target),
            });
        }
        Ok(())
    }

    pub(super) fn compile_constructor(
        &mut self,
        name: &str,
        arg: Option<&Expr>,
    ) -> Result<(), CompileError> {
        match name {
            "Result.Ok" => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(0);
            }
            "Result.Err" => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(1);
            }
            "Option.Some" => {
                self.compile_constructor_arg(arg)?;
                self.emit_op(WRAP);
                self.emit_u8(2);
            }
            "Option.None" => {
                let idx = self.add_constant(NanValue::NONE);
                self.emit_op(LOAD_CONST);
                self.emit_u16(idx);
            }
            _ => {
                if let Some((type_name, variant_name)) = name.rsplit_once('.')
                    && let Some(type_id) = self.resolve_type_id(type_name)
                    && let Some(variant_id) = self.arena.find_variant_id(type_id, variant_name)
                {
                    let field_count = if let Some(a) = arg {
                        self.compile_expr(a)?;
                        1u8
                    } else {
                        0u8
                    };
                    self.emit_op(VARIANT_NEW);
                    self.emit_u16(type_id as u16);
                    self.emit_u16(variant_id);
                    self.emit_u8(field_count);
                    return Ok(());
                }
                return Err(CompileError {
                    msg: format!("unknown constructor: {}", name),
                });
            }
        }
        Ok(())
    }

    fn compile_constructor_arg(&mut self, arg: Option<&Expr>) -> Result<(), CompileError> {
        if let Some(a) = arg {
            self.compile_expr(a)
        } else {
            self.emit_op(LOAD_UNIT);
            Ok(())
        }
    }

    pub(super) fn compile_attr(&mut self, obj: &Expr, field: &str) -> Result<(), CompileError> {
        if let Some(path) = self.flatten_path(obj)
            && let Some(symbol_id) = self.symbols.resolve_namespace_path(&path)
        {
            let idx = self.add_constant(VmSymbolTable::symbol_ref(symbol_id));
            self.emit_op(LOAD_CONST);
            self.emit_u16(idx);
            let field_symbol_id = self.symbols.intern_name(field);
            self.emit_op(RECORD_GET_NAMED);
            self.emit_u32(field_symbol_id);
            return Ok(());
        }

        if let Some(field_idx) = self
            .infer_record_field_idx(obj, field)
            .or_else(|| self.resolve_record_field_idx(obj, field))
        {
            self.compile_expr(obj)?;
            self.emit_op(RECORD_GET);
            self.emit_u8(field_idx);
            return Ok(());
        }

        self.compile_expr(obj)?;
        let field_symbol_id = self.symbols.intern_name(field);
        self.emit_op(RECORD_GET_NAMED);
        self.emit_u32(field_symbol_id);
        Ok(())
    }

    fn infer_record_field_idx(&self, obj: &Expr, field: &str) -> Option<u8> {
        let type_name = match obj {
            Expr::RecordCreate { type_name, .. } | Expr::RecordUpdate { type_name, .. } => {
                type_name.as_str()
            }
            _ => return None,
        };
        let type_id = self.resolve_type_id(type_name)?;
        let fields = self.arena.get_field_names(type_id);
        fields
            .iter()
            .position(|name| name == field)
            .map(|idx| idx as u8)
    }

    fn resolve_record_field_idx(&self, obj: &Expr, field: &str) -> Option<u8> {
        let field_symbol_id = self.code_store.symbols.find(field)?;
        match obj {
            Expr::Ident(type_name)
                if type_name.chars().next().is_some_and(|c| c.is_uppercase()) =>
            {
                let type_id = self.resolve_type_id(type_name)?;
                self.code_store
                    .record_field_slots
                    .get(&(type_id, field_symbol_id))
                    .copied()
            }
            _ => None,
        }
    }
}
