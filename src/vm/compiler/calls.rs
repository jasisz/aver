use super::{CallTarget, CompileError, FnCompiler};
use crate::ast::Expr;
use crate::nan_value::NanValue;
use crate::vm::opcode::*;

impl<'a> FnCompiler<'a> {
    /// Resolve a dotted path (Ns, member) to what it means.
    fn resolve_dotted_call(&self, ns: &str, method: &str) -> CallTarget {
        match (ns, method) {
            ("Result", "Ok") => return CallTarget::Wrapper(0),
            ("Result", "Err") => return CallTarget::Wrapper(1),
            ("Option", "Some") => return CallTarget::Wrapper(2),
            ("Option", "None") => return CallTarget::None_,
            _ => {}
        }
        if let Some(type_id) = self.resolve_type_id(ns)
            && let Some(variant_id) = self.arena.find_variant_id(type_id, method)
        {
            return CallTarget::Variant(type_id, variant_id);
        }
        let qualified = format!("{}.{}", ns, method);
        if let Some(fn_id) = self.code_store.find(&qualified) {
            return CallTarget::KnownFn(fn_id);
        }
        CallTarget::Builtin(qualified)
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
            CallTarget::Builtin(qualified) => {
                for arg in args {
                    self.compile_expr(arg)?;
                }
                match qualified.as_str() {
                    "List.len" => self.emit_op(LIST_LEN),
                    "List.get" => self.emit_op(LIST_GET),
                    "List.append" => self.emit_op(LIST_APPEND),
                    "List.prepend" => self.emit_op(LIST_PREPEND),
                    _ => {
                        self.emit_op(CALL_BUILTIN);
                        let name_idx = self.arena.push_string(&qualified);
                        self.emit_u16(name_idx as u16);
                        self.emit_u8(args.len() as u8);
                    }
                }
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
        if let Some(ns) = self.flatten_path(obj)
            && ns.chars().next().is_some_and(|c| c.is_uppercase())
        {
            let qualified = format!("{}.{}", ns, field);
            match self.resolve_dotted_call(&ns, field) {
                CallTarget::KnownFn(_) => {
                    if let Some(&idx) = self.global_names.get(&qualified) {
                        self.emit_op(LOAD_GLOBAL);
                        self.emit_u16(idx);
                        return Ok(());
                    }
                }
                CallTarget::None_ => {
                    let idx = self.add_constant(NanValue::NONE);
                    self.emit_op(LOAD_CONST);
                    self.emit_u16(idx);
                    return Ok(());
                }
                CallTarget::Variant(type_id, variant_id) => {
                    self.emit_op(VARIANT_NEW);
                    self.emit_u16(type_id as u16);
                    self.emit_u16(variant_id);
                    self.emit_u8(0);
                    return Ok(());
                }
                CallTarget::Builtin(_) => {
                    return Err(CompileError {
                        msg: format!(
                            "standalone builtin function values are not yet supported in VM: {}",
                            qualified
                        ),
                    });
                }
                _ => {}
            }
        }

        self.compile_expr(obj)?;
        let name_idx = self.arena.push_string(field);
        let nv = NanValue::new_string(name_idx);
        let const_idx = self.add_constant(nv);
        self.emit_op(RECORD_GET_NAMED);
        self.emit_u16(const_idx);
        Ok(())
    }
}
