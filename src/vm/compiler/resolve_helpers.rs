//! Small resolution / canonicalisation helpers shared by the VM MIR
//! walker. These outlived the HIR call/expression walker (removed when
//! the VM went MIR-only): the MIR walker reuses them to recover
//! canonical fn / type names, resolve ids by name, materialise literal
//! `NanValue`s, and map buffer intrinsics to opcodes.

use crate::ast::Literal;
use crate::ir::hir::BuiltinIntrinsic;
use crate::ir::identity::{FnId, FnKey};
use crate::nan_value::NanValue;
use crate::vm::opcode::{
    BUFFER_APPEND_SEP_UNLESS_FIRST, BUFFER_APPEND_STR, BUFFER_FINALIZE, BUFFER_NEW,
};

use super::{CompileError, FnCompiler};

/// Map a typed [`BuiltinIntrinsic`] to its VM opcode + expected
/// arity. Returns `None` for `ToStr`, which lowers to a CONCAT-with-
/// empty trick instead of a dedicated opcode. The arity is checked
/// at the callsite so a future intrinsic with a different shape
/// can't accidentally re-use the wrong opcode.
pub(super) fn buffer_intrinsic_opcode(intrinsic: BuiltinIntrinsic) -> Option<(u8, usize)> {
    match intrinsic {
        BuiltinIntrinsic::BufNew => Some((BUFFER_NEW, 1)),
        BuiltinIntrinsic::BufAppend => Some((BUFFER_APPEND_STR, 2)),
        BuiltinIntrinsic::BufAppendSepUnlessFirst => Some((BUFFER_APPEND_SEP_UNLESS_FIRST, 2)),
        BuiltinIntrinsic::BufFinalize => Some((BUFFER_FINALIZE, 1)),
        BuiltinIntrinsic::ToStr => None,
    }
}

impl FnCompiler<'_> {
    pub(super) fn resolve_type_id(&self, name: &str) -> Option<u32> {
        self.arena.find_type_id(name)
    }

    pub(super) fn resolve_fn_id_by_name(&self, name: &str) -> Option<u32> {
        self.module_scope()
            .get(name)
            .copied()
            .or_else(|| self.code_store.find(name))
    }

    /// Look up the canonical source-level name for a resolved fn
    /// identity — the dotted `Module.fn` shape the resolver's symbol
    /// table records.
    pub(super) fn canonical_fn_name(&self, fn_id: FnId) -> Result<String, CompileError> {
        let entry = self.symbol_table.fn_entry(fn_id);
        Ok(canonical_name_from_key(&entry.key))
    }

    /// Look up the canonical source-level name for a user-defined
    /// type identity (record name or sum-type name). Used by ctor
    /// emission and pattern matching to recover the qualified
    /// `Module.Type` form the arena was registered with.
    pub(super) fn canonical_type_name(
        &self,
        type_id: crate::ir::identity::TypeId,
    ) -> Result<String, CompileError> {
        let entry = self.symbol_table.type_entry(type_id);
        let key = &entry.key;
        Ok(match key.scope_str() {
            Some(scope) => format!("{}.{}", scope, key.name),
            None => key.name.clone(),
        })
    }

    pub(super) fn nan_literal(&mut self, lit: &Literal) -> NanValue {
        match lit {
            Literal::Int(i) => NanValue::new_int(*i, self.arena),
            Literal::Float(f) => NanValue::new_float(*f),
            Literal::Bool(true) => NanValue::TRUE,
            Literal::Bool(false) => NanValue::FALSE,
            Literal::Unit => NanValue::UNIT,
            Literal::Str(s) => NanValue::new_string_value(s, self.arena),
        }
    }
}

fn canonical_name_from_key(key: &FnKey) -> String {
    key.canonical()
}
