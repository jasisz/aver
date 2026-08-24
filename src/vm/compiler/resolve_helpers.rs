//! Small resolution / canonicalisation helpers shared by the VM MIR
//! walker. These outlived the HIR call/expression walker (removed when
//! the VM went MIR-only): the MIR walker reuses them to recover
//! canonical fn / type names, resolve ids by name, materialise literal
//! `NanValue`s, and map buffer intrinsics to opcodes.

use crate::ast::Literal;
use crate::ir::hir::BuiltinIntrinsic;
use crate::ir::identity::{FnId, FnKey};
use crate::nan_value::{NanIntExt, NanValue};
use crate::vm::opcode::{
    BITS_LOW, BITS_SHIFT_LEFT, BITS_SHIFT_RIGHT, BRANCH_PATH_CHILD_LITERAL,
    BRANCH_PATH_PARSE_LITERAL, BUFFER_APPEND_SEP_UNLESS_FIRST, BUFFER_APPEND_STR, BUFFER_FINALIZE,
    BUFFER_NEW, BYTE_BUILDER_FINALIZE, BYTE_BUILDER_NEW, BYTE_BUILDER_PUSH, INT_DIV_EUCLID,
    INT_MOD_EUCLID, LIST_BUILDER_FINALIZE, LIST_BUILDER_NEW, LIST_BUILDER_PUSH, STR_CODE1,
    STR_CODE1_LOWER, STR_CODE1_UPPER, STR_CURSOR_CODE, STR_CURSOR_END, STR_CURSOR_HEAD,
    STR_CURSOR_NEXT, STR_FOLD_LOWER, STR_FOLD_UPPER, STR_INDEX_BUILD, STR_INDEX_CHAR_AT,
    STR_INDEX_SLICE, VECTOR_NEW_LITERAL,
};
use std::str::FromStr;

use super::{CompileError, FnCompiler};

/// Map a typed [`BuiltinIntrinsic`] to its VM opcode + expected
/// arity. Returns `None` for `ToStr`, which lowers to a CONCAT-with-
/// empty trick instead of a dedicated opcode. The arity is checked
/// at the callsite so a future intrinsic with a different shape
/// can't accidentally re-use the wrong opcode.
///
/// Covers the deforestation buffer ops *and* the const-fold Euclidean
/// div/mod intrinsics (`__int_div_euclid` / `__int_mod_euclid`), both
/// 2-arg → push the unchecked Euclidean result.
pub(super) fn buffer_intrinsic_opcode(intrinsic: BuiltinIntrinsic) -> Option<(u8, usize)> {
    match intrinsic {
        BuiltinIntrinsic::BufNew => Some((BUFFER_NEW, 1)),
        BuiltinIntrinsic::BufAppend => Some((BUFFER_APPEND_STR, 2)),
        BuiltinIntrinsic::BufAppendSepUnlessFirst => Some((BUFFER_APPEND_SEP_UNLESS_FIRST, 2)),
        BuiltinIntrinsic::BufFinalize => Some((BUFFER_FINALIZE, 1)),
        BuiltinIntrinsic::IntDivEuclid => Some((INT_DIV_EUCLID, 2)),
        BuiltinIntrinsic::IntModEuclid => Some((INT_MOD_EUCLID, 2)),
        BuiltinIntrinsic::BitsShiftLeft => Some((BITS_SHIFT_LEFT, 2)),
        BuiltinIntrinsic::BitsShiftRight => Some((BITS_SHIFT_RIGHT, 2)),
        BuiltinIntrinsic::BitsLow => Some((BITS_LOW, 2)),
        BuiltinIntrinsic::VectorNew => Some((VECTOR_NEW_LITERAL, 2)),
        BuiltinIntrinsic::BranchPathChild => Some((BRANCH_PATH_CHILD_LITERAL, 2)),
        BuiltinIntrinsic::BranchPathParse => Some((BRANCH_PATH_PARSE_LITERAL, 1)),
        BuiltinIntrinsic::StrCursorEnd => Some((STR_CURSOR_END, 2)),
        BuiltinIntrinsic::StrCursorHead => Some((STR_CURSOR_HEAD, 2)),
        BuiltinIntrinsic::StrCursorNext => Some((STR_CURSOR_NEXT, 2)),
        BuiltinIntrinsic::StrCode1 => Some((STR_CODE1, 1)),
        BuiltinIntrinsic::StrCode1Lower => Some((STR_CODE1_LOWER, 1)),
        BuiltinIntrinsic::StrCode1Upper => Some((STR_CODE1_UPPER, 1)),
        BuiltinIntrinsic::StrCursorCode => Some((STR_CURSOR_CODE, 2)),
        BuiltinIntrinsic::StrFoldLower => Some((STR_FOLD_LOWER, 1)),
        BuiltinIntrinsic::StrFoldUpper => Some((STR_FOLD_UPPER, 1)),
        BuiltinIntrinsic::StrIndexBuild => Some((STR_INDEX_BUILD, 1)),
        BuiltinIntrinsic::StrIndexCharAt => Some((STR_INDEX_CHAR_AT, 3)),
        BuiltinIntrinsic::StrIndexSlice => Some((STR_INDEX_SLICE, 4)),
        BuiltinIntrinsic::LstNew => Some((LIST_BUILDER_NEW, 1)),
        BuiltinIntrinsic::LstPush => Some((LIST_BUILDER_PUSH, 2)),
        BuiltinIntrinsic::LstFinalize => Some((LIST_BUILDER_FINALIZE, 1)),
        BuiltinIntrinsic::BytNew => Some((BYTE_BUILDER_NEW, 1)),
        BuiltinIntrinsic::BytPush => Some((BYTE_BUILDER_PUSH, 2)),
        BuiltinIntrinsic::BytFinalize => Some((BYTE_BUILDER_FINALIZE, 1)),
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
            Literal::BigInt(s) => NanValue::from_aver_int(
                aver_rt::AverInt::from_str(s).expect("lexer-validated big integer literal"),
                self.arena,
            ),
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
