/// Whether a representation plan has a rendered proof face. `IntCarrier`
/// results are admitted ONLY through the exact straight-line integer face
/// (`add(param0, box(k))`): a bare Int passthrough or any other
/// carrier-returning shape has no coherent face yet (its `Cod` would read as
/// `Int` while `codRepr` falls to the verbatim `WVal` relation), so it must
/// never be selected here nor accepted by the verifier.
fn expr_fragment_plan_has_face(plan: &ExprFragmentPlan) -> bool {
    plan.result != FragTy::IntCarrier || expr_fragment_int_add_face(plan).is_some()
}

pub(crate) fn fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
) -> Option<FragmentPlan> {
    // Encodability gate at MIR time uses a placeholder host table (indices do
    // not affect encoding SHAPE) and requires full canonical BYTE lowering to
    // succeed, because the wasm-gc emitter emits the function body from this
    // plan: a plan that cannot byte-lower must never be selected. The face
    // gate mirrors the verifier: carrier-returning plans without the
    // straight-line integer face stay unplanned (fail-closed decline there,
    // untouched bytes here).
    if let Some(plan) = sym_plan_from_mir_fn(mir_fn)
        && let Some(frag) = plan.to_expr_fragment_plan(&FragHostTable::placeholder())
        && lower_expr_fragment_plan_code_entry_bytes(&frag, 0).is_ok()
        && expr_fragment_plan_has_face(&frag)
    {
        return Some(FragmentPlan::Sym(plan));
    }
    let plan = repr_expr_fragment_plan_from_mir_fn(mir_fn)?;
    if lower_expr_fragment_plan_code_entry_bytes(&plan, 0).is_ok()
        && expr_fragment_plan_has_face(&plan)
    {
        Some(FragmentPlan::Expr(plan))
    } else {
        None
    }
}

#[cfg(test)]
pub(crate) fn expr_fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
) -> Option<ExprFragmentPlan> {
    fragment_plan_from_mir_fn(mir_fn)?.to_expr_fragment_plan(&FragHostTable::placeholder())
}

pub(crate) fn sym_plan_from_mir_fn(mir_fn: &crate::ir::mir::MirFn) -> Option<SymPlan> {
    if !mir_fn.effects.is_empty() {
        return None;
    }

    let mut params = Vec::new();
    let mut params_by_slot = std::collections::HashMap::<u32, (u32, SymTy)>::new();
    for (idx, param) in mir_fn.params.iter().enumerate() {
        if param.local.0 != idx as u32 {
            return None;
        }
        let ty = sym_ty_from_mir_name(&param.ty)?;
        params.push(ty.clone());
        params_by_slot.insert(param.local.0, (idx as u32, ty));
    }

    let result = sym_ty_from_mir_name(&mir_fn.return_type)?;
    let mut builder = MirSymPlanBuilder {
        params_by_slot: &params_by_slot,
        nodes: Vec::new(),
    };
    let (root, root_ty) = builder.lower_expr(&mir_fn.body)?;
    if root_ty != result {
        return None;
    }
    Some(SymPlan {
        params,
        result,
        body: builder.finish(root)?,
    })
}

fn repr_expr_fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
) -> Option<ExprFragmentPlan> {
    if !mir_fn.effects.is_empty() {
        return None;
    }

    let mut params = Vec::new();
    let mut params_by_slot = std::collections::HashMap::<u32, (u32, FragTy)>::new();
    for (idx, param) in mir_fn.params.iter().enumerate() {
        if param.local.0 != idx as u32 {
            return None;
        }
        let ty = expr_fragment_ty_from_mir_name(&param.ty)?;
        params.push(ty);
        params_by_slot.insert(param.local.0, (idx as u32, ty));
    }

    let result = expr_fragment_ty_from_mir_name(&mir_fn.return_type)?;
    let mut builder = MirExprFragmentBuilder {
        params_by_slot: &params_by_slot,
        nodes: Vec::new(),
    };
    let (root, root_ty) = builder.lower_expr(&mir_fn.body)?;
    if root_ty != result {
        return None;
    }
    Some(ExprFragmentPlan {
        params,
        result,
        body: builder.finish(root)?,
    })
}

fn sym_ty_from_mir_name(ty: &str) -> Option<SymTy> {
    let ty = ty.trim();
    match ty {
        "Int" => Some(SymTy::Int),
        "Float" => Some(SymTy::Float),
        "Bool" => Some(SymTy::Bool),
        "String" => Some(SymTy::String),
        "" => None,
        other => Some(SymTy::Named(other.to_string())),
    }
}

fn expr_fragment_ty_from_mir_name(ty: &str) -> Option<FragTy> {
    match ty.trim() {
        "Float" => Some(FragTy::F64),
        "Bool" => Some(FragTy::BoolI32),
        "Int" => Some(FragTy::IntCarrier),
        _ => None,
    }
}

struct MirSymPlanBuilder<'a> {
    params_by_slot: &'a std::collections::HashMap<u32, (u32, SymTy)>,
    nodes: Vec<SymNode>,
}

impl MirSymPlanBuilder<'_> {
    fn lower_expr(
        &mut self,
        expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
    ) -> Option<(SymValueId, SymTy)> {
        match &expr.node {
            crate::ir::mir::MirExpr::Literal(lit) => match &lit.node {
                crate::ast::Literal::Bool(value) => {
                    self.push_node(SymTy::Bool, SymNodeKind::ConstBool(*value))
                }
                crate::ast::Literal::Float(value) => {
                    self.push_node(SymTy::Float, SymNodeKind::ConstFloatBits(value.to_bits()))
                }
                crate::ast::Literal::Str(value) => {
                    self.push_node(SymTy::String, SymNodeKind::ConstStringBytes(value.as_bytes().to_vec()))
                }
                _ => None,
            },
            crate::ir::mir::MirExpr::Local(local) => {
                let (index, ty) = self.params_by_slot.get(&local.node.slot.0)?.clone();
                self.push_node(ty, SymNodeKind::Param { index })
            }
            crate::ir::mir::MirExpr::BinOp(binop) => self.lower_binop(&binop.node),
            crate::ir::mir::MirExpr::IfThenElse(ite) => self.lower_if(&ite.node),
            _ => None,
        }
    }

    fn lower_binop(&mut self, binop: &crate::ir::mir::MirBinOp) -> Option<(SymValueId, SymTy)> {
        // Narrow straight-line integer face: exactly `param + k` over the
        // single Int parameter (const on the right, matching the emitted byte
        // shape). Anything broader stays unplanned so the emitted bytes are
        // untouched and legacy classification is unaffected.
        if binop.op == crate::ast::BinOp::Add
            && self.params_by_slot.len() == 1
            && self.expr_is_int_param(&binop.lhs)
            && let Some(k) = mir_int_literal(&binop.rhs)
        {
            let (lhs, lhs_ty) = self.lower_expr(&binop.lhs)?;
            if lhs_ty != SymTy::Int {
                return None;
            }
            let (rhs, _) = self.push_node(SymTy::Int, SymNodeKind::ConstInt(k))?;
            return self.push_node(
                SymTy::Int,
                SymNodeKind::Prim {
                    op: SymPrim::IntAdd,
                    args: vec![lhs, rhs],
                },
            );
        }

        if let Some((operand, op, k, const_on_left)) = self.int_const_cmp_shape(binop) {
            return self.lower_int_const_cmp(operand, op, k, const_on_left);
        }

        let (lhs, lhs_ty) = self.lower_expr(&binop.lhs)?;
        let (rhs, rhs_ty) = self.lower_expr(&binop.rhs)?;
        if lhs_ty == SymTy::String && rhs_ty == SymTy::String {
            if binop.op != crate::ast::BinOp::Add {
                return None;
            }
            return self.push_node(
                SymTy::String,
                SymNodeKind::Prim {
                    op: SymPrim::StringConcat,
                    args: vec![lhs, rhs],
                },
            );
        }

        if lhs_ty != SymTy::Float || rhs_ty != SymTy::Float {
            return None;
        }
        let (op, result_ty) = match binop.op {
            crate::ast::BinOp::Add => (SymPrim::FloatAdd, SymTy::Float),
            crate::ast::BinOp::Mul => (SymPrim::FloatMul, SymTy::Float),
            crate::ast::BinOp::Lte => (SymPrim::FloatLe, SymTy::Bool),
            _ => return None,
        };
        self.push_node(
            result_ty,
            SymNodeKind::Prim {
                op,
                args: vec![lhs, rhs],
            },
        )
    }

    fn int_const_cmp_shape<'a>(
        &self,
        binop: &'a crate::ir::mir::MirBinOp,
    ) -> Option<(&'a crate::ast::Spanned<crate::ir::mir::MirExpr>, crate::ast::BinOp, i64, bool)>
    {
        if let Some(k) = mir_int_literal(&binop.rhs)
            && self.expr_is_int_param(&binop.lhs)
        {
            return Some((&binop.lhs, binop.op, k, false));
        }
        if let Some(k) = mir_int_literal(&binop.lhs)
            && self.expr_is_int_param(&binop.rhs)
        {
            return Some((&binop.rhs, binop.op, k, true));
        }
        None
    }

    fn expr_is_int_param(&self, expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>) -> bool {
        match &expr.node {
            crate::ir::mir::MirExpr::Local(local) => self
                .params_by_slot
                .get(&local.node.slot.0)
                .is_some_and(|(_, ty)| ty == &SymTy::Int),
            _ => false,
        }
    }

    fn lower_int_const_cmp(
        &mut self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        op: crate::ast::BinOp,
        k: i64,
        const_on_left: bool,
    ) -> Option<(SymValueId, SymTy)> {
        let eff = if const_on_left { flip_cmp(op) } else { op };
        let op = sym_int_const_cmp_op(eff)?;
        let (value, value_ty) = self.lower_expr(operand)?;
        if value_ty != SymTy::Int {
            return None;
        }
        self.push_node(
            SymTy::Bool,
            SymNodeKind::IntConstCmp {
                op,
                value,
                constant: k,
            },
        )
    }

    fn lower_if(&mut self, ite: &crate::ir::mir::MirIfThenElse) -> Option<(SymValueId, SymTy)> {
        let (cond, cond_ty) = self.lower_expr(&ite.cond)?;
        if cond_ty != SymTy::Bool {
            return None;
        }

        let mut then_builder = MirSymPlanBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (then_root, then_ty) = then_builder.lower_expr(&ite.then_branch)?;
        let then_block = then_builder.finish(then_root)?;

        let mut else_builder = MirSymPlanBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (else_root, else_ty) = else_builder.lower_expr(&ite.else_branch)?;
        let else_block = else_builder.finish(else_root)?;

        if then_ty != else_ty || then_block.result_ty() != else_block.result_ty() {
            return None;
        }

        self.push_node(
            then_ty,
            SymNodeKind::If {
                cond,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        )
    }

    fn push_node(&mut self, ty: SymTy, kind: SymNodeKind) -> Option<(SymValueId, SymTy)> {
        let id = SymValueId(self.nodes.len());
        self.nodes.push(SymNode {
            id,
            ty: ty.clone(),
            kind,
        });
        Some((id, ty))
    }

    fn finish(self, result: SymValueId) -> Option<SymBlock> {
        let block = SymBlock {
            nodes: self.nodes,
            result,
        };
        block.result_ty()?;
        Some(block)
    }
}

struct MirExprFragmentBuilder<'a> {
    params_by_slot: &'a std::collections::HashMap<u32, (u32, FragTy)>,
    nodes: Vec<FragNode>,
}

impl MirExprFragmentBuilder<'_> {
    fn lower_expr(
        &mut self,
        expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
    ) -> Option<(FragValueId, FragTy)> {
        match &expr.node {
            crate::ir::mir::MirExpr::Literal(lit) => match &lit.node {
                crate::ast::Literal::Bool(value) => {
                    self.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(*value))
                }
                crate::ast::Literal::Float(value) => {
                    self.push_node(FragTy::F64, FragNodeKind::ConstF64(value.to_bits()))
                }
                _ => None,
            },
            crate::ir::mir::MirExpr::Local(local) => {
                let (index, ty) = *self.params_by_slot.get(&local.node.slot.0)?;
                self.push_node(ty, FragNodeKind::Local { index })
            }
            crate::ir::mir::MirExpr::BinOp(binop) => self.lower_binop(&binop.node),
            crate::ir::mir::MirExpr::IfThenElse(ite) => self.lower_if(&ite.node),
            _ => None,
        }
    }

    fn lower_binop(&mut self, binop: &crate::ir::mir::MirBinOp) -> Option<(FragValueId, FragTy)> {
        if let Some(cmp) = self.int_const_cmp_shape(binop) {
            return self.lower_int_const_cmp(cmp.0, cmp.1, cmp.2, cmp.3);
        }

        let (lhs, lhs_ty) = self.lower_expr(&binop.lhs)?;
        let (rhs, rhs_ty) = self.lower_expr(&binop.rhs)?;
        if lhs_ty != FragTy::F64 || rhs_ty != FragTy::F64 {
            return None;
        }
        let (op, result_ty) = match binop.op {
            crate::ast::BinOp::Add => (FragPrim::F64Add, FragTy::F64),
            crate::ast::BinOp::Mul => (FragPrim::F64Mul, FragTy::F64),
            crate::ast::BinOp::Lte => (FragPrim::F64Le, FragTy::BoolI32),
            _ => return None,
        };
        self.push_node(
            result_ty,
            FragNodeKind::Prim {
                op,
                args: vec![lhs, rhs],
            },
        )
    }

    fn int_const_cmp_shape<'a>(
        &self,
        binop: &'a crate::ir::mir::MirBinOp,
    ) -> Option<(&'a crate::ast::Spanned<crate::ir::mir::MirExpr>, crate::ast::BinOp, i64, bool)>
    {
        if let Some(k) = mir_int_literal(&binop.rhs)
            && self.expr_is_int_param(&binop.lhs)
        {
            return Some((&binop.lhs, binop.op, k, false));
        }
        if let Some(k) = mir_int_literal(&binop.lhs)
            && self.expr_is_int_param(&binop.rhs)
        {
            return Some((&binop.rhs, binop.op, k, true));
        }
        None
    }

    fn expr_is_int_param(&self, expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>) -> bool {
        match &expr.node {
            crate::ir::mir::MirExpr::Local(local) => self
                .params_by_slot
                .get(&local.node.slot.0)
                .is_some_and(|(_, ty)| *ty == FragTy::IntCarrier),
            _ => false,
        }
    }

    fn lower_int_const_cmp(
        &mut self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        op: crate::ast::BinOp,
        k: i64,
        const_on_left: bool,
    ) -> Option<(FragValueId, FragTy)> {
        let eff = if const_on_left { flip_cmp(op) } else { op };
        let small_prim = i64_const_cmp_prim(eff)?;
        let big_kind = big_int_const_cmp_kind(eff)?;

        let (carrier, carrier_ty) = self.lower_expr(operand)?;
        if carrier_ty != FragTy::IntCarrier {
            return None;
        }
        let (magf, _) = self.push_node(
            FragTy::Ref,
            FragNodeKind::StructGet {
                field: 1,
                receiver: carrier,
            },
        )?;
        let (is_small, _) =
            self.push_node(FragTy::BoolI32, FragNodeKind::RefIsNull { value: magf })?;

        let then_block = self.lower_int_small_const_cmp_block(operand, small_prim, k)?;
        let else_block = self.lower_int_big_const_cmp_block(operand, big_kind)?;
        self.push_node(
            FragTy::BoolI32,
            FragNodeKind::If {
                cond: is_small,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        )
    }

    fn lower_int_small_const_cmp_block(
        &self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        op: FragPrim,
        k: i64,
    ) -> Option<FragBlock> {
        let mut block = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (carrier, carrier_ty) = block.lower_expr(operand)?;
        if carrier_ty != FragTy::IntCarrier {
            return None;
        }
        let (small, _) = block.push_node(
            FragTy::I64,
            FragNodeKind::StructGet {
                field: 0,
                receiver: carrier,
            },
        )?;
        let (constant, _) = block.push_node(FragTy::I64, FragNodeKind::ConstI64(k))?;
        let (result, _) = block.push_node(
            FragTy::BoolI32,
            FragNodeKind::Prim {
                op,
                args: vec![small, constant],
            },
        )?;
        block.finish(result)
    }

    fn lower_int_big_const_cmp_block(
        &self,
        operand: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        kind: BigIntConstCmpKind,
    ) -> Option<FragBlock> {
        let mut block = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        match kind {
            BigIntConstCmpKind::Always(value) => {
                let (result, _) =
                    block.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(value))?;
                block.finish(result)
            }
            BigIntConstCmpKind::SignLtZero | BigIntConstCmpKind::SignGtZero => {
                let (carrier, carrier_ty) = block.lower_expr(operand)?;
                if carrier_ty != FragTy::IntCarrier {
                    return None;
                }
                let (sign, _) = block.push_node(
                    FragTy::RawI32,
                    FragNodeKind::StructGet {
                        field: 2,
                        receiver: carrier,
                    },
                )?;
                let (zero, _) =
                    block.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(false))?;
                let op = match kind {
                    BigIntConstCmpKind::SignLtZero => FragPrim::I32LtS,
                    BigIntConstCmpKind::SignGtZero => FragPrim::I32GtS,
                    BigIntConstCmpKind::Always(_) => unreachable!(),
                };
                let (result, _) = block.push_node(
                    FragTy::BoolI32,
                    FragNodeKind::Prim {
                        op,
                        args: vec![sign, zero],
                    },
                )?;
                block.finish(result)
            }
        }
    }

    fn lower_if(&mut self, ite: &crate::ir::mir::MirIfThenElse) -> Option<(FragValueId, FragTy)> {
        let (cond, cond_ty) = self.lower_expr(&ite.cond)?;
        if cond_ty != FragTy::BoolI32 {
            return None;
        }

        let mut then_builder = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (then_root, then_ty) = then_builder.lower_expr(&ite.then_branch)?;
        let then_block = then_builder.finish(then_root)?;

        let mut else_builder = MirExprFragmentBuilder {
            params_by_slot: self.params_by_slot,
            nodes: Vec::new(),
        };
        let (else_root, else_ty) = else_builder.lower_expr(&ite.else_branch)?;
        let else_block = else_builder.finish(else_root)?;

        if then_ty != else_ty || then_block.result_ty()? != else_block.result_ty()? {
            return None;
        }

        self.push_node(
            then_ty,
            FragNodeKind::If {
                cond,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        )
    }

    fn push_node(&mut self, ty: FragTy, kind: FragNodeKind) -> Option<(FragValueId, FragTy)> {
        let id = FragValueId(self.nodes.len());
        self.nodes.push(FragNode { id, ty, kind });
        Some((id, ty))
    }

    fn finish(self, result: FragValueId) -> Option<FragBlock> {
        self.nodes.get(result.0)?;
        Some(FragBlock {
            nodes: self.nodes,
            result,
        })
    }
}

#[derive(Clone, Copy)]
enum BigIntConstCmpKind {
    Always(bool),
    SignLtZero,
    SignGtZero,
}

fn mir_int_literal(expr: &crate::ast::Spanned<crate::ir::mir::MirExpr>) -> Option<i64> {
    match &expr.node {
        crate::ir::mir::MirExpr::Literal(lit) => match lit.node {
            crate::ast::Literal::Int(k) => Some(k),
            _ => None,
        },
        _ => None,
    }
}

fn flip_cmp(op: crate::ast::BinOp) -> crate::ast::BinOp {
    match op {
        crate::ast::BinOp::Lt => crate::ast::BinOp::Gt,
        crate::ast::BinOp::Gt => crate::ast::BinOp::Lt,
        crate::ast::BinOp::Lte => crate::ast::BinOp::Gte,
        crate::ast::BinOp::Gte => crate::ast::BinOp::Lte,
        crate::ast::BinOp::Eq => crate::ast::BinOp::Eq,
        crate::ast::BinOp::Neq => crate::ast::BinOp::Neq,
        other => other,
    }
}

fn i64_const_cmp_prim(op: crate::ast::BinOp) -> Option<FragPrim> {
    match op {
        crate::ast::BinOp::Eq => Some(FragPrim::I64Eq),
        crate::ast::BinOp::Lt => Some(FragPrim::I64LtS),
        crate::ast::BinOp::Lte => Some(FragPrim::I64LeS),
        crate::ast::BinOp::Gte => Some(FragPrim::I64GeS),
        // `expr-fragment-v1` has no `i64.gt_s`/`i64.ne` proof rule yet.
        crate::ast::BinOp::Gt | crate::ast::BinOp::Neq => None,
        _ => None,
    }
}

fn sym_int_const_cmp_op(op: crate::ast::BinOp) -> Option<SymIntCmp> {
    match op {
        crate::ast::BinOp::Eq => Some(SymIntCmp::Eq),
        crate::ast::BinOp::Lt => Some(SymIntCmp::Lt),
        crate::ast::BinOp::Lte => Some(SymIntCmp::Le),
        crate::ast::BinOp::Gte => Some(SymIntCmp::Ge),
        // Kept aligned with the representation encoder: no `Gt`/`Neq`
        // admission until the carrier proof rules grow those branches.
        crate::ast::BinOp::Gt | crate::ast::BinOp::Neq => None,
        _ => None,
    }
}

fn big_int_const_cmp_kind(op: crate::ast::BinOp) -> Option<BigIntConstCmpKind> {
    match op {
        crate::ast::BinOp::Eq => Some(BigIntConstCmpKind::Always(false)),
        crate::ast::BinOp::Lt | crate::ast::BinOp::Lte => Some(BigIntConstCmpKind::SignLtZero),
        crate::ast::BinOp::Gte => Some(BigIntConstCmpKind::SignGtZero),
        // Kept aligned with `i64_const_cmp_prim`: no `Gt`/`Neq` admission yet.
        crate::ast::BinOp::Gt | crate::ast::BinOp::Neq => None,
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinOp, Literal, Spanned};
    use crate::ir::FnId;
    use crate::ir::mir::{
        LocalId, MirBinOp, MirExpr, MirFn, MirFnRepr, MirLocal, MirParam,
    };

    fn span<T>(node: T) -> Spanned<T> {
        Spanned {
            node,
            line: 0,
            ty: std::sync::OnceLock::new(),
        }
    }

    fn int_local(slot: u32) -> Spanned<MirExpr> {
        span(MirExpr::Local(span(MirLocal::at(LocalId(slot)))))
    }

    fn int_lit(value: i64) -> Spanned<MirExpr> {
        span(MirExpr::Literal(span(Literal::Int(value))))
    }

    fn float_local(slot: u32) -> Spanned<MirExpr> {
        span(MirExpr::Local(span(MirLocal::at(LocalId(slot)))))
    }

    fn float_binop_fn(op: BinOp) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "f".to_string(),
            params: vec![
                MirParam {
                    local: LocalId(0),
                    name: "a".to_string(),
                    ty: "Float".to_string(),
                },
                MirParam {
                    local: LocalId(1),
                    name: "b".to_string(),
                    ty: "Float".to_string(),
                },
            ],
            return_type: match op {
                BinOp::Lte => "Bool".to_string(),
                _ => "Float".to_string(),
            },
            effects: vec![],
            body: span(MirExpr::BinOp(span(MirBinOp {
                op,
                lhs: Box::new(float_local(0)),
                rhs: Box::new(float_local(1)),
            }))),
            local_count: 2,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    fn int_predicate_fn(op: BinOp, lhs: Spanned<MirExpr>, rhs: Spanned<MirExpr>) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "p".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "x".to_string(),
                ty: "Int".to_string(),
            }],
            return_type: "Bool".to_string(),
            effects: vec![],
            body: span(MirExpr::BinOp(span(MirBinOp {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            }))),
            local_count: 1,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    fn int_identity_fn() -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "id".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "x".to_string(),
                ty: "Int".to_string(),
            }],
            return_type: "Int".to_string(),
            effects: vec![],
            body: int_local(0),
            local_count: 1,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    #[test]
    fn direct_float_mir_prefers_source_level_sym_plan() {
        let mir_fn = float_binop_fn(BinOp::Add);
        let plan = fragment_plan_from_mir_fn(&mir_fn).expect("plan");
        let FragmentPlan::Sym(sym) = plan else {
            panic!("direct source-level float fragment should use SymPlan")
        };

        assert_eq!(sym.params, vec![SymTy::Float, SymTy::Float]);
        assert_eq!(sym.result, SymTy::Float);
        assert!(matches!(
            sym.body.nodes[2].kind,
            SymNodeKind::Prim {
                op: SymPrim::FloatAdd,
                ..
            }
        ));
    }

    #[test]
    fn direct_int_identity_stays_unplanned_without_a_face() {
        // A bare Int passthrough ENCODES (the sym plan and its representation
        // encoding both exist), but it has no rendered proof face: its `Cod`
        // would read as `Int` while `codRepr` falls to the verbatim `WVal`
        // relation. The producer must therefore never select it — the export
        // keeps its ordinary MIR-emitted body and declines fail-closed at
        // classification (see `idGoal` in the goal-matrix fixture).
        let mir_fn = int_identity_fn();
        let sym = sym_plan_from_mir_fn(&mir_fn).expect("sym plan exists");
        assert_eq!(sym.params, vec![SymTy::Int]);
        assert_eq!(sym.result, SymTy::Int);
        let expr_plan = sym
            .to_expr_fragment_plan(&FragHostTable::placeholder())
            .expect("source int identity encodes to a representation plan");
        assert_eq!(expr_plan.result, FragTy::IntCarrier);
        assert!(
            !expr_fragment_plan_has_face(&expr_plan),
            "carrier identity must not have a rendered proof face"
        );
        assert!(
            fragment_plan_from_mir_fn(&mir_fn).is_none(),
            "producer must not select a carrier-returning plan without the \
             straight-line integer face"
        );
    }

    #[test]
    fn int_carrier_comparison_prefers_source_level_sym_plan() {
        let mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
        let plan = fragment_plan_from_mir_fn(&mir_fn).expect("plan");
        let FragmentPlan::Sym(sym) = plan else {
            panic!("source-level int const comparison should use SymPlan")
        };

        assert_eq!(sym.params, vec![SymTy::Int]);
        assert_eq!(sym.result, SymTy::Bool);
        assert!(matches!(sym.body.nodes[0].kind, SymNodeKind::Param { index: 0 }));
        assert!(matches!(
            sym.body.nodes[1].kind,
            SymNodeKind::IntConstCmp {
                op: SymIntCmp::Lt,
                value: SymValueId(0),
                constant: 0,
            }
        ));
    }

    #[test]
    fn int_param_less_than_literal_lowers_from_mir_to_aint_plan() {
        let mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
        let plan = expr_fragment_plan_from_mir_fn(&mir_fn).expect("plan");

        assert_eq!(plan.params, vec![FragTy::IntCarrier]);
        assert_eq!(plan.result, FragTy::BoolI32);
        assert_eq!(plan.body.result_ty(), Some(FragTy::BoolI32));
        assert_eq!(plan.body.nodes.len(), 4);

        assert!(matches!(plan.body.nodes[0].kind, FragNodeKind::Local { index: 0 }));
        assert!(matches!(
            plan.body.nodes[1].kind,
            FragNodeKind::StructGet {
                field: 1,
                receiver: FragValueId(0)
            }
        ));
        assert!(matches!(
            plan.body.nodes[2].kind,
            FragNodeKind::RefIsNull {
                value: FragValueId(1)
            }
        ));
        let FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } = &plan.body.nodes[3].kind
        else {
            panic!("root must be an if over small-vs-big AverInt carrier")
        };
        assert_eq!(*cond, FragValueId(2));
        assert_eq!(then_block.result_ty(), Some(FragTy::BoolI32));
        assert_eq!(else_block.result_ty(), Some(FragTy::BoolI32));
        assert!(matches!(
            then_block.nodes[2].kind,
            FragNodeKind::ConstI64(0)
        ));
        assert!(matches!(
            then_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I64LtS,
                ..
            }
        ));
        assert!(matches!(
            else_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I32LtS,
                ..
            }
        ));
    }

    #[test]
    fn int_literal_on_left_flips_comparison_before_plan_lowering() {
        let mir_fn = int_predicate_fn(BinOp::Lte, int_lit(0), int_local(0));
        let plan = expr_fragment_plan_from_mir_fn(&mir_fn).expect("plan");
        let FragNodeKind::If {
            then_block,
            else_block,
            ..
        } = &plan.body.nodes[3].kind
        else {
            panic!("root must be an if over small-vs-big AverInt carrier")
        };

        assert!(matches!(
            then_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I64GeS,
                ..
            }
        ));
        assert!(matches!(
            else_block.nodes[3].kind,
            FragNodeKind::Prim {
                op: FragPrim::I32GtS,
                ..
            }
        ));
    }
}
