/// Whether a representation plan has a rendered proof face. `IntCarrier`
/// results are admitted ONLY through the exact straight-line integer face
/// (`add(param0, box(k))`): a bare Int passthrough or any other
/// carrier-returning shape has no coherent face yet (its `Cod` would read as
/// `Int` while `codRepr` falls to the verbatim `WVal` relation), so it must
/// never be selected here nor accepted by the verifier. Likewise, `AdtRef`
/// values are admitted ONLY through the exact field-projection face: any other
/// ADT-ref plan (a bare reference passthrough, a projection chain) stays
/// unplanned here and declines fail-closed at the verifier.
fn expr_fragment_plan_has_face(plan: &ExprFragmentPlan) -> bool {
    let tag_dispatch = expr_fragment_is_tag_dispatch(plan);
    let int_face_ok = plan.result != FragTy::IntCarrier
        || expr_fragment_int_add_face(plan).is_some()
        || tag_dispatch;
    let adt_face_ok = !expr_fragment_plan_touches_adt_ref(plan)
        || expr_fragment_project_face(plan).is_some()
        || tag_dispatch;
    int_face_ok && adt_face_ok
}

/// Producer-side record layout resolver: `(record type name, field name) ->
/// (declared field index, field source type name)`. Backed by the wasm-gc
/// emitter's type registry; returns `None` for newtypes/carriers (whose field
/// reads are identity, not `struct.get`), so those never plan a projection.
pub(crate) type RecordFieldLookup<'a> = &'a dyn Fn(&str, &str) -> Option<(u32, String)>;

pub(crate) fn fragment_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
    record_fields: RecordFieldLookup,
) -> Option<FragmentPlan> {
    // Encodability gate at MIR time uses placeholder host/struct tables
    // (indices do not affect encoding SHAPE) and requires full canonical BYTE
    // lowering to succeed, because the wasm-gc emitter emits the function body
    // from this plan: a plan that cannot byte-lower must never be selected.
    // The face gate mirrors the verifier: carrier-returning plans without the
    // straight-line integer face, and ADT-ref plans without the
    // field-projection face, stay unplanned (fail-closed decline there,
    // untouched bytes here).
    if let Some(plan) = sym_plan_from_mir_fn(mir_fn, record_fields)
        && let Some(frag) = plan.to_expr_fragment_plan(
            &FragHostTable::placeholder(),
            &FragStructTable::placeholder_for(&plan),
        )
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
    let plan = fragment_plan_from_mir_fn(mir_fn, &|_, _| None)?;
    let struct_table = match &plan {
        FragmentPlan::Sym(sym) => FragStructTable::placeholder_for(sym),
        FragmentPlan::Expr(_) => FragStructTable::default(),
    };
    plan.to_expr_fragment_plan(&FragHostTable::placeholder(), &struct_table)
}

pub(crate) fn sym_plan_from_mir_fn(
    mir_fn: &crate::ir::mir::MirFn,
    record_fields: RecordFieldLookup,
) -> Option<SymPlan> {
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
        record_fields,
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
        // MIR carries the resolver type's Debug rendering: the source String
        // type prints as `Str`.
        "String" | "Str" => Some(SymTy::String),
        "" => None,
        other => {
            if let Some(inner) = other
                .strip_prefix("Option(")
                .and_then(|value| value.strip_suffix(')'))
            {
                return Some(SymTy::App(
                    "Option".to_string(),
                    vec![sym_ty_from_mir_name(inner)?],
                ));
            }
            if let Some(inner) = other
                .strip_prefix("Result(")
                .and_then(|value| value.strip_suffix(')'))
            {
                let (ok, err) = split_mir_debug_binary_args(inner)?;
                return Some(SymTy::App(
                    "Result".to_string(),
                    vec![sym_ty_from_mir_name(ok)?, sym_ty_from_mir_name(err)?],
                ));
            }
            // User records/ADTs print as `Named { id: ..., name: "User" }`;
            // extract the source name. Anything else keeps the pre-existing
            // whole-token fallback.
            if let Some(name) = mir_named_type_name(other) {
                return Some(SymTy::Named(name));
            }
            Some(SymTy::Named(other.to_string()))
        }
    }
}

fn split_mir_debug_binary_args(value: &str) -> Option<(&str, &str)> {
    let mut depth = 0i32;
    for (at, ch) in value.char_indices() {
        match ch {
            '(' | '<' | '[' | '{' => depth += 1,
            ')' | '>' | ']' | '}' => depth -= 1,
            ',' if depth == 0 => {
                return Some((value[..at].trim(), value[at + 1..].trim()));
            }
            _ => {}
        }
    }
    None
}

/// Extract the source type name from the resolver `Type::Named` Debug
/// rendering (`Named { id: ..., name: "User" }`). Returns `None` for any
/// other shape or a non-canonical name token.
fn mir_named_type_name(ty: &str) -> Option<String> {
    let rest = ty.strip_prefix("Named {")?.strip_suffix('}')?;
    let idx = rest.find("name: \"")?;
    let after = &rest[idx + 7..];
    let name = &after[..after.find('"')?];
    if !name.is_empty() && !name.chars().any(char::is_whitespace) && !name.contains('=') {
        Some(name.to_string())
    } else {
        None
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
    record_fields: RecordFieldLookup<'a>,
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
            crate::ir::mir::MirExpr::Project(spanned_proj) => {
                // Record field access `base.field`: only a named-record base
                // resolvable through the emitter's registry plans a
                // projection; newtypes/carriers (identity reads) and unknown
                // layouts return `None` from the lookup and stay unplanned.
                let proj = &spanned_proj.node;
                let (value, base_ty) = self.lower_expr(&proj.base)?;
                let SymTy::Named(type_name) = base_ty else {
                    return None;
                };
                let (field, field_ty_name) = (self.record_fields)(&type_name, &proj.field)?;
                let field_ty = sym_ty_from_mir_name(&field_ty_name)?;
                self.push_node(
                    field_ty.clone(),
                    SymNodeKind::ProjectField {
                        type_name,
                        field,
                        field_ty,
                        value,
                    },
                )
            }
            crate::ir::mir::MirExpr::IfThenElse(ite) => self.lower_if(&ite.node),
            crate::ir::mir::MirExpr::Match(m) => self.lower_tag_match(&m.node),
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
            record_fields: self.record_fields,
            nodes: Vec::new(),
        };
        let (then_root, then_ty) = then_builder.lower_expr(&ite.then_branch)?;
        let then_block = then_builder.finish(then_root)?;

        let mut else_builder = MirSymPlanBuilder {
            params_by_slot: self.params_by_slot,
            record_fields: self.record_fields,
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

    fn lower_tag_match(
        &mut self,
        m: &crate::ir::mir::MirMatch,
    ) -> Option<(SymValueId, SymTy)> {
        if self.params_by_slot.len() != 1 || m.arms.len() != 2 {
            return None;
        }
        let crate::ir::mir::MirExpr::Local(subject_local) = &m.subject.node else {
            return None;
        };
        let (_, subject_ty) = self.params_by_slot.get(&subject_local.node.slot.0)?;
        let (type_name, hit_ctor, miss_ctor, tag) = match subject_ty {
            SymTy::App(name, args) if name == "Option" && args.len() == 1 => (
                "Option",
                crate::ir::hir::BuiltinCtor::OptionSome,
                crate::ir::hir::BuiltinCtor::OptionNone,
                crate::codegen::wasm_gc::OPTION_SOME_TAG,
            ),
            SymTy::App(name, args) if name == "Result" && args.len() == 2 => (
                "Result",
                crate::ir::hir::BuiltinCtor::ResultOk,
                crate::ir::hir::BuiltinCtor::ResultErr,
                crate::codegen::wasm_gc::RESULT_OK_TAG,
            ),
            _ => return None,
        };

        let mut hit = None;
        let mut miss = None;
        let mut wildcard = None;
        for arm in &m.arms {
            let value = mir_int_literal(&arm.body)?;
            match &arm.pattern {
                crate::ir::mir::MirPattern::Ctor {
                    ctor: crate::ir::mir::MirCtor::Builtin(ctor),
                    ..
                } if *ctor == hit_ctor && hit.is_none() => hit = Some(value),
                crate::ir::mir::MirPattern::Ctor {
                    ctor: crate::ir::mir::MirCtor::Builtin(ctor),
                    ..
                } if *ctor == miss_ctor && miss.is_none() => miss = Some(value),
                crate::ir::mir::MirPattern::Wildcard if wildcard.is_none() => {
                    wildcard = Some(value)
                }
                _ => return None,
            }
        }
        match (hit, miss, wildcard) {
            (Some(hit), Some(miss), None) => self.push_tag_match(
                &m.subject,
                type_name,
                tag,
                hit,
                miss,
            ),
            (Some(hit), None, Some(miss)) => self.push_tag_match(
                &m.subject,
                type_name,
                tag,
                hit,
                miss,
            ),
            (None, Some(miss), Some(hit)) => self.push_tag_match(
                &m.subject,
                type_name,
                tag,
                hit,
                miss,
            ),
            _ => None,
        }
    }

    fn push_tag_match(
        &mut self,
        subject: &crate::ast::Spanned<crate::ir::mir::MirExpr>,
        type_name: &str,
        tag: i32,
        hit: i64,
        miss: i64,
    ) -> Option<(SymValueId, SymTy)> {
        let (scrutinee, _) = self.lower_expr(subject)?;
        let int_block = |value| SymBlock {
            nodes: vec![SymNode {
                id: SymValueId(0),
                ty: SymTy::Int,
                kind: SymNodeKind::ConstInt(value),
            }],
            result: SymValueId(0),
        };
        self.push_node(
            SymTy::Int,
            SymNodeKind::TagMatch {
                type_name: type_name.to_string(),
                scrutinee,
                tag: i64::from(tag),
                hit: Box::new(int_block(hit)),
                miss: Box::new(int_block(miss)),
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
    use crate::ir::hir::BuiltinCtor;
    use crate::ir::FnId;
    use crate::ir::mir::{
        LocalId, MirBinOp, MirCtor, MirExpr, MirFn, MirFnRepr, MirLocal, MirMatch, MirMatchArm,
        MirParam, MirPattern,
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

    fn option_slot_count_fn(some_body: Spanned<MirExpr>) -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "slotCount".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "egg".to_string(),
                ty: "Option(Int)".to_string(),
            }],
            return_type: "Int".to_string(),
            effects: vec![],
            body: span(MirExpr::Match(span(MirMatch {
                subject: Box::new(int_local(0)),
                arms: vec![
                    MirMatchArm {
                        pattern: MirPattern::Ctor {
                            ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
                            bindings: vec![],
                            binding_names: vec![],
                        },
                        body: int_lit(0),
                    },
                    MirMatchArm {
                        pattern: MirPattern::Ctor {
                            ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
                            bindings: vec![LocalId(1)],
                            binding_names: vec!["value".to_string()],
                        },
                        body: some_body,
                    },
                ],
            }))),
            local_count: 2,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    #[test]
    fn option_literal_match_lowers_to_operational_tag_dispatch() {
        let mir_fn = option_slot_count_fn(int_lit(1));
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None).expect("tag plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("Option tag match must remain a source symbolic plan")
        };
        assert_eq!(
            sym.params,
            vec![SymTy::App("Option".to_string(), vec![SymTy::Int])]
        );
        assert!(matches!(
            &sym.body.nodes[1].kind,
            SymNodeKind::TagMatch {
                type_name,
                scrutinee: SymValueId(0),
                tag,
                hit,
                miss,
            } if type_name == "Option"
                && *tag == i64::from(crate::codegen::wasm_gc::OPTION_SOME_TAG)
                && matches!(hit.nodes[0].kind, SymNodeKind::ConstInt(1))
                && matches!(miss.nodes[0].kind, SymNodeKind::ConstInt(0))
        ));
        let encoded = plan
            .to_expr_fragment_plan(
                &FragHostTable::placeholder(),
                &FragStructTable::placeholder_for(sym),
            )
            .expect("tag plan encodes");
        assert!(expr_fragment_is_tag_dispatch(&encoded));
    }

    #[test]
    fn option_match_using_payload_stays_unplanned() {
        let mir_fn = option_slot_count_fn(int_local(1));
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None).is_none(),
            "payload-dependent match exceeds the narrow tag-dispatch producer scope"
        );
    }

    #[test]
    fn direct_float_mir_prefers_source_level_sym_plan() {
        let mir_fn = float_binop_fn(BinOp::Add);
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None).expect("plan");
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
        let sym = sym_plan_from_mir_fn(&mir_fn, &|_, _| None).expect("sym plan exists");
        assert_eq!(sym.params, vec![SymTy::Int]);
        assert_eq!(sym.result, SymTy::Int);
        let expr_plan = sym
            .to_expr_fragment_plan(&FragHostTable::placeholder(), &FragStructTable::default())
            .expect("source int identity encodes to a representation plan");
        assert_eq!(expr_plan.result, FragTy::IntCarrier);
        assert!(
            !expr_fragment_plan_has_face(&expr_plan),
            "carrier identity must not have a rendered proof face"
        );
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None).is_none(),
            "producer must not select a carrier-returning plan without the \
             straight-line integer face"
        );
    }

    #[test]
    fn int_carrier_comparison_prefers_source_level_sym_plan() {
        let mir_fn = int_predicate_fn(BinOp::Lt, int_local(0), int_lit(0));
        let plan = fragment_plan_from_mir_fn(&mir_fn, &|_, _| None).expect("plan");
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

    fn user_name_projection_fn() -> MirFn {
        MirFn {
            fn_id: FnId(0),
            name: "userName".to_string(),
            params: vec![MirParam {
                local: LocalId(0),
                name: "u".to_string(),
                ty: "User".to_string(),
            }],
            return_type: "String".to_string(),
            effects: vec![],
            body: span(MirExpr::Project(span(crate::ir::mir::MirProject {
                base: Box::new(int_local(0)),
                field: "name".to_string(),
            }))),
            local_count: 1,
            aliased_slots: std::sync::Arc::new(Vec::new()),
            repr: MirFnRepr::default(),
        }
    }

    #[test]
    fn record_string_field_projection_plans_through_the_project_face() {
        let mir_fn = user_name_projection_fn();
        let record_fields = |record: &str, field: &str| -> Option<(u32, String)> {
            (record == "User" && field == "name").then(|| (0, "String".to_string()))
        };
        let plan = fragment_plan_from_mir_fn(&mir_fn, &record_fields).expect("plan");
        let FragmentPlan::Sym(sym) = &plan else {
            panic!("record projection should plan as a source-level SymPlan")
        };
        assert_eq!(sym.params, vec![SymTy::Named("User".to_string())]);
        assert_eq!(sym.result, SymTy::String);
        assert!(matches!(
            &sym.body.nodes[1].kind,
            SymNodeKind::ProjectField {
                type_name,
                field: 0,
                field_ty: SymTy::String,
                value: SymValueId(0),
            } if type_name == "User"
        ));
        let frag = sym
            .to_expr_fragment_plan(
                &FragHostTable::placeholder(),
                &FragStructTable::placeholder_for(sym),
            )
            .expect("projection encodes to a representation plan");
        assert!(
            expr_fragment_project_face(&frag).is_some(),
            "encoded projection must match the field-projection face"
        );
    }

    #[test]
    fn record_int_field_projection_stays_unplanned_without_a_face() {
        // `pairFst`-style Int-field reads have no verbatim projection face:
        // the SymPlan exists, but the encoder fail-closes (scalar field), so
        // the export keeps its MIR-emitted body and legacy classification.
        let mut mir_fn = user_name_projection_fn();
        mir_fn.return_type = "Int".to_string();
        let record_fields = |record: &str, field: &str| -> Option<(u32, String)> {
            (record == "User" && field == "name").then(|| (0, "Int".to_string()))
        };
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &record_fields).is_none(),
            "scalar-field projection must stay unplanned"
        );
    }

    #[test]
    fn record_projection_without_layout_lookup_stays_unplanned() {
        let mir_fn = user_name_projection_fn();
        assert!(
            fragment_plan_from_mir_fn(&mir_fn, &|_, _| None).is_none(),
            "projection over an unknown record layout must stay unplanned"
        );
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
