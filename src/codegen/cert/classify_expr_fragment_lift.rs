struct FragLiftState {
    params: Vec<FragTy>,
    carrier: u32,
    stack: Vec<FragValueId>,
    nodes: Vec<FragNode>,
}

impl FragLiftState {
    fn new(params: Vec<FragTy>, carrier: u32) -> Self {
        Self {
            params,
            carrier,
            stack: Vec::new(),
            nodes: Vec::new(),
        }
    }

    fn push_node(&mut self, ty: FragTy, kind: FragNodeKind) -> Option<FragValueId> {
        // Keep this checker deliberately small. Large expressions should grow a
        // profiled limit intentionally, not by accident.
        if self.nodes.len() >= 256 {
            return None;
        }
        let id = FragValueId(self.nodes.len());
        self.nodes.push(FragNode { id, ty, kind });
        self.stack.push(id);
        Some(id)
    }

    fn pop_expect(&mut self, ty: FragTy) -> Option<FragValueId> {
        let id = self.stack.pop()?;
        let got = self.nodes.get(id.0)?.ty;
        (got == ty).then_some(id)
    }

    fn pop_i32(&mut self) -> Option<FragValueId> {
        let id = self.stack.pop()?;
        let got = self.nodes.get(id.0)?.ty;
        matches!(got, FragTy::RawI32 | FragTy::BoolI32).then_some(id)
    }

    fn finish(self, result: FragTy) -> Option<FragBlock> {
        let [root] = self.stack.as_slice() else {
            return None;
        };
        let root = *root;
        (self.nodes.get(root.0)?.ty == result).then_some(FragBlock {
            nodes: self.nodes,
            result: root,
        })
    }
}

fn nr_expr_fragment(f: &UserFn, body: &StructuralBody, carrier: Option<u32>) -> Option<Cert> {
    if f.arity == 0 || !f.calls.is_empty() {
        return None;
    }
    let carrier = carrier?;
    let raw_ops = strip_trailing_end(&f.ops);
    let params = f
        .params
        .iter()
        .map(|ty| expr_fragment_ty_from_wasm_param(ty, carrier))
        .collect::<Option<Vec<_>>>()?;
    let result = expr_fragment_ty_from_wasm_result(f.result?)?;
    let mut st = FragLiftState::new(params.clone(), carrier);
    lift_expr_fragment_nodes(&mut st, &body.tree)?;
    let body = st.finish(result)?;
    let plan = ExprFragmentPlan {
        params,
        result,
        body,
    };
    let canonical_ops = lower_expr_fragment_plan(&plan, carrier).ok()?;
    if canonical_ops.as_slice() != raw_ops {
        return None;
    }
    let canonical_code_entry_bytes =
        lower_expr_fragment_plan_code_entry_bytes(&plan, carrier).ok()?;
    if canonical_code_entry_bytes != f.code_entry_bytes {
        return None;
    }
    Some(Cert::ExprFragment {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        plan,
        ops: canonical_ops,
    })
}

fn expr_fragment_ty_from_wasm_param(ty: &TyKind, carrier: u32) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        TyKind::Ref(idx) if *idx == carrier => Some(FragTy::IntCarrier),
        _ => None,
    }
}

fn expr_fragment_ty_from_wasm_result(ty: TyKind) -> Option<FragTy> {
    match ty {
        TyKind::F64 => Some(FragTy::F64),
        TyKind::I32 => Some(FragTy::BoolI32),
        _ => None,
    }
}

fn lift_expr_fragment_nodes(st: &mut FragLiftState, nodes: &[InstrNode]) -> Option<()> {
    for node in nodes {
        match node {
            InstrNode::Op(op) => lift_expr_fragment_op(st, op)?,
            InstrNode::IfElse(then_b, else_b) => lift_expr_fragment_if(st, then_b, else_b)?,
        }
    }
    Some(())
}

fn lift_expr_fragment_op(st: &mut FragLiftState, op: &Op) -> Option<()> {
    match op {
        Op::LocalGet(idx) => {
            let ty = *st.params.get(*idx as usize)?;
            st.push_node(ty, FragNodeKind::Local { index: *idx })?;
        }
        Op::I32Const(0) => {
            st.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(false))?;
        }
        Op::I32Const(1) => {
            st.push_node(FragTy::BoolI32, FragNodeKind::ConstBool(true))?;
        }
        Op::I64Const(k) => {
            st.push_node(FragTy::I64, FragNodeKind::ConstI64(*k))?;
        }
        Op::I32Const(k) => {
            st.push_node(FragTy::RawI32, FragNodeKind::ConstI32(*k))?;
        }
        Op::F64Const(bits) => {
            st.push_node(FragTy::F64, FragNodeKind::ConstF64(*bits))?;
        }
        Op::StructGet(carrier, field) if *carrier == st.carrier => {
            let receiver = st.pop_expect(FragTy::IntCarrier)?;
            let ty = match field {
                0 => FragTy::I64,
                1 => FragTy::Ref,
                2 => FragTy::RawI32,
                _ => return None,
            };
            st.push_node(
                ty,
                FragNodeKind::StructGet {
                    field: *field,
                    receiver,
                },
            )?;
        }
        Op::RefIsNull => {
            let value = st.pop_expect(FragTy::Ref)?;
            if !matches!(
                st.nodes.get(value.0).map(|node| &node.kind),
                Some(FragNodeKind::StructGet { field: 1, .. })
            ) {
                return None;
            }
            st.push_node(FragTy::BoolI32, FragNodeKind::RefIsNull { value })?;
        }
        Op::F64Add => lift_expr_fragment_f64_bin(st, FragPrim::F64Add)?,
        Op::F64Mul => lift_expr_fragment_f64_bin(st, FragPrim::F64Mul)?,
        Op::F64Le => {
            let rhs = st.pop_expect(FragTy::F64)?;
            let lhs = st.pop_expect(FragTy::F64)?;
            st.push_node(
                FragTy::BoolI32,
                FragNodeKind::Prim {
                    op: FragPrim::F64Le,
                    args: vec![lhs, rhs],
                },
            )?;
        }
        Op::I64Eq => lift_expr_fragment_i64_cmp(st, FragPrim::I64Eq)?,
        Op::I64LeS => lift_expr_fragment_i64_cmp(st, FragPrim::I64LeS)?,
        Op::I64LtS => lift_expr_fragment_i64_cmp(st, FragPrim::I64LtS)?,
        Op::I64GeS => lift_expr_fragment_i64_cmp(st, FragPrim::I64GeS)?,
        Op::I32LtS => lift_expr_fragment_i32_cmp(st, FragPrim::I32LtS)?,
        Op::I32GtS => lift_expr_fragment_i32_cmp(st, FragPrim::I32GtS)?,
        _ => return None,
    }
    Some(())
}

fn lift_expr_fragment_f64_bin(st: &mut FragLiftState, op: FragPrim) -> Option<()> {
    let rhs = st.pop_expect(FragTy::F64)?;
    let lhs = st.pop_expect(FragTy::F64)?;
    st.push_node(
        FragTy::F64,
        FragNodeKind::Prim {
            op,
            args: vec![lhs, rhs],
        },
    )?;
    Some(())
}

fn lift_expr_fragment_i64_cmp(st: &mut FragLiftState, op: FragPrim) -> Option<()> {
    let rhs = st.pop_expect(FragTy::I64)?;
    let lhs = st.pop_expect(FragTy::I64)?;
    st.push_node(
        FragTy::BoolI32,
        FragNodeKind::Prim {
            op,
            args: vec![lhs, rhs],
        },
    )?;
    Some(())
}

fn lift_expr_fragment_i32_cmp(st: &mut FragLiftState, op: FragPrim) -> Option<()> {
    let rhs = st.pop_i32()?;
    let lhs = st.pop_i32()?;
    st.push_node(
        FragTy::BoolI32,
        FragNodeKind::Prim {
            op,
            args: vec![lhs, rhs],
        },
    )?;
    Some(())
}

fn lift_expr_fragment_if(
    st: &mut FragLiftState,
    then_b: &[InstrNode],
    else_b: &[InstrNode],
) -> Option<()> {
    let cond = st.pop_expect(FragTy::BoolI32)?;
    // v1 accepts expression-shaped branches only. This is the fail-closed
    // version of the "branch must not consume below the block base" rule.
    if !st.stack.is_empty() {
        return None;
    }

    let mut then_st = FragLiftState::new(st.params.clone(), st.carrier);
    lift_expr_fragment_nodes(&mut then_st, then_b)?;
    let then_ty = then_st.stack_result_ty()?;
    let then_block = then_st.finish(then_ty)?;

    let mut else_st = FragLiftState::new(st.params.clone(), st.carrier);
    lift_expr_fragment_nodes(&mut else_st, else_b)?;
    let else_ty = else_st.stack_result_ty()?;
    let else_block = else_st.finish(else_ty)?;

    let ty = then_block.result_ty()?;
    if else_block.result_ty()? != ty {
        return None;
    }
    st.push_node(
        ty,
        FragNodeKind::If {
            cond,
            then_block: Box::new(then_block),
            else_block: Box::new(else_block),
        },
    )?;
    Some(())
}

trait FragStackResultTy {
    fn stack_result_ty(&self) -> Option<FragTy>;
}

impl FragStackResultTy for FragLiftState {
    fn stack_result_ty(&self) -> Option<FragTy> {
        let [root] = self.stack.as_slice() else {
            return None;
        };
        self.nodes.get(root.0).map(|node| node.ty)
    }
}
