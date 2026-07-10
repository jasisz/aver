fn lower_expr_fragment_plan(plan: &ExprFragmentPlan, carrier: u32) -> Result<Vec<Op>, String> {
    lower_expr_fragment_block(&plan.body, carrier)
}

pub(crate) fn lower_expr_fragment_plan_function(
    plan: &ExprFragmentPlan,
    carrier: u32,
) -> Result<wasm_encoder::Function, String> {
    let carrier_ref = wasm_encoder::ValType::Ref(wasm_encoder::RefType {
        nullable: true,
        heap_type: wasm_encoder::HeapType::Concrete(carrier),
    });
    let mut func = wasm_encoder::Function::new([(1, carrier_ref)]);
    func.raw(lower_expr_fragment_plan_expr_bytes(plan, carrier)?);
    Ok(func)
}

fn lower_expr_fragment_plan_expr_bytes(
    plan: &ExprFragmentPlan,
    carrier: u32,
) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    lower_expr_fragment_block_bytes(&plan.body, carrier, &mut out)?;
    out.push(0x0b);
    Ok(out)
}

fn lower_expr_fragment_plan_body_bytes(
    plan: &ExprFragmentPlan,
    carrier: u32,
) -> Result<Vec<u8>, String> {
    let mut out = Vec::new();
    // `expr-fragment-v1` is pinned to the current wasm-gc cert island shape:
    // one unused scratch local of the Int carrier reference type, followed by
    // the canonical expression body.
    push_u32_leb(&mut out, 1);
    push_u32_leb(&mut out, 1);
    out.push(0x63);
    push_u32_leb(&mut out, carrier);
    out.extend(lower_expr_fragment_plan_expr_bytes(plan, carrier)?);
    Ok(out)
}

fn lower_expr_fragment_plan_code_entry_bytes(
    plan: &ExprFragmentPlan,
    carrier: u32,
) -> Result<Vec<u8>, String> {
    let body = lower_expr_fragment_plan_body_bytes(plan, carrier)?;
    let body_len = u32::try_from(body.len())
        .map_err(|_| "expr-fragment body is too large to encode".to_string())?;
    let mut out = Vec::new();
    push_u32_leb(&mut out, body_len);
    out.extend(body);
    Ok(out)
}

fn lower_expr_fragment_block(block: &FragBlock, carrier: u32) -> Result<Vec<Op>, String> {
    let mut ops = Vec::new();
    let mut stack = Vec::<FragValueId>::new();
    for node in &block.nodes {
        match &node.kind {
            FragNodeKind::Local { index } => {
                ops.push(Op::LocalGet(*index));
                stack.push(node.id);
            }
            FragNodeKind::ConstBool(value) => {
                ops.push(Op::I32Const(if *value { 1 } else { 0 }));
                stack.push(node.id);
            }
            FragNodeKind::ConstI64(value) => {
                ops.push(Op::I64Const(*value));
                stack.push(node.id);
            }
            FragNodeKind::ConstI32(value) => {
                ops.push(Op::I32Const(*value));
                stack.push(node.id);
            }
            FragNodeKind::ConstF64(bits) => {
                ops.push(Op::F64Const(*bits));
                stack.push(node.id);
            }
            FragNodeKind::StructGet { field, receiver } => {
                lower_pop(&mut stack, *receiver, node.id)?;
                ops.push(Op::StructGet(carrier, *field));
                stack.push(node.id);
            }
            FragNodeKind::RefIsNull { value } => {
                lower_pop(&mut stack, *value, node.id)?;
                ops.push(Op::RefIsNull);
                stack.push(node.id);
            }
            FragNodeKind::Prim { op, args } => {
                for arg in args.iter().rev() {
                    lower_pop(&mut stack, *arg, node.id)?;
                }
                ops.push(op_to_wasm(*op));
                stack.push(node.id);
            }
            FragNodeKind::HostCall { func_idx, args, .. } => {
                for arg in args.iter().rev() {
                    lower_pop(&mut stack, *arg, node.id)?;
                }
                ops.push(Op::Call(*func_idx));
                stack.push(node.id);
            }
            FragNodeKind::If {
                cond,
                then_block,
                else_block,
            } => {
                lower_pop(&mut stack, *cond, node.id)?;
                if !stack.is_empty() {
                    return Err(format!(
                        "plan if v{} would leave non-empty stack before branch",
                        node.id.0
                    ));
                }
                ops.push(Op::If);
                ops.extend(lower_expr_fragment_block(then_block, carrier)?);
                ops.push(Op::Else);
                ops.extend(lower_expr_fragment_block(else_block, carrier)?);
                ops.push(Op::End);
                stack.push(node.id);
            }
        }
    }
    if stack.as_slice() != [block.result] {
        return Err(format!(
            "canonical lowering final stack {} does not equal block result v{}",
            render_fragment_value_stack(&stack),
            block.result.0
        ));
    }
    Ok(ops)
}

fn lower_expr_fragment_block_bytes(
    block: &FragBlock,
    carrier: u32,
    out: &mut Vec<u8>,
) -> Result<(), String> {
    let mut stack = Vec::<FragValueId>::new();
    for node in &block.nodes {
        match &node.kind {
            FragNodeKind::Local { index } => {
                out.push(0x20);
                push_u32_leb(out, *index);
                stack.push(node.id);
            }
            FragNodeKind::ConstBool(value) => {
                out.push(0x41);
                push_i32_leb(out, if *value { 1 } else { 0 });
                stack.push(node.id);
            }
            FragNodeKind::ConstI64(value) => {
                out.push(0x42);
                push_i64_leb(out, *value);
                stack.push(node.id);
            }
            FragNodeKind::ConstI32(value) => {
                out.push(0x41);
                push_i32_leb(out, *value);
                stack.push(node.id);
            }
            FragNodeKind::ConstF64(bits) => {
                out.push(0x44);
                out.extend(bits.to_le_bytes());
                stack.push(node.id);
            }
            FragNodeKind::StructGet { field, receiver } => {
                lower_pop(&mut stack, *receiver, node.id)?;
                out.push(0xfb);
                push_u32_leb(out, 0x02);
                push_u32_leb(out, carrier);
                push_u32_leb(out, *field);
                stack.push(node.id);
            }
            FragNodeKind::RefIsNull { value } => {
                lower_pop(&mut stack, *value, node.id)?;
                out.push(0xd1);
                stack.push(node.id);
            }
            FragNodeKind::Prim { op, args } => {
                for arg in args.iter().rev() {
                    lower_pop(&mut stack, *arg, node.id)?;
                }
                push_prim_opcode(out, *op);
                stack.push(node.id);
            }
            FragNodeKind::HostCall { func_idx, args, .. } => {
                for arg in args.iter().rev() {
                    lower_pop(&mut stack, *arg, node.id)?;
                }
                out.push(0x10);
                push_u32_leb(out, *func_idx);
                stack.push(node.id);
            }
            FragNodeKind::If {
                cond,
                then_block,
                else_block,
            } => {
                lower_pop(&mut stack, *cond, node.id)?;
                if !stack.is_empty() {
                    return Err(format!(
                        "canonical byte lowering for if v{} would leave non-empty stack before branch",
                        node.id.0
                    ));
                }
                out.push(0x04);
                push_expr_fragment_blocktype(out, node.ty)?;
                lower_expr_fragment_block_bytes(then_block, carrier, out)?;
                out.push(0x05);
                lower_expr_fragment_block_bytes(else_block, carrier, out)?;
                out.push(0x0b);
                stack.push(node.id);
            }
        }
    }
    if stack.as_slice() != [block.result] {
        return Err(format!(
            "canonical byte lowering final stack {} does not equal block result v{}",
            render_fragment_value_stack(&stack),
            block.result.0
        ));
    }
    Ok(())
}

fn push_expr_fragment_blocktype(out: &mut Vec<u8>, ty: FragTy) -> Result<(), String> {
    let byte = match ty {
        FragTy::BoolI32 | FragTy::RawI32 => 0x7f,
        FragTy::I64 => 0x7e,
        FragTy::F64 => 0x7c,
        FragTy::IntCarrier | FragTy::Ref => {
            return Err(format!(
                "canonical byte lowering does not support if result `{}` yet",
                ty.plan_tag()
            ));
        }
    };
    out.push(byte);
    Ok(())
}

fn push_prim_opcode(out: &mut Vec<u8>, op: FragPrim) {
    out.push(match op {
        FragPrim::F64Add => 0xa0,
        FragPrim::F64Mul => 0xa2,
        FragPrim::F64Le => 0x65,
        FragPrim::I64Eq => 0x51,
        FragPrim::I64LtS => 0x53,
        FragPrim::I64LeS => 0x57,
        FragPrim::I64GeS => 0x59,
        FragPrim::I32LtS => 0x48,
        FragPrim::I32GtS => 0x4a,
    });
}

fn push_u32_leb(out: &mut Vec<u8>, mut value: u32) {
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        out.push(byte);
        if value == 0 {
            break;
        }
    }
}

fn push_i32_leb(out: &mut Vec<u8>, value: i32) {
    push_i64_leb(out, value as i64);
}

fn push_i64_leb(out: &mut Vec<u8>, mut value: i64) {
    loop {
        let byte = (value as u8) & 0x7f;
        value >>= 7;
        let sign_set = (byte & 0x40) != 0;
        let done = (value == 0 && !sign_set) || (value == -1 && sign_set);
        out.push(if done { byte } else { byte | 0x80 });
        if done {
            break;
        }
    }
}

fn lower_pop(
    stack: &mut Vec<FragValueId>,
    expected: FragValueId,
    node: FragValueId,
) -> Result<(), String> {
    let got = stack
        .pop()
        .ok_or_else(|| format!("canonical lowering stack underflow at v{}", node.0))?;
    if got == expected {
        Ok(())
    } else {
        Err(format!(
            "canonical lowering for v{} expected stack value v{}, got v{}",
            node.0, expected.0, got.0
        ))
    }
}

fn op_to_wasm(op: FragPrim) -> Op {
    match op {
        FragPrim::F64Add => Op::F64Add,
        FragPrim::F64Mul => Op::F64Mul,
        FragPrim::F64Le => Op::F64Le,
        FragPrim::I64Eq => Op::I64Eq,
        FragPrim::I64LeS => Op::I64LeS,
        FragPrim::I64LtS => Op::I64LtS,
        FragPrim::I64GeS => Op::I64GeS,
        FragPrim::I32LtS => Op::I32LtS,
        FragPrim::I32GtS => Op::I32GtS,
    }
}

fn render_fragment_value_stack(stack: &[FragValueId]) -> String {
    if stack.is_empty() {
        return "[]".to_string();
    }
    format!(
        "[{}]",
        stack
            .iter()
            .map(|id| format!("v{}", id.0))
            .collect::<Vec<_>>()
            .join(",")
    )
}
