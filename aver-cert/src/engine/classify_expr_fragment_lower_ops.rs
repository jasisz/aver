// `Op`-level canonical lowering of expression-fragment plans. This is the
// checker-side twin of the byte lowering in `classify_expr_fragment_lower.rs`:
// the classifier re-derives a straight-line `Op` body from the plan and
// compares it against the disassembled artifact. It lives in the `engine`
// layer because `Op` is a classifier type; the byte lowering the wasm-gc
// emitter needs stays in the `plans` layer.

fn lower_expr_fragment_plan(plan: &ExprFragmentPlan, carrier: u32) -> Result<Vec<Op>, String> {
    lower_expr_fragment_block(&plan.body, carrier)
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
            FragNodeKind::StructGetUser {
                ty_idx,
                field,
                value,
            } => {
                lower_pop(&mut stack, *value, node.id)?;
                ops.push(Op::StructGet(*ty_idx, *field));
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
            FragNodeKind::SelfCall {
                tail,
                func_idx,
                args,
            } => {
                for arg in args.iter().rev() {
                    lower_pop(&mut stack, *arg, node.id)?;
                }
                ops.push(if *tail {
                    Op::ReturnCall(*func_idx)
                } else {
                    Op::Call(*func_idx)
                });
                stack.push(node.id);
            }
            FragNodeKind::VectorGetOrDefault {
                arr_ty,
                to_index_idx,
                box_idx,
                default,
            } => {
                // Monolithic template over pinned locals 0/1; consumes no
                // operand stack values, so it is canonical only as the sole
                // value (twin of `PlanLower.vectorGetOrDefaultTemplate`).
                if !stack.is_empty() {
                    return Err(format!(
                        "fused vector read v{} requires an empty stack",
                        node.id.0
                    ));
                }
                ops.extend([
                    Op::LocalGet(1),
                    Op::Call(*to_index_idx),
                    Op::I32Const(0),
                    Op::I32GeS,
                    Op::LocalGet(1),
                    Op::Call(*to_index_idx),
                    Op::LocalGet(0),
                    Op::ArrayLen,
                    Op::I32LtU,
                    Op::I32And,
                    Op::If,
                    Op::LocalGet(0),
                    Op::LocalGet(1),
                    Op::Call(*to_index_idx),
                    Op::ArrayGet(*arr_ty),
                    Op::Else,
                    Op::I64Const(*default),
                    Op::Call(*box_idx),
                    Op::End,
                ]);
                stack.push(node.id);
            }
            FragNodeKind::StructNew { ty_idx, args } => {
                for arg in args.iter().rev() {
                    lower_pop(&mut stack, *arg, node.id)?;
                }
                ops.push(Op::StructNew(*ty_idx, args.len() as u32));
                stack.push(node.id);
            }
            FragNodeKind::IntSignCmp {
                op,
                constant,
                scratch,
                value,
            } => {
                // Monolithic sign template (twin of
                // `PlanLower.intSignCmpTemplate`).
                lower_pop(&mut stack, *value, node.id)?;
                ops.extend([
                    Op::LocalSet(*scratch),
                    Op::LocalGet(*scratch),
                    Op::StructGet(carrier, 1),
                    Op::RefIsNull,
                    Op::If,
                    Op::LocalGet(*scratch),
                    Op::StructGet(carrier, 0),
                    Op::I64Const(*constant),
                    op_to_wasm(int_sign_cmp_small_prim(*op)),
                    Op::Else,
                ]);
                match int_sign_cmp_sign_prim(*op) {
                    None => ops.push(Op::I32Const(0)),
                    Some(prim) => ops.extend([
                        Op::LocalGet(*scratch),
                        Op::StructGet(carrier, 2),
                        Op::I32Const(0),
                        op_to_wasm(prim),
                    ]),
                }
                ops.push(Op::End);
                stack.push(node.id);
            }
            FragNodeKind::If {
                cond,
                then_block,
                else_block,
            } => {
                lower_pop(&mut stack, *cond, node.id)?;
                // Values already on the stack stay beneath the branch (twin
                // of `PlanLower`'s arm; `InterpreterSequencing.wRunF_frame`).
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

fn op_to_wasm(op: FragPrim) -> Op {
    match op {
        FragPrim::F64Add => Op::F64Add,
        FragPrim::F64Mul => Op::F64Mul,
        FragPrim::F64Le => Op::F64Le,
        FragPrim::F64Ge => Op::F64Ge,
        FragPrim::F64Lt => Op::F64Lt,
        FragPrim::F64Gt => Op::F64Gt,
        FragPrim::F64Eq => Op::F64Eq,
        FragPrim::I64Eq => Op::I64Eq,
        FragPrim::I64LeS => Op::I64LeS,
        FragPrim::I64LtS => Op::I64LtS,
        FragPrim::I64GeS => Op::I64GeS,
        FragPrim::I64GtS => Op::I64GtS,
        FragPrim::I32Eq => Op::I32Eq,
        FragPrim::I32And => Op::I32And,
        FragPrim::I32LtS => Op::I32LtS,
        FragPrim::I32GtS => Op::I32GtS,
        FragPrim::I32GeS => Op::I32GeS,
    }
}
