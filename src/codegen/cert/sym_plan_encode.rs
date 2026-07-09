fn expr_fragment_block_from_sym(block: &SymBlock) -> Option<FragBlock> {
    let mut encoder = SymToFragEncoder {
        source_nodes: &block.nodes,
        nodes: Vec::new(),
        sym_to_frag: Vec::new(),
    };
    for node in &block.nodes {
        encoder.encode_node(node)?;
    }
    let result = *encoder.sym_to_frag.get(block.result.0)?;
    Some(FragBlock {
        nodes: encoder.nodes,
        result,
    })
}

struct SymToFragEncoder<'a> {
    source_nodes: &'a [SymNode],
    nodes: Vec<FragNode>,
    sym_to_frag: Vec<FragValueId>,
}

impl SymToFragEncoder<'_> {
    fn encode_node(&mut self, node: &SymNode) -> Option<()> {
        if node.id.0 != self.sym_to_frag.len() {
            return None;
        }
        let ty = node.ty.to_frag_ty()?;
        let result = match &node.kind {
            SymNodeKind::Param { index } => self.push_node(ty, FragNodeKind::Local { index: *index }),
            SymNodeKind::ConstBool(value) => self.push_node(ty, FragNodeKind::ConstBool(*value)),
            SymNodeKind::ConstFloatBits(bits) => self.push_node(ty, FragNodeKind::ConstF64(*bits)),
            SymNodeKind::ConstStringBytes(_) => return None,
            SymNodeKind::Prim { op, args } => {
                let args = args
                    .iter()
                    .map(|id| self.sym_to_frag.get(id.0).copied())
                    .collect::<Option<Vec<_>>>()?;
                self.push_node(
                    ty,
                    FragNodeKind::Prim {
                        op: op.to_frag_prim()?,
                        args,
                    },
                )
            }
            SymNodeKind::Construct { .. } => return None,
            SymNodeKind::IntConstCmp {
                op,
                value,
                constant,
            } => self.encode_int_const_cmp(*op, *value, *constant)?,
            SymNodeKind::If {
                cond,
                then_block,
                else_block,
            } => {
                let cond = *self.sym_to_frag.get(cond.0)?;
                self.push_node(
                    ty,
                    FragNodeKind::If {
                        cond,
                        then_block: Box::new(expr_fragment_block_from_sym(then_block)?),
                        else_block: Box::new(expr_fragment_block_from_sym(else_block)?),
                    },
                )
            }
        };
        self.sym_to_frag.push(result);
        Some(())
    }

    fn push_node(&mut self, ty: FragTy, kind: FragNodeKind) -> FragValueId {
        let id = FragValueId(self.nodes.len());
        self.nodes.push(FragNode { id, ty, kind });
        id
    }

    fn encode_int_const_cmp(
        &mut self,
        op: SymIntCmp,
        value: SymValueId,
        constant: i64,
    ) -> Option<FragValueId> {
        let carrier = *self.sym_to_frag.get(value.0)?;
        let param_index = match self.source_nodes.get(value.0)? {
            SymNode {
                ty: SymTy::Int,
                kind: SymNodeKind::Param { index },
                ..
            } => *index,
            _ => return None,
        };
        let magf = self.push_node(
            FragTy::Ref,
            FragNodeKind::StructGet {
                field: 1,
                receiver: carrier,
            },
        );
        let is_small = self.push_node(FragTy::BoolI32, FragNodeKind::RefIsNull { value: magf });
        let then_block = sym_int_small_const_cmp_block(param_index, op, constant)?;
        let else_block = sym_int_big_const_cmp_block(param_index, op)?;
        Some(self.push_node(
            FragTy::BoolI32,
            FragNodeKind::If {
                cond: is_small,
                then_block: Box::new(then_block),
                else_block: Box::new(else_block),
            },
        ))
    }
}

fn sym_int_small_const_cmp_block(index: u32, op: SymIntCmp, k: i64) -> Option<FragBlock> {
    let mut nodes = Vec::new();
    let carrier = push_frag_node(&mut nodes, FragTy::IntCarrier, FragNodeKind::Local { index });
    let small = push_frag_node(
        &mut nodes,
        FragTy::I64,
        FragNodeKind::StructGet {
            field: 0,
            receiver: carrier,
        },
    );
    let constant = push_frag_node(&mut nodes, FragTy::I64, FragNodeKind::ConstI64(k));
    let result = push_frag_node(
        &mut nodes,
        FragTy::BoolI32,
        FragNodeKind::Prim {
            op: sym_int_small_const_cmp_prim(op)?,
            args: vec![small, constant],
        },
    );
    Some(FragBlock { nodes, result })
}

fn sym_int_big_const_cmp_block(index: u32, op: SymIntCmp) -> Option<FragBlock> {
    let mut nodes = Vec::new();
    match sym_int_big_const_cmp_kind(op)? {
        SymBigIntConstCmpKind::Always(value) => {
            let result = push_frag_node(&mut nodes, FragTy::BoolI32, FragNodeKind::ConstBool(value));
            Some(FragBlock { nodes, result })
        }
        SymBigIntConstCmpKind::SignLtZero | SymBigIntConstCmpKind::SignGtZero => {
            let carrier =
                push_frag_node(&mut nodes, FragTy::IntCarrier, FragNodeKind::Local { index });
            let sign = push_frag_node(
                &mut nodes,
                FragTy::RawI32,
                FragNodeKind::StructGet {
                    field: 2,
                    receiver: carrier,
                },
            );
            let zero = push_frag_node(&mut nodes, FragTy::BoolI32, FragNodeKind::ConstBool(false));
            let prim = match sym_int_big_const_cmp_kind(op)? {
                SymBigIntConstCmpKind::SignLtZero => FragPrim::I32LtS,
                SymBigIntConstCmpKind::SignGtZero => FragPrim::I32GtS,
                SymBigIntConstCmpKind::Always(_) => unreachable!(),
            };
            let result = push_frag_node(
                &mut nodes,
                FragTy::BoolI32,
                FragNodeKind::Prim {
                    op: prim,
                    args: vec![sign, zero],
                },
            );
            Some(FragBlock { nodes, result })
        }
    }
}

fn push_frag_node(nodes: &mut Vec<FragNode>, ty: FragTy, kind: FragNodeKind) -> FragValueId {
    let id = FragValueId(nodes.len());
    nodes.push(FragNode { id, ty, kind });
    id
}

#[derive(Clone, Copy)]
enum SymBigIntConstCmpKind {
    Always(bool),
    SignLtZero,
    SignGtZero,
}

fn sym_int_small_const_cmp_prim(op: SymIntCmp) -> Option<FragPrim> {
    match op {
        SymIntCmp::Eq => Some(FragPrim::I64Eq),
        SymIntCmp::Lt => Some(FragPrim::I64LtS),
        SymIntCmp::Le => Some(FragPrim::I64LeS),
        SymIntCmp::Ge => Some(FragPrim::I64GeS),
    }
}

fn sym_int_big_const_cmp_kind(op: SymIntCmp) -> Option<SymBigIntConstCmpKind> {
    match op {
        SymIntCmp::Eq => Some(SymBigIntConstCmpKind::Always(false)),
        SymIntCmp::Lt | SymIntCmp::Le => Some(SymBigIntConstCmpKind::SignLtZero),
        SymIntCmp::Ge => Some(SymBigIntConstCmpKind::SignGtZero),
    }
}
