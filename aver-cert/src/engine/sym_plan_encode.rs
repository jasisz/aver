impl SymPrim {
    fn to_frag_prim(self) -> Option<FragPrim> {
        match self {
            SymPrim::FloatAdd => Some(FragPrim::F64Add),
            SymPrim::FloatMul => Some(FragPrim::F64Mul),
            SymPrim::FloatLe => Some(FragPrim::F64Le),
            SymPrim::FloatGe => Some(FragPrim::F64Ge),
            SymPrim::FloatLt => Some(FragPrim::F64Lt),
            SymPrim::FloatGt => Some(FragPrim::F64Gt),
            SymPrim::FloatEq => Some(FragPrim::F64Eq),
            // `IntAdd` has no representation-level primitive: the encoder
            // binds it to a `hostCall add` node through the role table.
            SymPrim::IntAdd => None,
            SymPrim::StringEq => None,
            SymPrim::StringConcat => None,
        }
    }
}

fn expr_fragment_block_from_sym(
    block: &SymBlock,
    host_table: &FragHostTable,
    struct_table: &FragStructTable,
) -> Option<FragBlock> {
    let mut encoder = SymToFragEncoder {
        source_nodes: &block.nodes,
        host_table,
        struct_table,
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
    host_table: &'a FragHostTable,
    struct_table: &'a FragStructTable,
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
            SymNodeKind::ConstInt(value) => {
                // A source Int literal is representation-boxed at the point of
                // appearance: raw i64 const, then the byte-derived `box` host
                // call. The source node maps to the boxed carrier value.
                let box_idx = self.host_table.lookup(FragHostRole::Box)?;
                let const_id = self.push_node(FragTy::I64, FragNodeKind::ConstI64(*value));
                self.push_node(
                    ty,
                    FragNodeKind::HostCall {
                        role: FragHostRole::Box,
                        func_idx: box_idx,
                        args: vec![const_id],
                    },
                )
            }
            SymNodeKind::ConstFloatBits(bits) => self.push_node(ty, FragNodeKind::ConstF64(*bits)),
            SymNodeKind::ConstStringBytes(_) => return None,
            SymNodeKind::Prim {
                op: SymPrim::IntAdd,
                args,
            } => {
                let add_idx = self.host_table.lookup(FragHostRole::Add)?;
                let args = args
                    .iter()
                    .map(|id| self.sym_to_frag.get(id.0).copied())
                    .collect::<Option<Vec<_>>>()?;
                self.push_node(
                    ty,
                    FragNodeKind::HostCall {
                        role: FragHostRole::Add,
                        func_idx: add_idx,
                        args,
                    },
                )
            }
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
            SymNodeKind::EmptyList { .. } => return None,
            SymNodeKind::ProjectField {
                type_name,
                field,
                field_ty: _,
                value,
            } => {
                // Only opaque reference fields encode: the projected value
                // flows verbatim through the field-projection face. Scalar
                // fields have no rendered proof face yet, so they fail-close
                // the encoding (twin of the Lean encoder arm).
                if ty != FragTy::AdtRef {
                    return None;
                }
                let ty_idx = self.struct_table.lookup(type_name)?;
                let value = *self.sym_to_frag.get(value.0)?;
                self.push_node(
                    ty,
                    FragNodeKind::StructGetUser {
                        ty_idx,
                        field: *field,
                        value,
                    },
                )
            }
            SymNodeKind::IntConstCmp {
                op,
                value,
                constant,
            } => self.encode_int_const_cmp(*op, *value, *constant)?,
            SymNodeKind::TagMatch {
                type_name,
                scrutinee,
                tag,
                hit,
                miss,
            } => {
                let ty_idx = self.struct_table.lookup(type_name)?;
                let scrutinee = *self.sym_to_frag.get(scrutinee.0)?;
                let tag_value = self.push_node(
                    FragTy::RawI32,
                    FragNodeKind::StructGetUser {
                        ty_idx,
                        field: 0,
                        value: scrutinee,
                    },
                );
                let constant = self.push_node(
                    FragTy::RawI32,
                    FragNodeKind::ConstI32(i32::try_from(*tag).ok()?),
                );
                let cond = self.push_node(
                    FragTy::BoolI32,
                    FragNodeKind::Prim {
                        op: FragPrim::I32Eq,
                        args: vec![tag_value, constant],
                    },
                );
                self.push_node(
                    ty,
                    FragNodeKind::If {
                        cond,
                        then_block: Box::new(expr_fragment_block_from_sym(
                            hit,
                            self.host_table,
                            self.struct_table,
                        )?),
                        else_block: Box::new(expr_fragment_block_from_sym(
                            miss,
                            self.host_table,
                            self.struct_table,
                        )?),
                    },
                )
            }
            SymNodeKind::VectorGetOrDefault { type_name, default } => {
                // Twin of the Lean encoder arm: resolve the vector's array
                // type through the byte-derived struct table and both helpers
                // through the byte-derived role table; a missing binding
                // fail-closes the encoding.
                let arr_ty = self.struct_table.lookup(type_name)?;
                let to_index_idx = self.host_table.lookup(FragHostRole::ToIndex)?;
                let box_idx = self.host_table.lookup(FragHostRole::Box)?;
                self.push_node(
                    ty,
                    FragNodeKind::VectorGetOrDefault {
                        arr_ty,
                        to_index_idx,
                        box_idx,
                        default: *default,
                    },
                )
            }
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
                        then_block: Box::new(expr_fragment_block_from_sym(
                            then_block,
                            self.host_table,
                            self.struct_table,
                        )?),
                        else_block: Box::new(expr_fragment_block_from_sym(
                            else_block,
                            self.host_table,
                            self.struct_table,
                        )?),
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
        SymIntCmp::Gt => Some(FragPrim::I64GtS),
    }
}

fn sym_int_big_const_cmp_kind(op: SymIntCmp) -> Option<SymBigIntConstCmpKind> {
    match op {
        SymIntCmp::Eq => Some(SymBigIntConstCmpKind::Always(false)),
        SymIntCmp::Lt | SymIntCmp::Le => Some(SymBigIntConstCmpKind::SignLtZero),
        // A Big carrier lies strictly outside the i64 range, so `> k` and
        // `>= k` are both decided by the sign limb alone (a Big never equals
        // an i64 literal) — the mirror of `Lt | Le` sharing `SignLtZero`.
        SymIntCmp::Ge | SymIntCmp::Gt => Some(SymBigIntConstCmpKind::SignGtZero),
    }
}
