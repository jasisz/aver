/// Source-level certificate plan. Unlike `ExprFragmentPlan`, this IR talks in
/// Aver semantic types and operations first; target representation only enters
/// later through a checked encoder/lowerer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SymTy {
    Int,
    Float,
    Bool,
    String,
    WVal,
}

impl From<FragSemTy> for SymTy {
    fn from(value: FragSemTy) -> Self {
        match value {
            FragSemTy::Float => SymTy::Float,
            FragSemTy::Bool => SymTy::Bool,
            FragSemTy::Int => SymTy::Int,
            FragSemTy::WVal => SymTy::WVal,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymValueId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SymPrim {
    FloatAdd,
    FloatMul,
    FloatLe,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymNodeKind {
    Param { index: u32 },
    ConstBool(bool),
    ConstFloatBits(u64),
    Prim {
        op: SymPrim,
        args: Vec<SymValueId>,
    },
    If {
        cond: SymValueId,
        then_block: Box<SymBlock>,
        else_block: Box<SymBlock>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymNode {
    pub id: SymValueId,
    pub ty: SymTy,
    pub kind: SymNodeKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymBlock {
    pub nodes: Vec<SymNode>,
    pub result: SymValueId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymPlan {
    pub params: Vec<SymTy>,
    pub result: SymTy,
    pub body: SymBlock,
}

impl SymPlan {
    /// Project the current representation-level expression fragment into the
    /// source-level subset when every node already has a direct Aver meaning.
    /// Representation-only nodes (`struct.get`, carrier limbs, raw i32/i64
    /// comparisons) deliberately fail here; they need explicit source
    /// constructors before they can become `SymPlan`.
    pub fn from_expr_fragment_source_subset(plan: &ExprFragmentPlan) -> Option<Self> {
        Some(Self {
            params: plan
                .params
                .iter()
                .map(|ty| SymTy::from(ty.sem_ty()))
                .collect(),
            result: SymTy::from(plan.result.sem_ty()),
            body: sym_block_from_frag_source_subset(&plan.body)?,
        })
    }
}

fn sym_block_from_frag_source_subset(block: &FragBlock) -> Option<SymBlock> {
    let nodes = block
        .nodes
        .iter()
        .map(sym_node_from_frag_source_subset)
        .collect::<Option<Vec<_>>>()?;
    Some(SymBlock {
        nodes,
        result: SymValueId(block.result.0),
    })
}

fn sym_node_from_frag_source_subset(node: &FragNode) -> Option<SymNode> {
    let ty = SymTy::from(node.ty.sem_ty());
    let kind = match &node.kind {
        FragNodeKind::Local { index } => SymNodeKind::Param { index: *index },
        FragNodeKind::ConstBool(value) => SymNodeKind::ConstBool(*value),
        FragNodeKind::ConstF64(bits) => SymNodeKind::ConstFloatBits(*bits),
        FragNodeKind::Prim { op, args } => SymNodeKind::Prim {
            op: match op {
                FragPrim::F64Add => SymPrim::FloatAdd,
                FragPrim::F64Mul => SymPrim::FloatMul,
                FragPrim::F64Le => SymPrim::FloatLe,
                FragPrim::I64Eq
                | FragPrim::I64LeS
                | FragPrim::I64LtS
                | FragPrim::I64GeS
                | FragPrim::I32LtS
                | FragPrim::I32GtS => return None,
            },
            args: args.iter().map(|id| SymValueId(id.0)).collect(),
        },
        FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } => SymNodeKind::If {
            cond: SymValueId(cond.0),
            then_block: Box::new(sym_block_from_frag_source_subset(then_block)?),
            else_block: Box::new(sym_block_from_frag_source_subset(else_block)?),
        },
        FragNodeKind::ConstI64(_)
        | FragNodeKind::ConstI32(_)
        | FragNodeKind::StructGet { .. }
        | FragNodeKind::RefIsNull { .. } => return None,
    };
    Some(SymNode {
        id: SymValueId(node.id.0),
        ty,
        kind,
    })
}

#[cfg(test)]
mod sym_plan_defs_tests {
    use super::*;

    #[test]
    fn sym_plan_projects_direct_float_fragment() {
        let plan = ExprFragmentPlan {
            params: vec![FragTy::F64, FragTy::F64],
            result: FragTy::F64,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::F64,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::F64,
                        kind: FragNodeKind::Local { index: 1 },
                    },
                    FragNode {
                        id: FragValueId(2),
                        ty: FragTy::F64,
                        kind: FragNodeKind::Prim {
                            op: FragPrim::F64Add,
                            args: vec![FragValueId(0), FragValueId(1)],
                        },
                    },
                ],
                result: FragValueId(2),
            },
        };

        let sym = SymPlan::from_expr_fragment_source_subset(&plan).expect("source subset");
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
    fn sym_plan_rejects_representation_only_int_limb_fragment() {
        let plan = ExprFragmentPlan {
            params: vec![FragTy::IntCarrier],
            result: FragTy::BoolI32,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::IntCarrier,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::I64,
                        kind: FragNodeKind::StructGet {
                            field: 0,
                            receiver: FragValueId(0),
                        },
                    },
                ],
                result: FragValueId(1),
            },
        };

        assert!(SymPlan::from_expr_fragment_source_subset(&plan).is_none());
    }
}
