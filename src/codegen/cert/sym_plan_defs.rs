/// Source-level certificate plan. Unlike `ExprFragmentPlan`, this IR talks in
/// Aver semantic types and operations first; target representation only enters
/// later through a checked encoder/lowerer.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SymTy {
    Int,
    Float,
    Bool,
    String,
}

impl SymTy {
    fn from_frag_ty(value: FragTy) -> Option<Self> {
        match value {
            FragTy::F64 => Some(SymTy::Float),
            FragTy::BoolI32 => Some(SymTy::Bool),
            FragTy::IntCarrier => Some(SymTy::Int),
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref => None,
        }
    }

    fn to_frag_ty(self) -> Option<FragTy> {
        match self {
            SymTy::Int => Some(FragTy::IntCarrier),
            SymTy::Float => Some(FragTy::F64),
            SymTy::Bool => Some(FragTy::BoolI32),
            SymTy::String => None,
        }
    }

    fn plan_tag(self) -> &'static str {
        match self {
            SymTy::Int => "int",
            SymTy::Float => "float",
            SymTy::Bool => "bool",
            SymTy::String => "string",
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "int" => Some(SymTy::Int),
            "float" => Some(SymTy::Float),
            "bool" => Some(SymTy::Bool),
            "string" => Some(SymTy::String),
            _ => None,
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
    StringEq,
    StringConcat,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum SymIntCmp {
    Eq,
    Lt,
    Le,
    Ge,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymNodeKind {
    Param { index: u32 },
    ConstBool(bool),
    ConstFloatBits(u64),
    ConstStringBytes(Vec<u8>),
    Prim {
        op: SymPrim,
        args: Vec<SymValueId>,
    },
    IntConstCmp {
        op: SymIntCmp,
        value: SymValueId,
        constant: i64,
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
                .copied()
                .map(SymTy::from_frag_ty)
                .collect::<Option<Vec<_>>>()?,
            result: SymTy::from_frag_ty(plan.result)?,
            body: sym_block_from_frag_source_subset(&plan.body)?,
        })
    }

    pub(crate) fn to_expr_fragment_plan(&self) -> Option<ExprFragmentPlan> {
        Some(ExprFragmentPlan {
            params: self
                .params
                .iter()
                .copied()
                .map(SymTy::to_frag_ty)
                .collect::<Option<Vec<_>>>()?,
            result: self.result.to_frag_ty()?,
            body: expr_fragment_block_from_sym(&self.body)?,
        })
    }
}

#[derive(Clone, Debug)]
pub enum FragmentPlan {
    Sym(SymPlan),
    Expr(ExprFragmentPlan),
}

impl FragmentPlan {
    pub(crate) fn to_expr_fragment_plan(&self) -> Option<ExprFragmentPlan> {
        match self {
            FragmentPlan::Sym(plan) => plan.to_expr_fragment_plan(),
            FragmentPlan::Expr(plan) => Some(plan.clone()),
        }
    }
}

fn expr_fragment_source_plan(
    source_plan: &Option<SymPlan>,
    plan: &ExprFragmentPlan,
) -> Option<SymPlan> {
    source_plan
        .clone()
        .or_else(|| SymPlan::from_expr_fragment_source_subset(plan))
}

#[derive(Clone, Debug)]
pub struct FragmentPlanArtifact {
    pub export_name: String,
    pub plan: FragmentPlan,
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
    let ty = SymTy::from_frag_ty(node.ty)?;
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
        let lean = sym_plan_lean_value(&sym);
        assert!(lean.contains("profile := \"sym-fragment-v1\""));
        assert!(lean.contains("result := .float"));
        assert!(lean.contains(".prim .floatAdd [0, 1]"));
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

    #[test]
    fn sym_plan_rejects_raw_wasm_value_fragment() {
        let plan = ExprFragmentPlan {
            params: vec![FragTy::I64],
            result: FragTy::I64,
            body: FragBlock {
                nodes: vec![FragNode {
                    id: FragValueId(0),
                    ty: FragTy::I64,
                    kind: FragNodeKind::Local { index: 0 },
                }],
                result: FragValueId(0),
            },
        };

        assert!(SymPlan::from_expr_fragment_source_subset(&plan).is_none());
    }

    #[test]
    fn sym_plan_models_string_concat_without_expr_encoding() {
        let plan = SymPlan {
            params: vec![SymTy::String],
            result: SymTy::String,
            body: SymBlock {
                nodes: vec![
                    SymNode {
                        id: SymValueId(0),
                        ty: SymTy::String,
                        kind: SymNodeKind::Param { index: 0 },
                    },
                    SymNode {
                        id: SymValueId(1),
                        ty: SymTy::String,
                        kind: SymNodeKind::ConstStringBytes(vec![33]),
                    },
                    SymNode {
                        id: SymValueId(2),
                        ty: SymTy::String,
                        kind: SymNodeKind::Prim {
                            op: SymPrim::StringConcat,
                            args: vec![SymValueId(0), SymValueId(1)],
                        },
                    },
                ],
                result: SymValueId(2),
            },
        };

        let lean = sym_plan_lean_value(&plan);
        assert!(lean.contains(".constStringBytes [33]"));
        assert!(lean.contains(".prim .stringConcat [0, 1]"));
        assert!(plan.to_expr_fragment_plan().is_none());
    }
}
