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

fn sym_fragment_sidecar(name: &str, plan: &SymPlan) -> FragmentPlanSidecar {
    let text = sym_fragment_plan_text(plan);
    FragmentPlanSidecar {
        path: sym_fragment_plan_path(name),
        sha256: sha256_hex(text.as_bytes()),
        text,
    }
}

fn sym_fragment_plan_path(name: &str) -> String {
    format!("fragments/{}.sym-fragment-v1.plan", hex(name.as_bytes()))
}

fn sym_fragment_plan_text(plan: &SymPlan) -> String {
    let mut out = String::new();
    out.push_str("aver.sym-fragment.plan.v1\n");
    out.push_str("profile sym-fragment-v1\n");
    out.push_str("params");
    for ty in &plan.params {
        out.push(' ');
        out.push_str(ty.plan_tag());
    }
    out.push('\n');
    out.push_str(&format!("result {}\n", plan.result.plan_tag()));
    out.push_str("body\n");
    render_sym_block_plan(&plan.body, 0, &mut out);
    out
}

fn sym_plan_lean_value(plan: &SymPlan) -> String {
    format!(
        "{{ profile := \"sym-fragment-v1\", params := [{}], result := {}, body := {} }}",
        plan.params
            .iter()
            .map(|ty| ty.lean_plan_ctor())
            .collect::<Vec<_>>()
            .join(", "),
        plan.result.lean_plan_ctor(),
        sym_block_lean_value(&plan.body)
    )
}

impl SymTy {
    fn lean_plan_ctor(self) -> &'static str {
        match self {
            SymTy::Int => ".int",
            SymTy::Float => ".float",
            SymTy::Bool => ".bool",
            SymTy::String => ".string",
        }
    }
}

impl SymPrim {
    fn to_frag_prim(self) -> FragPrim {
        match self {
            SymPrim::FloatAdd => FragPrim::F64Add,
            SymPrim::FloatMul => FragPrim::F64Mul,
            SymPrim::FloatLe => FragPrim::F64Le,
        }
    }

    fn plan_tag(self) -> &'static str {
        match self {
            SymPrim::FloatAdd => "float.add",
            SymPrim::FloatMul => "float.mul",
            SymPrim::FloatLe => "float.le",
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "float.add" => Some(SymPrim::FloatAdd),
            "float.mul" => Some(SymPrim::FloatMul),
            "float.le" => Some(SymPrim::FloatLe),
            _ => None,
        }
    }

    fn lean_plan_ctor(self) -> &'static str {
        match self {
            SymPrim::FloatAdd => ".floatAdd",
            SymPrim::FloatMul => ".floatMul",
            SymPrim::FloatLe => ".floatLe",
        }
    }
}

impl SymIntCmp {
    fn plan_tag(self) -> &'static str {
        match self {
            SymIntCmp::Eq => "int.eq",
            SymIntCmp::Lt => "int.lt",
            SymIntCmp::Le => "int.le",
            SymIntCmp::Ge => "int.ge",
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "int.eq" => Some(SymIntCmp::Eq),
            "int.lt" => Some(SymIntCmp::Lt),
            "int.le" => Some(SymIntCmp::Le),
            "int.ge" => Some(SymIntCmp::Ge),
            _ => None,
        }
    }

    fn lean_plan_ctor(self) -> &'static str {
        match self {
            SymIntCmp::Eq => ".eq",
            SymIntCmp::Lt => ".lt",
            SymIntCmp::Le => ".le",
            SymIntCmp::Ge => ".ge",
        }
    }
}

fn sym_block_lean_value(block: &SymBlock) -> String {
    format!(
        "({{ nodes := [{}], result := {} }} : SymBlock)",
        block
            .nodes
            .iter()
            .map(sym_node_lean_value)
            .collect::<Vec<_>>()
            .join(", "),
        block.result.0
    )
}

fn sym_node_lean_value(node: &SymNode) -> String {
    format!(
        "{{ id := {}, ty := {}, kind := {} }}",
        node.id.0,
        node.ty.lean_plan_ctor(),
        sym_node_kind_lean_value(&node.kind)
    )
}

fn sym_node_kind_lean_value(kind: &SymNodeKind) -> String {
    match kind {
        SymNodeKind::Param { index } => format!(".param {index}"),
        SymNodeKind::ConstBool(value) => format!(".constBool {value}"),
        SymNodeKind::ConstFloatBits(bits) => format!(".constFloatBits 0x{bits:016x}"),
        SymNodeKind::Prim { op, args } => format!(
            ".prim {} [{}]",
            op.lean_plan_ctor(),
            args.iter()
                .map(|id| id.0.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
        SymNodeKind::IntConstCmp {
            op,
            value,
            constant,
        } => format!(
            ".intConstCmp {} {} ({} : Int)",
            op.lean_plan_ctor(),
            value.0,
            constant
        ),
        SymNodeKind::If {
            cond,
            then_block,
            else_block,
        } => format!(
            ".ifElse {} {} {}",
            cond.0,
            sym_block_lean_value(then_block),
            sym_block_lean_value(else_block)
        ),
    }
}

fn render_sym_block_plan(block: &SymBlock, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!("{pad}block result=v{}\n", block.result.0));
    for node in &block.nodes {
        render_sym_node_plan(node, indent + 1, out);
    }
    out.push_str(&format!("{pad}end\n"));
}

fn render_sym_node_plan(node: &SymNode, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!("{pad}v{} ty={} ", node.id.0, node.ty.plan_tag()));
    match &node.kind {
        SymNodeKind::Param { index } => {
            out.push_str(&format!("param index={index}\n"));
        }
        SymNodeKind::ConstBool(value) => {
            out.push_str(&format!("const.bool value={value}\n"));
        }
        SymNodeKind::ConstFloatBits(bits) => {
            out.push_str(&format!("const.float bits=0x{bits:016x}\n"));
        }
        SymNodeKind::Prim { op, args } => {
            out.push_str(&format!(
                "prim op={} args={}\n",
                op.plan_tag(),
                render_sym_plan_ids(args)
            ));
        }
        SymNodeKind::IntConstCmp {
            op,
            value,
            constant,
        } => {
            out.push_str(&format!(
                "int.const-cmp op={} value=v{} constant={}\n",
                op.plan_tag(),
                value.0,
                constant
            ));
        }
        SymNodeKind::If {
            cond,
            then_block,
            else_block,
        } => {
            out.push_str(&format!("if cond=v{}\n", cond.0));
            out.push_str(&format!("{pad}then\n"));
            render_sym_block_plan(then_block, indent + 1, out);
            out.push_str(&format!("{pad}else\n"));
            render_sym_block_plan(else_block, indent + 1, out);
            out.push_str(&format!("{pad}endif\n"));
        }
    }
}

fn render_sym_plan_ids(args: &[SymValueId]) -> String {
    args.iter()
        .map(|id| format!("v{}", id.0))
        .collect::<Vec<_>>()
        .join(",")
}

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
            SymNodeKind::Prim { op, args } => {
                let args = args
                    .iter()
                    .map(|id| self.sym_to_frag.get(id.0).copied())
                    .collect::<Option<Vec<_>>>()?;
                self.push_node(
                    ty,
                    FragNodeKind::Prim {
                        op: op.to_frag_prim(),
                        args,
                    },
                )
            }
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
    Some(FragBlock {
        nodes,
        result,
    })
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
}
