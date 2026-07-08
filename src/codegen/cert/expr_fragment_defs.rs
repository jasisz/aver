/// Typed, ordered non-recursive expression fragment. The compiler emits this as
/// an untrusted plan sidecar, and the verifier checks/canonically lowers it
/// before using it as a certificate witness. Every value has an explicit type
/// and a defining node.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FragTy {
    F64,
    BoolI32,
    IntCarrier,
    I64,
    RawI32,
    Ref,
}

impl FragTy {
    fn plan_tag(self) -> &'static str {
        match self {
            FragTy::F64 => "f64",
            FragTy::BoolI32 => "bool-i32",
            FragTy::IntCarrier => "int-carrier",
            FragTy::I64 => "i64",
            FragTy::RawI32 => "raw-i32",
            FragTy::Ref => "ref",
        }
    }

    fn lean_plan_ctor(self) -> &'static str {
        match self {
            FragTy::F64 => ".f64",
            FragTy::BoolI32 => ".boolI32",
            FragTy::IntCarrier => ".intCarrier",
            FragTy::I64 => ".i64",
            FragTy::RawI32 => ".rawI32",
            FragTy::Ref => ".ref",
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "f64" => Some(FragTy::F64),
            "bool-i32" => Some(FragTy::BoolI32),
            "int-carrier" => Some(FragTy::IntCarrier),
            "i64" => Some(FragTy::I64),
            "raw-i32" => Some(FragTy::RawI32),
            "ref" => Some(FragTy::Ref),
            _ => None,
        }
    }

    fn source_name(self) -> &'static str {
        match self {
            FragTy::F64 => "Float",
            FragTy::BoolI32 => "Bool",
            FragTy::IntCarrier => "Int",
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref => "WVal",
        }
    }

    fn lean_dom_type(self) -> &'static str {
        match self {
            FragTy::F64 => "UInt64",
            FragTy::BoolI32 => "Bool",
            FragTy::IntCarrier => "Int",
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref => "WVal",
        }
    }

    fn lean_arg_repr(self, name: &str, carrier: &str) -> String {
        match self {
            FragTy::F64 => format!(".f64v {name}"),
            FragTy::BoolI32 => format!("b32 {name}"),
            FragTy::IntCarrier => format!("carrierSmall {carrier} {name}"),
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref => name.to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct FragValueId(pub(crate) usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum FragPrim {
    F64Add,
    F64Mul,
    F64Le,
    I64Eq,
    I64LeS,
    I64LtS,
    I64GeS,
    I32LtS,
    I32GtS,
}

impl FragPrim {
    fn plan_tag(self) -> &'static str {
        match self {
            FragPrim::F64Add => "f64.add",
            FragPrim::F64Mul => "f64.mul",
            FragPrim::F64Le => "f64.le",
            FragPrim::I64Eq => "i64.eq",
            FragPrim::I64LeS => "i64.le_s",
            FragPrim::I64LtS => "i64.lt_s",
            FragPrim::I64GeS => "i64.ge_s",
            FragPrim::I32LtS => "i32.lt_s",
            FragPrim::I32GtS => "i32.gt_s",
        }
    }

    fn lean_plan_ctor(self) -> &'static str {
        match self {
            FragPrim::F64Add => ".f64Add",
            FragPrim::F64Mul => ".f64Mul",
            FragPrim::F64Le => ".f64Le",
            FragPrim::I64Eq => ".i64Eq",
            FragPrim::I64LeS => ".i64LeS",
            FragPrim::I64LtS => ".i64LtS",
            FragPrim::I64GeS => ".i64GeS",
            FragPrim::I32LtS => ".i32LtS",
            FragPrim::I32GtS => ".i32GtS",
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum FragNodeKind {
    Local { index: u32 },
    ConstBool(bool),
    ConstI64(i64),
    ConstI32(i32),
    ConstF64(u64),
    StructGet {
        field: u32,
        receiver: FragValueId,
    },
    RefIsNull {
        value: FragValueId,
    },
    Prim {
        op: FragPrim,
        args: Vec<FragValueId>,
    },
    If {
        cond: FragValueId,
        then_block: Box<FragBlock>,
        else_block: Box<FragBlock>,
    },
}

#[derive(Clone, Debug)]
pub(crate) struct FragNode {
    pub(crate) id: FragValueId,
    pub(crate) ty: FragTy,
    pub(crate) kind: FragNodeKind,
}

#[derive(Clone, Debug)]
pub(crate) struct FragBlock {
    pub(crate) nodes: Vec<FragNode>,
    pub(crate) result: FragValueId,
}

impl FragBlock {
    fn node(&self, id: FragValueId) -> Option<&FragNode> {
        self.nodes.get(id.0).filter(|node| node.id == id)
    }

    fn result_ty(&self) -> Option<FragTy> {
        self.node(self.result).map(|node| node.ty)
    }
}

#[derive(Clone, Debug)]
pub struct ExprFragmentPlan {
    pub(crate) params: Vec<FragTy>,
    pub(crate) result: FragTy,
    pub(crate) body: FragBlock,
}

impl ExprFragmentPlan {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn source_dom(&self) -> String {
        self.params
            .iter()
            .map(|ty| ty.source_name())
            .collect::<Vec<_>>()
            .join(" x ")
    }

    fn source_cod(&self) -> String {
        self.result.source_name().to_string()
    }
}

#[derive(Clone, Debug)]
pub struct ExprFragmentPlanArtifact {
    pub export_name: String,
    pub plan: ExprFragmentPlan,
}

#[derive(Clone, Debug)]
pub struct FragmentPlanSidecar {
    pub path: String,
    pub sha256: String,
    pub text: String,
}

fn expr_fragment_sidecar(name: &str, plan: &ExprFragmentPlan) -> FragmentPlanSidecar {
    let text = expr_fragment_plan_text(plan);
    FragmentPlanSidecar {
        path: expr_fragment_plan_path(name),
        sha256: sha256_hex(text.as_bytes()),
        text,
    }
}

fn expr_fragment_plan_path(name: &str) -> String {
    format!(
        "fragments/{}.expr-fragment-v1.plan",
        hex(name.as_bytes())
    )
}

fn expr_fragment_plan_text(plan: &ExprFragmentPlan) -> String {
    let mut out = String::new();
    out.push_str("aver.expr-fragment.plan.v1\n");
    out.push_str("profile expr-fragment-v1\n");
    out.push_str("params");
    for ty in &plan.params {
        out.push(' ');
        out.push_str(ty.plan_tag());
    }
    out.push('\n');
    out.push_str(&format!("result {}\n", plan.result.plan_tag()));
    out.push_str("body\n");
    render_fragment_block_plan(&plan.body, 0, &mut out);
    out
}

fn expr_fragment_plan_lean_value(plan: &ExprFragmentPlan) -> String {
    format!(
        "{{ profile := \"expr-fragment-v1\", params := [{}], result := {}, body := {} }}",
        plan.params
            .iter()
            .map(|ty| ty.lean_plan_ctor())
            .collect::<Vec<_>>()
            .join(", "),
        plan.result.lean_plan_ctor(),
        expr_fragment_block_lean_value(&plan.body)
    )
}

fn expr_fragment_block_lean_value(block: &FragBlock) -> String {
    format!(
        "({{ nodes := [{}], result := {} }} : FragBlock)",
        block
            .nodes
            .iter()
            .map(expr_fragment_node_lean_value)
            .collect::<Vec<_>>()
            .join(", "),
        block.result.0
    )
}

fn expr_fragment_node_lean_value(node: &FragNode) -> String {
    format!(
        "{{ id := {}, ty := {}, kind := {} }}",
        node.id.0,
        node.ty.lean_plan_ctor(),
        expr_fragment_node_kind_lean_value(&node.kind)
    )
}

fn expr_fragment_node_kind_lean_value(kind: &FragNodeKind) -> String {
    match kind {
        FragNodeKind::Local { index } => format!(".local {index}"),
        FragNodeKind::ConstBool(value) => format!(".constBool {value}"),
        FragNodeKind::ConstI64(value) => format!(".constI64 ({value} : Int)"),
        FragNodeKind::ConstI32(value) => format!(".constI32 ({value} : Int)"),
        FragNodeKind::ConstF64(bits) => format!(".constF64Bits 0x{bits:016x}"),
        FragNodeKind::StructGet { field, receiver } => {
            format!(".structGet {field} {}", receiver.0)
        }
        FragNodeKind::RefIsNull { value } => format!(".refIsNull {}", value.0),
        FragNodeKind::Prim { op, args } => format!(
            ".prim {} [{}]",
            op.lean_plan_ctor(),
            args.iter()
                .map(|id| id.0.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
        FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } => format!(
            ".ifElse {} {} {}",
            cond.0,
            expr_fragment_block_lean_value(then_block),
            expr_fragment_block_lean_value(else_block)
        ),
    }
}

fn render_fragment_block_plan(block: &FragBlock, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!("{pad}block result=v{}\n", block.result.0));
    for node in &block.nodes {
        render_fragment_node_plan(node, indent + 1, out);
    }
    out.push_str(&format!("{pad}end\n"));
}

fn render_fragment_node_plan(node: &FragNode, indent: usize, out: &mut String) {
    let pad = "  ".repeat(indent);
    out.push_str(&format!(
        "{pad}v{} ty={} ",
        node.id.0,
        node.ty.plan_tag()
    ));
    match &node.kind {
        FragNodeKind::Local { index } => {
            out.push_str(&format!("local index={index}\n"));
        }
        FragNodeKind::ConstBool(v) => {
            out.push_str(&format!("const.bool value={v}\n"));
        }
        FragNodeKind::ConstI64(v) => {
            out.push_str(&format!("const.i64 value={v}\n"));
        }
        FragNodeKind::ConstI32(v) => {
            out.push_str(&format!("const.i32 value={v}\n"));
        }
        FragNodeKind::ConstF64(bits) => {
            out.push_str(&format!("const.f64 bits=0x{bits:016x}\n"));
        }
        FragNodeKind::StructGet { field, receiver } => {
            out.push_str(&format!("struct.get field={field} receiver=v{}\n", receiver.0));
        }
        FragNodeKind::RefIsNull { value } => {
            out.push_str(&format!("ref.is_null value=v{}\n", value.0));
        }
        FragNodeKind::Prim { op, args } => {
            out.push_str(&format!(
                "prim op={} args={}\n",
                op.plan_tag(),
                render_fragment_plan_ids(args)
            ));
        }
        FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } => {
            out.push_str(&format!("if cond=v{}\n", cond.0));
            out.push_str(&format!("{pad}then\n"));
            render_fragment_block_plan(then_block, indent + 1, out);
            out.push_str(&format!("{pad}else\n"));
            render_fragment_block_plan(else_block, indent + 1, out);
            out.push_str(&format!("{pad}endif\n"));
        }
    }
}

fn render_fragment_plan_ids(args: &[FragValueId]) -> String {
    args.iter()
        .map(|id| format!("v{}", id.0))
        .collect::<Vec<_>>()
        .join(",")
}

fn expr_fragment_dom_type(params: &[FragTy]) -> String {
    match params {
        [] => "Unit".to_string(),
        [single] => single.lean_dom_type().to_string(),
        many => many
            .iter()
            .map(|ty| ty.lean_dom_type())
            .collect::<Vec<_>>()
            .join(" × "),
    }
}

fn expr_fragment_dom_accessor(root: &str, index: usize, len: usize) -> String {
    if len <= 1 {
        return root.to_string();
    }
    if index == 0 {
        return format!("{root}.1");
    }
    expr_fragment_dom_accessor(&format!("{root}.2"), index - 1, len - 1)
}

fn expr_fragment_dom_repr_list(params: &[FragTy], root: &str, carrier: &str) -> String {
    let args = params
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            let access = expr_fragment_dom_accessor(root, i, params.len());
            ty.lean_arg_repr(&access, carrier)
        })
        .collect::<Vec<_>>();
    format!("[{}]", args.join(", "))
}
