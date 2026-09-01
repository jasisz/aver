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
        out.push_str(&ty.plan_tag());
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
    fn lean_plan_ctor(&self) -> String {
        match self {
            SymTy::Int => ".int".to_string(),
            SymTy::Float => ".float".to_string(),
            SymTy::Bool => ".bool".to_string(),
            SymTy::String => ".string".to_string(),
            SymTy::Named(name) => format!("(.named {})", lean_str(name)),
            SymTy::App(name, args) if args.len() == 1 => format!(
                "(.app1 {} {})",
                lean_str(name),
                args[0].lean_plan_ctor()
            ),
            SymTy::App(name, args) if args.len() == 2 => format!(
                "(.app2 {} {} {})",
                lean_str(name),
                args[0].lean_plan_ctor(),
                args[1].lean_plan_ctor()
            ),
            SymTy::App(_, _) => unreachable!("source type parser emits unary/binary apps only"),
        }
    }
}

impl SymPrim {
    fn plan_tag(self) -> &'static str {
        match self {
            SymPrim::FloatAdd => "float.add",
            SymPrim::FloatMul => "float.mul",
            SymPrim::FloatLe => "float.le",
            SymPrim::FloatGe => "float.ge",
            SymPrim::FloatLt => "float.lt",
            SymPrim::FloatGt => "float.gt",
            SymPrim::FloatEq => "float.eq",
            SymPrim::IntAdd => "int.add",
            SymPrim::IntSub => "int.sub",
            SymPrim::IntMul => "int.mul",
            SymPrim::StringEq => "string.eq",
            SymPrim::StringConcat => "string.concat",
            SymPrim::BoolAnd => "bool.and",
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "float.add" => Some(SymPrim::FloatAdd),
            "float.mul" => Some(SymPrim::FloatMul),
            "float.le" => Some(SymPrim::FloatLe),
            "float.ge" => Some(SymPrim::FloatGe),
            "float.lt" => Some(SymPrim::FloatLt),
            "float.gt" => Some(SymPrim::FloatGt),
            "float.eq" => Some(SymPrim::FloatEq),
            "int.add" => Some(SymPrim::IntAdd),
            "int.sub" => Some(SymPrim::IntSub),
            "int.mul" => Some(SymPrim::IntMul),
            "string.eq" => Some(SymPrim::StringEq),
            "string.concat" => Some(SymPrim::StringConcat),
            "bool.and" => Some(SymPrim::BoolAnd),
            _ => None,
        }
    }

    fn lean_plan_ctor(self) -> &'static str {
        match self {
            SymPrim::FloatAdd => ".floatAdd",
            SymPrim::FloatMul => ".floatMul",
            SymPrim::FloatLe => ".floatLe",
            SymPrim::FloatGe => ".floatGe",
            SymPrim::FloatLt => ".floatLt",
            SymPrim::FloatGt => ".floatGt",
            SymPrim::FloatEq => ".floatEq",
            SymPrim::IntAdd => ".intAdd",
            SymPrim::IntSub => ".intSub",
            SymPrim::IntMul => ".intMul",
            SymPrim::StringEq => ".stringEq",
            SymPrim::StringConcat => ".stringConcat",
            SymPrim::BoolAnd => ".boolAnd",
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
            SymIntCmp::Gt => "int.gt",
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "int.eq" => Some(SymIntCmp::Eq),
            "int.lt" => Some(SymIntCmp::Lt),
            "int.le" => Some(SymIntCmp::Le),
            "int.ge" => Some(SymIntCmp::Ge),
            "int.gt" => Some(SymIntCmp::Gt),
            _ => None,
        }
    }

    fn lean_plan_ctor(self) -> &'static str {
        match self {
            SymIntCmp::Eq => ".eq",
            SymIntCmp::Lt => ".lt",
            SymIntCmp::Le => ".le",
            SymIntCmp::Ge => ".ge",
            SymIntCmp::Gt => ".gt",
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
        SymNodeKind::ConstInt(value) => format!(".constInt ({value} : Int)"),
        SymNodeKind::ConstFloatBits(bits) => format!(".constFloatBits 0x{bits:016x}"),
        SymNodeKind::ConstStringBytes(bytes) => {
            format!(".constStringBytes {}", render_byte_list(bytes))
        }
        SymNodeKind::Prim { op, args } => format!(
            ".prim {} [{}]",
            op.lean_plan_ctor(),
            args.iter()
                .map(|id| id.0.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
        SymNodeKind::Construct {
            type_name,
            ctor_name,
            args,
        } => format!(
            ".construct {} {} [{}]",
            lean_str(type_name),
            lean_str(ctor_name),
            args.iter()
                .map(|id| id.0.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
        SymNodeKind::EmptyList { elem_ty } => {
            format!(".emptyList {}", elem_ty.lean_plan_ctor())
        }
        SymNodeKind::ProjectField {
            type_name,
            field,
            field_ty,
            value,
        } => format!(
            ".projectField {} {field} {} {}",
            lean_str(type_name),
            field_ty.lean_plan_ctor(),
            value.0
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
        SymNodeKind::IntCmp { op, lhs, rhs } => format!(
            ".intCmp {} {} {}",
            op.lean_plan_ctor(),
            lhs.0,
            rhs.0
        ),
        SymNodeKind::TagMatch {
            type_name,
            scrutinee,
            tag,
            hit,
            miss,
        } => format!(
            ".tagMatch {} {} ({} : Int) {} {}",
            lean_str(type_name),
            scrutinee.0,
            tag,
            sym_block_lean_value(hit),
            sym_block_lean_value(miss)
        ),
        SymNodeKind::VectorGetOrDefault { type_name, default } => format!(
            ".vectorGetOrDefault {} ({} : Int)",
            lean_str(type_name),
            default
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
        SymNodeKind::ConstInt(value) => {
            out.push_str(&format!("const.int value={value}\n"));
        }
        SymNodeKind::ConstFloatBits(bits) => {
            out.push_str(&format!("const.float bits=0x{bits:016x}\n"));
        }
        SymNodeKind::ConstStringBytes(bytes) => {
            out.push_str(&format!("const.string hex={}\n", hex(bytes)));
        }
        SymNodeKind::Prim { op, args } => {
            out.push_str(&format!(
                "prim op={} args={}\n",
                op.plan_tag(),
                render_sym_plan_ids(args)
            ));
        }
        SymNodeKind::EmptyList { elem_ty } => {
            out.push_str(&format!("empty.list elem={}\n", elem_ty.plan_tag()));
        }
        SymNodeKind::Construct {
            type_name,
            ctor_name,
            args,
        } => {
            out.push_str(&format!(
                "construct type={} ctor={} args={}\n",
                type_name,
                ctor_name,
                render_sym_plan_ids(args)
            ));
        }
        SymNodeKind::ProjectField {
            type_name,
            field,
            // The node line's `ty=` tag carries the field type; the parser
            // reads it back from there.
            field_ty: _,
            value,
        } => {
            out.push_str(&format!(
                "project.field type={type_name} field={field} value=v{}\n",
                value.0
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
        SymNodeKind::IntCmp { op, lhs, rhs } => {
            out.push_str(&format!(
                "int.cmp op={} lhs=v{} rhs=v{}\n",
                op.plan_tag(),
                lhs.0,
                rhs.0
            ));
        }
        SymNodeKind::TagMatch {
            type_name,
            scrutinee,
            tag,
            hit,
            miss,
        } => {
            out.push_str(&format!(
                "tag.match type={type_name} scrutinee=v{} tag={tag}\n",
                scrutinee.0
            ));
            out.push_str(&format!("{pad}hit\n"));
            render_sym_block_plan(hit, indent + 1, out);
            out.push_str(&format!("{pad}miss\n"));
            render_sym_block_plan(miss, indent + 1, out);
            out.push_str(&format!("{pad}endtag\n"));
        }
        SymNodeKind::VectorGetOrDefault { type_name, default } => {
            out.push_str(&format!(
                "vector.get_or_default type={type_name} default={default}\n"
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
