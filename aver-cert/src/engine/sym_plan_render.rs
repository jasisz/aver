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
