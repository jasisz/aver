/// Source-level certificate plan. Unlike `ExprFragmentPlan`, this IR talks in
/// Aver semantic types and operations first; target representation only enters
/// later through a checked encoder/lowerer.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SymTy {
    Int,
    Float,
    Bool,
    String,
    Named(String),
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

    fn to_frag_ty(&self) -> Option<FragTy> {
        match self {
            SymTy::Int => Some(FragTy::IntCarrier),
            SymTy::Float => Some(FragTy::F64),
            SymTy::Bool => Some(FragTy::BoolI32),
            SymTy::String | SymTy::Named(_) => None,
        }
    }

    fn plan_tag(&self) -> String {
        match self {
            SymTy::Int => "int".to_string(),
            SymTy::Float => "float".to_string(),
            SymTy::Bool => "bool".to_string(),
            SymTy::String => "string".to_string(),
            SymTy::Named(name) => format!("named:{name}"),
        }
    }

    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "int" => Some(SymTy::Int),
            "float" => Some(SymTy::Float),
            "bool" => Some(SymTy::Bool),
            "string" => Some(SymTy::String),
            _ => tag
                .strip_prefix("named:")
                .filter(|name| !name.is_empty() && !name.chars().any(char::is_whitespace))
                .map(|name| SymTy::Named(name.to_string())),
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
    IntAdd,
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
    ConstInt(i64),
    ConstFloatBits(u64),
    ConstStringBytes(Vec<u8>),
    Prim {
        op: SymPrim,
        args: Vec<SymValueId>,
    },
    Construct {
        type_name: String,
        ctor_name: String,
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

    /// Encode to the representation-level plan. `host_table` supplies the
    /// byte-derived wasm indices for host-bound source operations (Int
    /// literals box; `intAdd` calls the carrier `add`); a role the table
    /// lacks fail-closes the encoding. Twin of the audited Lean
    /// `PlanCheck.encodeSymRawPlanToExprFragmentRawPlan`.
    pub(crate) fn to_expr_fragment_plan(
        &self,
        host_table: &FragHostTable,
    ) -> Option<ExprFragmentPlan> {
        Some(ExprFragmentPlan {
            params: self
                .params
                .iter()
                .map(SymTy::to_frag_ty)
                .collect::<Option<Vec<_>>>()?,
            result: self.result.to_frag_ty()?,
            body: expr_fragment_block_from_sym(&self.body, host_table)?,
        })
    }
}

#[derive(Clone, Debug)]
pub enum FragmentPlan {
    Sym(SymPlan),
    Expr(ExprFragmentPlan),
}

impl FragmentPlan {
    pub(crate) fn to_expr_fragment_plan(
        &self,
        host_table: &FragHostTable,
    ) -> Option<ExprFragmentPlan> {
        match self {
            FragmentPlan::Sym(plan) => plan.to_expr_fragment_plan(host_table),
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

fn adt_constructor_sym_plan_from_cert(c: &Cert, model_info: &ModelInfo) -> Option<SymPlan> {
    if !adt_constructor_uses_model(c, model_info) {
        return None;
    }
    let Cert::AdtConstructor {
        name,
        arity,
        fields,
        ..
    } = c.inner()
    else {
        return None;
    };
    let sig = model_info.fns.get(name)?;
    if sig.params.len() != *arity {
        return None;
    }
    let ind = model_info.inductives.get(&sig.ret)?;
    let ctor = ind.ctors.first()?;
    if ctor.fields != sig.params {
        return None;
    }
    let params = sig
        .params
        .iter()
        .map(|ty| sym_ty_from_source_type_name(ty))
        .collect::<Option<Vec<_>>>()?;
    let result = sym_ty_from_source_type_name(&sig.ret)?;
    let SymTy::Named(type_name) = result.clone() else {
        return None;
    };
    if !sym_plan_simple_token(&type_name) || !sym_plan_simple_token(&ctor.name) {
        return None;
    }
    let mut nodes = params
        .iter()
        .enumerate()
        .map(|(i, ty)| SymNode {
            id: SymValueId(i),
            ty: ty.clone(),
            kind: SymNodeKind::Param { index: i as u32 },
        })
        .collect::<Vec<_>>();
    let args = fields
        .iter()
        .map(|field| match field {
            ConstructorField::Local(i) if (*i as usize) < params.len() => {
                Some(SymValueId(*i as usize))
            }
            ConstructorField::Local(_) | ConstructorField::Null => None,
        })
        .collect::<Option<Vec<_>>>()?;
    let result_id = SymValueId(nodes.len());
    nodes.push(SymNode {
        id: result_id,
        ty: result,
        kind: SymNodeKind::Construct {
            type_name,
            ctor_name: ctor.name.clone(),
            args,
        },
    });
    Some(SymPlan {
        params,
        result: sym_ty_from_source_type_name(&sig.ret)?,
        body: SymBlock {
            nodes,
            result: result_id,
        },
    })
}

fn sym_ty_from_source_type_name(ty: &str) -> Option<SymTy> {
    let ty = ty.trim();
    match ty {
        "Int" => Some(SymTy::Int),
        "Float" => Some(SymTy::Float),
        "Bool" => Some(SymTy::Bool),
        "String" => Some(SymTy::String),
        _ if sym_plan_simple_token(ty) => Some(SymTy::Named(ty.to_string())),
        _ => None,
    }
}

fn sym_plan_simple_token(value: &str) -> bool {
    !value.is_empty() && !value.chars().any(char::is_whitespace) && !value.contains('=')
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
        | FragNodeKind::HostCall { .. }
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
    fn sym_plan_named_types_roundtrip_but_do_not_encode_to_expr_fragment() {
        let named = SymTy::Named("User".to_string());
        assert_eq!(named.plan_tag(), "named:User");
        assert_eq!(SymTy::from_plan_tag("named:User"), Some(named.clone()));
        assert_eq!(named.to_frag_ty(), None);

        let plan = SymPlan {
            params: vec![named.clone()],
            result: SymTy::String,
            body: SymBlock {
                nodes: vec![
                    SymNode {
                        id: SymValueId(0),
                        ty: named,
                        kind: SymNodeKind::Param { index: 0 },
                    },
                    SymNode {
                        id: SymValueId(1),
                        ty: SymTy::String,
                        kind: SymNodeKind::ConstStringBytes(b"Ada".to_vec()),
                    },
                ],
                result: SymValueId(1),
            },
        };
        let lean = sym_plan_lean_value(&plan);
        assert!(lean.contains("params := [(.named \"User\")]"));
        assert!(plan.to_expr_fragment_plan(&FragHostTable::placeholder()).is_none());
    }

    #[test]
    fn sym_plan_construct_roundtrips_but_does_not_encode_to_expr_fragment() {
        let plan = SymPlan {
            params: vec![SymTy::Int],
            result: SymTy::Named("Op".to_string()),
            body: SymBlock {
                nodes: vec![
                    SymNode {
                        id: SymValueId(0),
                        ty: SymTy::Int,
                        kind: SymNodeKind::Param { index: 0 },
                    },
                    SymNode {
                        id: SymValueId(1),
                        ty: SymTy::Named("Op".to_string()),
                        kind: SymNodeKind::Construct {
                            type_name: "Op".to_string(),
                            ctor_name: "Add".to_string(),
                            args: vec![SymValueId(0)],
                        },
                    },
                ],
                result: SymValueId(1),
            },
        };

        let text = sym_fragment_plan_text(&plan);
        assert!(text.contains("result named:Op"));
        assert!(text.contains("construct type=Op ctor=Add args=v0"));

        let lean = sym_plan_lean_value(&plan);
        assert!(lean.contains("result := (.named \"Op\")"));
        assert!(lean.contains(".construct \"Op\" \"Add\" [0]"));

        let mut parser = SymPlanParser::new(&text, vec![SymTy::Int], SymTy::Named("Op".to_string()));
        let parsed = parser.parse().expect("parse construct plan");
        assert_eq!(parsed, plan.body);
        assert!(plan.to_expr_fragment_plan(&FragHostTable::placeholder()).is_none());
    }

    #[test]
    fn adt_constructor_cert_projects_to_source_construct_plan() {
        let cert = Cert::AdtConstructor {
            name: "mkOp".to_string(),
            self_idx: 1,
            code_idx: 1,
            type_idx: 1,
            nlocals: 0,
            carrier: 0,
            struct_idx: 3,
            field_count: 1,
            arity: 1,
            fields: vec![ConstructorField::Local(0)],
            ops: Vec::new(),
        };
        let mut model_info = ModelInfo::default();
        model_info.fns.insert(
            "mkOp".to_string(),
            FnSig {
                params: vec!["Int".to_string()],
                ret: "Op".to_string(),
            },
        );
        model_info.inductives.insert(
            "Op".to_string(),
            InductiveInfo {
                ctors: vec![CtorInfo {
                    name: "Add".to_string(),
                    fields: vec!["Int".to_string()],
                }],
            },
        );

        let plan = adt_constructor_sym_plan_from_cert(&cert, &model_info)
            .expect("project constructor to SymPlan");
        assert_eq!(plan.params, vec![SymTy::Int]);
        assert_eq!(plan.result, SymTy::Named("Op".to_string()));
        assert!(matches!(
            &plan.body.nodes[1].kind,
            SymNodeKind::Construct {
                type_name,
                ctor_name,
                args,
            } if type_name == "Op" && ctor_name == "Add" && args == &[SymValueId(0)]
        ));
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
        assert!(plan.to_expr_fragment_plan(&FragHostTable::placeholder()).is_none());
    }
}
