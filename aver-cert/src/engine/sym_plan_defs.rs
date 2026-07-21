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
    App(String, Vec<SymTy>),
}

impl SymTy {
    /// Canonical source spelling used by the wasm-gc monomorphisation
    /// registry. This is representation metadata only; it does not turn an
    /// operational tag dispatch into a source-constructor claim.
    pub fn canonical_name(&self) -> String {
        match self {
            SymTy::Int => "Int".to_string(),
            SymTy::Float => "Float".to_string(),
            SymTy::Bool => "Bool".to_string(),
            SymTy::String => "String".to_string(),
            SymTy::Named(name) => name.clone(),
            SymTy::App(name, args) => format!(
                "{name}<{}>",
                args.iter()
                    .map(SymTy::canonical_name)
                    .collect::<Vec<_>>()
                    .join(",")
            ),
        }
    }

    fn from_frag_ty(value: FragTy) -> Option<Self> {
        match value {
            FragTy::F64 => Some(SymTy::Float),
            FragTy::BoolI32 => Some(SymTy::Bool),
            FragTy::IntCarrier => Some(SymTy::Int),
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref | FragTy::AdtRef => None,
        }
    }

    fn to_frag_ty(&self) -> Option<FragTy> {
        match self {
            SymTy::Int => Some(FragTy::IntCarrier),
            SymTy::Float => Some(FragTy::F64),
            SymTy::Bool => Some(FragTy::BoolI32),
            // Strings and named user types are whole references at the
            // representation level: both encode to the opaque `adtRef` (twin
            // of the Lean `encodeSymTy?`). String OPERATIONS still do not
            // encode; this only lets reference-typed values flow verbatim
            // through the field-projection face.
            SymTy::String | SymTy::Named(_) | SymTy::App(_, _) => Some(FragTy::AdtRef),
        }
    }

    #[cfg(feature = "engine")]
    fn plan_tag(&self) -> String {
        match self {
            SymTy::Int => "int".to_string(),
            SymTy::Float => "float".to_string(),
            SymTy::Bool => "bool".to_string(),
            SymTy::String => "string".to_string(),
            SymTy::Named(name) => format!("named:{name}"),
            SymTy::App(name, args) => format!(
                "app:{name}[{}]",
                args.iter().map(SymTy::plan_tag).collect::<Vec<_>>().join(";")
            ),
        }
    }

    #[cfg(feature = "engine")]
    fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "int" => Some(SymTy::Int),
            "float" => Some(SymTy::Float),
            "bool" => Some(SymTy::Bool),
            "string" => Some(SymTy::String),
            _ => parse_sym_ty_plan_tag(tag),
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
    EmptyList { elem_ty: SymTy },
    /// Source-level record/ADT field projection: read declared field `field`
    /// (source declaration order) of a value of the named user type.
    /// `field_ty` is the field's source type; encoding binds the projection to
    /// the exact wasm struct type index through the byte-derived struct table.
    ProjectField {
        type_name: String,
        field: u32,
        field_ty: SymTy,
        value: SymValueId,
    },
    IntConstCmp {
        op: SymIntCmp,
        value: SymValueId,
        constant: i64,
    },
    /// Operational discriminant dispatch over an ADT value. The encoded plan
    /// reads field 0 of the struct named by `type_name`, compares it with
    /// `tag`, and evaluates `hit` on equality or `miss` otherwise. This does
    /// not assert any source-constructor-to-tag relationship.
    TagMatch {
        type_name: String,
        scrutinee: SymValueId,
        tag: i64,
        hit: Box<SymBlock>,
        miss: Box<SymBlock>,
    },
    If {
        cond: SymValueId,
        then_block: Box<SymBlock>,
        else_block: Box<SymBlock>,
    },
    /// Monolithic fused `Option.withDefault(Vector.get(p0, p1), default)`:
    /// read the `type_name` vector in param 0 at the Int index in param 1,
    /// yielding the element in bounds and the literal `default` otherwise.
    /// The vector and index are pinned to params 0 and 1. Twin of
    /// `SymNodeKind.vectorGetOrDefault`.
    VectorGetOrDefault {
        type_name: String,
        default: i64,
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

impl SymBlock {
    pub fn result_ty(&self) -> Option<SymTy> {
        self.nodes
            .get(self.result.0)
            .filter(|node| node.id == self.result)
            .map(|node| node.ty.clone())
    }
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
    pub fn to_expr_fragment_plan(
        &self,
        host_table: &FragHostTable,
        struct_table: &FragStructTable,
    ) -> Option<ExprFragmentPlan> {
        Some(ExprFragmentPlan {
            params: self
                .params
                .iter()
                .map(SymTy::to_frag_ty)
                .collect::<Option<Vec<_>>>()?,
            result: self.result.to_frag_ty()?,
            body: expr_fragment_block_from_sym(&self.body, host_table, struct_table)?,
        })
    }
}

#[derive(Clone, Debug)]
pub enum FragmentPlan {
    Sym(SymPlan),
    Expr(ExprFragmentPlan),
}

impl FragmentPlan {
    pub fn to_expr_fragment_plan(
        &self,
        host_table: &FragHostTable,
        struct_table: &FragStructTable,
    ) -> Option<ExprFragmentPlan> {
        match self {
            FragmentPlan::Sym(plan) => plan.to_expr_fragment_plan(host_table, struct_table),
            FragmentPlan::Expr(plan) => Some(plan.clone()),
        }
    }
}

#[cfg(feature = "engine")]
fn expr_fragment_source_plan(
    source_plan: &Option<SymPlan>,
    plan: &ExprFragmentPlan,
) -> Option<SymPlan> {
    source_plan
        .clone()
        .or_else(|| SymPlan::from_expr_fragment_source_subset(plan))
}

#[cfg(feature = "engine")]
fn adt_constructor_sym_plan_from_cert(c: &Cert, model_info: &ModelInfo) -> Option<SymPlan> {
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
    let params = sig
        .params
        .iter()
        .map(|ty| sym_ty_from_source_type_name(ty))
        .collect::<Option<Vec<_>>>()?;
    let result = sym_ty_from_source_type_name(&sig.ret)?;
    let (type_name, ctor_name, source_args) = match &result {
        SymTy::Named(type_name) if adt_constructor_uses_model(c, model_info) => {
            let ind = model_info.inductives.get(&sig.ret)?;
            let ctor = ind.ctors.first()?;
            if ctor.fields != sig.params || !sym_plan_simple_token(&ctor.name) {
                return None;
            }
            (type_name.clone(), ctor.name.clone(), None)
        }
        SymTy::App(type_name, type_args) if type_name == "List" && type_args.len() == 1 => {
            let elem_ty = type_args[0].clone();
            let source_args = match (params.as_slice(), fields.as_slice()) {
                ([head], [ConstructorField::Local(0), ConstructorField::Null])
                    if *head == elem_ty => Some(true),
                ([head, tail], [ConstructorField::Local(0), ConstructorField::Local(1)])
                    if *head == elem_ty && *tail == result => Some(false),
                _ => return None,
            };
            ("List".to_string(), "::".to_string(), source_args)
        }
        _ => return None,
    };
    if !sym_plan_simple_token(&type_name) {
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
    let args = if source_args == Some(true) {
        let empty_id = SymValueId(nodes.len());
        nodes.push(SymNode {
            id: empty_id,
            ty: result.clone(),
            kind: SymNodeKind::EmptyList {
                elem_ty: match &result {
                    SymTy::App(_, args) => args[0].clone(),
                    _ => unreachable!(),
                },
            },
        });
        vec![SymValueId(0), empty_id]
    } else {
        fields
            .iter()
            .map(|field| match field {
                ConstructorField::Local(i) if (*i as usize) < params.len() => {
                    Some(SymValueId(*i as usize))
                }
                ConstructorField::Local(_) | ConstructorField::Null => None,
            })
            .collect::<Option<Vec<_>>>()?
    };
    let result_id = SymValueId(nodes.len());
    nodes.push(SymNode {
        id: result_id,
        ty: result,
        kind: SymNodeKind::Construct {
            type_name,
            ctor_name,
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

#[cfg(feature = "engine")]
fn sym_plan_is_list_construct(plan: &SymPlan) -> bool {
    matches!(&plan.result, SymTy::App(name, args) if name == "List" && args.len() == 1)
}

#[cfg(feature = "engine")]
fn sym_ty_from_source_type_name(ty: &str) -> Option<SymTy> {
    let ty = strip_balanced_outer_parens(ty.trim());
    match ty {
        "Int" => Some(SymTy::Int),
        "Float" => Some(SymTy::Float),
        "Bool" => Some(SymTy::Bool),
        "String" => Some(SymTy::String),
        _ if let Some(rest) = ty.strip_prefix("List ") => Some(SymTy::App(
            "List".to_string(),
            vec![sym_ty_from_source_type_name(rest)?],
        )),
        _ if let Some((left, right)) = split_top_level(ty, '×') => Some(SymTy::App(
            "Tuple".to_string(),
            vec![
                sym_ty_from_source_type_name(left)?,
                sym_ty_from_source_type_name(right)?,
            ],
        )),
        _ if sym_plan_simple_token(ty) => Some(SymTy::Named(ty.to_string())),
        _ => None,
    }
}

#[cfg(feature = "engine")]
fn strip_balanced_outer_parens(mut value: &str) -> &str {
    loop {
        let Some(inner) = value.strip_prefix('(').and_then(|v| v.strip_suffix(')')) else {
            return value;
        };
        let mut depth = 0i32;
        let balanced = inner.chars().all(|ch| {
            match ch {
                '(' => depth += 1,
                ')' => depth -= 1,
                _ => {}
            }
            depth >= 0
        }) && depth == 0;
        if !balanced {
            return value;
        }
        value = inner.trim();
    }
}

#[cfg(feature = "engine")]
fn split_top_level(value: &str, needle: char) -> Option<(&str, &str)> {
    let mut depth = 0i32;
    for (at, ch) in value.char_indices() {
        match ch {
            '(' | '<' => depth += 1,
            ')' | '>' => depth -= 1,
            _ if ch == needle && depth == 0 => {
                return Some((value[..at].trim(), value[at + ch.len_utf8()..].trim()));
            }
            _ => {}
        }
    }
    None
}

#[cfg(feature = "engine")]
fn parse_sym_ty_plan_tag(tag: &str) -> Option<SymTy> {
    if let Some(name) = tag.strip_prefix("named:") {
        return sym_plan_simple_token(name).then(|| SymTy::Named(name.to_string()));
    }
    let app = tag.strip_prefix("app:")?;
    let open = app.find('[')?;
    let name = &app[..open];
    let args_text = app.get(open + 1..)?.strip_suffix(']')?;
    if !sym_plan_simple_token(name) || args_text.is_empty() {
        return None;
    }
    let mut args = Vec::new();
    let mut start = 0usize;
    let mut depth = 0i32;
    for (at, ch) in args_text.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            ';' if depth == 0 => {
                args.push(SymTy::from_plan_tag(&args_text[start..at])?);
                start = at + 1;
            }
            _ => {}
        }
    }
    args.push(SymTy::from_plan_tag(&args_text[start..])?);
    Some(SymTy::App(name.to_string(), args))
}

#[cfg(feature = "engine")]
fn sym_plan_simple_token(value: &str) -> bool {
    !value.is_empty() && !value.chars().any(char::is_whitespace) && !value.contains('=')
}

#[derive(Clone, Debug)]
pub struct FragmentPlanArtifact {
    pub export_name: String,
    pub plan: FragmentPlan,
}

fn sym_block_struct_names_in_order(block: &SymBlock, out: &mut Vec<String>) {
    for node in &block.nodes {
        match &node.kind {
            SymNodeKind::ProjectField { type_name, .. } => out.push(type_name.clone()),
            SymNodeKind::VectorGetOrDefault { type_name, .. } => out.push(type_name.clone()),
            SymNodeKind::TagMatch {
                type_name,
                hit,
                miss,
                ..
            } => {
                out.push(type_name.clone());
                sym_block_struct_names_in_order(hit, out);
                sym_block_struct_names_in_order(miss, out);
            }
            SymNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                sym_block_struct_names_in_order(then_block, out);
                sym_block_struct_names_in_order(else_block, out);
            }
            _ => {}
        }
    }
}

/// The distinct source type names whose wasm struct identity is needed by a
/// projection or operational tag dispatch, in first appearance order.
pub(crate) fn sym_plan_project_type_names(plan: &SymPlan) -> Vec<String> {
    let mut all = Vec::new();
    sym_block_struct_names_in_order(&plan.body, &mut all);
    let mut out = Vec::new();
    for name in all {
        if !out.contains(&name) {
            out.push(name);
        }
    }
    out
}

#[cfg(feature = "engine")]
fn frag_block_struct_get_user_tys_in_order(block: &FragBlock, out: &mut Vec<u32>) {
    for node in &block.nodes {
        match &node.kind {
            FragNodeKind::StructGetUser { ty_idx, .. } => out.push(*ty_idx),
            FragNodeKind::VectorGetOrDefault { arr_ty, .. } => out.push(*arr_ty),
            FragNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                frag_block_struct_get_user_tys_in_order(then_block, out);
                frag_block_struct_get_user_tys_in_order(else_block, out);
            }
            _ => {}
        }
    }
}

/// The struct-table entries a source plan and its encoded representation plan
/// pin together: each `project.field` type name paired with the wasm struct
/// index its `struct.get.user` encoding resolved to (deterministic node
/// order). `None` when the pairing is inconsistent — fail-closed.
#[cfg(feature = "engine")]
fn expr_fragment_struct_table_entries(
    source_plan: &SymPlan,
    plan: &ExprFragmentPlan,
) -> Option<Vec<(String, u32)>> {
    let mut names = Vec::new();
    sym_block_struct_names_in_order(&source_plan.body, &mut names);
    let mut indices = Vec::new();
    frag_block_struct_get_user_tys_in_order(&plan.body, &mut indices);
    if names.len() != indices.len() {
        return None;
    }
    let mut table = FragStructTable::default();
    for (name, idx) in names.into_iter().zip(indices) {
        if !table.insert(&name, idx) {
            return None;
        }
    }
    Some(table.entries)
}

/// Resolve the struct table a producer fragment plan needs at emit time, from
/// the emitter's own type registry (`resolve`: record type name -> wasm struct
/// type index). Fail-closed on any unknown name.
pub fn frag_struct_table_for_plan(
    plan: &FragmentPlan,
    resolve: &dyn Fn(&str, &SymTy) -> Option<u32>,
) -> Option<FragStructTable> {
    let bindings = match plan {
        FragmentPlan::Sym(plan) => sym_plan_struct_bindings(plan)?,
        FragmentPlan::Expr(_) => Vec::new(),
    };
    let mut table = FragStructTable::default();
    for (name, ty) in bindings {
        let idx = resolve(&name, &ty)?;
        if !table.insert(&name, idx) {
            return None;
        }
    }
    Some(table)
}

fn sym_block_struct_bindings(block: &SymBlock, out: &mut Vec<(String, SymTy)>) -> Option<()> {
    for node in &block.nodes {
        match &node.kind {
            SymNodeKind::ProjectField {
                type_name, value, ..
            } => {
                let ty = block.nodes.get(value.0)?.ty.clone();
                out.push((type_name.clone(), ty));
            }
            SymNodeKind::TagMatch {
                type_name,
                scrutinee,
                hit,
                miss,
                ..
            } => {
                let ty = block.nodes.get(scrutinee.0)?.ty.clone();
                out.push((type_name.clone(), ty));
                sym_block_struct_bindings(hit, out)?;
                sym_block_struct_bindings(miss, out)?;
            }
            SymNodeKind::VectorGetOrDefault { type_name, .. } => {
                // The fused read is pinned to the `Vector<Int>` in param 0.
                out.push((
                    type_name.clone(),
                    SymTy::App("Vector".to_string(), vec![SymTy::Int]),
                ));
            }
            SymNodeKind::If {
                then_block,
                else_block,
                ..
            } => {
                sym_block_struct_bindings(then_block, out)?;
                sym_block_struct_bindings(else_block, out)?;
            }
            _ => {}
        }
    }
    Some(())
}

fn sym_plan_struct_bindings(plan: &SymPlan) -> Option<Vec<(String, SymTy)>> {
    let mut all = Vec::new();
    sym_block_struct_bindings(&plan.body, &mut all)?;
    let mut out = Vec::<(String, SymTy)>::new();
    for (name, ty) in all {
        if let Some((_, existing)) = out.iter().find(|(existing, _)| existing == &name) {
            if existing != &ty {
                return None;
            }
        } else {
            out.push((name, ty));
        }
    }
    Some(out)
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
                | FragPrim::I32Eq
                | FragPrim::I32LtS
                | FragPrim::I32GtS => return None,
            },
            args: args.iter().map(|id| SymValueId(id.0)).collect(),
        },
        FragNodeKind::VectorGetOrDefault { .. } => return None,
        FragNodeKind::If {
            cond,
            then_block,
            else_block,
        } => SymNodeKind::If {
            cond: SymValueId(cond.0),
            then_block: Box::new(sym_block_from_frag_source_subset(then_block)?),
            else_block: Box::new(sym_block_from_frag_source_subset(else_block)?),
        },
        // `StructGetUser` deliberately does NOT project back to source here:
        // the source meaning (type name, field) cannot be recovered from the
        // representation node alone. Projection certs carry their SymPlan
        // forward from the producer instead.
        FragNodeKind::ConstI64(_)
        | FragNodeKind::ConstI32(_)
        | FragNodeKind::HostCall { .. }
        | FragNodeKind::SelfCall { .. }
        | FragNodeKind::StructGet { .. }
        | FragNodeKind::StructGetUser { .. }
        | FragNodeKind::RefIsNull { .. } => return None,
    };
    Some(SymNode {
        id: SymValueId(node.id.0),
        ty,
        kind,
    })
}

#[cfg(all(test, feature = "engine"))]
mod sym_plan_defs_tests {
    use super::*;

    fn int_const_block(value: i64) -> SymBlock {
        SymBlock {
            nodes: vec![SymNode {
                id: SymValueId(0),
                ty: SymTy::Int,
                kind: SymNodeKind::ConstInt(value),
            }],
            result: SymValueId(0),
        }
    }

    fn slot_count_sym_plan() -> SymPlan {
        let option_int = SymTy::App("Option".to_string(), vec![SymTy::Int]);
        SymPlan {
            params: vec![option_int.clone()],
            result: SymTy::Int,
            body: SymBlock {
                nodes: vec![
                    SymNode {
                        id: SymValueId(0),
                        ty: option_int,
                        kind: SymNodeKind::Param { index: 0 },
                    },
                    SymNode {
                        id: SymValueId(1),
                        ty: SymTy::Int,
                        kind: SymNodeKind::TagMatch {
                            type_name: "Option".to_string(),
                            scrutinee: SymValueId(0),
                            tag: 1,
                            hit: Box::new(int_const_block(1)),
                            miss: Box::new(int_const_block(0)),
                        },
                    },
                ],
                result: SymValueId(1),
            },
        }
    }

    #[test]
    fn slot_count_tag_match_encoder_is_node_for_node_fixture_twin() {
        let sym = slot_count_sym_plan();
        let host_table = FragHostTable {
            box_idx: Some(6),
            ..FragHostTable::default()
        };
        let struct_table = FragStructTable {
            entries: vec![("Option".to_string(), 2)],
        };
        let actual = sym
            .to_expr_fragment_plan(&host_table, &struct_table)
            .expect("slotCount symbolic plan encodes");
        let boxed_const = |value| FragBlock {
            nodes: vec![
                FragNode {
                    id: FragValueId(0),
                    ty: FragTy::I64,
                    kind: FragNodeKind::ConstI64(value),
                },
                FragNode {
                    id: FragValueId(1),
                    ty: FragTy::IntCarrier,
                    kind: FragNodeKind::HostCall {
                        role: FragHostRole::Box,
                        func_idx: 6,
                        args: vec![FragValueId(0)],
                    },
                },
            ],
            result: FragValueId(1),
        };
        let expected = ExprFragmentPlan {
            params: vec![FragTy::AdtRef],
            result: FragTy::IntCarrier,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::RawI32,
                        kind: FragNodeKind::StructGetUser {
                            ty_idx: 2,
                            field: 0,
                            value: FragValueId(0),
                        },
                    },
                    FragNode {
                        id: FragValueId(2),
                        ty: FragTy::RawI32,
                        kind: FragNodeKind::ConstI32(1),
                    },
                    FragNode {
                        id: FragValueId(3),
                        ty: FragTy::BoolI32,
                        kind: FragNodeKind::Prim {
                            op: FragPrim::I32Eq,
                            args: vec![FragValueId(1), FragValueId(2)],
                        },
                    },
                    FragNode {
                        id: FragValueId(4),
                        ty: FragTy::IntCarrier,
                        kind: FragNodeKind::If {
                            cond: FragValueId(3),
                            then_block: Box::new(boxed_const(1)),
                            else_block: Box::new(boxed_const(0)),
                        },
                    },
                ],
                result: FragValueId(4),
            },
        };
        assert_eq!(actual, expected);
    }

    #[test]
    fn slot_count_tag_match_lean_render_is_fixture_twin() {
        let actual = sym_plan_lean_value(&slot_count_sym_plan());
        let expected = "{ profile := \"sym-fragment-v1\", params := [(.app1 \"Option\" .int)], result := .int, body := ({ nodes := [{ id := 0, ty := (.app1 \"Option\" .int), kind := .param 0 }, { id := 1, ty := .int, kind := .tagMatch \"Option\" 0 (1 : Int) ({ nodes := [{ id := 0, ty := .int, kind := .constInt (1 : Int) }], result := 0 } : SymBlock) ({ nodes := [{ id := 0, ty := .int, kind := .constInt (0 : Int) }], result := 0 } : SymBlock) }], result := 1 } : SymBlock) }";
        assert_eq!(actual, expected);
    }

    #[test]
    fn slot_count_tag_match_sidecar_roundtrips_and_rejects_wrong_type_name() {
        let plan = slot_count_sym_plan();
        let text = sym_fragment_plan_text(&plan);
        let mut parser = SymPlanParser::new(&text, plan.params.clone(), plan.result.clone());
        assert_eq!(parser.parse().expect("parse tag.match plan"), plan.body);

        let malformed = text.replace("type=Option", "type=Unknown");
        let mut parser = SymPlanParser::new(&malformed, plan.params.clone(), plan.result.clone());
        let error = parser.parse().expect_err("wrong tag.match type must decline");
        assert!(
            error.contains("names type `Unknown`") && error.contains("app:Option[int]"),
            "unexpected parser error: {error}"
        );
    }

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
        // Named types encode to the opaque `adtRef` representation type; the
        // plan below still does NOT encode because its body carries a string
        // literal (no representation-level constructor).
        assert_eq!(named.to_frag_ty(), Some(FragTy::AdtRef));

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
        assert!(plan.to_expr_fragment_plan(&FragHostTable::placeholder(), &FragStructTable::default()).is_none());
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
        assert!(plan.to_expr_fragment_plan(&FragHostTable::placeholder(), &FragStructTable::default()).is_none());
    }

    #[test]
    fn adt_constructor_cert_projects_to_source_construct_plan() {
        let cert = Cert::AdtConstructor {
            name: "mkOp".to_string(),
            self_idx: 1,
            type_idx: 1,
            nlocals: 0,
            carrier: 0,
            struct_idx: 3,
            field_count: 1,
            elem_ty: TyKind::Ref { nullable: true, idx: 3 },
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
    fn list_cons_constructor_projects_generic_source_types_and_empty_tail() {
        let cert = Cert::AdtConstructor {
            name: "singletonEntry".to_string(),
            self_idx: 1,
            type_idx: 1,
            nlocals: 1,
            carrier: 18,
            struct_idx: 25,
            field_count: 2,
            elem_ty: TyKind::Ref { nullable: true, idx: 3 },
            arity: 1,
            fields: vec![ConstructorField::Local(0), ConstructorField::Null],
            ops: vec![
                Op::LocalGet(0),
                Op::RefNull(Some(25)),
                Op::StructNew(25, 2),
            ],
        };
        let mut model_info = ModelInfo::default();
        model_info.fns.insert(
            "singletonEntry".to_string(),
            FnSig {
                params: vec!["(String × Json)".to_string()],
                ret: "List (String × Json)".to_string(),
            },
        );

        let plan = adt_constructor_sym_plan_from_cert(&cert, &model_info)
            .expect("List cons projects to a generic SymPlan");
        let elem = SymTy::App(
            "Tuple".to_string(),
            vec![SymTy::String, SymTy::Named("Json".to_string())],
        );
        assert_eq!(plan.params, vec![elem.clone()]);
        assert_eq!(
            plan.result,
            SymTy::App("List".to_string(), vec![elem.clone()])
        );
        assert!(matches!(
            &plan.body.nodes[1].kind,
            SymNodeKind::EmptyList { elem_ty } if elem_ty == &elem
        ));
        assert!(matches!(
            &plan.body.nodes[2].kind,
            SymNodeKind::Construct { type_name, ctor_name, args }
                if type_name == "List" && ctor_name == "::"
                    && args == &[SymValueId(0), SymValueId(1)]
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
        assert!(plan.to_expr_fragment_plan(&FragHostTable::placeholder(), &FragStructTable::default()).is_none());
    }
}
