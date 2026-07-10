/// Model-facing type used by the legacy `ExprFragment` obligation. This is not
/// the source-level `SymPlan` type system: raw representation limbs stay `WVal`
/// here because older obligations can still be byte-level/verbatim.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FragModelTy {
    Float,
    Bool,
    Int,
    WVal,
}

impl FragModelTy {
    fn display_name(self) -> &'static str {
        match self {
            FragModelTy::Float => "Float",
            FragModelTy::Bool => "Bool",
            FragModelTy::Int => "Int",
            FragModelTy::WVal => "WVal",
        }
    }

    fn lean_dom_type(self) -> &'static str {
        match self {
            FragModelTy::Float => "UInt64",
            FragModelTy::Bool => "Bool",
            FragModelTy::Int => "Int",
            FragModelTy::WVal => "WVal",
        }
    }
}

/// Typed, ordered non-recursive expression fragment. The compiler emits this as
/// an untrusted plan sidecar, and the verifier checks/canonically lowers it
/// before using it as a certificate witness. Every value has an explicit
/// representation type and a defining node.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FragTy {
    F64,
    BoolI32,
    IntCarrier,
    I64,
    RawI32,
    Ref,
    /// Opaque user-ADT / record reference. Unlike `Ref` (a carrier limb), this
    /// is a whole user struct/array reference read verbatim (`model_ty = WVal`).
    /// The concrete wasm type index is never part of the type: it lives on the
    /// projecting node (`StructGetUser`) and is bound to bytes by the byte-exact
    /// gate, mirroring how `HostCall` carries its resolved `func_idx`.
    AdtRef,
}

impl FragTy {
    fn model_ty(self) -> FragModelTy {
        match self {
            FragTy::F64 => FragModelTy::Float,
            FragTy::BoolI32 => FragModelTy::Bool,
            FragTy::IntCarrier => FragModelTy::Int,
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref | FragTy::AdtRef => FragModelTy::WVal,
        }
    }

    fn plan_tag(self) -> &'static str {
        match self {
            FragTy::F64 => "f64",
            FragTy::BoolI32 => "bool-i32",
            FragTy::IntCarrier => "int-carrier",
            FragTy::I64 => "i64",
            FragTy::RawI32 => "raw-i32",
            FragTy::Ref => "ref",
            FragTy::AdtRef => "adt-ref",
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
            FragTy::AdtRef => ".adtRef",
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
            "adt-ref" => Some(FragTy::AdtRef),
            _ => None,
        }
    }

    fn source_name(self) -> &'static str {
        self.model_ty().display_name()
    }

    fn lean_dom_type(self) -> &'static str {
        self.model_ty().lean_dom_type()
    }

    fn lean_arg_repr(self, name: &str, carrier: &str) -> String {
        match self {
            FragTy::F64 => format!(".f64v {name}"),
            FragTy::BoolI32 => format!("b32 {name}"),
            FragTy::IntCarrier => format!("carrierSmall {carrier} {name}"),
            FragTy::I64 | FragTy::RawI32 | FragTy::Ref | FragTy::AdtRef => name.to_string(),
        }
    }
}

/// Runtime host-helper role admitted by `expr-fragment-v1`. Each role fixes a
/// representation-level type signature; the resolved wasm function index is
/// carried on the node (variant B) and bound to the module bytes by the
/// byte-exact gate, and to the byte-derived role table by the Rust checker.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum FragHostRole {
    Box,
    Add,
    /// The strict integer-subtraction contract (`carrier sub`). Admitted by the
    /// Lean `HostRole` grammar for the fuel-recursion descent `sub(n, box(1))`.
    Sub,
}

impl FragHostRole {
    fn plan_tag(self) -> &'static str {
        match self {
            FragHostRole::Box => "box",
            FragHostRole::Add => "add",
            FragHostRole::Sub => "sub",
        }
    }

    fn lean_ctor(self) -> &'static str {
        match self {
            FragHostRole::Box => ".box",
            FragHostRole::Add => ".add",
            FragHostRole::Sub => ".sub",
        }
    }

    pub(crate) fn from_plan_tag(tag: &str) -> Option<Self> {
        match tag {
            "box" => Some(FragHostRole::Box),
            "add" => Some(FragHostRole::Add),
            "sub" => Some(FragHostRole::Sub),
            _ => None,
        }
    }

    /// Static registry of representation-level role signatures: argument types
    /// and result type. Twin of `PlanCheck.hostCallResultTy?`.
    pub(crate) fn signature(self) -> (&'static [FragTy], FragTy) {
        match self {
            FragHostRole::Box => (&[FragTy::I64], FragTy::IntCarrier),
            FragHostRole::Add => (&[FragTy::IntCarrier, FragTy::IntCarrier], FragTy::IntCarrier),
            FragHostRole::Sub => (&[FragTy::IntCarrier, FragTy::IntCarrier], FragTy::IntCarrier),
        }
    }
}

/// The byte-derived host-role table: which wasm function index realises each
/// admitted host role in THIS module. Derived from the audited disassembler
/// (`box` = the exported `__rt_aint_from_i64`; `add` = the body-shape role),
/// never from a plan or sidecar. Plans must cite exactly these indices.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct FragHostTable {
    pub(crate) box_idx: Option<u32>,
    pub(crate) add_idx: Option<u32>,
    /// The strict `sub` binding (byte-derived exactly like `add`: carrier-binop
    /// signature + `i64.sub`-first + uniqueness, fail-closed to `None`).
    /// Carried Rust-side as an S2 prerequisite but NOT yet part of the Lean
    /// grammar: it is deliberately absent from `lean_value()` and `lookup()`
    /// until the S2 control-flow leg extends the `HostRole` enum.
    pub(crate) sub_idx: Option<u32>,
}

impl FragHostTable {
    pub(crate) fn lookup(&self, role: FragHostRole) -> Option<u32> {
        match role {
            FragHostRole::Box => self.box_idx,
            FragHostRole::Add => self.add_idx,
            FragHostRole::Sub => self.sub_idx,
        }
    }

    /// The `List (HostRole × Nat)` literal the Lean encoder and artifact claims
    /// consume, in fixed role order (box, add) for deterministic rendering.
    pub(crate) fn lean_value(&self) -> String {
        let mut entries = Vec::new();
        if let Some(idx) = self.box_idx {
            entries.push(format!("(.box, {idx})"));
        }
        if let Some(idx) = self.add_idx {
            entries.push(format!("(.add, {idx})"));
        }
        // `sub_idx` is deliberately NOT rendered: the Lean `HostRole` grammar
        // gains `.sub` only in the S2 control-flow leg. Emitting it now would
        // change the audited artifact/claim surface, so `sub` stays invisible
        // to Lean even though it is derived and carried Rust-side.
        format!("[{}]", entries.join(", "))
    }

    /// A placeholder table for producer-side encodability gating at MIR time,
    /// before any wasm indices exist. Encoding shape does not depend on the
    /// index values, so gating with placeholders is exact; real byte-derived
    /// indices are always used wherever bytes are available.
    pub(crate) fn placeholder() -> Self {
        FragHostTable {
            box_idx: Some(0),
            add_idx: Some(0),
            sub_idx: Some(0),
        }
    }
}

/// The struct-binding table: which wasm struct type index realises each source
/// record/ADT type name in THIS module. On the producer side it is resolved
/// from the emitter's type registry; on the verifier side it is re-derived from
/// the export's own byte-derived `struct.get` instructions and validated
/// against the module's struct context. Plans never carry these indices as
/// trusted data: a wrong table encodes to canonical bytes that cannot match
/// the module, so the claim fail-closes at the byte-exact gate.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(crate) struct FragStructTable {
    /// `(source type name, wasm struct type index)`, sorted by name for
    /// deterministic rendering.
    pub(crate) entries: Vec<(String, u32)>,
}

impl FragStructTable {
    pub(crate) fn lookup(&self, name: &str) -> Option<u32> {
        self.entries
            .iter()
            .find(|(entry, _)| entry == name)
            .map(|(_, idx)| *idx)
    }

    /// Insert one binding; `false` when the name is already bound to a
    /// DIFFERENT index (an inconsistent table must fail-close).
    pub(crate) fn insert(&mut self, name: &str, idx: u32) -> bool {
        match self.lookup(name) {
            Some(existing) => existing == idx,
            None => {
                self.entries.push((name.to_string(), idx));
                self.entries.sort();
                true
            }
        }
    }

    /// The Lean `List (String × Nat)` literal claims and witnesses consume.
    pub(crate) fn lean_value(&self) -> String {
        format!(
            "[{}]",
            self.entries
                .iter()
                .map(|(name, idx)| format!("({}, {idx})", lean_str(name)))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    /// A placeholder table for producer-side encodability gating at MIR time,
    /// before any wasm type indices exist: every projected type name maps to 0.
    /// Encoding shape does not depend on the index values.
    pub(crate) fn placeholder_for(plan: &SymPlan) -> Self {
        let mut names = sym_plan_project_type_names(plan);
        names.sort();
        FragStructTable {
            entries: names.into_iter().map(|name| (name, 0)).collect(),
        }
    }
}

/// The Lean `List (String × Nat)` literal of a module-wide struct table: the
/// consistent union of per-export entries. An inconsistent union (one name
/// bound to two indices) fail-closes.
pub fn frag_struct_table_lean_from_entries<'a>(
    entries: impl IntoIterator<Item = &'a (String, u32)>,
) -> Result<String, String> {
    let mut table = FragStructTable::default();
    for (name, idx) in entries {
        if !table.insert(name, *idx) {
            return Err(format!(
                "inconsistent byte-derived struct table: `{name}` binds to both {} and {idx}",
                table.lookup(name).unwrap_or(0)
            ));
        }
    }
    Ok(table.lean_value())
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
    /// Projection of `field` out of a user struct of wasm type `ty_idx` (a whole
    /// record/ADT, not the Int carrier). Lowers to `struct.get ty_idx field`
    /// with the type index taken from the node (bound to bytes by the byte-exact
    /// gate; validated against the byte-derived struct context by the checker).
    StructGetUser {
        ty_idx: u32,
        field: u32,
        value: FragValueId,
    },
    RefIsNull {
        value: FragValueId,
    },
    Prim {
        op: FragPrim,
        args: Vec<FragValueId>,
    },
    HostCall {
        role: FragHostRole,
        func_idx: u32,
        args: Vec<FragValueId>,
    },
    /// A self-recursive call to the function being certified. `tail` selects
    /// `return_call` (`0x12`) over `call` (`0x10`). `func_idx` is the resolved
    /// self index, bound to the module bytes by the byte-exact gate.
    SelfCall {
        tail: bool,
        func_idx: u32,
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
        FragNodeKind::StructGetUser {
            ty_idx,
            field,
            value,
        } => format!(".structGetUser {ty_idx} {field} {}", value.0),
        FragNodeKind::RefIsNull { value } => format!(".refIsNull {}", value.0),
        FragNodeKind::Prim { op, args } => format!(
            ".prim {} [{}]",
            op.lean_plan_ctor(),
            args.iter()
                .map(|id| id.0.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
        FragNodeKind::HostCall {
            role,
            func_idx,
            args,
        } => format!(
            ".hostCall {} {func_idx} [{}]",
            role.lean_ctor(),
            args.iter()
                .map(|id| id.0.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
        FragNodeKind::SelfCall {
            tail,
            func_idx,
            args,
        } => format!(
            ".selfCall {tail} {func_idx} [{}]",
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
        FragNodeKind::StructGetUser {
            ty_idx,
            field,
            value,
        } => {
            out.push_str(&format!(
                "struct.get.user ty={ty_idx} field={field} value=v{}\n",
                value.0
            ));
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
        FragNodeKind::HostCall {
            role,
            func_idx,
            args,
        } => {
            out.push_str(&format!(
                "hostcall role={} func={func_idx} args={}\n",
                role.plan_tag(),
                render_fragment_plan_ids(args)
            ));
        }
        FragNodeKind::SelfCall {
            tail,
            func_idx,
            args,
        } => {
            out.push_str(&format!(
                "selfcall tail={tail} func={func_idx} args={}\n",
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

#[cfg(test)]
mod expr_fragment_sem_ty_tests {
    use super::*;

    /// Concrete heap-type indices are SIGNED s33 LEB128: 63 is the last index
    /// whose signed encoding coincides with the unsigned one; 64 has bit 6 set
    /// in its low group, so signed encoding needs a continuation (`c0 00`).
    #[test]
    fn heap_type_indices_use_signed_s33_leb() {
        let enc = |idx: u32| {
            let mut out = Vec::new();
            push_s33_heap_idx(&mut out, idx);
            out
        };
        assert_eq!(enc(0), vec![0x00]);
        assert_eq!(enc(2), vec![0x02]);
        assert_eq!(enc(63), vec![0x3f]);
        assert_eq!(enc(64), vec![0xc0, 0x00]);
        assert_eq!(enc(127), vec![0xff, 0x00]);
        assert_eq!(enc(128), vec![0x80, 0x01]);
    }

    /// The carrier local declaration and the Int-carrier `if` block type both
    /// carry a concrete heap-type index; at carrier 64 they must use the s33
    /// continuation encoding, at 63 the single byte.
    #[test]
    fn carrier_local_decl_and_blocktype_are_s33_at_boundary() {
        // Local declaration prefix of the canonical body: `01 01 63 <s33 c>`.
        let plan = add_two_hostcall_plan();
        let bytes63 = lower_expr_fragment_plan_code_entry_bytes(&plan, 63).expect("carrier 63");
        assert_eq!(&bytes63[1..5], &[0x01, 0x01, 0x63, 0x3f]);
        let bytes64 = lower_expr_fragment_plan_code_entry_bytes(&plan, 64).expect("carrier 64");
        assert_eq!(&bytes64[1..6], &[0x01, 0x01, 0x63, 0xc0, 0x00]);

        // Value-if block type `04 63 <s33 c>` in a recursion-shaped body.
        let rec63 = recursion_plan_recursive(10, 11, 12, 1, 7, false, BodyOperand::Input);
        let rb63 = lower_expr_fragment_plan_code_entry_bytes(&rec63, 63).expect("rec carrier 63");
        assert!(
            rb63.windows(3).any(|w| w == [0x04, 0x63, 0x3f]),
            "carrier-63 value-if block type missing: {rb63:02x?}"
        );
        let rb64 = lower_expr_fragment_plan_code_entry_bytes(&rec63, 64).expect("rec carrier 64");
        assert!(
            rb64.windows(4).any(|w| w == [0x04, 0x63, 0xc0, 0x00]),
            "carrier-64 value-if block type missing: {rb64:02x?}"
        );
    }

    #[test]
    fn frag_ty_keeps_model_face_separate_from_wasm_repr() {
        assert_eq!(FragTy::F64.model_ty(), FragModelTy::Float);
        assert_eq!(FragTy::BoolI32.model_ty(), FragModelTy::Bool);
        assert_eq!(FragTy::IntCarrier.model_ty(), FragModelTy::Int);
        assert_eq!(FragTy::I64.model_ty(), FragModelTy::WVal);
        assert_eq!(FragTy::RawI32.model_ty(), FragModelTy::WVal);
        assert_eq!(FragTy::Ref.model_ty(), FragModelTy::WVal);

        assert_eq!(FragTy::IntCarrier.plan_tag(), "int-carrier");
        assert_eq!(FragTy::IntCarrier.source_name(), "Int");
        assert_eq!(FragTy::IntCarrier.lean_dom_type(), "Int");
    }

    fn add_two_hostcall_plan() -> ExprFragmentPlan {
        ExprFragmentPlan {
            params: vec![FragTy::IntCarrier],
            result: FragTy::IntCarrier,
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
                        kind: FragNodeKind::ConstI64(2),
                    },
                    FragNode {
                        id: FragValueId(2),
                        ty: FragTy::IntCarrier,
                        kind: FragNodeKind::HostCall {
                            role: FragHostRole::Box,
                            func_idx: 6,
                            args: vec![FragValueId(1)],
                        },
                    },
                    FragNode {
                        id: FragValueId(3),
                        ty: FragTy::IntCarrier,
                        kind: FragNodeKind::HostCall {
                            role: FragHostRole::Add,
                            func_idx: 7,
                            args: vec![FragValueId(0), FragValueId(2)],
                        },
                    },
                ],
                result: FragValueId(3),
            },
        }
    }

    #[test]
    fn hostcall_plan_lowers_to_addtwo_bytes_and_ops() {
        let plan = add_two_hostcall_plan();
        // Byte lowering reproduces the empirically pinned addTwo code-entry
        // `0d 01 01 63 02 20 00 42 02 10 06 10 07 0b`.
        let bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, 2).expect("lower bytes");
        assert_eq!(
            bytes,
            vec![13, 1, 1, 99, 2, 32, 0, 66, 2, 16, 6, 16, 7, 11]
        );
        // Op lowering matches the straight-line body the checker re-derives.
        let ops = lower_expr_fragment_plan(&plan, 2).expect("lower ops");
        assert_eq!(
            ops,
            vec![Op::LocalGet(0), Op::I64Const(2), Op::Call(6), Op::Call(7)]
        );
    }

    #[test]
    fn hostcall_plan_text_round_trips_through_parser() {
        let plan = add_two_hostcall_plan();
        let text = expr_fragment_plan_text(&plan);
        let mut parser =
            FragPlanParser::new(&text, vec![FragTy::IntCarrier], FragTy::IntCarrier);
        let body = parser.parse().expect("parse hostcall plan");
        let reparsed = ExprFragmentPlan {
            params: vec![FragTy::IntCarrier],
            result: FragTy::IntCarrier,
            body,
        };
        assert_eq!(
            lower_expr_fragment_plan_code_entry_bytes(&reparsed, 2).expect("relower"),
            vec![13, 1, 1, 99, 2, 32, 0, 66, 2, 16, 6, 16, 7, 11]
        );
    }

    #[test]
    fn hostcall_plan_lean_value_uses_host_call_ctor() {
        let lean = expr_fragment_plan_lean_value(&add_two_hostcall_plan());
        assert!(lean.contains(".hostCall .box 6 [1]"), "lean = {lean}");
        assert!(lean.contains(".hostCall .add 7 [0, 2]"), "lean = {lean}");
    }

    /// The field-projection plan shape for `userName(u: User) -> String` = `u.name`.
    /// Param 0 is the User reference (`AdtRef`); the body projects field 0 of the
    /// user struct (wasm type index 15) verbatim. Carrier scratch-local type = 18.
    fn user_name_projection_plan() -> ExprFragmentPlan {
        ExprFragmentPlan {
            params: vec![FragTy::AdtRef],
            result: FragTy::AdtRef,
            body: FragBlock {
                nodes: vec![
                    FragNode {
                        id: FragValueId(0),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::Local { index: 0 },
                    },
                    FragNode {
                        id: FragValueId(1),
                        ty: FragTy::AdtRef,
                        kind: FragNodeKind::StructGetUser {
                            ty_idx: 15,
                            field: 0,
                            value: FragValueId(0),
                        },
                    },
                ],
                result: FragValueId(1),
            },
        }
    }

    #[test]
    fn user_struct_projection_plan_lowers_to_username_bytes_and_ops() {
        let plan = user_name_projection_plan();
        // Byte lowering reproduces the empirically pinned userName code-entry
        // `0b 01 01 63 12 20 00 fb 02 0f 00 0b` (carrier scratch local type 18,
        // `struct.get 15 0`).
        let bytes = lower_expr_fragment_plan_code_entry_bytes(&plan, 18).expect("lower bytes");
        assert_eq!(
            bytes,
            vec![0x0b, 0x01, 0x01, 0x63, 0x12, 0x20, 0x00, 0xfb, 0x02, 0x0f, 0x00, 0x0b]
        );
        // Op lowering matches the `[local.get 0, struct.get 15 0]` body the
        // checker re-derives (struct type index from the node, not the carrier).
        let ops = lower_expr_fragment_plan(&plan, 18).expect("lower ops");
        assert_eq!(ops, vec![Op::LocalGet(0), Op::StructGet(15, 0)]);
    }

    #[test]
    fn user_struct_projection_plan_text_round_trips_through_parser() {
        let plan = user_name_projection_plan();
        let text = expr_fragment_plan_text(&plan);
        let mut parser = FragPlanParser::new(&text, vec![FragTy::AdtRef], FragTy::AdtRef);
        let body = parser.parse().expect("parse projection plan");
        let reparsed = ExprFragmentPlan {
            params: vec![FragTy::AdtRef],
            result: FragTy::AdtRef,
            body,
        };
        assert_eq!(
            lower_expr_fragment_plan_code_entry_bytes(&reparsed, 18).expect("relower"),
            vec![0x0b, 0x01, 0x01, 0x63, 0x12, 0x20, 0x00, 0xfb, 0x02, 0x0f, 0x00, 0x0b]
        );
    }

    #[test]
    fn user_struct_projection_plan_text_and_lean_render_the_user_node() {
        let plan = user_name_projection_plan();
        let text = expr_fragment_plan_text(&plan);
        assert!(
            text.contains("struct.get.user ty=15 field=0 value=v0"),
            "plan text = {text}"
        );
        let lean = expr_fragment_plan_lean_value(&plan);
        assert!(lean.contains(".structGetUser 15 0 0"), "lean = {lean}");
        assert!(lean.contains("params := [.adtRef]"), "lean = {lean}");
        assert!(lean.contains("result := .adtRef"), "lean = {lean}");
    }
}
