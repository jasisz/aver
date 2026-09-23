// ---- byte-derived module facts ---------------------------------------------
//
// Everything the producer reads off the exact module bytes: the type section
// (for the type-table self-check), function types, exports, every code entry
// (for the plan self-check and the closure), passive data segments, the Int
// carrier and the runtime helper roles. None of it is authority: the wall
// re-derives or pins every fact from `ArtifactBytes.modBytes`.

/// A value type as the wall's decoder distinguishes it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ValT {
    I32,
    I64,
    F64,
    Eqref,
    /// `(ref null idx)`, concrete.
    RefNull(u32),
    Other,
}

/// A field storage type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StorT {
    I8,
    Val(ValT),
    Other,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum CompT {
    Func(Vec<ValT>, Vec<ValT>),
    Struct(Vec<StorT>),
    Array(StorT),
}

#[derive(Clone, Debug)]
struct TypeFact {
    comp: CompT,
    is_final: bool,
    supertype: Option<u32>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum HostOp {
    LocalGet(u32),
    LocalSet(u32),
    I32Const(i32),
    ArrayLen,
    ArrayGetU(u32),
    ArrayGet(u32),
    ArrayNewDefault(u32),
    ArrayCopy(u32, u32),
    I32Ne,
    I32GeU,
    I32Add,
    If,
    Block,
    Loop,
    Br(u32),
    BrIf(u32),
    Return,
    End,
    Other,
}

/// The first `i64` arithmetic operator of a helper body.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FirstI64Arith {
    Add,
    Sub,
    Mul,
}

#[derive(Clone)]
struct CodeFact {
    /// The exact code entry, size prefix included.
    entry: Vec<u8>,
    nlocals: usize,
    calls: Vec<u32>,
    host_ops: Vec<HostOp>,
    first_arith: Option<FirstI64Arith>,
    kernel_arith_scan: Option<Option<FirstI64Arith>>,
}

/// The runtime helper indices the subject declares (`CertDecode.AddSub.Roles`
/// plus the `ArithHostParams` the helper templates are synthesized from).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct HostRoles {
    pub box_idx: Option<u32>,
    pub add_idx: Option<u32>,
    pub mul_idx: Option<u32>,
    pub sub_idx: Option<u32>,
    pub to_index_idx: Option<u32>,
    pub cmp_idx: Option<u32>,
    pub eq_idx: Option<u32>,
    /// `__aint_divmod`: not exported; declared only at the one function
    /// whose body is the wall's template (`divmod_template_body`).
    pub divmod_idx: Option<u32>,
    pub limb_idx: Option<u32>,
    pub decompose_idx: Option<u32>,
    pub normalize_idx: Option<u32>,
    pub strip_idx: Option<u32>,
    pub umag_cmp_idx: Option<u32>,
}

impl HostRoles {
    /// `(carrier, limb, decompose, normalize, strip, umagCmp)` for a carriered
    /// module, `None` for a carrierless one.
    pub fn arith_params(&self, carrier: Option<u32>) -> Option<(u32, u32, u32, u32, u32, u32)> {
        self.box_idx?;
        Some((
            carrier?,
            self.limb_idx?,
            self.decompose_idx?,
            self.normalize_idx?,
            self.strip_idx?,
            self.umag_cmp_idx?,
        ))
    }

    fn lean_option(index: Option<u32>) -> String {
        index.map_or_else(|| "none".to_string(), |i| format!("some {i}"))
    }

    /// The `CertDecode.AddSub.Roles` literal.
    pub fn roles_lean_value(&self) -> String {
        format!(
            "({{ box := {}, add := {}, mul := {}, sub := {}, toIndex := {}, cmp := {}, eq := {}, divmod := {} }} : CertDecode.AddSub.Roles)",
            Self::lean_option(self.box_idx),
            Self::lean_option(self.add_idx),
            Self::lean_option(self.mul_idx),
            Self::lean_option(self.sub_idx),
            Self::lean_option(self.to_index_idx),
            Self::lean_option(self.cmp_idx),
            Self::lean_option(self.eq_idx),
            Self::lean_option(self.divmod_idx),
        )
    }

    pub fn arith_params_record_lean(&self, carrier: Option<u32>) -> Option<String> {
        self.arith_params(carrier).map(|(carrier, limb, decompose, normalize, strip, umag)| {
            format!(
                "({{ carrier := {carrier}, limb := {limb}, decompose := {decompose}, normalize := {normalize}, strip := {strip}, umagCmp := {umag} }} : ArithTemplateDerisk.ArithHostParams)"
            )
        })
    }
}

/// `ArithTemplateDerisk.divmodTemplateBody`: hex literal bytes, and each
/// declared index as `<u:name>` (unsigned LEB) or `<s:name>` (signed heap
/// type).
const DIVMOD_TEMPLATE: &str = concat!(
    "0b047e0163<s:limb>017f0163<s:limb>037f0263<s:limb>017f0263<s:limb>097f077e0163",
    "<s:limb>2000fb02<u:carrier>01d12001fb02<u:carrier>01d1712000fb02<u:carrier>00428080",
    "808080808080807f512001fb02<u:carrier>00427f517145710463<s:carrier>2000fb02",
    "<u:carrier>0021032001fb02<u:carrier>002104200320048121062006420053044020062004420053",
    "047e420020047d0520040b7c21060b20020463<s:carrier>2006d0<s:limb>4100fb00<u:carrier>05",
    "200320047f210520032004812106200642005304402004420055047e200542017d05200542017c0b2105",
    "0b2005d0<s:limb>4100fb00<u:carrier>0b05200010<u:decompose>21082107200110",
    "<u:decompose>210a2109200710<u:strip>210b200910<u:strip>210c200b45047f410105200b0bfb",
    "07<u:limb>210d200c41016afb07<u:limb>210e4100210f200c410146044020094100fb0b<u:limb>21",
    "0442002106200b41016b21170240034020174100480d01200642208620072017fb0b<u:limb>84211c20",
    "0d2017201c200480fb0e<u:limb>201c2004822106201741016b21170c000b0b200e41002006fb0e",
    "<u:limb>0502402007200b2009200c10<u:umagCmp>211920194100480440200e410020074100200bfb",
    "11<u:limb><u:limb>0c010b200c41016b210f200b200f6b2115200e410020072015200ffb11<u:limb>",
    "<u:limb>201541206c41016b21140240034020144100480d01200f200c49047f200f05200c0b41016a21",
    "0f4200211b41002117024003402017200f4f0d01200e2017fb0b<u:limb>42018642ffffffff0f83201b",
    "84211c200e2017fb0b<u:limb>421f88211b200e2017201cfb0e<u:limb>201741016a21170c000b0b20",
    "1441206e21152014412070211620072015fb0b<u:limb>2016ad884201834200520440200e4100200e41",
    "00fb0b<u:limb>420184fb0e<u:limb>0b02400340200f450d01200e200f41016bfb0b<u:limb>420052",
    "0d01200f41016b210f0c000b0b200e200f2009200c10<u:umagCmp>2119201941004e04404200211d41",
    "002117024003402017200c41016a4f0d01200e2017fb0b<u:limb>2017200c49047e20092017fb0b",
    "<u:limb>0542000b7d201d7d211e201e4200530440201e4280808080107c211e4201211d054200211d0b",
    "200e2017201e42ffffffff0f83fb0e<u:limb>201741016a21170c000b0b200d2015200d2015fb0b",
    "<u:limb>42012016ad8684fb0e<u:limb>0b201441016b21140c000b0b0b0b200c41016a210f02400340",
    "200f450d01200e200f41016bfb0b<u:limb>4200520d01200f41016b210f0c000b0b2008410048200f41",
    "004771211a20020463<s:carrier>201a0440200cfb07<u:limb>21104200211d410021170240034020",
    "17200c4f0d0120092017fb0b<u:limb>2017200f49047e200e2017fb0b<u:limb>0542000b7d201d7d21",
    "1e201e4200530440201e4280808080107c211e4201211d054200211d0b20102017201e42ffffffff0f83",
    "fb0e<u:limb>201741016a21170c000b0b2010211105200e21110b410121122011201210",
    "<u:normalize>052008200a6c2112201a0440200b41016afb07<u:limb>211041002117024003402017",
    "200b4f0d0120102017200d2017fb0b<u:limb>fb0e<u:limb>201741016a21170c000b0b4201211b4100",
    "211702400340201b500d0120102017fb0b<u:limb>201b7c211c20102017201c42ffffffff0f83fb0e",
    "<u:limb>201c422088211b201741016a21170c000b0b2010211105200d21110b2011201210",
    "<u:normalize>0b0b0b",
);

/// The canonical `__aint_divmod` body (locals and code, no size prefix) over
/// the declared indices, exactly as the wall synthesizes it.
fn divmod_template_body(p: (u32, u32, u32, u32, u32, u32)) -> Option<Vec<u8>> {
    let (carrier, limb, decompose, normalize, strip, umag_cmp) = p;
    let mut out = Vec::new();
    let mut rest = DIVMOD_TEMPLATE;
    while !rest.is_empty() {
        if let Some(tail) = rest.strip_prefix('<') {
            let (tok, after) = tail.split_once('>')?;
            let (enc, name) = tok.split_once(':')?;
            let v = u64::from(match name {
                "carrier" => carrier,
                "limb" => limb,
                "decompose" => decompose,
                "normalize" => normalize,
                "strip" => strip,
                "umagCmp" => umag_cmp,
                _ => return None,
            });
            match enc {
                "u" => uleb(v, &mut out)?,
                "s" => s33(v, &mut out)?,
                _ => return None,
            }
            rest = after;
        } else {
            out.push(u8::from_str_radix(rest.get(..2)?, 16).ok()?);
            rest = &rest[2..];
        }
    }
    Some(out)
}

/// The body of a code entry: the bytes after its size prefix.
fn entry_body(entry: &[u8]) -> Option<&[u8]> {
    let mut size = 0usize;
    let mut shift = 0;
    for (i, b) in entry.iter().enumerate() {
        size |= usize::from(b & 0x7f) << shift;
        shift += 7;
        if b & 0x80 == 0 {
            return entry.get(i + 1..i + 1 + size);
        }
    }
    None
}

/// A byte-exact String helper role.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringHostRole {
    Eq,
    Concat,
}

impl StringHostRole {
    fn lean_value(self) -> &'static str {
        match self {
            StringHostRole::Eq => ".eq",
            StringHostRole::Concat => ".concat",
        }
    }

    fn manifest_value(self) -> &'static str {
        match self {
            StringHostRole::Eq => "stringEq",
            StringHostRole::Concat => "stringConcat",
        }
    }
}

pub type StringHostRoles = Vec<(u32, StringHostRole)>;

struct ModuleFacts {
    nimports: u32,
    types: Vec<TypeFact>,
    /// Entries of the explicit rec group that opens the type section.
    first_group_len: usize,
    func_types: Vec<u32>,
    exports: Vec<(String, u8, u32)>,
    code: Vec<CodeFact>,
    /// Passive data segments by index (`None` for an active one).
    data: Vec<Option<Vec<u8>>>,
    carrier: Option<u32>,
    roles: HostRoles,
    string_roles: StringHostRoles,
}

fn val_t(vt: &wasmparser::ValType) -> ValT {
    use wasmparser::{AbstractHeapType, HeapType, ValType};
    match vt {
        ValType::I32 => ValT::I32,
        ValType::I64 => ValT::I64,
        ValType::F64 => ValT::F64,
        ValType::Ref(rt) if rt.is_nullable() => match rt.heap_type() {
            HeapType::Concrete(idx) => idx.as_module_index().map_or(ValT::Other, ValT::RefNull),
            HeapType::Abstract {
                shared: false,
                ty: AbstractHeapType::Eq,
            } => ValT::Eqref,
            _ => ValT::Other,
        },
        _ => ValT::Other,
    }
}

fn stor_t(st: &wasmparser::StorageType) -> StorT {
    match st {
        wasmparser::StorageType::I8 => StorT::I8,
        wasmparser::StorageType::Val(v) => StorT::Val(val_t(v)),
        wasmparser::StorageType::I16 => StorT::Other,
    }
}

fn host_op(op: &wasmparser::Operator<'_>) -> HostOp {
    use wasmparser::Operator as O;
    match op {
        O::LocalGet { local_index } => HostOp::LocalGet(*local_index),
        O::LocalSet { local_index } => HostOp::LocalSet(*local_index),
        O::I32Const { value } => HostOp::I32Const(*value),
        O::ArrayLen => HostOp::ArrayLen,
        O::ArrayGetU { array_type_index } => HostOp::ArrayGetU(*array_type_index),
        O::ArrayGet { array_type_index } => HostOp::ArrayGet(*array_type_index),
        O::ArrayNewDefault { array_type_index } => HostOp::ArrayNewDefault(*array_type_index),
        O::ArrayCopy {
            array_type_index_dst,
            array_type_index_src,
        } => HostOp::ArrayCopy(*array_type_index_dst, *array_type_index_src),
        O::I32Ne => HostOp::I32Ne,
        O::I32GeU => HostOp::I32GeU,
        O::I32Add => HostOp::I32Add,
        O::If { .. } => HostOp::If,
        O::Block { .. } => HostOp::Block,
        O::Loop { .. } => HostOp::Loop,
        O::Br { relative_depth } => HostOp::Br(*relative_depth),
        O::BrIf { relative_depth } => HostOp::BrIf(*relative_depth),
        O::Return => HostOp::Return,
        O::End => HostOp::End,
        _ => HostOp::Other,
    }
}

/// Byte-for-byte mirror of the certificate decoder's first-arith body scan
/// (`CertDecode.AddSub`): `None` when the decoder's scan would fail.
fn kernel_first_arith_scan(bytes: &[u8]) -> Option<Option<FirstI64Arith>> {
    fn skip_uleb(bytes: &[u8], cursor: &mut usize) -> Option<u64> {
        let mut value: u64 = 0;
        let mut shift: u32 = 0;
        for _ in 0..5 {
            let byte = *bytes.get(*cursor)?;
            *cursor += 1;
            value |= u64::from(byte & 0x7f) << shift;
            if byte < 128 {
                if shift != 0 && byte == 0 {
                    return None;
                }
                return Some(value);
            }
            shift += 7;
        }
        None
    }
    fn skip_sleb(bytes: &[u8], cursor: &mut usize) -> Option<()> {
        for _ in 0..10 {
            let byte = *bytes.get(*cursor)?;
            *cursor += 1;
            if byte < 128 {
                return Some(());
            }
        }
        None
    }
    fn skip_block_type(bytes: &[u8], cursor: &mut usize) -> Option<()> {
        let byte = *bytes.get(*cursor)?;
        if byte == 0x40 || matches!(byte, 0x7b..=0x7f) {
            *cursor += 1;
            Some(())
        } else if byte == 0x63 || byte == 0x64 {
            *cursor += 1;
            skip_sleb(bytes, cursor)
        } else {
            skip_sleb(bytes, cursor)
        }
    }
    let mut cursor = 0usize;
    loop {
        if cursor == bytes.len() {
            return Some(None);
        }
        let op = bytes[cursor];
        cursor += 1;
        match op {
            0x7c => return Some(Some(FirstI64Arith::Add)),
            0x7d => return Some(Some(FirstI64Arith::Sub)),
            0x7e => return Some(Some(FirstI64Arith::Mul)),
            0x45..=0xc4 => {}
            0x0b | 0x05 | 0x0f | 0x00 | 0x01 | 0x1a | 0x1b | 0xd1 => {}
            0x20 | 0x21 | 0x22 | 0x23 | 0x24 | 0x0c | 0x0d | 0x10 | 0x12 => {
                skip_uleb(bytes, &mut cursor)?;
            }
            0x0e => {
                let count = skip_uleb(bytes, &mut cursor)?;
                for _ in 0..count.checked_add(1)? {
                    skip_uleb(bytes, &mut cursor)?;
                }
            }
            0x11 => {
                skip_uleb(bytes, &mut cursor)?;
                skip_uleb(bytes, &mut cursor)?;
            }
            0x02..=0x04 => skip_block_type(bytes, &mut cursor)?,
            0x41 | 0x42 => skip_sleb(bytes, &mut cursor)?,
            0x43 => {
                if bytes.len() - cursor < 4 {
                    return None;
                }
                cursor += 4;
            }
            0x44 => {
                if bytes.len() - cursor < 8 {
                    return None;
                }
                cursor += 8;
            }
            0xd0 => skip_sleb(bytes, &mut cursor)?,
            0xd2 => {
                skip_uleb(bytes, &mut cursor)?;
            }
            0xfb => {
                let sub = skip_uleb(bytes, &mut cursor)?;
                match sub {
                    0x00 | 0x01 | 0x06 | 0x07 | 0x0b | 0x0c | 0x0d | 0x0e => {
                        skip_uleb(bytes, &mut cursor)?;
                    }
                    0x02 | 0x05 | 0x08 | 0x09 => {
                        skip_uleb(bytes, &mut cursor)?;
                        skip_uleb(bytes, &mut cursor)?;
                    }
                    0x0f => {}
                    0x14..=0x17 => skip_sleb(bytes, &mut cursor)?,
                    _ => return None,
                }
            }
            _ => return None,
        }
    }
}

/// `__wasmgc_string_eq`, by its byte-exact opcode shape (the wall classifies
/// the same shape in `CertDecode.StringHost`).
fn is_string_eq_host(
    code: &CodeFact,
    sig: &CompT,
    strings: &std::collections::HashSet<u32>,
) -> bool {
    let CompT::Func(params, results) = sig else {
        return false;
    };
    let [ValT::RefNull(lhs), ValT::RefNull(rhs)] = params.as_slice() else {
        return false;
    };
    if lhs != rhs
        || results.as_slice() != [ValT::I32]
        || code.nlocals != 2
        || !code.calls.is_empty()
    {
        return false;
    }
    let t = *lhs;
    if !strings.contains(&t) {
        return false;
    }
    use HostOp::*;
    let expected = [
        LocalGet(0),
        ArrayLen,
        LocalGet(1),
        ArrayLen,
        I32Ne,
        If,
        I32Const(0),
        Return,
        End,
        LocalGet(0),
        ArrayLen,
        LocalSet(2),
        I32Const(0),
        LocalSet(3),
        Block,
        Loop,
        LocalGet(3),
        LocalGet(2),
        I32GeU,
        BrIf(1),
        LocalGet(0),
        LocalGet(3),
        ArrayGetU(t),
        LocalGet(1),
        LocalGet(3),
        ArrayGetU(t),
        I32Ne,
        If,
        I32Const(0),
        Return,
        End,
        LocalGet(3),
        I32Const(1),
        I32Add,
        LocalSet(3),
        Br(0),
        End,
        End,
        I32Const(1),
        End,
    ];
    code.host_ops.as_slice() == expected
}

/// `__wasmgc_concat_n`, by its byte-exact opcode shape.
fn is_string_concat_host(
    code: &CodeFact,
    sig: &CompT,
    strings: &std::collections::HashSet<u32>,
) -> bool {
    let CompT::Func(params, results) = sig else {
        return false;
    };
    let [ValT::RefNull(container)] = params.as_slice() else {
        return false;
    };
    let [ValT::RefNull(byte)] = results.as_slice() else {
        return false;
    };
    if code.nlocals != 7 || !code.calls.is_empty() || !strings.contains(byte) {
        return false;
    }
    let (container, byte) = (*container, *byte);
    use HostOp::*;
    let expected = [
        LocalGet(0),
        ArrayLen,
        LocalSet(3),
        I32Const(0),
        LocalSet(1),
        I32Const(0),
        LocalSet(2),
        Block,
        Loop,
        LocalGet(2),
        LocalGet(3),
        I32GeU,
        BrIf(1),
        LocalGet(1),
        LocalGet(0),
        LocalGet(2),
        ArrayGet(container),
        ArrayLen,
        I32Add,
        LocalSet(1),
        LocalGet(2),
        I32Const(1),
        I32Add,
        LocalSet(2),
        Br(0),
        End,
        End,
        LocalGet(1),
        ArrayNewDefault(byte),
        LocalSet(6),
        I32Const(0),
        LocalSet(7),
        I32Const(0),
        LocalSet(2),
        Block,
        Loop,
        LocalGet(2),
        LocalGet(3),
        I32GeU,
        BrIf(1),
        LocalGet(0),
        LocalGet(2),
        ArrayGet(container),
        LocalSet(4),
        LocalGet(4),
        ArrayLen,
        LocalSet(5),
        LocalGet(6),
        LocalGet(7),
        LocalGet(4),
        I32Const(0),
        LocalGet(5),
        ArrayCopy(byte, byte),
        LocalGet(7),
        LocalGet(5),
        I32Add,
        LocalSet(7),
        LocalGet(2),
        I32Const(1),
        I32Add,
        LocalSet(2),
        Br(0),
        End,
        End,
        LocalGet(6),
        End,
    ];
    code.host_ops.as_slice() == expected
}

impl ModuleFacts {
    fn parse(wasm_bytes: &[u8]) -> Result<Self, String> {
        use wasmparser::{CompositeInnerType, DataKind, Operator, Parser, Payload};

        wasmparser::Validator::new()
            .validate_all(wasm_bytes)
            .map_err(|e| format!("wasm module failed validation: {e}"))?;

        let mut facts = ModuleFacts {
            nimports: 0,
            types: Vec::new(),
            first_group_len: 0,
            func_types: Vec::new(),
            exports: Vec::new(),
            code: Vec::new(),
            data: Vec::new(),
            carrier: None,
            roles: HostRoles::default(),
            string_roles: Vec::new(),
        };
        let mut has_non_function_import = false;
        let mut next_entry_start: Option<usize> = None;
        let mut first_group = true;
        let mut limb = None;
        for payload in Parser::new(0).parse_all(wasm_bytes) {
            match payload.map_err(|e| format!("wasm parse: {e}"))? {
                Payload::TypeSection(reader) => {
                    for rg in reader {
                        let rg = rg.map_err(|e| format!("type read: {e}"))?;
                        let explicit = rg.is_explicit_rec_group();
                        let mut count = 0usize;
                        for sub in rg.into_types() {
                            count += 1;
                            let idx = facts.types.len() as u32;
                            let comp = match &sub.composite_type.inner {
                                CompositeInnerType::Func(ft) => CompT::Func(
                                    ft.params().iter().map(val_t).collect(),
                                    ft.results().iter().map(val_t).collect(),
                                ),
                                CompositeInnerType::Struct(st) => CompT::Struct(
                                    st.fields.iter().map(|f| stor_t(&f.element_type)).collect(),
                                ),
                                CompositeInnerType::Array(at) => {
                                    CompT::Array(stor_t(&at.0.element_type))
                                }
                                _ => CompT::Array(StorT::Other),
                            };
                            if let CompT::Struct(fields) = &comp
                                && facts.carrier.is_none()
                                && fields.len() == 3
                                && fields[0] == StorT::Val(ValT::I64)
                                && fields[2] == StorT::Val(ValT::I32)
                            {
                                facts.carrier = Some(idx);
                                if let StorT::Val(ValT::RefNull(l)) = fields[1] {
                                    limb = Some(l);
                                }
                            }
                            facts.types.push(TypeFact {
                                comp,
                                is_final: sub.is_final,
                                supertype: sub.supertype_idx.and_then(|p| p.as_module_index()),
                            });
                        }
                        if first_group {
                            first_group = false;
                            if explicit {
                                facts.first_group_len = count;
                            }
                        }
                    }
                }
                Payload::ImportSection(reader) => {
                    for group in reader {
                        let group = group.map_err(|e| format!("import read: {e}"))?;
                        for imp in group {
                            let (_, imp) = imp.map_err(|e| format!("import read: {e}"))?;
                            if let wasmparser::TypeRef::Func(_) = imp.ty {
                                facts.nimports += 1;
                            } else {
                                has_non_function_import = true;
                            }
                        }
                    }
                }
                Payload::FunctionSection(reader) => {
                    for t in reader {
                        facts
                            .func_types
                            .push(t.map_err(|e| format!("func read: {e}"))?);
                    }
                }
                Payload::ExportSection(reader) => {
                    for ex in reader {
                        let ex = ex.map_err(|e| format!("export read: {e}"))?;
                        facts.exports.push((
                            ex.name.to_string(),
                            external_kind_byte(ex.kind),
                            ex.index,
                        ));
                    }
                }
                Payload::CodeSectionStart { range, size, .. } => {
                    next_entry_start = Some(
                        range
                            .end
                            .checked_sub(size as usize)
                            .ok_or("code section size is outside its byte range")?,
                    );
                }
                Payload::CodeSectionEntry(body) => {
                    let start = next_entry_start.ok_or("code entry before code section start")?;
                    let end = body.range().end;
                    let entry = wasm_bytes
                        .get(start..end)
                        .ok_or("code entry outside the module")?
                        .to_vec();
                    next_entry_start = Some(end);
                    let mut nlocals = 0usize;
                    let mut lr = body
                        .get_locals_reader()
                        .map_err(|e| format!("locals: {e}"))?;
                    for _ in 0..lr.get_count() {
                        let (n, _) = lr.read().map_err(|e| format!("locals: {e}"))?;
                        nlocals += n as usize;
                    }
                    let mut opr = body
                        .get_operators_reader()
                        .map_err(|e| format!("ops: {e}"))?;
                    let scan_start = opr.original_position();
                    let kernel_arith_scan = wasm_bytes
                        .get(scan_start..end)
                        .and_then(kernel_first_arith_scan);
                    let mut calls = Vec::new();
                    let mut host_ops = Vec::new();
                    let mut first_arith = None;
                    while !opr.eof() {
                        let op = opr.read().map_err(|e| format!("op read: {e}"))?;
                        host_ops.push(host_op(&op));
                        match op {
                            Operator::Call { function_index }
                            | Operator::ReturnCall { function_index } => calls.push(function_index),
                            Operator::I64Add => {
                                first_arith.get_or_insert(FirstI64Arith::Add);
                            }
                            Operator::I64Sub => {
                                first_arith.get_or_insert(FirstI64Arith::Sub);
                            }
                            Operator::I64Mul => {
                                first_arith.get_or_insert(FirstI64Arith::Mul);
                            }
                            _ => {}
                        }
                    }
                    facts.code.push(CodeFact {
                        entry,
                        nlocals,
                        calls,
                        host_ops,
                        first_arith,
                        kernel_arith_scan,
                    });
                }
                Payload::DataSection(reader) => {
                    for data in reader {
                        let data = data.map_err(|e| format!("data read: {e}"))?;
                        facts.data.push(match data.kind {
                            DataKind::Passive => Some(data.data.to_vec()),
                            DataKind::Active { .. } => None,
                        });
                    }
                }
                _ => {}
            }
        }
        facts.derive_roles(limb, has_non_function_import)?;
        Ok(facts)
    }

    fn export_idx(&self, name: &str) -> Option<u32> {
        self.exports
            .iter()
            .find(|(n, kind, _)| n == name && *kind == 0)
            .map(|(_, _, i)| *i)
    }

    fn fn_sig(&self, func_idx: u32) -> Option<&CompT> {
        let def = func_idx.checked_sub(self.nimports)? as usize;
        let ty = *self.func_types.get(def)?;
        Some(&self.types.get(ty as usize)?.comp)
    }

    fn code_of(&self, func_idx: u32) -> Option<&CodeFact> {
        self.code.get(func_idx.checked_sub(self.nimports)? as usize)
    }

    /// The runtime helper roles, derived exactly as the wall's decoders and
    /// template pins read them. A module whose role table the wall cannot
    /// resolve is refused here, naming the reason.
    fn derive_roles(
        &mut self,
        limb: Option<u32>,
        has_non_function_import: bool,
    ) -> Result<(), String> {
        let box_idx = self.export_idx("__rt_aint_from_i64");
        let carrier = self.carrier;
        let is_carrier_binop = |this: &Self, func_idx: u32| -> bool {
            let (Some(c), Some(CompT::Func(params, results))) = (carrier, this.fn_sig(func_idx))
            else {
                return false;
            };
            params.as_slice() == [ValT::RefNull(c), ValT::RefNull(c)]
                && results.as_slice() == [ValT::RefNull(c)]
        };
        if box_idx.is_some() {
            if carrier.is_none() {
                return Err("module exports the Int box helper `__rt_aint_from_i64` but declares no Int carrier struct type; the certificate decoder cannot resolve its host-role table".into());
            }
            if has_non_function_import {
                return Err("module exports the Int box helper `__rt_aint_from_i64` and also declares a non-function import; the certificate decoder declines such modules".into());
            }
            for (def, code) in self.code.iter().enumerate() {
                let func_idx = self.nimports + def as u32;
                if !is_carrier_binop(self, func_idx) {
                    continue;
                }
                match code.kernel_arith_scan {
                    Some(first) if first == code.first_arith => {}
                    _ => {
                        return Err(format!(
                            "function index {func_idx} has the Int carrier-binop signature and a body the certificate decoder's role scan cannot classify; the module-wide host-role table is undecodable"
                        ));
                    }
                }
            }
        }
        let unique = |arith: FirstI64Arith| -> Option<u32> {
            let hits: Vec<u32> = self
                .code
                .iter()
                .enumerate()
                .map(|(def, code)| (self.nimports + def as u32, code))
                .filter(|(idx, code)| {
                    code.first_arith == Some(arith) && is_carrier_binop(self, *idx)
                })
                .map(|(idx, _)| idx)
                .collect();
            match hits.as_slice() {
                [only] => Some(*only),
                _ => None,
            }
        };
        let add_idx = unique(FirstI64Arith::Add);
        let mut roles = HostRoles {
            box_idx,
            add_idx,
            mul_idx: unique(FirstI64Arith::Mul),
            sub_idx: unique(FirstI64Arith::Sub),
            to_index_idx: self.export_idx("__aint_to_index"),
            cmp_idx: self.export_idx("__aint_cmp"),
            eq_idx: self.export_idx("__aint_eq"),
            limb_idx: limb,
            ..HostRoles::default()
        };
        // The add helper calls the four bignum sub-routines; bucket its
        // callees by signature. The wall template-pins every index.
        if let Some(code) = add_idx.and_then(|i| self.code_of(i)) {
            let mut seen = std::collections::HashSet::new();
            for &callee in &code.calls {
                if !seen.insert(callee) {
                    continue;
                }
                if let Some(CompT::Func(params, results)) = self.fn_sig(callee) {
                    match (params.len(), results.len()) {
                        (1, 2) => roles.decompose_idx = roles.decompose_idx.or(Some(callee)),
                        (2, 1) => roles.normalize_idx = roles.normalize_idx.or(Some(callee)),
                        (1, 1) => roles.strip_idx = roles.strip_idx.or(Some(callee)),
                        (4, _) => roles.umag_cmp_idx = roles.umag_cmp_idx.or(Some(callee)),
                        _ => {}
                    }
                }
            }
        }
        // `__aint_divmod` is not exported: declare it only at the one function
        // whose body is the template the wall pins it by.
        if let Some(tmpl) = roles.arith_params(carrier).and_then(divmod_template_body) {
            let hits: Vec<u32> = self
                .code
                .iter()
                .enumerate()
                .filter(|(_, code)| entry_body(&code.entry) == Some(tmpl.as_slice()))
                .map(|(def, _)| self.nimports + def as u32)
                .collect();
            if let [only] = hits.as_slice() {
                roles.divmod_idx = Some(*only);
            }
        }
        self.roles = roles;
        let strings: std::collections::HashSet<u32> = self
            .types
            .iter()
            .enumerate()
            .filter(|(_, t)| t.comp == CompT::Array(StorT::I8))
            .map(|(i, _)| i as u32)
            .collect();
        let mut string_roles = Vec::new();
        for (def, code) in self.code.iter().enumerate() {
            let func_idx = self.nimports + def as u32;
            let Some(sig) = self.fn_sig(func_idx) else {
                continue;
            };
            if is_string_eq_host(code, sig, &strings) {
                string_roles.push((func_idx, StringHostRole::Eq));
            } else if is_string_concat_host(code, sig, &strings) {
                string_roles.push((func_idx, StringHostRole::Concat));
            }
        }
        self.string_roles = string_roles;
        Ok(())
    }
}
