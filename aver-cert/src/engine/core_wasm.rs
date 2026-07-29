/// A user function recovered from the emitted module.
#[derive(Clone)]
struct UserFn {
    name: String,
    wasm_idx: u32,
    type_idx: u32,
    arity: usize,
    /// Byte-level parameter type kinds from the declared function signature.
    params: Vec<TyKind>,
    /// Byte-level FIRST result type kind (convenience for the many single-result
    /// classifiers). `None` when the function returns nothing.
    result: Option<TyKind>,
    /// Byte-level COMPLETE result-kind vector from the declared function
    /// signature. Verbatim routes require this to be exactly one recognized kind
    /// (a nullable reference for plan-backed dispatches, `f64` for the scalar
    /// legacy route), so a two-result or zero-result signature is rejected.
    results: Vec<TyKind>,
    nlocals: usize,
    /// Raw code-entry bytes: body-size prefix followed by the function body
    /// bytes, ending after the final `end`.
    code_entry_bytes: Vec<u8>,
    ops: Vec<Op>,
    /// call targets in body order, for reason reporting.
    calls: Vec<u32>,
    /// `module.name` of every host capability (function import) this body calls,
    /// in body order. Diagnostic only: no certified template admits a call into
    /// the import surface, so a non-empty list names the blocker outright.
    host_capability_calls: Vec<String>,
    /// The declared parameter kinds resolved against the module's own type
    /// section, so a decline can say "a String" instead of "a reference to type
    /// 20". Diagnostic only; every admission gate reads `params`.
    param_shapes: Vec<ValShape>,
    /// The declared result kinds, resolved the same way.
    result_shapes: Vec<ValShape>,
    /// The first instruction in the body that the opcode vocabulary does not
    /// model, named as the disassembler saw it. Diagnostic only; `None` when
    /// the body is fully in vocabulary or the instruction carried an operand
    /// the disassembler could not resolve.
    first_unsupported_op: Option<String>,
    has_loop_or_branch: bool,
}

/// What a declared parameter or result actually is, once its wasm type is
/// resolved against the module's own type section. Purely for decline reasons:
/// nothing is admitted or refused on a `ValShape`, so a misreading here can
/// only make a message vaguer, never certify anything.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ValShape {
    /// A reference to the Int carrier struct: a boxed `Int`.
    Int,
    /// A reference to a string byte-array type: a `String`.
    Str,
    /// Any other reference: a user record, variant or list value.
    UserRef,
    /// A bare machine scalar (`i64`, `i32`, `f64`) — `Int` in unboxed form,
    /// `Bool`, or `Float`.
    Scalar,
    /// A wasm type outside the certificate's type vocabulary altogether.
    Raw,
}

impl ValShape {
    /// Whether the certified fragment treats this shape as an opaque value
    /// rather than a number it can compute with. Scalars and boxed `Int`s are
    /// the fragment's native vocabulary; everything else is only ever read or
    /// built by one of the specific ADT/String templates.
    fn outside_scalar_fragment(self) -> bool {
        matches!(self, ValShape::Str | ValShape::UserRef | ValShape::Raw)
    }

    fn describe(self) -> &'static str {
        match self {
            ValShape::Int => "an Int",
            ValShape::Str => "a String",
            ValShape::UserRef => "a user record, variant or list value",
            ValShape::Scalar => "a machine scalar",
            ValShape::Raw => "a wasm type the certified fragment does not model",
        }
    }
}

#[derive(Clone)]
struct CodeEntry {
    nlocals: usize,
    code_entry_bytes: Vec<u8>,
    ops: Vec<Op>,
    calls: Vec<u32>,
    has_loop_or_branch: bool,
    /// Name of the first instruction the opcode vocabulary does not model, as
    /// the disassembler read it. Diagnostic only.
    first_unsupported_op: Option<String>,
    host_role: Option<HostRole>,
    /// The first `i64` arithmetic operator seen in the body — the strict
    /// discriminator the plan-first host-role table uses to tell the
    /// behavioural `add` helper apart from the `mul` helper (whose umag loops
    /// also contain `i64.add`).
    first_arith_strict: Option<FirstI64Arith>,
    /// Result of the certificate decoder's own first-arith body scan, mirrored
    /// byte for byte over the body bytes after the locals vector. `None` means
    /// the decoder's scan would FAIL on this body (an instruction encoding
    /// outside its vocabulary); `Some(first)` is its successful verdict. Used
    /// to refuse certification of any module whose module-wide role scan the
    /// verifier could never complete.
    kernel_arith_scan: Option<Option<FirstI64Arith>>,
    host_ops: Vec<HostOp>,
}

/// The minimal opcode surface the two templates need. Anything else is `Other`
/// (which forces a decline) — a certified body never contains an `Other`.
#[derive(Clone, Debug, PartialEq)]
enum Op {
    LocalGet(u32),
    LocalSet(u32),
    I64Const(i64),
    I32Const(i32),
    F64Const(u64),
    RefTest(u32),
    RefCast(u32),
    StructNew(u32, u32),
    StructGet(u32, u32),
    ArrayNewData {
        type_idx: u32,
        data_idx: u32,
        bytes: Vec<u8>,
    },
    ArrayNewDataUnresolved {
        type_idx: u32,
        data_idx: u32,
        offset: i32,
        len: i32,
    },
    /// `array.new_fixed ty n` — build a fixed-size array of `n` elements from
    /// the top `n` stack values. Captured (not `Op::Other`) so the concat
    /// recognizer can see the container construction; functions that use it in
    /// any shape other than the contracted concat beachhead are still declined.
    ArrayNewFixed(u32, u32),
    /// `ref.null <heap>` carrying the disassembled heap type. `Some(idx)` is a
    /// concrete module type index (e.g. the List struct a widened match's `[]`
    /// default nulls); `None` is any abstract heap type (func/extern/none/…),
    /// which is not re-lowerable by the plan grammar and so is a fail-closed
    /// form. Every classifier ignores the payload (matches `RefNull(_)`), so
    /// null-default recognition is unchanged; the index is threaded purely for
    /// byte-exact re-lowering in the S2 control-flow grammar.
    RefNull(Option<u32>),
    RefIsNull,
    I64Eq,
    I64LeS,
    I64LtS,
    I64GeS,
    I64GtS,
    F64Add,
    F64Mul,
    F64Le,
    F64Ge,
    F64Lt,
    F64Gt,
    F64Eq,
    I32Eq,
    I32LtS,
    I32GtS,
    /// The fused vector-read bounds-check comparisons and combiner, plus the
    /// array read itself. Captured (not `Op::Other`) so the fused-read plan
    /// re-lowering compares exactly against the disassembled body.
    I32LtU,
    I32GeS,
    I32And,
    ArrayLen,
    ArrayGet(u32),
    If,
    Else,
    End,
    Call(u32),
    ReturnCall(u32),
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum HostRole {
    Add,
    Mul,
    Sub,
    StringEq,
    /// The byte-array concatenation helper (`String.concat` lowering): takes a
    /// container array of string-arrays, returns the byte-concatenated array.
    StringConcat,
}

/// Public differential surface for the two byte-exact string helper roles.
/// The production trust path is the audited `CertDecode.StringHost.roleTable`
/// equality; Rust keeps classifying independently as a fail-fast oracle.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StringHostRole {
    Eq,
    Concat,
}

pub type StringHostRoles = Vec<(u32, StringHostRole)>;

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

fn string_host_roles_lean_value(roles: &StringHostRoles) -> String {
    format!(
        "[{}]",
        roles
            .iter()
            .map(|(idx, role)| format!("({idx}, {})", role.lean_value()))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum HostOp {
    LocalGet(u32),
    LocalSet(u32),
    I32Const(i32),
    ArrayLen,
    ArrayGetU(u32),
    /// `array.get` (signed/typed, NOT the unsigned `array.get_u`). Used by the
    /// concat helper which reads element arrays out of the container array.
    ArrayGet(u32),
    /// `array.new_default ty` — allocate a zero-filled array of a type index.
    /// Used by the concat helper to allocate the result byte array.
    ArrayNewDefault(u32),
    /// `array.copy dst_ty src_ty` — bulk copy elements between arrays. The two
    /// operands are the destination and source array type indices.
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

/// The non-recursive operand of a body-consumed fuel recursion's combinator
/// `f n = if n≤0 then base else <combine>`, where `<combine>` applies a host
/// arithmetic helper to the self-call result and this operand. From the bytes.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BodyOperand {
    /// The descending input `n` (`local.get 0`), as in `sumTo`'s `n + f(n-1)`.
    Input,
    /// A boxed integer literal, as in `2 + f(n-1)`.
    Const(i64),
}

/// Which arithmetic contract the body-recursion combinator obeys. The model
/// operator selects the semantic evaluator; the distinct byte-derived host role
/// independently pins that selection to the corresponding runtime helper.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Combinator {
    /// `f(n-1)` combined with the other operand by integer `+` (host `add`).
    Add,
    /// integer `*` (host `mul`).
    Mul,
}

impl Combinator {
    /// The obligation host slot / theorem contract param this combinator draws.
    fn param(self) -> &'static str {
        match self {
            Combinator::Add => "add",
            Combinator::Mul => "mul",
        }
    }
}

/// Byte-level summary of one wasm value type in a function signature. Typed
/// admission gates key on these (the shape of the claim as the BYTES declare
/// it) — never on the source model's types, and never on a bare parameter
/// count.
#[derive(Clone, Copy, PartialEq, Eq)]
enum TyKind {
    /// Abstract `eq` reference — the emitter's parameter type for a user ADT
    /// value that the body dispatches on.
    Eqref,
    /// Concrete reference to module type `idx`, carrying the declared
    /// `nullable` bit (`ref null idx` vs `ref idx`). Nullability is retained
    /// rather than erased so a verbatim result can be pinned to the exact
    /// `ref null` form the certified signature promises.
    Ref { nullable: bool, idx: u32 },
    I64,
    I32,
    F64,
    Other,
}
