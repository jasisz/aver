/// A user function recovered from the emitted module.
#[derive(Clone)]
struct UserFn {
    name: String,
    wasm_idx: u32,
    code_idx: u32,
    type_idx: u32,
    arity: usize,
    /// Byte-level parameter type kinds from the declared function signature.
    params: Vec<TyKind>,
    /// Byte-level result type kind from the declared function signature.
    result: Option<TyKind>,
    nlocals: usize,
    /// Raw code-entry bytes: body-size prefix followed by the function body
    /// bytes, ending after the final `end`.
    code_entry_bytes: Vec<u8>,
    ops: Vec<Op>,
    /// call targets in body order, for reason reporting.
    calls: Vec<u32>,
    has_loop_or_branch: bool,
}

#[derive(Clone)]
struct CodeEntry {
    nlocals: usize,
    code_entry_bytes: Vec<u8>,
    ops: Vec<Op>,
    calls: Vec<u32>,
    has_loop_or_branch: bool,
    host_role: Option<HostRole>,
    /// The first `i64` arithmetic operator seen in the body — the strict
    /// discriminator the plan-first host-role table uses to tell the
    /// behavioural `add` helper apart from the `mul` helper (whose umag loops
    /// also contain `i64.add`).
    first_arith_strict: Option<FirstI64Arith>,
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
    RefNull,
    RefIsNull,
    I64Eq,
    I64LeS,
    I64LtS,
    I64GeS,
    F64Add,
    F64Mul,
    F64Le,
    I32LtS,
    I32GtS,
    If,
    Else,
    End,
    Call(u32),
    ReturnCall(u32),
    Other,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum HostRole {
    Add,
    Sub,
    StringEq,
    /// The byte-array concatenation helper (`String.concat` lowering): takes a
    /// container array of string-arrays, returns the byte-concatenated array.
    StringConcat,
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

/// Which arithmetic contract the body-recursion combinator obeys. The bignum
/// `add` and `mul` helpers are not byte-distinguishable (both use i64 add/sub/mul
/// internally), so this is read from the MODEL operator — the trusted source of
/// what the function computes, the same spec the whole certificate is stated
/// against. The checker re-derives it from the model too, so the host still pins.
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
    /// Concrete reference to module type `t` (e.g. the Int carrier struct).
    Ref(u32),
    I64,
    I32,
    F64,
    Other,
}
