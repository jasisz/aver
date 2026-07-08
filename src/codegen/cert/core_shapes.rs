/// One recognised leaf of a `VariantDispatch` hit arm: what the arm computes
/// from the variant's Int payload.
#[derive(Clone, PartialEq)]
enum ArmLeaf {
    /// Return the projected payload unchanged.
    Proj,
    /// Combine the payload with a boxed constant through a contracted host:
    /// `k op x` when `const_first`, else `x op k`.
    HostOp {
        role: HostRole,
        k: i64,
        const_first: bool,
    },
}

/// The straight-line integer shape of one function inside a composition's call
/// closure. Every shape is unary (`Int -> Int`), non-recursive, branch-free, and
/// its simulation lemma is provable over the caller's composed code table by the
/// probe's straight-line skeleton (rcases the host/callee `Option`, cite, close).
#[derive(Clone)]
enum LeafShape {
    /// `[localGet 0, localGet 0, call add]` — model `x + x`.
    SelfSum { add_idx: u32 },
    /// `[localGet 0, call c1, ..., call cm]` (m >= 1), each `ci` a user function
    /// in the closure — model `cm (... (c1 x))`. The composition point.
    Chain { calls: Vec<u32> },
}

/// One function in a composition caller's transitive call closure: its verbatim
/// body (for the shared `CodeTbl`), its self index, and its recognised shape.
#[derive(Clone)]
struct ClosureEntry {
    name: String,
    self_idx: u32,
    nlocals: usize,
    ops: Vec<Op>,
    shape: LeafShape,
}

#[derive(Clone, PartialEq)]
enum VerbatimDefault {
    Null,
    F64Bits(u64),
    Array { type_idx: u32, bytes: Vec<u8> },
}

#[derive(Clone, PartialEq)]
enum StringEqDefault {
    Input,
    Verbatim(VerbatimDefault),
}

type StringEqVerbatimChain = (
    Vec<(VerbatimDefault, VerbatimDefault)>,
    StringEqDefault,
    u32,
);

#[derive(Clone, PartialEq)]
enum ConstructorField {
    Local(u32),
    Null,
}
