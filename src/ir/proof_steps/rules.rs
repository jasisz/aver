//! The wall: the closed set of rules a step may apply, each an equation
//! schema over named binders with equational premises.
//!
//! The same table exists three times, on purpose: here (producers
//! instantiate it), in the Aver replayer (`tools/proof-kernel/`, which
//! re-derives every instance), and as the Lean lemmas the renderer names
//! (`codegen::lean::proof_steps`). Rule identifiers are stable strings;
//! changing a rule's meaning means a new identifier and a new
//! [`super::FORMAT_VERSION`].

use crate::ast::BinOp;
use crate::ir::hir::BuiltinIntrinsic;

use super::Eqn;
use super::term::{self, Term, binop, bool_and, bool_not, bool_or, boolean, var};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WallRule {
    // Bool connectives at a literal argument.
    AndTrueL,
    AndFalseL,
    AndTrueR,
    AndFalseR,
    OrTrueL,
    OrFalseL,
    OrTrueR,
    OrFalseR,
    NotTrue,
    NotFalse,
    // Projections of a true conjunction.
    AndElimL,
    AndElimR,
    // A decided Int comparison decides its complement.
    LeOfNotGt,
    LeFalseOfGt,
    GeOfNotLt,
    GeFalseOfLt,
    LtOfNotGe,
    LtFalseOfGe,
    GtOfNotLe,
    GtFalseOfLe,
    EqOfNotNe,
    EqFalseOfNe,
    NeOfNotEq,
    NeFalseOfEq,
    // Two decided orders decide an equality.
    EqOfLeGe,
    EqFalseOfLt,
    EqFalseOfGt,
    // The product of two nonnegative Ints is nonnegative.
    MulNonneg,
    // A true Int equality lets one side stand for the other.
    EqOfBeq,
    // Equality (not on a value that may hold a Float) is reflexive.
    BeqRefl,
    // Equality reads the same with its sides swapped.
    BeqSymm,
    // Ring laws of Int.
    AddComm,
    MulComm,
    AddAssoc,
    MulAssoc,
    AddZero,
    ZeroAdd,
    MulOne,
    OneMul,
    SubZero,
    // `Int.div` and `Int.mod` by a divisor that is not 0.
    DivOk,
    ModOk,
    // Euclidean division by a positive divisor.
    DivModRecompose,
    DivRange,
    ModRange,
    // `Int.min`, `Int.max` and `Int.abs` by the comparison that picks their
    // value: `a <= b` for the smaller and the larger of two, `a < 0` for
    // the absolute value (`0 - a` then, else `a`).
    MinLe,
    MinGt,
    MaxLe,
    MaxGt,
    AbsNeg,
    AbsNonneg,
    // The bit-level view of Int (infinite two's complement): complement is
    // `-a - 1`, a shift left by `n >= 0` multiplies by `2^n`, a mask
    // `2^n - 1` keeps the remainder by `2^n` and a mask `2^n` keeps that
    // bit of the quotient, for every sign of `a`.
    BitsNot,
    BitsShl,
    BitsAndLow,
    BitsAndBit,
    // Constructor equations of the list builtins: each builtin on `[]` and
    // on `List.prepend(x, a)`, with Aver's truncation of a count (a count
    // at or below zero takes nothing and drops nothing).
    ConcatNil,
    ConcatCons,
    LenNil,
    LenCons,
    TakeNil,
    TakeConsLe,
    TakeConsGt,
    DropNil,
    DropConsLe,
    DropConsGt,
    ReverseNil,
    ReverseCons,
    // `List.contains` on `[]`, and on `List.prepend(x, a)` where the value
    // looked for is the element in front (`(y == x) = true`: found) or is
    // not (`(y == x) = false`: the rest decides). Both bind `x` and `y` at
    // a type whose `==` is equality, the only types the kernel takes
    // `x == x` as true at; a Float is not equal to itself (`0.0 / 0.0`).
    ContainsNil,
    ContainsConsFound,
    ContainsConsOther,
    // The Map builtins on the empty map and on `Map.set(m, k, v)`: a read
    // at the key just set, at another key (`k != k2`), and the size after a
    // set at a key the map holds or does not hold.
    MapGetEmpty,
    MapGetSetSame,
    MapGetSetOther,
    MapHasEmpty,
    MapHasSetSame,
    MapHasSetOther,
    MapLenEmpty,
    MapLenSetPresent,
    MapLenSetAbsent,
    // Vectors: the list a vector holds, a read below zero or past the end,
    // a write out of range, a read after a write at the same and at
    // another index, the length after a write, and a vector made by
    // `Vector.new` with a literal size.
    VecToListOfList,
    VecOfListToList,
    VecLenToList,
    VecGetNegative,
    VecGetPastEnd,
    VecSetOutOfRange,
    VecGetSetSame,
    VecGetSetOther,
    VecLenSet,
    VecLenNew,
    VecGetNew,
    // `Option.withDefault` and `Result.withDefault` on each constructor:
    // the value a `Some` or an `Ok` holds, the default for `None` or an
    // `Err`.
    OptionDefaultSome,
    OptionDefaultNone,
    ResultDefaultOk,
    ResultDefaultErr,
}

impl WallRule {
    pub const ALL: [WallRule; 94] = [
        WallRule::AndTrueL,
        WallRule::AndFalseL,
        WallRule::AndTrueR,
        WallRule::AndFalseR,
        WallRule::OrTrueL,
        WallRule::OrFalseL,
        WallRule::OrTrueR,
        WallRule::OrFalseR,
        WallRule::NotTrue,
        WallRule::NotFalse,
        WallRule::AndElimL,
        WallRule::AndElimR,
        WallRule::LeOfNotGt,
        WallRule::LeFalseOfGt,
        WallRule::GeOfNotLt,
        WallRule::GeFalseOfLt,
        WallRule::LtOfNotGe,
        WallRule::LtFalseOfGe,
        WallRule::GtOfNotLe,
        WallRule::GtFalseOfLe,
        WallRule::EqOfNotNe,
        WallRule::EqFalseOfNe,
        WallRule::NeOfNotEq,
        WallRule::NeFalseOfEq,
        WallRule::EqOfLeGe,
        WallRule::EqFalseOfLt,
        WallRule::EqFalseOfGt,
        WallRule::MulNonneg,
        WallRule::EqOfBeq,
        WallRule::BeqRefl,
        WallRule::BeqSymm,
        WallRule::AddComm,
        WallRule::MulComm,
        WallRule::AddAssoc,
        WallRule::MulAssoc,
        WallRule::AddZero,
        WallRule::ZeroAdd,
        WallRule::MulOne,
        WallRule::OneMul,
        WallRule::SubZero,
        WallRule::DivOk,
        WallRule::ModOk,
        WallRule::DivModRecompose,
        WallRule::DivRange,
        WallRule::ModRange,
        WallRule::MinLe,
        WallRule::MinGt,
        WallRule::MaxLe,
        WallRule::MaxGt,
        WallRule::AbsNeg,
        WallRule::AbsNonneg,
        WallRule::BitsNot,
        WallRule::BitsShl,
        WallRule::BitsAndLow,
        WallRule::BitsAndBit,
        WallRule::ConcatNil,
        WallRule::ConcatCons,
        WallRule::LenNil,
        WallRule::LenCons,
        WallRule::TakeNil,
        WallRule::TakeConsLe,
        WallRule::TakeConsGt,
        WallRule::DropNil,
        WallRule::DropConsLe,
        WallRule::DropConsGt,
        WallRule::ReverseNil,
        WallRule::ReverseCons,
        WallRule::ContainsNil,
        WallRule::ContainsConsFound,
        WallRule::ContainsConsOther,
        WallRule::MapGetEmpty,
        WallRule::MapGetSetSame,
        WallRule::MapGetSetOther,
        WallRule::MapHasEmpty,
        WallRule::MapHasSetSame,
        WallRule::MapHasSetOther,
        WallRule::MapLenEmpty,
        WallRule::MapLenSetPresent,
        WallRule::MapLenSetAbsent,
        WallRule::VecToListOfList,
        WallRule::VecOfListToList,
        WallRule::VecLenToList,
        WallRule::VecGetNegative,
        WallRule::VecGetPastEnd,
        WallRule::VecSetOutOfRange,
        WallRule::VecGetSetSame,
        WallRule::VecGetSetOther,
        WallRule::VecLenSet,
        WallRule::VecLenNew,
        WallRule::VecGetNew,
        WallRule::OptionDefaultSome,
        WallRule::OptionDefaultNone,
        WallRule::ResultDefaultOk,
        WallRule::ResultDefaultErr,
    ];

    /// Stable identifier, shared with the replayer.
    pub fn id(self) -> &'static str {
        match self {
            WallRule::AndTrueL => "bool.and.true_l",
            WallRule::AndFalseL => "bool.and.false_l",
            WallRule::AndTrueR => "bool.and.true_r",
            WallRule::AndFalseR => "bool.and.false_r",
            WallRule::OrTrueL => "bool.or.true_l",
            WallRule::OrFalseL => "bool.or.false_l",
            WallRule::OrTrueR => "bool.or.true_r",
            WallRule::OrFalseR => "bool.or.false_r",
            WallRule::NotTrue => "bool.not.true",
            WallRule::NotFalse => "bool.not.false",
            WallRule::AndElimL => "bool.and.elim_l",
            WallRule::AndElimR => "bool.and.elim_r",
            WallRule::LeOfNotGt => "int.le.of_not_gt",
            WallRule::LeFalseOfGt => "int.le.false_of_gt",
            WallRule::GeOfNotLt => "int.ge.of_not_lt",
            WallRule::GeFalseOfLt => "int.ge.false_of_lt",
            WallRule::LtOfNotGe => "int.lt.of_not_ge",
            WallRule::LtFalseOfGe => "int.lt.false_of_ge",
            WallRule::GtOfNotLe => "int.gt.of_not_le",
            WallRule::GtFalseOfLe => "int.gt.false_of_le",
            WallRule::EqOfNotNe => "int.eq.of_not_ne",
            WallRule::EqFalseOfNe => "int.eq.false_of_ne",
            WallRule::NeOfNotEq => "int.ne.of_not_eq",
            WallRule::NeFalseOfEq => "int.ne.false_of_eq",
            WallRule::EqOfLeGe => "int.eq.of_le_ge",
            WallRule::MulNonneg => "int.mul_nonneg",
            WallRule::EqFalseOfLt => "int.eq.false_of_lt",
            WallRule::EqFalseOfGt => "int.eq.false_of_gt",
            WallRule::EqOfBeq => "int.eq.of_beq",
            WallRule::BeqRefl => "bool.beq.refl",
            WallRule::BeqSymm => "bool.beq.symm",
            WallRule::AddComm => "int.add_comm",
            WallRule::MulComm => "int.mul_comm",
            WallRule::AddAssoc => "int.add_assoc",
            WallRule::MulAssoc => "int.mul_assoc",
            WallRule::AddZero => "int.add_zero",
            WallRule::ZeroAdd => "int.zero_add",
            WallRule::MulOne => "int.mul_one",
            WallRule::OneMul => "int.one_mul",
            WallRule::SubZero => "int.sub_zero",
            WallRule::DivOk => "int.div.ok",
            WallRule::ModOk => "int.mod.ok",
            WallRule::DivModRecompose => "int.div_mod_recompose",
            WallRule::DivRange => "int.div_range",
            WallRule::ModRange => "int.mod_range",
            WallRule::MinLe => "int.min.le",
            WallRule::MinGt => "int.min.gt",
            WallRule::MaxLe => "int.max.le",
            WallRule::MaxGt => "int.max.gt",
            WallRule::AbsNeg => "int.abs.neg",
            WallRule::AbsNonneg => "int.abs.nonneg",
            WallRule::BitsNot => "bits.not",
            WallRule::BitsShl => "bits.shl",
            WallRule::BitsAndLow => "bits.and_low",
            WallRule::BitsAndBit => "bits.and_bit",
            WallRule::ConcatNil => "list.concat.nil",
            WallRule::ConcatCons => "list.concat.cons",
            WallRule::LenNil => "list.len.nil",
            WallRule::LenCons => "list.len.cons",
            WallRule::TakeNil => "list.take.nil",
            WallRule::TakeConsLe => "list.take.cons_le",
            WallRule::TakeConsGt => "list.take.cons_gt",
            WallRule::DropNil => "list.drop.nil",
            WallRule::DropConsLe => "list.drop.cons_le",
            WallRule::DropConsGt => "list.drop.cons_gt",
            WallRule::ReverseNil => "list.reverse.nil",
            WallRule::ReverseCons => "list.reverse.cons",
            WallRule::ContainsNil => "list.contains.nil",
            WallRule::ContainsConsFound => "list.contains.cons_found",
            WallRule::ContainsConsOther => "list.contains.cons_other",
            WallRule::MapGetEmpty => "map.get.empty",
            WallRule::MapGetSetSame => "map.get.set_same",
            WallRule::MapGetSetOther => "map.get.set_other",
            WallRule::MapHasEmpty => "map.has.empty",
            WallRule::MapHasSetSame => "map.has.set_same",
            WallRule::MapHasSetOther => "map.has.set_other",
            WallRule::MapLenEmpty => "map.len.empty",
            WallRule::MapLenSetPresent => "map.len.set_present",
            WallRule::MapLenSetAbsent => "map.len.set_absent",
            WallRule::VecToListOfList => "vector.to_list.of_list",
            WallRule::VecOfListToList => "vector.of_list.to_list",
            WallRule::VecLenToList => "vector.len.to_list",
            WallRule::VecGetNegative => "vector.get.negative",
            WallRule::VecGetPastEnd => "vector.get.past_end",
            WallRule::VecSetOutOfRange => "vector.set.out_of_range",
            WallRule::VecGetSetSame => "vector.get.set_same",
            WallRule::VecGetSetOther => "vector.get.set_other",
            WallRule::VecLenSet => "vector.len.set",
            WallRule::VecLenNew => "vector.len.new",
            WallRule::VecGetNew => "vector.get.new",
            WallRule::OptionDefaultSome => "option.with_default.some",
            WallRule::OptionDefaultNone => "option.with_default.none",
            WallRule::ResultDefaultOk => "result.with_default.ok",
            WallRule::ResultDefaultErr => "result.with_default.err",
        }
    }

    pub fn from_id(id: &str) -> Option<WallRule> {
        WallRule::ALL.iter().copied().find(|r| r.id() == id)
    }

    /// Binder names, in the order a substitution lists them.
    pub fn binders(self) -> &'static [&'static str] {
        match self {
            WallRule::AndTrueL | WallRule::AndFalseL | WallRule::OrTrueL | WallRule::OrFalseL => {
                &["b"]
            }
            WallRule::AndTrueR | WallRule::AndFalseR | WallRule::OrTrueR | WallRule::OrFalseR => {
                &["a"]
            }
            WallRule::NotTrue | WallRule::NotFalse => &[],
            WallRule::AddZero
            | WallRule::ZeroAdd
            | WallRule::MulOne
            | WallRule::OneMul
            | WallRule::SubZero => &["a"],
            WallRule::AddAssoc | WallRule::MulAssoc => &["a", "b", "c"],
            WallRule::DivModRecompose | WallRule::ModRange | WallRule::DivOk | WallRule::ModOk => {
                &["a", "k"]
            }
            WallRule::BeqRefl => &["a"],
            WallRule::DivRange => &["a", "k", "m", "n"],
            WallRule::BitsNot | WallRule::AbsNeg | WallRule::AbsNonneg => &["a"],
            WallRule::ContainsNil => &["y"],
            WallRule::ContainsConsFound | WallRule::ContainsConsOther => &["x", "a", "y"],
            WallRule::BitsShl => &["a", "n", "k"],
            WallRule::BitsAndLow => &["a", "m", "k", "n"],
            WallRule::BitsAndBit => &["a", "k", "n"],
            WallRule::ConcatNil => &["b"],
            WallRule::ConcatCons => &["x", "a", "b"],
            WallRule::LenNil | WallRule::ReverseNil => &[],
            WallRule::LenCons | WallRule::ReverseCons => &["x", "a"],
            WallRule::TakeNil | WallRule::DropNil => &["n"],
            WallRule::TakeConsLe
            | WallRule::TakeConsGt
            | WallRule::DropConsLe
            | WallRule::DropConsGt => &["x", "a", "n"],
            WallRule::MapGetEmpty | WallRule::MapHasEmpty => &["k"],
            WallRule::MapLenEmpty => &[],
            WallRule::MapGetSetSame
            | WallRule::MapHasSetSame
            | WallRule::MapLenSetPresent
            | WallRule::MapLenSetAbsent => &["m", "k", "v"],
            WallRule::MapGetSetOther | WallRule::MapHasSetOther => &["m", "k", "v", "k2"],
            WallRule::VecToListOfList => &["l"],
            WallRule::VecOfListToList => &["v"],
            WallRule::VecLenToList => &["v"],
            WallRule::VecGetNegative => &["v", "i"],
            WallRule::VecGetPastEnd => &["v", "i"],
            WallRule::VecSetOutOfRange => &["v", "i", "x"],
            WallRule::VecGetSetSame => &["v", "i", "x"],
            WallRule::VecGetSetOther => &["v", "i", "x", "j"],
            WallRule::VecLenSet => &["v", "i", "x"],
            WallRule::VecLenNew => &["n", "x"],
            WallRule::VecGetNew => &["n", "x", "i"],
            WallRule::OptionDefaultSome | WallRule::ResultDefaultOk => &["x", "d"],
            WallRule::OptionDefaultNone => &["d"],
            WallRule::ResultDefaultErr => &["e", "d"],

            _ => &["a", "b"],
        }
    }

    /// The rule's premises and conclusion over its binders.
    pub fn schema(self) -> (Vec<Eqn>, Eqn) {
        let a = || var("a");
        let b = || var("b");
        let c = || var("c");
        let t = || boolean(true);
        let f = || boolean(false);
        let i = |n: i64| term::int(&n.into());
        let cmp = |op: BinOp, x: Term, y: Term| binop(op, x, y);
        let is = |x: Term, v: bool| Eqn::new(x, boolean(v));
        // Complement rules: premise `(a P b) = v`, conclusion `(a Q b) = w`.
        let compl = |p: BinOp, v: bool, q: BinOp, w: bool| {
            (vec![is(cmp(p, a(), b()), v)], is(cmp(q, a(), b()), w))
        };
        let div = |x: Term, k: Term| term::intrinsic(BuiltinIntrinsic::IntDivEuclid, vec![x, k]);
        let modu = |x: Term, k: Term| term::intrinsic(BuiltinIntrinsic::IntModEuclid, vec![x, k]);
        let shl = |x: Term, n: Term| term::intrinsic(BuiltinIntrinsic::BitsShiftLeft, vec![x, n]);
        let bits_and =
            |x: Term, y: Term| term::builtin("Bits.and", vec![x, y], Some(crate::ast::Type::Int));
        let x = || var("x");
        let n = || var("n");
        let nil = term::nil;
        let cons = |h: Term, t: Term| term::builtin("List.prepend", vec![h, t], None);
        let list = |name: &str, args: Vec<Term>| term::builtin(name, args, None);
        let len = |l: Term| term::builtin("List.len", vec![l], Some(crate::ast::Type::Int));
        let contains = |l: Term, y: Term| {
            term::builtin("List.contains", vec![l, y], Some(crate::ast::Type::Bool))
        };
        let at_most_zero = || is(cmp(BinOp::Lte, n(), i(0)), true);
        let above_zero = || is(cmp(BinOp::Gt, n(), i(0)), true);
        let pred = || binop(BinOp::Sub, n(), i(1));
        let (m, k, v, k2) = (|| var("m"), || var("k"), || var("v"), || var("k2"));
        let empty =
            || crate::ast::Spanned::bare(crate::ir::hir::ResolvedExpr::MapLiteral(Vec::new()));
        let set = || term::builtin("Map.set", vec![m(), k(), v()], None);
        let get = |map: Term, key: Term| term::builtin("Map.get", vec![map, key], None);
        let has = |map: Term, key: Term| {
            term::builtin("Map.has", vec![map, key], Some(crate::ast::Type::Bool))
        };
        let size = |map: Term| term::builtin("Map.len", vec![map], Some(crate::ast::Type::Int));
        let option = |c: crate::ir::hir::BuiltinCtor, args: Vec<Term>| {
            crate::ast::Spanned::bare(crate::ir::hir::ResolvedExpr::Ctor(
                crate::ir::hir::ResolvedCtor::Builtin(c),
                args,
            ))
        };
        let other = || is(cmp(BinOp::Neq, k(), k2()), true);
        let (vv, ii, jj, l, nn) = (
            || var("v"),
            || var("i"),
            || var("j"),
            || var("l"),
            || var("n"),
        );
        let vb = |name: &str, args: Vec<Term>, ty: Option<crate::ast::Type>| {
            term::builtin(name, args, ty)
        };
        let vlen = |t: Term| vb("Vector.len", vec![t], Some(crate::ast::Type::Int));
        let vget = |t: Term, at: Term| vb("Vector.get", vec![t, at], None);
        let vset = || vb("Vector.set", vec![vv(), ii(), x()], None);
        let written = || vb("Option.withDefault", vec![vset(), vv()], None);
        let made = || {
            crate::ast::Spanned::bare(crate::ir::hir::ResolvedExpr::Call(
                crate::ir::hir::ResolvedCallee::Intrinsic(BuiltinIntrinsic::VectorNew),
                vec![nn(), x()],
            ))
        };
        let none = || option(crate::ir::hir::BuiltinCtor::OptionNone, vec![]);
        let some = |t: Term| option(crate::ir::hir::BuiltinCtor::OptionSome, vec![t]);
        let in_range = |at: Term, end: Term| {
            is(
                bool_and(cmp(BinOp::Lte, i(0), at.clone()), cmp(BinOp::Lt, at, end)),
                true,
            )
        };
        match self {
            WallRule::AndTrueL => (vec![], Eqn::new(bool_and(t(), b()), b())),
            WallRule::AndFalseL => (vec![], Eqn::new(bool_and(f(), b()), f())),
            WallRule::AndTrueR => (vec![], Eqn::new(bool_and(a(), t()), a())),
            WallRule::AndFalseR => (vec![], Eqn::new(bool_and(a(), f()), f())),
            WallRule::OrTrueL => (vec![], Eqn::new(bool_or(t(), b()), t())),
            WallRule::OrFalseL => (vec![], Eqn::new(bool_or(f(), b()), b())),
            WallRule::OrTrueR => (vec![], Eqn::new(bool_or(a(), t()), t())),
            WallRule::OrFalseR => (vec![], Eqn::new(bool_or(a(), f()), a())),
            WallRule::NotTrue => (vec![], Eqn::new(bool_not(t()), f())),
            WallRule::NotFalse => (vec![], Eqn::new(bool_not(f()), t())),
            WallRule::AndElimL => (vec![is(bool_and(a(), b()), true)], is(a(), true)),
            WallRule::AndElimR => (vec![is(bool_and(a(), b()), true)], is(b(), true)),
            WallRule::LeOfNotGt => compl(BinOp::Gt, false, BinOp::Lte, true),
            WallRule::LeFalseOfGt => compl(BinOp::Gt, true, BinOp::Lte, false),
            WallRule::GeOfNotLt => compl(BinOp::Lt, false, BinOp::Gte, true),
            WallRule::GeFalseOfLt => compl(BinOp::Lt, true, BinOp::Gte, false),
            WallRule::LtOfNotGe => compl(BinOp::Gte, false, BinOp::Lt, true),
            WallRule::LtFalseOfGe => compl(BinOp::Gte, true, BinOp::Lt, false),
            WallRule::GtOfNotLe => compl(BinOp::Lte, false, BinOp::Gt, true),
            WallRule::GtFalseOfLe => compl(BinOp::Lte, true, BinOp::Gt, false),
            WallRule::EqOfNotNe => compl(BinOp::Neq, false, BinOp::Eq, true),
            WallRule::EqFalseOfNe => compl(BinOp::Neq, true, BinOp::Eq, false),
            WallRule::NeOfNotEq => compl(BinOp::Eq, false, BinOp::Neq, true),
            WallRule::NeFalseOfEq => compl(BinOp::Eq, true, BinOp::Neq, false),
            WallRule::EqOfLeGe => (
                vec![
                    is(cmp(BinOp::Lte, a(), b()), true),
                    is(cmp(BinOp::Gte, a(), b()), true),
                ],
                is(cmp(BinOp::Eq, a(), b()), true),
            ),
            WallRule::EqFalseOfLt => compl(BinOp::Lt, true, BinOp::Eq, false),
            WallRule::EqFalseOfGt => compl(BinOp::Gt, true, BinOp::Eq, false),
            WallRule::MulNonneg => (
                vec![
                    is(cmp(BinOp::Lte, i(0), a()), true),
                    is(cmp(BinOp::Lte, i(0), b()), true),
                ],
                is(cmp(BinOp::Lte, i(0), binop(BinOp::Mul, a(), b())), true),
            ),
            WallRule::EqOfBeq => (vec![is(cmp(BinOp::Eq, a(), b()), true)], Eqn::new(a(), b())),
            WallRule::BeqRefl => (vec![], is(cmp(BinOp::Eq, a(), a()), true)),
            WallRule::BeqSymm => (
                vec![],
                Eqn::new(cmp(BinOp::Eq, a(), b()), cmp(BinOp::Eq, b(), a())),
            ),
            WallRule::AddComm => (
                vec![],
                Eqn::new(binop(BinOp::Add, a(), b()), binop(BinOp::Add, b(), a())),
            ),
            WallRule::MulComm => (
                vec![],
                Eqn::new(binop(BinOp::Mul, a(), b()), binop(BinOp::Mul, b(), a())),
            ),
            WallRule::AddAssoc => (
                vec![],
                Eqn::new(
                    binop(BinOp::Add, binop(BinOp::Add, a(), b()), c()),
                    binop(BinOp::Add, a(), binop(BinOp::Add, b(), c())),
                ),
            ),
            WallRule::MulAssoc => (
                vec![],
                Eqn::new(
                    binop(BinOp::Mul, binop(BinOp::Mul, a(), b()), c()),
                    binop(BinOp::Mul, a(), binop(BinOp::Mul, b(), c())),
                ),
            ),
            WallRule::AddZero => (vec![], Eqn::new(binop(BinOp::Add, a(), i(0)), a())),
            WallRule::ZeroAdd => (vec![], Eqn::new(binop(BinOp::Add, i(0), a()), a())),
            WallRule::MulOne => (vec![], Eqn::new(binop(BinOp::Mul, a(), i(1)), a())),
            WallRule::OneMul => (vec![], Eqn::new(binop(BinOp::Mul, i(1), a()), a())),
            WallRule::SubZero => (vec![], Eqn::new(binop(BinOp::Sub, a(), i(0)), a())),
            WallRule::DivOk | WallRule::ModOk => {
                let (name, value) = match self {
                    WallRule::DivOk => ("Int.div", div(a(), var("k"))),
                    _ => ("Int.mod", modu(a(), var("k"))),
                };
                let result = crate::ast::Type::Result(
                    Box::new(crate::ast::Type::Int),
                    Box::new(crate::ast::Type::Str),
                );
                (
                    vec![is(cmp(BinOp::Eq, var("k"), i(0)), false)],
                    Eqn::new(
                        vb(name, vec![a(), var("k")], Some(result)),
                        option(crate::ir::hir::BuiltinCtor::ResultOk, vec![value]),
                    ),
                )
            }
            WallRule::DivModRecompose => (
                vec![is(cmp(BinOp::Gt, var("k"), i(0)), true)],
                Eqn::new(
                    binop(
                        BinOp::Add,
                        binop(BinOp::Mul, div(a(), var("k")), var("k")),
                        modu(a(), var("k")),
                    ),
                    a(),
                ),
            ),
            WallRule::DivRange => (
                vec![
                    is(
                        bool_and(cmp(BinOp::Lte, i(0), a()), cmp(BinOp::Lt, a(), var("m"))),
                        true,
                    ),
                    is(
                        bool_and(
                            cmp(BinOp::Gt, var("k"), i(0)),
                            cmp(BinOp::Eq, var("m"), binop(BinOp::Mul, var("n"), var("k"))),
                        ),
                        true,
                    ),
                ],
                is(
                    bool_and(
                        cmp(BinOp::Lte, i(0), div(a(), var("k"))),
                        cmp(BinOp::Lt, div(a(), var("k")), var("n")),
                    ),
                    true,
                ),
            ),
            WallRule::ModRange => (
                vec![is(cmp(BinOp::Gt, var("k"), i(0)), true)],
                is(
                    bool_and(
                        cmp(BinOp::Lte, i(0), modu(a(), var("k"))),
                        cmp(BinOp::Lt, modu(a(), var("k")), var("k")),
                    ),
                    true,
                ),
            ),
            WallRule::MinLe | WallRule::MinGt | WallRule::MaxLe | WallRule::MaxGt => {
                let le = matches!(self, WallRule::MinLe | WallRule::MaxLe);
                let min = matches!(self, WallRule::MinLe | WallRule::MinGt);
                let name = if min { "Int.min" } else { "Int.max" };
                (
                    vec![is(cmp(BinOp::Lte, a(), b()), le)],
                    Eqn::new(
                        term::builtin(name, vec![a(), b()], Some(crate::ast::Type::Int)),
                        if le == min { a() } else { b() },
                    ),
                )
            }
            WallRule::AbsNeg | WallRule::AbsNonneg => {
                let neg = self == WallRule::AbsNeg;
                (
                    vec![is(cmp(BinOp::Lt, a(), i(0)), neg)],
                    Eqn::new(
                        term::builtin("Int.abs", vec![a()], Some(crate::ast::Type::Int)),
                        if neg {
                            binop(BinOp::Sub, i(0), a())
                        } else {
                            a()
                        },
                    ),
                )
            }
            WallRule::BitsNot => (
                vec![],
                Eqn::new(
                    term::builtin("Bits.not", vec![a()], Some(crate::ast::Type::Int)),
                    binop(BinOp::Sub, binop(BinOp::Sub, i(0), a()), i(1)),
                ),
            ),
            WallRule::BitsShl => (
                vec![is(
                    bool_and(
                        cmp(BinOp::Lte, i(0), n()),
                        cmp(BinOp::Eq, k(), shl(i(1), n())),
                    ),
                    true,
                )],
                Eqn::new(shl(a(), n()), binop(BinOp::Mul, a(), k())),
            ),
            WallRule::BitsAndLow => (
                vec![is(
                    bool_and(
                        bool_and(
                            cmp(BinOp::Lte, i(0), n()),
                            cmp(BinOp::Eq, binop(BinOp::Add, m(), i(1)), k()),
                        ),
                        cmp(BinOp::Eq, k(), shl(i(1), n())),
                    ),
                    true,
                )],
                Eqn::new(bits_and(a(), m()), modu(a(), k())),
            ),
            WallRule::BitsAndBit => (
                vec![is(
                    bool_and(
                        cmp(BinOp::Lte, i(0), n()),
                        cmp(BinOp::Eq, k(), shl(i(1), n())),
                    ),
                    true,
                )],
                Eqn::new(
                    bits_and(a(), k()),
                    binop(BinOp::Mul, k(), modu(div(a(), k()), i(2))),
                ),
            ),
            WallRule::ConcatNil => (vec![], Eqn::new(list("List.concat", vec![nil(), b()]), b())),
            WallRule::ConcatCons => (
                vec![],
                Eqn::new(
                    list("List.concat", vec![cons(x(), a()), b()]),
                    cons(x(), list("List.concat", vec![a(), b()])),
                ),
            ),
            WallRule::LenNil => (vec![], Eqn::new(len(nil()), i(0))),
            WallRule::LenCons => (
                vec![],
                Eqn::new(len(cons(x(), a())), binop(BinOp::Add, len(a()), i(1))),
            ),
            WallRule::TakeNil => (vec![], Eqn::new(list("List.take", vec![nil(), n()]), nil())),
            WallRule::TakeConsLe => (
                vec![at_most_zero()],
                Eqn::new(list("List.take", vec![cons(x(), a()), n()]), nil()),
            ),
            WallRule::TakeConsGt => (
                vec![above_zero()],
                Eqn::new(
                    list("List.take", vec![cons(x(), a()), n()]),
                    cons(x(), list("List.take", vec![a(), pred()])),
                ),
            ),
            WallRule::DropNil => (vec![], Eqn::new(list("List.drop", vec![nil(), n()]), nil())),
            WallRule::DropConsLe => (
                vec![at_most_zero()],
                Eqn::new(list("List.drop", vec![cons(x(), a()), n()]), cons(x(), a())),
            ),
            WallRule::DropConsGt => (
                vec![above_zero()],
                Eqn::new(
                    list("List.drop", vec![cons(x(), a()), n()]),
                    list("List.drop", vec![a(), pred()]),
                ),
            ),
            WallRule::ReverseNil => (vec![], Eqn::new(list("List.reverse", vec![nil()]), nil())),
            WallRule::ReverseCons => (
                vec![],
                Eqn::new(
                    list("List.reverse", vec![cons(x(), a())]),
                    list(
                        "List.concat",
                        vec![list("List.reverse", vec![a()]), cons(x(), nil())],
                    ),
                ),
            ),
            WallRule::ContainsNil => (vec![], is(contains(nil(), var("y")), false)),
            WallRule::ContainsConsFound => (
                vec![is(cmp(BinOp::Eq, var("y"), x()), true)],
                is(contains(cons(x(), a()), var("y")), true),
            ),
            WallRule::ContainsConsOther => (
                vec![is(cmp(BinOp::Eq, var("y"), x()), false)],
                Eqn::new(contains(cons(x(), a()), var("y")), contains(a(), var("y"))),
            ),
            WallRule::MapGetEmpty => (
                vec![],
                Eqn::new(
                    get(empty(), k()),
                    option(crate::ir::hir::BuiltinCtor::OptionNone, vec![]),
                ),
            ),
            WallRule::MapGetSetSame => (
                vec![],
                Eqn::new(
                    get(set(), k()),
                    option(crate::ir::hir::BuiltinCtor::OptionSome, vec![v()]),
                ),
            ),
            WallRule::MapGetSetOther => (vec![other()], Eqn::new(get(set(), k2()), get(m(), k2()))),
            WallRule::MapHasEmpty => (vec![], is(has(empty(), k()), false)),
            WallRule::MapHasSetSame => (vec![], is(has(set(), k()), true)),
            WallRule::MapHasSetOther => (vec![other()], Eqn::new(has(set(), k2()), has(m(), k2()))),
            WallRule::MapLenEmpty => (vec![], Eqn::new(size(empty()), i(0))),
            WallRule::MapLenSetPresent => (
                vec![is(has(m(), k()), true)],
                Eqn::new(size(set()), size(m())),
            ),
            WallRule::MapLenSetAbsent => (
                vec![is(has(m(), k()), false)],
                Eqn::new(size(set()), binop(BinOp::Add, size(m()), i(1))),
            ),
            WallRule::VecToListOfList => (
                vec![],
                Eqn::new(
                    vb(
                        "List.fromVector",
                        vec![vb("Vector.fromList", vec![l()], None)],
                        None,
                    ),
                    l(),
                ),
            ),
            WallRule::VecOfListToList => (
                vec![],
                Eqn::new(
                    vb(
                        "Vector.fromList",
                        vec![vb("List.fromVector", vec![vv()], None)],
                        None,
                    ),
                    vv(),
                ),
            ),
            WallRule::VecLenToList => (
                vec![],
                Eqn::new(
                    vlen(vv()),
                    vb(
                        "List.len",
                        vec![vb("List.fromVector", vec![vv()], None)],
                        Some(crate::ast::Type::Int),
                    ),
                ),
            ),
            WallRule::VecGetNegative => (
                vec![is(cmp(BinOp::Lt, ii(), i(0)), true)],
                Eqn::new(vget(vv(), ii()), none()),
            ),
            WallRule::VecGetPastEnd => (
                vec![is(cmp(BinOp::Gte, ii(), vlen(vv())), true)],
                Eqn::new(vget(vv(), ii()), none()),
            ),
            WallRule::VecSetOutOfRange => (
                vec![is(
                    bool_or(
                        cmp(BinOp::Lt, ii(), i(0)),
                        cmp(BinOp::Gte, ii(), vlen(vv())),
                    ),
                    true,
                )],
                Eqn::new(vset(), none()),
            ),
            WallRule::VecGetSetSame => (
                vec![in_range(ii(), vlen(vv()))],
                Eqn::new(vget(written(), ii()), some(x())),
            ),
            WallRule::VecGetSetOther => (
                vec![
                    in_range(ii(), vlen(vv())),
                    is(cmp(BinOp::Neq, ii(), jj()), true),
                ],
                Eqn::new(vget(written(), jj()), vget(vv(), jj())),
            ),
            WallRule::VecLenSet => (vec![], Eqn::new(vlen(written()), vlen(vv()))),
            WallRule::VecLenNew => (
                vec![is(cmp(BinOp::Lte, i(0), nn()), true)],
                Eqn::new(vlen(made()), nn()),
            ),
            WallRule::VecGetNew => (
                vec![in_range(ii(), nn())],
                Eqn::new(vget(made(), ii()), some(x())),
            ),
            WallRule::OptionDefaultSome => (
                vec![],
                Eqn::new(
                    vb("Option.withDefault", vec![some(x()), var("d")], None),
                    x(),
                ),
            ),
            WallRule::OptionDefaultNone => (
                vec![],
                Eqn::new(
                    vb("Option.withDefault", vec![none(), var("d")], None),
                    var("d"),
                ),
            ),
            WallRule::ResultDefaultOk => (
                vec![],
                Eqn::new(
                    vb(
                        "Result.withDefault",
                        vec![
                            option(crate::ir::hir::BuiltinCtor::ResultOk, vec![x()]),
                            var("d"),
                        ],
                        None,
                    ),
                    x(),
                ),
            ),
            WallRule::ResultDefaultErr => (
                vec![],
                Eqn::new(
                    vb(
                        "Result.withDefault",
                        vec![
                            option(crate::ir::hir::BuiltinCtor::ResultErr, vec![var("e")]),
                            var("d"),
                        ],
                        None,
                    ),
                    var("d"),
                ),
            ),
        }
    }

    /// Premises and conclusion at a substitution; `None` when the
    /// substitution does not bind exactly the rule's binders.
    pub fn instantiate(self, subst: &[(String, Term)]) -> Option<(Vec<Eqn>, Eqn)> {
        let binders = self.binders();
        if subst.len() != binders.len() || subst.iter().zip(binders).any(|((k, _), b)| k != b) {
            return None;
        }
        let (premises, concl) = self.schema();
        let inst = |e: &Eqn| -> Option<Eqn> {
            Some(Eqn::new(
                term::subst(&e.lhs, subst).ok()?,
                term::subst(&e.rhs, subst).ok()?,
            ))
        };
        Some((
            premises.iter().map(inst).collect::<Option<Vec<_>>>()?,
            inst(&concl)?,
        ))
    }
}
