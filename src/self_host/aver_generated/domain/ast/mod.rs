#[allow(unused_imports)]
use crate::*;

::aver_rt::thin_sum! {
    #[derive(Debug)]
    pub Expr / Expr__view / Expr__pair {
        0 ExprInt(Expr__ExprInt),
        1 ExprFloat(Expr__ExprFloat),
        2 ExprStr(Expr__ExprStr),
        3 ExprBool(Expr__ExprBool),
        4 ExprBoolBranch(Expr__ExprBoolBranch),
        5 ExprVar(Expr__ExprVar),
        6 ExprSlot(Expr__ExprSlot),
        7 ExprBinopSlotInt(Expr__ExprBinopSlotInt),
        8 ExprBinopSlots(Expr__ExprBinopSlots),
        9 ExprCmpSlotInt(Expr__ExprCmpSlotInt),
        10 ExprCmpSlots(Expr__ExprCmpSlots),
        11 ExprVectorGetOrInt(Expr__ExprVectorGetOrInt),
        12 ExprIntModOrInt(Expr__ExprIntModOrInt),
        13 ExprAdd(Expr__ExprAdd),
        14 ExprSub(Expr__ExprSub),
        15 ExprMul(Expr__ExprMul),
        16 ExprDiv(Expr__ExprDiv),
        17 ExprNeg(Expr__ExprNeg),
        18 ExprEq(Expr__ExprEq),
        19 ExprNeq(Expr__ExprNeq),
        20 ExprLt(Expr__ExprLt),
        21 ExprGt(Expr__ExprGt),
        22 ExprLte(Expr__ExprLte),
        23 ExprGte(Expr__ExprGte),
        24 ExprConcat(Expr__ExprConcat),
        25 ExprTuple(Expr__ExprTuple),
        26 ExprList(Expr__ExprList),
        27 ExprRecord(Expr__ExprRecord),
        28 ExprFieldAccess(Expr__ExprFieldAccess),
        29 ExprCall(Expr__ExprCall),
        30 ExprCallDirect(Expr__ExprCallDirect),
        31 ExprCallBuiltin(Expr__ExprCallBuiltin),
        32 ExprCallBuiltinId(Expr__ExprCallBuiltinId),
        33 ExprMatch(Expr__ExprMatch),
        34 ExprPropagate(Expr__ExprPropagate),
        35 ExprIndependentProduct(Expr__ExprIndependentProduct),
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.view(), f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprInt(pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Expr__ExprInt {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprFloat(pub f64);

impl aver_rt::thin::HandOver for Expr__ExprFloat {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprStr(pub AverStr);

impl aver_rt::thin::HandOver for Expr__ExprStr {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprBool(pub bool);

impl aver_rt::thin::HandOver for Expr__ExprBool {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprBoolBranch(pub Expr, pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprBoolBranch {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
            && self.1.walk_eq(&other.1, walk)
            && self.2.walk_eq(&other.2, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprBoolBranch {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
        aver_rt::thin::HandOver::hand_over(self.2, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprVar(pub AverStr);

impl aver_rt::thin::HandOver for Expr__ExprVar {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprSlot(pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Expr__ExprSlot {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprBinopSlotInt(pub BinOp, pub aver_rt::AverInt, pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Expr__ExprBinopSlotInt {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprBinopSlots(pub BinOp, pub aver_rt::AverInt, pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Expr__ExprBinopSlots {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprCmpSlotInt(pub CmpOp, pub aver_rt::AverInt, pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Expr__ExprCmpSlotInt {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprCmpSlots(pub CmpOp, pub aver_rt::AverInt, pub aver_rt::AverInt);

impl aver_rt::thin::HandOver for Expr__ExprCmpSlots {}

#[derive(Clone, Debug)]
pub struct Expr__ExprVectorGetOrInt(pub Expr, pub Expr, pub aver_rt::AverInt);

impl PartialEq for Expr__ExprVectorGetOrInt {
    fn eq(&self, other: &Self) -> bool {
        self.2 == other.2 && self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Expr__ExprVectorGetOrInt {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.2 == other.2 && self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprVectorGetOrInt {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug)]
pub struct Expr__ExprIntModOrInt(pub Expr, pub Expr, pub aver_rt::AverInt);

impl PartialEq for Expr__ExprIntModOrInt {
    fn eq(&self, other: &Self) -> bool {
        self.2 == other.2 && self.0 == other.0 && self.1 == other.1
    }
}

impl aver_rt::WalkEq for Expr__ExprIntModOrInt {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.2 == other.2 && self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprIntModOrInt {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprAdd(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprAdd {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprAdd {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprSub(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprSub {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprSub {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprMul(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprMul {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprMul {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprDiv(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprDiv {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprDiv {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprNeg(pub Expr);

impl aver_rt::WalkEq for Expr__ExprNeg {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprNeg {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprEq(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprEq {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprEq {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprNeq(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprNeq {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprNeq {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprLt(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprLt {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprLt {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprGt(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprGt {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprGt {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprLte(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprLte {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprLte {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprGte(pub Expr, pub Expr);

impl aver_rt::WalkEq for Expr__ExprGte {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1.walk_eq(&other.1, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprGte {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
        aver_rt::thin::HandOver::hand_over(self.1, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprConcat(pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprConcat {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprTuple(pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprTuple {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprList(pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprList {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprRecord(pub AverStr, pub aver_rt::AverList<(AverStr, Expr)>);

impl aver_rt::thin::HandOver for Expr__ExprRecord {}

#[derive(Clone, Debug)]
pub struct Expr__ExprFieldAccess(pub Expr, pub AverStr);

impl PartialEq for Expr__ExprFieldAccess {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && self.0 == other.0
    }
}

impl aver_rt::WalkEq for Expr__ExprFieldAccess {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.1 == other.1 && self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprFieldAccess {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprCall(pub AverStr, pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprCall {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprCallDirect(pub aver_rt::AverInt, pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprCallDirect {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprCallBuiltin(pub AverStr, pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprCallBuiltin {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprCallBuiltinId(pub aver_rt::AverInt, pub aver_rt::AverList<Expr>);

impl aver_rt::thin::HandOver for Expr__ExprCallBuiltinId {}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprMatch(pub Expr, pub aver_rt::AverList<MatchArm>);

impl aver_rt::WalkEq for Expr__ExprMatch {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk) && self.1 == other.1
    }
}

impl aver_rt::thin::HandOver for Expr__ExprMatch {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr__ExprPropagate(pub Expr);

impl aver_rt::WalkEq for Expr__ExprPropagate {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        self.0.walk_eq(&other.0, walk)
    }
}

impl aver_rt::thin::HandOver for Expr__ExprPropagate {
    fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
        aver_rt::thin::HandOver::hand_over(self.0, pending);
    }
}

#[derive(Clone, Debug)]
pub struct Expr__ExprIndependentProduct(pub aver_rt::AverList<Expr>, pub bool);

impl PartialEq for Expr__ExprIndependentProduct {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1 && self.0 == other.0
    }
}

impl aver_rt::thin::HandOver for Expr__ExprIndependentProduct {}

impl aver_rt::WalkEq for Expr {
    fn walk_eq(&self, other: &Self, walk: aver_rt::Walk<'_>) -> bool {
        match Expr::view_pair(self, other) {
            Some(Expr__pair::ExprInt(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprFloat(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprStr(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprBool(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprBoolBranch(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprVar(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprSlot(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprBinopSlotInt(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprBinopSlots(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprCmpSlotInt(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprCmpSlots(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprVectorGetOrInt(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprIntModOrInt(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprAdd(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprSub(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprMul(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprDiv(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprNeg(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprEq(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprNeq(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprLt(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprGt(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprLte(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprGte(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprConcat(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprTuple(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprList(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprRecord(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprFieldAccess(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprCall(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprCallDirect(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprCallBuiltin(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprCallBuiltinId(a0, b0)) => a0 == b0,
            Some(Expr__pair::ExprMatch(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprPropagate(a0, b0)) => {
                walk.node(self, other, false, |walk| a0.walk_eq(b0, walk))
            }
            Some(Expr__pair::ExprIndependentProduct(a0, b0)) => a0 == b0,
            None => false,
        }
    }
}

impl PartialEq for Expr {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        aver_rt::walk_eq(self, other)
    }
}

impl aver_rt::AverDisplay for Expr {
    fn aver_display(&self) -> String {
        match self.view() {
            Expr__view::ExprInt(f) => format!("ExprInt({})", (&f.0).aver_display_inner()),
            Expr__view::ExprFloat(f) => format!("ExprFloat({})", (&f.0).aver_display_inner()),
            Expr__view::ExprStr(f) => format!("ExprStr({})", (&f.0).aver_display_inner()),
            Expr__view::ExprBool(f) => format!("ExprBool({})", (&f.0).aver_display_inner()),
            Expr__view::ExprBoolBranch(f) => format!(
                "ExprBoolBranch({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprVar(f) => format!("ExprVar({})", (&f.0).aver_display_inner()),
            Expr__view::ExprSlot(f) => format!("ExprSlot({})", (&f.0).aver_display_inner()),
            Expr__view::ExprBinopSlotInt(f) => format!(
                "ExprBinopSlotInt({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprBinopSlots(f) => format!(
                "ExprBinopSlots({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprCmpSlotInt(f) => format!(
                "ExprCmpSlotInt({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprCmpSlots(f) => format!(
                "ExprCmpSlots({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprVectorGetOrInt(f) => format!(
                "ExprVectorGetOrInt({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprIntModOrInt(f) => format!(
                "ExprIntModOrInt({})",
                vec![
                    (&f.0).aver_display_inner(),
                    (&f.1).aver_display_inner(),
                    (&f.2).aver_display_inner()
                ]
                .join(", ")
            ),
            Expr__view::ExprAdd(f) => format!(
                "ExprAdd({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprSub(f) => format!(
                "ExprSub({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprMul(f) => format!(
                "ExprMul({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprDiv(f) => format!(
                "ExprDiv({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprNeg(f) => format!("ExprNeg({})", (&f.0).aver_display_inner()),
            Expr__view::ExprEq(f) => format!(
                "ExprEq({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprNeq(f) => format!(
                "ExprNeq({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprLt(f) => format!(
                "ExprLt({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprGt(f) => format!(
                "ExprGt({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprLte(f) => format!(
                "ExprLte({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprGte(f) => format!(
                "ExprGte({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprConcat(f) => format!("ExprConcat({})", (&f.0).aver_display_inner()),
            Expr__view::ExprTuple(f) => format!("ExprTuple({})", (&f.0).aver_display_inner()),
            Expr__view::ExprList(f) => format!("ExprList({})", (&f.0).aver_display_inner()),
            Expr__view::ExprRecord(f) => format!(
                "ExprRecord({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprFieldAccess(f) => format!(
                "ExprFieldAccess({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprCall(f) => format!(
                "ExprCall({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprCallDirect(f) => format!(
                "ExprCallDirect({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprCallBuiltin(f) => format!(
                "ExprCallBuiltin({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprCallBuiltinId(f) => format!(
                "ExprCallBuiltinId({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprMatch(f) => format!(
                "ExprMatch({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
            Expr__view::ExprPropagate(f) => {
                format!("ExprPropagate({})", (&f.0).aver_display_inner())
            }
            Expr__view::ExprIndependentProduct(f) => format!(
                "ExprIndependentProduct({})",
                vec![(&f.0).aver_display_inner(), (&f.1).aver_display_inner()].join(", ")
            ),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for Expr {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.Expr".to_string()),
        );
        match self.view() {
            Expr__view::ExprInt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprFloat(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprFloat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprStr(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprStr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprBool(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprBool".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprBoolBranch(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprBoolBranch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprVar(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprVar".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprSlot(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprSlot".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprBinopSlotInt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprBinopSlotInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprBinopSlots(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprBinopSlots".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprCmpSlotInt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprCmpSlotInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprCmpSlots(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprCmpSlots".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprVectorGetOrInt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprVectorGetOrInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprIntModOrInt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprIntModOrInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                        ReplayValue::to_replay_json((&f.2)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprAdd(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprAdd".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprSub(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprSub".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprMul(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprMul".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprDiv(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprDiv".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprNeg(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprNeg".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprEq(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprEq".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprNeq(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprNeq".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprLt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprLt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprGt(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprGt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprLte(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprLte".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprGte(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprGte".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprConcat(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprConcat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprTuple(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprTuple".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprList(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprList".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprRecord(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprRecord".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprFieldAccess(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprFieldAccess".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprCall(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprCall".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprCallDirect(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprCallDirect".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprCallBuiltin(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprCallBuiltin".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprCallBuiltinId(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprCallBuiltinId".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprMatch(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprMatch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprPropagate(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprPropagate".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json((&f.0))]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Expr__view::ExprIndependentProduct(f) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("ExprIndependentProduct".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json((&f.0)),
                        ReplayValue::to_replay_json((&f.1)),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.Expr" && type_name != "Expr" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.Expr, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "ExprInt" => Ok(Expr::ExprInt(Expr__ExprInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprInt missing field #{}", 0))?,
                )?,
            ))),
            "ExprFloat" => Ok(Expr::ExprFloat(Expr__ExprFloat(
                <f64 as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprFloat missing field #{}", 0))?,
                )?,
            ))),
            "ExprStr" => Ok(Expr::ExprStr(Expr__ExprStr(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprStr missing field #{}", 0))?,
                )?,
            ))),
            "ExprBool" => Ok(Expr::ExprBool(Expr__ExprBool(
                <bool as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprBool missing field #{}", 0))?,
                )?,
            ))),
            "ExprBoolBranch" => Ok(Expr::ExprBoolBranch(Expr__ExprBoolBranch(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprBoolBranch missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprBoolBranch missing field #{}", 1))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant ExprBoolBranch missing field #{}", 2))?,
                )?,
            ))),
            "ExprVar" => Ok(Expr::ExprVar(Expr__ExprVar(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprVar missing field #{}", 0))?,
                )?,
            ))),
            "ExprSlot" => Ok(Expr::ExprSlot(Expr__ExprSlot(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprSlot missing field #{}", 0))?,
                )?,
            ))),
            "ExprBinopSlotInt" => {
                Ok(Expr::ExprBinopSlotInt(Expr__ExprBinopSlotInt(
                    <BinOp as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(|| {
                        format!("$variant ExprBinopSlotInt missing field #{}", 0)
                    })?)?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(1).ok_or_else(|| {
                            format!("$variant ExprBinopSlotInt missing field #{}", 1)
                        })?,
                    )?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(2).ok_or_else(|| {
                            format!("$variant ExprBinopSlotInt missing field #{}", 2)
                        })?,
                    )?,
                )))
            }
            "ExprBinopSlots" => Ok(Expr::ExprBinopSlots(Expr__ExprBinopSlots(
                <BinOp as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprBinopSlots missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprBinopSlots missing field #{}", 1))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant ExprBinopSlots missing field #{}", 2))?,
                )?,
            ))),
            "ExprCmpSlotInt" => Ok(Expr::ExprCmpSlotInt(Expr__ExprCmpSlotInt(
                <CmpOp as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprCmpSlotInt missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprCmpSlotInt missing field #{}", 1))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant ExprCmpSlotInt missing field #{}", 2))?,
                )?,
            ))),
            "ExprCmpSlots" => Ok(Expr::ExprCmpSlots(Expr__ExprCmpSlots(
                <CmpOp as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprCmpSlots missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprCmpSlots missing field #{}", 1))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant ExprCmpSlots missing field #{}", 2))?,
                )?,
            ))),
            "ExprVectorGetOrInt" => {
                Ok(Expr::ExprVectorGetOrInt(Expr__ExprVectorGetOrInt(
                    <Expr as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(|| {
                        format!("$variant ExprVectorGetOrInt missing field #{}", 0)
                    })?)?,
                    <Expr as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(|| {
                        format!("$variant ExprVectorGetOrInt missing field #{}", 1)
                    })?)?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(2).ok_or_else(|| {
                            format!("$variant ExprVectorGetOrInt missing field #{}", 2)
                        })?,
                    )?,
                )))
            }
            "ExprIntModOrInt" => {
                Ok(Expr::ExprIntModOrInt(Expr__ExprIntModOrInt(
                    <Expr as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(|| {
                        format!("$variant ExprIntModOrInt missing field #{}", 0)
                    })?)?,
                    <Expr as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(|| {
                        format!("$variant ExprIntModOrInt missing field #{}", 1)
                    })?)?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(2).ok_or_else(|| {
                            format!("$variant ExprIntModOrInt missing field #{}", 2)
                        })?,
                    )?,
                )))
            }
            "ExprAdd" => Ok(Expr::ExprAdd(Expr__ExprAdd(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprAdd missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprAdd missing field #{}", 1))?,
                )?,
            ))),
            "ExprSub" => Ok(Expr::ExprSub(Expr__ExprSub(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprSub missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprSub missing field #{}", 1))?,
                )?,
            ))),
            "ExprMul" => Ok(Expr::ExprMul(Expr__ExprMul(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprMul missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprMul missing field #{}", 1))?,
                )?,
            ))),
            "ExprDiv" => Ok(Expr::ExprDiv(Expr__ExprDiv(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprDiv missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprDiv missing field #{}", 1))?,
                )?,
            ))),
            "ExprNeg" => Ok(Expr::ExprNeg(Expr__ExprNeg(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprNeg missing field #{}", 0))?,
                )?,
            ))),
            "ExprEq" => Ok(Expr::ExprEq(Expr__ExprEq(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprEq missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprEq missing field #{}", 1))?,
                )?,
            ))),
            "ExprNeq" => Ok(Expr::ExprNeq(Expr__ExprNeq(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprNeq missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprNeq missing field #{}", 1))?,
                )?,
            ))),
            "ExprLt" => Ok(Expr::ExprLt(Expr__ExprLt(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprLt missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprLt missing field #{}", 1))?,
                )?,
            ))),
            "ExprGt" => Ok(Expr::ExprGt(Expr__ExprGt(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprGt missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprGt missing field #{}", 1))?,
                )?,
            ))),
            "ExprLte" => Ok(Expr::ExprLte(Expr__ExprLte(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprLte missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprLte missing field #{}", 1))?,
                )?,
            ))),
            "ExprGte" => Ok(Expr::ExprGte(Expr__ExprGte(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprGte missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprGte missing field #{}", 1))?,
                )?,
            ))),
            "ExprConcat" => Ok(Expr::ExprConcat(Expr__ExprConcat(
                <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprConcat missing field #{}", 0))?,
                )?,
            ))),
            "ExprTuple" => Ok(Expr::ExprTuple(Expr__ExprTuple(
                <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprTuple missing field #{}", 0))?,
                )?,
            ))),
            "ExprList" => Ok(Expr::ExprList(Expr__ExprList(
                <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprList missing field #{}", 0))?,
                )?,
            ))),
            "ExprRecord" => Ok(Expr::ExprRecord(Expr__ExprRecord(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprRecord missing field #{}", 0))?,
                )?,
                <aver_rt::AverList<(AverStr, Expr)> as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprRecord missing field #{}", 1))?,
                )?,
            ))),
            "ExprFieldAccess" => {
                Ok(Expr::ExprFieldAccess(Expr__ExprFieldAccess(
                    <Expr as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(|| {
                        format!("$variant ExprFieldAccess missing field #{}", 0)
                    })?)?,
                    <AverStr as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                        || format!("$variant ExprFieldAccess missing field #{}", 1),
                    )?)?,
                )))
            }
            "ExprCall" => Ok(Expr::ExprCall(Expr__ExprCall(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprCall missing field #{}", 0))?,
                )?,
                <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprCall missing field #{}", 1))?,
                )?,
            ))),
            "ExprCallDirect" => Ok(Expr::ExprCallDirect(Expr__ExprCallDirect(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprCallDirect missing field #{}", 0))?,
                )?,
                <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprCallDirect missing field #{}", 1))?,
                )?,
            ))),
            "ExprCallBuiltin" => {
                Ok(Expr::ExprCallBuiltin(Expr__ExprCallBuiltin(
                    <AverStr as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(
                        || format!("$variant ExprCallBuiltin missing field #{}", 0),
                    )?)?,
                    <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                        fields.get(1).ok_or_else(|| {
                            format!("$variant ExprCallBuiltin missing field #{}", 1)
                        })?,
                    )?,
                )))
            }
            "ExprCallBuiltinId" => Ok(Expr::ExprCallBuiltinId(Expr__ExprCallBuiltinId(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields.get(0).ok_or_else(|| {
                        format!("$variant ExprCallBuiltinId missing field #{}", 0)
                    })?,
                )?,
                <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                    fields.get(1).ok_or_else(|| {
                        format!("$variant ExprCallBuiltinId missing field #{}", 1)
                    })?,
                )?,
            ))),
            "ExprMatch" => Ok(Expr::ExprMatch(Expr__ExprMatch(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprMatch missing field #{}", 0))?,
                )?,
                <aver_rt::AverList<MatchArm> as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant ExprMatch missing field #{}", 1))?,
                )?,
            ))),
            "ExprPropagate" => Ok(Expr::ExprPropagate(Expr__ExprPropagate(
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant ExprPropagate missing field #{}", 0))?,
                )?,
            ))),
            "ExprIndependentProduct" => {
                Ok(Expr::ExprIndependentProduct(Expr__ExprIndependentProduct(
                    <aver_rt::AverList<Expr> as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant ExprIndependentProduct missing field #{}", 0)
                        })?,
                    )?,
                    <bool as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(|| {
                        format!("$variant ExprIndependentProduct missing field #{}", 1)
                    })?)?,
                )))
            }
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.Expr",
                variant_name
            )),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expr,
    pub bindingSlots: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
}

impl aver_rt::AverDisplay for MatchArm {
    fn aver_display(&self) -> String {
        format!(
            "MatchArm({})",
            vec![
                format!("pattern: {}", self.pattern.aver_display_inner()),
                format!("body: {}", self.body.aver_display_inner()),
                format!("bindingSlots: {}", self.bindingSlots.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for MatchArm {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert(
            "pattern".to_string(),
            ReplayValue::to_replay_json(&self.pattern),
        );
        fields.insert("body".to_string(), ReplayValue::to_replay_json(&self.body));
        fields.insert(
            "bindingSlots".to_string(),
            ReplayValue::to_replay_json(&self.bindingSlots),
        );
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.MatchArm".to_string()),
        );
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != "Domain.Ast.MatchArm" && type_name != "MatchArm" {
            return Err(format!(
                "$record type mismatch: expected Domain.Ast.MatchArm, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            pattern: <Pattern as ReplayValue>::from_replay_json(
                fields
                    .get("pattern")
                    .ok_or_else(|| "$record MatchArm missing field 'pattern'".to_string())?,
            )?,
            body: <Expr as ReplayValue>::from_replay_json(
                fields
                    .get("body")
                    .ok_or_else(|| "$record MatchArm missing field 'body'".to_string())?,
            )?,
            bindingSlots:
                <aver_rt::AverMap<AverStr, aver_rt::AverInt> as ReplayValue>::from_replay_json(
                    fields.get("bindingSlots").ok_or_else(|| {
                        "$record MatchArm missing field 'bindingSlots'".to_string()
                    })?,
                )?,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    PatInt(aver_rt::AverInt),
    PatFloat(f64),
    PatBool(bool),
    PatStr(AverStr),
    PatEmpty,
    PatCons(AverStr, AverStr),
    PatConstructor(AverStr, aver_rt::AverList<AverStr>),
    PatConstructorId(aver_rt::AverInt, AverStr, aver_rt::AverList<AverStr>),
    PatTuple(aver_rt::AverList<Pattern>),
    PatWild,
    PatVar(AverStr),
}

impl aver_rt::AverDisplay for Pattern {
    fn aver_display(&self) -> String {
        match self {
            Pattern::PatInt(f0) => format!("PatInt({})", f0.aver_display_inner()),
            Pattern::PatFloat(f0) => format!("PatFloat({})", f0.aver_display_inner()),
            Pattern::PatBool(f0) => format!("PatBool({})", f0.aver_display_inner()),
            Pattern::PatStr(f0) => format!("PatStr({})", f0.aver_display_inner()),
            Pattern::PatEmpty => "PatEmpty".to_string(),
            Pattern::PatCons(f0, f1) => format!(
                "PatCons({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Pattern::PatConstructor(f0, f1) => format!(
                "PatConstructor({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Pattern::PatConstructorId(f0, f1, f2) => format!(
                "PatConstructorId({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Pattern::PatTuple(f0) => format!("PatTuple({})", f0.aver_display_inner()),
            Pattern::PatWild => "PatWild".to_string(),
            Pattern::PatVar(f0) => format!("PatVar({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for Pattern {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.Pattern".to_string()),
        );
        match self {
            Pattern::PatInt(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatFloat(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatFloat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatBool(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatBool".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatStr(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatStr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatEmpty => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatEmpty".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatCons(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatCons".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatConstructor(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatConstructor".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatConstructorId(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatConstructorId".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatTuple(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatTuple".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatWild => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatWild".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Pattern::PatVar(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("PatVar".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.Pattern" && type_name != "Pattern" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.Pattern, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "PatInt" => Ok(Pattern::PatInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant PatInt missing field #{}", 0))?,
                )?,
            )),
            "PatFloat" => Ok(Pattern::PatFloat(<f64 as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant PatFloat missing field #{}", 0))?,
            )?)),
            "PatBool" => Ok(Pattern::PatBool(<bool as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant PatBool missing field #{}", 0))?,
            )?)),
            "PatStr" => Ok(Pattern::PatStr(<AverStr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant PatStr missing field #{}", 0))?,
            )?)),
            "PatEmpty" => Ok(Pattern::PatEmpty),
            "PatCons" => Ok(Pattern::PatCons(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant PatCons missing field #{}", 0))?,
                )?,
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant PatCons missing field #{}", 1))?,
                )?,
            )),
            "PatConstructor" => Ok(Pattern::PatConstructor(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant PatConstructor missing field #{}", 0))?,
                )?,
                <aver_rt::AverList<AverStr> as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant PatConstructor missing field #{}", 1))?,
                )?,
            )),
            "PatConstructorId" => {
                Ok(Pattern::PatConstructorId(
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant PatConstructorId missing field #{}", 0)
                        })?,
                    )?,
                    <AverStr as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                        || format!("$variant PatConstructorId missing field #{}", 1),
                    )?)?,
                    <aver_rt::AverList<AverStr> as ReplayValue>::from_replay_json(
                        fields.get(2).ok_or_else(|| {
                            format!("$variant PatConstructorId missing field #{}", 2)
                        })?,
                    )?,
                ))
            }
            "PatTuple" => Ok(Pattern::PatTuple(
                <aver_rt::AverList<Pattern> as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant PatTuple missing field #{}", 0))?,
                )?,
            )),
            "PatWild" => Ok(Pattern::PatWild),
            "PatVar" => Ok(Pattern::PatVar(<AverStr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant PatVar missing field #{}", 0))?,
            )?)),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.Pattern",
                variant_name
            )),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    StmtBind(AverStr, Expr),
    StmtBindSlot(aver_rt::AverInt, Expr),
    StmtExpr(Expr),
}

impl aver_rt::AverDisplay for Stmt {
    fn aver_display(&self) -> String {
        match self {
            Stmt::StmtBind(f0, f1) => format!(
                "StmtBind({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Stmt::StmtBindSlot(f0, f1) => format!(
                "StmtBindSlot({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Stmt::StmtExpr(f0) => format!("StmtExpr({})", f0.aver_display_inner()),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for Stmt {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.Stmt".to_string()),
        );
        match self {
            Stmt::StmtBind(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("StmtBind".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Stmt::StmtBindSlot(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("StmtBindSlot".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            Stmt::StmtExpr(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("StmtExpr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.Stmt" && type_name != "Stmt" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.Stmt, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "StmtBind" => Ok(Stmt::StmtBind(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant StmtBind missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant StmtBind missing field #{}", 1))?,
                )?,
            )),
            "StmtBindSlot" => Ok(Stmt::StmtBindSlot(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant StmtBindSlot missing field #{}", 0))?,
                )?,
                <Expr as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant StmtBindSlot missing field #{}", 1))?,
                )?,
            )),
            "StmtExpr" => Ok(Stmt::StmtExpr(<Expr as ReplayValue>::from_replay_json(
                fields
                    .get(0)
                    .ok_or_else(|| format!("$variant StmtExpr missing field #{}", 0))?,
            )?)),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.Stmt",
                variant_name
            )),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FastLeaf {
    LeafConstInt(aver_rt::AverInt),
    LeafConstFloat(f64),
    LeafConstStr(AverStr),
    LeafConstBool(bool),
    LeafSlot(aver_rt::AverInt),
    LeafFieldAccess(aver_rt::AverInt, AverStr),
    LeafMapGet(aver_rt::AverInt, aver_rt::AverInt),
    LeafMapSet(aver_rt::AverInt, aver_rt::AverInt, aver_rt::AverInt),
    LeafMapHas(aver_rt::AverInt, aver_rt::AverInt),
    LeafMapRemove(aver_rt::AverInt, aver_rt::AverInt),
    LeafVectorNew(aver_rt::AverInt, aver_rt::AverInt),
    LeafVectorLen(aver_rt::AverInt),
    LeafVectorGetOrInt(aver_rt::AverInt, aver_rt::AverInt, aver_rt::AverInt),
    LeafBinopSlots(BinOp, aver_rt::AverInt, aver_rt::AverInt),
    LeafCmpSlots(CmpOp, aver_rt::AverInt, aver_rt::AverInt),
}

impl aver_rt::AverDisplay for FastLeaf {
    fn aver_display(&self) -> String {
        match self {
            FastLeaf::LeafConstInt(f0) => format!("LeafConstInt({})", f0.aver_display_inner()),
            FastLeaf::LeafConstFloat(f0) => format!("LeafConstFloat({})", f0.aver_display_inner()),
            FastLeaf::LeafConstStr(f0) => format!("LeafConstStr({})", f0.aver_display_inner()),
            FastLeaf::LeafConstBool(f0) => format!("LeafConstBool({})", f0.aver_display_inner()),
            FastLeaf::LeafSlot(f0) => format!("LeafSlot({})", f0.aver_display_inner()),
            FastLeaf::LeafFieldAccess(f0, f1) => format!(
                "LeafFieldAccess({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            FastLeaf::LeafMapGet(f0, f1) => format!(
                "LeafMapGet({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            FastLeaf::LeafMapSet(f0, f1, f2) => format!(
                "LeafMapSet({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            FastLeaf::LeafMapHas(f0, f1) => format!(
                "LeafMapHas({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            FastLeaf::LeafMapRemove(f0, f1) => format!(
                "LeafMapRemove({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            FastLeaf::LeafVectorNew(f0, f1) => format!(
                "LeafVectorNew({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            FastLeaf::LeafVectorLen(f0) => format!("LeafVectorLen({})", f0.aver_display_inner()),
            FastLeaf::LeafVectorGetOrInt(f0, f1, f2) => format!(
                "LeafVectorGetOrInt({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            FastLeaf::LeafBinopSlots(f0, f1, f2) => format!(
                "LeafBinopSlots({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            FastLeaf::LeafCmpSlots(f0, f1, f2) => format!(
                "LeafCmpSlots({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for FastLeaf {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.FastLeaf".to_string()),
        );
        match self {
            FastLeaf::LeafConstInt(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafConstInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafConstFloat(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafConstFloat".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafConstStr(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafConstStr".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafConstBool(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafConstBool".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafSlot(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafSlot".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafFieldAccess(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafFieldAccess".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafMapGet(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafMapGet".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafMapSet(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafMapSet".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafMapHas(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafMapHas".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafMapRemove(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafMapRemove".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafVectorNew(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafVectorNew".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafVectorLen(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafVectorLen".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafVectorGetOrInt(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafVectorGetOrInt".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafBinopSlots(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafBinopSlots".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FastLeaf::LeafCmpSlots(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("LeafCmpSlots".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.FastLeaf" && type_name != "FastLeaf" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.FastLeaf, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "LeafConstInt" => Ok(FastLeaf::LeafConstInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafConstInt missing field #{}", 0))?,
                )?,
            )),
            "LeafConstFloat" => Ok(FastLeaf::LeafConstFloat(
                <f64 as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafConstFloat missing field #{}", 0))?,
                )?,
            )),
            "LeafConstStr" => Ok(FastLeaf::LeafConstStr(
                <AverStr as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafConstStr missing field #{}", 0))?,
                )?,
            )),
            "LeafConstBool" => Ok(FastLeaf::LeafConstBool(
                <bool as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafConstBool missing field #{}", 0))?,
                )?,
            )),
            "LeafSlot" => Ok(FastLeaf::LeafSlot(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafSlot missing field #{}", 0))?,
                )?,
            )),
            "LeafFieldAccess" => {
                Ok(FastLeaf::LeafFieldAccess(
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant LeafFieldAccess missing field #{}", 0)
                        })?,
                    )?,
                    <AverStr as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                        || format!("$variant LeafFieldAccess missing field #{}", 1),
                    )?)?,
                ))
            }
            "LeafMapGet" => Ok(FastLeaf::LeafMapGet(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafMapGet missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafMapGet missing field #{}", 1))?,
                )?,
            )),
            "LeafMapSet" => Ok(FastLeaf::LeafMapSet(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafMapSet missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafMapSet missing field #{}", 1))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant LeafMapSet missing field #{}", 2))?,
                )?,
            )),
            "LeafMapHas" => Ok(FastLeaf::LeafMapHas(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafMapHas missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafMapHas missing field #{}", 1))?,
                )?,
            )),
            "LeafMapRemove" => Ok(FastLeaf::LeafMapRemove(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafMapRemove missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafMapRemove missing field #{}", 1))?,
                )?,
            )),
            "LeafVectorNew" => Ok(FastLeaf::LeafVectorNew(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafVectorNew missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafVectorNew missing field #{}", 1))?,
                )?,
            )),
            "LeafVectorLen" => Ok(FastLeaf::LeafVectorLen(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafVectorLen missing field #{}", 0))?,
                )?,
            )),
            "LeafVectorGetOrInt" => Ok(FastLeaf::LeafVectorGetOrInt(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields.get(0).ok_or_else(|| {
                        format!("$variant LeafVectorGetOrInt missing field #{}", 0)
                    })?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields.get(1).ok_or_else(|| {
                        format!("$variant LeafVectorGetOrInt missing field #{}", 1)
                    })?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields.get(2).ok_or_else(|| {
                        format!("$variant LeafVectorGetOrInt missing field #{}", 2)
                    })?,
                )?,
            )),
            "LeafBinopSlots" => Ok(FastLeaf::LeafBinopSlots(
                <BinOp as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafBinopSlots missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafBinopSlots missing field #{}", 1))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant LeafBinopSlots missing field #{}", 2))?,
                )?,
            )),
            "LeafCmpSlots" => Ok(FastLeaf::LeafCmpSlots(
                <CmpOp as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant LeafCmpSlots missing field #{}", 0))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant LeafCmpSlots missing field #{}", 1))?,
                )?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(2)
                        .ok_or_else(|| format!("$variant LeafCmpSlots missing field #{}", 2))?,
                )?,
            )),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.FastLeaf",
                variant_name
            )),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum FnFastPath {
    FastNone,
    FastSingleExpr,
    FastLeaf(FastLeaf),
    FastForwardCall(aver_rt::AverInt, aver_rt::AverIntList),
    FastBoolSlotBranch(aver_rt::AverInt, FastLeaf, FastLeaf),
    FastEqIntBranch(aver_rt::AverInt, aver_rt::AverInt, FastLeaf, FastLeaf),
    FastEqStringBranch(aver_rt::AverInt, AverStr, FastLeaf, FastLeaf),
    FastLtIntSlotsBranch(aver_rt::AverInt, aver_rt::AverInt, FastLeaf, FastLeaf),
    FastListSlotBranch(
        aver_rt::AverInt,
        FastLeaf,
        aver_rt::AverInt,
        aver_rt::AverInt,
        FastLeaf,
    ),
}

impl aver_rt::AverDisplay for FnFastPath {
    fn aver_display(&self) -> String {
        match self {
            FnFastPath::FastNone => "FastNone".to_string(),
            FnFastPath::FastSingleExpr => "FastSingleExpr".to_string(),
            FnFastPath::FastLeaf(f0) => format!("FastLeaf({})", f0.aver_display_inner()),
            FnFastPath::FastForwardCall(f0, f1) => format!(
                "FastForwardCall({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            FnFastPath::FastBoolSlotBranch(f0, f1, f2) => format!(
                "FastBoolSlotBranch({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            FnFastPath::FastEqIntBranch(f0, f1, f2, f3) => format!(
                "FastEqIntBranch({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner()
                ]
                .join(", ")
            ),
            FnFastPath::FastEqStringBranch(f0, f1, f2, f3) => format!(
                "FastEqStringBranch({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner()
                ]
                .join(", ")
            ),
            FnFastPath::FastLtIntSlotsBranch(f0, f1, f2, f3) => format!(
                "FastLtIntSlotsBranch({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner()
                ]
                .join(", ")
            ),
            FnFastPath::FastListSlotBranch(f0, f1, f2, f3, f4) => format!(
                "FastListSlotBranch({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner(),
                    f4.aver_display_inner()
                ]
                .join(", ")
            ),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for FnFastPath {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.FnFastPath".to_string()),
        );
        match self {
            FnFastPath::FastNone => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastNone".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastSingleExpr => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastSingleExpr".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastLeaf(f0) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastLeaf".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![ReplayValue::to_replay_json(f0)]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastForwardCall(f0, f1) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastForwardCall".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastBoolSlotBranch(f0, f1, f2) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastBoolSlotBranch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastEqIntBranch(f0, f1, f2, f3) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastEqIntBranch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                        ReplayValue::to_replay_json(f3),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastEqStringBranch(f0, f1, f2, f3) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastEqStringBranch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                        ReplayValue::to_replay_json(f3),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastLtIntSlotsBranch(f0, f1, f2, f3) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastLtIntSlotsBranch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                        ReplayValue::to_replay_json(f3),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            FnFastPath::FastListSlotBranch(f0, f1, f2, f3, f4) => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("FastListSlotBranch".to_string()),
                );
                payload.insert(
                    "fields".to_string(),
                    serde_json::Value::Array(vec![
                        ReplayValue::to_replay_json(f0),
                        ReplayValue::to_replay_json(f1),
                        ReplayValue::to_replay_json(f2),
                        ReplayValue::to_replay_json(f3),
                        ReplayValue::to_replay_json(f4),
                    ]),
                );
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.FnFastPath" && type_name != "FnFastPath" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.FnFastPath, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "FastNone" => Ok(FnFastPath::FastNone),
            "FastSingleExpr" => Ok(FnFastPath::FastSingleExpr),
            "FastLeaf" => Ok(FnFastPath::FastLeaf(
                <FastLeaf as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant FastLeaf missing field #{}", 0))?,
                )?,
            )),
            "FastForwardCall" => Ok(FnFastPath::FastForwardCall(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(
                    fields
                        .get(0)
                        .ok_or_else(|| format!("$variant FastForwardCall missing field #{}", 0))?,
                )?,
                <aver_rt::AverIntList as ReplayValue>::from_replay_json(
                    fields
                        .get(1)
                        .ok_or_else(|| format!("$variant FastForwardCall missing field #{}", 1))?,
                )?,
            )),
            "FastBoolSlotBranch" => {
                Ok(FnFastPath::FastBoolSlotBranch(
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant FastBoolSlotBranch missing field #{}", 0)
                        })?,
                    )?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                        || format!("$variant FastBoolSlotBranch missing field #{}", 1),
                    )?)?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(2).ok_or_else(
                        || format!("$variant FastBoolSlotBranch missing field #{}", 2),
                    )?)?,
                ))
            }
            "FastEqIntBranch" => {
                Ok(FnFastPath::FastEqIntBranch(
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant FastEqIntBranch missing field #{}", 0)
                        })?,
                    )?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(1).ok_or_else(|| {
                            format!("$variant FastEqIntBranch missing field #{}", 1)
                        })?,
                    )?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(2).ok_or_else(
                        || format!("$variant FastEqIntBranch missing field #{}", 2),
                    )?)?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(3).ok_or_else(
                        || format!("$variant FastEqIntBranch missing field #{}", 3),
                    )?)?,
                ))
            }
            "FastEqStringBranch" => {
                Ok(FnFastPath::FastEqStringBranch(
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant FastEqStringBranch missing field #{}", 0)
                        })?,
                    )?,
                    <AverStr as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                        || format!("$variant FastEqStringBranch missing field #{}", 1),
                    )?)?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(2).ok_or_else(
                        || format!("$variant FastEqStringBranch missing field #{}", 2),
                    )?)?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(3).ok_or_else(
                        || format!("$variant FastEqStringBranch missing field #{}", 3),
                    )?)?,
                ))
            }
            "FastLtIntSlotsBranch" => Ok(FnFastPath::FastLtIntSlotsBranch(
                <aver_rt::AverInt as ReplayValue>::from_replay_json(fields.get(0).ok_or_else(
                    || format!("$variant FastLtIntSlotsBranch missing field #{}", 0),
                )?)?,
                <aver_rt::AverInt as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                    || format!("$variant FastLtIntSlotsBranch missing field #{}", 1),
                )?)?,
                <FastLeaf as ReplayValue>::from_replay_json(fields.get(2).ok_or_else(|| {
                    format!("$variant FastLtIntSlotsBranch missing field #{}", 2)
                })?)?,
                <FastLeaf as ReplayValue>::from_replay_json(fields.get(3).ok_or_else(|| {
                    format!("$variant FastLtIntSlotsBranch missing field #{}", 3)
                })?)?,
            )),
            "FastListSlotBranch" => {
                Ok(FnFastPath::FastListSlotBranch(
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(0).ok_or_else(|| {
                            format!("$variant FastListSlotBranch missing field #{}", 0)
                        })?,
                    )?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(1).ok_or_else(
                        || format!("$variant FastListSlotBranch missing field #{}", 1),
                    )?)?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(2).ok_or_else(|| {
                            format!("$variant FastListSlotBranch missing field #{}", 2)
                        })?,
                    )?,
                    <aver_rt::AverInt as ReplayValue>::from_replay_json(
                        fields.get(3).ok_or_else(|| {
                            format!("$variant FastListSlotBranch missing field #{}", 3)
                        })?,
                    )?,
                    <FastLeaf as ReplayValue>::from_replay_json(fields.get(4).ok_or_else(
                        || format!("$variant FastListSlotBranch missing field #{}", 4),
                    )?)?,
                ))
            }
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.FnFastPath",
                variant_name
            )),
        }
    }
}

pub type FnDef = Box<FnDef__Fields>;

#[derive(Clone, Debug)]
pub struct FnDef__Fields {
    pub name: AverStr,
    pub params: aver_rt::AverList<AverStr>,
    pub body: aver_rt::AverList<Stmt>,
    pub slotCount: aver_rt::AverInt,
    pub slotMap: aver_rt::AverMap<AverStr, aver_rt::AverInt>,
    pub fastPath: FnFastPath,
    pub tailLoop: bool,
}

impl PartialEq for FnDef__Fields {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.slotCount == other.slotCount
            && self.tailLoop == other.tailLoop
            && self.params == other.params
            && self.body == other.body
            && self.slotMap == other.slotMap
            && self.fastPath == other.fastPath
    }
}

impl aver_rt::AverDisplay for FnDef__Fields {
    fn aver_display(&self) -> String {
        format!(
            "FnDef({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("params: {}", self.params.aver_display_inner()),
                format!("body: {}", self.body.aver_display_inner()),
                format!("slotCount: {}", self.slotCount.aver_display_inner()),
                format!("slotMap: {}", self.slotMap.aver_display_inner()),
                format!("fastPath: {}", self.fastPath.aver_display_inner()),
                format!("tailLoop: {}", self.tailLoop.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for FnDef {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("name".to_string(), ReplayValue::to_replay_json(&self.name));
        fields.insert(
            "params".to_string(),
            ReplayValue::to_replay_json(&self.params),
        );
        fields.insert("body".to_string(), ReplayValue::to_replay_json(&self.body));
        fields.insert(
            "slotCount".to_string(),
            ReplayValue::to_replay_json(&self.slotCount),
        );
        fields.insert(
            "slotMap".to_string(),
            ReplayValue::to_replay_json(&self.slotMap),
        );
        fields.insert(
            "fastPath".to_string(),
            ReplayValue::to_replay_json(&self.fastPath),
        );
        fields.insert(
            "tailLoop".to_string(),
            ReplayValue::to_replay_json(&self.tailLoop),
        );
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.FnDef".to_string()),
        );
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != "Domain.Ast.FnDef" && type_name != "FnDef" {
            return Err(format!(
                "$record type mismatch: expected Domain.Ast.FnDef, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Box::new(FnDef__Fields {
            name: <AverStr as ReplayValue>::from_replay_json(
                fields
                    .get("name")
                    .ok_or_else(|| "$record FnDef missing field 'name'".to_string())?,
            )?,
            params: <aver_rt::AverList<AverStr> as ReplayValue>::from_replay_json(
                fields
                    .get("params")
                    .ok_or_else(|| "$record FnDef missing field 'params'".to_string())?,
            )?,
            body: <aver_rt::AverList<Stmt> as ReplayValue>::from_replay_json(
                fields
                    .get("body")
                    .ok_or_else(|| "$record FnDef missing field 'body'".to_string())?,
            )?,
            slotCount: <aver_rt::AverInt as ReplayValue>::from_replay_json(
                fields
                    .get("slotCount")
                    .ok_or_else(|| "$record FnDef missing field 'slotCount'".to_string())?,
            )?,
            slotMap:
                <aver_rt::AverMap<AverStr, aver_rt::AverInt> as ReplayValue>::from_replay_json(
                    fields
                        .get("slotMap")
                        .ok_or_else(|| "$record FnDef missing field 'slotMap'".to_string())?,
                )?,
            fastPath: <FnFastPath as ReplayValue>::from_replay_json(
                fields
                    .get("fastPath")
                    .ok_or_else(|| "$record FnDef missing field 'fastPath'".to_string())?,
            )?,
            tailLoop: <bool as ReplayValue>::from_replay_json(
                fields
                    .get("tailLoop")
                    .ok_or_else(|| "$record FnDef missing field 'tailLoop'".to_string())?,
            )?,
        }))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub deps: aver_rt::AverList<AverStr>,
    pub fns: aver_rt::AverList<FnDef>,
    pub stmts: aver_rt::AverList<Stmt>,
}

impl aver_rt::AverDisplay for Program {
    fn aver_display(&self) -> String {
        format!(
            "Program({})",
            vec![
                format!("deps: {}", self.deps.aver_display_inner()),
                format!("fns: {}", self.fns.aver_display_inner()),
                format!("stmts: {}", self.stmts.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for Program {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut fields = serde_json::Map::new();
        fields.insert("deps".to_string(), ReplayValue::to_replay_json(&self.deps));
        fields.insert("fns".to_string(), ReplayValue::to_replay_json(&self.fns));
        fields.insert(
            "stmts".to_string(),
            ReplayValue::to_replay_json(&self.stmts),
        );
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.Program".to_string()),
        );
        payload.insert("fields".to_string(), serde_json::Value::Object(fields));
        aver_replay::wrap_marker("$record", serde_json::Value::Object(payload))
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$record")?;
        let obj = aver_replay::expect_object(payload, "$record")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$record missing field 'type'".to_string())?,
            "$record.type",
        )?;
        if type_name != "Domain.Ast.Program" && type_name != "Program" {
            return Err(format!(
                "$record type mismatch: expected Domain.Ast.Program, got {}",
                type_name
            ));
        }
        let fields = aver_replay::expect_object(
            obj.get("fields")
                .ok_or_else(|| "$record missing field 'fields'".to_string())?,
            "$record.fields",
        )?;
        Ok(Self {
            deps: <aver_rt::AverList<AverStr> as ReplayValue>::from_replay_json(
                fields
                    .get("deps")
                    .ok_or_else(|| "$record Program missing field 'deps'".to_string())?,
            )?,
            fns: <aver_rt::AverList<FnDef> as ReplayValue>::from_replay_json(
                fields
                    .get("fns")
                    .ok_or_else(|| "$record Program missing field 'fns'".to_string())?,
            )?,
            stmts: <aver_rt::AverList<Stmt> as ReplayValue>::from_replay_json(
                fields
                    .get("stmts")
                    .ok_or_else(|| "$record Program missing field 'stmts'".to_string())?,
            )?,
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinOp {
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
}

impl BinOp {
    fn aver_key_rank(&self) -> usize {
        match self {
            BinOp::OpAdd => 0,
            BinOp::OpDiv => 1,
            BinOp::OpMul => 2,
            BinOp::OpSub => 3,
        }
    }
}

impl PartialOrd for BinOp {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BinOp {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for BinOp {
    fn aver_display(&self) -> String {
        match self {
            BinOp::OpAdd => "OpAdd".to_string(),
            BinOp::OpSub => "OpSub".to_string(),
            BinOp::OpMul => "OpMul".to_string(),
            BinOp::OpDiv => "OpDiv".to_string(),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for BinOp {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.BinOp".to_string()),
        );
        match self {
            BinOp::OpAdd => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("OpAdd".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            BinOp::OpSub => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("OpSub".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            BinOp::OpMul => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("OpMul".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            BinOp::OpDiv => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("OpDiv".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.BinOp" && type_name != "BinOp" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.BinOp, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "OpAdd" => Ok(BinOp::OpAdd),
            "OpSub" => Ok(BinOp::OpSub),
            "OpMul" => Ok(BinOp::OpMul),
            "OpDiv" => Ok(BinOp::OpDiv),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.BinOp",
                variant_name
            )),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CmpOp {
    CmpEq,
    CmpNeq,
    CmpLt,
    CmpGt,
    CmpLte,
    CmpGte,
}

impl CmpOp {
    fn aver_key_rank(&self) -> usize {
        match self {
            CmpOp::CmpEq => 0,
            CmpOp::CmpGt => 1,
            CmpOp::CmpGte => 2,
            CmpOp::CmpLt => 3,
            CmpOp::CmpLte => 4,
            CmpOp::CmpNeq => 5,
        }
    }
}

impl PartialOrd for CmpOp {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for CmpOp {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for CmpOp {
    fn aver_display(&self) -> String {
        match self {
            CmpOp::CmpEq => "CmpEq".to_string(),
            CmpOp::CmpNeq => "CmpNeq".to_string(),
            CmpOp::CmpLt => "CmpLt".to_string(),
            CmpOp::CmpGt => "CmpGt".to_string(),
            CmpOp::CmpLte => "CmpLte".to_string(),
            CmpOp::CmpGte => "CmpGte".to_string(),
        }
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

impl aver_replay::ReplayValue for CmpOp {
    fn to_replay_json(&self) -> serde_json::Value {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            serde_json::Value::String("Domain.Ast.CmpOp".to_string()),
        );
        match self {
            CmpOp::CmpEq => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("CmpEq".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            CmpOp::CmpNeq => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("CmpNeq".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            CmpOp::CmpLt => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("CmpLt".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            CmpOp::CmpGt => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("CmpGt".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            CmpOp::CmpLte => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("CmpLte".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
            CmpOp::CmpGte => {
                payload.insert(
                    "name".to_string(),
                    serde_json::Value::String("CmpGte".to_string()),
                );
                payload.insert("fields".to_string(), serde_json::Value::Array(vec![]));
                aver_replay::wrap_marker("$variant", serde_json::Value::Object(payload))
            }
        }
    }

    fn from_replay_json(value: &serde_json::Value) -> Result<Self, String> {
        let payload = aver_replay::expect_marker(value, "$variant")?;
        let obj = aver_replay::expect_object(payload, "$variant")?;
        let type_name = aver_replay::expect_string(
            obj.get("type")
                .ok_or_else(|| "$variant missing field 'type'".to_string())?,
            "$variant.type",
        )?;
        if type_name != "Domain.Ast.CmpOp" && type_name != "CmpOp" {
            return Err(format!(
                "$variant type mismatch: expected Domain.Ast.CmpOp, got {}",
                type_name
            ));
        }
        let variant_name = aver_replay::expect_string(
            obj.get("name")
                .ok_or_else(|| "$variant missing field 'name'".to_string())?,
            "$variant.name",
        )?;
        let fields = aver_replay::expect_array(
            obj.get("fields")
                .ok_or_else(|| "$variant missing field 'fields'".to_string())?,
            "$variant.fields",
        )?;
        match variant_name {
            "CmpEq" => Ok(CmpOp::CmpEq),
            "CmpNeq" => Ok(CmpOp::CmpNeq),
            "CmpLt" => Ok(CmpOp::CmpLt),
            "CmpGt" => Ok(CmpOp::CmpGt),
            "CmpLte" => Ok(CmpOp::CmpLte),
            "CmpGte" => Ok(CmpOp::CmpGte),
            _ => Err(format!(
                "unknown variant '{}' for Domain.Ast.CmpOp",
                variant_name
            )),
        }
    }
}

/// Tag ID for Result.Ok constructor.
pub fn tagResultOk() -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_rt::AverInt::from_i64(1)
}

/// Tag ID for Result.Err constructor.
pub fn tagResultErr() -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_rt::AverInt::from_i64(2)
}

/// Tag ID for Option.Some constructor.
pub fn tagOptionSome() -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_rt::AverInt::from_i64(3)
}

/// Tag ID for Option.None constructor.
pub fn tagOptionNone() -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_rt::AverInt::from_i64(4)
}

/// Base tag ID for user-defined constructors. User tags start from this value.
pub fn tagUserBase() -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_rt::AverInt::from_i64(100)
}

/// Modulo span used to keep hashed user constructor tags bounded.
pub fn userTagSpan() -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    aver_rt::AverInt::from_i64(1000003)
}

/// Map constructor names to tag IDs. Builtins keep fixed tags; user constructors get stable hashed tags.
#[inline(always)]
pub fn ctorNameToTag(name @ _: AverStr) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let __arg1 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::ctorNameToTag__indexed(name, __arg1)
}

/// Compute a stable non-zero offset for a user-defined constructor tag from its full dotted name.
#[inline(always)]
pub fn userCtorTagOffset(name @ _: AverStr) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let __arg1 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::userCtorTagOffset__indexed(name, __arg1)
}

/// Walk the constructor name and accumulate a bounded rolling hash.
#[inline(always)]
pub fn userCtorTagOffsetLoop(
    name @ _: AverStr,
    pos @ _: aver_rt::AverInt,
    acc @ _: aver_rt::AverInt,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let __arg3 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::userCtorTagOffsetLoop__indexed(name, pos, acc, __arg3)
}

/// Update the user constructor rolling hash and keep it in a bounded range.
pub fn userCtorTagStep(acc @ _: aver_rt::AverInt, code @ _: aver_rt::AverInt) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let next @ _ = acc.mul(&aver_rt::AverInt::from_i64(131)).add(&code);
    (next)
        .rem_euclid(&(aver_rt::AverInt::from_i64(1000003)))
        .unwrap()
}

/// One spelling per constructor: drop the declaring-module qualifier from a Module.Type.Variant reference, so a variant a dependency builds as Shade.Dark and its caller writes as Palette.Shade.Dark is the same constructor.
#[inline(always)]
pub fn canonicalCtorName(name @ _: AverStr) -> AverStr {
    crate::cancel_checkpoint();
    let __arg1 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::canonicalCtorName__indexed(name, &__arg1)
}

/// A name with at most one dot is already the shortest spelling it has.
#[inline(always)]
pub fn canonicalCtorNameAtLastDot(
    name @ _: AverStr,
    total @ _: aver_rt::AverInt,
    lastDot @ _: aver_rt::AverInt,
) -> AverStr {
    crate::cancel_checkpoint();
    let __arg3 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::canonicalCtorNameAtLastDot__indexed(
        name, total, lastDot, &__arg3,
    )
}

/// Keep only the trailing pair when a module qualifier precedes a Type.Variant reference; leave every other dotted name whole.
#[inline(always)]
pub fn canonicalCtorNameAtBothDots(
    name @ _: AverStr,
    total @ _: aver_rt::AverInt,
    lastDot @ _: aver_rt::AverInt,
    prevDot @ _: aver_rt::AverInt,
) -> AverStr {
    crate::cancel_checkpoint();
    let __arg4 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::canonicalCtorNameAtBothDots__indexed(
        name, total, lastDot, prevDot, &__arg4,
    )
}

/// Whether the trailing two segments spell Type.Variant. Both begin with an uppercase letter, the same rule the surface parser applies to constructor patterns.
#[inline(always)]
pub fn isTypeVariantTail(typeName @ _: AverStr, variantName @ _: AverStr) -> bool {
    crate::cancel_checkpoint();
    (crate::aver_generated::domain::ast::beginsUppercase(&typeName)
        && crate::aver_generated::domain::ast::beginsUppercase(&variantName))
}

/// Whether a dotted-name segment begins with an uppercase letter.
#[inline(always)]
pub fn beginsUppercase(segment @ _: &AverStr) -> bool {
    crate::cancel_checkpoint();
    match ((aver_rt::AverInt::from_i64(0))
        .to_usize()
        .and_then(|__i| segment.chars().nth(__i).map(|c| c.to_string())))
    .into_aver()
    {
        Some(c @ _) => (c != (c.to_lowercase()).into_aver()),
        None => false,
    }
}

/// Index of the last '.' at or before pos, or -1 when that prefix holds none.
#[inline(always)]
pub fn lastDotAtOrBefore(name @ _: AverStr, pos @ _: aver_rt::AverInt) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    let __arg2 @ _ = aver_rt::string_index_build(&name);
    crate::aver_generated::domain::ast::lastDotAtOrBefore__indexed(name, pos, __arg2)
}

/// Map builtin function names to integer IDs for fast dispatch.
#[inline(always)]
pub fn builtinNameToId(name @ _: AverStr) -> Option<aver_rt::AverInt> {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name;
        if &*__dispatch_subject == "Map.set" {
            Some(aver_rt::AverInt::from_i64(1))
        } else {
            if &*__dispatch_subject == "Map.get" {
                Some(aver_rt::AverInt::from_i64(2))
            } else {
                if &*__dispatch_subject == "Map.has" {
                    Some(aver_rt::AverInt::from_i64(3))
                } else {
                    if &*__dispatch_subject == "Map.fromList" {
                        Some(aver_rt::AverInt::from_i64(4))
                    } else {
                        if &*__dispatch_subject == "Map.entries" {
                            Some(aver_rt::AverInt::from_i64(5))
                        } else {
                            if &*__dispatch_subject == "Map.remove" {
                                Some(aver_rt::AverInt::from_i64(6))
                            } else {
                                if &*__dispatch_subject == "Vector.new" {
                                    Some(aver_rt::AverInt::from_i64(7))
                                } else {
                                    if &*__dispatch_subject == "Vector.get" {
                                        Some(aver_rt::AverInt::from_i64(8))
                                    } else {
                                        if &*__dispatch_subject == "Vector.set" {
                                            Some(aver_rt::AverInt::from_i64(9))
                                        } else {
                                            if &*__dispatch_subject == "Vector.len" {
                                                Some(aver_rt::AverInt::from_i64(10))
                                            } else {
                                                if &*__dispatch_subject == "Vector.fromList" {
                                                    Some(aver_rt::AverInt::from_i64(11))
                                                } else {
                                                    if &*__dispatch_subject == "List.fromVector" {
                                                        Some(aver_rt::AverInt::from_i64(12))
                                                    } else {
                                                        if &*__dispatch_subject == "Option.None" {
                                                            Some(aver_rt::AverInt::from_i64(13))
                                                        } else {
                                                            if &*__dispatch_subject == "Option.Some"
                                                            {
                                                                Some(aver_rt::AverInt::from_i64(14))
                                                            } else {
                                                                if &*__dispatch_subject
                                                                    == "Option.withDefault"
                                                                {
                                                                    Some(
                                                                        aver_rt::AverInt::from_i64(
                                                                            15,
                                                                        ),
                                                                    )
                                                                } else {
                                                                    if &*__dispatch_subject
                                                                        == "Result.Ok"
                                                                    {
                                                                        Some(aver_rt::AverInt::from_i64(16))
                                                                    } else {
                                                                        if &*__dispatch_subject
                                                                            == "Result.Err"
                                                                        {
                                                                            Some(aver_rt::AverInt::from_i64(17))
                                                                        } else {
                                                                            if &*__dispatch_subject
                                                                                == "Result.withDefault"
                                                                            {
                                                                                Some(aver_rt::AverInt::from_i64(18))
                                                                            } else {
                                                                                if &*__dispatch_subject == "String.fromInt" { Some(aver_rt::AverInt::from_i64(19)) } else { if &*__dispatch_subject == "List.take" { Some(aver_rt::AverInt::from_i64(20)) } else { if &*__dispatch_subject == "List.drop" { Some(aver_rt::AverInt::from_i64(21)) } else { None } } }
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Synthesized indexed worker of `ctorNameToTag`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn ctorNameToTag__indexed(
    name @ _: AverStr,
    mut __str_index @ _: aver_rt::StringIndex,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    {
        let __dispatch_subject = name.clone();
        if &*__dispatch_subject == "Result.Ok" {
            aver_rt::AverInt::from_i64(1)
        } else {
            if &*__dispatch_subject == "Result.Err" {
                aver_rt::AverInt::from_i64(2)
            } else {
                if &*__dispatch_subject == "Option.Some" {
                    aver_rt::AverInt::from_i64(3)
                } else {
                    if &*__dispatch_subject == "Option.None" {
                        aver_rt::AverInt::from_i64(4)
                    } else {
                        aver_rt::AverInt::from_i64(100).add(
                            &crate::aver_generated::domain::ast::userCtorTagOffset__indexed(
                                name,
                                __str_index,
                            ),
                        )
                    }
                }
            }
        }
    }
}

/// Synthesized indexed worker of `userCtorTagOffset`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn userCtorTagOffset__indexed(
    name @ _: AverStr,
    mut __str_index @ _: aver_rt::StringIndex,
) -> aver_rt::AverInt {
    crate::cancel_checkpoint();
    crate::aver_generated::domain::ast::userCtorTagOffsetLoop__indexed(
        name,
        aver_rt::AverInt::from_i64(0),
        aver_rt::AverInt::from_i64(0),
        __str_index,
    )
}

/// Synthesized indexed worker of `userCtorTagOffsetLoop`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn userCtorTagOffsetLoop__indexed(
    mut name @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut acc @ _: aver_rt::AverInt,
    mut __str_index @ _: aver_rt::StringIndex,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        let accPlusOne @ _ = acc.add(&aver_rt::AverInt::from_i64(1));
        if (pos < aver_rt::AverInt::from_i64(name.chars().count() as i64)) {
            match aver_rt::string_index_char_at_i64(
                &name,
                &__str_index,
                aver_rt::string_position(&pos),
            ) {
                Some(ch @ _) => {
                    let __tco1 = pos.add(&aver_rt::AverInt::from_i64(1));
                    let __tco2 = {
                        let __arg6 @ _ = (ch)
                            .chars()
                            .next()
                            .map(|c| aver_rt::AverInt::from_i64(c as i64))
                            .unwrap_or(aver_rt::AverInt::from_i64(0));
                        crate::aver_generated::domain::ast::userCtorTagStep(acc, __arg6)
                    };
                    pos = __tco1;
                    acc = __tco2;
                    continue;
                }
                None => {
                    return accPlusOne;
                }
            }
        } else {
            return accPlusOne;
        }
    }
}

/// Synthesized indexed worker of `canonicalCtorName`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn canonicalCtorName__indexed(
    name @ _: AverStr,
    __str_index @ _: &aver_rt::StringIndex,
) -> AverStr {
    crate::cancel_checkpoint();
    let total @ _ = aver_rt::AverInt::from_i64(name.chars().count() as i64);
    let __arg3 @ _ = crate::aver_generated::domain::ast::lastDotAtOrBefore__indexed(
        name.clone(),
        total.sub(&aver_rt::AverInt::from_i64(1)),
        __str_index.clone(),
    );
    crate::aver_generated::domain::ast::canonicalCtorNameAtLastDot__indexed(
        name,
        total,
        __arg3,
        __str_index,
    )
}

/// Synthesized indexed worker of `canonicalCtorNameAtLastDot`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn canonicalCtorNameAtLastDot__indexed(
    name @ _: AverStr,
    total @ _: aver_rt::AverInt,
    lastDot @ _: aver_rt::AverInt,
    __str_index @ _: &aver_rt::StringIndex,
) -> AverStr {
    crate::cancel_checkpoint();
    if (lastDot > aver_rt::AverInt::from_i64(0)) {
        {
            let __arg4 @ _ = crate::aver_generated::domain::ast::lastDotAtOrBefore__indexed(
                name.clone(),
                lastDot.sub(&aver_rt::AverInt::from_i64(1)),
                __str_index.clone(),
            );
            crate::aver_generated::domain::ast::canonicalCtorNameAtBothDots__indexed(
                name,
                total,
                lastDot,
                __arg4,
                __str_index,
            )
        }
    } else {
        name
    }
}

/// Synthesized indexed worker of `canonicalCtorNameAtBothDots`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn canonicalCtorNameAtBothDots__indexed(
    name @ _: AverStr,
    total @ _: aver_rt::AverInt,
    lastDot @ _: aver_rt::AverInt,
    prevDot @ _: aver_rt::AverInt,
    __str_index @ _: &aver_rt::StringIndex,
) -> AverStr {
    crate::cancel_checkpoint();
    if (prevDot < aver_rt::AverInt::from_i64(0)) {
        name
    } else {
        if {
            let __arg5 @ _ = aver_rt::string_index_slice_i64(
                &name,
                &__str_index,
                aver_rt::string_position(&prevDot.add(&aver_rt::AverInt::from_i64(1))),
                aver_rt::string_position(&lastDot),
            );
            {
                let __arg6 @ _ = aver_rt::string_index_slice_i64(
                    &name,
                    &__str_index,
                    aver_rt::string_position(&lastDot.add(&aver_rt::AverInt::from_i64(1))),
                    aver_rt::string_position(&total),
                );
                crate::aver_generated::domain::ast::isTypeVariantTail(__arg5, __arg6)
            }
        } {
            aver_rt::string_index_slice_i64(
                &name,
                &__str_index,
                aver_rt::string_position(&prevDot.add(&aver_rt::AverInt::from_i64(1))),
                aver_rt::string_position(&total),
            )
        } else {
            name
        }
    }
}

/// Synthesized indexed worker of `lastDotAtOrBefore`. Its hidden String.Index is built by the ABI-preserving wrapper and forwarded through the recursive string-flow component.
#[inline(always)]
pub fn lastDotAtOrBefore__indexed(
    mut name @ _: AverStr,
    mut pos @ _: aver_rt::AverInt,
    mut __str_index @ _: aver_rt::StringIndex,
) -> aver_rt::AverInt {
    loop {
        crate::cancel_checkpoint();
        if (pos < aver_rt::AverInt::from_i64(0)) {
            return aver_rt::AverInt::from_i64(-1);
        } else {
            match aver_rt::string_index_char_at_i64(
                &name,
                &__str_index,
                aver_rt::string_position(&pos),
            ) {
                Some(c @ _) => {
                    if (&*c == ".") {
                        return pos;
                    } else {
                        {
                            let __tco1 = pos.sub(&aver_rt::AverInt::from_i64(1));
                            pos = __tco1;
                            continue;
                        }
                    }
                }
                None => {
                    return aver_rt::AverInt::from_i64(-1);
                }
            }
        }
    }
}
