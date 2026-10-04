#[allow(unused_imports)]
use crate::proof_kernel::*;
use ::aver_rt::aver_list_match;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Proof {
    PRefl(crate::proof_kernel::aver_generated::kernel::term::Term),
    PSymm(std::sync::Arc<Proof>),
    PTrans(
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        aver_rt::AverList<Proof>,
    ),
    PCongr(
        crate::proof_kernel::aver_generated::kernel::term::Term,
        std::sync::Arc<Proof>,
    ),
    PUnfold(
        AverStr,
        aver_rt::AverInt,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        aver_rt::AverList<Proof>,
    ),
    PConst(AverStr),
    PArm(
        aver_rt::AverInt,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        std::sync::Arc<Proof>,
    ),
    PProj(crate::proof_kernel::aver_generated::kernel::term::Term),
    PHyp(AverStr),
    PRule(
        AverStr,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
        aver_rt::AverList<Proof>,
    ),
    PLaw(
        AverStr,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
        aver_rt::AverList<Proof>,
    ),
    PCompute(
        crate::proof_kernel::aver_generated::kernel::term::Term,
        crate::proof_kernel::aver_generated::kernel::term::Term,
    ),
    PCases(
        crate::proof_kernel::aver_generated::kernel::term::Term,
        AverStr,
        std::sync::Arc<Proof>,
        std::sync::Arc<Proof>,
    ),
    PEnum(
        AverStr,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        aver_rt::AverList<Proof>,
    ),
    PAbsurd(
        std::sync::Arc<Proof>,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        crate::proof_kernel::aver_generated::kernel::term::Term,
    ),
    PInduct(
        AverStr,
        aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        crate::proof_kernel::aver_generated::kernel::term::Term,
        aver_rt::AverList<Case>,
    ),
}

impl Proof {
    fn aver_key_rank(&self) -> usize {
        match self {
            Proof::PAbsurd(..) => 0,
            Proof::PArm(..) => 1,
            Proof::PCases(..) => 2,
            Proof::PCompute(..) => 3,
            Proof::PCongr(..) => 4,
            Proof::PConst(..) => 5,
            Proof::PEnum(..) => 6,
            Proof::PHyp(..) => 7,
            Proof::PInduct(..) => 8,
            Proof::PLaw(..) => 9,
            Proof::PProj(..) => 10,
            Proof::PRefl(..) => 11,
            Proof::PRule(..) => 12,
            Proof::PSymm(..) => 13,
            Proof::PTrans(..) => 14,
            Proof::PUnfold(..) => 15,
        }
    }
}

impl PartialOrd for Proof {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Proof {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let rank = self.aver_key_rank().cmp(&other.aver_key_rank());
        if rank != std::cmp::Ordering::Equal {
            return rank;
        }
        match (self, other) {
            (Proof::PAbsurd(a0, a1, a2), Proof::PAbsurd(b0, b1, b2)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1))
                .then_with(|| a2.cmp(b2)),
            (Proof::PArm(a0, a1, a2, a3), Proof::PArm(b0, b1, b2, b3)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1))
                .then_with(|| a2.cmp(b2))
                .then_with(|| a3.cmp(b3)),
            (Proof::PCases(a0, a1, a2, a3), Proof::PCases(b0, b1, b2, b3)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
                    .then_with(|| a3.cmp(b3))
            }
            (Proof::PCompute(a0, a1), Proof::PCompute(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Proof::PCongr(a0, a1), Proof::PCongr(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Proof::PConst(a0), Proof::PConst(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Proof::PEnum(a0, a1, a2, a3), Proof::PEnum(b0, b1, b2, b3)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
                    .then_with(|| a3.cmp(b3))
            }
            (Proof::PHyp(a0), Proof::PHyp(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Proof::PInduct(a0, a1, a2, a3, a4), Proof::PInduct(b0, b1, b2, b3, b4)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
                    .then_with(|| a3.cmp(b3))
                    .then_with(|| a4.cmp(b4))
            }
            (Proof::PLaw(a0, a1, a2), Proof::PLaw(b0, b1, b2)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1))
                .then_with(|| a2.cmp(b2)),
            (Proof::PProj(a0), Proof::PProj(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Proof::PRefl(a0), Proof::PRefl(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Proof::PRule(a0, a1, a2), Proof::PRule(b0, b1, b2)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1))
                .then_with(|| a2.cmp(b2)),
            (Proof::PSymm(a0), Proof::PSymm(b0)) => {
                std::cmp::Ordering::Equal.then_with(|| a0.cmp(b0))
            }
            (Proof::PTrans(a0, a1), Proof::PTrans(b0, b1)) => std::cmp::Ordering::Equal
                .then_with(|| a0.cmp(b0))
                .then_with(|| a1.cmp(b1)),
            (Proof::PUnfold(a0, a1, a2, a3, a4), Proof::PUnfold(b0, b1, b2, b3, b4)) => {
                std::cmp::Ordering::Equal
                    .then_with(|| a0.cmp(b0))
                    .then_with(|| a1.cmp(b1))
                    .then_with(|| a2.cmp(b2))
                    .then_with(|| a3.cmp(b3))
                    .then_with(|| a4.cmp(b4))
            }
            _ => std::cmp::Ordering::Equal,
        }
    }
}

impl aver_rt::AverDisplay for Proof {
    fn aver_display(&self) -> String {
        match self {
            Proof::PRefl(f0) => format!("PRefl({})", f0.aver_display_inner()),
            Proof::PSymm(f0) => format!("PSymm({})", f0.aver_display_inner()),
            Proof::PTrans(f0, f1) => format!(
                "PTrans({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Proof::PCongr(f0, f1) => format!(
                "PCongr({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Proof::PUnfold(f0, f1, f2, f3, f4) => format!(
                "PUnfold({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner(),
                    f4.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PConst(f0) => format!("PConst({})", f0.aver_display_inner()),
            Proof::PArm(f0, f1, f2, f3) => format!(
                "PArm({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PProj(f0) => format!("PProj({})", f0.aver_display_inner()),
            Proof::PHyp(f0) => format!("PHyp({})", f0.aver_display_inner()),
            Proof::PRule(f0, f1, f2) => format!(
                "PRule({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PLaw(f0, f1, f2) => format!(
                "PLaw({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PCompute(f0, f1) => format!(
                "PCompute({})",
                vec![f0.aver_display_inner(), f1.aver_display_inner()].join(", ")
            ),
            Proof::PCases(f0, f1, f2, f3) => format!(
                "PCases({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PEnum(f0, f1, f2, f3) => format!(
                "PEnum({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner(),
                    f3.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PAbsurd(f0, f1, f2) => format!(
                "PAbsurd({})",
                vec![
                    f0.aver_display_inner(),
                    f1.aver_display_inner(),
                    f2.aver_display_inner()
                ]
                .join(", ")
            ),
            Proof::PInduct(f0, f1, f2, f3, f4) => format!(
                "PInduct({})",
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Case {
    pub binders: aver_rt::AverList<AverStr>,
    pub ihs: aver_rt::AverList<AverStr>,
    pub proof: Proof,
}

impl PartialOrd for Case {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Case {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.binders.cmp(&other.binders))
            .then_with(|| self.ihs.cmp(&other.ihs))
            .then_with(|| self.proof.cmp(&other.proof))
    }
}

impl aver_rt::AverDisplay for Case {
    fn aver_display(&self) -> String {
        format!(
            "Case({})",
            vec![
                format!("binders: {}", self.binders.aver_display_inner()),
                format!("ihs: {}", self.ihs.aver_display_inner()),
                format!("proof: {}", self.proof.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Def {
    pub name: AverStr,
    pub params: aver_rt::AverList<AverStr>,
    pub lets: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Binding>,
    pub body: crate::proof_kernel::aver_generated::kernel::term::Term,
}

impl PartialOrd for Def {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Def {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.body.cmp(&other.body))
            .then_with(|| self.lets.cmp(&other.lets))
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.params.cmp(&other.params))
    }
}

impl aver_rt::AverDisplay for Def {
    fn aver_display(&self) -> String {
        format!(
            "Def({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("params: {}", self.params.aver_display_inner()),
                format!("lets: {}", self.lets.aver_display_inner()),
                format!("body: {}", self.body.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Const {
    pub name: AverStr,
    pub value: crate::proof_kernel::aver_generated::kernel::term::Term,
}

impl PartialOrd for Const {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Const {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.name.cmp(&other.name))
            .then_with(|| self.value.cmp(&other.value))
    }
}

impl aver_rt::AverDisplay for Const {
    fn aver_display(&self) -> String {
        format!(
            "Const({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("value: {}", self.value.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Law {
    pub key: AverStr,
    pub givens: aver_rt::AverList<AverStr>,
    pub premise: aver_rt::AverList<crate::proof_kernel::aver_generated::kernel::term::Term>,
    pub lhs: crate::proof_kernel::aver_generated::kernel::term::Term,
    pub rhs: crate::proof_kernel::aver_generated::kernel::term::Term,
}

impl PartialOrd for Law {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Law {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.givens.cmp(&other.givens))
            .then_with(|| self.key.cmp(&other.key))
            .then_with(|| self.lhs.cmp(&other.lhs))
            .then_with(|| self.premise.cmp(&other.premise))
            .then_with(|| self.rhs.cmp(&other.rhs))
    }
}

impl aver_rt::AverDisplay for Law {
    fn aver_display(&self) -> String {
        format!(
            "Law({})",
            vec![
                format!("key: {}", self.key.aver_display_inner()),
                format!("givens: {}", self.givens.aver_display_inner()),
                format!("premise: {}", self.premise.aver_display_inner()),
                format!("lhs: {}", self.lhs.aver_display_inner()),
                format!("rhs: {}", self.rhs.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Given {
    pub name: AverStr,
    pub fin: crate::proof_kernel::aver_generated::kernel::term::Fin,
}

impl PartialOrd for Given {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Given {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.fin.cmp(&other.fin))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for Given {
    fn aver_display(&self) -> String {
        format!(
            "Given({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("fin: {}", self.fin.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Script {
    pub obligation: Law,
    pub finite: aver_rt::AverList<Given>,
    pub defs: aver_rt::AverList<Def>,
    pub consts: aver_rt::AverList<Const>,
    pub laws: aver_rt::AverList<Law>,
    pub proof: Proof,
}

impl PartialOrd for Script {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Script {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.consts.cmp(&other.consts))
            .then_with(|| self.defs.cmp(&other.defs))
            .then_with(|| self.finite.cmp(&other.finite))
            .then_with(|| self.laws.cmp(&other.laws))
            .then_with(|| self.obligation.cmp(&other.obligation))
            .then_with(|| self.proof.cmp(&other.proof))
    }
}

impl aver_rt::AverDisplay for Script {
    fn aver_display(&self) -> String {
        format!(
            "Script({})",
            vec![
                format!("obligation: {}", self.obligation.aver_display_inner()),
                format!("finite: {}", self.finite.aver_display_inner()),
                format!("defs: {}", self.defs.aver_display_inner()),
                format!("consts: {}", self.consts.aver_display_inner()),
                format!("laws: {}", self.laws.aver_display_inner()),
                format!("proof: {}", self.proof.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Hyp {
    pub name: AverStr,
    pub eqn: crate::proof_kernel::aver_generated::kernel::term::Eqn,
}

impl PartialOrd for Hyp {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hyp {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
            .then_with(|| self.eqn.cmp(&other.eqn))
            .then_with(|| self.name.cmp(&other.name))
    }
}

impl aver_rt::AverDisplay for Hyp {
    fn aver_display(&self) -> String {
        format!(
            "Hyp({})",
            vec![
                format!("name: {}", self.name.aver_display_inner()),
                format!("eqn: {}", self.eqn.aver_display_inner())
            ]
            .join(", ")
        )
    }
    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// A law's when, if it has one.
#[inline(always)]
pub fn premiseOf(law @ _: &Law) -> Option<crate::proof_kernel::aver_generated::kernel::term::Term> {
    crate::proof_kernel::cancel_checkpoint();
    aver_list_match!(law.premise.clone(), [] => None, [p, rest] => Some(p))
}
